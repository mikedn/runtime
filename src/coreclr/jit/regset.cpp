// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "emit.h"

var_types SpillTempSet::GetTempType(var_types type)
{
#ifdef FEATURE_SIMD
    if (type == TYP_SIMD12)
    {
        return TYP_SIMD16;
    }
#endif

    return varActualType(type);
}

unsigned SpillTempSet::GetTempListIndex(unsigned size)
{
    noway_assert((TempMinSize <= size) && (size <= TempMaxSize) && (size % TempMinSize == 0));

    return genLog2(size / TempMinSize);
}

void SpillTempSet::PreAllocateTemps(const unsigned* typeSpillCounts)
{
    JITDUMP("Creating spill temps:\n");

    for (int t = 0; t < TYP_COUNT; t++)
    {
        var_types type       = static_cast<var_types>(t);
        unsigned  spillCount = typeSpillCounts[t];

        if (spillCount == 0)
        {
            continue;
        }

        // Only normalized types should have anything in the spill count array.
        assert(type == GetTempType(type));

        unsigned size      = varTypeSize(type);
        unsigned listIndex = GetTempListIndex(size);

        for (unsigned i = 0; i < spillCount; i++)
        {
            tempCount++;

            SpillTemp* temp      = new (compiler, CMK_SpillTemp) SpillTemp(-static_cast<int>(tempCount), size, type);
            temp->next           = freeTemps[listIndex];
            freeTemps[listIndex] = temp;

            JITDUMP("Temp #%u %s\n", -temp->GetNum(), varTypeName(type));
        }
    }
}

SpillTemp* SpillTempSet::FindTempByNum(int num) const
{
    assert(num < 0);

    for (SpillTemp* temp = GetFirstTemp(Free); temp != nullptr; temp = GetNextTemp(temp, Free))
    {
        if (temp->GetNum() == num)
        {
            return temp;
        }
    }

    for (SpillTemp* temp = GetFirstTemp(Used); temp != nullptr; temp = GetNextTemp(temp, Used))
    {
        if (temp->GetNum() == num)
        {
            return temp;
        }
    }

    return nullptr;
}

SpillTemp* SpillTempSet::GetFirstTemp(TempState state) const
{
    SpillTemp* const* lists     = state == Free ? freeTemps : usedTemps;
    unsigned          listIndex = 0;

    while ((listIndex < TempListCount - 1) && (lists[listIndex] == nullptr))
    {
        listIndex++;
    }

    return lists[listIndex];
}

SpillTemp* SpillTempSet::GetNextTemp(SpillTemp* temp, TempState state) const
{
    assert(temp != nullptr);

    SpillTemp* next = temp->next;

    if (next != nullptr)
    {
        return next;
    }

    SpillTemp* const* lists     = state == Free ? freeTemps : usedTemps;
    unsigned          listIndex = GetTempListIndex(temp->GetSize());

    while ((++listIndex < TempListCount) && (next == nullptr))
    {
        next = lists[listIndex];
    }

    return next;
}

SpillTemp* SpillTempSet::AllocTemp(var_types type)
{
    type = GetTempType(type);

    unsigned    listIndex = GetTempListIndex(varTypeSize(type));
    SpillTemp** last      = &freeTemps[listIndex];
    SpillTemp*  temp;

    for (temp = *last; temp != nullptr; last = &temp->next, temp = *last)
    {
        if (temp->GetType() == type)
        {
            *last = temp->next;
            break;
        }
    }

    noway_assert(temp != nullptr);

    JITDUMP("Using temp #%d\n", -temp->GetNum());
    INDEBUG(usedTempCount++);

    temp->next           = usedTemps[listIndex];
    usedTemps[listIndex] = temp;

    return temp;
}

void SpillTempSet::ReleaseTemp(SpillTemp* temp)
{
    assert(temp != nullptr);
    assert(usedTempCount != 0);

    JITDUMP("Releasing temp #%d\n", -temp->GetNum());
    INDEBUG(usedTempCount--);

    unsigned    listIndex = GetTempListIndex(temp->GetSize());
    SpillTemp** last      = &usedTemps[listIndex];
    SpillTemp*  t;

    for (t = *last; t != nullptr; last = &t->next, t = *last)
    {
        if (t == temp)
        {
            *last = t->next;
            break;
        }
    }

    assert(t != nullptr);

    temp->next           = freeTemps[listIndex];
    freeTemps[listIndex] = temp;
}

SpillTemp* SpillTempSet::DefSpillTemp(GenTree* node, unsigned regIndex, var_types type)
{
    assert(regIndex < _countof(regDefMap));

    SpillTempDef* def;

    if (defFreeList != nullptr)
    {
        def         = defFreeList;
        defFreeList = def->next;
    }
    else
    {
        def = new (compiler, CMK_SpillTemp) SpillTempDef;
    }

    def->next           = regDefMap[regIndex];
    regDefMap[regIndex] = def;

    def->node = node;
    def->temp = AllocTemp(type);

    return def->temp;
}

SpillTemp* SpillTempSet::UseSpillTemp(GenTree* node, unsigned regIndex)
{
    assert(regIndex < _countof(regDefMap));

    SpillTempDef** prevLink = &regDefMap[regIndex];
    SpillTempDef*  def      = *prevLink;

    while (def->node != node)
    {
        prevLink = &def->next;
        def      = def->next;
    }

    *prevLink = def->next;

    def->next   = defFreeList;
    defFreeList = def;

    return def->temp;
}

#ifdef DEBUG

bool SpillTempSet::AreAllTempsFree() const
{
    unsigned usedCount = 0;

    for (SpillTemp* temp = GetFirstTemp(Used); temp != nullptr; temp = GetNextTemp(temp, Used))
    {
        ++usedCount;
    }

    assert(usedCount == usedTempCount);

    if (usedTempCount != 0)
    {
        return false;
    }

    for (unsigned i = 0; i < _countof(usedTemps); i++)
    {
        if (usedTemps[i] != nullptr)
        {
            return false;
        }
    }

    return true;
}

bool SpillTempSet::AreAllSpillDefsFree() const
{
    if (usedTempCount != 0)
    {
        return false;
    }

    for (unsigned i = 0; i < _countof(regDefMap); i++)
    {
        if (regDefMap[i] != nullptr)
        {
            return false;
        }
    }

    return true;
}

void SpillTempSet::End() const
{
    assert(AreAllSpillDefsFree());

    if (tempCount > 0)
    {
        JITDUMP("%u tmps used\n", tempCount);
    }
}

void SpillTempSet::Done() const
{
    assert(AreAllSpillDefsFree());
    assert(AreAllTempsFree());

    unsigned   count;
    SpillTemp* temp;

    for (temp = GetFirstTemp(), count = temp ? 1 : 0; temp != nullptr; temp = GetNextTemp(temp), count += temp ? 1 : 0)
    {
        assert(temp->IsAllocated());
    }

    assert(count == tempCount);
    assert(usedTempCount == 0);
}

#endif // DEBUG
