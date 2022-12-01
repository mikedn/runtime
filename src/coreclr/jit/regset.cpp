// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "emit.h"

var_types SpillTempSet::tmpNormalizeType(var_types type)
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

void SpillTempSet::tmpPreAllocateTemps(var_types type, unsigned count)
{
    assert(type == tmpNormalizeType(type));

    unsigned size = varTypeSize(type);
    unsigned slot = GetTempListIndex(size);

    for (unsigned i = 0; i < count; i++)
    {
        tempCount++;

        SpillTemp* temp = new (compiler, CMK_SpillTemp) SpillTemp(-static_cast<int>(tempCount), size, type);
        temp->tdNext    = freeTemps[slot];
        freeTemps[slot] = temp;

        JITDUMP("pre-allocated temp #%u, slot %u, size = %u\n", -temp->tdTempNum(), slot, temp->tdTempSize());
    }
}

SpillTemp* SpillTempSet::tmpFindNum(int num, TempState state) const
{
    assert(num < 0);

    for (SpillTemp* temp = tmpListBeg(state); temp != nullptr; temp = tmpListNxt(temp, state))
    {
        if (temp->tdTempNum() == num)
        {
            return temp;
        }
    }

    return nullptr;
}

SpillTemp* SpillTempSet::tmpListBeg(TempState state) const
{
    SpillTemp* const* lists     = state == TEMP_USAGE_FREE ? freeTemps : usedTemps;
    unsigned          listIndex = 0;

    while ((listIndex < TempListCount - 1) && (lists[listIndex] == nullptr))
    {
        listIndex++;
    }

    return lists[listIndex];
}

SpillTemp* SpillTempSet::tmpListNxt(SpillTemp* temp, TempState state) const
{
    assert(temp != nullptr);

    SpillTemp* next = temp->tdNext;

    if (next != nullptr)
    {
        return next;
    }

    SpillTemp* const* lists     = state == TEMP_USAGE_FREE ? freeTemps : usedTemps;
    unsigned          listIndex = GetTempListIndex(temp->tdTempSize());

    while ((++listIndex < TempListCount) && (next == nullptr))
    {
        next = lists[listIndex];
    }

    return next;
}

SpillTemp* SpillTempSet::AllocTemp(var_types type)
{
    type = tmpNormalizeType(type);

    unsigned    listIndex = GetTempListIndex(varTypeSize(type));
    SpillTemp** last      = &freeTemps[listIndex];
    SpillTemp*  temp;

    for (temp = *last; temp != nullptr; last = &temp->tdNext, temp = *last)
    {
        if (temp->tdTempType() == type)
        {
            *last = temp->tdNext;
            break;
        }
    }

    noway_assert(temp != nullptr);

    JITDUMP("Using temp #%d\n", -temp->tdTempNum());
    INDEBUG(usedTempCount++);

    temp->tdNext         = usedTemps[listIndex];
    usedTemps[listIndex] = temp;

    return temp;
}

void SpillTempSet::tmpRlsTemp(SpillTemp* temp)
{
    assert(temp != nullptr);
    assert(usedTempCount != 0);

    JITDUMP("Releasing temp #%d\n", -temp->tdTempNum());
    INDEBUG(usedTempCount--);

    unsigned    listIndex = GetTempListIndex(temp->tdTempSize());
    SpillTemp** last      = &usedTemps[listIndex];
    SpillTemp*  t;

    for (t = *last; t != nullptr; last = &t->tdNext, t = *last)
    {
        if (t == temp)
        {
            *last = t->tdNext;
            break;
        }
    }

    assert(t != nullptr);

    temp->tdNext         = freeTemps[listIndex];
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

bool SpillTempSet::tmpAllFree() const
{
    unsigned usedCount = 0;

    for (SpillTemp* temp = tmpListBeg(TEMP_USAGE_USED); temp != nullptr; temp = tmpListNxt(temp, TEMP_USAGE_USED))
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

bool SpillTempSet::rsSpillChk() const
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

void SpillTempSet::tmpEnd() const
{
    assert(rsSpillChk());

    if (tempCount > 0)
    {
        JITDUMP("%u tmps used\n", tempCount);
    }
}

void SpillTempSet::tmpDone() const
{
    assert(rsSpillChk());
    assert(tmpAllFree());

    unsigned   count;
    SpillTemp* temp;

    for (temp = tmpListBeg(), count = temp ? 1 : 0; temp != nullptr; temp = tmpListNxt(temp), count += temp ? 1 : 0)
    {
        assert(temp->tdLegalOffset());
    }

    assert(count == tempCount);
    assert(usedTempCount == 0);
}

#endif // DEBUG
