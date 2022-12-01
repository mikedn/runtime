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

    return size / TempMinSize - 1;
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

    SpillTemp* const* lists = state == TEMP_USAGE_FREE ? freeTemps : usedTemps;
    unsigned          size  = temp->tdTempSize();

    while ((size < TempMaxSize) && (next == nullptr))
    {
        size += TempMinSize;
        next = lists[GetTempListIndex(size)];
    }

    assert((next == nullptr) || (next->tdTempSize() == size));

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

SpillTempDef* SpillTempSet::AllocDef()
{
    if (defFreeList != nullptr)
    {
        SpillTempDef* def = defFreeList;
        defFreeList       = def->next;
        return def;
    }

    return new (compiler, CMK_SpillTemp) SpillTempDef();
}

void SpillTempSet::FreeDef(SpillTempDef* def)
{
    def->next   = defFreeList;
    defFreeList = def;
}

SpillTemp* SpillTempSet::AllocSpillTemp(GenTree* node, regNumber reg, var_types type)
{
    SpillTemp* temp = AllocTemp(type);

    SpillTempDef* def = AllocDef();
    def->temp         = temp;
    def->node         = node;
    def->next         = regDefMap[reg];
    regDefMap[reg]    = def;

    return temp;
}

SpillTempDef* SpillTempSet::rsGetSpillInfo(GenTree* node, regNumber reg, SpillTempDef** prevDef)
{
    SpillTempDef* prev = nullptr;
    SpillTempDef* def  = regDefMap[reg];

    for (; def != nullptr; prev = def, def = def->next)
    {
        if (def->node == node)
        {
            break;
        }
    }

    if (prevDef != nullptr)
    {
        *prevDef = prev;
    }

    return def;
}

SpillTemp* SpillTempSet::rsGetSpillTempWord(regNumber reg, SpillTempDef* def, SpillTempDef* prevDef)
{
    assert((prevDef == nullptr) || (prevDef->next == def));

    if (prevDef != nullptr)
    {
        prevDef->next = def->next;
    }
    else
    {
        regDefMap[reg] = def->next;
    }

    SpillTemp* temp = def->temp;
    FreeDef(def);
    return temp;
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

    for (regNumber reg = REG_FIRST; reg < REG_COUNT; reg = REG_NEXT(reg))
    {
        if (regDefMap[reg] != nullptr)
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
