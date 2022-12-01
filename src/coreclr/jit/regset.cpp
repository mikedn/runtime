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

        // Only normalized types should have anything in the spill count array.
        assert((spillCount == 0) || (type == GetTempType(type)));

        tempCount += spillCount;
    }

    if (tempCount == 0)
    {
        return;
    }

    temps = compiler->getAllocator(CMK_SpillTemp).allocate<SpillTemp>(tempCount);

    unsigned tempIndex = 0;

    for (int t = 0; t < TYP_COUNT; t++)
    {
        var_types type       = static_cast<var_types>(t);
        unsigned  spillCount = typeSpillCounts[t];

        if (spillCount == 0)
        {
            continue;
        }

        unsigned size      = varTypeSize(type);
        unsigned listIndex = GetTempListIndex(size);

        for (unsigned i = 0; i < spillCount; i++, tempIndex++)
        {
            SpillTemp* temp = new (&temps[tempIndex]) SpillTemp(-static_cast<int>(tempIndex + 1), size, type);

            temp->next           = freeTemps[listIndex];
            freeTemps[listIndex] = temp;

            JITDUMP("Temp #%u %s\n", -temp->GetNum(), varTypeName(type));
        }
    }
}

SpillTemp* SpillTempSet::FindTempByNum(int num) const
{
    unsigned index = static_cast<unsigned>(-num) - 1;
    assert(index < tempCount);

    SpillTemp* temp = &temps[index];
    assert(temp->GetNum() == num);
    return temp;
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
    INDEBUG(defCount++);

    temp->next           = usedTemps[listIndex];
    usedTemps[listIndex] = temp;

    return temp;
}

void SpillTempSet::ReleaseTemp(SpillTemp* temp)
{
    assert(temp != nullptr);
    assert(defCount != 0);

    JITDUMP("Releasing temp #%d\n", -temp->GetNum());
    INDEBUG(defCount--);

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
