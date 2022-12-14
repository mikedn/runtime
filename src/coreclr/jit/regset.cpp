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

unsigned SpillTempSet::GetTempListIndex(var_types type)
{
    unsigned size = varTypeSize(type);
    noway_assert((TempMinSize <= size) && (size <= TempMaxSize) && (size % TempMinSize == 0));
    return genLog2(size / TempMinSize);
}

void SpillTempSet::PreAllocateTemps(const unsigned* typeSpillCounts)
{
    // Maintain the original temp list order - due to the type size buckets and single
    // linked list use the old code ended up allocating temps in a partially reversed
    // order - {INT}, {BYREF, REF, LONG} rather than {INT}, {LONG, REF, BYREF}.
    // So we need to know where the size bucket lists end in the temp array to be able
    // to fill lists backwards.
    // TODO-MIKE-Cleanup: This order isn't special in any way but a different ordering
    // results in some diffs. Otherwise this is just unncessary complication.
    unsigned listEnds[TempListCount]{};

    for (int t = 0; t < TYP_COUNT; t++)
    {
        unsigned spillCount = typeSpillCounts[t];

        if (spillCount == 0)
        {
            continue;
        }

        var_types type      = static_cast<var_types>(t);
        unsigned  listIndex = GetTempListIndex(type);

        // Only normalized types should have anything in the spill count array.
        assert(type == GetTempType(type));

        listEnds[listIndex] += spillCount;
    }

    for (unsigned i = 1; i < _countof(listEnds); i++)
    {
        listEnds[i] += listEnds[i - 1];
    }

    tempCount = listEnds[_countof(listEnds) - 1];

    if (tempCount == 0)
    {
        return;
    }

    JITDUMP("Creating %u spill temps:\n", tempCount);

    temps = compiler->getAllocator(CMK_SpillTemp).allocate<SpillTemp>(tempCount);

    for (int t = 0; t < TYP_COUNT; t++)
    {
        unsigned spillCount = typeSpillCounts[t];

        if (spillCount == 0)
        {
            continue;
        }

        var_types type      = static_cast<var_types>(t);
        unsigned  listIndex = GetTempListIndex(type);

        for (unsigned i = 0; i < spillCount; i++)
        {
            unsigned   n    = --listEnds[listIndex];
            SpillTemp* temp = new (&temps[n]) SpillTemp(n, type);

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

    unsigned    listIndex = GetTempListIndex(type);
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

    INDEBUG(temp->next = nullptr);

    return temp;
}

void SpillTempSet::ReleaseTemp(SpillTemp* temp)
{
    unsigned listIndex   = GetTempListIndex(temp->GetType());
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
    def->temp = AllocTemp(GetTempType(type));

    JITDUMP("Spill temp T%02d def\n", -def->temp->GetNum());
    INDEBUG(defCount++);

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

    JITDUMP("Spill temp T%02d use\n", -def->temp->GetNum());
    INDEBUG(defCount--);

    ReleaseTemp(def->temp);

    return def->temp;
}
