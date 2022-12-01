// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "vartype.h"
#include "target.h"

class SpillTemp
{
    friend class SpillTempSet;

    SpillTemp* next;
    const int  num;
    int offset      INDEBUG(= INT_MIN);
    const var_types type;

public:
    SpillTemp(int num, var_types type) : num(num), type(type)
    {
        assert(num < 0);
    }

    int GetNum() const
    {
        assert(num < 0);
        return num;
    }

    var_types GetType() const
    {
        return type;
    }

    int GetOffset() const
    {
        assert(offset != INT_MIN);
        return offset;
    }

    void SetOffset(int offs)
    {
        offset = offs;
    }

    void AdjustOffset(int delta)
    {
        offset += delta;
    }
};

struct SpillTempDef
{
    SpillTempDef* next;
    GenTree*      node;
    SpillTemp*    temp;
};

class SpillTempSet
{
    static constexpr unsigned TempMinSize = 4;
#ifdef TARGET_ARM64
    static constexpr unsigned TempMaxSize   = FP_REGSIZE_BYTES;
    static constexpr unsigned TempListCount = 3; // 4, 8, 16
#elif defined(TARGET_XARCH) && defined(FEATURE_SIMD)
    static constexpr unsigned TempMaxSize   = YMM_REGSIZE_BYTES;
    static constexpr unsigned TempListCount = 4; // 4, 8, 16, 32
#else
    static constexpr unsigned TempMaxSize   = 8;
    static constexpr unsigned TempListCount = 2; // 4, 8
#endif

    class Compiler* compiler;
    SpillTempDef*   defFreeList = nullptr;
    SpillTempDef*   regDefMap[MAX_MULTIREG_COUNT]{};
    SpillTemp*      freeTemps[TempListCount]{};
    SpillTemp*      temps     = nullptr;
    unsigned        tempCount = 0;
    INDEBUG(unsigned defCount = 0;)

    enum TempState
    {
        Free,
        Used
    };

public:
    SpillTempSet(class Compiler* compiler) : compiler(compiler)
    {
    }

    static var_types GetTempType(var_types type);
    void PreAllocateTemps(const unsigned* typeSpillCounts);
    SpillTemp* FindTempByNum(int num) const;

    SpillTemp* DefSpillTemp(GenTree* node, unsigned regIndex, var_types type);
    SpillTemp* UseSpillTemp(GenTree* node, unsigned regIndex);

    SpillTemp* begin() const
    {
        return temps;
    }

    SpillTemp* end() const
    {
        return temps + tempCount;
    }

#ifdef DEBUG
    unsigned GetDefCount() const
    {
        return defCount;
    }
#endif

private:
    SpillTemp* AllocTemp(var_types type);
    void ReleaseTemp(SpillTemp* temp);
    static unsigned GetTempListIndex(var_types type);
};
