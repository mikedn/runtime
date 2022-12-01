// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "vartype.h"
#include "target.h"

class SpillTemp
{
    INDEBUG(static constexpr int BAD_TEMP_OFFSET = 0xDDDDDDDD;)

    friend class SpillTempSet;

    SpillTemp*      next;
    const int       num;
    int             offset;
    const uint8_t   size;
    const var_types type;

public:
    SpillTemp(int num, unsigned size, var_types type)
        : num(num)
#ifdef DEBUG
        , offset(BAD_TEMP_OFFSET)
#endif
        , size(static_cast<uint8_t>(size))
        , type(type)
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

    unsigned GetSize() const
    {
        return size;
    }

#ifdef DEBUG
    bool IsAllocated() const
    {
        return offset != BAD_TEMP_OFFSET;
    }
#endif

    int GetOffset() const
    {
        assert(IsAllocated());
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
    SpillTemp*      usedTemps[TempListCount]{};
    SpillTemp*      temps     = nullptr;
    unsigned        tempCount = 0;
    INDEBUG(unsigned usedTempCount = 0;)

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
    void ReleaseTemp(SpillTemp* temp);

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
    bool AreAllTempsFree() const;
    bool AreAllSpillDefsFree() const;
    void End() const;
    void Done() const;
#endif

private:
    SpillTemp* AllocTemp(var_types type);
    static unsigned GetTempListIndex(unsigned size);
};
