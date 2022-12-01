// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "vartype.h"
#include "target.h"

class SpillTemp
{
public:
    SpillTemp* tdNext;

private:
    INDEBUG(static const int BAD_TEMP_OFFSET = 0xDDDDDDDD;)

    const int       tdNum;
    int             tdOffs;
    const uint8_t   tdSize;
    const var_types tdType;

public:
    SpillTemp(int tdNum, unsigned tdSize, var_types tdType)
        : tdNum(tdNum)
        ,
#ifdef DEBUG
        tdOffs(BAD_TEMP_OFFSET)
        ,
#endif
        tdSize(static_cast<uint8_t>(tdSize))
        , tdType(tdType)
    {
        assert(tdNum < 0);
    }

    int tdTempNum() const
    {
        assert(tdNum < 0);
        return tdNum;
    }

#ifdef DEBUG
    bool tdLegalOffset() const
    {
        return tdOffs != BAD_TEMP_OFFSET;
    }
#endif

    int tdTempOffs() const
    {
        assert(tdLegalOffset());
        return tdOffs;
    }

    void tdSetTempOffs(int offs)
    {
        tdOffs = offs;
        assert(tdLegalOffset());
    }

    void tdAdjustTempOffs(int offs)
    {
        tdOffs += offs;
        assert(tdLegalOffset());
    }

    unsigned tdTempSize() const
    {
        return tdSize;
    }

    var_types tdTempType() const
    {
        return tdType;
    }

    unsigned GetTempNum() const
    {
        return tdNum;
    }

    var_types GetType() const
    {
        return tdType;
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

    static var_types tmpNormalizeType(var_types type);
    void tmpPreAllocateTemps(var_types type, unsigned count);
    SpillTemp* tmpFindNum(int num) const;
    SpillTemp* tmpListBeg(TempState state = Free) const;
    SpillTemp* tmpListNxt(SpillTemp* temp, TempState state = Free) const;
    void tmpRlsTemp(SpillTemp* temp);

    SpillTemp* DefSpillTemp(GenTree* node, unsigned regIndex, var_types type);
    SpillTemp* UseSpillTemp(GenTree* node, unsigned regIndex);

#ifdef DEBUG
    bool tmpAllFree() const;
    bool rsSpillChk() const;
    void tmpEnd() const;
    void tmpDone() const;
#endif

private:
    SpillTemp* AllocTemp(var_types type);
    static unsigned GetTempListIndex(unsigned size);
};

using TempDsc = SpillTemp;
using RegSet  = SpillTempSet;
