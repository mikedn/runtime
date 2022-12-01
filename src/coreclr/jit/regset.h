// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*****************************************************************************/

#ifndef _REGSET_H
#define _REGSET_H
#include "vartype.h"
#include "target.h"

class LclVarDsc;
class Compiler;
class CodeGen;
class GCInfo;

class TempDsc
{
public:
    TempDsc* tdNext;

private:
    int tdOffs;
#ifdef DEBUG
    static const int BAD_TEMP_OFFSET = 0xDDDDDDDD; // used as a sentinel "bad value" for tdOffs in DEBUG
#endif                                             // DEBUG

    int       tdNum;
    BYTE      tdSize;
    var_types tdType;

public:
    TempDsc(int _tdNum, unsigned _tdSize, var_types _tdType) : tdNum(_tdNum), tdSize((BYTE)_tdSize), tdType(_tdType)
    {
#ifdef DEBUG
        // temps must have a negative number (so they have a different number from all local variables)
        assert(tdNum < 0);
        tdOffs = BAD_TEMP_OFFSET;
#endif // DEBUG
        if (tdNum != _tdNum)
        {
            IMPL_LIMITATION("too many spill temps");
        }
    }

#ifdef DEBUG
    bool tdLegalOffset() const
    {
        return tdOffs != BAD_TEMP_OFFSET;
    }
#endif // DEBUG

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

    int tdTempNum() const
    {
        assert(tdNum < 0);
        return tdNum;
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

class RegSet
{
    friend class CodeGen;
    friend class CodeGenInterface;

private:
    Compiler* m_rsCompiler;

public:
    RegSet::RegSet(Compiler* compiler) : m_rsCompiler(compiler)
    {
    }

    // The same descriptor is also used for 'multi-use' register tracking, BTW.
    struct SpillDsc
    {
        SpillDsc* spillNext; // next spilled value of same reg
        GenTree*  spillTree; // the value that was spilled
        TempDsc*  spillTemp; // the temp holding the spilled value

        static SpillDsc* alloc(RegSet* regSet);
        static void freeDsc(RegSet* regSet, SpillDsc* spillDsc);
    };

private:
    //-------------------------------------------------------------------------
    //
    //  The following tables keep track of spilled register values.
    //

    // When a register gets spilled, the old information is stored here
    SpillDsc* rsSpillDesc[REG_COUNT] = {};
    SpillDsc* rsSpillFree            = nullptr; // list of unused spill descriptors

    TempDsc* AllocSpillTemp(GenTree* node, regNumber reg, var_types type);
    SpillDsc* rsGetSpillInfo(GenTree* tree, regNumber reg, SpillDsc** pPrevDsc = nullptr);
    TempDsc* rsGetSpillTempWord(regNumber oldReg, SpillDsc* dsc, SpillDsc* prevDsc);

public:
    enum TEMP_USAGE_TYPE
    {
        TEMP_USAGE_FREE,
        TEMP_USAGE_USED
    };

    static var_types tmpNormalizeType(var_types type);
    TempDsc* tmpGetTemp(var_types type); // get temp for the given type
    void tmpRlsTemp(TempDsc* temp);
    TempDsc* tmpFindNum(int temp, TEMP_USAGE_TYPE usageType = TEMP_USAGE_FREE) const;

    TempDsc* tmpListBeg(TEMP_USAGE_TYPE usageType = TEMP_USAGE_FREE) const;
    TempDsc* tmpListNxt(TempDsc* curTemp, TEMP_USAGE_TYPE usageType = TEMP_USAGE_FREE) const;

#ifdef DEBUG
    bool tmpAllFree() const;
    bool rsSpillChk() const;
    void tmpDone() const;
    void tmpEnd() const;
#endif

    void tmpPreAllocateTemps(var_types type, unsigned count);

private:
    unsigned tmpCount = 0;             // Number of temps
    INDEBUG(unsigned tmpGetCount = 0;) // Temps which haven't been released yet

    static unsigned tmpSlot(unsigned size); // which slot in tmpFree[] or tmpUsed[] to use

    enum TEMP_CONSTANTS : unsigned
    {
#if defined(FEATURE_SIMD)
#if defined(TARGET_XARCH)
        TEMP_MAX_SIZE = YMM_REGSIZE_BYTES,
#elif defined(TARGET_ARM64)
        TEMP_MAX_SIZE = FP_REGSIZE_BYTES,
#endif // defined(TARGET_XARCH) || defined(TARGET_ARM64)
#else  // !FEATURE_SIMD
        TEMP_MAX_SIZE = sizeof(double),
#endif // !FEATURE_SIMD
        TEMP_SLOT_COUNT = (TEMP_MAX_SIZE / sizeof(int))
    };

    TempDsc* tmpFree[TEMP_MAX_SIZE / sizeof(int)] = {};
    TempDsc* tmpUsed[TEMP_MAX_SIZE / sizeof(int)] = {};
};

#endif // _REGSET_H
