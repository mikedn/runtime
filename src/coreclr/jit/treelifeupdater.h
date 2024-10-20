// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "compiler.h"
#include "emit.h"

class CodeGen;

struct DbgInfoVarLoc
{
    enum VarLocType
    {
        VLT_REG,
        VLT_REG_BYREF,
        VLT_REG_FP,
        VLT_STK,
        VLT_STK_BYREF,
        VLT_REG_REG,
        VLT_REG_STK,
        VLT_STK_REG,
        VLT_STK2,
        VLT_FPSTK,
        VLT_FIXED_VA,
        VLT_COUNT,
        VLT_INVALID
    };

    VarLocType vlType;

    union {
        // VLT_REG/VLT_REG_FP -- Any pointer-sized enregistered value (TYP_INT, TYP_REF, etc)
        // eg. EAX
        // VLT_REG_BYREF -- the specified register contains the address of the variable
        // eg. [EAX]

        struct
        {
            regNumber vlrReg;
        } vlReg;

        // VLT_STK       -- Any 32 bit value which is on the stack
        // eg. [ESP+0x20], or [EBP-0x28]
        // VLT_STK_BYREF -- the specified stack location contains the address of the variable
        // eg. mov EAX, [ESP+0x20]; [EAX]

        struct
        {
            regNumber vlsBaseReg;
            int32_t   vlsOffset;
        } vlStk;

        // VLT_REG_REG -- TYP_LONG/TYP_DOUBLE with both DWords enregistered
        // eg. RBM_EAXEDX

        struct
        {
            regNumber vlrrReg1;
            regNumber vlrrReg2;
        } vlRegReg;

        // VLT_REG_STK -- Partly enregistered TYP_LONG/TYP_DOUBLE
        // eg { LowerDWord=EAX UpperDWord=[ESP+0x8] }

        struct
        {
            regNumber vlrsReg;

            struct
            {
                regNumber vlrssBaseReg;
                int32_t   vlrssOffset;
            } vlrsStk;
        } vlRegStk;

        // VLT_STK_REG -- Partly enregistered TYP_LONG/TYP_DOUBLE
        // eg { LowerDWord=[ESP+0x8] UpperDWord=EAX }

        struct
        {
            struct
            {
                regNumber vlsrsBaseReg;
                int32_t   vlsrsOffset;
            } vlsrStk;

            regNumber vlsrReg;
        } vlStkReg;

        // VLT_STK2 -- Any 64 bit value which is on the stack, in 2 successive DWords
        // eg 2 DWords at [ESP+0x10]

        struct
        {
            regNumber vls2BaseReg;
            int32_t   vls2Offset;
        } vlStk2;

        // VLT_FPSTK -- enregisterd TYP_DOUBLE (on the FP stack)
        // eg. ST(3). Actually it is ST("FPstkHeight - vpFpStk")

        struct
        {
            unsigned vlfReg;
        } vlFPstk;

        // VLT_FIXED_VA -- fixed argument of a varargs function.
        // The argument location depends on the size of the variable
        // arguments (...). Inspecting the VARARGS_HANDLE indicates the
        // location of the first arg. This argument can then be accessed
        // relative to the position of the first arg

        struct
        {
            unsigned vlfvOffset;
        } vlFixedVarArg;

        // VLT_MEMORY

        struct
        {
            void* rpValue; // pointer to the in-process location of the value.
        } vlMemory;
    };

    DbgInfoVarLoc()
    {
    }

    DbgInfoVarLoc(const LclVarDsc* lcl, RegNum baseReg, int offset, bool isFramePointerUsed);
    void SetRegLocation(RegNum reg1);
    void SetRegLocation(RegNum reg1, RegNum reg2);
    void SetStackLocation(RegNum baseReg, int offset);

    INDEBUG(void Dump(const char* suffix = nullptr) const;)

#ifdef LATE_DISASM
    bool IsInReg(RegNum reg) const;
    bool IsOnStack(RegNum reg, int offset) const;
#endif

private:
    void InitRegLocation(const LclVarDsc* lcl, var_types type, RegNum baseReg, int offset, bool isFramePointerUsed);
    void InitStackLocation(const LclVarDsc* lcl, var_types type, RegNum baseReg, int offset, bool isFramePointerUsed);
};

bool operator==(const DbgInfoVarLoc& x, const DbgInfoVarLoc& y);
bool operator!=(const DbgInfoVarLoc& x, const DbgInfoVarLoc& y);

struct DbgInfoVarRange
{
    DbgInfoVarRange* next = nullptr;
    emitLocation     startOffset;
    emitLocation     endOffset;
    DbgInfoVarLoc    location;

    DbgInfoVarRange(DbgInfoVarLoc location) : location(location)
    {
    }

    INDEBUG(void Dump(const char* suffix = nullptr) const;)
};

class DbgInfoVar
{
    DbgInfoVarRange* firstRange = nullptr;
    DbgInfoVarRange* lastRange  = nullptr;
    unsigned         count      = 0;

public:
    bool HasOpenRange() const
    {
        return (lastRange != nullptr) && !lastRange->endOffset.Valid();
    }

    DbgInfoVarRange* GetRanges() const
    {
        return firstRange;
    }

    unsigned GetRangeCount() const
    {
        return count;
    }

    DbgInfoVarRange* StartRange(CodeGen* codeGen, const DbgInfoVarLoc& varLoc);
    DbgInfoVarRange* EndRange(CodeGen* codeGen);

    void InsertRangeFront(DbgInfoVarRange* range);
};

// Handles changes in variable liveness from a given node.
// Keeps set of temporary VARSET_TP during its lifetime to avoid unnecessary memory allocations.
class CodeGenLivenessUpdater
{
    Compiler* compiler;
    VARSET_TP currentLife;
    VARSET_TP liveGCLcl;
    regMaskTP liveLclRegs     = RBM_NONE;
    regMaskTP liveGCRefRegs   = RBM_NONE;
    regMaskTP liveGCByRefRegs = RBM_NONE;

    unsigned    dbgInfoVarCount        = 0;
    DbgInfoVar* dbgInfoVars            = nullptr;
    IL_OFFSET   prevBlockEndILOffset   = 0;
    bool        lastBlockHasBeenEmited = false;
#ifdef FEATURE_EH_FUNCLETS
    bool inFuncletRegion = false;
#endif

#ifdef DEBUG
    VARSET_TP scratchSet1;
    VARSET_TP scratchSet2;
#endif

    void UpdateLifePromoted(CodeGen* codeGen, GenTreeLclVarCommon* lclNode);

    void SetLiveLclRegs(regMaskTP regs);
    void UpdateLiveLclRegs(const LclVarDsc* lcl, bool isDying);

    void AddGCRefRegs(regMaskTP regMask DEBUGARG(bool forceOutput = false));
    void AddGCByRefRegs(regMaskTP regMask DEBUGARG(bool forceOutput = false));

    int GetVarStackOffset(CodeGen* codeGen, const LclVarDsc* lcl) const;
    DbgInfoVarLoc GetVarLocation(CodeGen* codeGen, const LclVarDsc* lcl) const;

    void StartRange(CodeGen* codeGen, const LclVarDsc* lcl);
    void UpdateRange(CodeGen* codeGen, const LclVarDsc* lcl);
    void EndRange(CodeGen* codeGen, const LclVarDsc* lcl);

#ifdef DEBUG
    void DumpDiff(CodeGen* codeGen);
    void DumpGCRefRegsDiff(regMaskTP gcRegGCrefSetNew DEBUGARG(bool forceOutput = false));
    void DumpGCByRefRegsDiff(regMaskTP gcRegByrefSetNew DEBUGARG(bool forceOutput = false));
#endif

public:
    CodeGenLivenessUpdater(Compiler* compiler) : compiler(compiler)
    {
    }

    void Begin();
    void End(CodeGen* codeGen);
    void BeginBlockCodeGen(CodeGen* codeGen, BasicBlock* block, const RegNumSmall* map);
    void BeginPrologEpilogCodeGen();

    void UpdateLife(CodeGen* codeGen, GenTreeLclVarCommon* lclNode);
    void UpdateLifeMultiReg(CodeGen* codeGen, GenTreeLclStore* store);

    void MoveReg(CodeGen* codeGen, LclVarDsc* lcl, GenTreeLclLoad* src, GenTreeCopyOrReload* dst);
    void Spill(LclVarDsc* lcl, GenTreeLclVar* lclNode);
    void Unspill(CodeGen* codeGen, LclVarDsc* lcl, GenTreeLclLoad* src, RegNum dstReg, var_types dstType);

    VARSET_TP GetGCLiveSet() const
    {
        return liveGCLcl;
    }

    void AddGCSlot(LclVarDsc* lcl);
    void RemoveGCSlot(LclVarDsc* lcl);

    regMaskTP GetLiveLclRegs() const
    {
        return liveLclRegs;
    }

    void AddLiveLclRegs(regMaskTP regs)
    {
        SetLiveLclRegs(liveLclRegs | regs);
    }

    void RemoveLiveLclRegs(regMaskTP regs)
    {
        SetLiveLclRegs(liveLclRegs & ~regs);
    }

    void RemoveGCRegs(regMaskTP regMask DEBUGARG(bool forceOutput = false));
    void SetGCRegType(regNumber reg, var_types type);
    void TransferGCRegType(regNumber dst, regNumber src);

    void SetGCRegs(var_types type, regMaskTP regs)
    {
        switch (type)
        {
            case TYP_REF:
                liveGCRefRegs = regs;
                break;
            case TYP_BYREF:
                liveGCByRefRegs = regs;
                break;
            default:
                assert(!"Bad GC reg type");
                break;
        }
    }

    regMaskTP GetGCRegs(var_types type) const
    {
        switch (type)
        {
            case TYP_REF:
                return liveGCRefRegs;
            case TYP_BYREF:
                return liveGCByRefRegs;
            default:
                assert(!"Bad GC reg type");
                return RBM_NONE;
        }
    }

    regMaskTP GetGCRegs(emitAttr attr) const
    {
        if (EA_IS_GCREF(attr))
        {
            return liveGCRefRegs;
        }

        if (EA_IS_BYREF(attr))
        {
            return liveGCByRefRegs;
        }

        assert(!"Bad GC type");
        return RBM_NONE;
    }

    regMaskTP GetGCRegs() const
    {
        return liveGCRefRegs | liveGCByRefRegs;
    }

    void StartUntrackedVarsRanges(CodeGen*    codeGen,
                                  BasicBlock* block,
                                  unsigned*   nextEnterScope,
                                  unsigned*   nextExitScope);

    void CreatePrologDbgInfoRanges(CodeGen* codeGen);
    void EndCodeGen(CodeGen* codeGen);

    DbgInfoVarRange* GetDbgInfoRanges(unsigned lclNum) const;
    unsigned GetDbgInfoRangeCount() const;

#ifdef DEBUG
    void VerifyLiveGCRegs(BasicBlock* block);
    void VerifyLiveRegVars(BasicBlock* block);
#endif
};
