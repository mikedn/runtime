// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "compiler.h"

// Handles changes in variable liveness from a given node.
// Keeps set of temporary VARSET_TP during its lifetime to avoid unnecessary memory allocations.
class CodeGenLivenessUpdater
{
    Compiler* compiler;
    GenTree*  currentNode;
    VARSET_TP currentLife;
    VARSET_TP varDeltaSet;
    VARSET_TP varStackGCPtrDeltaSet;
    VARSET_TP liveGCLcl;
    regMaskTP liveLclRegs     = RBM_NONE;
    regMaskTP liveGCRefRegs   = RBM_NONE;
    regMaskTP liveGCByRefRegs = RBM_NONE;
#ifdef DEBUG
    VARSET_TP scratchSet1;
    VARSET_TP scratchSet2;
    unsigned  epoch;
#endif

    void UpdateLifePromoted(class CodeGen* codeGen, GenTreeLclVarCommon* lclNode);

    void SetLiveLclRegs(regMaskTP regs);

    void AddGCRefRegs(regMaskTP regMask DEBUGARG(bool forceOutput = false));
    void AddGCByRefRegs(regMaskTP regMask DEBUGARG(bool forceOutput = false));

#ifdef DEBUG
    void DumpDiff(class CodeGen* codeGen);
    void DumpGCRefRegsDiff(regMaskTP gcRegGCrefSetNew DEBUGARG(bool forceOutput = false));
    void DumpGCByRefRegsDiff(regMaskTP gcRegByrefSetNew DEBUGARG(bool forceOutput = false));
#endif

public:
    CodeGenLivenessUpdater(Compiler* compiler) : compiler(compiler)
    {
    }

    void Begin();
    void BeginBlock();
    void BeginBlockCodeGen(BasicBlock* block);
    void BeginPrologCodeGen();
    void BeginMethodEpilogCodeGen();

    void ChangeLife(class CodeGen* codeGen, VARSET_VALARG_TP newLife);
    void UpdateLife(class CodeGen* codeGen, GenTreeLclVarCommon* lclNode);
    void UpdateLifeMultiReg(class CodeGen* codeGen, GenTreeLclVar* lclNode);

    VARSET_VALARG_TP GetLiveSet() const
    {
        return currentLife;
    }

    VARSET_TP& GetGCLiveSet()
    {
        return liveGCLcl;
    }

    void RemoveGCSlot(LclVarDsc* lcl)
    {
        if (lcl->HasGCSlotLiveness())
        {
            VarSetOps::RemoveElemD(compiler, liveGCLcl, lcl->GetLivenessBitIndex());
        }
    }

    regMaskTP GetLiveLclRegs() const
    {
        return liveLclRegs;
    }

    void UpdateLiveLclRegs(const LclVarDsc* lcl, bool isDying DEBUGARG(GenTree* node = nullptr));

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

    regMaskTP GetGCRegs() const
    {
        return liveGCRefRegs | liveGCByRefRegs;
    }
};
