#include "jitpch.h"
#include "treelifeupdater.h"
#include "codegen.h"

void CodeGenLivenessUpdater::Begin()
{
    currentLife = VarSetOps::MakeEmpty(compiler);
    varDeltaSet = VarSetOps::MakeEmpty(compiler);
    liveGCLcl   = VarSetOps::MakeEmpty(compiler);

#ifdef DEBUG
    scratchSet1 = VarSetOps::MakeEmpty(compiler);
    scratchSet2 = VarSetOps::MakeEmpty(compiler);
    epoch       = compiler->GetCurLVEpoch();
#endif
}

void CodeGenLivenessUpdater::End(CodeGen* codeGen)
{
    if (VarSetOps::IsEmpty(compiler, currentLife))
    {
        return;
    }

    liveGCRefRegs   = RBM_NONE;
    liveGCByRefRegs = RBM_NONE;
    liveLclRegs     = RBM_NONE;

    VarSetOps::ClearD(compiler, currentLife);
    VarSetOps::ClearD(compiler, liveGCLcl);

#ifdef USING_VARIABLE_LIVE_RANGE
    // TODO-MIKE-Review: This might be dead code, it looks like siEndAllVariableLiveRange
    // has already been called and as a result siEndVariableLiveRange does nothing.
    for (VarSetOps::Enumerator e(compiler, currentLife); e.MoveNext();)
    {
        unsigned lclNum = compiler->lvaTrackedIndexToLclNum(e.Current());

        codeGen->getVariableLiveKeeper()->siEndVariableLiveRange(lclNum);
    }
#endif

#ifdef USING_SCOPE_INFO
    codeGen->siUpdate();
#endif
}

void CodeGenLivenessUpdater::BeginBlock()
{
    currentNode     = nullptr;
    liveLclRegs     = RBM_NONE;
    liveGCRefRegs   = RBM_NONE;
    liveGCByRefRegs = RBM_NONE;
}

static regMaskTP GetLclRegs(const LclVarDsc* lcl)
{
    assert(lcl->lvIsInReg());

    if (varTypeUsesFloatReg(lcl->GetType()))
    {
        return genRegMaskFloat(lcl->GetRegNum(), lcl->GetType());
    }

    return genRegMask(lcl->GetRegNum());
}

void CodeGenLivenessUpdater::ChangeLife(CodeGen* codeGen, VARSET_VALARG_TP newLife)
{
    DBEXEC(compiler->verbose, compiler->dmpVarSetDiff("Live vars at start of block: ", currentLife, newLife);)

    if (VarSetOps::Equal(compiler, currentLife, newLife))
    {
        return;
    }

    DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet1, liveGCLcl));

    VarSetOps::Assign(compiler, varDeltaSet, currentLife);
    VarSetOps::DiffD(compiler, varDeltaSet, newLife);

    for (VarSetOps::Enumerator e(compiler, varDeltaSet); e.MoveNext();)
    {
        unsigned   lclNum     = compiler->lvaTrackedIndexToLclNum(e.Current());
        LclVarDsc* lcl        = compiler->lvaGetDesc(lclNum);
        bool       isGCRef    = lcl->TypeIs(TYP_REF);
        bool       isByRef    = lcl->TypeIs(TYP_BYREF);
        bool       isInReg    = lcl->lvIsInReg();
        bool       isInMemory = !isInReg || lcl->IsAlwaysAliveInMemory();

        if (isInReg)
        {
            // TODO-Cleanup: Move the code from compUpdateLifeVar to UpdateLiveLclRegs
            // that updates the gc sets
            regMaskTP lclRegs = GetLclRegs(lcl);

            if (isGCRef)
            {
                liveGCRefRegs &= ~lclRegs;
            }
            else if (isByRef)
            {
                liveGCByRefRegs &= ~lclRegs;
            }

            UpdateLiveLclRegs(lcl, /*isDying*/ true);
        }

        if (isInMemory && (isGCRef || isByRef) && lcl->HasGCSlotLiveness())
        {
            VarSetOps::RemoveElemD(compiler, liveGCLcl, e.Current());
        }

#ifdef USING_VARIABLE_LIVE_RANGE
        codeGen->getVariableLiveKeeper()->siEndVariableLiveRange(lclNum);
#endif
    }

    VarSetOps::Assign(compiler, varDeltaSet, newLife);
    VarSetOps::DiffD(compiler, varDeltaSet, currentLife);

    for (VarSetOps::Enumerator e(compiler, varDeltaSet); e.MoveNext();)
    {
        unsigned   lclNum  = compiler->lvaTrackedIndexToLclNum(e.Current());
        LclVarDsc* lcl     = compiler->lvaGetDesc(lclNum);
        bool       isGCRef = lcl->TypeIs(TYP_REF);
        bool       isByRef = lcl->TypeIs(TYP_BYREF);

        if (lcl->lvIsInReg())
        {
            // If this variable is going live in a register, it is no longer live on the stack,
            // unless it is an EH var, which always remains live on the stack.
            if (!lcl->IsAlwaysAliveInMemory() && lcl->HasGCSlotLiveness())
            {
                VarSetOps::RemoveElemD(compiler, liveGCLcl, e.Current());
            }

            UpdateLiveLclRegs(lcl, /*isDying*/ false);

            regMaskTP lclRegs = GetLclRegs(lcl);

            if (isGCRef)
            {
                liveGCRefRegs |= lclRegs;
            }
            else if (isByRef)
            {
                liveGCByRefRegs |= lclRegs;
            }
        }
        else if (lcl->HasGCSlotLiveness())
        {
            VarSetOps::AddElemD(compiler, liveGCLcl, e.Current());
        }

#ifdef USING_VARIABLE_LIVE_RANGE
        codeGen->getVariableLiveKeeper()->siStartVariableLiveRange(lcl, lclNum);
#endif
    }

    VarSetOps::Assign(compiler, currentLife, newLife);

#ifdef USING_SCOPE_INFO
    codeGen->siUpdate();
#endif

    DBEXEC(compiler->verbose, compiler->dmpVarSetDiff("GC stack vars: ", scratchSet1, liveGCLcl);)
}

void CodeGenLivenessUpdater::UpdateLife(CodeGen* codeGen, GenTreeLclVarCommon* lclNode)
{
    assert(lclNode->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD) && !lclNode->IsMultiRegLclVar());
    assert(compiler->GetCurLVEpoch() == epoch);

    // TODO-Cleanup: We shouldn't really be calling this more than once
    if (lclNode == currentNode)
    {
        return;
    }

    currentNode = lclNode;

    LclVarDsc* lcl = compiler->lvaGetDesc(lclNode);

    if (!lcl->HasLiveness())
    {
        if (!lcl->IsAddressExposed() && lcl->IsPromoted())
        {
            UpdateLifePromoted(codeGen, lclNode);
        }

        return;
    }

    bool isBorn  = ((lclNode->gtFlags & GTF_VAR_DEF) != 0) && ((lclNode->gtFlags & GTF_VAR_USEASG) == 0);
    bool isDying = (lclNode->gtFlags & GTF_VAR_DEATH) != 0;
    bool spill   = lclNode->IsAnyRegSpill();

    if (isBorn || isDying)
    {
        DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet1, currentLife);)
        DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet2, liveGCLcl);)

        if (isBorn && lcl->IsRegCandidate() && (lclNode->GetRegNum() != REG_NA))
        {
            lcl->SetRegNum(lclNode->GetRegNum());
        }

        bool isInReg    = lcl->lvIsInReg() && (lclNode->GetRegNum() != REG_NA);
        bool isInMemory = !isInReg || lcl->IsAlwaysAliveInMemory();

        if (isInReg)
        {
            UpdateLiveLclRegs(lcl, isDying);
        }

        DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet1, currentLife);)

        bool changed;

        if (isDying)
        {
            // TODO-MIKE-Review: Why does the assert below fail? Old comment
            // mentions QMARKs but there's no such thing in LIR. CopyProp
            // liveness had a similar issue but the same fix isn't sufficient
            // here.
            //
            // assert(VarSetOps::IsMember(compiler, newLife, lcl->GetLivenessBitIndex()));

            changed = VarSetOps::TryRemoveElemD(compiler, currentLife, lcl->GetLivenessBitIndex());
        }
        else
        {
            // This shouldn't be in newLife, unless this is debug code, in which
            // case we keep vars live everywhere, OR the variable is address-exposed,
            // OR this block is part of a try block, in which case it may be live at the handler
            // Could add a check that, if it's in newLife, that it's also in
            // fgGetHandlerLiveVars(compCurBB), but seems excessive
            //
            // For a dead store, it can be the case that we set both isBorn and isDying to true.
            // (We don't eliminate dead stores under MinOpts, so we can't assume they're always
            // eliminated.)  If it's both, we handled it above.
            changed = VarSetOps::TryAddElemD(compiler, currentLife, lcl->GetLivenessBitIndex());
        }

        if (changed)
        {
            if (isInMemory && lcl->HasGCSlotLiveness())
            {
                if (isBorn)
                {
                    VarSetOps::AddElemD(compiler, liveGCLcl, lcl->GetLivenessBitIndex());
                }
                else
                {
                    VarSetOps::RemoveElemD(compiler, liveGCLcl, lcl->GetLivenessBitIndex());
                }
            }

            DBEXEC(compiler->verbose, DumpDiff(codeGen);)

#ifdef USING_VARIABLE_LIVE_RANGE
            codeGen->getVariableLiveKeeper()->siStartOrCloseVariableLiveRange(lcl, lclNode->GetLclNum(), isBorn,
                                                                              isDying);
#endif

#ifdef USING_SCOPE_INFO
            codeGen->siUpdate();
#endif
        }
    }

    if (spill)
    {
        // TODO-MIKE-Review: There's somehting dubious going on here, or perhaps in LSRA. On ARM64 a last-use
        // gets spilled and that results in an assert in "variable range". The range was already closed above
        // and SpillRegCandidateLclVar tries to update it for spill. It may be that SpillRegCandidateLclVar
        // needs to use siStartOrCloseVariableLiveRange instead of siUpdateVariableLiveRange in this case but
        // then it's not clear why would a last-use need spilling to begin with.
        codeGen->SpillRegCandidateLclVar(lclNode->AsLclVar());

        if (lcl->HasGCSlotLiveness() && VarSetOps::TryAddElemD(compiler, liveGCLcl, lcl->lvVarIndex))
        {
            JITDUMP("GC pointer V%02u becoming live on stack\n", lclNode->GetLclNum());
        }
    }
}

void CodeGenLivenessUpdater::UpdateLifeMultiReg(CodeGen* codeGen, GenTreeLclVar* lclNode)
{
    assert(lclNode->OperIs(GT_STORE_LCL_VAR) && ((lclNode->gtFlags & GTF_VAR_USEASG) == 0));

    DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet1, currentLife);)
    DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet2, liveGCLcl);)

    LclVarDsc* lcl = compiler->lvaGetDesc(lclNode);

    assert(lcl->IsIndependentPromoted());

    for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
    {
        LclVarDsc* fieldLcl = compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

        bool isInReg      = fieldLcl->lvIsInReg();
        bool isInMemory   = !isInReg || fieldLcl->IsAlwaysAliveInMemory();
        bool isFieldDying = lclNode->IsLastUse(i);

        if (isInReg)
        {
            UpdateLiveLclRegs(fieldLcl, isFieldDying DEBUGARG(lclNode));
        }

        if (isFieldDying)
        {
            VarSetOps::RemoveElemD(compiler, currentLife, fieldLcl->GetLivenessBitIndex());
        }
        else
        {
            VarSetOps::AddElemD(compiler, currentLife, fieldLcl->GetLivenessBitIndex());
        }

        if (isInMemory && fieldLcl->HasGCSlotLiveness())
        {
            // TODO-MIKE-Review: Should we remove the local from the GC var set when the field is dying?
            // The "scalar" version of this code doesn't do it, it checks "isBorn" instead of "isDying".

            VarSetOps::AddElemD(compiler, liveGCLcl, fieldLcl->GetLivenessBitIndex());
        }
    }

    DBEXEC(compiler->verbose, DumpDiff(codeGen);)
}

void CodeGenLivenessUpdater::UpdateLifePromoted(CodeGen* codeGen, GenTreeLclVarCommon* lclNode)
{
    assert(!lclNode->IsMultiRegLclVar() && !lclNode->IsAnyRegSpill());

    bool isBorn  = lclNode->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD);
    bool isDying = lclNode->HasLastUse();

    if (!isBorn && !isDying)
    {
        return;
    }

    DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet1, currentLife);)
    DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet2, liveGCLcl);)

    LclVarDsc* lcl = compiler->lvaGetDesc(lclNode);

    unsigned lclOffset    = 0;
    unsigned lclEndOffset = lcl->TypeIs(TYP_STRUCT) ? lcl->GetLayout()->GetSize() : varTypeSize(lcl->GetType());

    if (GenTreeLclFld* lclFld = lclNode->IsLclFld())
    {
        lclOffset    = lclFld->GetLclOffs();
        lclEndOffset = lclOffset + (lclFld->TypeIs(TYP_STRUCT) ? lclFld->GetLayout(compiler)->GetSize()
                                                               : varTypeSize(lclFld->GetType()));
    }

    for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
    {
        LclVarDsc* fieldLcl = compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

        assert(!fieldLcl->TypeIs(TYP_STRUCT));

        unsigned fieldOffset    = fieldLcl->GetPromotedFieldOffset();
        unsigned fieldEndOffset = fieldOffset + varTypeSize(fieldLcl->GetType());
        bool     partialOverlap = (fieldOffset < lclEndOffset) && (fieldEndOffset > lclOffset);

        if (!partialOverlap)
        {
            continue;
        }

        if (!fieldLcl->HasLiveness())
        {
            continue;
        }

        bool totalOverlap = (lclOffset <= fieldOffset) && (fieldEndOffset <= lclEndOffset);

        if (lclNode->IsLastUse(i))
        {
            VarSetOps::RemoveElemD(compiler, currentLife, fieldLcl->GetLivenessBitIndex());
        }
        else
        {
            VarSetOps::AddElemD(compiler, currentLife, fieldLcl->GetLivenessBitIndex());
        }

        if (fieldLcl->HasGCSlotLiveness())
        {
            // TODO-MIKE-Review: Should we remove the local from the GC var set when the field is dying?
            // The "scalar" version of this code doesn't do it, it checks "isBorn" instead of "isDying".

            if (isBorn)
            {
                VarSetOps::AddElemD(compiler, liveGCLcl, fieldLcl->GetLivenessBitIndex());
            }
            else
            {
                VarSetOps::RemoveElemD(compiler, liveGCLcl, fieldLcl->GetLivenessBitIndex());
            }
        }
    }

    DBEXEC(compiler->verbose, DumpDiff(codeGen);)
}

void CodeGenLivenessUpdater::BeginBlockCodeGen(BasicBlock* block)
{
    regMaskTP newLiveRegSet  = RBM_NONE;
    regMaskTP newRegGCrefSet = RBM_NONE;
    regMaskTP newRegByrefSet = RBM_NONE;
#ifdef DEBUG
    VARSET_TP removedGCVars = VarSetOps::MakeEmpty(compiler);
    VARSET_TP addedGCVars   = VarSetOps::MakeEmpty(compiler);
#endif

    for (VarSetOps::Enumerator en(compiler, block->bbLiveIn); en.MoveNext();)
    {
        LclVarDsc* lcl = compiler->lvaGetDescByTrackedIndex(en.Current());

        if (lcl->lvIsInReg())
        {
            regMaskTP lclRegs = GetLclRegs(lcl);

            newLiveRegSet |= lclRegs;

            if (lcl->TypeIs(TYP_REF))
            {
                newRegGCrefSet |= lclRegs;
            }
            else if (lcl->TypeIs(TYP_BYREF))
            {
                newRegByrefSet |= lclRegs;
            }

            if (!lcl->IsAlwaysAliveInMemory() && lcl->HasGCSlotLiveness())
            {
#ifdef DEBUG
                if (compiler->verbose && VarSetOps::IsMember(compiler, liveGCLcl, en.Current()))
                {
                    VarSetOps::AddElemD(compiler, removedGCVars, en.Current());
                }
#endif

                VarSetOps::RemoveElemD(compiler, liveGCLcl, en.Current());
            }
        }

        if ((!lcl->lvIsInReg() || lcl->IsAlwaysAliveInMemory()) && lcl->HasGCSlotLiveness())
        {
#ifdef DEBUG
            if (compiler->verbose && !VarSetOps::IsMember(compiler, liveGCLcl, en.Current()))
            {
                VarSetOps::AddElemD(compiler, addedGCVars, en.Current());
            }
#endif

            VarSetOps::AddElemD(compiler, liveGCLcl, en.Current());
        }
    }

    SetLiveLclRegs(newLiveRegSet);

#ifdef DEBUG
    if (compiler->verbose)
    {
        if (!VarSetOps::IsEmpty(compiler, addedGCVars))
        {
            printf("Added GCVars: ");
            dumpConvertedVarSet(compiler, addedGCVars);
            printf("\n");
        }

        if (!VarSetOps::IsEmpty(compiler, removedGCVars))
        {
            printf("Removed GCVars: ");
            dumpConvertedVarSet(compiler, removedGCVars);
            printf("\n");
        }
    }
#endif // DEBUG

    AddGCRefRegs(newRegGCrefSet DEBUGARG(true));
    AddGCByRefRegs(newRegByrefSet DEBUGARG(true));

    if (handlerGetsXcptnObj(block->bbCatchTyp))
    {
        for (GenTree* node : LIR::AsRange(block))
        {
            if (node->OperIs(GT_CATCH_ARG))
            {
                AddGCRefRegs(RBM_EXCEPTION_OBJECT);
                break;
            }
        }
    }
}

void CodeGenLivenessUpdater::BeginPrologCodeGen()
{
    liveGCRefRegs   = RBM_NONE;
    liveGCByRefRegs = RBM_NONE;
    VarSetOps::ClearD(compiler, liveGCLcl);
}

void CodeGenLivenessUpdater::BeginMethodEpilogCodeGen()
{
    emitter* emitter = compiler->codeGen->GetEmitter();

    VarSetOps::Assign(compiler, liveGCLcl, emitter->emitInitGCrefVars);
    liveGCRefRegs   = emitter->emitInitGCrefRegs;
    liveGCByRefRegs = emitter->emitInitByrefRegs;

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("liveGCLcl ");
        dumpConvertedVarSet(compiler, liveGCLcl);
        printf(", liveGCRefRegs");
        emitter::emitDispRegSet(liveGCRefRegs);
        printf(", liveGCByRefRegs");
        emitter::emitDispRegSet(liveGCByRefRegs);
        printf("\n");
    }
#endif
}

void CodeGenLivenessUpdater::SpillGCSlot(LclVarDsc* lcl)
{
    RemoveGCRegs(GetLclRegs(lcl));

    if (!lcl->HasGCSlotLiveness())
    {
        return;
    }

#ifdef DEBUG
    if (!VarSetOps::IsMember(compiler, liveGCLcl, lcl->GetLivenessBitIndex()))
    {
        JITDUMP("GC slot V%02u becoming live\n", lcl - compiler->lvaTable);
    }
    else
    {
        JITDUMP("GC slot V%02u continuing live\n", lcl - compiler->lvaTable);
    }
#endif

    VarSetOps::AddElemD(compiler, liveGCLcl, lcl->GetLivenessBitIndex());
}

void CodeGenLivenessUpdater::UnspillGCSlot(LclVarDsc* lcl DEBUGARG(GenTreeLclVar* lclVar))
{
    if (!lcl->IsAlwaysAliveInMemory())
    {
        RemoveGCSlot(lcl);
    }

    JITDUMP("V%02u in reg %s is becoming live at [%06u]\n", lcl - compiler->lvaTable, getRegName(lcl->GetRegNum()),
            lclVar->GetID());

    AddLiveLclRegs(GetLclRegs(lcl));
}

void CodeGenLivenessUpdater::RemoveGCSlot(LclVarDsc* lcl)
{
    if (!lcl->HasGCSlotLiveness())
    {
        return;
    }

#ifdef DEBUG
    if (VarSetOps::IsMember(compiler, liveGCLcl, lcl->GetLivenessBitIndex()))
    {
        JITDUMP("GC slot V%02u becoming dead\n", lcl - compiler->lvaTable);
    }
#endif

    VarSetOps::RemoveElemD(compiler, liveGCLcl, lcl->GetLivenessBitIndex());
}

void CodeGenLivenessUpdater::UpdateLiveLclRegs(const LclVarDsc* lcl, bool isDying DEBUGARG(GenTree* node))
{
    regMaskTP regs = GetLclRegs(lcl);

    JITDUMP("V%02u in reg %s is becoming %s at [%06u]\n", (lcl - compiler->lvaTable), getRegName(lcl->GetRegNum()),
            isDying ? "dead" : "live", node == nullptr ? 0 : node->GetID());

    if (isDying)
    {
        // We'd like to be able to assert the following, however if we are walking
        // through a qmark/colon tree, we may encounter multiple last-use nodes.
        // assert((liveness.GetLiveLclRegs() & regMask) == regMask);

        RemoveLiveLclRegs(regs);
    }
    else
    {
        // If this is going live, the register must not have a variable in it, except
        // in the case of an exception or "spill at single-def" variable, which may be
        // already treated as live in the register.
        assert(lcl->IsAlwaysAliveInMemory() || ((liveLclRegs & regs) == RBM_NONE));

        AddLiveLclRegs(regs);
    }
}

void CodeGenLivenessUpdater::SetLiveLclRegs(regMaskTP regs)
{
    DBEXEC(compiler->verbose, emitter::emitDispRegSetDiff("Live regs: ", liveLclRegs, regs);)

    liveLclRegs = regs;
}

void CodeGenLivenessUpdater::AddGCRefRegs(regMaskTP regs DEBUGARG(bool forceOutput))
{
    assert((liveGCByRefRegs & regs) == RBM_NONE);

    regMaskTP newRefRegs   = liveGCRefRegs | regs;
    regMaskTP newByRefRegs = liveGCByRefRegs & ~regs;

    INDEBUG(DumpGCRefRegsDiff(newRefRegs, forceOutput));
    INDEBUG(DumpGCByRefRegsDiff(newByRefRegs));

    liveGCRefRegs   = newRefRegs;
    liveGCByRefRegs = newByRefRegs;
}

void CodeGenLivenessUpdater::AddGCByRefRegs(regMaskTP regs DEBUGARG(bool forceOutput))
{
    regMaskTP newRefRegs   = liveGCRefRegs & ~regs;
    regMaskTP newByRefRegs = liveGCByRefRegs | regs;

    INDEBUG(DumpGCRefRegsDiff(newRefRegs));
    INDEBUG(DumpGCByRefRegsDiff(newByRefRegs, forceOutput));

    liveGCRefRegs   = newRefRegs;
    liveGCByRefRegs = newByRefRegs;
}

void CodeGenLivenessUpdater::RemoveGCRegs(regMaskTP regs DEBUGARG(bool forceOutput))
{
    regMaskTP newRefRegs   = liveGCRefRegs & ~(regs & ~liveLclRegs);
    regMaskTP newByRefRegs = liveGCByRefRegs & ~(regs & ~liveLclRegs);

    INDEBUG(DumpGCRefRegsDiff(newRefRegs, forceOutput));
    INDEBUG(DumpGCByRefRegsDiff(newByRefRegs, forceOutput));

    liveGCRefRegs   = newRefRegs;
    liveGCByRefRegs = newByRefRegs;
}

void CodeGenLivenessUpdater::SetGCRegType(regNumber reg, var_types type)
{
    regMaskTP regs = genRegMask(reg);

    switch (type)
    {
        case TYP_REF:
            AddGCRefRegs(regs);
            break;
        case TYP_BYREF:
            AddGCByRefRegs(regs);
            break;
        default:
            RemoveGCRegs(regs);
            break;
    }
}

void CodeGenLivenessUpdater::TransferGCRegType(regNumber dst, regNumber src)
{
    regMaskTP srcMask = genRegMask(src);
    regMaskTP dstMask = genRegMask(dst);

    if ((GetGCRegs(TYP_REF) & srcMask) != RBM_NONE)
    {
        AddGCRefRegs(dstMask);
    }
    else if ((GetGCRegs(TYP_BYREF) & srcMask) != RBM_NONE)
    {
        AddGCByRefRegs(dstMask);
    }
    else
    {
        RemoveGCRegs(dstMask);
    }
}

#ifdef DEBUG
void CodeGenLivenessUpdater::DumpDiff(CodeGen* codeGen)
{
    if (!VarSetOps::Equal(compiler, scratchSet1, currentLife))
    {
        compiler->dmpVarSetDiff("Live vars: ", scratchSet1, currentLife);
    }

    if (!VarSetOps::Equal(compiler, scratchSet2, liveGCLcl))
    {
        compiler->dmpVarSetDiff("GC stack vars: ", scratchSet2, liveGCLcl);
    }
}

void CodeGenLivenessUpdater::DumpGCRefRegsDiff(regMaskTP newRegs DEBUGARG(bool forceOutput))
{
    if (compiler->verbose && (forceOutput || (liveGCRefRegs != newRegs)))
    {
        emitter::emitDispRegSetDiff("GC regs: ", liveGCRefRegs, newRegs);
    }
}

void CodeGenLivenessUpdater::DumpGCByRefRegsDiff(regMaskTP newRegs DEBUGARG(bool forceOutput))
{
    if (compiler->verbose && (forceOutput || (liveGCByRefRegs != newRegs)))
    {
        emitter::emitDispRegSetDiff("Byref regs: ", liveGCByRefRegs, newRegs);
    }
}
#endif // DEBUG
