#include "jitpch.h"
#include "treelifeupdater.h"
#include "codegen.h"

void CodeGenLivenessUpdater::Begin()
{
    currentLife           = VarSetOps::MakeEmpty(compiler);
    varDeltaSet           = VarSetOps::MakeEmpty(compiler);
    varStackGCPtrDeltaSet = VarSetOps::MakeEmpty(compiler);

#ifdef DEBUG
    scratchSet1 = VarSetOps::MakeEmpty(compiler);
    scratchSet2 = VarSetOps::MakeEmpty(compiler);
    epoch       = compiler->GetCurLVEpoch();
#endif

    // Also, initialize "HasStackGCPtrLiveness" for all tracked variables that do not fully
    // live in a register (i.e. they live on the stack for all or part of their lifetime).
    // Note that lvRegister indicates that a lclVar is in a register for its entire lifetime.

    for (unsigned lclNum = 0; lclNum < compiler->lvaCount; lclNum++)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

        if (lcl->HasLiveness() || lcl->lvIsRegCandidate())
        {
            if (!lcl->lvRegister && compiler->lvaIsGCTracked(lcl))
            {
                lcl->SetHasStackGCPtrLiveness();
            }
        }
    }
}

void CodeGenLivenessUpdater::ChangeLife(CodeGen* codeGen, VARSET_VALARG_TP newLife)
{
    DBEXEC(compiler->verbose, compiler->dmpVarSetDiff("Live vars at start of block: ", currentLife, newLife);)

    if (VarSetOps::Equal(compiler, currentLife, newLife))
    {
        return;
    }

    DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet1, codeGen->gcInfo.gcVarPtrSetCur));

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
            // TODO-Cleanup: Move the code from compUpdateLifeVar to genUpdateRegLife
            // that updates the gc sets
            regMaskTP regMask = lcl->lvRegMask();

            if (isGCRef)
            {
                codeGen->gcInfo.gcRegGCrefSetCur &= ~regMask;
            }
            else if (isByRef)
            {
                codeGen->gcInfo.gcRegByrefSetCur &= ~regMask;
            }

            codeGen->genUpdateRegLife(lcl, false /*isBorn*/, true /*isDying*/ DEBUGARG(nullptr));
        }

        if (isInMemory && (isGCRef || isByRef))
        {
            VarSetOps::RemoveElemD(compiler, codeGen->gcInfo.gcVarPtrSetCur, e.Current());
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
            if (!lcl->IsAlwaysAliveInMemory())
            {
                VarSetOps::RemoveElemD(compiler, codeGen->gcInfo.gcVarPtrSetCur, e.Current());
            }

            codeGen->genUpdateRegLife(lcl, true /*isBorn*/, false /*isDying*/ DEBUGARG(nullptr));

            regMaskTP regMask = lcl->lvRegMask();

            if (isGCRef)
            {
                codeGen->gcInfo.gcRegGCrefSetCur |= regMask;
            }
            else if (isByRef)
            {
                codeGen->gcInfo.gcRegByrefSetCur |= regMask;
            }
        }
        else if (compiler->lvaIsGCTracked(lcl))
        {
            VarSetOps::AddElemD(compiler, codeGen->gcInfo.gcVarPtrSetCur, e.Current());
        }

#ifdef USING_VARIABLE_LIVE_RANGE
        codeGen->getVariableLiveKeeper()->siStartVariableLiveRange(lcl, lclNum);
#endif
    }

    VarSetOps::Assign(compiler, currentLife, newLife);

#ifdef USING_SCOPE_INFO
    codeGen->siUpdate();
#endif

    DBEXEC(compiler->verbose, compiler->dmpVarSetDiff("GC stack vars: ", scratchSet1, codeGen->gcInfo.gcVarPtrSetCur);)
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
        DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet2, codeGen->gcInfo.gcVarPtrSetCur);)

        if (isBorn && lcl->IsRegCandidate() && (lclNode->GetRegNum() != REG_NA))
        {
            lcl->SetRegNum(lclNode->GetRegNum());
        }

        bool isInReg    = lcl->lvIsInReg() && (lclNode->GetRegNum() != REG_NA);
        bool isInMemory = !isInReg || lcl->IsAlwaysAliveInMemory();

        if (isInReg)
        {
            codeGen->genUpdateRegLife(lcl, isBorn, isDying DEBUGARG(lclNode));
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
            if (isInMemory && lcl->HasStackGCPtrLiveness())
            {
                if (isBorn)
                {
                    VarSetOps::AddElemD(compiler, codeGen->gcInfo.gcVarPtrSetCur, lcl->GetLivenessBitIndex());
                }
                else
                {
                    VarSetOps::RemoveElemD(compiler, codeGen->gcInfo.gcVarPtrSetCur, lcl->GetLivenessBitIndex());
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
        codeGen->SpillRegCandidateLclVar(lclNode->AsLclVar());

        if (lcl->HasStackGCPtrLiveness() &&
            VarSetOps::TryAddElemD(compiler, codeGen->gcInfo.gcVarPtrSetCur, lcl->lvVarIndex))
        {
            JITDUMP("GC pointer V%02u becoming live on stack\n", lclNode->GetLclNum());
        }
    }
}

void CodeGenLivenessUpdater::UpdateLifeMultiReg(CodeGen* codeGen, GenTreeLclVar* lclNode)
{
    assert(lclNode->OperIs(GT_STORE_LCL_VAR) && ((lclNode->gtFlags & GTF_VAR_USEASG) == 0));

    DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet1, currentLife);)
    DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet2, codeGen->gcInfo.gcVarPtrSetCur);)

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
            codeGen->genUpdateRegLife(fieldLcl, true, isFieldDying DEBUGARG(lclNode));
        }

        if (isFieldDying)
        {
            VarSetOps::RemoveElemD(compiler, currentLife, fieldLcl->GetLivenessBitIndex());
        }
        else
        {
            VarSetOps::AddElemD(compiler, currentLife, fieldLcl->GetLivenessBitIndex());
        }

        if (isInMemory && fieldLcl->HasStackGCPtrLiveness())
        {
            // TODO-MIKE-Review: Should we remove the local from the GC var set when the field is dying?
            // The "scalar" version of this code doesn't do it, it checks "isBorn" instead of "isDying".

            VarSetOps::AddElemD(compiler, codeGen->gcInfo.gcVarPtrSetCur, fieldLcl->GetLivenessBitIndex());
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
    DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet2, codeGen->gcInfo.gcVarPtrSetCur);)

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

        if (fieldLcl->HasStackGCPtrLiveness())
        {
            // TODO-MIKE-Review: Should we remove the local from the GC var set when the field is dying?
            // The "scalar" version of this code doesn't do it, it checks "isBorn" instead of "isDying".

            if (isBorn)
            {
                VarSetOps::AddElemD(compiler, codeGen->gcInfo.gcVarPtrSetCur, fieldLcl->GetLivenessBitIndex());
            }
            else
            {
                VarSetOps::RemoveElemD(compiler, codeGen->gcInfo.gcVarPtrSetCur, fieldLcl->GetLivenessBitIndex());
            }
        }
    }

    DBEXEC(compiler->verbose, DumpDiff(codeGen);)
}

#ifdef DEBUG
void CodeGenLivenessUpdater::DumpDiff(CodeGen* codeGen)
{
    if (!VarSetOps::Equal(compiler, scratchSet1, currentLife))
    {
        compiler->dmpVarSetDiff("Live vars: ", scratchSet1, currentLife);
    }

    if (!VarSetOps::Equal(compiler, scratchSet2, codeGen->gcInfo.gcVarPtrSetCur))
    {
        compiler->dmpVarSetDiff("GC stack vars: ", scratchSet2, codeGen->gcInfo.gcVarPtrSetCur);
    }
}
#endif
