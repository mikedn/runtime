#include "jitpch.h"
#include "treelifeupdater.h"
#include "codegen.h"

void CodeGenLivenessUpdater::Begin()
{
    currentLife           = VarSetOps::MakeEmpty(compiler);
    newLife               = VarSetOps::MakeEmpty(compiler);
    varDeltaSet           = VarSetOps::MakeEmpty(compiler);
    varStackGCPtrDeltaSet = VarSetOps::MakeEmpty(compiler);

    INDEBUG(scratchSet = VarSetOps::MakeEmpty(compiler);)
    INDEBUG(epoch = compiler->GetCurLVEpoch();)

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

    DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet, codeGen->gcInfo.gcVarPtrSetCur));

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

    DBEXEC(compiler->verbose, compiler->dmpVarSetDiff("GC stack vars: ", scratchSet, codeGen->gcInfo.gcVarPtrSetCur);)
}

void CodeGenLivenessUpdater::UpdateLife(CodeGen* codeGen, GenTreeLclVarCommon* lclNode)
{
    assert(lclNode->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));
    assert(compiler->GetCurLVEpoch() == epoch);

    // TODO-Cleanup: We shouldn't really be calling this more than once
    if (lclNode == currentNode)
    {
        return;
    }

    currentNode = lclNode;

    LclVarDsc* lcl = compiler->lvaGetDesc(lclNode);

    if (lcl->IsAddressExposed() || (!lcl->HasLiveness() && !lcl->IsPromoted()))
    {
        return;
    }

    bool isBorn;
    bool isDying;

    if (lclNode->IsMultiRegLclVar())
    {
        assert((lclNode->gtFlags & GTF_VAR_USEASG) == 0);

        isBorn = ((lclNode->gtFlags & GTF_VAR_DEF) != 0);
        // Note that for multireg locals we can have definitions for which some of those are last uses.
        // We don't want to add those to the varDeltaSet because otherwise they will be added as newly
        // live.
        isDying = !isBorn && lclNode->HasLastUse();
    }
    else
    {
        isBorn  = ((lclNode->gtFlags & GTF_VAR_DEF) != 0) && ((lclNode->gtFlags & GTF_VAR_USEASG) == 0);
        isDying = (lclNode->gtFlags & GTF_VAR_DEATH) != 0;
    }

    bool spill = lclNode->IsAnyRegSpill();

    if (isBorn || isDying)
    {
        VarSetOps::Assign(compiler, newLife, currentLife);
        // Since all tracked vars are register candidates, but not all are in registers at all times,
        // we maintain two separate sets of variables - the total set of variables that are either
        // born or dying here, and the subset of those that are on the stack
        VarSetOps::ClearD(compiler, varDeltaSet);
        VarSetOps::ClearD(compiler, varStackGCPtrDeltaSet);

        if (lcl->HasLiveness())
        {
            VarSetOps::AddElemD(compiler, varDeltaSet, lcl->GetLivenessBitIndex());

            if (isBorn && lcl->lvIsRegCandidate() && lclNode->gtHasReg())
            {
                codeGen->genUpdateVarReg(lcl, lclNode);
            }

            bool isInReg    = lcl->lvIsInReg() && (lclNode->GetRegNum() != REG_NA);
            bool isInMemory = !isInReg || lcl->IsAlwaysAliveInMemory();

            if (isInReg)
            {
                codeGen->genUpdateRegLife(lcl, isBorn, isDying DEBUGARG(lclNode));
            }

            if (isInMemory && lcl->HasStackGCPtrLiveness())
            {
                VarSetOps::AddElemD(compiler, varStackGCPtrDeltaSet, lcl->GetLivenessBitIndex());
            }
        }
        else if (lclNode->IsMultiRegLclVar())
        {
            assert(compiler->lvaEnregMultiRegVars);

            for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
            {
                LclVarDsc* fieldLcl = compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

                bool isInReg        = fieldLcl->lvIsInReg() && (lclNode->GetRegNum(i) != REG_NA);
                bool isInMemory     = !isInReg || fieldLcl->IsAlwaysAliveInMemory();
                bool isFieldDying   = lclNode->AsLclVar()->IsLastUse(i);
                bool isFieldSpilled = spill && lclNode->IsRegSpill(i);

                if ((isBorn && !isFieldDying) || (!isBorn && isFieldDying))
                {
                    VarSetOps::AddElemD(compiler, varDeltaSet, fieldLcl->GetLivenessBitIndex());

                    if (isInMemory && fieldLcl->HasStackGCPtrLiveness())
                    {
                        VarSetOps::AddElemD(compiler, varStackGCPtrDeltaSet, fieldLcl->GetLivenessBitIndex());
                    }
                }

                if (isInReg)
                {
                    if (isBorn)
                    {
                        codeGen->genUpdateVarReg(fieldLcl, lclNode, i);
                    }

                    codeGen->genUpdateRegLife(fieldLcl, isBorn, isFieldDying DEBUGARG(lclNode));

                    // If this was marked for spill, genProduceReg should already have spilled it.
                    assert(!isFieldSpilled);
                }
            }

            spill = false;
        }
        else if (lcl->IsPromoted())
        {
            bool hasDeadTrackedFields = false;

            if (isDying)
            {
                assert(!isBorn);

                VARSET_TP* deadTrackedFields = nullptr;
                hasDeadTrackedFields         = compiler->LookupPromotedStructDeathVars(lclNode, &deadTrackedFields);
                if (hasDeadTrackedFields)
                {
                    VarSetOps::Assign(compiler, varDeltaSet, *deadTrackedFields);
                }
            }

            for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
            {
                LclVarDsc* fieldLcl = compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

                if (fieldLcl->HasLiveness())
                {
                    unsigned index = fieldLcl->GetLivenessBitIndex();

                    // Since it's not multi-reg LCL_VAR it must be P-DEP and thus DNER.
                    assert(!fieldLcl->lvIsInReg());

                    if (!hasDeadTrackedFields)
                    {
                        VarSetOps::AddElemD(compiler, varDeltaSet, index);
                    }

                    if (!hasDeadTrackedFields || VarSetOps::IsMember(compiler, varDeltaSet, index))
                    {
                        if (fieldLcl->HasStackGCPtrLiveness())
                        {
                            VarSetOps::AddElemD(compiler, varStackGCPtrDeltaSet, index);
                        }
                    }
                }
            }
        }

        if (isDying)
        {
            // TODO-MIKE-Review: Why does the assert below fail? Old comment
            // mentions QMARKs but there's no such thing in LIR. CopyProp
            // liveness had a similar issue but the same fix isn't sufficient
            // here.
            //
            // assert(VarSetOps::IsSubset(compiler, regVarDeltaSet, newLife));

            VarSetOps::DiffD(compiler, newLife, varDeltaSet);
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
            VarSetOps::UnionD(compiler, newLife, varDeltaSet);
        }

        if (!VarSetOps::Equal(compiler, currentLife, newLife))
        {
            DBEXEC(compiler->verbose, compiler->dmpVarSetDiff("Live vars: ", currentLife, newLife);)

            VarSetOps::Assign(compiler, currentLife, newLife);

            if (!VarSetOps::IsEmpty(compiler, varStackGCPtrDeltaSet))
            {
                DBEXEC(compiler->verbose, VarSetOps::Assign(compiler, scratchSet, codeGen->gcInfo.gcVarPtrSetCur);)

                if (isBorn)
                {
                    VarSetOps::UnionD(compiler, codeGen->gcInfo.gcVarPtrSetCur, varStackGCPtrDeltaSet);
                }
                else
                {
                    VarSetOps::DiffD(compiler, codeGen->gcInfo.gcVarPtrSetCur, varStackGCPtrDeltaSet);
                }

                DBEXEC(compiler->verbose,
                       compiler->dmpVarSetDiff("GC stack vars: ", scratchSet, codeGen->gcInfo.gcVarPtrSetCur);)
            }

#ifdef USING_VARIABLE_LIVE_RANGE
            codeGen->getVariableLiveKeeper()->siStartOrCloseVariableLiveRanges(varDeltaSet, isBorn, isDying);
#endif

#ifdef USING_SCOPE_INFO
            codeGen->siUpdate();
#endif
        }
    }

    if (spill)
    {
        assert(!lcl->IsPromoted());

        codeGen->genSpillVar(lclNode->AsLclVar());

        if (lcl->HasStackGCPtrLiveness() &&
            VarSetOps::TryAddElemD(compiler, codeGen->gcInfo.gcVarPtrSetCur, lcl->lvVarIndex))
        {
            JITDUMP("GC pointer V%02u becoming live on stack\n", lclNode->GetLclNum());
        }
    }
}
