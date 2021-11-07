#include "jitpch.h"
#include "treelifeupdater.h"
#include "codegen.h"

void CodeGenLivenessUpdater::Begin()
{
    currentLife           = VarSetOps::MakeEmpty(compiler);
    newLife               = VarSetOps::MakeEmpty(compiler);
    varDeltaSet           = VarSetOps::MakeEmpty(compiler);
    varStackGCPtrDeltaSet = VarSetOps::MakeEmpty(compiler);

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

// Update live sets for only the given field of a multi-reg LclVar node.
//
// This method need only be used when the fields are dying or going live at different times,
// e.g. when I ready the 0th field/reg of one node and define the 0th field/reg of another
// before reading the subsequent fields/regs.
//
// Returns true iff the variable needs to be spilled.
//
bool CodeGenLivenessUpdater::UpdateLifeFieldVar(CodeGen* codeGen, GenTreeLclVar* lclNode, unsigned regIndex)
{
    assert(lclNode->OperIs(GT_LCL_VAR));
    assert(lclNode->IsMultiReg() && compiler->lvaEnregMultiRegVars);
    assert((lclNode->gtFlags & GTF_VAR_USEASG) == 0);

    unsigned   lclNum = compiler->lvaGetDesc(lclNode)->GetPromotedFieldLclNum(regIndex);
    LclVarDsc* lcl    = compiler->lvaGetDesc(lclNum);
    unsigned   index  = lcl->GetLivenessBitIndex();

    bool isBorn  = (lclNode->gtFlags & GTF_VAR_DEF) != 0;
    bool isDying = !isBorn && lclNode->IsLastUse(regIndex);
    bool spill   = ((lclNode->gtFlags & lclNode->GetRegSpillFlagByIdx(regIndex)) & GTF_SPILL) != 0;

    if (isBorn || isDying)
    {
        bool isInReg    = lcl->lvIsInReg() && (lclNode->GetRegNumByIdx(regIndex) != REG_NA);
        bool isInMemory = !isInReg || lcl->lvLiveInOutOfHndlr;

        if (isInReg)
        {
            if (isBorn)
            {
                codeGen->genUpdateVarReg(lcl, lclNode, regIndex);
            }

            codeGen->genUpdateRegLife(lcl, isBorn, isDying DEBUGARG(lclNode));
        }

        VarSetOps::Assign(compiler, newLife, currentLife);

        if (isDying)
        {
            VarSetOps::RemoveElemD(compiler, newLife, index);
        }
        else
        {
            VarSetOps::AddElemD(compiler, newLife, index);
        }

        if (!VarSetOps::Equal(compiler, currentLife, newLife))
        {
#ifdef DEBUG
            if (compiler->verbose)
            {
                compiler->dmpVarSet("Live vars: ", currentLife);
                compiler->dmpVarSet(" => ", newLife);
                printf("\n");
            }
#endif

            VarSetOps::Assign(compiler, currentLife, newLife);

            // Only add vars to the gcInfo.gcVarPtrSetCur if they are currently on stack,
            // since the gcInfo.gcTrkStkPtrLcls includes all TRACKED vars that EVER live
            // on the stack (i.e. are not always in a register).

            if (isInMemory && lcl->HasStackGCPtrLiveness())
            {
#ifdef DEBUG
                if (compiler->verbose)
                {
                    compiler->dmpVarSet("GCvars: ", codeGen->gcInfo.gcVarPtrSetCur);
                }
#endif

                if (isBorn)
                {
                    VarSetOps::AddElemD(compiler, codeGen->gcInfo.gcVarPtrSetCur, index);
                }
                else
                {
                    VarSetOps::RemoveElemD(compiler, codeGen->gcInfo.gcVarPtrSetCur, index);
                }

#ifdef DEBUG
                if (compiler->verbose)
                {
                    compiler->dmpVarSet(" => ", codeGen->gcInfo.gcVarPtrSetCur);
                    printf("\n");
                }
#endif

#ifdef USING_VARIABLE_LIVE_RANGE
                codeGen->getVariableLiveKeeper()->siStartOrCloseVariableLiveRange(lcl, lclNum, isBorn, isDying);
#endif

#ifdef USING_SCOPE_INFO
                codeGen->siUpdate();
#endif
            }
        }
    }

    if (spill)
    {
        if (lcl->HasStackGCPtrLiveness() && VarSetOps::TryAddElemD(compiler, codeGen->gcInfo.gcVarPtrSetCur, index))
        {
            JITDUMP("Var V%02u becoming live\n", lclNum);
        }
    }

    return spill;
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

    bool spill = (lclNode->gtFlags & GTF_SPILL) != 0;

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
            bool isInMemory = !isInReg || lcl->lvLiveInOutOfHndlr;

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

                bool isInReg        = fieldLcl->lvIsInReg() && (lclNode->AsLclVar()->GetRegNumByIdx(i) != REG_NA);
                bool isInMemory     = !isInReg || fieldLcl->lvLiveInOutOfHndlr;
                bool isFieldDying   = lclNode->AsLclVar()->IsLastUse(i);
                bool isFieldSpilled = spill && ((lclNode->GetRegSpillFlagByIdx(i) & GTF_SPILL) != 0);

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
#ifdef DEBUG
            if (compiler->verbose)
            {
                compiler->dmpVarSet("Live vars: ", currentLife);
                compiler->dmpVarSet(" => ", newLife);
                printf("\n");
            }
#endif

            VarSetOps::Assign(compiler, currentLife, newLife);

            if (!VarSetOps::IsEmpty(compiler, varStackGCPtrDeltaSet))
            {
#ifdef DEBUG
                if (compiler->verbose)
                {
                    compiler->dmpVarSet("GCvars: ", codeGen->gcInfo.gcVarPtrSetCur);
                }
#endif

                if (isBorn)
                {
                    VarSetOps::UnionD(compiler, codeGen->gcInfo.gcVarPtrSetCur, varStackGCPtrDeltaSet);
                }
                else
                {
                    VarSetOps::DiffD(compiler, codeGen->gcInfo.gcVarPtrSetCur, varStackGCPtrDeltaSet);
                }

#ifdef DEBUG
                if (compiler->verbose)
                {
                    compiler->dmpVarSet(" => ", codeGen->gcInfo.gcVarPtrSetCur);
                    printf("\n");
                }
#endif
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
