#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#include "treelifeupdater.h"

CodeGenLivenessUpdater::CodeGenLivenessUpdater(Compiler* compiler)
    : compiler(compiler)
    , newLife(VarSetOps::MakeEmpty(compiler))
    , stackVarDeltaSet(VarSetOps::MakeEmpty(compiler))
    , varDeltaSet(VarSetOps::MakeEmpty(compiler))
    , gcTrkStkDeltaSet(VarSetOps::MakeEmpty(compiler))
#ifdef DEBUG
    , epoch(compiler->GetCurLVEpoch())
#endif // DEBUG
{
}

//------------------------------------------------------------------------
// UpdateLifeFieldVar: Update live sets for only the given field of a multi-reg LclVar node.
//
// Arguments:
//    lclNode - the GT_LCL_VAR node.
//    multiRegIndex - the index of the field being updated.
//
// Return Value:
//    Returns true iff the variable needs to be spilled.
//
// Notes:
//    This method need only be used when the fields are dying or going live at different times,
//    e.g. when I ready the 0th field/reg of one node and define the 0th field/reg of another
//    before reading the subsequent fields/regs.
//
bool CodeGenLivenessUpdater::UpdateLifeFieldVar(GenTreeLclVar* lclNode, unsigned multiRegIndex)
{
    assert(lclNode->OperIs(GT_LCL_VAR));

    LclVarDsc* parentVarDsc = compiler->lvaGetDesc(lclNode);
    assert(parentVarDsc->lvPromoted && (multiRegIndex < parentVarDsc->lvFieldCnt) && lclNode->IsMultiReg() &&
           compiler->lvaEnregMultiRegVars);
    unsigned   fieldVarNum = parentVarDsc->lvFieldLclStart + multiRegIndex;
    LclVarDsc* fldVarDsc   = compiler->lvaGetDesc(fieldVarNum);
    assert(fldVarDsc->HasLiveness());
    unsigned fldVarIndex = fldVarDsc->lvVarIndex;
    assert((lclNode->gtFlags & GTF_VAR_USEASG) == 0);

    bool isBorn  = ((lclNode->gtFlags & GTF_VAR_DEF) != 0);
    bool isDying = !isBorn && lclNode->IsLastUse(multiRegIndex);
    // GTF_SPILL will be set if any registers need to be spilled.
    GenTreeFlags spillFlags = (lclNode->gtFlags & lclNode->GetRegSpillFlagByIdx(multiRegIndex));
    bool         spill      = ((spillFlags & GTF_SPILL) != 0);

    if (isBorn || isDying)
    {
        regNumber reg        = lclNode->GetRegNumByIdx(multiRegIndex);
        bool      isInReg    = fldVarDsc->lvIsInReg() && reg != REG_NA;
        bool      isInMemory = !isInReg || fldVarDsc->lvLiveInOutOfHndlr;

        if (isInReg)
        {
            if (isBorn)
            {
                compiler->codeGen->genUpdateVarReg(fldVarDsc, lclNode, multiRegIndex);
            }
            compiler->codeGen->genUpdateRegLife(fldVarDsc, isBorn, isDying DEBUGARG(lclNode));
        }

        VarSetOps::Assign(compiler, newLife, compiler->compCurLife);

        if (isDying)
        {
            VarSetOps::RemoveElemD(compiler, newLife, fldVarIndex);
        }
        else
        {
            VarSetOps::AddElemD(compiler, newLife, fldVarIndex);
        }

        if (!VarSetOps::Equal(compiler, compiler->compCurLife, newLife))
        {
#ifdef DEBUG
            if (compiler->verbose)
            {
                printf("Live vars: ");
                dumpConvertedVarSet(compiler, compiler->compCurLife);
                printf(" => ");
                dumpConvertedVarSet(compiler, newLife);
                printf("\n");
            }
#endif // DEBUG

            VarSetOps::Assign(compiler, compiler->compCurLife, newLife);

            // Only add vars to the gcInfo.gcVarPtrSetCur if they are currently on stack, since the
            // gcInfo.gcTrkStkPtrLcls
            // includes all TRACKED vars that EVER live on the stack (i.e. are not always in a register).
            VarSetOps::Assign(compiler, gcTrkStkDeltaSet, compiler->codeGen->gcInfo.gcTrkStkPtrLcls);

            if (isInMemory && VarSetOps::IsMember(compiler, gcTrkStkDeltaSet, fldVarIndex))
            {
#ifdef DEBUG
                if (compiler->verbose)
                {
                    printf("GCvars: ");
                    dumpConvertedVarSet(compiler, compiler->codeGen->gcInfo.gcVarPtrSetCur);
                    printf(" => ");
                }
#endif // DEBUG

                if (isBorn)
                {
                    VarSetOps::AddElemD(compiler, compiler->codeGen->gcInfo.gcVarPtrSetCur, fldVarIndex);
                }
                else
                {
                    VarSetOps::RemoveElemD(compiler, compiler->codeGen->gcInfo.gcVarPtrSetCur, fldVarIndex);
                }

#ifdef DEBUG
                if (compiler->verbose)
                {
                    dumpConvertedVarSet(compiler, compiler->codeGen->gcInfo.gcVarPtrSetCur);
                    printf("\n");
                }
#endif // DEBUG

#ifdef USING_VARIABLE_LIVE_RANGE
                // For each of the LclVarDsc that are reporting change, variable or fields
                compiler->codeGen->getVariableLiveKeeper()->siStartOrCloseVariableLiveRange(fldVarDsc, fieldVarNum,
                                                                                            isBorn, isDying);
#endif // USING_VARIABLE_LIVE_RANGE

#ifdef USING_SCOPE_INFO
                compiler->codeGen->siUpdate();
#endif // USING_SCOPE_INFO
            }
        }
    }

    if (spill)
    {
        if (VarSetOps::IsMember(compiler, compiler->codeGen->gcInfo.gcTrkStkPtrLcls, fldVarIndex) &&
            VarSetOps::TryAddElemD(compiler, compiler->codeGen->gcInfo.gcVarPtrSetCur, fldVarIndex))
        {
            JITDUMP("Var V%02u becoming live\n", fieldVarNum);
        }
    }

    return spill;
}

GenTreeLclVar* CodeGenLivenessUpdater::IsLocalAddr(GenTree* addr)
{
    if (GenTreeAddrMode* addrMode = addr->IsAddrMode())
    {
        // We use this method in backward dataflow after liveness computation - fgInterBlockLocalVarLiveness().
        // Therefore it is critical that we don't miss 'uses' of any local.  It may seem this method overlooks
        // if the index part of the LEA has indir( someAddrOperator ( lclVar ) ) to search for a use but it's
        // covered by the fact we're traversing the expression in execution order and we also visit the index.

        // TODO-MIKE-Review: And if the index is visted what? Complete nonsense.

        GenTree* base = addrMode->GetBase();

        if (base != nullptr)
        {
            addr = base;
        }
    }

    // TODO-MIKE-Review: Why doesn't this check for LCL_FLD_ADDR?

    return addr->OperIs(GT_LCL_VAR_ADDR) ? addr->AsLclVar() : nullptr;
}

void CodeGenLivenessUpdater::UpdateLife(GenTree* tree)
{
    assert(!tree->OperIs(GT_PHI_ARG));
    assert(compiler->GetCurLVEpoch() == epoch);

    // TODO-Cleanup: We shouldn't really be calling this more than once
    if (tree == compiler->compCurLifeTree)
    {
        return;
    }

    compiler->compCurLifeTree = tree;

    GenTreeLclVarCommon* lclVarTree = nullptr;

    if (tree->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
    {
        lclVarTree = tree->AsLclVarCommon();
    }
    else if (GenTreeIndir* indir = tree->IsIndir())
    {
        lclVarTree = IsLocalAddr(indir->GetAddr());
    }

    if (lclVarTree == nullptr)
    {
        return;
    }

    LclVarDsc* varDsc = compiler->lvaGetDesc(lclVarTree);

    if (varDsc->IsAddressExposed() || (!varDsc->HasLiveness() && !varDsc->IsPromoted()))
    {
        return;
    }

    // if it's a partial definition then variable "x" must have had a previous, original, site to be born.
    bool isBorn;
    bool isDying;
    // GTF_SPILL will be set on a MultiRegLclVar if any registers need to be spilled.
    bool spill = ((lclVarTree->gtFlags & GTF_SPILL) != 0);

    if (lclVarTree->IsMultiRegLclVar())
    {
        // We should never have an 'IndirOfAddrOfLocal' for a multi-reg.
        assert(lclVarTree == tree);
        assert((lclVarTree->gtFlags & GTF_VAR_USEASG) == 0);
        isBorn = ((lclVarTree->gtFlags & GTF_VAR_DEF) != 0);
        // Note that for multireg locals we can have definitions for which some of those are last uses.
        // We don't want to add those to the varDeltaSet because otherwise they will be added as newly
        // live.
        isDying = !isBorn && lclVarTree->HasLastUse();
    }
    else
    {
        isBorn  = ((lclVarTree->gtFlags & GTF_VAR_DEF) != 0 && (lclVarTree->gtFlags & GTF_VAR_USEASG) == 0);
        isDying = ((lclVarTree->gtFlags & GTF_VAR_DEATH) != 0);
    }

    if (isBorn || isDying)
    {
        VarSetOps::Assign(compiler, newLife, compiler->compCurLife);
        // Since all tracked vars are register candidates, but not all are in registers at all times,
        // we maintain two separate sets of variables - the total set of variables that are either
        // born or dying here, and the subset of those that are on the stack
        VarSetOps::ClearD(compiler, stackVarDeltaSet);
        VarSetOps::ClearD(compiler, varDeltaSet);

        if (varDsc->HasLiveness())
        {
            VarSetOps::AddElemD(compiler, varDeltaSet, varDsc->lvVarIndex);

            if (isBorn && varDsc->lvIsRegCandidate() && tree->gtHasReg())
            {
                compiler->codeGen->genUpdateVarReg(varDsc, tree);
            }
            bool isInReg    = varDsc->lvIsInReg() && tree->GetRegNum() != REG_NA;
            bool isInMemory = !isInReg || varDsc->lvLiveInOutOfHndlr;
            if (isInReg)
            {
                compiler->codeGen->genUpdateRegLife(varDsc, isBorn, isDying DEBUGARG(tree));
            }
            if (isInMemory)
            {
                VarSetOps::AddElemD(compiler, stackVarDeltaSet, varDsc->lvVarIndex);
            }
        }
        else if (lclVarTree->IsMultiRegLclVar())
        {
            assert(varDsc->lvPromoted && compiler->lvaEnregMultiRegVars);
            unsigned firstFieldVarNum = varDsc->lvFieldLclStart;
            for (unsigned i = 0; i < varDsc->lvFieldCnt; ++i)
            {
                bool       fieldIsSpilled = spill && ((lclVarTree->GetRegSpillFlagByIdx(i) & GTF_SPILL) != 0);
                LclVarDsc* fldVarDsc      = &(compiler->lvaTable[firstFieldVarNum + i]);
                noway_assert(fldVarDsc->lvIsStructField);
                assert(fldVarDsc->HasLiveness());
                unsigned  fldVarIndex  = fldVarDsc->lvVarIndex;
                regNumber reg          = lclVarTree->AsLclVar()->GetRegNumByIdx(i);
                bool      isInReg      = fldVarDsc->lvIsInReg() && reg != REG_NA;
                bool      isInMemory   = !isInReg || fldVarDsc->lvLiveInOutOfHndlr;
                bool      isFieldDying = lclVarTree->AsLclVar()->IsLastUse(i);
                if ((isBorn && !isFieldDying) || (!isBorn && isFieldDying))
                {
                    VarSetOps::AddElemD(compiler, varDeltaSet, fldVarIndex);
                    if (isInMemory)
                    {
                        VarSetOps::AddElemD(compiler, stackVarDeltaSet, fldVarIndex);
                    }
                }
                if (isInReg)
                {
                    if (isBorn)
                    {
                        compiler->codeGen->genUpdateVarReg(fldVarDsc, tree, i);
                    }
                    compiler->codeGen->genUpdateRegLife(fldVarDsc, isBorn, isFieldDying DEBUGARG(tree));
                    // If this was marked for spill, genProduceReg should already have spilled it.
                    assert(!fieldIsSpilled);
                }
            }
            spill = false;
        }
        else if (varDsc->lvPromoted)
        {
            // If hasDeadTrackedFieldVars is true, then, for a LDOBJ(ADDR(<promoted struct local>)),
            // *deadTrackedFieldVars indicates which tracked field vars are dying.
            bool hasDeadTrackedFieldVars = false;

            if ((tree != lclVarTree) && isDying)
            {
                assert(!isBorn); // GTF_VAR_DEATH only set for LDOBJ last use.
                VARSET_TP* deadTrackedFieldVars = nullptr;
                hasDeadTrackedFieldVars = compiler->LookupPromotedStructDeathVars(lclVarTree, &deadTrackedFieldVars);
                if (hasDeadTrackedFieldVars)
                {
                    VarSetOps::Assign(compiler, varDeltaSet, *deadTrackedFieldVars);
                }
            }

            unsigned firstFieldVarNum = varDsc->lvFieldLclStart;
            for (unsigned i = 0; i < varDsc->lvFieldCnt; ++i)
            {
                LclVarDsc* fldVarDsc = compiler->lvaGetDesc(firstFieldVarNum + i);
                noway_assert(fldVarDsc->lvIsStructField);
                if (fldVarDsc->HasLiveness())
                {
                    unsigned fldVarIndex = fldVarDsc->lvVarIndex;
                    // We should never see enregistered fields in a struct local unless
                    // IsMultiRegLclVar() returns true, in which case we've handled this above.
                    assert(!fldVarDsc->lvIsInReg());
                    noway_assert(fldVarIndex < compiler->lvaTrackedCount);

                    if (!hasDeadTrackedFieldVars)
                    {
                        VarSetOps::AddElemD(compiler, varDeltaSet, fldVarIndex);
                    }

                    if (!hasDeadTrackedFieldVars || VarSetOps::IsMember(compiler, varDeltaSet, fldVarIndex))
                    {
                        VarSetOps::AddElemD(compiler, stackVarDeltaSet, fldVarIndex);
                    }
                }
            }
        }

        // First, update the live set
        if (isDying)
        {
            // We'd like to be able to assert the following, however if we are walking
            // through a qmark/colon tree, we may encounter multiple last-use nodes.
            // assert (VarSetOps::IsSubset(compiler, regVarDeltaSet, newLife));
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

        if (!VarSetOps::Equal(compiler, compiler->compCurLife, newLife))
        {
#ifdef DEBUG
            if (compiler->verbose)
            {
                printf("Live vars: ");
                dumpConvertedVarSet(compiler, compiler->compCurLife);
                printf(" => ");
                dumpConvertedVarSet(compiler, newLife);
                printf("\n");
            }
#endif // DEBUG

            VarSetOps::Assign(compiler, compiler->compCurLife, newLife);

            // Only add vars to the gcInfo.gcVarPtrSetCur if they are currently on stack, since the
            // gcInfo.gcTrkStkPtrLcls
            // includes all TRACKED vars that EVER live on the stack (i.e. are not always in a register).
            VarSetOps::Assign(compiler, gcTrkStkDeltaSet, compiler->codeGen->gcInfo.gcTrkStkPtrLcls);
            VarSetOps::IntersectionD(compiler, gcTrkStkDeltaSet, stackVarDeltaSet);

            if (!VarSetOps::IsEmpty(compiler, gcTrkStkDeltaSet))
            {
#ifdef DEBUG
                if (compiler->verbose)
                {
                    printf("GCvars: ");
                    dumpConvertedVarSet(compiler, compiler->codeGen->gcInfo.gcVarPtrSetCur);
                    printf(" => ");
                }
#endif // DEBUG

                if (isBorn)
                {
                    VarSetOps::UnionD(compiler, compiler->codeGen->gcInfo.gcVarPtrSetCur, gcTrkStkDeltaSet);
                }
                else
                {
                    VarSetOps::DiffD(compiler, compiler->codeGen->gcInfo.gcVarPtrSetCur, gcTrkStkDeltaSet);
                }

#ifdef DEBUG
                if (compiler->verbose)
                {
                    dumpConvertedVarSet(compiler, compiler->codeGen->gcInfo.gcVarPtrSetCur);
                    printf("\n");
                }
#endif // DEBUG
            }

#ifdef USING_VARIABLE_LIVE_RANGE
            // For each of the LclVarDsc that are reporting change, variable or fields
            compiler->codeGen->getVariableLiveKeeper()->siStartOrCloseVariableLiveRanges(varDeltaSet, isBorn, isDying);
#endif // USING_VARIABLE_LIVE_RANGE

#ifdef USING_SCOPE_INFO
            compiler->codeGen->siUpdate();
#endif // USING_SCOPE_INFO
        }
    }

    if (spill)
    {
        assert(!varDsc->lvPromoted);
        compiler->codeGen->genSpillVar(tree->AsLclVar());

        if (VarSetOps::IsMember(compiler, compiler->codeGen->gcInfo.gcTrkStkPtrLcls, varDsc->lvVarIndex) &&
            VarSetOps::TryAddElemD(compiler, compiler->codeGen->gcInfo.gcVarPtrSetCur, varDsc->lvVarIndex))
        {
            JITDUMP("Var V%02u becoming live\n", varDsc - compiler->lvaTable);
        }
    }
}
