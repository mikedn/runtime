// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

//
//
//                                    CopyProp
//
// This stage performs value numbering based copy propagation. Since copy propagation
// is about data flow, we cannot find them in assertion prop phase. In assertion prop
// we can identify copies, like so: if (a == b) else, i.e., control flow assertions.
//
// To identify data flow copies, we'll follow a similar approach to SSA renaming.
// We would walk each path in the graph keeping track of every live definition. Thus
// when we see a variable that shares the VN with a live definition, we'd replace this
// variable with the variable in the live definition, if suitable.
//
///////////////////////////////////////////////////////////////////////////////////////

#include "jitpch.h"
#include "ssabuilder.h"

/**************************************************************************************
 *
 * Corresponding to the live definition pushes, pop the stack as we finish a sub-paths
 * of the graph originating from the block. Refer SSA renaming for any additional info.
 * "curSsaName" tracks the currently live definitions.
 */
void Compiler::optBlockCopyPropPopStacks(BasicBlock* block, LclNumToGenTreePtrStack* curSsaName)
{
    for (Statement* stmt : block->Statements())
    {
        for (GenTree* tree = stmt->GetTreeList(); tree != nullptr; tree = tree->gtNext)
        {
            if (!tree->IsLocal())
            {
                continue;
            }
            const unsigned lclNum = optIsSsaLocal(tree);
            if (lclNum == BAD_VAR_NUM)
            {
                continue;
            }
            if (tree->gtFlags & GTF_VAR_DEF)
            {
                GenTreePtrStack* stack = nullptr;
                curSsaName->Lookup(lclNum, &stack);
                stack->Pop();
                if (stack->Empty())
                {
                    curSsaName->Remove(lclNum);
                }
            }
        }
    }
}

#ifdef DEBUG
void Compiler::optDumpCopyPropStack(LclNumToGenTreePtrStack* curSsaName)
{
    JITDUMP("{ ");
    for (LclNumToGenTreePtrStack::KeyIterator iter = curSsaName->Begin(); !iter.Equal(curSsaName->End()); ++iter)
    {
        GenTreeLclVarCommon* lclVar    = iter.GetValue()->Top()->AsLclVarCommon();
        unsigned             ssaLclNum = optIsSsaLocal(lclVar);
        assert(ssaLclNum != BAD_VAR_NUM);

        if (ssaLclNum == lclVar->GetLclNum())
        {
            JITDUMP("%d-[%06d]:V%02u ", iter.Get(), dspTreeID(lclVar), ssaLclNum);
        }
        else
        {
            // A promoted field was asigned using the parent struct, print `ssa field lclNum(parent lclNum)`.
            JITDUMP("%d-[%06d]:V%02u(V%02u) ", iter.Get(), dspTreeID(lclVar), ssaLclNum, lclVar->GetLclNum());
        }
    }
    JITDUMP("}\n\n");
}
#endif
/*******************************************************************************************************
 *
 * Given the "lclVar" and "copyVar" compute if the copy prop will be beneficial.
 *
 */
int Compiler::optCopyProp_LclVarScore(LclVarDsc* lclVarDsc, LclVarDsc* copyVarDsc, bool preferOp2)
{
    int score = 0;

    if (lclVarDsc->lvVolatileHint)
    {
        score += 4;
    }

    if (copyVarDsc->lvVolatileHint)
    {
        score -= 4;
    }

    if (lclVarDsc->lvDoNotEnregister)
    {
        score += 4;
    }

    if (copyVarDsc->lvDoNotEnregister)
    {
        score -= 4;
    }

#ifdef TARGET_X86
    // For doubles we also prefer to change parameters into non-parameter local variables
    if (lclVarDsc->lvType == TYP_DOUBLE)
    {
        if (lclVarDsc->lvIsParam)
        {
            score += 2;
        }

        if (copyVarDsc->lvIsParam)
        {
            score -= 2;
        }
    }
#endif

    // Otherwise we prefer to use the op2LclNum
    return score + ((preferOp2) ? 1 : -1);
}

class CopyPropLivenessUpdater
{
    Compiler* compiler;
    VARSET_TP liveSet;
    VARSET_TP killSet;

public:
    CopyPropLivenessUpdater::CopyPropLivenessUpdater(Compiler* compiler)
        : compiler(compiler), liveSet(VarSetOps::MakeEmpty(compiler)), killSet(VarSetOps::MakeEmpty(compiler))
    {
    }

    void BeginBlock(BasicBlock* block)
    {
        VarSetOps::Assign(compiler, liveSet, block->bbLiveIn);
    }

    void BeginStatement()
    {
        VarSetOps::ClearD(compiler, killSet);
    }

    VARSET_VALARG_TP GetLiveSet() const
    {
        return liveSet;
    }

    VARSET_VALARG_TP GetKillSet() const
    {
        return killSet;
    }

    void Update(GenTree* node)
    {
        if (!node->OperIs(GT_LCL_VAR, GT_LCL_FLD))
        {
            return;
        }

        GenTreeLclVarCommon* lclNode = node->AsLclVarCommon();
        LclVarDsc*           lcl     = compiler->lvaGetDesc(lclNode);

        assert(!lcl->IsPromoted() || lcl->IsDependentPromoted() || lcl->lvIsMultiRegRet);

        if (!lcl->IsInSsa())
        {
            return;
        }

        bool isBorn  = ((lclNode->gtFlags & GTF_VAR_DEF) != 0) && ((lclNode->gtFlags & GTF_VAR_USEASG) == 0);
        bool isDying = ((lclNode->gtFlags & GTF_VAR_DEATH) != 0);

        if (!isBorn && !isDying)
        {
            return;
        }

#ifdef DEBUG
        if (compiler->verbose)
        {
            compiler->dmpVarSet("Live vars: ", liveSet);
        }
#endif

        if (isDying)
        {
            VarSetOps::RemoveElemD(compiler, liveSet, lcl->GetLivenessBitIndex());
        }
        else
        {
            VarSetOps::AddElemD(compiler, liveSet, lcl->GetLivenessBitIndex());
        }

#ifdef DEBUG
        if (compiler->verbose)
        {
            compiler->dmpVarSet(" => ", liveSet);
            printf("\n");
        }
#endif
    }

    void Kill(unsigned lclNum)
    {
        VarSetOps::AddElemD(compiler, killSet, compiler->lvaGetDesc(lclNum)->GetLivenessBitIndex());
    }
};

// Perform copy propagation on a given tree as we walk the graph and if it is a local
// variable, then look up all currently live definitions and check if any of those
// definitions share the same value number. If so, then we can make the replacement.
//
void Compiler::optCopyProp(BasicBlock*              block,
                           Statement*               stmt,
                           GenTreeLclVar*           tree,
                           unsigned                 lclNum,
                           LclNumToGenTreePtrStack* curSsaName,
                           CopyPropLivenessUpdater& liveness)
{
    assert(tree->OperIs(GT_LCL_VAR) && ((tree->gtFlags & GTF_VAR_DEF) == 0));

    // TODO-Review: EH successor/predecessor iteration seems broken.
    if (block->bbCatchTyp == BBCT_FINALLY || block->bbCatchTyp == BBCT_FAULT)
    {
        return;
    }

    ValueNum treeVN = tree->gtVNPair.GetConservative();

    if (treeVN == ValueNumStore::NoVN)
    {
        return;
    }

    LclVarDsc* lcl = lvaGetDesc(lclNum);

    for (LclNumToGenTreePtrStack::KeyIterator iter = curSsaName->Begin(); !iter.Equal(curSsaName->End()); ++iter)
    {
        unsigned newLclNum = iter.Get();

        // Nothing to do if same.
        if (lclNum == newLclNum)
        {
            continue;
        }

        GenTreeLclVarCommon* op = iter.GetValue()->Top()->AsLclVarCommon();

        if (op->GetType() != tree->GetType())
        {
            continue;
        }

        LclVarDsc* newLcl = lvaGetDesc(newLclNum);

        // TODO-MIKE-Review: This shouldn't be needed, it mostly exists to reject the case
        // of LONG locals accessed via INT LCL_VAR nodes. Thing is, if the two nodes have
        // the same type and the same VN then the local type should not matter. Removing
        // this produces no diffs so it can stay for now, at least because VN can't be
        // trusted when it comes to this kind of implicit casting.
        if (varActualType(op->GetType()) != varActualType(newLcl->GetType()))
        {
            continue;
        }

        // Skip variables with assignments embedded in the statement (i.e., with a comma). Because we
        // are not currently updating their SSA names as live in the copy-prop pass of the stmt.
        if (VarSetOps::IsMember(this, liveness.GetKillSet(), newLcl->GetLivenessBitIndex()))
        {
            continue;
        }

        // Do not copy propagate if the old and new lclVar have different 'doNotEnregister' settings.
        // This is primarily to avoid copy propagating to IND(ADDR(LCL_VAR)) where the replacement lclVar
        // is not marked 'lvDoNotEnregister'.
        // However, in addition, it may not be profitable to propagate a 'doNotEnregister' lclVar to an
        // existing use of an enregisterable lclVar.

        if (lcl->lvDoNotEnregister != newLcl->lvDoNotEnregister)
        {
            continue;
        }

        if ((gsShadowVarInfo != nullptr) && newLcl->IsParam() && (gsShadowVarInfo[newLclNum].shadowLclNum == lclNum))
        {
            continue;
        }

        unsigned newSsaNum = SsaConfig::RESERVED_SSA_NUM;
        ValueNum opVN;

        if ((op->gtFlags & GTF_VAR_DEF) != 0)
        {
            newSsaNum = GetSsaNumForLocalVarDef(op);

            // TODO-MIKE-CQ: The VN should always be obtained from the local SSA data, not from the node.
            // Def nodes don't need a VN since they don't produce a value and there actually are some
            // that don't have a VN (PHI defs apparently) and unnecessarily block copy propagation.
            if ((op->gtFlags & GTF_VAR_USEASG) != 0)
            {
                opVN = newLcl->GetPerSsaData(newSsaNum)->m_vnPair.GetConservative();
            }
            else
            {
                opVN = op->gtVNPair.GetConservative();
            }
        }
        else // parameters, this pointer etc.
        {
            newSsaNum = op->GetSsaNum();
            opVN      = op->gtVNPair.GetConservative();
        }

        if (newSsaNum == SsaConfig::RESERVED_SSA_NUM)
        {
            continue;
        }

        if (opVN != treeVN)
        {
            continue;
        }

        if (optCopyProp_LclVarScore(lcl, newLcl, true) <= 0)
        {
            continue;
        }

        // Check whether the newLclNum is live before being substituted. Otherwise, we could end
        // up in a situation where there must've been a phi node that got pruned because the variable
        // is not live anymore. For example,
        //  if
        //     x0 = 1
        //  else
        //     x1 = 2
        //  print(c) <-- x is not live here. Let's say 'c' shares the value number with "x0."
        //
        // If we simply substituted 'c' with "x0", we would be wrong. Ideally, there would be a phi
        // node x2 = phi(x0, x1) which can then be used to substitute 'c' with. But because of pruning
        // there would be no such phi node. To solve this we'll check if 'x' is live, before replacing
        // 'c' with 'x.'
        // Because of this dependence on live variable analysis, CopyProp phase is immediately
        // after Liveness, SSA and VN.
        if (!newLcl->lvIsThisPtr && !VarSetOps::IsMember(this, liveness.GetLiveSet(), newLcl->GetLivenessBitIndex()))
        {
            continue;
        }

        JITDUMP("[%06u] replacing V%02u:%u by V%02u:%u\n", tree->GetID(), tree->GetLclNum(), tree->GetSsaNum(),
                newLclNum, newSsaNum);

        tree->SetLclNum(newLclNum);
        tree->SetSsaNum(newSsaNum);

        break;
    }
}

unsigned Compiler::optIsSsaLocal(GenTree* tree)
{
    if (!tree->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        return BAD_VAR_NUM;
    }

    unsigned lclNum = tree->AsLclVarCommon()->GetLclNum();

    if (!lvaInSsa(lclNum))
    {
        return BAD_VAR_NUM;
    }

    return lclNum;
}

//------------------------------------------------------------------------------
// optBlockCopyProp : Perform copy propagation using currently live definitions on the current block's
//                    variables. Also as new definitions are encountered update the "curSsaName" which
//                    tracks the currently live definitions.
//
// Arguments:
//    block       -  Block the tree belongs to
//    curSsaName  -  The map from lclNum to its recently live definitions as a stack

void Compiler::optBlockCopyProp(BasicBlock*              block,
                                LclNumToGenTreePtrStack* curSsaName,
                                CopyPropLivenessUpdater& liveness)
{
#ifdef DEBUG
    JITDUMP("Copy Assertion for " FMT_BB "\n", block->bbNum);
    if (verbose)
    {
        printf("  curSsaName stack: ");
        optDumpCopyPropStack(curSsaName);
    }
#endif

    liveness.BeginBlock(block);

    for (Statement* stmt : block->Statements())
    {
        liveness.BeginStatement();

        // Walk the tree to find if any local variable can be replaced with current live definitions.
        for (GenTree* tree = stmt->GetTreeList(); tree != nullptr; tree = tree->gtNext)
        {
            liveness.Update(tree);

            const unsigned lclNum = optIsSsaLocal(tree);

            if (lclNum == BAD_VAR_NUM)
            {
                continue;
            }

            if ((tree->gtFlags & GTF_VAR_DEF) != 0)
            {
                // TODO-Review: Merge this loop with the following loop to correctly update the
                // live SSA num while also propagating copies.
                //
                // 1. This loop performs copy prop with currently live (on-top-of-stack) SSA num.
                // 2. The subsequent loop maintains a stack for each lclNum with
                //    currently active SSA numbers when definitions are encountered.
                //
                // If there is an embedded definition using a "comma" in a stmt, then the currently
                // live SSA number will get updated only in the next loop (2). However, this new
                // definition is now supposed to be live (on tos). If we did not update the stacks
                // using (2), copy prop (1) will use a SSA num defined outside the stmt ignoring the
                // embedded update. Killing the variable is a simplification to produce 0 ASM diffs
                // for an update release.
                liveness.Kill(lclNum);
            }
            else if (tree->OperIs(GT_LCL_VAR))
            {
                optCopyProp(block, stmt, tree->AsLclVar(), lclNum, curSsaName, liveness);
            }
        }

        // This logic must be in sync with SSA renaming process.
        for (GenTree* tree = stmt->GetTreeList(); tree != nullptr; tree = tree->gtNext)
        {
            const unsigned lclNum = optIsSsaLocal(tree);
            if (lclNum == BAD_VAR_NUM)
            {
                continue;
            }

            if ((tree->gtFlags & GTF_VAR_DEF) != 0)
            {
                GenTreePtrStack* stack;
                if (!curSsaName->Lookup(lclNum, &stack))
                {
                    stack = new (curSsaName->GetAllocator()) GenTreePtrStack(curSsaName->GetAllocator());
                }
                stack->Push(tree);
                curSsaName->Set(lclNum, stack, LclNumToGenTreePtrStack::Overwrite);
            }
            // If we encounter first use of a param or this pointer add it as a live definition.
            // Since they are always live, do it only once.
            else if (tree->OperIs(GT_LCL_VAR) && (lvaTable[lclNum].lvIsParam || lvaTable[lclNum].lvIsThisPtr))
            {
                GenTreePtrStack* stack;
                if (!curSsaName->Lookup(lclNum, &stack))
                {
                    stack = new (curSsaName->GetAllocator()) GenTreePtrStack(curSsaName->GetAllocator());
                    stack->Push(tree);
                    curSsaName->Set(lclNum, stack);
                }
            }
        }
    }
}

/**************************************************************************************
 *
 * This stage performs value numbering based copy propagation. Since copy propagation
 * is about data flow, we cannot find them in assertion prop phase. In assertion prop
 * we can identify copies that like so: if (a == b) else, i.e., control flow assertions.
 *
 * To identify data flow copies, we follow a similar approach to SSA renaming. We walk
 * each path in the graph keeping track of every live definition. Thus when we see a
 * variable that shares the VN with a live definition, we'd replace this variable with
 * the variable in the live definition.
 *
 * We do this to be in conventional SSA form. This can very well be changed later.
 *
 * For example, on some path in the graph:
 *    a0 = x0
 *    :            <- other blocks
 *    :
 *    a1 = y0
 *    :
 *    :            <- other blocks
 *    b0 = x0, we cannot substitute x0 with a0, because currently our backend doesn't
 * treat lclNum and ssaNum together as a variable, but just looks at lclNum. If we
 * substituted x0 with a0, then we'd be in general SSA form.
 *
 */
void Compiler::optVnCopyProp()
{
#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In optVnCopyProp()\n");
    }
#endif

    if (fgSsaPassesCompleted == 0)
    {
        return;
    }

    class CopyPropDomTreeVisitor : public DomTreeVisitor<CopyPropDomTreeVisitor>
    {
        // The map from lclNum to its recently live definitions as a stack.
        LclNumToGenTreePtrStack m_curSsaName;
        CopyPropLivenessUpdater m_liveness;

    public:
        CopyPropDomTreeVisitor(Compiler* compiler)
            : DomTreeVisitor(compiler, compiler->fgSsaDomTree)
            , m_curSsaName(compiler->getAllocator(CMK_CopyProp))
            , m_liveness(compiler)
        {
        }

        void PreOrderVisit(BasicBlock* block)
        {
            // TODO-Cleanup: Move this function from Compiler to this class.
            m_compiler->optBlockCopyProp(block, &m_curSsaName, m_liveness);
        }

        void PostOrderVisit(BasicBlock* block)
        {
            // TODO-Cleanup: Move this function from Compiler to this class.
            m_compiler->optBlockCopyPropPopStacks(block, &m_curSsaName);
        }
    };

    CopyPropDomTreeVisitor visitor(this);
    visitor.WalkTree();
}
