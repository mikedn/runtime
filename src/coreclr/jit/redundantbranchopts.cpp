// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "ssabuilder.h"

class RedundantBranchesDomTreeVisitor : public DomTreeVisitor<RedundantBranchesDomTreeVisitor>
{
public:
    bool madeChanges;

    RedundantBranchesDomTreeVisitor(SsaOptimizer& ssa)
        : DomTreeVisitor(ssa.GetCompiler(), ssa.GetDomTree()), madeChanges(false)
    {
    }

    void End()
    {
        for (BasicBlock* block : m_compiler->Blocks())
        {
            block->bbFlags &= ~BBF_VISITED;
        }

        DBEXEC(m_compiler->verbose && madeChanges, m_compiler->fgDispBasicBlocks(m_compiler->verboseTrees));
    }

    void PreOrderVisit(BasicBlock* block)
    {
    }

    void PostOrderVisit(BasicBlock* block)
    {
        if ((block->bbFlags & BBF_REMOVED) != 0)
        {
            return;
        }

        if (block->bbJumpKind == BBJ_COND)
        {
            madeChanges |= VisitBranch(block);
        }
    }

private:
    bool VisitBranch(BasicBlock* block);
    bool JumpThread(BasicBlock* block, BasicBlock* domBlock);
    bool IsReachable(BasicBlock* fromBlock, BasicBlock* toBlock, BasicBlock* excludedBlock);
};

bool RedundantBranchesDomTreeVisitor::VisitBranch(BasicBlock* const block)
{
    Statement* const stmt = block->GetLastStatement();

    if (stmt == nullptr)
    {
        return false;
    }

    GenTree* const jumpTree = stmt->GetRootNode();

    if (!jumpTree->OperIs(GT_JTRUE))
    {
        return false;
    }

    GenTree* const compare = jumpTree->AsUnOp()->GetOp(0);

    if (!compare->OperIsCompare())
    {
        return false;
    }

    // Walk up the dom tree and see if any dominating block has branched on
    // exactly this tree's VN...
    //
    BasicBlock* prevBlock  = block;
    BasicBlock* domBlock   = block->bbIDom;
    int         relopValue = -1;

    if (domBlock == nullptr)
    {
        return false;
    }

    while (domBlock != nullptr)
    {
        // Check the current dominator
        //
        if (domBlock->bbJumpKind == BBJ_COND)
        {
            Statement* const domJumpStmt = domBlock->lastStmt();
            GenTree* const   domJumpTree = domJumpStmt->GetRootNode();
            assert(domJumpTree->OperIs(GT_JTRUE));
            GenTree* const domCmpTree = domJumpTree->AsOp()->gtGetOp1();

            if (domCmpTree->OperIsCompare())
            {
                // We can use liberal VNs as bounds checks are not yet
                // manifest explicitly as relops.
                //
                ValueNum domCmpVN = domCmpTree->GetLiberalVN();

                // Note we could also infer the tree relop's value from similar relops higher in the dom tree.
                // For example, (x >= 0) dominating (x > 0), or (x < 0) dominating (x > 0).
                //
                // That is left as a future enhancement.
                //
                if (domCmpVN == compare->GetLiberalVN())
                {
                    // The compare in "tree" is redundant.
                    // Is there a unique path from the dominating compare?
                    //
                    JITDUMP("\nDominator " FMT_BB " of " FMT_BB " has relop with same liberal VN:\n", domBlock->bbNum,
                            block->bbNum);
                    DISPTREE(domCmpTree);
                    JITDUMP(" Redundant compare; current relop:\n");
                    DISPTREE(compare);

                    BasicBlock* const trueSuccessor  = domBlock->bbJumpDest;
                    BasicBlock* const falseSuccessor = domBlock->bbNext;
                    const bool        trueReaches    = IsReachable(trueSuccessor, block, domBlock);
                    const bool        falseReaches   = IsReachable(falseSuccessor, block, domBlock);

                    if (trueReaches && falseReaches)
                    {
                        // Both dominating compare outcomes reach the current block so we can't infer the
                        // value of the relop.
                        //
                        // However we may be able to update the flow from block's predecessors so they
                        // bypass block and instead transfer control to jump's successors (aka jump threading).
                        //
                        const bool wasThreaded = JumpThread(block, domBlock);

                        if (wasThreaded)
                        {
                            return true;
                        }
                    }
                    else if (trueReaches)
                    {
                        // Taken jump in dominator reaches, fall through doesn't; relop must be true.
                        //
                        JITDUMP("Jump successor " FMT_BB " of " FMT_BB " reaches, relop must be true\n",
                                domBlock->bbJumpDest->bbNum, domBlock->bbNum);
                        relopValue = 1;
                        break;
                    }
                    else if (falseReaches)
                    {
                        // Fall through from dominator reaches, taken jump doesn't; relop must be false.
                        //
                        JITDUMP("Fall through successor " FMT_BB " of " FMT_BB " reaches, relop must be false\n",
                                domBlock->bbNext->bbNum, domBlock->bbNum);
                        relopValue = 0;
                        break;
                    }
                    else
                    {
                        // No apparent path from the dominating BB.
                        //
                        // We should rarely see this given that IsReachable is returning
                        // up to date results, but as we optimize we create unreachable blocks,
                        // and that can lead to cases where we can't find paths. That means we may be
                        // optimizing code that is now unreachable, but attempts to fix or avoid
                        // doing that lead to more complications, and it isn't that common.
                        // So we just tolerate it.
                        //
                        // No point in looking further up the tree.
                        //
                        break;
                    }
                }
            }
        }

        // Keep looking higher up in the tree
        //
        prevBlock = domBlock;
        domBlock  = domBlock->bbIDom;
    }

    // Did we determine the relop value via dominance checks? If so, optimize.
    //
    if (relopValue == -1)
    {
        return false;
    }

    // Bail out if tree is has certain side effects
    //
    // Note we really shouldn't get here if the tree has non-exception effects,
    // as they should have impacted the value number.
    //
    if ((compare->gtFlags & GTF_SIDE_EFFECT) != 0)
    {
        // Bail if there is a non-exception effect.
        //
        if ((compare->gtFlags & GTF_SIDE_EFFECT) != GTF_EXCEPT)
        {
            JITDUMP("Current relop has non-exception side effects, so we won't optimize\n");
            return false;
        }

        // Be conservative if there is an exception effect and we're in an EH region
        // as we might not model the full extent of EH flow.
        //
        if (block->hasTryIndex())
        {
            JITDUMP("Current relop has exception side effect and is in a try, so we won't optimize\n");
            return false;
        }
    }

    JITDUMP("\nRedundant branch opt in " FMT_BB ":\n", block->bbNum);

    compare->ChangeToIntCon(relopValue);

    bool folded = m_compiler->fgFoldConditional(block);
    assert(folded);
    return true;
}

bool RedundantBranchesDomTreeVisitor::JumpThread(BasicBlock* const block, BasicBlock* const domBlock)
{
    assert(block->bbJumpKind == BBJ_COND);
    assert(domBlock->bbJumpKind == BBJ_COND);

    // If the dominating block is not the immediate dominator
    // we might need to duplicate a lot of code to thread
    // the jumps. See if that's the case.
    //
    const bool isIDom = domBlock == block->bbIDom;
    if (!isIDom)
    {
        // Walk up the dom tree until we hit dom block.
        //
        // If none of the doms in the stretch are BBJ_COND,
        // then we must have already optimized them, and
        // so should not have to duplicate code to thread.
        //
        BasicBlock* idomBlock = block->bbIDom;
        while ((idomBlock != nullptr) && (idomBlock != domBlock))
        {
            if (idomBlock->bbJumpKind == BBJ_COND)
            {
                JITDUMP(" -- " FMT_BB " not closest branching dom, so no threading\n", idomBlock->bbNum);
                return false;
            }
            JITDUMP(" -- bypassing %sdom " FMT_BB " as it was already optimized\n",
                    (idomBlock == block->bbIDom) ? "i" : "", idomBlock->bbNum);
            idomBlock = idomBlock->bbIDom;
        }

        // If we didn't bail out above, we should have reached domBlock.
        //
        assert(idomBlock == domBlock);
    }

    JITDUMP("Both successors of %sdom " FMT_BB " reach " FMT_BB " -- attempting jump threading\n", isIDom ? "i" : "",
            domBlock->bbNum, block->bbNum);

    // If the block is the first block of try-region, then skip jump threading
    if (m_compiler->bbIsTryBeg(block))
    {
        JITDUMP(FMT_BB " is first block of try-region; no threading\n", block->bbNum);
        return false;
    }

    // Since flow is going to bypass block, make sure there
    // is nothing in block that can cause a side effect.
    //
    // Note we neglect PHI assignments. This reflects a general lack of
    // SSA update abilities in the jit. We really should update any uses
    // of PHIs defined here with the corresponding PHI input operand.
    //
    // TODO: if block has side effects, for those predecessors that are
    // favorable (ones that don't reach block via a critical edge), consider
    // duplicating block's IR into the predecessor. This is the jump threading
    // analog of the optimization we encourage via fgOptimizeUncondBranchToSimpleCond.
    //
    Statement* const lastStmt = block->lastStmt();

    for (Statement* const stmt : block->NonPhiStatements())
    {
        GenTree* const tree = stmt->GetRootNode();

        // We can ignore exception side effects in the jump tree.
        //
        // They are covered by the exception effects in the dominating compare.
        // We know this because the VNs match and they encode exception states.
        //
        if ((tree->gtFlags & GTF_SIDE_EFFECT) != 0)
        {
            if (stmt == lastStmt)
            {
                assert(tree->OperIs(GT_JTRUE));
                if ((tree->gtFlags & GTF_SIDE_EFFECT) == GTF_EXCEPT)
                {
                    // However, be conservative if block is in a try as we might not
                    // have a full picture of EH flow.
                    //
                    if (!block->hasTryIndex())
                    {
                        // We will ignore the side effect on this tree.
                        //
                        continue;
                    }
                }
            }

            JITDUMP(FMT_BB " has side effects; no threading\n", block->bbNum);
            return false;
        }
    }

    // In order to optimize we have to be able to determine which predecessors
    // are correlated exclusively with a true value for block's relop, and which
    // are correlated exclusively with a false value (aka true preds and false preds).
    //
    // To do this we try and follow the flow from domBlock to block; any block pred
    // reachable from domBlock's true edge is a true pred, and vice versa.
    //
    // However, there are some exceptions:
    //
    // * It's possible for a pred to be reachable from both paths out of domBlock;
    // if so, we can't jump thread that pred.
    //
    // * It's also possible that a pred can't branch directly to a successor as
    // it might violate EH region constraints. Since this causes the same issues
    // as an ambiguous pred we'll just classify these as ambiguous too.
    //
    // * It's also possible to have preds with implied eh flow to the current
    // block, eg a catch return, and so we won't see either path reachable.
    // We'll handle those as ambiguous as well.
    //
    // * It's also possible that the pred is a switch; we will treat switch
    // preds as ambiguous as well.
    //
    // For true preds and false preds we can reroute flow. It may turn out that
    // one of the preds falls through to block. We would prefer not to introduce
    // a new block to allow changing that fall through to a jump, so if we have
    // both a pred that is not a true pred, and a fall through, we defer optimizing
    // the fall through pred as well.
    //
    // This creates an ordering issue, and to resolve it we have to walk the pred
    // list twice. Classification of preds should be cheap so we just rerun the
    // reachability checks twice as well.
    //
    int               numPreds          = 0;
    int               numAmbiguousPreds = 0;
    int               numTruePreds      = 0;
    int               numFalsePreds     = 0;
    BasicBlock*       uniqueTruePred    = nullptr;
    BasicBlock*       uniqueFalsePred   = nullptr;
    BasicBlock*       fallThroughPred   = nullptr;
    BasicBlock* const trueSuccessor     = domBlock->bbJumpDest;
    BasicBlock* const falseSuccessor    = domBlock->bbNext;
    BasicBlock* const trueTarget        = block->bbJumpDest;
    BasicBlock* const falseTarget       = block->bbNext;

    for (BasicBlock* const predBlock : block->PredBlocks())
    {
        numPreds++;

        // Treat switch preds as ambiguous for now.
        //
        if (predBlock->bbJumpKind == BBJ_SWITCH)
        {
            JITDUMP(FMT_BB " is a switch pred\n", predBlock->bbNum);
            numAmbiguousPreds++;
            continue;
        }

        const bool isTruePred =
            ((predBlock == domBlock) && (trueSuccessor == block)) || IsReachable(trueSuccessor, predBlock, domBlock);
        const bool isFalsePred =
            ((predBlock == domBlock) && (falseSuccessor == block)) || IsReachable(falseSuccessor, predBlock, domBlock);

        if (isTruePred == isFalsePred)
        {
            // Either both reach, or neither reaches.
            //
            // We should rarely see (false,false) given that IsReachable is returning
            // up to date results, but as we optimize we create unreachable blocks,
            // and that can lead to cases where we can't find paths. That means we may be
            // optimizing code that is now unreachable, but attempts to fix or avoid doing that
            // lead to more complications, and it isn't that common. So we tolerate it.
            //
            JITDUMP(FMT_BB " is an ambiguous pred\n", predBlock->bbNum);
            numAmbiguousPreds++;
            continue;
        }

        if (isTruePred)
        {
            if (!BasicBlock::sameEHRegion(predBlock, trueTarget))
            {
                JITDUMP(FMT_BB " is an eh constrained pred\n", predBlock->bbNum);
                numAmbiguousPreds++;
                continue;
            }

            if (numTruePreds == 0)
            {
                uniqueTruePred = predBlock;
            }
            else
            {
                uniqueTruePred = nullptr;
            }

            numTruePreds++;
            JITDUMP(FMT_BB " is a true pred\n", predBlock->bbNum);
        }
        else
        {
            assert(isFalsePred);

            if (!BasicBlock::sameEHRegion(predBlock, falseTarget))
            {
                JITDUMP(FMT_BB " is an eh constrained pred\n", predBlock->bbNum);
                numAmbiguousPreds++;
                continue;
            }

            if (numFalsePreds == 0)
            {
                uniqueFalsePred = predBlock;
            }
            else
            {
                uniqueFalsePred = nullptr;
            }

            numFalsePreds++;
            JITDUMP(FMT_BB " is a false pred\n", predBlock->bbNum);
        }

        // Note if the true or false pred is the fall through pred.
        //
        if (predBlock->bbNext == block)
        {
            JITDUMP(FMT_BB " is the fall-through pred\n", predBlock->bbNum);
            assert(fallThroughPred == nullptr);
            fallThroughPred = predBlock;
        }
    }

    // All preds should have been classified.
    //
    assert(numPreds == numTruePreds + numFalsePreds + numAmbiguousPreds);

    if ((numTruePreds == 0) && (numFalsePreds == 0))
    {
        // This is possible, but should be rare.
        //
        JITDUMP(FMT_BB " only has ambiguous preds, not optimizing\n", block->bbNum);
        return false;
    }

    if ((numAmbiguousPreds > 0) && (fallThroughPred != nullptr))
    {
        JITDUMP(FMT_BB " has both ambiguous preds and a fall through pred, not optimizing\n", block->bbNum);
        return false;
    }

    // We should be good to go
    //
    JITDUMP("Optimizing via jump threading\n");

    // Now reroute the flow from the predecessors.
    //
    // If there is a fall through pred, modify block by deleting the terminal
    // jump statement, and update it to jump or fall through to the appropriate successor.
    // Note this is just a refinement of pre-existing flow so no EH check is needed.
    //
    // All other predecessors must reach block via a jump. So we can update their
    // flow directly by changing their jump targets to the appropriate successor,
    // provided it's a permissable flow in our EH model.
    //
    for (BasicBlock* const predBlock : block->PredBlocks())
    {
        if (predBlock->bbJumpKind == BBJ_SWITCH)
        {
            // Skip over switch preds, they will continue to flow to block.
            //
            continue;
        }

        const bool isTruePred =
            ((predBlock == domBlock) && (trueSuccessor == block)) || IsReachable(trueSuccessor, predBlock, domBlock);
        const bool isFalsePred =
            ((predBlock == domBlock) && (falseSuccessor == block)) || IsReachable(falseSuccessor, predBlock, domBlock);

        if (isTruePred == isFalsePred)
        {
            // Skip over ambiguous preds, they will continue to flow to block.
            //
            continue;
        }

        if (!BasicBlock::sameEHRegion(predBlock, isTruePred ? trueTarget : falseTarget))
        {
            // Skip over eh constrained preds, they will continue to flow to block.
            continue;
        }

        // Is this the one and only unambiguous fall through pred?
        //
        if (predBlock->bbNext == block)
        {
            assert(predBlock == fallThroughPred);

            // No other pred can safely pass control through block.
            //
            assert(numAmbiguousPreds == 0);

            // Clean out the terminal branch statement; we are going to repurpose this block
            //
            Statement* lastStmt = block->lastStmt();
            m_compiler->fgRemoveStmt(block, lastStmt);

            if (isTruePred)
            {
                JITDUMP("Fall through flow from pred " FMT_BB " -> " FMT_BB " implies predicate true\n",
                        predBlock->bbNum, block->bbNum);
                JITDUMP("  repurposing " FMT_BB " to always jump to " FMT_BB "\n", block->bbNum, trueTarget->bbNum);
                m_compiler->fgRemoveRefPred(block->bbNext, block);
                block->bbJumpKind = BBJ_ALWAYS;
            }
            else
            {
                assert(isFalsePred);
                JITDUMP("Fall through flow from pred " FMT_BB " -> " FMT_BB " implies predicate false\n",
                        predBlock->bbNum, block->bbNum);
                JITDUMP("  repurposing " FMT_BB " to always fall through to " FMT_BB "\n", block->bbNum,
                        falseTarget->bbNum);
                m_compiler->fgRemoveRefPred(block->bbJumpDest, block);
                block->bbJumpKind = BBJ_NONE;
            }
        }
        else
        {
            assert(predBlock->bbNext != block);
            if (isTruePred)
            {
                assert(!IsReachable(falseSuccessor, predBlock, domBlock));
                JITDUMP("Jump flow from pred " FMT_BB " -> " FMT_BB
                        " implies predicate true; we can safely redirect flow to be " FMT_BB " -> " FMT_BB "\n",
                        predBlock->bbNum, block->bbNum, predBlock->bbNum, trueTarget->bbNum);

                m_compiler->fgRemoveRefPred(block, predBlock);
                m_compiler->fgReplaceJumpTarget(predBlock, trueTarget, block);
                m_compiler->fgAddRefPred(trueTarget, predBlock);
            }
            else
            {
                assert(isFalsePred);
                JITDUMP("Jump flow from pred " FMT_BB " -> " FMT_BB
                        " implies predicate false; we can safely redirect flow to be " FMT_BB " -> " FMT_BB "\n",
                        predBlock->bbNum, block->bbNum, predBlock->bbNum, falseTarget->bbNum);

                m_compiler->fgRemoveRefPred(block, predBlock);
                m_compiler->fgReplaceJumpTarget(predBlock, falseTarget, block);
                m_compiler->fgAddRefPred(falseTarget, predBlock);
            }
        }
    }

    m_compiler->fgModified = true;

    return true;
}

//------------------------------------------------------------------------
// IsReachable: see if there's a path from one block to another,
//   including paths involving EH flow.
//
// Arguments:
//    fromBlock - staring block
//    toBlock   - ending block
//    excludedBlock - ignore paths that flow through this block
//
// Returns:
//    true if there is a path, false if there is no path
//
// Notes:
//    Like fgReachable, but computed on demand (and so accurate given
//    the current flow graph), and also considers paths involving EH.
//
//    This may overstate "true" reachability in methods where there are
//    finallies with multiple continuations.
//
bool RedundantBranchesDomTreeVisitor::IsReachable(BasicBlock* const fromBlock,
                                                  BasicBlock* const toBlock,
                                                  BasicBlock* const excludedBlock)
{
    if (fromBlock == toBlock)
    {
        return true;
    }

    for (BasicBlock* const block : m_compiler->Blocks())
    {
        block->bbFlags &= ~BBF_VISITED;
    }

    ArrayStack<BasicBlock*> stack(m_compiler->getAllocator(CMK_Reachability));
    stack.Push(fromBlock);

    while (!stack.Empty())
    {
        BasicBlock* const nextBlock = stack.Pop();
        nextBlock->bbFlags |= BBF_VISITED;
        assert(nextBlock != toBlock);

        if (nextBlock == excludedBlock)
        {
            continue;
        }

        for (BasicBlock* succ : nextBlock->GetAllSuccs(m_compiler))
        {
            if (succ == toBlock)
            {
                return true;
            }

            if ((succ->bbFlags & BBF_VISITED) != 0)
            {
                continue;
            }

            stack.Push(succ);
        }
    }

    return false;
}

PhaseStatus SsaOptimizer::DoRedundantBranches()
{
    RedundantBranchesDomTreeVisitor visitor(*this);
    visitor.WalkTree();
    return visitor.madeChanges ? PhaseStatus::MODIFIED_EVERYTHING : PhaseStatus::MODIFIED_NOTHING;
}
