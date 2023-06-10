// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                              Optimizer                                    XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#pragma warning(disable : 4701)
#endif

//------------------------------------------------------------------------
// optSetBlockWeights: adjust block weights, as follows:
// 1. A block that is not reachable from the entry block is marked "run rarely".
// 2. If we're not using profile weights, then any block with a non-zero weight
//    that doesn't dominate all the return blocks has its weight dropped in half
//    (but only if the first block *does* dominate all the returns).
//
// Notes:
//    Depends on dominators, and fgReturnBlocks being set.
//
void Compiler::optSetBlockWeights()
{
    noway_assert(opts.OptimizationEnabled());
    assert(fgDomsComputed);

#ifdef DEBUG
    bool changed = false;
#endif

    bool       firstBBDominatesAllReturns = true;
    const bool usingProfileWeights        = fgIsUsingProfileWeights();

    for (BasicBlock* const block : Blocks())
    {
        /* Blocks that can't be reached via the first block are rarely executed */
        if (!fgReachable(fgFirstBB, block))
        {
            block->bbSetRunRarely();
        }

        if (!usingProfileWeights && firstBBDominatesAllReturns)
        {
            if (block->bbWeight != BB_ZERO_WEIGHT)
            {
                // Calculate our bbWeight:
                //
                //  o BB_UNITY_WEIGHT if we dominate all BBJ_RETURN blocks
                //  o otherwise BB_UNITY_WEIGHT / 2
                //
                bool blockDominatesAllReturns = true; // Assume that we will dominate

                for (BasicBlockList* retBlocks = fgReturnBlocks; retBlocks != nullptr; retBlocks = retBlocks->next)
                {
                    if (!fgDominate(block, retBlocks->block))
                    {
                        blockDominatesAllReturns = false;
                        break;
                    }
                }

                if (block == fgFirstBB)
                {
                    firstBBDominatesAllReturns = blockDominatesAllReturns;
                }
                else
                {
                    // If we are not using profile weight then we lower the weight
                    // of blocks that do not dominate a return block
                    //
                    if (!blockDominatesAllReturns)
                    {
                        INDEBUG(changed = true);
                        block->inheritWeightPercentage(block, 50);
                    }
                }
            }
        }
    }

#if DEBUG
    if (changed && verbose)
    {
        printf("\nAfter optSetBlockWeights:\n");
        fgDispBasicBlocks();
        printf("\n");
    }

    /* Check that the flowgraph data (bbNum, bbRefs, bbPreds) is up-to-date */
    fgDebugCheckBBlist();
#endif
}

/*****************************************************************************
 *
 *  Marks the blocks between 'begBlk' and 'endBlk' as part of a loop.
 */

void Compiler::optMarkLoopBlocks(BasicBlock* begBlk, BasicBlock* endBlk, bool excludeEndBlk)
{
    /* Calculate the 'loopWeight',
       this is the amount to increase each block in the loop
       Our heuristic is that loops are weighted eight times more
       than straight line code.
       Thus we increase each block by 7 times the weight of
       the loop header block,
       if the loops are all properly formed gives us:
       (assuming that BB_LOOP_WEIGHT_SCALE is 8)

          1 -- non loop basic block
          8 -- single loop nesting
         64 -- double loop nesting
        512 -- triple loop nesting

    */

    noway_assert(begBlk->bbNum <= endBlk->bbNum);
    noway_assert(begBlk->isLoopHead());
    noway_assert(fgReachable(begBlk, endBlk));
    noway_assert(!opts.MinOpts());

#ifdef DEBUG
    if (verbose)
    {
        printf("\nMarking a loop from " FMT_BB " to " FMT_BB, begBlk->bbNum,
               excludeEndBlk ? endBlk->bbPrev->bbNum : endBlk->bbNum);
    }
#endif

    /* Build list of backedges for block begBlk */
    flowList* backedgeList = nullptr;

    for (BasicBlock* const predBlock : begBlk->PredBlocks())
    {
        /* Is this a backedge? */
        if (predBlock->bbNum >= begBlk->bbNum)
        {
            backedgeList = new (this, CMK_FlowList) flowList(predBlock, backedgeList);

#if MEASURE_BLOCK_SIZE
            genFlowNodeCnt += 1;
            genFlowNodeSize += sizeof(flowList);
#endif // MEASURE_BLOCK_SIZE
        }
    }

    /* At least one backedge must have been found (the one from endBlk) */
    noway_assert(backedgeList);

    BasicBlock* curBlk = begBlk;

    while (true)
    {
        noway_assert(curBlk);

        // For curBlk to be part of a loop that starts at begBlk
        // curBlk must be reachable from begBlk and (since this is a loop)
        // likewise begBlk must be reachable from curBlk.
        //

        if (fgReachable(curBlk, begBlk) && fgReachable(begBlk, curBlk))
        {
            /* If this block reaches any of the backedge blocks we set reachable   */
            /* If this block dominates any of the backedge blocks we set dominates */
            bool reachable = false;
            bool dominates = false;

            for (flowList* tmp = backedgeList; tmp != nullptr; tmp = tmp->flNext)
            {
                BasicBlock* backedge = tmp->getBlock();

                if (!curBlk->isRunRarely())
                {
                    reachable |= fgReachable(curBlk, backedge);
                    dominates |= fgDominate(curBlk, backedge);

                    if (dominates && reachable)
                    {
                        break;
                    }
                }
            }

            if (reachable)
            {
                noway_assert(curBlk->bbWeight > BB_ZERO_WEIGHT);

                if (!curBlk->hasProfileWeight())
                {
                    BasicBlock::weight_t scale = BB_LOOP_WEIGHT_SCALE;

                    if (!dominates)
                    {
                        scale = scale / 2;
                    }

                    curBlk->scaleBBWeight(scale);
                }

                JITDUMP("\n    " FMT_BB "(wt=" FMT_WT ")", curBlk->bbNum, curBlk->getBBWeight(this));
            }
        }

        /* Stop if we've reached the last block in the loop */

        if (curBlk == endBlk)
        {
            break;
        }

        curBlk = curBlk->bbNext;

        /* If we are excluding the endBlk then stop if we've reached endBlk */

        if (excludeEndBlk && (curBlk == endBlk))
        {
            break;
        }
    }
}

/*****************************************************************************
 *
 *   Unmark the blocks between 'begBlk' and 'endBlk' as part of a loop.
 */

void Compiler::optUnmarkLoopBlocks(BasicBlock* begBlk, BasicBlock* endBlk)
{
    /* A set of blocks that were previously marked as a loop are now
       to be unmarked, since we have decided that for some reason this
       loop no longer exists.
       Basically we are just reseting the blocks bbWeight to their
       previous values.
    */

    noway_assert(begBlk->bbNum <= endBlk->bbNum);
    noway_assert(begBlk->isLoopHead());

    noway_assert(!opts.MinOpts());

    unsigned backEdgeCount = 0;

    for (BasicBlock* const predBlock : begBlk->PredBlocks())
    {
        /* is this a backward edge? (from predBlock to begBlk) */

        if (begBlk->bbNum > predBlock->bbNum)
        {
            continue;
        }

        /* We only consider back-edges that are BBJ_COND or BBJ_ALWAYS for loops */

        if ((predBlock->bbJumpKind != BBJ_COND) && (predBlock->bbJumpKind != BBJ_ALWAYS))
        {
            continue;
        }

        backEdgeCount++;
    }

    /* Only unmark the loop blocks if we have exactly one loop back edge */
    if (backEdgeCount != 1)
    {
#ifdef DEBUG
        if (verbose)
        {
            if (backEdgeCount > 0)
            {
                printf("\nNot removing loop at " FMT_BB ", due to an additional back edge", begBlk->bbNum);
            }
            else if (backEdgeCount == 0)
            {
                printf("\nNot removing loop at " FMT_BB ", due to no back edge", begBlk->bbNum);
            }
        }
#endif
        return;
    }
    noway_assert(backEdgeCount == 1);
    noway_assert(fgReachable(begBlk, endBlk));

#ifdef DEBUG
    if (verbose)
    {
        printf("\nUnmarking loop at " FMT_BB, begBlk->bbNum);
    }
#endif

    BasicBlock* curBlk = begBlk;
    while (true)
    {
        noway_assert(curBlk);

        // For curBlk to be part of a loop that starts at begBlk
        // curBlk must be reachable from begBlk and (since this is a loop)
        // likewise begBlk must be reachable from curBlk.
        //
        if (!curBlk->isRunRarely() && fgReachable(curBlk, begBlk) && fgReachable(begBlk, curBlk))
        {
            // Don't unmark blocks that are set to BB_MAX_WEIGHT
            // Don't unmark blocks when we are using profile weights
            //
            if (!curBlk->isMaxBBWeight() && !curBlk->hasProfileWeight())
            {
                BasicBlock::weight_t scale = 1.0f / BB_LOOP_WEIGHT_SCALE;

                if (!fgDominate(curBlk, endBlk))
                {
                    scale *= 2;
                }

                curBlk->scaleBBWeight(scale);
            }

            JITDUMP("\n    " FMT_BB "(wt=" FMT_WT ")", curBlk->bbNum, curBlk->getBBWeight(this));
        }

        /* Stop if we've reached the last block in the loop */

        if (curBlk == endBlk)
        {
            break;
        }

        curBlk = curBlk->bbNext;

        /* Stop if we go past the last block in the loop, as it may have been deleted */
        if (curBlk->bbNum > endBlk->bbNum)
        {
            break;
        }
    }

    JITDUMP("\n");

#if FEATURE_LOOP_ALIGN
    if (begBlk->isLoopAlign())
    {
        // Clear the loop alignment bit on the head of a loop, since it's no longer a loop.
        begBlk->bbFlags &= ~BBF_LOOP_ALIGN;
        JITDUMP("Removing LOOP_ALIGN flag from removed loop in " FMT_BB "\n", begBlk->bbNum);
    }
#endif
}

/*****************************************************************************************************
 *
 *  Function called to update the loop table and bbWeight before removing a block
 */

void Compiler::optUpdateLoopsBeforeRemoveBlock(BasicBlock* block, bool skipUnmarkLoop)
{
    if (!optLoopsMarked)
    {
        return;
    }

    noway_assert(!opts.MinOpts());

    bool removeLoop = false;

    /* If an unreachable block was part of a loop entry or bottom then the loop is unreachable */
    /* Special case: the block was the head of a loop - or pointing to a loop entry */

    for (unsigned loopNum = 0; loopNum < optLoopCount; loopNum++)
    {
        LoopDsc& loop = optLoopTable[loopNum];

        /* Some loops may have been already removed by
         * loop unrolling or conditional folding */

        if (loop.lpFlags & LPFLG_REMOVED)
        {
            continue;
        }

        if (block == loop.lpEntry || block == loop.lpBottom)
        {
            loop.lpFlags |= LPFLG_REMOVED;
            continue;
        }

#ifdef DEBUG
        if (verbose)
        {
            printf("\nUpdateLoopsBeforeRemoveBlock Before: ");
            optPrintLoopInfo(loopNum);
        }
#endif

        /* If the loop is still in the table
         * any block in the loop must be reachable !!! */

        noway_assert(loop.lpEntry != block);
        noway_assert(loop.lpBottom != block);

        if (loop.lpExit == block)
        {
            loop.lpExit = nullptr;
            loop.lpFlags &= ~LPFLG_ONE_EXIT;
        }

        /* If this points to the actual entry in the loop
         * then the whole loop may become unreachable */

        switch (block->bbJumpKind)
        {
            case BBJ_NONE:
            case BBJ_COND:
                if (block->bbNext == loop.lpEntry)
                {
                    removeLoop = true;
                    break;
                }
                if (block->bbJumpKind == BBJ_NONE)
                {
                    break;
                }

                FALLTHROUGH;

            case BBJ_ALWAYS:
                noway_assert(block->bbJumpDest);
                if (block->bbJumpDest == loop.lpEntry)
                {
                    removeLoop = true;
                }
                break;

            case BBJ_SWITCH:
                for (BasicBlock* const bTarget : block->SwitchTargets())
                {
                    if (bTarget == loop.lpEntry)
                    {
                        removeLoop = true;
                        break;
                    }
                }
                break;

            default:
                break;
        }

        if (removeLoop)
        {
            /* Check if the entry has other predecessors outside the loop
             * TODO: Replace this when predecessors are available */

            for (BasicBlock* const auxBlock : Blocks())
            {
                /* Ignore blocks in the loop */

                if (loop.lpContains(auxBlock))
                {
                    continue;
                }

                switch (auxBlock->bbJumpKind)
                {
                    case BBJ_NONE:
                    case BBJ_COND:
                        if (auxBlock->bbNext == loop.lpEntry)
                        {
                            removeLoop = false;
                            break;
                        }
                        if (auxBlock->bbJumpKind == BBJ_NONE)
                        {
                            break;
                        }

                        FALLTHROUGH;

                    case BBJ_ALWAYS:
                        noway_assert(auxBlock->bbJumpDest);
                        if (auxBlock->bbJumpDest == loop.lpEntry)
                        {
                            removeLoop = false;
                        }
                        break;

                    case BBJ_SWITCH:
                        for (BasicBlock* const bTarget : auxBlock->SwitchTargets())
                        {
                            if (bTarget == loop.lpEntry)
                            {
                                removeLoop = false;
                                break;
                            }
                        }
                        break;

                    default:
                        break;
                }
            }

            if (removeLoop)
            {
                loop.lpFlags |= LPFLG_REMOVED;
            }
        }
        else if (loop.lpHead == block)
        {
            /* The loop has a new head - Just update the loop table */
            loop.lpHead = block->bbPrev;
        }

#ifdef DEBUG
        if (verbose)
        {
            printf("\nUpdateLoopsBeforeRemoveBlock After: ");
            optPrintLoopInfo(loopNum);
        }
#endif
    }

    if ((skipUnmarkLoop == false) && ((block->bbJumpKind == BBJ_ALWAYS) || (block->bbJumpKind == BBJ_COND)) &&
        (block->bbJumpDest->isLoopHead()) && (block->bbJumpDest->bbNum <= block->bbNum) && fgDomsComputed &&
        (fgCurBBEpochSize == fgDomBBcount + 1) && fgReachable(block->bbJumpDest, block))
    {
        optUnmarkLoopBlocks(block->bbJumpDest, block);
    }
}

#ifdef DEBUG

/*****************************************************************************
 *
 *  Print loop info in an uniform way.
 */

void Compiler::optPrintLoopInfo(unsigned      loopInd,
                                BasicBlock*   lpHead,
                                BasicBlock*   lpFirst,
                                BasicBlock*   lpTop,
                                BasicBlock*   lpEntry,
                                BasicBlock*   lpBottom,
                                unsigned char lpExitCnt,
                                BasicBlock*   lpExit,
                                unsigned      parentLoop) const
{
    noway_assert(lpHead);

    printf(FMT_LP ", from " FMT_BB, loopInd, lpFirst->bbNum);
    if (lpTop != lpFirst)
    {
        printf(" (loop top is " FMT_BB ")", lpTop->bbNum);
    }

    printf(" to " FMT_BB " (Head=" FMT_BB ", Entry=" FMT_BB ", ExitCnt=%d", lpBottom->bbNum, lpHead->bbNum,
           lpEntry->bbNum, lpExitCnt);

    if (lpExitCnt == 1)
    {
        printf(" at " FMT_BB, lpExit->bbNum);
    }

    if (parentLoop != BasicBlock::NOT_IN_LOOP)
    {
        printf(", parent loop = " FMT_LP, parentLoop);
    }
    printf(")");
}

/*****************************************************************************
 *
 *  Print loop information given the index of the loop in the loop table.
 */

void Compiler::optPrintLoopInfo(unsigned lnum) const
{
    noway_assert(lnum < optLoopCount);

    const LoopDsc* ldsc = &optLoopTable[lnum];

    optPrintLoopInfo(lnum, ldsc->lpHead, ldsc->lpFirst, ldsc->lpTop, ldsc->lpEntry, ldsc->lpBottom, ldsc->lpExitCnt,
                     ldsc->lpExit, ldsc->lpParent);
}

#endif

//------------------------------------------------------------------------
// optPopulateInitInfo: Populate loop init info in the loop table.
//
// Arguments:
//     init     -  the tree that is supposed to initialize the loop iterator.
//     iterVar  -  loop iteration variable.
//
// Return Value:
//     "false" if the loop table could not be populated with the loop iterVar init info.
//
// Operation:
//     The 'init' tree is checked if its lhs is a local and rhs is either
//     a const or a local.
//
bool Compiler::optPopulateInitInfo(unsigned loopInd, GenTree* init, unsigned iterVar)
{
    // Operator should be =
    if (init->gtOper != GT_ASG)
    {
        return false;
    }

    GenTree* lhs = init->AsOp()->gtOp1;
    GenTree* rhs = init->AsOp()->gtOp2;
    // LHS has to be local and should equal iterVar.
    if (!lhs->OperIs(GT_LCL_VAR) || (lhs->AsLclVar()->GetLclNum() != iterVar))
    {
        return false;
    }

    // RHS can be constant or local var.
    // TODO-CQ: CLONE: Add arr length for descending loops.
    if (rhs->OperIs(GT_CNS_INT) && rhs->TypeIs(TYP_INT))
    {
        optLoopTable[loopInd].lpFlags |= LPFLG_CONST_INIT;
        optLoopTable[loopInd].lpConstInit = rhs->AsIntCon()->GetInt32Value();
    }
    else if (rhs->OperIs(GT_LCL_VAR))
    {
        optLoopTable[loopInd].lpFlags |= LPFLG_VAR_INIT;
        optLoopTable[loopInd].lpVarInit = rhs->AsLclVar()->GetLclNum();
    }
    else
    {
        return false;
    }
    return true;
}

//----------------------------------------------------------------------------------
// optCheckIterInLoopTest: Check if iter var is used in loop test.
//
// Arguments:
//      test          "jtrue" tree or an asg of the loop iter termination condition
//      from/to       blocks (beg, end) which are part of the loop.
//      iterVar       loop iteration variable.
//      loopInd       loop index.
//
//  Operation:
//      The test tree is parsed to check if "iterVar" matches the lhs of the condition
//      and the rhs limit is extracted from the "test" tree. The limit information is
//      added to the loop table.
//
//  Return Value:
//      The loop test tree
//
GenTreeOp* Compiler::optGetLoopTest(unsigned loopInd, GenTree* test, BasicBlock* from, BasicBlock* to, unsigned iterVar)
{
    GenTreeOp* relop;

    if (test->OperIs(GT_JTRUE))
    {
        relop = test->AsUnOp()->GetOp(0)->AsOp();
    }
    else
    {
        assert(test->OperIs(GT_ASG));
        relop = test->AsOp()->GetOp(1)->AsOp();
    }

    noway_assert(relop->OperIsCompare());

    GenTree* opr1 = relop->AsOp()->gtOp1;
    GenTree* opr2 = relop->AsOp()->gtOp2;

    GenTree* iterOp;
    GenTree* limitOp;

    // Make sure op1 or op2 is the iterVar.
    if (opr1->OperIs(GT_LCL_VAR) && (opr1->AsLclVar()->GetLclNum() == iterVar))
    {
        iterOp  = opr1;
        limitOp = opr2;
    }
    else if (opr2->OperIs(GT_LCL_VAR) && (opr2->AsLclVar()->GetLclNum() == iterVar))
    {
        iterOp  = opr2;
        limitOp = opr1;
    }
    else
    {
        return nullptr;
    }

    if (iterOp->gtType != TYP_INT)
    {
        return nullptr;
    }

    // Check what type of limit we have - constant, variable or arr-len.
    if (limitOp->OperIs(GT_CNS_INT))
    {
        optLoopTable[loopInd].lpFlags |= LPFLG_CONST_LIMIT;
        if ((limitOp->gtFlags & GTF_ICON_SIMD_COUNT) != 0)
        {
            optLoopTable[loopInd].lpFlags |= LPFLG_SIMD_LIMIT;
        }
    }
    else if (limitOp->OperIs(GT_LCL_VAR) && !optIsVarAssigned(from, to, nullptr, limitOp->AsLclVar()->GetLclNum()))
    {
        optLoopTable[loopInd].lpFlags |= LPFLG_VAR_LIMIT;
    }
    else if (limitOp->OperIs(GT_ARR_LENGTH))
    {
        optLoopTable[loopInd].lpFlags |= LPFLG_ARRLEN_LIMIT;
    }
    else
    {
        return nullptr;
    }

    return relop;
}

unsigned Compiler::optIsLoopIncrTree(GenTree* incr)
{
    if (!incr->OperIs(GT_ASG) || !incr->TypeIs(TYP_INT))
    {
        return BAD_VAR_NUM;
    }

    GenTree* dst = incr->AsOp()->GetOp(0);
    GenTree* src = incr->AsOp()->GetOp(1);

    if (!dst->OperIs(GT_LCL_VAR) || !src->OperIs(GT_ADD, GT_SUB) || !src->TypeIs(TYP_INT))
    {
        return BAD_VAR_NUM;
    }

    GenTree* step = src->AsOp()->GetOp(1);
    src           = src->AsOp()->GetOp(0);

    if (!src->OperIs(GT_LCL_VAR) || (src->AsLclVar()->GetLclNum() != dst->AsLclVar()->GetLclNum()))
    {
        return BAD_VAR_NUM;
    }

    if (!step->OperIs(GT_CNS_INT))
    {
        return BAD_VAR_NUM;
    }

    return dst->AsLclVar()->GetLclNum();
}

//----------------------------------------------------------------------------------
// optIsLoopTestEvalIntoTemp:
//      Pattern match if the test tree is computed into a tmp
//      and the "tmp" is used as jump condition for loop termination.
//
// Arguments:
//      testStmt    - is the JTRUE statement that is of the form: jmpTrue (Vtmp != 0)
//                    where Vtmp contains the actual loop test result.
//      newTestStmt - contains the statement that is the actual test stmt involving
//                    the loop iterator.
//
//  Return Value:
//      Returns true if a new test tree can be obtained.
//
//  Operation:
//      Scan if the current stmt is a jtrue with (Vtmp != 0) as condition
//      Then returns the rhs for def of Vtmp as the "test" node.
//
//  Note:
//      This method just retrieves what it thinks is the "test" node,
//      the callers are expected to verify that "iterVar" is used in the test.
//
bool Compiler::optIsLoopTestEvalIntoTemp(Statement* testStmt, Statement** newTestStmt)
{
    GenTree* test = testStmt->GetRootNode();

    if (test->gtOper != GT_JTRUE)
    {
        return false;
    }

    GenTree* relop = test->gtGetOp1();
    noway_assert(relop->OperIsCompare());

    GenTree* opr1 = relop->AsOp()->gtOp1;
    GenTree* opr2 = relop->AsOp()->gtOp2;

    // Make sure we have jtrue (vtmp != 0)
    if (relop->OperIs(GT_NE) && opr1->OperIs(GT_LCL_VAR) && opr2->OperIs(GT_CNS_INT) && opr2->IsIntegralConst(0))
    {
        // Get the previous statement to get the def (rhs) of Vtmp to see
        // if the "test" is evaluated into Vtmp.
        Statement* prevStmt = testStmt->GetPrevStmt();
        if (prevStmt == nullptr)
        {
            return false;
        }

        GenTree* tree = prevStmt->GetRootNode();
        if (tree->OperIs(GT_ASG))
        {
            GenTree* lhs = tree->AsOp()->gtOp1;
            GenTree* rhs = tree->AsOp()->gtOp2;

            // Return as the new test node.
            if (lhs->OperIs(GT_LCL_VAR) && (lhs->AsLclVar()->GetLclNum() == opr1->AsLclVar()->GetLclNum()))
            {
                if (rhs->OperIsCompare())
                {
                    *newTestStmt = prevStmt;
                    return true;
                }
            }
        }
    }
    return false;
}

//----------------------------------------------------------------------------------
// optExtractInitTestIncr:
//      Extract the "init", "test" and "incr" nodes of the loop.
//
// Arguments:
//      head    - Loop head block
//      bottom  - Loop bottom block
//      top     - Loop top block
//      ppInit  - The init stmt of the loop if found.
//      ppTest  - The test stmt of the loop if found.
//      ppIncr  - The incr stmt of the loop if found.
//
//  Return Value:
//      The results are put in "ppInit", "ppTest" and "ppIncr" if the method
//      returns true. Returns false if the information can't be extracted.
//
//  Operation:
//      Check if the "test" stmt is last stmt in the loop "bottom". If found good,
//      "test" stmt is found. Try to find the "incr" stmt. Check previous stmt of
//      "test" to get the "incr" stmt. If it is not found it could be a loop of the
//      below form.
//
//                     +-------<-----------------<-----------+
//                     |                                     |
//                     v                                     |
//      BBinit(head) -> BBcond(top) -> BBLoopBody(bottom) ---^
//
//      Check if the "incr" tree is present in the loop "top" node as the last stmt.
//      Also check if the "test" tree is assigned to a tmp node and the tmp is used
//      in the jtrue condition.
//
//  Note:
//      This method just retrieves what it thinks is the "test" node,
//      the callers are expected to verify that "iterVar" is used in the test.
//
GenTreeOp* Compiler::optExtractInitTestIncr(
    BasicBlock* head, BasicBlock* bottom, BasicBlock* top, GenTree** ppInit, GenTree** ppTest)
{
    assert(ppInit != nullptr);
    assert(ppTest != nullptr);

    // Check if last two statements in the loop body are the increment of the iterator
    // and the loop termination test.
    noway_assert(bottom->bbStmtList != nullptr);
    Statement* testStmt = bottom->lastStmt();
    noway_assert(testStmt != nullptr && testStmt->GetNextStmt() == nullptr);

    Statement* newTestStmt;
    if (optIsLoopTestEvalIntoTemp(testStmt, &newTestStmt))
    {
        testStmt = newTestStmt;
    }

    // Check if we have the incr stmt before the test stmt, if we don't,
    // check if incr is part of the loop "top".
    Statement* incrStmt = testStmt->GetPrevStmt();
    if (incrStmt == nullptr || optIsLoopIncrTree(incrStmt->GetRootNode()) == BAD_VAR_NUM)
    {
        if (top == nullptr || top->bbStmtList == nullptr || top->bbStmtList->GetPrevStmt() == nullptr)
        {
            return nullptr;
        }

        // If the prev stmt to loop test is not incr, then check if we have loop test evaluated into a tmp.
        incrStmt = top->lastStmt();
        if (optIsLoopIncrTree(incrStmt->GetRootNode()) == BAD_VAR_NUM)
        {
            return nullptr;
        }
    }

    assert(testStmt != incrStmt);

    // Find the last statement in the loop pre-header which we expect to be the initialization of
    // the loop iterator.
    Statement* phdrStmt = head->firstStmt();
    if (phdrStmt == nullptr)
    {
        return nullptr;
    }

    Statement* initStmt = phdrStmt->GetPrevStmt();
    noway_assert(initStmt != nullptr && (initStmt->GetNextStmt() == nullptr));

    // If it is a duplicated loop condition, skip it.
    if (initStmt->IsCompilerAdded())
    {
        bool doGetPrev = true;
#ifdef DEBUG
        if (opts.optRepeat)
        {
            // Previous optimization passes may have inserted compiler-generated
            // statements other than duplicated loop conditions.
            doGetPrev = (initStmt->GetPrevStmt() != nullptr);
        }
        else
        {
            // Must be a duplicated loop condition.
            noway_assert(initStmt->GetRootNode()->gtOper == GT_JTRUE);
        }
#endif // DEBUG
        if (doGetPrev)
        {
            initStmt = initStmt->GetPrevStmt();
        }
        noway_assert(initStmt != nullptr);
    }

    *ppInit = initStmt->GetRootNode();
    *ppTest = testStmt->GetRootNode();
    return incrStmt->GetRootNode()->AsOp();
}

/*****************************************************************************
 *
 *  Record the loop in the loop table.  Return true if successful, false if
 *  out of entries in loop table.
 */

bool Compiler::optRecordLoop(BasicBlock* head,
                             BasicBlock* first,
                             BasicBlock* top,
                             BasicBlock* entry,
                             BasicBlock* bottom,
                             BasicBlock* exit,
                             unsigned    exitCnt)
{
    // Record this loop in the table, if there's room.

    assert(optLoopCount <= BasicBlock::MAX_LOOP_NUM);
    if (optLoopCount == BasicBlock::MAX_LOOP_NUM)
    {
#if COUNT_LOOPS
        loopOverflowThisMethod = true;
#endif
        return false;
    }

    // Assumed preconditions on the loop we're adding.
    assert(first->bbNum <= top->bbNum);
    assert(top->bbNum <= entry->bbNum);
    assert(entry->bbNum <= bottom->bbNum);
    assert(head->bbNum < top->bbNum || head->bbNum > bottom->bbNum);

    unsigned loopInd = optLoopCount;

    if (optLoopTable == nullptr)
    {
        assert(loopInd == 0);
        optLoopTable = getAllocator(CMK_LoopOpt).allocate<LoopDsc>(BasicBlock::MAX_LOOP_NUM);
    }
    else
    {
        // If the new loop contains any existing ones, add it in the right place.
        for (unsigned prevPlus1 = optLoopCount; prevPlus1 > 0; prevPlus1--)
        {
            unsigned prev = prevPlus1 - 1;
            if (optLoopTable[prev].lpContainedBy(first, bottom))
            {
                loopInd = prev;
            }
        }
        // Move up any loops if necessary.
        for (unsigned j = optLoopCount; j > loopInd; j--)
        {
            optLoopTable[j] = optLoopTable[j - 1];
        }
    }

#ifdef DEBUG
    for (unsigned i = loopInd + 1; i < optLoopCount; i++)
    {
        // The loop is well-formed.
        assert(optLoopTable[i].lpWellFormed());
        // Check for disjoint.
        if (optLoopTable[i].lpDisjoint(first, bottom))
        {
            continue;
        }
        // Otherwise, assert complete containment (of optLoopTable[i] in new loop).
        assert(optLoopTable[i].lpContainedBy(first, bottom));
    }
#endif // DEBUG

    optLoopTable[loopInd].lpHead     = head;
    optLoopTable[loopInd].lpFirst    = first;
    optLoopTable[loopInd].lpTop      = top;
    optLoopTable[loopInd].lpBottom   = bottom;
    optLoopTable[loopInd].lpEntry    = entry;
    optLoopTable[loopInd].lpExit     = exit;
    optLoopTable[loopInd].lpExitCnt  = static_cast<uint8_t>(exitCnt);
    optLoopTable[loopInd].lpParent   = BasicBlock::NOT_IN_LOOP;
    optLoopTable[loopInd].lpChild    = BasicBlock::NOT_IN_LOOP;
    optLoopTable[loopInd].lpSibling  = BasicBlock::NOT_IN_LOOP;
    optLoopTable[loopInd].lpFlags    = LPFLG_EMPTY;
    optLoopTable[loopInd].lpIterTree = nullptr;
    optLoopTable[loopInd].lpTestTree = nullptr;

    // We haven't yet recorded any side effects.

    // If DO-WHILE loop mark it as such.
    if (head->bbNext == entry)
    {
        optLoopTable[loopInd].lpFlags |= LPFLG_DO_WHILE;
    }

    // If single exit loop mark it as such.
    if (exitCnt == 1)
    {
        noway_assert(exit);
        optLoopTable[loopInd].lpFlags |= LPFLG_ONE_EXIT;
    }

    //
    // Try to find loops that have an iterator (i.e. for-like loops) "for (init; test; incr){ ... }"
    // We have the following restrictions:
    //     1. The loop condition must be a simple one i.e. only one JTRUE node
    //     2. There must be a loop iterator (a local var) that is
    //        incremented (decremented) with a constant value
    //     3. The iterator is incremented exactly once
    //     4. The loop condition must use the iterator.
    //
    if (bottom->bbJumpKind == BBJ_COND)
    {
        GenTree*   init;
        GenTree*   test;
        GenTreeOp* incr = optExtractInitTestIncr(head, bottom, top, &init, &test);

        if (incr == nullptr)
        {
            goto DONE_LOOP;
        }

        unsigned iterVar = incr->AsOp()->GetOp(0)->AsLclVar()->GetLclNum();
        assert(optIsLoopIncrTree(incr) == iterVar);
        assert(incr->GetOp(1)->OperIs(GT_ADD, GT_SUB));

        if (optIsVarAssigned(head->bbNext, bottom, incr, iterVar))
        {
            JITDUMP("iterVar is assigned in loop\n");
            goto DONE_LOOP;
        }

        // Make sure the "iterVar" initialization is never skipped,
        // i.e. every pred of ENTRY other than HEAD is in the loop.
        for (BasicBlock* const predBlock : entry->PredBlocks())
        {
            if ((predBlock != head) && !optLoopTable[loopInd].lpContains(predBlock))
            {
                goto DONE_LOOP;
            }
        }

        if (!optPopulateInitInfo(loopInd, init, iterVar))
        {
            goto DONE_LOOP;
        }

        if (GenTreeOp* testTree = optGetLoopTest(loopInd, test, head->bbNext, bottom, iterVar))
        {
            optLoopTable[loopInd].lpIterTree = incr;
            optLoopTable[loopInd].lpTestTree = testTree;

#if COUNT_LOOPS
            iterLoopCount++;
            simpleTestLoopCount++;

            if ((optLoopTable[loopInd].lpFlags & (LPFLG_CONST_INIT | LPFLG_CONST_LIMIT)) ==
                (LPFLG_CONST_INIT | LPFLG_CONST_LIMIT))
            {
                constIterLoopCount++;
            }
#endif

#ifdef DEBUG
            if (verbose && 0)
            {
                printf("\nConstant loop initializer:\n");
                gtDispTree(init);

                printf("\nConstant loop body:\n");

                BasicBlock* block = head;
                do
                {
                    block = block->bbNext;
                    for (Statement* const stmt : block->Statements())
                    {
                        if (stmt->GetRootNode() == incr)
                        {
                            break;
                        }
                        printf("\n");
                        gtDispTree(stmt->GetRootNode());
                    }
                } while (block != bottom);
            }
#endif // DEBUG
        }
    }

DONE_LOOP:
    DBEXEC(verbose, optPrintLoopRecording(loopInd));
    optLoopCount++;
    return true;
}

#ifdef DEBUG
//------------------------------------------------------------------------
// optPrintLoopRecording: Print a recording of the loop.
//
// Arguments:
//      loopInd     - loop index.
//
void Compiler::optPrintLoopRecording(unsigned loopInd) const
{
    const LoopDsc& loop = optLoopTable[loopInd];

    printf("Recorded loop %s", (loopInd != optLoopCount ? "(extended) " : ""));
    optPrintLoopInfo(optLoopCount, // Not necessarily the loop index, but the number of loops that have been added.
                     loop.lpHead, loop.lpFirst, loop.lpTop, loop.lpEntry, loop.lpBottom, loop.lpExitCnt, loop.lpExit);

    // If an iterator loop print the iterator and the initialization.
    if (loop.lpIterTree != nullptr)
    {
        printf(" [over V%02u", loop.lpIterVar());
        printf(" (");
        printf(GenTree::OpName(loop.lpIterOper()));
        printf(" ");
        printf("%d )", loop.lpIterConst());

        if (loop.lpFlags & LPFLG_CONST_INIT)
        {
            printf(" from %d", loop.lpConstInit);
        }
        if (loop.lpFlags & LPFLG_VAR_INIT)
        {
            printf(" from V%02u", loop.lpVarInit);
        }

        // If a simple test condition print operator and the limits */
        printf(GenTree::OpName(loop.lpTestOper()));

        if (loop.lpFlags & LPFLG_CONST_LIMIT)
        {
            printf("%d ", loop.lpConstLimit());
        }

        if (loop.lpFlags & LPFLG_VAR_LIMIT)
        {
            printf("V%02u ", loop.lpVarLimit());
        }

        printf("]");
    }

    printf("\n");
}

void Compiler::optCheckPreds()
{
    for (BasicBlock* const block : Blocks())
    {
        for (BasicBlock* const predBlock : block->PredBlocks())
        {
            // make sure this pred is part of the BB list
            BasicBlock* bb;
            for (bb = fgFirstBB; bb; bb = bb->bbNext)
            {
                if (bb == predBlock)
                {
                    break;
                }
            }
            noway_assert(bb);
            switch (bb->bbJumpKind)
            {
                case BBJ_COND:
                    if (bb->bbJumpDest == block)
                    {
                        break;
                    }
                    FALLTHROUGH;
                case BBJ_NONE:
                    noway_assert(bb->bbNext == block);
                    break;
                case BBJ_EHFILTERRET:
                case BBJ_ALWAYS:
                case BBJ_EHCATCHRET:
                    noway_assert(bb->bbJumpDest == block);
                    break;
                default:
                    break;
            }
        }
    }
}

#endif // DEBUG

namespace
{
//------------------------------------------------------------------------
// LoopSearch: Class that handles scanning a range of blocks to detect a loop,
//             moving blocks to make the loop body contiguous, and recording
//             the loop.
//
// We will use the following terminology:
//   HEAD    - the basic block that flows into the loop ENTRY block (Currently MUST be lexically before entry).
//             Not part of the looping of the loop.
//   FIRST   - the lexically first basic block (in bbNext order) within this loop.
//   TOP     - the target of the backward edge from BOTTOM. In most cases FIRST and TOP are the same.
//   BOTTOM  - the lexically last block in the loop (i.e. the block from which we jump to the top)
//   EXIT    - the predecessor of loop's unique exit edge, if it has a unique exit edge; else nullptr
//   ENTRY   - the entry in the loop (not necessarly the TOP), but there must be only one entry
//
//   We (currently) require the body of a loop to be a contiguous (in bbNext order) sequence of basic blocks.
//   When the loop is identified, blocks will be moved out to make it a compact contiguous region if possible,
//   and in cases where compaction is not possible, we'll subsequently treat all blocks in the lexical range
//   between TOP and BOTTOM as part of the loop even if they aren't part of the SCC.
//   Regarding nesting:  Since a given block can only have one back-edge (we only detect loops with back-edges
//   from BBJ_COND or BBJ_ALWAYS blocks), no two loops will share the same BOTTOM.  Two loops may share the
//   same FIRST/TOP/ENTRY as reported by LoopSearch, and optCanonicalizeLoopNest will subsequently re-write
//   the CFG so that no two loops share the same FIRST/TOP/ENTRY anymore.
//
//        |
//        v
//      head
//        |
//        |  top/first <--+
//        |       |       |
//        |      ...      |
//        |       |       |
//        |       v       |
//        +---> entry     |
//                |       |
//               ...      |
//                |       |
//                v       |
//         +-- exit/tail  |
//         |      |       |
//         |     ...      |
//         |      |       |
//         |      v       |
//         |    bottom ---+
//         |
//         +------+
//                |
//                v
//
class LoopSearch
{

    // Keeping track of which blocks are in the loop requires two block sets since we may add blocks
    // as we go but the BlockSet type's max ID doesn't increase to accommodate them.  Define a helper
    // struct to make the ensuing code more readable.
    struct LoopBlockSet
    {
    private:
        // Keep track of blocks with bbNum <= oldBlockMaxNum in a regular BlockSet, since
        // it can hold all of them.
        BlockSet oldBlocksInLoop; // Blocks with bbNum <= oldBlockMaxNum

        // Keep track of blocks with bbNum > oldBlockMaxNum in a separate BlockSet, but
        // indexing them by (blockNum - oldBlockMaxNum); since we won't generate more than
        // one new block per old block, this must be sufficient to track any new blocks.
        BlockSet newBlocksInLoop; // Blocks with bbNum > oldBlockMaxNum

        Compiler*    comp;
        unsigned int oldBlockMaxNum;

    public:
        LoopBlockSet(Compiler* comp)
            : oldBlocksInLoop(BlockSetOps::UninitVal())
            , newBlocksInLoop(BlockSetOps::UninitVal())
            , comp(comp)
            , oldBlockMaxNum(comp->fgBBNumMax)
        {
        }

        void Reset(unsigned int seedBlockNum)
        {
            if (BlockSetOps::MayBeUninit(oldBlocksInLoop))
            {
                // Either the block sets are uninitialized (and long), so we need to initialize
                // them (and allocate their backing storage), or they are short and empty, so
                // assigning MakeEmpty to them is as cheap as ClearD.
                oldBlocksInLoop = BlockSetOps::MakeEmpty(comp);
                newBlocksInLoop = BlockSetOps::MakeEmpty(comp);
            }
            else
            {
                // We know the backing storage is already allocated, so just clear it.
                BlockSetOps::ClearD(comp, oldBlocksInLoop);
                BlockSetOps::ClearD(comp, newBlocksInLoop);
            }
            assert(seedBlockNum <= oldBlockMaxNum);
            BlockSetOps::AddElemD(comp, oldBlocksInLoop, seedBlockNum);
        }

        bool CanRepresent(unsigned int blockNum)
        {
            // We can represent old blocks up to oldBlockMaxNum, and
            // new blocks up to 2 * oldBlockMaxNum.
            return (blockNum <= 2 * oldBlockMaxNum);
        }

        bool IsMember(unsigned int blockNum)
        {
            if (blockNum > oldBlockMaxNum)
            {
                return BlockSetOps::IsMember(comp, newBlocksInLoop, blockNum - oldBlockMaxNum);
            }
            return BlockSetOps::IsMember(comp, oldBlocksInLoop, blockNum);
        }

        void Insert(unsigned int blockNum)
        {
            if (blockNum > oldBlockMaxNum)
            {
                BlockSetOps::AddElemD(comp, newBlocksInLoop, blockNum - oldBlockMaxNum);
            }
            else
            {
                BlockSetOps::AddElemD(comp, oldBlocksInLoop, blockNum);
            }
        }

        bool TestAndInsert(unsigned int blockNum)
        {
            if (blockNum > oldBlockMaxNum)
            {
                unsigned int shiftedNum = blockNum - oldBlockMaxNum;
                if (!BlockSetOps::IsMember(comp, newBlocksInLoop, shiftedNum))
                {
                    BlockSetOps::AddElemD(comp, newBlocksInLoop, shiftedNum);
                    return false;
                }
            }
            else
            {
                if (!BlockSetOps::IsMember(comp, oldBlocksInLoop, blockNum))
                {
                    BlockSetOps::AddElemD(comp, oldBlocksInLoop, blockNum);
                    return false;
                }
            }
            return true;
        }
    };

    LoopBlockSet loopBlocks; // Set of blocks identified as part of the loop
    Compiler*    comp;

    // See LoopSearch class comment header for a diagram relating these fields:
    BasicBlock* head;   // Predecessor of unique entry edge
    BasicBlock* first;  // Lexically first in-loop block
    BasicBlock* top;    // Successor of back-edge from BOTTOM
    BasicBlock* bottom; // Predecessor of back-edge to TOP, also lexically last in-loop block
    BasicBlock* entry;  // Successor of unique entry edge

    BasicBlock* lastExit;       // Most recently discovered exit block
    unsigned    exitCount;      // Number of discovered exit edges
    unsigned    oldBlockMaxNum; // Used to identify new blocks created during compaction
    BlockSet    bottomBlocks;   // BOTTOM blocks of already-recorded loops
#ifdef DEBUG
    bool forgotExit = false; // Flags a rare case where lastExit gets nulled out, for assertions
#endif
    bool changedFlowGraph = false; // Signals that loop compaction has modified the flow graph

public:
    LoopSearch(Compiler* comp)
        : loopBlocks(comp), comp(comp), oldBlockMaxNum(comp->fgBBNumMax), bottomBlocks(BlockSetOps::MakeEmpty(comp))
    {
        // Make sure we've renumbered such that the bitsets can hold all the bits
        assert(comp->fgBBNumMax <= comp->fgCurBBEpochSize);
    }

    //------------------------------------------------------------------------
    // RecordLoop: Notify the Compiler that a loop has been found.
    //
    // Return Value:
    //    true  - Loop successfully recorded.
    //    false - Compiler has run out of loop descriptors; loop not recorded.
    //
    bool RecordLoop()
    {
        /* At this point we have a compact loop - record it in the loop table
        * If we found only one exit, record it in the table too
        * (otherwise an exit = nullptr in the loop table means multiple exits) */

        BasicBlock* onlyExit = (exitCount == 1 ? lastExit : nullptr);
        if (comp->optRecordLoop(head, first, top, entry, bottom, onlyExit, exitCount))
        {
            // Record the BOTTOM block for future reference before returning.
            assert(bottom->bbNum <= oldBlockMaxNum);
            BlockSetOps::AddElemD(comp, bottomBlocks, bottom->bbNum);
            return true;
        }

        // Unable to record this loop because the loop descriptor table overflowed.
        return false;
    }

    //------------------------------------------------------------------------
    // ChangedFlowGraph: Determine whether loop compaction has modified the flow graph.
    //
    // Return Value:
    //    true  - The flow graph has been modified; fgUpdateChangedFlowGraph should
    //            be called (which is the caller's responsibility).
    //    false - The flow graph has not been modified by this LoopSearch.
    //
    bool ChangedFlowGraph()
    {
        return changedFlowGraph;
    }

    //------------------------------------------------------------------------
    // FindLoop: Search for a loop with the given HEAD block and back-edge.
    //
    // Arguments:
    //    head - Block to be the HEAD of any loop identified
    //    top - Block to be the TOP of any loop identified
    //    bottom - Block to be the BOTTOM of any loop identified
    //
    // Return Value:
    //    true  - Found a valid loop.
    //    false - Did not find a valid loop.
    //
    // Notes:
    //    May modify flow graph to make loop compact before returning.
    //    Will set instance fields to track loop's extent and exits if a valid
    //    loop is found, and potentially trash them otherwise.
    //
    bool FindLoop(BasicBlock* head, BasicBlock* top, BasicBlock* bottom)
    {
        /* Is this a loop candidate? - We look for "back edges", i.e. an edge from BOTTOM
        * to TOP (note that this is an abuse of notation since this is not necessarily a back edge
        * as the definition says, but merely an indication that we have a loop there).
        * Thus, we have to be very careful and after entry discovery check that it is indeed
        * the only place we enter the loop (especially for non-reducible flow graphs).
        */

        if (top->bbNum > bottom->bbNum) // is this a backward edge? (from BOTTOM to TOP)
        {
            // Edge from BOTTOM to TOP is not a backward edge
            return false;
        }

        if (bottom->bbNum > oldBlockMaxNum)
        {
            // Not a true back-edge; bottom is a block added to reconnect fall-through during
            // loop processing, so its block number does not reflect its position.
            return false;
        }

        if ((bottom->bbJumpKind == BBJ_EHFINALLYRET) || (bottom->bbJumpKind == BBJ_EHFILTERRET) ||
            (bottom->bbJumpKind == BBJ_EHCATCHRET) || (bottom->bbJumpKind == BBJ_CALLFINALLY) ||
            (bottom->bbJumpKind == BBJ_SWITCH))
        {
            /* BBJ_EHFINALLYRET, BBJ_EHFILTERRET, BBJ_EHCATCHRET, and BBJ_CALLFINALLY can never form a loop.
            * BBJ_SWITCH that has a backward jump appears only for labeled break. */
            return false;
        }

        /* The presence of a "back edge" is an indication that a loop might be present here
        *
        * LOOP:
        *        1. A collection of STRONGLY CONNECTED nodes i.e. there is a path from any
        *           node in the loop to any other node in the loop (wholly within the loop)
        *        2. The loop has a unique ENTRY, i.e. there is only one way to reach a node
        *           in the loop from outside the loop, and that is through the ENTRY
        */

        /* Let's find the loop ENTRY */
        BasicBlock* entry = FindEntry(head, top, bottom);

        if (entry == nullptr)
        {
            // For now, we only recognize loops where HEAD has some successor ENTRY in the loop.
            return false;
        }

        // Passed the basic checks; initialize instance state for this back-edge.
        this->head      = head;
        this->top       = top;
        this->entry     = entry;
        this->bottom    = bottom;
        this->lastExit  = nullptr;
        this->exitCount = 0;

        // Now we find the "first" block -- the earliest block reachable within the loop.
        // With our current algorithm, this is always the same as "top".
        this->first = top;

        if (!HasSingleEntryCycle())
        {
            // There isn't actually a loop between TOP and BOTTOM
            return false;
        }

        if (!loopBlocks.IsMember(top->bbNum))
        {
            // The "back-edge" we identified isn't actually part of the flow cycle containing ENTRY
            return false;
        }

        // Disqualify loops where the first block of the loop is less nested in EH than
        // the bottom block. That is, we don't want to handle loops where the back edge
        // goes from within an EH region to a first block that is outside that same EH
        // region. Note that we *do* handle loops where the first block is the *first*
        // block of a more nested EH region (since it is legal to branch to the first
        // block of an immediately more nested EH region). So, for example, disqualify
        // this:
        //
        // BB02
        // ...
        // try {
        // ...
        // BB10 BBJ_COND => BB02
        // ...
        // }
        //
        // Here, BB10 is more nested than BB02.

        if (bottom->hasTryIndex() && !comp->bbInTryRegions(bottom->getTryIndex(), first))
        {
            JITDUMP("Loop 'first' " FMT_BB " is in an outer EH region compared to loop 'bottom' " FMT_BB ". Rejecting "
                    "loop.\n",
                    first->bbNum, bottom->bbNum);
            return false;
        }

#if defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
        // Disqualify loops where the first block of the loop is a finally target.
        // The main problem is when multiple loops share a 'first' block that is a finally
        // target and we canonicalize the loops by adding a new loop head. In that case, we
        // need to update the blocks so the finally target bit is moved to the newly created
        // block, and removed from the old 'first' block. This is 'hard', so at this point
        // in the RyuJIT codebase (when we don't expect to keep the "old" ARM32 code generator
        // long-term), it's easier to disallow the loop than to update the flow graph to
        // support this case.

        if ((first->bbFlags & BBF_FINALLY_TARGET) != 0)
        {
            JITDUMP("Loop 'first' " FMT_BB " is a finally target. Rejecting loop.\n", first->bbNum);
            return false;
        }
#endif // defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)

        // Compact the loop (sweep through it and move out any blocks that aren't part of the
        // flow cycle), and find the exits.
        if (!MakeCompactAndFindExits())
        {
            // Unable to preserve well-formed loop during compaction.
            return false;
        }

        // We have a valid loop.
        return true;
    }

    //------------------------------------------------------------------------
    // GetExitCount: Return the exit count computed for the loop
    //
    unsigned GetExitCount() const
    {
        return exitCount;
    }

private:
    //------------------------------------------------------------------------
    // FindEntry: See if given HEAD flows to valid ENTRY between given TOP and BOTTOM
    //
    // Arguments:
    //    head - Block to be the HEAD of any loop identified
    //    top - Block to be the TOP of any loop identified
    //    bottom - Block to be the BOTTOM of any loop identified
    //
    // Return Value:
    //    Block to be the ENTRY of any loop identified, or nullptr if no
    //    such entry meeting our criteria can be found.
    //
    // Notes:
    //    Returns main entry if one is found, does not check for side-entries.
    //
    BasicBlock* FindEntry(BasicBlock* head, BasicBlock* top, BasicBlock* bottom)
    {
        if (head->bbJumpKind == BBJ_ALWAYS)
        {
            if (head->bbJumpDest->bbNum <= bottom->bbNum && head->bbJumpDest->bbNum >= top->bbNum)
            {
                /* OK - we enter somewhere within the loop */

                /* some useful asserts
                * Cannot enter at the top - should have being caught by redundant jumps */

                assert((head->bbJumpDest != top) || (head->bbFlags & BBF_KEEP_BBJ_ALWAYS));

                return head->bbJumpDest;
            }
            else
            {
                /* special case - don't consider now */
                // assert (!"Loop entered in weird way!");
                return nullptr;
            }
        }
        // Can we fall through into the loop?
        else if (head->bbJumpKind == BBJ_NONE || head->bbJumpKind == BBJ_COND)
        {
            /* The ENTRY is at the TOP (a do-while loop) */
            return top;
        }
        else
        {
            return nullptr; // head does not flow into the loop bail for now
        }
    }

    //------------------------------------------------------------------------
    // HasSingleEntryCycle: Perform a reverse flow walk from ENTRY, visiting
    //    only blocks between TOP and BOTTOM, to determine if such a cycle
    //    exists and if it has a single entry.
    //
    // Return Value:
    //    true  - Found a single-entry cycle.
    //    false - Did not find a single-entry cycle.
    //
    // Notes:
    //    Will mark (in `loopBlocks`) all blocks found to participate in the
    //    cycle.
    //
    bool HasSingleEntryCycle()
    {
        // Now do a backwards flow walk from entry to see if we have a single-entry loop
        bool foundCycle = false;

        // Seed the loop block set and worklist with the entry block.
        loopBlocks.Reset(entry->bbNum);
        jitstd::list<BasicBlock*> worklist(comp->getAllocator(CMK_LoopOpt));
        worklist.push_back(entry);

        while (!worklist.empty())
        {
            BasicBlock* block = worklist.back();
            worklist.pop_back();

            /* Make sure ENTRY dominates all blocks in the loop
            * This is necessary to ensure condition 2. above
            */
            if (block->bbNum > oldBlockMaxNum)
            {
                // This is a new block we added to connect fall-through, so the
                // recorded dominator information doesn't cover it.  Just continue,
                // and when we process its unique predecessor we'll abort if ENTRY
                // doesn't dominate that.
            }
            else if (!comp->fgDominate(entry, block))
            {
                return false;
            }

            // Add preds to the worklist, checking for side-entries.
            for (BasicBlock* const predBlock : block->PredBlocks())
            {
                unsigned int testNum = PositionNum(predBlock);

                if ((testNum < top->bbNum) || (testNum > bottom->bbNum))
                {
                    // Pred is out of loop range
                    if (block == entry)
                    {
                        if (predBlock == head)
                        {
                            // This is the single entry we expect.
                            continue;
                        }
                        // ENTRY has some pred other than head outside the loop.  If ENTRY does not
                        // dominate this pred, we'll consider this a side-entry and skip this loop;
                        // otherwise the loop is still valid and this may be a (flow-wise) back-edge
                        // of an outer loop.  For the dominance test, if `predBlock` is a new block, use
                        // its unique predecessor since the dominator tree has info for that.
                        BasicBlock* effectivePred = (predBlock->bbNum > oldBlockMaxNum ? predBlock->bbPrev : predBlock);
                        if (comp->fgDominate(entry, effectivePred))
                        {
                            // Outer loop back-edge
                            continue;
                        }
                    }

                    // There are multiple entries to this loop, don't consider it.
                    return false;
                }

                bool isFirstVisit;
                if (predBlock == entry)
                {
                    // We have indeed found a cycle in the flow graph.
                    isFirstVisit = !foundCycle;
                    foundCycle   = true;
                    assert(loopBlocks.IsMember(predBlock->bbNum));
                }
                else if (loopBlocks.TestAndInsert(predBlock->bbNum))
                {
                    // Already visited this pred
                    isFirstVisit = false;
                }
                else
                {
                    // Add this predBlock to the worklist
                    worklist.push_back(predBlock);
                    isFirstVisit = true;
                }

                if (isFirstVisit && (predBlock->bbNext != nullptr) &&
                    (PositionNum(predBlock->bbNext) == predBlock->bbNum))
                {
                    // We've created a new block immediately after `predBlock` to
                    // reconnect what was fall-through.  Mark it as in-loop also;
                    // it needs to stay with `prev` and if it exits the loop we'd
                    // just need to re-create it if we tried to move it out.
                    loopBlocks.Insert(predBlock->bbNext->bbNum);
                }
            }
        }

        return foundCycle;
    }

    //------------------------------------------------------------------------
    // PositionNum: Get the number identifying a block's position per the
    //    lexical ordering that existed before searching for (and compacting)
    //    loops.
    //
    // Arguments:
    //    block - Block whose position is desired.
    //
    // Return Value:
    //    A number indicating that block's position relative to others.
    //
    // Notes:
    //    When the given block is a new one created during loop compaction,
    //    the number of its unique predecessor is returned.
    //
    unsigned int PositionNum(BasicBlock* block)
    {
        if (block->bbNum > oldBlockMaxNum)
        {
            // This must be a block we inserted to connect fall-through after moving blocks.
            // To determine if it's in the loop or not, use the number of its unique predecessor
            // block.
            assert(block->bbPreds->getBlock() == block->bbPrev);
            assert(block->bbPreds->flNext == nullptr);
            return block->bbPrev->bbNum;
        }
        return block->bbNum;
    }

    //------------------------------------------------------------------------
    // MakeCompactAndFindExits: Compact the loop (sweep through it and move out
    //   any blocks that aren't part of the flow cycle), and find the exits (set
    //   lastExit and exitCount).
    //
    // Return Value:
    //    true  - Loop successfully compacted (or `loopBlocks` expanded to
    //            include all blocks in the lexical range), exits enumerated.
    //    false - Loop cannot be made compact and remain well-formed.
    //
    bool MakeCompactAndFindExits()
    {
        // Compaction (if it needs to happen) will require an insertion point.
        BasicBlock* moveAfter = nullptr;

        for (BasicBlock* previous = top->bbPrev; previous != bottom;)
        {
            BasicBlock* block = previous->bbNext;

            if (loopBlocks.IsMember(block->bbNum))
            {
                // This block is a member of the loop.  Check to see if it may exit the loop.
                CheckForExit(block);

                // Done processing this block; move on to the next.
                previous = block;
                continue;
            }

            // This blocks is lexically between TOP and BOTTOM, but it does not
            // participate in the flow cycle.  Check for a run of consecutive
            // such blocks.
            BasicBlock* lastNonLoopBlock = block;
            BasicBlock* nextLoopBlock    = block->bbNext;
            while (!loopBlocks.IsMember(nextLoopBlock->bbNum))
            {
                lastNonLoopBlock = nextLoopBlock;
                nextLoopBlock    = nextLoopBlock->bbNext;
                // This loop must terminate because we know BOTTOM is in loopBlocks.
            }

            // Choose an insertion point for non-loop blocks if we haven't yet done so.
            if (moveAfter == nullptr)
            {
                moveAfter = FindInsertionPoint();
            }

            if (!BasicBlock::sameEHRegion(previous, nextLoopBlock) || !BasicBlock::sameEHRegion(previous, moveAfter))
            {
                // EH regions would be ill-formed if we moved these blocks out.
                // See if we can consider them loop blocks without introducing
                // a side-entry.
                if (CanTreatAsLoopBlocks(block, lastNonLoopBlock))
                {
                    // The call to `canTreatAsLoop` marked these blocks as part of the loop;
                    // iterate without updating `previous` so that we'll analyze them as part
                    // of the loop.
                    continue;
                }
                else
                {
                    // We can't move these out of the loop or leave them in, so just give
                    // up on this loop.
                    return false;
                }
            }

            // Now physically move the blocks.
            BasicBlock* moveBefore = moveAfter->bbNext;

            comp->fgUnlinkRange(block, lastNonLoopBlock);
            comp->fgMoveBlocksAfter(block, lastNonLoopBlock, moveAfter);
            comp->ehUpdateLastBlocks(moveAfter, lastNonLoopBlock);

            // Apply any adjustments needed for fallthrough at the boundaries of the moved region.
            FixupFallThrough(moveAfter, moveBefore, block);
            FixupFallThrough(lastNonLoopBlock, nextLoopBlock, moveBefore);
            // Also apply any adjustments needed where the blocks were snipped out of the loop.
            BasicBlock* newBlock = FixupFallThrough(previous, block, nextLoopBlock);
            if (newBlock != nullptr)
            {
                // This new block is in the loop and is a loop exit.
                loopBlocks.Insert(newBlock->bbNum);
                lastExit = newBlock;
                ++exitCount;
            }

            // Update moveAfter for the next insertion.
            moveAfter = lastNonLoopBlock;

            // Note that we've changed the flow graph, and continue without updating
            // `previous` so that we'll process nextLoopBlock.
            changedFlowGraph = true;
        }

        if ((exitCount == 1) && (lastExit == nullptr))
        {
            // If we happen to have a loop with two exits, one of which goes to an
            // infinite loop that's lexically nested inside it, where the inner loop
            // can't be moved out,  we can end up in this situation (because
            // CanTreatAsLoopBlocks will have decremented the count expecting to find
            // another exit later).  Bump the exit count to 2, since downstream code
            // will not be prepared for null lastExit with exitCount of 1.
            assert(forgotExit);
            exitCount = 2;
        }

        // Loop compaction was successful
        return true;
    }

    //------------------------------------------------------------------------
    // FindInsertionPoint: Find an appropriate spot to which blocks that are
    //    lexically between TOP and BOTTOM but not part of the flow cycle
    //    can be moved.
    //
    // Return Value:
    //    Block after which to insert moved blocks.
    //
    BasicBlock* FindInsertionPoint()
    {
        // Find an insertion point for blocks we're going to move.  Move them down
        // out of the loop, and if possible find a spot that won't break up fall-through.
        BasicBlock* moveAfter = bottom;
        while (moveAfter->bbFallsThrough())
        {
            // Keep looking for a better insertion point if we can.
            BasicBlock* newMoveAfter = TryAdvanceInsertionPoint(moveAfter);

            if (newMoveAfter == nullptr)
            {
                // Ran out of candidate insertion points, so just split up the fall-through.
                return moveAfter;
            }

            moveAfter = newMoveAfter;
        }

        return moveAfter;
    }

    //------------------------------------------------------------------------
    // TryAdvanceInsertionPoint: Find the next legal insertion point after
    //    the given one, if one exists.
    //
    // Arguments:
    //    oldMoveAfter - Prior insertion point; find the next after this.
    //
    // Return Value:
    //    The next block after `oldMoveAfter` that is a legal insertion point
    //    (i.e. blocks being swept out of the loop can be moved immediately
    //    after it), if one exists, else nullptr.
    //
    BasicBlock* TryAdvanceInsertionPoint(BasicBlock* oldMoveAfter)
    {
        BasicBlock* newMoveAfter = oldMoveAfter->bbNext;

        if (!BasicBlock::sameEHRegion(oldMoveAfter, newMoveAfter))
        {
            // Don't cross an EH region boundary.
            return nullptr;
        }

        if ((newMoveAfter->bbJumpKind == BBJ_ALWAYS) || (newMoveAfter->bbJumpKind == BBJ_COND))
        {
            unsigned int destNum = newMoveAfter->bbJumpDest->bbNum;
            if ((destNum >= top->bbNum) && (destNum <= bottom->bbNum) && !loopBlocks.IsMember(destNum))
            {
                // Reversing this branch out of block `newMoveAfter` could confuse this algorithm
                // (in particular, the edge would still be numerically backwards but no longer be
                // lexically backwards, so a lexical forward walk from TOP would not find BOTTOM),
                // so don't do that.
                // We're checking for BBJ_ALWAYS and BBJ_COND only here -- we don't need to
                // check for BBJ_SWITCH because we'd never consider it a loop back-edge.
                return nullptr;
            }
        }

        // Similarly check to see if advancing to `newMoveAfter` would reverse the lexical order
        // of an edge from the run of blocks being moved to `newMoveAfter` -- doing so would
        // introduce a new lexical back-edge, which could (maybe?) confuse the loop search
        // algorithm, and isn't desirable layout anyway.
        for (BasicBlock* const predBlock : newMoveAfter->PredBlocks())
        {
            unsigned int predNum = predBlock->bbNum;

            if ((predNum >= top->bbNum) && (predNum <= bottom->bbNum) && !loopBlocks.IsMember(predNum))
            {
                // Don't make this forward edge a backwards edge.
                return nullptr;
            }
        }

        if (IsRecordedBottom(newMoveAfter))
        {
            // This is the BOTTOM of another loop; don't move any blocks past it, to avoid moving them
            // out of that loop (we should have already done so when processing that loop if it were legal).
            return nullptr;
        }

        // Advancing the insertion point is ok, except that we can't split up any CallFinally/BBJ_ALWAYS
        // pair, so if we've got such a pair recurse to see if we can move past the whole thing.
        return (newMoveAfter->isBBCallAlwaysPair() ? TryAdvanceInsertionPoint(newMoveAfter) : newMoveAfter);
    }

    //------------------------------------------------------------------------
    // isOuterBottom: Determine if the given block is the BOTTOM of a previously
    //    recorded loop.
    //
    // Arguments:
    //    block - Block to check for BOTTOM-ness.
    //
    // Return Value:
    //    true - The blocks was recorded as `bottom` of some earlier-processed loop.
    //    false - No loops yet recorded have this block as their `bottom`.
    //
    bool IsRecordedBottom(BasicBlock* block)
    {
        if (block->bbNum > oldBlockMaxNum)
        {
            // This is a new block, which can't be an outer bottom block because we only allow old blocks
            // as BOTTOM.
            return false;
        }
        return BlockSetOps::IsMember(comp, bottomBlocks, block->bbNum);
    }

    //------------------------------------------------------------------------
    // CanTreatAsLoopBlocks: If the given range of blocks can be treated as
    //    loop blocks, add them to loopBlockSet and return true.  Otherwise,
    //    return false.
    //
    // Arguments:
    //    firstNonLoopBlock - First block in the run to be subsumed.
    //    lastNonLoopBlock - Last block in the run to be subsumed.
    //
    // Return Value:
    //    true - The blocks from `fistNonLoopBlock` to `lastNonLoopBlock` were
    //           successfully added to `loopBlocks`.
    //    false - Treating the blocks from `fistNonLoopBlock` to `lastNonLoopBlock`
    //            would not be legal (it would induce a side-entry).
    //
    // Notes:
    //    `loopBlocks` may be modified even if `false` is returned.
    //    `exitCount` and `lastExit` may be modified if this process identifies
    //    in-loop edges that were previously counted as exits.
    //
    bool CanTreatAsLoopBlocks(BasicBlock* firstNonLoopBlock, BasicBlock* lastNonLoopBlock)
    {
        for (BasicBlock* const testBlock : comp->Blocks(firstNonLoopBlock, lastNonLoopBlock))
        {
            for (BasicBlock* const testPred : testBlock->PredBlocks())
            {
                unsigned int predPosNum         = PositionNum(testPred);
                unsigned int firstNonLoopPosNum = PositionNum(firstNonLoopBlock);
                unsigned int lastNonLoopPosNum  = PositionNum(lastNonLoopBlock);

                if (loopBlocks.IsMember(predPosNum) ||
                    ((predPosNum >= firstNonLoopPosNum) && (predPosNum <= lastNonLoopPosNum)))
                {
                    // This pred is in the loop (or what will be the loop if we determine this
                    // run of exit blocks doesn't include a side-entry).

                    if (predPosNum < firstNonLoopPosNum)
                    {
                        // We've already counted this block as an exit, so decrement the count.
                        --exitCount;
                        if (lastExit == testPred)
                        {
                            // Erase this now-bogus `lastExit` entry.
                            lastExit = nullptr;
                            INDEBUG(forgotExit = true);
                        }
                    }
                }
                else
                {
                    // This pred is not in the loop, so this constitutes a side-entry.
                    return false;
                }
            }

            // Either we're going to abort the loop on a subsequent testBlock, or this
            // testBlock is part of the loop.
            loopBlocks.Insert(testBlock->bbNum);
        }

        // All blocks were ok to leave in the loop.
        return true;
    }

    //------------------------------------------------------------------------
    // FixupFallThrough: Re-establish any broken control flow connectivity
    //    and eliminate any "goto-next"s that were created by changing the
    //    given block's lexical follower.
    //
    // Arguments:
    //    block - Block whose `bbNext` has changed.
    //    oldNext - Previous value of `block->bbNext`.
    //    newNext - New value of `block->bbNext`.
    //
    // Return Value:
    //    If a new block is created to reconnect flow, the new block is
    //    returned; otherwise, nullptr.
    //
    BasicBlock* FixupFallThrough(BasicBlock* block, BasicBlock* oldNext, BasicBlock* newNext)
    {
        // If we create a new block, that will be our return value.
        BasicBlock* newBlock = nullptr;

        if (block->bbFallsThrough())
        {
            // Need to reconnect the flow from `block` to `oldNext`.

            if ((block->bbJumpKind == BBJ_COND) && (block->bbJumpDest == newNext))
            {
                /* Reverse the jump condition */
                GenTree* test = block->lastNode();
                noway_assert(test->OperIsConditionalJump());

                if (test->OperGet() == GT_JTRUE)
                {
                    GenTree* cond = comp->gtReverseCond(test->AsOp()->gtOp1);
                    assert(cond == test->AsOp()->gtOp1); // Ensure `gtReverseCond` did not create a new node.
                    test->AsOp()->gtOp1 = cond;
                }
                else
                {
                    comp->gtReverseCond(test);
                }

                // Redirect the Conditional JUMP to go to `oldNext`
                block->bbJumpDest = oldNext;
            }
            else
            {
                // Insert an unconditional jump to `oldNext` just after `block`.
                newBlock = comp->fgConnectFallThrough(block, oldNext);
                noway_assert((newBlock == nullptr) || loopBlocks.CanRepresent(newBlock->bbNum));
            }
        }
        else if ((block->bbJumpKind == BBJ_ALWAYS) && (block->bbJumpDest == newNext))
        {
            // We've made `block`'s jump target its bbNext, so remove the jump.
            if (!comp->fgOptimizeBranchToNext(block, newNext, block->bbPrev))
            {
                // If optimizing away the goto-next failed for some reason, mark it KEEP_BBJ_ALWAYS to
                // prevent assertions from complaining about it.
                block->bbFlags |= BBF_KEEP_BBJ_ALWAYS;
            }
        }

        // Make sure we don't leave around a goto-next unless it's marked KEEP_BBJ_ALWAYS.
        assert(((block->bbJumpKind != BBJ_COND) && (block->bbJumpKind != BBJ_ALWAYS)) ||
               (block->bbJumpDest != newNext) || ((block->bbFlags & BBF_KEEP_BBJ_ALWAYS) != 0));
        return newBlock;
    }

    //------------------------------------------------------------------------
    // CheckForExit: Check if the given block has any successor edges that are
    //    loop exits, and update `lastExit` and `exitCount` if so.
    //
    // Arguments:
    //    block - Block whose successor edges are to be checked.
    //
    // Notes:
    //    If one block has multiple exiting successor edges, those are counted
    //    as multiple exits in `exitCount`.
    //
    void CheckForExit(BasicBlock* block)
    {
        BasicBlock* exitPoint;

        switch (block->bbJumpKind)
        {
            case BBJ_COND:
            case BBJ_CALLFINALLY:
            case BBJ_ALWAYS:
            case BBJ_EHCATCHRET:
                assert(block->bbJumpDest);
                exitPoint = block->bbJumpDest;

                if (!loopBlocks.IsMember(exitPoint->bbNum))
                {
                    /* exit from a block other than BOTTOM */
                    lastExit = block;
                    exitCount++;
                }
                break;

            case BBJ_NONE:
                break;

            case BBJ_EHFINALLYRET:
            case BBJ_EHFILTERRET:
                /* The "try" associated with this "finally" must be in the
                * same loop, so the finally block will return control inside the loop */
                break;

            case BBJ_THROW:
            case BBJ_RETURN:
                /* those are exits from the loop */
                lastExit = block;
                exitCount++;
                break;

            case BBJ_SWITCH:
                for (BasicBlock* const exitPoint : block->SwitchTargets())
                {
                    if (!loopBlocks.IsMember(exitPoint->bbNum))
                    {
                        lastExit = block;
                        exitCount++;
                    }
                }
                break;

            default:
                noway_assert(!"Unexpected bbJumpKind");
                break;
        }

        if (block->bbFallsThrough() && !loopBlocks.IsMember(block->bbNext->bbNum))
        {
            // Found a fall-through exit.
            lastExit = block;
            exitCount++;
        }
    }
};
}

/*****************************************************************************
 * Find the natural loops, using dominators. Note that the test for
 * a loop is slightly different from the standard one, because we have
 * not done a depth first reordering of the basic blocks.
 */

void Compiler::optFindNaturalLoops()
{
#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In optFindNaturalLoops()\n");
    }
#endif // DEBUG

    noway_assert(fgDomsComputed);
    assert(fgHasLoops);

#if COUNT_LOOPS
    hasMethodLoops         = false;
    loopsThisMethod        = 0;
    loopOverflowThisMethod = false;
#endif

    LoopSearch search(this);

    for (BasicBlock* head = fgFirstBB; head->bbNext != nullptr; head = head->bbNext)
    {
        BasicBlock* top = head->bbNext;

        //  Blocks that are rarely run have a zero bbWeight and should
        //  never be optimized here

        if (top->bbWeight == BB_ZERO_WEIGHT)
        {
            continue;
        }

        for (BasicBlock* const predBlock : top->PredBlocks())
        {
            if (search.FindLoop(head, top, predBlock))
            {
                // Found a loop; record it and see if we've hit the limit.
                bool recordedLoop = search.RecordLoop();

                (void)recordedLoop; // avoid unusued variable warnings in COUNT_LOOPS and !DEBUG

#if COUNT_LOOPS
                if (!hasMethodLoops)
                {
                    /* mark the method as containing natural loops */
                    totalLoopMethods++;
                    hasMethodLoops = true;
                }

                /* increment total number of loops found */
                totalLoopCount++;
                loopsThisMethod++;

                /* keep track of the number of exits */
                loopExitCountTable.record(search.GetExitCount());

                // Note that we continue to look for loops even if
                // (optLoopCount == BasicBlock::MAX_LOOP_NUM), in contrast to the !COUNT_LOOPS code below.
                // This gives us a better count and stats. Hopefully it doesn't affect actual codegen.
                CLANG_FORMAT_COMMENT_ANCHOR;

#else  // COUNT_LOOPS
                assert(recordedLoop);
                if (optLoopCount == BasicBlock::MAX_LOOP_NUM)
                {
                    // We won't be able to record any more loops, so stop looking.
                    goto NO_MORE_LOOPS;
                }
#endif // COUNT_LOOPS

                // Continue searching preds of `top` to see if any other are
                // back-edges (this can happen for nested loops).  The iteration
                // is safe because the compaction we do only modifies predecessor
                // lists of blocks that gain or lose fall-through from their
                // `bbPrev`, but since the motion is from within the loop to below
                // it, we know we're not altering the relationship between `top`
                // and its `bbPrev`.
            }
        }
    }

#if !COUNT_LOOPS
NO_MORE_LOOPS:
#endif // !COUNT_LOOPS

#if COUNT_LOOPS
    loopCountTable.record(loopsThisMethod);
    if (maxLoopsPerMethod < loopsThisMethod)
    {
        maxLoopsPerMethod = loopsThisMethod;
    }
    if (loopOverflowThisMethod)
    {
        totalLoopOverflows++;
    }
#endif // COUNT_LOOPS

    bool mod = search.ChangedFlowGraph();

    if (mod)
    {
        // Need to renumber blocks now since loop canonicalization
        // depends on it; can defer the rest of fgUpdateChangedFlowGraph()
        // until after canonicalizing loops.  Dominator information is
        // recorded in terms of block numbers, so flag it invalid.
        fgDomsComputed = false;
        fgRenumberBlocks();
    }

    // Now the loop indices are stable.  We can figure out parent/child relationships
    // (using table indices to name loops), and label blocks.
    for (unsigned loopInd = 1; loopInd < optLoopCount; loopInd++)
    {
        for (unsigned possibleParent = loopInd; possibleParent > 0;)
        {
            possibleParent--;
            if (optLoopTable[possibleParent].lpContains(optLoopTable[loopInd]))
            {
                optLoopTable[loopInd].lpParent       = static_cast<uint8_t>(possibleParent);
                optLoopTable[loopInd].lpSibling      = optLoopTable[possibleParent].lpChild;
                optLoopTable[possibleParent].lpChild = static_cast<uint8_t>(loopInd);
                break;
            }
        }
    }

    // Now label the blocks with the innermost loop to which they belong.  Since parents
    // precede children in the table, doing the labeling for each loop in order will achieve
    // this -- the innermost loop labeling will be done last.
    for (unsigned loopInd = 0; loopInd < optLoopCount; loopInd++)
    {
        for (BasicBlock* const blk : optLoopTable[loopInd].LoopBlocks())
        {
            blk->bbNatLoopNum = static_cast<LoopNum>(loopInd);
        }
    }

    // Make sure that loops are canonical: that every loop has a unique "top", by creating an empty "nop"
    // one, if necessary, for loops containing others that share a "top."
    for (unsigned loopInd = 0; loopInd < optLoopCount; loopInd++)
    {
        // Traverse the outermost loops as entries into the loop nest; so skip non-outermost.
        if (optLoopTable[loopInd].lpParent != BasicBlock::NOT_IN_LOOP)
        {
            continue;
        }

        // Otherwise...
        if (optCanonicalizeLoopNest(loopInd))
        {
            mod = true;
        }
    }
    if (mod)
    {
        fgUpdateChangedFlowGraph(/*computePreds*/ true);
        fgComputeDoms();
    }

#ifdef DEBUG
    if (verbose && optLoopCount > 0)
    {
        printf("\nFinal natural loop table:\n");
        for (unsigned loopInd = 0; loopInd < optLoopCount; loopInd++)
        {
            optPrintLoopInfo(loopInd);
            printf("\n");
        }
    }
#endif // DEBUG
}

//-----------------------------------------------------------------------------
//
// All the inner loops that whose block weight meets a threshold are marked
// as needing alignment.
//

void Compiler::optIdentifyLoopsForAlignment()
{
#if FEATURE_LOOP_ALIGN
    if (codeGen->ShouldAlignLoops())
    {
        for (unsigned loopInd = 0; loopInd < optLoopCount; loopInd++)
        {
            BasicBlock* first = optLoopTable[loopInd].lpFirst;

            // An innerloop candidate that might need alignment
            if (optLoopTable[loopInd].lpChild == BasicBlock::NOT_IN_LOOP)
            {
                if (first->getBBWeight(this) >= (opts.compJitAlignLoopMinBlockWeight * BB_UNITY_WEIGHT))
                {
                    first->bbFlags |= BBF_LOOP_ALIGN;
                    JITDUMP(FMT_LP " that starts at " FMT_BB " needs alignment, weight=" FMT_WT ".\n", loopInd,
                            first->bbNum, first->getBBWeight(this));
                }
                else
                {
                    JITDUMP("Skip alignment for " FMT_LP " that starts at " FMT_BB " weight=" FMT_WT ".\n", loopInd,
                            first->bbNum, first->getBBWeight(this));
                }
            }
        }
    }
#endif
}

//------------------------------------------------------------------------
// optRedirectBlock: Replace the branch successors of a block based on a block map.
//
// Updates the successors of `blk`: if `blk2` is a branch successor of `blk`, and there is a mapping
// for `blk2->blk3` in `redirectMap`, change `blk` so that `blk3` is this branch successor.
//
// Note that fall-through successors are not modified, including predecessor lists.
//
// Arguments:
//     blk          - block to redirect
//     redirectMap  - block->block map specifying how the `blk` target will be redirected.
//     updatePreds  - if `true`, update the predecessor lists to match.
//
void Compiler::optRedirectBlock(BasicBlock* blk, BlockToBlockMap* redirectMap, const bool updatePreds)
{
    BasicBlock* newJumpDest = nullptr;
    switch (blk->bbJumpKind)
    {
        case BBJ_NONE:
        case BBJ_THROW:
        case BBJ_RETURN:
        case BBJ_EHFILTERRET:
        case BBJ_EHFINALLYRET:
        case BBJ_EHCATCHRET:
            // These have no jump destination to update.
            break;

        case BBJ_ALWAYS:
        case BBJ_LEAVE:
        case BBJ_CALLFINALLY:
        case BBJ_COND:
            // All of these have a single jump destination to update.
            if (redirectMap->Lookup(blk->bbJumpDest, &newJumpDest))
            {
                if (updatePreds)
                {
                    fgRemoveRefPred(blk->bbJumpDest, blk);
                    fgAddRefPred(newJumpDest, blk);
                }
                blk->bbJumpDest = newJumpDest;
            }
            break;

        case BBJ_SWITCH:
        {
            bool redirected = false;
            for (unsigned i = 0; i < blk->bbJumpSwt->bbsCount; i++)
            {
                BasicBlock* switchDest = blk->bbJumpSwt->bbsDstTab[i];
                if (redirectMap->Lookup(switchDest, &newJumpDest))
                {
                    if (updatePreds)
                    {
                        fgRemoveRefPred(switchDest, blk);
                        fgAddRefPred(newJumpDest, blk);
                    }
                    blk->bbJumpSwt->bbsDstTab[i] = newJumpDest;
                    redirected                   = true;
                }
            }
            // If any redirections happened, invalidate the switch table map for the switch.
            if (redirected)
            {
                blk->bbJumpSwt->nonDuplicates = nullptr;
            }
        }
        break;

        default:
            unreached();
    }
}

// TODO-Cleanup: This should be a static member of the BasicBlock class.
void Compiler::optCopyBlkDest(BasicBlock* from, BasicBlock* to)
{
    assert(from->bbJumpKind == to->bbJumpKind); // Precondition.

    // copy the jump destination(s) from "from" to "to".
    switch (to->bbJumpKind)
    {
        case BBJ_ALWAYS:
        case BBJ_LEAVE:
        case BBJ_CALLFINALLY:
        case BBJ_COND:
            // All of these have a single jump destination to update.
            to->bbJumpDest = from->bbJumpDest;
            break;

        case BBJ_SWITCH:
            to->bbJumpSwt = new (this, CMK_BasicBlock) BBswtDesc(this, from->bbJumpSwt);
            break;

        default:
            break;
    }
}

// Returns true if 'block' is an entry block for any loop in 'optLoopTable'
bool Compiler::optIsLoopEntry(BasicBlock* block) const
{
    for (unsigned loopInd = 0; loopInd < optLoopCount; loopInd++)
    {
        // Traverse the outermost loops as entries into the loop nest; so skip non-outermost.
        if (optLoopTable[loopInd].lpEntry == block)
        {
            return true;
        }
    }
    return false;
}

// Canonicalize the loop nest rooted at parent loop 'loopInd'.
// Returns 'true' if the flow graph is modified.
bool Compiler::optCanonicalizeLoopNest(unsigned loopInd)
{
    bool modified = false;

    // Is the top of the current loop not in any nested loop?
    if (optLoopTable[loopInd].lpTop->bbNatLoopNum != loopInd)
    {
        if (optCanonicalizeLoop(loopInd))
        {
            modified = true;
        }
    }

    for (unsigned child = optLoopTable[loopInd].lpChild; child != BasicBlock::NOT_IN_LOOP;
         child          = optLoopTable[child].lpSibling)
    {
        if (optCanonicalizeLoopNest(child))
        {
            modified = true;
        }
    }

    return modified;
}

bool Compiler::optCanonicalizeLoop(unsigned loopInd)
{
    // Is the top uniquely part of the current loop?
    BasicBlock* t = optLoopTable[loopInd].lpTop;

    if (t->bbNatLoopNum == loopInd)
    {
        return false;
    }

    JITDUMP("in optCanonicalizeLoop: " FMT_LP " has top " FMT_BB " (bottom " FMT_BB ") with natural loop number " FMT_LP
            ": need to canonicalize\n",
            loopInd, t->bbNum, optLoopTable[loopInd].lpBottom->bbNum, t->bbNatLoopNum);

    // Otherwise, the top of this loop is also part of a nested loop.
    //
    // Insert a new unique top for this loop. We must be careful to put this new
    // block in the correct EH region. Note that f->bbPrev might be in a different
    // EH region. For example:
    //
    // try {
    //      ...
    //      BB07
    // }
    // BB08 // "first"
    //
    // In this case, first->bbPrev is BB07, which is in a different 'try' region.
    // On the other hand, the first block of multiple loops might be the first
    // block of a 'try' region that is completely contained in the multiple loops.
    // for example:
    //
    // BB08 try { }
    // ...
    // BB10 BBJ_ALWAYS => BB08
    // ...
    // BB12 BBJ_ALWAYS => BB08
    //
    // Here, we have two loops, both with BB08 as the "first" block. Block BB08
    // is a single-block "try" region. Neither loop "bottom" block is in the same
    // "try" region as BB08. This is legal because you can jump to the first block
    // of a try region. With EH normalization, no two "try" regions will share
    // this block. In this case, we need to insert a new block for the outer loop
    // in the same EH region as the branch from the "bottom":
    //
    // BB30 BBJ_NONE
    // BB08 try { }
    // ...
    // BB10 BBJ_ALWAYS => BB08
    // ...
    // BB12 BBJ_ALWAYS => BB30
    //
    // Another possibility is that the "first" block of the loop nest can be the first block
    // of a "try" region that also has other predecessors than those in the loop, or even in
    // the "try" region (since blocks can target the first block of a "try" region). For example:
    //
    // BB08 try {
    // ...
    // BB10 BBJ_ALWAYS => BB08
    // ...
    // BB12 BBJ_ALWAYS => BB08
    // BB13 }
    // ...
    // BB20 BBJ_ALWAYS => BB08
    // ...
    // BB25 BBJ_ALWAYS => BB08
    //
    // Here, BB08 has 4 flow graph predecessors: BB10, BB12, BB20, BB25. These are all potential loop
    // bottoms, for four possible nested loops. However, we require all the loop bottoms to be in the
    // same EH region. For loops BB08..BB10 and BB08..BB12, we need to add a new "top" block within
    // the try region, immediately before BB08. The bottom of the loop BB08..BB10 loop will target the
    // old BB08, and the bottom of the BB08..BB12 loop will target the new loop header. The other branches
    // (BB20, BB25) must target the new loop header, both for correctness, and to avoid the illegal
    // situation of branching to a non-first block of a 'try' region.
    //
    // We can also have a loop nest where the "first" block is outside of a "try" region
    // and the back edges are inside a "try" region, for example:
    //
    // BB02 // "first"
    // ...
    // BB09 try { BBJ_COND => BB02
    // ...
    // BB15 BBJ_COND => BB02
    // ...
    // BB21 } // end of "try"
    //
    // In this case, both loop back edges were formed by "leave" instructions that were
    // imported into branches that were later made conditional. In this case, we don't
    // want to copy the EH region of the back edge, since that would create a block
    // outside of and disjoint with the "try" region of the back edge. However, to
    // simplify things, we disqualify this type of loop, so we should never see this here.

    BasicBlock* h = optLoopTable[loopInd].lpHead;
    BasicBlock* f = optLoopTable[loopInd].lpFirst;
    BasicBlock* b = optLoopTable[loopInd].lpBottom;

    // The loop must be entirely contained within a single handler region.
    assert(BasicBlock::sameHndRegion(f, b));

    // If the bottom block is in the same "try" region, then we extend the EH
    // region. Otherwise, we add the new block outside the "try" region.
    bool        extendRegion = BasicBlock::sameTryRegion(f, b);
    BasicBlock* newT         = fgNewBBbefore(BBJ_NONE, f, extendRegion);
    if (!extendRegion)
    {
        // We need to set the EH region manually. Set it to be the same
        // as the bottom block.
        newT->copyEHRegion(b);
    }

    // The new block can reach the same set of blocks as the old one, but don't try to reflect
    // that in its reachability set here -- creating the new block may have changed the BlockSet
    // representation from short to long, and canonicalizing loops is immediately followed by
    // a call to fgUpdateChangedFlowGraph which will recompute the reachability sets anyway.

    // Redirect the "bottom" of the current loop to "newT".
    BlockToBlockMap* blockMap = new (getAllocator(CMK_LoopOpt)) BlockToBlockMap(getAllocator(CMK_LoopOpt));
    blockMap->Set(t, newT);
    optRedirectBlock(b, blockMap);

    // Redirect non-loop preds of "t" to also go to "newT". Inner loops that also branch to "t" should continue
    // to do so. However, there maybe be other predecessors from outside the loop nest that need to be updated
    // to point to "newT". This normally wouldn't happen, since they too would be part of the loop nest. However,
    // they might have been prevented from participating in the loop nest due to different EH nesting, or some
    // other reason.
    //
    // Note that optRedirectBlock doesn't update the predecessors list. So, if the same 't' block is processed
    // multiple times while canonicalizing multiple loop nests, we'll attempt to redirect a predecessor multiple times.
    // This is ok, because after the first redirection, the topPredBlock branch target will no longer match the source
    // edge of the blockMap, so nothing will happen.
    bool firstPred = true;
    for (BasicBlock* const topPredBlock : t->PredBlocks())
    {
        // Skip if topPredBlock is in the loop.
        // Note that this uses block number to detect membership in the loop. We are adding blocks during
        // canonicalization, and those block numbers will be new, and larger than previous blocks. However, we work
        // outside-in, so we shouldn't encounter the new blocks at the loop boundaries, or in the predecessor lists.
        if (t->bbNum <= topPredBlock->bbNum && topPredBlock->bbNum <= b->bbNum)
        {
            JITDUMP("in optCanonicalizeLoop: 'top' predecessor " FMT_BB " is in the range of " FMT_LP " (" FMT_BB
                    ".." FMT_BB "); not redirecting its bottom edge\n",
                    topPredBlock->bbNum, loopInd, t->bbNum, b->bbNum);
            continue;
        }

        JITDUMP("in optCanonicalizeLoop: redirect top predecessor " FMT_BB " to " FMT_BB "\n", topPredBlock->bbNum,
                newT->bbNum);
        optRedirectBlock(topPredBlock, blockMap);

        // When we have profile data then the 'newT' block will inherit topPredBlock profile weight
        if (topPredBlock->hasProfileWeight())
        {
            // This corrects an issue when the topPredBlock has a profile based weight
            //
            if (firstPred)
            {
                JITDUMP("in optCanonicalizeLoop: block " FMT_BB " will inheritWeight from " FMT_BB "\n", newT->bbNum,
                        topPredBlock->bbNum);

                newT->inheritWeight(topPredBlock);
                firstPred = false;
            }
            else
            {
                JITDUMP("in optCanonicalizeLoop: block " FMT_BB " will also contribute to the weight of " FMT_BB "\n",
                        newT->bbNum, topPredBlock->bbNum);

                BasicBlock::weight_t newWeight = newT->getBBWeight(this) + topPredBlock->getBBWeight(this);
                newT->setBBProfileWeight(newWeight);
            }
        }
    }

    assert(newT->bbNext == f);
    if (f != t)
    {
        newT->bbJumpKind = BBJ_ALWAYS;
        newT->bbJumpDest = t;
        newT->bbStmtList = nullptr;
        fgInsertStmtAtEnd(newT, fgNewStmtFromTree(gtNewOperNode(GT_NOP, TYP_VOID, nullptr)));
    }

    // If it had been a do-while loop (top == entry), update entry, as well.
    BasicBlock* origE = optLoopTable[loopInd].lpEntry;
    if (optLoopTable[loopInd].lpTop == origE)
    {
        optLoopTable[loopInd].lpEntry = newT;
    }
    optLoopTable[loopInd].lpTop   = newT;
    optLoopTable[loopInd].lpFirst = newT;

    newT->bbNatLoopNum = static_cast<LoopNum>(loopInd);

    JITDUMP("in optCanonicalizeLoop: made new block " FMT_BB " [%p] the new unique top of loop %d.\n", newT->bbNum,
            dspPtr(newT), loopInd);

    // Make sure the head block still goes to the entry...
    if (h->bbJumpKind == BBJ_NONE && h->bbNext != optLoopTable[loopInd].lpEntry)
    {
        h->bbJumpKind = BBJ_ALWAYS;
        h->bbJumpDest = optLoopTable[loopInd].lpEntry;
    }
    else if (h->bbJumpKind == BBJ_COND && h->bbNext == newT && newT != optLoopTable[loopInd].lpEntry)
    {
        BasicBlock* h2               = fgNewBBafter(BBJ_ALWAYS, h, /*extendRegion*/ true);
        optLoopTable[loopInd].lpHead = h2;
        h2->bbJumpDest               = optLoopTable[loopInd].lpEntry;
        h2->bbStmtList               = nullptr;
        fgInsertStmtAtEnd(h2, fgNewStmtFromTree(gtNewOperNode(GT_NOP, TYP_VOID, nullptr)));
    }

    // If any loops nested in "loopInd" have the same head and entry as "loopInd",
    // it must be the case that they were do-while's (since "h" fell through to the entry).
    // The new node "newT" becomes the head of such loops.
    for (unsigned childLoop = optLoopTable[loopInd].lpChild; childLoop != BasicBlock::NOT_IN_LOOP;
         childLoop          = optLoopTable[childLoop].lpSibling)
    {
        if (optLoopTable[childLoop].lpEntry == origE && optLoopTable[childLoop].lpHead == h &&
            newT->bbJumpKind == BBJ_NONE && newT->bbNext == origE)
        {
            optUpdateLoopHead(childLoop, h, newT);
        }
    }
    return true;
}

bool Compiler::optLoopContains(unsigned l1, unsigned l2)
{
    assert(l1 != BasicBlock::NOT_IN_LOOP);
    if (l1 == l2)
    {
        return true;
    }
    else if (l2 == BasicBlock::NOT_IN_LOOP)
    {
        return false;
    }
    else
    {
        return optLoopContains(l1, optLoopTable[l2].lpParent);
    }
}

void Compiler::optUpdateLoopHead(unsigned loopInd, BasicBlock* from, BasicBlock* to)
{
    assert(optLoopTable[loopInd].lpHead == from);
    optLoopTable[loopInd].lpHead = to;
    for (unsigned childLoop = optLoopTable[loopInd].lpChild; childLoop != BasicBlock::NOT_IN_LOOP;
         childLoop          = optLoopTable[childLoop].lpSibling)
    {
        if (optLoopTable[childLoop].lpHead == from)
        {
            optUpdateLoopHead(childLoop, from, to);
        }
    }
}

//-----------------------------------------------------------------------------
// optIterSmallOverflow: Helper for loop unrolling. Determine if "i += const" will
// cause an overflow exception for the small types.
//
// Arguments:
//    iterAtExit - iteration constant at loop exit
//    incrType   - type of increment
//
// Returns:
//   true if overflow
//
// static
bool Compiler::optIterSmallOverflow(int iterAtExit, var_types incrType)
{
    int type_MAX;

    switch (incrType)
    {
        case TYP_BYTE:
            type_MAX = SCHAR_MAX;
            break;
        case TYP_UBYTE:
            type_MAX = UCHAR_MAX;
            break;
        case TYP_SHORT:
            type_MAX = SHRT_MAX;
            break;
        case TYP_USHORT:
            type_MAX = USHRT_MAX;
            break;

        case TYP_UINT: // Detected by checking for 32bit ....
        case TYP_INT:
            return false; // ... overflow same as done for TYP_INT

        default:
            NO_WAY("Bad type");
    }

    if (iterAtExit > type_MAX)
    {
        return true;
    }
    else
    {
        return false;
    }
}

//-----------------------------------------------------------------------------
// optIterSmallUnderflow: Helper for loop unrolling. Determine if "i -= const" will
// cause an underflow exception for the small types.
//
// Arguments:
//    iterAtExit - iteration constant at loop exit
//    decrType   - type of decrement
//
// Returns:
//   true if overflow
//
// static
bool Compiler::optIterSmallUnderflow(int iterAtExit, var_types decrType)
{
    int type_MIN;

    switch (decrType)
    {
        case TYP_BYTE:
            type_MIN = SCHAR_MIN;
            break;
        case TYP_SHORT:
            type_MIN = SHRT_MIN;
            break;
        case TYP_UBYTE:
            type_MIN = 0;
            break;
        case TYP_USHORT:
            type_MIN = 0;
            break;

        case TYP_UINT: // Detected by checking for 32bit ....
        case TYP_INT:
            return false; // ... underflow same as done for TYP_INT

        default:
            NO_WAY("Bad type");
    }

    if (iterAtExit < type_MIN)
    {
        return true;
    }
    else
    {
        return false;
    }
}

//-----------------------------------------------------------------------------
// optComputeLoopRep: Helper for loop unrolling. Computes the number of repetitions
// in a constant loop.
//
// Arguments:
//    constInit    - loop constant initial value
//    constLimit   - loop constant limit
//    iterInc      - loop iteration increment
//    iterOper     - loop iteration increment operator (ADD, SUB, etc.)
//    iterOperType - iteration operator type
//    testOper     - type of loop test (i.e. GT_LE, GT_GE, etc.)
//    unsTest      - true if test is unsigned
//    dupCond      - true if the loop head contains a test which skips this loop
//    iterCount    - *iterCount is set to the iteration count, if the function returns `true`
//
// Returns:
//   true if the loop has a constant repetition count, false if that cannot be proven
//
bool Compiler::optComputeLoopRep(int        constInit,
                                 int        constLimit,
                                 int        iterInc,
                                 genTreeOps iterOper,
                                 var_types  iterOperType,
                                 genTreeOps testOper,
                                 bool       unsTest,
                                 bool       dupCond,
                                 unsigned*  iterCount)
{
    noway_assert(genActualType(iterOperType) == TYP_INT);

    int64_t constInitX;
    int64_t constLimitX;

    unsigned loopCount;
    int      iterSign;

    // Using this, we can just do a signed comparison with other 32 bit values.
    if (unsTest)
    {
        constLimitX = static_cast<unsigned>(constLimit);
    }
    else
    {
        constLimitX = constLimit;
    }

#define INIT_ITER_BY_TYPE(type)                                                                                        \
    constInitX = (type)constInit;                                                                                      \
    iterInc    = (type)iterInc;

    switch (iterOperType)
    {
        case TYP_BYTE:
            INIT_ITER_BY_TYPE(signed char);
            break;
        case TYP_UBYTE:
            INIT_ITER_BY_TYPE(unsigned char);
            break;
        case TYP_SHORT:
            INIT_ITER_BY_TYPE(signed short);
            break;
        case TYP_USHORT:
            INIT_ITER_BY_TYPE(unsigned short);
            break;

        case TYP_INT:
        case TYP_UINT:
            if (unsTest)
            {
                constInitX = static_cast<unsigned>(constInit);
            }
            else
            {
                constInitX = constInit;
            }
            break;

        default:
            unreached();
    }

    // If iterInc is zero we have an infinite loop.
    if (iterInc == 0)
    {
        return false;
    }

    // Set iterSign to +1 for positive iterInc and -1 for negative iterInc.
    iterSign = (iterInc > 0) ? +1 : -1;

    // Initialize loopCount to zero.
    loopCount = 0;

    // If dupCond is true then the loop head contains a test which skips
    // this loop, if the constInit does not pass the loop test.
    // Such a loop can execute zero times.
    // If dupCond is false then we have a true do-while loop which we
    // always execute the loop once before performing the loop test
    if (!dupCond)
    {
        loopCount += 1;
        constInitX += iterInc;
    }

    // bail if count is based on wrap-around math
    if (iterInc > 0)
    {
        if (constLimitX < constInitX)
        {
            return false;
        }
    }
    else if (constLimitX > constInitX)
    {
        return false;
    }

    switch (testOper)
    {
        int64_t iterAtExitX;

        case GT_EQ:
            // Something like "for (i=init; i == lim; i++)" doesn't make any sense.
            return false;

        case GT_NE:
            // Consider: "for (i = init; i != lim; i += const)"
            // This is tricky since it may have a constant number of iterations or loop forever.
            // We have to compute "(lim - init) mod iterInc" to see if it is zero.
            // If "mod iterInc" is not zero then the limit test will miss and a wrap will occur
            // which is probably not what the end user wanted, but it is legal.

            if (iterInc > 0)
            {
                // Stepping by one, i.e. Mod with 1 is always zero.
                if (iterInc != 1)
                {
                    if (((constLimitX - constInitX) % iterInc) != 0)
                    {
                        return false;
                    }
                }
            }
            else
            {
                noway_assert(iterInc < 0);
                // Stepping by -1, i.e. Mod with 1 is always zero.
                if (iterInc != -1)
                {
                    if (((constInitX - constLimitX) % (-iterInc)) != 0)
                    {
                        return false;
                    }
                }
            }

            switch (iterOper)
            {
                case GT_SUB:
                    iterInc = -iterInc;
                    FALLTHROUGH;

                case GT_ADD:
                    if (constInitX != constLimitX)
                    {
                        loopCount += (unsigned)((constLimitX - constInitX - iterSign) / iterInc) + 1;
                    }

                    iterAtExitX = (int)(constInitX + iterInc * (int)loopCount);

                    if (unsTest)
                    {
                        iterAtExitX = (unsigned)iterAtExitX;
                    }

                    // Check if iteration incr will cause overflow for small types
                    if (optIterSmallOverflow((int)iterAtExitX, iterOperType))
                    {
                        return false;
                    }

                    // iterator with 32bit overflow. Bad for TYP_(U)INT
                    if (iterAtExitX < constLimitX)
                    {
                        return false;
                    }

                    *iterCount = loopCount;
                    return true;

                default:
                    unreached();
            }

        case GT_LT:
            switch (iterOper)
            {
                case GT_SUB:
                    iterInc = -iterInc;
                    FALLTHROUGH;

                case GT_ADD:
                    if (constInitX < constLimitX)
                    {
                        loopCount += (unsigned)((constLimitX - constInitX - iterSign) / iterInc) + 1;
                    }

                    iterAtExitX = (int)(constInitX + iterInc * (int)loopCount);

                    if (unsTest)
                    {
                        iterAtExitX = (unsigned)iterAtExitX;
                    }

                    // Check if iteration incr will cause overflow for small types
                    if (optIterSmallOverflow((int)iterAtExitX, iterOperType))
                    {
                        return false;
                    }

                    // iterator with 32bit overflow. Bad for TYP_(U)INT
                    if (iterAtExitX < constLimitX)
                    {
                        return false;
                    }

                    *iterCount = loopCount;
                    return true;

                default:
                    unreached();
            }

        case GT_LE:
            switch (iterOper)
            {
                case GT_SUB:
                    iterInc = -iterInc;
                    FALLTHROUGH;

                case GT_ADD:
                    if (constInitX <= constLimitX)
                    {
                        loopCount += (unsigned)((constLimitX - constInitX) / iterInc) + 1;
                    }

                    iterAtExitX = (int)(constInitX + iterInc * (int)loopCount);

                    if (unsTest)
                    {
                        iterAtExitX = (unsigned)iterAtExitX;
                    }

                    // Check if iteration incr will cause overflow for small types
                    if (optIterSmallOverflow((int)iterAtExitX, iterOperType))
                    {
                        return false;
                    }

                    // iterator with 32bit overflow. Bad for TYP_(U)INT
                    if (iterAtExitX <= constLimitX)
                    {
                        return false;
                    }

                    *iterCount = loopCount;
                    return true;

                default:
                    unreached();
            }

        case GT_GT:
            switch (iterOper)
            {
                case GT_SUB:
                    iterInc = -iterInc;
                    FALLTHROUGH;

                case GT_ADD:
                    if (constInitX > constLimitX)
                    {
                        loopCount += (unsigned)((constLimitX - constInitX - iterSign) / iterInc) + 1;
                    }

                    iterAtExitX = (int)(constInitX + iterInc * (int)loopCount);

                    if (unsTest)
                    {
                        iterAtExitX = (unsigned)iterAtExitX;
                    }

                    // Check if small types will underflow
                    if (optIterSmallUnderflow((int)iterAtExitX, iterOperType))
                    {
                        return false;
                    }

                    // iterator with 32bit underflow. Bad for TYP_INT and unsigneds
                    if (iterAtExitX > constLimitX)
                    {
                        return false;
                    }

                    *iterCount = loopCount;
                    return true;

                default:
                    unreached();
            }

        case GT_GE:
            switch (iterOper)
            {
                case GT_SUB:
                    iterInc = -iterInc;
                    FALLTHROUGH;

                case GT_ADD:
                    if (constInitX >= constLimitX)
                    {
                        loopCount += (unsigned)((constLimitX - constInitX) / iterInc) + 1;
                    }

                    iterAtExitX = (int)(constInitX + iterInc * (int)loopCount);

                    if (unsTest)
                    {
                        iterAtExitX = (unsigned)iterAtExitX;
                    }

                    // Check if small types will underflow
                    if (optIterSmallUnderflow((int)iterAtExitX, iterOperType))
                    {
                        return false;
                    }

                    // iterator with 32bit underflow. Bad for TYP_INT and unsigneds
                    if (iterAtExitX >= constLimitX)
                    {
                        return false;
                    }

                    *iterCount = loopCount;
                    return true;

                default:
                    unreached();
            }

        default:
            unreached();
    }
}

#ifdef _PREFAST_
#pragma warning(push)
#pragma warning(disable : 21000) // Suppress PREFast warning about overly large function
#endif

// Look for loop unrolling candidates and unroll them.
//
// Loops must be of the form:
//   for (i=icon; i<icon; i++) { ... }
//
// Loops handled are fully unrolled; there is no partial unrolling.
//
// Limitations: only the following loop types are handled:
// 1. "while" loops
// 2. constant bound loops
//
// Cost heuristics:
// 1. there are cost metrics for maximum number of allowed iterations, and maximum unroll size
// 2. single-iteration loops are always allowed (to eliminate the loop structure).
// 3. otherwise, only loops where the limit is Vector<T>.Length are currently allowed
//
// In stress modes, these heuristic limits are expanded, and loops aren't required to have the
// Vector<T>.Length limit.
//
// Returns:
//   suitable phase status
//
PhaseStatus Compiler::phUnrollLoops()
{
    if (compCodeOpt() == SMALL_CODE)
    {
        return PhaseStatus::MODIFIED_NOTHING;
    }

    if (optLoopCount == 0)
    {
        return PhaseStatus::MODIFIED_NOTHING;
    }

#ifdef DEBUG
    if (JitConfig.JitNoUnroll())
    {
        return PhaseStatus::MODIFIED_NOTHING;
    }
#endif

    static const unsigned ITER_LIMIT[COUNT_OPT_CODE]{
        10, // BLENDED_CODE
        0,  // SMALL_CODE
        20  // FAST_CODE
    };

    assert(ITER_LIMIT[BLENDED_CODE] == 10);

    static const int UNROLL_LIMIT_SZ[COUNT_OPT_CODE]{
        300, // BLENDED_CODE
        0,   // SMALL_CODE
        600  // FAST_CODE
    };

    assert(UNROLL_LIMIT_SZ[BLENDED_CODE] == 300);

    unsigned iterLimit = ITER_LIMIT[compCodeOpt()];

#ifdef DEBUG
    if (compStressCompile(STRESS_UNROLL_LOOPS, 50))
    {
        iterLimit *= 10;
    }
#endif

    bool change = false;

    // Visit loops from highest to lowest number to visit them in innermost to outermost order.
    for (unsigned lnum = optLoopCount - 1; lnum != UINT_MAX; --lnum)
    {
        const unsigned loopFlags = optLoopTable[lnum].lpFlags;

        // Check for required flags:
        // LPFLG_DO_WHILE - required because this transform only handles loops of this form
        // LPFLG_CONST_INIT & LPFLG_CONST_LIMIT - required because this transform only handles full unrolls
        const unsigned requiredFlags = LPFLG_DO_WHILE | LPFLG_CONST_INIT | LPFLG_CONST_LIMIT;

        // Ignore the loop if we don't have a do-while that has a constant number of iterations.

        if ((loopFlags & requiredFlags) != requiredFlags)
        {
            continue;
        }

        if ((loopFlags & (LPFLG_DONT_UNROLL | LPFLG_REMOVED)) != 0)
        {
            continue;
        }

        BasicBlock* const head = optLoopTable[lnum].lpHead;
        noway_assert(head);
        BasicBlock* const bottom = optLoopTable[lnum].lpBottom;
        noway_assert(bottom);
        const int        lbeg         = optLoopTable[lnum].lpConstInit;
        const int        llim         = optLoopTable[lnum].lpConstLimit();
        const genTreeOps testOper     = optLoopTable[lnum].lpTestOper();
        const unsigned   lvar         = optLoopTable[lnum].lpIterVar();
        const int        iterInc      = optLoopTable[lnum].lpIterConst();
        const genTreeOps iterOper     = optLoopTable[lnum].lpIterOper();
        const var_types  iterOperType = optLoopTable[lnum].lpIterOperType();
        const bool       unsTest      = optLoopTable[lnum].lpTestTree->IsUnsigned();

        if (lvaGetDesc(lvar)->IsAddressExposed())
        {
            continue;
        }

        if (lvaGetDesc(lvar)->IsPromotedField())
        {
            continue;
        }

        // Locate/initialize the increment/test statements.
        Statement* initStmt = head->lastStmt();
        noway_assert((initStmt != nullptr) && (initStmt->GetNextStmt() == nullptr));

        Statement* testStmt = bottom->lastStmt();
        noway_assert((testStmt != nullptr) && (testStmt->GetNextStmt() == nullptr));

        Statement* incrStmt = testStmt->GetPrevStmt();
        noway_assert(incrStmt != nullptr);

        bool dupCond;

        if (initStmt->IsCompilerAdded())
        {
            // Must be a duplicated loop condition.
            noway_assert(initStmt->GetRootNode()->gtOper == GT_JTRUE);

            dupCond  = true;
            initStmt = initStmt->GetPrevStmt();
            noway_assert(initStmt != nullptr);
        }
        else
        {
            dupCond = false;
        }

        unsigned totalIter; // total number of iterations in the constant loop

        if (!optComputeLoopRep(lbeg, llim, iterInc, iterOper, iterOperType, testOper, unsTest, dupCond, &totalIter))
        {
            continue;
        }

        // Forget it if there are too many repetitions or not a constant loop.

        if (totalIter > iterLimit)
        {
            continue;
        }

        int unrollLimitSz = UNROLL_LIMIT_SZ[compCodeOpt()];

        if (INDEBUG(compStressCompile(STRESS_UNROLL_LOOPS, 50) ||) false)
        {
            // In stress mode, quadruple the size limit, and drop
            // the restriction that loop limit must be vector element count.
            unrollLimitSz *= 4;
        }
        else if (totalIter <= 1)
        {
            // No limit for single iteration loops
            unrollLimitSz = INT_MAX;
        }
        else if ((loopFlags & LPFLG_SIMD_LIMIT) == 0)
        {
            // Otherwise unroll only if limit is Vector_.Length
            // (as a heuristic, not for correctness/structural reasons)
            continue;
        }

        GenTree* incr = incrStmt->GetRootNode();

        // Don't unroll loops we don't understand.
        if (incr->gtOper != GT_ASG)
        {
            continue;
        }
        incr = incr->AsOp()->gtOp2;

        GenTree* init = initStmt->GetRootNode();

        // Make sure everything looks ok.
        // clang-format off
        if ((init->gtOper != GT_ASG) ||
            (init->AsOp()->gtOp1->gtOper != GT_LCL_VAR) ||
            (init->AsOp()->gtOp1->AsLclVar()->GetLclNum() != lvar) ||
            (init->AsOp()->gtOp2->gtOper != GT_CNS_INT) ||
            (init->AsOp()->gtOp2->AsIntCon()->gtIconVal != lbeg) ||

            !((incr->gtOper == GT_ADD) || (incr->gtOper == GT_SUB)) ||
            (incr->AsOp()->gtOp1->gtOper != GT_LCL_VAR) ||
            (incr->AsOp()->gtOp1->AsLclVar()->GetLclNum() != lvar) ||
            (incr->AsOp()->gtOp2->gtOper != GT_CNS_INT) ||
            (incr->AsOp()->gtOp2->AsIntCon()->gtIconVal != iterInc) ||

            (testStmt->GetRootNode()->gtOper != GT_JTRUE))
        {
            noway_assert(!"Bad precondition in Compiler::optUnrollLoops()");
            continue;
        }
        // clang-format on

        // Heuristic: Estimated cost in code size of the unrolled loop.

        unsigned loopRetCount = 0; // number of BBJ_RETURN blocks in loop

        {
            ClrSafeInt<unsigned> loopCostSz; // Cost is size of one iteration

            BasicBlock* block    = head->bbNext;
            uint16_t    tryIndex = block->bbTryIndex;
            bool        hasEH    = false;

            for (;; block = block->bbNext)
            {
                if (block->bbTryIndex != tryIndex)
                {
                    // Unrolling would require cloning EH regions
                    hasEH = true;
                    break;
                }

                if (block->bbJumpKind == BBJ_RETURN)
                {
                    ++loopRetCount;
                }

                for (Statement* const stmt : block->Statements())
                {
                    gtSetStmtInfo(stmt);
                    loopCostSz += stmt->GetCostSz();
                }

                if (block == bottom)
                {
                    break;
                }
            }

            if (hasEH)
            {
                continue;
            }

#ifdef JIT32_GCENCODER
            if (fgReturnCount + loopRetCount * (totalIter - 1) > SET_EPILOGCNT_MAX)
            {
                // Jit32 GC encoder can't report more than SET_EPILOGCNT_MAX epilogs.
                continue;
            }
#endif // !JIT32_GCENCODER

            // Compute the estimated increase in code size for the unrolled loop.

            ClrSafeInt<unsigned> fixedLoopCostSz(8);

            ClrSafeInt<int> unrollCostSz = ClrSafeInt<int>(loopCostSz * ClrSafeInt<unsigned>(totalIter)) -
                                           ClrSafeInt<int>(loopCostSz + fixedLoopCostSz);

            // Don't unroll if too much code duplication would result.

            if (unrollCostSz.IsOverflow() || (unrollCostSz.Value() > unrollLimitSz))
            {
                continue;
            }

#ifdef DEBUG
            if (verbose)
            {
                printf("\nUnrolling loop " FMT_BB, head->bbNext->bbNum);
                if (head->bbNext->bbNum != bottom->bbNum)
                {
                    printf(".." FMT_BB, bottom->bbNum);
                }
                printf(" over V%02u from %u to %u unrollCostSz = %d\n\n", lvar, lbeg, llim, unrollCostSz);
            }
#endif
        }

#if FEATURE_LOOP_ALIGN
        for (BasicBlock* block = head->bbNext;; block = block->bbNext)
        {
            if (block->isLoopAlign())
            {
                block->bbFlags &= ~BBF_LOOP_ALIGN;
                JITDUMP("Removing LOOP_ALIGN flag from unrolled loop in " FMT_BB "\n", block->bbNum);
            }

            if (block == bottom)
            {
                break;
            }
        }
#endif

        // Create the unrolled loop statement list.
        {
            BlockToBlockMap blockMap(getAllocator(CMK_LoopUnroll));
            BasicBlock*     insertAfter = bottom;

            for (int lval = lbeg; totalIter; totalIter--)
            {
                for (BasicBlock* block = head->bbNext;; block = block->bbNext)
                {
                    BasicBlock* newBlock = insertAfter =
                        fgNewBBafter(block->bbJumpKind, insertAfter, /*extendRegion*/ true);
                    blockMap.Set(block, newBlock, BlockToBlockMap::Overwrite);

                    if (!BasicBlock::CloneBlockState(this, newBlock, block, lvar, lval))
                    {
                        // cloneExpr doesn't handle everything
                        BasicBlock* oldBottomNext = insertAfter->bbNext;
                        bottom->bbNext            = oldBottomNext;
                        oldBottomNext->bbPrev     = bottom;
                        optLoopTable[lnum].lpFlags |= LPFLG_DONT_UNROLL;
                        goto DONE_LOOP;
                    }

                    // Block weight should no longer have the loop multiplier
                    //
                    // Note this is not quite right, as we may not have upscaled by this amount
                    // and we might not have upscaled at all, if we had profile data.
                    //
                    newBlock->scaleBBWeight(1.0f / BB_LOOP_WEIGHT_SCALE);

                    // Jump dests are set in a post-pass; make sure CloneBlockState hasn't tried to set them.
                    assert(newBlock->bbJumpDest == nullptr);

                    if (block == bottom)
                    {
                        // Remove the test; we're doing a full unroll.

                        Statement* testCopyStmt = newBlock->lastStmt();
                        GenTree*   testCopyExpr = testCopyStmt->GetRootNode();
                        assert(testCopyExpr->OperIs(GT_JTRUE));

                        GenTree* sideEffList = gtExtractSideEffList(testCopyExpr, GTF_SIDE_EFFECT | GTF_ORDER_SIDEEFF);
                        if (sideEffList == nullptr)
                        {
                            fgRemoveStmt(newBlock, testCopyStmt);
                        }
                        else
                        {
                            testCopyStmt->SetRootNode(sideEffList);
                        }
                        newBlock->bbJumpKind = BBJ_NONE;

                        // Exit this loop; we've walked all the blocks.
                        break;
                    }
                }

                // Now redirect any branches within the newly-cloned iteration
                for (BasicBlock* block = head->bbNext; block != bottom; block = block->bbNext)
                {
                    BasicBlock* newBlock = blockMap[block];
                    optCopyBlkDest(block, newBlock);
                    optRedirectBlock(newBlock, &blockMap);
                }

                /* update the new value for the unrolled iterator */

                switch (iterOper)
                {
                    case GT_ADD:
                        lval += iterInc;
                        break;
                    case GT_SUB:
                        lval -= iterInc;
                        break;
                    default:
                        unreached();
                }
            }

            // Remove the old loop body
            for (BasicBlock* block = head->bbNext;; block = block->bbNext)
            {
                block->bbStmtList = nullptr;
                block->bbJumpKind = BBJ_NONE;
                block->bbFlags &= ~BBF_LOOP_HEAD;
                if (block->bbJumpDest != nullptr)
                {
                    block->bbJumpDest = nullptr;
                }

                if (block == bottom)
                {
                    break;
                }
            }

            // If the HEAD is a BBJ_COND drop the condition (and make HEAD a BBJ_NONE block).

            if (head->bbJumpKind == BBJ_COND)
            {
                Statement* preHeaderStmt = head->firstStmt();
                noway_assert(preHeaderStmt != nullptr);

                testStmt = preHeaderStmt->GetPrevStmt();
                noway_assert((testStmt != nullptr) && (testStmt->GetNextStmt() == nullptr));
                noway_assert(testStmt->GetRootNode()->gtOper == GT_JTRUE);

                initStmt = testStmt->GetPrevStmt();
                noway_assert((initStmt != nullptr) && (initStmt->GetNextStmt() == testStmt));

                initStmt->SetNextStmt(nullptr);
                preHeaderStmt->SetPrevStmt(initStmt);
                head->bbJumpKind = BBJ_NONE;
            }
            else
            {
                /* the loop must execute */
                noway_assert(head->bbJumpKind == BBJ_NONE);
            }

#ifdef DEBUG
            if (verbose)
            {
                printf("Whole unrolled loop:\n");

                gtDispTree(initStmt->GetRootNode());
                printf("\n");
                fgDumpTrees(head->bbNext, insertAfter);
            }
#endif

            // Remember that something has changed.

            change = true;

            // Make sure to update loop table.

            // Mark the loop as removed. Make head and bottom nullptr to make it likelier for downstream
            // phases that don't properly check the LPFLG_REMOVED flag to hit an assert or an access violation.

            optLoopTable[lnum].lpFlags |= LPFLG_REMOVED;
            optLoopTable[lnum].lpHead = optLoopTable[lnum].lpBottom = nullptr;

            // Note if we created new BBJ_RETURNs
            fgReturnCount += loopRetCount * (totalIter - 1);
        }

    DONE_LOOP:;
    }

    if (change)
    {
        fgUpdateChangedFlowGraph(/*computePreds*/ true);
        fgComputeDoms();
    }

    INDEBUG(fgDebugCheckBBlist(true));

    return change ? PhaseStatus::MODIFIED_EVERYTHING : PhaseStatus::MODIFIED_NOTHING;
}
#ifdef _PREFAST_
#pragma warning(pop)
#endif

// static
Compiler::fgWalkResult Compiler::optInvertCountTreeInfo(GenTree** pTree, fgWalkData* data)
{
    OptInvertCountTreeInfoType* o = (OptInvertCountTreeInfoType*)data->pCallbackData;

    if (Compiler::IsSharedStaticHelper(*pTree))
    {
        o->sharedStaticHelperCount += 1;
    }

    if ((*pTree)->OperGet() == GT_ARR_LENGTH)
    {
        o->arrayLengthCount += 1;
    }

    return WALK_CONTINUE;
}

//-----------------------------------------------------------------------------
// optInvertWhileLoop: modify flow and duplicate code so that for/while loops are
//   entered at top and tested at bottom (aka loop rotation or bottom testing).
//   Creates a "zero trip test" condition which guards entry to the loop.
//   Enables loop invariant hoisting and loop cloning, which depend on
//   `do {} while` format loops. Enables creation of a pre-header block after the
//   zero trip test to place code that only runs if the loop is guaranteed to
//   run at least once.
//
// Arguments:
//   block -- block that may be the predecessor of the un-rotated loop's test block.
//
// Returns:
//   true if any IR changes possibly made (used to determine phase return status)
//
// Notes:
//   Uses a simple lexical screen to detect likely loops.
//
//   Specifically, we're looking for the following case:
//
//          ...
//          jmp test                // `block` argument
//   loop:
//          ...
//          ...
//   test:
//          ..stmts..
//          cond
//          jtrue loop
//
//   If we find this, and the condition is simple enough, we change
//   the loop to the following:
//
//          ...
//          ..stmts..               // duplicated cond block statments
//          cond                    // duplicated cond
//          jfalse done
//          // else fall-through
//   loop:
//          ...
//          ...
//   test:
//          ..stmts..
//          cond
//          jtrue loop
//   done:
//
//  Makes no changes if the flow pattern match fails.
//
//  May not modify a loop if profile is unfavorable, if the cost of duplicating
//  code is large (factoring in potential CSEs).
//
bool Compiler::optInvertWhileLoop(BasicBlock* block)
{
    assert(opts.OptimizationEnabled());
    assert(compCodeOpt() != SMALL_CODE);

    // Does the BB end with an unconditional jump?

    if (block->bbJumpKind != BBJ_ALWAYS || (block->bbFlags & BBF_KEEP_BBJ_ALWAYS))
    {
        // It can't be one of the ones we use for our exception magic
        return false;
    }

    // Get hold of the jump target
    BasicBlock* bTest = block->bbJumpDest;

    // Does the block consist of 'jtrue(cond) block' ?
    if (bTest->bbJumpKind != BBJ_COND)
    {
        return false;
    }

    // bTest must be a backwards jump to block->bbNext
    if (bTest->bbJumpDest != block->bbNext)
    {
        return false;
    }

    // Since test is a BBJ_COND it will have a bbNext
    noway_assert(bTest->bbNext != nullptr);

    // 'block' must be in the same try region as the condition, since we're going to insert a duplicated condition
    // in a new block after 'block', and the condition might include exception throwing code.
    // On non-funclet platforms (x86), the catch exit is a BBJ_ALWAYS, but we don't want that to
    // be considered as the head of a loop, so also disallow different handler regions.
    if (!BasicBlock::sameEHRegion(block, bTest))
    {
        return false;
    }

    // The duplicated condition block will branch to bTest->bbNext, so that also better be in the
    // same try region (or no try region) to avoid generating illegal flow.
    BasicBlock* bTestNext = bTest->bbNext;
    if (bTestNext->hasTryIndex() && !BasicBlock::sameTryRegion(block, bTestNext))
    {
        return false;
    }

    // It has to be a forward jump. Defer this check until after all the cheap checks
    // are done, since it iterates forward in the block list looking for bbJumpDest.
    //  TODO-CQ: Check if we can also optimize the backwards jump as well.
    //
    if (!fgIsForwardBranch(block))
    {
        return false;
    }

    // Find the loop termination test at the bottom of the loop.
    Statement* condStmt = bTest->lastStmt();

    // Verify the test block ends with a conditional that we can manipulate.
    GenTree* const condTree = condStmt->GetRootNode();
    noway_assert(condTree->gtOper == GT_JTRUE);
    if (!condTree->AsOp()->gtOp1->OperIsCompare())
    {
        return false;
    }

    // Estimate the cost of cloning the entire test block.
    //
    // Note: it would help throughput to compute the maximum cost
    // first and early out for large bTest blocks, as we are doing two
    // tree walks per tree. But because of this helper call scan, the
    // maximum cost depends on the trees in the block.
    //
    // We might consider flagging blocks with hoistable helper calls
    // during importation, so we can avoid the helper search and
    // implement an early bail out for large blocks with no helper calls.
    //
    // Note that gtPrepareCost can cause operand swapping, so we must
    // return `true` (possible IR change) from here on.

    unsigned estDupCostSz = 0;

    for (Statement* const stmt : bTest->Statements())
    {
        GenTree* tree = stmt->GetRootNode();
        gtPrepareCost(tree);
        estDupCostSz += tree->GetCostSz();
    }

    BasicBlock::weight_t       loopIterations            = BB_LOOP_WEIGHT_SCALE;
    bool                       allProfileWeightsAreValid = false;
    BasicBlock::weight_t const weightBlock               = block->bbWeight;
    BasicBlock::weight_t const weightTest                = bTest->bbWeight;
    BasicBlock::weight_t const weightNext                = block->bbNext->bbWeight;

    // If we have profile data then we calculate the number of times
    // the loop will iterate into loopIterations
    if (fgIsUsingProfileWeights())
    {
        // Only rely upon the profile weight when all three of these blocks
        // have good profile weights
        if (block->hasProfileWeight() && bTest->hasProfileWeight() && block->bbNext->hasProfileWeight())
        {
            // If this while loop never iterates then don't bother transforming
            //
            if (weightNext == BB_ZERO_WEIGHT)
            {
                return true;
            }

            // We generally expect weightTest == weightNext + weightBlock.
            //
            // Tolerate small inconsistencies...
            //
            if (!fgProfileWeightsConsistent(weightBlock + weightNext, weightTest))
            {
                JITDUMP("Profile weights locally inconsistent: block " FMT_WT ", next " FMT_WT ", test " FMT_WT "\n",
                        weightBlock, weightNext, weightTest);
            }
            else
            {
                allProfileWeightsAreValid = true;

                // Determine iteration count
                //
                //   weightNext is the number of time this loop iterates
                //   weightBlock is the number of times that we enter the while loop
                //   loopIterations is the average number of times that this loop iterates
                //
                loopIterations = weightNext / weightBlock;
            }
        }
        else
        {
            JITDUMP("Missing profile data for loop!\n");
        }
    }

    unsigned maxDupCostSz = 34;

    if ((compCodeOpt() == FAST_CODE) || compStressCompile(STRESS_DO_WHILE_LOOPS, 30))
    {
        maxDupCostSz *= 4;
    }

    // If this loop iterates a lot then raise the maxDupCost
    if (loopIterations >= 12.0)
    {
        maxDupCostSz *= 2;
        if (loopIterations >= 96.0)
        {
            maxDupCostSz *= 2;
        }
    }

    // If the compare has too high cost then we don't want to dup.

    bool costIsTooHigh = (estDupCostSz > maxDupCostSz);

    OptInvertCountTreeInfoType optInvertTotalInfo = {};
    if (costIsTooHigh)
    {
        // If we already know that the cost is acceptable, then don't waste time walking the tree
        // counting things to boost the maximum allowed cost.
        //
        // If the loop condition has a shared static helper, we really want this loop converted
        // as not converting the loop will disable loop hoisting, meaning the shared helper will
        // be executed on every loop iteration.
        //
        // If the condition has array.Length operations, also boost, as they are likely to be CSE'd.

        for (Statement* const stmt : bTest->Statements())
        {
            GenTree* tree = stmt->GetRootNode();

            OptInvertCountTreeInfoType optInvertInfo = {};
            fgWalkTreePre(&tree, Compiler::optInvertCountTreeInfo, &optInvertInfo);
            optInvertTotalInfo.sharedStaticHelperCount += optInvertInfo.sharedStaticHelperCount;
            optInvertTotalInfo.arrayLengthCount += optInvertInfo.arrayLengthCount;

            if ((optInvertInfo.sharedStaticHelperCount > 0) || (optInvertInfo.arrayLengthCount > 0))
            {
                // Calculate a new maximum cost. We might be able to early exit.

                unsigned newMaxDupCostSz =
                    maxDupCostSz + 24 * min(optInvertTotalInfo.sharedStaticHelperCount, (int)(loopIterations + 1.5)) +
                    8 * optInvertTotalInfo.arrayLengthCount;

                // Is the cost too high now?
                costIsTooHigh = (estDupCostSz > newMaxDupCostSz);
                if (!costIsTooHigh)
                {
                    // No need counting any more trees; we're going to do the transformation.
                    JITDUMP("Decided to duplicate loop condition block after counting helpers in tree [%06u] in "
                            "block " FMT_BB,
                            dspTreeID(tree), bTest->bbNum);
                    maxDupCostSz = newMaxDupCostSz; // for the JitDump output below
                    break;
                }
            }
        }
    }

#ifdef DEBUG
    if (verbose)
    {
        // Note that `optInvertTotalInfo.sharedStaticHelperCount = 0` means either there were zero helpers, or the
        // tree walk to count them was not done.
        printf(
            "\nDuplication of loop condition [%06u] is %s, because the cost of duplication (%i) is %s than %i,"
            "\n   loopIterations = %7.3f, optInvertTotalInfo.sharedStaticHelperCount >= %d, validProfileWeights = %s\n",
            dspTreeID(condTree), costIsTooHigh ? "not done" : "performed", estDupCostSz,
            costIsTooHigh ? "greater" : "less or equal", maxDupCostSz, loopIterations,
            optInvertTotalInfo.sharedStaticHelperCount, dspBool(allProfileWeightsAreValid));
    }
#endif

    if (costIsTooHigh)
    {
        return true;
    }

    bool foundCondTree = false;

    // Create a new block after `block` to put the copied condition code.
    block->bbJumpKind    = BBJ_NONE;
    block->bbJumpDest    = nullptr;
    BasicBlock* bNewCond = fgNewBBafter(BBJ_COND, block, /*extendRegion*/ true);

    // Clone each statement in bTest and append to bNewCond.
    for (Statement* const stmt : bTest->Statements())
    {
        GenTree* originalTree = stmt->GetRootNode();
        GenTree* clonedTree   = gtCloneExpr(originalTree);

        // Special case handling needed for the conditional jump tree
        if (originalTree == condTree)
        {
            foundCondTree = true;

            // Get the compare subtrees
            GenTree* originalCompareTree = originalTree->AsOp()->gtOp1;
            GenTree* clonedCompareTree   = clonedTree->AsOp()->gtOp1;
            assert(originalCompareTree->OperIsCompare());
            assert(clonedCompareTree->OperIsCompare());

            // Flag compare and cloned copy so later we know this loop
            // has a proper zero trip test.
            originalCompareTree->gtFlags |= GTF_RELOP_ZTT;
            clonedCompareTree->gtFlags |= GTF_RELOP_ZTT;

            // The original test branches to remain in the loop.  The
            // new cloned test will branch to avoid the loop.  So the
            // cloned compare needs to reverse the branch condition.
            gtReverseCond(clonedCompareTree);
        }

        Statement* clonedStmt = fgNewStmtAtEnd(bNewCond, clonedTree);

        if (opts.compDbgInfo)
        {
            clonedStmt->SetILOffsetX(stmt->GetILOffsetX());
        }

        clonedStmt->SetCompilerAdded();
    }

    assert(foundCondTree);

    // Flag the block that received the copy as potentially having an array/vtable
    // reference, nullcheck, object/array allocation if the block copied from did;
    // this is a conservative guess.
    if (auto copyFlags = bTest->bbFlags & BBF_IR_SUMMARY)
    {
        bNewCond->bbFlags |= copyFlags;
    }

    bNewCond->bbJumpDest = bTest->bbNext;
    bNewCond->inheritWeight(block);

    // Update bbRefs and bbPreds for 'bNewCond', 'bNewCond->bbNext' 'bTest' and 'bTest->bbNext'.

    fgAddRefPred(bNewCond, block);
    fgAddRefPred(bNewCond->bbNext, bNewCond);

    fgRemoveRefPred(bTest, block);
    fgAddRefPred(bTest->bbNext, bNewCond);

    // Move all predecessor edges that look like loop entry edges to point to the new cloned condition
    // block, not the existing condition block. The idea is that if we only move `block` to point to
    // `bNewCond`, but leave other `bTest` predecessors still pointing to `bTest`, when we eventually
    // recognize loops, the loop will appear to have multiple entries, which will prevent optimization.
    // We don't have loops yet, but blocks should be in increasing lexical numbered order, so use that
    // as the proxy for predecessors that are "in" versus "out" of the potential loop. Note that correctness
    // is maintained no matter which condition block we point to, but we'll lose optimization potential
    // (and create spaghetti code) if we get it wrong.

    BlockToBlockMap blockMap(getAllocator(CMK_LoopOpt));
    bool            blockMapInitialized = false;

    unsigned loopFirstNum  = bNewCond->bbNext->bbNum;
    unsigned loopBottomNum = bTest->bbNum;
    for (BasicBlock* const predBlock : bTest->PredBlocks())
    {
        unsigned bNum = predBlock->bbNum;
        if ((loopFirstNum <= bNum) && (bNum <= loopBottomNum))
        {
            // Looks like the predecessor is from within the potential loop; skip it.
            continue;
        }

        if (!blockMapInitialized)
        {
            blockMapInitialized = true;
            blockMap.Set(bTest, bNewCond);
        }

        // Redirect the predecessor to the new block.
        JITDUMP("Redirecting non-loop " FMT_BB " -> " FMT_BB " to " FMT_BB " -> " FMT_BB "\n", predBlock->bbNum,
                bTest->bbNum, predBlock->bbNum, bNewCond->bbNum);
        optRedirectBlock(predBlock, &blockMap, /*updatePreds*/ true);
    }

    // If we have profile data for all blocks and we know that we are cloning the
    // `bTest` block into `bNewCond` and thus changing the control flow from `block` so
    // that it no longer goes directly to `bTest` anymore, we have to adjust
    // various weights.
    //
    if (allProfileWeightsAreValid)
    {
        // Update the weight for bTest
        //
        JITDUMP("Reducing profile weight of " FMT_BB " from " FMT_WT " to " FMT_WT "\n", bTest->bbNum, weightTest,
                weightNext);
        bTest->bbWeight = weightNext;

        // Determine the new edge weights.
        //
        // We project the next/jump ratio for block and bTest by using
        // the original likelihoods out of bTest.
        //
        // Note "next" is the loop top block, not bTest's bbNext,
        // we'll call this latter block "after".
        //
        BasicBlock::weight_t const testToNextLikelihood  = min(1.0f, weightNext / weightTest);
        BasicBlock::weight_t const testToAfterLikelihood = 1.0f - testToNextLikelihood;

        // Adjust edges out of bTest (which now has weight weightNext)
        //
        BasicBlock::weight_t const testToNextWeight  = weightNext * testToNextLikelihood;
        BasicBlock::weight_t const testToAfterWeight = weightNext * testToAfterLikelihood;

        flowList* const edgeTestToNext  = fgGetPredForBlock(bTest->bbJumpDest, bTest);
        flowList* const edgeTestToAfter = fgGetPredForBlock(bTest->bbNext, bTest);

        JITDUMP("Setting weight of " FMT_BB " -> " FMT_BB " to " FMT_WT " (iterate loop)\n", bTest->bbNum,
                bTest->bbJumpDest->bbNum, testToNextWeight);
        JITDUMP("Setting weight of " FMT_BB " -> " FMT_BB " to " FMT_WT " (exit loop)\n", bTest->bbNum,
                bTest->bbNext->bbNum, testToAfterWeight);

        edgeTestToNext->setEdgeWeights(testToNextWeight, testToNextWeight, bTest->bbJumpDest);
        edgeTestToAfter->setEdgeWeights(testToAfterWeight, testToAfterWeight, bTest->bbNext);

        // Adjust edges out of block, using the same distribution.
        //
        JITDUMP("Profile weight of " FMT_BB " remains unchanged at " FMT_WT "\n", block->bbNum, weightBlock);

        BasicBlock::weight_t const blockToNextLikelihood  = testToNextLikelihood;
        BasicBlock::weight_t const blockToAfterLikelihood = testToAfterLikelihood;

        BasicBlock::weight_t const blockToNextWeight  = weightBlock * blockToNextLikelihood;
        BasicBlock::weight_t const blockToAfterWeight = weightBlock * blockToAfterLikelihood;

        flowList* const edgeBlockToNext  = fgGetPredForBlock(bNewCond->bbNext, bNewCond);
        flowList* const edgeBlockToAfter = fgGetPredForBlock(bNewCond->bbJumpDest, bNewCond);

        JITDUMP("Setting weight of " FMT_BB " -> " FMT_BB " to " FMT_WT " (enter loop)\n", bNewCond->bbNum,
                bNewCond->bbNext->bbNum, blockToNextWeight);
        JITDUMP("Setting weight of " FMT_BB " -> " FMT_BB " to " FMT_WT " (avoid loop)\n", bNewCond->bbNum,
                bNewCond->bbJumpDest->bbNum, blockToAfterWeight);

        edgeBlockToNext->setEdgeWeights(blockToNextWeight, blockToNextWeight, bNewCond->bbNext);
        edgeBlockToAfter->setEdgeWeights(blockToAfterWeight, blockToAfterWeight, bNewCond->bbJumpDest);

#ifdef DEBUG
        // Verify profile for the two target blocks is consistent.
        //
        fgDebugCheckIncomingProfileData(bNewCond->bbNext);
        fgDebugCheckIncomingProfileData(bNewCond->bbJumpDest);
#endif // DEBUG
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("\nDuplicated loop exit block at " FMT_BB " for loop (" FMT_BB " - " FMT_BB ")\n", bNewCond->bbNum,
               bNewCond->bbNext->bbNum, bTest->bbNum);
        printf("Estimated code size expansion is %d\n", estDupCostSz);

        fgDumpBlock(bNewCond);
        fgDumpBlock(bTest);
    }
#endif // DEBUG

    return true;
}

//-----------------------------------------------------------------------------
// optInvertLoops: invert while loops in the method
//
// Returns:
//   suitable phase status
//
PhaseStatus Compiler::optInvertLoops()
{
    noway_assert(opts.OptimizationEnabled());
    noway_assert(fgModified == false);

#if defined(OPT_CONFIG)
    if (!JitConfig.JitDoLoopInversion())
    {
        JITDUMP("Loop inversion disabled\n");
        return PhaseStatus::MODIFIED_NOTHING;
    }
#endif // OPT_CONFIG

    if (compCodeOpt() == SMALL_CODE)
    {
        return PhaseStatus::MODIFIED_NOTHING;
    }

    bool madeChanges = false; // Assume no changes made
    for (BasicBlock* const block : Blocks())
    {
        // Make sure the appropriate fields are initialized
        //
        if (block->bbWeight == BB_ZERO_WEIGHT)
        {
            // Zero weighted block can't have a LOOP_HEAD flag
            noway_assert(block->isLoopHead() == false);
            continue;
        }

        if (optInvertWhileLoop(block))
        {
            madeChanges = true;
        }
    }

    if (fgModified)
    {
        // Reset fgModified here as we've done a consistent set of edits.
        //
        fgModified = false;
    }

    return madeChanges ? PhaseStatus::MODIFIED_EVERYTHING : PhaseStatus::MODIFIED_NOTHING;
}

//-----------------------------------------------------------------------------
// optOptimizeLayout: reorder blocks to reduce cost of control flow
//
// Returns:
//   suitable phase status
//
PhaseStatus Compiler::optOptimizeLayout()
{
    noway_assert(opts.OptimizationEnabled());
    noway_assert(fgModified == false);

    bool madeChanges = false;

    madeChanges |= fgUpdateFlowGraph(nullptr, /* doTailDup */ true);
    madeChanges |= fgReorderBlocks();
    madeChanges |= fgUpdateFlowGraph();

    // fgReorderBlocks can cause IR changes even if it does not modify
    // the flow graph. It calls gtPrepareCost which can cause operand swapping.
    // Work around this for now.
    //
    // Note phase status only impacts dumping and checking done post-phase,
    // it has no impact on a release build.
    //
    madeChanges = true;

    return madeChanges ? PhaseStatus::MODIFIED_EVERYTHING : PhaseStatus::MODIFIED_NOTHING;
}

// Find and classify natural loops
//
// Notes:
//  Also (re)sets all non-IBC block weights, and marks loops potentially needing
//  alignment padding.
//
PhaseStatus Compiler::phFindLoops()
{
    noway_assert(opts.OptimizationEnabled());

#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In optFindLoops()\n");
    }
#endif

    optSetBlockWeights();

    /* Were there any loops in the flow graph? */

    if (fgHasLoops)
    {
        /* now that we have dominator information we can find loops */

        optFindNaturalLoops();

        unsigned loopNum = 0;

        /* Iterate over the flow graph, marking all loops */

        /* We will use the following terminology:
         * top        - the first basic block in the loop (i.e. the head of the backward edge)
         * bottom     - the last block in the loop (i.e. the block from which we jump to the top)
         * lastBottom - used when we have multiple back-edges to the same top
         */

        for (BasicBlock* const top : Blocks())
        {
            BasicBlock* foundBottom = nullptr;

            for (BasicBlock* const bottom : top->PredBlocks())
            {
                /* Is this a loop candidate? - We look for "back edges" */

                /* is this a backward edge? (from BOTTOM to TOP) */

                if (top->bbNum > bottom->bbNum)
                {
                    continue;
                }

                /* 'top' also must have the BBF_LOOP_HEAD flag set */

                if (top->isLoopHead() == false)
                {
                    continue;
                }

                /* We only consider back-edges that are BBJ_COND or BBJ_ALWAYS for loops */

                if ((bottom->bbJumpKind != BBJ_COND) && (bottom->bbJumpKind != BBJ_ALWAYS))
                {
                    continue;
                }

                /* the top block must be able to reach the bottom block */
                if (!fgReachable(top, bottom))
                {
                    continue;
                }

                /* Found a new loop, record the longest backedge in foundBottom */

                if ((foundBottom == nullptr) || (bottom->bbNum > foundBottom->bbNum))
                {
                    foundBottom = bottom;
                }
            }

            if (foundBottom)
            {
                loopNum++;

                /* Mark all blocks between 'top' and 'bottom' */

                optMarkLoopBlocks(top, foundBottom, false);
            }

            // We track at most 255 loops
            if (loopNum == 255)
            {
#if COUNT_LOOPS
                totalUnnatLoopOverflows++;
#endif
                break;
            }
        }

        // Check if any of the loops need alignment

        JITDUMP("\n");
        optIdentifyLoopsForAlignment();

#if COUNT_LOOPS
        totalUnnatLoopCount += loopNum;
#endif

#ifdef DEBUG
        if (verbose)
        {
            if (loopNum > 0)
            {
                printf("\nFound a total of %d loops.", loopNum);
                printf("\nAfter loop weight marking:\n");
                fgDispBasicBlocks();
                printf("\n");
            }
        }

        fgDebugCheckLoopTable();
#endif
        optLoopsMarked = true;
    }

    return PhaseStatus::MODIFIED_EVERYTHING;
}

bool Compiler::optIsVarAssigned(BasicBlock* beg, BasicBlock* end, GenTree* skip, unsigned lclNum)
{
    struct WalkData
    {
        GenTree* skip;
        unsigned lclNum;
    } walkData{skip, lclNum};

    for (;;)
    {
        noway_assert(beg != nullptr);

        for (Statement* stmt : beg->Statements())
        {
            if (fgWalkTreePre(stmt->GetRootNodePointer(),
                              [](GenTree** use, Compiler::fgWalkData* data) {
                                  GenTree*  tree = *use;
                                  WalkData* desc = static_cast<WalkData*>(data->pCallbackData);

                                  if (tree->OperIs(GT_ASG))
                                  {
                                      GenTree* dest = tree->AsOp()->GetOp(0);

                                      // TODO-MIKE-Cleanup: Why the crap are LCL_FLD assignments ignored?
                                      // This is likely used only for INT locals but then you can actually
                                      // modify an INT local with a LCL_FLD...
                                      if (dest->OperIs(GT_LCL_VAR) && (dest->AsLclVar()->GetLclNum() == desc->lclNum) &&
                                          (tree != desc->skip))
                                      {
                                          return Compiler::WALK_ABORT;
                                      }
                                  }

                                  return Compiler::WALK_CONTINUE;
                              },
                              &walkData) != WALK_CONTINUE)
            {
                return true;
            }
        }

        if (beg == end)
        {
            break;
        }

        beg = beg->bbNext;
    }

    return false;
}

void Compiler::optRemoveRangeCheck(GenTreeBoundsChk* check, GenTreeOp* comma, Statement* stmt)
{
    JITDUMPTREE(stmt->GetRootNode(), "Before optRemoveRangeCheck [%06u]:\n", check->GetID());

    assert((comma == nullptr) || (comma->OperIs(GT_COMMA) && (comma->GetOp(0) == check)));
    assert((comma != nullptr) || (stmt->GetRootNode() == check));

    GenTree* sideEffects = gtExtractSideEffList(check, GTF_ASG, /* ignoreRoot */ true);

    if (sideEffects == nullptr)
    {
        check->ChangeToNothingNode();

        if (comma != nullptr)
        {
            gtUpdateSideEffects(stmt, comma);
        }
    }
    else if (comma != nullptr)
    {
        comma->SetOp(0, sideEffects);
        gtUpdateSideEffects(stmt, comma);
    }
    else
    {
        stmt->SetRootNode(sideEffects);
    }

    JITDUMPTREE(stmt->GetRootNode(), "After optRemoveRangeCheck [%06u]:\n", check->GetID());
}

//-----------------------------------------------------------------------------
// OptTestInfo:     Member of OptBoolsDsc struct used to test if a GT_JTRUE or GT_RETURN node
//                  is a boolean comparison
//
struct OptTestInfo
{
    GenTree* testTree; // The root node of basic block with GT_JTRUE or GT_RETURN type to check boolean condition on
    GenTree* compTree; // The compare node (i.e. GT_EQ or GT_NE node) of the testTree
    bool     isBool;   // If the compTree is boolean expression
};

//-----------------------------------------------------------------------------
// OptBoolsDsc:     Descriptor used for Boolean Optimization
//
class OptBoolsDsc
{
public:
    OptBoolsDsc(BasicBlock* b1, BasicBlock* b2, Compiler* comp)
    {
        m_b1   = b1;
        m_b2   = b2;
        m_b3   = nullptr;
        m_comp = comp;
    }

private:
    BasicBlock* m_b1; // The first basic block with the BBJ_COND conditional jump type
    BasicBlock* m_b2; // The next basic block of m_b1. Either BBJ_COND or BBJ_RETURN type
    BasicBlock* m_b3; // m_b1->bbJumpDest. Null if m_b2 is not a return block.

    Compiler* m_comp; // The pointer to the Compiler instance

    OptTestInfo m_testInfo1; // The first test info
    OptTestInfo m_testInfo2; // The second test info
    GenTree*    m_t3;        // The root node of the first statement of m_b3

    GenTree* m_c1; // The first operand of m_testInfo1.compTree
    GenTree* m_c2; // The first operand of m_testInfo2.compTree

    bool m_sameTarget; // if m_b1 and m_b2 jumps to the same destination

    genTreeOps m_foldOp;   // The fold operator (e.g., GT_AND or GT_OR)
    var_types  m_foldType; // The type of the folded tree
    genTreeOps m_cmpOp;    // The comparison operator (e.g., GT_EQ or GT_NE)

public:
    bool optOptimizeBoolsCondBlock();
    bool optOptimizeBoolsReturnBlock(BasicBlock* b3);
#ifdef DEBUG
    void optOptimizeBoolsGcStress();
#endif

private:
    Statement* optOptimizeBoolsChkBlkCond();
    GenTree* optIsBoolComp(OptTestInfo* pOptTest);
    bool optOptimizeBoolsChkTypeCostCond();
    void optOptimizeBoolsUpdateTrees();
};

//-----------------------------------------------------------------------------
//  optOptimizeBoolsCondBlock:  Optimize boolean when bbJumpKind of both m_b1 and m_b2 are BBJ_COND
//
//  Returns:
//      true if boolean optimization is done and m_b1 and m_b2 are folded into m_b1, else false.
//
//  Notes:
//      m_b1 and m_b2 are set on entry.
//
//      Case 1: if b1.bbJumpDest == b2.bbJumpDest, it transforms
//          B1 : brtrue(t1, Bx)
//          B2 : brtrue(t2, Bx)
//          B3 :
//      to
//          B1 : brtrue(t1|t2, BX)
//          B3 :
//
//          For example, (x == 0 && y == 0 && z == 0) generates
//              B1: GT_JTRUE (BBJ_COND), jump to B4
//              B2: GT_JTRUE (BBJ_COND), jump to B4
//              B3: GT_RETURN (BBJ_RETURN)
//              B4: GT_RETURN (BBJ_RETURN)
//          and B1 and B2 are folded into B1:
//              B1: GT_JTRUE (BBJ_COND), jump to B4
//              B3: GT_RETURN (BBJ_RETURN)
//              B4: GT_RETURN (BBJ_RETURN)
//
//      Case 2: if B1.bbJumpDest == B2->bbNext, it transforms
//          B1 : brtrue(t1, B3)
//          B2 : brtrue(t2, Bx)
//          B3 :
//      to
//          B1 : brtrue((!t1) && t2, Bx)
//          B3 :
//
bool OptBoolsDsc::optOptimizeBoolsCondBlock()
{
    assert(m_b1 != nullptr && m_b2 != nullptr && m_b3 == nullptr);

    // Check if m_b1 and m_b2 jump to the same target and get back pointers to m_testInfo1 and t2 tree nodes

    m_t3 = nullptr;

    // Check if m_b1 and m_b2 have the same bbJumpDest

    if (m_b1->bbJumpDest == m_b2->bbJumpDest)
    {
        // Given the following sequence of blocks :
        //        B1: brtrue(t1, BX)
        //        B2: brtrue(t2, BX)
        //        B3:
        // we will try to fold it to :
        //        B1: brtrue(t1|t2, BX)
        //        B3:

        m_sameTarget = true;
    }
    else if (m_b1->bbJumpDest == m_b2->bbNext)
    {
        // Given the following sequence of blocks :
        //        B1: brtrue(t1, B3)
        //        B2: brtrue(t2, BX)
        //        B3:
        // we will try to fold it to :
        //        B1: brtrue((!t1)&&t2, BX)
        //        B3:

        m_sameTarget = false;
    }
    else
    {
        return false;
    }

    Statement* const s1 = optOptimizeBoolsChkBlkCond();
    if (s1 == nullptr)
    {
        return false;
    }

    // Find the branch conditions of m_b1 and m_b2

    m_c1 = optIsBoolComp(&m_testInfo1);
    if (m_c1 == nullptr)
    {
        return false;
    }

    m_c2 = optIsBoolComp(&m_testInfo2);
    if (m_c2 == nullptr)
    {
        return false;
    }

    // Find the type and cost conditions of m_testInfo1 and m_testInfo2

    if (!optOptimizeBoolsChkTypeCostCond())
    {
        return false;
    }

    // Get the fold operator and the comparison operator

    genTreeOps foldOp;
    genTreeOps cmpOp;
    var_types  foldType = m_c1->TypeGet();
    if (varTypeIsGC(foldType))
    {
        foldType = TYP_I_IMPL;
    }

    assert(m_testInfo1.compTree->gtOper == GT_EQ || m_testInfo1.compTree->gtOper == GT_NE);

    if (m_sameTarget)
    {
        // Both conditions must be the same

        if (m_testInfo1.compTree->gtOper != m_testInfo2.compTree->gtOper)
        {
            return false;
        }

        if (m_testInfo1.compTree->gtOper == GT_EQ)
        {
            // t1:c1==0 t2:c2==0 ==> Branch to BX if either value is 0
            // So we will branch to BX if (c1&c2)==0

            foldOp = GT_AND;
            cmpOp  = GT_EQ;
        }
        else
        {
            // t1:c1!=0 t2:c2!=0 ==> Branch to BX if either value is non-0
            // So we will branch to BX if (c1|c2)!=0

            foldOp = GT_OR;
            cmpOp  = GT_NE;
        }
    }
    else
    {
        // The m_b1 condition must be the reverse of the m_b2 condition because the only operators
        // that we will see here are GT_EQ and GT_NE. So, if they are not the same, we have one of each.

        if (m_testInfo1.compTree->gtOper == m_testInfo2.compTree->gtOper)
        {
            return false;
        }

        if (m_testInfo1.compTree->gtOper == GT_EQ)
        {
            // t1:c1==0 t2:c2!=0 ==> Branch to BX if both values are non-0
            // So we will branch to BX if (c1&c2)!=0

            foldOp = GT_AND;
            cmpOp  = GT_NE;
        }
        else
        {
            // t1:c1!=0 t2:c2==0 ==> Branch to BX if both values are 0
            // So we will branch to BX if (c1|c2)==0

            foldOp = GT_OR;
            cmpOp  = GT_EQ;
        }
    }

    // Anding requires both values to be 0 or 1

    if ((foldOp == GT_AND) && (!m_testInfo1.isBool || !m_testInfo2.isBool))
    {
        return false;
    }

    //
    // Now update the trees
    //

    m_foldOp   = foldOp;
    m_foldType = foldType;
    m_cmpOp    = cmpOp;

    optOptimizeBoolsUpdateTrees();

#ifdef DEBUG
    if (m_comp->verbose)
    {
        printf("Folded %sboolean conditions of " FMT_BB " and " FMT_BB " to :\n", m_c2->OperIsLeaf() ? "" : "non-leaf ",
               m_b1->bbNum, m_b2->bbNum);
        m_comp->gtDispStmt(s1);
        printf("\n");
    }
#endif

    // Return true to continue the bool optimization for the rest of the BB chain
    return true;
}

//-----------------------------------------------------------------------------
// optOptimizeBoolsChkBlkCond: Checks block conditions if it can be boolean optimized
//
// Return:
//      If all conditions pass, returns the last statement of m_b1, else return nullptr.
//
// Notes:
//      This method checks if the second (and third block for cond/return/return case) contains only one statement,
//      and checks if tree operators are of the right type, e.g, GT_JTRUE, GT_RETURN.
//
//      On entry, m_b1, m_b2 are set and m_b3 is set for cond/return/return case.
//      If it passes all the conditions, m_testInfo1.testTree, m_testInfo2.testTree and m_t3 are set
//      to the root nodes of m_b1, m_b2 and m_b3 each.
//      SameTarget is also updated to true if m_b1 and m_b2 jump to the same destination.
//
Statement* OptBoolsDsc::optOptimizeBoolsChkBlkCond()
{
    assert(m_b1 != nullptr && m_b2 != nullptr);

    bool optReturnBlock = false;
    if (m_b3 != nullptr)
    {
        optReturnBlock = true;
    }

    // Find the block conditions of m_b1 and m_b2

    if (m_b2->countOfInEdges() > 1 || (optReturnBlock && m_b3->countOfInEdges() > 1))
    {
        return nullptr;
    }

    // Find the condition for the first block

    Statement* s1 = m_b1->lastStmt();

    GenTree* testTree1 = s1->GetRootNode();
    assert(testTree1->gtOper == GT_JTRUE);

    // The second and the third block must contain a single statement

    Statement* s2 = m_b2->firstStmt();
    if (s2->GetPrevStmt() != s2)
    {
        return nullptr;
    }

    GenTree* testTree2 = s2->GetRootNode();

    if (!optReturnBlock)
    {
        assert(testTree2->gtOper == GT_JTRUE);
    }
    else
    {
        if (testTree2->gtOper != GT_RETURN)
        {
            return nullptr;
        }

        Statement* s3 = m_b3->firstStmt();
        if (s3->GetPrevStmt() != s3)
        {
            return nullptr;
        }

        GenTree* testTree3 = s3->GetRootNode();
        if (testTree3->gtOper != GT_RETURN)
        {
            return nullptr;
        }

        if (!varTypeIsIntegral(testTree2->TypeGet()) || !varTypeIsIntegral(testTree3->TypeGet()))
        {
            return nullptr;
        }

        // The third block is Return with "CNS_INT int 0/1"
        if (testTree3->AsOp()->gtOp1->gtOper != GT_CNS_INT)
        {
            return nullptr;
        }

        if (testTree3->AsOp()->gtOp1->gtType != TYP_INT)
        {
            return nullptr;
        }

        m_t3 = testTree3;
    }

    m_testInfo1.testTree = testTree1;
    m_testInfo2.testTree = testTree2;

    return s1;
}

//-----------------------------------------------------------------------------
// optOptimizeBoolsChkTypeCostCond: Checks if type conditions meet the folding condition, and
//                                  if cost to fold is not too expensive
//
// Return:
//      True if it meets type conditions and cost conditions.	Else false.
//
bool OptBoolsDsc::optOptimizeBoolsChkTypeCostCond()
{
    assert(m_testInfo1.compTree->OperIs(GT_EQ, GT_NE) && m_testInfo1.compTree->AsOp()->gtOp1 == m_c1);
    assert(m_testInfo2.compTree->OperIs(GT_EQ, GT_NE) && m_testInfo2.compTree->AsOp()->gtOp1 == m_c2);

    //
    // Leave out floats where the bit-representation is more complicated
    // - there are two representations for 0.
    //
    if (varTypeIsFloating(m_c1->TypeGet()) || varTypeIsFloating(m_c2->TypeGet()))
    {
        return false;
    }

    // Make sure the types involved are of the same sizes
    if (genTypeSize(m_c1->TypeGet()) != genTypeSize(m_c2->TypeGet()))
    {
        return false;
    }
    if (genTypeSize(m_testInfo1.compTree->TypeGet()) != genTypeSize(m_testInfo2.compTree->TypeGet()))
    {
        return false;
    }
#ifdef TARGET_ARMARCH
    // Skip the small operand which we cannot encode.
    if (varTypeIsSmall(m_c1->TypeGet()))
        return false;
#endif
    // The second condition must not contain side effects

    if (m_c2->gtFlags & GTF_GLOB_EFFECT)
    {
        return false;
    }

    // The second condition must not be too expensive

    m_comp->gtPrepareCost(m_c2);

    if (m_c2->GetCostEx() > 12)
    {
        return false;
    }

    return true;
}

//-----------------------------------------------------------------------------
// optOptimizeBoolsUpdateTrees: Fold the trees based on fold type and comparison type,
//                              update the edges, unlink removed blocks and update loop table
//
void OptBoolsDsc::optOptimizeBoolsUpdateTrees()
{
    assert(m_b1 != nullptr && m_b2 != nullptr);

    bool optReturnBlock = false;
    if (m_b3 != nullptr)
    {
        optReturnBlock = true;
    }

    assert(m_foldOp != NULL && m_foldType != NULL && m_c1 != nullptr && m_c2 != nullptr);

    GenTree* cmpOp1 = m_comp->gtNewOperNode(m_foldOp, m_foldType, m_c1, m_c2);
    if (m_testInfo1.isBool && m_testInfo2.isBool)
    {
        // When we 'OR'/'AND' two booleans, the result is boolean as well
        cmpOp1->gtFlags |= GTF_BOOLEAN;
    }

    GenTree* t1Comp = m_testInfo1.compTree;
    t1Comp->SetOper(m_cmpOp);
    t1Comp->AsOp()->gtOp1         = cmpOp1;
    t1Comp->AsOp()->gtOp2->gtType = m_foldType; // Could have been varTypeIsGC()
    if (optReturnBlock)
    {
        // Update tree when m_b1 is BBJ_COND and m_b2 and m_b3 are GT_RETURN (BBJ_RETURN)
        t1Comp->AsOp()->gtOp2->AsIntCon()->gtIconVal = 0;
        m_testInfo1.testTree->gtOper                 = GT_RETURN;
        m_testInfo1.testTree->gtType                 = m_testInfo2.testTree->gtType;

        // Update the return count of flow graph
        assert(m_comp->fgReturnCount >= 2);
        --m_comp->fgReturnCount;
    }

    if (!optReturnBlock)
    {
        // Update edges if m_b1: BBJ_COND and m_b2: BBJ_COND

        flowList* edge1 = m_comp->fgGetPredForBlock(m_b1->bbJumpDest, m_b1);
        flowList* edge2;

        if (m_sameTarget)
        {
            edge2 = m_comp->fgGetPredForBlock(m_b2->bbJumpDest, m_b2);
        }
        else
        {
            edge2 = m_comp->fgGetPredForBlock(m_b2->bbNext, m_b2);

            m_comp->fgRemoveRefPred(m_b1->bbJumpDest, m_b1);

            m_b1->bbJumpDest = m_b2->bbJumpDest;

            m_comp->fgAddRefPred(m_b2->bbJumpDest, m_b1);
        }

        assert(edge1 != nullptr);
        assert(edge2 != nullptr);

        BasicBlock::weight_t edgeSumMin = edge1->edgeWeightMin() + edge2->edgeWeightMin();
        BasicBlock::weight_t edgeSumMax = edge1->edgeWeightMax() + edge2->edgeWeightMax();
        if ((edgeSumMax >= edge1->edgeWeightMax()) && (edgeSumMax >= edge2->edgeWeightMax()))
        {
            edge1->setEdgeWeights(edgeSumMin, edgeSumMax, m_b1->bbJumpDest);
        }
        else
        {
            edge1->setEdgeWeights(BB_ZERO_WEIGHT, BB_MAX_WEIGHT, m_b1->bbJumpDest);
        }
    }

    /* Modify the target of the conditional jump and update bbRefs and bbPreds */

    if (optReturnBlock)
    {
        m_b1->bbJumpDest = nullptr;
        m_b1->bbJumpKind = BBJ_RETURN;
#ifdef DEBUG
        m_b1->bbJumpSwt = m_b2->bbJumpSwt;
#endif
        assert(m_b2->bbJumpKind == BBJ_RETURN);
        assert(m_b1->bbNext == m_b2);
        assert(m_b3 != nullptr);
    }
    else
    {
        assert(m_b1->bbJumpKind == BBJ_COND);
        assert(m_b2->bbJumpKind == BBJ_COND);
        assert(m_b1->bbJumpDest == m_b2->bbJumpDest);
        assert(m_b1->bbNext == m_b2);
        assert(m_b2->bbNext != nullptr);
    }

    if (!optReturnBlock)
    {
        // Update bbRefs and bbPreds
        //
        // Replace pred 'm_b2' for 'm_b2->bbNext' with 'm_b1'
        // Remove  pred 'm_b2' for 'm_b2->bbJumpDest'
        m_comp->fgReplacePred(m_b2->bbNext, m_b2, m_b1);
        m_comp->fgRemoveRefPred(m_b2->bbJumpDest, m_b2);
    }

    // Get rid of the second block

    m_comp->fgUnlinkBlock(m_b2);
    m_b2->bbFlags |= BBF_REMOVED;
    // If m_b2 was the last block of a try or handler, update the EH table.
    m_comp->ehUpdateForDeletedBlock(m_b2);

    if (optReturnBlock)
    {
        // Get rid of the third block
        m_comp->fgUnlinkBlock(m_b3);
        m_b3->bbFlags |= BBF_REMOVED;
        // If m_b3 was the last block of a try or handler, update the EH table.
        m_comp->ehUpdateForDeletedBlock(m_b3);
    }

    // Update loop table
    m_comp->fgUpdateLoopsAfterCompacting(m_b1, m_b2);
    if (optReturnBlock)
    {
        m_comp->fgUpdateLoopsAfterCompacting(m_b1, m_b3);
    }
}

//-----------------------------------------------------------------------------
//  optOptimizeBoolsReturnBlock: Optimize boolean when m_b1 is BBJ_COND and m_b2 and m_b3 are BBJ_RETURN
//
// Arguments:
//      b3:    Pointer to basic block b3
//
//  Returns:
//      true if boolean optimization is done and m_b1, m_b2 and m_b3 are folded into m_b1, else false.
//
//  Notes:
//      m_b1, m_b2 and m_b3 of OptBoolsDsc are set on entry.
//
//      if B1.bbJumpDest == b3, it transforms
//          B1 : brtrue(t1, B3)
//          B2 : ret(t2)
//          B3 : ret(0)
//      to
//          B1 : ret((!t1) && t2)
//
//          For example, (x==0 && y==0) generates:
//              B1: GT_JTRUE (BBJ_COND), jumps to B3
//              B2: GT_RETURN (BBJ_RETURN)
//              B3: GT_RETURN (BBJ_RETURN),
//          and it is folded into
//              B1: GT_RETURN (BBJ_RETURN)
//
bool OptBoolsDsc::optOptimizeBoolsReturnBlock(BasicBlock* b3)
{
    assert(m_b1 != nullptr && m_b2 != nullptr);

    // m_b3 is set for cond/return/return case
    m_b3 = b3;

    m_sameTarget        = false;
    Statement* const s1 = optOptimizeBoolsChkBlkCond();
    if (s1 == nullptr)
    {
        return false;
    }

    // Find the branch conditions of m_b1 and m_b2

    m_c1 = optIsBoolComp(&m_testInfo1);
    if (m_c1 == nullptr)
    {
        return false;
    }

    m_c2 = optIsBoolComp(&m_testInfo2);
    if (m_c2 == nullptr)
    {
        return false;
    }

    // Find the type and cost conditions of m_testInfo1 and m_testInfo2

    if (!optOptimizeBoolsChkTypeCostCond())
    {
        return false;
    }

    // Get the fold operator (m_foldOp, e.g., GT_OR/GT_AND) and
    // the comparison operator (m_cmpOp, e.g., GT_EQ/GT_NE)

    var_types foldType = m_c1->TypeGet();
    if (varTypeIsGC(foldType))
    {
        foldType = TYP_I_IMPL;
    }
    m_foldType = foldType;

    m_foldOp = GT_NONE;
    m_cmpOp  = GT_NONE;

    genTreeOps foldOp;
    genTreeOps cmpOp;

    ssize_t it1val = m_testInfo1.compTree->AsOp()->gtOp2->AsIntCon()->gtIconVal;
    ssize_t it2val = m_testInfo2.compTree->AsOp()->gtOp2->AsIntCon()->gtIconVal;
    ssize_t it3val = m_t3->AsOp()->gtOp1->AsIntCon()->gtIconVal;

    if ((m_testInfo1.compTree->gtOper == GT_NE && m_testInfo2.compTree->gtOper == GT_EQ) &&
        (it1val == 0 && it2val == 0 && it3val == 0))
    {
        // Case: x == 0 && y == 0
        //      t1:c1!=0 t2:c2==0 t3:c3==0
        //      ==> true if (c1|c2)==0
        foldOp = GT_OR;
        cmpOp  = GT_EQ;
    }
    else if ((m_testInfo1.compTree->gtOper == GT_EQ && m_testInfo2.compTree->gtOper == GT_NE) &&
             (it1val == 0 && it2val == 0 && it3val == 0))
    {
        // Case: x == 1 && y ==1
        //      t1:c1!=1 t2:c2==1 t3:c3==0 is reversed from optIsBoolComp() to: t1:c1==0 t2:c2!=0 t3:c3==0
        //      ==> true if (c1&c2)!=0
        foldOp = GT_AND;
        cmpOp  = GT_NE;
    }
    else if ((m_testInfo1.compTree->gtOper == GT_EQ && m_testInfo2.compTree->gtOper == GT_EQ) &&
             (it1val == 0 && it2val == 0 && it3val == 1))
    {
        // Case: x == 0 || y == 0
        //      t1:c1==0 t2:c2==0 t3:c3==1
        //      ==> true if (c1&c2)==0
        foldOp = GT_AND;
        cmpOp  = GT_EQ;
    }
    else if ((m_testInfo1.compTree->gtOper == GT_NE && m_testInfo2.compTree->gtOper == GT_NE) &&
             (it1val == 0 && it2val == 0 && it3val == 1))
    {
        // Case: x == 1 || y == 1
        //      t1:c1==1 t2:c2==1 t3:c3==1 is reversed from optIsBoolComp() to: t1:c1!=0 t2:c2!=0 t3:c3==1
        //      ==> true if (c1|c2)!=0
        foldOp = GT_OR;
        cmpOp  = GT_NE;
    }
    else
    {
        // Require NOT operation for operand(s). Do Not fold.
        return false;
    }

    if ((foldOp == GT_AND || cmpOp == GT_NE) && (!m_testInfo1.isBool || !m_testInfo2.isBool))
    {
        // x == 1 && y == 1: Skip cases where x or y is greather than 1, e.g., x=3, y=1
        // x == 0 || y == 0: Skip cases where x and y have opposite bits set, e.g., x=2, y=1
        // x == 1 || y == 1: Skip cases where either x or y is greater than 1, e.g., x=2, y=0
        return false;
    }

    m_foldOp = foldOp;
    m_cmpOp  = cmpOp;

    // Now update the trees

    optOptimizeBoolsUpdateTrees();

#ifdef DEBUG
    if (m_comp->verbose)
    {
        printf("Folded %sboolean conditions of " FMT_BB ", " FMT_BB " and " FMT_BB " to :\n",
               m_c2->OperIsLeaf() ? "" : "non-leaf ", m_b1->bbNum, m_b2->bbNum, m_b3->bbNum);
        m_comp->gtDispStmt(s1);
        printf("\n");
    }
#endif

    // Return true to continue the bool optimization for the rest of the BB chain
    return true;
}

//-----------------------------------------------------------------------------
//  optOptimizeBoolsGcStress: Replace x==null with (x|x)==0 if x is a GC-type.
//                            This will stress code-gen and the emitter to make sure they support such trees.
//
#ifdef DEBUG

void OptBoolsDsc::optOptimizeBoolsGcStress()
{
    if (!m_comp->compStressCompile(m_comp->STRESS_OPT_BOOLS_GC, 20))
    {
        return;
    }

    assert(m_b1->bbJumpKind == BBJ_COND);
    GenTree* cond = m_b1->lastStmt()->GetRootNode();

    assert(cond->gtOper == GT_JTRUE);

    OptTestInfo test;
    test.testTree = cond;

    GenTree* comparand = optIsBoolComp(&test);

    if (comparand == nullptr || !varTypeIsGC(comparand->TypeGet()))
    {
        return;
    }
    GenTree* relop  = test.compTree;
    bool     isBool = test.isBool;

    if (comparand->gtFlags & (GTF_ASG | GTF_CALL | GTF_ORDER_SIDEEFF))
    {
        return;
    }

    GenTree* comparandClone = m_comp->gtCloneExpr(comparand);

    noway_assert(relop->AsOp()->gtOp1 == comparand);
    genTreeOps oper      = m_comp->compStressCompile(m_comp->STRESS_OPT_BOOLS_GC, 50) ? GT_OR : GT_AND;
    relop->AsOp()->gtOp1 = m_comp->gtNewOperNode(oper, TYP_I_IMPL, comparand, comparandClone);

    // Comparand type is already checked, and we have const int, there is no harm
    // morphing it into a TYP_I_IMPL.
    noway_assert(relop->AsOp()->gtOp2->gtOper == GT_CNS_INT);
    relop->AsOp()->gtOp2->gtType = TYP_I_IMPL;
}

#endif

//-----------------------------------------------------------------------------
// optIsBoolComp:   Function used by folding of boolean conditionals
//
// Arguments:
//      pOptTest    The test info for the test tree
//
// Return:
//      On success, return the first operand (gtOp1) of compTree, else return nullptr.
//
// Notes:
//      On entry, testTree is set.
//      On success, compTree is set to the compare node (i.e. GT_EQ or GT_NE) of the testTree.
//      isBool is set to true if the comparand (i.e., operand 1 of compTree is boolean. Otherwise, false.
//
//      Given a GT_JTRUE or GT_RETURN node, this method checks if it is a boolean comparison
//      of the form "if (boolVal ==/!=  0/1)".This is translated into
//      a GT_EQ/GT_NE node with "opr1" being a boolean lclVar and "opr2" the const 0/1.
//
//      When isBool == true, if the comparison was against a 1 (i.e true)
//      then we morph the tree by reversing the GT_EQ/GT_NE and change the 1 to 0.
//
GenTree* OptBoolsDsc::optIsBoolComp(OptTestInfo* pOptTest)
{
    pOptTest->isBool = false;

    assert(pOptTest->testTree->gtOper == GT_JTRUE || pOptTest->testTree->gtOper == GT_RETURN);
    GenTree* cond = pOptTest->testTree->AsOp()->gtOp1;

    // The condition must be "!= 0" or "== 0"

    if ((cond->gtOper != GT_EQ) && (cond->gtOper != GT_NE))
    {
        return nullptr;
    }

    // Return the compare node to the caller

    pOptTest->compTree = cond;

    // Get hold of the comparands

    GenTree* opr1 = cond->AsOp()->gtOp1;
    GenTree* opr2 = cond->AsOp()->gtOp2;

    if (opr2->gtOper != GT_CNS_INT)
    {
        return nullptr;
    }

    if (!opr2->IsIntegralConst(0) && !opr2->IsIntegralConst(1))
    {
        return nullptr;
    }

    ssize_t ival2 = opr2->AsIntCon()->gtIconVal;

    // Is the value a boolean?
    // We can either have a boolean expression (marked GTF_BOOLEAN) or
    // a local variable that is marked as being boolean (lvIsBoolean)

    if (opr1->gtFlags & GTF_BOOLEAN)
    {
        pOptTest->isBool = true;
    }
    else if (opr1->OperIs(GT_CNS_INT) && (opr1->IsIntegralConst(0) || opr1->IsIntegralConst(1)))
    {
        pOptTest->isBool = true;
    }
    else if (opr1->OperIs(GT_LCL_VAR))
    {
        if (m_comp->lvaGetDesc(opr1->AsLclVar())->lvIsBoolean)
        {
            pOptTest->isBool = true;
        }
    }

    // Was our comparison against the constant 1 (i.e. true)
    if (ival2 == 1)
    {
        // If this is a boolean expression tree we can reverse the relop
        // and change the true to false.
        if (pOptTest->isBool)
        {
            m_comp->gtReverseRelop(cond->AsOp());
            opr2->AsIntCon()->gtIconVal = 0;
        }
        else
        {
            return nullptr;
        }
    }

    return opr1;
}

//-----------------------------------------------------------------------------
// optOptimizeBools:    Folds boolean conditionals for GT_JTRUE/GT_RETURN nodes
//
// Notes:
//      If the operand of GT_JTRUE/GT_RETURN node is GT_EQ/GT_NE of the form
//      "if (boolVal ==/!=  0/1)", the GT_EQ/GT_NE nodes are translated into a
//      GT_EQ/GT_NE node with
//          "op1" being a boolean GT_OR/GT_AND lclVar and
//          "op2" the const 0/1.
//      For example, the folded tree for the below boolean optimization is shown below:
//      Case 1:     (x == 0 && y ==0) => (x | y) == 0
//          *  RETURN   int
//          \--*  EQ        int
//             +--*  OR         int
//             |  +--*  LCL_VAR     int     V00 arg0
//             |  \--*  LCL_VAR     int     V01 arg1
//             \--*  CNS_INT    int     0
//
//      Case 2:     (x == null && y == null) ==> (x | y) == 0
//          *  RETURN    int
//          \-- * EQ        int
//              + -- * OR        long
//              |    +-- * LCL_VAR   ref    V00 arg0
//              |    \-- * LCL_VAR   ref    V01 arg1
//              \-- * CNS_INT   long   0
//
//      Case 3:     (x == 0 && y == 0 && z == 0) ==> ((x | y) | z) == 0
//          *  RETURN    int
//          \-- * EQ        int
//              + -- * OR        int
//              |    +-- * OR        int
//              |    |   +-- * LCL_VAR   int    V00 arg0
//              |    |   \-- * LCL_VAR   int    V01 arg1
//              |    \-- * LCL_VAR   int    V02 arg2
//              \-- * CNS_INT   int    0
//
//      Case 4:     (x == 0 && y == 0 && z == 0 && w == 0) ==> (((x | y) | z) | w) == 0
//          *  RETURN    int
//          \-- *  EQ        int
//              +  *  OR        int
//              |  +--*  OR        int
//              |  |  +--*  OR        int
//              |  |  |  +--*  LCL_VAR   int    V00 arg0
//              |  |  |  \--*  LCL_VAR   int    V01 arg1
//              |  |  \--*  LCL_VAR   int    V02 arg2
//              |  \--*  LCL_VAR   int    V03 arg3
//              \--*  CNS_INT   int    0
//
//      Patterns that are not optimized include (x == 1 && y == 1), (x == 1 || y == 1),
//      (x == 0 || y == 0) because currently their comptree is not marked as boolean expression.
//      When m_foldOp == GT_AND or m_cmpOp == GT_NE, both compTrees must be boolean expression
//      in order to skip below cases when compTree is not boolean expression:
//          - x == 1 && y == 1 ==> (x&y)!=0: Skip cases where x or y is greather than 1, e.g., x=3, y=1
//          - x == 1 || y == 1 ==> (x|y)!=0: Skip cases where either x or y is greater than 1, e.g., x=2, y=0
//          - x == 0 || y == 0 ==> (x&y)==0: Skip cases where x and y have opposite bits set, e.g., x=2, y=1
//
void Compiler::optOptimizeBools()
{
#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In optOptimizeBools()\n");
        if (verboseTrees)
        {
            printf("Blocks/Trees before phase\n");
            fgDispBasicBlocks(true);
        }
    }
#endif
    bool change;

    do
    {
        change = false;

        for (BasicBlock* const b1 : Blocks())
        {
            // We're only interested in conditional jumps here

            if (b1->bbJumpKind != BBJ_COND)
            {
                continue;
            }

            // If there is no next block, we're done

            BasicBlock* b2 = b1->bbNext;
            if (b2 == nullptr)
            {
                break;
            }

            // The next block must not be marked as BBF_DONT_REMOVE
            if (b2->bbFlags & BBF_DONT_REMOVE)
            {
                continue;
            }

            OptBoolsDsc optBoolsDsc(b1, b2, this);

            // The next block needs to be a condition or return block.

            if (b2->bbJumpKind == BBJ_COND)
            {
                if ((b1->bbJumpDest != b2->bbJumpDest) && (b1->bbJumpDest != b2->bbNext))
                {
                    continue;
                }

                // When it is conditional jumps

                if (optBoolsDsc.optOptimizeBoolsCondBlock())
                {
                    change = true;
                }
            }
            else if (b2->bbJumpKind == BBJ_RETURN)
            {
                // Set b3 to b1 jump destination
                BasicBlock* b3 = b1->bbJumpDest;

                // b3 must not be marked as BBF_DONT_REMOVE

                if (b3->bbFlags & BBF_DONT_REMOVE)
                {
                    continue;
                }

                // b3 must be RETURN type

                if (b3->bbJumpKind != BBJ_RETURN)
                {
                    continue;
                }

                if (optBoolsDsc.optOptimizeBoolsReturnBlock(b3))
                {
                    change = true;
                }
            }
            else
            {
#ifdef DEBUG
                optBoolsDsc.optOptimizeBoolsGcStress();
#endif
            }
        }
    } while (change);

#ifdef DEBUG
    fgDebugCheckBBlist();
#endif
}

// TODO-MIKE-Review: Does this thing actually work? Copies are added but
// they don't appear to be used. The idea was probably that copy prop will
// pick up these copies but that doesn't seem to happen.
// Something similar (and better) could likely be done directly in SSA.
// As is now it appears to do more harm than good, the added copies aren't
// eliminated and apparently interfere with register allocation.
void Compiler::optAddCopies()
{
#ifdef DEBUG
    if (verboseTrees)
    {
        printf("Blocks/Trees at start of phase\n");
        fgDispBasicBlocks(true);
    }
#endif

    // Don't add any copies if we have reached the tracking limit.
    if (lvaHaveManyLocals())
    {
        return;
    }

    for (unsigned lclNum = 0; lclNum < lvaCount; lclNum++)
    {
        LclVarDsc* varDsc = lvaGetDesc(lclNum);
        var_types  typ    = varDsc->TypeGet();

        // We only add copies for non temp local variables
        // that have a single def and that can possibly be enregistered

        if (varDsc->lvIsTemp || !varDsc->lvSingleDef || (typ == TYP_STRUCT) || varTypeIsSmall(typ))
        {
            continue;
        }

        // If locals must be initialized to zero, that initialization counts as a second definition.
        // VB in particular allows usage of variables not explicitly initialized.
        // Note that this effectively disables this optimization for all local variables
        // as C# sets InitLocals all the time starting in Whidbey.

        if (!varDsc->IsParam() && info.compInitMem)
        {
            continue;
        }

        // On x86 we may want to add a copy for an incoming double parameter
        // because we can ensure that the copy we make is double aligned
        // where as we can never ensure the alignment of an incoming double parameter
        //
        // On all other platforms we will never need to make a copy
        // for an incoming double parameter
        //
        // TODO-MIKE-CQ: So if this is done in order to get around DOUBLE parameter
        // alignment then why the crap it's also checking for FLOAT?!?

        bool isFloatParam = false;

#ifdef TARGET_X86
        isFloatParam = varDsc->IsParam() && varTypeIsFloating(typ);
#endif

        if (!isFloatParam && !varDsc->lvHasEHRefs)
        {
            continue;
        }

        // We don't want to add a copy for a variable that is part of a struct
        if (varDsc->lvIsStructField)
        {
            continue;
        }

        if (BlockSetOps::MayBeUninit(varDsc->lvUseBlocks))
        {
            // No references
            continue;
        }

        // We require that the weighted ref count be significant.
        if (varDsc->lvRefCntWtd() <= (BB_LOOP_WEIGHT_SCALE * BB_UNITY_WEIGHT / 2))
        {
            continue;
        }

        // For parameters, we only want to add a copy for the heavier-than-average
        // uses instead of adding a copy to cover every single use.
        // 'paramImportantUseDom' is the set of blocks that dominate the
        // heavier-than-average uses of a parameter.
        // Initial value is all blocks.

        BlockSet paramImportantUseDom(BlockSetOps::MakeFull(this));

        // This will be threshold for determining heavier-than-average uses
        BasicBlock::weight_t paramAvgWtdRefDiv2 =
            (varDsc->lvRefCntWtd() + varDsc->lvRefCnt() / 2) / (varDsc->lvRefCnt() * 2);

        bool paramFoundImportantUse = false;

        JITDUMP("Trying to add a copy for %s V%02u, avg_wtd = %s\n", varDsc->IsParam() ? "param" : "local", lclNum,
                refCntWtd2str(paramAvgWtdRefDiv2));

        //
        // We must have a ref in a block that is dominated only by the entry block
        //

        bool isDominatedByFirstBB = false;

        BlockSetOps::Iter iter(this, varDsc->lvUseBlocks);
        unsigned          bbNum = 0;
        while (iter.NextElem(&bbNum))
        {
            /* Find the block 'bbNum' */
            BasicBlock* block = fgFirstBB;
            while (block && (block->bbNum != bbNum))
            {
                block = block->bbNext;
            }
            noway_assert(block && (block->bbNum == bbNum));

            bool     importantUseInBlock = varDsc->IsParam() && (block->getBBWeight(this) > paramAvgWtdRefDiv2);
            bool     isPreHeaderBlock    = ((block->bbFlags & BBF_LOOP_PREHEADER) != 0);
            BlockSet blockDom(BlockSetOps::UninitVal());
            BlockSet blockDomSub0(BlockSetOps::UninitVal());

            if (block->bbIDom == nullptr && isPreHeaderBlock)
            {
                // Loop Preheader blocks that we insert will have a bbDom set that is nullptr
                // but we can instead use the bNext successor block's dominator information
                noway_assert(block->bbNext != nullptr);
                BlockSetOps::AssignNoCopy(this, blockDom, fgGetDominatorSet(block->bbNext));
            }
            else
            {
                BlockSetOps::AssignNoCopy(this, blockDom, fgGetDominatorSet(block));
            }

            if (!BlockSetOps::IsEmpty(this, blockDom))
            {
                BlockSetOps::Assign(this, blockDomSub0, blockDom);
                if (isPreHeaderBlock)
                {
                    // We must clear bbNext block number from the dominator set
                    BlockSetOps::RemoveElemD(this, blockDomSub0, block->bbNext->bbNum);
                }
                /* Is this block dominated by fgFirstBB? */
                if (BlockSetOps::IsMember(this, blockDomSub0, fgFirstBB->bbNum))
                {
                    isDominatedByFirstBB = true;
                }
            }

#ifdef DEBUG
            if (verbose)
            {
                printf("        Referenced in " FMT_BB ", bbWeight is %s", bbNum,
                       refCntWtd2str(block->getBBWeight(this)));

                if (isDominatedByFirstBB)
                {
                    printf(", which is dominated by BB01");
                }

                if (importantUseInBlock)
                {
                    printf(", ImportantUse");
                }

                printf("\n");
            }
#endif

            /* If this is a heavier-than-average block, then track which
               blocks dominate this use of the parameter. */
            if (importantUseInBlock)
            {
                paramFoundImportantUse = true;
                BlockSetOps::IntersectionD(this, paramImportantUseDom,
                                           blockDomSub0); // Clear blocks that do not dominate
            }
        }

        // We should have found at least one heavier-than-averageDiv2 block.
        if (varDsc->IsParam())
        {
            if (!paramFoundImportantUse)
            {
                continue;
            }
        }

        // For us to add a new copy:
        // we require that we have a floating point parameter
        // or an EH live variable that is always reached from the first BB
        // and we have at least one block available in paramImportantUseDom
        bool doCopy = (isFloatParam || (isDominatedByFirstBB && varDsc->lvHasEHRefs)) &&
                      !BlockSetOps::IsEmpty(this, paramImportantUseDom);

        // Under stress mode we expand the number of candidates
        // to include parameters of any type
        // or any variable that is always reached from the first BB
        //
        if (compStressCompile(STRESS_GENERIC_VARN, 30))
        {
            // Ensure that we preserve the invariants required by the subsequent code.
            if (varDsc->IsParam() || isDominatedByFirstBB)
            {
                doCopy = true;
            }
        }

        if (!doCopy)
        {
            continue;
        }

        unsigned copyLclNum = lvaGrabTemp(false DEBUGARG("optAddCopies"));
        // Because lvaGrabTemp may have reallocated the lvaTable, ensure varDsc
        // is still in sync with lvaTable[lclNum];
        varDsc = lvaGetDesc(lclNum);

        if (varTypeIsSIMD(varDsc->GetType()))
        {
            lvaSetStruct(copyLclNum, varDsc->GetLayout(), /* checkUnsafeBuffer */ false);
            assert(lvaGetDesc(copyLclNum)->GetType() == typ);
        }
        else
        {
            lvaGetDesc(copyLclNum)->SetType(typ);
        }

        JITDUMP("Finding the best place to insert the assignment V%02i = V%02i\n", copyLclNum, lclNum);

        Statement* stmt;

        if (varDsc->IsParam())
        {
            noway_assert((varDsc->lvDefStmt == nullptr) || varDsc->lvIsStructField);

            // Create a new copy assignment tree
            GenTree* copyAsgn = gtNewAssignNode(gtNewLclvNode(copyLclNum, typ), gtNewLclvNode(lclNum, typ));

            /* Find the best block to insert the new assignment     */
            /* We will choose the lowest weighted block, and within */
            /* those block, the highest numbered block which        */
            /* dominates all the uses of the local variable         */

            /* Our default is to use the first block */
            BasicBlock*          bestBlock  = fgFirstBB;
            BasicBlock::weight_t bestWeight = bestBlock->getBBWeight(this);
            BasicBlock*          block      = bestBlock;

#ifdef DEBUG
            if (verbose)
            {
                printf("        Starting at " FMT_BB ", bbWeight is %s", block->bbNum,
                       refCntWtd2str(block->getBBWeight(this)));

                printf(", bestWeight is %s\n", refCntWtd2str(bestWeight));
            }
#endif

            /* We have already calculated paramImportantUseDom above. */
            BlockSetOps::Iter iter(this, paramImportantUseDom);
            unsigned          bbNum = 0;
            while (iter.NextElem(&bbNum))
            {
                /* Advance block to point to 'bbNum' */
                /* This assumes that the iterator returns block number is increasing lexical order. */
                while (block && (block->bbNum != bbNum))
                {
                    block = block->bbNext;
                }
                noway_assert(block && (block->bbNum == bbNum));

#ifdef DEBUG
                if (verbose)
                {
                    printf("        Considering " FMT_BB ", bbWeight is %s", block->bbNum,
                           refCntWtd2str(block->getBBWeight(this)));

                    printf(", bestWeight is %s\n", refCntWtd2str(bestWeight));
                }
#endif

                // Does this block have a smaller bbWeight value?
                if (block->getBBWeight(this) > bestWeight)
                {
#ifdef DEBUG
                    if (verbose)
                    {
                        printf("bbWeight too high\n");
                    }
#endif
                    continue;
                }

                // Don't use blocks that are exception handlers because
                // inserting a new first statement will interface with
                // the CATCHARG

                if (handlerGetsXcptnObj(block->bbCatchTyp))
                {
#ifdef DEBUG
                    if (verbose)
                    {
                        printf("Catch block\n");
                    }
#endif
                    continue;
                }

                // Don't use the BBJ_ALWAYS block marked with BBF_KEEP_BBJ_ALWAYS. These
                // are used by EH code. The JIT can not generate code for such a block.

                if (block->bbFlags & BBF_KEEP_BBJ_ALWAYS)
                {
#if defined(FEATURE_EH_FUNCLETS)
                    // With funclets, this is only used for BBJ_CALLFINALLY/BBJ_ALWAYS pairs. For x86, it is also used
                    // as the "final step" block for leaving finallys.
                    assert(block->isBBCallAlwaysPairTail());
#endif // FEATURE_EH_FUNCLETS
#ifdef DEBUG
                    if (verbose)
                    {
                        printf("Internal EH BBJ_ALWAYS block\n");
                    }
#endif
                    continue;
                }

                // This block will be the new candidate for the insert point
                // for the new assignment
                CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef DEBUG
                if (verbose)
                {
                    printf("new bestBlock\n");
                }
#endif

                bestBlock  = block;
                bestWeight = block->getBBWeight(this);
            }

            // If there is a use of the variable in this block
            // then we insert the assignment at the beginning
            // otherwise we insert the statement at the end
            CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef DEBUG
            if (verbose)
            {
                printf("        Insert copy at the %s of " FMT_BB "\n",
                       (BlockSetOps::IsEmpty(this, paramImportantUseDom) ||
                        BlockSetOps::IsMember(this, varDsc->lvUseBlocks, bestBlock->bbNum))
                           ? "start"
                           : "end",
                       bestBlock->bbNum);
            }
#endif

            if (BlockSetOps::IsEmpty(this, paramImportantUseDom) ||
                BlockSetOps::IsMember(this, varDsc->lvUseBlocks, bestBlock->bbNum))
            {
                stmt = fgNewStmtAtBeg(bestBlock, copyAsgn);
            }
            else
            {
                stmt = fgNewStmtNearEnd(bestBlock, copyAsgn);
            }
        }
        else
        {
            noway_assert(varDsc->lvDefStmt != nullptr);

            /* Locate the assignment to varDsc in the lvDefStmt */
            stmt = varDsc->lvDefStmt;

            GenTreeOp* tree = nullptr;

            for (GenTree* node = stmt->GetRootNode(); node != nullptr; node = node->gtPrev)
            {
                if (!node->OperIs(GT_ASG))
                {
                    continue;
                }

                GenTree* dest = node->AsOp()->GetOp(0);

                if (!dest->OperIs(GT_LCL_VAR) || (dest->AsLclVar()->GetLclNum() != lclNum))
                {
                    continue;
                }

                tree = node->AsOp();
                break;
            }

            noway_assert(tree != nullptr);

            GenTree* newAsg  = gtNewAssignNode(gtNewLclvNode(copyLclNum, typ), tree->GetOp(1));
            GenTree* copyAsg = gtNewAssignNode(tree->GetOp(0), gtNewLclvNode(copyLclNum, typ));

            tree->ChangeOper(GT_COMMA);
            tree->SetOp(0, newAsg);
            tree->SetOp(1, copyAsg);
            tree->SetType(TYP_VOID);
            tree->SetSideEffects(newAsg->GetSideEffects() | copyAsg->GetSideEffects());
            tree->gtFlags &= ~GTF_REVERSE_OPS;
        }

        JITDUMPTREE(stmt->GetRootNode(), "\nIntroduced a copy for V%02u\n", lclNum);
    }
}

// Remove redundant zero intializations.
//
// Notes:
//    This phase iterates over basic blocks starting with the first basic block until there is no unique
//    basic block successor or until it detects a loop. It keeps track of local nodes it encounters.
//    When it gets to an assignment to a local variable or a local field, it checks whether the assignment
//    is the first reference to the local (or to the parent of the local field), and, if so,
//    it may do one of two optimizations:
//      1. If the following conditions are true:
//            the local is untracked,
//            the rhs of the assignment is 0,
//            the local is guaranteed to be fully initialized in the prolog,
//         then the explicit zero initialization is removed.
//      2. If the following conditions are true:
//            the assignment is to a local (and not a field),
//            the local is not lvLiveInOutOfHndlr or no exceptions can be thrown between the prolog and the assignment,
//            either the local has no gc pointers or there are no gc-safe points between the prolog and the assignment,
//         then the local is marked with lvHasExplicitInit which tells the codegen not to insert zero initialization
//         for this local in the prolog.
//
void Compiler::phRemoveRedundantZeroInits()
{
    using LclVarRefCounts = JitHashTable<unsigned, JitSmallPrimitiveKeyFuncs<unsigned>, unsigned>;

    CompAllocator   allocator(getAllocator(CMK_ZeroInit));
    LclVarRefCounts refCounts(allocator);
    BitVecTraits    bitVecTraits(lvaCount, this);
    BitVec          zeroInitLocals = BitVecOps::MakeEmpty(&bitVecTraits);
    bool            hasGCSafePoint = false;
    bool            canThrow       = false;

    assert(fgStmtListThreaded);

    for (BasicBlock* block = fgFirstBB; (block != nullptr) && ((block->bbFlags & BBF_MARKED) == 0);
         block             = block->GetUniqueSucc())
    {
        block->bbFlags |= BBF_MARKED;
        CompAllocator   allocator(getAllocator(CMK_ZeroInit));
        LclVarRefCounts defsInBlock(allocator);
        bool            removedTrackedDefs = false;
        for (Statement* stmt = block->FirstNonPhiDef(); stmt != nullptr;)
        {
            Statement* next = stmt->GetNextStmt();
            for (GenTree* const tree : stmt->TreeList())
            {
                if (((tree->gtFlags & GTF_CALL) != 0))
                {
                    hasGCSafePoint = true;
                }

                if ((tree->gtFlags & GTF_EXCEPT) != 0)
                {
                    canThrow = true;
                }

                switch (tree->gtOper)
                {
                    case GT_LCL_ADDR:
                    {
                        unsigned  lclNum    = tree->AsLclAddr()->GetLclNum();
                        unsigned* pRefCount = refCounts.LookupPointer(lclNum);
                        if (pRefCount != nullptr)
                        {
                            *pRefCount = (*pRefCount) + 1;
                        }
                        else
                        {
                            refCounts.Set(lclNum, 1);
                        }
                        break;
                    }

                    case GT_LCL_VAR:
                    case GT_LCL_FLD:
                    {
                        unsigned  lclNum    = tree->AsLclVarCommon()->GetLclNum();
                        unsigned* pRefCount = refCounts.LookupPointer(lclNum);
                        if (pRefCount != nullptr)
                        {
                            *pRefCount = (*pRefCount) + 1;
                        }
                        else
                        {
                            refCounts.Set(lclNum, 1);
                        }

                        if ((tree->gtFlags & GTF_VAR_DEF) == 0)
                        {
                            break;
                        }

                        // We need to count the number of tracked var defs in the block
                        // so that we can update block->bbVarDef if we remove any tracked var defs.

                        LclVarDsc* const lclDsc = lvaGetDesc(lclNum);
                        if (lclDsc->lvTracked)
                        {
                            unsigned* pDefsCount = defsInBlock.LookupPointer(lclNum);
                            if (pDefsCount != nullptr)
                            {
                                *pDefsCount = (*pDefsCount) + 1;
                            }
                            else
                            {
                                defsInBlock.Set(lclNum, 1);
                            }
                        }
                        else if (varTypeIsStruct(lclDsc) && ((tree->gtFlags & GTF_VAR_USEASG) == 0) &&
                                 lclDsc->IsPromoted())
                        {
                            for (unsigned i = 0; i < lclDsc->GetPromotedFieldCount(); ++i)
                            {
                                unsigned fieldLclNum = lclDsc->GetPromotedFieldLclNum(i);

                                if (lvaGetDesc(fieldLclNum)->lvTracked)
                                {
                                    unsigned* pDefsCount = defsInBlock.LookupPointer(fieldLclNum);

                                    if (pDefsCount != nullptr)
                                    {
                                        (*pDefsCount)++;
                                    }
                                    else
                                    {
                                        defsInBlock.Set(fieldLclNum, 1);
                                    }
                                }
                            }
                        }

                        break;
                    }
                    case GT_ASG:
                    {
                        GenTreeLclVarCommon* lclNode = nullptr;

                        if (tree->AsOp()->GetOp(0)->OperIs(GT_LCL_VAR, GT_LCL_FLD))
                        {
                            lclNode = tree->AsOp()->GetOp(0)->AsLclVarCommon();
                        }

                        // TODO-MIKE-CQ: This could also recognize indirect local stores.
                        // Though they're so rare that's hardly worth the trouble...

                        if (lclNode == nullptr)
                        {
                            break;
                        }

                        const unsigned lclNum = lclNode->GetLclNum();

                        LclVarDsc* const lclDsc    = lvaGetDesc(lclNum);
                        unsigned*        pRefCount = refCounts.LookupPointer(lclNum);

                        // pRefCount can't be null because the local node on the lhs of the assignment
                        // must have already been seen.
                        assert(pRefCount != nullptr);
                        if (*pRefCount != 1)
                        {
                            break;
                        }

                        unsigned parentRefCount = 0;
                        if (lclDsc->lvIsStructField && refCounts.Lookup(lclDsc->lvParentLcl, &parentRefCount) &&
                            (parentRefCount != 0))
                        {
                            break;
                        }

                        unsigned fieldRefCount = 0;
                        if (lclDsc->lvPromoted)
                        {
                            for (unsigned i = lclDsc->lvFieldLclStart;
                                 (fieldRefCount == 0) && (i < lclDsc->lvFieldLclStart + lclDsc->lvFieldCnt); ++i)
                            {
                                refCounts.Lookup(i, &fieldRefCount);
                            }
                        }

                        if (fieldRefCount != 0)
                        {
                            break;
                        }

                        // The local hasn't been referenced before this assignment.
                        bool removedExplicitZeroInit = false;
                        bool totalOverlap            = !lclNode->IsPartialLclFld(this);

                        if (tree->AsOp()->GetOp(1)->IsIntegralConst(0))
                        {
                            bool bbInALoop  = (block->bbFlags & BBF_BACKWARD_JUMP) != 0;
                            bool bbIsReturn = block->bbJumpKind == BBJ_RETURN;

                            if (!bbInALoop || bbIsReturn)
                            {
                                if (BitVecOps::IsMember(&bitVecTraits, zeroInitLocals, lclNum) ||
                                    (lclDsc->lvIsStructField &&
                                     BitVecOps::IsMember(&bitVecTraits, zeroInitLocals, lclDsc->lvParentLcl)) ||
                                    ((!lclDsc->lvTracked || !totalOverlap) &&
                                     !fgVarNeedsExplicitZeroInit(lclNum, bbInALoop, bbIsReturn)))
                                {
                                    // We are guaranteed to have a zero initialization in the prolog or a
                                    // dominating explicit zero initialization and the local hasn't been redefined
                                    // between the prolog and this explicit zero initialization so the assignment
                                    // can be safely removed.
                                    if (tree == stmt->GetRootNode())
                                    {
                                        fgRemoveStmt(block, stmt);
                                        removedExplicitZeroInit      = true;
                                        lclDsc->lvSuppressedZeroInit = 1;

                                        if (lclDsc->lvTracked)
                                        {
                                            removedTrackedDefs   = true;
                                            unsigned* pDefsCount = defsInBlock.LookupPointer(lclNum);
                                            *pDefsCount          = (*pDefsCount) - 1;
                                        }
                                    }
                                }

                                if (totalOverlap)
                                {
                                    BitVecOps::AddElemD(&bitVecTraits, zeroInitLocals, lclNum);
                                }
                                *pRefCount = 0;
                            }
                        }

                        if (!removedExplicitZeroInit && totalOverlap && (!canThrow || !lclDsc->lvLiveInOutOfHndlr))
                        {
                            // If compMethodRequiresPInvokeFrame() returns true, lower may later
                            // insert a call to CORINFO_HELP_INIT_PINVOKE_FRAME which is a gc-safe point.
                            if (!lclDsc->HasGCPtr() ||
                                (!codeGen->GetInterruptible() && !hasGCSafePoint && !compMethodRequiresPInvokeFrame()))
                            {
                                // The local hasn't been used and won't be reported to the gc between
                                // the prolog and this explicit intialization. Therefore, it doesn't
                                // require zero initialization in the prolog.
                                lclDsc->lvHasExplicitInit = 1;

                                // If the local is the only field of a promoted struct local then the
                                // promoted struct local also doesn't require zero initialization in
                                // the prolog.
                                if (lclDsc->IsPromotedField())
                                {
                                    LclVarDsc* parentLcl = lvaGetDesc(lclDsc->GetPromotedFieldParentLclNum());

                                    if (parentLcl->GetLayout()->GetSize() == varTypeSize(lclDsc->GetType()))
                                    {
                                        parentLcl->lvHasExplicitInit = 1;
                                    }
                                }

                                JITDUMP("Marking V%02u as having an explicit init\n", lclNum);
                            }
                        }
                        break;
                    }
                    default:
                        break;
                }
            }
            stmt = next;
        }

        if (removedTrackedDefs)
        {
            LclVarRefCounts::KeyIterator iter(defsInBlock.Begin());
            LclVarRefCounts::KeyIterator end(defsInBlock.End());
            for (; !iter.Equal(end); ++iter)
            {
                unsigned int lclNum = iter.Get();
                if (defsInBlock[lclNum] == 0)
                {
                    VarSetOps::RemoveElemD(this, block->bbVarDef, lvaGetDesc(lclNum)->lvVarIndex);
                }
            }
        }
    }

    for (BasicBlock* block = fgFirstBB; (block != nullptr) && ((block->bbFlags & BBF_MARKED) != 0);
         block             = block->GetUniqueSucc())
    {
        block->bbFlags &= ~BBF_MARKED;
    }
}
