// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef _MSC_VER
#pragma hdrstop
#endif

// Flowgraph Miscellany

//------------------------------------------------------------------------
// blockNeedsGCPoll: Determine whether the block needs GC poll inserted
//
// Arguments:
//   block         - the block to check
//
// Notes:
//    The GC poll may not be required because of optimizations applied earlier
//    or because of GC poll done implicitly by regular unmanaged calls.
//
// Returns:
//    Whether the GC poll needs to be inserted after the block
//
static bool blockNeedsGCPoll(BasicBlock* block)
{
    bool blockMayNeedGCPoll = false;
    for (Statement* const stmt : block->NonPhiStatements())
    {
        if ((stmt->GetRootNode()->gtFlags & GTF_CALL) != 0)
        {
            for (GenTree* const tree : stmt->TreeList())
            {
                if (tree->OperGet() == GT_CALL)
                {
                    GenTreeCall* call = tree->AsCall();
                    if (call->IsUnmanaged())
                    {
                        if (!call->IsSuppressGCTransition())
                        {
                            // If the block contains regular unmanaged call, we can depend on it
                            // to poll for GC. No need to scan further.
                            return false;
                        }

                        blockMayNeedGCPoll = true;
                    }
                }
            }
        }
    }
    return blockMayNeedGCPoll;
}

//------------------------------------------------------------------------------
// fgInsertGCPolls : Insert GC polls for basic blocks containing calls to methods
//                   with SuppressGCTransitionAttribute.
//
// Notes:
//    When not optimizing, the method relies on BBF_HAS_SUPPRESSGC_CALL flag to
//    find the basic blocks that require GC polls; when optimizing the tree nodes
//    are scanned to find calls to methods with SuppressGCTransitionAttribute.
//
//    This must be done after any transformations that would add control flow between
//    calls.
//
// Returns:
//    PhaseStatus indicating what, if anything, was changed.
//

PhaseStatus Compiler::fgInsertGCPolls()
{
    PhaseStatus result = PhaseStatus::MODIFIED_NOTHING;

    if ((optMethodFlags & OMF_NEEDS_GCPOLLS) == 0)
    {
        return result;
    }

    bool createdPollBlocks = false;

#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In fgInsertGCPolls() for %s\n", info.compFullName);
        fgDispBasicBlocks(false);
        printf("\n");
    }
#endif // DEBUG

    BasicBlock* block;

    // Walk through the blocks and hunt for a block that needs a GC Poll
    for (block = fgFirstBB; block != nullptr; block = block->bbNext)
    {
        // When optimizations are enabled, we can't rely on BBF_HAS_SUPPRESSGC_CALL flag:
        // the call could've been moved, e.g., hoisted from a loop, CSE'd, etc.
        if (opts.OptimizationDisabled() ? ((block->bbFlags & BBF_HAS_SUPPRESSGC_CALL) == 0) : !blockNeedsGCPoll(block))
        {
            continue;
        }

        result = PhaseStatus::MODIFIED_EVERYTHING;

        // This block needs a GC poll. We either just insert a callout or we split the block and inline part of
        // the test.

        // If we're doing GCPOLL_CALL, just insert a GT_CALL node before the last node in the block.
        CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef DEBUG
        switch (block->bbJumpKind)
        {
            case BBJ_RETURN:
            case BBJ_ALWAYS:
            case BBJ_COND:
            case BBJ_SWITCH:
            case BBJ_NONE:
            case BBJ_THROW:
            case BBJ_CALLFINALLY:
                break;
            default:
                assert(!"Unexpected block kind");
        }
#endif // DEBUG

        GCPollType pollType = GCPOLL_INLINE;

        // We'd like to inset an inline poll. Below is the list of places where we
        // can't or don't want to emit an inline poll. Check all of those. If after all of that we still
        // have INLINE, then emit an inline check.

        if (opts.OptimizationDisabled())
        {
#ifdef DEBUG
            if (verbose)
            {
                printf("Selecting CALL poll in block " FMT_BB " because of debug/minopts\n", block->bbNum);
            }
#endif // DEBUG

            // Don't split blocks and create inlined polls unless we're optimizing.
            pollType = GCPOLL_CALL;
        }
        else if (genReturnBB == block)
        {
#ifdef DEBUG
            if (verbose)
            {
                printf("Selecting CALL poll in block " FMT_BB " because it is the single return block\n", block->bbNum);
            }
#endif // DEBUG

            // we don't want to split the single return block
            pollType = GCPOLL_CALL;
        }
        else if (BBJ_SWITCH == block->bbJumpKind)
        {
#ifdef DEBUG
            if (verbose)
            {
                printf("Selecting CALL poll in block " FMT_BB " because it is a SWITCH block\n", block->bbNum);
            }
#endif // DEBUG

            // We don't want to deal with all the outgoing edges of a switch block.
            pollType = GCPOLL_CALL;
        }
        else if ((block->bbFlags & BBF_COLD) != 0)
        {
#ifdef DEBUG
            if (verbose)
            {
                printf("Selecting CALL poll in block " FMT_BB " because it is a cold block\n", block->bbNum);
            }
#endif // DEBUG

            // We don't want to split a cold block.
            pollType = GCPOLL_CALL;
        }

        BasicBlock* curBasicBlock = fgCreateGCPoll(pollType, block);
        createdPollBlocks |= (block != curBasicBlock);
        block = curBasicBlock;
    }

    // If we split a block to create a GC Poll, then rerun fgReorderBlocks to push the rarely run blocks out
    // past the epilog.  We should never split blocks unless we're optimizing.
    if (createdPollBlocks)
    {
        noway_assert(opts.OptimizationEnabled());
        fgReorderBlocks();
        constexpr bool computePreds = true;
        constexpr bool computeDoms  = false;
        fgUpdateChangedFlowGraph(computePreds, computeDoms);
    }
#ifdef DEBUG
    if (verbose)
    {
        printf("*************** After fgInsertGCPolls()\n");
        fgDispBasicBlocks(true);
    }
#endif // DEBUG

    return result;
}

//------------------------------------------------------------------------------
// fgCreateGCPoll : Insert a GC poll of the specified type for the given basic block.
//
// Arguments:
//    pollType  - The type of GC poll to insert
//    block     - Basic block to insert the poll for
//
// Return Value:
//    If new basic blocks are inserted, the last inserted block; otherwise, the input block.
//

BasicBlock* Compiler::fgCreateGCPoll(GCPollType pollType, BasicBlock* block)
{
    bool createdPollBlocks;

    void* addrTrap;
    void* pAddrOfCaptureThreadGlobal;

    addrTrap = info.compCompHnd->getAddrOfCaptureThreadGlobal(&pAddrOfCaptureThreadGlobal);

    // If the trap and address of thread global are null, make the call.
    if (addrTrap == nullptr && pAddrOfCaptureThreadGlobal == nullptr)
    {
        pollType = GCPOLL_CALL;
    }

    // Create the GC_CALL node
    GenTree* call = gtNewHelperCallNode(CORINFO_HELP_POLL_GC, TYP_VOID);
    call          = fgMorphCall(call->AsCall());
    gtSetEvalOrder(call);

    BasicBlock* bottom = nullptr;

    if (pollType == GCPOLL_CALL)
    {
        createdPollBlocks = false;

        Statement* newStmt = nullptr;
        if ((block->bbJumpKind == BBJ_ALWAYS) || (block->bbJumpKind == BBJ_CALLFINALLY) ||
            (block->bbJumpKind == BBJ_NONE))
        {
            // For BBJ_ALWAYS, BBJ_CALLFINALLY, and BBJ_NONE and  we don't need to insert it before the condition.
            // Just append it.
            newStmt = fgNewStmtAtEnd(block, call);
        }
        else
        {
            newStmt = fgNewStmtNearEnd(block, call);
            // For DDB156656, we need to associate the GC Poll with the IL offset (and therefore sequence
            // point) of the tree before which we inserted the poll.  One example of when this is a
            // problem:
            //  if (...) {  //1
            //      ...
            //  } //2
            //  else { //3
            //      ...
            //  }
            //  (gcpoll) //4
            //  return. //5
            //
            //  If we take the if statement at 1, we encounter a jump at 2.  This jumps over the else
            //  and lands at 4.  4 is where we inserted the gcpoll.  However, that is associated with
            //  the sequence point a 3.  Therefore, the debugger displays the wrong source line at the
            //  gc poll location.
            //
            //  More formally, if control flow targets an instruction, that instruction must be the
            //  start of a new sequence point.
            Statement* nextStmt = newStmt->GetNextStmt();
            if (nextStmt != nullptr)
            {
                // Is it possible for gtNextStmt to be NULL?
                newStmt->SetILOffsetX(nextStmt->GetILOffsetX());
            }
        }

        if (fgStmtListThreaded)
        {
            gtSetStmtInfo(newStmt);
            fgSetStmtSeq(newStmt);
        }

        block->bbFlags |= BBF_GC_SAFE_POINT;
#ifdef DEBUG
        if (verbose)
        {
            printf("*** creating GC Poll in block " FMT_BB "\n", block->bbNum);
            gtDispBlockStmts(block);
        }
#endif // DEBUG
    }
    else // GCPOLL_INLINE
    {
        assert(pollType == GCPOLL_INLINE);
        createdPollBlocks = true;
        // if we're doing GCPOLL_INLINE, then:
        //  1) Create two new blocks: Poll and Bottom.  The original block is called Top.

        // I want to create:
        // top -> poll -> bottom (lexically)
        // so that we jump over poll to get to bottom.
        BasicBlock*   top                = block;
        BasicBlock*   topFallThrough     = nullptr;
        unsigned char lpIndexFallThrough = BasicBlock::NOT_IN_LOOP;

        if (top->bbJumpKind == BBJ_COND)
        {
            topFallThrough     = top->bbNext;
            lpIndexFallThrough = topFallThrough->bbNatLoopNum;
        }

        BasicBlock* poll          = fgNewBBafter(BBJ_NONE, top, true);
        bottom                    = fgNewBBafter(top->bbJumpKind, poll, true);
        BBjumpKinds   oldJumpKind = top->bbJumpKind;
        unsigned char lpIndex     = top->bbNatLoopNum;

        // Update block flags
        const BasicBlockFlags originalFlags = top->bbFlags | BBF_GC_SAFE_POINT;

        // We are allowed to split loops and we need to keep a few other flags...
        //
        noway_assert((originalFlags & (BBF_SPLIT_NONEXIST &
                                       ~(BBF_LOOP_HEAD | BBF_LOOP_CALL0 | BBF_LOOP_CALL1 | BBF_LOOP_PREHEADER |
                                         BBF_RETLESS_CALL))) == 0);
        top->bbFlags = originalFlags & (~(BBF_SPLIT_LOST | BBF_LOOP_PREHEADER | BBF_RETLESS_CALL) | BBF_GC_SAFE_POINT);
        bottom->bbFlags |= originalFlags & (BBF_SPLIT_GAINED | BBF_IMPORTED | BBF_GC_SAFE_POINT | BBF_LOOP_PREHEADER |
                                            BBF_RETLESS_CALL);
        bottom->inheritWeight(top);
        poll->bbFlags |= originalFlags & (BBF_SPLIT_GAINED | BBF_IMPORTED | BBF_GC_SAFE_POINT);

        // Mark Poll as rarely run.
        poll->bbSetRunRarely();
        poll->bbNatLoopNum = lpIndex; // Set the bbNatLoopNum in case we are in a loop

        // Bottom gets all the outgoing edges and inherited flags of Original.
        bottom->bbJumpDest   = top->bbJumpDest;
        bottom->bbNatLoopNum = lpIndex; // Set the bbNatLoopNum in case we are in a loop
        if (lpIndex != BasicBlock::NOT_IN_LOOP)
        {
            // Set the new lpBottom in the natural loop table
            optLoopTable[lpIndex].lpBottom = bottom;
        }

        if (lpIndexFallThrough != BasicBlock::NOT_IN_LOOP)
        {
            // Set the new lpHead in the natural loop table
            optLoopTable[lpIndexFallThrough].lpHead = bottom;
        }

        // Add the GC_CALL node to Poll.
        Statement* pollStmt = fgNewStmtAtEnd(poll, call);
        if (fgStmtListThreaded)
        {
            gtSetStmtInfo(pollStmt);
            fgSetStmtSeq(pollStmt);
        }

        // Remove the last statement from Top and add it to Bottom if necessary.
        if ((oldJumpKind == BBJ_COND) || (oldJumpKind == BBJ_RETURN) || (oldJumpKind == BBJ_THROW))
        {
            Statement* stmt = top->firstStmt();
            while (stmt->GetNextStmt() != nullptr)
            {
                stmt = stmt->GetNextStmt();
            }
            fgRemoveStmt(top, stmt);
            fgInsertStmtAtEnd(bottom, stmt);
        }

        // for BBJ_ALWAYS blocks, bottom is an empty block.

        // Create a GT_EQ node that checks against g_TrapReturningThreads.  True jumps to Bottom,
        // false falls through to poll.  Add this to the end of Top.  Top is now BBJ_COND.  Bottom is
        // now a jump target
        CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef ENABLE_FAST_GCPOLL_HELPER
        // Prefer the fast gc poll helepr over the double indirection
        noway_assert(pAddrOfCaptureThreadGlobal == nullptr);
#endif

        GenTree* value; // The value of g_TrapReturningThreads
        if (pAddrOfCaptureThreadGlobal != nullptr)
        {
            // Use a double indirection
            GenTree* addr =
                gtNewIndOfIconHandleNode(TYP_I_IMPL, (size_t)pAddrOfCaptureThreadGlobal, GTF_ICON_CONST_PTR, true);

            value = gtNewOperNode(GT_IND, TYP_INT, addr);
            // This indirection won't cause an exception.
            value->gtFlags |= GTF_IND_NONFAULTING;
        }
        else
        {
            // Use a single indirection
            value = gtNewIndOfIconHandleNode(TYP_INT, (size_t)addrTrap, GTF_ICON_GLOBAL_PTR, false);
        }

        // NOTE: in c++ an equivalent load is done via LoadWithoutBarrier() to ensure that the
        // program order is preserved. (not hoisted out of a loop or cached in a local, for example)
        //
        // Here we introduce the read really late after all major optimizations are done, and the location
        // is formally unknown, so noone could optimize the load, thus no special flags are needed.

        // Compare for equal to zero
        GenTree* trapRelop = gtNewOperNode(GT_EQ, TYP_INT, value, gtNewIconNode(0, TYP_INT));

        trapRelop->gtFlags |= GTF_RELOP_JMP_USED | GTF_DONT_CSE;
        GenTree* trapCheck = gtNewOperNode(GT_JTRUE, TYP_VOID, trapRelop);
        gtSetEvalOrder(trapCheck);
        Statement* trapCheckStmt = fgNewStmtAtEnd(top, trapCheck);
        if (fgStmtListThreaded)
        {
            gtSetStmtInfo(trapCheckStmt);
            fgSetStmtSeq(trapCheckStmt);
        }

#ifdef DEBUG
        if (verbose)
        {
            printf("Adding trapCheck in " FMT_BB "\n", top->bbNum);
            gtDispTree(trapCheck);
        }
#endif

        top->bbJumpDest = bottom;
        top->bbJumpKind = BBJ_COND;

        // Bottom has Top and Poll as its predecessors.  Poll has just Top as a predecessor.
        fgAddRefPred(bottom, poll);
        fgAddRefPred(bottom, top);
        fgAddRefPred(poll, top);

        // Replace Top with Bottom in the predecessor list of all outgoing edges from Bottom
        // (1 for unconditional branches, 2 for conditional branches, N for switches).
        switch (oldJumpKind)
        {
            case BBJ_NONE:
                fgReplacePred(bottom->bbNext, top, bottom);
                break;
            case BBJ_RETURN:
            case BBJ_THROW:
                // no successors
                break;
            case BBJ_COND:
                // replace predecessor in the fall through block.
                noway_assert(bottom->bbNext);
                fgReplacePred(bottom->bbNext, top, bottom);

                // fall through for the jump target
                FALLTHROUGH;

            case BBJ_ALWAYS:
            case BBJ_CALLFINALLY:
                fgReplacePred(bottom->bbJumpDest, top, bottom);
                break;
            case BBJ_SWITCH:
                NO_WAY("SWITCH should be a call rather than an inlined poll.");
                break;
            default:
                NO_WAY("Unknown block type for updating predecessor lists.");
        }

        if (compCurBB == top)
        {
            compCurBB = bottom;
        }

#ifdef DEBUG
        if (verbose)
        {
            printf("*** creating inlined GC Poll in top block " FMT_BB "\n", top->bbNum);
            gtDispBlockStmts(top);
            printf(" poll block is " FMT_BB "\n", poll->bbNum);
            gtDispBlockStmts(poll);
            printf(" bottom block is " FMT_BB "\n", bottom->bbNum);
            gtDispBlockStmts(bottom);

            printf("\nAfter this change in fgCreateGCPoll the BB graph is:");
            fgDispBasicBlocks(false);
        }
#endif // DEBUG
    }

    return createdPollBlocks ? bottom : block;
}

//------------------------------------------------------------------------
// fgCanSwitchToOptimized: Determines if conditions are met to allow switching the opt level to optimized
//
// Return Value:
//    True if the opt level may be switched from tier 0 to optimized, false otherwise
//
// Assumptions:
//    - compInitOptions() has been called
//    - compSetOptimizationLevel() has not been called
//
// Notes:
//    This method is to be called at some point before compSetOptimizationLevel() to determine if the opt level may be
//    changed based on information gathered in early phases.

bool Compiler::fgCanSwitchToOptimized()
{
    bool result = opts.jitFlags->IsSet(JitFlags::JIT_FLAG_TIER0) && !opts.jitFlags->IsSet(JitFlags::JIT_FLAG_MIN_OPT) &&
                  !opts.compDbgCode && !compIsForInlining();
    if (result)
    {
        // Ensure that it would be safe to change the opt level
        assert(opts.optFlags == CLFLG_MINOPT);
        assert(!opts.IsMinOptsSet());
    }

    return result;
}

//------------------------------------------------------------------------
// fgSwitchToOptimized: Switch the opt level from tier 0 to optimized
//
// Assumptions:
//    - fgCanSwitchToOptimized() is true
//    - compSetOptimizationLevel() has not been called
//
// Notes:
//    This method is to be called at some point before compSetOptimizationLevel() to switch the opt level to optimized
//    based on information gathered in early phases.

void Compiler::fgSwitchToOptimized()
{
    assert(fgCanSwitchToOptimized());

    // Switch to optimized and re-init options
    JITDUMP("****\n**** JIT Tier0 jit request switching to Tier1 because of loop\n****\n");
    assert(opts.jitFlags->IsSet(JitFlags::JIT_FLAG_TIER0));
    opts.jitFlags->Clear(JitFlags::JIT_FLAG_TIER0);
    opts.jitFlags->Clear(JitFlags::JIT_FLAG_BBINSTR);

    // Leave a note for jit diagnostics
    compSwitchedToOptimized = true;

    compInitOptions(opts.jitFlags);

    // Notify the VM of the change
    info.compCompHnd->setMethodAttribs(info.compMethodHnd, CORINFO_FLG_SWITCHED_TO_OPTIMIZED);
}

//------------------------------------------------------------------------
// fgMayExplicitTailCall: Estimates conservatively for an explicit tail call, if the importer may actually use a tail
// call.
//
// Return Value:
//    - False if a tail call will not be generated
//    - True if a tail call *may* be generated
//
// Assumptions:
//    - compInitOptions() has been called
//    - info.compIsVarArgs has been initialized
//    - An explicit tail call has been seen
//    - compSetOptimizationLevel() has not been called

bool Compiler::fgMayExplicitTailCall()
{
    assert(!compIsForInlining());

    if (info.compFlags & CORINFO_FLG_SYNCH)
    {
        // Caller is synchronized
        return false;
    }

    if (opts.IsReversePInvoke())
    {
        // Reverse P/Invoke
        return false;
    }

#if !FEATURE_FIXED_OUT_ARGS
    if (info.compIsVarArgs)
    {
        // Caller is varargs
        return false;
    }
#endif // FEATURE_FIXED_OUT_ARGS

    return true;
}

//------------------------------------------------------------------------
// fgFindJumpTargets: walk the IL stream, determining jump target offsets
//
// Arguments:
//    codeAddr   - base address of the IL code buffer
//    codeSize   - number of bytes in the IL code buffer
//    jumpTarget - [OUT] bit vector for flagging jump targets
//
// Notes:
//    If inlining or prejitting the root, this method also makes
//    various observations about the method that factor into inline
//    decisions.
//
//    May throw an exception if the IL is malformed.
//
//    jumpTarget[N] is set to 1 if IL offset N is a jump target in the method.
//
//    Also sets lvAddrExposed and lvHasILStoreOp, ilHasMultipleILStoreOp in lvaTable[].

#ifdef _PREFAST_
#pragma warning(push)
#pragma warning(disable : 21000) // Suppress PREFast warning about overly large function
#endif

//------------------------------------------------------------------------
// fgImport: read the IL for the method and create jit IR
//
// Returns:
//    phase status
//
PhaseStatus Compiler::fgImport()
{
    impImport();

    // Estimate how much of method IL was actually imported.
    //
    // Note this includes (to some extent) the impact of importer folded
    // branches, provided the folded tree covered the entire block's IL.
    unsigned importedILSize = 0;
    for (BasicBlock* const block : Blocks())
    {
        if ((block->bbFlags & BBF_IMPORTED) != 0)
        {
            // Assume if we generate any IR for the block we generate IR for the entire block.
            if (block->firstStmt() != nullptr)
            {
                IL_OFFSET beginOffset = block->bbCodeOffs;
                IL_OFFSET endOffset   = block->bbCodeOffsEnd;

                if ((beginOffset != BAD_IL_OFFSET) && (endOffset != BAD_IL_OFFSET) && (endOffset > beginOffset))
                {
                    unsigned blockILSize = endOffset - beginOffset;
                    importedILSize += blockILSize;
                }
            }
        }
    }

    // Could be tripped up if we ever duplicate blocks
    assert(importedILSize <= info.compILCodeSize);

    // Leave a note if we only did a partial import.
    if (importedILSize != info.compILCodeSize)
    {
        JITDUMP("\n** Note: %s IL was partially imported -- imported %u of %u bytes of method IL\n",
                compIsForInlining() ? "inlinee" : "root method", importedILSize, info.compILCodeSize);
    }

    // Record this for diagnostics and for the inliner's budget computations
    info.compILImportSize = importedILSize;

    if (compIsForInlining())
    {
        compInlineResult->SetImportedILSize(info.compILImportSize);
    }

    // Full preds are only used later on
    assert(!fgComputePredsDone);
    if (fgCheapPredsValid)
    {
        // Cheap predecessors are only used during importation
        fgRemovePreds();
    }

    return PhaseStatus::MODIFIED_EVERYTHING;
}

/*****************************************************************************
 * This function returns true if tree is a node with a call
 * that unconditionally throws an exception
 */

bool Compiler::fgIsThrow(GenTree* tree)
{
    if (!tree->IsCall())
    {
        return false;
    }
    GenTreeCall* call = tree->AsCall();
    if ((call->gtCallType == CT_HELPER) && s_helperCallProperties.AlwaysThrow(eeGetHelperNum(call->gtCallMethHnd)))
    {
        noway_assert(call->gtFlags & GTF_EXCEPT);
        return true;
    }
    return false;
}

/*****************************************************************************
 * This function returns true for blocks that are in different hot-cold regions.
 * It returns false when the blocks are both in the same regions
 */

bool Compiler::fgInDifferentRegions(BasicBlock* blk1, BasicBlock* blk2)
{
    noway_assert(blk1 != nullptr);
    noway_assert(blk2 != nullptr);

    if (fgFirstColdBlock == nullptr)
    {
        return false;
    }

    // If one block is Hot and the other is Cold then we are in different regions
    return ((blk1->bbFlags & BBF_COLD) != (blk2->bbFlags & BBF_COLD));
}

bool Compiler::fgIsBlockCold(BasicBlock* blk)
{
    noway_assert(blk != nullptr);

    if (fgFirstColdBlock == nullptr)
    {
        return false;
    }

    return ((blk->bbFlags & BBF_COLD) != 0);
}

/*****************************************************************************
 * This function returns true if tree is a GT_COMMA node with a call
 * that unconditionally throws an exception
 */

bool Compiler::fgIsCommaThrow(GenTree* tree, bool forFolding /* = false */)
{
    // Instead of always folding comma throws,
    // with stress enabled we only fold half the time

    if (forFolding && compStressCompile(STRESS_FOLD, 50))
    {
        return false; /* Don't fold */
    }

    /* Check for cast of a GT_COMMA with a throw overflow */
    if ((tree->gtOper == GT_COMMA) && (tree->gtFlags & GTF_CALL) && (tree->gtFlags & GTF_EXCEPT))
    {
        return (fgIsThrow(tree->AsOp()->gtOp1));
    }
    return false;
}

GenTreeCall* Compiler::fgGetStaticsCCtorHelper(CORINFO_CLASS_HANDLE cls, CorInfoHelpFunc helper)
{
    bool         bNeedClassID = true;
    GenTreeFlags callFlags    = GTF_EMPTY;

    var_types type = TYP_BYREF;

    // This is sort of ugly, as we have knowledge of what the helper is returning.
    // We need the return type.
    switch (helper)
    {
        case CORINFO_HELP_GETSHARED_GCSTATIC_BASE_NOCTOR:
            bNeedClassID = false;
            FALLTHROUGH;

        case CORINFO_HELP_GETSHARED_GCTHREADSTATIC_BASE_NOCTOR:
            callFlags |= GTF_CALL_HOISTABLE;
            FALLTHROUGH;

        case CORINFO_HELP_GETSHARED_GCSTATIC_BASE:
        case CORINFO_HELP_GETSHARED_GCSTATIC_BASE_DYNAMICCLASS:
        case CORINFO_HELP_GETSHARED_NONGCSTATIC_BASE_DYNAMICCLASS:
        case CORINFO_HELP_GETSHARED_GCTHREADSTATIC_BASE:
        case CORINFO_HELP_GETSHARED_GCTHREADSTATIC_BASE_DYNAMICCLASS:
        case CORINFO_HELP_GETSHARED_NONGCTHREADSTATIC_BASE_DYNAMICCLASS:
            // type = TYP_BYREF;
            break;

        case CORINFO_HELP_GETSHARED_NONGCSTATIC_BASE_NOCTOR:
            bNeedClassID = false;
            FALLTHROUGH;

        case CORINFO_HELP_GETSHARED_NONGCTHREADSTATIC_BASE_NOCTOR:
            callFlags |= GTF_CALL_HOISTABLE;
            FALLTHROUGH;

        case CORINFO_HELP_GETSHARED_NONGCSTATIC_BASE:
        case CORINFO_HELP_GETSHARED_NONGCTHREADSTATIC_BASE:
        case CORINFO_HELP_CLASSINIT_SHARED_DYNAMICCLASS:
            type = TYP_I_IMPL;
            break;

        default:
            assert(!"unknown shared statics helper");
            break;
    }

    GenTreeCall::Use* argList = nullptr;

    GenTree* opModuleIDArg;
    GenTree* opClassIDArg;

    // Get the class ID
    unsigned clsID;
    size_t   moduleID;
    void*    pclsID;
    void*    pmoduleID;

    clsID = info.compCompHnd->getClassDomainID(cls, &pclsID);

    moduleID = info.compCompHnd->getClassModuleIdForStatics(cls, nullptr, &pmoduleID);

    if (!(callFlags & GTF_CALL_HOISTABLE))
    {
        if (info.compCompHnd->getClassAttribs(cls) & CORINFO_FLG_BEFOREFIELDINIT)
        {
            callFlags |= GTF_CALL_HOISTABLE;
        }
    }

    if (pmoduleID)
    {
        opModuleIDArg = gtNewIndOfIconHandleNode(TYP_I_IMPL, (size_t)pmoduleID, GTF_ICON_CIDMID_HDL, true);
    }
    else
    {
        opModuleIDArg = gtNewIconNode((size_t)moduleID, TYP_I_IMPL);
    }

    if (bNeedClassID)
    {
        if (pclsID)
        {
            opClassIDArg = gtNewIndOfIconHandleNode(TYP_INT, (size_t)pclsID, GTF_ICON_CIDMID_HDL, true);
        }
        else
        {
            opClassIDArg = gtNewIconNode(clsID, TYP_INT);
        }

        // call the helper to get the base
        argList = gtNewCallArgs(opModuleIDArg, opClassIDArg);
    }
    else
    {
        argList = gtNewCallArgs(opModuleIDArg);
    }

    GenTreeCall* result = gtNewHelperCallNode(helper, type, argList);
    result->gtFlags |= callFlags;

    // If we're importing the special EqualityComparer<T>.Default or Comparer<T>.Default
    // intrinsics, flag the helper call. Later during inlining, we can
    // remove the helper call if the associated field lookup is unused.
    if ((info.compFlags & CORINFO_FLG_JIT_INTRINSIC) != 0)
    {
        NamedIntrinsic ni = lookupNamedIntrinsic(info.compMethodHnd);
        if ((ni == NI_System_Collections_Generic_EqualityComparer_get_Default) ||
            (ni == NI_System_Collections_Generic_Comparer_get_Default))
        {
            JITDUMP("\nmarking helper call [%06u] as special dce...\n", result->gtTreeID);
            result->gtCallMoreFlags |= GTF_CALL_M_HELPER_SPECIAL_DCE;
        }
    }

    return result;
}

GenTreeCall* Compiler::fgGetSharedCCtor(CORINFO_CLASS_HANDLE cls)
{
#ifdef FEATURE_READYTORUN_COMPILER
    if (opts.IsReadyToRun())
    {
        CORINFO_RESOLVED_TOKEN resolvedToken;
        memset(&resolvedToken, 0, sizeof(resolvedToken));
        resolvedToken.hClass = cls;

        return impReadyToRunHelperToTree(&resolvedToken, CORINFO_HELP_READYTORUN_STATIC_BASE, TYP_BYREF);
    }
#endif

    // Call the shared non gc static helper, as its the fastest
    return fgGetStaticsCCtorHelper(cls, info.compCompHnd->getSharedCCtorHelper(cls));
}

bool Compiler::fgAddrCouldBeNull(GenTree* addr)
{
    addr = addr->gtEffectiveVal();

    if (addr->OperIs(GT_ADD))
    {
        GenTree* op1 = addr->AsOp()->GetOp(0);
        GenTree* op2 = addr->AsOp()->GetOp(1);

        if (GenTreeIntCon* const2 = op2->IsIntCon())
        {
            // Static struct field boxed instances cannot be null.
            if ((const2->GetFieldSeq() != nullptr) && (const2->GetFieldSeq()->IsBoxedValueField()))
            {
                assert(const2->GetValue() >= TARGET_POINTER_SIZE);
                return !op1->TypeIs(TYP_REF);
            }
        }

        return true;
    }

    if (addr->OperIs(GT_CNS_INT))
    {
        // TODO-MIKE-Review: It's not clear what this has to do with handles. Any
        // non 0 constant is obviously not null. It may be an invalid address but
        // it's not like the spec requires detecting such addresses.

        return !addr->IsIconHandle();
    }

    if (addr->OperIs(GT_CNS_STR, GT_FIELD_ADDR, GT_INDEX_ADDR, GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR, GT_CLS_VAR_ADDR))
    {
        return false;
    }

    if (addr->OperIs(GT_LCL_VAR))
    {
        // TODO-MIKE-CQ: The return buffer addres is supposed to be non null too.
        // But surprise, the JIT ABI is broken and the address may be null.
        // Oh well, given how the address is used it probably doesn't matter.

        return !lvaGetDesc(addr->AsLclVar())->IsImplicitByRefParam();
    }

    return true;
}

//------------------------------------------------------------------------------
// fgOptimizeDelegateConstructor: try and optimize construction of a delegate
//
// Arguments:
//    call -- call to original delegate constructor
//    exactContextHnd -- [out] context handle to update
//    ldftnToken -- [in]  resolved token for the method the delegate will invoke,
//      if known, or nullptr if not known
//
// Return Value:
//    Original call tree if no optimization applies.
//    Updated call tree if optimized.

GenTree* Compiler::fgOptimizeDelegateConstructor(GenTreeCall*            call,
                                                 CORINFO_CONTEXT_HANDLE* ExactContextHnd,
                                                 CORINFO_RESOLVED_TOKEN* ldftnToken)
{
    JITDUMP("\nfgOptimizeDelegateConstructor: ");
    noway_assert(call->gtCallType == CT_USER_FUNC);
    CORINFO_METHOD_HANDLE methHnd = call->gtCallMethHnd;
    CORINFO_CLASS_HANDLE  clsHnd  = info.compCompHnd->getMethodClass(methHnd);

    GenTree* targetMethod = call->gtCallArgs->GetNext()->GetNode();
    noway_assert(targetMethod->TypeGet() == TYP_I_IMPL);
    genTreeOps            oper            = targetMethod->OperGet();
    CORINFO_METHOD_HANDLE targetMethodHnd = nullptr;
    GenTree*              qmarkNode       = nullptr;
    if (oper == GT_FTN_ADDR)
    {
        targetMethodHnd = targetMethod->AsFptrVal()->gtFptrMethod;
    }
    else if (oper == GT_CALL && targetMethod->AsCall()->gtCallMethHnd == eeFindHelper(CORINFO_HELP_VIRTUAL_FUNC_PTR))
    {
        GenTree* handleNode = targetMethod->AsCall()->gtCallArgs->GetNext()->GetNext()->GetNode();

        if (handleNode->OperGet() == GT_CNS_INT)
        {
            // it's a ldvirtftn case, fetch the methodhandle off the helper for ldvirtftn. It's the 3rd arg
            targetMethodHnd = CORINFO_METHOD_HANDLE(handleNode->AsIntCon()->gtCompileTimeHandle);
        }
        // Sometimes the argument to this is the result of a generic dictionary lookup, which shows
        // up as a GT_QMARK.
        else if (handleNode->OperGet() == GT_QMARK)
        {
            qmarkNode = handleNode;
        }
    }
    // Sometimes we don't call CORINFO_HELP_VIRTUAL_FUNC_PTR but instead just call
    // CORINFO_HELP_RUNTIMEHANDLE_METHOD directly.
    else if (oper == GT_QMARK)
    {
        qmarkNode = targetMethod;
    }
    if (qmarkNode != nullptr)
    {
        noway_assert(qmarkNode->OperGet() == GT_QMARK);
        // The argument is actually a generic dictionary lookup.  For delegate creation it looks
        // like:
        // GT_QMARK
        //  GT_COLON
        //      op1 -> call
        //              Arg 1 -> token (has compile time handle)
        //      op2 -> lclvar
        //
        //
        // In this case I can find the token (which is a method handle) and that is the compile time
        // handle.
        noway_assert(qmarkNode->AsQmark()->GetThen()->IsCall());

        GenTreeCall* runtimeLookupCall = qmarkNode->AsQmark()->GetThen()->AsCall();

        // This could be any of CORINFO_HELP_RUNTIMEHANDLE_(METHOD|CLASS)(_LOG?)
        GenTree* tokenNode = runtimeLookupCall->gtCallArgs->GetNext()->GetNode();
        noway_assert(tokenNode->OperGet() == GT_CNS_INT);
        targetMethodHnd = CORINFO_METHOD_HANDLE(tokenNode->AsIntCon()->gtCompileTimeHandle);
    }

    // Verify using the ldftnToken gives us all of what we used to get
    // via the above pattern match, and more...
    if (ldftnToken != nullptr)
    {
        assert(ldftnToken->hMethod != nullptr);

        if (targetMethodHnd != nullptr)
        {
            assert(targetMethodHnd == ldftnToken->hMethod);
        }

        targetMethodHnd = ldftnToken->hMethod;
    }
    else
    {
        assert(targetMethodHnd == nullptr);
    }

#ifdef FEATURE_READYTORUN_COMPILER
    if (opts.IsReadyToRun())
    {
        if (IsTargetAbi(CORINFO_CORERT_ABI))
        {
            if (ldftnToken != nullptr)
            {
                JITDUMP("optimized\n");

                GenTree*             thisPointer       = call->gtCallThisArg->GetNode();
                GenTree*             targetObjPointers = call->gtCallArgs->GetNode();
                GenTreeCall::Use*    helperArgs        = nullptr;
                CORINFO_LOOKUP       pLookup;
                CORINFO_CONST_LOOKUP entryPoint;
                info.compCompHnd->getReadyToRunDelegateCtorHelper(ldftnToken, clsHnd, &pLookup);
                if (!pLookup.lookupKind.needsRuntimeLookup)
                {
                    helperArgs = gtNewCallArgs(thisPointer, targetObjPointers);
                    entryPoint = pLookup.constLookup;
                }
                else
                {
                    assert(oper != GT_FTN_ADDR);
                    CORINFO_CONST_LOOKUP genericLookup;
                    info.compCompHnd->getReadyToRunHelper(ldftnToken, &pLookup.lookupKind,
                                                          CORINFO_HELP_READYTORUN_GENERIC_HANDLE, &genericLookup);
                    GenTree* ctxTree = getRuntimeContextTree(pLookup.lookupKind.runtimeLookupKind);
                    helperArgs       = gtNewCallArgs(thisPointer, targetObjPointers, ctxTree);
                    entryPoint       = genericLookup;
                }
                call = gtNewHelperCallNode(CORINFO_HELP_READYTORUN_DELEGATE_CTOR, TYP_VOID, helperArgs);
                call->setEntryPoint(entryPoint);
            }
            else
            {
                JITDUMP("not optimized, CORERT no ldftnToken\n");
            }
        }
        // ReadyToRun has this optimization for a non-virtual function pointers only for now.
        else if (oper == GT_FTN_ADDR)
        {
            JITDUMP("optimized\n");

            GenTree*          thisPointer       = call->gtCallThisArg->GetNode();
            GenTree*          targetObjPointers = call->gtCallArgs->GetNode();
            GenTreeCall::Use* helperArgs        = gtNewCallArgs(thisPointer, targetObjPointers);

            call = gtNewHelperCallNode(CORINFO_HELP_READYTORUN_DELEGATE_CTOR, TYP_VOID, helperArgs);

            CORINFO_LOOKUP entryPoint;
            info.compCompHnd->getReadyToRunDelegateCtorHelper(ldftnToken, clsHnd, &entryPoint);
            assert(!entryPoint.lookupKind.needsRuntimeLookup);
            call->setEntryPoint(entryPoint.constLookup);
        }
        else
        {
            JITDUMP("not optimized, R2R virtual case\n");
        }
    }
    else
#endif
        if (targetMethodHnd != nullptr)
    {
        CORINFO_METHOD_HANDLE alternateCtor = nullptr;
        DelegateCtorArgs      ctorData;
        ctorData.pMethod = info.compMethodHnd;
        ctorData.pArg3   = nullptr;
        ctorData.pArg4   = nullptr;
        ctorData.pArg5   = nullptr;

        alternateCtor = info.compCompHnd->GetDelegateCtor(methHnd, clsHnd, targetMethodHnd, &ctorData);
        if (alternateCtor != methHnd)
        {
            JITDUMP("optimized\n");
            // we erase any inline info that may have been set for generics has it is not needed here,
            // and in fact it will pass the wrong info to the inliner code
            *ExactContextHnd = nullptr;

            call->gtCallMethHnd = alternateCtor;

            noway_assert(call->gtCallArgs->GetNext()->GetNext() == nullptr);
            GenTreeCall::Use* addArgs = nullptr;
            if (ctorData.pArg5)
            {
                GenTree* arg5 = gtNewIconHandleNode(size_t(ctorData.pArg5), GTF_ICON_FTN_ADDR);
                addArgs       = gtPrependNewCallArg(arg5, addArgs);
            }
            if (ctorData.pArg4)
            {
                GenTree* arg4 = gtNewIconHandleNode(size_t(ctorData.pArg4), GTF_ICON_FTN_ADDR);
                addArgs       = gtPrependNewCallArg(arg4, addArgs);
            }
            if (ctorData.pArg3)
            {
                GenTree* arg3 = gtNewIconHandleNode(size_t(ctorData.pArg3), GTF_ICON_FTN_ADDR);
                addArgs       = gtPrependNewCallArg(arg3, addArgs);
            }
            call->gtCallArgs->GetNext()->SetNext(addArgs);
        }
        else
        {
            JITDUMP("not optimized, no alternate ctor\n");
        }
    }
    else
    {
        JITDUMP("not optimized, no target method\n");
    }
    return call;
}

/*****************************************************************************
 *
 *  Mark whether the edge "srcBB -> dstBB" forms a loop that will always
 *  execute a call or not.
 */

inline void Compiler::fgLoopCallTest(BasicBlock* srcBB, BasicBlock* dstBB)
{
    /* Bail if this is not a backward edge */

    if (srcBB->bbNum < dstBB->bbNum)
    {
        return;
    }

    /* Unless we already know that there is a loop without a call here ... */

    if (!(dstBB->bbFlags & BBF_LOOP_CALL0))
    {
        /* Check whether there is a loop path that doesn't call */

        if (optReachWithoutCall(dstBB, srcBB))
        {
            dstBB->bbFlags |= BBF_LOOP_CALL0;
            dstBB->bbFlags &= ~BBF_LOOP_CALL1;
        }
        else
        {
            dstBB->bbFlags |= BBF_LOOP_CALL1;
        }
    }
}

/*****************************************************************************
 *
 *  Mark which loops are guaranteed to execute a call.
 */

void Compiler::fgLoopCallMark()
{
    /* If we've already marked all the block, bail */

    if (fgLoopCallMarked)
    {
        return;
    }

    fgLoopCallMarked = true;

    /* Walk the blocks, looking for backward edges */

    for (BasicBlock* const block : Blocks())
    {
        switch (block->bbJumpKind)
        {
            case BBJ_COND:
            case BBJ_CALLFINALLY:
            case BBJ_ALWAYS:
            case BBJ_EHCATCHRET:
                fgLoopCallTest(block, block->bbJumpDest);
                break;

            case BBJ_SWITCH:
                for (BasicBlock* const bTarget : block->SwitchTargets())
                {
                    fgLoopCallTest(block, bTarget);
                }
                break;

            default:
                break;
        }
    }
}

/*****************************************************************************
 *
 *  Note the fact that the given block is a loop header.
 */

inline void Compiler::fgMarkLoopHead(BasicBlock* block)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("fgMarkLoopHead: Checking loop head block " FMT_BB ": ", block->bbNum);
    }
#endif

    /* Have we decided to generate fully interruptible code already? */

    if (GetInterruptible())
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("method is already fully interruptible\n");
        }
#endif
        return;
    }

    /* Is the loop head block known to execute a method call? */

    if (block->bbFlags & BBF_GC_SAFE_POINT)
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("this block will execute a call\n");
        }
#endif
        return;
    }

    /* Are dominator sets available? */

    if (fgDomsComputed)
    {
        /* Make sure that we know which loops will always execute calls */

        if (!fgLoopCallMarked)
        {
            fgLoopCallMark();
        }

        /* Will every trip through our loop execute a call? */

        if (block->bbFlags & BBF_LOOP_CALL1)
        {
#ifdef DEBUG
            if (verbose)
            {
                printf("this block dominates a block that will execute a call\n");
            }
#endif
            return;
        }
    }

    /*
     *  We have to make this method fully interruptible since we can not
     *  ensure that this loop will execute a call every time it loops.
     *
     *  We'll also need to generate a full register map for this method.
     */

    assert(!codeGen->isGCTypeFixed());

    if (!compCanEncodePtrArgCntMax())
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("a callsite with more than 1023 pushed args exists\n");
        }
#endif
        return;
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("no guaranteed callsite exits, marking method as fully interruptible\n");
    }
#endif
    SetInterruptible(true);
}

GenTree* Compiler::fgGetCritSectOfStaticMethod()
{
    noway_assert(!compIsForInlining());

    noway_assert(info.compIsStatic); // This method should only be called for static methods.

    GenTree* tree = nullptr;

    CORINFO_LOOKUP_KIND kind;
    info.compCompHnd->getLocationOfThisType(info.compMethodHnd, &kind);

    if (!kind.needsRuntimeLookup)
    {
        void *critSect = nullptr, **pCrit = nullptr;
        critSect = info.compCompHnd->getMethodSync(info.compMethodHnd, (void**)&pCrit);
        noway_assert((!critSect) != (!pCrit));

        tree = gtNewIconEmbHndNode(critSect, pCrit, GTF_ICON_METHOD_HDL, info.compMethodHnd);
    }
    else
    {
        // Collectible types requires that for shared generic code, if we use the generic context paramter
        // that we report it. (This is a conservative approach, we could detect some cases particularly when the
        // context parameter is this that we don't need the eager reporting logic.)
        lvaGenericsContextInUse = true;

        switch (kind.runtimeLookupKind)
        {
            case CORINFO_LOOKUP_THISOBJ:
            {
                noway_assert(!"Should never get this for static method.");
                break;
            }

            case CORINFO_LOOKUP_CLASSPARAM:
            {
                // In this case, the hidden param is the class handle.
                tree = gtNewLclvNode(info.compTypeCtxtArg, TYP_I_IMPL);
                tree->gtFlags |= GTF_VAR_CONTEXT;
                break;
            }

            case CORINFO_LOOKUP_METHODPARAM:
            {
                // In this case, the hidden param is the method handle.
                tree = gtNewLclvNode(info.compTypeCtxtArg, TYP_I_IMPL);
                tree->gtFlags |= GTF_VAR_CONTEXT;
                // Call helper CORINFO_HELP_GETCLASSFROMMETHODPARAM to get the class handle
                // from the method handle.
                tree = gtNewHelperCallNode(CORINFO_HELP_GETCLASSFROMMETHODPARAM, TYP_I_IMPL, gtNewCallArgs(tree));
                break;
            }

            default:
            {
                noway_assert(!"Unknown LOOKUP_KIND");
                break;
            }
        }

        noway_assert(tree); // tree should now contain the CORINFO_CLASS_HANDLE for the exact class.

        // Given the class handle, get the pointer to the Monitor.
        tree = gtNewHelperCallNode(CORINFO_HELP_GETSYNCFROMCLASSHANDLE, TYP_I_IMPL, gtNewCallArgs(tree));
    }

    noway_assert(tree);
    return tree;
}

#if defined(FEATURE_EH_FUNCLETS)

/*****************************************************************************
 *
 *  Add monitor enter/exit calls for synchronized methods, and a try/fault
 *  to ensure the 'exit' is called if the 'enter' was successful. On x86, we
 *  generate monitor enter/exit calls and tell the VM the code location of
 *  these calls. When an exception occurs between those locations, the VM
 *  automatically releases the lock. For non-x86 platforms, the JIT is
 *  responsible for creating a try/finally to protect the monitor enter/exit,
 *  and the VM doesn't need to know anything special about the method during
 *  exception processing -- it's just a normal try/finally.
 *
 *  We generate the following code:
 *
 *      void Foo()
 *      {
 *          unsigned byte acquired = 0;
 *          try {
 *              JIT_MonEnterWorker(<lock object>, &acquired);
 *
 *              *** all the preexisting user code goes here ***
 *
 *              JIT_MonExitWorker(<lock object>, &acquired);
 *          } fault {
 *              JIT_MonExitWorker(<lock object>, &acquired);
 *         }
 *      L_return:
 *         ret
 *      }
 *
 *  If the lock is actually acquired, then the 'acquired' variable is set to 1
 *  by the helper call. During normal exit, the finally is called, 'acquired'
 *  is 1, and the lock is released. If an exception occurs before the lock is
 *  acquired, but within the 'try' (extremely unlikely, but possible), 'acquired'
 *  will be 0, and the monitor exit call will quickly return without attempting
 *  to release the lock. Otherwise, 'acquired' will be 1, and the lock will be
 *  released during exception processing.
 *
 *  For synchronized methods, we generate a single return block.
 *  We can do this without creating additional "step" blocks because "ret" blocks
 *  must occur at the top-level (of the original code), not nested within any EH
 *  constructs. From the CLI spec, 12.4.2.8.2.3 "ret": "Shall not be enclosed in any
 *  protected block, filter, or handler." Also, 3.57: "The ret instruction cannot be
 *  used to transfer control out of a try, filter, catch, or finally block. From within
 *  a try or catch, use the leave instruction with a destination of a ret instruction
 *  that is outside all enclosing exception blocks."
 *
 *  In addition, we can add a "fault" at the end of a method and be guaranteed that no
 *  control falls through. From the CLI spec, section 12.4 "Control flow": "Control is not
 *  permitted to simply fall through the end of a method. All paths shall terminate with one
 *  of these instructions: ret, throw, jmp, or (tail. followed by call, calli, or callvirt)."
 *
 *  We only need to worry about "ret" and "throw", as the CLI spec prevents any other
 *  alternatives. Section 15.4.3.3 "Implementation information" states about exiting
 *  synchronized methods: "Exiting a synchronized method using a tail. call shall be
 *  implemented as though the tail. had not been specified." Section 3.37 "jmp" states:
 *  "The jmp instruction cannot be used to transferred control out of a try, filter,
 *  catch, fault or finally block; or out of a synchronized region." And, "throw" will
 *  be handled naturally; no additional work is required.
 */

void Compiler::fgAddSyncMethodEnterExit()
{
    assert((info.compFlags & CORINFO_FLG_SYNCH) != 0);

    // We need to do this transformation before funclets are created.
    assert(!fgFuncletsCreated);

    // Assume we don't need to update the bbPreds lists.
    assert(!fgComputePredsDone);

#if !FEATURE_EH
    // If we don't support EH, we can't add the EH needed by synchronized methods.
    // Of course, we could simply ignore adding the EH constructs, since we don't
    // support exceptions being thrown in this mode, but we would still need to add
    // the monitor enter/exit, and that doesn't seem worth it for this minor case.
    // By the time EH is working, we can just enable the whole thing.
    NYI("No support for synchronized methods");
#endif // !FEATURE_EH

    // Create a scratch first BB where we can put the new variable initialization.
    // Don't put the scratch BB in the protected region.

    fgEnsureFirstBBisScratch();
    assert(fgFirstBB->bbJumpKind == BBJ_NONE);

    // Create a block for the start of the try region, where the monitor enter call
    // will go.

    BasicBlock* tryBegBB  = fgNewBBafter(BBJ_NONE, fgFirstBB, false);
    BasicBlock* tryNextBB = tryBegBB->bbNext;
    BasicBlock* tryLastBB = fgLastBB;

    // If we have profile data the new block will inherit the next block's weight
    if (tryNextBB->hasProfileWeight())
    {
        tryBegBB->inheritWeight(tryNextBB);
    }

    // Create a block for the fault.

    assert(!tryLastBB->bbFallsThrough());
    BasicBlock* faultBB = fgNewBBafter(BBJ_EHFINALLYRET, tryLastBB, false);

    assert(tryLastBB->bbNext == faultBB);
    assert(faultBB->bbNext == nullptr);
    assert(faultBB == fgLastBB);

    { // Scope the EH region creation

        // Add the new EH region at the end, since it is the least nested,
        // and thus should be last.

        EHblkDsc* newEntry;
        unsigned  XTnew = compHndBBtabCount;

        newEntry = fgAddEHTableEntry(XTnew);

        // Initialize the new entry

        newEntry->ebdHandlerType = EH_HANDLER_FAULT;

        newEntry->ebdTryBeg  = tryBegBB;
        newEntry->ebdTryLast = tryLastBB;

        newEntry->ebdHndBeg  = faultBB;
        newEntry->ebdHndLast = faultBB;

        newEntry->ebdTyp = 0; // unused for fault

        newEntry->ebdEnclosingTryIndex = EHblkDsc::NO_ENCLOSING_INDEX;
        newEntry->ebdEnclosingHndIndex = EHblkDsc::NO_ENCLOSING_INDEX;

        newEntry->ebdTryBegOffset    = tryBegBB->bbCodeOffs;
        newEntry->ebdTryEndOffset    = tryLastBB->bbCodeOffsEnd;
        newEntry->ebdFilterBegOffset = 0;
        newEntry->ebdHndBegOffset    = 0; // handler doesn't correspond to any IL
        newEntry->ebdHndEndOffset    = 0; // handler doesn't correspond to any IL

        // Set some flags on the new region. This is the same as when we set up
        // EH regions in fgFindBasicBlocks(). Note that the try has no enclosing
        // handler, and the fault has no enclosing try.

        tryBegBB->bbFlags |= BBF_DONT_REMOVE | BBF_TRY_BEG | BBF_IMPORTED;

        faultBB->bbFlags |= BBF_DONT_REMOVE | BBF_IMPORTED;
        faultBB->bbCatchTyp = BBCT_FAULT;

        tryBegBB->setTryIndex(XTnew);
        tryBegBB->clearHndIndex();

        faultBB->clearTryIndex();
        faultBB->setHndIndex(XTnew);

        // Walk the user code blocks and set all blocks that don't already have a try handler
        // to point to the new try handler.

        BasicBlock* tmpBB;
        for (tmpBB = tryBegBB->bbNext; tmpBB != faultBB; tmpBB = tmpBB->bbNext)
        {
            if (!tmpBB->hasTryIndex())
            {
                tmpBB->setTryIndex(XTnew);
            }
        }

        // Walk the EH table. Make every EH entry that doesn't already have an enclosing
        // try index mark this new entry as their enclosing try index.

        unsigned  XTnum;
        EHblkDsc* HBtab;

        for (XTnum = 0, HBtab = compHndBBtab; XTnum < XTnew; XTnum++, HBtab++)
        {
            if (HBtab->ebdEnclosingTryIndex == EHblkDsc::NO_ENCLOSING_INDEX)
            {
                HBtab->ebdEnclosingTryIndex =
                    (unsigned short)XTnew; // This EH region wasn't previously nested, but now it is.
            }
        }

#ifdef DEBUG
        if (verbose)
        {
            JITDUMP("Synchronized method - created additional EH descriptor EH#%u for try/fault wrapping monitor "
                    "enter/exit\n",
                    XTnew);
            fgDispBasicBlocks();
            fgDispHandlerTab();
        }

        fgVerifyHandlerTab();
#endif // DEBUG
    }

    // Add monitor enter/exit calls.

    lvaMonAcquired = lvaNewTemp(TYP_INT, true DEBUGARG("monitor 'acquired' temp"));

    GenTreeOp* init = gtNewAssignNode(gtNewLclvNode(lvaMonAcquired, TYP_INT), gtNewIconNode(0));
    fgNewStmtAtEnd(fgFirstBB, init);
    JITDUMPTREE(init, "\nSynchronized method - Add 'acquired' initialization in first block " FMT_BB "\n",
                fgFirstBB->bbNum);

    // Make a copy of the 'this' pointer to be used in the handler so it does
    // not inhibit enregistration of all uses of the variable.
    unsigned thisCopyLclNum = 0;

    if (!info.compIsStatic)
    {
        thisCopyLclNum = lvaNewTemp(TYP_REF, true DEBUGARG("monitor EH exit 'this' copy"));
        init = gtNewAssignNode(gtNewLclvNode(thisCopyLclNum, TYP_REF), gtNewLclvNode(info.compThisArg, TYP_REF));
        fgNewStmtAtEnd(tryBegBB, init);
    }

    CorInfoHelpFunc enterHelper = info.compIsStatic ? CORINFO_HELP_MON_ENTER_STATIC : CORINFO_HELP_MON_ENTER;
    CorInfoHelpFunc exitHelper  = info.compIsStatic ? CORINFO_HELP_MON_EXIT_STATIC : CORINFO_HELP_MON_EXIT;

    fgInsertMonitorCall(tryBegBB, enterHelper, info.compThisArg);
    fgInsertMonitorCall(faultBB, exitHelper, thisCopyLclNum);

    for (BasicBlock* const block : Blocks())
    {
        if (block->bbJumpKind == BBJ_RETURN)
        {
            fgInsertMonitorCall(block, exitHelper, info.compThisArg);
        }
    }
}

void Compiler::fgInsertMonitorCall(BasicBlock* block, CorInfoHelpFunc helper, unsigned thisLclNum)
{
    assert((block->bbJumpKind == BBJ_NONE) || (block->bbJumpKind == BBJ_RETURN) ||
           (block->bbJumpKind == BBJ_EHFINALLYRET));

    GenTree* monitor  = info.compIsStatic ? fgGetCritSectOfStaticMethod() : gtNewLclvNode(thisLclNum, TYP_REF);
    GenTree* acquired = gtNewLclVarAddrNode(lvaMonAcquired);
    GenTree* call     = gtNewHelperCallNode(helper, TYP_VOID, gtNewCallArgs(monitor, acquired));

    JITDUMPTREE(call, "\nSynchronized method - Add monitor call to block " FMT_BB "\n", block->bbNum);

    if ((block->bbJumpKind != BBJ_RETURN) || !block->lastStmt()->GetRootNode()->OperIs(GT_RETURN))
    {
        fgNewStmtAtEnd(block, call);
        return;
    }

    Statement* retStmt = block->lastStmt();
    GenTree*   retNode = retStmt->GetRootNode();

    if (!retNode->TypeIs(TYP_VOID))
    {
        GenTree* retExpr = retNode->AsUnOp()->GetOp(0);

        // Anything with side effects needs to stay inside the synchronized region.
        // That should include address exposed locals, even if the chance that such
        // locals are used by another thread is slim. But we haven't yet dermined
        // which locals are address exposed so GTF_GLOB_REF may not be present,
        // fall back to address taken.

        if ((retExpr->GetSideEffects() != 0) || impHasAddressTakenLocals(retExpr))
        {
            unsigned   retTempLclNum = lvaGrabTemp(true DEBUGARG("monitor 'return' temp"));
            LclVarDsc* retTempLcl    = lvaGetDesc(retTempLclNum);

            if (varTypeIsStruct(retNode->GetType()))
            {
                lvaSetStruct(retTempLclNum, info.GetRetLayout(), /* checkUnsafeBuffer */ false);
            }
            else
            {
                retTempLcl->SetType(retNode->GetType());
            }

            GenTreeOp* retTempInit = gtNewAssignNode(gtNewLclvNode(retTempLclNum, retTempLcl->GetType()), retExpr);
            fgInsertStmtBefore(block, retStmt, gtNewStmt(retTempInit));
            retNode->AsUnOp()->SetOp(0, gtNewLclvNode(retTempLclNum, retTempLcl->GetType()));
        }
    }

    fgInsertStmtBefore(block, retStmt, gtNewStmt(call));
}

// Convert a BBJ_RETURN block in a synchronized method to a BBJ_ALWAYS.
// We've previously added a 'try' block around the original program code using fgAddSyncMethodEnterExit().
// Thus, we put BBJ_RETURN blocks inside a 'try'. In IL this is illegal. Instead, we would
// see a 'leave' inside a 'try' that would get transformed into BBJ_CALLFINALLY/BBJ_ALWAYS blocks
// during importing, and the BBJ_ALWAYS would point at an outer block with the BBJ_RETURN.
// Here, we mimic some of the logic of importing a LEAVE to get the same effect for synchronized methods.
void Compiler::fgConvertSyncReturnToLeave(BasicBlock* block)
{
    assert(!fgFuncletsCreated);
    assert(info.compFlags & CORINFO_FLG_SYNCH);
    assert(genReturnBB != nullptr);
    assert(genReturnBB != block);
    assert(fgReturnCount <= 1); // We have a single return for synchronized methods
    assert(block->bbJumpKind == BBJ_RETURN);
    assert((block->bbFlags & BBF_HAS_JMP) == 0);
    assert(block->hasTryIndex());
    assert(!block->hasHndIndex());
    assert(compHndBBtabCount >= 1);

    unsigned tryIndex = block->getTryIndex();
    assert(tryIndex == compHndBBtabCount - 1); // The BBJ_RETURN must be at the top-level before we inserted the
                                               // try/finally, which must be the last EH region.

    EHblkDsc* ehDsc = ehGetDsc(tryIndex);
    assert(ehDsc->ebdEnclosingTryIndex ==
           EHblkDsc::NO_ENCLOSING_INDEX); // There are no enclosing regions of the BBJ_RETURN block
    assert(ehDsc->ebdEnclosingHndIndex == EHblkDsc::NO_ENCLOSING_INDEX);

    // Convert the BBJ_RETURN to BBJ_ALWAYS, jumping to genReturnBB.
    block->bbJumpKind = BBJ_ALWAYS;
    block->bbJumpDest = genReturnBB;
    fgAddRefPred(genReturnBB, block);

#ifdef DEBUG
    if (verbose)
    {
        printf("Synchronized method - convert block " FMT_BB " to BBJ_ALWAYS [targets " FMT_BB "]\n", block->bbNum,
               block->bbJumpDest->bbNum);
    }
#endif
}

#endif // FEATURE_EH_FUNCLETS

void Compiler::fgAddReversePInvokeEnterExit()
{
    assert(opts.IsReversePInvoke());

    lvaReversePInvokeFrameVar = lvaGrabTempWithImplicitUse(false DEBUGARG("Reverse Pinvoke FrameVar"));
    lvaGetDesc(lvaReversePInvokeFrameVar)->SetBlockType(eeGetEEInfo()->sizeOfReversePInvokeFrame);

    // Add enter pinvoke exit callout at the start of prolog

    GenTree*        pInvokeFrameVar = gtNewLclVarAddrNode(lvaReversePInvokeFrameVar);
    CorInfoHelpFunc reversePInvokeEnterHelper;

    GenTreeCall::Use* args;

    if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_TRACK_TRANSITIONS))
    {
        reversePInvokeEnterHelper = CORINFO_HELP_JIT_REVERSE_PINVOKE_ENTER_TRACK_TRANSITIONS;

        GenTree* stubArgument;
        if (info.compPublishStubParam)
        {
            // If we have a secret param for a Reverse P/Invoke, that means that we are in an IL stub.
            // In this case, the method handle we pass down to the Reverse P/Invoke helper should be
            // the target method, which is passed in the secret parameter.
            stubArgument = gtNewLclvNode(lvaStubArgumentVar, TYP_I_IMPL);
        }
        else
        {
            stubArgument = gtNewIconNode(0, TYP_I_IMPL);
        }

        args = gtNewCallArgs(pInvokeFrameVar, gtNewIconEmbMethHndNode(info.compMethodHnd), stubArgument);
    }
    else
    {
        reversePInvokeEnterHelper = CORINFO_HELP_JIT_REVERSE_PINVOKE_ENTER;
        args                      = gtNewCallArgs(pInvokeFrameVar);
    }

    GenTree* tree = gtNewHelperCallNode(reversePInvokeEnterHelper, TYP_VOID, args);

    fgEnsureFirstBBisScratch();

    fgNewStmtAtBeg(fgFirstBB, tree);

#ifdef DEBUG
    if (verbose)
    {
        printf("\nReverse PInvoke method - Add reverse pinvoke enter in first basic block %s\n",
               fgFirstBB->dspToString());
        gtDispTree(tree);
        printf("\n");
    }
#endif

    // Add reverse pinvoke exit callout at the end of epilog

    tree = gtNewLclVarAddrNode(lvaReversePInvokeFrameVar);

    CorInfoHelpFunc reversePInvokeExitHelper = opts.jitFlags->IsSet(JitFlags::JIT_FLAG_TRACK_TRANSITIONS)
                                                   ? CORINFO_HELP_JIT_REVERSE_PINVOKE_EXIT_TRACK_TRANSITIONS
                                                   : CORINFO_HELP_JIT_REVERSE_PINVOKE_EXIT;

    tree = gtNewHelperCallNode(reversePInvokeExitHelper, TYP_VOID, gtNewCallArgs(tree));

    assert(genReturnBB != nullptr);

    fgNewStmtNearEnd(genReturnBB, tree);

#ifdef DEBUG
    if (verbose)
    {
        printf("\nReverse PInvoke method - Add reverse pinvoke exit in return basic block %s\n",
               genReturnBB->dspToString());
        gtDispTree(tree);
        printf("\n");
    }
#endif
}

namespace
{
// Define a helper class for merging return blocks (which we do when the input has
// more than the limit for this configuration).
//
// Notes: sets fgReturnCount, genReturnBB, and genReturnLocal.
class MergedReturns
{
public:
#ifdef JIT32_GCENCODER

    // X86 GC encoding has a hard limit of SET_EPILOGCNT_MAX epilogs.
    const static unsigned ReturnCountHardLimit = SET_EPILOGCNT_MAX;
#else  // JIT32_GCENCODER

    // We currently apply a hard limit of '4' to all other targets (see
    // the other uses of SET_EPILOGCNT_MAX), though it would be good
    // to revisit that decision based on CQ analysis.
    const static unsigned ReturnCountHardLimit = 4;
#endif // JIT32_GCENCODER

private:
    Compiler* comp;

    // As we discover returns, we'll record them in `returnBlocks`, until
    // the limit is reached, at which point we'll keep track of the merged
    // return blocks in `returnBlocks`.
    BasicBlock* returnBlocks[ReturnCountHardLimit];

    // Each constant value returned gets its own merged return block that
    // returns that constant (up to the limit on number of returns); in
    // `returnConstants` we track the constant values returned by these
    // merged constant return blocks.
    INT64 returnConstants[ReturnCountHardLimit];

    // Indicators of where in the lexical block list we'd like to place
    // each constant return block.
    BasicBlock* insertionPoints[ReturnCountHardLimit];

    // Number of return blocks allowed
    PhasedVar<unsigned> maxReturns;

    // Flag to keep track of when we've hit the limit of returns and are
    // actively merging returns together.
    bool mergingReturns = false;

public:
    MergedReturns(Compiler* comp) : comp(comp)
    {
        comp->fgReturnCount = 0;
    }

    void SetMaxReturns(unsigned value)
    {
        maxReturns = value;
        maxReturns.MarkAsReadOnly();
    }

    //------------------------------------------------------------------------
    // Record: Make note of a return block in the input program.
    //
    // Arguments:
    //    returnBlock - Block in the input that has jump kind BBJ_RETURN
    //
    // Notes:
    //    Updates fgReturnCount appropriately, and generates a merged return
    //    block if necessary.  If a constant merged return block is used,
    //    `returnBlock` is rewritten to jump to it.  If a non-constant return
    //    block is used, `genReturnBB` is set to that block, and `genReturnLocal`
    //    is set to the lclvar that it returns; morph will need to rewrite
    //    `returnBlock` to set the local and jump to the return block in such
    //    cases, which it will do after some key transformations like rewriting
    //    tail calls and calls that return to hidden buffers.  In either of these
    //    cases, `fgReturnCount` and the merged return block's profile information
    //    will be updated to reflect or anticipate the rewrite of `returnBlock`.
    //
    void Record(BasicBlock* returnBlock)
    {
        // Add this return to our tally
        unsigned oldReturnCount = comp->fgReturnCount++;

        if (!mergingReturns)
        {
            if (oldReturnCount < maxReturns)
            {
                // No need to merge just yet; simply record this return.
                returnBlocks[oldReturnCount] = returnBlock;
                return;
            }

            // We'e reached our threshold
            mergingReturns = true;

            // Merge any returns we've already identified
            for (unsigned i = 0, searchLimit = 0; i < oldReturnCount; ++i)
            {
                BasicBlock* mergedReturnBlock = Merge(returnBlocks[i], searchLimit);
                if (returnBlocks[searchLimit] == mergedReturnBlock)
                {
                    // We've added a new block to the searchable set
                    ++searchLimit;
                }
            }
        }

        // We have too many returns, so merge this one in.
        // Search limit is new return count minus one (to exclude this block).
        unsigned searchLimit = comp->fgReturnCount - 1;
        Merge(returnBlock, searchLimit);
    }

    //------------------------------------------------------------------------
    // EagerCreate: Force creation of a non-constant merged return block `genReturnBB`.
    //
    // Return Value:
    //    The newly-created block which returns `genReturnLocal`.
    //
    BasicBlock* EagerCreate()
    {
        mergingReturns = true;
        return Merge(nullptr, 0);
    }

    //------------------------------------------------------------------------
    // PlaceReturns: Move any generated const return blocks to an appropriate
    //     spot in the lexical block list.
    //
    // Notes:
    //    The goal is to set things up favorably for a reasonable layout without
    //    putting too much burden on fgReorderBlocks; in particular, since that
    //    method doesn't (currently) shuffle non-profile, non-rare code to create
    //    fall-through and reduce gotos, this method places each const return
    //    block immediately after its last predecessor, so that the flow from
    //    there to it can become fallthrough without requiring any motion to be
    //    performed by fgReorderBlocks.
    //
    void PlaceReturns()
    {
        if (!mergingReturns)
        {
            // No returns generated => no returns to place.
            return;
        }

        for (unsigned index = 0; index < comp->fgReturnCount; ++index)
        {
            BasicBlock* returnBlock    = returnBlocks[index];
            BasicBlock* genReturnBlock = comp->genReturnBB;
            if (returnBlock == genReturnBlock)
            {
                continue;
            }

            BasicBlock* insertionPoint = insertionPoints[index];
            assert(insertionPoint != nullptr);

            comp->fgUnlinkBlock(returnBlock);
            comp->fgMoveBlocksAfter(returnBlock, returnBlock, insertionPoint);
            // Treat the merged return block as belonging to the same EH region
            // as the insertion point block, to make sure we don't break up
            // EH regions; since returning a constant won't throw, this won't
            // affect program behavior.
            comp->fgExtendEHRegionAfter(insertionPoint);
        }
    }

private:
    //------------------------------------------------------------------------
    // CreateReturnBB: Create a basic block to serve as a merged return point, stored to
    //    `returnBlocks` at the given index, and optionally returning the given constant.
    //
    // Arguments:
    //    index - Index into `returnBlocks` to store the new block into.
    //    returnConst - Constant that the new block should return; may be nullptr to
    //      indicate that the new merged return is for the non-constant case, in which
    //      case, if the method's return type is non-void, `comp->genReturnLocal` will
    //      be initialized to a new local of the appropriate type, and the new block will
    //      return it.
    //
    // Return Value:
    //    The new merged return block.
    //
    BasicBlock* CreateReturnBB(unsigned index, GenTreeIntConCommon* returnConst = nullptr)
    {
        BasicBlock* newReturnBB = comp->fgNewBBinRegion(BBJ_RETURN);
        newReturnBB->bbRefs     = 1; // bbRefs gets update later, for now it should be 1
        comp->fgReturnCount++;

        noway_assert(newReturnBB->bbNext == nullptr);

        JITDUMP("\n newReturnBB [" FMT_BB "] created\n", newReturnBB->bbNum);

        GenTree* returnExpr;

        if (comp->info.retDesc.GetRegCount() == 0)
        {
            noway_assert((comp->info.compRetType == TYP_VOID) || varTypeIsStruct(comp->info.compRetType));

            returnExpr = new (comp, GT_RETURN) GenTreeOp(GT_RETURN, TYP_VOID);

            comp->genReturnLocal = BAD_VAR_NUM;
        }
        else if (returnConst != nullptr)
        {
            returnExpr = comp->gtNewOperNode(GT_RETURN, returnConst->GetType(), returnConst);

            returnConstants[index] = returnConst->IntegralValue();
        }
        else if (comp->info.compRetBuffArg != BAD_VAR_NUM)
        {
            assert(comp->info.retDesc.GetRegCount() == 1);

            GenTree* retBuffAddr = comp->gtNewLclvNode(comp->info.compRetBuffArg, TYP_BYREF);
            retBuffAddr->gtFlags |= GTF_DONT_CSE;
            returnExpr = comp->gtNewOperNode(GT_RETURN, TYP_BYREF, retBuffAddr);

            comp->genReturnLocal = BAD_VAR_NUM;
        }
        else
        {
            // There is a return value, so create a temp for it.  Real returns will store the value in there and
            // it'll be reloaded by the single return.
            unsigned   lclNum = comp->lvaGrabTemp(true DEBUGARG("merged return temp"));
            LclVarDsc* lcl    = comp->lvaGetDesc(lclNum);

            if (varTypeIsStruct(comp->info.GetRetSigType()))
            {
                comp->lvaSetStruct(lclNum, comp->info.GetRetLayout(), false);
                lcl->lvIsMultiRegRet = (comp->info.retDesc.GetRegCount() > 1);
            }
            else
            {
                lcl->SetType(varActualType(comp->info.compRetType));
                comp->compFloatingPointUsed |= varTypeIsFloating(comp->info.compRetType);
            }

            GenTree* retTemp = comp->gtNewLclvNode(lclNum, lcl->GetType());
            // make sure copy prop ignores this node (make sure it always does a reload from the temp).
            retTemp->gtFlags |= GTF_DONT_CSE;
            returnExpr = comp->gtNewOperNode(GT_RETURN, lcl->GetType(), retTemp);

            comp->genReturnLocal = lclNum;
        }

        // Add 'return' expression to the return block
        comp->fgNewStmtAtEnd(newReturnBB, returnExpr);
        // Flag that this 'return' was generated by return merging so that subsequent
        // return block morhping will know to leave it alone.
        returnExpr->gtFlags |= GTF_RET_MERGED;

#ifdef DEBUG
        if (comp->verbose)
        {
            printf("\nmergeReturns statement tree ");
            Compiler::printTreeID(returnExpr);
            printf(" added to genReturnBB %s\n", newReturnBB->dspToString());
            comp->gtDispTree(returnExpr);
            printf("\n");
        }
#endif
        assert(index < maxReturns);
        returnBlocks[index] = newReturnBB;
        return newReturnBB;
    }

    //------------------------------------------------------------------------
    // Merge: Find or create an appropriate merged return block for the given input block.
    //
    // Arguments:
    //    returnBlock - Return block from the input program to find a merged return for.
    //                  May be nullptr to indicate that new block suitable for non-constant
    //                  returns should be generated but no existing block modified.
    //    searchLimit - Blocks in `returnBlocks` up to but not including index `searchLimit`
    //                  will be checked to see if we already have an appropriate merged return
    //                  block for this case.  If a new block must be created, it will be stored
    //                  to `returnBlocks` at index `searchLimit`.
    //
    // Return Value:
    //    Merged return block suitable for handling this return value.  May be newly-created
    //    or pre-existing.
    //
    // Notes:
    //    If a constant-valued merged return block is used, `returnBlock` will be rewritten to
    //    jump to the merged return block and its `GT_RETURN` statement will be removed.  If
    //    a non-constant-valued merged return block is used, `genReturnBB` and `genReturnLocal`
    //    will be set so that Morph can perform that rewrite, which it will do after some key
    //    transformations like rewriting tail calls and calls that return to hidden buffers.
    //    In either of these cases, `fgReturnCount` and the merged return block's profile
    //    information will be updated to reflect or anticipate the rewrite of `returnBlock`.
    //
    BasicBlock* Merge(BasicBlock* returnBlock, unsigned searchLimit)
    {
        assert(mergingReturns);

        BasicBlock* mergedReturnBlock = nullptr;

        // Do not look for mergable constant returns in debug codegen as
        // we may lose track of sequence points.
        if ((returnBlock != nullptr) && (maxReturns > 1) && !comp->opts.compDbgCode)
        {
            // Check to see if this is a constant return so that we can search
            // for and/or create a constant return block for it.

            GenTreeIntConCommon* retConst = GetReturnConst(returnBlock);
            if (retConst != nullptr)
            {
                // We have a constant.  Now find or create a corresponding return block.

                unsigned    index;
                BasicBlock* constReturnBlock = FindConstReturnBlock(retConst, searchLimit, &index);

                if (constReturnBlock == nullptr)
                {
                    // We didn't find a const return block.  See if we have space left
                    // to make one.

                    // We have already allocated `searchLimit` slots.
                    unsigned slotsReserved = searchLimit;
                    if (comp->genReturnBB == nullptr)
                    {
                        // We haven't made a non-const return yet, so we have to reserve
                        // a slot for one.
                        ++slotsReserved;
                    }

                    if (slotsReserved < maxReturns)
                    {
                        // We have enough space to allocate a slot for this constant.
                        constReturnBlock = CreateReturnBB(searchLimit, retConst);
                    }
                }

                if (constReturnBlock != nullptr)
                {
                    // Found a constant merged return block.
                    mergedReturnBlock = constReturnBlock;

                    // Change BBJ_RETURN to BBJ_ALWAYS targeting const return block.
                    assert((comp->info.compFlags & CORINFO_FLG_SYNCH) == 0);
                    returnBlock->bbJumpKind = BBJ_ALWAYS;
                    returnBlock->bbJumpDest = constReturnBlock;

                    // Remove GT_RETURN since constReturnBlock returns the constant.
                    assert(returnBlock->lastStmt()->GetRootNode()->OperIs(GT_RETURN));
                    assert(returnBlock->lastStmt()->GetRootNode()->gtGetOp1()->IsIntegralConst());
                    comp->fgRemoveStmt(returnBlock, returnBlock->lastStmt());

                    // Using 'returnBlock' as the insertion point for 'mergedReturnBlock'
                    // will give it a chance to use fallthrough rather than BBJ_ALWAYS.
                    // Resetting this after each merge ensures that any branches to the
                    // merged return block are lexically forward.

                    insertionPoints[index] = returnBlock;

                    // Update profile information in the mergedReturnBlock to
                    // reflect the additional flow.
                    //
                    if (returnBlock->hasProfileWeight())
                    {
                        BasicBlock::weight_t const oldWeight =
                            mergedReturnBlock->hasProfileWeight() ? mergedReturnBlock->bbWeight : BB_ZERO_WEIGHT;
                        BasicBlock::weight_t const newWeight = oldWeight + returnBlock->bbWeight;

                        JITDUMP("merging profile weight " FMT_WT " from " FMT_BB " to const return " FMT_BB "\n",
                                returnBlock->bbWeight, returnBlock->bbNum, mergedReturnBlock->bbNum);

                        mergedReturnBlock->setBBProfileWeight(newWeight);
                        DISPBLOCK(mergedReturnBlock);
                    }
                }
            }
        }

        if (mergedReturnBlock == nullptr)
        {
            // No constant return block for this return; use the general one.
            // We defer flow update and profile update to morph.
            //
            mergedReturnBlock = comp->genReturnBB;
            if (mergedReturnBlock == nullptr)
            {
                // No general merged return for this function yet; create one.
                // There had better still be room left in the array.
                assert(searchLimit < maxReturns);
                mergedReturnBlock = CreateReturnBB(searchLimit);
                comp->genReturnBB = mergedReturnBlock;
                // Downstream code expects the `genReturnBB` to always remain
                // once created, so that it can redirect flow edges to it.
                mergedReturnBlock->bbFlags |= BBF_DONT_REMOVE;
            }
        }

        if (returnBlock != nullptr)
        {
            // Update fgReturnCount to reflect or anticipate that `returnBlock` will no longer
            // be a return point.
            comp->fgReturnCount--;
        }

        return mergedReturnBlock;
    }

    //------------------------------------------------------------------------
    // GetReturnConst: If the given block returns an integral constant, return the
    //     GenTreeIntConCommon that represents the constant.
    //
    // Arguments:
    //    returnBlock - Block whose return value is to be inspected.
    //
    // Return Value:
    //    GenTreeIntCommon that is the argument of `returnBlock`'s `GT_RETURN` if
    //    such exists; nullptr otherwise.
    //
    static GenTreeIntConCommon* GetReturnConst(BasicBlock* returnBlock)
    {
        Statement* lastStmt = returnBlock->lastStmt();
        if (lastStmt == nullptr)
        {
            return nullptr;
        }

        GenTree* lastExpr = lastStmt->GetRootNode();
        if (!lastExpr->OperIs(GT_RETURN))
        {
            return nullptr;
        }

        GenTree* retExpr = lastExpr->gtGetOp1();
        if ((retExpr == nullptr) || !retExpr->IsIntegralConst())
        {
            return nullptr;
        }

        return retExpr->AsIntConCommon();
    }

    //------------------------------------------------------------------------
    // FindConstReturnBlock: Scan the already-created merged return blocks, up to `searchLimit`,
    //     and return the one corresponding to the given const expression if it exists.
    //
    // Arguments:
    //    constExpr - GenTreeIntCommon representing the constant return value we're
    //        searching for.
    //    searchLimit - Check `returnBlocks`/`returnConstants` up to but not including
    //        this index.
    //    index - [out] Index of return block in the `returnBlocks` array, if found;
    //        searchLimit otherwise.
    //
    // Return Value:
    //    A block that returns the same constant, if one is found; otherwise nullptr.
    //
    BasicBlock* FindConstReturnBlock(GenTreeIntConCommon* constExpr, unsigned searchLimit, unsigned* index)
    {
        INT64 constVal = constExpr->IntegralValue();

        for (unsigned i = 0; i < searchLimit; ++i)
        {
            // Need to check both for matching const val and for genReturnBB
            // because genReturnBB is used for non-constant returns and its
            // corresponding entry in the returnConstants array is garbage.
            // Check the returnBlocks[] first, so we don't access an uninitialized
            // returnConstants[] value (which some tools like valgrind will
            // complain about).

            BasicBlock* returnBlock = returnBlocks[i];

            if (returnBlock == comp->genReturnBB)
            {
                continue;
            }

            if (returnConstants[i] == constVal)
            {
                *index = i;
                return returnBlock;
            }
        }

        *index = searchLimit;
        return nullptr;
    }
};
}

/*****************************************************************************
*
*  Add any internal blocks/trees we may need
*/

void Compiler::fgAddInternal()
{
    noway_assert(!compIsForInlining());

    // The backend requires a scratch BB into which it can safely insert a P/Invoke method prolog if one is
    // required. Similarly, we need a scratch BB for poisoning. Create it here.
    if (compMethodRequiresPInvokeFrame() || compShouldPoisonFrame())
    {
        fgEnsureFirstBBisScratch();
        fgFirstBB->bbFlags |= BBF_DONT_REMOVE;
    }

    /*
    <BUGNUM> VSW441487 </BUGNUM>

    The "this" pointer is implicitly used in the following cases:
    1. Locking of synchronized methods
    2. Dictionary access of shared generics code
    3. If a method has "catch(FooException<T>)", the EH code accesses "this" to determine T.
    4. Initializing the type from generic methods which require precise cctor semantics
    5. Verifier does special handling of "this" in the .ctor

    However, we might overwrite it with a "starg 0".
    In this case, we will redirect all "ldarg(a)/starg(a) 0" to a temp lvaTable[lvaArg0Var]
    */

    if (!info.compIsStatic)
    {
        if (lvaArg0Var != info.compThisArg)
        {
            // When we're using the general encoder, we mark compThisArg address-taken to ensure that it is not
            // enregistered (since the decoder always reports a stack location for "this" for generics
            // context vars).
            bool lva0CopiedForGenericsCtxt;
#ifndef JIT32_GCENCODER
            lva0CopiedForGenericsCtxt = ((info.compMethodInfo->options & CORINFO_GENERICS_CTXT_FROM_THIS) != 0);
#else  // JIT32_GCENCODER
            lva0CopiedForGenericsCtxt          = false;
#endif // JIT32_GCENCODER
            noway_assert(lva0CopiedForGenericsCtxt || !lvaTable[info.compThisArg].lvAddrExposed);
            noway_assert(!lvaTable[info.compThisArg].lvHasILStoreOp);
            noway_assert(lvaTable[lvaArg0Var].lvAddrExposed || lvaTable[lvaArg0Var].lvHasILStoreOp ||
                         lva0CopiedForGenericsCtxt);

            var_types thisType = lvaTable[info.compThisArg].TypeGet();

            // Now assign the original input "this" to the temp

            GenTree* tree;

            tree = gtNewLclvNode(lvaArg0Var, thisType);

            tree = gtNewAssignNode(tree,                                     // dst
                                   gtNewLclvNode(info.compThisArg, thisType) // src
                                   );

            /* Create a new basic block and stick the assignment in it */

            fgEnsureFirstBBisScratch();

            fgNewStmtAtEnd(fgFirstBB, tree);

#ifdef DEBUG
            if (verbose)
            {
                printf("\nCopy \"this\" to lvaArg0Var in first basic block %s\n", fgFirstBB->dspToString());
                gtDispTree(tree);
                printf("\n");
            }
#endif
        }
    }

    // Merge return points if required or beneficial
    MergedReturns merger(this);

#if defined(FEATURE_EH_FUNCLETS)
    // Add the synchronized method enter/exit calls and try/finally protection. Note
    // that this must happen before the one BBJ_RETURN block is created below, so the
    // BBJ_RETURN block gets placed at the top-level, not within an EH region. (Otherwise,
    // we'd have to be really careful when creating the synchronized method try/finally
    // not to include the BBJ_RETURN block.)
    if ((info.compFlags & CORINFO_FLG_SYNCH) != 0)
    {
        fgAddSyncMethodEnterExit();
    }
#endif // FEATURE_EH_FUNCLETS

    //
    //  We will generate just one epilog (return block)
    //   when we are asked to generate enter/leave callbacks
    //   or for methods with PInvoke
    //   or for methods calling into unmanaged code
    //   or for synchronized methods.
    //
    BasicBlock* lastBlockBeforeGenReturns = fgLastBB;
    if (compIsProfilerHookNeeded() || compMethodRequiresPInvokeFrame() || opts.IsReversePInvoke() ||
        ((info.compFlags & CORINFO_FLG_SYNCH) != 0))
    {
        // We will generate only one return block
        // We will transform the BBJ_RETURN blocks
        //  into jumps to the one return block
        //
        merger.SetMaxReturns(1);

        // Eagerly create the genReturnBB since the lowering of these constructs
        // will expect to find it.
        BasicBlock* mergedReturn = merger.EagerCreate();
        assert(mergedReturn == genReturnBB);
    }
    else
    {
        bool stressMerging = compStressCompile(STRESS_MERGED_RETURNS, 50);

        //
        // We are allowed to have multiple individual exits
        // However we can still decide to have a single return
        //
        if ((compCodeOpt() == SMALL_CODE) || stressMerging)
        {
            // Under stress or for Small_Code case we always
            // generate a single return block when we have multiple
            // return points
            //
            merger.SetMaxReturns(1);
        }
        else
        {
            merger.SetMaxReturns(MergedReturns::ReturnCountHardLimit);
        }
    }

    // Visit the BBJ_RETURN blocks and merge as necessary.

    for (BasicBlock* block = fgFirstBB; block != lastBlockBeforeGenReturns->bbNext; block = block->bbNext)
    {
        if ((block->bbJumpKind == BBJ_RETURN) && ((block->bbFlags & BBF_HAS_JMP) == 0))
        {
            merger.Record(block);
        }
    }

    merger.PlaceReturns();

    if (compMethodRequiresPInvokeFrame())
    {
        // The P/Invoke helpers only require a frame variable, so only allocate the
        // TCB variable if we're not using them.
        if (!opts.ShouldUsePInvokeHelpers())
        {
            info.compLvFrameListRoot = lvaNewTemp(TYP_I_IMPL, false DEBUGARG("Pinvoke FrameListRoot"));
            lvaGetDesc(info.compLvFrameListRoot)->lvImplicitlyReferenced = 1;
        }

        lvaInlinedPInvokeFrameVar = lvaGrabTempWithImplicitUse(false DEBUGARG("Pinvoke FrameVar"));
        lvaGetDesc(lvaInlinedPInvokeFrameVar)->SetBlockType(eeGetEEInfo()->inlinedCallFrameInfo.size);
    }

    // Do we need to insert a "JustMyCode" callback?

    CORINFO_JUST_MY_CODE_HANDLE* pDbgHandle = nullptr;
    CORINFO_JUST_MY_CODE_HANDLE  dbgHandle  = nullptr;
    if (opts.compDbgCode && !opts.jitFlags->IsSet(JitFlags::JIT_FLAG_IL_STUB))
    {
        dbgHandle = info.compCompHnd->getJustMyCodeHandle(info.compMethodHnd, &pDbgHandle);
    }

    noway_assert(!dbgHandle || !pDbgHandle);

    if (dbgHandle || pDbgHandle)
    {
        // Test the JustMyCode VM global state variable
        GenTree* embNode        = gtNewIconEmbHndNode(dbgHandle, pDbgHandle, GTF_ICON_GLOBAL_PTR, info.compMethodHnd);
        GenTree* guardCheckVal  = gtNewOperNode(GT_IND, TYP_INT, embNode);
        GenTree* guardCheckCond = gtNewOperNode(GT_EQ, TYP_INT, guardCheckVal, gtNewZeroConNode(TYP_INT));

        // Create the callback which will yield the final answer

        GenTree* callback = gtNewHelperCallNode(CORINFO_HELP_DBG_IS_JUST_MY_CODE, TYP_VOID);

        // Stick the conditional call at the start of the method

        fgEnsureFirstBBisScratch();
        fgNewStmtAtEnd(fgFirstBB, gtNewQmarkNode(TYP_VOID, guardCheckCond, gtNewNothingNode(), callback));
    }

#if !defined(FEATURE_EH_FUNCLETS)

    /* Is this a 'synchronized' method? */

    if (info.compFlags & CORINFO_FLG_SYNCH)
    {
        GenTree* tree = NULL;

        /* Insert the expression "enterCrit(this)" or "enterCrit(handle)" */

        if (info.compIsStatic)
        {
            tree = fgGetCritSectOfStaticMethod();

            tree = gtNewHelperCallNode(CORINFO_HELP_MON_ENTER_STATIC, TYP_VOID, gtNewCallArgs(tree));
        }
        else
        {
            noway_assert(lvaTable[info.compThisArg].lvType == TYP_REF);

            tree = gtNewLclvNode(info.compThisArg, TYP_REF);

            tree = gtNewHelperCallNode(CORINFO_HELP_MON_ENTER, TYP_VOID, gtNewCallArgs(tree));
        }

        /* Create a new basic block and stick the call in it */

        fgEnsureFirstBBisScratch();

        fgNewStmtAtEnd(fgFirstBB, tree);

#ifdef DEBUG
        if (verbose)
        {
            printf("\nSynchronized method - Add enterCrit statement in first basic block %s\n",
                   fgFirstBB->dspToString());
            gtDispTree(tree);
            printf("\n");
        }
#endif

        /* We must be generating a single exit point for this to work */

        noway_assert(genReturnBB != nullptr);

        /* Create the expression "exitCrit(this)" or "exitCrit(handle)" */

        if (info.compIsStatic)
        {
            tree = fgGetCritSectOfStaticMethod();

            tree = gtNewHelperCallNode(CORINFO_HELP_MON_EXIT_STATIC, TYP_VOID, gtNewCallArgs(tree));
        }
        else
        {
            tree = gtNewLclvNode(info.compThisArg, TYP_REF);

            tree = gtNewHelperCallNode(CORINFO_HELP_MON_EXIT, TYP_VOID, gtNewCallArgs(tree));
        }

        fgNewStmtNearEnd(genReturnBB, tree);

#ifdef DEBUG
        if (verbose)
        {
            printf("\nSynchronized method - Add exit expression ");
            printTreeID(tree);
            printf("\n");
        }
#endif

        // Reset cookies used to track start and end of the protected region in synchronized methods
        syncStartEmitCookie = NULL;
        syncEndEmitCookie   = NULL;
    }

#endif // !FEATURE_EH_FUNCLETS

    if (opts.IsReversePInvoke())
    {
        fgAddReversePInvokeEnterExit();
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("\n*************** After fgAddInternal()\n");
        fgDispBasicBlocks();
        fgDispHandlerTab();
    }
#endif
}

/*****************************************************************************/
/*****************************************************************************/

void Compiler::fgFindOperOrder()
{
#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In fgFindOperOrder()\n");
    }
#endif

    /* Walk the basic blocks and for each statement determine
     * the evaluation order, cost, FP levels, etc... */

    for (BasicBlock* const block : Blocks())
    {
        compCurBB = block;
        for (Statement* const stmt : block->Statements())
        {
            /* Recursively process the statement */

            compCurStmt = stmt;
            gtSetStmtInfo(stmt);
        }
    }
}

//------------------------------------------------------------------------
// fgSimpleLowering: do full walk of all IR, lowering selected operations
// and computing lvaOutgoingArgumentAreaSize.
//
// Notes:
//    Lowers GT_ARR_LENGTH, GT_ARR_BOUNDS_CHECK
//
//    For target ABIs with fixed out args area, computes upper bound on
//    the size of this area from the calls in the IR.
//
//    Outgoing arg area size is computed here because we want to run it
//    after optimization (in case calls are removed) and need to look at
//    all possible calls in the method.

void Compiler::fgSimpleLowering()
{
#if FEATURE_FIXED_OUT_ARGS
    unsigned outgoingArgSpaceSize = 0;
#endif // FEATURE_FIXED_OUT_ARGS

    for (BasicBlock* const block : Blocks())
    {
        compCurBB = block;

        LIR::Range& range = LIR::AsRange(block);
        for (GenTree* tree : range)
        {
            switch (tree->OperGet())
            {
                case GT_ARR_LENGTH:
                {
                    GenTree* arr = tree->AsArrLen()->GetArray();

                    noway_assert(arr->gtNext == tree);

                    GenTree* addr;

                    if (arr->IsIntegralConst(0))
                    {
                        // If the array is NULL, then we should get a NULL reference
                        // exception when computing its length.  We need to maintain
                        // an invariant where there is no sum of two constants node,
                        // so let's simply return an indirection of NULL. Also change
                        // the address to I_IMPL, there's no reason to keep the REF.

                        addr = arr;
                        addr->SetType(TYP_I_IMPL);
                    }
                    else
                    {
                        GenTree* ofs = gtNewIconNode(tree->AsArrLen()->GetLenOffs(), TYP_I_IMPL);
                        addr         = gtNewOperNode(GT_ADD, TYP_BYREF, arr, ofs);

                        range.InsertAfter(arr, ofs, addr);
                    }

                    tree->ChangeOper(GT_IND);
                    tree->AsIndir()->SetAddr(addr);

                    break;
                }

                case GT_ARR_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
                case GT_HW_INTRINSIC_CHK:
#endif
                    tree->AsBoundsChk()->SetThrowBlock(fgGetRngChkTarget(block, tree->AsBoundsChk()->GetThrowKind()));
                    break;

#if FEATURE_FIXED_OUT_ARGS
                case GT_CALL:
                {
                    GenTreeCall* call = tree->AsCall();
                    // Fast tail calls use the caller-supplied scratch
                    // space so have no impact on this method's outgoing arg size.
                    if (!call->IsFastTailCall())
                    {
                        // Update outgoing arg size to handle this call
                        const unsigned thisCallOutAreaSize =
                            max(call->GetInfo()->GetNextSlotNum() * REGSIZE_BYTES, MIN_ARG_AREA_FOR_CALL);

                        if (thisCallOutAreaSize > outgoingArgSpaceSize)
                        {
                            outgoingArgSpaceSize = thisCallOutAreaSize;
                            JITDUMP("Bumping outgoingArgSpaceSize to %u for call [%06d]\n", outgoingArgSpaceSize,
                                    dspTreeID(tree));
                        }
                        else
                        {
                            JITDUMP("outgoingArgSpaceSize %u sufficient for call [%06d], which needs %u\n",
                                    outgoingArgSpaceSize, dspTreeID(tree), thisCallOutAreaSize);
                        }
                    }
                    else
                    {
                        JITDUMP("outgoingArgSpaceSize not impacted by fast tail call [%06d]\n", dspTreeID(tree));
                    }
                    break;
                }
#endif // FEATURE_FIXED_OUT_ARGS

                default:
                {
                    // No other operators need processing.
                    break;
                }
            } // switch on oper
        }     // foreach tree
    }         // foreach BB

#if FEATURE_FIXED_OUT_ARGS
    // Finish computing the outgoing args area size
    //
    // Need to make sure the MIN_ARG_AREA_FOR_CALL space is added to the frame if:
    // 1. there are calls to THROW_HEPLPER methods.
    // 2. we are generating profiling Enter/Leave/TailCall hooks. This will ensure
    //    that even methods without any calls will have outgoing arg area space allocated.
    //
    // An example for these two cases is Windows Amd64, where the ABI requires to have 4 slots for
    // the outgoing arg space if the method makes any calls.
    if (outgoingArgSpaceSize < MIN_ARG_AREA_FOR_CALL)
    {
        if (compUsesThrowHelper || compIsProfilerHookNeeded())
        {
            outgoingArgSpaceSize = MIN_ARG_AREA_FOR_CALL;
            JITDUMP("Bumping outgoingArgSpaceSize to %u for throw helper or profile hook", outgoingArgSpaceSize);
        }
    }

    // If a function has localloc, we will need to move the outgoing arg space when the
    // localloc happens. When we do this, we need to maintain stack alignment. To avoid
    // leaving alignment-related holes when doing this move, make sure the outgoing
    // argument space size is a multiple of the stack alignment by aligning up to the next
    // stack alignment boundary.
    if (compLocallocUsed)
    {
        outgoingArgSpaceSize = roundUp(outgoingArgSpaceSize, STACK_ALIGN);
        JITDUMP("Bumping outgoingArgSpaceSize to %u for localloc", outgoingArgSpaceSize);
    }

    assert((outgoingArgSpaceSize % TARGET_POINTER_SIZE) == 0);

    lvaOutgoingArgSpaceSize.SetFinalValue(outgoingArgSpaceSize);
    lvaGetDesc(lvaOutgoingArgSpaceVar)->SetBlockType(outgoingArgSpaceSize);
#endif // FEATURE_FIXED_OUT_ARGS

#ifdef DEBUG
    if (verbose && fgRngChkThrowAdded)
    {
        printf("\nAfter fgSimpleLowering() added some RngChk throw blocks");
        fgDispBasicBlocks();
        fgDispHandlerTab();
        printf("\n");
    }
#endif
}

/*****************************************************************************************************
 *
 *  Function to return the last basic block in the main part of the function. With funclets, it is
 *  the block immediately before the first funclet.
 *  An inclusive end of the main method.
 */

BasicBlock* Compiler::fgLastBBInMainFunction()
{
#if defined(FEATURE_EH_FUNCLETS)

    if (fgFirstFuncletBB != nullptr)
    {
        return fgFirstFuncletBB->bbPrev;
    }

#endif // FEATURE_EH_FUNCLETS

    assert(fgLastBB->bbNext == nullptr);

    return fgLastBB;
}

/*****************************************************************************************************
 *
 *  Function to return the first basic block after the main part of the function. With funclets, it is
 *  the block of the first funclet.  Otherwise it is NULL if there are no funclets (fgLastBB->bbNext).
 *  This is equivalent to fgLastBBInMainFunction()->bbNext
 *  An exclusive end of the main method.
 */

BasicBlock* Compiler::fgEndBBAfterMainFunction()
{
#if defined(FEATURE_EH_FUNCLETS)

    if (fgFirstFuncletBB != nullptr)
    {
        return fgFirstFuncletBB;
    }

#endif // FEATURE_EH_FUNCLETS

    assert(fgLastBB->bbNext == nullptr);

    return nullptr;
}

#if defined(FEATURE_EH_FUNCLETS)

/*****************************************************************************
 * Introduce a new head block of the handler for the prolog to be put in, ahead
 * of the current handler head 'block'.
 * Note that this code has some similarities to fgCreateLoopPreHeader().
 */

void Compiler::fgInsertFuncletPrologBlock(BasicBlock* block)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("\nCreating funclet prolog header for " FMT_BB "\n", block->bbNum);
    }
#endif

    assert(block->hasHndIndex());
    assert(fgFirstBlockOfHandler(block) == block); // this block is the first block of a handler

    /* Allocate a new basic block */

    BasicBlock* newHead = bbNewBasicBlock(BBJ_NONE);
    newHead->bbFlags |= BBF_INTERNAL;
    newHead->inheritWeight(block);
    newHead->bbRefs = 0;

    fgInsertBBbefore(block, newHead); // insert the new block in the block list
    fgExtendEHRegionBefore(block);    // Update the EH table to make the prolog block the first block in the block's EH
                                      // block.

    // Distribute the pred list between newHead and block. Incoming edges coming from outside
    // the handler go to the prolog. Edges coming from with the handler are back-edges, and
    // go to the existing 'block'.

    for (BasicBlock* const predBlock : block->PredBlocks())
    {
        if (!fgIsIntraHandlerPred(predBlock, block))
        {
            // It's a jump from outside the handler; add it to the newHead preds list and remove
            // it from the block preds list.

            switch (predBlock->bbJumpKind)
            {
                case BBJ_CALLFINALLY:
                    noway_assert(predBlock->bbJumpDest == block);
                    predBlock->bbJumpDest = newHead;
                    fgRemoveRefPred(block, predBlock);
                    fgAddRefPred(newHead, predBlock);
                    break;

                default:
                    // The only way into the handler is via a BBJ_CALLFINALLY (to a finally handler), or
                    // via exception handling.
                    noway_assert(false);
                    break;
            }
        }
    }

    assert(nullptr == fgGetPredForBlock(block, newHead));
    fgAddRefPred(block, newHead);

    assert((newHead->bbFlags & BBF_INTERNAL) == BBF_INTERNAL);
}

/*****************************************************************************
 *
 * Every funclet will have a prolog. That prolog will be inserted as the first instructions
 * in the first block of the funclet. If the prolog is also the head block of a loop, we
 * would end up with the prolog instructions being executed more than once.
 * Check for this by searching the predecessor list for loops, and create a new prolog header
 * block when needed. We detect a loop by looking for any predecessor that isn't in the
 * handler's try region, since the only way to get into a handler is via that try region.
 */

void Compiler::fgCreateFuncletPrologBlocks()
{
    noway_assert(fgComputePredsDone);
    noway_assert(!fgDomsComputed); // this function doesn't maintain the dom sets
    assert(!fgFuncletsCreated);

    bool prologBlocksCreated = false;

    for (EHblkDsc* const HBtab : EHClauses(this))
    {
        BasicBlock* head = HBtab->ebdHndBeg;

        if (fgAnyIntraHandlerPreds(head))
        {
            // We need to create a new block in which to place the prolog, and split the existing
            // head block predecessor edges into those that should point to the prolog, and those
            // that shouldn't.
            //
            // It's arguable that we should just always do this, and not only when we "need to",
            // so there aren't two different code paths. However, it's unlikely to be necessary
            // for catch handlers because they have an incoming argument (the exception object)
            // that needs to get stored or saved, so back-arcs won't normally go to the head. It's
            // possible when writing in IL to generate a legal loop (e.g., push an Exception object
            // on the stack before jumping back to the catch head), but C# probably won't. This will
            // most commonly only be needed for finallys with a do/while loop at the top of the
            // finally.
            //
            // Note that we don't check filters. This might be a bug, but filters always have a filter
            // object live on entry, so it's at least unlikely (illegal?) that a loop edge targets the
            // filter head.

            fgInsertFuncletPrologBlock(head);
            prologBlocksCreated = true;
        }
    }

    if (prologBlocksCreated)
    {
        // If we've modified the graph, reset the 'modified' flag, since the dominators haven't
        // been computed.
        fgModified = false;

#if DEBUG
        if (verbose)
        {
            JITDUMP("\nAfter fgCreateFuncletPrologBlocks()");
            fgDispBasicBlocks();
            fgDispHandlerTab();
        }

        fgVerifyHandlerTab();
        fgDebugCheckBBlist();
#endif // DEBUG
    }
}

/*****************************************************************************
 *
 *  Function to create funclets out of all EH catch/finally/fault blocks.
 *  We only move filter and handler blocks, not try blocks.
 */

void Compiler::fgCreateFunclets()
{
    assert(!fgFuncletsCreated);

#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In fgCreateFunclets()\n");
    }
#endif

    fgCreateFuncletPrologBlocks();

    unsigned           XTnum;
    EHblkDsc*          HBtab;
    const unsigned int funcCnt = ehFuncletCount() + 1;

    if (!FitsIn<unsigned short>(funcCnt))
    {
        IMPL_LIMITATION("Too many funclets");
    }

    FuncInfoDsc* funcInfo = new (this, CMK_BasicBlock) FuncInfoDsc[funcCnt];

    unsigned short funcIdx;

    // Setup the root FuncInfoDsc and prepare to start associating
    // FuncInfoDsc's with their corresponding EH region
    memset((void*)funcInfo, 0, funcCnt * sizeof(FuncInfoDsc));
    assert(funcInfo[0].funKind == FUNC_ROOT);
    funcIdx = 1;

    // Because we iterate from the top to the bottom of the compHndBBtab array, we are iterating
    // from most nested (innermost) to least nested (outermost) EH region. It would be reasonable
    // to iterate in the opposite order, but the order of funclets shouldn't matter.
    //
    // We move every handler region to the end of the function: each handler will become a funclet.
    //
    // Note that fgRelocateEHRange() can add new entries to the EH table. However, they will always
    // be added *after* the current index, so our iteration here is not invalidated.
    // It *can* invalidate the compHndBBtab pointer itself, though, if it gets reallocated!

    for (XTnum = 0; XTnum < compHndBBtabCount; XTnum++)
    {
        HBtab = ehGetDsc(XTnum); // must re-compute this every loop, since fgRelocateEHRange changes the table
        if (HBtab->HasFilter())
        {
            assert(funcIdx < funcCnt);
            funcInfo[funcIdx].funKind    = FUNC_FILTER;
            funcInfo[funcIdx].funEHIndex = (unsigned short)XTnum;
            funcIdx++;
        }
        assert(funcIdx < funcCnt);
        funcInfo[funcIdx].funKind    = FUNC_HANDLER;
        funcInfo[funcIdx].funEHIndex = (unsigned short)XTnum;
        HBtab->ebdFuncIndex          = funcIdx;
        funcIdx++;
        fgRelocateEHRange(XTnum, FG_RELOCATE_HANDLER);
    }

    // We better have populated all of them by now
    assert(funcIdx == funcCnt);

    // Publish
    compCurrFuncIdx   = 0;
    compFuncInfos     = funcInfo;
    compFuncInfoCount = (unsigned short)funcCnt;

    fgFuncletsCreated = true;

#if DEBUG
    if (verbose)
    {
        JITDUMP("\nAfter fgCreateFunclets()");
        fgDispBasicBlocks();
        fgDispHandlerTab();
    }

    fgVerifyHandlerTab();
    fgDebugCheckBBlist();
#endif // DEBUG
}

#endif // defined(FEATURE_EH_FUNCLETS)

/*-------------------------------------------------------------------------
 *
 * Walk the basic blocks list to determine the first block to place in the
 * cold section.  This would be the first of a series of rarely executed blocks
 * such that no succeeding blocks are in a try region or an exception handler
 * or are rarely executed.
 */

void Compiler::fgDetermineFirstColdBlock()
{
#ifdef DEBUG
    if (verbose)
    {
        printf("\n*************** In fgDetermineFirstColdBlock()\n");
    }
#endif // DEBUG

    // Since we may need to create a new transistion block
    // we assert that it is OK to create new blocks.
    //
    assert(fgSafeBasicBlockCreation);

    fgFirstColdBlock = nullptr;

    if (!opts.compProcedureSplitting)
    {
        JITDUMP("No procedure splitting will be done for this method\n");
        return;
    }

#ifdef DEBUG
    if ((compHndBBtabCount > 0) && !opts.compProcedureSplittingEH)
    {
        JITDUMP("No procedure splitting will be done for this method with EH (by request)\n");
        return;
    }
#endif // DEBUG

#if defined(FEATURE_EH_FUNCLETS)
    // TODO-CQ: handle hot/cold splitting in functions with EH (including synchronized methods
    // that create EH in methods without explicit EH clauses).

    if (compHndBBtabCount > 0)
    {
        JITDUMP("No procedure splitting will be done for this method with EH (implementation limitation)\n");
        return;
    }
#endif // FEATURE_EH_FUNCLETS

    BasicBlock* firstColdBlock       = nullptr;
    BasicBlock* prevToFirstColdBlock = nullptr;
    BasicBlock* block;
    BasicBlock* lblk;

    for (lblk = nullptr, block = fgFirstBB; block != nullptr; lblk = block, block = block->bbNext)
    {
        bool blockMustBeInHotSection = false;

#if HANDLER_ENTRY_MUST_BE_IN_HOT_SECTION
        if (bbIsHandlerBeg(block))
        {
            blockMustBeInHotSection = true;
        }
#endif // HANDLER_ENTRY_MUST_BE_IN_HOT_SECTION

        // Do we have a candidate for the first cold block?
        if (firstColdBlock != nullptr)
        {
            // We have a candidate for first cold block

            // Is this a hot block?
            if (blockMustBeInHotSection || (block->isRunRarely() == false))
            {
                // We have to restart the search for the first cold block
                firstColdBlock       = nullptr;
                prevToFirstColdBlock = nullptr;
            }
        }
        else // (firstColdBlock == NULL)
        {
            // We don't have a candidate for first cold block

            // Is this a cold block?
            if (!blockMustBeInHotSection && (block->isRunRarely() == true))
            {
                //
                // If the last block that was hot was a BBJ_COND
                // then we will have to add an unconditional jump
                // so the code size for block needs be large
                // enough to make it worth our while
                //
                if ((lblk == nullptr) || (lblk->bbJumpKind != BBJ_COND) || (fgGetCodeEstimate(block) >= 8))
                {
                    // This block is now a candidate for first cold block
                    // Also remember the predecessor to this block
                    firstColdBlock       = block;
                    prevToFirstColdBlock = lblk;
                }
            }
        }
    }

    if (firstColdBlock == fgFirstBB)
    {
        // If the first block is Cold then we can't move any blocks
        // into the cold section

        firstColdBlock = nullptr;
    }

    if (firstColdBlock != nullptr)
    {
        noway_assert(prevToFirstColdBlock != nullptr);

        if (prevToFirstColdBlock == nullptr)
        {
            return; // To keep Prefast happy
        }

        // If we only have one cold block
        // then it may not be worth it to move it
        // into the Cold section as a jump to the
        // Cold section is 5 bytes in size.
        //
        if (firstColdBlock->bbNext == nullptr)
        {
            // If the size of the cold block is 7 or less
            // then we will keep it in the Hot section.
            //
            if (fgGetCodeEstimate(firstColdBlock) < 8)
            {
                firstColdBlock = nullptr;
                goto EXIT;
            }
        }

        // When the last Hot block fall through into the Cold section
        // we may need to add a jump
        //
        if (prevToFirstColdBlock->bbFallsThrough())
        {
            switch (prevToFirstColdBlock->bbJumpKind)
            {
                default:
                    noway_assert(!"Unhandled jumpkind in fgDetermineFirstColdBlock()");
                    break;

                case BBJ_CALLFINALLY:
                    // A BBJ_CALLFINALLY that falls through is always followed
                    // by an empty BBJ_ALWAYS.
                    //
                    assert(prevToFirstColdBlock->isBBCallAlwaysPair());
                    firstColdBlock =
                        firstColdBlock->bbNext; // Note that this assignment could make firstColdBlock == nullptr
                    break;

                case BBJ_COND:
                    //
                    // This is a slightly more complicated case, because we will
                    // probably need to insert a block to jump to the cold section.
                    //
                    if (firstColdBlock->isEmpty() && (firstColdBlock->bbJumpKind == BBJ_ALWAYS))
                    {
                        // We can just use this block as the transitionBlock
                        firstColdBlock = firstColdBlock->bbNext;
                        // Note that this assignment could make firstColdBlock == NULL
                    }
                    else
                    {
                        BasicBlock* transitionBlock = fgNewBBafter(BBJ_ALWAYS, prevToFirstColdBlock, true);
                        transitionBlock->bbJumpDest = firstColdBlock;
                        transitionBlock->inheritWeight(firstColdBlock);

                        noway_assert(fgComputePredsDone);

                        // Update the predecessor list for firstColdBlock
                        fgReplacePred(firstColdBlock, prevToFirstColdBlock, transitionBlock);

                        // Add prevToFirstColdBlock as a predecessor for transitionBlock
                        fgAddRefPred(transitionBlock, prevToFirstColdBlock);
                    }
                    break;

                case BBJ_NONE:
                    // If the block preceding the first cold block is BBJ_NONE,
                    // convert it to BBJ_ALWAYS to force an explicit jump.

                    prevToFirstColdBlock->bbJumpDest = firstColdBlock;
                    prevToFirstColdBlock->bbJumpKind = BBJ_ALWAYS;
                    break;
            }
        }
    }

    for (block = firstColdBlock; block != nullptr; block = block->bbNext)
    {
        block->bbFlags |= BBF_COLD;
    }

EXIT:;

#ifdef DEBUG
    if (verbose)
    {
        if (firstColdBlock)
        {
            printf("fgFirstColdBlock is " FMT_BB ".\n", firstColdBlock->bbNum);
        }
        else
        {
            printf("fgFirstColdBlock is NULL.\n");
        }

        fgDispBasicBlocks();
    }

    fgVerifyHandlerTab();
#endif // DEBUG

    fgFirstColdBlock = firstColdBlock;
}

/* static */
unsigned Compiler::acdHelper(SpecialCodeKind codeKind)
{
    switch (codeKind)
    {
        case SCK_RNGCHK_FAIL:
            return CORINFO_HELP_RNGCHKFAIL;
        case SCK_ARG_EXCPN:
            return CORINFO_HELP_THROW_ARGUMENTEXCEPTION;
        case SCK_ARG_RNG_EXCPN:
            return CORINFO_HELP_THROW_ARGUMENTOUTOFRANGEEXCEPTION;
        case SCK_DIV_BY_ZERO:
            return CORINFO_HELP_THROWDIVZERO;
        case SCK_ARITH_EXCPN:
            return CORINFO_HELP_OVERFLOW;
        default:
            assert(!"Bad codeKind");
            return 0;
    }
}

//------------------------------------------------------------------------
// fgAddCodeRef: Find/create an added code entry associated with the given block and with the given kind.
//
// Arguments:
//   srcBlk  - the block that needs an entry;
//   refData - the index to use as the cache key for sharing throw blocks;
//   kind    - the kind of exception;
//
// Return Value:
//   The target throw helper block or nullptr if throw helper blocks are disabled.
//
BasicBlock* Compiler::fgAddCodeRef(BasicBlock* srcBlk, unsigned refData, SpecialCodeKind kind)
{
    // Record that the code will call a THROW_HELPER
    // so on Windows Amd64 we can allocate the 4 outgoing
    // arg slots on the stack frame if there are no other calls.
    compUsesThrowHelper = true;

    if (!fgUseThrowHelperBlocks())
    {
        return nullptr;
    }

    const static BBjumpKinds jumpKinds[] = {
        BBJ_NONE,   // SCK_NONE
        BBJ_THROW,  // SCK_RNGCHK_FAIL
        BBJ_ALWAYS, // SCK_PAUSE_EXEC
        BBJ_THROW,  // SCK_DIV_BY_ZERO
        BBJ_THROW,  // SCK_ARITH_EXCP, SCK_OVERFLOW
        BBJ_THROW,  // SCK_ARG_EXCPN
        BBJ_THROW,  // SCK_ARG_RNG_EXCPN
    };

    noway_assert(sizeof(jumpKinds) == SCK_COUNT); // sanity check

    /* First look for an existing entry that matches what we're looking for */

    AddCodeDsc* add = fgFindExcptnTarget(kind, refData);

    if (add) // found it
    {
        return add->acdDstBlk;
    }

    /* We have to allocate a new entry and prepend it to the list */

    add          = new (this, CMK_Unknown) AddCodeDsc;
    add->acdData = refData;
    add->acdKind = kind;
    add->acdNext = fgAddCodeList;
#if !FEATURE_FIXED_OUT_ARGS
    add->acdStkLvl     = 0;
    add->acdStkLvlInit = false;
#endif // !FEATURE_FIXED_OUT_ARGS

    fgAddCodeList = add;

    /* Create the target basic block */

    BasicBlock* newBlk;

    newBlk = add->acdDstBlk = fgNewBBinRegion(jumpKinds[kind], srcBlk, /* runRarely */ true, /* insertAtEnd */ true);

#ifdef DEBUG
    if (verbose)
    {
        const char* msgWhere = "";
        if (!srcBlk->hasTryIndex() && !srcBlk->hasHndIndex())
        {
            msgWhere = "non-EH region";
        }
        else if (!srcBlk->hasTryIndex())
        {
            msgWhere = "handler";
        }
        else if (!srcBlk->hasHndIndex())
        {
            msgWhere = "try";
        }
        else if (srcBlk->getTryIndex() < srcBlk->getHndIndex())
        {
            msgWhere = "try";
        }
        else
        {
            msgWhere = "handler";
        }

        const char* msg;
        switch (kind)
        {
            case SCK_RNGCHK_FAIL:
                msg = " for RNGCHK_FAIL";
                break;
            case SCK_PAUSE_EXEC:
                msg = " for PAUSE_EXEC";
                break;
            case SCK_DIV_BY_ZERO:
                msg = " for DIV_BY_ZERO";
                break;
            case SCK_OVERFLOW:
                msg = " for OVERFLOW";
                break;
            case SCK_ARG_EXCPN:
                msg = " for ARG_EXCPN";
                break;
            case SCK_ARG_RNG_EXCPN:
                msg = " for ARG_RNG_EXCPN";
                break;
            default:
                msg = " for ??";
                break;
        }

        printf("\nfgAddCodeRef - Add BB in %s%s, new block %s\n", msgWhere, msg, add->acdDstBlk->dspToString());
    }
#endif // DEBUG

    /* Mark the block as added by the compiler and not removable by future flow
       graph optimizations. Note that no bbJumpDest points to these blocks. */

    newBlk->bbFlags |= BBF_IMPORTED;
    newBlk->bbFlags |= BBF_DONT_REMOVE;

    /* Remember that we're adding a new basic block */

    fgAddCodeModf      = true;
    fgRngChkThrowAdded = true;

    /* Now figure out what code to insert */

    int helper = CORINFO_HELP_UNDEF;

    switch (kind)
    {
        case SCK_RNGCHK_FAIL:
            helper = CORINFO_HELP_RNGCHKFAIL;
            break;

        case SCK_DIV_BY_ZERO:
            helper = CORINFO_HELP_THROWDIVZERO;
            break;

        case SCK_ARITH_EXCPN:
            helper = CORINFO_HELP_OVERFLOW;
            noway_assert(SCK_OVERFLOW == SCK_ARITH_EXCPN);
            break;

        case SCK_ARG_EXCPN:
            helper = CORINFO_HELP_THROW_ARGUMENTEXCEPTION;
            break;

        case SCK_ARG_RNG_EXCPN:
            helper = CORINFO_HELP_THROW_ARGUMENTOUTOFRANGEEXCEPTION;
            break;

        // case SCK_PAUSE_EXEC:
        //     noway_assert(!"add code to pause exec");

        default:
            noway_assert(!"unexpected code addition kind");
            return nullptr;
    }

    noway_assert(helper != CORINFO_HELP_UNDEF);

    // Add the appropriate helper call.
    GenTreeCall* tree = gtNewHelperCallNode(helper, TYP_VOID);

    // Store the tree in the new basic block.
    assert(!srcBlk->isEmpty());
    if (!srcBlk->IsLIR())
    {
        // There are no args here but fgMorphArgs has side effects
        // such as setting the outgoing arg area (which is necessary
        // on AMD if there are any calls).
        tree = fgMorphArgs(tree);

        fgInsertStmtAtEnd(newBlk, fgNewStmtFromTree(tree));
    }
    else
    {
        LIR::InsertHelperCallBefore(this, LIR::AsRange(newBlk), nullptr, tree);
    }

    return add->acdDstBlk;
}

/*****************************************************************************
 * Finds the block to jump to, to throw a given kind of exception
 * We maintain a cache of one AddCodeDsc for each kind, to make searching fast.
 * Note : Each block uses the same (maybe shared) block as the jump target for
 * a given type of exception
 */

Compiler::AddCodeDsc* Compiler::fgFindExcptnTarget(SpecialCodeKind kind, unsigned refData)
{
    assert(fgUseThrowHelperBlocks());
    if (!(fgExcptnTargetCache[kind] && // Try the cached value first
          fgExcptnTargetCache[kind]->acdData == refData))
    {
        // Too bad, have to search for the jump target for the exception

        AddCodeDsc* add = nullptr;

        for (add = fgAddCodeList; add != nullptr; add = add->acdNext)
        {
            if (add->acdData == refData && add->acdKind == kind)
            {
                break;
            }
        }

        fgExcptnTargetCache[kind] = add; // Cache it
    }

    return fgExcptnTargetCache[kind];
}

//------------------------------------------------------------------------
// fgGetRngChkTarget: Create/find the appropriate throw block for a range check.
//
// Arguments:
//   block - the block that contains the range check
//   kind  - the kind of exception
//
// Return Value:
//   The target throw helper block this check jumps to upon failure.
//
BasicBlock* Compiler::fgGetRngChkTarget(BasicBlock* block, SpecialCodeKind kind)
{
    if (!fgUseThrowHelperBlocks() || compIsForInlining())
    {
        return nullptr;
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("*** Computing fgRngChkTarget for block " FMT_BB "\n", block->bbNum);
        if (!block->IsLIR())
        {
            gtDispStmt(compCurStmt);
        }
    }
#endif

    // We attach the throw block to the containing try region (if any).
    return fgAddCodeRef(block, bbThrowIndex(block), kind);
}

/*****************************************************************************
 *
 *  Figure out the order in which operators should be evaluated, along with
 *  other information (such as the register sets trashed by each subtree).
 *  Also finds blocks that need GC polls and inserts them as needed.
 */

void Compiler::fgSetBlockOrder()
{
#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In fgSetBlockOrder()\n");
    }
#endif // DEBUG

#ifdef DEBUG
    BasicBlock::s_nMaxTrees = 0;
#endif

    /* Walk the basic blocks to assign sequence numbers */

    /* If we don't compute the doms, then we never mark blocks as loops. */
    if (fgDomsComputed)
    {
        for (BasicBlock* const block : Blocks())
        {
            /* If this block is a loop header, mark it appropriately */

            if (block->isLoopHead())
            {
                fgMarkLoopHead(block);
            }
        }
    }
    else
    {
        /* If we don't have the dominators, use an abbreviated test for fully interruptible.  If there are
         * any back edges, check the source and destination blocks to see if they're GC Safe.  If not, then
         * go fully interruptible. */
        for (BasicBlock* const block : Blocks())
        {
// true if the edge is forward, or if it is a back edge and either the source and dest are GC safe.
#define EDGE_IS_GC_SAFE(src, dst)                                                                                      \
    (((src)->bbNum < (dst)->bbNum) || (((src)->bbFlags | (dst)->bbFlags) & BBF_GC_SAFE_POINT))

            bool partiallyInterruptible = true;
            switch (block->bbJumpKind)
            {
                case BBJ_COND:
                case BBJ_ALWAYS:
                    partiallyInterruptible = EDGE_IS_GC_SAFE(block, block->bbJumpDest);
                    break;

                case BBJ_SWITCH:
                    for (BasicBlock* const bTarget : block->SwitchTargets())
                    {
                        partiallyInterruptible &= EDGE_IS_GC_SAFE(block, bTarget);
                    }
                    break;

                default:
                    break;
            }

            if (!partiallyInterruptible)
            {
                // DDB 204533:
                // The GC encoding for fully interruptible methods does not
                // support more than 1023 pushed arguments, so we can't set
                // SetInterruptible() here when we have 1024 or more pushed args
                //
                if (compCanEncodePtrArgCntMax())
                {
                    SetInterruptible(true);
                }
                break;
            }
#undef EDGE_IS_GC_SAFE
        }
    }

    for (BasicBlock* const block : Blocks())
    {

#if FEATURE_FASTTAILCALL
#ifndef JIT32_GCENCODER
        if (block->endsWithTailCallOrJmp(this, true) && optReachWithoutCall(fgFirstBB, block))
        {
            // This tail call might combine with other tail calls to form a
            // loop.  Thus we need to either add a poll, or make the method
            // fully interruptible.  I chose the later because that's what
            // JIT64 does.
            SetInterruptible(true);
        }
#endif // !JIT32_GCENCODER
#endif // FEATURE_FASTTAILCALL

        fgSetBlockOrder(block);
    }

    /* Remember that now the tree list is threaded */

    fgStmtListThreaded = true;

#ifdef DEBUG
    if (verbose)
    {
        printf("The biggest BB has %4u tree nodes\n", BasicBlock::s_nMaxTrees);
    }
    fgDebugCheckLinks();
#endif // DEBUG
}

void Compiler::fgSetBlockOrder(BasicBlock* block)
{
    for (Statement* stmt : block->Statements())
    {
        fgSetStmtSeq(stmt);

        /* Are there any more trees in this basic block? */

        if (stmt->GetNextStmt() == nullptr)
        {
            /* last statement in the tree list */
            noway_assert(block->lastStmt() == stmt);
            break;
        }

#ifdef DEBUG
        if (block->bbStmtList == stmt)
        {
            /* first statement in the list */
            assert(stmt->GetPrevStmt()->GetNextStmt() == nullptr);
        }
        else
        {
            assert(stmt->GetPrevStmt()->GetNextStmt() == stmt);
        }

        assert(stmt->GetNextStmt()->GetPrevStmt() == stmt);
#endif // DEBUG
    }
}

//------------------------------------------------------------------------
// fgGetFirstNode: Get the first node in the tree, in execution order
//
// Arguments:
//    tree - The top node of the tree of interest
//
// Return Value:
//    The first node in execution order, that belongs to tree.
//
// Assumptions:
//     'tree' must either be a leaf, or all of its constituent nodes must be contiguous
//     in execution order. SequenceDebugCheckVisitor checks this.
//
/* static */
GenTree* Compiler::fgGetFirstNode(GenTree* tree)
{
    GenTreeOperandIterator i = tree->OperandsBegin();

    while (i != tree->OperandsEnd())
    {
        tree = *i;
        i    = tree->OperandsBegin();
    }

    return tree;
}

// Tree visitor that traverse the entire tree and links nodes in linear execution order.
class SequenceVisitor : public GenTreeVisitor<SequenceVisitor>
{
    GenTree  m_dummyHead;
    GenTree* m_tail;
    bool     m_isLIR;
    INDEBUG(unsigned m_seqNum;)

public:
    enum
    {
        DoPostOrder       = true,
        UseExecutionOrder = true
    };

    SequenceVisitor(Compiler* compiler, bool isLIR)
        : GenTreeVisitor(compiler)
        , m_tail(&m_dummyHead)
        , m_isLIR(isLIR)
#ifdef DEBUG
        , m_seqNum(0)
#endif
    {
    }

    GenTree* Sequence(GenTree* tree)
    {
        WalkTree(&tree, nullptr);

        m_tail->gtNext = nullptr;

        GenTree* head = m_dummyHead.gtNext;
        head->gtPrev  = nullptr;
        assert(head->gtSeqNum == 1);
        return head;
    }

    fgWalkResult PostOrderVisit(GenTree** use, GenTree* user)
    {
        GenTree* node = *use;

        // If we are sequencing for LIR:
        // - Clear the reverse ops flag
        // - If we are processing a node that does not appear in LIR, do not add it to the list.
        if (m_isLIR)
        {
            node->gtFlags &= ~GTF_REVERSE_OPS;

            if (node->OperIs(GT_ARGPLACE))
            {
                return Compiler::WALK_CONTINUE;
            }
        }

        node->gtPrev   = m_tail;
        m_tail->gtNext = node;
        m_tail         = node;

        INDEBUG(node->gtSeqNum = ++m_seqNum;)
        // Keep track of the highest # of tree nodes
        INDEBUG(BasicBlock::s_nMaxTrees = max(m_seqNum, BasicBlock::s_nMaxTrees);)

        return Compiler::WALK_CONTINUE;
    }
};

#ifdef DEBUG
class SequenceDebugCheckVisitor : public GenTreeVisitor<SequenceDebugCheckVisitor>
{
    GenTree*             m_head;
    GenTree*             m_tail;
    bool                 m_isLIR;
    unsigned             m_seqNum;
    ArrayStack<GenTree*> m_nodeStack;
    ArrayStack<GenTree*> m_operands;

public:
    enum
    {
        DoPreOrder        = true,
        DoPostOrder       = true,
        UseExecutionOrder = true
    };

    SequenceDebugCheckVisitor(Compiler* compiler, bool isLIR)
        : GenTreeVisitor(compiler)
        , m_head(nullptr)
        , m_tail(nullptr)
        , m_isLIR(isLIR)
        , m_seqNum(0)
        , m_nodeStack(compiler->getAllocator(CMK_DebugOnly))
        , m_operands(compiler->getAllocator(CMK_DebugOnly))
    {
    }

    void CheckSequence(GenTree* tree)
    {
        WalkTree(&tree, nullptr);

        assert(m_head->gtPrev == nullptr);
        assert(m_tail->gtNext == nullptr);
        assert(m_head->gtSeqNum == 1);
    }

    fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
    {
        m_nodeStack.Push(*use);
        return Compiler::WALK_CONTINUE;
    }

    fgWalkResult PostOrderVisit(GenTree** use, GenTree* user)
    {
        GenTree* node = *use;

        // GTF_REVERSE_OPS is never set in LIR.
        assert(!m_isLIR || !node->IsReverseOp());

        // Placeholders are not linked in LIR.
        if (m_isLIR && node->OperIs(GT_ARGPLACE))
        {
            return Compiler::WALK_CONTINUE;
        }

        // Check if the gtNext/gtPrev links are valid.
        assert(node->gtSeqNum == ++m_seqNum);
        assert(node->gtPrev == m_tail);
        assert((m_tail == nullptr) || (m_tail->gtNext == node));

        if (m_head == nullptr)
        {
            m_head = node;
        }

        m_tail = node;

        // Now check if GenTree::Operands() returns the operands in the correct order.
        m_operands.Clear();

        // The operands have been pushed by PreOrderVisit onto the node stack but they're
        // reversed, the last operand is on top of the stack. Move them to another stack
        // to get the correct order.
        while (m_nodeStack.Top(0) != node)
        {
            m_operands.Push(m_nodeStack.Pop());
        }

        for (GenTree* op : node->Operands())
        {
            assert(m_operands.Top() == op);
            m_operands.Pop();
        }

        return Compiler::WALK_CONTINUE;
    }
};

void Compiler::fgCheckTreeSeq(GenTree* tree, bool isLIR)
{
    SequenceDebugCheckVisitor check(this, isLIR);
    check.CheckSequence(tree);
}

#endif // DEBUG

//------------------------------------------------------------------------
// fgSetTreeSeq: Sequence a tree.
//
// Arguments:
//    tree - The tree to sequence
//    isLIR - Do LIR mode sequencing (reset GTF_REVERSE_OPS and skip nodes like GT_ARGPLACE)
//
// Return Value:
//    Returns the first node (execution order) in the sequenced tree.
//
GenTree* Compiler::fgSetTreeSeq(GenTree* tree, bool isLIR)
{
    SequenceVisitor visitor(this, isLIR);
    GenTree*        firstNode = visitor.Sequence(tree);
    INDEBUG(fgCheckTreeSeq(tree, isLIR);)
    return firstNode;
}

//------------------------------------------------------------------------
// fgSetStmtSeq: Sequence a statement.
//
// Arguments:
//    stmt - The statement to sequence
//
void Compiler::fgSetStmtSeq(Statement* stmt)
{
    stmt->SetTreeList(fgSetTreeSeq(stmt->GetRootNode(), false));
}

/*****************************************************************************/
/*static*/
Compiler::fgWalkResult Compiler::fgChkThrowCB(GenTree** pTree, fgWalkData* data)
{
    GenTree* tree = *pTree;

    // If this tree doesn't have the EXCEPT flag set, then there is no
    // way any of the child nodes could throw, so we can stop recursing.
    if (!(tree->gtFlags & GTF_EXCEPT))
    {
        return Compiler::WALK_SKIP_SUBTREES;
    }

    switch (tree->gtOper)
    {
        case GT_MUL:
        case GT_ADD:
        case GT_SUB:
        case GT_CAST:
            if (tree->gtOverflow())
            {
                return Compiler::WALK_ABORT;
            }
            break;

        case GT_INDEX_ADDR:
            // These two call CORINFO_HELP_RNGCHKFAIL for Debug code
            if (tree->gtFlags & GTF_INX_RNGCHK)
            {
                return Compiler::WALK_ABORT;
            }
            break;

        case GT_ARR_BOUNDS_CHECK:
            return Compiler::WALK_ABORT;

        default:
            break;
    }

    return Compiler::WALK_CONTINUE;
}

/*****************************************************************************/
/*static*/
Compiler::fgWalkResult Compiler::fgChkLocAllocCB(GenTree** pTree, fgWalkData* data)
{
    GenTree* tree = *pTree;

    if (tree->gtOper == GT_LCLHEAP)
    {
        return Compiler::WALK_ABORT;
    }

    return Compiler::WALK_CONTINUE;
}
