// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef DEBUG
void Compiler::fgPrintEdgeWeights()
{
    // Print out all of the edge weights
    for (BasicBlock* const bDst : Blocks())
    {
        if (bDst->bbPreds != nullptr)
        {
            printf("    Edge weights into " FMT_BB " :", bDst->bbNum);
            for (flowList* const edge : bDst->PredEdges())
            {
                BasicBlock* bSrc = edge->getBlock();
                // This is the control flow edge (bSrc -> bDst)

                printf(FMT_BB " ", bSrc->bbNum);

                if (edge->edgeWeightMin() < BB_MAX_WEIGHT)
                {
                    printf("(%f", edge->edgeWeightMin());
                }
                else
                {
                    printf("(MAX");
                }
                if (edge->edgeWeightMin() != edge->edgeWeightMax())
                {
                    if (edge->edgeWeightMax() < BB_MAX_WEIGHT)
                    {
                        printf("..%f", edge->edgeWeightMax());
                    }
                    else
                    {
                        printf("..MAX");
                    }
                }
                printf(")");
                if (edge->flNext != nullptr)
                {
                    printf(", ");
                }
            }
            printf("\n");
        }
    }
}

void Compiler::fgDebugCheckUpdate()
{
    if (!compStressCompile(STRESS_CHK_FLOW_UPDATE, 30))
    {
        return;
    }

    /* We check for these conditions:
     * no unreachable blocks  -> no blocks have countOfInEdges() = 0
     * no empty blocks        -> !block->isEmpty(), unless non-removable or multiple in-edges
     * no un-imported blocks  -> no blocks have BBF_IMPORTED not set (this is
     *                           kind of redundand with the above, but to make sure)
     * no un-compacted blocks -> BBJ_NONE followed by block with no jumps to it (countOfInEdges() = 1)
     */

    BasicBlock* prev;
    BasicBlock* block;
    for (prev = nullptr, block = fgFirstBB; block != nullptr; prev = block, block = block->bbNext)
    {
        /* no unreachable blocks */

        if ((block->countOfInEdges() == 0) && !(block->bbFlags & BBF_DONT_REMOVE)
#if defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
            // With funclets, we never get rid of the BBJ_ALWAYS part of a BBJ_CALLFINALLY/BBJ_ALWAYS pair,
            // even if we can prove that the finally block never returns.
            && !block->isBBCallAlwaysPairTail()
#endif // FEATURE_EH_FUNCLETS
                )
        {
            noway_assert(!"Unreachable block not removed!");
        }

        /* no empty blocks */

        if (block->isEmpty() && !(block->bbFlags & BBF_DONT_REMOVE))
        {
            switch (block->bbJumpKind)
            {
                case BBJ_CALLFINALLY:
                case BBJ_EHFINALLYRET:
                case BBJ_EHFILTERRET:
                case BBJ_RETURN:
                /* for BBJ_ALWAYS is probably just a GOTO, but will have to be treated */
                case BBJ_ALWAYS:
                case BBJ_EHCATCHRET:
                    /* These jump kinds are allowed to have empty tree lists */
                    break;

                default:
                    /* it may be the case that the block had more than one reference to it
                     * so we couldn't remove it */

                    if (block->countOfInEdges() == 0)
                    {
                        noway_assert(!"Empty block not removed!");
                    }
                    break;
            }
        }

        /* no un-imported blocks */

        if (!(block->bbFlags & BBF_IMPORTED))
        {
            /* internal blocks do not count */

            if (!(block->bbFlags & BBF_INTERNAL))
            {
                noway_assert(!"Non IMPORTED block not removed!");
            }
        }

        bool prevIsCallAlwaysPair = block->isBBCallAlwaysPairTail();

        // Check for an unnecessary jumps to the next block
        bool doAssertOnJumpToNextBlock = false; // unless we have a BBJ_COND or BBJ_ALWAYS we can not assert

        if (block->bbJumpKind == BBJ_COND)
        {
            // A conditional branch should never jump to the next block
            // as it can be folded into a BBJ_NONE;
            doAssertOnJumpToNextBlock = true;
        }
        else if (block->bbJumpKind == BBJ_ALWAYS)
        {
            // Generally we will want to assert if a BBJ_ALWAYS branches to the next block
            doAssertOnJumpToNextBlock = true;

            // If the BBF_KEEP_BBJ_ALWAYS flag is set we allow it to jump to the next block
            if (block->bbFlags & BBF_KEEP_BBJ_ALWAYS)
            {
                doAssertOnJumpToNextBlock = false;
            }

            // A call/always pair is also allowed to jump to the next block
            if (prevIsCallAlwaysPair)
            {
                doAssertOnJumpToNextBlock = false;
            }

            // We are allowed to have a branch from a hot 'block' to a cold 'bbNext'
            //
            if ((block->bbNext != nullptr) && fgInDifferentRegions(block, block->bbNext))
            {
                doAssertOnJumpToNextBlock = false;
            }
        }

        if (doAssertOnJumpToNextBlock)
        {
            if (block->bbJumpDest == block->bbNext)
            {
                noway_assert(!"Unnecessary jump to the next block!");
            }
        }

        /* Make sure BBF_KEEP_BBJ_ALWAYS is set correctly */

        if ((block->bbJumpKind == BBJ_ALWAYS) && prevIsCallAlwaysPair)
        {
            noway_assert(block->bbFlags & BBF_KEEP_BBJ_ALWAYS);
        }

        /* For a BBJ_CALLFINALLY block we make sure that we are followed by */
        /* an BBJ_ALWAYS block with BBF_INTERNAL set */
        /* or that it's a BBF_RETLESS_CALL */
        if (block->bbJumpKind == BBJ_CALLFINALLY)
        {
            assert((block->bbFlags & BBF_RETLESS_CALL) || block->isBBCallAlwaysPair());
        }

        /* no un-compacted blocks */

        if (fgCanCompactBlocks(block, block->bbNext))
        {
            noway_assert(!"Found un-compacted blocks!");
        }
    }
}

void Compiler::fgDispReach()
{
    printf("------------------------------------------------\n");
    printf("BBnum  Reachable by \n");
    printf("------------------------------------------------\n");

    for (BasicBlock* const block : Blocks())
    {
        printf(FMT_BB " : ", block->bbNum);
        for (BlockSetOps::Enumerator e(this, block->bbReach); e.MoveNext();)
        {
            printf(FMT_BB " ", e.Current());
        }
        printf("\n");
    }
}

void Compiler::fgDispDoms(BasicBlock** postOrder) const
{
    // Don't bother printing this when we have a large number of BasicBlocks in the method
    if (fgBBcount > 256)
    {
        return;
    }

    printf("------------------------------------------------\n");
    printf("BBnum  Dominated by\n");
    printf("------------------------------------------------\n");

    for (unsigned i = 1; i <= fgBBNumMax; ++i)
    {
        BasicBlock* current = postOrder[i];
        printf(FMT_BB ":  ", current->bbNum);
        while (current != current->bbIDom)
        {
            printf(FMT_BB " ", current->bbNum);
            current = current->bbIDom;
        }
        printf("\n");
    }
}

static unsigned CountDigits(unsigned num, unsigned base = 10)
{
    assert(2 <= base && base <= 16);
    unsigned count = 1;
    while (num >= base)
    {
        num /= base;
        ++count;
    }
    return count;
}

static unsigned CountDigits(float num)
{
    unsigned count = 1;
    while (num >= 10)
    {
        num /= 10;
        ++count;
    }
    return count;
}

void Compiler::fgTableDispBasicBlock(const BasicBlock* block, int ibcColWidth)
{
    unsigned maxBlockNum      = compIsForInlining() ? impInlineInfo->InlinerCompiler->fgBBNumMax : fgBBNumMax;
    unsigned maxBlockNumWidth = Max(CountDigits(maxBlockNum), 2u);
    unsigned blockNumWidth    = Max(CountDigits(block->bbNum), 2u);

    printf("%s %2u", block->dspToString(maxBlockNumWidth - blockNumWidth), block->bbRefs);

    if (block->hasTryIndex())
    {
        printf(" %2u", block->getTryIndex());
    }
    else
    {
        printf("   ");
    }

    if (block->hasHndIndex())
    {
        printf(" %2u ", block->getHndIndex());
    }
    else
    {
        printf("    ");
    }

    unsigned predsLength;

    if (fgCheapPredsValid)
    {
        predsLength = block->dspCheapPreds();
    }
    else
    {
        predsLength = block->dspPreds();
    }

    if (predsLength < 19)
    {
        printf("%*s ", 19 - predsLength, "");
    }
    else
    {
        printf(" ");
    }

    if (block->isMaxBBWeight())
    {
        printf(" MAX  ");
    }
    else
    {
        BasicBlock::weight_t weight = block->getBBWeight(this);

        if (weight <= 99999)
        {
            printf("%6s", refCntWtd2str(weight));
        }
        else if (weight <= 99999 * BB_UNITY_WEIGHT)
        {
            printf("%5u.", static_cast<unsigned>(FloatingPointUtils::round(weight / BB_UNITY_WEIGHT)));
        }
        else
        {
            printf("%5uk", static_cast<unsigned>(FloatingPointUtils::round(weight / 1000 / BB_UNITY_WEIGHT)));
        }
    }

    if (ibcColWidth > 0)
    {
        if (block->hasProfileWeight())
        {
            printf("%*u", ibcColWidth, (unsigned)FloatingPointUtils::round(block->bbWeight));
        }
        else
        {
            printf("%*s", ibcColWidth, "");
        }
    }

    printf(" ");

    if (block->bbNatLoopNum == BasicBlock::NOT_IN_LOOP)
    {
        printf("   ");
    }
    else
    {
        printf("%2u ", block->bbNatLoopNum);
    }

    block->dspBlockILRange();

    if ((block->bbFlags & BBF_REMOVED) != 0)
    {
        printf("[removed]        ");
    }
    else
    {
        switch (block->bbJumpKind)
        {
            case BBJ_COND:
                printf("-> " FMT_BB "%*s ( cond )", block->bbJumpDest->bbNum,
                       maxBlockNumWidth - max(CountDigits(block->bbJumpDest->bbNum), 2), "");
                break;

            case BBJ_CALLFINALLY:
                printf("-> " FMT_BB "%*s (callf )", block->bbJumpDest->bbNum,
                       maxBlockNumWidth - max(CountDigits(block->bbJumpDest->bbNum), 2), "");
                break;

            case BBJ_ALWAYS:
                if ((block->bbFlags & BBF_KEEP_BBJ_ALWAYS) != 0)
                {
                    printf("-> " FMT_BB "%*s (ALWAYS)", block->bbJumpDest->bbNum,
                           maxBlockNumWidth - max(CountDigits(block->bbJumpDest->bbNum), 2), "");
                }
                else
                {
                    printf("-> " FMT_BB "%*s (always)", block->bbJumpDest->bbNum,
                           maxBlockNumWidth - max(CountDigits(block->bbJumpDest->bbNum), 2), "");
                }
                break;

            case BBJ_LEAVE:
                printf("-> " FMT_BB "%*s (leave )", block->bbJumpDest->bbNum,
                       maxBlockNumWidth - max(CountDigits(block->bbJumpDest->bbNum), 2), "");
                break;

            case BBJ_EHFINALLYRET:
                printf("%*s        (finret)", maxBlockNumWidth - 2, "");
                break;

            case BBJ_EHFILTERRET:
                printf("-> " FMT_BB "%*s (fltret)", block->bbJumpDest->bbNum,
                       maxBlockNumWidth - max(CountDigits(block->bbJumpDest->bbNum), 2), "");
                break;

            case BBJ_EHCATCHRET:
                printf("-> " FMT_BB "%*s ( cret )", block->bbJumpDest->bbNum,
                       maxBlockNumWidth - max(CountDigits(block->bbJumpDest->bbNum), 2), "");
                break;

            case BBJ_THROW:
                printf("%*s        (throw )", maxBlockNumWidth - 2, "");
                break;

            case BBJ_RETURN:
                printf("%*s        (return)", maxBlockNumWidth - 2, "");
                break;

            default:
                printf("%*s                ", maxBlockNumWidth - 2, "");
                break;

            case BBJ_SWITCH:
            {
                printf("->");

                const BBswtDesc* switchDesc   = block->bbJumpSwt;
                int              switchLength = 0;

                for (unsigned i = 0; i < switchDesc->bbsCount; i++)
                {
                    switchLength += printf("%s" FMT_BB, i == 0 ? "" : ",", switchDesc->bbsDstTab[i]->bbNum);

                    if (switchDesc->bbsHasDefault && (i == switchDesc->bbsCount - 1))
                    {
                        switchLength += printf("[def]");
                    }

                    if (switchDesc->bbsHasDominantCase && (i == switchDesc->bbsDominantCase))
                    {
                        switchLength += printf("[dom(" FMT_WT ")]", switchDesc->bbsDominantFraction);
                    }
                }

                if (switchLength < 7)
                {
                    printf("%*s", 8 - switchLength, "");
                }

                printf(" (switch)");
            }
            break;
        }

        printf(" ");
    }

    if (block->hasTryIndex())
    {
        printf("T%u ", block->getTryIndex());
    }
    else
    {
        printf("   ");
    }

    if (block->hasHndIndex())
    {
        printf("H%u ", block->getHndIndex());
    }
    else
    {
        printf("   ");
    }

    if ((block->bbFlags & BBF_FUNCLET_BEG) != 0)
    {
        printf("F ");
    }
    else
    {
        printf("  ");
    }

    int length = 0;

    switch (block->bbCatchTyp)
    {
        case BBCT_NONE:
            break;
        case BBCT_FAULT:
            length += printf("fault { ");
            break;
        case BBCT_FINALLY:
            length += printf("finally { ");
            break;
        case BBCT_FILTER:
            length += printf("filter { ");
            break;
        case BBCT_FILTER_HANDLER:
            length += printf("filtHnd { ");
            break;
        default:
            length += printf("catch { ");
            break;
    }

    if ((block->bbFlags & BBF_TRY_BEG) != 0)
    {
        for (EHblkDsc* ehClause : EHClauses(this))
        {
            if (ehClause->ebdTryBeg == block)
            {
                length += printf("try { ");
            }
        }
    }

    for (EHblkDsc* ehClause : EHClauses(this))
    {
        if ((ehClause->ebdTryLast == block) || (ehClause->ebdHndLast == block) ||
            (ehClause->HasFilter() && (block->bbNext == ehClause->ebdHndBeg)))
        {
            length += printf("} ");
        }
    }

    while (length < 12)
    {
        length += printf(" ");
    }

    block->dspFlags();

    printf("\n");
}

void Compiler::fgDispBasicBlocks(BasicBlock* firstBlock, BasicBlock* lastBlock, bool dumpTrees)
{
    // If any block has IBC data, we add an "IBC weight" column just before the 'IL range' column. This column is as
    // wide as necessary to accommodate all the various IBC weights. It's at least 4 characters wide, to accommodate
    // the "IBC" title and leading space.
    unsigned ibcColWidth = 0;

    for (BasicBlock* block = firstBlock; block != nullptr; block = block->bbNext)
    {
        if (block->hasProfileWeight())
        {
            ibcColWidth = Max(ibcColWidth, CountDigits(block->bbWeight));
        }

        if (block == lastBlock)
        {
            break;
        }
    }

    if (ibcColWidth > 0)
    {
        ibcColWidth = Max(ibcColWidth, 3u) + 1; // + 1 for the leading space
    }

    unsigned bbNumMax         = compIsForInlining() ? impInlineInfo->InlinerCompiler->fgBBNumMax : fgBBNumMax;
    unsigned maxBlockNumWidth = CountDigits(bbNumMax);
    maxBlockNumWidth          = Max(maxBlockNumWidth, 2u);
    int padWidth              = maxBlockNumWidth - 2; // Account for functions with a large number of blocks.

    // clang-format off

    printf("\n------%*s-------------------------------------%*s--------------------------%*s----------------------------------------\n",
        padWidth, "------------",
        ibcColWidth, "------------",
        maxBlockNumWidth, "----");
    printf("BBnum %*sBBid ref try hnd %s     weight  %*s%s  lp [IL range]     [jump]%*s    [EH region]         [flags]\n",
        padWidth, "",
        (fgCheapPredsValid ? "cheap preds" : (fgComputePredsDone ? "preds      " : "           ")),
        (ibcColWidth > 0 ? ibcColWidth - 3 : 0), "",  // Subtract 3 for the width of "IBC", printed next.
        (ibcColWidth > 0 ? "IBC" : ""),
        maxBlockNumWidth, "");
    printf("------%*s-------------------------------------%*s--------------------------%*s----------------------------------------\n",
        padWidth, "------------",
        ibcColWidth, "------------",
        maxBlockNumWidth, "----");

    // clang-format on

    for (BasicBlock* block = firstBlock; block != nullptr; block = block->bbNext)
    {
        // First, do some checking on the bbPrev links
        if (block->bbPrev != nullptr)
        {
            if (block->bbPrev->bbNext != block)
            {
                printf("bad prev link\n");
            }
        }
        else if (block != fgFirstBB)
        {
            printf("bad prev link!\n");
        }

        if (block == fgFirstColdBlock)
        {
            printf(
                "~~~~~~%*s~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%*s~~~~~~~~~~~~~~~~~~~~~~~~~~%*s~~~~~~~~~~~~~~~~~~~~~~~~"
                "~~~~~~~~~~~~~~~~\n",
                padWidth, "~~~~~~~~~~~~", ibcColWidth, "~~~~~~~~~~~~", maxBlockNumWidth, "~~~~");
        }

#ifdef FEATURE_EH_FUNCLETS
        if (block == fgFirstFuncletBB)
        {
            printf(
                "++++++%*s+++++++++++++++++++++++++++++++++++++%*s++++++++++++++++++++++++++%*s++++++++++++++++++++++++"
                "++++++++++++++++ funclets follow\n",
                padWidth, "++++++++++++", ibcColWidth, "++++++++++++", maxBlockNumWidth, "++++");
        }
#endif

        fgTableDispBasicBlock(block, ibcColWidth);

        if (block == lastBlock)
        {
            break;
        }
    }

    printf(
        "------%*s-------------------------------------%*s--------------------------%*s--------------------------------"
        "--------\n",
        padWidth, "------------", ibcColWidth, "------------", maxBlockNumWidth, "----");

    if (dumpTrees)
    {
        fgDumpTrees(firstBlock, lastBlock);
    }
}

void Compiler::fgDispBasicBlocks(bool dumpTrees)
{
    fgDispBasicBlocks(fgFirstBB, nullptr, dumpTrees);
}

void Compiler::fgDumpBlock(BasicBlock* block)
{
    printf("\n------------ ");

    block->dspBlockHeader(this, true, false, true);

    if (!block->IsLIR())
    {
        for (Statement* const stmt : block->Statements())
        {
            printf("\n" FMT_BB " ", block->bbNum);
            gtDispStmt(stmt);
        }
    }
    else
    {
        dmpLIRRange(LIR::AsRange(block));
    }
}

void Compiler::fgDumpTrees(BasicBlock* firstBlock, BasicBlock* lastBlock)
{
    for (BasicBlock* block = firstBlock; block != nullptr; block = block->bbNext)
    {
        fgDumpBlock(block);

        if (block == lastBlock)
        {
            break;
        }
    }

    printf("\n---------------------------------------------------------------------------------------------------------"
           "----------\n");
}

// BBPredsChecker checks jumps from the block's predecessors to the block.
class BBPredsChecker
{
    Compiler* comp;

public:
    BBPredsChecker(Compiler* compiler) : comp(compiler)
    {
    }

    // Check basic block predecessors list.
    //
    // This DEBUG routine checks that all predecessors have the correct traversal stamp
    // and have correct jumps to the block.
    // It calculates the number of incoming edges from the internal block,
    // i.e. it does not count the global incoming edge for the first block.
    //
    // Returns the number of incoming edges for the block.
    unsigned CheckBBPreds(BasicBlock* block, unsigned curTraversalStamp)
    {
        if (comp->fgCheapPredsValid)
        {
            return 0;
        }

        if (!comp->fgComputePredsDone)
        {
            assert(block->bbPreds == nullptr);
            return 0;
        }

        unsigned blockRefs = 0;
        for (flowList* const pred : block->PredEdges())
        {
            blockRefs += pred->flDupCount;

            BasicBlock* blockPred = pred->getBlock();

            // Make sure this pred is part of the BB list.
            assert(blockPred->bbTraversalStamp == curTraversalStamp);

            EHblkDsc* ehTryDsc = comp->ehGetBlockTryDsc(block);
            if (ehTryDsc != nullptr)
            {
                assert(CheckEhTryDsc(block, blockPred, ehTryDsc));
            }

            EHblkDsc* ehHndDsc = comp->ehGetBlockHndDsc(block);
            if (ehHndDsc != nullptr)
            {
                assert(CheckEhHndDsc(block, blockPred, ehHndDsc));
            }

            assert(CheckJump(blockPred, block));
        }

        // Make sure preds are in increasing BBnum order
        assert(block->checkPredListOrder());

        return blockRefs;
    }

private:
    bool CheckEhTryDsc(BasicBlock* block, BasicBlock* blockPred, EHblkDsc* ehTryDsc)
    {
        // You can jump to the start of a try
        if (ehTryDsc->ebdTryBeg == block)
        {
            return true;
        }

        // You can jump within the same try region
        if (comp->bbInTryRegions(block->getTryIndex(), blockPred))
        {
            return true;
        }

        // The catch block can jump back into the middle of the try
        if (comp->bbInCatchHandlerRegions(block, blockPred))
        {
            return true;
        }

        // The end of a finally region is a BBJ_EHFINALLYRET block (during importing, BBJ_LEAVE) which
        // is marked as "returning" to the BBJ_ALWAYS block following the BBJ_CALLFINALLY
        // block that does a local call to the finally. This BBJ_ALWAYS is within
        // the try region protected by the finally (for x86, ARM), but that's ok.
        BasicBlock* prevBlock = block->bbPrev;
        if (prevBlock->bbJumpKind == BBJ_CALLFINALLY && block->bbJumpKind == BBJ_ALWAYS &&
            blockPred->bbJumpKind == BBJ_EHFINALLYRET)
        {
            return true;
        }

        // For OSR, we allow the firstBB to branch to the middle of a try.
        if (comp->opts.IsOSR() && (blockPred == comp->fgFirstBB))
        {
            return true;
        }

        printf("Jump into the middle of try region: " FMT_BB " branches to " FMT_BB "\n", blockPred->bbNum,
               block->bbNum);
        assert(!"Jump into middle of try region");
        return false;
    }

    bool CheckEhHndDsc(BasicBlock* block, BasicBlock* blockPred, EHblkDsc* ehHndlDsc)
    {
        // You can do a BBJ_EHFINALLYRET or BBJ_EHFILTERRET into a handler region
        if ((blockPred->bbJumpKind == BBJ_EHFINALLYRET) || (blockPred->bbJumpKind == BBJ_EHFILTERRET))
        {
            return true;
        }

        // Our try block can call our finally block
        if ((block->bbCatchTyp == BBCT_FINALLY) && (blockPred->bbJumpKind == BBJ_CALLFINALLY) &&
            comp->ehCallFinallyInCorrectRegion(blockPred, block->getHndIndex()))
        {
            return true;
        }

        // You can jump within the same handler region
        if (comp->bbInHandlerRegions(block->getHndIndex(), blockPred))
        {
            return true;
        }

        // A filter can jump to the start of the filter handler
        if (ehHndlDsc->HasFilter())
        {
            return true;
        }

        printf("Jump into the middle of handler region: " FMT_BB " branches to " FMT_BB "\n", blockPred->bbNum,
               block->bbNum);
        assert(!"Jump into the middle of handler region");
        return false;
    }

    bool CheckJump(BasicBlock* blockPred, BasicBlock* block)
    {
        switch (blockPred->bbJumpKind)
        {
            case BBJ_COND:
                assert(blockPred->bbNext == block || blockPred->bbJumpDest == block);
                return true;

            case BBJ_NONE:
                assert(blockPred->bbNext == block);
                return true;

            case BBJ_CALLFINALLY:
            case BBJ_ALWAYS:
            case BBJ_EHCATCHRET:
            case BBJ_EHFILTERRET:
                assert(blockPred->bbJumpDest == block);
                return true;

            case BBJ_EHFINALLYRET:
                assert(CheckEHFinallyRet(blockPred, block));
                return true;

            case BBJ_THROW:
            case BBJ_RETURN:
                assert(!"THROW and RETURN block cannot be in the predecessor list!");
                break;

            case BBJ_SWITCH:
                for (BasicBlock* const bTarget : blockPred->SwitchTargets())
                {
                    if (block == bTarget)
                    {
                        return true;
                    }
                }
                assert(!"SWITCH in the predecessor list with no jump label to BLOCK!");
                break;

            default:
                assert(!"Unexpected bbJumpKind");
                break;
        }
        return false;
    }

    bool CheckEHFinallyRet(BasicBlock* blockPred, BasicBlock* block)
    {
        // If the current block is a successor to a BBJ_EHFINALLYRET (return from finally),
        // then the lexically previous block should be a call to the same finally.
        // Verify all of that.

        unsigned    hndIndex = blockPred->getHndIndex();
        EHblkDsc*   ehDsc    = comp->ehGetDsc(hndIndex);
        BasicBlock* finBeg   = ehDsc->ebdHndBeg;

        // Because there is no bbPrev, we have to search for the lexically previous
        // block.  We can shorten the search by only looking in places where it is legal
        // to have a call to the finally.

        BasicBlock* begBlk;
        BasicBlock* endBlk;
        comp->ehGetCallFinallyBlockRange(hndIndex, &begBlk, &endBlk);

        for (BasicBlock* bcall = begBlk; bcall != endBlk; bcall = bcall->bbNext)
        {
            if (bcall->bbJumpKind != BBJ_CALLFINALLY || bcall->bbJumpDest != finBeg)
            {
                continue;
            }

            if (block == bcall->bbNext)
            {
                return true;
            }
        }

#ifdef FEATURE_EH_FUNCLETS
        if (comp->fgFuncletsCreated)
        {
            // There is no easy way to search just the funclets that were pulled out of
            // the corresponding try body, so instead we search all the funclets, and if
            // we find a potential 'hit' we check if the funclet we're looking at is
            // from the correct try region.

            for (BasicBlock* const bcall : comp->Blocks(comp->fgFirstFuncletBB))
            {
                if (bcall->bbJumpKind != BBJ_CALLFINALLY || bcall->bbJumpDest != finBeg)
                {
                    continue;
                }

                if (block != bcall->bbNext)
                {
                    continue;
                }

                if (comp->ehCallFinallyInCorrectRegion(bcall, hndIndex))
                {
                    return true;
                }
            }
        }
#endif // FEATURE_EH_FUNCLETS

        assert(!"BBJ_EHFINALLYRET predecessor of block that doesn't follow a BBJ_CALLFINALLY!");
        return false;
    }
};

void Compiler::fgDebugCheckBBlist(bool checkBBNum, bool checkBBRefs)
{
    fgDebugCheckBlockLinks();
    fgFirstBBisScratch();

    if (fgBBcount > 10000 && expensiveDebugCheckLevel < 1)
    {
        // The basic block checks are too expensive if there are too many blocks,
        // so give up unless we've been told to try hard.
        return;
    }

#ifdef FEATURE_EH_FUNCLETS
    bool reachedFirstFunclet = false;

    if (fgFuncletsCreated)
    {
        // Make sure that fgFirstFuncletBB is accurate.
        // It should be the first basic block in a handler region.

        if (fgFirstFuncletBB != nullptr)
        {
            assert(fgFirstFuncletBB->hasHndIndex() == true);
            assert(fgFirstFuncletBB->bbFlags & BBF_FUNCLET_BEG);
        }
    }
#endif // FEATURE_EH_FUNCLETS

    // This variable is used to generate "traversal labels": one-time constants with which
    // we label basic blocks that are members of the basic block list, in order to have a
    // fast, high-probability test for membership in that list.  Type is "volatile" because
    // it's incremented with an atomic operation, which wants a volatile type; "long" so that
    // wrap-around to 0 (which I think has the highest probability of accidental collision) is
    // postponed a *long* time.
    static volatile LONG traverseLabel     = 1;
    unsigned             curTraversalStamp = static_cast<unsigned>(InterlockedIncrement(&traverseLabel));

    // Check bbNum, bbRefs and bbPreds
    // First, pick a traversal stamp, and label all the blocks with it.

    for (BasicBlock* const block : Blocks())
    {
        block->bbTraversalStamp = curTraversalStamp;
    }

    for (BasicBlock* const block : Blocks())
    {
        if (checkBBNum)
        {
            // Check that bbNum is sequential
            assert(block->bbNext == nullptr || (block->bbNum + 1 == block->bbNext->bbNum));
        }

        // If the block is a BBJ_COND, a BBJ_SWITCH or a
        // lowered GT_SWITCH_TABLE node then make sure it
        // ends with a conditional jump or a GT_SWITCH

        if (block->KindIs(BBJ_COND))
        {
            assert((block->lastNode()->gtNext == nullptr) && block->lastNode()->OperIsConditionalJump());
        }
        else if (block->KindIs(BBJ_SWITCH))
        {
            assert((block->lastNode()->gtNext == nullptr) && block->lastNode()->OperIs(GT_SWITCH, GT_SWITCH_TABLE));
        }

        if (block->bbCatchTyp == BBCT_FILTER)
        {
            if (!fgCheapPredsValid) // Don't check cheap preds
            {
                // A filter has no predecessors
                assert(block->bbPreds == nullptr);
            }
        }

#ifdef FEATURE_EH_FUNCLETS
        if (fgFuncletsCreated)
        {
            //
            // There should be no handler blocks until
            // we get to the fgFirstFuncletBB block,
            // then every block should be a handler block
            //
            if (!reachedFirstFunclet)
            {
                if (block == fgFirstFuncletBB)
                {
                    assert(block->hasHndIndex() == true);
                    reachedFirstFunclet = true;
                }
                else
                {
                    assert(block->hasHndIndex() == false);
                }
            }
            else // reachedFirstFunclet
            {
                assert(block->hasHndIndex() == true);
            }
        }
#endif // FEATURE_EH_FUNCLETS

        if (checkBBRefs)
        {
            assert(fgComputePredsDone);
        }

        BBPredsChecker checker(this);
        unsigned       blockRefs = checker.CheckBBPreds(block, curTraversalStamp);

        // First basic block has an additional global incoming edge.
        if (block == fgFirstBB)
        {
            blockRefs += 1;
        }

        // Under OSR, if we also are keeping the original method entry around,
        // mark that as implicitly referenced as well.
        if (opts.IsOSR() && (block == fgEntryBB))
        {
            blockRefs += 1;
        }

        /* Check the bbRefs */
        if (checkBBRefs)
        {
            if (block->bbRefs != blockRefs)
            {
                // Check to see if this block is the beginning of a filter or a handler and adjust the ref count
                // appropriately.
                for (EHblkDsc* const HBtab : EHClauses(this))
                {
                    if (HBtab->ebdHndBeg == block)
                    {
                        blockRefs++;
                    }
                    if (HBtab->HasFilter() && (HBtab->ebdFilter == block))
                    {
                        blockRefs++;
                    }
                }
            }

            assert(block->bbRefs == blockRefs);
        }

        /* Check that BBF_HAS_HANDLER is valid bbTryIndex */
        if (block->hasTryIndex())
        {
            assert(block->getTryIndex() < compHndBBtabCount);
        }

        // A branch or fall-through to a BBJ_CALLFINALLY block must come from the `try` region associated
        // with the finally block the BBJ_CALLFINALLY is targeting. There is one special case: if the
        // BBJ_CALLFINALLY is the first block of a `try`, then its predecessor can be outside the `try`:
        // either a branch or fall-through to the first block.
        //
        // Note that this IR condition is a choice. It naturally occurs when importing EH constructs.
        // This condition prevents flow optimizations from skipping blocks in a `try` and branching
        // directly to the BBJ_CALLFINALLY. Relaxing this constraint would require careful thinking about
        // the implications, such as data flow optimizations.
        //
        // Don't depend on predecessors list for the check.
        for (BasicBlock* const succBlock : block->Succs())
        {
            if (succBlock->bbJumpKind == BBJ_CALLFINALLY)
            {
                BasicBlock* finallyBlock = succBlock->bbJumpDest;
                assert(finallyBlock->hasHndIndex());
                unsigned finallyIndex = finallyBlock->getHndIndex();

                // Now make sure the block branching to the BBJ_CALLFINALLY is in the correct region. The branch
                // to the BBJ_CALLFINALLY can come from the try region of the finally block, or from a more nested
                // try region, e.g.:
                //    try {
                //        try {
                //            LEAVE L_OUTER; // this becomes a branch to a BBJ_CALLFINALLY in an outer try region
                //                           // (in the FEATURE_EH_CALLFINALLY_THUNKS case)
                //        } catch {
                //        }
                //    } finally {
                //    }
                //    L_OUTER:
                //
                EHblkDsc* ehDsc = ehGetDsc(finallyIndex);
                if (ehDsc->ebdTryBeg == succBlock)
                {
                    // The BBJ_CALLFINALLY is the first block of it's `try` region. Don't check the predecessor.
                    // Note that this case won't occur in the FEATURE_EH_CALLFINALLY_THUNKS case, since the
                    // BBJ_CALLFINALLY in that case won't exist in the `try` region of the `finallyIndex`.
                }
                else
                {
                    assert(bbInTryRegions(finallyIndex, block));
                }
            }
        }

        /* Check if BBF_RUN_RARELY is set that we have bbWeight of zero */
        if (block->isRunRarely())
        {
            assert(block->bbWeight == BB_ZERO_WEIGHT);
        }
        else
        {
            assert(block->bbWeight > BB_ZERO_WEIGHT);
        }
    }

    // Make sure the one return BB is not changed.
    if (genReturnBB != nullptr)
    {
        assert(genReturnBB->GetFirstLIRNode() != nullptr || genReturnBB->bbStmtList != nullptr);
    }

    if (info.compIsStatic)
    {
        assert(lvaThisLclNum == BAD_VAR_NUM);
    }
    else
    {
#ifndef JIT32_GCENCODER
        // The general encoder/decoder (currently) only reports "this" as a generics context as a stack location,
        // so we mark info.compThisArg as lvAddrTaken to ensure that it is not enregistered. Otherwise, it should
        // not be address-taken. This variable determines if the address-taken-ness of this param is OK.
        const bool genericsContextIsThis = info.ThisParamIsGenericsContext();
#else
        const bool genericsContextIsThis = false;
#endif

        LclVarDsc* thisParam = lvaGetDesc(info.GetThisParamLclNum());
        LclVarDsc* thisLcl   = lvaGetDesc(lvaThisLclNum);

        bool thisAddrExposedOK = !thisParam->IsAddressExposed();
#ifndef JIT32_GCENCODER
        thisAddrExposedOK = thisAddrExposedOK || genericsContextIsThis;
#endif

        // This param should never be address taken or stored to.
        // In addition, lvaThisLclNum should remain 0 if this param
        // is not address taken or stored to.
        assert(thisAddrExposedOK && !thisParam->lvHasILStoreOp &&
               ((lvaThisLclNum == info.GetThisParamLclNum()) ||
                (thisLcl->IsAddressExposed() || thisLcl->lvHasILStoreOp || genericsContextIsThis)));
    }
}

void Compiler::fgDebugCheckFlags(GenTree* tree)
{
    class CheckFlagsTreeVisitor : public GenTreeVisitor<CheckFlagsTreeVisitor>
    {
        Compiler* m_compiler;
        ArrayStack<GenTree*, 16> m_operands;

    public:
        enum
        {
            DoPreOrder  = true,
            DoPostOrder = true,
        };

        CheckFlagsTreeVisitor(Compiler* compiler)
            : m_compiler(compiler), m_operands(compiler->getAllocator(CMK_DebugOnly))
        {
        }

        GenTreeWalkResult PreOrderVisit(GenTree** use, GenTree* user)
        {
            m_operands.Push(*use);
            return GenTreeWalkResult::Continue;
        }

        GenTreeWalkResult PostOrderVisit(GenTree** use, GenTree* user)
        {
            GenTree*     node          = *use;
            GenTreeFlags expectedFlags = GTF_NONE;

            while (m_operands.Top() != node)
            {
                expectedFlags |= m_operands.Top()->GetSideEffects();
                m_operands.Pop();
            }

            if (node->OperMayThrow(m_compiler))
            {
                expectedFlags |= GTF_EXCEPT;
            }

            if (node->OperRequiresCallFlag(m_compiler))
            {
                expectedFlags |= GTF_CALL;
            }

            if (node->OperRequiresAsgFlag())
            {
                expectedFlags |= GTF_ASG;

                if (node->OperIsAtomicOp() || node->OperIs(GT_MEMORYBARRIER) || node->IsDynBlk())
                {
                    expectedFlags |= GTF_GLOB_REF;
                }
            }
            else
            {
                assert(!node->OperIsAtomicOp() && !node->OperIs(GT_MEMORYBARRIER) && !node->IsDynBlk());
            }

            if ((node->IsIndir() && node->AsIndir()->IsVolatile()) ||
                (node->IsDynBlk() && node->AsDynBlk()->IsVolatile()))
            {
                expectedFlags |= GTF_ORDER_SIDEEFF;
            }

            if (node->IsLclRef() && !node->IsLclAddr())
            {
                if (node->AsLclRef()->GetLcl()->IsAddressExposed())
                {
                    expectedFlags |= GTF_GLOB_REF;
                }
            }

            // TODO-MIKE-Review: This should require GLOB_REF for
            // OBJ/BLK, indirect stores and load/store intrinsics.
            // It remains to be seen if there aren't any cases where
            // those do not need GLOB_REF, though that's unlikely.
            // In general, all indirs should have GLOB_REF, with the
            // exception of loads of runtime data that is known to be
            // invariant.

            GenTreeFlags actualFlags = node->GetSideEffects();

            switch (node->GetOper())
            {
                case GT_CATCH_ARG:
                    expectedFlags |= GTF_ORDER_SIDEEFF;
                    break;

                case GT_IND_LOAD:
                    if (GenTreeIntCon* addr = node->AsIndLoad()->GetAddr()->IsIntCon())
                    {
                        HandleKind handleKind = addr->GetHandleKind();

                        if (handleKind != HandleKind::None)
                        {
                            if ((node->gtFlags & GTF_IND_INVARIANT) != 0)
                            {
                                expectedFlags |= GTF_IND_INVARIANT;
                            }

                            // We currently expect all handles to be non-null.
                            assert((node->gtFlags & GTF_IND_NONFAULTING) != 0);

                            actualFlags |= GTF_IND_NONFAULTING;
                            expectedFlags |= GTF_IND_NONFAULTING;

                            if ((handleKind != HandleKind::Static) && (handleKind != HandleKind::BlockCount) &&
                                (handleKind != HandleKind::MutableData))
                            {
                                actualFlags |= GTF_IND_INVARIANT;
                            }

                            if (handleKind == HandleKind::Static)
                            {
                                actualFlags |= GTF_GLOB_REF;
                            }
                        }
                    }
                    break;

                case GT_CALL:
                    // Calls may have argument "setup" trees that are stores but
                    // their GTF_ASG side effect is not inherited by the call node.
                    // Struct args can have complicated setup, where the temp store
                    // is hidden inside a COMMA/FIELD_LIST, so we'll simply check
                    // for GTF_ASG instead of a store node.

                    for (GenTreeUse& use : node->AsCall()->Uses())
                    {
                        if (use.GetNode()->HasAnySideEffect(GTF_ASG))
                        {
                            actualFlags |= GTF_ASG;
                        }
                    }
                    break;

                default:
                    break;
            }

            CheckFlags(node, actualFlags, expectedFlags);

            return GenTreeWalkResult::Continue;
        }

    private:
        void CheckFlags(GenTree* node, GenTreeFlags actualFlags, GenTreeFlags expectedFlags)
        {
            GenTreeFlags missingFlags = expectedFlags & ~actualFlags;
            // We can't treat GTF_GLOB_REF or GTF_ORDER_SIDEEFF as being "extra" flags
            // because we currently have no way to figure out when they're required.
            GenTreeFlags extraFlags = actualFlags & ~(expectedFlags | GTF_GLOB_REF | GTF_ORDER_SIDEEFF);

            if (missingFlags != GTF_NONE)
            {
                printf("Missing flags on tree [%06u]: ", node->GetID());
                DumpFlags(node, missingFlags);
                printf("\n");
                m_compiler->gtDispTree(node);

                assert(!"Missing flags on tree");
            }
            else if (extraFlags != GTF_NONE)
            {
                printf("Extra flags on tree [%06u]: ", node->GetID());
                DumpFlags(node, extraFlags);
                printf("\n");
                m_compiler->gtDispTree(node);

                assert(!"Extra flags on tree");
            }
        }

        void DumpFlags(GenTree* node, GenTreeFlags flags)
        {
            if (node->OperIs(GT_IND_LOAD))
            {
                printf("%c", (flags & GTF_IND_INVARIANT) ? '#' : '-');
                printf("%c", (flags & GTF_IND_NONFAULTING) ? 'n' : '-');
                printf("%c", (flags & GTF_IND_NONNULL) ? '@' : '-');
            }

            m_compiler->gtDispFlags(flags, GTF_DEBUG_NONE);
        }
    } visitor(this);

    visitor.WalkTree(&tree, nullptr);
}

// Check correctness of the internal gtNext, gtPrev threading of a statement.
// This threading is only valid when fgStmtListThreaded is true.
// This calls an alternate method for FGOrderLinear.
void Compiler::fgDebugCheckNodeLinks(BasicBlock* block, Statement* stmt)
{
    // LIR blocks are checked using BasicBlock::CheckLIR().
    if (block->IsLIR())
    {
        LIR::AsRange(block).CheckLIR(this);
        // TODO: return?
    }

    assert(fgStmtListThreaded);

    noway_assert(stmt->GetNodeList());

    // The first node's gtPrev must be nullptr (the gtPrev list is not circular).
    // The last node's gtNext must be nullptr (the gtNext list is not circular). This is tested if the loop below
    // terminates.
    assert(stmt->GetNodeList()->gtPrev == nullptr);

    for (GenTree* tree = stmt->GetNodeList(); tree != nullptr; tree = tree->gtNext)
    {
        if (tree->gtPrev)
        {
            noway_assert(tree->gtPrev->gtNext == tree);
        }
        else
        {
            noway_assert(tree == stmt->GetNodeList());
        }

        if (tree->gtNext)
        {
            noway_assert(tree->gtNext->gtPrev == tree);
        }
        else
        {
            noway_assert(tree == stmt->GetRootNode());
        }

        // Cross-check gtPrev,gtNext with GetOp() for simple trees

        GenTree* expectedPrevTree = nullptr;

        if (tree->OperIsLeaf())
        {
            if (tree->OperIs(GT_CATCH_ARG))
            {
                noway_assert(tree->HasAnySideEffect(GTF_ORDER_SIDEEFF));
                noway_assert(stmt == block->FirstNonPhiDef());
                noway_assert(stmt->GetNodeList()->OperIs(GT_CATCH_ARG));
                noway_assert(stmt->GetRootNode()->HasAnySideEffect(GTF_ORDER_SIDEEFF));
            }
        }
        else if (tree->OperIsUnary() && (tree->AsUnOp()->gtOp1 != nullptr))
        {
            expectedPrevTree = tree->AsUnOp()->GetOp(0);
        }
        else if (tree->OperIsBinary() && (tree->AsOp()->gtOp1 != nullptr))
        {
            expectedPrevTree = (tree->AsOp()->gtOp2 == nullptr) || tree->IsReverseOp() ? tree->AsOp()->GetOp(0)
                                                                                       : tree->AsOp()->GetOp(1);
        }

        noway_assert(expectedPrevTree == nullptr ||     // No expectations about the prev node
                     tree->gtPrev == expectedPrevTree); // The "normal" case
    }
}

// Check the correctness of the links between statements
// and ordinary nodes within a statement.
void Compiler::fgDebugCheckLinks(bool morphTrees)
{
    // This used to be only on for stress, and there was a comment stating that
    // it was "quite an expensive operation" but I did not find that to be true.
    // Set DO_SANITY_DEBUG_CHECKS to false to revert to that behavior.
    const bool DO_SANITY_DEBUG_CHECKS = true;

    if (!DO_SANITY_DEBUG_CHECKS && !compStressCompile(STRESS_CHK_FLOW_UPDATE, 30))
    {
        return;
    }

    fgDebugCheckBlockLinks();

    // For each block check the links between the trees.
    for (BasicBlock* const block : Blocks())
    {
        if (block->IsLIR())
        {
            LIR::AsRange(block).CheckLIR(this);
        }
        else
        {
            fgDebugCheckStmtsList(block, morphTrees);
        }
    }

    fgDebugCheckNodesUniqueness();
}

// Performs the set of checks:
//    - all statements in the block are linked correctly
//    - check statements flags
//    - check nodes gtNext and gtPrev values, if the node list is threaded
//
// Arguments:
//    block  - the block to check statements in
//    morphTrees - try to morph trees in the checker
//
// Checking that all bits that are set in treeFlags are also set in chkFlags is currently disabled.
void Compiler::fgDebugCheckStmtsList(BasicBlock* block, bool morphTrees)
{
    assert(!block->IsLIR());

    for (Statement* const stmt : block->Statements())
    {
        // Verify that bbStmtList is threaded correctly.
        // Note that for the statements list, the GetPrevStmt() list is circular.
        // The GetNextStmt() list is not: GetNextStmt() of the last statement in a block is nullptr.

        assert(stmt->GetPrevStmt() != nullptr);

        if (stmt == block->bbStmtList)
        {
            assert(stmt->GetPrevStmt()->GetNextStmt() == nullptr);
        }
        else
        {
            assert(stmt->GetPrevStmt()->GetNextStmt() == stmt);
        }

        if (stmt->GetNextStmt() != nullptr)
        {
            assert(stmt->GetNextStmt()->GetPrevStmt() == stmt);
        }
        else
        {
            assert(block->lastStmt() == stmt);
        }

        fgDebugCheckFlags(stmt->GetRootNode());

        // Not only will this stress moMorphBlockStmt, but we also get all the checks
        // done by moMorphTree.

        if (morphTrees)
        {
            // If 'stmt' is removed from the block, start a new check for the current block,
            // break the current check.
            if (moMorphBlockStmt(block, stmt DEBUGARG("test morphing")))
            {
                fgDebugCheckStmtsList(block, morphTrees);
                break;
            }

            if (fgStmtListThreaded)
            {
                gtSetCosts(stmt->GetRootNode());
                gtSetStmtOrder(stmt);
            }
        }

        // For each statement check that the nodes are threaded correctly - m_treeList.
        if (fgStmtListThreaded)
        {
            fgDebugCheckNodeLinks(block, stmt);
            gtCheckTreeSeq(stmt->GetRootNode(), false);
        }
    }
}

// Ensure that bbNext and bbPrev are consistent
void Compiler::fgDebugCheckBlockLinks()
{
    assert(fgFirstBB->bbPrev == nullptr);

    for (BasicBlock* const block : Blocks())
    {
        if (block->bbNext)
        {
            assert(block->bbNext->bbPrev == block);
        }
        else
        {
            assert(block == fgLastBB);
        }

        if (block->bbPrev)
        {
            assert(block->bbPrev->bbNext == block);
        }
        else
        {
            assert(block == fgFirstBB);
        }

        // If this is a switch, check that the tables are consistent.
        // Note that we don't call GetSwitchDescMap(), because it has the side-effect
        // of allocating it if it is not present.
        if ((block->bbJumpKind == BBJ_SWITCH) && (block->bbJumpSwt->nonDuplicates != nullptr))
        {
            // Create a set with all the successors. Don't use BlockSet, so we don't need to worry
            // about the BlockSet version.

            BitVecTraits uniqueSuccSetTraits(fgBBNumMax + 1, this);
            BitVec       uniqueSuccSet   = BitVecOps::MakeEmpty(uniqueSuccSetTraits);
            unsigned     uniqueSuccCount = 0;

            for (BasicBlock* const succ : block->SwitchTargets())
            {
                uniqueSuccCount += BitVecOps::TryAddElemD(uniqueSuccSetTraits, uniqueSuccSet, succ->bbNum);
            }

            assert(block->bbJumpSwt->numDistinctSuccs == uniqueSuccCount);

            for (unsigned i = 0; i < uniqueSuccCount; i++)
            {
                assert(
                    BitVecOps::IsMember(uniqueSuccSetTraits, uniqueSuccSet, block->bbJumpSwt->nonDuplicates[i]->bbNum));
            }
        }
    }
}

// Check that each tree in the method has its own unique gtTreeId.
void Compiler::fgDebugCheckNodesUniqueness()
{
    class UniquenessCheckWalker
    {
        Compiler*    comp;
        BitVecTraits uniqueNodesTraits;
        BitVec       uniqueNodes;

    public:
        UniquenessCheckWalker(Compiler* comp)
            : comp(comp)
            , uniqueNodesTraits(comp->compGenTreeID, comp)
            , uniqueNodes(BitVecOps::MakeEmpty(uniqueNodesTraits))
        {
        }

        static GenTreeWalkResult MarkTreeId(GenTree** use, GenTree* user, void* data)
        {
            UniquenessCheckWalker* walker = static_cast<UniquenessCheckWalker*>(data);
            walker->CheckTreeId(*use);
            return GenTreeWalkResult::Continue;
        }

        void CheckTreeId(GenTree* node)
        {
            if (!BitVecOps::TryAddElemD(uniqueNodesTraits, uniqueNodes, node->GetID()))
            {
                JITDUMP("Duplicate node ID was found: %u\n", node->GetID());
                assert(!"Duplicate node ID was found");
            }
        }
    };

    UniquenessCheckWalker walker(this);

    for (BasicBlock* const block : Blocks())
    {
        if (block->IsLIR())
        {
            for (GenTree* node : LIR::AsRange(block))
            {
                walker.CheckTreeId(node);
            }
        }
        else
        {
            for (Statement* const stmt : block->Statements())
            {
                GenTree* root = stmt->GetRootNode();
                fgWalkTreePre(&root, UniquenessCheckWalker::MarkTreeId, &walker);
            }
        }
    }
}

// Checks that the loop table is valid.
//    - If the method has natural loops, the loop table is not null
//    - All basic blocks with loop numbers set have a corresponding loop in the table
//    - All basic blocks without a loop number are not in a loop
//    - All parents of the loop with the block contain that block
//
void Compiler::fgDebugCheckLoopTable() const
{
    if (optLoopCount > 0)
    {
        assert(optLoopTable != nullptr);
    }

    for (BasicBlock* const block : Blocks())
    {
        if (optLoopCount == 0)
        {
            assert(block->bbNatLoopNum == BasicBlock::NOT_IN_LOOP);
            continue;
        }

        // Walk the loop table and find the first loop that contains our block.
        // It should be the innermost one.
        int loopNum = BasicBlock::NOT_IN_LOOP;
        for (int i = optLoopCount - 1; i >= 0; i--)
        {
            // Ignore removed loops
            if (optLoopTable[i].lpFlags & LPFLG_REMOVED)
            {
                continue;
            }
            // Does this loop contain our block?
            if (optLoopTable[i].lpContains(block))
            {
                loopNum = i;
                break;
            }
        }

        // If there is at least one loop that contains this block...
        if (loopNum != BasicBlock::NOT_IN_LOOP)
        {
            // ...it must be the one pointed to by bbNatLoopNum.
            assert(block->bbNatLoopNum == loopNum);
        }
        else
        {
            // Otherwise, this block should not point to a loop.
            assert(block->bbNatLoopNum == BasicBlock::NOT_IN_LOOP);
        }

        // All loops that contain the innermost loop with this block must also contain this block.
        while (loopNum != BasicBlock::NOT_IN_LOOP)
        {
            assert(optLoopTable[loopNum].lpContains(block));

            loopNum = optLoopTable[loopNum].lpParent;
        }
    }
}

#endif // DEBUG

#ifdef DUMP_FLOWGRAPHS

struct EscapeMapping
{
    char        ch;
    const char* sub;
};

// clang-format off
static EscapeMapping s_FilePathEscapeMapping[]
{
    {':', "="},
    {'<', "["},
    {'>', "]"},
    {';', "~semi~"},
    {'|', "~bar~"},
    {'&', "~amp~"},
    {'"', "~quot~"},
    {'*', "~star~"},
    {0, nullptr}
};

static EscapeMapping s_XmlEscapeMapping[]
{
    {'<', "&lt;"},
    {'>', "&gt;"},
    {'&', "&amp;"},
    {'"', "&quot;"},
    {0, nullptr}
};
// clang-format on

static const char* EscapeString(const char* nameIn, const EscapeMapping* map, CompAllocator allocator)
{
    const char* nameOut = nameIn;
    unsigned    lengthOut;
    unsigned    index;
    bool        match;
    bool        subsitutionRequired;
    const char* pChar;

    lengthOut           = 1;
    subsitutionRequired = false;
    pChar               = nameIn;
    while (*pChar != '\0')
    {
        match = false;
        index = 0;
        while (map[index].ch != 0)
        {
            if (*pChar == map[index].ch)
            {
                match = true;
                break;
            }
            index++;
        }
        if (match)
        {
            subsitutionRequired = true;
            lengthOut += (unsigned)strlen(map[index].sub);
        }
        else
        {
            lengthOut += 1;
        }
        pChar++;
    }

    if (subsitutionRequired)
    {
        char* newName = allocator.allocate<char>(lengthOut);
        char* pDest;
        pDest = newName;
        pChar = nameIn;
        while (*pChar != '\0')
        {
            match = false;
            index = 0;
            while (map[index].ch != 0)
            {
                if (*pChar == map[index].ch)
                {
                    match = true;
                    break;
                }
                index++;
            }
            if (match)
            {
                strcpy(pDest, map[index].sub);
                pDest += strlen(map[index].sub);
            }
            else
            {
                *pDest++ = *pChar;
            }
            pChar++;
        }
        *pDest++ = '\0';
        nameOut  = (const char*)newName;
    }

    return nameOut;
}

static void DumpDotDouble(FILE* fgxFile, double value)
{
    assert(value >= 0.0);

    if ((value >= 0.010) || (value == 0.0))
    {
        fprintf(fgxFile, "\"%7.3f\"", value);
    }
    else if (value >= 0.00010)
    {
        fprintf(fgxFile, "\"%7.5f\"", value);
    }
    else
    {
        fprintf(fgxFile, "\"%7E\"", value);
    }
}

// Dump a tree into the DOT file. Used to provide a very short, one-line,
// visualization of a BBJ_COND block.
static void DumpDotTree(FILE* fgxFile, GenTree* const tree)
{
    if (tree->OperIsRelop())
    {
        DumpDotTree(fgxFile, tree->AsOp()->GetOp(0));
        fprintf(fgxFile, " %s ", GenTree::OpName(tree->GetOper()));
        DumpDotTree(fgxFile, tree->AsOp()->GetOp(1));
    }
    else if (GenTreeIntCon* intCon = tree->IsIntCon())
    {
        fprintf(fgxFile, "%d", intCon->GetValue());
    }
    else if (GenTreeDblCon* dblCon = tree->IsDblCon())
    {
        fprintf(fgxFile, "%g", dblCon->GetValue());
    }
    else if (GenTreeLclLoad* lclLoad = tree->IsLclLoad())
    {
        fprintf(fgxFile, FMT_LCL, lclLoad->GetLcl()->GetLclNum());
    }
    else if (GenTreeArrLen* arrLen = tree->IsArrLen())
    {
        DumpDotTree(fgxFile, arrLen->GetArray());
        fprintf(fgxFile, ".Length");
    }
    else
    {
        fprintf(fgxFile, "[%s]", GenTree::OpName(tree->GetOper()));
    }
}

// Open a file to dump either the xml or dot format flow graph
//
// The filename to use to write the data comes from the COMPlus_JitDumpFgFile or COMPlus_NgenDumpFgFile
// configuration. If unset, use "default". The "type" argument is used as a filename extension,
// e.g., "default.dot".
//
// There are several "special" filenames recognized:
// "profiled" -- only create graphs for methods with profile info, one file per method.
// "hot" -- only create graphs for the hot region, one file per method.
// "cold" -- only create graphs for the cold region, one file per method.
// "jit" -- only create graphs for JITing, one file per method.
// "all" -- create graphs for all regions, one file per method.
// "stdout" -- output to stdout, not a file.
// "stderr" -- output to stderr, not a file.
static FILE* OpenFlowGraphFile(Compiler* compiler, bool* wbDontClose, Phases phase, PhasePosition pos, LPCWSTR type)
{
    FILE*       fgxFile;
    LPCWSTR     prePhasePattern  = nullptr; // pre-phase:  default (used in Release) is no pre-phase dump
    LPCWSTR     postPhasePattern = W("*");  // post-phase: default (used in Release) is dump all phases
    bool        dumpFunction     = true;    // default (used in Release) is always dump
    LPCWSTR     filename         = nullptr;
    LPCWSTR     pathname         = nullptr;
    const char* escapedString;
    bool        createDuplicateFgxFiles = true;

    if (compiler->fgBBcount <= 1)
    {
        return nullptr;
    }

    CompiledMethodInfo& info = compiler->info;

#ifdef DEBUG
    if (compiler->opts.IsJitFlagSet(JitFlags::JIT_FLAG_PREJIT))
    {
        dumpFunction =
            JitConfig.NgenDumpFg().contains(info.compMethodName, info.compClassName, &info.compMethodInfo->args);
        filename = JitConfig.NgenDumpFgFile();
        pathname = JitConfig.NgenDumpFgDir();
    }
    else
    {
        dumpFunction =
            JitConfig.JitDumpFg().contains(info.compMethodName, info.compClassName, &info.compMethodInfo->args);
        filename = JitConfig.JitDumpFgFile();
        pathname = JitConfig.JitDumpFgDir();
    }

    prePhasePattern  = JitConfig.JitDumpFgPrePhase();
    postPhasePattern = JitConfig.JitDumpFgPhase();
#endif // DEBUG

    if (!dumpFunction)
    {
        return nullptr;
    }

    LPCWSTR phaseName = PhaseShortNames[phase];

    if (pos == PhasePosition::PrePhase)
    {
        if (prePhasePattern == nullptr)
        {
            // If pre-phase pattern is not specified, then don't dump for any pre-phase.
            return nullptr;
        }
        else if (*prePhasePattern != W('*'))
        {
            if (wcsstr(prePhasePattern, phaseName) == nullptr)
            {
                return nullptr;
            }
        }
    }
    else
    {
        assert(pos == PhasePosition::PostPhase);
        if (postPhasePattern == nullptr)
        {
            // There's no post-phase pattern specified. If there is a pre-phase pattern specified, then that will
            // be the only set of phases dumped. If neither are specified, then post-phase dump after
            // PHASE_DETERMINE_FIRST_COLD_BLOCK.
            if (prePhasePattern != nullptr)
            {
                return nullptr;
            }
            if (phase != PHASE_DETERMINE_FIRST_COLD_BLOCK)
            {
                return nullptr;
            }
        }
        else if (*postPhasePattern != W('*'))
        {
            if (wcsstr(postPhasePattern, phaseName) == nullptr)
            {
                return nullptr;
            }
        }
    }

    if (filename == nullptr)
    {
        filename = W("default");
    }

    if (wcscmp(filename, W("profiled")) == 0)
    {
        if (compiler->fgFirstBB->hasProfileWeight())
        {
            createDuplicateFgxFiles = true;
            goto ONE_FILE_PER_METHOD;
        }
        else
        {
            return nullptr;
        }
    }
    if (wcscmp(filename, W("hot")) == 0)
    {
        if (info.compMethodInfo->regionKind == CORINFO_REGION_HOT)

        {
            createDuplicateFgxFiles = true;
            goto ONE_FILE_PER_METHOD;
        }
        else
        {
            return nullptr;
        }
    }
    else if (wcscmp(filename, W("cold")) == 0)
    {
        if (info.compMethodInfo->regionKind == CORINFO_REGION_COLD)
        {
            createDuplicateFgxFiles = true;
            goto ONE_FILE_PER_METHOD;
        }
        else
        {
            return nullptr;
        }
    }
    else if (wcscmp(filename, W("jit")) == 0)
    {
        if (info.compMethodInfo->regionKind == CORINFO_REGION_JIT)
        {
            createDuplicateFgxFiles = true;
            goto ONE_FILE_PER_METHOD;
        }
        else
        {
            return nullptr;
        }
    }
    else if (wcscmp(filename, W("all")) == 0)
    {
        createDuplicateFgxFiles = true;

    ONE_FILE_PER_METHOD:;

        escapedString = EscapeString(info.compFullName, s_FilePathEscapeMapping, compiler->getAllocator(CMK_DebugOnly));

        const char* tierName = compiler->compGetTieringName(true);
        size_t      wCharCount =
            strlen(escapedString) + wcslen(phaseName) + 1 + strlen("~999") + wcslen(type) + strlen(tierName) + 1;
        if (pathname != nullptr)
        {
            wCharCount += wcslen(pathname) + 1;
        }
        filename = (LPCWSTR)alloca(wCharCount * sizeof(WCHAR));

        if (pathname != nullptr)
        {
            swprintf_s((LPWSTR)filename, wCharCount, W("%s\\%S-%s-%S.%s"), pathname, escapedString, phaseName, tierName,
                       type);
        }
        else
        {
            swprintf_s((LPWSTR)filename, wCharCount, W("%S.%s"), escapedString, type);
        }
        fgxFile = _wfopen(filename, W("r")); // Check if this file already exists
        if (fgxFile != nullptr)
        {
            // For Generic methods we will have both hot and cold versions
            if (createDuplicateFgxFiles == false)
            {
                fclose(fgxFile);
                return nullptr;
            }
            // Yes, this filename already exists, so create a different one by appending ~2, ~3, etc...
            for (int i = 2; i < 1000; i++)
            {
                fclose(fgxFile);
                if (pathname != nullptr)
                {
                    swprintf_s((LPWSTR)filename, wCharCount, W("%s\\%S~%d.%s"), pathname, escapedString, i, type);
                }
                else
                {
                    swprintf_s((LPWSTR)filename, wCharCount, W("%S~%d.%s"), escapedString, i, type);
                }
                fgxFile = _wfopen(filename, W("r")); // Check if this file exists
                if (fgxFile == nullptr)
                {
                    break;
                }
            }
            // If we have already created 1000 files with this name then just fail
            if (fgxFile != nullptr)
            {
                fclose(fgxFile);
                return nullptr;
            }
        }
        fgxFile      = _wfopen(filename, W("a+"));
        *wbDontClose = false;
    }
    else if (wcscmp(filename, W("stdout")) == 0)
    {
        fgxFile      = jitstdout;
        *wbDontClose = true;
    }
    else if (wcscmp(filename, W("stderr")) == 0)
    {
        fgxFile      = stderr;
        *wbDontClose = true;
    }
    else
    {
        LPCWSTR origFilename = filename;
        size_t  wCharCount   = wcslen(origFilename) + wcslen(type) + 2;
        if (pathname != nullptr)
        {
            wCharCount += wcslen(pathname) + 1;
        }
        filename = (LPCWSTR)alloca(wCharCount * sizeof(WCHAR));
        if (pathname != nullptr)
        {
            swprintf_s((LPWSTR)filename, wCharCount, W("%s\\%s.%s"), pathname, origFilename, type);
        }
        else
        {
            swprintf_s((LPWSTR)filename, wCharCount, W("%s.%s"), origFilename, type);
        }
        fgxFile      = _wfopen(filename, W("a+"));
        *wbDontClose = false;
    }

    return fgxFile;
}

//------------------------------------------------------------------------
// fgDumpFlowGraph: Dump the xml or dot format flow graph, if enabled for this phase.
//
// Arguments:
//    phase       - A phase identifier to indicate which phase is associated with the dump,
//                  i.e. which phase has just completed.
//    pos         - Are we being called to dump the flow graph pre-phase or post-phase?
//
// Return Value:
//    True iff a flowgraph has been dumped.
//
// Notes:
//    The xml dumps are the historical mechanism for dumping the flowgraph.
//    The dot format can be viewed by:
//    - https://sketchviz.com/
//    - Graphviz (http://www.graphviz.org/)
//      - The command:
//           "C:\Program Files (x86)\Graphviz2.38\bin\dot.exe" -Tsvg -oFoo.svg -Kdot Foo.dot
//        will produce a Foo.svg file that can be opened with any svg-capable browser.
//    - http://rise4fun.com/Agl/
//      - Cut and paste the graph from your .dot file, replacing the digraph on the page, and then click the play
//        button.
//      - It will show a rotating '/' and then render the graph in the browser.
//    MSAGL has also been open-sourced to https://github.com/Microsoft/automatic-graph-layout.
//
//    Here are the config values that control it:
//      COMPlus_JitDumpFg              A string (ala the COMPlus_JitDump string) indicating what methods to dump
//                                     flowgraphs for.
//      COMPlus_JitDumpFgDir           A path to a directory into which the flowgraphs will be dumped.
//      COMPlus_JitDumpFgFile          The filename to use. The default is "default.[xml|dot]".
//                                     Note that the new graphs will be appended to this file if it already exists.
//      COMPlus_NgenDumpFg             Same as COMPlus_JitDumpFg, but for ngen compiles.
//      COMPlus_NgenDumpFgDir          Same as COMPlus_JitDumpFgDir, but for ngen compiles.
//      COMPlus_NgenDumpFgFile         Same as COMPlus_JitDumpFgFile, but for ngen compiles.
//      COMPlus_JitDumpFgPhase         Phase(s) after which to dump the flowgraph.
//                                     Set to the short name of a phase to see the flowgraph after that phase.
//                                     Leave unset to dump after COLD-BLK (determine first cold block) or set to *
//                                     for all phases.
//      COMPlus_JitDumpFgPrePhase      Phase(s) before which to dump the flowgraph.
//      COMPlus_JitDumpFgDot           0 for xml format, non-zero for dot format. (Default is dot format.)
//      COMPlus_JitDumpFgEH            (dot only) 0 for no exception-handling information; non-zero to include
//                                     exception-handling regions.
//      COMPlus_JitDumpFgLoops         (dot only) 0 for no loop information; non-zero to include loop regions.
//      COMPlus_JitDumpFgConstrained   (dot only) 0 == don't constrain to mostly linear layout; non-zero == force
//                                     mostly lexical block linear layout.
//      COMPlus_JitDumpFgBlockId       Display blocks with block ID, not just bbNum.
//
// Example:
//
// If you want to dump just before and after a single phase, say loop cloning, use:
//      set COMPlus_JitDumpFgPhase=LP-CLONE
//      set COMPlus_JitDumpFgPrePhase=LP-CLONE
//
bool Compiler::fgDumpFlowGraph(Phases phase, PhasePosition pos)
{
    bool result    = false;
    bool dontClose = false;

#ifdef DEBUG
    const bool createDotFile = JitConfig.JitDumpFgDot() != 0;
    const bool includeEH     = (JitConfig.JitDumpFgEH() != 0) && !compIsForInlining();
    // The loop table is not well maintained after the optimization phases, but there is no single point at which
    // it is declared invalid. For now, refuse to add loop information starting at the rationalize phase, to
    // avoid asserts.
    const bool includeLoops = (JitConfig.JitDumpFgLoops() != 0) && !compIsForInlining() && (phase < PHASE_RATIONALIZE);
    const bool constrained  = JitConfig.JitDumpFgConstrained() != 0;
    const bool useBlockId   = JitConfig.JitDumpFgBlockID() != 0;
#else
    const bool createDotFile = true;
    const bool includeEH     = false;
    const bool includeLoops  = false;
    const bool constrained   = true;
    const bool useBlockId    = false;
#endif

    FILE* fgxFile = OpenFlowGraphFile(this, &dontClose, phase, pos, createDotFile ? W("dot") : W("fgx"));

    if (fgxFile == nullptr)
    {
        return false;
    }

    JITDUMP("Dumping flow graph %s phase %s\n", (pos == PhasePosition::PrePhase) ? "before" : "after",
            PhaseNames[phase]);

    bool        validWeights  = fgHaveValidEdgeWeights;
    double      weightDivisor = (double)BasicBlock::getCalledCount(this);
    const char* escapedString;
    const char* regionString = "NONE";

    if (info.compMethodInfo->regionKind == CORINFO_REGION_HOT)
    {
        regionString = "HOT";
    }
    else if (info.compMethodInfo->regionKind == CORINFO_REGION_COLD)
    {
        regionString = "COLD";
    }
    else if (info.compMethodInfo->regionKind == CORINFO_REGION_JIT)
    {
        regionString = "JIT";
    }

    if (createDotFile)
    {
        fprintf(fgxFile, "digraph FlowGraph {\n");
        fprintf(fgxFile, "    graph [label = \"%s%s\\n%s\\n%s\"];\n", info.compMethodName,
                compIsForInlining() ? "\\n(inlinee)" : "", (pos == PhasePosition::PrePhase) ? "before" : "after",
                PhaseNames[phase]);
        fprintf(fgxFile, "    node [shape = \"Box\"];\n");
    }
    else
    {
        fprintf(fgxFile, "<method");

        escapedString = EscapeString(info.compFullName, s_XmlEscapeMapping, getAllocator(CMK_DebugOnly));
        fprintf(fgxFile, "\n    name=\"%s\"", escapedString);

        escapedString = EscapeString(info.compClassName, s_XmlEscapeMapping, getAllocator(CMK_DebugOnly));
        fprintf(fgxFile, "\n    className=\"%s\"", escapedString);

        escapedString = EscapeString(info.compMethodName, s_XmlEscapeMapping, getAllocator(CMK_DebugOnly));
        fprintf(fgxFile, "\n    methodName=\"%s\"", escapedString);
        fprintf(fgxFile, "\n    ngenRegion=\"%s\"", regionString);

        fprintf(fgxFile, "\n    bytesOfIL=\"%d\"", info.compILCodeSize);
        fprintf(fgxFile, "\n    localVarCount=\"%d\"", lvaCount);

        if (fgHaveProfileData())
        {
            fprintf(fgxFile, "\n    calledCount=\"%f\"", fgCalledCount);
            fprintf(fgxFile, "\n    profileData=\"true\"");
        }
        if (compHndBBtabCount > 0)
        {
            fprintf(fgxFile, "\n    hasEHRegions=\"true\"");
        }
        if (fgHasLoops)
        {
            fprintf(fgxFile, "\n    hasLoops=\"true\"");
        }
        if (validWeights)
        {
            fprintf(fgxFile, "\n    validEdgeWeights=\"true\"");
            if (!fgSlopUsedInEdgeWeights && !fgRangeUsedInEdgeWeights)
            {
                fprintf(fgxFile, "\n    exactEdgeWeights=\"true\"");
            }
        }
        if (fgFirstColdBlock != nullptr)
        {
            fprintf(fgxFile, "\n    firstColdBlock=\"%d\"", fgFirstColdBlock->bbNum);
        }

        fprintf(fgxFile, ">");

        fprintf(fgxFile, "\n    <blocks");
        fprintf(fgxFile, "\n        blockCount=\"%d\"", fgBBcount);
        fprintf(fgxFile, ">");
    }

    // In some cases, we want to change the display based on whether an edge is lexically backwards, forwards,
    // or lexical successor. Also, for the region tree, using the lexical order is useful for determining where
    // to insert in the tree, to determine nesting. We'd like to use the bbNum to do this. However, we don't
    // want to renumber the blocks. So, create a mapping of bbNum to ordinal, and compare block order by
    // comparing the mapped ordinals instead.
    //
    // For inlinees, the max block number of the inliner is used, so we need to allocate the block map based on
    // that size, even though it means allocating a block map possibly much bigger than what's required for just
    // the inlinee blocks.

    unsigned  blkMapSize   = 1 + (compIsForInlining() ? impInlineInfo->InlinerCompiler->fgBBNumMax : fgBBNumMax);
    unsigned  blockOrdinal = 1;
    unsigned* blkMap       = new (this, CMK_DebugOnly) unsigned[blkMapSize];
    memset(blkMap, 0, sizeof(unsigned) * blkMapSize);
    for (BasicBlock* const block : Blocks())
    {
        assert(block->bbNum < blkMapSize);
        blkMap[block->bbNum] = blockOrdinal++;
    }

    static const char* kindImage[]{"EHFINALLYRET", "EHFILTERRET", "EHCATCHRET",  "THROW", "RETURN", "NONE",
                                   "ALWAYS",       "LEAVE",       "CALLFINALLY", "COND",  "SWITCH"};

    BasicBlock* block;
    for (block = fgFirstBB, blockOrdinal = 1; block != nullptr; block = block->bbNext, blockOrdinal++)
    {
        if (createDotFile)
        {
            fprintf(fgxFile, "    " FMT_BB " [label = \"", block->bbNum);

            if (useBlockId)
            {
                fprintf(fgxFile, "%s", block->dspToString());
            }
            else
            {
                fprintf(fgxFile, FMT_BB, block->bbNum);
            }

            if (block->bbJumpKind == BBJ_COND)
            {
                fprintf(fgxFile, "\\n");

                // Include a line with the basics of the branch condition, if possible.
                // Find the loop termination test at the bottom of the loop.
                if (Statement* condStmt = block->lastStmt())
                {
                    GenTree* const condTree = condStmt->GetRootNode();
                    assert(condTree->OperIs(GT_JTRUE));
                    DumpDotTree(fgxFile, condTree->AsUnOp()->GetOp(0));
                }
            }

            // "Raw" Profile weight
            if (block->hasProfileWeight())
            {
                fprintf(fgxFile, "\\n\\n%7.2f", block->getBBWeight(this) / BB_UNITY_WEIGHT);
            }

            // end of block label
            fprintf(fgxFile, "\"");

            // other node attributes
            //
            if (block == fgFirstBB)
            {
                fprintf(fgxFile, ", shape = \"house\"");
            }
            else if (block->bbJumpKind == BBJ_RETURN)
            {
                fprintf(fgxFile, ", shape = \"invhouse\"");
            }
            else if (block->bbJumpKind == BBJ_THROW)
            {
                fprintf(fgxFile, ", shape = \"trapezium\"");
            }
            else if (block->bbFlags & BBF_INTERNAL)
            {
                fprintf(fgxFile, ", shape = \"note\"");
            }

            fprintf(fgxFile, "];\n");
        }
        else
        {
            fprintf(fgxFile, "\n        <block");
            fprintf(fgxFile, "\n            id=\"%d\"", block->bbNum);
            fprintf(fgxFile, "\n            ordinal=\"%d\"", blockOrdinal);
            fprintf(fgxFile, "\n            jumpKind=\"%s\"", kindImage[block->bbJumpKind]);
            if (block->hasTryIndex())
            {
                fprintf(fgxFile, "\n            inTry=\"%s\"", "true");
            }
            if (block->hasHndIndex())
            {
                fprintf(fgxFile, "\n            inHandler=\"%s\"", "true");
            }
            if ((fgFirstBB->hasProfileWeight()) && ((block->bbFlags & BBF_COLD) == 0))
            {
                fprintf(fgxFile, "\n            hot=\"true\"");
            }
            if (block->bbFlags & (BBF_HAS_NEWOBJ | BBF_HAS_NEWARRAY))
            {
                fprintf(fgxFile, "\n            callsNew=\"true\"");
            }
            if (block->bbFlags & BBF_LOOP_HEAD)
            {
                fprintf(fgxFile, "\n            loopHead=\"true\"");
            }

            const char* rootTreeOpName = "n/a";
            if (block->IsLIR() || (block->lastStmt() != nullptr))
            {
                if (GenTree* lastNode = block->lastNode())
                {
                    rootTreeOpName = GenTree::OpName(lastNode->GetOper());
                }
            }

            fprintf(fgxFile, "\n            weight=");
            DumpDotDouble(fgxFile, ((double)block->bbWeight) / weightDivisor);
            fprintf(fgxFile, "\n            startOffset=\"%d\"", block->bbCodeOffs);
            fprintf(fgxFile, "\n            rootTreeOp=\"%s\"", rootTreeOpName);
            fprintf(fgxFile, "\n            endOffset=\"%d\"", block->bbCodeOffsEnd);
            fprintf(fgxFile, ">");
            fprintf(fgxFile, "\n        </block>");
        }
    }

    if (!createDotFile)
    {
        fprintf(fgxFile, "\n    </blocks>");

        fprintf(fgxFile, "\n    <edges");
        fprintf(fgxFile, "\n        edgeCount=\"%d\"", fgEdgeCount);
        fprintf(fgxFile, ">");
    }

    if (fgComputePredsDone)
    {
        unsigned    edgeNum = 1;
        BasicBlock* bTarget;
        for (bTarget = fgFirstBB; bTarget != nullptr; bTarget = bTarget->bbNext)
        {
            double targetWeightDivisor;
            if (bTarget->bbWeight == BB_ZERO_WEIGHT)
            {
                targetWeightDivisor = 1.0;
            }
            else
            {
                targetWeightDivisor = (double)bTarget->bbWeight;
            }

            for (flowList* const edge : bTarget->PredEdges())
            {
                BasicBlock* bSource = edge->getBlock();
                double      sourceWeightDivisor;
                if (bSource->bbWeight == BB_ZERO_WEIGHT)
                {
                    sourceWeightDivisor = 1.0;
                }
                else
                {
                    sourceWeightDivisor = (double)bSource->bbWeight;
                }
                if (createDotFile)
                {
                    fprintf(fgxFile, "    " FMT_BB " -> " FMT_BB, bSource->bbNum, bTarget->bbNum);

                    const char* sep = "";

                    if (blkMap[bSource->bbNum] > blkMap[bTarget->bbNum])
                    {
                        // Lexical backedge
                        fprintf(fgxFile, " [color=green");
                        sep = ", ";
                    }
                    else if ((blkMap[bSource->bbNum] + 1) == blkMap[bTarget->bbNum])
                    {
                        // Lexical successor
                        fprintf(fgxFile, " [color=blue, weight=20");
                        sep = ", ";
                    }
                    else
                    {
                        fprintf(fgxFile, " [");
                    }

                    if (validWeights)
                    {
                        BasicBlock::weight_t edgeWeight = (edge->edgeWeightMin() + edge->edgeWeightMax()) / 2;
                        fprintf(fgxFile, "%slabel=\"%7.2f\"", sep, (double)edgeWeight / weightDivisor);
                    }

                    fprintf(fgxFile, "];\n");
                }
                else
                {
                    fprintf(fgxFile, "\n        <edge");
                    fprintf(fgxFile, "\n            id=\"%d\"", edgeNum);
                    fprintf(fgxFile, "\n            source=\"%d\"", bSource->bbNum);
                    fprintf(fgxFile, "\n            target=\"%d\"", bTarget->bbNum);
                    if (bSource->bbJumpKind == BBJ_SWITCH)
                    {
                        if (edge->flDupCount >= 2)
                        {
                            fprintf(fgxFile, "\n            switchCases=\"%d\"", edge->flDupCount);
                        }
                        if (bSource->bbJumpSwt->getDefault() == bTarget)
                        {
                            fprintf(fgxFile, "\n            switchDefault=\"true\"");
                        }
                    }
                    if (validWeights)
                    {
                        BasicBlock::weight_t edgeWeight = (edge->edgeWeightMin() + edge->edgeWeightMax()) / 2;
                        fprintf(fgxFile, "\n            weight=");
                        DumpDotDouble(fgxFile, ((double)edgeWeight) / weightDivisor);

                        if (edge->edgeWeightMin() != edge->edgeWeightMax())
                        {
                            fprintf(fgxFile, "\n            minWeight=");
                            DumpDotDouble(fgxFile, ((double)edge->edgeWeightMin()) / weightDivisor);
                            fprintf(fgxFile, "\n            maxWeight=");
                            DumpDotDouble(fgxFile, ((double)edge->edgeWeightMax()) / weightDivisor);
                        }

                        if (edgeWeight > 0)
                        {
                            if (edgeWeight < bSource->bbWeight)
                            {
                                fprintf(fgxFile, "\n            out=");
                                DumpDotDouble(fgxFile, ((double)edgeWeight) / sourceWeightDivisor);
                            }
                            if (edgeWeight < bTarget->bbWeight)
                            {
                                fprintf(fgxFile, "\n            in=");
                                DumpDotDouble(fgxFile, ((double)edgeWeight) / targetWeightDivisor);
                            }
                        }
                    }
                }
                if (!createDotFile)
                {
                    fprintf(fgxFile, ">");
                    fprintf(fgxFile, "\n        </edge>");
                }

                ++edgeNum;
            }
        }
    }

    // For dot, show edges w/o pred lists, and add invisible bbNext links.
    // Also, add EH and/or loop regions as "cluster" subgraphs, if requested.
    //
    if (createDotFile)
    {
        for (BasicBlock* const bSource : Blocks())
        {
            if (constrained)
            {
                // Invisible edge for bbNext chain
                //
                if (bSource->bbNext != nullptr)
                {
                    fprintf(fgxFile, "    " FMT_BB " -> " FMT_BB " [style=\"invis\", weight=25];\n", bSource->bbNum,
                            bSource->bbNext->bbNum);
                }
            }

            if (fgComputePredsDone)
            {
                // Already emitted pred edges above.
                //
                continue;
            }

            // Emit successor edges
            //
            for (BasicBlock* const bTarget : bSource->Succs())
            {
                fprintf(fgxFile, "    " FMT_BB " -> " FMT_BB, bSource->bbNum, bTarget->bbNum);
                if (blkMap[bSource->bbNum] > blkMap[bTarget->bbNum])
                {
                    // Lexical backedge
                    fprintf(fgxFile, " [color=green]\n");
                }
                else if ((blkMap[bSource->bbNum] + 1) == blkMap[bTarget->bbNum])
                {
                    // Lexical successor
                    fprintf(fgxFile, " [color=blue]\n");
                }
                else
                {
                    fprintf(fgxFile, ";\n");
                }
            }
        }

        if ((includeEH && (compHndBBtabCount > 0)) || (includeLoops && (optLoopCount > 0)))
        {
            // Generate something like:
            //    subgraph cluster_0 {
            //      label = "xxx";
            //      color = yyy;
            //      bb; bb;
            //      subgraph {
            //        label = "aaa";
            //        color = bbb;
            //        bb; bb...
            //      }
            //      ...
            //    }
            //
            // Thus, the subgraphs need to be nested to show the region nesting.
            //
            // The EH table is in order, top-to-bottom, most nested to least nested where
            // there is a parent/child relationship. The loop table the opposite: it is
            // in order from the least nested to most nested.
            //
            // Build a region tree, collecting all the regions we want to display,
            // and then walk it to emit the regions.

            // RegionGraph: represent non-overlapping, possibly nested, block ranges in the flow graph.
            class RegionGraph
            {
            public:
                enum class RegionType
                {
                    Root,
                    EH,
                    Loop
                };

            private:
                struct Region
                {
                    Region(RegionType rgnType, const char* rgnName, BasicBlock* bbStart, BasicBlock* bbEnd)
                        : m_rgnNext(nullptr)
                        , m_rgnChild(nullptr)
                        , m_rgnType(rgnType)
                        , m_bbStart(bbStart)
                        , m_bbEnd(bbEnd)
                    {
                        strcpy_s(m_rgnName, sizeof(m_rgnName), rgnName);
                    }

                    Region*     m_rgnNext;
                    Region*     m_rgnChild;
                    RegionType  m_rgnType;
                    char        m_rgnName[30];
                    BasicBlock* m_bbStart;
                    BasicBlock* m_bbEnd;
                };

            public:
                RegionGraph(Compiler* comp, unsigned* blkMap, unsigned blkMapSize)
                    : m_comp(comp), m_rgnRoot(nullptr), m_blkMap(blkMap), m_blkMapSize(blkMapSize)
                {
                    // Create a root region that encompasses the whole function.
                    m_rgnRoot =
                        new (m_comp, CMK_DebugOnly) Region(RegionType::Root, "Root", comp->fgFirstBB, comp->fgLastBB);
                }

                //------------------------------------------------------------------------
                // Insert: Insert a region [start..end] (inclusive) into the graph.
                //
                // Arguments:
                //    name    - the textual label to use for the region
                //    rgnType - the region type
                //    start   - start block of the region
                //    end     - last block of the region
                //
                void Insert(const char* name, RegionType rgnType, BasicBlock* start, BasicBlock* end)
                {
                    JITDUMP("Insert region: %s, type: %s, start: " FMT_BB ", end: " FMT_BB "\n", name,
                            GetRegionType(rgnType), start->bbNum, end->bbNum);

                    assert(start != nullptr);
                    assert(end != nullptr);

                    Region*  newRgn          = new (m_comp, CMK_DebugOnly) Region(rgnType, name, start, end);
                    unsigned newStartOrdinal = m_blkMap[start->bbNum];
                    unsigned newEndOrdinal   = m_blkMap[end->bbNum];

                    Region*  curRgn          = m_rgnRoot;
                    unsigned curStartOrdinal = m_blkMap[curRgn->m_bbStart->bbNum];
                    unsigned curEndOrdinal   = m_blkMap[curRgn->m_bbEnd->bbNum];

                    // A range can be a single block, but there can be no overlap between ranges.
                    assert(newStartOrdinal <= newEndOrdinal);
                    assert(curStartOrdinal <= curEndOrdinal);
                    assert(newStartOrdinal >= curStartOrdinal);
                    assert(newEndOrdinal <= curEndOrdinal);

                    // We know the new region will be part of the current region. Should it be a direct
                    // child, or put within one of the existing children?
                    Region** lastChildPtr = &curRgn->m_rgnChild;
                    Region*  child        = curRgn->m_rgnChild;
                    while (child != nullptr)
                    {
                        unsigned childStartOrdinal = m_blkMap[child->m_bbStart->bbNum];
                        unsigned childEndOrdinal   = m_blkMap[child->m_bbEnd->bbNum];

                        // Consider the following cases, where each "x" is a block in the range:
                        //    xxxxxxx      // current 'child' range; we're comparing against this
                        //    xxxxxxx      // (1) same range; could be considered child or parent
                        //  xxxxxxxxx      // (2) parent range, shares last block
                        //    xxxxxxxxx    // (3) parent range, shares first block
                        //  xxxxxxxxxxx    // (4) fully overlapping parent range
                        // xx              // (5) non-overlapping preceding sibling range
                        //            xx   // (6) non-overlapping following sibling range
                        //      xxx        // (7) child range
                        //    xxx          // (8) child range, shares same start block
                        //    x            // (9) single-block child range, shares same start block
                        //        xxx      // (10) child range, shares same end block
                        //          x      // (11) single-block child range, shares same end block
                        //  xxxxxxx        // illegal: overlapping ranges
                        //  xxx            // illegal: overlapping ranges (shared child start block and new end block)
                        //      xxxxxxx    // illegal: overlapping ranges
                        //          xxx    // illegal: overlapping ranges (shared child end block and new start block)

                        // Assert the child is properly nested within the parent.
                        // Note that if regions have the same start and end, you can't tell which is nested within the
                        // other, though it shouldn't matter.
                        assert(childStartOrdinal <= childEndOrdinal);
                        assert(curStartOrdinal <= childStartOrdinal);
                        assert(childEndOrdinal <= curEndOrdinal);

                        // Should the new region be before this child?
                        // Case (5).
                        if (newEndOrdinal < childStartOrdinal)
                        {
                            // Insert before this child.
                            newRgn->m_rgnNext = child;
                            *lastChildPtr     = newRgn;
                            break;
                        }
                        else if ((newStartOrdinal >= childStartOrdinal) && (newEndOrdinal <= childEndOrdinal))
                        {
                            // Insert as a child of this child.
                            // Need to recurse to walk the child's children list to see where it belongs.
                            // Case (1), (7), (8), (9), (10), (11).

                            curStartOrdinal = m_blkMap[child->m_bbStart->bbNum];
                            curEndOrdinal   = m_blkMap[child->m_bbEnd->bbNum];

                            lastChildPtr = &child->m_rgnChild;
                            child        = child->m_rgnChild;

                            continue;
                        }
                        else if (newStartOrdinal <= childStartOrdinal)
                        {
                            // The new region is a parent of one or more of the existing children.
                            // Case (2), (3), (4).

                            // Find all the children it encompasses.
                            Region** lastEndChildPtr = &child->m_rgnNext;
                            Region*  endChild        = child->m_rgnNext;
                            while (endChild != nullptr)
                            {
                                unsigned endChildStartOrdinal = m_blkMap[endChild->m_bbStart->bbNum];
                                unsigned endChildEndOrdinal   = m_blkMap[endChild->m_bbEnd->bbNum];
                                assert(endChildStartOrdinal <= endChildEndOrdinal);

                                if (newEndOrdinal < endChildStartOrdinal)
                                {
                                    // Found the range
                                    break;
                                }

                                lastEndChildPtr = &endChild->m_rgnNext;
                                endChild        = endChild->m_rgnNext;
                            }

                            // The range is [child..endChild previous]. If endChild is nullptr, then
                            // the range is to the end of the parent. Move these all to be
                            // children of newRgn, and put newRgn in where `child` is.
                            newRgn->m_rgnNext = endChild;
                            *lastChildPtr     = newRgn;

                            newRgn->m_rgnChild = child;
                            *lastEndChildPtr   = nullptr;

                            break;
                        }

                        // Else, look for next child.
                        // Case (6).

                        lastChildPtr = &child->m_rgnNext;
                        child        = child->m_rgnNext;
                    }

                    if (child == nullptr)
                    {
                        // Insert as the last child (could be the only child).
                        *lastChildPtr = newRgn;
                    }
                }

#ifdef DEBUG

                const unsigned dumpIndentIncrement = 2; // How much to indent each nested level.

                //------------------------------------------------------------------------
                // GetRegionType: get a textual name for the region type, to be used in dumps.
                //
                // Arguments:
                //    rgnType - the region type
                //
                static const char* GetRegionType(RegionType rgnType)
                {
                    switch (rgnType)
                    {
                        case RegionType::Root:
                            return "Root";
                        case RegionType::EH:
                            return "EH";
                        case RegionType::Loop:
                            return "Loop";
                        default:
                            return "UNKNOWN";
                    }
                }

                //------------------------------------------------------------------------
                // DumpRegionNode: Region graph dump helper to dump a region node at the given indent,
                // and recursive dump its children.
                //
                // Arguments:
                //    rgn    - the region to dump
                //    indent - number of leading characters to indent all output
                //
                void DumpRegionNode(Region* rgn, unsigned indent) const
                {
                    printf("%*s======\n", indent, "");
                    printf("%*sType: %s\n", indent, "", GetRegionType(rgn->m_rgnType));
                    printf("%*sName: %s\n", indent, "", rgn->m_rgnName);
                    printf("%*sRange: " FMT_BB ".." FMT_BB "\n", indent, "", rgn->m_bbStart->bbNum,
                           rgn->m_bbEnd->bbNum);

                    for (Region* child = rgn->m_rgnChild; child != nullptr; child = child->m_rgnNext)
                    {
                        DumpRegionNode(child, indent + dumpIndentIncrement);
                    }
                }

                //------------------------------------------------------------------------
                // Dump: dump the entire region graph
                //
                void Dump()
                {
                    printf("Region graph:\n");
                    DumpRegionNode(m_rgnRoot, 0);
                    printf("\n");
                }

                //------------------------------------------------------------------------
                // VerifyNode: verify the region graph rooted at `rgn`.
                //
                // Arguments:
                //    rgn  - the node (and its children) to check.
                //
                void Verify(Region* rgn)
                {
                    // The region needs to be a non-overlapping parent to all its children.
                    // The children need to be non-overlapping, and in increasing order.

                    unsigned rgnStartOrdinal = m_blkMap[rgn->m_bbStart->bbNum];
                    unsigned rgnEndOrdinal   = m_blkMap[rgn->m_bbEnd->bbNum];
                    assert(rgnStartOrdinal <= rgnEndOrdinal);

                    Region* child     = rgn->m_rgnChild;
                    Region* lastChild = nullptr;
                    if (child != nullptr)
                    {
                        unsigned childStartOrdinal = m_blkMap[child->m_bbStart->bbNum];
                        unsigned childEndOrdinal   = m_blkMap[child->m_bbEnd->bbNum];
                        assert(childStartOrdinal <= childEndOrdinal);
                        assert(rgnStartOrdinal <= childStartOrdinal);

                        while (true)
                        {
                            Verify(child);

                            lastChild                      = child;
                            unsigned lastChildStartOrdinal = childStartOrdinal;
                            unsigned lastChildEndOrdinal   = childEndOrdinal;

                            child = child->m_rgnNext;
                            if (child == nullptr)
                            {
                                break;
                            }

                            childStartOrdinal = m_blkMap[child->m_bbStart->bbNum];
                            childEndOrdinal   = m_blkMap[child->m_bbEnd->bbNum];
                            assert(childStartOrdinal <= childEndOrdinal);

                            // The children can't overlap; they can't share any blocks.
                            assert(lastChildEndOrdinal < childStartOrdinal);
                        }

                        // The parent region must fully include the last child.
                        assert(childEndOrdinal <= rgnEndOrdinal);
                    }
                }

                //------------------------------------------------------------------------
                // Verify: verify the region graph satisfies proper nesting, and other legality rules.
                //
                void Verify()
                {
                    assert(m_comp != nullptr);
                    assert(m_blkMap != nullptr);
                    for (unsigned i = 0; i < m_blkMapSize; i++)
                    {
                        assert(m_blkMap[i] < m_blkMapSize);
                    }

                    // The root region has no siblings.
                    assert(m_rgnRoot != nullptr);
                    assert(m_rgnRoot->m_rgnNext == nullptr);
                    Verify(m_rgnRoot);
                }

#endif // DEBUG

                //------------------------------------------------------------------------
                // Output: output the region graph to the .dot file
                //
                // Arguments:
                //    file - the file to write output to.
                //
                void Output(FILE* file)
                {
                    unsigned clusterNum = 0;

                    // Output the regions; don't output the top (root) region that represents the whole function.
                    for (Region* child = m_rgnRoot->m_rgnChild; child != nullptr; child = child->m_rgnNext)
                    {
                        OutputRegion(file, clusterNum, child, 4);
                    }
                    fprintf(file, "\n");
                }

            private:
                //------------------------------------------------------------------------
                // GetColorForRegion: get a color name to use for a region
                //
                // Arguments:
                //    rgn - the region for which we need a color
                //
                static const char* GetColorForRegion(Region* rgn)
                {
                    RegionType rgnType = rgn->m_rgnType;
                    switch (rgnType)
                    {
                        case RegionType::EH:
                            return "red";
                        case RegionType::Loop:
                            return "blue";
                        default:
                            return "black";
                    }
                }

                //------------------------------------------------------------------------
                // OutputRegion: helper function to output a region and its nested children
                // to the .dot file.
                //
                // Arguments:
                //    file       - the file to write output to.
                //    clusterNum - the number of this dot "cluster". This is updated as we
                //                 create new clusters.
                //    rgn        - the region to output.
                //    indent     - the current indent level, in characters.
                //
                void OutputRegion(FILE* file, unsigned& clusterNum, Region* rgn, unsigned indent)
                {
                    fprintf(file, "%*ssubgraph cluster_%u {\n", indent, "", clusterNum);
                    indent += 4;
                    fprintf(file, "%*slabel = \"%s\";\n", indent, "", rgn->m_rgnName);
                    fprintf(file, "%*scolor = %s;\n", indent, "", GetColorForRegion(rgn));
                    clusterNum++;

                    bool        needIndent = true;
                    BasicBlock* bbCur      = rgn->m_bbStart;
                    BasicBlock* bbEnd      = rgn->m_bbEnd->bbNext;
                    Region*     child      = rgn->m_rgnChild;
                    BasicBlock* childCurBB = (child == nullptr) ? nullptr : child->m_bbStart;

                    // Count the children and assert we output all of them.
                    unsigned totalChildren = 0;
                    unsigned childCount    = 0;
                    for (Region* tmpChild = child; tmpChild != nullptr; tmpChild = tmpChild->m_rgnNext)
                    {
                        totalChildren++;
                    }

                    while (bbCur != bbEnd)
                    {
                        // Output from bbCur to current child first block.
                        while ((bbCur != childCurBB) && (bbCur != bbEnd))
                        {
                            fprintf(file, "%*s" FMT_BB ";", needIndent ? indent : 0, "", bbCur->bbNum);
                            needIndent = false;
                            bbCur      = bbCur->bbNext;
                        }

                        if (bbCur == bbEnd)
                        {
                            // We're done at this level.
                            break;
                        }
                        else
                        {
                            assert(bbCur != nullptr); // Or else we should also have `bbCur == bbEnd`
                            assert(child != nullptr);

                            // If there is a child, output that child.
                            if (!needIndent)
                            {
                                // We've printed some basic blocks, so put the subgraph on a new line.
                                fprintf(file, "\n");
                            }
                            OutputRegion(file, clusterNum, child, indent);
                            needIndent = true;

                            childCount++;

                            bbCur      = child->m_bbEnd->bbNext; // Next, output blocks after this child.
                            child      = child->m_rgnNext;       // Move to the next child, if any.
                            childCurBB = (child == nullptr) ? nullptr : child->m_bbStart;
                        }
                    }

                    // Put the end brace on its own line and leave the cursor at the beginning of the line for the
                    // parent.
                    indent -= 4;
                    fprintf(file, "\n%*s}\n", indent, "");

                    assert(childCount == totalChildren);
                }

                Compiler* m_comp;
                Region*   m_rgnRoot;
                unsigned* m_blkMap;
                unsigned  m_blkMapSize;
            };

            // Define the region graph object. We'll add regions to this, then output the graph.

            RegionGraph rgnGraph(this, blkMap, blkMapSize);

            // Add the EH regions to the region graph. An EH region consists of a region for the
            // `try`, a region for the handler, and, for filter/filter-handlers, a region for the
            // `filter` as well.

            if (includeEH)
            {
                char      name[30];
                unsigned  XTnum;
                EHblkDsc* ehDsc;
                for (XTnum = 0, ehDsc = compHndBBtab; XTnum < compHndBBtabCount; XTnum++, ehDsc++)
                {
                    sprintf_s(name, sizeof(name), "EH#%u try", XTnum);
                    rgnGraph.Insert(name, RegionGraph::RegionType::EH, ehDsc->ebdTryBeg, ehDsc->ebdTryLast);
                    const char* handlerType = "";
                    switch (ehDsc->ebdHandlerType)
                    {
                        case EH_HANDLER_CATCH:
                            handlerType = "catch";
                            break;
                        case EH_HANDLER_FILTER:
                            handlerType = "filter-hnd";
                            break;
                        case EH_HANDLER_FAULT:
                            handlerType = "fault";
                            break;
                        case EH_HANDLER_FINALLY:
                            handlerType = "finally";
                            break;
                        case EH_HANDLER_FAULT_WAS_FINALLY:
                            handlerType = "fault-was-finally";
                            break;
                    }
                    sprintf_s(name, sizeof(name), "EH#%u %s", XTnum, handlerType);
                    rgnGraph.Insert(name, RegionGraph::RegionType::EH, ehDsc->ebdHndBeg, ehDsc->ebdHndLast);
                    if (ehDsc->HasFilter())
                    {
                        sprintf_s(name, sizeof(name), "EH#%u filter", XTnum);
                        rgnGraph.Insert(name, RegionGraph::RegionType::EH, ehDsc->ebdFilter, ehDsc->ebdHndBeg->bbPrev);
                    }
                }
            }

            // Add regions for the loops. Note that loops are assumed to be contiguous from `lpFirst` to `lpBottom`.

            if (includeLoops)
            {
                char name[30];
                for (unsigned loopNum = 0; loopNum < optLoopCount; loopNum++)
                {
                    const LoopDsc& loop = optLoopTable[loopNum];
                    if (loop.lpFlags & LPFLG_REMOVED)
                    {
                        continue;
                    }
                    sprintf_s(name, sizeof(name), FMT_LP, loopNum);
                    rgnGraph.Insert(name, RegionGraph::RegionType::Loop, loop.lpFirst, loop.lpBottom);
                }
            }

            // All the regions have been added. Now, output them.
            DBEXEC(verbose, rgnGraph.Dump());
            INDEBUG(rgnGraph.Verify());
            rgnGraph.Output(fgxFile);
        }
    }

    if (createDotFile)
    {
        fprintf(fgxFile, "}\n");
    }
    else
    {
        fprintf(fgxFile, "\n    </edges>");
        fprintf(fgxFile, "\n</method>\n");
    }

    if (dontClose)
    {
        fprintf(fgxFile, "\n");
    }
    else
    {
        fclose(fgxFile);
    }

    return result;
}

#endif // DUMP_FLOWGRAPHS
