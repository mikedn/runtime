// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "jitstd/algorithm.h"

#if MEASURE_BLOCK_SIZE
size_t BasicBlock::s_Size;
size_t BasicBlock::s_Count;
#endif

#ifdef DEBUG
static flowList* ShuffleHelper(unsigned hash, flowList* res)
{
    flowList* head = res;
    for (flowList *prev = nullptr; res != nullptr; prev = res, res = res->flNext)
    {
        unsigned blkHash = (hash ^ (res->getBlock()->bbNum << 16) ^ res->getBlock()->bbNum);
        if (((blkHash % 1879) & 1) && prev != nullptr)
        {
            // Swap res with head.
            prev->flNext = head;
            std::swap(head->flNext, res->flNext);
            std::swap(head, res);
        }
    }
    return head;
}

unsigned SsaStressHashHelper()
{
    // hash = 0: turned off, hash = 1: use method hash, hash = *: use custom hash.
    unsigned hash = JitConfig.JitSsaStress();

    if (hash == 0)
    {
        return hash;
    }

    if (hash == 1)
    {
        return JitTls::GetCompiler()->info.compMethodHash();
    }

    return ((hash >> 16) == 0) ? ((hash << 16) | hash) : hash;
}
#endif

EHSuccessorIterPosition::EHSuccessorIterPosition(Compiler* comp, BasicBlock* block)
    : m_remainingRegSuccs(block->NumSucc(comp)), m_curRegSucc(nullptr), m_curTry(comp->ehGetBlockExnFlowDsc(block))
{
    // If "block" is a "leave helper" block (the empty BBJ_ALWAYS block that pairs with a
    // preceding BBJ_CALLFINALLY block to implement a "leave" IL instruction), then no exceptions
    // can occur within it, so clear m_curTry if it's non-null.
    if (m_curTry != nullptr)
    {
        if (block->isBBCallAlwaysPairTail())
        {
            m_curTry = nullptr;
        }
    }

    if (m_curTry == nullptr && m_remainingRegSuccs > 0)
    {
        // Examine the successors to see if any are the start of try blocks.
        FindNextRegSuccTry(comp, block);
    }
}

void EHSuccessorIterPosition::FindNextRegSuccTry(Compiler* comp, BasicBlock* block)
{
    assert(m_curTry == nullptr);

    // Must now consider the next regular successor, if any.
    while (m_remainingRegSuccs > 0)
    {
        m_remainingRegSuccs--;
        m_curRegSucc = block->GetSucc(m_remainingRegSuccs, comp);
        if (comp->bbIsTryBeg(m_curRegSucc))
        {
            assert(m_curRegSucc->hasTryIndex()); // Since it is a try begin.
            unsigned newTryIndex = m_curRegSucc->getTryIndex();

            // If the try region started by "m_curRegSucc" (represented by newTryIndex) contains m_block,
            // we've already yielded its handler, as one of the EH handler successors of m_block itself.
            if (comp->bbInExnFlowRegions(newTryIndex, block))
            {
                continue;
            }

            // Otherwise, consider this try.
            m_curTry = comp->ehGetDsc(newTryIndex);
            break;
        }
    }
}

void EHSuccessorIterPosition::Advance(Compiler* comp, BasicBlock* block)
{
    assert(m_curTry != nullptr);
    if (m_curTry->ebdEnclosingTryIndex != EHblkDsc::NO_ENCLOSING_INDEX)
    {
        m_curTry = comp->ehGetDsc(m_curTry->ebdEnclosingTryIndex);

        // If we've gone over into considering try's containing successors,
        // then the enclosing try must have the successor as its first block.
        if (m_curRegSucc == nullptr || m_curTry->ebdTryBeg == m_curRegSucc)
        {
            return;
        }

        // Otherwise, give up, try the next regular successor.
        m_curTry = nullptr;
    }
    else
    {
        m_curTry = nullptr;
    }

    // We've exhausted all try blocks.
    // See if there are any remaining regular successors that start try blocks.
    FindNextRegSuccTry(comp, block);
}

BasicBlock* EHSuccessorIterPosition::Current(Compiler* comp, BasicBlock* block)
{
    assert(m_curTry != nullptr);
    return m_curTry->ExFlowBlock();
}

static flowList emptyBlockPredsWithEH(nullptr, nullptr);

flowList* Compiler::BlockPredsWithEH(BasicBlock* blk)
{
    flowList* res = blk->bbPreds;

    if (!blk->hasHndIndex())
    {
        return res;
    }

    unsigned  tryIndex = blk->getHndIndex();
    EHblkDsc* ehblk    = ehGetDsc(tryIndex);

    if (blk != ehblk->ExFlowBlock())
    {
        return res;
    }

    if (blk->bbPredsWithEH != nullptr)
    {
        return blk->bbPredsWithEH == &emptyBlockPredsWithEH ? nullptr : blk->bbPredsWithEH;
    }

    // Find the first block of the try.
    BasicBlock* tryStart = ehblk->ebdTryBeg;
    for (BasicBlock* const tryStartPredBlock : tryStart->PredBlocks())
    {
        res = new (this, CMK_FlowList) flowList(tryStartPredBlock, res);

#if MEASURE_BLOCK_SIZE
        genFlowNodeCnt += 1;
        genFlowNodeSize += sizeof(flowList);
#endif
    }

    // Now add all blocks handled by this handler (except for second blocks of BBJ_CALLFINALLY/BBJ_ALWAYS pairs;
    // these cannot cause transfer to the handler...)
    // TODO-Throughput: It would be nice if we could iterate just over the blocks in the try, via
    // something like:
    //   for (BasicBlock* bb = ehblk->ebdTryBeg; bb != ehblk->ebdTryLast->bbNext; bb = bb->bbNext)
    //     (plus adding in any filter blocks outside the try whose exceptions are handled here).
    // That doesn't work, however: funclets have caused us to sometimes split the body of a try into
    // more than one sequence of contiguous blocks.  We need to find a better way to do this.
    for (BasicBlock* const bb : Blocks())
    {
        if (bbInExnFlowRegions(tryIndex, bb) && !bb->isBBCallAlwaysPairTail())
        {
            res = new (this, CMK_FlowList) flowList(bb, res);

#if MEASURE_BLOCK_SIZE
            genFlowNodeCnt += 1;
            genFlowNodeSize += sizeof(flowList);
#endif
        }
    }

#ifdef DEBUG
    if (unsigned hash = SsaStressHashHelper())
    {
        res = ShuffleHelper(hash, res);
    }
#endif

    blk->bbPredsWithEH = res == nullptr ? &emptyBlockPredsWithEH : res;

    return res;
}

// Returns false if pred list is not in increasing bbNum order.
bool BasicBlock::checkPredListOrder() const
{
    unsigned lastBBNum = 0;
    for (BasicBlock* const predBlock : PredBlocks())
    {
        const unsigned bbNum = predBlock->bbNum;
        if (bbNum <= lastBBNum)
        {
            assert(bbNum != lastBBNum);
            return false;
        }
        lastBBNum = bbNum;
    }
    return true;
}

// Ensure all pred list entries appear in increasing
void BasicBlock::ensurePredListOrder(Compiler* compiler)
{
    // First, check if list is already in order.
    //
    if (checkPredListOrder())
    {
        return;
    }

    reorderPredList(compiler);
    assert(checkPredListOrder());
}

// Relink pred list in increasing bbNum order.
void BasicBlock::reorderPredList(Compiler* compiler)
{
    // Count number or entries.
    //
    int count = 0;
    for (flowList* const pred : PredEdges())
    {
        count++;
    }

    // If only 0 or 1 entry, nothing to reorder.
    //
    if (count < 2)
    {
        return;
    }

    // Allocate sort vector if needed.
    //
    if (compiler->fgPredListSortVector == nullptr)
    {
        CompAllocator allocator        = compiler->getAllocator(CMK_FlowList);
        compiler->fgPredListSortVector = new (allocator) jitstd::vector<flowList*>(allocator);
    }

    jitstd::vector<flowList*>* const sortVector = compiler->fgPredListSortVector;
    sortVector->clear();

    // Fill in the vector from the list.
    //
    for (flowList* const pred : PredEdges())
    {
        sortVector->push_back(pred);
    }

    // Sort by increasing bbNum
    //
    struct flowListBBNumCmp
    {
        bool operator()(const flowList* f1, const flowList* f2)
        {
            return f1->getBlock()->bbNum < f2->getBlock()->bbNum;
        }
    };

    jitstd::sort(sortVector->begin(), sortVector->end(), flowListBBNumCmp());

    // Rethread the list.
    //
    flowList* last = nullptr;

    for (flowList* current : *sortVector)
    {
        if (last == nullptr)
        {
            bbPreds = current;
        }
        else
        {
            last->flNext = current;
        }

        last = current;
    }

    last->flNext = nullptr;

    // Note this lastPred is only used transiently.
    bbLastPred = last;
}

//------------------------------------------------------------------------
// CloneBlockState: Try to populate `to` block with a copy of `from` block's statements, replacing
//                  uses of local `varNum` with IntCns `varVal`.
//
// Arguments:
//    compiler - Jit compiler instance
//    to - New/empty block to copy statements into
//    from - Block to copy statements from
//    varNum - lclVar uses with lclNum `varNum` will be replaced; can be ~0 to indicate no replacement.
//    varVal - If replacing uses of `varNum`, replace them with int constants with value `varVal`.
//
// Return Value:
//    Cloning may fail because this routine uses `gtCloneExpr` for cloning and it can't handle all
//    IR nodes.  If cloning of any statement fails, `false` will be returned and block `to` may be
//    partially populated.  If cloning of all statements succeeds, `true` will be returned and
//    block `to` will be fully populated.

bool BasicBlock::CloneBlockState(
    Compiler* compiler, BasicBlock* to, const BasicBlock* from, const LclVarDsc* constLcl, const int constVal)
{
    assert(to->bbStmtList == nullptr);

    to->bbFlags  = from->bbFlags;
    to->bbWeight = from->bbWeight;
    to->copyEHRegion(from);
    to->bbCatchTyp    = from->bbCatchTyp;
    to->bbRefs        = from->bbRefs;
    to->bbCodeOffs    = from->bbCodeOffs;
    to->bbCodeOffsEnd = from->bbCodeOffsEnd;
    to->bbNatLoopNum  = from->bbNatLoopNum;

    if (from->bbReach != BlockSetOps::UninitVal())
    {
        // TODO-MIKE-Review: It may be possible to make a shallow copy of bbReach.
        // It is only modified in fgComputeReachabilitySets, which creates new ones
        // every time it is run, so any sharing of bitsets between block would not
        // affect it. But there are a few assignments to bbReach that would need to
        // be reviewed. In general, bbReach handling is a mess.
        to->bbReach = BlockSetOps::MakeCopy(compiler, from->bbReach);
    }
    else
    {
        assert(to->bbReach == BlockSetOps::UninitVal());
    }

    for (Statement* const fromStmt : from->Statements())
    {
        auto newExpr = compiler->gtCloneExpr(fromStmt->GetRootNode(), GTF_NONE, constLcl, constVal);
        if (!newExpr)
        {
            // gtCloneExpr doesn't handle all opcodes, so may fail to clone a statement.
            // When that happens, it returns nullptr; abandon the rest of this block and
            // return `false` to the caller to indicate that cloning was unsuccessful.
            return false;
        }
        compiler->fgInsertStmtAtEnd(to, compiler->gtNewStmt(newExpr));
    }
    return true;
}

// LIR helpers
void BasicBlock::MakeLIR()
{
    assert(!IsLIR());

    m_firstNode = nullptr;
    m_lastNode  = nullptr;
    bbFlags |= BBF_IS_LIR;
}

bool BasicBlock::IsLIR() const
{
    return (bbFlags & BBF_IS_LIR) != 0;
}

//------------------------------------------------------------------------
// firstStmt: Returns the first statement in the block
//
// Arguments:
//    None.
//
// Return Value:
//    The first statement in the block's bbStmtList.
//
Statement* BasicBlock::firstStmt() const
{
    return bbStmtList;
}

//------------------------------------------------------------------------
// lastStmt: Returns the last statement in the block
//
// Arguments:
//    None.
//
// Return Value:
//    The last statement in the block's bbStmtList.
//
Statement* BasicBlock::lastStmt() const
{
    if (bbStmtList == nullptr)
    {
        return nullptr;
    }

    Statement* result = bbStmtList->GetPrevStmt();
    assert(result != nullptr && result->GetNextStmt() == nullptr);
    return result;
}

void BasicBlock::SetLastStatement(Statement* last)
{
#ifdef DEBUG
    Statement* s = bbStmtList;
    while ((s != nullptr) && (s != last))
    {
        s = s->GetNextStmt();
    }
    assert(s == last);

    assert(last->GetNextStmt() == nullptr);
#endif

    bbStmtList->SetPrevStmt(last);
}

void BasicBlock::SetStatements(Statement* first, Statement* last)
{
#ifdef DEBUG
    assert(first->GetPrevStmt() == nullptr);

    Statement* s = first;
    while ((s != nullptr) && (s != last))
    {
        Statement* n = s->GetNextStmt();
        assert((n == nullptr) || (n->GetPrevStmt() == s));
        s = n;
    }
    assert(s == last);

    assert(last->GetNextStmt() == nullptr);
#endif

    bbStmtList = first;
    first->SetPrevStmt(last);
}

GenTree* BasicBlock::lastNode() const
{
    return IsLIR() ? m_lastNode : lastStmt()->GetRootNode();
}

//------------------------------------------------------------------------
// GetUniquePred: Returns the unique predecessor of a block, if one exists.
// The predecessor lists must be accurate.
//
// Arguments:
//    None.
//
// Return Value:
//    The unique predecessor of a block, or nullptr if there is no unique predecessor.
//
// Notes:
//    If the first block has a predecessor (which it may have, if it is the target of
//    a backedge), we never want to consider it "unique" because the prolog is an
//    implicit predecessor.

BasicBlock* BasicBlock::GetUniquePred(Compiler* compiler) const
{
    if ((bbPreds == nullptr) || (bbPreds->flNext != nullptr) || (this == compiler->fgFirstBB))
    {
        return nullptr;
    }
    else
    {
        return bbPreds->getBlock();
    }
}

//------------------------------------------------------------------------
// GetUniqueSucc: Returns the unique successor of a block, if one exists.
// Only considers BBJ_ALWAYS and BBJ_NONE block types.
//
// Arguments:
//    None.
//
// Return Value:
//    The unique successor of a block, or nullptr if there is no unique successor.

BasicBlock* BasicBlock::GetUniqueSucc() const
{
    if (bbJumpKind == BBJ_ALWAYS)
    {
        return bbJumpDest;
    }
    else if (bbJumpKind == BBJ_NONE)
    {
        return bbNext;
    }
    else
    {
        return nullptr;
    }
}

//------------------------------------------------------------------------
// isEmpty: check if block is empty or contains only ignorable statements
//
// Return Value:
//    True if block is empty, or contains only PHI assignments,
//    or contains zero or more PHI assignments followed by NOPs.
//
bool BasicBlock::isEmpty() const
{
    if (!IsLIR())
    {
        for (Statement* const stmt : NonPhiStatements())
        {
            if (!stmt->GetRootNode()->OperIs(GT_NOP))
            {
                return false;
            }
        }
    }
    else
    {
        for (GenTree* node : LIR::AsRange(this))
        {
            if (!node->OperIs(GT_IL_OFFSET))
            {
                return false;
            }
        }
    }

    return true;
}

Statement* BasicBlock::FirstNonPhiDef() const
{
    Statement* stmt = firstStmt();
    if (stmt == nullptr)
    {
        return nullptr;
    }
    GenTree* tree = stmt->GetRootNode();
    while (tree->IsPhiDef())
    {
        stmt = stmt->GetNextStmt();
        if (stmt == nullptr)
        {
            return nullptr;
        }
        tree = stmt->GetRootNode();
    }
    return stmt;
}

Statement* BasicBlock::FirstNonPhiDefOrCatchArgAsg() const
{
    Statement* stmt = FirstNonPhiDef();
    if (stmt == nullptr)
    {
        return nullptr;
    }
    GenTree* tree = stmt->GetRootNode();
    if (tree->OperIs(GT_LCL_STORE) && tree->AsLclStore()->GetValue()->OperIs(GT_CATCH_ARG))
    {
        stmt = stmt->GetNextStmt();
    }
    return stmt;
}

/*****************************************************************************
 *
 *  Can a BasicBlock be inserted after this without altering the flowgraph
 */

bool BasicBlock::bbFallsThrough() const
{
    switch (bbJumpKind)
    {
    case BBJ_THROW:
    case BBJ_EHFINALLYRET:
    case BBJ_EHFILTERRET:
    case BBJ_EHCATCHRET:
    case BBJ_RETURN:
    case BBJ_ALWAYS:
    case BBJ_LEAVE:
    case BBJ_SWITCH:
        return false;

    case BBJ_NONE:
    case BBJ_COND:
        return true;

    case BBJ_CALLFINALLY:
        return ((bbFlags & BBF_RETLESS_CALL) == 0);

    default:
        assert(!"Unknown bbJumpKind in bbFallsThrough()");
        return true;
    }
}

unsigned BasicBlock::NumSucc() const
{
    switch (bbJumpKind)
    {
    case BBJ_THROW:
    case BBJ_RETURN:
    case BBJ_EHFINALLYRET:
    case BBJ_EHFILTERRET:
        return 0;

    case BBJ_CALLFINALLY:
    case BBJ_ALWAYS:
    case BBJ_EHCATCHRET:
    case BBJ_LEAVE:
    case BBJ_NONE:
        return 1;

    case BBJ_COND:
        if (bbJumpDest == bbNext)
        {
            return 1;
        }
        else
        {
            return 2;
        }

    case BBJ_SWITCH:
        return bbJumpSwt->bbsCount;

    default:
        unreached();
    }
}

BasicBlock* BasicBlock::GetSucc(unsigned i) const
{
    assert(i < NumSucc()); // Index bounds check.
    switch (bbJumpKind)
    {
    case BBJ_CALLFINALLY:
    case BBJ_ALWAYS:
    case BBJ_EHCATCHRET:
    case BBJ_LEAVE:
        return bbJumpDest;

    case BBJ_NONE:
        return bbNext;

    case BBJ_COND:
        if (i == 0)
        {
            return bbNext;
        }
        else
        {
            assert(i == 1);
            return bbJumpDest;
        }

    case BBJ_SWITCH:
        return bbJumpSwt->bbsDstTab[i];

    default:
        unreached();
    }
}

// Returns the count of distinct block successors.
unsigned BasicBlock::NumSucc(Compiler* comp) const
{
    assert(comp != nullptr);

    switch (bbJumpKind)
    {
    case BBJ_EHFINALLYRET:
    {
        BasicBlock* hndBeg = comp->fgFirstBlockOfHandler(this);

        if (hndBeg->bbCatchTyp == BBCT_FINALLY)
        {
            return comp->fgNSuccsOfFinallyRet(this);
        }

        assert(hndBeg->bbCatchTyp == BBCT_FAULT);

        return 0;
    }

    case BBJ_THROW:
    case BBJ_RETURN:
        return 0;

    case BBJ_CALLFINALLY:
    case BBJ_ALWAYS:
    case BBJ_EHCATCHRET:
    case BBJ_EHFILTERRET:
    case BBJ_LEAVE:
    case BBJ_NONE:
        return 1;

    case BBJ_COND:
        return 1 + (bbJumpDest != bbNext);

    case BBJ_SWITCH:
        return comp->GetDescriptorForSwitch(this)->numDistinctSuccs;

    default:
        unreached();
    }
}

BasicBlock* BasicBlock::GetSucc(unsigned i, Compiler* comp) const
{
    assert(comp != nullptr);
    assert(i < NumSucc(comp));

    switch (bbJumpKind)
    {
    case BBJ_EHFINALLYRET:
        // Note: the following call is expensive.
        return comp->fgSuccOfFinallyRet(this, i);

    case BBJ_EHFILTERRET:
        assert(comp->fgFirstBlockOfHandler(this) == bbJumpDest);
        FALLTHROUGH;
    case BBJ_CALLFINALLY:
    case BBJ_ALWAYS:
    case BBJ_EHCATCHRET:
    case BBJ_LEAVE:
        return bbJumpDest;

    case BBJ_NONE:
        return bbNext;

    case BBJ_COND:
        if (i == 0)
        {
            return bbNext;
        }
        else
        {
            assert(i == 1);
            return bbJumpDest;
        }

    case BBJ_SWITCH:
    {
        BBswtDesc* sd = comp->GetDescriptorForSwitch(this);
        assert(i < sd->numDistinctSuccs); // Range check.
        return sd->nonDuplicates[i];
    }

    default:
        unreached();
    }
}

bool BasicBlock::EndsWithJmp(Compiler* comp) const
{
    return comp->compJmpOpUsed && (bbJumpKind == BBJ_RETURN) && ((bbFlags & BBF_HAS_JMP) != 0) &&
           lastNode()->OperIs(GT_JMP);
}

bool BasicBlock::EndsWithTailCall(Compiler* comp) const
{
    return EndsWithTailCall(comp, /*fastTailCallsOnly*/ false, /*tailCallsConvertibleToLoopOnly*/ false) != nullptr;
}

bool BasicBlock::EndsWithFastTailCall(Compiler* comp) const
{
    return EndsWithTailCall(comp, /*fastTailCallsOnly*/ true, /*tailCallsConvertibleToLoopOnly*/ false) != nullptr;
}

GenTreeCall* BasicBlock::EndsWithTailCallConvertibleToLoop(Compiler* comp) const
{
    return EndsWithTailCall(comp, /*fastTailCallsOnly*/ false, /*tailCallsConvertibleToLoopOnly*/ true);
}

GenTreeCall* BasicBlock::EndsWithTailCall(Compiler* comp,
                                          bool      fastTailCallsOnly,
                                          bool      tailCallsConvertibleToLoopOnly) const
{
    assert(!fastTailCallsOnly || !tailCallsConvertibleToLoopOnly);

    if (!comp->compTailCallUsed)
    {
        return nullptr;
    }

    bool result = (bbJumpKind == BBJ_RETURN) && ((bbFlags & BBF_HAS_JMP) != 0);

    if (!fastTailCallsOnly && !tailCallsConvertibleToLoopOnly)
    {
        result |= (bbJumpKind == BBJ_THROW);
    }

    if (!result)
    {
        return nullptr;
    }

    GenTreeCall* call = lastNode()->IsCall();

    if (call == nullptr)
    {
        return nullptr;
    }

    if (tailCallsConvertibleToLoopOnly)
    {
        result = call->IsTailCallConvertibleToLoop();
    }
    else if (fastTailCallsOnly)
    {
        result = call->IsFastTailCall();
    }
    else
    {
        result = call->IsTailCall();
    }

    return result ? call : nullptr;
}

/*****************************************************************************
 *
 *  Allocate a basic block but don't append it to the current BB list.
 */

BasicBlock* Compiler::bbNewBasicBlock(BBjumpKinds jumpKind)
{
    BasicBlock* block;

    /* Allocate the block descriptor and zero it out */
    assert(fgSafeBasicBlockCreation);

    block = new (this, CMK_BasicBlock) BasicBlock;

#if MEASURE_BLOCK_SIZE
    BasicBlock::s_Count += 1;
    BasicBlock::s_Size += sizeof(*block);
#endif

#ifdef DEBUG
    // fgLookupBB() is invalid until fgInitBBLookup() is called again.
    fgBBs = (BasicBlock**)0xCDCD;
#endif

    // scopeInfo needs to be able to differentiate between blocks which
    // correspond to some instrs (and so may have some LocalVarInfo
    // boundaries), or have been inserted by the JIT
    block->bbCodeOffs    = BAD_IL_OFFSET;
    block->bbCodeOffsEnd = BAD_IL_OFFSET;

#ifdef DEBUG
    block->bbID = compBasicBlockID++;
#endif

    /* Give the block a number, set the ancestor count and weight */

    ++fgBBcount;
    ++fgBBNumMax;

    if (compIsForInlining())
    {
        block->bbNum = ++impInlineInfo->InlinerCompiler->fgBBNumMax;
    }
    else
    {
        block->bbNum = fgBBNumMax;
    }

    if (compRationalIRForm)
    {
        block->bbFlags |= BBF_IS_LIR;
    }

    block->bbRefs       = 1;
    block->bbWeight     = BB_UNITY_WEIGHT;
    block->bbJumpKind   = jumpKind;
    block->bbNatLoopNum = NoLoopNum;

    if (jumpKind == BBJ_THROW)
    {
        block->bbSetRunRarely();
    }

    livInitNewBlock(block);

    JITDUMP("New Basic Block %s created.\n", block->dspToString());

    return block;
}

// Determine if this is the first block of a BBJ_CALLFINALLY/BBJ_ALWAYS pair
//
// In the flow graph, this becomes a block that calls the finally, and a second, immediately
// following empty block (in the bbNext chain) to which the finally will return, and which
// branches unconditionally to the next block to be executed outside the try/finally.
// Note that code is often generated differently than this description. For example, on ARM,
// the target of the BBJ_ALWAYS is loaded in LR (the return register), and a direct jump is
// made to the 'finally'. The effect is that the 'finally' returns directly to the target of
// the BBJ_ALWAYS. A "retless" BBJ_CALLFINALLY is one that has no corresponding BBJ_ALWAYS.
// This can happen if the finally is known to not return (e.g., it contains a 'throw'). In
// that case, the BBJ_CALLFINALLY flags has BBF_RETLESS_CALL set. Note that ARM never has
// "retless" BBJ_CALLFINALLY blocks due to a requirement to use the BBJ_ALWAYS for
// generating code.
bool BasicBlock::IsCallFinallyAlwaysPairHead() const
{
    if (bbJumpKind == BBJ_CALLFINALLY)
    {
#ifdef TARGET_ARM
        assert((bbFlags & BBF_RETLESS_CALL) == 0);
#else
        if ((bbFlags & BBF_RETLESS_CALL) == 0)
#endif
        {
            assert(bbNext != nullptr);
            assert(bbNext->bbJumpKind == BBJ_ALWAYS);
            assert((bbNext->bbFlags & BBF_KEEP_BBJ_ALWAYS) != 0);
            assert(bbNext->isEmpty());
            assert(bbNext->bbJumpDest != nullptr);

            return true;
        }
    }

    return false;
}

bool BasicBlock::IsCallFinallyAlwaysPairTail() const
{
    return (bbPrev != nullptr) && bbPrev->IsCallFinallyAlwaysPairHead();
}

// Return true iff the block is the target of an EH edge; false otherwise.
//
// For the purposes of this method (and its callers), an EH edge is one on
// which the EH flow model requires that all locals must be reloaded from
// the stack before use, since control flow may transfer to this block through
// control flow that is not reflected in the flowgraph.
// Note that having a predecessor in a different EH region doesn't require
// that locals must be reloaded from the stack. That's only required when
// this block might be entered via flow that is not represented by an edge
// in the flowgraph.
//
bool BasicBlock::hasEHBoundaryIn() const
{
    bool returnVal = bbCatchTyp != BBCT_NONE;

#ifdef FEATURE_EH_FUNCLETS
    assert(returnVal || ((bbFlags & BBF_FUNCLET_BEG) == 0));
#endif

    return returnVal;
}

// Returns true iff the block ends in an exception boundary that requires that
// no locals are live in registers; false otherwise.
//
// We may have a successor in a different EH region, but it is OK to have locals
// live in registers if any successor is a normal flow edge. That's because the
// EH write-thru semantics ensure that we always have an up-to-date value on the
// stack.
//
bool BasicBlock::hasEHBoundaryOut() const
{
    return (bbJumpKind == BBJ_EHFILTERRET) || (bbJumpKind == BBJ_EHFINALLYRET)
#ifdef FEATURE_EH_FUNCLETS
           || (bbJumpKind == BBJ_EHCATCHRET)
#endif
        ;
}

BBswtDesc::BBswtDesc(Compiler* comp, const BBswtDesc* other)
    : bbsDstTab(new (comp, CMK_BasicBlock) BasicBlock*[other->bbsCount])
    , bbsCount(other->bbsCount)
    , bbsDominantCase(other->bbsDominantCase)
    , bbsDominantFraction(other->bbsDominantFraction)
    , bbsHasDefault(other->bbsHasDefault)
    , bbsHasDominantCase(other->bbsHasDominantCase)
{
    for (unsigned i = 0; i < bbsCount; i++)
    {
        bbsDstTab[i] = other->bbsDstTab[i];
    }
}

#ifdef DEBUG

void BasicBlock::dspBlockILRange() const
{
    if (bbCodeOffs != BAD_IL_OFFSET)
    {
        printf("[%03X..", bbCodeOffs);
    }
    else
    {
        printf("[%s..", "???");
    }

    if (bbCodeOffsEnd != BAD_IL_OFFSET)
    {
        printf("%03X)", bbCodeOffsEnd);
    }
    else
    {
        printf("%s)", "???");
    }
}

void BasicBlock::dspFlags() const
{
    if (bbFlags & BBF_MARKED)
    {
        printf("m ");
    }
    if (bbFlags & BBF_REMOVED)
    {
        printf("del ");
    }
    if (bbFlags & BBF_DONT_REMOVE)
    {
        printf("keep ");
    }
    if (bbFlags & BBF_IMPORTED)
    {
        printf("i ");
    }
    if (bbFlags & BBF_INTERNAL)
    {
        printf("internal ");
    }
    if (bbFlags & BBF_TRY_BEG)
    {
        printf("try ");
    }
    if (bbFlags & BBF_RUN_RARELY)
    {
        printf("rare ");
    }
    if (bbFlags & BBF_LOOP_HEAD)
    {
        printf("Loop ");
    }
    if (bbFlags & BBF_LOOP_CALL0)
    {
        printf("Loop0 ");
    }
    if (bbFlags & BBF_LOOP_CALL1)
    {
        printf("Loop1 ");
    }
    if (bbFlags & BBF_HAS_LABEL)
    {
        printf("label ");
    }
    if (bbFlags & BBF_HAS_JMP)
    {
        printf("jmp ");
    }
    if (bbFlags & BBF_HAS_CALL)
    {
        printf("hascall ");
    }
    if (bbFlags & BBF_GC_SAFE_POINT)
    {
        printf("gcsafe ");
    }
    if (bbFlags & BBF_FUNCLET_BEG)
    {
        printf("flet ");
    }
    if (bbFlags & BBF_HAS_IDX_LEN)
    {
        printf("idxlen ");
    }
    if (bbFlags & BBF_HAS_NEWARRAY)
    {
        printf("new[] ");
    }
    if (bbFlags & BBF_HAS_NEWOBJ)
    {
        printf("newobj ");
    }
    if (bbFlags & BBF_HAS_NULLCHECK)
    {
        printf("nullcheck ");
    }
#if defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
    if (bbFlags & BBF_FINALLY_TARGET)
    {
        printf("ftarget ");
    }
#endif
    if (bbFlags & BBF_BACKWARD_JUMP)
    {
        printf("bwd ");
    }
    if (bbFlags & BBF_BACKWARD_JUMP_TARGET)
    {
        printf("bwd-target ");
    }
    if (bbFlags & BBF_PATCHPOINT)
    {
        printf("ppoint ");
    }
    if (bbFlags & BBF_RETLESS_CALL)
    {
        printf("retless ");
    }
    if (bbFlags & BBF_LOOP_PREHEADER)
    {
        printf("LoopPH ");
    }
    if (bbFlags & BBF_COLD)
    {
        printf("cold ");
    }
    if (bbFlags & BBF_PROF_WEIGHT)
    {
        printf("IBC ");
    }
    if (bbFlags & BBF_IS_LIR)
    {
        printf("LIR ");
    }
    if (bbFlags & BBF_KEEP_BBJ_ALWAYS)
    {
        printf("KEEP ");
    }
    if (bbFlags & BBF_CLONED_FINALLY_BEGIN)
    {
        printf("cfb ");
    }
    if (bbFlags & BBF_CLONED_FINALLY_END)
    {
        printf("cfe ");
    }
    if (bbFlags & BBF_LOOP_ALIGN)
    {
        printf("align ");
    }
}

unsigned BasicBlock::dspPreds() const
{
    unsigned length = 0;

    for (flowList* const pred : PredEdges())
    {
        length += printf("%s" FMT_BB, length == 0 ? "" : ",", pred->getBlock()->bbNum);

        if (pred->flDupCount > 1)
        {
            length += printf("(%u)", pred->flDupCount);
        }
    }

    return length;
}

unsigned BasicBlock::dspCheapPreds() const
{
    unsigned length = 0;

    for (BasicBlockList* pred = bbCheapPreds; pred != nullptr; pred = pred->next)
    {
        length += printf("%s" FMT_BB, length == 0 ? "" : ",", pred->block->bbNum);
    }

    return length;
}

void BasicBlock::dspSuccs(Compiler* compiler) const
{
    unsigned index = 0;

    for (BasicBlock* const succ : Succs(compiler))
    {
        printf("%s" FMT_BB, index++ == 0 ? "" : ",", succ->bbNum);
    }
}

void BasicBlock::dspJumpKind() const
{
    switch (bbJumpKind)
    {
    case BBJ_EHFINALLYRET:
        printf(" (finret)");
        return;

    case BBJ_EHFILTERRET:
        printf(" (fltret)");
        return;

    case BBJ_EHCATCHRET:
        printf(" -> " FMT_BB " (cret)", bbJumpDest->bbNum);
        return;

    case BBJ_THROW:
        printf(" (throw)");
        return;

    case BBJ_RETURN:
        printf(" (return)");
        return;

    case BBJ_NONE:
        return;

    case BBJ_ALWAYS:
        if (bbFlags & BBF_KEEP_BBJ_ALWAYS)
        {
            printf(" -> " FMT_BB " (ALWAYS)", bbJumpDest->bbNum);
        }
        else
        {
            printf(" -> " FMT_BB " (always)", bbJumpDest->bbNum);
        }
        return;

    case BBJ_LEAVE:
        printf(" -> " FMT_BB " (leave)", bbJumpDest->bbNum);
        return;

    case BBJ_CALLFINALLY:
        printf(" -> " FMT_BB " (callf)", bbJumpDest->bbNum);
        return;

    case BBJ_COND:
        printf(" -> " FMT_BB " (cond)", bbJumpDest->bbNum);
        return;

    case BBJ_SWITCH:
    {
        printf(" ->");

        BasicBlock** const successors = bbJumpSwt->bbsDstTab;

        for (unsigned i = 0, count = bbJumpSwt->bbsCount; i < count; i++)
        {
            printf("%c" FMT_BB, (i == 0) ? ' ' : ',', successors[i]->bbNum);

            if (bbJumpSwt->bbsHasDefault && (i == count - 1))
            {
                printf("[def]");
            }

            if (bbJumpSwt->bbsHasDominantCase && (i == bbJumpSwt->bbsDominantCase))
            {
                printf("[dom(" FMT_WT ")]", bbJumpSwt->bbsDominantFraction);
            }
        }

        printf(" (switch)");
        return;
    }

    default:
        unreached();
    }
}

void BasicBlock::dspBlockHeader(Compiler* compiler, bool showKind, bool showFlags, bool showPreds) const
{
    printf(FMT_BB " ", bbNum);

    dspBlockILRange();

    if (showKind)
    {
        dspJumpKind();
    }

    if (showPreds)
    {
        printf(", preds={");

        if (compiler->fgCheapPredsValid)
        {
            dspCheapPreds();
        }
        else
        {
            dspPreds();
        }

        printf("} succs={");
        dspSuccs(compiler);
        printf("}");
    }

    if (showFlags)
    {
        printf(" flags=%16llx: ", bbFlags);
        dspFlags();
    }

    printf("\n");
}

const char* BasicBlock::dspToString(int blockNumPadding) const
{
    static char   buffers[3][64]; // static array of 3 to allow 3 concurrent calls in one printf
    static size_t nextBufferIndex = 0;

    auto& buffer    = buffers[nextBufferIndex];
    nextBufferIndex = (nextBufferIndex + 1) % _countof(buffers);
    _snprintf_s(buffer, _countof(buffer), _countof(buffer), FMT_BB "%*s [%04u]", bbNum, blockNumPadding, "", bbID);
    return buffer;
}

#endif // DEBUG
