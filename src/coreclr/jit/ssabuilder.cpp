// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "ssaconfig.h"
#include "ssarenamestate.h"
#include "ssabuilder.h"

namespace
{
/**
 * Method that finds a common IDom parent, much like least common ancestor.
 *
 * @param finger1 A basic block that might share IDom ancestor with finger2.
 * @param finger2 A basic block that might share IDom ancestor with finger1.
 *
 * @see "A simple, fast dominance algorithm" by Keith D. Cooper, Timothy J. Harvey, Ken Kennedy.
 *
 * @return A basic block whose IDom is the dominator for finger1 and finger2,
 * or else NULL.  This may be called while immediate dominators are being
 * computed, and if the input values are members of the same loop (each reachable from the other),
 * then one may not yet have its immediate dominator computed when we are attempting
 * to find the immediate dominator of the other.  So a NULL return value means that the
 * the two inputs are in a cycle, not that they don't have a common dominator ancestor.
 */
static inline BasicBlock* IntersectDom(BasicBlock* finger1, BasicBlock* finger2)
{
    while (finger1 != finger2)
    {
        if (finger1 == nullptr || finger2 == nullptr)
        {
            return nullptr;
        }
        while (finger1 != nullptr && finger1->bbPostOrderNum < finger2->bbPostOrderNum)
        {
            finger1 = finger1->bbIDom;
        }
        if (finger1 == nullptr)
        {
            return nullptr;
        }
        while (finger2 != nullptr && finger2->bbPostOrderNum < finger1->bbPostOrderNum)
        {
            finger2 = finger2->bbIDom;
        }
    }
    return finger1;
}

} // end of anonymous namespace.

// =================================================================================
//                                      SSA
// =================================================================================

void Compiler::fgSsaBuild()
{
    assert(!ssaForm);

    SsaBuilder builder(this);
    builder.Build();
    ssaForm = true;

#ifdef DEBUG
    if (verbose)
    {
        JITDUMP("\nAfter fgSsaBuild:\n");
        fgDispBasicBlocks(/*dumpTrees*/ true);
    }
#endif // DEBUG
}

void Compiler::fgResetForSsa()
{
    for (unsigned i = 0; i < lvaCount; ++i)
    {
        lvaTable[i].lvPerSsaData.Reset();
    }

    lvMemoryPerSsaData.Reset();
    m_memorySsaMap = nullptr;

    for (BasicBlock* const blk : Blocks())
    {
        // Eliminate phis.

        blk->bbMemorySsaPhiFunc = nullptr;

        if (blk->bbStmtList != nullptr)
        {
            Statement* last = blk->lastStmt();
            blk->bbStmtList = blk->FirstNonPhiDef();
            if (blk->bbStmtList != nullptr)
            {
                blk->bbStmtList->SetPrevStmt(last);
            }
        }

        for (Statement* const stmt : blk->Statements())
        {
            for (GenTree* const tree : stmt->TreeList())
            {
                if (tree->IsLocal())
                {
                    tree->AsLclVarCommon()->SetSsaNum(SsaConfig::RESERVED_SSA_NUM);
                }
            }
        }
    }
}

/**
 *  Constructor for the SSA builder.
 *
 *  @param pCompiler Current compiler instance.
 *
 *  @remarks Initializes the class and member pointers/objects that use constructors.
 */
SsaBuilder::SsaBuilder(Compiler* pCompiler)
    : m_pCompiler(pCompiler)
    , m_allocator(pCompiler->getAllocator(CMK_SSA))
    , m_visitedTraits(0, pCompiler) // at this point we do not know the size, SetupBBRoot can add a block
    , m_renameStack(m_allocator, pCompiler->lvaCount)
{
}

//------------------------------------------------------------------------
//  TopologicalSort: Topologically sort the graph and return the number of nodes visited.
//
//  Arguments:
//     postOrder - The array in which the arranged basic blocks have to be returned.
//     count - The size of the postOrder array.
//
//  Return Value:
//     The number of nodes visited while performing DFS on the graph.

int SsaBuilder::TopologicalSort(BasicBlock** postOrder, int count)
{
    Compiler* comp = m_pCompiler;

    // TopologicalSort is called first so m_visited should already be empty
    assert(BitVecOps::IsEmpty(&m_visitedTraits, m_visited));

    // Display basic blocks.
    DBEXEC(VERBOSE, comp->fgDispBasicBlocks());
    DBEXEC(VERBOSE, comp->fgDispHandlerTab());

    auto DumpBlockAndSuccessors = [](Compiler* comp, BasicBlock* block) {
#ifdef DEBUG
        if (comp->verboseSsa)
        {
            printf("[SsaBuilder::TopologicalSort] Pushing " FMT_BB ": [", block->bbNum);
            AllSuccessorEnumerator successors(comp, block);
            unsigned               index = 0;
            while (true)
            {
                bool        isEHsucc = successors.IsNextEHSuccessor();
                BasicBlock* succ     = successors.NextSuccessor(comp);

                if (succ == nullptr)
                {
                    break;
                }

                printf("%s%s" FMT_BB, (index++ ? ", " : ""), (isEHsucc ? "[EH]" : ""), succ->bbNum);
            }
            printf("]\n");
        }
#endif
    };

    // Compute order.
    int         postIndex = 0;
    BasicBlock* block     = comp->fgFirstBB;
    BitVecOps::AddElemD(&m_visitedTraits, m_visited, block->bbNum);

    ArrayStack<AllSuccessorEnumerator> blocks(m_allocator);
    blocks.Emplace(comp, block);
    DumpBlockAndSuccessors(comp, block);

    while (!blocks.Empty())
    {
        BasicBlock* block = blocks.TopRef().Block();
        BasicBlock* succ  = blocks.TopRef().NextSuccessor(comp);

        if (succ != nullptr)
        {
            // if the block on TOS still has unreached successors, visit them
            if (BitVecOps::TryAddElemD(&m_visitedTraits, m_visited, succ->bbNum))
            {
                blocks.Emplace(comp, succ);
                DumpBlockAndSuccessors(comp, succ);
            }
        }
        else
        {
            // all successors have been visited
            blocks.Pop();

            DBG_SSA_JITDUMP("[SsaBuilder::TopologicalSort] postOrder[%d] = " FMT_BB "\n", postIndex, block->bbNum);
            postOrder[postIndex]  = block;
            block->bbPostOrderNum = postIndex;
            postIndex += 1;
        }
    }

    // In the absence of EH (because catch/finally have no preds), this should be valid.
    // assert(postIndex == (count - 1));

    return postIndex;
}

/**
 * Computes the immediate dominator IDom for each block iteratively.
 *
 * @param postOrder The array of basic blocks arranged in postOrder.
 * @param count The size of valid elements in the postOrder array.
 *
 * @see "A simple, fast dominance algorithm." paper.
 */
void SsaBuilder::ComputeImmediateDom(BasicBlock** postOrder, int count)
{
    JITDUMP("[SsaBuilder::ComputeImmediateDom]\n");

    // Add entry point to visited as its IDom is NULL.
    BitVecOps::ClearD(&m_visitedTraits, m_visited);
    BitVecOps::AddElemD(&m_visitedTraits, m_visited, m_pCompiler->fgFirstBB->bbNum);

    assert(postOrder[count - 1] == m_pCompiler->fgFirstBB);

    bool changed = true;
    while (changed)
    {
        changed = false;

        // In reverse post order, except for the entry block (count - 1 is entry BB).
        for (int i = count - 2; i >= 0; --i)
        {
            BasicBlock* block = postOrder[i];

            DBG_SSA_JITDUMP("Visiting in reverse post order: " FMT_BB ".\n", block->bbNum);

            // Find the first processed predecessor block.
            flowList*   predList  = m_pCompiler->BlockPredsWithEH(block);
            BasicBlock* predBlock = nullptr;
            for (flowList* pred = predList; pred != nullptr; pred = pred->flNext)
            {
                if (BitVecOps::IsMember(&m_visitedTraits, m_visited, pred->getBlock()->bbNum))
                {
                    predBlock = pred->getBlock();
                    break;
                }
            }

            // There could just be a single basic block, so just check if there were any preds.
            if (predBlock != nullptr)
            {
                DBG_SSA_JITDUMP("Pred block is " FMT_BB ".\n", predBlock->bbNum);
            }

            // Intersect DOM, if computed, for all predecessors.
            BasicBlock* bbIDom = predBlock;
            for (flowList* pred = predList; pred != nullptr; pred = pred->flNext)
            {
                if (predBlock != pred->getBlock())
                {
                    BasicBlock* domAncestor = IntersectDom(pred->getBlock(), bbIDom);
                    // The result may be NULL if "block" and "pred->getBlock()" are part of a
                    // cycle -- neither is guaranteed ordered wrt the other in reverse postorder,
                    // so we may be computing the IDom of "block" before the IDom of "pred->getBlock()" has
                    // been computed.  But that's OK -- if they're in a cycle, they share the same immediate
                    // dominator, so the contribution of "pred->getBlock()" is not necessary to compute
                    // the result.
                    if (domAncestor != nullptr)
                    {
                        bbIDom = domAncestor;
                    }
                }
            }

            // Did we change the bbIDom value?  If so, we go around the outer loop again.
            if (block->bbIDom != bbIDom)
            {
                changed = true;

                // IDom has changed, update it.
                DBG_SSA_JITDUMP("bbIDom of " FMT_BB " becomes " FMT_BB ".\n", block->bbNum, bbIDom ? bbIDom->bbNum : 0);
                block->bbIDom = bbIDom;
            }

            // Mark the current block as visited.
            BitVecOps::AddElemD(&m_visitedTraits, m_visited, block->bbNum);

            DBG_SSA_JITDUMP("Marking block " FMT_BB " as processed.\n", block->bbNum);
        }
    }
}

//------------------------------------------------------------------------
// ComputeDominanceFrontiers: Compute flow graph dominance frontiers
//
// Arguments:
//    postOrder - an array containing all flow graph blocks
//    count     - the number of blocks in the postOrder array
//    mapDF     - a caller provided hashtable that will be populated
//                with blocks and their dominance frontiers (only those
//                blocks that have non-empty frontiers will be included)
//
// Notes:
//     Recall that the dominance frontier of a block B is the set of blocks
//     B3 such that there exists some B2 s.t. B3 is a successor of B2, and
//     B dominates B2. Note that this dominance need not be strict -- B2
//     and B may be the same node.
//     See "A simple, fast dominance algorithm", by Cooper, Harvey, and Kennedy.
//
void SsaBuilder::ComputeDominanceFrontiers(BasicBlock** postOrder, int count, BlkToBlkVectorMap* mapDF)
{
    DBG_SSA_JITDUMP("Computing DF:\n");

    for (int i = 0; i < count; ++i)
    {
        BasicBlock* block = postOrder[i];

        DBG_SSA_JITDUMP("Considering block " FMT_BB ".\n", block->bbNum);

        // Recall that B3 is in the dom frontier of B1 if there exists a B2
        // such that B1 dom B2, !(B1 dom B3), and B3 is an immediate successor
        // of B2.  (Note that B1 might be the same block as B2.)
        // In that definition, we're considering "block" to be B3, and trying
        // to find B1's.  To do so, first we consider the predecessors of "block",
        // searching for candidate B2's -- "block" is obviously an immediate successor
        // of its immediate predecessors.  If there are zero or one preds, then there
        // is no pred, or else the single pred dominates "block", so no B2 exists.

        flowList* blockPreds = m_pCompiler->BlockPredsWithEH(block);

        // If block has 0/1 predecessor, skip.
        if ((blockPreds == nullptr) || (blockPreds->flNext == nullptr))
        {
            DBG_SSA_JITDUMP("   Has %d preds; skipping.\n", blockPreds == nullptr ? 0 : 1);
            continue;
        }

        // Otherwise, there are > 1 preds.  Each is a candidate B2 in the definition --
        // *unless* it dominates "block"/B3.

        for (flowList* pred = blockPreds; pred != nullptr; pred = pred->flNext)
        {
            DBG_SSA_JITDUMP("   Considering predecessor " FMT_BB ".\n", pred->getBlock()->bbNum);

            // If we've found a B2, then consider the possible B1's.  We start with
            // B2, since a block dominates itself, then traverse upwards in the dominator
            // tree, stopping when we reach the root, or the immediate dominator of "block"/B3.
            // (Note that we are guaranteed to encounter this immediate dominator of "block"/B3:
            // a predecessor must be dominated by B3's immediate dominator.)
            // Along this way, make "block"/B3 part of the dom frontier of the B1.
            // When we reach this immediate dominator, the definition no longer applies, since this
            // potential B1 *does* dominate "block"/B3, so we stop.
            for (BasicBlock* b1 = pred->getBlock(); (b1 != nullptr) && (b1 != block->bbIDom); // !root && !loop
                 b1             = b1->bbIDom)
            {
                DBG_SSA_JITDUMP("      Adding " FMT_BB " to dom frontier of pred dom " FMT_BB ".\n", block->bbNum,
                                b1->bbNum);

                BlkVector& b1DF = *mapDF->Emplace(b1, m_allocator);
                // It's possible to encounter the same DF multiple times, ensure that we don't add duplicates.
                if (b1DF.empty() || (b1DF.back() != block))
                {
                    b1DF.push_back(block);
                }
            }
        }
    }

#ifdef DEBUG
    if (m_pCompiler->verboseSsa)
    {
        printf("\nComputed DF:\n");
        for (int i = 0; i < count; ++i)
        {
            BasicBlock* b = postOrder[i];
            printf("Block " FMT_BB " := {", b->bbNum);

            BlkVector* bDF = mapDF->LookupPointer(b);
            if (bDF != nullptr)
            {
                int index = 0;
                for (BasicBlock* f : *bDF)
                {
                    printf("%s" FMT_BB, (index++ == 0) ? "" : ",", f->bbNum);
                }
            }
            printf("}\n");
        }
    }
#endif
}

//------------------------------------------------------------------------
// ComputeIteratedDominanceFrontier: Compute the iterated dominance frontier
// for the specified block.
//
// Arguments:
//    b     - the block to computed the frontier for
//    mapDF - a map containing the dominance frontiers of all blocks
//    bIDF  - a caller provided vector where the IDF is to be stored
//
// Notes:
//    The iterated dominance frontier is formed by a closure operation:
//    the IDF of B is the smallest set that includes B's dominance frontier,
//    and also includes the dominance frontier of all elements of the set.
//
void SsaBuilder::ComputeIteratedDominanceFrontier(BasicBlock* b, const BlkToBlkVectorMap* mapDF, BlkVector* bIDF)
{
    assert(bIDF->empty());

    BlkVector* bDF = mapDF->LookupPointer(b);

    if (bDF != nullptr)
    {
        // Compute IDF(b) - start by adding DF(b) to IDF(b).
        bIDF->reserve(bDF->size());
        BitVecOps::ClearD(&m_visitedTraits, m_visited);

        for (BasicBlock* f : *bDF)
        {
            BitVecOps::AddElemD(&m_visitedTraits, m_visited, f->bbNum);
            bIDF->push_back(f);
        }

        // Now for each block f from IDF(b) add DF(f) to IDF(b). This may result in new
        // blocks being added to IDF(b) and the process repeats until no more new blocks
        // are added. Note that since we keep adding to bIDF we can't use iterators as
        // they may get invalidated. This happens to be a convenient way to avoid having
        // to track newly added blocks in a separate set.
        for (size_t newIndex = 0; newIndex < bIDF->size(); newIndex++)
        {
            BasicBlock* f   = (*bIDF)[newIndex];
            BlkVector*  fDF = mapDF->LookupPointer(f);

            if (fDF != nullptr)
            {
                for (BasicBlock* ff : *fDF)
                {
                    if (BitVecOps::TryAddElemD(&m_visitedTraits, m_visited, ff->bbNum))
                    {
                        bIDF->push_back(ff);
                    }
                }
            }
        }
    }

#ifdef DEBUG
    if (m_pCompiler->verboseSsa)
    {
        printf("IDF(" FMT_BB ") := {", b->bbNum);
        int index = 0;
        for (BasicBlock* f : *bIDF)
        {
            printf("%s" FMT_BB, (index++ == 0) ? "" : ",", f->bbNum);
        }
        printf("}\n");
    }
#endif
}

/**
 * Returns the phi GT_PHI node if the variable already has a phi node.
 *
 * @param block The block for which the existence of a phi node needs to be checked.
 * @param lclNum The lclNum for which the occurrence of a phi node needs to be checked.
 *
 * @return If there is a phi node for the lclNum, returns the GT_PHI tree, else NULL.
 */
static GenTree* GetPhiNode(BasicBlock* block, unsigned lclNum)
{
    // Walk the statements for phi nodes.
    for (Statement* const stmt : block->Statements())
    {
        // A prefix of the statements of the block are phi definition nodes. If we complete processing
        // that prefix, exit.
        if (!stmt->IsPhiDefnStmt())
        {
            break;
        }

        GenTree* tree = stmt->GetRootNode();

        GenTree* phiLhs = tree->AsOp()->gtOp1;
        assert(phiLhs->OperGet() == GT_LCL_VAR);
        if (phiLhs->AsLclVarCommon()->GetLclNum() == lclNum)
        {
            return tree->AsOp()->gtOp2;
        }
    }
    return nullptr;
}

//------------------------------------------------------------------------
// InsertPhi: Insert a new GT_PHI statement.
//
// Arguments:
//    block  - The block where to insert the statement
//    lclNum - The variable number
//
void SsaBuilder::InsertPhi(BasicBlock* block, unsigned lclNum)
{
    var_types type = m_pCompiler->lvaGetDesc(lclNum)->TypeGet();

    GenTree* lhs = m_pCompiler->gtNewLclvNode(lclNum, type);
    // PHIs and all the associated nodes do not generate any code so the costs are always 0
    lhs->SetCosts(0, 0);
    GenTree* phi = new (m_pCompiler, GT_PHI) GenTreePhi(type);
    phi->SetCosts(0, 0);
    GenTree* asg = m_pCompiler->gtNewAssignNode(lhs, phi);
    // Evaluate the assignment RHS (the PHI node) first. This way the LHS will end up right
    // in front of the assignment in the linear order, that ensures that using FindUser
    // on the LHS to find the assignment doesn't have to traverse the PHI and its args.
    asg->gtFlags |= GTF_REVERSE_OPS;
    asg->SetCosts(0, 0);

    // Create the statement and chain everything in linear order - PHI, LCL_VAR, ASG
    Statement* stmt = m_pCompiler->gtNewStmt(asg);
    stmt->SetTreeList(phi);
    phi->gtNext = lhs;
    lhs->gtPrev = phi;
    lhs->gtNext = asg;
    asg->gtPrev = lhs;

#ifdef DEBUG
    unsigned seqNum = 1;
    for (GenTree* const node : stmt->TreeList())
    {
        node->gtSeqNum = seqNum++;
    }
#endif // DEBUG

    m_pCompiler->fgInsertStmtAtBeg(block, stmt);

    JITDUMP("Added PHI definition for V%02u at start of " FMT_BB ".\n", lclNum, block->bbNum);
}

//------------------------------------------------------------------------
// AddPhiArg: Add a new GT_PHI_ARG node to an existing GT_PHI node.
//
// Arguments:
//    block  - The block that contains the statement
//    stmt   - The statement that contains the GT_PHI node
//    lclNum - The variable number
//    ssaNum - The SSA number
//    pred   - The predecessor block
//
void SsaBuilder::AddPhiArg(
    BasicBlock* block, Statement* stmt, GenTreePhi* phi, unsigned lclNum, unsigned ssaNum, BasicBlock* pred)
{
#ifdef DEBUG
    // Make sure it isn't already present: we should only add each definition once.
    for (GenTreePhi::Use& use : phi->Uses())
    {
        assert(use.GetNode()->AsPhiArg()->GetSsaNum() != ssaNum);
    }
#endif // DEBUG

    var_types type = m_pCompiler->lvaGetDesc(lclNum)->TypeGet();

    GenTree* phiArg = new (m_pCompiler, GT_PHI_ARG) GenTreePhiArg(type, lclNum, ssaNum, pred);
    // Costs are not relevant for PHI args.
    phiArg->SetCosts(0, 0);
    // The argument order doesn't matter so just insert at the front of the list because
    // it's easier. It's also easier to insert in linear order since the first argument
    // will be first in linear order as well.
    phi->gtUses = new (m_pCompiler, CMK_ASTNode) GenTreePhi::Use(phiArg, phi->gtUses);

    GenTree* head = stmt->GetTreeList();
    assert(head->OperIs(GT_PHI, GT_PHI_ARG));
    stmt->SetTreeList(phiArg);
    phiArg->gtNext = head;
    head->gtPrev   = phiArg;

#ifdef DEBUG
    unsigned seqNum = 1;
    for (GenTree* const node : stmt->TreeList())
    {
        node->gtSeqNum = seqNum++;
    }
#endif // DEBUG

    DBG_SSA_JITDUMP("Added PHI arg u:%d for V%02u from " FMT_BB " in " FMT_BB ".\n", ssaNum, lclNum, pred->bbNum,
                    block->bbNum);
}

// Special value to represent a to-be-filled in Memory Phi arg list.
static BasicBlock::MemoryPhiArg EmptyMemoryPhiDef(0, nullptr);

/**
 * Inserts phi functions at DF(b) for variables v that are live after the phi
 * insertion point i.e., v in live-in(b).
 *
 * To do so, the function computes liveness, dominance frontier and inserts a phi node,
 * if we have var v in def(b) and live-in(l) and l is in DF(b).
 *
 * @param postOrder The array of basic blocks arranged in postOrder.
 * @param count The size of valid elements in the postOrder array.
 */
void SsaBuilder::InsertPhiFunctions(BasicBlock** postOrder, int count)
{
    JITDUMP("*************** In SsaBuilder::InsertPhiFunctions()\n");

    // Compute dominance frontier.
    BlkToBlkVectorMap mapDF(m_allocator);
    ComputeDominanceFrontiers(postOrder, count, &mapDF);
    EndPhase(PHASE_BUILD_SSA_DF);

    // Use the same IDF vector for all blocks to avoid unnecessary memory allocations
    BlkVector blockIDF(m_allocator);

    JITDUMP("Inserting phi functions:\n");

    for (int i = 0; i < count; ++i)
    {
        BasicBlock* block = postOrder[i];
        DBG_SSA_JITDUMP("Considering dominance frontier of block " FMT_BB ":\n", block->bbNum);

        blockIDF.clear();
        ComputeIteratedDominanceFrontier(block, &mapDF, &blockIDF);

        if (blockIDF.empty())
        {
            continue;
        }

        // For each local var number "lclNum" that "block" assigns to...
        VarSetOps::Iter defVars(m_pCompiler, block->bbVarDef);
        unsigned        varIndex = 0;
        while (defVars.NextElem(&varIndex))
        {
            unsigned lclNum = m_pCompiler->lvaTrackedIndexToLclNum(varIndex);
            DBG_SSA_JITDUMP("  Considering local var V%02u:\n", lclNum);

            if (!m_pCompiler->lvaInSsa(lclNum))
            {
                DBG_SSA_JITDUMP("  Skipping because it is excluded.\n");
                continue;
            }

            // For each block "bbInDomFront" that is in the dominance frontier of "block"...
            for (BasicBlock* bbInDomFront : blockIDF)
            {
                DBG_SSA_JITDUMP("     Considering " FMT_BB " in dom frontier of " FMT_BB ":\n", bbInDomFront->bbNum,
                                block->bbNum);

                // Check if variable "lclNum" is live in block "*iterBlk".
                if (!VarSetOps::IsMember(m_pCompiler, bbInDomFront->bbLiveIn, varIndex))
                {
                    continue;
                }

                // Check if we've already inserted a phi node.
                if (GetPhiNode(bbInDomFront, lclNum) == nullptr)
                {
                    // We have a variable i that is defined in block j and live at l, and l belongs to dom frontier of
                    // j. So insert a phi node at l.
                    InsertPhi(bbInDomFront, lclNum);
                }
            }
        }

        // Now make a similar phi definition if the block defines memory.
        if (block->bbMemoryDef)
        {
            // For each block "bbInDomFront" that is in the dominance frontier of "block".
            for (BasicBlock* bbInDomFront : blockIDF)
            {
                DBG_SSA_JITDUMP("     Considering " FMT_BB " in dom frontier of " FMT_BB " for Memory phis:\n",
                                bbInDomFront->bbNum, block->bbNum);

                // Check if memory is live into block "*iterBlk".
                if (!bbInDomFront->bbMemoryLiveIn)
                {
                    continue;
                }

                // Check if we've already inserted a phi node.
                if (bbInDomFront->bbMemorySsaPhiFunc == nullptr)
                {
                    // We have a variable i that is defined in block j and live at l, and l belongs to dom frontier
                    // of j. So insert a phi node at l.
                    JITDUMP("Inserting phi definition for Memory at start of " FMT_BB ".\n", bbInDomFront->bbNum);
                    bbInDomFront->bbMemorySsaPhiFunc = &EmptyMemoryPhiDef;
                }
            }
        }
    }
    EndPhase(PHASE_BUILD_SSA_INSERT_PHIS);
}

//------------------------------------------------------------------------
// RenameDef: Rename a local or memory definition generated by a GT_ASG node.
//
// Arguments:
//    asgNode - The GT_ASG node that generates the definition
//    block - The basic block that contains `asgNode`
//
void SsaBuilder::RenameDef(GenTreeOp* asgNode, BasicBlock* block)
{
    assert(asgNode->OperIs(GT_ASG));

    GenTree*             dst     = asgNode->GetOp(0);
    GenTreeLclVarCommon* lclNode = nullptr;

    if (dst->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        lclNode           = dst->AsLclVarCommon();
        unsigned   lclNum = lclNode->GetLclNum();
        LclVarDsc* varDsc = m_pCompiler->lvaGetDesc(lclNum);

        if (varDsc->IsInSsa())
        {
            // Promoted variables are not in SSA, only their fields are.
            assert(!varDsc->IsPromoted());
            // If it's a SSA local then it cannot be address exposed and thus does not define SSA memory.
            assert(!varDsc->IsAddressExposed());
            // This should have been marked as defintion.
            assert((lclNode->gtFlags & GTF_VAR_DEF) != 0);

            unsigned ssaNum = varDsc->lvPerSsaData.AllocSsaNum(m_allocator, block, asgNode);

            if (lclNode->IsPartialLclFld(m_pCompiler))
            {
                assert((lclNode->gtFlags & GTF_VAR_USEASG) != 0);

                lclNode->SetSsaNum(m_renameStack.Top(lclNum));
                m_pCompiler->SetPartialSsaDefNum(lclNode->AsLclFld(), ssaNum);
            }
            else
            {
                assert((lclNode->gtFlags & GTF_VAR_USEASG) == 0);
                lclNode->SetSsaNum(ssaNum);
            }

            m_renameStack.Push(block, lclNum, ssaNum);

            // If necessary, add "lclNum/ssaNum" to the arg list of a phi def in any
            // handlers for try blocks that "block" is within.  (But only do this for "real" definitions,
            // not phi definitions.)
            if (!asgNode->gtGetOp2()->OperIs(GT_PHI))
            {
                AddDefToHandlerPhis(block, lclNum, ssaNum);
            }

            return;
        }

        lclNode->SetSsaNum(SsaConfig::RESERVED_SSA_NUM);

        if (!varDsc->IsAddressExposed())
        {
            return;
        }
    }

    // Figure out if "asgNode" may make a new GC heap state (if we care for this block).
    if (!block->bbMemoryHavoc && m_pCompiler->ehBlockHasExnFlowDsc(block))
    {
        bool isMemoryDef = false;

        if (lclNode != nullptr)
        {
            isMemoryDef = m_pCompiler->lvaGetDesc(lclNode)->IsAddressExposed();
        }
        else
        {
            if (GenTreeIndir* indir = dst->IsIndir())
            {
                if (GenTreeLclVarCommon* lclAddr = indir->GetAddr()->IsLocalAddrExpr())
                {
                    assert(m_pCompiler->lvaGetDesc(lclAddr)->IsAddressExposed());
                }
            }

            isMemoryDef = true;
        }

        // TODO-MIKE-Review: Looks like this misses HWINTRINSIC memory stores...

        if (isMemoryDef)
        {
            unsigned ssaNum = m_pCompiler->lvMemoryPerSsaData.AllocSsaNum(m_allocator);
            m_renameStack.PushMemory(block, ssaNum);
            m_pCompiler->GetMemorySsaMap()->Set(asgNode, ssaNum);

#ifdef DEBUG
            if (m_pCompiler->verboseSsa)
            {
                printf("Node [%06u] in try block defines memory; SSA # = %u.\n", asgNode->GetID(), ssaNum);
            }
#endif

            AddMemoryDefToHandlerPhis(block, ssaNum);
        }
    }
}

//------------------------------------------------------------------------
// RenameLclUse: Rename a use of a local variable.
//
// Arguments:
//    lclNode - A GT_LCL_VAR or GT_LCL_FLD node that is not a definition
//
void SsaBuilder::RenameLclUse(GenTreeLclVarCommon* lclNode)
{
    assert(lclNode->OperIs(GT_LCL_VAR, GT_LCL_FLD));
    assert((lclNode->gtFlags & GTF_VAR_DEF) == 0);

    unsigned lclNum = lclNode->GetLclNum();
    unsigned ssaNum;

    if (!m_pCompiler->lvaInSsa(lclNum))
    {
        ssaNum = SsaConfig::RESERVED_SSA_NUM;
    }
    else
    {
        // Promoted variables are not in SSA, only their fields are.
        assert(!m_pCompiler->lvaGetDesc(lclNum)->lvPromoted);

        ssaNum = m_renameStack.Top(lclNum);
    }

    lclNode->SetSsaNum(ssaNum);
}

void SsaBuilder::AddDefToHandlerPhis(BasicBlock* block, unsigned lclNum, unsigned ssaNum)
{
    assert(m_pCompiler->lvaTable[lclNum].lvTracked); // Precondition.
    unsigned lclIndex = m_pCompiler->lvaTable[lclNum].lvVarIndex;

    EHblkDsc* tryBlk = m_pCompiler->ehGetBlockExnFlowDsc(block);
    if (tryBlk != nullptr)
    {
        DBG_SSA_JITDUMP("Definition of local V%02u/d:%d in block " FMT_BB
                        " has exn handler; adding as phi arg to handlers.\n",
                        lclNum, ssaNum, block->bbNum);
        while (true)
        {
            BasicBlock* handler = tryBlk->ExFlowBlock();

            // Is "lclNum" live on entry to the handler?
            if (VarSetOps::IsMember(m_pCompiler, handler->bbLiveIn, lclIndex))
            {
#ifdef DEBUG
                bool phiFound = false;
#endif
                // A prefix of blocks statements will be SSA definitions.  Search those for "lclNum".
                for (Statement* const stmt : handler->Statements())
                {
                    // If the tree is not an SSA def, break out of the loop: we're done.
                    if (!stmt->IsPhiDefnStmt())
                    {
                        break;
                    }

                    GenTree* tree = stmt->GetRootNode();

                    assert(tree->IsPhiDefn());

                    if (tree->AsOp()->gtOp1->AsLclVar()->GetLclNum() == lclNum)
                    {
                        // It's the definition for the right local.  Add "ssaNum" to the RHS.
                        AddPhiArg(handler, stmt, tree->gtGetOp2()->AsPhi(), lclNum, ssaNum, block);
#ifdef DEBUG
                        phiFound = true;
#endif
                        break;
                    }
                }
                assert(phiFound);
            }

            unsigned nextTryIndex = tryBlk->ebdEnclosingTryIndex;
            if (nextTryIndex == EHblkDsc::NO_ENCLOSING_INDEX)
            {
                break;
            }

            tryBlk = m_pCompiler->ehGetDsc(nextTryIndex);
        }
    }
}

void SsaBuilder::AddMemoryDefToHandlerPhis(BasicBlock* block, unsigned ssaNum)
{
    assert(m_pCompiler->ehBlockHasExnFlowDsc(block));

    // Don't do anything for a compiler-inserted BBJ_ALWAYS that is a "leave helper".
    if ((block->bbFlags & BBF_INTERNAL) && block->isBBCallAlwaysPairTail())
    {
        return;
    }

    // Otherwise...
    DBG_SSA_JITDUMP("Definition of Memory:%u in block " FMT_BB " has exn handler; adding as phi arg to handlers.\n",
                    ssaNum, block->bbNum);
    EHblkDsc* tryBlk = m_pCompiler->ehGetBlockExnFlowDsc(block);
    while (true)
    {
        BasicBlock* handler = tryBlk->ExFlowBlock();

        // Is memoryKind live on entry to the handler?
        if (handler->bbMemoryLiveIn)
        {
            // Add "ssaNum" to the phi args of memoryKind.
            BasicBlock::MemoryPhiArg*& phiArg = handler->bbMemorySsaPhiFunc;

            if (phiArg == &EmptyMemoryPhiDef)
            {
                phiArg = nullptr;
            }

#ifdef DEBUG
            for (auto arg = phiArg; arg != nullptr; arg = arg->GetNext())
            {
                assert(arg->GetSsaNum() != ssaNum);
            }
#endif

            phiArg = new (m_pCompiler) BasicBlock::MemoryPhiArg(ssaNum, phiArg);

            DBG_SSA_JITDUMP("   Added phi arg u:%u for Memory to phi defn in handler block " FMT_BB ".\n", ssaNum,
                            handler->bbNum);
        }
        unsigned tryInd = tryBlk->ebdEnclosingTryIndex;
        if (tryInd == EHblkDsc::NO_ENCLOSING_INDEX)
        {
            break;
        }
        tryBlk = m_pCompiler->ehGetDsc(tryInd);
    }
}

//------------------------------------------------------------------------
// BlockRenameVariables: Rename all definitions and uses within a block.
//
// Arguments:
//    block - The block
//
void SsaBuilder::BlockRenameVariables(BasicBlock* block)
{
    // First handle the incoming memory state.
    if (block->bbMemorySsaPhiFunc != nullptr)
    {
        unsigned ssaNum = m_pCompiler->lvMemoryPerSsaData.AllocSsaNum(m_allocator);
        m_renameStack.PushMemory(block, ssaNum);

        DBG_SSA_JITDUMP("Ssa # for Memory phi on entry to " FMT_BB " is %d.\n", block->bbNum, ssaNum);

        block->bbMemorySsaNumIn = ssaNum;
    }
    else
    {
        block->bbMemorySsaNumIn = m_renameStack.TopMemory();
    }

    BasicBlockFlags earlyPropBlockSummary = BBF_EMPTY;

    // Walk the statements of the block and rename definitions and uses.
    for (Statement* const stmt : block->Statements())
    {
        for (GenTree* const tree : stmt->Nodes())
        {
            if (tree->OperIs(GT_ASG))
            {
                RenameDef(tree->AsOp(), block);
            }
            // PHI_ARG nodes already have SSA numbers so we only need to check LCL_VAR and LCL_FLD nodes.
            else if (tree->OperIs(GT_LCL_VAR, GT_LCL_FLD))
            {
                if ((tree->gtFlags & GTF_VAR_DEF) == 0)
                {
                    RenameLclUse(tree->AsLclVarCommon());
                }
            }
            else if (tree->OperIs(GT_NULLCHECK))
            {
                earlyPropBlockSummary |= BBF_HAS_NULLCHECK;
            }
            else if (tree->OperIs(GT_ARR_LENGTH))
            {
                earlyPropBlockSummary |= BBF_HAS_IDX_LEN;
            }
            else if (tree->IsHelperCall())
            {
                GenTreeCall*   call   = tree->AsCall();
                GenTreeIntCon* length = nullptr;

                switch (Compiler::eeGetHelperNum(call->GetMethodHandle()))
                {
                    case CORINFO_HELP_NEWARR_1_DIRECT:
                    case CORINFO_HELP_NEWARR_1_OBJ:
                    case CORINFO_HELP_NEWARR_1_VC:
                    case CORINFO_HELP_NEWARR_1_ALIGN8:
                        length = call->GetArgNodeByArgNum(1)->IsIntCon();
                        break;
                    case CORINFO_HELP_READYTORUN_NEWARR_1:
                        length = call->GetArgNodeByArgNum(call->GetInfo()->GetArgCount() - 1)->IsIntCon();
                        break;
                    default:
                        break;
                }

                if (length != nullptr)
                {
                    earlyPropBlockSummary |= BBF_HAS_NEWARRAY;
                }
            }
        }
    }

    block->bbFlags |= earlyPropBlockSummary;

    if ((earlyPropBlockSummary & BBF_HAS_NULLCHECK) != 0)
    {
        m_pCompiler->optMethodFlags |= OMF_HAS_NULLCHECK;
    }

    if ((earlyPropBlockSummary & BBF_HAS_IDX_LEN) != 0)
    {
        m_pCompiler->optMethodFlags |= OMF_HAS_ARRAYREF;
    }

    if ((earlyPropBlockSummary & BBF_HAS_NEWARRAY) != 0)
    {
        m_pCompiler->optMethodFlags |= OMF_HAS_NEWARRAY;
    }

    // Now handle the final memory state.

    // If the block defines memory, allocate an SSA variable for the final memory state in the block.
    // (This may be redundant with the last SSA var explicitly created, but there's no harm in that.)
    if (block->bbMemoryDef)
    {
        unsigned ssaNum = m_pCompiler->lvMemoryPerSsaData.AllocSsaNum(m_allocator);
        m_renameStack.PushMemory(block, ssaNum);

        if (m_pCompiler->ehBlockHasExnFlowDsc(block))
        {
            AddMemoryDefToHandlerPhis(block, ssaNum);
        }

        block->bbMemorySsaNumOut = ssaNum;
    }
    else
    {
        block->bbMemorySsaNumOut = m_renameStack.TopMemory();
    }

    DBG_SSA_JITDUMP("Ssa # for Memory on entry to " FMT_BB " is %d; on exit is %d.\n", block->bbNum,
                    block->bbMemorySsaNumIn, block->bbMemorySsaNumOut);
}

//------------------------------------------------------------------------
// AddPhiArgsToSuccessors: Add GT_PHI_ARG nodes to the GT_PHI nodes within block's successors.
//
// Arguments:
//    block - The block
//
void SsaBuilder::AddPhiArgsToSuccessors(BasicBlock* block)
{
    for (BasicBlock* succ : block->GetAllSuccs(m_pCompiler))
    {
        // Walk the statements for phi nodes.
        for (Statement* const stmt : succ->Statements())
        {
            // A prefix of the statements of the block are phi definition nodes. If we complete processing
            // that prefix, exit.
            if (!stmt->IsPhiDefnStmt())
            {
                break;
            }

            GenTree*    tree = stmt->GetRootNode();
            GenTreePhi* phi  = tree->gtGetOp2()->AsPhi();

            unsigned lclNum = tree->AsOp()->gtOp1->AsLclVar()->GetLclNum();
            unsigned ssaNum = m_renameStack.Top(lclNum);
            // Search the arglist for an existing definition for ssaNum.
            // (Can we assert that its the head of the list?  This should only happen when we add
            // during renaming for a definition that occurs within a try, and then that's the last
            // value of the var within that basic block.)

            bool found = false;
            for (GenTreePhi::Use& use : phi->Uses())
            {
                if (use.GetNode()->AsPhiArg()->GetSsaNum() == ssaNum)
                {
                    found = true;
                    break;
                }
            }
            if (!found)
            {
                AddPhiArg(succ, stmt, phi, lclNum, ssaNum, block);
            }
        }

        // Now handle memory.
        BasicBlock::MemoryPhiArg*& succMemPhiArg = succ->bbMemorySsaPhiFunc;

        if (succMemPhiArg != nullptr)
        {
            if (succMemPhiArg == &EmptyMemoryPhiDef)
            {
                succMemPhiArg = nullptr;
            }

            // This is a quadratic algorithm. We might need to consider some switch over to a
            // hash table representation for the arguments of a phi node, to make this linear.
            bool found = false;
            for (auto arg = succMemPhiArg; arg != nullptr; arg = arg->GetNext())
            {
                if (arg->m_ssaNum == block->bbMemorySsaNumOut)
                {
                    found = true;
                    break;
                }
            }

            if (!found)
            {
                succMemPhiArg = new (m_pCompiler) BasicBlock::MemoryPhiArg(block->bbMemorySsaNumOut, succMemPhiArg);
            }

            DBG_SSA_JITDUMP("  Added phi arg for Memory u:%d from " FMT_BB " in " FMT_BB ".\n",
                            block->bbMemorySsaNumOut, block->bbNum, succ->bbNum);
        }

        // If "succ" is the first block of a try block (and "block" is not also in that try block)
        // then we must look at the vars that have phi defs in the corresponding handler;
        // the current SSA name for such vars must be included as an argument to that phi.
        if (m_pCompiler->bbIsTryBeg(succ))
        {
            assert(succ->hasTryIndex());
            unsigned tryInd = succ->getTryIndex();

            while (tryInd != EHblkDsc::NO_ENCLOSING_INDEX)
            {
                // Check if the predecessor "block" is within the same try block.
                if (block->hasTryIndex())
                {
                    for (unsigned blockTryInd = block->getTryIndex(); blockTryInd != EHblkDsc::NO_ENCLOSING_INDEX;
                         blockTryInd          = m_pCompiler->ehGetEnclosingTryIndex(blockTryInd))
                    {
                        if (blockTryInd == tryInd)
                        {
                            // It is; don't execute the loop below.
                            tryInd = EHblkDsc::NO_ENCLOSING_INDEX;
                            break;
                        }
                    }

                    // The loop just above found that the predecessor "block" is within the same
                    // try block as "succ."  So we don't need to process this try, or any
                    // further outer try blocks here, since they would also contain both "succ"
                    // and "block".
                    if (tryInd == EHblkDsc::NO_ENCLOSING_INDEX)
                    {
                        break;
                    }
                }

                EHblkDsc* succTry = m_pCompiler->ehGetDsc(tryInd);
                // This is necessarily true on the first iteration, but not
                // necessarily on the second and subsequent.
                if (succTry->ebdTryBeg != succ)
                {
                    break;
                }

                // succ is the first block of this try.  Look at phi defs in the handler.
                // For a filter, we consider the filter to be the "real" handler.
                BasicBlock* handlerStart = succTry->ExFlowBlock();

                for (Statement* const stmt : handlerStart->Statements())
                {
                    GenTree* tree = stmt->GetRootNode();

                    // Check if the first n of the statements are phi nodes. If not, exit.
                    if (tree->OperGet() != GT_ASG || tree->AsOp()->gtOp2 == nullptr ||
                        tree->AsOp()->gtOp2->OperGet() != GT_PHI)
                    {
                        break;
                    }

                    // Get the phi node from GT_ASG.
                    GenTree* lclVar = tree->AsOp()->gtOp1;
                    unsigned lclNum = lclVar->AsLclVar()->GetLclNum();

                    // If the variable is live-out of "blk", and is therefore live on entry to the try-block-start
                    // "succ", then we make sure the current SSA name for the
                    // var is one of the args of the phi node.  If not, go on.
                    LclVarDsc* lclVarDsc = &m_pCompiler->lvaTable[lclNum];
                    if (!lclVarDsc->lvTracked ||
                        !VarSetOps::IsMember(m_pCompiler, block->bbLiveOut, lclVarDsc->lvVarIndex))
                    {
                        continue;
                    }

                    GenTreePhi* phi = tree->gtGetOp2()->AsPhi();

                    unsigned ssaNum = m_renameStack.Top(lclNum);

                    // See if this ssaNum is already an arg to the phi.
                    bool alreadyArg = false;
                    for (GenTreePhi::Use& use : phi->Uses())
                    {
                        if (use.GetNode()->AsPhiArg()->GetSsaNum() == ssaNum)
                        {
                            alreadyArg = true;
                            break;
                        }
                    }
                    if (!alreadyArg)
                    {
                        AddPhiArg(handlerStart, stmt, phi, lclNum, ssaNum, block);
                    }
                }

                // Now handle memory.
                BasicBlock::MemoryPhiArg*& memPhiArg = handlerStart->bbMemorySsaPhiFunc;

                if (memPhiArg != nullptr)
                {
                    if (memPhiArg == &EmptyMemoryPhiDef)
                    {
                        memPhiArg = nullptr;
                    }

                    // This path has a potential to introduce redundant phi args, due to multiple
                    // preds of the same try-begin block having the same live-out memory def, and/or
                    // due to nested try-begins each having preds with the same live-out memory def.
                    // Avoid doing quadratic processing on handler phis, and instead live with the
                    // occasional redundancy.
                    memPhiArg = new (m_pCompiler) BasicBlock::MemoryPhiArg(block->bbMemorySsaNumOut, memPhiArg);

                    DBG_SSA_JITDUMP("  Added phi arg for Memory u:%d from " FMT_BB " in " FMT_BB ".\n",
                                    block->bbMemorySsaNumOut, block->bbNum, handlerStart->bbNum);
                }

                tryInd = succTry->ebdEnclosingTryIndex;
            }
        }
    }
}

//------------------------------------------------------------------------
// RenameVariables: Rename all definitions and uses within the compiled method.
//
// Notes:
//    See Briggs, Cooper, Harvey and Simpson "Practical Improvements to the Construction
//    and Destruction of Static Single Assignment Form."
//
void SsaBuilder::RenameVariables()
{
    JITDUMP("*************** In SsaBuilder::RenameVariables()\n");

    // The first thing we do is treat parameters and must-init variables as if they have a
    // virtual definition before entry -- they start out at SSA name 1.
    for (unsigned lclNum = 0; lclNum < m_pCompiler->lvaCount; lclNum++)
    {
        LclVarDsc* lcl = m_pCompiler->lvaGetDesc(lclNum);

        if (lcl->IsInSsa() &&
            VarSetOps::IsMember(m_pCompiler, m_pCompiler->fgFirstBB->bbLiveIn, lcl->GetLivenessBitIndex()))
        {
            unsigned ssaNum = lcl->lvPerSsaData.AllocSsaNum(m_allocator);
            // HasImplicitSsaDef assumes that this is always the first SSA def.
            assert(ssaNum == SsaConfig::FIRST_SSA_NUM);
            m_renameStack.Push(m_pCompiler->fgFirstBB, lclNum, ssaNum);
        }
    }

    // In ValueNum we'd assume un-inited memory gets FIRST_SSA_NUM.
    // The memory is a parameter.  Use FIRST_SSA_NUM as first SSA name.
    unsigned initMemorySsaNum = m_pCompiler->lvMemoryPerSsaData.AllocSsaNum(m_allocator);
    assert(initMemorySsaNum == SsaConfig::FIRST_SSA_NUM);

    m_renameStack.PushMemory(m_pCompiler->fgFirstBB, initMemorySsaNum);

    // Initialize the memory ssa numbers for unreachable blocks. ValueNum expects
    // memory ssa numbers to have some intitial value.
    for (BasicBlock* const block : m_pCompiler->Blocks())
    {
        if (block->bbIDom == nullptr)
        {
            block->bbMemorySsaNumIn  = initMemorySsaNum;
            block->bbMemorySsaNumOut = initMemorySsaNum;
        }
    }

    class SsaRenameDomTreeVisitor : public DomTreeVisitor<SsaRenameDomTreeVisitor>
    {
        SsaBuilder*     m_builder;
        SsaRenameState* m_renameStack;
        BasicBlockFlags m_earlyPropBlockSummary;

    public:
        SsaRenameDomTreeVisitor(Compiler* compiler, SsaBuilder* builder, SsaRenameState* renameStack)
            : DomTreeVisitor(compiler, compiler->fgSsaDomTree), m_builder(builder), m_renameStack(renameStack)
        {
        }

        void Begin()
        {
            m_compiler->optMethodFlags &= ~(OMF_HAS_ARRAYREF | OMF_HAS_NEWARRAY | OMF_HAS_NULLCHECK);
        }

        void PreOrderVisit(BasicBlock* block)
        {
            // TODO-Cleanup: Move these functions from SsaBuilder to this class.
            m_builder->BlockRenameVariables(block);
            m_builder->AddPhiArgsToSuccessors(block);
        }

        void PostOrderVisit(BasicBlock* block)
        {
            m_renameStack->PopBlockStacks(block);
        }
    };

    SsaRenameDomTreeVisitor visitor(m_pCompiler, this, &m_renameStack);
    visitor.WalkTree();
}

#ifdef DEBUG
/**
 * Print the blocks, the phi nodes get printed as well.
 * @example:
 * After SSA BB02:
 *                [0027CC0C] -----------                 stmtExpr  void  (IL 0x019...0x01B)
 * N001 (  1,  1)       [0027CB70] -----------                 const     int    23
 * N003 (  3,  3)    [0027CBD8] -A------R--                 =         int
 * N002 (  1,  1)       [0027CBA4] D------N---                 lclVar    int    V01 arg1         d:5
 *
 * After SSA BB04:
 *                [0027D530] -----------                 stmtExpr  void  (IL   ???...  ???)
 * N002 (  0,  0)       [0027D4C8] -----------                 phi       int
 *                            [0027D8CC] -----------                 lclVar    int    V01 arg1         u:5
 *                            [0027D844] -----------                 lclVar    int    V01 arg1         u:4
 * N004 (  2,  2)    [0027D4FC] -A------R--                 =         int
 * N003 (  1,  1)       [0027D460] D------N---                 lclVar    int    V01 arg1         d:3
 */
void SsaBuilder::Print(BasicBlock** postOrder, int count)
{
    for (int i = count - 1; i >= 0; --i)
    {
        printf("After SSA " FMT_BB ":\n", postOrder[i]->bbNum);
        m_pCompiler->gtDispBlockStmts(postOrder[i]);
    }
}
#endif // DEBUG

/**
 * Build SSA form.
 *
 * Sorts the graph topologically.
 *   - Collects them in postOrder array.
 *
 * Identifies each block's immediate dominator.
 *   - Computes this in bbIDom of each BasicBlock.
 *
 * Computes DOM tree relation.
 *   - Computes domTree as block -> set of blocks.
 *   - Computes pre/post order traversal of the DOM tree.
 *
 * Inserts phi nodes.
 *   - Computes dominance frontier as block -> set of blocks.
 *   - Allocates block use/def/livein/liveout and computes it.
 *   - Inserts phi nodes with only rhs at the beginning of the blocks.
 *
 * Renames variables.
 *   - Walks blocks in evaluation order and gives uses and defs names.
 *   - Gives empty phi nodes their rhs arguments as they become known while renaming.
 *
 * @return true if successful, for now, this must always be true.
 *
 * @see "A simple, fast dominance algorithm" by Keith D. Cooper, Timothy J. Harvey, Ken Kennedy.
 * @see Briggs, Cooper, Harvey and Simpson "Practical Improvements to the Construction
 *      and Destruction of Static Single Assignment Form."
 */
void SsaBuilder::Build()
{
#ifdef DEBUG
    if (m_pCompiler->verbose)
    {
        printf("*************** In SsaBuilder::Build()\n");
    }
#endif

    // Ensure that there's a first block outside a try, so that the dominator tree has a unique root.
    SetupBBRoot();

    // Just to keep block no. & index same add 1.
    int blockCount = m_pCompiler->fgBBNumMax + 1;

    JITDUMP("[SsaBuilder] Max block count is %d.\n", blockCount);

    // Allocate the postOrder array for the graph.

    BasicBlock** postOrder;

    if (blockCount > DEFAULT_MIN_OPTS_BB_COUNT)
    {
        postOrder = new (m_allocator) BasicBlock*[blockCount];
    }
    else
    {
        postOrder = (BasicBlock**)alloca(blockCount * sizeof(BasicBlock*));
    }

    m_visitedTraits = BitVecTraits(blockCount, m_pCompiler);
    m_visited       = BitVecOps::MakeEmpty(&m_visitedTraits);

    // TODO-Cleanup: We currently have two dominance computations happening.  We should unify them; for
    // now, at least forget the results of the first. Note that this does not clear fgDomTreePreOrder
    // and fgDomTreePostOrder nor does the subsequent code call fgNumberDomTree once the new dominator
    // tree is built. The pre/post order numbers that were generated previously and used for loop
    // recognition are still being used by optPerformHoistExpr via fgCreateLoopPreHeader. That's rather
    // odd, considering that SetupBBRoot may have added a new block.
    for (BasicBlock* const block : m_pCompiler->Blocks())
    {
        block->bbIDom         = nullptr;
        block->bbPostOrderNum = 0;
    }

    // Topologically sort the graph.
    int count = TopologicalSort(postOrder, blockCount);
    JITDUMP("[SsaBuilder] Topologically sorted the graph.\n");
    EndPhase(PHASE_BUILD_SSA_TOPOSORT);

    // Compute IDom(b).
    ComputeImmediateDom(postOrder, count);

    m_pCompiler->fgSsaDomTree = m_pCompiler->fgBuildDomTree();
    EndPhase(PHASE_BUILD_SSA_DOMS);

    // Compute liveness on the graph.
    DBEXEC(m_pCompiler->verbose, m_pCompiler->lvaTableDump());
    m_pCompiler->lvaMarkLivenessTrackedLocals();
    m_pCompiler->fgLocalVarLiveness();
    EndPhase(PHASE_BUILD_SSA_LIVENESS);
    DBEXEC(m_pCompiler->verbose, m_pCompiler->lvaTableDump());

    m_pCompiler->optRemoveRedundantZeroInits();
    EndPhase(PHASE_ZERO_INITS);

    // Mark all variables that will be tracked by SSA
    for (unsigned lclNum = 0; lclNum < m_pCompiler->lvaCount; lclNum++)
    {
        m_pCompiler->lvaTable[lclNum].lvInSsa = IncludeInSsa(lclNum);
    }

    // Insert phi functions.
    InsertPhiFunctions(postOrder, count);

    // Rename local variables and collect UD information for each ssa var.
    RenameVariables();
    EndPhase(PHASE_BUILD_SSA_RENAME);

#ifdef DEBUG
    // At this point we are in SSA form. Print the SSA form.
    if (m_pCompiler->verboseSsa)
    {
        Print(postOrder, count);
    }
#endif
}

void SsaBuilder::SetupBBRoot()
{
    // Allocate a bbroot, if necessary.
    // We need a unique block to be the root of the dominator tree.
    // This can be violated if the first block is in a try, or if it is the first block of
    // a loop (which would necessarily be an infinite loop) -- i.e., it has a predecessor.

    // If neither condition holds, no reason to make a new block.
    if (!m_pCompiler->fgFirstBB->hasTryIndex() && m_pCompiler->fgFirstBB->bbPreds == nullptr)
    {
        return;
    }

    BasicBlock* bbRoot = m_pCompiler->bbNewBasicBlock(BBJ_NONE);
    bbRoot->bbFlags |= BBF_INTERNAL;

    // May need to fix up preds list, so remember the old first block.
    BasicBlock* oldFirst = m_pCompiler->fgFirstBB;

    // Copy the liveness information from the first basic block.
    if (m_pCompiler->fgLocalVarLivenessDone)
    {
        VarSetOps::Assign(m_pCompiler, bbRoot->bbLiveIn, oldFirst->bbLiveIn);
        VarSetOps::Assign(m_pCompiler, bbRoot->bbLiveOut, oldFirst->bbLiveIn);
    }

    // Copy the bbWeight.  (This is technically wrong, if the first block is a loop head, but
    // it shouldn't matter...)
    bbRoot->inheritWeight(oldFirst);

    // There's an artifical incoming reference count for the first BB.  We're about to make it no longer
    // the first BB, so decrement that.
    assert(oldFirst->bbRefs > 0);
    oldFirst->bbRefs--;

    m_pCompiler->fgInsertBBbefore(m_pCompiler->fgFirstBB, bbRoot);

    assert(m_pCompiler->fgFirstBB == bbRoot);
    if (m_pCompiler->fgComputePredsDone)
    {
        m_pCompiler->fgAddRefPred(oldFirst, bbRoot);
    }
}

//------------------------------------------------------------------------
// IncludeInSsa: Check if the specified variable can be included in SSA.
//
// Arguments:
//    lclNum - the variable number
//
// Return Value:
//    true if the variable is included in SSA
//
bool SsaBuilder::IncludeInSsa(unsigned lclNum)
{
    LclVarDsc* lcl = m_pCompiler->lvaGetDesc(lclNum);

    if (!lcl->HasLiveness())
    {
        return false;
    }

    assert(!lcl->IsAddressExposed());
    assert(!lcl->IsPromoted());

    if (lcl->lvOverlappingFields)
    {
        return false;
    }

    if (lcl->IsPromotedField())
    {
        LclVarDsc* parentLcl = m_pCompiler->lvaGetDesc(lcl->GetPromotedFieldParentLclNum());

        if (parentLcl->IsDependentPromoted() || parentLcl->lvIsMultiRegRet)
        {
            // SSA must exclude struct fields that are not independent:
            // - we don't model the struct assignment properly when multiple fields
            //   can be assigned by one struct assignment.
            // - SSA doesn't allow a single node to contain multiple SSA definitions.
            // - dependent promoted fields are never candidates for a register.
            return false;
        }
    }

    return true;
}
