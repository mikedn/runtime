// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "ssarenamestate.h"

class SsaBuilder
{
    Compiler*      m_pCompiler;
    CompAllocator  m_allocator;
    BitVecTraits   m_visitedTraits;
    BitVec         m_visited;
    SsaRenameState m_renameStack;

    using BlockVector = jitstd::vector<BasicBlock*>;
    using BlockDFMap  = JitHashTable<BasicBlock*, JitPtrKeyFuncs<BasicBlock>, BlockVector>;

public:
    SsaBuilder(Compiler* compiler);

    void Build();

private:
    void SetupBBRoot();
    bool IncludeInSsa(unsigned lclNum);
    int TopologicalSort(BasicBlock** postOrder, int count);
    static BasicBlock* IntersectDom(BasicBlock* finger1, BasicBlock* finger2);
    void ComputeImmediateDom(BasicBlock** postOrder, int count);
    void ComputeDominanceFrontiers(BasicBlock** postOrder, int count, BlockDFMap* mapDF);
    void ComputeIteratedDominanceFrontier(BasicBlock* b, const BlockDFMap* mapDF, BlockVector* bIDF);

    void InsertPhiFunctions(BasicBlock** postOrder, int count, const BlockDFMap& mapDF);
    static GenTreeSsaPhi* GetPhiNode(BasicBlock* block, unsigned lclNum);
    void InsertPhi(BasicBlock* block, unsigned lclNum);
    void AddPhiArg(
        BasicBlock* block, Statement* stmt, GenTreeSsaPhi* phi, unsigned lclNum, GenTreeSsaDef* def, BasicBlock* pred);

    void RenameVariables();
    void BlockRenameVariables(BasicBlock* block);
    void RenameDef(GenTreeOp* asgNode, BasicBlock* block);
    void RenamePhiDef(GenTreeSsaDef* def, BasicBlock* block);
    void RenameLclUse(GenTreeLclVarCommon* lclNode, Statement* stmt, BasicBlock* block);

    void AddDefToHandlerPhis(BasicBlock* block, unsigned lclNum, GenTreeSsaDef* def);
    void AddMemoryDefToHandlerPhis(BasicBlock* block, unsigned ssaNum);
    void AddPhiArgsToSuccessors(BasicBlock* block);

    INDEBUG(void Print(BasicBlock** postOrder, int count);)
};

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

#ifdef OPT_CONFIG

void Compiler::fgSsaReset()
{
    assert(opts.optRepeat);
    assert(JitConfig.JitOptRepeatCount() > 0);

    fgDomsComputed = false;
    optLoopCount   = 0;
    optLoopTable   = nullptr;
    INDEBUG(fgLocalVarLivenessDone = false;)
    ssaForm          = false;
    vnStore          = nullptr;
    vnLoopTable      = nullptr;
    apAssertionCount = 0;
    apAssertionTable = nullptr;

    for (unsigned i = 0; i < lvaCount; ++i)
    {
        lvaTable[i].ClearSsa();
    }

    lvMemoryPerSsaData.Reset();
    m_memorySsaMap = nullptr;

    for (BasicBlock* block : Blocks())
    {
        block->bbFlags &= ~BBF_LOOP_FLAGS;
        block->bbNatLoopNum       = BasicBlock::NOT_IN_LOOP;
        block->bbPredsWithEH      = nullptr;
        block->bbMemorySsaPhiFunc = nullptr;
        block->bbMemorySsaNumIn   = SsaConfig::RESERVED_SSA_NUM;
        block->bbMemorySsaNumOut  = SsaConfig::RESERVED_SSA_NUM;

        Statement* first = block->FirstNonPhiDef();

        if (first == nullptr)
        {
            block->bbStmtList = nullptr;
            continue;
        }

        Statement* last = block->GetLastStatement();
        INDEBUG(first->SetPrevStmt(nullptr);)
        block->SetStatements(first, last);

        for (Statement* stmt : block->Statements())
        {
            for (GenTree* node : stmt->Nodes())
            {
                node->SetVNs({NoVN, NoVN});

                if (node->OperIs(GT_LCL_VAR, GT_LCL_FLD))
                {
                    node->gtFlags &= ~GTF_VAR_FIELD_DEATH_MASK;
                }
            }
        }
    }

    fgComputeReachability();
    fgComputeDoms();
    optFindLoops();
}

#endif // OPT_CONFIG

SsaBuilder::SsaBuilder(Compiler* pCompiler)
    : m_pCompiler(pCompiler)
    , m_allocator(pCompiler->getAllocator(CMK_SSA))
    , m_visitedTraits(0, pCompiler) // at this point we do not know the size, SetupBBRoot can add a block
    , m_renameStack(m_allocator, pCompiler->lvaCount)
{
}

void SsaBuilder::Build()
{
    JITDUMP("*************** In SsaBuilder::Build()\n");

    SetupBBRoot();

    int blockCount = m_pCompiler->fgBBNumMax + 1;

    JITDUMP("[SsaBuilder] Max block count is %d.\n", blockCount);

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

    int count = TopologicalSort(postOrder, blockCount);
    m_pCompiler->EndPhase(PHASE_BUILD_SSA_TOPOSORT);

    ComputeImmediateDom(postOrder, count);

    m_pCompiler->fgSsaDomTree = m_pCompiler->fgBuildDomTree();
    m_pCompiler->EndPhase(PHASE_BUILD_SSA_DOMS);

    DBEXEC(m_pCompiler->verbose, m_pCompiler->lvaTableDump());
    m_pCompiler->lvaMarkLivenessTrackedLocals();
    m_pCompiler->fgLocalVarLiveness();
    m_pCompiler->EndPhase(PHASE_BUILD_SSA_LIVENESS);
    DBEXEC(m_pCompiler->verbose, m_pCompiler->lvaTableDump());

    m_pCompiler->optRemoveRedundantZeroInits();
    m_pCompiler->EndPhase(PHASE_ZERO_INITS);

    for (unsigned lclNum = 0; lclNum < m_pCompiler->lvaCount; lclNum++)
    {
        m_pCompiler->lvaGetDesc(lclNum)->m_isSsa = IncludeInSsa(lclNum);
    }

    BlockDFMap mapDF(m_allocator);
    ComputeDominanceFrontiers(postOrder, count, &mapDF);
    m_pCompiler->EndPhase(PHASE_BUILD_SSA_DF);
    InsertPhiFunctions(postOrder, count, mapDF);
    m_pCompiler->EndPhase(PHASE_BUILD_SSA_INSERT_PHIS);
    RenameVariables();
    m_pCompiler->EndPhase(PHASE_BUILD_SSA_RENAME);

    DBEXEC(m_pCompiler->verboseSsa, Print(postOrder, count))
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

    assert(!m_pCompiler->fgLocalVarLivenessDone);

    // Copy the bbWeight.  (This is technically wrong, if the first block is a loop head, but
    // it shouldn't matter...)
    bbRoot->inheritWeight(oldFirst);

    // There's an artifical incoming reference count for the first BB.  We're about to make it no longer
    // the first BB, so decrement that.
    assert(oldFirst->bbRefs > 0);
    oldFirst->bbRefs--;

    m_pCompiler->fgInsertBBbefore(m_pCompiler->fgFirstBB, bbRoot);
    assert(m_pCompiler->fgFirstBB == bbRoot);
    m_pCompiler->fgAddRefPred(oldFirst, bbRoot);
}

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

    JITDUMP("[SsaBuilder] Topologically sorted the graph.\n");

    return postIndex;
}

// Method that finds a common IDom parent, much like least common ancestor.
// See "A simple, fast dominance algorithm" by Keith D. Cooper, Timothy J. Harvey, Ken Kennedy.
//
// Returns a basic block whose IDom is the dominator for finger1 and finger2, or else NULL.
// This may be called while immediate dominators are being computed, and if the input values
// are members of the same loop (each reachable from the other), then one may not yet have
// its immediate dominator computed when we are attempting to find the immediate dominator
// of the other. So a NULL return value means that the the two inputs are in a cycle, not
// that they don't have a common dominator ancestor.
BasicBlock* SsaBuilder::IntersectDom(BasicBlock* finger1, BasicBlock* finger2)
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

void SsaBuilder::ComputeDominanceFrontiers(BasicBlock** postOrder, int count, BlockDFMap* mapDF)
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

                BlockVector& b1DF = *mapDF->Emplace(b1, m_allocator);
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

            BlockVector* bDF = mapDF->LookupPointer(b);
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

void SsaBuilder::ComputeIteratedDominanceFrontier(BasicBlock* b, const BlockDFMap* mapDF, BlockVector* bIDF)
{
    assert(bIDF->empty());

    BlockVector* bDF = mapDF->LookupPointer(b);

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
            BasicBlock*  f   = (*bIDF)[newIndex];
            BlockVector* fDF = mapDF->LookupPointer(f);

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

GenTreeSsaPhi* SsaBuilder::GetPhiNode(BasicBlock* block, unsigned lclNum)
{
    for (Statement* const stmt : block->Statements())
    {
        GenTree* tree = stmt->GetRootNode();

        if (!tree->IsSsaPhiDef())
        {
            break;
        }

        if (tree->AsSsaDef()->GetLclNum() == lclNum)
        {
            return tree->AsSsaDef()->GetValue()->AsSsaPhi();
        }
    }

    return nullptr;
}

void SsaBuilder::InsertPhi(BasicBlock* block, unsigned lclNum)
{
    var_types type = m_pCompiler->lvaGetDesc(lclNum)->GetType();

    GenTreeSsaPhi* phi = new (m_pCompiler, GT_SSA_PHI) GenTreeSsaPhi(type);
    phi->SetCosts(0, 0);

    GenTreeSsaDef* def = new (m_pCompiler, GT_SSA_DEF) GenTreeSsaDef(phi, block, lclNum, SsaConfig::RESERVED_SSA_NUM);
    def->gtFlags       = GTF_VAR_DEF | GTF_ASG;
    def->SetCosts(0, 0);

    Statement* stmt = m_pCompiler->gtNewStmt(def);
    stmt->SetTreeList(phi);
    phi->gtNext = def;
    def->gtPrev = phi;

#ifdef DEBUG
    unsigned seqNum = 1;
    for (GenTree* const node : stmt->Nodes())
    {
        node->gtSeqNum = seqNum++;
    }
#endif // DEBUG

    m_pCompiler->fgInsertStmtAtBeg(block, stmt);

    JITDUMP("Added PHI definition for V%02u at start of " FMT_BB ".\n", lclNum, block->bbNum);
}

void SsaBuilder::AddPhiArg(
    BasicBlock* block, Statement* stmt, GenTreeSsaPhi* phi, unsigned lclNum, GenTreeSsaDef* def, BasicBlock* pred)
{
#ifdef DEBUG
    // Make sure it isn't already present: we should only add each definition once.
    for (GenTreeSsaPhi::Use& use : phi->Uses())
    {
        assert(use.GetNode()->GetDef() != def);
    }
#endif // DEBUG

    GenTreeSsaUse* use = new (m_pCompiler, GT_SSA_USE) GenTreeSsaUse(def, pred);
    // We need to keep PHI args (e.g. we can't propagate a constant to a PHI arg).
    // TODO-MIKE-SSA: This may be unreliable, only some transforms check GTF_DONT_CSE.
    use->gtFlags |= GTF_DONT_CSE;
    // Costs are not relevant for PHI args.
    use->SetCosts(0, 0);
    // The argument order doesn't matter so just insert at the front of the list because
    // it's easier. It's also easier to insert in linear order since the first argument
    // will be first in linear order as well.
    phi->m_uses = new (m_pCompiler, CMK_ASTNode) GenTreeSsaPhi::Use(use, phi->m_uses);

    GenTree* head = stmt->GetTreeList();
    assert(head->OperIs(GT_SSA_PHI, GT_SSA_USE));
    stmt->SetTreeList(use);
    use->gtNext  = head;
    head->gtPrev = use;

#ifdef DEBUG
    unsigned seqNum = 1;
    for (GenTree* const node : stmt->TreeList())
    {
        node->gtSeqNum = seqNum++;
    }
#endif // DEBUG

    DBG_SSA_JITDUMP("Added PHI arg u:%d for V%02u from " FMT_BB " in " FMT_BB ".\n", def->GetSsaNum(), lclNum,
                    pred->bbNum, block->bbNum);
}

// Special value to represent a to-be-filled in Memory Phi arg list.
static BasicBlock::MemoryPhiArg EmptyMemoryPhiDef(0, nullptr);

void SsaBuilder::InsertPhiFunctions(BasicBlock** postOrder, int count, const BlockDFMap& mapDF)
{
    JITDUMP("*************** In SsaBuilder::InsertPhiFunctions()\n");

    // Use the same IDF vector for all blocks to avoid unnecessary memory allocations
    BlockVector blockIDF(m_allocator);

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
        for (VarSetOps::Enumerator en(m_pCompiler, block->bbVarDef); en.MoveNext();)
        {
            unsigned   lclNum = m_pCompiler->lvaTrackedIndexToLclNum(en.Current());
            LclVarDsc* lcl    = m_pCompiler->lvaGetDesc(lclNum);
            DBG_SSA_JITDUMP("  Considering local var V%02u:\n", lclNum);

            if (!lcl->IsSsa())
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
                if (!VarSetOps::IsMember(m_pCompiler, bbInDomFront->bbLiveIn, en.Current()))
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
}

void SsaBuilder::RenameDef(GenTreeOp* asgNode, BasicBlock* block)
{
    assert(asgNode->OperIs(GT_ASG));

    GenTree*             dst     = asgNode->GetOp(0);
    GenTreeLclVarCommon* lclNode = nullptr;

    if (dst->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        lclNode           = dst->AsLclVarCommon();
        unsigned   lclNum = lclNode->GetLclNum();
        LclVarDsc* lcl    = m_pCompiler->lvaGetDesc(lclNum);

        if (lcl->IsSsa())
        {
            // Promoted variables are not in SSA, only their fields are.
            assert(!lcl->IsPromoted());
            // If it's a SSA local then it cannot be address exposed and thus does not define SSA memory.
            assert(!lcl->IsAddressExposed());
            // This should have been marked as defintion.
            assert((lclNode->gtFlags & GTF_VAR_DEF) != 0);

            GenTreeFlags defFlags = lclNode->gtFlags & ~GTF_DONT_CSE;

            if (lclNode->gtPrev != nullptr)
            {
                lclNode->gtPrev->gtNext = lclNode->gtNext;
            }

            if (lclNode->gtNext != nullptr)
            {
                lclNode->gtNext->gtPrev = lclNode->gtPrev;
            }

            GenTree* value  = asgNode->GetOp(1);
            unsigned ssaNum = lcl->AllocSsaNum(m_allocator);

            if (GenTreeLclFld* lclFld = lclNode->IsLclFld())
            {
                GenTree* structValue;

                if (lclFld->IsPartialLclFld(m_pCompiler))
                {
                    assert((lclFld->gtFlags & GTF_VAR_USEASG) != 0);

                    structValue = new (m_pCompiler, GT_SSA_USE) GenTreeSsaUse(m_renameStack.Top(lclNum), block);
                }
                else if (lcl->TypeIs(TYP_STRUCT))
                {
                    assert((lclFld->gtFlags & GTF_VAR_USEASG) == 0);

                    // TODO-MIKE-SSA: This leaves us with an STRUCT INSERT from which we
                    // cannot recover the struct layout. In VN we might get away with it
                    // because it will simply insert the field value using the field seq
                    // into a zero map, and then cast the struct to the layout of the def.
                    // We won't be able to CSE the INSERT but we should not need that.
                    structValue = m_pCompiler->gtNewZeroConNode(TYP_INT);
                }
                else
                {
                    assert((lclFld->gtFlags & GTF_VAR_USEASG) == 0);

                    // TODO-MIKE-SSA: Using INSERT/EXTRACT with non-STRUCT types is dubious,
                    // 64 bit BITCASTs on 32 bit targets and shift/bitwise ops for oddities
                    // like (((byte*)&int_local) + 1) = 42; should avoid this this mess.
                    structValue = m_pCompiler->gtNewZeroConNode(lcl->GetType());
                }

                structValue->SetCosts(0, 0);

                unsigned      fieldTypeNum = varTypeToTypeNum(lclFld->GetType(), lclFld->GetLayoutNum());
                unsigned      fieldOffs    = lclFld->GetLclOffs();
                FieldSeqNode* fieldSeq     = lclFld->GetFieldSeq();

                GenTree* insert = lclFld;
                insert->SetOper(GT_INSERT);
                insert->SetType(lcl->GetType());
                insert->AsInsert()->SetStructValue(structValue);
                insert->AsInsert()->SetFieldValue(value);
                insert->AsInsert()->SetField(fieldTypeNum, fieldOffs, fieldSeq);
                insert->gtFlags = GTF_DONT_CSE;
                insert->SetSideEffects(value->GetSideEffects());

                // TODO-MIKE-SSA: Pff, manual node linking sucks.

                asgNode->gtPrev->gtNext = structValue;
                structValue->gtPrev     = asgNode->gtPrev;
                structValue->gtNext     = insert;
                insert->gtPrev          = structValue;
                insert->gtNext          = asgNode;
                asgNode->gtPrev         = insert;

                value = insert;
            }
            else
            {
                assert((lclNode->gtFlags & GTF_VAR_USEASG) == 0);
            }

            GenTree* def = asgNode;
            def->SetOper(GT_SSA_DEF);
            def->AsSsaDef()->Init();
            def->AsSsaDef()->SetLclNum(lclNum);
            def->AsSsaDef()->SetSsaNum(ssaNum);
            def->AsSsaDef()->SetBlock(block);
            def->AsSsaDef()->SetValue(value);
            def->SetType(lcl->lvNormalizeOnStore() ? varActualType(lcl->GetType()) : lcl->GetType());
            def->gtFlags = defFlags | value->GetSideEffects() | GTF_ASG;

            m_renameStack.Push(block, lclNum, def->AsSsaDef());

            // If necessary, add "lclNum/ssaNum" to the arg list of a phi def in any
            // handlers for try blocks that "block" is within.  (But only do this for "real" definitions,
            // not phi definitions.)
            if (!value->IsSsaPhi())
            {
                AddDefToHandlerPhis(block, lclNum, def->AsSsaDef());
            }

            return;
        }

        if (!lcl->IsAddressExposed())
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
                if (GenTreeLclAddr* lclAddr = indir->GetAddr()->IsLocalAddrExpr())
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

void SsaBuilder::RenamePhiDef(GenTreeSsaDef* def, BasicBlock* block)
{
    unsigned   lclNum = def->GetLclNum();
    LclVarDsc* lcl    = m_pCompiler->lvaGetDesc(lclNum);

    def->SetSsaNum(lcl->AllocSsaNum(m_allocator));
    m_renameStack.Push(block, lclNum, def);
}

void SsaBuilder::RenameLclUse(GenTreeLclVarCommon* lclNode, Statement* stmt, BasicBlock* block)
{
    assert(lclNode->OperIs(GT_LCL_VAR, GT_LCL_FLD));
    assert((lclNode->gtFlags & GTF_VAR_DEF) == 0);

    unsigned   lclNum = lclNode->GetLclNum();
    LclVarDsc* lcl    = m_pCompiler->lvaGetDesc(lclNum);

    if (!lcl->IsSsa())
    {
        return;
    }

    // Promoted variables are not in SSA, only their fields are.
    assert(!lcl->IsPromoted());

    GenTreeSsaDef* def = m_renameStack.Top(lclNum);

    if (GenTreeLclFld* lclFld = lclNode->IsLclFld())
    {
        GenTreeSsaUse* use = new (m_pCompiler, GT_SSA_USE) GenTreeSsaUse(def, block);
        use->SetCosts(0, 0);
        use->gtFlags |= lclNode->gtFlags & GTF_VAR_DEATH;

        unsigned      fieldOffset = lclFld->GetLclOffs();
        FieldSeqNode* fieldSeq    = lclFld->GetFieldSeq();
        unsigned      fieldLayoutNum =
            lclFld->TypeIs(TYP_STRUCT) ? lclFld->GetLayoutNum() : static_cast<unsigned>(lclFld->GetType());

        lclNode->ChangeOper(GT_EXTRACT);
        GenTreeExtract* extract = lclNode->AsExtract();

        extract->SetField(fieldLayoutNum, fieldOffset, fieldSeq);
        extract->SetStructValue(use);

        // Block constant propagation when EXTRACT is used as a form of reinterpretation,
        // we risk ending up with FP constants that fgSsaDestroy doesn't know how to handle.
        if (!use->TypeIs(TYP_STRUCT))
        {
            use->gtFlags |= GTF_DONT_CSE;
        }

        if (extract->gtPrev == nullptr)
        {
            assert(stmt->GetTreeList() == extract);

            stmt->SetTreeList(use);
            use->gtNext     = extract;
            extract->gtPrev = use;
        }
        else
        {
            extract->gtPrev->gtNext = use;
            use->gtPrev             = extract->gtPrev;
            use->gtNext             = extract;
            extract->gtPrev         = use;
        }
    }
    else
    {
        lclNode->SetOper(GT_SSA_USE);
        lclNode->AsSsaUse()->Init();
        lclNode->AsSsaUse()->SetBlock(block);

        def->AddUse(lclNode->AsSsaUse());
    }
}

void SsaBuilder::AddDefToHandlerPhis(BasicBlock* block, unsigned lclNum, GenTreeSsaDef* def)
{
    assert(m_pCompiler->lvaTable[lclNum].lvTracked); // Precondition.
    unsigned lclIndex = m_pCompiler->lvaTable[lclNum].lvVarIndex;

    EHblkDsc* tryBlk = m_pCompiler->ehGetBlockExnFlowDsc(block);
    if (tryBlk != nullptr)
    {
        DBG_SSA_JITDUMP("Definition of local V%02u/d:%d in block " FMT_BB
                        " has exn handler; adding as phi arg to handlers.\n",
                        lclNum, def->GetSsaNum(), block->bbNum);
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
                    GenTree* tree = stmt->GetRootNode();

                    // If the tree is not an SSA def, break out of the loop: we're done.
                    if (!tree->IsSsaPhiDef())
                    {
                        break;
                    }

                    if (tree->AsSsaDef()->GetLclNum() == lclNum)
                    {
                        // It's the definition for the right local.  Add "ssaNum" to the RHS.
                        AddPhiArg(handler, stmt, tree->AsSsaDef()->GetValue()->AsSsaPhi(), lclNum, def, block);
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
            else if (tree->OperIs(GT_SSA_DEF))
            {
                RenamePhiDef(tree->AsSsaDef(), block);
            }
            // PHI_ARG nodes already have SSA numbers so we only need to check LCL_VAR and LCL_FLD nodes.
            else if (tree->OperIs(GT_LCL_VAR, GT_LCL_FLD))
            {
                if ((tree->gtFlags & GTF_VAR_DEF) == 0)
                {
                    RenameLclUse(tree->AsLclVarCommon(), stmt, block);
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

#ifdef DEBUG
        unsigned seqNum = 0;
        for (GenTree* node : stmt->Nodes())
        {
            node->gtSeqNum = ++seqNum;
        }
#endif
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

void SsaBuilder::AddPhiArgsToSuccessors(BasicBlock* block)
{
    for (BasicBlock* succ : block->GetAllSuccs(m_pCompiler))
    {
        // Walk the statements for phi nodes.
        for (Statement* const stmt : succ->Statements())
        {
            GenTree* tree = stmt->GetRootNode();

            // A prefix of the statements of the block are phi definition nodes. If we complete processing
            // that prefix, exit.
            if (!tree->IsSsaPhiDef())
            {
                break;
            }

            GenTreeSsaPhi* phi = tree->AsSsaDef()->GetValue()->AsSsaPhi();

            unsigned       lclNum = tree->AsSsaDef()->GetLclNum();
            GenTreeSsaDef* def    = m_renameStack.Top(lclNum);
            // Search the arglist for an existing definition for ssaNum.
            // (Can we assert that its the head of the list?  This should only happen when we add
            // during renaming for a definition that occurs within a try, and then that's the last
            // value of the var within that basic block.)

            bool found = false;
            for (GenTreeSsaPhi::Use& use : phi->Uses())
            {
                if (use.GetNode()->GetDef() == def)
                {
                    found = true;
                    break;
                }
            }
            if (!found)
            {
                AddPhiArg(succ, stmt, phi, lclNum, def, block);
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
                    if (!tree->IsSsaPhiDef())
                    {
                        break;
                    }

                    unsigned lclNum = tree->AsSsaDef()->GetLclNum();

                    // If the variable is live-out of "blk", and is therefore live on entry to the try-block-start
                    // "succ", then we make sure the current SSA name for the
                    // var is one of the args of the phi node.  If not, go on.
                    LclVarDsc* lclVarDsc = &m_pCompiler->lvaTable[lclNum];
                    if (!lclVarDsc->lvTracked ||
                        !VarSetOps::IsMember(m_pCompiler, block->bbLiveOut, lclVarDsc->lvVarIndex))
                    {
                        continue;
                    }

                    GenTreeSsaPhi* phi = tree->AsSsaDef()->GetValue()->AsSsaPhi();

                    GenTreeSsaDef* def = m_renameStack.Top(lclNum);

                    // See if this ssaNum is already an arg to the phi.
                    bool alreadyArg = false;
                    for (GenTreeSsaPhi::Use& use : phi->Uses())
                    {
                        if (use.GetNode()->GetDef() == def)
                        {
                            alreadyArg = true;
                            break;
                        }
                    }
                    if (!alreadyArg)
                    {
                        AddPhiArg(handlerStart, stmt, phi, lclNum, def, block);
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

void SsaBuilder::RenameVariables()
{
    JITDUMP("*************** In SsaBuilder::RenameVariables()\n");

    // The first thing we do is treat parameters and must-init variables as if they have a
    // virtual definition before entry -- they start out at SSA name 1.
    GenTreeSsaDef* firstInitSsaDef = nullptr;
    GenTreeSsaDef* lastInitSsaDef  = nullptr;

    for (unsigned lclNum = 0; lclNum < m_pCompiler->lvaCount; lclNum++)
    {
        LclVarDsc* lcl = m_pCompiler->lvaGetDesc(lclNum);

        if (lcl->IsSsa() &&
            VarSetOps::IsMember(m_pCompiler, m_pCompiler->fgFirstBB->bbLiveIn, lcl->GetLivenessBitIndex()))
        {
            unsigned ssaNum = lcl->AllocSsaNum(m_allocator);
            // HasImplicitSsaDef assumes that this is always the first SSA def.
            assert(ssaNum == SsaConfig::FIRST_SSA_NUM);

            // TODO-MIKE-SSA: Having a SSA_UNDEF oper might be better than using a LCL_VAR
            // as a fake def value. It saves a bit of memory by not allocating an extra
            // node and avoids the weird situation of still having LCL_VAR nodes for locals
            // that are supposed to be in SSA form. Though these nodes are not part of any
            // basic block so they're invisible to anything except SSA code, which can treat
            // them specially (by basically ignoring them).

            GenTreeSsaDef* def =
                new (m_pCompiler, GT_SSA_DEF) GenTreeSsaDef(m_pCompiler->gtNewLclvNode(lclNum, lcl->GetType()),
                                                            m_pCompiler->fgFirstBB, lclNum, ssaNum);

            m_renameStack.Push(m_pCompiler->fgFirstBB, lclNum, def);

            if (firstInitSsaDef == nullptr)
            {
                firstInitSsaDef = def;
                lastInitSsaDef  = def;
            }
            else
            {
                lastInitSsaDef->gtNext = def;
                lastInitSsaDef         = def;
            }
        }
    }

    m_pCompiler->m_initSsaDefs = firstInitSsaDef;

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
void SsaBuilder::Print(BasicBlock** postOrder, int count)
{
    for (int i = count - 1; i >= 0; --i)
    {
        printf("After SSA " FMT_BB ":\n", postOrder[i]->bbNum);
        m_pCompiler->gtDispBlockStmts(postOrder[i]);
    }
}
#endif // DEBUG

void GenTreeSsaDef::AddUse(GenTreeSsaUse* use)
{
    use->m_def = this;

    if (m_uses == nullptr)
    {
        use->m_nextUse = use;
        use->m_prevUse = use;
    }
    else
    {
        GenTreeSsaUse* prev = m_uses;
        GenTreeSsaUse* next = prev->m_nextUse;

        prev->m_nextUse = use;
        use->m_prevUse  = prev;
        use->m_nextUse  = next;
        next->m_prevUse = use;
    }

    m_uses = use;
}

void GenTreeSsaDef::RemoveUse(GenTreeSsaUse* use)
{
    assert(use->m_def == this);

    GenTreeSsaUse* prev = use->m_prevUse;
    GenTreeSsaUse* next = use->m_nextUse;

    if (next == use)
    {
        m_uses = nullptr;
        return;
    }

    prev->m_nextUse = next;
    next->m_prevUse = prev;

    if (use->m_def->m_uses == use)
    {
        use->m_def->m_uses = next;
    }
}

bool GenTreeSsaPhi::Equals(GenTreeSsaPhi* phi1, GenTreeSsaPhi* phi2)
{
    if (phi1->GetType() != phi2->GetType())
    {
        return false;
    }

    UseIterator i1   = phi1->Uses().begin();
    UseIterator end1 = phi1->Uses().end();
    UseIterator i2   = phi2->Uses().begin();
    UseIterator end2 = phi2->Uses().end();

    for (; (i1 != end1) && (i2 != end2); ++i1, ++i2)
    {
        if (!Compare(i1->GetNode(), i2->GetNode()))
        {
            return false;
        }
    }

    return (i1 == end1) && (i2 == end2);
}

static void DestroySsaUses(GenTreeSsaDef* def)
{
    unsigned       lclNum = def->GetLclNum();
    GenTreeSsaUse* uses   = def->GetUseList();

    // TODO-MIKE-SSA: The SSA_DEF Uses iterator cannot be used because
    // we change the uses to LCL_VAR as we visit them. Maybe it can be
    // changed to support this (or have a special kind of iterator that
    // supports this), it may be useful in other situations.

    if (uses != nullptr)
    {
        GenTreeSsaUse* use = uses;

        do
        {
            GenTreeSsaUse* nextUse = use->GetNextUse();

            GenTree* load = use;
            load->SetOper(GT_LCL_VAR);
            load->AsLclVar()->SetLclNum(lclNum);

            use = nextUse;
        } while (use != uses);
    }
}

static void DestroySsaDef(Compiler* compiler, GenTreeSsaDef* def, Statement* stmt)
{
    DestroySsaUses(def);

    unsigned lclNum = def->GetLclNum();
    GenTree* store  = def;

    if (GenTreeInsert* insert = def->GetValue()->IsInsert())
    {
        GenTree*         structValue = insert->GetStructValue();
        const FieldInfo& field       = insert->GetField();

        // TODO-MIKE-SSA: Similar to the EXTRACT case, we may need
        // to handle more cases here once optimizations are enabled.

        store->SetOper(GT_STORE_LCL_FLD);
        store->SetType(field.GetType());
        store->AsLclFld()->SetOp(0, insert->GetFieldValue());
        store->AsLclFld()->SetLayoutNum(field.GetLayoutNum());
        store->AsLclFld()->SetLclOffs(field.GetOffset());
        store->AsLclFld()->SetFieldSeq(field.GetFieldSeq());
        store->AsLclFld()->SetLclNum(lclNum);
        store->gtFlags |= GTF_VAR_DEF;

        if (store->IsPartialLclFld(compiler))
        {
            store->gtFlags |= GTF_VAR_USEASG;
        }

        structValue->gtNext->gtPrev = structValue->gtPrev;

        if (structValue->gtPrev != nullptr)
        {
            structValue->gtPrev->gtNext = structValue->gtNext;
        }
        else
        {
            assert(stmt->GetNodeList() == structValue);
            stmt->SetNodeList(structValue->gtNext);
        }

        insert->gtNext->gtPrev = insert->gtPrev;
        insert->gtPrev->gtNext = insert->gtNext;
    }
    else
    {
        store->SetOper(GT_STORE_LCL_VAR);
        store->AsLclVar()->SetLclNum(lclNum);
        store->gtFlags |= GTF_VAR_DEF;
    }
}

static void DestroyExtract(Statement* stmt, GenTreeExtract* extract)
{
    GenTree* src = extract->GetStructValue();

    unsigned lclNum;

    // TODO-MIKE-SSA: Initially the source is always SSA_USE but during
    // destruction we'll tipically encounter the corresponding SSA_DEF
    // first and change the source to LCL_VAR (or maybe LCL_FLD?)
    // And once optimizations are running again we might see something
    // else here (e.g. an INT 0 resulting from struct init propagation)
    // so we'll need to handle more cases.

    if (GenTreeSsaUse* use = src->IsSsaUse())
    {
        lclNum = use->GetDef()->GetLclNum();
    }
    else
    {
        lclNum = src->AsLclVar()->GetLclNum();
    }

    FieldInfo field  = extract->GetField();
    GenTree*  lclFld = extract;

    lclFld->SetOper(GT_LCL_FLD);
    lclFld->AsLclFld()->SetLclNum(lclNum);
    lclFld->AsLclFld()->SetLclOffs(field.GetOffset());
    lclFld->AsLclFld()->SetLayoutNum(field.GetLayoutNum());
    lclFld->AsLclFld()->SetFieldSeq(field.GetFieldSeq());

    src->gtNext->gtPrev = src->gtPrev;

    if (src->gtPrev != nullptr)
    {
        src->gtPrev->gtNext = src->gtNext;
    }
    else
    {
        stmt->SetTreeList(src->gtNext);
    }
}

void Compiler::fgSsaDestroy()
{
    for (GenTree* def = m_initSsaDefs; def != nullptr; def = def->gtNext)
    {
        DestroySsaUses(def->AsSsaDef());
    }

    for (BasicBlock* block : Blocks())
    {
        Statement* lastPhiDef = nullptr;

        for (Statement* stmt : block->Statements())
        {
            if (stmt->GetRootNode()->IsSsaPhiDef())
            {
                DestroySsaUses(stmt->GetRootNode()->AsSsaDef());
                lastPhiDef = stmt;

                // We don't care about the rest of the nodes, they're uses that were
                // (or will be) transformed when their defs are encountered.
                continue;
            }

            for (GenTree* node : stmt->Nodes())
            {
                if (GenTreeSsaDef* def = node->IsSsaDef())
                {
                    DestroySsaDef(this, def, stmt);
                }
                else if (GenTreeExtract* extract = node->IsExtract())
                {
                    DestroyExtract(stmt, extract);
                }
            }

#ifdef DEBUG
            unsigned seqNum = 0;
            for (GenTree* node : stmt->Nodes())
            {
                node->gtSeqNum = ++seqNum;
            }
#endif
        }

        if (lastPhiDef != nullptr)
        {
            Statement* first = lastPhiDef->GetNextStmt();

            if (first == nullptr)
            {
                block->bbStmtList = nullptr;
            }
            else
            {
                Statement* last = block->GetLastStatement();
                INDEBUG(first->SetPrevStmt(nullptr));
                block->SetStatements(first, last);
            }
        }
    }
}
