// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

using VNToBoolMap = JitHashTable<ValueNum, JitSmallPrimitiveKeyFuncs<ValueNum>, bool>;
using VNSet       = JitHashSet<ValueNum, JitSmallPrimitiveKeyFuncs<ValueNum>>;

class LoopHoistTreeVisitor;

class LoopHoist
{
    friend class LoopHoistTreeVisitor;

    using LoopDsc = Compiler::LoopDsc;

    struct LoopStats
    {
        // The register count for the non-FP expressions from inside this loop that have been hoisted
        int lpHoistedExprCount;
        // The register count for the non-FP LclVars that are read/written inside this loop
        int lpLoopVarCount;
        // The register count for the non-FP LclVars that are alive inside or across this loop
        int lpVarInOutCount;
        // The register count for the FP expressions from inside this loop that have been hoisted
        int lpHoistedFPExprCount;
        // The register count for the FP LclVars that are read/written inside this loop
        int lpLoopVarFPCount;
        // The register count for the FP LclVars that are alive inside or across this loop
        int lpVarInOutFPCount;
    };

    Compiler* const      compiler;
    ValueNumStore* const vnStore;
    LoopStats* const     loopStats;
    VNLoop* const        vnLoopTable;
    LoopDsc* const       optLoopTable;
    unsigned const       optLoopCount;
    // The set of variables hoisted in the current loop (or nullptr if there are none).
    VNSet* m_pHoistedInCurLoop = nullptr;
    // Value numbers of expressions that have been hoisted in parent loops in the loop nest.
    VNSet m_hoistedInParentLoops;
    // Value numbers of expressions that have been hoisted in the current (or most recent) loop in the nest.
    // Previous decisions on loop-invariance of value numbers in the current loop.
    VNToBoolMap m_curLoopVnInvariantCache;
#ifndef TARGET_64BIT
    VARSET_TP lvaLongVars; // set of long (64-bit) variables
#endif
    VARSET_TP lvaFloatVars; // set of floating-point (32-bit and 64-bit) variables

public:
    LoopHoist(Compiler* compiler)
        : compiler(compiler)
        , vnStore(compiler->vnStore)
        , loopStats(compiler->getAllocator(CMK_LoopHoist).allocate<LoopStats>(compiler->optLoopCount))
        , vnLoopTable(compiler->valueNumbering->vnLoopTable)
        , optLoopTable(compiler->optLoopTable)
        , optLoopCount(compiler->optLoopCount)
        , m_hoistedInParentLoops(compiler->getAllocator(CMK_LoopHoist))
        , m_curLoopVnInvariantCache(compiler->getAllocator(CMK_LoopHoist))
    {
    }

    void Run();

private:
    void optHoistLoopNest(unsigned lnum);
    void optHoistThisLoop(unsigned lnum);
    bool optIsProfitableToHoistableTree(GenTree* tree, unsigned lnum);
    void optHoistLoopBlocks(unsigned loopNum, ArrayStack<BasicBlock*>* blocks);
    void optHoistCandidate(GenTree* tree, unsigned lnum);
    bool optVNIsLoopInvariant(ValueNum vn, unsigned lnum);
    void optPerformHoistExpr(GenTree* origExpr, unsigned lnum);

    VNSet* GetHoistedInCurrentLoop()
    {
        if (m_pHoistedInCurLoop == nullptr)
        {
            m_pHoistedInCurLoop =
                new (compiler->getAllocator(CMK_LoopHoist)) VNSet(compiler->getAllocator(CMK_LoopHoist));
        }

        return m_pHoistedInCurLoop;
    }

    VNSet* ExtractHoistedInCurrentLoop()
    {
        VNSet* res          = m_pHoistedInCurLoop;
        m_pHoistedInCurLoop = nullptr;
        return res;
    }
};

void LoopHoist::optPerformHoistExpr(GenTree* origExpr, unsigned lnum)
{
    JITDUMPTREE(origExpr, "\nHoisting a copy of [%06u] into PreHeader for loop " FMT_LP " <" FMT_BB ".." FMT_BB ">:\n",
                origExpr->GetID(), lnum, optLoopTable[lnum].lpFirst->bbNum, optLoopTable[lnum].lpBottom->bbNum);
    JITDUMP("\n");

    assert(!origExpr->OperIs(GT_ASG, GT_LCL_DEF, GT_STORE_LCL_VAR));

    // Create a copy of the expression and mark it for CSE's.
    GenTree* hoistExpr = compiler->gtCloneExpr(origExpr, GTF_MAKE_CSE);

    // Copy any loop memory dependence.
    vnStore->CopyLoopMemoryDependence(origExpr, hoistExpr);

    // At this point we should have a cloned expression, marked with the GTF_MAKE_CSE flag
    assert(hoistExpr != origExpr);
    assert((hoistExpr->gtFlags & GTF_MAKE_CSE) != 0);

    GenTree* hoist = compiler->gtUnusedValNode(hoistExpr);

    compiler->fgCreateLoopPreHeader(lnum);

    BasicBlock* preHead = optLoopTable[lnum].lpHead;
    assert(preHead->bbJumpKind == BBJ_NONE);

    compiler->fgMorphBlock = preHead;
    hoist                  = compiler->fgMorphTree(hoist);

    Statement* hoistStmt = compiler->fgNewStmtAtEnd(preHead, hoist);
    hoistStmt->SetCompilerAdded();

    compiler->gtSetStmtInfo(hoistStmt);
    compiler->fgSetStmtSeq(hoistStmt);

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("This hoisted copy placed in PreHeader (" FMT_BB "):\n", preHead->bbNum);
        compiler->gtDispTree(hoist);
    }
#endif

#if LOOP_HOIST_STATS
    if (!compiler->m_curLoopHasHoistedExpression)
    {
        compiler->m_loopsWithHoistedExpressions++;
        compiler->m_curLoopHasHoistedExpression = true;
    }
    compiler->m_totalHoistedExpressions++;
#endif // LOOP_HOIST_STATS
}

void Compiler::optHoistLoopCode()
{
    // If we don't have any loops in the method then take an early out now.
    if (optLoopCount == 0)
    {
        return;
    }

#ifdef DEBUG
    unsigned jitNoHoist = JitConfig.JitNoHoist();
    if (jitNoHoist > 0)
    {
        return;
    }
#endif

#if 0
    // The code in this #if has been useful in debugging loop hoisting issues, by
    // enabling selective enablement of the loop hoisting optimization according to
    // method hash.
#ifdef DEBUG
    unsigned methHash = info.compMethodHash();
    char* lostr = getenv("loophoisthashlo");
    unsigned methHashLo = 0;
    if (lostr != NULL)
    {
        sscanf_s(lostr, "%x", &methHashLo);
        // methHashLo = (unsigned(atoi(lostr)) << 2);  // So we don't have to use negative numbers.
    }
    char* histr = getenv("loophoisthashhi");
    unsigned methHashHi = UINT32_MAX;
    if (histr != NULL)
    {
        sscanf_s(histr, "%x", &methHashHi);
        // methHashHi = (unsigned(atoi(histr)) << 2);  // So we don't have to use negative numbers.
    }
    if (methHash < methHashLo || methHash > methHashHi)
        return;
    printf("Doing loop hoisting in %s (0x%x).\n", info.compFullName, methHash);
#endif // DEBUG
#endif // 0     -- debugging loop hoisting issues

#ifdef DEBUG
    if (verbose)
    {
        printf("\n*************** In optHoistLoopCode()\n");
        printf("Blocks/Trees before phase\n");
        fgDispBasicBlocks(true);
        printf("");
    }
#endif

    LoopHoist hoist(this);
    hoist.Run();

#if DEBUG
    if (fgModified)
    {
        if (verbose)
        {
            printf("Blocks/Trees after optHoistLoopCode() modified flowgraph\n");
            fgDispBasicBlocks(true);
            printf("");
        }

        // Make sure that the predecessor lists are accurate
        fgDebugCheckBBlist();
    }
#endif
}

void LoopHoist::Run()
{
    VarSetOps::AssignNoCopy(compiler, lvaFloatVars, VarSetOps::MakeEmpty(compiler));
#ifndef TARGET_64BIT
    VarSetOps::AssignNoCopy(compiler, lvaLongVars, VarSetOps::MakeEmpty(compiler));
#endif

    for (unsigned i = 0; i < compiler->lvaCount; i++)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(i);

        if (lcl->HasLiveness())
        {
            if (varTypeIsFloating(lcl->GetType()))
            {
                VarSetOps::AddElemD(compiler, lvaFloatVars, lcl->GetLivenessBitIndex());
            }
#ifndef TARGET_64BIT
            else if (lcl->TypeIs(TYP_LONG))
            {
                VarSetOps::AddElemD(compiler, lvaLongVars, lcl->GetLivenessBitIndex());
            }
#endif
        }
    }

    for (unsigned lnum = 0; lnum < optLoopCount; lnum++)
    {
        if (optLoopTable[lnum].lpFlags & LPFLG_REMOVED)
        {
            continue;
        }

        if (optLoopTable[lnum].lpParent == BasicBlock::NOT_IN_LOOP)
        {
            optHoistLoopNest(lnum);
        }
    }
}

void LoopHoist::optHoistLoopNest(unsigned lnum)
{
    // Do this loop, then recursively do all nested loops.
    CLANG_FORMAT_COMMENT_ANCHOR;

#if LOOP_HOIST_STATS
    // Record stats
    compiler->m_curLoopHasHoistedExpression = false;
    compiler->m_loopsConsidered++;
#endif // LOOP_HOIST_STATS

    optHoistThisLoop(lnum);

    VNSet* hoistedInCurLoop = ExtractHoistedInCurrentLoop();

    if (optLoopTable[lnum].lpChild != BasicBlock::NOT_IN_LOOP)
    {
        // Add the ones hoisted in "lnum" to "hoistedInParents" for any nested loops.
        if (hoistedInCurLoop != nullptr)
        {
            for (ValueNum vn : *hoistedInCurLoop)
            {
                INDEBUG(bool added =) m_hoistedInParentLoops.Add(vn);
                assert(added);
            }
        }

        for (unsigned child = optLoopTable[lnum].lpChild; child != BasicBlock::NOT_IN_LOOP;
             child          = optLoopTable[child].lpSibling)
        {
            optHoistLoopNest(child);
        }

        // Now remove them.
        if (hoistedInCurLoop != nullptr)
        {
            for (ValueNum vn : *hoistedInCurLoop)
            {
                m_hoistedInParentLoops.Remove(vn);
            }
        }
    }
}

void LoopHoist::optHoistThisLoop(unsigned lnum)
{
    LoopDsc*   pLoopDsc = &optLoopTable[lnum];
    VNLoop*    vnLoop   = &vnLoopTable[lnum];
    LoopStats& stats    = loopStats[lnum];

    /* If loop was removed continue */

    if (pLoopDsc->lpFlags & LPFLG_REMOVED)
    {
        return;
    }

    /* Get the head and tail of the loop */

    BasicBlock* head = pLoopDsc->lpHead;
    BasicBlock* tail = pLoopDsc->lpBottom;
    BasicBlock* lbeg = pLoopDsc->lpEntry;

    // We must have a do-while loop
    if ((pLoopDsc->lpFlags & LPFLG_DO_WHILE) == 0)
    {
        return;
    }

    // The loop-head must dominate the loop-entry.
    // TODO-CQ: Couldn't we make this true if it's not?
    if (!compiler->fgDominate(head, lbeg))
    {
        return;
    }

    // if lbeg is the start of a new try block then we won't be able to hoist
    if (!BasicBlock::sameTryRegion(head, lbeg))
    {
        return;
    }

    // We don't bother hoisting when inside of a catch block
    if ((lbeg->bbCatchTyp != BBCT_NONE) && (lbeg->bbCatchTyp != BBCT_FINALLY))
    {
        return;
    }

    unsigned begn = lbeg->bbNum;
    unsigned endn = tail->bbNum;

    // Ensure the per-loop sets/tables are empty.
    m_curLoopVnInvariantCache.RemoveAll();

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("optHoistLoopCode for loop " FMT_LP " <" FMT_BB ".." FMT_BB ">:\n", lnum, begn, endn);
        printf("  Loop body %s a call\n", vnLoop->lpContainsCall ? "contains" : "does not contain");
        printf("  Loop has %s\n", (pLoopDsc->lpFlags & LPFLG_ONE_EXIT) ? "single exit" : "multiple exits");
    }
#endif

    VARSET_TP loopVars(VarSetOps::Intersection(compiler, vnLoop->lpVarInOut, vnLoop->lpVarUseDef));

    stats.lpVarInOutCount    = VarSetOps::Count(compiler, vnLoop->lpVarInOut);
    stats.lpLoopVarCount     = VarSetOps::Count(compiler, loopVars);
    stats.lpHoistedExprCount = 0;

#ifndef TARGET_64BIT
    unsigned longVarsCount = VarSetOps::Count(compiler, lvaLongVars);

    if (longVarsCount > 0)
    {
        // Since 64-bit variables take up two registers on 32-bit targets, we increase
        //  the Counts such that each TYP_LONG variable counts twice.
        //
        VARSET_TP loopLongVars(VarSetOps::Intersection(compiler, loopVars, lvaLongVars));
        VARSET_TP inOutLongVars(VarSetOps::Intersection(compiler, vnLoop->lpVarInOut, lvaLongVars));

#ifdef DEBUG
        if (compiler->verbose)
        {
            printf("\n  LONGVARS(%d)=", VarSetOps::Count(compiler, lvaLongVars));
            compiler->lvaDispVarSet(lvaLongVars);
        }
#endif
        stats.lpLoopVarCount += VarSetOps::Count(compiler, loopLongVars);
        stats.lpVarInOutCount += VarSetOps::Count(compiler, inOutLongVars);
    }
#endif // !TARGET_64BIT

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("\n  USEDEF  (%d)=", VarSetOps::Count(compiler, vnLoop->lpVarUseDef));
        compiler->lvaDispVarSet(vnLoop->lpVarUseDef);

        printf("\n  INOUT   (%d)=", stats.lpVarInOutCount);
        compiler->lvaDispVarSet(vnLoop->lpVarInOut);

        printf("\n  LOOPVARS(%d)=", stats.lpLoopVarCount);
        compiler->lvaDispVarSet(loopVars);
        printf("\n");
    }
#endif

    unsigned floatVarsCount = VarSetOps::Count(compiler, lvaFloatVars);

    if (floatVarsCount > 0)
    {
        VARSET_TP loopFPVars(VarSetOps::Intersection(compiler, loopVars, lvaFloatVars));
        VARSET_TP inOutFPVars(VarSetOps::Intersection(compiler, vnLoop->lpVarInOut, lvaFloatVars));

        stats.lpLoopVarFPCount     = VarSetOps::Count(compiler, loopFPVars);
        stats.lpVarInOutFPCount    = VarSetOps::Count(compiler, inOutFPVars);
        stats.lpHoistedFPExprCount = 0;

        stats.lpLoopVarCount -= stats.lpLoopVarFPCount;
        stats.lpVarInOutCount -= stats.lpVarInOutFPCount;

#ifdef DEBUG
        if (compiler->verbose)
        {
            printf("  INOUT-FP(%d)=", stats.lpVarInOutFPCount);
            compiler->lvaDispVarSet(inOutFPVars);

            printf("\n  LOOPV-FP(%d)=", stats.lpLoopVarFPCount);
            compiler->lvaDispVarSet(loopFPVars);

            printf("\n");
        }
#endif
    }
    else // (floatVarsCount == 0)
    {
        stats.lpLoopVarFPCount     = 0;
        stats.lpVarInOutFPCount    = 0;
        stats.lpHoistedFPExprCount = 0;
    }

    // Find the set of definitely-executed blocks.
    // Ideally, the definitely-executed blocks are the ones that post-dominate the entry block.
    // Until we have post-dominators, we'll special-case for single-exit blocks.
    ArrayStack<BasicBlock*> defExec(compiler->getAllocator(CMK_LoopHoist));
    if (pLoopDsc->lpFlags & LPFLG_ONE_EXIT)
    {
        assert(pLoopDsc->lpExit != nullptr);
        BasicBlock* cur = pLoopDsc->lpExit;
        // Push dominators, until we reach "entry" or exit the loop.
        while (cur != nullptr && pLoopDsc->lpContains(cur) && cur != pLoopDsc->lpEntry)
        {
            defExec.Push(cur);
            cur = cur->bbIDom;
        }
        // If we didn't reach the entry block, give up and *just* push the entry block.
        if (cur != pLoopDsc->lpEntry)
        {
            defExec.Clear();
        }
        defExec.Push(pLoopDsc->lpEntry);
    }
    else // More than one exit
    {
        // We'll assume that only the entry block is definitely executed.
        // We could in the future do better.
        defExec.Push(pLoopDsc->lpEntry);
    }

    optHoistLoopBlocks(lnum, &defExec);
}

bool LoopHoist::optIsProfitableToHoistableTree(GenTree* tree, unsigned lnum)
{
    LoopDsc*   pLoopDsc = &optLoopTable[lnum];
    VNLoop*    vnLoop   = &vnLoopTable[lnum];
    LoopStats& stats    = loopStats[lnum];

    bool loopContainsCall = vnLoop->lpContainsCall;

    int availRegCount;
    int hoistedExprCount;
    int loopVarCount;
    int varInOutCount;

    if (varTypeIsFloating(tree))
    {
        hoistedExprCount = stats.lpHoistedFPExprCount;
        loopVarCount     = stats.lpLoopVarFPCount;
        varInOutCount    = stats.lpVarInOutFPCount;

        availRegCount = CNT_CALLEE_SAVED_FLOAT;
        if (!loopContainsCall)
        {
            availRegCount += CNT_CALLEE_TRASH_FLOAT - 1;
        }
#ifdef TARGET_ARM
        // For ARM each double takes two FP registers
        // For now on ARM we won't track singles/doubles
        // and instead just assume that we always have doubles.
        //
        availRegCount /= 2;
#endif
    }
    else
    {
        hoistedExprCount = stats.lpHoistedExprCount;
        loopVarCount     = stats.lpLoopVarCount;
        varInOutCount    = stats.lpVarInOutCount;

        availRegCount = CNT_CALLEE_SAVED - 1;
        if (!loopContainsCall)
        {
            availRegCount += CNT_CALLEE_TRASH - 1;
        }
#ifndef TARGET_64BIT
        // For our 32-bit targets Long types take two registers.
        if (varTypeIsLong(tree->TypeGet()))
        {
            availRegCount = (availRegCount + 1) / 2;
        }
#endif
    }

    // decrement the availRegCount by the count of expression that we have already hoisted.
    availRegCount -= hoistedExprCount;

    // the variables that are read/written inside the loop should
    // always be a subset of the InOut variables for the loop
    assert(loopVarCount <= varInOutCount);

    // When loopVarCount >= availRegCount we believe that all of the
    // available registers will get used to hold LclVars inside the loop.
    // This pessimistically assumes that each loopVar has a conflicting
    // lifetime with every other loopVar.
    // For this case we will hoist the expression only if is profitable
    // to place it in a stack home location (GetCostEx() >= 2*IND_COST_EX)
    // as we believe it will be placed in the stack or one of the other
    // loopVars will be spilled into the stack
    //
    if (loopVarCount >= availRegCount)
    {
        // Don't hoist expressions that are not heavy: tree->GetCostEx() < (2*IND_COST_EX)
        if (tree->GetCostEx() < (2 * IND_COST_EX))
        {
            return false;
        }
    }

    // When varInOutCount < availRegCount we are know that there are
    // some available register(s) when we enter the loop body.
    // When varInOutCount == availRegCount there often will be a register
    // available when we enter the loop body, since a loop often defines a
    // LclVar on exit or there is often at least one LclVar that is worth
    // spilling to the stack to make way for this hoisted expression.
    // So we are willing hoist an expression with GetCostEx() == MIN_CSE_COST
    //
    if (varInOutCount > availRegCount)
    {
        // Don't hoist expressions that barely meet CSE cost requirements: tree->GetCostEx() == MIN_CSE_COST
        if (tree->GetCostEx() <= Compiler::MIN_CSE_COST + 1)
        {
            return false;
        }
    }

    return true;
}

class LoopHoistTreeVisitor : public GenTreeVisitor<LoopHoistTreeVisitor>
{
    class Value
    {
        GenTree* m_node;

    public:
        bool m_hoistable;
        bool m_cctorDependent;
        bool m_invariant;

        Value(GenTree* node) : m_node(node), m_hoistable(false), m_cctorDependent(false), m_invariant(false)
        {
        }

        GenTree* Node()
        {
            return m_node;
        }
    };

    ArrayStack<Value> m_valueStack;
    bool              m_beforeSideEffect;
    unsigned          m_loopNum;
    LoopHoist*        m_loopHoist;

    bool IsNodeHoistable(GenTree* node)
    {
        // TODO-CQ: This is a more restrictive version of a check that cseIsCandidate already does - it allows
        // a struct typed node if a class handle can be recovered from it.
        if (node->TypeGet() == TYP_STRUCT)
        {
            return false;
        }

        // Tree must be a suitable CSE candidate for us to be able to hoist it.
        return m_compiler->cseIsCandidate(node);
    }

    bool IsTreeVNInvariant(GenTree* tree)
    {
        ValueNum vn            = tree->gtVNPair.GetLiberal();
        bool     vnIsInvariant = m_loopHoist->optVNIsLoopInvariant(vn, m_loopNum);

        // Even though VN is invariant in the loop (say a constant) its value may depend on position
        // of tree, so for loop hoisting we must also check that any memory read by tree
        // is also invariant in the loop.
        //
        if (vnIsInvariant)
        {
            vnIsInvariant = IsTreeLoopMemoryInvariant(tree);
        }
        return vnIsInvariant;
    }

    //------------------------------------------------------------------------
    // IsTreeLoopMemoryInvariant: determine if the value number of tree
    //   is dependent on the tree being executed within the current loop
    //
    // Arguments:
    //   tree -- tree in question
    //
    // Returns:
    //   true if tree could be evaluated just before loop and get the
    //   same value.
    //
    // Note:
    //   Calls are optimistically assumed to be invariant.
    //   Caller must do their own analysis for these tree types.
    //
    bool IsTreeLoopMemoryInvariant(GenTree* tree)
    {
        if (tree->IsCall())
        {
            // Calls are handled specially by hoisting, and loop memory dependence
            // must be checked by other means.
            //
            return true;
        }

        if (BasicBlock* loopEntryBlock = m_compiler->vnStore->GetLoopMemoryBlock(tree))
        {
            ValueNum loopMemoryVN = m_compiler->GetMemoryPerSsaData(loopEntryBlock->memoryEntrySsaNum)->m_vn;

            if (!m_loopHoist->optVNIsLoopInvariant(loopMemoryVN, m_loopNum))
            {
                return false;
            }
        }

        return true;
    }

public:
    enum
    {
        ComputeStack      = false,
        DoPreOrder        = true,
        DoPostOrder       = true,
        DoLclVarsOnly     = false,
        UseExecutionOrder = true,
    };

    LoopHoistTreeVisitor(Compiler* compiler, unsigned loopNum, LoopHoist* loopHoist)
        : GenTreeVisitor(compiler)
        , m_valueStack(compiler->getAllocator(CMK_LoopHoist))
        , m_beforeSideEffect(true)
        , m_loopNum(loopNum)
        , m_loopHoist(loopHoist)
    {
    }

    void HoistBlock(BasicBlock* block)
    {
        for (Statement* const stmt : block->NonPhiStatements())
        {
            WalkTree(stmt->GetRootNodePointer(), nullptr);
            assert(m_valueStack.TopRef().Node() == stmt->GetRootNode());

            if (m_valueStack.TopRef().m_hoistable)
            {
                m_loopHoist->optHoistCandidate(stmt->GetRootNode(), m_loopNum);
            }

            m_valueStack.Clear();
        }

        // Only uncondtionally executed blocks in the loop are visited (see optHoistThisLoop)
        // so after we're done visiting the first block we need to assume the worst, that the
        // blocks that are not visisted have side effects.
        m_beforeSideEffect = false;
    }

    fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
    {
        GenTree* node = *use;
        m_valueStack.Emplace(node);
        return fgWalkResult::WALK_CONTINUE;
    }

    fgWalkResult PostOrderVisit(GenTree** use, GenTree* user)
    {
        GenTree* tree = *use;

        if (tree->OperIs(GT_LCL_VAR, GT_LCL_FLD))
        {
            return fgWalkResult::WALK_CONTINUE;
        }

        if (GenTreeLclUse* use = tree->IsLclUse())
        {
            // TODO-MIKE-Cleanup: Unreachable blocks aren't properly removed (see Runtime_57061_2).
            // Such blocks may or may not be traversed by various JIT phases - SSA builder does not
            // traverse them but this code does and ends up asserting due to missing SSA numbers.
            // Well, at least that's why this probably checks for NoSsaNum, but it seems unlikely
            // that loop hositing would hit dead code. We'll see.

            bool isInvariant = !m_compiler->optLoopTable[m_loopNum].lpContains(use->GetDef()->GetBlock());

            // TODO-CQ: This VN invariance check should not be necessary and in some cases it is conservative - it
            // is possible that the SSA def is outside the loop but VN does not understand what the node is doing
            // (e.g. LCL_FLD-based type reinterpretation) and assigns a "new, unique VN" to the node. This VN is
            // associated with the block where the node is, a loop block, and thus the VN is considered to not be
            // invariant.
            // On the other hand, it is possible for a SSA def to be inside the loop yet the use to be invariant,
            // if the defining expression is also invariant. In such a case the VN invariance would help but it is
            // blocked by the SSA invariance check.
            isInvariant = isInvariant && IsTreeVNInvariant(tree);

            if (isInvariant)
            {
                Value& top = m_valueStack.TopRef();
                assert(top.Node() == tree);
                top.m_invariant = true;
                // In general it doesn't make sense to hoist a local node but there are exceptions, for example
                // LCL_FLD nodes (because then the variable cannot be enregistered and the node always turns
                // into a memory access).
                top.m_hoistable = IsNodeHoistable(tree);
            }

            return fgWalkResult::WALK_CONTINUE;
        }

        // Initclass CLS_VAR_ADDRs and IconHandles are the base cases of cctor dependent trees.
        // In the IconHandle case, it's of course the dereference, rather than the constant itself, that is
        // truly dependent on the cctor.  So a more precise approach would be to separately propagate
        // isCctorDependent and isAddressWhoseDereferenceWouldBeCctorDependent, but we don't for
        // simplicity/throughput; the constant itself would be considered non-hoistable anyway, since
        // cseIsCandidate returns false for constants.
        bool treeIsCctorDependent = (tree->OperIs(GT_CLS_VAR_ADDR) && ((tree->gtFlags & GTF_CLS_VAR_INITCLASS) != 0)) ||
                                    (tree->OperIs(GT_CNS_INT) && ((tree->gtFlags & GTF_ICON_INITCLASS) != 0));
        bool treeIsInvariant          = true;
        bool treeHasHoistableChildren = false;
        int  childCount;

        for (childCount = 0; m_valueStack.TopRef(childCount).Node() != tree; childCount++)
        {
            Value& child = m_valueStack.TopRef(childCount);

            if (child.m_hoistable)
            {
                treeHasHoistableChildren = true;
            }

            if (!child.m_invariant)
            {
                treeIsInvariant = false;
            }

            if (child.m_cctorDependent)
            {
                // Normally, a parent of a cctor-dependent tree is also cctor-dependent.
                treeIsCctorDependent = true;

                // Check for the case where we can stop propagating cctor-dependent upwards.
                if (tree->OperIs(GT_COMMA) && (child.Node() == tree->gtGetOp2()))
                {
                    GenTree* op1 = tree->gtGetOp1();
                    if (op1->OperIs(GT_CALL))
                    {
                        GenTreeCall* call = op1->AsCall();
                        if ((call->gtCallType == CT_HELPER) &&
                            Compiler::s_helperCallProperties.MayRunCctor(Compiler::eeGetHelperNum(call->gtCallMethHnd)))
                        {
                            // Hoisting the comma is ok because it would hoist the initialization along
                            // with the static field reference.
                            treeIsCctorDependent = false;
                            // Hoisting the static field without hoisting the initialization would be
                            // incorrect, make sure we consider the field (which we flagged as
                            // cctor-dependent) non-hoistable.
                            noway_assert(!child.m_hoistable);
                        }
                    }
                }
            }
        }

        // If all the children of "tree" are hoistable, then "tree" itself can be hoisted,
        // unless it has a static var reference that can't be hoisted past its cctor call.
        bool treeIsHoistable = treeIsInvariant && !treeIsCctorDependent;

        // But we must see if anything else prevents "tree" from being hoisted.
        //
        if (treeIsInvariant)
        {
            if (treeIsHoistable)
            {
                treeIsHoistable = IsNodeHoistable(tree);
            }

            // If it's a call, it must be a helper call, and be pure.
            // Further, if it may run a cctor, it must be labeled as "Hoistable"
            // (meaning it won't run a cctor because the class is not precise-init).
            if (treeIsHoistable && tree->IsCall())
            {
                GenTreeCall* call = tree->AsCall();
                if (call->gtCallType != CT_HELPER)
                {
                    treeIsHoistable = false;
                }
                else
                {
                    CorInfoHelpFunc helpFunc = Compiler::eeGetHelperNum(call->gtCallMethHnd);
                    if (!Compiler::s_helperCallProperties.IsPure(helpFunc))
                    {
                        treeIsHoistable = false;
                    }
                    else if (Compiler::s_helperCallProperties.MayRunCctor(helpFunc) &&
                             ((call->gtFlags & GTF_CALL_HOISTABLE) == 0))
                    {
                        treeIsHoistable = false;
                    }
                }
            }

            if (treeIsHoistable)
            {
                if (!m_beforeSideEffect)
                {
                    // For now, we give up on an expression that might raise an exception if it is after the
                    // first possible global side effect (and we assume we're after that if we're not in the first
                    // block).
                    // TODO-CQ: this is when we might do loop cloning.
                    //
                    if ((tree->gtFlags & GTF_EXCEPT) != 0)
                    {
                        treeIsHoistable = false;
                    }
                }
            }

            // Is the value of the whole tree loop invariant?
            treeIsInvariant = IsTreeVNInvariant(tree);

            // Is the value of the whole tree loop invariant?
            if (!treeIsInvariant)
            {
                // Here we have a tree that is not loop invariant and we thus cannot hoist
                treeIsHoistable = false;
            }
        }

        // Next check if we need to set 'm_beforeSideEffect' to false.
        //
        // If we have already set it to false then we can skip these checks
        //
        if (m_beforeSideEffect)
        {
            // Is the value of the whole tree loop invariant?
            if (!treeIsInvariant)
            {
                // We have a tree that is not loop invariant and we thus cannot hoist
                assert(treeIsHoistable == false);

                // Check if we should clear m_beforeSideEffect.
                // If 'tree' can throw an exception then we need to set m_beforeSideEffect to false.
                // Note that calls are handled below
                if (tree->OperMayThrow(m_compiler) && !tree->IsCall())
                {
                    m_beforeSideEffect = false;
                }
            }

            // In the section below, we only care about memory side effects.  We assume that expressions will
            // be hoisted so that they are evaluated in the same order as they would have been in the loop,
            // and therefore throw exceptions in the same order.
            //
            if (tree->IsCall())
            {
                // If it's a call, it must be a helper call that does not mutate the heap.
                // Further, if it may run a cctor, it must be labeled as "Hoistable"
                // (meaning it won't run a cctor because the class is not precise-init).
                GenTreeCall* call = tree->AsCall();
                if (call->gtCallType != CT_HELPER)
                {
                    m_beforeSideEffect = false;
                }
                else
                {
                    CorInfoHelpFunc helpFunc = Compiler::eeGetHelperNum(call->gtCallMethHnd);
                    if (Compiler::s_helperCallProperties.MutatesHeap(helpFunc))
                    {
                        m_beforeSideEffect = false;
                    }
                    else if (Compiler::s_helperCallProperties.MayRunCctor(helpFunc) &&
                             (call->gtFlags & GTF_CALL_HOISTABLE) == 0)
                    {
                        m_beforeSideEffect = false;
                    }

                    // Additional check for helper calls that throw exceptions
                    if (!treeIsInvariant)
                    {
                        // We have a tree that is not loop invariant and we thus cannot hoist
                        assert(treeIsHoistable == false);

                        // Does this helper call throw?
                        if (!Compiler::s_helperCallProperties.NoThrow(helpFunc))
                        {
                            m_beforeSideEffect = false;
                        }
                    }
                }
            }
            else if (tree->OperIs(GT_ASG))
            {
                // If the LHS of the assignment has a global reference, then assume it's a global side effect.
                GenTree* lhs = tree->AsOp()->gtOp1;
                if (lhs->gtFlags & GTF_GLOB_REF)
                {
                    m_beforeSideEffect = false;
                }
            }
            else if (tree->OperIs(GT_XADD, GT_XORR, GT_XAND, GT_XCHG, GT_LOCKADD, GT_CMPXCHG, GT_MEMORYBARRIER))
            {
                // If this node is a MEMORYBARRIER or an Atomic operation
                // then don't hoist and stop any further hoisting after this node
                treeIsHoistable    = false;
                m_beforeSideEffect = false;
            }
        }

        // If this 'tree' is hoistable then we return and the caller will
        // decide to hoist it as part of larger hoistable expression.
        //
        if (!treeIsHoistable && treeHasHoistableChildren)
        {
            // The current tree is not hoistable but it has hoistable children that we need
            // to hoist now.
            //
            // In order to preserve the original execution order, we also need to hoist any
            // other hoistable trees that we encountered so far.
            // At this point the stack contains (in top to bottom order):
            //   - the current node's children
            //   - the current node
            //   - ancestors of the current node and some of their descendants
            //
            // The ancestors have not been visited yet in post order so they're not hoistable
            // (and they cannot become hoistable because the current node is not) but some of
            // their descendants may have already been traversed and be hoistable.
            //
            // The execution order is actually bottom to top so we'll start hoisting from
            // the bottom of the stack, skipping the current node (which is expected to not
            // be hoistable).
            //
            // Note that the treeHasHoistableChildren check avoids unnecessary stack traversing
            // and also prevents hoisting trees too early. If the current tree is not hoistable
            // and it doesn't have any hoistable children then there's no point in hoisting any
            // other trees. Doing so would interfere with the cctor dependent case, where the
            // cctor dependent node is initially not hoistable and may become hoistable later,
            // when its parent comma node is visited.
            //
            for (unsigned i = 0; i < m_valueStack.Size(); i++)
            {
                Value& value = m_valueStack.BottomRef(i);

                if (value.m_hoistable)
                {
                    assert(value.Node() != tree);

                    // Don't hoist this tree again.
                    value.m_hoistable = false;
                    value.m_invariant = false;

                    m_loopHoist->optHoistCandidate(value.Node(), m_loopNum);
                }
            }
        }

        m_valueStack.Pop(childCount);

        Value& top = m_valueStack.TopRef();
        assert(top.Node() == tree);
        top.m_hoistable      = treeIsHoistable;
        top.m_cctorDependent = treeIsCctorDependent;
        top.m_invariant      = treeIsInvariant;

        return fgWalkResult::WALK_CONTINUE;
    }
};

//------------------------------------------------------------------------
// optHoistLoopBlocks: Hoist invariant expression out of the loop.
//
// Arguments:
//    loopNum - The number of the loop
//    blocks - A stack of blocks belonging to the loop
//    hoistContext - The loop hoist context
//
// Assumptions:
//    The `blocks` stack contains the definitely-executed blocks in
//    the loop, in the execution order, starting with the loop entry
//    block on top of the stack.
//
void LoopHoist::optHoistLoopBlocks(unsigned loopNum, ArrayStack<BasicBlock*>* blocks)
{
    LoopDsc* loopDsc = &optLoopTable[loopNum];
    assert(blocks->Top() == loopDsc->lpEntry);

    LoopHoistTreeVisitor visitor(compiler, loopNum, this);

    while (!blocks->Empty())
    {
        BasicBlock*          block       = blocks->Pop();
        BasicBlock::weight_t blockWeight = block->getBBWeight(compiler);

        JITDUMP("    optHoistLoopBlocks " FMT_BB " (weight=%6s) of loop " FMT_LP " <" FMT_BB ".." FMT_BB
                ">, firstBlock is %s\n",
                block->bbNum, refCntWtd2str(blockWeight), loopNum, loopDsc->lpFirst->bbNum, loopDsc->lpBottom->bbNum,
                dspBool(block == loopDsc->lpEntry));

        if (blockWeight < (BB_UNITY_WEIGHT / 10))
        {
            JITDUMP("      block weight is too small to perform hoisting.\n");
            continue;
        }

        visitor.HoistBlock(block);
    }
}

void LoopHoist::optHoistCandidate(GenTree* tree, unsigned lnum)
{
    assert(lnum != BasicBlock::NOT_IN_LOOP);

    // It must pass the hoistable profitablity tests for this loop level
    if (!optIsProfitableToHoistableTree(tree, lnum))
    {
        return;
    }

    if (m_hoistedInParentLoops.Contains(tree->GetLiberalVN()))
    {
        return;
    }

    if (GetHoistedInCurrentLoop()->Contains(tree->GetLiberalVN()))
    {
        return;
    }

    // Expression can be hoisted
    optPerformHoistExpr(tree, lnum);

    // Increment lpHoistedExprCount or lpHoistedFPExprCount
    if (!varTypeIsFloating(tree->TypeGet()))
    {
        loopStats[lnum].lpHoistedExprCount++;
#ifndef TARGET_64BIT
        // For our 32-bit targets Long types take two registers.
        if (varTypeIsLong(tree->TypeGet()))
        {
            loopStats[lnum].lpHoistedExprCount++;
        }
#endif
    }
    else // Floating point expr hoisted
    {
        loopStats[lnum].lpHoistedFPExprCount++;
    }

    GetHoistedInCurrentLoop()->Add(tree->GetLiberalVN());
}

bool LoopHoist::optVNIsLoopInvariant(ValueNum vn, unsigned lnum)
{
    // If it is not a VN, is not loop-invariant.
    if (vn == ValueNumStore::NoVN)
    {
        return false;
    }

    // We'll always short-circuit constants.
    if (vnStore->IsVNConstant(vn) || vn == vnStore->VNForVoid())
    {
        return true;
    }

    // If we've done this query previously, don't repeat.
    bool previousRes = false;
    if (m_curLoopVnInvariantCache.Lookup(vn, &previousRes))
    {
        return previousRes;
    }

    bool      res = true;
    VNFuncApp funcApp;
    if (vnStore->GetVNFunc(vn, &funcApp))
    {
        if ((funcApp.m_func == VNF_PhiDef) || (funcApp.m_func == VNF_PhiMemoryDef))
        {
            res = !compiler->optLoopContains(lnum, vnStore->ConstantHostPtr<BasicBlock>(funcApp[1])->bbNatLoopNum);
        }
        else if (funcApp.m_func == VNF_MemOpaque)
        {
            res = !compiler->optLoopContains(lnum, funcApp[0]);
        }
        else
        {
            for (unsigned i = 0; i < funcApp.m_arity; i++)
            {
                if (funcApp.m_func == VNF_MapStore)
                {
                    assert(funcApp.m_arity == 4);

                    if (i == 3)
                    {
                        res = !compiler->optLoopContains(lnum, funcApp[3]);
                        break;
                    }
                }

                // TODO-CQ: We need to either make sure that *all* VN functions
                // always take VN args, or else have a list of arg positions to exempt, as implicitly
                // constant.
                if (!optVNIsLoopInvariant(funcApp.m_args[i], lnum))
                {
                    res = false;
                    break;
                }
            }
        }
    }

    m_curLoopVnInvariantCache.Set(vn, res);
    return res;
}

/*****************************************************************************
 *
 *  Creates a pre-header block for the given loop - a preheader is a BBJ_NONE
 *  header. The pre-header will replace the current lpHead in the loop table.
 *  The loop has to be a do-while loop. Thus, all blocks dominated by lpHead
 *  will also be dominated by the loop-top, lpHead->bbNext.
 *
 */

void Compiler::fgCreateLoopPreHeader(unsigned lnum)
{
    LoopDsc* pLoopDsc = &optLoopTable[lnum];

    /* This loop has to be a "do-while" loop */

    assert(pLoopDsc->lpFlags & LPFLG_DO_WHILE);

    /* Have we already created a loop-preheader block? */

    if (pLoopDsc->lpFlags & LPFLG_HAS_PREHEAD)
    {
        return;
    }

    BasicBlock* head  = pLoopDsc->lpHead;
    BasicBlock* top   = pLoopDsc->lpTop;
    BasicBlock* entry = pLoopDsc->lpEntry;

    // if 'entry' and 'head' are in different try regions then we won't be able to hoist
    if (!BasicBlock::sameTryRegion(head, entry))
    {
        return;
    }

    // Ensure that lpHead always dominates lpEntry

    noway_assert(fgDominate(head, entry));

    /* Get hold of the first block of the loop body */

    assert(top == entry);

    /* Allocate a new basic block */

    BasicBlock* preHead = bbNewBasicBlock(BBJ_NONE);
    preHead->bbFlags |= BBF_INTERNAL | BBF_LOOP_PREHEADER;

    // Must set IL code offset
    preHead->bbCodeOffs = top->bbCodeOffs;

    // Set the default value of the preHead weight in case we don't have
    // valid profile data and since this blocks weight is just an estimate
    // we clear any BBF_PROF_WEIGHT flag that we may have picked up from head.
    //
    preHead->inheritWeight(head);
    preHead->bbFlags &= ~BBF_PROF_WEIGHT;

    // Copy the bbReach set from head for the new preHead block
    preHead->bbReach = BlockSetOps::MakeEmpty(this);
    BlockSetOps::Assign(this, preHead->bbReach, head->bbReach);
    // Also include 'head' in the preHead bbReach set
    BlockSetOps::AddElemD(this, preHead->bbReach, head->bbNum);

#ifdef DEBUG
    if (verbose)
    {
        printf("\nCreated PreHeader (" FMT_BB ") for loop " FMT_LP " (" FMT_BB " - " FMT_BB "), with weight = %s\n",
               preHead->bbNum, lnum, top->bbNum, pLoopDsc->lpBottom->bbNum, refCntWtd2str(preHead->getBBWeight(this)));
    }
#endif

    // The preheader block is part of the containing loop (if any).
    preHead->bbNatLoopNum = pLoopDsc->lpParent;

    if (fgIsUsingProfileWeights() && (head->bbJumpKind == BBJ_COND))
    {
        if ((head->bbWeight == BB_ZERO_WEIGHT) || (head->bbNext->bbWeight == BB_ZERO_WEIGHT))
        {
            preHead->bbWeight = BB_ZERO_WEIGHT;
            preHead->bbFlags |= BBF_RUN_RARELY;
        }
        else
        {
            bool allValidProfileWeights =
                (head->hasProfileWeight() && head->bbJumpDest->hasProfileWeight() && head->bbNext->hasProfileWeight());

            if (allValidProfileWeights)
            {
                BasicBlock::weight_t loopEnteredCount;
                BasicBlock::weight_t loopSkippedCount;

                if (fgHaveValidEdgeWeights)
                {
                    flowList* edgeToNext = fgGetPredForBlock(head->bbNext, head);
                    flowList* edgeToJump = fgGetPredForBlock(head->bbJumpDest, head);
                    noway_assert(edgeToNext != nullptr);
                    noway_assert(edgeToJump != nullptr);

                    loopEnteredCount = (edgeToNext->edgeWeightMin() + edgeToNext->edgeWeightMax()) / 2.0f;
                    loopSkippedCount = (edgeToJump->edgeWeightMin() + edgeToJump->edgeWeightMax()) / 2.0f;
                }
                else
                {
                    loopEnteredCount = head->bbNext->bbWeight;
                    loopSkippedCount = head->bbJumpDest->bbWeight;
                }

                JITDUMP("%s; loopEnterCount " FMT_WT " loopSkipCount " FMT_WT "\n",
                        fgHaveValidEdgeWeights ? "valid edge weights" : "no edge weights", loopEnteredCount,
                        loopSkippedCount);

                BasicBlock::weight_t loopTakenRatio = loopEnteredCount / (loopEnteredCount + loopSkippedCount);

                JITDUMP("%s; loopEnterCount " FMT_WT " loopSkipCount " FMT_WT " taken ratio " FMT_WT "\n",
                        fgHaveValidEdgeWeights ? "valid edge weights" : "no edge weights", loopEnteredCount,
                        loopSkippedCount, loopTakenRatio);

                // Calculate a good approximation of the preHead's block weight
                BasicBlock::weight_t preHeadWeight = (head->bbWeight * loopTakenRatio);
                preHead->setBBProfileWeight(preHeadWeight);
                noway_assert(!preHead->isRunRarely());
            }
        }
    }

    // Link in the preHead block
    fgInsertBBbefore(top, preHead);

    // Ideally we would re-run SSA and VN if we optimized by doing loop hoisting.
    // However, that is too expensive at this point. Instead, we update the phi
    // node block references, if we created pre-header block due to hoisting.
    // This is sufficient because any definition participating in SSA that flowed
    // into the phi via the loop header block will now flow through the preheader
    // block from the header block.

    for (Statement* const stmt : top->Statements())
    {
        GenTree* tree = stmt->GetRootNode();

        if (!tree->IsPhiDef())
        {
            break;
        }

        for (GenTreePhi::Use& use : tree->AsLclDef()->GetValue()->AsPhi()->Uses())
        {
            if (use.GetNode()->GetBlock() == head)
            {
                use.GetNode()->SetBlock(preHead);
            }
        }
    }

    // The handler can't begin at the top of the loop.  If it did, it would be incorrect
    // to set the handler index on the pre header without updating the exception table.
    noway_assert(!top->hasHndIndex() || fgFirstBlockOfHandler(top) != top);

    // Update the EH table to make the hoisted block part of the loop's EH block.
    fgExtendEHRegionBefore(top);

    // TODO-CQ: set dominators for this block, to allow loop optimizations requiring them
    //        (e.g: hoisting expression in a loop with the same 'head' as this one)

    /* Update the loop entry */

    pLoopDsc->lpHead = preHead;
    pLoopDsc->lpFlags |= LPFLG_HAS_PREHEAD;

    /* The new block becomes the 'head' of the loop - update bbRefs and bbPreds
       All predecessors of 'beg', (which is the entry in the loop)
       now have to jump to 'preHead', unless they are dominated by 'head' */

    preHead->bbRefs                 = 0;
    flowList* const edgeToPreHeader = fgAddRefPred(preHead, head);
    edgeToPreHeader->setEdgeWeights(preHead->bbWeight, preHead->bbWeight, preHead);
    bool checkNestedLoops = false;

    for (BasicBlock* const predBlock : top->PredBlocks())
    {
        if (fgDominate(top, predBlock))
        {
            // note: if 'top' dominates predBlock, 'head' dominates predBlock too
            // (we know that 'head' dominates 'top'), but using 'top' instead of
            // 'head' in the test allows us to not enter here if 'predBlock == head'

            if (predBlock != pLoopDsc->lpBottom)
            {
                noway_assert(predBlock != head);
                checkNestedLoops = true;
            }
            continue;
        }

        switch (predBlock->bbJumpKind)
        {
            case BBJ_NONE:
                noway_assert(predBlock == head);
                break;

            case BBJ_COND:
                if (predBlock == head)
                {
                    noway_assert(predBlock->bbJumpDest != top);
                    break;
                }
                FALLTHROUGH;

            case BBJ_ALWAYS:
            case BBJ_EHCATCHRET:
                noway_assert(predBlock->bbJumpDest == top);
                predBlock->bbJumpDest = preHead;

                if (predBlock == head)
                {
                    // This is essentially the same case of predBlock being a BBJ_NONE. We may not be
                    // able to make this a BBJ_NONE if it's an internal block (for example, a leave).
                    // Just break, pred will be removed after switch.
                }
                else
                {
                    fgRemoveRefPred(top, predBlock);
                    fgAddRefPred(preHead, predBlock);
                }
                break;

            case BBJ_SWITCH:
                unsigned jumpCnt;
                jumpCnt = predBlock->bbJumpSwt->bbsCount;
                BasicBlock** jumpTab;
                jumpTab = predBlock->bbJumpSwt->bbsDstTab;

                do
                {
                    assert(*jumpTab);
                    if ((*jumpTab) == top)
                    {
                        (*jumpTab) = preHead;

                        fgRemoveRefPred(top, predBlock);
                        fgAddRefPred(preHead, predBlock);
                    }
                } while (++jumpTab, --jumpCnt);
                break;

            default:
                noway_assert(!"Unexpected bbJumpKind");
                break;
        }
    }

    noway_assert(!fgGetPredForBlock(top, preHead));
    fgRemoveRefPred(top, head);
    flowList* const edgeFromPreHeader = fgAddRefPred(top, preHead);
    edgeFromPreHeader->setEdgeWeights(preHead->bbWeight, preHead->bbWeight, top);

    /*
        If we found at least one back-edge in the flowgraph pointing to the top/entry of the loop
        (other than the back-edge of the loop we are considering) then we likely have nested
        do-while loops with the same entry block and inserting the preheader block changes the head
        of all the nested loops. Now we will update this piece of information in the loop table, and
        mark all nested loops as having a preheader (the preheader block can be shared among all nested
        do-while loops with the same entry block).
    */
    if (checkNestedLoops)
    {
        for (unsigned l = 0; l < optLoopCount; l++)
        {
            if (optLoopTable[l].lpHead == head)
            {
                noway_assert(l != lnum); // pLoopDsc->lpHead was already changed from 'head' to 'preHead'
                noway_assert(optLoopTable[l].lpEntry == top);
                optUpdateLoopHead(l, optLoopTable[l].lpHead, preHead);
                optLoopTable[l].lpFlags |= LPFLG_HAS_PREHEAD;
#ifdef DEBUG
                if (verbose)
                {
                    printf("Same PreHeader (" FMT_BB ") can be used for loop " FMT_LP " (" FMT_BB " - " FMT_BB ")\n\n",
                           preHead->bbNum, l, top->bbNum, optLoopTable[l].lpBottom->bbNum);
                }
#endif
            }
        }
    }
}
