// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "ssabuilder.h"
#include "valuenum.h"

using VNSet = JitHashSet<ValueNum>;
class LoopHoistTreeVisitor;

class LoopHoist
{
    friend class LoopHoistTreeVisitor;

    struct LoopStats
    {
        VARSET_TP floatLocals;
#ifndef TARGET_64BIT
        VARSET_TP longLocals;
#endif
        VARSET_TP inOutLocals;
        VARSET_TP useDefLocals;
        // The register count for the non-FP LclVars that are read/written inside this loop
        int intLocalCount;
        // The register count for the FP LclVars that are read/written inside this loop
        int floatLocalCount;
        // The register count for the non-FP LclVars that are alive inside or across this loop
        int intInOutLocalCount;
        // The register count for the FP LclVars that are alive inside or across this loop
        int floatInOutLocalCount;
        // The register count for the non-FP expressions from inside this loop that have been hoisted
        int intHoistedExprCount;
        // The register count for the FP expressions from inside this loop that have been hoisted
        int floatHoistedExprCount;

        LoopStats(Compiler* compiler)
            : floatLocals(VarSetOps::MakeEmpty(compiler))
#ifndef TARGET_64BIT
            , longLocals(VarSetOps::MakeEmpty(compiler))
#endif
            , inOutLocals(VarSetOps::MakeEmpty(compiler))
            , useDefLocals(VarSetOps::MakeEmpty(compiler))
        {
        }
    };

    SsaOptimizer&        ssa;
    Compiler* const      compiler;
    ValueNumStore* const vnStore;
    LoopDsc* const       loopTable;
    unsigned const       loopCount;
    VNSet*               hoistedInCurrentLoop = nullptr;
    VNSet                hoistedInParentLoops;
    JitHashMap<ValueNum, bool> loopInvariantCache;
    LoopStats stats;
    unsigned  hoistedCount = 0;

public:
    LoopHoist(SsaOptimizer& ssa)
        : ssa(ssa)
        , compiler(ssa.GetCompiler())
        , vnStore(ssa.GetVNStore())
        , loopTable(ssa.GetLoopTable())
        , loopCount(ssa.GetLoopCount())
        , hoistedInParentLoops(compiler->getAllocator(CMK_LoopHoist))
        , loopInvariantCache(compiler->getAllocator(CMK_LoopHoist))
        , stats(compiler)
    {
    }

    bool Run();

private:
    void HoistLoopNest(unsigned loopNum);
    void HoistLoop(unsigned loopNum);
    bool IsHoistingProfitable(GenTree* expr, unsigned loopNum) const;
    void HoistLoopBlocks(unsigned loopNum, ArrayStack<BasicBlock*>& blocks);
    void HoistCandidate(GenTree* expr, unsigned loopNum);
    bool IsLoopInvariant(ValueNum vn, unsigned loopNum);
    void HoistExpr(GenTree* expr, unsigned loopNum);

    VNSet* GetHoistedInCurrentLoop()
    {
        if (hoistedInCurrentLoop == nullptr)
        {
            hoistedInCurrentLoop =
                new (compiler->getAllocator(CMK_LoopHoist)) VNSet(compiler->getAllocator(CMK_LoopHoist));
        }

        return hoistedInCurrentLoop;
    }

    VNSet* RemoveHoistedInCurrentLoop()
    {
        VNSet* result        = hoistedInCurrentLoop;
        hoistedInCurrentLoop = nullptr;
        return result;
    }
};

void LoopHoist::HoistExpr(GenTree* expr, unsigned loopNum)
{
    JITDUMPTREE(expr, "\nHoisting a copy of [%06u] into PreHeader for loop " FMT_LP " <" FMT_BB ".." FMT_BB ">:\n",
                expr->GetID(), loopNum, loopTable[loopNum].lpFirst->bbNum, loopTable[loopNum].lpBottom->bbNum);
    JITDUMP("\n");

    assert(!expr->OperIs(GT_LCL_DEF, GT_LCL_STORE, GT_LCL_STORE_FLD, GT_IND_STORE, GT_IND_STORE_OBJ, GT_IND_STORE_BLK));

    GenTree* hoistExpr = compiler->gtCloneExpr(expr, GTF_MAKE_CSE);
    assert(hoistExpr != expr);
    assert((hoistExpr->gtFlags & GTF_MAKE_CSE) != 0);

    vnStore->CopyLoopMemoryDependence(expr, hoistExpr);

    GenTree* hoist = compiler->gtUnusedValNode(hoistExpr);

    compiler->fgCreateLoopPreHeader(loopNum);

    BasicBlock* preHead = loopTable[loopNum].lpHead;
    assert(preHead->bbJumpKind == BBJ_NONE);

    // TODO-MIKE-Review: Is there anything to morph here? The hoisted tree
    // is a clone of another tree that was already morphed.
    compiler->moMorphBlock = preHead;
    hoist                  = compiler->gtMorphTree(hoist);

    Statement* hoistStmt = compiler->fgNewStmtAtEnd(preHead, hoist);
    compiler->gtSetCosts(hoistStmt->GetRootNode());
    compiler->gtSetStmtOrder(hoistStmt);

    hoistedCount++;

    JITDUMPTREE(hoist, "This hoisted copy placed in PreHeader (" FMT_BB "):\n", preHead->bbNum);

#if LOOP_HOIST_STATS
    if (!compiler->m_curLoopHasHoistedExpression)
    {
        compiler->m_loopsWithHoistedExpressions++;
        compiler->m_curLoopHasHoistedExpression = true;
    }

    compiler->m_totalHoistedExpressions++;
#endif
}

bool LoopHoist::Run()
{
    for (LclVarDsc* lcl : compiler->LivenessLocals())
    {
        if (varTypeIsFloating(lcl->GetType()))
        {
            VarSetOps::AddElemD(compiler, stats.floatLocals, lcl->GetLivenessBitIndex());
        }
#ifndef TARGET_64BIT
        else if (lcl->TypeIs(TYP_LONG))
        {
            VarSetOps::AddElemD(compiler, stats.longLocals, lcl->GetLivenessBitIndex());
        }
#endif
    }

    for (unsigned i = 0; i < loopCount; i++)
    {
        if (loopTable[i].IsRemoved())
        {
            continue;
        }

        if (loopTable[i].lpParent == NoLoopNum)
        {
            HoistLoopNest(i);
        }
    }

    return hoistedCount != 0;
}

void LoopHoist::HoistLoopNest(unsigned loopNum)
{
#if LOOP_HOIST_STATS
    compiler->m_curLoopHasHoistedExpression = false;
    compiler->m_loopsConsidered++;
#endif

    HoistLoop(loopNum);

    VNSet* hoistedInCurrentLoop = RemoveHoistedInCurrentLoop();

    if (loopTable[loopNum].lpChild == NoLoopNum)
    {
        return;
    }

    // Add the ones hoisted in this loop to "hoistedInParents" for any nested loops.
    if (hoistedInCurrentLoop != nullptr)
    {
        for (ValueNum vn : *hoistedInCurrentLoop)
        {
            INDEBUG(bool added =) hoistedInParentLoops.Add(vn);
            assert(added);
        }
    }

    for (unsigned child = loopTable[loopNum].lpChild; child != NoLoopNum; child = loopTable[child].lpSibling)
    {
        HoistLoopNest(child);
    }

    // Now remove them.
    if (hoistedInCurrentLoop != nullptr)
    {
        for (ValueNum vn : *hoistedInCurrentLoop)
        {
            hoistedInParentLoops.Remove(vn);
        }
    }
}

void LoopHoist::HoistLoop(unsigned loopNum)
{
    LoopDsc* loopDesc = &loopTable[loopNum];

    if (loopDesc->IsRemoved())
    {
        return;
    }

    if ((loopDesc->lpFlags & LPFLG_DO_WHILE) == 0)
    {
        return;
    }

    BasicBlock* head  = loopDesc->lpHead;
    BasicBlock* entry = loopDesc->lpEntry;

    // The loop-head must dominate the loop-entry.
    // TODO-CQ: Couldn't we make this true if it's not?
    if (!compiler->fgDominate(head, entry))
    {
        return;
    }

    if (!BasicBlock::sameTryRegion(head, entry))
    {
        return;
    }

    // We don't bother hoisting when inside of a catch block
    if ((entry->bbCatchTyp != BBCT_NONE) && (entry->bbCatchTyp != BBCT_FINALLY))
    {
        return;
    }

    BasicBlock* bottom = loopDesc->lpBottom;

    JITDUMP("optHoistLoopCode for loop " FMT_LP " <" FMT_BB ".." FMT_BB ">:\n"
            "  Loop body %s a call\n"
            "  Loop has %s\n",
            loopNum, entry->bbNum, bottom->bbNum, loopDesc->HasCall() ? "contains" : "does not contain",
            loopDesc->HasUniqueExit() ? "single exit" : "multiple exits");

    loopInvariantCache.Clear();

    VarSetOps::ClearD(compiler, stats.inOutLocals);
    VarSetOps::ClearD(compiler, stats.useDefLocals);

    for (BasicBlock* block : loopDesc->LoopBlocks())
    {
        if (block->GetLoopNum() == NoLoopNum)
        {
            // We encountered a block that was moved into the loop range (by fgReorderBlocks),
            // but not marked correctly as being inside the loop.
            // All done, no need to keep visiting more blocks.
            //
            // TODO-MIKE-Review: What about liveness?
            // And in general this case is dubious. Why wasn't the block marked correctly?
            // Is it a part of the loop or not? Why wasn't this fixed? Stupid JIT commenting
            // as usual, write a bunch of crap that doesn't actually explain anything.
            break;
        }

        VarSetOps::UnionD(compiler, stats.inOutLocals, block->bbLiveIn);
        VarSetOps::UnionD(compiler, stats.inOutLocals, block->bbLiveOut);

        VarSetOps::UnionD(compiler, stats.useDefLocals, block->bbVarUse);
        VarSetOps::UnionD(compiler, stats.useDefLocals, block->bbVarDef);
    }

    VARSET_TP loopLocals = VarSetOps::Alloc(compiler);
    VarSetOps::Intersection(compiler, loopLocals, stats.inOutLocals, stats.useDefLocals);

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("\n  USEDEF ");
        compiler->lvaDispVarSet(stats.useDefLocals);
        printf("\n");

        printf("\n  INOUT ");
        compiler->lvaDispVarSet(stats.inOutLocals);
        printf("\n");

        printf("\n  LOOPVARS ");
        compiler->lvaDispVarSet(loopLocals);
        printf("\n");
    }
#endif

    stats.intLocalCount      = VarSetOps::Count(compiler, loopLocals);
    stats.intInOutLocalCount = VarSetOps::Count(compiler, stats.inOutLocals);

#ifndef TARGET_64BIT
    if (!VarSetOps::IsEmpty(compiler, stats.longLocals))
    {
        // Since 64-bit variables take up two registers on 32-bit targets,
        // we increase the counts such that each LONG variable counts twice.

        VARSET_TP longLocals = VarSetOps::Alloc(compiler);
        VarSetOps::Intersection(compiler, longLocals, loopLocals, stats.longLocals);
        stats.intLocalCount += VarSetOps::Count(compiler, longLocals);

        VarSetOps::Intersection(compiler, longLocals, stats.inOutLocals, stats.longLocals);
        stats.intInOutLocalCount += VarSetOps::Count(compiler, longLocals);
    }
#endif

    if (!VarSetOps::IsEmpty(compiler, stats.floatLocals))
    {
        VARSET_TP floatLocals = VarSetOps::Alloc(compiler);
        VarSetOps::Intersection(compiler, floatLocals, loopLocals, stats.floatLocals);
        stats.floatLocalCount = VarSetOps::Count(compiler, floatLocals);
        stats.intLocalCount -= stats.floatLocalCount;

        VarSetOps::Intersection(compiler, floatLocals, stats.inOutLocals, stats.floatLocals);
        stats.floatInOutLocalCount = VarSetOps::Count(compiler, floatLocals);
        stats.intInOutLocalCount -= stats.floatInOutLocalCount;
    }
    else
    {
        stats.floatLocalCount      = 0;
        stats.floatInOutLocalCount = 0;
    }

    JITDUMP("intLocalCount %u, intInOutLocalCount %u, floatLocalCount %u, floatInOutLocalCount %u\n",
            stats.intLocalCount, stats.intInOutLocalCount, stats.floatLocalCount, stats.floatInOutLocalCount);

    stats.intHoistedExprCount   = 0;
    stats.floatHoistedExprCount = 0;

    // Find the set of definitely-executed blocks.
    // Ideally, the definitely-executed blocks are the ones that post-dominate the entry block.
    // Until we have post-dominators, we'll special-case for single-exit blocks.
    ArrayStack<BasicBlock*> defExec(compiler->getAllocator(CMK_LoopHoist));

    if (loopDesc->HasUniqueExit())
    {
        assert(loopDesc->lpExit != nullptr);

        BasicBlock* cur = loopDesc->lpExit;

        // Push dominators, until we reach "entry" or exit the loop.
        while ((cur != nullptr) && loopDesc->lpContains(cur) && (cur != loopDesc->lpEntry))
        {
            defExec.Push(cur);
            cur = cur->bbIDom;
        }

        // If we didn't reach the entry block, give up and *just* push the entry block.
        if (cur != loopDesc->lpEntry)
        {
            defExec.Clear();
        }

        defExec.Push(loopDesc->lpEntry);
    }
    else
    {
        // More than one exit.
        // We'll assume that only the entry block is definitely executed.
        // We could in the future do better.
        defExec.Push(loopDesc->lpEntry);
    }

    HoistLoopBlocks(loopNum, defExec);
}

bool LoopHoist::IsHoistingProfitable(GenTree* tree, unsigned loopNum) const
{
    int availRegCount;
    int intHoistedExprCount;
    int loopVarCount;
    int varInOutCount;

    if (varTypeIsFloating(tree->GetType()))
    {
        intHoistedExprCount = stats.floatHoistedExprCount;
        loopVarCount        = stats.floatLocalCount;
        varInOutCount       = stats.floatInOutLocalCount;
        availRegCount       = CNT_CALLEE_SAVED_FLOAT;

        if (!loopTable[loopNum].HasCall())
        {
            availRegCount += CNT_CALLEE_TRASH_FLOAT - 1;
        }

#ifdef TARGET_ARM
        // For ARM each double takes two FP registers
        // For now on ARM we won't track singles/doubles
        // and instead just assume that we always have doubles.
        availRegCount /= 2;
#endif
    }
    else
    {
        intHoistedExprCount = stats.intHoistedExprCount;
        loopVarCount        = stats.intLocalCount;
        varInOutCount       = stats.intInOutLocalCount;
        availRegCount       = CNT_CALLEE_SAVED - 1;

        if (!loopTable[loopNum].HasCall())
        {
            availRegCount += CNT_CALLEE_TRASH - 1;
        }

#ifndef TARGET_64BIT
        if (tree->TypeIs(TYP_LONG))
        {
            availRegCount = (availRegCount + 1) / 2;
        }
#endif
    }

    // decrement the availRegCount by the count of expression that we have already hoisted.
    availRegCount -= intHoistedExprCount;

    // the variables that are read/written inside the loop should
    // always be a subset of the InOut variables for the loop
    assert(loopVarCount <= varInOutCount);

    // When loopVarCount >= availRegCount we believe that all of the available registers will get
    // used to hold locals inside the loop. This pessimistically assumes that each loop local has
    // a conflicting lifetime with every other loop local. For this case we will hoist the
    // expression only if is profitable to place it in a stack home location as we believe it will
    // be placed in the stack or one of the other loop local will be spilled into the stack.

    if (loopVarCount >= availRegCount)
    {
        if (tree->GetCostEx() < 2 * IND_COST_EX)
        {
            return false;
        }
    }

    // When varInOutCount < availRegCount we are know that there are some available register(s) when
    // we enter the loop body. When varInOutCount == availRegCount there often will be a register
    // available when we enter the loop body, since a loop often defines a local on exit or there is
    // often at least one local that is worth spilling to the stack to make way for this hoisted
    // expression. So we are willing hoist an expression with GetCostEx() == MinCseCost

    if (varInOutCount > availRegCount)
    {
        // Don't hoist expressions that barely meet CSE cost requirements
        if (tree->GetCostEx() <= SsaOptimizer::MinCseCost + 1)
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
        GenTree* node;

    public:
        bool hoistable      = false;
        bool cctorDependent = false;
        bool invariant      = false;

        Value(GenTree* node) : node(node)
        {
        }

        GenTree* Node()
        {
            return node;
        }
    };

    SsaOptimizer&     ssa;
    ArrayStack<Value> m_valueStack;
    bool              m_beforeSideEffect = true;
    unsigned          m_loopNum;
    LoopHoist*        m_loopHoist;

    bool IsNodeHoistable(GenTree* node) const
    {
        return !node->TypeIs(TYP_STRUCT) && !node->HasAnySideEffect(GTF_ASG) && ssa.IsCseCandidate(node);
    }

    bool IsTreeVNInvariant(GenTree* tree) const
    {
        // Even though VN is invariant in the loop (say a constant) its value may depend on position
        // of tree, so for loop hoisting we must also check that any memory read by tree
        // is also invariant in the loop.

        return m_loopHoist->IsLoopInvariant(tree->GetLiberalVN(), m_loopNum) && IsTreeLoopMemoryInvariant(tree);
    }

    // Determine if the value number of an expression is dependent on the expression being
    // executed within the current loop.
    // Calls are optimistically assumed to be invariant.
    // Caller must do their own analysis for these tree types.
    bool IsTreeLoopMemoryInvariant(GenTree* tree) const
    {
        if (tree->IsCall())
        {
            // Calls are handled specially by hoisting, and loop memory dependence
            // must be checked by other means.
            return true;
        }

        if (BasicBlock* loopEntryBlock = ssa.GetVNStore()->GetLoopMemoryBlock(tree))
        {
            ValueNum loopMemoryVN = loopEntryBlock->memoryEntryDef->vn;

            if (!m_loopHoist->IsLoopInvariant(loopMemoryVN, m_loopNum))
            {
                return false;
            }
        }

        return true;
    }

public:
    enum
    {
        DoPreOrder        = true,
        DoPostOrder       = true,
        UseExecutionOrder = true,
    };

    LoopHoistTreeVisitor(SsaOptimizer& ssa, unsigned loopNum, LoopHoist* loopHoist)
        : ssa(ssa)
        , m_valueStack(ssa.GetCompiler()->getAllocator(CMK_LoopHoist))
        , m_loopNum(loopNum)
        , m_loopHoist(loopHoist)
    {
    }

    void HoistBlock(BasicBlock* block)
    {
        for (Statement* stmt : block->NonPhiStatements())
        {
            WalkTree(stmt->GetRootNodePointer(), nullptr);
            assert(m_valueStack.TopRef().Node() == stmt->GetRootNode());

            if (m_valueStack.TopRef().hoistable)
            {
                m_loopHoist->HoistCandidate(stmt->GetRootNode(), m_loopNum);
            }

            m_valueStack.Clear();
        }

        // Only unconditionally executed blocks in the loop are visited (see HoistLoop)
        // so after we're done visiting the first block we need to assume the worst, that the
        // blocks that are not visited have side effects.
        m_beforeSideEffect = false;
    }

    GenTreeWalkResult PreOrderVisit(GenTree** use, GenTree* user)
    {
        GenTree* node = *use;
        m_valueStack.Emplace(node);
        return GenTreeWalkResult::Continue;
    }

    GenTreeWalkResult PostOrderVisit(GenTree** use, GenTree* user)
    {
        GenTree* tree = *use;

        if (tree->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD))
        {
            return GenTreeWalkResult::Continue;
        }

        if (GenTreeLclUse* use = tree->IsLclUse())
        {
            // TODO-MIKE-Cleanup: Unreachable blocks aren't properly removed (see Runtime_57061_2).
            // Such blocks may or may not be traversed by various JIT phases - SSA builder does not
            // traverse them but this code does and ends up asserting due to missing SSA numbers.
            // Well, at least that's why this probably checks for NoSsaNum, but it seems unlikely
            // that loop hoisting would hit dead code. We'll see.

            // TODO-CQ: This VN invariance check should not be necessary and in some cases it is conservative -
            // it is possible that the SSA def is outside the loop but VN does not understand what the node is
            // doing (e.g. LCL_LOAD_FLD-based type reinterpretation) and assigns a "new, unique VN" to the node.
            // This VN is associated with the block where the node is, a loop block, and thus the VN is considered
            // to not be invariant.
            // On the other hand, it is possible for a SSA def to be inside the loop yet the use to be invariant,
            // if the defining expression is also invariant. In such a case the VN invariance would help but it is
            // blocked by the SSA invariance check.

            if (!ssa.GetLoop(m_loopNum)->lpContains(use->GetDef()->GetBlock()) && IsTreeVNInvariant(tree))
            {
                Value& top = m_valueStack.TopRef();
                assert(top.Node() == tree);
                top.invariant = true;
                // In general it doesn't make sense to hoist a local node but there are exceptions,
                // for example LCL_LOAD_FLD nodes (because then the variable cannot be enregistered
                // and the node always turns into a memory access).
                top.hoistable = IsNodeHoistable(tree);
            }

            return GenTreeWalkResult::Continue;
        }

        // Initclass CLS_VAR_ADDRs and CNS_INT are the base cases of cctor dependent trees.
        // In the CNS_INT case, it's of course the dereference, rather than the constant itself,
        // that is truly dependent on the cctor. So a more precise approach would be to separately
        // propagate isCctorDependent and isAddressWhoseDereferenceWouldBeCctorDependent, but we
        // don't for simplicity/throughput; the constant itself would be considered non-hoistable
        // anyway, since cseIsCandidate returns false for constants.
        bool isCctorDependent = (tree->OperIs(GT_CLS_VAR_ADDR) && ((tree->gtFlags & GTF_CLS_VAR_INITCLASS) != 0)) ||
                                (tree->OperIs(GT_CNS_INT) && ((tree->gtFlags & GTF_ICON_INITCLASS) != 0));
        bool     isInvariant          = true;
        bool     hasHoistableChildren = false;
        unsigned childCount;

        for (childCount = 0; m_valueStack.TopRef(childCount).Node() != tree; childCount++)
        {
            Value& child = m_valueStack.TopRef(childCount);

            if (child.hoistable)
            {
                hasHoistableChildren = true;
            }

            if (!child.invariant)
            {
                isInvariant = false;
            }

            if (child.cctorDependent)
            {
                // Normally, a parent of a cctor-dependent tree is also cctor-dependent.
                isCctorDependent = true;

                // Check for the case where we can stop propagating cctor-dependent upwards.
                if (tree->OperIs(GT_COMMA) && (child.Node() == tree->AsOp()->GetOp(1)))
                {
                    if (GenTreeCall* call = tree->AsOp()->GetOp(0)->IsCall())
                    {
                        if (call->IsHelperCall() && HelperCallProperties::MayRunCctor(call->GetHelperFunc()))
                        {
                            // Hoisting the comma is ok because it would hoist the initialization along
                            // with the static field reference.
                            isCctorDependent = false;
                            // Hoisting the static field without hoisting the initialization would be
                            // incorrect, make sure we consider the field (which we flagged as
                            // cctor-dependent) non-hoistable.
                            noway_assert(!child.hoistable);
                        }
                    }
                }
            }
        }

        // If all the children of "tree" are hoistable, then "tree" itself can be hoisted,
        // unless it has a static var reference that can't be hoisted past its cctor call.
        bool isHoistable = isInvariant && !isCctorDependent;

        // But we must see if anything else prevents "tree" from being hoisted.
        if (isInvariant)
        {
            if (isHoistable)
            {
                isHoistable = IsNodeHoistable(tree);
            }

            // If it's a call, it must be a helper call, and be pure.
            // Further, if it may run a cctor, it must be labeled as "Hoistable"
            // (meaning it won't run a cctor because the class is not precise-init).
            if (isHoistable && tree->IsCall())
            {
                GenTreeCall* call = tree->AsCall();

                if (CorInfoHelpFunc helper = call->IsHelperCall())
                {
                    if (!HelperCallProperties::IsPure(helper) ||
                        (HelperCallProperties::MayRunCctor(helper) && !call->IsHoistable()))
                    {
                        isHoistable = false;
                    }
                }
                else
                {
                    isHoistable = false;
                }
            }

            if (isHoistable)
            {
                if (!m_beforeSideEffect)
                {
                    // For now, we give up on an expression that might raise an exception if it is after the
                    // first possible global side effect (and we assume we're after that if we're not in the first
                    // block).
                    // TODO-CQ: this is when we might do loop cloning.
                    if (tree->HasAnySideEffect(GTF_EXCEPT))
                    {
                        isHoistable = false;
                    }
                }
            }

            // Is the value of the whole tree loop invariant?
            isInvariant = IsTreeVNInvariant(tree);

            // Is the value of the whole tree loop invariant?
            if (!isInvariant)
            {
                // Here we have a tree that is not loop invariant and we thus cannot hoist
                isHoistable = false;
            }
        }

        // Next check if we need to set 'm_beforeSideEffect' to false.
        // If we have already set it to false then we can skip these checks
        if (m_beforeSideEffect)
        {
            // Is the value of the whole tree loop invariant?
            if (!isInvariant)
            {
                // We have a tree that is not loop invariant and we thus cannot hoist
                assert(!isHoistable);

                // Check if we should clear m_beforeSideEffect.
                // If 'tree' can throw an exception then we need to set m_beforeSideEffect to false.
                // Note that calls are handled below
                if (tree->OperMayThrow(ssa.GetCompiler()) && !tree->IsCall())
                {
                    m_beforeSideEffect = false;
                }
            }

            // In the section below, we only care about memory side effects.  We assume that expressions will
            // be hoisted so that they are evaluated in the same order as they would have been in the loop,
            // and therefore throw exceptions in the same order.
            if (GenTreeCall* call = tree->IsCall())
            {
                // If it's a call, it must be a helper call that does not mutate the heap.
                // Further, if it may run a cctor, it must be labeled as "Hoistable"
                // (meaning it won't run a cctor because the class is not precise-init).

                if (CorInfoHelpFunc helper = call->IsHelperCall())
                {
                    if (HelperCallProperties::MutatesHeap(helper) ||
                        (HelperCallProperties::MayRunCctor(helper) && !call->IsHoistable()))
                    {
                        m_beforeSideEffect = false;
                    }

                    if (!isInvariant)
                    {
                        assert(!isHoistable);

                        if (!HelperCallProperties::NoThrow(helper))
                        {
                            m_beforeSideEffect = false;
                        }
                    }
                }
                else
                {
                    m_beforeSideEffect = false;
                }
            }
            else if (tree->OperIs(GT_IND_STORE, GT_IND_STORE_OBJ, GT_IND_STORE_BLK))
            {
                m_beforeSideEffect = false;
            }
            else if (tree->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD))
            {
                LclVarDsc* lcl = tree->AsLclRef()->GetLcl();

                if (lcl->IsAddressExposed()
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
                    && !lcl->lvIsImplicitByRefArgTemp
#endif
                    )
                {
                    m_beforeSideEffect = false;
                }
            }
            else if (tree->OperIsAtomicOp() || tree->OperIs(GT_MEMORYBARRIER))
            {
                // If this node is a MEMORYBARRIER or an Atomic operation
                // then don't hoist and stop any further hoisting after this node
                isHoistable        = false;
                m_beforeSideEffect = false;
            }
        }

        // If this 'tree' is hoistable then we return and the caller will
        // decide to hoist it as part of larger hoistable expression.
        if (!isHoistable && hasHoistableChildren)
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
            // Note that the hasHoistableChildren check avoids unnecessary stack traversing
            // and also prevents hoisting trees too early. If the current tree is not hoistable
            // and it doesn't have any hoistable children then there's no point in hoisting any
            // other trees. Doing so would interfere with the cctor dependent case, where the
            // cctor dependent node is initially not hoistable and may become hoistable later,
            // when its parent comma node is visited.

            for (unsigned i = 0; i < m_valueStack.Size(); i++)
            {
                Value& value = m_valueStack.BottomRef(i);

                if (value.hoistable)
                {
                    assert(value.Node() != tree);

                    // Don't hoist this tree again.
                    value.hoistable = false;
                    value.invariant = false;

                    m_loopHoist->HoistCandidate(value.Node(), m_loopNum);
                }
            }
        }

        m_valueStack.Pop(childCount);

        Value& top = m_valueStack.TopRef();
        assert(top.Node() == tree);
        top.hoistable      = isHoistable;
        top.cctorDependent = isCctorDependent;
        top.invariant      = isInvariant;

        return GenTreeWalkResult::Continue;
    }
};

// Hoist invariant expression out of the loop.
// "blocks" contains the definitely-executed blocks in the loop, in the execution
// order, starting with the loop entry block on top of the stack.
void LoopHoist::HoistLoopBlocks(unsigned loopNum, ArrayStack<BasicBlock*>& blocks)
{
    LoopDsc& loop = loopTable[loopNum];
    assert(blocks.Top() == loop.lpEntry);

    LoopHoistTreeVisitor visitor(ssa, loopNum, this);

    while (!blocks.Empty())
    {
        BasicBlock* block       = blocks.Pop();
        weight_t    blockWeight = block->getBBWeight(compiler);

        JITDUMP("    HoistLoopBlocks " FMT_BB " (weight=%6s) of loop " FMT_LP " <" FMT_BB ".." FMT_BB
                ">, firstBlock is %s\n",
                block->bbNum, refCntWtd2str(blockWeight), loopNum, loop.lpFirst->bbNum, loop.lpBottom->bbNum,
                dspBool(block == loop.lpEntry));

        if (blockWeight < (BB_UNITY_WEIGHT / 10))
        {
            JITDUMP("      block weight is too small to perform hoisting.\n");
            continue;
        }

        visitor.HoistBlock(block);
    }
}

void LoopHoist::HoistCandidate(GenTree* tree, unsigned loopNum)
{
    assert(loopNum != NoLoopNum);

    // It must pass the hoistable profitability tests for this loop level
    if (!IsHoistingProfitable(tree, loopNum))
    {
        return;
    }

    if (hoistedInParentLoops.Contains(tree->GetLiberalVN()))
    {
        return;
    }

    if (GetHoistedInCurrentLoop()->Contains(tree->GetLiberalVN()))
    {
        return;
    }

    HoistExpr(tree, loopNum);

    if (!varTypeIsFloating(tree->GetType()))
    {
        stats.intHoistedExprCount++;

#ifndef TARGET_64BIT
        if (tree->TypeIs(TYP_LONG))
        {
            stats.intHoistedExprCount++;
        }
#endif
    }
    else
    {
        stats.floatHoistedExprCount++;
    }

    GetHoistedInCurrentLoop()->Add(tree->GetLiberalVN());
}

bool LoopHoist::IsLoopInvariant(ValueNum vn, unsigned loopNum)
{
    if (vn == NoVN)
    {
        return false;
    }

    if (vnStore->IsVNConstant(vn))
    {
        return true;
    }

    if (bool* cached = loopInvariantCache.Find(vn))
    {
        return *cached;
    }

    bool         invariant = true;
    VNFuncApp    funcApp;
    const VNFunc func = vnStore->GetVNFunc(vn, &funcApp);

    if ((func == VNF_Phi) || (func == VNF_MemoryPhi))
    {
        invariant = !compiler->optLoopContains(loopNum, vnStore->ConstantHostPtr<BasicBlock>(funcApp[1])->GetLoopNum());
    }
    else if (func == VNF_Unique)
    {
        invariant = !compiler->optLoopContains(loopNum, funcApp[0] - 1);
    }
    else if (func != VNF_None)
    {
        // TODO-CQ: We need to either make sure that *all* VN functions always take VN args,
        // or else have a list of arg positions to exempt, as implicitly constant.
        if (func == VNF_MapStore)
        {
            assert(funcApp.arity == 4);

            invariant     = !compiler->optLoopContains(loopNum, funcApp[3] - 1);
            funcApp.arity = 3;
        }

        for (unsigned i = 0; i < funcApp.arity; i++)
        {
            if (!IsLoopInvariant(funcApp[i], loopNum))
            {
                invariant = false;
                break;
            }
        }
    }

    loopInvariantCache.Add(vn, invariant);

    return invariant;
}

// Creates a pre-header block for the given loop - a preheader is a BBJ_NONE
// header. The pre-header will replace the current head in the loop table.
// The loop has to be a do-while loop. Thus, all blocks dominated by lpHead
// will also be dominated by the loop-top, head->bbNext.
void Compiler::fgCreateLoopPreHeader(unsigned loopNum)
{
    LoopDsc& loopDesc = optLoopTable[loopNum];

    assert((loopDesc.lpFlags & LPFLG_DO_WHILE) != 0);

    if ((loopDesc.lpFlags & LPFLG_HAS_PREHEAD) != 0)
    {
        return;
    }

    BasicBlock* head  = loopDesc.lpHead;
    BasicBlock* top   = loopDesc.lpTop;
    BasicBlock* entry = loopDesc.lpEntry;

    // if 'entry' and 'head' are in different try regions then we won't be able to hoist
    if (!BasicBlock::sameTryRegion(head, entry))
    {
        return;
    }

    noway_assert(fgDominate(head, entry));
    assert(top == entry);

    BasicBlock* preHead = bbNewBasicBlock(BBJ_NONE);
    preHead->bbFlags |= BBF_INTERNAL | BBF_LOOP_PREHEADER;

    // Must set IL code offset
    preHead->bbCodeOffs = top->bbCodeOffs;

    // Set the default value of the preHead weight in case we don't have
    // valid profile data and since this blocks weight is just an estimate
    // we clear any BBF_PROF_WEIGHT flag that we may have picked up from head.
    preHead->inheritWeight(head);
    preHead->bbFlags &= ~BBF_PROF_WEIGHT;

    // Copy the bbReach set from head for the new preHead block
    preHead->bbReach = BlockSetOps::MakeCopy(this, head->bbReach);
    // Also include 'head' in the preHead bbReach set
    BlockSetOps::AddElemD(this, preHead->bbReach, head->bbNum);

    JITDUMP("\nCreated PreHeader (" FMT_BB ") for loop " FMT_LP " (" FMT_BB " - " FMT_BB "), with weight = %s\n",
            preHead->bbNum, loopNum, top->bbNum, loopDesc.lpBottom->bbNum, refCntWtd2str(preHead->getBBWeight(this)));

    // The preheader block is part of the containing loop (if any).
    preHead->SetLoopNum(loopDesc.lpParent);

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
                weight_t loopEnteredCount;
                weight_t loopSkippedCount;

                if (fgHaveValidEdgeWeights)
                {
                    FlowEdge* edgeToNext = fgGetPredForBlock(head->bbNext, head);
                    FlowEdge* edgeToJump = fgGetPredForBlock(head->bbJumpDest, head);
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

                weight_t loopTakenRatio = loopEnteredCount / (loopEnteredCount + loopSkippedCount);

                JITDUMP("%s; loopEnterCount " FMT_WT " loopSkipCount " FMT_WT " taken ratio " FMT_WT "\n",
                        fgHaveValidEdgeWeights ? "valid edge weights" : "no edge weights", loopEnteredCount,
                        loopSkippedCount, loopTakenRatio);

                // Calculate a good approximation of the preHead's block weight
                weight_t preHeadWeight = (head->bbWeight * loopTakenRatio);
                preHead->setBBProfileWeight(preHeadWeight);
                noway_assert(!preHead->isRunRarely());
            }
        }
    }

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

    loopDesc.lpHead = preHead;
    loopDesc.lpFlags |= LPFLG_HAS_PREHEAD;

    // The new block becomes the 'head' of the loop - update bbRefs and bbPreds
    // All predecessors of 'beg', (which is the entry in the loop)
    // now have to jump to 'preHead', unless they are dominated by 'head'

    preHead->bbRefs                 = 0;
    FlowEdge* const edgeToPreHeader = fgAddRefPred(preHead, head);
    edgeToPreHeader->setEdgeWeights(preHead->bbWeight, preHead->bbWeight, preHead);
    bool checkNestedLoops = false;

    for (BasicBlock* const predBlock : top->PredBlocks())
    {
        if (fgDominate(top, predBlock))
        {
            // note: if 'top' dominates predBlock, 'head' dominates predBlock too
            // (we know that 'head' dominates 'top'), but using 'top' instead of
            // 'head' in the test allows us to not enter here if 'predBlock == head'

            if (predBlock != loopDesc.lpBottom)
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
    FlowEdge* edgeFromPreHeader = fgAddRefPred(top, preHead);
    edgeFromPreHeader->setEdgeWeights(preHead->bbWeight, preHead->bbWeight, top);

    // If we found at least one back-edge in the flowgraph pointing to the top/entry of the loop
    // (other than the back-edge of the loop we are considering) then we likely have nested
    // do-while loops with the same entry block and inserting the preheader block changes the head
    // of all the nested loops. Now we will update this piece of information in the loop table, and
    // mark all nested loops as having a preheader (the preheader block can be shared among all nested
    // do-while loops with the same entry block).
    if (checkNestedLoops)
    {
        LoopDsc* loopTable = optLoopTable;

        for (unsigned i = 0, count = optLoopCount; i < count; i++)
        {
            if (loopTable[i].lpHead == head)
            {
                noway_assert(i != loopNum); // loopDesc->lpHead was already changed from 'head' to 'preHead'
                noway_assert(loopTable[i].lpEntry == top);

                optUpdateLoopHead(i, head, preHead);
                loopTable[i].lpFlags |= LPFLG_HAS_PREHEAD;

                JITDUMP("Same PreHeader (" FMT_BB ") can be used for loop " FMT_LP " (" FMT_BB " - " FMT_BB ")\n\n",
                        preHead->bbNum, i, top->bbNum, loopTable[i].lpBottom->bbNum);
            }
        }
    }
}

PhaseStatus SsaOptimizer::DoLoopHoist()
{
    // TODO-MIKE-Cleanup: Only CSE needs costs for all trees, loop hoisting only needs
    // costs for candidates, which are far fewer. At least in theory, CSE could compute
    // costs on the fly, as it traverses trees to find CSE candidates. Though right now
    // it uses the linear order for traversal...
    for (BasicBlock* block : compiler->Blocks())
    {
        for (Statement* stmt : block->Statements())
        {
            compiler->gtSetCosts(stmt->GetRootNode());
        }
    }

    if (loopCount == 0)
    {
        return PhaseStatus::MODIFIED_NOTHING;
    }

    LoopHoist hoist(*this);
    return hoist.Run() ? PhaseStatus::MODIFIED_EVERYTHING : PhaseStatus::MODIFIED_NOTHING;
}
