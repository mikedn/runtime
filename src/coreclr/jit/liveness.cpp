// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "lower.h"

void Compiler::fgMarkUseDef(LivenessState& state, GenTreeLclVarCommon* node)
{
    assert(node->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    LclVarDsc* lcl = lvaGetDesc(node);

    assert(!lcl->IsAddressExposed());

    // We should never encounter a reference to a local that has a zero ref count.
    // TODO-MIKE-Review: It's not clear why promotion makes a difference.
    if ((lcl->lvRefCnt() == 0) && !lcl->IsPromoted())
    {
        JITDUMP("Found reference to V%02u with zero refCnt.\n", node->GetLclNum());
        assert(!"We should never encounter a reference to a lclVar that has a zero refCnt.");
        lcl->SetRefCount(1);
    }

    const bool isDef = node->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD);
    const bool isUse = !isDef || (node->OperIs(GT_STORE_LCL_FLD) && node->IsPartialLclFld(this));

    assert(isDef || isUse);

    if (lcl->HasLiveness())
    {
        if (isUse && !VarSetOps::IsMember(this, state.fgCurDefSet, lcl->lvVarIndex))
        {
            VarSetOps::AddElemD(this, state.fgCurUseSet, lcl->lvVarIndex);
        }

        if (isDef)
        {
            VarSetOps::AddElemD(this, state.fgCurDefSet, lcl->lvVarIndex);
        }

        return;
    }

    if (!lcl->IsPromoted())
    {
        return;
    }

    unsigned lclOffset    = 0;
    unsigned lclEndOffset = lcl->TypeIs(TYP_STRUCT) ? lcl->GetLayout()->GetSize() : varTypeSize(lcl->GetType());

    if (GenTreeLclFld* lclFld = node->IsLclFld())
    {
        lclOffset    = lclFld->GetLclOffs();
        lclEndOffset = lclOffset + (lclFld->TypeIs(TYP_STRUCT) ? lclFld->GetLayout(this)->GetSize()
                                                               : varTypeSize(lclFld->GetType()));
    }

    for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
    {
        LclVarDsc* fieldLcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

        assert(!fieldLcl->TypeIs(TYP_STRUCT));

        unsigned fieldOffset    = fieldLcl->GetPromotedFieldOffset();
        unsigned fieldEndOffset = fieldOffset + varTypeSize(fieldLcl->GetType());
        bool     partialOverlap = (fieldOffset < lclEndOffset) && (fieldEndOffset > lclOffset);

        if (!partialOverlap)
        {
            continue;
        }

        if (!fieldLcl->HasLiveness())
        {
            continue;
        }

        bool totalOverlap = (lclOffset <= fieldOffset) && (fieldEndOffset <= lclEndOffset);
        bool isFieldUse   = !isDef || !totalOverlap;

        if (isFieldUse && !VarSetOps::IsMember(this, state.fgCurDefSet, fieldLcl->GetLivenessBitIndex()))
        {
            VarSetOps::AddElemD(this, state.fgCurUseSet, fieldLcl->GetLivenessBitIndex());
        }

        if (isDef)
        {
            VarSetOps::AddElemD(this, state.fgCurDefSet, fieldLcl->GetLivenessBitIndex());
        }
    }
}

void Compiler::fgLocalVarLivenessUntracked()
{
    assert(lvaTrackedCount == 0);

    for (BasicBlock* const block : Blocks())
    {
        block->bbVarUse  = VarSetOps::UninitVal();
        block->bbVarDef  = VarSetOps::UninitVal();
        block->bbLiveIn  = VarSetOps::UninitVal();
        block->bbLiveOut = VarSetOps::UninitVal();

        block->bbMemoryUse     = false;
        block->bbMemoryDef     = false;
        block->bbMemoryLiveIn  = false;
        block->bbMemoryLiveOut = false;
    }

    if (!compRationalIRForm)
    {
        // Even if there are no tracked locals we still use memory liveness.
        fgPerBlockLocalVarLiveness();
        fgLiveVarAnalysis();
    }

    fgInterBlockLocalVarLivenessUntracked();

    // Since there are no tracked locals liveness basically never runs.
    INDEBUG(fgLocalVarLivenessDone = false;)
}

void Compiler::phSsaLiveness()
{
    assert(opts.OptimizationEnabled());
    DBEXEC(verbose, lvaTableDump());

    lvaMarkLivenessTrackedLocals();
    fgLocalVarLiveness();

    DBEXEC(verbose, lvaTableDump());
}

void Compiler::fgLocalVarLiveness()
{
    assert(opts.OptimizationEnabled());

    // TODO-MIKE-Review: See if we can simply reset these during liveness computation
    // (e.g. if it's not live in then set it to false).
    for (unsigned lclNum = 0; lclNum < lvaCount; ++lclNum)
    {
        lvaTable[lclNum].lvMustInit = false;
    }

    if (lvaTrackedCount == 0)
    {
        fgLocalVarLivenessUntracked();
        return;
    }

    for (BasicBlock* const block : Blocks())
    {
        block->bbVarUse  = VarSetOps::MakeEmpty(this);
        block->bbVarDef  = VarSetOps::MakeEmpty(this);
        block->bbLiveIn  = VarSetOps::MakeEmpty(this);
        block->bbLiveOut = VarSetOps::MakeEmpty(this);

        block->bbMemoryUse     = false;
        block->bbMemoryDef     = false;
        block->bbMemoryLiveIn  = false;
        block->bbMemoryLiveOut = false;
    }

    for (bool changed = true; changed;)
    {
        if (compRationalIRForm)
        {
            fgPerBlockLocalVarLivenessLIR();
        }
        else
        {
            fgPerBlockLocalVarLiveness();
        }

        fgLiveVarAnalysis();
        changed = fgInterBlockLocalVarLiveness();
    }

    INDEBUG(fgLocalVarLivenessDone = true;)
}

void Compiler::livInitNewBlock(BasicBlock* block)
{
    if (lvaTrackedCount != 0)
    {
        block->bbVarUse  = VarSetOps::MakeEmpty(this);
        block->bbVarDef  = VarSetOps::MakeEmpty(this);
        block->bbLiveIn  = VarSetOps::MakeEmpty(this);
        block->bbLiveOut = VarSetOps::MakeEmpty(this);
    }

    block->bbMemoryUse     = false;
    block->bbMemoryDef     = false;
    block->bbMemoryLiveIn  = false;
    block->bbMemoryLiveOut = false;
}

void Compiler::fgPerNodeLocalVarLiveness(LivenessState& state, GenTree* tree)
{
    switch (tree->GetOper())
    {
        case GT_LCL_VAR:
        case GT_LCL_FLD:
            if (lvaGetDesc(tree->AsLclVarCommon())->IsAddressExposed())
            {
                state.fgCurMemoryUse = true;
                break;
            }

            fgMarkUseDef(state, tree->AsLclVarCommon());
            break;

        case GT_LCL_ADDR:
            assert(lvaGetDesc(tree->AsLclAddr())->IsAddressExposed());
            break;

        case GT_IND:
        case GT_OBJ:
        case GT_BLK:
            // For Volatile indirection, first mutate GcHeap/ByrefExposed
            // see comments in ValueNum.cpp (under case GT_IND)
            // This models Volatile reads as def-then-use of memory.
            // and allows for a CSE of a subsequent non-volatile read
            if ((tree->gtFlags & GTF_IND_VOLATILE) != 0)
            {
                // For any Volatile indirection, we must handle it as a memory def
                state.fgCurMemoryDef = true;
            }

            if (GenTreeLclAddr* lclNode = tree->AsIndir()->GetAddr()->SkipComma()->IsLocalAddrExpr())
            {
                assert(lvaGetDesc(lclNode)->IsAddressExposed());
            }

            state.fgCurMemoryUse = true;
            break;

        // We'll assume these are use-then-defs of memory.
        case GT_LOCKADD:
        case GT_XORR:
        case GT_XAND:
        case GT_XADD:
        case GT_XCHG:
        case GT_CMPXCHG:
        case GT_COPY_BLK:
        case GT_INIT_BLK:
            state.fgCurMemoryUse   = true;
            state.fgCurMemoryDef   = true;
            state.fgCurMemoryHavoc = true;
            break;

        case GT_MEMORYBARRIER:
            // Simliar to any Volatile indirection, we must handle this as a definition of GcHeap/ByrefExposed
            state.fgCurMemoryDef = true;
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
        {
            GenTreeHWIntrinsic* hwIntrinsicNode = tree->AsHWIntrinsic();

            // We can't call vnClearMemory unless the block has recorded a MemoryDef
            //
            if (hwIntrinsicNode->OperIsMemoryStore())
            {
                state.fgCurMemoryDef = true;
            }
            if (hwIntrinsicNode->OperIsMemoryLoad())
            {
                state.fgCurMemoryUse = true;
            }
            break;
        }
#endif

        // For now, all calls read/write GcHeap/ByrefExposed, writes in their entirety.  Might tighten this case later.
        case GT_CALL:
        {
            GenTreeCall* call    = tree->AsCall();
            bool         modHeap = true;
            if (call->gtCallType == CT_HELPER)
            {
                CorInfoHelpFunc helpFunc = eeGetHelperNum(call->gtCallMethHnd);

                if (!s_helperCallProperties.MutatesHeap(helpFunc) && !s_helperCallProperties.MayRunCctor(helpFunc))
                {
                    modHeap = false;
                }
            }
            if (modHeap)
            {
                state.fgCurMemoryUse   = true;
                state.fgCurMemoryDef   = true;
                state.fgCurMemoryHavoc = true;
            }
            break;
        }

        case GT_STORE_LCL_VAR:
        case GT_STORE_LCL_FLD:
            if (lvaGetDesc(tree->AsLclVarCommon())->IsAddressExposed())
            {
                state.fgCurMemoryDef = true;
                break;
            }

            fgMarkUseDef(state, tree->AsLclVarCommon());
            break;

        case GT_STOREIND:
        case GT_STORE_OBJ:
        case GT_STORE_BLK:
            if (GenTreeLclAddr* lclNode = tree->AsIndir()->GetAddr()->SkipComma()->IsLocalAddrExpr())
            {
                assert(lvaGetDesc(lclNode)->IsAddressExposed());
            }

            state.fgCurMemoryDef = true;
            break;

        default:
            assert(!tree->OperIs(GT_ASG, GT_QMARK));
            break;
    }
}

void Compiler::fgPerBlockLocalVarLiveness()
{
    assert(!compRationalIRForm);

    for (BasicBlock* block : Blocks())
    {
        LivenessState state;

        state.fgCurUseSet      = block->bbVarUse;
        state.fgCurDefSet      = block->bbVarDef;
        state.fgCurMemoryUse   = false;
        state.fgCurMemoryDef   = false;
        state.fgCurMemoryHavoc = false;

        VarSetOps::ClearD(this, state.fgCurUseSet);
        VarSetOps::ClearD(this, state.fgCurDefSet);

        for (Statement* const stmt : block->NonPhiStatements())
        {
            for (GenTree* const node : stmt->Nodes())
            {
                fgPerNodeLocalVarLiveness(state, node);
            }
        }

        block->bbVarUse      = state.fgCurUseSet;
        block->bbVarDef      = state.fgCurDefSet;
        block->bbMemoryUse   = state.fgCurMemoryUse;
        block->bbMemoryDef   = state.fgCurMemoryDef;
        block->bbMemoryHavoc = state.fgCurMemoryHavoc;

        // Also clear the IN set, just in case we will do multiple DFAs
        VarSetOps::ClearD(this, block->bbLiveIn);

        block->bbMemoryLiveIn = false;

        DBEXEC(verbose, fgDispBBLocalLiveness(block))
    }
}

void Compiler::fgPerBlockLocalVarLivenessLIR()
{
    assert(compRationalIRForm && (lvaTrackedCount != 0));

    for (BasicBlock* block : Blocks())
    {
        LivenessState state;

        state.fgCurUseSet = block->bbVarUse;
        state.fgCurDefSet = block->bbVarDef;

        VarSetOps::ClearD(this, state.fgCurUseSet);
        VarSetOps::ClearD(this, state.fgCurDefSet);

        for (GenTree* node : LIR::AsRange(block))
        {
            if (node->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
            {
                if (!lvaGetDesc(node->AsLclVarCommon())->IsAddressExposed())
                {
                    fgMarkUseDef(state, node->AsLclVarCommon());
                }
            }
            else if (node->OperIs(GT_LCL_ADDR))
            {
                assert(lvaGetDesc(node->AsLclAddr())->IsAddressExposed());
            }
        }

        block->bbVarUse = state.fgCurUseSet;
        block->bbVarDef = state.fgCurDefSet;

        // Also clear the IN set, just in case we will do multiple DFAs
        VarSetOps::ClearD(this, block->bbLiveIn);

        DBEXEC(verbose, fgDispBBLocalLiveness(block))
    }
}

//------------------------------------------------------------------------
// fgGetHandlerLiveVars: determine set of locals live because of implicit
//   exception flow from a block.
//
// Arguments:
//    block - the block in question
//
// Returns:
//    Additional set of locals to be considered live throughout the block.
//
// Notes:
//    Assumes caller has screened candidate blocks to only those with
//    exception flow, via `ehBlockHasExnFlowDsc`.
//
//    Exception flow can arise because of a newly raised exception (for
//    blocks within try regions) or because of an actively propagating exception
//    (for filter blocks). This flow effectively creates additional successor
//    edges in the flow graph that the jit does not model. This method computes
//    the net contribution from all the missing successor edges.
//
//    For example, with the following C# source, during EH processing of the throw,
//    the outer filter will execute in pass1, before the inner handler executes
//    in pass2, and so the filter blocks should show the inner handler's local is live.
//
//    try
//    {
//        using (AllocateObject())   // ==> try-finally; handler calls Dispose
//        {
//            throw new Exception();
//        }
//    }
//    catch (Exception e1) when (IsExpectedException(e1))
//    {
//        Console.WriteLine("In catch 1");
//    }

VARSET_VALRET_TP Compiler::fgGetHandlerLiveVars(BasicBlock* block)
{
    noway_assert(block);
    noway_assert(ehBlockHasExnFlowDsc(block));

    VARSET_TP liveVars(VarSetOps::MakeEmpty(this));
    EHblkDsc* HBtab = ehGetBlockExnFlowDsc(block);

    do
    {
        /* Either we enter the filter first or the catch/finally */
        if (HBtab->HasFilter())
        {
            VarSetOps::UnionD(this, liveVars, HBtab->ebdFilter->bbLiveIn);
#if defined(FEATURE_EH_FUNCLETS)
            // The EH subsystem can trigger a stack walk after the filter
            // has returned, but before invoking the handler, and the only
            // IP address reported from this method will be the original
            // faulting instruction, thus everything in the try body
            // must report as live any variables live-out of the filter
            // (which is the same as those live-in to the handler)
            VarSetOps::UnionD(this, liveVars, HBtab->ebdHndBeg->bbLiveIn);
#endif // FEATURE_EH_FUNCLETS
        }
        else
        {
            VarSetOps::UnionD(this, liveVars, HBtab->ebdHndBeg->bbLiveIn);
        }

        /* If we have nested try's edbEnclosing will provide them */
        noway_assert((HBtab->ebdEnclosingTryIndex == EHblkDsc::NO_ENCLOSING_INDEX) ||
                     (HBtab->ebdEnclosingTryIndex > ehGetIndex(HBtab)));

        unsigned outerIndex = HBtab->ebdEnclosingTryIndex;
        if (outerIndex == EHblkDsc::NO_ENCLOSING_INDEX)
        {
            break;
        }
        HBtab = ehGetDsc(outerIndex);

    } while (true);

    // If this block is within a filter, we also need to report as live
    // any vars live into enclosed finally or fault handlers, since the
    // filter will run during the first EH pass, and enclosed or enclosing
    // handlers will run during the second EH pass. So all these handlers
    // are "exception flow" successors of the filter.
    //
    // Note we are relying on ehBlockHasExnFlowDsc to return true
    // for any filter block that we should examine here.
    if (block->hasHndIndex())
    {
        const unsigned thisHndIndex   = block->getHndIndex();
        EHblkDsc*      enclosingHBtab = ehGetDsc(thisHndIndex);

        if (enclosingHBtab->InFilterRegionBBRange(block))
        {
            assert(enclosingHBtab->HasFilter());

            // Search the EH table for enclosed regions.
            //
            // All the enclosed regions will be lower numbered and
            // immediately prior to and contiguous with the enclosing
            // region in the EH tab.
            unsigned index = thisHndIndex;

            while (index > 0)
            {
                index--;
                unsigned enclosingIndex = ehGetEnclosingTryIndex(index);
                bool     isEnclosed     = false;

                // To verify this is an enclosed region, search up
                // through the enclosing regions until we find the
                // region associated with the filter.
                while (enclosingIndex != EHblkDsc::NO_ENCLOSING_INDEX)
                {
                    if (enclosingIndex == thisHndIndex)
                    {
                        isEnclosed = true;
                        break;
                    }

                    enclosingIndex = ehGetEnclosingTryIndex(enclosingIndex);
                }

                // If we found an enclosed region, check if the region
                // is a try fault or try finally, and if so, add any
                // locals live into the enclosed region's handler into this
                // block's live-in set.
                if (isEnclosed)
                {
                    EHblkDsc* enclosedHBtab = ehGetDsc(index);

                    if (enclosedHBtab->HasFinallyOrFaultHandler())
                    {
                        VarSetOps::UnionD(this, liveVars, enclosedHBtab->ebdHndBeg->bbLiveIn);
                    }
                }
                // Once we run across a non-enclosed region, we can stop searching.
                else
                {
                    break;
                }
            }
        }
    }

    return liveVars;
}

// This is the classic algorithm for live variable analysis.
class LiveVarAnalysis
{
    Compiler* m_compiler;

    bool m_hasPossibleBackEdge;

    bool      m_memoryLiveIn : 1;
    bool      m_memoryLiveOut : 1;
    VARSET_TP m_liveIn;
    VARSET_TP m_liveOut;

public:
    LiveVarAnalysis(Compiler* compiler)
        : m_compiler(compiler)
        , m_hasPossibleBackEdge(false)
        , m_memoryLiveIn(false)
        , m_memoryLiveOut(false)
        , m_liveIn(VarSetOps::MakeEmpty(compiler))
        , m_liveOut(VarSetOps::MakeEmpty(compiler))
    {
    }

    bool PerBlockAnalysis(BasicBlock* block, bool keepAliveThis)
    {
        VarSetOps::ClearD(m_compiler, m_liveOut);
        m_memoryLiveOut = false;

        if (block->EndsWithJmp(m_compiler))
        {
            // A JMP uses all the arguments, so mark them all as live at the JMP instruction.
            const LclVarDsc* varDscEndParams = m_compiler->lvaTable + m_compiler->info.compArgsCount;

            for (LclVarDsc* varDsc = m_compiler->lvaTable; varDsc < varDscEndParams; varDsc++)
            {
                noway_assert(!varDsc->lvPromoted);
                if (varDsc->lvTracked)
                {
                    VarSetOps::AddElemD(m_compiler, m_liveOut, varDsc->lvVarIndex);
                }
            }
        }

        // Additionally, union in all the live-in tracked vars of successors.
        for (BasicBlock* succ : block->GetAllSuccs(m_compiler))
        {
            VarSetOps::UnionD(m_compiler, m_liveOut, succ->bbLiveIn);
            m_memoryLiveOut |= succ->bbMemoryLiveIn;
            if (succ->bbNum <= block->bbNum)
            {
                m_hasPossibleBackEdge = true;
            }
        }

        /* For lvaKeepAliveAndReportThis methods, "this" has to be kept alive everywhere
           Note that a function may end in a throw on an infinite loop (as opposed to a return).
           "this" has to be alive everywhere even in such methods. */

        if (keepAliveThis)
        {
            VarSetOps::AddElemD(m_compiler, m_liveOut, m_compiler->lvaTable[m_compiler->info.compThisArg].lvVarIndex);
        }

        /* Compute the 'm_liveIn'  set */
        VarSetOps::LivenessD(m_compiler, m_liveIn, block->bbVarDef, block->bbVarUse, m_liveOut);

        // Even if block->bbMemoryDef is set, we must assume that it doesn't kill memory liveness from m_memoryLiveOut,
        // since (without proof otherwise) the use and def may touch different memory at run-time.
        m_memoryLiveIn = m_memoryLiveOut || block->bbMemoryUse;

        // Does this block have implicit exception flow to a filter or handler?
        // If so, include the effects of that flow.
        if (m_compiler->ehBlockHasExnFlowDsc(block))
        {
            const VARSET_TP& liveVars(m_compiler->fgGetHandlerLiveVars(block));
            VarSetOps::UnionD(m_compiler, m_liveIn, liveVars);
            VarSetOps::UnionD(m_compiler, m_liveOut, liveVars);

            // Implicit eh edges can induce loop-like behavior,
            // so make sure we iterate to closure.
            m_hasPossibleBackEdge = true;
        }

        /* Has there been any change in either live set? */

        bool liveInChanged = !VarSetOps::Equal(m_compiler, block->bbLiveIn, m_liveIn);
        if (liveInChanged || !VarSetOps::Equal(m_compiler, block->bbLiveOut, m_liveOut))
        {
            VarSetOps::Assign(m_compiler, block->bbLiveIn, m_liveIn);
            VarSetOps::Assign(m_compiler, block->bbLiveOut, m_liveOut);
        }

        const bool memoryLiveInChanged = (block->bbMemoryLiveIn != m_memoryLiveIn);
        if (memoryLiveInChanged || (block->bbMemoryLiveOut != m_memoryLiveOut))
        {
            block->bbMemoryLiveIn  = m_memoryLiveIn;
            block->bbMemoryLiveOut = m_memoryLiveOut;
        }

        return liveInChanged || memoryLiveInChanged;
    }

    void Run()
    {
        const bool keepAliveThis =
            m_compiler->lvaKeepAliveAndReportThis() && m_compiler->lvaTable[m_compiler->info.compThisArg].lvTracked;

        /* Live Variable Analysis - Backward dataflow */
        bool changed;
        do
        {
            changed = false;

            /* Visit all blocks and compute new data flow values */

            VarSetOps::ClearD(m_compiler, m_liveIn);
            VarSetOps::ClearD(m_compiler, m_liveOut);

            m_memoryLiveIn  = false;
            m_memoryLiveOut = false;

            for (BasicBlock* block = m_compiler->fgLastBB; block; block = block->bbPrev)
            {
                // sometimes block numbers are not monotonically increasing which
                // would cause us not to identify backedges
                if (block->bbNext && block->bbNext->bbNum <= block->bbNum)
                {
                    m_hasPossibleBackEdge = true;
                }

                if (PerBlockAnalysis(block, keepAliveThis))
                {
                    changed = true;
                }
            }
            // if there is no way we could have processed a block without seeing all of its predecessors
            // then there is no need to iterate
            if (!m_hasPossibleBackEdge)
            {
                break;
            }
        } while (changed);
    }
};

void Compiler::fgLiveVarAnalysis()
{
    LiveVarAnalysis analysis(this);
    analysis.Run();

#ifdef DEBUG
    if (verbose)
    {
        printf("\nBB liveness after fgLiveVarAnalysis():\n\n");
        fgDispBBLiveness();
    }
#endif // DEBUG
}

void Compiler::fgComputeLifeTrackedLocalUse(VARSET_TP& liveOut, LclVarDsc* lcl, GenTreeLclVarCommon* node)
{
    assert(node->OperIs(GT_LCL_VAR, GT_LCL_FLD));

    if (VarSetOps::TryAddElemD(this, liveOut, lcl->GetLivenessBitIndex()))
    {
        node->gtFlags |= GTF_VAR_DEATH;
    }
    else
    {
        node->gtFlags &= ~GTF_VAR_DEATH;
    }
}

bool Compiler::fgComputeLifeTrackedLocalDef(VARSET_TP&           liveOut,
                                            VARSET_VALARG_TP     keepAlive,
                                            LclVarDsc*           lcl,
                                            GenTreeLclVarCommon* node)
{
    assert(node->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    const unsigned index = lcl->GetLivenessBitIndex();

    if (VarSetOps::IsMember(this, liveOut, index))
    {
        if (node->OperIs(GT_STORE_LCL_VAR) || !node->IsPartialLclFld(this))
        {
            if (!VarSetOps::IsMember(this, keepAlive, index))
            {
                VarSetOps::RemoveElemD(this, liveOut, index);
            }
        }
    }
    else
    {
        node->gtFlags |= GTF_VAR_DEATH;

        if (!opts.MinOpts())
        {
            noway_assert(!VarSetOps::IsMember(this, keepAlive, index));
            assert(!lcl->IsAddressExposed());

            return true;
        }
    }

    return false;
}

bool Compiler::fgComputeLifePromotedLocal(VARSET_TP&           liveOut,
                                          VARSET_VALARG_TP     keepAlive,
                                          LclVarDsc*           lcl,
                                          GenTreeLclVarCommon* node)
{
    assert(node->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));
    assert(lcl->IsPromoted() && !lcl->IsAddressExposed());

    unsigned lclOffset    = 0;
    unsigned lclEndOffset = lcl->TypeIs(TYP_STRUCT) ? lcl->GetLayout()->GetSize() : varTypeSize(lcl->GetType());

    if (GenTreeLclFld* lclFld = node->IsLclFld())
    {
        lclOffset    = lclFld->GetLclOffs();
        lclEndOffset = lclOffset + (lclFld->TypeIs(TYP_STRUCT) ? lclFld->GetLayout(this)->GetSize()
                                                               : varTypeSize(lclFld->GetType()));
    }

    bool isDef     = node->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD);
    bool isLastUse = true;

    for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
    {
        LclVarDsc* fieldLcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

        assert(!fieldLcl->TypeIs(TYP_STRUCT));

        unsigned fieldOffset    = fieldLcl->GetPromotedFieldOffset();
        unsigned fieldEndOffset = fieldOffset + varTypeSize(fieldLcl->GetType());
        bool     partialOverlap = (fieldOffset < lclEndOffset) && (fieldEndOffset > lclOffset);

        if (!partialOverlap)
        {
            continue;
        }

        if (!fieldLcl->HasLiveness())
        {
            isLastUse = false;
            continue;
        }

        bool totalOverlap = (lclOffset <= fieldOffset) && (fieldEndOffset <= lclEndOffset);
        bool isLiveOut    = VarSetOps::IsMember(this, liveOut, fieldLcl->GetLivenessBitIndex());

        node->SetLastUse(i, !isLiveOut);
        isLastUse &= !isLiveOut;

        if (!isDef || !totalOverlap)
        {
            VarSetOps::AddElemD(this, liveOut, fieldLcl->GetLivenessBitIndex());
        }
        else if (!VarSetOps::IsMember(this, keepAlive, fieldLcl->GetLivenessBitIndex()))
        {
            VarSetOps::RemoveElemD(this, liveOut, fieldLcl->GetLivenessBitIndex());
        }
    }

    return isDef && isLastUse && !(lcl->lvCustomLayout && lcl->lvContainsHoles);
}

bool Compiler::fgComputeLifeBlock(VARSET_TP& life, VARSET_VALARG_TP keepAlive, BasicBlock* block)
{
    Statement* firstStmt = block->FirstNonPhiDef();

    if (firstStmt == nullptr)
    {
        return false;
    }

    bool       stmtRemoved = false;
    Statement* prevStmt    = block->lastStmt();
    Statement* stmt;

    do
    {
        noway_assert(prevStmt != nullptr);

        stmt     = prevStmt;
        prevStmt = stmt->GetPrevStmt();

        stmtRemoved |= fgComputeLifeStmt(life, keepAlive, stmt, block);
    } while (stmt != firstStmt);

    return stmtRemoved;
}

bool Compiler::fgComputeLifeStmt(VARSET_TP& liveOut, VARSET_VALARG_TP keepAlive, Statement* stmt, BasicBlock* block)
{
    bool updateStmt = false;
    INDEBUG(bool modified = false;)

    noway_assert(VarSetOps::IsSubset(this, keepAlive, liveOut));

    for (GenTree* node = stmt->GetRootNode(); node != nullptr;)
    {
        if (node->OperIs(GT_LCL_VAR, GT_LCL_FLD))
        {
            GenTreeLclVarCommon* lclNode = node->AsLclVarCommon();
            LclVarDsc*           lcl     = lvaGetDesc(lclNode);

            if (lcl->HasLiveness())
            {
                fgComputeLifeTrackedLocalUse(liveOut, lcl, lclNode);
            }
            else if (lcl->IsPromoted() && !lcl->IsAddressExposed())
            {
                bool isDeadStore = fgComputeLifePromotedLocal(liveOut, keepAlive, lcl, lclNode);
                assert(!isDeadStore);
            }
        }
        else if (node->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
        {
            GenTreeLclVarCommon* lclNode     = node->AsLclVarCommon();
            LclVarDsc*           lcl         = lvaGetDesc(lclNode);
            bool                 isDeadStore = false;

            if (lcl->HasLiveness())
            {
                isDeadStore = fgComputeLifeTrackedLocalDef(liveOut, keepAlive, lcl, lclNode);
            }
            else if (lcl->IsPromoted() && !lcl->IsAddressExposed())
            {
                isDeadStore = fgComputeLifePromotedLocal(liveOut, keepAlive, lcl, lclNode);
            }

            if (isDeadStore)
            {
                INDEBUG(modified = true);

                GenTree* prevNode = fgRemoveDeadStore(lclNode, stmt, block);

                if (prevNode == nullptr)
                {
                    // The entire statement was removed, we're done.

                    // TODO-MIKE-Review: Why do we care about an entire statement being removed
                    // but not about the other cases where only some nodes are removed? Those
                    // could affect liveness as well.

                    return true;
                }

                // When we have a nested store we have to postpone node reordering
                // until the current backward liveness traversal is complete.
                updateStmt = prevNode != stmt->GetRootNode();
                node       = prevNode;

                continue;
            }
        }

        node = node->gtPrev;
    }

    if (updateStmt)
    {
        gtSetCosts(stmt->GetRootNode());
        gtSetOrder(stmt->GetRootNode());
        gtSetStmtSeq(stmt);

        // We removed dead nested stores, we need to remove inherited GTF_ASG flags.
        gtUpdateStmtSideEffects(stmt);
    }

#ifdef DEBUG
    if (modified)
    {
        JITDUMPTREE(stmt->GetRootNode(), "\nfgComputeLifeStmt modified tree:\n");
    }
#endif

    return false;
}

bool Compiler::fgComputeLifeLIR(VARSET_TP& life, VARSET_VALARG_TP keepAliveVars, BasicBlock* block)
{
    noway_assert(VarSetOps::IsSubset(this, keepAliveVars, life));

    LIR::Range& blockRange = LIR::AsRange(block);
    GenTree*    firstNode  = blockRange.FirstNode();

    if (firstNode == nullptr)
    {
        return false;
    }

    bool useDefRemoved = false;

    for (GenTree *node = blockRange.LastNode(), *next, *end = firstNode->gtPrev; node != end; node = next)
    {
        next = node->gtPrev;

        switch (node->OperGet())
        {
            case GT_LCL_VAR:
            case GT_LCL_FLD:
            {
                GenTreeLclVarCommon* lclNode = node->AsLclVarCommon();
                LclVarDsc*           lcl     = lvaGetDesc(lclNode);

                if (node->IsUnusedValue())
                {
                    JITDUMP("Removing dead LclVar use:\n");
                    DISPNODE(lclNode);

                    blockRange.Delete(this, block, node);

                    if (lcl->HasLiveness())
                    {
                        useDefRemoved = true;
                    }
                }
                else if (lcl->HasLiveness())
                {
                    fgComputeLifeTrackedLocalUse(life, lcl, lclNode);
                }
                else if (lcl->IsPromoted() && !lcl->IsAddressExposed())
                {
                    fgComputeLifePromotedLocal(life, keepAliveVars, lcl, lclNode);
                }
                break;
            }

            case GT_STORE_LCL_VAR:
            case GT_STORE_LCL_FLD:
            {
                GenTreeLclVarCommon* lclNode     = node->AsLclVarCommon();
                LclVarDsc*           lcl         = lvaGetDesc(lclNode);
                bool                 isDeadStore = false;

                if (lcl->HasLiveness())
                {
                    isDeadStore = fgComputeLifeTrackedLocalDef(life, keepAliveVars, lcl, lclNode);
                }
                else
                {
                    // We have accurate ref counts when running late liveness so we can eliminate
                    // some stores if the local has a ref count of 1. Note that local addresses
                    // also count so a ref count of 1 here implies that the local is not address
                    // taken. It may still be marked as address exposed though - local address
                    // nodes may have been dead and removed earlier.

                    // Optimizations have to be enabled, otherwise all locals are implicitly
                    // referenced and have ref count 1.
                    assert(opts.OptimizationEnabled());

                    // TODO-MIKE-Review: Should implicitly referenced locals be excluded here?

                    if ((lcl->lvRefCnt() == 1) && !lcl->lvPinned)
                    {
                        if (lcl->IsPromotedField())
                        {
                            LclVarDsc* parentLcl = lvaGetDesc(lcl->GetPromotedFieldParentLclNum());

                            if ((parentLcl->lvRefCnt() == 1) && parentLcl->IsDependentPromoted())
                            {
                                isDeadStore = true;
                            }
                        }
                        else
                        {
                            if (!lcl->IsIndependentPromoted())
                            {
                                isDeadStore = true;
                            }
                        }
                    }

                    if (!isDeadStore && lcl->IsPromoted() && !lcl->IsAddressExposed())
                    {
                        isDeadStore = fgComputeLifePromotedLocal(life, keepAliveVars, lcl, lclNode);
                    }
                }

                if (isDeadStore)
                {
                    assert(!opts.MinOpts());

                    JITDUMP("Removing dead store:\n");
                    DISPNODE(lclNode);

                    lclNode->GetOp(0)->SetUnusedValue();
                    blockRange.Remove(node);
                    useDefRemoved = true;
                }

                break;
            }

            case GT_LCL_ADDR:
                assert(lvaGetDesc(node->AsLclAddr())->IsAddressExposed());
                FALLTHROUGH;
            case GT_LABEL:
            case GT_FTN_ADDR:
            case GT_CNS_INT:
            case GT_CNS_LNG:
            case GT_CNS_DBL:
            case GT_CNS_STR:
            case GT_CLS_VAR_ADDR:
            case GT_PHYSREG:
                // These are all side-effect-free leaf nodes.
                if (node->IsUnusedValue())
                {
                    JITDUMP("Removing dead node:\n");
                    DISPNODE(node);

                    blockRange.Remove(node);
                }
                break;

            case GT_CALL:
            {
                GenTreeCall* const call = node->AsCall();
                if ((call->TypeIs(TYP_VOID) || call->IsUnusedValue()) && !call->HasSideEffects(this))
                {
                    JITDUMP("Removing dead call:\n");
                    DISPNODE(call);

                    node->VisitOperands([](GenTree* operand) {
                        if (operand->IsValue())
                        {
                            operand->SetUnusedValue();
                        }

                        // Special-case PUTARG_STK: since this operator is not considered a value,
                        // DCE will not remove these nodes.
                        if (operand->OperIs(GT_PUTARG_STK))
                        {
                            operand->AsPutArgStk()->GetOp(0)->SetUnusedValue();
                            operand->ChangeToNothingNode();
                        }

                        return GenTree::VisitResult::Continue;
                    });

                    blockRange.Remove(node);
                }
                break;
            }

            case GT_BLK:
            case GT_OBJ:
                if (node->IsUnusedValue())
                {
                    if (node->OperMayThrow(this))
                    {
                        // IR doesn't expect dummy uses of `GT_OBJ/BLK`.
                        JITDUMP("Transform an unused OBJ/BLK node [%06u]\n", node->GetID());
                        Lowering::TransformUnusedIndirection(node->AsIndir());
                    }
                    else
                    {
                        node->AsIndir()->GetAddr()->SetUnusedValue();
                        blockRange.Remove(node);
                    }
                }
                break;

            case GT_LOCKADD:
            case GT_XORR:
            case GT_XAND:
            case GT_XADD:
            case GT_XCHG:
            case GT_CMPXCHG:
            case GT_MEMORYBARRIER:
            case GT_STOREIND:
            case GT_STORE_OBJ:
            case GT_STORE_BLK:
            case GT_COPY_BLK:
            case GT_INIT_BLK:
            case GT_JMP:
            case GT_JCMP:
            case GT_CMP:
            case GT_JCC:
            case GT_JTRUE:
            case GT_RETURN:
            case GT_SWITCH:
            case GT_RETFILT:
            case GT_START_NONGC:
            case GT_START_PREEMPTGC:
            case GT_PROF_HOOK:
#ifndef FEATURE_EH_FUNCLETS
            case GT_END_LFIN:
#endif
            case GT_SWITCH_TABLE:
            case GT_PINVOKE_PROLOG:
            case GT_PINVOKE_EPILOG:
            case GT_RETURNTRAP:
            case GT_PUTARG_STK:
            case GT_IL_OFFSET:
            case GT_KEEPALIVE:
            case GT_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
            case GT_HWINTRINSIC:
#endif
                // These nodes cannot be removed, some always have side effects, some are flow
                // control related and can only be removed by flowgraph updates, some just have
                // special meaning, like IL_OFFSET.
                //
                // TODO-MIKE-Review: Can we get rid of all this and just use the default case
                // that checks for all sorts of things anyway? Though as is now it will happily
                // remove a JTRUE. One way or another this looks rather bug prone. It would be
                // better to have a list of nodes that can be removed so if we miss something
                // we don't accidentally remove needed stuff.
                break;

            case GT_NOP:
                // NOTE: we need to keep some NOPs around because they are referenced by calls.
                // See the dead store removal code above (case GT_STORE_LCL_VAR) for more explanation.
                //
                // TODO-MIKE-Review: Well, there's no explanation above...
                if ((node->gtFlags & GTF_ORDER_SIDEEFF) != 0)
                {
                    break;
                }
                FALLTHROUGH;
            default:
                if ((!node->IsValue() || node->IsUnusedValue()) && !node->HasImplicitFlagsDef() &&
                    !node->OperMayThrow(this))
                {
                    JITDUMP("Removing dead node:\n");
                    DISPNODE(node);

                    node->VisitOperands([](GenTree* operand) {
                        operand->SetUnusedValue();
                        return GenTree::VisitResult::Continue;
                    });

                    blockRange.Remove(node);
                }
                break;
        }
    }

    return useDefRemoved;
}

GenTree* Compiler::fgRemoveDeadStore(GenTreeLclVarCommon* store, Statement* stmt, BasicBlock* block)
{
    assert(!compRationalIRForm);
    assert(store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    JITDUMPTREE(store, "Dead store:\n");

    GenTree* sideEffects = nullptr;

    if ((store->GetOp(0)->gtFlags & GTF_SIDE_EFFECT) != 0)
    {
        sideEffects = gtExtractSideEffList(store->GetOp(0));

        if (sideEffects != nullptr)
        {
            JITDUMPTREE(sideEffects, "Extracted dead store side effects:\n");
            noway_assert((sideEffects->gtFlags & GTF_SIDE_EFFECT) != 0);
        }
    }

    if (stmt->GetRootNode() != store)
    {
        // This is a nested store, we can change it to a NOP/COMMA to avoid
        // having to find the user (which would usually be a COMMA that now
        // becomes useless). But we need to be careful about sequencing, we
        // need to continue the backward traversal so we need to preserve
        // the original node order - we cannot call gtSetOrder like usual.

        if (sideEffects == nullptr)
        {
            store->ChangeToNothingNode();
        }
        else
        {
            store->SetOper(GT_COMMA);
            store->SetType(TYP_VOID);

            GenTreeOp* comma = store->AsOp();

            if (sideEffects->OperIs(GT_COMMA))
            {
                comma->SetOp(0, sideEffects->AsOp()->GetOp(0));
                comma->SetOp(1, sideEffects->AsOp()->GetOp(1));
                comma->SetReverseOps(sideEffects->IsReverseOp());
            }
            else
            {
                comma->SetOp(0, sideEffects);
                comma->SetOp(1, gtNewNothingNode());
                comma->SetReverseOps(false);
            }

            comma->SetSideEffects(sideEffects->GetSideEffects());
        }

        gtSetStmtSeq(stmt);

        return store;
    }

    if (sideEffects != nullptr)
    {
        stmt->SetRootNode(sideEffects);

        gtSetCosts(sideEffects);
        gtSetOrder(sideEffects);
        gtSetStmtSeq(stmt);

        return sideEffects;
    }

    fgRemoveStmt(block, stmt DEBUGARG(false));

    return nullptr;
}

void Compiler::fgInterBlockLocalVarLivenessUntracked()
{
    assert(lvaTrackedCount == 0);

    VARSET_TP keepAlive = VarSetOps::UninitVal();
    VARSET_TP life      = VarSetOps::UninitVal();

    for (BasicBlock* const block : Blocks())
    {
        if (compRationalIRForm)
        {
            fgComputeLifeLIR(life, keepAlive, block);
        }
        else
        {
            fgComputeLifeBlock(life, keepAlive, block);
        }
    }
}

bool Compiler::fgInterBlockLocalVarLiveness()
{
    VARSET_TP handlerLive    = VarSetOps::MakeEmpty(this);
    VARSET_TP finallyLiveOut = VarSetOps::MakeEmpty(this);

    for (BasicBlock* const block : Blocks())
    {
        if (block->hasEHBoundaryIn())
        {
            VarSetOps::UnionD(this, handlerLive, block->bbLiveIn);
        }

        if (block->hasEHBoundaryOut())
        {
            VarSetOps::UnionD(this, handlerLive, block->bbLiveOut);

            if (block->bbJumpKind == BBJ_EHFINALLYRET)
            {
                // Live on exit from finally - we track these separately because,
                // in addition to having EH live-out semantics, they are must-init.
                VarSetOps::UnionD(this, finallyLiveOut, block->bbLiveOut);
            }
        }
    }

    for (unsigned lclNum = 0; lclNum < lvaCount; lclNum++)
    {
        LclVarDsc* lcl = lvaGetDesc(lclNum);

        if (!lcl->HasLiveness())
        {
            continue;
        }

        // Uninitialized locals may need auto-initialization. Note that the liveness of
        // such locals will bubble to the top (fgFirstBB) in fgInterBlockLocalVarLiveness.

        // Fields of dependently promoted structs may be tracked. We shouldn't set lvMustInit
        // on them since the whole parent struct will be initialized; however, lvLiveInOutOfHndlr
        // should be set on them as appropriate.

        if (!lcl->IsParam() && VarSetOps::IsMember(this, fgFirstBB->bbLiveIn, lcl->GetLivenessBitIndex()) &&
            (info.compInitMem || varTypeIsGC(lcl->GetType())) && !lcl->IsDependentPromotedField(this))
        {
            lcl->lvMustInit = true;
        }

        // Mark all variables that are live on entry to an exception handler
        // or on exit from a filter handler or finally.

        bool isFinallyLiveOut = VarSetOps::IsMember(this, finallyLiveOut, lcl->GetLivenessBitIndex());

        if (isFinallyLiveOut || VarSetOps::IsMember(this, handlerLive, lcl->GetLivenessBitIndex()))
        {
            lvaSetLiveInOutOfHandler(lclNum);

            if (isFinallyLiveOut && !lcl->IsParam() && varTypeIsGC(lcl->TypeGet()))
            {
                lcl->lvMustInit = true;
            }
        }
    }

    bool      useDefRemoved = false;
    bool      changed       = false;
    VARSET_TP keepAlive     = VarSetOps::MakeEmpty(this);
    VARSET_TP life          = VarSetOps::MakeEmpty(this);

    for (BasicBlock* const block : Blocks())
    {
        if (ehBlockHasExnFlowDsc(block))
        {
            VarSetOps::Assign(this, keepAlive, fgGetHandlerLiveVars(block));

            noway_assert(VarSetOps::IsSubset(this, keepAlive, handlerLive));
        }
        else
        {
            VarSetOps::ClearD(this, keepAlive);
        }

        VarSetOps::Assign(this, life, block->bbLiveOut);

        if (compRationalIRForm)
        {
            useDefRemoved |= fgComputeLifeLIR(life, keepAlive, block);
        }
        else
        {
            useDefRemoved |= fgComputeLifeBlock(life, keepAlive, block);
        }

        if (!VarSetOps::Equal(this, life, block->bbLiveIn))
        {
            // Some variables have become dead all across the block
            // so life should be a subset of block->bbLiveIn
            noway_assert(VarSetOps::IsSubset(this, life, block->bbLiveIn));

            VarSetOps::Assign(this, block->bbLiveIn, life);

            // We changed the liveIn of the block, which may affect liveOut
            // of others, which may expose more dead stores.
            changed = true;
        }
    }

    return useDefRemoved && changed;
}

#ifdef DEBUG

void Compiler::fgDispBBLocalLiveness(BasicBlock* block)
{
    VARSET_TP allVars(VarSetOps::Union(this, block->bbVarUse, block->bbVarDef));
    printf(FMT_BB, block->bbNum);
    printf(" USE(%d)=", VarSetOps::Count(this, block->bbVarUse));
    lvaDispVarSet(block->bbVarUse, allVars);

    if (!block->IsLIR())
    {
        if (block->bbMemoryUse)
        {
            printf(" + Memory");
        }
    }

    printf("\n     DEF(%d)=", VarSetOps::Count(this, block->bbVarDef));
    lvaDispVarSet(block->bbVarDef, allVars);

    if (!block->IsLIR())
    {
        if (block->bbMemoryDef)
        {
            printf(" + Memory");
        }
        if (block->bbMemoryHavoc)
        {
            printf("*");
        }
    }

    printf("\n\n");
}

void Compiler::fgDispBBLiveness(BasicBlock* block)
{
    VARSET_TP allVars(VarSetOps::Union(this, block->bbLiveIn, block->bbLiveOut));
    printf(FMT_BB, block->bbNum);
    printf(" IN (%d)=", VarSetOps::Count(this, block->bbLiveIn));
    lvaDispVarSet(block->bbLiveIn, allVars);

    if (!block->IsLIR())
    {
        if (block->bbMemoryLiveIn)
        {
            printf(" + Memory");
        }
    }

    printf("\n     OUT(%d)=", VarSetOps::Count(this, block->bbLiveOut));
    lvaDispVarSet(block->bbLiveOut, allVars);

    if (!block->IsLIR())
    {
        if (block->bbMemoryLiveOut)
        {
            printf(" + Memory");
        }
    }

    printf("\n\n");
}

void Compiler::fgDispBBLiveness()
{
    for (BasicBlock* const block : Blocks())
    {
        fgDispBBLiveness(block);
    }
}

#endif // DEBUG
