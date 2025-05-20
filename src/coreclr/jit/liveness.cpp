// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "lower.h"

struct Compiler::LivenessState
{
    VARSET_TP fgCurUseSet; // vars used by block (before a def)
    VARSET_TP fgCurDefSet; // vars assigned by block (before a use)

    bool fgCurMemoryUse;   // True iff the current basic block uses memory.
    bool fgCurMemoryDef;   // True iff the current basic block modifies memory.
    bool fgCurMemoryHavoc; // True if the current basic block is known to set memory to a "havoc" value.
};

void Compiler::fgMarkUseDef(LivenessState& state, GenTreeLclRef* node)
{
    assert(node->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD, GT_LCL_STORE, GT_LCL_STORE_FLD));

    LclVarDsc* lcl = node->GetLcl();

    assert(!lcl->IsAddressExposed());

    // We should never encounter a reference to a local that has a zero ref count.
    // TODO-MIKE-Review: It's not clear why promotion makes a difference.
    if ((lcl->GetRefCount() == 0) && !lcl->IsPromoted())
    {
        JITDUMP("Found reference to V%02u with zero refCnt.\n", lcl->GetLclNum());
        assert(!"We should never encounter a reference to a lclVar that has a zero refCnt.");
        lcl->SetRefCount(1);
    }

    const bool isDef = node->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD);
    const bool isUse = !isDef || (node->OperIs(GT_LCL_STORE_FLD) && node->IsPartialLclFld(this));

    assert(isDef || isUse);

    if (lcl->HasLiveness())
    {
        if (isUse && !VarSetOps::IsMember(this, state.fgCurDefSet, lcl->GetLivenessBitIndex()))
        {
            VarSetOps::AddElemD(this, state.fgCurUseSet, lcl->GetLivenessBitIndex());
        }

        if (isDef)
        {
            VarSetOps::AddElemD(this, state.fgCurDefSet, lcl->GetLivenessBitIndex());
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

    for (LclVarDsc* fieldLcl : PromotedFields(lcl))
    {
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
    for (LclVarDsc* lcl : Locals())
    {
        lcl->lvMustInit = false;
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
        case GT_LCL_LOAD:
        case GT_LCL_LOAD_FLD:
            if (tree->AsLclRef()->GetLcl()->IsAddressExposed())
            {
                state.fgCurMemoryUse = true;
                break;
            }

            fgMarkUseDef(state, tree->AsLclRef());
            break;

        case GT_LCL_STORE:
        case GT_LCL_STORE_FLD:
            if (tree->AsLclRef()->GetLcl()->IsAddressExposed())
            {
                state.fgCurMemoryDef = true;
                break;
            }

            fgMarkUseDef(state, tree->AsLclRef());
            break;

        case GT_LCL_ADDR:
            assert(tree->AsLclAddr()->GetLcl()->IsAddressExposed());
            break;

        case GT_IND_LOAD:
        case GT_IND_LOAD_OBJ:
        case GT_IND_LOAD_BLK:
            if (tree->AsIndir()->IsVolatile())
            {
                // Treat volatile loads as memory defs, so that subsequent loads can't
                // see any previous stores, effectively preventing reordering.
                state.fgCurMemoryDef = true;
            }

            state.fgCurMemoryUse = true;
            break;

        case GT_IND_STORE:
        case GT_IND_STORE_OBJ:
        case GT_IND_STORE_BLK:
            state.fgCurMemoryDef = true;
            break;

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
            state.fgCurMemoryDef = true;
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
        {
            GenTreeHWIntrinsic* hwIntrinsicNode = tree->AsHWIntrinsic();

            if (hwIntrinsicNode->IsMemoryStore())
            {
                state.fgCurMemoryDef = true;
            }

            if (hwIntrinsicNode->IsMemoryLoad())
            {
                state.fgCurMemoryUse = true;
            }
            break;
        }
#endif

        case GT_CALL:
        {
            GenTreeCall* call    = tree->AsCall();
            bool         modHeap = true;

            if (call->IsHelperCall())
            {
                CorInfoHelpFunc helpFunc = eeGetHelperNum(call->GetMethodHandle());

                if (!HelperCallProperties::MutatesHeap(helpFunc) && !HelperCallProperties::MayRunCctor(helpFunc))
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

        default:
            assert(!tree->OperIs(GT_QMARK));
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
            if (node->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD, GT_LCL_STORE, GT_LCL_STORE_FLD))
            {
                if (!node->AsLclRef()->GetLcl()->IsAddressExposed())
                {
                    fgMarkUseDef(state, node->AsLclRef());
                }
            }
            else if (node->OperIs(GT_LCL_ADDR))
            {
                assert(node->AsLclAddr()->GetLcl()->IsAddressExposed());
            }
        }

        block->bbVarUse = state.fgCurUseSet;
        block->bbVarDef = state.fgCurDefSet;

        // Also clear the IN set, just in case we will do multiple DFAs
        VarSetOps::ClearD(this, block->bbLiveIn);

        DBEXEC(verbose, fgDispBBLocalLiveness(block))
    }
}

// Determine set of locals live because of implicit exception flow from a block.
//
// Assumes caller has screened candidate blocks to only those with
// exception flow, via `ehBlockHasExnFlowDsc`.
//
// Exception flow can arise because of a newly raised exception (for
// blocks within try regions) or because of an actively propagating exception
// (for filter blocks). This flow effectively creates additional successor
// edges in the flow graph that the jit does not model. This method computes
// the net contribution from all the missing successor edges.
//
// For example, with the following C# source, during EH processing of the throw,
// the outer filter will execute in pass1, before the inner handler executes
// in pass2, and so the filter blocks should show the inner handler's local is live.
//
// try
// {
//     using (AllocateObject())   // ==> try-finally; handler calls Dispose
//     {
//         throw new Exception();
//     }
// }
// catch (Exception e1) when (IsExpectedException(e1))
// {
//     Console.WriteLine("In catch 1");
// }

void Compiler::fgGetHandlerLiveVars(BasicBlock* block, VARSET_TP& liveVars)
{
    assert(ehBlockHasExnFlowDsc(block));

    VarSetOps::ClearD(this, liveVars);
    EHblkDsc* ehDesc = ehGetBlockExnFlowDsc(block);

    while (true)
    {
        if (ehDesc->HasFilter())
        {
            VarSetOps::UnionD(this, liveVars, ehDesc->ebdFilter->bbLiveIn);

#ifdef FEATURE_EH_FUNCLETS
            // The EH subsystem can trigger a stack walk after the filter has returned, but before
            // invoking the handler, and the only IP address reported from this method will be the
            // original faulting instruction, thus everything in the try body must report as live
            // any variables live-out of the filter (which is the same as those live-in to the handler).
            VarSetOps::UnionD(this, liveVars, ehDesc->ebdHndBeg->bbLiveIn);
#endif
        }
        else
        {
            VarSetOps::UnionD(this, liveVars, ehDesc->ebdHndBeg->bbLiveIn);
        }

        unsigned enclosingIndex = ehDesc->ebdEnclosingTryIndex;

        noway_assert((enclosingIndex == EHblkDsc::NO_ENCLOSING_INDEX) || (enclosingIndex > ehGetIndex(ehDesc)));

        if (enclosingIndex == EHblkDsc::NO_ENCLOSING_INDEX)
        {
            break;
        }

        ehDesc = ehGetDsc(enclosingIndex);
    }

    // If this block is within a filter, we also need to report as live any locals live into enclosed
    // finally or fault handlers, since the filter will run during the first EH pass, and enclosed or
    // enclosing handlers will run during the second EH pass. So all these handlers are "exception flow"
    // successors of the filter.
    //
    // Note we are relying on ehBlockHasExnFlowDsc to return true for any filter block that we should
    // examine here.

    if (!block->hasHndIndex())
    {
        return;
    }

    const unsigned thisHndIndex    = block->getHndIndex();
    EHblkDsc*      enclosingEHDesc = ehGetDsc(thisHndIndex);

    if (!enclosingEHDesc->InFilterRegionBBRange(block))
    {
        return;
    }

    assert(enclosingEHDesc->HasFilter());

    // Search the EH table for enclosed regions.
    // All the enclosed regions will be lower numbered and immediately prior to and contiguous
    // with the enclosing region in the EH tab.

    for (unsigned index = thisHndIndex; index > 0;)
    {
        index--;
        unsigned enclosingIndex = ehGetEnclosingTryIndex(index);
        bool     isEnclosed     = false;

        // To verify this is an enclosed region, search up through the enclosing regions until
        // we find the region associated with the filter.
        while (enclosingIndex != EHblkDsc::NO_ENCLOSING_INDEX)
        {
            if (enclosingIndex == thisHndIndex)
            {
                isEnclosed = true;
                break;
            }

            enclosingIndex = ehGetEnclosingTryIndex(enclosingIndex);
        }

        // Once we run across a non-enclosed region, we can stop searching.
        if (!isEnclosed)
        {
            break;
        }

        // If we found an enclosed region, check if the region is a try fault or try finally,
        // and if so, add any locals live into the enclosed region's handler into this block's
        // live-in set.
        EHblkDsc* enclosedEHDesc = ehGetDsc(index);

        if (enclosedEHDesc->HasFinallyOrFaultHandler())
        {
            VarSetOps::UnionD(this, liveVars, enclosedEHDesc->ebdHndBeg->bbLiveIn);
        }
    }
}

class LiveVarAnalysis
{
    Compiler* compiler;
    unsigned  keepAliveThisBitIndex = UINT_MAX;
    bool      mayHaveBackEdge       = false;
    bool      memoryLiveIn          = false;
    bool      memoryLiveOut         = false;
    VARSET_TP liveIn;
    VARSET_TP liveOut;
    VARSET_TP ehLiveVars = VarSetOps::UninitVal();

public:
    LiveVarAnalysis(Compiler* compiler)
        : compiler(compiler), liveIn(VarSetOps::MakeEmpty(compiler)), liveOut(VarSetOps::MakeEmpty(compiler))
    {
        if (compiler->lvaKeepAliveAndReportThis())
        {
            LclVarDsc* thisLcl = compiler->lvaGetDesc(compiler->info.GetThisParamLclNum());

            if (thisLcl->HasLiveness())
            {
                keepAliveThisBitIndex = thisLcl->GetLivenessBitIndex();
            }
        }
    }

    bool PerBlockAnalysis(BasicBlock* block)
    {
        VarSetOps::ClearD(compiler, liveOut);
        memoryLiveOut = false;

        for (BasicBlock* succ : block->GetAllSuccs(compiler))
        {
            VarSetOps::UnionD(compiler, liveOut, succ->bbLiveIn);
            memoryLiveOut |= succ->bbMemoryLiveIn;

            if (succ->bbNum <= block->bbNum)
            {
                mayHaveBackEdge = true;
            }
        }

        // For lvaKeepAliveAndReportThis methods, "this" has to be kept alive everywhere.
        // Note that a function may end in a throw on an infinite loop (as opposed to a return).
        // "this" has to be alive everywhere even in such methods.

        if (keepAliveThisBitIndex != UINT_MAX)
        {
            VarSetOps::AddElemD(compiler, liveOut, keepAliveThisBitIndex);
        }

        // A JMP uses all parameters, so mark them all as live at the JMP instruction.

        if (block->EndsWithJmp(compiler))
        {
            for (LclVarDsc* lcl : compiler->Params())
            {
                noway_assert(!lcl->IsPromoted());

                if (lcl->HasLiveness())
                {
                    VarSetOps::AddElemD(compiler, liveOut, lcl->GetLivenessBitIndex());
                }
            }
        }

        VarSetOps::LivenessD(compiler, liveIn, block->bbVarDef, block->bbVarUse, liveOut);

        // Even if block->bbMemoryDef is set, we must assume that it doesn't kill memory liveness
        // from memoryLiveOut, since (without proof otherwise) the use and def may touch different
        // memory at run-time.
        memoryLiveIn = memoryLiveOut || block->bbMemoryUse;

        if (compiler->ehBlockHasExnFlowDsc(block))
        {
            if (ehLiveVars == VarSetOps::UninitVal())
            {
                ehLiveVars = VarSetOps::Alloc(compiler);
            }

            compiler->fgGetHandlerLiveVars(block, ehLiveVars);
            VarSetOps::UnionD(compiler, liveIn, ehLiveVars);
            VarSetOps::UnionD(compiler, liveOut, ehLiveVars);

            // Implicit EH edges can induce loop-like behavior,
            // so make sure we iterate to closure.
            mayHaveBackEdge = true;
        }

        bool liveInChanged = !VarSetOps::Equal(compiler, block->bbLiveIn, liveIn);
        if (liveInChanged || !VarSetOps::Equal(compiler, block->bbLiveOut, liveOut))
        {
            VarSetOps::Assign(compiler, block->bbLiveIn, liveIn);
            VarSetOps::Assign(compiler, block->bbLiveOut, liveOut);
        }

        bool memoryLiveInChanged = (block->bbMemoryLiveIn != memoryLiveIn);
        if (memoryLiveInChanged || (block->bbMemoryLiveOut != memoryLiveOut))
        {
            block->bbMemoryLiveIn  = memoryLiveIn;
            block->bbMemoryLiveOut = memoryLiveOut;
        }

        return liveInChanged || memoryLiveInChanged;
    }

    void Run()
    {
        bool changed;

        do
        {
            changed = false;

            VarSetOps::ClearD(compiler, liveIn);
            VarSetOps::ClearD(compiler, liveOut);

            memoryLiveIn  = false;
            memoryLiveOut = false;

            for (BasicBlock* block = compiler->fgLastBB; block != nullptr; block = block->bbPrev)
            {
                // Sometimes block numbers are not monotonically increasing,
                // which would cause us not to identify backward edges.
                if ((block->bbNext != nullptr) && (block->bbNext->bbNum <= block->bbNum))
                {
                    mayHaveBackEdge = true;
                }

                if (PerBlockAnalysis(block))
                {
                    changed = true;
                }
            }

            // If there is no way we could have processed a block without seeing
            // all of its predecessors then there is no need to iterate.
        } while (mayHaveBackEdge && changed);
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
#endif
}

void Compiler::fgComputeLifeTrackedLocalUse(VARSET_TP& liveOut, LclVarDsc* lcl, GenTreeLclRef* node)
{
    assert(node->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD));

    node->SetLastUse(0, VarSetOps::TryAddElemD(this, liveOut, lcl->GetLivenessBitIndex()));
}

bool Compiler::fgComputeLifeTrackedLocalDef(VARSET_TP&     liveOut,
                                            VARSET_TP      keepAlive,
                                            LclVarDsc*     lcl,
                                            GenTreeLclRef* node)
{
    assert(node->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD));

    const unsigned index = lcl->GetLivenessBitIndex();

    if (VarSetOps::IsMember(this, liveOut, index))
    {
        if (node->OperIs(GT_LCL_STORE) || !node->IsPartialLclFld(this))
        {
            if (!VarSetOps::IsMember(this, keepAlive, index))
            {
                VarSetOps::RemoveElemD(this, liveOut, index);
            }
        }
    }
    else
    {
        node->SetLastUse(0, true);

        if (!opts.MinOpts())
        {
            noway_assert(!VarSetOps::IsMember(this, keepAlive, index));
            assert(!lcl->IsAddressExposed());

            return true;
        }
    }

    return false;
}

bool Compiler::fgComputeLifePromotedLocal(VARSET_TP& liveOut, VARSET_TP keepAlive, LclVarDsc* lcl, GenTreeLclRef* node)
{
    assert(node->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD, GT_LCL_STORE, GT_LCL_STORE_FLD));
    assert(lcl->IsPromoted() && !lcl->IsAddressExposed());

    unsigned lclOffset    = 0;
    unsigned lclEndOffset = lcl->TypeIs(TYP_STRUCT) ? lcl->GetLayout()->GetSize() : varTypeSize(lcl->GetType());

    if (GenTreeLclFld* lclFld = node->IsLclFld())
    {
        lclOffset    = lclFld->GetLclOffs();
        lclEndOffset = lclOffset + (lclFld->TypeIs(TYP_STRUCT) ? lclFld->GetLayout(this)->GetSize()
                                                               : varTypeSize(lclFld->GetType()));
    }

    bool isDef     = node->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD);
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

bool Compiler::fgComputeLifeBlock(VARSET_TP& life, VARSET_TP keepAlive, BasicBlock* block)
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

bool Compiler::fgComputeLifeStmt(VARSET_TP& liveOut, VARSET_TP keepAlive, Statement* stmt, BasicBlock* block)
{
    bool updateStmt = false;
    INDEBUG(bool modified = false);

    noway_assert(VarSetOps::IsSubset(this, keepAlive, liveOut));

    for (GenTree* node = stmt->GetRootNode(); node != nullptr;)
    {
        if (node->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD))
        {
            GenTreeLclRef* lclNode = node->AsLclRef();
            LclVarDsc*     lcl     = lclNode->GetLcl();

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
        else if (node->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD))
        {
            GenTreeLclRef* lclNode     = node->AsLclRef();
            LclVarDsc*     lcl         = lclNode->GetLcl();
            bool           isDeadStore = false;

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
        gtSetStmtOrder(stmt);

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

bool Compiler::fgComputeLifeLIR(VARSET_TP& life, VARSET_TP keepAlive, BasicBlock* block)
{
    noway_assert(VarSetOps::IsSubset(this, keepAlive, life));

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

        switch (node->GetOper())
        {
            case GT_LCL_LOAD:
            case GT_LCL_LOAD_FLD:
            {
                GenTreeLclRef* load = node->AsLclRef();
                LclVarDsc*     lcl  = load->GetLcl();

                if (node->IsUnusedValue())
                {
                    JITDUMPLIRNODE(load, "Removing dead local use:\n");

                    blockRange.Delete(this, block, node);

                    if (lcl->HasLiveness())
                    {
                        useDefRemoved = true;
                    }
                }
                else if (lcl->HasLiveness())
                {
                    fgComputeLifeTrackedLocalUse(life, lcl, load);
                }
                else if (lcl->IsPromoted() && !lcl->IsAddressExposed())
                {
                    fgComputeLifePromotedLocal(life, keepAlive, lcl, load);
                }
                break;
            }

            case GT_LCL_STORE:
            case GT_LCL_STORE_FLD:
            {
                GenTreeLclRef* store       = node->AsLclRef();
                LclVarDsc*     lcl         = store->GetLcl();
                bool           isDeadStore = false;

                if (lcl->HasLiveness())
                {
                    isDeadStore = fgComputeLifeTrackedLocalDef(life, keepAlive, lcl, store);
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

                    if ((lcl->GetRefCount() == 1) && !lcl->IsPinning())
                    {
                        if (lcl->IsPromotedField())
                        {
                            LclVarDsc* parentLcl = lvaGetDesc(lcl->GetPromotedFieldParentLclNum());

                            if ((parentLcl->GetRefCount() == 1) && parentLcl->IsDependentPromoted())
                            {
                                isDeadStore = true;
                            }
                        }
                        else if (lcl->IsIndependentPromoted())
                        {
                            // We may have a dead multi-reg store without any uses of the fields.
                            unsigned totalRefCount = 0;

                            for (LclVarDsc* fieldLcl : PromotedFields(lcl))
                            {
                                totalRefCount += fieldLcl->GetRefCount();
                            }

                            if (totalRefCount == 0)
                            {
                                isDeadStore = true;
                            }
                        }
                        else
                        {
                            isDeadStore = true;
                        }
                    }

                    if (!isDeadStore && lcl->IsPromoted() && !lcl->IsAddressExposed())
                    {
                        isDeadStore = fgComputeLifePromotedLocal(life, keepAlive, lcl, store);
                    }
                }

                if (isDeadStore)
                {
                    assert(!opts.MinOpts());

                    JITDUMPLIRNODE(store, "Removing dead local store:\n");

                    store->GetOp(0)->SetUnusedValue();
                    blockRange.Unlink(node);
                    useDefRemoved = true;
                }

                break;
            }

            case GT_LCL_ADDR:
                assert(node->AsLclAddr()->GetLcl()->IsAddressExposed());
                FALLTHROUGH;
            case GT_LABEL:
            case GT_CNS_INT:
#ifndef TARGET_64BIT
            case GT_CNS_LNG:
#endif
            case GT_CNS_DBL:
            case GT_CNS_STR:
            case GT_CONST_ADDR:
            case GT_REG_USE:
                // These are all side-effect-free leaf nodes.
                if (node->IsUnusedValue())
                {
                    JITDUMPLIRNODE(node, "Removing dead node:\n");

                    blockRange.Unlink(node);
                }
                break;

            case GT_CALL:
            {
                GenTreeCall* const call = node->AsCall();

                if ((call->TypeIs(TYP_VOID) || call->IsUnusedValue()) && !call->HasSideEffects(false, false))
                {
                    JITDUMPLIRNODE(call, "Removing dead call:\n");

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

                    blockRange.Unlink(node);
                }
                break;
            }

            case GT_IND_LOAD_BLK:
            case GT_IND_LOAD_OBJ:
                if (node->IsUnusedValue())
                {
                    if (node->IndirMayThrow(this))
                    {
                        // IR doesn't expect dummy uses of IND_LOAD_OBJ/BLK.
                        JITDUMP("Transform an unused IND_LOAD_OBJ/BLK node [%06u]\n", node->GetID());
                        Lowering::TransformUnusedIndirection(node->AsIndir());
                    }
                    else
                    {
                        node->AsIndir()->GetAddr()->SetUnusedValue();
                        blockRange.Unlink(node);
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
            case GT_IND_STORE:
            case GT_IND_STORE_OBJ:
            case GT_IND_STORE_BLK:
            case GT_COPY_BLK:
            case GT_INIT_BLK:
            case GT_JMP:
#ifdef TARGET_ARM64
            case GT_JCMP:
#endif
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
            case GT_NO_OP:
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

            default:
                if ((!node->IsValue() || node->IsUnusedValue()) && !node->HasImplicitFlagsDef() &&
                    !node->OperMayThrow(this))
                {
                    JITDUMPLIRNODE(node, "Removing dead node:\n");

                    node->VisitOperands([](GenTree* operand) {
                        operand->SetUnusedValue();
                        return GenTree::VisitResult::Continue;
                    });

                    blockRange.Unlink(node);
                }
                break;
        }
    }

    return useDefRemoved;
}

GenTree* Compiler::fgRemoveDeadStore(GenTreeLclRef* store, Statement* stmt, BasicBlock* block)
{
    assert(!compRationalIRForm);
    assert(store->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD));

    JITDUMPTREE(store, "Dead store:\n");

    GenTree* sideEffects = nullptr;

    if (store->GetOp(0)->HasAnySideEffect(GTF_SIDE_EFFECT))
    {
        sideEffects = gtExtractSideEffList(store->GetOp(0));

        if (sideEffects != nullptr)
        {
            JITDUMPTREE(sideEffects, "Extracted dead store side effects:\n");
            noway_assert(sideEffects->HasAnySideEffect(GTF_SIDE_EFFECT));
        }
    }

    if (stmt->GetRootNode() != store)
    {
        // This is a nested store, we can change it to a NOP/COMMA to avoid
        // having to find the user (which would usually be a COMMA that now
        // becomes useless). But we need to be careful about sequencing, we
        // need to continue the backward traversal so we need to preserve
        // the original node order - we cannot call gtSetStmtOrder like usual.

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
        gtSetStmtOrder(stmt);

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

    for (LclVarDsc* lcl : LivenessLocals())
    {
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
            lvaSetLiveInOutOfHandler(lcl);

            if (isFinallyLiveOut && !lcl->IsParam() && varTypeIsGC(lcl->GetType()))
            {
                lcl->lvMustInit = true;
            }
        }
    }

    bool      useDefRemoved = false;
    bool      changed       = false;
    VARSET_TP keepAlive     = VarSetOps::Alloc(this);
    VARSET_TP life          = VarSetOps::Alloc(this);

    for (BasicBlock* const block : Blocks())
    {
        VarSetOps::Assign(this, life, block->bbLiveOut);

        if (ehBlockHasExnFlowDsc(block))
        {
            fgGetHandlerLiveVars(block, keepAlive);
            noway_assert(VarSetOps::IsSubset(this, keepAlive, handlerLive));
        }
        else
        {
            VarSetOps::ClearD(this, keepAlive);
        }

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
    VARSET_TP allVars = VarSetOps::Alloc(this);
    VarSetOps::Union(this, allVars, block->bbVarUse, block->bbVarDef);

    printf(FMT_BB ":\nUSE = ", block->bbNum);
    lvaDispVarSet(block->bbVarUse, allVars);

    if (!block->IsLIR())
    {
        if (block->bbMemoryUse)
        {
            printf(" + Memory");
        }
    }

    printf("\nDEF = ");
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
    VARSET_TP allVars = VarSetOps::Alloc(this);
    VarSetOps::Union(this, allVars, block->bbLiveIn, block->bbLiveOut);

    printf(FMT_BB ":\nIN = ", block->bbNum);
    lvaDispVarSet(block->bbLiveIn, allVars);

    if (!block->IsLIR())
    {
        if (block->bbMemoryLiveIn)
        {
            printf(" + Memory");
        }
    }

    printf("\nOUT = ");
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
