// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "lower.h"

class Liveness
{
    Compiler* const compiler;
    CompAllocator   liveSetAllocator;
    unsigned const  liveSetSize;
    unsigned const  liveSetWordCount;
    bool const      isLIR;

    unsigned keepAliveThisBitIndex = UINT_MAX;
    bool     mayHaveBackEdge       = false;
    bool     memoryLiveIn          = false;
    bool     memoryLiveOut         = false;
    LiveSet  ehLiveSet             = LiveSetOps::UninitVal();
    LiveSet  handlerLive           = LiveSetOps::UninitVal();
    LiveSet  finallyLiveOut        = LiveSetOps::UninitVal();
    LiveSet  liveIn;
    LiveSet  liveOut;

    struct
    {
        LiveSet uses;
        LiveSet defs;

        bool memoryUse;
        bool memoryDef;
        bool memoryHavoc;
    } state;

    class LiveSetTraits
    {
    public:
        using Env  = Liveness*;
        using Word = ::LiveBitSetTraits::Word;

        static unsigned GetSize(const Liveness* liveness)
        {
            return liveness->liveSetSize;
        }

        static unsigned GetWordCount(const Liveness* liveness)
        {
            return liveness->liveSetWordCount;
        }

        static bool IsShort(const Liveness* liveness)
        {
            return GetWordCount(liveness) <= 1;
        }

        static Word* Alloc(Liveness* liveness, unsigned wordCount)
        {
            return liveness->liveSetAllocator.allocate<Word>(wordCount);
        }
    };

    using LiveSetOps = BitSetOps<LiveSetTraits>;

    void LivenessUntracked();
    void MarkUse(GenTreeLclRef* node);
    void MarkDef(GenTreeLclStore* node);
    void MarkDef(GenTreeLclStoreFld* node);
    void MarkPromotedUseDef(GenTreeLclRef* node, LclVarDsc* lcl);
    void PerNodeLiveness(GenTree* node);
    void PerBlockLiveness();
    void PerBlockLivenessLIR();
    void LiveAnalysis();
    bool PerBlockAnalysis(BasicBlock* block);
    void ComputeLifeTrackedLocalUse(LiveSet& liveOut, LclVarDsc* lcl, GenTreeLclRef* node);
    bool ComputeLifeTrackedLocalDef(LiveSet& liveOut, LiveSet keepAlive, LclVarDsc* lcl, GenTreeLclRef* node);
    bool ComputeLifePromotedLocal(LiveSet& liveOut, LiveSet keepAlive, LclVarDsc* lcl, GenTreeLclRef* node);
    bool ComputeLifeBlock(LiveSet& liveOut, LiveSet keepAlive, BasicBlock* block);
    bool ComputeLifeStmt(LiveSet& liveOut, LiveSet keepAlive, Statement* stmt, BasicBlock* block);
    bool ComputeLifeLIR(LiveSet& liveOut, LiveSet keepAlive, BasicBlock* block);
    void InterBlockLivenessUntracked();
    bool InterBlockLiveness();

    GenTree* RemoveDeadStore(GenTreeLclRef* store, Statement* stmt, BasicBlock* block);

#ifdef DEBUG
    void DumpBlockLiveness(BasicBlock* block);
#endif

public:
    Liveness(Compiler* compiler)
        : compiler(compiler)
        , liveSetAllocator(compiler->getAllocator(CMK_bitset))
        , liveSetSize(compiler->lvaTrackedCount)
        , liveSetWordCount(compiler->lvaLiveSetWordCount)
        , isLIR(compiler->compRationalIRForm)
        , liveIn(LiveSetOps::MakeEmpty(this))
        , liveOut(LiveSetOps::MakeEmpty(this))
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

    void Compute();
};

void Liveness::MarkUse(GenTreeLclRef* node)
{
    assert(node->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD));

    LclVarDsc* lcl = node->GetLcl();

    assert(!lcl->IsAddressExposed());
    assert(lcl->GetRefCount() != 0);

    if (lcl->HasLiveness())
    {
        if (!LiveSetOps::IsMember(this, state.defs, lcl->GetLivenessBitIndex()))
        {
            LiveSetOps::AddElemD(this, state.uses, lcl->GetLivenessBitIndex());
        }
    }
    else if (lcl->IsPromoted())
    {
        MarkPromotedUseDef(node, lcl);
    }
}

void Liveness::MarkDef(GenTreeLclStore* node)
{
    assert(node->OperIs(GT_LCL_STORE));

    LclVarDsc* lcl = node->GetLcl();

    assert(!lcl->IsAddressExposed());
    assert((lcl->GetRefCount() != 0) || (lcl->IsIndependentPromoted() && lcl->lvIsMultiRegRet));

    if (lcl->HasLiveness())
    {
        LiveSetOps::AddElemD(this, state.defs, lcl->GetLivenessBitIndex());
    }
    else if (lcl->IsPromoted())
    {
        MarkPromotedUseDef(node, lcl);
    }
}

void Liveness::MarkDef(GenTreeLclStoreFld* node)
{
    assert(node->OperIs(GT_LCL_STORE_FLD));

    LclVarDsc* lcl = node->GetLcl();

    assert(!lcl->IsAddressExposed());
    assert((lcl->GetRefCount() != 0) || (lcl->IsIndependentPromoted() && lcl->lvIsMultiRegRet));

    if (lcl->HasLiveness())
    {
        if (node->IsPartial(compiler) && !LiveSetOps::IsMember(this, state.defs, lcl->GetLivenessBitIndex()))
        {
            LiveSetOps::AddElemD(this, state.uses, lcl->GetLivenessBitIndex());
        }

        LiveSetOps::AddElemD(this, state.defs, lcl->GetLivenessBitIndex());
    }
    else if (lcl->IsPromoted())
    {
        MarkPromotedUseDef(node, lcl);
    }
}

void Liveness::MarkPromotedUseDef(GenTreeLclRef* node, LclVarDsc* lcl)
{
    assert(node->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD, GT_LCL_STORE, GT_LCL_STORE_FLD));
    assert(lcl->IsPromoted());
    assert(!lcl->IsAddressExposed());
    assert((lcl->GetRefCount() != 0) || (lcl->IsIndependentPromoted() && lcl->lvIsMultiRegRet));

    const bool isDef        = node->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD);
    unsigned   lclOffset    = 0;
    unsigned   lclEndOffset = lcl->TypeIs(TYP_STRUCT) ? lcl->GetLayout()->GetSize() : varTypeSize(lcl->GetType());

    if (GenTreeLclFld* lclFld = node->IsLclFld())
    {
        lclOffset    = lclFld->GetLclOffs();
        lclEndOffset = lclOffset + (lclFld->TypeIs(TYP_STRUCT) ? lclFld->GetLayout(compiler)->GetSize()
                                                               : varTypeSize(lclFld->GetType()));
    }

    for (LclVarDsc* fieldLcl : compiler->PromotedFields(lcl))
    {
        assert(!fieldLcl->TypeIs(TYP_STRUCT));

        if (!fieldLcl->HasLiveness())
        {
            continue;
        }

        unsigned fieldOffset    = fieldLcl->GetPromotedFieldOffset();
        unsigned fieldEndOffset = fieldOffset + varTypeSize(fieldLcl->GetType());
        bool     partialOverlap = (fieldOffset < lclEndOffset) && (fieldEndOffset > lclOffset);

        if (!partialOverlap)
        {
            continue;
        }

        bool totalOverlap = (lclOffset <= fieldOffset) && (fieldEndOffset <= lclEndOffset);
        bool isFieldUse   = !isDef || !totalOverlap;

        if (isFieldUse && !LiveSetOps::IsMember(this, state.defs, fieldLcl->GetLivenessBitIndex()))
        {
            LiveSetOps::AddElemD(this, state.uses, fieldLcl->GetLivenessBitIndex());
        }

        if (isDef)
        {
            LiveSetOps::AddElemD(this, state.defs, fieldLcl->GetLivenessBitIndex());
        }
    }
}

void Liveness::LivenessUntracked()
{
    assert(liveSetSize == 0);

    for (BasicBlock* const block : compiler->Blocks())
    {
        block->bbVarUse  = LiveSetOps::UninitVal();
        block->bbVarDef  = LiveSetOps::UninitVal();
        block->bbLiveIn  = LiveSetOps::UninitVal();
        block->bbLiveOut = LiveSetOps::UninitVal();

        block->bbMemoryUse     = false;
        block->bbMemoryDef     = false;
        block->bbMemoryLiveIn  = false;
        block->bbMemoryLiveOut = false;
    }

    if (!isLIR)
    {
        // Even if there are no tracked locals we still use memory liveness.
        PerBlockLiveness();
        LiveAnalysis();
    }

    InterBlockLivenessUntracked();

    // Since there are no tracked locals liveness basically never runs.
    INDEBUG(compiler->fgLocalVarLivenessDone = false;)
}

void Liveness::Compute()
{
    assert(compiler->opts.OptimizationEnabled());

    if (liveSetSize == 0)
    {
        LivenessUntracked();
        return;
    }

    for (BasicBlock* const block : compiler->Blocks())
    {
        block->bbVarUse  = LiveSetOps::Alloc(this);
        block->bbVarDef  = LiveSetOps::Alloc(this);
        block->bbLiveIn  = LiveSetOps::Alloc(this);
        block->bbLiveOut = LiveSetOps::MakeEmpty(this);

        block->bbMemoryUse     = false;
        block->bbMemoryDef     = false;
        block->bbMemoryLiveIn  = false;
        block->bbMemoryLiveOut = false;
    }

    handlerLive    = LiveSetOps::Alloc(this);
    finallyLiveOut = LiveSetOps::Alloc(this);

    for (bool changed = true; changed;)
    {
        if (isLIR)
        {
            PerBlockLivenessLIR();
        }
        else
        {
            PerBlockLiveness();
        }

        LiveAnalysis();
        changed = InterBlockLiveness();
    }

    LiveSet liveIn = compiler->fgFirstBB->bbLiveIn;

    for (LclVarDsc* lcl : compiler->LivenessLocals())
    {
        lcl->lvMustInit = false;

        // Uninitialized locals may need auto-initialization. Note that the liveness of
        // such locals will bubble to the top (fgFirstBB) in InterBlockLiveness.

        // Fields of dependently promoted structs may be tracked. We shouldn't set lvMustInit
        // on them since the whole parent struct will be initialized; however, lvLiveInOutOfHndlr
        // should be set on them as appropriate.

        if (!lcl->IsParam() && LiveSetOps::IsMember(this, liveIn, lcl->GetLivenessBitIndex()) &&
            (compiler->info.compInitMem || varTypeIsGC(lcl->GetType())) && !lcl->IsDependentPromotedField(compiler))
        {
            lcl->lvMustInit = true;
        }

        // Mark all variables that are live on entry to an exception handler
        // or on exit from a filter handler or finally.

        bool isFinallyLiveOut = LiveSetOps::IsMember(this, finallyLiveOut, lcl->GetLivenessBitIndex());

        if (isFinallyLiveOut || LiveSetOps::IsMember(this, handlerLive, lcl->GetLivenessBitIndex()))
        {
            compiler->lvaSetLiveInOutOfHandler(lcl);

            if (isFinallyLiveOut && !lcl->IsParam() && varTypeIsGC(lcl->GetType()))
            {
                lcl->lvMustInit = true;
            }
        }
    }

    INDEBUG(compiler->fgLocalVarLivenessDone = true;)
}

void Liveness::PerNodeLiveness(GenTree* tree)
{
    switch (tree->GetOper())
    {
        case GT_LCL_LOAD:
        case GT_LCL_LOAD_FLD:
            if (tree->AsLclRef()->GetLcl()->IsAddressExposed())
            {
                state.memoryUse = true;
                break;
            }

            MarkUse(tree->AsLclRef());
            break;

        case GT_LCL_STORE:
            if (tree->AsLclRef()->GetLcl()->IsAddressExposed())
            {
                state.memoryDef = true;
                break;
            }

            MarkDef(tree->AsLclStore());
            break;

        case GT_LCL_STORE_FLD:
            if (tree->AsLclRef()->GetLcl()->IsAddressExposed())
            {
                state.memoryDef = true;
                break;
            }

            MarkDef(tree->AsLclStoreFld());
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
                state.memoryDef = true;
            }

            state.memoryUse = true;
            break;

        case GT_IND_STORE:
        case GT_IND_STORE_OBJ:
        case GT_IND_STORE_BLK:
            state.memoryDef = true;
            break;

        case GT_LOCKADD:
        case GT_XORR:
        case GT_XAND:
        case GT_XADD:
        case GT_XCHG:
        case GT_CMPXCHG:
        case GT_COPY_BLK:
        case GT_INIT_BLK:
            state.memoryUse   = true;
            state.memoryDef   = true;
            state.memoryHavoc = true;
            break;

        case GT_MEMORYBARRIER:
            state.memoryDef = true;
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
        {
            GenTreeHWIntrinsic* hwIntrinsicNode = tree->AsHWIntrinsic();

            if (hwIntrinsicNode->IsMemoryStore())
            {
                state.memoryDef = true;
            }

            if (hwIntrinsicNode->IsMemoryLoad())
            {
                state.memoryUse = true;
            }
            break;
        }
#endif

        case GT_CALL:
        {
            GenTreeCall* call    = tree->AsCall();
            bool         modHeap = true;

            if (CorInfoHelpFunc helper = call->IsHelperCall())
            {
                if (!HelperCallProperties::MutatesHeap(helper) && !HelperCallProperties::MayRunCctor(helper))
                {
                    modHeap = false;
                }
            }

            if (modHeap)
            {
                state.memoryUse   = true;
                state.memoryDef   = true;
                state.memoryHavoc = true;
            }
            break;
        }

        default:
            assert(!tree->OperIs(GT_QMARK, GT_PHI));
            break;
    }
}

void Liveness::PerBlockLiveness()
{
    assert(!isLIR);

    for (BasicBlock* block : compiler->Blocks())
    {
        state.uses        = block->bbVarUse;
        state.defs        = block->bbVarDef;
        state.memoryUse   = false;
        state.memoryDef   = false;
        state.memoryHavoc = false;

        LiveSetOps::ClearD(this, state.uses);
        LiveSetOps::ClearD(this, state.defs);
        // Also clear the IN set, just in case we will do multiple DFAs
        LiveSetOps::ClearD(this, block->bbLiveIn);
        block->bbMemoryLiveIn = false;

        for (Statement* const stmt : block->Statements())
        {
            for (GenTree* const node : stmt->Nodes())
            {
                PerNodeLiveness(node);
            }
        }

        block->bbVarUse      = state.uses;
        block->bbVarDef      = state.defs;
        block->bbMemoryUse   = state.memoryUse;
        block->bbMemoryDef   = state.memoryDef;
        block->bbMemoryHavoc = state.memoryHavoc;

        DBEXEC(compiler->verbose, DumpBlockLiveness(block))
    }
}

void Liveness::PerBlockLivenessLIR()
{
    assert(isLIR && (liveSetSize != 0));

    for (BasicBlock* block : compiler->Blocks())
    {
        state.uses = block->bbVarUse;
        state.defs = block->bbVarDef;

        LiveSetOps::ClearD(this, state.uses);
        LiveSetOps::ClearD(this, state.defs);
        // Also clear the IN set, just in case we will do multiple DFAs
        LiveSetOps::ClearD(this, block->bbLiveIn);

        for (GenTree* node : LIR::AsRange(block))
        {
            if (node->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD))
            {
                if (!node->AsLclRef()->GetLcl()->IsAddressExposed())
                {
                    MarkUse(node->AsLclRef());
                }
            }
            else if (node->OperIs(GT_LCL_STORE))
            {
                if (!node->AsLclRef()->GetLcl()->IsAddressExposed())
                {
                    MarkDef(node->AsLclStore());
                }
            }
            else if (node->OperIs(GT_LCL_STORE_FLD))
            {
                if (!node->AsLclRef()->GetLcl()->IsAddressExposed())
                {
                    MarkDef(node->AsLclStoreFld());
                }
            }
            else if (node->OperIs(GT_LCL_ADDR))
            {
                assert(node->AsLclAddr()->GetLcl()->IsAddressExposed());
            }
        }

        block->bbVarUse = state.uses;
        block->bbVarDef = state.defs;

        DBEXEC(compiler->verbose, DumpBlockLiveness(block))
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

void Compiler::fgGetHandlerLiveVars(BasicBlock* block, LiveSet& liveVars)
{
    assert(ehBlockHasExnFlowDsc(block));

    LiveSetOps::ClearD(this, liveVars);
    EHblkDsc* ehDesc = ehGetBlockExnFlowDsc(block);

    while (true)
    {
        if (ehDesc->HasFilter())
        {
            LiveSetOps::UnionD(this, liveVars, ehDesc->ebdFilter->bbLiveIn);

#ifdef FEATURE_EH_FUNCLETS
            // The EH subsystem can trigger a stack walk after the filter has returned, but before
            // invoking the handler, and the only IP address reported from this method will be the
            // original faulting instruction, thus everything in the try body must report as live
            // any variables live-out of the filter (which is the same as those live-in to the handler).
            LiveSetOps::UnionD(this, liveVars, ehDesc->ebdHndBeg->bbLiveIn);
#endif
        }
        else
        {
            LiveSetOps::UnionD(this, liveVars, ehDesc->ebdHndBeg->bbLiveIn);
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

    for (unsigned index = thisHndIndex; index != 0; index--)
    {
        EHblkDsc* enclosedEHDesc = ehGetDsc(index - 1);
        unsigned  enclosingIndex = enclosedEHDesc->ebdEnclosingTryIndex;
        bool      isEnclosed     = false;

        // To verify this is indeed an enclosed region, search up through the enclosing regions
        // until we find the region associated with the filter.
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

        if (enclosedEHDesc->HasFinallyOrFaultHandler())
        {
            LiveSetOps::UnionD(this, liveVars, enclosedEHDesc->ebdHndBeg->bbLiveIn);
        }
    }
}

bool Liveness::PerBlockAnalysis(BasicBlock* block)
{
    LiveSetOps::ClearD(this, liveOut);
    memoryLiveOut = false;

    for (BasicBlock* succ : block->GetAllSuccs(compiler))
    {
        LiveSetOps::UnionD(this, liveOut, succ->bbLiveIn);
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
        LiveSetOps::AddElemD(this, liveOut, keepAliveThisBitIndex);
    }

    // A JMP uses all parameters, so mark them all as live at the JMP instruction.

    if (block->EndsWithJmp(compiler))
    {
        for (LclVarDsc* lcl : compiler->Params())
        {
            noway_assert(!lcl->IsPromoted());

            if (lcl->HasLiveness())
            {
                LiveSetOps::AddElemD(this, liveOut, lcl->GetLivenessBitIndex());
            }
        }
    }

    LiveSetOps::LivenessD(this, liveIn, block->bbVarDef, block->bbVarUse, liveOut);

    // Even if block->bbMemoryDef is set, we must assume that it doesn't kill memory liveness
    // from memoryLiveOut, since (without proof otherwise) the use and def may touch different
    // memory at run-time.
    memoryLiveIn = memoryLiveOut || block->bbMemoryUse;

    if (compiler->ehBlockHasExnFlowDsc(block))
    {
        if (ehLiveSet == LiveSetOps::UninitVal())
        {
            ehLiveSet = LiveSetOps::Alloc(this);
        }

        compiler->fgGetHandlerLiveVars(block, ehLiveSet);
        LiveSetOps::UnionD(this, liveIn, ehLiveSet);
        LiveSetOps::UnionD(this, liveOut, ehLiveSet);

        // Implicit EH edges can induce loop-like behavior,
        // so make sure we iterate to closure.
        mayHaveBackEdge = true;
    }

    bool liveInChanged = !LiveSetOps::Equal(this, block->bbLiveIn, liveIn);
    if (liveInChanged || !LiveSetOps::Equal(this, block->bbLiveOut, liveOut))
    {
        LiveSetOps::Assign(this, block->bbLiveIn, liveIn);
        LiveSetOps::Assign(this, block->bbLiveOut, liveOut);
    }

    bool memoryLiveInChanged = (block->bbMemoryLiveIn != memoryLiveIn);
    if (memoryLiveInChanged || (block->bbMemoryLiveOut != memoryLiveOut))
    {
        block->bbMemoryLiveIn  = memoryLiveIn;
        block->bbMemoryLiveOut = memoryLiveOut;
    }

    return liveInChanged || memoryLiveInChanged;
}

void Liveness::LiveAnalysis()
{
    bool changed;

    do
    {
        changed = false;

        LiveSetOps::ClearD(this, liveIn);
        memoryLiveIn = false;

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

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("\nBB liveness after LiveAnalysis():\n\n");
        compiler->fgDispBBLiveness();
    }
#endif
}

void Liveness::ComputeLifeTrackedLocalUse(LiveSet& liveOut, LclVarDsc* lcl, GenTreeLclRef* node)
{
    assert(node->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD));

    node->SetLastUse(0, LiveSetOps::TryAddElemD(this, liveOut, lcl->GetLivenessBitIndex()));
}

bool Liveness::ComputeLifeTrackedLocalDef(LiveSet& liveOut, LiveSet keepAlive, LclVarDsc* lcl, GenTreeLclRef* node)
{
    assert(node->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD));

    const unsigned index = lcl->GetLivenessBitIndex();

    if (!LiveSetOps::IsMember(this, liveOut, index))
    {
        node->SetLastUse(0, true);

        assert(!LiveSetOps::IsMember(this, keepAlive, index));

        return true;
    }

    if (node->OperIs(GT_LCL_STORE) || !node->AsLclStoreFld()->IsPartial(compiler))
    {
        if (!LiveSetOps::IsMember(this, keepAlive, index))
        {
            LiveSetOps::RemoveElemD(this, liveOut, index);
        }
    }

    return false;
}

bool Liveness::ComputeLifePromotedLocal(LiveSet& liveOut, LiveSet keepAlive, LclVarDsc* lcl, GenTreeLclRef* node)
{
    assert(node->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD, GT_LCL_STORE, GT_LCL_STORE_FLD));
    assert(lcl->IsPromoted() && !lcl->IsAddressExposed());

    unsigned lclOffset    = 0;
    unsigned lclEndOffset = lcl->TypeIs(TYP_STRUCT) ? lcl->GetLayout()->GetSize() : varTypeSize(lcl->GetType());

    if (GenTreeLclFld* lclFld = node->IsLclFld())
    {
        lclOffset    = lclFld->GetLclOffs();
        lclEndOffset = lclOffset + (lclFld->TypeIs(TYP_STRUCT) ? lclFld->GetLayout(compiler)->GetSize()
                                                               : varTypeSize(lclFld->GetType()));
    }

    bool isDef     = node->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD);
    bool isLastUse = true;

    for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
    {
        LclVarDsc* fieldLcl = compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

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

        bool totalOverlap   = (lclOffset <= fieldOffset) && (fieldEndOffset <= lclEndOffset);
        bool isFieldLastUse = !LiveSetOps::IsMember(this, liveOut, fieldLcl->GetLivenessBitIndex());

        isLastUse &= isFieldLastUse;

        if (!isDef || !totalOverlap)
        {
            LiveSetOps::AddElemD(this, liveOut, fieldLcl->GetLivenessBitIndex());
        }
        else if (!LiveSetOps::IsMember(this, keepAlive, fieldLcl->GetLivenessBitIndex()))
        {
            LiveSetOps::RemoveElemD(this, liveOut, fieldLcl->GetLivenessBitIndex());
        }

        node->SetLastUse(i, isFieldLastUse);
    }

    return isDef && isLastUse && !(lcl->lvCustomLayout && lcl->lvContainsHoles);
}

bool Liveness::ComputeLifeBlock(LiveSet& liveOut, LiveSet keepAlive, BasicBlock* block)
{
    Statement* firstStmt = block->GetFirstStatement();

    if (firstStmt == nullptr)
    {
        return false;
    }

    bool       useDefRemoved = false;
    Statement* prevStmt      = block->GetLastStatement();
    Statement* stmt;

    do
    {
        noway_assert(prevStmt != nullptr);

        stmt     = prevStmt;
        prevStmt = stmt->GetPrevStmt();

        useDefRemoved |= ComputeLifeStmt(liveOut, keepAlive, stmt, block);
    } while (stmt != firstStmt);

    return useDefRemoved;
}

bool Liveness::ComputeLifeStmt(LiveSet& liveOut, LiveSet keepAlive, Statement* stmt, BasicBlock* block)
{
    bool updateStmt       = false;
    bool deadStoreRemoved = false;

    noway_assert(LiveSetOps::IsSubset(this, keepAlive, liveOut));

    for (GenTree* node = stmt->GetRootNode(); node != nullptr;)
    {
        assert(!node->OperIs(GT_PHI));

        if (node->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD))
        {
            GenTreeLclRef* lclNode = node->AsLclRef();
            LclVarDsc*     lcl     = lclNode->GetLcl();

            if (lcl->HasLiveness())
            {
                ComputeLifeTrackedLocalUse(liveOut, lcl, lclNode);
            }
            else if (lcl->IsPromoted() && !lcl->IsAddressExposed())
            {
                bool isDeadStore = ComputeLifePromotedLocal(liveOut, keepAlive, lcl, lclNode);
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
                isDeadStore = ComputeLifeTrackedLocalDef(liveOut, keepAlive, lcl, lclNode);
            }
            else if (lcl->IsPromoted() && !lcl->IsAddressExposed())
            {
                isDeadStore = ComputeLifePromotedLocal(liveOut, keepAlive, lcl, lclNode);
            }

            if (isDeadStore)
            {
                GenTree* prevNode = RemoveDeadStore(lclNode, stmt, block);

                if (prevNode == nullptr)
                {
                    // The entire statement was removed, we're done.
                    return true;
                }

                // When we have a nested store we have to postpone node reordering
                // until the current backward liveness traversal is complete.
                updateStmt       = prevNode != stmt->GetRootNode();
                node             = prevNode;
                deadStoreRemoved = true;

                continue;
            }
        }

        node = node->gtPrev;
    }

    if (updateStmt)
    {
        compiler->gtSetStmtOrder(stmt);

        // We removed dead nested stores, we need to remove inherited GTF_ASG flags.
        compiler->gtUpdateStmtSideEffects(stmt);
    }

    if (deadStoreRemoved)
    {
        JITDUMPTREE(stmt->GetRootNode(), "\nComputeLifeStmt modified tree:\n");
    }

    return deadStoreRemoved;
}

bool Liveness::ComputeLifeLIR(LiveSet& liveOut, LiveSet keepAlive, BasicBlock* block)
{
    noway_assert(LiveSetOps::IsSubset(this, keepAlive, liveOut));

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

                    blockRange.Delete(compiler, block, node);

                    if (lcl->HasLiveness())
                    {
                        useDefRemoved = true;
                    }
                }
                else if (lcl->HasLiveness())
                {
                    ComputeLifeTrackedLocalUse(liveOut, lcl, load);
                }
                else if (lcl->IsPromoted() && !lcl->IsAddressExposed())
                {
                    ComputeLifePromotedLocal(liveOut, keepAlive, lcl, load);
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
                    isDeadStore = ComputeLifeTrackedLocalDef(liveOut, keepAlive, lcl, store);
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
                    assert(compiler->opts.OptimizationEnabled());

                    // TODO-MIKE-Review: Should implicitly referenced locals be excluded here?

                    if ((lcl->GetRefCount() == 1) && !lcl->IsPinning())
                    {
                        if (lcl->IsPromotedField())
                        {
                            LclVarDsc* parentLcl = compiler->lvaGetDesc(lcl->GetPromotedFieldParentLclNum());

                            if ((parentLcl->GetRefCount() == 1) && parentLcl->IsDependentPromoted())
                            {
                                isDeadStore = true;
                            }
                        }
                        else if (lcl->IsIndependentPromoted())
                        {
                            // We may have a dead multi-reg store without any uses of the fields.
                            unsigned totalRefCount = 0;

                            for (LclVarDsc* fieldLcl : compiler->PromotedFields(lcl))
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
                        isDeadStore = ComputeLifePromotedLocal(liveOut, keepAlive, lcl, store);
                    }
                }

                if (isDeadStore)
                {
                    assert(!compiler->opts.MinOpts());

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
                    if (node->HasAnySideEffect(GTF_EXCEPT))
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
                assert(!node->OperIs(GT_PHI));

                if ((!node->IsValue() || node->IsUnusedValue()) && !node->HasImplicitFlagsDef() &&
                    !node->HasAnySideEffect(GTF_EXCEPT))
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

GenTree* Liveness::RemoveDeadStore(GenTreeLclRef* store, Statement* stmt, BasicBlock* block)
{
    assert(!isLIR);
    assert(store->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD));

    JITDUMPTREE(store, "Dead store:\n");

    GenTree* sideEffects = nullptr;

    if (store->GetOp(0)->HasAnySideEffect(GTF_SIDE_EFFECT))
    {
        sideEffects = compiler->gtExtractSideEffList(store->GetOp(0));

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
                comma->SetOp(1, compiler->gtNewNothingNode());
                comma->SetReverseOps(false);
            }

            comma->SetSideEffects(sideEffects->GetSideEffects());
        }

        compiler->gtSetStmtSeq(stmt);

        return store;
    }

    if (sideEffects != nullptr)
    {
        stmt->SetRootNode(sideEffects);
        compiler->gtSetStmtOrder(stmt);

        return sideEffects;
    }

    compiler->fgRemoveStmt(block, stmt DEBUGARG(false));

    return nullptr;
}

void Liveness::InterBlockLivenessUntracked()
{
    assert(liveSetSize == 0);

    LiveSet keepAlive = LiveSetOps::UninitVal();
    LiveSet life      = LiveSetOps::UninitVal();

    for (BasicBlock* const block : compiler->Blocks())
    {
        if (isLIR)
        {
            ComputeLifeLIR(life, keepAlive, block);
        }
        else
        {
            ComputeLifeBlock(life, keepAlive, block);
        }
    }
}

bool Liveness::InterBlockLiveness()
{
    LiveSetOps::ClearD(this, handlerLive);
    LiveSetOps::ClearD(this, finallyLiveOut);

    if (compiler->compHndBBtabCount != 0)
    {
        for (BasicBlock* const block : compiler->Blocks())
        {
            if (block->hasEHBoundaryIn())
            {
                LiveSetOps::UnionD(this, handlerLive, block->bbLiveIn);
            }

            if (block->hasEHBoundaryOut())
            {
                LiveSetOps::UnionD(this, handlerLive, block->bbLiveOut);

                if (block->bbJumpKind == BBJ_EHFINALLYRET)
                {
                    // Live on exit from finally - we track these separately because,
                    // in addition to having EH live-out semantics, they are must-init.
                    LiveSetOps::UnionD(this, finallyLiveOut, block->bbLiveOut);
                }
            }
        }
    }

    bool    useDefRemoved = false;
    bool    changed       = false;
    LiveSet keepAlive     = LiveSetOps::Alloc(this);
    LiveSet liveOut       = LiveSetOps::Alloc(this);

    for (BasicBlock* const block : compiler->Blocks())
    {
        LiveSetOps::Assign(this, liveOut, block->bbLiveOut);

        if (compiler->ehBlockHasExnFlowDsc(block))
        {
            compiler->fgGetHandlerLiveVars(block, keepAlive);
            noway_assert(LiveSetOps::IsSubset(this, keepAlive, handlerLive));
        }
        else
        {
            LiveSetOps::ClearD(this, keepAlive);
        }

        if (isLIR)
        {
            useDefRemoved |= ComputeLifeLIR(liveOut, keepAlive, block);
        }
        else
        {
            useDefRemoved |= ComputeLifeBlock(liveOut, keepAlive, block);
        }

        if (!LiveSetOps::Equal(this, liveOut, block->bbLiveIn))
        {
            // Some variables have become dead all across the block
            // so life should be a subset of block->bbLiveIn
            noway_assert(LiveSetOps::IsSubset(this, liveOut, block->bbLiveIn));

            LiveSetOps::Assign(this, block->bbLiveIn, liveOut);

            // We changed the liveIn of the block, which may affect liveOut
            // of others, which may expose more dead stores.
            changed = true;
        }
    }

    return useDefRemoved && changed;
}

#ifdef DEBUG

void Liveness::DumpBlockLiveness(BasicBlock* block)
{
    LiveSet allVars = LiveSetOps::Alloc(this);
    LiveSetOps::Union(this, allVars, block->bbVarUse, block->bbVarDef);

    printf(FMT_BB ":\nUSE = ", block->bbNum);
    compiler->lvaDispVarSet(block->bbVarUse, allVars);

    if (!block->IsLIR())
    {
        if (block->bbMemoryUse)
        {
            printf(" + Memory");
        }
    }

    printf("\nDEF = ");
    compiler->lvaDispVarSet(block->bbVarDef, allVars);

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
    LiveSet allVars = LiveSetOps::Alloc(this);
    LiveSetOps::Union(this, allVars, block->bbLiveIn, block->bbLiveOut);

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

void Compiler::livInitNewBlock(BasicBlock* block)
{
    if (lvaTrackedCount != 0)
    {
        block->bbVarUse  = LiveSetOps::MakeEmpty(this);
        block->bbVarDef  = LiveSetOps::MakeEmpty(this);
        block->bbLiveIn  = LiveSetOps::MakeEmpty(this);
        block->bbLiveOut = LiveSetOps::MakeEmpty(this);
    }

    block->bbMemoryUse     = false;
    block->bbMemoryDef     = false;
    block->bbMemoryLiveIn  = false;
    block->bbMemoryLiveOut = false;
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
    Liveness livenss(this);
    livenss.Compute();
}
