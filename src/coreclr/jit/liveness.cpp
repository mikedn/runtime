// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "lower.h"

void Compiler::fgMarkUseDef(GenTreeLclVarCommon* node)
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
        lcl->setLvRefCnt(1);
    }

    const bool isDef = (node->gtFlags & GTF_VAR_DEF) != 0;
    const bool isUse = !isDef || ((node->gtFlags & GTF_VAR_USEASG) != 0);

    assert(isDef || isUse);

    if (lcl->HasLiveness())
    {
        if (isUse && !VarSetOps::IsMember(this, fgCurDefSet, lcl->lvVarIndex))
        {
            VarSetOps::AddElemD(this, fgCurUseSet, lcl->lvVarIndex);
        }

        if (isDef)
        {
            VarSetOps::AddElemD(this, fgCurDefSet, lcl->lvVarIndex);
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

        if (isFieldUse && !VarSetOps::IsMember(this, fgCurDefSet, fieldLcl->GetLivenessBitIndex()))
        {
            VarSetOps::AddElemD(this, fgCurUseSet, fieldLcl->GetLivenessBitIndex());
        }

        if (isDef)
        {
            VarSetOps::AddElemD(this, fgCurDefSet, fieldLcl->GetLivenessBitIndex());
        }
    }
}

void Compiler::fgLocalVarLivenessAlwaysLive()
{
    assert(compRationalIRForm);

    // TODO-MIKE-Review: Check if this is really needed in minopts.
    for (unsigned lclNum = 0; lclNum < lvaCount; lclNum++)
    {
        lvaGetDesc(lclNum)->lvMustInit = false;
    }

    for (BasicBlock* const block : Blocks())
    {
        // Strictly speaking, the assignments for the "Def" cases aren't necessary here.
        // The empty set would do as well.  Use means "use-before-def", so as long as that's
        // "all", this has the right effect.

        block->bbVarUse = VarSetOps::MakeFull(this);
        block->bbVarDef = VarSetOps::MakeFull(this);
        block->bbLiveIn = VarSetOps::MakeFull(this);

        switch (block->bbJumpKind)
        {
            case BBJ_EHFINALLYRET:
            case BBJ_THROW:
            case BBJ_RETURN:
                block->bbLiveOut = VarSetOps::MakeEmpty(this);
                break;
            default:
                block->bbLiveOut = VarSetOps::MakeFull(this);
                break;
        }
    }

    fgBBVarSetsInited = true;

    if (opts.compDbgCode && (info.compVarScopesCount > 0))
    {
        fgExtendDbgLifetimes();
    }

    fgLocalVarLivenessDone = true;
}

void Compiler::fgLocalVarLiveness()
{
#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In fgLocalVarLiveness()\n");

        if (compRationalIRForm)
        {
            lvaTableDump();
        }
    }
#endif // DEBUG

    fgLocalVarLivenessInit();

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

    fgBBVarSetsInited = true;

    for (bool changed = true; changed;)
    {
        if (compRationalIRForm)
        {
            if (lvaTrackedCount != 0)
            {
                assert(!opts.compDbgCode);

                fgPerBlockLocalVarLivenessLIR();
                fgLiveVarAnalysis();
            }
            else if (opts.compDbgCode && (info.compVarScopesCount > 0))
            {
                fgExtendDbgLifetimes();
            }
        }
        else
        {
            assert(opts.OptimizationEnabled());

            fgPerBlockLocalVarLiveness();
            fgLiveVarAnalysis();
        }

        if (lvaTrackedCount == 0)
        {
            fgInterBlockLocalVarLivenessUntracked();
            changed = false;
        }
        else
        {
            changed = fgInterBlockLocalVarLiveness();
        }
    }

    EndPhase(PHASE_LCLVARLIVENESS);
}

void Compiler::livInitNewBlock(BasicBlock* block)
{
    // We will give all the blocks var sets after the number of tracked variables
    // is determined and frozen.  After that, if we dynamically create a basic block,
    // we will initialize its var sets.
    if (fgBBVarSetsInited)
    {
        VarSetOps::AssignNoCopy(this, block->bbVarUse, VarSetOps::MakeEmpty(this));
        VarSetOps::AssignNoCopy(this, block->bbVarDef, VarSetOps::MakeEmpty(this));
        VarSetOps::AssignNoCopy(this, block->bbLiveIn, VarSetOps::MakeEmpty(this));
        VarSetOps::AssignNoCopy(this, block->bbLiveOut, VarSetOps::MakeEmpty(this));
    }
    else
    {
        VarSetOps::AssignNoCopy(this, block->bbVarUse, VarSetOps::UninitVal());
        VarSetOps::AssignNoCopy(this, block->bbVarDef, VarSetOps::UninitVal());
        VarSetOps::AssignNoCopy(this, block->bbLiveIn, VarSetOps::UninitVal());
        VarSetOps::AssignNoCopy(this, block->bbLiveOut, VarSetOps::UninitVal());
    }

    block->bbMemoryUse     = false;
    block->bbMemoryDef     = false;
    block->bbMemoryLiveIn  = false;
    block->bbMemoryLiveOut = false;
}

void Compiler::fgLocalVarLivenessInit()
{
    JITDUMP("In fgLocalVarLivenessInit\n");

    // We mark a lcl as must-init in a first pass of local variable
    // liveness (Liveness1), then assertion prop eliminates the
    // uninit-use of a variable Vk, asserting it will be init'ed to
    // null.  Then, in a second local-var liveness (Liveness2), the
    // variable Vk is no longer live on entry to the method, since its
    // uses have been replaced via constant propagation.
    //
    // This leads to a bug: since Vk is no longer live on entry, the
    // register allocator sees Vk and an argument Vj as having
    // disjoint lifetimes, and allocates them to the same register.
    // But Vk is still marked "must-init", and this initialization (of
    // the register) trashes the value in Vj.
    //
    // Therefore, initialize must-init to false for all variables in
    // each liveness phase.
    for (unsigned lclNum = 0; lclNum < lvaCount; ++lclNum)
    {
        lvaTable[lclNum].lvMustInit = false;
    }
}

void Compiler::fgPerNodeLocalVarLiveness(GenTree* tree)
{
    switch (tree->GetOper())
    {
        case GT_LCL_VAR:
        case GT_LCL_FLD:
            if ((tree->gtFlags & GTF_VAR_DEF) == 0)
            {
                if (lvaGetDesc(tree->AsLclVarCommon())->IsAddressExposed())
                {
                    fgCurMemoryUse = true;
                }
                else
                {
                    fgMarkUseDef(tree->AsLclVarCommon());
                }
            }
            break;

        case GT_LCL_VAR_ADDR:
        case GT_LCL_FLD_ADDR:
            assert(lvaGetDesc(tree->AsLclVarCommon())->IsAddressExposed());
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
                fgCurMemoryDef = true;
            }

            // If the GT_IND is the lhs of an assignment, we'll handle it
            // as a memory def, when we get to assignment.
            // Otherwise, we treat it as a use here.
            if ((tree->gtFlags & GTF_IND_ASG_LHS) == 0)
            {
                GenTree* addr = tree->AsIndir()->GetAddr()->SkipComma();

                if (GenTreeLclVarCommon* lclNode = addr->IsLocalAddrExpr())
                {
                    assert(lvaGetDesc(lclNode)->IsAddressExposed());
                }

                fgCurMemoryUse = true;
            }
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
            fgCurMemoryUse   = true;
            fgCurMemoryDef   = true;
            fgCurMemoryHavoc = true;
            break;

        case GT_MEMORYBARRIER:
            // Simliar to any Volatile indirection, we must handle this as a definition of GcHeap/ByrefExposed
            fgCurMemoryDef = true;
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
        {
            GenTreeHWIntrinsic* hwIntrinsicNode = tree->AsHWIntrinsic();

            // We can't call vnClearMemory unless the block has recorded a MemoryDef
            //
            if (hwIntrinsicNode->OperIsMemoryStore())
            {
                fgCurMemoryDef = true;
            }
            if (hwIntrinsicNode->OperIsMemoryLoad())
            {
                fgCurMemoryUse = true;
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
                fgCurMemoryUse   = true;
                fgCurMemoryDef   = true;
                fgCurMemoryHavoc = true;
            }

            fgPInvokeFrameLiveness(call);
            break;
        }

        case GT_ASG:
            if (tree->AsOp()->GetOp(0)->OperIs(GT_LCL_VAR, GT_LCL_FLD))
            {
                GenTreeLclVarCommon* lclNode = tree->AsOp()->GetOp(0)->AsLclVarCommon();

                if (lvaGetDesc(lclNode)->IsAddressExposed())
                {
                    fgCurMemoryDef = true;
                    break;
                }

                fgMarkUseDef(lclNode);
                break;
            }

            if (GenTreeIndir* indir = tree->AsOp()->GetOp(0)->IsIndir())
            {
                if (GenTreeLclVarCommon* lclNode = indir->GetAddr()->IsLocalAddrExpr())
                {
                    assert(lvaGetDesc(lclNode)->IsAddressExposed());
                }
            }

            fgCurMemoryDef = true;
            break;

        case GT_QMARK:
        case GT_STORE_LCL_VAR:
        case GT_STORE_LCL_FLD:
        case GT_STORE_OBJ:
        case GT_STORE_BLK:
        case GT_STOREIND:
            unreached();

        default:
            break;
    }
}

void Compiler::fgPInvokeFrameLiveness(GenTreeCall* call)
{
    assert(!compRationalIRForm);

    // If this is a tail-call via helper, and we have any unmanaged P/Invoke calls in
    // the method, then we're going to run the P/Invoke epilog, which uses the frame
    // list root local too.

    if ((info.compLvFrameListRoot != BAD_VAR_NUM) && (call->RequiresPInvokeFrame() || call->IsTailCallViaJitHelper()))
    {
        LclVarDsc* lcl = lvaGetDesc(info.compLvFrameListRoot);

        if (lcl->HasLiveness() && !VarSetOps::IsMember(this, fgCurDefSet, lcl->GetLivenessBitIndex()))
        {
            VarSetOps::AddElemD(this, fgCurUseSet, lcl->GetLivenessBitIndex());
        }
    }
}

void Compiler::fgPerBlockLocalVarLiveness()
{
    assert(!compRationalIRForm);

    for (BasicBlock* block : Blocks())
    {
        fgCurUseSet = block->bbVarUse;
        fgCurDefSet = block->bbVarDef;

        VarSetOps::ClearD(this, fgCurUseSet);
        VarSetOps::ClearD(this, fgCurDefSet);

        fgCurMemoryUse   = false;
        fgCurMemoryDef   = false;
        fgCurMemoryHavoc = false;

        compCurBB = block;

        for (Statement* const stmt : block->NonPhiStatements())
        {
            compCurStmt = stmt;

            for (GenTree* const node : stmt->Nodes())
            {
                fgPerNodeLocalVarLiveness(node);
            }
        }

        // Mark the FrameListRoot as used, if applicable.

        if ((block->bbJumpKind == BBJ_RETURN) && (info.compLvFrameListRoot != BAD_VAR_NUM))
        {
#ifdef TARGET_64BIT
            // 32-bit targets always pop the frame in the epilog.
            // For 64-bit targets, we only do this in the epilog for IL stubs;
            // for non-IL stubs the frame is popped after every PInvoke call.
            if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_IL_STUB))
#endif
            {
                LclVarDsc* lcl = lvaGetDesc(info.compLvFrameListRoot);

                if (lcl->HasLiveness())
                {
                    VarSetOps::AddElemD(this, fgCurUseSet, lcl->GetLivenessBitIndex());
                }
            }
        }

        block->bbVarUse = fgCurUseSet;
        block->bbVarDef = fgCurDefSet;

        block->bbMemoryUse   = fgCurMemoryUse;
        block->bbMemoryDef   = fgCurMemoryDef;
        block->bbMemoryHavoc = fgCurMemoryHavoc;

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
        fgCurUseSet = block->bbVarUse;
        fgCurDefSet = block->bbVarDef;

        VarSetOps::ClearD(this, fgCurUseSet);
        VarSetOps::ClearD(this, fgCurDefSet);

        for (GenTree* node : LIR::AsRange(block))
        {
            if (node->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
            {
                if (!lvaGetDesc(node->AsLclVarCommon())->IsAddressExposed())
                {
                    fgMarkUseDef(node->AsLclVarCommon());
                }
            }
            else if (node->OperIs(GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR))
            {
                assert(lvaGetDesc(node->AsLclVarCommon())->IsAddressExposed());
            }
        }

        block->bbVarUse = fgCurUseSet;
        block->bbVarDef = fgCurDefSet;

        // Also clear the IN set, just in case we will do multiple DFAs
        VarSetOps::ClearD(this, block->bbLiveIn);

        DBEXEC(verbose, fgDispBBLocalLiveness(block))
    }
}

void Compiler::fgExtendDbgLifetimes()
{
    assert(compRationalIRForm && opts.compDbgCode && (info.compVarScopesCount > 0) && (lvaTrackedCount == 0));

    // raMarkStkVars() reserves stack space for unused variables (which needs
    // to be initialized). However, arguments don't need to be initialized.
    // So just ensure that they don't have a 0 ref cnt

    for (unsigned lclNum = 0; lclNum < info.compArgsCount; lclNum++)
    {
        LclVarDsc* lcl = lvaGetDesc(lclNum);

        if (lcl->IsRegParam())
        {
            lcl->lvImplicitlyReferenced = true;
        }
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
        /* Compute the 'liveOut' set */
        VarSetOps::ClearD(m_compiler, m_liveOut);
        m_memoryLiveOut = false;
        if (block->endsWithJmpMethod(m_compiler))
        {
            // A JMP uses all the arguments, so mark them all
            // as live at the JMP instruction
            //
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

/*****************************************************************************
 *
 *  This is the classic algorithm for Live Variable Analysis.
 *  If updateInternalOnly==true, only update BBF_INTERNAL blocks.
 */

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

void Compiler::fgComputeLifeCall(VARSET_TP& life, GenTreeCall* call)
{
    assert(!compRationalIRForm);

    // TODO: we should generate the code for saving to/restoring from the inlined N/Direct
    // frame instead.

    // If this is a tail-call via helper, and we have any unmanaged P/Invoke calls in
    // the method, then we're going to run the P/Invoke epilog, which uses the frame
    // list root local too.

    if ((info.compLvFrameListRoot != BAD_VAR_NUM) && (call->RequiresPInvokeFrame() || call->IsTailCallViaJitHelper()))
    {
        LclVarDsc* lcl = lvaGetDesc(info.compLvFrameListRoot);

        if (lcl->HasLiveness())
        {
            VarSetOps::AddElemD(this, life, lcl->GetLivenessBitIndex());
        }
    }
}

void Compiler::fgComputeLifeTrackedLocalUse(VARSET_TP& liveOut, LclVarDsc* lcl, GenTreeLclVarCommon* node)
{
    assert(node->OperIs(GT_LCL_VAR, GT_LCL_FLD));
    assert((node->gtFlags & GTF_VAR_DEF) == 0);

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
    assert(node->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));
    assert((node->gtFlags & GTF_VAR_DEF) != 0);

    const unsigned index = lcl->GetLivenessBitIndex();

    if (VarSetOps::IsMember(this, liveOut, index))
    {
        if ((node->gtFlags & GTF_VAR_USEASG) == 0)
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

bool Compiler::fgComputeLifeUntrackedLocal(VARSET_TP&           liveOut,
                                           VARSET_VALARG_TP     keepAlive,
                                           LclVarDsc*           lcl,
                                           GenTreeLclVarCommon* node)
{
    assert(node->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    bool isDef = ((node->gtFlags & GTF_VAR_DEF) != 0);

    // We have accurate ref counts when running late liveness so we can eliminate
    // some stores if the local has a ref count of 1.
    if (isDef && compRationalIRForm && (lcl->lvRefCnt() == 1) && !lcl->lvPinned)
    {
        if (lcl->IsPromotedField())
        {
            LclVarDsc* parentLcl = lvaGetDesc(lcl->GetPromotedFieldParentLclNum());

            if ((parentLcl->lvRefCnt() == 1) && parentLcl->IsDependentPromoted())
            {
                return true;
            }
        }
        else
        {
            if (!lcl->IsIndependentPromoted())
            {
                return true;
            }
        }
    }

    if (lcl->IsAddressExposed() || !lcl->IsPromoted())
    {
        return false;
    }

    unsigned lclOffset    = 0;
    unsigned lclEndOffset = lcl->TypeIs(TYP_STRUCT) ? lcl->GetLayout()->GetSize() : varTypeSize(lcl->GetType());

    if (GenTreeLclFld* lclFld = node->IsLclFld())
    {
        lclOffset    = lclFld->GetLclOffs();
        lclEndOffset = lclOffset + (lclFld->TypeIs(TYP_STRUCT) ? lclFld->GetLayout(this)->GetSize()
                                                               : varTypeSize(lclFld->GetType()));
    }

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

void Compiler::fgComputeLifeStmt(VARSET_TP& liveOut, VARSET_VALARG_TP keepAlive, Statement* stmt)
{
    bool updateStmt = false;
    INDEBUG(bool modified = false;)

    noway_assert(VarSetOps::IsSubset(this, keepAlive, liveOut));

    for (GenTree* node = stmt->GetRootNode(); node != nullptr;)
    {
        if (node->OperIs(GT_CALL))
        {
            fgComputeLifeCall(liveOut, node->AsCall());
        }
        else if (node->OperIs(GT_LCL_VAR, GT_LCL_FLD) && ((node->gtFlags & GTF_VAR_DEF) == 0))
        {
            GenTreeLclVarCommon* lclNode = node->AsLclVarCommon();
            LclVarDsc*           lcl     = lvaGetDesc(lclNode);

            if (lcl->HasLiveness())
            {
                fgComputeLifeTrackedLocalUse(liveOut, lcl, lclNode);
            }
            else
            {
                bool isDeadStore = fgComputeLifeUntrackedLocal(liveOut, keepAlive, lcl, lclNode);
                assert(!isDeadStore);
            }
        }
        else if (node->OperIs(GT_ASG))
        {
            if (GenTreeLclVarCommon* lclNode = node->AsOp()->GetOp(0)->SkipComma()->IsLclVarCommon())
            {
                assert((lclNode->gtFlags & GTF_VAR_DEF) != 0);

                LclVarDsc* lcl = lvaGetDesc(lclNode);
                bool       isDeadStore;

                if (lcl->HasLiveness())
                {
                    isDeadStore = fgComputeLifeTrackedLocalDef(liveOut, keepAlive, lcl, lclNode);
                }
                else
                {
                    isDeadStore = fgComputeLifeUntrackedLocal(liveOut, keepAlive, lcl, lclNode);
                }

                if (isDeadStore)
                {
                    GenTree* nextNode = fgRemoveDeadStore(node->AsOp());

                    if (nextNode == nullptr)
                    {
                        // The entire statement was removed, we're done.
                        return;
                    }

                    if (nextNode == stmt->GetRootNode())
                    {
                        // The statement root was replaced, sequence the entire statement
                        // and start over.

                        updateStmt = false;
                        gtSetStmtInfo(stmt);
                    }
                    else
                    {
                        // The statement was modified. We need to sequence it so we can
                        // continue the current linear order traversal but we can't call
                        // gtSetStmtInfo because that would change evaluation order and
                        // mess up the current traversal. Instead, call gtSetStmtInfo
                        // after traversal is complete.

                        updateStmt = true;
                    }

                    INDEBUG(modified = true;)
                    fgSetStmtSeq(stmt);
                    node = nextNode;

                    continue;
                }
            }
        }

        node = node->gtPrev;
    }

    if (updateStmt)
    {
        gtSetStmtInfo(stmt);
        fgSetStmtSeq(stmt);
        gtUpdateStmtSideEffects(stmt);
    }

#ifdef DEBUG
    if (modified)
    {
        JITDUMPTREE(stmt->GetRootNode(), "\nfgComputeLifeStmt modified tree:\n");
    }
#endif
}

void Compiler::fgComputeLifeLIR(VARSET_TP& life, VARSET_VALARG_TP keepAliveVars, BasicBlock* block)
{
    noway_assert(VarSetOps::IsSubset(this, keepAliveVars, life));

    LIR::Range& blockRange = LIR::AsRange(block);
    GenTree*    firstNode  = blockRange.FirstNode();
    if (firstNode == nullptr)
    {
        return;
    }
    for (GenTree *node = blockRange.LastNode(), *next = nullptr, *end = firstNode->gtPrev; node != end; node = next)
    {
        next = node->gtPrev;

        switch (node->OperGet())
        {
            case GT_CALL:
            {
                GenTreeCall* const call = node->AsCall();
                if (((call->TypeGet() == TYP_VOID) || call->IsUnusedValue()) && !call->HasSideEffects(this))
                {
                    JITDUMP("Removing dead call:\n");
                    DISPNODE(call);

                    node->VisitOperands([](GenTree* operand) -> GenTree::VisitResult {
                        if (operand->IsValue())
                        {
                            operand->SetUnusedValue();
                        }

                        // Special-case PUTARG_STK: since this operator is not considered a value, DCE will not remove
                        // these nodes.
                        if (operand->OperIs(GT_PUTARG_STK))
                        {
                            operand->AsPutArgStk()->gtOp1->SetUnusedValue();
                            operand->ChangeToNothingNode();
                        }

                        return GenTree::VisitResult::Continue;
                    });

                    blockRange.Remove(node);
                }
                break;
            }

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

                    if (lcl->lvTracked && !opts.MinOpts())
                    {
                        fgStmtRemoved = true;
                    }
                }
                else if (lcl->lvTracked)
                {
                    fgComputeLifeTrackedLocalUse(life, lcl, lclNode);
                }
                else
                {
                    fgComputeLifeUntrackedLocal(life, keepAliveVars, lcl, lclNode);
                }
                break;
            }

            case GT_LCL_VAR_ADDR:
            case GT_LCL_FLD_ADDR:
                assert(lvaGetDesc(node->AsLclVarCommon())->IsAddressExposed());

                if (node->IsUnusedValue())
                {
                    JITDUMP("Removing dead LclVar address:\n");
                    DISPNODE(node);

                    blockRange.Delete(this, block, node);
                }
                break;

            case GT_STORE_LCL_VAR:
            case GT_STORE_LCL_FLD:
            {
                GenTreeLclVarCommon* lclNode = node->AsLclVarCommon();
                LclVarDsc*           lcl     = lvaGetDesc(lclNode);
                bool                 isDeadStore;

                if (lcl->lvTracked)
                {
                    isDeadStore = fgComputeLifeTrackedLocalDef(life, keepAliveVars, lcl, lclNode);
                }
                else
                {
                    isDeadStore = fgComputeLifeUntrackedLocal(life, keepAliveVars, lcl, lclNode);
                }

                if (isDeadStore)
                {
                    JITDUMP("Removing dead store:\n");
                    DISPNODE(lclNode);

                    // Remove the store. DCE will iteratively clean up any ununsed operands.
                    lclNode->gtOp1->SetUnusedValue();

                    fgRemoveDeadStoreLIR(node, block);
                }

                break;
            }

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

            case GT_LOCKADD:
            case GT_XORR:
            case GT_XAND:
            case GT_XADD:
            case GT_XCHG:
            case GT_CMPXCHG:
            case GT_MEMORYBARRIER:
            case GT_JMP:
            case GT_STOREIND:
            case GT_ARR_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
            case GT_HW_INTRINSIC_CHK:
#endif
            case GT_STORE_OBJ:
            case GT_STORE_BLK:
            case GT_COPY_BLK:
            case GT_INIT_BLK:
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
#if !defined(FEATURE_EH_FUNCLETS)
            case GT_END_LFIN:
#endif // !FEATURE_EH_FUNCLETS
            case GT_SWITCH_TABLE:
            case GT_PINVOKE_PROLOG:
            case GT_PINVOKE_EPILOG:
            case GT_RETURNTRAP:
            case GT_PUTARG_STK:
            case GT_IL_OFFSET:
            case GT_KEEPALIVE:
#ifdef FEATURE_HW_INTRINSICS
            case GT_HWINTRINSIC:
#endif // FEATURE_HW_INTRINSICS
                // Never remove these nodes, as they are always side-effecting.
                //
                // NOTE: the only side-effect of some of these nodes (GT_CMP, GT_SUB_HI) is a write to the flags
                // register.
                // Properly modeling this would allow these nodes to be removed.
                break;

            case GT_NOP:
            {
                // NOTE: we need to keep some NOPs around because they are referenced by calls. See the dead store
                // removal code above (case GT_STORE_LCL_VAR) for more explanation.
                if ((node->gtFlags & GTF_ORDER_SIDEEFF) != 0)
                {
                    break;
                }
                fgTryRemoveNonLocal(node, &blockRange);
            }
            break;

            case GT_BLK:
            case GT_OBJ:
            {
                bool removed = fgTryRemoveNonLocal(node, &blockRange);
                if (!removed && node->IsUnusedValue())
                {
                    // IR doesn't expect dummy uses of `GT_OBJ/BLK`.
                    JITDUMP("Transform an unused OBJ/BLK node [%06d]\n", dspTreeID(node));
                    Lowering::TransformUnusedIndirection(node->AsIndir(), this, block);
                }
            }
            break;

            default:
                fgTryRemoveNonLocal(node, &blockRange);
                break;
        }
    }
}

//---------------------------------------------------------------------
// fgTryRemoveNonLocal - try to remove a node if it is unused and has no direct
//   side effects.
//
// Arguments
//    node       - the non-local node to try;
//    blockRange - the block range that contains the node.
//
// Return value:
//    None
//
// Notes: local nodes are processed independently and are not expected in this function.
//
bool Compiler::fgTryRemoveNonLocal(GenTree* node, LIR::Range* blockRange)
{
    assert(!node->OperIsLocal());
    if (!node->IsValue() || node->IsUnusedValue())
    {
        // We are only interested in avoiding the removal of nodes with direct side effects
        // (as opposed to side effects of their children).
        // This default case should never include calls or assignments.
        assert(!node->OperRequiresAsgFlag() && !node->OperIs(GT_CALL));
        if (((node->gtFlags & GTF_SET_FLAGS) == 0) && !node->OperMayThrow(this))
        {
            JITDUMP("Removing dead node:\n");
            DISPNODE(node);

            node->VisitOperands([](GenTree* operand) -> GenTree::VisitResult {
                operand->SetUnusedValue();
                return GenTree::VisitResult::Continue;
            });

            blockRange->Remove(node);
            return true;
        }
    }
    return false;
}

void Compiler::fgRemoveDeadStoreLIR(GenTree* store, BasicBlock* block)
{
    assert(!opts.MinOpts());

    LIR::Range& blockRange = LIR::AsRange(block);

#ifdef DEBUG
    LIR::Use use;
    assert(!blockRange.TryGetUse(store, &use));
#endif

    blockRange.Remove(store);
    fgStmtRemoved = true;
}

// Remove a dead assignment. Returns true if the entire statement was removed.
GenTree* Compiler::fgRemoveDeadStore(GenTreeOp* asgNode)
{
    assert(!compRationalIRForm);
    assert(asgNode->OperIs(GT_ASG));

    JITDUMPTREE(asgNode, "Dead assignment:\n");

    GenTree* sideEffects = nullptr;

    if ((asgNode->GetOp(1)->gtFlags & GTF_SIDE_EFFECT) != 0)
    {
        gtExtractSideEffList(asgNode->GetOp(1), &sideEffects);

        if (sideEffects != nullptr)
        {
            JITDUMPTREE(sideEffects, "Extracted dead assignment side effects:\n");
            noway_assert((sideEffects->gtFlags & GTF_SIDE_EFFECT) != 0);
        }
    }

    if (asgNode->gtNext == nullptr)
    {
        // This is a top level assignment, we can remove the entire statement
        // if there are no side effects.

        noway_assert(compCurStmt->GetRootNode() == asgNode);

        if (sideEffects == nullptr)
        {
            fgRemoveStmt(compCurBB, compCurStmt DEBUGARG(false));

            return nullptr;
        }

        // Replace the assignment statement with the list of side effects
        // and process the statement again.
        compCurStmt->SetRootNode(sideEffects);
        return sideEffects;
    }

    // This is a nested assignment, we can change it to a NOP/COMMA.

    if (sideEffects == nullptr)
    {
        asgNode->ChangeToNothingNode();
    }
    else if (sideEffects->OperIs(GT_ASG))
    {
        asgNode->SetOp(0, sideEffects->AsOp()->GetOp(0));
        asgNode->SetOp(1, sideEffects->AsOp()->GetOp(1));
        asgNode->SetType(sideEffects->GetType());
        asgNode->SetSideEffects(sideEffects->GetSideEffects());
        asgNode->SetReverseOps(sideEffects->IsReverseOp());
    }
    else
    {
        asgNode->SetOper(GT_COMMA);
        asgNode->SetType(TYP_VOID);

        if (sideEffects->OperIs(GT_COMMA))
        {
            asgNode->SetOp(0, sideEffects->AsOp()->GetOp(0));
            asgNode->SetOp(1, sideEffects->AsOp()->GetOp(1));
            asgNode->SetReverseOps(sideEffects->IsReverseOp());
        }
        else
        {
            asgNode->SetOp(0, sideEffects);
            asgNode->SetOp(1, gtNewNothingNode());
            asgNode->SetReverseOps(false);
        }

        asgNode->SetSideEffects(sideEffects->GetSideEffects());
    }

    return asgNode;
}

void Compiler::fgInterBlockLocalVarLivenessUntracked()
{
    assert(lvaTrackedCount == 0);

    fgStmtRemoved = false;

    VARSET_TP keepAlive = VarSetOps::UninitVal();
    VARSET_TP life      = VarSetOps::UninitVal();

    for (BasicBlock* const block : Blocks())
    {
        compCurBB = block;

        if (block->IsLIR())
        {
            fgComputeLifeLIR(life, keepAlive, block);
        }
        else
        {
            Statement* firstStmt = block->FirstNonPhiDef();

            if (firstStmt == nullptr)
            {
                continue;
            }

            Statement* nextStmt = block->lastStmt();

            do
            {
                noway_assert(nextStmt != nullptr);

                compCurStmt = nextStmt;
                nextStmt    = nextStmt->GetPrevStmt();

                fgComputeLifeStmt(life, keepAlive, compCurStmt);
            } while (compCurStmt != firstStmt);
        }

        noway_assert(compCurBB == block);
        INDEBUG(compCurBB = nullptr);
    }

    fgLocalVarLivenessDone = true;
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
            lvaSetVarLiveInOutOfHandler(lclNum);

            if (isFinallyLiveOut && !lcl->IsParam() && varTypeIsGC(lcl->TypeGet()))
            {
                lcl->lvMustInit = true;
            }
        }
    }

    fgStmtRemoved       = false;
    bool      changed   = false;
    VARSET_TP keepAlive = VarSetOps::MakeEmpty(this);
    VARSET_TP life      = VarSetOps::MakeEmpty(this);

    for (BasicBlock* const block : Blocks())
    {
        compCurBB = block;

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

        if (block->IsLIR())
        {
            fgComputeLifeLIR(life, keepAlive, block);
        }
        else
        {
            Statement* firstStmt = block->FirstNonPhiDef();

            if (firstStmt == nullptr)
            {
                continue;
            }

            Statement* nextStmt = block->lastStmt();

            do
            {
                noway_assert(nextStmt != nullptr);

                compCurStmt = nextStmt;
                nextStmt    = nextStmt->GetPrevStmt();

                fgComputeLifeStmt(life, keepAlive, compCurStmt);
            } while (compCurStmt != firstStmt);
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

        noway_assert(compCurBB == block);
        INDEBUG(compCurBB = nullptr);
    }

    fgLocalVarLivenessDone = true;

    return fgStmtRemoved && changed;
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
