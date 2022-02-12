// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#ifndef TARGET_64BIT
#include "decomposelongs.h"
#endif
#include "lower.h"

void Compiler::fgMarkUseDef(GenTreeLclVarCommon* node)
{
    assert(node->OperIs(GT_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    LclVarDsc* lcl = lvaGetDesc(node);

    assert(!lcl->IsAddressExposed());

    // We should never encounter a reference to a local that has a zero ref count.
    if ((lcl->lvRefCnt() == 0) && (!varTypeIsPromotable(lcl->GetType()) || !lcl->IsPromoted()))
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

    if (lcl->IsPromoted())
    {
        // TODO-MIKE-Cleanup: This is kind of strange because it doesn't bother to
        // check which fields actually overlap a LCL_FLD access. Since a LCL_FLD
        // which doesn't overlap all fields is supposed to be "USEASG", this should
        // be correct - all fields are always used so we don't risk eliminating
        // previous stores as dead. But this means we also can't eliminate stores
        // that are truly dead due to such false uses.
        //
        // It wouldn't be too difficult to check which fields overlap but it's not
        // clear if it's worth the trouble. A common source of such partial access
        // are multi-reg call args/returns but those end up using the entire struct
        // anyway (e.g. 2 partial LCL_FLDs to load the entire struct in 2 registers).
        //
        // And then the question is if there's any need to preserve P-DEP promotion.
        // Such fields are not in SSA and are not enregistered so probably the only
        // advantage they may have is dead store elimination, and we don't do that.
        // If we undo P-DEP then the struct local will be in SSA (assuming that
        // P-DEP wasn't due to the local being address-exposed) so value numbering
        // works too and may allow certain optimizations to be performed.

        for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
        {
            LclVarDsc* fieldLcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

            if (fieldLcl->HasLiveness())
            {
                if (isUse && !VarSetOps::IsMember(this, fgCurDefSet, fieldLcl->lvVarIndex))
                {
                    VarSetOps::AddElemD(this, fgCurUseSet, fieldLcl->lvVarIndex);
                }

                if (isDef)
                {
                    VarSetOps::AddElemD(this, fgCurDefSet, fieldLcl->lvVarIndex);
                }
            }
        }
    }
}

/*****************************************************************************/
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

    // Init liveness data structures.
    fgLocalVarLivenessInit();

    EndPhase(PHASE_LCLVARLIVENESS_INIT);

    // Make sure we haven't noted any partial last uses of promoted structs.
    ClearPromotedStructDeathVars();

    // Initialize the per-block var sets.
    fgInitBlockVarSets();

    fgLocalVarLivenessChanged = false;
    do
    {
        /* Figure out use/def info for all basic blocks */
        fgPerBlockLocalVarLiveness();
        EndPhase(PHASE_LCLVARLIVENESS_PERBLOCK);

        /* Live variable analysis. */

        fgStmtRemoved = false;
        fgInterBlockLocalVarLiveness();
    } while (fgStmtRemoved && fgLocalVarLivenessChanged);

    EndPhase(PHASE_LCLVARLIVENESS_INTERBLOCK);
}

/*****************************************************************************/
void Compiler::fgLocalVarLivenessInit()
{
    JITDUMP("In fgLocalVarLivenessInit\n");

    // Sort locals first, if we're optimizing
    if (opts.OptimizationEnabled())
    {
        lvaSortByRefCount();
    }

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
                    fgCurMemoryUse |= memoryKindSet(ByrefExposed);
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
                // For any Volatile indirection, we must handle it as a
                // definition of the GcHeap/ByrefExposed
                fgCurMemoryDef |= memoryKindSet(GcHeap, ByrefExposed);
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
                    fgCurMemoryUse |= memoryKindSet(ByrefExposed);
                }
                else
                {
                    fgCurMemoryUse |= memoryKindSet(GcHeap, ByrefExposed);
                }
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
            fgCurMemoryUse |= memoryKindSet(GcHeap, ByrefExposed);
            fgCurMemoryDef |= memoryKindSet(GcHeap, ByrefExposed);
            fgCurMemoryHavoc |= memoryKindSet(GcHeap, ByrefExposed);
            break;

        case GT_MEMORYBARRIER:
            // Simliar to any Volatile indirection, we must handle this as a definition of GcHeap/ByrefExposed
            fgCurMemoryDef |= memoryKindSet(GcHeap, ByrefExposed);
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
        {
            GenTreeHWIntrinsic* hwIntrinsicNode = tree->AsHWIntrinsic();

            // We can't call fgMutateGcHeap unless the block has recorded a MemoryDef
            //
            if (hwIntrinsicNode->OperIsMemoryStore())
            {
                // We currently handle this like a Volatile store, so it counts as a definition of GcHeap/ByrefExposed
                fgCurMemoryDef |= memoryKindSet(GcHeap, ByrefExposed);
            }
            if (hwIntrinsicNode->OperIsMemoryLoad())
            {
                // This instruction loads from memory and we need to record this information
                fgCurMemoryUse |= memoryKindSet(GcHeap, ByrefExposed);
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
                fgCurMemoryUse |= memoryKindSet(GcHeap, ByrefExposed);
                fgCurMemoryDef |= memoryKindSet(GcHeap, ByrefExposed);
                fgCurMemoryHavoc |= memoryKindSet(GcHeap, ByrefExposed);
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
                    fgCurMemoryDef |= memoryKindSet(ByrefExposed);
                    byrefStatesMatchGcHeapStates = false;
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
                    fgCurMemoryDef |= memoryKindSet(ByrefExposed);
                    byrefStatesMatchGcHeapStates = false;
                    break;
                }
            }

            fgCurMemoryDef |= memoryKindSet(GcHeap, ByrefExposed);
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

void Compiler::fgPerNodeLocalVarLivenessLIR(GenTree* tree)
{
    switch (tree->GetOper())
    {
        case GT_LCL_VAR:
        case GT_LCL_FLD:
        case GT_STORE_LCL_VAR:
        case GT_STORE_LCL_FLD:
            if (!lvaGetDesc(tree->AsLclVarCommon())->IsAddressExposed())
            {
                fgMarkUseDef(tree->AsLclVarCommon());
            }
            break;

        case GT_LCL_VAR_ADDR:
        case GT_LCL_FLD_ADDR:
            assert(lvaGetDesc(tree->AsLclVarCommon())->IsAddressExposed());
            break;

        case GT_CALL:
            fgPInvokeFrameLiveness(tree->AsCall());
            break;

        default:
            break;
    }
}

void Compiler::fgPInvokeFrameLiveness(GenTreeCall* call)
{
    // If this is a p/invoke unmanaged call or if this is a tail-call via helper,
    // and we have an unmanaged p/invoke call in the method,
    // then we're going to run the p/invoke epilog.
    // So we mark the FrameRoot as used by this instruction.
    // This ensures that the block->bbVarUse will contain
    // the FrameRoot local var if is it a tracked variable.

    if (!compMethodRequiresPInvokeFrame())
    {
        return;
    }

    if (!call->IsUnmanaged() && !call->IsTailCallViaJitHelper())
    {
        return;
    }

    assert((!opts.ShouldUsePInvokeHelpers()) || (info.compLvFrameListRoot == BAD_VAR_NUM));

    if (opts.ShouldUsePInvokeHelpers() || call->IsSuppressGCTransition())
    {
        return;
    }

    LclVarDsc* lcl = lvaGetDesc(info.compLvFrameListRoot);

    if (lcl->lvTracked && !VarSetOps::IsMember(this, fgCurDefSet, lcl->lvVarIndex))
    {
        VarSetOps::AddElemD(this, fgCurUseSet, lcl->lvVarIndex);
    }
}

/*****************************************************************************/
void Compiler::fgPerBlockLocalVarLiveness()
{
    JITDUMP("*************** In fgPerBlockLocalVarLiveness()\n");

    unsigned livenessVarEpoch = GetCurLVEpoch();

    BasicBlock* block;

    // If we don't require accurate local var lifetimes, things are simple.
    if (!backendRequiresLocalVarLifetimes())
    {
        unsigned   lclNum;
        LclVarDsc* varDsc;

        VARSET_TP liveAll(VarSetOps::MakeEmpty(this));

        /* We simply make everything live everywhere */

        for (lclNum = 0, varDsc = lvaTable; lclNum < lvaCount; lclNum++, varDsc++)
        {
            if (varDsc->lvTracked)
            {
                VarSetOps::AddElemD(this, liveAll, varDsc->lvVarIndex);
            }
        }

        for (block = fgFirstBB; block; block = block->bbNext)
        {
            // Strictly speaking, the assignments for the "Def" cases aren't necessary here.
            // The empty set would do as well.  Use means "use-before-def", so as long as that's
            // "all", this has the right effect.
            VarSetOps::Assign(this, block->bbVarUse, liveAll);
            VarSetOps::Assign(this, block->bbVarDef, liveAll);
            VarSetOps::Assign(this, block->bbLiveIn, liveAll);
            block->bbMemoryUse     = fullMemoryKindSet;
            block->bbMemoryDef     = fullMemoryKindSet;
            block->bbMemoryLiveIn  = fullMemoryKindSet;
            block->bbMemoryLiveOut = fullMemoryKindSet;

            switch (block->bbJumpKind)
            {
                case BBJ_EHFINALLYRET:
                case BBJ_THROW:
                case BBJ_RETURN:
                    VarSetOps::AssignNoCopy(this, block->bbLiveOut, VarSetOps::MakeEmpty(this));
                    break;
                default:
                    VarSetOps::Assign(this, block->bbLiveOut, liveAll);
                    break;
            }
        }

        // In minopts, we don't explicitly build SSA or value-number; GcHeap and
        // ByrefExposed implicitly (conservatively) change state at each instr.
        byrefStatesMatchGcHeapStates = true;

        return;
    }

    // Avoid allocations in the long case.
    VarSetOps::AssignNoCopy(this, fgCurUseSet, VarSetOps::MakeEmpty(this));
    VarSetOps::AssignNoCopy(this, fgCurDefSet, VarSetOps::MakeEmpty(this));

    // GC Heap and ByrefExposed can share states unless we see a def of byref-exposed
    // memory that is not a GC Heap def.
    byrefStatesMatchGcHeapStates = true;

    for (block = fgFirstBB; block; block = block->bbNext)
    {
        VarSetOps::ClearD(this, fgCurUseSet);
        VarSetOps::ClearD(this, fgCurDefSet);

        fgCurMemoryUse   = emptyMemoryKindSet;
        fgCurMemoryDef   = emptyMemoryKindSet;
        fgCurMemoryHavoc = emptyMemoryKindSet;

        compCurBB = block;
        if (block->IsLIR())
        {
            for (GenTree* node : LIR::AsRange(block))
            {
                fgPerNodeLocalVarLivenessLIR(node);
            }
        }
        else
        {
            for (Statement* const stmt : block->NonPhiStatements())
            {
                compCurStmt = stmt;
                for (GenTree* const node : stmt->TreeList())
                {
                    fgPerNodeLocalVarLiveness(node);
                }
            }
        }

        // Mark the FrameListRoot as used, if applicable.

        if (block->bbJumpKind == BBJ_RETURN && compMethodRequiresPInvokeFrame())
        {
            assert((!opts.ShouldUsePInvokeHelpers()) || (info.compLvFrameListRoot == BAD_VAR_NUM));
            if (!opts.ShouldUsePInvokeHelpers())
            {
                noway_assert(info.compLvFrameListRoot < lvaCount);

                // 32-bit targets always pop the frame in the epilog.
                // For 64-bit targets, we only do this in the epilog for IL stubs;
                // for non-IL stubs the frame is popped after every PInvoke call.
                CLANG_FORMAT_COMMENT_ANCHOR;
#ifdef TARGET_64BIT
                if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_IL_STUB))
#endif
                {
                    LclVarDsc* varDsc = &lvaTable[info.compLvFrameListRoot];

                    if (varDsc->lvTracked)
                    {
                        if (!VarSetOps::IsMember(this, fgCurDefSet, varDsc->lvVarIndex))
                        {
                            VarSetOps::AddElemD(this, fgCurUseSet, varDsc->lvVarIndex);
                        }
                    }
                }
            }
        }

        VarSetOps::Assign(this, block->bbVarUse, fgCurUseSet);
        VarSetOps::Assign(this, block->bbVarDef, fgCurDefSet);
        block->bbMemoryUse   = fgCurMemoryUse;
        block->bbMemoryDef   = fgCurMemoryDef;
        block->bbMemoryHavoc = fgCurMemoryHavoc;

        /* also initialize the IN set, just in case we will do multiple DFAs */

        VarSetOps::AssignNoCopy(this, block->bbLiveIn, VarSetOps::MakeEmpty(this));
        block->bbMemoryLiveIn = emptyMemoryKindSet;

        DBEXEC(verbose, fgDispBBLocalLiveness(block))
    }

    noway_assert(livenessVarEpoch == GetCurLVEpoch());

    if (!compRationalIRForm)
    {
        JITDUMP("** Memory liveness computed, GcHeap states and ByrefExposed states %s\n",
                (byrefStatesMatchGcHeapStates ? "match" : "diverge"));
    }
}

// Helper functions to mark variables live over their entire scope

void Compiler::fgBeginScopeLife(VARSET_TP* inScope, VarScopeDsc* var)
{
    assert(var);

    LclVarDsc* lclVarDsc1 = &lvaTable[var->vsdVarNum];

    if (lclVarDsc1->lvTracked)
    {
        VarSetOps::AddElemD(this, *inScope, lclVarDsc1->lvVarIndex);
    }
}

void Compiler::fgEndScopeLife(VARSET_TP* inScope, VarScopeDsc* var)
{
    assert(var);

    LclVarDsc* lclVarDsc1 = &lvaTable[var->vsdVarNum];

    if (lclVarDsc1->lvTracked)
    {
        VarSetOps::RemoveElemD(this, *inScope, lclVarDsc1->lvVarIndex);
    }
}

/*****************************************************************************/

void Compiler::fgMarkInScope(BasicBlock* block, VARSET_VALARG_TP inScope)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("Scope info: block " FMT_BB " marking in scope: ", block->bbNum);
        dumpConvertedVarSet(this, inScope);
        printf("\n");
    }
#endif // DEBUG

    /* Record which vars are artifically kept alive for debugging */

    VarSetOps::Assign(this, block->bbScope, inScope);

    /* Being in scope implies a use of the variable. Add the var to bbVarUse
       so that redoing fgLiveVarAnalysis() will work correctly */

    VarSetOps::UnionD(this, block->bbVarUse, inScope);

    /* Artifically mark all vars in scope as alive */

    VarSetOps::UnionD(this, block->bbLiveIn, inScope);
    VarSetOps::UnionD(this, block->bbLiveOut, inScope);
}

void Compiler::fgUnmarkInScope(BasicBlock* block, VARSET_VALARG_TP unmarkScope)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("Scope info: block " FMT_BB " UNmarking in scope: ", block->bbNum);
        dumpConvertedVarSet(this, unmarkScope);
        printf("\n");
    }
#endif // DEBUG

    assert(VarSetOps::IsSubset(this, unmarkScope, block->bbScope));

    VarSetOps::DiffD(this, block->bbScope, unmarkScope);
    VarSetOps::DiffD(this, block->bbVarUse, unmarkScope);
    VarSetOps::DiffD(this, block->bbLiveIn, unmarkScope);
    VarSetOps::DiffD(this, block->bbLiveOut, unmarkScope);
}

#ifdef DEBUG

void Compiler::fgDispDebugScopes()
{
    printf("\nDebug scopes:\n");

    for (BasicBlock* const block : Blocks())
    {
        printf(FMT_BB ": ", block->bbNum);
        dumpConvertedVarSet(this, block->bbScope);
        printf("\n");
    }
}

#endif // DEBUG

/*****************************************************************************
 *
 * Mark variables live across their entire scope.
 */

#if defined(FEATURE_EH_FUNCLETS)

void Compiler::fgExtendDbgScopes()
{
    compResetScopeLists();

#ifdef DEBUG
    if (verbose)
    {
        printf("\nMarking vars alive over their entire scope :\n\n");
    }

    if (verbose)
    {
        compDispScopeLists();
    }
#endif // DEBUG

    VARSET_TP inScope(VarSetOps::MakeEmpty(this));

    // Mark all tracked LocalVars live over their scope - walk the blocks
    // keeping track of the current life, and assign it to the blocks.

    for (BasicBlock* const block : Blocks())
    {
        // If we get to a funclet, reset the scope lists and start again, since the block
        // offsets will be out of order compared to the previous block.

        if (block->bbFlags & BBF_FUNCLET_BEG)
        {
            compResetScopeLists();
            VarSetOps::ClearD(this, inScope);
        }

        // Process all scopes up to the current offset

        if (block->bbCodeOffs != BAD_IL_OFFSET)
        {
            compProcessScopesUntil(block->bbCodeOffs, &inScope, &Compiler::fgBeginScopeLife, &Compiler::fgEndScopeLife);
        }

        // Assign the current set of variables that are in scope to the block variables tracking this.

        fgMarkInScope(block, inScope);
    }

#ifdef DEBUG
    if (verbose)
    {
        fgDispDebugScopes();
    }
#endif // DEBUG
}

#else // !FEATURE_EH_FUNCLETS

void Compiler::fgExtendDbgScopes()
{
    compResetScopeLists();

#ifdef DEBUG
    if (verbose)
    {
        printf("\nMarking vars alive over their entire scope :\n\n");
        compDispScopeLists();
    }
#endif // DEBUG

    VARSET_TP inScope(VarSetOps::MakeEmpty(this));
    compProcessScopesUntil(0, &inScope, &Compiler::fgBeginScopeLife, &Compiler::fgEndScopeLife);

    IL_OFFSET lastEndOffs = 0;

    // Mark all tracked LocalVars live over their scope - walk the blocks
    // keeping track of the current life, and assign it to the blocks.

    for (BasicBlock* const block : Blocks())
    {
        // Find scopes becoming alive. If there is a gap in the instr
        // sequence, we need to process any scopes on those missing offsets.

        if (block->bbCodeOffs != BAD_IL_OFFSET)
        {
            if (lastEndOffs != block->bbCodeOffs)
            {
                noway_assert(lastEndOffs < block->bbCodeOffs);

                compProcessScopesUntil(block->bbCodeOffs, &inScope, &Compiler::fgBeginScopeLife,
                                       &Compiler::fgEndScopeLife);
            }
            else
            {
                while (VarScopeDsc* varScope = compGetNextEnterScope(block->bbCodeOffs))
                {
                    fgBeginScopeLife(&inScope, varScope);
                }
            }
        }

        // Assign the current set of variables that are in scope to the block variables tracking this.

        fgMarkInScope(block, inScope);

        // Find scopes going dead.

        if (block->bbCodeOffsEnd != BAD_IL_OFFSET)
        {
            VarScopeDsc* varScope;
            while ((varScope = compGetNextExitScope(block->bbCodeOffsEnd)) != nullptr)
            {
                fgEndScopeLife(&inScope, varScope);
            }

            lastEndOffs = block->bbCodeOffsEnd;
        }
    }

    /* Everything should be out of scope by the end of the method. But if the
       last BB got removed, then inScope may not be empty. */

    noway_assert(VarSetOps::IsEmpty(this, inScope) || lastEndOffs < info.compILCodeSize);
}

#endif // !FEATURE_EH_FUNCLETS

/*****************************************************************************
 *
 * For debuggable code, we allow redundant assignments to vars
 * by marking them live over their entire scope.
 */

void Compiler::fgExtendDbgLifetimes()
{
#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In fgExtendDbgLifetimes()\n");
    }
#endif // DEBUG

    noway_assert(opts.compDbgCode && (info.compVarScopesCount > 0));

    /*-------------------------------------------------------------------------
     *   Extend the lifetimes over the entire reported scope of the variable.
     */

    fgExtendDbgScopes();

/*-------------------------------------------------------------------------
 * Partly update liveness info so that we handle any funky BBF_INTERNAL
 * blocks inserted out of sequence.
 */

#ifdef DEBUG
    if (verbose && 0)
    {
        fgDispBBLiveness();
    }
#endif

    fgLiveVarAnalysis(true);

    /* For compDbgCode, we prepend an empty BB which will hold the
       initializations of variables which are in scope at IL offset 0 (but
       not initialized by the IL code). Since they will currently be
       marked as live on entry to fgFirstBB, unmark the liveness so that
       the following code will know to add the initializations. */

    assert(fgFirstBBisScratch());

    VARSET_TP trackedArgs(VarSetOps::MakeEmpty(this));

    for (unsigned argNum = 0; argNum < info.compArgsCount; argNum++)
    {
        LclVarDsc* argDsc = lvaGetDesc(argNum);

        if (argDsc->IsIndependentPromoted())
        {
            noway_assert(argDsc->GetPromotedFieldCount() == 1); // We only handle one field here
            argDsc = lvaGetDesc(argDsc->GetPromotedFieldLclNum(0));
        }

        noway_assert(argDsc->lvIsParam);

        if (argDsc->lvTracked)
        {
            noway_assert(!VarSetOps::IsMember(this, trackedArgs, argDsc->lvVarIndex)); // Each arg should define a
                                                                                       // different bit.
            VarSetOps::AddElemD(this, trackedArgs, argDsc->lvVarIndex);
        }
    }

    // Don't unmark struct locals, either.
    VARSET_TP noUnmarkVars(trackedArgs);

    for (unsigned i = 0; i < lvaCount; i++)
    {
        LclVarDsc* varDsc = &lvaTable[i];
        if (varTypeIsStruct(varDsc) && varDsc->lvTracked)
        {
            VarSetOps::AddElemD(this, noUnmarkVars, varDsc->lvVarIndex);
        }
    }
    fgUnmarkInScope(fgFirstBB, VarSetOps::Diff(this, fgFirstBB->bbScope, noUnmarkVars));

    /*-------------------------------------------------------------------------
     * As we keep variables artifically alive over their entire scope,
     * we need to also artificially initialize them if the scope does
     * not exactly match the real lifetimes, or they will contain
     * garbage until they are initialized by the IL code.
     */

    VARSET_TP initVars(VarSetOps::MakeEmpty(this)); // Vars which are artificially made alive

    for (BasicBlock* const block : Blocks())
    {
        VarSetOps::ClearD(this, initVars);

        switch (block->bbJumpKind)
        {
            case BBJ_NONE:
                PREFIX_ASSUME(block->bbNext != nullptr);
                VarSetOps::UnionD(this, initVars, block->bbNext->bbScope);
                break;

            case BBJ_ALWAYS:
            case BBJ_EHCATCHRET:
            case BBJ_EHFILTERRET:
                VarSetOps::UnionD(this, initVars, block->bbJumpDest->bbScope);
                break;

            case BBJ_CALLFINALLY:
                if (!(block->bbFlags & BBF_RETLESS_CALL))
                {
                    assert(block->isBBCallAlwaysPair());
                    PREFIX_ASSUME(block->bbNext != nullptr);
                    VarSetOps::UnionD(this, initVars, block->bbNext->bbScope);
                }
                VarSetOps::UnionD(this, initVars, block->bbJumpDest->bbScope);
                break;

            case BBJ_COND:
                PREFIX_ASSUME(block->bbNext != nullptr);
                VarSetOps::UnionD(this, initVars, block->bbNext->bbScope);
                VarSetOps::UnionD(this, initVars, block->bbJumpDest->bbScope);
                break;

            case BBJ_SWITCH:
                for (BasicBlock* const bTarget : block->SwitchTargets())
                {
                    VarSetOps::UnionD(this, initVars, bTarget->bbScope);
                }
                break;

            case BBJ_EHFINALLYRET:
            case BBJ_RETURN:
                break;

            case BBJ_THROW:
                /* We don't have to do anything as we mark
                 * all vars live on entry to a catch handler as
                 * volatile anyway
                 */
                break;

            default:
                noway_assert(!"Unexpected bbJumpKind");
                break;
        }

        /* If the var is already live on entry to the current BB,
           we would have already initialized it. So ignore bbLiveIn */

        VarSetOps::DiffD(this, initVars, block->bbLiveIn);

        /* Add statements initializing the vars, if there are any to initialize */

        VarSetOps::Iter iter(this, initVars);
        unsigned        varIndex = 0;
        while (iter.NextElem(&varIndex))
        {
            /* Create initialization tree */

            unsigned   varNum = lvaTrackedIndexToLclNum(varIndex);
            LclVarDsc* varDsc = lvaGetDesc(varNum);
            var_types  type   = varDsc->TypeGet();

            // Don't extend struct lifetimes -- they aren't enregistered, anyway.
            if (type == TYP_STRUCT)
            {
                continue;
            }

            // If we haven't already done this ...
            if (!fgLocalVarLivenessDone)
            {
                // Create a "zero" node
                GenTree* zero = gtNewZeroConNode(genActualType(type));

                // Create initialization node
                if (!block->IsLIR())
                {
                    GenTree* varNode  = gtNewLclvNode(varNum, type);
                    GenTree* initNode = gtNewAssignNode(varNode, zero);

                    // Create a statement for the initializer, sequence it, and append it to the current BB.
                    Statement* initStmt = gtNewStmt(initNode);
                    gtSetStmtInfo(initStmt);
                    fgSetStmtSeq(initStmt);
                    fgInsertStmtNearEnd(block, initStmt);
                }
                else
                {
                    GenTree* store = gtNewStoreLclVar(varNum, type, zero);

                    LIR::Range initRange = LIR::EmptyRange();
                    initRange.InsertBefore(nullptr, zero, store);

#if !defined(TARGET_64BIT)
                    DecomposeLongs::DecomposeRange(this, initRange);
#endif // !defined(TARGET_64BIT)
                    m_pLowering->LowerRange(block, initRange);

                    // Naively inserting the initializer at the end of the block may add code after the block's
                    // terminator, in which case the inserted code will never be executed (and the IR for the
                    // block will be invalid). Use `LIR::InsertBeforeTerminator` to avoid this problem.
                    LIR::InsertBeforeTerminator(block, std::move(initRange));
                }

#ifdef DEBUG
                if (verbose)
                {
                    printf("Created zero-init of V%02u in " FMT_BB "\n", varNum, block->bbNum);
                }
#endif                                         // DEBUG
                block->bbFlags |= BBF_CHANGED; // indicates that the contents of the block have changed.
            }

            /* Update liveness information so that redoing fgLiveVarAnalysis()
               will work correctly if needed */

            VarSetOps::AddElemD(this, block->bbVarDef, varIndex);
            VarSetOps::AddElemD(this, block->bbLiveOut, varIndex);
        }
    }

    // raMarkStkVars() reserves stack space for unused variables (which
    //   needs to be initialized). However, arguments don't need to be initialized.
    //   So just ensure that they don't have a 0 ref cnt

    unsigned lclNum = 0;
    for (LclVarDsc *varDsc = lvaTable; lclNum < lvaCount; lclNum++, varDsc++)
    {
        if (lclNum >= info.compArgsCount)
        {
            break; // early exit for loop
        }

        if (varDsc->lvIsRegArg)
        {
            varDsc->lvImplicitlyReferenced = true;
        }
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("\nBB liveness after fgExtendDbgLifetimes():\n\n");
        fgDispBBLiveness();
        printf("\n");
    }
#endif // DEBUG
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

    unsigned  m_memoryLiveIn;
    unsigned  m_memoryLiveOut;
    VARSET_TP m_liveIn;
    VARSET_TP m_liveOut;

    LiveVarAnalysis(Compiler* compiler)
        : m_compiler(compiler)
        , m_hasPossibleBackEdge(false)
        , m_memoryLiveIn(emptyMemoryKindSet)
        , m_memoryLiveOut(emptyMemoryKindSet)
        , m_liveIn(VarSetOps::MakeEmpty(compiler))
        , m_liveOut(VarSetOps::MakeEmpty(compiler))
    {
    }

    bool PerBlockAnalysis(BasicBlock* block, bool updateInternalOnly, bool keepAliveThis)
    {
        /* Compute the 'liveOut' set */
        VarSetOps::ClearD(m_compiler, m_liveOut);
        m_memoryLiveOut = emptyMemoryKindSet;
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
        m_memoryLiveIn = m_memoryLiveOut | block->bbMemoryUse;

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
            if (updateInternalOnly)
            {
                // Only "extend" liveness over BBF_INTERNAL blocks

                noway_assert(block->bbFlags & BBF_INTERNAL);

                liveInChanged = !VarSetOps::IsSubset(m_compiler, m_liveIn, block->bbLiveIn);
                if (liveInChanged || !VarSetOps::IsSubset(m_compiler, m_liveOut, block->bbLiveOut))
                {
#ifdef DEBUG
                    if (m_compiler->verbose)
                    {
                        printf("Scope info: block " FMT_BB " LiveIn+ ", block->bbNum);
                        dumpConvertedVarSet(m_compiler, VarSetOps::Diff(m_compiler, m_liveIn, block->bbLiveIn));
                        printf(", LiveOut+ ");
                        dumpConvertedVarSet(m_compiler, VarSetOps::Diff(m_compiler, m_liveOut, block->bbLiveOut));
                        printf("\n");
                    }
#endif // DEBUG

                    VarSetOps::UnionD(m_compiler, block->bbLiveIn, m_liveIn);
                    VarSetOps::UnionD(m_compiler, block->bbLiveOut, m_liveOut);
                }
            }
            else
            {
                VarSetOps::Assign(m_compiler, block->bbLiveIn, m_liveIn);
                VarSetOps::Assign(m_compiler, block->bbLiveOut, m_liveOut);
            }
        }

        const bool memoryLiveInChanged = (block->bbMemoryLiveIn != m_memoryLiveIn);
        if (memoryLiveInChanged || (block->bbMemoryLiveOut != m_memoryLiveOut))
        {
            block->bbMemoryLiveIn  = m_memoryLiveIn;
            block->bbMemoryLiveOut = m_memoryLiveOut;
        }

        return liveInChanged || memoryLiveInChanged;
    }

    void Run(bool updateInternalOnly)
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

            m_memoryLiveIn  = emptyMemoryKindSet;
            m_memoryLiveOut = emptyMemoryKindSet;

            for (BasicBlock* block = m_compiler->fgLastBB; block; block = block->bbPrev)
            {
                // sometimes block numbers are not monotonically increasing which
                // would cause us not to identify backedges
                if (block->bbNext && block->bbNext->bbNum <= block->bbNum)
                {
                    m_hasPossibleBackEdge = true;
                }

                if (updateInternalOnly)
                {
                    /* Only update BBF_INTERNAL blocks as they may be
                       syntactically out of sequence. */

                    noway_assert(m_compiler->opts.compDbgCode && (m_compiler->info.compVarScopesCount > 0));

                    if (!(block->bbFlags & BBF_INTERNAL))
                    {
                        continue;
                    }
                }

                if (PerBlockAnalysis(block, updateInternalOnly, keepAliveThis))
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

public:
    static void Run(Compiler* compiler, bool updateInternalOnly)
    {
        LiveVarAnalysis analysis(compiler);
        analysis.Run(updateInternalOnly);
    }
};

/*****************************************************************************
 *
 *  This is the classic algorithm for Live Variable Analysis.
 *  If updateInternalOnly==true, only update BBF_INTERNAL blocks.
 */

void Compiler::fgLiveVarAnalysis(bool updateInternalOnly)
{
    if (!backendRequiresLocalVarLifetimes())
    {
        return;
    }

    LiveVarAnalysis::Run(this, updateInternalOnly);

#ifdef DEBUG
    if (verbose && !updateInternalOnly)
    {
        printf("\nBB liveness after fgLiveVarAnalysis():\n\n");
        fgDispBBLiveness();
    }
#endif // DEBUG
}

//------------------------------------------------------------------------
// Compiler::fgComputeLifeCall: compute the changes to local var liveness
//                              due to a GT_CALL node.
//
// Arguments:
//    life - The live set that is being computed.
//    call - The call node in question.
//
void Compiler::fgComputeLifeCall(VARSET_TP& life, GenTreeCall* call)
{
    assert(call != nullptr);

    // If this is a tail-call via helper, and we have any unmanaged p/invoke calls in
    // the method, then we're going to run the p/invoke epilog
    // So we mark the FrameRoot as used by this instruction.
    // This ensure that this variable is kept alive at the tail-call
    if (call->IsTailCallViaJitHelper() && compMethodRequiresPInvokeFrame())
    {
        assert((!opts.ShouldUsePInvokeHelpers()) || (info.compLvFrameListRoot == BAD_VAR_NUM));
        if (!opts.ShouldUsePInvokeHelpers())
        {
            // Get the FrameListRoot local and make it live.

            noway_assert(info.compLvFrameListRoot < lvaCount);

            LclVarDsc* frameVarDsc = &lvaTable[info.compLvFrameListRoot];

            if (frameVarDsc->lvTracked)
            {
                VarSetOps::AddElemD(this, life, frameVarDsc->lvVarIndex);
            }
        }
    }

    // TODO: we should generate the code for saving to/restoring
    //       from the inlined N/Direct frame instead.

    /* Is this call to unmanaged code? */
    if (call->IsUnmanaged() && compMethodRequiresPInvokeFrame())
    {
        // Get the FrameListRoot local and make it live.
        assert((!opts.ShouldUsePInvokeHelpers()) || (info.compLvFrameListRoot == BAD_VAR_NUM));
        if (!opts.ShouldUsePInvokeHelpers() && !call->IsSuppressGCTransition())
        {
            noway_assert(info.compLvFrameListRoot < lvaCount);

            LclVarDsc* frameVarDsc = &lvaTable[info.compLvFrameListRoot];

            if (frameVarDsc->lvTracked)
            {
                unsigned varIndex = frameVarDsc->lvVarIndex;
                noway_assert(varIndex < lvaTrackedCount);

                // Is the variable already known to be alive?
                //
                if (VarSetOps::IsMember(this, life, varIndex))
                {
                    // Since we may call this multiple times, clear the GTF_CALL_M_FRAME_VAR_DEATH if set.
                    //
                    call->gtCallMoreFlags &= ~GTF_CALL_M_FRAME_VAR_DEATH;
                }
                else
                {
                    // The variable is just coming to life
                    // Since this is a backwards walk of the trees
                    // that makes this change in liveness a 'last-use'
                    //
                    VarSetOps::AddElemD(this, life, varIndex);
                    call->gtCallMoreFlags |= GTF_CALL_M_FRAME_VAR_DEATH;
                }
            }
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

    VARSET_TP fieldSet(VarSetOps::MakeEmpty(this));
    bool      allFieldAreTracked = true;

    for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
    {
        LclVarDsc* fieldLcl = lvaGetDesc(lcl->GetPromotedFieldLclNum(i));

        if (!fieldLcl->lvTracked)
        {
            allFieldAreTracked = false;
            continue;
        }

        const unsigned index = fieldLcl->GetLivenessBitIndex();

        VarSetOps::AddElemD(this, fieldSet, index);

        if (isDef && node->IsMultiRegLclVar() && !VarSetOps::IsMember(this, liveOut, index))
        {
            node->AsLclVar()->SetLastUse(i);
        }
    }

    if (isDef)
    {
        VARSET_TP liveFields(VarSetOps::Intersection(this, liveOut, fieldSet));

        if ((node->gtFlags & GTF_VAR_USEASG) == 0)
        {
            VarSetOps::DiffD(this, fieldSet, keepAlive);
            VarSetOps::DiffD(this, liveOut, fieldSet);
        }

        if (allFieldAreTracked && VarSetOps::IsEmpty(this, liveFields) && !opts.MinOpts())
        {
            return !(lcl->lvCustomLayout && lcl->lvContainsHoles);
        }

        return false;
    }
    else
    {
        if (VarSetOps::IsSubset(this, fieldSet, liveOut))
        {
            node->gtFlags &= ~GTF_VAR_DEATH;
            return false;
        }

        // Some fields are being used, and they are not currently live.
        node->gtFlags |= GTF_VAR_DEATH;

        if (!VarSetOps::IsEmptyIntersection(this, fieldSet, liveOut))
        {
            // Only a subset of the variables are becoming alive; we must record that subset.
            // (Lack of an entry for "node" will be considered to imply all become dead in the
            // forward traversal.)

            VARSET_TP* deadVarSet = new (this, CMK_bitset) VARSET_TP;
            VarSetOps::AssignNoCopy(this, *deadVarSet, VarSetOps::Diff(this, fieldSet, liveOut));
            GetPromotedStructDeathVars()->Set(node, deadVarSet, NodeToVarsetPtrMap::Overwrite);
        }

        VarSetOps::UnionD(this, liveOut, fieldSet);

        return false;
    }
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

                    // Removing a call does not affect liveness unless it is a tail call in a method with P/Invokes or
                    // is itself a P/Invoke, in which case it may affect the liveness of the frame root variable.
                    if (!opts.MinOpts() && !opts.ShouldUsePInvokeHelpers() &&
                        ((call->IsTailCall() && compMethodRequiresPInvokeFrame()) ||
                         (call->IsUnmanaged() && !call->IsSuppressGCTransition())) &&
                        lvaTable[info.compLvFrameListRoot].lvTracked)
                    {
                        fgStmtRemoved = true;
                    }
                }
                else
                {
                    fgComputeLifeCall(life, call);
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

/*****************************************************************************
 *
 *  Iterative data flow for live variable info and availability of range
 *  check index expressions.
 */
void Compiler::fgInterBlockLocalVarLiveness()
{
#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In fgInterBlockLocalVarLiveness()\n");
    }
#endif

    /* This global flag is set whenever we remove a statement */

    fgStmtRemoved = false;

    // keep track if a bbLiveIn changed due to dead store removal
    fgLocalVarLivenessChanged = false;

    /* Compute the IN and OUT sets for tracked variables */

    fgLiveVarAnalysis();

    /* For debuggable code, we mark vars as live over their entire
     * reported scope, so that it will be visible over the entire scope
     */

    if (opts.compDbgCode && (info.compVarScopesCount > 0))
    {
        fgExtendDbgLifetimes();
    }

    // Nothing more to be done if the backend does not require accurate local var lifetimes.
    if (!backendRequiresLocalVarLifetimes())
    {
        fgLocalVarLivenessDone = true;
        return;
    }

    //-------------------------------------------------------------------------
    // Variables involved in exception-handlers and finally blocks need
    // to be specially marked
    //

    VARSET_TP exceptVars(VarSetOps::MakeEmpty(this));  // vars live on entry to a handler
    VARSET_TP finallyVars(VarSetOps::MakeEmpty(this)); // vars live on exit of a 'finally' block

    for (BasicBlock* const block : Blocks())
    {
        if (block->hasEHBoundaryIn())
        {
            // Note the set of variables live on entry to exception handler.
            VarSetOps::UnionD(this, exceptVars, block->bbLiveIn);
        }

        if (block->hasEHBoundaryOut())
        {
            // Get the set of live variables on exit from an exception region.
            VarSetOps::UnionD(this, exceptVars, block->bbLiveOut);
            if (block->bbJumpKind == BBJ_EHFINALLYRET)
            {
                // Live on exit from finally.
                // We track these separately because, in addition to having EH live-out semantics,
                // they are must-init.
                VarSetOps::UnionD(this, finallyVars, block->bbLiveOut);
            }
        }
    }

    LclVarDsc* varDsc;
    unsigned   varNum;

    for (varNum = 0, varDsc = lvaTable; varNum < lvaCount; varNum++, varDsc++)
    {
        // Ignore the variable if it's not tracked

        if (!varDsc->lvTracked)
        {
            continue;
        }

        // Un-init locals may need auto-initialization. Note that the
        // liveness of such locals will bubble to the top (fgFirstBB)
        // in fgInterBlockLocalVarLiveness()

        // Fields of dependently promoted structs may be tracked. We shouldn't set lvMustInit on them since
        // the whole parent struct will be initialized; however, lvLiveInOutOfHndlr should be set on them
        // as appropriate.

        if (!varDsc->IsParam() && VarSetOps::IsMember(this, fgFirstBB->bbLiveIn, varDsc->lvVarIndex) &&
            (info.compInitMem || varTypeIsGC(varDsc->GetType())) && !varDsc->IsDependentPromotedField(this))
        {
            varDsc->lvMustInit = true;
        }

        // Mark all variables that are live on entry to an exception handler
        // or on exit from a filter handler or finally.

        bool isFinallyVar = VarSetOps::IsMember(this, finallyVars, varDsc->lvVarIndex);
        if (isFinallyVar || VarSetOps::IsMember(this, exceptVars, varDsc->lvVarIndex))
        {
            // Mark the variable appropriately.
            lvaSetVarLiveInOutOfHandler(varNum);

            // Mark all pointer variables live on exit from a 'finally' block as
            // 'explicitly initialized' (must-init) for GC-ref types.

            if (isFinallyVar)
            {
                // Set lvMustInit only if we have a non-arg, GC pointer.
                if (!varDsc->lvIsParam && varTypeIsGC(varDsc->TypeGet()))
                {
                    varDsc->lvMustInit = true;
                }
            }
        }
    }

    /*-------------------------------------------------------------------------
     * Now fill in liveness info within each basic block - Backward DataFlow
     */

    for (BasicBlock* const block : Blocks())
    {
        /* Tell everyone what block we're working on */

        compCurBB = block;

        /* Remember those vars live on entry to exception handlers */
        /* if we are part of a try block */

        VARSET_TP volatileVars(VarSetOps::MakeEmpty(this));

        if (ehBlockHasExnFlowDsc(block))
        {
            VarSetOps::Assign(this, volatileVars, fgGetHandlerLiveVars(block));

            // volatileVars is a subset of exceptVars
            noway_assert(VarSetOps::IsSubset(this, volatileVars, exceptVars));
        }

        /* Start with the variables live on exit from the block */

        VARSET_TP life(VarSetOps::MakeCopy(this, block->bbLiveOut));
        VARSET_TP keepAlive(VarSetOps::Union(this, volatileVars, compCurBB->bbScope));

        /* Mark any interference we might have at the end of the block */

        if (block->IsLIR())
        {
            fgComputeLifeLIR(life, keepAlive, block);
        }
        else
        {
            /* Get the first statement in the block */

            Statement* firstStmt = block->FirstNonPhiDef();

            if (firstStmt == nullptr)
            {
                continue;
            }

            /* Walk all the statements of the block backwards - Get the LAST stmt */

            Statement* nextStmt = block->lastStmt();

            do
            {
                noway_assert(nextStmt != nullptr);

                compCurStmt = nextStmt;
                nextStmt    = nextStmt->GetPrevStmt();

                fgComputeLifeStmt(life, keepAlive, compCurStmt);
            } while (compCurStmt != firstStmt);
        }

        /* Done with the current block - if we removed any statements, some
         * variables may have become dead at the beginning of the block
         * -> have to update bbLiveIn */

        if (!VarSetOps::Equal(this, life, block->bbLiveIn))
        {
            /* some variables have become dead all across the block
               So life should be a subset of block->bbLiveIn */

            // We changed the liveIn of the block, which may affect liveOut of others,
            // which may expose more dead stores.
            fgLocalVarLivenessChanged = true;

            noway_assert(VarSetOps::IsSubset(this, life, block->bbLiveIn));

            /* set the new bbLiveIn */

            VarSetOps::Assign(this, block->bbLiveIn, life);

            /* compute the new bbLiveOut for all the predecessors of this block */
        }

        noway_assert(compCurBB == block);
#ifdef DEBUG
        compCurBB = nullptr;
#endif
    }

    fgLocalVarLivenessDone = true;
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
        for (MemoryKind memoryKind : allMemoryKinds())
        {
            if ((block->bbMemoryUse & memoryKindSet(memoryKind)) != 0)
            {
                printf(" + %s", memoryKindNames[memoryKind]);
            }
        }
    }

    printf("\n     DEF(%d)=", VarSetOps::Count(this, block->bbVarDef));
    lvaDispVarSet(block->bbVarDef, allVars);

    if (!block->IsLIR())
    {
        for (MemoryKind memoryKind : allMemoryKinds())
        {
            if ((block->bbMemoryDef & memoryKindSet(memoryKind)) != 0)
            {
                printf(" + %s", memoryKindNames[memoryKind]);
            }
            if ((block->bbMemoryHavoc & memoryKindSet(memoryKind)) != 0)
            {
                printf("*");
            }
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
        for (MemoryKind memoryKind : allMemoryKinds())
        {
            if ((block->bbMemoryLiveIn & memoryKindSet(memoryKind)) != 0)
            {
                printf(" + %s", memoryKindNames[memoryKind]);
            }
        }
    }

    printf("\n     OUT(%d)=", VarSetOps::Count(this, block->bbLiveOut));
    lvaDispVarSet(block->bbLiveOut, allVars);

    if (!block->IsLIR())
    {
        for (MemoryKind memoryKind : allMemoryKinds())
        {
            if ((block->bbMemoryLiveOut & memoryKindSet(memoryKind)) != 0)
            {
                printf(" + %s", memoryKindNames[memoryKind]);
            }
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
