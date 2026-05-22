// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "lower.h"
#include "jitgcinfo.h"
#ifndef TARGET_64BIT
#include "decomposelongs.h"
#endif

void Lowering::Run()
{
#ifdef PROFILING_SUPPORTED
#ifdef UNIX_AMD64_ABI
    if (comp->opts.IsProfilerHookNeeded())
    {
        comp->codeGen->needToAlignFrame = true;
    }
#endif
#endif

    // TODO-MIKE-Cleanup: See if this can be done during the existing lowering traversal.
    // It looks like we may end up inserting block in front of previously lowered blocks
    // and miss lowering these new blocks. But then these blocks are trivial and don't
    // really need any lowering (they contain only calls to helpers with no args).
    for (BasicBlock* block : comp->Blocks())
    {
        unsigned throwIndex = comp->bbThrowIndex(block);

        for (GenTree* node : LIR::AsRange(block))
        {
            if (GenTreeBoundsChk* boundsChk = node->IsBoundsChk())
            {
                boundsChk->SetThrowBlock(comp->fgGetThrowHelperBlock(boundsChk->GetThrowKind(), block, throwIndex));
            }
        }
    }

    // If we have any PInvoke calls, insert the one-time prolog code. We'll inserted the epilog code in the
    // appropriate spots later. NOTE: there is a minor optimization opportunity here, as we still create p/invoke
    // data structures and setup/teardown even if we've eliminated all p/invoke calls due to dead code elimination.
    if (comp->info.IsPInvokeFrameRequired())
    {
        InsertPInvokeMethodProlog();
    }

#ifndef TARGET_64BIT
    DecomposeLongs decomp(comp);
    if (comp->compLongUsed)
    {
        decomp.PromoteLongVars();
    }
#endif

    for (BasicBlock* const block : comp->Blocks())
    {
#ifndef TARGET_64BIT
        if (comp->compLongUsed)
        {
            decomp.DecomposeBlock(block);
        }
#endif

        LowerBlock(block);
    }

    if (comp->fgHasEH() || comp->info.IsPInvokeFrameRequired() || comp->opts.IsProfilerHookNeeded() ||
        comp->compLocallocUsed
#ifdef TARGET_X86
        || comp->compTailCallUsed
#endif
#ifdef JIT32_GCENCODER
        || comp->info.compPublishStubParam || comp->info.compIsVarArgs || comp->lvaReportParamTypeArg()
#endif
        || comp->opts.compDbgEnC)
    {
        comp->opts.SetFramePointerRequired();
    }

#if FEATURE_FIXED_OUT_ARGS
    // Finish computing the outgoing args area size
    //
    // Need to make sure the MIN_ARG_AREA_FOR_CALL space is added to the frame if:
    // 1. there are calls to THROW_HEPLPER methods.
    // 2. we are generating profiling Enter/Leave/TailCall hooks. This will ensure
    //    that even methods without any calls will have outgoing arg area space allocated.
    //
    // An example for these two cases is Windows Amd64, where the ABI requires to have 4 slots for
    // the outgoing arg space if the method makes any calls.
    if (outgoingArgAreaSize < MIN_ARG_AREA_FOR_CALL)
    {
        if (comp->compUsesThrowHelper || comp->opts.IsProfilerHookNeeded())
        {
            outgoingArgAreaSize = MIN_ARG_AREA_FOR_CALL;
            JITDUMP("Increasing outgoingArgAreaSize to %u for throw helper or profile hook", outgoingArgAreaSize);
        }
    }

    // If a function has localloc, we will need to move the outgoing arg space when the
    // localloc happens. When we do this, we need to maintain stack alignment. To avoid
    // leaving alignment-related holes when doing this move, make sure the outgoing
    // argument space size is a multiple of the stack alignment by aligning up to the next
    // stack alignment boundary.
    if (comp->compLocallocUsed)
    {
        outgoingArgAreaSize = roundUp(outgoingArgAreaSize, STACK_ALIGN);
        JITDUMP("Increasing outgoingArgAreaSize to %u for localloc", outgoingArgAreaSize);
    }

    assert(outgoingArgAreaSize % REGSIZE_BYTES == 0);

    comp->codeGen->outgoingArgSpaceSize.SetFinalValue(outgoingArgAreaSize);
    comp->lvaGetDesc(comp->lvaOutgoingArgSpaceVar)->SetBlockType(outgoingArgAreaSize);
#endif // FEATURE_FIXED_OUT_ARGS

#ifdef DEBUG
    JITDUMP("Lower has completed modifying nodes.\n");
    if (comp->verbose)
    {
        comp->fgDispBasicBlocks(true);
    }
#endif

    if (comp->opts.OptimizationDisabled())
    {
        INDEBUG(VerifyAllLocalsImplicitlyReferenced());
    }
    else
    {
        assert(comp->opts.EnregLocals());

        DBEXEC(comp->verbose, comp->lvaTableDump());

        comp->lvaComputeLclRefCounts();
        comp->lvaMarkLivenessTrackedLocals();
        comp->fgLocalVarLiveness();
        comp->EndPhase(PHASE_LIR_LIVENESS);

        // Liveness can delete code, which may create empty blocks.
        comp->optLoopsMarked = false;

        if (comp->fgUpdateFlowGraph(this))
        {
            JITDUMP("Flowgraph was modified, running liveness again\n");
            comp->fgLocalVarLiveness();
            comp->EndPhase(PHASE_LIR_LIVENESS);
        }

        // Recompute local var ref counts again after liveness to reflect
        // impact of any dead code removal. Note this may leave us with
        // tracked vars that have zero refs.
        comp->lvaComputeLclRefCounts();
    }

    DBEXEC(comp->verbose, comp->lvaTableDump());
}

void Lowering::LowerBlock(BasicBlock* block)
{
    assert(block->isEmpty() || block->IsLIR());

    m_block = block;

    GenTree* node = BlockRange().FirstNode();

    while (node != nullptr)
    {
        node = LowerNode(node);
    }

    assert(VerifyBlock(block));
}

GenTree* Lowering::LowerNode(GenTree* node)
{
    assert(node != nullptr);

    switch (node->GetOper())
    {
        case GT_LCL_ADDR:
            assert(node->AsLclAddr()->GetLcl()->IsAddressExposed());
            break;
        case GT_LCL_LOAD:
            LowerLclLoad(node->AsLclLoad());
            break;
        case GT_LCL_STORE:
            LowerLclStore(node->AsLclStore());
            break;
        case GT_LCL_LOAD_FLD:
            LowerLclLoadFld(node->AsLclLoadFld());
            break;
        case GT_LCL_STORE_FLD:
            LowerLclStoreFld(node->AsLclStoreFld());
            break;
        case GT_NULLCHECK:
        case GT_IND_LOAD:
            LowerIndir(node->AsIndir());
            break;
        case GT_IND_STORE:
            LowerIndStore(node->AsIndStore());
            break;
        case GT_IND_STORE_OBJ:
            LowerIndStoreObj(node->AsIndStoreObj());
            break;
        case GT_IND_STORE_BLK:
            LowerIndStoreBlk(node->AsIndStoreBlk());
            break;

#ifdef TARGET_ARM
        case GT_FMUL:
            LowerFloatMul(node->AsOp());
            break;
#endif

#ifdef TARGET_ARM64
        case GT_FXT:
            LowerFloatExtend(node->AsUnOp());
            break;
        case GT_FTRUNC:
            LowerFloatTruncate(node->AsUnOp());
            break;
        case GT_FNEG:
            LowerFloatNegate(node->AsUnOp());
            break;

        case GT_FADD:
        case GT_FSUB:
        case GT_FMUL:
        case GT_FDIV:
            LowerFloatArithmetic(node->AsOp());
            break;

        case GT_INTRINSIC:
            LowerIntrinsic(node->AsIntrinsic());
            break;

        case GT_NOT:
            LowerNot(node->AsUnOp());
            break;

        case GT_AND:
        case GT_OR:
        case GT_XOR:
            LowerLogical(node->AsOp());
            break;

        case GT_NEG:
            LowerNegate(node->AsUnOp());
            break;

        case GT_ADD:
        case GT_SUB:
            LowerArithmetic(node->AsOp());
            break;

        case GT_OVF_SADD:
        case GT_OVF_UADD:
        case GT_OVF_SSUB:
        case GT_OVF_USUB:
            ContainCheckBinary(node->AsOp());
            break;

        case GT_MUL:
        case GT_SMULH:
        case GT_UMULH:
            LowerMultiply(node->AsOp());
            break;

        case GT_UDIV:
            LowerUnsignedDiv(node->AsOp());
            break;

        case GT_SDIV:
            return LowerSignedDiv(node->AsOp());

        case GT_UREM:
        case GT_SREM:
            unreached();

        case GT_LT:
        case GT_LE:
        case GT_GT:
        case GT_GE:
        case GT_EQ:
        case GT_NE:
        case GT_TEST_EQ:
        case GT_TEST_NE:
        case GT_CMP:
            return LowerRelop(node->AsOp());

        case GT_BOUNDS_CHECK:
            ContainCheckBoundsChk(node->AsBoundsChk());
            break;

        case GT_XORR:
        case GT_XAND:
        case GT_XADD:
            ContainImmOperand(node, node->AsOp()->GetOp(1));
            break;

        case GT_CMPXCHG:
            ContainImmOperand(node, node->AsCmpXchg()->GetCompareValue());
            break;
#else // TARGET_ARM64

#ifdef TARGET_XARCH
        case GT_MUL:
        case GT_SMULH:
        case GT_UMULH:
        case GT_OVF_SMUL:
        case GT_OVF_UMUL:
#ifdef TARGET_X86
        case GT_SMULL:
        case GT_UMULL:
#endif
            ContainCheckMul(node->AsOp());
            break;

        case GT_FADD:
        case GT_FSUB:
        case GT_FMUL:
        case GT_FDIV:
            ContainCheckFloatBinary(node->AsOp());
            break;

        case GT_FXT:
        case GT_FTRUNC:
            return LowerFloatConvert(node->AsUnOp());

        case GT_INTRINSIC:
            ContainCheckIntrinsic(node->AsIntrinsic());
            break;

        case GT_BOUNDS_CHECK:
            ContainCheckBoundsChk(node->AsBoundsChk());
            break;

        case GT_XADD:
            ContainCheckXAdd(node->AsOp());
            break;
#endif // TARGET_XARCH

        case GT_ADD:
        case GT_OVF_SADD:
        case GT_OVF_UADD:
            if (GenTree* next = LowerAdd(node->AsOp()))
            {
                return next;
            }
            break;

        case GT_OVF_SSUB:
        case GT_OVF_USUB:
#ifdef TARGET_ARM
            node->AddImplicitFlagsDef();
#endif
            FALLTHROUGH;
#ifndef TARGET_64BIT
        case GT_ADD_LO:
        case GT_ADD_HI:
        case GT_SUB_LO:
        case GT_SUB_HI:
        case GT_OVF_SADDC:
        case GT_OVF_UADDC:
        case GT_OVF_SSUBB:
        case GT_OVF_USUBB:
#endif
        case GT_AND:
        case GT_SUB:
        case GT_OR:
        case GT_XOR:
            ContainCheckBinary(node->AsOp());
            break;

        case GT_LT:
        case GT_LE:
        case GT_GT:
        case GT_GE:
        case GT_EQ:
        case GT_NE:
        case GT_TEST_EQ:
        case GT_TEST_NE:
        case GT_CMP:
            return LowerCompare(node->AsOp());

#ifndef USE_HELPERS_FOR_INT_DIV
        case GT_UDIV:
        case GT_UREM:
            if (!LowerUnsignedDivRem(node->AsOp()))
            {
                ContainCheckDivRem(node->AsOp());
            }
            break;
#endif

        case GT_SDIV:
        case GT_SREM:
            return LowerSignedDivRem(node);
#endif // !TARGET_ARM64

        case GT_ROL:
#ifdef TARGET_XARCH
            LowerRotateLeft(node->AsOp());
            break;
#else
            unreached();
#endif

        case GT_ROR:
            LowerRotateRight(node->AsOp());
            break;

        case GT_SWITCH:
            return LowerSwitch(node->AsUnOp());

        case GT_CALL:
            LowerCall(node->AsCall());
            break;

        case GT_JTRUE:
            return LowerJTrue(node->AsUnOp());

        case GT_JMP:
            LowerJmp(node->AsJmp());
            break;

        case GT_RETURN:
            LowerReturn(node->AsUnOp());
            break;

        case GT_RETURNTRAP:
            ContainCheckReturnTrap(node->AsOp());
            break;

        case GT_BITCAST:
            return LowerBitCast(node->AsUnOp());

        case GT_OVF_SCONV:
        case GT_OVF_UCONV:
            LowerOvfConv(node->AsUnOp());
            break;

        case GT_OVF_U:
            LowerOvfUnsigned(node->AsUnOp());
            break;

        case GT_OVF_TRUNC:
        case GT_OVF_STRUNC:
        case GT_OVF_UTRUNC:
            LowerOvfTruncate(node->AsUnOp());
            break;

        case GT_CONV:
            return LowerConv(node->AsUnOp());

#ifdef TARGET_64BIT
        case GT_TRUNC:
            return LowerTruncate(node->AsUnOp());
        case GT_SXT:
            LowerSignedExtend(node->AsUnOp());
            break;
        case GT_UXT:
            LowerUnsignedExtend(node->AsUnOp());
            break;
#endif

        case GT_STOF:
        case GT_UTOF:
            LowerIntToFloat(node->AsUnOp());
            break;

        case GT_FTOS:
        case GT_FTOU:
            LowerFloatToInt(node->AsUnOp());
            break;

        case GT_ARR_ELEM:
            return LowerArrElem(node->AsArrElem());

#ifndef TARGET_64BIT
        case GT_LSH_HI:
        case GT_RSH_LO:
            ContainCheckShiftRotate(node->AsOp());
            break;
#endif

        case GT_LSH:
        case GT_RSH:
        case GT_RSZ:
            LowerShift(node->AsOp());
            break;

        case GT_LCLHEAP:
            LowerLclHeap(node->AsUnOp());
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            LowerHWIntrinsic(node->AsHWIntrinsic());
            break;
#endif

        case GT_KEEPALIVE:
            return LowerKeepAlive(node->AsUnOp());

        default:
            break;
    }

    return node->gtNext;
}

GenTree* Lowering::LowerKeepAlive(GenTreeUnOp* node)
{
    GenTree* value = node->GetOp(0);

    if (GenTreeLclLoad* load = value->IsLclLoad())
    {
        // Address exposed locals are always live so the KEEPALIVE is not necessary.
        if (load->GetLcl()->IsAddressExposed())
        {
            GenTree* next = node->gtNext;
            BlockRange().Unlink(load);
            BlockRange().Unlink(node);
            return next;
        }
    }

    value->SetRegOptional();

    return node->gtNext;
}

bool Lowering::ContainImmOperand(GenTree* instr, GenTree* operand) const
{
    assert(!instr->OperIsLeaf());

    if (!IsImmOperand(operand, instr))
    {
        return false;
    }

    operand->SetContained();
    return true;
}

bool Lowering::IsSafeToMoveForward(GenTree* move, GenTree* before)
{
    if (move->gtNext == before)
    {
        return true;
    }

    m_scratchSideEffects.Clear();
    m_scratchSideEffects.AddNode(comp, move);

    for (GenTree* node = move->gtNext; node != before; node = node->gtNext)
    {
        if (m_scratchSideEffects.InterferesWith(comp, node, /* strict */ true))
        {
            return false;
        }
    }

    return true;
}

bool Lowering::IsSafeToMoveMemOperandForward(GenTree* before, GenTree* mem)
{
    assert(IsMemOperand(mem));
    return IsSafeToMoveForward(mem, before);
}

bool Lowering::IsSafeToMoveAddrModeForward(GenTree* before, GenTreeAddrMode* addr) const
{
    return (addr->gtNext == before) || IsSafeToMoveLclRegUseForward(before, addr->GetBase(), addr->GetIndex());
}

bool Lowering::IsSafeToMoveLclRegUseForward(GenTree* before, GenTree* use1, GenTree* use2) const
{
    assert(before != nullptr);
    assert(use1 != use2);

    LclVarDsc* lcl1 = nullptr;

    if (use1 != nullptr)
    {
        if (GenTreeLclLoad* lclUse = use1->IsLclLoad())
        {
            if (!lclUse->GetLcl()->lvDoNotEnregister)
            {
                lcl1 = lclUse->GetLcl();
            }
        }
    }

    LclVarDsc* lcl2 = nullptr;

    if (use2 != nullptr)
    {
        if (GenTreeLclLoad* lclUse = use2->IsLclLoad())
        {
            if (!lclUse->GetLcl()->lvDoNotEnregister)
            {
                lcl2 = lclUse->GetLcl();
            }
        }
    }

    for (GenTree* cursor = before; (lcl1 != nullptr) || (lcl2 != nullptr); cursor = cursor->gtPrev)
    {
        assert(cursor != nullptr);

        if (cursor == use1)
        {
            lcl1 = nullptr;
        }
        else if (cursor == use2)
        {
            lcl2 = nullptr;
        }

        if (GenTreeLclStore* store = cursor->IsLclStore())
        {
            if ((store->GetLcl() == lcl1) || (store->GetLcl() == lcl2))
            {
                return false;
            }
        }
    }

    return true;
}

GenTreeLclLoad* Lowering::ReplaceWithLclLoad(LIR::Use& use, LclVarDsc* tempLcl)
{
    GenTree* def = use.Def();

    if (def->OperIs(GT_LCL_LOAD) && (tempLcl == nullptr))
    {
        return def->AsLclLoad();
    }

    GenTreeLclStore* store;
    use.ReplaceWithLclLoad(comp, tempLcl, &store);
    GenTreeLclLoad* load = use.Def()->AsLclLoad();

    LowerLclStore(store);
    LowerLclLoad(load);

    return load;
}

GenTree* Lowering::LowerSwitch(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_SWITCH));

    unsigned     jumpCnt;
    unsigned     targetCnt;
    BasicBlock** jumpTab;

    // The first step is to build the default case conditional construct that is
    // shared between both kinds of expansion of the switch node.

    // To avoid confusion, we'll alias m_block to originalSwitchBB
    // that represents the node we're morphing.
    BasicBlock* originalSwitchBB = m_block;
    LIR::Range& switchBBRange    = LIR::AsRange(originalSwitchBB);

    // jumpCnt is the number of elements in the jump table array.
    // jumpTab is the actual pointer to the jump table array.
    // targetCnt is the number of unique targets in the jump table array.
    jumpCnt   = originalSwitchBB->bbJumpSwt->bbsCount;
    jumpTab   = originalSwitchBB->bbJumpSwt->bbsDstTab;
    targetCnt = originalSwitchBB->NumSucc(comp);

// GT_SWITCH must be a top-level node with no use.
#ifdef DEBUG
    {
        LIR::Use use;
        assert(!switchBBRange.TryGetUse(node, &use));
    }
#endif

    JITDUMP("Lowering switch " FMT_BB ", %d cases\n", originalSwitchBB->bbNum, jumpCnt);

    // Handle a degenerate case: if the switch has only a default case, just convert it
    // to an unconditional branch. This should only happen in minopts or with debuggable
    // code.
    if (targetCnt == 1)
    {
        JITDUMP("Lowering switch " FMT_BB ": single target; converting to BBJ_ALWAYS\n", originalSwitchBB->bbNum);
        noway_assert(comp->opts.OptimizationDisabled());
        if (originalSwitchBB->bbNext == jumpTab[0])
        {
            originalSwitchBB->bbJumpKind = BBJ_NONE;
            originalSwitchBB->bbJumpDest = nullptr;
        }
        else
        {
            originalSwitchBB->bbJumpKind = BBJ_ALWAYS;
            originalSwitchBB->bbJumpDest = jumpTab[0];
        }
        // Remove extra predecessor links if there was more than one case.
        for (unsigned i = 1; i < jumpCnt; ++i)
        {
            (void)comp->fgRemoveRefPred(jumpTab[i], originalSwitchBB);
        }

        // We have to get rid of the GT_SWITCH node but a child might have side effects so just assign
        // the result of the child subtree to a temp.

        // TODO-MIKE-Cleanup: This seems useless, the switch value should simply be marked unused.

        GenTree*   value = node->GetOp(0);
        var_types  type  = varActualType(value->GetType());
        LclVarDsc* lcl   = comp->lvaNewTemp(type, true DEBUGARG("unused switch value temp"));
        GenTree*   store = comp->gtNewLclStore(lcl, type, value);

        switchBBRange.InsertAfter(node, store);
        switchBBRange.Unlink(node);

        return store;
    }

    noway_assert(jumpCnt >= 2);

    // Spill the argument to the switch node into a local so that it can be used later.
    LIR::Use use(switchBBRange, &node->AsOp()->gtOp1, node);
    ReplaceWithLclLoad(use);

    // GT_SWITCH(indexExpression) is now two statements:
    //   1. a statement containing 'asg' (for temp = indexExpression)
    //   2. and a statement with GT_SWITCH(temp)

    assert(node->OperIs(GT_SWITCH));
    GenTree*   temp        = node->AsUnOp()->GetOp(0);
    LclVarDsc* tempLcl     = temp->AsLclLoad()->GetLcl();
    var_types  tempLclType = temp->GetType();

    BasicBlock* defaultBB   = jumpTab[jumpCnt - 1];
    BasicBlock* followingBB = originalSwitchBB->bbNext;

    /* Is the number of cases right for a test and jump switch? */
    const bool fFirstCaseFollows = (followingBB == jumpTab[0]);
    const bool fDefaultFollows   = (followingBB == defaultBB);

    unsigned minSwitchTabJumpCnt = 2; // table is better than just 2 cmp/jcc

    // This means really just a single cmp/jcc (aka a simple if/else)
    if (fFirstCaseFollows || fDefaultFollows)
    {
        minSwitchTabJumpCnt++;
    }

#if defined(TARGET_ARM)
    // On ARM for small switch tables we will
    // generate a sequence of compare and branch instructions
    // because the code to load the base of the switch
    // table is huge and hideous due to the relocation... :(
    minSwitchTabJumpCnt += 2;
#endif // TARGET_ARM

    // Once we have the temporary variable, we construct the conditional branch for
    // the default case.  As stated above, this conditional is being shared between
    // both GT_SWITCH lowering code paths.
    // This condition is of the form: if (temp > jumpTableLength - 2){ goto jumpTable[jumpTableLength - 1]; }

    GenTree* switchValue = comp->gtNewLclLoad(tempLcl, tempLclType);
    GenTree* switchLimit = comp->gtNewIconNode(jumpCnt - 2, varActualType(tempLclType));
    GenTree* limitTest   = comp->gtNewOperNode(GT_GT, TYP_INT, switchValue, switchLimit);
    // Make sure we perform an unsigned comparison, just in case the switch index in 'temp'
    // is now less than zero 0 (that would also hit the default case).
    limitTest->SetRelopUnsigned(true);

    GenTree* limitBranch = comp->gtNewOperNode(GT_JTRUE, TYP_VOID, limitTest);

    switchBBRange.InsertAfter(switchBBRange.LastNode(), switchValue, switchLimit, limitTest, limitBranch);

    BasicBlock* afterDefaultCondBlock = comp->fgSplitBlockAfterNode(originalSwitchBB, limitBranch);

    // afterDefaultCondBlock is now the switch, and all the switch targets have it as a predecessor.
    // originalSwitchBB is now a BBJ_NONE, and there is a predecessor edge in afterDefaultCondBlock
    // representing the fall-through flow from originalSwitchBB.
    assert(originalSwitchBB->bbJumpKind == BBJ_NONE);
    assert(originalSwitchBB->bbNext == afterDefaultCondBlock);
    assert(afterDefaultCondBlock->bbJumpKind == BBJ_SWITCH);
    assert(afterDefaultCondBlock->bbJumpSwt->bbsHasDefault);
    assert(afterDefaultCondBlock->isEmpty()); // Nothing here yet.

    // The GT_SWITCH code is still in originalSwitchBB (it will be removed later).

    // Turn originalSwitchBB into a BBJ_COND.
    originalSwitchBB->bbJumpKind = BBJ_COND;
    originalSwitchBB->bbJumpDest = jumpTab[jumpCnt - 1];

    // Fix the pred for the default case: the default block target still has originalSwitchBB
    // as a predecessor, but the fgSplitBlockAfterStatement() moved all predecessors to point
    // to afterDefaultCondBlock.
    flowList* oldEdge = comp->fgRemoveRefPred(jumpTab[jumpCnt - 1], afterDefaultCondBlock);
    comp->fgAddRefPred(jumpTab[jumpCnt - 1], originalSwitchBB, oldEdge);

    bool useJumpSequence = jumpCnt < minSwitchTabJumpCnt;

#if defined(TARGET_UNIX) && defined(TARGET_ARM)
    // Force using an inlined jumping instead switch table generation.
    // Switch jump table is generated with incorrect values in CoreRT case,
    // so any large switch will crash after loading to PC any such value.
    // I think this is due to the fact that we use absolute addressing
    // instead of relative. But in CoreRT is used as a rule relative
    // addressing when we generate an executable.
    // See also https://github.com/dotnet/runtime/issues/8683
    // Also https://github.com/dotnet/coreclr/pull/13197
    useJumpSequence = useJumpSequence || comp->IsTargetAbi(CORINFO_CORERT_ABI);
#endif // defined(TARGET_UNIX) && defined(TARGET_ARM)

    // If we originally had 2 unique successors, check to see whether there is a unique
    // non-default case, in which case we can eliminate the switch altogether.
    // Note that the single unique successor case is handled above.
    BasicBlock* uniqueSucc = nullptr;
    if (targetCnt == 2)
    {
        uniqueSucc = jumpTab[0];
        noway_assert(jumpCnt >= 2);
        for (unsigned i = 1; i < jumpCnt - 1; i++)
        {
            if (jumpTab[i] != uniqueSucc)
            {
                uniqueSucc = nullptr;
                break;
            }
        }
    }
    if (uniqueSucc != nullptr)
    {
        // If the unique successor immediately follows this block, we have nothing to do -
        // it will simply fall-through after we remove the switch, below.
        // Otherwise, make this a BBJ_ALWAYS.
        // Now, fixup the predecessor links to uniqueSucc.  In the original jumpTab:
        //   jumpTab[i-1] was the default target, which we handled above,
        //   jumpTab[0] is the first target, and we'll leave that predecessor link.
        // Remove any additional predecessor links to uniqueSucc.
        for (unsigned i = 1; i < jumpCnt - 1; ++i)
        {
            assert(jumpTab[i] == uniqueSucc);
            (void)comp->fgRemoveRefPred(uniqueSucc, afterDefaultCondBlock);
        }
        if (afterDefaultCondBlock->bbNext == uniqueSucc)
        {
            afterDefaultCondBlock->bbJumpKind = BBJ_NONE;
            afterDefaultCondBlock->bbJumpDest = nullptr;
        }
        else
        {
            afterDefaultCondBlock->bbJumpKind = BBJ_ALWAYS;
            afterDefaultCondBlock->bbJumpDest = uniqueSucc;
        }
    }
    // If the number of possible destinations is small enough, we proceed to expand the switch
    // into a series of conditional branches, otherwise we follow the jump table based switch
    // transformation.
    else if (useJumpSequence || comp->compStressCompile(Compiler::STRESS_SWITCH_CMP_BR_EXPANSION, 50))
    {
        // Lower the switch into a series of compare and branch IR trees.
        //
        // In this case we will morph the node in the following way:
        // 1. Generate a JTRUE statement to evaluate the default case. (This happens above.)
        // 2. Start splitting the switch basic block into subsequent basic blocks, each of which will contain
        //    a statement that is responsible for performing a comparison of the table index and conditional
        //    branch if equal.

        JITDUMP("Lowering switch " FMT_BB ": using compare/branch expansion\n", originalSwitchBB->bbNum);

        // We'll use 'afterDefaultCondBlock' for the first conditional. After that, we'll add new
        // blocks. If we end up not needing it at all (say, if all the non-default cases just fall through),
        // we'll delete it.
        bool        fUsedAfterDefaultCondBlock = false;
        BasicBlock* currentBlock               = afterDefaultCondBlock;
        LIR::Range* currentBBRange             = &LIR::AsRange(currentBlock);

        // Walk to entries 0 to jumpCnt - 1. If a case target follows, ignore it and let it fall through.
        // If no case target follows, the last one doesn't need to be a compare/branch: it can be an
        // unconditional branch.
        bool fAnyTargetFollows = false;
        for (unsigned i = 0; i < jumpCnt - 1; ++i)
        {
            assert(currentBlock != nullptr);

            // Remove the switch from the predecessor list of this case target's block.
            // We'll add the proper new predecessor edge later.
            flowList* oldEdge = comp->fgRemoveRefPred(jumpTab[i], afterDefaultCondBlock);

            if (jumpTab[i] == followingBB)
            {
                // This case label follows the switch; let it fall through.
                fAnyTargetFollows = true;
                continue;
            }

            // We need a block to put in the new compare and/or branch.
            // If we haven't used the afterDefaultCondBlock yet, then use that.
            if (fUsedAfterDefaultCondBlock)
            {
                BasicBlock* newBlock = comp->fgNewBBafter(BBJ_NONE, currentBlock, true);
                comp->fgAddRefPred(newBlock, currentBlock); // The fall-through predecessor.
                currentBlock   = newBlock;
                currentBBRange = &LIR::AsRange(currentBlock);
            }
            else
            {
                assert(currentBlock == afterDefaultCondBlock);
                fUsedAfterDefaultCondBlock = true;
            }

            // We're going to have a branch, either a conditional or unconditional,
            // to the target. Set the target.
            currentBlock->bbJumpDest = jumpTab[i];

            // Wire up the predecessor list for the "branch" case.
            comp->fgAddRefPred(jumpTab[i], currentBlock, oldEdge);

            if (!fAnyTargetFollows && (i == jumpCnt - 2))
            {
                // We're processing the last one, and there is no fall through from any case
                // to the following block, so we can use an unconditional branch to the final
                // case: there is no need to compare against the case index, since it's
                // guaranteed to be taken (since the default case was handled first, above).

                currentBlock->bbJumpKind = BBJ_ALWAYS;
            }
            else
            {
                // Otherwise, it's a conditional branch. Set the branch kind, then add the
                // condition statement.
                currentBlock->bbJumpKind = BBJ_COND;

                GenTree* switchValue = comp->gtNewLclLoad(tempLcl, tempLclType);
                GenTree* caseValue   = comp->gtNewIconNode(i, tempLclType);
                GenTree* caseTest    = comp->gtNewOperNode(GT_EQ, TYP_INT, switchValue, caseValue);
                GenTree* caseBranch  = comp->gtNewOperNode(GT_JTRUE, TYP_VOID, caseTest);
                currentBBRange->InsertAfter(currentBBRange->LastNode(), switchValue, caseValue, caseTest, caseBranch);
            }
        }

        if (fAnyTargetFollows)
        {
            // There is a fall-through to the following block. In the loop
            // above, we deleted all the predecessor edges from the switch.
            // In this case, we need to add one back.
            comp->fgAddRefPred(currentBlock->bbNext, currentBlock);
        }

        if (!fUsedAfterDefaultCondBlock)
        {
            // All the cases were fall-through! We don't need this block.
            // Convert it from BBJ_SWITCH to BBJ_NONE and unset the BBF_DONT_REMOVE flag
            // so fgRemoveBlock() doesn't complain.
            JITDUMP("Lowering switch " FMT_BB ": all switch cases were fall-through\n", originalSwitchBB->bbNum);
            assert(currentBlock == afterDefaultCondBlock);
            assert(currentBlock->bbJumpKind == BBJ_SWITCH);
            currentBlock->bbJumpKind = BBJ_NONE;
            currentBlock->bbFlags &= ~BBF_DONT_REMOVE;
            comp->fgRemoveBlock(currentBlock, /* unreachable */ false); // It's an empty block.
        }
    }
    else
    {
        // At this point the default case has already been handled and we need to generate a jump
        // table based switch or a bit test based switch at the end of afterDefaultCondBlock. Both
        // switch variants need the switch value so create the necessary LclVar node here.
        GenTree*    switchValue      = comp->gtNewLclLoad(tempLcl, tempLclType);
        LIR::Range& switchBlockRange = LIR::AsRange(afterDefaultCondBlock);
        switchBlockRange.InsertAtEnd(switchValue);

        // Try generating a bit test based switch first,
        // if that's not possible a jump table based switch will be generated.
        if (!TryLowerSwitchToBitTest(jumpTab, jumpCnt, targetCnt, afterDefaultCondBlock, switchValue->AsLclLoad()))
        {
            JITDUMP("Lowering switch " FMT_BB ": using jump table expansion\n", originalSwitchBB->bbNum);

#ifdef TARGET_64BIT
            if (tempLclType != TYP_LONG)
            {
                // SWITCH_TABLE expects the switch value (the index into the jump table) to be LONG.
                // Note that the switch value is unsigned so the cast should be unsigned as well.
                switchValue = comp->gtNewOperNode(GT_UXT, TYP_LONG, switchValue);
                switchBlockRange.InsertAtEnd(switchValue);
            }
#endif

            GenTree* switchTable = comp->gtNewJmpTableNode();
            GenTree* switchJump  = comp->gtNewOperNode(GT_SWITCH_TABLE, TYP_VOID, switchValue, switchTable);
            switchBlockRange.InsertAfter(switchValue, switchTable, switchJump);

            // this block no longer branches to the default block
            afterDefaultCondBlock->bbJumpSwt->removeDefault();
        }

        comp->fgInvalidateSwitchDescMapEntry(afterDefaultCondBlock);
    }

    GenTree* next = node->gtNext;

    // Get rid of the GT_SWITCH(temp).
    switchBBRange.Unlink(node->AsUnOp()->GetOp(0));
    switchBBRange.Unlink(node);

    return next;
}

//------------------------------------------------------------------------
// TryLowerSwitchToBitTest: Attempts to transform a jump table switch into a bit test.
//
// Arguments:
//    jumpTable - The jump table
//    jumpCount - The number of blocks in the jump table
//    targetCount - The number of distinct blocks in the jump table
//    bbSwitch - The switch block
//    switchValue - A LclVar node that provides the switch value
//
// Return value:
//    true if the switch has been lowered to a bit test
//
// Notes:
//    If the jump table contains less than 32 (64 on 64 bit targets) entries and there
//    are at most 2 distinct jump targets then the jump table can be converted to a word
//    of bits where a 0 bit corresponds to one jump target and a 1 bit corresponds to the
//    other jump target. Instead of the indirect jump a BT-JCC sequence is used to jump
//    to the appropriate target:
//        mov eax, 245 ; jump table converted to a "bit table"
//        bt  eax, ebx ; ebx is supposed to contain the switch value
//        jc target1
//      target0:
//        ...
//      target1:
//    Such code is both shorter and faster (in part due to the removal of a memory load)
//    than the traditional jump table base code. And of course, it also avoids the need
//    to emit the jump table itself that can reach up to 256 bytes (for 64 entries).
//
bool Lowering::TryLowerSwitchToBitTest(BasicBlock*     jumpTable[],
                                       unsigned        jumpCount,
                                       unsigned        targetCount,
                                       BasicBlock*     bbSwitch,
                                       GenTreeLclLoad* switchValue)
{
#ifndef TARGET_XARCH
    // Other architectures may use this if they substitute GT_BT with equivalent code.
    return false;
#else
    assert(jumpCount >= 2);
    assert(targetCount >= 2);
    assert(bbSwitch->bbJumpKind == BBJ_SWITCH);

    //
    // Quick check to see if it's worth going through the jump table. The bit test switch supports
    // up to 2 targets but targetCount also includes the default block so we need to allow 3 targets.
    // We'll ensure that there are only 2 targets when building the bit table.
    //

    if (targetCount > 3)
    {
        return false;
    }

    //
    // The number of bits in the bit table is the same as the number of jump table entries. But the
    // jump table also includes the default target (at the end) so we need to ignore it. The default
    // has already been handled by a JTRUE(GT(switchValue, jumpCount - 2)) that LowerSwitch generates.
    //

    const unsigned bitCount = jumpCount - 1;

    if (bitCount > varTypeBitSize(TYP_I_IMPL))
    {
        return false;
    }

    //
    // Build a bit table where a bit set to 0 corresponds to bbCase0 and a bit set to 1 corresponds to
    // bbCase1. Simply use the first block in the jump table as bbCase1, later we can invert the bit
    // table and/or swap the blocks if it's beneficial.
    //

    BasicBlock* bbCase0  = nullptr;
    BasicBlock* bbCase1  = jumpTable[0];
    size_t      bitTable = 1;

    for (unsigned bitIndex = 1; bitIndex < bitCount; bitIndex++)
    {
        if (jumpTable[bitIndex] == bbCase1)
        {
            bitTable |= (size_t(1) << bitIndex);
        }
        else if (bbCase0 == nullptr)
        {
            bbCase0 = jumpTable[bitIndex];
        }
        else if (jumpTable[bitIndex] != bbCase0)
        {
            // If it's neither bbCase0 nor bbCase1 then it means we have 3 targets. There can't be more
            // than 3 because of the check at the start of the function.
            assert(targetCount == 3);
            return false;
        }
    }

    //
    // One of the case blocks has to follow the switch block. This requirement could be avoided
    // by adding a BBJ_ALWAYS block after the switch block but doing that sometimes negatively
    // impacts register allocation.
    //

    if ((bbSwitch->bbNext != bbCase0) && (bbSwitch->bbNext != bbCase1))
    {
        return false;
    }

#ifdef TARGET_64BIT
    //
    // See if we can avoid a 8 byte immediate on 64 bit targets. If all upper 32 bits are 1
    // then inverting the bit table will make them 0 so that the table now fits in 32 bits.
    // Note that this does not change the number of bits in the bit table, it just takes
    // advantage of the fact that loading a 32 bit immediate into a 64 bit register zero
    // extends the immediate value to 64 bit.
    //

    if (~bitTable <= UINT32_MAX)
    {
        bitTable = ~bitTable;
        std::swap(bbCase0, bbCase1);
    }
#endif

    //
    // Rewire the blocks as needed and figure out the condition to use for JCC.
    //

    GenCondition bbSwitchCondition;
    bbSwitch->bbJumpKind = BBJ_COND;

    comp->fgRemoveAllRefPreds(bbCase1, bbSwitch);
    comp->fgRemoveAllRefPreds(bbCase0, bbSwitch);

    if (bbSwitch->bbNext == bbCase0)
    {
        // GenCondition::C generates JC so we jump to bbCase1 when the bit is set
        bbSwitchCondition    = GenCondition::C;
        bbSwitch->bbJumpDest = bbCase1;

        comp->fgAddRefPred(bbCase0, bbSwitch);
        comp->fgAddRefPred(bbCase1, bbSwitch);
    }
    else
    {
        assert(bbSwitch->bbNext == bbCase1);

        // GenCondition::NC generates JNC so we jump to bbCase0 when the bit is not set
        bbSwitchCondition    = GenCondition::NC;
        bbSwitch->bbJumpDest = bbCase0;

        comp->fgAddRefPred(bbCase0, bbSwitch);
        comp->fgAddRefPred(bbCase1, bbSwitch);
    }

    //
    // Append BT(bitTable, switchValue) and JCC(condition) to the switch block.
    //

    var_types bitTableType = bitCount <= varTypeBitSize(TYP_INT) ? TYP_INT : TYP_LONG;
    GenTree*  bitTableIcon = comp->gtNewIconNode(bitTable, bitTableType);
    GenTree*  bitTest      = comp->gtNewOperNode(GT_BT, TYP_VOID, bitTableIcon, switchValue);
    bitTest->AddImplicitFlagsDef();
    GenTreeCC* jcc = new (comp, GT_JCC) GenTreeCC(GT_JCC, bbSwitchCondition);
    jcc->AddImplicitFlagsUse();

    LIR::AsRange(bbSwitch).InsertAfter(switchValue, bitTableIcon, bitTest, jcc);

    return true;
#endif // TARGET_XARCH
}

#ifdef TARGET_X86

void Lowering::InsertFieldListArgStore(GenTreeFieldList* fields, GenTreeCall* call, CallArgInfo* argInfo)
{
    assert(argInfo->GetRegCount() == 0);

    unsigned currentOffset = argInfo->GetStackSize();
    INDEBUG(unsigned prevFieldOffset = currentOffset);

    assert(fields->Uses().IsSorted());
    fields->Uses().Reverse();

    unsigned fieldIndex = 0;

    for (GenTreeFieldList::Use& use : fields->Uses())
    {
        GenTree*  value       = use.GetNode();
        unsigned  fieldOffset = use.GetOffset();
        var_types fieldType   = use.GetType();

        assert(fieldOffset < prevFieldOffset);
        INDEBUG(prevFieldOffset = fieldOffset);

        if (fieldType == TYP_LONG)
        {
            assert(value->OperIs(GT_LONG));
            assert(fieldOffset % REGSIZE_BYTES == 0);

            GenTree* valueLo = value->AsOp()->GetOp(0);
            GenTree* valueHi = value->AsOp()->GetOp(1);

            GenTreeArgStore* argStoreHi = NewArgStore(valueHi, call);
            argStoreHi->SetArgType(TYP_INT);
            argStoreHi->SetPushSize(currentOffset - fieldOffset - REGSIZE_BYTES);
            BlockRange().InsertBefore(fields, argStoreHi);
            LowerArgStore(argStoreHi);

            BlockRange().Unlink(value);

            currentOffset = fieldOffset + REGSIZE_BYTES;
            value         = valueLo;
            fieldType     = TYP_INT;
        }

        unsigned alignedOffset = fieldOffset & ~(REGSIZE_BYTES - 1);
        unsigned pushSize      = roundUp(currentOffset - alignedOffset, REGSIZE_BYTES);
        currentOffset -= pushSize;
        unsigned offset = fieldOffset - currentOffset;

        if (varTypeIsSmall(fieldType) && (pushSize >= 4) && (offset == 0))
        {
            fieldType = TYP_INT;
        }

        GenTreeArgStore* argStore = NewArgStore(value, call);
        argStore->SetArgType(fieldType);
        argStore->SetPushSize(pushSize);
        argStore->SetOffset(offset);
        BlockRange().InsertBefore(fields, argStore);
        LowerArgStore(argStore);

        if (fieldIndex++ == 0)
        {
            argInfo->SetNode(argStore);
        }
    }

    if (currentOffset != 0)
    {
        GenTreeIntCon*   zero     = comp->gtNewIconNode(0);
        GenTreeArgStore* argStore = NewArgStore(zero, call);
        argStore->SetArgType(TYP_VOID);
        argStore->SetPushSize(currentOffset);
        BlockRange().InsertBefore(fields, zero, argStore);
        zero->SetContained();
    }
}

#else // !TARGET_X86

void Lowering::InsertFieldListArgStore(GenTreeFieldList* fields, GenTreeCall* call, CallArgInfo* argInfo)
{
#ifndef TARGET_ARM
    assert(argInfo->GetRegCount() == 0);
#endif

    unsigned argOffset  = argInfo->GetStackOffset();
    unsigned fieldIndex = 0;

    for (GenTreeFieldList::Use& use : fields->Uses())
    {
        GenTree*         value       = use.GetNode();
        unsigned         fieldOffset = use.GetOffset();
        var_types        fieldType   = use.GetType();

#ifndef TARGET_64BIT
        GenTreeArgStore* argStoreHi  = nullptr;

        if (fieldType == TYP_LONG)
        {
            assert(value->OperIs(GT_LONG));
            assert(fieldOffset % REGSIZE_BYTES == 0);

            GenTree* valueLo = value->AsOp()->GetOp(0);
            GenTree* valueHi = value->AsOp()->GetOp(1);

            argStoreHi = NewArgStore(valueHi, call);
            argStoreHi->SetArgType(TYP_INT);
            argStoreHi->SetOffset(argOffset + fieldOffset + 4);
            argStoreHi->SetSplitRegCount(0);

            BlockRange().Unlink(value);

            value     = valueLo;
            fieldType = TYP_INT;
        }
#endif

        GenTreeArgStore* argStore = NewArgStore(value, call);
        argStore->SetArgType(fieldType);
        argStore->SetOffset(argOffset + fieldOffset);
        argStore->SetSplitRegCount(0);
        BlockRange().InsertBefore(fields, argStore);
        LowerArgStore(argStore);

#ifndef TARGET_64BIT
        if (argStoreHi != nullptr)
        {
            BlockRange().InsertBefore(fields, argStoreHi);
            LowerArgStore(argStoreHi);
        }
#endif

        if (fieldIndex++ == 0 ARM_ONLY(&&(argInfo->GetRegCount() == 0)))
        {
            argInfo->SetNode(argStore);
        }
    }
}

#endif // !TARGET_X86

#if FEATURE_MULTIREG_ARGS

void Lowering::InsertFieldListArgReg(GenTreeFieldList* fields, GenTreeCall* call, CallArgInfo* argInfo)
{
    unsigned          regIndex = 0;
    GenTreeCall::Use* after    = argInfo->GetUse();

    for (GenTreeFieldList::Use *nextField, *field = fields->Uses().GetHead(); field != nullptr; field = nextField)
    {
        nextField = field->GetNext();

        GenTree* putArgReg = InsertPutArgReg(field->GetNode(), argInfo, regIndex);

        if (regIndex == 0)
        {
            argInfo->SetNode(putArgReg);
        }
        else
        {
            static_assert_no_msg(sizeof(GenTreeCall::Use) <= sizeof(GenTreeFieldList::Use));
            GenTreeCall::Use* newUse = new (field) GenTreeCall::Use(putArgReg, after->GetNext());
            after->SetNext(newUse);
            after = newUse;
        }

#ifdef TARGET_ARM
        regIndex += putArgReg->TypeIs(TYP_LONG) ? 2 : 1;
#else
        regIndex++;
#endif
    }
}

#endif // FEATURE_MULTIREG_ARGS

#ifdef TARGET_ARM

void Lowering::InsertPutArgSplit(GenTreeCall* call, CallArgInfo* argInfo)
{
    if (call->IsFastTailCall())
    {
        NYI_ARM("fast tail call with split argument");
    }

    assert(Compiler::typIsLayoutNum(argInfo->GetSigTypeNum()));
    assert(argInfo->GetStackSize() != 0);
    assert(argInfo->GetStackOffset() == 0);
    assert((0 <= argInfo->GetRegCount()) && (argInfo->GetRegCount() <= MAX_ARG_REG_COUNT));

    GenTree* arg = argInfo->GetNode();

    GenTreeArgStore* argStore = NewArgStore(arg, call);
    argStore->SetArgTypeNum(argInfo->GetSigTypeNum());
    argStore->SetSplitRegCount(argInfo->GetRegCount());
    BlockRange().InsertAfter(arg, argStore);
    LowerArgStore(argStore);

    const unsigned regCount = argInfo->GetRegCount();
    GenTree*       regDefs[MAX_ARG_REG_COUNT];
    GenTree*       after     = argStore;
    ClassLayout*   argLayout = comp->typGetLayoutByNum(argInfo->GetSigTypeNum());
    assert(argInfo->GetRegCount() <= argLayout->GetSlotCount());

    if (arg->IsIntCon(0))
    {
        for (unsigned i = 0; i < regCount; i++)
        {
            GenTree* regVal = comp->gtNewIconNode(0);
            regDefs[i]      = comp->gtNewOperNode(GT_PUTARG_REG, TYP_INT, regVal);
            BlockRange().InsertAfter(after, regVal, regDefs[i]);
        }
    }
    else if (GenTreeIndLoadObj* load = arg->IsIndLoadObj())
    {
        GenTree*  baseAddr = load->GetAddr();
        int32_t   offset   = 0;
        GenTree** baseAddrUse;
        GenTree*  user;

        if (baseAddr->isContained())
        {
            GenTreeAddrMode* am = baseAddr->AsAddrMode();
            assert(!am->HasIndex());
            baseAddr    = am->GetBase();
            offset      = am->GetOffset();
            baseAddrUse = &am->gtOp1;
            user        = am;
        }
        else
        {
            baseAddrUse = &load->gtOp1;
            user        = load;
        }

        if (!baseAddr->OperIs(GT_LCL_LOAD) || baseAddr->AsLclLoad()->GetLcl()->lvDoNotEnregister)
        {
            LIR::Use use(BlockRange(), baseAddrUse, user);
            baseAddr = ReplaceWithLclLoad(use);
        }

        for (unsigned i = 0; i < regCount; i++, offset += REGSIZE_BYTES)
        {
            baseAddr = comp->gtNewLclLoad(baseAddr->AsLclLoad()->GetLcl(), baseAddr->GetType());

            GenTree* regAddr = comp->gtNewAddrMode(baseAddr, offset);
            GenTree* regVal  = comp->gtNewIndLoad(argLayout->GetGCPtrType(i), regAddr);
            regDefs[i]       = comp->gtNewOperNode(GT_PUTARG_REG, varActualType(regVal->GetType()), regVal);
            BlockRange().InsertAfter(after, baseAddr, regAddr, regVal, regDefs[i]);
            regAddr->SetContained();
            after = regDefs[i];
        }
    }
    else
    {
        LclVarDsc* srcLcl;
        unsigned   srcOffset;

        if (arg->OperIs(GT_LCL_LOAD))
        {
            srcLcl    = arg->AsLclLoad()->GetLcl();
            srcOffset = 0;
        }
        else
        {
            srcLcl    = arg->AsLclLoadFld()->GetLcl();
            srcOffset = arg->AsLclLoadFld()->GetLclOffs();
        }

        for (unsigned i = 0; i < regCount; i++, srcOffset += REGSIZE_BYTES)
        {
            GenTree* regVal = comp->gtNewLclLoadFld(argLayout->GetGCPtrType(i), srcLcl, srcOffset);
            regDefs[i]      = comp->gtNewOperNode(GT_PUTARG_REG, varActualType(regVal->GetType()), regVal);
            BlockRange().InsertAfter(after, regVal, regDefs[i]);
            after = regDefs[i];
        }
    }

    for (unsigned i = 0; i < regCount; i++)
    {
        regDefs[i]->SetRegNum(argInfo->GetRegNum(i));

        if (i == 0)
        {
            argInfo->SetNode(regDefs[i]);
        }
        else
        {
            comp->gtInsertNewCallArgAfter(regDefs[i], argInfo->GetUse());
        }
    }
}

void Lowering::InsertFieldListArgSplit(GenTreeFieldList* fields, GenTreeCall* call, CallArgInfo* argInfo)
{
    assert(argInfo->GetNode() == fields);

    if (call->IsFastTailCall())
    {
        NYI_ARM("fast tail call with split argument");
    }

    GenTreeFieldList::Use* regUse = fields->Uses().GetHead();
    GenTree*               before = fields->gtNext;

    for (unsigned i = 0; i < argInfo->GetRegCount(); i++)
    {
        GenTree* regVal = regUse->GetNode();

        if (regVal->TypeIs(TYP_DOUBLE))
        {
            GenTree* bitcast = comp->gtNewBitCastNode(TYP_LONG, regVal);
            bitcast->SetRegNum(0, argInfo->GetRegNum(i));
            bitcast->SetRegNum(1, argInfo->GetRegNum(i + 1));
            BlockRange().InsertAfter(regVal, bitcast);
            regVal = bitcast;
        }

        GenTree* regDef = comp->gtNewOperNode(GT_PUTARG_REG, varActualType(regVal->GetType()), regVal);
        regDef->SetRegNum(argInfo->GetRegNum(i));

        if (regVal->TypeIs(TYP_LONG))
        {
            regDef->SetRegNum(1, argInfo->GetRegNum(++i));
        }

        BlockRange().InsertBefore(before, regDef);

        if (argInfo->GetNode() == fields)
        {
            argInfo->SetNode(regDef);
        }
        else
        {
            comp->gtInsertNewCallArgAfter(regDef, argInfo->GetUse());
        }

        regUse = regUse->GetNext();
        fields->Uses().SetHead(regUse);
    }

    for (GenTreeFieldList::Use& stackUse : fields->Uses())
    {
        stackUse.SetOffset(stackUse.GetOffset() - argInfo->GetRegCount() * REGSIZE_BYTES);
    }

    BlockRange().MoveBefore(before, fields);
    InsertFieldListArgStore(fields, call, argInfo);
}

#endif // TARGET_ARM

#ifdef TARGET_ARM64

void Lowering::InsertFieldListArgSplit(GenTreeFieldList* fields, GenTreeCall* call, CallArgInfo* argInfo)
{
    assert(argInfo->GetRegCount() == 1);
    assert(argInfo->GetRegNum(0) == REG_R7);
    assert(argInfo->GetStackOffset() == 0);
    assert(argInfo->GetStackSize() == REGSIZE_BYTES);

    GenTreeFieldList::Use* regUse = fields->Uses().GetHead();
    assert(regUse->GetOffset() == 0);

    GenTreeFieldList::Use* stackUse = regUse->GetNext();
    assert(stackUse->GetOffset() == 8);
    assert(stackUse->GetNext() == nullptr);

    GenTree* regVal = regUse->GetNode();
    GenTree* regDef = comp->gtNewOperNode(GT_PUTARG_REG, varActualType(regVal->GetType()), regVal);
    regDef->SetRegNum(REG_R7);
    BlockRange().InsertAfter(regVal, regDef);
    argInfo->SetNode(regDef);

    GenTree*         stackVal = stackUse->GetNode();
    GenTreeArgStore* argStore = NewArgStore(stackVal, call);
    argStore->SetArgType(varActualType(stackUse->GetType()));
    BlockRange().InsertAfter(fields, argStore);

    LowerArgStore(argStore);
}

#endif // TARGET_ARM64

GenTreeArgStore* Lowering::NewArgStore(GenTree* value, GenTreeCall* call)
{
    return new (comp, GT_ARG_STORE) GenTreeArgStore(value, call);
}

GenTreeArgStore* Lowering::NewArgStore(GenTree* value, CallArgInfo* argInfo, GenTreeCall* call)
{
    assert(argInfo->GetSigTypeNum() != 0);
    assert(argInfo->GetRegCount() == 0);
#ifdef WINDOWS_AMD64_ABI
    assert(argInfo->GetStackSize() == REGSIZE_BYTES);
#endif

    GenTreeArgStore* store = new (comp, GT_ARG_STORE) GenTreeArgStore(value, call);
    store->SetArgTypeNum(argInfo->GetSigTypeNum());
#if FEATURE_FIXED_OUT_ARGS
    store->SetOffset(argInfo->GetStackOffset());
#else
    store->SetPushSize(argInfo->GetStackSize());
#endif

    if (Compiler::typIsLayoutNum(store->GetArgTypeNum()))
    {
        ClassLayout* layout = comp->typGetLayoutByNum(store->GetArgTypeNum());

        if (!varTypeIsStruct(value->GetType()))
        {
            if (!value->IsIntCon(0) || (layout->GetSize() <= REGSIZE_BYTES))
            {
                store->SetArgType(varActualType(value->GetType()));
            }
        }
#ifdef FEATURE_SIMD
        else if (varTypeIsSIMD(value->GetType()))
        {
            var_types argType;

            if (layout->IsVector())
            {
                argType = layout->GetVectorType();
            }
            else
            {
                argType = value->GetType();

                if (varTypeSize(argType) > layout->GetSize())
                {
                    if (layout->GetSize() == 8)
                    {
                        argType = TYP_SIMD8;
                    }
                    else
                    {
                        assert(layout->GetSize() == 12);
                        argType = TYP_SIMD12;
                    }
                }
            }

            if ((argType == TYP_SIMD12) && (argInfo->GetStackSize() >= 16))
            {
                argType = TYP_SIMD16;
            }

            store->SetArgType(argType);
        }
#endif
    }
    else
    {
        assert(!varTypeIsStruct(value->GetType()));

        if (argInfo->GetStackSize() == REGSIZE_BYTES)
        {
#ifdef TARGET_ARM64
            if (value->IsIntCon(0))
            {
                store->SetArgType(TYP_LONG);
            }
            else
#endif
            {
                store->SetArgType(varActualType(value->GetType()));
            }
        }
    }

    return store;
}

void Lowering::InsertFieldListPutArg(GenTreeCall* call, CallArgInfo* argInfo)
{
    GenTreeFieldList* fields = argInfo->GetNode()->AsFieldList();

    if (argInfo->GetRegCount() == 0)
    {
        InsertFieldListArgStore(fields, call, argInfo);
    }
    else if (argInfo->GetStackSize() != 0)
    {
#if FEATURE_ARG_SPLIT
        InsertFieldListArgSplit(fields, call, argInfo);
#else
        unreached();
#endif
    }
    else
    {
#if FEATURE_MULTIREG_ARGS
        InsertFieldListArgReg(fields, call, argInfo);
#else
        unreached();
#endif
    }

    BlockRange().Unlink(fields);
}

void Lowering::InsertPutArg(GenTreeCall* call, CallArgInfo* argInfo)
{
    GenTree* arg = argInfo->GetNode();

    assert(!arg->IsFieldList());
    assert(!arg->OperIs(GT_IND_LOAD_OBJ) || arg->TypeIs(TYP_STRUCT));

    if (arg->TypeIs(TYP_STRUCT) && !arg->IsCall())
    {
        arg->SetContained();
    }

    if (argInfo->GetRegCount() == 0)
    {
        GenTreeArgStore* argStore = NewArgStore(arg, argInfo, call);
        BlockRange().InsertAfter(arg, argStore);
        argInfo->SetNode(argStore);
        LowerArgStore(argStore);
    }
    else if (argInfo->GetStackSize() != 0)
    {
#ifdef TARGET_ARM
        InsertPutArgSplit(call, argInfo);
#else
        unreached();
#endif
    }
    else
    {
        GenTree* putArgReg = InsertPutArgReg(arg, argInfo, 0);
        argInfo->SetNode(putArgReg);

#ifdef TARGET_ARM
        assert(argInfo->GetRegCount() == (putArgReg->TypeIs(TYP_LONG) ? 2u : 1u));
#else
        assert(argInfo->GetRegCount() == 1);
#endif
    }
}

GenTree* Lowering::InsertPutArgReg(GenTree* arg, CallArgInfo* argInfo, unsigned regIndex)
{
    var_types type   = varActualType(arg->GetType());
    RegNum    argReg = argInfo->GetRegNum(regIndex);

#ifdef TARGET_ARM
    // LONG args are passed via FIELD_LIST.
    assert(type != TYP_LONG);

    if ((type == TYP_DOUBLE) && genIsValidIntReg(argReg))
    {
        GenTree* intArg = comp->gtNewBitCastNode(TYP_LONG, arg);
        intArg->SetRegNum(argReg);
        intArg->SetRegNum(1, argInfo->GetRegNum(regIndex + 1));
        BlockRange().InsertAfter(arg, intArg);

        arg  = intArg;
        type = TYP_LONG;
    }

    GenTree* putArg = comp->gtNewOperNode(GT_PUTARG_REG, type, arg);

    if (type == TYP_LONG)
    {
        putArg->SetRegNum(1, argInfo->GetRegNum(regIndex + 1));
    }
#else
    GenTree* putArg = comp->gtNewOperNode(GT_PUTARG_REG, type, arg);
#endif

    assert(varTypeUsesFloatReg(type) == genIsValidFloatReg(argReg));

    putArg->SetRegNum(argReg);
    BlockRange().InsertAfter(arg, putArg);
    return putArg;
}

#ifndef TARGET_64BIT

void Lowering::InsertLongPutArg(GenTreeCall* call, CallArgInfo* argInfo)
{
    GenTree* arg = argInfo->GetNode();

    if (arg->OperIs(GT_BITCAST))
    {
        if (argInfo->GetRegCount() != 0)
        {
            assert(argInfo->GetRegCount() == 2);

            GenTree* putArg = comp->gtNewOperNode(GT_PUTARG_REG, TYP_LONG, arg);
            putArg->SetRegNum(0, argInfo->GetRegNum(0));
            putArg->SetRegNum(1, argInfo->GetRegNum(1));
            BlockRange().InsertAfter(arg, putArg);
            argInfo->SetNode(putArg);
        }
        else
        {
            BlockRange().Unlink(arg);
            arg = arg->AsUnOp()->GetOp(0);
            assert(arg->TypeIs(TYP_DOUBLE X86_ARG(TYP_SIMD8)));
            argInfo->SetNode(arg);
            argInfo->SetArgType(arg->GetType());
            argInfo->GetUse()->SetSigTypeNum(static_cast<unsigned>(arg->GetType()));
            InsertPutArg(call, argInfo);
        }

        return;
    }

    noway_assert(arg->OperIs(GT_LONG));

    GenTree* argLo = arg->AsOp()->GetOp(0);
    GenTree* argHi = arg->AsOp()->GetOp(1);

#if FEATURE_MULTIREG_ARGS
    if (argInfo->GetRegCount() == 2)
    {
        GenTree* putArgRegLo = InsertPutArgReg(argLo, argInfo, 0);
        GenTree* putArgRegHi = InsertPutArgReg(argHi, argInfo, 1);

        comp->gtInsertNewCallArgAfter(putArgRegHi, argInfo->GetUse());

        argInfo->SetNode(putArgRegLo);
    }
    else
#endif
    {
        noway_assert(argInfo->GetRegCount() == 0);

        GenTreeArgStore* argStoreLo = NewArgStore(argLo, call);
        GenTreeArgStore* argStoreHi = NewArgStore(argHi, call);
        argStoreLo->SetArgType(TYP_INT);
        argStoreHi->SetArgType(TYP_INT);
#if FEATURE_FIXED_OUT_ARGS
        argStoreLo->SetOffset(argInfo->GetStackOffset() + 0);
        argStoreHi->SetOffset(argInfo->GetStackOffset() + 4);
        BlockRange().InsertAfter(arg, argStoreLo, argStoreHi);
#else
        argStoreLo->SetPushSize(4);
        argStoreHi->SetPushSize(4);
        BlockRange().InsertAfter(arg, argStoreHi, argStoreLo);
        LowerArgStore(argStoreHi);
        LowerArgStore(argStoreLo);
#endif

        argInfo->SetNode(argStoreLo);
    }

    BlockRange().Unlink(arg);
}

#endif // TARGET_64BIT

void Lowering::LowerCallArgs(GenTreeCall* call)
{
    CallInfo* info = call->GetInfo();

#if FEATURE_FIXED_OUT_ARGS
    if (!call->IsFastTailCall())
    {
        unsigned callArgSize = info->GetStackArgsSize();

        if (callArgSize > outgoingArgAreaSize)
        {
            outgoingArgAreaSize = callArgSize;
            JITDUMP("\nIncreasing outgoingArgAreaSize to %u for call [%06u]\n\n", outgoingArgAreaSize, call->GetID());
        }
    }
#endif

#ifndef TARGET_X86
    // TODO-MIKE-Review: What does the arg slot count has to do with x64 or any other non-x86
    // architectures? This condition does reduce code size but it appears to do so by accident:
    // EBP based address modes have smaller encoding than ESP based ones but then this basically
    // counts arg stores and those always use ESP. What we really need is the number of non-arg
    // stack references that exist, and this has nothing to do with that.
    if (info->GetStackArgsSize() - INIT_ARG_STACK_SLOT * REGSIZE_BYTES >= 4 * REGSIZE_BYTES)
    {
        comp->opts.SetFramePointerRequired();
    }
#endif

    for (unsigned i = 0; i < info->GetArgCount(); i++)
    {
        JITDUMPLIRRANGE(BlockRange(), info->GetArgInfo(i)->GetNode(), "Lowering CALL arg %u (before):\n", i);
        LowerCallArg(call, info->GetArgInfo(i));
        JITDUMPLIRRANGE(BlockRange(), info->GetArgInfo(i)->GetNode(), "Lowering CALL arg %u (after):\n", i);
        JITDUMP("\n");
    }
}

void Lowering::LowerCallArg(GenTreeCall* call, CallArgInfo* argInfo)
{
    GenTree* arg = argInfo->GetNode();

    assert(!arg->OperIs(GT_PUTARG_REG, GT_ARG_STORE));
    assert(arg->IsValue());

    if (arg->IsFieldList())
    {
        InsertFieldListPutArg(call, argInfo);
    }
#ifndef TARGET_64BIT
    else if (arg->TypeIs(TYP_LONG))
    {
        InsertLongPutArg(call, argInfo);
    }
#endif
    else
    {
        InsertPutArg(call, argInfo);
    }
}

void Lowering::LowerCall(GenTreeCall* call)
{
    JITDUMPLIRRANGE(BlockRange(), call, "\nLowering CALL (before):\n");

#ifdef UNIX_AMD64_ABI
    if (!call->IsFastTailCall())
    {
        comp->codeGen->needToAlignFrame = true;
    }
#endif

    call->ClearOtherRegs();

#ifdef TARGET_X86
    if (call->IsTailCallViaJitHelper())
    {
        LowerTailCallViaJitHelper(call);
    }
    else
#endif
        if (call->IsVirtualVtable())
    {
        if (!call->IsExpandedEarly())
        {
            call->SetCallAddr(LowerVirtualVtableCall(call));
        }
    }
    else if (call->IsVirtualStubIndirect())
    {
        call->SetCallAddr(LowerIndirectVirtualStubCall(call));
    }
    else if (call->IsVirtualStubDirect())
    {
        call->SetCallAddr(LowerVirtualStubCall(call));
    }
    else if (call->IsDelegateInvoke())
    {
        call->SetCallAddr(LowerDelegateInvoke(call));
    }
    else if (call->IsUnmanaged())
    {
        InsertUnmanagedCallPrologAndEpilog(call);

        if (!call->IsIndirectCall())
        {
            call->SetCallAddr(LowerDirectUnmanagedCall(call));
        }
    }
    else if (!call->IsIndirectCall())
    {
        assert(!call->IsVirtual());

        call->SetCallAddr(LowerDirectCall(call));
    }

    LowerCallArgs(call);

#if FEATURE_FASTTAILCALL
    if (call->IsFastTailCall())
    {
        // Lower fast tail call can introduce new temps to set up args correctly for Callee.
        // This involves patching LCL_* nodes holding caller stack args and replacing them
        // with a new temp. Control expr also can contain nodes that need to be patched.
        // Therefore lower fast tail call must be done after controlExpr is inserted into LIR.
        LowerFastTailCall(call);
    }
#endif

    RemoveNonRegCallArgs(call);

    if (varTypeIsStruct(call->GetType()))
    {
        LowerStructCall(call);
    }

#ifdef TARGET_XARCH
    ContainCheckCallAddr(call);
#endif

    JITDUMPLIRRANGE(BlockRange(), call, "Lowering CALL (after):\n");
    JITDUMP("\n");
}

void Lowering::RemoveNonRegCallArgs(GenTreeCall* call)
{
    GenTreeCall::Use** prevUseLink = &call->m_uses;

    for (GenTreeCall::Use& use : call->Uses())
    {
        GenTree* node = use.GetNode();

        if (node->IsArgStore())
        {
            continue;
        }

        assert(!node->IsFieldList());

        *prevUseLink = &use;
        prevUseLink  = &use.NextRef();
    }

    *prevUseLink = nullptr;
}

#if FEATURE_FASTTAILCALL

// Lower a call node dispatched as a fast tail call (epilog + jmp).
//
// For fast tail calls it is necessary to set up stack args in the incoming
// arg stack space area. When args passed also come from this area we may
// run into problems because we may end up overwriting the stack slot before
// using it. For example, for foo(a, b) { return bar(b, a); }, if a and b
// are on incoming arg stack space in foo they need to be swapped in this
// area for the call to bar. This function detects this situation and
// introduces a temp when an outgoing argument would overwrite a later-used
// incoming argument.
//
// This function also handles inserting necessary profiler hooks and PInvoke
// method epilogs in case there are inlined PInvokes.
void Lowering::LowerFastTailCall(GenTreeCall* call)
{
    assert(call->IsFastTailCall());

    // Tail call restrictions i.e. conditions under which tail prefix is ignored.
    // Most of these checks are already done by importer or fgMorphTailCall().
    // This serves as a double sanity check.
    assert(!comp->info.IsSynchronized());
    assert(!comp->opts.IsReversePInvoke());
    assert(!call->IsUnmanaged());
    assert(!comp->compLocallocUsed);

#ifdef TARGET_AMD64
    assert(!comp->getNeedsGSSecurityCookie()); // jit64 compat: tail calls from methods that need GS check
#endif

    // VM cannot use return address hijacking when A() and B() tail call each
    // other in mutual recursion.  Therefore, this block is reachable through
    // a GC-safe point or the whole method is marked as fully interruptible.
    //
    // TODO-Cleanup:
    // fgReachWithoutCall() depends on the fact that loop headers blocks
    // will have a block number > fgLastBB.  These loop headers gets added
    // after dominator computation and get skipped by OptReachWithoutCall().
    // The below condition cannot be asserted in lower because we may add
    // new basic blocks for range check failure, which have higher block
    // numbers than the loop header block number.
    //
    // assert(m_block->HasGCSafePoint() ||
    //        !comp->fgReachWithoutCall(comp->fgFirstBB, m_block) || comp->GetInterruptible());

    // If PInvokes are in-lined, we have to remember to execute PInvoke method epilog anywhere
    // that a method returns. This is a case of caller method has both PInvokes and tail calls.
    if (comp->info.IsPInvokeFrameRequired())
    {
        InsertPInvokeMethodEpilog(INDEBUG(call));
    }

    // Args for tail call are setup in incoming arg area. The GC-ness of args of
    // caller and callee (which being tail called) may not match. Therefore, everything
    // from arg setup until the epilog need to be non-interruptible by GC. This is
    // achieved by inserting GT_START_NONGC before the very first GT_ARG_STORE node
    // of call is setup. Note that once a stack arg is setup, it cannot have nested
    // calls subsequently in execution order to setup other args, because the nested
    // call could over-write the stack arg that is setup earlier.
    ArrayStack<GenTreeArgStore*> putargs(comp->getAllocator(CMK_ArrayStack));

    for (GenTreeUse& use : call->Uses())
    {
        if (GenTreeArgStore* argStore = use.GetNode()->IsArgStore())
        {
            putargs.Push(argStore);
        }
    }

    GenTree* startNonGCNode = nullptr;

    if (!putargs.Empty())
    {
        // Get the earliest operand of the first PUTARG_STK node. We will make
        // the required copies of args before this node.
        GenTree* insertionPoint = BlockRange().FindFirstTreeLeaf(putargs.Get(0));
        // Insert GT_START_NONGC node before we evaluate the PUTARG_STK args.
        // Note that if there are no args to be setup on stack, no need to
        // insert GT_START_NONGC node.
        startNonGCNode = new (comp, GT_START_NONGC) GenTree(GT_START_NONGC, TYP_VOID);
        BlockRange().InsertBefore(insertionPoint, startNonGCNode);

        // GC-interruptability in the following case:
        //     foo(a, b, c, d, e) { bar(a, b, c, d, e); }
        //     bar(a, b, c, d, e) { foo(a, b, d, d, e); }
        //
        // Since the instruction group starting from the instruction that sets up first
        // stack arg to the end of the tail call is marked as GC non-interruptible, this
        // will form a non-interruptible tight loop causing GC-starvation. To fix this
        // we insert GT_NO_OP as embedded stmt before GT_START_NONGC, if the method has
        // a single basic block and is not a GC-safe point. The presence of a single NOP
        // outside GC non-interruptible region will prevent GC starvation.
        if ((comp->fgBBcount == 1) && !m_block->HasGCSafePoint())
        {
            assert(comp->fgFirstBB == m_block);
            GenTree* noOp = new (comp, GT_NO_OP) GenTree(GT_NO_OP, TYP_VOID);
            BlockRange().InsertBefore(startNonGCNode, noOp);
        }

        // Since this is a fast tail call each PUTARG_STK will place the argument in the
        // _incoming_ arg space area. This will effectively overwrite our already existing
        // incoming args that live in that area. If we have later uses of those args, this
        // is a problem. We introduce a defensive copy into a temp here of those args that
        // potentially may cause problems.
        for (unsigned i = 0; i < putargs.Size(); i++)
        {
            GenTreeArgStore* arg = putargs.Get(i);

#ifdef WINDOWS_AMD64_ABI
            unsigned argSize = REGSIZE_BYTES;
#else
            unsigned argTypeNum = arg->GetArgTypeNum();
            unsigned argSize;

            if (Compiler::typIsLayoutNum(argTypeNum))
            {
                ClassLayout* argLayout = comp->typGetLayoutByNum(argTypeNum);
                argSize                = argLayout->GetSize();
            }
            else
            {
                argSize = varTypeSize(static_cast<var_types>(argTypeNum));
            }
#endif

            unsigned argStartOffset = arg->GetOffset();
            unsigned argEndOffset   = argStartOffset + roundUp(argSize, REGSIZE_BYTES);

            for (LclVarDsc* paramLcl : comp->Params())
            {
                if (paramLcl->IsRegParam())
                {
                    continue;
                }

                assert(paramLcl->GetStackOffset() != BAD_STK_OFFS);

                unsigned paramStartOffset = static_cast<unsigned>(paramLcl->GetStackOffset());
#ifdef WINDOWS_AMD64_ABI
                unsigned paramEndOffset = paramStartOffset + REGSIZE_BYTES;
#else
                unsigned paramEndOffset = paramStartOffset + paramLcl->GetFrameSize();
#endif

                // If ranges do not overlap then this PUTARG_STK will not mess up the arg.
                if ((argEndOffset <= paramStartOffset) || (argStartOffset >= paramEndOffset))
                {
                    continue;
                }

                // Codegen cannot handle a partially overlapping copy. For example, if we have
                // bar(S16 stack, S32 stack2)
                // foo(S32 stack, S32 stack2) { bar(..., stack) }
                // then we may end up having to move 'stack' in foo 16 bytes ahead. It is possible
                // that this PUTARG_STK is the only use, in which case we will need to introduce
                // a temp, so look for uses starting from it. Note that we assume that in-place
                // copies are OK.
                GenTree* lookForUsesFrom = arg->gtNext;

                if (argStartOffset != paramStartOffset)
                {
                    lookForUsesFrom = insertionPoint;
                }

                RehomeParamForFastTailCall(paramLcl, insertionPoint, lookForUsesFrom, call);

                if (paramLcl->IsPromoted())
                {
                    for (LclVarDsc* fieldLcl : comp->PromotedFields(paramLcl))
                    {
                        RehomeParamForFastTailCall(fieldLcl, insertionPoint, lookForUsesFrom, call);
                    }
                }
            }
        }
    }

    if (comp->opts.IsProfilerHookNeeded())
    {
        InsertProfTailCallHook(call, startNonGCNode);
    }
}

void Lowering::InsertProfTailCallHook(GenTreeCall* call, GenTree* startNonGCNode)
{
    assert(comp->opts.IsProfilerHookNeeded());
    assert(call->IsFastTailCall());

    GenTree* insertionPoint = startNonGCNode;

    if (insertionPoint == nullptr)
    {
        for (GenTreeUse& use : call->Uses())
        {
            if (use.GetNode()->OperIs(GT_PUTARG_REG))
            {
                insertionPoint = use.GetNode();
                break;
            }
        }

        if (insertionPoint == nullptr)
        {
            insertionPoint = call;
        }
    }

    BlockRange().InsertBefore(insertionPoint, new (comp, GT_PROF_HOOK) GenTree(GT_PROF_HOOK, TYP_VOID));
}

// Scan the range of nodes [rangeStart, rangeEnd) and update all references
// to the specified local to use a new temp instead. The temp is initialized
// with the original local's value before "insertTempBefore".
// It is assumed that the specified local is not accessed inside the range
// via an address that originated outside of the range.
void Lowering::RehomeParamForFastTailCall(LclVarDsc* paramLcl,
                                          GenTree*   insertTempBefore,
                                          GenTree*   rangeStart,
                                          GenTree*   rangeEnd)
{
    LclVarDsc* tmpLcl = nullptr;

    for (GenTree* node = rangeStart; node != rangeEnd; node = node->gtNext)
    {
        if (!node->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD, GT_LCL_STORE, GT_LCL_STORE_FLD, GT_LCL_ADDR))
        {
            continue;
        }

        if (node->AsLclRef()->GetLcl() != paramLcl)
        {
            continue;
        }

        if (tmpLcl == nullptr)
        {
            tmpLcl                    = comp->lvaAllocTemp(true DEBUGARG("fast tail call param temp"));
            tmpLcl->lvDoNotEnregister = paramLcl->lvDoNotEnregister;

            var_types type = varActualType(paramLcl->GetType());

            if (varTypeIsStruct(type))
            {
                comp->lvaSetStruct(tmpLcl, paramLcl->GetLayout(), /* checkUnsafeBuffer */ false);
            }
            else
            {
                tmpLcl->SetType(type);
            }

            GenTree* value = comp->gtNewLclLoad(paramLcl, type);

            if (type == TYP_STRUCT)
            {
                // TODO-MIKE-CQ: This code was previously using IND_STORE_BLK with a block layout.
                //
                // It's best to avoid using block layout when the struct layout is available (and
                // BLK/STORE_BLK) but doing so has a somewhat unfortunate side-effect: this copy
                // is done in a no-GC region but LCL_STORE doesn't know that and it will do its
                // normal GC copy thing. For unrolled copies it doesn't really matter, as the same
                // code is being generated in both cases, the only difference is that for large
                // copies we get "rep movsq" instead of a helper call.
                //
                // Perhaps there should be a way to tell LCL_STORE to ignore GC info in such
                // cases. But then there's no real need to put this copy in the no-GC region so
                // maybe it's best to leave LCL_STORE as is and insert the copy before the no-GC
                // region.
            }
            else
            {
                // TODO-MIKE-Review: This code came from gtNewTempAssign and it's not clear if it's
                // needed and if it's correct. Load "normalization" is done with casts, not by having
                // the LCL_LOAD node type set to the small int type of the local. If a cast already
                // exists doing this is pointless. If a cast does not exist then it means that morph
                // decided that it's not needed and changing the type here is also pointless.
                // Removing this results in one byte diff in corelib PMI diff due to a movzx being
                // changed to a mov. The movzx was indeed redundant.

                if (paramLcl->lvNormalizeOnLoad())
                {
                    value->SetType(paramLcl->GetType());
                }
            }

            GenTreeLclStore* store = comp->gtNewLclStore(tmpLcl, type, value);
            BlockRange().InsertBefore(insertTempBefore, value, store);

            if (type == TYP_STRUCT)
            {
                LowerNode(store);
            }
        }

        node->AsLclRef()->SetLcl(tmpLcl);
    }
}

#endif // FEATURE_FASTTAILCALL

#ifndef TARGET_64BIT
// Decomposes a LONG compare node.
// This is done during lowering because DecomposeLongs handles only nodes
// that produce LONG values. Compare nodes may consume LONG values but
// produce INT values.
GenTree* Lowering::DecomposeLongCompare(GenTreeOp* cmp)
{
    assert(cmp->GetOp(0)->TypeIs(TYP_LONG));

    GenTreeOp* src1 = cmp->GetOp(0)->AsOp();
    GenTreeOp* src2 = cmp->GetOp(1)->AsOp();
    assert(src1->OperIs(GT_LONG));
    assert(src2->OperIs(GT_LONG));
    GenTree* loSrc1 = src1->GetOp(0);
    GenTree* hiSrc1 = src1->GetOp(1);
    GenTree* loSrc2 = src2->GetOp(0);
    GenTree* hiSrc2 = src2->GetOp(1);
    BlockRange().Unlink(src1);
    BlockRange().Unlink(src2);

    genTreeOps condition = cmp->GetOper();
    GenTree*   loCmp;
    GenTree*   hiCmp;

    if (cmp->OperIs(GT_EQ, GT_NE))
    {
        // Transform (x EQ|NE y) into (((x.lo XOR y.lo) OR (x.hi XOR y.hi)) EQ|NE 0). If y is 0 then this can
        // be reduced to just ((x.lo OR x.hi) EQ|NE 0). The OR is expected to set the condition flags so we
        // don't need to generate a redundant compare against 0, we only generate a SETCC|JCC instruction.
        //
        // XOR is used rather than SUB because it is commutative and thus allows swapping the operands when
        // the first happens to be a constant. Usually only the second compare operand is a constant but it's
        // still possible to have a constant on the left side. For example, when src1 is a uint->ulong cast
        // then hiSrc1 would be 0.

        if (loSrc1->OperIs(GT_CNS_INT))
        {
            std::swap(loSrc1, loSrc2);
        }

        if (loSrc2->IsIntCon(0))
        {
            BlockRange().Unlink(loSrc2);
            loCmp = loSrc1;
        }
        else
        {
            loCmp = comp->gtNewOperNode(GT_XOR, TYP_INT, loSrc1, loSrc2);
            BlockRange().InsertBefore(cmp, loCmp);
            ContainCheckBinary(loCmp->AsOp());
        }

        if (hiSrc1->IsIntCon())
        {
            std::swap(hiSrc1, hiSrc2);
        }

        if (hiSrc2->IsIntCon(0))
        {
            BlockRange().Unlink(hiSrc2);
            hiCmp = hiSrc1;
        }
        else
        {
            hiCmp = comp->gtNewOperNode(GT_XOR, TYP_INT, hiSrc1, hiSrc2);
            BlockRange().InsertBefore(cmp, hiCmp);
            ContainCheckBinary(hiCmp->AsOp());
        }

        hiCmp = comp->gtNewOperNode(GT_OR, TYP_INT, loCmp, hiCmp);
        BlockRange().InsertBefore(cmp, hiCmp);
        ContainCheckBinary(hiCmp->AsOp());
    }
    else
    {
        assert(cmp->OperIs(GT_LT, GT_LE, GT_GE, GT_GT));

        // If the compare is signed then (x LT|GE y) can be transformed into ((x SUB y) LT|GE 0).
        // If the compare is unsigned we can still use SUB but we need to check the Carry flag,
        // not the actual result. In both cases we can simply check the appropiate condition flags
        // and ignore the actual result:
        //     SUB_LO loSrc1, loSrc2
        //     SUB_HI hiSrc1, hiSrc2
        //     SETCC|JCC (signed|unsigned LT|GE)
        // If loSrc2 happens to be 0 then the first SUB can be eliminated and the second one can
        // be turned into a CMP because the first SUB would have set carry to 0. This effectively
        // transforms a long compare against 0 into an int compare of the high part against 0.
        //
        // (x LE|GT y) can to be transformed into ((x SUB y) LE|GT 0) but checking that a long value
        // is greater than 0 is not so easy. We need to turn this into a positive/negative check
        // like the one we get for LT|GE compares, this can be achieved by swapping the compare:
        //     (x LE|GT y) becomes (y GE|LT x)
        //
        // Having to swap operands is problematic when the second operand is a constant. The constant
        // moves to the first operand where it cannot be contained and thus needs a register. This can
        // be avoided by changing the constant such that LE|GT becomes LT|GE:
        //     (x LE|GT 41) becomes (x LT|GE 42)

        if (cmp->OperIs(GT_LE, GT_GT))
        {
            bool mustSwap = true;

            if (loSrc2->IsIntCon() && hiSrc2->IsIntCon())
            {
                uint32_t loValue  = static_cast<uint32_t>(loSrc2->AsIntCon()->GetValue());
                uint32_t hiValue  = static_cast<uint32_t>(hiSrc2->AsIntCon()->GetValue());
                uint64_t value    = static_cast<uint64_t>(loValue) | (static_cast<uint64_t>(hiValue) << 32);
                uint64_t maxValue = cmp->IsRelopUnsigned() ? UINT64_MAX : INT64_MAX;

                if (value != maxValue)
                {
                    value++;
                    loValue = value & UINT32_MAX;
                    hiValue = (value >> 32) & UINT32_MAX;
                    loSrc2->AsIntCon()->SetValue(loValue);
                    hiSrc2->AsIntCon()->SetValue(hiValue);

                    condition = cmp->OperIs(GT_LE) ? GT_LT : GT_GE;
                    mustSwap  = false;
                }
            }

            if (mustSwap)
            {
                std::swap(loSrc1, loSrc2);
                std::swap(hiSrc1, hiSrc2);
                condition = GenTree::SwapRelop(condition);
            }
        }

        assert((condition == GT_LT) || (condition == GT_GE));

        if (loSrc2->IsIntCon(0))
        {
            BlockRange().Unlink(loSrc2);

            // Very conservative dead code removal... but it helps.

            if (loSrc1->OperIs(GT_CNS_INT, GT_LCL_LOAD, GT_LCL_LOAD_FLD))
            {
                BlockRange().Unlink(loSrc1);
            }
            else
            {
                loSrc1->SetUnusedValue();
            }

            hiCmp = comp->gtNewOperNode(GT_CMP, TYP_VOID, hiSrc1, hiSrc2);
            BlockRange().InsertBefore(cmp, hiCmp);
            ContainCheckCompare(hiCmp->AsOp());
        }
        else
        {
            loCmp = comp->gtNewOperNode(GT_CMP, TYP_VOID, loSrc1, loSrc2);
            hiCmp = comp->gtNewOperNode(GT_SUB_HI, TYP_INT, hiSrc1, hiSrc2);
            BlockRange().InsertBefore(cmp, loCmp, hiCmp);
            ContainCheckCompare(loCmp->AsOp());
            ContainCheckBinary(hiCmp->AsOp());

            // Try to move the first SUB_HI operands right in front of it, this allows using
            // a single temporary register instead of 2 (one for CMP and one for SUB_HI). Do
            // this only for locals as they won't change condition flags. Note that we could
            // move constants (except 0 which generates XOR reg, reg) but it's extremely rare
            // to have a constant as the first operand.

            if (hiSrc1->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD))
            {
                BlockRange().Unlink(hiSrc1);
                BlockRange().InsertBefore(hiCmp, hiSrc1);
            }
        }
    }

    hiCmp->AddImplicitFlagsDef();

    if (hiCmp->IsValue())
    {
        hiCmp->SetUnusedValue();
    }

    LIR::Use cmpUse;
    if (BlockRange().TryGetUse(cmp, &cmpUse) && cmpUse.User()->OperIs(GT_JTRUE))
    {
        BlockRange().Unlink(cmp);

        GenTree* jcc       = cmpUse.User();
        jcc->AsOp()->gtOp1 = nullptr;
        jcc->ChangeOper(GT_JCC);
        jcc->AddImplicitFlagsUse();
        jcc->AsCC()->SetCondition(GenCondition::FromIntegralRelop(condition, cmp->IsRelopUnsigned()));
    }
    else
    {
        GenCondition cond = GenCondition::FromIntegralRelop(condition, cmp->IsRelopUnsigned());

        cmp->AsOp()->gtOp1 = nullptr;
        cmp->AsOp()->gtOp2 = nullptr;
        cmp->ChangeOper(GT_SETCC);
        cmp->AddImplicitFlagsUse();
        cmp->AsCC()->SetCondition(cond);
    }

    return cmp->gtNext;
}
#endif // !TARGET_64BIT

void Lowering::LowerJmp(GenTreeJmp* jmp)
{
    // If PInvokes are inlined, we have to remember to execute PInvoke method
    // epilog anywhere that a method returns.
    if (comp->info.IsPInvokeFrameRequired())
    {
        InsertPInvokeMethodEpilog(INDEBUG(jmp));
    }

    CORINFO_CONST_LOOKUP entryPoint;
    comp->info.compCompHnd->getFunctionEntryPoint(jmp->GetMethodHandle(), &entryPoint);
    jmp->SetEntryPoint(entryPoint);
}

void Lowering::LowerReturn(GenTreeUnOp* ret)
{
    assert(ret->OperIs(GT_RETURN));

    JITDUMPLIRRANGE(BlockRange(), ret, "Lowering RETURN:\n");

    if (varTypeIsStruct(ret->GetType()))
    {
        LowerStructReturn(ret);
    }

    // Method doing PInvokes has exactly one return block unless it has tail calls.
    if (comp->info.IsPInvokeFrameRequired() && (m_block == comp->genReturnBB))
    {
        InsertPInvokeMethodEpilog(INDEBUG(ret));
    }

    if (!ret->TypeIs(TYP_VOID))
    {
        ContainCheckRet(ret);
    }
}

void Lowering::LowerLclLoad(GenTreeLclLoad* lclVar)
{
    assert(!lclVar->IsMultiReg());
    assert(!lclVar->GetLcl()->IsIndependentPromoted());

#ifdef FEATURE_SIMD
    if (lclVar->TypeIs(TYP_SIMD12))
    {
        WidenSIMD12IfNecessary(lclVar);
    }
#endif
}

void Lowering::LowerLclStore(GenTreeLclStore* store)
{
#ifdef FEATURE_SIMD
    if (store->TypeIs(TYP_SIMD12))
    {
        WidenSIMD12IfNecessary(store);
    }
#endif

    GenTree*   src = store->GetValue();
    LclVarDsc* lcl = store->GetLcl();

#if FEATURE_MULTIREG_RET
    if (src->IsMultiRegNode())
    {
        MakeMultiRegLclStore(store, src);
    }
#endif

    assert(!lcl->IsIndependentPromoted() || store->IsMultiReg());

    // TODO-MIKE-Cleanup: This code doesn't make any sense, it's most likely dead.
    if (!src->TypeIs(TYP_STRUCT) && (varTypeUsesFloatReg(store->GetType()) != varTypeUsesFloatReg(src->GetType())))
    {
        if (lcl->lvDoNotEnregister)
        {
            // This is an actual store, we'll just retype it.
            store->SetType(src->GetType());
        }
        else
        {
            GenTreeUnOp* bitcast = comp->gtNewBitCastNode(store->GetType(), src);
            store->SetValue(bitcast);
            BlockRange().InsertBefore(store, bitcast);
            LowerBitCast(bitcast);
            src = bitcast;
        }
    }

    if (store->TypeIs(TYP_STRUCT))
    {
        ClassLayout* layout = lcl->GetLayout();

        if (GenTreeCall* call = src->IsCall())
        {
            if (layout->GetSize() < call->GetRetLayout()->GetSize())
            {
                store->SetValue(SpillStructCall(call, store));
            }

            return;
        }

        LowerStructStore(store, GetStructStoreKind(true, layout, src), layout);
        return;
    }

    LowerLclStoreArch(store);
}

void Lowering::LowerLclLoadFld(GenTreeLclLoadFld* load)
{
    comp->lvaSetDoNotEnregister(load->GetLcl() DEBUG_ARG(Compiler::DNER_LocalField));
}

void Lowering::LowerLclStoreFld(GenTreeLclStoreFld* store)
{
    comp->lvaSetDoNotEnregister(store->GetLcl() DEBUG_ARG(Compiler::DNER_LocalField));

    GenTree* value = store->GetValue();

    if (value->OperIs(GT_BITCAST))
    {
        GenTree* src = value->AsUnOp()->GetOp(0);

        if (varTypeUsesFloatReg(src->GetType()) != varTypeUsesFloatReg(store->GetType())
#ifndef TARGET_64BIT
            && !src->TypeIs(TYP_LONG)
#endif
                )
        {
            assert(varTypeSize(src->GetType()) == varTypeSize(store->GetType()));

            src->ClearContained();
            src->ClearRegOptional();
            store->SetType(src->GetType());
            store->SetValue(src);

            BlockRange().Unlink(value);
            value = src;
        }
    }

    if (varTypeIsStruct(store->GetType()))
    {
        ClassLayout* layout = store->GetLayout(comp);

        if (GenTreeCall* call = value->IsCall())
        {
            unsigned size = varTypeIsSIMD(store->GetType()) ? varTypeSize(store->GetType()) : layout->GetSize();

            if ((call->GetRegCount() == 1) && (varTypeSize(call->GetRegType(0)) <= size))
            {
                call->SetType(call->GetRegType(0));
                store->SetType(call->GetType());

                return;
            }

            if ((call->GetRegCount() > 1) && varTypeIsSIMD(store->GetType()))
            {
                // TODO-MIKE-Cleanup: SIMD stores are a bit of a problem - sometimes the layout
                // is missing. It may be possible to get things to work without layout but that
                // would likely complicate the already complicated struct store handling even
                // more. We'll just use call's layout, provided that it has the same SIMD type.
                // It's unlikely to get type mismatches like SIMD16/SIMD12 in this case. If it
                // happens then just spill the call so we get a "pure" SIMD load/store.

                if (call->GetType() == store->GetType())
                {
                    layout = call->GetRetLayout();
                    store->SetLayout(layout, comp);
                    store->SetType(TYP_STRUCT);
                    call->SetType(TYP_STRUCT);
                }
                else
                {
                    size = 0;
                }
            }

            if (size < call->GetRetLayout()->GetSize())
            {
                store->SetValue(SpillStructCall(call, store));
            }

            return;
        }

        if (store->TypeIs(TYP_STRUCT))
        {
            ClassLayout*    layout = store->GetLayout(comp);
            StructStoreKind kind   = GetStructStoreKind(true, layout, value);
            LowerStructStore(store, kind, layout);

            return;
        }
    }

    assert(varTypeUsesFloatReg(store->GetType()) == varTypeUsesFloatReg(value->GetType()));

#ifdef TARGET_XARCH
    if (varTypeIsByte(store->GetType()) && (value->OperIsCompare() || value->OperIs(GT_SETCC)))
    {
        value->SetType(store->GetType());
    }
#endif

    ContainCheckStoreLcl(store);
}

void Lowering::LowerStructReturn(GenTreeUnOp* ret)
{
    assert(ret->OperIs(GT_RETURN) && varTypeIsStruct(ret->GetType()));

    GenTree* src = ret->GetOp(0);

    if (src->IsMultiRegCall())
    {
        return;
    }

    if (GenTreeFieldList* fieldList = src->IsFieldList())
    {
#ifdef FEATURE_HW_INTRINSICS
        for (GenTreeFieldList::Use& use : fieldList->Uses())
        {
            // Workaround poor register allocation on linux-x64 - if the returned value is already in XMM0
            // then attempting to extract its elements to XMM0 and XMM1 results in a spill to temp because
            // the first extract kills the value in XMM0, which is then needed again to extract to XMM1.
            // At this point we don't really care about the precise type - FLOAT/DOUBLE/SIMDn - we only care
            // that the value is in an XMM registers so we can get rid of the extract to XMM0.
            // This doesn't appear to be a problem on arm64 but that may simply be due to more registers
            // being available, otherwise there's nothing to suggest that arm64 doesn't have the same issue.

            if (GenTreeHWIntrinsic* extract = use.GetNode()->IsHWIntrinsic())
            {
                if ((extract->GetIntrinsic() == NI_VEC_EXTRACT) && extract->GetOp(1)->IsIntCon(0) &&
                    varTypeUsesFloatReg(extract->GetType()) && varTypeUsesFloatReg(extract->GetOp(0)->GetType()))
                {
                    GenTree* vec = extract->GetOp(0);
                    vec->ClearContained();
                    use.SetNode(vec);
                    BlockRange().Unlink(extract->GetOp(1));
                    BlockRange().Unlink(extract);
                }
            }
        }
#endif // FEATURE_HW_INTRINSICS

        return;
    }

    assert(comp->info.retDesc.GetRegCount() == 1);

#ifdef DEBUG
    if (!varTypeIsStruct(src->GetType()))
    {
        var_types retActualType = varActualType(comp->info.retDesc.GetRegType(0));
        var_types srcActualType = varActualType(src->GetType());

        bool constStructInit                  = src->IsIntCon(0);
        bool implicitCastFromSameOrBiggerSize = varTypeSize(retActualType) <= varTypeSize(srcActualType);

        // This could happen if we have retyped op1 as a primitive type during struct promotion.
        bool actualTypesMatch = (retActualType == srcActualType);

        assert(actualTypesMatch || constStructInit || implicitCastFromSameOrBiggerSize);
    }
#endif // DEBUG

    if (src->OperIs(GT_IND_LOAD, GT_IND_LOAD_OBJ))
    {
        var_types    retRegType = comp->info.retDesc.GetRegType(0);
        ClassLayout* retLayout  = comp->info.GetRetLayout();

        if (retLayout->GetSize() == varTypeSize(retRegType))
        {
            if (varTypeIsSmall(retRegType))
            {
                retRegType = varTypeToSmallUnsigned(retRegType);
            }

            src->ChangeOper(GT_IND_LOAD);
            src->SetType(retRegType);

            LowerIndir(src->AsIndLoad());
        }
        else
        {
#if defined(TARGET_X86) || defined(WINDOWS_AMD64_ABI)
            unreached();
#else
            assert(retLayout->GetSize() < varTypeSize(retRegType));

            LclVarDsc* tempLcl = comp->lvaNewTemp(retLayout, true DEBUGARG("indir ret temp"));
            comp->lvaSetDoNotEnregister(tempLcl DEBUGARG(Compiler::DNER_LocalField));

            GenTreeLclLoadFld* retRegValue = comp->gtNewLclLoadFld(retRegType, tempLcl, 0);
            ret->SetOp(0, retRegValue);
            BlockRange().InsertBefore(ret, retRegValue);

            GenTreeLclStore* tempStore = comp->gtNewLclStore(tempLcl, src->GetType(), src);
            BlockRange().InsertAfter(src, tempStore);

            src->ChangeOper(GT_IND_LOAD_OBJ);
            src->AsIndLoadObj()->SetLayout(retLayout);

            LowerLclStore(tempStore);
#endif
        }

        ret->SetType(varActualType(retRegType));

        return;
    }

    var_types retRegType = varActualType(comp->info.retDesc.GetRegType(0));
    ret->SetType(retRegType);

    switch (src->GetOper())
    {
        case GT_CALL:
            assert(src->TypeIs(retRegType)); // Type should be changed during call processing.
            break;

        case GT_LCL_LOAD:
            LowerRetSingleRegStructLclVar(ret);
            break;

        case GT_LCL_LOAD_FLD:
            assert(src->AsLclLoadFld()->GetLcl()->lvDoNotEnregister);
            src->SetType(retRegType);
            break;

        case GT_CNS_INT:
        case GT_CNS_DBL:
            unreached();

        default:
            assert(!src->TypeIs(TYP_STRUCT));

            if (varTypeUsesFloatReg(ret->GetType()) != varTypeUsesFloatReg(src->GetType()))
            {
                GenTreeUnOp* bitcast = comp->gtNewBitCastNode(ret->GetType(), src);
                ret->SetOp(0, bitcast);
                BlockRange().InsertBefore(ret, bitcast);
                LowerBitCast(bitcast);
            }
            break;
    }
}

//----------------------------------------------------------------------------------------------
// LowerRetSingleRegStructLclVar: Lowers a return node with a struct lclVar as a source.
//
// Notes:
//    - the function is only for LclVars that are returned in one register;
//    - if LclVar is allocated in memory then read it as return type;
//    - if LclVar can be enregistered read it as register type and add a bitcast if necessary;
//
void Lowering::LowerRetSingleRegStructLclVar(GenTreeUnOp* ret)
{
    assert(ret->OperIs(GT_RETURN));
    assert(comp->info.retDesc.GetRegCount() == 1);

    GenTreeLclLoad* load = ret->GetOp(0)->AsLclLoad();
    LclVarDsc*      lcl  = load->GetLcl();

    if (lcl->TypeIs(TYP_STRUCT))
    {
        // TODO-1stClassStructs: We can no longer independently promote
        // or enregister this struct, since it is referenced as a whole.
        comp->lvaSetDoNotEnregister(lcl DEBUGARG(Compiler::DNER_BlockOp));
    }

    if (lcl->lvDoNotEnregister)
    {
        load->ChangeToLclLoadFld(ret->GetType(), lcl, 0, FieldSeqStore::NotAField());
    }
    else
    {
        var_types regType = lcl->GetRegisterType(load);
        assert(regType != TYP_UNDEF);
        load->SetType(regType);

        if (varTypeUsesFloatReg(ret->GetType()) != varTypeUsesFloatReg(regType))
        {
            GenTreeUnOp* bitcast = comp->gtNewBitCastNode(ret->GetType(), load);
            ret->SetOp(0, bitcast);
            BlockRange().InsertBefore(ret, bitcast);
            LowerBitCast(bitcast);
        }
    }
}

void Lowering::LowerStructCall(GenTreeCall* call)
{
    assert(varTypeIsStruct(call->GetType()));

    if (call->GetRegCount() > 1)
    {
        return;
    }

    LIR::Use callUse;
    if (!BlockRange().TryGetUse(call, &callUse))
    {
        return;
    }

    GenTree*  user    = callUse.User();
    var_types regType = call->GetRegType(0);

    switch (user->GetOper())
    {
        case GT_RETURN:
            call->SetType(varActualType(regType));
            break;

        case GT_LCL_STORE:
        case GT_LCL_STORE_FLD:
        case GT_IND_STORE_OBJ:
            // Leave as is, the user will handle it.
            assert(user->TypeIs(call->GetType()) || varTypeIsSIMD(user->GetType()));
            break;

        case GT_IND_STORE:
            if (!varTypeIsSIMD(user->GetType()))
            {
                call->SetType(varActualType(regType));

                assert(user->TypeIs(TYP_REF) || (user->TypeIs(TYP_I_IMPL) && comp->IsTargetAbi(CORINFO_CORERT_ABI)));
                assert(call->IsHelperCall());
                assert(regType == user->GetType());
            }
            break;

        default:
            unreached();
    }
}

// Spill a call return value to a temp, to handle odd cases where the call return registers
// cannot be stored directly for various reasons - x86 multireg return that needs GC barriers,
// HFAs that somehow got truncated etc.
GenTree* Lowering::SpillStructCall(GenTreeCall* call, GenTree* user)
{
    LclVarDsc* lcl = comp->lvaNewTemp(call->GetRetLayout(), true DEBUGARG("odd struct call return temp"));

    GenTreeLclStore* store = comp->gtNewLclStore(lcl, lcl->GetType(), call);
    GenTreeLclLoad*  load  = comp->gtNewLclLoad(lcl, lcl->GetType());
    BlockRange().InsertAfter(call, store);
    BlockRange().InsertBefore(user, load);

    return load;
}

GenTree* Lowering::LowerIndirectVirtualStubCall(GenTreeCall* call)
{
    assert(call->IsVirtualStubIndirect() X86_ONLY(&&!call->IsTailCallViaJitHelper()));

    // The importer decided we needed a stub call via a computed
    // stub dispatch address, i.e. an address which came from a dictionary lookup.
    //   - The dictionary lookup produces an indirected address, suitable for call
    //     via "call [VirtualStubParam.reg]"
    //
    // This combination will only be generated for shared generic code and when
    // stub dispatch is active.

    // moSetupCallArgs will have created trees to pass the address in VirtualStubParam.reg.
    // All we have to do here is add an indirection to generate the actual call target.

    GenTreeIndLoad* ind = comp->gtNewIndLoad(TYP_I_IMPL, call->GetCallAddr());
    BlockRange().InsertAfter(call->GetCallAddr(), ind);
    return ind;
}

GenTree* Lowering::LowerDirectCall(GenTreeCall* call)
{
    assert(!call->IsIndirectCall() && !call->IsUnmanaged());

    // Don't support tail calling helper methods.
    // But we might encounter tail calls dispatched via JIT helper appear as a tail call to helper.
    noway_assert(!call->IsTailCall() X86_ONLY(|| call->IsTailCallViaJitHelper()) || call->IsUserCall());

    CORINFO_CONST_LOOKUP entryPoint;

#ifdef FEATURE_READYTORUN_COMPILER
    if (call->m_entryPointAddr != nullptr)
    {
        entryPoint = call->GetEntryPoint();
    }
    else
#endif
        if (CorInfoHelpFunc helper = call->IsHelperCall())
    {
        void* pAddr;
        entryPoint.addr = comp->info.compCompHnd->getHelperFtn(helper, &pAddr);

        if (entryPoint.addr != nullptr)
        {
            assert(pAddr == nullptr);

            entryPoint.accessType = IAT_VALUE;
        }
        else
        {
            entryPoint.accessType = IAT_PVALUE;
            entryPoint.addr       = pAddr;
        }
    }
    else
    {
        CORINFO_ACCESS_FLAGS accessFlags = CORINFO_ACCESS_ANY;

        if (!call->HasNullCheck())
        {
            accessFlags = static_cast<CORINFO_ACCESS_FLAGS>(accessFlags | CORINFO_ACCESS_NONNULL);
        }

        comp->info.compCompHnd->getFunctionEntryPoint(call->GetMethodHandle(), &entryPoint, accessFlags);
    }

    if ((entryPoint.accessType == IAT_VALUE) && IsCallTargetInRange(entryPoint.addr))
    {
        call->m_entryPointAccessType = IAT_VALUE;
        call->m_entryPointAddr       = entryPoint.addr;

        return nullptr;
    }

#if defined(FEATURE_READYTORUN_COMPILER) && defined(TARGET_ARMARCH)
    // Skip inserting the indirection node to load the address that is already
    // computed in REG_R2R_INDIRECT_PARAM as a hidden parameter. Instead during
    // codegen, just load the call target from REG_R2R_INDIRECT_PARAM.
    if ((entryPoint.accessType == IAT_PVALUE) && call->IsR2RRelativeIndir())
    {
        return nullptr;
    }
#endif

    return ExpandConstLookupCallTarget(entryPoint, call DEBUGARG(call));
}

GenTree* Lowering::LowerDirectUnmanagedCall(GenTreeCall* call)
{
    assert(call->IsUserCall());

    CORINFO_CONST_LOOKUP entryPoint;
    comp->info.compCompHnd->getAddressOfPInvokeTarget(call->GetMethodHandle(), &entryPoint);

    // IsCallTargetInRange always return true on x64. It wants to use rip-based addressing for
    // this call. Unfortunately, in case of PInvokes (and SuppressGCTransition) to external libs
    // (e.g. kernel32.dll) the relative offset is unlikely to fit into disp32 and we will have
    // to turn fAllowRel32 off globally.
    // TODO-MIKE-Review: Does this apply to x86?
    if ((entryPoint.accessType == IAT_VALUE) && IsCallTargetInRange(entryPoint.addr) &&
        (!call->IsSuppressGCTransition() || comp->opts.IsJitFlagSet(JitFlags::JIT_FLAG_PREJIT)))
    {
        call->m_entryPointAccessType = IAT_VALUE;
        call->m_entryPointAddr       = entryPoint.addr;

        return nullptr;
    }

    return ExpandConstLookupCallTarget(entryPoint, call DEBUGARG(call));
}

GenTree* Lowering::ExpandConstLookupCallTarget(const CORINFO_CONST_LOOKUP& entryPoint,
                                               GenTree* insertBefore DEBUGARG(GenTreeCall* call))
{
    GenTreeIntCon* addr = comp->gtNewIconHandleNode(entryPoint.addr, HandleKind::MethodAddr);
    INDEBUG(addr->SetDumpHandle(call->GetMethodHandle()));
    BlockRange().InsertBefore(insertBefore, addr);

    if (entryPoint.accessType == IAT_VALUE)
    {
        return addr;
    }

    GenTreeIndLoad* load = comp->gtNewIndLoad(TYP_I_IMPL, addr);
    BlockRange().InsertBefore(insertBefore, load);
    ContainCheckIndir(load);

    if (entryPoint.accessType == IAT_PVALUE)
    {
        return load;
    }

    if (entryPoint.accessType == IAT_PPVALUE)
    {
        // TODO-CQ: Expanding earlier would allow CSEing of the first load which is invariant.
        load = comp->gtNewIndLoad(TYP_I_IMPL, load);
        BlockRange().InsertBefore(insertBefore, load);
        ContainCheckIndir(load);

        return load;
    }

    noway_assert(entryPoint.accessType == IAT_RELPVALUE);

    addr            = comp->gtNewIconHandleNode(entryPoint.addr, HandleKind::MethodAddr);
    GenTree* target = comp->gtNewOperNode(GT_ADD, TYP_I_IMPL, load, addr);
    BlockRange().InsertBefore(insertBefore, addr, target);
    ContainCheckBinary(target->AsOp());

    return target;
}

GenTree* Lowering::LowerDelegateInvoke(GenTreeCall* call)
{
    assert(call->IsUserCall() && call->IsDelegateInvoke());
    assert((comp->info.compCompHnd->getMethodAttribs(call->GetMethodHandle()) &
            (CORINFO_FLG_DELEGATE_INVOKE | CORINFO_FLG_FINAL)) == (CORINFO_FLG_DELEGATE_INVOKE | CORINFO_FLG_FINAL));
#ifdef TARGET_X86
    assert(!call->IsTailCallViaJitHelper());
#endif

    call->gtFlags &= ~GTF_CALL_DELEGATE_INV;

    CallArgInfo* thisArg = call->GetArgInfoByArgNum(0);

    GenTree* delegateThis = thisArg->GetNode();
    assert(delegateThis->TypeIs(TYP_REF));

    LclVarDsc* lcl = comp->lvaNewTemp(TYP_REF, true DEBUGARG("delegate invoke this"));

    LIR::Use use(BlockRange(), &thisArg->GetUse()->NodeRef(), call);
    delegateThis = ReplaceWithLclLoad(use, lcl);

    const CORINFO_EE_INFO* eeInfo = comp->eeGetEEInfo();

    GenTree*        targetThisAddr = comp->gtNewAddrMode(delegateThis, eeInfo->offsetOfDelegateInstance);
    GenTreeIndLoad* targetThis     = comp->gtNewIndLoad(TYP_REF, targetThisAddr);
    BlockRange().InsertAfter(delegateThis, targetThisAddr, targetThis);
    thisArg->SetNode(targetThis);
    ContainCheckIndir(targetThis);

    delegateThis                = comp->gtNewLclLoad(lcl, TYP_REF);
    GenTreeAddrMode* targetAddr = comp->gtNewAddrMode(delegateThis, eeInfo->offsetOfDelegateFirstTarget);
    GenTreeIndLoad*  target     = comp->gtNewIndLoad(TYP_I_IMPL, targetAddr);
    BlockRange().InsertBefore(call, delegateThis, targetAddr, target);
    ContainCheckIndir(target);

    return target;
}

GenTree* Lowering::LowerVirtualVtableCall(GenTreeCall* call)
{
    assert(call->IsUserCall());
    assert(!call->IsExpandedEarly() && (call->GetCallAddr() == nullptr));

    // Get hold of the vtable offset (note: this might be expensive)
    unsigned vtabOffsOfIndirection;
    unsigned vtabOffsAfterIndirection;
    bool     isRelative;
    comp->info.compCompHnd->getMethodVTableOffset(call->GetMethodHandle(), &vtabOffsOfIndirection,
                                                  &vtabOffsAfterIndirection, &isRelative);

    CallArgInfo* thisArgInfo = call->GetArgInfoByArgNum(0);
    assert(thisArgInfo->GetRegNum() == REG_ARG_0);
    GenTree* thisPtr = thisArgInfo->GetNode();

    GenTree* thisUse;

    if (thisPtr->OperIs(GT_LCL_LOAD))
    {
        thisUse = comp->gtNewLclLoad(thisPtr->AsLclLoad()->GetLcl(), thisPtr->GetType());
    }
    else if (thisPtr->OperIs(GT_LCL_LOAD_FLD))
    {
        thisUse = comp->gtNewLclLoadFld(thisPtr->GetType(), thisPtr->AsLclLoadFld()->GetLcl(),
                                        thisPtr->AsLclLoadFld()->GetLclOffs());
    }
    else
    {
        if (vtableCallTempLcl == nullptr)
        {
            vtableCallTempLcl = comp->lvaAllocTemp(true DEBUGARG("virtual vtable call"));
        }

        LIR::Use thisPtrUse(BlockRange(), &thisArgInfo->GetUse()->NodeRef(), call);
        ReplaceWithLclLoad(thisPtrUse, vtableCallTempLcl);
        thisUse = comp->gtNewLclLoad(vtableCallTempLcl, thisPtr->GetType());
    }

    GenTree* mtAddr = comp->gtNewAddrMode(thisUse, VPTR_OFFS);
    GenTree* mt     = comp->gtNewIndLoad(TYP_I_IMPL, mtAddr);
    BlockRange().InsertBefore(call, thisUse, mtAddr, mt);
    ContainCheckIndir(mt->AsIndLoad());

    // TODO-MIKE-Cleanup: This is dead code.
    if (isRelative)
    {
        assert(vtabOffsOfIndirection != CORINFO_VIRTUALCALL_NO_CHUNK);

        LclVarDsc*       mtTempLcl   = comp->lvaNewTemp(TYP_I_IMPL, true DEBUGARG("vtbl call MT"));
        GenTreeLclStore* mtTempStore = comp->gtNewLclStore(mtTempLcl, TYP_I_IMPL, mt);
        BlockRange().InsertBefore(call, mtTempStore);

        GenTree* mtTempUse1    = comp->gtNewLclLoad(mtTempLcl, TYP_I_IMPL);
        GenTree* chunkOffsAddr = comp->gtNewAddrMode(mtTempUse1, vtabOffsOfIndirection);
        GenTree* chunkOffs     = comp->gtNewIndLoad(TYP_I_IMPL, chunkOffsAddr);
        BlockRange().InsertBefore(call, mtTempUse1, chunkOffsAddr, chunkOffs);
        ContainCheckIndir(chunkOffs->AsIndLoad());

        GenTree* mtTempUse2    = comp->gtNewLclLoad(mtTempLcl, TYP_I_IMPL);
        GenTree* offs          = comp->gtNewIconNode(vtabOffsOfIndirection + vtabOffsAfterIndirection, TYP_I_IMPL);
        GenTree* chunkBaseAddr = comp->gtNewOperNode(GT_ADD, TYP_I_IMPL, mtTempUse2, offs);
        GenTree* slotAddr      = comp->gtNewAddrMode(TYP_I_IMPL, chunkBaseAddr, chunkOffs, 1, 0);
        BlockRange().InsertBefore(call, mtTempUse2, offs, chunkBaseAddr, slotAddr);

        LclVarDsc*       slotAddrTempLcl   = comp->lvaNewTemp(TYP_I_IMPL, true DEBUGARG("vtbl call slot addr"));
        GenTreeLclStore* slotAddrTempStore = comp->gtNewLclStore(slotAddrTempLcl, TYP_I_IMPL, slotAddr);
        BlockRange().InsertBefore(call, slotAddrTempStore);

        GenTree* slotAddrTempUse1 = comp->gtNewLclLoad(slotAddrTempLcl, TYP_I_IMPL);
        GenTree* codeOffs         = comp->gtNewIndLoad(TYP_I_IMPL, slotAddrTempUse1);
        GenTree* slotAddrTempUse2 = comp->gtNewLclLoad(slotAddrTempLcl, TYP_I_IMPL);
        GenTree* target           = comp->gtNewOperNode(GT_ADD, TYP_I_IMPL, codeOffs, slotAddrTempUse2);
        BlockRange().InsertBefore(call, slotAddrTempUse1, codeOffs, slotAddrTempUse2, target);
        ContainCheckIndir(codeOffs->AsIndLoad());

        return target;
    }

    GenTree* chunkAddr;

    if (vtabOffsOfIndirection == CORINFO_VIRTUALCALL_NO_CHUNK)
    {
        chunkAddr = mt;
    }
    else
    {
        GenTree* chunkAddrAddr = comp->gtNewAddrMode(mt, vtabOffsOfIndirection);
        chunkAddr              = comp->gtNewIndLoad(TYP_I_IMPL, chunkAddrAddr);
        BlockRange().InsertBefore(call, chunkAddrAddr, chunkAddr);
        ContainCheckIndir(chunkAddr->AsIndLoad());
    }

    GenTree*        slotAddr = comp->gtNewAddrMode(chunkAddr, vtabOffsAfterIndirection);
    GenTreeIndLoad* target   = comp->gtNewIndLoad(TYP_I_IMPL, slotAddr);
    BlockRange().InsertBefore(call, slotAddr, target);
    ContainCheckIndir(target);

    return target;
}

GenTree* Lowering::LowerVirtualStubCall(GenTreeCall* call)
{
    assert(call->IsVirtualStubDirect() X86_ONLY(&&!call->IsTailCallViaJitHelper()));

    // An x86 JIT which uses full stub dispatch must generate only
    // the following stub dispatch calls:
    //
    // (1) isCallRelativeIndirect:
    //        call dword ptr [rel32]  ;  FF 15 ---rel32----
    // (2) isCallRelative:
    //        call abc                ;     E8 ---rel32----
    // (3) isCallRegisterIndirect:
    //     3-byte nop                 ;
    //     call dword ptr [eax]       ;     FF 10
    //
    // THIS IS VERY TIGHTLY TIED TO THE PREDICATES IN
    // vm\i386\cGenCpu.h, esp. isCallRegisterIndirect.

    noway_assert(call->m_entryPointAddr != nullptr);
    // If not indirect, then it should always be relative indir call. This is ensured by VM.
    noway_assert(call->m_entryPointAccessType == IAT_PVALUE);

#if defined(FEATURE_READYTORUN_COMPILER) && defined(TARGET_ARMARCH)
    // Skip inserting the indirection node to load the address that is already
    // computed in REG_R2R_INDIRECT_PARAM as a hidden parameter. Instead during the
    // codegen, just load the call target from REG_R2R_INDIRECT_PARAM.
    // However, for tail calls, the call target is always computed in RBM_FASTTAILCALL_TARGET
    // and so do not optimize virtual stub calls for such cases.
    if (!call->IsTailCall())
    {
        return nullptr;
    }
#endif

    // TODO-Cleanup: start emitting random NOPS

    GenTreeIntCon*  addr   = comp->gtNewIconHandleNode(call->m_entryPointAddr, HandleKind::MethodAddr);
    GenTreeIndLoad* target = comp->gtNewIndLoad(TYP_I_IMPL, addr);
    BlockRange().InsertBefore(call, addr, target);
    ContainCheckIndir(target);

    return target;
}

// Create code to perform a "return trap", used in PInvoke epilogs to invoke a GC
// under a condition. The return trap checks some global location (the runtime tells
// us where that is and how many indirections to make), then, based on the result,
// conditionally calls a GC helper. We use a special node for this because at this
// time (late in the compilation phases), introducing flow is tedious/difficult.
// This is used for PInvoke inlining.
void Lowering::InsertReturnTrap(GenTree* before)
{
    // The GT_RETURNTRAP node expands to this:
    //    if (g_TrapReturningThreads)
    //    {
    //       RareDisablePreemptiveGC();
    //    }

    // The only thing to do here is build up the expression that evaluates 'g_TrapReturningThreads'.

    void*    pAddrOfCaptureThreadGlobal = nullptr;
    int32_t* addrOfCaptureThreadGlobal =
        comp->info.compCompHnd->getAddrOfCaptureThreadGlobal(&pAddrOfCaptureThreadGlobal);

    GenTree* trapAddr;

    if (addrOfCaptureThreadGlobal != nullptr)
    {
        trapAddr = comp->gtNewIconHandleNode(addrOfCaptureThreadGlobal, HandleKind::MethodAddr);
    }
    else
    {
        GenTree* trapAddrAddr = comp->gtNewIconHandleNode(pAddrOfCaptureThreadGlobal, HandleKind::MethodAddr);
        BlockRange().InsertBefore(before, trapAddrAddr);
        trapAddr = comp->gtNewIndLoad(TYP_I_IMPL, trapAddrAddr);
    }

    GenTree* trapValue = comp->gtNewIndLoad(TYP_INT, trapAddr);
    GenTree* trap      = comp->gtNewOperNode(GT_RETURNTRAP, TYP_INT, trapValue);

    BlockRange().InsertBefore(before, trapAddr, trapValue, trap);

    ContainCheckReturnTrap(trap->AsOp());
}

// Create code that stores the given constant (0 or 1) into the thread's GC state field.
// This is used for PInvoke inlining.
void Lowering::InsertSetGCState(GenTree* before, int state)
{
    assert((state == 0) || (state == 1));

    const CORINFO_EE_INFO& info = *comp->eeGetEEInfo();

    LclVarDsc* pInvokeFrameListLcl = comp->lvaGetDesc(comp->lvaPInvokeFrameListVar);

    GenTreeLclLoad*  base      = comp->gtNewLclLoad(pInvokeFrameListLcl, TYP_I_IMPL);
    GenTreeAddrMode* addr      = comp->gtNewAddrMode(base, info.offsetOfGCState);
    GenTreeIntCon*   stateNode = comp->gtNewIconNode(state);
    GenTreeIndStore* store     = comp->gtNewIndStore(TYP_BYTE, addr, stateNode);

    BlockRange().InsertBefore(before, base, addr, stateNode, store);

    ContainCheckIndStore(store);
}

// Create code that either links or unlinks the locally-allocated
// InlinedCallFrame from the Frame list.
void Lowering::InsertFrameLinkUpdate(LIR::Range& block, GenTree* before, FrameLinkAction action)
{
    const CORINFO_EE_INFO& info = *comp->eeGetEEInfo();

    LclVarDsc* pInvokeFrameLcl     = comp->lvaGetDesc(comp->lvaInlinedPInvokeFrameVar);
    LclVarDsc* pInvokeFrameListLcl = comp->lvaGetDesc(comp->lvaPInvokeFrameListVar);

    GenTree* tcb  = comp->gtNewLclLoad(pInvokeFrameListLcl, TYP_I_IMPL);
    GenTree* addr = comp->gtNewAddrMode(tcb, info.offsetOfThreadFrame);
    GenTree* data = nullptr;

    if (action == PushFrame)
    {
        data = comp->gtNewLclAddr(pInvokeFrameLcl, info.inlinedCallFrameInfo.offsetOfFrameVptr,
                                  FieldSeqStore::NotAField());
        comp->lvaSetAddressExposed(pInvokeFrameLcl);
    }
    else
    {
        assert(action == PopFrame);

        data = comp->gtNewLclLoadFld(TYP_BYREF, pInvokeFrameLcl, info.inlinedCallFrameInfo.offsetOfFrameLink);
    }

    GenTreeIndStore* store = comp->gtNewIndStore(TYP_I_IMPL, addr, data);
    block.InsertBefore(before, tcb, addr, data, store);
    ContainCheckIndStore(store);
}

// Create the code that runs at the start of every method that has PInvoke calls.
//
// Initialize the TCB local and the InlinedCallFrame object. Then link ("push")
// the InlinedCallFrame object on the Frame chain. The layout of InlinedCallFrame
// is defined in vm/frames.h. See also vm/jitinterface.cpp for more information.
// The offsets of these fields is returned by the VM in a call to ICorStaticInfo::getEEInfo().
//
// The (current) layout is as follows:
//
//  64-bit  32-bit                                    CORINFO_EE_INFO
//  offset  offset  field name                        offset                  when set
//  -----------------------------------------------------------------------------------------
//  +00h    +00h    GS cookie                         offsetOfGSCookie
//  +08h    +04h    vptr for class InlinedCallFrame   offsetOfFrameVptr       method prolog
//  +10h    +08h    m_Next                            offsetOfFrameLink       method prolog
//  +18h    +0Ch    m_Datum                           offsetOfCallTarget      call site
//  +20h    n/a     m_StubSecretArg                                           not set by JIT
//  +28h    +10h    m_pCallSiteSP                     offsetOfCallSiteSP      x86: call site, and zeroed in method
//                                                                              prolog;
//                                                                            non-x86: method prolog (SP remains
//                                                                              constant in function, after prolog: no
//                                                                              localloc and PInvoke in same function)
//  +30h    +14h    m_pCallerReturnAddress            offsetOfReturnAddress   call site
//  +38h    +18h    m_pCalleeSavedFP                  offsetOfCalleeSavedFP   not set by JIT
//          +1Ch    m_pThread
//          +20h    m_pSPAfterProlog                  offsetOfSPAfterProlog   arm only
//          +20/24h JIT retval spill area (int)                               before call_gc    ???
//          +24/28h JIT retval spill area (long)                              before call_gc    ???
//          +28/2Ch Saved value of EBP                                        method prolog     ???
//
// Note that in the VM, InlinedCallFrame is a C++ class whose objects have a 'this' pointer that points
// to the InlinedCallFrame vptr (the 2nd field listed above), and the GS cookie is stored *before*
// the object. When we link the InlinedCallFrame onto the Frame chain, we must point at this location,
// and not at the beginning of the InlinedCallFrame local, which is actually the GS cookie.
//
// Return Value:
//    none
//
void Lowering::InsertPInvokeMethodProlog()
{
    noway_assert(comp->info.compUnmanagedCallCountWithGCTransition);
    noway_assert(comp->lvaInlinedPInvokeFrameVar != BAD_VAR_NUM);

    if (comp->opts.ShouldUsePInvokeHelpers())
    {
        return;
    }

    LclVarDsc* pInvokeFrameLcl     = comp->lvaGetDesc(comp->lvaInlinedPInvokeFrameVar);
    LclVarDsc* pInvokeFrameListLcl = comp->lvaGetDesc(comp->lvaPInvokeFrameListVar);

    JITDUMP("======= Inserting PInvoke method prolog\n");

    // The first BB must be a scratch BB in order for us to be able to safely insert the P/Invoke prolog.
    assert(comp->fgFirstBBisScratch());

    LIR::Range& firstBlockRange = LIR::AsRange(comp->fgFirstBB);

    const CORINFO_EE_INFO*                       pInfo         = comp->eeGetEEInfo();
    const CORINFO_EE_INFO::InlinedCallFrameInfo& callFrameInfo = pInfo->inlinedCallFrameInfo;

    // First arg:  &compiler->lvaInlinedPInvokeFrameVar + callFrameInfo.offsetOfFrameVptr

    GenTree* frameAddr = comp->gtNewLclAddr(pInvokeFrameLcl, callFrameInfo.offsetOfFrameVptr, nullptr);
    comp->lvaSetAddressExposed(pInvokeFrameLcl);

    // Call runtime helper to fill in our InlinedCallFrame and push it on the Frame list:
    //     TCB = CORINFO_HELP_INIT_PINVOKE_FRAME(&symFrameStart, secretArg);

    GenTreeCall::Use* argList = comp->gtNewCallArgs(frameAddr);

#if !defined(TARGET_X86) && !defined(TARGET_ARM)
    if (comp->info.compPublishStubParam)
    {
        comp->gtInsertNewCallArgAfter(comp->gtNewRegUseNode(REG_SECRET_STUB_PARAM), argList);
    }
    else
    {
        comp->gtInsertNewCallArgAfter(comp->gtNewIconNode(0, TYP_I_IMPL), argList);
    }
#endif

    GenTree* insertionPoint = firstBlockRange.FirstNonCatchArgNode();

    GenTreeCall* pInvokeInitFrame = comp->gtNewHelperCallNode(CORINFO_HELP_INIT_PINVOKE_FRAME, TYP_I_IMPL, argList);
    LIR::InsertHelperCallBefore(comp, firstBlockRange, insertionPoint, pInvokeInitFrame);
    GenTreeLclStore* store = comp->gtNewLclStore(pInvokeFrameListLcl, TYP_I_IMPL, pInvokeInitFrame);
    firstBlockRange.InsertBefore(insertionPoint, store);

#if !defined(TARGET_X86) && !defined(TARGET_ARM)
    // For x86, this step is done at the call site (due to stack pointer not being static in the function).
    // For arm32, CallSiteSP is set up by the call to CORINFO_HELP_INIT_PINVOKE_FRAME.
    GenTreeRegUse* sp      = comp->gtNewRegUseNode(REG_SPBASE);
    GenTree*       storeSP = comp->gtNewLclStoreFld(TYP_I_IMPL, pInvokeFrameLcl, callFrameInfo.offsetOfCallSiteSP, sp);
    firstBlockRange.InsertBefore(insertionPoint, sp, storeSP);
    AMD64_ONLY(sp->SetContained());
#endif

#ifndef TARGET_ARM
    // For arm32, CalleeSavedFP is set up by the call to CORINFO_HELP_INIT_PINVOKE_FRAME.
    GenTreeRegUse* fp = comp->gtNewRegUseNode(REG_FPBASE);
    GenTree* storeFP  = comp->gtNewLclStoreFld(TYP_I_IMPL, pInvokeFrameLcl, callFrameInfo.offsetOfCalleeSavedFP, fp);
    firstBlockRange.InsertBefore(insertionPoint, fp, storeFP);
    fp->SetContained();
#endif

#ifdef TARGET_64BIT
    // On 32-bit targets, CORINFO_HELP_INIT_PINVOKE_FRAME initializes the PInvoke frame and then pushes it
    // onto the current thread's Frame stack. On 64-bit targets, it only initializes the PInvoke frame.
    if (comp->opts.IsJitFlagSet(JitFlags::JIT_FLAG_IL_STUB))
    {
        // Push a frame - if we are NOT in an IL stub, this is done right before the call
        // The init routine sets InlinedCallFrame's m_pNext, so we just set the thread's top-of-stack
        InsertFrameLinkUpdate(firstBlockRange, insertionPoint, PushFrame);
    }
#endif
}

void Lowering::InsertPInvokeMethodEpilog(INDEBUG(GenTree* lastNode))
{
    assert(comp->info.compUnmanagedCallCountWithGCTransition);

    if (comp->opts.ShouldUsePInvokeHelpers())
    {
        return;
    }

    JITDUMP("======= Inserting PInvoke method epilog\n");

    // Method doing PInvoke calls has exactly one return block unless it has "jmp" or tail calls.
    assert(((m_block == comp->genReturnBB) && m_block->KindIs(BBJ_RETURN)) || m_block->EndsWithJmp(comp) ||
           m_block->EndsWithTailCall(comp));

    GenTree* insertionPoint = BlockRange().LastNode();
    assert(insertionPoint == lastNode);

    // Pop the frame if necessary. This always happens in the epilog on 32-bit targets. For 64-bit targets, we
    // only do this in the epilog for IL stubs; for non-IL stubs the frame is popped after every PInvoke call.
    CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef TARGET_64BIT
    if (comp->opts.IsJitFlagSet(JitFlags::JIT_FLAG_IL_STUB))
#endif
    {
        InsertFrameLinkUpdate(BlockRange(), insertionPoint, PopFrame);
    }
}

// Emit the call-site prolog for calls to unmanaged code.
// It does all the necessary call-site setup of the InlinedCallFrame.
void Lowering::InsertUnmanagedCallProlog(GenTreeCall* call)
{
    JITDUMP("======= Inserting unmanaged call prolog\n");

    GenTree* insertBefore = call->IsIndirectCall() ? call->GetCallAddr() : call;

    const CORINFO_EE_INFO::InlinedCallFrameInfo& callFrameInfo = comp->eeGetEEInfo()->inlinedCallFrameInfo;

    LclVarDsc* pInvokeFrameLcl = comp->lvaGetDesc(comp->lvaInlinedPInvokeFrameVar);

    noway_assert(comp->lvaInlinedPInvokeFrameVar != BAD_VAR_NUM);

    if (comp->opts.ShouldUsePInvokeHelpers())
    {
        // First argument is the address of the frame variable.
        GenTree* frameAddr = comp->gtNewLclAddr(pInvokeFrameLcl);
        comp->lvaSetAddressExposed(pInvokeFrameLcl);

#if defined(TARGET_X86) && defined(TARGET_WINDOWS)
        // On x86 targets, PInvoke calls need the size of the stack args in InlinedCallFrame.m_Datum.
        // This is because the callee pops stack arguments, and we need to keep track of this during stack
        // walking
        const unsigned    numStkArgBytes = call->GetInfo()->GetStackArgsSize();
        GenTree*          stackBytes     = comp->gtNewIconNode(numStkArgBytes, TYP_INT);
        GenTreeCall::Use* args           = comp->gtNewCallArgs(frameAddr, stackBytes);
#else
        GenTreeCall::Use* args = comp->gtNewCallArgs(frameAddr);
#endif
        // Note that this is a special helper function that's a leaf and does not
        // modify its parameter area, so it can be nested within this call site.
        GenTreeCall* pInvokeBegin = comp->gtNewHelperCallNode(CORINFO_HELP_JIT_PINVOKE_BEGIN, TYP_VOID, args);
        LIR::InsertHelperCallBefore(comp, BlockRange(), insertBefore, pInvokeBegin);
        LowerCall(pInvokeBegin);

        return;
    }

    // Emit the following sequence:
    //
    // InlinedCallFrame.callTarget = methodHandle   // stored in m_Datum
    // InlinedCallFrame.m_pCallSiteSP = SP          // x86 only
    // InlinedCallFrame.m_pCallerReturnAddress = return address
    // GT_START_PREEEMPTC
    // Thread.gcState = 0
    // (non-stub) - update top Frame on TCB         // 64-bit targets only

    // ----------------------------------------------------------------------------------
    // Setup InlinedCallFrame.callSiteTarget (which is how the JIT refers to it).
    // The actual field is InlinedCallFrame.m_Datum which has many different uses and meanings.

    GenTree* src = nullptr;

    if (call->IsIndirectCall())
    {
#ifndef TARGET_64BIT
        // On 32-bit targets, indirect calls need the size of the stack args in InlinedCallFrame.m_Datum.
        const unsigned numStkArgBytes = call->GetInfo()->GetStackArgsSize();

        src = comp->gtNewIconNode(numStkArgBytes, TYP_INT);
#else
        // On 64-bit targets, indirect calls may need the stub parameter value in InlinedCallFrame.m_Datum.
        // If the stub parameter value is not needed, m_Datum will be initialized by the VM.
        if (comp->info.compPublishStubParam)
        {
            src = comp->gtNewLclLoad(comp->lvaGetDesc(comp->lvaStubArgumentVar), TYP_I_IMPL);
        }
#endif
    }
    else
    {
        assert(call->IsUserCall());

        void*                 pEmbedMethodHandle = nullptr;
        CORINFO_METHOD_HANDLE embedMethodHandle =
            comp->info.compCompHnd->embedMethodHandle(call->GetMethodHandle(), &pEmbedMethodHandle);

        noway_assert((embedMethodHandle == nullptr) != (pEmbedMethodHandle == nullptr));

        if (embedMethodHandle != nullptr)
        {
            src = comp->gtNewIconHandleNode(embedMethodHandle, HandleKind::MethodAddr);
        }
        else
        {
            GenTree* srcAddr = comp->gtNewIconHandleNode(pEmbedMethodHandle, HandleKind::MethodAddr);
            BlockRange().InsertBefore(src, srcAddr);
            src = comp->gtNewIndLoad(TYP_I_IMPL, srcAddr);
        }
    }

    if (src != nullptr)
    {
        // Store into InlinedCallFrame.m_Datum, the offset of which is given by offsetOfCallTarget.
        GenTreeLclStoreFld* store =
            comp->gtNewLclStoreFld(TYP_I_IMPL, pInvokeFrameLcl, callFrameInfo.offsetOfCallTarget, src);
        BlockRange().InsertBefore(insertBefore, src, store);
        ContainCheckStoreLcl(store);
    }

#ifdef TARGET_X86
    GenTreeRegUse*      callSiteSP = comp->gtNewRegUseNode(REG_SPBASE);
    GenTreeLclStoreFld* storeCallSiteSP =
        comp->gtNewLclStoreFld(TYP_INT, pInvokeFrameLcl, callFrameInfo.offsetOfCallSiteSP, callSiteSP);
    BlockRange().InsertBefore(insertBefore, callSiteSP, storeCallSiteSP);
    callSiteSP->SetContained();
#endif

    // ----------------------------------------------------------------------------------
    // InlinedCallFrame.m_pCallerReturnAddress = &label (the address of the instruction immediately following the call)

    GenTree* returnLabel = new (comp, GT_LABEL) GenTree(GT_LABEL, TYP_I_IMPL);
    GenTree* storeReturnLabel =
        comp->gtNewLclStoreFld(TYP_I_IMPL, pInvokeFrameLcl, callFrameInfo.offsetOfReturnAddress, returnLabel);
    BlockRange().InsertBefore(insertBefore, returnLabel, storeReturnLabel);

    // Push the PInvoke frame if necessary. On 32-bit targets this only happens in the method prolog if a method
    // contains PInvokes; on 64-bit targets this is necessary in non-stubs.
    CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef TARGET_64BIT
    if (!comp->opts.IsJitFlagSet(JitFlags::JIT_FLAG_IL_STUB))
    {
        // Set the TCB's frame to be the one we just created.
        // Note the init routine for the InlinedCallFrame (CORINFO_HELP_INIT_PINVOKE_FRAME)
        // has prepended it to the linked list to maintain the stack of Frames.
        //
        // Stubs do this once per stub, not once per call.
        InsertFrameLinkUpdate(BlockRange(), insertBefore, PushFrame);
    }
#endif // TARGET_64BIT

    // IMPORTANT **** This instruction must be the last real instruction ****
    // It changes the thread's state to Preemptive mode
    // ----------------------------------------------------------------------------------
    //  [tcb + offsetOfGcState] = 0
    InsertSetGCState(insertBefore, 0);

    // Indicate that codegen has switched this thread to preemptive GC.
    // This tree node doesn't generate any code, but impacts LSRA and gc reporting.
    // This tree node is simple so doesn't require sequencing.
    GenTree* preemptiveGCNode = new (comp, GT_START_PREEMPTGC) GenTree(GT_START_PREEMPTGC, TYP_VOID);
    BlockRange().InsertBefore(insertBefore, preemptiveGCNode);
}

// Insert the code that goes after every unmanaged call.
void Lowering::InsertUnmanagedCallEpilog(GenTreeCall* call)
{
    JITDUMP("======= Inserting unmanaged call epilog\n");

    noway_assert(comp->lvaInlinedPInvokeFrameVar != BAD_VAR_NUM);
    LclVarDsc* pInvokeFrameLcl = comp->lvaGetDesc(comp->lvaInlinedPInvokeFrameVar);

    if (comp->opts.ShouldUsePInvokeHelpers())
    {
        GenTreeCall::Use* args = comp->gtNewCallArgs(comp->gtNewLclAddr(pInvokeFrameLcl));
        comp->lvaSetAddressExposed(pInvokeFrameLcl);
        GenTreeCall* pInvokeEnd = comp->gtNewHelperCallNode(CORINFO_HELP_JIT_PINVOKE_END, TYP_VOID, args);
        LIR::InsertHelperCallBefore(comp, BlockRange(), call->gtNext, pInvokeEnd);

        return;
    }

    // gcstate = 1
    GenTree* insertionPoint = call->gtNext;
    InsertSetGCState(insertionPoint, 1);
    InsertReturnTrap(insertionPoint);

    // Pop the frame if necessary. On 32-bit targets this only happens in the method epilog; on 64-bit targets thi
    // happens after every PInvoke call in non-stubs. 32-bit targets instead mark the frame as inactive.
    CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef TARGET_64BIT
    if (!comp->opts.IsJitFlagSet(JitFlags::JIT_FLAG_IL_STUB))
    {
        InsertFrameLinkUpdate(BlockRange(), insertionPoint, PopFrame);
    }
#else
    const CORINFO_EE_INFO::InlinedCallFrameInfo& callFrameInfo = comp->eeGetEEInfo()->inlinedCallFrameInfo;

    // ----------------------------------------------------------------------------------
    // InlinedCallFrame.m_pCallerReturnAddress = nullptr

    GenTreeIntCon* zero = comp->gtNewIconNode(0, TYP_I_IMPL);
    GenTreeLclFld* storeCallSiteTracker =
        comp->gtNewLclStoreFld(TYP_I_IMPL, pInvokeFrameLcl, callFrameInfo.offsetOfReturnAddress, zero);
    BlockRange().InsertBefore(insertionPoint, zero, storeCallSiteTracker);
    ContainCheckStoreLcl(storeCallSiteTracker);
#endif // TARGET_64BIT
}

void Lowering::InsertUnmanagedCallPrologAndEpilog(GenTreeCall* call)
{
    assert(call->IsUnmanaged() X86_ONLY(&&!call->IsTailCallViaJitHelper()));

    // PInvoke lowering varies depending on the flags passed in by the EE. By default,
    // GC transitions are generated inline; if CORJIT_FLAG_USE_PINVOKE_HELPERS is specified,
    // GC transitions are instead performed using helper calls. Examples of each case are given
    // below. Note that the data structure that is used to store information about a call frame
    // containing any P/Invoke calls is initialized in the method prolog (see
    // InsertPInvokeMethod{Prolog,Epilog} for details).
    //
    // Inline transitions:
    //     InlinedCallFrame inlinedCallFrame;
    //
    //     ...
    //
    //     // Set up frame information
    //     inlinedCallFrame.callTarget = methodHandle;      // stored in m_Datum
    //     inlinedCallFrame.m_pCallSiteSP = SP;             // x86 only
    //     inlinedCallFrame.m_pCallerReturnAddress = &label; (the address of the instruction immediately following the
    //     call)
    //     Thread.m_pFrame = &inlinedCallFrame; (non-IL-stub only)
    //
    //     // Switch the thread's GC mode to preemptive mode
    //     thread->m_fPreemptiveGCDisabled = 0;
    //
    //     // Call the unmanaged method
    //     target();
    //
    //     // Switch the thread's GC mode back to cooperative mode
    //     thread->m_fPreemptiveGCDisabled = 1;
    //
    //     // Rendezvous with a running collection if necessary
    //     if (g_TrapReturningThreads)
    //         RareDisablePreemptiveGC();
    //
    // Transitions using helpers:
    //
    //     OpaqueFrame opaqueFrame;
    //
    //     ...
    //
    //     // Call the JIT_PINVOKE_BEGIN helper
    //     JIT_PINVOKE_BEGIN(&opaqueFrame);
    //
    //     // Call the unmanaged method
    //     target();
    //
    //     // Call the JIT_PINVOKE_END helper
    //     JIT_PINVOKE_END(&opaqueFrame);
    //
    // Note that the JIT_PINVOKE_{BEGIN.END} helpers currently use the default calling convention for the target
    // platform. They may be changed in the future such that they preserve all register values.

    // All code generated by this function must not contain the randomly-inserted NOPs
    // that we insert to inhibit JIT spraying in partial trust scenarios.
    // The PINVOKE_PROLOG op signals this to the code generator/emitter.

    GenTree* prolog = new (comp, GT_PINVOKE_PROLOG) GenTree(GT_PINVOKE_PROLOG, TYP_VOID);
    BlockRange().InsertBefore(call, prolog);

    if (!call->IsSuppressGCTransition())
    {
        InsertUnmanagedCallProlog(call);
        InsertUnmanagedCallEpilog(call);
    }
}

bool Lowering::TryCreateAddrMode(GenTree* addr, bool isContainable)
{
    if (!addr->OperIs(GT_ADD))
    {
        return false;
    }

    AddrMode am(addr);
    am.Extract(comp);

    if (am.HasTooManyNodes())
    {
        return false;
    }

    if (!isContainable)
    {
        // this is just a reg-const add
        if (am.index == nullptr)
        {
            return false;
        }

        // this is just a reg-reg add
        if ((am.scale == 1) && (am.offset == 0))
        {
            return false;
        }
    }

    if (!IsSafeToMoveLclRegUseForward(addr, am.base, am.index))
    {
        JITDUMPLIRNODE(addr, "No addressing mode:\n  ");
        return false;
    }

    JITDUMP("Addressing mode:\n");

    if (am.base != nullptr)
    {
        JITDUMPLIRNODE(am.base, "Base\n");
    }

    if (am.index != nullptr)
    {
        JITDUMPLIRNODE(am.index, " + Index * %u", am.scale);
    }

    JITDUMP(" + %d\n", am.offset);

    // Save the (potentially) unused operands before changing the address to LEA.
    ArrayStack<GenTree*> unusedStack(comp->getAllocator(CMK_ArrayStack));
    unusedStack.Push(addr->AsOp()->GetOp(0));
    unusedStack.Push(addr->AsOp()->GetOp(1));

    addr->ChangeOper(GT_LEA);
    // Make sure there are no leftover side effects (though the existing ADD we're
    // changing shouldn't have any at this point, but sometimes it does).
    addr->SetSideEffects(GTF_NONE);

    GenTreeAddrMode* addrMode = addr->AsAddrMode();
    addrMode->SetBase(am.base);
    addrMode->SetIndex(am.index);
    addrMode->SetScale(am.scale);
    addrMode->SetOffset(am.offset);

    // Neither the base nor the index should now be contained.
    if (am.base != nullptr)
    {
        am.base->ClearContained();
    }

    if (am.index != nullptr)
    {
        am.index->ClearContained();
    }

    // Remove all the nodes that are no longer used.
    assert(am.nodes[0] == addr);

    for (unsigned i = 1; i < am.nodeCount; i++)
    {
        GenTree* node = am.nodes[i];
        assert(node->OperIs(GT_ADD, GT_LSH, GT_MUL, GT_CNS_INT));
        BlockRange().Unlink(node);
    }

    JITDUMPLIRNODE(addrMode, "New addressing mode node:\n  ");
    JITDUMP("\n");

    return true;
}

GenTree* Lowering::LowerAdd(GenTreeOp* node)
{
    assert(node->OperIs(GT_ADD, GT_OVF_SADD, GT_OVF_UADD));

    GenTree* op1 = node->GetOp(0);
    GenTree* op2 = node->GetOp(1);
    LIR::Use use;

    // It is not the best place to do such simple arithmetic optimizations,
    // but it allows us to avoid `LEA(addr, 0)` nodes and doing that in morph
    // requires more changes. Delete that part if we get an expression optimizer.
    if (op2->IsIntCon(0))
    {
        JITDUMPLIRNODE(node, "Lower: optimize val + 0: ");
        JITDUMPLIRNODE(op1, "Replaced with: ");

        if (BlockRange().TryGetUse(node, &use))
        {
            use.SetDef(op1);
        }
        else
        {
            op1->SetUnusedValue();
        }

        GenTree* next = node->gtNext;
        BlockRange().Unlink(op2);
        BlockRange().Unlink(node);
        JITDUMP("Remove [%06u], [%06u]\n", op2->GetID(), node->GetID());

        return next;
    }

#ifdef TARGET_XARCH
    if (node->OperIs(GT_ADD) && BlockRange().TryGetUse(node, &use))
    {
        // If this is a child of an indir, let the parent handle it.
        // If there is a chain of adds, only look at the topmost one.
        GenTree* parent = use.User();
        if (!parent->IsIndir() && !parent->OperIs(GT_ADD))
        {
            TryCreateAddrMode(node, false);
        }
    }
#endif

    if (node->OperIs(GT_ADD, GT_OVF_SADD, GT_OVF_UADD))
    {
        ContainCheckBinary(node);
    }

    return nullptr;
}

#ifndef TARGET_ARM64
void Lowering::LowerShift(GenTreeOp* shift)
{
    assert(shift->OperIs(GT_LSH, GT_RSH, GT_RSZ));

    GenTree* shiftBy = shift->GetOp(1);

    if (shiftBy->OperIs(GT_CNS_INT))
    {
#if defined(TARGET_AMD64) || defined(TARGET_ARM64)
        size_t mask = shift->TypeIs(TYP_LONG) ? 0x3f : 0x1f;
#elif defined(TARGET_X86) || defined(TARGET_ARM)
        size_t           mask           = 0x1f;
#else
#error Unknown target
#endif

        unsigned shiftByBits = static_cast<unsigned>(shiftBy->AsIntCon()->GetValue()) & mask;
        shiftBy->AsIntCon()->SetValue(shiftByBits);

        if ((shiftByBits >= 24) && shift->OperIs(GT_LSH) && comp->opts.OptimizationEnabled())
        {
            // Remove source casts if the shift discards the produced sign/zero bits.
            //
            // Some of this would probably be better done during morph or some sort
            // of tree narrowing phase. The problem is that this removes INT to LONG
            // casts, transforming
            //     LSH.long(CAST.long(x.int), 32)
            // into
            //     LSH.long(x.int, 32)
            //
            // While there's nothing intrinsically wrong about having a node with
            // different source and destination types, it is possible that some
            // frontend phases might get confused by such a shift node.

            unsigned consumedBits = varTypeBitSize(shift->GetType());

            assert((consumedBits == 32) || (consumedBits == 64));
            assert(shiftByBits < consumedBits);

            consumedBits -= shiftByBits;

            GenTree* src = shift->GetOp(0);

            if (src->OperIs(GT_SXT, GT_UXT) && (consumedBits <= 32))
            {
                JITDUMP("Removing SXT/UXT [%06u] producing 32 bits from LSH [%06u] consuming %u bits\n", src->GetID(),
                        shift->GetID(), consumedBits);

                BlockRange().Unlink(src);
                src = src->AsUnOp()->GetOp(0);
                src->ClearContained();
            }

            if (src->OperIs(GT_TRUNC) && (consumedBits <= 32))
            {
                GenTreeUnOp* cast = src->AsUnOp();

                JITDUMP("Removing TRUNC [%06u] producing 32 bits from LSH [%06u] consuming %u bits\n", cast->GetID(),
                        shift->GetID(), consumedBits);

                BlockRange().Unlink(src);
                src = cast->GetOp(0);
                src->ClearContained();

#ifndef TARGET_64BIT
                if (src->OperIs(GT_LONG))
                {
                    // We're run into a long to int cast on a 32 bit target. The LONG node
                    // needs to be removed since the shift wouldn't know what to do with it.
                    // TODO-MIKE-Cleanup: Why doesn't CAST lowering deal with this?!

                    BlockRange().Unlink(src);
                    src->AsOp()->GetOp(1)->SetUnusedValue();
                    src = src->AsOp()->GetOp(0);
                }
#endif
            }

            while (src->OperIs(GT_CONV))
            {
                GenTreeUnOp* cast = src->AsUnOp();

                assert(varTypeIsIntegral(cast->GetOp(0)->GetType()));
                assert(varTypeIsSmall(cast->GetType()));

                unsigned producedBits = varTypeBitSize(cast->GetType());

                if (consumedBits > producedBits)
                {
                    break;
                }

                JITDUMP("Removing CONV [%06u] producing %u bits from LSH [%06u] consuming %u bits\n", cast->GetID(),
                        producedBits, shift->GetID(), consumedBits);

                BlockRange().Unlink(src);
                src = cast->GetOp(0);
                src->ClearContained();
            }

            if (src->OperIs(GT_SXT, GT_UXT) && (consumedBits <= 32))
            {
                JITDUMP("Removing SXT/UXT [%06u] producing 32 bits from LSH [%06u] consuming %u bits\n", src->GetID(),
                        shift->GetID(), consumedBits);

                BlockRange().Unlink(src);
                src = src->AsUnOp()->GetOp(0);
                src->ClearContained();
            }

#ifdef TARGET_XARCH
            // If the source is a small signed int memory operand then we can make it unsigned
            // if the sign bits aren't consumed, movzx has smaller encoding than movsx.

            if (src->OperIs(GT_LCL_LOAD_FLD, GT_IND_LOAD) && varTypeIsSmall(src->GetType()) &&
                (consumedBits <= varTypeBitSize(src->GetType())))
            {
                src->SetType(varTypeToSmallUnsigned(src->GetType()));
            }
#endif

            shift->SetOp(0, src);
        }
    }
    else
    {
#if defined(TARGET_AMD64) || defined(TARGET_ARM64)
        size_t mask = shift->TypeIs(TYP_LONG) ? 0x3f : 0x1f;
#elif defined(TARGET_X86)
        size_t           mask           = 0x1f;
#elif defined(TARGET_ARM)
        size_t mask = 0xff;
#elif
#error Unknown target
#endif

#ifndef TARGET_ARM
        // Remove unnecessary shift count masking. x64/x86/ARM64 shift instructions mask the shift count
        // to 5 bits (or 6 bits for 64 bit operations). ARM32 only masks 8 bits so this isn't likely to
        // be very useful since the main goal is to remove the masking done by the C# compiler.

        while (shiftBy->OperIs(GT_AND))
        {
            GenTree* maskOp = shiftBy->AsOp()->GetOp(1);

            if (!maskOp->OperIs(GT_CNS_INT))
            {
                break;
            }

            if ((static_cast<size_t>(maskOp->AsIntCon()->GetValue()) & mask) != mask)
            {
                break;
            }

            BlockRange().Unlink(shiftBy);
            BlockRange().Unlink(maskOp);

            shiftBy = shiftBy->AsOp()->GetOp(0);
            shiftBy->ClearContained();
        }

        shift->SetOp(1, shiftBy);
#endif
    }

    ContainCheckShiftRotate(shift);
}
#endif // !TARGET_ARM64

#ifdef FEATURE_SIMD
void Lowering::WidenSIMD12IfNecessary(GenTreeLclVar* node)
{
    assert(node->TypeIs(TYP_SIMD12));

    // Assumption 1:
    // RyuJit backend depends on the assumption that on 64-Bit targets Vector3 size is rounded off
    // to TARGET_POINTER_SIZE and hence Vector3 locals on stack can be treated as TYP_SIMD16 for
    // reading and writing purposes.
    //
    // Assumption 2:
    // RyuJit backend is making another implicit assumption that Vector3 type args when passed in
    // registers or on stack, the upper most 4-bytes will be zero.
    //
    // For P/Invoke return and Reverse P/Invoke argument passing, native compiler doesn't guarantee
    // that upper 4-bytes of a Vector3 type struct is zero initialized and hence assumption 2 is
    // invalid.
    //
    // RyuJIT x64 Windows: arguments are treated as passed by ref and hence read/written just 12
    // bytes. In case of Vector3 returns, Caller allocates a zero initialized Vector3 local and
    // passes it retBuf arg and Callee method writes only 12 bytes to retBuf. For this reason,
    // there is no need to clear upper 4-bytes of Vector3 type args.
    //
    // RyuJIT x64 Unix: arguments are treated as passed by value and read/written as if SIMD16.
    // Vector3 return values are returned two return registers and Caller assembles them into a
    // single xmm reg. Hence RyuJIT explicitly generates code to clears upper 4-bytes of Vector3
    // type args in prolog and Vector3 type return value of a call
    //
    // RyuJIT x86 Windows: all non-param Vector3 local vars are allocated as 16 bytes. Vector3 arguments
    // are pushed as 12 bytes. For return values, a 16-byte local is allocated and the address passed
    // as a return buffer pointer. The callee doesn't write the high 4 bytes, and we don't need to clear
    // it either.

    if (CanWidenSimd12ToSimd16(node->GetLcl()))
    {
        JITDUMPLIRNODE(node, "Mapping SIMD12 local node to SIMD16:\n");

        node->SetType(TYP_SIMD16);
    }
}

bool Lowering::CanWidenSimd12ToSimd16(const LclVarDsc* lcl)
{
    assert(lcl->TypeIs(TYP_SIMD12));

    if (lcl->IsDependentPromotedField(comp))
    {
        lcl = comp->lvaGetDesc(lcl->GetPromotedFieldParentLclNum());

        if (lcl->GetPromotedFieldCount() > 1)
        {
            return false;
        }
    }

    // TODO-MIKE-Cleanup: Maybe this should be solely based on GetFrameSize?
    // But GetFrameSize's primary purpose is to return the local size for our
    // own frame allocation needs, it shouldn't have to deal with param sizes
    // which are ABI specific (except for reg params, which may have allocated
    // space on our own frame).
    // Use lvaGetParamAllocSize perhaps? Originally that was kind of expensive
    // but now it's probably reasonable enough, though it would still repeat
    // the same computation for every node we try to widen.
    // Ideally, we'd just compute the local allocation size once and store it
    // int LclVarDsc, but that would increase the size of LclVarDsc and it's
    // not need often enough to justify that.

    if (lcl->IsParam())
    {
#if defined(OSX_ARM64_ABI)
        // Vector3 HFA size isn't rounded up to 16 bytes on osx-arm64 when
        // passed in stack.
        return !lcl->IsRegParam();
#elif defined(UNIX_AMD64_ABI) || defined(TARGET_ARM64)
        return true;
#else
        // x86 Vector3 params are always 12 byte in size so we can't widen.
        // ARM32 doesn't support SIMD but it would have the same restriction
        // for stack params (though not for reg params).
        // For anything else we're just being conservative.
        return false;
#endif
    }

    return lcl->GetFrameSize() == 16;
}
#endif // FEATURE_SIMD

static unsigned OffsetOfMDArrayLowerBound(var_types elemType, unsigned rank, unsigned dimension)
{
    // Note that the lower bound and length fields of the Array object are always INT, even on 64-bit targets.
    return Compiler::eeGetArrayDataOffset(elemType) + varTypeSize(TYP_INT) * (dimension + rank);
}

static unsigned OffsetOfMDArrayDimensionSize(var_types elemType, unsigned rank, unsigned dimension)
{
    // Note that the lower bound and length fields of the Array object are always INT, even on 64-bit targets.
    return Compiler::eeGetArrayDataOffset(elemType) + varTypeSize(TYP_INT) * dimension;
}

GenTree* Lowering::LowerArrElem(GenTreeArrElem* elem)
{
    assert(elem->TypeIs(TYP_BYREF));

    GenTree* array = elem->GetArray();

    assert(array->TypeIs(TYP_REF));

    // TODO-MIKE-Review: Allowing DNER LCL_LOAD results in poor CQ,
    // we really should always have the array reference in a register.
    if (!array->OperIs(GT_LCL_LOAD))
    {
        LIR::Use use(BlockRange(), &elem->GetUse(0).NodeRef(), elem);
        ReplaceWithLclLoad(use);
        array = elem->GetArray();
    }

    if (mdArrayLengthTempLcl == nullptr)
    {
        // TODO-MIKE-Review: It's possible to use only 2 temporaries, if we do the index
        // multiplication earlier. However, that causes problems with madd formation on ARM64.
        mdArrayLengthTempLcl = comp->lvaNewTemp(TYP_INT, true DEBUG_ARG("MDArrayLengthTemp"));
        mdArrayIndex1TempLcl = comp->lvaNewTemp(TYP_INT, true DEBUG_ARG("MDArrayIndex1Temp"));
        mdArrayIndex2TempLcl = comp->lvaNewTemp(TYP_INT, true DEBUG_ARG("MDArrayIndex2Temp"));
    }

    // TODO-MIKE-Review: This should probably be done during global morphing, so that the
    // invariant dimension computations can be hoisted. We could also propagate constant
    // lower bounds and length if the array is created within the same method (and lower
    // bounds would be very useful to propagate as they're always 0 for C# created arrays).

    LclVarDsc* const arrayLcl  = array->AsLclLoad()->GetLcl();
    LclVarDsc* const lengthLcl = mdArrayLengthTempLcl;
    LclVarDsc* const index1Lcl = mdArrayIndex1TempLcl;
    LclVarDsc* const index2Lcl = mdArrayIndex2TempLcl;

    var_types const elemType         = elem->GetElemType();
    unsigned const  rank             = elem->GetRank();
    unsigned        lowerBoundOffset = OffsetOfMDArrayLowerBound(elemType, rank, 0);
    unsigned        lengthOffset     = OffsetOfMDArrayDimensionSize(elemType, rank, 0);

    GenTree* addr  = comp->gtNewAddrMode(array, lowerBoundOffset);
    GenTree* load  = comp->gtNewIndLoad(TYP_INT, addr);
    GenTree* index = comp->gtNewOperNode(GT_SUB, TYP_INT, elem->GetIndex(0), load);
    GenTree* store = comp->gtNewLclStore(index1Lcl, TYP_INT, index);
    BlockRange().InsertBefore(elem, addr, load, index, store);
    GenTree* nextToLower = addr;

    array = comp->gtNewLclLoad(arrayLcl, TYP_REF);
    addr  = comp->gtNewAddrMode(array, lengthOffset);
    load  = comp->gtNewIndLoad(TYP_INT, addr);
    BlockRange().InsertBefore(elem, array, addr, load);

    index           = comp->gtNewLclLoad(index1Lcl, TYP_INT);
    GenTree* check  = comp->gtNewBoundsChk(index, load, ThrowHelperKind::IndexOutOfRange);
    GenTree* linear = comp->gtNewLclLoad(index1Lcl, TYP_INT);
    BlockRange().InsertBefore(elem, index, check, linear);
    GenTree* mul;

    for (unsigned dim = 1; dim < rank; dim++)
    {
        lowerBoundOffset = OffsetOfMDArrayLowerBound(elemType, rank, dim);
        lengthOffset     = OffsetOfMDArrayDimensionSize(elemType, rank, dim);

        array = comp->gtNewLclLoad(arrayLcl, TYP_REF);
        addr  = comp->gtNewAddrMode(array, lowerBoundOffset);
        load  = comp->gtNewIndLoad(TYP_INT, addr);
        index = comp->gtNewOperNode(GT_SUB, TYP_INT, elem->GetIndex(dim), load);
        BlockRange().InsertBefore(elem, array, addr, load, index);

        array = comp->gtNewLclLoad(arrayLcl, TYP_REF);
        addr  = comp->gtNewAddrMode(array, lengthOffset);
        load  = comp->gtNewIndLoad(TYP_INT, addr);
        store = comp->gtNewLclStore(lengthLcl, TYP_INT, load);
        BlockRange().InsertBefore(elem, array, addr, load, store);

        store = comp->gtNewLclStore(index2Lcl, TYP_INT, index);
        index = comp->gtNewLclLoad(index2Lcl, TYP_INT);
        load  = comp->gtNewLclLoad(lengthLcl, TYP_INT);
        check = comp->gtNewBoundsChk(index, load, ThrowHelperKind::IndexOutOfRange);
        BlockRange().InsertBefore(elem, store, index, load, check);

        index  = comp->gtNewLclLoad(index2Lcl, TYP_INT);
        load   = comp->gtNewLclLoad(lengthLcl, TYP_INT);
        mul    = comp->gtNewOperNode(GT_MUL, TYP_INT, linear, load);
        linear = comp->gtNewOperNode(GT_ADD, TYP_INT, mul, index);
        BlockRange().InsertBefore(elem, index, load, mul, linear);
    }

    LIR::Use elemUse;

    if (!BlockRange().TryGetUse(elem, &elemUse))
    {
        linear->SetUnusedValue();
    }
    else
    {
        unsigned scale    = elem->GetElemSize();
        GenTree* leaIndex = linear;

#ifdef TARGET_64BIT
        // TODO-MIKE-CQ: This is not eliminated by codegen even if the upper 32 bits are known to be 0.
        // Actually, this probably a LSRA problem, since it allocates a different register to UXT for
        // no reason (the UXT operand is SDSU so it should just reuse its register).
        leaIndex = comp->gtNewOperNode(GT_UXT, TYP_LONG, leaIndex);
        BlockRange().InsertBefore(elem, leaIndex);
#endif

        if (!AddrMode::IsIndexScale(scale))
        {
            GenTree* scaleNode = comp->gtNewIconNode(scale, TYP_I_IMPL);
            leaIndex           = comp->gtNewOperNode(GT_MUL, TYP_I_IMPL, leaIndex, scaleNode);
            BlockRange().InsertBefore(elem, scaleNode, leaIndex);
            scale = 1;
        }

        unsigned offset = Compiler::eeGetMDArrayDataOffset(elemType, rank);
        array           = comp->gtNewLclLoad(arrayLcl, TYP_REF);
        GenTree* lea    = comp->gtNewAddrMode(TYP_BYREF, array, leaIndex, scale, offset);
        BlockRange().InsertBefore(elem, array, lea);

        elemUse.SetDef(lea);
    }

    BlockRange().Unlink(elem);
    return nextToLower;
}

#ifdef DEBUG
void Lowering::VerifyAllLocalsImplicitlyReferenced()
{
    assert(comp->opts.OptimizationDisabled());
    assert(!comp->opts.EnregLocals());
    assert(!comp->fgLocalVarLivenessDone);

    for (LclVarDsc* lcl : comp->Locals())
    {
        assert(varTypeIsValidLclType(lcl->GetType()));

        if (comp->lvaIsX86VarargsStackParam(lcl))
        {
            assert(lcl->GetRefCount() == 0);
        }
        else
        {
            // lvaAllocTemp should automatically set lvImplicitlyReferenced after lvaMarkLocalVars phase.
            assert(lcl->lvImplicitlyReferenced);
        }

        assert(!lcl->HasLiveness());
        assert(!lcl->lvMustInit);
    }
}

void Lowering::VerifyCallArg(GenTree* arg)
{
    assert(arg->IsValue() || arg->IsArgStore());

    if (GenTreeFieldList* fieldList = arg->IsFieldList())
    {
        assert(fieldList->isContained());

        for (GenTreeFieldList::Use& use : fieldList->Uses())
        {
            assert(use.GetNode()->OperIs(GT_PUTARG_REG));
        }
    }
    else
    {
        assert(arg->OperIs(GT_PUTARG_REG, GT_ARG_STORE));
    }
}

void Lowering::VerifyCall(GenTreeCall* call)
{
    for (GenTreeUse& use : call->Uses())
    {
        VerifyCallArg(use.GetNode());
    }
}

void Lowering::VerifyNode(GenTree* node)
{
    switch (node->GetOper())
    {
        case GT_CALL:
            VerifyCall(node->AsCall());
            break;

#ifdef FEATURE_SIMD
        case GT_HWINTRINSIC:
            assert(!node->TypeIs(TYP_SIMD12));
            break;
#endif

        case GT_LCL_LOAD:
        case GT_LCL_STORE:
        {
            LclVarDsc* lcl = node->AsLclVar()->GetLcl();
#ifdef FEATURE_SIMD
            assert(!node->TypeIs(TYP_SIMD12) || !CanWidenSimd12ToSimd16(lcl));
#endif
            assert(!lcl->IsPromoted() || lcl->lvDoNotEnregister || lcl->lvIsMultiRegRet);
        }
        break;

        case GT_LCL_ADDR:
            assert(node->AsLclAddr()->GetLcl()->IsAddressExposed());
            break;

        case GT_PHI:
        case GT_LCL_USE:
        case GT_LCL_DEF:
        case GT_INSERT:
        case GT_EXTRACT:
            assert(!"Should not see SSA nodes in lowering");
            break;

        case GT_LCL_LOAD_FLD:
        case GT_LCL_STORE_FLD:
            assert(node->AsLclFld()->GetLcl()->lvDoNotEnregister);
            break;

        default:
            break;
    }
}

bool Lowering::VerifyBlock(BasicBlock* block)
{
    assert(block->isEmpty() || block->IsLIR());

    for (GenTree* node : LIR::AsRange(block))
    {
        VerifyNode(node);
    }

    assert(LIR::AsRange(block).CheckLIR(comp, true));
    return true;
}
#endif // DEBUG

#if FEATURE_MULTIREG_RET

void Lowering::MakeMultiRegLclStore(GenTreeLclStore* store, GenTree* value)
{
    assert(value->IsMultiRegNode());

    LclVarDsc* lcl = store->GetLcl();

    bool canEnregister = false;

    if (comp->opts.EnregLocals() && lcl->IsIndependentPromoted())
    {
        if (GenTreeCall* call = value->IsCall())
        {
            // TODO-MIKE-Cleanup: This should probably be only an assert, we should not
            // reach here with a P-INDEP local if the fields and registers do not match.
            canEnregister = lcl->GetPromotedFieldCount() == call->GetRegCount();
        }
#ifndef TARGET_64BIT
        else
        {
            canEnregister = lcl->TypeIs(TYP_LONG) && value->IsMultiRegOpLong();
        }
#endif
    }

    if (canEnregister)
    {
        store->SetMultiReg();
    }
    else
    {
        assert(!store->IsMultiReg());

        if (lcl->IsPromoted() && !lcl->lvDoNotEnregister)
        {
            comp->lvaSetDoNotEnregister(lcl DEBUGARG(Compiler::DNER_BlockOp));
        }
    }
}

#endif // FEATURE_MULTIREG_RET

void Lowering::ContainCheckReturnTrap(GenTreeOp* node)
{
    assert(node->OperIs(GT_RETURNTRAP));
    assert(node->GetOp(0)->OperIs(GT_IND_LOAD));

#ifdef TARGET_XARCH
    node->GetOp(0)->SetContained();
#endif
}

void Lowering::LowerLclHeap(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_LCLHEAP) && node->TypeIs(TYP_I_IMPL));

    if (GenTreeIntCon* size = node->GetOp(0)->IsIntCon())
    {
        if (size->GetValue() == 0)
        {
            node->ChangeToIntCon(0);
            BlockRange().Unlink(size);
        }
        else
        {
            size->SetContained();
        }
    }
}

void Lowering::ContainCheckRet(GenTreeUnOp* ret)
{
    assert(ret->OperIs(GT_RETURN));

    GenTree* src = ret->GetOp(0);

#ifndef TARGET_64BIT
    if (ret->TypeIs(TYP_LONG))
    {
        noway_assert(src->TypeIs(TYP_DOUBLE) || src->OperIs(GT_LONG, GT_BITCAST));

        return;
    }
#endif

    assert(!ret->TypeIs(TYP_STRUCT) || src->IsFieldList() || src->IsCall());
}

void Lowering::ContainCheckJTrue(GenTreeUnOp* node)
{
    GenTree* cmp = node->GetOp(0);
    // The compare does not need to be generated into a register.
    cmp->SetType(TYP_VOID);
    cmp->AddImplicitFlagsDef();
}

GenTree* Lowering::LowerBitCast(GenTreeUnOp* bitcast)
{
    assert(bitcast->OperIs(GT_BITCAST));
    assert(!bitcast->TypeIs(TYP_STRUCT));

    auto CanRetypeIndLoad = [](GenTreeIndLoad* load, var_types type) {
#ifdef TARGET_ARMARCH
        // For simplicity and safety recognize only the typical int <-> float case.
        return (type == TYP_INT) || ((type == TYP_FLOAT) && load->TypeIs(TYP_INT) && !load->IsUnaligned());
#elif !defined(TARGET_64BIT)
        return type != TYP_LONG;
#else
        return true;
#endif
    };

    auto CanRetypeLclLoadFld = [](GenTreeLclFld* load, var_types type) {
#ifdef TARGET_ARMARCH
        return (type == TYP_INT) || ((type == TYP_FLOAT) && ((load->GetLclOffs() % 4) == 0));
#elif !defined(TARGET_64BIT)
        return type != TYP_LONG;
#else
        return true;
#endif
    };

    auto CanRetypeLclLoad = [](var_types type) {
#ifdef TARGET_ARMARCH
        return (type == TYP_INT) || (type == TYP_FLOAT);
#elif !defined(TARGET_64BIT)
        return type != TYP_LONG;
#else
        return true;
#endif
    };

    GenTree* next   = bitcast->gtNext;
    GenTree* src    = bitcast->GetOp(0);
    bool     remove = false;

#ifndef TARGET_64BIT
    if (src->OperIs(GT_LONG))
    {
        assert(bitcast->TypeIs(TYP_DOUBLE X86_ARG(TYP_SIMD8)));
        assert(src->isContained());

        return next;
    }
#endif

    if ((src->OperIs(GT_IND_LOAD) && CanRetypeIndLoad(src->AsIndLoad(), bitcast->GetType())) ||
        (src->OperIs(GT_LCL_LOAD_FLD) && CanRetypeLclLoadFld(src->AsLclLoadFld(), bitcast->GetType())))
    {
        src->SetType(bitcast->GetType());
        remove = true;
    }
    else if (src->OperIs(GT_LCL_LOAD))
    {
        LclVarDsc* srcLcl = src->AsLclLoad()->GetLcl();

        if (srcLcl->lvDoNotEnregister)
        {
            if (CanRetypeLclLoad(bitcast->GetType()))
            {
                // If it's not a register candidate then we can turn it into a LCL_LOAD_FLD and retype it.
                src->ChangeToLclLoadFld(bitcast->GetType(), srcLcl, 0, FieldSeqStore::NotAField());
                comp->lvaSetDoNotEnregister(srcLcl DEBUGARG(Compiler::DNER_LocalField));
                remove = true;
            }
            else
            {
                src->SetContained();
            }
        }
        else
        {
            src->SetRegOptional();
        }
    }

    if (remove)
    {
        LIR::Use use;

        if (BlockRange().TryGetUse(bitcast, &use))
        {
            use.SetDef(src);
        }
        else
        {
            src->SetUnusedValue();
        }

        BlockRange().Unlink(bitcast);
    }

    return next;
}

void Lowering::LowerOvfConv(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_OVF_SCONV, GT_OVF_UCONV) && varTypeIsSmallInt(node->GetType()));
    assert(varActualTypeIsIntOrI(node->GetOp(0)->GetType()));
}

void Lowering::LowerOvfUnsigned(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_OVF_U) && node->TypeIs(TYP_INT, TYP_I_IMPL));
    assert(node->GetType() == varActualType(node->GetOp(0)->GetType()));
}

void Lowering::LowerOvfTruncate(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_OVF_TRUNC, GT_OVF_STRUNC, GT_OVF_UTRUNC));
    assert(node->TypeIs(TYP_INT) && node->GetOp(0)->TypeIs(TYP_LONG));
#ifndef TARGET_64BIT
    assert(node->GetOp(0)->OperIs(GT_LONG) && node->GetOp(0)->isContained());
#endif
}

GenTree* Lowering::LowerConv(GenTreeUnOp* cast)
{
    assert(cast->OperIs(GT_CONV) && varTypeIsSmallInt(cast->GetType()));

    GenTree*  src     = cast->GetOp(0);
    var_types dstType = cast->GetType();
    var_types srcType = src->GetType();
    bool      remove  = false;

#ifdef TARGET_64BIT
    if ((srcType == TYP_LONG) && src->OperIs(GT_LCL_LOAD))
    {
        src->SetType(TYP_INT);
    }
#else
    assert(srcType != TYP_LONG);
#endif

    if (IsMemOperand(src))
    {
        // TODO-MIKE-Cleanup: Morph does something similar but more restrictive. It's not clear
        // if there are any advantages in doing such a transform earlier (in fact there may be one
        // disadvantage - retyping nodes may prevent them from being CSEd) so it should be deleted.
        // But the UBYTE/BYTE-SHORT case should probably only be handled in morph.

        if (varTypeSize(dstType) <= varTypeSize(srcType))
        {
            src->SetType(dstType);
            remove = true;
        }
        else if ((srcType == TYP_UBYTE) || (srcType == TYP_BYTE && dstType == TYP_SHORT))
        {
            remove = true;
        }

        if (remove)
        {
            LIR::Use use;

            if (BlockRange().TryGetUse(cast, &use))
            {
                use.SetDef(src);
            }
            else
            {
                src->SetUnusedValue();
            }

            GenTree* next = cast->gtNext;
            BlockRange().Unlink(cast);
            return next;
        }
    }

    // TODO-MIKE-Review: This is probably incorrect in some rare cases - e.g. CONV<ushort>(param<byte>).
    // On osx-arm64 the param stack space is only 1 byte so we can't load ushort directly from that.
    src->SetRegOptional();

    return cast->gtNext;
}

#ifdef TARGET_64BIT
GenTree* Lowering::LowerTruncate(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_TRUNC) && node->TypeIs(TYP_INT) && node->GetOp(0)->TypeIs(TYP_LONG));

    GenTree* src = node->GetOp(0);

    if (IsMemOperand(src) || (src->OperIs(GT_LCL_LOAD) && IsSafeToMoveLclRegUseForward(node, src, nullptr)))
    {
        // TODO-MIKE-Cleanup: Morph does something similar but more restrictive. It's not clear
        // if there are any advantages in doing such a transform earlier (in fact there may be one
        // disadvantage - retyping nodes may prevent them from being CSEd) so it should be deleted.

        src->SetType(TYP_INT);

        LIR::Use use;

        if (BlockRange().TryGetUse(node, &use))
        {
            use.SetDef(src);
        }
        else
        {
            src->SetUnusedValue();
        }

        GenTree* next = node->gtNext;
        BlockRange().Unlink(node);

        return next;
    }

    return node->gtNext;
}

void Lowering::LowerSignedExtend(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_SXT) && node->TypeIs(TYP_LONG));

    GenTree* src = node->GetOp(0);

    // TODO-MIKE-Review: On arm64 this may interfere with s/umull generation.
    if (varTypeIsSmallUnsigned(src->GetType()))
    {
        node->SetOper(GT_UXT);
        LowerUnsignedExtend(node);

        return;
    }

    ContainCheckIntExtend(node, src);
}

void Lowering::LowerUnsignedExtend(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_UXT) && node->TypeIs(TYP_LONG));

    GenTree* src = node->GetOp(0);

    if (varTypeIsSmallSigned(src->GetType()))
    {
        return;
    }

    ContainCheckIntExtend(node, src);
}

#endif // TARGET_64BIT

#ifndef TARGET_ARM64
void Lowering::LowerFloatToInt(GenTreeUnOp* cast)
{
    assert(cast->OperIs(GT_FTOS, GT_FTOU) && cast->TypeIs(TYP_INT, TYP_LONG));
    assert(varTypeIsFloating(cast->GetOp(0)->GetType()));
#ifndef TARGET_64BIT
    assert(!cast->TypeIs(TYP_LONG));
#endif

#ifdef TARGET_XARCH
    ContainCheckFloatToInt(cast);
#endif
}

void Lowering::LowerIntToFloat(GenTreeUnOp* cast)
{
    assert(cast->OperIs(GT_STOF, GT_UTOF) && varTypeIsFloating(cast->GetType()));
    assert(varTypeIsIntegral(cast->GetOp(0)->GetType()));
#ifndef TARGET_64BIT
    assert(!cast->GetOp(0)->TypeIs(TYP_LONG));
#endif

#ifdef TARGET_XARCH
    ContainCheckIntToFloat(cast);
#endif
}
#endif

void Lowering::LowerIndir(GenTreeIndir* ind)
{
    assert(ind->OperIs(GT_IND_LOAD, GT_NULLCHECK) && !ind->TypeIs(TYP_STRUCT));

    // TODO-Cleanup: We're passing isContainable = true but ContainCheckIndir rejects
    // address containment in some cases so we end up creating trivial (reg + offset)
    // or (reg + reg) LEAs that are not necessary.
    TryCreateAddrMode(ind->GetAddr(), true);
    ContainCheckIndir(ind);

    if (ind->OperIs(GT_NULLCHECK) || ind->IsUnusedValue())
    {
        TransformUnusedIndirection(ind);
    }
}

void Lowering::TransformUnusedIndirection(GenTreeIndir* ind)
{
    // A nullcheck is essentially the same as an indirection with no use.
    // The difference lies in whether a target register must be allocated.
    // On XARCH we can generate a compare with no target register as long as the address
    // is not contained.
    // On ARM64 we can generate a load to REG_ZR in all cases.
    // However, on ARM we must always generate a load to a register.
    // In the case where we require a target register, it is better to use IND_LOAD, since
    // NULLCHECK is a non-value node and would therefore require an internal register to
    // use as the target. That is non-optimal because it will be modeled as conflicting
    // with the source register(s).
    // So, to summarize:
    // - On ARM64, always use NULLCHECK for a dead indirection.
    // - On ARM, always use IND_LOAD.
    // - On XARCH, use IND_LOAD if we have a contained address, and NULLCHECK otherwise.
    // In all cases, change the type to TYP_INT.

    assert(ind->OperIs(GT_NULLCHECK, GT_IND_LOAD, GT_IND_LOAD_BLK, GT_IND_LOAD_OBJ));

    ind->SetType(TYP_INT);

#ifdef TARGET_ARM64
    bool useNullCheck = true;
#elif TARGET_ARM
    bool useNullCheck = false;
#else
    bool useNullCheck = !ind->GetAddr()->isContained();
#endif

    if (useNullCheck && !ind->OperIs(GT_NULLCHECK))
    {
        ind->ChangeOper(GT_NULLCHECK);
        ind->ClearUnusedValue();
    }
    else if (!useNullCheck && !ind->OperIs(GT_IND_LOAD))
    {
        ind->ChangeOper(GT_IND_LOAD);
        ind->SetUnusedValue();
    }
}

void Lowering::LowerIndStore(GenTreeIndStore* store)
{
    assert(!store->TypeIs(TYP_STRUCT));

    GenTree* value = store->GetValue();

    if (value->OperIs(GT_BITCAST))
    {
        GenTree* src = value->AsUnOp()->GetOp(0);

        if (varTypeUsesFloatReg(src->GetType()) != varTypeUsesFloatReg(store->GetType())
#ifndef TARGET_64BIT
            && !src->TypeIs(TYP_LONG)
#endif
                )
        {
            assert(varTypeSize(src->GetType()) == varTypeSize(store->GetType()));

            src->ClearContained();
            src->ClearRegOptional();
            store->SetType(src->GetType());
            store->SetValue(src);

            BlockRange().Unlink(value);
            value = src;
        }
    }

#ifndef WINDOWS_AMD64_ABI
    if (GenTreeCall* call = value->IsCall())
    {
        if (call->GetRegCount() > 1)
        {
            assert(varTypeIsSIMD(store->GetType()) && varTypeIsSIMD(call->GetType()));

            call->SetType(TYP_STRUCT);

            store->SetOper(GT_IND_STORE_OBJ);
            store->SetType(TYP_STRUCT);
            store->AsIndStoreObj()->SetLayout(call->GetRetLayout());

            LowerIndStoreObj(store->AsIndStoreObj());

            return;
        }
    }
#endif

    TryCreateAddrMode(store->GetAddr(), true);

    if (GCInfo::GetWriteBarrierForm(store) == GCInfo::WBF_NoBarrier)
    {
        LowerIndStoreArch(store);
    }
}

void Lowering::LowerIndStoreObj(GenTreeIndStoreObj* store)
{
    assert(store->TypeIs(TYP_STRUCT));

    GenTree*     value  = store->GetValue();
    ClassLayout* layout = store->GetLayout();

    if (GenTreeCall* call = value->IsCall())
    {
        if ((call->GetRegCount() == 1) && (varTypeSize(call->GetRegType(0)) <= layout->GetSize()))
        {
            call->SetType(call->GetRegType(0));

            store->SetOper(GT_IND_STORE);
            store->SetType(call->GetType());
            LowerIndStore(store->AsIndStore());

            return;
        }

        if (layout->GetSize() >= call->GetRetLayout()->GetSize())
        {
#if defined(UNIX_AMD64_ABI) || defined(TARGET_ARM64)
            if (layout->HasGCRef())
            {
                store->SetKind(StructStoreKind::UnrollRegsWB);
                ContainStructStoreAddressUnrollRegsWB(store->GetAddr());

                return;
            }
#endif

#if FEATURE_MULTIREG_RET
            if (!layout->HasGCRef())
            {
                store->SetKind(StructStoreKind::UnrollRegs);
                ContainStructStoreAddress(store, layout->GetSize(), store->GetAddr());

                return;
            }
#endif
        }

        store->SetValue(SpillStructCall(call, store));
    }
    else if (TryTransformStoreObjToStoreInd(store))
    {
        return;
    }

    StructStoreKind kind = GetStructStoreKind(false, layout, value);
    store->SetKind(kind);
    LowerStructStore(store, kind, layout);
}

void Lowering::LowerStructStore(GenTree* store, StructStoreKind kind, ClassLayout* layout)
{
    assert(store->OperIs(GT_IND_STORE_OBJ, GT_LCL_STORE, GT_LCL_STORE_FLD) && store->TypeIs(TYP_STRUCT));
    assert(!layout->IsBlockLayout());

    GenTree* dstAddr = nullptr;
    GenTree* src;

    if (!store->OperIs(GT_IND_STORE_OBJ))
    {
        src = store->AsLclRef()->GetOp(0);
    }
    else
    {
        dstAddr = store->AsIndStoreObj()->GetAddr();
        src     = store->AsIndStoreObj()->GetValue();

        assert(dstAddr->TypeIs(TYP_BYREF, TYP_I_IMPL));

#ifdef TARGET_XARCH
        TryCreateAddrMode(dstAddr, false);
#endif

        if ((kind == StructStoreKind::UnrollInit) || (kind == StructStoreKind::UnrollCopy))
        {
            ContainStructStoreAddress(store, layout->GetSize(), dstAddr);
        }
    }

    assert((src->OperIs(GT_IND_LOAD_OBJ, GT_LCL_LOAD, GT_LCL_LOAD_FLD) && src->TypeIs(TYP_STRUCT)) || src->IsIntCon(0));
    assert(!src->OperIs(GT_IND_LOAD_OBJ) || !src->AsIndLoadObj()->GetAddr()->isContained());

    if (src->TypeIs(TYP_STRUCT))
    {
        src->SetContained();
    }

    if (kind == StructStoreKind::UnrollInit)
    {
#ifdef TARGET_XARCH
        unsigned size = layout->GetSize();

        if (size == 1)
        {
            src->SetContained();
        }
        else if (size >= XMM_REGSIZE_BYTES)
        {
#ifdef TARGET_AMD64
            if ((size % 16 == 0) && (!store->IsIndStoreObj() || !layout->HasGCPtr()))
#else
            if (size % 8 == 0)
#endif
            {
                src->SetContained();
            }
        }
#elif defined(TARGET_ARM64)
        // Use REG_ZR as source on ARM64.
        src->SetContained();
#endif
    }
    else if (src->OperIs(GT_IND_LOAD_OBJ))
    {
        if (kind == StructStoreKind::UnrollCopy)
        {
            ContainStructStoreAddress(store, layout->GetSize(), src->AsIndLoadObj()->GetAddr());
        }
#ifdef TARGET_XARCH
        else
        {
            TryCreateAddrMode(src->AsIndLoadObj()->GetAddr(), false);
        }
#endif
    }
}

void Lowering::LowerIndStoreBlk(GenTreeIndStoreBlk* store)
{
    assert(store->TypeIs(TYP_STRUCT));

    GenTree* dstAddr = store->GetAddr();
    GenTree* src     = store->GetValue();
    unsigned size    = store->GetLayout()->GetSize();

    assert(size != 0);

#ifdef TARGET_XARCH
    TryCreateAddrMode(dstAddr, false);
#endif

    if (!src->OperIs(GT_IND_LOAD_BLK))
    {
        assert(src->OperIs(GT_INIT_VAL) || src->IsIntCon(0));

        if (src->OperIs(GT_INIT_VAL))
        {
            src->SetContained();
            src = src->AsUnOp()->GetOp(0);
        }

        if (size > INITBLK_UNROLL_LIMIT)
        {
            store->SetKind(StructStoreKind::LargeInit);
        }
        else if (!src->OperIs(GT_CNS_INT))
        {
#ifdef TARGET_XARCH
            // TODO-CQ: We could unroll even when the initialization value is not a constant
            // by inserting a MUL init, 0x01010101 instruction. We need to determine if the
            // extra latency that MUL introduces isn't worse that rep stosb. Likely not.

            // TODO-MIKE-Review: Why does x64 uses RepStos instead of MemSet like in the
            // constant case? RepStos/MemSet selection should depend only on size.
            store->SetKind(StructStoreKind::RepStos);
#else
            store->SetKind(StructStoreKind::MemSet);
#endif
        }
        else
        {
            store->SetKind(StructStoreKind::UnrollInit);

            // The fill value of an initblk is interpreted to hold a
            // value of (unsigned int8) however a constant of any size
            // may practically reside on the evaluation stack. So extract
            // the lower byte out of the initVal constant and replicate
            // it to a larger constant whose size is sufficient to support
            // the largest width store of the desired inline expansion.

            ssize_t fill = src->AsIntCon()->GetUInt8Value();

            if (fill == 0)
            {
#ifdef TARGET_XARCH
                // If the size is multiple of XMM register size there's no need to load 0 in a GPR,
                // codegen will use xorps to generate 0 directly in the temporary XMM register.
                if ((size % XMM_REGSIZE_BYTES) == 0)
                {
                    src->SetContained();
                }
#elif defined(TARGET_ARM64)
                // Use REG_ZR as source on ARM64.
                src->SetContained();
#endif
            }
#ifdef TARGET_64BIT
            else if (size >= 4)
            {
                fill *= 0x0101010101010101LL;
                src->SetType(TYP_LONG);
            }
#endif
            else
            {
                fill *= 0x01010101;
            }

            src->AsIntCon()->SetValue(fill);

            ContainStructStoreAddress(store, size, dstAddr);
        }
    }
    else
    {
        assert(src->OperIs(GT_IND_LOAD_BLK) && src->TypeIs(TYP_STRUCT));
        assert(!src->AsBlk()->GetAddr()->isContained());

        src->SetContained();

        if (size > CPBLK_UNROLL_LIMIT)
        {
            store->SetKind(StructStoreKind::LargeCopy);

#ifdef TARGET_XARCH
            if (src->OperIs(GT_IND_LOAD_BLK))
            {
                TryCreateAddrMode(src->AsBlk()->GetAddr(), false);
            }
#endif
        }
        else
        {
            store->SetKind(StructStoreKind::UnrollCopy);

            if (src->OperIs(GT_IND_LOAD_BLK))
            {
                ContainStructStoreAddress(store, size, src->AsBlk()->GetAddr());
            }

            ContainStructStoreAddress(store, size, dstAddr);
        }
    }
}

bool Lowering::TryTransformStoreObjToStoreInd(GenTreeIndStoreObj* store)
{
#if 0
    if (!comp->opts.OptimizationEnabled())
    {
        return false;
    }

    var_types regType = store->GetLayout()->GetRegisterType();

    if (regType == TYP_UNDEF)
    {
        return false;
    }

    if (varTypeIsSIMD(regType))
    {
        // TODO-CQ: support STORE_IND SIMD16(SIMD16, CNT_INT 0).
        return false;
    }

    if (varTypeIsGC(regType))
    {
        // TODO-CQ: STOREIND does not try to contain src if we need a barrier,
        // STORE_OBJ generates better code currently.
        return false;
    }

    GenTree* src = store->GetValue();

    if (varTypeIsSmall(regType) && !src->IsIntCon(0))
    {
        // source operand INDIR will use a widening instruction
        // and generate worse code, like `movzx` instead of `mov`
        // on x64.
        return false;
    }

    JITDUMP("Replacing STORE_OBJ with STOREIND for [06%u]", store->GetID());
    store->ChangeOper(GT_IND_STORE);
    store->SetType(regType);

    if (varTypeIsStruct(src->GetType()))
    {
        if (src->OperIs(GT_IND_LOAD_OBJ))
        {
            src->ChangeOper(GT_IND_LOAD);
        }

        src->SetType(regType);
        LowerNode(src);
    }
    else
    {
        assert(src->IsIntCon(0));

        src->SetType(varActualType(regType));
    }

    LowerStoreIndirCommon(store->AsStoreInd());

    return true;
#else
    return false;
#endif
}

#ifdef FEATURE_SIMD
bool Lowering::ContainSIMD12MemToMemCopy(GenTree* store, GenTree* value)
{
    assert(IsMemStore(store));
    assert(store->TypeIs(TYP_SIMD12));

    if ((varTypeSize(value->GetType()) < 12) || !IsMemOperand(value) || !IsSafeToMoveMemOperandForward(store, value))
    {
        return false;
    }

    value->SetContained();

    if (value->OperIs(GT_IND_LOAD))
    {
        GenTree* addr = value->AsIndLoad()->GetAddr();

        if (addr->isContained() && (!addr->IsAddrMode() || !IsSafeToMoveAddrModeForward(store, addr->AsAddrMode())))
        {
            addr->ClearContained();
        }
    }

    return true;
}
#endif

#ifdef FEATURE_HW_INTRINSICS
LclVarDsc* Lowering::GetSimdMemoryTemp(var_types type)
{
#if defined(TARGET_XARCH)
    assert((type == TYP_SIMD16) || (type == TYP_SIMD32));
    LclVarDsc*& tempLcl = type == TYP_SIMD32 ? m_simd32MemoryTempLcl : m_simd16MemoryTempLcl;
#elif defined(TARGET_ARM64)
    assert((type == TYP_SIMD16) || (type == TYP_SIMD8));
    LclVarDsc*& tempLcl = type == TYP_SIMD8 ? m_simd8MemoryTempLcl : m_simd16MemoryTempLcl;
#endif

    if (tempLcl == nullptr)
    {
        LclVarDsc* lcl = comp->lvaAllocTemp(false DEBUGARG("Vector GetElement temp"));
        lcl->lvType    = type;
        comp->lvaSetDoNotEnregister(lcl DEBUGARG(Compiler::DNER_LocalField));

        tempLcl = lcl;
    }

    return tempLcl;
}

GenTree* Lowering::TryRemoveCastIfPresent(var_types expectedType, GenTree* op)
{
    if (op->OperIs(GT_CONV) && varTypeIsIntegral(expectedType))
    {
        GenTree* castOp = op->AsUnOp()->GetOp(0);

        assert(varTypeIsIntegral(castOp->GetType()));

        if (varTypeSize(op->GetType()) < varTypeSize(expectedType))
        {
            return op;
        }

        BlockRange().Unlink(op);
        castOp->ClearContained();
        return castOp;
    }

    return op;
}

bool Lowering::VectorConstant::AllBitsZero(var_types type) const
{
    assert(varTypeIsTargetVec(type));

    for (unsigned i = 0; i < varTypeSize(type); i++)
    {
        if (u8[i] != 0)
        {
            return false;
        }
    }
    return true;
}

bool Lowering::VectorConstant::AllBitsOne(var_types type) const
{
    assert(varTypeIsTargetVec(type));

    for (unsigned i = 0; i < varTypeSize(type); i++)
    {
        if (u8[i] != 0xFF)
        {
            return false;
        }
    }
    return true;
}

bool Lowering::VectorConstant::Insert(var_types type, int index, GenTree* value)
{
    if (GenTreeIntCon* icon = value->IsIntCon())
    {
        switch (type)
        {
            case TYP_BYTE:
            case TYP_UBYTE:
                u8[index] = icon->GetUInt8Value();
                return true;
            case TYP_SHORT:
            case TYP_USHORT:
                u16[index] = icon->GetUInt16Value();
                return true;
            case TYP_INT:
                u32[index] = icon->GetUInt32Value();
                return true;
#ifdef TARGET_64BIT
            case TYP_LONG:
                u64[index] = icon->GetUInt64Value();
                return true;
#endif
            default:
                return false;
        }
    }

    if (GenTreeDblCon* dcon = value->IsDblCon())
    {
        if (type == TYP_FLOAT)
        {
            u32[index] = dcon->GetFloatBits();
        }
        else
        {
            u64[index] = dcon->GetDoubleBits();
        }

        return true;
    }

#ifndef TARGET_64BIT
    if (value->OperIs(GT_LONG) && value->AsOp()->GetOp(0)->IsIntCon() && value->AsOp()->GetOp(1)->IsIntCon())
    {
        uint64_t loBits = value->AsOp()->GetOp(0)->AsIntCon()->GetUInt32Value();
        uint64_t hiBits = value->AsOp()->GetOp(1)->AsIntCon()->GetUInt32Value();
        u64[index]      = (hiBits << 32) | loBits;
        return true;
    }
#endif

    return false;
}

bool Lowering::VectorConstant::Pack(GenTreeHWIntrinsic* create)
{
    unsigned  numOps  = create->GetNumOps();
    var_types eltType = create->GetSimdBaseType();

    for (unsigned i = 0; i < numOps; i++)
    {
        if (!Insert(eltType, i, create->GetOp(i)))
        {
            return false;
        }
    }

    return true;
}

bool Lowering::VectorConstant::Splat(GenTreeHWIntrinsic* create)
{
    var_types eltType = create->GetSimdBaseType();
    GenTree*  op1     = create->GetOp(0);

    if (!Insert(eltType, 0, op1))
    {
        return false;
    }

    unsigned eltSize  = varTypeSize(eltType);
    unsigned eltCount = varTypeSize(create->GetType()) / eltSize;

    for (unsigned i = 1; i < eltCount; i++)
    {
        switch (eltSize)
        {
            case 1:
                u8[i] = u8[0];
                break;
            case 2:
                u16[i] = u16[0];
                break;
            case 4:
                u32[i] = u32[0];
                break;
            default:
                assert(eltSize == 8);
                u64[i] = u64[0];
                break;
        }
    }

    return true;
}
#endif

bool Lowering::IsMemStore(GenTree* node)
{
    if (node->OperIs(GT_IND_STORE, GT_LCL_STORE_FLD))
    {
        return true;
    }

    if (node->OperIs(GT_LCL_STORE))
    {
        return node->AsLclStore()->GetLcl()->lvDoNotEnregister;
    }

    return false;
}

bool Lowering::IsMemOperand(GenTree* node)
{
    if (node->OperIs(GT_IND_LOAD, GT_LCL_LOAD_FLD))
    {
        return true;
    }

    if (node->OperIs(GT_LCL_LOAD))
    {
        return node->AsLclLoad()->GetLcl()->lvDoNotEnregister;
    }

    return false;
}

PhaseStatus Compiler::phLower()
{
    Lowering lowering(this);
    lowering.Run();
    return PhaseStatus::MODIFIED_EVERYTHING;
}
