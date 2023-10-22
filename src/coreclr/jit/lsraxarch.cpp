// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_XARCH

#include "lsra.h"

void LinearScan::BuildNode(GenTree* tree)
{
    assert(!tree->isContained());

    switch (tree->GetOper())
    {
        case GT_LCL_VAR:
        case GT_LCL_FLD:
            assert(!compiler->lvaGetDesc(tree->AsLclVarCommon())->IsRegCandidate());

#ifdef FEATURE_SIMD
            // Need an additional register to read upper 4 bytes of Vector3.
            if (tree->TypeIs(TYP_SIMD12))
            {
                // We need an internal register different from targetReg in which 'interlocked'
                // produces its result because both targetReg and internal reg will be in
                // use at the same time.
                BuildInternalFloatDef(tree, allSIMDRegs());
                setInternalRegsDelayFree = true;
                BuildInternalUses();
            }
#endif
            BuildDef(tree);
            break;

        case GT_STORE_LCL_VAR:
            BuildStoreLclVar(tree->AsLclVar());
            break;

        case GT_STORE_LCL_FLD:
            BuildStoreLclFld(tree->AsLclFld());
            break;

        case GT_START_PREEMPTGC:
            BuildKills(tree, RBM_NONE);
            break;

        case GT_PROF_HOOK:
            BuildKills(tree, getKillSetForProfilerHook());
            break;

        case GT_CNS_INT:
        case GT_CNS_LNG:
        case GT_CNS_DBL:
            assert(!tree->IsReuseRegVal());
            BuildDef(tree)->getInterval()->isConstant = true;
            break;

        case GT_RETURN:
            BuildReturn(tree->AsUnOp());
            BuildKills(tree, getKillSetForReturn());
            break;

        case GT_RETFILT:
            if (!tree->TypeIs(TYP_VOID))
            {
                assert(tree->TypeIs(TYP_INT));
                BuildUse(tree->AsUnOp()->GetOp(0), RBM_INTRET);
            }
            break;

        case GT_KEEPALIVE:
            BuildKeepAlive(tree->AsUnOp());
            break;

        case GT_SETCC:
#ifdef TARGET_X86
            BuildDef(tree, allByteRegs());
#else
            BuildDef(tree);
#endif
            break;

        case GT_SWITCH_TABLE:
            BuildInternalIntDef(tree);
            BuildUse(tree->AsOp()->GetOp(0));
            BuildUse(tree->AsOp()->GetOp(1));
            BuildInternalUses();
            break;

        case GT_BT:
            BuildUse(tree->AsOp()->GetOp(0));
            BuildUse(tree->AsOp()->GetOp(1));
            break;

        case GT_FADD:
        case GT_FSUB:
        case GT_FMUL:
        case GT_FDIV:
            if (compiler->canUseVexEncoding())
            {
                BuildOperandUses(tree->AsOp()->GetOp(0));
                BuildOperandUses(tree->AsOp()->GetOp(1));
                BuildDef(tree);
                break;
            }
            FALLTHROUGH;
#ifndef TARGET_64BIT
        case GT_ADD_LO:
        case GT_ADD_HI:
        case GT_SUB_LO:
        case GT_SUB_HI:
#endif
        case GT_ADD:
        case GT_SUB:
        case GT_AND:
        case GT_OR:
        case GT_XOR:
            BuildRMWUses(tree->AsOp());
            FALLTHROUGH;
        case GT_JMPTABLE:
        case GT_LCL_ADDR:
        case GT_CLS_VAR_ADDR:
        case GT_PHYSREG:
        case GT_LABEL:
            BuildDef(tree);
            FALLTHROUGH;
        case GT_NOP:
        case GT_NO_OP:
        case GT_IL_OFFSET:
        case GT_START_NONGC:
        case GT_PINVOKE_PROLOG:
        case GT_MEMORYBARRIER:
        case GT_JTRUE:
        case GT_JCC:
        case GT_JMP:
#ifndef FEATURE_EH_FUNCLETS
        case GT_END_LFIN:
#endif
            break;

        case GT_LOCKADD:
            BuildUse(tree->AsOp()->GetOp(0));

            if (!tree->AsOp()->GetOp(1)->IsContainedIntCon())
            {
                BuildUse(tree->AsOp()->GetOp(1));
            }
            break;

        case GT_RETURNTRAP:
            // TODO-MIKE-Review: This internal def occurs after the use.
            BuildInternalIntDef(tree);
            assert(tree->AsUnOp()->GetOp(0)->isContained());
            BuildAddrUses(tree->AsUnOp()->GetOp(0)->AsIndir()->GetAddr());
            BuildInternalUses();
            BuildKills(tree, compiler->compHelperCallKillSet(CORINFO_HELP_STOP_FOR_GC));
            break;

        case GT_MOD:
        case GT_DIV:
        case GT_UMOD:
        case GT_UDIV:
            BuildModDiv(tree->AsOp());
            break;

        case GT_MUL:
            BuildMul(tree->AsOp());
            break;

        case GT_MULHI:
#ifdef TARGET_X86
        case GT_MUL_LONG:
#endif
            BuildMulLong(tree->AsOp());
            break;

        case GT_INTRINSIC:
            BuildIntrinsic(tree->AsIntrinsic());
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            BuildHWIntrinsic(tree->AsHWIntrinsic());
            break;
#endif

        case GT_CAST:
            BuildCast(tree->AsCast());
            break;

        case GT_BITCAST:
            if (!tree->AsUnOp()->GetOp(0)->isContained())
            {
                BuildUse(tree->AsUnOp()->GetOp(0));
            }
            BuildDef(tree);
            break;

        case GT_FNEG:
            // TODO-MIKE-Review: Where is this internal reg used???
            BuildInternalFloatDef(tree, internalFloatRegCandidates());
            BuildUse(tree->AsUnOp()->GetOp(0));
            BuildInternalUses();
            BuildDef(tree);
            break;

        case GT_NEG:
        case GT_NOT:
        case GT_BSWAP:
        case GT_BSWAP16:
        case GT_INC_SATURATE:
            BuildUse(tree->AsUnOp()->GetOp(0));
            BuildDef(tree);
            break;

        case GT_LSH:
        case GT_RSH:
        case GT_RSZ:
        case GT_ROL:
        case GT_ROR:
#ifdef TARGET_X86
        case GT_LSH_HI:
        case GT_RSH_LO:
#endif
            BuildShiftRotate(tree->AsOp());
            break;

        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GE:
        case GT_GT:
        case GT_TEST_EQ:
        case GT_TEST_NE:
        case GT_CMP:
            BuildCmp(tree->AsOp());
            break;

        case GT_CKFINITE:
            // TODO-MIKE-Review: This internal def occurs after the use, though it
            // should not matter since it's an integer register and the use is float.
            BuildInternalIntDef(tree);
            BuildUse(tree->AsUnOp()->GetOp(0));
            BuildInternalUses();
            BuildDef(tree);
            break;

        case GT_CMPXCHG:
            BuildCmpXchg(tree->AsCmpXchg());
            break;

        case GT_XORR:
        case GT_XAND:
        case GT_XADD:
        case GT_XCHG:
            BuildInterlocked(tree->AsOp());
            break;

        case GT_PUTARG_REG:
            BuildPutArgReg(tree->AsUnOp());
            break;

        case GT_CALL:
            BuildCall(tree->AsCall());
            break;

        case GT_PUTARG_STK:
            BuildPutArgStk(tree->AsPutArgStk());
            break;

        case GT_STORE_BLK:
        case GT_STORE_OBJ:
            BuildStructStore(tree->AsBlk(), tree->AsBlk()->GetKind(), tree->AsBlk()->GetLayout());
            break;

        case GT_COPY_BLK:
        case GT_INIT_BLK:
            BuildStoreDynBlk(tree->AsDynBlk());
            break;

        case GT_LCLHEAP:
            BuildLclHeap(tree->AsUnOp());
            break;

        case GT_BOUNDS_CHECK:
            BuildBoundsChk(tree->AsBoundsChk());
            break;

        case GT_ARR_INDEX:
            assert(!tree->AsArrIndex()->ArrObj()->isContained());
            assert(!tree->AsArrIndex()->IndexExpr()->isContained());
            // The lifetime of the arrObj must be extended because it is
            // used multiple times while the result is being computed.
            setDelayFree(BuildUse(tree->AsArrIndex()->ArrObj()));
            BuildUse(tree->AsArrIndex()->IndexExpr());
            BuildDef(tree);
            break;

        case GT_ARR_OFFSET:
            if (!tree->AsArrOffs()->GetOp(0)->isContained())
            {
                BuildInternalIntDef(tree);
                BuildUse(tree->AsArrOffs()->GetOp(0));
            }

            BuildUse(tree->AsArrOffs()->GetOp(1));
            BuildUse(tree->AsArrOffs()->GetOp(2));
            BuildInternalUses();
            BuildDef(tree);
            break;

        case GT_LEA:
            BuildAddrMode(tree->AsAddrMode());
            break;

        case GT_STOREIND:
            if (GCInfo::GetWriteBarrierForm(tree->AsStoreInd()) != GCInfo::WBF_NoBarrier)
            {
                BuildGCWriteBarrier(tree->AsStoreInd());
            }
            else
            {
                BuildStoreInd(tree->AsStoreInd());
            }
            break;

        case GT_NULLCHECK:
            BuildUse(tree->AsUnOp()->GetOp(0));
            break;

        case GT_IND:
            BuildLoadInd(tree->AsIndir());
            break;

        case GT_CATCH_ARG:
            BuildDef(tree, RBM_EXCEPTION_OBJECT);
            break;

        case GT_INDEX_ADDR:
#ifdef TARGET_64BIT
            // On 64-bit we always need a temporary register:
            //   - if the index is `native int` then we need to load the array
            //     length into a register to widen it to `native int`
            //   - if the index is `int` (or smaller) then we need to widen
            //     it to `long` to perform the address calculation
            BuildInternalIntDef(tree);
#else
            assert(!varTypeIsLong(tree->AsIndexAddr()->GetIndex()->GetType()));

            switch (tree->AsIndexAddr()->GetElemSize())
            {
                case 1:
                case 2:
                case 4:
                case 8:
                    break;
                default:
                    BuildInternalIntDef(tree);
                    break;
            }
#endif
            BuildRMWUses(tree->AsOp());
            BuildInternalUses();
            BuildDef(tree);
            break;

        case GT_INSTR:
            BuildInstr(tree->AsInstr());
            break;

        default:
            unreached();
    }
}

void LinearScan::BuildAddrMode(GenTreeAddrMode* lea)
{
    if (GenTree* base = lea->GetBase())
    {
        BuildUse(base);
    }

    if (GenTree* index = lea->GetIndex())
    {
        BuildUse(index);
    }

    BuildDef(lea);
}

void LinearScan::BuildCmpXchg(GenTreeCmpXchg* cmpxchg)
{
    BuildUse(cmpxchg->GetOp(0), allRegs(TYP_INT) & ~RBM_RAX);
    BuildUse(cmpxchg->GetOp(1), allRegs(TYP_INT) & ~RBM_RAX);
    BuildUse(cmpxchg->GetOp(2), RBM_RAX);
    BuildDef(cmpxchg, RBM_RAX);
}

void LinearScan::BuildInterlocked(GenTreeOp* interlocked)
{
    GenTree* addr  = interlocked->GetOp(0);
    GenTree* value = interlocked->GetOp(1);
    assert(!addr->isContained());
    RefPosition* addrUse = BuildUse(addr);
    setDelayFree(addrUse);
    tgtPrefUse = addrUse;
    assert(!value->isContained());
    BuildUse(value);
    BuildDef(interlocked);
}

void LinearScan::BuildOperandUses(GenTree* node, regMaskTP candidates)
{
    if (!node->isContained())
    {
        BuildUse(node, candidates);
    }
#ifndef TARGET_64BIT
    else if (node->OperIs(GT_LONG))
    {
        BuildUse(node->AsOp()->GetOp(0));
        BuildUse(node->AsOp()->GetOp(1));
    }
#endif
    else if (node->OperIsIndir())
    {
        BuildAddrUses(node->AsIndir()->GetAddr(), candidates);
    }
    else if (node->OperIs(GT_LEA))
    {
        BuildAddrUses(node, candidates);
    }
#ifdef FEATURE_HW_INTRINSICS
    else if (GenTreeHWIntrinsic* hwi = node->IsHWIntrinsic())
    {
        if (hwi->OperIsMemoryLoad())
        {
            BuildAddrUses(hwi->GetOp(0));
        }
        // TODO-MIKE-Review: What is this for?
        else if (hwi->GetNumOps() >= 1)
        {
            BuildUse(hwi->GetOp(0), candidates);
        }
    }
#endif // FEATURE_HW_INTRINSICS
}

#ifdef DEBUG
// Check for instructions that use the read/modify/write register format (e.g. ADD eax, 42).
bool LinearScan::isRMWRegOper(GenTreeOp* tree)
{
    switch (tree->GetOper())
    {
        case GT_FADD:
        case GT_FSUB:
        case GT_FMUL:
        case GT_FDIV:
            return !compiler->canUseVexEncoding();

#ifdef TARGET_X86
        case GT_ADD_LO:
        case GT_ADD_HI:
        case GT_SUB_LO:
        case GT_SUB_HI:
#endif
        case GT_ADD:
        case GT_SUB:
        case GT_AND:
        case GT_OR:
        case GT_XOR:
        // TODO-MIKE-Review: Given the very specific register constraints MUL has,
        // does it really need to be treated as RMW or will some special casing do?
        case GT_MULHI:
#ifdef TARGET_X86
        case GT_MUL_LONG:
#endif
        // TODO-MIKE-Review: INDEX_ADDR isn't RMW but old code was bogus and removing this causes diffs.
        case GT_INDEX_ADDR:
            return true;

        case GT_MUL:
            return !tree->GetOp(0)->IsContainedIntCon() && !tree->GetOp(1)->IsContainedIntCon();

        default:
            return false;
    }
}
#endif // DEBUG

void LinearScan::BuildRMWUses(GenTreeOp* node)
{
    assert(isRMWRegOper(node));

    GenTree*  op1           = node->GetOp(0);
    GenTree*  op2           = node->GetOp(1);
    regMaskTP op1Candidates = RBM_NONE;
    regMaskTP op2Candidates = RBM_NONE;

#ifdef TARGET_X86
    if (varTypeIsByte(node->GetType()))
    {
        regMaskTP byteCandidates = allByteRegs();

        if (!op1->isContained())
        {
            assert(byteCandidates != RBM_NONE);
            op1Candidates = byteCandidates;
        }

        if (node->OperIsCommutative() && !op2->isContained())
        {
            assert(byteCandidates != RBM_NONE);
            op2Candidates = byteCandidates;
        }
    }
#endif // TARGET_X86

    bool prefOp1 = !op1->isContained();
    bool prefOp2 = node->OperIsCommutative() && !op2->isContained();

    // Determine which operand, if any, should be delayRegFree. Normally, this would be op2,
    // but if we have a commutative operator and op1 is a contained memory op, it would be op1.
    // We need to make the delayRegFree operand remain live until the op is complete, by marking
    // the source(s) associated with op2 as "delayFree".
    // Note that if op2 of a binary RMW operator is a memory op, even if the operator
    // is commutative, codegen cannot reverse them.
    // TODO-XArch-CQ: This is not actually the case for all RMW binary operators, but there's
    // more work to be done to correctly reverse the operands if they involve memory
    // operands.  Also, we may need to handle more cases than GT_IND, especially once
    // we've modified the register allocator to not require all nodes to be assigned
    // a register (e.g. a spilled lclVar can often be referenced directly from memory).
    // Note that we may have a null op2, even with 2 sources, if op1 is a base/index memory op.
    GenTree* delayUseOperand = op2;
    if (node->OperIsCommutative())
    {
        if (op1->isContained() && op2 != nullptr)
        {
            delayUseOperand = op1;
        }
        else if (!op2->isContained() || op2->IsIntCon())
        {
            // If we have a commutative operator and op2 is not a memory op, we don't need
            // to set delayRegFree on either operand because codegen can swap them.
            delayUseOperand = nullptr;
        }
    }
    else if (op1->isContained())
    {
        delayUseOperand = nullptr;
    }

    if (delayUseOperand != nullptr)
    {
        assert(!prefOp1 || delayUseOperand != op1);
        assert(!prefOp2 || delayUseOperand != op2);
    }

    if (prefOp1)
    {
        assert(!op1->isContained());
        tgtPrefUse = BuildUse(op1, op1Candidates);
    }
    else if (delayUseOperand == op1)
    {
        BuildDelayFreeUses(op1, op2, op1Candidates);
    }
    else
    {
        BuildOperandUses(op1, op1Candidates);
    }

    if (prefOp2)
    {
        assert(!op2->isContained());
        tgtPrefUse2 = BuildUse(op2, op2Candidates);
    }
    else if (delayUseOperand == op2)
    {
        BuildDelayFreeUses(op2, op1, op2Candidates);
    }
    else
    {
        BuildOperandUses(op2, op2Candidates);
    }
}

void LinearScan::BuildDelayFreeUses(GenTree* node, GenTree* rmwNode, regMaskTP candidates)
{
    Interval* rmwInterval  = nullptr;
    bool      rmwIsLastUse = false;

    if ((rmwNode != nullptr) && isCandidateLclVar(rmwNode))
    {
        rmwInterval = getIntervalForLocalVarNode(rmwNode->AsLclVar());
        // Note: we don't handle multi-reg vars here. It's not clear that there
        // are any cases where we'd encounter a multi-reg var in an RMW context.
        assert(!rmwNode->AsLclVar()->IsMultiReg());
        rmwIsLastUse = rmwNode->AsLclVar()->IsLastUse(0);
    }

    auto BuildDelayFreeUse = [this, rmwInterval, rmwIsLastUse](GenTree* operand, regMaskTP candidates) {
        RefPosition* use = BuildUse(operand, candidates);

        if ((use->getInterval() != rmwInterval) || (!rmwIsLastUse && !use->lastUse))
        {
            setDelayFree(use);
        }
    };

    if (!node->isContained())
    {
        BuildDelayFreeUse(node, candidates);

        return;
    }

#ifdef FEATURE_HW_INTRINSICS
    if (GenTreeHWIntrinsic* hwIntrinsicNode = node->IsHWIntrinsic())
    {
        BuildDelayFreeUse(hwIntrinsicNode->GetOp(0), candidates);

        return;
    }
#endif

    if (GenTreeIndir* indir = node->IsIndir())
    {
        GenTree* addr = indir->GetAddr();

        if (!addr->isContained())
        {
            // TODO-MIKE-Review: Using "candidates" here and below is likely bogus.
            // The caller usually cares only about the case of a non contained
            // operand, it doesn't know or care about whatever registers an address
            // mode needs. Some callers pass candidates such as XMM0 or "byte regs"
            // on x86...

            BuildDelayFreeUse(addr, candidates);
        }
        else if (GenTreeAddrMode* const addrMode = addr->IsAddrMode())
        {
            if (GenTree* base = addrMode->GetBase())
            {
                BuildDelayFreeUse(base, candidates);
            }

            if (GenTree* index = addrMode->GetIndex())
            {
                BuildDelayFreeUse(index, candidates);
            }
        }

        return;
    }
}

void LinearScan::BuildShiftRotate(GenTreeOp* tree)
{
    GenTree*  shiftBy       = tree->GetOp(1);
    GenTree*  source        = tree->GetOp(0);
    regMaskTP srcCandidates = RBM_NONE;
    regMaskTP dstCandidates = RBM_NONE;

    if (shiftBy->isContained())
    {
        assert(shiftBy->IsIntCon());
    }
    else
    {
        srcCandidates = allRegs(TYP_INT) & ~RBM_RCX;
        dstCandidates = allRegs(TYP_INT) & ~RBM_RCX;
    }

#ifdef TARGET_X86
    // The first operand of a GT_LSH_HI and GT_RSH_LO oper is a GT_LONG so that
    // we can have a three operand form.
    if (tree->OperIs(GT_LSH_HI, GT_RSH_LO))
    {
        assert(source->OperIs(GT_LONG) && source->isContained());

        GenTree* sourceLo = source->AsOp()->GetOp(0);
        GenTree* sourceHi = source->AsOp()->GetOp(1);

        assert(!sourceLo->isContained() && !sourceHi->isContained());

        RefPosition* sourceLoUse = BuildUse(sourceLo, srcCandidates);
        RefPosition* sourceHiUse = BuildUse(sourceHi, srcCandidates);

        if (tree->OperIs(GT_LSH_HI))
        {
            setDelayFree(sourceLoUse);
        }
        else
        {
            setDelayFree(sourceHiUse);
        }
    }
    else
#endif
    {
        tgtPrefUse = BuildUse(source, srcCandidates);
    }

    if (!shiftBy->IsContainedIntCon())
    {
        BuildDelayFreeUses(shiftBy, source, RBM_RCX);
        buildKillPositionsForNode(tree, currentLoc + 1, RBM_RCX);
    }

    BuildDef(tree, dstCandidates);
}

void LinearScan::BuildCall(GenTreeCall* call)
{
#ifdef WINDOWS_AMD64_ABI
    bool varargsHasFloatRegArgs = false;

    if (call->IsVarargs())
    {
        // We will need an internal int reg for any float arguments to a varArgs call.
        for (GenTreeCall::Use& use : call->LateArgs())
        {
            GenTree* argNode = use.GetNode();

            if (argNode->OperIs(GT_PUTARG_REG))
            {
                varargsHasFloatRegArgs |= HandleFloatVarArgs(call, argNode);

                continue;
            }

            if (argNode->OperIs(GT_FIELD_LIST))
            {
                for (GenTreeFieldList::Use& use : argNode->AsFieldList()->Uses())
                {
                    varargsHasFloatRegArgs |= HandleFloatVarArgs(call, use.GetNode());
                }

                continue;
            }
        }
    }
#endif // WINDOWS_AMD64_ABI

    for (GenTreeCall::Use& arg : call->LateArgs())
    {
        GenTree* argNode = arg.GetNode();

        INDEBUG(CallArgInfo* argInfo = call->GetArgInfoByArgNode(argNode);)

        if (argNode->OperIs(GT_PUTARG_STK))
        {
            assert(argInfo->GetRegCount() == 0);
            assert(!argNode->isContained());

            continue;
        }

#ifdef UNIX_AMD64_ABI
        if (argNode->OperIs(GT_FIELD_LIST))
        {
            assert(argNode->isContained());

            unsigned regIndex = 0;
            for (GenTreeFieldList::Use& use : argNode->AsFieldList()->Uses())
            {
                assert(use.GetNode()->GetRegNum() == argInfo->GetRegNum(regIndex));

                BuildUse(use.GetNode(), genRegMask(use.GetNode()->GetRegNum()));
                regIndex++;
            }

            continue;
        }
#endif

        assert(argNode->OperIs(GT_PUTARG_REG));
        assert(argNode->GetRegNum() == argInfo->GetRegNum());

        BuildUse(argNode, genRegMask(argNode->GetRegNum()));
    }

    GenTree* ctrlExpr = call->IsIndirectCall() ? call->gtCallAddr : call->gtControlExpr;

    if (ctrlExpr != nullptr)
    {
        regMaskTP ctrlExprCandidates = RBM_NONE;

        // In case of fast tail implemented as jmp, make sure that gtControlExpr is
        // computed into a register.
        if (call->IsFastTailCall())
        {
            assert(!ctrlExpr->isContained());
            // Fast tail call - make sure that call target is always computed in RAX
            // so that epilog sequence can generate "jmp rax" to achieve fast tail call.
            ctrlExprCandidates = RBM_RAX;
        }
#ifdef TARGET_X86
        else if (call->IsVirtualStub() && call->IsIndirectCall())
        {
            // On x86, we need to generate a very specific pattern for indirect VSD calls:
            //
            //    3-byte nop
            //    call dword ptr [eax]
            //
            // Where EAX is also used as an argument to the stub dispatch helper. Make
            // sure that the call target address is computed into EAX in this case.
            assert(ctrlExpr->OperIs(GT_IND) && ctrlExpr->isContained());
            ctrlExprCandidates = RBM_VIRTUAL_STUB_TARGET;
        }
#endif // TARGET_X86

#ifdef WINDOWS_AMD64_ABI
        // If it is a fast tail call, it is already preferenced to use RAX.
        // Therefore, no need set src candidates on call tgt again.
        if (varargsHasFloatRegArgs && !call->IsFastTailCall())
        {
            // Don't assign the call target to any of the argument registers because
            // we will use them to also pass floating point arguments as required
            // by win-x64 ABI.
            ctrlExprCandidates = allRegs(TYP_INT) & ~RBM_ARG_REGS;
        }
#endif

        if (ctrlExpr->isContained())
        {
            BuildAddrUses(ctrlExpr->AsIndir()->GetAddr(), ctrlExprCandidates);
        }
        else
        {
            BuildUse(ctrlExpr, ctrlExprCandidates);
        }
    }

    BuildInternalUses();
    BuildKills(call, getKillSetForCall(call));

#ifdef TARGET_X86
    if (call->IsHelperCall(compiler, CORINFO_HELP_INIT_PINVOKE_FRAME))
    {
        BuildDef(call, RBM_PINVOKE_TCB);
    }
    else
#endif
        if (call->HasMultiRegRetVal() || varTypeIsStruct(call->GetType()))
    {
        for (unsigned i = 0; i < call->GetRegCount(); i++)
        {
            BuildDef(call, call->GetRegType(i), genRegMask(call->GetRetDesc()->GetRegNum(i)), i);
        }
    }
    else if (varTypeUsesFloatReg(call->GetType()))
    {
#ifdef TARGET_X86
        // The return value will be on the X87 stack, and we will need to move it.
        BuildDef(call);
#else
        BuildDef(call, RBM_FLOATRET);
#endif
    }
    else if (!call->TypeIs(TYP_VOID))
    {
        BuildDef(call, RBM_INTRET);
    }
}

#ifdef WINDOWS_AMD64_ABI
bool LinearScan::HandleFloatVarArgs(GenTreeCall* call, GenTree* argNode)
{
    assert(call->IsVarargs());

    if (!varTypeIsFloating(argNode->GetType()))
    {
        return false;
    }

    regNumber floatReg = argNode->GetRegNum();
    regNumber intReg   = MapVarargsParamFloatRegToIntReg(floatReg);

    BuildInternalIntDef(call, genRegMask(intReg));

    return true;
}
#endif

void LinearScan::BuildStructStore(GenTree* store, StructStoreKind kind, ClassLayout* layout)
{
#ifdef UNIX_AMD64_ABI
    if (kind == StructStoreKind::UnrollRegsWB)
    {
        BuildStructStoreUnrollRegsWB(store->AsObj(), layout);

        return;
    }
#endif

    GenTree* dstAddr = nullptr;
    GenTree* src;

    if (store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
    {
        src = store->AsLclVarCommon()->GetOp(0);
    }
    else
    {
        dstAddr = store->AsBlk()->GetAddr();
        src     = store->AsBlk()->GetValue();
    }

    unsigned size = layout->GetSize();

    GenTree* srcAddrOrFill = nullptr;

#if FEATURE_MULTIREG_RET
    if (kind == StructStoreKind::UnrollRegs)
    {
        assert(src->IsCall());
    }
    else
#endif
        if (src->OperIs(GT_INIT_VAL, GT_CNS_INT))
    {
        if (src->OperIs(GT_INIT_VAL))
        {
            assert(src->isContained());
            src = src->AsUnOp()->GetOp(0);
        }

        srcAddrOrFill = src;
    }
    else if (src->OperIs(GT_IND, GT_OBJ, GT_BLK))
    {
        assert(src->isContained());
        srcAddrOrFill = src->AsIndir()->GetAddr();
    }
    else
    {
        assert(src->OperIs(GT_LCL_VAR, GT_LCL_FLD));
        assert(src->isContained());
    }

    regMaskTP dstAddrRegMask = RBM_NONE;
    regMaskTP srcRegMask     = RBM_NONE;
    regMaskTP sizeRegMask    = RBM_NONE;
#ifdef TARGET_X86
    RefPosition* internalByteDef = nullptr;
#endif

    switch (kind)
    {
#if FEATURE_MULTIREG_RET
        case StructStoreKind::UnrollRegs:
            break;
#endif

        case StructStoreKind::UnrollInit:
            if ((size >= XMM_REGSIZE_BYTES)
#ifdef TARGET_AMD64
                && (!store->IsObj() || !layout->HasGCPtr())
#endif
                    )
            {
                BuildInternalFloatDef(store, internalFloatRegCandidates());
                SetContainsAVXFlags();
            }

#ifdef TARGET_X86
            if ((size & 1) != 0)
            {
                // We'll need to store a byte so a byte register is needed on x86.
                srcRegMask = allByteRegs();
            }
#endif
            break;

        case StructStoreKind::UnrollCopy:
            if (size >= XMM_REGSIZE_BYTES)
            {
                BuildInternalFloatDef(store, internalFloatRegCandidates());
                SetContainsAVXFlags();
            }

#ifdef TARGET_X86
            if ((size & 1) != 0)
            {
                // We'll need to store a byte so a byte register is needed on x86.
                internalByteDef = BuildInternalIntDef(store, allByteRegs());
            }
            else
#endif
                if ((size % XMM_REGSIZE_BYTES) != 0)
            {
                BuildInternalIntDef(store);
            }
            break;

        case StructStoreKind::UnrollCopyWBRepMovs:
            sizeRegMask = RBM_RCX;
            FALLTHROUGH;
        case StructStoreKind::UnrollCopyWB:
            dstAddrRegMask = RBM_RDI;
            srcRegMask     = RBM_RSI;
            break;

        case StructStoreKind::RepStos:
            assert(!src->isContained());
            dstAddrRegMask = RBM_RDI;
            srcRegMask     = RBM_RAX;
            sizeRegMask    = RBM_RCX;
            break;

        case StructStoreKind::RepMovs:
            dstAddrRegMask = RBM_RDI;
            srcRegMask     = RBM_RSI;
            sizeRegMask    = RBM_RCX;
            break;

#ifdef TARGET_AMD64
        case StructStoreKind::MemSet:
            assert(!src->isContained());
            FALLTHROUGH;
        case StructStoreKind::MemCpy:
            dstAddrRegMask = RBM_ARG_0;
            srcRegMask     = RBM_ARG_1;
            sizeRegMask    = RBM_ARG_2;
            break;
#endif

        default:
            unreached();
    }

    if ((dstAddr == nullptr) && (dstAddrRegMask != RBM_NONE))
    {
        // This is a local destination; we'll use a temp register for its address.
        assert(store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));
        BuildInternalIntDef(store, dstAddrRegMask);
    }

    if ((srcAddrOrFill == nullptr) && (srcRegMask != RBM_NONE))
    {
        // This is a local source; we'll use a temp register for its address.
        assert(src->isContained() && src->OperIs(GT_LCL_VAR, GT_LCL_FLD));
        BuildInternalIntDef(store, srcRegMask);
    }

    if (sizeRegMask != RBM_NONE)
    {
        // Reserve a temp register for the block size argument.
        BuildInternalIntDef(store, sizeRegMask);
    }

    unsigned useCount = 0;

    if (dstAddr != nullptr)
    {
        if (!dstAddr->isContained())
        {
            BuildUse(dstAddr, dstAddrRegMask);
            useCount++;
        }
        else if (GenTreeAddrMode* am = dstAddr->IsAddrMode())
        {
            useCount += BuildAddrModeUses(am);
        }
    }

#if FEATURE_MULTIREG_RET
    if (kind == StructStoreKind::UnrollRegs)
    {
        for (unsigned i = 0, count = src->AsCall()->GetRegCount(); i < count; i++)
        {
            BuildUse(src, RBM_NONE, i);
            useCount++;
        }
    }
    else
#endif
        if (srcAddrOrFill != nullptr)
    {
        if (!srcAddrOrFill->isContained())
        {
            BuildUse(srcAddrOrFill, srcRegMask);
            useCount++;
        }
        else if (GenTreeAddrMode* am = srcAddrOrFill->IsAddrMode())
        {
            useCount += BuildAddrModeUses(am);
        }
    }

#ifdef TARGET_X86
    // If we require a byte register on x86, we may run into an over-constrained situation
    // if we have BYTE_REG_COUNT or more uses.
    // This is because the byteable register requirement doesn't "reserve" a specific register,
    // and it would be possible for the incoming sources to all be occupying the byteable
    // registers, leaving none free for the internal register.
    // In this scenario, we will require EAX to ensure that it is reserved and available.
    // We need to make that modification prior to building the uses for the internal register,
    // so that when we create the use we will also create the RefTypeFixedRef on the RegRecord.
    if (useCount >= BYTE_REG_COUNT)
    {
        // Only unrolled copies may reach the limit, when both source and destination are
        // base + index address modes.
        assert(kind == StructStoreKind::UnrollCopy);

        if (internalByteDef != nullptr)
        {
            internalByteDef->registerAssignment = RBM_EAX;
        }
    }
#endif

    BuildInternalUses();
    BuildKills(store, getKillSetForStructStore(kind));
}

void LinearScan::BuildStructStoreUnrollRegsWB(GenTreeObj* store, ClassLayout* layout)
{
#ifndef UNIX_AMD64_ABI
    unreached();
#else
    assert(layout == store->GetLayout());
    assert(layout->GetSlotCount() == 2);

    GenTree*     addr  = store->GetAddr();
    GenTreeCall* value = store->GetValue()->AsCall();

    assert(value->GetRegCount() == 2);

    regMaskTP killSet     = compiler->compHelperCallKillSet(CORINFO_HELP_CHECKED_ASSIGN_REF);
    regMaskTP addrRegMask = RBM_NONE;

    if (layout->IsGCRef(0))
    {
        addrRegMask = RBM_ALLINT & ~killSet;
        BuildInternalIntDef(store, RBM_ALLINT & ~killSet);
    }
    else
    {
        assert(layout->IsGCRef(1));

        addrRegMask = RBM_ARG_0;
    }

    if (!addr->isContained())
    {
        BuildUse(addr, addrRegMask);
    }
    else if (GenTreeAddrMode* am = addr->IsAddrMode())
    {
        BuildUse(am->GetBase(), addrRegMask);
        assert(am->GetIndex() == nullptr);
    }

    BuildUse(value, RBM_NONE, 0);
    BuildUse(value, RBM_NONE, 1);
    BuildInternalUses();
    BuildKills(store, killSet);
#endif
}

void LinearScan::BuildPutArgStk(GenTreePutArgStk* putArgStk)
{
    GenTree* src = putArgStk->GetOp(0);

    if (GenTreeFieldList* fieldList = src->IsFieldList())
    {
        assert(src->isContained());

        RefPosition* simdTemp = nullptr;
        RefPosition* intTemp  = nullptr;
#ifdef TARGET_X86
        unsigned prevOffset = putArgStk->GetArgSize();
#endif

        // We need to iterate over the fields twice; once to determine the need for internal temps,
        // and once to actually build the uses.
        for (GenTreeFieldList::Use& use : fieldList->Uses())
        {
            GenTree* const  fieldNode   = use.GetNode();
            const unsigned  fieldOffset = use.GetOffset();
            const var_types fieldType   = use.GetType();

#ifdef FEATURE_SIMD
            // Note that we need to check the GT_FIELD_LIST type, not 'fieldType'. This is because the
            // GT_FIELD_LIST will be TYP_SIMD12 whereas the fieldType might be TYP_SIMD16 for lclVar, where
            // we "round up" to 16.
            if ((fieldType == TYP_SIMD12) && (simdTemp == nullptr))
            {
                simdTemp = BuildInternalFloatDef(putArgStk);
            }
#endif

#ifdef TARGET_X86
            assert(fieldType != TYP_LONG);

            if (putArgStk->GetKind() == GenTreePutArgStk::Kind::Push)
            {
                // We can treat as a slot any field that is stored at a slot boundary, where the previous
                // field is not in the same slot. (Note that we store the fields in reverse order.)
                const bool fieldIsSlot = ((fieldOffset % 4) == 0) && ((prevOffset - fieldOffset) >= 4);
                if (intTemp == nullptr)
                {
                    intTemp = BuildInternalIntDef(putArgStk);
                }
                if (!fieldIsSlot && varTypeIsByte(fieldType))
                {
                    // If this field is a slot--i.e. it is an integer field that is 4-byte aligned and takes up 4 bytes
                    // (including padding)--we can store the whole value rather than just the byte. Otherwise, we will
                    // need a byte-addressable register for the store. We will enforce this requirement on an internal
                    // register, which we can use to copy multiple byte values.
                    intTemp->registerAssignment &= allByteRegs();
                }
            }

            prevOffset = fieldOffset;
#endif // TARGET_X86
        }

        for (GenTreeFieldList::Use& use : fieldList->Uses())
        {
            GenTree* const fieldNode = use.GetNode();
            if (!fieldNode->isContained())
            {
                BuildUse(fieldNode);
            }
        }

        BuildInternalUses();

        return;
    }

#if defined(FEATURE_SIMD) && defined(TARGET_X86)
    if (putArgStk->IsSIMD12())
    {
        BuildInternalFloatDef(putArgStk, internalFloatRegCandidates());
        BuildUse(src);
        BuildInternalUses();

        return;
    }
#endif

#ifdef TARGET_X86
    if (src->IsMultiRegCall() && varTypeIsStruct(src->GetType()))
    {
        for (unsigned i = 0; i < src->AsCall()->GetRegCount(); i++)
        {
            BuildUse(src, RBM_NONE, i);
        }

        return;
    }
#endif

    if (src->TypeIs(TYP_STRUCT))
    {
        assert(src->isContained());

        switch (putArgStk->GetKind())
        {
#ifdef TARGET_X86
            case GenTreePutArgStk::Kind::Push:
                break;
#endif

            case GenTreePutArgStk::Kind::Unroll:
                ClassLayout* layout;
                unsigned     size;

                if (src->OperIs(GT_LCL_VAR))
                {
                    layout = compiler->lvaGetDesc(src->AsLclVar())->GetLayout();
                    size   = roundUp(layout->GetSize(), REGSIZE_BYTES);
                }
                else if (src->OperIs(GT_LCL_FLD))
                {
                    layout = src->AsLclFld()->GetLayout(compiler);
                    size   = roundUp(layout->GetSize(), REGSIZE_BYTES);
                }
                else if (src->IsIntegralConst(0))
                {
                    layout = nullptr;
                    size   = putArgStk->GetSlotCount() * REGSIZE_BYTES;
                }
                else
                {
                    layout = src->AsObj()->GetLayout();
                    size   = layout->GetSize();
                }

                // If we have a remainder smaller than XMM_REGSIZE_BYTES, we need an integer temp reg.
                //
                // x86 specific note: if the size is odd, the last copy operation would be of size 1 byte.
                // But on x86 only RBM_BYTE_REGS could be used as byte registers.  Therefore, exclude
                // RBM_NON_BYTE_REGS from internal candidates.
                if ((size % XMM_REGSIZE_BYTES) != 0)
                {
                    regMaskTP regMask = allRegs(TYP_INT);

#ifdef TARGET_X86
                    if ((size % 2) != 0)
                    {
                        regMask &= ~RBM_NON_BYTE_REGS;
                    }
#endif
                    BuildInternalIntDef(putArgStk, regMask);
                }

#ifdef TARGET_X86
                if (size >= XMM_REGSIZE_BYTES / 2)
#else
                if (size >= XMM_REGSIZE_BYTES)
#endif
                {
                    // If we have a buffer larger than or equal to XMM_REGSIZE_BYTES on x64/ux,
                    // or larger than or equal to 8 bytes on x86, reserve an XMM register to use it for a
                    // series of 16-byte loads and stores.
                    BuildInternalFloatDef(putArgStk, internalFloatRegCandidates());
                    SetContainsAVXFlags();
                }
                break;

            case GenTreePutArgStk::Kind::RepInstrXMM:
                BuildInternalFloatDef(putArgStk, internalFloatRegCandidates());
                SetContainsAVXFlags();
                FALLTHROUGH;
            case GenTreePutArgStk::Kind::RepInstr:
                BuildInternalIntDef(putArgStk, RBM_RDI);
                BuildInternalIntDef(putArgStk, RBM_RCX);
                BuildInternalIntDef(putArgStk, RBM_RSI);
                break;

            case GenTreePutArgStk::Kind::GCUnrollXMM:
                BuildInternalFloatDef(putArgStk, internalFloatRegCandidates());
                SetContainsAVXFlags();
                FALLTHROUGH;
            case GenTreePutArgStk::Kind::GCUnroll:
                BuildInternalIntDef(putArgStk);
                break;

            default:
                unreached();
        }

        if (src->OperIs(GT_OBJ))
        {
            BuildAddrUses(src->AsObj()->GetAddr());
        }

        BuildInternalUses();

        return;
    }

#ifdef WINDOWS_AMD64_ABI
    assert(putArgStk->GetSlotCount() == 1);
#else
    if ((src->IsIntegralConst(0) && (putArgStk->GetSlotCount() > 1)))
    {
        if (putArgStk->GetKind() == GenTreePutArgStk::Kind::RepInstrZero)
        {
            BuildInternalIntDef(putArgStk, RBM_RDI);
            BuildInternalIntDef(putArgStk, RBM_RCX);
            BuildUse(src, RBM_RAX);
        }
        else
        {
            assert(putArgStk->GetKind() == GenTreePutArgStk::Kind::UnrollZero);
            assert(src->isContained());

#ifdef TARGET_X86
            if (putArgStk->GetArgSize() >= XMM_REGSIZE_BYTES)
#endif
            {
                BuildInternalFloatDef(putArgStk, internalFloatRegCandidates());
            }
        }

        BuildInternalUses();

        return;
    }
#endif // !WINDOWS_AMD64_ABI

    if (!src->isContained())
    {
        BuildUse(src);
    }
#ifdef TARGET_X86
    else if (src->OperIs(GT_IND))
    {
        BuildAddrUses(src->AsIndir()->GetAddr());
    }
#endif
}

void LinearScan::BuildLclHeap(GenTreeUnOp* tree)
{
    // Size                  Init Memory  # temp regs
    // 0                     don't care   0 (returns 0)
    // const <= 6 reg words  don't care   0 (pushes '0')
    // const > 6 reg words   Yes          0 (pushes '0')
    // const < PageSize      No           0 (amd64) 1 (x86 tmpReg for subtracting from esp)
    // const >= PageSize     No           2 (regCnt and tmpReg for subtracting from sp)
    // variable              Yes          0 (regCnt = targetReg and pushes '0')
    // variable              No           2 (regCnt and tmpReg for subtracting from sp)
    //
    // Note: Here we don't need internal register to be different from targetReg.
    // Rather, require it to be different from operand's reg.

    GenTree* size         = tree->GetOp(0);
    unsigned tempRegCount = 0;

    if (!size->IsIntCon())
    {
        if (!compiler->info.compInitMem)
        {
            tempRegCount = 2;
        }
    }
    else
    {
        assert(size->isContained());

        size_t sizeVal = size->AsIntCon()->GetUnsignedValue();

        if ((sizeVal != 0) && !compiler->info.compInitMem)
        {
            sizeVal = AlignUp(sizeVal, STACK_ALIGN);

            if (sizeVal / REGSIZE_BYTES > 6)
            {
                if (sizeVal < compiler->eeGetPageSize())
                {
#ifdef TARGET_X86
                    // x86 needs a register to avoid generating "sub" on ESP.
                    tempRegCount = 1;
#endif
                }
                else
                {
                    tempRegCount = 2;
                }
            }
        }
    }

    for (unsigned i = 0; i < tempRegCount; i++)
    {
        BuildInternalIntDef(tree);
    }

    if (!size->isContained())
    {
        BuildUse(size);
    }

    BuildInternalUses();
    BuildDef(tree);
}

void LinearScan::BuildModDiv(GenTreeOp* tree)
{
    assert(tree->OperIs(GT_DIV, GT_MOD, GT_UDIV, GT_UMOD) && varTypeIsIntegral(tree->GetType()));

    GenTree* op1 = tree->GetOp(0);
    GenTree* op2 = tree->GetOp(1);

#ifdef TARGET_X86
    if (op1->OperIs(GT_LONG))
    {
        assert(tree->OperIs(GT_UMOD));
        assert(op1->isContained());
        assert(op2->IsIntCon());

        GenTree* loVal = op1->AsOp()->GetOp(0);
        GenTree* hiVal = op1->AsOp()->GetOp(1);
        assert(!loVal->isContained() && !hiVal->isContained());

        BuildInternalIntDef(tree);
        BuildUse(loVal, RBM_EAX);
        BuildUse(hiVal, RBM_EDX);
    }
    else
#endif
    {
        tgtPrefUse = BuildUse(op1, RBM_EAX);
    }

    BuildDelayFreeUses(op2, op1, allRegs(TYP_INT) & ~(RBM_RAX | RBM_RDX));
    BuildInternalUses();
    BuildKills(tree, RBM_RAX | RBM_RDX);
    BuildDef(tree, tree->OperIs(GT_DIV, GT_UDIV) ? RBM_RAX : RBM_RDX);
}

void LinearScan::BuildIntrinsic(GenTreeIntrinsic* tree)
{
    GenTree* op1 = tree->GetOp(0);

    assert(varTypeIsFloating(op1->GetType()) && (op1->GetType() == tree->GetType()));
    assert(tree->gtOp2 == nullptr);

    switch (tree->GetIntrinsic())
    {
        case NI_System_Math_Abs:
            // TODO-MIKE-Review: Where is this internal reg used???
            BuildInternalFloatDef(tree, internalFloatRegCandidates());
            break;
        case NI_System_Math_Ceiling:
        case NI_System_Math_Floor:
        case NI_System_Math_Round:
        case NI_System_Math_Sqrt:
            break;
        default:
            unreached();
    }

    if (!op1->isContained())
    {
        tgtPrefUse = BuildUse(op1);
    }
    else if (op1->OperIs(GT_IND))
    {
        BuildAddrUses(op1->AsIndir()->GetAddr());
    }

    BuildInternalUses();
    BuildDef(tree);
}

#ifdef FEATURE_HW_INTRINSICS
void LinearScan::BuildHWIntrinsic(GenTreeHWIntrinsic* node)
{
    // Set the AVX Flags if this instruction may use VEX encoding for SIMD operations.
    // Note that this may be true even if the ISA is not AVX (e.g. for platform-agnostic
    // intrinsics or non-AVX intrinsics that will use VEX encoding if it is available
    // on the target).
    if (node->IsSimd())
    {
        SetContainsAVXFlags(node->GetSimdSize());
    }

    unsigned  numOps        = node->GetNumOps();
    regMaskTP dstCandidates = RBM_NONE;

    if (numOps != 0)
    {
        GenTree* op1 = nullptr;
        GenTree* op2 = nullptr;
        GenTree* op3 = nullptr;

        switch (numOps)
        {
            case 1:
                op1 = node->GetOp(0);
                break;
            case 2:
                op1 = node->GetOp(0);
                op2 = node->GetOp(1);
                break;
            case 3:
            case 4:
            case 5:
                op1 = node->GetOp(0);
                op2 = node->GetOp(1);
                op3 = node->GetOp(2);
                break;
            default:
                unreached();
        }

        GenTree*            lastOp      = node->GetLastOp();
        NamedIntrinsic      intrinsicId = node->GetIntrinsic();
        HWIntrinsicCategory category    = HWIntrinsicInfo::lookupCategory(intrinsicId);

        if ((category == HW_Category_IMM) && !HWIntrinsicInfo::NoJmpTableImm(intrinsicId))
        {
            if (HWIntrinsicInfo::isImmOp(intrinsicId, lastOp) && !lastOp->IsContainedIntCon())
            {
                // We need two extra reg when lastOp isn't a constant so the offset
                // into the jump table for the fallback path can be computed.
                BuildInternalIntDef(node);
                BuildInternalIntDef(node);
            }
        }

        var_types baseType  = node->GetSimdBaseType();
        bool      isRMW     = node->isRMWHWIntrinsic(compiler);
        bool      buildUses = true;

        auto BuildOperand = [this](GenTree* op) { BuildOperandUses(op); };

        // Create internal temps, and handle any other special requirements.
        // Note that the default case for building uses will handle the RMW flag,
        // but if the uses are built in the individual cases, buildUses is set to
        // false, and any RMW handling (delayFree) must be handled within the case.
        switch (intrinsicId)
        {
            case NI_Vector128_CreateScalarUnsafe:
            case NI_Vector256_CreateScalarUnsafe:
                assert(numOps == 1);

                if (varTypeIsFloating(baseType) && !op1->isContained())
                {
                    tgtPrefUse = BuildUse(op1);
                    buildUses  = false;
                }
                break;

            case NI_Vector128_GetElement:
            case NI_Vector256_GetElement:
                assert(numOps == 2);
                assert(op2->IsIntCon() || op1->isContained());

                if (varTypeIsFloating(baseType) && !op1->isContained() && op2->IsIntegralConst(0))
                {
                    tgtPrefUse = BuildUse(op1);
                    buildUses  = false;
                }
                break;

            case NI_Vector128_ToVector256:
            case NI_Vector128_ToVector256Unsafe:
            case NI_Vector256_GetLower:
                assert(numOps == 1);

                if (!op1->isContained())
                {
                    tgtPrefUse = BuildUse(op1);
                    buildUses  = false;
                }
                break;

            case NI_SSE2_MaskMove:
                assert(numOps == 3);
                assert(!isRMW);

                BuildUse(op1);
                BuildUse(op2);
                BuildUse(op3, RBM_RDI);
                buildUses = false;
                break;

            case NI_SSE41_BlendVariable:
                assert(numOps == 3);

                if (!compiler->canUseVexEncoding())
                {
                    assert(isRMW);

                    // SSE4.1 blendv* hardcode the mask vector (op3) in XMM0
                    tgtPrefUse = BuildUse(op1);

                    if (op2->isContained())
                    {
                        BuildOperand(op2);
                    }
                    else
                    {
                        BuildDelayFreeUses(op2, op1);
                    }

                    BuildDelayFreeUses(op3, op1, RBM_XMM0);
                    buildUses = false;
                }
                break;

            case NI_SSE41_Extract:
                assert(!varTypeIsFloating(baseType));
#ifdef TARGET_X86
                if (varTypeIsByte(baseType))
                {
                    dstCandidates = allByteRegs();
                }
#endif
                break;

#ifdef TARGET_X86
            case NI_SSE42_Crc32:
            case NI_SSE42_X64_Crc32:
                // TODO-XArch-Cleanup: Currently we use the BaseType to bring the type of the second argument
                // to the code generator. We may want to encode the overload info in another way.
                assert(numOps == 2);
                assert(isRMW);

                // CRC32 may operate over "byte" but on x86 only RBM_BYTE_REGS can be used as byte registers.
                tgtPrefUse = BuildUse(op1);
                BuildDelayFreeUses(op2, op1, varTypeIsByte(baseType) ? allByteRegs() : RBM_NONE);
                buildUses = false;
                break;
#endif // TARGET_X86

            case NI_BMI2_MultiplyNoFlags:
            case NI_BMI2_X64_MultiplyNoFlags:
                assert((numOps == 2) || (numOps == 3));

                BuildUse(op1, RBM_EDX);
                BuildOperand(op2);

                if (numOps == 3)
                {
                    // op3 reg should be different from target reg to
                    // store the lower half result after executing the instruction
                    BuildDelayFreeUses(op3, op1);
                    // Need a internal register different from the dst to take the lower half result
                    BuildInternalIntDef(node);
                    setInternalRegsDelayFree = true;
                }

                buildUses = false;
                break;

            case NI_FMA_MultiplyAdd:
            case NI_FMA_MultiplyAddNegated:
            case NI_FMA_MultiplyAddNegatedScalar:
            case NI_FMA_MultiplyAddScalar:
            case NI_FMA_MultiplyAddSubtract:
            case NI_FMA_MultiplySubtract:
            case NI_FMA_MultiplySubtractAdd:
            case NI_FMA_MultiplySubtractNegated:
            case NI_FMA_MultiplySubtractNegatedScalar:
            case NI_FMA_MultiplySubtractScalar:
            {
                assert(numOps == 3);
                assert(isRMW);

                const bool copiesUpperBits = HWIntrinsicInfo::CopiesUpperBits(intrinsicId);

                // Intrinsics with CopyUpperBits semantics cannot have op1 be contained
                assert(!copiesUpperBits || !op1->isContained());

                if (op2->isContained())
                {
                    // 132 form: op1 = (op1 * op3) + [op2]

                    tgtPrefUse = BuildUse(op1);
                    BuildOperand(op2);
                    BuildDelayFreeUses(op3, op1);
                }
                else if (op1->isContained())
                {
                    // 231 form: op3 = (op2 * op3) + [op1]

                    tgtPrefUse = BuildUse(op3);
                    BuildOperand(op1);
                    BuildDelayFreeUses(op2, op1);
                }
                else
                {
                    // 213 form: op1 = (op2 * op1) + [op3]

                    tgtPrefUse = BuildUse(op1);

                    if (copiesUpperBits)
                    {
                        BuildDelayFreeUses(op2, op1);
                    }
                    else
                    {
                        tgtPrefUse2 = BuildUse(op2);
                    }

                    if (op3->isContained())
                    {
                        BuildOperand(op3);
                    }
                    else
                    {
                        BuildDelayFreeUses(op3, op1);
                    }
                }

                buildUses = false;
                break;
            }

            case NI_AVXVNNI_MultiplyWideningAndAdd:
            case NI_AVXVNNI_MultiplyWideningAndAddSaturate:
                assert(numOps == 3);

                tgtPrefUse = BuildUse(op1);
                BuildDelayFreeUses(op2, op1);

                if (op3->isContained())
                {
                    BuildOperand(op3);
                }
                else
                {
                    BuildDelayFreeUses(op3, op1);
                }

                buildUses = false;
                break;

            case NI_AVX2_GatherVector128:
            case NI_AVX2_GatherVector256:
                assert(numOps == 3);
                assert(op3->isContained());
                assert(!isRMW);

                BuildOperand(op1);
                BuildDelayFreeUses(op2);
                BuildInternalFloatDef(node, allSIMDRegs());
                setInternalRegsDelayFree = true;
                buildUses                = false;
                break;

            case NI_AVX2_GatherMaskVector128:
            case NI_AVX2_GatherMaskVector256:
                assert(numOps == 5);
                assert(node->GetOp(4)->isContained());
                assert(!isRMW);

                BuildOperand(op1);
                BuildDelayFreeUses(op2);
                BuildDelayFreeUses(op3);
                BuildDelayFreeUses(node->GetOp(3));
                BuildInternalFloatDef(node, allSIMDRegs());
                setInternalRegsDelayFree = true;
                buildUses                = false;
                break;

            default:
                assert((intrinsicId > NI_HW_INTRINSIC_START) && (intrinsicId < NI_HW_INTRINSIC_END));
                break;
        }

        if (buildUses)
        {
            assert((numOps > 0) && (numOps < 4));

            if (node->OperIsMemoryLoadOrStore())
            {
                BuildAddrUses(op1);
            }
            else if (isRMW && !op1->isContained())
            {
                tgtPrefUse = BuildUse(op1);
            }
            else
            {
                BuildOperand(op1);
            }

            if (op2 != nullptr)
            {
                if (op2->IsHWIntrinsic() && op2->AsHWIntrinsic()->OperIsMemoryLoad() && op2->isContained())
                {
                    BuildAddrUses(op2->AsHWIntrinsic()->GetOp(0));
                }
                else if (isRMW)
                {
                    if (!op2->isContained() && HWIntrinsicInfo::IsCommutative(intrinsicId))
                    {
                        // When op2 is not contained and we are commutative, we can set op2
                        // to also be a tgtPrefUse. Codegen will then swap the operands.
                        tgtPrefUse2 = BuildUse(op2);
                    }
                    else if (!op2->isContained() || varTypeIsArithmetic(node->GetType()))
                    {
                        // When op2 is not contained or if we are producing a scalar value
                        // we need to mark it as delay free because the operand and target
                        // exist in the same register set.
                        BuildDelayFreeUses(op2, op1);
                    }
                    else
                    {
                        // When op2 is contained and we are not producing a scalar value we
                        // have no concerns of overwriting op2 because they exist in different
                        // register sets.
                        BuildOperand(op2);
                    }
                }
                else
                {
                    BuildOperand(op2);
                }

                if (op3 != nullptr)
                {
                    if (isRMW)
                    {
                        BuildDelayFreeUses(op3, op1);
                    }
                    else
                    {
                        BuildOperand(op3);
                    }
                }
            }
        }

        BuildInternalUses();
    }

    if (node->IsValue())
    {
        RefPosition* def = BuildDef(node, dstCandidates);

        if (node->IsHWIntrinsicZero())
        {
            def->getInterval()->isConstant = true;
        }
    }
}
#endif

void LinearScan::BuildBoundsChk(GenTreeBoundsChk* node)
{
    BuildOperandUses(node->GetOp(0));
    BuildOperandUses(node->GetOp(1));
}

void LinearScan::BuildCast(GenTreeCast* cast)
{
    GenTree*  src        = cast->GetOp(0);
    regMaskTP candidates = RBM_NONE;

#ifdef TARGET_X86
    if (varTypeIsByte(cast->GetType()))
    {
        candidates = allByteRegs();
    }

    assert(!varTypeIsLong(src->GetType()) || (src->OperIs(GT_LONG) && src->isContained()));
#else
    // Overflow checking cast from TYP_(U)LONG to TYP_UINT requires a temporary
    // register to extract the upper 32 bits of the 64 bit source register.
    if (cast->gtOverflow() && varTypeIsLong(src->GetType()) && (cast->GetCastType() == TYP_UINT))
    {
        // Here we don't need internal register to be different from targetReg,
        // rather require it to be different from operand's reg.
        BuildInternalIntDef(cast);

        // If the cast operand ends up being in memory then the value will be loaded directly
        // into the destination register and thus the internal register has to be different.
        if (src->isContained() || src->IsRegOptional())
        {
            setInternalRegsDelayFree = true;
        }
    }
#endif

    if (!src->isContained())
    {
        BuildUse(src, candidates);
    }
    else if (src->OperIs(GT_IND))
    {
        // TODO-MIKE-Review: Address mode registers don't need the "byte reg" constraint...
        BuildAddrUses(src->AsIndir()->GetAddr(), candidates);
    }
#ifdef TARGET_X86
    else if (src->OperIs(GT_LONG))
    {
        BuildUse(src->AsOp()->GetOp(0));
        BuildUse(src->AsOp()->GetOp(1));
    }
#endif
    else
    {
        assert(src->OperIs(GT_LCL_VAR, GT_LCL_FLD));
    }

    BuildInternalUses();
    BuildDef(cast, candidates);
}

void LinearScan::BuildLoadInd(GenTreeIndir* load)
{
    assert(load->OperIs(GT_IND) && !load->TypeIs(TYP_STRUCT));

#ifdef FEATURE_SIMD
    if (varTypeIsSIMD(load->GetType()))
    {
        SetContainsAVXFlags(varTypeSize(load->GetType()));

        if (load->TypeIs(TYP_SIMD12))
        {
            BuildInternalFloatDef(load);
            // We need an internal register different from the destination
            // register and both registers are used at the same time.
            setInternalRegsDelayFree = true;
        }
    }
#endif

    BuildAddrUses(load->GetAddr());
    BuildInternalUses();
    BuildDef(load);
}

void LinearScan::BuildStoreInd(GenTreeIndir* store)
{
    assert(store->OperIs(GT_STOREIND) && !store->TypeIs(TYP_STRUCT));

#ifdef FEATURE_SIMD
    if (varTypeIsSIMD(store->GetType()))
    {
        SetContainsAVXFlags(varTypeSize(store->GetType()));

        if (store->TypeIs(TYP_SIMD12))
        {
            GenTree* value = store->GetValue();

            if (value->isContained())
            {
#ifdef TARGET_64BIT
                BuildInternalIntDef(store);
#else
                BuildInternalFloatDef(store);
#endif
                BuildAddrUses(store->GetAddr());

                if (value->OperIs(GT_IND))
                {
                    BuildAddrUses(value->AsIndir()->GetAddr());
                }

                BuildInternalUses();

                return;
            }

            BuildInternalFloatDef(store);
        }
    }
#endif // FEATURE_SIMD

    BuildAddrUses(store->GetAddr());

    GenTree* value   = store->GetValue();
    bool     isShift = false;

    if (value->isContained() && value->OperIsRMWMemOp())
    {
        GenTreeIndir* load;

        if (value->OperIsBinary())
        {
            isShift = value->OperIsShiftOrRotate();
            load    = value->AsOp()->GetOp(0)->AsIndir();
            value   = value->AsOp()->GetOp(1);
        }
        else
        {
            load  = value->AsUnOp()->GetOp(0)->AsIndir();
            value = nullptr;
        }
    }

    if ((value != nullptr) && !value->isContained())
    {
        regMaskTP regs = RBM_NONE;

        if (isShift)
        {
            regs = RBM_RCX;
        }
#ifdef TARGET_X86
        else if (varTypeIsByte(store->GetType()))
        {
            regs = allByteRegs();
        }
#endif

        BuildUse(value, regs);

        if (isShift)
        {
            // TODO-MIKE-Review: It's not clear why shifts needs this.
            buildKillPositionsForNode(store, currentLoc + 1, RBM_RCX);
        }
    }

    BuildInternalUses();
}

void LinearScan::BuildMul(GenTreeOp* mul)
{
    assert(mul->OperIs(GT_MUL) && varTypeIsIntegral(mul->GetType()));

    GenTree* op1 = mul->GetOp(0);
    GenTree* op2 = mul->GetOp(1);

    if (!op1->IsContainedIntCon() && !op2->IsContainedIntCon())
    {
        BuildRMWUses(mul);
    }
    else
    {
        BuildOperandUses(op1);
        BuildOperandUses(op2);
    }

    if (mul->gtOverflow() && mul->IsUnsigned())
    {
        // We need to use the "MUL reg/mem" form to get an extended 64/128 bit
        // result and check the upper half for non-zero to detect overflow.

        BuildKills(mul, RBM_RAX | RBM_RDX);
        BuildDef(mul, RBM_RAX);
    }
    else
    {
        BuildDef(mul);
    }
}

void LinearScan::BuildMulLong(GenTreeOp* mul)
{
#ifdef TARGET_X86
    assert(mul->OperIs(GT_MULHI, GT_MUL_LONG));
#else
    assert(mul->OperIs(GT_MULHI));
#endif
    assert(varTypeIsIntegral(mul->GetType()));

    GenTree* op1 = mul->GetOp(0);
    GenTree* op2 = mul->GetOp(1);

    BuildRMWUses(mul);
    BuildKills(mul, RBM_RAX | RBM_RDX);

#ifdef TARGET_X86
    if (mul->OperIs(GT_MUL_LONG))
    {
        BuildDef(mul, TYP_INT, RBM_RAX, 0);
        BuildDef(mul, TYP_INT, RBM_RDX, 1);
    }
    else
#endif
    {
        BuildDef(mul, RBM_RDX);
    }
}

void LinearScan::SetContainsAVXFlags(unsigned byteSize)
{
    if (!compiler->canUseVexEncoding())
    {
        return;
    }

    compiler->compExactlyDependsOn(InstructionSet_AVX);
    compiler->GetEmitter()->SetContainsAVX();

    if (byteSize == 32)
    {
        compiler->GetEmitter()->SetContains256bitAVX();
    }
}

void LinearScan::BuildCmp(GenTreeOp* cmp)
{
    assert(cmp->OperIsCompare() || cmp->OperIs(GT_CMP));

    regMaskTP dstCandidates = RBM_NONE;
    regMaskTP op1Candidates = RBM_NONE;
    regMaskTP op2Candidates = RBM_NONE;
    GenTree*  op1           = cmp->GetOp(0);
    GenTree*  op2           = cmp->GetOp(1);

#ifdef TARGET_X86
    // If the compare is not used by a jump then we need to generate a SETcc instruction,
    // which requires the dst be a byte register.
    if (!cmp->TypeIs(TYP_VOID))
    {
        dstCandidates = allByteRegs();
    }

    bool needByteRegs = false;

    if (varTypeIsByte(cmp->GetType()))
    {
        if (!varTypeIsFloating(op1->GetType()))
        {
            needByteRegs = true;
        }
    }
    else if (varTypeIsByte(op1->GetType()) && varTypeIsByte(op2->GetType()))
    {
        needByteRegs = true;
    }
    else if (varTypeIsByte(op1->GetType()) && op2->IsIntCon())
    {
        needByteRegs = true;
    }
    else if (op1->IsIntCon() && varTypeIsByte(op2->GetType()))
    {
        needByteRegs = true;
    }

    if (needByteRegs)
    {
        if (!op1->isContained())
        {
            op1Candidates = allByteRegs();
        }

        if (!op2->isContained())
        {
            op2Candidates = allByteRegs();
        }
    }
#endif // TARGET_X86

    BuildOperandUses(op1, op1Candidates);
    BuildOperandUses(op2, op2Candidates);

    if (!cmp->TypeIs(TYP_VOID))
    {
        BuildDef(cmp, dstCandidates);
    }
}

#endif // TARGET_XARCH
