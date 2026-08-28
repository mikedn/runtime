// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_XARCH

#include "lsra.h"
#include "jitgcinfo.h"

void LinearScan::BuildNode(GenTree* tree)
{
    assert(!tree->isContained());

    switch (tree->GetOper())
    {
        case GT_LCL_LOAD:
        case GT_LCL_LOAD_FLD:
            assert(!tree->AsLclRef()->GetLcl()->IsRegCandidate());

#ifdef FEATURE_SIMD
            if (tree->TypeIs(TYP_SIMD12) && !compiler->compOpportunisticallyDependsOn(InstructionSet_SSE41))
            {
                BuildInternalFloatDef(tree, allFloatRegs());
                setInternalRegsDelayFree = true;
                BuildInternalUses();
            }
#endif
            BuildDef(tree);
            break;

        case GT_LCL_STORE:
            BuildLclStore(tree->AsLclStore());
            break;

        case GT_LCL_STORE_FLD:
            BuildLclStoreFld(tree->AsLclStoreFld());
            break;

        case GT_START_PREEMPTGC:
            BuildKills(tree, RBM_NONE);
            break;

        case GT_PROF_HOOK:
            BuildKills(tree, getKillSetForProfilerHook());
            break;

        case GT_CNS_INT:
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

        case GT_FTRUNC:
        case GT_FXT:
        case GT_STOF:
        case GT_UTOF:
        case GT_FTOS:
        case GT_FTOU:
        case GT_SXT:
        case GT_UXT:
            BuildOperandUses(tree->AsUnOp()->GetOp(0));
            BuildDef(tree);
            break;

        case GT_FADD:
        case GT_FSUB:
        case GT_FMUL:
        case GT_FDIV:
            if (compiler->codeGen->UseVexEncoding())
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
        case GT_OVF_SADDC:
        case GT_OVF_UADDC:
        case GT_OVF_SSUBB:
        case GT_OVF_USUBB:
#endif
        case GT_ADD:
        case GT_SUB:
        case GT_AND:
        case GT_OR:
        case GT_XOR:
        case GT_OVF_SADD:
        case GT_OVF_UADD:
        case GT_OVF_SSUB:
        case GT_OVF_USUB:
            BuildRMWUses(tree->AsOp());
            FALLTHROUGH;
        case GT_JMPTABLE:
        case GT_LCL_ADDR:
        case GT_CONST_ADDR:
        case GT_REG_USE:
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
            // Also, x86 doesn't need this register.
            BuildInternalIntDef(tree);
            assert(tree->AsUnOp()->GetOp(0)->isContained());
            BuildAddrUses(tree->AsUnOp()->GetOp(0)->AsIndir()->GetAddr());
            BuildInternalUses();
            BuildKills(tree, Compiler::compHelperCallKillSet(CORINFO_HELP_STOP_FOR_GC));
            break;

        case GT_SREM:
        case GT_SDIV:
        case GT_UREM:
        case GT_UDIV:
            BuildDivMod(tree->AsOp());
            break;

        case GT_MUL:
        case GT_OVF_SMUL:
        case GT_OVF_UMUL:
            BuildMul(tree->AsOp());
            break;

        case GT_SMULH:
        case GT_UMULH:
#ifdef TARGET_X86
        case GT_SMULL:
        case GT_UMULL:
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

        case GT_OVF_TRUNC:
        case GT_OVF_STRUNC:
        case GT_OVF_UTRUNC:
            BuildOvfTruncate(tree->AsUnOp());
            break;

        case GT_OVF_U:
            BuildOvfUnsigned(tree->AsUnOp());
            break;

        case GT_OVF_SCONV:
        case GT_OVF_UCONV:
            BuildOvfConv(tree->AsUnOp());
            break;

        case GT_CONV:
            BuildConv(tree->AsUnOp());
            break;

        case GT_TRUNC:
            BuildOperandUses(tree->AsUnOp()->GetOp(0));
            BuildDef(tree);
            break;

        case GT_BITCAST:
            BuildBitCast(tree->AsUnOp());
            break;

        case GT_FNEG:
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

        case GT_ARG_STORE:
            BuildArgStore(tree->AsArgStore());
            break;

        case GT_IND_STORE_BLK:
        case GT_IND_STORE_OBJ:
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

        case GT_LEA:
            BuildAddrMode(tree->AsAddrMode());
            break;

        case GT_IND_STORE:
            if (GCInfo::GetWriteBarrierForm(tree->AsIndStore()) != GCInfo::WBF_NoBarrier)
            {
                BuildGCWriteBarrier(tree->AsIndStore());
            }
            else
            {
                BuildIndStore(tree->AsIndStore());
            }
            break;

        case GT_NULLCHECK:
            BuildUse(tree->AsNullCheck()->GetAddr());
            break;

        case GT_IND_LOAD:
            BuildLoadInd(tree->AsIndLoad());
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
            assert(!tree->AsIndexAddr()->GetIndex()->TypeIs(TYP_LONG));

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
            BuildUse(tree->AsOp()->GetOp(0));
            BuildUse(tree->AsOp()->GetOp(1));
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

void LinearScan::BuildBitCast(GenTreeUnOp* bitcast)
{
    GenTree* value = bitcast->GetOp(0);

    if (!value->isContained())
    {
        BuildUse(value);
    }
#ifdef TARGET_X86
    else if (value->OperIs(GT_LONG))
    {
        BuildUse(value->AsOp()->GetOp(0));
        BuildUse(value->AsOp()->GetOp(1));

        if (!compiler->compOpportunisticallyDependsOn(InstructionSet_SSE41))
        {
            BuildInternalFloatDef(bitcast);
        }
    }

    if (bitcast->TypeIs(TYP_LONG))
    {
        if (!compiler->compOpportunisticallyDependsOn(InstructionSet_SSE41))
        {
            BuildInternalFloatDef(bitcast);
        }

        BuildDef(bitcast, TYP_INT, genRegMask(bitcast->GetRegNum(0)), 0);
        BuildDef(bitcast, TYP_INT, genRegMask(bitcast->GetRegNum(1)), 1);
    }
    else
#endif
    {
        BuildDef(bitcast);
    }

    BuildInternalUses();
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
    BuildUse(cmpxchg->GetOp(0), allIntRegs() & ~RBM_RAX);
    BuildUse(cmpxchg->GetOp(1), allIntRegs() & ~RBM_RAX);
    BuildUse(cmpxchg->GetOp(2), RBM_RAX);
    BuildDef(cmpxchg, RBM_RAX);
}

void LinearScan::BuildInterlocked(GenTreeOp* interlocked)
{
    GenTree* addr  = interlocked->GetOp(0);
    GenTree* value = interlocked->GetOp(1);

    tgtPrefUse = BuildDelayFreeUse(addr);
    BuildUse(value);
    BuildDef(interlocked);
}

RefPosition* LinearScan::BuildOperandUses(GenTree* node X86_ARG(regMaskTP candidates))
{
    if (!node->isContained())
    {
        return BuildUse(node X86_ARG(candidates));
    }

    if (node->OperIs(GT_IND_LOAD))
    {
        BuildAddrUses(node->AsIndLoad()->GetAddr());
    }

    return nullptr;
}

#ifdef DEBUG
// Check for instructions that use the read/modify/write register format (e.g. ADD eax, 42).
static bool IsRMWRegOper(GenTreeOp* node, Compiler* compiler)
{
    switch (node->GetOper())
    {
        case GT_FADD:
        case GT_FSUB:
        case GT_FMUL:
        case GT_FDIV:
            return !compiler->codeGen->UseVexEncoding();

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
        case GT_SMULH:
        case GT_UMULH:
#ifdef TARGET_X86
        case GT_SMULL:
        case GT_UMULL:
#endif
        // Note that overflow checking operations are reg RMW only if we do not
        // enregister local variables that are EH live, otherwise we may modify
        // the register assigned to the local BEFORE throwing an exception.
        case GT_OVF_SADD:
        case GT_OVF_UADD:
        case GT_OVF_SSUB:
        case GT_OVF_USUB:
#ifdef TARGET_X86
        case GT_OVF_SADDC:
        case GT_OVF_UADDC:
        case GT_OVF_SSUBB:
        case GT_OVF_USUBB:
#endif
            return true;

        case GT_MUL:
        case GT_OVF_SMUL:
        case GT_OVF_UMUL:
            return !node->GetOp(0)->IsContainedIntCon() && !node->GetOp(1)->IsContainedIntCon();

        default:
            return false;
    }
}
#endif // DEBUG

void LinearScan::BuildRMWUses(GenTreeOp* node)
{
    assert(IsRMWRegOper(node, compiler));

    GenTree* op1 = node->GetOp(0);
    GenTree* op2 = node->GetOp(1);

#ifdef TARGET_X86
    regMaskTP opCandidates = varTypeIsByte(node->GetType()) ? allByteRegs() : RBM_NONE;
#endif

    bool prefOp1 = !op1->isContained();
    bool prefOp2 = node->IsCommutative() && !op2->isContained();

    // Determine which operand, if any, should be delayRegFree. Normally, this would be op2,
    // but if we have a commutative operator and op1 is a contained memory op, it would be op1.
    // We need to make the delayRegFree operand remain live until the op is complete, by marking
    // the source(s) associated with op2 as "delayFree".
    // Note that if op2 of a binary RMW operator is a memory op, even if the operator
    // is commutative, codegen cannot reverse them.
    // TODO-XArch-CQ: This is not actually the case for all RMW binary operators, but there's
    // more work to be done to correctly reverse the operands if they involve memory
    // operands. Also, we may need to handle more cases than IND_LOAD (e.g. spill temps).
    GenTree* delayUseOperand = op2;

    if (node->IsCommutative())
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
        tgtPrefUse = BuildUse(op1 X86_ARG(opCandidates));
    }
    else if (delayUseOperand == op1)
    {
        assert(op1->isContained());
        BuildDelayFreeOperandUses(op1, op2);
    }
    else
    {
        assert(op1->isContained());
        BuildOperandUses(op1);
    }

    if (prefOp2)
    {
        tgtPrefUse2 = BuildUse(op2 X86_ARG(opCandidates));
    }
    else if (delayUseOperand == op2)
    {
        BuildDelayFreeOperandUses(op2, op1 X86_ARG(opCandidates));
    }
    else
    {
        BuildOperandUses(op2 X86_ARG(opCandidates));
    }
}

void LinearScan::BuildDelayFreeOperandUses(GenTree* op, GenTree* rmwOp, regMaskTP candidates)
{
    if (!op->isContained())
    {
        BuildDelayFreeUse(op, rmwOp, candidates);
        return;
    }

#ifdef FEATURE_HW_INTRINSICS
    if (GenTreeHWIntrinsic* hwIntrinsicNode = op->IsHWIntrinsic())
    {
        BuildDelayFreeUse(hwIntrinsicNode->GetOp(0), rmwOp, candidates);
        return;
    }
#endif

    if (GenTreeIndir* indir = op->IsIndir())
    {
        GenTree* addr = indir->GetAddr();

        if (!addr->isContained())
        {
            // TODO-MIKE-Review: Using "candidates" here and below is likely bogus.
            // The caller usually cares only about the case of a non contained
            // operand, it doesn't know or care about whatever registers an address
            // mode needs. Some callers pass candidates such as XMM0 or "byte regs"
            // on x86...

            BuildDelayFreeUse(addr, rmwOp, candidates);
        }
        else if (GenTreeAddrMode* const addrMode = addr->IsAddrMode())
        {
            if (GenTree* base = addrMode->GetBase())
            {
                BuildDelayFreeUse(base, rmwOp, candidates);
            }

            if (GenTree* index = addrMode->GetIndex())
            {
                BuildDelayFreeUse(index, rmwOp, candidates);
            }
        }
    }
}

void LinearScan::BuildShiftRotate(GenTreeOp* tree)
{
    GenTree*  value         = tree->GetOp(0);
    GenTree*  shiftBy       = tree->GetOp(1);
    regMaskTP regCandidates = RBM_NONE;

    if (shiftBy->isContained())
    {
        assert(shiftBy->IsIntCon());
    }
    else
    {
        regCandidates = allIntRegs() & ~RBM_RCX;
    }

#ifdef TARGET_X86
    // The first operand of a GT_LSH_HI and GT_RSH_LO oper is a GT_LONG so that
    // we can have a three operand form.
    if (tree->OperIs(GT_LSH_HI, GT_RSH_LO))
    {
        assert(value->OperIs(GT_LONG) && value->isContained());

        GenTree* sourceLo = value->AsOp()->GetOp(0);
        GenTree* sourceHi = value->AsOp()->GetOp(1);

        assert(!sourceLo->isContained() && !sourceHi->isContained());

        RefPosition* sourceLoUse = BuildUse(sourceLo, regCandidates);
        RefPosition* sourceHiUse = BuildUse(sourceHi, regCandidates);

        setDelayFree(tree->OperIs(GT_LSH_HI) ? sourceLoUse : sourceHiUse);
    }
    else
#endif
    {
        tgtPrefUse = BuildUse(value, regCandidates);
    }

    if (!shiftBy->isContained())
    {
        BuildDelayFreeUse(shiftBy, value, RBM_RCX);
        buildKillPositionsForNode(tree, currentLoc + 1, RBM_RCX);
    }

    BuildDef(tree, regCandidates);
}

void LinearScan::BuildCall(GenTreeCall* call)
{
#ifdef WINDOWS_AMD64_ABI
    bool varargsHasFloatRegArgs = false;

    if (call->IsVarargs())
    {
        // We will need an internal int reg for any float arguments to a varArgs call.
        for (GenTreeUse& use : call->Uses())
        {
            GenTree* argNode = use.GetNode();

            if (GenTreeFieldList* fieldList = argNode->IsFieldList())
            {
                for (GenTreeFieldList::Use& use : fieldList->Uses())
                {
                    varargsHasFloatRegArgs |= HandleFloatVarArgs(call, use.GetNode());
                }

                continue;
            }

            assert(argNode->OperIs(GT_PUTARG_REG));

            varargsHasFloatRegArgs |= HandleFloatVarArgs(call, argNode);
        }
    }
#endif // WINDOWS_AMD64_ABI

    for (GenTreeUse& use : call->Uses())
    {
        GenTree* argNode = use.GetNode();

        assert(argNode->OperIs(GT_PUTARG_REG));
        INDEBUG(CallArgInfo* argInfo = call->TryGetArgInfoByArgNode(argNode);)
        assert((argInfo == nullptr) || (argNode->GetRegNum() == argInfo->GetRegNum()));

        BuildUse(argNode, genRegMask(argNode->GetRegNum()));
    }

    if (GenTree* addr = call->GetCallAddr())
    {
        regMaskTP addrCandidates = RBM_NONE;

        // In case of fast tail implemented as jmp, make sure that gtControlExpr is
        // computed into a register.
        if (call->IsFastTailCall())
        {
            assert(!addr->isContained());
            // Fast tail call - make sure that call target is always computed in RAX
            // so that epilog sequence can generate "jmp rax" to achieve fast tail call.
            addrCandidates = RBM_RAX;
        }
#ifdef TARGET_X86
        else if (call->IsVirtualStubIndirect())
        {
            // On x86, we need to generate a very specific pattern for indirect VSD calls:
            //
            //    3-byte nop
            //    call dword ptr [eax]
            //
            // Where EAX is also used as an argument to the stub dispatch helper. Make
            // sure that the call target address is computed into EAX in this case.
            assert(addr->OperIs(GT_IND_LOAD) && addr->isContained());
            addrCandidates = RBM_VIRTUAL_STUB_TARGET;
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
            addrCandidates = allIntRegs() & ~RBM_ARG_REGS;
        }
#endif

        if (addr->isContained())
        {
            BuildAddrUses(addr->AsIndir()->GetAddr(), addrCandidates);
        }
        else
        {
            BuildUse(addr, addrCandidates);
        }
    }

    BuildInternalUses();
    BuildKills(call, getKillSetForCall(call));

#ifdef TARGET_X86
    if (call->IsHelperCall(CORINFO_HELP_INIT_PINVOKE_FRAME))
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

    RegNum floatReg = argNode->GetRegNum();
    RegNum intReg   = MapVarargsParamFloatRegToIntReg(floatReg);

    BuildInternalIntDef(call, genRegMask(intReg));

    return true;
}
#endif

void LinearScan::BuildStructStore(GenTree* store, StructStoreKind kind, ClassLayout* layout)
{
#ifdef UNIX_AMD64_ABI
    if (kind == StructStoreKind::UnrollRegsWB)
    {
        BuildStructStoreUnrollRegsWB(store->AsIndStoreObj(), layout);

        return;
    }
#endif

    GenTree* dstAddr = nullptr;
    GenTree* src;

    if (store->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD))
    {
        src = store->AsLclRef()->GetOp(0);
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
    else if (src->OperIs(GT_IND_LOAD, GT_IND_LOAD_OBJ, GT_IND_LOAD_BLK))
    {
        assert(src->isContained());
        srcAddrOrFill = src->AsIndir()->GetAddr();
    }
    else
    {
        assert(src->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD));
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
                && (!store->IsIndStoreObj() || !layout->HasGCPtr())
#endif
                    )
            {
                BuildInternalFloatDef(store, internalFloatRegCandidates());
                SetContainsVexInstructions();
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
                SetContainsVexInstructions();
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
        assert(store->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD));
        BuildInternalIntDef(store, dstAddrRegMask);
    }

    if ((srcAddrOrFill == nullptr) && (srcRegMask != RBM_NONE))
    {
        // This is a local source; we'll use a temp register for its address.
        assert(src->isContained() && src->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD));
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

void LinearScan::BuildStructStoreUnrollRegsWB(GenTreeIndStoreObj* store, ClassLayout* layout)
{
#ifndef UNIX_AMD64_ABI
    unreached();
#else
    assert(layout == store->GetLayout());
    assert(layout->GetSlotCount() == 2);

    GenTree*     addr  = store->GetAddr();
    GenTreeCall* value = store->GetValue()->AsCall();

    assert(value->GetRegCount() == 2);

    regMaskTP killSet     = Compiler::compHelperCallKillSet(CORINFO_HELP_CHECKED_ASSIGN_REF);
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

void LinearScan::BuildArgStore(GenTreeArgStore* store)
{
    GenTree*  src  = store->GetOp(0);
    var_types type = store->GetArgType();

#ifdef TARGET_X86
    if (src->IsMultiRegCall() && varTypeIsStruct(src->GetType()))
    {
        assert(src->AsCall()->GetRegCount() == 2);

        BuildUse(src, RBM_NONE, 0);
        BuildUse(src, RBM_NONE, 1);

        return;
    }
#endif

    if (type == TYP_STRUCT)
    {
#ifndef WINDOWS_AMD64_ABI
        if (src->IsIntCon(0))
        {
            if (store->GetKind() == GenTreeArgStore::Kind::RepInstrZero)
            {
                BuildUse(src, RBM_RAX);
                BuildInternalIntDef(store, RBM_RDI);
                BuildInternalIntDef(store, RBM_RCX);
            }
#ifdef TARGET_X86
            else if (store->GetKind() == GenTreeArgStore::Kind::Push)
            {
                assert(src->isContained());
            }
#endif
            else
            {
                assert(store->GetKind() == GenTreeArgStore::Kind::UnrollZero);
                assert(src->isContained());

                BuildInternalFloatDef(store, internalFloatRegCandidates());
            }

            BuildInternalUses();

            return;
        }
#endif // !WINDOWS_AMD64_ABI

        assert(src->TypeIs(TYP_STRUCT));
        assert(src->isContained());

        ClassLayout* layout = compiler->typGetLayoutByNum(store->GetArgTypeNum());

        switch (store->GetKind())
        {
#ifdef TARGET_X86
            case GenTreeArgStore::Kind::Push:
                break;
#endif

            case GenTreeArgStore::Kind::Unroll:
                unsigned size;
                size = layout->GetSize();

                if (src->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD))
                {
                    size = roundUp(size, REGSIZE_BYTES);
                }

                if ((size % XMM_REGSIZE_BYTES) != 0)
                {
                    BuildInternalIntDef(store, X86_ONLY((size % 2) != 0 ? allByteRegs() :) allIntRegs());
                }

#ifdef TARGET_X86
                if (size >= XMM_REGSIZE_BYTES / 2)
#else
                if (size >= XMM_REGSIZE_BYTES)
#endif
                {
                    BuildInternalFloatDef(store, internalFloatRegCandidates());
                    SetContainsVexInstructions();
                }
                break;

            case GenTreeArgStore::Kind::RepInstrXMM:
                BuildInternalFloatDef(store, internalFloatRegCandidates());
                SetContainsVexInstructions();
                FALLTHROUGH;
            case GenTreeArgStore::Kind::RepInstr:
                BuildInternalIntDef(store, RBM_RDI);
                BuildInternalIntDef(store, RBM_RCX);
                BuildInternalIntDef(store, RBM_RSI);
                break;

            case GenTreeArgStore::Kind::GCUnrollXMM:
                BuildInternalFloatDef(store, internalFloatRegCandidates());
                SetContainsVexInstructions();
                FALLTHROUGH;
            case GenTreeArgStore::Kind::GCUnroll:
                BuildInternalIntDef(store);
                break;

            default:
                unreached();
        }

        if (src->OperIs(GT_IND_LOAD_OBJ))
        {
            BuildAddrUses(src->AsIndLoadObj()->GetAddr());
        }

        BuildInternalUses();

        return;
    }

#ifndef WINDOWS_AMD64_ABI
    if (type == TYP_SIMD12)
    {
        BuildUse(src);

        if (!compiler->compOpportunisticallyDependsOn(InstructionSet_SSE41))
        {
            BuildInternalFloatDef(store);
            BuildInternalUses();
        }

        return;
    }
#endif

    if (!src->isContained())
    {
        BuildUse(src X86_ARG(varTypeIsByte(type) ? allByteRegs() : RBM_NONE));
    }
#ifdef TARGET_X86
    else if (src->OperIs(GT_IND_LOAD))
    {
        BuildAddrUses(src->AsIndLoad()->GetAddr());
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

void LinearScan::BuildDivMod(GenTreeOp* tree)
{
    assert(tree->OperIs(GT_SDIV, GT_SREM, GT_UDIV, GT_UREM) && varTypeIsIntegral(tree->GetType()));

    GenTree* op1 = tree->GetOp(0);
    GenTree* op2 = tree->GetOp(1);

#ifdef TARGET_X86
    if (op1->OperIs(GT_LONG))
    {
        assert(tree->OperIs(GT_UREM));
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

    BuildDelayFreeOperandUses(op2, op1, allIntRegs() & ~(RBM_RAX | RBM_RDX));
    BuildInternalUses();
    BuildKills(tree, RBM_RAX | RBM_RDX);
    BuildDef(tree, tree->OperIs(GT_SDIV, GT_UDIV) ? RBM_RAX : RBM_RDX);
}

void LinearScan::BuildIntrinsic(GenTreeIntrinsic* tree)
{
    GenTree* op1 = tree->GetOp(0);
    assert(tree->gtOp2 == nullptr);

    if (!op1->isContained())
    {
        tgtPrefUse = BuildUse(op1);
    }
    else if (op1->OperIs(GT_IND_LOAD))
    {
        BuildAddrUses(op1->AsIndLoad()->GetAddr());
    }

    BuildDef(tree);
}

void LinearScan::SetContainsVexInstructions()
{
    if (compiler->codeGen->UseVexEncoding())
    {
        compiler->codeGen->SetContainsVexInstructions();
    }
}

#ifdef FEATURE_HW_INTRINSICS
void LinearScan::BuildHWIntrinsic(GenTreeHWIntrinsic* node)
{
    if (node->IsVec())
    {
        SetContainsVexInstructions();

        if (node->TypeIs(TYP_SIMD32))
        {
            compiler->codeGen->SetContainsVex256Instructions();
        }
    }

    unsigned numOps = node->GetNumOps();
    X86_ONLY(regMaskTP dstCandidates = RBM_NONE;)

    if (numOps != 0)
    {
        NamedIntrinsic intrinsicId = node->GetIntrinsic();

        GenTree* op1    = node->GetOp(0);
        GenTree* op2    = numOps >= 2 ? node->GetOp(1) : nullptr;
        GenTree* op3    = numOps >= 3 ? node->GetOp(2) : nullptr;
        GenTree* lastOp = node->GetLastOp();

        if (HWIntrinsicInfo::HasIMM(intrinsicId) && varActualTypeIsInt(lastOp->GetType()) &&
            !lastOp->IsContainedIntCon())
        {
            // We need two extra reg when lastOp isn't a constant so the offset
            // into the jump table for the fallback path can be computed.
            BuildInternalIntDef(node);
            BuildInternalIntDef(node);
        }

        var_types baseType  = node->GetVecEltType();
        bool      isRMW     = node->IsRMW(compiler);
        bool      buildUses = true;

        auto BuildOperand = [this](GenTree* node) {
            if (!node->isContained())
            {
                BuildUse(node);
            }
            else if (node->OperIs(GT_IND_LOAD))
            {
                BuildAddrUses(node->AsIndLoad()->GetAddr());
            }
            else if (GenTreeAddrMode* addrMode = node->IsAddrMode())
            {
                BuildAddrModeUses(addrMode);
            }
            else if (GenTreeHWIntrinsic* hwi = node->IsHWIntrinsic())
            {
                if (hwi->IsMemoryLoad())
                {
                    BuildAddrUses(hwi->GetOp(0));
                }
                // TODO-MIKE-Review: What is this for?
                else if (hwi->GetNumOps() >= 1)
                {
                    BuildUse(hwi->GetOp(0));
                }
            }
        };

        // Create internal temps, and handle any other special requirements.
        // Note that the default case for building uses will handle the RMW flag,
        // but if the uses are built in the individual cases, buildUses is set to
        // false, and any RMW handling (delayFree) must be handled within the case.
        switch (intrinsicId)
        {
            case NI_VEC_REGCAST:
                assert(numOps == 1);
                assert(varTypeUsesVecReg(op1->GetType()));

                if (!op1->isContained())
                {
                    tgtPrefUse = BuildUse(op1);
                    buildUses  = false;
                }
                break;

            case NI_VEC_ITOV:
                assert(numOps == 1);
                assert(varTypeIsIntegral(baseType));
                break;

            case NI_VEC_EXTRACT:
                assert(numOps == 2);
                assert(op2->IsIntCon() || op1->isContained());

                if (varTypeIsFloating(baseType) && !op1->isContained() && op2->IsIntCon(0))
                {
                    tgtPrefUse = BuildUse(op1);
                    buildUses  = false;
                }
                break;

            case NI_VEC_ZEXT:
            case NI_VEC_TRUNC:
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
                assert(isRMW);
                assert(!compiler->codeGen->UseVexEncoding());

                tgtPrefUse = BuildUse(op1);

                if (op2->isContained())
                {
                    BuildOperand(op2);
                }
                else
                {
                    BuildDelayFreeUse(op2, op1);
                }

                BuildDelayFreeUse(op3, op1, RBM_XMM0);
                buildUses = false;
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
                BuildDelayFreeOperandUses(op2, op1, varTypeIsByte(baseType) ? allByteRegs() : RBM_NONE);
                buildUses = false;
                break;
#endif // TARGET_X86

            case NI_BMI2_MultiplyNoFlags:
            case NI_BMI2_X64_MultiplyNoFlags:
                assert((numOps == 2) || (numOps == 3));

                BuildUse(op1, RBM_EDX);
                BuildOperandUses(op2);

                if (numOps == 3)
                {
                    BuildDelayFreeUse(op3, op1);
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

                const bool isScalar = HWIntrinsicInfo::IsXmmScalar(intrinsicId);

                if (op2->isContained())
                {
                    // 132 form: op1 = (op1 * op3) + [op2]

                    tgtPrefUse = BuildUse(op1);
                    BuildOperand(op2);
                    BuildDelayFreeUse(op3, op1);
                }
                else if (op1->isContained())
                {
                    assert(!isScalar);

                    // 231 form: op3 = (op2 * op3) + [op1]

                    tgtPrefUse = BuildUse(op3);
                    BuildOperand(op1);
                    BuildDelayFreeUse(op2, op1);
                }
                else
                {
                    // 213 form: op1 = (op2 * op1) + [op3]

                    tgtPrefUse = BuildUse(op1);

                    if (isScalar)
                    {
                        BuildDelayFreeUse(op2, op1);
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
                        BuildDelayFreeUse(op3, op1);
                    }
                }

                buildUses = false;
                break;
            }

            case NI_AVXVNNI_MultiplyWideningAndAdd:
            case NI_AVXVNNI_MultiplyWideningAndAddSaturate:
                assert(numOps == 3);

                tgtPrefUse = BuildUse(op1);
                BuildDelayFreeUse(op2, op1);

                if (op3->isContained())
                {
                    BuildOperand(op3);
                }
                else
                {
                    BuildDelayFreeUse(op3, op1);
                }

                buildUses = false;
                break;

            case NI_AVX2_GATHERD:
            case NI_AVX2_GATHERQ:
                if (numOps == 3)
                {
                    assert(op3->IsContainedIntCon());
                    assert(!isRMW);

                    BuildUse(op1);
                    BuildDelayFreeUse(op2);
                }
                else
                {
                    assert(numOps == 5);
                    assert(node->GetOp(4)->IsContainedIntCon());
                    assert(!isRMW);

                    BuildUse(op1);
                    BuildUse(op2);
                    BuildDelayFreeUse(op3);
                    BuildDelayFreeUse(node->GetOp(3));
                }

                BuildInternalFloatDef(node, allFloatRegs());
                setInternalRegsDelayFree = true;
                buildUses                = false;
                break;

            default:
                assert(NI_HW_INTRINSIC_FIRST <= intrinsicId && intrinsicId <= NI_HW_INTRINSIC_LAST);
                break;
        }

        if (buildUses)
        {
            assert((numOps > 0) && (numOps < 4));

            if (node->IsMemoryLoadOrStore())
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
                if (op2->IsHWIntrinsic() && op2->AsHWIntrinsic()->IsMemoryLoad() && op2->isContained())
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
                        BuildDelayFreeOperandUses(op2, op1);
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
                        BuildDelayFreeOperandUses(op3, op1);
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
        RefPosition* def = BuildDef(node X86_ARG(dstCandidates));

        if (node->IsVecZero())
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

void LinearScan::BuildConv(GenTreeUnOp* conv)
{
    assert(conv->OperIs(GT_CONV) && varTypeIsSmallInt(conv->GetType()));

    GenTree* src = conv->GetOp(0);
#ifdef TARGET_X86
    assert(!src->TypeIs(TYP_LONG));
#endif

    if (!src->isContained())
    {
        BuildUse(src X86_ARG(varTypeIsByte(conv->GetType()) ? allByteRegs() : RBM_NONE));
    }
    else
    {
        assert(src->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD));
    }

    BuildDef(conv);
}

void LinearScan::BuildLoadInd(GenTreeIndir* load)
{
    assert(load->OperIs(GT_IND_LOAD) && !load->TypeIs(TYP_STRUCT));

#ifdef FEATURE_SIMD
    if (load->TypeIs(TYP_SIMD12) && !compiler->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        BuildInternalFloatDef(load);
        // We need an internal register different from the destination
        // register and both registers are used at the same time.
        setInternalRegsDelayFree = true;
    }
#endif

    BuildAddrUses(load->GetAddr());
    BuildInternalUses();
    BuildDef(load);
}

void LinearScan::BuildIndStore(GenTreeIndir* store)
{
    assert(store->OperIs(GT_IND_STORE) && !store->TypeIs(TYP_STRUCT));

    GenTree* value = store->GetValue();

#ifdef FEATURE_SIMD
    if (store->TypeIs(TYP_SIMD12) && value->isContained())
    {
#ifdef TARGET_64BIT
        BuildInternalIntDef(store);
#else
        BuildInternalFloatDef(store);
#endif
        BuildAddrUses(store->GetAddr());

        if (value->OperIs(GT_IND_LOAD))
        {
            BuildAddrUses(value->AsIndLoad()->GetAddr());
        }

        BuildInternalUses();

        return;
    }
#endif // FEATURE_SIMD

    BuildAddrUses(store->GetAddr());

    bool isShift = false;

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

#ifdef FEATURE_SIMD
    if (store->TypeIs(TYP_SIMD12) && !value->IsVecZero() &&
        !compiler->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        BuildInternalFloatDef(store);
    }
#endif

    BuildInternalUses();
}

void LinearScan::BuildMul(GenTreeOp* mul)
{
    assert(mul->OperIs(GT_MUL, GT_OVF_SMUL, GT_OVF_UMUL) && varTypeIsIntegral(mul->GetType()));

    GenTree* op1 = mul->GetOp(0);
    GenTree* op2 = mul->GetOp(1);

    if (op2->IsContainedIntCon())
    {
        BuildOperandUses(op1);
    }
    else
    {
        BuildRMWUses(mul);
    }

    if (mul->OperIs(GT_OVF_UMUL))
    {
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
    assert(mul->OperIs(GT_SMULH, GT_UMULH, GT_SMULL, GT_UMULL));
#else
    assert(mul->OperIs(GT_SMULH, GT_UMULH));
#endif
    assert(varTypeIsIntegral(mul->GetType()));

    GenTree* op1 = mul->GetOp(0);
    GenTree* op2 = mul->GetOp(1);

    BuildRMWUses(mul);
    BuildKills(mul, RBM_RAX | RBM_RDX);

#ifdef TARGET_X86
    if (mul->OperIs(GT_SMULL, GT_UMULL))
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

void LinearScan::BuildCmp(GenTreeOp* cmp)
{
    assert(cmp->OperIsCompare() || cmp->OperIs(GT_CMP));

    GenTree* op1 = cmp->GetOp(0);
    GenTree* op2 = cmp->GetOp(1);

#ifdef TARGET_X86
    bool needByteRegs = false;

    if (varTypeIsByte(op1->GetType()) && varTypeIsByte(op2->GetType()))
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

    regMaskTP opCandidates = needByteRegs ? allByteRegs() : RBM_NONE;
#endif // TARGET_X86

    BuildOperandUses(op1 X86_ARG(opCandidates));
    BuildOperandUses(op2 X86_ARG(opCandidates));

    if (!cmp->TypeIs(TYP_VOID))
    {
        BuildDef(cmp X86_ARG(allByteRegs()));
    }
}

#endif // TARGET_XARCH
