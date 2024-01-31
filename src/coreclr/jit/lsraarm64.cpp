// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_ARM64

#include "lsra.h"
#include "emit.h"

void LinearScan::BuildNode(GenTree* tree)
{
    assert(!tree->isContained());

    switch (tree->GetOper())
    {
        case GT_LCL_VAR:
        case GT_LCL_FLD:
            assert(!compiler->lvaGetDesc(tree->AsLclVarCommon())->IsRegCandidate());

            // Need an additional register to read upper 4 bytes of Vector3.
            if (tree->TypeIs(TYP_SIMD12))
            {
                // We need an internal register different from targetReg in which 'interlocked' produces its result
                // because both targetReg and internal reg will be in use at the same time.
                BuildInternalFloatDef(tree, allSIMDRegs());
                setInternalRegsDelayFree = true;
                BuildInternalUses();
            }

            BuildDef(tree);
            break;

        case GT_STORE_LCL_VAR:
            BuildStoreLclVar(tree->AsLclVar());
            break;

        case GT_STORE_LCL_FLD:
            BuildStoreLclFld(tree->AsLclFld());
            break;

        case GT_PROF_HOOK:
            BuildKills(tree, getKillSetForProfilerHook());
            break;

        case GT_START_PREEMPTGC:
            BuildKills(tree, RBM_NONE);
            break;

        case GT_CNS_DBL:
            if (!emitter::emitIns_valid_imm_for_fmov(tree->AsDblCon()->GetValue()))
            {
                // Reserve register to load constant from memory (IF_LARGELDC)
                BuildInternalIntDef(tree);
                BuildInternalUses();
            }
            FALLTHROUGH;
        case GT_CNS_INT:
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

        case GT_SWITCH_TABLE:
            BuildInternalIntDef(tree);
            BuildUse(tree->AsOp()->GetOp(0));
            BuildUse(tree->AsOp()->GetOp(1));
            BuildInternalUses();
            break;

        case GT_FADD:
        case GT_FSUB:
        case GT_FMUL:
        case GT_FDIV:
        case GT_MULHI:
        case GT_DIV:
        case GT_UDIV:
            BuildUse(tree->AsOp()->GetOp(0));
            BuildUse(tree->AsOp()->GetOp(1));
            BuildDef(tree);
            break;

        case GT_ADD:
        case GT_SUB:
        case GT_AND:
        case GT_OR:
        case GT_XOR:
        case GT_LSH:
        case GT_RSH:
        case GT_RSZ:
        case GT_ROR:
            BuildUse(tree->AsOp()->GetOp(0));

            if (!tree->AsOp()->GetOp(1)->isContained())
            {
                BuildUse(tree->AsOp()->GetOp(1));
            }
            FALLTHROUGH;
        case GT_JMPTABLE:
        case GT_LCL_ADDR:
        case GT_CONST_ADDR:
        case GT_PHYSREG:
        case GT_LABEL:
        case GT_SETCC:
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
            break;

        case GT_JCMP:
            BuildUse(tree->AsOp()->GetOp(0));
            break;

        case GT_RETURNTRAP:
            BuildUse(tree->AsUnOp()->GetOp(0));
            BuildKills(tree, compiler->compHelperCallKillSet(CORINFO_HELP_STOP_FOR_GC));
            break;

        case GT_MUL:
            if (tree->gtOverflow())
            {
                BuildInternalIntDef(tree);
                setInternalRegsDelayFree = true;
            }

            BuildUse(tree->AsOp()->GetOp(0));
            BuildUse(tree->AsOp()->GetOp(1));

            if (tree->gtOverflow())
            {
                BuildInternalUses();
            }

            BuildDef(tree);
            break;

        case GT_INTRINSIC:
            switch (tree->AsIntrinsic()->GetIntrinsic())
            {
                GenTree* op1;

                case NI_System_Math_Abs:
                case NI_System_Math_Ceiling:
                case NI_System_Math_Floor:
                case NI_System_Math_Round:
                case NI_System_Math_Sqrt:
                    op1 = tree->AsIntrinsic()->GetOp(0);
                    assert(varTypeIsFloating(op1->GetType()) && (op1->GetType() == tree->GetType()));
                    BuildUse(op1);
                    BuildDef(tree);
                    break;
                default:
                    unreached();
            }
            break;

        case GT_HWINTRINSIC:
            BuildHWIntrinsic(tree->AsHWIntrinsic());
            break;

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
        case GT_NEG:
        case GT_NOT:
        case GT_BSWAP:
        case GT_BSWAP16:
        case GT_INC_SATURATE:
            BuildUse(tree->AsUnOp()->GetOp(0));
            BuildDef(tree);
            break;

        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GE:
        case GT_GT:
        case GT_TEST_EQ:
        case GT_TEST_NE:
            BuildCmp(tree->AsOp());
            break;

        case GT_CKFINITE:
            BuildInternalIntDef(tree);
            BuildUse(tree->AsUnOp()->GetOp(0));
            BuildDef(tree);
            BuildInternalUses();
            break;

        case GT_CMPXCHG:
            BuildCmpXchg(tree->AsCmpXchg());
            break;

        case GT_LOCKADD:
        case GT_XORR:
        case GT_XAND:
        case GT_XADD:
        case GT_XCHG:
            BuildInterlocked(tree->AsOp());
            break;

#if FEATURE_ARG_SPLIT
        case GT_PUTARG_SPLIT:
            BuildPutArgSplit(tree->AsPutArgSplit());
            break;
#endif

        case GT_PUTARG_STK:
            BuildPutArgStk(tree->AsPutArgStk());
            break;

        case GT_PUTARG_REG:
            BuildPutArgReg(tree->AsUnOp());
            break;

        case GT_CALL:
            BuildCall(tree->AsCall());
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
            BuildInternalIntDef(tree);
            setInternalRegsDelayFree = true;
            // The lifetime of the arrObj must be extended because it is
            // used multiple times while the result is being computed.
            setDelayFree(BuildUse(tree->AsArrIndex()->ArrObj()));
            BuildUse(tree->AsArrIndex()->IndexExpr());
            BuildInternalUses();
            BuildDef(tree);
            break;

        case GT_ARR_OFFSET:
            // This consumes the offset, if any, the arrObj and the effective index,
            // and produces the flattened offset for this dimension.
            if (!tree->AsArrOffs()->GetOp(0)->isContained())
            {
                BuildUse(tree->AsArrOffs()->GetOp(0));
            }

            BuildUse(tree->AsArrOffs()->GetOp(1));
            BuildUse(tree->AsArrOffs()->GetOp(2));
            BuildInternalIntDef(tree);
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
                GenTreeStoreInd* store = tree->AsStoreInd();

                BuildIndir(store);

                if (!store->GetValue()->isContained())
                {
                    BuildUse(store->GetValue());
                }
            }
            break;

        case GT_NULLCHECK:
        case GT_IND:
            BuildIndir(tree->AsIndir());
            break;

        case GT_CATCH_ARG:
            BuildDef(tree, RBM_EXCEPTION_OBJECT);
            break;

        case GT_INDEX_ADDR:
            BuildUse(tree->AsOp()->GetOp(0));
            BuildUse(tree->AsOp()->GetOp(1));
            BuildInternalIntDef(tree);
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

    // TODO-MIKE-Review: This does not check for a missing base like ARM version does.
    // But then there's little point in building such LEAs on ARM64...

    if (((lea->GetIndex() != nullptr) && (lea->GetOffset() != 0)) ||
        !emitter::emitIns_valid_imm_for_add(lea->GetOffset(), EA_8BYTE))
    {
        BuildInternalIntDef(lea);
        BuildInternalUses();
    }

    BuildDef(lea);
}

void LinearScan::BuildCmpXchg(GenTreeCmpXchg* cmpxchg)
{
    if (!compiler->compOpportunisticallyDependsOn(InstructionSet_Atomics))
    {
        // For ARMv8 exclusives requires a single internal register
        BuildInternalIntDef(cmpxchg);
    }

    // For ARMv8 exclusives the lifetime of the addr and data must be extended because
    // it may be used used multiple during retries

    // For ARMv8.1 atomic cas the lifetime of the addr and data must be extended to prevent
    // them being reused as the target register which must be destroyed early

    RefPosition* locationUse = BuildUse(cmpxchg->GetOp(0));
    setDelayFree(locationUse);

    RefPosition* valueUse = BuildUse(cmpxchg->GetOp(1));
    setDelayFree(valueUse);

    if (!cmpxchg->GetOp(2)->isContained())
    {
        RefPosition* comparandUse = BuildUse(cmpxchg->GetOp(2));

        // For ARMv8 exclusives the lifetime of the comparand must be extended because
        // it may be used used multiple during retries
        if (!compiler->compOpportunisticallyDependsOn(InstructionSet_Atomics))
        {
            setDelayFree(comparandUse);
        }
    }

    // Internals may not collide with target
    setInternalRegsDelayFree = true;
    BuildInternalUses();
    BuildDef(cmpxchg);
}

void LinearScan::BuildInterlocked(GenTreeOp* interlocked)
{
    if (!compiler->compOpportunisticallyDependsOn(InstructionSet_Atomics))
    {
        // GT_XCHG requires a single internal register; the others require two.
        BuildInternalIntDef(interlocked);

        if (!interlocked->OperIs(GT_XCHG))
        {
            BuildInternalIntDef(interlocked);
        }
    }
    else if (interlocked->OperIs(GT_XAND))
    {
        // for ldclral we need an internal register.
        BuildInternalIntDef(interlocked);
    }

    assert(!interlocked->gtGetOp1()->isContained());

    RefPosition* op1Use = BuildUse(interlocked->GetOp(0));
    RefPosition* op2Use = nullptr;

    if (!interlocked->GetOp(1)->isContained())
    {
        op2Use = BuildUse(interlocked->GetOp(1));
    }

    // For ARMv8 exclusives the lifetime of the addr and data must be extended because
    // it may be used used multiple during retries
    if (!compiler->compOpportunisticallyDependsOn(InstructionSet_Atomics))
    {
        // Internals may not collide with target
        if (!interlocked->TypeIs(TYP_VOID))
        {
            setDelayFree(op1Use);

            if (op2Use != nullptr)
            {
                setDelayFree(op2Use);
            }

            setInternalRegsDelayFree = true;
        }

        BuildInternalUses();
    }

    if (!interlocked->TypeIs(TYP_VOID))
    {
        BuildDef(interlocked);
    }
}

void LinearScan::BuildLclHeap(GenTreeUnOp* tree)
{
    // Size                   Init Memory  # temp regs
    // 0                      don't care   0
    // const <= 6 reg words   don't care   0
    // const < PageSize       No           0
    // > 6 reg words          Yes          0
    // variable               Yes          0
    // variable               No           2

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

            if ((sizeVal / (REGSIZE_BYTES * 2) > 4) && (sizeVal >= compiler->eeGetPageSize()))
            {
                tempRegCount = 2;
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

void LinearScan::BuildHWIntrinsic(GenTreeHWIntrinsic* node)
{
    const HWIntrinsic intrin(node);

    if ((intrin.id == NI_Vector64_GetElement) || (intrin.id == NI_Vector128_GetElement))
    {
        BuildHWIntrinsicGetElement(node);

        return;
    }

    const bool hasImmediateOperand = HWIntrinsicInfo::HasImmediateOperand(intrin.id);

    if (hasImmediateOperand && !HWIntrinsicInfo::NoJmpTableImm(intrin.id))
    {
        // We may need to allocate an additional general-purpose register when an intrinsic
        // has a non-const immediate operand and the intrinsic does not have an alternative
        // non-const fallback form. However, for a case when the operand can take only two
        // possible values - zero and one the codegen can use cbnz to do conditional branch,
        // so such register is not needed.

        int      immLowerBound = 0;
        int      immUpperBound = 0;
        unsigned immVecSize;

        if (intrin.category == HW_Category_SIMDByIndexedElement)
        {
            assert((node->GetNumOps() == 3) || (node->GetNumOps() == 4));
            var_types indexedElementOpType = node->GetOp(node->GetNumOps() - 2)->GetType();
            assert(varTypeIsSIMD(indexedElementOpType));
            immVecSize = varTypeSize(indexedElementOpType);
        }
        else
        {
            immVecSize = node->GetSimdSize();
        }

        HWIntrinsicInfo::lookupImmBounds(intrin.id, immVecSize, intrin.baseType, &immLowerBound, &immUpperBound);
        GenTree* immOp = nullptr;

        if ((immLowerBound != 0) || (immUpperBound != 1))
        {
            if ((intrin.category == HW_Category_SIMDByIndexedElement) ||
                (intrin.category == HW_Category_ShiftLeftByImmediate) ||
                (intrin.category == HW_Category_ShiftRightByImmediate))
            {
                immOp = node->GetLastOp();
            }
            else
            {
                switch (intrin.id)
                {
                    case NI_AdvSimd_DuplicateSelectedScalarToVector64:
                    case NI_AdvSimd_DuplicateSelectedScalarToVector128:
                    case NI_AdvSimd_Extract:
                    case NI_AdvSimd_Insert:
                    case NI_AdvSimd_InsertScalar:
                    case NI_AdvSimd_LoadAndInsertScalar:
                    case NI_AdvSimd_Arm64_DuplicateSelectedScalarToVector128:
                        immOp = node->GetOp(1);
                        break;
                    case NI_AdvSimd_ExtractVector64:
                    case NI_AdvSimd_ExtractVector128:
                    case NI_AdvSimd_StoreSelectedScalar:
                        immOp = node->GetOp(2);
                        break;
                    case NI_AdvSimd_Arm64_InsertSelectedScalar:
                        assert(node->GetOp(1)->IsContainedIntCon());
                        assert(node->GetOp(3)->IsContainedIntCon());
                        break;
                    default:
                        unreached();
                }
            }
        }

        if ((immOp != nullptr) && !immOp->IsContainedIntCon())
        {
            BuildInternalIntDef(node);
        }
    }

    // Determine whether this is an RMW operation where op2+ must be marked delayFree so that it
    // is not allocated the same register as the target.
    const bool isRMW = node->isRMWHWIntrinsic(compiler);

    if (intrin.op1 != nullptr)
    {
        bool vecRegToVecRegMove = false;

        if ((intrin.id == NI_Vector64_CreateScalarUnsafe) || (intrin.id == NI_Vector128_CreateScalarUnsafe) ||
            (intrin.id == NI_Vector64_CreateScalar) || (intrin.id == NI_Vector128_CreateScalar))
        {
            vecRegToVecRegMove = varTypeIsFloating(intrin.op1->GetType());
        }
        else if (intrin.id == NI_AdvSimd_Arm64_DuplicateToVector64)
        {
            vecRegToVecRegMove = intrin.op1->TypeIs(TYP_DOUBLE);
        }

        if (node->OperIsMemoryLoadOrStore())
        {
            BuildAddrUses(intrin.op1);
        }
        else if (!intrin.op1->isContained())
        {
            RefPosition* use = BuildUse(intrin.op1);

            // If we have an RMW intrinsic or an intrinsic with simple move semantic
            // between two SIMD registers, we want to preference op1Reg to the target
            // if op1 is not contained.
            if (isRMW || vecRegToVecRegMove)
            {
                tgtPrefUse = use;
            }
        }
    }

    if ((intrin.category == HW_Category_SIMDByIndexedElement) && (varTypeSize(intrin.baseType) == 2))
    {
        // Some "Advanced SIMD scalar x indexed element" and "Advanced SIMD vector x indexed element"
        // instructions (e.g. "MLA (by element)") have encoding that restricts what registers that
        // can be used for the indexed element when the element size is H (i.e. 2 bytes).
        assert(intrin.op2 != nullptr);

        GenTree* immOp;

        if ((intrin.op4 != nullptr) || ((intrin.op3 != nullptr) && !hasImmediateOperand))
        {
            if (isRMW)
            {
                BuildDelayFreeUse(intrin.op2, nullptr);
                BuildDelayFreeUse(intrin.op3, nullptr, RBM_ASIMD_INDEXED_H_ELEMENT_ALLOWED_REGS);
            }
            else
            {
                BuildUse(intrin.op2);
                BuildUse(intrin.op3, RBM_ASIMD_INDEXED_H_ELEMENT_ALLOWED_REGS);
            }

            immOp = intrin.op4;
        }
        else
        {
            assert(!isRMW);

            BuildUse(intrin.op2, RBM_ASIMD_INDEXED_H_ELEMENT_ALLOWED_REGS);
            immOp = intrin.op3;
        }

        if (immOp != nullptr)
        {
            assert(hasImmediateOperand);
            assert(varTypeIsIntegral(immOp->GetType()));

            if (!immOp->IsContainedIntCon())
            {
                BuildUse(immOp);
            }
        }
    }
    else if (node->GetNumOps() > 1)
    {
        GenTree* rmwOp = node->GetOp(0);

        for (unsigned i = 1; i < node->GetNumOps(); i++)
        {
            GenTree* op = node->GetOp(i);

            if (!op->isContained())
            {
                if (isRMW)
                {
                    BuildDelayFreeUse(op, rmwOp);
                }
                else
                {
                    BuildUse(op);
                }
            }
        }
    }

    BuildInternalUses();

    if (!node->TypeIs(TYP_VOID))
    {
        BuildDef(node);
    }
}

void LinearScan::BuildHWIntrinsicGetElement(GenTreeHWIntrinsic* node)
{
    assert((node->GetIntrinsic() == NI_Vector64_GetElement) || (node->GetIntrinsic() == NI_Vector128_GetElement));

    GenTree* vec   = node->GetOp(0);
    GenTree* index = node->GetOp(1);

    if (!vec->isContained())
    {
        RefPosition* use = BuildUse(vec);

        if (varTypeIsFloating(node->GetType()) && index->IsIntegralConst(0))
        {
            tgtPrefUse = use;
        }
    }
    else if (!vec->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        BuildAddrUses(vec->AsIndir()->GetAddr());
    }
    else if (!index->IsIntCon())
    {
        BuildInternalIntDef(node);
    }

    if (!index->IsContainedIntCon())
    {
        BuildUse(index);
    }

    BuildInternalUses();
    BuildDef(node);
}

#endif // TARGET_ARM64
