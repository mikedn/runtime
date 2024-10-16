// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef FEATURE_HW_INTRINSICS

#include "emit.h"
#include "codegen.h"
#include "sideeffects.h"
#include "lower.h"

#if DEBUG
static bool IsContainableHWIntrinsicOp(Compiler* compiler, GenTreeHWIntrinsic* node, GenTree* op)
{
    // The Lowering::IsContainableHWIntrinsicOp call is not quite right, since it follows
    // pre-register allocation logic. However, this check is still important due to the
    // various containment rules that SIMD intrinsics follow.
    //
    // We use isContainable to track the special HWIntrinsic node containment rules (for
    // things like LoadAligned and LoadUnaligned) and we use the supportsRegOptional check
    // to support general-purpose loads (both from stack spillage and for isUsedFromMemory
    // contained nodes, in the case where the register allocator decided to not allocate a
    // register in the first place).

    bool supportsRegOptional = false;
    bool isContainable       = Lowering::IsContainableHWIntrinsicOp(compiler, node, op, &supportsRegOptional);
    return isContainable || supportsRegOptional || op->OperIs(GT_IND_LOAD);
}
#endif // DEBUG

static bool genIsTableDrivenHWIntrinsic(NamedIntrinsic intrinsicId, HWIntrinsicCategory category)
{
    // TODO - make more categories to the table-driven framework
    // HW_Category_Helper and HW_Flag_MultiIns/HW_Flag_SpecialCodeGen usually need manual codegen

    const bool tableDrivenCategory =
        (category != HW_Category_Special) && (category != HW_Category_Scalar) && (category != HW_Category_Helper);
    const bool tableDrivenFlag =
        !HWIntrinsicInfo::GeneratesMultipleIns(intrinsicId) && !HWIntrinsicInfo::HasSpecialCodegen(intrinsicId);
    return tableDrivenCategory && tableDrivenFlag;
}

void CodeGen::genHWIntrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic         intrinsicId = node->GetIntrinsic();
    CORINFO_InstructionSet isa         = HWIntrinsicInfo::lookupIsa(intrinsicId);
    HWIntrinsicCategory    category    = HWIntrinsicInfo::lookupCategory(intrinsicId);
    int                    numArgs     = node->GetNumOps();

    int ival = HWIntrinsicInfo::lookupIval(intrinsicId, compiler->compOpportunisticallyDependsOn(InstructionSet_AVX));

    assert(HWIntrinsicInfo::RequiresCodegen(intrinsicId));

    if (genIsTableDrivenHWIntrinsic(intrinsicId, category))
    {
        GenTree*  op1       = node->GetOp(0);
        GenTree*  op2       = numArgs >= 2 ? node->GetOp(1) : nullptr;
        regNumber targetReg = node->GetRegNum();
        var_types baseType  = node->GetSimdBaseType();

        regNumber op1Reg = REG_NA;
        regNumber op2Reg = REG_NA;
        emitter*  emit   = GetEmitter();

        assert(numArgs >= 0);
        instruction ins = HWIntrinsicInfo::lookupIns(intrinsicId, baseType);
        assert(ins != INS_invalid);
        emitAttr simdSize = emitVecTypeSize(node->GetSimdSize());
        assert(simdSize != 0);

        switch (numArgs)
        {
            case 1:
            {
                if (node->OperIsMemoryLoad())
                {
                    genConsumeAddress(op1);
                    GetEmitter()->emitIns_R_A(ins, simdSize, node->GetRegNum(), op1);
                }
                else
                {
                    genConsumeRegs(op1);
                    op1Reg = op1->GetRegNum();

                    if ((ival != -1) && varTypeIsFloating(baseType))
                    {
                        assert((ival >= 0) && (ival <= 127));
                        if ((category == HW_Category_SIMDScalar) && HWIntrinsicInfo::CopiesUpperBits(intrinsicId))
                        {
                            assert(!op1->isContained());
                            emit->emitIns_SIMD_R_R_R_I(ins, simdSize, targetReg, op1Reg, op1Reg,
                                                       static_cast<int8_t>(ival));
                        }
                        else
                        {
                            genHWIntrinsic_R_RM_I(node, ins, static_cast<int8_t>(ival));
                        }
                    }
                    else if ((category == HW_Category_SIMDScalar) && HWIntrinsicInfo::CopiesUpperBits(intrinsicId))
                    {
                        emit->emitIns_SIMD_R_R_R(ins, simdSize, targetReg, op1Reg, op1Reg);
                    }
                    else
                    {
                        genHWIntrinsic_R_RM(node, ins, simdSize, targetReg, op1);
                    }
                }
                break;
            }

            case 2:
            {
                if (category == HW_Category_MemoryStore)
                {
                    genConsumeAddress(op1);

                    if (((intrinsicId == NI_SSE_Store) || (intrinsicId == NI_SSE2_Store)) && op2->isContained())
                    {
                        GenTreeHWIntrinsic* extract = op2->AsHWIntrinsic();

                        assert((extract->GetIntrinsic() == NI_AVX_ExtractVector128) ||
                               (extract->GetIntrinsic() == NI_AVX2_ExtractVector128));

                        regNumber regData = genConsumeReg(extract->GetOp(0));

                        ins  = HWIntrinsicInfo::lookupIns(extract->GetIntrinsic(), extract->GetSimdBaseType());
                        ival = static_cast<int>(extract->GetOp(1)->AsIntCon()->IconValue());

                        emit->emitIns_A_R_I(ins, EA_32BYTE, op1, regData, ival);
                    }
                    else
                    {
                        genConsumeReg(op2);
                        GetEmitter()->emitIns_A_R(ins, simdSize, op1, op2->GetRegNum());
                    }
                    break;
                }
                genConsumeRegs(op1);
                genConsumeRegs(op2);

                op1Reg = op1->GetRegNum();
                op2Reg = op2->GetRegNum();

                if ((op1Reg != targetReg) && (op2Reg == targetReg) && node->isRMWHWIntrinsic(compiler))
                {
                    // We have "reg2 = reg1 op reg2" where "reg1 != reg2" on a RMW intrinsic.
                    //
                    // For non-commutative intrinsics, we should have ensured that op2 was marked
                    // delay free in order to prevent it from getting assigned the same register
                    // as target. However, for commutative intrinsics, we can just swap the operands
                    // in order to have "reg2 = reg2 op reg1" which will end up producing the right code.

                    noway_assert(node->OperIsCommutative());
                    op2Reg = op1Reg;
                    op1Reg = targetReg;
                }

                if ((ival != -1) && varTypeIsFloating(baseType))
                {
                    assert((ival >= 0) && (ival <= 127));
                    genHWIntrinsic_R_R_RM_I(node, ins, static_cast<int8_t>(ival));
                }
                else if (category == HW_Category_MemoryLoad)
                {
                    // Get the address and the 'other' register.
                    GenTree*  addr;
                    regNumber otherReg;
                    if (intrinsicId == NI_AVX_MaskLoad || intrinsicId == NI_AVX2_MaskLoad)
                    {
                        addr     = op1;
                        otherReg = op2Reg;
                    }
                    else
                    {
                        addr     = op2;
                        otherReg = op1Reg;
                    }

                    if (GenTreeLclAddr* lclAddr = addr->IsLclAddr())
                    {
                        GetEmitter()->emitIns_SIMD_R_R_S(ins, simdSize, targetReg, otherReg, GetStackAddrMode(lclAddr));
                    }
                    else
                    {
                        GetEmitter()->emitIns_SIMD_R_R_A(ins, simdSize, targetReg, otherReg, addr);
                    }
                }
                else if (HWIntrinsicInfo::isImmOp(intrinsicId, op2))
                {
                    assert(ival == -1);
                    auto emitSwCase = [&](int8_t i) { genHWIntrinsic_R_RM_I(node, ins, i); };

                    if (op2->IsCnsIntOrI())
                    {
                        ssize_t ival = op2->AsIntCon()->IconValue();
                        assert((ival >= 0) && (ival <= 255));
                        emitSwCase((int8_t)ival);
                    }
                    else
                    {
                        regNumber baseReg = node->ExtractTempReg();
                        regNumber offsReg = node->GetSingleTempReg();
                        genHWIntrinsicJumpTableFallback(intrinsicId, op2Reg, baseReg, offsReg, emitSwCase);
                    }
                }
                else if (node->TypeGet() == TYP_VOID)
                {
                    genHWIntrinsic_R_RM(node, ins, simdSize, op1Reg, op2);
                }
                else
                {
                    genHWIntrinsic_R_R_RM(node, ins, simdSize);
                }
                break;
            }

            case 3:
            {
                genConsumeRegs(op1);
                op1Reg = op1->GetRegNum();

                genConsumeRegs(op2);
                op2Reg = op2->GetRegNum();

                GenTree* op3 = node->GetOp(2);
                genConsumeRegs(op3);
                regNumber op3Reg = op3->GetRegNum();

                if (HWIntrinsicInfo::isImmOp(intrinsicId, op3))
                {
                    assert(ival == -1);

                    auto emitSwCase = [&](int8_t i) { genHWIntrinsic_R_R_RM_I(node, ins, i); };

                    if (op3->IsCnsIntOrI())
                    {
                        ssize_t ival = op3->AsIntCon()->IconValue();
                        assert((ival >= 0) && (ival <= 255));
                        emitSwCase((int8_t)ival);
                    }
                    else
                    {
                        regNumber baseReg = node->ExtractTempReg();
                        regNumber offsReg = node->GetSingleTempReg();
                        genHWIntrinsicJumpTableFallback(intrinsicId, op3Reg, baseReg, offsReg, emitSwCase);
                    }
                }
                else if (category == HW_Category_MemoryStore)
                {
                    // The Mask instructions do not currently support containment of the address.
                    assert(!op2->isContained());
                    if (intrinsicId == NI_AVX_MaskStore || intrinsicId == NI_AVX2_MaskStore)
                    {
                        emit->emitIns_AR_R_R(ins, simdSize, op2Reg, op3Reg, op1Reg, 0);
                    }
                    else
                    {
                        assert(intrinsicId == NI_SSE2_MaskMove);
                        assert(targetReg == REG_NA);

                        emit->emitIns_Mov(INS_mov, EA_PTRSIZE, REG_RDI, op3Reg, /* canSkip */ true);
                        emit->emitIns_R_R(ins, simdSize, op1Reg, op2Reg);
                    }
                }
                else
                {
                    switch (intrinsicId)
                    {
                        case NI_SSE41_BlendVariable:
                        case NI_AVX_BlendVariable:
                        case NI_AVX2_BlendVariable:
                        {
                            genHWIntrinsic_R_R_RM_R(node, ins);
                            break;
                        }
                        case NI_AVXVNNI_MultiplyWideningAndAdd:
                        case NI_AVXVNNI_MultiplyWideningAndAddSaturate:
                        {
                            assert(targetReg != REG_NA);
                            assert(op1Reg != REG_NA);
                            assert(op2Reg != REG_NA);

                            genHWIntrinsic_R_R_R_RM(ins, simdSize, targetReg, op1Reg, op2Reg, op3);
                            break;
                        }
                        default:
                        {
                            unreached();
                            break;
                        };
                    }
                }
                break;
            }

            default:
                unreached();
                break;
        }
        genProduceReg(node);
        return;
    }

    switch (isa)
    {
        case InstructionSet_Vector128:
        case InstructionSet_Vector256:
            genBaseIntrinsic(node);
            break;
        case InstructionSet_X86Base:
        case InstructionSet_X86Base_X64:
            genX86BaseIntrinsic(node);
            break;
        case InstructionSet_SSE:
        case InstructionSet_SSE_X64:
            genSSEIntrinsic(node);
            break;
        case InstructionSet_SSE2:
        case InstructionSet_SSE2_X64:
            genSSE2Intrinsic(node);
            break;
        case InstructionSet_SSE41:
        case InstructionSet_SSE41_X64:
            genSSE41Intrinsic(node);
            break;
        case InstructionSet_SSE42:
        case InstructionSet_SSE42_X64:
            genSSE42Intrinsic(node);
            break;
        case InstructionSet_AVX:
        case InstructionSet_AVX2:
            genAvxOrAvx2Intrinsic(node);
            break;
        case InstructionSet_AES:
            genAESIntrinsic(node);
            break;
        case InstructionSet_BMI1:
        case InstructionSet_BMI1_X64:
        case InstructionSet_BMI2:
        case InstructionSet_BMI2_X64:
            genBMI1OrBMI2Intrinsic(node);
            break;
        case InstructionSet_FMA:
            genFMAIntrinsic(node);
            break;
        case InstructionSet_LZCNT:
        case InstructionSet_LZCNT_X64:
            genLZCNTIntrinsic(node);
            break;
        case InstructionSet_PCLMULQDQ:
            genPCLMULQDQIntrinsic(node);
            break;
        case InstructionSet_POPCNT:
        case InstructionSet_POPCNT_X64:
            genPOPCNTIntrinsic(node);
            break;
        default:
            unreached();
            break;
    }
}

bool CodeGen::IsMemoryOperand(GenTree* op, StackAddrMode* s, GenTree** addr, ConstData** data)
{
    if (IsLocalMemoryOperand(op, s))
    {
        *addr = nullptr;
        *data = nullptr;

        return true;
    }

    if (GenTreeDblCon* dblCon = op->IsDblCon())
    {
        *addr = nullptr;
        *data = GetEmitter()->GetFloatConst(dblCon->GetValue(), dblCon->GetType());

        return true;
    }

    GenTree* loadAddr;

    if (op->OperIs(GT_IND_LOAD))
    {
        loadAddr = op->AsIndLoad()->GetAddr();
    }
#ifdef FEATURE_HW_INTRINSICS
    else if (GenTreeHWIntrinsic* intrin = op->IsHWIntrinsic())
    {
        assert(intrin->OperIsMemoryLoad());
        assert(intrin->IsUnary());

        loadAddr = intrin->GetOp(0);
    }
#endif
    else
    {
        return false;
    }

    if (GenTreeLclAddr* lclAddr = loadAddr->IsLclAddr())
    {
        assert(lclAddr->isContained());

        *s    = GetStackAddrMode(lclAddr);
        *addr = nullptr;
        *data = nullptr;
    }
    else
    {
        *addr = loadAddr;
        *data = nullptr;
    }

    return true;
}

void CodeGen::genHWIntrinsic_R_RM(
    GenTreeHWIntrinsic* node, instruction ins, emitAttr attr, regNumber reg, GenTree* rmOp)
{
    Emitter& emit = *GetEmitter();

    assert(reg != REG_NA);

    if (rmOp->isUsedFromReg())
    {
        if (IsMovIns(ins))
        {
            emit.emitIns_Mov(ins, attr, reg, rmOp->GetRegNum(), /* canSkip */ false);
        }
        else
        {
            emit.emitIns_R_R(ins, attr, reg, rmOp->GetRegNum());
        }

        return;
    }

    assert(HWIntrinsicInfo::SupportsContainment(node->GetIntrinsic()));
    assert(IsContainableHWIntrinsicOp(compiler, node, rmOp));

    StackAddrMode s;
    GenTree*      addr;
    ConstData*    data;

    if (!IsMemoryOperand(rmOp, &s, &addr, &data))
    {
        unreached();
    }
    else if (addr != nullptr)
    {
        emit.emitIns_R_A(ins, attr, reg, addr);
    }
    else if (data != nullptr)
    {
        emit.emitIns_R_C(ins, attr, reg, data);
    }
    else
    {
        emit.emitIns_R_S(ins, attr, reg, s);
    }
}

void CodeGen::genHWIntrinsic_R_RM_I(GenTreeHWIntrinsic* node, instruction ins, int8_t ival)
{
    regNumber targetReg = node->GetRegNum();
    GenTree*  op1       = node->GetOp(0);
    emitAttr  simdSize  = emitVecTypeSize(node->GetSimdSize());

    // TODO-XArch-CQ: Commutative operations can have op1 be contained
    // TODO-XArch-CQ: Non-VEX encoded instructions can have both ops contained

    assert(targetReg != REG_NA);
    assert(!node->OperIsCommutative()); // One operand intrinsics cannot be commutative

    if (op1->isContained() || op1->isUsedFromSpillTemp())
    {
        assert(HWIntrinsicInfo::SupportsContainment(node->GetIntrinsic()));
        assert(IsContainableHWIntrinsicOp(compiler, node, op1));
    }

    inst_RV_TT_IV(ins, simdSize, targetReg, op1, ival);
}

void CodeGen::inst_RV_TT_IV(instruction ins, emitAttr attr, regNumber reg1, GenTree* rmOp, int ival)
{
    assert(attr != EA_1BYTE);

    Emitter& emit = *GetEmitter();

    if (rmOp->isUsedFromReg())
    {
        emit.emitIns_SIMD_R_R_I(ins, attr, reg1, rmOp->GetRegNum(), ival);

        return;
    }

    StackAddrMode s;
    GenTree*      addr;
    ConstData*    data;

    if (!IsMemoryOperand(rmOp, &s, &addr, &data))
    {
        unreached();
    }
    else if (addr != nullptr)
    {
        emit.emitIns_R_A_I(ins, attr, reg1, addr, ival);
    }
    else if (data != nullptr)
    {
        emit.emitIns_R_C_I(ins, attr, reg1, data, ival);
    }
    else
    {
        emit.emitIns_R_S_I(ins, attr, reg1, s, ival);
    }
}

void CodeGen::genHWIntrinsic_R_R_RM(GenTreeHWIntrinsic* node, instruction ins, emitAttr attr)
{
    regNumber targetReg = node->GetRegNum();
    GenTree*  op1       = node->GetOp(0);
    GenTree*  op2       = node->GetOp(1);
    regNumber op1Reg    = op1->GetRegNum();

    assert(targetReg != REG_NA);
    assert(op1Reg != REG_NA);

    genHWIntrinsic_R_R_RM(node, ins, attr, targetReg, op1Reg, op2);
}

void CodeGen::genHWIntrinsic_R_R_RM(
    GenTreeHWIntrinsic* node, instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, GenTree* op2)
{
    assert(targetReg != REG_NA);
    assert(op1Reg != REG_NA);

    if (op2->isContained() || op2->isUsedFromSpillTemp())
    {
        assert(HWIntrinsicInfo::SupportsContainment(node->GetIntrinsic()));
        assert(IsContainableHWIntrinsicOp(compiler, node, op2));
    }

    inst_RV_RV_TT(ins, attr, targetReg, op1Reg, op2, node->isRMWHWIntrinsic(compiler));
}

void CodeGen::inst_RV_RV_TT(
    instruction ins, emitAttr size, regNumber targetReg, regNumber op1Reg, GenTree* op2, bool isRMW)
{
    assert(size != EA_1BYTE);

    // TODO-XArch-CQ: Commutative operations can have op1 be contained
    // TODO-XArch-CQ: Non-VEX encoded instructions can have both ops contained

    Emitter& emit = *GetEmitter();

    if (op2->isUsedFromReg())
    {
        regNumber op2Reg = op2->GetRegNum();

        if ((op1Reg != targetReg) && (op2Reg == targetReg) && isRMW)
        {
            // We have "reg2 = reg1 op reg2" where "reg1 != reg2" on a RMW instruction.
            //
            // For non-commutative instructions, we should have ensured that op2 was marked
            // delay free in order to prevent it from getting assigned the same register
            // as target. However, for commutative instructions, we can just swap the operands
            // in order to have "reg2 = reg2 op reg1" which will end up producing the right code.

            op2Reg = op1Reg;
            op1Reg = targetReg;
        }

        emit.emitIns_SIMD_R_R_R(ins, size, targetReg, op1Reg, op2Reg);

        return;
    }

    StackAddrMode s;
    GenTree*      addr;
    ConstData*    data;

    if (!IsMemoryOperand(op2, &s, &addr, &data))
    {
        unreached();
    }
    else if (addr != nullptr)
    {
        emit.emitIns_SIMD_R_R_A(ins, size, targetReg, op1Reg, addr);
    }
    else if (data != nullptr)
    {
        emit.emitIns_SIMD_R_R_C(ins, size, targetReg, op1Reg, data);
    }
    else
    {
        emit.emitIns_SIMD_R_R_S(ins, size, targetReg, op1Reg, s);
    }
}

void CodeGen::genHWIntrinsic_R_R_RM_I(GenTreeHWIntrinsic* node, instruction ins, int8_t ival)
{
    regNumber targetReg = node->GetRegNum();
    GenTree*  op1       = node->GetOp(0);
    GenTree*  op2       = node->GetOp(1);
    emitAttr  simdSize  = emitVecTypeSize(node->GetSimdSize());
    Emitter&  emit      = *GetEmitter();

    assert(targetReg != REG_NA);

    // TODO-XArch-CQ: Commutative operations can have op1 be contained
    // TODO-XArch-CQ: Non-VEX encoded instructions can have both ops contained

    if (op1->isContained())
    {
        assert(ins == INS_insertps);
        assert(op1->IsHWIntrinsicZero());
        assert(op2->isUsedFromReg());

        regNumber op2Reg = op2->GetRegNum();
        ival |= 0b1111 & ~(1 << ((ival >> 4) & 0b11));
        emit.emitIns_SIMD_R_R_R_I(ins, simdSize, targetReg, op2Reg, op2Reg, ival);

        return;
    }

    regNumber op1Reg = op1->GetRegNum();
    assert(op1Reg != REG_NA);

    if (op2->isUsedFromReg())
    {
        regNumber op2Reg = op2->GetRegNum();

        if ((op1Reg != targetReg) && (op2Reg == targetReg) && node->isRMWHWIntrinsic(compiler))
        {
            // We have "reg2 = reg1 op reg2" where "reg1 != reg2" on a RMW intrinsic.
            //
            // For non-commutative intrinsics, we should have ensured that op2 was marked
            // delay free in order to prevent it from getting assigned the same register
            // as target. However, for commutative intrinsics, we can just swap the operands
            // in order to have "reg2 = reg2 op reg1" which will end up producing the right code.

            noway_assert(node->OperIsCommutative());
            op2Reg = op1Reg;
            op1Reg = targetReg;
        }

        emit.emitIns_SIMD_R_R_R_I(ins, simdSize, targetReg, op1Reg, op2Reg, ival);

        return;
    }

    if (op2->IsDblConPositiveZero())
    {
        assert(ins == INS_insertps);

        ival |= 1 << ((ival >> 4) & 0b11);
        emit.emitIns_SIMD_R_R_R_I(ins, simdSize, targetReg, op1Reg, op1Reg, ival);

        return;
    }

    assert(HWIntrinsicInfo::SupportsContainment(node->GetIntrinsic()));
    assert((ins == INS_insertps) || IsContainableHWIntrinsicOp(compiler, node, op2));

    StackAddrMode s;
    GenTree*      addr;
    ConstData*    data;

    if (!IsMemoryOperand(op2, &s, &addr, &data))
    {
        unreached();
    }
    else if (addr != nullptr)
    {
        emit.emitIns_SIMD_R_R_A_I(ins, simdSize, targetReg, op1Reg, addr, ival);
    }
    else if (data != nullptr)
    {
        emit.emitIns_SIMD_R_R_C_I(ins, simdSize, targetReg, op1Reg, data, ival);
    }
    else
    {
        emit.emitIns_SIMD_R_R_S_I(ins, simdSize, targetReg, op1Reg, s, ival);
    }
}

void CodeGen::genHWIntrinsic_R_R_RM_R(GenTreeHWIntrinsic* node, instruction ins)
{
    regNumber targetReg = node->GetRegNum();
    GenTree*  op1       = node->GetOp(0);
    GenTree*  op2       = node->GetOp(1);
    GenTree*  op3       = node->GetOp(2);
    emitAttr  simdSize  = emitVecTypeSize(node->GetSimdSize());
    Emitter&  emit      = *GetEmitter();

    regNumber op1Reg = op1->GetRegNum();
    regNumber op3Reg = op3->GetRegNum();

    assert(targetReg != REG_NA);
    assert(op1Reg != REG_NA);
    assert(op3Reg != REG_NA);

    if (op2->isUsedFromReg())
    {
        emit.emitIns_SIMD_R_R_R_R(ins, simdSize, targetReg, op1Reg, op2->GetRegNum(), op3Reg);

        return;
    }

    assert(HWIntrinsicInfo::SupportsContainment(node->GetIntrinsic()));
    assert(IsContainableHWIntrinsicOp(compiler, node, op2));

    StackAddrMode s;
    GenTree*      addr;
    ConstData*    data;

    if (!IsMemoryOperand(op2, &s, &addr, &data))
    {
        unreached();
    }
    else if (addr != nullptr)
    {
        emit.emitIns_SIMD_R_R_A_R(ins, simdSize, targetReg, op1Reg, op3Reg, addr);
    }
    else if (data != nullptr)
    {
        // We can't have a DblCon operand on blend instructions.
        unreached();
    }
    else
    {
        emit.emitIns_SIMD_R_R_S_R(ins, simdSize, targetReg, op1Reg, op3Reg, s);
    }
}

void CodeGen::genHWIntrinsic_R_R_R_RM(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op2Reg, GenTree* op3)
{
    assert(targetReg != REG_NA);
    assert(op1Reg != REG_NA);
    assert(op2Reg != REG_NA);

    Emitter& emit = *GetEmitter();

    if (op3->isUsedFromReg())
    {
        emit.emitIns_SIMD_R_R_R_R(ins, attr, targetReg, op1Reg, op2Reg, op3->GetRegNum());

        return;
    }

    StackAddrMode s;
    GenTree*      addr;
    ConstData*    data;

    if (!IsMemoryOperand(op3, &s, &addr, &data))
    {
        unreached();
    }
    else if (addr != nullptr)
    {
        emit.emitIns_SIMD_R_R_R_A(ins, attr, targetReg, op1Reg, op2Reg, addr);
    }
    else if (data != nullptr)
    {
        emit.emitIns_SIMD_R_R_R_C(ins, attr, targetReg, op1Reg, op2Reg, data);
    }
    else
    {
        emit.emitIns_SIMD_R_R_R_S(ins, attr, targetReg, op1Reg, op2Reg, s);
    }
}

template <typename HWIntrinsicSwitchCaseBody>
void CodeGen::genHWIntrinsicJumpTableFallback(NamedIntrinsic            intrinsic,
                                              regNumber                 nonConstImmReg,
                                              regNumber                 baseReg,
                                              regNumber                 offsReg,
                                              HWIntrinsicSwitchCaseBody emitSwCase)
{
    assert(nonConstImmReg != REG_NA);
    // AVX2 Gather intrinsics use managed non-const fallback since they have discrete imm8 value range
    // that does work with the current compiler generated jump-table fallback
    assert(!HWIntrinsicInfo::isAVX2GatherIntrinsic(intrinsic));
    Emitter& emit = *GetEmitter();

    const unsigned maxByte = static_cast<unsigned>(HWIntrinsicInfo::lookupImmUpperBound(intrinsic) + 1);
    assert(maxByte <= 256);

    insGroup** labels;
    ConstData* data = emit.CreateTempLabelTable(&labels, maxByte, true);

    emit.emitIns_R_C(INS_lea, EA_PTRSIZE, offsReg, data);
    emit.emitIns_R_ARX(INS_mov, EA_4BYTE, offsReg, offsReg, nonConstImmReg, 4, 0);
    emit.emitIns_R_L(baseReg, compiler->fgFirstBB->emitLabel);
    emit.emitIns_R_R(INS_add, EA_PTRSIZE, offsReg, baseReg);
    emit.emitIns_R(INS_i_jmp, EA_PTRSIZE, offsReg);

    insGroup* switchTableEnd = GetEmitter()->CreateTempLabel();

    for (unsigned i = 0; i < maxByte; i++)
    {
        emit.DefineTempLabel(labels[i]);
        emitSwCase(static_cast<int8_t>(i));
        emit.emitIns_J(INS_jmp, switchTableEnd);
    }

    emit.DefineTempLabel(switchTableEnd);
}

void CodeGen::genConsumeHWIntrinsicOperands(GenTreeHWIntrinsic* node)
{
    for (GenTreeHWIntrinsic::Use& use : node->Uses())
    {
        genConsumeRegs(use.GetNode());
    }
}

void CodeGen::genBaseIntrinsic(GenTreeHWIntrinsic* node)
{
    genConsumeHWIntrinsicOperands(node);

    NamedIntrinsic intrinsicId = node->GetIntrinsic();
    regNumber      targetReg   = node->GetRegNum();
    var_types      baseType    = node->GetSimdBaseType();

    assert(varTypeIsArithmetic(baseType));
    assert(node->GetNumOps() <= 2);

    GenTree* op1 = node->GetNumOps() >= 1 ? node->GetOp(0) : nullptr;
    GenTree* op2 = node->GetNumOps() >= 2 ? node->GetOp(1) : nullptr;

    emitter*    emit   = GetEmitter();
    emitAttr    attr   = emitVecTypeSize(node->GetSimdSize());
    instruction ins    = HWIntrinsicInfo::lookupIns(intrinsicId, baseType);
    regNumber   op1Reg = (op1 == nullptr) ? REG_NA : op1->GetRegNum();

    switch (intrinsicId)
    {
        case NI_Vector128_CreateScalarUnsafe:
        case NI_Vector256_CreateScalarUnsafe:
        {
            if (varTypeIsIntegral(baseType))
            {
                genHWIntrinsic_R_RM(node, ins, emitActualTypeSize(baseType), targetReg, op1);
            }
            else
            {
                assert(varTypeIsFloating(baseType));

                attr = emitTypeSize(baseType);

                if (op1->isContained() || op1->isUsedFromSpillTemp())
                {
                    genHWIntrinsic_R_RM(node, ins, attr, targetReg, op1);
                }
                else
                {
                    // Just use movaps for reg->reg moves as it has zero-latency on modern CPUs
                    emit->emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
                }
            }
            break;
        }

        case NI_Vector128_GetElement:
        case NI_Vector256_GetElement:
            genVectorGetElement(node);
            break;

        case NI_Vector128_ToVector256:
        {
            // ToVector256 has zero-extend semantics in order to ensure it is deterministic
            // We always emit a move to the target register, even when op1Reg == targetReg,
            // in order to ensure that Bits MAXVL-1:128 are zeroed.

            attr = emitTypeSize(TYP_SIMD16);

            if (op1->isContained() || op1->isUsedFromSpillTemp())
            {
                genHWIntrinsic_R_RM(node, ins, attr, targetReg, op1);
            }
            else
            {
                // Just use movaps for reg->reg moves as it has zero-latency on modern CPUs
                emit->emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ false);
            }
            break;
        }

        case NI_Vector128_ToVector256Unsafe:
        case NI_Vector256_GetLower:
        {
            if (op1->isContained() || op1->isUsedFromSpillTemp())
            {
                genHWIntrinsic_R_RM(node, ins, attr, targetReg, op1);
            }
            else
            {
                // Just use movaps for reg->reg moves as it has zero-latency on modern CPUs
                emit->emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
            }
            break;
        }

        case NI_Vector256_get_AllBitsSet:
            assert(op1 == nullptr);
            assert(ins == INS_pcmpeqd);
            if (!compiler->compOpportunisticallyDependsOn(InstructionSet_AVX2))
            {
                assert(compiler->compIsaSupportedDebugOnly(InstructionSet_AVX));
                emit->emitIns_SIMD_R_R_R(INS_xorps, EA_32BYTE, targetReg, targetReg, targetReg);
                emit->emitIns_SIMD_R_R_R_I(INS_cmpps, EA_32BYTE, targetReg, targetReg, targetReg, 15);
                break;
            }
            FALLTHROUGH;
        case NI_Vector128_get_Zero:
        case NI_Vector256_get_Zero:
        case NI_Vector128_get_AllBitsSet:
            assert(op1 == nullptr);
            emit->emitIns_SIMD_R_R_R(ins, attr, targetReg, targetReg, targetReg);
            break;

        default:
            unreached();
    }

    genProduceReg(node);
}

void CodeGen::genVectorGetElement(GenTreeHWIntrinsic* node)
{
    var_types baseType = node->GetSimdBaseType();
    GenTree*  src      = node->GetOp(0);
    GenTree*  index    = node->GetOp(1);
    regNumber destReg  = node->GetRegNum();

    if (!src->isUsedFromReg())
    {
        regNumber baseReg;
        regNumber indexReg;
        unsigned  scale;
        int       offset;

        if (src->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD))
        {
            LclVarDsc* lcl = src->AsLclVarCommon()->GetLcl();

            bool isEBPbased;
            int  frameOffset = compiler->lvaLclFrameAddress(lcl, &isEBPbased);

#if !FEATURE_FIXED_OUT_ARGS
            if (!isEBPbased)
            {
                // Adjust the offset by the amount currently pushed on the CPU stack
                frameOffset += genStackLevel;
            }
#endif

            baseReg  = isEBPbased ? REG_EBP : REG_ESP;
            indexReg = REG_NA;
            scale    = 1;
            offset   = frameOffset + src->AsLclVarCommon()->GetLclOffs();
        }
        else if (src->AsIndir()->GetAddr()->isUsedFromReg())
        {
            baseReg  = src->AsIndir()->GetAddr()->GetRegNum();
            indexReg = REG_NA;
            scale    = 1;
            offset   = 0;
        }
        else
        {
            assert(index->IsIntCon());

            GenTreeAddrMode* am = src->AsIndir()->GetAddr()->AsAddrMode();

            baseReg  = am->GetBase()->GetRegNum();
            indexReg = am->HasIndex() ? am->GetIndex()->GetRegNum() : REG_NA;
            scale    = am->GetScale();
            offset   = am->GetOffset();
        }

        if (index->IsIntCon())
        {
            offset += index->AsIntCon()->GetInt32Value() * varTypeSize(baseType);
        }
        else
        {
            assert(indexReg == REG_NA);
            assert(scale == 1);
            assert(index->isUsedFromReg());

            indexReg = index->GetRegNum();
            scale    = varTypeSize(baseType);
        }

        GetEmitter()->emitIns_R_ARX(ins_Load(baseType), emitTypeSize(baseType), destReg, baseReg, indexReg, scale,
                                    offset);

        return;
    }

    regNumber srcReg     = src->GetRegNum();
    ssize_t   indexValue = index->AsIntCon()->GetValue();
    emitter*  emit       = GetEmitter();

    assert(varTypeIsFloating(baseType));

    if (indexValue == 0)
    {
        emit->emitIns_Mov(INS_movaps, EA_16BYTE, destReg, srcReg, true);
        return;
    }

    if (baseType == TYP_FLOAT)
    {
        if (indexValue == 1)
        {
            if (compiler->compOpportunisticallyDependsOn(InstructionSet_SSE3))
            {
                emit->emitIns_R_R(INS_movshdup, EA_16BYTE, destReg, srcReg);
            }
            else
            {
                emit->emitIns_SIMD_R_R_R_I(INS_shufps, EA_16BYTE, destReg, srcReg, srcReg, 0x55);
            }
        }
        else if (indexValue == 2)
        {
            emit->emitIns_SIMD_R_R_R(INS_unpckhps, EA_16BYTE, destReg, srcReg, srcReg);
        }
        else
        {
            assert(indexValue == 3);
            emit->emitIns_SIMD_R_R_R_I(INS_shufps, EA_16BYTE, destReg, srcReg, srcReg, -1);
        }
    }
    else
    {
        assert(baseType == TYP_DOUBLE);
        assert(indexValue == 1);

        emit->emitIns_SIMD_R_R_R(INS_unpckhpd, EA_16BYTE, destReg, srcReg, srcReg);
    }
}

void CodeGen::genX86BaseIntrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsicId = node->GetIntrinsic();

    switch (intrinsicId)
    {
        case NI_X86Base_BitScanForward:
        case NI_X86Base_BitScanReverse:
        case NI_X86Base_X64_BitScanForward:
        case NI_X86Base_X64_BitScanReverse:
        {
            GenTree*    op1        = node->GetOp(0);
            regNumber   targetReg  = node->GetRegNum();
            var_types   targetType = node->TypeGet();
            instruction ins        = HWIntrinsicInfo::lookupIns(intrinsicId, targetType);

            genConsumeHWIntrinsicOperands(node);
            genHWIntrinsic_R_RM(node, ins, emitTypeSize(targetType), targetReg, op1);
            genProduceReg(node);
            break;
        }

        default:
            unreached();
            break;
    }
}

void CodeGen::genSSEIntrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsicId = node->GetIntrinsic();
    GenTree*       op1         = node->GetNumOps() >= 1 ? node->GetOp(0) : nullptr;
    GenTree*       op2         = node->GetNumOps() >= 2 ? node->GetOp(1) : nullptr;
    regNumber      targetReg   = node->GetRegNum();
    var_types      targetType  = node->GetType();
    var_types      baseType    = node->GetSimdBaseType();

    regNumber op1Reg = REG_NA;
    emitter*  emit   = GetEmitter();

    genConsumeHWIntrinsicOperands(node);

    switch (intrinsicId)
    {
        case NI_SSE_X64_ConvertToInt64:
        case NI_SSE_X64_ConvertToInt64WithTruncation:
        {
            assert(targetType == TYP_LONG);
            assert(op1 != nullptr);
            assert(op2 == nullptr);
            instruction ins = HWIntrinsicInfo::lookupIns(intrinsicId, baseType);
            genHWIntrinsic_R_RM(node, ins, EA_8BYTE, targetReg, op1);
            break;
        }

        case NI_SSE_X64_ConvertScalarToVector128Single:
        {
            assert(baseType == TYP_LONG);
            assert(op1 != nullptr);
            assert(op2 != nullptr);
            instruction ins = HWIntrinsicInfo::lookupIns(intrinsicId, baseType);
            genHWIntrinsic_R_R_RM(node, ins, EA_8BYTE);
            break;
        }

        case NI_SSE_Prefetch0:
        case NI_SSE_Prefetch1:
        case NI_SSE_Prefetch2:
        case NI_SSE_PrefetchNonTemporal:
        {
            assert(baseType == TYP_UBYTE);
            assert(op2 == nullptr);

            // These do not support containment.
            assert(!op1->isContained());
            instruction ins = HWIntrinsicInfo::lookupIns(intrinsicId, node->GetSimdBaseType());
            op1Reg          = op1->GetRegNum();
            emit->emitIns_AR(ins, emitTypeSize(baseType), op1Reg, 0);
            break;
        }

        case NI_SSE_StoreFence:
        {
            assert(baseType == TYP_VOID);
            assert(op1 == nullptr);
            assert(op2 == nullptr);
            emit->emitIns(INS_sfence);
            break;
        }

        default:
            unreached();
            break;
    }

    genProduceReg(node);
}

void CodeGen::genSSE2Intrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsicId = node->GetIntrinsic();
    GenTree*       op1         = node->GetNumOps() >= 1 ? node->GetOp(0) : nullptr;
    GenTree*       op2         = node->GetNumOps() >= 2 ? node->GetOp(1) : nullptr;
    regNumber      targetReg   = node->GetRegNum();
    var_types      targetType  = node->GetType();
    var_types      baseType    = node->GetSimdBaseType();
    regNumber      op1Reg      = REG_NA;
    emitter*       emit        = GetEmitter();

    genConsumeHWIntrinsicOperands(node);

    switch (intrinsicId)
    {
        case NI_SSE2_X64_ConvertScalarToVector128Double:
        {
            assert(baseType == TYP_LONG);
            assert(op1 != nullptr);
            assert(op2 != nullptr);
            instruction ins = HWIntrinsicInfo::lookupIns(intrinsicId, baseType);
            genHWIntrinsic_R_R_RM(node, ins, EA_8BYTE);
            break;
        }

        case NI_SSE2_X64_ConvertScalarToVector128Int64:
        case NI_SSE2_X64_ConvertScalarToVector128UInt64:
        {
            assert(baseType == TYP_LONG || baseType == TYP_ULONG);
            assert(op1 != nullptr);
            assert(op2 == nullptr);
            instruction ins = HWIntrinsicInfo::lookupIns(intrinsicId, baseType);
            genHWIntrinsic_R_RM(node, ins, emitTypeSize(baseType), targetReg, op1);
            break;
        }

        case NI_SSE2_ConvertToInt32:
        case NI_SSE2_ConvertToInt32WithTruncation:
        case NI_SSE2_ConvertToUInt32:
        case NI_SSE2_X64_ConvertToInt64:
        case NI_SSE2_X64_ConvertToInt64WithTruncation:
        case NI_SSE2_X64_ConvertToUInt64:
        {
            assert(op2 == nullptr);
            emitAttr attr;

            if (varTypeIsIntegral(baseType))
            {
                assert(baseType == TYP_INT || baseType == TYP_UINT || baseType == TYP_LONG || baseType == TYP_ULONG);
                attr = emitActualTypeSize(baseType);
            }
            else
            {
                assert(baseType == TYP_DOUBLE || baseType == TYP_FLOAT);
                attr = emitTypeSize(targetType);
            }

            instruction ins = HWIntrinsicInfo::lookupIns(intrinsicId, baseType);
            genHWIntrinsic_R_RM(node, ins, attr, targetReg, op1);
            break;
        }

        case NI_SSE2_LoadFence:
        {
            assert(baseType == TYP_VOID);
            assert(op1 == nullptr);
            assert(op2 == nullptr);
            emit->emitIns(INS_lfence);
            break;
        }

        case NI_SSE2_MemoryFence:
        {
            assert(baseType == TYP_VOID);
            assert(op1 == nullptr);
            assert(op2 == nullptr);
            emit->emitIns(INS_mfence);
            break;
        }

        case NI_SSE2_StoreNonTemporal:
        case NI_SSE2_X64_StoreNonTemporal:
        {
            assert(baseType == TYP_INT || baseType == TYP_UINT || baseType == TYP_LONG || baseType == TYP_ULONG);
            assert(op1 != nullptr);
            assert(op2 != nullptr);

            instruction ins = HWIntrinsicInfo::lookupIns(intrinsicId, baseType);
            GetEmitter()->emitIns_A_R(ins, emitTypeSize(baseType), op1, op2->GetRegNum());
            break;
        }

        default:
            unreached();
            break;
    }

    genProduceReg(node);
}

void CodeGen::genSSE41Intrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsicId = node->GetIntrinsic();
    GenTree*       op1         = node->GetNumOps() >= 1 ? node->GetOp(0) : nullptr;
    GenTree*       op2         = node->GetNumOps() >= 2 ? node->GetOp(1) : nullptr;
    regNumber      targetReg   = node->GetRegNum();
    var_types      baseType    = node->GetSimdBaseType();

    emitter* emit = GetEmitter();

    genConsumeHWIntrinsicOperands(node);

    switch (intrinsicId)
    {
        case NI_SSE41_ConvertToVector128Int16:
        case NI_SSE41_ConvertToVector128Int32:
        case NI_SSE41_ConvertToVector128Int64:
        {
            instruction ins = HWIntrinsicInfo::lookupIns(intrinsicId, baseType);

            if (!varTypeIsSIMD(op1->gtType))
            {
                GetEmitter()->emitIns_R_A(ins, EA_16BYTE, node->GetRegNum(), op1);
            }
            else
            {
                genHWIntrinsic_R_RM(node, ins, EA_16BYTE, targetReg, op1);
            }
            break;
        }

        case NI_SSE41_Extract:
        case NI_SSE41_X64_Extract:
        {
            assert(!varTypeIsFloating(baseType));

            instruction ins  = HWIntrinsicInfo::lookupIns(intrinsicId, baseType);
            emitAttr    attr = emitActualTypeSize(node->TypeGet());

            auto emitSwCase = [&](int8_t i) { inst_RV_TT_IV(ins, attr, targetReg, op1, i); };

            if (op2->IsCnsIntOrI())
            {
                ssize_t ival = op2->AsIntCon()->IconValue();
                assert((ival >= 0) && (ival <= 255));
                emitSwCase((int8_t)ival);
            }
            else
            {
                regNumber baseReg = node->ExtractTempReg();
                regNumber offsReg = node->GetSingleTempReg();
                genHWIntrinsicJumpTableFallback(intrinsicId, op2->GetRegNum(), baseReg, offsReg, emitSwCase);
            }
            break;
        }

        default:
            unreached();
            break;
    }

    genProduceReg(node);
}

void CodeGen::genSSE42Intrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsicId = node->GetIntrinsic();
    regNumber      targetReg   = node->GetRegNum();
    GenTree*       op1         = node->GetNumOps() >= 1 ? node->GetOp(0) : nullptr;
    GenTree*       op2         = node->GetNumOps() >= 2 ? node->GetOp(1) : nullptr;
    var_types      baseType    = node->GetSimdBaseType();
    var_types      targetType  = node->GetType();
    emitter*       emit        = GetEmitter();

    genConsumeHWIntrinsicOperands(node);
    regNumber op1Reg = op1->GetRegNum();

    assert(targetReg != REG_NA);
    assert(op1Reg != REG_NA);
    assert(op2 != nullptr);
    assert(!node->OperIsCommutative());

    switch (intrinsicId)
    {
        case NI_SSE42_Crc32:
        case NI_SSE42_X64_Crc32:
        {
            assert((op2->GetRegNum() != targetReg) || (op1Reg == targetReg));
            emit->emitIns_Mov(INS_mov, emitTypeSize(targetType), targetReg, op1Reg, /* canSkip */ true);

            if ((baseType == TYP_UBYTE) || (baseType == TYP_USHORT)) // baseType is the type of the second argument
            {
                assert(targetType == TYP_INT);
                genHWIntrinsic_R_RM(node, INS_crc32, emitTypeSize(baseType), targetReg, op2);
            }
            else
            {
                assert(op1->TypeGet() == op2->TypeGet());
                assert((targetType == TYP_INT) || (targetType == TYP_LONG));
                genHWIntrinsic_R_RM(node, INS_crc32, emitTypeSize(targetType), targetReg, op2);
            }

            break;
        }

        default:
        {
            unreached();
            break;
        }
    }

    genProduceReg(node);
}

void CodeGen::genAvxOrAvx2Intrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsicId = node->GetIntrinsic();
    var_types      baseType    = node->GetSimdBaseType();
    emitAttr       attr        = emitVecTypeSize(node->GetSimdSize());
    var_types      targetType  = node->TypeGet();
    instruction    ins         = HWIntrinsicInfo::lookupIns(intrinsicId, baseType);
    int            numArgs     = node->GetNumOps();
    GenTree*       op1         = numArgs >= 1 ? node->GetOp(0) : nullptr;
    GenTree*       op2         = numArgs >= 2 ? node->GetOp(1) : nullptr;
    regNumber      op1Reg      = REG_NA;
    regNumber      op2Reg      = REG_NA;
    regNumber      targetReg   = node->GetRegNum();
    emitter*       emit        = GetEmitter();

    genConsumeHWIntrinsicOperands(node);

    switch (intrinsicId)
    {
        case NI_AVX2_ConvertToInt32:
        case NI_AVX2_ConvertToUInt32:
        {
            op1Reg = op1->GetRegNum();
            assert(numArgs == 1);
            assert((baseType == TYP_INT) || (baseType == TYP_UINT));
            instruction ins = HWIntrinsicInfo::lookupIns(intrinsicId, baseType);
            emit->emitIns_Mov(ins, emitActualTypeSize(baseType), targetReg, op1Reg, /* canSkip */ false);
            break;
        }

        case NI_AVX2_ConvertToVector256Int16:
        case NI_AVX2_ConvertToVector256Int32:
        case NI_AVX2_ConvertToVector256Int64:
        {
            instruction ins = HWIntrinsicInfo::lookupIns(intrinsicId, baseType);

            if (!varTypeIsSIMD(op1->gtType))
            {
                GetEmitter()->emitIns_R_A(ins, EA_32BYTE, node->GetRegNum(), op1);
            }
            else
            {
                genHWIntrinsic_R_RM(node, ins, EA_32BYTE, targetReg, op1);
            }
            break;
        }

        case NI_AVX2_GatherVector128:
        case NI_AVX2_GatherVector256:
        case NI_AVX2_GatherMaskVector128:
        case NI_AVX2_GatherMaskVector256:
        {
            op1Reg = op1->GetRegNum();

            op2Reg = op2->GetRegNum();

            GenTree* op3 = node->GetOp(2);

            GenTree* op4     = nullptr;
            GenTree* lastOp  = nullptr;
            GenTree* indexOp = nullptr;

            regNumber op3Reg       = REG_NA;
            regNumber op4Reg       = REG_NA;
            regNumber addrBaseReg  = REG_NA;
            regNumber addrIndexReg = REG_NA;
            regNumber maskReg      = node->ExtractTempReg(RBM_ALLFLOAT);

            if (numArgs == 5)
            {
                assert(intrinsicId == NI_AVX2_GatherMaskVector128 || intrinsicId == NI_AVX2_GatherMaskVector256);
                op4          = node->GetOp(3);
                lastOp       = node->GetOp(4);
                op3Reg       = op3->GetRegNum();
                op4Reg       = op4->GetRegNum();
                addrBaseReg  = op2Reg;
                addrIndexReg = op3Reg;
                indexOp      = op3;

                // copy op4Reg into the tmp mask register,
                // the mask register will be cleared by gather instructions
                emit->emitIns_Mov(INS_movaps, attr, maskReg, op4Reg, /* canSkip */ false);

                // copy source vector to the target register for masking merge
                emit->emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
            }
            else
            {
                assert(intrinsicId == NI_AVX2_GatherVector128 || intrinsicId == NI_AVX2_GatherVector256);
                addrBaseReg  = op1Reg;
                addrIndexReg = op2Reg;
                indexOp      = op2;
                lastOp       = op3;

                // generate all-one mask vector
                emit->emitIns_SIMD_R_R_R(INS_pcmpeqd, attr, maskReg, maskReg, maskReg);
            }

            bool isVector128GatherWithVector256Index = (targetType == TYP_SIMD16) && (indexOp->TypeGet() == TYP_SIMD32);

            // hwintrinsiclistxarch.h uses Dword index instructions in default
            if (varTypeIsLong(node->GetAuxiliaryType()))
            {
                switch (ins)
                {
                    case INS_vpgatherdd:
                        ins = INS_vpgatherqd;
                        if (isVector128GatherWithVector256Index)
                        {
                            // YMM index in address mode
                            attr = emitTypeSize(TYP_SIMD32);
                        }
                        break;
                    case INS_vpgatherdq:
                        ins = INS_vpgatherqq;
                        break;
                    case INS_vgatherdps:
                        ins = INS_vgatherqps;
                        if (isVector128GatherWithVector256Index)
                        {
                            // YMM index in address mode
                            attr = emitTypeSize(TYP_SIMD32);
                        }
                        break;
                    case INS_vgatherdpd:
                        ins = INS_vgatherqpd;
                        break;
                    default:
                        unreached();
                }
            }

            assert(lastOp->IsCnsIntOrI());
            ssize_t ival = lastOp->AsIntCon()->IconValue();
            assert((ival >= 0) && (ival <= 255));

            assert(targetReg != maskReg);
            assert(targetReg != addrIndexReg);
            assert(maskReg != addrIndexReg);
            emit->emitIns_R_AR_R(ins, attr, targetReg, maskReg, addrBaseReg, addrIndexReg, (int8_t)ival, 0);

            break;
        }

        default:
            unreached();
            break;
    }

    genProduceReg(node);
}

void CodeGen::genAESIntrinsic(GenTreeHWIntrinsic* node)
{
    NYI("Implement AES intrinsic code generation");
}

void CodeGen::genBMI1OrBMI2Intrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsicId = node->GetIntrinsic();
    regNumber      targetReg   = node->GetRegNum();
    GenTree*       op1         = node->GetNumOps() >= 1 ? node->GetOp(0) : nullptr;
    GenTree*       op2         = node->GetNumOps() >= 2 ? node->GetOp(1) : nullptr;
    var_types      targetType  = node->TypeGet();
    instruction    ins         = HWIntrinsicInfo::lookupIns(intrinsicId, targetType);
    emitter*       emit        = GetEmitter();

    assert(targetReg != REG_NA);
    assert(op1 != nullptr);

    genConsumeHWIntrinsicOperands(node);

    switch (intrinsicId)
    {
        case NI_BMI1_AndNot:
        case NI_BMI1_X64_AndNot:
        case NI_BMI1_BitFieldExtract:
        case NI_BMI1_X64_BitFieldExtract:
        case NI_BMI2_ParallelBitDeposit:
        case NI_BMI2_ParallelBitExtract:
        case NI_BMI2_X64_ParallelBitDeposit:
        case NI_BMI2_X64_ParallelBitExtract:
        case NI_BMI2_ZeroHighBits:
        case NI_BMI2_X64_ZeroHighBits:
        {
            assert(op2 != nullptr);
            assert((targetType == TYP_INT) || (targetType == TYP_LONG));
            genHWIntrinsic_R_R_RM(node, ins, emitTypeSize(node->TypeGet()));
            break;
        }

        case NI_BMI1_ExtractLowestSetBit:
        case NI_BMI1_GetMaskUpToLowestSetBit:
        case NI_BMI1_ResetLowestSetBit:
        case NI_BMI1_X64_ExtractLowestSetBit:
        case NI_BMI1_X64_GetMaskUpToLowestSetBit:
        case NI_BMI1_X64_ResetLowestSetBit:
        {
            assert(op2 == nullptr);
            assert((targetType == TYP_INT) || (targetType == TYP_LONG));
            genHWIntrinsic_R_RM(node, ins, emitTypeSize(node->TypeGet()), targetReg, op1);
            break;
        }

        case NI_BMI1_TrailingZeroCount:
        case NI_BMI1_X64_TrailingZeroCount:
        {
            assert(op2 == nullptr);
            assert((targetType == TYP_INT) || (targetType == TYP_LONG));
            genXCNTIntrinsic(node, ins);
            break;
        }

        case NI_BMI2_MultiplyNoFlags:
        case NI_BMI2_X64_MultiplyNoFlags:
        {
            int numArgs = node->GetNumOps();
            assert(numArgs == 2 || numArgs == 3);

            regNumber op1Reg = REG_NA;
            regNumber op2Reg = REG_NA;
            regNumber op3Reg = REG_NA;
            regNumber lowReg = REG_NA;

            if (numArgs == 2)
            {
                op1Reg = op1->GetRegNum();
                op2Reg = op2->GetRegNum();
                lowReg = targetReg;
            }
            else
            {
                op1Reg       = op1->GetRegNum();
                op2Reg       = op2->GetRegNum();
                GenTree* op3 = node->GetOp(2);
                op3Reg       = op3->GetRegNum();
                assert(!op3->isContained());
                assert(op3Reg != op1Reg);
                assert(op3Reg != targetReg);
                assert(op3Reg != REG_EDX);
                lowReg = node->GetSingleTempReg();
                assert(op3Reg != lowReg);
                assert(lowReg != targetReg);
            }

            // These do not support containment
            assert(!op2->isContained());
            emitAttr attr = emitTypeSize(targetType);

            // mov the first operand into implicit source operand EDX/RDX
            assert((op2Reg != REG_EDX) || (op1Reg == REG_EDX));
            emit->emitIns_Mov(INS_mov, attr, REG_EDX, op1Reg, /* canSkip */ true);

            // generate code for MULX
            genHWIntrinsic_R_R_RM(node, ins, attr, targetReg, lowReg, op2);

            // If requires the lower half result, store in the memory pointed to by op3
            if (node->GetNumOps() == 3)
            {
                emit->emitIns_AR_R(INS_mov, attr, lowReg, op3Reg, 0);
            }

            break;
        }

        default:
        {
            unreached();
            break;
        }
    }

    genProduceReg(node);
}

void CodeGen::genFMAIntrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsicId = node->GetIntrinsic();
    var_types      baseType    = node->GetSimdBaseType();
    emitAttr       attr        = emitVecTypeSize(node->GetSimdSize());
    instruction    ins         = HWIntrinsicInfo::lookupIns(intrinsicId, baseType);
    GenTree*       op1         = node->GetOp(0);
    GenTree*       op2         = node->GetOp(1);
    GenTree*       op3         = node->GetOp(2);
    regNumber      targetReg   = node->GetRegNum();

    assert(node->IsTernary());
    genConsumeHWIntrinsicOperands(node);

    regNumber op1Reg;
    regNumber op2Reg;

    bool       isCommutative   = false;
    const bool copiesUpperBits = HWIntrinsicInfo::CopiesUpperBits(intrinsicId);

    // Intrinsics with CopyUpperBits semantics cannot have op1 be contained
    assert(!copiesUpperBits || !op1->isContained());

    if (op2->isContained() || op2->isUsedFromSpillTemp())
    {
        // 132 form: op1 = (op1 * op3) + [op2]

        ins    = (instruction)(ins - 1);
        op1Reg = op1->GetRegNum();
        op2Reg = op3->GetRegNum();
        op3    = op2;
    }
    else if (op1->isContained() || op1->isUsedFromSpillTemp())
    {
        // 231 form: op3 = (op2 * op3) + [op1]

        ins    = (instruction)(ins + 1);
        op1Reg = op3->GetRegNum();
        op2Reg = op2->GetRegNum();
        op3    = op1;
    }
    else
    {
        // 213 form: op1 = (op2 * op1) + [op3]

        op1Reg = op1->GetRegNum();
        op2Reg = op2->GetRegNum();

        isCommutative = !copiesUpperBits;
    }

    if (isCommutative && (op1Reg != targetReg) && (op2Reg == targetReg))
    {
        assert(node->isRMWHWIntrinsic(compiler));

        // We have "reg2 = (reg1 * reg2) +/- op3" where "reg1 != reg2" on a RMW intrinsic.
        //
        // For non-commutative intrinsics, we should have ensured that op2 was marked
        // delay free in order to prevent it from getting assigned the same register
        // as target. However, for commutative intrinsics, we can just swap the operands
        // in order to have "reg2 = reg2 op reg1" which will end up producing the right code.

        op2Reg = op1Reg;
        op1Reg = targetReg;
    }

    genHWIntrinsic_R_R_R_RM(ins, attr, targetReg, op1Reg, op2Reg, op3);
    genProduceReg(node);
}

void CodeGen::genLZCNTIntrinsic(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_LZCNT_LeadingZeroCount || node->GetIntrinsic() == NI_LZCNT_X64_LeadingZeroCount);

    genConsumeRegs(node->GetOp(0));
    genXCNTIntrinsic(node, INS_lzcnt);
    genProduceReg(node);
}

void CodeGen::genPCLMULQDQIntrinsic(GenTreeHWIntrinsic* node)
{
    NYI("Implement PCLMULQDQ intrinsic code generation");
}

void CodeGen::genPOPCNTIntrinsic(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_POPCNT_PopCount || node->GetIntrinsic() == NI_POPCNT_X64_PopCount);

    genConsumeRegs(node->GetOp(0));
    genXCNTIntrinsic(node, INS_popcnt);
    genProduceReg(node);
}

void CodeGen::genXCNTIntrinsic(GenTreeHWIntrinsic* node, instruction ins)
{
    // LZCNT/TZCNT/POPCNT have a false dependency on the target register on Intel Sandy Bridge, Haswell, and Skylake
    // (POPCNT only) processors, so insert a `XOR target, target` to break the dependency via XOR triggering register
    // renaming, but only if it's not an actual dependency.

    GenTree*  op1        = node->GetOp(0);
    regNumber sourceReg1 = REG_NA;
    regNumber sourceReg2 = REG_NA;

    if (!op1->isContained())
    {
        sourceReg1 = op1->GetRegNum();
    }
    else if (GenTreeIndir* indir = op1->IsIndir())
    {
        GenTree* addr = indir->GetAddr();

        if (!addr->isContained())
        {
            sourceReg1 = addr->GetRegNum();
        }
        else if (GenTreeAddrMode* addrMode = addr->IsAddrMode())
        {
            if (GenTree* base = addrMode->GetBase())
            {
                sourceReg1 = base->GetRegNum();
            }

            if (GenTree* index = addrMode->GetIndex())
            {
                sourceReg2 = index->GetRegNum();
            }
        }
    }

    regNumber targetReg = node->GetRegNum();
    if ((targetReg != sourceReg1) && (targetReg != sourceReg2))
    {
        GetEmitter()->emitIns_R_R(INS_xor, EA_4BYTE, targetReg, targetReg);
    }
    genHWIntrinsic_R_RM(node, ins, emitTypeSize(node->TypeGet()), targetReg, op1);
}

#endif // FEATURE_HW_INTRINSICS
