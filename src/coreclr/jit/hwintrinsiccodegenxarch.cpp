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

void CodeGen::GenHWIntrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsic = node->GetIntrinsic();

    assert(HWIntrinsicInfo::RequiresCodegen(intrinsic));

    if (!HWIntrinsicInfo::HasSpecialCodegen(intrinsic))
    {
        GenGenericIntrinsic(node);
        return;
    }

    switch (HWIntrinsicInfo::GetIsa(intrinsic))
    {
        case InstructionSet_ILLEGAL:
            GenVecIntrinsic(node);
            break;
        case InstructionSet_Vector128:
        case InstructionSet_Vector256:
            GenVectorNIntrinsic(node);
            break;
        case InstructionSet_X86Base:
        case InstructionSet_X86Base_X64:
            GenX86BaseIntrinsic(node);
            break;
        case InstructionSet_SSE:
        case InstructionSet_SSE_X64:
        case InstructionSet_SSE2:
        case InstructionSet_SSE2_X64:
            GenSSE2Intrinsic(node);
            break;
        case InstructionSet_SSE41:
        case InstructionSet_SSE41_X64:
            GenSSE41Intrinsic(node);
            break;
        case InstructionSet_SSE42:
        case InstructionSet_SSE42_X64:
            GenSSE42Intrinsic(node);
            break;
        case InstructionSet_AVX:
        case InstructionSet_AVX2:
            GenAVXIntrinsic(node);
            break;
        case InstructionSet_AES:
            GenAESIntrinsic(node);
            break;
        case InstructionSet_BMI1:
        case InstructionSet_BMI1_X64:
        case InstructionSet_BMI2:
        case InstructionSet_BMI2_X64:
            GenBMIIntrinsic(node);
            break;
        case InstructionSet_FMA:
            GenFMAIntrinsic(node);
            break;
        case InstructionSet_LZCNT:
        case InstructionSet_LZCNT_X64:
            GenLZCNTIntrinsic(node);
            break;
        case InstructionSet_PCLMULQDQ:
            GenPCLMULQDQIntrinsic(node);
            break;
        case InstructionSet_POPCNT:
        case InstructionSet_POPCNT_X64:
            GenPOPCNTIntrinsic(node);
            break;
        default:
            unreached();
    }
}

void CodeGen::GenGenericIntrinsic(GenTreeHWIntrinsic* node)
{
    const NamedIntrinsic intrinsic = node->GetIntrinsic();
    GenTree*             op1       = node->GetOp(0);
    RegNum               dstReg    = node->GetRegNum();
    var_types            eltType   = node->GetSimdBaseType();
    Emitter&             emit      = *GetEmitter();

    HWIntrinsicCategory category = HWIntrinsicInfo::GetCategory(intrinsic);
    instruction         ins      = HWIntrinsicInfo::GetIns(intrinsic, eltType);
    assert(ins != INS_invalid);
    emitAttr vecSize = emitVecTypeSize(node->GetSimdSize());
    assert(vecSize != 0);
    int implicitImm = -1;

    if (varTypeIsFloating(eltType))
    {
        implicitImm =
            HWIntrinsicInfo::GetImplicitImm(intrinsic, compiler->compOpportunisticallyDependsOn(InstructionSet_AVX));
    }

    switch (node->GetNumOps())
    {
        case 1:
        {
            if (node->IsMemoryLoad())
            {
                genConsumeAddress(op1);
                emit.emitIns_R_A(ins, vecSize, dstReg, op1);
            }
            else if ((category == HW_Category_SIMDScalar) && HWIntrinsicInfo::CopiesUpperBits(intrinsic))
            {
                RegNum op1Reg = UseReg(op1);

                if (implicitImm != -1)
                {
                    assert((implicitImm >= 0) && (implicitImm <= 127));
                    emit.emitIns_SIMD_R_R_R_I(ins, vecSize, dstReg, op1Reg, op1Reg, static_cast<int8_t>(implicitImm));
                }
                else
                {
                    emit.emitIns_SIMD_R_R_R(ins, vecSize, dstReg, op1Reg, op1Reg);
                }
            }
            else
            {
                UseHWIntrinsicOp(op1);

                if (implicitImm != -1)
                {
                    assert((implicitImm >= 0) && (implicitImm <= 127));
                    genHWIntrinsic_R_RM_I(node, ins, static_cast<int8_t>(implicitImm));
                }
                else
                {
                    genHWIntrinsic_R_RM(node, ins, vecSize, dstReg, op1);
                }
            }
            break;
        }

        case 2:
        {
            GenTree* op2 = node->GetOp(1);

            if (category == HW_Category_MemoryStore)
            {
                genConsumeAddress(op1);

                if (((intrinsic == NI_SSE_Store) || (intrinsic == NI_SSE2_Store)) && op2->isContained())
                {
                    GenTreeHWIntrinsic* extract = op2->AsHWIntrinsic();

                    assert((extract->GetIntrinsic() == NI_AVX_ExtractVector128) ||
                           (extract->GetIntrinsic() == NI_AVX2_ExtractVector128));

                    RegNum valueReg = UseReg(extract->GetOp(0));

                    ins     = HWIntrinsicInfo::GetIns(extract->GetIntrinsic(), extract->GetSimdBaseType());
                    int imm = extract->GetOp(1)->AsIntCon()->GetInt32Value();

                    emit.emitIns_A_R_I(ins, EA_32BYTE, op1, valueReg, imm);
                }
                else
                {
                    RegNum valueReg = UseReg(op2);

                    emit.emitIns_A_R(ins, vecSize, op1, valueReg);
                }
                break;
            }

            UseHWIntrinsicOp(op1);
            UseHWIntrinsicOp(op2);

            RegNum op1Reg = op1->GetRegNum();
            RegNum op2Reg = op2->GetRegNum();

            if ((op1Reg != dstReg) && (op2Reg == dstReg) && node->IsRMW(compiler))
            {
                // We have "reg2 = reg1 op reg2" where "reg1 != reg2" on a RMW intrinsic.
                //
                // For non-commutative intrinsics, we should have ensured that op2 was marked
                // delay free in order to prevent it from getting assigned the same register
                // as target. However, for commutative intrinsics, we can just swap the operands
                // in order to have "reg2 = reg2 op reg1" which will end up producing the right code.

                noway_assert(node->IsCommutative());
                op2Reg = op1Reg;
                op1Reg = dstReg;
            }

            if (implicitImm != -1)
            {
                assert((implicitImm >= 0) && (implicitImm <= 127));
                genHWIntrinsic_R_R_RM_I(node, ins, static_cast<int8_t>(implicitImm));
            }
            else if (category == HW_Category_MemoryLoad)
            {
                // Get the address and the 'other' register.
                GenTree* addr;
                RegNum   otherReg;

                if (intrinsic == NI_AVX_MaskLoad || intrinsic == NI_AVX2_MaskLoad)
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
                    emit.emitIns_SIMD_R_R_S(ins, vecSize, dstReg, otherReg, GetStackAddrMode(lclAddr));
                }
                else
                {
                    emit.emitIns_SIMD_R_R_A(ins, vecSize, dstReg, otherReg, addr);
                }
            }
            else if (HWIntrinsicInfo::IsImmOp(intrinsic, op2))
            {
                assert(implicitImm == -1);
                auto emitSwCase = [&](int8_t i) { genHWIntrinsic_R_RM_I(node, ins, i); };

                if (op2->IsIntCon())
                {
                    ssize_t imm = op2->AsIntCon()->GetValue();
                    assert((imm >= 0) && (imm <= 255));
                    emitSwCase(static_cast<int8_t>(imm));
                }
                else
                {
                    RegNum baseReg = node->ExtractTempReg();
                    RegNum offsReg = node->GetSingleTempReg();

                    GenHWIntrinsicJumpTableFallback(intrinsic, op2Reg, baseReg, offsReg, emitSwCase);
                }
            }
            else if (node->TypeIs(TYP_VOID))
            {
                genHWIntrinsic_R_RM(node, ins, vecSize, op1Reg, op2);
            }
            else
            {
                genHWIntrinsic_R_R_RM(node, ins, vecSize, dstReg, op1->GetRegNum(), op2);
            }
            break;
        }

        case 3:
        {
            assert(implicitImm == -1);

            GenTree* op2 = node->GetOp(1);
            GenTree* op3 = node->GetOp(2);

            UseHWIntrinsicOp(op1);
            UseHWIntrinsicOp(op2);
            UseHWIntrinsicOp(op3);

            RegNum op1Reg = op1->GetRegNum();
            RegNum op2Reg = op2->GetRegNum();
            RegNum op3Reg = op3->GetRegNum();

            if (HWIntrinsicInfo::IsImmOp(intrinsic, op3))
            {
                auto emitSwCase = [&](int8_t i) { genHWIntrinsic_R_R_RM_I(node, ins, i); };

                if (op3->IsIntCon())
                {
                    ssize_t imm = op3->AsIntCon()->GetValue();
                    assert((imm >= 0) && (imm <= 255));
                    emitSwCase(static_cast<int8_t>(imm));
                }
                else
                {
                    RegNum baseReg = node->ExtractTempReg();
                    RegNum offsReg = node->GetSingleTempReg();

                    GenHWIntrinsicJumpTableFallback(intrinsic, op3Reg, baseReg, offsReg, emitSwCase);
                }
            }
            else if (category == HW_Category_MemoryStore)
            {
                // The Mask instructions do not currently support containment of the address.
                assert(!op2->isContained());

                if (intrinsic == NI_AVX_MaskStore || intrinsic == NI_AVX2_MaskStore)
                {
                    emit.emitIns_AR_R_R(ins, vecSize, op2Reg, op3Reg, op1Reg, 0);
                }
                else
                {
                    assert(intrinsic == NI_SSE2_MaskMove);
                    assert(dstReg == REG_NA);

                    emit.emitIns_Mov(INS_mov, EA_PTRSIZE, REG_RDI, op3Reg, /* canSkip */ true);
                    emit.emitIns_R_R(ins, vecSize, op1Reg, op2Reg);
                }
            }
            else
            {
                switch (intrinsic)
                {
                    case NI_SSE41_BlendVariable:
                    case NI_AVX_BlendVariable:
                    case NI_AVX2_BlendVariable:
                        genHWIntrinsic_R_R_RM_R(node, ins);
                        break;
                    case NI_AVXVNNI_MultiplyWideningAndAdd:
                    case NI_AVXVNNI_MultiplyWideningAndAddSaturate:
                        assert(dstReg != REG_NA);
                        assert(op1Reg != REG_NA);
                        assert(op2Reg != REG_NA);
                        genHWIntrinsic_R_R_R_RM(ins, vecSize, dstReg, op1Reg, op2Reg, op3);
                        break;
                    default:
                        unreached();
                }
            }
            break;
        }

        default:
            unreached();
    }

    DefReg(node);
}

void CodeGen::UseHWIntrinsicOp(GenTree* op)
{
#ifndef TARGET_64BIT
    assert(!op->OperIs(GT_LONG));
#endif

    if (op->isUsedFromSpillTemp())
    {
        return;
    }

    if (!op->isContained())
    {
        UseReg(op);
        return;
    }

    if (op->IsIndir())
    {
        genConsumeAddress(op->AsIndir()->GetAddr());
        return;
    }

    if (op->IsAddrMode())
    {
        genConsumeAddress(op);
        return;
    }

    if (op->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD))
    {
        assert(IsValidContainedLcl(op->AsLclRef()));
        liveness.UpdateLife(this, op->AsLclRef());

        return;
    }

#ifdef FEATURE_HW_INTRINSICS
    if (GenTreeHWIntrinsic* hwi = op->IsHWIntrinsic())
    {
        if (hwi->GetNumOps() != 0)
        {
            HWIntrinsicCategory category = HWIntrinsicInfo::GetCategory(hwi->GetIntrinsic());
            assert((category == HW_Category_MemoryLoad) || (category == HW_Category_MemoryStore));
            genConsumeAddress(hwi->GetOp(0));

            if (category == HW_Category_MemoryStore)
            {
                assert(hwi->IsBinary());
                UseReg(hwi->GetOp(1));
            }
            else
            {
                assert(hwi->IsUnary());
            }
        }

        return;
    }
#endif

    assert(op->OperIsLeaf());
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
        assert(intrin->IsMemoryLoad());
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

void CodeGen::genHWIntrinsic_R_RM(GenTreeHWIntrinsic* node, instruction ins, emitAttr attr, RegNum reg, GenTree* rmOp)
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

void CodeGen::genHWIntrinsic_R_RM_I(GenTreeHWIntrinsic* node, instruction ins, int8_t imm)
{
    RegNum   dstReg = node->GetRegNum();
    GenTree* op1    = node->GetOp(0);
    emitAttr size   = emitVecTypeSize(node->GetSimdSize());

    // TODO-XArch-CQ: Commutative operations can have op1 be contained
    // TODO-XArch-CQ: Non-VEX encoded instructions can have both ops contained

    assert(dstReg != REG_NA);
    assert(!node->IsCommutative()); // One operand intrinsics cannot be commutative

    if (op1->isContained() || op1->isUsedFromSpillTemp())
    {
        assert(HWIntrinsicInfo::SupportsContainment(node->GetIntrinsic()));
        assert(IsContainableHWIntrinsicOp(compiler, node, op1));
    }

    inst_RV_TT_IV(ins, size, dstReg, op1, imm);
}

void CodeGen::inst_RV_TT_IV(instruction ins, emitAttr attr, RegNum reg1, GenTree* rmOp, int imm)
{
    assert(attr != EA_1BYTE);

    Emitter& emit = *GetEmitter();

    if (rmOp->isUsedFromReg())
    {
        emit.emitIns_SIMD_R_R_I(ins, attr, reg1, rmOp->GetRegNum(), imm);

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
        emit.emitIns_R_A_I(ins, attr, reg1, addr, imm);
    }
    else if (data != nullptr)
    {
        emit.emitIns_R_C_I(ins, attr, reg1, data, imm);
    }
    else
    {
        emit.emitIns_R_S_I(ins, attr, reg1, s, imm);
    }
}

void CodeGen::genHWIntrinsic_R_R_RM(
    GenTreeHWIntrinsic* node, instruction ins, emitAttr attr, RegNum dstReg, RegNum op1Reg, GenTree* op2)
{
    assert(dstReg != REG_NA);
    assert(op1Reg != REG_NA);

    if (op2->isContained() || op2->isUsedFromSpillTemp())
    {
        assert(HWIntrinsicInfo::SupportsContainment(node->GetIntrinsic()));
        assert(IsContainableHWIntrinsicOp(compiler, node, op2));
    }

    inst_RV_RV_TT(ins, attr, dstReg, op1Reg, op2, node->IsRMW(compiler));
}

void CodeGen::inst_RV_RV_TT(instruction ins, emitAttr size, RegNum dstReg, RegNum op1Reg, GenTree* op2, bool isRMW)
{
    assert(size != EA_1BYTE);

    // TODO-XArch-CQ: Commutative operations can have op1 be contained
    // TODO-XArch-CQ: Non-VEX encoded instructions can have both ops contained

    Emitter& emit = *GetEmitter();

    if (op2->isUsedFromReg())
    {
        RegNum op2Reg = op2->GetRegNum();

        if ((op1Reg != dstReg) && (op2Reg == dstReg) && isRMW)
        {
            // We have "reg2 = reg1 op reg2" where "reg1 != reg2" on a RMW instruction.
            //
            // For non-commutative instructions, we should have ensured that op2 was marked
            // delay free in order to prevent it from getting assigned the same register
            // as target. However, for commutative instructions, we can just swap the operands
            // in order to have "reg2 = reg2 op reg1" which will end up producing the right code.

            op2Reg = op1Reg;
            op1Reg = dstReg;
        }

        emit.emitIns_SIMD_R_R_R(ins, size, dstReg, op1Reg, op2Reg);

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
        emit.emitIns_SIMD_R_R_A(ins, size, dstReg, op1Reg, addr);
    }
    else if (data != nullptr)
    {
        emit.emitIns_SIMD_R_R_C(ins, size, dstReg, op1Reg, data);
    }
    else
    {
        emit.emitIns_SIMD_R_R_S(ins, size, dstReg, op1Reg, s);
    }
}

void CodeGen::genHWIntrinsic_R_R_RM_I(GenTreeHWIntrinsic* node, instruction ins, int8_t imm)
{
    RegNum   dstReg = node->GetRegNum();
    GenTree* op1    = node->GetOp(0);
    GenTree* op2    = node->GetOp(1);
    emitAttr size   = emitVecTypeSize(node->GetSimdSize());
    Emitter& emit   = *GetEmitter();

    assert(dstReg != REG_NA);

    // TODO-XArch-CQ: Commutative operations can have op1 be contained
    // TODO-XArch-CQ: Non-VEX encoded instructions can have both ops contained

    if (op1->isContained())
    {
        assert(ins == INS_insertps);
        assert(op1->IsHWIntrinsicZero());
        assert(op2->isUsedFromReg());

        RegNum op2Reg = op2->GetRegNum();
        imm |= 0b1111 & ~(1 << ((imm >> 4) & 0b11));
        emit.emitIns_SIMD_R_R_R_I(ins, size, dstReg, op2Reg, op2Reg, imm);

        return;
    }

    RegNum op1Reg = op1->GetRegNum();
    assert(op1Reg != REG_NA);

    if (op2->isUsedFromReg())
    {
        RegNum op2Reg = op2->GetRegNum();

        if ((op1Reg != dstReg) && (op2Reg == dstReg) && node->IsRMW(compiler))
        {
            // We have "reg2 = reg1 op reg2" where "reg1 != reg2" on a RMW intrinsic.
            //
            // For non-commutative intrinsics, we should have ensured that op2 was marked
            // delay free in order to prevent it from getting assigned the same register
            // as target. However, for commutative intrinsics, we can just swap the operands
            // in order to have "reg2 = reg2 op reg1" which will end up producing the right code.

            noway_assert(node->IsCommutative());
            op2Reg = op1Reg;
            op1Reg = dstReg;
        }

        emit.emitIns_SIMD_R_R_R_I(ins, size, dstReg, op1Reg, op2Reg, imm);

        return;
    }

    if (op2->IsDblConPositiveZero())
    {
        assert(ins == INS_insertps);

        imm |= 1 << ((imm >> 4) & 0b11);
        emit.emitIns_SIMD_R_R_R_I(ins, size, dstReg, op1Reg, op1Reg, imm);

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
        emit.emitIns_SIMD_R_R_A_I(ins, size, dstReg, op1Reg, addr, imm);
    }
    else if (data != nullptr)
    {
        emit.emitIns_SIMD_R_R_C_I(ins, size, dstReg, op1Reg, data, imm);
    }
    else
    {
        emit.emitIns_SIMD_R_R_S_I(ins, size, dstReg, op1Reg, s, imm);
    }
}

void CodeGen::genHWIntrinsic_R_R_RM_R(GenTreeHWIntrinsic* node, instruction ins)
{
    RegNum   dstReg = node->GetRegNum();
    GenTree* op1    = node->GetOp(0);
    GenTree* op2    = node->GetOp(1);
    GenTree* op3    = node->GetOp(2);
    emitAttr size   = emitTypeSize(node->GetType());
    Emitter& emit   = *GetEmitter();

    RegNum op1Reg = op1->GetRegNum();
    RegNum op3Reg = op3->GetRegNum();

    assert(dstReg != REG_NA);
    assert(op1Reg != REG_NA);
    assert(op3Reg != REG_NA);

    if (op2->isUsedFromReg())
    {
        emit.emitIns_SIMD_R_R_R_R(ins, size, dstReg, op1Reg, op2->GetRegNum(), op3Reg);

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
        emit.emitIns_SIMD_R_R_A_R(ins, size, dstReg, op1Reg, op3Reg, addr);
    }
    else if (data != nullptr)
    {
        // We can't have a DblCon operand on blend instructions.
        unreached();
    }
    else
    {
        emit.emitIns_SIMD_R_R_S_R(ins, size, dstReg, op1Reg, op3Reg, s);
    }
}

void CodeGen::genHWIntrinsic_R_R_R_RM(
    instruction ins, emitAttr attr, RegNum dstReg, RegNum op1Reg, RegNum op2Reg, GenTree* op3)
{
    assert(dstReg != REG_NA);
    assert(op1Reg != REG_NA);
    assert(op2Reg != REG_NA);

    Emitter& emit = *GetEmitter();

    if (op3->isUsedFromReg())
    {
        emit.emitIns_SIMD_R_R_R_R(ins, attr, dstReg, op1Reg, op2Reg, op3->GetRegNum());

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
        emit.emitIns_SIMD_R_R_R_A(ins, attr, dstReg, op1Reg, op2Reg, addr);
    }
    else if (data != nullptr)
    {
        emit.emitIns_SIMD_R_R_R_C(ins, attr, dstReg, op1Reg, op2Reg, data);
    }
    else
    {
        emit.emitIns_SIMD_R_R_R_S(ins, attr, dstReg, op1Reg, op2Reg, s);
    }
}

template <typename HWIntrinsicSwitchCaseBody>
void CodeGen::GenHWIntrinsicJumpTableFallback(NamedIntrinsic            intrinsic,
                                              RegNum                    nonConstImmReg,
                                              RegNum                    baseReg,
                                              RegNum                    offsReg,
                                              HWIntrinsicSwitchCaseBody emitSwCase)
{
    assert(nonConstImmReg != REG_NA);
    assert(!HWIntrinsicInfo::IsAvx2GatherIntrinsic(intrinsic));
    Emitter& emit = *GetEmitter();

    const unsigned maxByte = static_cast<unsigned>(HWIntrinsicInfo::GetImmOpUpperBound(intrinsic) + 1);
    assert(maxByte <= 256);

    insGroup** labels;
    ConstData* data = emit.CreateTempLabelTable(&labels, maxByte, true);

    emit.emitIns_R_C(INS_lea, EA_PTRSIZE, offsReg, data);
    emit.emitIns_R_ARX(INS_mov, EA_4BYTE, offsReg, offsReg, nonConstImmReg, 4, 0);
    emit.emitIns_R_L(baseReg, compiler->fgFirstBB->emitLabel);
    emit.emitIns_R_R(INS_add, EA_PTRSIZE, offsReg, baseReg);
    emit.emitIns_R(INS_i_jmp, EA_PTRSIZE, offsReg);

    insGroup* switchTableEnd = emit.CreateTempLabel();

    for (unsigned i = 0; i < maxByte; i++)
    {
        emit.DefineTempLabel(labels[i]);
        emitSwCase(static_cast<int8_t>(i));
        emit.emitIns_J(INS_jmp, switchTableEnd);
    }

    emit.DefineTempLabel(switchTableEnd);
}

void CodeGen::UseHWIntrinsicOperands(GenTreeHWIntrinsic* node)
{
    for (GenTreeHWIntrinsic::Use& use : node->Uses())
    {
        UseHWIntrinsicOp(use.GetNode());
    }
}

void CodeGen::GenVecIntrinsic(GenTreeHWIntrinsic* node)
{
    RegNum    dstReg  = node->GetRegNum();
    var_types type    = node->GetType();
    var_types eltType = node->GetSimdBaseType();
    Emitter&  emit    = *GetEmitter();

    assert(varTypeIsTargetVec(type) || (node->GetIntrinsic() == NI_VEC_EXTRACT));
    assert(varTypeIsArithmetic(eltType));

    switch (node->GetIntrinsic())
    {
        case NI_VEC_ONE_BITS:
            if ((type != TYP_SIMD16) && !compiler->compOpportunisticallyDependsOn(InstructionSet_AVX2))
            {
                assert(compiler->opts.IsIsaSupported(InstructionSet_AVX));
                emit.emitIns_SIMD_R_R_R(INS_xorps, EA_16BYTE, dstReg, dstReg, dstReg);
                emit.emitIns_SIMD_R_R_R_I(INS_cmpps, EA_32BYTE, dstReg, dstReg, dstReg, 15);
            }
            else
            {
                emit.emitIns_SIMD_R_R_R(INS_pcmpeqd, emitTypeSize(type), dstReg, dstReg, dstReg);
            }
            break;

        case NI_VEC_ZERO:
            emit.emitIns_SIMD_R_R_R(INS_xorps, EA_16BYTE, dstReg, dstReg, dstReg);
            break;

        case NI_VEC_EXTRACT:
            GenVecExtract(node);
            break;

        default:
            unreached();
    }

    DefReg(node);
}

void CodeGen::GenVectorNIntrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsic = node->GetIntrinsic();
    RegNum         dstReg    = node->GetRegNum();
    var_types      eltType   = node->GetSimdBaseType();
    GenTree*       op1       = node->GetOp(0);
    instruction    ins       = HWIntrinsicInfo::GetIns(intrinsic, eltType);
    Emitter&       emit      = *GetEmitter();

    assert(varTypeIsArithmetic(eltType));

    UseHWIntrinsicOp(op1);

    auto GenMove = [&](emitAttr size, bool canSkip) {
        if (op1->isContained() || op1->isUsedFromSpillTemp())
        {
            genHWIntrinsic_R_RM(node, ins, size, dstReg, op1);
        }
        else
        {
            RegNum op1Reg = op1->GetRegNum();
            emit.emitIns_Mov(INS_movaps, size, dstReg, op1Reg, canSkip);
        }
    };

    switch (intrinsic)
    {
        case NI_Vector128_CreateScalarUnsafe:
        case NI_Vector256_CreateScalarUnsafe:
            if (varTypeIsIntegral(eltType))
            {
                genHWIntrinsic_R_RM(node, INS_movd, emitActualTypeSize(eltType), dstReg, op1);
            }
            else
            {
                GenMove(emitTypeSize(node->GetType()), /* canSkip */ true);
            }
            break;
        case NI_Vector128_ToVector256:
            GenMove(EA_16BYTE, /* canSkip */ false);
            break;
        case NI_Vector128_ToVector256Unsafe:
            GenMove(EA_16BYTE, /* canSkip */ true);
            break;
        case NI_Vector256_GetLower:
            GenMove(EA_32BYTE, /* canSkip */ true);
            break;
        default:
            unreached();
    }

    DefReg(node);
}

void CodeGen::GenVecExtract(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_VEC_EXTRACT);

    UseHWIntrinsicOperands(node);

    var_types eltType = node->GetSimdBaseType();
    GenTree*  vec     = node->GetOp(0);
    GenTree*  index   = node->GetOp(1);
    RegNum    destReg = node->GetRegNum();
    Emitter&  emit    = *GetEmitter();

    if (!vec->isUsedFromReg())
    {
        RegNum   baseReg;
        RegNum   indexReg;
        unsigned scale;
        int      offset;

        if (vec->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD))
        {
            LclVarDsc* lcl = vec->AsLclRef()->GetLcl();

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
            offset   = frameOffset + vec->AsLclRef()->GetLclOffs();
        }
        else if (vec->AsIndir()->GetAddr()->isUsedFromReg())
        {
            baseReg  = vec->AsIndir()->GetAddr()->GetRegNum();
            indexReg = REG_NA;
            scale    = 1;
            offset   = 0;
        }
        else
        {
            assert(index->IsIntCon());

            GenTreeAddrMode* am = vec->AsIndir()->GetAddr()->AsAddrMode();

            baseReg  = am->GetBase()->GetRegNum();
            indexReg = am->HasIndex() ? am->GetIndex()->GetRegNum() : REG_NA;
            scale    = am->GetScale();
            offset   = am->GetOffset();
        }

        if (index->IsIntCon())
        {
            offset += index->AsIntCon()->GetInt32Value() * varTypeSize(eltType);
        }
        else
        {
            assert(indexReg == REG_NA);
            assert(scale == 1);
            assert(index->isUsedFromReg());

            indexReg = index->GetRegNum();
            scale    = varTypeSize(eltType);
        }

        emit.emitIns_R_ARX(ins_Load(eltType), emitTypeSize(eltType), destReg, baseReg, indexReg, scale, offset);

        return;
    }

    RegNum  srcReg     = vec->GetRegNum();
    ssize_t indexValue = index->AsIntCon()->GetValue();

    assert(varTypeIsFloating(eltType));

    if (indexValue == 0)
    {
        emit.emitIns_Mov(INS_movaps, EA_16BYTE, destReg, srcReg, true);
        return;
    }

    if (eltType == TYP_FLOAT)
    {
        if (indexValue == 1)
        {
            if (compiler->compOpportunisticallyDependsOn(InstructionSet_SSE3))
            {
                emit.emitIns_R_R(INS_movshdup, EA_16BYTE, destReg, srcReg);
            }
            else
            {
                emit.emitIns_SIMD_R_R_R_I(INS_shufps, EA_16BYTE, destReg, srcReg, srcReg, 0x55);
            }
        }
        else if (indexValue == 2)
        {
            emit.emitIns_SIMD_R_R_R(INS_unpckhps, EA_16BYTE, destReg, srcReg, srcReg);
        }
        else
        {
            assert(indexValue == 3);
            emit.emitIns_SIMD_R_R_R_I(INS_shufps, EA_16BYTE, destReg, srcReg, srcReg, -1);
        }
    }
    else
    {
        assert(eltType == TYP_DOUBLE);
        assert(indexValue == 1);

        emit.emitIns_SIMD_R_R_R(INS_unpckhpd, EA_16BYTE, destReg, srcReg, srcReg);
    }
}

void CodeGen::GenX86BaseIntrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsic = node->GetIntrinsic();

    switch (intrinsic)
    {
        case NI_X86Base_BitScanForward:
        case NI_X86Base_BitScanReverse:
        case NI_X86Base_X64_BitScanForward:
        case NI_X86Base_X64_BitScanReverse:
        {
            GenTree*    op1    = node->GetOp(0);
            RegNum      dstReg = node->GetRegNum();
            var_types   type   = node->GetType();
            instruction ins    = HWIntrinsicInfo::GetIns(intrinsic, type);

            UseRMRegs(op1);
            genHWIntrinsic_R_RM(node, ins, emitTypeSize(type), dstReg, op1);
            break;
        }

        default:
            unreached();
    }

    DefReg(node);
}

void CodeGen::GenSSE2Intrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsic = node->GetIntrinsic();
    var_types      type      = node->GetType();
    var_types      eltType   = node->GetSimdBaseType();
    RegNum         dstReg    = node->GetRegNum();
    GenTree*       op1       = node->GetNumOps() >= 1 ? node->GetOp(0) : nullptr;
    GenTree*       op2       = node->GetNumOps() >= 2 ? node->GetOp(1) : nullptr;
    Emitter&       emit      = *GetEmitter();

    UseHWIntrinsicOperands(node);

    switch (intrinsic)
    {
        case NI_SSE_ConvertToInt32:
        case NI_SSE2_ConvertToInt32:
        case NI_SSE_X64_ConvertToInt64:
        case NI_SSE2_X64_ConvertToInt64:
        case NI_SSE_ConvertToInt32WithTruncation:
        case NI_SSE2_ConvertToInt32WithTruncation:
        case NI_SSE_X64_ConvertToInt64WithTruncation:
        case NI_SSE2_X64_ConvertToInt64WithTruncation:
            assert(op1 != nullptr);
            assert(op2 == nullptr);
            genHWIntrinsic_R_RM(node, HWIntrinsicInfo::GetIns(intrinsic, eltType), emitTypeSize(type), dstReg, op1);
            break;

        case NI_SSE_ConvertScalarToVector128Single:
        case NI_SSE_X64_ConvertScalarToVector128Single:
        case NI_SSE2_ConvertScalarToVector128Double:
        case NI_SSE2_X64_ConvertScalarToVector128Double:
            assert(op1 != nullptr);
            assert(op2 != nullptr);
            genHWIntrinsic_R_R_RM(node, HWIntrinsicInfo::GetIns(intrinsic, eltType), emitActualTypeSize(op2->GetType()),
                                  dstReg, op1->GetRegNum(), op2);
            break;

        case NI_SSE2_ConvertScalarToVector128Int32:
        case NI_SSE2_X64_ConvertScalarToVector128Int64:
            assert(HWIntrinsicInfo::GetIns(intrinsic, eltType) == INS_movd);
            assert(op1 != nullptr);
            assert(op2 == nullptr);
            genHWIntrinsic_R_RM(node, INS_movd, emitActualTypeSize(op1->GetType()), dstReg, op1);
            break;

        case NI_SSE_StoreFence:
            assert(op1 == nullptr);
            assert(op2 == nullptr);
            emit.emitIns(INS_sfence);
            break;

        case NI_SSE2_LoadFence:
            assert(op1 == nullptr);
            assert(op2 == nullptr);
            emit.emitIns(INS_lfence);
            break;

        case NI_SSE2_MemoryFence:
            assert(op1 == nullptr);
            assert(op2 == nullptr);
            emit.emitIns(INS_mfence);
            break;

        case NI_SSE_Prefetch0:
        case NI_SSE_Prefetch1:
        case NI_SSE_Prefetch2:
        case NI_SSE_PrefetchNonTemporal:
            assert((op1 != nullptr) && !op1->isContained());
            emit.emitIns_AR(HWIntrinsicInfo::GetIns(intrinsic, eltType), EA_1BYTE, op1->GetRegNum(), 0);
            break;

        case NI_SSE2_StoreNonTemporal:
        case NI_SSE2_X64_StoreNonTemporal:
            assert(HWIntrinsicInfo::GetIns(intrinsic, eltType) == INS_movnti);
            assert(op1 != nullptr);
            assert(op2 != nullptr);
            emit.emitIns_A_R(INS_movnti, emitTypeSize(eltType), op1, op2->GetRegNum());
            break;

        default:
            unreached();
    }

    DefReg(node);
}

void CodeGen::GenSSE41Intrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsic = node->GetIntrinsic();
    RegNum         dstReg    = node->GetRegNum();
    var_types      eltType   = node->GetSimdBaseType();

    UseHWIntrinsicOperands(node);

    switch (intrinsic)
    {
        case NI_SSE41_ConvertToVector128Int16:
        case NI_SSE41_ConvertToVector128Int32:
        case NI_SSE41_ConvertToVector128Int64:
        {
            instruction ins = HWIntrinsicInfo::GetIns(intrinsic, eltType);
            GenTree*    op1 = node->GetOp(0);

            if (!varTypeIsSIMD(op1->GetType()))
            {
                GetEmitter()->emitIns_R_A(ins, EA_16BYTE, node->GetRegNum(), op1);
            }
            else
            {
                genHWIntrinsic_R_RM(node, ins, EA_16BYTE, dstReg, op1);
            }
            break;
        }

        case NI_SSE41_Extract:
        case NI_SSE41_X64_Extract:
        {
            assert(varTypeIsIntegral(eltType));

            instruction ins  = HWIntrinsicInfo::GetIns(intrinsic, eltType);
            emitAttr    attr = emitActualTypeSize(node->GetType());
            GenTree*    op1  = node->GetOp(0);
            GenTree*    op2  = node->GetOp(1);

            auto emitSwCase = [&](int8_t i) { inst_RV_TT_IV(ins, attr, dstReg, op1, i); };

            if (op2->IsIntCon())
            {
                ssize_t imm = op2->AsIntCon()->GetValue();
                assert((imm >= 0) && (imm <= 255));
                emitSwCase(static_cast<int8_t>(imm));
            }
            else
            {
                RegNum baseReg = node->ExtractTempReg();
                RegNum offsReg = node->GetSingleTempReg();
                GenHWIntrinsicJumpTableFallback(intrinsic, op2->GetRegNum(), baseReg, offsReg, emitSwCase);
            }
            break;
        }

        default:
            unreached();
    }

    DefReg(node);
}

void CodeGen::GenSSE42Intrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsic = node->GetIntrinsic();
    RegNum         dstReg    = node->GetRegNum();
    var_types      type      = node->GetType();
    emitAttr       size      = emitTypeSize(type);

    UseHWIntrinsicOperands(node);

    switch (intrinsic)
    {
        case NI_SSE42_CRC32B:
        case NI_SSE42_CRC32W:
        case NI_SSE42_CRC32:
        {
            GenTree* op1  = node->GetOp(0);
            GenTree* op2  = node->GetOp(1);
            RegNum   reg1 = op1->GetRegNum();
            assert((op2->GetRegNum() != dstReg) || (reg1 == dstReg));

            GetEmitter()->emitIns_Mov(INS_mov, size, dstReg, reg1, /* canSkip */ true);

            if (intrinsic == NI_SSE42_CRC32B)
            {
                size = EA_1BYTE;
            }
            else if (intrinsic == NI_SSE42_CRC32W)
            {
                size = EA_2BYTE;
            }

            genHWIntrinsic_R_RM(node, INS_crc32, size, dstReg, op2);
        }
        break;

        default:
            unreached();
    }

    DefReg(node);
}

void CodeGen::GenAVXIntrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsic = node->GetIntrinsic();
    var_types      eltType   = node->GetSimdBaseType();
    RegNum         dstReg    = node->GetRegNum();
    Emitter&       emit      = *GetEmitter();

    UseHWIntrinsicOperands(node);

    switch (intrinsic)
    {
        case NI_AVX2_ConvertToVector256Int16:
        case NI_AVX2_ConvertToVector256Int32:
        case NI_AVX2_ConvertToVector256Int64:
        {
            assert(node->TypeIs(TYP_SIMD32));
            assert(node->GetNumOps() == 1);

            GenTree*    op1 = node->GetOp(0);
            instruction ins = HWIntrinsicInfo::GetIns(intrinsic, eltType);

            if (!varTypeIsSIMD(op1->GetType()))
            {
                emit.emitIns_R_A(ins, EA_32BYTE, node->GetRegNum(), op1);
            }
            else
            {
                genHWIntrinsic_R_RM(node, ins, EA_32BYTE, dstReg, op1);
            }
            break;
        }

        case NI_AVX2_GATHERD:
        case NI_AVX2_GATHERQ:
        {
            GenTree* baseOp     = nullptr;
            GenTree* indexOp    = nullptr;
            GenTree* scaleOp    = nullptr;
            RegNum   maskDstReg = node->ExtractTempReg(RBM_ALLFLOAT);
            emitAttr size       = emitTypeSize(node->GetType());

            if (node->GetNumOps() == 5)
            {
                GenTree* srcOp  = node->GetOp(0);
                baseOp          = node->GetOp(1);
                indexOp         = node->GetOp(2);
                GenTree* maskOp = node->GetOp(3);
                scaleOp         = node->GetOp(4);

                emit.emitIns_Mov(INS_movaps, size, maskDstReg, maskOp->GetRegNum(), /* canSkip */ false);
                emit.emitIns_Mov(INS_movaps, size, dstReg, srcOp->GetRegNum(), /* canSkip */ true);
            }
            else
            {
                assert(node->GetNumOps() == 3);

                baseOp  = node->GetOp(0);
                indexOp = node->GetOp(1);
                scaleOp = node->GetOp(2);

                emit.emitIns_SIMD_R_R_R(INS_pcmpeqd, size, maskDstReg, maskDstReg, maskDstReg);
            }

            instruction ins = HWIntrinsicInfo::GetIns(intrinsic, eltType);

            if ((intrinsic == NI_AVX2_GATHERQ) && node->TypeIs(TYP_SIMD16) && indexOp->TypeIs(TYP_SIMD32) &&
                (ins == INS_vpgatherqd || ins == INS_vgatherqps))
            {
                size = EA_32BYTE;
            }

            RegNum  baseReg  = baseOp->GetRegNum();
            RegNum  indexReg = indexOp->GetRegNum();
            ssize_t scale    = scaleOp->AsIntCon()->GetValue();

            assert(dstReg != maskDstReg);
            assert(dstReg != indexReg);
            assert(maskDstReg != indexReg);
            assert((scale >= 0) && (scale <= 8));

            emit.emitIns_R_AR_R(ins, size, dstReg, maskDstReg, baseReg, indexReg, static_cast<int8_t>(scale), 0);

            break;
        }

        default:
            unreached();
    }

    DefReg(node);
}

void CodeGen::GenAESIntrinsic(GenTreeHWIntrinsic* node)
{
    NYI("Implement AES intrinsic code generation");
}

void CodeGen::GenBMIIntrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsic = node->GetIntrinsic();
    RegNum         dstReg    = node->GetRegNum();
    var_types      dstType   = node->GetType();
    GenTree*       op1       = node->GetOp(0);
    instruction    ins       = HWIntrinsicInfo::GetIns(intrinsic, dstType);

    UseHWIntrinsicOperands(node);

    switch (intrinsic)
    {
        case NI_BMI1_AndNot:
        case NI_BMI1_X64_AndNot:
        case NI_BMI1_BitFieldExtract:
        case NI_BMI1_X64_BitFieldExtract:
        case NI_BMI2_ParallelBitDeposit:
        case NI_BMI2_X64_ParallelBitDeposit:
        case NI_BMI2_ParallelBitExtract:
        case NI_BMI2_X64_ParallelBitExtract:
        case NI_BMI2_ZeroHighBits:
        case NI_BMI2_X64_ZeroHighBits:
            assert((dstType == TYP_INT) || (dstType == TYP_LONG));
            genHWIntrinsic_R_R_RM(node, ins, emitTypeSize(dstType), dstReg, op1->GetRegNum(), node->GetOp(1));
            break;

        case NI_BMI1_ExtractLowestSetBit:
        case NI_BMI1_X64_ExtractLowestSetBit:
        case NI_BMI1_GetMaskUpToLowestSetBit:
        case NI_BMI1_X64_GetMaskUpToLowestSetBit:
        case NI_BMI1_ResetLowestSetBit:
        case NI_BMI1_X64_ResetLowestSetBit:
            assert((dstType == TYP_INT) || (dstType == TYP_LONG));
            genHWIntrinsic_R_RM(node, ins, emitTypeSize(dstType), dstReg, op1);
            break;

        case NI_BMI1_TrailingZeroCount:
        case NI_BMI1_X64_TrailingZeroCount:
            GenXCNTIntrinsic(node, ins);
            break;

        case NI_BMI2_MultiplyNoFlags:
        case NI_BMI2_X64_MultiplyNoFlags:
        {
            GenTree* op2    = node->GetOp(1);
            RegNum   op1Reg = REG_NA;
            RegNum   op2Reg = REG_NA;
            RegNum   op3Reg = REG_NA;
            RegNum   lowReg = REG_NA;

            if (node->GetNumOps() == 2)
            {
                op1Reg = op1->GetRegNum();
                op2Reg = op2->GetRegNum();
                lowReg = dstReg;
            }
            else
            {
                GenTree* op3 = node->GetOp(2);
                assert(!op3->isContained());

                op1Reg = op1->GetRegNum();
                op2Reg = op2->GetRegNum();
                op3Reg = op3->GetRegNum();
                lowReg = node->GetSingleTempReg();

                assert(op3Reg != op1Reg);
                assert(op3Reg != dstReg);
                assert(op3Reg != REG_EDX);
                assert(op3Reg != lowReg);
                assert(lowReg != dstReg);
            }

            assert(!op2->isContained());
            emitAttr attr = emitTypeSize(dstType);

            assert((op2Reg != REG_EDX) || (op1Reg == REG_EDX));
            GetEmitter()->emitIns_Mov(INS_mov, attr, REG_EDX, op1Reg, /* canSkip */ true);

            genHWIntrinsic_R_R_RM(node, ins, attr, dstReg, lowReg, op2);

            if (node->GetNumOps() == 3)
            {
                GetEmitter()->emitIns_AR_R(INS_mov, attr, lowReg, op3Reg, 0);
            }

            break;
        }

        default:
            unreached();
    }

    DefReg(node);
}

void CodeGen::GenFMAIntrinsic(GenTreeHWIntrinsic* node)
{
    assert(varTypeIsTargetVec(node->GetType()));

    NamedIntrinsic intrinsic = node->GetIntrinsic();
    emitAttr       size      = emitTypeSize(node->GetType());
    instruction    ins       = HWIntrinsicInfo::GetIns(intrinsic, node->GetSimdBaseType());
    GenTree*       op1       = node->GetOp(0);
    GenTree*       op2       = node->GetOp(1);
    GenTree*       op3       = node->GetOp(2);
    RegNum         dstReg    = node->GetRegNum();

    UseHWIntrinsicOperands(node);

    RegNum op1Reg;
    RegNum op2Reg;

    bool       isCommutative   = false;
    const bool copiesUpperBits = HWIntrinsicInfo::CopiesUpperBits(intrinsic);

    // Intrinsics with CopyUpperBits semantics cannot have op1 be contained
    assert(!copiesUpperBits || !op1->isContained());

    if (op2->isContained() || op2->isUsedFromSpillTemp())
    {
        // 132 form: op1 = (op1 * op3) + [op2]

        ins    = static_cast<instruction>(ins - 1);
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

    if (isCommutative && (op1Reg != dstReg) && (op2Reg == dstReg))
    {
        assert(node->IsRMW(compiler));

        // We have "reg2 = (reg1 * reg2) +/- op3" where "reg1 != reg2" on a RMW intrinsic.
        //
        // For non-commutative intrinsics, we should have ensured that op2 was marked
        // delay free in order to prevent it from getting assigned the same register
        // as target. However, for commutative intrinsics, we can just swap the operands
        // in order to have "reg2 = reg2 op reg1" which will end up producing the right code.

        op2Reg = op1Reg;
        op1Reg = dstReg;
    }

    genHWIntrinsic_R_R_R_RM(ins, size, dstReg, op1Reg, op2Reg, op3);
    DefReg(node);
}

void CodeGen::GenLZCNTIntrinsic(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_LZCNT_LeadingZeroCount || node->GetIntrinsic() == NI_LZCNT_X64_LeadingZeroCount);

    UseRMRegs(node->GetOp(0));
    GenXCNTIntrinsic(node, INS_lzcnt);
    DefReg(node);
}

void CodeGen::GenPCLMULQDQIntrinsic(GenTreeHWIntrinsic* node)
{
    NYI("Implement PCLMULQDQ intrinsic code generation");
}

void CodeGen::GenPOPCNTIntrinsic(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_POPCNT_PopCount || node->GetIntrinsic() == NI_POPCNT_X64_PopCount);

    UseRMRegs(node->GetOp(0));
    GenXCNTIntrinsic(node, INS_popcnt);
    DefReg(node);
}

void CodeGen::GenXCNTIntrinsic(GenTreeHWIntrinsic* node, instruction ins)
{
    assert(node->TypeIs(TYP_INT, TYP_LONG));

    // LZCNT/TZCNT/POPCNT have a false dependency on the target register on Intel Sandy Bridge, Haswell, and Skylake
    // (POPCNT only) processors, so insert a `XOR target, target` to break the dependency via XOR triggering register
    // renaming, but only if it's not an actual dependency.

    GenTree* op1        = node->GetOp(0);
    RegNum   sourceReg1 = REG_NA;
    RegNum   sourceReg2 = REG_NA;

    if (!op1->isContained())
    {
        sourceReg1 = op1->GetRegNum();
    }
    else if (GenTreeIndLoad* indir = op1->IsIndLoad())
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

    RegNum dstReg = node->GetRegNum();

    if ((dstReg != sourceReg1) && (dstReg != sourceReg2))
    {
        GetEmitter()->emitIns_R_R(INS_xor, EA_4BYTE, dstReg, dstReg);
    }

    genHWIntrinsic_R_RM(node, ins, emitTypeSize(node->GetType()), dstReg, op1);
}

#endif // FEATURE_HW_INTRINSICS
