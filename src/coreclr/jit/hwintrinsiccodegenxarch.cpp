// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef FEATURE_HW_INTRINSICS

#include "emit.h"
#include "codegen.h"
#include "sideeffects.h"
#include "lower.h"

#if DEBUG
static bool IsHWIntrinsicMemOp(Compiler* compiler, GenTreeHWIntrinsic* instr, GenTree* op)
{
    // The Lowering::IsHWIntrinsicMemOp call is not quite right, since it follows
    // pre-register allocation logic. However, this check is still important due to the
    // various containment rules that SIMD intrinsics follow.
    //
    // We use isContainable to track the special HWIntrinsic instr containment rules (for
    // things like LoadAligned and LoadUnaligned) and we use the supportsRegOptional check
    // to support general-purpose loads (both from stack spillage and for isUsedFromMemory
    // contained nodes, in the case where the register allocator decided to not allocate a
    // register in the first place).

    bool supportsRegOptional = false;
    bool isContainable       = Lowering::IsHWIntrinsicMemOp(compiler, instr, op, &supportsRegOptional);
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
    var_types            eltType   = node->GetVecEltType();
    Emitter&             emit      = *GetEmitter();

    HWIntrinsicCategory category = HWIntrinsicInfo::GetCategory(intrinsic);
    instruction         ins      = HWIntrinsicInfo::GetIns(intrinsic, eltType);
    assert(ins != INS_invalid);
    emitAttr vecSize = emitVecTypeSize(node->GetVecSize());
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
                UseAddrRegs(op1);
                emit.Ins_R_A(ins, vecSize, dstReg, op1);
            }
            else if (HWIntrinsicInfo::DupUnaryOp(intrinsic))
            {
                RegNum op1Reg = UseReg(op1);

                if (implicitImm != -1)
                {
                    assert((implicitImm >= 0) && (implicitImm <= 127));
                    emit.VexIns_R_R_R_I(ins, vecSize, dstReg, op1Reg, op1Reg, static_cast<int8_t>(implicitImm));
                }
                else
                {
                    emit.VexIns_R_R_R(ins, vecSize, dstReg, op1Reg, op1Reg);
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

            if (HWIntrinsicInfo::IsStore(intrinsic))
            {
                UseAddrRegs(op1);

                if (((intrinsic == NI_SSE_Store) || (intrinsic == NI_SSE2_Store)) && op2->isContained())
                {
                    GenTreeHWIntrinsic* extract = op2->AsHWIntrinsic();

                    assert((extract->GetIntrinsic() == NI_AVX_ExtractVector128) ||
                           (extract->GetIntrinsic() == NI_AVX2_ExtractVector128));

                    RegNum valueReg = UseReg(extract->GetOp(0));

                    ins     = HWIntrinsicInfo::GetIns(extract->GetIntrinsic(), extract->GetVecEltType());
                    int imm = extract->GetOp(1)->AsIntCon()->GetInt32Value();

                    emit.Ins_A_R_I(ins, EA_32BYTE, op1, valueReg, imm);
                }
                else
                {
                    RegNum valueReg = UseReg(op2);

                    emit.Ins_A_R(ins, vecSize, op1, valueReg);
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
            else if (HWIntrinsicInfo::IsLoad(intrinsic))
            {
                unreached();
            }
            else if (HWIntrinsicInfo::HasIMM(intrinsic) && varActualTypeIsInt(op2->GetType()))
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

            if (HWIntrinsicInfo::HasIMM(intrinsic) && varActualTypeIsInt(op3->GetType()))
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
            else
            {
                switch (intrinsic)
                {
                    case NI_SSE41_BlendVariable:
                        inst_BlendV_R_R_RM_R(node, ins);
                        break;
                    case NI_AVX_BlendVariable:
                    case NI_AVX2_BlendVariable:
                        inst_VexBlendV_R_R_RM_R(node, ins);
                        break;
                    case NI_AVXVNNI_MultiplyWideningAndAdd:
                    case NI_AVXVNNI_MultiplyWideningAndAddSaturate:
                        assert(dstReg != REG_NA);
                        assert(op1Reg != REG_NA);
                        assert(op2Reg != REG_NA);
                        inst_VexRMW_R_R_RM(ins, vecSize, dstReg, op1Reg, op2Reg, op3);
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

    if (op->OperIs(GT_IND_LOAD))
    {
        UseAddrRegs(op->AsIndLoad()->GetAddr());
        return;
    }

    if (GenTreeAddrMode* am = op->IsAddrMode())
    {
        UseAddrRegs(am);
        return;
    }

    if (op->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD))
    {
        assert(IsValidContainedLcl(op->AsLclRef()));
        liveness.UpdateLife(this, op->AsLclRef());

        return;
    }

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

    if (op->OperIs(GT_IND_LOAD))
    {
        GenTree* loadAddr = op->AsIndLoad()->GetAddr();

        if (GenTreeLclAddr* lclAddr = loadAddr->IsLclAddr())
        {
            assert(lclAddr->isContained());

            *s    = GetStackAddrMode(lclAddr);
            *addr = nullptr;
            *data = nullptr;
        }
        else if (GenTreeConstAddr* constAddr = loadAddr->IsConstAddr())
        {
            *addr = nullptr;
            *data = constAddr->GetData();
        }
        else
        {
            *addr = loadAddr;
            *data = nullptr;
        }

        return true;
    }

    return false;
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
            emit.Ins_R_R(ins, attr, reg, rmOp->GetRegNum());
        }

        return;
    }

    assert(HWIntrinsicInfo::SupportsContainment(node->GetIntrinsic()));
    assert(IsHWIntrinsicMemOp(compiler, node, rmOp));

    StackAddrMode s;
    GenTree*      addr;
    ConstData*    data;

    if (!IsMemoryOperand(rmOp, &s, &addr, &data))
    {
        unreached();
    }
    else if (addr != nullptr)
    {
        emit.Ins_R_A(ins, attr, reg, addr);
    }
    else if (data != nullptr)
    {
        emit.Ins_R_C(ins, attr, reg, data);
    }
    else
    {
        emit.Ins_R_S(ins, attr, reg, s);
    }
}

void CodeGen::genHWIntrinsic_R_RM_I(GenTreeHWIntrinsic* node, instruction ins, int8_t imm)
{
    RegNum   dstReg = node->GetRegNum();
    GenTree* op1    = node->GetOp(0);
    emitAttr size   = emitVecTypeSize(node->GetVecSize());

    // TODO-XArch-CQ: Commutative operations can have op1 be contained
    // TODO-XArch-CQ: Non-VEX encoded instructions can have both ops contained

    assert(dstReg != REG_NA);
    assert(!node->IsCommutative()); // One operand intrinsics cannot be commutative

    if (op1->isContained() || op1->isUsedFromSpillTemp())
    {
        assert(HWIntrinsicInfo::SupportsContainment(node->GetIntrinsic()));
        assert(IsHWIntrinsicMemOp(compiler, node, op1));
    }

    inst_R_RM_I(ins, size, dstReg, op1, imm);
}

void CodeGen::inst_R_RM_I(instruction ins, emitAttr attr, RegNum reg1, GenTree* rmOp, int imm)
{
    assert(attr != EA_1BYTE);

    Emitter& emit = *GetEmitter();

    if (rmOp->isUsedFromReg())
    {
        emit.VexIns_R_R_I(ins, attr, reg1, rmOp->GetRegNum(), imm);

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
        emit.Ins_R_A_I(ins, attr, reg1, addr, imm);
    }
    else if (data != nullptr)
    {
        emit.Ins_R_C_I(ins, attr, reg1, data, imm);
    }
    else
    {
        emit.Ins_R_S_I(ins, attr, reg1, s, imm);
    }
}

void CodeGen::genHWIntrinsic_R_R_RM(
    GenTreeHWIntrinsic* node, instruction ins, emitAttr attr, RegNum dstReg, RegNum op1Reg, GenTree* op2)
{
    if (op2->isContained() || op2->isUsedFromSpillTemp())
    {
        assert(HWIntrinsicInfo::SupportsContainment(node->GetIntrinsic()));
        assert(IsHWIntrinsicMemOp(compiler, node, op2));
    }

    inst_R_R_RM(ins, attr, dstReg, op1Reg, op2, node->IsRMW(compiler));
}

void CodeGen::inst_R_R_RM(instruction ins, emitAttr size, RegNum dstReg, RegNum op1Reg, GenTree* op2, bool isRMW)
{
    assert(size != EA_1BYTE);
    assert(dstReg != REG_NA);
    assert(op1Reg != REG_NA);

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

        emit.VexIns_R_R_R(ins, size, dstReg, op1Reg, op2Reg);

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
        emit.VexIns_R_R_A(ins, size, dstReg, op1Reg, addr);
    }
    else if (data != nullptr)
    {
        emit.VexIns_R_R_C(ins, size, dstReg, op1Reg, data);
    }
    else
    {
        emit.VexIns_R_R_S(ins, size, dstReg, op1Reg, s);
    }
}

void CodeGen::inst_Vex_R_R_RM(instruction ins, emitAttr size, RegNum dstReg, RegNum op1Reg, GenTree* op2)
{
    assert(UseVexEncoding());
    assert((size == EA_4BYTE) || (size == EA_8BYTE) || (size == EA_16BYTE) || (size == EA_32BYTE));
    assert(dstReg != REG_NA);
    assert(op1Reg != REG_NA);

    Emitter& emit = *GetEmitter();

    if (op2->isUsedFromReg())
    {
        emit.Ins_R_R_R(ins, size, dstReg, op1Reg, op2->GetRegNum());

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
        emit.Ins_R_R_A(ins, size, dstReg, op1Reg, addr);
    }
    else if (data != nullptr)
    {
        emit.Ins_R_R_C(ins, size, dstReg, op1Reg, data);
    }
    else
    {
        emit.Ins_R_R_S(ins, size, dstReg, op1Reg, s);
    }
}

void CodeGen::genHWIntrinsic_R_R_RM_I(GenTreeHWIntrinsic* node, instruction ins, int8_t imm)
{
    RegNum   dstReg = node->GetRegNum();
    GenTree* op1    = node->GetOp(0);
    GenTree* op2    = node->GetOp(1);
    emitAttr size   = emitVecTypeSize(node->GetVecSize());
    Emitter& emit   = *GetEmitter();

    assert(dstReg != REG_NA);

    // TODO-XArch-CQ: Commutative operations can have op1 be contained
    // TODO-XArch-CQ: Non-VEX encoded instructions can have both ops contained

    if (op1->isContained())
    {
        assert(ins == INS_insertps);
        assert(op1->IsVecZero());
        assert(op2->isUsedFromReg());

        RegNum op2Reg = op2->GetRegNum();
        imm |= 0b1111 & ~(1 << ((imm >> 4) & 0b11));
        emit.VexIns_R_R_R_I(ins, size, dstReg, op2Reg, op2Reg, imm);

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

        emit.VexIns_R_R_R_I(ins, size, dstReg, op1Reg, op2Reg, imm);

        return;
    }

    if (op2->IsDblConPositiveZero())
    {
        assert(ins == INS_insertps);

        imm |= 1 << ((imm >> 4) & 0b11);
        emit.VexIns_R_R_R_I(ins, size, dstReg, op1Reg, op1Reg, imm);

        return;
    }

    assert(HWIntrinsicInfo::SupportsContainment(node->GetIntrinsic()));
    assert((ins == INS_insertps) || IsHWIntrinsicMemOp(compiler, node, op2));

    StackAddrMode s;
    GenTree*      addr;
    ConstData*    data;

    if (!IsMemoryOperand(op2, &s, &addr, &data))
    {
        unreached();
    }
    else if (addr != nullptr)
    {
        emit.VexIns_R_R_A_I(ins, size, dstReg, op1Reg, addr, imm);
    }
    else if (data != nullptr)
    {
        emit.VexIns_R_R_C_I(ins, size, dstReg, op1Reg, data, imm);
    }
    else
    {
        emit.VexIns_R_R_S_I(ins, size, dstReg, op1Reg, s, imm);
    }
}

void CodeGen::inst_BlendV_R_R_RM_R(GenTreeHWIntrinsic* node, instruction ins)
{
    assert(!UseVexEncoding());
    assert(IsSse41Blendv(ins));
    assert(node->GetType() == TYP_SIMD16);

    RegNum   dstReg = node->GetRegNum();
    GenTree* op1    = node->GetOp(0);
    GenTree* op2    = node->GetOp(1);
    GenTree* op3    = node->GetOp(2);
    Emitter& emit   = *GetEmitter();

    RegNum op1Reg = op1->GetRegNum();
    RegNum op3Reg = op3->GetRegNum();

    assert(dstReg != REG_NA);
    assert(op1Reg != REG_NA);
    assert(op3Reg != REG_NA);

    // TODO-MIKE-Review: Check if these moves are actually correct...
    emit.emitIns_Mov(INS_movaps, EA_16BYTE, REG_XMM0, op3Reg, /* canSkip */ true);
    emit.emitIns_Mov(INS_movaps, EA_16BYTE, dstReg, op1Reg, /* canSkip */ true);

    if (op2->isUsedFromReg())
    {
        RegNum op2Reg = op2->GetRegNum();
        emit.Ins_R_R(ins, EA_16BYTE, dstReg, op2Reg);

        return;
    }

    assert(HWIntrinsicInfo::SupportsContainment(node->GetIntrinsic()));
    assert(IsHWIntrinsicMemOp(compiler, node, op2));

    StackAddrMode s;
    GenTree*      addr;
    ConstData*    data;

    if (!IsMemoryOperand(op2, &s, &addr, &data))
    {
        unreached();
    }
    else if (addr != nullptr)
    {
        emit.Ins_R_A(ins, EA_16BYTE, dstReg, addr);
    }
    else if (data != nullptr)
    {
        emit.Ins_R_C(ins, EA_16BYTE, dstReg, data);
    }
    else
    {
        emit.Ins_R_S(ins, EA_16BYTE, dstReg, s);
    }
}

void CodeGen::inst_VexBlendV_R_R_RM_R(GenTreeHWIntrinsic* node, instruction ins)
{
    assert(UseVexEncoding());
    assert(IsAvxBlendv(ins));

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
        emit.Ins_R_R_R_R(ins, size, dstReg, op1Reg, op2->GetRegNum(), op3Reg);

        return;
    }

    assert(HWIntrinsicInfo::SupportsContainment(node->GetIntrinsic()));
    assert(IsHWIntrinsicMemOp(compiler, node, op2));

    StackAddrMode s;
    GenTree*      addr;
    ConstData*    data;

    if (!IsMemoryOperand(op2, &s, &addr, &data))
    {
        unreached();
    }
    else if (addr != nullptr)
    {
        emit.Ins_R_R_A_R(ins, size, dstReg, op1Reg, op3Reg, addr);
    }
    else if (data != nullptr)
    {
        emit.Ins_R_R_C_R(ins, size, dstReg, op1Reg, op3Reg, data);
    }
    else
    {
        emit.Ins_R_R_S_R(ins, size, dstReg, op1Reg, op3Reg, s);
    }
}

void CodeGen::inst_VexRMW_R_R_RM(
    instruction ins, emitAttr size, RegNum dstReg, RegNum op1Reg, RegNum op2Reg, GenTree* op3)
{
    assert(IsFMAInstruction(ins) || IsAVXVNNIInstruction(ins));
    assert(dstReg != REG_NA);
    assert(op1Reg != REG_NA);
    assert(op2Reg != REG_NA);
    assert((op2Reg != dstReg) || (op1Reg == dstReg));

    Emitter& emit = *GetEmitter();
    assert(emit.UseVexEncoding());

    emit.emitIns_Mov(INS_movaps, size, dstReg, op1Reg, /* canSkip */ true);

    if (op3->isUsedFromReg())
    {
        RegNum op3Reg = op3->GetRegNum();
        assert((op3Reg != dstReg) || (op1Reg == dstReg));

        emit.Ins_R_R_R(ins, size, dstReg, op2Reg, op3Reg);

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
        emit.Ins_R_R_A(ins, size, dstReg, op2Reg, addr);
    }
    else if (data != nullptr)
    {
        emit.Ins_R_R_C(ins, size, dstReg, op2Reg, data);
    }
    else
    {
        emit.Ins_R_R_S(ins, size, dstReg, op2Reg, s);
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

    const unsigned maxByte = HWIntrinsicInfo::GetImmOpUpperBound(intrinsic) + 1;
    assert(maxByte <= 256);

    insGroup** labels;
    ConstData* data = emit.CreateTempLabelTable(&labels, maxByte, true);

    emit.Ins_R_C(INS_lea, EA_PTRSIZE, offsReg, data);
    emit.Ins_R_ARX(INS_mov, EA_4BYTE, offsReg, offsReg, nonConstImmReg, 4, 0);
    emit.Ins_R_L(baseReg, compiler->fgFirstBB->emitLabel);
    emit.Ins_R_R(INS_add, EA_PTRSIZE, offsReg, baseReg);
    emit.Ins_R(INS_i_jmp, EA_PTRSIZE, offsReg);

    insGroup* switchTableEnd = emit.CreateTempLabel();

    for (unsigned i = 0; i < maxByte; i++)
    {
        emit.DefineTempLabel(labels[i]);
        emitSwCase(static_cast<int8_t>(i));
        emit.Ins_J(INS_jmp, switchTableEnd);
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
    var_types eltType = node->GetVecEltType();
    Emitter&  emit    = *GetEmitter();

    assert(varTypeIsTargetVec(type) || (node->GetIntrinsic() == NI_VEC_EXTRACT));
    assert(varTypeIsArithmetic(eltType));

    auto GenMove = [&](emitAttr size, bool canSkip) {
        GenTree*    op1 = node->GetOp(0);
        instruction ins = HWIntrinsicInfo::GetIns(node->GetIntrinsic(), eltType);

        UseHWIntrinsicOp(op1);

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

    switch (node->GetIntrinsic())
    {
        case NI_VEC_ONE_BITS:
            if ((type != TYP_SIMD16) && !compiler->compOpportunisticallyDependsOn(InstructionSet_AVX2))
            {
                assert(compiler->opts.IsIsaSupported(InstructionSet_AVX));
                emit.VexIns_R_R_R(INS_xorps, EA_16BYTE, dstReg, dstReg, dstReg);
                emit.VexIns_R_R_R_I(INS_cmpps, EA_32BYTE, dstReg, dstReg, dstReg, 15);
            }
            else
            {
                emit.VexIns_R_R_R(INS_pcmpeqd, emitTypeSize(type), dstReg, dstReg, dstReg);
            }
            break;

        case NI_VEC_ZERO:
            emit.VexIns_R_R_R(INS_xorps, EA_16BYTE, dstReg, dstReg, dstReg);
            break;

        case NI_VEC_EXTRACT:
            GenVecExtract(node);
            break;

        case NI_VEC_REGCAST:
        {
            GenTree* op1 = node->GetOp(0);

            assert(varTypeUsesVecReg(op1->GetType()));
            assert(op1->GetType() != type);

            if (op1->isContained() || op1->isUsedFromSpillTemp())
            {
                var_types loadType = op1->GetType();
                genHWIntrinsic_R_RM(node, ins_Load(loadType), emitTypeSize(loadType), dstReg, op1);
            }
            else
            {
                RegNum op1Reg = UseReg(op1);
                emit.emitIns_Mov(INS_movaps, EA_16BYTE, dstReg, op1Reg, /*canSkip*/ true);
            }
            break;
        }

        case NI_VEC_ITOV:
        {
            GenTree* op1 = node->GetOp(0);

            assert(varTypeIsIntegral(eltType));
            assert(varActualType(eltType) == varActualType(op1->GetType()));

            UseHWIntrinsicOp(op1);
            genHWIntrinsic_R_RM(node, INS_movd, emitActualTypeSize(eltType), dstReg, op1);
            break;
        }

        case NI_VEC_ZEXT:
            GenMove(EA_16BYTE, /* canSkip */ false);
            break;
        case NI_VEC_TRUNC:
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

    var_types eltType = node->GetVecEltType();
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

        emit.Ins_R_ARX(ins_Load(eltType), emitTypeSize(eltType), destReg, baseReg, indexReg, scale, offset);

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
                emit.Ins_R_R(INS_movshdup, EA_16BYTE, destReg, srcReg);
            }
            else
            {
                emit.VexIns_R_R_R_I(INS_shufps, EA_16BYTE, destReg, srcReg, srcReg, 0x55);
            }
        }
        else if (indexValue == 2)
        {
            emit.VexIns_R_R_R(INS_unpckhps, EA_16BYTE, destReg, srcReg, srcReg);
        }
        else
        {
            assert(indexValue == 3);
            emit.VexIns_R_R_R_I(INS_shufps, EA_16BYTE, destReg, srcReg, srcReg, -1);
        }
    }
    else
    {
        assert(eltType == TYP_DOUBLE);
        assert(indexValue == 1);

        emit.VexIns_R_R_R(INS_unpckhpd, EA_16BYTE, destReg, srcReg, srcReg);
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
    var_types      eltType   = node->GetVecEltType();
    RegNum         dstReg    = node->GetRegNum();
    GenTree*       op1       = node->GetNumOps() >= 1 ? node->GetOp(0) : nullptr;
    GenTree*       op2       = node->GetNumOps() >= 2 ? node->GetOp(1) : nullptr;
    Emitter&       emit      = *GetEmitter();

    UseHWIntrinsicOperands(node);

    switch (intrinsic)
    {
        case NI_SSE_LoadLow:
        case NI_SSE_LoadHigh:
        case NI_SSE2_LoadLow:
        case NI_SSE2_LoadHigh:
        {
            assert(node->TypeIs(TYP_SIMD16));

            instruction ins      = HWIntrinsicInfo::GetIns(intrinsic, eltType);
            RegNum      otherReg = op1->GetRegNum();

            if (GenTreeLclAddr* lclAddr = op2->IsLclAddr())
            {
                emit.VexIns_R_R_S(ins, EA_16BYTE, dstReg, otherReg, GetStackAddrMode(lclAddr));
            }
            else if (GenTreeConstAddr* constAddr = op2->IsConstAddr())
            {
                emit.VexIns_R_R_C(ins, EA_16BYTE, dstReg, otherReg, constAddr->GetData());
            }
            else
            {
                emit.VexIns_R_R_A(ins, EA_16BYTE, dstReg, otherReg, op2);
            }
            break;
        }

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

        case NI_SSE2_MaskMove:
            assert(HWIntrinsicInfo::GetIns(intrinsic, eltType) == INS_maskmovdqu);
            emit.emitIns_Mov(INS_mov, EA_PTRSIZE, REG_RDI, node->GetOp(2)->GetRegNum(), /* canSkip */ true);
            emit.Ins_R_R(INS_maskmovdqu, emitVecTypeSize(node->GetVecSize()), op1->GetRegNum(), op2->GetRegNum());
            return;

        case NI_SSE_StoreFence:
            assert(op1 == nullptr);
            assert(op2 == nullptr);
            emit.emitIns(INS_sfence);
            return;

        case NI_SSE2_LoadFence:
            assert(op1 == nullptr);
            assert(op2 == nullptr);
            emit.emitIns(INS_lfence);
            return;

        case NI_SSE2_MemoryFence:
            assert(op1 == nullptr);
            assert(op2 == nullptr);
            emit.emitIns(INS_mfence);
            return;

        case NI_SSE_Prefetch0:
        case NI_SSE_Prefetch1:
        case NI_SSE_Prefetch2:
        case NI_SSE_PrefetchNonTemporal:
            emit.Ins_A(HWIntrinsicInfo::GetIns(intrinsic, eltType), EA_1BYTE, op1);
            return;

        case NI_SSE2_StoreNonTemporal:
        case NI_SSE2_X64_StoreNonTemporal:
            assert(HWIntrinsicInfo::GetIns(intrinsic, eltType) == INS_movnti);
            assert(op1 != nullptr);
            assert(op2 != nullptr);
            emit.Ins_A_R(INS_movnti, emitTypeSize(eltType), op1, op2->GetRegNum());
            return;

        default:
            unreached();
    }

    DefReg(node);
}

void CodeGen::GenSSE41Intrinsic(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsic = node->GetIntrinsic();
    RegNum         dstReg    = node->GetRegNum();
    var_types      eltType   = node->GetVecEltType();

    UseHWIntrinsicOperands(node);

    switch (intrinsic)
    {
        case NI_SSE41_ConvertToVector128Int16:
        case NI_SSE41_ConvertToVector128Int32:
        case NI_SSE41_ConvertToVector128Int64:
        {
            instruction ins = HWIntrinsicInfo::GetIns(intrinsic, eltType);
            GenTree*    op1 = node->GetOp(0);

            if (!varTypeIsVec(op1->GetType()))
            {
                GetEmitter()->Ins_R_A(ins, EA_16BYTE, node->GetRegNum(), op1);
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

            assert(op1->isUsedFromReg());
            RegNum srcReg = op1->GetRegNum();

            auto emitSwCase = [&](int8_t i) { GetEmitter()->Ins_R_R_I(ins, attr, dstReg, srcReg, i); };

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
    var_types      eltType   = node->GetVecEltType();
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

            if (!varTypeIsVec(op1->GetType()))
            {
                emit.Ins_R_A(ins, EA_32BYTE, node->GetRegNum(), op1);
            }
            else
            {
                genHWIntrinsic_R_RM(node, ins, EA_32BYTE, dstReg, op1);
            }
            break;
        }

        case NI_AVX_MaskLoad:
        case NI_AVX2_MaskLoad:
        {
            assert(varTypeIsTargetVec(node->GetType()));

            GenTree* addr = node->GetOp(0);
            GenTree* mask = node->GetOp(1);

            instruction ins     = HWIntrinsicInfo::GetIns(intrinsic, eltType);
            emitAttr    size    = emitTypeSize(node->GetType());
            RegNum      maskReg = mask->GetRegNum();

            if (GenTreeLclAddr* lclAddr = addr->IsLclAddr())
            {
                emit.Ins_R_R_S(ins, size, dstReg, maskReg, GetStackAddrMode(lclAddr));
            }
            else if (GenTreeConstAddr* constAddr = addr->IsConstAddr())
            {
                emit.Ins_R_R_C(ins, size, dstReg, maskReg, constAddr->GetData());
            }
            else
            {
                emit.Ins_R_R_A(ins, size, dstReg, maskReg, addr);
            }
            break;
        }

        case NI_AVX_MaskStore:
        case NI_AVX2_MaskStore:
        {
            instruction ins      = HWIntrinsicInfo::GetIns(intrinsic, eltType);
            emitAttr    size     = emitVecTypeSize(node->GetVecSize());
            GenTree*    addr     = node->GetOp(0);
            RegNum      baseReg  = REG_NA;
            RegNum      indexReg = REG_NA;
            unsigned    scale    = 0;
            int         disp     = 0;
            RegNum      maskReg  = node->GetOp(1)->GetRegNum();
            RegNum      valueReg = node->GetOp(2)->GetRegNum();

            if (addr->isUsedFromReg())
            {
                baseReg = addr->GetRegNum();
            }
            else
            {
                GenTreeAddrMode* am = addr->AsAddrMode();

                baseReg = am->GetBase()->GetRegNum();

                if (am->HasIndex())
                {
                    indexReg = am->GetIndex()->GetRegNum();
                }

                scale = am->GetScale();
                disp  = am->GetOffset();
            }

            emit.Ins_ARX_R_R(ins, size, baseReg, indexReg, scale, disp, maskReg, valueReg);

            return;
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

                emit.VexIns_R_R_R(INS_pcmpeqd, size, maskDstReg, maskDstReg, maskDstReg);
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

            emit.Ins_R_ARX_R(ins, size, dstReg, baseReg, indexReg, static_cast<int8_t>(scale), 0, maskDstReg);

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
            inst_Vex_R_R_RM(ins, emitTypeSize(dstType), dstReg, op1->GetRegNum(), node->GetOp(1));
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
            emitAttr size = emitTypeSize(dstType);

            assert((op2Reg != REG_EDX) || (op1Reg == REG_EDX));
            GetEmitter()->emitIns_Mov(INS_mov, size, REG_EDX, op1Reg, /* canSkip */ true);

            inst_Vex_R_R_RM(ins, size, dstReg, lowReg, op2);

            if (node->GetNumOps() == 3)
            {
                GetEmitter()->Ins_AR_R(INS_mov, size, lowReg, op3Reg, 0);
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
    instruction    ins       = HWIntrinsicInfo::GetIns(intrinsic, node->GetVecEltType());
    GenTree*       op1       = node->GetOp(0);
    GenTree*       op2       = node->GetOp(1);
    GenTree*       op3       = node->GetOp(2);
    RegNum         dstReg    = node->GetRegNum();

    UseHWIntrinsicOperands(node);

    RegNum op1Reg;
    RegNum op2Reg;

    const bool isScalar = HWIntrinsicInfo::IsXmmScalar(intrinsic);

    if (!op2->isUsedFromReg())
    {
        // 132 form: op1 = (op1 * op3) + [op2]

        ins    = static_cast<instruction>(ins - 1);
        op1Reg = op1->GetRegNum();
        op2Reg = op3->GetRegNum();
        op3    = op2;
    }
    else if (!op1->isUsedFromReg())
    {
        assert(!isScalar);

        // 231 form: op3 = (op2 * op3) + [op1]

        ins    = static_cast<instruction>(ins + 1);
        op1Reg = op3->GetRegNum();
        op2Reg = op2->GetRegNum();
        op3    = op1;
    }
    else
    {
        // 213 form: op1 = (op2 * op1) + [op3]

        op1Reg = op1->GetRegNum();
        op2Reg = op2->GetRegNum();

        if (!isScalar && (op1Reg != dstReg) && (op2Reg == dstReg))
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
    }

    inst_VexRMW_R_R_RM(ins, size, dstReg, op1Reg, op2Reg, op3);
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
        GetEmitter()->Ins_R_R(INS_xor, EA_4BYTE, dstReg, dstReg);
    }

    genHWIntrinsic_R_RM(node, ins, emitTypeSize(node->GetType()), dstReg, op1);
}

#endif // FEATURE_HW_INTRINSICS
