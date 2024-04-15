// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_ARM64

#include "codegen.h"

struct ExpandNonConstImmHelper
{
    CodeGen* const codeGen;
    insGroup*      endLabel     = nullptr;
    insGroup*      nonZeroLabel = nullptr;
    int            immValue;
    int            immLowerBound;
    int            immUpperBound;
    regNumber      nonConstImmReg;
    regNumber      branchTargetReg = REG_NA;
    Emitter&       emit;

    ExpandNonConstImmHelper(CodeGen* codeGen, GenTree* immOp, GenTreeHWIntrinsic* intrin)
        : codeGen(codeGen), emit(*codeGen->GetEmitter())
    {
        assert(varTypeIsIntegral(immOp->GetType()));

        const HWIntrinsicCategory category = HWIntrinsicInfo::lookupCategory(intrin->GetIntrinsic());
        unsigned                  simdSize;

        if (category == HW_Category_SIMDByIndexedElement)
        {
            assert((intrin->GetNumOps() == 3) || (intrin->GetNumOps() == 4));
            var_types indexedElementOpType = intrin->GetOp(intrin->GetNumOps() - 2)->GetType();
            assert(varTypeIsSIMD(indexedElementOpType));
            simdSize = varTypeSize(indexedElementOpType);
        }
        else
        {
            simdSize = intrin->GetSimdSize();
        }

        HWIntrinsicInfo::lookupImmBounds(intrin->GetIntrinsic(), simdSize, intrin->GetSimdBaseType(), &immLowerBound,
                                         &immUpperBound);

        nonConstImmReg = immOp->GetRegNum();
        immValue       = immLowerBound;

        if (TestImmOpZeroOrOne())
        {
            nonZeroLabel = emit.CreateTempLabel();
        }
        else
        {
            // At the moment, this helper supports only intrinsics that correspond to one machine instruction.
            // If we ever encounter an intrinsic that is either lowered into multiple instructions or the
            // number of instructions that correspond to each case is unknown apriori - we can extend
            // support to these by using the same approach as in hwintrinsicxarch.cpp - adding an additional
            // indirection level in form of a branch table.
            assert(!HWIntrinsicInfo::GeneratesMultipleIns(intrin->GetIntrinsic()));

            branchTargetReg = intrin->GetSingleTempReg();
        }

        endLabel = emit.CreateTempLabel();

        EmitBegin();
    }

    void EmitBegin()
    {
        if (TestImmOpZeroOrOne())
        {
            emit.emitIns_J_R(INS_cbnz, EA_4BYTE, nonZeroLabel, nonConstImmReg);
        }
        else
        {
            insGroup* beginLabel = emit.CreateTempLabel();

            // Here we assume that each case consists of one arm64 instruction followed by "b endLabel".
            // Since an arm64 instruction is 4 bytes, we branch to AddressOf(beginLabel) + (nonConstImmReg << 3).
            emit.emitIns_R_L(branchTargetReg, beginLabel);
            emit.emitIns_R_R_R_I(INS_add, EA_8BYTE, branchTargetReg, branchTargetReg, nonConstImmReg, 3, INS_OPTS_LSL);

            // If the lower bound is non zero we need to adjust the branch target value by subtracting
            // (immLowerBound << 3).
            if (immLowerBound != 0)
            {
                emit.emitIns_R_R_I(INS_sub, EA_8BYTE, branchTargetReg, branchTargetReg,
                                   static_cast<ssize_t>(immLowerBound) << 3);
            }

            emit.emitIns_R(INS_br, EA_8BYTE, branchTargetReg);
            emit.DefineInlineTempLabel(beginLabel);
        }
    }

    void EmitCaseEnd()
    {
        assert(!Done());

        const bool isLastCase = (immValue == immUpperBound);

        if (isLastCase)
        {
            emit.DefineInlineTempLabel(endLabel);
        }
        else
        {
            emit.emitIns_J(INS_b, endLabel);

            if (TestImmOpZeroOrOne())
            {
                emit.DefineInlineTempLabel(nonZeroLabel);
            }
        }

        immValue++;
    }

    bool Done() const
    {
        return (immValue > immUpperBound);
    }

    int ImmValue() const
    {
        return immValue;
    }

    bool TestImmOpZeroOrOne() const
    {
        return (immLowerBound == 0) && (immUpperBound == 1);
    }
};

template <class InsGenerator>
static void ExpandNonConstImm(CodeGen* codeGen, GenTree* immOp, GenTreeHWIntrinsic* intrin, InsGenerator generator)
{
    if (GenTreeIntCon* imm = immOp->IsContainedIntCon())
    {
        generator(imm->GetInt32Value());
        return;
    }

    ExpandNonConstImmHelper helper(codeGen, immOp, intrin);

    while (!helper.Done())
    {
        generator(helper.ImmValue());
        helper.EmitCaseEnd();
    }
}

void CodeGen::genHWIntrinsic(GenTreeHWIntrinsic* node)
{
    const HWIntrinsic intrin(node);

    if ((intrin.id == NI_Vector64_GetElement) || (intrin.id == NI_Vector128_GetElement))
    {
        genVectorGetElement(node);
        DefReg(node);

        return;
    }

    // TODO-MIKE-Review: These 2 have confusing names. emitSize appears to be the vector
    // element size while attr is the size of the vector itself. Well, unless the node's
    // type is not a vector. Oh well.
    emitAttr attr = node->TypeIs(TYP_VOID) ? EA_UNKNOWN : emitTypeSize(node->GetType());
    emitAttr emitSize;
    insOpts  opt;

    if (HWIntrinsicInfo::SIMDScalar(intrin.id))
    {
        emitSize = emitTypeSize(intrin.baseType);
        opt      = INS_OPTS_NONE;
    }
    else if (intrin.category == HW_Category_Scalar)
    {
        emitSize = emitActualTypeSize(intrin.baseType);
        opt      = INS_OPTS_NONE;
    }
    else
    {
        emitSize = emitVecTypeSize(node->GetSimdSize());
        opt      = GetVecArrangementOpt(emitSize, intrin.baseType);
    }

    const bool isRMW         = node->isRMWHWIntrinsic(compiler);
    const bool hasImmOperand = HWIntrinsicInfo::HasImmediateOperand(intrin.id);

    regNumber defReg = node->GetRegNum();
    regNumber regs[4]{REG_NA, REG_NA, REG_NA, REG_NA};

    noway_assert(intrin.numOperands <= _countof(regs));

    for (unsigned i = 0; i < intrin.numOperands; i++)
    {
        GenTree* op = node->GetOp(i);

        if (op->isContained())
        {
            assert(op->IsIntCon() || op->IsDblCon());
        }
        else
        {
            regs[i] = UseReg(op);
        }
    }

    Emitter& emit = *GetEmitter();

    if (intrin.IsTableDriven())
    {
        const instruction ins = HWIntrinsicInfo::lookupIns(intrin.id, intrin.baseType);
        assert(ins != INS_invalid);

        if (intrin.category == HW_Category_SIMDByIndexedElement)
        {
            if (isRMW)
            {
                assert(defReg != regs[1]);
                assert(defReg != regs[2]);

                emit.emitIns_Mov(INS_mov, attr, defReg, regs[0], /* canSkip */ true);

                regs[0] = regs[1];
                regs[1] = regs[2];
            }

            if (hasImmOperand)
            {
                GenTree* immOp = isRMW ? intrin.op4 : intrin.op3;

                ExpandNonConstImm(this, immOp, node, [&](int imm) {
                    emit.emitIns_R_R_R_I(ins, emitSize, defReg, regs[0], regs[1], imm, opt);
                });
            }
            else
            {
                emit.emitIns_R_R_R_I(ins, emitSize, defReg, regs[0], regs[1], 0, opt);
            }
        }
        else if ((intrin.category == HW_Category_ShiftLeftByImmediate) ||
                 (intrin.category == HW_Category_ShiftRightByImmediate))
        {
            assert(hasImmOperand);

            GenTree* immOp;

            if (isRMW)
            {
                emit.emitIns_Mov(INS_mov, attr, defReg, regs[0], /* canSkip */ true);

                immOp   = intrin.op3;
                regs[0] = regs[1];
            }
            else
            {
                immOp = intrin.op2;
            }

            ExpandNonConstImm(this, immOp, node,
                              [&](int imm) { emit.emitIns_R_R_I(ins, emitSize, defReg, regs[0], imm, opt); });
        }
        else
        {
            assert(!hasImmOperand);

            switch (intrin.numOperands)
            {
                case 1:
                    emit.emitIns_R_R(ins, emitSize, defReg, regs[0], opt);
                    break;

                case 2:
                    if (isRMW)
                    {
                        assert(defReg != regs[1]);

                        emit.emitIns_Mov(INS_mov, attr, defReg, regs[0], /* canSkip */ true);
                        emit.emitIns_R_R(ins, emitSize, defReg, regs[1], opt);
                    }
                    else
                    {
                        emit.emitIns_R_R_R(ins, emitSize, defReg, regs[0], regs[1], opt);
                    }
                    break;

                case 3:
                    assert(isRMW);
                    assert(defReg != regs[1]);
                    assert(defReg != regs[2]);

                    emit.emitIns_Mov(INS_mov, attr, defReg, regs[0], /* canSkip */ true);
                    emit.emitIns_R_R_R(ins, emitSize, defReg, regs[1], regs[2], opt);
                    break;

                default:
                    unreached();
            }
        }
    }
    else
    {
        instruction ins = INS_invalid;

        switch (intrin.id)
        {
            case NI_AdvSimd_AddWideningLower:
                assert(varTypeIsIntegral(intrin.baseType));

                if (intrin.op1->TypeIs(TYP_SIMD8))
                {
                    ins = varTypeIsUnsigned(intrin.baseType) ? INS_uaddl : INS_saddl;
                }
                else
                {
                    assert(intrin.op1->TypeIs(TYP_SIMD16));
                    ins = varTypeIsUnsigned(intrin.baseType) ? INS_uaddw : INS_saddw;
                }
                break;

            case NI_AdvSimd_SubtractWideningLower:
                assert(varTypeIsIntegral(intrin.baseType));

                if (intrin.op1->TypeIs(TYP_SIMD8))
                {
                    ins = varTypeIsUnsigned(intrin.baseType) ? INS_usubl : INS_ssubl;
                }
                else
                {
                    assert(intrin.op1->TypeIs(TYP_SIMD16));
                    ins = varTypeIsUnsigned(intrin.baseType) ? INS_usubw : INS_ssubw;
                }
                break;

            case NI_AdvSimd_AddWideningUpper:
                assert(varTypeIsIntegral(intrin.baseType));

                if (node->GetAuxiliaryType() == intrin.baseType)
                {
                    ins = varTypeIsUnsigned(intrin.baseType) ? INS_uaddl2 : INS_saddl2;
                }
                else
                {
                    ins = varTypeIsUnsigned(intrin.baseType) ? INS_uaddw2 : INS_saddw2;
                }
                break;

            case NI_AdvSimd_SubtractWideningUpper:
                assert(varTypeIsIntegral(intrin.baseType));

                if (node->GetAuxiliaryType() == intrin.baseType)
                {
                    ins = varTypeIsUnsigned(intrin.baseType) ? INS_usubl2 : INS_ssubl2;
                }
                else
                {
                    ins = varTypeIsUnsigned(intrin.baseType) ? INS_usubw2 : INS_ssubw2;
                }
                break;

            default:
                ins = HWIntrinsicInfo::lookupIns(intrin.id, intrin.baseType);
                break;
        }

        assert(ins != INS_invalid);

        switch (intrin.id)
        {
            case NI_AdvSimd_BitwiseSelect:
                // Even though BitwiseSelect is an RMW intrinsic per se, we don't want to mark it as such
                // since we can handle all possible allocation decisions for targetReg.
                assert(!isRMW);

                if (defReg == regs[0])
                {
                    emit.emitIns_R_R_R(INS_bsl, emitSize, defReg, regs[1], regs[2], opt);
                }
                else if (defReg == regs[1])
                {
                    emit.emitIns_R_R_R(INS_bif, emitSize, defReg, regs[2], regs[0], opt);
                }
                else if (defReg == regs[2])
                {
                    emit.emitIns_R_R_R(INS_bit, emitSize, defReg, regs[1], regs[0], opt);
                }
                else
                {
                    emit.emitIns_Mov(INS_mov, emitSize, defReg, regs[0], /* canSkip */ false);
                    emit.emitIns_R_R_R(INS_bsl, emitSize, defReg, regs[1], regs[2], opt);
                }
                break;

            case NI_AdvSimd_DuplicateSelectedScalarToVector64:
            case NI_AdvSimd_DuplicateSelectedScalarToVector128:
            case NI_AdvSimd_Arm64_DuplicateSelectedScalarToVector128:
                if (GenTreeIntCon* imm = intrin.op2->IsIntCon())
                {
                    assert(Arm64Imm::IsVecIndex(imm->GetValue(), emitSize, GetVecElemsize(opt)));
                }

                emitSize = emitActualTypeSize(node->GetType());
                opt      = GetVecArrangementOpt(emitSize, intrin.baseType);
                assert(opt != INS_OPTS_NONE);

                ExpandNonConstImm(this, intrin.op2, node,
                                  [&](int imm) { emit.emitIns_R_R_I(ins, emitSize, defReg, regs[0], imm, opt); });
                break;

            case NI_AdvSimd_Extract:
                emitSize = emitTypeSize(intrin.baseType);

                ExpandNonConstImm(this, intrin.op2, node, [&](int imm) {
                    emit.emitIns_R_R_I(ins, emitSize, defReg, regs[0], imm, INS_OPTS_NONE);
                });
                break;

            case NI_AdvSimd_ExtractVector64:
            case NI_AdvSimd_ExtractVector128:
                opt = (intrin.id == NI_AdvSimd_ExtractVector64) ? INS_OPTS_8B : INS_OPTS_16B;

                ExpandNonConstImm(this, intrin.op3, node, [&](int imm) {
                    const int byteIndex = genTypeSize(intrin.baseType) * imm;
                    emit.emitIns_R_R_R_I(ins, emitSize, defReg, regs[0], regs[1], byteIndex, opt);
                });
                break;

            case NI_AdvSimd_Insert:
                assert(isRMW);

                emit.emitIns_Mov(INS_mov, attr, defReg, regs[0], /* canSkip */ true);

                if (intrin.op3->isContained())
                {
                    assert(intrin.op2->isContained());

                    if (intrin.op3->IsIntegralConst(0) || intrin.op3->IsDblConPositiveZero())
                    {
                        ssize_t imm = intrin.op2->AsIntCon()->GetValue();
                        emit.emitIns_R_R_I(INS_ins, emitSize, defReg, REG_ZR, imm, opt);
                    }
                    else
                    {
                        assert(intrin.op2->IsIntegralConst(0));
                        double imm = intrin.op3->AsDblCon()->GetValue();
                        emit.emitIns_R_F(INS_fmov, emitSize, defReg, imm, opt);
                    }
                    break;
                }

                assert(defReg != regs[2]);

                if (varTypeIsFloating(intrin.baseType))
                {
                    ExpandNonConstImm(this, intrin.op2, node, [&](int imm) {
                        emit.emitIns_R_R_I_I(ins, emitSize, defReg, regs[2], imm, 0, opt);
                    });
                }
                else
                {
                    ExpandNonConstImm(this, intrin.op2, node,
                                      [&](int imm) { emit.emitIns_R_R_I(ins, emitSize, defReg, regs[2], imm, opt); });
                }
                break;

            case NI_AdvSimd_InsertScalar:
                assert(isRMW);
                assert(defReg != regs[2]);

                emit.emitIns_Mov(INS_mov, attr, defReg, regs[0], /* canSkip */ true);

                ExpandNonConstImm(this, intrin.op2, node,
                                  [&](int imm) { emit.emitIns_R_R_I_I(ins, emitSize, defReg, regs[2], imm, 0, opt); });
                break;

            case NI_AdvSimd_Arm64_InsertSelectedScalar:
                assert(isRMW);
                assert(defReg != regs[2]);

                emit.emitIns_Mov(INS_mov, attr, defReg, regs[0], /* canSkip */ true);
                emit.emitIns_R_R_I_I(ins, emitSize, defReg, regs[2], intrin.op2->AsIntCon()->GetInt32Value(),
                                     intrin.op4->AsIntCon()->GetInt32Value(), opt);
                break;

            case NI_AdvSimd_LoadAndInsertScalar:
                assert(isRMW);
                assert(defReg != regs[2]);

                emit.emitIns_Mov(INS_mov, attr, defReg, regs[0], /* canSkip */ true);

                ExpandNonConstImm(this, intrin.op2, node,
                                  [&](int imm) { emit.emitIns_R_R_I(ins, emitSize, defReg, regs[2], imm); });
                break;

            case NI_AdvSimd_StoreSelectedScalar:
                ExpandNonConstImm(this, intrin.op3, node,
                                  [&](int imm) { emit.emitIns_R_R_I(ins, emitSize, regs[1], regs[0], imm, opt); });
                break;

            case NI_Vector64_CreateScalarUnsafe:
            case NI_Vector128_CreateScalarUnsafe:
                if (GenTreeIntCon* imm = intrin.op1->IsContainedIntCon())
                {
                    emit.emitIns_R_I(INS_movi, emitSize, defReg, imm->GetValue(), opt);
                    break;
                }
                FALLTHROUGH;
            case NI_Vector64_CreateScalar:
            case NI_Vector128_CreateScalar:
                if (GenTreeDblCon* imm = intrin.op1->IsContainedDblCon())
                {
                    emit.emitIns_R_F(INS_fmov, emitTypeSize(intrin.baseType), defReg, imm->GetValue(), INS_OPTS_NONE);
                    break;
                }

                assert(intrin.op1->isUsedFromReg());

                bool canSkip;
                canSkip = varTypeIsFloating(intrin.baseType) && ((intrin.id == NI_Vector64_CreateScalarUnsafe) ||
                                                                 (intrin.id == NI_Vector128_CreateScalarUnsafe));
                emit.emitIns_Mov(INS_fmov, emitActualTypeSize(intrin.baseType), defReg, regs[0], canSkip,
                                 INS_OPTS_NONE);
                break;

            case NI_AdvSimd_Arm64_AddSaturateScalar:
                if (varTypeIsUnsigned(node->GetAuxiliaryType()) != varTypeIsUnsigned(intrin.baseType))
                {
                    ins = varTypeIsUnsigned(intrin.baseType) ? INS_usqadd : INS_suqadd;

                    emit.emitIns_Mov(INS_mov, attr, defReg, regs[0], /* canSkip */ true);
                    emit.emitIns_R_R(ins, emitSize, defReg, regs[1], opt);
                    break;
                }

                emit.emitIns_R_R_R(ins, emitSize, defReg, regs[0], regs[1], opt);
                break;

            case NI_AdvSimd_DuplicateToVector64:
            case NI_AdvSimd_DuplicateToVector128:
            case NI_AdvSimd_Arm64_DuplicateToVector64:
            case NI_AdvSimd_Arm64_DuplicateToVector128:
                if (varTypeIsFloating(intrin.baseType))
                {
                    if (GenTreeDblCon* dbl = intrin.op1->IsContainedDblCon())
                    {
                        emit.emitIns_R_F(INS_fmov, emitSize, defReg, dbl->GetValue(), opt);
                    }
                    else if (intrin.id == NI_AdvSimd_Arm64_DuplicateToVector64)
                    {
                        assert(intrin.baseType == TYP_DOUBLE);
                        assert(IsMovIns(ins));

                        emit.emitIns_Mov(ins, emitSize, defReg, regs[0], /* canSkip */ false, opt);
                    }
                    else
                    {
                        emit.emitIns_R_R_I(ins, emitSize, defReg, regs[0], 0, opt);
                    }
                    break;
                }

                if (GenTreeIntCon* imm = intrin.op1->IsContainedIntCon())
                {
                    opt = GetVecArrangementOpt(emitSize, intrin.baseType);
                    emit.emitIns_R_I(INS_movi, emitSize, defReg, imm->GetValue(), opt);
                    break;
                }

                if (IsMovIns(ins))
                {
                    emit.emitIns_Mov(ins, emitSize, defReg, regs[0], /* canSkip */ false, opt);
                    break;
                }

                emit.emitIns_R_R(ins, emitSize, defReg, regs[0], opt);
                break;

            case NI_AdvSimd_Arm64_StorePair:
            case NI_AdvSimd_Arm64_StorePairNonTemporal:
                emit.emitIns_R_R_R(ins, emitSize, regs[1], regs[2], regs[0]);
                break;
            case NI_AdvSimd_Arm64_StorePairScalar:
            case NI_AdvSimd_Arm64_StorePairScalarNonTemporal:
                emit.emitIns_R_R_R(ins, emitTypeSize(intrin.baseType), regs[1], regs[2], regs[0]);
                break;
            case NI_Crc32_ComputeCrc32:
            case NI_Crc32_ComputeCrc32C:
            case NI_Crc32_Arm64_ComputeCrc32:
            case NI_Crc32_Arm64_ComputeCrc32C:
                emit.emitIns_R_R_R(ins, emitSize, defReg, regs[0], regs[1], opt);
                break;
            case NI_AdvSimd_AbsoluteCompareLessThan:
            case NI_AdvSimd_AbsoluteCompareLessThanOrEqual:
            case NI_AdvSimd_CompareLessThan:
            case NI_AdvSimd_CompareLessThanOrEqual:
            case NI_AdvSimd_Arm64_AbsoluteCompareLessThan:
            case NI_AdvSimd_Arm64_AbsoluteCompareLessThanScalar:
            case NI_AdvSimd_Arm64_AbsoluteCompareLessThanOrEqual:
            case NI_AdvSimd_Arm64_AbsoluteCompareLessThanOrEqualScalar:
            case NI_AdvSimd_Arm64_CompareLessThan:
            case NI_AdvSimd_Arm64_CompareLessThanScalar:
            case NI_AdvSimd_Arm64_CompareLessThanOrEqual:
            case NI_AdvSimd_Arm64_CompareLessThanOrEqualScalar:
                emit.emitIns_R_R_R(ins, emitSize, defReg, regs[1], regs[0], opt);
                break;
            case NI_AdvSimd_FusedMultiplyAddScalar:
            case NI_AdvSimd_FusedMultiplyAddNegatedScalar:
            case NI_AdvSimd_FusedMultiplySubtractNegatedScalar:
            case NI_AdvSimd_FusedMultiplySubtractScalar:
                assert(opt == INS_OPTS_NONE);
                emit.emitIns_R_R_R_R(ins, emitSize, defReg, regs[1], regs[2], regs[0]);
                break;
            case NI_AdvSimd_Store:
                emit.emitIns_R_R(ins, emitSize, regs[1], regs[0], opt);
                break;
            case NI_AdvSimd_AddWideningLower:
            case NI_AdvSimd_AddWideningUpper:
            case NI_AdvSimd_SubtractWideningLower:
            case NI_AdvSimd_SubtractWideningUpper:
                emit.emitIns_R_R_R(ins, emitSize, defReg, regs[0], regs[1], opt);
                break;
            case NI_Vector64_get_Zero:
            case NI_Vector128_get_Zero:
                emit.emitIns_R_I(INS_movi, EA_16BYTE, defReg, 0, INS_OPTS_16B);
                break;
            case NI_Vector64_get_AllBitsSet:
                emit.emitIns_R_I(INS_movi, EA_8BYTE, defReg, 0xFF, INS_OPTS_8B);
                break;
            case NI_Vector128_get_AllBitsSet:
                emit.emitIns_R_I(INS_movi, EA_16BYTE, defReg, 0xFF, INS_OPTS_16B);
                break;
            case NI_Vector64_ToVector128:
                emit.emitIns_Mov(ins, emitSize, defReg, regs[0], /* canSkip */ false);
                break;
            case NI_Vector64_ToVector128Unsafe:
            case NI_Vector128_GetLower:
                emit.emitIns_Mov(ins, emitSize, defReg, regs[0], /* canSkip */ true);
                break;
            case NI_AdvSimd_ReverseElement16:
                emit.emitIns_R_R(ins, emitSize, defReg, regs[0], (emitSize == EA_8BYTE) ? INS_OPTS_4H : INS_OPTS_8H);
                break;
            case NI_AdvSimd_ReverseElement32:
                emit.emitIns_R_R(ins, emitSize, defReg, regs[0], (emitSize == EA_8BYTE) ? INS_OPTS_2S : INS_OPTS_4S);
                break;
            case NI_AdvSimd_ReverseElement8:
                emit.emitIns_R_R(ins, emitSize, defReg, regs[0], (emitSize == EA_8BYTE) ? INS_OPTS_8B : INS_OPTS_16B);
                break;
            default:
                unreached();
        }
    }

    DefReg(node);
}

void CodeGen::genVectorGetElement(GenTreeHWIntrinsic* node)
{
    assert((node->GetIntrinsic() == NI_Vector64_GetElement) || (node->GetIntrinsic() == NI_Vector128_GetElement));

    var_types eltType = node->GetSimdBaseType();
    GenTree*  vec     = node->GetOp(0);
    GenTree*  index   = node->GetOp(1);
    regNumber destReg = node->GetRegNum();
    Emitter&  emit    = *GetEmitter();

    if (vec->isUsedFromReg())
    {
        regNumber vecReg     = UseReg(vec);
        ssize_t   indexValue = index->AsIntCon()->GetValue();

        if (!varTypeIsFloating(eltType) || (destReg != vecReg) || (indexValue != 0))
        {
            instruction ins = HWIntrinsicInfo::lookupIns(node->GetIntrinsic(), eltType);

            emit.emitIns_R_R_I(ins, emitTypeSize(eltType), destReg, vecReg, indexValue, INS_OPTS_NONE);
        }

        return;
    }

    if (!index->IsIntCon())
    {
        regNumber baseReg;

        if (vec->OperIs(GT_LCL_VAR, GT_LCL_FLD))
        {
            baseReg = node->ExtractTempReg();
            emit.Ins_R_S(INS_lea, EA_PTRSIZE, baseReg, GetStackAddrMode(vec->AsLclVarCommon()));
        }
        else
        {
            baseReg = UseReg(vec->AsIndir()->GetAddr());
        }

        regNumber indexReg = UseReg(index);
        assert(baseReg != indexReg);

        emit.emitIns_R_R_R_Ext(ins_Load(eltType), emitTypeSize(eltType), destReg, baseReg, indexReg, INS_OPTS_UXTW,
                               genLog2(varTypeSize(eltType)));

        return;
    }

    int offset = index->AsIntCon()->GetInt32Value() * varTypeSize(eltType);

    if (vec->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        emit.Ins_R_S(ins_Load(eltType), emitActualTypeSize(eltType), destReg,
                     GetStackAddrMode(vec->AsLclVarCommon()->GetLcl(), vec->AsLclVarCommon()->GetLclOffs() + offset));

        return;
    }

    GenTree*  addr = vec->AsIndir()->GetAddr();
    regNumber baseReg;

    if (addr->isUsedFromReg())
    {
        baseReg = UseReg(addr);
    }
    else
    {
        GenTreeAddrMode* am = vec->AsIndir()->GetAddr()->AsAddrMode();

        baseReg = UseReg(am->GetBase());
        assert(!am->HasIndex());
        offset += am->GetOffset();
    }

    emit.emitIns_R_R_I(ins_Load(eltType), emitActualTypeSize(eltType), destReg, baseReg, offset);
}

#endif // TARGET_ARM64
