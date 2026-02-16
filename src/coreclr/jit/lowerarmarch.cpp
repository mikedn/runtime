// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_ARMARCH

#include "sideeffects.h"
#include "lower.h"
#include "codegen.h"
#ifdef FEATURE_HW_INTRINSICS
#include "hwintrinsic.h"
#endif

#ifdef TARGET_ARM

bool Lowering::IsCallTargetInRange(void* addr)
{
    return ArmImm::IsBlImm(reinterpret_cast<ssize_t>(addr), comp);
}

bool Lowering::IsImmOperand(GenTree* operand, GenTree* instr) const
{
    // TODO-CQ: We can contain a floating point 0.0 constant in VCMP.

    if (!operand->IsIntCon() || operand->AsIntCon()->ImmedValNeedsReloc(comp))
    {
        return false;
    }

    int32_t value = operand->AsIntCon()->GetInt32Value();

    switch (instr->GetOper())
    {
        case GT_ADD:
        case GT_SUB:
            return ArmImm::IsAddImm(value, instr->HasImplicitFlagsDef() ? INS_FLAGS_SET : INS_FLAGS_DONT_CARE);
        case GT_OVF_SADD:
        case GT_OVF_UADD:
        case GT_OVF_SSUB:
        case GT_OVF_USUB:
            return ArmImm::IsAddImm(value, INS_FLAGS_SET);
        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GE:
        case GT_GT:
        case GT_CMP:
        case GT_AND:
        case GT_OR:
        case GT_XOR:
            return ArmImm::IsAluImm(value);
        default:
            return false;
    }
}

void Lowering::LowerFloatMul(GenTreeOp* mul)
{
    assert(mul->OperIs(GT_FMUL));

    GenTree* op2 = mul->GetOp(1);

    if (op2->IsDblCon2())
    {
        op2->SetContained();
    }
}

#endif // TARGET_ARM

void Lowering::LowerLclStoreArch(GenTreeLclStore* store)
{
    GenTree* src = store->GetValue();

    if (GenTreeIntCon* con = src->IsIntCon())
    {
        LclVarDsc* lcl = store->GetLcl();

        // TODO-MIKE-Review: This code is likely useless on ARM64, str and strh/strb
        // have the same encoding size. Also, the imm adjustment appears to have been
        // mindlessly copied from x86.

        if (varTypeIsSmall(store->GetType()) && !lcl->IsPromotedField() && !lcl->lvWasStructField)
        {
            assert(varActualTypeIsInt(lcl->GetType()));

            if (!varTypeIsSmallUnsigned(lcl->GetType()))
            {
                ssize_t value = con->GetValue();

                if (varTypeIsByte(store->GetType()))
                {
                    if ((value & 0x7f) != value)
                    {
                        value |= 0xffffff00;
                    }
                }
                else
                {
                    assert(varTypeIsShort(store->GetType()));

                    if ((value & 0x7fff) != value)
                    {
                        value |= 0xffff0000;
                    }
                }

                con->SetValue(value);
            }

            store->SetType(TYP_INT);
        }
    }

    ContainCheckStoreLcl(store);
}

void Lowering::LowerIndStoreArch(GenTreeIndStore* store)
{
    ContainCheckIndStore(store);
}

void Lowering::LowerArgStore(GenTreeArgStore* store)
{
    GenTree* src = store->GetOp(0);

    if (src->TypeIs(TYP_STRUCT))
    {
        if (src->OperIs(GT_IND_LOAD_OBJ))
        {
            unsigned size = src->AsIndLoadObj()->GetLayout()->GetSize();

            ContainStructStoreAddress(store, size, src->AsIndLoadObj()->GetAddr());
        }

        return;
    }

#ifdef TARGET_ARM64
    if (src->IsIntCon(0) || src->IsDblConPositiveZero() || src->IsHWIntrinsicZero())
    {
        src->SetContained();
    }
#endif
}

static bool IsValidGenericLoadStoreOffset(ssize_t offset, unsigned size ARM64_ARG(bool ldp))
{
    assert(size < INT32_MAX);

    // All integer load/store instructions on both ARM32 and ARM64 support
    // offsets in range -255..255. Of course, this is a rather conservative
    // check. For example, if the offset and size are a multiple of 8 we
    // could allow a combined offset of up to 32760 on ARM64.
    if ((offset < -255) || (offset > 255) || (offset + static_cast<int>(size) > 256))
    {
        return false;
    }

#ifdef TARGET_ARM64
    // Except that LDP/STP are more restrictive, they do not have an unscaled
    // offset form so the offset has to be a multiple of 8.
    if (ldp && (offset % REGSIZE_BYTES != 0))
    {
        return false;
    }
#endif

    return true;
}

void Lowering::ContainStructStoreAddress(GenTree* store, unsigned size, GenTree* addr)
{
    assert(store->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD, GT_ARG_STORE) ||
           (store->OperIs(GT_IND_STORE_BLK, GT_IND_STORE_OBJ) &&
            ((store->AsBlk()->GetKind() == StructStoreKind::UnrollCopy) ||
             (store->AsBlk()->GetKind() == StructStoreKind::UnrollInit ||
              (store->AsBlk()->GetKind() == StructStoreKind::UnrollRegs)))));

    if (addr->OperIs(GT_LCL_ADDR))
    {
        addr->SetContained();
        return;
    }

    if (!addr->OperIs(GT_ADD) || !addr->AsOp()->GetOp(1)->OperIs(GT_CNS_INT))
    {
        return;
    }

    GenTreeIntCon* offsetNode = addr->AsOp()->GetOp(1)->AsIntCon();
    ssize_t        offset     = offsetNode->GetValue();

    if (!IsValidGenericLoadStoreOffset(offset, size ARM64_ARG(size >= 2 * REGSIZE_BYTES)))
    {
        return;
    }

    GenTree* baseAddr = addr->AsOp()->GetOp(0);

    if (!IsSafeToMoveLclRegUseForward(store, baseAddr, nullptr))
    {
        return;
    }

    BlockRange().Unlink(offsetNode);

    addr->ChangeToAddrMode(baseAddr, nullptr, 0, static_cast<int>(offset));
    addr->SetContained();
}

void Lowering::ContainStructStoreAddressUnrollRegsWB(GenTree* addr)
{
    if (!addr->OperIs(GT_ADD))
    {
        return;
    }

    int offset;

    if (GenTreeIntCon* intCon = addr->AsOp()->GetOp(1)->IsIntCon())
    {
        if (intCon->GetValue() > 255)
        {
            return;
        }

        offset = intCon->GetInt32Value();

        BlockRange().Unlink(intCon);
    }
    else
    {
        return;
    }

    addr->ChangeToAddrMode(addr->AsOp()->GetOp(0), nullptr, 0, offset);
    addr->SetContained();
}

#ifdef TARGET_ARM

void Lowering::LowerRotateRight(GenTreeOp* node)
{
    assert(node->OperIs(GT_ROR) && node->TypeIs(TYP_INT));

    ContainCheckShiftRotate(node);
}

void Lowering::ContainCheckShiftRotate(GenTreeOp* node)
{
    assert(node->OperIsShiftOrRotate() && node->TypeIs(TYP_INT));

    if (node->OperIs(GT_LSH_HI, GT_RSH_LO))
    {
        GenTree* source = node->GetOp(0);
        assert(source->OperIs(GT_LONG));
        source->SetContained();
    }

    if (GenTree* shiftBy = node->GetOp(1)->IsIntCon())
    {
        shiftBy->SetContained();
    }
}

GenTree* Lowering::LowerConstIntDivRem(GenTreeOp* node)
{
    assert(node->OperIs(GT_SDIV, GT_SREM) && node->TypeIs(TYP_INT));

    GenTree* dividend = node->GetOp(0);
    GenTree* divisor  = node->GetOp(1);

    if (!divisor->IsIntCon())
    {
        return nullptr;
    }

    if (dividend->IsIntCon())
    {
        // We shouldn't see a SDIV/SREM with constant operands here but if we do then it's likely
        // because optimizations are disabled or it's a case that's supposed to throw an exception.
        // Don't optimize this.
        return nullptr;
    }

    int32_t divisorValue = divisor->AsIntCon()->GetInt32Value();

    if ((divisorValue == -1) || (divisorValue == 0))
    {
        // x / 0 and x % 0 can't be optimized because they are required to throw an exception.

        // x / -1 can't be optimized because INT_MIN / -1 is required to throw an exception.

        // x % -1 is always 0 and the IL spec says that the rem instruction "can" throw an exception if x is
        // the minimum representable integer. However, the C# spec says that an exception "is" thrown in this
        // case so optimizing this case would break C# code.

        // A runtime check could be used to handle this case but it's probably too rare to matter.
        return nullptr;
    }

    bool isDiv = node->OperIs(GT_SDIV);

    if (isDiv && (divisorValue == INT32_MIN))
    {
        node->SetOper(GT_EQ);

        return node;
    }

    uint32_t absDivisorValue =
        (divisorValue == INT32_MIN) ? static_cast<uint32_t>(divisorValue) : static_cast<uint32_t>(abs(divisorValue));

    if (!isPow2(absDivisorValue))
    {
        // Currently there's no S/UMULH for ARM32
        return nullptr;
    }

    LIR::Use use;
    if (!BlockRange().TryGetUse(node, &use))
    {
        return nullptr;
    }

    LIR::Use opDividend(BlockRange(), &node->gtOp1, node);
    dividend = ReplaceWithLclLoad(opDividend);

    GenTree*   shiftBy    = comp->gtNewIconNode(31);
    GenTreeOp* adjustment = comp->gtNewOperNode(GT_RSH, TYP_INT, dividend, shiftBy);
    shiftBy->SetContained();
    BlockRange().InsertAfter(dividend, shiftBy, adjustment);

    if (absDivisorValue == 2)
    {
        adjustment->SetOper(GT_RSZ);
    }
    else
    {
        GenTree*   imm  = comp->gtNewIconNode(absDivisorValue - 1, TYP_INT);
        GenTreeOp* mask = comp->gtNewOperNode(GT_AND, TYP_INT, adjustment, imm);
        BlockRange().InsertAfter(adjustment, imm, mask);
        ContainCheckBinary(mask);

        adjustment = mask;
    }

    dividend = comp->gtNewLclLoad(dividend->AsLclLoad()->GetLcl(), TYP_INT);

    GenTreeOp* adjustedDividend = comp->gtNewOperNode(GT_ADD, TYP_INT, adjustment, dividend);
    BlockRange().InsertAfter(adjustment, dividend, adjustedDividend);
    ContainCheckBinary(adjustedDividend);

    GenTree* newDivMod;
    BlockRange().Unlink(divisor);

    if (isDiv)
    {
        divisor->AsIntCon()->SetValue(genLog2(absDivisorValue));

        newDivMod = comp->gtNewOperNode(GT_RSH, TYP_INT, adjustedDividend, divisor);
        divisor->SetContained();
        BlockRange().InsertAfter(adjustedDividend, divisor, newDivMod);

        if (divisorValue < 0)
        {
            GenTree* neg = comp->gtNewOperNode(GT_NEG, TYP_INT, newDivMod);
            BlockRange().InsertAfter(newDivMod, neg);
            newDivMod = neg;
        }
    }
    else
    {
        // divisor % dividend = dividend - divisor x (dividend / divisor)
        // divisor x (dividend / divisor) translates to (dividend >> log2(divisor)) << log2(divisor)
        // which simply discards the low log2(divisor) bits, that's just dividend & ~(divisor - 1)
        divisor->AsIntCon()->SetValue(static_cast<int32_t>(~(absDivisorValue - 1)));

        GenTreeOp* mask = comp->gtNewOperNode(GT_AND, TYP_INT, adjustedDividend, divisor);
        dividend        = comp->gtNewLclLoad(dividend->AsLclLoad()->GetLcl(), TYP_INT);
        newDivMod       = comp->gtNewOperNode(GT_SUB, TYP_INT, dividend, mask);

        BlockRange().InsertAfter(adjustedDividend, divisor, mask, dividend, newDivMod);
        ContainCheckBinary(mask);
    }

    use.SetDef(newDivMod);
    BlockRange().Unlink(node);

    return newDivMod->gtNext;
}

GenTree* Lowering::LowerSignedDivRem(GenTree* node)
{
    assert(node->OperIs(GT_SDIV, GT_SREM) && node->TypeIs(TYP_INT));

    GenTree* next = node->gtNext;

    if (GenTree* newNode = LowerConstIntDivRem(node->AsOp()))
    {
        return newNode;
    }

    return next;
}

GenTree* Lowering::LowerCompare(GenTreeOp* cmp)
{
    if (cmp->GetOp(0)->TypeIs(TYP_LONG))
    {
        return DecomposeLongCompare(cmp);
    }

    ContainCheckCompare(cmp);
    return cmp->gtNext;
}

GenTree* Lowering::LowerJTrue(GenTreeUnOp* jtrue)
{
    ContainCheckJTrue(jtrue);

    assert(jtrue->gtNext == nullptr);
    return nullptr;
}
#endif // TARGET_ARM

#ifdef FEATURE_HW_INTRINSICS

//----------------------------------------------------------------------------------------------
// LowerHWIntrinsicFusedMultiplyAddScalar: Lowers AdvSimd_FusedMultiplyAddScalar intrinsics
//   when some of the operands are negated by "containing" such negation.
//
//  Arguments:
//     node - The original hardware intrinsic node
//
// |  op1 | op2 | op3 |
// |  +   |  +  |  +  | AdvSimd_FusedMultiplyAddScalar
// |  +   |  +  |  -  | AdvSimd_FusedMultiplySubtractScalar
// |  +   |  -  |  +  | AdvSimd_FusedMultiplySubtractScalar
// |  +   |  -  |  -  | AdvSimd_FusedMultiplyAddScalar
// |  -   |  +  |  +  | AdvSimd_FusedMultiplySubtractNegatedScalar
// |  -   |  +  |  -  | AdvSimd_FusedMultiplyAddNegatedScalar
// |  -   |  -  |  +  | AdvSimd_FusedMultiplyAddNegatedScalar
// |  -   |  -  |  -  | AdvSimd_FusedMultiplySubtractNegatedScalar
//
void Lowering::LowerHWIntrinsicFusedMultiplyAddScalar(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_AdvSimd_FusedMultiplyAddScalar);

    GenTree* op1 = node->GetOp(0);
    GenTree* op2 = node->GetOp(1);
    GenTree* op3 = node->GetOp(2);

    auto lowerOperand = [this](GenTree* op) {
        bool wasNegated = false;

        if (op->IsHWIntrinsic() && ((op->AsHWIntrinsic()->GetIntrinsic() == NI_AdvSimd_Arm64_DuplicateToVector64) ||
                                    (op->AsHWIntrinsic()->GetIntrinsic() == NI_Vector64_CreateScalarUnsafe)))
        {
            GenTreeHWIntrinsic* createVector64 = op->AsHWIntrinsic();
            GenTree*            valueOp        = createVector64->GetOp(0);

            if (valueOp->OperIs(GT_FNEG))
            {
                createVector64->SetOp(0, valueOp->AsUnOp()->GetOp(0));
                BlockRange().Unlink(valueOp);
                wasNegated = true;
            }
        }

        return wasNegated;
    };

    const bool op1WasNegated = lowerOperand(op1);
    const bool op2WasNegated = lowerOperand(op2);
    const bool op3WasNegated = lowerOperand(op3);

    if (op1WasNegated)
    {
        if (op2WasNegated != op3WasNegated)
        {
            node->SetIntrinsic(NI_AdvSimd_FusedMultiplyAddNegatedScalar);
        }
        else
        {
            node->SetIntrinsic(NI_AdvSimd_FusedMultiplySubtractNegatedScalar);
        }
    }
    else if (op2WasNegated != op3WasNegated)
    {
        node->SetIntrinsic(NI_AdvSimd_FusedMultiplySubtractScalar);
    }
}

void Lowering::LowerHWIntrinsic(GenTreeHWIntrinsic* node)
{
    assert(!node->TypeIs(TYP_SIMD32));

    if (node->TypeIs(TYP_SIMD12))
    {
        // SIMD12 HWINTRINSIC nodes produce in fact a SIMD16 value.
        node->SetType(TYP_SIMD16);
    }

    NamedIntrinsic intrinsicId = node->GetIntrinsic();

    switch (intrinsicId)
    {
        case NI_VEC_PACK:
            if (node->IsUnary())
            {
                LowerHWIntrinsicCreateBroadcast(node);
            }
            else
            {
                LowerHWIntrinsicCreate(node);
            }
            assert(!node->IsHWIntrinsic() || (node->GetIntrinsic() != intrinsicId));
            LowerNode(node);
            return;

        case NI_Vector64_CreateScalarUnsafe:
        case NI_Vector128_CreateScalarUnsafe:
            LowerHWIntrinsicCreateScalarUnsafe(node);
            return;

        case NI_VEC_SUM:
            LowerVecSum(node);
            return;

        case NI_VEC_EXTRACT:
            LowerVecExtract(node);
            return;

        case NI_AdvSimd_FusedMultiplyAddScalar:
            LowerHWIntrinsicFusedMultiplyAddScalar(node);
            break;

        case NI_AdvSimd_Insert:
            node->SetOp(2, TryRemoveCastIfPresent(node->GetSimdBaseType(), node->GetOp(2)));
            break;

        default:
            break;
    }

    ContainCheckHWIntrinsic(node);
}

bool Lowering::IsValidConstForMovImm(GenTreeHWIntrinsic* node)
{
    assert((node->GetIntrinsic() == NI_VEC_PACK) || (node->GetIntrinsic() == NI_Vector64_CreateScalar) ||
           (node->GetIntrinsic() == NI_Vector128_CreateScalar) ||
           (node->GetIntrinsic() == NI_Vector64_CreateScalarUnsafe) ||
           (node->GetIntrinsic() == NI_Vector128_CreateScalarUnsafe) ||
           (node->GetIntrinsic() == NI_AdvSimd_DuplicateToVector64) ||
           (node->GetIntrinsic() == NI_AdvSimd_DuplicateToVector128) ||
           (node->GetIntrinsic() == NI_AdvSimd_Arm64_DuplicateToVector64) ||
           (node->GetIntrinsic() == NI_AdvSimd_Arm64_DuplicateToVector128));
    assert(node->IsUnary());
    assert(varTypeIsTargetVec(node->GetType()));

    GenTree* op1 = node->GetOp(0);

    if (GenTreeIntCon* icon = op1->IsIntCon())
    {
        if ((node->GetIntrinsic() == NI_Vector64_CreateScalar) || (node->GetIntrinsic() == NI_Vector128_CreateScalar))
        {
            return false;
        }

        emitAttr attr = emitTypeSize(node->GetType());
        insOpts  opt  = GetVecArrangementOpt(attr, node->GetSimdBaseType());

        return Arm64Imm::IsMoviImm(icon->GetUInt64Value(), opt);
    }
    else if (GenTreeDblCon* dcon = op1->IsDblCon())
    {
        assert(varTypeIsFloating(node->GetSimdBaseType()));

        return Arm64Imm::IsFMovImm(dcon->GetValue());
    }

    return false;
}

void Lowering::LowerHWIntrinsicCreateScalarUnsafe(GenTreeHWIntrinsic* node)
{
    GenTree* op = node->GetOp(0);

    if (op->IsDblConPositiveZero() || op->IsIntCon(0))
    {
        BlockRange().Unlink(op);
        node->SetIntrinsic(NI_VEC_ZERO, 0);
    }
    else
    {
        ContainCheckHWIntrinsic(node);
    }
}

void Lowering::LowerHWIntrinsicCreate(GenTreeHWIntrinsic* node)
{
    var_types type    = node->GetType();
    var_types eltType = node->GetSimdBaseType();
    unsigned  numOps  = node->GetNumOps();

    assert(varTypeIsTargetVec(type));
    assert(varTypeIsArithmetic(eltType));
    assert(numOps == varTypeSize(type) / varTypeSize(eltType));

    // TODO-ARM64-CQ: We should be able to modify at least the paths that use Insert to trivially support partial
    // vector constants. With this, we can create a constant if say 50% of the inputs are also constant and just
    // insert the non-constant values which should still allow some gains.

    VectorConstant vecConst;

    if (vecConst.Create(node))
    {
        LowerHWIntrinsicCreateConst(node, vecConst);
        return;
    }

    unsigned nonZeroOpMask = 0;

    for (unsigned i = 0; i < numOps; i++)
    {
        GenTree* op = node->GetOp(i);

        // TODO-MIKE-CQ: This can be extended to small int elements but special handling is
        // needed to account for CreateScalar not having a small int version. We'd need to
        // either zero extend the small int value or not skip the first couple of 0 inserts.
        // Zero extending might be best as uxtb/h is faster than ins and we may get it for
        // free (e.g. if the operand is an indir or constant).

        if (op->IsDblConPositiveZero() || (!varTypeIsSmall(eltType) && op->IsIntCon(0)))
        {
            BlockRange().Unlink(op);
        }
        else
        {
            nonZeroOpMask |= 1 << i;
        }
    }

    // Only the first operand is non-0, convert to CreateScalar.
    if (nonZeroOpMask == 1)
    {
        GenTree* op = node->GetOp(0);
        node->SetIntrinsic(type == TYP_SIMD8 ? NI_Vector64_CreateScalar : NI_Vector128_CreateScalar, 1);
        node->SetOp(0, op);
        LowerNode(node);

        return;
    }

    // TODO-MIKE-Review: Much of this code assumes that operand order matches evaluation order.
    // This assumption only holds because gtSetEvalOrder/GTF_REVERSE_OPS aren't able to control
    // the ordering of intrinsic nodes with more than 2 operands.

    GenTree* vec = nullptr;

    for (unsigned i = 0; nonZeroOpMask != 0; nonZeroOpMask >>= 1, i++)
    {
        if ((nonZeroOpMask & 1) == 0)
        {
            continue;
        }

        GenTree* op = node->GetOp(i);

        if (i == 0)
        {
            NamedIntrinsic createScalar;

            // If we have 0 operands then use CreateScalar to ensure that upper elements are zeroed.
            if (nonZeroOpMask != ((1u << numOps) - 1))
            {
                createScalar = type == TYP_SIMD8 ? NI_Vector64_CreateScalar : NI_Vector128_CreateScalar;
            }
            else
            {
                createScalar = type == TYP_SIMD8 ? NI_Vector64_CreateScalarUnsafe : NI_Vector128_CreateScalarUnsafe;
            }

            op  = TryRemoveCastIfPresent(eltType, op);
            vec = comp->gtNewVecNode(type, createScalar, eltType, op);
            BlockRange().InsertAfter(op, vec);
            LowerNode(vec);

            continue;
        }

        GenTree* zero = nullptr;

        if (vec == nullptr)
        {
            zero = comp->gtNewVecZeroNode(type, eltType);
            vec  = zero;
        }

        GenTree* idx = comp->gtNewIconNode(i);

        if (nonZeroOpMask != 1)
        {
            vec = comp->gtNewVecNode(type, NI_AdvSimd_Insert, eltType, vec, idx, op);

            if (zero == nullptr)
            {
                BlockRange().InsertAfter(op, idx, vec);
            }
            else
            {
                BlockRange().InsertAfter(op, zero, idx, vec);
            }

            LowerNode(vec);
        }
        else
        {
            if (zero == nullptr)
            {
                BlockRange().InsertBefore(node, idx);
            }
            else
            {
                BlockRange().InsertBefore(node, zero, idx);
            }

            node->SetIntrinsic(NI_AdvSimd_Insert, 3);
            node->SetOp(0, vec);
            node->SetOp(1, idx);
            node->SetOp(2, op);
            LowerNode(node);
        }
    }
}

void Lowering::LowerHWIntrinsicCreateBroadcast(GenTreeHWIntrinsic* node)
{
    assert(node->IsUnary());

    var_types eltType = node->GetSimdBaseType();

    assert(varTypeIsTargetVec(node->GetType()));
    assert(varTypeIsArithmetic(eltType));

    VectorConstant vecConst;

    if (!IsValidConstForMovImm(node) && vecConst.Broadcast(node))
    {
        LowerHWIntrinsicCreateConst(node, vecConst);
        return;
    }

    NamedIntrinsic intrinsic;

    if (varTypeSize(eltType) == 8)
    {
        intrinsic =
            node->TypeIs(TYP_SIMD8) ? NI_AdvSimd_Arm64_DuplicateToVector64 : NI_AdvSimd_Arm64_DuplicateToVector128;
    }
    else
    {
        intrinsic = node->TypeIs(TYP_SIMD8) ? NI_AdvSimd_DuplicateToVector64 : NI_AdvSimd_DuplicateToVector128;
    }

    node->SetIntrinsic(intrinsic);
    node->SetOp(0, TryRemoveCastIfPresent(eltType, node->GetOp(0)));
}

void Lowering::LowerHWIntrinsicCreateConst(GenTreeHWIntrinsic* node, const VectorConstant& vecConst)
{
    var_types type    = node->GetType();
    var_types eltType = node->GetSimdBaseType();
    unsigned  numOps  = node->GetNumOps();

    assert(varTypeIsTargetVec(type));
    assert(varTypeIsArithmetic(eltType));

    for (unsigned i = 0; i < numOps; i++)
    {
        BlockRange().Unlink(node->GetOp(i));
    }

    if (vecConst.AllBitsZero(type))
    {
        node->SetIntrinsic(NI_VEC_ZERO);
        node->SetNumOps(0);
        return;
    }

    if (vecConst.AllBitsOne(type))
    {
        node->SetIntrinsic(NI_VEC_ONE_BITS);
        node->SetNumOps(0);
        return;
    }

    ConstData* data = comp->codeGen->GetConst(vecConst.u8, varTypeSize(type), varTypeSize(type) DEBUGARG(type));

    GenTree* addr = new (comp, GT_CONST_ADDR) GenTreeConstAddr(data);
    BlockRange().InsertBefore(node, addr);

    GenTree* indir = node;
    indir->ChangeOper(GT_IND_LOAD);
    indir->AsIndLoad()->SetAddr(addr);
}

void Lowering::LowerVecExtract(GenTreeHWIntrinsic* node)
{
    var_types eltType = node->GetSimdBaseType();

    assert(varTypeIsArithmetic(eltType));

    GenTree* vec = node->GetOp(0);
    GenTree* idx = node->GetOp(1);

    if (IsMemOperand(vec) && IsSafeToMoveMemOperandForward(node, vec))
    {
        vec->SetContained();
    }

    if (!idx->IsIntCon())
    {
        if (!vec->isContained())
        {
            LclVarDsc*       tempLcl = GetSimdMemoryTemp(vec->GetType());
            GenTreeLclStore* store   = comp->gtNewLclStore(tempLcl, vec->GetType(), vec);
            BlockRange().InsertAfter(vec, store);

            vec = comp->gtNewLclLoad(tempLcl, vec->GetType());
            BlockRange().InsertBefore(node, vec);
            node->SetOp(0, vec);
            vec->SetContained();
        }
        else if (GenTreeIndir* indir = vec->IsIndir())
        {
            indir->GetAddr()->ClearContained();
        }

        return;
    }

    // We should have a bounds check inserted for any index outside the allowed range
    // but we need to generate some code anyways, and so we'll mask here for simplicity.

    // TODO-MIKE-Cleanup: Ideally the "reg, imm" case should be handled by lowering it
    // to the corresponding instruction while the "mem, imm" case should be handled by
    // adjusting the memory offset as needed.
    // We only really need to something special about the "local, non-const-index" case
    // because the only way to get implement that is by taking the address of the local,
    // which requires making the local address exposed.

    unsigned count = varTypeTargetVecSize(vec->GetType()) / varTypeSize(eltType);
    unsigned index = idx->AsIntCon()->GetUInt32Value() % count;

    idx->AsIntCon()->SetValue(index);
    idx->SetContained();

    if (vec->isContained())
    {
        if (GenTreeIndir* indir = vec->IsIndir())
        {
            GenTree* addr = indir->GetAddr();

            if (addr->isContained())
            {
                int offset = static_cast<int>(index * varTypeSize(eltType));

                addr->SetContained(addr->IsAddrMode() && !addr->AsAddrMode()->HasIndex() &&
                                   (addr->AsAddrMode()->GetOffset() <= INT32_MAX - offset) &&
                                   IsValidGenericLoadStoreOffset(addr->AsAddrMode()->GetOffset() + offset, 0, false));
            }
        }
    }
}

void Lowering::LowerVecSum(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_VEC_SUM);
    assert(node->GetSimdBaseType() == TYP_FLOAT);
    assert(node->GetSimdSize() == 16);

    GenTree* vec = node->GetOp(0);

    node->SetOp(0, vec);
    LIR::Use vecUse(BlockRange(), &node->GetUse(0).NodeRef(), node);
    vec = ReplaceWithLclLoad(vecUse);

    GenTree* mul2 = comp->gtNewLclLoad(vec->AsLclLoad()->GetLcl(), TYP_SIMD16);
    GenTree* addp = comp->gtNewVecNode(TYP_SIMD16, NI_AdvSimd_Arm64_AddPairwise, TYP_FLOAT, vec, mul2);
    BlockRange().InsertBefore(node, mul2, addp);
    LowerNode(addp);

    node->SetIntrinsic(NI_AdvSimd_Arm64_AddPairwiseScalar, TYP_FLOAT, 8, 1);
    node->SetOp(0, addp);
    LowerNode(node);
}
#endif // FEATURE_HW_INTRINSICS

void Lowering::ContainCheckIndStore(GenTreeIndStore* store)
{
    ContainCheckIndir(store);

#ifdef TARGET_ARM64
    GenTree* value = store->GetValue();

    // TODO-MIKE-CQ-ARM64: SIMD16 0 is problematic to contain because we need
    // stp xzr, xzr, [...] but emitInsLoadStoreOp does not support stp. Currently
    // STORE_BLK.struct<16> works better than STOREIND.simd16 because of this.
    if (store->TypeIs(TYP_SIMD8, TYP_SIMD12))
    {
        if (value->IsHWIntrinsicZero())
        {
            value->SetContained();
        }
        else if (store->TypeIs(TYP_SIMD12))
        {
            ContainSIMD12MemToMemCopy(store, value);
        }
    }
    else if (value->IsIntCon(0) || value->IsDblConPositiveZero())
    {
        value->SetContained();
    }
#endif // TARGET_ARM64
}

void Lowering::ContainCheckIndir(GenTreeIndir* indir)
{
    assert(!indir->TypeIs(TYP_STRUCT));

    GenTree* addr = indir->GetAddr();

    if (GenTreeAddrMode* am = addr->IsAddrMode())
    {
#ifdef FEATURE_SIMD
        if (indir->TypeIs(TYP_SIMD12) && (am->HasIndex() || !IsValidGenericLoadStoreOffset(am->GetOffset(), 8, false)))
        {
            return;
        }
#endif

#ifdef TARGET_ARM
        // ARM floating-point load/store doesn't support a form similar to integer
        // ldr Rdst, [Rbase + Roffset] with offset in a register. The only supported
        // form is vldr Rdst, [Rbase + imm] with a more limited constraint on the imm.
        if (varTypeIsFloating(indir->GetType()) && (am->HasIndex() || !ArmImm::IsVLdStImm(am->GetOffset())))
        {
            return;
        }
#endif

        if (!IsSafeToMoveAddrModeForward(indir, am))
        {
            return;
        }

        addr->SetContained();
    }
#ifdef TARGET_ARM64
    else if (addr->OperIs(GT_CONST_ADDR, GT_LCL_ADDR))
#else
    else if (addr->OperIs(GT_LCL_ADDR))
#endif
    {
#ifdef FEATURE_SIMD
        if (indir->TypeIs(TYP_SIMD12))
        {
            return;
        }
#endif

        addr->SetContained();
    }
}

void Lowering::ContainCheckBinary(GenTreeOp* node)
{
    ContainImmOperand(node, node->GetOp(1));
}

void Lowering::ContainCheckStoreLcl(GenTreeLclRef* store)
{
    assert(store->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD));

    GenTree* src = store->GetOp(0);

#ifdef TARGET_ARM64
    if (src->IsIntCon(0) || src->IsDblConPositiveZero() || src->IsHWIntrinsicZero())
    {
        src->SetContained();
        return;
    }

    if (store->TypeIs(TYP_SIMD12) && IsMemStore(store))
    {
        ContainSIMD12MemToMemCopy(store, src);
        return;
    }
#endif

#ifdef TARGET_ARM
    if (src->OperIs(GT_LONG))
    {
        src->SetContained();
        return;
    }
#endif

    // If the source is a containable immediate, make it contained, unless it is
    // an int-size or larger store of zero to memory, because we can generate smaller code
    // by zeroing a register and then storing it.
    var_types type = store->GetLcl()->GetRegisterType(store);

    if (IsImmOperand(src, store) && (!src->IsIntCon(0) || varTypeIsSmall(type)))
    {
        src->SetContained();
    }
}

void Lowering::ContainCheckCompare(GenTreeOp* cmp)
{
    ContainImmOperand(cmp, cmp->GetOp(1));
}

void Lowering::ContainCheckBoundsChk(GenTreeBoundsChk* node)
{
    if (!ContainImmOperand(node, node->GetIndex()))
    {
        ContainImmOperand(node, node->GetLength());
    }
}

#ifdef FEATURE_HW_INTRINSICS
void Lowering::ContainCheckHWIntrinsic(GenTreeHWIntrinsic* node)
{
    // TODO-MIKE-CQ: It seems that there's no support for generating vector immediate ORR/BIC.

    GenTree* immOp = nullptr;

    if (HWIntrinsicInfo::HasImmediateOperand(node->GetIntrinsic()))
    {
        // TODO-Mike-Review: What's the point of HasImmediateOperand if you need
        // special casing to figure out which one is the imm operand?!?!
        switch (node->GetIntrinsic())
        {
            case NI_AdvSimd_Insert:
            case NI_AdvSimd_InsertScalar:
            case NI_AdvSimd_LoadAndInsertScalar:
                immOp = node->GetOp(1);
                break;
            default:
                immOp = node->GetLastOp();
                break;
        }

        assert(varTypeIsIntegral(immOp->GetType()));

        if (immOp->IsIntCon())
        {
            immOp->SetContained();
        }

        if (node->GetIntrinsic() == NI_AdvSimd_Arm64_InsertSelectedScalar)
        {
            assert(node->GetOp(1)->IsIntCon());
            assert(node->GetOp(3)->IsIntCon());

            node->GetOp(1)->SetContained();
        }
    }

    if (HWIntrinsicInfo::SupportsContainment(node->GetIntrinsic()))
    {
        switch (node->GetIntrinsic())
        {
            case NI_Vector64_CreateScalar:
            case NI_Vector128_CreateScalar:
            case NI_Vector64_CreateScalarUnsafe:
            case NI_Vector128_CreateScalarUnsafe:
            case NI_AdvSimd_DuplicateToVector64:
            case NI_AdvSimd_DuplicateToVector128:
            case NI_AdvSimd_Arm64_DuplicateToVector64:
            case NI_AdvSimd_Arm64_DuplicateToVector128:
                if (IsValidConstForMovImm(node))
                {
                    node->GetOp(0)->SetContained();
                }
                break;

            case NI_AdvSimd_Insert:
                if (GenTreeIntCon* index = immOp->IsIntCon())
                {
                    GenTree* value = node->GetOp(2);

                    if (value->IsIntCon(0) || value->IsDblConPositiveZero())
                    {
                        value->SetContained();
                    }
                    else if ((index->GetValue() == 0) && value->IsDblCon())
                    {
                        assert(varTypeIsFloating(node->GetSimdBaseType()));

                        if (Arm64Imm::IsFMovImm(value->AsDblCon()->GetValue()))
                        {
                            value->SetContained();
                        }
                    }
                }
                break;

            default:
                unreached();
        }
    }
}
#endif // FEATURE_HW_INTRINSICS

#endif // TARGET_ARMARCH
