// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX             Lowering for ARM and ARM64 common code                        XX
XX                                                                           XX
XX  This encapsulates common logic for lowering trees for the ARM and ARM64  XX
XX  architectures.  For a more detailed view of what is lowering, please     XX
XX  take a look at Lower.cpp                                                 XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"

#ifdef TARGET_ARMARCH

#include "sideeffects.h"
#include "lower.h"
#include "lsra.h"
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

#endif // TARGET_ARM

void Lowering::LowerStoreLclVarArch(GenTreeLclStore* store)
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

            if (!varTypeIsUnsigned(lcl->GetType()))
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

void Lowering::LowerPutArgStk(GenTreePutArgStk* putArgStk)
{
    GenTree* src = putArgStk->GetOp(0);

    if (src->OperIs(GT_FIELD_LIST))
    {
#ifdef TARGET_ARM64
        // Don't bother with GT_PUTARG_SPLIT, codegen doesn't support contained
        // constants currently and it's only used by varargs methods on win-64.
        if (putArgStk->OperIs(GT_PUTARG_STK))
        {
            for (GenTreeFieldList::Use& use : src->AsFieldList()->Uses())
            {
                GenTree* node = use.GetNode();

                if (node->IsIntegralConst(0) || node->IsDblConPositiveZero())
                {
                    node->SetContained();
                }
            }
        }
#endif
        return;
    }

    if (src->TypeIs(TYP_STRUCT))
    {
        if (src->OperIs(GT_IND_LOAD_OBJ))
        {
            unsigned size = src->AsIndLoadObj()->GetLayout()->GetSize();

            ContainStructStoreAddress(putArgStk, size, src->AsIndLoadObj()->GetAddr());
        }

        return;
    }

#ifdef TARGET_ARM64
    if (src->IsIntegralConst(0) && (putArgStk->GetSlotCount() > 1))
    {
        assert(comp->typIsLayoutNum(putArgStk->GetArgInfo()->GetSigTypeNum()));
        assert(putArgStk->GetSlotCount() == 2);

        src->SetContained();

        return;
    }

    if (src->IsIntegralConst(0) && comp->typIsLayoutNum(putArgStk->GetArgInfo()->GetSigTypeNum()))
    {
        ClassLayout* layout = comp->typGetLayoutByNum(putArgStk->GetArgInfo()->GetSigTypeNum());
        assert(layout->GetSize() <= REGSIZE_BYTES);

        if (layout->GetSize() > 4)
        {
            src->SetType(TYP_LONG);
        }

        return;
    }

    if (src->IsIntegralConst(0) || src->IsDblConPositiveZero())
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
    assert(store->OperIsPutArgSplit() || store->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD, GT_PUTARG_STK) ||
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
    ssize_t        offset     = offsetNode->IconValue();

    if (!IsValidGenericLoadStoreOffset(offset, size ARM64_ARG(size >= 2 * REGSIZE_BYTES)))
    {
        return;
    }

    if (!IsSafeToMoveForward(addr, store))
    {
        return;
    }

    BlockRange().Remove(offsetNode);

    addr->ChangeToAddrMode(addr->AsOp()->GetOp(0), nullptr, 0, static_cast<int>(offset));
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

        BlockRange().Remove(intCon);
    }
    else
    {
        return;
    }

    addr->ChangeToAddrMode(addr->AsOp()->GetOp(0), nullptr, 0, offset);
    addr->SetContained();
}

void Lowering::LowerRotate(GenTree* tree)
{
    if (tree->OperGet() == GT_ROL)
    {
        // There is no ROL instruction on ARM. Convert ROL into ROR.
        GenTree* rotatedValue        = tree->AsOp()->GetOp(0);
        unsigned rotatedValueBitSize = varTypeSize(rotatedValue->GetType()) * 8;
        GenTree* rotateLeftIndexNode = tree->AsOp()->GetOp(1);

        if (GenTreeIntCon* imm = rotateLeftIndexNode->IsIntCon())
        {
            imm->SetValue(rotatedValueBitSize - imm->GetValue());
        }
        else
        {
            GenTree* tmp =
                comp->gtNewOperNode(GT_NEG, varActualType(rotateLeftIndexNode->GetType()), rotateLeftIndexNode);
            BlockRange().InsertAfter(rotateLeftIndexNode, tmp);
            tree->AsOp()->SetOp(1, tmp);
        }
        tree->ChangeOper(GT_ROR);
    }
    ContainCheckShiftRotate(tree->AsOp());
}

#ifdef TARGET_ARM
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
                BlockRange().Remove(valueOp);
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
        case NI_Vector64_Create:
        case NI_Vector128_Create:
            if (node->IsUnary())
            {
                LowerHWIntrinsicCreateBroadcast(node);
            }
            else
            {
                LowerHWIntrinsicCreate(node);
            }
            assert(!node->OperIsHWIntrinsic() || (node->GetIntrinsic() != intrinsicId));
            LowerNode(node);
            return;

        case NI_Vector64_CreateScalarUnsafe:
        case NI_Vector128_CreateScalarUnsafe:
            LowerHWIntrinsicCreateScalarUnsafe(node);
            return;

        case NI_Vector128_Sum:
            LowerHWIntrinsicSum(node);
            return;

        case NI_Vector64_GetElement:
        case NI_Vector128_GetElement:
            LowerHWIntrinsicGetElement(node);
            return;

        case NI_Vector64_op_Equality:
        case NI_Vector128_op_Equality:
        case NI_Vector64_op_Inequality:
        case NI_Vector128_op_Inequality:
            unreached();

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

//----------------------------------------------------------------------------------------------
// Lowering::IsValidConstForMovImm: Determines if the given node can be replaced by a mov/fmov immediate instruction
//
//  Arguments:
//     node - The hardware intrinsic node.
//
//  Returns:
//     true if the node can be replaced by a mov/fmov immediate instruction; otherwise, false
//
//  IMPORTANT:
//     This check may end up modifying node's first operand if it is a cast node that can be removed
bool Lowering::IsValidConstForMovImm(GenTreeHWIntrinsic* node)
{
    assert((node->GetIntrinsic() == NI_Vector64_Create) || (node->GetIntrinsic() == NI_Vector128_Create) ||
           (node->GetIntrinsic() == NI_Vector64_CreateScalar) || (node->GetIntrinsic() == NI_Vector128_CreateScalar) ||
           (node->GetIntrinsic() == NI_Vector64_CreateScalarUnsafe) ||
           (node->GetIntrinsic() == NI_Vector128_CreateScalarUnsafe) ||
           (node->GetIntrinsic() == NI_AdvSimd_DuplicateToVector64) ||
           (node->GetIntrinsic() == NI_AdvSimd_DuplicateToVector128) ||
           (node->GetIntrinsic() == NI_AdvSimd_Arm64_DuplicateToVector64) ||
           (node->GetIntrinsic() == NI_AdvSimd_Arm64_DuplicateToVector128));
    assert(node->IsUnary());

    GenTree* op1    = node->GetOp(0);
    GenTree* castOp = nullptr;

    if (GenTreeIntCon* icon = op1->IsIntCon())
    {
        emitAttr emitSize = emitVecTypeSize(node->GetSimdSize());
        insOpts  opt      = GetVecArrangementOpt(emitSize, node->GetSimdBaseType());

        if ((node->GetIntrinsic() == NI_Vector64_CreateScalar) || (node->GetIntrinsic() == NI_Vector128_CreateScalar))
        {
            return false;
        }

        if (Arm64Imm::IsMoviImm(icon->GetUInt64Value(), opt))
        {
            if (castOp != nullptr)
            {
                // We found a containable immediate under
                // a cast, so remove the cast from the LIR.

                BlockRange().Remove(node->GetOp(0));
                node->SetOp(0, op1);
            }
            return true;
        }
    }
    else if (GenTreeDblCon* dcon = op1->IsDblCon())
    {
        assert(varTypeIsFloating(node->GetSimdBaseType()));
        assert(castOp == nullptr);

        return Arm64Imm::IsFMovImm(dcon->GetValue());
    }

    return false;
}

void Lowering::LowerHWIntrinsicCreateScalarUnsafe(GenTreeHWIntrinsic* node)
{
    GenTree* op = node->GetOp(0);

    if (op->IsDblConPositiveZero() || op->IsIntegralConst(0))
    {
        BlockRange().Remove(op);

        node->SetIntrinsic(node->GetIntrinsic() == NI_Vector128_CreateScalarUnsafe ? NI_Vector128_get_Zero
                                                                                   : NI_Vector64_get_Zero,
                           0);
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
    unsigned  size    = node->GetSimdSize();
    unsigned  numOps  = node->GetNumOps();

    assert(varTypeIsSIMD(type));
    assert(varTypeIsArithmetic(eltType));
    assert((size == 8) || (size == 16));
    assert(numOps == (size / varTypeSize(eltType)));

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

        if (op->IsDblConPositiveZero() || (!varTypeIsSmall(eltType) && op->IsIntegralConst(0)))
        {
            BlockRange().Remove(op);
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
        node->SetIntrinsic((size == 8) ? NI_Vector64_CreateScalar : NI_Vector128_CreateScalar, 1);
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
                createScalar = (size == 8) ? NI_Vector64_CreateScalar : NI_Vector128_CreateScalar;
            }
            else
            {
                createScalar = (size == 8) ? NI_Vector64_CreateScalarUnsafe : NI_Vector128_CreateScalarUnsafe;
            }

            op  = TryRemoveCastIfPresent(eltType, op);
            vec = comp->gtNewSimdHWIntrinsicNode(type, createScalar, eltType, size, op);
            BlockRange().InsertAfter(op, vec);
            LowerNode(vec);

            continue;
        }

        GenTree* zero = nullptr;

        if (vec == nullptr)
        {
            zero = comp->gtNewZeroSimdHWIntrinsicNode(type, eltType);
            vec  = zero;
        }

        GenTree* idx = comp->gtNewIconNode(i);

        if (nonZeroOpMask != 1)
        {
            vec = comp->gtNewSimdHWIntrinsicNode(type, NI_AdvSimd_Insert, eltType, size, vec, idx, op);

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
    unsigned  size    = node->GetSimdSize();

    assert(varTypeIsSIMD(node->GetType()));
    assert(varTypeIsArithmetic(eltType));
    assert((size == 8) || (size == 12) || (size == 16));

    VectorConstant vecConst;

    if (!IsValidConstForMovImm(node) && vecConst.Broadcast(node))
    {
        LowerHWIntrinsicCreateConst(node, vecConst);
        return;
    }

    NamedIntrinsic intrinsic;

    if (varTypeSize(eltType) == 8)
    {
        intrinsic = size == 8 ? NI_AdvSimd_Arm64_DuplicateToVector64 : NI_AdvSimd_Arm64_DuplicateToVector128;
    }
    else
    {
        intrinsic = size == 8 ? NI_AdvSimd_DuplicateToVector64 : NI_AdvSimd_DuplicateToVector128;
    }

    node->SetIntrinsic(intrinsic);
    node->SetOp(0, TryRemoveCastIfPresent(eltType, node->GetOp(0)));
}

void Lowering::LowerHWIntrinsicCreateConst(GenTreeHWIntrinsic* node, const VectorConstant& vecConst)
{
    unsigned numOps = node->GetNumOps();

    for (unsigned i = 0; i < numOps; i++)
    {
        BlockRange().Remove(node->GetOp(i));
    }

    unsigned size = node->GetSimdSize();

    if (vecConst.AllBitsZero(size))
    {
        node->SetIntrinsic((size == 8) ? NI_Vector64_get_Zero : NI_Vector128_get_Zero);
        node->SetNumOps(0);
        return;
    }

    if ((size != 12) && vecConst.AllBitsOne(size))
    {
        node->SetIntrinsic((size == 8) ? NI_Vector64_get_AllBitsSet : NI_Vector128_get_AllBitsSet);
        node->SetNumOps(0);
        return;
    }

    size = size == 12 ? 16 : size;

    ConstData* data = comp->codeGen->GetConst(vecConst.u8, size, size DEBUGARG(getSIMDTypeForSize(size)));

    GenTree* addr = new (comp, GT_CONST_ADDR) GenTreeConstAddr(data);
    BlockRange().InsertBefore(node, addr);

    GenTree* indir = node;
    indir->ChangeOper(GT_IND_LOAD);
    indir->AsIndLoad()->SetAddr(addr);
}

void Lowering::LowerHWIntrinsicGetElement(GenTreeHWIntrinsic* node)
{
    var_types eltType = node->GetSimdBaseType();

    assert(varTypeIsArithmetic(eltType));

    GenTree* vec = node->GetOp(0);
    GenTree* idx = node->GetOp(1);

    if (IsMemOperand(vec) && IsSafeToMoveForward(vec, node))
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

    unsigned count = node->GetSimdSize() / varTypeSize(eltType);
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

void Lowering::LowerHWIntrinsicSum(GenTreeHWIntrinsic* node)
{
    assert(node->GetIntrinsic() == NI_Vector128_Sum);
    assert(node->GetSimdBaseType() == TYP_FLOAT);
    assert(node->GetSimdSize() == 16);

    GenTree* vec = node->GetOp(0);

    node->SetOp(0, vec);
    LIR::Use vecUse(BlockRange(), &node->GetUse(0).NodeRef(), node);
    vec = ReplaceWithLclLoad(vecUse);

    GenTree* mul2 = comp->gtNewLclLoad(vec->AsLclLoad()->GetLcl(), TYP_SIMD16);
    GenTree* addp = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AdvSimd_Arm64_AddPairwise, TYP_FLOAT, 16, vec, mul2);
    BlockRange().InsertBefore(node, mul2, addp);
    LowerNode(addp);

    node->SetIntrinsic(NI_AdvSimd_Arm64_AddPairwiseScalar, TYP_FLOAT, 8, 1);
    node->SetOp(0, addp);
    LowerNode(node);
}
#endif // FEATURE_HW_INTRINSICS

void Lowering::ContainCheckCallOperands(GenTreeCall* call)
{
    // There are no contained operands for arm.
}

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
    else if (value->IsIntegralConst(0) || value->IsDblConPositiveZero())
    {
        value->SetContained();
    }
#endif // TARGET_ARM64
}

void Lowering::ContainCheckIndir(GenTreeIndir* indir)
{
    assert(!indir->TypeIs(TYP_STRUCT));

    GenTree* addr = indir->GetAddr();

#ifdef FEATURE_SIMD
    if (indir->TypeIs(TYP_SIMD12))
    {
        if (addr->IsAddrMode() && !addr->AsAddrMode()->HasIndex() &&
            IsValidGenericLoadStoreOffset(addr->AsAddrMode()->GetOffset(), 8, false) &&
            IsSafeToMoveForward(addr, indir))
        {
            addr->SetContained();
        }

        return;
    }
#endif // FEATURE_SIMD

    if (addr->IsAddrMode() && IsSafeToMoveForward(addr, indir))
    {
        bool makeContained = true;

#ifdef TARGET_ARM
        // ARM floating-point load/store doesn't support a form similar to integer
        // ldr Rdst, [Rbase + Roffset] with offset in a register. The only supported
        // form is vldr Rdst, [Rbase + imm] with a more limited constraint on the imm.
        GenTreeAddrMode* lea = addr->AsAddrMode();
        if (varTypeIsFloating(indir->GetType()) && (lea->HasIndex() || !ArmImm::IsVLdStImm(lea->GetOffset())))
        {
            makeContained = false;
        }
#endif

        if (makeContained)
        {
            addr->SetContained();
        }
    }
#ifdef TARGET_ARM64
    else if (addr->OperIs(GT_CONST_ADDR, GT_LCL_ADDR))
#else
    else if (addr->OperIs(GT_LCL_ADDR))
#endif
    {
        addr->SetContained();
    }
}

void Lowering::ContainCheckBinary(GenTreeOp* node)
{
    ContainImmOperand(node, node->GetOp(1));
}

void Lowering::ContainCheckMul(GenTreeOp* node)
{
#ifdef TARGET_64BIT
    assert(node->OperIs(GT_MUL, GT_OVF_SMUL, GT_OVF_UMUL, GT_SMULH, GT_UMULH));
#else
    assert(node->OperIs(GT_MUL, GT_OVF_SMUL, GT_OVF_UMUL, GT_SMULH, GT_UMULH, GT_SMULL, GT_UMULL));
#endif

    // ARM doesn't have mul instructions with an immediate operand
}

void Lowering::ContainCheckDivOrMod(GenTreeOp* node)
{
    assert(node->OperIs(GT_DIV, GT_UDIV));

    // ARM doesn't have div instructions with an immediate operand
}

void Lowering::ContainCheckShiftRotate(GenTreeOp* node)
{
    assert(node->OperIsShiftOrRotate());

#ifdef TARGET_ARM
    if (node->OperIs(GT_LSH_HI, GT_RSH_LO))
    {
        GenTree* source = node->GetOp(0);
        assert(source->OperIs(GT_LONG));
        source->SetContained();
    }
#endif

    if (GenTree* shiftBy = node->GetOp(1)->IsIntCon())
    {
        shiftBy->SetContained();
    }
}

void Lowering::ContainCheckStoreLcl(GenTreeLclVarCommon* store)
{
    assert(store->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD));

    GenTree* src = store->GetOp(0);

#ifdef TARGET_ARM64
    if (src->IsIntegralConst(0) || src->IsDblConPositiveZero() || src->IsHWIntrinsicZero())
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

    if (IsImmOperand(src, store) && (!src->IsIntegralConst(0) || varTypeIsSmall(type)))
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

                    if (value->IsIntegralConst(0) || value->IsDblConPositiveZero())
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
