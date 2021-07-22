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
#ifdef _MSC_VER
#pragma hdrstop
#endif

#ifdef TARGET_ARMARCH // This file is ONLY used for ARM and ARM64 architectures

#include "jit.h"
#include "sideeffects.h"
#include "lower.h"
#include "lsra.h"
#include "codegen.h"

#ifdef FEATURE_HW_INTRINSICS
#include "hwintrinsic.h"
#endif

//------------------------------------------------------------------------
// IsCallTargetInRange: Can a call target address be encoded in-place?
//
// Return Value:
//    True if the addr fits into the range.
//
bool Lowering::IsCallTargetInRange(void* addr)
{
    return comp->codeGen->validImmForBL((ssize_t)addr);
}

//------------------------------------------------------------------------
// IsContainableImmed: Is an immediate encodable in-place?
//
// Return Value:
//    True if the immediate can be folded into an instruction,
//    for example small enough and non-relocatable.
//
// TODO-CQ: we can contain a floating point 0.0 constant in a compare instruction
// (vcmp on arm, fcmp on arm64).
//
bool Lowering::IsContainableImmed(GenTree* parentNode, GenTree* childNode) const
{
    if (!varTypeIsFloating(parentNode->TypeGet()))
    {
        // Make sure we have an actual immediate
        if (!childNode->IsCnsIntOrI())
            return false;
        if (childNode->AsIntCon()->ImmedValNeedsReloc(comp))
            return false;

        // TODO-CrossBitness: we wouldn't need the cast below if GenTreeIntCon::gtIconVal had target_ssize_t type.
        target_ssize_t immVal = (target_ssize_t)childNode->AsIntCon()->gtIconVal;
        emitAttr       attr   = emitActualTypeSize(childNode->TypeGet());
        emitAttr       size   = EA_SIZE(attr);
#ifdef TARGET_ARM
        insFlags flags = parentNode->gtSetFlags() ? INS_FLAGS_SET : INS_FLAGS_DONT_CARE;
#endif

        switch (parentNode->OperGet())
        {
            case GT_ADD:
            case GT_SUB:
#ifdef TARGET_ARM64
            case GT_CMPXCHG:
            case GT_LOCKADD:
            case GT_XORR:
            case GT_XAND:
            case GT_XADD:
                return comp->compOpportunisticallyDependsOn(InstructionSet_Atomics)
                           ? false
                           : emitter::emitIns_valid_imm_for_add(immVal, size);
#elif defined(TARGET_ARM)
                return emitter::emitIns_valid_imm_for_add(immVal, flags);
#endif
                break;

#ifdef TARGET_ARM64
            case GT_EQ:
            case GT_NE:
            case GT_LT:
            case GT_LE:
            case GT_GE:
            case GT_GT:
            case GT_ARR_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
            case GT_HW_INTRINSIC_CHK:
#endif
                return emitter::emitIns_valid_imm_for_cmp(immVal, size);
            case GT_AND:
            case GT_OR:
            case GT_XOR:
            case GT_TEST_EQ:
            case GT_TEST_NE:
                return emitter::emitIns_valid_imm_for_alu(immVal, size);
            case GT_JCMP:
                assert(((parentNode->gtFlags & GTF_JCMP_TST) == 0) ? (immVal == 0) : isPow2(immVal));
                return true;
#elif defined(TARGET_ARM)
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
                return emitter::emitIns_valid_imm_for_alu(immVal);
#endif // TARGET_ARM

#ifdef TARGET_ARM64
            case GT_STORE_LCL_FLD:
            case GT_STORE_LCL_VAR:
                if (immVal == 0)
                    return true;
                break;
#endif

            default:
                break;
        }
    }

    return false;
}

void Lowering::LowerStoreLclVarArch(GenTreeLclVar* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR));

    GenTree* src = store->GetOp(0);

    if (src->OperIs(GT_CNS_INT))
    {
        // Try to widen the ops if they are going into a local var.
        GenTreeIntCon* con    = src->AsIntCon();
        ssize_t        ival   = con->gtIconVal;
        unsigned       varNum = store->GetLclNum();
        LclVarDsc*     varDsc = comp->lvaGetDesc(varNum);

        unsigned size = genTypeSize(store);
        // If we are storing a constant into a local variable
        // we extend the size of the store here
        if ((size < 4) && !varTypeIsStruct(varDsc))
        {
            if (!varTypeIsUnsigned(varDsc))
            {
                if (genTypeSize(store) == 1)
                {
                    if ((ival & 0x7f) != ival)
                    {
                        ival = ival | 0xffffff00;
                    }
                }
                else
                {
                    assert(genTypeSize(store) == 2);
                    if ((ival & 0x7fff) != ival)
                    {
                        ival = ival | 0xffff0000;
                    }
                }
            }

            // A local stack slot is at least 4 bytes in size, regardless of
            // what the local var is typed as, so auto-promote it here
            // unless it is a field of a promoted struct
            // TODO-CQ: if the field is promoted shouldn't we also be able to do this?
            if (!varDsc->lvIsStructField)
            {
                store->gtType = TYP_INT;
                con->SetIconValue(ival);
            }
        }
    }

    ContainCheckStoreLcl(store);
}

void Lowering::LowerStoreIndir(GenTreeStoreInd* store)
{
    ContainCheckStoreIndir(store);
}

void Lowering::LowerStructStore(GenTreeBlk* store)
{
    GenTree*     dstAddr = store->GetAddr();
    GenTree*     src     = store->GetValue();
    ClassLayout* layout  = store->GetLayout();
    unsigned     size    = layout != nullptr ? layout->GetSize() : UINT32_MAX;

    if (src->OperIs(GT_INIT_VAL, GT_CNS_INT))
    {
        if (src->OperIs(GT_INIT_VAL))
        {
            src->SetContained();
            src = src->AsUnOp()->GetOp(0);
        }

        if ((size > INITBLK_UNROLL_LIMIT) || !src->OperIs(GT_CNS_INT))
        {
            store->SetKind(StructStoreKind::MemSet);
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
#ifdef TARGET_ARM64
                // On ARM64 we can just use REG_ZR instead of having to load
                // the constant into a real register like on ARM32.
                src->SetContained();
#endif
            }
#ifdef TARGET_ARM64
            else if (size >= REGSIZE_BYTES)
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

            ContainBlockStoreAddress(store, size, dstAddr);
        }
    }
    else
    {
        assert(src->OperIs(GT_IND, GT_OBJ, GT_BLK, GT_LCL_VAR, GT_LCL_FLD));
        src->SetContained();

        if (src->OperIs(GT_IND, GT_OBJ, GT_BLK))
        {
            // TODO-Cleanup: Make sure that GT_IND lowering didn't mark the source address as contained.
            // Sometimes the GT_IND type is a non-struct type and then GT_IND lowering may contain the
            // address, not knowing that GT_IND is part of a block op that has containment restrictions.
            src->AsIndir()->GetAddr()->ClearContained();
        }

        // If the struct contains GC pointers we need to generate GC write barriers, unless
        // the destination is a local variable. Even if the destination is a local we're still
        // going to use UnrollWB if the size is too large for normal unrolling.

        if ((layout != nullptr) && layout->HasGCPtr() && (!dstAddr->OperIsLocalAddr() || (size > CPBLK_UNROLL_LIMIT)))
        {
            assert(dstAddr->TypeIs(TYP_BYREF, TYP_I_IMPL));

            store->SetKind(StructStoreKind::UnrollCopyWB);
        }
        else if (size <= CPBLK_UNROLL_LIMIT)
        {
            store->SetKind(StructStoreKind::UnrollCopy);

            if (src->OperIs(GT_IND, GT_OBJ, GT_BLK))
            {
                ContainBlockStoreAddress(store, size, src->AsIndir()->GetAddr());
            }

            ContainBlockStoreAddress(store, size, dstAddr);
        }
        else
        {
            store->SetKind(StructStoreKind::MemCpy);
        }
    }
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
        if (src->OperIs(GT_OBJ))
        {
            unsigned size = src->AsObj()->GetLayout()->GetSize();

            ContainBlockStoreAddress(putArgStk, size, src->AsObj()->GetAddr());
        }

        return;
    }

#ifdef TARGET_ARM64
    if (src->IsIntegralConst(0) || src->IsDblConPositiveZero())
    {
        src->SetContained();
    }
#endif
}

bool IsValidGenericLoadStoreOffset(ssize_t offset, unsigned size ARM64_ARG(bool ldp))
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

//------------------------------------------------------------------------
// ContainBlockStoreAddress: Attempt to contain an address used by an unrolled block store.
//
// Arguments:
//    store - the block store node
//    size - the block size
//    addr - the address node to try to contain
//
void Lowering::ContainBlockStoreAddress(GenTree* store, unsigned size, GenTree* addr)
{
    assert(
        (store->OperIs(GT_STORE_BLK, GT_STORE_OBJ) && ((store->AsBlk()->GetKind() == StructStoreKind::UnrollCopy) ||
                                                       (store->AsBlk()->GetKind() == StructStoreKind::UnrollInit))) ||
        store->OperIsPutArgStkOrSplit());

    if (addr->OperIsLocalAddr())
    {
        addr->SetContained();
        return;
    }

    if (!addr->OperIs(GT_ADD) || addr->gtOverflow() || !addr->AsOp()->gtGetOp2()->OperIs(GT_CNS_INT))
    {
        return;
    }

    GenTreeIntCon* offsetNode = addr->AsOp()->gtGetOp2()->AsIntCon();
    ssize_t        offset     = offsetNode->IconValue();

    if (!IsValidGenericLoadStoreOffset(offset, size ARM64_ARG(size >= 2 * REGSIZE_BYTES)))
    {
        return;
    }

    if (!IsSafeToContainMem(store, addr))
    {
        return;
    }

    BlockRange().Remove(offsetNode);

    addr->ChangeOper(GT_LEA);
    addr->AsAddrMode()->SetIndex(nullptr);
    addr->AsAddrMode()->SetScale(0);
    addr->AsAddrMode()->SetOffset(static_cast<int>(offset));
    addr->SetContained();
}

//------------------------------------------------------------------------
// LowerRotate: Lower GT_ROL and GT_ROR nodes.
//
// Arguments:
//    tree - the node to lower
//
// Return Value:
//    None.
//
void Lowering::LowerRotate(GenTree* tree)
{
    if (tree->OperGet() == GT_ROL)
    {
        // There is no ROL instruction on ARM. Convert ROL into ROR.
        GenTree* rotatedValue        = tree->AsOp()->gtOp1;
        unsigned rotatedValueBitSize = genTypeSize(rotatedValue->gtType) * 8;
        GenTree* rotateLeftIndexNode = tree->AsOp()->gtOp2;

        if (rotateLeftIndexNode->IsCnsIntOrI())
        {
            ssize_t rotateLeftIndex                    = rotateLeftIndexNode->AsIntCon()->gtIconVal;
            ssize_t rotateRightIndex                   = rotatedValueBitSize - rotateLeftIndex;
            rotateLeftIndexNode->AsIntCon()->gtIconVal = rotateRightIndex;
        }
        else
        {
            GenTree* tmp = comp->gtNewOperNode(GT_NEG, genActualType(rotateLeftIndexNode->gtType), rotateLeftIndexNode);
            BlockRange().InsertAfter(rotateLeftIndexNode, tmp);
            tree->AsOp()->gtOp2 = tmp;
        }
        tree->ChangeOper(GT_ROR);
    }
    ContainCheckShiftRotate(tree->AsOp());
}

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

            if (valueOp->OperIs(GT_NEG))
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

//----------------------------------------------------------------------------------------------
// Lowering::LowerHWIntrinsic: Perform containment analysis for a hardware intrinsic node.
//
//  Arguments:
//     node - The hardware intrinsic node.
//
void Lowering::LowerHWIntrinsic(GenTreeHWIntrinsic* node)
{
    assert(node->TypeGet() != TYP_SIMD32);

    if (node->TypeGet() == TYP_SIMD12)
    {
        // GT_HWINTRINSIC node requiring to produce TYP_SIMD12 in fact
        // produces a TYP_SIMD16 result
        node->gtType = TYP_SIMD16;
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

    if (varTypeIsIntegral(node->GetSimdBaseType()) && op1->OperIs(GT_CAST))
    {
        // We will sometimes get a cast around a constant value (such as for
        // certain long constants) which would block the below containment.
        // So we will temporarily check what the cast is from instead so we
        // can catch those cases as well.

        // TODO-MIKE-Review: Huh, why would a cast from a constant would ever reach
        // lowering? And if that does happen then how can the cast be removed without
        // checking anything? Overflow, cast from float, widening cast?!?!?!

        castOp = op1->AsCast()->CastOp();
        op1    = castOp;
    }

    if (GenTreeIntCon* icon = op1->IsIntCon())
    {
        emitAttr emitSize = emitActualTypeSize(getSIMDTypeForSize(node->GetSimdSize()));
        insOpts  opt      = emitSimdArrangementOpt(emitSize, node->GetSimdBaseType());

        if ((node->GetIntrinsic() == NI_Vector64_CreateScalar) || (node->GetIntrinsic() == NI_Vector128_CreateScalar))
        {
            return false;
        }

        if (emitter::EncodeMoviImm(icon->GetUInt64Value(), opt).ins != INS_invalid)
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

        return comp->GetEmitter()->emitIns_valid_imm_for_fmov(dcon->GetValue());
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

    var_types type = getSIMDTypeForSize(size);
    size           = (size == 12) ? 16 : size;
    unsigned align = size;

    UNATIVE_OFFSET       offset = comp->GetEmitter()->emitDataConst(vecConst.u8, size, align, type);
    CORINFO_FIELD_HANDLE handle = comp->eeFindJitDataOffs(offset);

    GenTree* addr = new (comp, GT_CLS_VAR_ADDR) GenTreeClsVar(GT_CLS_VAR_ADDR, TYP_I_IMPL, handle);
    BlockRange().InsertBefore(node, addr);

    GenTree* indir = node;
    indir->ChangeOper(GT_IND);
    indir->AsIndir()->SetAddr(addr);
}

void Lowering::LowerHWIntrinsicGetElement(GenTreeHWIntrinsic* node)
{
    var_types eltType = node->GetSimdBaseType();

    assert(varTypeIsArithmetic(eltType));

    GenTree* vec = node->GetOp(0);
    GenTree* idx = node->GetOp(1);

    if (IsContainableMemoryOp(vec) && IsSafeToContainMem(node, vec))
    {
        vec->SetContained();
    }

    if (!idx->IsIntCon())
    {
        if (!vec->isContained())
        {
            unsigned tempLclNum = GetSimdMemoryTemp(vec->GetType());
            GenTree* store      = NewStoreLclVar(tempLclNum, vec->GetType(), vec);
            BlockRange().InsertAfter(vec, store);

            vec = comp->gtNewLclvNode(tempLclNum, vec->GetType());
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
    vec = ReplaceWithLclVar(vecUse);

    GenTree* mul2 = comp->gtNewLclvNode(vec->AsLclVar()->GetLclNum(), TYP_SIMD16);
    GenTree* addp = comp->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AdvSimd_Arm64_AddPairwise, TYP_FLOAT, 16, vec, mul2);
    BlockRange().InsertBefore(node, mul2, addp);
    LowerNode(addp);

    node->SetIntrinsic(NI_AdvSimd_Arm64_AddPairwiseScalar, TYP_FLOAT, 8, 1);
    node->SetOp(0, addp);
    LowerNode(node);
}
#endif // FEATURE_HW_INTRINSICS

//------------------------------------------------------------------------
// Containment analysis
//------------------------------------------------------------------------

//------------------------------------------------------------------------
// ContainCheckCallOperands: Determine whether operands of a call should be contained.
//
// Arguments:
//    call       - The call node of interest
//
// Return Value:
//    None.
//
void Lowering::ContainCheckCallOperands(GenTreeCall* call)
{
    // There are no contained operands for arm.
}

void Lowering::ContainCheckStoreIndir(GenTreeStoreInd* store)
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

//------------------------------------------------------------------------
// ContainCheckIndir: Determine whether operands of an indir should be contained.
//
// Arguments:
//    indirNode - The indirection node of interest
//
// Notes:
//    This is called for both store and load indirections.
//
// Return Value:
//    None.
//
void Lowering::ContainCheckIndir(GenTreeIndir* indirNode)
{
    // If this is the rhs of a block copy it will be handled when we handle the store.
    if (indirNode->TypeGet() == TYP_STRUCT)
    {
        return;
    }

    GenTree* addr = indirNode->GetAddr();

#ifdef FEATURE_SIMD
    if (indirNode->TypeIs(TYP_SIMD12))
    {
        if (addr->OperIs(GT_LEA) && !addr->AsAddrMode()->HasIndex() &&
            IsValidGenericLoadStoreOffset(addr->AsAddrMode()->GetOffset(), 8, false) &&
            IsSafeToContainMem(indirNode, addr))
        {
            addr->SetContained();
        }

        return;
    }
#endif // FEATURE_SIMD

    if ((addr->OperGet() == GT_LEA) && IsSafeToContainMem(indirNode, addr))
    {
        bool makeContained = true;

#ifdef TARGET_ARM
        // ARM floating-point load/store doesn't support a form similar to integer
        // ldr Rdst, [Rbase + Roffset] with offset in a register. The only supported
        // form is vldr Rdst, [Rbase + imm] with a more limited constraint on the imm.
        GenTreeAddrMode* lea = addr->AsAddrMode();
        int              cns = lea->Offset();
        if (lea->HasIndex() || !emitter::emitIns_valid_imm_for_vldst_offset(cns))
        {
            if (indirNode->OperGet() == GT_STOREIND)
            {
                if (varTypeIsFloating(indirNode->AsStoreInd()->Data()))
                {
                    makeContained = false;
                }
            }
            else if (indirNode->OperGet() == GT_IND)
            {
                if (varTypeIsFloating(indirNode))
                {
                    makeContained = false;
                }
            }
        }
#endif // TARGET_ARM

        if (makeContained)
        {
            MakeSrcContained(indirNode, addr);
        }
    }
    else if (addr->OperIs(GT_CLS_VAR_ADDR, GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR))
    {
        // These nodes go into an addr mode:
        // - GT_CLS_VAR_ADDR turns into a constant.
        // - GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR is a stack addr mode.

        // make this contained, it turns into a constant that goes into an addr mode
        MakeSrcContained(indirNode, addr);
    }
}

//------------------------------------------------------------------------
// ContainCheckBinary: Determine whether a binary op's operands should be contained.
//
// Arguments:
//    node - the node we care about
//
void Lowering::ContainCheckBinary(GenTreeOp* node)
{
    // Check and make op2 contained (if it is a containable immediate)
    CheckImmedAndMakeContained(node, node->gtOp2);
}

//------------------------------------------------------------------------
// ContainCheckMul: Determine whether a mul op's operands should be contained.
//
// Arguments:
//    node - the node we care about
//
void Lowering::ContainCheckMul(GenTreeOp* node)
{
    ContainCheckBinary(node);
}

//------------------------------------------------------------------------
// ContainCheckDivOrMod: determine which operands of a div/mod should be contained.
//
// Arguments:
//    node - the node we care about
//
void Lowering::ContainCheckDivOrMod(GenTreeOp* node)
{
    assert(node->OperIs(GT_DIV, GT_UDIV));

    // ARM doesn't have a div instruction with an immediate operand
}

//------------------------------------------------------------------------
// ContainCheckShiftRotate: Determine whether a mul op's operands should be contained.
//
// Arguments:
//    node - the node we care about
//
void Lowering::ContainCheckShiftRotate(GenTreeOp* node)
{
    GenTree* shiftBy = node->gtOp2;
    assert(node->OperIsShiftOrRotate());

#ifdef TARGET_ARM
    GenTree* source = node->gtOp1;
    if (node->OperIs(GT_LSH_HI, GT_RSH_LO))
    {
        assert(source->OperGet() == GT_LONG);
        MakeSrcContained(node, source);
    }
#endif // TARGET_ARM

    if (shiftBy->IsCnsIntOrI())
    {
        MakeSrcContained(node, shiftBy);
    }
}

void Lowering::ContainCheckStoreLcl(GenTreeLclVarCommon* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    GenTree* src = store->gtGetOp1();

#ifdef TARGET_XARCH
    if (src->OperIs(GT_BITCAST))
    {
        // If we know that the source of the bitcast will be in a register, then we can make
        // the bitcast itself contained. This will allow us to store directly from the other
        // type if this node doesn't get a register.
        GenTree* bitCastSrc = src->AsUnOp()->GetOp(0);
        if (!bitCastSrc->isContained() && !bitCastSrc->IsRegOptional())
        {
            src->SetContained();
            return;
        }
    }
#endif

#ifdef TARGET_ARM64
    if (src->IsIntegralConst(0) || src->IsDblConPositiveZero() || src->IsHWIntrinsicZero())
    {
        src->SetContained();
        return;
    }

    if (store->TypeIs(TYP_SIMD12) && IsContainableMemoryOp(store))
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
    var_types type = comp->lvaGetDesc(store)->GetRegisterType(store);

    if (IsContainableImmed(store, src) && (!src->IsIntegralConst(0) || varTypeIsSmall(type)))
    {
        src->SetContained();
    }
}

void Lowering::ContainCheckCast(GenTreeCast* cast)
{
    GenTree* src = cast->GetOp(0);

#if !defined(TARGET_64BIT)
    if (src->OperIs(GT_LONG))
    {
        src->SetContained();
        return;
    }
#endif

    var_types srcType = src->GetType();
    var_types dstType = cast->GetCastType();

    if (varTypeIsIntegral(dstType) && varTypeIsIntegral(srcType))
    {
        bool isContainable = IsContainableMemoryOp(src);

        if (src->OperIs(GT_IND))
        {
            GenTree* addr = src->AsIndir()->GetAddr();

            if (src->AsIndir()->IsVolatile())
            {
                isContainable = false;
            }
            else if (addr->isContained())
            {
                // Indirs with contained address modes are problematic, thanks in part to messed up
                // address mode formation in LowerArrElem and createAddressNodeForSIMDInit, which
                // produce base+index+offset address modes that are invalid on ARMARCH. Such indirs
                // need a temp register and if the indir itself is contained then nobody's going to
                // reserve it, as this is normally done in LSRA's BuildIndir.
                //
                // Also, when the indir is contained, the type of the generated load instruction may
                // be different from the actual indir type, affecting immediate offset validity.
                //
                // So allow containment if the address mode is definitely always valid: base+index
                // of base+offset, if the offset is valid no matter the indir type is.
                //
                // Perhaps it would be better to not contain the indir and instead retype it
                // and remove the cast. Unfortunately there's at least on case where this is
                // not possible: there's no way to retype the indir in CAST<long>(IND<int>).
                // The best solution would be to lower indir+cast to the actual load instruction
                // to be emitted.

                if (!addr->IsAddrMode())
                {
                    isContainable = false;
                }
                else if (addr->AsAddrMode()->HasIndex() && (addr->AsAddrMode()->Offset() != 0))
                {
                    isContainable = false;
                }
                else if (addr->AsAddrMode()->Offset() < -255 || addr->AsAddrMode()->Offset() > 255)
                {
                    isContainable = false;
                }
            }
        }

        if (isContainable && (!cast->gtOverflow() || IsSafeToContainMem(cast, src)))
        {
            // If this isn't an overflow checking cast then we can move it
            // right after the source node to avoid the interference check.
            if (!cast->gtOverflow() && (cast->gtPrev != src))
            {
                BlockRange().Remove(cast);
                BlockRange().InsertAfter(src, cast);
            }

            src->SetContained();
        }
        else
        {
            src->SetRegOptional();
        }
    }
}

//------------------------------------------------------------------------
// ContainCheckCompare: determine whether the sources of a compare node should be contained.
//
// Arguments:
//    node - pointer to the node
//
void Lowering::ContainCheckCompare(GenTreeOp* cmp)
{
    CheckImmedAndMakeContained(cmp, cmp->gtOp2);
}

//------------------------------------------------------------------------
// ContainCheckBoundsChk: determine whether any source of a bounds check node should be contained.
//
// Arguments:
//    node - pointer to the node
//
void Lowering::ContainCheckBoundsChk(GenTreeBoundsChk* node)
{
    if (!CheckImmedAndMakeContained(node, node->GetIndex()))
    {
        CheckImmedAndMakeContained(node, node->GetLength());
    }
}

#ifdef FEATURE_HW_INTRINSICS

//----------------------------------------------------------------------------------------------
// ContainCheckHWIntrinsic: Perform containment analysis for a hardware intrinsic node.
//
//  Arguments:
//     node - The hardware intrinsic node.
//
void Lowering::ContainCheckHWIntrinsic(GenTreeHWIntrinsic* node)
{
    // TODO-MIKE-CQ: It seems that there's no support for generating vector immediate ORR/BIC.

    const HWIntrinsic intrin(node);

    const bool hasImmediateOperand = HWIntrinsicInfo::HasImmediateOperand(intrin.id);

    if ((intrin.category == HW_Category_ShiftLeftByImmediate) ||
        (intrin.category == HW_Category_ShiftRightByImmediate) ||
        ((intrin.category == HW_Category_SIMDByIndexedElement) && hasImmediateOperand))
    {
        switch (intrin.numOperands)
        {
            case 4:
                assert(varTypeIsIntegral(intrin.op4));
                if (intrin.op4->IsCnsIntOrI())
                {
                    MakeSrcContained(node, intrin.op4);
                }
                break;

            case 3:
                assert(varTypeIsIntegral(intrin.op3));
                if (intrin.op3->IsCnsIntOrI())
                {
                    MakeSrcContained(node, intrin.op3);
                }
                break;

            case 2:
                assert(varTypeIsIntegral(intrin.op2));
                if (intrin.op2->IsCnsIntOrI())
                {
                    MakeSrcContained(node, intrin.op2);
                }
                break;

            default:
                unreached();
        }
    }
    else if (hasImmediateOperand || HWIntrinsicInfo::SupportsContainment(intrin.id))
    {
        switch (intrin.id)
        {
            case NI_AdvSimd_DuplicateSelectedScalarToVector64:
            case NI_AdvSimd_DuplicateSelectedScalarToVector128:
            case NI_AdvSimd_Extract:
            case NI_AdvSimd_InsertScalar:
            case NI_AdvSimd_LoadAndInsertScalar:
            case NI_AdvSimd_Arm64_DuplicateSelectedScalarToVector128:
                assert(hasImmediateOperand);
                assert(varTypeIsIntegral(intrin.op2));
                if (intrin.op2->IsCnsIntOrI())
                {
                    MakeSrcContained(node, intrin.op2);
                }
                break;

            case NI_AdvSimd_ExtractVector64:
            case NI_AdvSimd_ExtractVector128:
            case NI_AdvSimd_StoreSelectedScalar:
                assert(hasImmediateOperand);
                assert(varTypeIsIntegral(intrin.op3));
                if (intrin.op3->IsCnsIntOrI())
                {
                    MakeSrcContained(node, intrin.op3);
                }
                break;

            case NI_AdvSimd_Insert:
                assert(hasImmediateOperand);
                assert(varTypeIsIntegral(intrin.op2));

                if (intrin.op2->IsCnsIntOrI())
                {
                    MakeSrcContained(node, intrin.op2);

                    if (intrin.op3->IsIntegralConst(0) || intrin.op3->IsDblConPositiveZero())
                    {
                        intrin.op3->SetContained();
                    }
                    else if ((intrin.op2->AsIntCon()->gtIconVal == 0) && intrin.op3->IsDblCon())
                    {
                        assert(varTypeIsFloating(intrin.baseType));

                        const double dataValue = intrin.op3->AsDblCon()->GetValue();

                        if (comp->GetEmitter()->emitIns_valid_imm_for_fmov(dataValue))
                        {
                            MakeSrcContained(node, intrin.op3);
                        }
                    }
                }
                break;

            case NI_AdvSimd_Arm64_InsertSelectedScalar:
                assert(hasImmediateOperand);
                assert(intrin.op2->IsCnsIntOrI());
                assert(intrin.op4->IsCnsIntOrI());

                MakeSrcContained(node, intrin.op2);
                MakeSrcContained(node, intrin.op4);
                break;

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
                    // Use node->gtOp1 as the above check may
                    // have removed a cast node and changed op1

                    MakeSrcContained(node, node->GetOp(0));
                }
                break;

            default:
                unreached();
        }
    }
}
#endif // FEATURE_HW_INTRINSICS

#endif // TARGET_ARMARCH
