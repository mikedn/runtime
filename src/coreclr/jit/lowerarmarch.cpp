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
#ifdef FEATURE_SIMD
            case GT_SIMD_CHK:
#endif
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

#ifdef FEATURE_SIMD
//----------------------------------------------------------------------------------------------
// Lowering::LowerSIMD: Perform containment analysis for a SIMD intrinsic node.
//
//  Arguments:
//     simdNode - The SIMD intrinsic node.
//
void Lowering::LowerSIMD(GenTreeSIMD* simdNode)
{
    assert(simdNode->gtType != TYP_SIMD32);

    if (simdNode->TypeGet() == TYP_SIMD12)
    {
        // GT_SIMD node requiring to produce TYP_SIMD12 in fact
        // produces a TYP_SIMD16 result
        simdNode->gtType = TYP_SIMD16;
    }

    ContainCheckSIMD(simdNode);
}
#endif // FEATURE_SIMD

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
    assert(node->gtHWIntrinsicId == NI_AdvSimd_FusedMultiplyAddScalar);

    GenTree* op1 = node->GetOp(0);
    GenTree* op2 = node->GetOp(1);
    GenTree* op3 = node->GetOp(2);

    auto lowerOperand = [this](GenTree* op) {
        bool wasNegated = false;

        if (op->OperIsHWIntrinsic() &&
            ((op->AsHWIntrinsic()->gtHWIntrinsicId == NI_AdvSimd_Arm64_DuplicateToVector64) ||
             (op->AsHWIntrinsic()->gtHWIntrinsicId == NI_Vector64_CreateScalarUnsafe)))
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
            node->gtHWIntrinsicId = NI_AdvSimd_FusedMultiplyAddNegatedScalar;
        }
        else
        {
            node->gtHWIntrinsicId = NI_AdvSimd_FusedMultiplySubtractNegatedScalar;
        }
    }
    else if (op2WasNegated != op3WasNegated)
    {
        node->gtHWIntrinsicId = NI_AdvSimd_FusedMultiplySubtractScalar;
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

    NamedIntrinsic intrinsicId = node->gtHWIntrinsicId;

    switch (intrinsicId)
    {
        case NI_Vector64_Create:
        case NI_Vector128_Create:
        {
            // We don't directly support the Vector64.Create or Vector128.Create methods in codegen
            // and instead lower them to other intrinsic nodes in LowerHWIntrinsicCreate so we expect
            // that the node is modified to either not be a HWIntrinsic node or that it is no longer
            // the same intrinsic as when it came in.

            LowerHWIntrinsicCreate(node);
            assert(!node->OperIsHWIntrinsic() || (node->gtHWIntrinsicId != intrinsicId));
            LowerNode(node);
            return;
        }

        case NI_Vector64_Dot:
        case NI_Vector128_Dot:
        {
            LowerHWIntrinsicDot(node);
            return;
        }

        case NI_Vector64_op_Equality:
        case NI_Vector128_op_Equality:
        {
            LowerHWIntrinsicCmpOp(node, GT_EQ);
            return;
        }

        case NI_Vector64_op_Inequality:
        case NI_Vector128_op_Inequality:
        {
            LowerHWIntrinsicCmpOp(node, GT_NE);
            return;
        }

        case NI_AdvSimd_FusedMultiplyAddScalar:
            LowerHWIntrinsicFusedMultiplyAddScalar(node);
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
    assert((node->gtHWIntrinsicId == NI_Vector64_Create) || (node->gtHWIntrinsicId == NI_Vector128_Create) ||
           (node->gtHWIntrinsicId == NI_Vector64_CreateScalarUnsafe) ||
           (node->gtHWIntrinsicId == NI_Vector128_CreateScalarUnsafe) ||
           (node->gtHWIntrinsicId == NI_AdvSimd_DuplicateToVector64) ||
           (node->gtHWIntrinsicId == NI_AdvSimd_DuplicateToVector128) ||
           (node->gtHWIntrinsicId == NI_AdvSimd_Arm64_DuplicateToVector64) ||
           (node->gtHWIntrinsicId == NI_AdvSimd_Arm64_DuplicateToVector128));
    assert(node->IsUnary());

    GenTree* op1    = node->GetOp(0);
    GenTree* castOp = nullptr;

    if (varTypeIsIntegral(node->gtSIMDBaseType) && op1->OperIs(GT_CAST))
    {
        // We will sometimes get a cast around a constant value (such as for
        // certain long constants) which would block the below containment.
        // So we will temporarily check what the cast is from instead so we
        // can catch those cases as well.

        castOp = op1->AsCast()->CastOp();
        op1    = castOp;
    }

    if (op1->IsCnsIntOrI())
    {
        const ssize_t dataValue = op1->AsIntCon()->gtIconVal;

        if (comp->GetEmitter()->emitIns_valid_imm_for_movi(dataValue, emitActualTypeSize(node->gtSIMDBaseType)))
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
    else if (op1->IsCnsFltOrDbl())
    {
        assert(varTypeIsFloating(node->gtSIMDBaseType));
        assert(castOp == nullptr);

        const double dataValue = op1->AsDblCon()->gtDconVal;
        return comp->GetEmitter()->emitIns_valid_imm_for_fmov(dataValue);
    }

    return false;
}

//----------------------------------------------------------------------------------------------
// Lowering::LowerHWIntrinsicCmpOp: Lowers a Vector128 or Vector256 comparison intrinsic
//
//  Arguments:
//     node  - The hardware intrinsic node.
//     cmpOp - The comparison operation, currently must be GT_EQ or GT_NE
//
void Lowering::LowerHWIntrinsicCmpOp(GenTreeHWIntrinsic* node, genTreeOps cmpOp)
{
    NamedIntrinsic intrinsicId = node->gtHWIntrinsicId;
    var_types      baseType    = node->gtSIMDBaseType;
    unsigned       simdSize    = node->gtSIMDSize;
    var_types      simdType    = Compiler::getSIMDTypeForSize(simdSize);

    assert((intrinsicId == NI_Vector64_op_Equality) || (intrinsicId == NI_Vector64_op_Inequality) ||
           (intrinsicId == NI_Vector128_op_Equality) || (intrinsicId == NI_Vector128_op_Inequality));

    assert(varTypeIsSIMD(simdType));
    assert(varTypeIsArithmetic(baseType));
    assert(simdSize != 0);
    assert(node->gtType == TYP_BOOL);
    assert((cmpOp == GT_EQ) || (cmpOp == GT_NE));

    // We have the following (with the appropriate simd size and where the intrinsic could be op_Inequality):
    //          /--*  op2  simd
    //          /--*  op1  simd
    //   node = *  HWINTRINSIC   simd   T op_Equality

    GenTree* op1 = node->GetOp(0);
    GenTree* op2 = node->GetOp(1);

    NamedIntrinsic cmpIntrinsic;

    switch (baseType)
    {
        case TYP_BYTE:
        case TYP_UBYTE:
        case TYP_SHORT:
        case TYP_USHORT:
        case TYP_INT:
        case TYP_UINT:
        case TYP_FLOAT:
        {
            cmpIntrinsic = NI_AdvSimd_CompareEqual;
            break;
        }

        case TYP_LONG:
        case TYP_ULONG:
        case TYP_DOUBLE:
        {
            cmpIntrinsic = NI_AdvSimd_Arm64_CompareEqual;
            break;
        }

        default:
        {
            unreached();
        }
    }

    GenTree* cmp = comp->gtNewSimdHWIntrinsicNode(simdType, cmpIntrinsic, baseType, simdSize, op1, op2);
    BlockRange().InsertBefore(node, cmp);
    LowerNode(cmp);

    if ((baseType == TYP_FLOAT) && (simdSize == 12))
    {
        // For TYP_SIMD12 we don't want the upper bits to participate in the comparison. So, we will insert all ones
        // into those bits of the result, "as if" the upper bits are equal. Then if all lower bits are equal, we get the
        // expected all-ones result, and will get the expected 0's only where there are non-matching bits.

        GenTree* idxCns = comp->gtNewIconNode(3, TYP_INT);
        BlockRange().InsertAfter(cmp, idxCns);

        GenTree* insCns = comp->gtNewIconNode(-1, TYP_INT);
        BlockRange().InsertAfter(idxCns, insCns);

        GenTree* tmp =
            comp->gtNewSimdAsHWIntrinsicNode(simdType, NI_AdvSimd_Insert, TYP_INT, simdSize, cmp, idxCns, insCns);
        BlockRange().InsertAfter(insCns, tmp);
        LowerNode(tmp);

        cmp = tmp;
    }

    GenTree* msk = comp->gtNewSimdHWIntrinsicNode(simdType, NI_AdvSimd_Arm64_MinAcross, TYP_UBYTE, simdSize, cmp);
    BlockRange().InsertAfter(cmp, msk);
    LowerNode(msk);

    GenTree* zroCns = comp->gtNewIconNode(0, TYP_INT);
    BlockRange().InsertAfter(msk, zroCns);

    GenTree* val = comp->gtNewSimdAsHWIntrinsicNode(TYP_UBYTE, NI_AdvSimd_Extract, TYP_UBYTE, simdSize, msk, zroCns);
    BlockRange().InsertAfter(zroCns, val);
    LowerNode(val);

    zroCns = comp->gtNewIconNode(0, TYP_INT);
    BlockRange().InsertAfter(val, zroCns);

    node->ChangeOper(cmpOp);

    GenTreeOp* relop = static_cast<GenTree*>(node)->AsOp();
    relop->SetType(TYP_INT);
    relop->SetOp(0, val);
    relop->SetOp(1, zroCns);

    // The CompareEqual will set (condition is true) or clear (condition is false) all bits of the respective element
    // The MinAcross then ensures we get either all bits set (all conditions are true) or clear (any condition is false)
    // So, we need to invert the condition from the operation since we compare against zero

    GenCondition cmpCnd = (cmpOp == GT_EQ) ? GenCondition::NE : GenCondition::EQ;
    GenTree*     cc     = LowerNodeCC(relop, cmpCnd);

    relop->SetType(TYP_VOID);
    relop->ClearUnusedValue();

    LowerNode(relop);
}

//----------------------------------------------------------------------------------------------
// Lowering::LowerHWIntrinsicCreate: Lowers a Vector64 or Vector128 Create call
//
//  Arguments:
//     node - The hardware intrinsic node.
//
void Lowering::LowerHWIntrinsicCreate(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsicId = node->gtHWIntrinsicId;
    var_types      simdType    = node->gtType;
    var_types      baseType    = node->gtSIMDBaseType;
    unsigned       simdSize    = node->gtSIMDSize;
    VectorConstant vecCns      = {};

    if ((simdSize == 8) && (simdType == TYP_DOUBLE))
    {
        // TODO-Cleanup: Struct retyping means we have the wrong type here. We need to
        //               manually fix it up so the simdType checks below are correct.
        simdType = TYP_SIMD8;
    }

    assert(varTypeIsSIMD(simdType));
    assert(varTypeIsArithmetic(baseType));
    assert(simdSize != 0);

    // Spare GenTrees to be used for the lowering logic below
    // Defined upfront to avoid naming conflicts, etc...
    GenTree* idx  = nullptr;
    GenTree* tmp1 = nullptr;
    GenTree* tmp2 = nullptr;
    GenTree* tmp3 = nullptr;

    unsigned argCnt    = node->GetNumOps();
    unsigned cnsArgCnt = 0;

    assert((argCnt == 1) || (argCnt == (simdSize / varTypeSize(baseType))));

    if (argCnt == 1)
    {
        for (unsigned i = 0; i < simdSize / varTypeSize(baseType); i++)
        {
            if (HandleArgForHWIntrinsicCreate(node->GetOp(0), i, vecCns, baseType))
            {
                cnsArgCnt = 1;
            }
        }
    }
    else
    {
        assert(argCnt == (simdSize / varTypeSize(baseType)));

        for (unsigned i = 0; i < argCnt; i++)
        {
            if (HandleArgForHWIntrinsicCreate(node->GetOp(i), i, vecCns, baseType))
            {
                cnsArgCnt += 1;
            }
        }
    }

    if ((argCnt == cnsArgCnt) && (argCnt == 1))
    {
        if (IsValidConstForMovImm(node))
        {
            // Set the cnsArgCnt to zero so we get lowered to a DuplicateToVector
            // intrinsic, which will itself mark the node as contained.
            cnsArgCnt = 0;
        }
    }

    if (argCnt == cnsArgCnt)
    {
        for (unsigned i = 0; i < argCnt; i++)
        {
            BlockRange().Remove(node->GetOp(i));
        }

        assert((simdSize == 8) || (simdSize == 16));

        if ((argCnt == 1) || VectorConstantIsBroadcastedI64(vecCns, simdSize / 8))
        {
            // If we are a single constant or if all parts are the same, we might be able to optimize
            // this even further for certain values, such as Zero or AllBitsSet.

            if (vecCns.i64[0] == 0)
            {
                node->SetIntrinsic((simdSize == 8) ? NI_Vector64_get_Zero : NI_Vector128_get_Zero);
                node->SetNumOps(0);
                return;
            }
            else if (vecCns.i64[0] == -1)
            {
                node->SetIntrinsic((simdSize == 8) ? NI_Vector64_get_AllBitsSet : NI_Vector128_get_AllBitsSet);
                node->SetNumOps(0);
                return;
            }
        }

        unsigned  cnsSize  = (simdSize == 12) ? 16 : simdSize;
        unsigned  cnsAlign = cnsSize;
        var_types dataType = Compiler::getSIMDTypeForSize(simdSize);

        UNATIVE_OFFSET       cnum       = comp->GetEmitter()->emitDataConst(&vecCns, cnsSize, cnsAlign, dataType);
        CORINFO_FIELD_HANDLE hnd        = comp->eeFindJitDataOffs(cnum);
        GenTree*             clsVarAddr = new (comp, GT_CLS_VAR_ADDR) GenTreeClsVar(GT_CLS_VAR_ADDR, TYP_I_IMPL, hnd);
        BlockRange().InsertBefore(node, clsVarAddr);

        GenTree* indir = node;
        indir->ChangeOper(GT_IND);
        indir->AsIndir()->SetAddr(clsVarAddr);

        // TODO-ARM64-CQ: We should be able to modify at least the paths that use Insert to trivially support partial
        // vector constants. With this, we can create a constant if say 50% of the inputs are also constant and just
        // insert the non-constant values which should still allow some gains.

        return;
    }

    if (argCnt == 1)
    {
        // We have the following (where simd is simd8 or simd16):
        //          /--*  op1  T
        //   node = *  HWINTRINSIC   simd   T Create

        // We will be constructing the following parts:
        //           /--*  op1  T
        //   node  = *  HWINTRINSIC   simd   T DuplicateToVector

        // This is roughly the following managed code:
        //   return AdvSimd.Arm64.DuplicateToVector(op1);

        if (varTypeIsLong(baseType) || (baseType == TYP_DOUBLE))
        {
            node->gtHWIntrinsicId =
                (simdType == TYP_SIMD8) ? NI_AdvSimd_Arm64_DuplicateToVector64 : NI_AdvSimd_Arm64_DuplicateToVector128;
        }
        else
        {
            node->gtHWIntrinsicId =
                (simdType == TYP_SIMD8) ? NI_AdvSimd_DuplicateToVector64 : NI_AdvSimd_DuplicateToVector128;
        }
        return;
    }

    // We have the following (where simd is simd8 or simd16):
    //          /--*  op1 T
    //          +--*  ... T
    //          +--*  opN T
    //   node = *  HWINTRINSIC   simd   T Create

    // We will be constructing the following parts:
    //          /--*  op1  T
    //   tmp1 = *  HWINTRINSIC   simd8  T CreateScalarUnsafe
    //   ...

    // This is roughly the following managed code:
    //   var tmp1 = Vector64.CreateScalarUnsafe(op1);
    //   ...

    GenTree* op1 = node->GetOp(0);

    NamedIntrinsic createScalarUnsafe =
        (simdType == TYP_SIMD8) ? NI_Vector64_CreateScalarUnsafe : NI_Vector128_CreateScalarUnsafe;

    tmp1 = comp->gtNewSimdHWIntrinsicNode(simdType, createScalarUnsafe, baseType, simdSize, op1);
    BlockRange().InsertAfter(op1, tmp1);
    LowerNode(tmp1);

    unsigned N   = 0;
    GenTree* opN = nullptr;

    for (N = 1; N < argCnt - 1; N++)
    {
        // We will be constructing the following parts:
        //   ...
        //   idx  =    CNS_INT       int    N
        //          /--*  tmp1 simd
        //          +--*  idx  int
        //          +--*  opN  T
        //   tmp1 = *  HWINTRINSIC   simd   T Insert
        //   ...

        // This is roughly the following managed code:
        //   ...
        //   tmp1 = AdvSimd.Insert(tmp1, N, opN);
        //   ...

        opN = node->GetOp(N);

        idx = comp->gtNewIconNode(N, TYP_INT);
        BlockRange().InsertBefore(opN, idx);

        tmp1 = comp->gtNewSimdHWIntrinsicNode(simdType, NI_AdvSimd_Insert, baseType, simdSize, tmp1, idx, opN);
        BlockRange().InsertAfter(opN, tmp1);
        LowerNode(tmp1);
    }

    assert(N == (argCnt - 1));

    // We will be constructing the following parts:
    //   idx  =    CNS_INT       int    N
    //          /--*  tmp1 simd
    //          +--*  idx  int
    //          +--*  opN  T
    //   node = *  HWINTRINSIC   simd   T Insert

    // This is roughly the following managed code:
    //   ...
    //   tmp1 = AdvSimd.Insert(tmp1, N, opN);
    //   ...

    opN = node->GetOp(N);

    idx = comp->gtNewIconNode(N, TYP_INT);
    BlockRange().InsertBefore(opN, idx);

    node->SetIntrinsic(NI_AdvSimd_Insert, 3);
    node->SetOp(0, tmp1);
    node->SetOp(1, idx);
    node->SetOp(2, opN);
}

//----------------------------------------------------------------------------------------------
// Lowering::LowerHWIntrinsicDot: Lowers a Vector64 or Vector128 Dot call
//
//  Arguments:
//     node - The hardware intrinsic node.
//
void Lowering::LowerHWIntrinsicDot(GenTreeHWIntrinsic* node)
{
    NamedIntrinsic intrinsicId = node->gtHWIntrinsicId;
    var_types      baseType    = node->gtSIMDBaseType;
    unsigned       simdSize    = node->gtSIMDSize;
    var_types      simdType    = Compiler::getSIMDTypeForSize(simdSize);

    assert((intrinsicId == NI_Vector64_Dot) || (intrinsicId == NI_Vector128_Dot));
    assert(varTypeIsSIMD(simdType));
    assert(varTypeIsArithmetic(baseType));
    assert(simdSize != 0);

    GenTree* op1 = node->GetOp(0);
    GenTree* op2 = node->GetOp(1);

    // Spare GenTrees to be used for the lowering logic below
    // Defined upfront to avoid naming conflicts, etc...
    GenTree* idx  = nullptr;
    GenTree* tmp1 = nullptr;
    GenTree* tmp2 = nullptr;

    if (simdSize == 12)
    {
        assert(baseType == TYP_FLOAT);

        // For 12 byte SIMD, we need to clear the upper 4 bytes:
        //   idx  =    CNS_INT       int    0x03
        //   tmp1 = *  CNS_DLB       float  0.0
        //          /--*  op1  simd16
        //          +--*  idx  int
        //          +--*  tmp1 simd16
        //   op1  = *  HWINTRINSIC   simd16 T Insert
        //   ...

        // This is roughly the following managed code:
        //    op1 = AdvSimd.Insert(op1, 0x03, 0.0f);
        //    ...

        idx = comp->gtNewIconNode(0x03, TYP_INT);
        BlockRange().InsertAfter(op1, idx);

        tmp1 = comp->gtNewZeroConNode(TYP_FLOAT);
        BlockRange().InsertAfter(idx, tmp1);
        LowerNode(tmp1);

        op1 = comp->gtNewSimdAsHWIntrinsicNode(simdType, NI_AdvSimd_Insert, baseType, simdSize, op1, idx, tmp1);
        BlockRange().InsertAfter(tmp1, op1);
        LowerNode(op1);
    }

    // We will be constructing the following parts:
    //   ...
    //          /--*  op1  simd16
    //          +--*  op2  simd16
    //   tmp1 = *  HWINTRINSIC   simd16 T Multiply
    //   ...

    // This is roughly the following managed code:
    //   ...
    //   var tmp1 = AdvSimd.Multiply(op1, op2);
    //   ...

    NamedIntrinsic multiply = (baseType == TYP_DOUBLE) ? NI_AdvSimd_Arm64_Multiply : NI_AdvSimd_Multiply;
    assert(!varTypeIsLong(baseType));

    tmp1 = comp->gtNewSimdAsHWIntrinsicNode(simdType, multiply, baseType, simdSize, op1, op2);
    BlockRange().InsertBefore(node, tmp1);
    LowerNode(tmp1);

    if (varTypeIsFloating(baseType))
    {
        // We will be constructing the following parts:
        //   ...
        //          /--*  tmp1 simd16
        //          *  STORE_LCL_VAR simd16
        //   tmp1 =    LCL_VAR       simd16
        //   tmp2 =    LCL_VAR       simd16
        //   ...

        // This is roughly the following managed code:
        //   ...
        //   var tmp2 = tmp1;
        //   ...

        node->SetOp(0, tmp1);
        LIR::Use tmp1Use(BlockRange(), &node->GetUse(0).NodeRef(), node);
        ReplaceWithLclVar(tmp1Use);
        tmp1 = node->GetOp(0);

        tmp2 = comp->gtClone(tmp1);
        BlockRange().InsertAfter(tmp1, tmp2);

        if (simdSize == 8)
        {
            assert(baseType == TYP_FLOAT);

            // We will be constructing the following parts:
            //   ...
            //          /--*  tmp1 simd8
            //          +--*  tmp2 simd8
            //   tmp1 = *  HWINTRINSIC   simd8  T AddPairwise
            //   ...

            // This is roughly the following managed code:
            //   ...
            //   var tmp1 = AdvSimd.AddPairwise(tmp1, tmp2);
            //   ...

            tmp1 = comp->gtNewSimdAsHWIntrinsicNode(simdType, NI_AdvSimd_AddPairwise, baseType, simdSize, tmp1, tmp2);
            BlockRange().InsertAfter(tmp2, tmp1);
            LowerNode(tmp1);
        }
        else
        {
            assert((simdSize == 12) || (simdSize == 16));

            // We will be constructing the following parts:
            //   ...
            //          /--*  tmp1 simd16
            //          +--*  tmp2 simd16
            //   tmp2 = *  HWINTRINSIC   simd16 T AddPairwise
            //   ...

            // This is roughly the following managed code:
            //   ...
            //   var tmp1 = AdvSimd.Arm64.AddPairwise(tmp1, tmp2);
            //   ...

            tmp1 = comp->gtNewSimdAsHWIntrinsicNode(simdType, NI_AdvSimd_Arm64_AddPairwise, baseType, simdSize, tmp1,
                                                    tmp2);
            BlockRange().InsertAfter(tmp2, tmp1);
            LowerNode(tmp1);

            if (baseType == TYP_FLOAT)
            {
                // Float needs an additional pairwise add to finish summing the parts
                // The first will have summed e0 with e1 and e2 with e3 and then repeats that for the upper half
                // So, we will have a vector that looks like this:
                //    < e0 + e1, e2 + e3, e0 + e1, e2 + e3>
                // Doing a second horizontal add with itself will then give us
                //    e0 + e1 + e2 + e3 in all elements of the vector

                // We will be constructing the following parts:
                //   ...
                //          /--*  tmp1 simd16
                //          *  STORE_LCL_VAR simd16
                //   tmp1 =    LCL_VAR       simd16
                //   tmp2 =    LCL_VAR       simd16
                //          /--*  tmp1 simd16
                //          +--*  tmp2 simd16
                //   tmp2 = *  HWINTRINSIC   simd16 T AddPairwise
                //   ...

                // This is roughly the following managed code:
                //   ...
                //   var tmp2 = tmp1;
                //   var tmp1 = AdvSimd.Arm64.AddPairwise(tmp1, tmp2);
                //   ...

                node->SetOp(0, tmp1);
                LIR::Use tmp1Use(BlockRange(), &node->GetUse(0).NodeRef(), node);
                ReplaceWithLclVar(tmp1Use);
                tmp1 = node->GetOp(0);

                tmp2 = comp->gtClone(tmp1);
                BlockRange().InsertAfter(tmp1, tmp2);

                tmp1 = comp->gtNewSimdAsHWIntrinsicNode(simdType, NI_AdvSimd_Arm64_AddPairwise, baseType, simdSize,
                                                        tmp1, tmp2);
                BlockRange().InsertAfter(tmp2, tmp1);
                LowerNode(tmp1);
            }
        }

        tmp2 = tmp1;
    }
    else
    {
        assert(varTypeIsIntegral(baseType));

        // We will be constructing the following parts:
        //   ...
        //          /--*  tmp1 simd16
        //   tmp2 = *  HWINTRINSIC   simd16 T AddAcross
        //   ...

        // This is roughly the following managed code:
        //   ...
        //   var tmp2 = AdvSimd.Arm64.AddAcross(tmp1);
        //   ...

        tmp2 = comp->gtNewSimdAsHWIntrinsicNode(simdType, NI_AdvSimd_Arm64_AddAcross, baseType, simdSize, tmp1);
        BlockRange().InsertAfter(tmp1, tmp2);
        LowerNode(tmp2);
    }

    // We will be constructing the following parts:
    //   ...
    //          /--*  tmp2 simd16
    //   node = *  HWINTRINSIC   simd16 T ToScalar

    // This is roughly the following managed code:
    //   ...
    //   return tmp2.ToScalar();

    node->SetIntrinsic((simdSize == 8) ? NI_Vector64_ToScalar : NI_Vector128_ToScalar);
    node->SetNumOps(1);
    node->SetOp(0, tmp2);

    LowerNode(node);

    return;
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
        if (value->IsSIMDZero() || value->IsHWIntrinsicZero())
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
#ifdef TARGET_ARM64
    else if (addr->OperIs(GT_CLS_VAR_ADDR, GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR))
    {
        // These nodes go into an addr mode:
        // - GT_CLS_VAR_ADDR turns into a constant.
        // - GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR is a stack addr mode.

        // make this contained, it turns into a constant that goes into an addr mode
        MakeSrcContained(indirNode, addr);
    }
#endif // TARGET_ARM64
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
    if (src->IsIntegralConst(0) || src->IsDblConPositiveZero() || src->IsSIMDZero() || src->IsHWIntrinsicZero())
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
    assert(node->OperIsBoundsCheck());
    if (!CheckImmedAndMakeContained(node, node->gtIndex))
    {
        CheckImmedAndMakeContained(node, node->gtArrLen);
    }
}

#ifdef FEATURE_SIMD
//----------------------------------------------------------------------------------------------
// ContainCheckSIMD: Perform containment analysis for a SIMD intrinsic node.
//
//  Arguments:
//     simdNode - The SIMD intrinsic node.
//
void Lowering::ContainCheckSIMD(GenTreeSIMD* simdNode)
{
    switch (simdNode->gtSIMDIntrinsicID)
    {
        GenTree* op1;
        GenTree* op2;

        case SIMDIntrinsicInit:
            op1 = simdNode->GetOp(0);
            if (op1->IsIntegralConst(0) || op1->IsDblConPositiveZero())
            {
                MakeSrcContained(simdNode, op1);
            }
            break;

        case SIMDIntrinsicGetItem:
        {
            // This implements get_Item method. The sources are:
            //  - the source SIMD struct
            //  - index (which element to get)
            // The result is baseType of SIMD struct.
            op1 = simdNode->GetOp(0);
            op2 = simdNode->GetOp(1);

            // If the index is a constant, mark it as contained.
            if (op2->IsCnsIntOrI())
            {
                MakeSrcContained(simdNode, op2);
            }

            if (IsContainableMemoryOp(op1))
            {
                MakeSrcContained(simdNode, op1);
                if (op1->OperGet() == GT_IND)
                {
                    op1->AsIndir()->Addr()->ClearContained();
                }
            }
            break;
        }

        default:
            break;
    }
}
#endif // FEATURE_SIMD

#ifdef FEATURE_HW_INTRINSICS

//----------------------------------------------------------------------------------------------
// ContainCheckHWIntrinsic: Perform containment analysis for a hardware intrinsic node.
//
//  Arguments:
//     node - The hardware intrinsic node.
//
void Lowering::ContainCheckHWIntrinsic(GenTreeHWIntrinsic* node)
{
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
            case NI_Vector64_GetElement:
            case NI_Vector128_GetElement:
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

                    if ((intrin.op2->AsIntCon()->gtIconVal == 0) && intrin.op3->IsCnsFltOrDbl())
                    {
                        assert(varTypeIsFloating(intrin.baseType));

                        const double dataValue = intrin.op3->AsDblCon()->gtDconVal;

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
