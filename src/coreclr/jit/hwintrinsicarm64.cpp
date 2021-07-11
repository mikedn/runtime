// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "hwintrinsic.h"

#ifdef FEATURE_HW_INTRINSICS

//------------------------------------------------------------------------
// Arm64VersionOfIsa: Gets the corresponding 64-bit only InstructionSet for a given InstructionSet
//
// Arguments:
//    isa -- The InstructionSet ID
//
// Return Value:
//    The 64-bit only InstructionSet associated with isa
static CORINFO_InstructionSet Arm64VersionOfIsa(CORINFO_InstructionSet isa)
{
    switch (isa)
    {
        case InstructionSet_AdvSimd:
            return InstructionSet_AdvSimd_Arm64;
        case InstructionSet_Aes:
            return InstructionSet_Aes_Arm64;
        case InstructionSet_ArmBase:
            return InstructionSet_ArmBase_Arm64;
        case InstructionSet_Crc32:
            return InstructionSet_Crc32_Arm64;
        case InstructionSet_Dp:
            return InstructionSet_Dp_Arm64;
        case InstructionSet_Sha1:
            return InstructionSet_Sha1_Arm64;
        case InstructionSet_Sha256:
            return InstructionSet_Sha256_Arm64;
        case InstructionSet_Rdm:
            return InstructionSet_Rdm_Arm64;
        default:
            return InstructionSet_NONE;
    }
}

//------------------------------------------------------------------------
// lookupInstructionSet: Gets the InstructionSet for a given class name
//
// Arguments:
//    className -- The name of the class associated with the InstructionSet to lookup
//
// Return Value:
//    The InstructionSet associated with className
static CORINFO_InstructionSet lookupInstructionSet(const char* className)
{
    assert(className != nullptr);

    if (className[0] == 'A')
    {
        if (strcmp(className, "AdvSimd") == 0)
        {
            return InstructionSet_AdvSimd;
        }
        if (strcmp(className, "Aes") == 0)
        {
            return InstructionSet_Aes;
        }
        if (strcmp(className, "ArmBase") == 0)
        {
            return InstructionSet_ArmBase;
        }
    }
    else if (className[0] == 'C')
    {
        if (strcmp(className, "Crc32") == 0)
        {
            return InstructionSet_Crc32;
        }
    }
    else if (className[0] == 'D')
    {
        if (strcmp(className, "Dp") == 0)
        {
            return InstructionSet_Dp;
        }
    }
    else if (className[0] == 'R')
    {
        if (strcmp(className, "Rdm") == 0)
        {
            return InstructionSet_Rdm;
        }
    }
    else if (className[0] == 'S')
    {
        if (strcmp(className, "Sha1") == 0)
        {
            return InstructionSet_Sha1;
        }
        if (strcmp(className, "Sha256") == 0)
        {
            return InstructionSet_Sha256;
        }
    }
    else if (className[0] == 'V')
    {
        if (strncmp(className, "Vector64", 8) == 0)
        {
            return InstructionSet_Vector64;
        }
        else if (strncmp(className, "Vector128", 9) == 0)
        {
            return InstructionSet_Vector128;
        }
    }

    return InstructionSet_ILLEGAL;
}

//------------------------------------------------------------------------
// lookupIsa: Gets the InstructionSet for a given class name and enclsoing class name
//
// Arguments:
//    className -- The name of the class associated with the InstructionSet to lookup
//    enclosingClassName -- The name of the enclosing class or nullptr if one doesn't exist
//
// Return Value:
//    The InstructionSet associated with className and enclosingClassName
CORINFO_InstructionSet HWIntrinsicInfo::lookupIsa(const char* className, const char* enclosingClassName)
{
    assert(className != nullptr);

    if (strcmp(className, "Arm64") == 0)
    {
        assert(enclosingClassName != nullptr);
        return Arm64VersionOfIsa(lookupInstructionSet(enclosingClassName));
    }
    else
    {
        return lookupInstructionSet(className);
    }
}

//------------------------------------------------------------------------
// isFullyImplementedIsa: Gets a value that indicates whether the InstructionSet is fully implemented
//
// Arguments:
//    isa - The InstructionSet to check
//
// Return Value:
//    true if isa is supported; otherwise, false
bool HWIntrinsicInfo::isFullyImplementedIsa(CORINFO_InstructionSet isa)
{
    switch (isa)
    {
        // These ISAs are fully implemented
        case InstructionSet_AdvSimd:
        case InstructionSet_AdvSimd_Arm64:
        case InstructionSet_Aes:
        case InstructionSet_Aes_Arm64:
        case InstructionSet_ArmBase:
        case InstructionSet_ArmBase_Arm64:
        case InstructionSet_Crc32:
        case InstructionSet_Crc32_Arm64:
        case InstructionSet_Dp:
        case InstructionSet_Dp_Arm64:
        case InstructionSet_Rdm:
        case InstructionSet_Rdm_Arm64:
        case InstructionSet_Sha1:
        case InstructionSet_Sha1_Arm64:
        case InstructionSet_Sha256:
        case InstructionSet_Sha256_Arm64:
        case InstructionSet_Vector64:
        case InstructionSet_Vector128:
            return true;

        default:
            return false;
    }
}

//------------------------------------------------------------------------
// isScalarIsa: Gets a value that indicates whether the InstructionSet is scalar
//
// Arguments:
//    isa - The InstructionSet to check
//
// Return Value:
//    true if isa is scalar; otherwise, false
bool HWIntrinsicInfo::isScalarIsa(CORINFO_InstructionSet isa)
{
    switch (isa)
    {
        case InstructionSet_ArmBase:
        case InstructionSet_ArmBase_Arm64:
        case InstructionSet_Crc32:
        case InstructionSet_Crc32_Arm64:
        {
            return true;
        }

        default:
        {
            return false;
        }
    }
}

//------------------------------------------------------------------------
// lookupImmBounds: Gets the lower and upper bounds for the imm-value of a given NamedIntrinsic
//
// Arguments:
//    intrinsic -- NamedIntrinsic associated with the HWIntrinsic to lookup
//    simdType  -- vector size
//    baseType  -- base type of the Vector64/128<T>
//    pImmLowerBound [OUT] - The lower incl. bound for a value of the intrinsic immediate operand
//    pImmUpperBound [OUT] - The upper incl. bound for a value of the intrinsic immediate operand
//
void HWIntrinsicInfo::lookupImmBounds(
    NamedIntrinsic intrinsic, unsigned simdSize, var_types baseType, int* pImmLowerBound, int* pImmUpperBound)
{
    HWIntrinsicCategory category            = HWIntrinsicInfo::lookupCategory(intrinsic);
    bool                hasImmediateOperand = HasImmediateOperand(intrinsic);

    assert(hasImmediateOperand);

    assert(pImmLowerBound != nullptr);
    assert(pImmUpperBound != nullptr);

    int immLowerBound = 0;
    int immUpperBound = 0;

    if (category == HW_Category_ShiftLeftByImmediate)
    {
        // The left shift amount is in the range 0 to the element width in bits minus 1.
        immUpperBound = BITS_PER_BYTE * genTypeSize(baseType) - 1;
    }
    else if (category == HW_Category_ShiftRightByImmediate)
    {
        // The right shift amount, in the range 1 to the element width in bits.
        immLowerBound = 1;
        immUpperBound = BITS_PER_BYTE * genTypeSize(baseType);
    }
    else if (category == HW_Category_SIMDByIndexedElement)
    {
        immUpperBound = getSIMDVectorLength(simdSize, baseType) - 1;
    }
    else
    {
        switch (intrinsic)
        {
            case NI_AdvSimd_DuplicateSelectedScalarToVector64:
            case NI_AdvSimd_DuplicateSelectedScalarToVector128:
            case NI_AdvSimd_Extract:
            case NI_AdvSimd_ExtractVector128:
            case NI_AdvSimd_ExtractVector64:
            case NI_AdvSimd_Insert:
            case NI_AdvSimd_InsertScalar:
            case NI_AdvSimd_LoadAndInsertScalar:
            case NI_AdvSimd_StoreSelectedScalar:
            case NI_AdvSimd_Arm64_DuplicateSelectedScalarToVector128:
            case NI_AdvSimd_Arm64_InsertSelectedScalar:
                immUpperBound = getSIMDVectorLength(simdSize, baseType) - 1;
                break;

            default:
                unreached();
        }
    }

    assert(immLowerBound <= immUpperBound);

    *pImmLowerBound = immLowerBound;
    *pImmUpperBound = immUpperBound;
}

//------------------------------------------------------------------------
// impNonConstFallback: generate alternate code when the imm-arg is not a compile-time constant
//
// Arguments:
//    intrinsic  -- intrinsic ID
//    simdType   -- Vector type
//    baseType   -- base type of the Vector64/128<T>
//
// Return Value:
//     return the IR of semantic alternative on non-const imm-arg
//
GenTree* Compiler::impNonConstFallback(NamedIntrinsic intrinsic, var_types simdType, var_types baseType)
{
    return nullptr;
}

GenTree* Compiler::impSpecialIntrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig)
{
    assert(!sig.hasThisParam);

    if (!featureSIMD)
    {
        return nullptr;
    }

    switch (intrinsic)
    {
        var_types eltType;
        unsigned  simdSize;
        GenTree*  op1;
        GenTree*  op2;

        case NI_Vector64_As:
        case NI_Vector64_AsByte:
        case NI_Vector64_AsDouble:
        case NI_Vector64_AsInt16:
        case NI_Vector64_AsInt32:
        case NI_Vector64_AsInt64:
        case NI_Vector64_AsSByte:
        case NI_Vector64_AsSingle:
        case NI_Vector64_AsUInt16:
        case NI_Vector64_AsUInt32:
        case NI_Vector64_AsUInt64:
        case NI_Vector128_As:
        case NI_Vector128_AsByte:
        case NI_Vector128_AsDouble:
        case NI_Vector128_AsInt16:
        case NI_Vector128_AsInt32:
        case NI_Vector128_AsInt64:
        case NI_Vector128_AsSByte:
        case NI_Vector128_AsSingle:
        case NI_Vector128_AsUInt16:
        case NI_Vector128_AsUInt32:
        case NI_Vector128_AsUInt64:
        case NI_Vector128_AsVector:
        case NI_Vector128_AsVector4:
        case NI_Vector128_AsVector128:
            assert(sig.paramCount == 1);
            assert(sig.paramType[0] == sig.retType);
            assert((sig.retType == TYP_SIMD8) || (sig.retType == TYP_SIMD16));

            return impSIMDPopStack(sig.paramType[0]);

        case NI_Vector64_get_Zero:
        case NI_Vector64_get_AllBitsSet:
        case NI_Vector128_get_Zero:
        case NI_Vector128_get_AllBitsSet:
            assert(sig.paramCount == 0);
            assert((sig.retType == TYP_SIMD8) || (sig.retType == TYP_SIMD16));

            eltType  = sig.retLayout->GetElementType();
            simdSize = sig.retLayout->GetSize();

            return gtNewSimdHWIntrinsicNode(sig.retType, intrinsic, eltType, simdSize);

        case NI_Vector64_Create:
        case NI_Vector128_Create:
            assert((sig.paramCount >= 1) && (sig.paramCount <= 16));
            assert((sig.retType == TYP_SIMD8) || (sig.retType == TYP_SIMD16));

            if (!compExactlyDependsOn(InstructionSet_AdvSimd))
            {
                return nullptr;
            }

            eltType  = sig.retLayout->GetElementType();
            simdSize = sig.retLayout->GetSize();

            {
                GenTreeHWIntrinsic* create = gtNewSimdHWIntrinsicNode(sig.retType, intrinsic, eltType, simdSize);
                create->SetNumOps(sig.paramCount, getAllocator(CMK_ASTNode));

                for (unsigned i = 0; i < sig.paramCount; i++)
                {
                    GenTree* op = impPopStack().val;
                    create->SetOp(sig.paramCount - 1 - i, op);
                    create->AddSideEffects(op->GetSideEffects());
                }

                return create;
            }

        case NI_Vector64_WithElement:
        case NI_Vector128_WithElement:
        {
            assert(sig.paramCount == 3);
            assert((sig.retType == TYP_SIMD8) || (sig.retType == TYP_SIMD16));
            assert(sig.paramType[0] == sig.retType);
            assert(sig.paramLayout[0]->GetElementType() == sig.paramType[2]);
            assert(sig.paramType[1] == TYP_INT);

            GenTreeIntCon* idx = impStackTop(1).val->IsIntCon();

            if ((idx == nullptr) || (idx->GetUInt32Value() >= sig.paramLayout[0]->GetElementCount()))
            {
                return nullptr;
            }

            GenTree* elt = impPopStack().val;
            /* idx = */ impPopStack();
            GenTree* vec = impSIMDPopStack(sig.retType);

            return gtNewSimdWithElementNode(sig.retType, sig.paramType[2], vec, idx, elt);
        }

        case NI_Vector64_GetElement:
        case NI_Vector128_GetElement:
            assert(sig.paramCount == 2);
            assert(sig.paramLayout[0]->GetElementType() == sig.retType);

            if (!compExactlyDependsOn(InstructionSet_AdvSimd))
            {
                return nullptr;
            }

            op2 = impPopStackCoerceArg(TYP_INT);
            op1 = impSIMDPopStack(sig.paramType[0]);
            return impVectorGetElement(sig.paramLayout[0], op1, op2);

        case NI_Vector64_ToScalar:
        case NI_Vector128_ToScalar:
            assert(sig.paramCount == 1);

            if (!compExactlyDependsOn(InstructionSet_AdvSimd))
            {
                return nullptr;
            }

            op2 = gtNewIconNode(0);
            op1 = impSIMDPopStack(sig.paramType[0]);
            return gtNewSimdGetElementNode(sig.paramType[0], sig.retType, op1, op2);

        case NI_AdvSimd_Extract:
            eltType = sig.retType;

            assert(sig.paramCount == 2);
            assert(sig.paramLayout[0]->GetElementType() == eltType);
            assert(sig.paramType[1] == TYP_UBYTE);

            if (!compExactlyDependsOn(InstructionSet_AdvSimd))
            {
                return nullptr;
            }

            op2 = impPopStackCoerceArg(TYP_INT);
            op1 = impSIMDPopStack(sig.paramType[0]);

            if (op2->IsIntCon() && (op2->AsIntCon()->GetUInt8Value() < sig.paramLayout[0]->GetElementCount()))
            {
                return gtNewSimdGetElementNode(sig.paramType[0], eltType, op1, op2);
            }

            simdSize = sig.paramLayout[0]->GetSize();

            return gtNewSimdHWIntrinsicNode(varTypeNodeType(sig.retType), NI_AdvSimd_Extract, eltType, simdSize, op1,
                                            op2);

        case NI_Vector128_GetUpper:
            assert(sig.paramCount == 1);
            assert((sig.paramType[0] == TYP_SIMD16) && (sig.retType == TYP_SIMD8));

            eltType = sig.retLayout->GetElementType();

            op1 = impSIMDPopStack(TYP_SIMD16);
            op2 = gtNewSimdHWIntrinsicNode(TYP_SIMD8, NI_Vector128_get_Zero, eltType, 8);
            op1 = gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AdvSimd_ExtractVector128, eltType, 16, op1, op2,
                                           gtNewIconNode(8 / varTypeSize(eltType)));
            return gtNewSimdHWIntrinsicNode(TYP_SIMD8, NI_Vector128_GetLower, eltType, 16, op1);

        default:
            return nullptr;
    }
}

#endif // FEATURE_HW_INTRINSICS
