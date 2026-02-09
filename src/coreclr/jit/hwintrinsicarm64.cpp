// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "hwintrinsic.h"

#ifdef FEATURE_HW_INTRINSICS

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

CORINFO_InstructionSet HWIntrinsicInfo::lookupIsa(const char* className, const char* enclosingClassName)
{
    assert(className != nullptr);

    if (strcmp(className, "Arm64") == 0)
    {
        assert(enclosingClassName != nullptr);
        return Arm64VersionOfIsa(lookupInstructionSet(enclosingClassName));
    }

    return lookupInstructionSet(className);
}

void HWIntrinsicInfo::GetImmOpBounds(
    NamedIntrinsic intrinsic, unsigned vecSize, var_types eltType, int* lowerBound, int* upperBound)
{
    assert(HasImmediateOperand(intrinsic));
    assert(lowerBound != nullptr);
    assert(upperBound != nullptr);

    HWIntrinsicCategory category = HWIntrinsicInfo::GetCategory(intrinsic);

    if (category == HW_Category_ShiftLeftByImmediate)
    {
        *lowerBound = 0;
        *upperBound = varTypeBitSize(eltType) - 1;
    }
    else if (category == HW_Category_ShiftRightByImmediate)
    {
        *lowerBound = 1;
        *upperBound = varTypeBitSize(eltType);
    }
    else if (category == HW_Category_SIMDByIndexedElement)
    {
        *lowerBound = 0;
        *upperBound = varTypeVecLength(vecSize, eltType) - 1;
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
                *lowerBound = 0;
                *upperBound = varTypeVecLength(vecSize, eltType) - 1;
                break;
            default:
                unreached();
        }
    }

    assert(*lowerBound <= *upperBound);
}

GenTree* Importer::ImportSpecialIntrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig)
{
    assert(!sig.hasThisParam);

    switch (intrinsic)
    {
        var_types eltType;
        unsigned  vecSize;
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
            assert(varTypeIsTargetVec(sig.retType));

            return PopVec(sig.paramType[0]);

        case NI_Vector64_get_Zero:
        case NI_Vector128_get_Zero:
            assert(sig.paramCount == 0);
            assert(varTypeIsTargetVec(sig.retType));

            eltType = varTypeNodeType(sig.retLayout->GetElementType());
            return NewVecNode(sig.retType, NI_VEC_ZERO, eltType);

        case NI_Vector64_get_AllBitsSet:
        case NI_Vector128_get_AllBitsSet:
            assert(sig.paramCount == 0);
            assert(varTypeIsTargetVec(sig.retType));

            eltType = varTypeNodeType(sig.retLayout->GetElementType());
            return NewVecNode(sig.retType, NI_VEC_ONE_BITS, eltType);

        case NI_Vector64_Create:
        case NI_Vector128_Create:
            assert((sig.paramCount >= 1) && (sig.paramCount <= 16));
            assert(varTypeIsTargetVec(sig.retType));

            eltType = varTypeNodeType(sig.retLayout->GetElementType());

            {
                GenTreeHWIntrinsic* create = NewVecNode(sig.retType, NI_VEC_PACK, eltType);
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
            assert(varTypeIsTargetVec(sig.retType));
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
            GenTree* vec = PopVec(sig.retType);

            return comp->gtNewVecInsertNode(sig.paramType[2], vec, idx, elt);
        }

        case NI_Vector64_GetElement:
        case NI_Vector128_GetElement:
            assert(sig.paramCount == 2);
            assert(sig.paramLayout[0]->GetElementType() == sig.retType);

            op2 = impPopStackCoerceArg(TYP_INT);
            op1 = PopVec(sig.paramType[0]);
            return impVecExtract(sig.paramLayout[0], op1, op2);

        case NI_Vector64_ToScalar:
        case NI_Vector128_ToScalar:
            assert(sig.paramCount == 1);

            op2 = comp->gtNewIconNode(0);
            op1 = PopVec(sig.paramType[0]);
            return comp->gtNewVecExtractNode(sig.retType, op1, op2);

        case NI_AdvSimd_Extract:
            eltType = sig.retType;

            assert(sig.paramCount == 2);
            assert(sig.paramLayout[0]->GetElementType() == eltType);
            assert(sig.paramType[1] == TYP_UBYTE);

            op2 = impPopStackCoerceArg(TYP_INT);
            op1 = PopVec(sig.paramType[0]);

            if (op2->IsIntCon() && (op2->AsIntCon()->GetUInt8Value() < sig.paramLayout[0]->GetElementCount()))
            {
                return NewVecExtractNode(eltType, op1, op2);
            }

            vecSize = sig.paramLayout[0]->GetSize();

            return NewVecNode(varTypeNodeType(sig.retType), NI_AdvSimd_Extract, eltType, vecSize, op1, op2);

        case NI_Vector128_GetUpper:
            assert(sig.paramCount == 1);
            assert((sig.paramType[0] == TYP_SIMD16) && (sig.retType == TYP_SIMD8));

            eltType = varTypeNodeType(sig.retLayout->GetElementType());

            op1 = PopVec(TYP_SIMD16);
            op2 = NewVecNode(TYP_SIMD8, NI_VEC_ZERO, eltType);
            op1 = NewVecNode(TYP_SIMD16, NI_AdvSimd_ExtractVector128, eltType, op1, op2,
                             comp->gtNewIconNode(8 / varTypeSize(eltType)));
            return NewVecNode(TYP_SIMD8, NI_Vector128_GetLower, eltType, 16, op1);

        case NI_ArmBase_Arm64_MultiplyHigh:
            assert(sig.paramCount == 2);
            assert((sig.retType == TYP_LONG) || (sig.retType == TYP_ULONG));
            assert(sig.retType == sig.paramType[0]);
            assert(sig.retType == sig.paramType[1]);

            op2 = impPopStack().val;
            op1 = impPopStack().val;
            return comp->gtNewOperNode(sig.retType == TYP_LONG ? GT_SMULH : GT_UMULH, TYP_LONG, op1, op2);

        default:
            return nullptr;
    }
}

#endif // FEATURE_HW_INTRINSICS
