// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "hwintrinsic.h"

#ifdef FEATURE_HW_INTRINSICS

static CORINFO_InstructionSet X64VersionOfIsa(CORINFO_InstructionSet isa)
{
    switch (isa)
    {
        case InstructionSet_X86Base:
            return InstructionSet_X86Base_X64;
        case InstructionSet_SSE:
            return InstructionSet_SSE_X64;
        case InstructionSet_SSE2:
            return InstructionSet_SSE2_X64;
        case InstructionSet_SSE3:
            return InstructionSet_SSE3_X64;
        case InstructionSet_SSSE3:
            return InstructionSet_SSSE3_X64;
        case InstructionSet_SSE41:
            return InstructionSet_SSE41_X64;
        case InstructionSet_SSE42:
            return InstructionSet_SSE42_X64;
        case InstructionSet_AVX:
            return InstructionSet_AVX_X64;
        case InstructionSet_AVX2:
            return InstructionSet_AVX2_X64;
        case InstructionSet_AVXVNNI:
            return InstructionSet_AVXVNNI_X64;
        case InstructionSet_AES:
            return InstructionSet_AES_X64;
        case InstructionSet_BMI1:
            return InstructionSet_BMI1_X64;
        case InstructionSet_BMI2:
            return InstructionSet_BMI2_X64;
        case InstructionSet_FMA:
            return InstructionSet_FMA_X64;
        case InstructionSet_LZCNT:
            return InstructionSet_LZCNT_X64;
        case InstructionSet_PCLMULQDQ:
            return InstructionSet_PCLMULQDQ_X64;
        case InstructionSet_POPCNT:
            return InstructionSet_POPCNT_X64;
        default:
            return InstructionSet_NONE;
    }
}

static CORINFO_InstructionSet lookupInstructionSet(const char* className)
{
    assert(className != nullptr);

    if (className[0] == 'A')
    {
        if (strcmp(className, "Aes") == 0)
        {
            return InstructionSet_AES;
        }
        if (strcmp(className, "Avx") == 0)
        {
            return InstructionSet_AVX;
        }
        if (strcmp(className, "Avx2") == 0)
        {
            return InstructionSet_AVX2;
        }
        if (strcmp(className, "AvxVnni") == 0)
        {
            return InstructionSet_AVXVNNI;
        }
    }
    else if (className[0] == 'S')
    {
        if (strcmp(className, "Sse") == 0)
        {
            return InstructionSet_SSE;
        }
        if (strcmp(className, "Sse2") == 0)
        {
            return InstructionSet_SSE2;
        }
        if (strcmp(className, "Sse3") == 0)
        {
            return InstructionSet_SSE3;
        }
        if (strcmp(className, "Ssse3") == 0)
        {
            return InstructionSet_SSSE3;
        }
        if (strcmp(className, "Sse41") == 0)
        {
            return InstructionSet_SSE41;
        }
        if (strcmp(className, "Sse42") == 0)
        {
            return InstructionSet_SSE42;
        }
    }
    else if (className[0] == 'B')
    {
        if (strcmp(className, "Bmi1") == 0)
        {
            return InstructionSet_BMI1;
        }
        if (strcmp(className, "Bmi2") == 0)
        {
            return InstructionSet_BMI2;
        }
    }
    else if (className[0] == 'P')
    {
        if (strcmp(className, "Pclmulqdq") == 0)
        {
            return InstructionSet_PCLMULQDQ;
        }
        if (strcmp(className, "Popcnt") == 0)
        {
            return InstructionSet_POPCNT;
        }
    }
    else if (className[0] == 'V')
    {
        if (strncmp(className, "Vector128", 9) == 0)
        {
            return InstructionSet_Vector128;
        }
        else if (strncmp(className, "Vector256", 9) == 0)
        {
            return InstructionSet_Vector256;
        }
    }
    else if (strcmp(className, "Fma") == 0)
    {
        return InstructionSet_FMA;
    }
    else if (strcmp(className, "Lzcnt") == 0)
    {
        return InstructionSet_LZCNT;
    }
    else if (strcmp(className, "X86Base") == 0)
    {
        return InstructionSet_X86Base;
    }

    return InstructionSet_ILLEGAL;
}

CORINFO_InstructionSet HWIntrinsicInfo::lookupIsa(const char* className, const char* enclosingClassName)
{
    assert(className != nullptr);

    if (strcmp(className, "X64") == 0)
    {
        assert(enclosingClassName != nullptr);
        return X64VersionOfIsa(lookupInstructionSet(enclosingClassName));
    }

    return lookupInstructionSet(className);
}

int HWIntrinsicInfo::lookupImmUpperBound(NamedIntrinsic id)
{
    assert(HWIntrinsicInfo::GetCategory(id) == HW_Category_IMM);

    switch (id)
    {
        case NI_AVX_Compare:
        case NI_AVX_CompareScalar:
            assert(!HWIntrinsicInfo::HasFullRangeImm(id));
            return 31; // enum FloatComparisonMode has 32 values
        case NI_AVX2_GatherVector128:
        case NI_AVX2_GatherVector256:
        case NI_AVX2_GatherMaskVector128:
        case NI_AVX2_GatherMaskVector256:
        case NI_AVX2_GATHERD:
        case NI_AVX2_GATHERQ:
            return 8;
        default:
            assert(HWIntrinsicInfo::HasFullRangeImm(id));
            return 255;
    }
}

bool HWIntrinsicInfo::isAVX2GatherIntrinsic(NamedIntrinsic id)
{
    switch (id)
    {
        case NI_AVX2_GatherVector128:
        case NI_AVX2_GatherVector256:
        case NI_AVX2_GatherMaskVector128:
        case NI_AVX2_GatherMaskVector256:
        case NI_AVX2_GATHERD:
        case NI_AVX2_GATHERQ:
            return true;
        default:
            return false;
    }
}

int HWIntrinsicInfo::GetImplicitImm(NamedIntrinsic id, bool opportunisticallyDependsOnAVX)
{
    switch (id)
    {
        case NI_SSE_CompareEqual:
        case NI_SSE_CompareScalarEqual:
        case NI_SSE2_CompareEqual:
        case NI_SSE2_CompareScalarEqual:
        case NI_AVX_CompareEqual:
            return static_cast<int>(FloatComparisonMode::OrderedEqualNonSignaling);
        case NI_SSE_CompareGreaterThan:
        case NI_SSE_CompareScalarGreaterThan:
        case NI_SSE2_CompareGreaterThan:
        case NI_SSE2_CompareScalarGreaterThan:
        case NI_AVX_CompareGreaterThan:
            if (opportunisticallyDependsOnAVX)
            {
                return static_cast<int>(FloatComparisonMode::OrderedGreaterThanSignaling);
            }

            // CompareGreaterThan is not directly supported in hardware without AVX support.
            // We will return the inverted case here and lowering will itself swap the ops
            // to ensure the emitted code remains correct. This simplifies the overall logic
            // here and for other use cases.

            assert(id != NI_AVX_CompareGreaterThan);
            return static_cast<int>(FloatComparisonMode::OrderedLessThanSignaling);
        case NI_SSE_CompareLessThan:
        case NI_SSE_CompareScalarLessThan:
        case NI_SSE2_CompareLessThan:
        case NI_SSE2_CompareScalarLessThan:
        case NI_AVX_CompareLessThan:
            return static_cast<int>(FloatComparisonMode::OrderedLessThanSignaling);
        case NI_SSE_CompareGreaterThanOrEqual:
        case NI_SSE_CompareScalarGreaterThanOrEqual:
        case NI_SSE2_CompareGreaterThanOrEqual:
        case NI_SSE2_CompareScalarGreaterThanOrEqual:
        case NI_AVX_CompareGreaterThanOrEqual:
            if (opportunisticallyDependsOnAVX)
            {
                return static_cast<int>(FloatComparisonMode::OrderedGreaterThanOrEqualSignaling);
            }

            // CompareGreaterThanOrEqual is not directly supported in hardware without AVX support.
            // We will return the inverted case here and lowering will itself swap the ops
            // to ensure the emitted code remains correct. This simplifies the overall logic
            // here and for other use cases.

            assert(id != NI_AVX_CompareGreaterThanOrEqual);
            return static_cast<int>(FloatComparisonMode::OrderedLessThanOrEqualSignaling);
        case NI_SSE_CompareLessThanOrEqual:
        case NI_SSE_CompareScalarLessThanOrEqual:
        case NI_SSE2_CompareLessThanOrEqual:
        case NI_SSE2_CompareScalarLessThanOrEqual:
        case NI_AVX_CompareLessThanOrEqual:
            return static_cast<int>(FloatComparisonMode::OrderedLessThanOrEqualSignaling);
        case NI_SSE_CompareNotEqual:
        case NI_SSE_CompareScalarNotEqual:
        case NI_SSE2_CompareNotEqual:
        case NI_SSE2_CompareScalarNotEqual:
        case NI_AVX_CompareNotEqual:
            return static_cast<int>(FloatComparisonMode::UnorderedNotEqualNonSignaling);
        case NI_SSE_CompareNotGreaterThan:
        case NI_SSE_CompareScalarNotGreaterThan:
        case NI_SSE2_CompareNotGreaterThan:
        case NI_SSE2_CompareScalarNotGreaterThan:
        case NI_AVX_CompareNotGreaterThan:
            if (opportunisticallyDependsOnAVX)
            {
                return static_cast<int>(FloatComparisonMode::UnorderedNotGreaterThanSignaling);
            }

            // CompareNotGreaterThan is not directly supported in hardware without AVX support.
            // We will return the inverted case here and lowering will itself swap the ops
            // to ensure the emitted code remains correct. This simplifies the overall logic
            // here and for other use cases.

            assert(id != NI_AVX_CompareNotGreaterThan);
            return static_cast<int>(FloatComparisonMode::UnorderedNotLessThanSignaling);
        case NI_SSE_CompareNotLessThan:
        case NI_SSE_CompareScalarNotLessThan:
        case NI_SSE2_CompareNotLessThan:
        case NI_SSE2_CompareScalarNotLessThan:
        case NI_AVX_CompareNotLessThan:
            return static_cast<int>(FloatComparisonMode::UnorderedNotLessThanSignaling);
        case NI_SSE_CompareNotGreaterThanOrEqual:
        case NI_SSE_CompareScalarNotGreaterThanOrEqual:
        case NI_SSE2_CompareNotGreaterThanOrEqual:
        case NI_SSE2_CompareScalarNotGreaterThanOrEqual:
        case NI_AVX_CompareNotGreaterThanOrEqual:
            if (opportunisticallyDependsOnAVX)
            {
                return static_cast<int>(FloatComparisonMode::UnorderedNotGreaterThanOrEqualSignaling);
            }

            // CompareNotGreaterThanOrEqual is not directly supported in hardware without AVX support.
            // We will return the inverted case here and lowering will itself swap the ops
            // to ensure the emitted code remains correct. This simplifies the overall logic
            // here and for other use cases.

            assert(id != NI_AVX_CompareNotGreaterThanOrEqual);
            return static_cast<int>(FloatComparisonMode::UnorderedNotLessThanOrEqualSignaling);
        case NI_SSE_CompareNotLessThanOrEqual:
        case NI_SSE_CompareScalarNotLessThanOrEqual:
        case NI_SSE2_CompareNotLessThanOrEqual:
        case NI_SSE2_CompareScalarNotLessThanOrEqual:
        case NI_AVX_CompareNotLessThanOrEqual:
            return static_cast<int>(FloatComparisonMode::UnorderedNotLessThanOrEqualSignaling);
        case NI_SSE_CompareOrdered:
        case NI_SSE_CompareScalarOrdered:
        case NI_SSE2_CompareOrdered:
        case NI_SSE2_CompareScalarOrdered:
        case NI_AVX_CompareOrdered:
            return static_cast<int>(FloatComparisonMode::OrderedNonSignaling);
        case NI_SSE_CompareUnordered:
        case NI_SSE_CompareScalarUnordered:
        case NI_SSE2_CompareUnordered:
        case NI_SSE2_CompareScalarUnordered:
        case NI_AVX_CompareUnordered:
            return static_cast<int>(FloatComparisonMode::UnorderedNonSignaling);

        case NI_SSE41_Ceiling:
        case NI_SSE41_CeilingScalar:
        case NI_SSE41_RoundToPositiveInfinity:
        case NI_SSE41_RoundToPositiveInfinityScalar:
        case NI_AVX_Ceiling:
        case NI_AVX_RoundToPositiveInfinity:
            return static_cast<int>(FloatRoundingMode::ToPositiveInfinity);
        case NI_SSE41_Floor:
        case NI_SSE41_FloorScalar:
        case NI_SSE41_RoundToNegativeInfinity:
        case NI_SSE41_RoundToNegativeInfinityScalar:
        case NI_AVX_Floor:
        case NI_AVX_RoundToNegativeInfinity:
            return static_cast<int>(FloatRoundingMode::ToNegativeInfinity);
        case NI_SSE41_RoundCurrentDirection:
        case NI_SSE41_RoundCurrentDirectionScalar:
        case NI_AVX_RoundCurrentDirection:
            return static_cast<int>(FloatRoundingMode::CurrentDirection);
        case NI_SSE41_RoundToNearestInteger:
        case NI_SSE41_RoundToNearestIntegerScalar:
        case NI_AVX_RoundToNearestInteger:
            return static_cast<int>(FloatRoundingMode::ToNearestInteger);
        case NI_SSE41_RoundToZero:
        case NI_SSE41_RoundToZeroScalar:
        case NI_AVX_RoundToZero:
            return static_cast<int>(FloatRoundingMode::ToZero);

        default:
            return -1;
    }
}

bool HWIntrinsicInfo::isFullyImplementedIsa(CORINFO_InstructionSet isa)
{
    switch (isa)
    {
        case InstructionSet_AES:
        case InstructionSet_AES_X64:
        case InstructionSet_AVX:
        case InstructionSet_AVX_X64:
        case InstructionSet_AVX2:
        case InstructionSet_AVX2_X64:
        case InstructionSet_AVXVNNI:
        case InstructionSet_AVXVNNI_X64:
        case InstructionSet_BMI1:
        case InstructionSet_BMI1_X64:
        case InstructionSet_BMI2:
        case InstructionSet_BMI2_X64:
        case InstructionSet_FMA:
        case InstructionSet_FMA_X64:
        case InstructionSet_LZCNT:
        case InstructionSet_LZCNT_X64:
        case InstructionSet_PCLMULQDQ:
        case InstructionSet_PCLMULQDQ_X64:
        case InstructionSet_POPCNT:
        case InstructionSet_POPCNT_X64:
        case InstructionSet_SSE:
        case InstructionSet_SSE_X64:
        case InstructionSet_SSE2:
        case InstructionSet_SSE2_X64:
        case InstructionSet_SSE3:
        case InstructionSet_SSE3_X64:
        case InstructionSet_SSSE3:
        case InstructionSet_SSSE3_X64:
        case InstructionSet_SSE41:
        case InstructionSet_SSE41_X64:
        case InstructionSet_SSE42:
        case InstructionSet_SSE42_X64:
        case InstructionSet_Vector128:
        case InstructionSet_Vector256:
        case InstructionSet_X86Base:
        case InstructionSet_X86Base_X64:
            return true;
        default:
            return false;
    }
}

bool HWIntrinsicInfo::isScalarIsa(CORINFO_InstructionSet isa)
{
    switch (isa)
    {
        case InstructionSet_BMI1:
        case InstructionSet_BMI1_X64:
        case InstructionSet_BMI2:
        case InstructionSet_BMI2_X64:
        case InstructionSet_LZCNT:
        case InstructionSet_LZCNT_X64:
        case InstructionSet_X86Base:
        case InstructionSet_X86Base_X64:
            // InstructionSet_POPCNT and InstructionSet_POPCNT_X64 are excluded
            // even though they are "scalar" ISA because they depend on SSE4.2
            // and Popcnt.IsSupported implies Sse42.IsSupported
            return true;

        default:
            return false;
    }
}

GenTree* Importer::ImportNonConstFallback(NamedIntrinsic intrinsic, var_types vecType, var_types eltType)
{
    assert(HWIntrinsicInfo::NoJmpTableImm(intrinsic));

    switch (intrinsic)
    {
        case NI_SSE2_ShiftLeftLogical:
        case NI_SSE2_ShiftRightArithmetic:
        case NI_SSE2_ShiftRightLogical:
        case NI_AVX2_ShiftLeftLogical:
        case NI_AVX2_ShiftRightArithmetic:
        case NI_AVX2_ShiftRightLogical:
        {
            GenTree* op2   = impPopStack().val;
            GenTree* op1   = PopVec(vecType);
            GenTree* tmpOp = NewVecNode(TYP_SIMD16, NI_SSE2_ConvertScalarToVector128Int32, TYP_INT, 16, op2);
            return NewVecNode(vecType, intrinsic, eltType, varTypeSize(vecType), op1, tmpOp);
        }

        default:
            return nullptr;
    }
}

GenTree* Importer::ImportSpecialIntrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig)
{
    switch (HWIntrinsicInfo::GetIsa(intrinsic))
    {
        case InstructionSet_Vector128:
        case InstructionSet_Vector256:
            return ImportBaseIntrinsic(intrinsic, sig);
        case InstructionSet_SSE:
        case InstructionSet_SSE2:
        case InstructionSet_SSE2_X64:
        case InstructionSet_SSE41:
        case InstructionSet_SSE41_X64:
            return ImportSSEIntrinsic(intrinsic, sig);
        case InstructionSet_AVX2:
            return ImportAVX2Intrinsic(intrinsic, sig);
        case InstructionSet_BMI1:
        case InstructionSet_BMI1_X64:
        case InstructionSet_BMI2:
        case InstructionSet_BMI2_X64:
            return ImportBMIIntrinsic(intrinsic, sig);
        default:
            return nullptr;
    }
}

GenTree* Importer::ImportBaseIntrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig)
{
    assert(!sig.hasThisParam);

    if (!opts.SIMDFeature())
    {
        return nullptr;
    }

    // TODO-MIKE-Cleanup: ISA checking & reporting is dubious.

    switch (intrinsic)
    {
        CORINFO_InstructionSet requiredIsa;
        var_types              eltType;
        unsigned               vecSize;
        GenTree*               op1;
        GenTree*               op2;

        case NI_Vector256_As:
        case NI_Vector256_AsByte:
        case NI_Vector256_AsDouble:
        case NI_Vector256_AsInt16:
        case NI_Vector256_AsInt32:
        case NI_Vector256_AsInt64:
        case NI_Vector256_AsSByte:
        case NI_Vector256_AsSingle:
        case NI_Vector256_AsUInt16:
        case NI_Vector256_AsUInt32:
        case NI_Vector256_AsUInt64:
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
        case NI_Vector128_AsVector4:
            assert(sig.paramCount == 1);
            assert(sig.paramType[0] == sig.retType);
            FALLTHROUGH;
        case NI_Vector128_AsVector:
        case NI_Vector128_AsVector128:
        case NI_Vector256_AsVector:
        case NI_Vector256_AsVector256:
        case NI_Vector128_ToVector256:
        case NI_Vector256_GetLower:
            assert(sig.paramCount == 1);
            assert((sig.paramType[0] == TYP_SIMD16) || (sig.paramType[0] == TYP_SIMD32));
            assert((sig.retType == TYP_SIMD16) || (sig.retType == TYP_SIMD32));

            if (((sig.paramType[0] == TYP_SIMD32) || (sig.retType == TYP_SIMD32)) &&
                !compExactlyDependsOn(InstructionSet_AVX))
            {
                return nullptr;
            }

            op1 = PopVec(sig.paramType[0]);

            if (sig.paramType[0] == sig.retType)
            {
                return op1;
            }

            intrinsic = sig.retType == TYP_SIMD16 ? NI_Vector256_GetLower : NI_Vector128_ToVector256;
            eltType   = varTypeNodeType(sig.paramLayout[0]->GetElementType());
            vecSize   = sig.retType == TYP_SIMD16 ? 32 : 16;
            return NewVecNode(sig.retType, intrinsic, eltType, vecSize, op1);

        case NI_Vector128_ToVector256Unsafe:
            assert(sig.paramCount == 1);
            assert((sig.paramType[0] == TYP_SIMD16) && (sig.retType == TYP_SIMD32));

            eltType = varTypeNodeType(sig.retLayout->GetElementType());

            if (!compExactlyDependsOn(InstructionSet_AVX))
            {
                return nullptr;
            }

            op1 = PopVec(TYP_SIMD16);
            return NewVecNode(TYP_SIMD32, NI_Vector128_ToVector256Unsafe, eltType, 16, op1);

        case NI_Vector128_get_Zero:
        case NI_Vector128_get_AllBitsSet:
        case NI_Vector256_get_Zero:
        case NI_Vector256_get_AllBitsSet:
            assert(sig.paramCount == 0);
            assert((sig.retType == TYP_SIMD16) || (sig.retType == TYP_SIMD32));

            eltType = varTypeNodeType(sig.retLayout->GetElementType());
            vecSize = sig.retLayout->GetSize();

            if (!compExactlyDependsOn(sig.retType == TYP_SIMD32 ? InstructionSet_AVX : InstructionSet_SSE))
            {
                return nullptr;
            }

            return NewVecNode(sig.retType, intrinsic, eltType, vecSize);

        case NI_Vector128_CreateScalarUnsafe:
        case NI_Vector256_CreateScalarUnsafe:
        case NI_Vector128_Create:
        case NI_Vector256_Create:
            assert((sig.paramCount >= 1) && (sig.paramCount <= 32));
            assert((sig.retType == TYP_SIMD16) || (sig.retType == TYP_SIMD32));

            eltType = varTypeNodeType(sig.retLayout->GetElementType());
            vecSize = sig.retLayout->GetSize();

            if (sig.retType == TYP_SIMD32)
            {
                requiredIsa = InstructionSet_AVX;
            }
            else
            {
                requiredIsa = eltType == TYP_FLOAT ? InstructionSet_SSE : InstructionSet_SSE2;
            }

            if (!compExactlyDependsOn(requiredIsa))
            {
                return nullptr;
            }

            {
                GenTreeHWIntrinsic* create = NewVecNode(sig.retType, intrinsic, eltType, vecSize);
                create->SetNumOps(sig.paramCount, getAllocator(CMK_ASTNode));

                for (unsigned i = 0; i < sig.paramCount; i++)
                {
                    GenTree* op = impPopStack().val;
                    create->SetOp(sig.paramCount - 1 - i, op);
                    create->AddSideEffects(op->GetSideEffects());
                }

                return create;
            }

        case NI_Vector128_WithElement:
        case NI_Vector256_WithElement:
        {
            assert(sig.paramCount == 3);
            assert((sig.retType == TYP_SIMD16) || (sig.retType == TYP_SIMD32));
            assert(sig.paramType[0] == sig.retType);
            assert(sig.paramLayout[0]->GetElementType() == sig.paramType[2]);
            assert(sig.paramType[1] == TYP_INT);

            GenTreeIntCon* idx = impStackTop(1).val->IsIntCon();

            if ((idx == nullptr) || (idx->GetUInt32Value() >= sig.paramLayout[0]->GetElementCount()))
            {
                return nullptr;
            }

            if (sig.retType == TYP_SIMD32)
            {
                requiredIsa = InstructionSet_AVX;
            }
            else if (sig.paramType[2] == TYP_FLOAT)
            {
                requiredIsa = InstructionSet_SSE;
            }
            else if ((sig.paramType[2] == TYP_DOUBLE) || varTypeIsShort(sig.paramType[2]))
            {
                requiredIsa = InstructionSet_SSE2;
            }
            else
            {
                // TODO-MIKE-CQ: Would it make sense to emulate PINSRD with 2 PINSRW?
                // Or just use float shuffles. Inserts/shuffles aren't necessarily faster
                // than going though memory but the memory access patterns generated by
                // vector inserts is likely to block or slow down store forwarding and
                // then the memory version won't be that fast.
                requiredIsa = InstructionSet_SSE41;
            }

            if (!compExactlyDependsOn(requiredIsa))
            {
                return nullptr;
            }

            GenTree* elt = impPopStack().val;
            /* idx = */ impPopStack();
            GenTree* vec = PopVec(sig.retType);

            return NewVecInsertNode(sig.retType, sig.paramType[2], vec, idx, elt);
        }

        case NI_Vector256_GetElement:
        case NI_Vector128_GetElement:
            assert(sig.paramCount == 2);
            assert(sig.paramLayout[0]->GetElementType() == sig.retType);

            if (sig.paramType[0] == TYP_SIMD32)
            {
                requiredIsa = InstructionSet_AVX;
            }
            else
            {
                requiredIsa = sig.retType == TYP_FLOAT ? InstructionSet_SSE : InstructionSet_SSE2;
            }

            if (!compExactlyDependsOn(requiredIsa))
            {
                return nullptr;
            }

            op2 = impPopStackCoerceArg(TYP_INT);
            op1 = PopVec(sig.paramType[0]);
            return impVectorGetElement(sig.paramLayout[0], op1, op2);

        case NI_Vector128_ToScalar:
        case NI_Vector256_ToScalar:
            assert(sig.paramCount == 1);
            assert(sig.paramLayout[0]->GetElementType() == sig.retType);

            if (sig.paramType[0] == TYP_SIMD32)
            {
                requiredIsa = InstructionSet_AVX;
                intrinsic   = NI_Vector256_GetElement;
            }
            else
            {
                requiredIsa = sig.retType == TYP_FLOAT ? InstructionSet_SSE : InstructionSet_SSE2;
                intrinsic   = NI_Vector128_GetElement;
            }

            if (!compExactlyDependsOn(requiredIsa))
            {
                return nullptr;
            }

            op2 = comp->gtNewIconNode(0);
            op1 = PopVec(sig.paramType[0]);
            return NewVecExtractNode(sig.paramType[0], sig.retType, op1, op2);

        default:
            return nullptr;
    }
}

GenTree* Importer::ImportSSEIntrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig)
{
    switch (intrinsic)
    {
        case NI_SSE_CompareScalarGreaterThan:
        case NI_SSE_CompareScalarGreaterThanOrEqual:
        case NI_SSE_CompareScalarNotGreaterThan:
        case NI_SSE_CompareScalarNotGreaterThanOrEqual:
        case NI_SSE2_CompareScalarGreaterThan:
        case NI_SSE2_CompareScalarGreaterThanOrEqual:
        case NI_SSE2_CompareScalarNotGreaterThan:
        case NI_SSE2_CompareScalarNotGreaterThanOrEqual:
        {
            assert(sig.paramCount == 2);
            GenTree* op2 = PopVec(TYP_SIMD16);
            GenTree* op1 = PopVec(TYP_SIMD16);

            var_types baseType = sig.retLayout->GetElementType();
            assert(varTypeIsFloating(baseType));

            if (compOpportunisticallyDependsOn(InstructionSet_AVX))
            {
                // These intrinsics are "special import" because the non-AVX path isn't directly
                // hardware supported. Instead, they start with "swapped operands" and we fix that here.

                FloatComparisonMode comparison =
                    static_cast<FloatComparisonMode>(HWIntrinsicInfo::GetImplicitImm(intrinsic, true));
                return NewVecNode(TYP_SIMD16, NI_AVX_CompareScalar, baseType, 16, op1, op2,
                                  comp->gtNewIconNode(static_cast<int>(comparison)));
            }

            GenTree* op1Uses[2];
            impMakeMultiUse(op1, 2, op1Uses, sig.paramLayout[0],
                            CHECK_SPILL_ALL DEBUGARG("Sse.CompareScalarGreaterThan temp"));
            GenTree* retNode = NewVecNode(TYP_SIMD16, intrinsic, baseType, 16, op2, op1Uses[0]);
            return NewVecNode(TYP_SIMD16, baseType == TYP_FLOAT ? NI_SSE_MoveScalar : NI_SSE2_MoveScalar, baseType, 16,
                              op1Uses[1], retNode);
        }

        case NI_SSE2_ConvertScalarToVector128UInt32:
            return NewVecNode(TYP_SIMD16, NI_SSE2_ConvertScalarToVector128Int32, TYP_INT, 16,
                              impPopStackCoerceArg(TYP_INT));

        case NI_SSE2_X64_ConvertScalarToVector128UInt64:
            return NewVecNode(TYP_SIMD16, NI_SSE2_X64_ConvertScalarToVector128Int64, TYP_LONG, 16,
                              impPopStackCoerceArg(TYP_LONG));

        case NI_SSE2_ConvertToInt32:
            assert(sig.paramCount == 1);
            if (sig.paramLayout[0]->GetElementType() == TYP_DOUBLE)
            {
                return NewVecNode(TYP_INT, NI_SSE2_ConvertToInt32, TYP_DOUBLE, 16, PopVec(TYP_SIMD16));
            }
            assert(sig.paramLayout[0]->GetElementType() == TYP_INT);
            FALLTHROUGH;
        case NI_SSE2_ConvertToUInt32:
            return NewVecExtractNode(TYP_SIMD16, TYP_INT, PopVec(TYP_SIMD16), comp->gtNewIconNode(0));

        case NI_SSE2_X64_ConvertToInt64:
            assert(sig.paramCount == 1);
            if (sig.paramLayout[0]->GetElementType() == TYP_DOUBLE)
            {
                return NewVecNode(TYP_LONG, NI_SSE2_X64_ConvertToInt64, TYP_DOUBLE, 16, PopVec(TYP_SIMD16));
            }
            assert(sig.paramLayout[0]->GetElementType() == TYP_LONG);
            FALLTHROUGH;
        case NI_SSE2_X64_ConvertToUInt64:
            return NewVecExtractNode(TYP_SIMD16, TYP_LONG, PopVec(TYP_SIMD16), comp->gtNewIconNode(0));

        case NI_SSE2_Extract:
        case NI_SSE41_Extract:
        case NI_SSE41_X64_Extract:
        {
            assert(sig.paramCount == 2);
            GenTree* op2 = impPopStackCoerceArg(TYP_INT);
            GenTree* op1 = PopVec(TYP_SIMD16);

            var_types eltType   = sig.paramLayout[0]->GetElementType();
            int       indexMask = static_cast<int>(sig.paramLayout[0]->GetElementCount()) - 1;

            if (GenTreeIntCon* intCon = op2->IsIntCon())
            {
                intCon->SetValue(intCon->GetValue() & indexMask);
            }
            else
            {
                op2 = comp->gtNewOperNode(GT_AND, TYP_INT, op2, comp->gtNewIconNode(indexMask));
            }

            return NewVecNode(varTypeNodeType(sig.retType), NI_Vector128_GetElement, varTypeNodeType(eltType), 16, op1,
                              op2);
        }

        case NI_SSE_Prefetch0:
        case NI_SSE_Prefetch1:
        case NI_SSE_Prefetch2:
        case NI_SSE_PrefetchNonTemporal:
        {
            assert(sig.paramCount == 1);
            assert(sig.retType == TYP_VOID);
            GenTree* op1 = impPopStack().val;
            return NewVecNode(TYP_VOID, intrinsic, TYP_UBYTE, 0, op1);
        }

        case NI_SSE_StoreFence:
        case NI_SSE2_LoadFence:
        case NI_SSE2_MemoryFence:
            assert(sig.paramCount == 0);
            assert(sig.retType == TYP_VOID);
            return NewVecNode(TYP_VOID, intrinsic, TYP_VOID, 0);

        case NI_SSE2_StoreNonTemporal:
        {
            assert(sig.paramCount == 2);
            assert(sig.retType == TYP_VOID);
            GenTree* op2 = impPopStack().val;
            GenTree* op1 = impPopStack().val;
            return NewVecNode(TYP_VOID, NI_SSE2_StoreNonTemporal, op2->GetType(), 0, op1, op2);
        }

        default:
            JITDUMP("Not implemented hardware intrinsic");
            return nullptr;
    }
}

GenTree* Importer::ImportAVX2Intrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig)
{
    switch (intrinsic)
    {
        case NI_AVX2_ConvertToInt32:
        case NI_AVX2_ConvertToUInt32:
            return NewVecExtractNode(TYP_SIMD32, TYP_INT, PopVec(TYP_SIMD32), comp->gtNewIconNode(0));

        case NI_AVX2_BroadcastScalarToVector128:
        {
            assert(sig.paramCount == 1);

            GenTree* op1;

            if (sig.paramType[0] == TYP_SIMD16)
            {
                if (sig.retLayout->GetElementType() == TYP_DOUBLE)
                {
                    intrinsic = NI_SSE3_MoveAndDuplicate;
                }

                op1 = PopVec(TYP_SIMD16);
            }
            else
            {
                op1 = impPopStack().val;
            }

            return NewVecNode(TYP_SIMD16, intrinsic, sig.retLayout->GetElementType(), 16, op1);
        }

        case NI_AVX2_PermuteVar8x32:
        {
            var_types eltType = sig.retLayout->GetElementType();
            GenTree*  control = PopVec(TYP_SIMD32);
            GenTree*  left    = PopVec(TYP_SIMD32);

            // AVX2.PermuteVar8x32 signature is messed up, parameter order does not match
            // instruction operand order.

            if (!gtCanSwapOrder(control, left))
            {
                // TODO-MIKE-Review: Can we simply set GTF_REVERSE_OPS to avoid creating a temp?

                LclVarDsc* tempLcl = lvaAllocTemp(true DEBUGARG("AVX2.PermuteVar8x32 temp"));
                impAppendTempStore(tempLcl, left, sig.paramLayout[0], CHECK_SPILL_ALL);
                left = comp->gtNewLclLoad(tempLcl, sig.paramType[0]);
            }

            return NewVecNode(TYP_SIMD32, NI_AVX2_PermuteVar8x32, eltType, 32, control, left);
        }

        case NI_AVX2_GatherVector128:
        case NI_AVX2_GatherVector256:
        {
            assert(sig.paramCount == 3);

            GenTree* op3 = PopHWIntrinsicArg(sig.paramType[2], sig.paramLayout[2]);
            GenTree* op2 = PopHWIntrinsicArg(sig.paramType[1], sig.paramLayout[1]);
            GenTree* op1 = PopHWIntrinsicArg(sig.paramType[0], sig.paramLayout[0]);

            unsigned  vecSize   = sig.retLayout->GetSize();
            var_types eltType   = sig.retLayout->GetElementType();
            var_types indexType = sig.paramLayout[1]->GetElementType();
            assert((indexType == TYP_INT) || (indexType == TYP_LONG));

            intrinsic = indexType == TYP_LONG ? NI_AVX2_GATHERQ : NI_AVX2_GATHERD;

            GenTree* node = NewVecNode(sig.retType, intrinsic, eltType, vecSize, op1, op2, op3);
            node->AddSideEffects(GTF_GLOB_REF | GTF_EXCEPT);
            return node;
        }

        case NI_AVX2_GatherMaskVector128:
        case NI_AVX2_GatherMaskVector256:
        {
            assert(sig.paramCount == 5);

            GenTree* op5 = PopHWIntrinsicArg(sig.paramType[4], sig.paramLayout[4]);
            GenTree* op4 = PopHWIntrinsicArg(sig.paramType[3], sig.paramLayout[3]);
            GenTree* op3 = PopHWIntrinsicArg(sig.paramType[2], sig.paramLayout[2]);
            GenTree* op2 = PopHWIntrinsicArg(sig.paramType[1], sig.paramLayout[1]);
            GenTree* op1 = PopHWIntrinsicArg(sig.paramType[0], sig.paramLayout[0]);

            unsigned  vecSize   = sig.retLayout->GetSize();
            var_types eltType   = sig.retLayout->GetElementType();
            var_types indexType = sig.paramLayout[2]->GetElementType();
            assert((indexType == TYP_INT) || (indexType == TYP_LONG));

            intrinsic = indexType == TYP_LONG ? NI_AVX2_GATHERQ : NI_AVX2_GATHERD;

            GenTree* node = NewVecNode(sig.retType, intrinsic, eltType, vecSize, op1, op2, op3, op4, op5);
            node->AddSideEffects(GTF_GLOB_REF | GTF_EXCEPT);
            return node;
        }

        default:
            JITDUMP("Not implemented hardware intrinsic");
            return nullptr;
    }
}

GenTree* Importer::ImportBMIIntrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig)
{
    switch (intrinsic)
    {
        case NI_BMI1_BitFieldExtract:
        case NI_BMI1_X64_BitFieldExtract:
            // The 3-arg version is implemented in managed code
            if (sig.paramCount == 3)
            {
                return nullptr;
            }
            FALLTHROUGH;
        case NI_BMI2_ZeroHighBits:
        case NI_BMI2_X64_ZeroHighBits:
        {
            assert(sig.paramCount == 2);
            assert(sig.retType == sig.paramType[0]);
            assert(sig.retType == TYP_UINT || sig.retType == TYP_ULONG);

            var_types type = varTypeNodeType(sig.retType);
            GenTree*  op2  = impPopStack().val;
            GenTree*  op1  = impPopStack().val;

            if (!gtCanSwapOrder(op1, op2))
            {
                // TODO-MIKE-Review: Can we simply set GTF_REVERSE_OPS to avoid creating a temp?

                LclVarDsc* tempLcl = lvaAllocTemp(true DEBUGARG("BMI.BitFieldExtract/ZeroHightBits temp"));
                impAppendTempStore(tempLcl, op1, CHECK_SPILL_ALL);
                op1 = comp->gtNewLclLoad(tempLcl, type);
            }

            // Instructions BZHI and BEXTR require to encode op2 (3rd register) in VEX.vvvv and op1
            // maybe memory operand, so swap op1 and op2 to unify the backend code.

            // TODO-MIKE-Review: It would be better for codegen to handle this instead of having
            // to swap here and potentially add a temp...

            return gtNewScalarHWIntrinsicNode(type, intrinsic, op2, op1);
        }

        default:
            return nullptr;
    }
}

#endif // FEATURE_HW_INTRINSICS
