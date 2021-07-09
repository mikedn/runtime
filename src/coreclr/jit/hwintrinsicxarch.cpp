// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "hwintrinsic.h"

#ifdef FEATURE_HW_INTRINSICS

//------------------------------------------------------------------------
// X64VersionOfIsa: Gets the corresponding 64-bit only InstructionSet for a given InstructionSet
//
// Arguments:
//    isa -- The InstructionSet ID
//
// Return Value:
//    The 64-bit only InstructionSet associated with isa
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

//------------------------------------------------------------------------
// lookupIsa: Gets the InstructionSet for a given class name and enclosing class name
//
// Arguments:
//    className -- The name of the class associated with the InstructionSet to lookup
//    enclosingClassName -- The name of the enclosing class of X64 classes
//
// Return Value:
//    The InstructionSet associated with className and enclosingClassName
CORINFO_InstructionSet HWIntrinsicInfo::lookupIsa(const char* className, const char* enclosingClassName)
{
    assert(className != nullptr);

    if (strcmp(className, "X64") == 0)
    {
        assert(enclosingClassName != nullptr);
        return X64VersionOfIsa(lookupInstructionSet(enclosingClassName));
    }
    else
    {
        return lookupInstructionSet(className);
    }
}

//------------------------------------------------------------------------
// lookupImmUpperBound: Gets the upper bound for the imm-value of a given NamedIntrinsic
//
// Arguments:
//    id -- The NamedIntrinsic associated with the HWIntrinsic to lookup
//
// Return Value:
//     The upper bound for the imm-value of the intrinsic associated with id
//
int HWIntrinsicInfo::lookupImmUpperBound(NamedIntrinsic id)
{
    assert(HWIntrinsicInfo::lookupCategory(id) == HW_Category_IMM);

    switch (id)
    {
        case NI_AVX_Compare:
        case NI_AVX_CompareScalar:
        {
            assert(!HWIntrinsicInfo::HasFullRangeImm(id));
            return 31; // enum FloatComparisonMode has 32 values
        }

        case NI_AVX2_GatherVector128:
        case NI_AVX2_GatherVector256:
        case NI_AVX2_GatherMaskVector128:
        case NI_AVX2_GatherMaskVector256:
            return 8;

        default:
        {
            assert(HWIntrinsicInfo::HasFullRangeImm(id));
            return 255;
        }
    }
}

//------------------------------------------------------------------------
// isAVX2GatherIntrinsic: Check if the intrinsic is AVX Gather*
//
// Arguments:
//    id   -- The NamedIntrinsic associated with the HWIntrinsic to lookup
//
// Return Value:
//     true if id is AVX Gather* intrinsic
//
bool HWIntrinsicInfo::isAVX2GatherIntrinsic(NamedIntrinsic id)
{
    switch (id)
    {
        case NI_AVX2_GatherVector128:
        case NI_AVX2_GatherVector256:
        case NI_AVX2_GatherMaskVector128:
        case NI_AVX2_GatherMaskVector256:
            return true;
        default:
            return false;
    }
}

//------------------------------------------------------------------------
// lookupFloatComparisonModeForSwappedArgs: Get the floating-point comparison
//      mode to use when the operands are swapped.
//
// Arguments:
//    comparison -- The comparison mode used for (op1, op2)
//
// Return Value:
//     The comparison mode to use for (op2, op1)
//
FloatComparisonMode HWIntrinsicInfo::lookupFloatComparisonModeForSwappedArgs(FloatComparisonMode comparison)
{
    switch (comparison)
    {
        // These comparison modes are the same even if the operands are swapped

        case FloatComparisonMode::OrderedEqualNonSignaling:
            return FloatComparisonMode::OrderedEqualNonSignaling;
        case FloatComparisonMode::UnorderedNonSignaling:
            return FloatComparisonMode::UnorderedNonSignaling;
        case FloatComparisonMode::UnorderedNotEqualNonSignaling:
            return FloatComparisonMode::UnorderedNotEqualNonSignaling;
        case FloatComparisonMode::OrderedNonSignaling:
            return FloatComparisonMode::OrderedNonSignaling;
        case FloatComparisonMode::UnorderedEqualNonSignaling:
            return FloatComparisonMode::UnorderedEqualNonSignaling;
        case FloatComparisonMode::OrderedFalseNonSignaling:
            return FloatComparisonMode::OrderedFalseNonSignaling;
        case FloatComparisonMode::OrderedNotEqualNonSignaling:
            return FloatComparisonMode::OrderedNotEqualNonSignaling;
        case FloatComparisonMode::UnorderedTrueNonSignaling:
            return FloatComparisonMode::UnorderedTrueNonSignaling;
        case FloatComparisonMode::OrderedEqualSignaling:
            return FloatComparisonMode::OrderedEqualSignaling;
        case FloatComparisonMode::UnorderedSignaling:
            return FloatComparisonMode::UnorderedSignaling;
        case FloatComparisonMode::UnorderedNotEqualSignaling:
            return FloatComparisonMode::UnorderedNotEqualSignaling;
        case FloatComparisonMode::OrderedSignaling:
            return FloatComparisonMode::OrderedSignaling;
        case FloatComparisonMode::UnorderedEqualSignaling:
            return FloatComparisonMode::UnorderedEqualSignaling;
        case FloatComparisonMode::OrderedFalseSignaling:
            return FloatComparisonMode::OrderedFalseSignaling;
        case FloatComparisonMode::OrderedNotEqualSignaling:
            return FloatComparisonMode::OrderedNotEqualSignaling;
        case FloatComparisonMode::UnorderedTrueSignaling:
            return FloatComparisonMode::UnorderedTrueSignaling;

        // These comparison modes need a different mode if the operands are swapped

        case FloatComparisonMode::OrderedLessThanSignaling:
            return FloatComparisonMode::OrderedGreaterThanSignaling;
        case FloatComparisonMode::OrderedLessThanOrEqualSignaling:
            return FloatComparisonMode::OrderedGreaterThanOrEqualSignaling;
        case FloatComparisonMode::UnorderedNotLessThanSignaling:
            return FloatComparisonMode::UnorderedNotGreaterThanSignaling;
        case FloatComparisonMode::UnorderedNotLessThanOrEqualSignaling:
            return FloatComparisonMode::UnorderedNotGreaterThanOrEqualSignaling;
        case FloatComparisonMode::UnorderedNotGreaterThanOrEqualSignaling:
            return FloatComparisonMode::UnorderedNotLessThanOrEqualSignaling;
        case FloatComparisonMode::UnorderedNotGreaterThanSignaling:
            return FloatComparisonMode::UnorderedNotLessThanSignaling;
        case FloatComparisonMode::OrderedGreaterThanOrEqualSignaling:
            return FloatComparisonMode::OrderedLessThanOrEqualSignaling;
        case FloatComparisonMode::OrderedGreaterThanSignaling:
            return FloatComparisonMode::OrderedLessThanSignaling;
        case FloatComparisonMode::OrderedLessThanNonSignaling:
            return FloatComparisonMode::OrderedGreaterThanNonSignaling;
        case FloatComparisonMode::OrderedLessThanOrEqualNonSignaling:
            return FloatComparisonMode::OrderedGreaterThanOrEqualNonSignaling;
        case FloatComparisonMode::UnorderedNotLessThanNonSignaling:
            return FloatComparisonMode::UnorderedNotGreaterThanNonSignaling;
        case FloatComparisonMode::UnorderedNotLessThanOrEqualNonSignaling:
            return FloatComparisonMode::UnorderedNotGreaterThanOrEqualNonSignaling;
        case FloatComparisonMode::UnorderedNotGreaterThanOrEqualNonSignaling:
            return FloatComparisonMode::UnorderedNotLessThanOrEqualNonSignaling;
        case FloatComparisonMode::UnorderedNotGreaterThanNonSignaling:
            return FloatComparisonMode::UnorderedNotLessThanNonSignaling;
        case FloatComparisonMode::OrderedGreaterThanOrEqualNonSignaling:
            return FloatComparisonMode::OrderedLessThanOrEqualNonSignaling;
        case FloatComparisonMode::OrderedGreaterThanNonSignaling:
            return FloatComparisonMode::OrderedLessThanNonSignaling;

        default:
            unreached();
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
        case InstructionSet_AES:
        case InstructionSet_AES_X64:
        case InstructionSet_AVX:
        case InstructionSet_AVX_X64:
        case InstructionSet_AVX2:
        case InstructionSet_AVX2_X64:
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
        case InstructionSet_BMI1:
        case InstructionSet_BMI1_X64:
        case InstructionSet_BMI2:
        case InstructionSet_BMI2_X64:
        case InstructionSet_LZCNT:
        case InstructionSet_LZCNT_X64:
        case InstructionSet_X86Base:
        case InstructionSet_X86Base_X64:
        {
            // InstructionSet_POPCNT and InstructionSet_POPCNT_X64 are excluded
            // even though they are "scalar" ISA because they depend on SSE4.2
            // and Popcnt.IsSupported implies Sse42.IsSupported
            return true;
        }

        default:
        {
            return false;
        }
    }
}

//------------------------------------------------------------------------
// impNonConstFallback: convert certain SSE2/AVX2 shift intrinsic to its semantic alternative when the imm-arg is
// not a compile-time constant
//
// Arguments:
//    intrinsic  -- intrinsic ID
//    simdType   -- Vector type
//    baseType   -- base type of the Vector128/256<T>
//
// Return Value:
//     return the IR of semantic alternative on non-const imm-arg
//
GenTree* Compiler::impNonConstFallback(NamedIntrinsic intrinsic, var_types simdType, var_types baseType)
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
            GenTree* op2 = impPopStack().val;
            GenTree* op1 = impSIMDPopStack(simdType);
            GenTree* tmpOp =
                gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_SSE2_ConvertScalarToVector128Int32, TYP_INT, 16, op2);
            return gtNewSimdHWIntrinsicNode(simdType, intrinsic, baseType, genTypeSize(simdType), op1, tmpOp);
        }

        default:
            return nullptr;
    }
}

GenTree* Compiler::impSpecialIntrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig)
{
    switch (HWIntrinsicInfo::lookupIsa(intrinsic))
    {
        case InstructionSet_Vector128:
        case InstructionSet_Vector256:
            return impBaseIntrinsic(intrinsic, sig);
        case InstructionSet_SSE:
        case InstructionSet_SSE2:
        case InstructionSet_SSE41:
        case InstructionSet_SSE41_X64:
            return impSSEIntrinsic(intrinsic, sig);
        case InstructionSet_AVX:
        case InstructionSet_AVX2:
            return impAvxOrAvx2Intrinsic(intrinsic, sig);
        case InstructionSet_BMI1:
        case InstructionSet_BMI1_X64:
        case InstructionSet_BMI2:
        case InstructionSet_BMI2_X64:
            return impBMI1OrBMI2Intrinsic(intrinsic, sig);
        default:
            return nullptr;
    }
}

GenTree* Compiler::impBaseIntrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig)
{
    assert(!sig.hasThisParam);

    if (!featureSIMD)
    {
        return nullptr;
    }

    // TODO-MIKE-Cleanup: ISA checking & reporting is dubious.

    switch (intrinsic)
    {
        CORINFO_InstructionSet requiredIsa;
        var_types              eltType;
        unsigned               simdSize;
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

            op1 = impSIMDPopStack(sig.paramType[0]);

            if (sig.paramType[0] == sig.retType)
            {
                return op1;
            }

            intrinsic = sig.retType == TYP_SIMD16 ? NI_Vector256_GetLower : NI_Vector128_ToVector256;
            eltType   = sig.paramLayout[0]->GetElementType();
            simdSize  = sig.retType == TYP_SIMD16 ? 32 : 16;
            return gtNewSimdHWIntrinsicNode(sig.retType, intrinsic, eltType, simdSize, op1);

        case NI_Vector128_ToVector256Unsafe:
            assert(sig.paramCount == 1);
            assert((sig.paramType[0] == TYP_SIMD16) && (sig.retType == TYP_SIMD32));

            eltType = sig.retLayout->GetElementType();

            if (!compExactlyDependsOn(InstructionSet_AVX))
            {
                return nullptr;
            }

            op1 = impSIMDPopStack(TYP_SIMD16);
            return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_Vector128_ToVector256Unsafe, eltType, 16, op1);

        case NI_Vector128_get_Zero:
        case NI_Vector128_get_AllBitsSet:
        case NI_Vector256_get_Zero:
        case NI_Vector256_get_AllBitsSet:
            assert(sig.paramCount == 0);
            assert((sig.retType == TYP_SIMD16) || (sig.retType == TYP_SIMD32));

            eltType  = sig.retLayout->GetElementType();
            simdSize = sig.retLayout->GetSize();

            if (!compExactlyDependsOn(sig.retType == TYP_SIMD32 ? InstructionSet_AVX : InstructionSet_SSE))
            {
                return nullptr;
            }

            return gtNewSimdHWIntrinsicNode(sig.retType, intrinsic, eltType, simdSize);

        case NI_Vector128_CreateScalarUnsafe:
        case NI_Vector256_CreateScalarUnsafe:
        case NI_Vector128_Create:
        case NI_Vector256_Create:
            assert((sig.paramCount >= 1) && (sig.paramCount <= 32));
            assert((sig.retType == TYP_SIMD16) || (sig.retType == TYP_SIMD32));

            eltType  = sig.retLayout->GetElementType();
            simdSize = sig.retLayout->GetSize();

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
            GenTree* vec = impSIMDPopStack(sig.retType);

            return gtNewSimdWithElementNode(sig.retType, sig.paramType[2], vec, idx, elt);
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
            op1 = impSIMDPopStack(sig.paramType[0]);
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

            op2 = gtNewIconNode(0);
            op1 = impSIMDPopStack(sig.paramType[0]);
            return gtNewSimdGetElementNode(sig.paramType[0], sig.retType, op1, op2);

        default:
            return nullptr;
    }
}

GenTree* Compiler::impSSEIntrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig)
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
            GenTree* op2 = impSIMDPopStack(TYP_SIMD16);
            GenTree* op1 = impSIMDPopStack(TYP_SIMD16);

            var_types baseType = sig.retLayout->GetElementType();
            assert(varTypeIsFloating(baseType));

            if (compOpportunisticallyDependsOn(InstructionSet_AVX))
            {
                // These intrinsics are "special import" because the non-AVX path isn't directly
                // hardware supported. Instead, they start with "swapped operands" and we fix that here.

                FloatComparisonMode comparison =
                    static_cast<FloatComparisonMode>(HWIntrinsicInfo::lookupIval(intrinsic, true));
                return gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AVX_CompareScalar, baseType, 16, op1, op2,
                                                gtNewIconNode(static_cast<int>(comparison)));
            }

            GenTree* op1Uses[2];
            impMakeMultiUse(op1, 2, op1Uses, sig.paramLayout[0],
                            CHECK_SPILL_ALL DEBUGARG("Sse.CompareScalarGreaterThan temp"));
            GenTree* retNode = gtNewSimdHWIntrinsicNode(TYP_SIMD16, intrinsic, baseType, 16, op2, op1Uses[0]);
            return gtNewSimdHWIntrinsicNode(TYP_SIMD16, baseType == TYP_FLOAT ? NI_SSE_MoveScalar : NI_SSE2_MoveScalar,
                                            baseType, 16, op1Uses[1], retNode);
        }

        case NI_SSE2_Extract:
        case NI_SSE41_Extract:
        case NI_SSE41_X64_Extract:
        {
            assert(sig.paramCount == 2);
            GenTree* op2 = impPopStackCoerceArg(TYP_INT);
            GenTree* op1 = impSIMDPopStack(TYP_SIMD16);

            var_types baseType  = sig.paramLayout[0]->GetElementType();
            int       indexMask = static_cast<int>(sig.paramLayout[0]->GetElementCount()) - 1;

            if (GenTreeIntCon* intCon = op2->IsIntCon())
            {
                intCon->SetValue(intCon->GetValue() & indexMask);
            }
            else
            {
                op2 = gtNewOperNode(GT_AND, TYP_INT, op2, gtNewIconNode(indexMask));
            }

            return gtNewSimdHWIntrinsicNode(sig.retType, NI_Vector128_GetElement, baseType, 16, op1, op2);
        }

        case NI_SSE_Prefetch0:
        case NI_SSE_Prefetch1:
        case NI_SSE_Prefetch2:
        case NI_SSE_PrefetchNonTemporal:
        {
            assert(sig.paramCount == 1);
            assert(sig.retType == TYP_VOID);
            GenTree* op1 = impPopStack().val;
            return gtNewSimdHWIntrinsicNode(TYP_VOID, intrinsic, TYP_UBYTE, 0, op1);
        }

        case NI_SSE_StoreFence:
        case NI_SSE2_LoadFence:
        case NI_SSE2_MemoryFence:
            assert(sig.paramCount == 0);
            assert(sig.retType == TYP_VOID);
            return gtNewSimdHWIntrinsicNode(TYP_VOID, intrinsic, TYP_VOID, 0);

        case NI_SSE2_StoreNonTemporal:
        {
            assert(sig.paramCount == 2);
            assert(sig.retType == TYP_VOID);
            GenTree* op2 = impPopStack().val;
            GenTree* op1 = impPopStack().val;
            return gtNewSimdHWIntrinsicNode(TYP_VOID, NI_SSE2_StoreNonTemporal, op2->TypeGet(), 0, op1, op2);
        }

        default:
            JITDUMP("Not implemented hardware intrinsic");
            return nullptr;
    }
}

GenTree* Compiler::impAvxOrAvx2Intrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig)
{
    switch (intrinsic)
    {
        case NI_AVX2_PermuteVar8x32:
        {
            var_types eltType = sig.retLayout->GetElementType();
            GenTree*  control = impSIMDPopStack(TYP_SIMD32);
            GenTree*  left    = impSIMDPopStack(TYP_SIMD32);

            // AVX2.PermuteVar8x32 signature is messed up, parameter order does not match
            // instruction operand order.

            if (!gtCanSwapOrder(control, left))
            {
                // TODO-MIKE-Review: Can we simply set GTF_REVERSE_OPS to avoid creating a temp?

                unsigned lclNum = lvaGrabTemp(true DEBUGARG("AVX2.PermuteVar8x32 temp"));
                impAppendTempAssign(lclNum, left, sig.paramLayout[0], CHECK_SPILL_ALL);
                left = gtNewLclvNode(lclNum, sig.paramType[0]);
            }

            return gtNewSimdHWIntrinsicNode(TYP_SIMD32, NI_AVX2_PermuteVar8x32, eltType, 32, control, left);
        }

        case NI_AVX2_GatherMaskVector128:
        case NI_AVX2_GatherMaskVector256:
        {
            assert(sig.paramCount == 5);

            GenTree* op5 = impPopArgForHWIntrinsic(sig.paramType[4], sig.paramLayout[4]);
            GenTree* op4 = impPopArgForHWIntrinsic(sig.paramType[3], sig.paramLayout[3]);
            GenTree* op3 = impPopArgForHWIntrinsic(sig.paramType[2], sig.paramLayout[2]);
            GenTree* op2 = impPopArgForHWIntrinsic(sig.paramType[1], sig.paramLayout[1]);
            GenTree* op1 = impPopArgForHWIntrinsic(sig.paramType[0], sig.paramLayout[0]);

            var_types eltType  = sig.retLayout->GetElementType();
            unsigned  simdSize = sig.retLayout->GetSize();

            GenTreeHWIntrinsic* retNode =
                gtNewSimdHWIntrinsicNode(sig.retType, intrinsic, eltType, simdSize, op1, op2, op3, op4, op5);
            retNode->SetAuxiliaryType(sig.paramLayout[2]->GetElementType());
            return retNode;
        }

        default:
            JITDUMP("Not implemented hardware intrinsic");
            return nullptr;
    }
}

GenTree* Compiler::impBMI1OrBMI2Intrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig)
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

            GenTree* op2 = impPopStack().val;
            GenTree* op1 = impPopStack().val;

            if (!gtCanSwapOrder(op1, op2))
            {
                // TODO-MIKE-Review: Can we simply set GTF_REVERSE_OPS to avoid creating a temp?

                unsigned lclNum = lvaGrabTemp(true DEBUGARG("BMI.BitFieldExtract/ZeroHightBits temp"));
                impAppendTempAssign(lclNum, op1, CHECK_SPILL_ALL);
                op1 = gtNewLclvNode(lclNum, varActualType(sig.paramType[0]));
            }

            // Instructions BZHI and BEXTR require to encode op2 (3rd register) in VEX.vvvv and op1
            // maybe memory operand, so swap op1 and op2 to unify the backend code.

            // TODO-MIKE-Review: It would be better for codegen to handle this instead of having
            // to swap here and potentially add a temp...

            return gtNewScalarHWIntrinsicNode(varActualType(sig.retType), intrinsic, op2, op1);
        }

        default:
            return nullptr;
    }
}

#endif // FEATURE_HW_INTRINSICS
