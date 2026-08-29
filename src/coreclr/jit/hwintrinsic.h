// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#ifdef FEATURE_HW_INTRINSICS

enum HWIntrinsicCategory : unsigned
{
#ifdef TARGET_XARCH
    // Simple SIMD intrinsics
    // - take Vector128/256<T> parameters
    // - return a Vector128/256<T>
    // - the codegen of overloads can be determined by intrinsicID and base type of returned vector
    HW_Category_SimpleSIMD,

    // Scalar intrinsics
    // - operate over general purpose registers, like crc32, lzcnt, popcnt, etc.
    HW_Category_Scalar,

    // Special intrinsics
    // - have to be addressed specially
    HW_Category_Special

#elif defined(TARGET_ARM64)

    // Most of the Arm64 intrinsic fall into SIMD category:
    // - vector or scalar intrinsics that operate on one-or-many SIMD registers
    HW_Category_SIMD,

    // Scalar intrinsics operate on general purpose registers (e.g. cls, clz, rbit)
    HW_Category_Scalar,

    // Memory access intrinsics
    HW_Category_MemoryLoad,
    HW_Category_MemoryStore,

    // These are Arm64 that share some features in a given category (e.g. immediate operand value range)
    HW_Category_ShiftLeftByImmediate,
    HW_Category_ShiftRightByImmediate,
    HW_Category_SIMDByIndexedElement,

    // Helper intrinsics
    // - do not directly correspond to a instruction, such as Vector64.AllBitsSet
    HW_Category_Helper,
#else
#error Unsupported platform
#endif
};

enum HWIntrinsicFlag : unsigned
{
    HW_Flag_None             = 0x00,
    HW_Flag_NoFlag           = HW_Flag_None,
    HW_Flag_Commutative      = 0x01,
    HW_Flag_SpecialImport    = 0x02,
    HW_Flag_BaseTypeFromArg0 = 0x04,
    HW_Flag_BaseTypeFromArg1 = 0x08,
    HW_Flag_NoCodeGen        = 0x10,
    HW_Flag_SpecialCodeGen   = 0x20,

#if defined(TARGET_XARCH)
    HW_Flag_DupUnaryOp    = 0x40,
    HW_Flag_IMM           = 0x80,
    HW_Flag_Load          = 0x100,
    HW_Flag_Store         = 0x200,
    HW_Flag_MayLoad       = 0x400,
    HW_Flag_MayStore      = 0x800,
    HW_Flag_NoRMW         = 0x1000,
    HW_Flag_NoContainment = 0x2000,
    HW_Flag_XmmScalar     = 0x4000,
#elif defined(TARGET_ARM64)
    // NoJmpTable IMM
    // the imm intrinsic does not need jumptable fallback when it gets non-const argument
    HW_Flag_NoJmpTableIMM = 0x40,

    // The intrinsic has an immediate operand
    // - the value can be (and should be) encoded in a corresponding instruction when the operand value is constant
    HW_Flag_HasImmediateOperand = 0x200,

    // The intrinsic has read/modify/write semantics in multiple-operands form.
    HW_Flag_HasRMWSemantics = 0x400,

    // The intrinsic operates on the lower part of a SIMD register
    // - the upper part of the source registers are ignored
    // - the upper part of the destination register is zeroed
    HW_Flag_SIMDScalar = 0x800,

#else
#error Unsupported platform
#endif
};

#if defined(TARGET_XARCH)
// This mirrors the System.Runtime.Intrinsics.X86.FloatComparisonMode enumeration
enum class FloatComparisonMode : uint8_t
{
    // _CMP_EQ_OQ
    OrderedEqualNonSignaling = 0,

    // _CMP_LT_OS
    OrderedLessThanSignaling = 1,

    // _CMP_LE_OS
    OrderedLessThanOrEqualSignaling = 2,

    // _CMP_UNORD_Q
    UnorderedNonSignaling = 3,

    // _CMP_NEQ_UQ
    UnorderedNotEqualNonSignaling = 4,

    // _CMP_NLT_US
    UnorderedNotLessThanSignaling = 5,

    // _CMP_NLE_US
    UnorderedNotLessThanOrEqualSignaling = 6,

    // _CMP_ORD_Q
    OrderedNonSignaling = 7,

    // _CMP_EQ_UQ
    UnorderedEqualNonSignaling = 8,

    // _CMP_NGE_US
    UnorderedNotGreaterThanOrEqualSignaling = 9,

    // _CMP_NGT_US
    UnorderedNotGreaterThanSignaling = 10,

    // _CMP_FALSE_OQ
    OrderedFalseNonSignaling = 11,

    // _CMP_NEQ_OQ
    OrderedNotEqualNonSignaling = 12,

    // _CMP_GE_OS
    OrderedGreaterThanOrEqualSignaling = 13,

    // _CMP_GT_OS
    OrderedGreaterThanSignaling = 14,

    // _CMP_TRUE_UQ
    UnorderedTrueNonSignaling = 15,

    // _CMP_EQ_OS
    OrderedEqualSignaling = 16,

    // _CMP_LT_OQ
    OrderedLessThanNonSignaling = 17,

    // _CMP_LE_OQ
    OrderedLessThanOrEqualNonSignaling = 18,

    // _CMP_UNORD_S
    UnorderedSignaling = 19,

    // _CMP_NEQ_US
    UnorderedNotEqualSignaling = 20,

    // _CMP_NLT_UQ
    UnorderedNotLessThanNonSignaling = 21,

    // _CMP_NLE_UQ
    UnorderedNotLessThanOrEqualNonSignaling = 22,

    // _CMP_ORD_S
    OrderedSignaling = 23,

    // _CMP_EQ_US
    UnorderedEqualSignaling = 24,

    // _CMP_NGE_UQ
    UnorderedNotGreaterThanOrEqualNonSignaling = 25,

    // _CMP_NGT_UQ
    UnorderedNotGreaterThanNonSignaling = 26,

    // _CMP_FALSE_OS
    OrderedFalseSignaling = 27,

    // _CMP_NEQ_OS
    OrderedNotEqualSignaling = 28,

    // _CMP_GE_OQ
    OrderedGreaterThanOrEqualNonSignaling = 29,

    // _CMP_GT_OQ
    OrderedGreaterThanNonSignaling = 30,

    // _CMP_TRUE_US
    UnorderedTrueSignaling = 31,
};

enum class FloatRoundingMode : uint8_t
{
    // _MM_FROUND_TO_NEAREST_INT
    ToNearestInteger = 0x00,

    // _MM_FROUND_TO_NEG_INF
    ToNegativeInfinity = 0x01,

    // _MM_FROUND_TO_POS_INF
    ToPositiveInfinity = 0x02,

    // _MM_FROUND_TO_ZERO
    ToZero = 0x03,

    // _MM_FROUND_CUR_DIRECTION
    CurrentDirection = 0x04,

    // _MM_FROUND_RAISE_EXC
    RaiseException = 0x00,

    // _MM_FROUND_NO_EXC
    NoException = 0x08,
};
#endif // TARGET_XARCH

struct HWIntrinsicInfo
{
    static NamedIntrinsic lookupId(Compiler*             comp,
                                   CORINFO_METHOD_HANDLE method,
                                   const char*           className,
                                   const char*           methodName,
                                   const char*           enclosingClassName);
    static CORINFO_InstructionSet lookupIsa(const char* className, const char* enclosingClassName);

#if defined(TARGET_XARCH)
    static unsigned GetImmOpUpperBound(NamedIntrinsic intrinsic);
#elif defined(TARGET_ARM64)
    static void GetImmOpBounds(
        NamedIntrinsic intrinsic, unsigned vecSize, var_types eltType, int* lowerBound, int* upperBound);
#else
#error Unsupported platform
#endif

#ifdef TARGET_XARCH
    static bool IsAvx2GatherIntrinsic(NamedIntrinsic id);
    static int GetImplicitImm(NamedIntrinsic id, bool opportunisticallyDependsOnAVX);
#endif

    static CORINFO_InstructionSet GetIsa(NamedIntrinsic id);
    static HWIntrinsicCategory GetCategory(NamedIntrinsic id);
    static instruction GetIns(NamedIntrinsic id, var_types type);
    static bool HasFlag(NamedIntrinsic id, HWIntrinsicFlag flag);

    static bool IsCommutative(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_Commutative);
    }

    static bool BaseTypeFromArg0(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_BaseTypeFromArg0);
    }

    static bool BaseTypeFromArg1(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_BaseTypeFromArg1);
    }

    static bool HasSpecialImport(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_SpecialImport);
    }

    static bool HasSpecialCodegen(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_SpecialCodeGen);
    }

    static bool RequiresCodegen(NamedIntrinsic id)
    {
        return !HasFlag(id, HW_Flag_NoCodeGen);
    }

#ifdef TARGET_XARCH
    static bool HasRMWSemantics(NamedIntrinsic id)
    {
        return !HasFlag(id, HW_Flag_NoRMW);
    }

    static bool HasIMM(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_IMM);
    }

    static bool IsLoad(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_Load);
    }

    static bool IsStore(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_Store);
    }

    static bool IsXmmScalar(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_XmmScalar);
    }

    static bool SupportsContainment(NamedIntrinsic id)
    {
        return !HasFlag(id, HW_Flag_NoContainment);
    }

    static bool DupUnaryOp(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_DupUnaryOp);
    }

    static bool MayLoad(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_MayLoad);
    }

    static bool MayStore(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_MayStore);
    }
#endif

#ifdef TARGET_ARM64
    static bool HasRMWSemantics(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_HasRMWSemantics);
    }

    static bool NoJmpTableImm(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_NoJmpTableIMM);
    }

    static bool SIMDScalar(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_SIMDScalar);
    }

    static bool HasImmediateOperand(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_HasImmediateOperand);
    }
#endif
};

struct HWIntrinsicSignature final
{
    var_types    retType;
    ClassLayout* retLayout;
    bool         hasThisParam;
    bool         isIntrinsicMethod;
    unsigned     paramCount;
    var_types    paramType[5];
    var_types    paramPointerType[5];
    ClassLayout* paramLayout[5];

    void Read(class Compiler* compiler, CORINFO_SIG_INFO* sig);
    var_types GetBaseTypeFromParam(unsigned index, ClassLayout** argLayout) const;
};

#ifdef TARGET_ARM64

struct HWIntrinsic final
{
    NamedIntrinsic      id;
    HWIntrinsicCategory category;
    var_types           vecEltType;
    unsigned            numOperands;
    GenTree*            ops[4];

    HWIntrinsic(const GenTreeHWIntrinsic* node)
        : id(node->GetIntrinsic())
        , category(HWIntrinsicInfo::GetCategory(id))
        , vecEltType(node->GetVecEltType())
        , numOperands(node->GetNumOps())
        , ops{numOperands >= 1 ? node->GetOp(0) : nullptr, numOperands >= 2 ? node->GetOp(1) : nullptr,
              numOperands >= 3 ? node->GetOp(2) : nullptr, numOperands >= 4 ? node->GetOp(3) : nullptr}
    {
        assert(HWIntrinsicInfo::RequiresCodegen(id));

        if (vecEltType == TYP_UNDEF)
        {
            vecEltType = InitializeScalarType(node);
        }
    }

private:
    var_types InitializeScalarType(const GenTreeHWIntrinsic* node) const
    {
        assert(category == HW_Category_Scalar);

        const GenTree* op;

        if (HWIntrinsicInfo::BaseTypeFromArg0(id))
        {
            assert(ops[0] != nullptr);
            op = ops[0];
        }
        else if (HWIntrinsicInfo::BaseTypeFromArg1(id))
        {
            assert(ops[1] != nullptr);
            op = ops[1];
        }
        else
        {
            op = node;
        }

        return varActualType(op->GetType());
    }
};

#endif // TARGET_ARM64

#ifdef DEBUG
const char* GetHWIntrinsicIdName(NamedIntrinsic id);
#endif

#endif // FEATURE_HW_INTRINSICS
