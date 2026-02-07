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

    // IMM intrinsics
    // - some SIMD intrinsics requires immediate value (i.e. imm8) to generate instruction
    HW_Category_IMM,

    // Scalar intrinsics
    // - operate over general purpose registers, like crc32, lzcnt, popcnt, etc.
    HW_Category_Scalar,

    // SIMD scalar
    // - operate over vector registers(XMM), but just compute on the first element
    HW_Category_SIMDScalar,

    // Memory access intrinsics
    // - e.g., Avx.Load, Avx.Store, Sse.LoadAligned
    HW_Category_MemoryLoad,
    HW_Category_MemoryStore,

    // Helper intrinsics
    // - do not directly correspond to a instruction, such as Avx.SetAllVector256
    HW_Category_Helper,

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
    HW_Flag_NoFlag = 0,

    // Commutative
    // - if a binary-op intrinsic is commutative (e.g., Add, Multiply), its op1 can be contained
    HW_Flag_Commutative = 0x1,

    // NoCodeGen
    // - should be transformed in the compiler front-end, cannot reach CodeGen
    HW_Flag_NoCodeGen = 0x2,

    // Select base type using the first argument type
    HW_Flag_BaseTypeFromFirstArg = 0x8,

    // Select base type using the second argument type
    HW_Flag_BaseTypeFromSecondArg = 0x10,

    // Indicates compFloatingPointUsed does not need to be set.
    HW_Flag_NoFloatingPointUsed = 0x20,

    // NoJmpTable IMM
    // the imm intrinsic does not need jumptable fallback when it gets non-const argument
    HW_Flag_NoJmpTableIMM = 0x40,

    // Special codegen
    // the intrinsics need special rules in CodeGen,
    // but may be table-driven in the front-end
    HW_Flag_SpecialCodeGen = 0x80,

    // Special import
    // the intrinsics need special rules in importer,
    // but may be table-driven in the back-end
    HW_Flag_SpecialImport = 0x100,

#if defined(TARGET_XARCH)
    // Full range IMM intrinsic
    // - the immediate value is valid on the full range of imm8 (0-255)
    HW_Flag_FullRangeIMM = 0x200,

    // Maybe IMM
    // the intrinsic has either imm or Vector overloads
    HW_Flag_MaybeIMM = 0x400,

    // Copy Upper bits
    // some SIMD scalar intrinsics need the semantics of copying upper bits from the source operand
    HW_Flag_CopyUpperBits = 0x800,

    // Maybe Memory Load/Store
    // - some intrinsics may have pointer overloads but without HW_Category_MemoryLoad/HW_Category_MemoryStore
    HW_Flag_MaybeMemoryLoad  = 0x1000,
    HW_Flag_MaybeMemoryStore = 0x2000,

    // No Read/Modify/Write Semantics
    // the intrinsic doesn't have read/modify/write semantics in two/three-operand form.
    HW_Flag_NoRMWSemantics = 0x4000,

    // NoContainment
    // the intrinsic cannot be handled by containment,
    // all the intrinsic that have explicit memory load/store semantics should have this flag
    HW_Flag_NoContainment = 0x8000,

#elif defined(TARGET_ARM64)
    // The intrinsic has an immediate operand
    // - the value can be (and should be) encoded in a corresponding instruction when the operand value is constant
    HW_Flag_HasImmediateOperand = 0x200,

    // The intrinsic has read/modify/write semantics in multiple-operands form.
    HW_Flag_HasRMWSemantics = 0x400,

    // The intrinsic operates on the lower part of a SIMD register
    // - the upper part of the source registers are ignored
    // - the upper part of the destination register is zeroed
    HW_Flag_SIMDScalar = 0x800,

    // The intrinsic supports some sort of containment analysis
    HW_Flag_SupportsContainment = 0x1000

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
    static int lookupImmUpperBound(NamedIntrinsic intrinsic);
#elif defined(TARGET_ARM64)
    static void LookupImmBounds(
        NamedIntrinsic intrinsic, unsigned vecSize, var_types eltType, int* lowerBound, int* upperBound);
#else
#error Unsupported platform
#endif

    static bool IsImmOp(NamedIntrinsic id, const GenTree* op);

#ifdef TARGET_XARCH
    static bool isAVX2GatherIntrinsic(NamedIntrinsic id);
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

    static bool RequiresCodegen(NamedIntrinsic id)
    {
        return !HasFlag(id, HW_Flag_NoCodeGen);
    }

    static bool SupportsContainment(NamedIntrinsic id)
    {
#if defined(TARGET_XARCH)
        return !HasFlag(id, HW_Flag_NoContainment);
#elif defined(TARGET_ARM64)
        return HasFlag(id, HW_Flag_SupportsContainment);
#else
#error Unsupported platform
#endif
    }

    static bool BaseTypeFromFirstArg(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_BaseTypeFromFirstArg);
    }

    static bool IsFloatingPointUsed(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_NoFloatingPointUsed);
    }

#ifdef TARGET_XARCH
    static bool HasFullRangeImm(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_FullRangeIMM);
    }

    static bool MaybeImm(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_MaybeIMM);
    }

    static bool CopiesUpperBits(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_CopyUpperBits);
    }

    static bool MaybeMemoryLoad(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_MaybeMemoryLoad);
    }

    static bool MaybeMemoryStore(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_MaybeMemoryStore);
    }
#endif

    static bool NoJmpTableImm(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_NoJmpTableIMM);
    }

    static bool BaseTypeFromSecondArg(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_BaseTypeFromSecondArg);
    }

    static bool HasSpecialCodegen(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_SpecialCodeGen);
    }

    static bool HasRMWSemantics(NamedIntrinsic id)
    {
#if defined(TARGET_XARCH)
        return !HasFlag(id, HW_Flag_NoRMWSemantics);
#elif defined(TARGET_ARM64)
        return HasFlag(id, HW_Flag_HasRMWSemantics);
#else
#error Unsupported platform
#endif
    }

    static bool HasSpecialImport(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_SpecialImport);
    }

#ifdef TARGET_ARM64
    static bool SIMDScalar(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_SIMDScalar);
    }

    static bool HasImmediateOperand(NamedIntrinsic id)
    {
        return HasFlag(id, HW_Flag_HasImmediateOperand);
    }
#endif // TARGET_ARM64
};

struct HWIntrinsicSignature final
{
    var_types    retType;
    ClassLayout* retLayout;
    bool         hasThisParam;
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
    var_types           baseType;
    unsigned            numOperands;
    GenTree*            op1;
    GenTree*            op2;
    GenTree*            op3;
    GenTree*            op4;

    HWIntrinsic(const GenTreeHWIntrinsic* node)
        : id(node->GetIntrinsic())
        , category(HWIntrinsicInfo::GetCategory(id))
        , baseType(TYP_UNDEF)
        , numOperands(node->GetNumOps())
        , op1(numOperands >= 1 ? node->GetOp(0) : nullptr)
        , op2(numOperands >= 2 ? node->GetOp(1) : nullptr)
        , op3(numOperands >= 3 ? node->GetOp(2) : nullptr)
        , op4(numOperands >= 4 ? node->GetOp(3) : nullptr)
    {
        assert(HWIntrinsicInfo::RequiresCodegen(id));

        InitializeBaseType(node);
    }

private:
    void InitializeBaseType(const GenTreeHWIntrinsic* node)
    {
        baseType = node->GetSimdBaseType();

        if (baseType == TYP_UNDEF)
        {
            assert(category == HW_Category_Scalar);

            const GenTree* op;

            if (HWIntrinsicInfo::BaseTypeFromFirstArg(id))
            {
                assert(op1 != nullptr);
                op = op1;
            }
            else if (HWIntrinsicInfo::BaseTypeFromSecondArg(id))
            {
                assert(op2 != nullptr);
                op = op2;
            }
            else
            {
                op = node;
            }

            // TODO-MIKE-Review: This stuff is dubious. We don't really know if we need
            // the actual type or the real type. These intrinsics should really use the
            // "SIMD" base type to store the type on import, when we know the signature
            // type.
            baseType = varActualType(op->GetType());
        }
    }
};

#endif // TARGET_ARM64

#ifdef DEBUG
const char* GetHWIntrinsicIdName(NamedIntrinsic id);
#endif

#endif // FEATURE_HW_INTRINSICS
