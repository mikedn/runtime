// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                           ValueNum                                        XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#include "valuenum.h"
#include "ssaconfig.h"

// Windows x86 and Windows ARM/ARM64 may not define _isnanf() but they do define _isnan().
// We will redirect the macros to these other functions if the macro is not defined for the
// platform. This has the side effect of a possible implicit upcasting for arguments passed.
#if (defined(HOST_X86) || defined(HOST_ARM) || defined(HOST_ARM64)) && !defined(HOST_UNIX)

#if !defined(_isnanf)
#define _isnanf _isnan
#endif

#endif // (defined(HOST_X86) || defined(HOST_ARM) || defined(HOST_ARM64)) && !defined(HOST_UNIX)

// We need to use target-specific NaN values when statically compute expressions.
// Otherwise, cross crossgen (e.g. x86_arm) would have different binary outputs
// from native crossgen (i.e. arm_arm) when the NaN got "embedded" into code.
//
// For example, when placing NaN value in r3 register
// x86_arm crossgen would emit
//   movw    r3, 0x00
//   movt    r3, 0xfff8
// while arm_arm crossgen (and JIT) output is
//   movw    r3, 0x00
//   movt    r3, 0x7ff8

struct FloatTraits
{
    //------------------------------------------------------------------------
    // NaN: Return target-specific float NaN value
    //
    // Notes:
    //    "Default" NaN value returned by expression 0.0f / 0.0f on x86/x64 has
    //    different binary representation (0xffc00000) than NaN on
    //    ARM32/ARM64 (0x7fc00000).

    static float NaN()
    {
#if defined(TARGET_XARCH)
        unsigned bits = 0xFFC00000u;
#elif defined(TARGET_ARMARCH)
        unsigned           bits = 0x7FC00000u;
#else
#error Unsupported or unset target architecture
#endif
        float result;
        static_assert(sizeof(bits) == sizeof(result), "sizeof(unsigned) must equal sizeof(float)");
        memcpy(&result, &bits, sizeof(result));
        return result;
    }
};

struct DoubleTraits
{
    //------------------------------------------------------------------------
    // NaN: Return target-specific double NaN value
    //
    // Notes:
    //    "Default" NaN value returned by expression 0.0 / 0.0 on x86/x64 has
    //    different binary representation (0xfff8000000000000) than NaN on
    //    ARM32/ARM64 (0x7ff8000000000000).

    static double NaN()
    {
#if defined(TARGET_XARCH)
        unsigned long long bits = 0xFFF8000000000000ull;
#elif defined(TARGET_ARMARCH)
        unsigned long long bits = 0x7FF8000000000000ull;
#else
#error Unsupported or unset target architecture
#endif
        double result;
        static_assert(sizeof(bits) == sizeof(result), "sizeof(unsigned long long) must equal sizeof(double)");
        memcpy(&result, &bits, sizeof(result));
        return result;
    }
};

//------------------------------------------------------------------------
// FpAdd: Computes value1 + value2
//
// Return Value:
//    TFpTraits::NaN() - If target ARM32/ARM64 and result value is NaN
//    value1 + value2  - Otherwise
//
// Notes:
//    See FloatTraits::NaN() and DoubleTraits::NaN() notes.

template <typename TFp, typename TFpTraits>
TFp FpAdd(TFp value1, TFp value2)
{
#ifdef TARGET_ARMARCH
    // If [value1] is negative infinity and [value2] is positive infinity
    //   the result is NaN.
    // If [value1] is positive infinity and [value2] is negative infinity
    //   the result is NaN.

    if (!_finite(value1) && !_finite(value2))
    {
        if (value1 < 0 && value2 > 0)
        {
            return TFpTraits::NaN();
        }

        if (value1 > 0 && value2 < 0)
        {
            return TFpTraits::NaN();
        }
    }
#endif // TARGET_ARMARCH

    return value1 + value2;
}

//------------------------------------------------------------------------
// FpSub: Computes value1 - value2
//
// Return Value:
//    TFpTraits::NaN() - If target ARM32/ARM64 and result value is NaN
//    value1 - value2  - Otherwise
//
// Notes:
//    See FloatTraits::NaN() and DoubleTraits::NaN() notes.

template <typename TFp, typename TFpTraits>
TFp FpSub(TFp value1, TFp value2)
{
#ifdef TARGET_ARMARCH
    // If [value1] is positive infinity and [value2] is positive infinity
    //   the result is NaN.
    // If [value1] is negative infinity and [value2] is negative infinity
    //   the result is NaN.

    if (!_finite(value1) && !_finite(value2))
    {
        if (value1 > 0 && value2 > 0)
        {
            return TFpTraits::NaN();
        }

        if (value1 < 0 && value2 < 0)
        {
            return TFpTraits::NaN();
        }
    }
#endif // TARGET_ARMARCH

    return value1 - value2;
}

//------------------------------------------------------------------------
// FpMul: Computes value1 * value2
//
// Return Value:
//    TFpTraits::NaN() - If target ARM32/ARM64 and result value is NaN
//    value1 * value2  - Otherwise
//
// Notes:
//    See FloatTraits::NaN() and DoubleTraits::NaN() notes.

template <typename TFp, typename TFpTraits>
TFp FpMul(TFp value1, TFp value2)
{
#ifdef TARGET_ARMARCH
    // From the ECMA standard:
    //
    // If [value1] is zero and [value2] is infinity
    //   the result is NaN.
    // If [value1] is infinity and [value2] is zero
    //   the result is NaN.

    if (value1 == 0 && !_finite(value2) && !_isnan(value2))
    {
        return TFpTraits::NaN();
    }
    if (!_finite(value1) && !_isnan(value1) && value2 == 0)
    {
        return TFpTraits::NaN();
    }
#endif // TARGET_ARMARCH

    return value1 * value2;
}

//------------------------------------------------------------------------
// FpDiv: Computes value1 / value2
//
// Return Value:
//    TFpTraits::NaN() - If target ARM32/ARM64 and result value is NaN
//    value1 / value2  - Otherwise
//
// Notes:
//    See FloatTraits::NaN() and DoubleTraits::NaN() notes.

template <typename TFp, typename TFpTraits>
TFp FpDiv(TFp dividend, TFp divisor)
{
#ifdef TARGET_ARMARCH
    // From the ECMA standard:
    //
    // If [dividend] is zero and [divisor] is zero
    //   the result is NaN.
    // If [dividend] is infinity and [divisor] is infinity
    //   the result is NaN.

    if (dividend == 0 && divisor == 0)
    {
        return TFpTraits::NaN();
    }
    else if (!_finite(dividend) && !_isnan(dividend) && !_finite(divisor) && !_isnan(divisor))
    {
        return TFpTraits::NaN();
    }
#endif // TARGET_ARMARCH

    return dividend / divisor;
}

template <typename TFp, typename TFpTraits>
TFp FpRem(TFp dividend, TFp divisor)
{
    // From the ECMA standard:
    //
    // If [divisor] is zero or [dividend] is infinity
    //   the result is NaN.
    // If [divisor] is infinity,
    //   the result is [dividend]

    if (divisor == 0 || !_finite(dividend))
    {
        return TFpTraits::NaN();
    }
    else if (!_finite(divisor) && !_isnan(divisor))
    {
        return dividend;
    }

    return (TFp)fmod((double)dividend, (double)divisor);
}

//--------------------------------------------------------------------------------
// GetVNFuncForNode: Given a GenTree node, this returns the proper VNFunc to use
// for ValueNumbering
//
// Arguments:
//    node - The GenTree node that we need the VNFunc for.
//
// Return Value:
//    The VNFunc to use for this GenTree node
//
// Notes:
//    Some opers have their semantics affected by GTF flags so they need to be
//    replaced by special VNFunc values:
//      - relops are affected by GTF_UNSIGNED/GTF_RELOP_NAN_UN
//      - ADD/SUB/MUL are affected by GTF_OVERFLOW and GTF_UNSIGNED
//
VNFunc GetVNFuncForNode(GenTree* node)
{
    static const VNFunc relopUnFuncs[]{VNF_LT_UN, VNF_LE_UN, VNF_GE_UN, VNF_GT_UN};
    static_assert_no_msg(GT_LE - GT_LT == 1);
    static_assert_no_msg(GT_GE - GT_LT == 2);
    static_assert_no_msg(GT_GT - GT_LT == 3);

    static const VNFunc binopOvfFuncs[]{VNF_ADD_OVF, VNF_SUB_OVF, VNF_MUL_OVF};
    static const VNFunc binopUnOvfFuncs[]{VNF_ADD_UN_OVF, VNF_SUB_UN_OVF, VNF_MUL_UN_OVF};
    static_assert_no_msg(GT_SUB - GT_ADD == 1);
    static_assert_no_msg(GT_MUL - GT_ADD == 2);

    switch (node->OperGet())
    {
        case GT_EQ:
            if (varTypeIsFloating(node->gtGetOp1()))
            {
                assert(varTypeIsFloating(node->gtGetOp2()));
                assert((node->gtFlags & GTF_RELOP_NAN_UN) == 0);
            }
            break;

        case GT_NE:
            if (varTypeIsFloating(node->gtGetOp1()))
            {
                assert(varTypeIsFloating(node->gtGetOp2()));
                assert((node->gtFlags & GTF_RELOP_NAN_UN) != 0);
            }
            break;

        case GT_LT:
        case GT_LE:
        case GT_GT:
        case GT_GE:
            if (varTypeIsFloating(node->gtGetOp1()))
            {
                assert(varTypeIsFloating(node->gtGetOp2()));
                if ((node->gtFlags & GTF_RELOP_NAN_UN) != 0)
                {
                    return relopUnFuncs[node->OperGet() - GT_LT];
                }
            }
            else
            {
                assert(varTypeIsIntegralOrI(node->gtGetOp1()));
                assert(varTypeIsIntegralOrI(node->gtGetOp2()));
                if (node->IsUnsigned())
                {
                    return relopUnFuncs[node->OperGet() - GT_LT];
                }
            }
            break;

        case GT_ADD:
        case GT_SUB:
        case GT_MUL:
            if (varTypeIsIntegralOrI(node->gtGetOp1()) && node->gtOverflow())
            {
                assert(varTypeIsIntegralOrI(node->gtGetOp2()));
                if (node->IsUnsigned())
                {
                    return binopUnOvfFuncs[node->OperGet() - GT_ADD];
                }
                else
                {
                    return binopOvfFuncs[node->OperGet() - GT_ADD];
                }
            }
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            return VNFuncHWIntrinsic(node->AsHWIntrinsic());
#endif

        case GT_CAST:
            // GT_CAST can overflow but it has special handling and it should not appear here.
            unreached();

        default:
            // Make sure we don't miss an onverflow oper, if a new one is ever added.
            assert(!GenTree::OperMayOverflow(node->OperGet()));
            break;
    }

    return VNFunc(node->OperGet());
}

bool ValueNumStore::VNFuncIsOverflowArithmetic(VNFunc vnf)
{
    static_assert_no_msg(VNF_ADD_OVF + 1 == VNF_SUB_OVF);
    static_assert_no_msg(VNF_SUB_OVF + 1 == VNF_MUL_OVF);
    static_assert_no_msg(VNF_MUL_OVF + 1 == VNF_ADD_UN_OVF);
    static_assert_no_msg(VNF_ADD_UN_OVF + 1 == VNF_SUB_UN_OVF);
    static_assert_no_msg(VNF_SUB_UN_OVF + 1 == VNF_MUL_UN_OVF);

    return VNF_ADD_OVF <= vnf && vnf <= VNF_MUL_UN_OVF;
}

bool ValueNumStore::VNFuncIsNumericCast(VNFunc vnf)
{
    return (vnf == VNF_Cast) || (vnf == VNF_CastOvf);
}

template <>
bool ValueNumStore::IsOverflowIntDiv(int v0, int v1)
{
    return (v1 == -1) && (v0 == INT32_MIN);
}

template <>
bool ValueNumStore::IsOverflowIntDiv(INT64 v0, INT64 v1)
{
    return (v1 == -1) && (v0 == INT64_MIN);
}

template <typename T>
bool ValueNumStore::IsOverflowIntDiv(T v0, T v1)
{
    return false;
}

template <>
bool ValueNumStore::IsIntZero(int v)
{
    return v == 0;
}
template <>
bool ValueNumStore::IsIntZero(unsigned v)
{
    return v == 0;
}
template <>
bool ValueNumStore::IsIntZero(INT64 v)
{
    return v == 0;
}
template <>
bool ValueNumStore::IsIntZero(UINT64 v)
{
    return v == 0;
}
template <typename T>
bool ValueNumStore::IsIntZero(T v)
{
    return false;
}

ValueNumStore::ValueNumStore(Compiler* comp, CompAllocator alloc)
    : m_pComp(comp)
    , m_alloc(alloc)
    , m_nextChunkBase(0)
    , m_fixedPointMapSels(alloc)
    , m_checkedBoundVNs(alloc)
    , m_chunks(alloc)
    , m_intCnsMap(nullptr)
    , m_longCnsMap(nullptr)
    , m_handleMap(nullptr)
    , m_floatCnsMap(nullptr)
    , m_doubleCnsMap(nullptr)
    , m_byrefCnsMap(nullptr)
    , m_VNFunc0Map(nullptr)
    , m_VNFunc1Map(nullptr)
    , m_VNFunc2Map(nullptr)
    , m_VNFunc3Map(nullptr)
    , m_VNFunc4Map(nullptr)
#ifdef DEBUG
    , m_numMapSels(0)
#endif
{
    // We have no current allocation chunks.
    for (unsigned i = 0; i < TYP_COUNT; i++)
    {
        for (unsigned j = CEA_Const; j <= CEA_Count; j++)
        {
            m_curAllocChunk[i][j] = NoChunk;
        }
    }

    for (unsigned i = 0; i < SmallIntConstNum; i++)
    {
        m_VNsForSmallIntConsts[i] = NoVN;
    }
    // We will reserve chunk 0 to hold some special constants, like the constant NULL, the "exception" value, and the
    // "zero map."
    Chunk* specialConstChunk = new (m_alloc) Chunk(m_alloc, &m_nextChunkBase, TYP_REF, CEA_Const);
    // Implicitly allocate 0 ==> NULL, and 1 ==> Exception, 2 ==> ZeroMap.
    specialConstChunk->m_numUsed += SRC_NumSpecialRefConsts;
    assert(m_chunks.Size() == 0);
    m_chunks.Push(specialConstChunk);

    m_mapSelectBudget = (int)JitConfig.JitVNMapSelBudget(); // We cast the unsigned DWORD to a signed int.

    // This value must be non-negative and non-zero, reset the value to DEFAULT_MAP_SELECT_BUDGET if it isn't.
    if (m_mapSelectBudget <= 0)
    {
        m_mapSelectBudget = DEFAULT_MAP_SELECT_BUDGET;
    }
}

//
// Unary EvalOp
//

template <typename T>
T ValueNumStore::EvalOp(VNFunc vnf, T v0)
{
    genTreeOps oper = genTreeOps(vnf);

    // Here we handle unary ops that are the same for all types.
    switch (oper)
    {
        case GT_NEG:
            // Note that GT_NEG is the only valid unary floating point operation
            return -v0;

        default:
            break;
    }

    // Otherwise must be handled by the type specific method
    return EvalOpSpecialized(vnf, v0);
}

template <>
double ValueNumStore::EvalOpSpecialized<double>(VNFunc vnf, double v0)
{
    // Here we handle specialized double unary ops.
    noway_assert(!"EvalOpSpecialized<double> - unary");
    return 0.0;
}

template <>
float ValueNumStore::EvalOpSpecialized<float>(VNFunc vnf, float v0)
{
    // Here we handle specialized float unary ops.
    noway_assert(!"EvalOpSpecialized<float> - unary");
    return 0.0f;
}

template <typename T>
T ValueNumStore::EvalOpSpecialized(VNFunc vnf, T v0)
{
    if (vnf < VNF_Boundary)
    {
        genTreeOps oper = genTreeOps(vnf);

        switch (oper)
        {
            case GT_NEG:
                return -v0;

            case GT_NOT:
                return ~v0;

            case GT_BSWAP16:
            {
                UINT16 v0_unsigned = UINT16(v0);

                v0_unsigned = ((v0_unsigned >> 8) & 0xFF) | ((v0_unsigned << 8) & 0xFF00);
                return T(v0_unsigned);
            }

            case GT_BSWAP:
                if (sizeof(T) == 4)
                {
                    UINT32 v0_unsigned = UINT32(v0);

                    v0_unsigned = ((v0_unsigned >> 24) & 0xFF) | ((v0_unsigned >> 8) & 0xFF00) |
                                  ((v0_unsigned << 8) & 0xFF0000) | ((v0_unsigned << 24) & 0xFF000000);
                    return T(v0_unsigned);
                }
                else if (sizeof(T) == 8)
                {
                    UINT64 v0_unsigned = UINT64(v0);

                    v0_unsigned = ((v0_unsigned >> 56) & 0xFF) | ((v0_unsigned >> 40) & 0xFF00) |
                                  ((v0_unsigned >> 24) & 0xFF0000) | ((v0_unsigned >> 8) & 0xFF000000) |
                                  ((v0_unsigned << 8) & 0xFF00000000) | ((v0_unsigned << 24) & 0xFF0000000000) |
                                  ((v0_unsigned << 40) & 0xFF000000000000) | ((v0_unsigned << 56) & 0xFF00000000000000);
                    return T(v0_unsigned);
                }
                else
                {
                    break; // unknown primitive
                }

            default:
                break;
        }
    }

    noway_assert(!"Unhandled operation in EvalOpSpecialized<T> - unary");
    return v0;
}

//
// Binary EvalOp
//

template <typename T>
T ValueNumStore::EvalOp(VNFunc vnf, T v0, T v1)
{
    // Here we handle the binary ops that are the same for all types.

    // Currently there are none (due to floating point NaN representations)

    // Otherwise must be handled by the type specific method
    return EvalOpSpecialized(vnf, v0, v1);
}

template <>
double ValueNumStore::EvalOpSpecialized<double>(VNFunc vnf, double v0, double v1)
{
    // Here we handle specialized double binary ops.
    if (vnf < VNF_Boundary)
    {
        genTreeOps oper = genTreeOps(vnf);

        // Here we handle
        switch (oper)
        {
            case GT_ADD:
                return FpAdd<double, DoubleTraits>(v0, v1);
            case GT_SUB:
                return FpSub<double, DoubleTraits>(v0, v1);
            case GT_MUL:
                return FpMul<double, DoubleTraits>(v0, v1);
            case GT_DIV:
                return FpDiv<double, DoubleTraits>(v0, v1);
            case GT_MOD:
                return FpRem<double, DoubleTraits>(v0, v1);

            default:
                // For any other value of 'oper', we will assert below
                break;
        }
    }

    noway_assert(!"EvalOpSpecialized<double> - binary");
    return v0;
}

template <>
float ValueNumStore::EvalOpSpecialized<float>(VNFunc vnf, float v0, float v1)
{
    // Here we handle specialized float binary ops.
    if (vnf < VNF_Boundary)
    {
        genTreeOps oper = genTreeOps(vnf);

        // Here we handle
        switch (oper)
        {
            case GT_ADD:
                return FpAdd<float, FloatTraits>(v0, v1);
            case GT_SUB:
                return FpSub<float, FloatTraits>(v0, v1);
            case GT_MUL:
                return FpMul<float, FloatTraits>(v0, v1);
            case GT_DIV:
                return FpDiv<float, FloatTraits>(v0, v1);
            case GT_MOD:
                return FpRem<float, FloatTraits>(v0, v1);

            default:
                // For any other value of 'oper', we will assert below
                break;
        }
    }
    assert(!"EvalOpSpecialized<float> - binary");
    return v0;
}

template <typename T>
T ValueNumStore::EvalOpSpecialized(VNFunc vnf, T v0, T v1)
{
    typedef typename std::make_unsigned<T>::type UT;

    assert((sizeof(T) == 4) || (sizeof(T) == 8));

    // Here we handle binary ops that are the same for all integer types
    if (vnf < VNF_Boundary)
    {
        genTreeOps oper = genTreeOps(vnf);

        switch (oper)
        {
            case GT_ADD:
                return v0 + v1;
            case GT_SUB:
                return v0 - v1;
            case GT_MUL:
                return v0 * v1;

            case GT_DIV:
                assert(IsIntZero(v1) == false);
                assert(IsOverflowIntDiv(v0, v1) == false);
                return v0 / v1;

            case GT_MOD:
                assert(IsIntZero(v1) == false);
                assert(IsOverflowIntDiv(v0, v1) == false);
                return v0 % v1;

            case GT_UDIV:
                assert(IsIntZero(v1) == false);
                return T(UT(v0) / UT(v1));

            case GT_UMOD:
                assert(IsIntZero(v1) == false);
                return T(UT(v0) % UT(v1));

            case GT_AND:
                return v0 & v1;
            case GT_OR:
                return v0 | v1;
            case GT_XOR:
                return v0 ^ v1;

            case GT_LSH:
                if (sizeof(T) == 8)
                {
                    return v0 << (v1 & 0x3F);
                }
                else
                {
                    return v0 << v1;
                }
            case GT_RSH:
                if (sizeof(T) == 8)
                {
                    return v0 >> (v1 & 0x3F);
                }
                else
                {
                    return v0 >> v1;
                }
            case GT_RSZ:
                if (sizeof(T) == 8)
                {
                    return UINT64(v0) >> (v1 & 0x3F);
                }
                else
                {
                    return UINT32(v0) >> v1;
                }
            case GT_ROL:
                if (sizeof(T) == 8)
                {
                    return (v0 << v1) | (UINT64(v0) >> (64 - v1));
                }
                else
                {
                    return (v0 << v1) | (UINT32(v0) >> (32 - v1));
                }

            case GT_ROR:
                if (sizeof(T) == 8)
                {
                    return (v0 << (64 - v1)) | (UINT64(v0) >> v1);
                }
                else
                {
                    return (v0 << (32 - v1)) | (UINT32(v0) >> v1);
                }

            default:
                // For any other value of 'oper', we will assert below
                break;
        }
    }
    else // must be a VNF_ function
    {
        switch (vnf)
        {
            // Here we handle those that are the same for all integer types.
            case VNF_ADD_OVF:
            case VNF_ADD_UN_OVF:
                assert(!CheckedOps::AddOverflows(v0, v1, vnf == VNF_ADD_UN_OVF));
                return v0 + v1;

            case VNF_SUB_OVF:
            case VNF_SUB_UN_OVF:
                assert(!CheckedOps::SubOverflows(v0, v1, vnf == VNF_SUB_UN_OVF));
                return v0 - v1;

            case VNF_MUL_OVF:
            case VNF_MUL_UN_OVF:
                assert(!CheckedOps::MulOverflows(v0, v1, vnf == VNF_MUL_UN_OVF));
                return v0 * v1;

            default:
                // For any other value of 'vnf', we will assert below
                break;
        }
    }

    noway_assert(!"Unhandled operation in EvalOpSpecialized<T> - binary");
    return v0;
}

template <>
int ValueNumStore::EvalComparison<double>(VNFunc vnf, double v0, double v1)
{
    // Here we handle specialized double comparisons.

    // We must check for a NaN argument as they they need special handling
    bool hasNanArg = (_isnan(v0) || _isnan(v1));

    if (vnf < VNF_Boundary)
    {
        genTreeOps oper = genTreeOps(vnf);

        if (hasNanArg)
        {
            // return false in all cases except for GT_NE;
            return (oper == GT_NE);
        }

        switch (oper)
        {
            case GT_EQ:
                return v0 == v1;
            case GT_NE:
                return v0 != v1;
            case GT_GT:
                return v0 > v1;
            case GT_GE:
                return v0 >= v1;
            case GT_LT:
                return v0 < v1;
            case GT_LE:
                return v0 <= v1;
            default:
                // For any other value of 'oper', we will assert below
                break;
        }
    }
    else // must be a VNF_ function
    {
        if (hasNanArg)
        {
            // unordered comparisons with NaNs always return true
            return true;
        }

        switch (vnf)
        {
            case VNF_GT_UN:
                return v0 > v1;
            case VNF_GE_UN:
                return v0 >= v1;
            case VNF_LT_UN:
                return v0 < v1;
            case VNF_LE_UN:
                return v0 <= v1;
            default:
                // For any other value of 'vnf', we will assert below
                break;
        }
    }
    noway_assert(!"Unhandled operation in EvalComparison<double>");
    return 0;
}

template <>
int ValueNumStore::EvalComparison<float>(VNFunc vnf, float v0, float v1)
{
    // Here we handle specialized float comparisons.

    // We must check for a NaN argument as they they need special handling
    bool hasNanArg = (_isnanf(v0) || _isnanf(v1));

    if (vnf < VNF_Boundary)
    {
        genTreeOps oper = genTreeOps(vnf);

        if (hasNanArg)
        {
            // return false in all cases except for GT_NE;
            return (oper == GT_NE);
        }

        switch (oper)
        {
            case GT_EQ:
                return v0 == v1;
            case GT_NE:
                return v0 != v1;
            case GT_GT:
                return v0 > v1;
            case GT_GE:
                return v0 >= v1;
            case GT_LT:
                return v0 < v1;
            case GT_LE:
                return v0 <= v1;
            default:
                // For any other value of 'oper', we will assert below
                break;
        }
    }
    else // must be a VNF_ function
    {
        if (hasNanArg)
        {
            // unordered comparisons with NaNs always return true
            return true;
        }

        switch (vnf)
        {
            case VNF_GT_UN:
                return v0 > v1;
            case VNF_GE_UN:
                return v0 >= v1;
            case VNF_LT_UN:
                return v0 < v1;
            case VNF_LE_UN:
                return v0 <= v1;
            default:
                // For any other value of 'vnf', we will assert below
                break;
        }
    }
    noway_assert(!"Unhandled operation in EvalComparison<float>");
    return 0;
}

template <typename T>
int ValueNumStore::EvalComparison(VNFunc vnf, T v0, T v1)
{
    typedef typename std::make_unsigned<T>::type UT;

    // Here we handle the compare ops that are the same for all integer types.
    if (vnf < VNF_Boundary)
    {
        genTreeOps oper = genTreeOps(vnf);
        switch (oper)
        {
            case GT_EQ:
                return v0 == v1;
            case GT_NE:
                return v0 != v1;
            case GT_GT:
                return v0 > v1;
            case GT_GE:
                return v0 >= v1;
            case GT_LT:
                return v0 < v1;
            case GT_LE:
                return v0 <= v1;
            default:
                // For any other value of 'oper', we will assert below
                break;
        }
    }
    else // must be a VNF_ function
    {
        switch (vnf)
        {
            case VNF_GT_UN:
                return T(UT(v0) > UT(v1));
            case VNF_GE_UN:
                return T(UT(v0) >= UT(v1));
            case VNF_LT_UN:
                return T(UT(v0) < UT(v1));
            case VNF_LE_UN:
                return T(UT(v0) <= UT(v1));
            default:
                // For any other value of 'vnf', we will assert below
                break;
        }
    }
    noway_assert(!"Unhandled operation in EvalComparison<T>");
    return 0;
}

// Create a ValueNum for an exception set singleton for 'x'
//
ValueNum ValueNumStore::VNExcSetSingleton(ValueNum x)
{
    return VNForFunc(TYP_REF, VNF_ExcSetCons, x, VNForEmptyExcSet());
}
// Create a ValueNumPair for an exception set singleton for 'xp'
//
ValueNumPair ValueNumStore::VNPExcSetSingleton(ValueNumPair xp)
{
    return ValueNumPair(VNExcSetSingleton(xp.GetLiberal()), VNExcSetSingleton(xp.GetConservative()));
}

//-------------------------------------------------------------------------------------------
// VNCheckAscending: - Helper method used to verify that elements in an exception set list
//                     are sorted in ascending order.  This method only checks that the
//                     next value in the list has a greater value number than 'item'.
//
// Arguments:
//    item           - The previous item visited in the exception set that we are iterating
//    xs1            - The tail portion of the exception set that we are iterating.
//
// Return Value:
//                   - Returns true when the next value is greater than 'item'
//                   - or whne we have an empty list remaining.
//
// Note:  - Duplicates items aren't allowed in an exception set
//          Used to verify that exception sets are in ascending order when processing them.
//
bool ValueNumStore::VNCheckAscending(ValueNum item, ValueNum xs1)
{
    if (xs1 == VNForEmptyExcSet())
    {
        return true;
    }
    else
    {
        VNFuncApp funcXs1;
        bool      b1 = GetVNFunc(xs1, &funcXs1);
        assert(b1 && funcXs1.m_func == VNF_ExcSetCons); // Precondition: xs1 is an exception set.

        return (item < funcXs1.m_args[0]);
    }
}

//-------------------------------------------------------------------------------------------
// VNExcSetUnion: - Given two exception sets, performs a set Union operation
//                  and returns the value number for the combined exception set.
//
// Arguments:     - The arguments must be applications of VNF_ExcSetCons or the empty set
//    xs0         - The value number of the first exception set
//    xs1         - The value number of the second exception set
//
// Return Value:  - The value number of the combined exception set
//
// Note: - Checks and relies upon the invariant that exceptions sets
//          1. Have no duplicate values
//          2. all elements in an exception set are in sorted order.
//
ValueNum ValueNumStore::VNExcSetUnion(ValueNum xs0, ValueNum xs1)
{
    if (xs0 == VNForEmptyExcSet())
    {
        return xs1;
    }
    else if (xs1 == VNForEmptyExcSet())
    {
        return xs0;
    }
    else
    {
        VNFuncApp funcXs0;
        bool      b0 = GetVNFunc(xs0, &funcXs0);
        assert(b0 && funcXs0.m_func == VNF_ExcSetCons); // Precondition: xs0 is an exception set.
        VNFuncApp funcXs1;
        bool      b1 = GetVNFunc(xs1, &funcXs1);
        assert(b1 && funcXs1.m_func == VNF_ExcSetCons); // Precondition: xs1 is an exception set.
        ValueNum res = NoVN;
        if (funcXs0.m_args[0] < funcXs1.m_args[0])
        {
            assert(VNCheckAscending(funcXs0.m_args[0], funcXs0.m_args[1]));

            // add the lower one (from xs0) to the result, advance xs0
            res = VNForFunc(TYP_REF, VNF_ExcSetCons, funcXs0.m_args[0], VNExcSetUnion(funcXs0.m_args[1], xs1));
        }
        else if (funcXs0.m_args[0] == funcXs1.m_args[0])
        {
            assert(VNCheckAscending(funcXs0.m_args[0], funcXs0.m_args[1]));
            assert(VNCheckAscending(funcXs1.m_args[0], funcXs1.m_args[1]));

            // Equal elements; add one (from xs0) to the result, advance both sets
            res = VNForFunc(TYP_REF, VNF_ExcSetCons, funcXs0.m_args[0],
                            VNExcSetUnion(funcXs0.m_args[1], funcXs1.m_args[1]));
        }
        else
        {
            assert(funcXs0.m_args[0] > funcXs1.m_args[0]);
            assert(VNCheckAscending(funcXs1.m_args[0], funcXs1.m_args[1]));

            // add the lower one (from xs1) to the result, advance xs1
            res = VNForFunc(TYP_REF, VNF_ExcSetCons, funcXs1.m_args[0], VNExcSetUnion(xs0, funcXs1.m_args[1]));
        }

        return res;
    }
}

//--------------------------------------------------------------------------------
// VNPExcSetUnion: - Returns a Value Number Pair that represents the set union
//                   for both parts.
//                   (see VNExcSetUnion for more details)
//
// Notes:   - This method is used to form a Value Number Pair when we
//            want both the Liberal and Conservative Value Numbers
//
ValueNumPair ValueNumStore::VNPExcSetUnion(ValueNumPair xs0vnp, ValueNumPair xs1vnp)
{
    return ValueNumPair(VNExcSetUnion(xs0vnp.GetLiberal(), xs1vnp.GetLiberal()),
                        VNExcSetUnion(xs0vnp.GetConservative(), xs1vnp.GetConservative()));
}

//-------------------------------------------------------------------------------------------
// VNExcSetIntersection: - Given two exception sets, performs a set Intersection operation
//                         and returns the value number for this exception set.
//
// Arguments:     - The arguments must be applications of VNF_ExcSetCons or the empty set
//    xs0         - The value number of the first exception set
//    xs1         - The value number of the second exception set
//
// Return Value:  - The value number of the new exception set.
//                  if the e are no values in common then VNForEmptyExcSet() is returned.
//
// Note: - Checks and relies upon the invariant that exceptions sets
//          1. Have no duplicate values
//          2. all elements in an exception set are in sorted order.
//
ValueNum ValueNumStore::VNExcSetIntersection(ValueNum xs0, ValueNum xs1)
{
    if ((xs0 == VNForEmptyExcSet()) || (xs1 == VNForEmptyExcSet()))
    {
        return VNForEmptyExcSet();
    }
    else
    {
        VNFuncApp funcXs0;
        bool      b0 = GetVNFunc(xs0, &funcXs0);
        assert(b0 && funcXs0.m_func == VNF_ExcSetCons); // Precondition: xs0 is an exception set.
        VNFuncApp funcXs1;
        bool      b1 = GetVNFunc(xs1, &funcXs1);
        assert(b1 && funcXs1.m_func == VNF_ExcSetCons); // Precondition: xs1 is an exception set.
        ValueNum res = NoVN;

        if (funcXs0.m_args[0] < funcXs1.m_args[0])
        {
            assert(VNCheckAscending(funcXs0.m_args[0], funcXs0.m_args[1]));
            res = VNExcSetIntersection(funcXs0.m_args[1], xs1);
        }
        else if (funcXs0.m_args[0] == funcXs1.m_args[0])
        {
            assert(VNCheckAscending(funcXs0.m_args[0], funcXs0.m_args[1]));
            assert(VNCheckAscending(funcXs1.m_args[0], funcXs1.m_args[1]));

            // Equal elements; Add it to the result.
            res = VNForFunc(TYP_REF, VNF_ExcSetCons, funcXs0.m_args[0],
                            VNExcSetIntersection(funcXs0.m_args[1], funcXs1.m_args[1]));
        }
        else
        {
            assert(funcXs0.m_args[0] > funcXs1.m_args[0]);
            assert(VNCheckAscending(funcXs1.m_args[0], funcXs1.m_args[1]));
            res = VNExcSetIntersection(xs0, funcXs1.m_args[1]);
        }

        return res;
    }
}

//--------------------------------------------------------------------------------
// VNPExcSetIntersection: - Returns a Value Number Pair that represents the set
//                 intersection for both parts.
//                 (see VNExcSetIntersection for more details)
//
// Notes:   - This method is used to form a Value Number Pair when we
//            want both the Liberal and Conservative Value Numbers
//
ValueNumPair ValueNumStore::VNPExcSetIntersection(ValueNumPair xs0vnp, ValueNumPair xs1vnp)
{
    return ValueNumPair(VNExcSetIntersection(xs0vnp.GetLiberal(), xs1vnp.GetLiberal()),
                        VNExcSetIntersection(xs0vnp.GetConservative(), xs1vnp.GetConservative()));
}

//----------------------------------------------------------------------------------------
// VNExcIsSubset     - Given two exception sets, returns true when vnCandidateSet is a
//                     subset of vnFullSet
//
// Arguments:        - The arguments must be applications of VNF_ExcSetCons or the empty set
//    vnFullSet      - The value number of the 'full' exception set
//    vnCandidateSet - The value number of the 'candidate' exception set
//
// Return Value:     - Returns true if every singleton ExcSet value in the vnCandidateSet
//                     is also present in the vnFullSet.
//
// Note: - Checks and relies upon the invariant that exceptions sets
//          1. Have no duplicate values
//          2. all elements in an exception set are in sorted order.
//
bool ValueNumStore::VNExcIsSubset(ValueNum vnFullSet, ValueNum vnCandidateSet)
{
    if (vnCandidateSet == VNForEmptyExcSet())
    {
        return true;
    }
    else if ((vnFullSet == VNForEmptyExcSet()) || (vnFullSet == ValueNumStore::NoVN))
    {
        return false;
    }

    VNFuncApp funcXsFull;
    bool      b0 = GetVNFunc(vnFullSet, &funcXsFull);
    assert(b0 && funcXsFull.m_func == VNF_ExcSetCons); // Precondition: vnFullSet is an exception set.
    VNFuncApp funcXsCand;
    bool      b1 = GetVNFunc(vnCandidateSet, &funcXsCand);
    assert(b1 && funcXsCand.m_func == VNF_ExcSetCons); // Precondition: vnCandidateSet is an exception set.

    ValueNum vnFullSetPrev = VNForNull();
    ValueNum vnCandSetPrev = VNForNull();

    ValueNum vnFullSetRemainder = funcXsFull.m_args[1];
    ValueNum vnCandSetRemainder = funcXsCand.m_args[1];

    while (true)
    {
        ValueNum vnFullSetItem = funcXsFull.m_args[0];
        ValueNum vnCandSetItem = funcXsCand.m_args[0];

        // Enforce that both sets are sorted by increasing ValueNumbers
        //
        assert(vnFullSetItem > vnFullSetPrev);
        assert(vnCandSetItem >= vnCandSetPrev); // equal when we didn't advance the candidate set

        if (vnFullSetItem > vnCandSetItem)
        {
            // The Full set does not contain the vnCandSetItem
            return false;
        }
        // now we must have (vnFullSetItem <= vnCandSetItem)

        // When we have a matching value we advance the candidate set
        //
        if (vnFullSetItem == vnCandSetItem)
        {
            // Have we finished matching?
            //
            if (vnCandSetRemainder == VNForEmptyExcSet())
            {
                // We matched every item in the candidate set'
                //
                return true;
            }

            // Advance the candidate set
            //
            b1 = GetVNFunc(vnCandSetRemainder, &funcXsCand);
            assert(b1 && funcXsCand.m_func == VNF_ExcSetCons); // Precondition: vnCandSetRemainder is an exception set.
            vnCandSetRemainder = funcXsCand.m_args[1];
        }

        if (vnFullSetRemainder == VNForEmptyExcSet())
        {
            // No more items are left in the full exception set
            return false;
        }

        //
        // We will advance the full set
        //
        b0 = GetVNFunc(vnFullSetRemainder, &funcXsFull);
        assert(b0 && funcXsFull.m_func == VNF_ExcSetCons); // Precondition: vnFullSetRemainder is an exception set.
        vnFullSetRemainder = funcXsFull.m_args[1];

        vnFullSetPrev = vnFullSetItem;
        vnCandSetPrev = vnCandSetItem;
    }
}

//-------------------------------------------------------------------------------------
// VNUnpackExc: - Given a ValueNum 'vnWx, return via write back parameters both
//                the normal and the exception set components.
//
// Arguments:
//    vnWx        - A value number, it may have an exception set
//    pvn         - a write back pointer to the normal value portion of 'vnWx'
//    pvnx        - a write back pointer for the exception set portion of 'vnWx'
//
// Return Values: - This method signature is void but returns two values using
//                  the write back parameters.
//
// Note: When 'vnWx' does not have an exception set, the orginal value is the
//       normal value and is written to 'pvn' and VNForEmptyExcSet() is
//       written to 'pvnx'.
//       When we have an exception set 'vnWx' will be a VN func with m_func
//       equal to VNF_ValWithExc.
//
void ValueNumStore::VNUnpackExc(ValueNum vnWx, ValueNum* pvn, ValueNum* pvnx)
{
    assert(vnWx != NoVN);
    VNFuncApp funcApp;
    if (GetVNFunc(vnWx, &funcApp) && funcApp.m_func == VNF_ValWithExc)
    {
        *pvn  = funcApp.m_args[0];
        *pvnx = funcApp.m_args[1];
    }
    else
    {
        *pvn  = vnWx;
        *pvnx = VNForEmptyExcSet();
    }
}

//-------------------------------------------------------------------------------------
// VNPUnpackExc: - Given a ValueNumPair 'vnpWx, return via write back parameters
//                 both the normal and the exception set components.
//                 (see VNUnpackExc for more details)
//
// Notes:   - This method is used to form a Value Number Pair when we
//            want both the Liberal and Conservative Value Numbers
//
void ValueNumStore::VNPUnpackExc(ValueNumPair vnpWx, ValueNumPair* pvnp, ValueNumPair* pvnpx)
{
    VNUnpackExc(vnpWx.GetLiberal(), pvnp->GetLiberalAddr(), pvnpx->GetLiberalAddr());
    VNUnpackExc(vnpWx.GetConservative(), pvnp->GetConservativeAddr(), pvnpx->GetConservativeAddr());
}

//-------------------------------------------------------------------------------------
// VNUnionExcSet: - Given a ValueNum 'vnWx' and a current 'vnExcSet', return an
//                  exception set of the Union of both exception sets.
//
// Arguments:
//    vnWx        - A value number, it may have an exception set
//    vnExcSet    - The value number for the current exception set
//
// Return Values: - The value number of the Union of the exception set of 'vnWx'
//                  with the current 'vnExcSet'.
//
// Note: When 'vnWx' does not have an exception set, 'vnExcSet' is returned.
//
ValueNum ValueNumStore::VNUnionExcSet(ValueNum vnWx, ValueNum vnExcSet)
{
    assert(vnWx != NoVN);
    VNFuncApp funcApp;
    if (GetVNFunc(vnWx, &funcApp) && funcApp.m_func == VNF_ValWithExc)
    {
        vnExcSet = VNExcSetUnion(funcApp.m_args[1], vnExcSet);
    }
    return vnExcSet;
}

//-------------------------------------------------------------------------------------
// VNPUnionExcSet: - Given a ValueNum 'vnWx' and a current 'excSet', return an
//                   exception set of the Union of both exception sets.
//                   (see VNUnionExcSet for more details)
//
// Notes:   - This method is used to form a Value Number Pair when we
//            want both the Liberal and Conservative Value Numbers
//
ValueNumPair ValueNumStore::VNPUnionExcSet(ValueNumPair vnpWx, ValueNumPair vnpExcSet)
{
    return ValueNumPair(VNUnionExcSet(vnpWx.GetLiberal(), vnpExcSet.GetLiberal()),
                        VNUnionExcSet(vnpWx.GetConservative(), vnpExcSet.GetConservative()));
}

//--------------------------------------------------------------------------------
// VNNormalValue: - Returns a Value Number that represents the result for the
//                  normal (non-exceptional) evaluation for the expression.
//
// Arguments:
//    vn         - The Value Number for the expression, including any excSet.
//                 This excSet is an optional item and represents the set of
//                 possible exceptions for the expression.
//
// Return Value:
//               - The Value Number for the expression without the exception set.
//                 This can be the orginal 'vn', when there are no exceptions.
//
// Notes:        - Whenever we have an exception set the Value Number will be
//                 a VN func with VNF_ValWithExc.
//                 This VN func has the normal value as m_args[0]
//
ValueNum ValueNumStore::VNNormalValue(ValueNum vn)
{
    VNFuncApp funcApp;
    if (GetVNFunc(vn, &funcApp) && funcApp.m_func == VNF_ValWithExc)
    {
        return funcApp.m_args[0];
    }
    else
    {
        return vn;
    }
}

//------------------------------------------------------------------------------------
// VNMakeNormalUnique:
//
// Arguments:
//    vn         - The current Value Number for the expression, including any excSet.
//                 This excSet is an optional item and represents the set of
//                 possible exceptions for the expression.
//
// Return Value:
//               - The normal value is set to a new unique VN, while keeping
//                 the excSet (if any)
//
ValueNum ValueNumStore::VNMakeNormalUnique(ValueNum orig)
{
    // First Unpack the existing Norm,Exc for 'elem'
    ValueNum vnOrigNorm;
    ValueNum vnOrigExcSet;
    VNUnpackExc(orig, &vnOrigNorm, &vnOrigExcSet);

    // Replace the normal value with a unique ValueNum
    ValueNum vnUnique = VNForExpr(m_pComp->compCurBB, TypeOfVN(vnOrigNorm));

    // Keep any ExcSet from 'elem'
    return VNWithExc(vnUnique, vnOrigExcSet);
}

//--------------------------------------------------------------------------------
// VNPMakeNormalUniquePair:
//
// Arguments:
//    vnp         - The Value Number Pair for the expression, including any excSet.
//
// Return Value:
//               - The normal values are set to a new unique VNs, while keeping
//                 the excSets (if any)
//
ValueNumPair ValueNumStore::VNPMakeNormalUniquePair(ValueNumPair vnp)
{
    return ValueNumPair(VNMakeNormalUnique(vnp.GetLiberal()), VNMakeNormalUnique(vnp.GetConservative()));
}

//--------------------------------------------------------------------------------
// VNNormalValue: - Returns a Value Number that represents the result for the
//                  normal (non-exceptional) evaluation for the expression.
//
// Arguments:
//    vnp        - The Value Number Pair for the expression, including any excSet.
//                 This excSet is an optional item and represents the set of
//                 possible exceptions for the expression.
//    vnk        - The ValueNumKind either liberal or conservative
//
// Return Value:
//               - The Value Number for the expression without the exception set.
//                 This can be the orginal 'vn', when there are no exceptions.
//
// Notes:        - Whenever we have an exception set the Value Number will be
//                 a VN func with VNF_ValWithExc.
//                 This VN func has the normal value as m_args[0]
//
ValueNum ValueNumStore::VNNormalValue(ValueNumPair vnp, ValueNumKind vnk)
{
    return VNNormalValue(vnp.Get(vnk));
}

//--------------------------------------------------------------------------------
// VNPNormalPair: - Returns a Value Number Pair that represents the result for the
//                  normal (non-exceptional) evaluation for the expression.
//                  (see VNNormalValue for more details)
// Arguments:
//    vnp         - The Value Number Pair for the expression, including any excSet.
//
// Notes:         - This method is used to form a Value Number Pair using both
//                  the Liberal and Conservative Value Numbers normal (non-exceptional)
//
ValueNumPair ValueNumStore::VNPNormalPair(ValueNumPair vnp)
{
    return ValueNumPair(VNNormalValue(vnp.GetLiberal()), VNNormalValue(vnp.GetConservative()));
}

//---------------------------------------------------------------------------
// VNExceptionSet: - Returns a Value Number that represents the set of possible
//                   exceptions that could be encountered for the expression.
//
// Arguments:
//    vn         - The Value Number for the expression, including any excSet.
//                 This excSet is an optional item and represents the set of
//                 possible exceptions for the expression.
//
// Return Value:
//               - The Value Number for the set of exceptions of the expression.
//                 If the 'vn' has no exception set then a special Value Number
//                 representing the empty exception set is returned.
//
// Notes:        - Whenever we have an exception set the Value Number will be
//                 a VN func with VNF_ValWithExc.
//                 This VN func has the exception set as m_args[1]
//
ValueNum ValueNumStore::VNExceptionSet(ValueNum vn)
{
    VNFuncApp funcApp;
    if (GetVNFunc(vn, &funcApp) && funcApp.m_func == VNF_ValWithExc)
    {
        return funcApp.m_args[1];
    }
    else
    {
        return VNForEmptyExcSet();
    }
}

//--------------------------------------------------------------------------------
// VNPExceptionSet:    - Returns a Value Number Pair that represents the set of possible
//                 exceptions that could be encountered for the expression.
//                 (see VNExceptionSet for more details)
//
// Notes:        - This method is used to form a Value Number Pair when we
//                 want both the Liberal and Conservative Value Numbers
//
ValueNumPair ValueNumStore::VNPExceptionSet(ValueNumPair vnp)
{
    return ValueNumPair(VNExceptionSet(vnp.GetLiberal()), VNExceptionSet(vnp.GetConservative()));
}

//---------------------------------------------------------------------------
// VNWithExc:    - Returns a Value Number that also can have both a normal value
//                 as well as am exception set.
//
// Arguments:
//    vn         - The current Value Number for the expression, it may include
//                 an exception set.
//    excSet     - The Value Number representing the new exception set that
//                 is to be added to any exceptions already present in 'vn'
//
// Return Value:
//               - The new Value Number for the combination the two inputs.
//                 If the 'excSet' is the special Value Number representing
//                 the empty exception set then 'vn' is returned.
//
// Notes:        - We use a Set Union operation, 'VNExcSetUnion', to add any
//                 new exception items from  'excSet' to the existing set.
//
ValueNum ValueNumStore::VNWithExc(ValueNum vn, ValueNum excSet)
{
    if (excSet == VNForEmptyExcSet())
    {
        return vn;
    }
    else
    {
        ValueNum vnNorm;
        ValueNum vnX;
        VNUnpackExc(vn, &vnNorm, &vnX);
        return VNForFunc(TypeOfVN(vnNorm), VNF_ValWithExc, vnNorm, VNExcSetUnion(vnX, excSet));
    }
}

//--------------------------------------------------------------------------------
// VNPWithExc:   - Returns a Value Number Pair that also can have both a normal value
//                 as well as am exception set.
//                 (see VNWithExc for more details)
//
// Notes:        = This method is used to form a Value Number Pair when we
//                 want both the Liberal and Conservative Value Numbers
//
ValueNumPair ValueNumStore::VNPWithExc(ValueNumPair vnp, ValueNumPair excSetVNP)
{
    return ValueNumPair(VNWithExc(vnp.GetLiberal(), excSetVNP.GetLiberal()),
                        VNWithExc(vnp.GetConservative(), excSetVNP.GetConservative()));
}

bool ValueNumStore::IsKnownNonNull(ValueNum vn)
{
    if (vn == NoVN)
    {
        return false;
    }
    VNFuncApp funcAttr;
    return GetVNFunc(vn, &funcAttr) && ((VNFuncAttribs(funcAttr.m_func) & VNFOA_KnownNonNull) != 0);
}

bool ValueNumStore::IsSharedStatic(ValueNum vn)
{
    if (vn == NoVN)
    {
        return false;
    }
    VNFuncApp funcAttr;
    return GetVNFunc(vn, &funcAttr) && ((VNFuncAttribs(funcAttr.m_func) & VNFOA_SharedStatic) != 0);
}

ValueNumStore::Chunk::Chunk(CompAllocator alloc, ValueNum* pNextBaseVN, var_types typ, ChunkExtraAttribs attribs)
    : m_defs(nullptr), m_numUsed(0), m_baseVN(*pNextBaseVN), m_typ(typ), m_attribs(attribs)
{
    // Allocate "m_defs" here, according to the typ/attribs pair.
    switch (attribs)
    {
        case CEA_NotAField:
            break; // Nothing to do.
        case CEA_Const:
            switch (typ)
            {
                case TYP_INT:
                    m_defs = alloc.allocate<Alloc<TYP_INT>::Type>(ChunkSize);
                    break;
                case TYP_FLOAT:
                    m_defs = alloc.allocate<Alloc<TYP_FLOAT>::Type>(ChunkSize);
                    break;
                case TYP_LONG:
                    m_defs = alloc.allocate<Alloc<TYP_LONG>::Type>(ChunkSize);
                    break;
                case TYP_DOUBLE:
                    m_defs = alloc.allocate<Alloc<TYP_DOUBLE>::Type>(ChunkSize);
                    break;
                case TYP_BYREF:
                    m_defs = alloc.allocate<Alloc<TYP_BYREF>::Type>(ChunkSize);
                    break;
                case TYP_REF:
                    // We allocate space for a single REF constant, NULL, so we can access these values uniformly.
                    // Since this value is always the same, we represent it as a static.
                    m_defs = &s_specialRefConsts[0];
                    break; // Nothing to do.
                default:
                    assert(false); // Should not reach here.
            }
            break;

        case CEA_Handle:
            m_defs = alloc.allocate<VNHandle>(ChunkSize);
            break;

        case CEA_Func0:
            m_defs = alloc.allocate<VNFunc>(ChunkSize);
            break;

        case CEA_Func1:
            m_defs = alloc.allocate<VNDefFunc1Arg>(ChunkSize);
            break;
        case CEA_Func2:
            m_defs = alloc.allocate<VNDefFunc2Arg>(ChunkSize);
            break;
        case CEA_Func3:
            m_defs = alloc.allocate<VNDefFunc3Arg>(ChunkSize);
            break;
        case CEA_Func4:
            m_defs = alloc.allocate<VNDefFunc4Arg>(ChunkSize);
            break;
        default:
            unreached();
    }
    *pNextBaseVN += ChunkSize;
}

ValueNumStore::Chunk* ValueNumStore::GetAllocChunk(var_types typ, ChunkExtraAttribs attribs)
{
    unsigned index = attribs;
    ChunkNum cn    = m_curAllocChunk[typ][index];
    if (cn != NoChunk)
    {
        Chunk* chunk = m_chunks.Get(cn);
        if (chunk->m_numUsed < ChunkSize)
        {
            return chunk;
        }
    }
    // Otherwise, must allocate a new one.
    Chunk* chunk                = new (m_alloc) Chunk(m_alloc, &m_nextChunkBase, typ, attribs);
    m_curAllocChunk[typ][index] = m_chunks.Size();
    m_chunks.Push(chunk);
    return chunk;
}

//------------------------------------------------------------------------
// VnForConst: Return value number for a constant.
//
// Arguments:
//   cnsVal - `T` constant to return a VN for;
//   numMap - VNMap<T> map where `T` type constants should be stored;
//   varType - jit type for the `T`: TYP_INT for int, TYP_LONG for long etc.
//
// Return value:
//    value number for the given constant.
//
// Notes:
//   First try to find an existing VN for `cnsVal` in `numMap`,
//   if it fails then allocate a new `varType` chunk and return that.
//
template <typename T, typename NumMap>
ValueNum ValueNumStore::VnForConst(T cnsVal, NumMap* numMap, var_types varType)
{
    ValueNum res;
    if (numMap->Lookup(cnsVal, &res))
    {
        return res;
    }
    else
    {
        Chunk*   chunk               = GetAllocChunk(varType, CEA_Const);
        unsigned offsetWithinChunk   = chunk->AllocVN();
        res                          = chunk->m_baseVN + offsetWithinChunk;
        T* chunkDefs                 = static_cast<T*>(chunk->m_defs);
        chunkDefs[offsetWithinChunk] = cnsVal;
        numMap->Set(cnsVal, res);
        return res;
    }
}

ValueNum ValueNumStore::VNForIntCon(INT32 cnsVal)
{
    if (IsSmallIntConst(cnsVal))
    {
        unsigned ind = cnsVal - SmallIntConstMin;
        ValueNum vn  = m_VNsForSmallIntConsts[ind];
        if (vn != NoVN)
        {
            return vn;
        }
        vn                          = VnForConst(cnsVal, GetIntCnsMap(), TYP_INT);
        m_VNsForSmallIntConsts[ind] = vn;
        return vn;
    }
    else
    {
        return VnForConst(cnsVal, GetIntCnsMap(), TYP_INT);
    }
}

ValueNum ValueNumStore::VNForLongCon(INT64 cnsVal)
{
    return VnForConst(cnsVal, GetLongCnsMap(), TYP_LONG);
}

ValueNum ValueNumStore::VNForFloatCon(float cnsVal)
{
    return VnForConst(cnsVal, GetFloatCnsMap(), TYP_FLOAT);
}

ValueNum ValueNumStore::VNForDoubleCon(double cnsVal)
{
    return VnForConst(cnsVal, GetDoubleCnsMap(), TYP_DOUBLE);
}

ValueNum ValueNumStore::VNForByrefCon(target_size_t cnsVal)
{
    return VnForConst(cnsVal, GetByrefCnsMap(), TYP_BYREF);
}

ValueNum ValueNumStore::VNForBitCastOper(var_types castToType)
{
    assert(castToType != TYP_STRUCT);

    uint32_t packedCastType = INT32(castToType) << INT32(VCA_BitCount);
    assert((packedCastType & INT32(VCA_ReservedBits)) == 0);

    return VNForIntCon(static_cast<int32_t>(packedCastType));
}

ValueNum ValueNumStore::VNForCastOper(var_types castToType, bool castFromUnsigned)
{
    assert(castToType != TYP_STRUCT);

    uint32_t packedCastType = static_cast<uint32_t>(castToType) << VCA_BitCount;
    assert((packedCastType & VCA_ReservedBits) == 0);

    if (castFromUnsigned)
    {
        packedCastType |= VCA_UnsignedSrc;
    }

    return VNForIntCon(static_cast<int32_t>(packedCastType));
}

void ValueNumStore::GetCastOperFromVN(ValueNum vn, var_types* pCastToType, bool* pSrcIsUnsigned)
{
    assert(pCastToType != nullptr);
    assert(pSrcIsUnsigned != nullptr);
    assert(IsVNInt32Constant(vn));

    int value = GetConstantInt32(vn);
    assert(value >= 0);

    *pSrcIsUnsigned = (value & INT32(VCA_UnsignedSrc)) != 0;
    *pCastToType    = var_types(value >> INT32(VCA_BitCount));

    assert(VNForCastOper(*pCastToType, *pSrcIsUnsigned) == vn);
}

ValueNum ValueNumStore::VNForHandle(ssize_t cnsVal, GenTreeFlags handleFlags)
{
    assert((handleFlags & ~GTF_ICON_HDL_MASK) == 0);

    ValueNum res;
    VNHandle handle;
    VNHandle::Initialize(&handle, cnsVal, handleFlags);
    if (GetHandleMap()->Lookup(handle, &res))
    {
        return res;
    }
    else
    {
        Chunk*   c                                           = GetAllocChunk(TYP_I_IMPL, CEA_Handle);
        unsigned offsetWithinChunk                           = c->AllocVN();
        res                                                  = c->m_baseVN + offsetWithinChunk;
        static_cast<VNHandle*>(c->m_defs)[offsetWithinChunk] = handle;
        GetHandleMap()->Set(handle, res);
        return res;
    }
}

ValueNum ValueNumStore::VNForTypeNum(unsigned typeNum)
{
    return VNForHandle(static_cast<ssize_t>(typeNum), GTF_ICON_CLASS_HDL);
}

ValueNum ValueNumStore::VNZeroForType(var_types type)
{
    switch (type)
    {
        case TYP_BOOL:
        case TYP_BYTE:
        case TYP_UBYTE:
        case TYP_SHORT:
        case TYP_USHORT:
        case TYP_INT:
        case TYP_UINT:
            return VNForIntCon(0);
        case TYP_LONG:
        case TYP_ULONG:
            return VNForLongCon(0);
        case TYP_FLOAT:
            return VNForFloatCon(0.0f);
        case TYP_DOUBLE:
            return VNForDoubleCon(0.0);
        case TYP_REF:
            return VNForNull();
        case TYP_BYREF:
            return VNForByrefCon(0);
        case TYP_STRUCT:
            return VNForZeroMap();
#ifdef FEATURE_SIMD
        case TYP_SIMD8:
        case TYP_SIMD12:
        case TYP_SIMD16:
        case TYP_SIMD32:
            return VNForLongCon(0);
#endif
        default:
            unreached();
    }
}

ValueNum ValueNumStore::VNForZeroMap()
{
    if (m_zeroMap == NoVN)
    {
        m_zeroMap = VNForFunc(TYP_STRUCT, VNF_ZeroMap);
    }

    return m_zeroMap;
}

// Returns the value number for one of the given "typ".
// It returns NoVN for a "typ" that has no one value, such as TYP_REF.
ValueNum ValueNumStore::VNOneForType(var_types typ)
{
    switch (typ)
    {
        case TYP_BOOL:
        case TYP_BYTE:
        case TYP_UBYTE:
        case TYP_SHORT:
        case TYP_USHORT:
        case TYP_INT:
        case TYP_UINT:
            return VNForIntCon(1);
        case TYP_LONG:
        case TYP_ULONG:
            return VNForLongCon(1);
        case TYP_FLOAT:
            return VNForFloatCon(1.0f);
        case TYP_DOUBLE:
            return VNForDoubleCon(1.0);

        default:
            return NoVN;
    }
}

class Object* ValueNumStore::s_specialRefConsts[] = {nullptr, nullptr, nullptr};

//----------------------------------------------------------------------------------------
//  VNForFunc  - Returns the ValueNum associated with 'func'
//               There is a one-to-one relationship between the ValueNum and 'func'
//
// Arguments:
//    typ            - The type of the resulting ValueNum produced by 'func'
//    func           - Any nullary VNFunc
//
// Return Value:     - Returns the ValueNum associated with 'func'
//
// Note: - This method only handles Nullary operators (i.e., symbolic constants).
//
ValueNum ValueNumStore::VNForFunc(var_types typ, VNFunc func)
{
    assert(VNFuncArityIsLegal(func, 0));
    assert(func != VNF_NotAField);

    ValueNum resultVN;

    // Have we already assigned a ValueNum for 'func' ?
    //
    if (!GetVNFunc0Map()->Lookup(func, &resultVN))
    {
        // Allocate a new ValueNum for 'func'
        Chunk*   c                                         = GetAllocChunk(typ, CEA_Func0);
        unsigned offsetWithinChunk                         = c->AllocVN();
        resultVN                                           = c->m_baseVN + offsetWithinChunk;
        static_cast<VNFunc*>(c->m_defs)[offsetWithinChunk] = func;
        GetVNFunc0Map()->Set(func, resultVN);
    }
    return resultVN;
}

//----------------------------------------------------------------------------------------
//  VNForFunc  - Returns the ValueNum associated with 'func'('arg0VN')
//               There is a one-to-one relationship between the ValueNum
//               and 'func'('arg0VN')
//
// Arguments:
//    typ            - The type of the resulting ValueNum produced by 'func'
//    func           - Any unary VNFunc
//    arg0VN         - The ValueNum of the argument to 'func'
//
// Return Value:     - Returns the ValueNum associated with 'func'('arg0VN')
//
// Note: - This method only handles Unary operators
//
ValueNum ValueNumStore::VNForFunc(var_types typ, VNFunc func, ValueNum arg0VN)
{
    assert(func != VNF_MemOpaque);
    assert(arg0VN == VNNormalValue(arg0VN)); // Arguments don't carry exceptions.

    // Try to perform constant-folding.
    if (CanEvalForConstantArgs(func) && IsVNConstant(arg0VN))
    {
        return EvalFuncForConstantArgs(typ, func, arg0VN);
    }

    ValueNum resultVN;

    // Have we already assigned a ValueNum for 'func'('arg0VN') ?
    VNDefFunc1Arg fstruct(func, arg0VN);
    if (!GetVNFunc1Map()->Lookup(fstruct, &resultVN))
    {
        // Otherwise, Allocate a new ValueNum for 'func'('arg0VN')
        //
        Chunk*   c                                                = GetAllocChunk(typ, CEA_Func1);
        unsigned offsetWithinChunk                                = c->AllocVN();
        resultVN                                                  = c->m_baseVN + offsetWithinChunk;
        static_cast<VNDefFunc1Arg*>(c->m_defs)[offsetWithinChunk] = fstruct;
        // Record 'resultVN' in the Func1Map
        GetVNFunc1Map()->Set(fstruct, resultVN);
    }
    return resultVN;
}

//----------------------------------------------------------------------------------------
//  VNForFunc  - Returns the ValueNum associated with 'func'('arg0VN','arg1VN')
//               There is a one-to-one relationship between the ValueNum
//               and 'func'('arg0VN','arg1VN')
//
// Arguments:
//    typ            - The type of the resulting ValueNum produced by 'func'
//    func           - Any binary VNFunc
//    arg0VN         - The ValueNum of the first argument to 'func'
//    arg1VN         - The ValueNum of the second argument to 'func'
//
// Return Value:     - Returns the ValueNum associated with 'func'('arg0VN','arg1VN')
//
// Note: - This method only handles Binary operators
//
ValueNum ValueNumStore::VNForFunc(var_types typ, VNFunc func, ValueNum arg0VN, ValueNum arg1VN)
{
    assert(arg0VN != NoVN && arg1VN != NoVN);
    assert(arg0VN == VNNormalValue(arg0VN)); // Arguments carry no exceptions.
    assert(arg1VN == VNNormalValue(arg1VN)); // Arguments carry no exceptions.

    assert(VNFuncArityIsLegal(func, 2));
    assert(func != VNF_MapSelect); // Precondition: use the special function VNForMapSelect defined for that.

    ValueNum resultVN;

    // When both operands are constants we can usually perform constant-folding.
    //
    if (CanEvalForConstantArgs(func) && IsVNConstant(arg0VN) && IsVNConstant(arg1VN))
    {
        bool canFold = true; // Normally we will be able to fold this 'func'

        // Special case for VNF_Cast of constant handles
        // Don't allow an eval/fold of a GT_CAST(non-I_IMPL, Handle)
        //
        if (VNFuncIsNumericCast(func) && (typ != TYP_I_IMPL) && IsVNHandle(arg0VN))
        {
            canFold = false;
        }

        // It is possible for us to have mismatched types (see Bug 750863)
        // We don't try to fold a binary operation when one of the constant operands
        // is a floating-point constant and the other is not, except for casts.
        // For casts, the second operand just carries the information about the source.

        var_types arg0VNtyp      = TypeOfVN(arg0VN);
        bool      arg0IsFloating = varTypeIsFloating(arg0VNtyp);

        var_types arg1VNtyp      = TypeOfVN(arg1VN);
        bool      arg1IsFloating = varTypeIsFloating(arg1VNtyp);

        if (!VNFuncIsNumericCast(func) && (arg0IsFloating != arg1IsFloating))
        {
            canFold = false;
        }

        if (typ == TYP_BYREF)
        {
            // We don't want to fold expressions that produce TYP_BYREF
            canFold = false;
        }

        bool shouldFold = canFold;

        if (canFold)
        {
            // We can fold the expression, but we don't want to fold
            // when the expression will always throw an exception
            shouldFold = VNEvalShouldFold(typ, func, arg0VN, arg1VN);
        }

        if (shouldFold)
        {
            return EvalFuncForConstantArgs(typ, func, arg0VN, arg1VN);
        }
    }
    // We canonicalize commutative operations.
    // (Perhaps should eventually handle associative/commutative [AC] ops -- but that gets complicated...)
    if (VNFuncIsCommutative(func))
    {
        // Order arg0 arg1 by numerical VN value.
        if (arg0VN > arg1VN)
        {
            std::swap(arg0VN, arg1VN);
        }
    }

    // Have we already assigned a ValueNum for 'func'('arg0VN','arg1VN') ?
    //
    VNDefFunc2Arg fstruct(func, arg0VN, arg1VN);
    if (!GetVNFunc2Map()->Lookup(fstruct, &resultVN))
    {
        if (func == VNF_CastClass)
        {
            // In terms of values, a castclass always returns its second argument, the object being cast.
            // The operation may also throw an exception
            ValueNum vnExcSet = VNExcSetSingleton(VNForFunc(TYP_REF, VNF_InvalidCastExc, arg1VN, arg0VN));
            resultVN          = VNWithExc(arg1VN, vnExcSet);
        }
        else
        {
            resultVN = EvalUsingMathIdentity(typ, func, arg0VN, arg1VN);

            // Do we have a valid resultVN?
            if ((resultVN == NoVN) || (TypeOfVN(resultVN) != typ))
            {
                // Otherwise, Allocate a new ValueNum for 'func'('arg0VN','arg1VN')
                //
                Chunk*   c                                                = GetAllocChunk(typ, CEA_Func2);
                unsigned offsetWithinChunk                                = c->AllocVN();
                resultVN                                                  = c->m_baseVN + offsetWithinChunk;
                static_cast<VNDefFunc2Arg*>(c->m_defs)[offsetWithinChunk] = fstruct;
                // Record 'resultVN' in the Func2Map
                GetVNFunc2Map()->Set(fstruct, resultVN);
            }
        }
    }
    return resultVN;
}

//----------------------------------------------------------------------------------------
//  VNForFunc  - Returns the ValueNum associated with 'func'('arg0VN','arg1VN','arg2VN')
//               There is a one-to-one relationship between the ValueNum
//               and 'func'('arg0VN','arg1VN','arg2VN')
//
// Arguments:
//    typ            - The type of the resulting ValueNum produced by 'func'
//    func           - Any binary VNFunc
//    arg0VN         - The ValueNum of the first argument to 'func'
//    arg1VN         - The ValueNum of the second argument to 'func'
//    arg2VN         - The ValueNum of the third argument to 'func'
//
// Return Value:     - Returns the ValueNum associated with 'func'('arg0VN','arg1VN','arg1VN)
//
// Note: - This method only handles Trinary operations
//         We have to special case VNF_PhiDef, as it's first two arguments are not ValueNums
//
ValueNum ValueNumStore::VNForFunc(var_types typ, VNFunc func, ValueNum arg0VN, ValueNum arg1VN, ValueNum arg2VN)
{
    assert(arg0VN != NoVN);
    assert(arg1VN != NoVN);
    assert(arg2VN != NoVN);
    assert(VNFuncArityIsLegal(func, 3));

#ifdef DEBUG
    // Function arguments carry no exceptions.
    //
    if (func != VNF_PhiDef)
    {
        // For a phi definition first and second argument are "plain" local/ssa numbers.
        // (I don't know if having such non-VN arguments to a VN function is a good idea -- if we wanted to declare
        // ValueNum to be "short" it would be a problem, for example.  But we'll leave it for now, with these explicit
        // exceptions.)
        assert(arg0VN == VNNormalValue(arg0VN));
        assert(arg1VN == VNNormalValue(arg1VN));
    }
    assert(arg2VN == VNNormalValue(arg2VN));
#endif

    ValueNum resultVN;

    // Have we already assigned a ValueNum for 'func'('arg0VN','arg1VN','arg2VN') ?
    //
    VNDefFunc3Arg fstruct(func, arg0VN, arg1VN, arg2VN);
    if (!GetVNFunc3Map()->Lookup(fstruct, &resultVN))
    {
        // Otherwise, Allocate a new ValueNum for 'func'('arg0VN','arg1VN','arg2VN')
        //
        Chunk*   c                                                = GetAllocChunk(typ, CEA_Func3);
        unsigned offsetWithinChunk                                = c->AllocVN();
        resultVN                                                  = c->m_baseVN + offsetWithinChunk;
        static_cast<VNDefFunc3Arg*>(c->m_defs)[offsetWithinChunk] = fstruct;
        // Record 'resultVN' in the Func3Map
        GetVNFunc3Map()->Set(fstruct, resultVN);
    }
    return resultVN;
}

// ----------------------------------------------------------------------------------------
//  VNForFunc  - Returns the ValueNum associated with 'func'('arg0VN','arg1VN','arg2VN','arg3VN')
//               There is a one-to-one relationship between the ValueNum
//               and 'func'('arg0VN','arg1VN','arg2VN','arg3VN')
//
// Arguments:
//    typ            - The type of the resulting ValueNum produced by 'func'
//    func           - Any binary VNFunc
//    arg0VN         - The ValueNum of the first argument to 'func'
//    arg1VN         - The ValueNum of the second argument to 'func'
//    arg2VN         - The ValueNum of the third argument to 'func'
//    arg3VN         - The ValueNum of the fourth argument to 'func'
//
// Return Value:     - Returns the ValueNum associated with 'func'('arg0VN','arg1VN','arg2VN','arg3VN')
//
// Note:   Currently the only four operand funcs are VNF_PtrToArrElem and VNF_MapStore.
//
ValueNum ValueNumStore::VNForFunc(
    var_types typ, VNFunc func, ValueNum arg0VN, ValueNum arg1VN, ValueNum arg2VN, ValueNum arg3VN)
{
    assert(arg0VN != NoVN && arg1VN != NoVN && arg2VN != NoVN && arg3VN != NoVN);

    // Function arguments carry no exceptions.
    assert(arg0VN == VNNormalValue(arg0VN));
    assert(arg1VN == VNNormalValue(arg1VN));
    assert(arg2VN == VNNormalValue(arg2VN));
    assert((func == VNF_MapStore) || (arg3VN == VNNormalValue(arg3VN)));
    assert(VNFuncArityIsLegal(func, 4));

    ValueNum resultVN;

    // Have we already assigned a ValueNum for 'func'('arg0VN','arg1VN','arg2VN','arg3VN') ?
    //
    VNDefFunc4Arg fstruct(func, arg0VN, arg1VN, arg2VN, arg3VN);
    if (!GetVNFunc4Map()->Lookup(fstruct, &resultVN))
    {
        // Otherwise, Allocate a new ValueNum for 'func'('arg0VN','arg1VN','arg2VN','arg3VN')
        //
        Chunk*   c                                                = GetAllocChunk(typ, CEA_Func4);
        unsigned offsetWithinChunk                                = c->AllocVN();
        resultVN                                                  = c->m_baseVN + offsetWithinChunk;
        static_cast<VNDefFunc4Arg*>(c->m_defs)[offsetWithinChunk] = fstruct;
        // Record 'resultVN' in the Func4Map
        GetVNFunc4Map()->Set(fstruct, resultVN);
    }
    return resultVN;
}

ValueNum ValueNumStore::VNForMapStore(var_types typ, ValueNum mapVN, ValueNum indexVN, ValueNum valueVN)
{
    BasicBlock* const            bb      = m_pComp->compCurBB;
    BasicBlock::loopNumber const loopNum = bb->bbNatLoopNum;
    ValueNum const               result  = VNForFunc(typ, VNF_MapStore, mapVN, indexVN, valueVN, loopNum);

#ifdef DEBUG
    if (m_pComp->verbose)
    {
        printf("    %s ", varTypeName(typ));
        m_pComp->vnPrint(result, 1);
        printf("\n");
    }
#endif

    return result;
}

// This requires a "ValueNumKind" because it will attempt, given "select(phi(m1, ..., mk), ind)", to evaluate
// "select(m1, ind)", ..., "select(mk, ind)" to see if they agree. It needs to know which kind of value number
// (liberal/conservative) to read from the SSA def referenced in the phi argument.
ValueNum ValueNumStore::VNForMapSelect(ValueNumKind vnk, var_types typ, ValueNum mapVN, ValueNum indexVN)
{
    int      budget          = m_mapSelectBudget;
    bool     usedRecursiveVN = false;
    ValueNum result          = VNForMapSelectWork(vnk, typ, mapVN, indexVN, &budget, &usedRecursiveVN);

    // The remaining budget should always be between [0..m_mapSelectBudget]
    assert((budget >= 0) && (budget <= m_mapSelectBudget));

#ifdef DEBUG
    if (m_pComp->verbose)
    {
        printf("    %s ", varTypeName(TypeOfVN(result)));
        m_pComp->vnPrint(result, 1);
        printf("\n");
    }
#endif

    return result;
}

//------------------------------------------------------------------------------
// VNForMapSelectWork : A method that does the work for VNForMapSelect and may call itself recursively.
//
//
// Arguments:
//    vnk  -             Value number kind
//    typ  -             Value type
//    arg0VN  -          Zeroth argument
//    arg1VN  -          First argument
//    pBudget -          Remaining budget for the outer evaluation
//    pUsedRecursiveVN - Out-parameter that is set to true iff RecursiveVN was returned from this method
//                       or from a method called during one of recursive invocations.
//
// Return Value:
//    Value number for the result of the evaluation.
//
// Notes:
//    This requires a "ValueNumKind" because it will attempt, given "select(phi(m1, ..., mk), ind)", to evaluate
//    "select(m1, ind)", ..., "select(mk, ind)" to see if they agree.  It needs to know which kind of value number
//    (liberal/conservative) to read from the SSA def referenced in the phi argument.

ValueNum ValueNumStore::VNForMapSelectWork(
    ValueNumKind vnk, var_types typ, ValueNum arg0VN, ValueNum arg1VN, int* pBudget, bool* pUsedRecursiveVN)
{
TailCall:
    // This label allows us to directly implement a tail call by setting up the arguments, and doing a goto to here.
    assert(arg0VN != NoVN && arg1VN != NoVN);
    assert(arg0VN == VNNormalValue(arg0VN)); // Arguments carry no exceptions.
    assert(arg1VN == VNNormalValue(arg1VN)); // Arguments carry no exceptions.

    *pUsedRecursiveVN = false;

#ifdef DEBUG
    // Provide a mechanism for writing tests that ensure we don't call this ridiculously often.
    m_numMapSels++;
#if 1
// This printing is sometimes useful in debugging.
// if ((m_numMapSels % 1000) == 0) printf("%d VNF_MapSelect applications.\n", m_numMapSels);
#endif
    unsigned selLim = JitConfig.JitVNMapSelLimit();
    assert(selLim == 0 || m_numMapSels < selLim);
#endif
    ValueNum res;

    VNDefFunc2Arg fstruct(VNF_MapSelect, arg0VN, arg1VN);
    if (GetVNFunc2Map()->Lookup(fstruct, &res))
    {
        return res;
    }
    else
    {
        // Give up if we've run out of budget.
        if (*pBudget == 0)
        {
            // We have to use 'nullptr' for the basic block here, because subsequent expressions
            // in different blocks may find this result in the VNFunc2Map -- other expressions in
            // the IR may "evaluate" to this same VNForExpr, so it is not "unique" in the sense
            // that permits the BasicBlock attribution.
            res = VNForExpr(nullptr, typ);
            GetVNFunc2Map()->Set(fstruct, res);
            return res;
        }

        // Reduce our budget by one
        (*pBudget)--;

        // If it's recursive, stop the recursion.
        if (SelectIsBeingEvaluatedRecursively(arg0VN, arg1VN))
        {
            *pUsedRecursiveVN = true;
            return RecursiveVN;
        }

        if (arg0VN == VNForZeroMap())
        {
            return VNZeroForType(typ);
        }
        else if (IsVNFunc(arg0VN))
        {
            VNFuncApp funcApp;
            GetVNFunc(arg0VN, &funcApp);
            if (funcApp.m_func == VNF_MapStore)
            {
                // select(store(m, i, v), i) == v
                if (funcApp.m_args[1] == arg1VN)
                {
                    m_pComp->optRecordLoopMemoryDependence(m_pComp->compCurTree, m_pComp->compCurBB, funcApp.m_args[0]);
                    return funcApp.m_args[2];
                }
                // i # j ==> select(store(m, i, v), j) == select(m, j)
                // Currently the only source of distinctions is when both indices are constants.
                else if (IsVNConstant(arg1VN) && IsVNConstant(funcApp.m_args[1]))
                {
                    assert(funcApp.m_args[1] != arg1VN); // we already checked this above.
                    // This is the equivalent of the recursive tail call:
                    // return VNForMapSelect(vnk, typ, funcApp.m_args[0], arg1VN);
                    // Make sure we capture any exceptions from the "i" and "v" of the store...
                    arg0VN = funcApp.m_args[0];
                    goto TailCall;
                }
            }
            else if (funcApp.m_func == VNF_PhiDef || funcApp.m_func == VNF_PhiMemoryDef)
            {
                unsigned  lclNum   = BAD_VAR_NUM;
                bool      isMemory = false;
                VNFuncApp phiFuncApp;
                bool      defArgIsFunc = false;
                if (funcApp.m_func == VNF_PhiDef)
                {
                    lclNum       = unsigned(funcApp.m_args[0]);
                    defArgIsFunc = GetVNFunc(funcApp.m_args[2], &phiFuncApp);
                }
                else
                {
                    assert(funcApp.m_func == VNF_PhiMemoryDef);
                    isMemory     = true;
                    defArgIsFunc = GetVNFunc(funcApp.m_args[1], &phiFuncApp);
                }
                if (defArgIsFunc && phiFuncApp.m_func == VNF_Phi)
                {
                    // select(phi(m1, m2), x): if select(m1, x) == select(m2, x), return that, else new fresh.
                    // Get the first argument of the phi.

                    // We need to be careful about breaking infinite recursion.  Record the outer select.
                    m_fixedPointMapSels.Push(VNDefFunc2Arg(VNF_MapSelect, arg0VN, arg1VN));

                    assert(IsVNConstant(phiFuncApp.m_args[0]));
                    unsigned phiArgSsaNum = ConstantValue<unsigned>(phiFuncApp.m_args[0]);
                    ValueNum phiArgVN;
                    if (isMemory)
                    {
                        phiArgVN = m_pComp->GetMemoryPerSsaData(phiArgSsaNum)->m_vnPair.Get(vnk);
                    }
                    else
                    {
                        phiArgVN = m_pComp->lvaTable[lclNum].GetPerSsaData(phiArgSsaNum)->m_vnPair.Get(vnk);
                    }
                    if (phiArgVN != ValueNumStore::NoVN)
                    {
                        bool     allSame = true;
                        ValueNum argRest = phiFuncApp.m_args[1];
                        ValueNum sameSelResult =
                            VNForMapSelectWork(vnk, typ, phiArgVN, arg1VN, pBudget, pUsedRecursiveVN);

                        // It is possible that we just now exceeded our budget, if so we need to force an early exit
                        // and stop calling VNForMapSelectWork
                        if (*pBudget <= 0)
                        {
                            // We don't have any budget remaining to verify that all phiArgs are the same
                            // so setup the default failure case now.
                            allSame = false;
                        }

                        while (allSame && argRest != ValueNumStore::NoVN)
                        {
                            ValueNum  cur = argRest;
                            VNFuncApp phiArgFuncApp;
                            if (GetVNFunc(argRest, &phiArgFuncApp) && phiArgFuncApp.m_func == VNF_Phi)
                            {
                                cur     = phiArgFuncApp.m_args[0];
                                argRest = phiArgFuncApp.m_args[1];
                            }
                            else
                            {
                                argRest = ValueNumStore::NoVN; // Cause the loop to terminate.
                            }
                            assert(IsVNConstant(cur));
                            phiArgSsaNum = ConstantValue<unsigned>(cur);
                            if (isMemory)
                            {
                                phiArgVN = m_pComp->GetMemoryPerSsaData(phiArgSsaNum)->m_vnPair.Get(vnk);
                            }
                            else
                            {
                                phiArgVN = m_pComp->lvaTable[lclNum].GetPerSsaData(phiArgSsaNum)->m_vnPair.Get(vnk);
                            }
                            if (phiArgVN == ValueNumStore::NoVN)
                            {
                                allSame = false;
                            }
                            else
                            {
                                bool     usedRecursiveVN = false;
                                ValueNum curResult =
                                    VNForMapSelectWork(vnk, typ, phiArgVN, arg1VN, pBudget, &usedRecursiveVN);
                                *pUsedRecursiveVN |= usedRecursiveVN;
                                if (sameSelResult == ValueNumStore::RecursiveVN)
                                {
                                    sameSelResult = curResult;
                                }
                                if (curResult != ValueNumStore::RecursiveVN && curResult != sameSelResult)
                                {
                                    allSame = false;
                                }
                            }
                        }
                        if (allSame && sameSelResult != ValueNumStore::RecursiveVN)
                        {
                            // Make sure we're popping what we pushed.
                            assert(FixedPointMapSelsTopHasValue(arg0VN, arg1VN));
                            m_fixedPointMapSels.Pop();

                            // To avoid exponential searches, we make sure that this result is memo-ized.
                            // The result is always valid for memoization if we didn't rely on RecursiveVN to get it.
                            // If RecursiveVN was used, we are processing a loop and we can't memo-ize this intermediate
                            // result if, e.g., this block is in a multi-entry loop.
                            if (!*pUsedRecursiveVN)
                            {
                                GetVNFunc2Map()->Set(fstruct, sameSelResult);
                            }

                            return sameSelResult;
                        }
                        // Otherwise, fall through to creating the select(phi(m1, m2), x) function application.
                    }
                    // Make sure we're popping what we pushed.
                    assert(FixedPointMapSelsTopHasValue(arg0VN, arg1VN));
                    m_fixedPointMapSels.Pop();
                }
            }
        }

        // We may have run out of budget and already assigned a result
        if (!GetVNFunc2Map()->Lookup(fstruct, &res))
        {
            // Otherwise, assign a new VN for the function application.
            Chunk*   c                                                = GetAllocChunk(typ, CEA_Func2);
            unsigned offsetWithinChunk                                = c->AllocVN();
            res                                                       = c->m_baseVN + offsetWithinChunk;
            static_cast<VNDefFunc2Arg*>(c->m_defs)[offsetWithinChunk] = fstruct;
            GetVNFunc2Map()->Set(fstruct, res);
        }
        return res;
    }
}

ValueNum ValueNumStore::EvalFuncForConstantArgs(var_types typ, VNFunc func, ValueNum arg0VN)
{
    assert(CanEvalForConstantArgs(func));
    assert(IsVNConstant(arg0VN));
    switch (TypeOfVN(arg0VN))
    {
        case TYP_INT:
        {
            int resVal = EvalOp<int>(func, ConstantValue<int>(arg0VN));
            // Unary op on a handle results in a handle.
            return IsVNHandle(arg0VN) ? VNForHandle(ssize_t(resVal), GetHandleFlags(arg0VN)) : VNForIntCon(resVal);
        }
        case TYP_LONG:
        {
            INT64 resVal = EvalOp<INT64>(func, ConstantValue<INT64>(arg0VN));
            // Unary op on a handle results in a handle.
            return IsVNHandle(arg0VN) ? VNForHandle(ssize_t(resVal), GetHandleFlags(arg0VN)) : VNForLongCon(resVal);
        }
        case TYP_FLOAT:
        {
            float resVal = EvalOp<float>(func, ConstantValue<float>(arg0VN));
            return VNForFloatCon(resVal);
        }
        case TYP_DOUBLE:
        {
            double resVal = EvalOp<double>(func, ConstantValue<double>(arg0VN));
            return VNForDoubleCon(resVal);
        }
        case TYP_REF:
        {
            // If arg0 has a possible exception, it wouldn't have been constant.
            assert(!VNHasExc(arg0VN));
            // Otherwise...
            assert(arg0VN == VNForNull());         // Only other REF constant.
            assert(func == VNFunc(GT_ARR_LENGTH)); // Only function we can apply to a REF constant!
            return VNWithExc(VNForVoid(), VNExcSetSingleton(VNForFunc(TYP_REF, VNF_NullPtrExc, VNForNull())));
        }
        default:
            // We will assert below
            break;
    }
    noway_assert(!"Unhandled operation in EvalFuncForConstantArgs");
    return NoVN;
}

bool ValueNumStore::SelectIsBeingEvaluatedRecursively(ValueNum map, ValueNum ind)
{
    for (unsigned i = 0; i < m_fixedPointMapSels.Size(); i++)
    {
        VNDefFunc2Arg& elem = m_fixedPointMapSels.GetRef(i);
        assert(elem.m_func == VNF_MapSelect);
        if (elem.m_arg0 == map && elem.m_arg1 == ind)
        {
            return true;
        }
    }
    return false;
}

#ifdef DEBUG
bool ValueNumStore::FixedPointMapSelsTopHasValue(ValueNum map, ValueNum index)
{
    if (m_fixedPointMapSels.Size() == 0)
    {
        return false;
    }
    VNDefFunc2Arg& top = m_fixedPointMapSels.TopRef();
    return top.m_func == VNF_MapSelect && top.m_arg0 == map && top.m_arg1 == index;
}
#endif

// Given an integer constant value number return its value as an int.
//
int ValueNumStore::GetConstantInt32(ValueNum argVN)
{
    assert(IsVNConstant(argVN));
    var_types argVNtyp = TypeOfVN(argVN);

    int result = 0;

    switch (argVNtyp)
    {
        case TYP_INT:
            result = ConstantValue<int>(argVN);
            break;
#ifndef TARGET_64BIT
        case TYP_REF:
        case TYP_BYREF:
            result = (int)ConstantValue<size_t>(argVN);
            break;
#endif
        default:
            unreached();
    }
    return result;
}

// Given an integer constant value number return its value as an INT64.
//
INT64 ValueNumStore::GetConstantInt64(ValueNum argVN)
{
    assert(IsVNConstant(argVN));
    var_types argVNtyp = TypeOfVN(argVN);

    INT64 result = 0;

    switch (argVNtyp)
    {
        case TYP_INT:
            result = (INT64)ConstantValue<int>(argVN);
            break;
        case TYP_LONG:
            result = ConstantValue<INT64>(argVN);
            break;
        case TYP_REF:
        case TYP_BYREF:
            result = (INT64)ConstantValue<size_t>(argVN);
            break;
        default:
            unreached();
    }
    return result;
}

// Given a double constant value number return its value as a double.
//
double ValueNumStore::GetConstantDouble(ValueNum argVN)
{
    assert(IsVNConstant(argVN));
    assert(TypeOfVN(argVN) == TYP_DOUBLE);

    return ConstantValue<double>(argVN);
}

// Given a float constant value number return its value as a float.
//
float ValueNumStore::GetConstantSingle(ValueNum argVN)
{
    assert(IsVNConstant(argVN));
    assert(TypeOfVN(argVN) == TYP_FLOAT);

    return ConstantValue<float>(argVN);
}

// Compute the proper value number when the VNFunc has all constant arguments
// This essentially performs constant folding at value numbering time
//
ValueNum ValueNumStore::EvalFuncForConstantArgs(var_types typ, VNFunc func, ValueNum arg0VN, ValueNum arg1VN)
{
    assert(CanEvalForConstantArgs(func));
    assert(IsVNConstant(arg0VN) && IsVNConstant(arg1VN));
    assert(!VNHasExc(arg0VN) && !VNHasExc(arg1VN)); // Otherwise, would not be constant.

    // if our func is the VNF_Cast operation we handle it first
    if (VNFuncIsNumericCast(func))
    {
        return EvalCastForConstantArgs(typ, func, arg0VN, arg1VN);
    }

    var_types arg0VNtyp = TypeOfVN(arg0VN);
    var_types arg1VNtyp = TypeOfVN(arg1VN);

    // When both arguments are floating point types
    // We defer to the EvalFuncForConstantFPArgs()
    if (varTypeIsFloating(arg0VNtyp) && varTypeIsFloating(arg1VNtyp))
    {
        return EvalFuncForConstantFPArgs(typ, func, arg0VN, arg1VN);
    }

    // after this we shouldn't have to deal with floating point types for arg0VN or arg1VN
    assert(!varTypeIsFloating(arg0VNtyp));
    assert(!varTypeIsFloating(arg1VNtyp));

    // Stack-normalize the result type.
    if (varTypeIsSmall(typ))
    {
        typ = TYP_INT;
    }

    ValueNum result; // left uninitialized, we are required to initialize it on all paths below.

    // Are both args of the same type?
    if (arg0VNtyp == arg1VNtyp)
    {
        if (arg0VNtyp == TYP_INT)
        {
            int arg0Val = ConstantValue<int>(arg0VN);
            int arg1Val = ConstantValue<int>(arg1VN);

            if (VNFuncIsComparison(func))
            {
                assert(typ == TYP_INT);
                result = VNForIntCon(EvalComparison(func, arg0Val, arg1Val));
            }
            else
            {
                assert(typ == TYP_INT);
                int resultVal = EvalOp<int>(func, arg0Val, arg1Val);
                // Bin op on a handle results in a handle.
                ValueNum handleVN = IsVNHandle(arg0VN) ? arg0VN : IsVNHandle(arg1VN) ? arg1VN : NoVN;
                if (handleVN != NoVN)
                {
                    result = VNForHandle(ssize_t(resultVal), GetHandleFlags(handleVN)); // Use VN for Handle
                }
                else
                {
                    result = VNForIntCon(resultVal);
                }
            }
        }
        else if (arg0VNtyp == TYP_LONG)
        {
            INT64 arg0Val = ConstantValue<INT64>(arg0VN);
            INT64 arg1Val = ConstantValue<INT64>(arg1VN);

            if (VNFuncIsComparison(func))
            {
                assert(typ == TYP_INT);
                result = VNForIntCon(EvalComparison(func, arg0Val, arg1Val));
            }
            else
            {
                assert(typ == TYP_LONG);
                INT64    resultVal = EvalOp<INT64>(func, arg0Val, arg1Val);
                ValueNum handleVN  = IsVNHandle(arg0VN) ? arg0VN : IsVNHandle(arg1VN) ? arg1VN : NoVN;

                if (handleVN != NoVN)
                {
                    result = VNForHandle(ssize_t(resultVal), GetHandleFlags(handleVN)); // Use VN for Handle
                }
                else
                {
                    result = VNForLongCon(resultVal);
                }
            }
        }
        else // both args are TYP_REF or both args are TYP_BYREF
        {
            size_t arg0Val = ConstantValue<size_t>(arg0VN); // We represent ref/byref constants as size_t's.
            size_t arg1Val = ConstantValue<size_t>(arg1VN); // Also we consider null to be zero.

            if (VNFuncIsComparison(func))
            {
                assert(typ == TYP_INT);
                result = VNForIntCon(EvalComparison(func, arg0Val, arg1Val));
            }
            else if (typ == TYP_INT) // We could see GT_OR of a constant ByRef and Null
            {
                int resultVal = (int)EvalOp<size_t>(func, arg0Val, arg1Val);
                result        = VNForIntCon(resultVal);
            }
            else // We could see GT_OR of a constant ByRef and Null
            {
                assert((typ == TYP_BYREF) || (typ == TYP_I_IMPL));
                size_t resultVal = EvalOp<size_t>(func, arg0Val, arg1Val);
                result           = VNForByrefCon((target_size_t)resultVal);
            }
        }
    }
    else // We have args of different types
    {
        // We represent ref/byref constants as size_t's.
        // Also we consider null to be zero.
        //
        INT64 arg0Val = GetConstantInt64(arg0VN);
        INT64 arg1Val = GetConstantInt64(arg1VN);

        if (VNFuncIsComparison(func))
        {
            assert(typ == TYP_INT);
            result = VNForIntCon(EvalComparison(func, arg0Val, arg1Val));
        }
        else if (typ == TYP_INT) // We could see GT_OR of an int and constant ByRef or Null
        {
            int resultVal = (int)EvalOp<INT64>(func, arg0Val, arg1Val);
            result        = VNForIntCon(resultVal);
        }
        else
        {
            assert(typ != TYP_INT);
            INT64 resultVal = EvalOp<INT64>(func, arg0Val, arg1Val);

            switch (typ)
            {
                case TYP_BYREF:
                    result = VNForByrefCon((target_size_t)resultVal);
                    break;
                case TYP_LONG:
                    result = VNForLongCon(resultVal);
                    break;
                case TYP_REF:
                    assert(resultVal == 0); // Only valid REF constant
                    result = VNForNull();
                    break;
                default:
                    unreached();
            }
        }
    }

    return result;
}

// Compute the proper value number when the VNFunc has all constant floating-point arguments
// This essentially must perform constant folding at value numbering time
//
ValueNum ValueNumStore::EvalFuncForConstantFPArgs(var_types typ, VNFunc func, ValueNum arg0VN, ValueNum arg1VN)
{
    assert(CanEvalForConstantArgs(func));
    assert(IsVNConstant(arg0VN) && IsVNConstant(arg1VN));

    // We expect both argument types to be floating-point types
    var_types arg0VNtyp = TypeOfVN(arg0VN);
    var_types arg1VNtyp = TypeOfVN(arg1VN);

    assert(varTypeIsFloating(arg0VNtyp));
    assert(varTypeIsFloating(arg1VNtyp));

    // We also expect both arguments to be of the same floating-point type
    assert(arg0VNtyp == arg1VNtyp);

    ValueNum result; // left uninitialized, we are required to initialize it on all paths below.

    if (VNFuncIsComparison(func))
    {
        assert(genActualType(typ) == TYP_INT);

        if (arg0VNtyp == TYP_FLOAT)
        {
            result = VNForIntCon(EvalComparison<float>(func, GetConstantSingle(arg0VN), GetConstantSingle(arg1VN)));
        }
        else
        {
            assert(arg0VNtyp == TYP_DOUBLE);
            result = VNForIntCon(EvalComparison<double>(func, GetConstantDouble(arg0VN), GetConstantDouble(arg1VN)));
        }
    }
    else
    {
        // We expect the return type to be the same as the argument type
        assert(varTypeIsFloating(typ));
        assert(arg0VNtyp == typ);

        if (typ == TYP_FLOAT)
        {
            float floatResultVal = EvalOp<float>(func, GetConstantSingle(arg0VN), GetConstantSingle(arg1VN));
            result               = VNForFloatCon(floatResultVal);
        }
        else
        {
            assert(typ == TYP_DOUBLE);

            double doubleResultVal = EvalOp<double>(func, GetConstantDouble(arg0VN), GetConstantDouble(arg1VN));
            result                 = VNForDoubleCon(doubleResultVal);
        }
    }

    return result;
}

// Compute the proper value number for a VNF_Cast with constant arguments
// This essentially must perform constant folding at value numbering time
//
ValueNum ValueNumStore::EvalCastForConstantArgs(var_types typ, VNFunc func, ValueNum arg0VN, ValueNum arg1VN)
{
    assert(VNFuncIsNumericCast(func));
    assert(IsVNConstant(arg0VN) && IsVNConstant(arg1VN));

    // Stack-normalize the result type.
    if (varTypeIsSmall(typ))
    {
        typ = TYP_INT;
    }

    var_types arg0VNtyp = TypeOfVN(arg0VN);

    if (IsVNHandle(arg0VN))
    {
        // We don't allow handles to be cast to random var_types.
        assert(typ == TYP_I_IMPL);
    }

    // We previously encoded the castToType operation using VNForCastOper().
    var_types castToType;
    bool      srcIsUnsigned;
    GetCastOperFromVN(arg1VN, &castToType, &srcIsUnsigned);
    var_types castFromType = arg0VNtyp;
    bool      checkedCast  = func == VNF_CastOvf;

    switch (castFromType) // GT_CAST source type
    {
#ifndef TARGET_64BIT
        case TYP_REF:
        case TYP_BYREF:
#endif
        case TYP_INT:
        {
            int arg0Val = GetConstantInt32(arg0VN);
            assert(!checkedCast || !CheckedOps::CastFromIntOverflows(arg0Val, castToType, srcIsUnsigned));

            switch (castToType)
            {
                case TYP_BYTE:
                    assert(typ == TYP_INT);
                    return VNForIntCon(INT8(arg0Val));
                case TYP_BOOL:
                case TYP_UBYTE:
                    assert(typ == TYP_INT);
                    return VNForIntCon(UINT8(arg0Val));
                case TYP_SHORT:
                    assert(typ == TYP_INT);
                    return VNForIntCon(INT16(arg0Val));
                case TYP_USHORT:
                    assert(typ == TYP_INT);
                    return VNForIntCon(UINT16(arg0Val));
                case TYP_INT:
                case TYP_UINT:
                    assert(typ == TYP_INT);
                    return arg0VN;
                case TYP_LONG:
                case TYP_ULONG:
                    assert(!IsVNHandle(arg0VN));
#ifdef TARGET_64BIT
                    if (typ == TYP_LONG)
                    {
                        if (srcIsUnsigned)
                        {
                            return VNForLongCon(INT64(unsigned(arg0Val)));
                        }
                        else
                        {
                            return VNForLongCon(INT64(arg0Val));
                        }
                    }
                    else
                    {
                        assert(typ == TYP_BYREF);
                        return VNForByrefCon(target_size_t(arg0Val));
                    }
#else // TARGET_32BIT
                    if (srcIsUnsigned)
                        return VNForLongCon(INT64(unsigned(arg0Val)));
                    else
                        return VNForLongCon(INT64(arg0Val));
#endif
                case TYP_BYREF:
                    assert(typ == TYP_BYREF);
                    return VNForByrefCon(target_size_t(arg0Val));

                case TYP_FLOAT:
                    assert(typ == TYP_FLOAT);
                    if (srcIsUnsigned)
                    {
                        return VNForFloatCon(float(unsigned(arg0Val)));
                    }
                    else
                    {
                        return VNForFloatCon(float(arg0Val));
                    }
                case TYP_DOUBLE:
                    assert(typ == TYP_DOUBLE);
                    if (srcIsUnsigned)
                    {
                        return VNForDoubleCon(double(unsigned(arg0Val)));
                    }
                    else
                    {
                        return VNForDoubleCon(double(arg0Val));
                    }
                default:
                    unreached();
            }
            break;
        }
            {
#ifdef TARGET_64BIT
                case TYP_REF:
                case TYP_BYREF:
#endif
                case TYP_LONG:
                    INT64 arg0Val = GetConstantInt64(arg0VN);
                    assert(!checkedCast || !CheckedOps::CastFromLongOverflows(arg0Val, castToType, srcIsUnsigned));

                    switch (castToType)
                    {
                        case TYP_BYTE:
                            assert(typ == TYP_INT);
                            return VNForIntCon(INT8(arg0Val));
                        case TYP_BOOL:
                        case TYP_UBYTE:
                            assert(typ == TYP_INT);
                            return VNForIntCon(UINT8(arg0Val));
                        case TYP_SHORT:
                            assert(typ == TYP_INT);
                            return VNForIntCon(INT16(arg0Val));
                        case TYP_USHORT:
                            assert(typ == TYP_INT);
                            return VNForIntCon(UINT16(arg0Val));
                        case TYP_INT:
                            assert(typ == TYP_INT);
                            return VNForIntCon(INT32(arg0Val));
                        case TYP_UINT:
                            assert(typ == TYP_INT);
                            return VNForIntCon(UINT32(arg0Val));
                        case TYP_LONG:
                        case TYP_ULONG:
                            assert(typ == TYP_LONG);
                            return arg0VN;
                        case TYP_BYREF:
                            assert(typ == TYP_BYREF);
                            return VNForByrefCon((target_size_t)arg0Val);
                        case TYP_FLOAT:
                            assert(typ == TYP_FLOAT);
                            if (srcIsUnsigned)
                            {
                                return VNForFloatCon(FloatingPointUtils::convertUInt64ToFloat(UINT64(arg0Val)));
                            }
                            else
                            {
                                return VNForFloatCon(float(arg0Val));
                            }
                        case TYP_DOUBLE:
                            assert(typ == TYP_DOUBLE);
                            if (srcIsUnsigned)
                            {
                                return VNForDoubleCon(FloatingPointUtils::convertUInt64ToDouble(UINT64(arg0Val)));
                            }
                            else
                            {
                                return VNForDoubleCon(double(arg0Val));
                            }
                        default:
                            unreached();
                    }
            }
        case TYP_FLOAT:
        {
            float arg0Val = GetConstantSingle(arg0VN);
            assert(!CheckedOps::CastFromFloatOverflows(arg0Val, castToType));

            switch (castToType)
            {
                case TYP_BYTE:
                    assert(typ == TYP_INT);
                    return VNForIntCon(INT8(arg0Val));
                case TYP_BOOL:
                case TYP_UBYTE:
                    assert(typ == TYP_INT);
                    return VNForIntCon(UINT8(arg0Val));
                case TYP_SHORT:
                    assert(typ == TYP_INT);
                    return VNForIntCon(INT16(arg0Val));
                case TYP_USHORT:
                    assert(typ == TYP_INT);
                    return VNForIntCon(UINT16(arg0Val));
                case TYP_INT:
                    assert(typ == TYP_INT);
                    return VNForIntCon(INT32(arg0Val));
                case TYP_UINT:
                    assert(typ == TYP_INT);
                    return VNForIntCon(UINT32(arg0Val));
                case TYP_LONG:
                    assert(typ == TYP_LONG);
                    return VNForLongCon(INT64(arg0Val));
                case TYP_ULONG:
                    assert(typ == TYP_LONG);
                    return VNForLongCon(UINT64(arg0Val));
                case TYP_FLOAT:
                    assert(typ == TYP_FLOAT);
                    return VNForFloatCon(arg0Val);
                case TYP_DOUBLE:
                    assert(typ == TYP_DOUBLE);
                    return VNForDoubleCon(double(arg0Val));
                default:
                    unreached();
            }
        }
        case TYP_DOUBLE:
        {
            double arg0Val = GetConstantDouble(arg0VN);
            assert(!CheckedOps::CastFromDoubleOverflows(arg0Val, castToType));

            switch (castToType)
            {
                case TYP_BYTE:
                    assert(typ == TYP_INT);
                    return VNForIntCon(INT8(arg0Val));
                case TYP_BOOL:
                case TYP_UBYTE:
                    assert(typ == TYP_INT);
                    return VNForIntCon(UINT8(arg0Val));
                case TYP_SHORT:
                    assert(typ == TYP_INT);
                    return VNForIntCon(INT16(arg0Val));
                case TYP_USHORT:
                    assert(typ == TYP_INT);
                    return VNForIntCon(UINT16(arg0Val));
                case TYP_INT:
                    assert(typ == TYP_INT);
                    return VNForIntCon(INT32(arg0Val));
                case TYP_UINT:
                    assert(typ == TYP_INT);
                    return VNForIntCon(UINT32(arg0Val));
                case TYP_LONG:
                    assert(typ == TYP_LONG);
                    return VNForLongCon(INT64(arg0Val));
                case TYP_ULONG:
                    assert(typ == TYP_LONG);
                    return VNForLongCon(UINT64(arg0Val));
                case TYP_FLOAT:
                    assert(typ == TYP_FLOAT);
                    return VNForFloatCon(float(arg0Val));
                case TYP_DOUBLE:
                    assert(typ == TYP_DOUBLE);
                    return VNForDoubleCon(arg0Val);
                default:
                    unreached();
            }
        }
        default:
            unreached();
    }
}

//-----------------------------------------------------------------------------------
// CanEvalForConstantArgs:  - Given a VNFunc value return true when we can perform
//                            compile-time constant folding for the operation.
//
// Arguments:
//    vnf        - The VNFunc that we are inquiring about
//
// Return Value:
//               - Returns true if we can always compute a constant result
//                 when given all constant args.
//
// Notes:        - When this method returns true, the logic to compute the
//                 compile-time result must also be added to EvalOP,
//                 EvalOpspecialized or EvalComparison
//
bool ValueNumStore::CanEvalForConstantArgs(VNFunc vnf)
{
    if (vnf < VNF_Boundary)
    {
        genTreeOps oper = genTreeOps(vnf);

        switch (oper)
        {
            // Only return true for the node kinds that have code that supports
            // them in EvalOP, EvalOpspecialized or EvalComparison

            // Unary Ops
            case GT_NEG:
            case GT_NOT:
            case GT_BSWAP16:
            case GT_BSWAP:

            // Binary Ops
            case GT_ADD:
            case GT_SUB:
            case GT_MUL:
            case GT_DIV:
            case GT_MOD:

            case GT_UDIV:
            case GT_UMOD:

            case GT_AND:
            case GT_OR:
            case GT_XOR:

            case GT_LSH:
            case GT_RSH:
            case GT_RSZ:
            case GT_ROL:
            case GT_ROR:

            // Equality Ops
            case GT_EQ:
            case GT_NE:
            case GT_GT:
            case GT_GE:
            case GT_LT:
            case GT_LE:

                // We can evaluate these.
                return true;

            default:
                // We can not evaluate these.
                return false;
        }
    }
    else
    {
        // some VNF_ that we can evaluate
        switch (vnf)
        {
            case VNF_GT_UN:
            case VNF_GE_UN:
            case VNF_LT_UN:
            case VNF_LE_UN:

            case VNF_ADD_OVF:
            case VNF_SUB_OVF:
            case VNF_MUL_OVF:
            case VNF_ADD_UN_OVF:
            case VNF_SUB_UN_OVF:
            case VNF_MUL_UN_OVF:

            case VNF_Cast:
            case VNF_CastOvf:
                // We can evaluate these.
                return true;

            default:
                // We can not evaluate these.
                return false;
        }
    }
}

//----------------------------------------------------------------------------------------
//  VNEvalShouldFold - Returns true if we should perform the folding operation.
//                     It returns false if we don't want to fold the expression,
//                     because it will always throw an exception.
//
// Arguments:
//    typ            - The type of the resulting ValueNum produced by 'func'
//    func           - Any binary VNFunc
//    arg0VN         - The ValueNum of the first argument to 'func'
//    arg1VN         - The ValueNum of the second argument to 'func'
//
// Return Value:     - Returns true if we should perform a folding operation.
//
// Notes:            - Does not handle operations producing TYP_BYREF.
//
bool ValueNumStore::VNEvalShouldFold(var_types typ, VNFunc func, ValueNum arg0VN, ValueNum arg1VN)
{
    assert(typ != TYP_BYREF);

    // We have some arithmetic operations that will always throw
    // an exception given particular constant argument(s).
    // (i.e. integer division by zero)
    //
    // We will avoid performing any constant folding on them
    // since they won't actually produce any result.
    // Instead they always will throw an exception.

    // Floating point operations do not throw exceptions.
    if (varTypeIsFloating(typ))
    {
        return true;
    }

    genTreeOps oper = genTreeOps(func);
    // Is this an integer divide/modulo that will always throw an exception?
    if (GenTree::StaticOperIs(oper, GT_DIV, GT_UDIV, GT_MOD, GT_UMOD))
    {
        if (!((typ == TYP_INT) || (typ == TYP_LONG)))
        {
            assert(!"Unexpected type in VNEvalShouldFold for integer division/modulus");
            return false;
        }
        // Just in case we have mismatched types.
        if ((TypeOfVN(arg0VN) != typ) || (TypeOfVN(arg1VN) != typ))
        {
            return false;
        }

        INT64 divisor = CoercedConstantValue<INT64>(arg1VN);

        if (divisor == 0)
        {
            // Don't fold, we have a divide by zero.
            return false;
        }
        else if ((oper == GT_DIV || oper == GT_MOD) && (divisor == -1))
        {
            // Don't fold if we have a division of INT32_MIN or INT64_MIN by -1.
            // Note that while INT_MIN % -1 is mathematically well-defined (and equal to 0),
            // we still give up on folding it because the "idiv" instruction is used to compute it on x64.
            // And "idiv" raises an exception on such inputs.
            INT64 dividend    = CoercedConstantValue<INT64>(arg0VN);
            INT64 badDividend = typ == TYP_INT ? INT32_MIN : INT64_MIN;

            // Only fold if our dividend is good.
            return dividend != badDividend;
        }
    }

    // Is this a checked operation that will always throw an exception?
    if (VNFuncIsOverflowArithmetic(func))
    {
        if (typ == TYP_INT)
        {
            int op1 = ConstantValue<int>(arg0VN);
            int op2 = ConstantValue<int>(arg1VN);

            switch (func)
            {
                case VNF_ADD_OVF:
                    return !CheckedOps::AddOverflows(op1, op2, CheckedOps::Signed);
                case VNF_SUB_OVF:
                    return !CheckedOps::SubOverflows(op1, op2, CheckedOps::Signed);
                case VNF_MUL_OVF:
                    return !CheckedOps::MulOverflows(op1, op2, CheckedOps::Signed);
                case VNF_ADD_UN_OVF:
                    return !CheckedOps::AddOverflows(op1, op2, CheckedOps::Unsigned);
                case VNF_SUB_UN_OVF:
                    return !CheckedOps::SubOverflows(op1, op2, CheckedOps::Unsigned);
                case VNF_MUL_UN_OVF:
                    return !CheckedOps::MulOverflows(op1, op2, CheckedOps::Unsigned);
                default:
                    assert(!"Unexpected checked operation in VNEvalShouldFold");
                    return false;
            }
        }
        else if (typ == TYP_LONG)
        {
            INT64 op1 = ConstantValue<INT64>(arg0VN);
            INT64 op2 = ConstantValue<INT64>(arg1VN);

            switch (func)
            {
                case VNF_ADD_OVF:
                    return !CheckedOps::AddOverflows(op1, op2, CheckedOps::Signed);
                case VNF_SUB_OVF:
                    return !CheckedOps::SubOverflows(op1, op2, CheckedOps::Signed);
                case VNF_MUL_OVF:
                    return !CheckedOps::MulOverflows(op1, op2, CheckedOps::Signed);
                case VNF_ADD_UN_OVF:
                    return !CheckedOps::AddOverflows(op1, op2, CheckedOps::Unsigned);
                case VNF_SUB_UN_OVF:
                    return !CheckedOps::SubOverflows(op1, op2, CheckedOps::Unsigned);
                case VNF_MUL_UN_OVF:
                    return !CheckedOps::MulOverflows(op1, op2, CheckedOps::Unsigned);
                default:
                    assert(!"Unexpected checked operation in VNEvalShouldFold");
                    return false;
            }
        }
        else
        {
            assert(!"Unexpected type in VNEvalShouldFold for overflow arithmetic");
            return false;
        }
    }

    // Is this a checked cast that will always throw an exception or one with an implementation-defined result?
    if (VNFuncIsNumericCast(func))
    {
        var_types castFromType = TypeOfVN(arg0VN);

        // By policy, we do not fold conversions from floating-point types that result in
        // overflow, as the value the C++ compiler gives us does not always match our own codegen.
        if ((func == VNF_CastOvf) || varTypeIsFloating(castFromType))
        {
            var_types castToType;
            bool      fromUnsigned;
            GetCastOperFromVN(arg1VN, &castToType, &fromUnsigned);

            switch (castFromType)
            {
                case TYP_INT:
                    return !CheckedOps::CastFromIntOverflows(GetConstantInt32(arg0VN), castToType, fromUnsigned);
                case TYP_LONG:
                    return !CheckedOps::CastFromLongOverflows(GetConstantInt64(arg0VN), castToType, fromUnsigned);
                case TYP_FLOAT:
                    return !CheckedOps::CastFromFloatOverflows(GetConstantSingle(arg0VN), castToType);
                case TYP_DOUBLE:
                    return !CheckedOps::CastFromDoubleOverflows(GetConstantDouble(arg0VN), castToType);
                default:
                    return false;
            }
        }
    }

    return true;
}

//----------------------------------------------------------------------------------------
//  EvalUsingMathIdentity
//                   - Attempts to evaluate 'func' by using mathematical identities
//                     that can be applied to 'func'.
//
// Arguments:
//    typ            - The type of the resulting ValueNum produced by 'func'
//    func           - Any binary VNFunc
//    arg0VN         - The ValueNum of the first argument to 'func'
//    arg1VN         - The ValueNum of the second argument to 'func'
//
// Return Value:     - When successful a  ValueNum for the expression is returned.
//                     When unsuccessful NoVN is returned.
//
ValueNum ValueNumStore::EvalUsingMathIdentity(var_types typ, VNFunc func, ValueNum arg0VN, ValueNum arg1VN)
{
    ValueNum resultVN = NoVN; // set default result to unsuccessful

    if (typ == TYP_BYREF) // We don't want/need to optimize a zero byref
    {
        return resultVN; // return the unsuccessful value
    }

    // (0 + x) == x
    // (x + 0) == x
    // This identity does not apply for floating point (when x == -0.0).
    auto identityForAddition = [=]() -> ValueNum {
        if (!varTypeIsFloating(typ))
        {
            ValueNum ZeroVN = VNZeroForType(typ);
            if (arg0VN == ZeroVN)
            {
                return arg1VN;
            }
            else if (arg1VN == ZeroVN)
            {
                return arg0VN;
            }
        }

        return NoVN;
    };

    // (x - 0) == x
    // (x - x) == 0
    // This identity does not apply for floating point (when x == -0.0).
    auto identityForSubtraction = [=]() -> ValueNum {
        if (!varTypeIsFloating(typ))
        {
            ValueNum ZeroVN = VNZeroForType(typ);
            if (arg1VN == ZeroVN)
            {
                return arg0VN;
            }
            else if (arg0VN == arg1VN)
            {
                return ZeroVN;
            }
        }

        return NoVN;
    };

    // These identities do not apply for floating point.
    auto identityForMultiplication = [=]() -> ValueNum {
        if (!varTypeIsFloating(typ))
        {
            // (0 * x) == 0
            // (x * 0) == 0
            ValueNum ZeroVN = VNZeroForType(typ);
            if (arg0VN == ZeroVN)
            {
                return ZeroVN;
            }
            else if (arg1VN == ZeroVN)
            {
                return ZeroVN;
            }

            // (x * 1) == x
            // (1 * x) == x
            ValueNum OneVN = VNOneForType(typ);
            if (arg0VN == OneVN)
            {
                return arg1VN;
            }
            else if (arg1VN == OneVN)
            {
                return arg0VN;
            }
        }

        return NoVN;
    };

    // We have ways of evaluating some binary functions.
    if (func < VNF_Boundary)
    {
        ValueNum ZeroVN;
        ValueNum OneVN;

        switch (genTreeOps(func))
        {
            case GT_ADD:
                resultVN = identityForAddition();
                break;

            case GT_SUB:
                resultVN = identityForSubtraction();
                break;

            case GT_MUL:
                resultVN = identityForMultiplication();
                break;

            case GT_DIV:
            case GT_UDIV:
                // (x / 1) == x
                // This identity does not apply for floating point
                //
                if (!varTypeIsFloating(typ))
                {
                    OneVN = VNOneForType(typ);
                    if (arg1VN == OneVN)
                    {
                        resultVN = arg0VN;
                    }
                }
                break;

            case GT_OR:
            case GT_XOR:
                // (0 | x) == x,  (0 ^ x) == x
                // (x | 0) == x,  (x ^ 0) == x
                ZeroVN = VNZeroForType(typ);
                if (arg0VN == ZeroVN)
                {
                    resultVN = arg1VN;
                }
                else if (arg1VN == ZeroVN)
                {
                    resultVN = arg0VN;
                }
                break;

            case GT_AND:
                // (x & 0) == 0
                // (0 & x) == 0
                ZeroVN = VNZeroForType(typ);
                if (arg0VN == ZeroVN)
                {
                    resultVN = ZeroVN;
                }
                else if (arg1VN == ZeroVN)
                {
                    resultVN = ZeroVN;
                }
                break;

            case GT_LSH:
            case GT_RSH:
            case GT_RSZ:
            case GT_ROL:
            case GT_ROR:
                // (x << 0)  == x
                // (x >> 0)  == x
                // (x rol 0) == x
                // (x ror 0) == x
                ZeroVN = VNZeroForType(typ);
                if (arg1VN == ZeroVN)
                {
                    resultVN = arg0VN;
                }
                // (0 << x)  == 0
                // (0 >> x)  == 0
                // (0 rol x) == 0
                // (0 ror x) == 0
                if (arg0VN == ZeroVN)
                {
                    resultVN = ZeroVN;
                }
                break;

            case GT_EQ:
                // (null == non-null) == false
                // (non-null == null) == false
                if (((arg0VN == VNForNull()) && IsKnownNonNull(arg1VN)) ||
                    ((arg1VN == VNForNull()) && IsKnownNonNull(arg0VN)))
                {
                    resultVN = VNZeroForType(typ);
                    break;
                }
                // (x == x) == true (integer only)
                FALLTHROUGH;
            case GT_GE:
            case GT_LE:
                // (x <= x) == true (integer only)
                // (x >= x) == true (integer only)
                if ((arg0VN == arg1VN) && varTypeIsIntegralOrI(TypeOfVN(arg0VN)))
                {
                    resultVN = VNOneForType(typ);
                }
                break;

            case GT_NE:
                // (null != non-null) == true
                // (non-null != null) == true
                if (((arg0VN == VNForNull()) && IsKnownNonNull(arg1VN)) ||
                    ((arg1VN == VNForNull()) && IsKnownNonNull(arg0VN)))
                {
                    resultVN = VNOneForType(typ);
                }
                // (x != x) == false (integer only)
                else if ((arg0VN == arg1VN) && varTypeIsIntegralOrI(TypeOfVN(arg0VN)))
                {
                    resultVN = VNZeroForType(typ);
                }
                break;

            case GT_GT:
            case GT_LT:
                // (x > x) == false (integer & floating point)
                // (x < x) == false (integer & floating point)
                if (arg0VN == arg1VN)
                {
                    resultVN = VNZeroForType(typ);
                }
                break;

            default:
                break;
        }
    }
    else // must be a VNF_ function
    {
        switch (func)
        {
            case VNF_ADD_OVF:
            case VNF_ADD_UN_OVF:
                resultVN = identityForAddition();
                break;

            case VNF_SUB_OVF:
            case VNF_SUB_UN_OVF:
                resultVN = identityForSubtraction();
                break;

            case VNF_MUL_OVF:
            case VNF_MUL_UN_OVF:
                resultVN = identityForMultiplication();
                break;

            case VNF_LT_UN:
                // (x < 0) == false
                // (x < x) == false
                std::swap(arg0VN, arg1VN);
                FALLTHROUGH;
            case VNF_GT_UN:
                // (0 > x) == false
                // (x > x) == false
                // None of the above identities apply to floating point comparisons.
                // For example, (NaN > NaN) is true instead of false because these are
                // unordered comparisons.
                if (varTypeIsIntegralOrI(TypeOfVN(arg0VN)) &&
                    ((arg0VN == VNZeroForType(TypeOfVN(arg0VN))) || (arg0VN == arg1VN)))
                {
                    resultVN = VNZeroForType(typ);
                }
                break;

            case VNF_GE_UN:
                // (x >= 0) == true
                // (x >= x) == true
                std::swap(arg0VN, arg1VN);
                FALLTHROUGH;
            case VNF_LE_UN:
                // (0 <= x) == true
                // (x <= x) == true
                // Unlike (x < x) and (x > x), (x >= x) and (x <= x) also apply to floating
                // point comparisons: x is either equal to itself or is unordered if it's NaN.
                if ((varTypeIsIntegralOrI(TypeOfVN(arg0VN)) && (arg0VN == VNZeroForType(TypeOfVN(arg0VN)))) ||
                    (arg0VN == arg1VN))
                {
                    resultVN = VNOneForType(typ);
                }
                break;

            default:
                break;
        }
    }
    return resultVN;
}

//------------------------------------------------------------------------
// VNForExpr: Opaque value number that is equivalent to itself but unique
//    from all other value numbers.
//
// Arguments:
//    block - BasicBlock where the expression that produces this value occurs.
//            May be nullptr to force conservative "could be anywhere" interpretation.
//     typ - Type of the expression in the IR
//
// Return Value:
//    A new value number distinct from any previously generated, that compares as equal
//    to itself, but not any other value number, and is annotated with the given
//    type and block.

ValueNum ValueNumStore::VNForExpr(BasicBlock* block, var_types typ)
{
    BasicBlock::loopNumber loopNum;
    if (block == nullptr)
    {
        loopNum = BasicBlock::MAX_LOOP_NUM;
    }
    else
    {
        loopNum = block->bbNatLoopNum;
    }

    // VNForFunc(typ, func, vn) but bypasses looking in the cache
    //
    VNDefFunc1Arg        fstruct(VNF_MemOpaque, loopNum);
    Chunk* const         c                 = GetAllocChunk(typ, CEA_Func1);
    unsigned const       offsetWithinChunk = c->AllocVN();
    VNDefFunc1Arg* const chunkSlots        = reinterpret_cast<VNDefFunc1Arg*>(c->m_defs);

    chunkSlots[offsetWithinChunk] = fstruct;

    ValueNum resultVN = c->m_baseVN + offsetWithinChunk;
    return resultVN;
}

var_types ValueNumStore::GetFieldType(CORINFO_FIELD_HANDLE fieldHandle, ClassLayout** fieldLayout)
{
    CORINFO_CLASS_HANDLE typeHandle = NO_CLASS_HANDLE;
    var_types            type = CorTypeToVarType(m_pComp->info.compCompHnd->getFieldType(fieldHandle, &typeHandle));

    if (type == TYP_STRUCT)
    {
        *fieldLayout = m_pComp->typGetObjLayout(typeHandle);
        type         = m_pComp->typGetStructType(*fieldLayout);
    }
    else
    {
        *fieldLayout = nullptr;
    }

    return type;
}

ValueNum ValueNumStore::MapInsertStructField(
    ValueNumKind vnk, ValueNum map, var_types mapType, FieldSeqNode* fieldSeq, ValueNum value, var_types storeType)
{
    assert(varTypeIsStruct(mapType));
    assert(!fieldSeq->IsBoxedValueField());

    struct
    {
        var_types fieldType;
        var_types mapType;
        ValueNum  fieldVN;
        ValueNum  mapVN;
    } fields[FieldSeqNode::MaxLength];

    unsigned count = 0;

    for (; fieldSeq != nullptr; fieldSeq = fieldSeq->GetNext(), count++)
    {
        assert(count < _countof(fields));

        CORINFO_FIELD_HANDLE fieldHandle = fieldSeq->GetFieldHandle();

        fields[count].fieldVN   = VNForFieldHandle(fieldHandle);
        fields[count].fieldType = CorTypeToVarType(m_pComp->info.compCompHnd->getFieldType(fieldHandle));
    }

    fields[0].mapVN   = map;
    fields[0].mapType = mapType;

    for (unsigned i = 0; i < count - 1; i++)
    {
        assert(varTypeIsStruct(fields[i].fieldType));
        fields[i + 1].mapVN   = VNForMapSelect(vnk, fields[i].fieldType, fields[i].mapVN, fields[i].fieldVN);
        fields[i + 1].mapType = fields[i].fieldType;
    }

    // TODO-MIKE: This is nonsense, it tries to coerce the stored value to the store type but the
    // stored value should already have the correct type (e.g. ASG(IND.double, some_float_value)
    // is invalid IR to begin with, there's no need to handle such a case). At the same time this
    // completely ignores the field type so if one stores to a FLOAT field by using an INT indir
    // chaos ensues. Do these poeple think before writing code?!?
    value = VNApplySelectorsAssignTypeCoerce(value, storeType);

    for (unsigned i = 1; i <= count; i++)
    {
        value = VNForMapStore(fields[count - 1].mapType, fields[count - i].mapVN, fields[count - i].fieldVN, value);
    }

    return value;
}

ValueNumPair ValueNumStore::MapInsertStructField(
    ValueNumPair map, var_types mapType, FieldSeqNode* fieldSeq, ValueNumPair value, var_types storeType)
{
    return {MapInsertStructField(VNK_Liberal, map.GetLiberal(), mapType, fieldSeq, value.GetLiberal(), storeType),
            MapInsertStructField(VNK_Conservative, map.GetConservative(), mapType, fieldSeq, value.GetConservative(),
                                 storeType)};
}

ValueNum ValueNumStore::MapExtractStructField(ValueNumKind  vnk,
                                              ValueNum      map,
                                              FieldSeqNode* fieldSeq,
                                              var_types     loadType)
{
    ClassLayout* fieldLayout = nullptr;

    for (; fieldSeq != nullptr; fieldSeq = fieldSeq->GetNext())
    {
        noway_assert(fieldSeq->IsField());

        var_types fieldType = GetFieldType(fieldSeq->GetFieldHandle(), &fieldLayout);
        map                 = VNForMapSelect(vnk, fieldType, map, VNForFieldHandle(fieldSeq->GetFieldHandle()));
    }

    return VNApplySelectorsTypeCheck(map, fieldLayout, loadType);
}

ValueNumPair ValueNumStore::MapExtractStructField(ValueNumPair map, FieldSeqNode* fieldSeq, var_types loadType)
{
    ClassLayout* fieldLayout = nullptr;
    var_types    fieldType;

    for (; fieldSeq != nullptr; fieldSeq = fieldSeq->GetNext())
    {
        noway_assert(fieldSeq->IsField());

        fieldType = GetFieldType(fieldSeq->GetFieldHandle(), &fieldLayout);

        ValueNum fieldVN = VNForFieldHandle(fieldSeq->GetFieldHandle());
        map.SetLiberal(VNForMapSelect(VNK_Liberal, fieldType, map.GetLiberal(), fieldVN));
        map.SetConservative(VNForMapSelect(VNK_Conservative, fieldType, map.GetConservative(), fieldVN));
    }

    map.SetLiberal(VNApplySelectorsTypeCheck(map.GetLiberal(), fieldLayout, loadType));
    map.SetConservative(VNApplySelectorsTypeCheck(map.GetConservative(), fieldLayout, loadType));

    return map;
}

ValueNum ValueNumStore::VNApplySelectorsTypeCheck(ValueNum vn, ClassLayout* layout, var_types loadType)
{
    var_types type = TypeOfVN(vn);

    if (loadType == type)
    {
        return vn;
    }

    unsigned structSize = layout == nullptr ? 0 : layout->GetSize();
    unsigned size       = (type == TYP_STRUCT) ? structSize : varTypeSize(type);
    unsigned loadSize   = varTypeSize(loadType);

    if (loadSize > size)
    {
        JITDUMP("    *** Value of size %u loaded as wider type %s\n", size, varTypeName(loadType));
        return VNMakeNormalUnique(vn);
    }

    if (varTypeIsStruct(loadType))
    {
        JITDUMP("    *** Value of type %s loaded as %s\n", varTypeName(type), varTypeName(loadType));
        return VNMakeNormalUnique(vn);
    }

    // TODO-MIKE-Fix: This is nonsense in reinterpretation case (e.g. load INT field as FLOAT).
    // It produces a VNF_Cast as if it is a INT to FLOAT cast but this is actually a bitcast.
    // See vn-ind-reinterpret.il.
    return VNForCast(vn, loadType, type);
}

ValueNum ValueNumStore::VNApplySelectorsAssignTypeCoerce(ValueNum srcVN, var_types storeType)
{
    var_types srcType = TypeOfVN(srcVN);

    if (storeType == srcType)
    {
        return srcVN;
    }

    if (IsVNConstant(srcVN) && (srcType == varActualType(storeType)))
    {
        // We record INT constants for small int fields.
        return srcVN;
    }

    if (varTypeIsStruct(storeType))
    {
        JITDUMP("    *** Value of type %s stored as %s\n", varTypeName(srcType), varTypeName(storeType));
        return VNMakeNormalUnique(srcVN);
    }

    // TODO-MIKE-Review: This is probably just as bogus as in VNApplySelectorsTypeCheck...
    // This also generates bizarre struct to scalar casts, though that's probably caused
    // by an issue in the calling code. In fact, the whole thing is probably bogus because
    // it involves the stored value's type and the store type, which are expected to be
    // the same anyway - attempting to store a FLOAT value using an IND indirection is
    // invalid IR that should not exist, not something that needs automation coercion.
    // Well, small int stores might need this but even then, the problem is that this
    // thing appears to ignore the type of the field being stored to...
    return VNForCast(srcVN, storeType, srcType);
}

bool ValueNumStore::IsVNNotAField(ValueNum vn)
{
    return m_chunks.Get(GetChunkNum(vn))->m_attribs == CEA_NotAField;
}

ValueNum ValueNumStore::VNForFieldSeq(FieldSeqNode* fieldSeq)
{
    if (fieldSeq == nullptr)
    {
        return VNZeroForType(TYP_I_IMPL);
    }

    if (fieldSeq == FieldSeqStore::NotAField())
    {
        // We always allocate a new, unique VN so that "Not a field" addresses are distinct.
        Chunk*   c                 = GetAllocChunk(TYP_I_IMPL, CEA_NotAField);
        unsigned offsetWithinChunk = c->AllocVN();
        ValueNum result            = c->m_baseVN + offsetWithinChunk;
        return result;
    }

    ValueNum fieldSeqVN = VNForFunc(TYP_I_IMPL, VNF_FieldSeq, VNForHostPtr(fieldSeq));

#ifdef DEBUG
    if (m_pComp->verbose)
    {
        printf("    %s ", varTypeName(TYP_I_IMPL));
        m_pComp->vnPrint(fieldSeqVN, 1);
        printf("\n");
    }
#endif

    return fieldSeqVN;
}

FieldSeqNode* ValueNumStore::FieldSeqVNToFieldSeq(ValueNum vn)
{
    if (vn == VNZeroForType(TYP_I_IMPL))
    {
        return nullptr;
    }

    VNFuncApp funcApp;
    bool      isFunc = GetVNFunc(vn, &funcApp);
    assert(isFunc);

    if (funcApp.m_func == VNF_NotAField)
    {
        return FieldSeqStore::NotAField();
    }

    assert(funcApp.m_func == VNF_FieldSeq);
    return ConstantHostPtr<FieldSeqNode>(funcApp.m_args[0]);
}

ValueNum ValueNumStore::FieldSeqVNAppend(ValueNum fsVN1, ValueNum fsVN2)
{
    FieldSeqNode* fieldSeq1 = FieldSeqVNToFieldSeq(fsVN1);
    FieldSeqNode* fieldSeq2 = FieldSeqVNToFieldSeq(fsVN2);

    ValueNum fieldSeqVN = VNForFieldSeq(m_pComp->GetFieldSeqStore()->Append(fieldSeq1, fieldSeq2));

#ifdef DEBUG
    if (m_pComp->verbose)
    {
        printf("    %s ", varTypeName(TYP_I_IMPL));
        m_pComp->vnPrint(fieldSeqVN, 1);
        printf("\n");
    }
#endif

    return fieldSeqVN;
}

ValueNum ValueNumStore::ExtendPtrVN(GenTreeOp* add)
{
    assert(add->OperIs(GT_ADD));

    ArrayInfo arrInfo;
    if (m_pComp->optIsArrayElemAddr(add, &arrInfo))
    {
        ValueNum indexVN = ExtractArrayElementIndex(arrInfo);

        if (indexVN != NoVN)
        {
            ValueNum elemTypeEqVN = VNForTypeNum(arrInfo.m_elemTypeNum);
            JITDUMP("    VNForTypeNum(elemTypeNum: %s) is " FMT_VN "\n",
                    m_pComp->typIsLayoutNum(arrInfo.m_elemTypeNum)
                        ? m_pComp->typGetLayoutByNum(arrInfo.m_elemTypeNum)->GetClassName()
                        : varTypeName(static_cast<var_types>(arrInfo.m_elemTypeNum)),
                    elemTypeEqVN);

            // We take the "VNNormalValue"s here, because if either has exceptional outcomes,
            // they will be captured as part of the value of the composite "addr" operation...
            ValueNum arrVN = VNNormalValue(arrInfo.m_arrayExpr->gtVNPair.GetLiberal());
            indexVN        = VNNormalValue(indexVN);

#ifdef DEBUG
            if (m_pComp->verbose)
            {
                printf("    Index VN is " FMT_VN " ", indexVN);
                vnDump(m_pComp, indexVN);
                printf("\n");
            }
#endif

            FieldSeqNode* fieldSeq = arrInfo.m_elemOffsetConst->GetFieldSeq()->GetNext();

            if (FieldSeqNode* zeroFieldSeq = m_pComp->GetZeroOffsetFieldSeq(add))
            {
                fieldSeq = m_pComp->GetFieldSeqStore()->Append(fieldSeq, zeroFieldSeq);
            }

            ValueNum fldSeqVN = VNForFieldSeq(fieldSeq);

            return VNForFunc(TYP_BYREF, VNF_PtrToArrElem, elemTypeEqVN, arrVN, indexVN, fldSeqVN);
        }
    }

    if (GenTreeIntCon* intCon = add->GetOp(1)->IsIntCon())
    {
        FieldSeqNode* fldSeq = intCon->GetFieldSeq();
        if ((fldSeq != nullptr) && !fldSeq->IsArrayElement())
        {
            return ExtendPtrVN(add->GetOp(0)->gtVNPair, fldSeq, static_cast<target_size_t>(intCon->GetValue()));
        }
    }

    return NoVN;
}

ValueNum ValueNumStore::ExtractArrayElementIndex(const ArrayInfo& arrayInfo)
{
    assert(arrayInfo.m_arrayExpr->TypeIs(TYP_REF));
    assert(arrayInfo.m_elemOffsetConst->GetFieldSeq()->IsArrayElement() &&
           (varActualType(arrayInfo.m_elemOffsetConst->GetType()) == TYP_I_IMPL));
    assert((arrayInfo.m_elemOffsetExpr == nullptr) ||
           (varActualType(arrayInfo.m_elemOffsetExpr->GetType()) == TYP_I_IMPL));

    target_size_t elemSize;

    if (m_pComp->typIsLayoutNum(arrayInfo.m_elemTypeNum))
    {
        elemSize = m_pComp->typGetLayoutByNum(arrayInfo.m_elemTypeNum)->GetSize();
    }
    else
    {
        elemSize = varTypeSize(static_cast<var_types>(arrayInfo.m_elemTypeNum));
    }

    // Note that while the index is a signed integer (int32 or native int) the offset is
    // really unsigned. Also, using unsigned values avoids C++ signed integer overflow.

    target_size_t offset   = static_cast<target_size_t>(arrayInfo.m_elemOffsetConst->GetValue());
    ValueNum      offsetVN = NoVN;

    if (arrayInfo.m_elemOffsetExpr != nullptr)
    {
        offsetVN = VNNormalValue(arrayInfo.m_elemOffsetExpr->gtVNPair.GetLiberal());
        assert(varActualType(TypeOfVN(offsetVN)) == TYP_I_IMPL);

        if (IsVNConstant(offsetVN))
        {
            offset += CoercedConstantValue<target_size_t>(offsetVN);
            offsetVN = NoVN;
        }
    }

    // The offset contains the array data offset, remove it so we can determine
    // the index as offset / elemSize.

    offset -= arrayInfo.m_elemOffsetConst->GetFieldSeq()->GetArrayDataOffs();

    // The offset should now be a multiple of the element size, unless it also
    // contains the offset of a struct field. Then the field sequence should
    // also include a struct field sequence.

    target_size_t fieldOffset = offset % elemSize;

    if (fieldOffset != 0)
    {
        assert(arrayInfo.m_elemOffsetConst->GetFieldSeq()->GetNext()->IsField());

        // TODO-MIKE-Cleanup: Would be good to actually retrieve the field offset
        // from the field sequence but that currently requires calling the VM.
        // Anyway, this path is pretty much never hit due to the COMMA added by
        // fgMorphArrayIndex.

        offset -= fieldOffset;
        assert(offset % elemSize == 0);
    }

    // Note that this index isn't necessarily the same as the original index.
    // The offset computation might have overflowed, especially on 32 bit targets,
    // so what we get is only congruent to the original index. For an INT array "a"
    // and "a[int.MaxValue]" we get (on 32 bit targets)
    //     0x7FFF'FFFF * 4 / 4 = 0xFFFF'FFFC / 4 = 0x3FFF'FFFF
    // Also, for something like "a[i - 3]" we get
    //     0xFFFF'FFFD * 4 / 4 = 0xFFFF'FFF4 / 4 = 0x3FFF'FFFD
    // The second case is perhaps a bit unfortunate, a 0xFFFF'FFFD result would have
    // been preferable.
    // It is correct for aliasing purposes ("a[i - 3]" and "a[i + 1073741821]" are
    // indeed aliased, even if "i + 1073741821" is an invalid index as far as range
    // checks are concerned) but other uses may require some extra care.

    // TODO-MIKE-Cleanup: Can we use signed integer division to avoid the second case?
    // It's probably not sufficient since a "negative" offset doesn't indicate that
    // the original index was negative.

    target_size_t index = offset / elemSize;

    if (offsetVN == NoVN)
    {
        // The entire offset is constant so the index is also constant.
        // Ignore indices that exceed INT32_MAX, they definitely don't represent
        // a valid element access and anyway such code will throw an exception.
        // We could also ignore indices that are invalid for the given element
        // type but that requires an extra integer division and it's not clear
        // if there's any real benefit in wasting cycles on that.

        return (index > INT32_MAX) ? NoVN : VNForUPtrSizeIntCon(index);
    }

    // Otherwise the offset is something like "ADD(MUL(i, elemSize), offset)". Morph
    // may transform a MUL into LSH and/or a series of constant multiplications, such
    // as "i * 2 * 3". Peel off constant multiplications to try to find the index.
    // We could use DIV(offset, elemSize) as the index VN but that's just one extra
    // VN we need to allocate.

    // TODO-MIKE-Consider: Do we even need to bother with the index? Can't we just use
    // the offset VN in VNF_PtrToArrElem? Having the index could probably be useful to
    // determine that a[i] and a[i + 1] do not alias but VN doesn't handle that now.

    for (VNFuncApp offsetVNFunc; (elemSize > 1) && GetVNFunc(offsetVN, &offsetVNFunc);)
    {
        ValueNum      unscaledOffsetVN;
        target_size_t scale;

        if (offsetVNFunc.m_func == VNFunc(GT_MUL))
        {
            ValueNum scaleVN;

            if (IsVNConstant(offsetVNFunc.m_args[1]))
            {
                unscaledOffsetVN = offsetVNFunc.m_args[0];
                scaleVN          = offsetVNFunc.m_args[1];
            }
            else if (IsVNConstant(offsetVNFunc.m_args[0]))
            {
                scaleVN          = offsetVNFunc.m_args[0];
                unscaledOffsetVN = offsetVNFunc.m_args[1];
            }
            else
            {
                break;
            }

            scale = CoercedConstantValue<target_size_t>(scaleVN);
        }
        else if (offsetVNFunc.m_func == VNFunc(GT_LSH))
        {
            ValueNum scaleVN;

            if (IsVNConstant(offsetVNFunc.m_args[1]))
            {
                unscaledOffsetVN = offsetVNFunc.m_args[0];
                scaleVN          = offsetVNFunc.m_args[1];
            }
            else
            {
                break;
            }

            scale = CoercedConstantValue<target_size_t>(scaleVN);
            scale = target_size_t(1) << scale;
        }
        else
        {
            break;
        }

        if (scale == elemSize)
        {
            offsetVN = unscaledOffsetVN;
            elemSize = 1;
        }
        else if ((scale < elemSize) && (elemSize % scale == 0))
        {
            offsetVN = unscaledOffsetVN;
            elemSize = elemSize / scale;
        }
        else if ((scale > elemSize) && (scale % elemSize == 0))
        {
            offsetVN = VNForFunc(TYP_I_IMPL, VNFunc(GT_MUL), unscaledOffsetVN, VNForUPtrSizeIntCon(scale / elemSize));
            elemSize = 1;
        }
        else
        {
            // TODO-MIKE-Consider: This doesn't handle cases like "(i * 2) * 3" that get
            // reassociated as "(i * 3) * 2" to take advantage of scaled addressing modes
            // (* 2) and LEA (* 3 = [rax + rax * 2]). We first encounter "* 2" that was
            // part of the index expression and give up, before we find "* 3" from the
            // offset expression. It's unlikely that it matters, the JIT doesn't seem to
            // perform such reassociation and anyway the only gain would be that we don't
            // have to create a new DIV VN.

            break;
        }
    }

    ValueNum indexVN;

    if (elemSize == 1)
    {
        indexVN = offsetVN;
    }
    else
    {
        indexVN = VNForFunc(TYP_I_IMPL, VNFunc(GT_DIV), offsetVN, VNForUPtrSizeIntCon(elemSize));
    }

    if (index != 0)
    {
        indexVN = VNForFunc(TYP_I_IMPL, VNFunc(GT_ADD), indexVN, VNForUPtrSizeIntCon(index));
    }

    return indexVN;
}

ValueNum ValueNumStore::ExtendPtrVN(ValueNumPair addrVNP, FieldSeqNode* fldSeq, target_size_t offset)
{
    assert(fldSeq != nullptr);

    ValueNum opAvnWx = addrVNP.GetLiberal();
    assert(VNIsValid(opAvnWx));
    ValueNum opAvn;
    ValueNum opAvnx;
    VNUnpackExc(opAvnWx, &opAvn, &opAvnx);
    assert(VNIsValid(opAvn) && VNIsValid(opAvnx));

    VNFuncApp funcApp;
    if (!GetVNFunc(opAvn, &funcApp))
    {
        return NoVN;
    }

    ValueNum res = NoVN;

    if (funcApp.m_func == VNF_LclAddr)
    {
#ifdef DEBUG
        // For PtrToLoc, lib == cons.
        VNFuncApp consFuncApp;
        assert(GetVNFunc(VNNormalValue(addrVNP.GetConservative()), &consFuncApp) && consFuncApp.Equals(funcApp));
#endif

        ValueNum newOffsetVN   = VNForUPtrSizeIntCon(ConstantValue<target_size_t>(funcApp.m_args[1]) + offset);
        ValueNum newFieldSeqVN = FieldSeqVNAppend(funcApp.m_args[2], VNForFieldSeq(fldSeq));

        res = VNForFunc(TYP_I_IMPL, VNF_LclAddr, funcApp.m_args[0], newOffsetVN, newFieldSeqVN);
    }
    else if (funcApp.m_func == VNF_PtrToStatic)
    {
        res = VNForFunc(TYP_BYREF, VNF_PtrToStatic, FieldSeqVNAppend(funcApp.m_args[0], VNForFieldSeq(fldSeq)));
    }
    else if (funcApp.m_func == VNF_PtrToArrElem)
    {
        res = VNForFunc(TYP_BYREF, VNF_PtrToArrElem, funcApp.m_args[0], funcApp.m_args[1], funcApp.m_args[2],
                        FieldSeqVNAppend(funcApp.m_args[3], VNForFieldSeq(fldSeq)));
    }

    return res == NoVN ? res : VNWithExc(res, opAvnx);
}

void Compiler::vnComma(GenTreeOp* comma)
{
    ValueNumPair op1vnp;
    ValueNumPair op1Xvnp;
    vnStore->VNPUnpackExc(comma->GetOp(0)->GetVNP(), &op1vnp, &op1Xvnp);

    ValueNumPair op2vnp;
    ValueNumPair op2Xvnp = ValueNumStore::VNPForEmptyExcSet();
    GenTree*     op2     = comma->GetOp(1);

    if (op2->OperIsIndir() && ((op2->gtFlags & GTF_IND_ASG_LHS) != 0))
    {
        // If op2 represents the lhs of an assignment then we give a VNForVoid for the lhs
        op2vnp = ValueNumPair(ValueNumStore::VNForVoid(), ValueNumStore::VNForVoid());
    }
    else
    {
        vnStore->VNPUnpackExc(op2->gtVNPair, &op2vnp, &op2Xvnp);
    }

    comma->SetVNP(vnStore->VNPWithExc(op2vnp, vnStore->VNPExcSetUnion(op1Xvnp, op2Xvnp)));
}

void Compiler::vnAssignment(GenTreeOp* asg)
{
    assert(asg->OperIs(GT_ASG));

    // TODO-MIKE: This is missing operand exceptions...
    asg->gtVNPair.SetBoth(ValueNumStore::VNForVoid());

    GenTree* store = asg->GetOp(0);
    GenTree* value = asg->GetOp(1);

    for (; store->OperIs(GT_COMMA); store = store->AsOp()->GetOp(1))
    {
        store->gtVNPair.SetBoth(ValueNumStore::VNForVoid());
    }

    if (store->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        vnLocalStore(store->AsLclVarCommon(), asg, value);
    }
    else
    {
        vnIndirStore(store->AsIndir(), asg, value);
    }
}

void Compiler::vnLocalStore(GenTreeLclVarCommon* store, GenTreeOp* asg, GenTree* value)
{
    assert(store->OperIs(GT_LCL_VAR, GT_LCL_FLD) && ((store->gtFlags & GTF_VAR_DEF) != 0));
    assert(!GetMemorySsaMap(GcHeap)->Lookup(asg));

    LclVarDsc* lcl = lvaGetDesc(store);

    if (lcl->IsAddressExposed())
    {
        vnClearByRefExposed(asg);

        return;
    }

    assert(!GetMemorySsaMap(ByrefExposed)->Lookup(asg));

    if (!lcl->IsInSsa())
    {
        return;
    }

    ValueNumPair valueVNP;

    if (store->TypeIs(TYP_STRUCT) && value->OperIs(GT_CNS_INT))
    {
        assert(value->AsIntCon()->GetValue() == 0);

        valueVNP.SetBoth(vnStore->VNForZeroMap());
    }
    else
    {
        valueVNP = vnStore->VNPNormalPair(value->GetVNP());

        if (value->GetType() != store->GetType())
        {
            // TODO-MIKE: This is dubious. In general both sides of the assignment have the same type, modulo small int.
            // While we could narrow the value here it's not clear if there's a good reason to do it because we may also
            // need to do it when loading. And using the signedness of the value's type doesn't make a lot of sense
            // since
            // for small int type the value is really INT and the signedness does not matter.
            // There are special cases like REF/BYREF and BYREF/I_IMPL conversions but it's not clear if using a
            // VNF_Cast
            // for those makes sense, VNF_BitCast might be preferrable. Besides, the REF/BYREF is also handled below by
            // replacing the value VN with a new, unique one. So why bother casting to begin with?
            bool fromUnsigned = varTypeIsUnsigned(value->GetType());
            valueVNP          = vnStore->VNPairForCast(valueVNP, store->GetType(), value->GetType(), fromUnsigned);
        }

        if ((value->GetType() != store->GetType()) && value->TypeIs(TYP_REF))
        {
            // If we have an unsafe IL assignment of a TYP_REF to a non-ref (typically a TYP_BYREF)
            // then don't propagate this ValueNumber to the lhs, instead create a new unique VN
            valueVNP.SetBoth(vnStore->VNForExpr(compCurBB, store->GetType()));
        }
    }

    unsigned lclDefSsaNum = store->GetSsaNum();

    if (GenTreeLclFld* lclFld = store->IsLclFld())
    {
        assert(((lclFld->gtFlags & GTF_VAR_USEASG) != 0) == lclFld->IsPartialLclFld(this));

        if (!lclFld->HasFieldSeq())
        {
            valueVNP.SetBoth(vnStore->VNForExpr(compCurBB, varActualType(lcl->GetType())));
        }
        else
        {
            ValueNumPair currentVNP;

            if ((store->gtFlags & GTF_VAR_USEASG) == 0)
            {
                // If the LCL_FLD exactly overlaps the local we can ignore the existing value,
                // just insert the new value into a zero map.
                currentVNP.SetBoth(vnStore->VNForZeroMap());
            }
            else
            {
                currentVNP = lcl->GetPerSsaData(store->GetSsaNum())->GetVNP();
            }

            valueVNP = vnStore->MapInsertStructField(currentVNP, lcl->GetType(), lclFld->GetFieldSeq(), valueVNP,
                                                     lclFld->GetType());
        }

        lclDefSsaNum = GetSsaNumForLocalVarDef(store);
    }

    lcl->GetPerSsaData(lclDefSsaNum)->SetVNP(valueVNP);
    store->SetVNP(valueVNP);

#ifdef DEBUG
    if (verbose)
    {
        printf("[%06u] ", store->GetID());
        gtDispNodeName(store);
        gtDispLeaf(store, nullptr);
        printf(" => ");
        vnpPrint(valueVNP, 1);
        printf("\n");
    }
#endif
}

void Compiler::vnLocalLoad(GenTreeLclVar* load)
{
    assert(load->OperIs(GT_LCL_VAR) && ((load->gtFlags & GTF_VAR_DEF) == 0));

    LclVarDsc* lcl = lvaGetDesc(load);

    if (lcl->IsAddressExposed())
    {
        ValueNum addrVN = vnStore->VNForFunc(TYP_I_IMPL, VNF_LclAddr, vnStore->VNForIntCon(load->GetLclNum()),
                                             vnStore->VNZeroForType(TYP_I_IMPL), vnStore->VNForFieldSeq(nullptr));

        load->gtVNPair.SetBoth(fgValueNumberByrefExposedLoad(load->GetType(), addrVN));

        return;
    }

    if (!lcl->IsInSsa())
    {
        load->gtVNPair.SetBoth(vnStore->VNForExpr(compCurBB, load->GetType()));

        return;
    }

    ValueNumPair vnp = lcl->GetPerSsaData(load->GetSsaNum())->m_vnPair;

    assert(vnp.GetLiberal() != ValueNumStore::NoVN);

    unsigned valSize = varTypeSize(varActualType(load->GetType()));
    unsigned lclSize = varTypeSize(varActualType(lcl->GetType()));

    if (valSize != lclSize)
    {
        // Expected type mismatch case is LONG local loaded as INT, ignore everything else.
        if (load->TypeIs(TYP_INT) && lcl->TypeIs(TYP_LONG))
        {
            vnp = vnStore->VNPairForCast(vnp, TYP_INT, TYP_LONG);
        }
        else
        {
            vnp.SetBoth(vnStore->VNForExpr(compCurBB, load->GetType()));
        }
    }

    // A BYREF local may have a zero offset field sequence that needs to be added.
    if (load->TypeIs(TYP_BYREF))
    {
        if (FieldSeqNode* fieldSeq = GetZeroOffsetFieldSeq(load))
        {
            ValueNum extendVN = vnStore->ExtendPtrVN(vnp, fieldSeq, 0);

            if (extendVN != ValueNumStore::NoVN)
            {
                vnp.SetBoth(extendVN);
            }
        }
    }

    load->SetVNP(vnp);
}

void Compiler::vnLocalFieldLoad(GenTreeLclFld* load)
{
    assert(load->OperIs(GT_LCL_FLD) && ((load->gtFlags & GTF_VAR_DEF) == 0));

    LclVarDsc*   lcl = lvaGetDesc(load);
    ValueNumPair vnp;

    if (!lcl->IsInSsa() || !load->HasFieldSeq())
    {
        vnp.SetBoth(vnStore->VNForExpr(compCurBB, load->GetType()));
    }
    else
    {
        assert(varTypeIsStruct(lcl->GetType()));

        vnp = lcl->GetPerSsaData(load->GetSsaNum())->GetVNP();
        vnp = vnStore->MapExtractStructField(vnp, load->GetFieldSeq(), load->GetType());
    }

    load->SetVNP(vnp);
}

void Compiler::vnIndirStore(GenTreeIndir* store, GenTreeOp* asg, GenTree* value)
{
    assert(asg->OperIs(GT_ASG));

    if (store->IsVolatile())
    {
        // For volatile stores, first mutate the heap. This prevents previous
        // stores from being visible after the store.
        vnClearGcHeap(store DEBUGARG("volatile store"));
    }

    if (asg->TypeIs(TYP_STRUCT))
    {
        assert(store->OperIs(GT_OBJ, GT_BLK));

        if (GenTreeLclVarCommon* lclAddr = store->AsIndir()->GetAddr()->IsLocalAddrExpr())
        {
            assert(lvaGetDesc(lclAddr)->IsAddressExposed());
            vnClearByRefExposed(asg);
        }
        else
        {
            // For now, arbitrary side effect on GcHeap/ByrefExposed.
            // TODO-CQ: Why not be complete, and get this case right?
            vnClearGcHeap(asg DEBUGARG("indirect struct store"));
        }

        return;
    }

    assert(store->OperIs(GT_IND));

    ValueNum valueVN = vnStore->VNNormalValue(value->GetLiberalVN());

    if (value->GetType() != store->GetType())
    {
        // TODO-MIKE: This is dubious. In general both sides of the assignment have the same type, modulo small int.
        // While we could narrow the value here it's not clear if there's a good reason to do it because we may also
        // need to do it when loading. And using the signedness of the value's type doesn't make a lot of sense since
        // for small int type the value is really INT and the signedness does not matter.
        // There are special cases like REF/BYREF and BYREF/I_IMPL conversions but it's not clear if using a VNF_Cast
        // for those makes sense, VNF_BitCast might be preferrable. Besides, the REF/BYREF is also handled below by
        // replacing the value VN with a new, unique one. So why bother casting to begin with?
        bool fromUnsigned = varTypeIsUnsigned(value->GetType());
        valueVN           = vnStore->VNForCast(valueVN, store->GetType(), value->GetType(), fromUnsigned);
    }

    if ((value->GetType() != store->GetType()) && value->TypeIs(TYP_REF))
    {
        // If we have an unsafe IL assignment of a TYP_REF to a non-ref (typically a TYP_BYREF)
        // then don't propagate this ValueNumber to the lhs, instead create a new unique VN
        valueVN = vnStore->VNForExpr(compCurBB, store->GetType());
    }

    GenTree*  addr   = store->GetAddr();
    ValueNum  addrVN = addr->gtVNPair.GetLiberal();
    VNFuncApp funcApp;

    if (vnStore->GetVNFunc(vnStore->VNNormalValue(addrVN), &funcApp) && (funcApp.m_func == VNF_PtrToStatic))
    {
        FieldSeqNode* fieldSeq = vnStore->FieldSeqVNToFieldSeq(funcApp.m_args[0]);

        if (fieldSeq == FieldSeqStore::NotAField())
        {
            vnClearGcHeap(asg DEBUGARG("static field store"));
        }
        else
        {
            ValueNum heapVN = vnStaticFieldStore(fieldSeq, valueVN, store->GetType());
            vnUpdateGcHeap(asg, heapVN DEBUGARG("static field store"));
        }

        return;
    }

    if (vnStore->GetVNFunc(vnStore->VNNormalValue(addrVN), &funcApp) && (funcApp.m_func == VNF_PtrToArrElem))
    {
        ValueNum heapVN = vnArrayElemStore(funcApp, valueVN, store->GetType());
        vnUpdateGcHeap(asg, heapVN DEBUGARG("array element store"));

        return;
    }

    GenTree* obj;
    if (FieldSeqNode* fieldSeq = optIsFieldAddr(addr, &obj))
    {
        ValueNum heapVN;

        if (obj == nullptr)
        {
            heapVN = vnStaticFieldStore(fieldSeq, valueVN, store->GetType());
        }
        else
        {
            ValueNum objVN = vnStore->VNNormalValue(obj->GetLiberalVN());
            heapVN         = vnObjFieldStore(objVN, fieldSeq, valueVN, store->GetType());
        }

        vnUpdateGcHeap(asg, heapVN DEBUGARG(obj == nullptr ? "static field store" : "object field store"));

        return;
    }

    if (GenTreeLclVarCommon* dstLclNode = addr->IsLocalAddrExpr())
    {
        assert(lvaGetDesc(dstLclNode)->IsAddressExposed());
        vnClearByRefExposed(asg);

        return;
    }

    vnClearGcHeap(asg DEBUGARG("indirect store"));
}

void Compiler::vnIndirLoad(GenTreeIndir* load)
{
    assert(load->OperIs(GT_IND, GT_OBJ, GT_BLK));
    assert((load->gtFlags & GTF_IND_ASG_LHS) == 0);

    GenTree*     addr = load->GetAddr();
    ValueNumPair addrVNP;
    ValueNumPair addrExcVNP;
    vnStore->VNPUnpackExc(addr->GetVNP(), &addrVNP, &addrExcVNP);

    VNFuncApp funcApp;

    if (load->IsInvariant())
    {
        assert(!load->IsVolatile());

        if (load->OperIs(GT_IND) && addr->TypeIs(TYP_REF) && load->TypeIs(TYP_I_IMPL) &&
            vnStore->GetVNFunc(addrVNP.GetLiberal(), &funcApp) && (funcApp.m_func == VNF_JitNew) && addrVNP.BothEqual())
        {
            load->SetVNP(vnStore->VNPWithExc({funcApp.m_args[0], funcApp.m_args[0]}, addrExcVNP));
            return;
        }

        if ((load->gtFlags & GTF_IND_NONNULL) != 0)
        {
            assert((load->gtFlags & GTF_IND_NONFAULTING) != 0);

            load->SetVNP(vnStore->VNPairForFunc(load->GetType(), VNF_NonNullIndirect, addrVNP));

            if (addr->IsIntCon())
            {
                assert(addrExcVNP.BothEqual() && (addrExcVNP.GetLiberal() == ValueNumStore::VNForEmptyExcSet()));
            }
            else
            {
                assert(false && "it's not expected to be hit at the moment, but can be allowed.");
                // tree->gtVNPair = vnStore->VNPWithExc(tree->gtVNPair, addrXvnp);
            }

            return;
        }

        ValueNum     readOnlyHeap = ValueNumStore::VNForROH();
        ValueNumPair vnp;
        vnp.SetLiberal(vnStore->VNForMapSelect(VNK_Liberal, TYP_REF, readOnlyHeap, addrVNP.GetLiberal()));
        vnp.SetConservative(
            vnStore->VNForMapSelect(VNK_Conservative, TYP_REF, readOnlyHeap, addrVNP.GetConservative()));

        load->SetVNP(vnStore->VNPWithExc(vnp, addrExcVNP));

        return;
    }

    // The conservative VN of a load is always a new, unique VN.
    ValueNum conservativeVN = vnStore->VNForExpr(compCurBB, load->GetType());

    if (load->IsVolatile())
    {
        // We just mutate GcHeap/ByrefExposed for volatile loads, and then do the load as normal.
        //
        // This allows:
        //   1: read s;
        //   2: volatile read s;
        //   3: read s;
        //
        // We should never assume that the values loaded by 1 and 2 are the same (because the heap was
        // mutated in between them) but we *should* be able to prove that the values loaded by 2 and
        // 3 are the same.

        vnClearGcHeap(load DEBUGARG("volatile load"));

        load->SetVNP(vnStore->VNPWithExc({conservativeVN, conservativeVN}, addrExcVNP));

        return;
    }

    // TODO-MIKE: Static fields are a mess. The address is sometimes CLS_VAR_ADDR and
    // sometimes CNS_INT. The later generates a VNHandle instead of VNF_PtrToStatic
    // and the handle can be recognized as being a static address but it lacks the
    // field handle/sequence so we can't do much with it. Ideally CNS_INT would also
    // generate VNF_PtrToStatic but then CSE barfs because it expects constant VNs
    // for constant nodes and VNF_PtrToStatic isn't a constant.
    // In the case of STRUCT static fields, CLS_VAR_ADDR is rare,
    // the C# compiler seems to prefer LDSFLDA-LDFLDA-LDFLD to LDSFLD-LDFLD-LDFLD and
    // the importer always uses CNS_INT for LDSFLDA. Not good for testing. Moreover
    // VN doesn't seem to recognize CNS_INT on its own, it only recognizes it together
    // with a subsequent STRUCT field access, which does not involve VNF_PtrToStatic.
    // This is somewhat risky because not matter what the IR pattern is we should end
    // up using the same field sequence in all cases, otherwise we may end up with
    // loads not correctly seeing previously stored values.

    if (vnStore->GetVNFunc(addrVNP.GetLiberal(), &funcApp) && (funcApp.m_func == VNF_PtrToStatic))
    {
        FieldSeqNode* fieldSeq = vnStore->FieldSeqVNToFieldSeq(funcApp.m_args[0]);
        ValueNumPair  valueVNP;

        if (fieldSeq == FieldSeqStore::NotAField())
        {
            valueVNP.SetBoth(conservativeVN);
        }
        else
        {
            valueVNP.SetLiberal(vnStaticFieldLoad(fieldSeq, load->GetType()));
            valueVNP.SetConservative(conservativeVN);
        }

        load->SetVNP(vnStore->VNPWithExc(valueVNP, addrExcVNP));

        return;
    }

    if (vnStore->GetVNFunc(addrVNP.GetLiberal(), &funcApp) && (funcApp.m_func == VNF_PtrToArrElem))
    {
        ValueNum valueVN = vnArrayElemLoad(funcApp, addrExcVNP.GetLiberal(), load->GetType());

        load->SetVNP({valueVN, conservativeVN});

        // TODO-CQ: what to do here about exceptions? We don't have the array and index conservative
        // values, so we don't have their exceptions. Maybe we should.

        return;
    }

    GenTree* obj = nullptr;
    if (FieldSeqNode* fieldSeq = optIsFieldAddr(addr, &obj))
    {
        ValueNum valueVN;

        if (obj == nullptr)
        {
            valueVN = vnStaticFieldLoad(fieldSeq, load->GetType());
        }
        else
        {
            valueVN = vnObjFieldLoad(vnStore->VNNormalValue(obj->GetLiberalVN()), fieldSeq, load->GetType());
        }

        load->SetVNP(vnStore->VNPWithExc({valueVN, conservativeVN}, addrExcVNP));

        return;
    }

    if (load->TypeIs(TYP_I_IMPL) && addr->TypeIs(TYP_REF))
    {
        // TODO-MIKE-CQ: Handle method table pointer loads properly. This code gives these
        // loads unique VNs, this prevents CSEing of such loads.
        // These loads can be handled by fgValueNumberByrefExposedLoad below but then it
        // turns out that CSEing is a problem for optAssertionIsSubtype - it specifically
        // looks for indirs that load the method table pointer.

        load->SetVNP(vnStore->VNPWithExc({conservativeVN, conservativeVN}, addrExcVNP));

        return;
    }

    ValueNum valueVN = fgValueNumberByrefExposedLoad(load->GetType(), addr->GetLiberalVN());
    load->SetVNP(vnStore->VNPWithExc({valueVN, conservativeVN}, addrExcVNP));
}

ValueNum Compiler::vnStaticFieldStore(FieldSeqNode* fieldSeq, ValueNum valueVN, var_types storeType)
{
    CORINFO_FIELD_HANDLE fieldHandle = fieldSeq->GetFieldHandle();
    assert(info.compCompHnd->isFieldStatic(fieldHandle));

    fieldSeq = fieldSeq->GetNext();

    if ((fieldSeq != nullptr) && fieldSeq->IsBoxedValueField())
    {
        fieldSeq = fieldSeq->GetNext();
    }

    ValueNum heapVN = fgCurMemoryVN[GcHeap];
    INDEBUG(vnPrintHeapVN(heapVN));

    ValueNum fieldVN = vnStore->VNForFieldHandle(fieldHandle);
    ValueNum fieldMapVN;

    if (fieldSeq == nullptr)
    {
        // TODO-MIKE: This may need VNApplySelectorsAssignTypeCoerce(valueVN)...
        fieldMapVN = valueVN;
    }
    else
    {
        ClassLayout* fieldLayout;
        var_types    fieldType = vnStore->GetFieldType(fieldHandle, &fieldLayout);
        assert(varTypeIsStruct(fieldType));

        fieldMapVN = vnStore->VNForMapSelect(VNK_Liberal, TYP_STRUCT, heapVN, fieldVN);
        fieldMapVN = vnStore->MapInsertStructField(VNK_Liberal, fieldMapVN, fieldType, fieldSeq, valueVN, storeType);
    }

    return vnStore->VNForMapStore(TYP_STRUCT, heapVN, fieldVN, fieldMapVN);
}

ValueNum Compiler::vnStaticFieldLoad(FieldSeqNode* fieldSeq, var_types loadType)
{
    CORINFO_FIELD_HANDLE fieldHandle = fieldSeq->GetFieldHandle();
    assert(info.compCompHnd->isFieldStatic(fieldHandle));
    ClassLayout* fieldLayout;
    var_types    fieldType = vnStore->GetFieldType(fieldHandle, &fieldLayout);

    ValueNum heapVN = fgCurMemoryVN[GcHeap];
    INDEBUG(vnPrintHeapVN(heapVN));

    ValueNum fieldVN = vnStore->VNForFieldHandle(fieldHandle);

    fieldSeq = fieldSeq->GetNext();

    if ((fieldSeq == nullptr) && (loadType == TYP_REF) && varTypeIsStruct(fieldType))
    {
        // This actually loads a boxed object reference for a static struct field.
        return vnStore->VNForMapSelect(VNK_Liberal, TYP_REF, heapVN, fieldVN);
    }

    if ((fieldSeq != nullptr) && fieldSeq->IsBoxedValueField())
    {
        fieldSeq = fieldSeq->GetNext();
    }

    ValueNum vn = vnStore->VNForMapSelect(VNK_Liberal, fieldType, heapVN, fieldVN);

    if (fieldSeq != nullptr)
    {
        return vnStore->MapExtractStructField(VNK_Liberal, vn, fieldSeq, loadType);
    }
    else
    {
        return vnStore->VNApplySelectorsTypeCheck(vn, fieldLayout, loadType);
    }
}

ValueNum Compiler::vnObjFieldStore(ValueNum objVN, FieldSeqNode* fieldSeq, ValueNum valueVN, var_types storeType)
{
    CORINFO_FIELD_HANDLE fieldHandle = fieldSeq->GetFieldHandle();
    assert(!info.compCompHnd->isFieldStatic(fieldHandle));
    // Currently struct stores are not handled.
    assert(storeType != TYP_STRUCT);

    ValueNum heapVN = fgCurMemoryVN[GcHeap];
    INDEBUG(vnPrintHeapVN(heapVN));

    ValueNum fieldVN    = vnStore->VNForFieldHandle(fieldHandle);
    ValueNum fieldMapVN = vnStore->VNForMapSelect(VNK_Liberal, TYP_STRUCT, heapVN, fieldVN);

    fieldSeq = fieldSeq->GetNext();

    if (fieldSeq != nullptr)
    {
        ClassLayout* fieldLayout;
        var_types    fieldType = vnStore->GetFieldType(fieldHandle, &fieldLayout);
        assert(varTypeIsStruct(fieldType));

        ValueNum objFieldMapVN = vnStore->VNForMapSelect(VNK_Liberal, fieldType, fieldMapVN, objVN);
        valueVN = vnStore->MapInsertStructField(VNK_Liberal, objFieldMapVN, fieldType, fieldSeq, valueVN, storeType);
    }

    // TODO-MIKE: This likely needs VNApplySelectorsAssignTypeCoerce(valueVN),
    // previously that was incorrectly done when storing to the heap map. It's
    // the store value that may need coercion, the field map value is always
    // treated as if it's a struct.
    fieldMapVN = vnStore->VNForMapStore(TYP_STRUCT, fieldMapVN, objVN, valueVN);

    return vnStore->VNForMapStore(TYP_STRUCT, heapVN, fieldVN, fieldMapVN);
}

ValueNum Compiler::vnObjFieldLoad(ValueNum objVN, FieldSeqNode* fieldSeq, var_types loadType)
{
    CORINFO_FIELD_HANDLE fieldHandle = fieldSeq->GetFieldHandle();
    assert(!info.compCompHnd->isFieldStatic(fieldHandle));
    ClassLayout* fieldLayout;
    var_types    fieldType = vnStore->GetFieldType(fieldHandle, &fieldLayout);

    ValueNum vn = fgCurMemoryVN[GcHeap];
    INDEBUG(vnPrintHeapVN(vn));

    vn = vnStore->VNForMapSelect(VNK_Liberal, TYP_STRUCT, vn, vnStore->VNForFieldHandle(fieldHandle));
    vn = vnStore->VNForMapSelect(VNK_Liberal, fieldType, vn, objVN);

    fieldSeq = fieldSeq->GetNext();

    if (fieldSeq != nullptr)
    {
        return vnStore->MapExtractStructField(VNK_Liberal, vn, fieldSeq, loadType);
    }
    else
    {
        return vnStore->VNApplySelectorsTypeCheck(vn, fieldLayout, loadType);
    }
}

ValueNum Compiler::vnArrayElemStore(const VNFuncApp& elemAddr, ValueNum valueVN, var_types storeType)
{
    assert(elemAddr.m_func == VNF_PtrToArrElem);

    ValueNum      elemTypeVN = elemAddr.m_args[0];
    ValueNum      arrayVN    = elemAddr.m_args[1];
    ValueNum      indexVN    = elemAddr.m_args[2];
    FieldSeqNode* fieldSeq   = vnStore->FieldSeqVNToFieldSeq(elemAddr.m_args[3]);

    assert(arrayVN == vnStore->VNNormalValue(arrayVN));
    assert(indexVN == vnStore->VNNormalValue(indexVN));

    unsigned     elemTypeNum = static_cast<unsigned>(vnStore->ConstantValue<ssize_t>(elemAddr.m_args[0]));
    ClassLayout* elemLayout  = typIsLayoutNum(elemTypeNum) ? typGetLayoutByNum(elemTypeNum) : nullptr;
    var_types    elemType = elemLayout == nullptr ? static_cast<var_types>(elemTypeNum) : typGetStructType(elemLayout);

    ValueNum heapVN = fgCurMemoryVN[GcHeap];
    INDEBUG(vnPrintHeapVN(heapVN));
    INDEBUG(vnPrintArrayElemAddr(elemAddr));

    // TODO-MIKE: We should get a field sequence only for arrays of structs.
    // This isn't the best place to check this but for now it gets pmi diff
    // working again.

    if (!varTypeIsStruct(elemType) && (fieldSeq != nullptr))
    {
        fieldSeq = FieldSeqNode::NotAField();
    }

    if (fieldSeq == FieldSeqStore::NotAField())
    {
        return vnStore->VNForMapStore(TYP_STRUCT, heapVN, elemTypeVN, vnStore->VNForExpr(compCurBB, TYP_STRUCT));
    }

    // TODO-MIKE: This likely needs VNApplySelectorsAssignTypeCoerce(valueVN)

    ValueNum arrayTypeMapVN = vnStore->VNForMapSelect(VNK_Liberal, TYP_STRUCT, heapVN, elemTypeVN);
    ValueNum arrayMapVN     = vnStore->VNForMapSelect(VNK_Liberal, TYP_STRUCT, arrayTypeMapVN, arrayVN);

    if (fieldSeq != nullptr)
    {
        ValueNum elemVN = vnStore->VNForMapSelect(VNK_Liberal, elemType, arrayMapVN, indexVN);
        valueVN         = vnStore->MapInsertStructField(VNK_Liberal, elemVN, elemType, fieldSeq, valueVN, storeType);
    }

    arrayMapVN     = vnStore->VNForMapStore(TYP_STRUCT, arrayMapVN, indexVN, valueVN);
    arrayTypeMapVN = vnStore->VNForMapStore(TYP_STRUCT, arrayTypeMapVN, arrayVN, arrayMapVN);
    return vnStore->VNForMapStore(TYP_STRUCT, heapVN, elemTypeVN, arrayTypeMapVN);
}

ValueNum Compiler::vnArrayElemLoad(const VNFuncApp& elemAddr, ValueNum excVN, var_types loadType)
{
    assert(elemAddr.m_func == VNF_PtrToArrElem);

    ValueNum      elemTypeVN = elemAddr.m_args[0];
    ValueNum      arrayVN    = elemAddr.m_args[1];
    ValueNum      indexVN    = elemAddr.m_args[2];
    FieldSeqNode* fieldSeq   = vnStore->FieldSeqVNToFieldSeq(elemAddr.m_args[3]);

    assert(arrayVN == vnStore->VNNormalValue(arrayVN));
    assert(indexVN == vnStore->VNNormalValue(indexVN));

    unsigned     elemTypeNum = static_cast<unsigned>(vnStore->ConstantValue<ssize_t>(elemAddr.m_args[0]));
    ClassLayout* elemLayout  = typIsLayoutNum(elemTypeNum) ? typGetLayoutByNum(elemTypeNum) : nullptr;
    var_types    elemType = elemLayout == nullptr ? static_cast<var_types>(elemTypeNum) : typGetStructType(elemLayout);

    ValueNum heapVN = fgCurMemoryVN[GcHeap];
    INDEBUG(vnPrintHeapVN(heapVN));
    INDEBUG(vnPrintArrayElemAddr(elemAddr));

    // TODO-MIKE: We should get a field sequence only for arrays of structs.
    // This isn't the best place to check this but for now it gets pmi diff
    // working again.

    if (!varTypeIsStruct(elemType) && (fieldSeq != nullptr))
    {
        fieldSeq = FieldSeqNode::NotAField();
    }

    if (fieldSeq == FieldSeqStore::NotAField())
    {
        return vnStore->VNForExpr(compCurBB, elemType);
    }

    ValueNum arrayTypeMapVN = vnStore->VNForMapSelect(VNK_Liberal, TYP_STRUCT, heapVN, elemTypeVN);
    ValueNum arrayMapVN     = vnStore->VNForMapSelect(VNK_Liberal, TYP_STRUCT, arrayTypeMapVN, arrayVN);
    ValueNum valueVN        = vnStore->VNForMapSelect(VNK_Liberal, elemType, arrayMapVN, indexVN);

    if (fieldSeq != nullptr)
    {
        valueVN = vnStore->MapExtractStructField(VNK_Liberal, valueVN, fieldSeq, loadType);
    }
    else
    {
        valueVN = vnStore->VNApplySelectorsTypeCheck(valueVN, elemLayout, loadType);
    }

    return vnStore->VNWithExc(valueVN, excVN);
}

void Compiler::vnNullCheck(GenTreeIndir* node)
{
    assert(node->OperIs(GT_NULLCHECK));

    ValueNum     value = ValueNumStore::VNForVoid();
    ValueNumPair exset = vnAddNullPtrExset(node->GetAddr()->GetVNP());
    node->SetVNP(vnStore->VNPWithExc({value, value}, exset));
}

void Compiler::vnArrayLength(GenTreeArrLen* node)
{
    VNFunc vnf = GetVNFuncForNode(node);
    assert(ValueNumStore::VNFuncIsLegal(vnf));

    GenTree*     array    = node->GetArray();
    ValueNumPair arrayVNP = vnStore->VNPNormalPair(array->GetVNP());

    // If we are fetching the array length for an array ref that came from global memory
    // then for CSE safety we must use the conservative value number for both.
    if ((array->gtFlags & GTF_GLOB_REF) != 0)
    {
        arrayVNP.SetBoth(arrayVNP.GetConservative());
    }

    ValueNumPair value = vnStore->VNPairForFunc(node->GetType(), vnf, arrayVNP);
    ValueNumPair exset = vnAddNullPtrExset(array->GetVNP());
    node->SetVNP(vnStore->VNPWithExc(value, exset));
}

void Compiler::vnCmpXchg(GenTreeCmpXchg* node)
{
    vnClearGcHeap(node DEBUGARG("cmpxchg intrinsic"));

    ValueNum     value = vnStore->VNForExpr(compCurBB, node->GetType());
    ValueNumPair exset = vnAddNullPtrExset(node->GetAddr()->GetVNP());
    exset              = vnStore->VNPUnionExcSet(node->GetValue()->GetVNP(), exset);
    exset              = vnStore->VNPUnionExcSet(node->GetCompareValue()->GetVNP(), exset);
    node->SetVNP(vnStore->VNPWithExc({value, value}, exset));
}

void Compiler::vnInterlocked(GenTreeOp* node)
{
    assert(node->OperIs(GT_XORR, GT_XAND, GT_XADD, GT_XCHG));

    vnClearGcHeap(node DEBUGARG("interlocked intrinsic"));

    ValueNum     value = vnStore->VNForExpr(compCurBB, node->GetType());
    ValueNumPair exset = vnAddNullPtrExset(node->GetOp(0)->GetVNP());
    exset              = vnStore->VNPUnionExcSet(node->GetOp(1)->GetVNP(), exset);
    node->SetVNP(vnStore->VNPWithExc({value, value}, exset));
}

ValueNum Compiler::fgValueNumberByrefExposedLoad(var_types type, ValueNum addrVN)
{
    if (type == TYP_STRUCT)
    {
        // We can't assign a value number for a read of a struct as we can't determine
        // how many bytes will be read by this load, so return a new unique value number
        return vnStore->VNForExpr(compCurBB, TYP_STRUCT);
    }

    addrVN = vnStore->VNNormalValue(addrVN);

    ValueNum memoryVN = fgCurMemoryVN[ByrefExposed];
    // The memoization for VNFunc applications does not factor in the result type, so
    // VNF_ByrefExposedLoad takes the loaded type as an explicit parameter.
    ValueNum typeVN = vnStore->VNForIntCon(type);

    return vnStore->VNForFunc(type, VNF_ByrefExposedLoad, typeVN, addrVN, memoryVN);
}

var_types ValueNumStore::TypeOfVN(ValueNum vn)
{
    if (vn == NoVN)
    {
        return TYP_UNDEF;
    }

    Chunk* c = m_chunks.Get(GetChunkNum(vn));
    return c->m_typ;
}

//------------------------------------------------------------------------
// LoopOfVN: If the given value number is VNF_MemOpaque, VNF_MapStore, or
//    VNF_MemoryPhiDef, return the loop number where the memory update occurs,
//    otherwise returns MAX_LOOP_NUM.
//
// Arguments:
//    vn - Value number to query
//
// Return Value:
//    The memory loop number, which may be BasicBlock::NOT_IN_LOOP.
//    Returns BasicBlock::MAX_LOOP_NUM if this VN is not a memory value number.
//
BasicBlock::loopNumber ValueNumStore::LoopOfVN(ValueNum vn)
{
    VNFuncApp funcApp;
    if (GetVNFunc(vn, &funcApp))
    {
        if (funcApp.m_func == VNF_MemOpaque)
        {
            return (BasicBlock::loopNumber)funcApp.m_args[0];
        }
        else if (funcApp.m_func == VNF_MapStore)
        {
            return (BasicBlock::loopNumber)funcApp.m_args[3];
        }
        else if (funcApp.m_func == VNF_PhiMemoryDef)
        {
            BasicBlock* const block = reinterpret_cast<BasicBlock*>(ConstantValue<ssize_t>(funcApp.m_args[0]));
            return block->bbNatLoopNum;
        }
    }

    return BasicBlock::MAX_LOOP_NUM;
}

bool ValueNumStore::IsVNConstant(ValueNum vn)
{
    if (vn == NoVN)
    {
        return false;
    }
    Chunk* c = m_chunks.Get(GetChunkNum(vn));
    if (c->m_attribs == CEA_Const)
    {
        return vn != VNForVoid(); // Void is not a "real" constant -- in the sense that it represents no value.
    }
    else
    {
        return c->m_attribs == CEA_Handle;
    }
}

bool ValueNumStore::IsVNInt32Constant(ValueNum vn)
{
    if (!IsVNConstant(vn))
    {
        return false;
    }

    return TypeOfVN(vn) == TYP_INT;
}

GenTreeFlags ValueNumStore::GetHandleFlags(ValueNum vn)
{
    assert(IsVNHandle(vn));
    Chunk*    c      = m_chunks.Get(GetChunkNum(vn));
    unsigned  offset = ChunkOffset(vn);
    VNHandle* handle = &static_cast<VNHandle*>(c->m_defs)[offset];
    return handle->m_flags;
}

bool ValueNumStore::IsVNHandle(ValueNum vn)
{
    if (vn == NoVN)
    {
        return false;
    }

    Chunk* c = m_chunks.Get(GetChunkNum(vn));
    return c->m_attribs == CEA_Handle;
}

bool ValueNumStore::IsVNConstantBound(ValueNum vn)
{
    // Do we have "var < 100"?
    if (vn == NoVN)
    {
        return false;
    }

    VNFuncApp funcAttr;
    if (!GetVNFunc(vn, &funcAttr))
    {
        return false;
    }
    if (funcAttr.m_func != (VNFunc)GT_LE && funcAttr.m_func != (VNFunc)GT_GE && funcAttr.m_func != (VNFunc)GT_LT &&
        funcAttr.m_func != (VNFunc)GT_GT)
    {
        return false;
    }

    return IsVNInt32Constant(funcAttr.m_args[0]) != IsVNInt32Constant(funcAttr.m_args[1]);
}

void ValueNumStore::GetConstantBoundInfo(ValueNum vn, ConstantBoundInfo* info)
{
    assert(IsVNConstantBound(vn));
    assert(info);

    // Do we have var < 100?
    VNFuncApp funcAttr;
    GetVNFunc(vn, &funcAttr);

    bool isOp1Const = IsVNInt32Constant(funcAttr.m_args[1]);

    if (isOp1Const)
    {
        info->cmpOper  = funcAttr.m_func;
        info->cmpOpVN  = funcAttr.m_args[0];
        info->constVal = GetConstantInt32(funcAttr.m_args[1]);
    }
    else
    {
        info->cmpOper  = GenTree::SwapRelop((genTreeOps)funcAttr.m_func);
        info->cmpOpVN  = funcAttr.m_args[1];
        info->constVal = GetConstantInt32(funcAttr.m_args[0]);
    }
}

//------------------------------------------------------------------------
// IsVNPositiveInt32Constant: returns true iff vn is a known Int32 constant that is greater then 0
//
// Arguments:
//    vn - Value number to query
bool ValueNumStore::IsVNPositiveInt32Constant(ValueNum vn)
{
    return IsVNInt32Constant(vn) && (ConstantValue<INT32>(vn) > 0);
}

//------------------------------------------------------------------------
// IsVNArrLenUnsignedBound: Checks if the specified vn represents an expression
//    of one of the following forms:
//    - "(uint)i < (uint)len" that implies (0 <= i < len)
//    - "const < (uint)len" that implies "len > const"
//    - "const <= (uint)len" that implies "len > const - 1"
//
// Arguments:
//    vn - Value number to query
//    info - Pointer to an UnsignedCompareCheckedBoundInfo object to return information about
//           the expression. Not populated if the vn expression isn't suitable (e.g. i <= len).
//           This enables optCreateJTrueBoundAssertion to immediately create an OAK_NO_THROW
//           assertion instead of the OAK_EQUAL/NOT_EQUAL assertions created by signed compares
//           (IsVNCompareCheckedBound, IsVNCompareCheckedBoundArith) that require further processing.
//
// Note:
//   For comparisons of the form constant <= length, this returns them as (constant - 1) < length
//
bool ValueNumStore::IsVNUnsignedCompareCheckedBound(ValueNum vn, UnsignedCompareCheckedBoundInfo* info)
{
    VNFuncApp funcApp;

    if (GetVNFunc(vn, &funcApp))
    {
        if ((funcApp.m_func == VNF_LT_UN) || (funcApp.m_func == VNF_GE_UN))
        {
            // We only care about "(uint)i < (uint)len" and its negation "(uint)i >= (uint)len"
            if (IsVNCheckedBound(funcApp.m_args[1]))
            {
                info->vnIdx   = funcApp.m_args[0];
                info->cmpOper = funcApp.m_func;
                info->vnBound = funcApp.m_args[1];
                return true;
            }
            // We care about (uint)len < constant and its negation "(uint)len >= constant"
            else if (IsVNPositiveInt32Constant(funcApp.m_args[1]) && IsVNCheckedBound(funcApp.m_args[0]))
            {
                // Change constant < len into (uint)len >= (constant - 1)
                // to make consuming this simpler (and likewise for it's negation).
                INT32 validIndex = ConstantValue<INT32>(funcApp.m_args[1]) - 1;
                assert(validIndex >= 0);

                info->vnIdx   = VNForIntCon(validIndex);
                info->cmpOper = (funcApp.m_func == VNF_GE_UN) ? VNF_LT_UN : VNF_GE_UN;
                info->vnBound = funcApp.m_args[0];
                return true;
            }
        }
        else if ((funcApp.m_func == VNF_GT_UN) || (funcApp.m_func == VNF_LE_UN))
        {
            // We only care about "(uint)a.len > (uint)i" and its negation "(uint)a.len <= (uint)i"
            if (IsVNCheckedBound(funcApp.m_args[0]))
            {
                info->vnIdx = funcApp.m_args[1];
                // Let's keep a consistent operand order - it's always i < len, never len > i
                info->cmpOper = (funcApp.m_func == VNF_GT_UN) ? VNF_LT_UN : VNF_GE_UN;
                info->vnBound = funcApp.m_args[0];
                return true;
            }
            // Look for constant > (uint)len and its negation "constant <= (uint)len"
            else if (IsVNPositiveInt32Constant(funcApp.m_args[0]) && IsVNCheckedBound(funcApp.m_args[1]))
            {
                // Change constant <= (uint)len to (constant - 1) < (uint)len
                // to make consuming this simpler (and likewise for it's negation).
                INT32 validIndex = ConstantValue<INT32>(funcApp.m_args[0]) - 1;
                assert(validIndex >= 0);

                info->vnIdx   = VNForIntCon(validIndex);
                info->cmpOper = (funcApp.m_func == VNF_LE_UN) ? VNF_LT_UN : VNF_GE_UN;
                info->vnBound = funcApp.m_args[1];
                return true;
            }
        }
    }

    return false;
}

bool ValueNumStore::IsVNCompareCheckedBound(ValueNum vn)
{
    // Do we have "var < len"?
    if (vn == NoVN)
    {
        return false;
    }

    VNFuncApp funcAttr;
    if (!GetVNFunc(vn, &funcAttr))
    {
        return false;
    }
    if (funcAttr.m_func != (VNFunc)GT_LE && funcAttr.m_func != (VNFunc)GT_GE && funcAttr.m_func != (VNFunc)GT_LT &&
        funcAttr.m_func != (VNFunc)GT_GT)
    {
        return false;
    }
    if (!IsVNCheckedBound(funcAttr.m_args[0]) && !IsVNCheckedBound(funcAttr.m_args[1]))
    {
        return false;
    }

    return true;
}

void ValueNumStore::GetCompareCheckedBound(ValueNum vn, CompareCheckedBoundArithInfo* info)
{
    assert(IsVNCompareCheckedBound(vn));

    // Do we have var < a.len?
    VNFuncApp funcAttr;
    GetVNFunc(vn, &funcAttr);

    bool isOp1CheckedBound = IsVNCheckedBound(funcAttr.m_args[1]);
    if (isOp1CheckedBound)
    {
        info->cmpOper = funcAttr.m_func;
        info->cmpOp   = funcAttr.m_args[0];
        info->vnBound = funcAttr.m_args[1];
    }
    else
    {
        info->cmpOper = GenTree::SwapRelop((genTreeOps)funcAttr.m_func);
        info->cmpOp   = funcAttr.m_args[1];
        info->vnBound = funcAttr.m_args[0];
    }
}

bool ValueNumStore::IsVNCheckedBoundArith(ValueNum vn)
{
    // Do we have "a.len +or- var"
    if (vn == NoVN)
    {
        return false;
    }

    VNFuncApp funcAttr;

    return GetVNFunc(vn, &funcAttr) &&                                                     // vn is a func.
           (funcAttr.m_func == (VNFunc)GT_ADD || funcAttr.m_func == (VNFunc)GT_SUB) &&     // the func is +/-
           (IsVNCheckedBound(funcAttr.m_args[0]) || IsVNCheckedBound(funcAttr.m_args[1])); // either op1 or op2 is a.len
}

void ValueNumStore::GetCheckedBoundArithInfo(ValueNum vn, CompareCheckedBoundArithInfo* info)
{
    // Do we have a.len +/- var?
    assert(IsVNCheckedBoundArith(vn));
    VNFuncApp funcArith;
    GetVNFunc(vn, &funcArith);

    bool isOp1CheckedBound = IsVNCheckedBound(funcArith.m_args[1]);
    if (isOp1CheckedBound)
    {
        info->arrOper = funcArith.m_func;
        info->arrOp   = funcArith.m_args[0];
        info->vnBound = funcArith.m_args[1];
    }
    else
    {
        info->arrOper = funcArith.m_func;
        info->arrOp   = funcArith.m_args[1];
        info->vnBound = funcArith.m_args[0];
    }
}

bool ValueNumStore::IsVNCompareCheckedBoundArith(ValueNum vn)
{
    // Do we have: "var < a.len - var"
    if (vn == NoVN)
    {
        return false;
    }

    VNFuncApp funcAttr;
    if (!GetVNFunc(vn, &funcAttr))
    {
        return false;
    }

    // Suitable comparator.
    if (funcAttr.m_func != (VNFunc)GT_LE && funcAttr.m_func != (VNFunc)GT_GE && funcAttr.m_func != (VNFunc)GT_LT &&
        funcAttr.m_func != (VNFunc)GT_GT)
    {
        return false;
    }

    // Either the op0 or op1 is arr len arithmetic.
    if (!IsVNCheckedBoundArith(funcAttr.m_args[0]) && !IsVNCheckedBoundArith(funcAttr.m_args[1]))
    {
        return false;
    }

    return true;
}

void ValueNumStore::GetCompareCheckedBoundArithInfo(ValueNum vn, CompareCheckedBoundArithInfo* info)
{
    assert(IsVNCompareCheckedBoundArith(vn));

    VNFuncApp funcAttr;
    GetVNFunc(vn, &funcAttr);

    // Check whether op0 or op1 is checked bound arithmetic.
    bool isOp1CheckedBoundArith = IsVNCheckedBoundArith(funcAttr.m_args[1]);
    if (isOp1CheckedBoundArith)
    {
        info->cmpOper = funcAttr.m_func;
        info->cmpOp   = funcAttr.m_args[0];
        GetCheckedBoundArithInfo(funcAttr.m_args[1], info);
    }
    else
    {
        info->cmpOper = GenTree::SwapRelop((genTreeOps)funcAttr.m_func);
        info->cmpOp   = funcAttr.m_args[1];
        GetCheckedBoundArithInfo(funcAttr.m_args[0], info);
    }
}

ValueNum ValueNumStore::GetArrForLenVn(ValueNum vn)
{
    if (vn == NoVN)
    {
        return NoVN;
    }

    VNFuncApp funcAttr;
    if (GetVNFunc(vn, &funcAttr) && funcAttr.m_func == (VNFunc)GT_ARR_LENGTH)
    {
        return funcAttr.m_args[0];
    }
    return NoVN;
}

bool ValueNumStore::IsVNNewArr(ValueNum vn, VNFuncApp* funcApp)
{
    if (vn == NoVN)
    {
        return false;
    }
    bool result = false;
    if (GetVNFunc(vn, funcApp))
    {
        result = (funcApp->m_func == VNF_JitNewArr) || (funcApp->m_func == VNF_JitReadyToRunNewArr);
    }
    return result;
}

int ValueNumStore::GetNewArrSize(ValueNum vn)
{
    VNFuncApp funcApp;
    if (IsVNNewArr(vn, &funcApp))
    {
        ValueNum arg1VN = funcApp.m_args[1];
        if (IsVNConstant(arg1VN) && TypeOfVN(arg1VN) == TYP_INT)
        {
            return ConstantValue<int>(arg1VN);
        }
    }
    return 0;
}

bool ValueNumStore::IsVNArrLen(ValueNum vn)
{
    if (vn == NoVN)
    {
        return false;
    }
    VNFuncApp funcAttr;
    return (GetVNFunc(vn, &funcAttr) && funcAttr.m_func == (VNFunc)GT_ARR_LENGTH);
}

bool ValueNumStore::IsVNCheckedBound(ValueNum vn)
{
    bool dummy;
    if (m_checkedBoundVNs.TryGetValue(vn, &dummy))
    {
        // This VN appeared as the conservative VN of the length argument of some
        // GT_ARR_BOUND node.
        return true;
    }
    if (IsVNArrLen(vn))
    {
        // Even if we haven't seen this VN in a bounds check, if it is an array length
        // VN then consider it a checked bound VN.  This facilitates better bounds check
        // removal by ensuring that compares against array lengths get put in the
        // optCseCheckedBoundMap; such an array length might get CSEd with one that was
        // directly used in a bounds check, and having the map entry will let us update
        // the compare's VN so that OptimizeRangeChecks can recognize such compares.
        return true;
    }

    return false;
}

void ValueNumStore::SetVNIsCheckedBound(ValueNum vn)
{
    // This is meant to flag VNs for lengths that aren't known at compile time, so we can
    // form and propagate assertions about them.  Ensure that callers filter out constant
    // VNs since they're not what we're looking to flag, and assertion prop can reason
    // directly about constants.
    assert(!IsVNConstant(vn));
    m_checkedBoundVNs.AddOrUpdate(vn, true);
}

ValueNum ValueNumStore::EvalMathFuncUnary(var_types typ, NamedIntrinsic gtMathFN, ValueNum arg0VN)
{
    assert(arg0VN == VNNormalValue(arg0VN));
    assert(m_pComp->IsMathIntrinsic(gtMathFN));

    // If the math intrinsic is not implemented by target-specific instructions, such as implemented
    // by user calls, then don't do constant folding on it during ReadyToRun. This minimizes precision loss.

    if (IsVNConstant(arg0VN) && (!m_pComp->opts.IsReadyToRun() || m_pComp->IsTargetIntrinsic(gtMathFN)))
    {
        assert(varTypeIsFloating(TypeOfVN(arg0VN)));

        if (typ == TYP_DOUBLE)
        {
            // Both operand and its result must be of the same floating point type.
            assert(typ == TypeOfVN(arg0VN));
            double arg0Val = GetConstantDouble(arg0VN);

            double res = 0.0;
            switch (gtMathFN)
            {
                case NI_System_Math_Abs:
                    res = fabs(arg0Val);
                    break;

                case NI_System_Math_Acos:
                    res = acos(arg0Val);
                    break;

                case NI_System_Math_Acosh:
                    res = acosh(arg0Val);
                    break;

                case NI_System_Math_Asin:
                    res = asin(arg0Val);
                    break;

                case NI_System_Math_Asinh:
                    res = asinh(arg0Val);
                    break;

                case NI_System_Math_Atan:
                    res = atan(arg0Val);
                    break;

                case NI_System_Math_Atanh:
                    res = atanh(arg0Val);
                    break;

                case NI_System_Math_Cbrt:
                    res = cbrt(arg0Val);
                    break;

                case NI_System_Math_Ceiling:
                    res = ceil(arg0Val);
                    break;

                case NI_System_Math_Cos:
                    res = cos(arg0Val);
                    break;

                case NI_System_Math_Cosh:
                    res = cosh(arg0Val);
                    break;

                case NI_System_Math_Exp:
                    res = exp(arg0Val);
                    break;

                case NI_System_Math_Floor:
                    res = floor(arg0Val);
                    break;

                case NI_System_Math_Log:
                    res = log(arg0Val);
                    break;

                case NI_System_Math_Log2:
                    res = log2(arg0Val);
                    break;

                case NI_System_Math_Log10:
                    res = log10(arg0Val);
                    break;

                case NI_System_Math_Sin:
                    res = sin(arg0Val);
                    break;

                case NI_System_Math_Sinh:
                    res = sinh(arg0Val);
                    break;

                case NI_System_Math_Round:
                    res = FloatingPointUtils::round(arg0Val);
                    break;

                case NI_System_Math_Sqrt:
                    res = sqrt(arg0Val);
                    break;

                case NI_System_Math_Tan:
                    res = tan(arg0Val);
                    break;

                case NI_System_Math_Tanh:
                    res = tanh(arg0Val);
                    break;

                default:
                    // the above are the only math intrinsics at the time of this writing.
                    unreached();
            }

            return VNForDoubleCon(res);
        }
        else if (typ == TYP_FLOAT)
        {
            // Both operand and its result must be of the same floating point type.
            assert(typ == TypeOfVN(arg0VN));
            float arg0Val = GetConstantSingle(arg0VN);

            float res = 0.0f;
            switch (gtMathFN)
            {
                case NI_System_Math_Abs:
                    res = fabsf(arg0Val);
                    break;

                case NI_System_Math_Acos:
                    res = acosf(arg0Val);
                    break;

                case NI_System_Math_Acosh:
                    res = acoshf(arg0Val);
                    break;

                case NI_System_Math_Asin:
                    res = asinf(arg0Val);
                    break;

                case NI_System_Math_Asinh:
                    res = asinhf(arg0Val);
                    break;

                case NI_System_Math_Atan:
                    res = atanf(arg0Val);
                    break;

                case NI_System_Math_Atanh:
                    res = atanhf(arg0Val);
                    break;

                case NI_System_Math_Cbrt:
                    res = cbrtf(arg0Val);
                    break;

                case NI_System_Math_Ceiling:
                    res = ceilf(arg0Val);
                    break;

                case NI_System_Math_Cos:
                    res = cosf(arg0Val);
                    break;

                case NI_System_Math_Cosh:
                    res = coshf(arg0Val);
                    break;

                case NI_System_Math_Exp:
                    res = expf(arg0Val);
                    break;

                case NI_System_Math_Floor:
                    res = floorf(arg0Val);
                    break;

                case NI_System_Math_Log:
                    res = logf(arg0Val);
                    break;

                case NI_System_Math_Log2:
                    res = log2f(arg0Val);
                    break;

                case NI_System_Math_Log10:
                    res = log10f(arg0Val);
                    break;

                case NI_System_Math_Sin:
                    res = sinf(arg0Val);
                    break;

                case NI_System_Math_Sinh:
                    res = sinhf(arg0Val);
                    break;

                case NI_System_Math_Round:
                    res = FloatingPointUtils::round(arg0Val);
                    break;

                case NI_System_Math_Sqrt:
                    res = sqrtf(arg0Val);
                    break;

                case NI_System_Math_Tan:
                    res = tanf(arg0Val);
                    break;

                case NI_System_Math_Tanh:
                    res = tanhf(arg0Val);
                    break;

                default:
                    // the above are the only math intrinsics at the time of this writing.
                    unreached();
            }

            return VNForFloatCon(res);
        }
        else
        {
            assert(typ == TYP_INT);
            int res = 0;

            if (gtMathFN == NI_System_Math_ILogB)
            {
                switch (TypeOfVN(arg0VN))
                {
                    case TYP_DOUBLE:
                    {
                        double arg0Val = GetConstantDouble(arg0VN);
                        res            = ilogb(arg0Val);
                        break;
                    }

                    case TYP_FLOAT:
                    {
                        float arg0Val = GetConstantSingle(arg0VN);
                        res           = ilogbf(arg0Val);
                        break;
                    }

                    default:
                        unreached();
                }
            }
            else
            {
                assert(gtMathFN == NI_System_Math_Round);

                switch (TypeOfVN(arg0VN))
                {
                    case TYP_DOUBLE:
                    {
                        double arg0Val = GetConstantDouble(arg0VN);
                        res            = int(FloatingPointUtils::round(arg0Val));
                        break;
                    }

                    case TYP_FLOAT:
                    {
                        float arg0Val = GetConstantSingle(arg0VN);
                        res           = int(FloatingPointUtils::round(arg0Val));
                        break;
                    }

                    default:
                        unreached();
                }
            }

            return VNForIntCon(res);
        }
    }
    else
    {
        assert((typ == TYP_DOUBLE) || (typ == TYP_FLOAT) ||
               ((typ == TYP_INT) && ((gtMathFN == NI_System_Math_ILogB) || (gtMathFN == NI_System_Math_Round))));

        VNFunc vnf = VNF_Boundary;
        switch (gtMathFN)
        {
            case NI_System_Math_Abs:
                vnf = VNF_Abs;
                break;
            case NI_System_Math_Acos:
                vnf = VNF_Acos;
                break;
            case NI_System_Math_Acosh:
                vnf = VNF_Acosh;
                break;
            case NI_System_Math_Asin:
                vnf = VNF_Asin;
                break;
            case NI_System_Math_Asinh:
                vnf = VNF_Asinh;
                break;
            case NI_System_Math_Atan:
                vnf = VNF_Atan;
                break;
            case NI_System_Math_Atanh:
                vnf = VNF_Atanh;
                break;
            case NI_System_Math_Cbrt:
                vnf = VNF_Cbrt;
                break;
            case NI_System_Math_Ceiling:
                vnf = VNF_Ceiling;
                break;
            case NI_System_Math_Cos:
                vnf = VNF_Cos;
                break;
            case NI_System_Math_Cosh:
                vnf = VNF_Cosh;
                break;
            case NI_System_Math_Exp:
                vnf = VNF_Exp;
                break;
            case NI_System_Math_Floor:
                vnf = VNF_Floor;
                break;
            case NI_System_Math_ILogB:
                vnf = VNF_ILogB;
                break;
            case NI_System_Math_Log:
                vnf = VNF_Log;
                break;
            case NI_System_Math_Log2:
                vnf = VNF_Log2;
                break;
            case NI_System_Math_Log10:
                vnf = VNF_Log10;
                break;
            case NI_System_Math_Round:
                if (typ == TYP_DOUBLE)
                {
                    vnf = VNF_RoundDouble;
                }
                else if (typ == TYP_INT)
                {
                    vnf = VNF_RoundInt32;
                }
                else if (typ == TYP_FLOAT)
                {
                    vnf = VNF_RoundSingle;
                }
                else
                {
                    noway_assert(!"Invalid INTRINSIC_Round");
                }
                break;
            case NI_System_Math_Sin:
                vnf = VNF_Sin;
                break;
            case NI_System_Math_Sinh:
                vnf = VNF_Sinh;
                break;
            case NI_System_Math_Sqrt:
                vnf = VNF_Sqrt;
                break;
            case NI_System_Math_Tan:
                vnf = VNF_Tan;
                break;
            case NI_System_Math_Tanh:
                vnf = VNF_Tanh;
                break;
            default:
                unreached(); // the above are the only math intrinsics at the time of this writing.
        }

        return VNForFunc(typ, vnf, arg0VN);
    }
}

ValueNum ValueNumStore::EvalMathFuncBinary(var_types typ, NamedIntrinsic gtMathFN, ValueNum arg0VN, ValueNum arg1VN)
{
    assert(varTypeIsFloating(typ));
    assert(arg0VN == VNNormalValue(arg0VN));
    assert(arg1VN == VNNormalValue(arg1VN));
    assert(m_pComp->IsMathIntrinsic(gtMathFN));

    // If the math intrinsic is not implemented by target-specific instructions, such as implemented
    // by user calls, then don't do constant folding on it during ReadyToRun. This minimizes precision loss.

    if (IsVNConstant(arg0VN) && IsVNConstant(arg1VN) &&
        (!m_pComp->opts.IsReadyToRun() || m_pComp->IsTargetIntrinsic(gtMathFN)))
    {
        if (typ == TYP_DOUBLE)
        {
            // Both the first operand and its result must be of the same floating point type.
            assert(typ == TypeOfVN(arg0VN));
            double arg0Val = GetConstantDouble(arg0VN);

            double res = 0.0;
            switch (gtMathFN)
            {
                case NI_System_Math_Atan2:
                {
                    assert(typ == TypeOfVN(arg1VN));
                    double arg1Val = GetConstantDouble(arg1VN);
                    res            = atan2(arg0Val, arg1Val);
                    break;
                }

                case NI_System_Math_FMod:
                {
                    assert(typ == TypeOfVN(arg1VN));
                    double arg1Val = GetConstantDouble(arg1VN);
                    res            = fmod(arg0Val, arg1Val);
                    break;
                }

                case NI_System_Math_Pow:
                {
                    assert(typ == TypeOfVN(arg1VN));
                    double arg1Val = GetConstantDouble(arg1VN);
                    res            = pow(arg0Val, arg1Val);
                    break;
                }

                default:
                    // the above are the only binary math intrinsics at the time of this writing.
                    unreached();
            }

            return VNForDoubleCon(res);
        }
        else
        {
            // Both operand and its result must be of the same floating point type.
            assert(typ == TYP_FLOAT);
            assert(typ == TypeOfVN(arg0VN));
            float arg0Val = GetConstantSingle(arg0VN);

            float res = 0.0f;
            switch (gtMathFN)
            {
                case NI_System_Math_Atan2:
                {
                    assert(typ == TypeOfVN(arg1VN));
                    float arg1Val = GetConstantSingle(arg1VN);
                    res           = atan2f(arg0Val, arg1Val);
                    break;
                }

                case NI_System_Math_FMod:
                {
                    assert(typ == TypeOfVN(arg1VN));
                    float arg1Val = GetConstantSingle(arg1VN);
                    res           = fmodf(arg0Val, arg1Val);
                    break;
                }

                case NI_System_Math_Pow:
                {
                    assert(typ == TypeOfVN(arg1VN));
                    float arg1Val = GetConstantSingle(arg1VN);
                    res           = powf(arg0Val, arg1Val);
                    break;
                }

                default:
                    // the above are the only binary math intrinsics at the time of this writing.
                    unreached();
            }

            return VNForFloatCon(res);
        }
    }
    else
    {
        VNFunc vnf = VNF_Boundary;

        switch (gtMathFN)
        {
            case NI_System_Math_Atan2:
                vnf = VNF_Atan2;
                break;

            case NI_System_Math_FMod:
                vnf = VNF_FMod;
                break;

            case NI_System_Math_Pow:
                vnf = VNF_Pow;
                break;

            default:
                // the above are the only binary math intrinsics at the time of this writing.
                unreached();
        }

        return VNForFunc(typ, vnf, arg0VN, arg1VN);
    }
}

bool ValueNumStore::IsVNFunc(ValueNum vn)
{
    if (vn == NoVN)
    {
        return false;
    }
    Chunk* c = m_chunks.Get(GetChunkNum(vn));
    switch (c->m_attribs)
    {
        case CEA_NotAField:
        case CEA_Func0:
        case CEA_Func1:
        case CEA_Func2:
        case CEA_Func3:
        case CEA_Func4:
            return true;
        default:
            return false;
    }
}

bool ValueNumStore::GetVNFunc(ValueNum vn, VNFuncApp* funcApp)
{
    if (vn == NoVN)
    {
        return false;
    }

    Chunk*   c      = m_chunks.Get(GetChunkNum(vn));
    unsigned offset = ChunkOffset(vn);
    assert(offset < c->m_numUsed);
    switch (c->m_attribs)
    {
        case CEA_Func4:
        {
            VNDefFunc4Arg* farg4 = &static_cast<VNDefFunc4Arg*>(c->m_defs)[offset];
            funcApp->m_func      = farg4->m_func;
            funcApp->m_arity     = 4;
            funcApp->m_args[0]   = farg4->m_arg0;
            funcApp->m_args[1]   = farg4->m_arg1;
            funcApp->m_args[2]   = farg4->m_arg2;
            funcApp->m_args[3]   = farg4->m_arg3;
            return true;
        }
        case CEA_Func3:
        {
            VNDefFunc3Arg* farg3 = &static_cast<VNDefFunc3Arg*>(c->m_defs)[offset];
            funcApp->m_func      = farg3->m_func;
            funcApp->m_arity     = 3;
            funcApp->m_args[0]   = farg3->m_arg0;
            funcApp->m_args[1]   = farg3->m_arg1;
            funcApp->m_args[2]   = farg3->m_arg2;
            return true;
        }
        case CEA_Func2:
        {
            VNDefFunc2Arg* farg2 = &static_cast<VNDefFunc2Arg*>(c->m_defs)[offset];
            funcApp->m_func      = farg2->m_func;
            funcApp->m_arity     = 2;
            funcApp->m_args[0]   = farg2->m_arg0;
            funcApp->m_args[1]   = farg2->m_arg1;
            return true;
        }
        case CEA_Func1:
        {
            VNDefFunc1Arg* farg1 = &static_cast<VNDefFunc1Arg*>(c->m_defs)[offset];
            funcApp->m_func      = farg1->m_func;
            funcApp->m_arity     = 1;
            funcApp->m_args[0]   = farg1->m_arg0;
            return true;
        }
        case CEA_Func0:
        {
            VNDefFunc0Arg* farg0 = &static_cast<VNDefFunc0Arg*>(c->m_defs)[offset];
            funcApp->m_func      = farg0->m_func;
            funcApp->m_arity     = 0;
            return true;
        }
        case CEA_NotAField:
        {
            funcApp->m_func  = VNF_NotAField;
            funcApp->m_arity = 0;
            return true;
        }
        default:
            return false;
    }
}

bool ValueNumStore::VNIsValid(ValueNum vn)
{
    ChunkNum cn = GetChunkNum(vn);
    if (cn >= m_chunks.Size())
    {
        return false;
    }
    // Otherwise...
    Chunk* c = m_chunks.Get(cn);
    return ChunkOffset(vn) < c->m_numUsed;
}

#ifdef DEBUG

void ValueNumStore::vnDump(Compiler* comp, ValueNum vn, bool isPtr)
{
    printf(" {");
    if (vn == NoVN)
    {
        printf("NoVN");
    }
    else if (IsVNHandle(vn))
    {
        ssize_t val = ConstantValue<ssize_t>(vn);
        printf("Hnd const: 0x%p", dspPtr(val));
    }
    else if (IsVNConstant(vn))
    {
        var_types vnt = TypeOfVN(vn);
        switch (vnt)
        {
            case TYP_BOOL:
            case TYP_BYTE:
            case TYP_UBYTE:
            case TYP_SHORT:
            case TYP_USHORT:
            case TYP_INT:
            case TYP_UINT:
            {
                int val = ConstantValue<int>(vn);
                if (isPtr)
                {
                    printf("PtrCns[%p]", dspPtr(val));
                }
                else
                {
                    printf("IntCns");
                    if ((val > -1000) && (val < 1000))
                    {
                        printf(" %ld", val);
                    }
                    else
                    {
                        printf(" 0x%X", val);
                    }
                }
            }
            break;
            case TYP_LONG:
            case TYP_ULONG:
            {
                INT64 val = ConstantValue<INT64>(vn);
                if (isPtr)
                {
                    printf("LngPtrCns: 0x%p", dspPtr(val));
                }
                else
                {
                    printf("LngCns: ");
                    if ((val > -1000) && (val < 1000))
                    {
                        printf(" %ld", val);
                    }
                    else if ((val & 0xFFFFFFFF00000000LL) == 0)
                    {
                        printf(" 0x%X", val);
                    }
                    else
                    {
                        printf(" 0x%llx", val);
                    }
                }
            }
            break;
            case TYP_FLOAT:
                printf("FltCns[%f]", ConstantValue<float>(vn));
                break;
            case TYP_DOUBLE:
                printf("DblCns[%f]", ConstantValue<double>(vn));
                break;
            case TYP_REF:
                if (vn == VNForNull())
                {
                    printf("null");
                }
                else if (vn == VNForVoid())
                {
                    printf("void");
                }
                else
                {
                    assert(vn == VNForZeroMap());
                    printf("zeroMap");
                }
                break;
            case TYP_BYREF:
                printf("byrefVal");
                break;
            case TYP_STRUCT:
                printf("structVal(zero)");
                break;

#ifdef FEATURE_SIMD
            case TYP_SIMD8:
            case TYP_SIMD12:
            case TYP_SIMD16:
            case TYP_SIMD32:
            {
                // Only the zero constant is currently allowed for SIMD types
                //
                INT64 val = ConstantValue<INT64>(vn);
                assert(val == 0);
                printf(" 0");
            }
            break;
#endif // FEATURE_SIMD

            // These should be unreached.
            default:
                unreached();
        }
    }
    else if (IsVNCompareCheckedBound(vn))
    {
        CompareCheckedBoundArithInfo info;
        GetCompareCheckedBound(vn, &info);
        info.dump(this);
    }
    else if (IsVNCompareCheckedBoundArith(vn))
    {
        CompareCheckedBoundArithInfo info;
        GetCompareCheckedBoundArithInfo(vn, &info);
        info.dump(this);
    }
    else if (IsVNFunc(vn))
    {
        VNFuncApp funcApp;
        GetVNFunc(vn, &funcApp);
        // A few special cases...
        switch (funcApp.m_func)
        {
            case VNF_FieldSeq:
                vnDumpFieldSeq(comp, &funcApp, true);
                break;
            case VNF_MapSelect:
                vnDumpMapSelect(comp, &funcApp);
                break;
            case VNF_MapStore:
                vnDumpMapStore(comp, &funcApp);
                break;
            case VNF_ValWithExc:
                vnDumpValWithExc(comp, &funcApp);
                break;
            case VNF_MemOpaque:
                vnDumpMemOpaque(comp, &funcApp);
                break;
            case VNF_LclAddr:
                vnDumpLclAddr(comp, &funcApp);
                break;
            case VNF_BitCast:
                DumpBitCast(funcApp);
                break;
            case VNF_Cast:
                DumpCast(funcApp);
                break;
            default:
                printf("%s", VNFuncName(funcApp.m_func));
#ifdef FEATURE_HW_INTRINSICS
                if (funcApp.m_func >= VNF_HWI_FIRST)
                {
                    var_types type = VNFuncSimdBaseType(funcApp.m_func);
                    unsigned  size = VNFuncSimdSize(funcApp.m_func);

                    if (type != TYP_UNDEF)
                    {
                        if (size == 0)
                        {
                            printf("<%s>", varTypeName(type));
                        }
                        else
                        {
                            printf("<%s x %u>", varTypeName(type), size / varTypeSize(type));
                        }
                    }
                }
#endif // FEATURE_HW_INTRINSICS
                printf("(");
                for (unsigned i = 0; i < funcApp.m_arity; i++)
                {
                    if (i > 0)
                    {
                        printf(", ");
                    }

                    printf(FMT_VN, funcApp.m_args[i]);

#if FEATURE_VN_DUMP_FUNC_ARGS
                    printf("=");
                    vnDump(comp, funcApp.m_args[i]);
#endif
                }
                printf(")");
        }
    }
    else
    {
        // Otherwise, just a VN with no structure; print just the VN.
        printf("%x", vn);
    }
    printf("}");
}

// Requires "valWithExc" to be a value with an exeception set VNFuncApp.
// Prints a representation of the exeception set on standard out.
void ValueNumStore::vnDumpValWithExc(Compiler* comp, VNFuncApp* valWithExc)
{
    assert(valWithExc->m_func == VNF_ValWithExc); // Precondition.

    ValueNum normVN = valWithExc->m_args[0]; // First arg is the VN from normal execution
    ValueNum excVN  = valWithExc->m_args[1]; // Second arg is the set of possible exceptions

    assert(IsVNFunc(excVN));
    VNFuncApp excSeq;
    GetVNFunc(excVN, &excSeq);

    printf("norm=");
    printf(FMT_VN, normVN);
    vnDump(comp, normVN);
    printf(", exc=");
    printf(FMT_VN, excVN);
    vnDumpExcSeq(comp, &excSeq, true);
}

// Requires "excSeq" to be a ExcSetCons sequence.
// Prints a representation of the set of exceptions on standard out.
void ValueNumStore::vnDumpExcSeq(Compiler* comp, VNFuncApp* excSeq, bool isHead)
{
    assert(excSeq->m_func == VNF_ExcSetCons); // Precondition.

    ValueNum curExc  = excSeq->m_args[0];
    bool     hasTail = (excSeq->m_args[1] != VNForEmptyExcSet());

    if (isHead && hasTail)
    {
        printf("(");
    }

    vnDump(comp, curExc);

    if (hasTail)
    {
        printf(", ");
        assert(IsVNFunc(excSeq->m_args[1]));
        VNFuncApp tail;
        GetVNFunc(excSeq->m_args[1], &tail);
        vnDumpExcSeq(comp, &tail, false);
    }

    if (isHead && hasTail)
    {
        printf(")");
    }
}

void ValueNumStore::vnDumpFieldSeq(Compiler* comp, VNFuncApp* fieldSeq, bool isHead)
{
    assert(fieldSeq->m_func == VNF_FieldSeq);

    printf("FieldSeq(");
    m_pComp->dmpFieldSeqFields(ConstantHostPtr<FieldSeqNode>(fieldSeq->m_args[0]));
    printf(")");
}

void ValueNumStore::vnDumpMapSelect(Compiler* comp, VNFuncApp* mapSelect)
{
    assert(mapSelect->m_func == VNF_MapSelect);

    ValueNum mapVN   = mapSelect->m_args[0];
    ValueNum indexVN = mapSelect->m_args[1];

    printf("MapSelect(");
    comp->vnPrint(mapVN, 0);
    printf(", ");
    comp->vnPrint(indexVN, 0);
    if (IsVNHandle(indexVN) && ((GetHandleFlags(indexVN) & GTF_ICON_FIELD_HDL) != 0))
    {
        CORINFO_FIELD_HANDLE fieldHandle =
            reinterpret_cast<CORINFO_FIELD_HANDLE>(ConstantValue<target_size_t>(indexVN));
        printf(" (%s)", comp->eeGetFieldName(fieldHandle));
    }
    printf(")");
}

void ValueNumStore::vnDumpMapStore(Compiler* comp, VNFuncApp* mapStore)
{
    assert(mapStore->m_func == VNF_MapStore);

    ValueNum mapVN    = mapStore->m_args[0];
    ValueNum indexVN  = mapStore->m_args[1];
    ValueNum newValVN = mapStore->m_args[2];
    unsigned loopNum  = mapStore->m_args[3];

    printf("MapStore(");
    comp->vnPrint(mapVN, 0);
    printf(", ");
    comp->vnPrint(indexVN, 0);
    if (IsVNHandle(indexVN) && ((GetHandleFlags(indexVN) & GTF_ICON_FIELD_HDL) != 0))
    {
        CORINFO_FIELD_HANDLE fieldHandle =
            reinterpret_cast<CORINFO_FIELD_HANDLE>(ConstantValue<target_size_t>(indexVN));
        printf(" (%s)", comp->eeGetFieldName(fieldHandle));
    }
    printf(", ");
    comp->vnPrint(newValVN, 0);
    if (loopNum != BasicBlock::NOT_IN_LOOP)
    {
        printf(", " FMT_LP, loopNum);
    }
    printf(")");
}

void ValueNumStore::vnDumpMemOpaque(Compiler* comp, VNFuncApp* memOpaque)
{
    assert(memOpaque->m_func == VNF_MemOpaque); // Precondition.
    const unsigned loopNum = memOpaque->m_args[0];

    if (loopNum == BasicBlock::NOT_IN_LOOP)
    {
        printf("MemOpaque:NotInLoop");
    }
    else if (loopNum == BasicBlock::MAX_LOOP_NUM)
    {
        printf("MemOpaque:Indeterminate");
    }
    else
    {
        printf("MemOpaque:L%02u", loopNum);
    }
}

void ValueNumStore::vnDumpLclAddr(Compiler* comp, VNFuncApp* func)
{
    assert(func->m_func == VNF_LclAddr);

    unsigned      lclNum     = ConstantValue<unsigned>(func->m_args[0]);
    target_size_t offset     = ConstantValue<target_size_t>(func->m_args[1]);
    ValueNum      fieldSeqVN = func->m_args[2];

    printf("LclAddr(V%02u, @%u,", lclNum, static_cast<unsigned>(offset));
    vnDump(comp, fieldSeqVN, false);
    printf(")");
}

void ValueNumStore::DumpBitCast(const VNFuncApp& cast)
{
    assert(cast.m_func == VNF_BitCast);

    uint32_t  packedCastType = static_cast<uint32_t>(GetConstantInt32(cast.m_args[1]));
    var_types toType         = static_cast<var_types>(packedCastType >> VCA_BitCount);
    var_types fromType       = varActualType(TypeOfVN(cast.m_args[0]));

    printf("BitCast<%s, %s>(" FMT_VN ", " FMT_VN ")", varTypeName(fromType), varTypeName(toType), cast.m_args[0],
           cast.m_args[1]);
}

void ValueNumStore::DumpCast(const VNFuncApp& cast)
{
    assert(cast.m_func == VNF_Cast);

    uint32_t  packedCastType = static_cast<uint32_t>(GetConstantInt32(cast.m_args[1]));
    var_types toType         = static_cast<var_types>(packedCastType >> VCA_BitCount);
    var_types fromType       = varActualType(TypeOfVN(cast.m_args[0]));

    if ((packedCastType & VCA_UnsignedSrc) != 0)
    {
        fromType = varTypeToUnsigned(fromType);
    }

    printf("Cast<%s, %s>(" FMT_VN ", " FMT_VN ")", varTypeName(fromType), varTypeName(toType), cast.m_args[0],
           cast.m_args[1]);
}

#endif // DEBUG

static UINT8 vnfOpAttribs[VNF_COUNT];

UINT8* ValueNumStore::s_vnfOpAttribs = nullptr;

void ValueNumStore::InitValueNumStoreStatics()
{
    // Make sure we have the constants right...
    assert(unsigned(VNFOA_Arity1) == (1 << VNFOA_ArityShift));
    assert(VNFOA_ArityMask == (VNFOA_MaxArity << VNFOA_ArityShift));

    s_vnfOpAttribs = &vnfOpAttribs[0];
    for (unsigned i = 0; i < GT_COUNT; i++)
    {
        genTreeOps gtOper = static_cast<genTreeOps>(i);
        unsigned   arity  = 0;
        if (GenTree::OperIsUnary(gtOper))
        {
            arity = 1;
        }
        else if (GenTree::OperIsBinary(gtOper))
        {
            arity = 2;
        }
        // Since GT_ARR_BOUNDS_CHECK is not currently GTK_BINOP
        else if (gtOper == GT_ARR_BOUNDS_CHECK)
        {
            arity = 2;
        }
        vnfOpAttribs[i] |= ((arity << VNFOA_ArityShift) & VNFOA_ArityMask);

        if (GenTree::OperIsCommutative(gtOper))
        {
            vnfOpAttribs[i] |= VNFOA_Commutative;
        }
    }

    // I so wish this wasn't the best way to do this...

    int vnfNum = VNF_Boundary + 1; // The macro definition below will update this after using it.

#define ValueNumFuncDef(vnf, arity, commute, knownNonNull, sharedStatic)                                               \
    if (commute)                                                                                                       \
        vnfOpAttribs[vnfNum] |= VNFOA_Commutative;                                                                     \
    if (knownNonNull)                                                                                                  \
        vnfOpAttribs[vnfNum] |= VNFOA_KnownNonNull;                                                                    \
    if (sharedStatic)                                                                                                  \
        vnfOpAttribs[vnfNum] |= VNFOA_SharedStatic;                                                                    \
    vnfOpAttribs[vnfNum] |= ((arity << VNFOA_ArityShift) & VNFOA_ArityMask);                                           \
    vnfNum++;

#include "valuenumfuncs.h"
#undef ValueNumFuncDef

    assert(vnfNum == VNF_COUNT);

    genTreeOps genTreeOpsIllegalAsVNFunc[]{GT_IND,
                                           GT_NULLCHECK,
                                           GT_QMARK,
                                           GT_LOCKADD,
                                           GT_XADD,
                                           GT_XCHG,
                                           GT_CMPXCHG,
                                           GT_LCLHEAP,
                                           GT_BOX,
                                           GT_XORR,
                                           GT_XAND,
                                           GT_COMMA,
                                           GT_ARR_BOUNDS_CHECK,
                                           GT_OBJ,
                                           GT_BLK,
                                           GT_JTRUE,
                                           GT_RETURN,
                                           GT_SWITCH,
                                           GT_RETFILT,
                                           GT_CKFINITE,
                                           GT_ASG,
                                           GT_NOP};

    for (auto oper : genTreeOpsIllegalAsVNFunc)
    {
        vnfOpAttribs[oper] |= VNFOA_IllegalGenTreeOp;
    }
}

#ifdef DEBUG
// Define the name array.
#define ValueNumFuncDef(vnf, arity, commute, knownNonNull, sharedStatic) #vnf,

const char* ValueNumStore::VNFuncNameArr[] = {
#include "valuenumfuncs.h"
#undef ValueNumFuncDef
};

// static
const char* ValueNumStore::VNFuncName(VNFunc vnf)
{
    vnf = VNFuncIndex(vnf);

    if (vnf < VNF_Boundary)
    {
        return GenTree::OpName(genTreeOps(vnf));
    }
    else
    {
        return VNFuncNameArr[vnf - (VNF_Boundary + 1)];
    }
}

static const char* s_reservedNameArr[] = {
    "$VN.Recursive",    // -2  RecursiveVN
    "$VN.No",           // -1  NoVN
    "$VN.Null",         //  0  VNForNull()
    "$VN.ReadOnlyHeap", //  2  VNForROH()
    "$VN.Void",         //  3  VNForVoid()
    "$VN.EmptyExcSet"   //  4  VNForEmptyExcSet()
};

// Returns the string name of "vn" when it is a reserved value number, nullptr otherwise
// static
const char* ValueNumStore::reservedName(ValueNum vn)
{
    int val = vn - ValueNumStore::RecursiveVN; // Add two, making 'RecursiveVN' equal to zero
    int max = ValueNumStore::SRC_NumSpecialRefConsts - ValueNumStore::RecursiveVN;

    if ((val >= 0) && (val < max))
    {
        return s_reservedNameArr[val];
    }
    return nullptr;
}

#endif // DEBUG

// Returns true if "vn" is a reserved value number

// static
bool ValueNumStore::isReservedVN(ValueNum vn)
{
    int val = vn - ValueNumStore::RecursiveVN; // Adding two, making 'RecursiveVN' equal to zero
    int max = ValueNumStore::SRC_NumSpecialRefConsts - ValueNumStore::RecursiveVN;

    if ((val >= 0) && (val < max))
    {
        return true;
    }
    return false;
}

#ifdef DEBUG
void ValueNumStore::RunTests(Compiler* comp)
{
    VNFunc VNF_Add = GenTreeOpToVNFunc(GT_ADD);

    ValueNumStore* vns    = new (comp->getAllocatorDebugOnly()) ValueNumStore(comp, comp->getAllocatorDebugOnly());
    ValueNum       vnNull = VNForNull();
    assert(vnNull == VNForNull());

    ValueNum vnFor1 = vns->VNForIntCon(1);
    assert(vnFor1 == vns->VNForIntCon(1));
    assert(vns->TypeOfVN(vnFor1) == TYP_INT);
    assert(vns->IsVNConstant(vnFor1));
    assert(vns->ConstantValue<int>(vnFor1) == 1);

    ValueNum vnFor100 = vns->VNForIntCon(100);
    assert(vnFor100 == vns->VNForIntCon(100));
    assert(vnFor100 != vnFor1);
    assert(vns->TypeOfVN(vnFor100) == TYP_INT);
    assert(vns->IsVNConstant(vnFor100));
    assert(vns->ConstantValue<int>(vnFor100) == 100);

    ValueNum vnFor1F = vns->VNForFloatCon(1.0f);
    assert(vnFor1F == vns->VNForFloatCon(1.0f));
    assert(vnFor1F != vnFor1 && vnFor1F != vnFor100);
    assert(vns->TypeOfVN(vnFor1F) == TYP_FLOAT);
    assert(vns->IsVNConstant(vnFor1F));
    assert(vns->ConstantValue<float>(vnFor1F) == 1.0f);

    ValueNum vnFor1D = vns->VNForDoubleCon(1.0);
    assert(vnFor1D == vns->VNForDoubleCon(1.0));
    assert(vnFor1D != vnFor1F && vnFor1D != vnFor1 && vnFor1D != vnFor100);
    assert(vns->TypeOfVN(vnFor1D) == TYP_DOUBLE);
    assert(vns->IsVNConstant(vnFor1D));
    assert(vns->ConstantValue<double>(vnFor1D) == 1.0);

    ValueNum vnRandom1   = vns->VNForExpr(nullptr, TYP_INT);
    ValueNum vnForFunc2a = vns->VNForFunc(TYP_INT, VNF_Add, vnFor1, vnRandom1);
    assert(vnForFunc2a == vns->VNForFunc(TYP_INT, VNF_Add, vnFor1, vnRandom1));
    assert(vnForFunc2a != vnFor1D && vnForFunc2a != vnFor1F && vnForFunc2a != vnFor1 && vnForFunc2a != vnRandom1);
    assert(vns->TypeOfVN(vnForFunc2a) == TYP_INT);
    assert(!vns->IsVNConstant(vnForFunc2a));
    assert(vns->IsVNFunc(vnForFunc2a));
    VNFuncApp fa2a;
    bool      b = vns->GetVNFunc(vnForFunc2a, &fa2a);
    assert(b);
    assert(fa2a.m_func == VNF_Add && fa2a.m_arity == 2 && fa2a.m_args[0] == vnFor1 && fa2a.m_args[1] == vnRandom1);

    ValueNum vnForFunc2b = vns->VNForFunc(TYP_INT, VNF_Add, vnFor1, vnFor100);
    assert(vnForFunc2b == vns->VNForFunc(TYP_INT, VNF_Add, vnFor1, vnFor100));
    assert(vnForFunc2b != vnFor1D && vnForFunc2b != vnFor1F && vnForFunc2b != vnFor1 && vnForFunc2b != vnFor100);
    assert(vns->TypeOfVN(vnForFunc2b) == TYP_INT);
    assert(vns->IsVNConstant(vnForFunc2b));
    assert(vns->ConstantValue<int>(vnForFunc2b) == 101);

    // printf("Did ValueNumStore::RunTests.\n");
}
#endif // DEBUG

// This represents the "to do" state of the value number computation.
struct ValueNumberState
{
    // These two stacks collectively represent the set of blocks that are candidates for
    // processing, because at least one predecessor has been processed.  Blocks on "m_toDoAllPredsDone"
    // have had *all* predecessors processed, and thus are candidates for some extra optimizations.
    // Blocks on "m_toDoNotAllPredsDone" have at least one predecessor that has not been processed.
    // Blocks are initially on "m_toDoNotAllPredsDone" may be moved to "m_toDoAllPredsDone" when their last
    // unprocessed predecessor is processed, thus maintaining the invariants.
    ArrayStack<BasicBlock*> m_toDoAllPredsDone;
    ArrayStack<BasicBlock*> m_toDoNotAllPredsDone;

    Compiler* m_comp;

    // TBD: This should really be a bitset...
    // For now:
    // first bit indicates completed,
    // second bit indicates that it's been pushed on all-done stack,
    // third bit indicates that it's been pushed on not-all-done stack.
    BYTE* m_visited;

    enum BlockVisitBits
    {
        BVB_complete     = 0x1,
        BVB_onAllDone    = 0x2,
        BVB_onNotAllDone = 0x4,
    };

    bool GetVisitBit(unsigned bbNum, BlockVisitBits bvb)
    {
        return (m_visited[bbNum] & bvb) != 0;
    }
    void SetVisitBit(unsigned bbNum, BlockVisitBits bvb)
    {
        m_visited[bbNum] |= bvb;
    }

    ValueNumberState(Compiler* comp)
        : m_toDoAllPredsDone(comp->getAllocator(CMK_ValueNumber))
        , m_toDoNotAllPredsDone(comp->getAllocator(CMK_ValueNumber))
        , m_comp(comp)
        , m_visited(new (comp, CMK_ValueNumber) BYTE[comp->fgBBNumMax + 1]())
    {
    }

    BasicBlock* ChooseFromNotAllPredsDone()
    {
        assert(m_toDoAllPredsDone.Size() == 0);
        // If we have no blocks with all preds done, then (ideally, if all cycles have been captured by loops)
        // we must have at least one block within a loop.  We want to do the loops first.  Doing a loop entry block
        // should break the cycle, making the rest of the body of the loop (unless there's a nested loop) doable by the
        // all-preds-done rule.  If several loop entry blocks are available, at least one should have all non-loop preds
        // done -- we choose that.
        for (unsigned i = 0; i < m_toDoNotAllPredsDone.Size(); i++)
        {
            BasicBlock* cand = m_toDoNotAllPredsDone.Get(i);

            // Skip any already-completed blocks (a block may have all its preds finished, get added to the
            // all-preds-done todo set, and get processed there).  Do this by moving the last one down, to
            // keep the array compact.
            while (GetVisitBit(cand->bbNum, BVB_complete))
            {
                if (i + 1 < m_toDoNotAllPredsDone.Size())
                {
                    cand = m_toDoNotAllPredsDone.Pop();
                    m_toDoNotAllPredsDone.Set(i, cand);
                }
                else
                {
                    // "cand" is the last element; delete it.
                    (void)m_toDoNotAllPredsDone.Pop();
                    break;
                }
            }
            // We may have run out of non-complete candidates above.  If so, we're done.
            if (i == m_toDoNotAllPredsDone.Size())
            {
                break;
            }

            // See if "cand" is a loop entry.
            unsigned lnum;
            if (m_comp->optBlockIsLoopEntry(cand, &lnum))
            {
                // "lnum" is the innermost loop of which "cand" is the entry; find the outermost.
                unsigned lnumPar = m_comp->optLoopTable[lnum].lpParent;
                while (lnumPar != BasicBlock::NOT_IN_LOOP)
                {
                    if (m_comp->optLoopTable[lnumPar].lpEntry == cand)
                    {
                        lnum = lnumPar;
                    }
                    else
                    {
                        break;
                    }
                    lnumPar = m_comp->optLoopTable[lnumPar].lpParent;
                }

                bool allNonLoopPredsDone = true;
                for (flowList* pred = m_comp->BlockPredsWithEH(cand); pred != nullptr; pred = pred->flNext)
                {
                    BasicBlock* predBlock = pred->getBlock();
                    if (!m_comp->optLoopTable[lnum].lpContains(predBlock))
                    {
                        if (!GetVisitBit(predBlock->bbNum, BVB_complete))
                        {
                            allNonLoopPredsDone = false;
                        }
                    }
                }
                if (allNonLoopPredsDone)
                {
                    return cand;
                }
            }
        }

        // If we didn't find a loop entry block with all non-loop preds done above, then return a random member (if
        // there is one).
        if (m_toDoNotAllPredsDone.Size() == 0)
        {
            return nullptr;
        }
        else
        {
            return m_toDoNotAllPredsDone.Pop();
        }
    }

// Debugging output that is too detailed for a normal JIT dump...
#define DEBUG_VN_VISIT 0

    // Record that "blk" has been visited, and add any unvisited successors of "blk" to the appropriate todo set.
    void FinishVisit(BasicBlock* blk)
    {
#ifdef DEBUG_VN_VISIT
        JITDUMP("finish(" FMT_BB ").\n", blk->bbNum);
#endif // DEBUG_VN_VISIT

        SetVisitBit(blk->bbNum, BVB_complete);

        for (BasicBlock* succ : blk->GetAllSuccs(m_comp))
        {
#ifdef DEBUG_VN_VISIT
            JITDUMP("   Succ(" FMT_BB ").\n", succ->bbNum);
#endif // DEBUG_VN_VISIT

            if (GetVisitBit(succ->bbNum, BVB_complete))
            {
                continue;
            }
#ifdef DEBUG_VN_VISIT
            JITDUMP("     Not yet completed.\n");
#endif // DEBUG_VN_VISIT

            bool allPredsVisited = true;
            for (flowList* pred = m_comp->BlockPredsWithEH(succ); pred != nullptr; pred = pred->flNext)
            {
                BasicBlock* predBlock = pred->getBlock();
                if (!GetVisitBit(predBlock->bbNum, BVB_complete))
                {
                    allPredsVisited = false;
                    break;
                }
            }

            if (allPredsVisited)
            {
#ifdef DEBUG_VN_VISIT
                JITDUMP("     All preds complete, adding to allDone.\n");
#endif // DEBUG_VN_VISIT

                assert(!GetVisitBit(succ->bbNum, BVB_onAllDone)); // Only last completion of last succ should add to
                                                                  // this.
                m_toDoAllPredsDone.Push(succ);
                SetVisitBit(succ->bbNum, BVB_onAllDone);
            }
            else
            {
#ifdef DEBUG_VN_VISIT
                JITDUMP("     Not all preds complete  Adding to notallDone, if necessary...\n");
#endif // DEBUG_VN_VISIT

                if (!GetVisitBit(succ->bbNum, BVB_onNotAllDone))
                {
#ifdef DEBUG_VN_VISIT
                    JITDUMP("       Was necessary.\n");
#endif // DEBUG_VN_VISIT
                    m_toDoNotAllPredsDone.Push(succ);
                    SetVisitBit(succ->bbNum, BVB_onNotAllDone);
                }
            }
        }
    }

    bool ToDoExists()
    {
        return m_toDoAllPredsDone.Size() > 0 || m_toDoNotAllPredsDone.Size() > 0;
    }
};

void Compiler::fgValueNumber()
{
#ifdef DEBUG
    // This could be a JITDUMP, but some people find it convenient to set a breakpoint on the printf.
    if (verbose)
    {
        printf("\n*************** In fgValueNumber()\n");
    }
#endif

    // If we skipped SSA, skip VN as well.
    if (fgSsaPassesCompleted == 0)
    {
        return;
    }

    // Allocate the value number store.
    assert(fgVNPassesCompleted > 0 || vnStore == nullptr);
    if (fgVNPassesCompleted == 0)
    {
        CompAllocator allocator(getAllocator(CMK_ValueNumber));
        vnStore = new (allocator) ValueNumStore(this, allocator);
    }
    else
    {
        ValueNumPair noVnp;
        // Make sure the memory SSA names have no value numbers.
        for (unsigned i = 0; i < lvMemoryPerSsaData.GetCount(); i++)
        {
            lvMemoryPerSsaData.GetSsaDefByIndex(i)->m_vnPair = noVnp;
        }
        for (BasicBlock* const blk : Blocks())
        {
            for (Statement* const stmt : blk->NonPhiStatements())
            {
                for (GenTree* const tree : stmt->TreeList())
                {
                    tree->gtVNPair.SetBoth(ValueNumStore::NoVN);
                }
            }
        }
    }

    // Compute the side effects of loops.
    optComputeLoopSideEffects();

    // At the block level, we will use a modified worklist algorithm.  We will have two
    // "todo" sets of unvisited blocks.  Blocks (other than the entry block) are put in a
    // todo set only when some predecessor has been visited, so all blocks have at least one
    // predecessor visited.  The distinction between the two sets is whether *all* predecessors have
    // already been visited.  We visit such blocks preferentially if they exist, since phi definitions
    // in such blocks will have all arguments defined, enabling a simplification in the case that all
    // arguments to the phi have the same VN.  If no such blocks exist, we pick a block with at least
    // one unvisited predecessor.  In this case, we assign a new VN for phi definitions.

    // Start by giving incoming arguments value numbers.
    // Also give must-init vars a zero of their type.
    for (unsigned lclNum = 0; lclNum < lvaCount; lclNum++)
    {
        LclVarDsc* varDsc = lvaGetDesc(lclNum);

        if (!varDsc->IsInSsa())
        {
            continue;
        }

        assert(varDsc->HasLiveness());

        if (varDsc->IsParam())
        {
            // We assume that code equivalent to this variable initialization loop
            // has been performed when doing SSA naming, so that all the variables we give
            // initial VNs to here have been given initial SSA definitions there.
            // SSA numbers always start from FIRST_SSA_NUM, and we give the value number to SSA name FIRST_SSA_NUM.
            // We use the VNF_InitVal(i) from here so we know that this value is loop-invariant
            // in all loops.
            ValueNum      initVal = vnStore->VNForFunc(varDsc->TypeGet(), VNF_InitVal, vnStore->VNForIntCon(lclNum));
            LclSsaVarDsc* ssaDef  = varDsc->GetPerSsaData(SsaConfig::FIRST_SSA_NUM);
            ssaDef->m_vnPair.SetBoth(initVal);
            ssaDef->SetBlock(fgFirstBB);
        }
        else if (info.compInitMem || varDsc->lvMustInit ||
                 VarSetOps::IsMember(this, fgFirstBB->bbLiveIn, varDsc->lvVarIndex))
        {
            // The last clause covers the use-before-def variables (the ones that are live-in to the the first block),
            // these are variables that are read before being initialized (at least on some control flow paths)
            // if they are not must-init, then they get VNF_InitVal(i), as with the param case.)

            bool isZeroed = (info.compInitMem || varDsc->lvMustInit);

            // For OSR, locals or promoted fields of locals may be missing the initial def
            // because of partial importation. We can't assume they are zero.
            if (lvaIsOSRLocal(lclNum))
            {
                isZeroed = false;
            }

            ValueNum  initVal = ValueNumStore::NoVN; // We must assign a new value to initVal
            var_types typ     = varDsc->TypeGet();

            switch (typ)
            {
                case TYP_BLK:
                    // TYP_BLK is used for the EHSlots locals on x86 (aka shadowSPslotsVar),
                    // for the lvaInlinedPInvokeFrameVar on x64, arm and x86
                    // and for the outgoing argument area if FEATURE_FIXED_OUT_ARGS is enabled.
                    // The stack associated with these LclVars are not zero initialized
                    // thus we set 'initVN' to a new, unique VN.
                    initVal = vnStore->VNForExpr(fgFirstBB, TYP_UNKNOWN);
                    break;

                case TYP_BYREF:
                    if (isZeroed)
                    {
                        // LclVars of TYP_BYREF can be zero-inited.
                        initVal = vnStore->VNForByrefCon(0);
                    }
                    else
                    {
                        // Here we have uninitialized TYP_BYREF
                        initVal = vnStore->VNForFunc(typ, VNF_InitVal, vnStore->VNForIntCon(lclNum));
                    }
                    break;

                default:
                    if (isZeroed)
                    {
                        // By default we will zero init these LclVars
                        // TODO-MIKE-Cleanup: For SIMD locals this generates bogus typed LONG 0 VN constants.
                        initVal = vnStore->VNZeroForType(typ);
                    }
                    else
                    {
                        initVal = vnStore->VNForFunc(typ, VNF_InitVal, vnStore->VNForIntCon(lclNum));
                    }
                    break;
            }
#ifdef TARGET_X86
            bool isVarargParam = (lclNum == lvaVarargsBaseOfStkArgs || lclNum == lvaVarargsHandleArg);
            if (isVarargParam)
            {
                initVal = vnStore->VNForExpr(fgFirstBB, TYP_UNKNOWN);
            }
#endif
            assert(initVal != ValueNumStore::NoVN);

            LclSsaVarDsc* ssaDef = varDsc->GetPerSsaData(SsaConfig::FIRST_SSA_NUM);
            ssaDef->m_vnPair.SetBoth(initVal);
            ssaDef->SetBlock(fgFirstBB);
        }
    }
    // Give memory an initial value number (about which we know nothing).
    ValueNum memoryInitVal = vnStore->VNForFunc(TYP_REF, VNF_InitVal, vnStore->VNForIntCon(-1)); // Use -1 for memory.
    GetMemoryPerSsaData(SsaConfig::FIRST_SSA_NUM)->m_vnPair.SetBoth(memoryInitVal);
#ifdef DEBUG
    if (verbose)
    {
        printf("Memory Initial Value in BB01 is: " FMT_VN "\n", memoryInitVal);
    }
#endif // DEBUG

    ValueNumberState vs(this);

    // Push the first block.  This has no preds.
    vs.m_toDoAllPredsDone.Push(fgFirstBB);

    while (vs.ToDoExists())
    {
        while (vs.m_toDoAllPredsDone.Size() > 0)
        {
            BasicBlock* toDo = vs.m_toDoAllPredsDone.Pop();
            fgValueNumberBlock(toDo);
            // Record that we've visited "toDo", and add successors to the right sets.
            vs.FinishVisit(toDo);
        }
        // OK, we've run out of blocks whose predecessors are done.  Pick one whose predecessors are not all done,
        // process that.  This may make more "all-done" blocks, so we'll go around the outer loop again --
        // note that this is an "if", not a "while" loop.
        if (vs.m_toDoNotAllPredsDone.Size() > 0)
        {
            BasicBlock* toDo = vs.ChooseFromNotAllPredsDone();
            if (toDo == nullptr)
            {
                continue; // We may have run out, because of completed blocks on the not-all-preds done list.
            }

            fgValueNumberBlock(toDo);
            // Record that we've visited "toDo", and add successors to the right sest.
            vs.FinishVisit(toDo);
        }
    }

    fgVNPassesCompleted++;
}

void Compiler::fgValueNumberBlock(BasicBlock* blk)
{
    compCurBB = blk;

    Statement* stmt = blk->firstStmt();

    // First: visit phi's.  If "newVNForPhis", give them new VN's.  If not,
    // first check to see if all phi args have the same value.
    for (; (stmt != nullptr) && stmt->IsPhiDefnStmt(); stmt = stmt->GetNextStmt())
    {
        GenTree* asg = stmt->GetRootNode();
        assert(asg->OperIs(GT_ASG));

        GenTreeLclVar* newSsaDef = asg->AsOp()->gtGetOp1()->AsLclVar();
        ValueNumPair   phiVNP;
        ValueNumPair   sameVNP;

        for (GenTreePhi::Use& use : asg->AsOp()->gtGetOp2()->AsPhi()->Uses())
        {
            GenTreePhiArg* phiArg         = use.GetNode()->AsPhiArg();
            ValueNum       phiArgSsaNumVN = vnStore->VNForIntCon(phiArg->GetSsaNum());
            ValueNumPair   phiArgVNP      = lvaGetDesc(phiArg)->GetPerSsaData(phiArg->GetSsaNum())->m_vnPair;

            phiArg->gtVNPair = phiArgVNP;

            if (phiVNP.GetLiberal() == ValueNumStore::NoVN)
            {
                // This is the first PHI argument
                phiVNP  = ValueNumPair(phiArgSsaNumVN, phiArgSsaNumVN);
                sameVNP = phiArgVNP;
            }
            else
            {
                phiVNP = vnStore->VNPairForFunc(newSsaDef->TypeGet(), VNF_Phi,
                                                ValueNumPair(phiArgSsaNumVN, phiArgSsaNumVN), phiVNP);

                if ((sameVNP.GetLiberal() != phiArgVNP.GetLiberal()) ||
                    (sameVNP.GetConservative() != phiArgVNP.GetConservative()))
                {
                    // If this argument's VNs are different from "same" then change "same" to NoVN.
                    // Note that this means that if any argument's VN is NoVN then the final result
                    // will also be NoVN, which is what we want.
                    sameVNP.SetBoth(ValueNumStore::NoVN);
                }
            }
        }

#ifdef DEBUG
        // There should be at least to 2 PHI arguments so phiVN's VNs should always be VNF_Phi functions.
        VNFuncApp phiFunc;
        assert(vnStore->GetVNFunc(phiVNP.GetLiberal(), &phiFunc) && (phiFunc.m_func == VNF_Phi));
        assert(vnStore->GetVNFunc(phiVNP.GetConservative(), &phiFunc) && (phiFunc.m_func == VNF_Phi));
#endif

        ValueNumPair newSsaDefVNP;

        if (sameVNP.BothDefined())
        {
            // If all the args of the phi had the same value(s, liberal and conservative), then there wasn't really
            // a reason to have the phi -- just pass on that value.
            newSsaDefVNP = sameVNP;
        }
        else
        {
            // They were not the same; we need to create a phi definition.
            ValueNum lclNumVN = ValueNum(newSsaDef->GetLclNum());
            ValueNum ssaNumVN = ValueNum(newSsaDef->GetSsaNum());

            newSsaDefVNP = vnStore->VNPairForFunc(newSsaDef->TypeGet(), VNF_PhiDef, ValueNumPair(lclNumVN, lclNumVN),
                                                  ValueNumPair(ssaNumVN, ssaNumVN), phiVNP);
        }

        LclSsaVarDsc* newSsaDefDsc = lvaGetDesc(newSsaDef)->GetPerSsaData(newSsaDef->GetSsaNum());
        newSsaDefDsc->m_vnPair     = newSsaDefVNP;
#ifdef DEBUG
        if (verbose)
        {
            printf("SSA PHI definition: set VN of local %d/%d to ", newSsaDef->GetLclNum(), newSsaDef->GetSsaNum());
            vnpPrint(newSsaDefVNP, 1);
            printf(" %s.\n", sameVNP.BothDefined() ? "(all same)" : "");
        }
#endif // DEBUG
    }

    // Now do the same for each MemoryKind.
    for (MemoryKind memoryKind : allMemoryKinds())
    {
        // Is there a phi for this block?
        if (blk->bbMemorySsaPhiFunc[memoryKind] == nullptr)
        {
            fgCurMemoryVN[memoryKind] = GetMemoryPerSsaData(blk->bbMemorySsaNumIn[memoryKind])->m_vnPair.GetLiberal();
            assert(fgCurMemoryVN[memoryKind] != ValueNumStore::NoVN);
        }
        else
        {
            if ((memoryKind == ByrefExposed) && byrefStatesMatchGcHeapStates)
            {
                // The update for GcHeap will copy its result to ByrefExposed.
                assert(memoryKind < GcHeap);
                assert(blk->bbMemorySsaPhiFunc[memoryKind] == blk->bbMemorySsaPhiFunc[GcHeap]);
                continue;
            }

            unsigned loopNum;
            ValueNum newMemoryVN;
            if (optBlockIsLoopEntry(blk, &loopNum))
            {
                newMemoryVN = fgMemoryVNForLoopSideEffects(memoryKind, blk, loopNum);
            }
            else
            {
                // Are all the VN's the same?
                BasicBlock::MemoryPhiArg* phiArgs = blk->bbMemorySsaPhiFunc[memoryKind];
                assert(phiArgs != BasicBlock::EmptyMemoryPhiDef);
                // There should be > 1 args to a phi.
                // But OSR might leave around "dead" try entry blocks...
                assert((phiArgs->m_nextArg != nullptr) || opts.IsOSR());
                ValueNum phiAppVN = vnStore->VNForIntCon(phiArgs->GetSsaNum());
                JITDUMP("  Building phi application: $%x = SSA# %d.\n", phiAppVN, phiArgs->GetSsaNum());
                bool     allSame = true;
                ValueNum sameVN  = GetMemoryPerSsaData(phiArgs->GetSsaNum())->m_vnPair.GetLiberal();
                if (sameVN == ValueNumStore::NoVN)
                {
                    allSame = false;
                }
                phiArgs = phiArgs->m_nextArg;
                while (phiArgs != nullptr)
                {
                    ValueNum phiArgVN = GetMemoryPerSsaData(phiArgs->GetSsaNum())->m_vnPair.GetLiberal();
                    if (phiArgVN == ValueNumStore::NoVN || phiArgVN != sameVN)
                    {
                        allSame = false;
                    }
#ifdef DEBUG
                    ValueNum oldPhiAppVN = phiAppVN;
#endif
                    unsigned phiArgSSANum   = phiArgs->GetSsaNum();
                    ValueNum phiArgSSANumVN = vnStore->VNForIntCon(phiArgSSANum);
                    JITDUMP("  Building phi application: $%x = SSA# %d.\n", phiArgSSANumVN, phiArgSSANum);
                    phiAppVN = vnStore->VNForFunc(TYP_REF, VNF_Phi, phiArgSSANumVN, phiAppVN);
                    JITDUMP("  Building phi application: $%x = phi($%x, $%x).\n", phiAppVN, phiArgSSANumVN,
                            oldPhiAppVN);
                    phiArgs = phiArgs->m_nextArg;
                }
                if (allSame)
                {
                    newMemoryVN = sameVN;
                }
                else
                {
                    newMemoryVN = vnStore->VNForFunc(TYP_REF, VNF_PhiMemoryDef,
                                                     vnStore->VNForHandle(ssize_t(blk), GTF_EMPTY), phiAppVN);
                }
            }
            GetMemoryPerSsaData(blk->bbMemorySsaNumIn[memoryKind])->m_vnPair.SetLiberal(newMemoryVN);
            fgCurMemoryVN[memoryKind] = newMemoryVN;
            if ((memoryKind == GcHeap) && byrefStatesMatchGcHeapStates)
            {
                // Keep the CurMemoryVNs in sync
                fgCurMemoryVN[ByrefExposed] = newMemoryVN;
            }
        }
#ifdef DEBUG
        if (verbose)
        {
            printf("The SSA definition for %s (#%d) at start of " FMT_BB " is ", memoryKindNames[memoryKind],
                   blk->bbMemorySsaNumIn[memoryKind], blk->bbNum);
            vnPrint(fgCurMemoryVN[memoryKind], 1);
            printf("\n");
        }
#endif // DEBUG
    }

    // Now iterate over the remaining statements, and their trees.
    for (; stmt != nullptr; stmt = stmt->GetNextStmt())
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("\n***** " FMT_BB ", " FMT_STMT "(before)\n", blk->bbNum, stmt->GetID());
            gtDispTree(stmt->GetRootNode());
            printf("\n");
        }
#endif

        for (GenTree* const tree : stmt->TreeList())
        {
            // Set up ambient var referring to current tree.
            compCurTree = tree;
            fgValueNumberTree(tree);
            compCurTree = nullptr;
        }

#ifdef DEBUG
        if (verbose)
        {
            printf("\n***** " FMT_BB ", " FMT_STMT "(after)\n", blk->bbNum, stmt->GetID());
            gtDispTree(stmt->GetRootNode());
            printf("\n");
            if (stmt->GetNextStmt() != nullptr)
            {
                printf("---------\n");
            }
        }
#endif
    }

    for (MemoryKind memoryKind : allMemoryKinds())
    {
        if ((memoryKind == GcHeap) && byrefStatesMatchGcHeapStates)
        {
            // The update to the shared SSA data will have already happened for ByrefExposed.
            assert(memoryKind > ByrefExposed);
            assert(blk->bbMemorySsaNumOut[memoryKind] == blk->bbMemorySsaNumOut[ByrefExposed]);
            assert(GetMemoryPerSsaData(blk->bbMemorySsaNumOut[memoryKind])->m_vnPair.GetLiberal() ==
                   fgCurMemoryVN[memoryKind]);
            continue;
        }

        if (blk->bbMemorySsaNumOut[memoryKind] != blk->bbMemorySsaNumIn[memoryKind])
        {
            GetMemoryPerSsaData(blk->bbMemorySsaNumOut[memoryKind])->m_vnPair.SetLiberal(fgCurMemoryVN[memoryKind]);
        }
    }

    compCurBB = nullptr;
}

ValueNum Compiler::fgMemoryVNForLoopSideEffects(MemoryKind  memoryKind,
                                                BasicBlock* entryBlock,
                                                unsigned    innermostLoopNum)
{
    // "loopNum" is the innermost loop for which "blk" is the entry; find the outermost one.
    assert(innermostLoopNum != BasicBlock::NOT_IN_LOOP);
    unsigned loopsInNest = innermostLoopNum;
    unsigned loopNum     = innermostLoopNum;
    while (loopsInNest != BasicBlock::NOT_IN_LOOP)
    {
        if (optLoopTable[loopsInNest].lpEntry != entryBlock)
        {
            break;
        }
        loopNum     = loopsInNest;
        loopsInNest = optLoopTable[loopsInNest].lpParent;
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("Computing %s state for block " FMT_BB ", entry block for loops %d to %d:\n",
               memoryKindNames[memoryKind], entryBlock->bbNum, innermostLoopNum, loopNum);
    }
#endif // DEBUG

    // If this loop has memory havoc effects, just use a new, unique VN.
    if (optLoopTable[loopNum].lpLoopHasMemoryHavoc[memoryKind])
    {
        ValueNum res = vnStore->VNForExpr(entryBlock, TYP_REF);
#ifdef DEBUG
        if (verbose)
        {
            printf("  Loop %d has memory havoc effect; heap state is new unique $%x.\n", loopNum, res);
        }
#endif // DEBUG
        return res;
    }

    // Otherwise, find the predecessors of the entry block that are not in the loop.
    // If there is only one such, use its memory value as the "base."  If more than one,
    // use a new unique VN.
    BasicBlock* nonLoopPred          = nullptr;
    bool        multipleNonLoopPreds = false;
    for (flowList* pred = BlockPredsWithEH(entryBlock); pred != nullptr; pred = pred->flNext)
    {
        BasicBlock* predBlock = pred->getBlock();
        if (!optLoopTable[loopNum].lpContains(predBlock))
        {
            if (nonLoopPred == nullptr)
            {
                nonLoopPred = predBlock;
            }
            else
            {
#ifdef DEBUG
                if (verbose)
                {
                    printf("  Entry block has >1 non-loop preds: (at least) " FMT_BB " and " FMT_BB ".\n",
                           nonLoopPred->bbNum, predBlock->bbNum);
                }
#endif // DEBUG
                multipleNonLoopPreds = true;
                break;
            }
        }
    }
    if (multipleNonLoopPreds)
    {
        ValueNum res = vnStore->VNForExpr(entryBlock, TYP_REF);
#ifdef DEBUG
        if (verbose)
        {
            printf("  Therefore, memory state is new, fresh $%x.\n", res);
        }
#endif // DEBUG
        return res;
    }
    // Otherwise, there is a single non-loop pred.
    assert(nonLoopPred != nullptr);
    // What is its memory post-state?
    ValueNum newMemoryVN = GetMemoryPerSsaData(nonLoopPred->bbMemorySsaNumOut[memoryKind])->m_vnPair.GetLiberal();
    assert(newMemoryVN != ValueNumStore::NoVN); // We must have processed the single non-loop pred before reaching the
                                                // loop entry.

#ifdef DEBUG
    if (verbose)
    {
        printf("  Init %s state is $%x, with new, fresh VN at:\n", memoryKindNames[memoryKind], newMemoryVN);
    }
#endif // DEBUG
    // Modify "base" by setting all the modified fields/field maps/array maps to unknown values.
    // These annotations apply specifically to the GcHeap, where we disambiguate across such stores.
    if (memoryKind == GcHeap)
    {
        // First the fields/field maps.
        Compiler::LoopDsc::FieldHandleSet* fieldsMod = optLoopTable[loopNum].lpFieldsModified;
        if (fieldsMod != nullptr)
        {
            for (Compiler::LoopDsc::FieldHandleSet::KeyIterator ki = fieldsMod->Begin(); !ki.Equal(fieldsMod->End());
                 ++ki)
            {
                CORINFO_FIELD_HANDLE fldHnd   = ki.Get();
                ValueNum             fldHndVN = vnStore->VNForFieldHandle(fldHnd);

#ifdef DEBUG
                if (verbose)
                {
                    const char* modName;
                    const char* fldName = eeGetFieldName(fldHnd, &modName);
                    printf("     VNForHandle(%s) is " FMT_VN "\n", fldName, fldHndVN);
                }
#endif // DEBUG

                newMemoryVN =
                    vnStore->VNForMapStore(TYP_REF, newMemoryVN, fldHndVN, vnStore->VNForExpr(entryBlock, TYP_REF));
            }
        }
        // Now do the array maps.
        Compiler::LoopDsc::TypeNumSet* elemTypesMod = optLoopTable[loopNum].lpArrayElemTypesModified;
        if (elemTypesMod != nullptr)
        {
            for (Compiler::LoopDsc::TypeNumSet::KeyIterator ki = elemTypesMod->Begin(); !ki.Equal(elemTypesMod->End());
                 ++ki)
            {
                unsigned elemTypeNum = ki.Get();

#ifdef DEBUG
                if (verbose)
                {
                    // If a valid class handle is given when the ElemType is set, DecodeElemType will
                    // return TYP_STRUCT, and elemClsHnd is that handle.
                    // Otherwise, elemClsHnd is NOT a valid class handle, and is the encoded var_types value.
                    if (typIsLayoutNum(elemTypeNum))
                    {
                        printf("     Array currentVNP %s[]\n", typGetLayoutByNum(elemTypeNum)->GetClassName());
                    }
                    else
                    {
                        printf("     Array currentVNP %s[]\n", varTypeName(static_cast<var_types>(elemTypeNum)));
                    }
                }
#endif // DEBUG

                ValueNum elemTypeVN = vnStore->VNForTypeNum(elemTypeNum);
                ValueNum uniqueVN   = vnStore->VNForExpr(entryBlock, TYP_REF);
                newMemoryVN         = vnStore->VNForMapStore(TYP_REF, newMemoryVN, elemTypeVN, uniqueVN);
            }
        }
    }
    else
    {
        // If there were any fields/elements modified, this should have been recorded as havoc
        // for ByrefExposed.
        assert(memoryKind == ByrefExposed);
        assert((optLoopTable[loopNum].lpFieldsModified == nullptr) ||
               optLoopTable[loopNum].lpLoopHasMemoryHavoc[memoryKind]);
        assert((optLoopTable[loopNum].lpArrayElemTypesModified == nullptr) ||
               optLoopTable[loopNum].lpLoopHasMemoryHavoc[memoryKind]);
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("  Final %s state is $%x.\n", memoryKindNames[memoryKind], newMemoryVN);
    }
#endif // DEBUG
    return newMemoryVN;
}

void Compiler::vnClearGcHeap(GenTree* node DEBUGARG(const char* comment))
{
    vnUpdateGcHeap(node, vnStore->VNForExpr(compCurBB, TYP_STRUCT) DEBUGARG(comment));
}

void Compiler::vnClearByRefExposed(GenTree* node)
{
    // TODO-MIKE-CQ: For stores to address exposed locals we could probably be
    // mode precise and use a map store with the local number as the "index".
    // For now, just use a new opaque VN.

    vnUpdateByRefExposed(node, vnStore->VNForExpr(compCurBB, TYP_STRUCT));
}

void Compiler::vnUpdateGcHeap(GenTree* node, ValueNum heapVN DEBUGARG(const char* comment))
{
    assert((compCurBB->bbMemoryDef & memoryKindSet(GcHeap, ByrefExposed)) == memoryKindSet(GcHeap, ByrefExposed));

    fgCurMemoryVN[GcHeap]       = heapVN;
    fgCurMemoryVN[ByrefExposed] = byrefStatesMatchGcHeapStates ? heapVN : vnStore->VNForExpr(compCurBB, TYP_STRUCT);

    INDEBUG(vnPrintMemVN(GcHeap, fgCurMemoryVN[GcHeap], comment));
    INDEBUG(vnPrintMemVN(ByrefExposed, fgCurMemoryVN[ByrefExposed], comment));

    vnUpdateMemorySsaDef(node, GcHeap);
}

void Compiler::vnUpdateByRefExposed(GenTree* node, ValueNum memVN)
{
    assert(!byrefStatesMatchGcHeapStates);
    assert((compCurBB->bbMemoryDef & memoryKindSet(ByrefExposed)) != 0);

    fgCurMemoryVN[ByrefExposed] = memVN;

    INDEBUG(vnPrintMemVN(ByrefExposed, memVN, "address-exposed local store"));

    vnUpdateMemorySsaDef(node, ByrefExposed);
}

void Compiler::vnUpdateMemorySsaDef(GenTree* node, MemoryKind memoryKind)
{
    unsigned ssaNum;

    if (GetMemorySsaMap(memoryKind)->Lookup(node, &ssaNum))
    {
        GetMemoryPerSsaData(ssaNum)->m_vnPair.SetLiberal(fgCurMemoryVN[memoryKind]);
        JITDUMP("    %s SSA def %u = " FMT_VN "\n", memoryKindNames[memoryKind], ssaNum, fgCurMemoryVN[memoryKind]);
    }
}

void Compiler::fgValueNumberTreeConst(GenTree* tree)
{
    var_types type = varActualType(tree->GetType());

    switch (type)
    {
        case TYP_LONG:
        case TYP_INT:
            if (tree->IsIntCon() && tree->IsIconHandle())
            {
                tree->gtVNPair.SetBoth(vnStore->VNForHandle(tree->AsIntCon()->GetValue(), tree->GetIconHandleFlag()));
            }
            else if (type == TYP_LONG)
            {
                tree->gtVNPair.SetBoth(vnStore->VNForLongCon(tree->AsIntConCommon()->LngValue()));
            }
            else
            {
                tree->gtVNPair.SetBoth(vnStore->VNForIntCon(tree->AsIntCon()->GetInt32Value()));
            }
            break;

        case TYP_FLOAT:
            tree->gtVNPair.SetBoth(vnStore->VNForFloatCon(tree->AsDblCon()->GetFloatValue()));
            break;
        case TYP_DOUBLE:
            tree->gtVNPair.SetBoth(vnStore->VNForDoubleCon(tree->AsDblCon()->GetDoubleValue()));
            break;

        case TYP_REF:
            if (tree->AsIntCon()->GetValue() == 0)
            {
                tree->gtVNPair.SetBoth(ValueNumStore::VNForNull());
            }
            else
            {
                assert(tree->IsIconHandle(GTF_ICON_STR_HDL)); // Constant object can be only frozen string.

                tree->gtVNPair.SetBoth(vnStore->VNForHandle(tree->AsIntCon()->GetValue(), tree->GetIconHandleFlag()));
            }
            break;

        case TYP_BYREF:
            if (tree->AsIntCon()->GetValue() == 0)
            {
                tree->gtVNPair.SetBoth(ValueNumStore::VNForNull());
            }
            else if (tree->IsIconHandle())
            {
                tree->gtVNPair.SetBoth(vnStore->VNForHandle(tree->AsIntCon()->GetValue(), tree->GetIconHandleFlag()));
            }
            else
            {
                tree->gtVNPair.SetBoth(
                    vnStore->VNForByrefCon(static_cast<target_size_t>(tree->AsIntCon()->GetValue())));
            }
            break;

        default:
            unreached();
    }
}

void Compiler::fgValueNumberTree(GenTree* tree)
{
    genTreeOps oper = tree->OperGet();

    switch (oper)
    {
        case GT_CNS_INT:
        case GT_CNS_LNG:
        case GT_CNS_DBL:
            fgValueNumberTreeConst(tree);
            break;

        case GT_LCL_VAR_ADDR:
            assert(lvaGetDesc(tree->AsLclVar())->IsAddressExposed());
            tree->gtVNPair.SetBoth(
                vnStore->VNForFunc(TYP_I_IMPL, VNF_LclAddr, vnStore->VNForIntCon(tree->AsLclVar()->GetLclNum()),
                                   vnStore->VNZeroForType(TYP_I_IMPL), vnStore->VNForFieldSeq(nullptr)));
            break;

        case GT_LCL_FLD_ADDR:
            assert(lvaGetDesc(tree->AsLclFld())->IsAddressExposed());
            tree->gtVNPair.SetBoth(vnStore->VNForFunc(TYP_I_IMPL, VNF_LclAddr,
                                                      vnStore->VNForIntCon(tree->AsLclFld()->GetLclNum()),
                                                      vnStore->VNForUPtrSizeIntCon(tree->AsLclFld()->GetLclOffs()),
                                                      vnStore->VNForFieldSeq(tree->AsLclFld()->GetFieldSeq())));
            break;

        case GT_CLS_VAR_ADDR:
            tree->gtVNPair.SetBoth(vnStore->VNForFunc(tree->GetType(), VNF_PtrToStatic,
                                                      vnStore->VNForFieldSeq(tree->AsClsVar()->GetFieldSeq())));
            break;

        case GT_LCL_VAR:
            if ((tree->gtFlags & GTF_VAR_DEF) == 0)
            {
                vnLocalLoad(tree->AsLclVar());
            }
            break;

        case GT_LCL_FLD:
            if ((tree->gtFlags & GTF_VAR_DEF) == 0)
            {
                vnLocalFieldLoad(tree->AsLclFld());
            }
            break;

        case GT_FTN_ADDR:
            // Use the value of the function pointer (actually, a method handle.)
            tree->gtVNPair.SetBoth(vnStore->VNForHandle(ssize_t(tree->AsFptrVal()->gtFptrMethod), GTF_ICON_METHOD_HDL));
            break;

        case GT_CATCH_ARG:
            // We know nothing about the value of a caught expression.
            tree->gtVNPair.SetBoth(vnStore->VNForExpr(compCurBB, tree->TypeGet()));
            break;

        case GT_MEMORYBARRIER:
            vnClearGcHeap(tree DEBUGARG("memory barrier"));
            break;

        // These do not represent values.
        case GT_NO_OP:
        case GT_JMP:   // Control flow
        case GT_LABEL: // Control flow
#if !defined(FEATURE_EH_FUNCLETS)
        case GT_END_LFIN: // Control flow
#endif
            tree->gtVNPair.SetBoth(vnStore->VNForExpr(compCurBB, tree->TypeGet()));
            break;

        case GT_ARGPLACE:
            // We'll give ARGPLACE the actual argument value number when the call
            // node itself is value numbered.
            tree->gtVNPair.SetBoth(ValueNumStore::VNForVoid());
            break;

        case GT_PHI_ARG:
            // This one is special because we should never process it in this method: it should
            // always be taken care of, when needed, during pre-processing of a blocks phi definitions.
            assert(false);
            break;

        case GT_ASG:
            vnAssignment(tree->AsOp());
            break;

        case GT_IND:
        case GT_OBJ:
        case GT_BLK:
            if ((tree->gtFlags & GTF_IND_ASG_LHS) == 0)
            {
                vnIndirLoad(tree->AsIndir());

                if (tree->OperMayThrow(this))
                {
                    vnAddNullPtrExset(tree, tree->AsIndir()->GetAddr());
                }
            }
            break;

        case GT_COPY_BLK:
        case GT_INIT_BLK:
            vnClearGcHeap(tree DEBUGARG("dynamic sized init/copy block"));
            tree->gtVNPair.SetBoth(vnStore->VNForVoid());
            break;

        case GT_CAST:
            fgValueNumberCastTree(tree);
            break;

        case GT_BITCAST:
            fgValueNumberBitCastTree(tree->AsUnOp());
            break;

        case GT_INTRINSIC:
            fgValueNumberIntrinsic(tree);
            // ToDo: model the exceptions for Intrinsics
            break;

        case GT_COMMA:
            vnComma(tree->AsOp());
            break;

        case GT_NULLCHECK:
            vnNullCheck(tree->AsIndir());
            break;

        case GT_ARR_LENGTH:
            vnArrayLength(tree->AsArrLen());
            break;

        case GT_QMARK:
        case GT_LOCKADD:
            unreached();

        case GT_XORR:
        case GT_XAND:
        case GT_XADD:
        case GT_XCHG:
            vnInterlocked(tree->AsOp());
            break;

        case GT_JTRUE:
        case GT_RETURN:
        case GT_SWITCH:
        case GT_RETFILT:
            // These nodes never need to have a ValueNumber
            tree->gtVNPair.SetBoth(ValueNumStore::NoVN);
            break;

        case GT_BOX:
            // BOX doesn't do anything at this point, the actual object allocation
            // and initialization happens separately (and not numbering BOX correctly
            // prevents seeing allocation related assertions through it)
            tree->gtVNPair = tree->gtGetOp1()->gtVNPair;
            break;

        case GT_CALL:
            fgValueNumberCall(tree->AsCall());
            break;

        case GT_ARR_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
        case GT_HW_INTRINSIC_CHK:
#endif
        {
            ValueNumPair vnpIndex  = tree->AsBoundsChk()->GetIndex()->gtVNPair;
            ValueNumPair vnpArrLen = tree->AsBoundsChk()->GetLength()->gtVNPair;

            ValueNumPair vnpExcSet = ValueNumStore::VNPForEmptyExcSet();

            // And collect the exceptions  from Index and ArrLen
            vnpExcSet = vnStore->VNPUnionExcSet(vnpIndex, vnpExcSet);
            vnpExcSet = vnStore->VNPUnionExcSet(vnpArrLen, vnpExcSet);

            // A bounds check node has no value, but may throw exceptions.
            tree->gtVNPair = vnStore->VNPWithExc(vnStore->VNPForVoid(), vnpExcSet);

            // next add the bounds check exception set for the current tree node
            fgValueNumberAddExceptionSetForBoundsCheck(tree);

            // Record non-constant value numbers that are used as the length argument to bounds checks, so
            // that
            // assertion prop will know that comparisons against them are worth analyzing.
            ValueNum lengthVN = tree->AsBoundsChk()->gtArrLen->gtVNPair.GetConservative();
            if ((lengthVN != ValueNumStore::NoVN) && !vnStore->IsVNConstant(lengthVN))
            {
                vnStore->SetVNIsCheckedBound(lengthVN);
            }
        }
        break;

        case GT_CMPXCHG:
            vnCmpXchg(tree->AsCmpXchg());
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            fgValueNumberHWIntrinsic(tree->AsHWIntrinsic());
            // ToDo: model the exceptions for Intrinsics
            break;
#endif

        case GT_LEA:
            // LEAs could probably value numbered as ADD/MUL expressions but
            // they should appear in frontend so it's not worth the trouble.
            tree->gtVNPair.SetBoth(vnStore->VNForExpr(compCurBB, tree->GetType()));
            break;

        case GT_LCLHEAP:
            tree->gtVNPair.SetBoth(vnStore->VNForExpr(compCurBB, tree->GetType()));
            // It is not necessary to model the StackOverflow exception for LCLHEAP
            break;

        case GT_CKFINITE:
            tree->gtVNPair.SetBoth(vnStore->VNForExpr(compCurBB, tree->GetType()));
            fgValueNumberAddExceptionSetForCkFinite(tree);
            break;

        case GT_NOP:
            if (tree->AsUnOp()->gtOp1 == nullptr)
            {
                tree->gtVNPair.SetBoth(vnStore->VNForExpr(compCurBB, tree->GetType()));
            }
            else
            {
                tree->SetVNP(tree->AsUnOp()->GetOp(0)->GetVNP());
            }
            break;

        default:
            if (GenTree::OperIsUnary(oper))
            {
                VNFunc vnf = GetVNFuncForNode(tree);
                assert(ValueNumStore::VNFuncIsLegal(vnf));

                ValueNumPair op1VNP;
                ValueNumPair op1VNPx;
                vnStore->VNPUnpackExc(tree->AsOp()->gtOp1->gtVNPair, &op1VNP, &op1VNPx);

                tree->gtVNPair = vnStore->VNPWithExc(vnStore->VNPairForFunc(tree->TypeGet(), vnf, op1VNP), op1VNPx);

                assert(!tree->OperMayThrow(this));
            }
            else if (GenTree::OperIsBinary(oper))
            {
                VNFunc vnf = GetVNFuncForNode(tree);
                assert(ValueNumStore::VNFuncIsLegal(vnf));

                ValueNumPair op1vnp;
                ValueNumPair op1Xvnp;
                vnStore->VNPUnpackExc(tree->AsOp()->GetOp(0)->GetVNP(), &op1vnp, &op1Xvnp);

                ValueNumPair op2vnp;
                ValueNumPair op2Xvnp;
                vnStore->VNPUnpackExc(tree->AsOp()->GetOp(1)->GetVNP(), &op2vnp, &op2Xvnp);
                ValueNumPair excSetPair = vnStore->VNPExcSetUnion(op1Xvnp, op2Xvnp);

                ValueNum newVN = ValueNumStore::NoVN;

                if ((oper == GT_ADD) && !tree->gtOverflowEx())
                {
                    newVN = vnStore->ExtendPtrVN(tree->AsOp());
                }

                if (newVN != ValueNumStore::NoVN)
                {
                    // We don't care about differences between liberal and conservative for pointer values.
                    newVN = vnStore->VNWithExc(newVN, excSetPair.GetLiberal());
                    tree->gtVNPair.SetBoth(newVN);
                }
                else
                {
                    ValueNumPair normalPair = vnStore->VNPairForFunc(tree->TypeGet(), vnf, op1vnp, op2vnp);
                    tree->gtVNPair          = vnStore->VNPWithExc(normalPair, excSetPair);
                }

                vnAddNodeExceptionSet(tree);
            }
            else
            {
                noway_assert(GenTree::OperIsSpecial(oper));
                tree->gtVNPair.SetBoth(vnStore->VNForExpr(compCurBB, tree->TypeGet()));
            }
            break;
    }

#ifdef DEBUG
    if (verbose)
    {
        if (tree->gtVNPair.GetLiberal() != ValueNumStore::NoVN)
        {
            printf("[%06u] ", tree->GetID());
            gtDispNodeName(tree);
            if (tree->OperIsLeaf() || tree->OperIsLocalStore()) // local stores used to be leaves
            {
                gtDispLeaf(tree, nullptr);
            }
            printf(" => ");
            vnpPrint(tree->gtVNPair, 1);
            printf("\n");
        }
    }
#endif // DEBUG
}

void Compiler::fgValueNumberIntrinsic(GenTree* tree)
{
    assert(tree->OperGet() == GT_INTRINSIC);
    GenTreeIntrinsic* intrinsic = tree->AsIntrinsic();
    ValueNumPair      arg0VNP, arg1VNP;
    ValueNumPair      arg0VNPx = ValueNumStore::VNPForEmptyExcSet();
    ValueNumPair      arg1VNPx = ValueNumStore::VNPForEmptyExcSet();

    vnStore->VNPUnpackExc(intrinsic->AsOp()->gtOp1->gtVNPair, &arg0VNP, &arg0VNPx);

    if (intrinsic->AsOp()->gtOp2 != nullptr)
    {
        vnStore->VNPUnpackExc(intrinsic->AsOp()->gtOp2->gtVNPair, &arg1VNP, &arg1VNPx);
    }

    if (IsMathIntrinsic(intrinsic->gtIntrinsicName))
    {
        // GT_INTRINSIC is a currently a subtype of binary operators. But most of
        // the math intrinsics are actually unary operations.

        if (intrinsic->AsOp()->gtOp2 == nullptr)
        {
            intrinsic->gtVNPair =
                vnStore->VNPWithExc(vnStore->EvalMathFuncUnary(tree->TypeGet(), intrinsic->gtIntrinsicName, arg0VNP),
                                    arg0VNPx);
        }
        else
        {
            ValueNumPair newVNP =
                vnStore->EvalMathFuncBinary(tree->TypeGet(), intrinsic->gtIntrinsicName, arg0VNP, arg1VNP);
            ValueNumPair excSet = vnStore->VNPExcSetUnion(arg0VNPx, arg1VNPx);
            intrinsic->gtVNPair = vnStore->VNPWithExc(newVNP, excSet);
        }
    }
    else
    {
        assert(intrinsic->gtIntrinsicId == CORINFO_INTRINSIC_Object_GetType);
        intrinsic->gtVNPair =
            vnStore->VNPWithExc(vnStore->VNPairForFunc(intrinsic->TypeGet(), VNF_ObjGetType, arg0VNP), arg0VNPx);
    }
}

#ifdef FEATURE_HW_INTRINSICS
void Compiler::fgValueNumberHWIntrinsic(GenTreeHWIntrinsic* node)
{
    if (node->OperIsMemoryStore())
    {
        vnClearGcHeap(node DEBUGARG("HWIntrinsic store"));
    }

    if (node->GetAuxiliaryType() != TYP_UNDEF)
    {
        // TODO-MIKE-CQ: We can't generate a proper VN for nodes that use the auxiliary
        // type because the type is simply ignored and we end up doing invalid CSE, see
        // vn-add-saturate-scalar.cs.
        node->gtVNPair.SetBoth(vnStore->VNForExpr(compCurBB, node->GetType()));
        return;
    }

    VNFunc func = GetVNFuncForNode(node);

    if (node->OperIsMemoryLoad())
    {
        if (node->GetNumOps() > 1)
        {
            // For now we will generate a unique value number for loads with more
            // than one operand (i.e. GatherVector128)
            node->gtVNPair.SetBoth(vnStore->VNForExpr(compCurBB, node->GetType()));
            return;
        }

        ValueNumPair addrVNP;
        ValueNumPair addrXVNP;
        vnStore->VNPUnpackExc(node->GetOp(0)->gtVNPair, &addrVNP, &addrXVNP);

        // The addrVN incorporates both addr's ValueNumber and the func operation
        // The func is used because operations such as LoadLow and LoadHigh perform
        // different operations, thus need to compute different ValueNumbers
        // We don't need to encode the result type as it will be encoded by the opcode in 'func'
        ValueNum addrVN = vnStore->VNForFunc(TYP_BYREF, func, addrVNP.GetLiberal());

        // The address could point anywhere, so it is an ByrefExposed load.
        ValueNum loadVN = fgValueNumberByrefExposedLoad(node->GetType(), addrVN);

        node->gtVNPair.SetLiberal(loadVN);
        node->gtVNPair.SetConservative(vnStore->VNForExpr(compCurBB, node->GetType()));
        node->gtVNPair = vnStore->VNPWithExc(node->gtVNPair, addrXVNP);
        vnAddNullPtrExset(node, node->GetOp(0));

        return;
    }

    unsigned arity = node->GetNumOps();

    if (arity > 4)
    {
        node->gtVNPair.SetBoth(vnStore->VNForExpr(compCurBB, node->GetType()));
        return;
    }

    ValueNumPair opsVnp[4];
    ValueNumPair xvnp = ValueNumStore::VNPForEmptyExcSet();

    for (unsigned i = 0; i < arity; i++)
    {
        ValueNumPair opXvnp;
        vnStore->VNPUnpackExc(node->GetOp(i)->gtVNPair, &opsVnp[i], &opXvnp);
        xvnp = vnStore->VNPExcSetUnion(xvnp, opXvnp);
    }

    ValueNumPair vnp;

    switch (arity)
    {
        case 0:
            vnp = vnStore->VNPairForFunc(node->GetType(), func);
            break;
        case 1:
            vnp = vnStore->VNPairForFunc(node->GetType(), func, opsVnp[0]);
            break;
        case 2:
            vnp = vnStore->VNPairForFunc(node->GetType(), func, opsVnp[0], opsVnp[1]);
            break;
        case 3:
            vnp = vnStore->VNPairForFunc(node->GetType(), func, opsVnp[0], opsVnp[1], opsVnp[2]);
            break;
        default:
            assert(arity == 4);
            vnp = vnStore->VNPairForFunc(node->GetType(), func, opsVnp[0], opsVnp[1], opsVnp[2], opsVnp[3]);
            break;
    }

    node->SetVNs(vnStore->VNPWithExc(vnp, xvnp));
}
#endif // FEATURE_HW_INTRINSICS

void Compiler::fgValueNumberBitCastTree(GenTreeUnOp* bitcast)
{
    assert(bitcast->OperIs(GT_BITCAST));

    GenTree*  src      = bitcast->GetOp(0);
    var_types toType   = bitcast->GetType();
    var_types fromType = src->GetType();

    assert(genTypeSize(toType) == genTypeSize(fromType));

    bitcast->SetVN(VNK_Liberal, vnStore->VNForBitCast(src->GetVN(VNK_Liberal), toType, fromType));
    bitcast->SetVN(VNK_Conservative, vnStore->VNForBitCast(src->GetVN(VNK_Conservative), toType, fromType));
}

void Compiler::fgValueNumberCastTree(GenTree* tree)
{
    assert(tree->OperGet() == GT_CAST);

    ValueNumPair srcVNPair        = tree->AsOp()->gtOp1->gtVNPair;
    var_types    castToType       = tree->CastToType();
    var_types    castFromType     = tree->CastFromType();
    bool         srcIsUnsigned    = ((tree->gtFlags & GTF_UNSIGNED) != 0);
    bool         hasOverflowCheck = tree->gtOverflowEx();

    assert(genActualType(castToType) == genActualType(tree->TypeGet())); // Ensure that the resultType is correct

    tree->gtVNPair = vnStore->VNPairForCast(srcVNPair, castToType, castFromType, srcIsUnsigned, hasOverflowCheck);
}

// Compute the normal ValueNumber for a cast operation with no exceptions
ValueNum ValueNumStore::VNForCast(ValueNum  srcVN,
                                  var_types castToType,
                                  var_types castFromType,
                                  bool      srcIsUnsigned /* = false */)
{
    // The resulting type after performingthe cast is always widened to a supported IL stack size
    var_types resultType = genActualType(castToType);

    // When we're considering actual value returned by a non-checking cast whether or not the source is
    // unsigned does *not* matter for non-widening casts.  That is, if we cast an int or a uint to short,
    // we just extract the first two bytes from the source bit pattern, not worrying about the interpretation.
    // The same is true in casting between signed/unsigned types of the same width.  Only when we're doing
    // a widening cast do we care about whether the source was unsigned,so we know whether to sign or zero extend it.
    //
    bool srcIsUnsignedNorm = srcIsUnsigned;
    if (genTypeSize(castToType) <= genTypeSize(castFromType))
    {
        srcIsUnsignedNorm = false;
    }

    ValueNum castTypeVN = VNForCastOper(castToType, srcIsUnsigned);
    ValueNum resultVN   = VNForFunc(resultType, VNF_Cast, srcVN, castTypeVN);

#ifdef DEBUG
    if (m_pComp->verbose)
    {
        printf("    %s ", varTypeName(resultType));
        m_pComp->vnPrint(resultVN, 1);
        printf("\n");
    }
#endif

    return resultVN;
}

ValueNum ValueNumStore::VNForBitCast(ValueNum src, var_types toType, var_types fromType)
{
    ValueNum srcVal;
    ValueNum srcExc;
    VNUnpackExc(src, &srcVal, &srcExc);

    ValueNum resultVal = NoVN;

    if (IsVNConstant(srcVal))
    {
        if ((fromType == TYP_FLOAT) && (toType == TYP_INT))
        {
            resultVal = VNForIntCon(jitstd::bit_cast<INT32>(ConstantValue<float>(srcVal)));
        }
        else if ((fromType == TYP_DOUBLE) && (toType == TYP_LONG))
        {
            resultVal = VNForLongCon(jitstd::bit_cast<INT64>(ConstantValue<double>(srcVal)));
        }
        else if ((fromType == TYP_INT) && (toType == TYP_FLOAT))
        {
            resultVal = VNForFloatCon(jitstd::bit_cast<float>(ConstantValue<int>(srcVal)));
        }
        else if ((fromType == TYP_LONG) && (toType == TYP_DOUBLE))
        {
            resultVal = VNForDoubleCon(jitstd::bit_cast<double>(ConstantValue<INT64>(srcVal)));
        }

        // TODO-MIKE-CQ: Handle BITCAST(Vector2 "constant") for win-x64 ABI needs, at least
        // the trivial zero case if not something more complicated like BITCAST(Create(2, 3)).
        // The main issue is that since VN doesn't really have vector constants we'd need to
        // "parse" the BITCAST operand VN(Func) probably...
    }

    if (resultVal == NoVN)
    {
        resultVal = VNForFunc(toType, VNF_BitCast, srcVal, VNForBitCastOper(toType));
    }

    return VNWithExc(resultVal, srcExc);
}

// Compute the ValueNumberPair for a cast operation
ValueNumPair ValueNumStore::VNPairForCast(ValueNumPair srcVNPair,
                                          var_types    castToType,
                                          var_types    castFromType,
                                          bool         srcIsUnsigned,    /* = false */
                                          bool         hasOverflowCheck) /* = false */
{
    // The resulting type after performingthe cast is always widened to a supported IL stack size
    var_types resultType = genActualType(castToType);

    ValueNumPair castArgVNP;
    ValueNumPair castArgxVNP;
    VNPUnpackExc(srcVNPair, &castArgVNP, &castArgxVNP);

    // When we're considering actual value returned by a non-checking cast, (hasOverflowCheck is false)
    // whether or not the source is unsigned does *not* matter for non-widening casts.
    // That is, if we cast an int or a uint to short, we just extract the first two bytes from the source
    // bit pattern, not worrying about the interpretation.  The same is true in casting between signed/unsigned
    // types of the same width.  Only when we're doing a widening cast do we care about whether the source
    // was unsigned, so we know whether to sign or zero extend it.
    //
    // Important: Casts to floating point cannot be optimized in this fashion. (bug 946768)
    //
    bool srcIsUnsignedNorm = srcIsUnsigned;
    if (!hasOverflowCheck && !varTypeIsFloating(castToType) && (genTypeSize(castToType) <= genTypeSize(castFromType)))
    {
        srcIsUnsignedNorm = false;
    }

    VNFunc       vnFunc     = hasOverflowCheck ? VNF_CastOvf : VNF_Cast;
    ValueNum     castTypeVN = VNForCastOper(castToType, srcIsUnsignedNorm);
    ValueNumPair castTypeVNPair(castTypeVN, castTypeVN);
    ValueNumPair castNormRes = VNPairForFunc(resultType, vnFunc, castArgVNP, castTypeVNPair);

    ValueNumPair resultVNP = VNPWithExc(castNormRes, castArgxVNP);

    // If we have a check for overflow, add the exception information.
    if (hasOverflowCheck)
    {
        ValueNumPair excSet = ValueNumStore::VNPForEmptyExcSet();

        ValueNumKind vnKinds[2] = {VNK_Liberal, VNK_Conservative};
        for (ValueNumKind vnKind : vnKinds)
        {
            // Do not add exceptions for folded casts.
            // We only fold checked casts that do not overflow.
            if (IsVNConstant(castNormRes.Get(vnKind)))
            {
                continue;
            }

            ValueNum ovfChk =
                VNForFunc(TYP_REF, VNF_ConvOverflowExc, castArgVNP.Get(vnKind), castTypeVNPair.Get(vnKind));
            excSet.Set(vnKind, VNExcSetSingleton(ovfChk));
        }

        excSet    = VNPExcSetUnion(excSet, castArgxVNP);
        resultVNP = VNPWithExc(castNormRes, excSet);
    }

    return resultVNP;
}

void Compiler::fgValueNumberHelperCallFunc(GenTreeCall* call, VNFunc vnf, ValueNumPair vnpExc)
{
    assert(vnf != VNF_Boundary);

    unsigned argCount = ValueNumStore::VNFuncArity(vnf);

    if (argCount == 0)
    {
        // TODO-MIKE-Review: This drops vnpExc unlike the case with arguments below...
        call->gtVNPair.SetBoth(vnStore->VNForFunc(call->GetType(), vnf));

        return;
    }

    CallInfo*    callInfo = call->GetInfo();
    ValueNumPair vnpCallArgs[3];
    noway_assert(callInfo->GetArgCount() <= 3);

    for (unsigned i = 0; i < callInfo->GetArgCount(); i++)
    {
        CallArgInfo* argInfo              = callInfo->GetArgInfo(i);
        vnpCallArgs[argInfo->GetArgNum()] = argInfo->GetNode()->GetVNP();
    }

    bool addUniqueArg            = false;
    bool useEntryPointAddrAsArg0 = false;

    switch (vnf)
    {
        case VNF_JitNew:
            addUniqueArg = true;
            vnpExc       = ValueNumStore::VNPForEmptyExcSet();
            break;

        case VNF_JitNewArr:
            addUniqueArg = true;
            // The New Array helper may throw an overflow exception
            vnpExc = vnStore->VNPExcSetSingleton(
                vnStore->VNPairForFunc(TYP_REF, VNF_NewArrOverflowExc, vnStore->VNPNormalPair(vnpCallArgs[1])));
            break;

        case VNF_Box:
        case VNF_BoxNullable:
            // Generate unique VN so, VNForFunc generates a uniq value number for box nullable.
            // Alternatively instead of using vnpUniqueArg below in VNPairForFunc(...),
            // we could use the value number of what the byref arg0 points to.
            //
            // But retrieving the value number of what the byref arg0 points to is quite a bit more work
            // and doing so only very rarely allows for an additional optimization.
            addUniqueArg = true;
            break;

        case VNF_JitReadyToRunNew:
            addUniqueArg            = true;
            useEntryPointAddrAsArg0 = true;
            vnpExc                  = ValueNumStore::VNPForEmptyExcSet();
            break;

        case VNF_JitReadyToRunNewArr:
            addUniqueArg            = true;
            useEntryPointAddrAsArg0 = true;
            // The New Array helper may throw an overflow exception
            vnpExc = vnStore->VNPExcSetSingleton(
                vnStore->VNPairForFunc(TYP_REF, VNF_NewArrOverflowExc, vnStore->VNPNormalPair(vnpCallArgs[0])));
            break;

        case VNF_ReadyToRunStaticBase:
        case VNF_ReadyToRunGenericStaticBase:
        case VNF_ReadyToRunIsInstanceOf:
        case VNF_ReadyToRunCastClass:
        case VNF_ReadyToRunGenericHandle:
            useEntryPointAddrAsArg0 = true;
            break;

        default:
            assert(s_helperCallProperties.IsPure(eeGetHelperNum(call->gtCallMethHnd)));
            break;
    }

    ValueNumPair vnpUniqueArg;

    if (addUniqueArg)
    {
        argCount--;
        vnpUniqueArg.SetBoth(vnStore->VNForExpr(compCurBB, call->GetType()));

        if (argCount == 0)
        {
            call->gtVNPair = vnStore->VNPairForFunc(call->GetType(), vnf, vnpUniqueArg);
            // TODO-MIKE-Review: This drops vnpExc unlike the case with arguments below...

            return;
        }
    }

    ValueNumPair vnpArgs[4];
    unsigned     vnpArgIndex = 0;

#ifdef FEATURE_READYTORUN_COMPILER
#ifdef TARGET_ARMARCH
    if (call->IsR2RRelativeIndir())
    {
#ifdef DEBUG
        CallArgInfo* indirectCellAddressArg = call->GetArgInfoByArgNum(0);
        assert(indirectCellAddressArg->GetNode()->IsIntCon());
        assert(indirectCellAddressArg->GetRegNum() == REG_R2R_INDIRECT_PARAM);
#endif

        // For ARM the entry point should have been added as an argument by morph.
        useEntryPointAddrAsArg0 = false;
    }
#endif // TARGET_ARMARCH

    if (useEntryPointAddrAsArg0)
    {
        ssize_t addrValue = reinterpret_cast<ssize_t>(call->gtEntryPoint.addr);
        vnpArgs[vnpArgIndex++].SetBoth(vnStore->VNForHandle(addrValue, GTF_ICON_FTN_ADDR));
    }
#endif // FEATURE_READYTORUN_COMPILER

    for (unsigned i = 0, count = callInfo->GetArgCount(); i < count; i++)
    {
        ValueNumPair vnpArgExc;
        vnStore->VNPUnpackExc(vnpCallArgs[i], &vnpArgs[vnpArgIndex++], &vnpArgExc);
        vnpExc = vnStore->VNPExcSetUnion(vnpExc, vnpArgExc);
    }

    if (addUniqueArg)
    {
        vnpArgs[vnpArgIndex++] = vnpUniqueArg;
    }

    ValueNumPair vnpCall;

    switch (vnpArgIndex)
    {
        case 1:
            vnpCall = vnStore->VNPairForFunc(call->TypeGet(), vnf, vnpArgs[0]);
            break;
        case 2:
            vnpCall = vnStore->VNPairForFunc(call->TypeGet(), vnf, vnpArgs[0], vnpArgs[1]);
            break;
        case 3:
            vnpCall = vnStore->VNPairForFunc(call->TypeGet(), vnf, vnpArgs[0], vnpArgs[1], vnpArgs[2]);
            break;
        default:
            noway_assert(vnpArgIndex == 4);
            vnpCall = vnStore->VNPairForFunc(call->TypeGet(), vnf, vnpArgs[0], vnpArgs[1], vnpArgs[2], vnpArgs[3]);
            break;
    }

    call->SetVNP(vnStore->VNPWithExc(vnpCall, vnpExc));
}

void Compiler::fgValueNumberCall(GenTreeCall* call)
{
    // Copy argument value numbers from actual arguments to ARGPLACE nodes.
    // TODO-MIKE-Review: Is this actually needed?
    CallInfo* info = call->GetInfo();

    for (unsigned i = 0, count = info->GetArgCount(); i < count; i++)
    {
        CallArgInfo* argInfo = info->GetArgInfo(i);

        if (argInfo->use->GetNode()->OperIs(GT_ARGPLACE))
        {
            argInfo->use->GetNode()->SetVNP(argInfo->GetNode()->GetVNP());
        }
    }

    if (call->gtCallType == CT_HELPER)
    {
        bool modHeap = fgValueNumberHelperCall(call);

        if (modHeap)
        {
            vnClearGcHeap(call DEBUGARG("helper call"));
        }
    }
    else
    {
        if (call->TypeGet() == TYP_VOID)
        {
            call->gtVNPair.SetBoth(ValueNumStore::VNForVoid());
        }
        else
        {
            call->gtVNPair.SetBoth(vnStore->VNForExpr(compCurBB, call->TypeGet()));
        }

        vnClearGcHeap(call DEBUGARG("user call"));
    }
}

VNFunc Compiler::fgValueNumberJitHelperMethodVNFunc(CorInfoHelpFunc helpFunc)
{
    assert(s_helperCallProperties.IsPure(helpFunc) || s_helperCallProperties.IsAllocator(helpFunc));

    VNFunc vnf = VNF_Boundary; // An illegal value...
    switch (helpFunc)
    {
        // These translate to other function symbols:
        case CORINFO_HELP_DIV:
            vnf = VNFunc(GT_DIV);
            break;
        case CORINFO_HELP_MOD:
            vnf = VNFunc(GT_MOD);
            break;
        case CORINFO_HELP_UDIV:
            vnf = VNFunc(GT_UDIV);
            break;
        case CORINFO_HELP_UMOD:
            vnf = VNFunc(GT_UMOD);
            break;
        case CORINFO_HELP_LLSH:
            vnf = VNFunc(GT_LSH);
            break;
        case CORINFO_HELP_LRSH:
            vnf = VNFunc(GT_RSH);
            break;
        case CORINFO_HELP_LRSZ:
            vnf = VNFunc(GT_RSZ);
            break;
        case CORINFO_HELP_LMUL:
        case CORINFO_HELP_LMUL_OVF:
            vnf = VNFunc(GT_MUL);
            break;
        case CORINFO_HELP_ULMUL_OVF:
            vnf = VNFunc(GT_MUL);
            break; // Is this the right thing?
        case CORINFO_HELP_LDIV:
            vnf = VNFunc(GT_DIV);
            break;
        case CORINFO_HELP_LMOD:
            vnf = VNFunc(GT_MOD);
            break;
        case CORINFO_HELP_ULDIV:
            vnf = VNFunc(GT_UDIV);
            break;
        case CORINFO_HELP_ULMOD:
            vnf = VNFunc(GT_UMOD);
            break;

        case CORINFO_HELP_LNG2DBL:
            vnf = VNF_Lng2Dbl;
            break;
        case CORINFO_HELP_ULNG2DBL:
            vnf = VNF_ULng2Dbl;
            break;
        case CORINFO_HELP_DBL2INT:
            vnf = VNF_Dbl2Int;
            break;
        case CORINFO_HELP_DBL2INT_OVF:
            vnf = VNF_Dbl2Int;
            break;
        case CORINFO_HELP_DBL2LNG:
            vnf = VNF_Dbl2Lng;
            break;
        case CORINFO_HELP_DBL2LNG_OVF:
            vnf = VNF_Dbl2Lng;
            break;
        case CORINFO_HELP_DBL2UINT:
            vnf = VNF_Dbl2UInt;
            break;
        case CORINFO_HELP_DBL2UINT_OVF:
            vnf = VNF_Dbl2UInt;
            break;
        case CORINFO_HELP_DBL2ULNG:
            vnf = VNF_Dbl2ULng;
            break;
        case CORINFO_HELP_DBL2ULNG_OVF:
            vnf = VNF_Dbl2ULng;
            break;
        case CORINFO_HELP_FLTREM:
            vnf = VNFunc(GT_MOD);
            break;
        case CORINFO_HELP_DBLREM:
            vnf = VNFunc(GT_MOD);
            break;
        case CORINFO_HELP_FLTROUND:
            vnf = VNF_FltRound;
            break; // Is this the right thing?
        case CORINFO_HELP_DBLROUND:
            vnf = VNF_DblRound;
            break; // Is this the right thing?

        // These allocation operations probably require some augmentation -- perhaps allocSiteId,
        // something about array length...
        case CORINFO_HELP_NEWFAST:
        case CORINFO_HELP_NEWSFAST:
        case CORINFO_HELP_NEWSFAST_FINALIZE:
        case CORINFO_HELP_NEWSFAST_ALIGN8:
        case CORINFO_HELP_NEWSFAST_ALIGN8_VC:
        case CORINFO_HELP_NEWSFAST_ALIGN8_FINALIZE:
            vnf = VNF_JitNew;
            break;

        case CORINFO_HELP_READYTORUN_NEW:
            vnf = VNF_JitReadyToRunNew;
            break;

        case CORINFO_HELP_NEWARR_1_DIRECT:
        case CORINFO_HELP_NEWARR_1_OBJ:
        case CORINFO_HELP_NEWARR_1_VC:
        case CORINFO_HELP_NEWARR_1_ALIGN8:
            vnf = VNF_JitNewArr;
            break;

        case CORINFO_HELP_READYTORUN_NEWARR_1:
            vnf = VNF_JitReadyToRunNewArr;
            break;

        case CORINFO_HELP_GETGENERICS_GCSTATIC_BASE:
            vnf = VNF_GetgenericsGcstaticBase;
            break;
        case CORINFO_HELP_GETGENERICS_NONGCSTATIC_BASE:
            vnf = VNF_GetgenericsNongcstaticBase;
            break;
        case CORINFO_HELP_GETSHARED_GCSTATIC_BASE:
            vnf = VNF_GetsharedGcstaticBase;
            break;
        case CORINFO_HELP_GETSHARED_NONGCSTATIC_BASE:
            vnf = VNF_GetsharedNongcstaticBase;
            break;
        case CORINFO_HELP_GETSHARED_GCSTATIC_BASE_NOCTOR:
            vnf = VNF_GetsharedGcstaticBaseNoctor;
            break;
        case CORINFO_HELP_GETSHARED_NONGCSTATIC_BASE_NOCTOR:
            vnf = VNF_GetsharedNongcstaticBaseNoctor;
            break;
        case CORINFO_HELP_READYTORUN_STATIC_BASE:
            vnf = VNF_ReadyToRunStaticBase;
            break;
        case CORINFO_HELP_READYTORUN_GENERIC_STATIC_BASE:
            vnf = VNF_ReadyToRunGenericStaticBase;
            break;
        case CORINFO_HELP_GETSHARED_GCSTATIC_BASE_DYNAMICCLASS:
            vnf = VNF_GetsharedGcstaticBaseDynamicclass;
            break;
        case CORINFO_HELP_GETSHARED_NONGCSTATIC_BASE_DYNAMICCLASS:
            vnf = VNF_GetsharedNongcstaticBaseDynamicclass;
            break;
        case CORINFO_HELP_CLASSINIT_SHARED_DYNAMICCLASS:
            vnf = VNF_ClassinitSharedDynamicclass;
            break;
        case CORINFO_HELP_GETGENERICS_GCTHREADSTATIC_BASE:
            vnf = VNF_GetgenericsGcthreadstaticBase;
            break;
        case CORINFO_HELP_GETGENERICS_NONGCTHREADSTATIC_BASE:
            vnf = VNF_GetgenericsNongcthreadstaticBase;
            break;
        case CORINFO_HELP_GETSHARED_GCTHREADSTATIC_BASE:
            vnf = VNF_GetsharedGcthreadstaticBase;
            break;
        case CORINFO_HELP_GETSHARED_NONGCTHREADSTATIC_BASE:
            vnf = VNF_GetsharedNongcthreadstaticBase;
            break;
        case CORINFO_HELP_GETSHARED_GCTHREADSTATIC_BASE_NOCTOR:
            vnf = VNF_GetsharedGcthreadstaticBaseNoctor;
            break;
        case CORINFO_HELP_GETSHARED_NONGCTHREADSTATIC_BASE_NOCTOR:
            vnf = VNF_GetsharedNongcthreadstaticBaseNoctor;
            break;
        case CORINFO_HELP_GETSHARED_GCTHREADSTATIC_BASE_DYNAMICCLASS:
            vnf = VNF_GetsharedGcthreadstaticBaseDynamicclass;
            break;
        case CORINFO_HELP_GETSHARED_NONGCTHREADSTATIC_BASE_DYNAMICCLASS:
            vnf = VNF_GetsharedNongcthreadstaticBaseDynamicclass;
            break;
        case CORINFO_HELP_GETSTATICFIELDADDR_CONTEXT:
            vnf = VNF_GetStaticAddrContext;
            break;
        case CORINFO_HELP_GETSTATICFIELDADDR_TLS:
            vnf = VNF_GetStaticAddrTLS;
            break;

        case CORINFO_HELP_RUNTIMEHANDLE_METHOD:
        case CORINFO_HELP_RUNTIMEHANDLE_METHOD_LOG:
            vnf = VNF_RuntimeHandleMethod;
            break;

        case CORINFO_HELP_READYTORUN_GENERIC_HANDLE:
            vnf = VNF_ReadyToRunGenericHandle;
            break;

        case CORINFO_HELP_RUNTIMEHANDLE_CLASS:
        case CORINFO_HELP_RUNTIMEHANDLE_CLASS_LOG:
            vnf = VNF_RuntimeHandleClass;
            break;

        case CORINFO_HELP_STRCNS:
            vnf = VNF_LazyStrCns;
            break;

        case CORINFO_HELP_CHKCASTCLASS:
        case CORINFO_HELP_CHKCASTCLASS_SPECIAL:
        case CORINFO_HELP_CHKCASTARRAY:
        case CORINFO_HELP_CHKCASTINTERFACE:
        case CORINFO_HELP_CHKCASTANY:
            vnf = VNF_CastClass;
            break;

        case CORINFO_HELP_READYTORUN_CHKCAST:
            vnf = VNF_ReadyToRunCastClass;
            break;

        case CORINFO_HELP_ISINSTANCEOFCLASS:
        case CORINFO_HELP_ISINSTANCEOFINTERFACE:
        case CORINFO_HELP_ISINSTANCEOFARRAY:
        case CORINFO_HELP_ISINSTANCEOFANY:
            vnf = VNF_IsInstanceOf;
            break;

        case CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE:
            vnf = VNF_TypeHandleToRuntimeType;
            break;

        case CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPEHANDLE:
            vnf = VNF_TypeHandleToRuntimeTypeHandle;
            break;

        case CORINFO_HELP_ARE_TYPES_EQUIVALENT:
            vnf = VNF_AreTypesEquivalent;
            break;

        case CORINFO_HELP_READYTORUN_ISINSTANCEOF:
            vnf = VNF_ReadyToRunIsInstanceOf;
            break;

        case CORINFO_HELP_LDELEMA_REF:
            vnf = VNF_LdElemA;
            break;

        case CORINFO_HELP_UNBOX:
            vnf = VNF_Unbox;
            break;

        // A constant within any method.
        case CORINFO_HELP_GETCURRENTMANAGEDTHREADID:
            vnf = VNF_ManagedThreadId;
            break;

        case CORINFO_HELP_GETREFANY:
            // TODO-CQ: This should really be interpreted as just a struct field reference, in terms of values.
            vnf = VNF_GetRefanyVal;
            break;

        case CORINFO_HELP_GETCLASSFROMMETHODPARAM:
            vnf = VNF_GetClassFromMethodParam;
            break;

        case CORINFO_HELP_GETSYNCFROMCLASSHANDLE:
            vnf = VNF_GetSyncFromClassHandle;
            break;

        case CORINFO_HELP_LOOP_CLONE_CHOICE_ADDR:
            vnf = VNF_LoopCloneChoiceAddr;
            break;

        case CORINFO_HELP_BOX:
            vnf = VNF_Box;
            break;

        case CORINFO_HELP_BOX_NULLABLE:
            vnf = VNF_BoxNullable;
            break;

        default:
            unreached();
    }

    assert(vnf != VNF_Boundary);
    return vnf;
}

bool Compiler::fgValueNumberHelperCall(GenTreeCall* call)
{
    CorInfoHelpFunc helpFunc    = eeGetHelperNum(call->gtCallMethHnd);
    bool            pure        = s_helperCallProperties.IsPure(helpFunc);
    bool            isAlloc     = s_helperCallProperties.IsAllocator(helpFunc);
    bool            modHeap     = s_helperCallProperties.MutatesHeap(helpFunc);
    bool            mayRunCctor = s_helperCallProperties.MayRunCctor(helpFunc);
    bool            noThrow     = s_helperCallProperties.NoThrow(helpFunc);

    ValueNumPair vnpExc = ValueNumStore::VNPForEmptyExcSet();

    // If the JIT helper can throw an exception make sure that we fill in
    // vnpExc with a Value Number that represents the exception(s) that can be thrown.
    if (!noThrow)
    {
        // If the helper is known to only throw only one particular exception
        // we can set vnpExc to that exception, otherwise we conservatively
        // model the JIT helper as possibly throwing multiple different exceptions
        //
        switch (helpFunc)
        {
            // This helper always throws the VNF_OverflowExc exception.
            case CORINFO_HELP_OVERFLOW:
                vnpExc = vnStore->VNPExcSetSingleton(
                    vnStore->VNPairForFunc(TYP_REF, VNF_OverflowExc, vnStore->VNPForVoid()));
                break;

            default:
                // Setup vnpExc with the information that multiple different exceptions
                // could be generated by this helper
                vnpExc = vnStore->VNPExcSetSingleton(vnStore->VNPairForFunc(TYP_REF, VNF_HelperMultipleExc));
        }
    }

    ValueNumPair vnpNorm;

    if (call->TypeGet() == TYP_VOID)
    {
        vnpNorm = ValueNumStore::VNPForVoid();
    }
    else
    {
        // TODO-CQ: this is a list of helpers we're going to treat as non-pure,
        // because they raise complications.  Eventually, we need to handle those complications...
        bool needsFurtherWork = false;
        switch (helpFunc)
        {
            case CORINFO_HELP_NEW_MDARR:
                // This is a varargs helper.  We need to represent the array shape in the VN world somehow.
                needsFurtherWork = true;
                break;
            default:
                break;
        }

        if (!needsFurtherWork && (pure || isAlloc))
        {
            VNFunc vnf = fgValueNumberJitHelperMethodVNFunc(helpFunc);

            if (mayRunCctor)
            {
                if ((call->gtFlags & GTF_CALL_HOISTABLE) == 0)
                {
                    modHeap = true;
                }
            }

            fgValueNumberHelperCallFunc(call, vnf, vnpExc);
            return modHeap;
        }
        else
        {
            vnpNorm.SetBoth(vnStore->VNForExpr(compCurBB, call->TypeGet()));
        }
    }

    call->gtVNPair = vnStore->VNPWithExc(vnpNorm, vnpExc);
    return modHeap;
}

ValueNum Compiler::vnGetBaseAddr(ValueNum addrVN)
{
    ValueNum  baseLVN = addrVN;
    ssize_t   offsetL = 0;
    VNFuncApp funcAttr;

    while (vnStore->GetVNFunc(baseLVN, &funcAttr) && (funcAttr.m_func == (VNFunc)GT_ADD) &&
           (vnStore->TypeOfVN(baseLVN) == TYP_BYREF))
    {
        // The arguments in value numbering functions are sorted in increasing order
        // Thus either arg could be the constant.
        if (vnStore->IsVNConstant(funcAttr.m_args[0]) && varTypeIsIntegral(vnStore->TypeOfVN(funcAttr.m_args[0])))
        {
            offsetL += vnStore->CoercedConstantValue<ssize_t>(funcAttr.m_args[0]);
            baseLVN = funcAttr.m_args[1];
        }
        else if (vnStore->IsVNConstant(funcAttr.m_args[1]) && varTypeIsIntegral(vnStore->TypeOfVN(funcAttr.m_args[1])))
        {
            offsetL += vnStore->CoercedConstantValue<ssize_t>(funcAttr.m_args[1]);
            baseLVN = funcAttr.m_args[0];
        }
        else // neither argument is a constant
        {
            break;
        }

        // TODO-MIKE-Review: This doesn't make a lot of sense...
        if (fgIsBigOffset(offsetL))
        {
            baseLVN = addrVN;
            break;
        }
    }

    return baseLVN;
}

ValueNum Compiler::vnAddNullPtrExset(ValueNum addrVN)
{
    // Peel off constant offsets so that obj.x and obj.y get the same exception VN.
    addrVN = vnGetBaseAddr(addrVN);

    // TODO-MIKE-Review: Shouldn't this be done before vnGetBaseAddr?
    ValueNum addrExset;
    vnStore->VNUnpackExc(addrVN, &addrVN, &addrExset);

    ValueNum nullPtrExset = vnStore->VNExcSetSingleton(vnStore->VNForFunc(TYP_REF, VNF_NullPtrExc, addrVN));
    return vnStore->VNExcSetUnion(nullPtrExset, addrExset);
}

ValueNumPair Compiler::vnAddNullPtrExset(ValueNumPair addrVNP)
{
    return {vnAddNullPtrExset(addrVNP.GetLiberal()), vnAddNullPtrExset(addrVNP.GetConservative())};
}

void Compiler::vnAddNullPtrExset(GenTree* node, GenTree* addr)
{
    assert(node->IsIndir() || node->OperIsImplicitIndir() || node->OperIs(GT_ARR_LENGTH));

    ValueNum libExset = vnAddNullPtrExset(addr->GetLiberalVN());
    ValueNum conExset = vnAddNullPtrExset(addr->GetConservativeVN());

    node->SetVNP(vnStore->VNPWithExc(vnStore->VNPNormalPair(node->GetVNP()), {libExset, conExset}));
}

//--------------------------------------------------------------------------------
// fgValueNumberAddExceptionSetForDivison
//         - Adds the exception sets for the current tree node
//           which is performing an integer division operation
//
// Arguments:
//    tree       - The current GenTree node,
//                 It must be a node that performs an integer division
//
// Return Value:
//               - The tree's gtVNPair is updated to include
//                 VNF_DivideByZeroExc and VNF_ArithmeticExc,
//                 We will omit one or both of them when the operation
//                 has constants arguments that preclude the exception.
//
void Compiler::fgValueNumberAddExceptionSetForDivision(GenTree* tree)
{
    genTreeOps oper = tree->OperGet();

    // A Divide By Zero exception may be possible.
    // The divisor is held in tree->AsOp()->gtOp2
    //
    bool isUnsignedOper         = (oper == GT_UDIV) || (oper == GT_UMOD);
    bool needDivideByZeroExcLib = true;
    bool needDivideByZeroExcCon = true;
    bool needArithmeticExcLib   = !isUnsignedOper; // Overflow isn't possible for unsigned divide
    bool needArithmeticExcCon   = !isUnsignedOper;

    // Determine if we have a 32-bit or 64-bit divide operation
    var_types typ = genActualType(tree->TypeGet());
    assert((typ == TYP_INT) || (typ == TYP_LONG));

    // Retrieve the Norm VN for op2 to use it for the DivideByZeroExc
    ValueNumPair vnpOp2Norm   = vnStore->VNPNormalPair(tree->AsOp()->gtOp2->gtVNPair);
    ValueNum     vnOp2NormLib = vnpOp2Norm.GetLiberal();
    ValueNum     vnOp2NormCon = vnpOp2Norm.GetConservative();

    if (typ == TYP_INT)
    {
        if (vnStore->IsVNConstant(vnOp2NormLib))
        {
            INT32 kVal = vnStore->ConstantValue<INT32>(vnOp2NormLib);
            if (kVal != 0)
            {
                needDivideByZeroExcLib = false;
            }
            if (!isUnsignedOper && (kVal != -1))
            {
                needArithmeticExcLib = false;
            }
        }
        if (vnStore->IsVNConstant(vnOp2NormCon))
        {
            INT32 kVal = vnStore->ConstantValue<INT32>(vnOp2NormCon);
            if (kVal != 0)
            {
                needDivideByZeroExcCon = false;
            }
            if (!isUnsignedOper && (kVal != -1))
            {
                needArithmeticExcCon = false;
            }
        }
    }
    else // (typ == TYP_LONG)
    {
        if (vnStore->IsVNConstant(vnOp2NormLib))
        {
            INT64 kVal = vnStore->ConstantValue<INT64>(vnOp2NormLib);
            if (kVal != 0)
            {
                needDivideByZeroExcLib = false;
            }
            if (!isUnsignedOper && (kVal != -1))
            {
                needArithmeticExcLib = false;
            }
        }
        if (vnStore->IsVNConstant(vnOp2NormCon))
        {
            INT64 kVal = vnStore->ConstantValue<INT64>(vnOp2NormCon);
            if (kVal != 0)
            {
                needDivideByZeroExcCon = false;
            }
            if (!isUnsignedOper && (kVal != -1))
            {
                needArithmeticExcCon = false;
            }
        }
    }

    // Retrieve the Norm VN for op1 to use it for the ArithmeticExc
    ValueNumPair vnpOp1Norm   = vnStore->VNPNormalPair(tree->AsOp()->gtOp1->gtVNPair);
    ValueNum     vnOp1NormLib = vnpOp1Norm.GetLiberal();
    ValueNum     vnOp1NormCon = vnpOp1Norm.GetConservative();

    if (needArithmeticExcLib || needArithmeticExcCon)
    {
        if (typ == TYP_INT)
        {
            if (vnStore->IsVNConstant(vnOp1NormLib))
            {
                INT32 kVal = vnStore->ConstantValue<INT32>(vnOp1NormLib);

                if (!isUnsignedOper && (kVal != INT32_MIN))
                {
                    needArithmeticExcLib = false;
                }
            }
            if (vnStore->IsVNConstant(vnOp1NormCon))
            {
                INT32 kVal = vnStore->ConstantValue<INT32>(vnOp1NormCon);

                if (!isUnsignedOper && (kVal != INT32_MIN))
                {
                    needArithmeticExcCon = false;
                }
            }
        }
        else // (typ == TYP_LONG)
        {
            if (vnStore->IsVNConstant(vnOp1NormLib))
            {
                INT64 kVal = vnStore->ConstantValue<INT64>(vnOp1NormLib);

                if (!isUnsignedOper && (kVal != INT64_MIN))
                {
                    needArithmeticExcLib = false;
                }
            }
            if (vnStore->IsVNConstant(vnOp1NormCon))
            {
                INT64 kVal = vnStore->ConstantValue<INT64>(vnOp1NormCon);

                if (!isUnsignedOper && (kVal != INT64_MIN))
                {
                    needArithmeticExcCon = false;
                }
            }
        }
    }

    // Unpack, Norm,Exc for the tree's VN
    ValueNumPair vnpTreeNorm;
    ValueNumPair vnpTreeExc;
    ValueNumPair vnpDivZeroExc = ValueNumStore::VNPForEmptyExcSet();
    ValueNumPair vnpArithmExc  = ValueNumStore::VNPForEmptyExcSet();

    vnStore->VNPUnpackExc(tree->gtVNPair, &vnpTreeNorm, &vnpTreeExc);

    if (needDivideByZeroExcLib)
    {
        vnpDivZeroExc.SetLiberal(
            vnStore->VNExcSetSingleton(vnStore->VNForFunc(TYP_REF, VNF_DivideByZeroExc, vnOp2NormLib)));
    }
    if (needDivideByZeroExcCon)
    {
        vnpDivZeroExc.SetConservative(
            vnStore->VNExcSetSingleton(vnStore->VNForFunc(TYP_REF, VNF_DivideByZeroExc, vnOp2NormCon)));
    }
    if (needArithmeticExcLib)
    {
        vnpArithmExc.SetLiberal(
            vnStore->VNExcSetSingleton(vnStore->VNForFunc(TYP_REF, VNF_ArithmeticExc, vnOp1NormLib, vnOp2NormLib)));
    }
    if (needArithmeticExcCon)
    {
        vnpArithmExc.SetConservative(
            vnStore->VNExcSetSingleton(vnStore->VNForFunc(TYP_REF, VNF_ArithmeticExc, vnOp1NormLib, vnOp2NormCon)));
    }

    // Combine vnpDivZeroExc with the exception set of tree
    ValueNumPair newExcSet = vnStore->VNPExcSetUnion(vnpTreeExc, vnpDivZeroExc);
    // Combine vnpArithmExc with the newExcSet
    newExcSet = vnStore->VNPExcSetUnion(newExcSet, vnpArithmExc);

    // Updated VN for tree, it now includes DivideByZeroExc and/or ArithmeticExc
    tree->gtVNPair = vnStore->VNPWithExc(vnpTreeNorm, newExcSet);
}

//--------------------------------------------------------------------------------
// fgValueNumberAddExceptionSetForOverflow
//         - Adds the exception set for the current tree node
//           which is performing an overflow checking math operation
//
// Arguments:
//    tree       - The current GenTree node,
//                 It must be a node that performs an overflow
//                 checking math operation
//
// Return Value:
//               - The tree's gtVNPair is updated to include the VNF_OverflowExc
//                 exception set, except for constant VNs and those produced from identities.
//
void Compiler::fgValueNumberAddExceptionSetForOverflow(GenTree* tree)
{
    assert(tree->gtOverflowEx());

    // We should only be dealing with an Overflow checking ALU operation.
    VNFunc vnf = GetVNFuncForNode(tree);
    assert(ValueNumStore::VNFuncIsOverflowArithmetic(vnf));

    ValueNumKind vnKinds[2] = {VNK_Liberal, VNK_Conservative};
    for (ValueNumKind vnKind : vnKinds)
    {
        ValueNum vn = tree->GetVN(vnKind);

        // Unpack Norm, Exc for the current VN.
        ValueNum vnNorm;
        ValueNum vnExcSet;
        vnStore->VNUnpackExc(vn, &vnNorm, &vnExcSet);

        // Don't add exceptions if the normal VN represents a constant.
        // We only fold to constant VNs for operations that provably cannot overflow.
        if (vnStore->IsVNConstant(vnNorm))
        {
            continue;
        }

        // Don't add exceptions if the tree's normal VN has been derived from an identity.
        // This takes care of x + 0 == x, 0 + x == x, x - 0 == x, x * 1 == x, 1 * x == x.
        // The x - x == 0 and x * 0 == 0, 0 * x == 0 cases are handled by the "IsVNConstant" check above.
        if ((vnNorm == vnStore->VNNormalValue(tree->gtGetOp1()->GetVN(vnKind))) ||
            (vnNorm == vnStore->VNNormalValue(tree->gtGetOp2()->GetVN(vnKind))))
        {
            // TODO-Review: would it be acceptable to make ValueNumStore::EvalUsingMathIdentity
            // public just to assert here?
            continue;
        }

#ifdef DEBUG
        // The normal value number function should now be the same overflow checking ALU operation as 'vnf'.
        VNFuncApp normFuncApp;
        assert(vnStore->GetVNFunc(vnNorm, &normFuncApp) && (normFuncApp.m_func == vnf));
#endif // DEBUG

        // Overflow-checking operations add an overflow exception.
        // The normal result is used as the input argument for the OverflowExc.
        ValueNum vnOverflowExc = vnStore->VNExcSetSingleton(vnStore->VNForFunc(TYP_REF, VNF_OverflowExc, vnNorm));

        // Combine the new Overflow exception with the original exception set.
        vnExcSet = vnStore->VNExcSetUnion(vnExcSet, vnOverflowExc);

        // Update the VN to include the Overflow exception.
        ValueNum newVN = vnStore->VNWithExc(vnNorm, vnExcSet);
        tree->SetVN(vnKind, newVN);
    }
}

//--------------------------------------------------------------------------------
// fgValueNumberAddExceptionSetForBoundsCheck
//          - Adds the exception set for the current tree node
//            which is performing an bounds check operation
//
// Arguments:
//    tree  - The current GenTree node,
//            It must be a node that performs a bounds check operation
//
// Return Value:
//          - The tree's gtVNPair is updated to include the
//            VNF_IndexOutOfRangeExc exception set.
//
void Compiler::fgValueNumberAddExceptionSetForBoundsCheck(GenTree* tree)
{
    GenTreeBoundsChk* node = tree->AsBoundsChk();
    assert(node != nullptr);

    ValueNumPair vnpIndex  = node->gtIndex->gtVNPair;
    ValueNumPair vnpArrLen = node->gtArrLen->gtVNPair;

    // Unpack, Norm,Exc for the tree's VN
    //
    ValueNumPair vnpTreeNorm;
    ValueNumPair vnpTreeExc;

    vnStore->VNPUnpackExc(tree->gtVNPair, &vnpTreeNorm, &vnpTreeExc);

    // Construct the exception set for bounds check
    ValueNumPair boundsChkExcSet = vnStore->VNPExcSetSingleton(
        vnStore->VNPairForFunc(TYP_REF, VNF_IndexOutOfRangeExc, vnStore->VNPNormalPair(vnpIndex),
                               vnStore->VNPNormalPair(vnpArrLen)));

    // Combine the new Overflow exception with the original exception set of tree
    ValueNumPair newExcSet = vnStore->VNPExcSetUnion(vnpTreeExc, boundsChkExcSet);

    // Update the VN for the tree it, the updated VN for tree
    // now includes the IndexOutOfRange exception.
    tree->gtVNPair = vnStore->VNPWithExc(vnpTreeNorm, newExcSet);
}

//--------------------------------------------------------------------------------
// fgValueNumberAddExceptionSetForCkFinite
//         - Adds the exception set for the current tree node
//           which is a CkFinite operation
//
// Arguments:
//    tree       - The current GenTree node,
//                 It must be a CkFinite node
//
// Return Value:
//               - The tree's gtVNPair is updated to include the VNF_ArithmeticExc
//                 exception set.
//
void Compiler::fgValueNumberAddExceptionSetForCkFinite(GenTree* tree)
{
    // We should only be dealing with an check finite operation.
    assert(tree->OperGet() == GT_CKFINITE);

    // Unpack, Norm,Exc for the tree's VN
    //
    ValueNumPair vnpTreeNorm;
    ValueNumPair vnpTreeExc;
    ValueNumPair newExcSet;

    vnStore->VNPUnpackExc(tree->gtVNPair, &vnpTreeNorm, &vnpTreeExc);

    // ckfinite adds an Arithmetic exception
    // The normal result is used as the input argument for the ArithmeticExc
    ValueNumPair arithmeticExcSet =
        vnStore->VNPExcSetSingleton(vnStore->VNPairForFunc(TYP_REF, VNF_ArithmeticExc, vnpTreeNorm));

    // Combine the new Arithmetic exception with the original exception set of tree
    newExcSet = vnStore->VNPExcSetUnion(vnpTreeExc, arithmeticExcSet);

    // Updated VN for tree, it now includes Arithmetic exception
    tree->gtVNPair = vnStore->VNPWithExc(vnpTreeNorm, newExcSet);
}

void Compiler::vnAddNodeExceptionSet(GenTree* node)
{
    if (!node->OperMayThrow(this))
    {
        return;
    }

    switch (node->GetOper())
    {
        case GT_ADD:
        case GT_SUB:
        case GT_MUL:
            fgValueNumberAddExceptionSetForOverflow(node);
            return;
        case GT_DIV:
        case GT_UDIV:
        case GT_MOD:
        case GT_UMOD:
            fgValueNumberAddExceptionSetForDivision(node);
            return;
        case GT_ARR_ELEM:
            vnAddNullPtrExset(node, node->AsArrElem()->GetArray());
            return;
        default:
            unreached();
    }
}

#ifdef DEBUG

void Compiler::vnpPrint(ValueNumPair vnp, unsigned level)
{
    if (vnp.BothEqual())
    {
        vnPrint(vnp.GetLiberal(), level);
    }
    else
    {
        printf("<l:");
        vnPrint(vnp.GetLiberal(), level);
        printf(", c:");
        vnPrint(vnp.GetConservative(), level);
        printf(">");
    }
}

void Compiler::vnPrint(ValueNum vn, unsigned level)
{

    if (ValueNumStore::isReservedVN(vn))
    {
        printf(ValueNumStore::reservedName(vn));
    }
    else
    {
        printf(FMT_VN, vn);
        if (level > 0)
        {
            vnStore->vnDump(this, vn);
        }
    }
}

void Compiler::vnPrintHeapVN(ValueNum vn, const char* comment)
{
    vnPrintMemVN(GcHeap, vn, comment);
}

void Compiler::vnPrintMemVN(MemoryKind kind, ValueNum vn, const char* comment)
{
    if (verbose)
    {
        printf("    %s = ", memoryKindNames[kind]);
        vnPrint(vn, 1);

        if (comment != nullptr)
        {
            printf(" ; %s", comment);
        }

        printf("\n");
    }
}

void Compiler::vnPrintArrayElemAddr(const VNFuncApp& elemAddr)
{
    if (verbose)
    {
        assert(elemAddr.m_func == VNF_PtrToArrElem);

        ValueNum      elemTypeVN = elemAddr.m_args[0];
        ValueNum      arrayVN    = elemAddr.m_args[1];
        ValueNum      indexVN    = elemAddr.m_args[2];
        FieldSeqNode* fieldSeq   = vnStore->FieldSeqVNToFieldSeq(elemAddr.m_args[3]);

        unsigned     elemTypeNum = static_cast<unsigned>(vnStore->ConstantValue<ssize_t>(elemAddr.m_args[0]));
        ClassLayout* elemLayout  = typIsLayoutNum(elemTypeNum) ? typGetLayoutByNum(elemTypeNum) : nullptr;
        var_types elemType = elemLayout == nullptr ? static_cast<var_types>(elemTypeNum) : typGetStructType(elemLayout);

        printf("    VNF_PtrToArrElem(");
        printf("%s", varTypeName(elemType));
        if (elemLayout != nullptr)
        {
            printf("<%s>", elemLayout->GetClassName());
        }
        printf(", ");
        vnPrint(arrayVN, 1);
        printf(", ");
        vnPrint(indexVN, 1);
        if (fieldSeq != nullptr)
        {
            printf(", ");
            dmpFieldSeqFields(fieldSeq);
        }
        printf(")\n");
    }
}
#endif // DEBUG
