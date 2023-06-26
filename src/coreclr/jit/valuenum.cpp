// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "ssabuilder.h"
#include "valuenum.h"

using FieldHandleSet = JitHashSet<CORINFO_FIELD_HANDLE, JitPtrKeyFuncs<struct CORINFO_FIELD_STRUCT_>>;
using TypeNumSet     = JitHashSet<unsigned, JitSmallPrimitiveKeyFuncs<unsigned>>;

struct VNLoop
{
    FieldHandleSet* modifiedFields         = nullptr;
    TypeNumSet*     modifiedArrayElemTypes = nullptr;

    bool hasMemoryHavoc = false;
    // TODO-MIKE-CQ: We could record individual AX local access like we do for fields and arrays.
    bool modifiesAddressExposedLocals = false;
};

class VNLoopMemorySummary
{
    Compiler* m_compiler;
    VNLoop*   m_vnLoopTable;
    LoopDsc*  m_loopTable;
    unsigned  m_loopNum;
    bool      m_memoryHavoc;
    bool      m_hasCall;
    bool      m_modifiesAddressExposedLocals;

public:
    VNLoopMemorySummary(ValueNumbering* valueNumbering, unsigned loopNum);
    void AddMemoryHavoc();
    void AddCall();
    void AddAddressExposedLocal(unsigned lclNum);
    void AddField(CORINFO_FIELD_HANDLE fieldHandle);
    void AddArrayType(unsigned elemTypeNum);
    bool IsComplete() const;
    void UpdateLoops() const;
};

class ValueNumbering
{
    friend class ValueNumberState;
    friend class VNLoopMemorySummary;
    friend class ValueNumStore;

    SsaOptimizer&  ssa;
    Compiler*      compiler;
    ICorJitInfo*   vm;
    ValueNumStore* vnStore;
    VNLoop*        vnLoopTable   = nullptr;
    ValueNum       fgCurMemoryVN = NoVN;

public:
    ValueNumbering(SsaOptimizer& ssa);
    void Run();

private:
    void SummarizeLoopMemoryStores();
    void SummarizeLoopBlockMemoryStores(BasicBlock* block, VNLoopMemorySummary& summary);
    void SummarizeLoopNodeMemoryStores(GenTree* node, VNLoopMemorySummary& summary);
    void SummarizeLoopLocalDefs(GenTreeLclDef* def, VNLoopMemorySummary& summary);
    void SummarizeLoopIndirMemoryStores(GenTreeIndir* store, VNLoopMemorySummary& summary);
    void SummarizeLoopObjFieldMemoryStores(GenTreeIndir* store, FieldSeqNode* fieldSeq, VNLoopMemorySummary& summary);
    void SummarizeLoopLocalMemoryStores(GenTreeLclVarCommon* store, VNLoopMemorySummary& summary);
    void SummarizeLoopCallMemoryStores(GenTreeCall* call, VNLoopMemorySummary& summary);
    bool BlockIsLoopEntry(BasicBlock* block, unsigned* loopNum);

    ValueNum BuildLoopEntryMemory(BasicBlock* entryBlock, unsigned loopNum);
    void ClearMemory(GenTree* node DEBUGARG(const char* comment = nullptr));
    void UpdateMemory(GenTree* node, ValueNum memVN DEBUGARG(const char* comment = nullptr));

    void NumberInitDefs();
    void NumberBlocks();
    void NumberBlock(BasicBlock* block);
    void NumberNode(GenTree* node);
    void NumberComma(GenTreeOp* comma);
    void NumberLclDef(GenTreeLclDef* def);
    void NumberLclUse(GenTreeLclUse* use);
    void NumberInsert(GenTreeInsert* insert);
    void NumberExtract(GenTreeExtract* extract);
    void NumberLclStore(GenTreeLclVar* store);
    void NumberLclLoad(GenTreeLclVar* load);
    void NumberLclFldStore(GenTreeLclFld* store);
    void NumberLclFldLoad(GenTreeLclFld* load);
    void NumberIndirStore(GenTreeIndir* store);
    void NumberIndirLoad(GenTreeIndir* load);
    void NumberNullCheck(GenTreeIndir* node);
    void NumberArrLen(GenTreeArrLen* node);
    void NumberCmpXchg(GenTreeCmpXchg* node);
    void NumberInterlocked(GenTreeOp* node);
    void NumberCast(GenTreeCast* cast);
    void NumberBitCast(GenTreeUnOp* bitcast);
    void NumberCall(GenTreeCall* call);
    void NumberHelperCall(GenTreeCall* call, VNFunc vnf, ValueNumPair vnpExc);
    bool NumberHelperCall(GenTreeCall* call);
    void NumberIntrinsic(GenTreeIntrinsic* intrinsic);
    void NumberBoundsCheck(GenTreeBoundsChk* check);
    void NumberDivMod(GenTreeOp* node);
#ifdef FEATURE_HW_INTRINSICS
    void NumberHWIntrinsic(GenTreeHWIntrinsic* node);
#endif

    ValueNum GetIntConVN(GenTreeIntCon* intCon);
    ValueNum LoadMemory(var_types type, ValueNum addrVN);
    ValueNum StoreStaticField(GenTreeIndir* store, FieldSeqNode* fieldSeq, GenTree* value);
    ValueNum LoadStaticField(GenTreeIndir* load, FieldSeqNode* fieldSeq);
    ValueNum StoreObjField(GenTreeIndir* store, ValueNum objVN, FieldSeqNode* fieldSeq, GenTree* value);
    ValueNum LoadObjField(GenTreeIndir* load, ValueNum objVN, FieldSeqNode* fieldSeq);
    ValueNum StoreArrayElem(GenTreeIndir* store, const VNFuncApp& elemAddr, GenTree* value);
    ValueNum LoadArrayElem(GenTreeIndir* store, const VNFuncApp& elemAddr);
    ValueNum StoreAddressExposedLocal(GenTree* store, ValueNum lclAddrVN, GenTree* value);
    ValueNum CastStruct(ValueNumKind         vnk,
                        ValueNum             valueVN,
                        CORINFO_CLASS_HANDLE fromClassHandle,
                        CORINFO_CLASS_HANDLE toClassHandle);
    ValueNum CastStruct(ValueNumKind vnk, ValueNum valueVN, ClassLayout* fromLayout, ClassLayout* toLayout);
    ValueNumPair CastStruct(ValueNumPair valueVNP, ClassLayout* fromLayout, ClassLayout* toLayout);
    ValueNum CoerceStoreValue(
        GenTree* store, GenTree* value, ValueNumKind vnk, var_types fieldType, ClassLayout* fieldLayout);
    var_types GetFieldType(CORINFO_FIELD_HANDLE fieldHandle, ClassLayout** fieldLayout);
    ValueNum InsertStructField(GenTree*      store,
                               GenTree*      value,
                               ValueNumKind  vnk,
                               ValueNum      structVN,
                               var_types     structType,
                               FieldSeqNode* fieldSeq);
    ValueNum ExtractStructField(GenTree* load, ValueNumKind vnk, ValueNum structVN, FieldSeqNode* fieldSeq);
    ValueNumPair ExtractStructField(GenTree* load, ValueNumPair structVN, FieldSeqNode* fieldSeq);
    ValueNum CoerceLoadValue(GenTree* load, ValueNum valueVN, var_types fieldType, ClassLayout* fieldLayout);
    ValueNum AddField(GenTreeOp* add);
    FieldSeqNode* IsFieldAddr(GenTree* addr, GenTree** obj);
    FieldSeqNode* IsStaticStructFieldAddr(GenTree* addr);
    bool IsArrayElemAddr(GenTree* addr, ArrayInfo* arrayInfo);
    VNFunc GetHelperCallFunc(CorInfoHelpFunc helpFunc);
    ValueNum GetBaseAddr(ValueNum addrVN);
    ValueNum AddNullRefExset(ValueNum addrVN);
    ValueNumPair AddNullRefExset(ValueNumPair addrVNP);
    void AddNullRefExset(GenTree* node, GenTree* addr);
    void AddOverflowExset(GenTreeOp* node);
    void NumbeCkFinite(GenTreeUnOp* node);

#ifdef DEBUG
    void TraceLocal(unsigned lclNum, ValueNumPair vnp, const char* comment = nullptr);
    void TraceMem(ValueNum vn, const char* comment = nullptr);
#endif

    LclVarDsc* lvaGetDesc(unsigned lclNum) const
    {
        return compiler->lvaGetDesc(lclNum);
    }

    LclVarDsc* lvaGetDesc(GenTreeLclVarCommon* lclNode) const
    {
        return compiler->lvaGetDesc(lclNode);
    }
};

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
            assert(varTypeIsIntegralOrI(node->gtGetOp1()));
            assert(varTypeIsIntegralOrI(node->gtGetOp2()));
            if (node->gtOverflow())
            {
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
bool ValueNumStore::IsOverflowIntDiv(int64_t v0, int64_t v1)
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
bool ValueNumStore::IsIntZero(int64_t v)
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

ValueNumStore::ValueNumStore(SsaOptimizer& ssa)
    : ssa(ssa)
    , compiler(ssa.GetCompiler())
    , alloc(ssa.GetCompiler()->getAllocator(CMK_ValueNumber))
    , m_fixedPointMapSels(alloc)
    , m_checkedBoundVNs(alloc)
    , m_chunks(alloc)
#ifdef DEBUG
    , m_vnNameMap(compiler->getAllocator(CMK_DebugOnly))
#endif
{
    for (unsigned i = 0; i < _countof(m_smallInt32VNMap); i++)
    {
        m_smallInt32VNMap[i] = NoVN;
    }

    // We will reserve chunk 0 to hold some special constants, like the constant NULL,
    // the "exception" value, and the "zero map."
    Chunk* specialConstChunk = new (alloc) Chunk(alloc, &m_nextChunkBase, TYP_REF, ChunkKind::Const);
    // Implicitly allocate 0 ==> NULL, and 1 ==> Exception, 2 ==> ZeroMap.
    specialConstChunk->m_count += SRC_NumSpecialRefConsts;
    assert(m_chunks.Size() == 0);
    m_chunks.Push(specialConstChunk);

    m_mapSelectBudget = (int)JitConfig.JitVNMapSelBudget(); // We cast the unsigned DWORD to a signed int.

    // This value must be non-negative and non-zero, reset the value to DefaultVNMapSelectBudget if it isn't.
    if (m_mapSelectBudget <= 0)
    {
        m_mapSelectBudget = DefaultVNMapSelectBudget;
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
        case GT_FNEG:
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
            case GT_FADD:
                return FpAdd<double, DoubleTraits>(v0, v1);
            case GT_FSUB:
                return FpSub<double, DoubleTraits>(v0, v1);
            case GT_FMUL:
                return FpMul<double, DoubleTraits>(v0, v1);
            case GT_FDIV:
                return FpDiv<double, DoubleTraits>(v0, v1);
            case GT_FMOD:
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
            case GT_FADD:
                return FpAdd<float, FloatTraits>(v0, v1);
            case GT_FSUB:
                return FpSub<float, FloatTraits>(v0, v1);
            case GT_FMUL:
                return FpMul<float, FloatTraits>(v0, v1);
            case GT_FDIV:
                return FpDiv<float, FloatTraits>(v0, v1);
            case GT_FMOD:
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

ValueNum ValueNumStore::ExsetCreate(ValueNum x)
{
    return VNForFunc(TYP_REF, VNF_ExsetCons, x, EmptyExsetVN());
}

ValueNumPair ValueNumStore::ExsetCreate(ValueNumPair xp)
{
    return {ExsetCreate(xp.GetLiberal()), ExsetCreate(xp.GetConservative())};
}

#ifdef DEBUG
bool ValueNumStore::ExsetIsOrdered(ValueNum item, ValueNum xs1)
{
    if (xs1 == EmptyExsetVN())
    {
        return true;
    }

    VNFuncApp funcXs1;
    GetVNFunc(xs1, &funcXs1);
    assert(funcXs1.m_func == VNF_ExsetCons);

    return item < funcXs1[0];
}
#endif // DEBUG

ValueNum ValueNumStore::ExsetUnion(ValueNum xs0, ValueNum xs1)
{
    if (xs0 == EmptyExsetVN())
    {
        return xs1;
    }

    if (xs1 == EmptyExsetVN())
    {
        return xs0;
    }

    const VNFuncDef2* cons0 = IsVNFunc<VNFuncDef2>(xs0, VNF_ExsetCons);
    const VNFuncDef2* cons1 = IsVNFunc<VNFuncDef2>(xs1, VNF_ExsetCons);

    if (cons0->m_arg0 < cons1->m_arg0)
    {
        assert(ExsetIsOrdered(cons0->m_arg0, cons0->m_arg1));

        // add the lower one (from xs0) to the result, advance xs0
        return VNForFunc(TYP_REF, VNF_ExsetCons, cons0->m_arg0, ExsetUnion(cons0->m_arg1, xs1));
    }

    if (cons0->m_arg0 > cons1->m_arg0)
    {
        assert(ExsetIsOrdered(cons1->m_arg0, cons1->m_arg1));

        // add the lower one (from xs1) to the result, advance xs1
        return VNForFunc(TYP_REF, VNF_ExsetCons, cons1->m_arg0, ExsetUnion(xs0, cons1->m_arg1));
    }

    assert(cons0->m_arg0 == cons1->m_arg0);
    assert(ExsetIsOrdered(cons0->m_arg0, cons0->m_arg1));
    assert(ExsetIsOrdered(cons1->m_arg0, cons1->m_arg1));

    // Equal elements; add one (from xs0) to the result, advance both sets
    return VNForFunc(TYP_REF, VNF_ExsetCons, cons0->m_arg0, ExsetUnion(cons0->m_arg1, cons1->m_arg1));
}

ValueNumPair ValueNumStore::ExsetUnion(ValueNumPair xs0vnp, ValueNumPair xs1vnp)
{
    return {ExsetUnion(xs0vnp.GetLiberal(), xs1vnp.GetLiberal()),
            ExsetUnion(xs0vnp.GetConservative(), xs1vnp.GetConservative())};
}

ValueNum ValueNumStore::ExsetIntersection(ValueNum xs0, ValueNum xs1)
{
    if ((xs0 == EmptyExsetVN()) || (xs1 == EmptyExsetVN()))
    {
        return EmptyExsetVN();
    }

    const VNFuncDef2* cons0 = IsVNFunc<VNFuncDef2>(xs0, VNF_ExsetCons);
    const VNFuncDef2* cons1 = IsVNFunc<VNFuncDef2>(xs1, VNF_ExsetCons);

    if (cons0->m_arg0 < cons1->m_arg0)
    {
        assert(ExsetIsOrdered(cons0->m_arg0, cons0->m_arg1));
        return ExsetIntersection(cons0->m_arg1, xs1);
    }

    if (cons0->m_arg0 > cons1->m_arg0)
    {
        assert(ExsetIsOrdered(cons1->m_arg0, cons1->m_arg1));
        return ExsetIntersection(xs0, cons1->m_arg1);
    }

    assert(cons0->m_arg0 == cons1->m_arg0);
    assert(ExsetIsOrdered(cons0->m_arg0, cons0->m_arg1));
    assert(ExsetIsOrdered(cons1->m_arg0, cons1->m_arg1));

    return VNForFunc(TYP_REF, VNF_ExsetCons, cons0->m_arg0, ExsetIntersection(cons0->m_arg1, cons1->m_arg1));
}

ValueNumPair ValueNumStore::ExsetIntersection(ValueNumPair xs0vnp, ValueNumPair xs1vnp)
{
    return {ExsetIntersection(xs0vnp.GetLiberal(), xs1vnp.GetLiberal()),
            ExsetIntersection(xs0vnp.GetConservative(), xs1vnp.GetConservative())};
}

bool ValueNumStore::ExsetIsSubset(ValueNum subset, ValueNum set)
{
    if (subset == EmptyExsetVN())
    {
        return true;
    }

    if ((set == EmptyExsetVN()) || (set == NoVN))
    {
        return false;
    }

    const VNFuncDef2* funcXsFull = IsVNFunc<VNFuncDef2>(set, VNF_ExsetCons);
    const VNFuncDef2* funcXsCand = IsVNFunc<VNFuncDef2>(subset, VNF_ExsetCons);

    ValueNum vnFullSetPrev = NullVN();
    ValueNum vnCandSetPrev = NullVN();

    ValueNum vnFullSetRemainder = funcXsFull->m_arg1;
    ValueNum vnCandSetRemainder = funcXsCand->m_arg1;

    while (true)
    {
        ValueNum vnFullSetItem = funcXsFull->m_arg0;
        ValueNum vnCandSetItem = funcXsCand->m_arg0;

        // Enforce that both sets are sorted by increasing ValueNumbers
        assert(vnFullSetItem > vnFullSetPrev);
        assert(vnCandSetItem >= vnCandSetPrev); // equal when we didn't advance the candidate set

        if (vnFullSetItem > vnCandSetItem)
        {
            // The Full set does not contain the vnCandSetItem
            return false;
        }

        // When we have a matching value we advance the candidate set
        if (vnFullSetItem == vnCandSetItem)
        {
            // Have we finished matching?
            if (vnCandSetRemainder == EmptyExsetVN())
            {
                // We matched every item in the candidate set'
                return true;
            }

            // Advance the candidate set
            funcXsCand         = IsVNFunc<VNFuncDef2>(vnCandSetRemainder, VNF_ExsetCons);
            vnCandSetRemainder = funcXsCand->m_arg1;
        }

        if (vnFullSetRemainder == EmptyExsetVN())
        {
            // No more items are left in the full exception set
            return false;
        }

        // We will advance the full set
        funcXsFull         = IsVNFunc<VNFuncDef2>(vnFullSetRemainder, VNF_ExsetCons);
        vnFullSetRemainder = funcXsFull->m_arg1;

        vnFullSetPrev = vnFullSetItem;
        vnCandSetPrev = vnCandSetItem;
    }
}

ValueNum ValueNumStore::UnpackExset(ValueNum vn, ValueNum* exset)
{
    if (const VNFuncDef2* valueExset = IsVNFunc<VNFuncDef2>(vn, VNF_ValWithExset))
    {
        *exset = valueExset->m_arg1;
        return valueExset->m_arg0;
    }

    *exset = EmptyExsetVN();
    return vn;
}

ValueNumPair ValueNumStore::UnpackExset(ValueNumPair vnp, ValueNumPair* exset)
{
    return {UnpackExset(vnp.GetLiberal(), exset->GetLiberalAddr()),
            UnpackExset(vnp.GetConservative(), exset->GetConservativeAddr())};
}

ValueNum ValueNumStore::ExtractValue(ValueNum vn) const
{
    VNFuncApp funcApp;
    return GetVNFunc(vn, &funcApp) == VNF_ValWithExset ? funcApp[0] : vn;
}

ValueNumPair ValueNumStore::ExtractValue(ValueNumPair vnp)
{
    return {ExtractValue(vnp.GetLiberal()), ExtractValue(vnp.GetConservative())};
}

ValueNum ValueNumStore::ExtractExset(ValueNum vn)
{
    VNFuncApp funcApp;
    return GetVNFunc(vn, &funcApp) == VNF_ValWithExset ? funcApp[1] : EmptyExsetVN();
}

ValueNumPair ValueNumStore::ExtractExset(ValueNumPair vnp)
{
    return {ExtractExset(vnp.GetLiberal()), ExtractExset(vnp.GetConservative())};
}

ValueNum ValueNumStore::PackExset(ValueNum vn, ValueNum exset)
{
    assert(!IsVNFunc<VNFuncDef2>(vn, VNF_ValWithExset));

    return exset == EmptyExsetVN() ? vn : VNForFunc(TypeOfVN(vn), VNF_ValWithExset, vn, exset);
}

ValueNumPair ValueNumStore::PackExset(ValueNumPair vnp, ValueNumPair exset)
{
    return {PackExset(vnp.GetLiberal(), exset.GetLiberal()), PackExset(vnp.GetConservative(), exset.GetConservative())};
}

#ifdef DEBUG
bool ValueNumStore::HasExset(ValueNum vn) const
{
    return IsVNFunc<VNFuncDef2>(vn, VNF_ValWithExset);
}
#endif

ValueNumStore::Chunk::Chunk(CompAllocator alloc, ValueNum* pNextBaseVN, var_types type, ChunkKind kind)
    : m_baseVN(*pNextBaseVN), m_type(type), m_kind(kind)
{
    // The "values" of special ref consts will be all be "null" -- their differing meanings will
    // be carried by the distinct value numbers.
    static class Object* specialRefConsts[SRC_NumSpecialRefConsts];

    // Allocate "m_defs" here, according to the typ/attribs pair.
    switch (kind)
    {
        case ChunkKind::NotAField:
            break; // Nothing to do.
        case ChunkKind::Const:
            switch (type)
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
                    m_defs = &specialRefConsts[0];
                    break; // Nothing to do.
                default:
                    assert(false); // Should not reach here.
            }
            break;

        case ChunkKind::Handle:
            m_defs = alloc.allocate<VNHandle>(ChunkSize);
            break;
        case ChunkKind::Func0:
            m_defs = alloc.allocate<VNFunc>(ChunkSize);
            break;
        case ChunkKind::Func1:
            m_defs = alloc.allocate<VNFuncDef1>(ChunkSize);
            break;
        case ChunkKind::Func2:
            m_defs = alloc.allocate<VNFuncDef2>(ChunkSize);
            break;
        case ChunkKind::Func3:
            m_defs = alloc.allocate<VNFuncDef3>(ChunkSize);
            break;
        case ChunkKind::Func4:
            m_defs = alloc.allocate<VNFuncDef4>(ChunkSize);
            break;
        default:
            unreached();
    }
    *pNextBaseVN += ChunkSize;
}

ValueNumStore::Chunk* ValueNumStore::GetAllocChunk(var_types type, ChunkKind kind)
{
    unsigned arity = static_cast<unsigned>(kind) - static_cast<unsigned>(ChunkKind::Func0);
    assert(arity <= _countof(m_currentFuncChunk));
    assert(type <= _countof(m_currentFuncChunk[arity]));
    return GetAllocChunk(type, kind, m_currentFuncChunk[arity][type]);
}

ValueNumStore::Chunk* ValueNumStore::GetAllocChunk(var_types type, ChunkKind kind, unsigned& current)
{
    assert(m_chunks.Size() != 0); // The special chunk is always allocated before normal chunks.

    if (current != 0)
    {
        Chunk* chunk = m_chunks.Get(current);
        if (chunk->m_count < ChunkSize)
        {
            return chunk;
        }
    }

    Chunk* chunk = new (alloc) Chunk(alloc, &m_nextChunkBase, type, kind);
    current      = m_chunks.Size();
    m_chunks.Push(chunk);
    return chunk;
}

ValueNum ValueNumStore::VNForHandle(ssize_t value, GenTreeFlags handleKind)
{
    assert((handleKind & ~GTF_ICON_HDL_MASK) == GTF_NONE);

    if (m_handleMap == nullptr)
    {
        m_handleMap = new (alloc) HandleVNMap(alloc);
    }

    VNHandle  handle(value, handleKind);
    ValueNum* vn = m_handleMap->Emplace(handle, NoVN);

    if (*vn == NoVN)
    {
        *vn = GetAllocChunk(TYP_I_IMPL, ChunkKind::Handle, m_currentHandleChunk)->AllocVN(handle);
    }

    return *vn;
}

template <typename T, typename NumMap>
ValueNum ValueNumStore::VnForConst(T cnsVal, NumMap* numMap, var_types varType, unsigned& currentChunk)
{
    ValueNum* vn = numMap->Emplace(cnsVal, NoVN);

    if (*vn == NoVN)
    {
        *vn = GetAllocChunk(varType, ChunkKind::Const, currentChunk)->AllocVN<T>(cnsVal);
    }

    return *vn;
}

ValueNum ValueNumStore::VNForIntCon(int32_t value)
{
    if (IsSmallIntConst(value))
    {
        unsigned index = value - SmallIntConstMin;

        if (m_smallInt32VNMap[index] == NoVN)
        {
            m_smallInt32VNMap[index] =
                GetAllocChunk(TYP_INT, ChunkKind::Const, m_currentInt32ConstChunk)->AllocVN<int32_t>(value);
        }

        return m_smallInt32VNMap[index];
    }

    if (m_int32VNMap == nullptr)
    {
        m_int32VNMap = new (alloc) Int32VNMap(alloc);
    }

    return VnForConst<int32_t, Int32VNMap>(value, m_int32VNMap, TYP_INT, m_currentInt32ConstChunk);
}

ValueNum ValueNumStore::VNForLongCon(int64_t value)
{
    if (m_int64VNMap == nullptr)
    {
        m_int64VNMap = new (alloc) Int64VNMap(alloc);
    }

    return VnForConst<int64_t, Int64VNMap>(value, m_int64VNMap, TYP_LONG, m_currentInt64ConstChunk);
}

ValueNum ValueNumStore::VNForFloatCon(float value)
{
    if (m_floatVNMap == nullptr)
    {
        m_floatVNMap = new (alloc) Int32VNMap(alloc);
    }

    int32_t bits = jitstd::bit_cast<int32_t>(value);
    return VnForConst<int32_t, Int32VNMap>(bits, m_floatVNMap, TYP_FLOAT, m_currentFloatConstChunk);
}

ValueNum ValueNumStore::VNForDoubleCon(double value)
{
    if (m_doubleVNMap == nullptr)
    {
        m_doubleVNMap = new (alloc) Int64VNMap(alloc);
    }

    int64_t bits = jitstd::bit_cast<int64_t>(value);
    return VnForConst<int64_t, Int64VNMap>(bits, m_doubleVNMap, TYP_DOUBLE, m_currentDoubleConstChunk);
}

ValueNum ValueNumStore::VNForDblCon(var_types type, double value)
{
    if (type == TYP_FLOAT)
    {
        return VNForFloatCon(static_cast<float>(value));
    }
    else
    {
        assert(type == TYP_DOUBLE);
        return VNForDoubleCon(value);
    }
}

ValueNum ValueNumStore::VNForByrefCon(target_size_t value)
{
    if (m_byrefVNMap == nullptr)
    {
        m_byrefVNMap = new (alloc) ByrefVNMap(alloc);
    }

    return VnForConst<target_size_t, ByrefVNMap>(value, m_byrefVNMap, TYP_BYREF, m_currentByrefConstChunk);
}

ValueNum ValueNumStore::VNForBitCastOper(var_types castToType)
{
    assert(castToType != TYP_STRUCT);

    uint32_t packedCastType = int32_t(castToType) << int32_t(VCA_BitCount);
    assert((packedCastType & int32_t(VCA_ReservedBits)) == 0);

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

    *pSrcIsUnsigned = (value & int32_t(VCA_UnsignedSrc)) != 0;
    *pCastToType    = var_types(value >> int32_t(VCA_BitCount));

    assert(VNForCastOper(*pCastToType, *pSrcIsUnsigned) == vn);
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
            return NullVN();
        case TYP_BYREF:
            return VNForByrefCon(0);
        case TYP_STRUCT:
#ifdef FEATURE_SIMD
        // TODO-MIKE-Consider: Using maps for SIMD values is questionable...
        case TYP_SIMD8:
        case TYP_SIMD12:
        case TYP_SIMD16:
        case TYP_SIMD32:
#endif
            return ZeroMapVN();
        default:
            unreached();
    }
}

ValueNum ValueNumStore::ZeroMapVN()
{
    if (m_zeroMap == NoVN)
    {
        m_zeroMap = VNForFunc(TYP_STRUCT, VNF_ZeroMap);
    }

    return m_zeroMap;
}

ValueNum ValueNumStore::ReadOnlyMemoryMapVN()
{
    if (m_readOnlyMemoryMap == NoVN)
    {
        m_readOnlyMemoryMap = VNForExpr(compiler->fgFirstBB, TYP_STRUCT);
    }

    return m_readOnlyMemoryMap;
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

ValueNum ValueNumStore::VNForFunc(var_types type, VNFunc func)
{
    assert(VNFuncArityIsLegal(func, 0));
    assert(func != VNF_NotAField);

    if (m_func0VNMap == nullptr)
    {
        m_func0VNMap = new (alloc) Func0VNMap(alloc);
    }

    ValueNum* vn = m_func0VNMap->Emplace(func, NoVN);

    if (*vn == NoVN)
    {
        *vn = GetAllocChunk(type, ChunkKind::Func0)->AllocVN(func);
    }

    return *vn;
}

ValueNum ValueNumStore::VNForFunc(var_types type, VNFunc func, ValueNum arg0)
{
    assert(func != VNF_MemOpaque);
    assert(!HasExset(arg0));

    if (CanEvalForConstantArgs(func) && IsVNConstant(arg0))
    {
        return EvalFuncForConstantArgs(type, func, arg0);
    }

    if (m_func1VNMap == nullptr)
    {
        m_func1VNMap = new (alloc) Func1VNMap(alloc);
    }

    VNFuncDef1 func1(func, arg0);
    ValueNum*  vn = m_func1VNMap->Emplace(func1, NoVN);

    if (*vn == NoVN)
    {
        *vn = GetAllocChunk(type, ChunkKind::Func1)->AllocVN(func1);
    }

    return *vn;
}

ValueNum ValueNumStore::HasFunc(var_types type, VNFunc func, ValueNum arg0)
{
    assert(func != VNF_MemOpaque);
    assert(!HasExset(arg0));

    if (m_func1VNMap == nullptr)
    {
        return false;
    }

    ValueNum* funcVN = m_func1VNMap->LookupPointer({func, arg0});
    return funcVN == nullptr ? NoVN : *funcVN;
}

ValueNum ValueNumStore::VNForFunc(var_types type, VNFunc func, ValueNum arg0, ValueNum arg1)
{
    assert(arg0 != NoVN && arg1 != NoVN);
    assert(!HasExset(arg0));
    assert(!HasExset(arg1));
    assert(VNFuncArityIsLegal(func, 2));
    assert(func != VNF_MapSelect); // Use the special function VNForMapSelect defined for that.

    if (func == VNF_CastClass)
    {
        // In terms of values, a castclass always returns its second argument, the object being cast.
        // The operation may also throw an exception
        return PackExset(arg1, ExsetCreate(VNForFunc(TYP_REF, VNF_InvalidCastExc, arg1, arg0)));
    }

    if (CanEvalForConstantArgs(func) && IsVNConstant(arg0) && IsVNConstant(arg1))
    {
        bool canFold = true;

        // Special case for VNF_Cast of constant handles
        // Don't allow an eval/fold of a GT_CAST(non-I_IMPL, Handle)
        if (VNFuncIsNumericCast(func) && (type != TYP_I_IMPL) && IsVNHandle(arg0))
        {
            canFold = false;
        }

        // It is possible for us to have mismatched types (see Bug 750863)
        // We don't try to fold a binary operation when one of the constant operands
        // is a floating-point constant and the other is not, except for casts.
        // For casts, the second operand just carries the information about the source.

        bool arg0IsFloating = varTypeIsFloating(TypeOfVN(arg0));
        bool arg1IsFloating = varTypeIsFloating(TypeOfVN(arg1));

        if (!VNFuncIsNumericCast(func) && (arg0IsFloating != arg1IsFloating))
        {
            canFold = false;
        }

        if (type == TYP_BYREF)
        {
            // We don't want to fold expressions that produce TYP_BYREF
            canFold = false;
        }

        bool shouldFold = canFold;

        if (canFold)
        {
            // We can fold the expression, but we don't want to fold
            // when the expression will always throw an exception
            shouldFold = VNEvalShouldFold(type, func, arg0, arg1);
        }

        if (shouldFold)
        {
            return EvalFuncForConstantArgs(type, func, arg0, arg1);
        }
    }

    // We canonicalize commutative operations.
    // (Perhaps should eventually handle associative/commutative [AC] ops -- but that gets complicated...)
    if (VNFuncIsCommutative(func) && (arg0 > arg1))
    {
        std::swap(arg0, arg1);
    }

    if (m_func2VNMap == nullptr)
    {
        m_func2VNMap = new (alloc) Func2VNMap(alloc);
    }

    VNFuncDef2 func2(func, arg0, arg1);
    ValueNum   vn;

    if (!m_func2VNMap->Lookup(func2, &vn))
    {
        vn = EvalUsingMathIdentity(type, func, arg0, arg1);

        if ((vn == NoVN) || (TypeOfVN(vn) != type))
        {
            vn = GetAllocChunk(type, ChunkKind::Func2)->AllocVN(func2);
            m_func2VNMap->Set(func2, vn);
        }
    }

    return vn;
}

ValueNum ValueNumStore::VNForFunc(var_types type, VNFunc func, ValueNum arg0, ValueNum arg1, ValueNum arg2)
{
    assert(arg0 != NoVN && arg1 != NoVN && arg2 != NoVN);
    assert(VNFuncArityIsLegal(func, 3));
    assert(!HasExset(arg0));
    assert(!HasExset(arg1));
    assert(!HasExset(arg2));

    if (m_func3VNMap == nullptr)
    {
        m_func3VNMap = new (alloc) Func3VNMap(alloc);
    }

    VNFuncDef3 func3(func, arg0, arg1, arg2);
    ValueNum*  vn = m_func3VNMap->Emplace(func3, NoVN);

    if (*vn == NoVN)
    {
        *vn = GetAllocChunk(type, ChunkKind::Func3)->AllocVN(func3);
    }

    return *vn;
}

ValueNum ValueNumStore::VNForFunc(
    var_types type, VNFunc func, ValueNum arg0, ValueNum arg1, ValueNum arg2, ValueNum arg3)
{
    assert(arg0 != NoVN && arg1 != NoVN && arg2 != NoVN && arg3 != NoVN);
    assert(!HasExset(arg0));
    assert(!HasExset(arg1));
    assert(!HasExset(arg2));
    assert((func == VNF_MapStore) || !HasExset(arg3));
    assert(VNFuncArityIsLegal(func, 4));

    if (m_func4VNMap == nullptr)
    {
        m_func4VNMap = new (alloc) Func4VNMap(alloc);
    }

    VNFuncDef4 func4(func, arg0, arg1, arg2, arg3);
    ValueNum*  vn = m_func4VNMap->Emplace(func4, NoVN);

    if (*vn == NoVN)
    {
        *vn = GetAllocChunk(type, ChunkKind::Func4)->AllocVN(func4);
    }

    return *vn;
}

ValueNum ValueNumStore::VNForMapStore(var_types type, ValueNum mapVN, ValueNum indexVN, ValueNum valueVN)
{
    assert(varTypeIsStruct(type));

    ValueNum vn = VNForFunc(type, VNF_MapStore, mapVN, indexVN, valueVN, m_currentBlock->GetLoopNum());
    INDEBUG(Trace(vn));
    return vn;
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

    INDEBUG(Trace(result));

    return result;
}

ValueNumPair ValueNumStore::VNForMapSelect(var_types type, ValueNumPair map, ValueNumPair index)
{
    return {VNForMapSelect(VNK_Liberal, type, map.GetLiberal(), index.GetLiberal()),
            VNForMapSelect(VNK_Conservative, type, map.GetConservative(), index.GetConservative())};
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
    ValueNumKind vnk, var_types typ, ValueNum mapVN, const ValueNum indexVN, int* pBudget, bool* pUsedRecursiveVN)
{
// This label allows us to directly implement a tail call by setting up the arguments, and doing a goto to here.
TailCall:
    assert((mapVN != NoVN) && (indexVN != NoVN));
    assert(!HasExset(mapVN));
    assert(!HasExset(indexVN));
    assert(varTypeIsStruct(TypeOfVN(mapVN)));

    *pUsedRecursiveVN = false;

#ifdef DEBUG
    // Provide a mechanism for writing tests that ensure we don't call this ridiculously often.
    m_numMapSels++;
    unsigned selLim = JitConfig.JitVNMapSelLimit();
    assert(selLim == 0 || m_numMapSels < selLim);
#endif

    if (mapVN == ZeroMapVN())
    {
        return VNZeroForType(typ);
    }

    VNFuncDef2 fstruct(VNF_MapSelect, mapVN, indexVN);

    if (m_func2VNMap == nullptr)
    {
        m_func2VNMap = new (alloc) Func2VNMap(alloc);
    }
    else if (ValueNum* cached = m_func2VNMap->LookupPointer(fstruct))
    {
        return *cached;
    }

    // Give up if we've run out of budget.
    if (*pBudget == 0)
    {
        // We have to use 'nullptr' for the basic block here, because subsequent expressions
        // in different blocks may find this result in the VNFunc2Map -- other expressions in
        // the IR may "evaluate" to this same VNForExpr, so it is not "unique" in the sense
        // that permits the BasicBlock attribution.
        // TODO-MIKE-Review: This should probably be MapSelect from the current map.
        ValueNum uniqueVN = VNForExpr(nullptr, typ);
        m_func2VNMap->Set(fstruct, uniqueVN);
        return uniqueVN;
    }

    // Reduce our budget by one
    (*pBudget)--;

    // If it's recursive, stop the recursion.
    if (m_fixedPointMapSels.Contains(mapVN))
    {
        *pUsedRecursiveVN = true;
        return RecursiveVN;
    }

    VNFuncApp    funcApp;
    const VNFunc func = GetVNFunc(mapVN, &funcApp);

    if (func == VNF_MapStore)
    {
        ValueNum storeMapVN   = funcApp[0];
        ValueNum storeIndexVN = funcApp[1];

        // select(store(m, i, v), i) == v
        if (storeIndexVN == indexVN)
        {
            RecordLoopMemoryDependence(m_currentNode, m_currentBlock, storeMapVN);
            return funcApp[2];
        }

        // We can skip a store if it doesn't alias this load.
        // Most indices used in maps are field handles (encoded as constants in the VN world),
        // we discard overlapping fields early so if we get 2 different constants we conclude
        // that they're different fields and thus they do not alias.
        // For arrays indices can be arbitrary values but if they're constants then it's clear
        // that we're accessing different array elements that do not alias (provided that we
        // don't mix up array elements of different types in the same map).
        // We can also have local addresses (VNF_LclAddr) values as map indices but we don't
        // try to do loads with these. So in the memory map we can assume that local address
        // values and constant values do not alias, since constants represents static/object
        // fields or array.
        // They could alias in an array map since local addresses are basically constants but
        // good luck writing any meaningful code that uses a local address as an array index.

        if (IsVNConstant(indexVN) && (IsVNConstant(storeIndexVN) || (GetVNFunc(storeIndexVN, &funcApp) == VNF_LclAddr)))
        {
            // This is the equivalent of the recursive tail call:
            // return VNForMapSelect(vnk, typ, storeMapVN, indexVN);
            mapVN = storeMapVN;
            goto TailCall;
        }
    }
    else if ((func == VNF_Phi) || ((func == VNF_MemoryPhi) && (vnk == VNK_Liberal)))
    {
        GetVNFunc(funcApp[0], &funcApp);
        assert(funcApp.m_func == VNF_PhiArgs);
        ValueNum argVN     = funcApp[0];
        ValueNum argListVN = funcApp[1];

        // select(phi(m1, m2), x): if select(m1, x) == select(m2, x), return that, else new fresh.

        if (const VNFuncDef1* f = IsVNFunc<VNFuncDef1>(argVN, VNF_PhiArgDef))
        {
            void* def = ConstantHostPtr<void*>(f->m_arg0);

            if (func == VNF_Phi)
            {
                argVN = static_cast<GenTreeLclDef*>(def)->GetVN(vnk);
            }
            else
            {
                argVN = static_cast<MemoryPhiArg*>(def)->GetDef()->vn;
            }
        }

        if (argVN != NoVN)
        {
            // We need to be careful about breaking infinite recursion. Record the outer map.
            m_fixedPointMapSels.Push(mapVN);

            ValueNum sameValueVN = VNForMapSelectWork(vnk, typ, argVN, indexVN, pBudget, pUsedRecursiveVN);

            // We don't have any budget remaining to verify that all PHI args are the same
            // so setup the default failure case now.
            bool allSame = *pBudget > 0;

            while ((argListVN != NoVN) && allSame)
            {
                if (const VNFuncDef2* f = IsVNFunc<VNFuncDef2>(argListVN, VNF_PhiArgs))
                {
                    argVN     = f->m_arg0;
                    argListVN = f->m_arg1;
                }
                else
                {
                    argVN     = argListVN;
                    argListVN = NoVN;
                }

                if (const VNFuncDef1* f = IsVNFunc<VNFuncDef1>(argVN, VNF_PhiArgDef))
                {
                    void* def = ConstantHostPtr<void*>(f->m_arg0);

                    if (func == VNF_Phi)
                    {
                        argVN = static_cast<GenTreeLclDef*>(def)->GetVN(vnk);
                    }
                    else
                    {
                        argVN = static_cast<MemoryPhiArg*>(def)->GetDef()->vn;
                    }

                    if (argVN == NoVN)
                    {
                        allSame = false;
                        break;
                    }
                }

                bool     usedRecursiveVN = false;
                ValueNum valueVN         = VNForMapSelectWork(vnk, typ, argVN, indexVN, pBudget, &usedRecursiveVN);
                *pUsedRecursiveVN |= usedRecursiveVN;

                if (sameValueVN == RecursiveVN)
                {
                    sameValueVN = valueVN;
                }

                if ((valueVN != RecursiveVN) && (valueVN != sameValueVN))
                {
                    allSame = false;
                    break;
                }
            }

            assert(m_fixedPointMapSels.Top() == mapVN);
            m_fixedPointMapSels.Pop();

            if (allSame && (sameValueVN != RecursiveVN))
            {
                // To avoid exponential searches, we make sure that this result is memo-ized.
                // The result is always valid for memoization if we didn't rely on RecursiveVN to get it.
                // If RecursiveVN was used, we are processing a loop and we can't memo-ize this intermediate
                // result if, e.g., this block is in a multi-entry loop.
                if (!*pUsedRecursiveVN)
                {
                    m_func2VNMap->Set(fstruct, sameValueVN);
                }

                return sameValueVN;
            }
        }

        // Otherwise, fall through to creating the select(phi(m1, m2), x) function application.
    }
    else
    {
        // TODO-MIKE-Consider: Using maps for SIMD values is questionable...
        assert(((func == VNF_MemoryPhi) && (vnk == VNK_Conservative)) || (func == VNF_MemOpaque) ||
               (func == VNF_MapSelect) || varTypeIsSIMD(TypeOfVN(mapVN)));
    }

    ValueNum* vn = m_func2VNMap->Emplace(fstruct, NoVN);

    // We may have run out of budget and already assigned a result
    if (*vn == NoVN)
    {
        *vn = GetAllocChunk(typ, ChunkKind::Func2)->AllocVN(fstruct);
    }

    return *vn;
}

// Record that tree's value number is dependent on a particular memory VN.
// Only tracks trees in loops, and memory updates in the same loop nest.
// So this is a coarse-grained dependence that is only usable for hoisting
// tree out of its enclosing loops.
void ValueNumStore::RecordLoopMemoryDependence(GenTree* node, BasicBlock* block, ValueNum memoryVN)
{
    // If tree is not in a loop, we don't need to track its loop dependence.
    unsigned const loopNum = block->GetLoopNum();

    if (loopNum == NoLoopNum)
    {
        return;
    }

    unsigned updateLoopNum = LoopOfVN(memoryVN);

    if (updateLoopNum >= MaxLoopNum)
    {
        // There should be only two special non-loop loop nums.
        assert((updateLoopNum == MaxLoopNum) || (updateLoopNum == NoLoopNum));

        // memoryVN defined outside of any loop, we can ignore.
        JITDUMP("      ==> Not updating loop memory dependence of [%06u], memory " FMT_VN " not defined in a loop\n",
                node->GetID(), memoryVN);
        return;
    }

    LoopDsc* loopTable = ssa.GetLoopTable();

    // If the loop was removed, then record the dependence in the nearest enclosing loop, if any.
    while ((loopTable[updateLoopNum].lpFlags & LPFLG_REMOVED) != 0)
    {
        unsigned const updateParentLoopNum = loopTable[updateLoopNum].lpParent;

        if (updateParentLoopNum == NoLoopNum)
        {
            // Memory VN was defined in a loop, but no longer.
            JITDUMP("      ==> Not updating loop memory dependence of [%06u], memory " FMT_VN
                    " no longer defined in a loop\n",
                    node->GetID(), memoryVN);
            break;
        }

        JITDUMP("      ==> " FMT_LP " removed, updating dependence to parent " FMT_LP "\n", updateLoopNum,
                updateParentLoopNum);

        updateLoopNum = updateParentLoopNum;
    }

    // If the update block is not the the header of a loop containing
    // block, we can also ignore the update.
    if (!compiler->optLoopContains(updateLoopNum, loopNum))
    {
        JITDUMP("      ==> Not updating loop memory dependence of [%06u]/" FMT_LP ", memory " FMT_VN "/" FMT_LP
                " is not defined in an enclosing loop\n",
                node->GetID(), loopNum, memoryVN, updateLoopNum);
        return;
    }

    // If we already have a recorded a loop entry block for this tree,
    // see if the new update is for a more closely nested loop.
    BasicBlock* mapBlock = nullptr;

    if (m_nodeToLoopMemoryBlockMap == nullptr)
    {
        m_nodeToLoopMemoryBlockMap = new (alloc) NodeBlockMap(alloc);
    }
    else if (m_nodeToLoopMemoryBlockMap->Lookup(node, &mapBlock))
    {
        unsigned const mapLoopNum = mapBlock->GetLoopNum();

        // If the update loop contains the existing map loop,
        // the existing map loop is more constraining.
        // So no update needed.
        if (compiler->optLoopContains(updateLoopNum, mapLoopNum))
        {
            JITDUMP("      ==> Not updating loop memory dependence of [%06u]; alrady constrained to " FMT_LP
                    " nested in " FMT_LP "\n",
                    node->GetID(), mapLoopNum, updateLoopNum);
            return;
        }
    }

    // MemoryVN now describes the most constraining loop memory dependence we know of. Update the map.
    JITDUMP("      ==> Updating loop memory dependence of [%06u] to " FMT_LP "\n", node->GetID(), updateLoopNum);
    m_nodeToLoopMemoryBlockMap->Set(node, loopTable[updateLoopNum].lpEntry, NodeBlockMap::Overwrite);
}

// Record that tree's loop memory dependence is the same as some other tree.
void ValueNumStore::CopyLoopMemoryDependence(GenTree* fromNode, GenTree* toNode)
{
    BasicBlock* block;
    if ((m_nodeToLoopMemoryBlockMap != nullptr) && m_nodeToLoopMemoryBlockMap->Lookup(fromNode, &block))
    {
        m_nodeToLoopMemoryBlockMap->Set(toNode, block);
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
            int32_t resVal = EvalOp<int32_t>(func, ConstantValue<int32_t>(arg0VN));
            return IsVNHandle(arg0VN) ? VNForHandle(ssize_t(resVal), GetHandleFlags(arg0VN)) : VNForIntCon(resVal);
        }
        case TYP_LONG:
        {
            int64_t resVal = EvalOp<int64_t>(func, ConstantValue<int64_t>(arg0VN));
            return IsVNHandle(arg0VN) ? VNForHandle(ssize_t(resVal), GetHandleFlags(arg0VN)) : VNForLongCon(resVal);
        }
        case TYP_FLOAT:
            return VNForFloatCon(EvalOp<float>(func, ConstantValue<float>(arg0VN)));
        case TYP_DOUBLE:
            return VNForDoubleCon(EvalOp<double>(func, ConstantValue<double>(arg0VN)));
        default:
            unreached();
    }
}

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

// Given an integer constant value number return its value as an int64_t.
//
int64_t ValueNumStore::GetConstantInt64(ValueNum argVN)
{
    assert(IsVNConstant(argVN));
    var_types argVNtyp = TypeOfVN(argVN);

    int64_t result = 0;

    switch (argVNtyp)
    {
        case TYP_INT:
            result = (int64_t)ConstantValue<int>(argVN);
            break;
        case TYP_LONG:
            result = ConstantValue<int64_t>(argVN);
            break;
        case TYP_REF:
        case TYP_BYREF:
            result = (int64_t)ConstantValue<size_t>(argVN);
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
    assert(!HasExset(arg0VN) && !HasExset(arg1VN)); // Otherwise, would not be constant.

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
            int64_t arg0Val = ConstantValue<int64_t>(arg0VN);
            int64_t arg1Val = ConstantValue<int64_t>(arg1VN);

            if (VNFuncIsComparison(func))
            {
                assert(typ == TYP_INT);
                result = VNForIntCon(EvalComparison(func, arg0Val, arg1Val));
            }
            else
            {
                assert(typ == TYP_LONG);
                int64_t  resultVal = EvalOp<int64_t>(func, arg0Val, arg1Val);
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
        int64_t arg0Val = GetConstantInt64(arg0VN);
        int64_t arg1Val = GetConstantInt64(arg1VN);

        if (VNFuncIsComparison(func))
        {
            assert(typ == TYP_INT);
            result = VNForIntCon(EvalComparison(func, arg0Val, arg1Val));
        }
        else if (typ == TYP_INT) // We could see GT_OR of an int and constant ByRef or Null
        {
            int resultVal = (int)EvalOp<int64_t>(func, arg0Val, arg1Val);
            result        = VNForIntCon(resultVal);
        }
        else
        {
            assert(typ != TYP_INT);
            int64_t resultVal = EvalOp<int64_t>(func, arg0Val, arg1Val);

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
                    result = NullVN();
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
        assert(varActualType(typ) == TYP_INT);

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
            result = VNForFloatCon(EvalOp<float>(func, GetConstantSingle(arg0VN), GetConstantSingle(arg1VN)));
        }
        else
        {
            assert(typ == TYP_DOUBLE);

            result = VNForDoubleCon(EvalOp<double>(func, GetConstantDouble(arg0VN), GetConstantDouble(arg1VN)));
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
                            return VNForLongCon(int64_t(unsigned(arg0Val)));
                        }
                        else
                        {
                            return VNForLongCon(int64_t(arg0Val));
                        }
                    }
                    else
                    {
                        assert(typ == TYP_BYREF);
                        return VNForByrefCon(target_size_t(arg0Val));
                    }
#else // TARGET_32BIT
                    if (srcIsUnsigned)
                        return VNForLongCon(int64_t(unsigned(arg0Val)));
                    else
                        return VNForLongCon(int64_t(arg0Val));
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
                    int64_t arg0Val = GetConstantInt64(arg0VN);
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
                            return VNForIntCon(int32_t(arg0Val));
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
                    return VNForIntCon(int32_t(arg0Val));
                case TYP_UINT:
                    assert(typ == TYP_INT);
                    return VNForIntCon(UINT32(arg0Val));
                case TYP_LONG:
                    assert(typ == TYP_LONG);
                    return VNForLongCon(int64_t(arg0Val));
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
                    return VNForIntCon(int32_t(arg0Val));
                case TYP_UINT:
                    assert(typ == TYP_INT);
                    return VNForIntCon(UINT32(arg0Val));
                case TYP_LONG:
                    assert(typ == TYP_LONG);
                    return VNForLongCon(int64_t(arg0Val));
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

            // Float ops
            case GT_FADD:
            case GT_FSUB:
            case GT_FMUL:
            case GT_FDIV:
            case GT_FMOD:
            case GT_FNEG:

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

        int64_t divisor = CoercedConstantValue<int64_t>(arg1VN);

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
            int64_t dividend    = CoercedConstantValue<int64_t>(arg0VN);
            int64_t badDividend = typ == TYP_INT ? INT32_MIN : INT64_MIN;

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
            int64_t op1 = ConstantValue<int64_t>(arg0VN);
            int64_t op2 = ConstantValue<int64_t>(arg1VN);

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
            case GT_FADD:
                resultVN = identityForAddition();
                break;

            case GT_SUB:
            case GT_FSUB:
                resultVN = identityForSubtraction();
                break;

            case GT_MUL:
            case GT_FMUL:
                resultVN = identityForMultiplication();
                break;

            case GT_DIV:
            case GT_FDIV:
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
                if (((arg0VN == NullVN()) && IsKnownNonNull(arg1VN)) ||
                    ((arg1VN == NullVN()) && IsKnownNonNull(arg0VN)))
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
                if (((arg0VN == NullVN()) && IsKnownNonNull(arg1VN)) ||
                    ((arg1VN == NullVN()) && IsKnownNonNull(arg0VN)))
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

ValueNum ValueNumStore::VNForExpr(BasicBlock* block, var_types type)
{
    LoopNum loopNum = block == nullptr ? MaxLoopNum : block->GetLoopNum();

    return GetAllocChunk(type, ChunkKind::Func1)->AllocVN(VNFuncDef1{VNF_MemOpaque, loopNum});
}

ValueNum ValueNumStore::VNForExpr(var_types type)
{
    return VNForExpr(m_currentBlock, type);
}

ValueNum ValueNumStore::UniqueVN(var_types type)
{
    return VNForExpr(nullptr, type);
}

ValueNum ValueNumStore::VNForTypeNum(unsigned typeNum)
{
    ValueNum vn = VNForIntCon(static_cast<int32_t>(typeNum));

#ifdef DEBUG
    if (compiler->verbose && !m_vnNameMap.Lookup(vn))
    {
        const char* name;

        if (compiler->typIsLayoutNum(typeNum))
        {
            name = compiler->typGetLayoutByNum(typeNum)->GetClassName();
        }
        else
        {
            name = varTypeName(static_cast<var_types>(typeNum));
        }

        m_vnNameMap.Set(vn, name);
    }
#endif

    return vn;
}

ValueNum ValueNumStore::VNForFieldSeqHandle(CORINFO_FIELD_HANDLE fieldHandle)
{
    ValueNum vn = VNForHostPtr(fieldHandle);

#ifdef DEBUG
    if (compiler->verbose && !m_vnNameMap.Lookup(vn))
    {
        const char* className;
        const char* fieldName = compiler->eeGetFieldName(fieldHandle, &className);
        size_t      length    = strlen(className) + strlen(fieldName) + 3;
        char*       name      = m_vnNameMap.GetAllocator().allocate<char>(length);
        _snprintf_s(name, length, _TRUNCATE, "%s::%s", className, fieldName);
        m_vnNameMap.Set(vn, name);
    }
#endif

    return vn;
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
        return GetAllocChunk(TYP_I_IMPL, ChunkKind::NotAField, m_currentNotAFieldChunk)->AllocVN();
    }

    ValueNum fieldSeqVN = VNForFunc(TYP_I_IMPL, VNF_FieldSeq, VNForHostPtr(fieldSeq));
    INDEBUG(Trace(fieldSeqVN));
    return fieldSeqVN;
}

FieldSeqNode* ValueNumStore::FieldSeqVNToFieldSeq(ValueNum vn)
{
    if (vn == VNZeroForType(TYP_I_IMPL))
    {
        return nullptr;
    }

    VNFuncApp funcApp;
    VNFunc    func = GetVNFunc(vn, &funcApp);

    if (func == VNF_NotAField)
    {
        return FieldSeqStore::NotAField();
    }

    assert(func == VNF_FieldSeq);
    return ConstantHostPtr<FieldSeqNode>(funcApp[0]);
}

ValueNum ValueNumStore::FieldSeqVNAppend(ValueNum fieldSeqVN, FieldSeqNode* fieldSeq)
{
    fieldSeqVN = VNForFieldSeq(compiler->GetFieldSeqStore()->Append(FieldSeqVNToFieldSeq(fieldSeqVN), fieldSeq));
    INDEBUG(Trace(fieldSeqVN));
    return fieldSeqVN;
}

ValueNum ValueNumbering::AddField(GenTreeOp* add)
{
    assert(add->OperIs(GT_ADD) && !add->gtOverflow());

    ArrayInfo arrInfo;
    if (IsArrayElemAddr(add, &arrInfo))
    {
        ValueNum indexVN = vnStore->ExtractArrayElementIndex(arrInfo);

        FieldSeqNode* fieldSeq = arrInfo.m_elemOffsetConst->GetFieldSeq()->GetNext();

        if (FieldSeqNode* zeroFieldSeq = compiler->GetZeroOffsetFieldSeq(add))
        {
            fieldSeq = compiler->GetFieldSeqStore()->Append(fieldSeq, zeroFieldSeq);
        }

        ValueNum elemTypeVN = vnStore->VNForTypeNum(arrInfo.m_elemTypeNum);
        ValueNum arrayVN    = vnStore->ExtractValue(arrInfo.m_arrayExpr->GetLiberalVN());
        ValueNum fieldSeqVN = vnStore->VNForFieldSeq(fieldSeq);
        ValueNum addrVN     = vnStore->VNForFunc(TYP_BYREF, VNF_PtrToArrElem, elemTypeVN, arrayVN, indexVN, fieldSeqVN);

        ValueNum exset = vnStore->ExtractExset(add->GetOp(0)->GetLiberalVN());
        exset          = vnStore->ExsetUnion(exset, vnStore->ExtractExset(add->GetOp(1)->GetLiberalVN()));

        return vnStore->PackExset(addrVN, exset);
    }

    if (GenTreeIntCon* intCon = add->GetOp(1)->IsIntCon())
    {
        FieldSeqNode* fldSeq = intCon->GetFieldSeq();
        if ((fldSeq != nullptr) && !fldSeq->IsArrayElement())
        {
            ValueNum exset;
            ValueNum addrVN = vnStore->UnpackExset(add->GetOp(0)->GetLiberalVN(), &exset);

            addrVN = vnStore->ExtendPtrVN(addrVN, fldSeq, static_cast<target_size_t>(intCon->GetValue()));

            return addrVN == NoVN ? addrVN : vnStore->PackExset(addrVN, exset);
        }
    }

    return NoVN;
}

FieldSeqNode* ValueNumbering::IsFieldAddr(GenTree* addr, GenTree** pObj)
{
    FieldSeqNode* fieldSeq     = nullptr;
    bool          mustBeStatic = false;

    addr = addr->SkipComma();

    if (addr->OperIs(GT_ADD))
    {
        GenTree* op1 = addr->AsOp()->GetOp(0);
        GenTree* op2 = addr->AsOp()->GetOp(1);

        if (op1->OperIs(GT_CNS_INT) && op1->IsIconHandle())
        {
            // If one operand is a field sequence/handle, the other operand must not also be a field sequence/handle.
            assert(!op2->OperIs(GT_CNS_INT) || !op2->IsIconHandle());

            fieldSeq = op1->AsIntCon()->GetFieldSeq();
            addr     = op2;
        }
        else if (op2->OperIs(GT_CNS_INT))
        {
            assert(!op1->OperIs(GT_CNS_INT) || !op1->IsIconHandle());

            fieldSeq = op2->AsIntCon()->GetFieldSeq();
            addr     = op1;
        }
    }
    else if ((fieldSeq = compiler->GetZeroOffsetFieldSeq(addr)) != nullptr)
    {
        // Reference type objects can't have a field at offset 0 (that's where the method table
        // pointer is) so this can only be a static field. If it isn't then it means that it's
        // a field of a struct value accessed via an (un)managed pointer and we don't recognize
        // those here.

        mustBeStatic = true;
    }

    if ((fieldSeq == nullptr) || (fieldSeq == FieldSeqStore::NotAField()))
    {
        // If we can't find a field sequence then it's not a field address.
        return nullptr;
    }

    if (fieldSeq->IsArrayElement())
    {
        // Array elements are handled separately.
        return nullptr;
    }

    if (fieldSeq->IsField() && vm->isFieldStatic(fieldSeq->GetFieldHandle()))
    {
        *pObj = nullptr;
        return fieldSeq;
    }

    if (mustBeStatic || !addr->TypeIs(TYP_REF))
    {
        return nullptr;
    }

    addr = addr->gtEffectiveVal();

    if (GenTreeIndir* indir = addr->IsIndir())
    {
        if (FieldSeqNode* staticStructFieldSeq = IsStaticStructFieldAddr(indir->GetAddr()->SkipComma()))
        {
            *pObj = nullptr;
            return compiler->GetFieldSeqStore()->Append(staticStructFieldSeq, fieldSeq);
        }
    }

    if (fieldSeq->IsBoxedValueField())
    {
        // Ignore boxed value fields, the same (pseudo) field is used for all boxed types
        // so a store to such a field may alias multiple actual fields. These can only be
        // disambiguated when used together with a static field.

        return nullptr;
    }

    assert(!vm->isValueClass(vm->getFieldClass(fieldSeq->GetFieldHandle())));

    *pObj = addr;
    return fieldSeq;
}

FieldSeqNode* ValueNumbering::IsStaticStructFieldAddr(GenTree* addr)
{
    FieldSeqNode* fieldSeq = nullptr;

    if (GenTreeClsVar* clsVarAddr = addr->IsClsVar())
    {
        fieldSeq = clsVarAddr->GetFieldSeq();
    }
    else if (GenTreeIntCon* intCon = addr->IsIntCon())
    {
        fieldSeq = intCon->GetFieldSeq();
    }
    else if (addr->OperIs(GT_ADD))
    {
        if (GenTreeIntCon* intCon = addr->AsOp()->GetOp(1)->IsIntCon())
        {
            fieldSeq = intCon->GetFieldSeq();
        }
    }
    else
    {
        fieldSeq = compiler->GetZeroOffsetFieldSeq(addr);
    }

    if ((fieldSeq == nullptr) || (fieldSeq->GetNext() != nullptr) || !fieldSeq->IsField() ||
        !vm->isFieldStatic(fieldSeq->GetFieldHandle()) ||
        (CorTypeToVarType(vm->getFieldType(fieldSeq->GetFieldHandle())) != TYP_STRUCT))
    {
        return nullptr;
    }

    return fieldSeq;
}

bool ValueNumbering::IsArrayElemAddr(GenTree* addr, ArrayInfo* arrayInfo)
{
    if (!addr->OperIs(GT_ADD) || !addr->TypeIs(TYP_BYREF))
    {
        return false;
    }

    GenTree* array  = addr->AsOp()->GetOp(0);
    GenTree* offset = addr->AsOp()->GetOp(1);

    if (!array->TypeIs(TYP_REF))
    {
        if (!offset->TypeIs(TYP_REF))
        {
            return false;
        }

        std::swap(array, offset);
    }

    assert(offset->TypeIs(TYP_I_IMPL));

    GenTree* offsetExpr  = nullptr;
    GenTree* offsetConst = offset;

    if (offset->OperIs(GT_ADD))
    {
        offsetExpr  = offset->AsOp()->GetOp(0);
        offsetConst = offset->AsOp()->GetOp(1);
    }

    if (!offsetConst->IsIntCon())
    {
        return false;
    }

    FieldSeqNode* fieldSeq = offsetConst->AsIntCon()->GetFieldSeq();

    if ((fieldSeq == nullptr) || !fieldSeq->IsArrayElement())
    {
        return false;
    }

    arrayInfo->m_arrayExpr       = array;
    arrayInfo->m_elemOffsetExpr  = offsetExpr;
    arrayInfo->m_elemOffsetConst = offsetConst->AsIntCon();
    arrayInfo->m_elemTypeNum     = fieldSeq->GetArrayElementTypeNum();

    if (!compiler->typIsLayoutNum(arrayInfo->m_elemTypeNum))
    {
        var_types elemType = static_cast<var_types>(arrayInfo->m_elemTypeNum);

        // The runtime allows casting between arrays of signed and unsigned integer types.
        // We're going to treat such arrays as aliased by always using the signed type.
        elemType                 = varTypeToSigned(elemType);
        arrayInfo->m_elemTypeNum = static_cast<unsigned>(elemType);
    }

    return true;
}

ValueNum ValueNumStore::ExtractArrayElementIndex(const ArrayInfo& arrayInfo)
{
    assert(arrayInfo.m_arrayExpr->TypeIs(TYP_REF));
    assert(arrayInfo.m_elemOffsetConst->GetFieldSeq()->IsArrayElement() &&
           (varActualType(arrayInfo.m_elemOffsetConst->GetType()) == TYP_I_IMPL));
    assert((arrayInfo.m_elemOffsetExpr == nullptr) ||
           (varActualType(arrayInfo.m_elemOffsetExpr->GetType()) == TYP_I_IMPL));

    target_size_t elemSize;

    if (compiler->typIsLayoutNum(arrayInfo.m_elemTypeNum))
    {
        elemSize = compiler->typGetLayoutByNum(arrayInfo.m_elemTypeNum)->GetSize();
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
        offsetVN = ExtractValue(arrayInfo.m_elemOffsetExpr->GetLiberalVN());
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

        return (index > INT32_MAX) ? UniqueVN(TYP_I_IMPL) : VNForUPtrSizeIntCon(index);
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

        if (offsetVNFunc.m_func == VNOP_MUL)
        {
            ValueNum scaleVN;

            if (IsVNConstant(offsetVNFunc[1]))
            {
                unscaledOffsetVN = offsetVNFunc[0];
                scaleVN          = offsetVNFunc[1];
            }
            else if (IsVNConstant(offsetVNFunc[0]))
            {
                scaleVN          = offsetVNFunc[0];
                unscaledOffsetVN = offsetVNFunc[1];
            }
            else
            {
                break;
            }

            scale = CoercedConstantValue<target_size_t>(scaleVN);
        }
        else if (offsetVNFunc.m_func == VNOP_LSH)
        {
            ValueNum scaleVN;

            if (IsVNConstant(offsetVNFunc[1]))
            {
                unscaledOffsetVN = offsetVNFunc[0];
                scaleVN          = offsetVNFunc[1];
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
            offsetVN = VNForFunc(TYP_I_IMPL, VNOP_MUL, unscaledOffsetVN, VNForUPtrSizeIntCon(scale / elemSize));
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
        indexVN = VNForFunc(TYP_I_IMPL, VNOP_DIV, offsetVN, VNForUPtrSizeIntCon(elemSize));
    }

    if (index != 0)
    {
        indexVN = VNForFunc(TYP_I_IMPL, VNOP_ADD, indexVN, VNForUPtrSizeIntCon(index));
    }

    return indexVN;
}

ValueNum ValueNumStore::ExtendPtrVN(ValueNum addrVN, FieldSeqNode* fieldSeq, target_size_t offset)
{
    assert(!HasExset(addrVN));
    assert((fieldSeq != nullptr) && !fieldSeq->IsArrayElement());

    VNFuncApp funcApp;
    VNFunc    func = GetVNFunc(addrVN, &funcApp);

    if (func == VNF_LclAddr)
    {
        ValueNum newOffsetVN   = VNForUPtrSizeIntCon(ConstantValue<target_size_t>(funcApp[1]) + offset);
        ValueNum newFieldSeqVN = FieldSeqVNAppend(funcApp[2], fieldSeq);

        return VNForFunc(TYP_I_IMPL, func, funcApp[0], newOffsetVN, newFieldSeqVN);
    }

    if (func == VNF_PtrToArrElem)
    {
        ValueNum newFieldSeqVN = FieldSeqVNAppend(funcApp[3], fieldSeq);

        return VNForFunc(TYP_BYREF, func, funcApp[0], funcApp[1], funcApp[2], newFieldSeqVN);
    }

    // TODO-MIKE-CQ: We could separate the static field handle and struct field sequence in VNF_PtrToStatic.
    // Then even if the field sequence is NotAField we could still know that the store doesn't alias any
    // other static fields, instance fields etc. But having NotAField for a static field is probably rare.
    if ((func == VNF_PtrToStatic) && (fieldSeq != FieldSeqStore::NotAField()))
    {
        return VNForFunc(TYP_BYREF, func, FieldSeqVNAppend(funcApp[0], fieldSeq));
    }

    return NoVN;
}

void ValueNumbering::NumberComma(GenTreeOp* comma)
{
    GenTree* op1 = comma->GetOp(0);
    GenTree* op2 = comma->GetOp(1);

    if (op1->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
    {
        op1 = op1->AsLclVarCommon()->GetOp(0);
    }

    ValueNumPair exset1;
    vnStore->UnpackExset(op1->GetVNP(), &exset1);

    ValueNumPair exset2 = ValueNumStore::EmptyExsetVNP();
    ValueNumPair vnp;

    if (op2->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD, GT_STOREIND, GT_STORE_OBJ, GT_STORE_BLK))
    {
        vnp = ValueNumStore::VoidVNP();
    }
    else
    {
        vnp = vnStore->UnpackExset(op2->GetVNP(), &exset2);
    }

    comma->SetVNP(vnStore->PackExset(vnp, vnStore->ExsetUnion(exset1, exset2)));
}

ValueNum ValueNumbering::CastStruct(ValueNumKind         vnk,
                                    ValueNum             valueVN,
                                    CORINFO_CLASS_HANDLE fromClassHandle,
                                    CORINFO_CLASS_HANDLE toClassHandle)
{
    // IR allows assignment of structs with different layout. This is problematic for
    // value numbering because "maps" used to represent struct values are indexed by
    // field handles rather than field offsets. If the layout is different then field
    // handles may also be different and we end up with stores that are effectively
    // aliased but aren't treated as such: assign a map containing field X to a local
    // that has a field Y at the same offset, store something to field Y of the local
    // then load field X - we'll get back the value originally stored in field X, not
    // the value stored in field Y.
    //
    // Create a map by extracting field values from the original map and inserting
    // into an empty map by using field handles from the new struct layout.

    VNFuncApp funcApp;
    if ((valueVN == vnStore->ZeroMapVN()) || (vnStore->GetVNFunc(valueVN, &funcApp) == VNF_MemOpaque))
    {
        return valueVN;
    }

    unsigned fromFieldCount = vm->getClassNumInstanceFields(fromClassHandle);
    unsigned toFieldCount   = vm->getClassNumInstanceFields(toClassHandle);

    if (fromFieldCount < toFieldCount)
    {
        return NoVN;
    }

    ValueNum castValueVN          = vnStore->ZeroMapVN();
    bool     allFieldHandlesEqual = true;

    for (unsigned i = 0; i < toFieldCount; i++)
    {
        CORINFO_FIELD_HANDLE fromFieldHandle = vm->getFieldInClass(fromClassHandle, i);
        CORINFO_FIELD_HANDLE toFieldHandle   = vm->getFieldInClass(toClassHandle, i);

        if (fromFieldHandle != toFieldHandle)
        {
            allFieldHandlesEqual = false;
        }

        unsigned fromOffset = vm->getFieldOffset(fromFieldHandle);
        unsigned toOffset   = vm->getFieldOffset(toFieldHandle);

        if (fromOffset != toOffset)
        {
            return NoVN;
        }

        CORINFO_CLASS_HANDLE fromFieldClassHandle;
        var_types            fromFieldType = CorTypeToVarType(vm->getFieldType(fromFieldHandle, &fromFieldClassHandle));
        CORINFO_CLASS_HANDLE toFieldClassHandle;
        var_types            toFieldType = CorTypeToVarType(vm->getFieldType(toFieldHandle, &toFieldClassHandle));

        if (fromFieldType != toFieldType)
        {
            return NoVN;
        }

        ValueNum fromFieldVN = vnStore->VNForFieldSeqHandle(fromFieldHandle);
        ValueNum toFieldVN   = vnStore->VNForFieldSeqHandle(toFieldHandle);

        ValueNum fieldValueVN = vnStore->VNForMapSelect(vnk, fromFieldType, valueVN, fromFieldVN);

        if ((fromFieldType == TYP_STRUCT) && (fromFieldClassHandle != toFieldClassHandle))
        {
            fieldValueVN = CastStruct(vnk, fieldValueVN, fromFieldClassHandle, toFieldClassHandle);

            if (fieldValueVN == NoVN)
            {
                return NoVN;
            }
        }

        castValueVN = vnStore->VNForMapStore(TYP_STRUCT, castValueVN, toFieldVN, fieldValueVN);
    }

    return allFieldHandlesEqual ? valueVN : castValueVN;
}

ValueNum ValueNumbering::CastStruct(ValueNumKind vnk, ValueNum valueVN, ClassLayout* fromLayout, ClassLayout* toLayout)
{
    valueVN = CastStruct(vnk, valueVN, fromLayout->GetClassHandle(), toLayout->GetClassHandle());

    if (valueVN == NoVN)
    {
        valueVN = vnStore->VNForExpr(TYP_STRUCT);
    }

    return valueVN;
}

ValueNumPair ValueNumbering::CastStruct(ValueNumPair valueVNP, ClassLayout* fromLayout, ClassLayout* toLayout)
{
    return {CastStruct(VNK_Liberal, valueVNP.GetLiberal(), fromLayout, toLayout),
            CastStruct(VNK_Conservative, valueVNP.GetConservative(), fromLayout, toLayout)};
}

ValueNum ValueNumbering::CoerceStoreValue(
    GenTree* store, GenTree* value, ValueNumKind vnk, var_types fieldType, ClassLayout* fieldLayout)
{
    ValueNum  valueVN   = vnStore->ExtractValue(value->GetVN(vnk));
    var_types valueType = vnStore->TypeOfVN(valueVN);

    unsigned  fieldSize = fieldType == TYP_STRUCT ? fieldLayout->GetSize() : varTypeSize(fieldType);
    var_types storeType;

    if (GenTreeInsert* insert = store->IsInsert())
    {
        storeType = insert->GetField().GetType();
    }
    else
    {
        storeType = store->GetType();
    }

    unsigned storeSize = varTypeSize(storeType);

    if (storeType == TYP_STRUCT)
    {
        ClassLayout* storeLayout;

        if (GenTreeLclFld* lclFld = store->IsLclFld())
        {
            storeLayout = lclFld->GetLayout(compiler);
        }
        else if (GenTreeInsert* insert = store->IsInsert())
        {
            storeLayout = compiler->typGetLayoutByNum(insert->GetField().GetTypeNum());
        }
        else
        {
            storeLayout = store->AsBlk()->GetLayout();
        }

        storeSize = storeLayout->GetSize();

        if (storeSize == fieldSize)
        {
            if (value->OperIs(GT_CNS_INT))
            {
                assert(value->IsIntegralConst(0));
                return vnStore->VNZeroForType(fieldType);
            }

            if (value->TypeIs(TYP_STRUCT))
            {
                ClassLayout* valueLayout = compiler->typGetStructLayout(value);

                if (valueLayout != fieldLayout)
                {
                    valueVN = CastStruct(VNK_Liberal, valueVN, valueLayout, fieldLayout);
                }
            }
            else
            {
                assert(value->OperIs(GT_INIT_VAL));
                valueVN = vnStore->VNForExpr(TYP_STRUCT);
            }
        }
    }

    if (storeSize > fieldSize)
    {
        // This store is wider than the field so it may modify other fields, leave it to the
        // caller to handle as the outcome depends on context (e.g. wider store to a static
        // field can still be treated as modifying only that static field, it could modify
        // other static fields but that's undefined behaviour and can be ignored).
        return NoVN;
    }

    if (storeSize < fieldSize)
    {
        // This store modifies only a part of the field, just store a new, unique value for now.
        // We could probably merge with the existing value but that's probably not worthwhile now.
        return vnStore->VNForExpr(fieldType);
    }

    // TODO-MIKE-Cleanup: This special casing is inherited from old code, it's probably not
    // necessary if the issue described below is properly fixed.

    if (vnStore->IsVNConstant(valueVN) && (valueType == varActualType(fieldType)))
    {
        if (varTypeIsSmall(fieldType))
        {
            valueVN = vnStore->VNForCast(valueVN, fieldType);
        }

        return valueVN;
    }

    // TODO-MIKE-CQ: Value numbering of small int load/stores is messy and may cause CQ issues.
    // Stores do not truncate the stored values, loads do not widen the loaded value to INT.
    // Truncation might not be necessary but the lack of explicit load widening (e.g. VNF_Cast)
    // is problematic as it may produce different value numbers for the same value. If we store
    // an INT value into a SHORT field and then load a SHORT the resulting value number should
    // be the same as the value number of a CAST from INT to SHORT. But the type of CAST is INT
    // so according to this check it doesn't fit into the field and we get a unique VN. The old
    // code did generate a VNF_Cast here but surprise, VNForCast doesn't attempt to eliminate
    // redundant casts and we end up with:
    //     CAST(x) = VNF_Cast<INT, SHORT>(x)
    //     LOAD<SHORT>(STORE<SHORT>(x) = VNF_Cast<INT, SHORT>(VNF_Cast<INT, SHORT>(x))
    // which is pretty much pointless, pointless enough that generating a unique number in this
    // case results in no diffs.
    // What probably needs to be done is:
    //   - allow storing INT values to small int fields here
    //   - widen loads from small int fields (add VNF_Cast in CoerceLoadValue when the field
    //     type is small int)
    //   - ensure that VNForCast deals with redundant casts
    // It's not clear if storing to small int fields should truncate. If loads are guaranteed
    // to perform widening it seems unnecessary to do anything when storing. Besides, it's not
    // even clear what truncation means anyway - VNF_Cast to field's small int type? That's
    // not quite right as that really produces an INT value, the JIT doesn't have a notion of
    // small int values. Also, there's a pretty good chance to have stores without subsequent
    // loads in the same method so truncation will just allocate a useless value number. So
    // probably no truncation it is.
    // All this may also allow elimination of small int typed value numbers. They just cause
    // confusion and waste memory due to how value number "chunks" are managed.

    if (valueType != fieldType)
    {
        if ((valueType == TYP_LONG) && (fieldType == TYP_INT))
        {
            valueVN = vnStore->VNForCast(valueVN, TYP_INT);
        }
        else if ((varTypeSize(valueType) == varTypeSize(fieldType)) && !varTypeIsSmall(valueType))
        {
            valueVN = vnStore->VNForBitCast(valueVN, fieldType);
        }
        else
        {
            valueVN = vnStore->VNForExpr(fieldType);
        }

        INDEBUG(vnStore->Trace(valueVN));
    }

    return valueVN;
}

ValueNum ValueNumbering::CoerceLoadValue(GenTree* load, ValueNum valueVN, var_types fieldType, ClassLayout* fieldLayout)
{
    assert(valueVN == vnStore->ExtractValue(valueVN));
    assert(varActualType(fieldType) == varActualType(vnStore->TypeOfVN(valueVN)));

    var_types loadType = load->GetType();

    if (loadType == TYP_STRUCT)
    {
        ClassLayout* loadLayout;

        if (GenTreeLclFld* lclFld = load->IsLclFld())
        {
            loadLayout = lclFld->GetLayout(compiler);
        }
        else if (GenTreeExtract* extract = load->IsExtract())
        {
            loadLayout = compiler->typGetLayoutByNum(extract->GetField().GetLayoutNum());
        }
        else
        {
            loadLayout = load->AsBlk()->GetLayout();
        }

        if ((fieldType == TYP_STRUCT) && (loadLayout == fieldLayout))
        {
            return valueVN;
        }

        // TODO-MIKE-Fix: Using VNForExpr with the current block here and below is highly suspect.
        // These are loads, one way or another they use memory that may have been defined
        // in a block different than the current one.
        // Also, as long as the load size is less or equal the field size the resulting
        // value depends solely on the loaded value and there's no need to use MemOpaque.
        return vnStore->VNForExpr(TYP_STRUCT);
    }

    if (loadType == fieldType)
    {
        return valueVN;
    }

    if (varTypeSize(loadType) <= varTypeSize(fieldType))
    {
        if (varTypeIsIntegral(loadType) && varTypeIsIntegral(fieldType))
        {
            return vnStore->VNForCast(valueVN, loadType);
        }
    }

    // TODO-MIKE-CQ: Check what other cases might be worth handling here (e.g. float/int,
    // long/double mismatches -> VNF_BitCast).

    return vnStore->VNForExpr(varActualType(loadType));
}

var_types ValueNumbering::GetFieldType(CORINFO_FIELD_HANDLE fieldHandle, ClassLayout** fieldLayout)
{
    CORINFO_CLASS_HANDLE typeHandle = NO_CLASS_HANDLE;
    var_types            type       = CorTypeToVarType(vm->getFieldType(fieldHandle, &typeHandle));

    if (type == TYP_STRUCT)
    {
        *fieldLayout = compiler->typGetObjLayout(typeHandle);
        type         = compiler->typGetStructType(*fieldLayout);
    }
    else
    {
        *fieldLayout = nullptr;
    }

    return type;
}

ValueNum ValueNumbering::InsertStructField(
    GenTree* store, GenTree* value, ValueNumKind vnk, ValueNum structVN, var_types structType, FieldSeqNode* fieldSeq)
{
    assert(varTypeIsStruct(structType));

    struct
    {
        ClassLayout* layout;
        ValueNum     fieldVN;
        ValueNum     valueVN;
        var_types    type;
    } fields[1 + FieldSeqNode::MaxLength];

    fields[0].valueVN = structVN;
    fields[0].type    = structType;

    unsigned count = 1;

    for (; fieldSeq != nullptr; fieldSeq = fieldSeq->GetNext(), count++)
    {
        assert(count < _countof(fields));

        CORINFO_FIELD_HANDLE fieldHandle = fieldSeq->GetFieldHandle();

        fields[count].fieldVN = vnStore->VNForFieldSeqHandle(fieldHandle);
        fields[count].type    = GetFieldType(fieldHandle, &fields[count].layout);
    }

    auto&    last    = fields[count - 1];
    ValueNum valueVN = CoerceStoreValue(store, value, vnk, last.type, last.layout);

    if (valueVN == NoVN)
    {
        return NoVN;
    }

    for (unsigned i = 1; i < count - 1; i++)
    {
        auto& current   = fields[i];
        auto& prev      = fields[i - 1];
        current.valueVN = vnStore->VNForMapSelect(vnk, current.type, prev.valueVN, current.fieldVN);
    }

    for (unsigned i = count - 1; i > 0; i--)
    {
        auto& current = fields[i];
        auto& prev    = fields[i - 1];
        valueVN       = vnStore->VNForMapStore(prev.type, prev.valueVN, current.fieldVN, valueVN);
    }

    return valueVN;
}

ValueNum ValueNumbering::ExtractStructField(GenTree* load, ValueNumKind vnk, ValueNum structVN, FieldSeqNode* fieldSeq)
{
    var_types    fieldType   = TYP_UNDEF;
    ClassLayout* fieldLayout = nullptr;

    for (; fieldSeq != nullptr; fieldSeq = fieldSeq->GetNext())
    {
        noway_assert(fieldSeq->IsField());

        fieldType = GetFieldType(fieldSeq->GetFieldHandle(), &fieldLayout);

        ValueNum fieldVN = vnStore->VNForFieldSeqHandle(fieldSeq->GetFieldHandle());

        structVN = vnStore->VNForMapSelect(vnk, fieldType, structVN, fieldVN);
    }

    return CoerceLoadValue(load, structVN, fieldType, fieldLayout);
}

ValueNumPair ValueNumbering::ExtractStructField(GenTree* load, ValueNumPair structVNP, FieldSeqNode* fieldSeq)
{
    var_types    fieldType   = TYP_UNDEF;
    ClassLayout* fieldLayout = nullptr;

    for (; fieldSeq != nullptr; fieldSeq = fieldSeq->GetNext())
    {
        noway_assert(fieldSeq->IsField());

        fieldType = GetFieldType(fieldSeq->GetFieldHandle(), &fieldLayout);

        ValueNum fieldVN = vnStore->VNForFieldSeqHandle(fieldSeq->GetFieldHandle());

        structVNP.SetLiberal(vnStore->VNForMapSelect(VNK_Liberal, fieldType, structVNP.GetLiberal(), fieldVN));
        structVNP.SetConservative(
            vnStore->VNForMapSelect(VNK_Conservative, fieldType, structVNP.GetConservative(), fieldVN));
    }

    structVNP.SetLiberal(CoerceLoadValue(load, structVNP.GetLiberal(), fieldType, fieldLayout));
    structVNP.SetConservative(CoerceLoadValue(load, structVNP.GetConservative(), fieldType, fieldLayout));

    return structVNP;
}

void ValueNumbering::SummarizeLoopLocalMemoryStores(GenTreeLclVarCommon* store, VNLoopMemorySummary& summary)
{
    if (lvaGetDesc(store)->IsAddressExposed())
    {
        summary.AddAddressExposedLocal(store->GetLclNum());
    }
}

void ValueNumbering::SummarizeLoopLocalDefs(GenTreeLclDef* def, VNLoopMemorySummary& summary)
{
    if (def->TypeIs(TYP_BYREF))
    {
        def->SetLiberalVN(def->GetValue()->GetLiberalVN());
    }
}

void ValueNumbering::NumberLclStore(GenTreeLclVar* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR) && ((store->gtFlags & GTF_VAR_DEF) != 0));
    assert(!lvaGetDesc(store)->IsSsa());

    if (!lvaGetDesc(store)->IsAddressExposed())
    {
        assert(ssa.GetMemoryDef(store) == nullptr);

        return;
    }

    ValueNum lclAddrVN = vnStore->VNForFunc(TYP_I_IMPL, VNF_LclAddr, vnStore->VNForIntCon(store->GetLclNum()),
                                            vnStore->VNZeroForType(TYP_I_IMPL), vnStore->VNForFieldSeq(nullptr));
    INDEBUG(vnStore->Trace(lclAddrVN));

    ValueNum memVN = StoreAddressExposedLocal(store, lclAddrVN, store->GetOp(0));
    UpdateMemory(store, memVN DEBUGARG("address-exposed local store"));
}

void ValueNumbering::NumberLclDef(GenTreeLclDef* def)
{
    LclVarDsc*   lcl   = lvaGetDesc(def->GetLclNum());
    GenTree*     value = def->GetValue();
    ValueNumPair valueVNP;

    if (def->TypeIs(TYP_STRUCT))
    {
        assert(lcl->TypeIs(TYP_STRUCT));

        if (value->TypeIs(TYP_STRUCT))
        {
            valueVNP = vnStore->ExtractValue(value->GetVNP());

            // TODO-MIKE-Fix: INSERT needs layout, the struct value may be a constant 0
            // for initialization and then we don't have layout. Well, 0 could probably
            // be replaced by a dedicated struct init code that has layout but it's
            // probably better for INSERT itself to have layout.
            // This can be ignored for now, we simply assume that the INSERT layout is
            // the same as the local layout, since INSERTs are currently built only from
            // LCL_FLD stores.
            if (!value->IsInsert())
            {
                ClassLayout* valueLayout = compiler->typGetStructLayout(value);

                if (valueLayout != lcl->GetLayout())
                {
                    valueVNP = CastStruct(valueVNP, valueLayout, lcl->GetLayout());
                }
            }
        }
        else if (value->OperIs(GT_CNS_INT))
        {
            assert(value->IsIntegralConst(0));
            valueVNP.SetBoth(vnStore->ZeroMapVN());
        }
        else
        {
            assert(value->OperIs(GT_INIT_VAL));
            valueVNP.SetBoth(vnStore->VNForExpr(TYP_STRUCT));
        }
    }
    else
    {
        valueVNP = vnStore->ExtractValue(value->GetVNP());

        var_types valueType = value->GetType();
        var_types defType   = def->GetType();

        if (valueType != defType)
        {
            if ((varTypeSize(valueType) == varTypeSize(defType)) && !varTypeIsSmall(valueType))
            {
                valueVNP.SetLiberal(vnStore->VNForBitCast(valueVNP.GetLiberal(), def->GetType()));
                valueVNP.SetConservative(vnStore->VNForBitCast(valueVNP.GetConservative(), def->GetType()));
            }
            else
            {
                // TODO-MIKE-CQ: This inserts superfluous casts for all sort of small int/INT mismatches,
                // these ultimately impact CSE because they make the stored value appear to be different
                // from the original value. This happens easily when storing a value loaded from a small
                // int field into an int or small int local - the load already widened the small int and
                // produced an INT value, now we're widening it again and given that VNForCast doesn't
                // attempt to remove redundant casts we end up with a new value number, different from
                // the one produced by the load.
                // This is also dubious in case the IR contains other type mismatches, possibly involving
                // SIMD types, SIMD12 and SIMD16 in particular.
                valueVNP = vnStore->VNForCast(valueVNP, defType);
            }
        }
    }

    def->SetVNP(valueVNP);

    INDEBUG(TraceLocal(def->GetLclNum(), valueVNP));
}

void ValueNumbering::NumberLclLoad(GenTreeLclVar* load)
{
    assert(load->OperIs(GT_LCL_VAR) && ((load->gtFlags & GTF_VAR_DEF) == 0));
    assert(!lvaGetDesc(load)->IsSsa());

    if (!lvaGetDesc(load)->IsAddressExposed())
    {
        load->SetVNP(ValueNumPair{vnStore->VNForExpr(load->GetType())});

        return;
    }

    ValueNum addrVN = vnStore->VNForFunc(TYP_I_IMPL, VNF_LclAddr, vnStore->VNForIntCon(load->GetLclNum()),
                                         vnStore->VNZeroForType(TYP_I_IMPL), vnStore->VNForFieldSeq(nullptr));

    // TODO-MIKE-Review: Setting the conservative VN to a non unique VN is suspect.
    // Well, the chance that an address-exposed local is modified by another thread
    // is slim so perhaps this is fine as it is but then NumberIndirStore should do the
    // same for local addresses.
    load->SetVNP(ValueNumPair{LoadMemory(load->GetType(), addrVN)});
}

void ValueNumbering::NumberLclUse(GenTreeLclUse* use)
{
    GenTreeLclDef* def = use->GetDef();
    LclVarDsc*     lcl = lvaGetDesc(def->GetLclNum());
    ValueNumPair   vnp = def->GetVNP();

    assert(vnp.GetLiberal() != NoVN);

    unsigned valSize = varTypeSize(varActualType(use->GetType()));
    unsigned lclSize = varTypeSize(varActualType(lcl->GetType()));

    if (valSize != lclSize)
    {
        // Expected type mismatch case is LONG local loaded as INT, ignore everything else.
        if (use->TypeIs(TYP_INT) && lcl->TypeIs(TYP_LONG))
        {
            vnp = vnStore->VNForCast(vnp, TYP_INT);
        }
        else
        {
            printf("bad type %s %s\n", varTypeName(use->GetType()), varTypeName(lcl->GetType()));
            vnp.SetBoth(vnStore->VNForExpr(use->GetType()));
        }
    }

    // A BYREF local may have a zero offset field sequence that needs to be added.
    if (use->TypeIs(TYP_BYREF))
    {
        if (FieldSeqNode* fieldSeq = compiler->GetZeroOffsetFieldSeq(use))
        {
            ValueNum extendVN = vnStore->ExtendPtrVN(vnp.GetLiberal(), fieldSeq, 0);

            if (extendVN != NoVN)
            {
                // TODO-MIKE-Fix: This doesn't make a lot of sense. We only look at the liberal VN,
                // the conservative VN might be different (e.g. the value stored in the local could
                // have been the result of a load from memory).
                vnp.SetBoth(extendVN);
            }
        }
    }

    use->SetVNP(vnp);
}

void ValueNumbering::NumberLclFldStore(GenTreeLclFld* store)
{
    assert(store->OperIs(GT_STORE_LCL_FLD) && ((store->gtFlags & GTF_VAR_DEF) != 0));
    assert(((store->gtFlags & GTF_VAR_USEASG) != 0) == store->IsPartialLclFld(compiler));
    assert(!lvaGetDesc(store)->IsSsa());

    if (!lvaGetDesc(store)->IsAddressExposed())
    {
        assert(ssa.GetMemoryDef(store) == nullptr);

        return;
    }

    ValueNum lclAddrVN = vnStore->VNForFunc(TYP_I_IMPL, VNF_LclAddr, vnStore->VNForIntCon(store->GetLclNum()),
                                            vnStore->VNForUPtrSizeIntCon(store->GetLclOffs()),
                                            vnStore->VNForFieldSeq(store->GetFieldSeq()));
    INDEBUG(vnStore->Trace(lclAddrVN));

    ValueNum memVN = StoreAddressExposedLocal(store, lclAddrVN, store->GetOp(0));
    UpdateMemory(store, memVN DEBUGARG("address-exposed local store"));
}

void ValueNumbering::NumberInsert(GenTreeInsert* insert)
{
    const FieldInfo& field = insert->GetField();
    ValueNumPair     valueVNP;

    if (!field.HasFieldSeq())
    {
        valueVNP.SetBoth(vnStore->VNForExpr(varActualType(insert->GetType())));
    }
    else
    {
        GenTree*     structValue = insert->GetStructValue();
        GenTree*     fieldValue  = insert->GetFieldValue();
        ValueNumPair currentVNP;

        if (structValue->IsIntegralConst(0))
        {
            currentVNP.SetBoth(vnStore->ZeroMapVN());
        }
        else
        {
            currentVNP = structValue->GetVNP();
        }

        FieldSeqNode* fieldSeq = field.GetFieldSeq();
        var_types     lclType  = insert->GetType();

        valueVNP.SetLiberal(
            InsertStructField(insert, fieldValue, VNK_Liberal, currentVNP.GetLiberal(), lclType, fieldSeq));

        if (valueVNP.GetLiberal() == NoVN)
        {
            valueVNP.SetLiberal(vnStore->VNForExpr(varActualType(insert->GetType())));
        }

        valueVNP.SetConservative(
            InsertStructField(insert, fieldValue, VNK_Conservative, currentVNP.GetConservative(), lclType, fieldSeq));

        if (valueVNP.GetConservative() == NoVN)
        {
            valueVNP.SetConservative(vnStore->VNForExpr(varActualType(insert->GetType())));
        }
    }

    insert->SetVNP(valueVNP);
}

void ValueNumbering::NumberLclFldLoad(GenTreeLclFld* load)
{
    assert(load->OperIs(GT_LCL_FLD) && ((load->gtFlags & GTF_VAR_DEF) == 0));
    assert(!lvaGetDesc(load)->IsSsa());

    if (!lvaGetDesc(load)->IsAddressExposed())
    {
        load->SetVNP(ValueNumPair{vnStore->VNForExpr(load->GetType())});

        return;
    }

    // Note that the field sequence is currently ignored because we don't try to resolve
    // these loads back to a store. We only care about getting the same VN when loading
    // the same type from the same address (provided that there are no interfering stores).
    ValueNum addrVN =
        vnStore->VNForFunc(TYP_I_IMPL, VNF_LclAddr, vnStore->VNForIntCon(load->GetLclNum()),
                           vnStore->VNForUPtrSizeIntCon(load->GetLclOffs()), vnStore->VNForFieldSeq(nullptr));

    load->SetLiberalVN(LoadMemory(load->GetType(), addrVN));
    load->SetConservativeVN(vnStore->VNForExpr(load->GetType()));
}

void ValueNumbering::NumberExtract(GenTreeExtract* extract)
{
    const FieldInfo& field = extract->GetField();

    if (!field.HasFieldSeq())
    {
        extract->SetVNP(ValueNumPair{vnStore->VNForExpr(extract->GetType())});

        return;
    }

    GenTree* structValue = extract->GetStructValue();
    assert(varTypeIsStruct(structValue->GetType()));

    ValueNumPair vnp = structValue->GetVNP();
    vnp              = ExtractStructField(extract, vnp, field.GetFieldSeq());
    extract->SetVNP(vnp);
}

void ValueNumbering::SummarizeLoopIndirMemoryStores(GenTreeIndir* store, VNLoopMemorySummary& summary)
{
    assert(store->OperIs(GT_STOREIND, GT_STORE_OBJ, GT_STORE_BLK));

    if (store->IsVolatile())
    {
        summary.AddMemoryHavoc();
        return;
    }

    GenTree*  addr   = store->GetAddr();
    ValueNum  addrVN = addr->GetLiberalVN();
    VNFuncApp funcApp;
    VNFunc    func = vnStore->GetVNFunc(addrVN, &funcApp);

    if (func == VNF_PtrToStatic)
    {
        FieldSeqNode* fieldSeq = vnStore->FieldSeqVNToFieldSeq(funcApp[0]);
        summary.AddField(fieldSeq->GetFieldHandle());

        return;
    }

    if (func == VNF_PtrToArrElem)
    {
        unsigned elemTypeNum = static_cast<unsigned>(vnStore->ConstantValue<int32_t>(funcApp[0]));
        summary.AddArrayType(elemTypeNum);

        return;
    }

    if (func == VNF_LclAddr)
    {
        unsigned lclNum = static_cast<unsigned>(vnStore->ConstantValue<int32_t>(funcApp[0]));
        summary.AddAddressExposedLocal(lclNum);

        return;
    }

    GenTree* obj;
    if (FieldSeqNode* fieldSeq = IsFieldAddr(addr, &obj))
    {
        if (obj == nullptr)
        {
            summary.AddField(fieldSeq->GetFieldHandle());
        }
        else
        {
            SummarizeLoopObjFieldMemoryStores(store, fieldSeq, summary);
        }

        return;
    }

    summary.AddMemoryHavoc();
}

void ValueNumbering::NumberIndirStore(GenTreeIndir* store)
{
    assert(store->OperIs(GT_STOREIND, GT_STORE_OBJ, GT_STORE_BLK));

    // TODO-MIKE-Fix: This is missing operand exceptions...
    store->SetVNP(ValueNumStore::VoidVNP());

    if (store->IsVolatile())
    {
        // For volatile stores, first mutate memory. This prevents previous
        // stores from being visible after the store.
        ClearMemory(store DEBUGARG("volatile store"));
    }

    GenTree*  value  = store->GetValue();
    GenTree*  addr   = store->GetAddr();
    ValueNum  addrVN = vnStore->ExtractValue(addr->GetLiberalVN());
    VNFuncApp funcApp;
    VNFunc    func = vnStore->GetVNFunc(addrVN, &funcApp);

    if (func == VNF_PtrToStatic)
    {
        ValueNum memVN = StoreStaticField(store, vnStore->FieldSeqVNToFieldSeq(funcApp[0]), value);
        UpdateMemory(store, memVN DEBUGARG("static field store"));

        return;
    }

    if (func == VNF_PtrToArrElem)
    {
        ValueNum memVN = StoreArrayElem(store, funcApp, value);
        UpdateMemory(store, memVN DEBUGARG("array element store"));

        return;
    }

    if (func == VNF_LclAddr)
    {
        assert(lvaGetDesc(vnStore->ConstantValue<int32_t>(funcApp[0]))->IsAddressExposed());
        ValueNum memVN = StoreAddressExposedLocal(store, addrVN, value);
        UpdateMemory(store, memVN DEBUGARG("address-exposed local store"));

        return;
    }

    GenTree* obj;
    if (FieldSeqNode* fieldSeq = IsFieldAddr(addr, &obj))
    {
        ValueNum memVN;

        if (obj == nullptr)
        {
            memVN = StoreStaticField(store, fieldSeq, value);
        }
        else
        {
            memVN = StoreObjField(store, vnStore->ExtractValue(obj->GetLiberalVN()), fieldSeq, value);
        }

        UpdateMemory(store, memVN DEBUGARG(obj == nullptr ? "static field store" : "object field store"));

        return;
    }

    ClearMemory(store DEBUGARG("indirect store"));
}

void ValueNumbering::NumberIndirLoad(GenTreeIndir* load)
{
    assert(load->OperIs(GT_IND, GT_OBJ, GT_BLK));
    assert((load->gtFlags & GTF_IND_ASG_LHS) == 0);

    GenTree*     addr = load->GetAddr();
    ValueNumPair addrExcVNP;
    ValueNumPair addrVNP = vnStore->UnpackExset(addr->GetVNP(), &addrExcVNP);

    if (addr->TypeIs(TYP_REF) && load->TypeIs(TYP_I_IMPL))
    {
        assert(load->OperIs(GT_IND) && !load->IsVolatile());

        ValueNumPair vnp;
        VNFuncApp    funcApp;

        if (addrVNP.BothEqual() && (vnStore->GetVNFunc(addrVNP.GetLiberal(), &funcApp) == VNF_JitNew))
        {
            vnp.SetBoth(funcApp[0]);
        }
        else
        {
            vnp = vnStore->VNPairForFunc(TYP_I_IMPL, VNF_ObjMT, addrVNP);
        }

        load->SetVNP(vnStore->PackExset(vnp, addrExcVNP));

        return;
    }

    if (load->IsInvariant())
    {
        assert(!load->IsVolatile());

        ValueNumPair vnp;

        if ((load->gtFlags & GTF_IND_NONNULL) != 0)
        {
            vnp = vnStore->VNPairForFunc(load->GetType(), VNF_NonNullIndirect, addrVNP);
        }
        else
        {
            vnp.SetBoth(vnStore->ReadOnlyMemoryMapVN());
            vnp = vnStore->VNForMapSelect(load->GetType(), vnp, addrVNP);
        }

        load->SetVNP(vnStore->PackExset(vnp, addrExcVNP));

        return;
    }

    // The conservative VN of a load is always a new, unique VN.
    ValueNum  conservativeVN = vnStore->VNForExpr(load->GetType());
    ValueNum  valueVN;
    VNFuncApp funcApp;
    GenTree*  obj;

    if (load->IsVolatile())
    {
        // We just mutate memory for volatile loads, and then do the load as normal.
        //
        // This allows:
        //   1: read s;
        //   2: volatile read s;
        //   3: read s;
        //
        // We should never assume that the values loaded by 1 and 2 are the same (because memory was
        // mutated in between them) but we *should* be able to prove that the values loaded by 2 and
        // 3 are the same.

        ClearMemory(load DEBUGARG("volatile load"));

        valueVN = conservativeVN;
    }
    else if (vnStore->GetVNFunc(addrVNP.GetLiberal(), &funcApp) == VNF_PtrToStatic)
    {
        // TODO-MIKE-CQ: Static fields are a mess. The address is sometimes CLS_VAR_ADDR,
        // sometimes CNS_INT. The later generates a VNHandle instead of VNF_PtrToStatic
        // and the handle can be recognized as being a static address but it lacks the
        // field handle/sequence so we can't do much with it. Ideally CNS_INT would also
        // generate VNF_PtrToStatic but then CSE barfs because it expects constant VNs
        // for constant nodes and VNF_PtrToStatic isn't a constant.
        // In the case of STRUCT static fields, CLS_VAR_ADDR is rare, the C# compiler
        // seems to prefer LDSFLDA-LDFLDA-LDFLD to LDSFLD-LDFLD-LDFLD and the importer
        // always uses CNS_INT for LDSFLDA. Not good for testing. Moreover, VN doesn't
        // seem to recognize CNS_INT on its own, it only recognizes it together with a
        // subsequent STRUCT field access, which does not involve VNF_PtrToStatic.
        // This is somewhat risky because no matter what the IR pattern is we should end
        // up using the same field sequence in all cases, otherwise we may end up with
        // loads not correctly seeing previously stored values.

        valueVN = LoadStaticField(load, vnStore->FieldSeqVNToFieldSeq(funcApp[0]));
    }
    else if (vnStore->GetVNFunc(addrVNP.GetLiberal(), &funcApp) == VNF_PtrToArrElem)
    {
        valueVN = LoadArrayElem(load, funcApp);

        // TODO-CQ: what to do here about exceptions? We don't have the array and index conservative
        // values, so we don't have their exceptions. Maybe we should.
        // TODO-MIKE-Fix: Actually we do have the liberal array and index and that's pretty much all
        // that matters for exceptions. But then this is only relevant if range checks are disabled...
    }
    else if (FieldSeqNode* fieldSeq = IsFieldAddr(addr, &obj))
    {
        if (obj == nullptr)
        {
            valueVN = LoadStaticField(load, fieldSeq);
        }
        else
        {
            valueVN = LoadObjField(load, vnStore->ExtractValue(obj->GetLiberalVN()), fieldSeq);
        }
    }
    else
    {
        valueVN = LoadMemory(load->GetType(), addrVNP.GetLiberal());
    }

    load->SetVNP(vnStore->PackExset({valueVN, conservativeVN}, addrExcVNP));
}

ValueNum ValueNumbering::StoreStaticField(GenTreeIndir* store, FieldSeqNode* fieldSeq, GenTree* value)
{
    // TODO-MIKE-CQ: Currently struct stores are not handled.
    if (store->TypeIs(TYP_STRUCT))
    {
        return vnStore->VNForExpr(TYP_STRUCT);
    }

    CORINFO_FIELD_HANDLE fieldHandle = fieldSeq->GetFieldHandle();
    assert(vm->isFieldStatic(fieldHandle));

    fieldSeq = fieldSeq->GetNext();

    if (fieldSeq != nullptr)
    {
        assert(fieldSeq->IsBoxedValueField());
        fieldSeq = fieldSeq->GetNext();
    }

    ClassLayout* fieldLayout;
    var_types    fieldType = GetFieldType(fieldHandle, &fieldLayout);
    ValueNum     fieldVN   = vnStore->VNForFieldSeqHandle(fieldHandle);

    ValueNum memVN = fgCurMemoryVN;
    INDEBUG(TraceMem(memVN));

    ValueNum valueVN;

    if (fieldSeq == nullptr)
    {
        valueVN = CoerceStoreValue(store, value, VNK_Liberal, fieldType, fieldLayout);
    }
    else
    {
        valueVN = vnStore->VNForMapSelect(VNK_Liberal, TYP_STRUCT, memVN, fieldVN);
        valueVN = InsertStructField(store, value, VNK_Liberal, valueVN, fieldType, fieldSeq);
    }

    if (valueVN == NoVN)
    {
        // If the store is wider than the field just store a new, unique VN in the field.
        // The store might modify other static fields but that's undefined behaviour.
        valueVN = vnStore->VNForExpr(fieldType);
    }

    return vnStore->VNForMapStore(TYP_STRUCT, memVN, fieldVN, valueVN);
}

ValueNum ValueNumbering::LoadStaticField(GenTreeIndir* load, FieldSeqNode* fieldSeq)
{
    CORINFO_FIELD_HANDLE fieldHandle = fieldSeq->GetFieldHandle();
    assert(vm->isFieldStatic(fieldHandle));
    ClassLayout* fieldLayout;
    var_types    fieldType = GetFieldType(fieldHandle, &fieldLayout);
    ValueNum     fieldVN   = vnStore->VNForFieldSeqHandle(fieldHandle);

    fieldSeq = fieldSeq->GetNext();

    if ((fieldSeq == nullptr) && load->TypeIs(TYP_REF) && varTypeIsStruct(fieldType))
    {
        // This actually loads a boxed object reference for a static struct field.
        // Note that this must come from the special read only heap. Not only that
        // the reference is indeed read only (even if the field itself isn't read
        // only) but we're using MapSelect(Memory, field) to load the boxed struct
        // contents, we cannot use the same value to load the reference.

        // TODO-MIKE-CQ: Enable this. It produces good improvements thanks to CSE
        // but there are also significatn regressions, apparently due to the lack
        // of OBJ address mode marking.
        //
        // return vnStore->VNForMapSelect(VNK_Liberal, TYP_REF, vnStore->ReadOnlyMemoryMapVN(), fieldVN);

        return vnStore->VNForExpr(TYP_REF);
    }

    if (fieldSeq != nullptr)
    {
        assert(fieldSeq->IsBoxedValueField());
        fieldSeq = fieldSeq->GetNext();
    }

    ValueNum memVN = fgCurMemoryVN;
    INDEBUG(TraceMem(memVN));

    ValueNum vn = vnStore->VNForMapSelect(VNK_Liberal, fieldType, memVN, fieldVN);

    if (fieldSeq != nullptr)
    {
        return ExtractStructField(load, VNK_Liberal, vn, fieldSeq);
    }
    else
    {
        return CoerceLoadValue(load, vn, fieldType, fieldLayout);
    }
}

void ValueNumbering::SummarizeLoopObjFieldMemoryStores(GenTreeIndir*        store,
                                                       FieldSeqNode*        fieldSeq,
                                                       VNLoopMemorySummary& summary)
{
    ClassLayout* fieldLayout;
    var_types    fieldType = GetFieldType(fieldSeq->GetTail()->GetFieldHandle(), &fieldLayout);

    unsigned fieldSize = fieldType == TYP_STRUCT ? fieldLayout->GetSize() : varTypeSize(fieldType);
    unsigned storeSize = store->IsBlk() ? store->AsBlk()->GetLayout()->GetSize() : varTypeSize(store->GetType());

    if (storeSize <= fieldSize)
    {
        summary.AddField(fieldSeq->GetFieldHandle());
    }
    else
    {
        summary.AddMemoryHavoc();
    }
}

ValueNum ValueNumbering::StoreObjField(GenTreeIndir* store, ValueNum objVN, FieldSeqNode* fieldSeq, GenTree* value)
{
    // TODO-MIKE-CQ: Currently struct stores are not handled.
    if (store->TypeIs(TYP_STRUCT))
    {
        return vnStore->VNForExpr(TYP_STRUCT);
    }

    CORINFO_FIELD_HANDLE fieldHandle = fieldSeq->GetFieldHandle();
    assert(!vm->isFieldStatic(fieldHandle));
    fieldSeq = fieldSeq->GetNext();
    ClassLayout* fieldLayout;
    var_types    fieldType = GetFieldType(fieldHandle, &fieldLayout);
    ValueNum     fieldVN   = vnStore->VNForFieldSeqHandle(fieldHandle);

    ValueNum memVN = fgCurMemoryVN;
    INDEBUG(TraceMem(memVN));

    ValueNum fieldMapVN = vnStore->VNForMapSelect(VNK_Liberal, TYP_STRUCT, memVN, fieldVN);
    ValueNum valueVN;

    if (fieldSeq == nullptr)
    {
        valueVN = CoerceStoreValue(store, value, VNK_Liberal, fieldType, fieldLayout);
    }
    else
    {
        valueVN = vnStore->VNForMapSelect(VNK_Liberal, fieldType, fieldMapVN, objVN);
        valueVN = InsertStructField(store, value, VNK_Liberal, valueVN, fieldType, fieldSeq);
    }

    if (valueVN == NoVN)
    {
        // If the store is wider than the field then update the entire memory.
        // TODO-MIKE-CQ: This is overly conservative, in practice such a store can only
        // modify fields of the same object, anything else (static fields, arrays) can
        // be modified but it's undefined behaviour and can be igored. We could probably
        // enumerate this object's fields and store unique values in those that overlap
        // the store.
        return vnStore->VNForExpr(TYP_STRUCT);
    }

    fieldMapVN = vnStore->VNForMapStore(TYP_STRUCT, fieldMapVN, objVN, valueVN);

    return vnStore->VNForMapStore(TYP_STRUCT, memVN, fieldVN, fieldMapVN);
}

ValueNum ValueNumbering::LoadObjField(GenTreeIndir* load, ValueNum objVN, FieldSeqNode* fieldSeq)
{
    CORINFO_FIELD_HANDLE fieldHandle = fieldSeq->GetFieldHandle();
    assert(!vm->isFieldStatic(fieldHandle));
    ValueNum fieldVN = vnStore->VNForFieldSeqHandle(fieldHandle);
    fieldSeq         = fieldSeq->GetNext();
    ClassLayout* fieldLayout;
    var_types    fieldType = GetFieldType(fieldHandle, &fieldLayout);

    ValueNum vn = fgCurMemoryVN;
    INDEBUG(TraceMem(vn));

    vn = vnStore->VNForMapSelect(VNK_Liberal, TYP_STRUCT, vn, fieldVN);
    vn = vnStore->VNForMapSelect(VNK_Liberal, fieldType, vn, objVN);

    if (fieldSeq != nullptr)
    {
        return ExtractStructField(load, VNK_Liberal, vn, fieldSeq);
    }
    else
    {
        return CoerceLoadValue(load, vn, fieldType, fieldLayout);
    }
}

ValueNum ValueNumbering::StoreArrayElem(GenTreeIndir* store, const VNFuncApp& elemAddr, GenTree* value)
{
    assert(elemAddr.m_func == VNF_PtrToArrElem);

    // TODO-MIKE-CQ: Currently struct stores are not handled.
    if (store->TypeIs(TYP_STRUCT))
    {
        return vnStore->VNForExpr(TYP_STRUCT);
    }

    ValueNum      elemTypeVN = elemAddr[0];
    ValueNum      arrayVN    = elemAddr[1];
    ValueNum      indexVN    = elemAddr[2];
    FieldSeqNode* fieldSeq   = vnStore->FieldSeqVNToFieldSeq(elemAddr[3]);

    unsigned     elemTypeNum = static_cast<unsigned>(vnStore->ConstantValue<int32_t>(elemAddr[0]));
    ClassLayout* elemLayout =
        compiler->typIsLayoutNum(elemTypeNum) ? compiler->typGetLayoutByNum(elemTypeNum) : nullptr;
    var_types elemType =
        elemLayout == nullptr ? static_cast<var_types>(elemTypeNum) : compiler->typGetStructType(elemLayout);

    ValueNum memVN = fgCurMemoryVN;
    INDEBUG(TraceMem(memVN));

    // TODO-MIKE-Fix: We should get a field sequence only for arrays of structs.
    // This isn't the best place to check this but for now it gets pmi diff
    // working again.

    if (!varTypeIsStruct(elemType) && (fieldSeq != nullptr))
    {
        fieldSeq = FieldSeqNode::NotAField();
    }

    if (fieldSeq == FieldSeqStore::NotAField())
    {
        return vnStore->VNForMapStore(TYP_STRUCT, memVN, elemTypeVN, vnStore->VNForExpr(TYP_STRUCT));
    }

    ValueNum arrayTypeMapVN = vnStore->VNForMapSelect(VNK_Liberal, TYP_STRUCT, memVN, elemTypeVN);
    ValueNum arrayMapVN     = vnStore->VNForMapSelect(VNK_Liberal, TYP_STRUCT, arrayTypeMapVN, arrayVN);

    ValueNum valueVN;

    if (fieldSeq == nullptr)
    {
        valueVN = CoerceStoreValue(store, value, VNK_Liberal, elemType, elemLayout);
    }
    else
    {
        valueVN = vnStore->VNForMapSelect(VNK_Liberal, elemType, arrayMapVN, indexVN);
        valueVN = InsertStructField(store, value, VNK_Liberal, valueVN, elemType, fieldSeq);
    }

    if (valueVN == NoVN)
    {
        // If the store is wider than the array element then update the entire array, not just the element.
        // Of course, such a store may modify other arrays/objects but that's undefined behaviour.
        arrayTypeMapVN = vnStore->VNForExpr(TYP_STRUCT);
    }
    else
    {
        arrayMapVN     = vnStore->VNForMapStore(TYP_STRUCT, arrayMapVN, indexVN, valueVN);
        arrayTypeMapVN = vnStore->VNForMapStore(TYP_STRUCT, arrayTypeMapVN, arrayVN, arrayMapVN);
    }

    return vnStore->VNForMapStore(TYP_STRUCT, memVN, elemTypeVN, arrayTypeMapVN);
}

ValueNum ValueNumbering::LoadArrayElem(GenTreeIndir* load, const VNFuncApp& elemAddr)
{
    assert(elemAddr.m_func == VNF_PtrToArrElem);

    ValueNum      elemTypeVN = elemAddr[0];
    ValueNum      arrayVN    = elemAddr[1];
    ValueNum      indexVN    = elemAddr[2];
    FieldSeqNode* fieldSeq   = vnStore->FieldSeqVNToFieldSeq(elemAddr[3]);

    unsigned     elemTypeNum = static_cast<unsigned>(vnStore->ConstantValue<int32_t>(elemAddr[0]));
    ClassLayout* elemLayout =
        compiler->typIsLayoutNum(elemTypeNum) ? compiler->typGetLayoutByNum(elemTypeNum) : nullptr;
    var_types elemType =
        elemLayout == nullptr ? static_cast<var_types>(elemTypeNum) : compiler->typGetStructType(elemLayout);

    ValueNum memVN = fgCurMemoryVN;
    INDEBUG(TraceMem(memVN));

    // TODO-MIKE-Fix: We should get a field sequence only for arrays of structs.
    // This isn't the best place to check this but for now it gets pmi diff
    // working again.

    if (!varTypeIsStruct(elemType) && (fieldSeq != nullptr))
    {
        fieldSeq = FieldSeqNode::NotAField();
    }

    if (fieldSeq == FieldSeqStore::NotAField())
    {
        // TODO-MIKE-Fix: Using VNForExpr with the current block for loads is suspect...
        return vnStore->VNForExpr(elemType);
    }

    ValueNum arrayTypeMapVN = vnStore->VNForMapSelect(VNK_Liberal, TYP_STRUCT, memVN, elemTypeVN);
    ValueNum arrayMapVN     = vnStore->VNForMapSelect(VNK_Liberal, TYP_STRUCT, arrayTypeMapVN, arrayVN);
    ValueNum valueVN        = vnStore->VNForMapSelect(VNK_Liberal, elemType, arrayMapVN, indexVN);

    if (fieldSeq != nullptr)
    {
        valueVN = ExtractStructField(load, VNK_Liberal, valueVN, fieldSeq);
    }
    else
    {
        valueVN = CoerceLoadValue(load, valueVN, elemType, elemLayout);
    }

    return valueVN;
}

ValueNum ValueNumbering::StoreAddressExposedLocal(GenTree* store, ValueNum lclAddrVN, GenTree* value)
{
    INDEBUG(VNFuncApp funcApp);
    assert(vnStore->GetVNFunc(lclAddrVN, &funcApp) == VNF_LclAddr);

    ValueNum memVN = fgCurMemoryVN;
    INDEBUG(TraceMem(memVN));

    // Currently we don't try to load address-exposed locals from memory so just store
    // whatever value we've got, without dealing with fields and coercion.
    // We just need to store something to create a new memory VN that keeps track that
    // some local (rather than a static/object field or array element) was modified.

    ValueNum valueVN = vnStore->ExtractValue(value->GetLiberalVN());

    // Note that the map index has to be a VNF_LclAddr VN, it cannot be just the local
    // number even if we currently don't care about field sequence and local offset.
    // We're using integers as map indices for array element types so using them for
    // locals as well would result in bad aliasing between locals and arrays.

    return vnStore->VNForMapStore(TYP_STRUCT, memVN, lclAddrVN, valueVN);
}

void ValueNumbering::NumberNullCheck(GenTreeIndir* node)
{
    assert(node->OperIs(GT_NULLCHECK));

    ValueNumPair exset = AddNullRefExset(node->GetAddr()->GetVNP());
    node->SetVNP(vnStore->PackExset(ValueNumStore::VoidVNP(), exset));
}

void ValueNumbering::NumberArrLen(GenTreeArrLen* node)
{
    VNFunc vnf = GetVNFuncForNode(node);
    assert(ValueNumStore::VNFuncIsLegal(vnf));

    GenTree*     array    = node->GetArray();
    ValueNumPair arrayVNP = vnStore->ExtractValue(array->GetVNP());

    // If we are fetching the array length for an array ref that came from global memory
    // then for CSE safety we must use the conservative value number for both.
    if ((array->gtFlags & GTF_GLOB_REF) != 0)
    {
        arrayVNP.SetBoth(arrayVNP.GetConservative());
    }

    ValueNumPair value = vnStore->VNPairForFunc(node->GetType(), vnf, arrayVNP);
    ValueNumPair exset = AddNullRefExset(array->GetVNP());
    node->SetVNP(vnStore->PackExset(value, exset));
}

void ValueNumbering::NumberCmpXchg(GenTreeCmpXchg* node)
{
    ClearMemory(node DEBUGARG("cmpxchg intrinsic"));

    ValueNumPair exset = AddNullRefExset(node->GetAddr()->GetVNP());
    exset              = vnStore->ExsetUnion(exset, vnStore->ExtractExset(node->GetValue()->GetVNP()));
    exset              = vnStore->ExsetUnion(exset, vnStore->ExtractExset(node->GetCompareValue()->GetVNP()));

    // TODO-MIKE-Fix: Using VNForExpr with the current block for loads is suspect...
    node->SetVNP(vnStore->PackExset(ValueNumPair{vnStore->VNForExpr(node->GetType())}, exset));
}

void ValueNumbering::NumberInterlocked(GenTreeOp* node)
{
    assert(node->OperIs(GT_XORR, GT_XAND, GT_XADD, GT_XCHG));

    ClearMemory(node DEBUGARG("interlocked intrinsic"));

    ValueNumPair exset = AddNullRefExset(node->GetOp(0)->GetVNP());
    exset              = vnStore->ExsetUnion(exset, vnStore->ExtractExset(node->GetOp(1)->GetVNP()));

    // TODO-MIKE-Fix: Using VNForExpr with the current block for loads is suspect...
    node->SetVNP(vnStore->PackExset(ValueNumPair{vnStore->VNForExpr(node->GetType())}, exset));
}

ValueNum ValueNumbering::LoadMemory(var_types type, ValueNum addrVN)
{
    assert(!vnStore->HasExset(addrVN));

    if (type == TYP_STRUCT)
    {
        // We can't assign a value number for a read of a struct as we can't determine
        // how many bytes will be read by this load, so return a new unique value number

        // TODO-MIKE-CQ: The type number should be used instead to get this to work.
        // TODO-MIKE-Fix: Using VNForExpr with the current block for loads is suspect...
        return vnStore->VNForExpr(TYP_STRUCT);
    }

    ValueNum memoryVN = fgCurMemoryVN;
    INDEBUG(TraceMem(memoryVN DEBUGARG("memory load")));

    // The memoization for VNFunc applications does not factor in the result type, so
    // VNF_ByrefExposedLoad takes the loaded type as an explicit parameter.

    // TODO-MIKE-CQ: It might make more sense to use the type size instead of the type
    // itself. Though to be useful this would likely require some CSE changes - if we
    // load INT and FLOAT from the same address then we could always load INT and add
    // a VNF_BitCast to FLOAT. But then the 2 loads still get different value numbers
    // and it would be up to CSE to figure out that it can replace the FLOAT load with
    // a BITCAST of the CSEd INT load. Probably too much work to be worthwhile.
    ValueNum typeVN = vnStore->VNForIntCon(type);

    return vnStore->VNForFunc(type, VNF_MemLoad, typeVN, addrVN, memoryVN);
}

//------------------------------------------------------------------------
// LoopOfVN: If the given value number is VNF_MemOpaque, VNF_MapStore, or
//    VNF_MemoryPhiDef, return the loop number where the memory update occurs,
//    otherwise returns MaxLoopNum.
//
// Arguments:
//    vn - Value number to query
//
// Return Value:
//    The memory loop number, which may be NoLoopNum.
//    Returns MaxLoopNum if this VN is not a memory value number.
//
LoopNum ValueNumStore::LoopOfVN(ValueNum vn)
{
    VNFuncApp funcApp;

    switch (GetVNFunc(vn, &funcApp))
    {
        case VNF_MemOpaque:
            return static_cast<LoopNum>(funcApp[0]);
        case VNF_MapStore:
            return static_cast<LoopNum>(funcApp[3]);
        case VNF_MemoryPhi:
            return ConstantHostPtr<BasicBlock>(funcApp[1])->GetLoopNum();
        default:
            return MaxLoopNum;
    }
}

var_types ValueNumStore::TypeOfVN(ValueNum vn) const
{
    return vn == NoVN ? TYP_UNDEF : m_chunks.Get(GetChunkNum(vn))->m_type;
}

var_types ValueNumStore::GetConstantType(ValueNum vn) const
{
    if ((vn == NoVN) || (vn == VoidVN()))
    {
        return TYP_UNDEF;
    }

    Chunk* c = m_chunks.Get(GetChunkNum(vn));
    return (c->m_kind == ChunkKind::Const || c->m_kind == ChunkKind::Handle) ? c->m_type : TYP_UNDEF;
}

bool ValueNumStore::IsVNConstant(ValueNum vn) const
{
    return GetConstantType(vn) != TYP_UNDEF;
}

bool ValueNumStore::IsVNInt32Constant(ValueNum vn) const
{
    return GetConstantType(vn) == TYP_INT;
}

bool ValueNumStore::IsIntegralConstant(ValueNum vn, ssize_t* value) const
{
    assert(!HasExset(vn));

    switch (GetConstantType(vn))
    {
        case TYP_INT:
            *value = ConstantValue<int32_t>(vn);
            return true;
#ifdef TARGET_64BIT
        case TYP_LONG:
            *value = ConstantValue<int64_t>(vn);
            return true;
#endif
        default:
            return false;
    }
}

GenTreeFlags ValueNumStore::GetHandleFlags(ValueNum vn) const
{
    assert(IsVNHandle(vn));

    Chunk*   chunk = m_chunks.Get(GetChunkNum(vn));
    unsigned index = ChunkOffset(vn);
    return static_cast<VNHandle*>(chunk->m_defs)[index].kind;
}

bool ValueNumStore::IsVNHandle(ValueNum vn) const
{
    return (vn != NoVN) && (m_chunks.Get(GetChunkNum(vn))->m_kind == ChunkKind::Handle);
}

bool ValueNumStore::IsVNCompareCheckedBound(const VNFuncApp& funcApp)
{
    assert(IsVNCompareCheckedBoundRelop(funcApp));

    return IsVNCheckedBound(funcApp[0]) || IsVNCheckedBound(funcApp[1]);
}

void ValueNumStore::GetCompareCheckedBound(const VNFuncApp& funcApp, CompareCheckedBoundArithInfo* info)
{
    assert(IsVNCompareCheckedBound(funcApp));

    if (IsVNCheckedBound(funcApp[1]))
    {
        info->cmpOper = static_cast<genTreeOps>(funcApp.m_func);
        info->cmpOp   = funcApp[0];
        info->vnBound = funcApp[1];
    }
    else
    {
        info->cmpOper = GenTree::SwapRelop(static_cast<genTreeOps>(funcApp.m_func));
        info->cmpOp   = funcApp[1];
        info->vnBound = funcApp[0];
    }
}

bool ValueNumStore::IsVNCheckedBoundArith(const VNFuncApp& funcApp)
{
    return funcApp.Is(GT_ADD, GT_SUB) && (IsVNCheckedBound(funcApp[0]) || IsVNCheckedBound(funcApp[1]));
}

bool ValueNumStore::IsVNCompareCheckedBoundArith(const VNFuncApp& funcApp)
{
    assert(IsVNCompareCheckedBoundRelop(funcApp));

    VNFuncApp arithFuncApp;
    return (GetVNFunc(funcApp[0], &arithFuncApp) && IsVNCheckedBoundArith(arithFuncApp)) ||
           (GetVNFunc(funcApp[1], &arithFuncApp) && IsVNCheckedBoundArith(arithFuncApp));
}

void ValueNumStore::GetCompareCheckedBoundArithInfo(const VNFuncApp& funcApp, CompareCheckedBoundArithInfo* info)
{
    assert(IsVNCompareCheckedBoundArith(funcApp));

    VNFuncApp arithFuncApp;

    if (GetVNFunc(funcApp[1], &arithFuncApp) && IsVNCheckedBoundArith(arithFuncApp))
    {
        info->cmpOper = static_cast<genTreeOps>(funcApp.m_func);
        info->cmpOp   = funcApp[0];
    }
    else
    {
        info->cmpOper = GenTree::SwapRelop(static_cast<genTreeOps>(funcApp.m_func));
        info->cmpOp   = funcApp[1];

        GetVNFunc(funcApp[0], &arithFuncApp);
        assert(IsVNCheckedBoundArith(arithFuncApp));
    }

    info->arrOper = static_cast<genTreeOps>(arithFuncApp.m_func);

    if (IsVNCheckedBound(arithFuncApp[1]))
    {
        info->arrOp   = arithFuncApp[0];
        info->vnBound = arithFuncApp[1];
    }
    else
    {
        info->arrOp   = arithFuncApp[1];
        info->vnBound = arithFuncApp[0];
    }
}

bool ValueNumStore::IsVNCheckedBound(ValueNum vn)
{
    if (m_checkedBoundVNs.Contains(vn))
    {
        // This VN appeared as the conservative VN of the length argument of some
        // BoundsChk node.
        return true;
    }

    // Even if we haven't seen this VN in a bounds check, if it is an array length
    // VN then consider it a checked bound VN. This facilitates better bounds check
    // removal by ensuring that compares against array lengths get put in the
    // Cse::checkedBoundMap; such an array length might get CSEd with one that was
    // directly used in a bounds check, and having the map entry will let us update
    // the compare's VN so that OptimizeRangeChecks can recognize such compares.
    VNFuncApp funcApp;
    return GetVNFunc(vn, &funcApp) == VNOP_ARR_LENGTH;
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
    assert(!HasExset(arg0VN));
    assert(compiler->IsMathIntrinsic(gtMathFN));

    // If the math intrinsic is not implemented by target-specific instructions, such as implemented
    // by user calls, then don't do constant folding on it during ReadyToRun. This minimizes precision loss.

    if (IsVNConstant(arg0VN) && (!compiler->opts.IsReadyToRun() || compiler->IsTargetIntrinsic(gtMathFN)))
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
    assert(!HasExset(arg0VN));
    assert(!HasExset(arg1VN));
    assert(compiler->IsMathIntrinsic(gtMathFN));

    // If the math intrinsic is not implemented by target-specific instructions, such as implemented
    // by user calls, then don't do constant folding on it during ReadyToRun. This minimizes precision loss.

    if (IsVNConstant(arg0VN) && IsVNConstant(arg1VN) &&
        (!compiler->opts.IsReadyToRun() || compiler->IsTargetIntrinsic(gtMathFN)))
    {
        if (typ == TYP_DOUBLE)
        {
            assert(typ == TypeOfVN(arg0VN));
            assert(typ == TypeOfVN(arg1VN));
            double arg0Val = GetConstantDouble(arg0VN);
            double arg1Val = GetConstantDouble(arg1VN);
            double res;

            switch (gtMathFN)
            {
                case NI_System_Math_Atan2:
                    res = atan2(arg0Val, arg1Val);
                    break;
                case NI_System_Math_FMod:
                    res = fmod(arg0Val, arg1Val);
                    break;
                case NI_System_Math_Pow:
                    res = pow(arg0Val, arg1Val);
                    break;
                default:
                    unreached();
            }

            return VNForDoubleCon(res);
        }
        else
        {
            assert(typ == TYP_FLOAT);
            assert(typ == TypeOfVN(arg0VN));
            assert(typ == TypeOfVN(arg1VN));
            float arg0Val = GetConstantSingle(arg0VN);
            float arg1Val = GetConstantSingle(arg1VN);
            float res;

            switch (gtMathFN)
            {
                case NI_System_Math_Atan2:
                    res = atan2f(arg0Val, arg1Val);
                    break;
                case NI_System_Math_FMod:
                    res = fmodf(arg0Val, arg1Val);
                    break;
                case NI_System_Math_Pow:
                    res = powf(arg0Val, arg1Val);
                    break;
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

VNFunc ValueNumStore::GetVNFunc(ValueNum vn, VNFuncApp* funcApp) const
{
    if (vn == NoVN)
    {
        funcApp->m_func = VNF_None;
        return VNF_None;
    }

    Chunk*   c      = m_chunks.Get(GetChunkNum(vn));
    unsigned offset = ChunkOffset(vn);
    assert(offset < c->m_count);

    switch (c->m_kind)
    {
        case ChunkKind::Func4:
        {
            VNFuncDef4* farg4  = &static_cast<VNFuncDef4*>(c->m_defs)[offset];
            funcApp->m_func    = farg4->m_func;
            funcApp->m_arity   = 4;
            funcApp->m_args[0] = farg4->m_arg0;
            funcApp->m_args[1] = farg4->m_arg1;
            funcApp->m_args[2] = farg4->m_arg2;
            funcApp->m_args[3] = farg4->m_arg3;
            return funcApp->m_func;
        }
        case ChunkKind::Func3:
        {
            VNFuncDef3* farg3  = &static_cast<VNFuncDef3*>(c->m_defs)[offset];
            funcApp->m_func    = farg3->m_func;
            funcApp->m_arity   = 3;
            funcApp->m_args[0] = farg3->m_arg0;
            funcApp->m_args[1] = farg3->m_arg1;
            funcApp->m_args[2] = farg3->m_arg2;
            return funcApp->m_func;
        }
        case ChunkKind::Func2:
        {
            VNFuncDef2* farg2  = &static_cast<VNFuncDef2*>(c->m_defs)[offset];
            funcApp->m_func    = farg2->m_func;
            funcApp->m_arity   = 2;
            funcApp->m_args[0] = farg2->m_arg0;
            funcApp->m_args[1] = farg2->m_arg1;
            return funcApp->m_func;
        }
        case ChunkKind::Func1:
        {
            VNFuncDef1* farg1  = &static_cast<VNFuncDef1*>(c->m_defs)[offset];
            funcApp->m_func    = farg1->m_func;
            funcApp->m_arity   = 1;
            funcApp->m_args[0] = farg1->m_arg0;
            return funcApp->m_func;
        }
        case ChunkKind::Func0:
        {
            VNFuncDef0* farg0 = &static_cast<VNFuncDef0*>(c->m_defs)[offset];
            funcApp->m_func   = farg0->m_func;
            funcApp->m_arity  = 0;
            return funcApp->m_func;
        }
        case ChunkKind::NotAField:
            funcApp->m_func  = VNF_NotAField;
            funcApp->m_arity = 0;
            return VNF_NotAField;
        default:
            funcApp->m_func = VNF_None;
            return VNF_None;
    }
}

#ifdef DEBUG

void ValueNumStore::Dump(ValueNum vn, bool isPtr)
{
    VNFuncApp funcApp;

    printf(" {");

    if (vn == NoVN)
    {
        printf("NoVN");
    }
    else if (IsVNHandle(vn))
    {
        printf("Hnd const: 0x%p", dspPtr(ConstantValue<ssize_t>(vn)));
    }
    else if (IsVNConstant(vn))
    {
        switch (TypeOfVN(vn))
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
                int64_t val = ConstantValue<int64_t>(vn);
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
                if (vn == NullVN())
                {
                    printf("null");
                }
                else if (vn == VoidVN())
                {
                    printf("void");
                }
                else
                {
                    assert(vn == ZeroMapVN());
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
                int64_t val = ConstantValue<int64_t>(vn);
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
    else if (VNFunc func = GetVNFunc(vn, &funcApp))
    {
        switch (func)
        {
            case VNF_FieldSeq:
                DumpFieldSeq(funcApp, true);
                break;
            case VNF_MapSelect:
                DumpMapSelect(funcApp);
                break;
            case VNF_MapStore:
                DumpMapStore(funcApp);
                break;
            case VNF_ValWithExset:
                DumpValWithExc(funcApp);
                break;
            case VNF_MemOpaque:
                DumpMemOpaque(funcApp);
                break;
            case VNF_LclAddr:
                DumpLclAddr(funcApp);
                break;
            case VNF_BitCast:
                DumpBitCast(funcApp);
                break;
            case VNF_Cast:
                DumpCast(funcApp);
                break;
            case VNF_PtrToArrElem:
                DumpPtrToArrElem(funcApp);
                break;
            default:
                if (IsVNCompareCheckedBoundRelop(funcApp))
                {
                    CompareCheckedBoundArithInfo info;

                    if (IsVNCompareCheckedBound(funcApp))
                    {
                        GetCompareCheckedBound(funcApp, &info);
                        info.Dump();
                        break;
                    }

                    if (IsVNCompareCheckedBoundArith(funcApp))
                    {
                        GetCompareCheckedBoundArithInfo(funcApp, &info);
                        info.Dump();
                        break;
                    }
                }

                printf("%s", GetFuncName(func));

#ifdef FEATURE_HW_INTRINSICS
                if (func >= VNF_HWI_FIRST)
                {
                    var_types type = VNFuncSimdBaseType(func);
                    unsigned  size = VNFuncSimdSize(func);

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
#endif

                printf("(");

                for (unsigned i = 0; i < funcApp.m_arity; i++)
                {
                    if (i > 0)
                    {
                        printf(", ");
                    }

                    printf(FMT_VN, funcApp[i]);
#if 0
                    printf("=");
                    vnDump(comp, funcApp[i]);
#endif
                }

                printf(")");
        }
    }
    else
    {
        printf("%x", vn);
    }

    printf("}");
}

void ValueNumStore::DumpValWithExc(const VNFuncApp& valWithExc)
{
    assert(valWithExc.m_func == VNF_ValWithExset);

    ValueNum normVN = valWithExc[0]; // First arg is the VN from normal execution
    ValueNum excVN  = valWithExc[1]; // Second arg is the set of possible exceptions

    VNFuncApp excSeq;
    GetVNFunc(excVN, &excSeq);

    printf("norm=");
    printf(FMT_VN, normVN);
    Dump(normVN);
    printf(", exc=");
    printf(FMT_VN, excVN);
    DumpExcSeq(excSeq, true);
}

void ValueNumStore::DumpExcSeq(const VNFuncApp& excSeq, bool isHead)
{
    assert(excSeq.m_func == VNF_ExsetCons);

    ValueNum curExc  = excSeq[0];
    bool     hasTail = excSeq[1] != EmptyExsetVN();

    if (isHead && hasTail)
    {
        printf("(");
    }

    Dump(curExc);

    if (hasTail)
    {
        printf(", ");
        VNFuncApp tail;
        GetVNFunc(excSeq[1], &tail);
        DumpExcSeq(tail, false);
    }

    if (isHead && hasTail)
    {
        printf(")");
    }
}

void ValueNumStore::DumpFieldSeq(const VNFuncApp& fieldSeq, bool isHead)
{
    assert(fieldSeq.m_func == VNF_FieldSeq);

    printf("FieldSeq(");
    compiler->dmpFieldSeqFields(ConstantHostPtr<FieldSeqNode>(fieldSeq[0]));
    printf(")");
}

void ValueNumStore::DumpMapSelect(const VNFuncApp& mapSelect)
{
    assert(mapSelect.m_func == VNF_MapSelect);

    ValueNum mapVN   = mapSelect[0];
    ValueNum indexVN = mapSelect[1];

    printf("MapSelect(");
    Print(mapVN, 0);
    printf(", ");
    Print(indexVN, 0);
    if (const char** name = m_vnNameMap.LookupPointer(indexVN))
    {
        printf(" (%s)", *name);
    }
    printf(")");
}

void ValueNumStore::DumpMapStore(const VNFuncApp& mapStore)
{
    assert(mapStore.m_func == VNF_MapStore);

    ValueNum mapVN    = mapStore[0];
    ValueNum indexVN  = mapStore[1];
    ValueNum newValVN = mapStore[2];
    unsigned loopNum  = mapStore[3];

    printf("MapStore(");
    Print(mapVN, 0);
    printf(", ");
    Print(indexVN, 0);
    if (const char** name = m_vnNameMap.LookupPointer(indexVN))
    {
        printf(" (%s)", *name);
    }
    printf(", ");
    Print(newValVN, 0);
    if (loopNum != NoLoopNum)
    {
        printf(", " FMT_LP, loopNum);
    }
    printf(")");
}

void ValueNumStore::DumpMemOpaque(const VNFuncApp& memOpaque)
{
    assert(memOpaque.m_func == VNF_MemOpaque);
    const unsigned loopNum = memOpaque[0];

    if (loopNum == NoLoopNum)
    {
        printf("MemOpaque:NotInLoop");
    }
    else if (loopNum == MaxLoopNum)
    {
        printf("MemOpaque:Indeterminate");
    }
    else
    {
        printf("MemOpaque:" FMT_LP, loopNum);
    }
}

void ValueNumStore::DumpLclAddr(const VNFuncApp& func)
{
    assert(func.m_func == VNF_LclAddr);

    unsigned      lclNum     = ConstantValue<unsigned>(func[0]);
    target_size_t offset     = ConstantValue<target_size_t>(func[1]);
    ValueNum      fieldSeqVN = func[2];

    printf("LclAddr(V%02u, @%u,", lclNum, static_cast<unsigned>(offset));
    Dump(fieldSeqVN, false);
    printf(")");
}

void ValueNumStore::DumpBitCast(const VNFuncApp& cast)
{
    assert(cast.m_func == VNF_BitCast);

    uint32_t  packedCastType = static_cast<uint32_t>(GetConstantInt32(cast[1]));
    var_types toType         = static_cast<var_types>(packedCastType >> VCA_BitCount);
    var_types fromType       = varActualType(TypeOfVN(cast[0]));

    printf("BitCast<%s, %s>(" FMT_VN ", " FMT_VN ")", varTypeName(fromType), varTypeName(toType), cast[0], cast[1]);
}

void ValueNumStore::DumpCast(const VNFuncApp& cast)
{
    assert(cast.m_func == VNF_Cast);

    uint32_t  packedCastType = static_cast<uint32_t>(GetConstantInt32(cast[1]));
    var_types toType         = static_cast<var_types>(packedCastType >> VCA_BitCount);
    var_types fromType       = varActualType(TypeOfVN(cast[0]));

    if ((packedCastType & VCA_UnsignedSrc) != 0)
    {
        fromType = varTypeToUnsigned(fromType);
    }

    printf("Cast<%s, %s>(" FMT_VN ", " FMT_VN ")", varTypeName(fromType), varTypeName(toType), cast[0], cast[1]);
}

void ValueNumStore::DumpPtrToArrElem(const VNFuncApp& elemAddr)
{
    assert(elemAddr.m_func == VNF_PtrToArrElem);

    ValueNum      elemTypeVN = elemAddr[0];
    ValueNum      arrayVN    = elemAddr[1];
    ValueNum      indexVN    = elemAddr[2];
    FieldSeqNode* fieldSeq   = FieldSeqVNToFieldSeq(elemAddr[3]);

    unsigned     elemTypeNum = static_cast<unsigned>(ConstantValue<int32_t>(elemAddr[0]));
    ClassLayout* elemLayout =
        compiler->typIsLayoutNum(elemTypeNum) ? compiler->typGetLayoutByNum(elemTypeNum) : nullptr;
    var_types elemType =
        elemLayout == nullptr ? static_cast<var_types>(elemTypeNum) : compiler->typGetStructType(elemLayout);

    printf("PtrToArrElem(");
    printf(FMT_VN, elemTypeVN);
    printf(" (%s", varTypeName(elemType));
    if (elemLayout != nullptr)
    {
        printf("<%s>", elemLayout->GetClassName());
    }
    printf("), ");
    Print(arrayVN, 1);
    printf(", ");
    Print(indexVN, 1);
    if (fieldSeq != nullptr)
    {
        printf(", ");
        compiler->dmpFieldSeqFields(fieldSeq);
    }
    printf(")");
}

#endif // DEBUG

struct VNFuncAttrs
{
    static constexpr unsigned MaxArity = 15;

    uint8_t arity : 4;
    bool    commutative : 1;
    bool    illegal : 1;
    bool    knownNotNull : 1;
};

static_assert_no_msg(sizeof(VNFuncAttrs) == 1);

static const VNFuncAttrs vnFuncAttrs[VNF_Count]{
#define GTNODE(n, s, k) {(((k)&GTK_BINOP) != 0) ? 2 : (((k)&GTK_UNOP) != 0), ((k)&GTK_COMMUTE) != 0, ((k)&GTK_VN) == 0},
#include "gtlist.h"
    {}, // VNF_Boundary
#define ValueNumFuncDef(f, a, c, n, s) {static_cast<uint8_t>(a & VNFuncAttrs::MaxArity), c, false, n},
#include "valuenumfuncs.h"
};

const VNFuncAttrs& ValueNumStore::VNFuncAttrs(VNFunc vnf)
{
    return vnFuncAttrs[VNFuncIndex(vnf)];
}

unsigned ValueNumStore::VNFuncArity(VNFunc vnf)
{
    return VNFuncAttrs(vnf).arity;
}

bool ValueNumStore::VNFuncArityIsLegal(VNFunc vnf, unsigned arity)
{
    return VNFuncArityIsVariable(vnf) || (VNFuncArity(vnf) == arity);
}

bool ValueNumStore::VNFuncArityIsVariable(VNFunc vnf)
{
    return VNFuncAttrs(vnf).arity == VNFuncAttrs::MaxArity;
}

bool ValueNumStore::VNFuncIsCommutative(VNFunc vnf)
{
    return VNFuncAttrs(vnf).commutative;
}

bool ValueNumStore::IsLegalVNFuncOper(genTreeOps gtOper)
{
    return !VNFuncAttrs(static_cast<VNFunc>(gtOper)).illegal;
}

bool ValueNumStore::IsKnownNonNull(ValueNum vn)
{
    VNFuncApp funcApp;
    return VNFuncAttrs(GetVNFunc(vn, &funcApp)).knownNotNull;
}

bool ValueNumStore::VNFuncIsLegal(VNFunc vnf)
{
    return (static_cast<unsigned>(vnf) > VNF_Boundary) || IsLegalVNFuncOper(static_cast<genTreeOps>(vnf));
}

VNFunc ValueNumStore::GenTreeOpToVNFunc(genTreeOps gtOper)
{
    assert(IsLegalVNFuncOper(gtOper));
    return static_cast<VNFunc>(gtOper);
}

bool ValueNumStore::VNFuncIsComparison(VNFunc vnf)
{
    if (vnf >= VNF_Boundary)
    {
        // For integer types we have unsigned comparisions, and
        // for floating point types these are the unordered variants.
        //
        return ((vnf == VNF_LT_UN) || (vnf == VNF_LE_UN) || (vnf == VNF_GE_UN) || (vnf == VNF_GT_UN));
    }
    genTreeOps gtOp = genTreeOps(vnf);
    return GenTree::OperIsCompare(gtOp) != 0;
}

#ifdef DEBUG
const char* ValueNumStore::GetFuncName(VNFunc vnf)
{
    vnf = VNFuncIndex(vnf);

    if (vnf < VNF_Boundary)
    {
        return GenTree::OpName(genTreeOps(vnf));
    }

#define ValueNumFuncDef(vnf, arity, commute, knownNonNull, sharedStatic) #vnf,
    static const char* const funcNames[]{
#include "valuenumfuncs.h"
    };
#undef ValueNumFuncDef

    return funcNames[vnf - (VNF_Boundary + 1)];
}

const char* ValueNumStore::GetReservedName(ValueNum vn)
{
    int val = vn - RecursiveVN;
    int max = ValueNumStore::SRC_NumSpecialRefConsts - RecursiveVN;

    if ((val < 0) || (val >= max))
    {
        return nullptr;
    }

    static const char* const reservedNames[]{
        "$VN.Recursive",  // -2  RecursiveVN
        "$VN.No",         // -1  NoVN
        "$VN.Null",       //  0  NullVN()
        "$VN.Void",       //  3  VoidVN()
        "$VN.EmptyExcSet" //  4  EmptyExsetVN()
    };

    return reservedNames[val];
}

#endif // DEBUG

// Returns true if "vn" is a reserved value number

// static
bool ValueNumStore::IsReservedVN(ValueNum vn)
{
    int val = vn - RecursiveVN; // Adding two, making 'RecursiveVN' equal to zero
    int max = ValueNumStore::SRC_NumSpecialRefConsts - RecursiveVN;

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

    SsaOptimizer ssa(comp);

    ValueNumStore* vns    = new (comp->getAllocatorDebugOnly()) ValueNumStore(ssa);
    ValueNum       vnNull = NullVN();
    assert(vnNull == NullVN());

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
    VNFuncApp fa2a;
    vns->GetVNFunc(vnForFunc2a, &fa2a);
    assert(fa2a.m_func == VNF_Add && fa2a.m_arity == 2 && fa2a[0] == vnFor1 && fa2a[1] == vnRandom1);

    ValueNum vnForFunc2b = vns->VNForFunc(TYP_INT, VNF_Add, vnFor1, vnFor100);
    assert(vnForFunc2b == vns->VNForFunc(TYP_INT, VNF_Add, vnFor1, vnFor100));
    assert(vnForFunc2b != vnFor1D && vnForFunc2b != vnFor1F && vnForFunc2b != vnFor1 && vnForFunc2b != vnFor100);
    assert(vns->TypeOfVN(vnForFunc2b) == TYP_INT);
    assert(vns->IsVNConstant(vnForFunc2b));
    assert(vns->ConstantValue<int>(vnForFunc2b) == 101);

    // printf("Did ValueNumStore::RunTests.\n");
}

void RunValueNumStoreTests(Compiler* comp)
{
    ValueNumStore::RunTests(comp);
}
#endif // DEBUG

// This represents the "to do" state of the value number computation.
class ValueNumberState
{
    Compiler*               m_comp;
    ValueNumbering*         valueNumbering;
    uint8_t*                m_visited;
    ArrayStack<BasicBlock*> m_toDoAllPredsDone;
    ArrayStack<BasicBlock*> m_toDoNotAllPredsDone;

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

public:
    ValueNumberState(Compiler* comp, ValueNumbering* valueNumbering)
        : m_comp(comp)
        , valueNumbering(valueNumbering)
        , m_visited(new (comp, CMK_ValueNumber) uint8_t[comp->fgBBNumMax + 1]())
        , m_toDoAllPredsDone(comp->getAllocator(CMK_ValueNumber))
        , m_toDoNotAllPredsDone(comp->getAllocator(CMK_ValueNumber))
    {
        m_toDoAllPredsDone.Push(comp->fgFirstBB);
    }

    BasicBlock* GetNextBlock(const SsaOptimizer& ssa)
    {
        if (m_toDoAllPredsDone.Empty() && !m_toDoNotAllPredsDone.Empty())
        {
            if (BasicBlock* block = ChooseFromNotAllPredsDone(ssa))
            {
                return block;
            }
        }

        if (!m_toDoAllPredsDone.Empty())
        {
            return m_toDoAllPredsDone.Pop();
        }

        return nullptr;
    }

    BasicBlock* ChooseFromNotAllPredsDone(const SsaOptimizer& ssa)
    {
        assert(m_toDoAllPredsDone.Empty());

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
            if (valueNumbering->BlockIsLoopEntry(cand, &lnum))
            {
                LoopDsc* loopTable = ssa.GetLoopTable();

                // "lnum" is the innermost loop of which "cand" is the entry; find the outermost.

                for (unsigned lnumPar = loopTable[lnum].lpParent; lnumPar != NoLoopNum;
                     lnumPar          = loopTable[lnumPar].lpParent)
                {
                    if (loopTable[lnumPar].lpEntry == cand)
                    {
                        lnum = lnumPar;
                    }
                    else
                    {
                        break;
                    }
                }

                bool allNonLoopPredsDone = true;
                for (flowList* pred = m_comp->BlockPredsWithEH(cand); pred != nullptr; pred = pred->flNext)
                {
                    BasicBlock* predBlock = pred->getBlock();

                    if (!loopTable[lnum].lpContains(predBlock))
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

        // If we didn't find a loop entry block with all non-loop preds done above,
        // then return a random member (if there is one).
        return m_toDoNotAllPredsDone.Empty() ? nullptr : m_toDoNotAllPredsDone.Pop();
    }

    // Record that "blk" has been visited, and add any unvisited successors of "blk" to the appropriate todo set.
    void FinishVisit(BasicBlock* blk)
    {
        JITDUMP("finish(" FMT_BB ").\n", blk->bbNum);

        SetVisitBit(blk->bbNum, BVB_complete);

        for (BasicBlock* succ : blk->GetAllSuccs(m_comp))
        {
            JITDUMP("   Succ(" FMT_BB ").\n", succ->bbNum);

            if (GetVisitBit(succ->bbNum, BVB_complete))
            {
                continue;
            }

            JITDUMP("     Not yet completed.\n");

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
                JITDUMP("     All preds complete, adding to allDone.\n");

                assert(!GetVisitBit(succ->bbNum, BVB_onAllDone)); // Only last completion of last succ should add to
                                                                  // this.
                m_toDoAllPredsDone.Push(succ);
                SetVisitBit(succ->bbNum, BVB_onAllDone);
            }
            else
            {
                JITDUMP("     Not all preds complete  Adding to notallDone, if necessary...\n");

                if (!GetVisitBit(succ->bbNum, BVB_onNotAllDone))
                {
                    JITDUMP("       Was necessary.\n");

                    m_toDoNotAllPredsDone.Push(succ);
                    SetVisitBit(succ->bbNum, BVB_onNotAllDone);
                }
            }
        }
    }
};

ValueNumbering::ValueNumbering(SsaOptimizer& ssa)
    : ssa(ssa), compiler(ssa.GetCompiler()), vm(ssa.GetCompiler()->info.compCompHnd), vnStore(ssa.GetVNStore())
{
}

void ValueNumbering::Run()
{
    if (compiler->optLoopCount > 0)
    {
        SummarizeLoopMemoryStores();
    }

    NumberInitDefs();
    NumberBlocks();
}

void ValueNumbering::NumberInitDefs()
{
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

    // The first block is expected to not be part of any loop so that
    // parameters and initial memory are treated as loop invariant.
    assert(compiler->fgFirstBB->GetLoopNum() == NoLoopNum);

    for (GenTreeLclDef* def = ssa.GetInitLclDefs(); def != nullptr; def = static_cast<GenTreeLclDef*>(def->gtNext))
    {
        unsigned   lclNum   = def->GetLclNum();
        LclVarDsc* lcl      = compiler->lvaGetDesc(lclNum);
        var_types  type     = lcl->GetType();
        bool       isZeroed = false;

        if (!lcl->IsParam())
        {
            isZeroed = (compiler->info.compInitMem || lcl->lvMustInit);

            // For OSR, locals or promoted fields of locals may be missing the initial def
            // because of partial importation. We can't assume they are zero.
            if (compiler->lvaIsOSRLocal(lclNum))
            {
                isZeroed = false;
            }

#ifdef TARGET_X86
            if (lclNum == compiler->lvaVarargsBaseOfStkArgs)
            {
                isZeroed = false;
            }
#endif

            if (type == TYP_BLK)
            {
                // TYP_BLK is used for the EHSlots locals on x86 (aka shadowSPslotsVar),
                // for the lvaInlinedPInvokeFrameVar on x64/x86/ARM and for the outgoing
                // argument area if FEATURE_FIXED_OUT_ARGS is enabled.
                // These locals are not zero initialized.
                isZeroed = false;
                type     = TYP_STRUCT;
            }
        }

        ValueNum initVN = isZeroed ? vnStore->VNZeroForType(type) : vnStore->VNForExpr(compiler->fgFirstBB, type);
        def->SetVNP(ValueNumPair{initVN});
        INDEBUG(TraceLocal(lclNum, def->GetVNP()));
    }

    // Give memory an initial value number (about which we know nothing).
    ssa.GetInitMemoryDef()->vn = vnStore->VNForExpr(compiler->fgFirstBB, TYP_STRUCT);
}

void ValueNumbering::NumberBlocks()
{
    ValueNumberState vs(compiler, this);

    while (BasicBlock* block = vs.GetNextBlock(ssa))
    {
        NumberBlock(block);
        vs.FinishVisit(block);
    }
}

void ValueNumbering::SummarizeLoopMemoryStores()
{
    LoopDsc* loopTable = ssa.GetLoopTable();
    unsigned loopCount = ssa.GetLoopCount();

    vnLoopTable = compiler->getAllocator(CMK_ValueNumber).allocate<VNLoop>(loopCount);

    for (unsigned loopNum = 0; loopNum < loopCount; loopNum++)
    {
        new (&vnLoopTable[loopNum]) VNLoop();
    }

    for (unsigned loopNum = 0; loopNum < loopCount; loopNum++)
    {
        if ((loopTable[loopNum].lpFlags & LPFLG_REMOVED) != 0)
        {
            continue;
        }

        if (loopTable[loopNum].lpParent != NoLoopNum)
        {
            continue;
        }

        for (BasicBlock* const block : loopTable[loopNum].LoopBlocks())
        {
            if (block->GetLoopNum() == NoLoopNum)
            {
                // We encountered a block that was moved into the loop range (by fgReorderBlocks),
                // but not marked correctly as being inside the loop.
                // We conservatively mark this loop (and any outer loops) as having memory havoc
                // side effects.
                VNLoopMemorySummary summary(this, loopNum);
                summary.AddMemoryHavoc();
                summary.UpdateLoops();

                // All done, no need to keep visiting more blocks.
                // TODO-MIKE-Review: What about calls?
                // And in general this case is dubious. Why wasn't the block marked correctly?
                // Is it a part of the loop or not? Why wasn't this fixed? Stupid JIT commenting
                // as usual, write a bunch of crap that doesn't actually explain anything.
                break;
            }

            VNLoopMemorySummary summary(this, block->GetLoopNum());

            if (!summary.IsComplete())
            {
                SummarizeLoopBlockMemoryStores(block, summary);
                summary.UpdateLoops();
            }
        }
    }

    JITDUMP("\n");
}

void ValueNumbering::SummarizeLoopBlockMemoryStores(BasicBlock* block, VNLoopMemorySummary& summary)
{
    for (Statement* const stmt : block->NonPhiStatements())
    {
        for (GenTree* const node : stmt->Nodes())
        {
            SummarizeLoopNodeMemoryStores(node, summary);

            if (summary.IsComplete())
            {
                return;
            }
        }

        if (summary.IsComplete())
        {
            return;
        }
    }
}

bool ValueNumbering::BlockIsLoopEntry(BasicBlock* block, unsigned* loopNum)
{
    LoopDsc* loopTable = ssa.GetLoopTable();

    for (unsigned n = block->GetLoopNum(); n != NoLoopNum; n = loopTable[n].lpParent)
    {
        if (loopTable[n].lpFlags & LPFLG_REMOVED)
        {
            continue;
        }

        if (loopTable[n].lpEntry == block)
        {
            *loopNum = n;
            return true;
        }
    }

    return false;
}

void ValueNumbering::NumberBlock(BasicBlock* block)
{
    vnStore->SetCurrentBlock(block);

    Statement* stmt    = block->firstStmt();
    ValueNum   blockVN = NoVN;

    for (; (stmt != nullptr) && stmt->GetRootNode()->IsPhiDef(); stmt = stmt->GetNextStmt())
    {
        GenTreeLclDef* def = stmt->GetRootNode()->AsLclDef();
        GenTreePhi*    phi = def->GetValue()->AsPhi();
        ValueNumPair   phiVNP;
        ValueNumPair   argsVNP;
        bool           isMeaningless = true;

        for (GenTreePhi::Use& use : phi->Uses())
        {
            GenTreeLclUse* arg    = use.GetNode();
            GenTreeLclDef* argDef = arg->GetDef();
            ValueNumPair   argVNP = argDef->GetVNP();

            if (argVNP.GetLiberal() == NoVN)
            {
                argVNP.SetBoth(vnStore->VNForFunc(def->GetType(), VNF_PhiArgDef, vnStore->VNForHostPtr(argDef)));
                INDEBUG(vnStore->Trace(argVNP));
            }

            arg->SetVNP(argVNP);

            if (argsVNP.GetLiberal() == NoVN)
            {
                phiVNP  = argVNP;
                argsVNP = argVNP;
            }
            else
            {
                isMeaningless &= (phiVNP == argVNP);
                argsVNP = vnStore->VNPairForFunc(def->GetType(), VNF_PhiArgs, argVNP, argsVNP);
                INDEBUG(vnStore->Trace(argsVNP));
            }
        }

        if (!isMeaningless)
        {
            INDEBUG(VNFuncApp argsFunc);
            assert(vnStore->GetVNFunc(argsVNP.GetLiberal(), &argsFunc) == VNF_PhiArgs);
            assert(vnStore->GetVNFunc(argsVNP.GetConservative(), &argsFunc) == VNF_PhiArgs);

            if (blockVN == NoVN)
            {
                blockVN = vnStore->VNForHostPtr(block);
            }

            ValueNum lclNumVN = vnStore->VNForIntCon(def->GetLclNum());

            phiVNP =
                vnStore->VNPairForFunc(def->GetType(), VNF_Phi, argsVNP, ValueNumPair{blockVN}, ValueNumPair{lclNumVN});
        }

        def->SetVNP(phiVNP);
        phi->SetVNP(phiVNP);

        INDEBUG(TraceLocal(def->GetLclNum(), phiVNP));
    }

    // Now do the same for memory.

    if (block->memoryPhi != nullptr)
    {
        ValueNum phiVN = NoVN;
        unsigned loopNum;

        if (BlockIsLoopEntry(block, &loopNum))
        {
            phiVN = BuildLoopEntryMemory(block, loopNum);
        }
        else
        {
            ValueNum argsVN        = NoVN;
            bool     isMeaningless = true;

            for (MemoryPhiArg* arg = block->memoryPhi; arg != nullptr; arg = arg->m_nextArg)
            {
                ValueNum argVN = arg->GetDef()->vn;
                INDEBUG(TraceMem(argVN, "predecessor memory"));

                if (argVN == NoVN)
                {
                    argVN = vnStore->VNForFunc(TYP_STRUCT, VNF_PhiArgDef, vnStore->VNForHostPtr(arg));
                }

                if (argsVN == NoVN)
                {
                    phiVN  = argVN;
                    argsVN = argVN;
                }
                else
                {
                    isMeaningless &= (phiVN == argVN);
                    argsVN = vnStore->VNForFunc(TYP_STRUCT, VNF_PhiArgs, argVN, argsVN);
                    INDEBUG(vnStore->Trace(argsVN));
                }
            }

            if (!isMeaningless)
            {
                INDEBUG(VNFuncApp argsFunc);
                assert(vnStore->GetVNFunc(argsVN, &argsFunc) == VNF_PhiArgs);

                if (blockVN == NoVN)
                {
                    blockVN = vnStore->VNForHostPtr(block);
                }

                phiVN = vnStore->VNForFunc(TYP_STRUCT, VNF_MemoryPhi, argsVN, blockVN);
            }
        }

        block->memoryEntryDef->vn = phiVN;
    }

    fgCurMemoryVN = block->memoryEntryDef->vn;
    assert(fgCurMemoryVN != NoVN);
    INDEBUG(TraceMem(fgCurMemoryVN));

    // Now iterate over the remaining statements, and their trees.
    for (; stmt != nullptr; stmt = stmt->GetNextStmt())
    {
#ifdef DEBUG
        if (compiler->verbose)
        {
            printf("\n***** " FMT_BB ", " FMT_STMT "(before)\n", block->bbNum, stmt->GetID());
            compiler->gtDispTree(stmt->GetRootNode());
            printf("\n");
        }
#endif

        for (GenTree* node : stmt->Nodes())
        {
            vnStore->SetCurrentNode(node);
            NumberNode(node);
        }

        vnStore->SetCurrentNode(nullptr);

#ifdef DEBUG
        if (compiler->verbose)
        {
            printf("\n***** " FMT_BB ", " FMT_STMT "(after)\n", block->bbNum, stmt->GetID());
            compiler->gtDispTree(stmt->GetRootNode());
            printf("\n");
            if (stmt->GetNextStmt() != nullptr)
            {
                printf("---------\n");
            }
        }
#endif
    }

    if (block->memoryExitDef != block->memoryEntryDef)
    {
        block->memoryExitDef->vn = fgCurMemoryVN;
    }

    vnStore->SetCurrentBlock(nullptr);
}

VNLoopMemorySummary::VNLoopMemorySummary(ValueNumbering* valueNumbering, unsigned loopNum)
    : m_compiler(valueNumbering->compiler)
    , m_vnLoopTable(valueNumbering->vnLoopTable)
    , m_loopTable(valueNumbering->compiler->optLoopTable)
    , m_loopNum(loopNum)
    , m_memoryHavoc(valueNumbering->vnLoopTable[loopNum].hasMemoryHavoc)
    , m_hasCall((m_loopTable[loopNum].lpFlags & LPFLG_HAS_CALL) != 0)
    , m_modifiesAddressExposedLocals(false)
{
    assert(loopNum < valueNumbering->compiler->optLoopCount);
}

void VNLoopMemorySummary::AddMemoryHavoc()
{
    m_memoryHavoc = true;
}

void VNLoopMemorySummary::AddCall()
{
    m_hasCall = true;
}

void VNLoopMemorySummary::AddAddressExposedLocal(unsigned lclNum)
{
    assert(m_compiler->lvaGetDesc(lclNum)->IsAddressExposed());

    if (m_modifiesAddressExposedLocals || m_memoryHavoc)
    {
        return;
    }

    m_modifiesAddressExposedLocals = true;

    for (unsigned n = m_loopNum; n != NoLoopNum; n = m_loopTable[n].lpParent)
    {
        VNLoop& loop = m_vnLoopTable[n];

        if (!loop.modifiesAddressExposedLocals)
        {
            JITDUMP("Loop " FMT_LP " stores to address-exposed local V%02u\n", n, lclNum);
        }

        loop.modifiesAddressExposedLocals = true;
    }
}

void VNLoopMemorySummary::AddField(CORINFO_FIELD_HANDLE fieldHandle)
{
    for (unsigned n = m_loopNum; n != NoLoopNum; n = m_loopTable[n].lpParent)
    {
        VNLoop& loop = m_vnLoopTable[n];

        if (loop.modifiedFields == nullptr)
        {
            loop.modifiedFields = new (m_compiler->getAllocator(CMK_ValueNumber))
                FieldHandleSet(m_compiler->getAllocator(CMK_ValueNumber));
        }

        if (loop.modifiedFields->Add(fieldHandle))
        {
            JITDUMP("Loop " FMT_LP " stores to field %s\n", n, m_compiler->eeGetFieldName(fieldHandle));
        }
    }
}

void VNLoopMemorySummary::AddArrayType(unsigned elemTypeNum)
{
    for (unsigned n = m_loopNum; n != NoLoopNum; n = m_loopTable[n].lpParent)
    {
        VNLoop& loop = m_vnLoopTable[n];

        if (loop.modifiedArrayElemTypes == nullptr)
        {
            loop.modifiedArrayElemTypes =
                new (m_compiler->getAllocator(CMK_ValueNumber)) TypeNumSet(m_compiler->getAllocator(CMK_ValueNumber));
        }

        if (loop.modifiedArrayElemTypes->Add(elemTypeNum))
        {
            JITDUMP("Loop " FMT_LP " stores to array of type %s\n", n, m_compiler->typGetName(elemTypeNum));
        }
    }
}

bool VNLoopMemorySummary::IsComplete() const
{
    // Once a loop is known to contain calls and memory havoc we can stop analyzing it.
    return m_memoryHavoc && m_hasCall;
}

void VNLoopMemorySummary::UpdateLoops() const
{
    if (m_memoryHavoc || m_hasCall)
    {
        for (unsigned n = m_loopNum; n != NoLoopNum; n = m_loopTable[n].lpParent)
        {
            VNLoop& loop = m_vnLoopTable[n];

            if (!loop.hasMemoryHavoc && m_memoryHavoc)
            {
                JITDUMP("Loop " FMT_LP " has memory havoc\n", n);
            }

            loop.hasMemoryHavoc |= m_memoryHavoc;

            if (m_hasCall)
            {
                // TODO-MIKE-Cleanup: Only LoopHoist needs LPFLG_HAS_CALL, it could
                // compute it cheaply based on BBF_HAS_CALL (which is also needed by
                // CSE) but that flag is prematurely set by global morph.

                if ((m_loopTable[n].lpFlags & LPFLG_HAS_CALL) == 0)
                {
                    JITDUMP("Loop " FMT_LP " has calls\n", n);
                }

                m_loopTable[n].lpFlags |= LPFLG_HAS_CALL;
            }
        }
    }

#if FEATURE_LOOP_ALIGN
    // A loop having call will not likely benefit from alignment.
    // TODO-MIKE-Cleanup: This has nothing to do with VN but it looks like
    // there are no other places that traverse the entire loop IR.
    if (m_hasCall && (m_loopTable[m_loopNum].lpChild == NoLoopNum))
    {
        BasicBlock* first = m_loopTable[m_loopNum].lpFirst;
        first->bbFlags &= ~BBF_LOOP_ALIGN;

        JITDUMP("Removing LOOP_ALIGN flag for " FMT_LP " that starts at " FMT_BB " because loop has a call.\n",
                m_loopNum, first->bbNum);
    }
#endif
}

ValueNum ValueNumbering::BuildLoopEntryMemory(BasicBlock* entryBlock, unsigned innermostLoopNum)
{
    LoopDsc* loopTable = ssa.GetLoopTable();

    // "loopNum" is the innermost loop for which "blk" is the entry; find the outermost one.
    assert(innermostLoopNum != NoLoopNum);

    unsigned loopNum = innermostLoopNum;

    for (unsigned loopsInNest = innermostLoopNum; loopsInNest != NoLoopNum;
         loopsInNest          = loopTable[loopsInNest].lpParent)
    {
        if (loopTable[loopsInNest].lpEntry != entryBlock)
        {
            break;
        }
        loopNum = loopsInNest;
    }

    if (vnLoopTable[loopNum].hasMemoryHavoc)
    {
        JITDUMP("    Loop " FMT_LP " has memory havoc effect\n", loopNum);
        return vnStore->VNForExpr(entryBlock, TYP_STRUCT);
    }

    // Otherwise, find the predecessors of the entry block that are not in the loop.
    // If there is only one such, use its memory value as the "base."  If more than one,
    // use a new unique VN.
    BasicBlock* nonLoopPred          = nullptr;
    bool        multipleNonLoopPreds = false;
    for (flowList* pred = compiler->BlockPredsWithEH(entryBlock); pred != nullptr; pred = pred->flNext)
    {
        BasicBlock* predBlock = pred->getBlock();
        if (!loopTable[loopNum].lpContains(predBlock))
        {
            if (nonLoopPred == nullptr)
            {
                nonLoopPred = predBlock;
            }
            else
            {
                multipleNonLoopPreds = true;
                break;
            }
        }
    }
    if (multipleNonLoopPreds)
    {
        JITDUMP("    Loop " FMT_LP " entry block has multiple non-loop predecessors\n", loopNum);
        return vnStore->VNForExpr(entryBlock, TYP_STRUCT);
    }
    // Otherwise, there is a single non-loop pred.
    assert(nonLoopPred != nullptr);
    // What is its memory post-state?
    ValueNum newMemoryVN = nonLoopPred->memoryExitDef->vn;
    assert(newMemoryVN != NoVN); // We must have processed the single non-loop pred before reaching the
                                 // loop entry.

    INDEBUG(TraceMem(newMemoryVN DEBUGARG("loop entry memory")));

    // Modify "base" by setting all the modified fields/field maps/array maps to unknown values.

    // First the fields/field maps.
    FieldHandleSet* fieldsMod = vnLoopTable[loopNum].modifiedFields;
    if (fieldsMod != nullptr)
    {
        for (CORINFO_FIELD_HANDLE fieldHandle : *fieldsMod)
        {
            ValueNum  fieldVN   = vnStore->VNForFieldSeqHandle(fieldHandle);
            var_types fieldType = TYP_STRUCT;

            if (vm->isFieldStatic(fieldHandle))
            {
                fieldType = CorTypeToVarType(vm->getFieldType(fieldHandle));
            }

            newMemoryVN =
                vnStore->VNForMapStore(TYP_STRUCT, newMemoryVN, fieldVN, vnStore->VNForExpr(entryBlock, fieldType));
        }
    }
    // Now do the array maps.
    TypeNumSet* elemTypesMod = vnLoopTable[loopNum].modifiedArrayElemTypes;
    if (elemTypesMod != nullptr)
    {
        for (unsigned elemTypeNum : *elemTypesMod)
        {
            ValueNum elemTypeVN = vnStore->VNForTypeNum(elemTypeNum);
            ValueNum uniqueVN   = vnStore->VNForExpr(entryBlock, TYP_STRUCT);
            newMemoryVN         = vnStore->VNForMapStore(TYP_STRUCT, newMemoryVN, elemTypeVN, uniqueVN);
        }
    }

    if (vnLoopTable[loopNum].modifiesAddressExposedLocals)
    {
        // We currently don't try to resolve address exposed loads to stores so do a dummy local store for now.
        ValueNum lclAddrVN = vnStore->VNForFunc(TYP_I_IMPL, VNF_LclAddr, vnStore->VNForIntCon(0),
                                                vnStore->VNZeroForType(TYP_I_IMPL), vnStore->VNForFieldSeq(nullptr));
        INDEBUG(vnStore->Trace(lclAddrVN, "dummy loop address exposed local"));
        ValueNum uniqueVN = vnStore->VNForExpr(entryBlock, TYP_STRUCT);
        newMemoryVN       = vnStore->VNForMapStore(TYP_STRUCT, newMemoryVN, lclAddrVN, uniqueVN);
    }

    return newMemoryVN;
}

void ValueNumbering::ClearMemory(GenTree* node DEBUGARG(const char* comment))
{
    UpdateMemory(node, vnStore->VNForExpr(TYP_STRUCT) DEBUGARG(comment));
}

void ValueNumbering::UpdateMemory(GenTree* node, ValueNum memVN DEBUGARG(const char* comment))
{
    assert(vnStore->GetCurrentBlock()->bbMemoryDef);

    fgCurMemoryVN = memVN;
    INDEBUG(TraceMem(fgCurMemoryVN, comment));

    SsaMemDef* def = ssa.GetMemoryDef(node);

    if (def != nullptr)
    {
        def->vn = memVN;
        JITDUMP("    Memory SSA def %u = " FMT_VN "\n", def->num, memVN);
    }
}

ValueNum ValueNumbering::GetIntConVN(GenTreeIntCon* intCon)
{
    var_types type = varActualType(intCon->GetType());

    switch (type)
    {
#ifdef TARGET_64BIT
        case TYP_LONG:
            if (intCon->IsHandle())
            {
                return vnStore->VNForHandle(intCon->GetInt64Value(), intCon->GetHandleKind());
            }

            return vnStore->VNForLongCon(intCon->GetInt64Value());
#endif

        case TYP_INT:
#ifndef TARGET_64BIT
            if (intCon->IsHandle())
            {
                return vnStore->VNForHandle(intCon->GetInt32Value(), intCon->GetHandleKind());
            }
#endif

            return vnStore->VNForIntCon(intCon->GetInt32Value());

        case TYP_REF:
            if (intCon->GetValue() == 0)
            {
                return ValueNumStore::NullVN();
            }

            assert(intCon->IsIconHandle(GTF_ICON_STR_HDL)); // Constant object can be only frozen string.
            return vnStore->VNForHandle(intCon->GetValue(), intCon->GetHandleKind());

        case TYP_BYREF:
            if (intCon->GetValue() == 0)
            {
                return ValueNumStore::NullVN();
            }

            if (intCon->IsHandle())
            {
                return vnStore->VNForHandle(intCon->GetValue(), intCon->GetHandleKind());
            }

            return vnStore->VNForByrefCon(static_cast<target_size_t>(intCon->GetValue()));

        default:
            unreached();
    }
}

void ValueNumbering::SummarizeLoopNodeMemoryStores(GenTree* node, VNLoopMemorySummary& summary)
{
    switch (node->GetOper())
    {
        case GT_STOREIND:
        case GT_STORE_OBJ:
        case GT_STORE_BLK:
            SummarizeLoopIndirMemoryStores(node->AsIndir(), summary);
            break;

        case GT_STORE_LCL_VAR:
        case GT_STORE_LCL_FLD:
            SummarizeLoopLocalMemoryStores(node->AsLclVarCommon(), summary);
            break;

        case GT_LCL_DEF:
            SummarizeLoopLocalDefs(node->AsLclDef(), summary);
            break;

        case GT_CALL:
            SummarizeLoopCallMemoryStores(node->AsCall(), summary);
            break;

        case GT_LOCKADD:
        case GT_XORR:
        case GT_XAND:
        case GT_XADD:
        case GT_XCHG:
        case GT_CMPXCHG:
        case GT_MEMORYBARRIER:
        case GT_COPY_BLK:
        case GT_INIT_BLK:
            summary.AddMemoryHavoc();
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            if (node->AsHWIntrinsic()->OperIsMemoryStore())
            {
                // TODO-MIKE-CQ: Ideally we'd figure out the store address (in most cases it's the first operand)
                // and restrict the store, at least in the trivial case of arrays where we just update the entire
                // array, without caring about individual elements. Problem is, we're dealing with native pointers
                // so we'll also have to deal with pinning locals and pointer arithmetic to be able to extract the
                // array type we need to update.
                summary.AddMemoryHavoc();
            }
            break;
#endif

        case GT_COMMA:
            node->SetLiberalVN(node->AsOp()->GetOp(1)->GetLiberalVN());
            break;

        case GT_LCL_USE:
            if (node->TypeIs(TYP_BYREF))
            {
                node->SetLiberalVN(node->AsLclUse()->GetDef()->GetLiberalVN());
            }
            break;

        case GT_LCL_ADDR:
            assert(lvaGetDesc(node->AsLclAddr())->IsAddressExposed());
            // TODO-MIKE-CQ: If the local is a promoted field we should use the parent instead,
            // so that byref exposed loads don't unnecessarily produce different value numbers.
            node->SetLiberalVN(vnStore->VNForFunc(TYP_I_IMPL, VNF_LclAddr,
                                                  vnStore->VNForIntCon(node->AsLclAddr()->GetLclNum()),
                                                  vnStore->VNZeroForType(TYP_I_IMPL), vnStore->VNForFieldSeq(nullptr)));
            break;

        case GT_CLS_VAR_ADDR:
            node->SetLiberalVN(vnStore->VNForFunc(node->GetType(), VNF_PtrToStatic,
                                                  vnStore->VNForFieldSeq(node->AsClsVar()->GetFieldSeq())));
            break;

        case GT_ADD:
        {
            ArrayInfo arrInfo;
            if (IsArrayElemAddr(node, &arrInfo))
            {
                ValueNum elemTypeEqVN   = vnStore->VNForTypeNum(arrInfo.m_elemTypeNum);
                ValueNum ptrToArrElemVN = vnStore->VNForFunc(TYP_BYREF, VNF_PtrToArrElem, elemTypeEqVN,
                                                             // The rest are dummy arguments.
                                                             vnStore->NullVN(), vnStore->NullVN(), vnStore->NullVN());
                node->SetLiberalVN(ptrToArrElemVN);
            }
            else if (node->AsOp()->GetOp(1)->IsIntCon())
            {
                VNFuncApp funcApp;
                VNFunc    func = vnStore->GetVNFunc(node->AsOp()->GetOp(0)->GetLiberalVN(), &funcApp);

                if ((func == VNF_LclAddr) || (func == VNF_PtrToStatic) || (func == VNF_PtrToArrElem))
                {
                    // For loop memory store summarization we don't care about the offset.
                    node->SetLiberalVN(node->AsOp()->GetOp(0)->GetLiberalVN());
                }
            }
        }
        break;

        default:
            break;
    }
}

void ValueNumbering::NumberNode(GenTree* node)
{
    genTreeOps oper = node->GetOper();

    switch (oper)
    {
        case GT_CNS_INT:
            node->SetVNP(ValueNumPair{GetIntConVN(node->AsIntCon())});
            break;

#ifndef TARGET_64BIT
        case GT_CNS_LNG:
            node->SetVNP(ValueNumPair{vnStore->VNForLongCon(node->AsLngCon()->GetValue())});
            break;
#endif

        case GT_CNS_DBL:
            node->SetVNP(ValueNumPair{vnStore->VNForDblCon(node->GetType(), node->AsDblCon()->GetValue())});
            break;

        case GT_LCL_ADDR:
            assert(lvaGetDesc(node->AsLclAddr())->IsAddressExposed());
            // TODO-MIKE-CQ: If the local is a promoted field we should use the parent instead,
            // so that byref exposed loads don't unnecessarily produce different value numbers.
            node->SetVNP(ValueNumPair{vnStore->VNForFunc(TYP_I_IMPL, VNF_LclAddr,
                                                         vnStore->VNForIntCon(node->AsLclAddr()->GetLclNum()),
                                                         vnStore->VNForUPtrSizeIntCon(node->AsLclAddr()->GetLclOffs()),
                                                         vnStore->VNForFieldSeq(node->AsLclAddr()->GetFieldSeq()))});
            break;

        case GT_CLS_VAR_ADDR:
            node->SetVNP(ValueNumPair{vnStore->VNForFunc(node->GetType(), VNF_PtrToStatic,
                                                         vnStore->VNForFieldSeq(node->AsClsVar()->GetFieldSeq()))});
            break;

        case GT_LCL_VAR:
            if ((node->gtFlags & GTF_VAR_DEF) == 0)
            {
                NumberLclLoad(node->AsLclVar());
            }
            break;

        case GT_LCL_FLD:
            if ((node->gtFlags & GTF_VAR_DEF) == 0)
            {
                NumberLclFldLoad(node->AsLclFld());
            }
            break;

        case GT_LCL_DEF:
            NumberLclDef(node->AsLclDef());
            break;

        case GT_LCL_USE:
            NumberLclUse(node->AsLclUse());
            break;

        case GT_INSERT:
            NumberInsert(node->AsInsert());
            break;

        case GT_EXTRACT:
            NumberExtract(node->AsExtract());
            break;

        case GT_FTN_ADDR:
            // Use the value of the function pointer (actually, a method handle.)
            node->SetVNP(
                ValueNumPair{vnStore->VNForHandle(ssize_t(node->AsFptrVal()->gtFptrMethod), GTF_ICON_METHOD_HDL)});
            break;

        case GT_CATCH_ARG:
            // We know nothing about the value of a caught expression.
            node->SetVNP(ValueNumPair{vnStore->VNForExpr(node->GetType())});
            break;

        case GT_MEMORYBARRIER:
            ClearMemory(node DEBUGARG("memory barrier"));
            break;

        case GT_COPY_BLK:
        case GT_INIT_BLK:
            ClearMemory(node DEBUGARG("dynamic sized init/copy block"));
            FALLTHROUGH;
        case GT_ARGPLACE:
            // We'll give ARGPLACE the actual argument value number when the call
            // node itself is value numbered.
            FALLTHROUGH;
        case GT_KEEPALIVE:
            node->SetVNP(ValueNumStore::VoidVNP());
            break;

        case GT_STOREIND:
        case GT_STORE_OBJ:
        case GT_STORE_BLK:
            NumberIndirStore(node->AsIndir());
            break;
        case GT_STORE_LCL_VAR:
            NumberLclStore(node->AsLclVar());
            break;
        case GT_STORE_LCL_FLD:
            NumberLclFldStore(node->AsLclFld());
            break;

        case GT_IND:
        case GT_OBJ:
        case GT_BLK:
            if ((node->gtFlags & GTF_IND_ASG_LHS) == 0)
            {
                NumberIndirLoad(node->AsIndir());

                if (node->OperMayThrow(compiler))
                {
                    AddNullRefExset(node, node->AsIndir()->GetAddr());
                }
            }
            break;

        case GT_CAST:
            NumberCast(node->AsCast());
            break;

        case GT_BITCAST:
            NumberBitCast(node->AsUnOp());
            break;

        case GT_INTRINSIC:
            NumberIntrinsic(node->AsIntrinsic());
            break;

        case GT_COMMA:
            NumberComma(node->AsOp());
            break;

        case GT_NULLCHECK:
            NumberNullCheck(node->AsIndir());
            break;

        case GT_ARR_LENGTH:
            NumberArrLen(node->AsArrLen());
            break;

        case GT_QMARK:
        case GT_LOCKADD:
            unreached();

        case GT_XORR:
        case GT_XAND:
        case GT_XADD:
        case GT_XCHG:
            NumberInterlocked(node->AsOp());
            break;

        case GT_JTRUE:
        case GT_RETURN:
        case GT_SWITCH:
        case GT_JMP:
        case GT_RETFILT:
#ifndef FEATURE_EH_FUNCLETS
        case GT_END_LFIN:
#endif
            // These nodes never need to have a ValueNumber
            node->SetVNP({});
            break;

        case GT_BOX:
            // BOX doesn't do anything at this point, the actual object allocation
            // and initialization happens separately (and not numbering BOX correctly
            // prevents seeing allocation related assertions through it)
            node->SetVNP(node->AsUnOp()->GetOp(0)->GetVNP());
            break;

        case GT_CALL:
            NumberCall(node->AsCall());
            break;

        case GT_BOUNDS_CHECK:
            NumberBoundsCheck(node->AsBoundsChk());
            break;

        case GT_CMPXCHG:
            NumberCmpXchg(node->AsCmpXchg());
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            NumberHWIntrinsic(node->AsHWIntrinsic());
            break;
#endif

        case GT_LEA:
        // LEAs could probably value numbered as ADD/MUL expressions but
        // they should not appear in frontend so it's not worth the trouble.
        case GT_LCLHEAP:
        // It is not necessary to model the StackOverflow exception for LCLHEAP
        case GT_LABEL:
            node->SetVNP(ValueNumPair{vnStore->VNForExpr(node->GetType())});
            break;

        case GT_CKFINITE:
            NumbeCkFinite(node->AsUnOp());
            break;

        case GT_NO_OP:
            assert(node->TypeIs(TYP_VOID));
            node->SetVNP(ValueNumStore::VoidVNP());
            break;

        case GT_NOP:
            if (node->AsUnOp()->gtOp1 == nullptr)
            {
                node->SetVNP(ValueNumStore::VoidVNP());
            }
            else
            {
                node->SetVNP(node->AsUnOp()->GetOp(0)->GetVNP());
            }
            break;

        case GT_DIV:
        case GT_UDIV:
        case GT_MOD:
        case GT_UMOD:
            NumberDivMod(node->AsOp());
            break;

        case GT_ADD:
            if (!node->gtOverflow())
            {
                ValueNum addrVN = AddField(node->AsOp());

                if (addrVN != NoVN)
                {
                    // We don't care about differences between liberal and conservative for pointer values.
                    // TODO-MIKE-Fix: That doesn't make a lot of sense, ExtendPtrVN only looks at the liberal VN.
                    node->SetVNP({addrVN, addrVN});
                    break;
                }
            }
            FALLTHROUGH;
        default:
            if (GenTree::OperIsUnary(oper))
            {
                VNFunc vnf = GetVNFuncForNode(node);
                assert(ValueNumStore::VNFuncIsLegal(vnf));
                assert(!node->OperMayThrow(compiler));

                ValueNumPair exset;
                ValueNumPair vnp = vnStore->UnpackExset(node->AsOp()->GetOp(0)->GetVNP(), &exset);
                node->SetVNP(vnStore->PackExset(vnStore->VNPairForFunc(node->GetType(), vnf, vnp), exset));
            }
            else if (GenTree::OperIsBinary(oper))
            {
                VNFunc vnf = GetVNFuncForNode(node);
                assert(ValueNumStore::VNFuncIsLegal(vnf));

                ValueNumPair exset1;
                ValueNumPair vnp1 = vnStore->UnpackExset(node->AsOp()->GetOp(0)->GetVNP(), &exset1);
                ValueNumPair exset2;
                ValueNumPair vnp2 = vnStore->UnpackExset(node->AsOp()->GetOp(1)->GetVNP(), &exset2);
                ValueNumPair vnp  = vnStore->VNPairForFunc(node->GetType(), vnf, vnp1, vnp2);
                node->SetVNP(vnStore->PackExset(vnp, vnStore->ExsetUnion(exset1, exset2)));

                if (node->OperIs(GT_ADD, GT_SUB, GT_MUL) && node->gtOverflow())
                {
                    AddOverflowExset(node->AsOp());
                }
                else
                {
                    assert(!node->OperMayThrow(compiler));
                }
            }
            else
            {
                // TODO-MIKE-CQ: It would be nice to give GT_ARR_ELEM a proper VN...
                noway_assert(GenTree::OperIsSpecial(oper));
                // TODO-MIKE-Fix: Using VNForExpr with the current block for arbitrary nodes is suspect.
                // Also, it might be better to explicitly list all the opers in this switch so
                // we don't accidentally miss one that may have some sort of side effects that
                // need special handling.
                node->SetVNP(ValueNumPair{vnStore->VNForExpr(node->GetType())});
            }
            break;
    }

#ifdef DEBUG
    if (compiler->verbose && (node->GetLiberalVN() != NoVN))
    {
        compiler->gtDispTree(node, false, false);
        printf("        = ");
        vnStore->Print(node->GetVNP(), 1);
        printf("\n");
    }
#endif
}

void ValueNumbering::NumberIntrinsic(GenTreeIntrinsic* intrinsic)
{
    // TODO: model the exceptions for Intrinsics

    ValueNumPair exset1;
    ValueNumPair vnp1 = vnStore->UnpackExset(intrinsic->GetOp(0)->GetVNP(), &exset1);

    if (!compiler->IsMathIntrinsic(intrinsic->GetIntrinsic()))
    {
        assert(intrinsic->GetIntrinsic() == NI_CORINFO_INTRINSIC_Object_GetType);
        assert(intrinsic->gtOp2 == nullptr);

        vnp1 = vnStore->VNPairForFunc(intrinsic->GetType(), VNF_ObjGetType, vnp1);
        intrinsic->SetVNP(vnStore->PackExset(vnp1, exset1));
    }
    else if (intrinsic->gtOp2 == nullptr)
    {
        vnp1 = vnStore->EvalMathFuncUnary(intrinsic->GetType(), intrinsic->GetIntrinsic(), vnp1);
        intrinsic->SetVNP(vnStore->PackExset(vnp1, exset1));
    }
    else
    {
        ValueNumPair exset2 = ValueNumStore::EmptyExsetVNP();
        ValueNumPair vnp2   = vnStore->UnpackExset(intrinsic->GetOp(1)->GetVNP(), &exset2);
        ValueNumPair vnp    = vnStore->EvalMathFuncBinary(intrinsic->GetType(), intrinsic->GetIntrinsic(), vnp1, vnp2);
        intrinsic->SetVNP(vnStore->PackExset(vnp, vnStore->ExsetUnion(exset1, exset2)));
    }
}

#ifdef FEATURE_HW_INTRINSICS
void ValueNumbering::NumberHWIntrinsic(GenTreeHWIntrinsic* node)
{
    // TODO: model the exceptions for intrinsics

    if (node->OperIsMemoryStore())
    {
        ClearMemory(node DEBUGARG("HWIntrinsic store"));
    }

    if (node->GetAuxiliaryType() != TYP_UNDEF)
    {
        // TODO-MIKE-CQ: We can't generate a proper VN for nodes that use the auxiliary
        // type because the type is simply ignored and we end up doing invalid CSE, see
        // vn-add-saturate-scalar.cs.
        node->SetVNP(ValueNumPair{vnStore->VNForExpr(node->GetType())});
        return;
    }

    VNFunc func = GetVNFuncForNode(node);

    if (node->OperIsMemoryLoad())
    {
        if (node->GetNumOps() > 1)
        {
            // For now we will generate a unique value number for loads with more
            // than one operand (i.e. GatherVector128)
            // TODO-MIKE-Fix: Using VNForExpr with the current block for loads is suspect...
            node->SetVNP(ValueNumPair{vnStore->VNForExpr(node->GetType())});
            return;
        }

        ValueNumPair addrXVNP;
        ValueNumPair addrVNP = vnStore->UnpackExset(node->GetOp(0)->GetVNP(), &addrXVNP);

        // The addrVN incorporates both addr's ValueNumber and the func operation
        // The func is used because operations such as LoadLow and LoadHigh perform
        // different operations, thus need to compute different ValueNumbers
        // We don't need to encode the result type as it will be encoded by the opcode in 'func'
        ValueNum addrVN = vnStore->VNForFunc(TYP_BYREF, func, addrVNP.GetLiberal());

        // The address could point anywhere, so it is an ByrefExposed load.
        ValueNum loadVN = LoadMemory(node->GetType(), addrVN);

        // TODO-MIKE-Fix: Using VNForExpr with the current block for loads is suspect...
        node->SetVNP(vnStore->PackExset({loadVN, vnStore->VNForExpr(node->GetType())}, addrXVNP));
        AddNullRefExset(node, node->GetOp(0));

        return;
    }

    unsigned arity = node->GetNumOps();

    if (arity > 4)
    {
        node->SetVNP(ValueNumPair{vnStore->VNForExpr(node->GetType())});
        return;
    }

    ValueNumPair opsVnp[4];
    ValueNumPair xvnp = ValueNumStore::EmptyExsetVNP();

    for (unsigned i = 0; i < arity; i++)
    {
        ValueNumPair opXvnp;
        opsVnp[i] = vnStore->UnpackExset(node->GetOp(i)->GetVNP(), &opXvnp);
        xvnp      = vnStore->ExsetUnion(xvnp, opXvnp);
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

    node->SetVNP(vnStore->PackExset(vnp, xvnp));
}
#endif // FEATURE_HW_INTRINSICS

void ValueNumbering::NumberBitCast(GenTreeUnOp* bitcast)
{
    assert(bitcast->OperIs(GT_BITCAST));

    var_types fromType = varActualType(bitcast->GetOp(0)->GetType());
    var_types toType   = bitcast->GetType();

    assert(varTypeSize(toType) == varTypeSize(fromType));

    ValueNumPair exset;
    ValueNumPair vnp = vnStore->UnpackExset(bitcast->GetOp(0)->GetVNP(), &exset);
    vnp.SetLiberal(vnStore->VNForBitCast(vnp.GetLiberal(), toType));
    vnp.SetConservative(vnStore->VNForBitCast(vnp.GetConservative(), toType));
    bitcast->SetVNP(vnStore->PackExset(vnp, exset));
}

ValueNum ValueNumStore::VNForBitCast(ValueNum valueVN, var_types toType)
{
    assert(!HasExset(valueVN));

    if (IsVNConstant(valueVN))
    {
        var_types valueType = TypeOfVN(valueVN);

        if ((valueType == TYP_FLOAT) && (toType == TYP_INT))
        {
            return VNForIntCon(jitstd::bit_cast<int32_t>(ConstantValue<float>(valueVN)));
        }

        if ((valueType == TYP_DOUBLE) && (toType == TYP_LONG))
        {
            return VNForLongCon(jitstd::bit_cast<int64_t>(ConstantValue<double>(valueVN)));
        }

        if ((valueType == TYP_INT) && (toType == TYP_FLOAT))
        {
            return VNForFloatCon(jitstd::bit_cast<float>(ConstantValue<int32_t>(valueVN)));
        }

        if ((valueType == TYP_LONG) && (toType == TYP_DOUBLE))
        {
            return VNForDoubleCon(jitstd::bit_cast<double>(ConstantValue<int64_t>(valueVN)));
        }

        // TODO-MIKE-CQ: Handle BITCAST(Vector2 "constant") for win-x64 ABI needs, at least
        // the trivial zero case if not something more complicated like BITCAST(Create(2, 3)).
        // The main issue is that since VN doesn't really have vector constants we'd need to
        // "parse" the BITCAST operand VN(Func) probably...
    }

    return VNForFunc(toType, VNF_BitCast, valueVN, VNForBitCastOper(toType));
}

void ValueNumbering::NumberCast(GenTreeCast* cast)
{
    var_types fromType      = varActualType(cast->GetOp(0)->GetType());
    bool      fromUnsigned  = cast->IsUnsigned();
    var_types toType        = cast->GetCastType();
    bool      checkOverflow = cast->gtOverflow();

    assert(varActualType(toType) == varActualType(cast->GetType()));

    // Sometimes GTF_UNSIGNED is unnecessarily set on CAST nodes, ignore it.
    // TODO-MIKE-Cleanup: Why is this here? Just don't set it in the first place or remove it in morph.

    if (varTypeIsFloating(fromType))
    {
        fromUnsigned = false;
    }
    else if (!checkOverflow && !varTypeIsFloating(toType) && (varTypeSize(toType) <= varTypeSize(fromType)))
    {
        fromUnsigned = false;
    }

    ValueNumPair exset;
    ValueNumPair vnp = vnStore->UnpackExset(cast->GetOp(0)->GetVNP(), &exset);

    VNFunc   vnFunc     = checkOverflow ? VNF_CastOvf : VNF_Cast;
    ValueNum castTypeVN = vnStore->VNForCastOper(toType, fromUnsigned);
    vnp                 = vnStore->VNPairForFunc(varActualType(toType), vnFunc, vnp, {castTypeVN, castTypeVN});

    if (checkOverflow)
    {
        // Do not add exceptions for folded casts. We only fold checked casts that do not overflow.
        if (!vnStore->IsVNConstant(vnp.GetLiberal()))
        {
            ValueNum ex = vnStore->VNForFunc(TYP_REF, VNF_ConvOverflowExc, vnp.GetLiberal(), castTypeVN);
            exset.SetLiberal(vnStore->ExsetUnion(exset.GetLiberal(), vnStore->ExsetCreate(ex)));
        }

        if (!vnStore->IsVNConstant(vnp.GetConservative()))
        {
            ValueNum ex = vnStore->VNForFunc(TYP_REF, VNF_ConvOverflowExc, vnp.GetConservative(), castTypeVN);
            exset.SetConservative(vnStore->ExsetUnion(exset.GetConservative(), vnStore->ExsetCreate(ex)));
        }
    }

    cast->SetVNP(vnStore->PackExset(vnp, exset));
}

ValueNum ValueNumStore::VNForCast(ValueNum valueVN, var_types toType)
{
    ValueNum castTypeVN = VNForCastOper(toType, false);
    ValueNum resultVN   = VNForFunc(varActualType(toType), VNF_Cast, valueVN, castTypeVN);
    INDEBUG(Trace(resultVN));
    return resultVN;
}

ValueNumPair ValueNumStore::VNForCast(ValueNumPair valueVNP, var_types toType)
{
    ValueNum castTypeVN = VNForCastOper(toType, false);
    return VNPairForFunc(varActualType(toType), VNF_Cast, valueVNP, {castTypeVN, castTypeVN});
}

void ValueNumbering::NumberHelperCall(GenTreeCall* call, VNFunc vnf, ValueNumPair vnpExc)
{
    assert(vnf != VNF_Boundary);

    unsigned argCount = ValueNumStore::VNFuncArity(vnf);

    if (argCount == 0)
    {
        // TODO-MIKE-Review: This drops vnpExc unlike the case with arguments below...
        call->SetVNP(ValueNumPair{vnStore->VNForFunc(call->GetType(), vnf)});

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
            vnpExc       = ValueNumStore::EmptyExsetVNP();
            break;

        case VNF_JitNewArr:
            addUniqueArg = true;
            // The New Array helper may throw an overflow exception
            vnpExc = vnStore->ExsetCreate(
                vnStore->VNPairForFunc(TYP_REF, VNF_NewArrOverflowExc, vnStore->ExtractValue(vnpCallArgs[1])));
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
            vnpExc                  = ValueNumStore::EmptyExsetVNP();
            break;

        case VNF_JitReadyToRunNewArr:
            addUniqueArg            = true;
            useEntryPointAddrAsArg0 = true;
            // The New Array helper may throw an overflow exception
            vnpExc = vnStore->ExsetCreate(
                vnStore->VNPairForFunc(TYP_REF, VNF_NewArrOverflowExc, vnStore->ExtractValue(vnpCallArgs[0])));
            break;

        case VNF_ReadyToRunStaticBase:
        case VNF_ReadyToRunGenericStaticBase:
        case VNF_ReadyToRunIsInstanceOf:
        case VNF_ReadyToRunCastClass:
        case VNF_ReadyToRunGenericHandle:
            useEntryPointAddrAsArg0 = true;
            break;

        default:
            assert(Compiler::s_helperCallProperties.IsPure(Compiler::eeGetHelperNum(call->GetMethodHandle())));
            break;
    }

    ValueNumPair vnpUniqueArg;

    if (addUniqueArg)
    {
        argCount--;
        // TODO-MIKE-Fix: Using VNForExpr with the current block for calls is suspect.
        // The call may return a value obtained by loading from memory defined
        // by a different block. This only works because calls have special
        // handling in loop hoisting.
        vnpUniqueArg.SetBoth(vnStore->VNForExpr(call->GetType()));

        if (argCount == 0)
        {
            call->SetVNP(vnStore->VNPairForFunc(call->GetType(), vnf, vnpUniqueArg));
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
        vnpArgs[vnpArgIndex++] = vnStore->UnpackExset(vnpCallArgs[i], &vnpArgExc);
        vnpExc                 = vnStore->ExsetUnion(vnpExc, vnpArgExc);
    }

    if (addUniqueArg)
    {
        vnpArgs[vnpArgIndex++] = vnpUniqueArg;
    }

    ValueNumPair vnpCall;

    switch (vnpArgIndex)
    {
        case 1:
            vnpCall = vnStore->VNPairForFunc(call->GetType(), vnf, vnpArgs[0]);
            break;
        case 2:
            vnpCall = vnStore->VNPairForFunc(call->GetType(), vnf, vnpArgs[0], vnpArgs[1]);
            break;
        case 3:
            vnpCall = vnStore->VNPairForFunc(call->GetType(), vnf, vnpArgs[0], vnpArgs[1], vnpArgs[2]);
            break;
        default:
            noway_assert(vnpArgIndex == 4);
            vnpCall = vnStore->VNPairForFunc(call->GetType(), vnf, vnpArgs[0], vnpArgs[1], vnpArgs[2], vnpArgs[3]);
            break;
    }

    ValueNumPair exset;
    vnpCall = vnStore->UnpackExset(vnpCall, &exset);

    call->SetVNP(vnStore->PackExset(vnpCall, vnStore->ExsetUnion(vnpExc, exset)));
}

void ValueNumbering::SummarizeLoopCallMemoryStores(GenTreeCall* call, VNLoopMemorySummary& summary)
{
    summary.AddCall();

    if (call->IsHelperCall())
    {
        CorInfoHelpFunc helpFunc = Compiler::eeGetHelperNum(call->gtCallMethHnd);
        if (Compiler::s_helperCallProperties.MutatesHeap(helpFunc))
        {
            summary.AddMemoryHavoc();
        }
        else if (Compiler::s_helperCallProperties.MayRunCctor(helpFunc))
        {
            // If the call is labeled as "Hoistable", then we've checked the
            // class that would be constructed, and it is not precise-init, so
            // the cctor will not be run by this call.  Otherwise, it might be,
            // and might have arbitrary side effects.
            if ((call->gtFlags & GTF_CALL_HOISTABLE) == 0)
            {
                summary.AddMemoryHavoc();
            }
        }
    }
    else
    {
        summary.AddMemoryHavoc();
    }
}

void ValueNumbering::NumberCall(GenTreeCall* call)
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
        bool modHeap = NumberHelperCall(call);

        if (modHeap)
        {
            ClearMemory(call DEBUGARG("helper call"));
        }
    }
    else
    {
        if (call->TypeIs(TYP_VOID))
        {
            call->SetVNP(ValueNumStore::VoidVNP());
        }
        else
        {
            // TODO-MIKE-Fix: Using VNForExpr with the current block for calls is suspect.
            // The call may return a value obtained by loading from memory defined
            // by a different block. This only works because calls have special
            // handling in loop hoisting.
            call->SetVNP(ValueNumPair{vnStore->VNForExpr(call->GetType())});
        }

        ClearMemory(call DEBUGARG("user call"));
    }
}

VNFunc ValueNumbering::GetHelperCallFunc(CorInfoHelpFunc helpFunc)
{
    assert(Compiler::s_helperCallProperties.IsPure(helpFunc) || Compiler::s_helperCallProperties.IsAllocator(helpFunc));

    VNFunc vnf = VNF_Boundary; // An illegal value...
    switch (helpFunc)
    {
        // These translate to other function symbols:
        case CORINFO_HELP_DIV:
            vnf = VNOP_DIV;
            break;
        case CORINFO_HELP_MOD:
            vnf = VNOP_MOD;
            break;
        case CORINFO_HELP_UDIV:
            vnf = VNOP_UDIV;
            break;
        case CORINFO_HELP_UMOD:
            vnf = VNOP_UMOD;
            break;
        case CORINFO_HELP_LLSH:
            vnf = VNOP_LSH;
            break;
        case CORINFO_HELP_LRSH:
            vnf = VNOP_RSH;
            break;
        case CORINFO_HELP_LRSZ:
            vnf = VNOP_RSZ;
            break;
        case CORINFO_HELP_LMUL:
        case CORINFO_HELP_LMUL_OVF:
            vnf = VNOP_MUL;
            break;
        case CORINFO_HELP_ULMUL_OVF:
            vnf = VNOP_MUL;
            break; // Is this the right thing?
        case CORINFO_HELP_LDIV:
            vnf = VNOP_DIV;
            break;
        case CORINFO_HELP_LMOD:
            vnf = VNOP_MOD;
            break;
        case CORINFO_HELP_ULDIV:
            vnf = VNOP_UDIV;
            break;
        case CORINFO_HELP_ULMOD:
            vnf = VNOP_UMOD;
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
            vnf = VNOP_FMOD;
            break;
        case CORINFO_HELP_DBLREM:
            vnf = VNOP_FMOD;
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

bool ValueNumbering::NumberHelperCall(GenTreeCall* call)
{
    const CorInfoHelpFunc helpFunc    = Compiler::eeGetHelperNum(call->GetMethodHandle());
    const bool            pure        = Compiler::s_helperCallProperties.IsPure(helpFunc);
    const bool            isAlloc     = Compiler::s_helperCallProperties.IsAllocator(helpFunc);
    const bool            mayRunCctor = Compiler::s_helperCallProperties.MayRunCctor(helpFunc);
    const bool            noThrow     = Compiler::s_helperCallProperties.NoThrow(helpFunc);
    bool                  modHeap     = Compiler::s_helperCallProperties.MutatesHeap(helpFunc);

    ValueNumPair exset;

    if (noThrow)
    {
        exset = ValueNumStore::EmptyExsetVNP();
    }
    else
    {
        // If the helper is known to only throw only one particular exception
        // we can set vnpExc to that exception, otherwise we conservatively
        // model the JIT helper as possibly throwing multiple different exceptions

        ValueNumPair ex;

        if (helpFunc == CORINFO_HELP_OVERFLOW)
        {
            ex = vnStore->VNPairForFunc(TYP_REF, VNF_OverflowExc, vnStore->VoidVNP());
        }
        else
        {
            // Setup vnpExc with the information that multiple different exceptions
            // could be generated by this helper
            // TODO-MIKE-Review: Hrm, shouldn't VNF_HelperMultipleExc be an unique VN, similar to VNForExpr?
            ex = vnStore->VNPairForFunc(TYP_REF, VNF_HelperMultipleExc);
        }

        exset = vnStore->ExsetCreate(ex);
    }

    ValueNumPair vnp;

    if (call->TypeIs(TYP_VOID))
    {
        vnp = ValueNumStore::VoidVNP();
    }
    else
    {
        // TODO-CQ: Handle CORINFO_HELP_NEW_MDARR
        if ((pure || isAlloc) && (helpFunc != CORINFO_HELP_NEW_MDARR))
        {
            NumberHelperCall(call, GetHelperCallFunc(helpFunc), exset);

            if (mayRunCctor && ((call->gtFlags & GTF_CALL_HOISTABLE) == 0))
            {
                modHeap = true;
            }

            return modHeap;
        }

        // TODO-MIKE-Fix: Using VNForExpr with the current block for calls is suspect.
        // The call may return a value obtained by loading from memory defined
        // by a different block. This only works because calls have special
        // handling in loop hoisting.
        vnp.SetBoth(vnStore->VNForExpr(call->GetType()));
    }

    call->SetVNP(vnStore->PackExset(vnp, exset));

    return modHeap;
}

ValueNum ValueNumbering::GetBaseAddr(ValueNum addrVN)
{
    ValueNum  baseVN = addrVN;
    ssize_t   offset = 0;
    VNFuncApp funcApp;

    while ((vnStore->GetVNFunc(baseVN, &funcApp) == VNOP_ADD) && (vnStore->TypeOfVN(baseVN) == TYP_BYREF))
    {
        // The arguments in value numbering functions are sorted in increasing order
        // Thus either arg could be the constant.
        if (vnStore->IsVNConstant(funcApp[0]) && varTypeIsIntegral(vnStore->TypeOfVN(funcApp[0])))
        {
            offset += vnStore->CoercedConstantValue<ssize_t>(funcApp[0]);
            baseVN = funcApp[1];
        }
        else if (vnStore->IsVNConstant(funcApp[1]) && varTypeIsIntegral(vnStore->TypeOfVN(funcApp[1])))
        {
            offset += vnStore->CoercedConstantValue<ssize_t>(funcApp[1]);
            baseVN = funcApp[0];
        }
        else // neither argument is a constant
        {
            break;
        }

        // TODO-MIKE-Review: This doesn't make a lot of sense...
        if (compiler->fgIsBigOffset(offset))
        {
            baseVN = addrVN;
            break;
        }
    }

    return baseVN;
}

ValueNum ValueNumbering::AddNullRefExset(ValueNum addrVN)
{
    // Peel off constant offsets so that obj.x and obj.y get the same exception VN.
    addrVN = GetBaseAddr(addrVN);

    // TODO-MIKE-Review: Shouldn't this be done before GetBaseAddr?
    ValueNum addrExset;
    addrVN = vnStore->UnpackExset(addrVN, &addrExset);

    ValueNum nullPtrExset = vnStore->ExsetCreate(vnStore->VNForFunc(TYP_REF, VNF_NullPtrExc, addrVN));
    return vnStore->ExsetUnion(nullPtrExset, addrExset);
}

ValueNumPair ValueNumbering::AddNullRefExset(ValueNumPair addrVNP)
{
    return {AddNullRefExset(addrVNP.GetLiberal()), AddNullRefExset(addrVNP.GetConservative())};
}

void ValueNumbering::AddNullRefExset(GenTree* node, GenTree* addr)
{
    assert(node->IsIndir() || node->OperIsImplicitIndir() || node->OperIs(GT_ARR_LENGTH));

    ValueNum libExset = AddNullRefExset(addr->GetLiberalVN());
    ValueNum conExset = AddNullRefExset(addr->GetConservativeVN());

    node->SetVNP(vnStore->PackExset(vnStore->ExtractValue(node->GetVNP()), {libExset, conExset}));
}

void ValueNumbering::NumberDivMod(GenTreeOp* node)
{
    assert(node->OperIs(GT_DIV, GT_MOD, GT_UDIV, GT_UMOD));
    assert(ValueNumStore::VNFuncIsLegal(static_cast<VNFunc>(node->GetOper())));

    ValueNumPair exset1;
    ValueNumPair vnp1 = vnStore->UnpackExset(node->AsOp()->GetOp(0)->GetVNP(), &exset1);
    ValueNumPair exset2;
    ValueNumPair vnp2  = vnStore->UnpackExset(node->AsOp()->GetOp(1)->GetVNP(), &exset2);
    ValueNumPair vnp   = vnStore->VNPairForFunc(node->GetType(), static_cast<VNFunc>(node->GetOper()), vnp1, vnp2);
    ValueNumPair exset = vnStore->ExsetUnion(exset1, exset2);

    const bool      isSigned = node->OperIs(GT_DIV, GT_MOD);
    const var_types type     = varActualType(node->GetType());
    assert((type == TYP_INT) || (type == TYP_LONG));

    const ValueNumKind vnKinds[]{VNK_Liberal, VNK_Conservative};
    for (ValueNumKind kind : vnKinds)
    {
        bool needDivideByZeroExc = true;
        bool needArithmeticExc   = isSigned;

        if (vnStore->IsVNConstant(vnp2.Get(kind)))
        {
            int64_t val = (type == TYP_INT) ? vnStore->ConstantValue<int32_t>(vnp2.Get(kind))
                                            : vnStore->ConstantValue<int64_t>(vnp2.Get(kind));

            if (val != 0)
            {
                needDivideByZeroExc = false;
            }

            if (isSigned && (val != -1))
            {
                needArithmeticExc = false;
            }
        }

        if (needArithmeticExc)
        {
            if (vnStore->IsVNConstant(vnp1.Get(kind)))
            {
                if ((type == TYP_INT) ? (vnStore->ConstantValue<int32_t>(vnp1.Get(kind)) != INT32_MIN)
                                      : (vnStore->ConstantValue<int64_t>(vnp1.Get(kind)) != INT64_MIN))
                {
                    needArithmeticExc = false;
                }
            }

            if (vnStore->IsVNConstant(vnp1.Get(kind)))
            {
                if ((type == TYP_INT) ? (vnStore->ConstantValue<int32_t>(vnp1.Get(kind)) != INT32_MIN)
                                      : (vnStore->ConstantValue<int64_t>(vnp1.Get(kind)) != INT64_MIN))
                {
                    needArithmeticExc = false;
                }
            }
        }

        if (needDivideByZeroExc)
        {
            ValueNum ex = vnStore->VNForFunc(TYP_REF, VNF_DivideByZeroExc, vnp2.Get(kind));
            exset.Set(kind, vnStore->ExsetUnion(exset.Get(kind), vnStore->ExsetCreate(ex)));
        }

        if (needArithmeticExc)
        {
            ValueNum ex = vnStore->VNForFunc(TYP_REF, VNF_ArithmeticExc, vnp1.Get(kind), vnp2.Get(kind));
            exset.Set(kind, vnStore->ExsetUnion(exset.Get(kind), vnStore->ExsetCreate(ex)));
        }
    }

    node->SetVNP(vnStore->PackExset(vnp, exset));
}

void ValueNumbering::AddOverflowExset(GenTreeOp* node)
{
    assert(node->OperIs(GT_ADD, GT_SUB, GT_MUL) && node->gtOverflow());

    VNFunc vnf = GetVNFuncForNode(node);
    assert(ValueNumStore::VNFuncIsOverflowArithmetic(vnf));

    ValueNumPair vnp = node->GetVNP();

    const ValueNumKind vnKinds[]{VNK_Liberal, VNK_Conservative};
    for (ValueNumKind kind : vnKinds)
    {
        ValueNum exset;
        ValueNum vn = vnStore->UnpackExset(vnp.Get(kind), &exset);

        // Don't add exceptions if the normal VN represents a constant.
        // We only fold to constant VNs for operations that provably cannot overflow.
        if (vnStore->IsVNConstant(vn))
        {
            continue;
        }

        // Don't add exceptions if the tree's normal VN has been derived from an identity.
        // This takes care of x + 0 == x, 0 + x == x, x - 0 == x, x * 1 == x, 1 * x == x.
        // The x - x == 0 and x * 0 == 0, 0 * x == 0 cases are handled by the "IsVNConstant" check above.
        if ((vnStore->ExtractValue(node->GetOp(0)->GetVN(kind)) == vn) ||
            (vnStore->ExtractValue(node->GetOp(1)->GetVN(kind))) == vn)
        {
            continue;
        }

        // The normal value number function should now be the same overflow checking ALU operation as 'vnf'.
        INDEBUG(VNFuncApp funcApp);
        assert(vnStore->GetVNFunc(vn, &funcApp) == vnf);

        ValueNum overflowExset = vnStore->ExsetCreate(vnStore->VNForFunc(TYP_REF, VNF_OverflowExc, vn));
        vnp.Set(kind, vnStore->PackExset(vn, vnStore->ExsetUnion(exset, overflowExset)));
    }

    node->SetVNP(vnp);
}

void ValueNumbering::NumberBoundsCheck(GenTreeBoundsChk* check)
{
    ValueNumPair indexExset;
    ValueNumPair indexVNP = vnStore->UnpackExset(check->GetIndex()->GetVNP(), &indexExset);
    ValueNumPair lengthExset;
    ValueNumPair lengthVNP    = vnStore->UnpackExset(check->GetIndex()->GetVNP(), &lengthExset);
    ValueNumPair outOfRangeEx = vnStore->VNPairForFunc(TYP_REF, VNF_IndexOutOfRangeExc, indexVNP, lengthVNP);

    ValueNumPair exset = vnStore->ExsetUnion(indexExset, lengthExset);
    exset              = vnStore->ExsetUnion(exset, vnStore->ExsetCreate(outOfRangeEx));

    check->SetVNP(vnStore->PackExset(ValueNumStore::VoidVNP(), exset));

    // Record non-constant value numbers that are used as the length argument to bounds checks,
    // so that assertion prop will know that comparisons against them are worth analyzing.
    // TODO-MIKE-Review: Shouldn't this extract the normal value from the conservative VN?
    ValueNum lengthVN = check->GetLength()->GetConservativeVN();
    if ((lengthVN != NoVN) && !vnStore->IsVNConstant(lengthVN))
    {
        vnStore->SetVNIsCheckedBound(lengthVN);
    }
}

void ValueNumbering::NumbeCkFinite(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_CKFINITE));

    ValueNumPair exset;
    ValueNumPair vnp = vnStore->UnpackExset(node->GetOp(0)->GetVNP(), &exset);
    ValueNumPair ex  = vnStore->VNPairForFunc(TYP_REF, VNF_ArithmeticExc, vnp);

    exset = vnStore->ExsetUnion(exset, vnStore->ExsetCreate(ex));
    node->SetVNP(vnStore->PackExset(vnp, exset));
}

#ifdef DEBUG

void ValueNumStore::Print(ValueNumPair vnp, unsigned level)
{
    if (vnp.BothEqual())
    {
        Print(vnp.GetLiberal(), level);
    }
    else
    {
        printf("<l:");
        Print(vnp.GetLiberal(), level);
        printf(", c:");
        Print(vnp.GetConservative(), level);
        printf(">");
    }
}

void ValueNumStore::Print(ValueNum vn, unsigned level)
{
    if (IsReservedVN(vn))
    {
        printf(GetReservedName(vn));
        return;
    }

    printf(FMT_VN, vn);

    if (level > 0)
    {
        Dump(vn);
    }
}

void ValueNumStore::Trace(ValueNum vn, const char* comment)
{
    if (compiler->verbose)
    {
        printf("    %s ", varTypeName(TypeOfVN(vn)));
        Print(vn, 1);

        if (comment != nullptr)
        {
            printf(" ; %s", comment);
        }

        printf("\n");
    }
}

void ValueNumStore::Trace(ValueNumPair vnp, const char* comment)
{
    if (compiler->verbose)
    {
        printf("    %s ", varTypeName(TypeOfVN(vnp.GetLiberal())));
        Print(vnp, 1);

        if (comment != nullptr)
        {
            printf(" ; %s", comment);
        }

        printf("\n");
    }
}

void ValueNumbering::TraceLocal(unsigned lclNum, ValueNumPair vnp, const char* comment)
{
    if (compiler->verbose)
    {
        printf("    V%02u = %s ", lclNum, varTypeName(vnStore->TypeOfVN(vnp.GetLiberal())));
        vnStore->Print(vnp, 1);

        if (comment != nullptr)
        {
            printf(" ; %s", comment);
        }

        printf("\n");
    }
}

void ValueNumbering::TraceMem(ValueNum vn, const char* comment)
{
    if (compiler->verbose)
    {
        printf("    Memory = %s ", varTypeName(vnStore->TypeOfVN(vn)));
        vnStore->Print(vn, 1);

        if (comment != nullptr)
        {
            printf(" ; %s", comment);
        }

        printf("\n");
    }
}

#endif // DEBUG

PhaseStatus SsaOptimizer::DoValueNumber()
{
    vnStore           = new (compiler->getAllocator(CMK_ValueNumber)) ValueNumStore(*this);
    compiler->vnStore = vnStore;

    ValueNumbering valueNumbering(*this);
    valueNumbering.Run();

    return PhaseStatus::MODIFIED_EVERYTHING;
}
