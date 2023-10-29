// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

// Defines the class "ValueNumStore", which maintains value numbers for a compilation.

// Recall that "value numbering" assigns an integer value number to each expression.  The "value
// number property" is that two expressions with the same value number will evaluate to the same value
// at runtime.  Expressions with different value numbers may or may not be equivalent.  This property
// of value numbers has obvious applications in redundancy-elimination optimizations.
//
// Since value numbers give us a way of talking about the (immutable) values to which expressions
// evaluate, they provide a good "handle" to use for attributing properties to values.  For example,
// we might note that some value number represents some particular integer constant -- which has obvious
// application to constant propagation.  Or that we know the exact type of some object reference,
// which might be used in devirtualization.
//
// Finally, we will also use value numbers to express control-flow-dependent assertions.  Some test may
// imply that after the test, something new is known about a value: that an object reference is non-null
// after a dereference (since control flow continued because no exception was thrown); that an integer value
// is restricted to some subrange in after a comparison test; etc.

#pragma once

#include "vartype.h"
#include "gentree.h"
#include "valuenumtype.h"
#include "smallhash.h"

enum VNFunc
{
#define GTNODE(n, s, k) VNOP_##n,
#include "gtlist.h"
    VNF_Boundary,
#define ValueNumFuncDef(nm, arity, commute, knownNonNull, sharedStatic) VNF_##nm,
#include "valuenumfuncs.h"
    VNF_Count,
    VNF_None = VNOP_NONE
};

static_assert_no_msg(static_cast<unsigned>(GT_NONE) == static_cast<unsigned>(VNOP_NONE));
static_assert_no_msg(static_cast<unsigned>(GT_COUNT) == static_cast<unsigned>(VNF_Boundary));

constexpr VNFunc VNFuncIndex(VNFunc vnf)
{
    return static_cast<VNFunc>(vnf & 0xFFFF);
}

#ifdef FEATURE_HW_INTRINSICS
constexpr VNFunc VNFuncHWIntrinsic(NamedIntrinsic intrinsic, var_types simdBaseType, unsigned simdSize)
{
    // TODO-MIKE-CQ: It may be useful to canonicalize the vector element type
    // somehow, as some SIMD operations are not affected by it (e.g. bitwise
    // operations). Old code sort of did this but apparently not for CQ reasons
    // but rather due to shoddy design. Removing it did not generate any diffs.
    // Such intrinsics are usually available for all element types so it's
    // unlikely that user code will reinterpret vectors in such a way that
    // we could see some benefit from canonicalization.
    return static_cast<VNFunc>((VNF_HWI_FIRST + (intrinsic - NI_HW_INTRINSIC_START - 1)) |
                               (static_cast<uint8_t>(simdBaseType) << 16) | (simdSize << 24));
}

inline VNFunc VNFuncHWIntrinsic(GenTreeHWIntrinsic* node)
{
    assert(node->GetAuxiliaryType() == TYP_UNDEF);
    return VNFuncHWIntrinsic(node->GetIntrinsic(), node->GetSimdBaseType(), node->GetSimdSize());
}

inline var_types VNFuncSimdBaseType(VNFunc vnf)
{
    assert(vnf >= VNF_HWI_FIRST);
    return static_cast<var_types>(vnf >> 16);
}

inline uint8_t VNFuncSimdSize(VNFunc vnf)
{
    assert(vnf >= VNF_HWI_FIRST);
    return static_cast<uint8_t>(vnf >> 24);
}
#endif // FEATURE_HW_INTRINSICS

// Given a GenTree node return the VNFunc that should be used when value numbering
VNFunc GetVNFuncForNode(GenTree* node);

// An instance of this struct represents an application of the function symbol
// "m_func" to the first "m_arity" (<= 4) argument values in "m_args."
struct VNFuncApp
{
    VNFunc   m_func;
    unsigned m_arity;
    ValueNum m_args[4];

    bool Is(genTreeOps oper) const
    {
        return m_func == static_cast<VNFunc>(oper);
    }

    bool Is(VNFunc func) const
    {
        return m_func == func;
    }

    template <typename... T>
    bool Is(genTreeOps oper, T... rest) const
    {
        return Is(oper) || Is(rest...);
    }

    template <typename... T>
    bool Is(VNFunc func, T... rest) const
    {
        return Is(func) || Is(rest...);
    }

    ValueNum operator[](unsigned i) const
    {
        assert(i < m_arity);
        return m_args[i];
    }
};

// We use a unique prefix character when printing value numbers in dumps:  i.e.  $1c0
// This define is used with string concatenation to put this in printf format strings
#define FMT_VN "$%x"

using NodeBlockMap = JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, BasicBlock*>;

class SsaOptimizer;

class ValueNumStore
{
    template <class Value, class KeyFuncs = JitLargePrimitiveKeyFuncs<Value>>
    class VNMap : public JitHashTable<Value, KeyFuncs, ValueNum>
    {
    public:
        VNMap(CompAllocator alloc) : JitHashTable<Value, KeyFuncs, ValueNum>(alloc)
        {
        }

        bool Set(Value value, ValueNum vn)
        {
            assert(vn != RecursiveVN);
            return JitHashTable<Value, KeyFuncs, ValueNum>::Set(value, vn);
        }

        bool Lookup(Value value, ValueNum* vn = nullptr) const
        {
            bool result = JitHashTable<Value, KeyFuncs, ValueNum>::Lookup(value, vn);
            assert(!result || *vn != RecursiveVN);
            return result;
        }
    };

    enum class ChunkKind : uint8_t
    {
        Const,     // This chunk contains constant values.
        Handle,    // This chunk contains handle constants.
        NotAField, // This chunk contains "not a field" values.
        Func0,     // Represents functions of arity 0.
        Func1,     // ...arity 1.
        Func2,     // ...arity 2.
        Func3,     // ...arity 3.
        Func4,     // ...arity 4.
    };

    using Func0VNMap = VNMap<VNFunc>;

    struct VNFuncDef0
    {
        static constexpr ChunkKind Kind  = ChunkKind::Func0;
        static constexpr unsigned  Arity = 0;

        VNFunc m_func;

        VNFuncDef0(VNFunc func) : m_func(func)
        {
        }

        bool operator==(const VNFuncDef0& y) const
        {
            return m_func == y.m_func;
        }
    };

    struct VNFuncDef1 : public VNFuncDef0
    {
        static constexpr ChunkKind Kind  = ChunkKind::Func1;
        static constexpr unsigned  Arity = 1;

        ValueNum m_arg0;

        VNFuncDef1(VNFunc func, ValueNum arg0) : VNFuncDef0(func), m_arg0(arg0)
        {
        }

        bool operator==(const VNFuncDef1& y) const
        {
            return VNFuncDef0::operator==(y) && m_arg0 == y.m_arg0;
        }
    };

    struct VNDefFunc1ArgKeyFuncs : public JitKeyFuncsDefEquals<VNFuncDef1>
    {
        static unsigned GetHashCode(VNFuncDef1 val)
        {
            return (val.m_func << 24) + val.m_arg0;
        }
    };

    using Func1VNMap = VNMap<VNFuncDef1, VNDefFunc1ArgKeyFuncs>;

    struct VNFuncDef2 : public VNFuncDef1
    {
        static constexpr ChunkKind Kind  = ChunkKind::Func2;
        static constexpr unsigned  Arity = 2;

        ValueNum m_arg1;

        VNFuncDef2(VNFunc func, ValueNum arg0, ValueNum arg1) : VNFuncDef1(func, arg0), m_arg1(arg1)
        {
        }

        bool operator==(const VNFuncDef2& y) const
        {
            return VNFuncDef1::operator==(y) && m_arg1 == y.m_arg1;
        }
    };

    struct VNDefFunc2ArgKeyFuncs : public JitKeyFuncsDefEquals<VNFuncDef2>
    {
        static unsigned GetHashCode(const VNFuncDef2& val)
        {
            return (val.m_func << 24) + (val.m_arg0 << 8) + val.m_arg1;
        }
    };

    using Func2VNMap = VNMap<VNFuncDef2, VNDefFunc2ArgKeyFuncs>;

    struct VNFuncDef3 : public VNFuncDef2
    {
        static constexpr ChunkKind Kind  = ChunkKind::Func3;
        static constexpr unsigned  Arity = 3;

        ValueNum m_arg2;

        VNFuncDef3(VNFunc func, ValueNum arg0, ValueNum arg1, ValueNum arg2)
            : VNFuncDef2(func, arg0, arg1), m_arg2(arg2)
        {
        }

        bool operator==(const VNFuncDef3& y) const
        {
            return VNFuncDef2::operator==(y) && m_arg2 == y.m_arg2;
        }
    };

    struct VNDefFunc3ArgKeyFuncs : public JitKeyFuncsDefEquals<VNFuncDef3>
    {
        static unsigned GetHashCode(const VNFuncDef3& val)
        {
            return (val.m_func << 24) + (val.m_arg0 << 16) + (val.m_arg1 << 8) + val.m_arg2;
        }
    };

    using Func3VNMap = VNMap<VNFuncDef3, VNDefFunc3ArgKeyFuncs>;

    struct VNFuncDef4 : public VNFuncDef3
    {
        static constexpr ChunkKind Kind  = ChunkKind::Func4;
        static constexpr unsigned  Arity = 4;

        ValueNum m_arg3;

        VNFuncDef4(VNFunc func, ValueNum arg0, ValueNum arg1, ValueNum arg2, ValueNum arg3)
            : VNFuncDef3(func, arg0, arg1, arg2), m_arg3(arg3)
        {
        }

        bool operator==(const VNFuncDef4& y) const
        {
            return VNFuncDef3::operator==(y) && m_arg3 == y.m_arg3;
        }
    };

    struct VNDefFunc4ArgKeyFuncs : public JitKeyFuncsDefEquals<VNFuncDef4>
    {
        static unsigned GetHashCode(const VNFuncDef4& val)
        {
            return (val.m_func << 24) + (val.m_arg0 << 16) + (val.m_arg1 << 8) + val.m_arg2 + (val.m_arg3 << 12);
        }
    };

    using Func4VNMap = VNMap<VNFuncDef4, VNDefFunc4ArgKeyFuncs>;

    using Int32VNMap = VNMap<int32_t>;
    using Int64VNMap = VNMap<int64_t>;
    using ByrefVNMap = VNMap<size_t>;

    // First, we need mechanisms for mapping from constants to value numbers.
    // For small integers, we'll use an array.
    static constexpr int      SmallIntConstMin = -1;
    static constexpr int      SmallIntConstMax = 10;
    static constexpr unsigned SmallIntConstNum = SmallIntConstMax - SmallIntConstMin + 1;

    struct VNHandle : public JitKeyFuncsDefEquals<VNHandle>
    {
        ssize_t    value;
        HandleKind kind;

        VNHandle(ssize_t value, HandleKind kind) : value(value), kind(kind)
        {
        }

        bool operator==(const VNHandle& y) const
        {
            return value == y.value && kind == y.kind;
        }

        static unsigned GetHashCode(const VNHandle& val)
        {
            return static_cast<unsigned>(val.value);
        }
    };

    using HandleVNMap = VNMap<VNHandle, VNHandle>;

    using CheckedBoundVNSet = SmallHashTable<ValueNum, bool, 8U>;

    // Convert a vartype_t to the value number's storage type for that vartype_t.
    // For example, ValueNum of type TYP_LONG are stored in a map of int64_t variables.
    // Lang is the language (C++) type for the corresponding vartype_t.
    template <int N>
    struct VarTypConv
    {
    };
    template <>
    struct VarTypConv<TYP_INT>
    {
        typedef int32_t Type;
        typedef int32_t Lang;
    };
    template <>
    struct VarTypConv<TYP_FLOAT>
    {
        typedef int32_t Type;
        typedef float   Lang;
    };
    template <>
    struct VarTypConv<TYP_LONG>
    {
        typedef int64_t Type;
        typedef int64_t Lang;
    };
    template <>
    struct VarTypConv<TYP_DOUBLE>
    {
        typedef int64_t Type;
        typedef double  Lang;
    };
    template <>
    struct VarTypConv<TYP_BYREF>
    {
        typedef size_t Type;
        typedef void*  Lang;
    };
    template <>
    struct VarTypConv<TYP_REF>
    {
        typedef class Object* Type;
        typedef class Object* Lang;
    };

    static constexpr unsigned MaxFuncArity = 4;

    struct Chunk
    {
        void*     m_defs  = nullptr;
        unsigned  m_count = 0;
        ValueNum  m_baseVN;
        var_types m_type;
        ChunkKind m_kind;

        Chunk(CompAllocator alloc, ValueNum* baseVN, var_types type, ChunkKind kind);

        unsigned AllocVN()
        {
            assert(m_count < ChunkSize);
            return m_baseVN + m_count++;
        }

        template <typename T>
        unsigned AllocVN(const T& value)
        {
            assert(m_count < ChunkSize);
            unsigned index                 = m_count++;
            static_cast<T*>(m_defs)[index] = value;
            return m_baseVN + index;
        }

        template <int N>
        struct Alloc
        {
            typedef typename VarTypConv<N>::Type Type;
        };
    };

    SsaOptimizer& ssa;
    Compiler*     compiler;
    CompAllocator alloc;
    BasicBlock*   m_currentBlock = nullptr;
    GenTree*      m_currentNode  = nullptr;

    // This map tracks nodes whose value numbers explicitly or implicitly depend on memory states.
    // The map provides the entry block of the most closely enclosing loop that
    // defines the memory region accessed when defining the nodes's VN.
    // This information should be consulted when considering hoisting node out of a loop, as the VN
    // for the node will only be valid within the indicated loop.
    // It is not fine-grained enough to track memory dependence within loops, so cannot be used
    // for more general code motion.
    // If a node does not have an entry in the map we currently assume the VN is not memory dependent
    // and so memory does not constrain hoisting.
    NodeBlockMap* m_nodeToLoopMemoryBlockMap = nullptr;
    // This is the maximum number of MapSelect terms that can be "considered" as part of evaluation of a top-level
    // MapSelect application.
    int m_mapSelectBudget;
    // The base VN of the next chunk to be allocated.  Should always be a multiple of ChunkSize.
    ValueNum m_nextChunkBase = 0;
    // When we evaluate "select(m, i)", if "m" is a the value of a phi definition, we look at
    // all the values of the phi args, and see if doing the "select" on each of them yields identical
    // results.  If so, that is the result of the entire "select" form.  We have to be careful, however,
    // because phis may be recursive in the presence of loop structures -- the VN for the phi may be (or be
    // part of the definition of) the VN's of some of the arguments.  But there will be at least one
    // argument that does *not* depend on the outer phi VN -- after all, we had to get into the loop somehow.
    // So we have to be careful about breaking infinite recursion.  We can ignore "recursive" results -- if all the
    // non-recursive results are the same, the recursion indicates that the loop structure didn't alter the result.
    // This stack represents the set of outer phis such that select(phi, ind) is being evaluated.
    ArrayStack<ValueNum, 1> m_fixedPointMapSels;
    // This is the set of value numbers that have been flagged as arguments to bounds checks, in the length position.
    CheckedBoundVNSet m_checkedBoundVNs;
    // This is a map from "chunk number" to the attributes of the chunk.
    ArrayStack<Chunk*, 8> m_chunks;
    // These entries indicate the current allocation chunk, if any, for each valid combination of <var_types,
    // ChunkKind>.
    // If the value is NoChunk, it indicates that there is no current allocation chunk for that pair, otherwise
    // it is the index in "m_chunks" of a chunk with the given attributes, in which the next allocation should
    // be attempted.
    unsigned m_currentInt32ConstChunk  = 0;
    unsigned m_currentInt64ConstChunk  = 0;
    unsigned m_currentFloatConstChunk  = 0;
    unsigned m_currentDoubleConstChunk = 0;
    unsigned m_currentByrefConstChunk  = 0;
    unsigned m_currentHandleChunk      = 0;
    unsigned m_currentNotAFieldChunk   = 0;
    unsigned m_currentFuncChunk[MaxFuncArity + 1][TYP_COUNT]{};

    ValueNum     m_zeroMap           = NoVN;
    ValueNum     m_readOnlyMemoryMap = NoVN;
    ValueNum     m_smallInt32VNMap[SmallIntConstNum];
    Int32VNMap*  m_int32VNMap  = nullptr;
    Int64VNMap*  m_int64VNMap  = nullptr;
    HandleVNMap* m_handleMap   = nullptr;
    Int32VNMap*  m_floatVNMap  = nullptr;
    Int64VNMap*  m_doubleVNMap = nullptr;
    ByrefVNMap*  m_byrefVNMap  = nullptr;
    Func0VNMap*  m_func0VNMap  = nullptr;
    Func1VNMap*  m_func1VNMap  = nullptr;
    Func2VNMap*  m_func2VNMap  = nullptr;
    Func3VNMap*  m_func3VNMap  = nullptr;
    Func4VNMap*  m_func4VNMap  = nullptr;

#ifdef DEBUG
    // This helps test some performance pathologies related to "evaluation" of VNF_MapSelect terms,
    // especially relating to GcHeap/ByrefExposed.  We count the number of applications of such terms we consider,
    // and if this exceeds a limit, indicated by a COMPlus_ variable, we assert.
    unsigned m_numMapSels = 0;

    JitHashTable<ValueNum, JitSmallPrimitiveKeyFuncs<ValueNum>, const char*> m_vnNameMap;
#endif

public:
    void SetCurrentBlock(BasicBlock* block)
    {
        m_currentBlock = block;
    }

#ifdef DEBUG
    BasicBlock* GetCurrentBlock() const
    {
        return m_currentBlock;
    }
#endif

    void SetCurrentNode(GenTree* node)
    {
        m_currentNode = node;
    }

    BasicBlock* GetLoopMemoryBlock(GenTree* node)
    {
        BasicBlock* block;
        return (m_nodeToLoopMemoryBlockMap != nullptr) && m_nodeToLoopMemoryBlockMap->Lookup(node, &block) ? block
                                                                                                           : nullptr;
    }

    void CopyLoopMemoryDependence(GenTree* fromNode, GenTree* toNode);

private:
    void RecordLoopMemoryDependence(GenTree* tree, BasicBlock* block, ValueNum memoryVN);

    // These enum constants are used to encode the cast operation in the lowest bits by VNForCastOper
    enum VNFCastAttrib
    {
        VCA_UnsignedSrc = 0x01,

        VCA_BitCount     = 1,    // the number of reserved bits
        VCA_ReservedBits = 0x01, // i.e. (VCA_UnsignedSrc)
    };

    static bool IsLegalVNFuncOper(genTreeOps gtOper);
    static bool VNFuncIsCommutative(VNFunc vnf);
    static bool VNFuncIsComparison(VNFunc vnf);

    // Returns "true" iff "vnf" can be evaluated for constant arguments.
    static bool CanEvalForConstantArgs(VNFunc vnf);

    // Returns "true" iff "vnf" should be folded by evaluating the func with constant arguments.
    bool VNEvalShouldFold(var_types typ, VNFunc func, ValueNum arg0VN, ValueNum arg1VN);

public:
    // Given an constant value number return its value.
    int GetConstantInt32(ValueNum argVN);
    int64_t GetConstantInt64(ValueNum argVN);
    double GetConstantDouble(ValueNum argVN);
    float GetConstantSingle(ValueNum argVN);

private:
    // Assumes that all the ValueNum arguments of each of these functions have been shown to represent constants.
    // Assumes that "vnf" is a operator of the appropriate arity (unary for the first, binary for the second).
    // Assume that "CanEvalForConstantArgs(vnf)" is true.
    // Returns the result of evaluating the function with those constant arguments.
    ValueNum EvalFuncForConstantArgs(var_types typ, VNFunc vnf, ValueNum vn0);
    ValueNum EvalFuncForConstantArgs(var_types typ, VNFunc vnf, ValueNum vn0, ValueNum vn1);
    ValueNum EvalFuncForConstantFPArgs(var_types typ, VNFunc vnf, ValueNum vn0, ValueNum vn1);
    ValueNum EvalCastForConstantArgs(var_types type, VNFunc vnf, ValueNum valVN, ValueNum typeVN);

    ValueNum EvalUsingMathIdentity(var_types typ, VNFunc vnf, ValueNum vn0, ValueNum vn1);

    template <typename T, typename NumMap>
    inline ValueNum VnForConst(T cnsVal, NumMap* numMap, var_types varType, unsigned& currentChunk);

public:
    ValueNumStore(SsaOptimizer& ssa);

    static const struct VNFuncAttrs& VNFuncAttrs(VNFunc vnf);
    static bool VNFuncIsLegal(VNFunc vnf);
    static bool VNFuncIsOverflowArithmetic(VNFunc vnf);
    static bool VNFuncIsNumericCast(VNFunc vnf);
    static unsigned VNFuncArity(VNFunc vnf);
    static bool VNFuncArityIsLegal(VNFunc vnf, unsigned arity);
    static bool VNFuncArityIsVariable(VNFunc vnf);
    static VNFunc GenTreeOpToVNFunc(genTreeOps oper);

    INDEBUG(static void RunTests(Compiler* comp);)

    ValueNum VNForIntCon(int32_t value);
    ValueNum VNForLongCon(int64_t value);
    ValueNum VNForFloatCon(float value);
    ValueNum VNForDoubleCon(double value);
    ValueNum VNForDblCon(var_types type, double value);
    ValueNum VNForByrefCon(target_size_t value);

#ifdef TARGET_64BIT
    ValueNum VNForPtrSizeIntCon(int64_t cnsVal)
    {
        return VNForLongCon(cnsVal);
    }
#else
    ValueNum VNForPtrSizeIntCon(int32_t cnsVal)
    {
        return VNForIntCon(cnsVal);
    }
#endif

    ValueNum VNForHostPtr(void* p)
    {
#ifdef HOST_64BIT
        return VNForLongCon(reinterpret_cast<int64_t>(p));
#else
        return VNForIntCon(reinterpret_cast<int32_t>(p));
#endif
    }

    ValueNum VNForUPtrSizeIntCon(target_size_t value)
    {
#ifdef TARGET_64BIT
        return VNForLongCon(static_cast<int64_t>(value));
#else
        return VNForIntCon(static_cast<int32_t>(value));
#endif
    }

    ValueNum VNForBitCastOper(var_types castToType);

    // Packs information about the cast into an integer constant represented by the returned value number,
    // to be used as the second operand of VNF_Cast & VNF_CastOvf.
    ValueNum VNForCastOper(var_types castToType, bool castFromUnsigned = false);

    // Unpacks the information stored by VNForCastOper in the constant represented by the value number.
    void GetCastOperFromVN(ValueNum vn, var_types* pCastToType, bool* pSrcIsUnsigned);

    // We keep handle values in a separate pool, so we don't confuse a handle with an int constant
    // that happens to be the same...
    ValueNum VNForHandle(ssize_t value, HandleKind handleKind);

    ValueNum VNForFieldSeqHandle(CORINFO_FIELD_HANDLE fieldHandle);

    ValueNum VNForTypeNum(unsigned typeNum);

    // The zero map is the map that returns a zero "for the appropriate type" when indexed at any index.
    ValueNum ZeroMapVN();

    // The ROH map is the map for "read-only memory".  We assume that this is never mutated, and always
    // has the same value number.
    ValueNum ReadOnlyMemoryMapVN();

    static ValueNum NullVN()
    {
        return ValueNum(SRC_Null);
    }

    static ValueNum VoidVN()
    {
        return ValueNum(SRC_Void);
    }

    static ValueNumPair VoidVNP()
    {
        return {VoidVN(), VoidVN()};
    }

    static ValueNum EmptyExsetVN()
    {
        return ValueNum(SRC_EmptyExset);
    }

    static ValueNumPair EmptyExsetVNP()
    {
        return {EmptyExsetVN(), EmptyExsetVN()};
    }

    // Returns the value number for zero of the given "typ".
    // It has an unreached() for a "typ" that has no zero value, such as TYP_VOID.
    ValueNum VNZeroForType(var_types typ);

    // Returns the value number for one of the given "typ".
    // It returns NoVN for a "typ" that has no one value, such as TYP_REF.
    ValueNum VNOneForType(var_types typ);

    ValueNum ExsetCreate(ValueNum x);
    ValueNumPair ExsetCreate(ValueNumPair x);
    INDEBUG(bool ExsetIsOrdered(ValueNum item, ValueNum xs1);)
    ValueNum ExsetUnion(ValueNum xs0, ValueNum xs1);
    ValueNumPair ExsetUnion(ValueNumPair xs0vnp, ValueNumPair xs1vnp);
    ValueNum ExsetIntersection(ValueNum xs0, ValueNum xs1);
    ValueNumPair ExsetIntersection(ValueNumPair xs0vnp, ValueNumPair xs1vnp);
    bool ExsetIsSubset(ValueNum vnCandidateSet, ValueNum vnFullSet);
    ValueNum PackExset(ValueNum vn, ValueNum exset);
    ValueNumPair PackExset(ValueNumPair vnp, ValueNumPair exset);
    ValueNum UnpackExset(ValueNum vn, ValueNum* exset);
    ValueNumPair UnpackExset(ValueNumPair vnp, ValueNumPair* exset);
    ValueNum ExtractValue(ValueNum vn) const;
    ValueNumPair ExtractValue(ValueNumPair vnp);
    ValueNum ExtractExset(ValueNum vn);
    ValueNumPair ExtractExset(ValueNumPair vn);
    INDEBUG(bool HasExset(ValueNum vn) const;)

    // True "iff" vn is a value known to be non-null.  (For example, the result of an allocation...)
    bool IsKnownNonNull(ValueNum vn);

    ValueNum VNForFunc(var_types type, VNFunc func);
    ValueNum VNForFunc(var_types type, VNFunc func, ValueNum arg0);
    ValueNum HasFunc(var_types type, VNFunc func, ValueNum arg0);
    ValueNum VNForFunc(var_types type, VNFunc func, ValueNum arg0, ValueNum arg1);
    ValueNum VNForFunc(var_types type, VNFunc func, ValueNum arg0, ValueNum arg1, ValueNum arg2);
    ValueNum VNForFunc(var_types type, VNFunc func, ValueNum arg0, ValueNum arg1, ValueNum arg2, ValueNum arg3);

    // This requires a "ValueNumKind" because it will attempt, given "select(phi(m1, ..., mk), ind)", to evaluate
    // "select(m1, ind)", ..., "select(mk, ind)" to see if they agree.  It needs to know which kind of value number
    // (liberal/conservative) to read from the SSA def referenced in the phi argument.
    ValueNum VNForMapSelect(ValueNumKind vnk, var_types typ, ValueNum op1VN, ValueNum op2VN);
    ValueNumPair VNForMapSelect(var_types type, ValueNumPair map, ValueNumPair index);

    // A method that does the work for VNForMapSelect and may call itself recursively.
    ValueNum VNForMapSelectWork(
        ValueNumKind vnk, var_types typ, ValueNum op1VN, ValueNum op2VN, int* pBudget, bool* pUsedRecursiveVN);

    // A specialized version of VNForFunc that is used for VNF_MapStore and provides some logging when verbose is set
    ValueNum VNForMapStore(var_types typ, ValueNum arg0VN, ValueNum arg1VN, ValueNum arg2VN);

    // These functions parallel the ones above, except that they take liberal/conservative VN pairs
    // as arguments, and return such a pair (the pair of the function applied to the liberal args, and
    // the function applied to the conservative args).
    ValueNumPair VNPairForFunc(var_types typ, VNFunc func)
    {
        ValueNumPair res;
        res.SetBoth(VNForFunc(typ, func));
        return res;
    }
    ValueNumPair VNPairForFunc(var_types typ, VNFunc func, ValueNumPair opVN)
    {
        return {VNForFunc(typ, func, opVN.GetLiberal()), VNForFunc(typ, func, opVN.GetConservative())};
    }
    ValueNumPair VNPairForFunc(var_types typ, VNFunc func, ValueNumPair op1VN, ValueNumPair op2VN)
    {
        return {VNForFunc(typ, func, op1VN.GetLiberal(), op2VN.GetLiberal()),
                VNForFunc(typ, func, op1VN.GetConservative(), op2VN.GetConservative())};
    }
    ValueNumPair VNPairForFunc(var_types typ, VNFunc func, ValueNumPair op1VN, ValueNumPair op2VN, ValueNumPair op3VN)
    {
        return {VNForFunc(typ, func, op1VN.GetLiberal(), op2VN.GetLiberal(), op3VN.GetLiberal()),
                VNForFunc(typ, func, op1VN.GetConservative(), op2VN.GetConservative(), op3VN.GetConservative())};
    }
    ValueNumPair VNPairForFunc(
        var_types typ, VNFunc func, ValueNumPair op1VN, ValueNumPair op2VN, ValueNumPair op3VN, ValueNumPair op4VN)
    {
        return {VNForFunc(typ, func, op1VN.GetLiberal(), op2VN.GetLiberal(), op3VN.GetLiberal(), op4VN.GetLiberal()),
                VNForFunc(typ, func, op1VN.GetConservative(), op2VN.GetConservative(), op3VN.GetConservative(),
                          op4VN.GetConservative())};
    }

    // Get a new, unique value number for an expression that we're not equating to some function,
    // which is the value of a tree in the given block.
    ValueNum VNForExpr(BasicBlock* block, var_types typ);
    ValueNum VNForExpr(var_types typ);
    ValueNum UniqueVN(var_types type);

    ValueNum VNForBitCast(ValueNum valueVN, var_types toType);

    ValueNum VNForCast(ValueNum valueVN, var_types toType);
    ValueNumPair VNForCast(ValueNumPair valueVNP, var_types toType);

    // PtrToLoc values need to express a field sequence as one of their arguments.  VN for null represents
    // empty sequence, otherwise, "FieldSeq(VN(FieldHandle), restOfSeq)".
    ValueNum VNForFieldSeq(FieldSeqNode* fieldSeq);

    // Requires that "vn" represents a field sequence, that is, is the result of a call to VNForFieldSeq.
    // Returns the FieldSequence it represents.
    FieldSeqNode* FieldSeqVNToFieldSeq(ValueNum vn);

    ValueNum FieldSeqVNAppend(ValueNum fieldSeqVN, FieldSeqNode* fieldSeq);

    ValueNum ExtendPtrVN(ValueNum addrVN, FieldSeqNode* fieldSeq, target_size_t offset);

    ValueNum ExtractArrayElementIndex(const struct ArrayInfo& arrayInfo);

    // Queries on value numbers.
    // All queries taking value numbers require that those value numbers are valid, that is, that
    // they have been returned by previous "VNFor..." operations.  They can assert false if this is
    // not true.

    var_types TypeOfVN(ValueNum vn) const;

    // Returns MaxLoopNum if the given value number's loop nest is unknown or ill-defined.
    LoopNum LoopOfVN(ValueNum vn);

    // Returns true iff the VN represents a (non-handle) constant.
    var_types GetConstantType(ValueNum vn) const;
    bool IsVNConstant(ValueNum vn) const;
    bool IsVNInt32Constant(ValueNum vn) const;
    bool IsIntegralConstant(ValueNum vn, ssize_t* value) const;

    // Returns true if the VN is known or likely to appear as the conservative value number
    // of the length argument to a GT_BOUNDS_CHECK node.
    bool IsVNCheckedBound(ValueNum vn);

    // Record that a VN is known to appear as the conservative value number of the length
    // argument to a GT_BOUNDS_CHECK node.
    void SetVNIsCheckedBound(ValueNum vn);

    struct CompareCheckedBoundArithInfo
    {
        // (vnBound - 1) > vnOp
        // (vnBound arrOper arrOp) cmpOper cmpOp
        ValueNum   vnBound = NoVN;
        genTreeOps arrOper = GT_NONE;
        ValueNum   arrOp   = NoVN;
        genTreeOps cmpOper = GT_NONE;
        ValueNum   cmpOp   = NoVN;

#ifdef DEBUG
        void Dump()
        {
            if (arrOper == GT_NONE)
            {
                printf("%s(" FMT_VN ", " FMT_VN ")", GenTree::OpName(cmpOper), cmpOp, vnBound);
            }
            else
            {
                printf("%s(" FMT_VN ", %s(" FMT_VN ", " FMT_VN "))", GenTree::OpName(cmpOper), cmpOp,
                       GenTree::OpName(arrOper), vnBound, arrOp);
            }
        }
#endif
    };

    static bool IsVNCompareCheckedBoundRelop(const VNFuncApp& funcApp)
    {
        return funcApp.Is(GT_LE, GT_GE, GT_LT, GT_GT);
    }

    // If "vn" is of the form "var < len" or "len <= var" return true.
    bool IsVNCompareCheckedBound(const VNFuncApp& funcApp);

    // If "vn" is checked bound, then populate the "info" fields for the boundVn, cmpOp, cmpOper.
    void GetCompareCheckedBound(const VNFuncApp& funcApp, CompareCheckedBoundArithInfo* info);

    // If "vn" is of the form "len +/- var" return true.
    bool IsVNCheckedBoundArith(const VNFuncApp& funcApp);

    // If "vn" is of the form "var < len +/- k" return true.
    bool IsVNCompareCheckedBoundArith(const VNFuncApp& funcApp);

    // If "vn" is checked bound arith, then populate the "info" fields for cmpOp, cmpOper.
    void GetCompareCheckedBoundArithInfo(const VNFuncApp& funcApp, CompareCheckedBoundArithInfo* info);

    HandleKind GetHandleKind(ValueNum vn) const;

    // Returns true iff the VN represents a handle constant.
    bool IsVNHandle(ValueNum vn) const;

private:
    template <typename T>
    T ConstantValueInternal(ValueNum vn DEBUGARG(bool coerce)) const
    {
        Chunk* c = m_chunks.Get(GetChunkNum(vn));
        assert(c->m_kind == ChunkKind::Const || c->m_kind == ChunkKind::Handle);

        unsigned offset = ChunkOffset(vn);

        switch (c->m_type)
        {
            case TYP_REF:
                assert(0 <= offset && offset <= 1); // Null or exception.
                FALLTHROUGH;
            case TYP_BYREF:
#ifdef _MSC_VER
                assert(&typeid(T) == &typeid(size_t)); // We represent ref/byref constants as size_t's.
#endif

                FALLTHROUGH;
            case TYP_INT:
            case TYP_LONG:
            case TYP_FLOAT:
            case TYP_DOUBLE:
                if (c->m_kind == ChunkKind::Handle)
                {
                    C_ASSERT(offsetof(VNHandle, value) == 0);
                    return (T) static_cast<VNHandle*>(c->m_defs)[offset].value;
                }

#ifdef DEBUG
                if (!coerce)
                {
                    T val1 = static_cast<T*>(c->m_defs)[offset];
                    T val2 = SafeGetConstantValue<T>(c, offset);

                    // Detect if there is a mismatch between the VN storage type and explicitly
                    // passed-in type T.
                    bool mismatch = false;
                    if (varTypeIsFloating(c->m_type))
                    {
                        mismatch = (memcmp(&val1, &val2, sizeof(val1)) != 0);
                    }
                    else
                    {
                        mismatch = (val1 != val2);
                    }

                    if (mismatch)
                    {
                        assert(
                            !"Called ConstantValue<T>(vn), but type(T) != type(vn); Use CoercedConstantValue instead.");
                    }
                }
#endif

                return SafeGetConstantValue<T>(c, offset);

            default:
                assert(false); // We do not record constants of this typ.
                return (T)0;
        }
    }

public:
    template <typename T>
    T* ConstantHostPtr(ValueNum vn)
    {
#ifdef HOST_64BIT
        return reinterpret_cast<T*>(ConstantValue<int64_t>(vn));
#else
        return reinterpret_cast<T*>(ConstantValue<int32_t>(vn));
#endif
    }

    // Requires that "vn" is a constant, and that its type is compatible with the explicitly passed
    // type "T". Also, note that "T" has to have an accurate storage size of the TypeOfVN(vn).
    template <typename T>
    T ConstantValue(ValueNum vn) const
    {
        return ConstantValueInternal<T>(vn DEBUGARG(false));
    }

    // Requires that "vn" is a constant, and that its type can be coerced to the explicitly passed
    // type "T".
    template <typename T>
    T CoercedConstantValue(ValueNum vn) const
    {
        return ConstantValueInternal<T>(vn DEBUGARG(true));
    }

    // Requires "mthFunc" to be an intrinsic math function (one of the allowable values for the "gtMath" field
    // of a GenTreeMath node).  For unary ops, return the value number for the application of this function to
    // "arg0VN". For binary ops, return the value number for the application of this function to "arg0VN" and
    // "arg1VN".

    ValueNum EvalMathFuncUnary(var_types typ, NamedIntrinsic mthFunc, ValueNum arg0VN);

    ValueNum EvalMathFuncBinary(var_types typ, NamedIntrinsic mthFunc, ValueNum arg0VN, ValueNum arg1VN);

    ValueNumPair EvalMathFuncUnary(var_types typ, NamedIntrinsic mthFunc, ValueNumPair arg0VNP)
    {
        return {EvalMathFuncUnary(typ, mthFunc, arg0VNP.GetLiberal()),
                EvalMathFuncUnary(typ, mthFunc, arg0VNP.GetConservative())};
    }

    ValueNumPair EvalMathFuncBinary(var_types typ, NamedIntrinsic mthFunc, ValueNumPair arg0VNP, ValueNumPair arg1VNP)
    {
        return {EvalMathFuncBinary(typ, mthFunc, arg0VNP.GetLiberal(), arg1VNP.GetLiberal()),
                EvalMathFuncBinary(typ, mthFunc, arg0VNP.GetConservative(), arg1VNP.GetConservative())};
    }

    // If "vn" represents a function application, returns "true" and set "*funcApp" to
    // the function application it represents; otherwise, return "false."
    VNFunc GetVNFunc(ValueNum vn, VNFuncApp* funcApp) const;

    template <typename T>
    const T* IsVNFunc(ValueNum vn, VNFunc func) const
    {
        if (vn == NoVN)
        {
            return nullptr;
        }

        Chunk*   chunk = m_chunks.Get(GetChunkNum(vn));
        unsigned index = ChunkOffset(vn);
        assert(index < chunk->m_count);

        if (chunk->m_kind != T::Kind)
        {
            return nullptr;
        }

        const T* def = &static_cast<T*>(chunk->m_defs)[index];
        return def->m_func == func ? def : nullptr;
    }

#ifdef DEBUG
    void Dump(ValueNum vn, bool isPtr = false);
    void DumpFieldSeq(const VNFuncApp& fieldSeq, bool isHead);
    void DumpMapSelect(const VNFuncApp& mapSelect);
    void DumpMapStore(const VNFuncApp& mapStore);
    void DumpMemOpaque(const VNFuncApp& memOpaque);
    void DumpValWithExc(const VNFuncApp& valWithExc);
    void DumpLclAddr(const VNFuncApp& lclAddr);
    void DumpBitCast(const VNFuncApp& cast);
    void DumpCast(const VNFuncApp& cast);
    void DumpPtrToArrElem(const VNFuncApp& elemAddr);
    void DumpExcSeq(const VNFuncApp& excSeq, bool isHead);

    static const char* GetFuncName(VNFunc vnf);
    static const char* GetReservedName(ValueNum vn);

    void Trace(ValueNum vn, const char* comment = nullptr);
    void Trace(ValueNumPair vnp, const char* comment = nullptr);
    void Print(ValueNumPair vnp, unsigned level);
    void Print(ValueNum vn, unsigned level);
#endif // DEBUG

    // Returns true if "vn" is a reserved value number
    static bool IsReservedVN(ValueNum);

private:
    // We will allocate value numbers in "chunks".  Each chunk will have the same type and "constness".
    static constexpr unsigned LogChunkSize    = 6;
    static constexpr unsigned ChunkSize       = 1 << LogChunkSize;
    static constexpr unsigned ChunkOffsetMask = ChunkSize - 1;

    // Returns the ChunkNum of the Chunk that holds "vn" (which is required to be a valid
    // value number, i.e., one returned by some VN-producing method of this class).
    static unsigned GetChunkNum(ValueNum vn)
    {
        return vn >> LogChunkSize;
    }

    // Returns the offset of the given "vn" within its chunk.
    static unsigned ChunkOffset(ValueNum vn)
    {
        return vn & ChunkOffsetMask;
    }

    // Returns a (pointer to a) chunk in which a new value number may be allocated.
    Chunk* GetAllocChunk(var_types type, ChunkKind kind);
    Chunk* GetAllocChunk(var_types type, ChunkKind kind, unsigned& current);

    static bool IsSmallIntConst(int i)
    {
        return SmallIntConstMin <= i && i <= SmallIntConstMax;
    }

    enum SpecialRefConsts
    {
        SRC_Null,
        SRC_Void,
        SRC_EmptyExset,
        SRC_NumSpecialRefConsts
    };

    template <typename T>
    T CoerceTypRefToT(Chunk* c, unsigned offset) const
    {
        noway_assert(sizeof(T) >= sizeof(VarTypConv<TYP_REF>::Type));
        unreached();
    }

    template <>
    size_t CoerceTypRefToT(Chunk* c, unsigned offset) const
    {
        return reinterpret_cast<size_t>(static_cast<VarTypConv<TYP_REF>::Type*>(c->m_defs)[offset]);
    }

    // Get the actual value and coerce the actual type c->m_typ to the wanted type T.
    template <typename T>
    FORCEINLINE T SafeGetConstantValue(Chunk* c, unsigned offset) const
    {
        switch (c->m_type)
        {
            case TYP_REF:
                return CoerceTypRefToT<T>(c, offset);
            case TYP_BYREF:
                return static_cast<T>(static_cast<VarTypConv<TYP_BYREF>::Type*>(c->m_defs)[offset]);
            case TYP_INT:
                return static_cast<T>(static_cast<VarTypConv<TYP_INT>::Type*>(c->m_defs)[offset]);
            case TYP_LONG:
                return static_cast<T>(static_cast<VarTypConv<TYP_LONG>::Type*>(c->m_defs)[offset]);
            case TYP_FLOAT:
                return static_cast<T>(static_cast<VarTypConv<TYP_FLOAT>::Lang*>(c->m_defs)[offset]);
            case TYP_DOUBLE:
                return static_cast<T>(static_cast<VarTypConv<TYP_DOUBLE>::Lang*>(c->m_defs)[offset]);
            default:
                assert(false);
                return (T)0;
        }
    }
};
