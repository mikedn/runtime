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
    VNOP_COUNT,
#define ValueNumFuncDef(nm, arity, commute, knownNonNull, sharedStatic) VNF_##nm,
#include "valuenumfuncs.h"
    VNF_Count,
    VNF_None = VNOP_NONE
};

static_assert_no_msg(static_cast<unsigned>(GT_NONE) == static_cast<unsigned>(VNOP_NONE));
static_assert_no_msg(static_cast<unsigned>(GT_COUNT) == static_cast<unsigned>(VNOP_COUNT));

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
    return static_cast<VNFunc>((VNF_HWI_FIRST + (intrinsic - NI_HW_INTRINSIC_FIRST)) |
                               (static_cast<uint8_t>(simdBaseType) << 16) | (simdSize << 24));
}

inline VNFunc VNFuncHWIntrinsic(GenTreeHWIntrinsic* node)
{
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

VNFunc GetRelopVNFunc(GenTreeOp* node);
VNFunc SwapRelopVNFunc(VNFunc cond);
VNFunc ReverseRelopVNFunc(VNFunc cond);

// An instance of this struct represents an application of the function symbol
// "m_func" to the first "m_arity" (<= 4) argument values in "m_args."
struct VNFuncApp
{
    VNFunc   m_func;
    unsigned m_arity;
    ValueNum m_args[4];

    bool Is(VNFunc func) const
    {
        return m_func == func;
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

struct VNHandle : public JitKeyFuncsDefEquals<VNHandle>
{
    void*      addr;
    HandleKind kind;

    VNHandle(void* addr, HandleKind kind) : addr(addr), kind(kind)
    {
    }

    bool operator==(const VNHandle& y) const
    {
        return addr == y.addr && kind == y.kind;
    }

    static unsigned GetHashCode(const VNHandle& val)
    {
        return static_cast<unsigned>(reinterpret_cast<size_t>(val.addr));
    }
};

// We use a unique prefix character when printing value numbers in dumps:  i.e.  $1c0
// This define is used with string concatenation to put this in printf format strings
#define FMT_VN "$%x"

using NodeBlockMap = JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, BasicBlock*>;

class SsaOptimizer;
class ValueNumbering;

class ValueNumStore
{
    friend class ValueNumbering;

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
    using ByrefVNMap = VNMap<target_ssize_t>;

    // First, we need mechanisms for mapping from constants to value numbers.
    // For small integers, we'll use an array.
    static constexpr int      SmallIntConstMin = -1;
    static constexpr int      SmallIntConstMax = 10;
    static constexpr unsigned SmallIntConstNum = SmallIntConstMax - SmallIntConstMin + 1;

    using HandleVNMap = VNMap<VNHandle, VNHandle>;

    using CheckedBoundVNSet = SmallHashTable<ValueNum, bool, 8U>;

    // Convert a vartype_t to the value number's storage type for that vartype_t.
    // For example, ValueNum of type TYP_LONG are stored in a map of int64_t variables.
    template <int N>
    struct VarTypConv
    {
    };
    template <>
    struct VarTypConv<TYP_INT>
    {
        typedef int32_t Type;
    };
    template <>
    struct VarTypConv<TYP_FLOAT>
    {
        typedef int32_t Type;
    };
    template <>
    struct VarTypConv<TYP_LONG>
    {
        typedef int64_t Type;
    };
    template <>
    struct VarTypConv<TYP_DOUBLE>
    {
        typedef int64_t Type;
    };
    template <>
    struct VarTypConv<TYP_BYREF>
    {
        typedef target_ssize_t Type;
    };
    template <>
    struct VarTypConv<TYP_REF>
    {
        typedef target_ssize_t Type;
    };

    static constexpr unsigned MaxFuncArity = 4;

    struct Chunk
    {
        void*     m_defs  = nullptr;
        unsigned  m_count = 0;
        ValueNum  m_baseVN;
        var_types m_type;
        ChunkKind m_kind;

        Chunk(CompAllocator alloc, ValueNum* nextBaseVN, var_types type, ChunkKind kind);

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

    ValueNum     m_zeroMap = NoVN;
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

    static bool IsLegalVNFuncOper(genTreeOps oper);
    static bool VNFuncIsComparison(VNFunc vnf);

    static bool CanEvalForConstantArgs1(VNFunc vnf);
    static bool CanEvalForConstantArgs2(VNFunc vnf);

public:
    static bool VNFuncIsCommutative(VNFunc vnf);

    bool IsVNConstant(ValueNum vn) const;
    const VNHandle* IsHandle(ValueNum vn) const;
    var_types GetConstType(ValueNum vn) const;
    bool IsConst(ValueNum vn) const;
    const int32_t* IsConstInt32(ValueNum vn) const;
    int32_t GetConstInt32(ValueNum vn) const;
    const int64_t* IsConstInt64(ValueNum vn) const;
    bool IsConstInt64(ValueNum vn, int64_t* value) const;
    int64_t GetConstInt64(ValueNum vn) const;
    var_types IsConstSize(ValueNum vn, ssize_t* value) const;
    var_types IsConstInt(ValueNum vn, int64_t* value) const;
    int64_t GetConstInt(ValueNum vn) const;
    const target_ssize_t* IsConstIntN(ValueNum vn) const;
    target_ssize_t GetConstIntN(ValueNum vn) const;
    target_ssize_t GetConstByRef(ValueNum vn) const;
    double GetConstDouble(ValueNum vn) const;
    float GetConstFloat(ValueNum vn) const;

    template <typename T>
    T* ConstantHostPtr(ValueNum vn)
    {
#ifdef HOST_64BIT
        return reinterpret_cast<T*>(GetConstInt64(vn));
#else
        return reinterpret_cast<T*>(GetConstInt32(vn));
#endif
    }

private:
    // Assumes that all the ValueNum arguments of each of these functions have been shown to represent constants.
    // Assumes that "vnf" is a operator of the appropriate arity (unary for the first, binary for the second).
    // Assume that "CanEvalForConstantArgs(vnf)" is true.
    // Returns the result of evaluating the function with those constant arguments.
    ValueNum EvalFuncForConstantArgs(var_types type, VNFunc vnf, ValueNum vn0);
    ValueNum EvalFuncForConstantArgs(var_types type, VNFunc vnf, ValueNum vn0, ValueNum vn1);
    ValueNum EvalFloatFunc(var_types resultType, VNFunc vnf, var_types type, ValueNum vn0, ValueNum vn1);

    ValueNum EvalUsingMathIdentity(var_types type, VNFunc vnf, ValueNum vn0, ValueNum vn1);

    template <typename T, typename NumMap>
    inline ValueNum VnForConst(T cnsVal, NumMap* numMap, var_types varType, unsigned& currentChunk);

public:
    ValueNumStore(SsaOptimizer& ssa);

    static const struct VNFuncAttrs& VNFuncAttrs(VNFunc vnf);
    static bool VNFuncIsLegal(VNFunc vnf);
    static unsigned VNFuncArity(VNFunc vnf);
    static bool VNFuncArityIsLegal(VNFunc vnf, unsigned arity);
    static bool VNFuncArityIsVariable(VNFunc vnf);
    static VNFunc GenTreeOpToVNFunc(genTreeOps oper);

    INDEBUG(static void RunTests(Compiler* comp);)

    ValueNum VNForIntCon(int32_t value);
    ValueNum VNForLongCon(int64_t value);
    ValueNum VNForIntCon(var_types type, ssize_t value);
    ValueNum VNForFloatCon(float value);
    ValueNum VNForDoubleCon(double value);
    ValueNum VNForDblCon(var_types type, double value);
    ValueNum VNForByrefCon(ssize_t value);

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

    // We keep handle values in a separate pool, so we don't confuse a handle with an int constant
    // that happens to be the same...
    ValueNum VNForHandle(void* addr, HandleKind handleKind);

    ValueNum VNForFieldSeqHandle(CORINFO_FIELD_HANDLE fieldHandle);

    ValueNum VNForTypeNum(unsigned typeNum);

    // The zero map is the map that returns a zero "for the appropriate type" when indexed at any index.
    ValueNum ZeroMapVN();

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

    ValueNum VNZeroForType(var_types type);
    ValueNum VNOneForType(var_types type);

    ValueNum ExsetCreate(ValueNum x);
    ValueNumPair ExsetCreate(ValueNumPair x);
    INDEBUG(bool ExsetIsOrdered(ValueNum item, ValueNum xs1) const;)
    ValueNum ExsetUnion(ValueNum xs0, ValueNum xs1);
    ValueNumPair ExsetUnion(ValueNumPair xs0vnp, ValueNumPair xs1vnp);
    ValueNum ExsetIntersection(ValueNum xs0, ValueNum xs1);
    ValueNumPair ExsetIntersection(ValueNumPair xs0vnp, ValueNumPair xs1vnp);
    bool ExsetIsSubset(ValueNum vnCandidateSet, ValueNum vnFullSet) const;
    ValueNum PackExset(ValueNum vn, ValueNum exset);
    ValueNumPair PackExset(ValueNumPair vnp, ValueNumPair exset);
    ValueNum UnpackExset(ValueNum vn, ValueNum* exset) const;
    ValueNumPair UnpackExset(ValueNumPair vnp, ValueNumPair* exset) const;
    ValueNum ExtractValue(ValueNum vn) const;
    ValueNumPair ExtractValue(ValueNumPair vnp) const;
    ValueNum ExtractExset(ValueNum vn) const;
    ValueNumPair ExtractExset(ValueNumPair vn) const;
    INDEBUG(bool HasExset(ValueNum vn) const;)

    // True "iff" vn is a value known to be non-null.  (For example, the result of an allocation...)
    bool IsKnownNonNull(ValueNum vn) const;

    ValueNum VNForFunc(var_types type, VNFunc func);
    ValueNum VNForFunc(var_types type, VNFunc func, ValueNum arg0);
    ValueNum HasFunc(var_types type, VNFunc func, ValueNum arg0);
    ValueNum VNForFunc(var_types type, VNFunc func, ValueNum arg0, ValueNum arg1);
    ValueNum VNForFunc(var_types type, VNFunc func, ValueNum arg0, ValueNum arg1, ValueNum arg2);
    ValueNum VNForFunc(var_types type, VNFunc func, ValueNum arg0, ValueNum arg1, ValueNum arg2, ValueNum arg3);

    struct VNMapSelect
    {
        ValueNumKind const vnk;
        var_types const    type;
        ValueNum const     indexVN;
        int                budget;
    };

    ValueNum VNForMapSelect(ValueNumKind vnk, var_types type, ValueNum mapVN, ValueNum indexVN);
    ValueNum VNForMapSelectWork(VNMapSelect& select, ValueNum mapVN, bool* usedRecursiveVN);
    ValueNum VNForMapStore(var_types type, ValueNum mapVN, ValueNum indexVN, ValueNum valueVN);

    ValueNumPair VNPairForFunc(var_types type, VNFunc func)
    {
        return ValueNumPair(VNForFunc(type, func));
    }

    ValueNumPair VNPairForFunc(var_types type, VNFunc func, ValueNumPair opVN)
    {
        return {VNForFunc(type, func, opVN.GetLiberal()), VNForFunc(type, func, opVN.GetConservative())};
    }

    ValueNumPair VNPairForFunc(var_types type, VNFunc func, ValueNumPair op1VN, ValueNumPair op2VN)
    {
        return {VNForFunc(type, func, op1VN.GetLiberal(), op2VN.GetLiberal()),
                VNForFunc(type, func, op1VN.GetConservative(), op2VN.GetConservative())};
    }

    ValueNumPair VNPairForFunc(var_types type, VNFunc func, ValueNumPair op1VN, ValueNumPair op2VN, ValueNumPair op3VN)
    {
        return {VNForFunc(type, func, op1VN.GetLiberal(), op2VN.GetLiberal(), op3VN.GetLiberal()),
                VNForFunc(type, func, op1VN.GetConservative(), op2VN.GetConservative(), op3VN.GetConservative())};
    }

    ValueNumPair VNPairForFunc(
        var_types type, VNFunc func, ValueNumPair op1VN, ValueNumPair op2VN, ValueNumPair op3VN, ValueNumPair op4VN)
    {
        return {VNForFunc(type, func, op1VN.GetLiberal(), op2VN.GetLiberal(), op3VN.GetLiberal(), op4VN.GetLiberal()),
                VNForFunc(type, func, op1VN.GetConservative(), op2VN.GetConservative(), op3VN.GetConservative(),
                          op4VN.GetConservative())};
    }

    // Get a new, unique value number for an expression that we're not equating to some function,
    // which is the value of a tree in the given block.
    ValueNum VNForExpr(BasicBlock* block, var_types type);
    ValueNum VNForExpr(var_types type);
    ValueNum UniqueVN(var_types type);

    ValueNum VNForBitCast(ValueNum valueVN, var_types toType);

    ValueNum VNForCast(ValueNum vn, var_types toType);

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

    // Returns true if the VN is known or likely to appear as the conservative value number
    // of the length argument to a GT_BOUNDS_CHECK node.
    bool IsVNCheckedBound(ValueNum vn);

    // Record that a VN is known to appear as the conservative value number of the length
    // argument to a GT_BOUNDS_CHECK node.
    void SetVNIsCheckedBound(ValueNum vn);

    struct CompareCheckedBoundArithInfo
    {
        // (vnBound - 1) > vnOp
        // (vnBound addFunc arrOp) cmpFunc cmpOp
        ValueNum vnBound = NoVN;
        VNFunc   addFunc = VNF_None;
        ValueNum arrOp   = NoVN;
        VNFunc   cmpFunc = VNF_None;
        ValueNum cmpOp   = NoVN;

#ifdef DEBUG
        void Dump() const
        {
            if (addFunc == VNF_None)
            {
                printf("%s(" FMT_VN ", " FMT_VN ")", GetFuncName(cmpFunc), cmpOp, vnBound);
            }
            else
            {
                printf("%s(" FMT_VN ", %s(" FMT_VN ", " FMT_VN "))", GetFuncName(cmpFunc), cmpOp, GetFuncName(addFunc),
                       vnBound, arrOp);
            }
        }
#endif
    };

    static bool IsVNCompareCheckedBoundRelop(const VNFuncApp& funcApp)
    {
        return funcApp.Is(VNF_COND_SLE, VNF_COND_SGE, VNF_COND_SLT, VNF_COND_SGT);
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

    ValueNum EvalMathFuncUnary(var_types type, NamedIntrinsic intrin, ValueNum argVN);
    ValueNum EvalMathFuncBinary(var_types type, NamedIntrinsic intrin, ValueNum arg0VN, ValueNum arg1VN);

    ValueNumPair EvalMathFuncUnary(var_types type, NamedIntrinsic func, ValueNumPair arg0VNP)
    {
        return {EvalMathFuncUnary(type, func, arg0VNP.GetLiberal()),
                EvalMathFuncUnary(type, func, arg0VNP.GetConservative())};
    }

    ValueNumPair EvalMathFuncBinary(var_types type, NamedIntrinsic func, ValueNumPair arg0VNP, ValueNumPair arg1VNP)
    {
        return {EvalMathFuncBinary(type, func, arg0VNP.GetLiberal(), arg1VNP.GetLiberal()),
                EvalMathFuncBinary(type, func, arg0VNP.GetConservative(), arg1VNP.GetConservative())};
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
    void Dump(ValueNum vn);
    void DumpFieldSeq(const VNFuncApp& fieldSeq, bool isHead);
    void DumpMapSelect(const VNFuncApp& mapSelect);
    void DumpMapStore(const VNFuncApp& mapStore);
    void DumpUnique(const VNFuncApp& memOpaque);
    void DumpValWithExc(const VNFuncApp& valWithExc);
    void DumpLclAddr(const VNFuncApp& lclAddr);
    void DumpBitCast(const VNFuncApp& cast) const;
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
        assert(vn != NoVN);
        return vn >> LogChunkSize;
    }

    // Returns the offset of the given "vn" within its chunk.
    static unsigned ChunkOffset(ValueNum vn)
    {
        assert(vn != NoVN);
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
};
