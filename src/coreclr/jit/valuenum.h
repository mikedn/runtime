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
    VNF_None = GT_NONE,
    // All members of the enumeration genTreeOps are also members of VNFunc.
    // (Though some of these may be labeled "illegal").
    VNF_Boundary = GT_COUNT,
#define ValueNumFuncDef(nm, arity, commute, knownNonNull, sharedStatic) VNF_##nm,
#include "valuenumfuncs.h"
    VNF_Count
};

constexpr VNFunc VNFuncIndex(VNFunc vnf)
{
    return static_cast<VNFunc>(vnf & 0xFFFF);
}

#ifdef FEATURE_HW_INTRINSICS
constexpr VNFunc VNFuncHWIntrinsic(NamedIntrinsic intrinsic, var_types simdBaseType, unsigned simdSize)
{
    VNFunc vnf = VNFunc(VNF_HWI_FIRST + (intrinsic - NI_HW_INTRINSIC_START - 1));

    // TODO-MIKE-CQ: It may be useful to canonicalize the vector element type
    // somehow, as some SIMD operations are not affected by it (e.g. bitwise
    // operations). Old code sort of did this but apparently not for CQ reasons
    // but rather due to shoddy design. Removing it did not generate any diffs.
    // Such intrinsics are usually available for all element types so it's
    // unlikely that user code will reinterpret vectors in such a way that
    // we could see some benefit from canonicalization.
    vnf = static_cast<VNFunc>(vnf | (static_cast<uint8_t>(simdBaseType) << 16));
    vnf = static_cast<VNFunc>(vnf | (simdSize << 24));

    return vnf;
}

inline VNFunc VNFuncHWIntrinsic(GenTreeHWIntrinsic* node)
{
    assert(node->GetAuxiliaryType() == TYP_UNDEF);
    return VNFuncHWIntrinsic(node->GetIntrinsic(), node->GetSimdBaseType(), node->GetSimdSize());
}

constexpr var_types VNFuncSimdBaseType(VNFunc vnf)
{
    assert(vnf >= VNF_HWI_FIRST);
    return static_cast<var_types>(vnf >> 16);
}

constexpr uint8_t VNFuncSimdSize(VNFunc vnf)
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

class SsaOptimizer;

class ValueNumStore
{
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

public:
    // We will reserve "max unsigned" to represent "not a value number", for maps that might start uninitialized.
    static const ValueNum NoVN = ::NoVN;
    // A second special value, used to indicate that a function evaluation would cause infinite recursion.
    static const ValueNum RecursiveVN = ::RecursiveVN;

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

    // return vnf(v0)
    template <typename T>
    static T EvalOp(VNFunc vnf, T v0);

    // returns vnf(v0, v1).
    template <typename T>
    T EvalOp(VNFunc vnf, T v0, T v1);

    // return vnf(v0) or vnf(v0, v1), respectively (must, of course be unary/binary ops, respectively.)
    template <typename T>
    static T EvalOpSpecialized(VNFunc vnf, T v0);
    template <typename T>
    T EvalOpSpecialized(VNFunc vnf, T v0, T v1);

    template <typename T>
    static int EvalComparison(VNFunc vnf, T v0, T v1);

    // Should only instantiate (in a non-trivial way) for "int" and "int64_t".  Returns true iff dividing "v0" by "v1"
    // would produce integer overflow (an ArithmeticException -- *not* division by zero, which is separate.)
    template <typename T>
    static bool IsOverflowIntDiv(T v0, T v1);

    // Should only instantiate (in a non-trivial way) for integral types (signed/unsigned int32/int64).
    // Returns true iff v is the zero of the appropriate type.
    template <typename T>
    static bool IsIntZero(T v);

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
    ValueNum EvalCastForConstantArgs(var_types typ, VNFunc vnf, ValueNum vn0, ValueNum vn1);

    ValueNum EvalUsingMathIdentity(var_types typ, VNFunc vnf, ValueNum vn0, ValueNum vn1);

    // This is the maximum number of MapSelect terms that can be "considered" as part of evaluation of a top-level
    // MapSelect application.
    int m_mapSelectBudget;

    template <typename T, typename NumMap>
    inline ValueNum VnForConst(T cnsVal, NumMap* numMap, var_types varType);

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
    ValueNum VNForHandle(ssize_t value, GenTreeFlags handleKind);

    ValueNum VNForFieldSeqHandle(CORINFO_FIELD_HANDLE fieldHandle);

    ValueNum VNForTypeNum(unsigned typeNum);

    // And the single constant for an object reference type.
    static ValueNum VNForNull()
    {
        // We reserve Chunk 0 for "special" VNs.  SRC_Null (== 0) is the VN of "null".
        return ValueNum(SRC_Null);
    }

    // The zero map is the map that returns a zero "for the appropriate type" when indexed at any index.
    ValueNum VNForZeroMap();

    // The ROH map is the map for "read-only memory".  We assume that this is never mutated, and always
    // has the same value number.
    ValueNum VNForReadOnlyMemoryMap();

    // A special value number for "void".
    static ValueNum VNForVoid()
    {
        // We reserve Chunk 0 for "special" VNs.  Let SRC_Void (== 4) be the value for "void".
        return ValueNum(SRC_Void);
    }
    static ValueNumPair VNPForVoid()
    {
        return ValueNumPair(VNForVoid(), VNForVoid());
    }

    // A special value number for the empty set of exceptions.
    static ValueNum VNForEmptyExcSet()
    {
        // We reserve Chunk 0 for "special" VNs.  Let SRC_EmptyExcSet (== 5) be the value for the empty set of
        // exceptions.
        return ValueNum(SRC_EmptyExcSet);
    }
    static ValueNumPair VNPForEmptyExcSet()
    {
        return ValueNumPair(VNForEmptyExcSet(), VNForEmptyExcSet());
    }

    // Returns the value number for zero of the given "typ".
    // It has an unreached() for a "typ" that has no zero value, such as TYP_VOID.
    ValueNum VNZeroForType(var_types typ);

    // Returns the value number for one of the given "typ".
    // It returns NoVN for a "typ" that has no one value, such as TYP_REF.
    ValueNum VNOneForType(var_types typ);

    // Create or return the existimg value number representing a singleton exception set
    // for the the exception value "x".
    ValueNum VNExcSetSingleton(ValueNum x);
    ValueNumPair VNPExcSetSingleton(ValueNumPair x);

    // Returns true if the current pair of items are in ascending order and they are not duplicates.
    // Used to verify that exception sets are in ascending order when processing them.
    bool VNCheckAscending(ValueNum item, ValueNum xs1);

    // Returns the VN representing the union of the two exception sets "xs0" and "xs1".
    // These must be VNForEmtpyExcSet() or applications of VNF_ExcSetCons, obeying
    // the ascending order invariant. (which is preserved in the result)
    ValueNum VNExcSetUnion(ValueNum xs0, ValueNum xs1);

    ValueNumPair VNPExcSetUnion(ValueNumPair xs0vnp, ValueNumPair xs1vnp);

    // Returns the VN representing the intersection of the two exception sets "xs0" and "xs1".
    // These must be applications of VNF_ExcSetCons or the empty set. (i.e VNForEmptyExcSet())
    // and also must be in ascending order.
    ValueNum VNExcSetIntersection(ValueNum xs0, ValueNum xs1);

    ValueNumPair VNPExcSetIntersection(ValueNumPair xs0vnp, ValueNumPair xs1vnp);

    // Returns true if every exeception singleton in the vnCandidateSet is also present
    // in the vnFullSet.
    // Both arguments must be either VNForEmptyExcSet() or applications of VNF_ExcSetCons.
    bool ExsetIsSubset(ValueNum vnCandidateSet, ValueNum vnFullSet);

    // Returns "true" iff "vn" is an application of "VNF_ValWithExc".
    bool VNHasExc(ValueNum vn)
    {
        VNFuncApp funcApp;
        return GetVNFunc(vn, &funcApp) && funcApp.m_func == VNF_ValWithExc;
    }

    // If vn "excSet" is "VNForEmptyExcSet()" we just return "vn"
    // otherwise we use VNExcSetUnion to combine the exception sets of both "vn" and "excSet"
    // and return that ValueNum
    ValueNum VNWithExc(ValueNum vn, ValueNum excSet);
    ValueNum PackExset(ValueNum vn, ValueNum exset);

    ValueNumPair VNPWithExc(ValueNumPair vnp, ValueNumPair excSetVNP);

    // This sets "*pvn" to the Normal value and sets "*pvnx" to Exception set value.
    // "pvnx" represents the set of all exceptions that can happen for the expression
    void VNUnpackExc(ValueNum vnWx, ValueNum* pvn, ValueNum* pvnx);
    ValueNum UnpackExset(ValueNum vn, ValueNum* exset);
    ValueNumPair UnpackExset(ValueNumPair vnp, ValueNumPair* exset);

    void VNPUnpackExc(ValueNumPair vnWx, ValueNumPair* pvn, ValueNumPair* pvnx);

    // This returns the Union of exceptions from vnWx and vnExcSet
    ValueNum VNUnionExcSet(ValueNum vnWx, ValueNum vnExcSet);

    // This returns the Union of exceptions from vnpWx and vnpExcSet
    ValueNumPair VNPUnionExcSet(ValueNumPair vnpWx, ValueNumPair vnpExcSet);

    // If "vn" is a "VNF_ValWithExc(norm, excSet)" value, returns the "norm" argument; otherwise,
    // just returns "vn".
    // The Normal value is the value number of the expression when no exceptions occurred
    ValueNum VNNormalValue(ValueNum vn) const;

    // Given a "vnp", get the ValueNum kind based upon vnk,
    // then call VNNormalValue on that ValueNum
    // The Normal value is the value number of the expression when no exceptions occurred
    ValueNum VNNormalValue(ValueNumPair vnp, ValueNumKind vnk);

    // Given a "vnp", get the NormalValuew for the VNK_Liberal part of that ValueNum
    // The Normal value is the value number of the expression when no exceptions occurred
    inline ValueNum VNLiberalNormalValue(ValueNumPair vnp)
    {
        return VNNormalValue(vnp, VNK_Liberal);
    }

    // Given a "vnp", get the NormalValuew for the VNK_Conservative part of that ValueNum
    // The Normal value is the value number of the expression when no exceptions occurred
    inline ValueNum VNConservativeNormalValue(ValueNumPair vnp)
    {
        return VNNormalValue(vnp, VNK_Conservative);
    }

    // Given a "vnp", get the Normal values for both the liberal and conservative parts of "vnp"
    // The Normal value is the value number of the expression when no exceptions occurred
    ValueNumPair VNPNormalPair(ValueNumPair vnp);

    // If "vn" is a "VNF_ValWithExc(norm, excSet)" value, returns the "excSet" argument; otherwise,
    // we return a special Value Number representing the empty exception set.
    // The exeception set value is the value number of the set of possible exceptions.
    ValueNum VNExceptionSet(ValueNum vn);

    ValueNumPair VNPExceptionSet(ValueNumPair vn);

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
        return ValueNumPair(VNForFunc(typ, func, opVN.GetLiberal()), VNForFunc(typ, func, opVN.GetConservative()));
    }
    ValueNumPair VNPairForFunc(var_types typ, VNFunc func, ValueNumPair op1VN, ValueNumPair op2VN)
    {
        return ValueNumPair(VNForFunc(typ, func, op1VN.GetLiberal(), op2VN.GetLiberal()),
                            VNForFunc(typ, func, op1VN.GetConservative(), op2VN.GetConservative()));
    }
    ValueNumPair VNPairForFunc(var_types typ, VNFunc func, ValueNumPair op1VN, ValueNumPair op2VN, ValueNumPair op3VN)
    {
        return ValueNumPair(VNForFunc(typ, func, op1VN.GetLiberal(), op2VN.GetLiberal(), op3VN.GetLiberal()),
                            VNForFunc(typ, func, op1VN.GetConservative(), op2VN.GetConservative(),
                                      op3VN.GetConservative()));
    }
    ValueNumPair VNPairForFunc(
        var_types typ, VNFunc func, ValueNumPair op1VN, ValueNumPair op2VN, ValueNumPair op3VN, ValueNumPair op4VN)
    {
        return ValueNumPair(VNForFunc(typ, func, op1VN.GetLiberal(), op2VN.GetLiberal(), op3VN.GetLiberal(),
                                      op4VN.GetLiberal()),
                            VNForFunc(typ, func, op1VN.GetConservative(), op2VN.GetConservative(),
                                      op3VN.GetConservative(), op4VN.GetConservative()));
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

    typedef SmallHashTable<ValueNum, bool, 8U> CheckedBoundVNSet;

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

    // Returns the flags on the current handle. GTF_ICON_CLASS_HDL for example.
    GenTreeFlags GetHandleFlags(ValueNum vn) const;

    // Returns true iff the VN represents a handle constant.
    bool IsVNHandle(ValueNum vn) const;

    // Convert a vartype_t to the value number's storage type for that vartype_t.
    // For example, ValueNum of type TYP_LONG are stored in a map of int64_t variables.
    // Lang is the language (C++) type for the corresponding vartype_t.
    template <int N>
    struct VarTypConv
    {
    };

private:
    struct Chunk;

    template <typename T>
    static T CoerceTypRefToT(Chunk* c, unsigned offset);

    // Get the actual value and coerce the actual type c->m_typ to the wanted type T.
    template <typename T>
    FORCEINLINE T SafeGetConstantValue(Chunk* c, unsigned offset) const;

    template <typename T>
    T ConstantValueInternal(ValueNum vn DEBUGARG(bool coerce)) const
    {
        Chunk* c = m_chunks.Get(GetChunkNum(vn));
        assert(c->m_attribs == CEA_Const || c->m_attribs == CEA_Handle);

        unsigned offset = ChunkOffset(vn);

        switch (c->m_typ)
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
                if (c->m_attribs == CEA_Handle)
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
                    if (varTypeIsFloating(c->m_typ))
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
        return ValueNumPair(EvalMathFuncUnary(typ, mthFunc, arg0VNP.GetLiberal()),
                            EvalMathFuncUnary(typ, mthFunc, arg0VNP.GetConservative()));
    }

    ValueNumPair EvalMathFuncBinary(var_types typ, NamedIntrinsic mthFunc, ValueNumPair arg0VNP, ValueNumPair arg1VNP)
    {
        return ValueNumPair(EvalMathFuncBinary(typ, mthFunc, arg0VNP.GetLiberal(), arg1VNP.GetLiberal()),
                            EvalMathFuncBinary(typ, mthFunc, arg0VNP.GetConservative(), arg1VNP.GetConservative()));
    }

    // If "vn" represents a function application, returns "true" and set "*funcApp" to
    // the function application it represents; otherwise, return "false."
    VNFunc GetVNFunc(ValueNum vn, VNFuncApp* funcApp) const;

#ifdef DEBUG
    void Dump(Compiler* comp, ValueNum vn, bool isPtr = false);
    void DumpFieldSeq(Compiler* comp, VNFuncApp* fieldSeq, bool isHead);
    void DumpMapSelect(Compiler* comp, VNFuncApp* mapSelect);
    void DumpMapStore(Compiler* comp, VNFuncApp* mapStore);
    void DumpMemOpaque(Compiler* comp, VNFuncApp* memOpaque);
    void DumpValWithExc(Compiler* comp, VNFuncApp* valWithExc);
    void DumpLclAddr(Compiler* comp, VNFuncApp* lclAddr);
    void DumpBitCast(const VNFuncApp& cast);
    void DumpCast(const VNFuncApp& cast);
    void DumpPtrToArrElem(const VNFuncApp& elemAddr);
    void DumpExcSeq(Compiler* comp, VNFuncApp* excSeq, bool isHead);

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
    static const unsigned LogChunkSize    = 6;
    static const unsigned ChunkSize       = 1 << LogChunkSize;
    static const unsigned ChunkOffsetMask = ChunkSize - 1;

    // A "ChunkNum" is a zero-based index naming a chunk in the Store, or else the special "NoChunk" value.
    typedef UINT32        ChunkNum;
    static const ChunkNum NoChunk = UINT32_MAX;

    // Returns the ChunkNum of the Chunk that holds "vn" (which is required to be a valid
    // value number, i.e., one returned by some VN-producing method of this class).
    static ChunkNum GetChunkNum(ValueNum vn)
    {
        return vn >> LogChunkSize;
    }

    // Returns the offset of the given "vn" within its chunk.
    static unsigned ChunkOffset(ValueNum vn)
    {
        return vn & ChunkOffsetMask;
    }

    // The base VN of the next chunk to be allocated.  Should always be a multiple of ChunkSize.
    ValueNum m_nextChunkBase = 0;

    enum ChunkExtraAttribs : BYTE
    {
        CEA_Const,     // This chunk contains constant values.
        CEA_Handle,    // This chunk contains handle constants.
        CEA_NotAField, // This chunk contains "not a field" values.
        CEA_Func0,     // Represents functions of arity 0.
        CEA_Func1,     // ...arity 1.
        CEA_Func2,     // ...arity 2.
        CEA_Func3,     // ...arity 3.
        CEA_Func4,     // ...arity 4.
        CEA_Count
    };

    // A "Chunk" holds "ChunkSize" value numbers, starting at "m_baseVN".  All of these share the same
    // "m_typ" and "m_attribs".  These properties determine the interpretation of "m_defs", as discussed below.
    struct Chunk
    {
        // If "m_defs" is non-null, it is an array of size ChunkSize, whose element type is determined by the other
        // members. The "m_numUsed" field indicates the number of elements of "m_defs" that are already consumed (the
        // next one to allocate).
        void*    m_defs;
        unsigned m_numUsed;

        // The value number of the first VN in the chunk.
        ValueNum m_baseVN;

        // The common attributes of this chunk.
        var_types         m_typ;
        ChunkExtraAttribs m_attribs;

        // Initialize a chunk, starting at "*baseVN", for the given "typ", and "attribs", using "alloc" for allocations.
        // (Increments "*baseVN" by ChunkSize.)
        Chunk(CompAllocator alloc, ValueNum* baseVN, var_types typ, ChunkExtraAttribs attribs);

        // Requires that "m_numUsed < ChunkSize."  Returns the offset of the allocated VN within the chunk; the
        // actual VN is this added to the "m_baseVN" of the chunk.
        unsigned AllocVN()
        {
            assert(m_numUsed < ChunkSize);
            return m_numUsed++;
        }

        template <int N>
        struct Alloc
        {
            typedef typename ValueNumStore::VarTypConv<N>::Type Type;
        };
    };

    struct VNHandle : public JitKeyFuncsDefEquals<VNHandle>
    {
        ssize_t      value;
        GenTreeFlags kind;

        VNHandle(ssize_t value, GenTreeFlags kind) : value(value), kind(kind)
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

    struct VNDefFunc0Arg
    {
        VNFunc m_func;

        VNDefFunc0Arg(VNFunc func) : m_func(func)
        {
        }

        bool operator==(const VNDefFunc0Arg& y) const
        {
            return m_func == y.m_func;
        }
    };

    struct VNDefFunc1Arg : public VNDefFunc0Arg
    {
        ValueNum m_arg0;

        VNDefFunc1Arg(VNFunc func, ValueNum arg0) : VNDefFunc0Arg(func), m_arg0(arg0)
        {
        }

        bool operator==(const VNDefFunc1Arg& y) const
        {
            return VNDefFunc0Arg::operator==(y) && m_arg0 == y.m_arg0;
        }
    };

    struct VNDefFunc2Arg : public VNDefFunc1Arg
    {
        ValueNum m_arg1;

        VNDefFunc2Arg(VNFunc func, ValueNum arg0, ValueNum arg1) : VNDefFunc1Arg(func, arg0), m_arg1(arg1)
        {
        }

        bool operator==(const VNDefFunc2Arg& y) const
        {
            return VNDefFunc1Arg::operator==(y) && m_arg1 == y.m_arg1;
        }
    };

    struct VNDefFunc3Arg : public VNDefFunc2Arg
    {
        ValueNum m_arg2;

        VNDefFunc3Arg(VNFunc func, ValueNum arg0, ValueNum arg1, ValueNum arg2)
            : VNDefFunc2Arg(func, arg0, arg1), m_arg2(arg2)
        {
        }

        bool operator==(const VNDefFunc3Arg& y) const
        {
            return VNDefFunc2Arg::operator==(y) && m_arg2 == y.m_arg2;
        }
    };

    struct VNDefFunc4Arg : public VNDefFunc3Arg
    {
        ValueNum m_arg3;

        VNDefFunc4Arg(VNFunc func, ValueNum arg0, ValueNum arg1, ValueNum arg2, ValueNum arg3)
            : VNDefFunc3Arg(func, arg0, arg1, arg2), m_arg3(arg3)
        {
        }

        bool operator==(const VNDefFunc4Arg& y) const
        {
            return VNDefFunc3Arg::operator==(y) && m_arg3 == y.m_arg3;
        }
    };

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
    // ChunkExtraAttribute>.
    // If the value is NoChunk, it indicates that there is no current allocation chunk for that pair, otherwise
    // it is the index in "m_chunks" of a chunk with the given attributes, in which the next allocation should
    // be attempted.
    ChunkNum m_curAllocChunk[TYP_COUNT][CEA_Count + 1];

    // Returns a (pointer to a) chunk in which a new value number may be allocated.
    Chunk* GetAllocChunk(var_types typ, ChunkExtraAttribs attribs);

    ValueNum m_zeroMap           = NoVN;
    ValueNum m_readOnlyMemoryMap = NoVN;

    // First, we need mechanisms for mapping from constants to value numbers.
    // For small integers, we'll use an array.
    static const int      SmallIntConstMin = -1;
    static const int      SmallIntConstMax = 10;
    static const unsigned SmallIntConstNum = SmallIntConstMax - SmallIntConstMin + 1;

    static bool IsSmallIntConst(int i)
    {
        return SmallIntConstMin <= i && i <= SmallIntConstMax;
    }

    ValueNum m_smallInt32VNMap[SmallIntConstNum];

    using Int32VNMap  = VNMap<int32_t>;
    using Int64VNMap  = VNMap<int64_t>;
    using HandleVNMap = VNMap<VNHandle, VNHandle>;
    using ByrefVNMap  = VNMap<size_t>;

    struct VNDefFunc1ArgKeyFuncs : public JitKeyFuncsDefEquals<VNDefFunc1Arg>
    {
        static unsigned GetHashCode(VNDefFunc1Arg val)
        {
            return (val.m_func << 24) + val.m_arg0;
        }
    };

    struct VNDefFunc2ArgKeyFuncs : public JitKeyFuncsDefEquals<VNDefFunc2Arg>
    {
        static unsigned GetHashCode(const VNDefFunc2Arg& val)
        {
            return (val.m_func << 24) + (val.m_arg0 << 8) + val.m_arg1;
        }
    };

    struct VNDefFunc3ArgKeyFuncs : public JitKeyFuncsDefEquals<VNDefFunc3Arg>
    {
        static unsigned GetHashCode(const VNDefFunc3Arg& val)
        {
            return (val.m_func << 24) + (val.m_arg0 << 16) + (val.m_arg1 << 8) + val.m_arg2;
        }
    };

    struct VNDefFunc4ArgKeyFuncs : public JitKeyFuncsDefEquals<VNDefFunc4Arg>
    {
        static unsigned GetHashCode(const VNDefFunc4Arg& val)
        {
            return (val.m_func << 24) + (val.m_arg0 << 16) + (val.m_arg1 << 8) + val.m_arg2 + (val.m_arg3 << 12);
        }
    };

    using Func0VNMap = VNMap<VNFunc>;
    using Func1VNMap = VNMap<VNDefFunc1Arg, VNDefFunc1ArgKeyFuncs>;
    using Func2VNMap = VNMap<VNDefFunc2Arg, VNDefFunc2ArgKeyFuncs>;
    using Func3VNMap = VNMap<VNDefFunc3Arg, VNDefFunc3ArgKeyFuncs>;
    using Func4VNMap = VNMap<VNDefFunc4Arg, VNDefFunc4ArgKeyFuncs>;

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

    enum SpecialRefConsts
    {
        SRC_Null,
        SRC_Void,
        SRC_EmptyExcSet,

        SRC_NumSpecialRefConsts
    };

#ifdef DEBUG
    // This helps test some performance pathologies related to "evaluation" of VNF_MapSelect terms,
    // especially relating to GcHeap/ByrefExposed.  We count the number of applications of such terms we consider,
    // and if this exceeds a limit, indicated by a COMPlus_ variable, we assert.
    unsigned m_numMapSels = 0;

    JitHashTable<ValueNum, JitSmallPrimitiveKeyFuncs<ValueNum>, const char*> m_vnNameMap;
#endif
};

template <>
struct ValueNumStore::VarTypConv<TYP_INT>
{
    typedef int32_t Type;
    typedef int     Lang;
};
template <>
struct ValueNumStore::VarTypConv<TYP_FLOAT>
{
    typedef int32_t Type;
    typedef float   Lang;
};
template <>
struct ValueNumStore::VarTypConv<TYP_LONG>
{
    typedef int64_t Type;
    typedef int64_t Lang;
};
template <>
struct ValueNumStore::VarTypConv<TYP_DOUBLE>
{
    typedef int64_t Type;
    typedef double  Lang;
};
template <>
struct ValueNumStore::VarTypConv<TYP_BYREF>
{
    typedef size_t Type;
    typedef void*  Lang;
};
template <>
struct ValueNumStore::VarTypConv<TYP_REF>
{
    typedef class Object* Type;
    typedef class Object* Lang;
};

// Get the actual value and coerce the actual type c->m_typ to the wanted type T.
template <typename T>
FORCEINLINE T ValueNumStore::SafeGetConstantValue(Chunk* c, unsigned offset) const
{
    switch (c->m_typ)
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

template <>
inline size_t ValueNumStore::CoerceTypRefToT(Chunk* c, unsigned offset)
{
    return reinterpret_cast<size_t>(static_cast<VarTypConv<TYP_REF>::Type*>(c->m_defs)[offset]);
}

template <typename T>
inline T ValueNumStore::CoerceTypRefToT(Chunk* c, unsigned offset)
{
    noway_assert(sizeof(T) >= sizeof(VarTypConv<TYP_REF>::Type));
    unreached();
}
