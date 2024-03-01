// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                    Inline functions                                       XX
XX                                                                           XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#ifndef _COMPILER_HPP_
#define _COMPILER_HPP_

#include "compilerbitsettraits.hpp"

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX  Miscellaneous utility functions. Some of these are defined in Utils.cpp  XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

/*****************************************************************************/
/*****************************************************************************/

inline bool getInlinePInvokeEnabled()
{
#ifdef DEBUG
    return JitConfig.JitPInvokeEnabled() && !JitConfig.StressCOMCall();
#else
    return true;
#endif
}

// Enforce float narrowing for buggy compilers (notably preWhidbey VC)
inline float forceCastToFloat(double d)
{
    Volatile<float> f = (float)d;
    return f;
}

// Enforce UInt32 narrowing for buggy compilers (notably Whidbey Beta 2 LKG)
inline UINT32 forceCastToUInt32(double d)
{
    Volatile<UINT32> u = (UINT32)d;
    return u;
}

/*****************************************************************************/
/*****************************************************************************
 *
 *  Return the highest bit that is set (that is, a mask that includes just the highest bit).
 *  TODO-ARM64-Throughput: we should convert these to use the _BitScanReverse() / _BitScanReverse64()
 *  compiler intrinsics, but our CRT header file intrin.h doesn't define these for ARM64 yet.
 */

inline unsigned int genFindHighestBit(unsigned int mask)
{
    assert(mask != 0);
    unsigned int bit = 1U << ((sizeof(unsigned int) * 8) - 1); // start looking at the top
    while ((bit & mask) == 0)
    {
        bit >>= 1;
    }
    return bit;
}

inline unsigned __int64 genFindHighestBit(unsigned __int64 mask)
{
    assert(mask != 0);
    unsigned __int64 bit = 1ULL << ((sizeof(unsigned __int64) * 8) - 1); // start looking at the top
    while ((bit & mask) == 0)
    {
        bit >>= 1;
    }
    return bit;
}

#if 0
// TODO-ARM64-Cleanup: These should probably be the implementation, when intrin.h is updated for ARM64
inline
unsigned int genFindHighestBit(unsigned int mask)
{
    assert(mask != 0);
    unsigned int index;
    _BitScanReverse(&index, mask);
    return 1L << index;
}

inline
unsigned __int64 genFindHighestBit(unsigned __int64 mask)
{
    assert(mask != 0);
    unsigned int index;
    _BitScanReverse64(&index, mask);
    return 1LL << index;
}
#endif // 0

/*****************************************************************************
*
*  Return true if the given 64-bit value has exactly zero or one bits set.
*/

template <typename T>
inline bool genMaxOneBit(T value)
{
    return (value & (value - 1)) == 0;
}

/*****************************************************************************
*
*  Return true if the given 32-bit value has exactly zero or one bits set.
*/

inline bool genMaxOneBit(unsigned value)
{
    return (value & (value - 1)) == 0;
}

/*****************************************************************************
*
*  Return true if the given 64-bit value has exactly one bit set.
*/

template <typename T>
inline bool genExactlyOneBit(T value)
{
    return ((value != 0) && genMaxOneBit(value));
}

/*****************************************************************************
*
*  Return true if the given 32-bit value has exactly zero or one bits set.
*/

inline bool genExactlyOneBit(unsigned value)
{
    return ((value != 0) && genMaxOneBit(value));
}

/*****************************************************************************
 *
 *  Given a value that has exactly one bit set, return the position of that
 *  bit, in other words return the logarithm in base 2 of the given value.
 */
inline unsigned genLog2(unsigned value)
{
    return BitPosition(value);
}

// Given an unsigned 64-bit value, returns the lower 32-bits in unsigned format
//
inline unsigned ulo32(unsigned __int64 value)
{
    return static_cast<unsigned>(value);
}

// Given an unsigned 64-bit value, returns the upper 32-bits in unsigned format
//
inline unsigned uhi32(unsigned __int64 value)
{
    return static_cast<unsigned>(value >> 32);
}

/*****************************************************************************
 *
 *  Given a value that has exactly one bit set, return the position of that
 *  bit, in other words return the logarithm in base 2 of the given value.
 */

inline unsigned genLog2(unsigned __int64 value)
{
    unsigned lo32 = ulo32(value);
    unsigned hi32 = uhi32(value);

    if (lo32 != 0)
    {
        assert(hi32 == 0);
        return genLog2(lo32);
    }
    else
    {
        return genLog2(hi32) + 32;
    }
}

/*****************************************************************************
 *
 *  Return the lowest bit that is set in the given register mask.
 */

inline regMaskTP genFindLowestReg(regMaskTP value)
{
    return (regMaskTP)genFindLowestBit(value);
}

/*****************************************************************************
 *
 *  Given 3 masks value, end, start, returns the bits of value between start
 *  and end (exclusive).
 *
 *  value[bitNum(end) - 1, bitNum(start) + 1]
 */

inline unsigned __int64 BitsBetween(unsigned __int64 value, unsigned __int64 end, unsigned __int64 start)
{
    assert(start != 0);
    assert(start < end);
    assert((start & (start - 1)) == 0);
    assert((end & (end - 1)) == 0);

    return value & ~((start - 1) | start) & // Ones to the left of set bit in the start mask.
           (end - 1);                       // Ones to the right of set bit in the end mask.
}

/*****************************************************************************
 * Returns true if value is between [start..end).
 * The comparison is inclusive of start, exclusive of end.
 */

/* static */
inline bool Compiler::jitIsBetween(unsigned value, unsigned start, unsigned end)
{
    return start <= value && value < end;
}

/*****************************************************************************
 * Returns true if value is between [start..end].
 * The comparison is inclusive of both start and end.
 */

/* static */
inline bool Compiler::jitIsBetweenInclusive(unsigned value, unsigned start, unsigned end)
{
    return start <= value && value <= end;
}

/******************************************************************************************
 * Return the EH descriptor for the given region index.
 */
inline EHblkDsc* Compiler::ehGetDsc(unsigned regionIndex)
{
    assert(regionIndex < compHndBBtabCount);
    return &compHndBBtab[regionIndex];
}

/******************************************************************************************
 * Return the EH descriptor index of the enclosing try, for the given region index.
 */
inline unsigned Compiler::ehGetEnclosingTryIndex(unsigned regionIndex)
{
    return ehGetDsc(regionIndex)->ebdEnclosingTryIndex;
}

/******************************************************************************************
 * Return the EH descriptor index of the enclosing handler, for the given region index.
 */
inline unsigned Compiler::ehGetEnclosingHndIndex(unsigned regionIndex)
{
    return ehGetDsc(regionIndex)->ebdEnclosingHndIndex;
}

/******************************************************************************************
 * Return the EH index given a region descriptor.
 */
inline unsigned Compiler::ehGetIndex(EHblkDsc* ehDsc)
{
    assert(compHndBBtab <= ehDsc && ehDsc < compHndBBtab + compHndBBtabCount);
    return (unsigned)(ehDsc - compHndBBtab);
}

/******************************************************************************************
 * Return the EH descriptor for the most nested 'try' region this BasicBlock is a member of
 * (or nullptr if this block is not in a 'try' region).
 */
inline EHblkDsc* Compiler::ehGetBlockTryDsc(BasicBlock* block)
{
    if (!block->hasTryIndex())
    {
        return nullptr;
    }

    return ehGetDsc(block->getTryIndex());
}

/******************************************************************************************
 * Return the EH descriptor for the most nested filter or handler region this BasicBlock is a member of
 * (or nullptr if this block is not in a filter or handler region).
 */
inline EHblkDsc* Compiler::ehGetBlockHndDsc(BasicBlock* block)
{
    if (!block->hasHndIndex())
    {
        return nullptr;
    }

    return ehGetDsc(block->getHndIndex());
}

//------------------------------------------------------------------------------
// genRegNumFromMask : Maps a single register mask to a register number.
//
// Arguments:
//    mask - the register mask
//
// Return Value:
//    The number of the register contained in the mask.
//
// Assumptions:
//    The mask contains one and only one register.

inline regNumber genRegNumFromMask(regMaskTP mask)
{
    assert(mask != 0); // Must have one bit set, so can't have a mask of zero

    /* Convert the mask to a register number */

    regNumber regNum = (regNumber)genLog2(mask);

    /* Make sure we got it right */

    assert(genRegMask(regNum) == mask);

    return regNum;
}

extern const BYTE genTypeSizes[TYP_COUNT];

template <class T>
inline unsigned genTypeSize(T value)
{
    assert((unsigned)TypeGet(value) < _countof(genTypeSizes));

    return genTypeSizes[TypeGet(value)];
}

/*****************************************************************************
 *
 *  The following function maps a 'precise' type to an actual type as seen
 *  by the VM (for example, 'byte' maps to 'int').
 */

inline var_types genActualType(var_types type)
{
    return varActualType(type);
}

const char* varTypeName(var_types);

/*****************************************************************************/
//  Helpers to pull little-endian values out of a byte stream.

inline unsigned __int8 getU1LittleEndian(const BYTE* ptr)
{
    return *(UNALIGNED unsigned __int8*)ptr;
}

inline unsigned __int16 getU2LittleEndian(const BYTE* ptr)
{
    return GET_UNALIGNED_VAL16(ptr);
}

inline unsigned __int32 getU4LittleEndian(const BYTE* ptr)
{
    return GET_UNALIGNED_VAL32(ptr);
}

inline signed __int8 getI1LittleEndian(const BYTE* ptr)
{
    return *(UNALIGNED signed __int8*)ptr;
}

inline signed __int16 getI2LittleEndian(const BYTE* ptr)
{
    return GET_UNALIGNED_VAL16(ptr);
}

inline signed __int32 getI4LittleEndian(const BYTE* ptr)
{
    return GET_UNALIGNED_VAL32(ptr);
}

inline signed __int64 getI8LittleEndian(const BYTE* ptr)
{
    return GET_UNALIGNED_VAL64(ptr);
}

inline float getR4LittleEndian(const BYTE* ptr)
{
    __int32 val = getI4LittleEndian(ptr);
    return *(float*)&val;
}

inline double getR8LittleEndian(const BYTE* ptr)
{
    __int64 val = getI8LittleEndian(ptr);
    return *(double*)&val;
}

#ifdef DEBUG
const char* refCntWtd2str(BasicBlock::weight_t refCntWtd);
#endif

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                          GenTree                                          XX
XX                      Inline functions                                     XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

void* GenTree::operator new(size_t sz, Compiler* comp, genTreeOps oper)
{
    size_t size = GenTree::s_gtNodeSizes[oper];

#if MEASURE_NODE_SIZE
    genNodeSizeStats.genTreeNodeCnt += 1;
    genNodeSizeStats.genTreeNodeSize += size;
    genNodeSizeStats.genTreeNodeActualSize += sz;

    genNodeSizeStatsPerFunc.genTreeNodeCnt += 1;
    genNodeSizeStatsPerFunc.genTreeNodeSize += size;
    genNodeSizeStatsPerFunc.genTreeNodeActualSize += sz;
#endif // MEASURE_NODE_SIZE

    assert(size >= sz);
    return comp->getAllocator(CMK_ASTNode).allocate<char>(size);
}

inline GenTree::GenTree(genTreeOps oper, var_types type DEBUGARG(bool largeNode))
    : gtOper(oper)
    , gtType(type)
#ifdef DEBUG
    , gtTreeID(JitTls::GetCompiler()->compGenTreeID++)
#endif
{
#ifdef DEBUG
    if ((s_gtNodeSizes[oper] == TREE_NODE_SZ_SMALL) && !largeNode)
    {
        gtDebugFlags |= GTF_DEBUG_NODE_SMALL;
    }
    else if ((s_gtNodeSizes[oper] == TREE_NODE_SZ_LARGE) || largeNode)
    {
        gtDebugFlags |= GTF_DEBUG_NODE_LARGE;
    }
    else
    {
        assert(!"bogus node size");
    }
#endif

#if COUNT_AST_OPERS
    InterlockedIncrement(&s_gtNodeCounts[oper]);
#endif
}

inline Statement* Compiler::gtNewStmt(GenTree* expr, IL_OFFSETX offset)
{
    return new (getAllocator(CMK_ASTNode)) Statement(expr, offset DEBUGARG(compStatementID++));
}

inline GenTreeUnOp* Compiler::gtNewOperNode(genTreeOps oper, var_types type, GenTree* op1)
{
    assert((GenTree::OperKind(oper) & (GTK_UNOP | GTK_BINOP)) != 0);
    // Can't use this to construct any types that extend unary/binary operator.
    assert((GenTree::OperKind(oper) & GTK_EXOP) == 0);
    assert(op1 != nullptr || oper == GT_RETFILT || oper == GT_NOP || (oper == GT_RETURN && type == TYP_VOID));

    return new (this, oper) GenTreeOp(oper, type, op1, nullptr);
}

// Returns an opcode that is of the largest node size in use.
inline genTreeOps LargeOpOpcode()
{
    assert(GenTree::s_gtNodeSizes[GT_CALL] == TREE_NODE_SZ_LARGE);
    return GT_CALL;
}

/******************************************************************************
 *
 * Use to create nodes which may later be morphed to another (big) operator
 */

inline GenTree* Compiler::gtNewLargeOperNode(genTreeOps oper, var_types type, GenTree* op1, GenTree* op2)
{
    assert((GenTree::OperKind(oper) & (GTK_UNOP | GTK_BINOP)) != 0);
    // Can't use this to construct any types that extend unary/binary operator.
    assert((GenTree::OperKind(oper) & GTK_EXOP) == 0);
    assert(GenTree::s_gtNodeSizes[oper] == TREE_NODE_SZ_SMALL);
    // Allocate a large node
    GenTree* node = new (this, LargeOpOpcode()) GenTreeOp(oper, type, op1, op2 DEBUGARG(/*largeNode*/ true));
    return node;
}

inline GenTreeIntCon* Compiler::gtNewIconHandleNode(void* value, HandleKind kind, FieldSeqNode* fieldSeq)
{
    return gtNewIconHandleNode(reinterpret_cast<size_t>(value), kind, fieldSeq);
}

inline GenTreeIntCon* Compiler::gtNewIconHandleNode(size_t value, HandleKind kind, FieldSeqNode* fieldSeq)
{
    assert(kind != HandleKind::None);

    if (fieldSeq == nullptr)
    {
        fieldSeq = FieldSeqStore::NotAField();
    }

    GenTreeIntCon* node = new (this, GT_CNS_INT) GenTreeIntCon(TYP_I_IMPL, value, fieldSeq);
    node->SetHandleKind(kind);
    return node;
}

// It may not be allowed to embed HANDLEs directly into the JITed code (for eg,
// as arguments to JIT helpers). Get a corresponding value that can be embedded.
// These are versions for each specific type of HANDLE
inline GenTree* Compiler::gtNewIconEmbModHndNode(CORINFO_MODULE_HANDLE modHnd)
{
    void* handleAddr;
    void* handle = reinterpret_cast<void*>(info.compCompHnd->embedModuleHandle(modHnd, &handleAddr));

    return gtNewConstLookupTree(handle, handleAddr, HandleKind::Module, modHnd);
}

inline GenTree* Compiler::gtNewIconEmbClsHndNode(CORINFO_CLASS_HANDLE clsHnd)
{
    void* handleAddr;
    void* handle = reinterpret_cast<void*>(info.compCompHnd->embedClassHandle(clsHnd, &handleAddr));

    return gtNewConstLookupTree(handle, handleAddr, HandleKind::Class, clsHnd);
}

inline GenTree* Compiler::gtNewIconEmbMethHndNode(CORINFO_METHOD_HANDLE methHnd)
{
    void* handleAddr;
    void* handle = reinterpret_cast<void*>(info.compCompHnd->embedMethodHandle(methHnd, &handleAddr));

    return gtNewConstLookupTree(handle, handleAddr, HandleKind::Method, methHnd);
}

inline GenTree* Compiler::gtNewIconEmbFldHndNode(CORINFO_FIELD_HANDLE fldHnd)
{
    void* handleAddr;
    void* handle = reinterpret_cast<void*>(info.compCompHnd->embedFieldHandle(fldHnd, &handleAddr));

    return gtNewConstLookupTree(handle, handleAddr, HandleKind::Field, fldHnd);
}

inline GenTreeCall* Compiler::gtNewRuntimeLookupHelperCallNode(CORINFO_RUNTIME_LOOKUP* lookup,
                                                               GenTree*                ctxTree,
                                                               void*                   compileTimeHandle)
{
    GenTreeIntCon* argNode = gtNewIconHandleNode(lookup->signature, HandleKind::MutableData);
    argNode->SetCompileTimeHandle(compileTimeHandle);
    return gtNewHelperCallNode(lookup->helper, TYP_I_IMPL, gtNewCallArgs(ctxTree, argNode));
}

inline GenTreeAllocObj* Compiler::gtNewAllocObjNode(
    unsigned int helper, bool helperHasSideEffects, CORINFO_CLASS_HANDLE clsHnd, var_types type, GenTree* op1)
{
    return new (this, GT_ALLOCOBJ) GenTreeAllocObj(type, helper, helperHasSideEffects, clsHnd, op1);
}

inline GenTree* Compiler::gtNewRuntimeLookup(CORINFO_GENERIC_HANDLE hnd, CorInfoGenericHandleType hndTyp, GenTree* tree)
{
    assert(tree != nullptr);
    return new (this, GT_RUNTIMELOOKUP) GenTreeRuntimeLookup(hnd, hndTyp, tree);
}

inline GenTree* Compiler::gtNewNullCheck(GenTree* addr)
{
    assert(varTypeIsI(addr->GetType()));
    assert(fgAddrCouldBeNull(addr));

    GenTreeIndir* nullCheck = new (this, GT_NULLCHECK) GenTreeIndir(GT_NULLCHECK, TYP_BYTE, addr);
    nullCheck->gtFlags |= GTF_EXCEPT;
    return nullCheck;
}

inline GenTreeIndir* Compiler::gtNewIndir(var_types type, GenTree* addr)
{
    assert(varTypeIsI(addr->GetType()));

    return new (this, GT_IND) GenTreeIndir(GT_IND, type, addr);
}

inline GenTreeFieldAddr* Compiler::gtNewFieldAddr(GenTree* addr, CORINFO_FIELD_HANDLE handle, unsigned offset)
{
    return gtNewFieldAddr(addr, GetFieldSeqStore()->CreateSingleton(handle), offset);
}

inline GenTreeFieldAddr* Compiler::gtNewFieldAddr(GenTree* addr, FieldSeqNode* fieldSeq, unsigned offset)
{
    // If "addr" is the address of a local, note that a field of that struct local has been accessed.
    if (addr->OperIs(GT_LCL_ADDR))
    {
        addr->AsLclAddr()->GetLcl()->lvFieldAccessed = true;
    }

    return new (this, GT_FIELD_ADDR) GenTreeFieldAddr(addr, fieldSeq, offset);
}

inline GenTreeIndir* Compiler::gtNewFieldIndir(var_types type, GenTreeFieldAddr* fieldAddr)
{
    assert(type != TYP_STRUCT);

    GenTreeIndir* indir = gtNewIndir(type, fieldAddr);
    indir->gtFlags |= gtGetFieldIndirFlags(fieldAddr);
    return indir;
}

inline GenTreeIndir* Compiler::gtNewFieldIndir(var_types type, unsigned layoutNum, GenTreeFieldAddr* fieldAddr)
{
    GenTreeIndir* indir;

    if (type == TYP_STRUCT)
    {
        indir = gtNewObjNode(typGetLayoutByNum(layoutNum), fieldAddr);
        // gtNewObjNode has other rules for adding GTF_GLOB_REF, remove it
        // and add it back below according to the old field rules.
        indir->gtFlags &= ~GTF_GLOB_REF;
    }
    else
    {
        indir = gtNewIndir(type, fieldAddr);
    }

    indir->gtFlags |= gtGetFieldIndirFlags(fieldAddr);

    return indir;
}

inline GenTreeIndexAddr* Compiler::gtNewArrayIndexAddr(GenTree* arr, GenTree* ind, var_types elemType)
{
    return new (this, GT_INDEX_ADDR)
        GenTreeIndexAddr(arr, ind, OFFSETOF__CORINFO_Array__length, OFFSETOF__CORINFO_Array__data, elemType);
}

inline GenTreeIndexAddr* Compiler::gtNewStringIndexAddr(GenTree* arr, GenTree* ind)
{
    return new (this, GT_INDEX_ADDR)
        GenTreeIndexAddr(arr, ind, OFFSETOF__CORINFO_String__stringLen, OFFSETOF__CORINFO_String__chars, TYP_USHORT);
}

inline GenTreeIndir* Compiler::gtNewIndexIndir(var_types type, GenTreeIndexAddr* indexAddr)
{
    GenTreeIndir* indir;

    if (type != TYP_STRUCT)
    {
        indir = gtNewIndir(type, indexAddr);
    }
    else
    {
        indir = gtNewObjNode(indexAddr->GetLayout(this), indexAddr);
    }

    indir->gtFlags |= GTF_GLOB_REF;

    if ((indexAddr->gtFlags & GTF_INX_RNGCHK) != 0)
    {
        indir->gtFlags |= GTF_IND_NONFAULTING;
    }
    else
    {
        indir->gtFlags |= GTF_EXCEPT;
    }

    return indir;
}

inline GenTreeArrLen* Compiler::gtNewArrLen(GenTree* arr, uint8_t lenOffs, GenTreeFlags flags = GTF_EXCEPT)
{
    return new (this, GT_ARR_LENGTH) GenTreeArrLen(arr, lenOffs, flags);
}

inline GenTreeBoundsChk* Compiler::gtNewBoundsChk(GenTree* index, GenTree* length, ThrowHelperKind kind)
{
    return new (this, GT_BOUNDS_CHECK) GenTreeBoundsChk(index, length, kind);
}

// Create (and check for) a "nothing" node, i.e. a node that doesn't produce
// any code. We currently use a "nop" node of type void for this purpose.
inline GenTree* Compiler::gtNewNothingNode()
{
    return new (this, GT_NOP) GenTreeOp(GT_NOP, TYP_VOID);
}

inline bool GenTree::IsNothingNode() const
{
    return (gtOper == GT_NOP) && (gtType == TYP_VOID);
}

inline void GenTree::ChangeToNothingNode()
{
    ChangeOper(GT_NOP);

    gtType        = TYP_VOID;
    AsOp()->gtOp1 = nullptr;
    AsOp()->gtOp2 = nullptr;

    gtFlags &= ~(GTF_ALL_EFFECT | GTF_REVERSE_OPS);
}

inline GenTree* Compiler::gtUnusedValNode(GenTree* expr)
{
    return gtNewCommaNode(expr, gtNewNothingNode());
}

inline GenTreeCast* Compiler::gtNewCastNode(GenTree* op1, bool fromUnsigned, var_types toType)
{
    return new (this, GT_CAST) GenTreeCast(toType, op1, fromUnsigned);
}

inline GenTreeIndir* Compiler::gtNewMethodTableLookup(GenTree* object)
{
    GenTreeIndir* result = gtNewIndir(TYP_I_IMPL, object);
    // TODO-MIKE-Review: In theory we could avoid setting GTF_EXCEPT when
    // the object is a string literal or a boxed struct used for static
    // struct fields. fgAddrCouldBeNull checks for those but it's overkill
    // since we basically never hit such cases.
    result->gtFlags |= GTF_IND_INVARIANT | GTF_EXCEPT;
    return result;
}

inline void GenTree::SetOperRaw(genTreeOps oper)
{
#if NODEBASH_STATS
    RecordOperBashing(gtOper, oper);
#endif

    gtOper = oper;
}

inline void GenTree::SetOper(genTreeOps oper, ValueNumberUpdate vnUpdate)
{
    assert(((gtDebugFlags & GTF_DEBUG_NODE_SMALL) != 0) != ((gtDebugFlags & GTF_DEBUG_NODE_LARGE) != 0));
    assert((s_gtNodeSizes[gtOper] == TREE_NODE_SZ_SMALL) || (s_gtNodeSizes[gtOper] == TREE_NODE_SZ_LARGE));
    assert((s_gtNodeSizes[oper] == TREE_NODE_SZ_SMALL) || (s_gtNodeSizes[oper] == TREE_NODE_SZ_LARGE));
    assert((s_gtNodeSizes[oper] == TREE_NODE_SZ_SMALL) || ((gtDebugFlags & GTF_DEBUG_NODE_LARGE) != 0));

#if defined(HOST_64BIT) && !defined(TARGET_64BIT)
    if ((gtOper == GT_CNS_LNG) && (oper == GT_CNS_INT))
    {
        // When converting from LONG to INT, we need to explicitly truncate the LONG value to INT,
        // if the host architecture represents INT and LONG with the same type (int64_t).
        AsLngCon()->SetValue(static_cast<int64_t>(static_cast<int32_t>(AsLngCon()->GetValue())));
    }
#endif

    SetOperRaw(oper);

#if DEBUGGABLE_GENTREE
    // Change the vtable of the node, so that it shows up correctly in the debugger.
    SetVtableForOper(oper);
#endif

    if (oper == GT_CNS_INT)
    {
        AsIntCon()->SetFieldSeq(FieldSeqStore::NotAField());
    }

    if (vnUpdate == CLEAR_VN)
    {
        m_vnp = {};
    }
}

inline void GenTree::SetOperResetFlags(genTreeOps oper)
{
    SetOper(oper);
    gtFlags = GTF_NONE;
}

inline void GenTree::ChangeOper(genTreeOps oper, ValueNumberUpdate vnUpdate)
{
    assert(!OperIsConst(oper));  // use ChangeOperConst
    assert(oper != GT_LCL_ADDR); // use ChangeToLclAddr

    GenTreeFlags mask = GTF_COMMON_MASK;

    if (OperIsIndirOrArrLength() && OperIsIndirOrArrLength(oper))
    {
        mask |= GTF_IND_NONFAULTING;
    }

    SetOper(oper, vnUpdate);

    gtFlags &= mask;

    switch (oper)
    {
        case GT_FIELD_LIST:
            AsFieldList()->SetType(TYP_STRUCT);
            AsFieldList()->ClearFields();
            AsFieldList()->SetContained();
            break;

        case GT_LCL_FLD:
        case GT_STORE_LCL_FLD:
            AsLclFld()->SetLayoutNum(0);
            AsLclFld()->SetLclOffs(0);
            AsLclFld()->SetFieldSeq(FieldSeqStore::NotAField());
            break;

        default:
            break;
    }
}

inline void GenTree::ChangeOperUnchecked(genTreeOps oper)
{
    GenTreeFlags mask = GTF_COMMON_MASK;

    if (OperIsIndirOrArrLength() && OperIsIndirOrArrLength(oper))
    {
        mask |= GTF_IND_NONFAULTING;
    }

    SetOperRaw(oper); // Trust the caller and don't use SetOper()

    gtFlags &= mask;
}

inline void GenTree::ChangeOperConst(genTreeOps oper)
{
    assert(OperIsConst(oper)); // use ChangeOper/ChangeToIntCon/ChangeToLngCon/ChangeToDblCon

    SetOperResetFlags(oper);

    if (oper == GT_CNS_INT)
    {
        AsIntCon()->SetFieldSeq(FieldSeqStore::NotAField());
    }
}

inline GenTreeIntCon* GenTree::ChangeToIntCon(ssize_t value)
{
#ifdef TARGET_64BIT
    assert((gtType == TYP_INT) || (gtType == TYP_LONG) || (gtType == TYP_REF) || (gtType == TYP_BYREF));
    assert((gtType != TYP_INT) || ((INT32_MIN <= value) && (value <= INT32_MAX)));
#else
    assert((gtType == TYP_INT) || (gtType == TYP_REF) || (gtType == TYP_BYREF));
#endif

    SetOperResetFlags(GT_CNS_INT);

    GenTreeIntCon* intCon = AsIntCon();
    intCon->SetValue(value);
    intCon->SetCompileTimeHandle(nullptr);
    INDEBUG(intCon->SetDumpHandle(nullptr));
    return intCon;
}

inline GenTreeIntCon* GenTree::ChangeToIntCon(var_types type, ssize_t value)
{
    SetType(varActualType(type));
    return ChangeToIntCon(value);
}

#ifndef TARGET_64BIT
inline GenTreeLngCon* GenTree::ChangeToLngCon(int64_t value)
{
    SetOperResetFlags(GT_CNS_LNG);

    GenTreeLngCon* lngCon = AsLngCon();
    lngCon->SetType(TYP_LONG);
    lngCon->SetValue(value);
    return lngCon;
}
#endif

inline GenTreeDblCon* GenTree::ChangeToDblCon(double value)
{
    assert(varTypeIsFloating(gtType));

    SetOperResetFlags(GT_CNS_DBL);

    GenTreeDblCon* dblCon = AsDblCon();
    dblCon->SetValue(value);
    return dblCon;
}

inline GenTreeDblCon* GenTree::ChangeToDblCon(var_types type, double value)
{
    assert(varTypeIsFloating(type));

    SetType(type);
    return ChangeToDblCon(value);
}

inline GenTreeFieldList* GenTree::ChangeToFieldList()
{
    SetOperResetFlags(GT_FIELD_LIST);

    GenTreeFieldList* fieldList = AsFieldList();
    fieldList->SetType(TYP_STRUCT);
    fieldList->ClearFields();
    fieldList->SetContained();
    return fieldList;
}

inline GenTreeLclFld* GenTree::ChangeToLclFld(var_types type, LclVarDsc* lcl, unsigned offset, FieldSeqNode* fieldSeq)
{
    assert(offset <= UINT16_MAX);
    assert((fieldSeq == nullptr) || (fieldSeq == FieldSeqNode::NotAField()) || fieldSeq->IsField());

    SetOperResetFlags(GT_LCL_FLD);

    GenTreeLclFld* lclFld = AsLclFld();
    lclFld->SetType(type);
    lclFld->SetLcl(lcl);
    lclFld->SetLclOffs(offset);
    lclFld->SetLayoutNum(0);
    lclFld->SetFieldSeq(fieldSeq == nullptr ? FieldSeqNode::NotAField() : fieldSeq);
    return lclFld;
}

inline GenTreeLclAddr* GenTree::ChangeToLclAddr(var_types type, LclVarDsc* lcl)
{
    // TODO-MIKE-Review: GTF_VAR_CLONED should not be needed on LCL_ADDR. Inlining
    // needs it only on params that are neither struct nor address taken and there
    // should be no need to ever take the address of such params. But if that does
    // happen we'd be left with an inlinee param that's used but not initialized,
    // can this be detected somehow? Maybe negate the flag, have the inliner set it
    // and CloneExpr remove it, then we can check here if we're trying to take the
    // address of such a param.

    SetOper(GT_LCL_ADDR);
    gtFlags = GTF_NONE;

    GenTreeLclAddr* addr = AsLclAddr();
    addr->SetType(type);
    addr->SetLcl(lcl);
    addr->SetLclOffs(0);
    addr->SetFieldSeq(nullptr);
    return addr;
}

inline GenTreeLclAddr* GenTree::ChangeToLclAddr(var_types type, LclVarDsc* lcl, unsigned offset, FieldSeqNode* fieldSeq)
{
    assert(offset <= UINT16_MAX);
    assert((fieldSeq == FieldSeqNode::NotAField()) || fieldSeq->IsField());

    SetOper(GT_LCL_ADDR);
    gtFlags = GTF_NONE;

    GenTreeLclAddr* addr = AsLclAddr();
    addr->SetType(type);
    addr->SetLcl(lcl);
    addr->SetLclOffs(offset);
    addr->SetFieldSeq(fieldSeq);
    return addr;
}

inline GenTreeAddrMode* GenTree::ChangeToAddrMode(GenTree* base, GenTree* index, unsigned scale, int offset)
{
    SetOperResetFlags(GT_LEA);

    GenTreeAddrMode* addrMode = AsAddrMode();
    addrMode->SetBase(base);
    addrMode->SetIndex(index);
    addrMode->SetScale(scale);
    addrMode->SetOffset(offset);
    return addrMode;
}

/*****************************************************************************
 *
 * Returns true if the node is of the "ovf" variety, for example, add.ovf.i1.
 * + gtOverflow() can only be called for valid operators (that is, we know it is one
 *   of the operators which may have GTF_OVERFLOW set).
 * + gtOverflowEx() is more expensive, and should be called only if gtOper may be
 *   an operator for which GTF_OVERFLOW is invalid.
 */

inline bool GenTree::gtOverflow() const
{
    assert(OperMayOverflow());

    if ((gtFlags & GTF_OVERFLOW) != 0)
    {
        assert(varTypeIsIntegral(TypeGet()));

        return true;
    }
    else
    {
        return false;
    }
}

inline bool GenTree::gtOverflowEx() const
{
    return OperMayOverflow() && gtOverflow();
}

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                          LclVarsInfo                                      XX
XX                      Inline functions                                     XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

inline LclVarDsc* Compiler::lvaNewTemp(var_types type, bool shortLifetime DEBUGARG(const char* reason))
{
    LclVarDsc* lcl = lvaAllocTemp(shortLifetime DEBUGARG(reason));
    lcl->SetType(type);
    return lcl;
}

inline LclVarDsc* Compiler::lvaNewTemp(ClassLayout* layout, bool shortLifetime DEBUGARG(const char* reason))
{
    assert(layout->IsValueClass());

    LclVarDsc* lcl = lvaAllocTemp(shortLifetime DEBUGARG(reason));
    lvaSetStruct(lcl, layout, false);
    return lcl;
}

inline LclVarDsc* Compiler::lvaNewTemp(CORINFO_CLASS_HANDLE classHandle,
                                       bool shortLifetime DEBUGARG(const char* reason))
{
    assert(info.compCompHnd->isValueClass(classHandle));

    LclVarDsc* lcl = lvaAllocTemp(shortLifetime DEBUGARG(reason));
    lvaSetStruct(lcl, typGetObjLayout(classHandle), false);
    return lcl;
}

inline LclVarDsc* Compiler::lvaNewTemp(GenTree* tree, bool shortLifetime DEBUGARG(const char* reason))
{
    assert(varTypeIsSIMD(tree->GetType())); // Only SIMD temps are supported for now.

    LclVarDsc* lcl      = lvaAllocTemp(shortLifetime DEBUGARG(reason));
    ClassLayout* layout = typGetVectorLayout(tree);

    if (layout != nullptr)
    {
        lvaSetStruct(lcl, layout, false);
    }
    else
    {
        lcl->lvType = tree->GetType();
    }

    return lcl;
}

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                          Importer                                         XX
XX                      Inline functions                                     XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#ifdef TARGET_ARMARCH
inline var_types Compiler::mangleVarArgsType(var_types type)
{
    if (!opts.UseSoftFP()
#ifdef TARGET_WINDOWS
        && !info.compIsVarArgs
#endif
        )
    {
        return type;
    }

    switch (type)
    {
        case TYP_FLOAT:
            return TYP_INT;
        case TYP_DOUBLE:
            return TYP_LONG;
        default:
            return type;
    }
}
#endif // TARGET_ARMARCH

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                       FlowGraph                                           XX
XX                      Inline functions                                     XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

/*
    Small inline function to change a given block to a throw block.

*/
inline void Compiler::fgConvertBBToThrowBB(BasicBlock* block)
{
    JITDUMP("Converting " FMT_BB " to BBJ_THROW\n", block->bbNum);

    // Ordering of the following operations matters.
    // First, note if we are looking at the first block of a call always pair.
    const bool isCallAlwaysPair = block->isBBCallAlwaysPair();

    // Scrub this block from the pred lists of any successors
    fgRemoveBlockAsPred(block);

    // Update jump kind after the scrub.
    block->bbJumpKind = BBJ_THROW;

    // Any block with a throw is rare
    block->bbSetRunRarely();

    // If we've converted a BBJ_CALLFINALLY block to a BBJ_THROW block,
    // then mark the subsequent BBJ_ALWAYS block as unreferenced.
    //
    // Must do this after we update bbJumpKind of block.
    if (isCallAlwaysPair)
    {
        BasicBlock* leaveBlk = block->bbNext;
        noway_assert(leaveBlk->bbJumpKind == BBJ_ALWAYS);

        // leaveBlk is now unreachable, so scrub the pred lists.
        leaveBlk->bbFlags &= ~BBF_DONT_REMOVE;
        leaveBlk->bbRefs  = 0;
        leaveBlk->bbPreds = nullptr;

#if defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
        // This function (fgConvertBBToThrowBB) can be called before the predecessor lists are created (e.g., in
        // fgMorph). The fgClearFinallyTargetBit() function to update the BBF_FINALLY_TARGET bit depends on these
        // predecessor lists. If there are no predecessor lists, we immediately clear all BBF_FINALLY_TARGET bits
        // (to allow subsequent dead code elimination to delete such blocks without asserts), and set a flag to
        // recompute them later, before they are required.
        if (fgComputePredsDone)
        {
            fgClearFinallyTargetBit(leaveBlk->bbJumpDest);
        }
        else
        {
            fgClearAllFinallyTargetBits();
            fgNeedToAddFinallyTargetBits = true;
        }
#endif // defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
    }
}

/*****************************************************************************
  Is the offset too big?
*/
inline bool Compiler::fgIsBigOffset(size_t offset)
{
    return (offset > compMaxUncheckedOffsetForNullObject);
}

#ifdef DEBUG
/*****************************************************************************
 *  Should we enable JitStress mode?
 *   0:   No stress
 *   !=2: Vary stress. Performance will be slightly/moderately degraded
 *   2:   Check-all stress. Performance will be REALLY horrible
 */

inline int getJitStressLevel()
{
    return JitConfig.JitStress();
}

#endif // DEBUG

/*****************************************************************************/
/* Map a register argument number ("RegArgNum") to a register number ("RegNum").
 * A RegArgNum is in this range:
 *      [0, MAX_REG_ARG)        -- for integer registers
 *      [0, MAX_FLOAT_REG_ARG)  -- for floating point registers
 * Note that RegArgNum's are overlapping for integer and floating-point registers,
 * while RegNum's are not (for ARM anyway, though for x86, it might be different).
 * If we have a fixed return buffer register and are given it's index
 * we return the fixed return buffer register
 */

inline regNumber genMapIntRegArgNumToRegNum(unsigned argNum)
{
#ifdef TARGET_ARM64
    if (argNum == RET_BUFF_ARGNUM)
    {
        return REG_ARG_RET_BUFF;
    }
#endif

    assert(argNum < ArrLen(intArgRegs));

    return intArgRegs[argNum];
}

inline regNumber genMapFloatRegArgNumToRegNum(unsigned argNum)
{
#ifndef TARGET_X86
    assert(argNum < ArrLen(fltArgRegs));

    return fltArgRegs[argNum];
#else
    assert(!"no x86 float arg regs\n");
    return REG_NA;
#endif
}

__forceinline regNumber genMapRegArgNumToRegNum(unsigned argNum, var_types type)
{
    if (varTypeUsesFloatArgReg(type))
    {
        return genMapFloatRegArgNumToRegNum(argNum);
    }
    else
    {
        return genMapIntRegArgNumToRegNum(argNum);
    }
}

/*****************************************************************************/
/* Map a register argument number ("RegArgNum") to a register mask of the associated register.
 * Note that for floating-pointer registers, only the low register for a register pair
 * (for a double on ARM) is returned.
 */

inline regMaskTP genMapIntRegArgNumToRegMask(unsigned argNum)
{
    assert(argNum < ArrLen(intArgMasks));

    return intArgMasks[argNum];
}

inline regMaskTP genMapFloatRegArgNumToRegMask(unsigned argNum)
{
#ifndef TARGET_X86
    assert(argNum < ArrLen(fltArgMasks));

    return fltArgMasks[argNum];
#else
    assert(!"no x86 float arg regs\n");
    return RBM_NONE;
#endif
}

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                          Optimizer                                        XX
XX                      Inline functions                                     XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

inline LclVarDsc* Compiler::LoopDsc::lpIterVar() const
{
    INDEBUG(VerifyIterator());
    return lpIterTree->GetLcl();
}

inline int Compiler::LoopDsc::lpIterConst() const
{
    INDEBUG(VerifyIterator());
    return lpIterTree->GetOp(0)->AsOp()->GetOp(1)->AsIntCon()->GetInt32Value();
}

inline genTreeOps Compiler::LoopDsc::lpIterOper() const
{
    INDEBUG(VerifyIterator());
    return lpIterTree->GetOp(0)->GetOper();
}

inline bool Compiler::LoopDsc::lpIsReversed() const
{
    INDEBUG(VerifyIterator());
    return lpTestTree->GetOp(1)->OperIs(GT_LCL_VAR) &&
           (lpTestTree->GetOp(1)->AsLclVar()->GetLcl() == lpIterTree->GetLcl());
}

inline genTreeOps Compiler::LoopDsc::lpTestOper() const
{
    INDEBUG(VerifyIterator());
    return lpIsReversed() ? GenTree::SwapRelop(lpTestTree->GetOper()) : lpTestTree->GetOper();
}

inline GenTree* Compiler::LoopDsc::lpIterator() const
{
    INDEBUG(VerifyIterator());
    return lpIsReversed() ? lpTestTree->GetOp(1) : lpTestTree->AsOp()->GetOp(0);
}

inline GenTree* Compiler::LoopDsc::lpLimit() const
{
    INDEBUG(VerifyIterator());
    return lpIsReversed() ? lpTestTree->GetOp(0) : lpTestTree->GetOp(1);
}

inline int Compiler::LoopDsc::lpConstLimit() const
{
    INDEBUG(VerifyIterator());
    assert(lpFlags & LPFLG_CONST_LIMIT);

    GenTree* limit = lpLimit();
    return limit->AsIntCon()->GetInt32Value();
}

inline LclVarDsc* Compiler::LoopDsc::lpVarLimit() const
{
    INDEBUG(VerifyIterator());
    assert(lpFlags & LPFLG_VAR_LIMIT);

    GenTree* limit = lpLimit();
    assert(limit->OperIs(GT_LCL_VAR));
    return limit->AsLclVar()->GetLcl();
}

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                Optimization activation rules                              XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

// should we try to replace integer multiplication with lea/add/shift sequences?
inline bool Compiler::optAvoidIntMult(void)
{
    return (compCodeOpt() != SMALL_CODE);
}

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                          EEInterface                                      XX
XX                      Inline functions                                     XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

extern var_types JITtype2varType(CorInfoType type);

#include "ee_il_dll.hpp"

inline CORINFO_METHOD_HANDLE Compiler::eeFindHelper(unsigned helper)
{
    assert(helper < CORINFO_HELP_COUNT);

    /* Helpers are marked by the fact that they are odd numbers
     * force this to be an odd number (will shift it back to extract) */

    return ((CORINFO_METHOD_HANDLE)((((size_t)helper) << 2) + 1));
}

inline CorInfoHelpFunc Compiler::eeGetHelperNum(CORINFO_METHOD_HANDLE method)
{
    // Helpers are marked by the fact that they are odd numbers
    if (!(((size_t)method) & 1))
    {
        return (CORINFO_HELP_UNDEF);
    }
    return ((CorInfoHelpFunc)(((size_t)method) >> 2));
}

//  TODO-Cleanup: Replace calls to IsSharedStaticHelper with new HelperCallProperties
//

inline bool Compiler::IsSharedStaticHelper(GenTree* tree)
{
    if (tree->gtOper != GT_CALL || tree->AsCall()->gtCallType != CT_HELPER)
    {
        return false;
    }

    CorInfoHelpFunc helper = eeGetHelperNum(tree->AsCall()->gtCallMethHnd);

    bool result1 =
        // More helpers being added to IsSharedStaticHelper (that have similar behaviors but are not true
        // ShareStaticHelperts)
        helper == CORINFO_HELP_STRCNS || helper == CORINFO_HELP_BOX ||

        // helpers being added to IsSharedStaticHelper
        helper == CORINFO_HELP_GETSTATICFIELDADDR_CONTEXT || helper == CORINFO_HELP_GETSTATICFIELDADDR_TLS ||
        helper == CORINFO_HELP_GETGENERICS_GCSTATIC_BASE || helper == CORINFO_HELP_GETGENERICS_NONGCSTATIC_BASE ||
        helper == CORINFO_HELP_GETGENERICS_GCTHREADSTATIC_BASE ||
        helper == CORINFO_HELP_GETGENERICS_NONGCTHREADSTATIC_BASE ||

        helper == CORINFO_HELP_GETSHARED_GCSTATIC_BASE || helper == CORINFO_HELP_GETSHARED_NONGCSTATIC_BASE ||
        helper == CORINFO_HELP_GETSHARED_GCSTATIC_BASE_NOCTOR ||
        helper == CORINFO_HELP_GETSHARED_NONGCSTATIC_BASE_NOCTOR ||
        helper == CORINFO_HELP_GETSHARED_GCSTATIC_BASE_DYNAMICCLASS ||
        helper == CORINFO_HELP_GETSHARED_NONGCSTATIC_BASE_DYNAMICCLASS ||
        helper == CORINFO_HELP_GETSHARED_GCTHREADSTATIC_BASE ||
        helper == CORINFO_HELP_GETSHARED_NONGCTHREADSTATIC_BASE ||
        helper == CORINFO_HELP_GETSHARED_GCTHREADSTATIC_BASE_NOCTOR ||
        helper == CORINFO_HELP_GETSHARED_NONGCTHREADSTATIC_BASE_NOCTOR ||
        helper == CORINFO_HELP_GETSHARED_GCTHREADSTATIC_BASE_DYNAMICCLASS ||
        helper == CORINFO_HELP_GETSHARED_NONGCTHREADSTATIC_BASE_DYNAMICCLASS ||
#ifdef FEATURE_READYTORUN_COMPILER
        helper == CORINFO_HELP_READYTORUN_STATIC_BASE || helper == CORINFO_HELP_READYTORUN_GENERIC_STATIC_BASE ||
#endif
        helper == CORINFO_HELP_CLASSINIT_SHARED_DYNAMICCLASS;
#if 0
    // See above TODO-Cleanup
    bool result2 = s_helperCallProperties.IsPure(helper) && s_helperCallProperties.NonNullReturn(helper);
    assert (result1 == result2);
#endif
    return result1;
}

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                          Compiler                                         XX
XX                      Inline functions                                     XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#ifndef DEBUG
inline bool Compiler::compStressCompile(compStressArea stressArea, unsigned weightPercentage)
{
    return false;
}
#endif

inline ArenaAllocator* Compiler::compGetArenaAllocator()
{
    return compArenaAllocator;
}

inline bool Compiler::compIsProfilerHookNeeded() const
{
#ifdef PROFILING_SUPPORTED
    return compProfilerHookNeeded
           // IL stubs are excluded by VM and we need to do the same even running
           // under a complus env hook to generate profiler hooks
           || (opts.compJitELTHookEnabled && !opts.jitFlags->IsSet(JitFlags::JIT_FLAG_IL_STUB));
#else
    return false;
#endif
}

/*****************************************************************************
 *
 *  Returns true if the compiler instance is created for inlining.
 */

inline bool Compiler::compIsForInlining() const
{
    return (impInlineInfo != nullptr);
}

#if MEASURE_CLRAPI_CALLS

inline void Compiler::CLRApiCallEnter(unsigned apix)
{
    if (pCompJitTimer != nullptr)
    {
        pCompJitTimer->CLRApiCallEnter(apix);
    }
}
inline void Compiler::CLRApiCallLeave(unsigned apix)
{
    if (pCompJitTimer != nullptr)
    {
        pCompJitTimer->CLRApiCallLeave(apix);
    }
}

inline void Compiler::CLR_API_Enter(API_ICorJitInfo_Names ename)
{
    CLRApiCallEnter(ename);
}

inline void Compiler::CLR_API_Leave(API_ICorJitInfo_Names ename)
{
    CLRApiCallLeave(ename);
}

#endif // MEASURE_CLRAPI_CALLS

//------------------------------------------------------------------------------
// fgVarNeedsExplicitZeroInit : Check whether the variable needs an explicit zero initialization.
//
// Arguments:
//    varNum     -       local var number
//    bbInALoop  -       true if the basic block may be in a loop
//    bbIsReturn -       true if the basic block always returns
//
// Returns:
//             true if the var needs explicit zero-initialization in this basic block;
//             false otherwise
//
// Notes:
//     If the variable is not being initialized in a loop, we can avoid explicit zero initialization if
//      - the variable is a gc pointer, or
//      - the variable is a struct with gc pointer fields and either all fields are gc pointer fields
//           or the struct is big enough to guarantee block initialization, or
//      - compInitMem is set and the variable has a long lifetime or has gc fields.
//     In these cases we will insert zero-initialization in the prolog if necessary.

bool Compiler::fgVarNeedsExplicitZeroInit(LclVarDsc* varDsc, bool bbInALoop, bool bbIsReturn)
{
    if (varDsc->IsDependentPromotedField(this))
    {
        // Fields of dependently promoted structs may only be initialized in the prolog
        // when the whole struct is initialized in the prolog.
        varDsc = lvaGetDesc(varDsc->GetPromotedFieldParentLclNum());
    }

    if (bbInALoop && !bbIsReturn)
    {
        return true;
    }

    if (lvaIsNeverZeroInitializedInProlog(varDsc))
    {
        return true;
    }

    if (varTypeIsGC(varDsc->GetType()))
    {
        return false;
    }

    if (varDsc->TypeIs(TYP_STRUCT) && varDsc->HasGCPtr())
    {
        ClassLayout* layout = varDsc->GetLayout();
        if (layout->GetSlotCount() == layout->GetGCPtrCount())
        {
            return false;
        }

        // Below conditions guarantee block initialization, which will initialize
        // all struct fields. If the logic for block initialization in CodeGen::CheckUseBlockInit()
        // changes, these conditions need to be updated.
        unsigned intSlots = roundUp(varDsc->GetFrameSize(), REGSIZE_BYTES) / 4;

#ifndef TARGET_64BIT
        if (intSlots > 4)
#elif defined(TARGET_AMD64)
        // We can clear using aligned SIMD so the threshold is lower,
        // and clears in order which is better for auto-prefetching
        if (intSlots > 4)
#else
        if (intSlots > 8)
#endif
        {
            return false;
        }
    }

    return !info.compInitMem || (varDsc->lvIsTemp && !varDsc->HasGCPtr());
}

// Note that compiler's allocator is an arena allocator that returns memory that is
// not zero-initialized and can contain data from a prior allocation lifetime.
inline void* __cdecl operator new(size_t sz, Compiler* compiler, CompMemKind cmk)
{
    return compiler->getAllocator(cmk).allocate<char>(sz);
}

inline void* __cdecl operator new[](size_t sz, Compiler* compiler, CompMemKind cmk)
{
    return compiler->getAllocator(cmk).allocate<char>(sz);
}

// This node should not be referenced by anyone now. Set its values to garbage
// to catch extra references
inline void DEBUG_DESTROY_NODE(GenTree* tree)
{
#ifdef DEBUG
    // Save gtOper in case we want to find out what this node was
    tree->gtOperSave = tree->gtOper;
    tree->gtType     = TYP_UNDEF;

    if (tree->OperIsSimple())
    {
        tree->AsOp()->gtOp1 = nullptr;
        tree->AsOp()->gtOp2 = nullptr;
    }

    // Must do this last, because the "AsOp()" check above will fail otherwise.
    // Don't call SetOper, because GT_COUNT is not a valid value.
    tree->gtOper = GT_COUNT;
#endif
}

inline unsigned LclVarDsc::GetRefCount() const
{
    assert(JitTls::GetCompiler()->lvaRefCountState == RCS_NORMAL);

    return m_refCount;
}

inline void LclVarDsc::SetRefCount(unsigned count)
{
    assert(JitTls::GetCompiler()->lvaRefCountState == RCS_NORMAL);

    m_refCount = static_cast<uint16_t>(count > UINT16_MAX ? UINT16_MAX : count);
}

inline BasicBlock::weight_t LclVarDsc::GetRefWeight() const
{
    assert(JitTls::GetCompiler()->lvaRefCountState == RCS_NORMAL);

    return jitstd::bit_cast<BasicBlock::weight_t>(m_refWeight);
}

inline void LclVarDsc::SetRefWeight(BasicBlock::weight_t weight)
{
    assert(JitTls::GetCompiler()->lvaRefCountState == RCS_NORMAL);

    m_refWeight = jitstd::bit_cast<uint32_t>(weight);
}

/*****************************************************************************/
#endif //_COMPILER_HPP_
/*****************************************************************************/
