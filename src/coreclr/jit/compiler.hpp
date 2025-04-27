// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

// Return the EH descriptor for the given region index.
inline EHblkDsc* Compiler::ehGetDsc(unsigned regionIndex)
{
    assert(regionIndex < compHndBBtabCount);
    return &compHndBBtab[regionIndex];
}

// Return the EH descriptor index of the enclosing try, for the given region index.
inline unsigned Compiler::ehGetEnclosingTryIndex(unsigned regionIndex)
{
    return ehGetDsc(regionIndex)->ebdEnclosingTryIndex;
}

// Return the EH descriptor index of the enclosing handler, for the given region index.
inline unsigned Compiler::ehGetEnclosingHndIndex(unsigned regionIndex)
{
    return ehGetDsc(regionIndex)->ebdEnclosingHndIndex;
}

// Return the EH index given a region descriptor.
inline unsigned Compiler::ehGetIndex(EHblkDsc* ehDsc)
{
    assert(compHndBBtab <= ehDsc && ehDsc < compHndBBtab + compHndBBtabCount);
    return static_cast<unsigned>(ehDsc - compHndBBtab);
}

// Return the EH descriptor for the most nested 'try' region this BasicBlock is a member of
// (or nullptr if this block is not in a 'try' region).
inline EHblkDsc* Compiler::ehGetBlockTryDsc(BasicBlock* block)
{
    return block->hasTryIndex() ? ehGetDsc(block->getTryIndex()) : nullptr;
}

// Return the EH descriptor for the most nested filter or handler region this BasicBlock is a member of
// (or nullptr if this block is not in a filter or handler region).
inline EHblkDsc* Compiler::ehGetBlockHndDsc(BasicBlock* block)
{
    return block->hasHndIndex() ? ehGetDsc(block->getHndIndex()) : nullptr;
}

//  Helpers to pull little-endian values out of a byte stream.
inline uint8_t getU1LittleEndian(const uint8_t* ptr)
{
    return *ptr;
}

inline uint16_t getU2LittleEndian(const uint8_t* ptr)
{
    return GET_UNALIGNED_VAL16(ptr);
}

inline uint32_t getU4LittleEndian(const uint8_t* ptr)
{
    return GET_UNALIGNED_VAL32(ptr);
}

inline uint64_t getU8LittleEndian(const uint8_t* ptr)
{
    return GET_UNALIGNED_VAL64(ptr);
}

inline int8_t getI1LittleEndian(const uint8_t* ptr)
{
    return *reinterpret_cast<const int8_t*>(ptr);
}

inline int16_t getI2LittleEndian(const uint8_t* ptr)
{
    return GET_UNALIGNED_VAL16(ptr);
}

inline int32_t getI4LittleEndian(const uint8_t* ptr)
{
    return GET_UNALIGNED_VAL32(ptr);
}

inline int64_t getI8LittleEndian(const uint8_t* ptr)
{
    return GET_UNALIGNED_VAL64(ptr);
}

inline float getR4LittleEndian(const uint8_t* ptr)
{
    return jitstd::bit_cast<float>(getU4LittleEndian(ptr));
}

inline double getR8LittleEndian(const uint8_t* ptr)
{
    return jitstd::bit_cast<double>(getU8LittleEndian(ptr));
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
    size_t size = s_gtNodeSizes[oper];

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
    assert(varTypeNodeType(type) == type);

#ifdef DEBUG
    assert((s_gtNodeSizes[oper] == TREE_NODE_SZ_SMALL) || (s_gtNodeSizes[oper] == TREE_NODE_SZ_LARGE));

    if ((s_gtNodeSizes[oper] == TREE_NODE_SZ_LARGE) || largeNode)
    {
        gtDebugFlags |= GTF_DEBUG_NODE_LARGE;
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

inline GenTree* Compiler::gtNewLargeOperNode(genTreeOps oper, var_types type, GenTree* op1, GenTree* op2)
{
    assert((GenTree::OperKind(oper) & (GTK_UNOP | GTK_BINOP)) != 0);
    // Can't use this to construct any types that extend unary/binary operator.
    assert((GenTree::OperKind(oper) & GTK_EXOP) == 0);
    assert(GenTree::s_gtNodeSizes[oper] == TREE_NODE_SZ_SMALL);
    // Allocate a large node
    return new (this, LargeOpOpcode()) GenTreeOp(oper, type, op1, op2 DEBUGARG(/*largeNode*/ true));
}

inline GenTreeIntCon* Compiler::gtNewIconHandleNode(void* addr, HandleKind kind, FieldSeqNode* fieldSeq)
{
    assert(kind != HandleKind::None);

    if (fieldSeq == nullptr)
    {
        fieldSeq = FieldSeqStore::NotAField();
    }

    return new (this, GT_CNS_INT) GenTreeIntCon(TYP_I_IMPL, addr, kind, fieldSeq);
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

inline GenTreeCall* Compiler::gtNewRuntimeLookupHelperCallNode(const CORINFO_RUNTIME_LOOKUP& lookup,
                                                               GenTree*                      ctxTree,
                                                               void*                         compileTimeHandle)
{
    GenTreeIntCon* argNode = gtNewIconHandleNode(lookup.signature, HandleKind::MutableData);
    argNode->SetCompileTimeHandle(compileTimeHandle);
    return gtNewHelperCallNode(lookup.helper, TYP_I_IMPL, gtNewCallArgs(ctxTree, argNode));
}

inline GenTree* Compiler::gtNewRuntimeLookup(CORINFO_GENERIC_HANDLE hnd, CorInfoGenericHandleType hndTyp, GenTree* tree)
{
    return new (this, GT_RUNTIMELOOKUP) GenTreeRuntimeLookup(hnd, hndTyp, tree);
}

inline GenTree* Compiler::gtNewNullCheck(GenTree* addr)
{
    assert(varTypeIsI(addr->GetType()));
    assert(fgAddrCouldBeNull(addr));

    return new (this, GT_NULLCHECK) GenTreeNullCheck(addr);
}

inline GenTreeAddrMode* Compiler::gtNewAddrMode(GenTree* base, int offset)
{
    return new (this, GT_LEA) GenTreeAddrMode(base, offset);
}

inline GenTreeIndLoad* Compiler::gtNewIndLoad(var_types type, GenTree* addr)
{
    assert(varTypeIsI(addr->GetType()));

    return new (this, GT_IND_LOAD) GenTreeIndLoad(type, addr);
}

inline GenTreeIndStore* Compiler::gtNewIndStore(var_types type, GenTree* addr, GenTree* value)
{
    assert(varTypeIsI(addr->GetType()));
    assert(varTypeSize(value->GetType()) != 0);

    return new (this, GT_IND_STORE) GenTreeIndStore(type, addr, value);
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

inline GenTreeIndir* Compiler::gtNewMethodTableLookup(GenTree* object)
{
    GenTreeIndir* result = gtNewIndLoad(TYP_I_IMPL, object);
    // TODO-MIKE-Review: In theory we could avoid setting GTF_EXCEPT when
    // the object is a string literal or a boxed struct used for static
    // struct fields. fgAddrCouldBeNull checks for those but it's overkill
    // since we basically never hit such cases.
    result->gtFlags |= GTF_IND_INVARIANT | GTF_EXCEPT;
    return result;
}

inline void GenTree::SetOper(genTreeOps oper, ValueNumberUpdate vnUpdate)
{
    assert((s_gtNodeSizes[gtOper] == TREE_NODE_SZ_SMALL) || (s_gtNodeSizes[gtOper] == TREE_NODE_SZ_LARGE));
    assert((s_gtNodeSizes[oper] == TREE_NODE_SZ_SMALL) || (s_gtNodeSizes[oper] == TREE_NODE_SZ_LARGE));
    assert((s_gtNodeSizes[oper] == TREE_NODE_SZ_SMALL) || ((gtDebugFlags & GTF_DEBUG_NODE_LARGE) != 0));

#if NODEBASH_STATS
    RecordOperBashing(gtOper, oper);
#endif

#if defined(HOST_64BIT) && !defined(TARGET_64BIT)
    if ((gtOper == GT_CNS_LNG) && (oper == GT_CNS_INT))
    {
        // When converting from LONG to INT, we need to explicitly truncate the LONG value to INT,
        // if the host architecture represents INT and LONG with the same type (int64_t).
        AsLngCon()->SetValue(static_cast<int64_t>(static_cast<int32_t>(AsLngCon()->GetValue())));
    }
#endif

    gtOper = oper;

#if DEBUGGABLE_GENTREE
    // Change the vtable of the node, so that it shows up correctly in the debugger.
    SetVTable();
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
    assert(!OperIsConst(oper));       // use ChangeToInt/Dbl/LngCon
    assert(oper != GT_LCL_ADDR);      // use ChangeToLclAddr
    assert(oper != GT_FIELD_LIST);    // use ChangeToFieldList
    assert(oper != GT_LCL_LOAD_FLD);  // use ChangeToLclLoadFld
    assert(oper != GT_LCL_STORE_FLD); // use ChangeToLclStoreFld

    // This cannot be used to change a relop into another relop as it might
    // incorrectly reset GTF_RELOP_UNSIGNED/GTF_RELOP_NAN_UN. Use SetOper
    // instead and update the flags as needed.
    assert(!OperIsRelop(oper) || !OperIsRelop(gtOper));

    GenTreeFlags mask = GTF_COMMON_MASK;

    if (OperIsIndirOrArrLength() && OperIsIndirOrArrLength(oper))
    {
        mask |= GTF_IND_NONFAULTING;
    }

    SetOper(oper, vnUpdate);

    gtFlags &= mask;
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

inline GenTreeIntCon* GenTree::ChangeToIntCon(void* addr, HandleKind kind)
{
    SetOperResetFlags(GT_CNS_INT);

    GenTreeIntCon* intCon = AsIntCon();
    intCon->SetType(TYP_I_IMPL);
    intCon->SetAddr(addr, kind);
    intCon->SetCompileTimeHandle(nullptr);
    INDEBUG(intCon->SetDumpHandle(nullptr));
    return intCon;
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

inline GenTreeLclLoad* GenTree::ChangeToLclLoad(var_types type, LclVarDsc* lcl)
{
    SetOperResetFlags(GT_LCL_LOAD);

    GenTreeLclLoad* load = AsLclLoad();
    load->SetType(type);
    load->SetLcl(lcl);
    load->SetSideEffects(lcl->IsAddressExposed() ? GTF_GLOB_REF : GTF_NONE);
    return load;
}

inline GenTreeLclStore* GenTree::ChangeToLclStore(var_types type, LclVarDsc* lcl, GenTree* value)
{
    SetOperResetFlags(GT_LCL_STORE);

    GenTreeLclStore* store = AsLclStore();
    store->SetType(type);
    store->SetLcl(lcl);
    store->SetValue(value);
    store->SetSideEffects(GTF_ASG | value->GetSideEffects() | (lcl->IsAddressExposed() ? GTF_GLOB_REF : GTF_NONE));
    return store;
}

inline GenTreeLclLoadFld* GenTree::ChangeToLclLoadFld(var_types     type,
                                                      LclVarDsc*    lcl,
                                                      unsigned      offset,
                                                      FieldSeqNode* fieldSeq)
{
    assert(offset <= UINT16_MAX);
    assert((fieldSeq == nullptr) || (fieldSeq == FieldSeqNode::NotAField()) || fieldSeq->IsField());

    SetOperResetFlags(GT_LCL_LOAD_FLD);

    GenTreeLclLoadFld* load = AsLclLoadFld();
    load->SetType(type);
    load->SetLcl(lcl);
    load->SetLclOffs(offset);
    load->SetLayoutNum(0);
    load->SetFieldSeq(fieldSeq == nullptr ? FieldSeqNode::NotAField() : fieldSeq);
    load->SetSideEffects(lcl->IsAddressExposed() ? GTF_GLOB_REF : GTF_NONE);
    return load;
}

inline GenTreeLclStoreFld* GenTree::ChangeToLclStoreFld(
    var_types type, LclVarDsc* lcl, unsigned offset, FieldSeqNode* fieldSeq, GenTree* value)
{
    assert(offset <= UINT16_MAX);
    assert((fieldSeq == nullptr) || (fieldSeq == FieldSeqNode::NotAField()) || fieldSeq->IsField());

    SetOperResetFlags(GT_LCL_STORE_FLD);

    GenTreeLclStoreFld* store = AsLclStoreFld();
    store->SetType(type);
    store->SetLcl(lcl);
    store->SetLclOffs(offset);
    store->SetLayoutNum(0);
    store->SetFieldSeq(fieldSeq == nullptr ? FieldSeqNode::NotAField() : fieldSeq);
    store->SetValue(value);
    store->SetSideEffects(GTF_ASG | value->GetSideEffects() | (lcl->IsAddressExposed() ? GTF_GLOB_REF : GTF_NONE));
    return store;
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

    SetOperResetFlags(GT_LCL_ADDR);

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

    SetOperResetFlags(GT_LCL_ADDR);

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

// Helper to change tree oper to a NULLCHECK.
//
// The function should not be called after lowering for platforms that do not support
// emitting NULLCHECK nodes, like arm32. Use `Lowering::TransformUnusedIndirection`
// that handles it and calls this function when appropriate.
//
inline void Compiler::gtChangeOperToNullCheck(GenTree* tree)
{
    assert(tree->OperIs(GT_FIELD_ADDR, GT_IND_LOAD, GT_IND_LOAD_OBJ, GT_IND_LOAD_BLK));

    // TODO-MIKE-Cleanup: There are multiple places that have special handling for FIELD_ADDR.
    // All that could probably done here instead. See impImportPop, inlInitInlineeArgs and
    // gtTryRemoveBoxUpstreamEffects.

    tree->ChangeOper(GT_NULLCHECK);
    tree->SetType(TYP_INT);
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
    return lpIterTree->GetValue()->AsOp()->GetOp(1)->AsIntCon()->GetInt32Value();
}

inline genTreeOps Compiler::LoopDsc::lpIterOper() const
{
    INDEBUG(VerifyIterator());
    return lpIterTree->GetValue()->GetOper();
}

inline bool Compiler::LoopDsc::lpIsReversed() const
{
    INDEBUG(VerifyIterator());
    return lpTestTree->GetOp(1)->OperIs(GT_LCL_LOAD) &&
           (lpTestTree->GetOp(1)->AsLclLoad()->GetLcl() == lpIterTree->GetLcl());
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
    return lpLimit()->AsIntCon()->GetInt32Value();
}

inline LclVarDsc* Compiler::LoopDsc::lpVarLimit() const
{
    INDEBUG(VerifyIterator());
    assert(lpFlags & LPFLG_VAR_LIMIT);
    return lpLimit()->AsLclLoad()->GetLcl();
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

#include "ee_il_dll.hpp"

inline CORINFO_METHOD_HANDLE Compiler::eeFindHelper(unsigned helper)
{
    assert(helper < CORINFO_HELP_COUNT);

    // Helpers are marked by the fact that they are odd numbers
    // force this to be an odd number (will shift it back to extract)

    return reinterpret_cast<CORINFO_METHOD_HANDLE>((static_cast<uintptr_t>(helper) << 2) + 1);
}

inline CorInfoHelpFunc Compiler::eeGetHelperNum(CORINFO_METHOD_HANDLE method)
{
    if ((reinterpret_cast<uintptr_t>(method) & 1) == 0)
    {
        return CORINFO_HELP_UNDEF;
    }

    return static_cast<CorInfoHelpFunc>(reinterpret_cast<uintptr_t>(method) >> 2);
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

inline bool Compiler::compIsProfilerHookNeeded() const
{
#ifdef PROFILING_SUPPORTED
    return compProfilerHookNeeded
           // IL stubs are excluded by VM and we need to do the same even running
           // under a complus env hook to generate profiler hooks
           || (opts.compJitELTHookEnabled && !opts.IsJitFlagSet(JitFlags::JIT_FLAG_IL_STUB));
#else
    return false;
#endif
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
    // Save oper in case we want to find out what this node was
    tree->gtOperSave = tree->GetOper();
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
