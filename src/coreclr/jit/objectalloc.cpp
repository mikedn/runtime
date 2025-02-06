// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "phase.h"
#include "smallhash.h"

class ObjectAllocator final
{
    typedef SmallHashTable<unsigned, unsigned, 8U> LocalToLocalMap;

    static const unsigned StackAllocMaxSize = 0x2000U;

    Compiler*    comp;
    bool         m_IsObjectStackAllocationEnabled = false;
    bool         m_AnalysisDone                   = false;
    BitVecTraits m_bitVecTraits;
    BitVec       m_EscapingPointers = BitVecOps::UninitVal();
    // We keep the set of possibly-stack-pointing pointers as a superset of the set of
    // definitely-stack-pointing pointers. All definitely-stack-pointing pointers are in both sets.
    BitVec          m_PossiblyStackPointingPointers   = BitVecOps::UninitVal();
    BitVec          m_DefinitelyStackPointingPointers = BitVecOps::UninitVal();
    LocalToLocalMap m_HeapLocalToStackLocalMap;
    BitVec*         m_ConnGraphAdjacencyMatrix = nullptr;

public:
    ObjectAllocator(Compiler* comp)
        : comp(comp)
        , m_bitVecTraits(comp->lvaCount, comp)
        , m_HeapLocalToStackLocalMap(comp->getAllocator(CMK_ObjectAllocator))
    {
    }

    bool IsObjectStackAllocationEnabled() const;
    void EnableObjectStackAllocation();

    PhaseStatus Run();

private:
    bool CanAllocateLclVarOnStack(unsigned lclNum, CORINFO_CLASS_HANDLE clsHnd);
    bool CanLclVarEscape(unsigned lclNum);
    void MarkLclVarAsPossiblyStackPointing(unsigned lclNum);
    void MarkLclVarAsDefinitelyStackPointing(unsigned lclNum);
    bool MayLclVarPointToStack(unsigned lclNum);
    bool DoesLclVarPointToStack(unsigned lclNum);
    void DoAnalysis();
    void MarkLclVarAsEscaping(unsigned lclNum);
    void MarkEscapingVarsAndBuildConnGraph();
    void AddConnGraphEdge(unsigned sourceLclNum, unsigned targetLclNum);
    void ComputeEscapingNodes();
    void ComputeStackObjectPointers();
    bool MorphAllocObjNodes();
    void RewriteUses();

    GenTreeCall* MorphAllocObjNodeIntoHelperCall(GenTreeAllocObj* allocObj);
    unsigned MorphAllocObjNodeIntoStackAlloc(GenTreeAllocObj* allocObj, BasicBlock* block, Statement* stmt);
};

//------------------------------------------------------------------------
// IsObjectStackAllocationEnabled: Returns true iff object stack allocation is enabled
//
// Return Value:
//    Returns true iff object stack allocation is enabled

inline bool ObjectAllocator::IsObjectStackAllocationEnabled() const
{
    return m_IsObjectStackAllocationEnabled;
}

//------------------------------------------------------------------------
// EnableObjectStackAllocation:       Enable object stack allocation.

inline void ObjectAllocator::EnableObjectStackAllocation()
{
    m_IsObjectStackAllocationEnabled = true;
}

//------------------------------------------------------------------------
// CanAllocateLclVarOnStack: Returns true iff local variable can be
//                           allocated on the stack.
//
// Arguments:
//    lclNum   - Local variable number
//    clsHnd   - Class handle of the variable class
//
// Return Value:
//    Returns true iff local variable can be allocated on the stack.
//
// Notes:
//    Stack allocation of objects with gc fields and boxed objects is currently disabled.

inline bool ObjectAllocator::CanAllocateLclVarOnStack(unsigned lclNum, CORINFO_CLASS_HANDLE clsHnd)
{
    assert(m_AnalysisDone);

    DWORD classAttribs = comp->info.compCompHnd->getClassAttribs(clsHnd);

    if ((classAttribs & CORINFO_FLG_VALUECLASS) != 0)
    {
        // TODO-ObjectStackAllocation: enable stack allocation of boxed structs
        return false;
    }

    if (!comp->info.compCompHnd->canAllocateOnStack(clsHnd))
    {
        return false;
    }

    const unsigned classSize = comp->info.compCompHnd->getHeapClassSize(clsHnd);

    return !CanLclVarEscape(lclNum) && (classSize <= StackAllocMaxSize);
}

//------------------------------------------------------------------------
// CanLclVarEscape:          Returns true iff local variable can
//                           potentially escape from the method
//
// Arguments:
//    lclNum   - Local variable number
//
// Return Value:
//    Returns true iff local variable can potentially escape from the method

inline bool ObjectAllocator::CanLclVarEscape(unsigned lclNum)
{
    return BitVecOps::IsMember(m_bitVecTraits, m_EscapingPointers, lclNum);
}

//------------------------------------------------------------------------
// MayLclVarPointToStack:          Returns true iff local variable may
//                                 point to a stack-allocated object
//
// Arguments:
//    lclNum   - Local variable number
//
// Return Value:
//    Returns true iff local variable may point to a stack-allocated object

inline bool ObjectAllocator::MayLclVarPointToStack(unsigned lclNum)
{
    assert(m_AnalysisDone);
    return BitVecOps::IsMember(m_bitVecTraits, m_PossiblyStackPointingPointers, lclNum);
}

//------------------------------------------------------------------------
// DoesLclVarPointToStack:         Returns true iff local variable definitely
//                                 points to a stack-allocated object (or is null)
//
// Arguments:
//    lclNum   - Local variable number
//
// Return Value:
//    Returns true iff local variable definitely points to a stack-allocated object
//    (or is null)

inline bool ObjectAllocator::DoesLclVarPointToStack(unsigned lclNum)
{
    assert(m_AnalysisDone);
    return BitVecOps::IsMember(m_bitVecTraits, m_DefinitelyStackPointingPointers, lclNum);
}

// Run analysis (if object stack allocation is enabled) and then
// morph each GT_ALLOCOBJ node either into an allocation helper
// call or stack allocation.
//
PhaseStatus ObjectAllocator::Run()
{
    if ((comp->optMethodFlags & OMF_HAS_NEWOBJ) == 0)
    {
        JITDUMP("no newobjs in this method; punting\n");
        return PhaseStatus::MODIFIED_NOTHING;
    }

    if (IsObjectStackAllocationEnabled())
    {
        JITDUMP("enabled, analyzing...\n");
        DoAnalysis();
    }
    else
    {
        JITDUMP("disabled, punting\n");
    }

    const bool didStackAllocate = MorphAllocObjNodes();

    if (didStackAllocate)
    {
        ComputeStackObjectPointers();
        RewriteUses();
        return PhaseStatus::MODIFIED_EVERYTHING;
    }
    else
    {
        return PhaseStatus::MODIFIED_NOTHING;
    }
}

//------------------------------------------------------------------------------
// MarkLclVarAsEscaping : Mark local variable as escaping.
//
//
// Arguments:
//    lclNum  - Escaping pointing local variable number

void ObjectAllocator::MarkLclVarAsEscaping(unsigned lclNum)
{
    BitVecOps::AddElemD(m_bitVecTraits, m_EscapingPointers, lclNum);
}

//------------------------------------------------------------------------------
// MarkLclVarAsPossiblyStackPointing : Mark local variable as possibly pointing
//                                     to a stack-allocated object.
//
//
// Arguments:
//    lclNum  - Possibly stack-object-pointing local variable number

void ObjectAllocator::MarkLclVarAsPossiblyStackPointing(unsigned lclNum)
{
    BitVecOps::AddElemD(m_bitVecTraits, m_PossiblyStackPointingPointers, lclNum);
}

//------------------------------------------------------------------------------
// MarkLclVarAsDefinitelyStackPointing : Mark local variable as definitely pointing
//                                       to a stack-allocated object.
//
//
// Arguments:
//    lclNum  - Definitely stack-object-pointing local variable number

void ObjectAllocator::MarkLclVarAsDefinitelyStackPointing(unsigned lclNum)
{
    BitVecOps::AddElemD(m_bitVecTraits, m_DefinitelyStackPointingPointers, lclNum);
}

//------------------------------------------------------------------------------
// AddConnGraphEdge : Record that the source local variable may point to the same set of objects
//                    as the set pointed to by target local variable.
//
// Arguments:
//    sourceLclNum  - Local variable number of the edge source
//    targetLclNum  - Local variable number of the edge target

void ObjectAllocator::AddConnGraphEdge(unsigned sourceLclNum, unsigned targetLclNum)
{
    BitVecOps::AddElemD(m_bitVecTraits, m_ConnGraphAdjacencyMatrix[sourceLclNum], targetLclNum);
}

//------------------------------------------------------------------------
// DoAnalysis: Walk over basic blocks of the method and detect all local
//             variables that can be allocated on the stack.

void ObjectAllocator::DoAnalysis()
{
    assert(m_IsObjectStackAllocationEnabled);
    assert(!m_AnalysisDone);

    if (comp->lvaCount > 0)
    {
        m_EscapingPointers         = BitVecOps::MakeEmpty(m_bitVecTraits);
        m_ConnGraphAdjacencyMatrix = new (comp->getAllocator(CMK_ObjectAllocator)) BitVec[comp->lvaCount];

        MarkEscapingVarsAndBuildConnGraph();
        ComputeEscapingNodes();
    }

    m_AnalysisDone = true;
}

//------------------------------------------------------------------------------
// MarkEscapingVarsAndBuildConnGraph : Walk the trees of the method and mark any ref/byref/i_impl
//                                     local variables that may escape. Build a connection graph
//                                     for ref/by_ref/i_impl local variables.
//
// Arguments:
//    sourceLclNum  - Local variable number of the edge source
//    targetLclNum  - Local variable number of the edge target
//
// Notes:
//     The connection graph has an edge from local variable s to local variable t if s may point
//     to the objects t points to at some point in the method. It's a simplified version
//     of the graph described in this paper:
//     https://www.cc.gatech.edu/~harrold/6340/cs6340_fall2009/Readings/choi99escape.pdf
//     We currently don't have field edges and the edges we do have are called "deferred" in the paper.

void ObjectAllocator::MarkEscapingVarsAndBuildConnGraph()
{
    class BuildConnGraphVisitor final : public GenTreeVisitor<BuildConnGraphVisitor>
    {
        ObjectAllocator&     m_allocator;
        ArrayStack<GenTree*> m_userStack;

    public:
        enum
        {
            DoPreOrder  = true,
            DoPostOrder = true
        };

        BuildConnGraphVisitor(ObjectAllocator& allocator)
            : m_allocator(allocator), m_userStack(allocator.comp->getAllocator(CMK_ObjectAllocator))
        {
        }

        GenTreeWalkResult PreOrderVisit(GenTree** use, GenTree* user)
        {
            m_userStack.Push(*use);
            return GenTreeWalkResult::Continue;
        }

        GenTreeWalkResult PostOrderVisit(GenTree** use, GenTree* user)
        {
            m_userStack.Pop();
            return GenTreeWalkResult::Continue;
        }

        void PreOrderVisitLclRef(GenTree** use, GenTree* user)
        {
            GenTreeLclRef* tree = (*use)->AsLclRef();

            assert(tree == m_userStack.Top());

            LclVarDsc* lcl     = nullptr;
            bool       escapes = false;

            if (tree->OperIs(GT_LCL_LOAD))
            {
                lcl     = tree->AsLclLoad()->GetLcl();
                escapes = tree->TypeIs(TYP_REF, TYP_BYREF, TYP_I_IMPL) && CanLclVarEscapeViaUserStack(lcl->GetLclNum());
            }
            else if (tree->OperIs(GT_LCL_ADDR))
            {
                lcl     = tree->AsLclAddr()->GetLcl();
                escapes = lcl->TypeIs(TYP_REF, TYP_BYREF, TYP_I_IMPL);
            }

            if (escapes)
            {
                if (!m_allocator.CanLclVarEscape(lcl->GetLclNum()))
                {
                    JITDUMP("V%02u first escapes via [%06u]\n", lcl->GetLclNum(), tree->GetID());
                }

                m_allocator.MarkLclVarAsEscaping(lcl->GetLclNum());
            }
        }

        bool CanLclVarEscapeViaUserStack(unsigned lclNum)
        {
            unsigned userIndex                     = 1;
            bool     keepChecking                  = true;
            bool     canLclVarEscapeViaParentStack = true;

            while (keepChecking)
            {
                if (userIndex >= m_userStack.Size())
                {
                    canLclVarEscapeViaParentStack = false;
                    break;
                }

                keepChecking                  = false;
                canLclVarEscapeViaParentStack = true;

                GenTree* value = m_userStack.Top(userIndex - 1);
                GenTree* user  = m_userStack.Top(userIndex);

                switch (user->GetOper())
                {
                    case GT_LCL_STORE:
                        // The local is the source of a store.
                        assert(user->AsLclStore()->GetValue() == value);

                        // Update the connection graph if we are assigning to a local.
                        // For all other assignments we mark the rhs local as escaping.
                        // TODO-ObjectStackAllocation: track assignments to fields.

                        m_allocator.AddConnGraphEdge(user->AsLclStore()->GetLcl()->GetLclNum(), lclNum);
                        canLclVarEscapeViaParentStack = false;
                        break;

                    case GT_IND_LOAD:
                        canLclVarEscapeViaParentStack = false;
                        break;
                    case GT_IND_STORE:
                        canLclVarEscapeViaParentStack = user->AsIndir()->GetValue() == value;
                        break;

                    case GT_EQ:
                    case GT_NE:
                        canLclVarEscapeViaParentStack = false;
                        break;

                    case GT_COMMA:
                        if (user->AsOp()->GetOp(0) == m_userStack.Top(userIndex - 1))
                        {
                            // Left child of GT_COMMA, it will be discarded
                            canLclVarEscapeViaParentStack = false;
                            break;
                        }
                        FALLTHROUGH;
                    case GT_QMARK:
                    case GT_ADD:
                    case GT_FIELD_ADDR:
                        // Check whether the local escapes via its grandparent.
                        ++userIndex;
                        keepChecking = true;
                        break;

                    case GT_CALL:
                        if (user->AsCall()->IsHelperCall())
                        {
                            // TODO-ObjectStackAllocation: Special-case helpers here that
                            // 1. Don't make objects escape.
                            // 2. Protect objects as interior (GCPROTECT_BEGININTERIOR() instead of GCPROTECT_BEGIN()).
                            // 3. Don't check that the object is in the heap in ValidateInner.

                            canLclVarEscapeViaParentStack = true;
                        }
                        break;

                    default:
                        break;
                }
            }

            return canLclVarEscapeViaParentStack;
        }
    };

    for (LclVarDsc* lcl : comp->Locals())
    {
        unsigned lclNum = lcl->GetLclNum();

        if (lcl->TypeIs(TYP_REF, TYP_BYREF, TYP_I_IMPL))
        {
            m_ConnGraphAdjacencyMatrix[lclNum] = BitVecOps::MakeEmpty(m_bitVecTraits);

            if (lcl->IsAddressExposed())
            {
                JITDUMP("   V%02u is address exposed\n", lclNum);
                MarkLclVarAsEscaping(lclNum);
            }
        }
        else
        {
            // Variable that may not point to objects will not participate in our analysis.
            m_ConnGraphAdjacencyMatrix[lclNum] = BitVecOps::UninitVal();
        }
    }

    for (BasicBlock* const block : comp->Blocks())
    {
        for (Statement* const stmt : block->Statements())
        {
            BuildConnGraphVisitor buildConnGraphVisitor(*this);
            buildConnGraphVisitor.WalkTree(stmt->GetRootNodePointer(), nullptr);
        }
    }
}

// Given an initial set of escaping nodes, update it to contain the full set
// of escaping nodes by computing nodes reachable from the given set.
void ObjectAllocator::ComputeEscapingNodes()
{
    BitVec escapingNodesToProcess = BitVecOps::MakeCopy(m_bitVecTraits, m_EscapingPointers);
    BitVec newEscapingNodes       = BitVecOps::UninitVal();

    for (bool doOneMoreIteration = true; doOneMoreIteration;)
    {
        doOneMoreIteration = false;

        for (BitVecOps::Enumerator e(m_bitVecTraits, escapingNodesToProcess); e.MoveNext();)
        {
            const unsigned lclNum = e.Current();

            if (BitVec adjacency = m_ConnGraphAdjacencyMatrix[lclNum])
            {
                doOneMoreIteration = true;

                if (newEscapingNodes == BitVecOps::UninitVal())
                {
                    newEscapingNodes = BitVecOps::Alloc(m_bitVecTraits);
                }

                BitVecOps::Diff(m_bitVecTraits, newEscapingNodes, adjacency, m_EscapingPointers);
                BitVecOps::UnionD(m_bitVecTraits, escapingNodesToProcess, newEscapingNodes);
                BitVecOps::UnionD(m_bitVecTraits, m_EscapingPointers, newEscapingNodes);
                BitVecOps::RemoveElemD(m_bitVecTraits, escapingNodesToProcess, lclNum);
            }
        }
    }
}

// Given an initial set of possibly stack-pointing nodes,
// and an initial set of definitely stack-pointing nodes,
// update both sets by computing nodes reachable from the
// given set in the reverse connection graph.
void ObjectAllocator::ComputeStackObjectPointers()
{
    bool changed = true;

    while (changed)
    {
        changed = false;
        for (LclVarDsc* lcl : comp->Locals())
        {
            if (lcl->TypeIs(TYP_REF, TYP_BYREF, TYP_I_IMPL))
            {
                unsigned lclNum = lcl->GetLclNum();

                if (!MayLclVarPointToStack(lclNum) &&
                    !BitVecOps::IsEmptyIntersection(m_bitVecTraits, m_PossiblyStackPointingPointers,
                                                    m_ConnGraphAdjacencyMatrix[lclNum]))
                {
                    // We discovered a new pointer that may point to the stack.
                    MarkLclVarAsPossiblyStackPointing(lclNum);

                    // Check if this pointer always points to the stack.
                    if (lcl->lvSingleDef)
                    {
                        // Check if we know what is assigned to this pointer.
                        BitVecOps::Enumerator e(m_bitVecTraits, m_ConnGraphAdjacencyMatrix[lclNum]);

                        if (e.MoveNext())
                        {
                            if (DoesLclVarPointToStack(e.Current()))
                            {
                                // The only store to lclNum local is definitely-stack-pointing
                                // rhsLclNum local so lclNum local is also definitely-stack-pointing.
                                MarkLclVarAsDefinitelyStackPointing(lclNum);
                            }

                            assert(!e.MoveNext());
                        }
                    }
                    changed = true;
                }
            }
        }
    }
}

#ifdef DEBUG
static GenTreeWalkResult AssertWhenAllocObjFoundVisitor(GenTree** use, GenTree* user, void* data)
{
    GenTree* tree = *use;
    assert((tree != nullptr) && !tree->OperIs(GT_ALLOCOBJ));
    return GenTreeWalkResult::Continue;
}
#endif

//------------------------------------------------------------------------
// MorphAllocObjNodes: Morph each GT_ALLOCOBJ node either into an
//                     allocation helper call or stack allocation.
//
// Returns:
//    true if any allocation was done as a stack allocation.
//
// Notes:
//    Runs only over the blocks having bbFlags BBF_HAS_NEWOBJ set.

bool ObjectAllocator::MorphAllocObjNodes()
{
    bool didStackAllocate             = false;
    m_PossiblyStackPointingPointers   = BitVecOps::MakeEmpty(m_bitVecTraits);
    m_DefinitelyStackPointingPointers = BitVecOps::MakeEmpty(m_bitVecTraits);

    for (BasicBlock* const block : comp->Blocks())
    {
        const bool basicBlockHasNewObj       = (block->bbFlags & BBF_HAS_NEWOBJ) == BBF_HAS_NEWOBJ;
        const bool basicBlockHasBackwardJump = (block->bbFlags & BBF_BACKWARD_JUMP) == BBF_BACKWARD_JUMP;
#ifndef DEBUG
        if (!basicBlockHasNewObj)
        {
            continue;
        }
#endif // DEBUG

        for (Statement* const stmt : block->Statements())
        {
            GenTree*         allocExpr = stmt->GetRootNode();
            GenTreeAllocObj* alloc     = nullptr;

            if (allocExpr->OperIs(GT_LCL_STORE) && allocExpr->TypeIs(TYP_REF))
            {
                alloc = allocExpr->AsLclStore()->GetValue()->IsAllocObj();
            }

            if (alloc == nullptr)
            {
                // We assume that ALLOCOBJ nodes are always present in the canonical form.
                INDEBUG(comp->fgWalkTreePre(stmt->GetRootNodePointer(), AssertWhenAllocObjFoundVisitor));

                continue;
            }

            assert(basicBlockHasNewObj);

            if (m_IsObjectStackAllocationEnabled)
            {
                LclVarDsc* lcl    = allocExpr->AsLclStore()->GetLcl();
                unsigned   lclNum = lcl->GetLclNum();

                // Don't attempt to do stack allocations inside basic blocks that may be in a loop.
                if (!basicBlockHasBackwardJump && CanAllocateLclVarOnStack(lclNum, alloc->GetClassHandle()))
                {
                    JITDUMP("Allocating local variable V%02u on the stack\n", lcl->GetLclNum());

                    const unsigned stackLclNum = MorphAllocObjNodeIntoStackAlloc(alloc, block, stmt);
                    m_HeapLocalToStackLocalMap.AddOrUpdate(lclNum, stackLclNum);
                    // We keep the set of possibly-stack-pointing pointers as a superset of the set of
                    // definitely-stack-pointing pointers. All definitely-stack-pointing pointers are in both sets.
                    MarkLclVarAsDefinitelyStackPointing(lclNum);
                    MarkLclVarAsPossiblyStackPointing(lclNum);
                    stmt->GetRootNode()->ChangeToNothingNode();
                    comp->optMethodFlags |= OMF_HAS_OBJSTACKALLOC;
                    didStackAllocate = true;

                    continue;
                }

                JITDUMP("Allocating local variable V%02u on the heap\n", lcl->GetLclNum());
            }

            GenTree* allocCall = MorphAllocObjNodeIntoHelperCall(alloc);
            allocExpr->AsLclStore()->SetValue(allocCall);
            allocExpr->AddSideEffects(allocCall->GetSideEffects());
        }
    }

    return didStackAllocate;
}

GenTreeCall* ObjectAllocator::MorphAllocObjNodeIntoHelperCall(GenTreeAllocObj* allocObj)
{
    CorInfoHelpFunc helper         = allocObj->GetHelper();
    bool            hasSideEffects = allocObj->HelperHasSideEffects();
#ifdef FEATURE_READYTORUN_COMPILER
    CORINFO_CONST_LOOKUP entryPoint = allocObj->GetEntryPoint();
#endif
    GenTreeCall::Use* args = nullptr;

#ifdef FEATURE_READYTORUN_COMPILER
    if (helper != CORINFO_HELP_READYTORUN_NEW)
#endif
    {
        args = comp->gtNewCallArgs(allocObj->GetOp(0));
    }

    GenTreeCall* helperCall = comp->gtChangeToHelperCall(allocObj, helper, args);

    if (hasSideEffects)
    {
        helperCall->gtCallMoreFlags |= GTF_CALL_M_ALLOC_SIDE_EFFECTS;
    }

#ifdef FEATURE_READYTORUN_COMPILER
    if (entryPoint.addr != nullptr)
    {
        assert(comp->opts.IsReadyToRun());
        helperCall->setEntryPoint(entryPoint);
    }
#endif

    return helperCall;
}

//------------------------------------------------------------------------
// MorphAllocObjNodeIntoStackAlloc: Morph a GT_ALLOCOBJ node into stack
//                                  allocation.
// Arguments:
//    allocObj - GT_ALLOCOBJ that will be replaced by a stack allocation
//    block    - a basic block where allocObj is
//    stmt     - a statement where allocObj is
//
// Return Value:
//    local num for the new stack allocated local
//
// Notes:
//    This function can insert additional statements before stmt.

unsigned ObjectAllocator::MorphAllocObjNodeIntoStackAlloc(GenTreeAllocObj* allocObj, BasicBlock* block, Statement* stmt)
{
    assert(allocObj != nullptr);
    assert(m_AnalysisDone);

    LclVarDsc* lcl = comp->lvaAllocTemp(/* shortLifetime */ false DEBUGARG("MorphAllocObjNodeIntoStackAlloc temp"));
    comp->lvaSetStruct(lcl, comp->typGetObjLayout(allocObj->GetClassHandle()), /* checkUnsafeBuffer */ true);

    // Initialize the object memory if necessary.
    bool bbInALoop  = (block->bbFlags & BBF_BACKWARD_JUMP) != 0;
    bool bbIsReturn = block->bbJumpKind == BBJ_RETURN;

    if (comp->fgVarNeedsExplicitZeroInit(lcl, bbInALoop, bbIsReturn))
    {
        GenTree* init = comp->gtNewLclStore(lcl, TYP_STRUCT, comp->gtNewIconNode(0));
        init->AddSideEffects(GTF_GLOB_REF);
        comp->fgInsertStmtBefore(block, stmt, comp->gtNewStmt(init));
    }
    else
    {
        JITDUMP("\nSuppressing zero-init for V%02u -- expect to zero in prolog\n", lcl->GetLclNum());
        lcl->lvSuppressedZeroInit    = true;
        comp->compSuppressedZeroInit = true;
    }

    GenTree* methodTableStore = comp->gtNewLclStoreFld(TYP_I_IMPL, lcl, 0, allocObj->GetOp(0));
    methodTableStore->AddSideEffects(GTF_GLOB_REF);
    comp->fgInsertStmtBefore(block, stmt, comp->gtNewStmt(methodTableStore));

    return lcl->GetLclNum();
}

//------------------------------------------------------------------------
// RewriteUses: Find uses of the newobj temp for stack-allocated
//              objects and replace with address of the stack local.

void ObjectAllocator::RewriteUses()
{
    class RewriteUsesVisitor final : public GenTreeVisitor<RewriteUsesVisitor>
    {
        Compiler*            m_compiler;
        ObjectAllocator&     m_allocator;
        ArrayStack<GenTree*> m_userStack;

    public:
        enum
        {
            DoPreOrder  = true,
            DoPostOrder = true
        };

        RewriteUsesVisitor(ObjectAllocator& allocator)
            : m_compiler(allocator.comp)
            , m_allocator(allocator)
            , m_userStack(allocator.comp->getAllocator(CMK_ObjectAllocator))
        {
        }

        GenTreeWalkResult PreOrderVisit(GenTree** use, GenTree* user)
        {
            m_userStack.Push(*use);
            return GenTreeWalkResult::Continue;
        }

        GenTreeWalkResult PostOrderVisit(GenTree** use, GenTree* user)
        {
            m_userStack.Pop();
            return GenTreeWalkResult::Continue;
        }

        void PreOrderVisitLclRef(GenTree** use, GenTree* user)
        {
            GenTreeLclRef* lclRef = (*use)->AsLclRef();
            LclVarDsc*     lcl    = lclRef->GetLcl();
            unsigned       lclNum = lcl->GetLclNum();

            if ((lclNum >= BitVecTraits::GetSize(m_allocator.m_bitVecTraits)) ||
                !m_allocator.MayLclVarPointToStack(lclNum))
            {
                return;
            }

            var_types newType;
            unsigned  newLclNum = BAD_VAR_NUM;

            if (m_allocator.m_HeapLocalToStackLocalMap.TryGetValue(lclNum, &newLclNum))
            {
                newType = TYP_I_IMPL;
                lclRef  = m_compiler->gtNewLclAddr(m_compiler->lvaGetDesc(newLclNum), newType);
                *use    = lclRef;
            }
            else
            {
                newType = m_allocator.DoesLclVarPointToStack(lclNum) ? TYP_I_IMPL : TYP_BYREF;

                if (lclRef->TypeIs(TYP_REF))
                {
                    lclRef->SetType(newType);
                }
            }

            if (lcl->GetType() != newType)
            {
                JITDUMP("changing the type of V%02u from %s to %s\n", lclNum, varTypeName(lcl->GetType()),
                        varTypeName(newType));
                lcl->SetType(newType);
            }

            UpdateUserTypes(lclRef, newType);
        }

        void UpdateUserTypes(GenTree* node, var_types newType)
        {
            assert(newType == TYP_BYREF || newType == TYP_I_IMPL);

            unsigned userIndex    = 1;
            bool     keepChecking = true;

            while (keepChecking && (userIndex < m_userStack.Size()))
            {
                keepChecking  = false;
                GenTree* user = m_userStack.Top(userIndex);

                switch (user->GetOper())
                {
                    case GT_LCL_STORE:
                        if ((node == user->AsLclStore()->GetValue()) && user->TypeIs(TYP_REF))
                        {
                            user->SetType(newType);
                        }
                        break;

                    case GT_IND_LOAD:
                        if (newType != TYP_BYREF)
                        {
                            // This indicates that a write barrier is not needed when writing
                            // to this field/indirection since the address is not pointing to the heap.
                            // It's either null or points to inside a stack-allocated object.
                            user->gtFlags |= GTF_IND_TGT_NOT_HEAP;
                        }
                        break;

                    case GT_IND_STORE:
                        if ((node == user->AsIndStore()->GetValue()) && user->TypeIs(TYP_REF))
                        {
                            user->SetType(newType);
                        }
                        break;

                    case GT_EQ:
                    case GT_NE:
                        break;

                    case GT_COMMA:
                        if (user->AsOp()->GetOp(0) == m_userStack.Top(userIndex - 1))
                        {
                            // Left child of GT_COMMA, it will be discarded
                            break;
                        }
                        FALLTHROUGH;
                    case GT_QMARK:
                    case GT_ADD:
                    case GT_FIELD_ADDR:
                        if (user->TypeIs(TYP_REF))
                        {
                            user->ChangeType(newType);
                        }
                        ++userIndex;
                        keepChecking = true;
                        break;

                    default:
                        unreached();
                }

                if (keepChecking)
                {
                    node = m_userStack.Top(userIndex - 1);
                }
            }
        }
    };

    for (BasicBlock* const block : comp->Blocks())
    {
        for (Statement* const stmt : block->Statements())
        {
            RewriteUsesVisitor rewriteUsesVisitor(*this);
            rewriteUsesVisitor.WalkTree(stmt->GetRootNodePointer(), nullptr);
        }
    }
}

PhaseStatus Compiler::phMorphAllocObj()
{
    // Transform each GT_ALLOCOBJ node into either an allocation helper call or
    // local variable allocation on the stack.
    ObjectAllocator objectAllocator(this);

    if (compObjectStackAllocation() && opts.OptimizationEnabled())
    {
        objectAllocator.EnableObjectStackAllocation();
    }

    return objectAllocator.Run();
}
