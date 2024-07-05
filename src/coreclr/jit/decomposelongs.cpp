// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                               DecomposeLongs                              XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/

//
// This file contains code to decompose 64-bit LONG operations on 32-bit platforms
// into multiple single-register operations so individual register usage and requirements
// are explicit for LSRA. The rationale behind this is to avoid adding code complexity
// downstream caused by the introduction of handling longs as special cases,
// especially in LSRA.
//
// Long decomposition happens on a statement immediately prior to more general
// purpose lowering.
//

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#ifndef TARGET_64BIT // DecomposeLongs is only used on 32-bit platforms

#include "decomposelongs.h"

//------------------------------------------------------------------------
// DecomposeLongs::PrepareForDecomposition:
//    Do one-time preparation required for LONG decomposition. Namely,
//    promote long variables to multi-register structs.
//
// Arguments:
//    None
//
// Return Value:
//    None.
//
void DecomposeLongs::PrepareForDecomposition()
{
    PromoteLongVars();
}

//------------------------------------------------------------------------
// DecomposeLongs::DecomposeBlock:
//    Do LONG decomposition on all the nodes in the given block. This must
//    be done before lowering the block, as decomposition can insert
//    additional nodes.
//
// Arguments:
//    block - the block to process
//
// Return Value:
//    None.
//
void DecomposeLongs::DecomposeBlock(BasicBlock* block)
{
    assert(block->isEmpty() || block->IsLIR());
    m_range = &LIR::AsRange(block);
    DecomposeRangeHelper();
}

//------------------------------------------------------------------------
// DecomposeLongs::DecomposeRange:
//    Do LONG decomposition on all the nodes in the given range. This must
//    be done before inserting a range of un-decomposed IR into a block
//    that has already been decomposed.
//
// Arguments:
//    compiler    - The compiler context.
//    range       - The range to decompose.
//
// Return Value:
//    None.
//
void DecomposeLongs::DecomposeRange(Compiler* compiler, LIR::Range& range)
{
    assert(compiler != nullptr);

    DecomposeLongs decomposer(compiler);
    decomposer.m_range = &range;

    decomposer.DecomposeRangeHelper();
}

//------------------------------------------------------------------------
// DecomposeLongs::DecomposeRangeHelper:
//    Decompiose each node in the current range.
//
//    Decomposition is done as an execution-order walk. Decomposition of
//    a particular node can create new nodes that need to be further
//    decomposed at higher levels. That is, decomposition "bubbles up"
//    through dataflow.
//
void DecomposeLongs::DecomposeRangeHelper()
{
    assert(m_range != nullptr);

    GenTree* node = Range().FirstNode();
    while (node != nullptr)
    {
        node = DecomposeNode(node);
    }

    assert(Range().CheckLIR(m_compiler, true));
}

//------------------------------------------------------------------------
// DecomposeNode: Decompose long-type trees into lower and upper halves.
//
// Arguments:
//    tree - the tree that will, if needed, be decomposed.
//
// Return Value:
//    The next node to process.
//
GenTree* DecomposeLongs::DecomposeNode(GenTree* tree)
{
    // Handle the case where we are implicitly using the lower half of a long lclVar.
    if (tree->TypeIs(TYP_INT) && tree->OperIs(GT_LCL_LOAD))
    {
        LclVarDsc* varDsc = tree->AsLclLoad()->GetLcl();

        if (varTypeIsLong(varDsc->GetType()) && varDsc->IsPromoted())
        {
#ifdef DEBUG
            if (m_compiler->verbose)
            {
                printf("Changing implicit reference to lo half of long lclVar to an explicit reference of its promoted "
                       "half:\n");
                m_compiler->gtDispTreeRange(Range(), tree);
            }
#endif
            tree->AsLclLoad()->SetLcl(m_compiler->lvaGetDesc(varDsc->GetPromotedFieldLclNum(0)));

            return tree->gtNext;
        }
    }

    if (tree->TypeGet() != TYP_LONG)
    {
        return tree->gtNext;
    }

#ifdef DEBUG
    if (m_compiler->verbose)
    {
        printf("Decomposing TYP_LONG tree.  BEFORE:\n");
        m_compiler->gtDispTreeRange(Range(), tree);
    }
#endif // DEBUG

    LIR::Use use;
    if (!Range().TryGetUse(tree, &use))
    {
        use = LIR::Use::GetDummyUse(Range(), tree);
    }

    GenTree* nextNode = nullptr;

    switch (tree->GetOper())
    {
        case GT_LCL_LOAD:
            nextNode = DecomposeLclLoad(use);
            break;
        case GT_LCL_STORE:
            nextNode = DecomposeLclStore(use);
            break;
        case GT_LCL_LOAD_FLD:
            nextNode = DecomposeLclLoadFld(use);
            break;
        case GT_LCL_STORE_FLD:
            nextNode = DecomposeLclStoreFld(use);
            break;
        case GT_IND_LOAD:
            nextNode = DecomposeIndLoad(use);
            break;
        case GT_IND_STORE:
            nextNode = DecomposeIndStore(use);
            break;

        case GT_CAST:
            nextNode = DecomposeCast(use);
            break;

        case GT_CNS_LNG:
            nextNode = DecomposeCnsLng(use);
            break;

        case GT_CALL:
            nextNode = DecomposeCall(use);
            break;

        case GT_RETURN:
#ifdef TARGET_X86
            assert(tree->AsUnOp()->GetOp(0)->OperIs(GT_LONG) || tree->AsUnOp()->GetOp(0)->TypeIs(TYP_DOUBLE));
#else
            assert(tree->AsUnOp()->GetOp(0)->OperIs(GT_LONG));
#endif
            break;

        case GT_NOT:
            nextNode = DecomposeNot(use);
            break;

        case GT_NEG:
            nextNode = DecomposeNeg(use);
            break;

        case GT_ADD:
        case GT_SUB:
        case GT_OVF_SADD:
        case GT_OVF_UADD:
        case GT_OVF_SSUB:
        case GT_OVF_USUB:
        case GT_OR:
        case GT_XOR:
        case GT_AND:
            nextNode = DecomposeArith(use);
            break;

        case GT_MUL:
            nextNode = DecomposeMul(use);
            break;

        case GT_UMOD:
            nextNode = DecomposeUMod(use);
            break;

        case GT_LSH:
        case GT_RSH:
        case GT_RSZ:
            nextNode = DecomposeShift(use);
            break;

        case GT_ROL:
        case GT_ROR:
            nextNode = DecomposeRotate(use);
            break;

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            nextNode = DecomposeHWIntrinsic(use);
            break;
#endif // FEATURE_HW_INTRINSICS

        case GT_LOCKADD:
        case GT_XORR:
        case GT_XAND:
        case GT_XADD:
        case GT_XCHG:
        case GT_CMPXCHG:
            NYI("Interlocked operations on TYP_LONG");
            break;

        default:
        {
            JITDUMP("Illegal TYP_LONG node %s in Decomposition.", GenTree::OpName(tree->OperGet()));
            assert(!"Illegal TYP_LONG node in Decomposition.");
            break;
        }
    }

    // If we replaced the argument to a GT_FIELD_LIST element with a GT_LONG node, split that field list
    // element into two elements: one for each half of the GT_LONG.
    if ((use.Def()->OperGet() == GT_LONG) && !use.IsDummyUse() && (use.User()->OperGet() == GT_FIELD_LIST))
    {
        DecomposeFieldList(use.User()->AsFieldList(), use.Def()->AsOp());
    }

#ifdef DEBUG
    if (m_compiler->verbose)
    {
        // NOTE: st_lcl_var doesn't dump properly afterwards.
        printf("Decomposing TYP_LONG tree.  AFTER:\n");
        m_compiler->gtDispTreeRange(Range(), use.Def());
    }
#endif

    // When casting from a decomposed long to a smaller integer we can discard the high part.
    if (m_compiler->opts.OptimizationEnabled() && !use.IsDummyUse() && use.User()->OperIs(GT_CAST) &&
        use.User()->TypeIs(TYP_INT) && use.Def()->OperIs(GT_LONG))
    {
        nextNode = OptimizeCastFromDecomposedLong(use.User()->AsCast(), nextNode);
    }

    return nextNode;
}

//------------------------------------------------------------------------
// FinalizeDecomposition: A helper function to finalize LONG decomposition by
// taking the resulting two halves of the decomposition, and tie them together
// with a new GT_LONG node that will replace the original node.
//
// Arguments:
//    use - the LIR::Use object for the def that needs to be decomposed.
//    loResult - the decomposed low part
//    hiResult - the decomposed high part
//    insertResultAfter - the node that the GT_LONG should be inserted after
//
// Return Value:
//    The next node to process.
//
GenTree* DecomposeLongs::FinalizeDecomposition(LIR::Use& use,
                                               GenTree*  loResult,
                                               GenTree*  hiResult,
                                               GenTree*  insertResultAfter)
{
    assert(use.IsInitialized());
    assert(loResult != nullptr);
    assert(hiResult != nullptr);
    assert(Range().Contains(loResult));
    assert(Range().Contains(hiResult));

    if (use.IsDummyUse())
    {
        loResult->SetUnusedValue();
        hiResult->SetUnusedValue();

        return insertResultAfter->gtNext;
    }

    GenTree* gtLong = new (m_compiler, GT_LONG) GenTreeOp(GT_LONG, TYP_LONG, loResult, hiResult);
    gtLong->SetSideEffects(GTF_EMPTY);

    loResult->ClearUnusedValue();
    hiResult->ClearUnusedValue();

    Range().InsertAfter(insertResultAfter, gtLong);

    use.ReplaceWith(m_compiler, gtLong);

    return gtLong->gtNext;
}

GenTree* DecomposeLongs::DecomposeLclLoad(LIR::Use& use)
{
    assert(use.IsInitialized());

    GenTree*   tree     = use.Def();
    LclVarDsc* varDsc   = tree->AsLclLoad()->GetLcl();
    GenTree*   loResult = tree;
    loResult->SetType(TYP_INT);

    GenTree* hiResult = m_compiler->gtNewLclLoad(varDsc, TYP_INT);
    Range().InsertAfter(loResult, hiResult);

    if (varDsc->IsPromoted())
    {
        assert(varDsc->GetPromotedFieldCount() == 2);

        loResult->AsLclLoad()->SetLcl(m_compiler->lvaGetDesc(varDsc->GetPromotedFieldLclNum(0)));
        hiResult->AsLclLoad()->SetLcl(m_compiler->lvaGetDesc(varDsc->GetPromotedFieldLclNum(1)));
    }
    else
    {
        m_compiler->lvaSetDoNotEnregister(varDsc DEBUGARG(Compiler::DNER_LocalField));

        loResult->SetOper(GT_LCL_LOAD_FLD);
        loResult->AsLclLoadFld()->SetLclOffs(0);
        loResult->AsLclLoadFld()->SetFieldSeq(FieldSeqStore::NotAField());

        hiResult->SetOper(GT_LCL_LOAD_FLD);
        hiResult->AsLclLoadFld()->SetLclOffs(4);
        hiResult->AsLclLoadFld()->SetFieldSeq(FieldSeqStore::NotAField());
    }

    return FinalizeDecomposition(use, loResult, hiResult, hiResult);
}

GenTree* DecomposeLongs::DecomposeLclLoadFld(LIR::Use& use)
{
    assert(use.IsInitialized());

    GenTree*       tree     = use.Def();
    GenTreeLclFld* loResult = tree->AsLclLoadFld();
    loResult->SetType(TYP_INT);

    GenTree* hiResult = m_compiler->gtNewLclLoadFld(TYP_INT, loResult->GetLcl(), loResult->GetLclOffs() + 4);
    Range().InsertAfter(loResult, hiResult);

    return FinalizeDecomposition(use, loResult, hiResult, hiResult);
}

GenTree* DecomposeLongs::DecomposeLclStore(LIR::Use& use)
{
    assert(use.IsInitialized());

    GenTreeLclStore* tree = use.Def()->AsLclStore();
    GenTree*         rhs  = tree->GetValue();

    if (rhs->OperIs(GT_CALL, GT_SMULL, GT_UMULL))
    {
        // CALLs are not decomposed, so will not be converted to LONG
        // LCL_STORE = CALL are handled in genMultiRegCallStoreToLocal
        // SMULL, UMULL are not decomposed, so will not be converted to LONG
        return tree->gtNext;
    }

    noway_assert(rhs->OperIs(GT_LONG));

    LclVarDsc* lcl = tree->GetLcl();

    if (!lcl->IsPromoted())
    {
        // We cannot decompose a st.lclVar that is not promoted because doing so
        // changes its liveness semantics. For example, consider the following
        // decomposition of a st.lclVar into two st.lclFlds:
        //
        // Before:
        //
        //          /--* t0      int
        //          +--* t1      int
        //     t2 = *  gt_long   long
        //
        //          /--* t2      long
        //          *  st.lclVar long    V0
        //
        // After:
        //          /--* t0      int
        //          *  st.lclFld int     V0    [+0]
        //
        //          /--* t1      int
        //          *  st.lclFld int     V0    [+4]
        //
        // Before decomposition, the `st.lclVar` is a simple def of `V0`. After
        // decomposition, each `st.lclFld` is a partial def of `V0`. This partial
        // def is treated as both a use and a def of the appropriate lclVar. This
        // difference will affect any situation in which the liveness of a variable
        // at a def matters (e.g. dead store elimination, live-in sets, etc.). As
        // a result, we leave these stores as-is and generate the decomposed store
        // in the code generator.
        //
        // NOTE: this does extend the lifetime of the low half of the `GT_LONG`
        // node as compared to the decomposed form. If we start doing more code
        // motion in the backend, this may cause some CQ issues and some sort of
        // decomposition could be beneficial.
        return tree->gtNext;
    }

    assert(lcl->GetPromotedFieldCount() == 2);
    GenTreeOp* value = rhs->AsOp();
    Range().Remove(value);

    GenTreeLclStore* loStore = tree;
    loStore->SetType(TYP_INT);
    loStore->SetLcl(m_compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(0)));
    loStore->SetValue(value->GetOp(0));
    GenTreeLclStore* hiStore =
        m_compiler->gtNewLclStore(m_compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(1)), TYP_INT, value->GetOp(1));

    Range().InsertAfter(loStore, hiStore);

    return hiStore->gtNext;
}

GenTree* DecomposeLongs::DecomposeLclStoreFld(LIR::Use& use)
{
    assert(use.IsInitialized());

    GenTreeLclStoreFld* store = use.Def()->AsLclStoreFld();

    GenTreeOp* value = store->GetValue()->AsOp();
    assert(value->OperIs(GT_LONG));
    Range().Remove(value);

    // The original store node will be repurposed to store the low half of the GT_LONG.
    store->SetValue(value->GetOp(0));
    store->SetType(TYP_INT);

    // Create the store for the upper half of the GT_LONG and insert it after the low store.
    GenTreeLclFld* hiStore =
        m_compiler->gtNewLclStoreFld(TYP_INT, store->GetLcl(), store->GetLclOffs() + 4, value->GetOp(1));

    Range().InsertAfter(store, hiStore);

    return hiStore->gtNext;
}

//------------------------------------------------------------------------
// DecomposeCast: Decompose GT_CAST.
//
// Arguments:
//    use - the LIR::Use object for the def that needs to be decomposed.
//
// Return Value:
//    The next node to process.
//
GenTree* DecomposeLongs::DecomposeCast(LIR::Use& use)
{
    assert(use.IsInitialized());

    GenTreeCast* cast     = use.Def()->AsCast();
    GenTree*     loResult = nullptr;
    GenTree*     hiResult = nullptr;

    var_types srcType = cast->GetOp(0)->GetType();
    var_types dstType = cast->GetCastType();

    if (cast->IsCastUnsigned())
    {
        srcType = varTypeToUnsigned(srcType);
    }

    bool skipDecomposition = false;

    if (varTypeIsLong(srcType))
    {
        if (cast->HasOverflowCheck() && (varTypeIsUnsigned(srcType) != varTypeIsUnsigned(dstType)))
        {
            GenTreeOp* srcOp = cast->GetOp(0)->AsOp();
            noway_assert(srcOp->OperIs(GT_LONG));
            GenTree* loSrcOp = srcOp->GetOp(0);
            GenTree* hiSrcOp = srcOp->GetOp(1);

            //
            // When casting between long types an overflow check is needed only if the types
            // have different signedness. In both cases (long->ulong and ulong->long) we only
            // need to check if the high part is negative or not. Use the existing cast node
            // to perform a int->uint cast of the high part to take advantage of the overflow
            // check provided by codegen.
            //

            loResult = loSrcOp;

            hiResult = cast;
            cast->SetCastType(TYP_UINT);
            cast->SetCastUnsigned(false);
            cast->SetOp(0, hiSrcOp);

            Range().Remove(srcOp);
        }
        else
        {
            NYI("Unimplemented long->long no-op cast decomposition");
        }
    }
    else if (varTypeIsIntegralOrI(srcType))
    {
        if (cast->HasOverflowCheck() && !varTypeIsUnsigned(srcType) && varTypeIsUnsigned(dstType))
        {
            //
            // An overflow check is needed only when casting from a signed type to ulong.
            // Change the cast type to uint to take advantage of the overflow check provided
            // by codegen and then zero extend the resulting uint to ulong.
            //

            loResult = cast;
            cast->SetCastType(TYP_UINT);

            hiResult = m_compiler->gtNewZeroConNode(TYP_INT);

            Range().InsertAfter(loResult, hiResult);
        }
        else
        {
            if (!use.IsDummyUse() && use.User()->OperIs(GT_MUL))
            {
                // This INT to LONG cast is used by a MUL that will be transformed by DecomposeMul into
                // a LONG_MUL and as a result the high operand produced by the cast will become dead.
                // Skip cast decomposition so DecomposeMul doesn't need to bother with dead code removal,
                // especially in the case of sign extending casts that also introduce new locals.

                skipDecomposition = true;
            }
            else if (varTypeIsUnsigned(srcType))
            {
                loResult = cast->gtGetOp1();
                hiResult = m_compiler->gtNewZeroConNode(TYP_INT);

                Range().InsertAfter(cast, hiResult);
                Range().Remove(cast);
            }
            else
            {
                LIR::Use   src(Range(), &(cast->AsOp()->gtOp1), cast);
                LclVarDsc* lcl = src.ReplaceWithLclLoad(m_compiler);

                loResult = src.Def();

                GenTree* loCopy  = m_compiler->gtNewLclLoad(lcl, TYP_INT);
                GenTree* shiftBy = m_compiler->gtNewIconNode(31, TYP_INT);
                hiResult         = m_compiler->gtNewOperNode(GT_RSH, TYP_INT, loCopy, shiftBy);

                Range().InsertAfter(cast, loCopy, shiftBy, hiResult);
                Range().Remove(cast);
            }
        }
    }
    else
    {
        NYI("Unimplemented cast decomposition");
    }

    if (skipDecomposition)
    {
        return cast->gtNext;
    }

    return FinalizeDecomposition(use, loResult, hiResult, hiResult);
}

//------------------------------------------------------------------------
// DecomposeCnsLng: Decompose GT_CNS_LNG.
//
// Arguments:
//    use - the LIR::Use object for the def that needs to be decomposed.
//
// Return Value:
//    The next node to process.
//
GenTree* DecomposeLongs::DecomposeCnsLng(LIR::Use& use)
{
    assert(use.IsInitialized());
    assert(use.Def()->OperGet() == GT_CNS_LNG);

    GenTree* tree  = use.Def();
    int32_t  hiVal = static_cast<int32_t>(tree->AsLngCon()->GetValue() >> 32);

    GenTree* loResult = tree;
    loResult->ChangeOperConst(GT_CNS_INT);
    loResult->SetType(TYP_INT);

    GenTree* hiResult = new (m_compiler, GT_CNS_INT) GenTreeIntCon(TYP_INT, hiVal);
    Range().InsertAfter(loResult, hiResult);

    return FinalizeDecomposition(use, loResult, hiResult, hiResult);
}

//------------------------------------------------------------------------
// DecomposeFieldList: Decompose GT_FIELD_LIST.
//
// Arguments:
//    fieldList - the GT_FIELD_LIST node that uses the given GT_LONG node.
//    longNode - the node to decompose
//
// Return Value:
//    The next node to process.
//
// Notes:
//    Split a LONG field list element into two elements: one for each half of the GT_LONG.
//
GenTree* DecomposeLongs::DecomposeFieldList(GenTreeFieldList* fieldList, GenTreeOp* longNode)
{
    assert(longNode->OperGet() == GT_LONG);

    GenTreeFieldList::Use* loUse = nullptr;
    for (GenTreeFieldList::Use& use : fieldList->Uses())
    {
        if (use.GetNode() == longNode)
        {
            loUse = &use;
            break;
        }
    }
    assert(loUse != nullptr);

    Range().Remove(longNode);

    loUse->SetNode(longNode->gtGetOp1());
    loUse->SetType(TYP_INT);

    fieldList->InsertFieldLIR(m_compiler, loUse, longNode->gtGetOp2(), loUse->GetOffset() + 4, TYP_INT);

    return fieldList->gtNext;
}

//------------------------------------------------------------------------
// DecomposeCall: Decompose GT_CALL.
//
// Arguments:
//    use - the LIR::Use object for the def that needs to be decomposed.
//
// Return Value:
//    The next node to process.
//
GenTree* DecomposeLongs::DecomposeCall(LIR::Use& use)
{
    assert(use.IsInitialized());
    assert(use.Def()->OperGet() == GT_CALL);

    // We only need to force var = call() if the call's result is used.
    return StoreMultiRegNodeToLcl(use);
}

GenTree* DecomposeLongs::DecomposeIndStore(LIR::Use& use)
{
    assert(use.IsInitialized());

    GenTreeIndStore* store  = use.Def()->AsIndStore();
    GenTreeOp*       gtLong = store->GetValue()->AsOp();

    assert(gtLong->OperIs(GT_LONG));

    // Save address to a temp. It is used in storeIndLow and storeIndHigh trees.
    LIR::Use address(Range(), &store->gtOp1, store);
    address.ReplaceWithLclLoad(m_compiler);
    JITDUMP("[DecomposeIndStore]: Saving address tree to a temp var:\n");
    DISPTREERANGE(Range(), address.Def());

    if (!gtLong->gtOp1->OperIsLeaf())
    {
        LIR::Use op1(Range(), &gtLong->gtOp1, gtLong);
        op1.ReplaceWithLclLoad(m_compiler);
        JITDUMP("[DecomposeIndStore]: Saving low data tree to a temp var:\n");
        DISPTREERANGE(Range(), op1.Def());
    }

    if (!gtLong->gtOp2->OperIsLeaf())
    {
        LIR::Use op2(Range(), &gtLong->gtOp2, gtLong);
        op2.ReplaceWithLclLoad(m_compiler);
        JITDUMP("[DecomposeIndStore]: Saving high data tree to a temp var:\n");
        DISPTREERANGE(Range(), op2.Def());
    }

    GenTreeLclLoad*  addrBase    = store->GetAddr()->AsLclLoad();
    GenTree*         dataLow     = gtLong->GetOp(0);
    GenTree*         dataHigh    = gtLong->GetOp(1);
    GenTreeIndStore* storeIndLow = store;

    Range().Remove(gtLong);
    Range().Remove(dataHigh);
    storeIndLow->gtOp2  = dataLow;
    storeIndLow->SetType(TYP_INT);

    assert(addrBase->TypeIs(TYP_BYREF, TYP_I_IMPL));
    GenTree* addrBaseHigh = m_compiler->gtNewLclLoad(addrBase->GetLcl(), addrBase->GetType());
    GenTree* addrHigh     = new (m_compiler, GT_LEA) GenTreeAddrMode(addrBase->GetType(), addrBaseHigh, nullptr, 0, 4);
    GenTree* storeIndHigh = m_compiler->gtNewIndStore(TYP_INT, addrHigh, dataHigh);
    storeIndHigh->gtFlags = (storeIndLow->gtFlags & (GTF_ALL_EFFECT | GTF_SPECIFIC_MASK));

    Range().InsertAfter(storeIndLow, dataHigh, addrBaseHigh, addrHigh, storeIndHigh);

    return storeIndHigh;
}

GenTree* DecomposeLongs::DecomposeIndLoad(LIR::Use& use)
{
    GenTreeIndLoad* indLow = use.Def()->AsIndLoad();

    LIR::Use address(Range(), &indLow->gtOp1, indLow);
    address.ReplaceWithLclLoad(m_compiler);
    JITDUMP("[DecomposeIndLoad]: Saving addr tree to a temp var:\n");
    DISPTREERANGE(Range(), address.Def());

    // Change the type of lower ind.
    indLow->SetType(TYP_INT);

    // Create tree of ind(addr+4)
    GenTreeLclLoad* addrBase = indLow->GetAddr()->AsLclLoad();
    assert(addrBase->TypeIs(TYP_BYREF, TYP_I_IMPL));

    GenTree* addrBaseHigh = m_compiler->gtNewLclLoad(addrBase->GetLcl(), addrBase->GetType());
    GenTree* addrHigh     = new (m_compiler, GT_LEA) GenTreeAddrMode(addrBase->GetType(), addrBaseHigh, nullptr, 0, 4);
    GenTree* indHigh      = m_compiler->gtNewIndLoad(TYP_INT, addrHigh);
    indHigh->gtFlags |= (indLow->gtFlags & (GTF_GLOB_REF | GTF_EXCEPT | GTF_SPECIFIC_MASK));

    Range().InsertAfter(indLow, addrBaseHigh, addrHigh, indHigh);

    return FinalizeDecomposition(use, indLow, indHigh, indHigh);
}

//------------------------------------------------------------------------
// DecomposeNot: Decompose GT_NOT.
//
// Arguments:
//    use - the LIR::Use object for the def that needs to be decomposed.
//
// Return Value:
//    The next node to process.
//
GenTree* DecomposeLongs::DecomposeNot(LIR::Use& use)
{
    assert(use.IsInitialized());
    assert(use.Def()->OperGet() == GT_NOT);

    GenTree* tree   = use.Def();
    GenTree* gtLong = tree->gtGetOp1();
    noway_assert(gtLong->OperGet() == GT_LONG);
    GenTree* loOp1 = gtLong->gtGetOp1();
    GenTree* hiOp1 = gtLong->gtGetOp2();

    Range().Remove(gtLong);

    GenTree* loResult       = tree;
    loResult->SetType(TYP_INT);
    loResult->AsOp()->gtOp1 = loOp1;

    GenTree* hiResult = new (m_compiler, GT_NOT) GenTreeOp(GT_NOT, TYP_INT, hiOp1, nullptr);
    Range().InsertAfter(loResult, hiResult);

    return FinalizeDecomposition(use, loResult, hiResult, hiResult);
}

//------------------------------------------------------------------------
// DecomposeNeg: Decompose GT_NEG.
//
// Arguments:
//    use - the LIR::Use object for the def that needs to be decomposed.
//
// Return Value:
//    The next node to process.
//
GenTree* DecomposeLongs::DecomposeNeg(LIR::Use& use)
{
    assert(use.IsInitialized());
    assert(use.Def()->OperGet() == GT_NEG);

    GenTree* tree   = use.Def();
    GenTree* gtLong = tree->gtGetOp1();
    noway_assert(gtLong->OperGet() == GT_LONG);

    GenTree* loOp1 = gtLong->gtGetOp1();
    GenTree* hiOp1 = gtLong->gtGetOp2();

    Range().Remove(gtLong);

    GenTree* loResult       = tree;
    loResult->SetType(TYP_INT);
    loResult->AsOp()->gtOp1 = loOp1;

    GenTree* zero = m_compiler->gtNewZeroConNode(TYP_INT);

#if defined(TARGET_X86)

    GenTree* hiAdjust = m_compiler->gtNewOperNode(GT_ADD_HI, TYP_INT, hiOp1, zero);
    GenTree* hiResult = m_compiler->gtNewOperNode(GT_NEG, TYP_INT, hiAdjust);
    Range().InsertAfter(loResult, zero, hiAdjust, hiResult);

    loResult->gtFlags |= GTF_SET_FLAGS;
    hiAdjust->gtFlags |= GTF_USE_FLAGS;

#elif defined(TARGET_ARM)

    // We tend to use "movs" to load zero to a register, and that sets the flags, so put the
    // zero before the loResult, which is setting the flags needed by GT_SUB_HI.
    GenTree* hiResult = m_compiler->gtNewOperNode(GT_SUB_HI, TYP_INT, zero, hiOp1);
    Range().InsertBefore(loResult, zero);
    Range().InsertAfter(loResult, hiResult);

    loResult->gtFlags |= GTF_SET_FLAGS;
    hiResult->gtFlags |= GTF_USE_FLAGS;

#endif

    return FinalizeDecomposition(use, loResult, hiResult, hiResult);
}

GenTree* DecomposeLongs::DecomposeArith(LIR::Use& use)
{
    assert(use.IsInitialized());

    GenTree*   tree = use.Def();
    genTreeOps oper = tree->OperGet();

    assert((oper == GT_ADD) || (oper == GT_SUB) || (oper == GT_OR) || (oper == GT_XOR) || (oper == GT_AND) ||
           (oper == GT_OVF_SADD) || (oper == GT_OVF_UADD) || (oper == GT_OVF_SSUB) || (oper == GT_OVF_USUB));

    GenTree* op1 = tree->gtGetOp1();
    GenTree* op2 = tree->gtGetOp2();

    // Both operands must have already been decomposed into GT_LONG operators.
    noway_assert((op1->OperGet() == GT_LONG) && (op2->OperGet() == GT_LONG));

    // Capture the lo and hi halves of op1 and op2.
    GenTree* loOp1 = op1->gtGetOp1();
    GenTree* hiOp1 = op1->gtGetOp2();
    GenTree* loOp2 = op2->gtGetOp1();
    GenTree* hiOp2 = op2->gtGetOp2();

    // Now, remove op1 and op2 from the node list.
    Range().Remove(op1);
    Range().Remove(op2);

    // We will reuse "tree" for the loResult, which will now be of TYP_INT, and its operands
    // will be the lo halves of op1 from above.
    GenTree* loResult = tree;

    GenTree* hiResult = new (m_compiler, oper) GenTreeOp(GetHiOper(oper), TYP_INT, hiOp1, hiOp2);
    Range().InsertAfter(loResult, hiResult);

    if ((oper == GT_ADD) || (oper == GT_SUB) || (oper == GT_OVF_SADD) || (oper == GT_OVF_UADD) ||
        (oper == GT_OVF_SSUB) || (oper == GT_OVF_USUB))
    {
        loResult->gtFlags |= GTF_SET_FLAGS;
        hiResult->gtFlags |= GTF_USE_FLAGS;

        if ((oper == GT_OVF_SADD) || (oper == GT_OVF_UADD) || (oper == GT_OVF_SSUB) || (oper == GT_OVF_USUB))
        {
            hiResult->AddSideEffects(GTF_EXCEPT);
            loResult->RemoveSideEffects(GTF_EXCEPT);
        }
    }

    loResult->SetOper(GetLoOper(oper));
    loResult->SetType(TYP_INT);
    loResult->AsOp()->SetOp(0, loOp1);
    loResult->AsOp()->SetOp(1, loOp2);

    return FinalizeDecomposition(use, loResult, hiResult, hiResult);
}

//------------------------------------------------------------------------
// DecomposeShift: Decompose GT_LSH, GT_RSH, GT_RSZ. For shift nodes being shifted
// by a constant int, we can inspect the shift amount and decompose to the appropriate
// node types, generating a shl/shld pattern for GT_LSH, a shrd/shr pattern for GT_RSZ,
// and a shrd/sar pattern for GT_SHR for most shift amounts. Shifting by 0, >= 32 and
// >= 64 are special cased to produce better code patterns.
//
// For all other shift nodes, we need to use the shift helper functions, so we here convert
// the shift into a helper call by pulling its arguments out of linear order and making
// them the args to a call, then replacing the original node with the new call.
//
// Arguments:
//    use - the LIR::Use object for the def that needs to be decomposed.
//
// Return Value:
//    The next node to process.
//
GenTree* DecomposeLongs::DecomposeShift(LIR::Use& use)
{
    assert(use.IsInitialized());

    GenTree* shift     = use.Def();
    GenTree* gtLong    = shift->gtGetOp1();
    GenTree* loOp1     = gtLong->gtGetOp1();
    GenTree* hiOp1     = gtLong->gtGetOp2();
    GenTree* shiftByOp = shift->gtGetOp2();

    genTreeOps oper        = shift->OperGet();
    genTreeOps shiftByOper = shiftByOp->OperGet();

    // tLo = ...
    // ...
    // tHi = ...
    // ...
    // tLong = long tLo, tHi
    // ...
    // tShiftAmount = ...
    // ...
    // tShift = shift tLong, tShiftAmount

    assert((oper == GT_LSH) || (oper == GT_RSH) || (oper == GT_RSZ));

    // If we are shifting by a constant int, we do not want to use a helper, instead, we decompose.
    if (shiftByOper == GT_CNS_INT)
    {
        // Reduce count modulo 64 to match behavior found in the shift helpers,
        // Compiler::gtFoldExpr and ValueNumStore::EvalOpIntegral.
        unsigned int count = shiftByOp->AsIntCon()->gtIconVal & 0x3F;
        Range().Remove(shiftByOp);

        if (count == 0)
        {
            GenTree* next = shift->gtNext;
            // Remove shift and don't do anything else.
            if (shift->IsUnusedValue())
            {
                gtLong->SetUnusedValue();
            }
            Range().Remove(shift);
            use.ReplaceWith(m_compiler, gtLong);
            return next;
        }

        GenTree* loResult;
        GenTree* hiResult;

        GenTree* insertAfter;

        switch (oper)
        {
            case GT_LSH:
            {
                if (count < 32)
                {
                    // For shifts of < 32 bits, we transform the code to:
                    //
                    //     tLo = ...
                    //           st.lclVar vLo, tLo
                    //     ...
                    //     tHi = ...
                    //     ...
                    //     tShiftLo = lsh vLo, tShiftAmountLo
                    //     tShitHiLong = long vLo, tHi
                    //     tShiftHi = lsh_hi tShiftHiLong, tShiftAmountHi
                    //
                    // This will produce:
                    //
                    //     reg1 = lo
                    //     shl lo, shift
                    //     shld hi, reg1, shift

                    Range().Remove(gtLong);

                    loOp1               = RepresentOpAsLclLoad(loOp1, gtLong, &gtLong->AsOp()->gtOp1);
                    LclVarDsc* loOp1Lcl = loOp1->AsLclLoad()->GetLcl();
                    Range().Remove(loOp1);

                    GenTree* shiftByHi = m_compiler->gtNewIconNode(count, TYP_INT);
                    GenTree* shiftByLo = m_compiler->gtNewIconNode(count, TYP_INT);

                    loResult = m_compiler->gtNewOperNode(GT_LSH, TYP_INT, loOp1, shiftByLo);

                    // Create a GT_LONG that contains loCopy and hiOp1. This will be used in codegen to
                    // generate the shld instruction
                    GenTree* loCopy = m_compiler->gtNewLclLoad(loOp1Lcl, TYP_INT);
                    GenTree* hiOp   = new (m_compiler, GT_LONG) GenTreeOp(GT_LONG, TYP_LONG, loCopy, hiOp1);
                    hiResult        = m_compiler->gtNewOperNode(GT_LSH_HI, TYP_INT, hiOp, shiftByHi);

                    Range().InsertBefore(shift, loOp1, shiftByLo, loResult);
                    Range().InsertBefore(shift, loCopy, hiOp, shiftByHi, hiResult);

                    insertAfter = hiResult;
                }
                else
                {
                    assert(count >= 32 && count < 64);

                    // Since we're left shifting at least 32 bits, we can remove the hi part of the shifted value iff
                    // it has no side effects.
                    //
                    // TODO-CQ: we could go perform this removal transitively (i.e. iteratively remove everything that
                    // feeds the hi operand while there are no side effects)
                    if ((hiOp1->gtFlags & GTF_ALL_EFFECT) == 0)
                    {
                        Range().Remove(hiOp1, true);
                    }
                    else
                    {
                        hiOp1->SetUnusedValue();
                    }

                    if (count == 32)
                    {
                        // Move loOp1 into hiResult (shift of 32 bits is just a mov of lo to hi)
                        // We need to make sure that we save lo to a temp variable so that we don't overwrite lo
                        // before saving it to hi in the case that we are doing an inplace shift. I.e.:
                        // x = x << 32

                        LIR::Use loOp1Use(Range(), &gtLong->AsOp()->gtOp1, gtLong);
                        loOp1Use.ReplaceWithLclLoad(m_compiler);

                        hiResult = loOp1Use.Def();
                        Range().Remove(gtLong);
                    }
                    else
                    {
                        Range().Remove(gtLong);
                        assert(count > 32 && count < 64);

                        // Move loOp1 into hiResult, do a GT_LSH with count - 32.
                        // We will compute hiResult before loResult in this case, so we don't need to store lo to a
                        // temp
                        GenTree* shiftBy = m_compiler->gtNewIconNode(count - 32, TYP_INT);
                        hiResult         = m_compiler->gtNewOperNode(GT_LSH, TYP_INT, loOp1, shiftBy);
                        Range().InsertBefore(shift, shiftBy, hiResult);
                    }

                    // Zero out loResult (shift of >= 32 bits shifts all lo bits to hiResult)
                    loResult = m_compiler->gtNewZeroConNode(TYP_INT);
                    Range().InsertBefore(shift, loResult);

                    insertAfter = loResult;
                }
            }
            break;
            case GT_RSZ:
            {
                Range().Remove(gtLong);

                if (count < 32)
                {
                    // Hi is a GT_RSZ, lo is a GT_RSH_LO. Will produce:
                    // reg1 = hi
                    // shrd lo, reg1, shift
                    // shr hi, shift

                    hiOp1               = RepresentOpAsLclLoad(hiOp1, gtLong, &gtLong->AsOp()->gtOp2);
                    LclVarDsc* hiOp1Lcl = hiOp1->AsLclLoad()->GetLcl();
                    GenTree*   hiCopy   = m_compiler->gtNewLclLoad(hiOp1Lcl, TYP_INT);

                    GenTree* shiftByHi = m_compiler->gtNewIconNode(count, TYP_INT);
                    GenTree* shiftByLo = m_compiler->gtNewIconNode(count, TYP_INT);

                    hiResult = m_compiler->gtNewOperNode(GT_RSZ, TYP_INT, hiOp1, shiftByHi);

                    // Create a GT_LONG that contains loOp1 and hiCopy. This will be used in codegen to
                    // generate the shrd instruction
                    GenTree* loOp = new (m_compiler, GT_LONG) GenTreeOp(GT_LONG, TYP_LONG, loOp1, hiCopy);
                    loResult      = m_compiler->gtNewOperNode(GT_RSH_LO, TYP_INT, loOp, shiftByLo);

                    Range().InsertBefore(shift, hiCopy, loOp);
                    Range().InsertBefore(shift, shiftByLo, loResult);
                    Range().InsertBefore(shift, shiftByHi, hiResult);
                }
                else
                {
                    assert(count >= 32 && count < 64);

                    // Since we're right shifting at least 32 bits, we can remove the lo part of the shifted value iff
                    // it has no side effects.
                    //
                    // TODO-CQ: we could go perform this removal transitively (i.e. iteratively remove everything that
                    // feeds the lo operand while there are no side effects)
                    if ((loOp1->gtFlags & (GTF_ALL_EFFECT | GTF_SET_FLAGS)) == 0)
                    {
                        Range().Remove(loOp1, true);
                    }
                    else
                    {
                        loOp1->SetUnusedValue();
                    }

                    if (count == 32)
                    {
                        // Move hiOp1 into loResult.
                        loResult = hiOp1;
                    }
                    else
                    {
                        assert(count > 32 && count < 64);

                        // Move hiOp1 into loResult, do a GT_RSZ with count - 32.
                        GenTree* shiftBy = m_compiler->gtNewIconNode(count - 32, TYP_INT);
                        loResult         = m_compiler->gtNewOperNode(GT_RSZ, TYP_INT, hiOp1, shiftBy);
                        Range().InsertBefore(shift, shiftBy, loResult);
                    }

                    // Zero out hi
                    hiResult = m_compiler->gtNewZeroConNode(TYP_INT);
                    Range().InsertBefore(shift, hiResult);
                }

                insertAfter = hiResult;
            }
            break;
            case GT_RSH:
            {
                Range().Remove(gtLong);

                hiOp1               = RepresentOpAsLclLoad(hiOp1, gtLong, &gtLong->AsOp()->gtOp2);
                LclVarDsc* hiOp1Lcl = hiOp1->AsLclLoad()->GetLcl();
                GenTree*   hiCopy   = m_compiler->gtNewLclLoad(hiOp1Lcl, TYP_INT);
                Range().Remove(hiOp1);

                if (count < 32)
                {
                    // Hi is a GT_RSH, lo is a GT_RSH_LO. Will produce:
                    // reg1 = hi
                    // shrd lo, reg1, shift
                    // sar hi, shift

                    GenTree* shiftByHi = m_compiler->gtNewIconNode(count, TYP_INT);
                    GenTree* shiftByLo = m_compiler->gtNewIconNode(count, TYP_INT);

                    hiResult = m_compiler->gtNewOperNode(GT_RSH, TYP_INT, hiOp1, shiftByHi);

                    // Create a GT_LONG that contains loOp1 and hiCopy. This will be used in codegen to
                    // generate the shrd instruction
                    GenTree* loOp = new (m_compiler, GT_LONG) GenTreeOp(GT_LONG, TYP_LONG, loOp1, hiCopy);
                    loResult      = m_compiler->gtNewOperNode(GT_RSH_LO, TYP_INT, loOp, shiftByLo);

                    Range().InsertBefore(shift, hiCopy, loOp);
                    Range().InsertBefore(shift, shiftByLo, loResult);
                    Range().InsertBefore(shift, shiftByHi, hiOp1, hiResult);
                }
                else
                {
                    assert(count >= 32 && count < 64);

                    // Since we're right shifting at least 32 bits, we can remove the lo part of the shifted value iff
                    // it has no side effects.
                    //
                    // TODO-CQ: we could go perform this removal transitively (i.e. iteratively remove everything that
                    // feeds the lo operand while there are no side effects)
                    if ((loOp1->gtFlags & (GTF_ALL_EFFECT | GTF_SET_FLAGS)) == 0)
                    {
                        Range().Remove(loOp1, true);
                    }
                    else
                    {
                        loOp1->SetUnusedValue();
                    }

                    if (count == 32)
                    {
                        // Move hiOp1 into loResult.
                        loResult = hiOp1;
                        Range().InsertBefore(shift, loResult);
                    }
                    else
                    {
                        assert(count > 32 && count < 64);

                        // Move hiOp1 into loResult, do a GT_RSH with count - 32.
                        GenTree* shiftBy = m_compiler->gtNewIconNode(count - 32, TYP_INT);
                        loResult         = m_compiler->gtNewOperNode(GT_RSH, TYP_INT, hiOp1, shiftBy);
                        Range().InsertBefore(shift, hiOp1, shiftBy, loResult);
                    }

                    // Propagate sign bit in hiResult
                    GenTree* shiftBy = m_compiler->gtNewIconNode(31, TYP_INT);
                    hiResult         = m_compiler->gtNewOperNode(GT_RSH, TYP_INT, hiCopy, shiftBy);
                    Range().InsertBefore(shift, shiftBy, hiCopy, hiResult);
                }

                insertAfter = hiResult;
            }
            break;
            default:
                unreached();
        }

        // Remove shift from Range
        Range().Remove(shift);

        return FinalizeDecomposition(use, loResult, hiResult, insertAfter);
    }
    else
    {
        // Because calls must be created as HIR and lowered to LIR, we need to dump
        // any LIR temps into lclVars before using them as arguments.
        shiftByOp = RepresentOpAsLclLoad(shiftByOp, shift, &shift->AsOp()->gtOp2);
        loOp1     = RepresentOpAsLclLoad(loOp1, gtLong, &gtLong->AsOp()->gtOp1);
        hiOp1     = RepresentOpAsLclLoad(hiOp1, gtLong, &gtLong->AsOp()->gtOp2);

        Range().Remove(shiftByOp);
        Range().Remove(gtLong);
        Range().Remove(loOp1);
        Range().Remove(hiOp1);

        CorInfoHelpFunc helper;

        switch (oper)
        {
            case GT_LSH:
                helper = CORINFO_HELP_LLSH;
                break;
            case GT_RSH:
                helper = CORINFO_HELP_LRSH;
                break;
            case GT_RSZ:
                helper = CORINFO_HELP_LRSZ;
                break;
            default:
                unreached();
        }

        GenTreeCall::Use* argList = m_compiler->gtNewCallArgs(loOp1, hiOp1, shiftByOp);
        GenTreeCall*      call    = m_compiler->gtNewHelperCallNode(helper, TYP_LONG, argList);

        if (shift->IsUnusedValue())
        {
            call->SetUnusedValue();
        }

        LIR::InsertHelperCallBefore(m_compiler, Range(), shift, call);

        Range().Remove(shift);
        use.ReplaceWith(m_compiler, call);
        return call;
    }
}

//------------------------------------------------------------------------
// DecomposeRotate: Decompose GT_ROL and GT_ROR with constant shift amounts. We can
// inspect the rotate amount and decompose to the appropriate node types, generating
// a shld/shld pattern for GT_ROL, a shrd/shrd pattern for GT_ROR, for most rotate
// amounts.
//
// Arguments:
//    use - the LIR::Use object for the def that needs to be decomposed.
//
// Return Value:
//    The next node to process.
//
GenTree* DecomposeLongs::DecomposeRotate(LIR::Use& use)
{
    GenTree* tree       = use.Def();
    GenTree* gtLong     = tree->gtGetOp1();
    GenTree* rotateByOp = tree->gtGetOp2();

    genTreeOps oper = tree->OperGet();

    assert((oper == GT_ROL) || (oper == GT_ROR));
    assert(rotateByOp->IsCnsIntOrI());

    // For longs, we need to change rols into two GT_LSH_HIs and rors into two GT_RSH_LOs
    // so we will get:
    //
    // shld lo, hi, rotateAmount
    // shld hi, loCopy, rotateAmount
    //
    // or:
    //
    // shrd lo, hi, rotateAmount
    // shrd hi, loCopy, rotateAmount

    unsigned count = (unsigned)rotateByOp->AsIntCon()->gtIconVal;
    Range().Remove(rotateByOp);

    // Make sure the rotate amount is between 0 and 63.
    assert((count < 64) && (count != 0));

    GenTree* loResult;
    GenTree* hiResult;

    if (count == 32)
    {
        // If the rotate amount is 32, then swap hi and lo
        LIR::Use loOp1Use(Range(), &gtLong->AsOp()->gtOp1, gtLong);
        loOp1Use.ReplaceWithLclLoad(m_compiler);

        LIR::Use hiOp1Use(Range(), &gtLong->AsOp()->gtOp2, gtLong);
        hiOp1Use.ReplaceWithLclLoad(m_compiler);

        hiResult              = loOp1Use.Def();
        loResult              = hiOp1Use.Def();
        gtLong->AsOp()->gtOp1 = loResult;
        gtLong->AsOp()->gtOp2 = hiResult;

        if (tree->IsUnusedValue())
        {
            gtLong->SetUnusedValue();
        }

        GenTree* next = tree->gtNext;
        // Remove tree and don't do anything else.
        Range().Remove(tree);
        use.ReplaceWith(m_compiler, gtLong);
        return next;
    }
    else
    {
        GenTree* loOp1;
        GenTree* hiOp1;

        if (count > 32)
        {
            // If count > 32, we swap hi and lo, and subtract 32 from count
            hiOp1 = gtLong->gtGetOp1();
            loOp1 = gtLong->gtGetOp2();

            Range().Remove(gtLong);
            loOp1 = RepresentOpAsLclLoad(loOp1, gtLong, &gtLong->AsOp()->gtOp2);
            hiOp1 = RepresentOpAsLclLoad(hiOp1, gtLong, &gtLong->AsOp()->gtOp1);

            count -= 32;
        }
        else
        {
            loOp1 = gtLong->gtGetOp1();
            hiOp1 = gtLong->gtGetOp2();

            Range().Remove(gtLong);
            loOp1 = RepresentOpAsLclLoad(loOp1, gtLong, &gtLong->AsOp()->gtOp1);
            hiOp1 = RepresentOpAsLclLoad(hiOp1, gtLong, &gtLong->AsOp()->gtOp2);
        }

        LclVarDsc* loOp1Lcl = loOp1->AsLclLoad()->GetLcl();
        LclVarDsc* hiOp1Lcl = hiOp1->AsLclLoad()->GetLcl();

        Range().Remove(loOp1);
        Range().Remove(hiOp1);

        GenTree* rotateByHi = m_compiler->gtNewIconNode(count, TYP_INT);
        GenTree* rotateByLo = m_compiler->gtNewIconNode(count, TYP_INT);

        oper = oper == GT_ROL ? GT_LSH_HI : GT_RSH_LO;

        // Create a GT_LONG that contains loOp1 and hiCopy. This will be used in codegen to
        // generate the shld instruction
        GenTree* hiCopy = m_compiler->gtNewLclLoad(hiOp1Lcl, TYP_INT);
        GenTree* loOp   = new (m_compiler, GT_LONG) GenTreeOp(GT_LONG, TYP_LONG, hiCopy, loOp1);
        loResult        = m_compiler->gtNewOperNode(oper, TYP_INT, loOp, rotateByLo);

        // Create a GT_LONG that contains loCopy and hiOp1. This will be used in codegen to
        // generate the shld instruction
        GenTree* loCopy = m_compiler->gtNewLclLoad(loOp1Lcl, TYP_INT);
        GenTree* hiOp   = new (m_compiler, GT_LONG) GenTreeOp(GT_LONG, TYP_LONG, loCopy, hiOp1);
        hiResult        = m_compiler->gtNewOperNode(oper, TYP_INT, hiOp, rotateByHi);

        Range().InsertBefore(tree, hiCopy, loOp1, loOp);
        Range().InsertBefore(tree, rotateByLo, loResult);
        Range().InsertBefore(tree, loCopy, hiOp1, hiOp);
        Range().InsertBefore(tree, rotateByHi, hiResult);

        Range().Remove(tree);

        return FinalizeDecomposition(use, loResult, hiResult, hiResult);
    }
}

//------------------------------------------------------------------------
// DecomposeMul: Decompose GT_MUL. These muls result in a mul instruction that
// returns its result in two registers like GT_CALLs do. Additionally, these muls are
// guaranteed to be in the form long = (long)int * (long)int. Therefore, to decompose
// these nodes, we convert them into GT_MUL_LONGs, undo the cast from int to long by
// stripping out the lo ops, and force them into the form var = mul, as we do for
// GT_CALLs. In codegen, we then produce a mul instruction that produces the result
// in edx:eax on x86 or in any two chosen by RA registers on arm32, and store those
// registers on the stack in GenStoreLclVarLong.
//
// All other GT_MULs have been converted to helper calls in morph.cpp
//
// Arguments:
//    use - the LIR::Use object for the def that needs to be decomposed.
//
// Return Value:
//    The next node to process.
//
GenTree* DecomposeLongs::DecomposeMul(LIR::Use& use)
{
    assert(use.IsInitialized());

    GenTreeOp* tree = use.Def()->AsOp();

    assert(tree->OperIs(GT_MUL));

    // We expect both operands to be int->long casts. DecomposeCast specifically
    // ignores such casts when they are used by GT_MULs.
    GenTreeCast* op1 = tree->GetOp(0)->AsCast();
    GenTreeCast* op2 = tree->GetOp(1)->AsCast();
    assert(op1->TypeIs(TYP_LONG));
    assert(op2->TypeIs(TYP_LONG));
    assert(op1->IsCastUnsigned() == op2->IsCastUnsigned());

    Range().Remove(op1);
    Range().Remove(op2);

    tree->SetOp(0, op1->GetOp(0));
    tree->SetOp(1, op2->GetOp(0));
    tree->SetOper(op1->IsCastUnsigned() ? GT_UMULL : GT_SMULL);

    return StoreMultiRegNodeToLcl(use);
}

//------------------------------------------------------------------------
// DecomposeUMod: Decompose GT_UMOD. The only GT_UMODs that make it to decompose
// are guaranteed to be an unsigned long mod with op2 which is a cast to long from
// a constant int whose value is between 2 and 0x3fffffff. All other GT_UMODs are
// morphed into helper calls. These GT_UMODs will actually return an int value in
// RDX. In decompose, we make the lo operation a TYP_INT GT_UMOD, with op2 as the
// original lo half and op1 as a GT_LONG. We make the hi part 0,  so we end up with:
//
// GT_UMOD[TYP_INT] ( GT_LONG [TYP_LONG] (loOp1, hiOp1), loOp2 [TYP_INT] )
//
// With the expectation that we will generate:
//
// EDX = hiOp1
// EAX = loOp1
// reg = loOp2
// idiv reg
// EDX is the remainder, and result of GT_UMOD
// mov hiReg = 0
//
// Arguments:
//    use - the LIR::Use object for the def that needs to be decomposed.
//
// Return Value:
//    The next node to process.
//
GenTree* DecomposeLongs::DecomposeUMod(LIR::Use& use)
{
    assert(use.IsInitialized());

    GenTree*   tree = use.Def();
    genTreeOps oper = tree->OperGet();

    assert(oper == GT_UMOD);

    GenTree* op1 = tree->gtGetOp1();
    GenTree* op2 = tree->gtGetOp2();
    assert(op1->OperGet() == GT_LONG);
    assert(op2->OperGet() == GT_LONG);

    GenTree* loOp2 = op2->gtGetOp1();
    GenTree* hiOp2 = op2->gtGetOp2();

    assert(loOp2->OperGet() == GT_CNS_INT);
    assert(hiOp2->OperGet() == GT_CNS_INT);
    assert((loOp2->AsIntCon()->gtIconVal >= 2) && (loOp2->AsIntCon()->gtIconVal <= 0x3fffffff));
    assert(hiOp2->AsIntCon()->gtIconVal == 0);

    // Get rid of op2's hi part. We don't need it.
    Range().Remove(hiOp2);
    Range().Remove(op2);

    // Lo part is the GT_UMOD
    GenTree* loResult       = tree;
    loResult->AsOp()->gtOp2 = loOp2;
    loResult->SetType(TYP_INT);

    // Set the high part to 0
    GenTree* hiResult = m_compiler->gtNewZeroConNode(TYP_INT);

    Range().InsertAfter(loResult, hiResult);

    return FinalizeDecomposition(use, loResult, hiResult, hiResult);
}

#ifdef FEATURE_HW_INTRINSICS

//------------------------------------------------------------------------
// DecomposeHWIntrinsic: Decompose GT_HWINTRINSIC.
//
// Arguments:
//    use - the LIR::Use object for the def that needs to be decomposed.
//
// Return Value:
//    The next node to process.
//
GenTree* DecomposeLongs::DecomposeHWIntrinsic(LIR::Use& use)
{
    GenTree* tree = use.Def();
    assert(tree->OperIs(GT_HWINTRINSIC));

    GenTreeHWIntrinsic* hwintrinsicTree = tree->AsHWIntrinsic();

    switch (hwintrinsicTree->GetIntrinsic())
    {
        case NI_Vector128_GetElement:
        case NI_Vector256_GetElement:
            return DecomposeHWIntrinsicGetElement(use, hwintrinsicTree);

        default:
            noway_assert(!"unexpected GT_HWINTRINSIC node in long decomposition");
            break;
    }

    return nullptr;
}

//------------------------------------------------------------------------
// DecomposeHWIntrinsicGetElement: Decompose GT_HWINTRINSIC -- NI_Vector*_GetElement.
//
// Decompose a get[i] node on Vector*<long>. For:
//
// GT_HWINTRINSIC{GetElement}[long](simd_var, index)
//
// create:
//
// tmp_simd_var = simd_var
// tmp_index = index
// loResult = GT_HWINTRINSIC{GetElement}[int](tmp_simd_var, tmp_index * 2)
// hiResult = GT_HWINTRINSIC{GetElement}[int](tmp_simd_var, tmp_index * 2 + 1)
// return: GT_LONG(loResult, hiResult)
//
// This isn't optimal codegen, since NI_Vector*_GetElement sometimes requires
// temps that could be shared, for example.
//
// Arguments:
//    use - the LIR::Use object for the def that needs to be decomposed.
//   node - the hwintrinsic node to decompose
//
// Return Value:
//    The next node to process.
//
GenTree* DecomposeLongs::DecomposeHWIntrinsicGetElement(LIR::Use& use, GenTreeHWIntrinsic* node)
{
    assert(node == use.Def());
    assert((node->GetIntrinsic() == NI_Vector128_GetElement) || (node->GetIntrinsic() == NI_Vector256_GetElement));
    assert(varTypeIsLong(node->GetType()));

    GenTree*  op1      = node->GetOp(0);
    GenTree*  op2      = node->GetOp(1);
    var_types baseType = node->GetSimdBaseType();
    unsigned  simdSize = node->GetSimdSize();

    assert(varTypeIsLong(baseType));
    assert(varTypeIsSIMD(op1->GetType()));

    bool    indexIsConst = op2->IsIntCon();
    ssize_t index        = 0;

    if (indexIsConst)
    {
        index = op2->AsIntCon()->gtIconVal;
    }

    GenTree*   simdTmpVar = RepresentOpAsLclLoad(op1, node, &node->GetUse(0).NodeRef());
    LclVarDsc* simdTmpLcl = simdTmpVar->AsLclLoad()->GetLcl();
    JITDUMP("[DecomposeHWIntrinsicGetElement]: Saving op1 tree to a temp var:\n");
    DISPTREERANGE(Range(), simdTmpVar);
    Range().Remove(simdTmpVar);
    op1 = node->GetOp(0);

    GenTree*   indexTmpVar = nullptr;
    LclVarDsc* indexTmpLcl = nullptr;

    if (!indexIsConst)
    {
        indexTmpVar = RepresentOpAsLclLoad(op2, node, &node->GetUse(1).NodeRef());
        indexTmpLcl = indexTmpVar->AsLclLoad()->GetLcl();
        JITDUMP("[DecomposeHWIntrinsicGetElement]: Saving op2 tree to a temp var:\n");
        DISPTREERANGE(Range(), indexTmpVar);
        Range().Remove(indexTmpVar);
        op2 = node->GetOp(1);
    }

    // Create:
    //      loResult = GT_HWINTRINSIC{GetElement}[int](tmp_simd_var, index * 2)

    GenTree* simdTmpVar1 = simdTmpVar;
    GenTree* indexTimesTwo1;

    if (indexIsConst)
    {
        // Reuse the existing index constant node.
        indexTimesTwo1 = node->GetOp(1);
        Range().Remove(indexTimesTwo1);
        indexTimesTwo1->AsIntCon()->SetIconValue(index * 2);

        Range().InsertBefore(node, simdTmpVar1, indexTimesTwo1);
    }
    else
    {
        GenTree* indexTmpVar1 = indexTmpVar;
        GenTree* two1         = m_compiler->gtNewIconNode(2, TYP_INT);
        indexTimesTwo1        = m_compiler->gtNewOperNode(GT_MUL, TYP_INT, indexTmpVar1, two1);
        Range().InsertBefore(node, simdTmpVar1, indexTmpVar1, two1, indexTimesTwo1);
    }

    GenTree* loResult = m_compiler->gtNewSimdHWIntrinsicNode(TYP_INT, node->GetIntrinsic(), TYP_INT, simdSize,
                                                             simdTmpVar1, indexTimesTwo1);
    Range().InsertBefore(node, loResult);

    // Create:
    //      hiResult = GT_HWINTRINSIC{GetElement}[int](tmp_simd_var, index * 2 + 1)

    GenTree* simdTmpVar2 = m_compiler->gtNewLclLoad(simdTmpLcl, op1->GetType());
    GenTree* indexTimesTwoPlusOne;

    if (indexIsConst)
    {
        indexTimesTwoPlusOne = m_compiler->gtNewIconNode(index * 2 + 1, TYP_INT);
        Range().InsertBefore(node, simdTmpVar2, indexTimesTwoPlusOne);
    }
    else
    {
        GenTree* indexTmpVar2   = m_compiler->gtNewLclLoad(indexTmpLcl, TYP_INT);
        GenTree* two2           = m_compiler->gtNewIconNode(2, TYP_INT);
        GenTree* indexTimesTwo2 = m_compiler->gtNewOperNode(GT_MUL, TYP_INT, indexTmpVar2, two2);
        GenTree* one            = m_compiler->gtNewIconNode(1, TYP_INT);
        indexTimesTwoPlusOne    = m_compiler->gtNewOperNode(GT_ADD, TYP_INT, indexTimesTwo2, one);
        Range().InsertBefore(node, simdTmpVar2, indexTmpVar2, two2, indexTimesTwo2);
        Range().InsertBefore(node, one, indexTimesTwoPlusOne);
    }

    GenTree* hiResult = m_compiler->gtNewSimdHWIntrinsicNode(TYP_INT, node->GetIntrinsic(), TYP_INT, simdSize,
                                                             simdTmpVar2, indexTimesTwoPlusOne);
    Range().InsertBefore(node, hiResult);

    // Done with the original tree; remove it.

    Range().Remove(node);

    return FinalizeDecomposition(use, loResult, hiResult, hiResult);
}

#endif // FEATURE_HW_INTRINSICS

//------------------------------------------------------------------------
// OptimizeCastFromDecomposedLong: optimizes a cast from GT_LONG by discarding
// the high part of the source and, if the cast is to INT, the cast node itself.
// Accounts for side effects and marks nodes unused as neccessary.
//
// Only accepts casts to integer types that are not long.
// Does not optimize checked casts.
//
// Arguments:
//    cast     - the cast tree that has a GT_LONG node as its operand.
//    nextNode - the next candidate for decomposition.
//
// Return Value:
//    The next node to process in DecomposeRange: "nextNode->gtNext" if
//    "cast == nextNode", simply "nextNode" otherwise.
//
// Notes:
//    Because "nextNode" usually is "cast", and this method may remove "cast"
//    from the linear order, it needs to return the updated "nextNode". Instead
//    of receiving it as an argument, it could assume that "nextNode" is always
//    "cast->GetOp(0)->gtNext", but not making that assumption seems better.
//
GenTree* DecomposeLongs::OptimizeCastFromDecomposedLong(GenTreeCast* cast, GenTree* nextNode)
{
    GenTreeOp* src     = cast->GetOp(0)->AsOp();
    var_types  dstType = cast->GetType();

    assert(src->OperIs(GT_LONG));
    assert(varActualTypeIsInt(dstType));

    if (cast->HasOverflowCheck())
    {
        return nextNode;
    }

    GenTree* loSrc = src->GetOp(0);
    GenTree* hiSrc = src->GetOp(1);

    JITDUMP("Optimizing a truncating cast [%06u] from decomposed LONG [%06u]\n", cast->GetID(), src->GetID());
    INDEBUG(GenTree* treeToDisplay = cast);

    // TODO-CQ: we could go perform this removal transitively.
    // See also identical code in shift decomposition.
    if ((hiSrc->gtFlags & (GTF_ALL_EFFECT | GTF_SET_FLAGS)) == 0)
    {
        JITDUMP("Removing the HI part of [%06u] and marking its operands unused:\n", src->GetID());
        DISPNODE(hiSrc);
        Range().Remove(hiSrc, /* markOperandsUnused */ true);
    }
    else
    {
        JITDUMP("The HI part of [%06u] has side effects, marking it unused\n", src->GetID());
        hiSrc->SetUnusedValue();
    }

    JITDUMP("Removing the LONG source:\n");
    DISPNODE(src);
    Range().Remove(src);

    if (varTypeIsSmall(dstType))
    {
        JITDUMP("Cast is to a small type, keeping it, the new source is [%06u]\n", loSrc->GetID());
        cast->SetOp(0, loSrc);
    }
    else
    {
        LIR::Use useOfCast;
        if (Range().TryGetUse(cast, &useOfCast))
        {
            useOfCast.ReplaceWith(m_compiler, loSrc);
        }
        else
        {
            loSrc->SetUnusedValue();
        }

        if (nextNode == cast)
        {
            nextNode = nextNode->gtNext;
        }

        INDEBUG(treeToDisplay = loSrc);
        JITDUMP("Removing the cast:\n");
        DISPNODE(cast);

        Range().Remove(cast);
    }

    JITDUMP("Final result:\n")
    DISPTREERANGE(Range(), treeToDisplay);

    return nextNode;
}

//------------------------------------------------------------------------
// StoreMultiRegNodeToLcl: Check if the user is a STORE_LCL_VAR, and if it isn't,
// store the node to a var. Then decompose the new LclVar.
//
// Arguments:
//    use - the LIR::Use object for the def that needs to be decomposed.
//
// Return Value:
//    The next node to process.
//
GenTree* DecomposeLongs::StoreMultiRegNodeToLcl(LIR::Use& use)
{
    if (use.IsDummyUse())
    {
        return use.Def()->gtNext;
    }

    if (use.User()->OperIs(GT_LCL_STORE))
    {
        LclVarDsc* lcl = use.User()->AsLclStore()->GetLcl();

        if (lcl->IsIndependentPromoted())
        {
            lcl->lvIsMultiRegRet = true;
        }

        return use.Def()->gtNext;
    }

    if (!m_compiler->compEnregLocals() || m_compiler->lvaHaveManyLocals())
    {
        use.ReplaceWithLclLoad(m_compiler);

        return DecomposeLclLoad(use);
    }

    LclVarDsc* lcl = m_compiler->lvaNewTemp(TYP_LONG, true DEBUGARG("multireg LONG temp"));

    LclVarDsc* fieldLclLo = m_compiler->lvaNewTemp(TYP_INT, false DEBUGARG("promoted long field"));
    fieldLclLo->MakePromotedField(lcl->GetLclNum(), 0, FieldSeqStore::NotAField());

    LclVarDsc* fieldLclHi = m_compiler->lvaNewTemp(TYP_INT, false DEBUGARG("promoted long field"));
    fieldLclHi->MakePromotedField(lcl->GetLclNum(), 4, FieldSeqStore::NotAField());

    lcl->SetPromotedFields(fieldLclLo->GetLclNum(), 2);
    lcl->lvIsMultiRegRet = true;

    GenTreeLclStore* store  = m_compiler->gtNewLclStore(lcl, TYP_LONG, use.Def());
    GenTreeLclLoad*  loadLo = m_compiler->gtNewLclLoad(fieldLclLo, TYP_INT);
    GenTreeLclLoad*  loadHi = m_compiler->gtNewLclLoad(fieldLclHi, TYP_INT);

    Range().InsertAfter(use.Def(), store, loadLo, loadHi);

    return FinalizeDecomposition(use, loadLo, loadHi, loadHi);
}

//------------------------------------------------------------------------
// Check is op already local var, if not store it to local.
//
// Arguments:
//    op - GenTree* to represent as local variable
//    user - user of op
//    edge - edge from user to op
//
// Return Value:
//    op represented as local var
//
GenTreeLclLoad* DecomposeLongs::RepresentOpAsLclLoad(GenTree* op, GenTree* user, GenTree** edge)
{
    if (op->OperIs(GT_LCL_LOAD))
    {
        return op->AsLclLoad();
    }

    LIR::Use opUse(Range(), edge, user);
    opUse.ReplaceWithLclLoad(m_compiler);
    return (*edge)->AsLclLoad();
}

genTreeOps DecomposeLongs::GetHiOper(genTreeOps oper)
{
    switch (oper)
    {
        case GT_ADD:
            return GT_ADD_HI;
        case GT_OVF_SADD:
            return GT_OVF_SADDC;
        case GT_OVF_UADD:
            return GT_OVF_UADDC;
        case GT_SUB:
            return GT_SUB_HI;
        case GT_OVF_SSUB:
            return GT_OVF_SSUBB;
        case GT_OVF_USUB:
            return GT_OVF_USUBB;
        case GT_OR:
            return GT_OR;
        case GT_AND:
            return GT_AND;
        case GT_XOR:
            return GT_XOR;
        default:
            assert(!"GetHiOper called for invalid oper");
            return GT_NONE;
    }
}

genTreeOps DecomposeLongs::GetLoOper(genTreeOps oper)
{
    switch (oper)
    {
        case GT_ADD:
        case GT_OVF_SADD:
        case GT_OVF_UADD:
            return GT_ADD_LO;
        case GT_SUB:
        case GT_OVF_SSUB:
        case GT_OVF_USUB:
            return GT_SUB_LO;
        case GT_OR:
            return GT_OR;
        case GT_AND:
            return GT_AND;
        case GT_XOR:
            return GT_XOR;
        default:
            assert(!"GetLoOper called for invalid oper");
            return GT_NONE;
    }
}

void DecomposeLongs::PromoteLongVars()
{
    if (!m_compiler->compEnregLocals() || m_compiler->fgNoStructPromotion)
    {
        return;
    }

    for (LclVarDsc* lcl : m_compiler->Locals())
    {
        if (!lcl->TypeIs(TYP_LONG) || (lcl->GetRefCount() == 0))
        {
            continue;
        }

        if (lcl->lvDoNotEnregister || lcl->IsPromotedField() || lcl->lvWasStructField)
        {
            continue;
        }

#ifdef DEBUG
        if (lcl->IsParam() && m_compiler->fgNoStructParamPromotion)
        {
            continue;
        }
#endif

#ifdef TARGET_ARM
        // TODO-MIKE-CQ: Promote ARM long params.
        if (lcl->IsParam())
        {
            continue;
        }
#endif

        assert(!lcl->lvIsMultiRegArg && !lcl->lvIsMultiRegRet);
        lcl->lvContainsHoles = false;

        JITDUMP("\nPromoting long local V%02u:", lcl->GetLclNum());

        unsigned lclNum = lcl->GetLclNum();

        LclVarDsc* fieldLclLo = m_compiler->lvaNewTemp(TYP_INT, false DEBUGARG("promoted long field"));
        fieldLclLo->MakePromotedField(lclNum, 0, FieldSeq::NotAField());
        LclVarDsc* fieldLclHi = m_compiler->lvaNewTemp(TYP_INT, false DEBUGARG("promoted long field"));
        fieldLclHi->MakePromotedField(lclNum, 4, FieldSeq::NotAField());

        if (lcl->IsParam())
        {
            fieldLclLo->lvIsParam = true;
            fieldLclHi->lvIsParam = true;

            // Currently we do not support enregistering incoming promoted aggregates with more than one field.
            m_compiler->lvaSetDoNotEnregister(fieldLclLo DEBUGARG(Compiler::DNER_LongParamField));
            m_compiler->lvaSetDoNotEnregister(fieldLclHi DEBUGARG(Compiler::DNER_LongParamField));
        }

        lcl->SetPromotedFields(fieldLclLo->GetLclNum(), 2);
    }

#ifdef DEBUG
    if (m_compiler->verbose)
    {
        printf("\nlvaTable after PromoteLongVars\n");
        m_compiler->lvaTableDump();
    }
#endif // DEBUG
}
#endif // !TARGET_64BIT
