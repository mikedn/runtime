// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

// This file contains code to decompose 64-bit LONG operations on 32-bit platforms
// into multiple single-register operations so individual register usage and requirements
// are explicit for LSRA. The rationale behind this is to avoid adding code complexity
// downstream caused by the introduction of handling longs as special cases,
// especially in LSRA.
// Long decomposition happens on a statement immediately prior to more general
// purpose lowering.

#include "jitpch.h"

#ifndef TARGET_64BIT // DecomposeLongs is only used on 32-bit platforms

#include "decomposelongs.h"

// Do LONG decomposition on all the nodes in the given block. This must
// be done before lowering the block, as decomposition can insert
// additional nodes.
// Decomposition is done as an execution-order walk. Decomposition of a particular
// node can create new nodes that need to be further decomposed at higher levels.
// That is, decomposition "bubbles up" through dataflow.
void DecomposeLongs::DecomposeBlock(BasicBlock* block)
{
    assert(block->isEmpty() || block->IsLIR());
    m_range = &LIR::AsRange(block);

    for (GenTree* node = Range().FirstNode(); node != nullptr;)
    {
        node = DecomposeNode(node);
    }

    assert(Range().CheckLIR(m_compiler, true));
}

GenTree* DecomposeLongs::DecomposeNode(GenTree* tree)
{
    // Handle the case where we are implicitly using the lower half of a long lclVar.
    if (tree->TypeIs(TYP_INT) && tree->OperIs(GT_LCL_LOAD))
    {
        LclVarDsc* lcl = tree->AsLclLoad()->GetLcl();

        if (lcl->TypeIs(TYP_LONG) && lcl->IsPromoted())
        {
            JITDUMPLIRRANGE(
                Range(), tree,
                "Changing implicit reference to low half of LONG local to an explicit reference of its promoted "
                "half:\n");

            tree->AsLclLoad()->SetLcl(m_compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(0)));

            return tree->gtNext;
        }
    }

    if (!tree->TypeIs(TYP_LONG))
    {
        return tree->gtNext;
    }

    JITDUMPLIRRANGE(Range(), tree, "Decomposing LONG tree. BEFORE:\n");

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
        case GT_OVF_U:
            nextNode = DecomposeOvfUnsigned(use);
            break;
        case GT_SXT:
            nextNode = DecomposeSignExtend(use);
            break;
        case GT_UXT:
            nextNode = DecomposeUnsignedExtend(use);
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
        case GT_BSWAP:
            nextNode = DecomposeBswap(use);
            break;
        case GT_AND:
        case GT_OR:
        case GT_XOR:
            nextNode = DecomposeBitwise(use);
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
            nextNode = DecomposeAddSub(use);
            break;
        case GT_MUL:
            nextNode = DecomposeMul(use);
            break;
        case GT_UREM:
            nextNode = DecomposeUMod(use);
            break;
        case GT_LSH:
        case GT_RSH:
        case GT_RSZ:
            nextNode = DecomposeShift(use);
            break;
#ifdef TARGET_XARCH
        case GT_ROL:
#endif
        case GT_ROR:
            nextNode = DecomposeRotate(use);
            break;
#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            nextNode = DecomposeHWIntrinsic(use);
            break;
#endif
        case GT_LOCKADD:
        case GT_XORR:
        case GT_XAND:
        case GT_XADD:
        case GT_XCHG:
        case GT_CMPXCHG:
            NYI("Interlocked operations on LONG");
            break;
        default:
            JITDUMP("Illegal LONG node %s in decomposition.", GenTree::OpName(tree->GetOper()));
            assert(!"Illegal LONG node in decomposition.");
            break;
    }

    JITDUMPLIRRANGE(Range(), use.Def(), "Decomposing LONG tree. AFTER:\n");

    if (!use.IsDummyUse() && use.User()->OperIs(GT_TRUNC))
    {
        nextNode = OptimizeTruncate(use.User()->AsUnOp(), nextNode);
    }

    return nextNode;
}

// A helper function to finalize LONG decomposition by taking the resulting two halves
// of the decomposition, and tie them together with a new GT_LONG node that will replace
// the original node.
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

    GenTree* gtLong = m_compiler->gtNewOperNode(GT_LONG, TYP_LONG, loResult, hiResult);
    gtLong->SetSideEffects(GTF_NONE);
    gtLong->SetContained();

    loResult->ClearUnusedValue();
    hiResult->ClearUnusedValue();

    Range().InsertAfter(insertResultAfter, gtLong);

    use.SetDef(gtLong);

    return gtLong->gtNext;
}

GenTree* DecomposeLongs::DecomposeLclLoad(LIR::Use& use)
{
    GenTreeLclLoad* load     = use.Def()->AsLclLoad();
    LclVarDsc*      lcl      = load->GetLcl();
    GenTree*        loResult = load;
    loResult->SetType(TYP_INT);

    GenTree* hiResult = m_compiler->gtNewLclLoad(lcl, TYP_INT);
    Range().InsertAfter(loResult, hiResult);

    if (lcl->IsPromoted())
    {
        assert(lcl->GetPromotedFieldCount() == 2);

        loResult->AsLclLoad()->SetLcl(m_compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(0)));
        hiResult->AsLclLoad()->SetLcl(m_compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(1)));
    }
    else
    {
        m_compiler->lvaSetDoNotEnregister(lcl DEBUGARG(Compiler::DNER_LocalField));

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
    GenTree*       tree     = use.Def();
    GenTreeLclFld* loResult = tree->AsLclLoadFld();
    loResult->SetType(TYP_INT);

    GenTree* hiResult = m_compiler->gtNewLclLoadFld(TYP_INT, loResult->GetLcl(), loResult->GetLclOffs() + 4);
    Range().InsertAfter(loResult, hiResult);

    return FinalizeDecomposition(use, loResult, hiResult, hiResult);
}

GenTree* DecomposeLongs::DecomposeLclStore(LIR::Use& use)
{
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
        // We cannot decompose a LCL_STORE that is not promoted because doing so
        // changes its liveness semantics - the resulting LCL_STORE_FLDs as partial
        // definitions and thus are also uses, while the original store was a full
        // definition of the local.
        // This difference will affect any situation in which the liveness of a variable
        // at a def matters (e.g. dead store elimination, live-in sets, etc.). As a
        // result, we leave these stores as-is and generate the decomposed store in the
        // the code generator.
        // NOTE: this does extend the lifetime of the low half of the `GT_LONG`
        // node as compared to the decomposed form. If we start doing more code
        // motion in the backend, this may cause some CQ issues and some sort of
        // decomposition could be beneficial.
        return tree->gtNext;
    }

    assert(lcl->GetPromotedFieldCount() == 2);
    GenTreeOp* value = rhs->AsOp();
    Range().Unlink(value);

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
    GenTreeLclStoreFld* store = use.Def()->AsLclStoreFld();

    GenTreeOp* value = store->GetValue()->AsOp();
    assert(value->OperIs(GT_LONG));
    Range().Unlink(value);

    // The original store node will be repurposed to store the low half of the GT_LONG.
    store->SetValue(value->GetOp(0));
    store->SetType(TYP_INT);

    // Create the store for the upper half of the GT_LONG and insert it after the low store.
    GenTreeLclFld* hiStore =
        m_compiler->gtNewLclStoreFld(TYP_INT, store->GetLcl(), store->GetLclOffs() + 4, value->GetOp(1));

    Range().InsertAfter(store, hiStore);

    return hiStore->gtNext;
}

GenTree* DecomposeLongs::DecomposeOvfUnsigned(LIR::Use& use)
{
    GenTreeUnOp* cast = use.Def()->AsUnOp();

    assert(cast->OperIs(GT_OVF_U) && cast->TypeIs(TYP_LONG) && cast->GetOp(0)->TypeIs(TYP_LONG));

    GenTree* loResult = nullptr;
    GenTree* hiResult = nullptr;

    GenTreeOp* srcOp = cast->GetOp(0)->AsOp();
    assert(srcOp->OperIs(GT_LONG));
    GenTree* loSrcOp = srcOp->GetOp(0);
    GenTree* hiSrcOp = srcOp->GetOp(1);

    cast->SetType(TYP_INT);
    cast->SetOp(0, hiSrcOp);

    Range().Unlink(srcOp);

    return FinalizeDecomposition(use, loSrcOp, cast, cast);
}

GenTree* DecomposeLongs::DecomposeSignExtend(LIR::Use& use)
{
    GenTreeUnOp* sxt = use.Def()->AsUnOp();
    assert(sxt->OperIs(GT_SXT) && sxt->TypeIs(TYP_LONG));

    if (varTypeIsSmallUnsigned(sxt->GetOp(0)->GetType()))
    {
        sxt->SetOper(GT_UXT);

        return DecomposeUnsignedExtend(use);
    }

    if (!use.IsDummyUse() && use.User()->OperIs(GT_MUL))
    {
        // This INT to LONG cast is used by a MUL that will be transformed by DecomposeMul into
        // a SMULL and as a result the high operand produced by the cast will become dead.
        // Skip cast decomposition so DecomposeMul doesn't need to bother with dead code removal,
        // especially in the case of sign extending casts that also introduce new locals.

        return sxt->gtNext;
    }

    LIR::Use   src(Range(), &sxt->gtOp1, sxt);
    LclVarDsc* lcl = src.ReplaceWithLclLoad(m_compiler);

    GenTree* loResult = src.Def();
    GenTree* loCopy   = m_compiler->gtNewLclLoad(lcl, TYP_INT);
    GenTree* shiftBy  = m_compiler->gtNewIconNode(31, TYP_INT);
    GenTree* hiResult = m_compiler->gtNewOperNode(GT_RSH, TYP_INT, loCopy, shiftBy);

    Range().InsertAfter(sxt, loCopy, shiftBy, hiResult);
    Range().Unlink(sxt);

    return FinalizeDecomposition(use, loResult, hiResult, hiResult);
}

GenTree* DecomposeLongs::DecomposeUnsignedExtend(LIR::Use& use)
{
    GenTreeUnOp* uxt = use.Def()->AsUnOp();
    assert(uxt->OperIs(GT_UXT) && uxt->TypeIs(TYP_LONG));

    if (!use.IsDummyUse() && use.User()->OperIs(GT_MUL))
    {
        // This INT to LONG cast is used by a MUL that will be transformed by DecomposeMul into
        // a UMULL and as a result the high operand produced by the cast will become dead.
        // Skip cast decomposition so DecomposeMul doesn't need to bother with dead code removal,
        // especially in the case of sign extending casts that also introduce new locals.

        return uxt->gtNext;
    }

    GenTree* loResult = uxt->GetOp(0);
    GenTree* hiResult = m_compiler->gtNewIconNode(0);

    Range().InsertAfter(uxt, hiResult);
    Range().Unlink(uxt);

    return FinalizeDecomposition(use, loResult, hiResult, hiResult);
}

GenTree* DecomposeLongs::DecomposeCnsLng(LIR::Use& use)
{
    GenTreeLngCon* node = use.Def()->AsLngCon();

    int32_t loVal = static_cast<int32_t>(node->GetValue() & UINT32_MAX);
    int32_t hiVal = static_cast<int32_t>(node->GetValue() >> 32);

    node->ChangeToIntCon(TYP_INT, loVal);

    GenTree* hiResult = m_compiler->gtNewIconNode(hiVal, TYP_INT);
    Range().InsertAfter(node, hiResult);

    return FinalizeDecomposition(use, node, hiResult, hiResult);
}

GenTree* DecomposeLongs::DecomposeCall(LIR::Use& use)
{
    GenTreeCall* call = use.Def()->AsCall();

    if (use.IsDummyUse())
    {
        call->SetType(TYP_VOID);
        call->SetRetSigType(TYP_VOID);
        call->GetRetDesc()->Reset();

        return call->gtNext;
    }

    if (use.User()->OperIs(GT_TRUNC))
    {
        GenTree* trunc = use.User();

        if (LIR::Use truncUse; Range().TryGetUse(trunc, &truncUse))
        {
            call->SetType(TYP_INT);
            call->SetRetSigType(TYP_INT);
            call->GetRetDesc()->InitializePrimitive(TYP_INT);

            truncUse.SetDef(call);
            use = truncUse;
        }
        else
        {
            call->SetType(TYP_VOID);
            call->SetRetSigType(TYP_VOID);
            call->GetRetDesc()->Reset();
        }

        Range().Unlink(trunc);

        return call->gtNext;
    }

    return StoreMultiRegNodeToLcl(use);
}

GenTree* DecomposeLongs::DecomposeIndStore(LIR::Use& use)
{
    GenTreeIndStore* store  = use.Def()->AsIndStore();
    GenTreeOp*       gtLong = store->GetValue()->AsOp();

    assert(gtLong->OperIs(GT_LONG));

    // Save address to a temp. It is used in storeIndLow and storeIndHigh trees.
    LIR::Use address(Range(), &store->gtOp1, store);
    address.ReplaceWithLclLoad(m_compiler);
    JITDUMPLIRRANGE(Range(), address.Def(), "[DecomposeIndStore]: Saving address tree to a temp var:\n");

    if (!gtLong->GetOp(0)->OperIsLeaf())
    {
        LIR::Use op1(Range(), &gtLong->gtOp1, gtLong);
        op1.ReplaceWithLclLoad(m_compiler);
        JITDUMPLIRRANGE(Range(), op1.Def(), "[DecomposeIndStore]: Saving low data tree to a temp var:\n");
    }

    if (!gtLong->GetOp(1)->OperIsLeaf())
    {
        LIR::Use op2(Range(), &gtLong->gtOp2, gtLong);
        op2.ReplaceWithLclLoad(m_compiler);
        JITDUMPLIRRANGE(Range(), op2.Def(), "[DecomposeIndStore]: Saving high data tree to a temp var:\n");
    }

    GenTreeLclLoad*  addrBase    = store->GetAddr()->AsLclLoad();
    GenTree*         dataLow     = gtLong->GetOp(0);
    GenTree*         dataHigh    = gtLong->GetOp(1);
    GenTreeIndStore* storeIndLow = store;

    Range().Unlink(gtLong);
    Range().Unlink(dataHigh);
    storeIndLow->SetValue(dataLow);
    storeIndLow->SetType(TYP_INT);

    assert(addrBase->TypeIs(TYP_BYREF, TYP_I_IMPL));
    GenTree* addrBaseHigh = m_compiler->gtNewLclLoad(addrBase->GetLcl(), addrBase->GetType());
    GenTree* addrHigh     = m_compiler->gtNewAddrMode(addrBaseHigh, 4);
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

    indLow->SetType(TYP_INT);

    GenTreeLclLoad* addrBase = indLow->GetAddr()->AsLclLoad();
    assert(addrBase->TypeIs(TYP_BYREF, TYP_I_IMPL));

    GenTree* addrBaseHigh = m_compiler->gtNewLclLoad(addrBase->GetLcl(), addrBase->GetType());
    GenTree* addrHigh     = m_compiler->gtNewAddrMode(addrBaseHigh, 4);
    GenTree* indHigh      = m_compiler->gtNewIndLoad(TYP_INT, addrHigh);
    indHigh->gtFlags |= (indLow->gtFlags & (GTF_GLOB_REF | GTF_EXCEPT | GTF_SPECIFIC_MASK));

    Range().InsertAfter(indLow, addrBaseHigh, addrHigh, indHigh);

    return FinalizeDecomposition(use, indLow, indHigh, indHigh);
}

GenTree* DecomposeLongs::DecomposeNot(LIR::Use& use)
{
    assert(use.Def()->OperIs(GT_NOT));

    GenTreeUnOp* node  = use.Def()->AsUnOp();
    GenTreeOp*   value = node->GetOp(0)->AsOp();
    assert(value->OperIs(GT_LONG));
    GenTree* loValue = value->GetOp(0);
    GenTree* hiValue = value->GetOp(1);
    Range().Unlink(value);

    node->SetType(TYP_INT);
    node->SetOp(0, loValue);

    GenTree* hiNode = m_compiler->gtNewOperNode(GT_NOT, TYP_INT, hiValue);
    Range().InsertAfter(node, hiNode);

    return FinalizeDecomposition(use, node, hiNode, hiNode);
}

GenTree* DecomposeLongs::DecomposeBswap(LIR::Use& use)
{
    assert(use.Def()->OperIs(GT_BSWAP));

    GenTreeUnOp* node  = use.Def()->AsUnOp();
    GenTreeOp*   value = node->GetOp(0)->AsOp();
    assert(value->OperIs(GT_LONG));
    GenTree* loValue = value->GetOp(0);
    GenTree* hiValue = value->GetOp(1);
    Range().Unlink(value);

    node->SetType(TYP_INT);
    node->SetOp(0, loValue);

    GenTree* hiNode = m_compiler->gtNewOperNode(GT_BSWAP, TYP_INT, hiValue);
    Range().InsertAfter(node, hiNode);

    return FinalizeDecomposition(use, hiNode, node, hiNode);
}

GenTree* DecomposeLongs::DecomposeNeg(LIR::Use& use)
{
    assert(use.Def()->OperIs(GT_NEG));

    GenTreeUnOp* node  = use.Def()->AsUnOp();
    GenTreeOp*   value = node->GetOp(0)->AsOp();
    assert(value->OperIs(GT_LONG));
    GenTree* loValue = value->GetOp(0);
    GenTree* hiValue = value->GetOp(1);
    Range().Unlink(value);

    node->SetType(TYP_INT);
    node->SetOp(0, loValue);

    GenTree* zero = m_compiler->gtNewIconNode(0);

#if defined(TARGET_X86)
    node->AddImplicitFlagsDef();
    GenTree* hiAdjust = m_compiler->gtNewOperNode(GT_ADD_HI, TYP_INT, hiValue, zero);
    hiAdjust->AddImplicitFlagsUse();
    GenTree* hiResult = m_compiler->gtNewOperNode(GT_NEG, TYP_INT, hiAdjust);
    Range().InsertAfter(node, zero, hiAdjust, hiResult);
#elif defined(TARGET_ARM)
    // We tend to use "movs" to load zero to a register, and that sets the flags, so
    // put the zero before the node, which is setting the flags needed by GT_SUB_HI.
    node->AddImplicitFlagsDef();
    GenTree* hiResult = m_compiler->gtNewOperNode(GT_SUB_HI, TYP_INT, zero, hiValue);
    hiResult->AddImplicitFlagsUse();
    Range().InsertBefore(node, zero);
    Range().InsertAfter(node, hiResult);
#endif

    return FinalizeDecomposition(use, node, hiResult, hiResult);
}

GenTree* DecomposeLongs::DecomposeAddSub(LIR::Use& use)
{
    GenTreeOp* node = use.Def()->AsOp();

    genTreeOps loOper;
    genTreeOps hiOper;

    switch (node->GetOper())
    {
        case GT_ADD:
            loOper = GT_ADD_LO;
            hiOper = GT_ADD_HI;
            break;
        case GT_OVF_SADD:
            loOper = GT_ADD_LO;
            hiOper = GT_OVF_SADDC;
            break;
        case GT_OVF_UADD:
            loOper = GT_ADD_LO;
            hiOper = GT_OVF_UADDC;
            break;
        case GT_SUB:
            loOper = GT_SUB_LO;
            hiOper = GT_SUB_HI;
            break;
        case GT_OVF_SSUB:
            loOper = GT_SUB_LO;
            hiOper = GT_OVF_SSUBB;
            break;
        default:
            assert(node->OperIs(GT_OVF_USUB));
            loOper = GT_SUB_LO;
            hiOper = GT_OVF_USUBB;
            break;
    }

    GenTreeOp* op1 = node->GetOp(0)->AsOp();
    GenTreeOp* op2 = node->GetOp(1)->AsOp();
    assert(op1->OperIs(GT_LONG) && op2->OperIs(GT_LONG));
    GenTree* loOp1 = op1->GetOp(0);
    GenTree* hiOp1 = op1->GetOp(1);
    GenTree* loOp2 = op2->GetOp(0);
    GenTree* hiOp2 = op2->GetOp(1);

    Range().Unlink(op1);
    Range().Unlink(op2);

    node->SetOper(loOper);
    node->SetType(TYP_INT);
    node->AsOp()->SetOp(0, loOp1);
    node->AsOp()->SetOp(1, loOp2);

    GenTree* hiNode = m_compiler->gtNewOperNode(hiOper, TYP_INT, hiOp1, hiOp2);

    node->AddImplicitFlagsDef();
    hiNode->AddImplicitFlagsUse();

    if (hiNode->OperIs(GT_OVF_SADDC, GT_OVF_UADDC, GT_OVF_SSUBB, GT_OVF_USUBB))
    {
        node->RemoveSideEffects(GTF_EXCEPT);
        hiNode->AddSideEffects(GTF_EXCEPT);
    }

    Range().InsertAfter(node, hiNode);
    return FinalizeDecomposition(use, node, hiNode, hiNode);
}

GenTree* DecomposeLongs::DecomposeBitwise(LIR::Use& use)
{
    GenTreeOp* node = use.Def()->AsOp();
    assert(node->OperIs(GT_AND, GT_OR, GT_XOR));

    GenTreeOp* op1 = node->GetOp(0)->AsOp();
    GenTreeOp* op2 = node->GetOp(1)->AsOp();
    assert(op1->OperIs(GT_LONG) && op2->OperIs(GT_LONG));
    GenTree* loOp1 = op1->GetOp(0);
    GenTree* hiOp1 = op1->GetOp(1);
    GenTree* loOp2 = op2->GetOp(0);
    GenTree* hiOp2 = op2->GetOp(1);

    Range().Unlink(op1);
    Range().Unlink(op2);

    node->SetType(TYP_INT);
    node->AsOp()->SetOp(0, loOp1);
    node->AsOp()->SetOp(1, loOp2);

    GenTree* hiNode = m_compiler->gtNewOperNode(node->GetOper(), TYP_INT, hiOp1, hiOp2);

    Range().InsertAfter(node, hiNode);
    return FinalizeDecomposition(use, node, hiNode, hiNode);
}

GenTree* DecomposeLongs::DecomposeShift(LIR::Use& use)
{
    assert(use.Def()->OperIs(GT_LSH, GT_RSH, GT_RSZ));

    GenTreeOp* shift     = use.Def()->AsOp();
    GenTreeOp* value     = shift->GetOp(0)->AsOp();
    GenTree*   loValue   = value->GetOp(0);
    GenTree*   hiValue   = value->GetOp(1);
    GenTree*   shiftByOp = shift->GetOp(1);
    genTreeOps oper      = shift->GetOper();

    // For shift nodes being shifted by a constant int, we can inspect the shift amount
    // and decompose to the appropriate node types, generating a SHL/SHLD pattern for LSH,
    // a SHRD/SHR pattern for RSZ, and a SHRD/SAR pattern for SHR for most shift amounts.
    // Shifting by 0, >= 32 and >= 64 are special cased to produce better code patterns.

    if (GenTreeIntCon* imm = shiftByOp->IsIntCon())
    {
        // Reduce count modulo 64 to match behavior found in the shift helpers,
        // Compiler::gtFoldExpr and ValueNumStore::EvalOpIntegral.
        unsigned count = static_cast<unsigned>(imm->GetValue() & 0x3F);
        Range().Unlink(shiftByOp);

        if (count == 0)
        {
            GenTree* next = shift->gtNext;

            if (shift->IsUnusedValue())
            {
                value->SetUnusedValue();
            }

            Range().Unlink(shift);
            use.SetDef(value);

            return next;
        }

        GenTree* loResult;
        GenTree* hiResult;
        GenTree* insertAfter;

        if (oper == GT_LSH)
        {
            if (count < 32)
            {
                // reg1 = lo
                // SHL lo, imm
                // SHLD hi, reg1, imm

                Range().Unlink(value);

                loValue             = RepresentOpAsLclLoad(loValue, value, &value->AsOp()->gtOp1);
                LclVarDsc* loOp1Lcl = loValue->AsLclLoad()->GetLcl();
                Range().Unlink(loValue);

                GenTree* shiftByHi = m_compiler->gtNewIconNode(count, TYP_INT);
                GenTree* shiftByLo = m_compiler->gtNewIconNode(count, TYP_INT);

                loResult = m_compiler->gtNewOperNode(GT_LSH, TYP_INT, loValue, shiftByLo);

                // Create a GT_LONG that contains loCopy and hiOp1. This will be used in codegen to
                // generate the shld instruction
                GenTree* loCopy = m_compiler->gtNewLclLoad(loOp1Lcl, TYP_INT);
                GenTree* hiOp   = m_compiler->gtNewOperNode(GT_LONG, TYP_LONG, loCopy, hiValue);
                hiOp->SetContained();
                hiResult = m_compiler->gtNewOperNode(GT_LSH_HI, TYP_INT, hiOp, shiftByHi);

                Range().InsertBefore(shift, loValue, shiftByLo, loResult);
                Range().InsertBefore(shift, loCopy, hiOp, shiftByHi, hiResult);

                insertAfter = hiResult;
            }
            else
            {
                assert(count >= 32 && count < 64);

                // Since we're left shifting at least 32 bits, we can remove the hi part of the shifted value iff
                // it has no side effects.
                // TODO-CQ: we could go perform this removal transitively (i.e. iteratively remove everything that
                // feeds the hi operand while there are no side effects)

                if (hiValue->HasSideEffects())
                {
                    hiValue->SetUnusedValue();
                }
                else
                {
                    Range().Remove(hiValue, true);
                }

                if (count == 32)
                {
                    // Move loOp1 into hiResult (shift of 32 bits is just a mov of lo to hi)
                    // We need to make sure that we save lo to a temp variable so that we don't overwrite lo
                    // before saving it to hi in the case that we are doing an inplace shift. I.e.:
                    // x = x << 32

                    LIR::Use loOp1Use(Range(), &value->AsOp()->gtOp1, value);
                    loOp1Use.ReplaceWithLclLoad(m_compiler);

                    hiResult = loOp1Use.Def();
                    Range().Unlink(value);
                }
                else
                {
                    Range().Unlink(value);
                    assert(count > 32 && count < 64);

                    // Move loOp1 into hiResult, do a GT_LSH with count - 32.
                    // We will compute hiResult before loResult in this case, so we don't need to store lo to a
                    // temp
                    GenTree* shiftBy = m_compiler->gtNewIconNode(count - 32, TYP_INT);
                    hiResult         = m_compiler->gtNewOperNode(GT_LSH, TYP_INT, loValue, shiftBy);
                    Range().InsertBefore(shift, shiftBy, hiResult);
                }

                // Zero out loResult (shift of >= 32 bits shifts all lo bits to hiResult)
                loResult = m_compiler->gtNewIconNode(0);
                Range().InsertBefore(shift, loResult);

                insertAfter = loResult;
            }
        }
        else if (oper == GT_RSZ)
        {
            Range().Unlink(value);

            if (count < 32)
            {
                // reg1 = hi
                // SHRD lo, reg1, shift
                // SHR hi, shift

                hiValue             = RepresentOpAsLclLoad(hiValue, value, &value->AsOp()->gtOp2);
                LclVarDsc* hiOp1Lcl = hiValue->AsLclLoad()->GetLcl();
                GenTree*   hiCopy   = m_compiler->gtNewLclLoad(hiOp1Lcl, TYP_INT);

                GenTree* shiftByHi = m_compiler->gtNewIconNode(count, TYP_INT);
                GenTree* shiftByLo = m_compiler->gtNewIconNode(count, TYP_INT);

                hiResult = m_compiler->gtNewOperNode(GT_RSZ, TYP_INT, hiValue, shiftByHi);

                // Create a GT_LONG that contains loOp1 and hiCopy. This will be used in codegen to
                // generate the shrd instruction
                GenTree* loOp = m_compiler->gtNewOperNode(GT_LONG, TYP_LONG, loValue, hiCopy);
                loOp->SetContained();
                loResult = m_compiler->gtNewOperNode(GT_RSH_LO, TYP_INT, loOp, shiftByLo);

                Range().InsertBefore(shift, hiCopy, loOp);
                Range().InsertBefore(shift, shiftByLo, loResult);
                Range().InsertBefore(shift, shiftByHi, hiResult);
            }
            else
            {
                assert(count >= 32 && count < 64);

                // Since we're right shifting at least 32 bits, we can remove the lo part of the shifted value iff
                // it has no side effects.
                // TODO-CQ: we could go perform this removal transitively (i.e. iteratively remove everything that
                // feeds the lo operand while there are no side effects)

                if (loValue->HasAnySideEffect(GTF_ALL_EFFECT) || loValue->HasImplicitFlagsDef())
                {
                    loValue->SetUnusedValue();
                }
                else
                {
                    Range().Remove(loValue, true);
                }

                if (count == 32)
                {
                    loResult = hiValue;
                }
                else
                {
                    assert(count > 32 && count < 64);

                    GenTree* shiftBy = m_compiler->gtNewIconNode(count - 32, TYP_INT);
                    loResult         = m_compiler->gtNewOperNode(GT_RSZ, TYP_INT, hiValue, shiftBy);
                    Range().InsertBefore(shift, shiftBy, loResult);
                }

                hiResult = m_compiler->gtNewIconNode(0);
                Range().InsertBefore(shift, hiResult);
            }

            insertAfter = hiResult;
        }
        else
        {
            assert(oper == GT_RSH);

            Range().Unlink(value);

            hiValue             = RepresentOpAsLclLoad(hiValue, value, &value->AsOp()->gtOp2);
            LclVarDsc* hiOp1Lcl = hiValue->AsLclLoad()->GetLcl();
            GenTree*   hiCopy   = m_compiler->gtNewLclLoad(hiOp1Lcl, TYP_INT);
            Range().Unlink(hiValue);

            if (count < 32)
            {
                // reg1 = hi
                // SHRD lo, reg1, shift
                // SAR hi, shift

                GenTree* shiftByHi = m_compiler->gtNewIconNode(count, TYP_INT);
                GenTree* shiftByLo = m_compiler->gtNewIconNode(count, TYP_INT);

                hiResult = m_compiler->gtNewOperNode(GT_RSH, TYP_INT, hiValue, shiftByHi);

                GenTree* loOp = m_compiler->gtNewOperNode(GT_LONG, TYP_LONG, loValue, hiCopy);
                loOp->SetContained();
                loResult = m_compiler->gtNewOperNode(GT_RSH_LO, TYP_INT, loOp, shiftByLo);

                Range().InsertBefore(shift, hiCopy, loOp);
                Range().InsertBefore(shift, shiftByLo, loResult);
                Range().InsertBefore(shift, shiftByHi, hiValue, hiResult);
            }
            else
            {
                assert(count >= 32 && count < 64);

                // Since we're right shifting at least 32 bits, we can remove the lo part of the shifted value iff
                // it has no side effects.
                // TODO-CQ: we could go perform this removal transitively (i.e. iteratively remove everything that
                // feeds the lo operand while there are no side effects)

                if (loValue->HasAnySideEffect(GTF_ALL_EFFECT) || loValue->HasImplicitFlagsDef())
                {
                    loValue->SetUnusedValue();
                }
                else
                {
                    Range().Remove(loValue, true);
                }

                if (count == 32)
                {
                    loResult = hiValue;
                    Range().InsertBefore(shift, loResult);
                }
                else
                {
                    assert(count > 32 && count < 64);

                    GenTree* shiftBy = m_compiler->gtNewIconNode(count - 32, TYP_INT);
                    loResult         = m_compiler->gtNewOperNode(GT_RSH, TYP_INT, hiValue, shiftBy);
                    Range().InsertBefore(shift, hiValue, shiftBy, loResult);
                }

                GenTree* shiftBy = m_compiler->gtNewIconNode(31, TYP_INT);
                hiResult         = m_compiler->gtNewOperNode(GT_RSH, TYP_INT, hiCopy, shiftBy);
                Range().InsertBefore(shift, shiftBy, hiCopy, hiResult);
            }

            insertAfter = hiResult;
        }

        Range().Unlink(shift);

        return FinalizeDecomposition(use, loResult, hiResult, insertAfter);
    }

    // For all other shift nodes, we need to use the shift helper functions, so we here convert
    // the shift into a helper call by pulling its arguments out of linear order and making
    // them the args to a call, then replacing the original node with the new call.

    // Because calls must be created as HIR and lowered to LIR, we need to dump
    // any LIR temps into lclVars before using them as arguments.

    shiftByOp = RepresentOpAsLclLoad(shiftByOp, shift, &shift->AsOp()->gtOp2);
    loValue   = RepresentOpAsLclLoad(loValue, value, &value->AsOp()->gtOp1);
    hiValue   = RepresentOpAsLclLoad(hiValue, value, &value->AsOp()->gtOp2);

    Range().Unlink(shiftByOp);
    Range().Unlink(value);
    Range().Unlink(loValue);
    Range().Unlink(hiValue);

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

    GenTreeCall::Use* argList = m_compiler->gtNewCallArgs(loValue, hiValue, shiftByOp);
    GenTreeCall*      call    = m_compiler->gtNewHelperCallNode(helper, TYP_LONG, argList);

    if (shift->IsUnusedValue())
    {
        call->SetUnusedValue();
    }

    LIR::InsertHelperCallBefore(m_compiler, Range(), shift, call);

    Range().Unlink(shift);
    use.SetDef(call);

    return DecomposeCall(use);
}

GenTree* DecomposeLongs::DecomposeRotate(LIR::Use& use)
{
    assert(use.Def()->OperIs(GT_ROL, GT_ROR));

    GenTreeOp* node       = use.Def()->AsOp();
    GenTreeOp* value      = node->GetOp(0)->AsOp();
    GenTree*   rotateByOp = node->GetOp(1);
    genTreeOps oper       = node->GetOper();

    unsigned count = rotateByOp->AsIntCon()->GetUInt32Value();
    assert((0 < count) && (count < 64));

    Range().Unlink(rotateByOp);

    GenTree* loResult;
    GenTree* hiResult;

    if (count == 32)
    {
        // If the rotate amount is 32, then swap hi and lo
        LIR::Use loOp1Use(Range(), &value->AsOp()->gtOp1, value);
        loOp1Use.ReplaceWithLclLoad(m_compiler);

        LIR::Use hiOp1Use(Range(), &value->AsOp()->gtOp2, value);
        hiOp1Use.ReplaceWithLclLoad(m_compiler);

        hiResult = loOp1Use.Def();
        loResult = hiOp1Use.Def();

        value->SetOp(0, loResult);
        value->SetOp(1, hiResult);

        if (node->IsUnusedValue())
        {
            value->SetUnusedValue();
        }

        GenTree* next = node->gtNext;
        Range().Unlink(node);
        use.SetDef(value);

        return next;
    }

    // SHLD lo, hi, imm
    // SHLD hi, loCopy, imm
    //
    // SHRD lo, hi, imm
    // SHRD hi, loCopy, imm

    GenTree* loOp1;
    GenTree* hiOp1;

    if (count > 32)
    {
        // If count > 32, we swap hi and lo, and subtract 32 from count
        hiOp1 = value->GetOp(0);
        loOp1 = value->GetOp(1);

        Range().Unlink(value);
        loOp1 = RepresentOpAsLclLoad(loOp1, value, &value->AsOp()->gtOp2);
        hiOp1 = RepresentOpAsLclLoad(hiOp1, value, &value->AsOp()->gtOp1);

        count -= 32;
    }
    else
    {
        loOp1 = value->GetOp(0);
        hiOp1 = value->GetOp(1);

        Range().Unlink(value);
        loOp1 = RepresentOpAsLclLoad(loOp1, value, &value->AsOp()->gtOp1);
        hiOp1 = RepresentOpAsLclLoad(hiOp1, value, &value->AsOp()->gtOp2);
    }

    LclVarDsc* loOp1Lcl = loOp1->AsLclLoad()->GetLcl();
    LclVarDsc* hiOp1Lcl = hiOp1->AsLclLoad()->GetLcl();

    Range().Unlink(loOp1);
    Range().Unlink(hiOp1);

    GenTree* rotateByHi = m_compiler->gtNewIconNode(count, TYP_INT);
    GenTree* rotateByLo = m_compiler->gtNewIconNode(count, TYP_INT);

    oper = oper == GT_ROL ? GT_LSH_HI : GT_RSH_LO;

    GenTree* hiCopy = m_compiler->gtNewLclLoad(hiOp1Lcl, TYP_INT);
    GenTree* loOp   = m_compiler->gtNewOperNode(GT_LONG, TYP_LONG, hiCopy, loOp1);
    loOp->SetContained();
    loResult = m_compiler->gtNewOperNode(oper, TYP_INT, loOp, rotateByLo);

    GenTree* loCopy = m_compiler->gtNewLclLoad(loOp1Lcl, TYP_INT);
    GenTree* hiOp   = m_compiler->gtNewOperNode(GT_LONG, TYP_LONG, loCopy, hiOp1);
    hiOp->SetContained();
    hiResult = m_compiler->gtNewOperNode(oper, TYP_INT, hiOp, rotateByHi);

    Range().InsertBefore(node, hiCopy, loOp1, loOp);
    Range().InsertBefore(node, rotateByLo, loResult);
    Range().InsertBefore(node, loCopy, hiOp1, hiOp);
    Range().InsertBefore(node, rotateByHi, hiResult);

    Range().Unlink(node);

    return FinalizeDecomposition(use, loResult, hiResult, hiResult);
}

// Decompose GT_MUL. These muls result in a mul instruction that returns its result in
// two registers like GT_CALLs do. Additionally, these muls are guaranteed to be in the
// form long = (long)int * (long)int. Therefore, to decompose these nodes, we convert
// them into GT_MUL_LONGs, undo the cast from int to long by stripping out the lo ops,
// and force them into the form var = mul, as we do for GT_CALLs.
// In codegen, we then produce a mul instruction that produces the result in edx:eax on
// x86 or in any two chosen by RA registers on ARM32.
// All other GT_MULs have been converted to helper calls in morph.cpp
GenTree* DecomposeLongs::DecomposeMul(LIR::Use& use)
{
    GenTreeOp* tree = use.Def()->AsOp();

    assert(tree->OperIs(GT_MUL));

    // We expect both operands to be extending casts. DecomposeSigned/UnsignedExtend
    // specifically ignores such casts when they are used by MULs.

    GenTreeUnOp* op1 = tree->GetOp(0)->AsUnOp();
    GenTreeUnOp* op2 = tree->GetOp(1)->AsUnOp();
    assert(op1->OperIs(GT_SXT, GT_UXT) && op1->TypeIs(TYP_LONG));
    assert(op2->OperIs(GT_SXT, GT_UXT) && op2->TypeIs(TYP_LONG));
    assert(op1->GetOper() == op2->GetOper());

    Range().Unlink(op1);
    Range().Unlink(op2);

    tree->SetOp(0, op1->GetOp(0));
    tree->SetOp(1, op2->GetOp(0));
    tree->SetOper(op1->OperIs(GT_UXT) ? GT_UMULL : GT_SMULL);

    return StoreMultiRegNodeToLcl(use);
}

// The only UREMs that make it to decompose are guaranteed to be
// an unsigned long mod with op2 which is a cast to long from a constant int whose value
// is between 2 and 0x3fffffff. All other GT_UMODs are morphed into helper calls.
// These UREMs will actually return an int value in RDX. In decompose, we make the lo
// operation a INT UREM, with op2 as the original lo half and op1 as a LONG.
// We make the hi part 0, so we end up with:
//
// UREM ( LONG (loOp1, hiOp1), loOp2 )
//
// With the expectation that we will generate:
//
// EDX = hiOp1
// EAX = loOp1
// reg = loOp2
// idiv reg
// EDX is the remainder, and result of UREM
// mov hiReg = 0
//
GenTree* DecomposeLongs::DecomposeUMod(LIR::Use& use)
{
    GenTreeOp* tree = use.Def()->AsOp();

    assert(tree->OperIs(GT_UREM));

    GenTreeOp* op1 = tree->GetOp(0)->AsOp();
    GenTreeOp* op2 = tree->GetOp(1)->AsOp();
    assert(op1->OperIs(GT_LONG));
    assert(op2->OperIs(GT_LONG));

    GenTree* loOp2 = op2->GetOp(0);
    GenTree* hiOp2 = op2->GetOp(1);

    assert((loOp2->AsIntCon()->GetValue() >= 2) && (loOp2->AsIntCon()->GetValue() <= 0x3fffffff));
    assert(hiOp2->AsIntCon()->GetValue() == 0);

    Range().Unlink(hiOp2);
    Range().Unlink(op2);

    GenTree* loResult = tree;
    loResult->AsOp()->SetOp(1, loOp2);
    loResult->SetType(TYP_INT);

    GenTree* hiResult = m_compiler->gtNewIconNode(0);

    Range().InsertAfter(loResult, hiResult);

    return FinalizeDecomposition(use, loResult, hiResult, hiResult);
}

#ifdef FEATURE_HW_INTRINSICS

GenTree* DecomposeLongs::DecomposeHWIntrinsic(LIR::Use& use)
{
    GenTreeHWIntrinsic* intrinsic = use.Def()->AsHWIntrinsic();

    switch (intrinsic->GetIntrinsic())
    {
        case NI_VEC_EXTRACT:
            return DecomposeVecExtract(use, intrinsic);
        default:
            unreached();
    }
}

GenTree* DecomposeLongs::DecomposeVecExtract(LIR::Use& use, GenTreeHWIntrinsic* node)
{
    assert(node == use.Def());
    assert(node->GetIntrinsic() == NI_VEC_EXTRACT);
    assert(node->TypeIs(TYP_LONG));
    assert(node->GetSimdBaseType() == TYP_LONG);

    GenTree* vec = node->GetOp(0);
    GenTree* idx = node->GetOp(1);

    assert(vec->TypeIs(TYP_SIMD16, TYP_SIMD32));
    assert(varActualTypeIsInt(idx->GetType()));

    if (GenTreeIntCon* idxCon = idx->IsIntCon())
    {
        idxCon->SetValue(idxCon->GetValue() * 2);

        if (vec->TypeIs(TYP_SIMD32))
        {
            GenTree* upper;

            if (idxCon->GetValue() >= 4)
            {
                idxCon->SetValue(idxCon->GetValue() - 4);
                GenTree* one = m_compiler->gtNewIconNode(1, TYP_INT);
                upper =
                    m_compiler->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_AVX2_ExtractVector128, TYP_LONG, 32, vec, one);
                Range().InsertAfter(vec, one, upper);
            }
            else
            {
                upper = m_compiler->gtNewSimdHWIntrinsicNode(TYP_SIMD16, NI_Vector256_GetLower, TYP_LONG, 32, vec);
                Range().InsertAfter(vec, upper);
            }

            node->SetOp(0, upper);
            vec = upper;
        }
    }
    else
    {
        GenTree* two = m_compiler->gtNewIconNode(2, TYP_INT);
        GenTree* mul = m_compiler->gtNewOperNode(GT_MUL, TYP_INT, idx, two);
        Range().InsertAfter(idx, two, mul);
        node->SetOp(1, mul);
        idx = RepresentOpAsLclLoad(mul, node, &node->GetUse(1).NodeRef());
    }

    vec = RepresentOpAsLclLoad(vec, node, &node->GetUse(0).NodeRef());
    Range().Unlink(vec);
    Range().Unlink(idx);

    GenTree* loResult = m_compiler->gtNewVecExtractNode(TYP_INT, vec, idx);
    Range().InsertBefore(node, vec, idx, loResult);

    if (GenTreeIntCon* idxCon = idx->IsIntCon())
    {
        idx = m_compiler->gtNewIconNode(idxCon->GetValue() + 1, TYP_INT);
    }
    else
    {
        idx          = m_compiler->gtNewLclLoad(idx->AsLclLoad()->GetLcl(), TYP_INT);
        GenTree* one = m_compiler->gtNewIconNode(1, TYP_INT);
        Range().InsertBefore(node, idx, one);
        idx = m_compiler->gtNewOperNode(GT_ADD, TYP_INT, idx, one);
    }

    vec = m_compiler->gtNewLclLoad(vec->AsLclLoad()->GetLcl(), vec->GetType());

    GenTree* hiResult = m_compiler->gtNewVecExtractNode(TYP_INT, vec, idx);
    Range().InsertBefore(node, vec, idx, hiResult);
    Range().Unlink(node);

    return FinalizeDecomposition(use, loResult, hiResult, hiResult);
}

#endif // FEATURE_HW_INTRINSICS

GenTree* DecomposeLongs::OptimizeTruncate(GenTreeUnOp* trunc, GenTree* nextNode)
{
    GenTreeOp* src = trunc->GetOp(0)->AsOp();

    assert(src->OperIs(GT_LONG));

    GenTree* loSrc = src->GetOp(0);
    GenTree* hiSrc = src->GetOp(1);

    JITDUMP("Optimizing TRUNC [%06u] from decomposed LONG [%06u]\n", trunc->GetID(), src->GetID());
    INDEBUG(GenTree* treeToDisplay = trunc);

    // TODO-CQ: we could go perform this removal transitively.
    // See also identical code in shift decomposition.
    if (hiSrc->HasAnySideEffect(GTF_ALL_EFFECT) || hiSrc->HasImplicitFlagsDef())
    {
        JITDUMP("The HI part of [%06u] has side effects, marking it unused\n", src->GetID());
        hiSrc->SetUnusedValue();
    }
    else
    {
        JITDUMPLIRNODE(hiSrc, "Removing the HI part of [%06u] and marking its operands unused:\n", src->GetID());
        Range().Remove(hiSrc, /* markOperandsUnused */ true);
    }

    JITDUMPLIRNODE(src, "Removing the LONG source:\n");
    Range().Unlink(src);

    LIR::Use use;
    if (Range().TryGetUse(trunc, &use))
    {
        use.SetDef(loSrc);
    }
    else
    {
        loSrc->SetUnusedValue();
    }

    if (nextNode == trunc)
    {
        nextNode = nextNode->gtNext;
    }

    INDEBUG(treeToDisplay = loSrc);
    JITDUMPLIRNODE(trunc, "Removing TRUNC:\n");

    Range().Unlink(trunc);

    JITDUMPLIRRANGE(Range(), treeToDisplay, "Final result:\n")

    return nextNode;
}

// Check if the user is a LCL_STORE, and if it isn't, store the node to a temp local.
// Then decompose the new store.
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

    if (!m_compiler->opts.EnregLocals() || m_compiler->lvaHaveManyLocals())
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

// Check is op already local, if not store it to temp.
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

void DecomposeLongs::PromoteLongVars()
{
    if (!m_compiler->opts.EnregLocals() || m_compiler->fgNoStructPromotion)
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
