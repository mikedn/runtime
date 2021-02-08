// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef _MSC_VER
#pragma hdrstop
#endif

//------------------------------------------------------------------------
// fgInline - expand inline candidates
//
// Returns:
//   phase status indicating if anything was modified
//
// Notes:
//   Inline candidates are identified during importation and candidate calls
//   must be top-level expressions. In input IR, the result of the call (if any)
//   is consumed elsewhere by a GT_RET_EXPR node.
//
//   For successful inlines, calls are replaced by a sequence of argument setup
//   instructions, the inlined method body, and return value cleanup. Note
//   Inlining may introduce new inline candidates. These are processed in a
//   depth-first fashion, as the inliner walks the IR in statement order.
//
//   After inline expansion in a statement, the statement tree
//   is walked to locate GT_RET_EXPR nodes. These are replaced by either
//   * the original call tree, if the inline failed
//   * the return value tree from the inlinee, if the inline succeeded
//
//   This replacement happens in preorder; on the postorder side of the same
//   tree walk, we look for opportunties to devirtualize or optimize now that
//   we know the context for the newly supplied return value tree.
//
//   Inline arguments may be directly substituted into the body of the inlinee
//   in some cases. See inlFetchInlineeArg.
//
PhaseStatus Compiler::fgInline()
{
    if (!opts.OptEnabled(CLFLG_INLINING))
    {
        return PhaseStatus::MODIFIED_NOTHING;
    }

#ifdef DEBUG
    if (verbose)
    {
        fgDispBasicBlocks(true);
    }

    fgPrintInlinedMethods = JitConfig.JitPrintInlinedMethods().contains(info.compMethodName, info.compClassName,
                                                                        &info.compMethodInfo->args);
#endif // DEBUG

    // Set the root inline context on all statements
    InlineContext* rootContext = m_inlineStrategy->GetRootContext();

    for (BasicBlock* block = fgFirstBB; block != nullptr; block = block->bbNext)
    {
        for (Statement* stmt : block->Statements())
        {
            stmt->SetInlineContext(rootContext);
        }
    }

    bool madeChanges = false;

    for (BasicBlock* block = fgFirstBB; block != nullptr; block = block->bbNext)
    {
        compCurBB = block;

        for (Statement* stmt : block->Statements())
        {
            INDEBUG(inlReportNonCandidates(stmt);)

            // The importer ensures that all inline candidates are statement roots.
            GenTree* expr = stmt->GetRootNode();

            if (GenTreeCall* call = expr->IsCall())
            {
                bool removeStmt = false;

                if (call->IsInlineCandidate())
                {
                    bool inlined = inlInlineCall(stmt, call);

                    removeStmt = inlined || (call->gtInlineCandidateInfo->retExprPlaceholder != nullptr);
                }
                else if (call->IsGuardedDevirtualizationCandidate())
                {
                    // TODO-MIKE-Cleanup: Shouldn't IndirectCallTransformer take care of this?!

                    removeStmt = (call->gtInlineCandidateInfo->retExprPlaceholder != nullptr);
                }

                if (removeStmt)
                {
                    fgRemoveStmt(block, stmt DEBUGARG(/*dumpStmt */ false));
                    madeChanges = true;
                    continue;
                }
            }

            // See if we need to replace some return value place holders.
            // Also, see if this replacement enables further devirtualization.
            //
            // Note we have both preorder and postorder callbacks here.
            //
            // The preorder callback is responsible for replacing GT_RET_EXPRs
            // with the appropriate expansion (call or inline result).
            // Replacement may introduce subtrees with GT_RET_EXPR and so
            // we rely on the preorder to recursively process those as well.
            //
            // On the way back up, the postorder callback then re-examines nodes for
            // possible further optimization, as the (now complete) GT_RET_EXPR
            // replacement may have enabled optimizations by providing more
            // specific types for trees or variables.
            fgWalkTree(stmt->GetRootNodePointer(), fgUpdateInlineReturnExpressionPlaceHolder, fgLateDevirtualization,
                       this);

            // COMMA(CALL, NOP) => CALL
            if (expr->OperIs(GT_COMMA) && expr->AsOp()->GetOp(0)->IsCall() && expr->AsOp()->GetOp(1)->OperIs(GT_NOP))
            {
                stmt->SetRootNode(expr->AsOp()->GetOp(0));
                madeChanges = true;
            }
        }
    }

#ifdef DEBUG
    inlDebugCheckInlineCandidates();
    fgVerifyHandlerTab();

    if (verbose || fgPrintInlinedMethods)
    {
        JITDUMP("**************** Inline Tree");
        printf("\n");
        m_inlineStrategy->Dump(verbose);
    }
#endif // DEBUG

    // TODO-MIKE-Fix: Change detection is not reliable due to the problem described in
    // fgUpdateInlineReturnExpressionPlaceHolder.
    madeChanges = true;

    return madeChanges ? PhaseStatus::MODIFIED_EVERYTHING : PhaseStatus::MODIFIED_NOTHING;
}

#ifdef DEBUG

// Some inlines may fail very early and never make it to candidate stage.
// Walk a statment tree and report such failures so that they appear in
// inline tree dumps.
//
void Compiler::inlReportNonCandidates(Statement* stmt)
{
    auto visitor = [](GenTree** use, fgWalkData* data) {
        if (GenTreeCall* call = (*use)->IsCall())
        {
            if (!call->IsInlineCandidate() && !call->IsGuardedDevirtualizationCandidate())
            {
                InlineObservation observation = call->gtInlineObservation;

                if (!InlIsValidObservation(observation))
                {
                    observation = InlineObservation::CALLSITE_NOT_CANDIDATE;
                }

                InlineResult result(data->compiler, call, nullptr, "inlNoteNonCandidates");
                result.NotePriorFailure(observation);
                result.SetReported();

                if (call->gtCallType == CT_USER_FUNC)
                {
                    data->compiler->m_inlineStrategy->NewFailure(static_cast<Statement*>(data->pCallbackData), result);
                }
            }
        }

        return WALK_CONTINUE;
    };

    fgWalkTreePre(stmt->GetRootNodePointer(), visitor, stmt);
}

#endif // DEBUG

#if FEATURE_MULTIREG_RET

GenTree* Compiler::inlGetStructAddress(GenTree* tree)
{
    switch (tree->GetOper())
    {
        case GT_BLK:
        case GT_OBJ:
        case GT_IND:
            return tree->AsIndir()->GetAddr();

        case GT_COMMA:
            tree->AsOp()->SetOp(1, inlGetStructAddress(tree->AsOp()->GetOp(1)));
            tree->SetType(TYP_BYREF);
            return tree;

        case GT_LCL_VAR:
        case GT_FIELD:
#ifdef FEATURE_SIMD
        case GT_SIMD:
#endif
#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
#endif
            // TODO-MIKE-Cleanup: Bleah, more ADDR(SIMD|HWINTRINSIC) nonsense...
            return gtNewOperNode(GT_ADDR, TYP_BYREF, tree);

        default:
            unreached();
    }
}

GenTree* Compiler::inlGetStructAsgDst(GenTree* dst, ClassLayout* layout)
{
    if (dst->OperIs(GT_LCL_VAR))
    {
        LclVarDsc* tmpLcl = lvaGetDesc(dst->AsLclVar());

        if (varTypeIsStruct(tmpLcl->GetType()) && !tmpLcl->IsImplicitByRefParam() && (tmpLcl->GetLayout() == layout))
        {
            dst->gtFlags |= GTF_DONT_CSE | GTF_VAR_DEF;
            return dst;
        }

        return gtNewObjNode(layout, gtNewOperNode(GT_ADDR, TYP_I_IMPL, dst));
    }

    GenTree* dstAddr = inlGetStructAddress(dst);

    if (dstAddr->OperIs(GT_ADDR))
    {
        GenTree* location = dstAddr->AsUnOp()->GetOp(0);

        if (location->OperIs(GT_LCL_VAR))
        {
            LclVarDsc* tmpLcl = lvaGetDesc(location->AsLclVar());

            if (varTypeIsStruct(tmpLcl->GetType()) && !tmpLcl->IsImplicitByRefParam() &&
                (tmpLcl->GetLayout() == layout))
            {
                dst->gtFlags |= GTF_DONT_CSE | GTF_VAR_DEF;
                return location;
            }
        }
    }

    GenTree* obj = gtNewObjNode(layout, dstAddr);
    obj->gtFlags |= GTF_DONT_CSE;
    return obj;
}

GenTree* Compiler::inlGetStructAsgSrc(GenTree* src, ClassLayout* layout)
{
    if (!src->OperIs(GT_LCL_VAR, GT_FIELD) && !src->OperIsSimdOrHWintrinsic())
    {
        GenTree* srcAddr = inlGetStructAddress(src);

        if (srcAddr->OperIs(GT_ADDR))
        {
            src = srcAddr->AsUnOp()->GetOp(0);
        }
        else
        {
            src = gtNewObjNode(layout, srcAddr);
        }
    }

    // TODO-MIKE-CQ: This should probably be removed, it's here only because
    // a previous implementation (gtNewBlkOpNode) was setting it. And it
    // probably blocks SIMD tree CSEing.
    src->gtFlags |= GTF_DONT_CSE;

    return src;
}

GenTree* Compiler::inlAssignStructInlineeToTemp(GenTree* src, ClassLayout* layout)
{
    assert(!src->OperIs(GT_RET_EXPR, GT_MKREFANY));

    unsigned tempLclNum = lvaGrabTemp(false DEBUGARG("RetBuf for struct inline return candidates."));
    lvaSetStruct(tempLclNum, layout, false);
    LclVarDsc* tempLcl = lvaGetDesc(tempLclNum);
    GenTree*   dst     = gtNewLclvNode(tempLclNum, tempLcl->GetType());

    // If we have a call, we'd like it to be: V00 = call(), but first check if
    // we have a ", , , call()" -- this is very defensive as we may never get
    // an inlinee that is made of commas. If the inlinee is not a call, then
    // we use a copy block to do the assignment.
    GenTree*   actualSrc = src;
    GenTreeOp* lastComma = nullptr;

    while (actualSrc->OperIs(GT_COMMA))
    {
        lastComma = actualSrc->AsOp();
        actualSrc = lastComma->GetOp(1);
    }

    GenTree* newAsg = nullptr;

    if (actualSrc->IsCall())
    {
        // When returning a multi-register value in a local var, make sure the variable is
        // marked as lvIsMultiRegRet, so it does not get promoted.
        if (actualSrc->AsCall()->HasMultiRegRetVal())
        {
            tempLcl->lvIsMultiRegRet = true;
        }

        // If inlinee was just a call, newAsg is v05 = call()
        newAsg = gtNewAssignNode(dst, actualSrc);

        // If inlinee was comma, but a deeper call, newAsg is (, , , v05 = call())
        if (lastComma != nullptr)
        {
            lastComma->SetOp(1, newAsg);
            newAsg = src;
        }
    }
    else
    {
        // Inlinee is not a call, so just create a copy block to the tmp.

        src = inlGetStructAsgSrc(src, layout);

        newAsg = gtNewAssignNode(dst, src);
        gtInitStructCopyAsg(newAsg->AsOp());
    }

    return gtNewOperNode(GT_COMMA, dst->GetType(), newAsg, gtNewLclvNode(tempLclNum, dst->GetType()));
}

void Compiler::inlAttachStructInlineeToAsg(GenTreeOp* asg, GenTree* src, ClassLayout* layout)
{
    assert(asg->OperIs(GT_ASG));

    GenTree* dst = asg->GetOp(0);

    if (src->IsCall())
    {
        if (dst->OperIs(GT_LCL_VAR))
        {
            // If it is a multireg return on x64/ux, the local variable should be marked as lvIsMultiRegRet
            if (src->AsCall()->HasMultiRegRetVal())
            {
                lvaGetDesc(dst->AsLclVar())->lvIsMultiRegRet = true;
            }

            return;
        }

        // Struct calls can only be assigned to locals, in all other cases we need to introduce a temp.
        src = inlAssignStructInlineeToTemp(src, layout);
    }

    dst = inlGetStructAsgDst(dst, layout);
    src = inlGetStructAsgSrc(src, layout);

    asg->SetOp(0, dst);
    asg->SetOp(1, src);
    asg->gtFlags = (GTF_ASG | src->gtFlags | dst->gtFlags) & GTF_ALL_EFFECT;

    gtInitStructCopyAsg(asg);
}

#endif // FEATURE_MULTIREG_RET

//------------------------------------------------------------------------
// fgUpdateInlineReturnExpressionPlaceHolder: callback to replace the
// inline return expression placeholder.
//
// Arguments:
//    use - the use of a tree node
//    data - context data for the tree walk
//
// Returns:
//    fgWalkResult indicating the walk should continue; that
//    is we wish to fully explore the tree.
//
// Notes:
//    Looks for GT_RET_EXPR nodes that arose from tree splitting done
//    during importation for inline candidates, and replaces them.
//
//    If the return type is a struct type and we're on a platform
//    where structs can be returned in multiple registers, ensure the
//    call has a suitable parent.
//
//    If the original call type and the substitution type are different
//    the functions makes necessary updates. It could happen if there was
//    an implicit conversion in the inlinee body.
//
Compiler::fgWalkResult Compiler::fgUpdateInlineReturnExpressionPlaceHolder(GenTree** use, fgWalkData* data)
{
    Compiler* comp = data->compiler;
    GenTree*  tree = *use;

    // All the operations here and in the corresponding postorder
    // callback (fgLateDevirtualization) are triggered by GT_CALL or
    // GT_RET_EXPR trees, and these (should) have the call side
    // effect flag.
    //
    // So bail out for any trees that don't have this flag.

    // TODO-MIKE-Fix: A change in master results in RET_EXPR appearing in trees without ancestors
    // having GTF_CALL set. It's not clear if the change in master is broken and hidden by the now
    // removed GT_PUTARG_TYPE or if there's a bad interaction with a change in mjit.
    // It could also be a pre-existing problem - RET_EXPR being replaced too late:
    //  - inlRecordInlineeArg itself skips RET_EXPR and sees a side effect free expression
    //  - the RET_EXPR is still there in the IR and is yet to be replaced
    //  - being side effect free the return expression is used directly, ignoring an arg temp
    //  - this may happen recursively and another RET_EXPR sneaks in, in place of the temp

    // if ((tree->gtFlags & GTF_CALL) == 0)
    //{
    //    return WALK_SKIP_SUBTREES;
    //}

    if (GenTreeRetExpr* retExpr = tree->IsRetExpr())
    {
#ifdef DEBUG
        if (comp->verbose)
        {
            printf("\nReplacing the return expression placeholder ");
            printTreeID(retExpr);

            if (data->parent != nullptr)
            {
                printf(" in\n");
                comp->gtDispTree(data->parent);
            }
        }
#endif

        GenTree*     value = nullptr;
        GenTreeCall* call  = nullptr;

        for (; retExpr != nullptr; retExpr = value->IsRetExpr())
        {
            // TODO-MIKE-Cleanup: RET_EXPR chain handling is dubious. A chain of RET_EXPR is
            // handled by gtRetExprVal and that gives us the block flags of the last RET_EXPR
            // in the chain. And we get the call from the first RET_EXPR in the chanin.
            // But if we run into foldable nodes then we get the block flags and call from
            // other RET_EXPRs. The call it unlikely to matter, it's only needed to get the
            // struct layout and struct aren't folded. But it's not clear what happens with
            // the block flags, should gtRetExprVal actually OR the flags?
            //
            // Can we even get a chain of RET_EXPR? It may be obvious that the answer is yes,
            // when we inline a method that just calls another method. But due to the way
            // RET_EXPR are replaced it seems like the RET_EXPR associated with the inner
            // call should have been already replaced when the outer RET_EXPR is encountered.
            // But the replacement is actually done too late - for an inline candidate tree
            // the replacement is actually done after inlining.

            call = retExpr->GetCall();

            uint64_t bbFlags = 0;
            value            = retExpr->gtRetExprVal(&bbFlags);
            comp->compCurBB->bbFlags |= (bbFlags & BBF_SPLIT_GAINED);

            // Try to fold the return value (e.g. returns of constant bools or small integers
            // will have widening casts). This folding may uncover more RET_EXPRs, so we loop
            // around until we've got something distinct.

            value = comp->gtFoldExpr(value);

            if (retExpr->TypeIs(TYP_BYREF) && !value->TypeIs(TYP_BYREF) && value->OperIs(GT_IND))
            {
                // An RVA static may have been reinterpreted as byref.
                assert(value->TypeIs(TYP_I_IMPL));
                JITDUMP("Updating type of the return GT_IND expression to TYP_BYREF\n");

                value->SetType(TYP_BYREF);
            }
        }

        JITDUMPTREE(value, "with inline return expression\n");

        tree = *use = value;

#if FEATURE_MULTIREG_RET
        // If an inline was rejected and the call returns a struct, we may
        // have deferred some work when importing call for cases where the
        // struct is returned in register(s).
        //
        // See the bail-out clauses in impCanonicalizeMultiRegCall for inline
        // candidates.
        //
        // Do the deferred work now.

        // TODO-MIKE-Cleanup: This seems to do more than impCanonicalizeMultiRegCall.
        // That one simply ensures that multi-reg calls are spilled to a local and
        // that lvIsMultiRegRet is set to true. But this code doesn't even bother to
        // check if the return expression is still a call...

        if (varTypeIsStruct(call->GetType()) && (call->GetRegCount() > 1))
        {
            // Is this a type that is returned in multiple registers
            // or a via a primitve type that is larger than the struct type?
            // if so we need to force into into a form we accept.
            // i.e. LclVar = call()

            // See assert below, we only look one level above for an asg parent.
            if (data->parent->OperIs(GT_ASG))
            {
                // Either lhs is a call V05 = call(); or lhs is addr, and asg becomes a copyBlk.
                comp->inlAttachStructInlineeToAsg(data->parent->AsOp(), tree, call->GetRetLayout());
            }
            else
            {
                // Just assign the inlinee to a variable to keep it simple.
                tree = *use = comp->inlAssignStructInlineeToTemp(tree, call->GetRetLayout());
            }
        }
#endif
    }

#if FEATURE_MULTIREG_RET && defined(DEBUG)
    // Make sure we don't have a tree like so: V05 = (, , , retExpr);
    // Since we only look one level above for the parent for '=' and
    // do not check if there is a series of COMMAs. See above.
    // Importer and FlowGraph will not generate such a tree, so just
    // leaving an assert in here. This can be fixed by looking ahead
    // when we visit GT_ASG similar to inlAttachStructInlineeToAsg.

    if (tree->OperIs(GT_ASG))
    {
        GenTree* value = tree->AsOp()->GetOp(1);

        if (value->OperIs(GT_COMMA))
        {
            value = value->SkipComma();

            bool isMultiRegCall = varTypeIsStruct(value->GetType()) && value->IsRetExpr() &&
                                  (value->AsRetExpr()->GetRetExpr() == nullptr) &&
                                  value->AsRetExpr()->GetCall()->HasMultiRegRetVal();

            noway_assert(!isMultiRegCall);
        }
    }
#endif

    return WALK_CONTINUE;
}

//------------------------------------------------------------------------
// fgLateDevirtualization: re-examine calls after inlining to see if we
//   can do more devirtualization
//
// Arguments:
//    pTree -- pointer to tree to examine for updates
//    data  -- context data for the tree walk
//
// Returns:
//    fgWalkResult indicating the walk should continue; that
//    is we wish to fully explore the tree.
//
// Notes:
//    We used to check this opportunistically in the preorder callback for
//    calls where the `obj` was fed by a return, but we now re-examine
//    all calls.
//
//    Late devirtualization (and eventually, perhaps, other type-driven
//    opts like cast optimization) can happen now because inlining or other
//    optimizations may have provided more accurate types than we saw when
//    first importing the trees.
//
//    It would be nice to screen candidate sites based on the likelihood
//    that something has changed. Otherwise we'll waste some time retrying
//    an optimization that will just fail again.

Compiler::fgWalkResult Compiler::fgLateDevirtualization(GenTree** pTree, fgWalkData* data)
{
    GenTree*  tree   = *pTree;
    GenTree*  parent = data->parent;
    Compiler* comp   = data->compiler;

    // In some (rare) cases the parent node of tree will be smashed to a NOP during
    // the preorder by fgAttachStructToInlineeArg.
    //
    // jit\Methodical\VT\callconv\_il_reljumper3 for x64 linux
    //
    // If so, just bail out here.
    if (tree == nullptr)
    {
        assert((parent != nullptr) && parent->OperGet() == GT_NOP);
        return WALK_CONTINUE;
    }

    if (tree->OperGet() == GT_CALL)
    {
        GenTreeCall* call          = tree->AsCall();
        bool         tryLateDevirt = call->IsVirtual() && (call->gtCallType == CT_USER_FUNC);

#ifdef DEBUG
        tryLateDevirt = tryLateDevirt && (JitConfig.JitEnableLateDevirtualization() == 1);
#endif // DEBUG

        if (tryLateDevirt)
        {
#ifdef DEBUG
            if (comp->verbose)
            {
                printf("**** Late devirt opportunity\n");
                comp->gtDispTree(call);
            }
#endif // DEBUG

            CORINFO_METHOD_HANDLE  method                 = call->gtCallMethHnd;
            unsigned               methodFlags            = 0;
            CORINFO_CONTEXT_HANDLE context                = nullptr;
            const bool             isLateDevirtualization = true;
            bool explicitTailCall = (call->AsCall()->gtCallMoreFlags & GTF_CALL_M_EXPLICIT_TAILCALL) != 0;
            comp->impDevirtualizeCall(call, &method, &methodFlags, &context, nullptr, isLateDevirtualization,
                                      explicitTailCall);
        }
    }
    else if (tree->OperGet() == GT_ASG)
    {
        // If we're assigning to a ref typed local that has one definition,
        // we may be able to sharpen the type for the local.
        GenTree* lhs = tree->gtGetOp1()->gtEffectiveVal();

        if ((lhs->OperGet() == GT_LCL_VAR) && (lhs->TypeGet() == TYP_REF))
        {
            const unsigned lclNum = lhs->AsLclVarCommon()->GetLclNum();
            LclVarDsc*     lcl    = comp->lvaGetDesc(lclNum);

            if (lcl->lvSingleDef)
            {
                GenTree*             rhs       = tree->gtGetOp2();
                bool                 isExact   = false;
                bool                 isNonNull = false;
                CORINFO_CLASS_HANDLE newClass  = comp->gtGetClassHandle(rhs, &isExact, &isNonNull);

                if (newClass != NO_CLASS_HANDLE)
                {
                    comp->lvaUpdateClass(lclNum, newClass, isExact);
                }
            }
        }
    }
    else if (tree->OperGet() == GT_JTRUE)
    {
        // See if this jtrue is now foldable.
        BasicBlock* block    = comp->compCurBB;
        GenTree*    condTree = tree->AsOp()->gtOp1;
        assert(tree == block->lastStmt()->GetRootNode());

        if (condTree->OperGet() == GT_CNS_INT)
        {
            JITDUMP(" ... found foldable jtrue at [%06u] in BB%02u\n", dspTreeID(tree), block->bbNum);
            noway_assert((block->bbNext->countOfInEdges() > 0) && (block->bbJumpDest->countOfInEdges() > 0));

            // We have a constant operand, and should have the all clear to optimize.
            // Update side effects on the tree, assert there aren't any, and bash to nop.
            comp->gtUpdateNodeSideEffects(tree);
            assert((tree->gtFlags & GTF_SIDE_EFFECT) == 0);
            tree->gtBashToNOP();

            BasicBlock* bTaken    = nullptr;
            BasicBlock* bNotTaken = nullptr;

            if (condTree->AsIntCon()->gtIconVal != 0)
            {
                block->bbJumpKind = BBJ_ALWAYS;
                bTaken            = block->bbJumpDest;
                bNotTaken         = block->bbNext;
            }
            else
            {
                block->bbJumpKind = BBJ_NONE;
                bTaken            = block->bbNext;
                bNotTaken         = block->bbJumpDest;
            }

            comp->fgRemoveRefPred(bNotTaken, block);

            // If that was the last ref, a subsequent flow-opt pass
            // will clean up the now-unreachable bNotTaken, and any
            // other transitively unreachable blocks.
            if (bNotTaken->bbRefs == 0)
            {
                JITDUMP("... it looks like BB%02u is now unreachable!\n", bNotTaken->bbNum);
            }
        }
    }
    else
    {
        GenTree* foldedTree = comp->gtFoldExpr(tree);
        *pTree              = foldedTree;
    }

    return WALK_CONTINUE;
}

#ifdef DEBUG

// Check that there are no more inline candidates or return expression
// placeholders left in the method.
//
void Compiler::inlDebugCheckInlineCandidates()
{
    auto visitor = [](GenTree** use, fgWalkData* data) {
        GenTree* node = *use;

        if (GenTreeCall* call = node->IsCall())
        {
            assert(!call->IsInlineCandidate());
        }
        else
        {
            assert(!node->IsRetExpr());
        }

        return WALK_CONTINUE;
    };

    for (BasicBlock* block = fgFirstBB; block != nullptr; block = block->bbNext)
    {
        for (Statement* stmt : block->Statements())
        {
            fgWalkTreePre(stmt->GetRootNodePointer(), visitor);
        }
    }
}

#endif // DEBUG

bool Compiler::inlInlineCall(Statement* stmt, GenTreeCall* call)
{
    assert(call->IsInlineCandidate());

    InlineResult result(this, call, stmt, "inlInlineCall");

    if (lvaCount >= MAX_LV_NUM_COUNT_FOR_INLINING)
    {
        // For now, attributing this to call site, though it's really
        // more of a budget issue (lvaCount currently includes all
        // caller and prospective callee locals). We still might be
        // able to inline other callees into this caller, or inline
        // this callee in other callers.

        // TODO-MIKE-CQ: This is kind of bogus. The inlinee may need no new locals,
        // this simply prevents inlining of trivial methods into large methods.

        result.NoteFatal(InlineObservation::CALLSITE_TOO_MANY_LOCALS);
        return false;
    }

    if (call->IsVirtual())
    {
        // TODO-MIKE-Cleanup: Why would we even reach here if the call is virtual?!
        // Maybe due to GDV but if that's the case then the indirect call transformer
        // should have cleared the candidate status.

        result.NoteFatal(InlineObservation::CALLSITE_IS_VIRTUAL);
        return false;
    }

    // Re-check this because guarded devirtualization may allow these through.
    if (gtIsRecursiveCall(call) && call->IsImplicitTailCall())
    {
        result.NoteFatal(InlineObservation::CALLSITE_IMPLICIT_REC_TAIL_CALL);
        return false;
    }

    // impMarkInlineCandidate is expected not to mark tail prefixed calls as inline candidates.
    noway_assert(!call->IsTailPrefixedCall());

    JITDUMPTREE(call, "Expanding inline candidate " FMT_TREEID " in " FMT_BB ":\n%s", call->GetID(), compCurBB->bbNum,
                call->IsImplicitTailCall() ? "Note: candidate is implicit tail call\n" : "");

    m_inlineStrategy->NoteAttempt(&result);

    unsigned initialLvaCount = lvaCount;

    inlInvokeInlineeCompiler(stmt, call, &result);

    assert(result.IsDecided());

    if (result.IsFailure())
    {
        memset(lvaTable + initialLvaCount, 0, (static_cast<size_t>(lvaCount) - initialLvaCount) * sizeof(*lvaTable));
        for (unsigned i = initialLvaCount; i < lvaCount; i++)
        {
            new (&lvaTable[i]) LclVarDsc();
        }
        lvaCount = initialLvaCount;

        // Before we do any cleanup, create a failing InlineContext to
        // capture details of the inlining attempt.
        INDEBUG(m_inlineStrategy->NewFailure(stmt, result);)

        // Clear the inline candidate flag so we can ensure later we tried
        // inlining all candidates.
        call->ClearInlineCandidate();
    }

    return result.IsSuccess();
}

void Compiler::inlInvokeInlineeCompiler(Statement* stmt, GenTreeCall* call, InlineResult* inlineResult)
{
    fgMorphStmt = stmt;

    InlineInfo inlineInfo;
    memset(&inlineInfo, 0, sizeof(inlineInfo));

    inlineInfo.InlinerCompiler     = this;
    inlineInfo.iciBlock            = compCurBB;
    inlineInfo.iciStmt             = stmt;
    inlineInfo.iciCall             = call;
    inlineInfo.fncHandle           = call->GetMethodHandle();
    inlineInfo.inlineCandidateInfo = call->GetInlineCandidateInfo();
    inlineInfo.inlineResult        = inlineResult;
    inlineInfo.profileScaleState   = InlineInfo::ProfileScaleState::UNDETERMINED;

    unsigned inlineDepth = inlCheckInlineDepthAndRecursion(&inlineInfo);

    if (inlineResult->IsFailure())
    {
        JITDUMP("Recursive or deep inline recursion detected. Will not expand this INLINECANDIDATE \n");
        return;
    }

    bool success = eeRunWithErrorTrap<InlineInfo>(
        [](InlineInfo* inlineInfo) {
            Compiler* inlinerCompiler = inlineInfo->InlinerCompiler;

            if (!inlinerCompiler->inlRecordInlineeArgsAndLocals(inlineInfo))
            {
                return;
            }

            inlineInfo->tokenLookupContextHandle = inlineInfo->inlineCandidateInfo->exactContextHnd;

            JITLOG_THIS(inlinerCompiler,
                        (LL_INFO100000, "INLINER: inlineInfo.tokenLookupContextHandle for %s set to 0x%p:\n",
                         inlinerCompiler->eeGetMethodFullName(inlineInfo->fncHandle),
                         inlinerCompiler->dspPtr(inlineInfo->tokenLookupContextHandle)));

            JitFlags compileFlags = *inlinerCompiler->opts.jitFlags;
            // The following flags are lost when inlining.
            // (This is checked in Compiler::compInitOptions().)
            compileFlags.Clear(JitFlags::JIT_FLAG_BBINSTR);
            compileFlags.Clear(JitFlags::JIT_FLAG_PROF_ENTERLEAVE);
            compileFlags.Clear(JitFlags::JIT_FLAG_DEBUG_EnC);
            compileFlags.Clear(JitFlags::JIT_FLAG_DEBUG_INFO);
            compileFlags.Clear(JitFlags::JIT_FLAG_REVERSE_PINVOKE);
            compileFlags.Clear(JitFlags::JIT_FLAG_TRACK_TRANSITIONS);

            compileFlags.Set(JitFlags::JIT_FLAG_SKIP_VERIFICATION);

            JITDUMP("\nInvoking compiler for the inlinee method %s :\n",
                    inlinerCompiler->eeGetMethodFullName(inlineInfo->fncHandle));

            int result = jitNativeCode(inlineInfo->fncHandle, inlineInfo->inlineCandidateInfo->methInfo.scope,
                                       inlinerCompiler->info.compCompHnd, &inlineInfo->inlineCandidateInfo->methInfo,
                                       nullptr, nullptr, &compileFlags, inlineInfo);

            if ((result != CORJIT_OK) && !inlineInfo->inlineResult->IsFailure())
            {
                // If we haven't yet determined why this inline fails, use
                // a catch-all something bad happened observation.

                inlineInfo->inlineResult->NoteFatal(InlineObservation::CALLSITE_COMPILATION_FAILURE);
            }
        },
        &inlineInfo);

    if (!success)
    {
        JITDUMP("\nInlining failed due to an exception during invoking the compiler for the inlinee method %s.\n",
                eeGetMethodFullName(inlineInfo.fncHandle));

        // If we haven't yet determined why this inline fails, use
        // a catch-all something bad happened observation.
        if (!inlineResult->IsFailure())
        {
            inlineResult->NoteFatal(InlineObservation::CALLSITE_COMPILATION_ERROR);
        }
    }

    if (inlineResult->IsFailure())
    {
        return;
    }

    // If there is non-NULL return, but we haven't set the pInlineInfo->retExpr,
    // That means we haven't imported any BB that contains CEE_RET opcode.
    // (This could happen for example for a BBJ_THROW block fall through a BBJ_RETURN block which
    // causes the BBJ_RETURN block not to be imported at all.)
    // Fail the inlining attempt
    if ((call->GetRetSigType() != TYP_VOID) && (inlineInfo.retExpr == nullptr))
    {
        JITDUMP("\nInlining failed because pInlineInfo->retExpr is not set in the inlinee method %s.\n",
                eeGetMethodFullName(inlineInfo.fncHandle));

        inlineResult->NoteFatal(InlineObservation::CALLEE_LACKS_RETURN);

        return;
    }

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // The inlining attempt cannot be failed starting from this point.
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    inlInsertInlineeCode(&inlineInfo);

    JITDUMP("\nSuccessfully inlined %s (%d IL bytes) (depth %d) [%s]\n"
            "--------------------------------------------------------------------------------------------\n",
            eeGetMethodFullName(inlineInfo.fncHandle), inlineInfo.inlineCandidateInfo->methInfo.ILCodeSize, inlineDepth,
            inlineResult->ReasonString());

    INDEBUG(impInlinedCodeSize += inlineInfo.inlineCandidateInfo->methInfo.ILCodeSize;)

    inlineResult->NoteSuccess();
}

// Compute depth of the candidate, and check for recursion.
// We generally disallow recursive inlines by policy. However, they are
// supported by the underlying machinery.
// Likewise the depth limit is a policy consideration, and serves mostly
// as a safeguard to prevent runaway inlining of small methods.
//
unsigned Compiler::inlCheckInlineDepthAndRecursion(const InlineInfo* inlineInfo)
{
    InlineContext* inlineContext = inlineInfo->iciStmt->GetInlineContext();
    assert(inlineContext != nullptr);

    int depth = 0;

    for (; inlineContext != nullptr; inlineContext = inlineContext->GetParent())
    {
        depth++;

        assert(inlineContext->GetCode() != nullptr);

        if (inlineContext->GetCode() == inlineInfo->inlineCandidateInfo->methInfo.ILCode)
        {
            inlineInfo->inlineResult->NoteFatal(InlineObservation::CALLSITE_IS_RECURSIVE);
            break;
        }

        if (depth > InlineStrategy::IMPLEMENTATION_MAX_INLINE_DEPTH)
        {
            break;
        }
    }

    inlineInfo->inlineResult->NoteInt(InlineObservation::CALLSITE_DEPTH, depth);
    return depth;
}

bool Compiler::inlRecordInlineeArgsAndLocals(InlineInfo* inlineInfo)
{
    assert(!compIsForInlining());

    if (!inlRecordInlineeArgs(inlineInfo))
    {
        return false;
    }

    if (!inlRecordInlineeLocals(inlineInfo))
    {
        return false;
    }

#ifdef FEATURE_SIMD
    if (varTypeIsSIMD(inlineInfo->iciCall->GetRetSigType()))
    {
        inlineInfo->hasSIMDTypeArgLocalOrReturn = true;
    }
#endif

    memset(inlineInfo->lclTmpNum, -1, sizeof(inlineInfo->lclTmpNum));

    return true;
}

bool Compiler::inlRecordInlineeArgs(InlineInfo* inlineInfo)
{
    CORINFO_SIG_INFO& argsSig = inlineInfo->inlineCandidateInfo->methInfo.args;

    inlineInfo->argCnt = argsSig.totalILArgs();

    GenTreeCall*      call    = inlineInfo->iciCall;
    GenTreeCall::Use* thisArg = call->gtCallThisArg;
    InlArgInfo*       argInfo = inlineInfo->inlArgInfo;
    unsigned          argNum  = 0;

    assert((argsSig.hasThis()) == (thisArg != nullptr));

    if (thisArg != nullptr)
    {
        argInfo[0].argIsThis = true;

        if (!inlRecordInlineeArg(inlineInfo, thisArg->GetNode(), argNum))
        {
            return false;
        }

        argNum++;
    }

    unsigned typeCtxtArgNum = UINT32_MAX;

    if ((argsSig.callConv & CORINFO_CALLCONV_PARAMTYPE) != 0)
    {
#if USER_ARGS_COME_LAST
        typeCtxtArgNum = (thisArg != nullptr) ? 1 : 0;
#else
        typeCtxtArgNum = methInfo.args.totalILArgs();
#endif
    }

    for (GenTreeCall::Use& use : call->Args())
    {
        if (call->HasRetBufArg() && (&use == call->gtCallArgs))
        {
            continue;
        }

        if (argNum == typeCtxtArgNum)
        {
            inlineInfo->typeContextArg = typeCtxtArgNum;
            typeCtxtArgNum             = UINT32_MAX;
            continue;
        }

        if (!inlRecordInlineeArg(inlineInfo, use.GetNode(), argNum))
        {
            return false;
        }

        argNum++;
    }

    assert(argNum == inlineInfo->argCnt);

    // Init the types of the arguments and make sure the types
    // from the trees match the types in the signature

    InlLclVarInfo* lclVarInfo = inlineInfo->lclVarInfo;
#ifdef FEATURE_SIMD
    bool foundSIMDType = inlineInfo->hasSIMDTypeArgLocalOrReturn;
#endif

    if (thisArg != nullptr)
    {
        var_types paramType;
        typeInfo  paramTypeInfo;

        CORINFO_CLASS_HANDLE methodClass = inlineInfo->inlineCandidateInfo->clsHandle;
        GenTree*             argNode     = thisArg->GetNode();

        if ((inlineInfo->inlineCandidateInfo->clsAttr & CORINFO_FLG_VALUECLASS) == 0)
        {
            if (argNode->GetType() != TYP_REF)
            {
                // The argument cannot be coerced to REF.
                inlineInfo->inlineResult->NoteFatal(InlineObservation::CALLSITE_ARG_NO_BASH_TO_REF);
                return false;
            }

            paramType     = TYP_REF;
            paramTypeInfo = typeInfo(TI_REF, methodClass);
        }
        else
        {
            // A native pointer can be passed as "this" to a method of a struct.
            assert(argNode->TypeIs(TYP_BYREF, TYP_I_IMPL));

            paramType = TYP_BYREF;

            if (info.compCompHnd->getTypeForPrimitiveValueClass(methodClass) == CORINFO_TYPE_UNDEF)
            {
                // TODO-MIKE-Cleanup: Like LDLOCA import, this generates incorrect type information,
                // TI_STRUCT without marking it byref. And then if the arg is a native pointer the
                // type info is changed to I_IMPL?! This makes no sense.

                paramTypeInfo = typeInfo(TI_STRUCT, methodClass);

                if (argNode->TypeIs(TYP_I_IMPL))
                {
                    paramTypeInfo = typeInfo(TI_I_IMPL);
                }

#ifdef FEATURE_SIMD
                if (!foundSIMDType && isSIMDorHWSIMDClass(methodClass))
                {
                    foundSIMDType = true;
                }
#endif
            }
        }

        lclVarInfo[0].lclType        = paramType;
        lclVarInfo[0].lclVerTypeInfo = paramTypeInfo;
    }

    CORINFO_ARG_LIST_HANDLE paramHandle = argsSig.args;

    for (unsigned i = (thisArg ? 1 : 0); i < argNum; i++, paramHandle = info.compCompHnd->getArgNext(paramHandle))
    {
        CORINFO_CLASS_HANDLE paramClass;
        CorInfoType          paramCorType = strip(info.compCompHnd->getArgType(&argsSig, paramHandle, &paramClass));
        var_types            paramType    = JITtype2varType(paramCorType);
        typeInfo             paramTypeInfo;

        if (paramType == TYP_REF)
        {
            paramTypeInfo = typeInfo(TI_REF, info.compCompHnd->getArgClass(&argsSig, paramHandle));
        }
        else if (paramType == TYP_BYREF)
        {
            // Don't generate typeInfo for byref, it's not needed and requires extra VM calls.
        }
        else if (paramType == TYP_STRUCT)
        {
#ifdef FEATURE_SIMD
            if (isSIMDorHWSIMDClass(paramClass))
            {
                // If this is a SIMD class (i.e. in the SIMD assembly), then we will consider that we've
                // found a SIMD type, even if this may not be a type we recognize (the assumption is that
                // it is likely to use a SIMD type, and therefore we want to increase the inlining multiplier).
                foundSIMDType = true;
                paramType     = impNormStructType(paramClass);
            }
#endif

            paramTypeInfo = typeInfo(TI_STRUCT, paramClass);
        }
        else if (paramClass != NO_CLASS_HANDLE)
        {
            assert(info.compCompHnd->isValueClass(paramClass));
            assert(info.compCompHnd->getTypeForPrimitiveValueClass(paramClass) == CORINFO_TYPE_UNDEF);

            // This is a "normed type" - a struct that contains a single primitive type field.
            // See lvaInitVarDsc.
            paramTypeInfo = typeInfo(TI_STRUCT, paramClass);
        }
        else if (paramCorType <= CORINFO_TYPE_DOUBLE)
        {
            // TODO-MIKE-Cleanup: This shouldn't be necessary but fgFindJumpTargets's normed type
            // check is broken - it uses typeInfo::IsValueClass(), which returns true for any
            // primitive type, and thus detects any primitive type local as being normed type.

            paramTypeInfo = typeInfo(JITtype2tiType(paramCorType));
        }

        lclVarInfo[i].lclType        = paramType;
        lclVarInfo[i].lclVerTypeInfo = paramTypeInfo;

        // Does the tree type match the signature type?

        GenTree* argNode = argInfo[i].argNode;

        if ((paramType == argNode->GetType()) || (paramType == varActualType(argNode->GetType())))
        {
            continue;
        }

        if (varTypeIsSmall(paramType))
        {
            // LCL_VARs associate with small int locals may have type INT so the above
            // check isn't sufficient and we may end up adding an unnecessary cast.

            if (argNode->OperIs(GT_LCL_VAR) && (paramType == lvaGetDesc(argNode->AsLclVar())->GetType()))
            {
                continue;
            }

            argNode = gtNewCastNode(TYP_INT, argNode, false, paramType);

            if (argInfo[i].argIsInvariant)
            {
                argNode = gtFoldExprConst(argNode);
                assert(argNode->OperIsConst());
            }

            argInfo[i].argNode     = argNode;
            argInfo[i].argIsLclVar = false;

            continue;
        }

        if (paramType == TYP_BYREF)
        {
            // Native int args can be coerced to BYREF.

            if (!argNode->TypeIs(TYP_I_IMPL))
            {
                inlineInfo->inlineResult->NoteFatal(InlineObservation::CALLSITE_ARG_TYPES_INCOMPATIBLE);
                return false;
            }

            lclVarInfo[i].lclVerTypeInfo = typeInfo(TI_I_IMPL);

            continue;
        }

        if (argNode->TypeIs(TYP_BYREF))
        {
            // BYREF args cannot be coerced to native int but the JIT ignores the spec.
            // But this is done only if the arg represents a local address which is BYREF
            // in spec but in reality is just a native pointer.

            if (paramType != TYP_I_IMPL)
            {
                inlineInfo->inlineResult->NoteFatal(InlineObservation::CALLSITE_ARG_TYPES_INCOMPATIBLE);
                return false;
            }

            if (argNode->IsLocalAddrExpr() == nullptr)
            {
                inlineInfo->inlineResult->NoteFatal(InlineObservation::CALLSITE_ARG_NO_BASH_TO_INT);
                return false;
            }

            assert(argNode->OperIs(GT_ADDR, GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR));
            argNode->SetType(TYP_I_IMPL);
            lclVarInfo[i].lclVerTypeInfo = typeInfo(TI_I_IMPL);

            continue;
        }

        assert(!"importer's a mess");
        inlineInfo->inlineResult->NoteFatal(InlineObservation::CALLSITE_ARG_TYPES_INCOMPATIBLE);
        return false;
    }

#ifdef FEATURE_SIMD
    inlineInfo->hasSIMDTypeArgLocalOrReturn |= foundSIMDType;
#endif

    return true;
}

bool Compiler::inlRecordInlineeArg(InlineInfo* inlineInfo, GenTree* argNode, unsigned argNum)
{
    InlArgInfo& argInfo = inlineInfo->inlArgInfo[argNum];

    // Save the original tree, might be a RET_EXPR and we need to keep it.
    argInfo.argNode = argNode;

    argNode = argNode->gtRetExprVal();

    if (argNode->OperIs(GT_MKREFANY))
    {
        inlineInfo->inlineResult->NoteFatal(InlineObservation::CALLSITE_ARG_IS_MKREFANY);
        return false;
    }

    if (argInfo.argIsThis && argNode->IsIntegralConst(0))
    {
        inlineInfo->inlineResult->NoteFatal(InlineObservation::CALLSITE_ARG_HAS_NULL_THIS);
        return false;
    }

    if (argNode->OperIsConst())
    {
        argInfo.argIsInvariant = true;
    }
    else if (argNode->OperIs(GT_LCL_VAR))
    {
        LclVarDsc* lcl = lvaGetDesc(argNode->AsLclVar());

        if (!lcl->lvHasLdAddrOp && !lcl->lvAddrExposed)
        {
            argInfo.argIsLclVar = true;
        }
        else
        {
            argInfo.argHasGlobRef = true;
        }
    }
    else if (GenTreeLclVar* addrLclVar = impIsAddressInLocal(argNode))
    {
        argInfo.argIsInvariant = true;

        if (varTypeIsStruct(addrLclVar->GetType()))
        {
#ifdef FEATURE_SIMD
            if (lvaGetDesc(addrLclVar)->lvSIMDType)
            {
                inlineInfo->hasSIMDTypeArgLocalOrReturn = true;
            }
#endif
        }
    }
    else
    {
        if ((argNode->gtFlags & GTF_ALL_EFFECT) != 0)
        {
            argInfo.argHasGlobRef = (argNode->gtFlags & GTF_GLOB_REF) != 0;
            argInfo.argHasSideEff = (argNode->gtFlags & (GTF_ALL_EFFECT & ~GTF_GLOB_REF)) != 0;
        }

        if (!argInfo.argHasGlobRef)
        {
            // Normally address exposed locals already have GTF_GLOB_REF but we haven't yet
            // determined which locals are address exposed so we'll have to settle for less,
            // if the expression contains any address taken locals then it's treated as if
            // it has GTF_GLOB_REF.

            argInfo.argHasGlobRef = gtHasAddressTakenLocals(argNode);
        }
    }

#ifdef DEBUG
    if (verbose)
    {
        if (argInfo.argIsThis)
        {
            printf("this argument: ");
        }
        else
        {
            printf("\nArgument #%u: ", argNum);
        }
        if (argInfo.argIsLclVar)
        {
            printf("is a local var");
        }
        if (argInfo.argIsInvariant)
        {
            printf("is invariant");
        }
        if (argInfo.argHasGlobRef)
        {
            printf("has global refs");
        }
        if (argInfo.argHasSideEff)
        {
            printf("has side effects");
        }
        if (argInfo.argHasLdargaOp)
        {
            printf("has its address taken");
        }
        if (argInfo.argHasStargOp)
        {
            printf("is stored to");
        }

        printf("\n");
        gtDispTree(argNode);
        printf("\n");
    }
#endif // DEBUG

    return true;
}

bool Compiler::inlRecordInlineeLocals(InlineInfo* pInlineInfo)
{
    CORINFO_METHOD_INFO* methInfo     = &pInlineInfo->inlineCandidateInfo->methInfo;
    InlineResult*        inlineResult = pInlineInfo->inlineResult;
    InlLclVarInfo*       lclVarInfo   = pInlineInfo->lclVarInfo;
    unsigned             argCnt       = pInlineInfo->argCnt;

    CORINFO_ARG_LIST_HANDLE argLst        = methInfo->locals.args;
    bool                    foundSIMDType = false;

    for (unsigned i = 0; i < methInfo->locals.numArgs; i++, argLst = info.compCompHnd->getArgNext(argLst))
    {
        CORINFO_CLASS_HANDLE lclClass;
        CorInfoTypeWithMod   lclCorType = info.compCompHnd->getArgType(&methInfo->locals, argLst, &lclClass);
        var_types            lclType    = JITtype2varType(strip(lclCorType));
        typeInfo             lclTypeInfo;

        if (varTypeIsGC(lclType))
        {
            if (lclType == TYP_REF)
            {
                lclTypeInfo = typeInfo(TI_REF, info.compCompHnd->getArgClass(&methInfo->locals, argLst));
            }

            if ((lclCorType & CORINFO_TYPE_MOD_PINNED) != 0)
            {
                // Pinned locals may cause inlines to fail.
                inlineResult->Note(InlineObservation::CALLEE_HAS_PINNED_LOCALS);
                if (inlineResult->IsFailure())
                {
                    return false;
                }

                JITDUMP("Inlinee local #%02u is pinned\n", i);

                lclVarInfo[i + argCnt].lclIsPinned = true;
            }

            pInlineInfo->numberOfGcRefLocals++;
        }
        else if (lclType == TYP_STRUCT)
        {
            if ((info.compCompHnd->getClassAttribs(lclClass) & CORINFO_FLG_CONTAINS_GC_PTR) != 0)
            {
                // If this local is a struct type with GC fields, inform the inliner.
                // It may choose to bail out on the inline.

                inlineResult->Note(InlineObservation::CALLEE_HAS_GC_STRUCT);
                if (inlineResult->IsFailure())
                {
                    return false;
                }

                // Do further notification in the case where the call site is rare; some policies do
                // not track the relative hotness of call sites for "always" inline cases.
                if (pInlineInfo->iciBlock->isRunRarely())
                {
                    inlineResult->Note(InlineObservation::CALLSITE_RARE_GC_STRUCT);
                    if (inlineResult->IsFailure())
                    {
                        return false;
                    }
                }
            }
#ifdef FEATURE_SIMD
            else if (isSIMDorHWSIMDClass(lclClass))
            {
                foundSIMDType = true;
                lclType       = impNormStructType(lclClass);
            }
#endif

            lclTypeInfo = typeInfo(TI_STRUCT, lclClass);
        }
        else if (lclClass != NO_CLASS_HANDLE)
        {
            assert(info.compCompHnd->isValueClass(lclClass));
            assert(info.compCompHnd->getTypeForPrimitiveValueClass(lclClass) == CORINFO_TYPE_UNDEF);

            // This is a "normed type" - a struct that contains a single primitive type field.
            // See lvaInitVarDsc.
            lclTypeInfo = typeInfo(TI_STRUCT, lclClass);
        }
        else if (strip(lclCorType) <= CORINFO_TYPE_DOUBLE)
        {
            // TODO-MIKE-Cleanup: This shouldn't be necessary but fgFindJumpTargets's normed type
            // check is broken - it uses typeInfo::IsValueClass(), which returns true for any
            // primitive type, and thus detects any primitive type local as being normed type.

            lclTypeInfo = typeInfo(JITtype2tiType(strip(lclCorType)));
        }

        lclVarInfo[i + argCnt].lclType        = lclType;
        lclVarInfo[i + argCnt].lclVerTypeInfo = lclTypeInfo;
        lclVarInfo[i + argCnt].lclHasLdlocaOp = false;
    }

#ifdef FEATURE_SIMD
    pInlineInfo->hasSIMDTypeArgLocalOrReturn |= foundSIMDType;
#endif

    return true;
}

//------------------------------------------------------------------------
// impInlineFetchLocal: get a local var that represents an inlinee local
//
// Arguments:
//    argNum -- number of the inlinee local
//    reason -- debug string describing purpose of the local var
//
// Returns:
//    Number of the local to use
//
// Notes:
//    This method is invoked only for locals actually used in the
//    inlinee body.
//
//    Allocates a new temp if necessary, and copies key properties
//    over from the inlinee local var info.

unsigned Compiler::impInlineFetchLocal(unsigned lclNum DEBUGARG(const char* reason))
{
    assert(compIsForInlining());

    unsigned tmpNum = impInlineInfo->lclTmpNum[lclNum];

    if (tmpNum == BAD_VAR_NUM)
    {
        const InlLclVarInfo& inlineeLocal = impInlineInfo->lclVarInfo[lclNum + impInlineInfo->argCnt];
        const var_types      lclTyp       = inlineeLocal.lclType;

        // The lifetime of this local might span multiple BBs.
        // So it is a long lifetime local.
        impInlineInfo->lclTmpNum[lclNum] = tmpNum = lvaGrabTemp(false DEBUGARG(reason));

        // Copy over key info
        lvaTable[tmpNum].lvHasLdAddrOp          = inlineeLocal.lclHasLdlocaOp;
        lvaTable[tmpNum].lvPinned               = inlineeLocal.lclIsPinned;
        lvaTable[tmpNum].lvHasILStoreOp         = inlineeLocal.lclHasStlocOp;
        lvaTable[tmpNum].lvHasMultipleILStoreOp = inlineeLocal.lclHasMultipleStlocOp;

        if (varTypeIsStruct(lclTyp))
        {
            lvaSetStruct(tmpNum, inlineeLocal.lclVerTypeInfo.GetClassHandle(), true /* unsafe value cls check */);
        }
        else
        {
            lvaTable[tmpNum].SetType(lclTyp);

            // Copy over class handle for ref types. Note this may be a
            // shared type -- someday perhaps we can get the exact
            // signature and pass in a more precise type.
            if (lclTyp == TYP_REF)
            {
                assert(lvaTable[tmpNum].lvSingleDef == 0);

                lvaTable[tmpNum].lvSingleDef = !inlineeLocal.lclHasMultipleStlocOp && !inlineeLocal.lclHasLdlocaOp;
                if (lvaTable[tmpNum].lvSingleDef)
                {
                    JITDUMP("Marked V%02u as a single def temp\n", tmpNum);
                }

                lvaSetClass(tmpNum, inlineeLocal.lclVerTypeInfo.GetClassHandleForObjRef());
            }
            else if (inlineeLocal.lclVerTypeInfo.IsType(TI_STRUCT))
            {
                // This is a "normed type", we need to set lclVerTypeInfo to preserve the struct handle.
                lvaTable[tmpNum].lvImpTypeInfo = inlineeLocal.lclVerTypeInfo;
            }
        }

#ifdef DEBUG
        // Sanity check that we're properly prepared for gc ref locals.
        if (varTypeIsGC(lclTyp))
        {
            // Since there are gc locals we should have seen them earlier
            // and if there was a return value, set up the spill temp.
            assert(impInlineInfo->HasGcRefLocals());
            assert((info.compRetType == TYP_VOID) || (lvaInlineeReturnSpillTemp != BAD_VAR_NUM));
        }
        else
        {
            // Make sure all pinned locals count as gc refs.
            assert(!inlineeLocal.lclIsPinned);
        }
#endif // DEBUG
    }

    return tmpNum;
}

GenTree* Compiler::inlFetchInlineeArg(unsigned argNum, InlArgInfo* inlArgInfo, InlLclVarInfo* lclVarInfo)
{
    InlArgInfo& argInfo = inlArgInfo[argNum];
    var_types   argType = lclVarInfo[argNum].lclType;

    GenTree* argNode = argInfo.argNode->gtRetExprVal();

    if (argInfo.argIsInvariant && !argInfo.argHasLdargaOp && !argInfo.argHasStargOp)
    {
        // Directly substitute constants or addresses of locals
        //
        // Clone the constant. Note that we cannot directly use
        // argNode in the trees even if !argInfo.argIsUsed as this
        // would introduce aliasing between inlArgInfo[].argNode and
        // impInlineExpr. Then gtFoldExpr() could change it, causing
        // further references to the argument working off of the
        // bashed copy.

        argInfo.argTmpNum = BAD_VAR_NUM;

        argNode = gtCloneExpr(argNode);

        // We may need to retype to ensure we match the callee's view of the type.
        // Otherwise callee-pass throughs of arguments can create return type
        // mismatches that block inlining.
        //
        // Note argument type mismatches that prevent inlining should
        // have been caught in inlRecordInlineeArgsAndLocals.

        if (argNode->GetType() != argType)
        {
            argNode->SetType(varActualType(argType));
        }

        return argNode;
    }

    if (argInfo.argIsLclVar && !argInfo.argHasLdargaOp && !argInfo.argHasStargOp)
    {
        assert(!argInfo.argHasGlobRef);

        // Directly substitute unaliased caller locals for args that cannot be modified
        //
        // Use the caller-supplied node if this is the first use.

        argInfo.argTmpNum = argNode->AsLclVar()->GetLclNum();

        // Use an equivalent copy if this is the second or subsequent
        // use, or if we need to retype.
        //
        // Note argument type mismatches that prevent inlining should
        // have been caught in inlRecordInlineeArgsAndLocals.

        if (argInfo.argIsUsed || (argNode->GetType() != argType))
        {
            if (!lvaGetDesc(argInfo.argTmpNum)->lvNormalizeOnLoad())
            {
                argType = varActualType(argType);
            }

            argNode = gtNewLclvNode(argInfo.argTmpNum, argType);
        }

        argInfo.argIsUsed = true;

        return argNode;
    }

    if (argInfo.argHasTmp)
    {
        // We already allocated a temp for this argument, use it.

        assert(argInfo.argIsUsed);
        assert(argInfo.argTmpNum < lvaCount);

        argNode = gtNewLclvNode(argInfo.argTmpNum, varActualType(argType));

        // This is the second or later use of the this argument,
        // so we have to use the temp (instead of the actual arg).
        argInfo.argSingleUse = nullptr;

        return argNode;
    }

    assert(!argInfo.argIsUsed);
    argInfo.argIsUsed = true;

    // Argument is a complex expression - it must be evaluated into a temp.

    unsigned   tmpLclNum = lvaGrabTemp(true DEBUGARG("inlinee arg"));
    LclVarDsc* tmpLcl    = lvaGetDesc(tmpLclNum);

    if (argInfo.argHasLdargaOp)
    {
        tmpLcl->lvHasLdAddrOp = 1;
    }

    const InlLclVarInfo& lclInfo = lclVarInfo[argNum];

    if (varTypeIsStruct(argType))
    {
        lvaSetStruct(tmpLclNum, lclInfo.lclVerTypeInfo.GetClassHandle(), /* unsafe value cls check */ true);
    }
    else
    {
        tmpLcl->SetType(argType);

        if (argType == TYP_REF)
        {
            if (!argInfo.argHasLdargaOp && !argInfo.argHasStargOp)
            {
                // If the arg can't be modified in the method body, use the type of the value,
                // if known. Otherwise, use the declared type.

                assert(tmpLcl->lvSingleDef == 0);
                tmpLcl->lvSingleDef = 1;

                JITDUMP("Marked V%02u as a single def temp\n", tmpLclNum);

                lvaSetClass(tmpLclNum, argInfo.argNode, lclInfo.lclVerTypeInfo.GetClassHandleForObjRef());
            }
            else
            {
                // Arg might be modified, use the declared type of the argument.

                lvaSetClass(tmpLclNum, lclInfo.lclVerTypeInfo.GetClassHandleForObjRef());
            }
        }
        else if (lclInfo.lclVerTypeInfo.IsType(TI_STRUCT))
        {
            // This is a "normed type", we need to set lclVerTypeInfo to preserve the struct handle.

            tmpLcl->lvImpTypeInfo = lclInfo.lclVerTypeInfo;
        }
    }

    argInfo.argHasTmp = true;
    argInfo.argTmpNum = tmpLclNum;

    // If we require strict exception order, then arguments must
    // be evaluated in sequence before the body of the inlined method.
    // So we need to evaluate them to a temp.
    // Also, if arguments have global or local references, we need to
    // evaluate them to a temp before the inlined body as the
    // inlined body may be modifying the global ref.
    //
    // TODO-1stClassStructs: We currently do not reuse an existing lclVar
    // if it is a struct, because it requires some additional handling.

    if (varTypeIsStruct(argType) || argInfo.argHasSideEff || argInfo.argHasGlobRef)
    {
        argNode = gtNewLclvNode(tmpLclNum, varActualType(argType));
    }
    else
    {
        // Allocate a large LCL_VAR node so we can replace it with any
        // other node if it turns out to be single use.

        argNode = gtNewLclLNode(tmpLclNum, varActualType(argType));

        // Record argNode as the very first use of this argument.
        // If there are no further uses of the arg, we may be
        // able to use the actual arg node instead of the temp.
        // If we do see any further uses, we will clear this.

        argInfo.argSingleUse = argNode;
    }

    return argNode;
}

//------------------------------------------------------------------------
// inlInsertInlineeCode: incorporate statements for an inline into the
// root method.
//
// Arguments:
//    inlineInfo -- info for the inline
//
// Notes:
//    The inlining attempt cannot be failed once this method is called.
//
//    Adds all inlinee statements, plus any glue statements needed
//    either before or after the inlined call.
//
//    Updates flow graph and assigns weights to inlinee
//    blocks. Currently does not attempt to read IBC data for the
//    inlinee.
//
//    Updates relevant root method status flags (eg optMethodFlags) to
//    include information from the inlinee.
//
//    Marks newly added statements with an appropriate inline context.

void Compiler::inlInsertInlineeCode(InlineInfo* pInlineInfo)
{
    GenTreeCall* call = pInlineInfo->iciCall;

    JITDUMP("\n---- Statements (and blocks) added due to the inlining of call " FMT_TREEID " ----\n", call->GetID());

    Statement*  callStmt  = pInlineInfo->iciStmt;
    BasicBlock* callBlock = pInlineInfo->iciBlock;

    noway_assert(callBlock->GetFirstStatement() != nullptr);
    noway_assert(callStmt->GetRootNode() == call);

    // Create a new inline context and mark the inlined statements with it
    InlineContext* inlineContext = m_inlineStrategy->NewSuccess(pInlineInfo);

    for (BasicBlock* block = InlineeCompiler->fgFirstBB; block != nullptr; block = block->bbNext)
    {
        for (Statement* stmt : block->Statements())
        {
            stmt->SetInlineContext(inlineContext);
        }
    }

    Statement* stmtAfter = fgInlinePrependStatements(pInlineInfo);

    JITDUMP("\nInlinee method body:\n");

    if ((InlineeCompiler->fgBBcount == 1) && (InlineeCompiler->fgFirstBB->bbJumpKind == BBJ_RETURN))
    {
        // Inlinee contains just one return block. So just insert its statement into the inliner block.

        if (InlineeCompiler->fgFirstBB->GetFirstStatement() == nullptr)
        {
            JITDUMP("\tInlinee method has no statements.\n");
        }
        else
        {
#ifdef DEBUG
            if (verbose)
            {
                for (Statement* stmt : InlineeCompiler->fgFirstBB->Statements())
                {
                    gtDispStmt(stmt);
                }
            }
#endif

            stmtAfter = fgInsertStmtListAfter(callBlock, stmtAfter, InlineeCompiler->fgFirstBB->GetFirstStatement());
        }

        // Copy inlinee bbFlags to caller bbFlags.
        const uint64_t inlineeBlockFlags = InlineeCompiler->fgFirstBB->bbFlags;

        noway_assert((inlineeBlockFlags & BBF_HAS_JMP) == 0);
        noway_assert((inlineeBlockFlags & BBF_KEEP_BBJ_ALWAYS) == 0);

        // Todo: we may want to exclude other flags here.
        callBlock->bbFlags |= (inlineeBlockFlags & ~BBF_RUN_RARELY);

        // Append statements to null out gc ref locals, if necessary.
        inlNullOutInlineeGCLocals(pInlineInfo, callBlock, stmtAfter);
    }
    else
    {
        BasicBlock* topBlock    = callBlock;
        BasicBlock* bottomBlock = inlSplitInlinerBlock(topBlock, stmtAfter);

        inlInsertInlineeBlocks(pInlineInfo, topBlock, bottomBlock, callStmt->GetILOffsetX());

#ifdef DEBUG
        if (verbose)
        {
            fgDispBasicBlocks(InlineeCompiler->fgFirstBB, InlineeCompiler->fgLastBB, true);
        }
#endif

        // Append statements to null out gc ref locals, if necessary.
        inlNullOutInlineeGCLocals(pInlineInfo, bottomBlock, nullptr);
    }

    inlPropagateInlineeCompilerState();

    // Record the return expression of non-void methods in the RET_EXPR node.
    if (pInlineInfo->iciCall->GetRetSigType() != TYP_VOID)
    {
        noway_assert(pInlineInfo->retExpr != nullptr);

        JITDUMPTREE(pInlineInfo->retExpr, "Return expression is:\n", call->GetID());

        call->gtInlineCandidateInfo->retExprPlaceholder->SetRetExpr(pInlineInfo->retExpr, pInlineInfo->retBB->bbFlags);
    }
}

BasicBlock* Compiler::inlSplitInlinerBlock(BasicBlock* topBlock, Statement* stmtAfter)
{
    BasicBlock* bottomBlock = fgNewBBafter(topBlock->bbJumpKind, topBlock, true);
    bottomBlock->bbRefs     = 1;
    bottomBlock->bbJumpDest = topBlock->bbJumpDest;
    bottomBlock->inheritWeight(topBlock);

    topBlock->bbJumpKind = BBJ_NONE;

    // Update block flags
    {
        const unsigned __int64 originalFlags = topBlock->bbFlags;
        noway_assert((originalFlags & BBF_SPLIT_NONEXIST) == 0);
        topBlock->bbFlags &= ~(BBF_SPLIT_LOST);
        bottomBlock->bbFlags |= originalFlags & BBF_SPLIT_GAINED;
    }

    // Split statements between topBlock and bottomBlock.
    // First figure out bottomBlock_Begin
    Statement* bottomBlock_Begin;
    bottomBlock_Begin = stmtAfter->GetNextStmt();

    if (topBlock->bbStmtList == nullptr)
    {
        // topBlock is empty before the split.
        // In this case, both topBlock and bottomBlock should be empty
        noway_assert(bottomBlock_Begin == nullptr);
        topBlock->bbStmtList    = nullptr;
        bottomBlock->bbStmtList = nullptr;
    }
    else if (topBlock->bbStmtList == bottomBlock_Begin)
    {
        noway_assert(bottomBlock_Begin != nullptr);

        // topBlock contains at least one statement before the split.
        // And the split is before the first statement.
        // In this case, topBlock should be empty, and everything else should be moved to the bottomBlock.
        bottomBlock->bbStmtList = topBlock->bbStmtList;
        topBlock->bbStmtList    = nullptr;
    }
    else if (bottomBlock_Begin == nullptr)
    {
        noway_assert(topBlock->bbStmtList != nullptr);

        // topBlock contains at least one statement before the split.
        // And the split is at the end of the topBlock.
        // In this case, everything should be kept in the topBlock, and the bottomBlock should be empty

        bottomBlock->bbStmtList = nullptr;
    }
    else
    {
        noway_assert(topBlock->bbStmtList != nullptr);
        noway_assert(bottomBlock_Begin != nullptr);

        // This is the normal case where both blocks should contain at least one statement.
        Statement* topBlock_Begin = topBlock->firstStmt();
        noway_assert(topBlock_Begin != nullptr);
        Statement* topBlock_End = bottomBlock_Begin->GetPrevStmt();
        noway_assert(topBlock_End != nullptr);
        Statement* bottomBlock_End = topBlock->lastStmt();
        noway_assert(bottomBlock_End != nullptr);

        // Break the linkage between 2 blocks.
        topBlock_End->SetNextStmt(nullptr);

        // Fix up all the pointers.
        topBlock->bbStmtList = topBlock_Begin;
        topBlock->bbStmtList->SetPrevStmt(topBlock_End);

        bottomBlock->bbStmtList = bottomBlock_Begin;
        bottomBlock->bbStmtList->SetPrevStmt(bottomBlock_End);
    }

    return bottomBlock;
}

// Insert the inlinee basic blocks into the inliner's flow graph.
//
void Compiler::inlInsertInlineeBlocks(InlineInfo* inlineInfo,
                                      BasicBlock* topBlock,
                                      BasicBlock* bottomBlock,
                                      IL_OFFSETX  ilOffset)
{
    assert((InlineeCompiler->fgBBcount > 1) || (InlineeCompiler->fgFirstBB->bbJumpKind != BBJ_RETURN));

    bool inheritWeight = true; // The firstBB does inherit the weight from the call block

    for (BasicBlock* block = InlineeCompiler->fgFirstBB; block != nullptr; block = block->bbNext)
    {
        // Methods that contain exception handling are never inlined.
        noway_assert(!block->hasTryIndex());
        noway_assert(!block->hasHndIndex());

        block->copyEHRegion(topBlock);
        block->bbFlags |= topBlock->bbFlags & BBF_BACKWARD_JUMP;

        if (ilOffset != BAD_IL_OFFSET)
        {
            block->bbCodeOffs    = jitGetILoffs(ilOffset);
            block->bbCodeOffsEnd = block->bbCodeOffs + 1; // TODO: is code size of 1 some magic number for inlining?
        }
        else
        {
            block->bbCodeOffs    = 0; // TODO: why not BAD_IL_OFFSET?
            block->bbCodeOffsEnd = 0;
            block->bbFlags |= BBF_INTERNAL;
        }

        if (block->bbJumpKind == BBJ_RETURN)
        {
            inheritWeight = true; // A return block does inherit the weight from the call block

            noway_assert((block->bbFlags & BBF_HAS_JMP) == 0);

            if (block->bbNext != nullptr)
            {
                JITDUMP("Convert return block " FMT_BB " to jump to the bottom block " FMT_BB "\n", block->bbNum,
                        bottomBlock->bbNum);

                block->bbJumpKind = BBJ_ALWAYS;
                block->bbJumpDest = bottomBlock;
            }
            else
            {
                JITDUMP("Convert return block " FMT_BB " to fall through to the bottom block " FMT_BB "\n",
                        block->bbNum, bottomBlock->bbNum);

                block->bbJumpKind = BBJ_NONE;
            }
        }

        // Update profile weight for callee blocks, if we didn't do it already.
        if (inlineInfo->profileScaleState != InlineInfo::ProfileScaleState::KNOWN)
        {
            if (inheritWeight)
            {
                block->inheritWeight(topBlock);
                inheritWeight = false;
            }
            else
            {
                block->modifyBBWeight(topBlock->bbWeight / 2);
            }
        }
    }

    // Insert inlinee's blocks into inliner's block list.
    topBlock->setNext(InlineeCompiler->fgFirstBB);
    InlineeCompiler->fgLastBB->setNext(bottomBlock);
    fgBBcount += InlineeCompiler->fgBBcount;
}

void Compiler::inlPropagateInlineeCompilerState()
{
    compLongUsed |= InlineeCompiler->compLongUsed;
    compFloatingPointUsed |= InlineeCompiler->compFloatingPointUsed;
    compLocallocUsed |= InlineeCompiler->compLocallocUsed;
    compLocallocOptimized |= InlineeCompiler->compLocallocOptimized;
    compQmarkUsed |= InlineeCompiler->compQmarkUsed;
    compNeedsGSSecurityCookie |= InlineeCompiler->compNeedsGSSecurityCookie;
    compGSReorderStackLayout |= InlineeCompiler->compGSReorderStackLayout;
    compHasBackwardJump |= InlineeCompiler->compHasBackwardJump;

    lvaGenericsContextInUse |= InlineeCompiler->lvaGenericsContextInUse;

#ifdef FEATURE_SIMD
    if (InlineeCompiler->usesSIMDTypes())
    {
        setUsesSIMDTypes(true);
    }
#endif // FEATURE_SIMD

    // Update unmanaged call details
    info.compUnmanagedCallCountWithGCTransition += InlineeCompiler->info.compUnmanagedCallCountWithGCTransition;

    // Update optMethodFlags
    INDEBUG(unsigned optMethodFlagsBefore = optMethodFlags;)

    optMethodFlags |= InlineeCompiler->optMethodFlags;

#ifdef DEBUG
    if (optMethodFlags != optMethodFlagsBefore)
    {
        JITDUMP("INLINER: Updating optMethodFlags --  root:%0x callee:%0x new:%0x\n", optMethodFlagsBefore,
                InlineeCompiler->optMethodFlags, optMethodFlags);
    }
#endif
}

//------------------------------------------------------------------------
// fgInlinePrependStatements: prepend statements needed to match up
// caller and inlined callee
//
// Arguments:
//    inlineInfo -- info for the inline
//
// Return Value:
//    The last statement that was added, or the original call if no
//    statements were added.
//
// Notes:
//    Statements prepended may include the following:
//    * This pointer null check
//    * Class initialization
//    * Zeroing of must-init locals in the callee
//    * Passing of call arguments via temps
//
//    Newly added statements are placed just after the original call
//    and are are given the same inline context as the call any calls
//    added here will appear to have been part of the immediate caller.

Statement* Compiler::fgInlinePrependStatements(InlineInfo* inlineInfo)
{
    BasicBlock*  block        = inlineInfo->iciBlock;
    Statement*   callStmt     = inlineInfo->iciStmt;
    IL_OFFSETX   callILOffset = callStmt->GetILOffsetX();
    Statement*   postStmt     = callStmt->GetNextStmt();
    Statement*   afterStmt    = callStmt; // afterStmt is the place where the new statements should be inserted after.
    Statement*   newStmt      = nullptr;
    GenTreeCall* call         = inlineInfo->iciCall->AsCall();

    noway_assert(call->gtOper == GT_CALL);

#ifdef DEBUG
    if (0 && verbose)
    {
        printf("\nfgInlinePrependStatements for iciCall= ");
        printTreeID(call);
        printf(":\n");
    }
#endif

    // Prepend statements for any initialization / side effects

    InlArgInfo*    inlArgInfo = inlineInfo->inlArgInfo;
    InlLclVarInfo* lclVarInfo = inlineInfo->lclVarInfo;

    // Create the null check statement (but not appending it to the statement list yet) for the 'this' pointer if
    // necessary.
    // The NULL check should be done after "argument setup statements".
    // The only reason we move it here is for calling "inlFetchInlineeArg(0,..." to reserve a temp
    // for the "this" pointer.
    // Note: Here we no longer do the optimization that was done by thisDereferencedFirst in the old inliner.
    // However the assetionProp logic will remove any unecessary null checks that we may have added
    //
    GenTree* nullcheck = nullptr;

    if (call->gtFlags & GTF_CALL_NULLCHECK && !inlineInfo->thisDereferencedFirst)
    {
        // Call inlFetchInlineeArg to "reserve" a temp for the "this" pointer.
        GenTree* thisOp = inlFetchInlineeArg(0, inlArgInfo, lclVarInfo);
        if (fgAddrCouldBeNull(thisOp))
        {
            nullcheck = gtNewNullCheck(thisOp, block);
            // The NULL-check statement will be inserted to the statement list after those statements
            // that assign arguments to temps and before the actual body of the inlinee method.
        }
    }

    afterStmt = inlInitInlineeArgs(inlineInfo, afterStmt);

    // Add the CCTOR check if asked for.
    // Note: We no longer do the optimization that is done before by staticAccessedFirstUsingHelper in the old inliner.
    //       Therefore we might prepend redundant call to HELPER.CORINFO_HELP_GETSHARED_NONGCSTATIC_BASE
    //       before the inlined method body, even if a static field of this type was accessed in the inlinee
    //       using a helper before any other observable side-effect.

    if (inlineInfo->inlineCandidateInfo->initClassResult & CORINFO_INITCLASS_USE_HELPER)
    {
        CORINFO_CLASS_HANDLE exactClass = eeGetClassFromContext(inlineInfo->inlineCandidateInfo->exactContextHnd);

        GenTree* tree = fgGetSharedCCtor(exactClass);
        newStmt       = gtNewStmt(tree, callILOffset);
        fgInsertStmtAfter(block, afterStmt, newStmt);
        afterStmt = newStmt;
    }

    // Insert the nullcheck statement now.
    if (nullcheck)
    {
        newStmt = gtNewStmt(nullcheck, callILOffset);
        fgInsertStmtAfter(block, afterStmt, newStmt);
        afterStmt = newStmt;
    }

    afterStmt = inlInitInlineeLocals(inlineInfo, block, afterStmt, callILOffset);

    // Update any newly added statements with the appropriate context.
    InlineContext* context = callStmt->GetInlineContext();
    assert(context != nullptr);
    for (Statement* addedStmt = callStmt->GetNextStmt(); addedStmt != postStmt; addedStmt = addedStmt->GetNextStmt())
    {
        assert(addedStmt->GetInlineContext() == nullptr);
        addedStmt->SetInlineContext(context);
    }

    return afterStmt;
}

Statement* Compiler::inlInitInlineeArgs(InlineInfo* inlineInfo, Statement* afterStmt)
{
    JITDUMP("\nInit inlinee args:\n");
    JITDUMP("-----------------------------------------------------------------------------------------------------\n");

    if (inlineInfo->argCnt == 0)
    {
        JITDUMP("\tInlinee has no args.\n");
        return afterStmt;
    }

    for (unsigned argNum = 0; argNum < inlineInfo->argCnt; argNum++)
    {
        const InlArgInfo& argInfo = inlineInfo->inlArgInfo[argNum];

        GenTree* argNode = argInfo.argNode;
        uint64_t bbFlags = 0;
        argNode          = argNode->gtRetExprVal(&bbFlags);

        // MKREFANY args currently fail inlining.
        assert(!argNode->OperIs(GT_MKREFANY));

        if (argInfo.argHasTmp)
        {
            noway_assert(argInfo.argIsUsed);

            // argSingleUse is non-NULL iff the argument's value was
            // referenced exactly once by the original IL. This offers an
            // opportunity to avoid an intermediate temp and just insert
            // the original argument tree.
            //
            // However, if the temp node has been cloned somewhere while
            // importing (e.g. when handling isinst or dup), or if the IL
            // took the address of the argument, then argSingleUse will
            // be set (because the value was only explicitly retrieved
            // once) but the optimization cannot be applied.

            GenTree* argSingleUseNode = argInfo.argSingleUse;

            if ((argSingleUseNode != nullptr) && ((argSingleUseNode->gtFlags & GTF_VAR_CLONED) == 0) &&
                !argInfo.argHasLdargaOp && !argInfo.argHasStargOp)
            {
                // Change the temp in-place to the actual argument.

                // We currently do not support this for struct arguments, so it must not be a GT_OBJ.
                assert(!argNode->OperIs(GT_OBJ));

                argSingleUseNode->ReplaceWith(argNode, this);
                continue;
            }

            // We're going to assign the argument value to the
            // temp we use for it in the inline body.
            const unsigned  tmpNum  = argInfo.argTmpNum;
            const var_types argType = inlineInfo->lclVarInfo[argNum].lclType;

            // Create the temp assignment for this argument

            GenTree* asg;

            if (!varTypeIsStruct(argType))
            {
                asg = gtNewTempAssign(tmpNum, argNode);
            }
            else
            {
                CORINFO_CLASS_HANDLE structHnd = gtGetStructHandleIfPresent(argNode);
                noway_assert((structHnd != NO_CLASS_HANDLE) || (argType != TYP_STRUCT));

                // TODO-MIKE-Cleanup: Workaround for the type mismatch issue described in
                // lvaSetStruct - the temp may have type A<SomeRefClass> and argNode may
                // have type A<Canon>. In such a case, impAssignStructPtr wraps the dest
                // temp in an OBJ that then cannot be removed and causes CQ issues.
                // To avoid that, temporarily change the type of the temp to the argNode's
                // type.
                //
                // In general the JIT doesn't care if the 2 sides of a struct assignment
                // have the same type so perhaps we can just change impAssignStructPtr to
                // simply not add the OBJ. But for now it's safer to do this here because
                // we're 99.99% sure that the types are really the same. If they're not
                // then the IL is likely invalid (pushed a struct with a different type
                // than the parameter type).

                LclVarDsc*   tmpLcl        = lvaGetDesc(tmpNum);
                ClassLayout* tmpLayout     = tmpLcl->GetLayout();
                bool         restoreLayout = false;

                if ((argType == TYP_STRUCT) && (structHnd != tmpLayout->GetClassHandle()))
                {
                    assert(info.compCompHnd->getClassSize(structHnd) == tmpLayout->GetSize());

                    tmpLcl->SetLayout(typGetObjLayout(structHnd));
                    restoreLayout = true;
                }

                assert(!argNode->TypeIs(TYP_STRUCT) || (structHnd != NO_CLASS_HANDLE));

                if (!varTypeIsStruct(argNode->GetType()) || (structHnd == NO_CLASS_HANDLE))
                {
                    asg = gtNewTempAssign(tmpNum, argNode);
                }
                else
                {
                    lvaSetStruct(tmpNum, structHnd, false);

                    // The argument cannot be a COMMA, impNormStructVal should have changed
                    // it to OBJ(COMMA(...)).
                    // It also cannot be MKREFANY because TypedReference parameters block
                    // inlining. That's probably an unnecessary limitation but who cares
                    // about TypedReference?
                    // This means that impAssignStructPtr won't have to add new statements,
                    // it cannot do that since we're not actually importing IL.

                    assert(!argNode->OperIs(GT_COMMA));

                    GenTree* dst     = gtNewLclvNode(tmpNum, tmpLcl->GetType());
                    GenTree* dstAddr = gtNewOperNode(GT_ADDR, TYP_BYREF, dst);
                    asg              = impAssignStructPtr(dstAddr, argNode, structHnd, CHECK_SPILL_NONE);
                }

                if (restoreLayout)
                {
                    tmpLcl->SetLayout(tmpLayout);
                }
            }

            Statement* stmt = gtNewStmt(asg, inlineInfo->iciStmt->GetILOffsetX());
            fgInsertStmtAfter(inlineInfo->iciBlock, afterStmt, stmt);
            afterStmt = stmt;

            DBEXEC(verbose, gtDispStmt(afterStmt));

            inlineInfo->iciBlock->bbFlags |= (bbFlags & BBF_SPLIT_GAINED);

            continue;
        }

        if (argInfo.argIsInvariant || argInfo.argIsLclVar)
        {
            assert(argNode->OperIsConst() || argNode->OperIs(GT_ADDR, GT_LCL_VAR));
            assert(!argInfo.argHasLdargaOp && !argInfo.argHasStargOp && !argInfo.argHasGlobRef);

            continue;
        }

        noway_assert(!argInfo.argIsUsed);

        if (argInfo.argHasSideEff)
        {
            Statement* newStmt = nullptr;
            bool       append  = true;

            if (argNode->OperIs(GT_OBJ))
            {
                GenTree* addr = argNode->AsObj()->GetAddr();
                GenTree* tree;

                if (fgAddrCouldBeNull(addr))
                {
                    tree = gtNewNullCheck(addr, inlineInfo->iciBlock);
                }
                else
                {
                    tree = gtUnusedValNode(addr);
                }

                newStmt = gtNewStmt(tree, inlineInfo->iciStmt->GetILOffsetX());
            }
            else
            {
                // In some special cases, unused args with side effects can
                // trigger further changes.
                //
                // (1) If the arg is a static field access and the field access
                // was produced by a call to EqualityComparer<T>.get_Default, the
                // helper call to ensure the field has a value can be suppressed.
                // This helper call is marked as a "Special DCE" helper during
                // importation, over in fgGetStaticsCCtorHelper.
                //
                // (2) NYI. If, after tunneling through GT_RET_VALs, we find that
                // the actual arg expression has no side effects, we can skip
                // appending all together. This will help jit TP a bit.

                // For case (1)
                //
                // Look for the following tree shapes
                // prejit: (IND (ADD (CONST, CALL(special dce helper...))))
                // jit   : (COMMA (CALL(special dce helper...), (FIELD ...)))

                if (argNode->OperIs(GT_COMMA))
                {
                    // Look for (COMMA (CALL(special dce helper...), (FIELD ...)))

                    GenTree* op1 = argNode->AsOp()->GetOp(0);
                    GenTree* op2 = argNode->AsOp()->GetOp(1);

                    if (op1->IsCall() && ((op1->AsCall()->gtCallMoreFlags & GTF_CALL_M_HELPER_SPECIAL_DCE) != 0) &&
                        op2->OperIs(GT_FIELD) && ((op2->gtFlags & GTF_EXCEPT) == 0))
                    {
                        JITDUMP("\nPerforming special dce on unused arg [%06u]: helper call [%06u]\n", argNode->GetID(),
                                op1->GetID());

                        append = false;
                    }
                }
                else if (argNode->OperIs(GT_IND))
                {
                    // Look for (IND (ADD (CONST, CALL(special dce helper...))))

                    GenTree* addr = argNode->AsIndir()->GetAddr();

                    if (addr->OperIs(GT_ADD))
                    {
                        GenTree* op1 = addr->AsOp()->GetOp(0);
                        GenTree* op2 = addr->AsOp()->GetOp(1);

                        if (op1->IsCall() && ((op1->AsCall()->gtCallMoreFlags & GTF_CALL_M_HELPER_SPECIAL_DCE) != 0) &&
                            op2->IsIntCon())
                        {
                            JITDUMP("\nPerforming special dce on unused arg [%06u]: helper call [%06u]\n",
                                    argNode->GetID(), op1->GetID());

                            append = false;
                        }
                    }
                }
            }

            if (!append)
            {
                assert(newStmt == nullptr);

                JITDUMP("Arg tree side effects were discardable, not appending anything for arg\n");
            }
            else
            {
                // If we don't have something custom to append,
                // just append the arg node as an unused value.

                if (newStmt == nullptr)
                {
                    newStmt = gtNewStmt(gtUnusedValNode(argNode), inlineInfo->iciStmt->GetILOffsetX());
                }

                fgInsertStmtAfter(inlineInfo->iciBlock, afterStmt, newStmt);
                afterStmt = newStmt;

                DBEXEC(verbose, gtDispStmt(afterStmt));
            }
        }
        else if (argNode->IsBoxedValue())
        {
            // Try to clean up any unnecessary boxing side effects
            // since the box itself will be ignored.
            gtTryRemoveBoxUpstreamEffects(argNode);
        }

        inlineInfo->iciBlock->bbFlags |= (bbFlags & BBF_SPLIT_GAINED);
    }

    JITDUMP("-----------------------------------------------------------------------------------------------------\n");

    return afterStmt;
}

Statement* Compiler::inlInitInlineeLocals(InlineInfo* inlineInfo,
                                          BasicBlock* block,
                                          Statement*  afterStmt,
                                          IL_OFFSETX  callILOffset)
{
    CORINFO_METHOD_INFO* InlineeMethodInfo = InlineeCompiler->info.compMethodInfo;

    unsigned lclCnt     = InlineeMethodInfo->locals.numArgs;
    bool     bbInALoop  = (block->bbFlags & BBF_BACKWARD_JUMP) != 0;
    bool     bbIsReturn = block->bbJumpKind == BBJ_RETURN;

    // If the callee contains zero-init locals, we need to explicitly initialize them if we are
    // in a loop or if the caller doesn't have compInitMem set. Otherwise we can rely on the
    // normal logic in the caller to insert zero-init in the prolog if necessary.
    if ((lclCnt != 0) && ((InlineeMethodInfo->options & CORINFO_OPT_INIT_LOCALS) != 0) &&
        ((bbInALoop && !bbIsReturn) || !info.compInitMem))
    {

#ifdef DEBUG
        if (verbose)
        {
            printf("\nZero init inlinee locals:\n");
        }
#endif // DEBUG

        InlLclVarInfo* lclVarInfo = inlineInfo->lclVarInfo;

        for (unsigned lclNum = 0; lclNum < lclCnt; lclNum++)
        {
            unsigned tmpNum = inlineInfo->lclTmpNum[lclNum];

            // If the local is used check whether we need to insert explicit zero initialization.
            if (tmpNum != BAD_VAR_NUM)
            {
                LclVarDsc* const tmpDsc = lvaGetDesc(tmpNum);
                if (!fgVarNeedsExplicitZeroInit(tmpNum, bbInALoop, bbIsReturn))
                {
                    JITDUMP("\nSuppressing zero-init for V%02u -- expect to zero in prolog\n", tmpNum);
                    tmpDsc->lvSuppressedZeroInit = 1;
                    compSuppressedZeroInit       = true;
                    continue;
                }

                var_types lclTyp = tmpDsc->GetType();
                noway_assert(lclTyp == lclVarInfo[lclNum + inlineInfo->argCnt].lclType);

                GenTree*   zero = varTypeIsStruct(lclTyp) ? gtNewIconNode(0) : gtNewZeroConNode(lclTyp);
                GenTreeOp* asg  = gtNewAssignNode(gtNewLclvNode(tmpNum, lclTyp), zero);
                Statement* stmt = gtNewStmt(asg, callILOffset);
                fgInsertStmtAfter(block, afterStmt, stmt);
                afterStmt = stmt;
            }
        }
    }

    return afterStmt;
}

// Null out inlinee GC local variables to avoid keeping GC objects alive longer than necessary.
//
void Compiler::inlNullOutInlineeGCLocals(const InlineInfo* inlineInfo, BasicBlock* block, Statement* stmtAfter)
{
    JITDUMP("Null out inlinee GC locals:\n");
    JITDUMP("-----------------------------------------------------------------------------------------------------\n");

    if (!inlineInfo->HasGcRefLocals())
    {
        JITDUMP("\tInlinee doesn't contain GC locals.\n");
        return;
    }

    if (inlineInfo->iciCall->IsImplicitTailCall())
    {
        // If the call we're inlining is in tail position then
        // we skip nulling the locals, since it can interfere
        // with tail calls introduced by the local.

        JITDUMP("Implicit tail call; skipping nulling.\n");
        return;
    }

    const IL_OFFSETX ilOffset = inlineInfo->iciStmt->GetILOffsetX();
    const unsigned   lclCount = InlineeCompiler->info.compMethodInfo->locals.numArgs;
    const unsigned   argCount = inlineInfo->argCnt;

    INDEBUG(unsigned gcLclCount = 0;)

    for (unsigned i = 0; i < lclCount; i++)
    {
        const var_types lclType = inlineInfo->lclVarInfo[argCount + i].lclType;

        if (!varTypeIsGC(lclType))
        {
            continue;
        }

        assert(gcLclCount++ < inlineInfo->numberOfGcRefLocals);

        const unsigned lclNum = inlineInfo->lclTmpNum[i];

        if (lclNum == BAD_VAR_NUM)
        {
            continue;
        }

        assert(lvaGetDesc(lclNum)->GetType() == lclType);

        if (inlineInfo->retExpr != nullptr)
        {
            // Does the local we're about to null out appear in the return
            // expression? If so we somehow messed up and didn't properly
            // spill the return value. See impInlineFetchLocal.

            noway_assert(!gtHasRef(inlineInfo->retExpr, lclNum));
        }

        GenTreeOp* nullAsg  = gtNewAssignNode(gtNewLclvNode(lclNum, lclType), gtNewZeroConNode(lclType));
        Statement* nullStmt = gtNewStmt(nullAsg, ilOffset);

        DBEXEC(verbose, gtDispStmt(nullStmt));

        if (stmtAfter == nullptr)
        {
            fgInsertStmtAtBeg(block, nullStmt);
        }
        else
        {
            fgInsertStmtAfter(block, stmtAfter, nullStmt);
        }

        stmtAfter = nullStmt;
    }

    JITDUMP("-----------------------------------------------------------------------------------------------------\n");
}
