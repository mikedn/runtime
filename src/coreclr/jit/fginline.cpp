// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef _MSC_VER
#pragma hdrstop
#endif

// Flowgraph Inline Support

/*****************************************************************************/

//------------------------------------------------------------------------
// fgCheckForInlineDepthAndRecursion: compute depth of the candidate, and
// check for recursion.
//
// Return Value:
//    The depth of the inline candidate. The root method is a depth 0, top-level
//    candidates at depth 1, etc.
//
// Notes:
//    We generally disallow recursive inlines by policy. However, they are
//    supported by the underlying machinery.
//
//    Likewise the depth limit is a policy consideration, and serves mostly
//    as a safeguard to prevent runaway inlining of small methods.
//
unsigned Compiler::fgCheckInlineDepthAndRecursion(InlineInfo* inlineInfo)
{
    BYTE*          candidateCode = inlineInfo->inlineCandidateInfo->methInfo.ILCode;
    InlineContext* inlineContext = inlineInfo->iciStmt->GetInlineContext();
    InlineResult*  inlineResult  = inlineInfo->inlineResult;

    // There should be a context for all candidates.
    assert(inlineContext != nullptr);
    int depth = 0;

    for (; inlineContext != nullptr; inlineContext = inlineContext->GetParent())
    {
        assert(inlineContext->GetCode() != nullptr);
        depth++;

        if (inlineContext->GetCode() == candidateCode)
        {
            // This inline candidate has the same IL code buffer as an already
            // inlined method does.
            inlineResult->NoteFatal(InlineObservation::CALLSITE_IS_RECURSIVE);
            break;
        }

        if (depth > InlineStrategy::IMPLEMENTATION_MAX_INLINE_DEPTH)
        {
            break;
        }
    }

    inlineResult->NoteInt(InlineObservation::CALLSITE_DEPTH, depth);
    return depth;
}

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
//   in some cases. See impInlineFetchArg.
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
            // In debug builds we want the inline tree to show all failed
            // inlines. Some inlines may fail very early and never make it to
            // candidate stage. So scan the tree looking for those early failures.
            INDEBUG(fgWalkTreePre(stmt->GetRootNodePointer(), fgFindNonInlineCandidate, stmt);)

            // The importer ensures that all inline candidates are statement roots.
            GenTree* expr = stmt->GetRootNode();

            if (GenTreeCall* call = expr->IsCall())
            {
                if (call->IsInlineCandidate() || call->IsGuardedDevirtualizationCandidate())
                {
                    bool inlined = fgMorphCallInline(stmt, call);

                    if (inlined || (call->gtInlineCandidateInfo->retExprPlaceholder != nullptr))
                    {
                        fgRemoveStmt(block, stmt DEBUGARG(/*dumpStmt */ false));
                        madeChanges = true;
                        continue;
                    }
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
    // Check that we should not have any inline candidate or return value place holder left.
    for (BasicBlock* block = fgFirstBB; block != nullptr; block = block->bbNext)
    {
        for (Statement* stmt : block->Statements())
        {
            fgWalkTreePre(stmt->GetRootNodePointer(), fgDebugCheckInlineCandidates);
        }
    }

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

//------------------------------------------------------------------------
// fgFindNonInlineCandidate: tree walk helper to ensure that a tree node
// that is not an inline candidate is noted as a failed inline.
//
// Arguments:
//    pTree - pointer to pointer tree node being walked
//    data  - contextual data for the walk
//
// Return Value:
//    walk result
//
// Note:
//    Invokes fgNoteNonInlineCandidate on the nodes it finds.

Compiler::fgWalkResult Compiler::fgFindNonInlineCandidate(GenTree** pTree, fgWalkData* data)
{
    GenTree* tree = *pTree;
    if (tree->gtOper == GT_CALL)
    {
        Compiler*    compiler = data->compiler;
        Statement*   stmt     = (Statement*)data->pCallbackData;
        GenTreeCall* call     = tree->AsCall();

        compiler->fgNoteNonInlineCandidate(stmt, call);
    }
    return WALK_CONTINUE;
}

//------------------------------------------------------------------------
// fgNoteNonInlineCandidate: account for inlining failures in calls
// not marked as inline candidates.
//
// Arguments:
//    stmt  - statement containing the call
//    call  - the call itself
//
// Notes:
//    Used in debug only to try and place descriptions of inline failures
//    into the proper context in the inline tree.

void Compiler::fgNoteNonInlineCandidate(Statement* stmt, GenTreeCall* call)
{
    if (call->IsInlineCandidate() || call->IsGuardedDevirtualizationCandidate())
    {
        return;
    }

    InlineResult      inlineResult(this, call, nullptr, "fgNotInlineCandidate");
    InlineObservation currentObservation = InlineObservation::CALLSITE_NOT_CANDIDATE;

    // Try and recover the reason left behind when the jit decided
    // this call was not a candidate.
    InlineObservation priorObservation = call->gtInlineObservation;

    if (InlIsValidObservation(priorObservation))
    {
        currentObservation = priorObservation;
    }

    // Propagate the prior failure observation to this result.
    inlineResult.NotePriorFailure(currentObservation);
    inlineResult.SetReported();

    if (call->gtCallType == CT_USER_FUNC)
    {
        // Create InlineContext for the failure
        m_inlineStrategy->NewFailure(stmt, &inlineResult);
    }
}

#endif

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
        LclVarDsc* lcl = lvaGetDesc(dst->AsLclVar());

        if (varTypeIsStruct(lcl->GetType()) && !lcl->IsImplicitByRefParam() && (lcl->GetLayout() == layout))
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
            LclVarDsc* lcl = lvaGetDesc(location->AsLclVar());

            if (varTypeIsStruct(lcl->GetType()) && !lcl->IsImplicitByRefParam() && (lcl->GetLayout() == layout))
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
    //  - impInlineRecordArgInfo itself skips RET_EXPR and sees a side effect free expression
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

/*****************************************************************************
 * Callback to make sure there is no more GT_RET_EXPR and GTF_CALL_INLINE_CANDIDATE nodes.
 */

/* static */
Compiler::fgWalkResult Compiler::fgDebugCheckInlineCandidates(GenTree** pTree, fgWalkData* data)
{
    GenTree* tree = *pTree;
    if (tree->gtOper == GT_CALL)
    {
        assert((tree->gtFlags & GTF_CALL_INLINE_CANDIDATE) == 0);
    }
    else
    {
        assert(tree->gtOper != GT_RET_EXPR);
    }

    return WALK_CONTINUE;
}

#endif // DEBUG

//------------------------------------------------------------------------------
// fgMorphCallInline: attempt to inline a call
//
// If successful, callee's IR is inserted in place of the call, and
// is marked with an InlineContext.
//
// If unsuccessful, the transformations done in anticipation of a
// possible inline are undone, and the candidate flag on the call
// is cleared.

bool Compiler::fgMorphCallInline(Statement* stmt, GenTreeCall* call)
{
    fgMorphStmt = stmt;

    InlineResult inlineResult(this, call, stmt, "fgInline");

    if (call->IsInlineCandidate())
    {
        fgMorphCallInlineHelper(call, &inlineResult);

        // We should have made up our minds one way or another....
        assert(inlineResult.IsDecided());

        // If we failed to inline, we have a bit of work to do to cleanup
        if (inlineResult.IsFailure())
        {
            // Before we do any cleanup, create a failing InlineContext to
            // capture details of the inlining attempt.
            INDEBUG(m_inlineStrategy->NewFailure(stmt, &inlineResult);)

            // Clear the Inline Candidate flag so we can ensure later we tried
            // inlining all candidates.
            call->gtFlags &= ~GTF_CALL_INLINE_CANDIDATE;
        }

        return inlineResult.IsSuccess();
    }

    // This wasn't an inline candidate. So it must be a GDV candidate.
    assert(call->IsGuardedDevirtualizationCandidate());

    return false;
}

/*****************************************************************************
 *  Helper to attempt to inline a call
 *  Sets success/failure in inline result
 *  If success, modifies current method's IR with inlinee's IR
 *  If failed, undoes any speculative modifications to current method
 */

void Compiler::fgMorphCallInlineHelper(GenTreeCall* call, InlineResult* result)
{
    // Don't expect any surprises here.
    assert(result->IsCandidate());

    if (lvaCount >= MAX_LV_NUM_COUNT_FOR_INLINING)
    {
        // For now, attributing this to call site, though it's really
        // more of a budget issue (lvaCount currently includes all
        // caller and prospective callee locals). We still might be
        // able to inline other callees into this caller, or inline
        // this callee in other callers.
        result->NoteFatal(InlineObservation::CALLSITE_TOO_MANY_LOCALS);
        return;
    }

    if (call->IsVirtual())
    {
        result->NoteFatal(InlineObservation::CALLSITE_IS_VIRTUAL);
        return;
    }

    // Re-check this because guarded devirtualization may allow these through.
    if (gtIsRecursiveCall(call) && call->IsImplicitTailCall())
    {
        result->NoteFatal(InlineObservation::CALLSITE_IMPLICIT_REC_TAIL_CALL);
        return;
    }

    // impMarkInlineCandidate() is expected not to mark tail prefixed calls
    // and recursive tail calls as inline candidates.
    noway_assert(!call->IsTailPrefixedCall());
    noway_assert(!call->IsImplicitTailCall() || !gtIsRecursiveCall(call));

    //
    // Calling inlinee's compiler to inline the method.
    //

    unsigned startVars = lvaCount;

#ifdef DEBUG
    if (verbose)
    {
        printf("Expanding INLINE_CANDIDATE in statement ");
        printStmtID(fgMorphStmt);
        printf(" in " FMT_BB ":\n", compCurBB->bbNum);
        gtDispStmt(fgMorphStmt);
        if (call->IsImplicitTailCall())
        {
            printf("Note: candidate is implicit tail call\n");
        }
    }
#endif

    impInlineRoot()->m_inlineStrategy->NoteAttempt(result);

    //
    // Invoke the compiler to inline the call.
    //

    fgInvokeInlineeCompiler(call, result);

    if (result->IsFailure())
    {
        // Undo some changes made in anticipation of inlining...

        // Zero out the used locals
        memset(lvaTable + startVars, 0, (lvaCount - startVars) * sizeof(*lvaTable));
        for (unsigned i = startVars; i < lvaCount; i++)
        {
            new (&lvaTable[i]) LclVarDsc(); // call the constructor.
        }

        lvaCount = startVars;

#ifdef DEBUG
        if (verbose)
        {
            // printf("Inlining failed. Restore lvaCount to %d.\n", lvaCount);
        }
#endif

        return;
    }

#ifdef DEBUG
    if (verbose)
    {
        // printf("After inlining lvaCount=%d.\n", lvaCount);
    }
#endif
}

void Compiler::fgInvokeInlineeCompiler(GenTreeCall* call, InlineResult* inlineResult)
{
    noway_assert(call->gtOper == GT_CALL);
    noway_assert((call->gtFlags & GTF_CALL_INLINE_CANDIDATE) != 0);
    noway_assert(opts.OptEnabled(CLFLG_INLINING));

    // This is the InlineInfo struct representing a method to be inlined.
    InlineInfo inlineInfo;
    memset(&inlineInfo, 0, sizeof(inlineInfo));
    CORINFO_METHOD_HANDLE fncHandle = call->gtCallMethHnd;

    inlineInfo.fncHandle              = fncHandle;
    inlineInfo.iciCall                = call;
    inlineInfo.iciStmt                = fgMorphStmt;
    inlineInfo.iciBlock               = compCurBB;
    inlineInfo.thisDereferencedFirst  = false;
    inlineInfo.retExpr                = nullptr;
    inlineInfo.retBB                  = nullptr;
    inlineInfo.retExprClassHnd        = nullptr;
    inlineInfo.retExprClassHndIsExact = false;
    inlineInfo.inlineResult           = inlineResult;
    inlineInfo.profileScaleState      = InlineInfo::ProfileScaleState::UNDETERMINED;
    inlineInfo.profileScaleFactor     = 0.0;
#ifdef FEATURE_SIMD
    inlineInfo.hasSIMDTypeArgLocalOrReturn = false;
#endif // FEATURE_SIMD

    InlineCandidateInfo* inlineCandidateInfo = call->gtInlineCandidateInfo;
    noway_assert(inlineCandidateInfo);
    // Store the link to inlineCandidateInfo into inlineInfo
    inlineInfo.inlineCandidateInfo = inlineCandidateInfo;

    unsigned inlineDepth = fgCheckInlineDepthAndRecursion(&inlineInfo);

    if (inlineResult->IsFailure())
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("Recursive or deep inline recursion detected. Will not expand this INLINECANDIDATE \n");
        }
#endif // DEBUG
        return;
    }

    // Set the trap to catch all errors (including recoverable ones from the EE)
    struct Param
    {
        Compiler*             pThis;
        GenTree*              call;
        CORINFO_METHOD_HANDLE fncHandle;
        InlineCandidateInfo*  inlineCandidateInfo;
        InlineInfo*           inlineInfo;
    } param;
    memset(&param, 0, sizeof(param));

    param.pThis               = this;
    param.call                = call;
    param.fncHandle           = fncHandle;
    param.inlineCandidateInfo = inlineCandidateInfo;
    param.inlineInfo          = &inlineInfo;
    bool success              = eeRunWithErrorTrap<Param>(
        [](Param* pParam) {
            // Init the local var info of the inlinee
            pParam->pThis->impInlineInitVars(pParam->inlineInfo);

            if (pParam->inlineInfo->inlineResult->IsCandidate())
            {
                /* Clear the temp table */
                memset(pParam->inlineInfo->lclTmpNum, -1, sizeof(pParam->inlineInfo->lclTmpNum));

                //
                // Prepare the call to jitNativeCode
                //

                pParam->inlineInfo->InlinerCompiler = pParam->pThis;
                if (pParam->pThis->impInlineInfo == nullptr)
                {
                    pParam->inlineInfo->InlineRoot = pParam->pThis;
                }
                else
                {
                    pParam->inlineInfo->InlineRoot = pParam->pThis->impInlineInfo->InlineRoot;
                }
                pParam->inlineInfo->argCnt                   = pParam->inlineCandidateInfo->methInfo.args.totalILArgs();
                pParam->inlineInfo->tokenLookupContextHandle = pParam->inlineCandidateInfo->exactContextHnd;

                JITLOG_THIS(pParam->pThis,
                            (LL_INFO100000, "INLINER: inlineInfo.tokenLookupContextHandle for %s set to 0x%p:\n",
                             pParam->pThis->eeGetMethodFullName(pParam->fncHandle),
                             pParam->pThis->dspPtr(pParam->inlineInfo->tokenLookupContextHandle)));

                JitFlags compileFlagsForInlinee = *pParam->pThis->opts.jitFlags;

                // The following flags are lost when inlining.
                // (This is checked in Compiler::compInitOptions().)
                compileFlagsForInlinee.Clear(JitFlags::JIT_FLAG_BBINSTR);
                compileFlagsForInlinee.Clear(JitFlags::JIT_FLAG_PROF_ENTERLEAVE);
                compileFlagsForInlinee.Clear(JitFlags::JIT_FLAG_DEBUG_EnC);
                compileFlagsForInlinee.Clear(JitFlags::JIT_FLAG_DEBUG_INFO);
                compileFlagsForInlinee.Clear(JitFlags::JIT_FLAG_REVERSE_PINVOKE);
                compileFlagsForInlinee.Clear(JitFlags::JIT_FLAG_TRACK_TRANSITIONS);

                compileFlagsForInlinee.Set(JitFlags::JIT_FLAG_SKIP_VERIFICATION);

#ifdef DEBUG
                if (pParam->pThis->verbose)
                {
                    printf("\nInvoking compiler for the inlinee method %s :\n",
                           pParam->pThis->eeGetMethodFullName(pParam->fncHandle));
                }
#endif // DEBUG

                int result =
                    jitNativeCode(pParam->fncHandle, pParam->inlineCandidateInfo->methInfo.scope,
                                  pParam->pThis->info.compCompHnd, &pParam->inlineCandidateInfo->methInfo,
                                  (void**)pParam->inlineInfo, nullptr, &compileFlagsForInlinee, pParam->inlineInfo);

                if (result != CORJIT_OK)
                {
                    // If we haven't yet determined why this inline fails, use
                    // a catch-all something bad happened observation.
                    InlineResult* innerInlineResult = pParam->inlineInfo->inlineResult;

                    if (!innerInlineResult->IsFailure())
                    {
                        innerInlineResult->NoteFatal(InlineObservation::CALLSITE_COMPILATION_FAILURE);
                    }
                }
            }
        },
        &param);
    if (!success)
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("\nInlining failed due to an exception during invoking the compiler for the inlinee method %s.\n",
                   eeGetMethodFullName(fncHandle));
        }
#endif // DEBUG

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

#ifdef DEBUG
    if (0 && verbose)
    {
        printf("\nDone invoking compiler for the inlinee method %s\n", eeGetMethodFullName(fncHandle));
    }
#endif // DEBUG

    // If there is non-NULL return, but we haven't set the pInlineInfo->retExpr,
    // That means we haven't imported any BB that contains CEE_RET opcode.
    // (This could happen for example for a BBJ_THROW block fall through a BBJ_RETURN block which
    // causes the BBJ_RETURN block not to be imported at all.)
    // Fail the inlining attempt
    if ((call->GetRetSigType() != TYP_VOID) && (inlineInfo.retExpr == nullptr))
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("\nInlining failed because pInlineInfo->retExpr is not set in the inlinee method %s.\n",
                   eeGetMethodFullName(fncHandle));
        }
#endif // DEBUG
        inlineResult->NoteFatal(InlineObservation::CALLEE_LACKS_RETURN);
        return;
    }

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // The inlining attempt cannot be failed starting from this point.
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    // We've successfully obtain the list of inlinee's basic blocks.
    // Let's insert it to inliner's basic block list.
    fgInsertInlineeBlocks(&inlineInfo);

#ifdef DEBUG

    if (verbose)
    {
        printf("Successfully inlined %s (%d IL bytes) (depth %d) [%s]\n", eeGetMethodFullName(fncHandle),
               inlineCandidateInfo->methInfo.ILCodeSize, inlineDepth, inlineResult->ReasonString());
    }

    if (verbose)
    {
        printf("--------------------------------------------------------------------------------------------\n");
    }
#endif // DEBUG

#if defined(DEBUG)
    impInlinedCodeSize += inlineCandidateInfo->methInfo.ILCodeSize;
#endif

    // We inlined...
    inlineResult->NoteSuccess();
}

//------------------------------------------------------------------------
// fgInsertInlineeBlocks: incorporate statements for an inline into the
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

void Compiler::fgInsertInlineeBlocks(InlineInfo* pInlineInfo)
{
    GenTreeCall* iciCall  = pInlineInfo->iciCall;
    Statement*   iciStmt  = pInlineInfo->iciStmt;
    BasicBlock*  iciBlock = pInlineInfo->iciBlock;
    BasicBlock*  block;

    noway_assert(iciBlock->bbStmtList != nullptr);
    noway_assert(iciStmt->GetRootNode() != nullptr);
    assert(iciStmt->GetRootNode() == iciCall);
    noway_assert(iciCall->gtOper == GT_CALL);

#ifdef DEBUG

    Statement* currentDumpStmt = nullptr;

    if (verbose)
    {
        printf("\n\n----------- Statements (and blocks) added due to the inlining of call ");
        printTreeID(iciCall);
        printf(" -----------\n");
    }

#endif // DEBUG

    // Create a new inline context and mark the inlined statements with it
    InlineContext* calleeContext = m_inlineStrategy->NewSuccess(pInlineInfo);

    for (block = InlineeCompiler->fgFirstBB; block != nullptr; block = block->bbNext)
    {
        for (Statement* stmt : block->Statements())
        {
            stmt->SetInlineContext(calleeContext);
        }
    }

    // Prepend statements
    Statement* stmtAfter = fgInlinePrependStatements(pInlineInfo);

#ifdef DEBUG
    if (verbose)
    {
        currentDumpStmt = stmtAfter;
        printf("\nInlinee method body:");
    }
#endif // DEBUG

    if ((InlineeCompiler->fgBBcount == 1) && (InlineeCompiler->fgFirstBB->bbJumpKind == BBJ_RETURN))
    {
        // Inlinee contains just one return block. So just insert its statement into the inliner block.

        if (InlineeCompiler->fgFirstBB->bbStmtList != nullptr)
        {
            stmtAfter = fgInsertStmtListAfter(iciBlock, stmtAfter, InlineeCompiler->fgFirstBB->firstStmt());
        }

        // Copy inlinee bbFlags to caller bbFlags.
        const unsigned __int64 inlineeBlockFlags = InlineeCompiler->fgFirstBB->bbFlags;
        noway_assert((inlineeBlockFlags & BBF_HAS_JMP) == 0);
        noway_assert((inlineeBlockFlags & BBF_KEEP_BBJ_ALWAYS) == 0);

        // Todo: we may want to exclude other flags here.
        iciBlock->bbFlags |= (inlineeBlockFlags & ~BBF_RUN_RARELY);

#ifdef DEBUG
        if (verbose)
        {
            noway_assert(currentDumpStmt);

            if (currentDumpStmt != stmtAfter)
            {
                do
                {
                    currentDumpStmt = currentDumpStmt->GetNextStmt();

                    printf("\n");

                    gtDispStmt(currentDumpStmt);
                    printf("\n");

                } while (currentDumpStmt != stmtAfter);
            }
        }
#endif // DEBUG

        // Append statements to null out gc ref locals, if necessary.
        fgInlineAppendStatements(pInlineInfo, iciBlock, stmtAfter);
    }
    else
    {
        BasicBlock* topBlock    = iciBlock;
        BasicBlock* bottomBlock = inlSplitInlinerBlock(topBlock, stmtAfter);

        inlInsertInlineeBlocks(pInlineInfo, topBlock, bottomBlock, iciStmt->GetILOffsetX());

        // Append statements to null out gc ref locals, if necessary.
        fgInlineAppendStatements(pInlineInfo, bottomBlock, nullptr);

#ifdef DEBUG
        if (verbose)
        {
            fgDispBasicBlocks(InlineeCompiler->fgFirstBB, InlineeCompiler->fgLastBB, true);
        }
#endif // DEBUG
    }

    inlPropagateInlineeCompilerState();

    // If there is non-NULL return, replace the GT_CALL with its return value expression,
    // so later it will be picked up by the GT_RET_EXPR node.
    if (pInlineInfo->iciCall->GetRetSigType() != TYP_VOID)
    {
        noway_assert(pInlineInfo->retExpr != nullptr);

#ifdef DEBUG
        if (verbose)
        {
            printf("\nReturn expression for call at ");
            printTreeID(iciCall);
            printf(" is\n");
            gtDispTree(pInlineInfo->retExpr);
        }
#endif // DEBUG

        iciCall->gtInlineCandidateInfo->retExprPlaceholder->SetRetExpr(pInlineInfo->retExpr,
                                                                       pInlineInfo->retBB->bbFlags);
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

void Compiler::inlInsertInlineeBlocks(InlineInfo* inlineInfo,
                                      BasicBlock* topBlock,
                                      BasicBlock* bottomBlock,
                                      IL_OFFSETX  ilOffset)
{
    assert((InlineeCompiler->fgBBcount > 1) || (InlineeCompiler->fgFirstBB->bbJumpKind != BBJ_RETURN));

    bool inheritWeight = true; // The firstBB does inherit the weight from the iciBlock

    for (BasicBlock* block = InlineeCompiler->fgFirstBB; block != nullptr; block = block->bbNext)
    {
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
            inheritWeight = true; // A return block does inherit the weight from the iciBlock
            noway_assert((block->bbFlags & BBF_HAS_JMP) == 0);
            if (block->bbNext)
            {
                block->bbJumpKind = BBJ_ALWAYS;
                block->bbJumpDest = bottomBlock;
#ifdef DEBUG
                if (verbose)
                {
                    printf("\nConvert bbJumpKind of " FMT_BB " to BBJ_ALWAYS to bottomBlock " FMT_BB "\n", block->bbNum,
                           bottomBlock->bbNum);
                }
#endif // DEBUG
            }
            else
            {
#ifdef DEBUG
                if (verbose)
                {
                    printf("\nConvert bbJumpKind of " FMT_BB " to BBJ_NONE\n", block->bbNum);
                }
#endif // DEBUG
                block->bbJumpKind = BBJ_NONE;
            }
        }

        // Update profile weight for callee blocks, if we didn't do it already.
        if (inlineInfo->profileScaleState == InlineInfo::ProfileScaleState::KNOWN)
        {
            continue;
        }

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

    // Insert inlinee's blocks into inliner's block list.
    topBlock->setNext(InlineeCompiler->fgFirstBB);
    InlineeCompiler->fgLastBB->setNext(bottomBlock);

    //
    // Add inlinee's block count to inliner's.
    //
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

#ifdef DEBUG
    unsigned optMethodFlagsBefore = optMethodFlags;
#endif

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
    // The only reason we move it here is for calling "impInlineFetchArg(0,..." to reserve a temp
    // for the "this" pointer.
    // Note: Here we no longer do the optimization that was done by thisDereferencedFirst in the old inliner.
    // However the assetionProp logic will remove any unecessary null checks that we may have added
    //
    GenTree* nullcheck = nullptr;

    if (call->gtFlags & GTF_CALL_NULLCHECK && !inlineInfo->thisDereferencedFirst)
    {
        // Call impInlineFetchArg to "reserve" a temp for the "this" pointer.
        GenTree* thisOp = impInlineFetchArg(0, inlArgInfo, lclVarInfo);
        if (fgAddrCouldBeNull(thisOp))
        {
            nullcheck = gtNewNullCheck(thisOp, block);
            // The NULL-check statement will be inserted to the statement list after those statements
            // that assign arguments to temps and before the actual body of the inlinee method.
        }
    }

    /* Treat arguments that had to be assigned to temps */
    if (inlineInfo->argCnt)
    {

#ifdef DEBUG
        if (verbose)
        {
            printf("\nArguments setup:\n");
        }
#endif // DEBUG

        for (unsigned argNum = 0; argNum < inlineInfo->argCnt; argNum++)
        {
            const InlArgInfo& argInfo        = inlArgInfo[argNum];
            const bool        argIsSingleDef = !argInfo.argHasLdargaOp && !argInfo.argHasStargOp;
            GenTree*          argNode        = inlArgInfo[argNum].argNode;

            uint64_t bbFlags = 0;
            argNode          = argNode->gtRetExprVal(&bbFlags);

            if (argInfo.argHasTmp)
            {
                noway_assert(argInfo.argIsUsed);

                /* argBashTmpNode is non-NULL iff the argument's value was
                   referenced exactly once by the original IL. This offers an
                   opportunity to avoid an intermediate temp and just insert
                   the original argument tree.

                   However, if the temp node has been cloned somewhere while
                   importing (e.g. when handling isinst or dup), or if the IL
                   took the address of the argument, then argBashTmpNode will
                   be set (because the value was only explicitly retrieved
                   once) but the optimization cannot be applied.
                 */

                GenTree* argSingleUseNode = argInfo.argBashTmpNode;

                if ((argSingleUseNode != nullptr) && !(argSingleUseNode->gtFlags & GTF_VAR_CLONED) && argIsSingleDef)
                {
                    // Change the temp in-place to the actual argument.
                    // We currently do not support this for struct arguments, so it must not be a GT_OBJ.
                    assert(argNode->gtOper != GT_OBJ);
                    argSingleUseNode->ReplaceWith(argNode, this);
                    continue;
                }
                else
                {
                    // We're going to assign the argument value to the
                    // temp we use for it in the inline body.
                    const unsigned  tmpNum  = argInfo.argTmpNum;
                    const var_types argType = lclVarInfo[argNum].lclType;

                    // Create the temp assignment for this argument

                    GenTree* asg;

                    if (varTypeIsStruct(argType))
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

                        if (varTypeIsStruct(argNode->GetType()) && (structHnd != NO_CLASS_HANDLE))
                        {
                            lvaSetStruct(tmpNum, structHnd, false);

                            // The argument cannot be a COMMA, impNormStructVal should have changed
                            // it to OBJ(COMMA(...)).
                            // It also cannot be MKREFANY because TypedReference parameters block
                            // inlining. That's probably an unnecessary limitation but who cares
                            // about TypedReference?
                            // This means that impAssignStructPtr won't have to add new statements,
                            // it cannot do that since we're not actually importing IL.

                            assert(!argNode->OperIs(GT_COMMA, GT_MKREFANY));

                            GenTree* dst     = gtNewLclvNode(tmpNum, tmpLcl->GetType());
                            GenTree* dstAddr = gtNewOperNode(GT_ADDR, TYP_BYREF, dst);
                            asg              = impAssignStructPtr(dstAddr, argNode, structHnd, CHECK_SPILL_NONE);
                        }
                        else
                        {
                            asg = gtNewTempAssign(tmpNum, argNode);
                        }

                        if (restoreLayout)
                        {
                            tmpLcl->SetLayout(tmpLayout);
                        }
                    }
                    else
                    {
                        asg = gtNewTempAssign(tmpNum, argNode);
                    }

                    Statement* stmt = gtNewStmt(asg, callILOffset);
                    fgInsertStmtAfter(block, afterStmt, stmt);
                    afterStmt = stmt;

                    // We used to refine the temp type here based on
                    // the actual arg, but we now do this up front, when
                    // creating the temp, over in impInlineFetchArg.
                    CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef DEBUG
                    if (verbose)
                    {
                        gtDispStmt(afterStmt);
                    }
#endif // DEBUG
                }
                block->bbFlags |= (bbFlags & BBF_SPLIT_GAINED);
            }
            else if (argInfo.argIsByRefToStructLocal)
            {
                // Do nothing. Arg was directly substituted as we read
                // the inlinee.
            }
            else
            {
                /* The argument is either not used or a const or lcl var */

                noway_assert(!argInfo.argIsUsed || argInfo.argIsInvariant || argInfo.argIsLclVar);

                /* Make sure we didnt change argNode's along the way, or else
                   subsequent uses of the arg would have worked with the bashed value */
                if (argInfo.argIsInvariant)
                {
                    assert(argNode->OperIsConst() || argNode->gtOper == GT_ADDR);
                }
                noway_assert((argInfo.argIsLclVar == 0) ==
                             (argNode->gtOper != GT_LCL_VAR || (argNode->gtFlags & GTF_GLOB_REF)));

                /* If the argument has side effects, append it */

                if (argInfo.argHasSideEff)
                {
                    noway_assert(argInfo.argIsUsed == false);
                    newStmt     = nullptr;
                    bool append = true;

                    if (argNode->gtOper == GT_OBJ || argNode->gtOper == GT_MKREFANY)
                    {
                        // Don't put GT_OBJ node under a GT_COMMA.
                        // Codegen can't deal with it.
                        // Just hang the address here in case there are side-effect.
                        newStmt = gtNewStmt(gtUnusedValNode(argNode->AsOp()->gtOp1), callILOffset);
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
                            GenTree* op1 = argNode->AsOp()->gtOp1;
                            GenTree* op2 = argNode->AsOp()->gtOp2;
                            if (op1->IsCall() &&
                                ((op1->AsCall()->gtCallMoreFlags & GTF_CALL_M_HELPER_SPECIAL_DCE) != 0) &&
                                (op2->gtOper == GT_FIELD) && ((op2->gtFlags & GTF_EXCEPT) == 0))
                            {
                                JITDUMP("\nPerforming special dce on unused arg [%06u]: helper call [%06u]\n",
                                        argNode->gtTreeID, op1->gtTreeID);
                                // Drop the whole tree
                                append = false;
                            }
                        }
                        else if (argNode->OperIs(GT_IND))
                        {
                            // Look for (IND (ADD (CONST, CALL(special dce helper...))))
                            GenTree* addr = argNode->AsIndir()->GetAddr();

                            if (addr->gtOper == GT_ADD)
                            {
                                GenTree* op1 = addr->AsOp()->gtOp1;
                                GenTree* op2 = addr->AsOp()->gtOp2;
                                if (op1->IsCall() &&
                                    ((op1->AsCall()->gtCallMoreFlags & GTF_CALL_M_HELPER_SPECIAL_DCE) != 0) &&
                                    op2->IsCnsIntOrI())
                                {
                                    // Drop the whole tree
                                    JITDUMP("\nPerforming special dce on unused arg [%06u]: helper call [%06u]\n",
                                            argNode->gtTreeID, op1->gtTreeID);
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
                            newStmt = gtNewStmt(gtUnusedValNode(argNode), callILOffset);
                        }

                        fgInsertStmtAfter(block, afterStmt, newStmt);
                        afterStmt = newStmt;
#ifdef DEBUG
                        if (verbose)
                        {
                            gtDispStmt(afterStmt);
                        }
#endif // DEBUG
                    }
                }
                else if (argNode->IsBoxedValue())
                {
                    // Try to clean up any unnecessary boxing side effects
                    // since the box itself will be ignored.
                    gtTryRemoveBoxUpstreamEffects(argNode);
                }

                block->bbFlags |= (bbFlags & BBF_SPLIT_GAINED);
            }
        }
    }

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

    //
    // Now zero-init inlinee locals
    //

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

//------------------------------------------------------------------------
// fgInlineAppendStatements: Append statements that are needed
// after the inlined call.
//
// Arguments:
//    inlineInfo - information about the inline
//    block      - basic block for the new statements
//    stmtAfter  - (optional) insertion point for mid-block cases
//
// Notes:
//    If the call we're inlining is in tail position then
//    we skip nulling the locals, since it can interfere
//    with tail calls introduced by the local.

void Compiler::fgInlineAppendStatements(InlineInfo* inlineInfo, BasicBlock* block, Statement* stmtAfter)
{
    // Null out any gc ref locals
    if (!inlineInfo->HasGcRefLocals())
    {
        // No ref locals, nothing to do.
        JITDUMP("fgInlineAppendStatements: no gc ref inline locals.\n");
        return;
    }

    if (inlineInfo->iciCall->IsImplicitTailCall())
    {
        JITDUMP("fgInlineAppendStatements: implicit tail call; skipping nulling.\n");
        return;
    }

    JITDUMP("fgInlineAppendStatements: nulling out gc ref inlinee locals.\n");

    Statement*           callStmt          = inlineInfo->iciStmt;
    IL_OFFSETX           callILOffset      = callStmt->GetILOffsetX();
    CORINFO_METHOD_INFO* InlineeMethodInfo = InlineeCompiler->info.compMethodInfo;
    const unsigned       lclCnt            = InlineeMethodInfo->locals.numArgs;
    InlLclVarInfo*       lclVarInfo        = inlineInfo->lclVarInfo;
    unsigned             gcRefLclCnt       = inlineInfo->numberOfGcRefLocals;
    const unsigned       argCnt            = inlineInfo->argCnt;

    for (unsigned lclNum = 0; lclNum < lclCnt; lclNum++)
    {
        // Is the local a gc ref type? Need to look at the
        // inline info for this since we will not have local
        // temps for unused inlinee locals.
        const var_types lclTyp = lclVarInfo[argCnt + lclNum].lclType;

        if (!varTypeIsGC(lclTyp))
        {
            // Nope, nothing to null out.
            continue;
        }

        // Ensure we're examining just the right number of locals.
        assert(gcRefLclCnt > 0);
        gcRefLclCnt--;

        // Fetch the temp for this inline local
        const unsigned tmpNum = inlineInfo->lclTmpNum[lclNum];

        // Is the local used at all?
        if (tmpNum == BAD_VAR_NUM)
        {
            // Nope, nothing to null out.
            continue;
        }

        // Local was used, make sure the type is consistent.
        assert(lvaTable[tmpNum].lvType == lclTyp);

        // Does the local we're about to null out appear in the return
        // expression?  If so we somehow messed up and didn't properly
        // spill the return value. See impInlineFetchLocal.
        GenTree* retExpr = inlineInfo->retExpr;
        if (retExpr != nullptr)
        {
            const bool interferesWithReturn = gtHasRef(inlineInfo->retExpr, tmpNum, false);
            noway_assert(!interferesWithReturn);
        }

        // Assign null to the local.
        GenTree*   nullExpr = gtNewTempAssign(tmpNum, gtNewZeroConNode(lclTyp));
        Statement* nullStmt = gtNewStmt(nullExpr, callILOffset);

        if (stmtAfter == nullptr)
        {
            fgInsertStmtAtBeg(block, nullStmt);
        }
        else
        {
            fgInsertStmtAfter(block, stmtAfter, nullStmt);
        }
        stmtAfter = nullStmt;

#ifdef DEBUG
        if (verbose)
        {
            gtDispStmt(nullStmt);
        }
#endif // DEBUG
    }

    // There should not be any GC ref locals left to null out.
    assert(gcRefLclCnt == 0);
}
