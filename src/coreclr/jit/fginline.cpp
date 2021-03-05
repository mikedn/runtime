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
//   in some cases. See inlUseArg.
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

    m_inlineStrategy->BeginInlining();

    for (BasicBlock* block = fgFirstBB; block != nullptr; block = block->bbNext)
    {
        compCurBB = block;

        for (Statement* stmt : block->Statements())
        {
            inlReplaceRetExpr(stmt);

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
                    continue;
                }
            }

            // COMMA(CALL, NOP) => CALL
            if (expr->OperIs(GT_COMMA) && expr->AsOp()->GetOp(0)->IsCall() && expr->AsOp()->GetOp(1)->OperIs(GT_NOP))
            {
                stmt->SetRootNode(expr->AsOp()->GetOp(0));
            }
        }

        if ((block->bbJumpKind == BBJ_COND) && block->lastStmt()->GetRootNode()->AsUnOp()->GetOp(0)->IsIntCon())
        {
            // Inlining may have created new constants so we may be able to eliminate some conditional branches.

            // TODO-MIKE-Cleanup: Why bother? If done right this would avoid unnecessary inlining
            // in dead code. But it's not done right - the conditional branch is eliminated but
            // if a successor becomes unreachable then it is not eliminated from the block list
            // so we're still going to inline in it.

            inlFoldJTrue(block);
        }
    }

#ifdef DEBUG
    inlDebugCheckInlineCandidates();
    fgVerifyHandlerTab();

    if (verbose || fgPrintInlinedMethods)
    {
        JITDUMP("\n---- Inline Tree ----\n");
        m_inlineStrategy->Dump(verbose);
    }
#endif

    return PhaseStatus::MODIFIED_EVERYTHING;
}

class RetExprReplaceVisitor : public GenTreeVisitor<RetExprReplaceVisitor>
{
    Statement* m_stmt;

public:
    RetExprReplaceVisitor(Compiler* compiler, Statement* stmt) : GenTreeVisitor(compiler), m_stmt(stmt)
    {
    }

    enum
    {
        ComputeStack      = false,
        DoPreOrder        = true,
        DoPostOrder       = true,
        DoLclVarsOnly     = false,
        UseExecutionOrder = false,
    };

    fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
    {
        GenTree* tree = *use;

        // All the operations here and in the corresponding postorder
        // callback (fgLateDevirtualization) are triggered by GT_CALL or
        // GT_RET_EXPR trees, and these (should) have the call side
        // effect flag.
        //
        // So bail out for any trees that don't have this flag.

        if ((tree->gtFlags & GTF_CALL) == 0)
        {
            return Compiler::WALK_SKIP_SUBTREES;
        }

        if (GenTreeRetExpr* retExpr = tree->IsRetExpr())
        {
            JITDUMP("---- Return expression placeholder " FMT_TREEID " ----\n", retExpr->GetID());

            if (user != nullptr)
            {
                JITDUMPTREE(user, "User:\n");
            }

            GenTree* value = retExpr->GetRetExpr();

            // We may get a chain of RET_EXPR, if inlinees return calls that are also inline
            // candidates. fgInline replaces RET_EXPRs as it traverses the statements but the
            // return expression of an inlinee isn't appended as a statement, it's referenced
            // only from InlineInfo.
            //
            // We need to find the last RET_EXPR in the chain to get the block IR summary. The
            // rest of the RET_EXPRs are basically empty return expressions so the IR summary
            // of their blocks isn't relevant.
            //
            // We may also have cases like RET_EXPR-ADD(RET_EXPR, INDEX...), the first RET_EXPR
            // is replaced now while the next one will be replaced in a subsequent call to
            // fgUpdateInlineReturnExpressionPlaceHolder. Each RET_EXPR will contribute its own
            // IR summary.

            while (value->IsRetExpr())
            {
                retExpr = value->AsRetExpr();
                value   = retExpr->GetRetExpr();
            }

            m_compiler->compCurBB->bbFlags |= retExpr->GetRetBlockIRSummary();

            if (tree->TypeIs(TYP_BYREF) && !value->TypeIs(TYP_BYREF) && value->OperIs(GT_IND))
            {
                // An RVA static may have been reinterpreted as byref.
                assert(value->TypeIs(TYP_I_IMPL));
                JITDUMP("Updating type of the return GT_IND expression to TYP_BYREF\n");

                value->SetType(TYP_BYREF);
            }

            JITDUMPTREE(value, "Return expression:\n");

            tree = *use = value;

#if FEATURE_MULTIREG_RET
            GenTreeCall* call = retExpr->GetCall();

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
                if (user->OperIs(GT_ASG))
                {
                    // Either lhs is a call V05 = call(); or lhs is addr, and asg becomes a copyBlk.
                    AttachStructInlineeToAsg(user->AsOp(), tree, call->GetRetLayout());
                }
                else
                {
                    // Just assign the inlinee to a variable to keep it simple.
                    tree = *use = AssignStructInlineeToTemp(tree, call->GetRetLayout());
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

        return Compiler::WALK_CONTINUE;
    }

    fgWalkResult PostOrderVisit(GenTree** use, GenTree* user)
    {
        GenTree* tree = *use;

        if (tree == nullptr)
        {
            // In some (rare) cases the parent node of tree will be changed to NOP during
            // the preorder by AttachStructInlineeToAsg because it's a self assignment of
            // a local (e.g. JIT\Methodical\VT\callconv\_il_reljumper3 for x64 linux).

            // TODO-MIKE-Cleanup: This is basically a hack. Can we return "skip subtrees"
            // from PreOrderVisit so we don't reach this case?

            assert((user != nullptr) && user->OperIs(GT_NOP));
            return Compiler::WALK_CONTINUE;
        }

        if (GenTreeCall* call = tree->IsCall())
        {
            if (call->IsVirtual() && call->IsUserCall() INDEBUG(&&(JitConfig.JitEnableLateDevirtualization() == 1)))
            {
                m_compiler->impLateDevirtualizeCall(call);
            }

#ifdef DEBUG
            // Some inlines may fail very early and never make it to candidate stage.
            // Report such failures so that they appear in inline tree dumps.
            if (!call->IsInlineCandidate() && !call->IsGuardedDevirtualizationCandidate())
            {
                InlineObservation observation = call->gtInlineObservation;

                if (!InlIsValidObservation(observation))
                {
                    observation = InlineObservation::CALLSITE_NOT_CANDIDATE;
                }

                InlineResult result(m_compiler, call, nullptr, "inlNoteNonCandidates");
                result.NotePriorFailure(observation);
                result.SetReported();

                if (call->IsUserCall())
                {
                    m_compiler->m_inlineStrategy->NewFailure(m_stmt, result);
                }
            }
#endif
        }
        else if (tree->OperIs(GT_ASG))
        {
            // If we're assigning to a ref typed local that has one definition,
            // we may be able to sharpen the type for the local.

            GenTree* lhs = tree->AsOp()->GetOp(0)->gtEffectiveVal();

            if (lhs->OperIs(GT_LCL_VAR) && lhs->TypeIs(TYP_REF) && m_compiler->lvaGetDesc(lhs->AsLclVar())->lvSingleDef)
            {
                bool                 isExact   = false;
                bool                 isNonNull = false;
                CORINFO_CLASS_HANDLE newClass =
                    m_compiler->gtGetClassHandle(tree->AsOp()->GetOp(1), &isExact, &isNonNull);

                if (newClass != NO_CLASS_HANDLE)
                {
                    m_compiler->lvaUpdateClass(lhs->AsLclVar()->GetLclNum(), newClass, isExact);
                }
            }
        }
        else if (!tree->OperIs(GT_JTRUE))
        {
            *use = m_compiler->gtFoldExpr(tree);
        }

        return Compiler::WALK_CONTINUE;
    }

#if FEATURE_MULTIREG_RET
private:
    GenTree* GetStructAddress(GenTree* tree)
    {
        switch (tree->GetOper())
        {
            case GT_OBJ:
            case GT_IND:
                return tree->AsIndir()->GetAddr();

            case GT_COMMA:
                tree->AsOp()->SetOp(1, GetStructAddress(tree->AsOp()->GetOp(1)));
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
                return m_compiler->gtNewAddrNode(tree);

            default:
                unreached();
        }
    }

    GenTree* GetStructAsgDst(GenTree* dst, ClassLayout* layout)
    {
        if (dst->OperIs(GT_LCL_VAR))
        {
            LclVarDsc* tmpLcl = m_compiler->lvaGetDesc(dst->AsLclVar());

            if (varTypeIsStruct(tmpLcl->GetType()) && !tmpLcl->IsImplicitByRefParam() &&
                (tmpLcl->GetLayout() == layout))
            {
                dst->gtFlags |= GTF_DONT_CSE | GTF_VAR_DEF;
                return dst;
            }

            return m_compiler->gtNewObjNode(layout, m_compiler->gtNewAddrNode(dst, TYP_I_IMPL));
        }

        GenTree* dstAddr = GetStructAddress(dst);

        if (dstAddr->OperIs(GT_ADDR))
        {
            GenTree* location = dstAddr->AsUnOp()->GetOp(0);

            if (location->OperIs(GT_LCL_VAR))
            {
                LclVarDsc* tmpLcl = m_compiler->lvaGetDesc(location->AsLclVar());

                if (varTypeIsStruct(tmpLcl->GetType()) && !tmpLcl->IsImplicitByRefParam() &&
                    (tmpLcl->GetLayout() == layout))
                {
                    dst->gtFlags |= GTF_DONT_CSE | GTF_VAR_DEF;
                    return location;
                }
            }
        }

        GenTree* obj = m_compiler->gtNewObjNode(layout, dstAddr);
        obj->gtFlags |= GTF_DONT_CSE;
        return obj;
    }

    GenTree* GetStructAsgSrc(GenTree* src, ClassLayout* layout)
    {
        if (!src->OperIs(GT_LCL_VAR, GT_FIELD) && !src->OperIsSimdOrHWintrinsic())
        {
            GenTree* srcAddr = GetStructAddress(src);

            if (srcAddr->OperIs(GT_ADDR))
            {
                src = srcAddr->AsUnOp()->GetOp(0);
            }
            else
            {
                src = m_compiler->gtNewObjNode(layout, srcAddr);
            }
        }

        // TODO-MIKE-CQ: This should probably be removed, it's here only because
        // a previous implementation (gtNewBlkOpNode) was setting it. And it
        // probably blocks SIMD tree CSEing.
        src->gtFlags |= GTF_DONT_CSE;

        return src;
    }

    GenTree* AssignStructInlineeToTemp(GenTree* src, ClassLayout* layout)
    {
        assert(!src->OperIs(GT_RET_EXPR, GT_MKREFANY));

        unsigned tempLclNum = m_compiler->lvaGrabTemp(false DEBUGARG("RetBuf for struct inline return candidates."));
        m_compiler->lvaSetStruct(tempLclNum, layout, false);
        LclVarDsc* tempLcl = m_compiler->lvaGetDesc(tempLclNum);
        GenTree*   dst     = m_compiler->gtNewLclvNode(tempLclNum, tempLcl->GetType());

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
            newAsg = m_compiler->gtNewAssignNode(dst, actualSrc);

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

            src = GetStructAsgSrc(src, layout);

            newAsg = m_compiler->gtNewAssignNode(dst, src);
            m_compiler->gtInitStructCopyAsg(newAsg->AsOp());
        }

        return m_compiler->gtNewOperNode(GT_COMMA, dst->GetType(), newAsg,
                                         m_compiler->gtNewLclvNode(tempLclNum, dst->GetType()));
    }

    void AttachStructInlineeToAsg(GenTreeOp* asg, GenTree* src, ClassLayout* layout)
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
                    m_compiler->lvaGetDesc(dst->AsLclVar())->lvIsMultiRegRet = true;
                }

                return;
            }

            // Struct calls can only be assigned to locals, in all other cases we need to introduce a temp.
            src = AssignStructInlineeToTemp(src, layout);
        }

        dst = GetStructAsgDst(dst, layout);
        src = GetStructAsgSrc(src, layout);

        asg->SetOp(0, dst);
        asg->SetOp(1, src);
        asg->gtFlags = (GTF_ASG | src->gtFlags | dst->gtFlags) & GTF_ALL_EFFECT;

        m_compiler->gtInitStructCopyAsg(asg);
    }
#endif // FEATURE_MULTIREG_RET
};

void Compiler::inlReplaceRetExpr(Statement* stmt)
{
    RetExprReplaceVisitor visitor(this, stmt);

    visitor.WalkTree(stmt->GetRootNodePointer(), nullptr);
}

void Compiler::inlFoldJTrue(BasicBlock* block)
{
    JITDUMP("Found foldable JTRUE in " FMT_BB "\n", block->bbNum);
    noway_assert((block->bbNext->countOfInEdges() > 0) && (block->bbJumpDest->countOfInEdges() > 0));

    GenTreeUnOp* jtrue = block->lastStmt()->GetRootNode()->AsUnOp();
    assert(jtrue->OperIs(GT_JTRUE));

    BasicBlock* removedSuccessor = nullptr;

    if (jtrue->GetOp(0)->AsIntCon()->GetValue() != 0)
    {
        block->bbJumpKind = BBJ_ALWAYS;
        removedSuccessor  = block->bbNext;
    }
    else
    {
        block->bbJumpKind = BBJ_NONE;
        removedSuccessor  = block->bbJumpDest;
    }

    fgRemoveRefPred(removedSuccessor, block);

    if (removedSuccessor->bbRefs == 0)
    {
        JITDUMP(FMT_BB " is now unreachable\n", removedSuccessor->bbNum);
    }

    fgRemoveStmt(block, block->lastStmt());
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

void jitInlineCode(InlineInfo* inlineInfo)
{
    Compiler* inlinerCompiler = inlineInfo->InlinerCompiler;

    if (!inlinerCompiler->inlAnalyzeInlineeSignature(inlineInfo))
    {
        return;
    }

    if (!inlinerCompiler->inlAnalyzeInlineeLocals(inlineInfo))
    {
        return;
    }

    JITLOG_THIS(inlinerCompiler, (LL_INFO100000, "INLINER: tokenLookupContextHandle for %s is 0x%p:\n",
                                  inlinerCompiler->eeGetMethodFullName(inlineInfo->iciCall->GetMethodHandle()),
                                  inlinerCompiler->dspPtr(inlineInfo->inlineCandidateInfo->exactContextHnd)));

    struct Param
    {
        InlineInfo* inlineInfo;
        int         result;
    } param{inlineInfo, CORJIT_INTERNALERROR};

    setErrorTrap(inlinerCompiler->info.compCompHnd, Param*, pParamOuter, &param)
    {
        setErrorTrap(nullptr, Param*, pParam, pParamOuter)
        {
            InlineInfo*     inlineInfo      = pParam->inlineInfo;
            Compiler*       inlinerCompiler = inlineInfo->InlinerCompiler;
            ArenaAllocator* allocator       = inlinerCompiler->compGetArenaAllocator();

            if (inlinerCompiler->InlineeCompiler == nullptr)
            {
                inlinerCompiler->InlineeCompiler = static_cast<Compiler*>(allocator->allocateMemory(sizeof(Compiler)));
            }

            Compiler* inlineeCompiler = inlinerCompiler->InlineeCompiler;

            JitTls::SetCompiler(inlineeCompiler);

            inlineeCompiler->compInit(allocator, inlineInfo->iciCall->GetMethodHandle(),
                                      inlinerCompiler->info.compCompHnd, &inlineInfo->inlineCandidateInfo->methInfo,
                                      inlineInfo);

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

            pParam->result = inlineeCompiler->compCompile(inlineInfo->inlineCandidateInfo->methInfo.scope, nullptr,
                                                          nullptr, &compileFlags);
        }
        finallyErrorTrap()
        {
            JitTls::SetCompiler(pParamOuter->inlineInfo->InlinerCompiler);
        }
        endErrorTrap()
    }
    impJitErrorTrap()
    {
        // Note that we failed to compile the inlinee, and that
        // there's no point trying to inline it again anywhere else.
        inlineInfo->inlineResult->NoteFatal(InlineObservation::CALLEE_COMPILATION_ERROR);

        param.result = __errc;
    }
    endErrorTrap()

        if ((param.result != CORJIT_OK) && !inlineInfo->inlineResult->IsFailure())
    {
        inlineInfo->inlineResult->NoteFatal(InlineObservation::CALLSITE_COMPILATION_FAILURE);
    }
}

void Compiler::inlInvokeInlineeCompiler(Statement* stmt, GenTreeCall* call, InlineResult* inlineResult)
{
    fgMorphStmt = stmt;

    InlineInfo inlineInfo;

    inlineInfo.InlinerCompiler     = this;
    inlineInfo.iciBlock            = compCurBB;
    inlineInfo.iciStmt             = stmt;
    inlineInfo.iciCall             = call;
    inlineInfo.inlineCandidateInfo = call->GetInlineCandidateInfo();
    inlineInfo.inlineResult        = inlineResult;

    inlineInfo.retExpr                = nullptr;
    inlineInfo.retBlockIRSummary      = 0;
    inlineInfo.retExprClassHnd        = NO_CLASS_HANDLE;
    inlineInfo.retExprClassHndIsExact = false;
    inlineInfo.retSpillTempLclNum     = BAD_VAR_NUM;

    inlineInfo.hasGCRefLocals        = false;
    inlineInfo.thisDereferencedFirst = false;
#ifdef FEATURE_SIMD
    inlineInfo.hasSIMDTypeArgLocalOrReturn = false;
#endif

    inlineInfo.profileScaleState  = InlineInfo::ProfileScaleState::UNDETERMINED;
    inlineInfo.profileScaleFactor = 0;

    inlineInfo.ilArgCount = 0;
    inlineInfo.ilLocCount = 0;

    unsigned inlineDepth = inlCheckInlineDepthAndRecursion(&inlineInfo);

    if (inlineResult->IsFailure())
    {
        JITDUMP("Recursive or deep inline recursion detected. Will not expand this INLINECANDIDATE \n");
        return;
    }

    JITDUMP("\nInvoking compiler for the inlinee method %s :\n", eeGetMethodFullName(call->GetMethodHandle()));

    if (!eeRunWithErrorTrap<InlineInfo>(jitInlineCode, &inlineInfo))
    {
        JITDUMP("\nInlining failed due to an exception during invoking the compiler for the inlinee method %s.\n",
                eeGetMethodFullName(inlineInfo.iciCall->GetMethodHandle()));

        if (!inlineResult->IsFailure())
        {
            inlineResult->NoteFatal(InlineObservation::CALLSITE_COMPILATION_ERROR);
        }
    }

    if (inlineResult->IsFailure())
    {
        return;
    }

    // If there is non-NULL return, but we haven't set the inlineInfo->retExpr,
    // That means we haven't imported any BB that contains CEE_RET opcode.
    // (This could happen for example for a BBJ_THROW block fall through a BBJ_RETURN block which
    // causes the BBJ_RETURN block not to be imported at all.)
    // Fail the inlining attempt
    if ((call->GetRetSigType() != TYP_VOID) && (inlineInfo.retExpr == nullptr))
    {
        JITDUMP("\nInlining failed because pInlineInfo->retExpr is not set in the inlinee method %s.\n",
                eeGetMethodFullName(inlineInfo.iciCall->GetMethodHandle()));

        inlineResult->NoteFatal(InlineObservation::CALLEE_LACKS_RETURN);

        return;
    }

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // The inlining attempt cannot be failed starting from this point.
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    inlInsertInlineeCode(&inlineInfo);

    JITDUMP("\nSuccessfully inlined %s (%d IL bytes) (depth %d) [%s]\n"
            "--------------------------------------------------------------------------------------------\n",
            eeGetMethodFullName(inlineInfo.iciCall->GetMethodHandle()),
            inlineInfo.inlineCandidateInfo->methInfo.ILCodeSize, inlineDepth, inlineResult->ReasonString());

    INDEBUG(impInlinedCodeSize += inlineInfo.inlineCandidateInfo->methInfo.ILCodeSize;)

    inlineResult->NoteSuccess();
}

void Compiler::inlAnalyzeInlineeReturn(InlineInfo* inlineInfo, unsigned returnBlockCount)
{
    if (info.GetRetSigType() == TYP_VOID)
    {
        return;
    }

    if ((returnBlockCount <= 1) && !inlineInfo->hasGCRefLocals)
    {
        // We need a spill temp only if there are multiple return blocks or if there
        // are GC locals (they need to be nulled out at the end of the inlinee and
        // doing so may interfere with the return expression).

        return;
    }

    if (inlineInfo->inlineCandidateInfo->preexistingSpillTemp != BAD_VAR_NUM)
    {
        // If we've spilled the ret expr to a temp we can reuse the temp as the
        // inlinee return spill temp.
        //
        // TODO: see if it is even better to always use this existing temp for
        // return values, even if we otherwise wouldn't need a return spill temp...

        inlineInfo->retSpillTempLclNum = inlineInfo->inlineCandidateInfo->preexistingSpillTemp;

        JITDUMP("\nInliner: re-using pre-existing spill temp V%02u\n", inlineInfo->retSpillTempLclNum);

        if (info.GetRetSigType() == TYP_REF)
        {
            // We may have co-opted an existing temp for the return spill.
            // We likely assumed it was single-def at the time, but now
            // we can see it has multiple definitions.

            LclVarDsc* lcl = lvaGetDesc(inlineInfo->retSpillTempLclNum);

            if ((returnBlockCount > 1) && lcl->lvSingleDef)
            {
                // Make sure it is no longer marked single def. This is only safe
                // to do if we haven't ever updated the type.

                assert(!lcl->lvClassInfoUpdated);
                JITDUMP("Marked return spill temp V%02u as NOT single def temp\n", inlineInfo->retSpillTempLclNum);
                lcl->lvSingleDef = false;
            }
        }

        return;
    }

    unsigned   spillLclNum = lvaGrabTemp(false DEBUGARG("inlinee return spill temp"));
    LclVarDsc* spillLcl    = lvaGetDesc(spillLclNum);

    if (varTypeIsStruct(info.GetRetSigType()))
    {
        lvaSetStruct(spillLclNum, info.compMethodInfo->args.retTypeClass, false);
    }
    else
    {
        spillLcl->SetType(info.GetRetSigType());

        if (info.GetRetSigType() == TYP_REF)
        {
            // If the method returns a ref class, set the class of the spill temp
            // to the method's return value. We may update this later if it turns
            // out we can prove the method returns a more specific type.

            if (returnBlockCount == 1)
            {
                spillLcl->lvSingleDef = true;
                JITDUMP("Marked return spill temp V%02u as a single def temp\n", spillLclNum);
            }

            if (info.compMethodInfo->args.retTypeClass != nullptr)
            {
                lvaSetClass(spillLclNum, info.compMethodInfo->args.retTypeClass);
            }
        }
    }

    inlineInfo->retSpillTempLclNum = spillLclNum;
}

bool Compiler::inlImportReturn(InlineInfo* inlineInfo, GenTree* retExpr, CORINFO_CLASS_HANDLE retExprClass)
{
    JITDUMPTREE(retExpr, "\nInlinee return expression:\n");

    // If the inlinee has GC ref locals we always need to have a spill temp
    // for the return value. This temp should have been set up in advance,
    // over in inlAnalyzeInlineeReturn.

    assert(!inlineInfo->hasGCRefLocals || (inlineInfo->retSpillTempLclNum != BAD_VAR_NUM));

    // Make sure the return value type matches the return signature type.
    {
        var_types callType   = varActualType(info.GetRetSigType());
        var_types returnType = varActualType(retExpr->GetType());

        if (returnType != callType)
        {
            // Allow TYP_BYREF to be returned as TYP_I_IMPL and vice versa.

            if (!((returnType == TYP_BYREF) && (callType == TYP_I_IMPL)) &&
                !((returnType == TYP_I_IMPL) && (callType == TYP_BYREF)))
            {
                JITDUMP("Return type mismatch: have %s, needed %s\n", varTypeName(returnType), varTypeName(callType));
                compInlineResult->NoteFatal(InlineObservation::CALLSITE_RETURN_TYPE_MISMATCH);
                return false;
            }

            JITDUMP("Allowing return type mismatch: have %s, needed %s\n", varTypeName(returnType),
                    varTypeName(callType));
        }
    }

    if (varTypeIsSmall(info.GetRetSigType()))
    {
        // Small-typed return values are normalized by the callee

        if (gtIsSmallIntCastNeeded(retExpr, info.GetRetSigType()))
        {
            retExpr = gtNewCastNode(TYP_INT, retExpr, false, info.GetRetSigType());
        }
    }
    else if (info.GetRetSigType() == TYP_REF)
    {
        if (inlineInfo->retSpillTempLclNum != BAD_VAR_NUM)
        {
            // If this method returns a REF type and we have a spill temp, track the actual types
            // seen in the returns so we can update the spill temp class when inlining is done.

            bool                 isExact      = false;
            bool                 isNonNull    = false;
            CORINFO_CLASS_HANDLE returnClsHnd = gtGetClassHandle(retExpr, &isExact, &isNonNull);

            if (inlineInfo->retExpr == nullptr)
            {
                // This is the first return, so best known type is the type
                // of this return value.
                inlineInfo->retExprClassHnd        = returnClsHnd;
                inlineInfo->retExprClassHndIsExact = isExact;
            }
            else if (inlineInfo->retExprClassHnd != returnClsHnd)
            {
                // This return site type differs from earlier seen sites,
                // so reset the info and we'll fall back to using the method's
                // declared return type for the return spill temp.
                inlineInfo->retExprClassHnd        = nullptr;
                inlineInfo->retExprClassHndIsExact = false;
            }
        }
    }
    else if (info.GetRetSigType() == TYP_BYREF)
    {
        // If we are inlining a method that returns a struct byref, check whether
        // we are "reinterpreting" the struct.

        GenTree* effectiveRetVal = retExpr->gtEffectiveVal();
        if (retExpr->TypeIs(TYP_BYREF) && effectiveRetVal->OperIs(GT_ADDR))
        {
            GenTree* location = effectiveRetVal->AsUnOp()->GetOp(0);
            if (location->OperIs(GT_LCL_VAR))
            {
                LclVarDsc* lcl = lvaGetDesc(location->AsLclVar());

                if (varTypeIsStruct(location->GetType()) && !isOpaqueSIMDLclVar(lcl))
                {
                    CORINFO_CLASS_HANDLE byrefClass;
                    var_types            byrefType = JITtype2varType(
                        info.compCompHnd->getChildType(info.compMethodInfo->args.retTypeClass, &byrefClass));

                    if (varTypeIsStruct(byrefType) && (lcl->GetLayout()->GetClassHandle() != byrefClass))
                    {
                        // We are returning a byref to struct1; the method signature specifies return type as
                        // byref to struct2. struct1 and struct2 are different so we are "reinterpreting" the
                        // struct (e.g. System.Runtime.CompilerServices.Unsafe.As<TFrom, TTo>).
                        // We need to mark the source struct variable as having overlapping fields because its
                        // fields may be accessed using field handles of a different type, which may confuse
                        // optimizations, in particular, value numbering.

                        JITDUMP("\nSetting lvOverlappingFields on V%02u due to struct reinterpretation\n",
                                location->AsLclVar()->GetLclNum());

                        lcl->lvOverlappingFields = true;
                    }
                }
            }
        }
    }

    if (retExpr->IsCall() && retExpr->AsCall()->TreatAsHasRetBufArg() && (info.retDesc.GetRegCount() > 1))
    {
        // The multi reg case is currently handled during unbox import.
        assert(info.retDesc.GetRegCount() == 1);

        // TODO-MIKE-CQ: If there's an inlinee spill temp we could use that as pseudo
        // return buffer instead of creating another temp. But that would leave the
        // inlinee spill temp address exposed so it's perhaps not such a good idea to
        // do it unconditionally. The inlinee spill temp should probably be used only
        // if the struct is large, when copying is likely to be more costly than the
        // lack of optimizations caused by address exposed.
        // No FX diffs if done so it's probably very rare so not worth the trouble now.

        retExpr = impSpillPseudoReturnBufferCall(retExpr, retExprClass);
    }

    if (inlineInfo->retSpillTempLclNum != BAD_VAR_NUM)
    {
        unsigned  lclNum  = inlineInfo->retSpillTempLclNum;
        var_types lclType = lvaGetDesc(lclNum)->GetType();
        GenTree*  lclVar  = gtNewLclvNode(lclNum, lclType);
        GenTree*  asg;

        if (varTypeIsStruct(retExpr->GetType()))
        {
            GenTree* lclAddr = gtNewAddrNode(lclVar, TYP_I_IMPL);

            asg = impAssignStructPtr(lclAddr, retExpr, retExprClass, CHECK_SPILL_NONE);
        }
        else
        {
            asg = gtNewAssignNode(lclVar, retExpr);
        }

        impAppendTree(asg, CHECK_SPILL_NONE, impCurStmtOffs);

        if (inlineInfo->retExpr == nullptr)
        {
            retExpr = gtNewLclvNode(lclNum, lclType);
        }
        else if (inlineInfo->iciCall->HasRetBufArg())
        {
            assert(inlineInfo->retExpr->OperIs(GT_ASG));
            assert(inlineInfo->retExpr->AsOp()->GetOp(1)->AsLclVar()->GetLclNum() == lclNum);
        }
        else
        {
            assert(inlineInfo->retExpr->AsLclVar()->GetLclNum() == lclNum);
        }
    }

    if (inlineInfo->retExpr == nullptr)
    {
        if (inlineInfo->iciCall->HasRetBufArg())
        {
            // TODO-MIKE-CQ: Why do we have an inlinee return spill temp when we also
            // have a return buffer? We first spill to the temp and then copy the temp
            // to the return buffer, that seems like an unnecessary copy.
            // Also, what happens if the return address arg has side effects?

            GenTree* retBufAddr = gtCloneExpr(inlineInfo->iciCall->gtCallArgs->GetNode());

            retExpr = impAssignStructPtr(retBufAddr, retExpr, retExprClass, CHECK_SPILL_ALL);
        }

        JITDUMPTREE(retExpr, "Inliner return expression:\n");

        inlineInfo->retExpr = retExpr;

        // If the inlinee has multiple blocks but a single return block then we'll insert
        // the inlinee blocks in the inliner and move the return expression to an existing
        // inliner block. The return expression may contain nodes such as INDEX so the
        // inliner block needs to "inherit" the IR summary from inlinee's return block.

        if ((inlineInfo->retSpillTempLclNum == BAD_VAR_NUM) && (fgFirstBB->bbNext != nullptr))
        {
            inlineInfo->retBlockIRSummary = compCurBB->bbFlags & BBF_IR_SUMMARY;
        }
    }

    return true;
}

void Compiler::inlUpdateRetSpillTempClass(InlineInfo* inlineInfo)
{
    // Update type of return spill temp if we have gathered
    // better info when importing the inlinee, and the return
    // spill temp is single def.

    if (inlineInfo->retSpillTempLclNum != BAD_VAR_NUM)
    {
        CORINFO_CLASS_HANDLE retExprClassHnd = inlineInfo->retExprClassHnd;
        if (retExprClassHnd != nullptr)
        {
            LclVarDsc* returnSpillVarDsc = lvaGetDesc(inlineInfo->retSpillTempLclNum);

            if (returnSpillVarDsc->lvSingleDef)
            {
                lvaUpdateClass(inlineInfo->retSpillTempLclNum, retExprClassHnd, inlineInfo->retExprClassHndIsExact);
            }
        }
    }
}

unsigned Compiler::inlCheckInlineDepthAndRecursion(const InlineInfo* inlineInfo)
{
    // Compute depth of the candidate, and check for recursion.
    // We generally disallow recursive inlines by policy. However, they are
    // supported by the underlying machinery.
    // Likewise the depth limit is a policy consideration, and serves mostly
    // as a safeguard to prevent runaway inlining of small methods.

    InlineContext* inlineContext = inlineInfo->iciStmt->GetInlineContext();

    if (inlineContext == nullptr)
    {
        inlineContext = m_inlineStrategy->GetRootContext();
    }

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

bool Compiler::inlAnalyzeInlineeSignature(InlineInfo* inlineInfo)
{
    CORINFO_SIG_INFO& argsSig = inlineInfo->inlineCandidateInfo->methInfo.args;

    inlineInfo->ilArgCount = argsSig.totalILArgs();

    GenTreeCall*      call    = inlineInfo->iciCall;
    GenTreeCall::Use* thisArg = call->gtCallThisArg;
    InlArgInfo*       argInfo = inlineInfo->ilArgInfo;
    unsigned          argNum  = 0;

    assert((argsSig.hasThis()) == (thisArg != nullptr));

    if (thisArg != nullptr)
    {
        new (&argInfo[argNum++]) InlArgInfo(thisArg->GetNode(), true);
    }

    unsigned typeCtxtArgNum = UINT32_MAX;

    if ((argsSig.callConv & CORINFO_CALLCONV_PARAMTYPE) != 0)
    {
#if USER_ARGS_COME_LAST
        typeCtxtArgNum = argNum;
#else
        typeCtxtArgNum = inlineInfo->ilArgCount;
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
            typeCtxtArgNum = UINT32_MAX;
            continue;
        }

        new (&argInfo[argNum++]) InlArgInfo(use.GetNode());
    }

    assert(argNum == inlineInfo->ilArgCount);

    for (unsigned i = 0; i < argNum; i++)
    {
        if (!inlAnalyzeInlineeArg(inlineInfo, i))
        {
            return false;
        }
    }

#ifdef FEATURE_SIMD
    bool foundSIMDType = inlineInfo->hasSIMDTypeArgLocalOrReturn;

    if (varTypeIsSIMD(inlineInfo->iciCall->GetRetSigType()))
    {
        foundSIMDType = true;
    }
#endif

    if (thisArg != nullptr)
    {
        CORINFO_CLASS_HANDLE methodClass = inlineInfo->inlineCandidateInfo->clsHandle;
        GenTree*             argNode     = thisArg->GetNode();

        if ((inlineInfo->inlineCandidateInfo->clsAttr & CORINFO_FLG_VALUECLASS) == 0)
        {
            if (argNode->IsIntegralConst(0))
            {
                inlineInfo->inlineResult->NoteFatal(InlineObservation::CALLSITE_ARG_HAS_NULL_THIS);
                return false;
            }

            if (!argNode->TypeIs(TYP_REF))
            {
                inlineInfo->inlineResult->NoteFatal(InlineObservation::CALLSITE_ARG_NO_BASH_TO_REF);
                return false;
            }

            argInfo[0].paramType  = TYP_REF;
            argInfo[0].paramClass = methodClass;
        }
        else
        {
            // A native pointer can be passed as "this" to a method of a struct.
            assert(argNode->TypeIs(TYP_BYREF, TYP_I_IMPL));

#ifdef FEATURE_SIMD
            if (!foundSIMDType && isSIMDorHWSIMDClass(methodClass))
            {
                foundSIMDType = true;
            }
#endif

            argInfo[0].paramType  = TYP_BYREF;
            argInfo[0].paramClass = NO_CLASS_HANDLE;
        }
    }

    CORINFO_ARG_LIST_HANDLE paramHandle = argsSig.args;

    for (unsigned i = (thisArg ? 1 : 0); i < argNum; i++, paramHandle = info.compCompHnd->getArgNext(paramHandle))
    {
        CORINFO_CLASS_HANDLE paramClass;
        CorInfoType          paramCorType = strip(info.compCompHnd->getArgType(&argsSig, paramHandle, &paramClass));
        var_types            paramType    = JITtype2varType(paramCorType);

        if (paramType == TYP_REF)
        {
            paramClass = info.compCompHnd->getArgClass(&argsSig, paramHandle);
        }
        else if (paramType == TYP_BYREF)
        {
            // Ignore whatever class the runtime may have returned, we don't need it.
            paramClass = NO_CLASS_HANDLE;
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
        }
        else if (paramClass != NO_CLASS_HANDLE)
        {
            // This is a "normed type" - a struct that contains a single primitive type field.
            // See lvaInitVarDsc.

            assert(info.compCompHnd->isValueClass(paramClass));
            assert(info.compCompHnd->getTypeForPrimitiveValueClass(paramClass) == CORINFO_TYPE_UNDEF);

            argInfo[i].paramHasNormedType = true;
        }
        else if (paramCorType <= CORINFO_TYPE_DOUBLE)
        {
            // TODO-MIKE-Cleanup: This is incorrect, it classifies all primitive types
            // as "normed". It is based on old code that used typeInfo's IsValueClass
            // instead of IsType(TI_STRUCT). Fixing this produces some diffs.

            argInfo[i].paramHasNormedType = true;
        }

        argInfo[i].paramType  = paramType;
        argInfo[i].paramClass = paramClass;

        // Does the tree type match the signature type?

        GenTree* argNode = argInfo[i].argNode;

        if ((paramType == argNode->GetType()) || (paramType == varActualType(argNode->GetType())))
        {
            continue;
        }

        if (varTypeIsSmall(paramType) && varTypeIsIntegral(argNode->GetType()))
        {
            if (!gtIsSmallIntCastNeeded(argNode, paramType))
            {
                continue;
            }

            argNode = gtNewCastNode(TYP_INT, argNode, false, paramType);

            if (argInfo[i].argNode->OperIsConst())
            {
                argNode = gtFoldExprConst(argNode);
                assert(argNode->OperIsConst());
            }

            argInfo[i].argNode              = argNode;
            argInfo[i].argIsUnaliasedLclVar = false;

            continue;
        }

        if ((paramType == TYP_BYREF) && argNode->TypeIs(TYP_I_IMPL))
        {
            // Native int args can be coerced to BYREF.

            continue;
        }

        if ((paramType == TYP_I_IMPL) && argNode->TypeIs(TYP_BYREF))
        {
            // BYREF args cannot be coerced to native int but the JIT ignores the spec.
            // But this is done only if the arg represents a local address which is BYREF
            // in spec but in reality is just a native pointer.

            if (argNode->IsLocalAddrExpr() == nullptr)
            {
                inlineInfo->inlineResult->NoteFatal(InlineObservation::CALLSITE_ARG_NO_BASH_TO_INT);
                return false;
            }

            assert(argNode->OperIs(GT_ADDR, GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR));
            argNode->SetType(TYP_I_IMPL);

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

bool Compiler::inlAnalyzeInlineeArg(InlineInfo* inlineInfo, unsigned argNum)
{
    InlArgInfo& argInfo = inlineInfo->ilArgInfo[argNum];

    JITDUMP("Argument %u%s ", argNum, argInfo.paramIsThis ? " (this)" : "");

    if (argInfo.argNode->OperIs(GT_MKREFANY))
    {
        inlineInfo->inlineResult->NoteFatal(InlineObservation::CALLSITE_ARG_IS_MKREFANY);
        return false;
    }

    assert(!argInfo.argNode->IsRetExpr());

    if (argInfo.argNode->OperIsConst())
    {
        argInfo.argIsInvariant = true;

        JITDUMP("is constant");
    }
    else if (argInfo.argNode->OperIs(GT_LCL_VAR))
    {
        LclVarDsc* lcl = lvaGetDesc(argInfo.argNode->AsLclVar());

        if (!lcl->lvHasLdAddrOp && !lcl->lvAddrExposed)
        {
            argInfo.argIsUnaliasedLclVar = true;

            JITDUMP("is unaliased local");
        }
        else
        {
            argInfo.argHasGlobRef = true;

            JITDUMP("is aliased local");
        }
    }
    else if (GenTreeLclVar* addrLclVar = impIsAddressInLocal(argInfo.argNode))
    {
        argInfo.argIsInvariant = true;

#ifdef FEATURE_SIMD
        if (varTypeIsStruct(addrLclVar->GetType()) && lvaGetDesc(addrLclVar)->lvSIMDType)
        {
            inlineInfo->hasSIMDTypeArgLocalOrReturn = true;
        }
#endif

        JITDUMP("is local address");
    }
    else
    {
        if ((argInfo.argNode->gtFlags & GTF_ALL_EFFECT) != 0)
        {
            argInfo.argHasGlobRef = (argInfo.argNode->gtFlags & GTF_GLOB_REF) != 0;
            argInfo.argHasSideEff = (argInfo.argNode->gtFlags & (GTF_ALL_EFFECT & ~GTF_GLOB_REF)) != 0;
        }

        if (!argInfo.argHasGlobRef)
        {
            // Normally address exposed locals already have GTF_GLOB_REF but we haven't yet
            // determined which locals are address exposed so we'll have to settle for less,
            // if the expression contains any address taken locals then it's treated as if
            // it has GTF_GLOB_REF.

            argInfo.argHasGlobRef = gtHasAddressTakenLocals(argInfo.argNode);
        }

        JITDUMP("%s%s%s%s", argInfo.argHasGlobRef || argInfo.argHasSideEff ? "has " : "is side effect free",
                argInfo.argHasGlobRef ? "global refs" : "",
                argInfo.argHasGlobRef && argInfo.argHasSideEff ? " and " : "",
                argInfo.argHasSideEff ? "side effects" : "");
    }

    JITDUMPTREE(argInfo.argNode, ":\n");

    return true;
}

void InlineInfo::NoteParamStore(unsigned ilArgNum)
{
    if (ilArgNum < ilArgCount)
    {
        ilArgInfo[ilArgNum].paramHasStores = true;
    }
}

void InlineInfo::NoteAddressTakenParam(unsigned ilArgNum)
{
    if (ilArgNum < ilArgCount)
    {
        ilArgInfo[ilArgNum].paramIsAddressTaken = true;
    }
}

bool InlineInfo::IsNormedTypeParam(unsigned ilArgNum) const
{
    return (ilArgNum < ilArgCount) && ilArgInfo[ilArgNum].paramHasNormedType;
}

bool InlineInfo::IsInvariantArg(unsigned ilArgNum) const
{
    return (ilArgNum < ilArgCount) && ilArgInfo[ilArgNum].argIsInvariant;
}

typeInfo InlineInfo::GetParamTypeInfo(unsigned ilArgNum) const
{
    assert(ilArgNum < ilArgCount);

    const InlArgInfo& argInfo = ilArgInfo[ilArgNum];

    if (argInfo.paramType == TYP_REF)
    {
        return typeInfo(TI_REF, argInfo.paramClass);
    }

    if (argInfo.paramClass != NO_CLASS_HANDLE)
    {
        // Make sure we didn't get a handle for a BYREF, it wouldn't be a normed/struct type.
        assert(argInfo.paramType != TYP_BYREF);

        return typeInfo(TI_STRUCT, argInfo.paramClass);
    }

    return typeInfo();
}

bool InlineInfo::IsThisParam(GenTree* tree) const
{
    return tree->OperIs(GT_LCL_VAR) && (ilArgCount > 0) &&
           (tree->AsLclVar()->GetLclNum() == ilArgInfo[0].paramLclNum) && ilArgInfo[0].paramIsThis;
}

bool Compiler::inlAnalyzeInlineeLocals(InlineInfo* inlineInfo)
{
    CORINFO_SIG_INFO&       localsSig     = inlineInfo->inlineCandidateInfo->methInfo.locals;
    CORINFO_ARG_LIST_HANDLE localHandle   = localsSig.args;
    bool                    foundSIMDType = false;

    inlineInfo->ilLocCount = localsSig.numArgs;

    for (unsigned i = 0; i < localsSig.numArgs; i++, localHandle = info.compCompHnd->getArgNext(localHandle))
    {
        CORINFO_CLASS_HANDLE lclClass;
        CorInfoTypeWithMod   lclCorType       = info.compCompHnd->getArgType(&localsSig, localHandle, &lclClass);
        var_types            lclType          = JITtype2varType(strip(lclCorType));
        bool                 lclIsPinned      = false;
        bool                 lclHasNormedType = false;

        if (varTypeIsGC(lclType))
        {
            if (lclType == TYP_REF)
            {
                lclClass = info.compCompHnd->getArgClass(&localsSig, localHandle);
            }
            else
            {
                lclClass = NO_CLASS_HANDLE;
            }

            if ((lclCorType & CORINFO_TYPE_MOD_PINNED) != 0)
            {
                // Pinned locals may cause inlines to fail.
                inlineInfo->inlineResult->Note(InlineObservation::CALLEE_HAS_PINNED_LOCALS);
                if (inlineInfo->inlineResult->IsFailure())
                {
                    return false;
                }

                JITDUMP("Inlinee local #%02u is pinned\n", i);

                lclIsPinned = true;
            }

            inlineInfo->hasGCRefLocals = true;
        }
        else if (lclType == TYP_STRUCT)
        {
            if ((info.compCompHnd->getClassAttribs(lclClass) & CORINFO_FLG_CONTAINS_GC_PTR) != 0)
            {
                // If this local is a struct type with GC fields, inform the inliner.
                // It may choose to bail out on the inline.

                inlineInfo->inlineResult->Note(InlineObservation::CALLEE_HAS_GC_STRUCT);
                if (inlineInfo->inlineResult->IsFailure())
                {
                    return false;
                }

                // Do further notification in the case where the call site is rare; some policies do
                // not track the relative hotness of call sites for "always" inline cases.
                if (inlineInfo->iciBlock->isRunRarely())
                {
                    inlineInfo->inlineResult->Note(InlineObservation::CALLSITE_RARE_GC_STRUCT);
                    if (inlineInfo->inlineResult->IsFailure())
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
        }
        else if (lclClass != NO_CLASS_HANDLE)
        {
            // This is a "normed type" - a struct that contains a single primitive type field.
            // See lvaInitVarDsc.

            assert(info.compCompHnd->isValueClass(lclClass));
            assert(info.compCompHnd->getTypeForPrimitiveValueClass(lclClass) == CORINFO_TYPE_UNDEF);

            lclHasNormedType = true;
        }
        else if (strip(lclCorType) <= CORINFO_TYPE_DOUBLE)
        {
            // TODO-MIKE-Cleanup: This is incorrect, it classifies all primitive types
            // as "normed". It is based on old code that used typeInfo's IsValueClass
            // instead of IsType(TI_STRUCT). Fixing this produces some diffs.

            lclHasNormedType = true;
        }

        new (&inlineInfo->ilLocInfo[i]) InlLocInfo(lclType, lclClass, lclIsPinned, lclHasNormedType);
    }

#ifdef FEATURE_SIMD
    inlineInfo->hasSIMDTypeArgLocalOrReturn |= foundSIMDType;
#endif

    return true;
}

void InlineInfo::NoteLocalStore(unsigned ilLocNum)
{
    if (ilLocNum < ilLocCount)
    {
        InlLocInfo& info = ilLocInfo[ilLocNum];

        if (info.lclHasStlocOp)
        {
            info.lclHasMultipleStlocOp = 1;
        }
        else
        {
            info.lclHasStlocOp = 1;
        }
    }
}

void InlineInfo::NoteAddressTakenLocal(unsigned ilLocNum)
{
    if (ilLocNum < ilLocCount)
    {
        ilLocInfo[ilLocNum].lclHasLdlocaOp = true;
    }
}

bool InlineInfo::IsNormedTypeLocal(unsigned ilLocNum) const
{
    return (ilLocNum < ilLocCount) && ilLocInfo[ilLocNum].lclHasNormedType;
}

unsigned Compiler::inlGetInlineeLocal(InlineInfo* inlineInfo, unsigned ilLocNum)
{
    assert(ilLocNum < inlineInfo->ilLocCount);

    if (inlineInfo->ilLocInfo[ilLocNum].lclIsUsed)
    {
        return inlineInfo->ilLocInfo[ilLocNum].lclNum;
    }

    return inlAllocInlineeLocal(inlineInfo, ilLocNum);
}

unsigned Compiler::inlAllocInlineeLocal(InlineInfo* inlineInfo, unsigned ilLocNum)
{
    InlLocInfo& lclInfo = inlineInfo->ilLocInfo[ilLocNum];

    assert(!lclInfo.lclIsUsed);

    const unsigned lclNum = lvaGrabTemp(false DEBUGARG("inlinee local"));

    lclInfo.lclNum    = lclNum;
    lclInfo.lclIsUsed = true;

    LclVarDsc* lcl = lvaGetDesc(lclNum);

    lcl->lvPinned               = lclInfo.lclIsPinned;
    lcl->lvHasLdAddrOp          = lclInfo.lclHasLdlocaOp;
    lcl->lvHasILStoreOp         = lclInfo.lclHasStlocOp;
    lcl->lvHasMultipleILStoreOp = lclInfo.lclHasMultipleStlocOp;

    if (varTypeIsStruct(lclInfo.lclType))
    {
        lvaSetStruct(lclNum, lclInfo.lclClass, /* unsafeValueClsCheck */ true);
    }
    else
    {
        lcl->SetType(lclInfo.lclType);

        // Copy over class handle for ref types. Note this may be a shared type, someday
        // perhaps we can get the exact signature and pass in a more precise type.

        if (lclInfo.lclType == TYP_REF)
        {
            assert(lcl->lvSingleDef == 0);

            lcl->lvSingleDef = !lclInfo.lclHasMultipleStlocOp && !lclInfo.lclHasLdlocaOp;

            if (lcl->lvSingleDef)
            {
                JITDUMP("Marked V%02u as a single def temp\n", lclNum);
            }

            lvaSetClass(lclNum, lclInfo.lclClass);
        }
        else if (lclInfo.lclClass != NO_CLASS_HANDLE)
        {
            // This is a "normed type", we need to set lvImpTypeInfo to preserve the struct handle.

            // Make sure we didn't get a handle for a BYREF, it wouldn't be a normed type.
            assert(lclInfo.lclType != TYP_BYREF);

            lcl->lvImpTypeInfo = typeInfo(TI_STRUCT, lclInfo.lclClass);
        }
    }

#ifdef DEBUG
    // Sanity check that we're properly prepared for GC pointer locals.
    if (varTypeIsGC(lclInfo.lclType))
    {
        // Since there are GC pointer locals we should have seen them earlier
        // and if there was a return value, set up the spill temp.
        assert(impInlineInfo->hasGCRefLocals);
        assert((info.GetRetSigType() == TYP_VOID) || (inlineInfo->retSpillTempLclNum != BAD_VAR_NUM));
    }
    else
    {
        // Make sure all pinned locals count as gc refs.
        assert(!lclInfo.lclIsPinned);
    }
#endif

    return lclNum;
}

GenTree* Compiler::inlUseArg(InlineInfo* inlineInfo, unsigned ilArgNum)
{
    assert(ilArgNum < inlineInfo->ilArgCount);

    InlArgInfo& argInfo = inlineInfo->ilArgInfo[ilArgNum];
    GenTree*    argNode = argInfo.argNode;

    assert(!argNode->IsRetExpr());

    if (argInfo.argIsInvariant && !argInfo.paramIsAddressTaken && !argInfo.paramHasStores)
    {
        // Directly substitute constants or addresses of locals
        //
        // Clone the argument expression. Note that we cannot use the original
        // argument the first time we encounter a use of the parameter, it may
        // get changed during import and then subsequent uses would clone the
        // changed tree instead of the original one, or we abort inlining and
        // the original call will use the changed tree.

        argNode = gtCloneExpr(argNode);

        // We may need to retype to ensure we match the callee's view of the type.
        // Otherwise callee-pass throughs of arguments can create return type
        // mismatches that block inlining.
        //
        // Note argument type mismatches that prevent inlining should
        // have been caught in inlAnalyzeInlineeArgs.

        if (argNode->GetType() != argInfo.paramType)
        {
            argNode->SetType(varActualType(argInfo.paramType));
        }

        return argNode;
    }

    if (argInfo.argIsUnaliasedLclVar && !argInfo.paramIsAddressTaken && !argInfo.paramHasStores)
    {
        // Directly substitute unaliased caller locals for args that cannot be modified
        //
        // Use the caller-supplied node if this is the first use.

        unsigned lclNum = argNode->AsLclVar()->GetLclNum();

        // Use an equivalent copy if this is the second or subsequent
        // use, or if we need to retype.
        //
        // Note argument type mismatches that prevent inlining should
        // have been caught in inlAnalyzeInlineeArgs.

        if ((argInfo.paramLclNum != BAD_VAR_NUM) || (argNode->GetType() != argInfo.paramType))
        {
            var_types paramType = argInfo.paramType;

            if (!lvaGetDesc(lclNum)->lvNormalizeOnLoad())
            {
                paramType = varActualType(paramType);
            }

            argNode = gtNewLclvNode(lclNum, paramType);
        }

        argInfo.paramLclNum = lclNum;

        return argNode;
    }

    if (argInfo.paramHasLcl)
    {
        // We already allocated a temp for this argument, use it.

        argNode = gtNewLclvNode(argInfo.paramLclNum, varActualType(argInfo.paramType));

        // This is the second or later use of the this argument,
        // so we have to use the temp (instead of the actual arg).
        argInfo.paramSingleUse = nullptr;

        return argNode;
    }

    // Argument is a complex expression - it must be evaluated into a temp.

    unsigned   tmpLclNum = lvaGrabTemp(true DEBUGARG("inlinee arg"));
    LclVarDsc* tmpLcl    = lvaGetDesc(tmpLclNum);

    if (argInfo.paramIsAddressTaken)
    {
        tmpLcl->lvHasLdAddrOp = 1;
    }

    if (varTypeIsStruct(argInfo.paramType))
    {
        lvaSetStruct(tmpLclNum, argInfo.paramClass, /* unsafeValueClsCheck */ true);
    }
    else
    {
        tmpLcl->SetType(argInfo.paramType);

        if (argInfo.paramType == TYP_REF)
        {
            if (!argInfo.paramIsAddressTaken && !argInfo.paramHasStores)
            {
                // If the arg can't be modified in the method body, use the type of the value,
                // if known. Otherwise, use the declared type.

                assert(tmpLcl->lvSingleDef == 0);
                tmpLcl->lvSingleDef = 1;

                JITDUMP("Marked V%02u as a single def temp\n", tmpLclNum);

                lvaSetClass(tmpLclNum, argInfo.argNode, argInfo.paramClass);
            }
            else
            {
                // Arg might be modified, use the declared type of the argument.

                lvaSetClass(tmpLclNum, argInfo.paramClass);
            }
        }
        else if (argInfo.paramClass != NO_CLASS_HANDLE)
        {
            // This is a "normed type", we need to set lvImpTypeInfo to preserve the struct handle.

            // Make sure we didn't get a handle for a BYREF, it wouldn't be a normed type.
            assert(argInfo.paramType != TYP_BYREF);

            tmpLcl->lvImpTypeInfo = typeInfo(TI_STRUCT, argInfo.paramClass);
        }
    }

    argInfo.paramHasLcl = true;
    argInfo.paramLclNum = tmpLclNum;

    // If we require strict exception order, then arguments must
    // be evaluated in sequence before the body of the inlined method.
    // So we need to evaluate them to a temp.
    // Also, if arguments have global or local references, we need to
    // evaluate them to a temp before the inlined body as the
    // inlined body may be modifying the global ref.
    //
    // TODO-1stClassStructs: We currently do not reuse an existing lclVar
    // if it is a struct, because it requires some additional handling.

    if (varTypeIsStruct(argInfo.paramType) || argInfo.argHasSideEff || argInfo.argHasGlobRef ||
        argInfo.paramIsAddressTaken || argInfo.paramHasStores)
    {
        argNode = gtNewLclvNode(tmpLclNum, varActualType(argInfo.paramType));
    }
    else
    {
        // Allocate a large LCL_VAR node so we can replace it with any
        // other node if it turns out to be single use.

        argNode = gtNewLclLNode(tmpLclNum, varActualType(argInfo.paramType));

        // Record argNode as the very first use of this argument.
        // If there are no further uses of the arg, we may be
        // able to use the actual arg node instead of the temp.
        // If we do see any further uses, we will clear this.

        argInfo.paramSingleUse = argNode;
    }

    return argNode;
}

void Compiler::inlInsertInlineeCode(InlineInfo* inlineInfo)
{
    GenTreeCall* call = inlineInfo->iciCall;

    JITDUMP("\n---- Statements (and blocks) added due to the inlining of call " FMT_TREEID " ----\n", call->GetID());

    InlineContext* inlineContext = m_inlineStrategy->NewSuccess(inlineInfo);

    for (BasicBlock* block = InlineeCompiler->fgFirstBB; block != nullptr; block = block->bbNext)
    {
        for (Statement* stmt : block->Statements())
        {
            stmt->SetInlineContext(inlineContext);
        }
    }

    Statement* stmtAfter = inlPrependStatements(inlineInfo);

    if ((InlineeCompiler->fgFirstBB->bbJumpKind == BBJ_RETURN) && (InlineeCompiler->fgFirstBB->bbNext == nullptr))
    {
        stmtAfter = inlInsertSingleBlockInlineeStatements(inlineInfo, stmtAfter);
        inlNullOutInlineeGCLocals(inlineInfo, stmtAfter);
    }
    else
    {
        Statement* insertAfter = stmtAfter;
        inlNullOutInlineeGCLocals(inlineInfo, stmtAfter);
        inlInsertInlineeBlocks(inlineInfo, insertAfter);
    }

    inlPropagateInlineeCompilerState();

    // Record the return expression of non-void methods in the RET_EXPR node.
    if (inlineInfo->iciCall->GetRetSigType() != TYP_VOID)
    {
        noway_assert(inlineInfo->retExpr != nullptr);

        JITDUMPTREE(inlineInfo->retExpr, "---- Return expression for placeholder " FMT_TREEID " ----\n",
                    call->gtInlineCandidateInfo->retExprPlaceholder->GetID());

        call->gtInlineCandidateInfo->retExprPlaceholder->SetRetExpr(inlineInfo->retExpr, inlineInfo->retBlockIRSummary);
    }
}

Statement* Compiler::inlInsertSingleBlockInlineeStatements(const InlineInfo* inlineInfo, Statement* stmtAfter)
{
    JITDUMP("---- Single block inlinee statements ----\n");

    BasicBlock* block        = inlineInfo->iciBlock;
    BasicBlock* inlineeBlock = InlineeCompiler->fgFirstBB;

    assert(inlineeBlock->bbJumpKind == BBJ_RETURN);
    assert(inlineeBlock->bbNext == nullptr);

    if (inlineeBlock->GetFirstStatement() == nullptr)
    {
        JITDUMP("Inlinee method has no statements.\n");
    }
    else
    {
#ifdef DEBUG
        if (verbose)
        {
            for (Statement* stmt : inlineeBlock->Statements())
            {
                gtDispStmt(stmt);
            }
        }
#endif

        stmtAfter = fgInsertStmtListAfter(block, stmtAfter, inlineeBlock->GetFirstStatement());
    }

    uint64_t inlineeBlockFlags = inlineeBlock->bbFlags;
    noway_assert((inlineeBlockFlags & BBF_HAS_JMP) == 0);
    noway_assert((inlineeBlockFlags & BBF_KEEP_BBJ_ALWAYS) == 0);
    // TODO-MIKE-Fix: This should only add BBF_IR_SUMMARY flags.
    block->bbFlags |= (inlineeBlockFlags & ~BBF_RUN_RARELY);

    return stmtAfter;
}

BasicBlock* Compiler::inlSplitInlinerBlock(const InlineInfo* inlineInfo, Statement* stmtAfter)
{
    BasicBlock* callBlock = inlineInfo->iciBlock;

    BasicBlock* returnTargetBlock = fgNewBBafter(callBlock->bbJumpKind, callBlock, true);
    returnTargetBlock->bbRefs     = 1;
    returnTargetBlock->bbJumpDest = callBlock->bbJumpDest;
    returnTargetBlock->inheritWeight(callBlock);

    callBlock->bbJumpKind = BBJ_NONE;

    // Update block flags
    {
        const uint64_t originalFlags = callBlock->bbFlags;
        noway_assert((originalFlags & BBF_SPLIT_NONEXIST) == 0);
        callBlock->bbFlags &= ~BBF_SPLIT_LOST;
        returnTargetBlock->bbFlags |= originalFlags & BBF_SPLIT_GAINED;
    }

    if (stmtAfter->GetNextStmt() != nullptr)
    {
        Statement* topBlockFirstStmt    = callBlock->GetFirstStatement();
        Statement* topBlockLastStmt     = stmtAfter;
        Statement* bottomBlockFirstStmt = stmtAfter->GetNextStmt();
        Statement* bottomBlockLastStmt  = callBlock->GetLastStatement();

        topBlockLastStmt->SetNextStmt(nullptr);
        bottomBlockFirstStmt->SetPrevStmt(nullptr);

        callBlock->SetLastStatement(topBlockLastStmt);
        returnTargetBlock->SetStatements(bottomBlockFirstStmt, bottomBlockLastStmt);
    }

    return returnTargetBlock;
}

void Compiler::inlInsertInlineeBlocks(const InlineInfo* inlineInfo, Statement* stmtAfter)
{
    JITDUMP("---- Inlinee basic blocks ----\n");

    assert((InlineeCompiler->fgBBcount > 1) || (InlineeCompiler->fgFirstBB->bbJumpKind != BBJ_RETURN));

    BasicBlock* returnTargetBlock = inlSplitInlinerBlock(inlineInfo, stmtAfter);
    IL_OFFSETX  ilOffset          = inlineInfo->iciStmt->GetILOffsetX();
    bool        inheritWeight     = true; // The firstBB does inherit the weight from the call block

    for (BasicBlock* block = InlineeCompiler->fgFirstBB; block != nullptr; block = block->bbNext)
    {
        // Methods that contain exception handling are never inlined.
        noway_assert(!block->hasTryIndex());
        noway_assert(!block->hasHndIndex());

        block->copyEHRegion(inlineInfo->iciBlock);
        block->bbFlags |= inlineInfo->iciBlock->bbFlags & BBF_BACKWARD_JUMP;

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
                JITDUMP("Convert return block " FMT_BB " to jump to " FMT_BB "\n", block->bbNum,
                        returnTargetBlock->bbNum);

                block->bbJumpKind = BBJ_ALWAYS;
                block->bbJumpDest = returnTargetBlock;
            }
            else
            {
                JITDUMP("Convert return block " FMT_BB " to fall through to " FMT_BB "\n", block->bbNum,
                        returnTargetBlock->bbNum);

                block->bbJumpKind = BBJ_NONE;
            }
        }

        // Update profile weight for callee blocks, if we didn't do it already.
        if (inlineInfo->profileScaleState != InlineInfo::ProfileScaleState::KNOWN)
        {
            if (inheritWeight)
            {
                block->inheritWeight(inlineInfo->iciBlock);
                inheritWeight = false;
            }
            else
            {
                block->modifyBBWeight(inlineInfo->iciBlock->bbWeight / 2);
            }
        }
    }

    // Insert inlinee's blocks into inliner's block list.
    inlineInfo->iciBlock->setNext(InlineeCompiler->fgFirstBB);
    InlineeCompiler->fgLastBB->setNext(returnTargetBlock);
    fgBBcount += InlineeCompiler->fgBBcount;

    DBEXEC(verbose, fgDispBasicBlocks(InlineeCompiler->fgFirstBB, InlineeCompiler->fgLastBB, true));
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

Statement* Compiler::inlPrependStatements(InlineInfo* inlineInfo)
{
    GenTree* nullCheckThisArg = nullptr;

    if (((inlineInfo->iciCall->gtFlags & GTF_CALL_NULLCHECK) != 0) && !inlineInfo->thisDereferencedFirst)
    {
        // We'll have to null check the "this" arg after inlinee args are initialized.
        // But args initialization needs to know about arg uses so we have to get the
        // "this" arg here, before calling inlInitInlineeArgs.

        nullCheckThisArg = inlUseArg(inlineInfo, 0);

        if (!fgAddrCouldBeNull(nullCheckThisArg))
        {
            nullCheckThisArg = nullptr;
        }
    }

    Statement* afterStmt = inlInitInlineeArgs(inlineInfo, inlineInfo->iciStmt);

    if ((inlineInfo->inlineCandidateInfo->initClassResult & CORINFO_INITCLASS_USE_HELPER) != 0)
    {
        // Add the static field initialization check if needed.
        // This might be redundant, a static field of this type could be accessed
        // in the inlinee body before any other observable side-effect.

        CORINFO_CLASS_HANDLE exactClass = eeGetClassFromContext(inlineInfo->inlineCandidateInfo->exactContextHnd);

        GenTree*   tree = fgGetSharedCCtor(exactClass);
        Statement* stmt = gtNewStmt(tree, inlineInfo->iciStmt->GetILOffsetX());
        fgInsertStmtAfter(inlineInfo->iciBlock, afterStmt, stmt);
        afterStmt = stmt;

        DBEXEC(verbose, gtDispStmt(stmt));
    }

    if (nullCheckThisArg != nullptr)
    {
        GenTree*   tree = gtNewNullCheck(nullCheckThisArg, inlineInfo->iciBlock);
        Statement* stmt = gtNewStmt(tree, inlineInfo->iciStmt->GetILOffsetX());
        fgInsertStmtAfter(inlineInfo->iciBlock, afterStmt, stmt);
        afterStmt = stmt;

        DBEXEC(verbose, gtDispStmt(stmt));
    }

    afterStmt = inlInitInlineeLocals(inlineInfo, afterStmt);

    return afterStmt;
}

Statement* Compiler::inlInitInlineeArgs(const InlineInfo* inlineInfo, Statement* afterStmt)
{
    JITDUMP("---- Init inlinee args ----\n");

    if (inlineInfo->ilArgCount == 0)
    {
        JITDUMP("Inlinee has no args.\n");
        return afterStmt;
    }

    for (unsigned argNum = 0; argNum < inlineInfo->ilArgCount; argNum++)
    {
        const InlArgInfo& argInfo = inlineInfo->ilArgInfo[argNum];
        GenTree*          argNode = argInfo.argNode;

        // MKREFANY args currently fail inlining, RET_EXPR should have been replaced already.
        assert(!argNode->OperIs(GT_MKREFANY, GT_RET_EXPR));

        if ((argInfo.paramSingleUse != nullptr) && ((argInfo.paramSingleUse->gtFlags & GTF_VAR_CLONED) == 0))
        {
            JITDUMP("Argument %u is single use\n", argNum);

            // paramSingleUse is set iff the argument's value was referenced exactly once
            // in the inlinee. This offers an opportunity to avoid a temp and just use the
            // original argument tree.
            //
            // It's possible for additional uses of the agument to appear without inlUseArg
            // being called (e.g. when handling isinst or dup) in which case this replacement
            // cannot be done. This relies on GTF_VAR_CLONED being set on LCL_VARs when they
            // are cloned to detect such cases, that means the importer is expected to not
            // "manually" clone LCL_VARs by doing gtNewLclvNode(existingLcl->GetLclNum()...).

            assert(!varTypeIsStruct(argNode->GetType()) && !argInfo.argHasGlobRef && !argInfo.argHasSideEff &&
                   !argInfo.paramIsAddressTaken && !argInfo.paramHasStores);

            argInfo.paramSingleUse->ReplaceWith(argNode, this);

            // TODO-MIKE-Fix: This moves the argument tree to some inlinee block,
            // it should copy BBF_IR_SUMMARY flags from the inliner call block.
            // However, we don't know which inlinee block the arg is moved to.
            // It doesn't seem worthwhile tracking the inlinee block just for
            // this - most of the BBF_IR_SUMMARY flags are associated with trees
            // that have side effects and we don't move those. BBF_HAS_NEWOBJ
            // is associated with ALLOCOBJ and this node doesn't have side effects
            // but then it's unlikely for an ALLOCOBJ to be an argument to a call.
            // Normally the result of ALLOCOBJ is stored to a temp because it has
            // to be passed to the constructor.

            continue;
        }

        if (argInfo.paramHasLcl)
        {
            GenTree* asg;

            if (argInfo.paramType != TYP_STRUCT)
            {
                asg = gtNewAssignNode(gtNewLclvNode(argInfo.paramLclNum, argInfo.paramType), argNode);

                if (varTypeIsSIMD(argInfo.paramType))
                {
                    gtInitStructCopyAsg(asg->AsOp());
                }
            }
            else
            {
                CORINFO_CLASS_HANDLE argClass = gtGetStructHandleIfPresent(argNode);
                noway_assert(argClass != NO_CLASS_HANDLE);

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

                LclVarDsc*   paramLcl      = lvaGetDesc(argInfo.paramLclNum);
                ClassLayout* paramLayout   = paramLcl->GetLayout();
                bool         restoreLayout = false;

                if (argClass != paramLayout->GetClassHandle())
                {
                    assert(info.compCompHnd->getClassSize(argClass) == paramLayout->GetSize());

                    paramLcl->SetLayout(typGetObjLayout(argClass));
                    restoreLayout = true;
                }

                // The argument cannot be a COMMA, impCanonicalizeStructCallArg should have changed
                // it to OBJ(COMMA(...)).
                // It also cannot be MKREFANY because TypedReference parameters block
                // inlining. That's probably an unnecessary limitation but who cares
                // about TypedReference?
                // This means that impAssignStructPtr won't have to add new statements,
                // it cannot do that since we're not actually importing IL.

                assert(!argNode->OperIs(GT_COMMA));

                GenTree* dst     = gtNewLclvNode(argInfo.paramLclNum, argInfo.paramType);
                GenTree* dstAddr = gtNewAddrNode(dst, TYP_BYREF);
                asg              = impAssignStructPtr(dstAddr, argNode, argClass, CHECK_SPILL_NONE);

                if (restoreLayout)
                {
                    paramLcl->SetLayout(paramLayout);
                }
            }

            Statement* stmt = gtNewStmt(asg, inlineInfo->iciStmt->GetILOffsetX());
            stmt->SetInlineContext(inlineInfo->iciStmt->GetInlineContext());
            fgInsertStmtAfter(inlineInfo->iciBlock, afterStmt, stmt);
            afterStmt = stmt;

            JITDUMP("Argument %u init\n", argNum);
            DBEXEC(verbose, gtDispStmt(stmt));

            continue;
        }

        if (argInfo.argIsInvariant || argInfo.argIsUnaliasedLclVar)
        {
            JITDUMP("Argument %u is invariant/unaliased local\n", argNum);

            assert(argNode->OperIsConst() || argNode->OperIs(GT_ADDR, GT_LCL_VAR));
            assert(!argInfo.paramIsAddressTaken && !argInfo.paramHasStores && !argInfo.argHasGlobRef);

            continue;
        }

        if (argInfo.argHasSideEff)
        {
            // This parameter isn't used. We need to preserve argument side effects though.

            GenTree* sideEffects = nullptr;

            if (argNode->OperIs(GT_OBJ))
            {
                GenTree* addr = argNode->AsObj()->GetAddr();

                if (fgAddrCouldBeNull(addr))
                {
                    sideEffects = gtNewNullCheck(addr, inlineInfo->iciBlock);
                }
                else
                {
                    sideEffects = gtUnusedValNode(addr);
                }
            }
            else
            {
                if (!inlCanDiscardArgSideEffects(argNode))
                {
                    sideEffects = gtUnusedValNode(argNode);
                }
            }

            if (sideEffects != nullptr)
            {
                Statement* stmt = gtNewStmt(sideEffects, inlineInfo->iciStmt->GetILOffsetX());
                stmt->SetInlineContext(inlineInfo->iciStmt->GetInlineContext());
                fgInsertStmtAfter(inlineInfo->iciBlock, afterStmt, stmt);
                afterStmt = stmt;

                JITDUMP("Argument %u is not used, keeping side effects\n", argNum);
                DBEXEC(verbose, gtDispStmt(stmt));
            }
            else
            {
                JITDUMP("Argument %u is not used, discarding side effects\n", argNum);
            }

            continue;
        }

        JITDUMP("Argument %u is not used\n", argNum);

        if (argNode->IsBox())
        {
            // BOX doesn't have side effects and can be removed and there's
            // more code associated with it that could be removed as well.
            gtTryRemoveBoxUpstreamEffects(argNode);

            continue;
        }
    }

    return afterStmt;
}

bool Compiler::inlCanDiscardArgSideEffects(GenTree* argNode)
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

            return true;
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
                JITDUMP("\nPerforming special dce on unused arg [%06u]: helper call [%06u]\n", argNode->GetID(),
                        op1->GetID());

                return true;
            }
        }
    }

    return false;
}

Statement* Compiler::inlInitInlineeLocals(const InlineInfo* inlineInfo, Statement* afterStmt)
{
    JITDUMP("---- Init inlinee locals ----\n");

    if (inlineInfo->ilLocCount == 0)
    {
        JITDUMP("Inlinee has no locals.\n");
        return afterStmt;
    }

    if ((inlineInfo->inlineCandidateInfo->methInfo.options & CORINFO_OPT_INIT_LOCALS) == 0)
    {
        JITDUMP("Inlinee does not require locals initialization.\n");
        return afterStmt;
    }

    // If the callee contains zero-init locals, we need to explicitly initialize them if we are
    // in a loop or if the caller doesn't have compInitMem set. Otherwise we can rely on the
    // normal logic in the caller to insert zero-init in the prolog if necessary.

    bool blockIsInLoop = (inlineInfo->iciBlock->bbFlags & BBF_BACKWARD_JUMP) != 0;
    bool blockIsReturn = inlineInfo->iciBlock->bbJumpKind == BBJ_RETURN;

    if (info.compInitMem && (!blockIsInLoop || blockIsReturn))
    {
        JITDUMP("Skipping, inliner will initialize inlinee locals.\n");
        return afterStmt;
    }

    for (unsigned i = 0; i < inlineInfo->ilLocCount; i++)
    {
        const InlLocInfo& lclInfo = inlineInfo->ilLocInfo[i];

        if (!lclInfo.lclIsUsed)
        {
            continue;
        }

        LclVarDsc* lcl = lvaGetDesc(lclInfo.lclNum);

        if (!fgVarNeedsExplicitZeroInit(lclInfo.lclNum, blockIsInLoop, blockIsReturn))
        {
            JITDUMP("Suppressing zero-init for V%02u, expect to zero in inliner's prolog\n", lclInfo.lclNum);
            lcl->lvSuppressedZeroInit = 1;
            compSuppressedZeroInit    = true;
            continue;
        }

        var_types  lclType = lcl->GetType();
        GenTree*   zero    = varTypeIsStruct(lclType) ? gtNewIconNode(0) : gtNewZeroConNode(lclType);
        GenTreeOp* asg     = gtNewAssignNode(gtNewLclvNode(lclInfo.lclNum, lclType), zero);
        Statement* stmt    = gtNewStmt(asg, inlineInfo->iciStmt->GetILOffsetX());
        fgInsertStmtAfter(inlineInfo->iciBlock, afterStmt, stmt);
        afterStmt = stmt;

        JITDUMP("Init inlinee local %u\n", i);
        DBEXEC(verbose, gtDispStmt(stmt));
    }

    return afterStmt;
}

void Compiler::inlNullOutInlineeGCLocals(const InlineInfo* inlineInfo, Statement* stmtAfter)
{
    JITDUMP("---- Null out inlinee GC locals ----\n");

    if (!inlineInfo->hasGCRefLocals)
    {
        JITDUMP("Inlinee has no GC locals.\n");
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

    for (unsigned i = 0; i < inlineInfo->ilLocCount; i++)
    {
        const InlLocInfo& lclInfo = inlineInfo->ilLocInfo[i];

        if (!varTypeIsGC(lclInfo.lclType) || !lclInfo.lclIsUsed)
        {
            continue;
        }

        assert(lvaGetDesc(lclInfo.lclNum)->GetType() == lclInfo.lclType);

        if (inlineInfo->retExpr != nullptr)
        {
            // Does the local we're about to null out appear in the return
            // expression? If so we somehow messed up and didn't properly
            // spill the return value. See inlFetchInlineeLocal.

            noway_assert(!gtHasRef(inlineInfo->retExpr, lclInfo.lclNum));
        }

        GenTree*   zero = gtNewZeroConNode(lclInfo.lclType);
        GenTree*   asg  = gtNewAssignNode(gtNewLclvNode(lclInfo.lclNum, lclInfo.lclType), zero);
        Statement* stmt = gtNewStmt(asg, inlineInfo->iciStmt->GetILOffsetX());
        fgInsertStmtAfter(inlineInfo->iciBlock, stmtAfter, stmt);
        stmtAfter = stmt;

        JITDUMP("Null out inlinee local %u\n", i);
        DBEXEC(verbose, gtDispStmt(stmt));
    }
}
