// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

// The IndirectCallTransformer transforms indirect calls that involve fat function
// pointers, guarded devirtualization candidates, or runtime lookup with dynamic dictionary expansion.
// These transformations introduce control flow and so can't easily be done in the importer.
class IndirectCallTransformer
{
    Compiler* compiler;

public:
    IndirectCallTransformer(Compiler* compiler) : compiler(compiler)
    {
    }

    unsigned Run()
    {
        unsigned count = 0;

        for (BasicBlock* const block : compiler->Blocks())
        {
            count += TransformBlock(block);
        }

        return count;
    }

private:
    unsigned TransformBlock(BasicBlock* block)
    {
        unsigned count = 0;

        for (Statement* const stmt : block->Statements())
        {
            GenTree* node = stmt->GetRootNode();

            if (node->OperIs(GT_LCL_STORE))
            {
                node = node->AsLclStore()->GetValue();
            }

            if (GenTreeCall* call = node->IsCall())
            {
                if (call->IsFatPointerCandidate())
                {
                    FatPointerCallTransformer transformer(compiler, block, stmt, call);
                    transformer.Run();
                    count++;
                }
                else if (call->IsGuardedDevirtualizationCandidate())
                {
                    GuardedDevirtualizationTransformer transformer(compiler, block, stmt, call);
                    transformer.Run();
                    count++;
                }
                else if (call->IsExpRuntimeLookup())
                {
                    ExpRuntimeLookupTransformer transformer(compiler, block, stmt, call);
                    transformer.Run();
                    count++;
                }
            }
        }

        return count;
    }

    class Transformer
    {
    protected:
        static constexpr int HIGH_PROBABILITY = 80;

        Compiler*    compiler;
        BasicBlock*  currBlock;
        Statement*   stmt;
        GenTreeCall* origCall;

    public:
        Transformer(Compiler* compiler, BasicBlock* block, Statement* stmt, GenTreeCall* call)
            : compiler(compiler), currBlock(block), stmt(stmt), origCall(call)
        {
            assert(!compiler->fgComputePredsDone);
        }

    protected:
        BasicBlock* CreateBasicBlock(BBjumpKinds jumpKind, BasicBlock* insertAfter)
        {
            BasicBlock* block = compiler->fgNewBBafter(jumpKind, insertAfter, true);
            block->bbFlags |= BBF_IMPORTED;
            return block;
        }
    };

    // A fat function pointer is a pointer with the second least significant bit
    // (aka FAT_POINTER_MASK) set. If the bit is set, the pointer (after clearing the bit)
    // actually points to a tuple <method pointer, instantiation argument pointer> where
    // instantiationArgument is a hidden first argument required by method pointer.
    //
    // Fat pointers are used in CoreRT as a replacement for instantiating stubs,
    // because CoreRT can't generate stubs in runtime.
    //
    // The JIT is responsible for emitting code to check the bit at runtime, branching
    // to one of two call sites.
    //
    // When the bit is not set, the code should execute the original indirect call.
    //
    // When the bit is set, the code should mask off the bit, use the resulting pointer
    // to load the real target address and the extra argument, and then call indirect
    // via the target, passing the extra argument.
    //
    class FatPointerCallTransformer final : public Transformer
    {
        static constexpr ssize_t FAT_POINTER_MASK = 0x2;

        BasicBlock* callBlock;
        BasicBlock* fatCallBlock;
        BasicBlock* remainderBlock;

    public:
        FatPointerCallTransformer(Compiler* compiler, BasicBlock* block, Statement* stmt, GenTreeCall* call)
            : Transformer(compiler, block, stmt, call)
        {
        }

        void Run()
        {
            JITDUMP("*** FatPointerCall: transforming " FMT_TREEID "\n", origCall->GetID());

            origCall->ClearFatPointerCandidate();

            remainderBlock = compiler->fgSplitBlockAfterStatement(currBlock, stmt);
            remainderBlock->bbFlags |= BBF_INTERNAL;
            remainderBlock->inheritWeight(currBlock);

            compiler->fgRemoveStmt(currBlock, stmt);
            currBlock->bbJumpKind = BBJ_COND;
            compiler->fgInsertStmtAtEnd(currBlock, CreateFatAddrCheck());

            callBlock = CreateBasicBlock(BBJ_ALWAYS, currBlock);
            callBlock->inheritWeightPercentage(currBlock, HIGH_PROBABILITY);
            callBlock->bbJumpDest = remainderBlock;
            compiler->fgInsertStmtAtEnd(callBlock, stmt);

            fatCallBlock = CreateBasicBlock(BBJ_NONE, callBlock);
            fatCallBlock->inheritWeightPercentage(currBlock, 100 - HIGH_PROBABILITY);
            compiler->fgInsertStmtAtEnd(fatCallBlock, CreateFatCall());
            currBlock->bbJumpDest = fatCallBlock;
        }

    private:
        Statement* CreateFatAddrCheck() const
        {
            GenTree* fatAddr = compiler->gtCloneExpr(origCall->GetCallAddr());
            GenTree* fatMask = compiler->gtNewIconNode(FAT_POINTER_MASK, TYP_I_IMPL);
            GenTree* fatTest = compiler->gtNewOperNode(GT_AND, TYP_I_IMPL, fatAddr, fatMask);
            GenTree* zero    = compiler->gtNewIconNode(0, TYP_I_IMPL);
            GenTree* fatCmp  = compiler->gtNewOperNode(GT_NE, TYP_INT, fatTest, zero);
            GenTree* fatJump = compiler->gtNewOperNode(GT_JTRUE, TYP_VOID, fatCmp);

            return compiler->gtNewStmt(fatJump, stmt->GetILOffsetX());
        }

        Statement* CreateFatCall() const
        {
            GenTree* fatAddr      = compiler->gtCloneExpr(origCall->GetCallAddr());
            GenTree* fatMask      = compiler->gtNewIconNode(FAT_POINTER_MASK, TYP_I_IMPL);
            GenTree* realAddr     = compiler->gtNewOperNode(GT_SUB, TYP_I_IMPL, fatAddr, fatMask);
            GenTree* realCallAddr = compiler->gtNewIndLoad(TYP_I_IMPL, realAddr);
            realAddr              = compiler->gtCloneExpr(realAddr);

            GenTree* pointerSize    = compiler->gtNewIconNode(TARGET_POINTER_SIZE, TYP_I_IMPL);
            GenTree* hiddenArgument = compiler->gtNewOperNode(GT_ADD, TYP_I_IMPL, realAddr, pointerSize);
            hiddenArgument          = compiler->gtNewIndLoad(TYP_I_IMPL, hiddenArgument);
            hiddenArgument          = compiler->gtNewIndLoad(TYP_I_IMPL, hiddenArgument);

            // TODO-MIKE-Review: This is cloning arbitrarily large trees. And CSE can't even
            // eliminating this redundancy, this requires hoisting, that the JIT doesn't have.
            GenTreeCall* fatCall = compiler->gtCloneExpr(origCall)->AsCall();
            fatCall->SetCallAddr(realCallAddr);
            AddHiddenArgument(fatCall, hiddenArgument);

            GenTree* fatTree = fatCall;

            if (GenTreeLclStore* store = stmt->GetRootNode()->IsLclStore())
            {
                fatTree = compiler->gtNewLclStore(store->GetLcl(), store->GetType(), fatCall);
            }
            else
            {
                assert(stmt->GetRootNode() == origCall);
            }

            return compiler->gtNewStmt(fatTree, stmt->GetILOffsetX());
        }

        void AddHiddenArgument(GenTreeCall* fatCall, GenTree* hiddenArgument) const
        {
            assert(!fatCall->IsUnmanaged());

#ifdef TARGET_X86
            compiler->gtAppendNewCallArg(fatCall, hiddenArgument);
#else
            if (fatCall->HasRetBufArg())
            {
                compiler->gtInsertNewCallArgAfter(hiddenArgument, fatCall->GetRetBufArg());
            }
            else if (fatCall->HasThisArg())
            {
                compiler->gtInsertNewCallArgAfter(hiddenArgument, fatCall->GetThisArg());
            }
            else
            {
                compiler->gtPrependNewCallArg(fatCall->m_args, hiddenArgument);
            }
#endif
        }
    };

    class GuardedDevirtualizationTransformer final : public Transformer
    {
        LclVarDsc*  returnTemp = nullptr;
        Statement*  lastStmt;
        BasicBlock* remainderBlock = nullptr;
        BasicBlock* checkBlock     = nullptr;
        BasicBlock* thenBlock      = nullptr;
        BasicBlock* elseBlock      = nullptr;
        unsigned    likelihood     = HIGH_PROBABILITY;

    public:
        GuardedDevirtualizationTransformer(Compiler* compiler, BasicBlock* block, Statement* stmt, GenTreeCall* call)
            : Transformer(compiler, block, stmt, call)
        {
        }

        void Run()
        {
            JITDUMP("\n----------------\n\n*** GuardedDevirtualization contemplating [%06u] in " FMT_BB " \n",
                    origCall->GetID(), currBlock->bbNum);

            InlineCandidateInfo* inlineInfo = origCall->HasInlinedCandidateInfo();

            // We currently need inline candidate info to guarded devirtualization.
            if (inlineInfo == nullptr)
            {
                JITDUMP("*** Bailing on [%06u] -- not an inline node\n", origCall->GetID());
                ClearFlag();
                return;
            }

            likelihood = inlineInfo->likelihood;
            assert((likelihood >= 0) && (likelihood <= 100));
            JITDUMP("Likelihood of correct guess is %u\n", likelihood);

            const bool isChainedGdv = (origCall->gtCallMoreFlags & GTF_CALL_M_GUARDED_DEVIRT_CHAIN) != 0;

            if (isChainedGdv)
            {
                JITDUMP("Expansion will chain to the previous GDV\n");
            }

            Transform();

            if (isChainedGdv)
            {
                TransformForChainedGdv();
            }

            // Look ahead and see if there's another GDV we might chain to this one.
            ScoutForChainedGdv();
        }

        void Transform()
        {
            FixupRetExpr();
            ClearFlag();
            SplitBlock();
            CreateCheck();
            CreateThen();
            CreateElse();
            compiler->fgRemoveStmt(currBlock, stmt);
            SetWeights();
            ChainFlow();
        }

    private:
        void SplitBlock()
        {
            remainderBlock = compiler->fgSplitBlockAfterStatement(currBlock, stmt);
            remainderBlock->bbFlags |= BBF_INTERNAL;
        }

        void SetWeights()
        {
            remainderBlock->inheritWeight(currBlock);
            checkBlock->inheritWeight(currBlock);
            thenBlock->inheritWeightPercentage(currBlock, likelihood);
            elseBlock->inheritWeightPercentage(currBlock, 100 - likelihood);
        }

        void ChainFlow()
        {
            checkBlock->bbJumpDest = elseBlock;
            thenBlock->bbJumpDest  = remainderBlock;
        }

        void ClearFlag()
        {
            origCall->ClearGuardedDevirtualizationCandidate();
        }

        void CreateCheck()
        {
            // There's no need for a new block here. We can just append to currBlock.
            checkBlock             = currBlock;
            checkBlock->bbJumpKind = BBJ_COND;

            GenTree*   thisTree = origCall->GetThisArg()->GetNode();
            LclVarDsc* thisLcl;

            if (thisTree->OperIs(GT_LCL_LOAD))
            {
                // TODO-MIKE-Review: Is it safe to use an existing LCL_VAR if it ends up being address
                // exposed? An address exposed local could be modified by a different thread. That's
                // extremely unusual but if it happens then type safety is compromised. And anyway,
                // using a DNER local will result in poor codegen.

                thisLcl = thisTree->AsLclLoad()->GetLcl();
            }
            else
            {
                thisLcl = compiler->lvaNewTemp(TYP_REF, true DEBUGARG("guarded devirt this temp"));

                GenTree* thisStore = compiler->gtNewLclStore(thisLcl, TYP_REF, thisTree);
                compiler->fgInsertStmtAtEnd(checkBlock, compiler->gtNewStmt(thisStore, stmt->GetILOffsetX()));
                origCall->GetThisArg()->SetNode(compiler->gtNewLclLoad(thisLcl, TYP_REF));
            }

            thisTree = compiler->gtNewLclLoad(thisLcl, TYP_REF);

            // Remember the current last statement. If we're doing a chained GDV, we'll clone/copy
            // all the code in the check block up to and including this statement.
            //
            // Note it's important that we clone/copy the temp store above, if we created one,
            // because flow along the "cold path" is going to bypass the check block.
            lastStmt = checkBlock->lastStmt();

            // Find target method table
            GenTree*                              methodTable       = compiler->gtNewMethodTableLookup(thisTree);
            GuardedDevirtualizationCandidateInfo* guardedInfo       = origCall->gtGuardedDevirtualizationCandidateInfo;
            CORINFO_CLASS_HANDLE                  clsHnd            = guardedInfo->guardedClassHandle;
            GenTree*                              targetMethodTable = compiler->gtNewIconEmbClsHndNode(clsHnd);

            // Compare and jump to else (which does the indirect call) if NOT equal
            GenTree*   methodTableCompare = compiler->gtNewOperNode(GT_NE, TYP_INT, targetMethodTable, methodTable);
            GenTree*   jmpTree            = compiler->gtNewOperNode(GT_JTRUE, TYP_VOID, methodTableCompare);
            Statement* jmpStmt            = compiler->gtNewStmt(jmpTree, stmt->GetILOffsetX());
            compiler->fgInsertStmtAtEnd(checkBlock, jmpStmt);
        }

        void FixupRetExpr()
        {
            // If call returns a value, we need to copy it to a temp, and
            // bash the associated GT_RET_EXPR to refer to the temp instead
            // of the call.
            //
            // Note implicit by-ref returns should have already been converted
            // so any struct copy we induce here should be cheap.
            InlineCandidateInfo* const inlineInfo = origCall->gtInlineCandidateInfo;
            GenTreeRetExpr* const      retExpr    = inlineInfo->retExprPlaceholder;

            // Sanity check the ret expr if non-null: it should refer to the original call.
            if (retExpr != nullptr)
            {
                assert(retExpr->GetCall() == origCall);
            }

            if (!origCall->TypeIs(TYP_VOID))
            {
                // If there's a spill temp already associated with this inline candidate,
                // use that instead of allocating a new temp.
                returnTemp = inlineInfo->preexistingSpillTemp;

                LclVarDsc* returnTempLcl;

                if (returnTemp != nullptr)
                {
                    JITDUMP("Reworking call(s) to return value via a existing return temp V%02u\n",
                            returnTemp->GetLclNum());

                    // We will be introducing multiple defs for this temp, so make sure
                    // it is no longer marked as single def.
                    //
                    // Otherwise, we could make an incorrect type deduction. Say the
                    // original call site returns a B, but after we devirtualize along the
                    // GDV happy path we see that method returns a D. We can't then assume that
                    // the return temp is type D, because we don't know what type the fallback
                    // path returns. So we have to stick with the current type for B as the
                    // return type.

                    returnTempLcl = returnTemp;

                    if (returnTempLcl->lvSingleDef)
                    {
                        // In this case it's ok if we already updated the type assuming single def,
                        // we just don't want any further updates.
                        JITDUMP("Return temp V%02u is no longer a single def temp\n", returnTemp->GetLclNum());

                        returnTempLcl->lvSingleDef = false;
                    }
                }
                else
                {
                    returnTempLcl = compiler->lvaAllocTemp(false DEBUGARG("guarded devirt return temp"));
                    returnTemp    = returnTempLcl;

                    JITDUMP("Reworking call(s) to return value via a new temp V%02u\n", returnTemp->GetLclNum());
                }

                if (varTypeIsStruct(origCall->GetType()))
                {
                    compiler->lvaSetStruct(returnTempLcl, origCall->GetRetLayout(), false);

                    if (origCall->HasMultiRegRetVal())
                    {
                        returnTempLcl->lvIsMultiRegRet = true;
                    }
                }
                else
                {
                    returnTempLcl->SetType(origCall->GetType());
                }

                GenTreeLclLoad* tempTree = compiler->gtNewLclLoad(returnTemp, origCall->GetType());

                JITDUMP("Bashing GT_RET_EXPR [%06u] to refer to temp V%02u\n", retExpr->GetID(),
                        returnTemp->GetLclNum());

                retExpr->ReplaceWith(tempTree);
            }
            else if (retExpr != nullptr)
            {
                // We still oddly produce GT_RET_EXPRs for some void
                // returning calls. Just bash the ret expr to a NOP.
                //
                // Todo: consider bagging creation of these RET_EXPRs. The only possible
                // benefit they provide is stitching back larger trees for failed inlines
                // of void-returning methods. But then the calls likely sit in commas and
                // the benefit of a larger tree is unclear.
                JITDUMP("Bashing GT_RET_EXPR [%06u] for VOID return to NOP\n", retExpr->GetID());
                retExpr->ChangeToNothingNode();
            }
            else
            {
                // We do not produce GT_RET_EXPRs for CTOR calls, so there is nothing to patch.
            }
        }

        void CreateThen()
        {
            thenBlock = CreateBasicBlock(BBJ_ALWAYS, checkBlock);
            thenBlock->bbFlags |= currBlock->bbFlags & BBF_SPLIT_GAINED;

            InlineCandidateInfo* inlineInfo = origCall->gtInlineCandidateInfo;
            CORINFO_CLASS_HANDLE clsHnd     = inlineInfo->guardedClassHandle;

            // copy 'this' to temp with exact type.
            LclVarDsc* thisTempLcl = compiler->lvaNewTemp(TYP_REF, false DEBUGARG("guarded devirt this exact temp"));
            GenTree*   clonedObj   = compiler->gtCloneExpr(origCall->GetThisArg()->GetNode());
            GenTree*   clonedStore = compiler->gtNewLclStore(thisTempLcl, TYP_REF, clonedObj);
            compiler->lvaSetClass(thisTempLcl, clsHnd, true);
            compiler->fgNewStmtAtEnd(thenBlock, clonedStore);

            // Clone call. Note we must use the special candidate helper.
            GenTreeCall* call = compiler->gtCloneCandidateCall(origCall);
            call->GetThisArg()->SetNode(compiler->gtNewLclLoad(thisTempLcl, TYP_REF));
            call->SetIsGuarded();

            JITDUMP("Direct call [%06u] in block " FMT_BB "\n", call->GetID(), thenBlock->bbNum);

            // Then invoke impLateDevirtualizeCall to actually transform the call for us,
            // given the original (base) method and the exact guarded class. It should succeed.
            CORINFO_METHOD_HANDLE  methodHnd;
            CORINFO_CONTEXT_HANDLE context;
            compiler->impLateDevirtualizeCall(call, inlineInfo, &methodHnd, &context);

            // We know this call can devirtualize or we would not have set up GDV here.
            // So impLateDevirtualizeCall should succeed in devirtualizing.
            assert(!call->IsVirtual());

            // If the devirtualizer was unable to transform the call to invoke the unboxed entry, the inline info
            // we set up may be invalid. We won't be able to inline anyways. So demote the call as an inline candidate.
            CORINFO_METHOD_HANDLE unboxedMethodHnd = inlineInfo->guardedMethodUnboxedEntryHandle;

            if ((unboxedMethodHnd != nullptr) && (methodHnd != unboxedMethodHnd))
            {
                // Demote this call to a non-inline candidate
                JITDUMP("Devirtualization was unable to use the unboxed entry; so marking call (to boxed entry) as not "
                        "inlineable\n");

                call->RemoveInlineCandidateInfo();

                if (returnTemp != nullptr)
                {
                    GenTree* store = compiler->gtNewLclStore(returnTemp, call->GetType(), call);
                    compiler->fgNewStmtAtEnd(thenBlock, store);
                }
                else
                {
                    compiler->fgNewStmtAtEnd(thenBlock, call);
                }
            }
            else
            {
                // Add the call.
                compiler->fgNewStmtAtEnd(thenBlock, call);

                // Re-establish this call as an inline candidate.
                GenTreeRetExpr* oldRetExpr       = inlineInfo->retExprPlaceholder;
                inlineInfo->clsHandle            = compiler->info.compCompHnd->getMethodClass(methodHnd);
                inlineInfo->exactContextHnd      = context;
                inlineInfo->preexistingSpillTemp = returnTemp;
                call->gtInlineCandidateInfo      = inlineInfo;

                // If there was a ret expr for this call, we need to create a new one
                // and append it just after the call.
                //
                // Note the original GT_RET_EXPR has been bashed to a temp.
                // we set all this up in FixupRetExpr().
                if (oldRetExpr != nullptr)
                {
                    inlineInfo->retExprPlaceholder = compiler->gtNewRetExpr(call);

                    GenTree* retExpr = inlineInfo->retExprPlaceholder;

                    if (returnTemp != nullptr)
                    {
                        retExpr = compiler->gtNewLclStore(returnTemp, call->GetType(), retExpr);
                    }
                    else
                    {
                        // We should always have a return temp if we return results by value
                        assert(origCall->TypeIs(TYP_VOID));
                    }

                    compiler->fgNewStmtAtEnd(thenBlock, retExpr);
                }
            }
        }

        void CreateElse()
        {
            elseBlock = CreateBasicBlock(BBJ_NONE, thenBlock);
            elseBlock->bbFlags |= currBlock->bbFlags & BBF_SPLIT_GAINED;
            GenTreeCall* call    = origCall;
            Statement*   newStmt = compiler->gtNewStmt(call);

            call->RemoveInlineCandidateInfo();
            call->SetIsGuarded();

            JITDUMP("Residual call [%06u] moved to block " FMT_BB "\n", call->GetID(), elseBlock->bbNum);

            if (returnTemp != nullptr)
            {
                newStmt->SetRootNode(compiler->gtNewLclStore(returnTemp, call->GetType(), call));
            }

            compiler->fgInsertStmtAtEnd(elseBlock, newStmt);

            // Set the original statement to a nop.
            stmt->SetRootNode(compiler->gtNewNothingNode());
        }

        // For chained GDV, we modify the expansion as follows:
        //
        // We verify the check block has two BBJ_NONE/ALWAYS predecessors, one of
        // which (the "cold path") ends in a normal call, the other in an
        // inline candidate call.
        //
        // All the statements in the check block before the type test are copied to the
        // predecessor blocks (one via cloning, the other via direct copy).
        //
        // The cold path block is then modified to bypass the type test and jump
        // directly to the else block.
        void TransformForChainedGdv()
        {
            // Find the hot/cold predecessors. (Consider: just record these when we did the scouting).
            BasicBlock* const coldBlock = checkBlock->bbPrev;

            if (coldBlock->bbJumpKind != BBJ_NONE)
            {
                JITDUMP("Unexpected flow from cold path " FMT_BB "\n", coldBlock->bbNum);
                return;
            }

            BasicBlock* const hotBlock = coldBlock->bbPrev;

            if ((hotBlock->bbJumpKind != BBJ_ALWAYS) || (hotBlock->bbJumpDest != checkBlock))
            {
                JITDUMP("Unexpected flow from hot path " FMT_BB "\n", hotBlock->bbNum);
                return;
            }

            JITDUMP("Hot pred block is " FMT_BB " and cold pred block is " FMT_BB "\n", hotBlock->bbNum,
                    coldBlock->bbNum);

            // Clone and and copy the statements in the check block up to
            // and including lastStmt over to the hot block.
            // This will be the "hot" copy of the code.

            Statement* const afterLastStmt = lastStmt->GetNextStmt();

            for (Statement* checkStmt = checkBlock->firstStmt(); checkStmt != afterLastStmt;)
            {
                Statement* const nextStmt = checkStmt->GetNextStmt();

                // We should have ensured during scouting that all the statements
                // here can safely be cloned.
                //
                // Consider: allow inline candidates here, and keep them viable
                // in the hot copy, and demote them in the cold copy.
                //
                Statement* const clonedStmt = compiler->gtCloneStmt(checkStmt);
                compiler->fgInsertStmtAtEnd(hotBlock, clonedStmt);
                checkStmt = nextStmt;
            }

            // Now move the same span of statements to the cold block.

            for (Statement* checkStmt = checkBlock->firstStmt(); checkStmt != afterLastStmt;)
            {
                Statement* const nextStmt = checkStmt->GetNextStmt();
                compiler->fgUnlinkStmt(checkBlock, checkStmt);
                compiler->fgInsertStmtAtEnd(coldBlock, checkStmt);
                checkStmt = nextStmt;
            }

            // Finally, rewire the cold block to jump to the else block,
            // not fall through to the the check block.

            coldBlock->bbJumpKind = BBJ_ALWAYS;
            coldBlock->bbJumpDest = elseBlock;
        }

        // When the current candidate has sufficiently high likelihood, scan
        // the remainder block looking for another GDV candidate.
        //
        // (also consider: if currBlock has sufficiently high execution frequency)
        //
        // We want to see if it makes sense to mark the subsequent GDV site as a "chained"
        // GDV, where we duplicate the code in between to stitch together the high-likelihood
        // outcomes without a join.
        void ScoutForChainedGdv()
        {
            // If the current call isn't sufficiently likely, don't try and form a chain.
            //
            const unsigned gdvChainLikelihood = JitConfig.JitGuardedDevirtualizationChainLikelihood();

            if (likelihood < gdvChainLikelihood)
            {
                return;
            }

            JITDUMP("Scouting for possible GDV chain as likelihood %u >= %u\n", likelihood, gdvChainLikelihood);

            const unsigned maxStatementDup   = JitConfig.JitGuardedDevirtualizationChainStatements();
            unsigned       chainStatementDup = 0;
            unsigned       chainNodeDup      = 0;
            unsigned       chainLikelihood   = 0;
            GenTreeCall*   chainedCall       = nullptr;

            // Helper class to check a statement for uncloneable nodes and count the total number of nodes.
            class UnclonableVisitor final : public GenTreeVisitor<UnclonableVisitor>
            {
                Compiler* m_compiler;

            public:
                enum
                {
                    DoPreOrder = true
                };

                GenTree* m_unclonableNode = nullptr;
                unsigned m_nodeCount      = 0;

                UnclonableVisitor(Compiler* compiler) : m_compiler(compiler)
                {
                }

                GenTreeWalkResult PreOrderVisit(GenTree** use, GenTree* user)
                {
                    GenTree* const node = *use;

                    if (GenTreeCall* call = node->IsCall())
                    {
                        if (call->IsInlineCandidate() && !call->IsGuardedDevirtualizationCandidate())
                        {
                            m_unclonableNode = node;
                            return GenTreeWalkResult::Abort;
                        }
                    }
                    else if (node->OperIs(GT_RET_EXPR))
                    {
                        m_unclonableNode = node;
                        return GenTreeWalkResult::Abort;
                    }

                    m_nodeCount++;
                    return GenTreeWalkResult::Continue;
                }
            };

            for (Statement* const nextStmt : remainderBlock->Statements())
            {
                JITDUMP(" Scouting " FMT_STMT "\n", nextStmt->GetID());

                // See if this is a guarded devirtualization candidate.
                // These will be top-level trees.
                GenTree* const root = nextStmt->GetRootNode();

                if (GenTreeCall* call = root->IsCall())
                {
                    if (GuardedDevirtualizationCandidateInfo* gdvInfo = call->HasGuardedDevirtualizationInfo())
                    {
                        if (gdvInfo->likelihood >= gdvChainLikelihood)
                        {
                            JITDUMP(
                                "GDV call at [%06u] has likelihood %u >= %u; chaining (%u stmts, %u nodes to dup).\n",
                                call->GetID(), gdvInfo->likelihood, gdvChainLikelihood, chainStatementDup,
                                chainNodeDup);

                            call->gtCallMoreFlags |= GTF_CALL_M_GUARDED_DEVIRT_CHAIN;
                            break;
                        }
                    }
                }

                // Stop searching if we've accumulated too much dup cost.
                // Consider: use node count instead.
                if (chainStatementDup >= maxStatementDup)
                {
                    JITDUMP("  reached max statement dup limit of %u, bailing out\n", maxStatementDup);
                    break;
                }

                // See if this statement's tree is one that we can clone.
                UnclonableVisitor unclonableVisitor(compiler);
                unclonableVisitor.WalkTree(nextStmt->GetRootNodePointer(), nullptr);

                if (unclonableVisitor.m_unclonableNode != nullptr)
                {
                    JITDUMP("  node [%06u] can't be cloned\n", unclonableVisitor.m_unclonableNode->GetID());
                    break;
                }

                // Looks like we can clone this, so keep scouting.
                chainStatementDup++;
                chainNodeDup += unclonableVisitor.m_nodeCount;
            }
        }
    };

    // Runtime lookup with dynamic dictionary expansion transformer, it expects helper
    // runtime lookup call with 2 additional arguments: handle & sizeCheck test.
    class ExpRuntimeLookupTransformer final : public Transformer
    {
        LclVarDsc* resultLcl;

    public:
        ExpRuntimeLookupTransformer(Compiler* compiler, BasicBlock* block, Statement* stmt, GenTreeCall* call)
            : Transformer(compiler, block, stmt, call), resultLcl(stmt->GetRootNode()->AsLclStore()->GetLcl())
        {
            assert(resultLcl->TypeIs(TYP_I_IMPL));
        }

        void Run()
        {
            JITDUMP("*** ExpRuntimeLookup: transforming " FMT_TREEID "\n", origCall->GetID());

            GenTreeUse* handleArg    = origCall->RemoveFirstArg();
            GenTreeUse* sizeCheckArg = origCall->RemoveFirstArg();
            origCall->ClearExpRuntimeLookup();

            GenTree* handle = handleArg->GetNode();
            assert(handle->TypeIs(TYP_I_IMPL) && handle->IsIndLoad());

            GenTree* sizeCheck = sizeCheckArg->GetNode();
            assert(sizeCheck->OperIs(GT_LE));

            Transform(handle, sizeCheck);
        }

    private:
        void Transform(GenTree* handle, GenTree* sizeCheck)
        {
            BasicBlock* resultBlock = compiler->fgSplitBlockAfterStatement(currBlock, stmt);
            resultBlock->bbFlags |= BBF_INTERNAL;
            resultBlock->inheritWeight(currBlock);
            compiler->fgRemoveStmt(currBlock, stmt);

            GenTree* callBlockJmp = compiler->gtNewOperNode(GT_JTRUE, TYP_VOID, sizeCheck);
            compiler->fgInsertStmtAtEnd(currBlock, compiler->gtNewStmt(callBlockJmp, stmt->GetILOffsetX()));
            currBlock->bbJumpKind = BBJ_COND;

            BasicBlock* nullCheckBlock = CreateBasicBlock(BBJ_COND, currBlock);
            nullCheckBlock->inheritWeightPercentage(currBlock, HIGH_PROBABILITY);

            GenTree* handleStore = compiler->gtNewLclStore(resultLcl, TYP_I_IMPL, handle);
            compiler->fgInsertStmtAtEnd(nullCheckBlock, compiler->gtNewStmt(handleStore, stmt->GetILOffsetX()));

            GenTree* handleLoad     = compiler->gtNewLclLoad(resultLcl, TYP_I_IMPL);
            GenTree* nullConst      = compiler->gtNewIconNode(0, TYP_I_IMPL);
            GenTree* notNullCheck   = compiler->gtNewOperNode(GT_NE, TYP_INT, handleLoad, nullConst);
            GenTree* resultBlockJmp = compiler->gtNewOperNode(GT_JTRUE, TYP_VOID, notNullCheck);
            compiler->fgInsertStmtAtEnd(nullCheckBlock, compiler->gtNewStmt(resultBlockJmp, stmt->GetILOffsetX()));
            nullCheckBlock->bbJumpDest = resultBlock;

            BasicBlock* callBlock = CreateBasicBlock(BBJ_NONE, nullCheckBlock);
            callBlock->inheritWeightPercentage(currBlock, 100 - HIGH_PROBABILITY);
            currBlock->bbJumpDest = callBlock;

            compiler->fgInsertStmtAtEnd(callBlock, stmt);
        }
    };
};

// Find and transform various indirect calls.
// These transformations happen post-import because they may introduce control flow.
PhaseStatus Compiler::phTransformIndirectCalls()
{
    unsigned count = 0;

    if (doesMethodHaveFatPointer() || doesMethodHaveGuardedDevirtualization() || doesMethodHaveExpRuntimeLookup())
    {
        IndirectCallTransformer indirectCallTransformer(this);
        count = indirectCallTransformer.Run();

        JITDUMP("\n -- %u calls transformed\n", count);

        clearMethodHasFatPointer();
        clearMethodHasGuardedDevirtualization();
        clearMethodHasExpRuntimeLookup();
    }
    else
    {
        JITDUMP("\n -- no candidates to transform\n");
    }

#ifdef DEBUG
    for (BasicBlock* const block : Blocks())
    {
        for (Statement* const stmt : block->Statements())
        {
            fgWalkTreePre(stmt->GetRootNodePointer(), [](GenTree** use, GenTree* user, void* data) {
                if (GenTreeCall* call = (*use)->IsCall())
                {
                    assert(!call->IsFatPointerCandidate());
                    assert(!call->IsGuardedDevirtualizationCandidate());
                    assert(!call->IsExpRuntimeLookup());
                }
                return GenTreeWalkResult::Continue;
            });
        }
    }
#endif

    return count == 0 ? PhaseStatus::MODIFIED_NOTHING : PhaseStatus::MODIFIED_EVERYTHING;
}
