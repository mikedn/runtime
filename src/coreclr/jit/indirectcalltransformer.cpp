// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

// The IndirectCallTransformer transforms indirect calls that involve fat function
// pointers, guarded devirtualization candidates, or runtime lookup with dynamic dictionary expansion.
// These transformations introduce control flow and so can't easily be done in the importer.
//
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
// before:
//   current block
//   {
//     previous statements
//     transforming statement
//     {
//       call with GTF_CALL_M_FAT_POINTER_CHECK flag set in function ptr
//     }
//     subsequent statements
//   }
//
// after:
//   current block
//   {
//     previous statements
//   } BBJ_NONE check block
//   check block
//   {
//     jump to else if function ptr has the FAT_POINTER_MASK bit set.
//   } BBJ_COND then block, else block
//   then block
//   {
//     original statement
//   } BBJ_ALWAYS remainder block
//   else block
//   {
//     clear FAT_POINTER_MASK bit
//     load actual function pointer
//     load instantiation argument
//     create newArgList = (instantiation argument, original argList)
//     call (actual function pointer, newArgList)
//   } BBJ_NONE remainder block
//   remainder block
//   {
//     subsequent statements
//   }
//
class IndirectCallTransformer
{
public:
    IndirectCallTransformer(Compiler* compiler) : compiler(compiler)
    {
    }

    //------------------------------------------------------------------------
    // Run: run transformation for each block.
    //
    // Returns:
    //   Count of calls transformed.
    int Run()
    {
        int count = 0;

        for (BasicBlock* const block : compiler->Blocks())
        {
            count += TransformBlock(block);
        }

        return count;
    }

private:
    //------------------------------------------------------------------------
    // TransformBlock: look through statements and transform statements with
    //   particular indirect calls
    //
    // Returns:
    //   Count of calls transformed.
    //
    int TransformBlock(BasicBlock* block)
    {
        int count = 0;

        for (Statement* const stmt : block->Statements())
        {
            if (compiler->doesMethodHaveFatPointer() && ContainsFatCalli(stmt))
            {
                FatPointerCallTransformer transformer(compiler, block, stmt);
                transformer.Run();
                count++;
            }
            else if (compiler->doesMethodHaveGuardedDevirtualization() &&
                     ContainsGuardedDevirtualizationCandidate(stmt))
            {
                GuardedDevirtualizationTransformer transformer(compiler, block, stmt);
                transformer.Run();
                count++;
            }
            else if (compiler->doesMethodHaveExpRuntimeLookup() && ContainsExpRuntimeLookup(stmt))
            {
                ExpRuntimeLookupTransformer transformer(compiler, block, stmt);
                transformer.Run();
                count++;
            }
        }

        return count;
    }

    //------------------------------------------------------------------------
    // ContainsFatCalli: check does this statement contain fat pointer call.
    //
    // Checks fatPointerCandidate in form of call() or lclVar = call().
    //
    // Return Value:
    //    true if contains, false otherwise.
    //
    bool ContainsFatCalli(Statement* stmt)
    {
        GenTree* fatPointerCandidate = stmt->GetRootNode();
        if (fatPointerCandidate->OperIs(GT_ASG))
        {
            fatPointerCandidate = fatPointerCandidate->gtGetOp2();
        }
        return fatPointerCandidate->IsCall() && fatPointerCandidate->AsCall()->IsFatPointerCandidate();
    }

    //------------------------------------------------------------------------
    // ContainsGuardedDevirtualizationCandidate: check does this statement contain a virtual
    // call that we'd like to guardedly devirtualize?
    //
    // Return Value:
    //    true if contains, false otherwise.
    //
    // Notes:
    //    calls are hoisted to top level ... (we hope)
    bool ContainsGuardedDevirtualizationCandidate(Statement* stmt)
    {
        GenTree* candidate = stmt->GetRootNode();
        return candidate->IsCall() && candidate->AsCall()->IsGuardedDevirtualizationCandidate();
    }

    //------------------------------------------------------------------------
    // ContainsExpRuntimeLookup: check if this statement contains a dictionary
    // with dynamic dictionary expansion that we want to transform in CFG.
    //
    // Return Value:
    //    true if contains, false otherwise.
    //
    bool ContainsExpRuntimeLookup(Statement* stmt)
    {
        GenTree* candidate = stmt->GetRootNode();
        if (candidate->OperIs(GT_ASG))
        {
            candidate = candidate->AsOp()->GetOp(1);
        }
        return candidate->IsCall() && candidate->AsCall()->IsExpRuntimeLookup();
    }

    class Transformer
    {
    public:
        Transformer(Compiler* compiler, BasicBlock* block, Statement* stmt)
            : compiler(compiler), currBlock(block), stmt(stmt)
        {
            remainderBlock = nullptr;
            checkBlock     = nullptr;
            thenBlock      = nullptr;
            elseBlock      = nullptr;
            origCall       = nullptr;
            likelihood     = HIGH_PROBABILITY;
        }

        //------------------------------------------------------------------------
        // Run: transform the statement as described above.
        //
        virtual void Run()
        {
            Transform();
        }

        void Transform()
        {
            JITDUMP("*** %s: transforming " FMT_STMT "\n", Name(), stmt->GetID());
            FixupRetExpr();
            ClearFlag();
            CreateRemainder();
            CreateCheck();
            CreateThen();
            CreateElse();
            RemoveOldStatement();
            SetWeights();
            ChainFlow();
        }

    protected:
        virtual const char*  Name()                       = 0;
        virtual void         ClearFlag()                  = 0;
        virtual GenTreeCall* GetCall(Statement* callStmt) = 0;
        virtual void FixupRetExpr()                       = 0;

        //------------------------------------------------------------------------
        // CreateRemainder: split current block at the call stmt and
        // insert statements after the call into remainderBlock.
        //
        void CreateRemainder()
        {
            remainderBlock = compiler->fgSplitBlockAfterStatement(currBlock, stmt);
            remainderBlock->bbFlags |= BBF_INTERNAL;
        }

        virtual void CreateCheck() = 0;

        //------------------------------------------------------------------------
        // CreateAndInsertBasicBlock: ask compiler to create new basic block.
        // and insert in into the basic block list.
        //
        // Arguments:
        //    jumpKind - jump kind for the new basic block
        //    insertAfter - basic block, after which compiler has to insert the new one.
        //
        // Return Value:
        //    new basic block.
        BasicBlock* CreateAndInsertBasicBlock(BBjumpKinds jumpKind, BasicBlock* insertAfter)
        {
            BasicBlock* block = compiler->fgNewBBafter(jumpKind, insertAfter, true);
            block->bbFlags |= BBF_IMPORTED;
            return block;
        }

        virtual void CreateThen() = 0;
        virtual void CreateElse() = 0;

        //------------------------------------------------------------------------
        // RemoveOldStatement: remove original stmt from current block.
        //
        void RemoveOldStatement()
        {
            compiler->fgRemoveStmt(currBlock, stmt);
        }

        //------------------------------------------------------------------------
        // SetWeights: set weights for new blocks.
        //
        virtual void SetWeights()
        {
            remainderBlock->inheritWeight(currBlock);
            checkBlock->inheritWeight(currBlock);
            thenBlock->inheritWeightPercentage(currBlock, likelihood);
            elseBlock->inheritWeightPercentage(currBlock, 100 - likelihood);
        }

        //------------------------------------------------------------------------
        // ChainFlow: link new blocks into correct cfg.
        //
        virtual void ChainFlow()
        {
            assert(!compiler->fgComputePredsDone);
            checkBlock->bbJumpDest = elseBlock;
            thenBlock->bbJumpDest  = remainderBlock;
        }

        Compiler*    compiler;
        BasicBlock*  currBlock;
        BasicBlock*  remainderBlock;
        BasicBlock*  checkBlock;
        BasicBlock*  thenBlock;
        BasicBlock*  elseBlock;
        Statement*   stmt;
        GenTreeCall* origCall;
        unsigned     likelihood;

        const int HIGH_PROBABILITY = 80;
    };

    class FatPointerCallTransformer final : public Transformer
    {
    public:
        FatPointerCallTransformer(Compiler* compiler, BasicBlock* block, Statement* stmt)
            : Transformer(compiler, block, stmt)
        {
            doesReturnValue = stmt->GetRootNode()->OperIs(GT_ASG);
            origCall        = GetCall(stmt);
            fptrAddress     = origCall->gtCallAddr;
            pointerType     = fptrAddress->TypeGet();
        }

    protected:
        virtual const char* Name()
        {
            return "FatPointerCall";
        }

        //------------------------------------------------------------------------
        // GetCall: find a call in a statement.
        //
        // Arguments:
        //    callStmt - the statement with the call inside.
        //
        // Return Value:
        //    call tree node pointer.
        virtual GenTreeCall* GetCall(Statement* callStmt)
        {
            GenTree*     tree = callStmt->GetRootNode();
            GenTreeCall* call = nullptr;
            if (doesReturnValue)
            {
                assert(tree->OperIs(GT_ASG));
                call = tree->gtGetOp2()->AsCall();
            }
            else
            {
                call = tree->AsCall(); // call with void return type.
            }
            return call;
        }

        //------------------------------------------------------------------------
        // ClearFlag: clear fat pointer candidate flag from the original call.
        //
        virtual void ClearFlag()
        {
            origCall->ClearFatPointerCandidate();
        }

        // FixupRetExpr: no action needed as we handle this in the importer.
        virtual void FixupRetExpr()
        {
        }

        //------------------------------------------------------------------------
        // CreateCheck: create check block, that checks fat pointer bit set.
        //
        virtual void CreateCheck()
        {
            checkBlock                 = CreateAndInsertBasicBlock(BBJ_COND, currBlock);
            GenTree*   fatPointerMask  = new (compiler, GT_CNS_INT) GenTreeIntCon(TYP_I_IMPL, FAT_POINTER_MASK);
            GenTree*   fptrAddressCopy = compiler->gtCloneExpr(fptrAddress);
            GenTree*   fatPointerAnd   = compiler->gtNewOperNode(GT_AND, TYP_I_IMPL, fptrAddressCopy, fatPointerMask);
            GenTree*   zero            = new (compiler, GT_CNS_INT) GenTreeIntCon(TYP_I_IMPL, 0);
            GenTree*   fatPointerCmp   = compiler->gtNewOperNode(GT_NE, TYP_INT, fatPointerAnd, zero);
            GenTree*   jmpTree         = compiler->gtNewOperNode(GT_JTRUE, TYP_VOID, fatPointerCmp);
            Statement* jmpStmt         = compiler->gtNewStmt(jmpTree, stmt->GetILOffsetX());
            compiler->fgInsertStmtAtEnd(checkBlock, jmpStmt);
        }

        //------------------------------------------------------------------------
        // CreateThen: create then block, that is executed if the check succeeds.
        // This simply executes the original call.
        //
        virtual void CreateThen()
        {
            thenBlock                     = CreateAndInsertBasicBlock(BBJ_ALWAYS, checkBlock);
            Statement* copyOfOriginalStmt = compiler->gtCloneStmt(stmt);
            compiler->fgInsertStmtAtEnd(thenBlock, copyOfOriginalStmt);
        }

        //------------------------------------------------------------------------
        // CreateElse: create else block, that is executed if call address is fat pointer.
        //
        virtual void CreateElse()
        {
            elseBlock = CreateAndInsertBasicBlock(BBJ_NONE, thenBlock);

            GenTree* fixedFptrAddress  = GetFixedFptrAddress();
            GenTree* actualCallAddress = compiler->gtNewIndir(pointerType, fixedFptrAddress);
            GenTree* hiddenArgument    = GetHiddenArgument(fixedFptrAddress);

            Statement* fatStmt = CreateFatCallStmt(actualCallAddress, hiddenArgument);
            compiler->fgInsertStmtAtEnd(elseBlock, fatStmt);
        }

        //------------------------------------------------------------------------
        // GetFixedFptrAddress: clear fat pointer bit from fat pointer address.
        //
        // Return Value:
        //    address without fat pointer bit set.
        GenTree* GetFixedFptrAddress()
        {
            GenTree* fptrAddressCopy = compiler->gtCloneExpr(fptrAddress);
            GenTree* fatPointerMask  = new (compiler, GT_CNS_INT) GenTreeIntCon(TYP_I_IMPL, FAT_POINTER_MASK);
            return compiler->gtNewOperNode(GT_SUB, pointerType, fptrAddressCopy, fatPointerMask);
        }

        //------------------------------------------------------------------------
        // GetHiddenArgument: load hidden argument.
        //
        // Arguments:
        //    fixedFptrAddress - pointer to the tuple <methodPointer, instantiationArgumentPointer>
        //
        // Return Value:
        //    generic context hidden argument.
        GenTree* GetHiddenArgument(GenTree* fixedFptrAddress)
        {
            GenTree* fixedFptrAddressCopy = compiler->gtCloneExpr(fixedFptrAddress);
            GenTree* wordSize             = compiler->gtNewIconNode(varTypeSize(TYP_I_IMPL), TYP_I_IMPL);
            GenTree* hiddenArgumentPtrPtr =
                compiler->gtNewOperNode(GT_ADD, pointerType, fixedFptrAddressCopy, wordSize);
            GenTree* hiddenArgumentPtr = compiler->gtNewIndir(pointerType, hiddenArgumentPtrPtr);
            return compiler->gtNewIndir(fixedFptrAddressCopy->GetType(), hiddenArgumentPtr);
        }

        //------------------------------------------------------------------------
        // CreateFatCallStmt: create call with fixed call address and hidden argument in the args list.
        //
        // Arguments:
        //    actualCallAddress - fixed call address
        //    hiddenArgument - generic context hidden argument
        //
        // Return Value:
        //    created call node.
        Statement* CreateFatCallStmt(GenTree* actualCallAddress, GenTree* hiddenArgument)
        {
            Statement*   fatStmt = compiler->gtCloneStmt(stmt);
            GenTreeCall* fatCall = GetCall(fatStmt);
            fatCall->gtCallAddr  = actualCallAddress;
            AddHiddenArgument(fatCall, hiddenArgument);
            return fatStmt;
        }

        //------------------------------------------------------------------------
        // AddHiddenArgument: add hidden argument to the call argument list.
        //
        // Arguments:
        //    fatCall - fat call node
        //    hiddenArgument - generic context hidden argument
        //
        void AddHiddenArgument(GenTreeCall* fatCall, GenTree* hiddenArgument)
        {
#ifndef TARGET_X86
            if (fatCall->HasRetBufArg())
            {
                GenTreeCall::Use* retBufArg = fatCall->gtCallArgs;
                compiler->gtInsertNewCallArgAfter(hiddenArgument, retBufArg);
            }
            else
            {
                fatCall->gtCallArgs = compiler->gtPrependNewCallArg(hiddenArgument, fatCall->gtCallArgs);
            }
#else
            if (fatCall->gtCallArgs == nullptr)
            {
                fatCall->gtCallArgs = compiler->gtNewCallArgs(hiddenArgument);
            }
            else
            {
                AddArgumentToTail(fatCall->gtCallArgs, hiddenArgument);
            }
#endif
        }

        //------------------------------------------------------------------------
        // AddArgumentToTail: add hidden argument to the tail of the call argument list.
        //
        // Arguments:
        //    argList - fat call node
        //    hiddenArgument - generic context hidden argument
        //
        void AddArgumentToTail(GenTreeCall::Use* argList, GenTree* hiddenArgument)
        {
            GenTreeCall::Use* iterator = argList;
            while (iterator->GetNext() != nullptr)
            {
                iterator = iterator->GetNext();
            }
            iterator->SetNext(compiler->gtNewCallArgs(hiddenArgument));
        }

    private:
        const int FAT_POINTER_MASK = 0x2;

        GenTree*  fptrAddress;
        var_types pointerType;
        bool      doesReturnValue;
    };

    class GuardedDevirtualizationTransformer final : public Transformer
    {
        LclVarDsc* returnTemp = nullptr;
        Statement* lastStmt;

    public:
        GuardedDevirtualizationTransformer(Compiler* compiler, BasicBlock* block, Statement* stmt)
            : Transformer(compiler, block, stmt)
        {
        }

        //------------------------------------------------------------------------
        // Run: transform the statement as described above.
        //
        virtual void Run()
        {
            origCall = GetCall(stmt);

            JITDUMP("\n----------------\n\n*** %s contemplating [%06u] in " FMT_BB " \n", Name(),
                    compiler->dspTreeID(origCall), currBlock->bbNum);

            // We currently need inline candidate info to guarded devirt.
            //
            if (!origCall->IsInlineCandidate())
            {
                JITDUMP("*** %s Bailing on [%06u] -- not an inline candidate\n", Name(), compiler->dspTreeID(origCall));
                ClearFlag();
                return;
            }

            likelihood = origCall->gtGuardedDevirtualizationCandidateInfo->likelihood;
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

            // Look ahead and see if there's another Gdv we might chain to this one.
            //
            ScoutForChainedGdv();
        }

    protected:
        virtual const char* Name()
        {
            return "GuardedDevirtualization";
        }

        //------------------------------------------------------------------------
        // GetCall: find a call in a statement.
        //
        // Arguments:
        //    callStmt - the statement with the call inside.
        //
        // Return Value:
        //    call tree node pointer.
        virtual GenTreeCall* GetCall(Statement* callStmt)
        {
            GenTree* tree = callStmt->GetRootNode();
            assert(tree->IsCall());
            GenTreeCall* call = tree->AsCall();
            return call;
        }

        //------------------------------------------------------------------------
        // ClearFlag: clear guarded devirtualization candidate flag from the original call.
        //
        virtual void ClearFlag()
        {
            origCall->ClearGuardedDevirtualizationCandidate();
        }

        //------------------------------------------------------------------------
        // CreateCheck: create check block and check method table
        //
        virtual void CreateCheck()
        {
            // There's no need for a new block here. We can just append to currBlock.
            //
            checkBlock             = currBlock;
            checkBlock->bbJumpKind = BBJ_COND;

            GenTree*   thisTree = origCall->gtCallThisArg->GetNode();
            LclVarDsc* thisLcl;

            if (thisTree->OperIs(GT_LCL_VAR))
            {
                // TODO-MIKE-Review: Is it safe to use an existing LCL_VAR if it ends up being address
                // exposed? An address exposed local could be modified by a different thread. That's
                // extremely unusual but if it happens then type safety is compromised. And anyway,
                // using a DNER local will result in poor codegen.

                thisLcl = thisTree->AsLclVar()->GetLcl();
            }
            else
            {
                thisLcl = compiler->lvaNewTemp(TYP_REF, true DEBUGARG("guarded devirt this temp"));

                GenTree*   asgTree = compiler->gtNewAssignNode(compiler->gtNewLclvNode(thisLcl, TYP_REF), thisTree);
                Statement* asgStmt = compiler->gtNewStmt(asgTree, stmt->GetILOffsetX());
                compiler->fgInsertStmtAtEnd(checkBlock, asgStmt);

                origCall->gtCallThisArg->SetNode(compiler->gtNewLclvNode(thisLcl, TYP_REF));
            }

            thisTree = compiler->gtNewLclvNode(thisLcl, TYP_REF);

            // Remember the current last statement. If we're doing a chained GDV, we'll clone/copy
            // all the code in the check block up to and including this statement.
            //
            // Note it's important that we clone/copy the temp assign above, if we created one,
            // because flow along the "cold path" is going to bypass the check block.
            //
            lastStmt = checkBlock->lastStmt();

            // Find target method table
            GenTree*                              methodTable       = compiler->gtNewMethodTableLookup(thisTree);
            GuardedDevirtualizationCandidateInfo* guardedInfo       = origCall->gtGuardedDevirtualizationCandidateInfo;
            CORINFO_CLASS_HANDLE                  clsHnd            = guardedInfo->guardedClassHandle;
            GenTree*                              targetMethodTable = compiler->gtNewIconEmbClsHndNode(clsHnd);

            // Compare and jump to else (which does the indirect call) if NOT equal
            //
            GenTree*   methodTableCompare = compiler->gtNewOperNode(GT_NE, TYP_INT, targetMethodTable, methodTable);
            GenTree*   jmpTree            = compiler->gtNewOperNode(GT_JTRUE, TYP_VOID, methodTableCompare);
            Statement* jmpStmt            = compiler->gtNewStmt(jmpTree, stmt->GetILOffsetX());
            compiler->fgInsertStmtAtEnd(checkBlock, jmpStmt);
        }

        //------------------------------------------------------------------------
        // FixupRetExpr: set up to repair return value placeholder from call
        //
        virtual void FixupRetExpr()
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
                //
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
                        //
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

                GenTree* tempTree = compiler->gtNewLclvNode(returnTemp, origCall->GetType());

                JITDUMP("Bashing GT_RET_EXPR [%06u] to refer to temp V%02u\n", retExpr->GetID(),
                        returnTemp->GetLclNum());

                retExpr->ReplaceWith(tempTree, compiler);
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

        //------------------------------------------------------------------------
        // CreateThen: create then block with direct call to method
        //
        virtual void CreateThen()
        {
            thenBlock = CreateAndInsertBasicBlock(BBJ_ALWAYS, checkBlock);
            thenBlock->bbFlags |= currBlock->bbFlags & BBF_SPLIT_GAINED;

            InlineCandidateInfo* inlineInfo = origCall->gtInlineCandidateInfo;
            CORINFO_CLASS_HANDLE clsHnd     = inlineInfo->guardedClassHandle;

            // copy 'this' to temp with exact type.
            LclVarDsc* thisTempLcl = compiler->lvaNewTemp(TYP_REF, false DEBUGARG("guarded devirt this exact temp"));
            GenTree*   clonedObj   = compiler->gtCloneExpr(origCall->gtCallThisArg->GetNode());
            GenTree*   assign = compiler->gtNewAssignNode(compiler->gtNewLclvNode(thisTempLcl, TYP_REF), clonedObj);
            compiler->lvaSetClass(thisTempLcl, clsHnd, true);
            compiler->fgNewStmtAtEnd(thenBlock, assign);

            // Clone call. Note we must use the special candidate helper.
            GenTreeCall* call   = compiler->gtCloneCandidateCall(origCall);
            call->gtCallThisArg = compiler->gtNewCallArgs(compiler->gtNewLclvNode(thisTempLcl, TYP_REF));
            call->SetIsGuarded();

            JITDUMP("Direct call [%06u] in block " FMT_BB "\n", compiler->dspTreeID(call), thenBlock->bbNum);

            // Then invoke impLateDevirtualizeCall to actually transform the call for us,
            // given the original (base) method and the exact guarded class. It should succeed.
            CORINFO_METHOD_HANDLE  methodHnd;
            CORINFO_CONTEXT_HANDLE context;
            compiler->impLateDevirtualizeCall(call, inlineInfo, &methodHnd, &context);

            // We know this call can devirtualize or we would not have set up GDV here.
            // So impLateDevirtualizeCall should succeed in devirtualizing.
            //
            assert(!call->IsVirtual());

            // If the devirtualizer was unable to transform the call to invoke the unboxed entry, the inline info
            // we set up may be invalid. We won't be able to inline anyways. So demote the call as an inline candidate.
            //
            CORINFO_METHOD_HANDLE unboxedMethodHnd = inlineInfo->guardedMethodUnboxedEntryHandle;
            if ((unboxedMethodHnd != nullptr) && (methodHnd != unboxedMethodHnd))
            {
                // Demote this call to a non-inline candidate
                //
                JITDUMP("Devirtualization was unable to use the unboxed entry; so marking call (to boxed entry) as not "
                        "inlineable\n");

                call->gtFlags &= ~GTF_CALL_INLINE_CANDIDATE;
                call->gtInlineCandidateInfo = nullptr;

                if (returnTemp != nullptr)
                {
                    GenTree* assign =
                        compiler->gtNewAssignNode(compiler->gtNewLclvNode(returnTemp, call->GetType()), call);
                    compiler->fgNewStmtAtEnd(thenBlock, assign);
                }
                else
                {
                    compiler->fgNewStmtAtEnd(thenBlock, call);
                }
            }
            else
            {
                // Add the call.
                //
                compiler->fgNewStmtAtEnd(thenBlock, call);

                // Re-establish this call as an inline candidate.
                //
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
                        retExpr =
                            compiler->gtNewAssignNode(compiler->gtNewLclvNode(returnTemp, call->GetType()), retExpr);
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

        //------------------------------------------------------------------------
        // CreateElse: create else block. This executes the unaltered indirect call.
        //
        virtual void CreateElse()
        {
            elseBlock = CreateAndInsertBasicBlock(BBJ_NONE, thenBlock);
            elseBlock->bbFlags |= currBlock->bbFlags & BBF_SPLIT_GAINED;
            GenTreeCall* call    = origCall;
            Statement*   newStmt = compiler->gtNewStmt(call);

            call->gtFlags &= ~GTF_CALL_INLINE_CANDIDATE;
            call->SetIsGuarded();

            JITDUMP("Residual call [%06u] moved to block " FMT_BB "\n", compiler->dspTreeID(call), elseBlock->bbNum);

            if (returnTemp != nullptr)
            {
                newStmt->SetRootNode(
                    compiler->gtNewAssignNode(compiler->gtNewLclvNode(returnTemp, call->GetType()), call));
            }

            // For stub calls, restore the stub address. For everything else,
            // null out the candidate info field.
            if (call->IsVirtualStub())
            {
                JITDUMP("Restoring stub addr %p from candidate info\n", call->gtInlineCandidateInfo->stubAddr);
                call->gtStubCallStubAddr = call->gtInlineCandidateInfo->stubAddr;
            }
            else
            {
                call->gtInlineCandidateInfo = nullptr;
            }

            compiler->fgInsertStmtAtEnd(elseBlock, newStmt);

            // Set the original statement to a nop.
            //
            stmt->SetRootNode(compiler->gtNewNothingNode());
        }

        // For chained gdv, we modify the expansion as follows:
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
        //
        void TransformForChainedGdv()
        {
            // Find the hot/cold predecessors. (Consider: just record these when
            // we did the scouting).
            //
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
            //
            // This will be the "hot" copy of the code.
            //
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
            //
            for (Statement* checkStmt = checkBlock->firstStmt(); checkStmt != afterLastStmt;)
            {
                Statement* const nextStmt = checkStmt->GetNextStmt();
                compiler->fgUnlinkStmt(checkBlock, checkStmt);
                compiler->fgInsertStmtAtEnd(coldBlock, checkStmt);
                checkStmt = nextStmt;
            }

            // Finally, rewire the cold block to jump to the else block,
            // not fall through to the the check block.
            //
            coldBlock->bbJumpKind = BBJ_ALWAYS;
            coldBlock->bbJumpDest = elseBlock;
        }

        // When the current candidate hads sufficiently high likelihood, scan
        // the remainer block looking for another GDV candidate.
        //
        // (also consider: if currBlock has sufficiently high execution frequency)
        //
        // We want to see if it makes sense to mark the subsequent GDV site as a "chained"
        // GDV, where we duplicate the code in between to stitch together the high-likehood
        // outcomes without a join.
        //
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

            // Helper class to check a statement for uncloneable nodes and count
            // the total number of nodes
            //
            class UnclonableVisitor final : public GenTreeVisitor<UnclonableVisitor>
            {
            public:
                enum
                {
                    DoPreOrder = true
                };

                GenTree* m_unclonableNode;
                unsigned m_nodeCount;

                UnclonableVisitor(Compiler* compiler)
                    : GenTreeVisitor<UnclonableVisitor>(compiler), m_unclonableNode(nullptr), m_nodeCount(0)
                {
                }

                fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
                {
                    GenTree* const node = *use;

                    if (node->IsCall())
                    {
                        GenTreeCall* const call = node->AsCall();

                        if (call->IsInlineCandidate() && !call->IsGuardedDevirtualizationCandidate())
                        {
                            m_unclonableNode = node;
                            return fgWalkResult::WALK_ABORT;
                        }
                    }
                    else if (node->OperIs(GT_RET_EXPR))
                    {
                        m_unclonableNode = node;
                        return fgWalkResult::WALK_ABORT;
                    }

                    m_nodeCount++;
                    return fgWalkResult::WALK_CONTINUE;
                }
            };

            for (Statement* const nextStmt : remainderBlock->Statements())
            {
                JITDUMP(" Scouting " FMT_STMT "\n", nextStmt->GetID());

                // See if this is a guarded devirt candidate.
                // These will be top-level trees.
                //
                GenTree* const root = nextStmt->GetRootNode();

                if (root->IsCall())
                {
                    GenTreeCall* const call = root->AsCall();

                    if (call->IsGuardedDevirtualizationCandidate() &&
                        (call->gtGuardedDevirtualizationCandidateInfo->likelihood >= gdvChainLikelihood))
                    {
                        JITDUMP("GDV call at [%06u] has likelihood %u >= %u; chaining (%u stmts, %u nodes to dup).\n",
                                compiler->dspTreeID(call), call->gtGuardedDevirtualizationCandidateInfo->likelihood,
                                gdvChainLikelihood, chainStatementDup, chainNodeDup);

                        call->gtCallMoreFlags |= GTF_CALL_M_GUARDED_DEVIRT_CHAIN;
                        break;
                    }
                }

                // Stop searching if we've accumulated too much dup cost.
                // Consider: use node count instead.
                //
                if (chainStatementDup >= maxStatementDup)
                {
                    JITDUMP("  reached max statement dup limit of %u, bailing out\n", maxStatementDup);
                    break;
                }

                // See if this statement's tree is one that we can clone.
                //
                UnclonableVisitor unclonableVisitor(compiler);
                unclonableVisitor.WalkTree(nextStmt->GetRootNodePointer(), nullptr);

                if (unclonableVisitor.m_unclonableNode != nullptr)
                {
                    JITDUMP("  node [%06u] can't be cloned\n", compiler->dspTreeID(unclonableVisitor.m_unclonableNode));
                    break;
                }

                // Looks like we can clone this, so keep scouting.
                //
                chainStatementDup++;
                chainNodeDup += unclonableVisitor.m_nodeCount;
            }
        }
    };

    // Runtime lookup with dynamic dictionary expansion transformer,
    // it expects helper runtime lookup call with additional arguments that are:
    // result handle, nullCheck tree, sizeCheck tree.
    // before:
    //   current block
    //   {
    //     previous statements
    //     transforming statement
    //     {
    //       ASG lclVar, call with GTF_CALL_M_EXP_RUNTIME_LOOKUP flag set and additional arguments.
    //     }
    //     subsequent statements
    //   }
    //
    // after:
    //   current block
    //   {
    //     previous statements
    //   } BBJ_NONE check block
    //   check block
    //   {
    //     jump to else if the handle fails size check
    //   } BBJ_COND check block2, else block
    //   check block2
    //   {
    //     jump to else if the handle fails null check
    //   } BBJ_COND then block, else block
    //   then block
    //   {
    //     return handle
    //   } BBJ_ALWAYS remainder block
    //   else block
    //   {
    //     do a helper call
    //   } BBJ_NONE remainder block
    //   remainder block
    //   {
    //     subsequent statements
    //   }
    //
    class ExpRuntimeLookupTransformer final : public Transformer
    {
        BasicBlock* checkBlock2 = nullptr;
        LclVarDsc*  resultLcl;

    public:
        ExpRuntimeLookupTransformer(Compiler* compiler, BasicBlock* block, Statement* stmt)
            : Transformer(compiler, block, stmt), resultLcl(stmt->GetRootNode()->AsOp()->GetOp(0)->AsLclVar()->GetLcl())
        {
            assert(resultLcl->TypeIs(TYP_I_IMPL));

            origCall = GetCall(stmt);
        }

    protected:
        virtual const char* Name() override
        {
            return "ExpRuntimeLookup";
        }

        //------------------------------------------------------------------------
        // GetCall: find a call in a statement.
        //
        // Arguments:
        //    callStmt - the statement with the call inside.
        //
        // Return Value:
        //    call tree node pointer.
        virtual GenTreeCall* GetCall(Statement* callStmt) override
        {
            GenTree* tree = callStmt->GetRootNode();
            assert(tree->OperIs(GT_ASG));
            GenTreeCall* call = tree->gtGetOp2()->AsCall();
            return call;
        }

        //------------------------------------------------------------------------
        // ClearFlag: clear runtime exp lookup flag from the original call.
        //
        virtual void ClearFlag() override
        {
            origCall->ClearExpRuntimeLookup();
        }

        // FixupRetExpr: no action needed.
        virtual void FixupRetExpr() override
        {
        }

        //------------------------------------------------------------------------
        // CreateCheck: create check blocks, that checks dictionary size and does null test.
        //
        virtual void CreateCheck() override
        {
            GenTreeCall::UseIterator argsIter  = origCall->Args().begin();
            GenTree*                 nullCheck = argsIter.GetUse()->GetNode();
            ++argsIter;
            GenTree* sizeCheck = argsIter.GetUse()->GetNode();
            ++argsIter;
            origCall->gtCallArgs = argsIter.GetUse();
            // The first argument is the handle now.
            checkBlock = CreateAndInsertBasicBlock(BBJ_COND, currBlock);

            assert(sizeCheck->OperIs(GT_LE));
            GenTree*   sizeJmpTree = compiler->gtNewOperNode(GT_JTRUE, TYP_VOID, sizeCheck);
            Statement* sizeJmpStmt = compiler->gtNewStmt(sizeJmpTree, stmt->GetILOffsetX());
            compiler->fgInsertStmtAtEnd(checkBlock, sizeJmpStmt);

            checkBlock2 = CreateAndInsertBasicBlock(BBJ_COND, checkBlock);
            assert(nullCheck->OperIs(GT_EQ));
            GenTree*   nullJmpTree = compiler->gtNewOperNode(GT_JTRUE, TYP_VOID, nullCheck);
            Statement* nullJmpStmt = compiler->gtNewStmt(nullJmpTree, stmt->GetILOffsetX());
            compiler->fgInsertStmtAtEnd(checkBlock2, nullJmpStmt);
        }

        //------------------------------------------------------------------------
        // CreateThen: create then block, that is executed if the checks succeed.
        // This simply returns the handle.
        //
        virtual void CreateThen() override
        {
            thenBlock = CreateAndInsertBasicBlock(BBJ_ALWAYS, checkBlock2);

            GenTree* resultHandle = origCall->gtCallArgs->GetNode();
            // The first argument is the real first argument for the call now.
            origCall->gtCallArgs = origCall->gtCallArgs->GetNext();

            AssignResult(thenBlock, resultHandle);
        }

        //------------------------------------------------------------------------
        // CreateElse: create else block, that is executed if the checks fail.
        //
        virtual void CreateElse() override
        {
            elseBlock = CreateAndInsertBasicBlock(BBJ_NONE, thenBlock);

            AssignResult(elseBlock, origCall);
        }

        void AssignResult(BasicBlock* block, GenTree* result)
        {
            GenTree* asg = compiler->gtNewAssignNode(compiler->gtNewLclvNode(resultLcl, TYP_I_IMPL), result);
            compiler->fgInsertStmtAtEnd(block, compiler->gtNewStmt(asg, stmt->GetILOffsetX()));
        }

        //------------------------------------------------------------------------
        // SetWeights: set weights for new blocks.
        //
        virtual void SetWeights() override
        {
            remainderBlock->inheritWeight(currBlock);
            checkBlock->inheritWeight(currBlock);
            checkBlock2->inheritWeightPercentage(checkBlock, HIGH_PROBABILITY);
            thenBlock->inheritWeightPercentage(currBlock, HIGH_PROBABILITY);
            elseBlock->inheritWeightPercentage(currBlock, 100 - HIGH_PROBABILITY);
        }

        //------------------------------------------------------------------------
        // ChainFlow: link new blocks into correct cfg.
        //
        virtual void ChainFlow() override
        {
            assert(!compiler->fgComputePredsDone);
            checkBlock->bbJumpDest  = elseBlock;
            checkBlock2->bbJumpDest = elseBlock;
            thenBlock->bbJumpDest   = remainderBlock;
        }
    };

    Compiler* compiler;
};

//------------------------------------------------------------------------
// fgTransformIndirectCalls: find and transform various indirect calls
//
// These transformations happen post-import because they may introduce
// control flow.
//
// Returns:
//   phase status indicating if changes were made
//
PhaseStatus Compiler::fgTransformIndirectCalls()
{
    int count = 0;
    if (doesMethodHaveFatPointer() || doesMethodHaveGuardedDevirtualization() || doesMethodHaveExpRuntimeLookup())
    {
        IndirectCallTransformer indirectCallTransformer(this);
        count = indirectCallTransformer.Run();

        JITDUMP("\n -- %d calls transformed\n", count);

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
            fgWalkTreePre(stmt->GetRootNodePointer(), [](GenTree** use, fgWalkData* data) {
                if (GenTreeCall* call = (*use)->IsCall())
                {
                    assert(!call->IsFatPointerCandidate());
                    assert(!call->IsGuardedDevirtualizationCandidate());
                    assert(!call->IsExpRuntimeLookup());
                }
                return WALK_CONTINUE;
            });
        }
    }
#endif

    return (count == 0) ? PhaseStatus::MODIFIED_NOTHING : PhaseStatus::MODIFIED_EVERYTHING;
}
