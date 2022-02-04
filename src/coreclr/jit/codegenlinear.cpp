// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX            Code Generation Support Methods for Linear Codegen             XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/
#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#include "emit.h"
#include "codegen.h"

//------------------------------------------------------------------------
// genInitializeRegisterState: Initialize the register state contained in 'regSet'.
//
// Assumptions:
//    On exit the "rsModifiedRegsMask" (in "regSet") holds all the registers' masks hosting an argument on the function
//    and elements of "rsSpillDesc" (in "regSet") are setted to nullptr.
//
// Notes:
//    This method is intended to be called only from initializeStructuresBeforeBlockCodeGeneration.
void CodeGen::genInitializeRegisterState()
{
    // Initialize the spill tracking logic

    regSet.rsSpillBeg();

    // If any arguments live in registers, mark those regs as such

    unsigned   varNum;
    LclVarDsc* varDsc;

    for (varNum = 0, varDsc = compiler->lvaTable; varNum < compiler->lvaCount; varNum++, varDsc++)
    {
        // Is this variable a parameter assigned to a register?
        if (!varDsc->lvIsParam || !varDsc->lvRegister)
        {
            continue;
        }

        // Is the argument live on entry to the method?
        if (!VarSetOps::IsMember(compiler, compiler->fgFirstBB->bbLiveIn, varDsc->lvVarIndex))
        {
            continue;
        }

        // Is this a floating-point argument?
        if (varDsc->IsFloatRegType())
        {
            continue;
        }

        noway_assert(!varTypeUsesFloatReg(varDsc->TypeGet()));

        // Mark the register as holding the variable
        assert(varDsc->GetRegNum() != REG_STK);
        if (!varDsc->lvAddrExposed)
        {
            regSet.verifyRegUsed(varDsc->GetRegNum());
        }
    }
}

//------------------------------------------------------------------------
// genInitialize: Initialize Scopes, registers, gcInfo and current liveness variables structures
// used in the generation of blocks' code before.
//
// Assumptions:
//    -The pointer logic in "gcInfo" for pointers on registers and variable is cleaned.
//    -If there is local var info siScopes scope logic in codegen is initialized in "siInit()"
//
// Notes:
//    This method is intended to be called when code generation for blocks happens, and before the list of blocks is
//    iterated.
void CodeGen::genInitialize()
{
    // Initialize the line# tracking logic
    if (compiler->opts.compScopeInfo)
    {
        siInit();
    }

#ifdef USING_VARIABLE_LIVE_RANGE
    initializeVariableLiveKeeper();
#endif //  USING_VARIABLE_LIVE_RANGE

    genPendingCallLabel = nullptr;

    // Initialize the pointer tracking code

    gcInfo.gcRegPtrSetInit();
    gcInfo.gcVarPtrSetInit();

    // Initialize the register set logic

    genInitializeRegisterState();

    // We initialize the stack level before first "BasicBlock" code is generated in case we need to report stack
    // variable needs home and so its stack offset.
    SetStackLevel(0);
}

//------------------------------------------------------------------------
// genCodeForBBlist: Generate code for all the blocks in a method
//
// Arguments:
//    None
//
// Notes:
//    This is the main method for linear codegen. It calls genCodeForTreeNode
//    to generate the code for each node in each BasicBlock, and handles BasicBlock
//    boundaries and branches.
//
void CodeGen::genCodeForBBlist()
{
    unsigned savedStkLvl;

#ifdef DEBUG
    genInterruptibleUsed = true;

    // You have to be careful if you create basic blocks from now on
    compiler->fgSafeBasicBlockCreation = false;
#endif // DEBUG

#if defined(DEBUG) && defined(TARGET_X86)

    // Check stack pointer on call stress mode is not compatible with fully interruptible GC. REVIEW: why?
    //
    if (GetInterruptible() && compiler->opts.compStackCheckOnCall)
    {
        compiler->opts.compStackCheckOnCall = false;
    }

#endif // defined(DEBUG) && defined(TARGET_X86)

#if defined(DEBUG) && defined(TARGET_XARCH)

    // Check stack pointer on return stress mode is not compatible with fully interruptible GC. REVIEW: why?
    // It is also not compatible with any function that makes a tailcall: we aren't smart enough to only
    // insert the SP check in the non-tailcall returns.
    //
    if ((GetInterruptible() || compiler->compTailCallUsed) && compiler->opts.compStackCheckOnRet)
    {
        compiler->opts.compStackCheckOnRet = false;
    }

#endif // defined(DEBUG) && defined(TARGET_XARCH)

    genMarkLabelsForCodegen();

    assert(!compiler->fgFirstBBScratch ||
           compiler->fgFirstBB == compiler->fgFirstBBScratch); // compiler->fgFirstBBScratch has to be first.

    /* Initialize structures used in the block list iteration */
    genInitialize();

    /*-------------------------------------------------------------------------
     *
     *  Walk the basic blocks and generate code for each one
     *
     */

    BasicBlock* block;

    for (block = compiler->fgFirstBB; block != nullptr; block = block->bbNext)
    {
#ifdef DEBUG
        if (compiler->verbose)
        {
            printf("\n=============== Generating ");
            block->dspBlockHeader(compiler, true, true);
            compiler->fgDispBBLiveness(block);
        }
#endif // DEBUG

        assert(LIR::AsRange(block).CheckLIR(compiler));

        // Figure out which registers hold variables on entry to this block

        regSet.ClearMaskVars();
        gcInfo.gcRegGCrefSetCur = RBM_NONE;
        gcInfo.gcRegByrefSetCur = RBM_NONE;

        compiler->m_pLinearScan->recordVarLocationsAtStartOfBB(block);

        // Updating variable liveness after last instruction of previous block was emitted
        // and before first of the current block is emitted
        m_liveness.ChangeLife(this, block->bbLiveIn);

        // Even if liveness didn't change, we need to update the registers containing GC references.
        // genUpdateLife will update the registers live due to liveness changes. But what about registers that didn't
        // change? We cleared them out above. Maybe we should just not clear them out, but update the ones that change
        // here. That would require handling the changes in recordVarLocationsAtStartOfBB().

        regMaskTP newLiveRegSet  = RBM_NONE;
        regMaskTP newRegGCrefSet = RBM_NONE;
        regMaskTP newRegByrefSet = RBM_NONE;
#ifdef DEBUG
        VARSET_TP removedGCVars(VarSetOps::MakeEmpty(compiler));
        VARSET_TP addedGCVars(VarSetOps::MakeEmpty(compiler));
#endif
        VarSetOps::Iter iter(compiler, block->bbLiveIn);
        unsigned        varIndex = 0;
        while (iter.NextElem(&varIndex))
        {
            LclVarDsc* varDsc = compiler->lvaGetDescByTrackedIndex(varIndex);

            if (varDsc->lvIsInReg())
            {
                newLiveRegSet |= varDsc->lvRegMask();
                if (varDsc->lvType == TYP_REF)
                {
                    newRegGCrefSet |= varDsc->lvRegMask();
                }
                else if (varDsc->lvType == TYP_BYREF)
                {
                    newRegByrefSet |= varDsc->lvRegMask();
                }
                if (!varDsc->IsAlwaysAliveInMemory())
                {
#ifdef DEBUG
                    if (verbose && VarSetOps::IsMember(compiler, gcInfo.gcVarPtrSetCur, varIndex))
                    {
                        VarSetOps::AddElemD(compiler, removedGCVars, varIndex);
                    }
#endif // DEBUG
                    VarSetOps::RemoveElemD(compiler, gcInfo.gcVarPtrSetCur, varIndex);
                }
            }
            if ((!varDsc->lvIsInReg() || varDsc->IsAlwaysAliveInMemory()) && compiler->lvaIsGCTracked(varDsc))
            {
#ifdef DEBUG
                if (verbose && !VarSetOps::IsMember(compiler, gcInfo.gcVarPtrSetCur, varIndex))
                {
                    VarSetOps::AddElemD(compiler, addedGCVars, varIndex);
                }
#endif // DEBUG
                VarSetOps::AddElemD(compiler, gcInfo.gcVarPtrSetCur, varIndex);
            }
        }

        regSet.SetMaskVars(newLiveRegSet);

#ifdef DEBUG
        if (compiler->verbose)
        {
            if (!VarSetOps::IsEmpty(compiler, addedGCVars))
            {
                printf("Added GCVars: ");
                dumpConvertedVarSet(compiler, addedGCVars);
                printf("\n");
            }
            if (!VarSetOps::IsEmpty(compiler, removedGCVars))
            {
                printf("Removed GCVars: ");
                dumpConvertedVarSet(compiler, removedGCVars);
                printf("\n");
            }
        }
#endif // DEBUG

        gcInfo.gcMarkRegSetGCref(newRegGCrefSet DEBUGARG(true));
        gcInfo.gcMarkRegSetByref(newRegByrefSet DEBUGARG(true));

        /* Blocks with handlerGetsXcptnObj()==true use GT_CATCH_ARG to
           represent the exception object (TYP_REF).
           We mark REG_EXCEPTION_OBJECT as holding a GC object on entry
           to the block,  it will be the first thing evaluated
           (thanks to GTF_ORDER_SIDEEFF).
         */

        if (handlerGetsXcptnObj(block->bbCatchTyp))
        {
            for (GenTree* node : LIR::AsRange(block))
            {
                if (node->OperGet() == GT_CATCH_ARG)
                {
                    gcInfo.gcMarkRegSetGCref(RBM_EXCEPTION_OBJECT);
                    break;
                }
            }
        }

#if defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
        genInsertNopForUnwinder(block);
#endif

        /* Start a new code output block */

        genUpdateCurrentFunclet(block);

        genLogLabel(block);

        // Tell everyone which basic block we're working on

        compiler->compCurBB = block;

        block->bbEmitCookie = nullptr;

        // If this block is a jump target or it requires a label then set 'needLabel' to true,
        //
        bool needLabel = (block->bbFlags & BBF_HAS_LABEL) != 0;

        if (block == compiler->fgFirstColdBlock)
        {
#ifdef DEBUG
            if (compiler->verbose)
            {
                printf("\nThis is the start of the cold region of the method\n");
            }
#endif
            // We should never have a block that falls through into the Cold section
            noway_assert(!block->bbPrev->bbFallsThrough());

            needLabel = true;
        }

        // We also want to start a new Instruction group by calling emitAddLabel below,
        // when we need accurate bbWeights for this block in the emitter.  We force this
        // whenever our previous block was a BBJ_COND and it has a different weight than us.
        //
        // Note: We need to have set compCurBB before calling emitAddLabel
        //
        if ((block->bbPrev != nullptr) && (block->bbPrev->bbJumpKind == BBJ_COND) &&
            (block->bbWeight != block->bbPrev->bbWeight))
        {
            JITDUMP("Adding label due to BB weight difference: BBJ_COND " FMT_BB " with weight " FMT_WT
                    " different from " FMT_BB " with weight " FMT_WT "\n",
                    block->bbPrev->bbNum, block->bbPrev->bbWeight, block->bbNum, block->bbWeight);
            needLabel = true;
        }

#if FEATURE_LOOP_ALIGN
        if (GetEmitter()->emitEndsWithAlignInstr())
        {
            // we had better be planning on starting a new IG
            assert(needLabel);
        }
#endif

        if (needLabel)
        {
            // Mark a label and update the current set of live GC refs

            block->bbEmitCookie = GetEmitter()->emitAddLabel(gcInfo.gcVarPtrSetCur, gcInfo.gcRegGCrefSetCur,
                                                             gcInfo.gcRegByrefSetCur, false DEBUG_ARG(block));
        }

        if (block == compiler->fgFirstColdBlock)
        {
            // We require the block that starts the Cold section to have a label
            noway_assert(block->bbEmitCookie);
            GetEmitter()->emitSetFirstColdIGCookie(block->bbEmitCookie);
        }

        // Both stacks are always empty on entry to a basic block.
        assert(genStackLevel == 0);
        genAdjustStackLevel(block);
        savedStkLvl = genStackLevel;

        // Needed when jitting debug code
        siBeginBlock(block);

        // BBF_INTERNAL blocks don't correspond to any single IL instruction.
        if (compiler->opts.compDbgInfo && (block->bbFlags & BBF_INTERNAL) &&
            !compiler->fgBBisScratch(block)) // If the block is the distinguished first scratch block, then no need to
                                             // emit a NO_MAPPING entry, immediately after the prolog.
        {
            genIPmappingAdd((IL_OFFSETX)ICorDebugInfo::NO_MAPPING, true);
        }

        bool firstMapping = true;

#if defined(FEATURE_EH_FUNCLETS)
        if (block->bbFlags & BBF_FUNCLET_BEG)
        {
            genReserveFuncletProlog(block);
        }
#endif // FEATURE_EH_FUNCLETS

        m_liveness.BeginBlock();

        // Emit poisoning into scratch BB that comes right after prolog.
        // We cannot emit this code in the prolog as it might make the prolog too large.
        if (compiler->compShouldPoisonFrame() && compiler->fgBBisScratch(block))
        {
            genPoisonFrame(newLiveRegSet);
        }

        // Traverse the block in linear order, generating code for each node as we
        // as we encounter it.
        CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef DEBUG
        // Set the use-order numbers for each node.
        {
            int useNum = 0;
            for (GenTree* node : LIR::AsRange(block))
            {
                assert((node->gtDebugFlags & GTF_DEBUG_NODE_CG_CONSUMED) == 0);

                node->gtUseNum = -1;
                if (node->isContained() || node->IsCopyOrReload())
                {
                    continue;
                }

                for (GenTree* operand : node->Operands())
                {
                    genNumberOperandUse(operand, useNum);
                }
            }
        }
#endif // DEBUG

        IL_OFFSETX currentILOffset = BAD_IL_OFFSET;
        for (GenTree* node : LIR::AsRange(block))
        {
            // Do we have a new IL offset?
            if (node->OperGet() == GT_IL_OFFSET)
            {
                GenTreeILOffset* ilOffset = node->AsILOffset();
                genEnsureCodeEmitted(currentILOffset);
                currentILOffset = ilOffset->gtStmtILoffsx;
                genIPmappingAdd(currentILOffset, firstMapping);
                firstMapping = false;
#ifdef DEBUG
                assert(ilOffset->gtStmtLastILoffs <= compiler->info.compILCodeSize ||
                       ilOffset->gtStmtLastILoffs == BAD_IL_OFFSET);

                if (compiler->opts.dspCode && compiler->opts.dspInstrs && ilOffset->gtStmtLastILoffs != BAD_IL_OFFSET)
                {
                    while (genCurDispOffset <= ilOffset->gtStmtLastILoffs)
                    {
                        genCurDispOffset += dumpSingleInstr(compiler->info.compCode, genCurDispOffset, ">    ");
                    }
                }

#endif // DEBUG
            }

            genCodeForTreeNode(node);
            if (node->gtHasReg() && node->IsUnusedValue())
            {
                genConsumeReg(node);
            }
        } // end for each node in block

#ifdef DEBUG
        // The following set of register spill checks and GC pointer tracking checks used to be
        // performed at statement boundaries. Now, with LIR, there are no statements, so they are
        // performed at the end of each block.
        // TODO: could these checks be performed more frequently? E.g., at each location where
        // the register allocator says there are no live non-variable registers. Perhaps this could
        // be done by using the map maintained by LSRA (operandToLocationInfoMap) to mark a node
        // somehow when, after the execution of that node, there will be no live non-variable registers.

        regSet.rsSpillChk();

        /* Make sure we didn't bungle pointer register tracking */

        regMaskTP ptrRegs       = gcInfo.gcRegGCrefSetCur | gcInfo.gcRegByrefSetCur;
        regMaskTP nonVarPtrRegs = ptrRegs & ~regSet.GetMaskVars();

        // If return is a GC-type, clear it.  Note that if a common
        // epilog is generated (genReturnBB) it has a void return
        // even though we might return a ref.  We can't use the compRetType
        // as the determiner because something we are tracking as a byref
        // might be used as a return value of a int function (which is legal)
        GenTree* blockLastNode = block->lastNode();
        if ((blockLastNode != nullptr) && (blockLastNode->gtOper == GT_RETURN) &&
            (varTypeIsGC(compiler->info.compRetType) ||
             (blockLastNode->AsOp()->gtOp1 != nullptr && varTypeIsGC(blockLastNode->AsOp()->gtOp1->TypeGet()))))
        {
            nonVarPtrRegs &= ~RBM_INTRET;
        }

        if (nonVarPtrRegs)
        {
            printf("Regset after " FMT_BB " gcr=", block->bbNum);
            printRegMaskInt(gcInfo.gcRegGCrefSetCur & ~regSet.GetMaskVars());
            compiler->GetEmitter()->emitDispRegSet(gcInfo.gcRegGCrefSetCur & ~regSet.GetMaskVars());
            printf(", byr=");
            printRegMaskInt(gcInfo.gcRegByrefSetCur & ~regSet.GetMaskVars());
            compiler->GetEmitter()->emitDispRegSet(gcInfo.gcRegByrefSetCur & ~regSet.GetMaskVars());
            printf(", regVars=");
            printRegMaskInt(regSet.GetMaskVars());
            compiler->GetEmitter()->emitDispRegSet(regSet.GetMaskVars());
            printf("\n");
        }

        noway_assert(nonVarPtrRegs == RBM_NONE);
#endif // DEBUG

#if defined(DEBUG)
        if (block->bbNext == nullptr)
        {
// Unit testing of the emitter: generate a bunch of instructions into the last block
// (it's as good as any, but better than the prologue, which can only be a single instruction
// group) then use COMPlus_JitLateDisasm=* to see if the late disassembler
// thinks the instructions are the same as we do.
#if defined(TARGET_AMD64) && defined(LATE_DISASM)
            genAmd64EmitterUnitTests();
#elif defined(TARGET_ARM64)
            genArm64EmitterUnitTests();
#endif // TARGET_ARM64
        }
#endif // defined(DEBUG)

        // It is possible to reach the end of the block without generating code for the current IL offset.
        // For example, if the following IR ends the current block, no code will have been generated for
        // offset 21:
        //
        //          (  0,  0) [000040] ------------                il_offset void   IL offset: 21
        //
        //     N001 (  0,  0) [000039] ------------                nop       void
        //
        // This can lead to problems when debugging the generated code. To prevent these issues, make sure
        // we've generated code for the last IL offset we saw in the block.
        genEnsureCodeEmitted(currentILOffset);

        /* Is this the last block, and are there any open scopes left ? */

        bool isLastBlockProcessed = (block->bbNext == nullptr);
        if (block->isBBCallAlwaysPair())
        {
            isLastBlockProcessed = (block->bbNext->bbNext == nullptr);
        }

#ifdef USING_VARIABLE_LIVE_RANGE
        if (compiler->opts.compDbgInfo && isLastBlockProcessed)
        {
            varLiveKeeper->siEndAllVariableLiveRange(m_liveness.GetLiveSet());
        }
#endif // USING_VARIABLE_LIVE_RANGE

        if (compiler->opts.compScopeInfo && (compiler->info.compVarScopesCount > 0))
        {
            siEndBlock(block);

#ifdef USING_SCOPE_INFO
            if (isLastBlockProcessed && siOpenScopeList.scNext)
            {
                /* This assert no longer holds, because we may insert a throw
                   block to demarcate the end of a try or finally region when they
                   are at the end of the method.  It would be nice if we could fix
                   our code so that this throw block will no longer be necessary. */

                // noway_assert(block->bbCodeOffsEnd != compiler->info.compILCodeSize);

                siCloseAllOpenScopes();
            }
#endif // USING_SCOPE_INFO
        }

        SubtractStackLevel(savedStkLvl);

#ifdef DEBUG
        // Current live set should be equal to the liveOut set, except that we don't keep
        // it up to date for vars that are not register candidates
        // (it would be nice to have a xor set function)

        VARSET_TP mismatchLiveVars(VarSetOps::Diff(compiler, block->bbLiveOut, m_liveness.GetLiveSet()));
        VarSetOps::UnionD(compiler, mismatchLiveVars,
                          VarSetOps::Diff(compiler, m_liveness.GetLiveSet(), block->bbLiveOut));
        VarSetOps::Iter mismatchLiveVarIter(compiler, mismatchLiveVars);
        unsigned        mismatchLiveVarIndex  = 0;
        bool            foundMismatchedRegVar = false;
        while (mismatchLiveVarIter.NextElem(&mismatchLiveVarIndex))
        {
            LclVarDsc* varDsc = compiler->lvaGetDescByTrackedIndex(mismatchLiveVarIndex);
            if (varDsc->lvIsRegCandidate())
            {
                if (!foundMismatchedRegVar)
                {
                    JITDUMP("Mismatched live reg vars after " FMT_BB ":", block->bbNum);
                    foundMismatchedRegVar = true;
                }
                JITDUMP(" V%02u", compiler->lvaTrackedIndexToLclNum(mismatchLiveVarIndex));
            }
        }
        if (foundMismatchedRegVar)
        {
            JITDUMP("\n");
            assert(!"Found mismatched live reg var(s) after block");
        }
#endif

        /* Both stacks should always be empty on exit from a basic block */
        noway_assert(genStackLevel == 0);

#ifdef TARGET_AMD64
        // On AMD64, we need to generate a NOP after a call that is the last instruction of the block, in several
        // situations, to support proper exception handling semantics. This is mostly to ensure that when the stack
        // walker computes an instruction pointer for a frame, that instruction pointer is in the correct EH region.
        // The document "X64 and ARM ABIs.docx" has more details. The situations:
        // 1. If the call instruction is in a different EH region as the instruction that follows it.
        // 2. If the call immediately precedes an OS epilog. (Note that what the JIT or VM consider an epilog might
        //    be slightly different from what the OS considers an epilog, and it is the OS-reported epilog that matters
        //    here.)
        // We handle case #1 here, and case #2 in the emitter.
        if (GetEmitter()->emitIsLastInsCall())
        {
            // Ok, the last instruction generated is a call instruction. Do any of the other conditions hold?
            // Note: we may be generating a few too many NOPs for the case of call preceding an epilog. Technically,
            // if the next block is a BBJ_RETURN, an epilog will be generated, but there may be some instructions
            // generated before the OS epilog starts, such as a GS cookie check.
            if ((block->bbNext == nullptr) || !BasicBlock::sameEHRegion(block, block->bbNext))
            {
                // We only need the NOP if we're not going to generate any more code as part of the block end.

                switch (block->bbJumpKind)
                {
                    case BBJ_ALWAYS:
                    case BBJ_THROW:
                    case BBJ_CALLFINALLY:
                    case BBJ_EHCATCHRET:
                    // We're going to generate more code below anyway, so no need for the NOP.

                    case BBJ_RETURN:
                    case BBJ_EHFINALLYRET:
                    case BBJ_EHFILTERRET:
                        // These are the "epilog follows" case, handled in the emitter.

                        break;

                    case BBJ_NONE:
                        if (block->bbNext == nullptr)
                        {
                            // Call immediately before the end of the code; we should never get here    .
                            instGen(INS_BREAKPOINT); // This should never get executed
                        }
                        else
                        {
                            // We need the NOP
                            instGen(INS_nop);
                        }
                        break;

                    case BBJ_COND:
                    case BBJ_SWITCH:
                    // These can't have a call as the last instruction!

                    default:
                        noway_assert(!"Unexpected bbJumpKind");
                        break;
                }
            }
        }
#endif // TARGET_AMD64

        /* Do we need to generate a jump or return? */

        switch (block->bbJumpKind)
        {
            case BBJ_RETURN:
                genExitCode(block);
                break;

            case BBJ_THROW:
                // If we have a throw at the end of a function or funclet, we need to emit another instruction
                // afterwards to help the OS unwinder determine the correct context during unwind.
                // We insert an unexecuted breakpoint instruction in several situations
                // following a throw instruction:
                // 1. If the throw is the last instruction of the function or funclet. This helps
                //    the OS unwinder determine the correct context during an unwind from the
                //    thrown exception.
                // 2. If this is this is the last block of the hot section.
                // 3. If the subsequent block is a special throw block.
                // 4. On AMD64, if the next block is in a different EH region.
                if ((block->bbNext == nullptr) || (block->bbNext->bbFlags & BBF_FUNCLET_BEG) ||
                    !BasicBlock::sameEHRegion(block, block->bbNext) ||
                    (!isFramePointerUsed() && compiler->fgIsThrowHlpBlk(block->bbNext)) ||
                    block->bbNext == compiler->fgFirstColdBlock)
                {
                    instGen(INS_BREAKPOINT); // This should never get executed
                }
                // Do likewise for blocks that end in DOES_NOT_RETURN calls
                // that were not caught by the above rules. This ensures that
                // gc register liveness doesn't change across call instructions
                // in fully-interruptible mode.
                else
                {
                    GenTree* call = block->lastNode();

                    if ((call != nullptr) && (call->gtOper == GT_CALL))
                    {
                        if ((call->AsCall()->gtCallMoreFlags & GTF_CALL_M_DOES_NOT_RETURN) != 0)
                        {
                            instGen(INS_BREAKPOINT); // This should never get executed
                        }
                    }
                }

                break;

            case BBJ_CALLFINALLY:
                block = genCallFinally(block);
                break;

#if defined(FEATURE_EH_FUNCLETS)

            case BBJ_EHCATCHRET:
                genEHCatchRet(block);
                FALLTHROUGH;

            case BBJ_EHFINALLYRET:
            case BBJ_EHFILTERRET:
                genReserveFuncletEpilog(block);
                break;

#else // !FEATURE_EH_FUNCLETS

            case BBJ_EHCATCHRET:
                noway_assert(!"Unexpected BBJ_EHCATCHRET"); // not used on x86
                break;

            case BBJ_EHFINALLYRET:
            case BBJ_EHFILTERRET:
                genEHFinallyOrFilterRet(block);
                break;

#endif // !FEATURE_EH_FUNCLETS

            case BBJ_NONE:
            case BBJ_SWITCH:
                break;

            case BBJ_ALWAYS:
                inst_JMP(EJ_jmp, block->bbJumpDest);
                FALLTHROUGH;

            case BBJ_COND:

#if FEATURE_LOOP_ALIGN
                // This is the last place where we operate on blocks and after this, we operate
                // on IG. Hence, if we know that the destination of "block" is the first block
                // of a loop and needs alignment (it has BBF_LOOP_ALIGN), then "block" represents
                // end of the loop. Propagate that information on the IG through "igLoopBackEdge".
                //
                // During emitter, this information will be used to calculate the loop size.
                // Depending on the loop size, decision of whether to align a loop or not will be taken.
                //
                // In the emitter, we need to calculate the loop size from `block->bbJumpDest` through
                // `block` (inclusive). Thus, we need to ensure there is a label on the lexical fall-through
                // block, even if one is not otherwise needed, to be able to calculate the size of this
                // loop (loop size is calculated by walking the instruction groups; see emitter::getLoopSize()).

                if (block->bbJumpDest->isLoopAlign())
                {
                    GetEmitter()->emitSetLoopBackEdge(block->bbJumpDest);

                    if (block->bbNext != nullptr)
                    {
                        JITDUMP("Mark " FMT_BB " as label: alignment end-of-loop\n", block->bbNext->bbNum);
                        block->bbNext->bbFlags |= BBF_HAS_LABEL;
                    }
                }
#endif // FEATURE_LOOP_ALIGN

                break;

            default:
                noway_assert(!"Unexpected bbJumpKind");
                break;
        }

#if FEATURE_LOOP_ALIGN

        // If next block is the first block of a loop (identified by BBF_LOOP_ALIGN),
        // then need to add align instruction in current "block". Also mark the
        // corresponding IG with IGF_LOOP_ALIGN to know that there will be align
        // instructions at the end of that IG.
        //
        // For non-adaptive alignment, add alignment instruction of size depending on the
        // compJitAlignLoopBoundary.
        // For adaptive alignment, alignment instruction will always be of 15 bytes.

        if ((block->bbNext != nullptr) && (block->bbNext->isLoopAlign()))
        {
            assert(ShouldAlignLoops());

            GetEmitter()->emitLoopAlignment();
        }
#endif

#if defined(DEBUG) && defined(USING_VARIABLE_LIVE_RANGE)
        if (compiler->verbose)
        {
            varLiveKeeper->dumpBlockVariableLiveRanges(block);
        }
#endif // defined(DEBUG) && defined(USING_VARIABLE_LIVE_RANGE)

        INDEBUG(compiler->compCurBB = nullptr);

    } //------------------ END-FOR each block of the method -------------------

    // There could be variables alive at this point. For example see lvaKeepAliveAndReportThis.
    // This call is for cleaning the GC refs
    m_liveness.ChangeLife(this, VarSetOps::MakeEmpty(compiler));

    /* Finalize the spill  tracking logic */

    regSet.rsSpillEnd();

    /* Finalize the temp   tracking logic */

    regSet.tmpEnd();

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("\n# ");
        printf("compCycleEstimate = %6d, compSizeEstimate = %5d ", compiler->compCycleEstimate,
               compiler->compSizeEstimate);
        printf("%s\n", compiler->info.compFullName);
    }
#endif
}

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                         Register Management                               XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

//------------------------------------------------------------------------
// genSpillVar: Spill a local variable
//
// Assumptions:
//    The lclVar must be a register candidate (lvRegCandidate)

void CodeGen::genSpillVar(GenTreeLclVar* tree)
{
    unsigned   varNum = tree->GetLclNum();
    LclVarDsc* varDsc = compiler->lvaGetDesc(varNum);

    assert(varDsc->lvIsRegCandidate());

    // We don't actually need to spill if it is already living in memory
    bool needsSpill = ((tree->gtFlags & GTF_VAR_DEF) == 0 && varDsc->lvIsInReg());
    if (needsSpill)
    {
        // In order for a lclVar to have been allocated to a register, it must not have been aliasable, and can
        // therefore be store-normalized (rather than load-normalized). In fact, not performing store normalization
        // can lead to problems on architectures where a lclVar may be allocated to a register that is not
        // addressable at the granularity of the lclVar's defined type (e.g. x86).
        var_types lclType = varDsc->GetActualRegisterType();
        emitAttr  size    = emitTypeSize(lclType);

        // If this is a write-thru or a single-def variable, we don't actually spill at a use,
        // but we will kill the var in the reg (below).
        if (!varDsc->IsAlwaysAliveInMemory())
        {
            instruction storeIns = ins_Store(lclType, compiler->lvaIsSimdTypedLocalAligned(varNum));
            assert(varDsc->GetRegNum() == tree->GetRegNum());
            inst_TT_RV(storeIns, size, tree, tree->GetRegNum());
        }

        // We should only have both SPILL (i.e. the flag causing this method to be called) and
        // SPILLED on a write-thru/single-def def, for which we should not be calling this method.
        assert(!tree->IsRegSpilled(0));

        // Remove the live var from the register.
        genUpdateRegLife(varDsc, /*isBorn*/ false, /*isDying*/ true DEBUGARG(tree));
        gcInfo.gcMarkRegSetNpt(varDsc->lvRegMask());

        if (varDsc->HasStackGCPtrLiveness())
        {
#ifdef DEBUG
            if (!VarSetOps::IsMember(compiler, gcInfo.gcVarPtrSetCur, varDsc->lvVarIndex))
            {
                JITDUMP("GC pointer V%02u becoming live on stack\n", varNum);
            }
            else
            {
                JITDUMP("GC pointer V%02u continuing live on stack\n", varNum);
            }
#endif
            VarSetOps::AddElemD(compiler, gcInfo.gcVarPtrSetCur, varDsc->lvVarIndex);
        }
    }

    tree->SetRegSpill(0, false);

    // If this is NOT a write-thru, reset the var location.
    if (!tree->IsRegSpilled(0))
    {
        varDsc->SetRegNum(REG_STK);
        if (varTypeIsMultiReg(tree))
        {
            varDsc->SetOtherReg(REG_STK);
        }
    }
    else
    {
        // We only have SPILL and SPILLED on a def of a write-thru lclVar
        // or a single-def var that is to be spilled at its definition.
        assert((varDsc->IsAlwaysAliveInMemory()) && ((tree->gtFlags & GTF_VAR_DEF) != 0));
    }

#ifdef USING_VARIABLE_LIVE_RANGE
    if (needsSpill)
    {
        // We need this after "lvRegNum" has change because now we are sure that varDsc->lvIsInReg() is false.
        // "SiVarLoc" constructor uses the "LclVarDsc" of the variable.
        varLiveKeeper->siUpdateVariableLiveRange(varDsc, varNum);
    }
#endif // USING_VARIABLE_LIVE_RANGE
}

//------------------------------------------------------------------------
// genUpdateVarReg: Update the current register location for a multi-reg lclVar
//
// Arguments:
//    varDsc   - the LclVarDsc for the lclVar
//    tree     - the lclVar node
//    regIndex - the index of the register in the node
//
// inline
void CodeGen::genUpdateVarReg(LclVarDsc* varDsc, GenTree* tree, int regIndex)
{
    // This should only be called for multireg lclVars.
    assert(compiler->lvaEnregMultiRegVars);
    assert(tree->IsMultiRegLclVar() || (tree->gtOper == GT_COPY));
    varDsc->SetRegNum(tree->GetRegNum(regIndex));
}

//------------------------------------------------------------------------
// genUpdateVarReg: Update the current register location for a lclVar
//
// Arguments:
//    varDsc - the LclVarDsc for the lclVar
//    tree   - the lclVar node
//
// inline
void CodeGen::genUpdateVarReg(LclVarDsc* varDsc, GenTree* tree)
{
    // This should not be called for multireg lclVars.
    assert((tree->OperIs(GT_LCL_VAR, GT_STORE_LCL_VAR) && !tree->IsMultiRegLclVar()) || tree->OperIs(GT_COPY));
    varDsc->SetRegNum(tree->GetRegNum());
}

//------------------------------------------------------------------------
// genUnspillLocal: Reload a register candidate local into a register, if needed.
//
// Arguments:
//     varNum    - The variable number of the local to be reloaded (unspilled).
//                 It may be a local field.
//     type      - The type of the local.
//     lclNode   - The node being unspilled. Note that for a multi-reg local,
//                 the gtLclNum will be that of the parent struct.
//     regNum    - The register that 'varNum' should be loaded to.
//     reSpill   - True if it will be immediately spilled after use.
//     isLastUse - True if this is a last use of 'varNum'.
//
// Notes:
//     The caller must have determined that this local needs to be unspilled.
void CodeGen::genUnspillLocal(
    unsigned varNum, var_types type, GenTreeLclVar* lclNode, regNumber regNum, bool reSpill, bool isLastUse)
{
    LclVarDsc* varDsc = compiler->lvaGetDesc(varNum);
    inst_set_SV_var(lclNode);
    instruction ins = ins_Load(type, compiler->lvaIsSimdTypedLocalAligned(varNum));
    GetEmitter()->emitIns_R_S(ins, emitTypeSize(type), regNum, varNum, 0);

    // TODO-Review: We would like to call:
    //      genUpdateRegLife(varDsc, /*isBorn*/ true, /*isDying*/ false DEBUGARG(tree));
    // instead of the following code, but this ends up hitting this assert:
    //      assert((regSet.GetMaskVars() & regMask) == 0);
    // due to issues with LSRA resolution moves.
    // So, just force it for now. This probably indicates a condition that creates a GC hole!
    //
    // Extra note: I think we really want to call something like gcInfo.gcUpdateForRegVarMove,
    // because the variable is not really going live or dead, but that method is somewhat poorly
    // factored because it, in turn, updates rsMaskVars which is part of RegSet not GCInfo.
    // TODO-Cleanup: This code exists in other CodeGen*.cpp files, and should be moved to CodeGenCommon.cpp.

    // Don't update the variable's location if we are just re-spilling it again.

    if (!reSpill)
    {
        varDsc->SetRegNum(regNum);

#ifdef USING_VARIABLE_LIVE_RANGE
        // We want "VariableLiveRange" inclusive on the beginning and exclusive on the ending.
        // For that we shouldn't report an update of the variable location if is becoming dead
        // on the same native offset.
        if (!isLastUse)
        {
            // Report the home change for this variable
            varLiveKeeper->siUpdateVariableLiveRange(varDsc, varNum);
        }
#endif // USING_VARIABLE_LIVE_RANGE

        if (!varDsc->IsAlwaysAliveInMemory())
        {
#ifdef DEBUG
            if (VarSetOps::IsMember(compiler, gcInfo.gcVarPtrSetCur, varDsc->lvVarIndex))
            {
                JITDUMP("Removing V%02u from gcVarPtrSetCur\n", varNum);
            }
#endif // DEBUG
            VarSetOps::RemoveElemD(compiler, gcInfo.gcVarPtrSetCur, varDsc->lvVarIndex);
        }

#ifdef DEBUG
        if (compiler->verbose)
        {
            printf("V%02u in reg ", varNum);
            varDsc->PrintVarReg();
            printf(" is becoming live  ");
            compiler->printTreeID(lclNode);
            printf("\n");
        }
#endif // DEBUG

        regSet.AddMaskVars(genGetRegMask(varDsc));
    }

    gcInfo.gcMarkRegPtrVal(regNum, type);
}

//------------------------------------------------------------------------
// genUnspillRegIfNeeded: Reload a MultiReg source value into a register, if needed
//
// Arguments:
//    tree          - the MultiReg node of interest.
//    multiRegIndex - the index of the value to reload, if needed.
//
// Notes:
//    It must *not* be a GT_LCL_VAR (those are handled separately).
//    In the normal case, the value will be reloaded into the register it
//    was originally computed into. However, if that register is not available,
//    the register allocator will have allocated a different register, and
//    inserted a GT_RELOAD to indicate the register into which it should be
//    reloaded.
//
void CodeGen::genUnspillRegIfNeeded(GenTree* tree, unsigned multiRegIndex)
{
    GenTree* unspillTree = tree;
    assert(unspillTree->IsMultiRegNode());

    if (tree->gtOper == GT_RELOAD)
    {
        unspillTree = tree->AsOp()->gtOp1;
    }

    if (!unspillTree->IsRegSpilled(multiRegIndex))
    {
        return;
    }

    regNumber dstReg = tree->GetRegNum(multiRegIndex);
    if (dstReg == REG_NA)
    {
        assert(tree->IsCopyOrReload());
        dstReg = unspillTree->GetRegNum(multiRegIndex);
    }
    if (tree->IsMultiRegLclVar())
    {
        GenTreeLclVar* lclNode     = tree->AsLclVar();
        unsigned       fieldVarNum = compiler->lvaGetDesc(lclNode)->lvFieldLclStart + multiRegIndex;
        bool           reSpill     = unspillTree->IsRegSpill(multiRegIndex);
        bool           isLastUse   = lclNode->IsLastUse(multiRegIndex);
        genUnspillLocal(fieldVarNum, compiler->lvaGetDesc(fieldVarNum)->TypeGet(), lclNode, dstReg, reSpill, isLastUse);
    }
    else
    {
        var_types dstType  = unspillTree->GetRegTypeByIndex(multiRegIndex);
        TempDsc*  t        = regSet.UnspillNodeReg(unspillTree, multiRegIndex);
        emitAttr  emitType = emitActualTypeSize(dstType);
        GetEmitter()->emitIns_R_S(ins_Load(dstType), emitType, dstReg, t->tdTempNum(), 0);
        regSet.tmpRlsTemp(t);
        gcInfo.gcMarkRegPtrVal(dstReg, dstType);
    }
}

//------------------------------------------------------------------------
// genUnspillRegIfNeeded: Reload the value into a register, if needed
//
// Arguments:
//    tree - the node of interest.
//
// Notes:
//    In the normal case, the value will be reloaded into the register it
//    was originally computed into. However, if that register is not available,
//    the register allocator will have allocated a different register, and
//    inserted a GT_RELOAD to indicate the register into which it should be
//    reloaded.
//
//    A GT_RELOAD never has a reg candidate lclVar or multi-reg lclVar as its child.
//    This is because register candidates locals always have distinct tree nodes
//    for uses and definitions. (This is unlike non-register candidate locals which
//    may be "defined" by a GT_LCL_VAR node that loads it into a register. It may
//    then have a GT_RELOAD inserted if it needs a different register, though this
//    is unlikely to happen except in stress modes.)
//
void CodeGen::genUnspillRegIfNeeded(GenTree* tree)
{
    GenTree* unspillTree = tree;
    if (tree->gtOper == GT_RELOAD)
    {
        unspillTree = tree->AsOp()->gtOp1;
    }

    if (unspillTree->IsAnyRegSpilled())
    {
        if (genIsRegCandidateLclVar(unspillTree))
        {
            // We never have a GT_RELOAD for this case.
            assert(tree == unspillTree);

            // Reset spilled flag, since we are going to load a local variable from its home location.
            unspillTree->SetRegSpilled(0, false);

            GenTreeLclVar* lcl       = unspillTree->AsLclVar();
            LclVarDsc*     varDsc    = compiler->lvaGetDesc(lcl->GetLclNum());
            var_types      spillType = varDsc->GetRegisterType(lcl);
            assert(spillType != TYP_UNDEF);

// TODO-Cleanup: The following code could probably be further merged and cleaned up.
#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
            // Load local variable from its home location.
            // In most cases the tree type will indicate the correct type to use for the load.
            // However, if it is NOT a normalizeOnLoad lclVar (i.e. NOT a small int that always gets
            // widened when loaded into a register), and its size is not the same as the actual register type
            // of the lclVar, then we need to change the type of the tree node when loading.
            // This situation happens due to "optimizations" that avoid a cast and
            // simply retype the node when using long type lclVar as an int.
            // While loading the int in that case would work for this use of the lclVar, if it is
            // later used as a long, we will have incorrectly truncated the long.
            // In the normalizeOnLoad case ins_Load will return an appropriate sign- or zero-
            // extending load.
            var_types lclActualType = varDsc->GetActualRegisterType();
            assert(lclActualType != TYP_UNDEF);
            if (spillType != lclActualType && !varTypeIsGC(spillType) && !varDsc->lvNormalizeOnLoad())
            {
                assert(!varTypeIsGC(varDsc));
                spillType = lclActualType;
            }
#elif defined(TARGET_ARM)
// No normalizing for ARM
#else
            NYI("Unspilling not implemented for this target architecture.");
#endif
            bool reSpill   = unspillTree->IsRegSpill(0);
            bool isLastUse = lcl->IsLastUse(0);
            genUnspillLocal(lcl->GetLclNum(), spillType, lcl->AsLclVar(), tree->GetRegNum(), reSpill, isLastUse);
        }
        else if (unspillTree->IsMultiRegLclVar())
        {
            // We never have a GT_RELOAD for this case.
            assert(tree == unspillTree);

            GenTreeLclVar* lclNode  = unspillTree->AsLclVar();
            LclVarDsc*     varDsc   = compiler->lvaGetDesc(lclNode->GetLclNum());
            unsigned       regCount = varDsc->lvFieldCnt;

            for (unsigned i = 0; i < regCount; ++i)
            {
                if (lclNode->IsRegSpilled(i))
                {
                    regNumber reg         = lclNode->GetRegNum(i);
                    unsigned  fieldVarNum = varDsc->lvFieldLclStart + i;
                    bool      reSpill     = lclNode->IsRegSpill(i);
                    bool      isLastUse   = lclNode->IsLastUse(i);
                    genUnspillLocal(fieldVarNum, compiler->lvaGetDesc(fieldVarNum)->TypeGet(), lclNode, reg, reSpill,
                                    isLastUse);
                }
            }
        }
        else if (unspillTree->IsMultiRegNode())
        {
            // Here we may have a GT_RELOAD, and we will need to use that node ('tree') to
            // do the unspilling if needed. However, that tree doesn't have the register
            // count, so we use 'unspillTree' for that.
            for (unsigned i = 0, count = unspillTree->GetMultiRegCount(compiler); i < count; ++i)
            {
                genUnspillRegIfNeeded(tree, i);
            }
        }
        else
        {
            var_types type = unspillTree->GetType();

            if ((type == TYP_STRUCT) && unspillTree->IsCall())
            {
                type = unspillTree->AsCall()->GetRegType(0);
            }

            // Here we may have a GT_RELOAD.
            // The spill temp allocated for it is associated with the original tree that defined the
            // register that it was spilled from.
            // So we use 'unspillTree' to recover that spill temp.
            TempDsc* t        = regSet.UnspillNodeReg(unspillTree, 0);
            emitAttr emitType = emitActualTypeSize(type);
            // Reload into the register specified by 'tree' which may be a GT_RELOAD.
            regNumber dstReg = tree->GetRegNum();
            GetEmitter()->emitIns_R_S(ins_Load(type), emitType, dstReg, t->tdTempNum(), 0);
            regSet.tmpRlsTemp(t);

            unspillTree->SetRegSpilled(0, false);
            gcInfo.gcMarkRegPtrVal(dstReg, unspillTree->TypeGet());
        }
    }
}

//------------------------------------------------------------------------
// genCopyRegIfNeeded: Copy the given node into the specified register
//
// Arguments:
//    node - The node that has been evaluated (consumed).
//    needReg - The register in which its value is needed.
//
// Notes:
//    This must be a node that has a register.
//
void CodeGen::genCopyRegIfNeeded(GenTree* node, regNumber needReg)
{
    assert((node->GetRegNum() != REG_NA) && (needReg != REG_NA));
    assert(!node->isUsedFromSpillTemp());
    inst_Mov(node->TypeGet(), needReg, node->GetRegNum(), /* canSkip */ true);
}

// Check that registers are consumed in the right order for the current node being generated.
#ifdef DEBUG
void CodeGen::genNumberOperandUse(GenTree* const operand, int& useNum) const
{
    assert(operand != nullptr);

    // Ignore argument placeholders.
    if (operand->OperGet() == GT_ARGPLACE)
    {
        return;
    }

    assert(operand->gtUseNum == -1);

    if (!operand->isContained() && !operand->IsCopyOrReload())
    {
        operand->gtUseNum = useNum;
        useNum++;
    }
    else
    {
        for (GenTree* op : operand->Operands())
        {
            genNumberOperandUse(op, useNum);
        }
    }
}

void CodeGen::genCheckConsumeNode(GenTree* const node)
{
    assert(node != nullptr);

    if (verbose)
    {
        if (node->gtUseNum == -1)
        {
            // nothing wrong if the node was not consumed
        }
        else if ((node->gtDebugFlags & GTF_DEBUG_NODE_CG_CONSUMED) != 0)
        {
            printf("Node was consumed twice:\n");
            compiler->gtDispTree(node, nullptr, nullptr, true);
        }
        else if ((lastConsumedNode != nullptr) && (node->gtUseNum < lastConsumedNode->gtUseNum))
        {
            printf("Nodes were consumed out-of-order:\n");
            compiler->gtDispTree(lastConsumedNode, nullptr, nullptr, true);
            compiler->gtDispTree(node, nullptr, nullptr, true);
        }
    }

    assert((node->OperGet() == GT_CATCH_ARG) || ((node->gtDebugFlags & GTF_DEBUG_NODE_CG_CONSUMED) == 0));
    assert((lastConsumedNode == nullptr) || (node->gtUseNum == -1) || (node->gtUseNum > lastConsumedNode->gtUseNum));

    node->gtDebugFlags |= GTF_DEBUG_NODE_CG_CONSUMED;
    lastConsumedNode = node;
}
#endif // DEBUG

//--------------------------------------------------------------------
// genConsumeReg: Do liveness update for a single register of a multireg child node
//                that is being consumed by codegen.
//
// Arguments:
//    tree          - GenTree node
//    multiRegIndex - The index of the register to be consumed
//
// Return Value:
//    Returns the reg number for the given multiRegIndex.
//
regNumber CodeGen::genConsumeReg(GenTree* tree, unsigned multiRegIndex)
{
    regNumber reg = tree->GetRegNum(multiRegIndex);
    if (tree->OperIs(GT_COPY))
    {
        reg = genRegCopy(tree, multiRegIndex);
    }
    else if (reg == REG_NA)
    {
        assert(tree->OperIs(GT_RELOAD));
        reg = tree->gtGetOp1()->GetRegNum(multiRegIndex);
        assert(reg != REG_NA);
    }
    genUnspillRegIfNeeded(tree, multiRegIndex);

    // UpdateLifeFieldVar() will return true if local var should be spilled.
    if (tree->IsMultiRegLclVar() && m_liveness.UpdateLifeFieldVar(this, tree->AsLclVar(), multiRegIndex))
    {
        GenTreeLclVar* lcl = tree->AsLclVar();
        genSpillLocal(lcl->GetLclNum(), lcl->GetFieldTypeByIndex(compiler, multiRegIndex), lcl,
                      lcl->GetRegNum(multiRegIndex));
    }

    if (tree->gtSkipReloadOrCopy()->OperIs(GT_LCL_VAR))
    {
        GenTreeLclVar* lcl    = tree->gtSkipReloadOrCopy()->AsLclVar();
        LclVarDsc*     varDsc = compiler->lvaGetDesc(lcl);
        assert(compiler->lvaEnregMultiRegVars && lcl->IsMultiReg());
        assert(varDsc->lvPromoted && (multiRegIndex < varDsc->lvFieldCnt));
        unsigned   fieldVarNum = varDsc->lvFieldLclStart + multiRegIndex;
        LclVarDsc* fldVarDsc   = compiler->lvaGetDesc(fieldVarNum);
        assert(fldVarDsc->lvLRACandidate);
        bool isFieldDying = lcl->IsLastUse(multiRegIndex);

        if (fldVarDsc->GetRegNum() == REG_STK)
        {
            // We have loaded this into a register only temporarily
            gcInfo.gcMarkRegSetNpt(reg);
        }
        else if (isFieldDying)
        {
            gcInfo.gcMarkRegSetNpt(genRegMask(fldVarDsc->GetRegNum()));
        }
    }
    else
    {
        gcInfo.gcMarkRegSetNpt(tree->gtGetRegMask());
    }
    return reg;
}

//--------------------------------------------------------------------
// genConsumeReg: Do liveness update for a subnode that is being
// consumed by codegen.
//
// Arguments:
//    tree - GenTree node
//
// Return Value:
//    Returns the reg number of tree.
//    In case of multi-reg call node returns the first reg number
//    of the multi-reg return.
regNumber CodeGen::genConsumeReg(GenTree* tree)
{
    if (tree->OperGet() == GT_COPY)
    {
        genRegCopy(tree);
    }

    // Handle the case where we have a lclVar that needs to be copied before use (i.e. because it
    // interferes with one of the other sources (or the target, if it's a "delayed use" register)).
    // TODO-Cleanup: This is a special copyReg case in LSRA - consider eliminating these and
    // always using GT_COPY to make the lclVar location explicit.
    // Note that we have to do this before calling genUpdateLife because otherwise if we spill it
    // the lvRegNum will be set to REG_STK and we will lose track of what register currently holds
    // the lclVar (normally when a lclVar is spilled it is then used from its former register
    // location, which matches the GetRegNum() on the node).
    // (Note that it doesn't matter if we call this before or after genUnspillRegIfNeeded
    // because if it's on the stack it will always get reloaded into tree->GetRegNum()).
    if (genIsRegCandidateLclVar(tree))
    {
        LclVarDsc* varDsc = compiler->lvaGetDesc(tree->AsLclVar());
        if (varDsc->GetRegNum() != REG_STK)
        {
            var_types regType = varDsc->GetRegisterType(tree->AsLclVar());
            inst_Mov(regType, tree->GetRegNum(), varDsc->GetRegNum(), /* canSkip */ true);
        }
    }

    genUnspillRegIfNeeded(tree);

    if (tree->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        genUpdateLife(tree->AsLclVarCommon());
    }

    // there are three cases where consuming a reg means clearing the bit in the live mask
    // 1. it was not produced by a local
    // 2. it was produced by a local that is going dead
    // 3. it was produced by a local that does not live in that reg (like one allocated on the stack)

    if (genIsRegCandidateLclVar(tree))
    {
        assert(tree->gtHasReg());

        LclVarDsc* varDsc = compiler->lvaGetDesc(tree->AsLclVar());
        assert(varDsc->lvLRACandidate);

        if (varDsc->GetRegNum() == REG_STK)
        {
            // We have loaded this into a register only temporarily
            gcInfo.gcMarkRegSetNpt(genRegMask(tree->GetRegNum()));
        }
        else if ((tree->gtFlags & GTF_VAR_DEATH) != 0)
        {
            gcInfo.gcMarkRegSetNpt(genRegMask(varDsc->GetRegNum()));
        }
    }
    else if (tree->gtSkipReloadOrCopy()->IsMultiRegLclVar())
    {
        assert(compiler->lvaEnregMultiRegVars);
        GenTreeLclVar* lcl              = tree->gtSkipReloadOrCopy()->AsLclVar();
        LclVarDsc*     varDsc           = compiler->lvaGetDesc(lcl);
        unsigned       firstFieldVarNum = varDsc->lvFieldLclStart;
        for (unsigned i = 0; i < varDsc->lvFieldCnt; ++i)
        {
            LclVarDsc* fldVarDsc = &(compiler->lvaTable[firstFieldVarNum + i]);
            assert(fldVarDsc->lvLRACandidate);
            regNumber reg;
            if (tree->OperIs(GT_COPY, GT_RELOAD) && (tree->GetRegNum(i) != REG_NA))
            {
                reg = tree->GetRegNum(i);
            }
            else
            {
                reg = lcl->GetRegNum(i);
            }
            bool isFieldDying = lcl->IsLastUse(i);

            if (fldVarDsc->GetRegNum() == REG_STK)
            {
                // We have loaded this into a register only temporarily
                gcInfo.gcMarkRegSetNpt(reg);
            }
            else if (isFieldDying)
            {
                gcInfo.gcMarkRegSetNpt(genRegMask(fldVarDsc->GetRegNum()));
            }
        }
    }
    else
    {
        gcInfo.gcMarkRegSetNpt(tree->gtGetRegMask());
    }

    genCheckConsumeNode(tree);
    return tree->GetRegNum();
}

void CodeGen::genConsumeAddress(GenTree* addr)
{
    if (!addr->isContained())
    {
        genConsumeReg(addr);
    }
    else if (addr->IsAddrMode())
    {
        genConsumeOperands(addr->AsAddrMode());
    }
}

void CodeGen::genConsumeRegs(GenTree* tree)
{
#if !defined(TARGET_64BIT)
    if (tree->OperGet() == GT_LONG)
    {
        genConsumeRegs(tree->gtGetOp1());
        genConsumeRegs(tree->gtGetOp2());
        return;
    }
#endif // !defined(TARGET_64BIT)

    if (tree->isUsedFromSpillTemp())
    {
        // spill temps are un-tracked and hence no need to update life
        return;
    }

    if (!tree->isContained())
    {
        genConsumeReg(tree);
        return;
    }

    if (tree->OperIsIndir())
    {
        genConsumeAddress(tree->AsIndir()->GetAddr());
        return;
    }

    if (tree->IsAddrMode())
    {
        genConsumeOperands(tree->AsAddrMode());
        return;
    }

    if (tree->OperIs(GT_BITCAST))
    {
        genConsumeReg(tree->AsUnOp()->GetOp(0));
        return;
    }

    if (tree->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        // A contained lcl var must be living on stack and marked as reg optional, or not be a
        // register candidate.
        LclVarDsc* varDsc = compiler->lvaGetDesc(tree->AsLclVarCommon());

        noway_assert(varDsc->GetRegNum() == REG_STK);
        noway_assert(tree->IsRegOptional() || !varDsc->lvLRACandidate);

        genUpdateLife(tree->AsLclVarCommon());

        return;
    }

#ifdef FEATURE_HW_INTRINSICS
    if (GenTreeHWIntrinsic* hwi = tree->IsHWIntrinsic())
    {
        if (hwi->GetNumOps() != 0)
        {
            HWIntrinsicCategory category = HWIntrinsicInfo::lookupCategory(hwi->GetIntrinsic());
            assert((category == HW_Category_MemoryLoad) || (category == HW_Category_MemoryStore));
            genConsumeAddress(hwi->GetOp(0));
            if (category == HW_Category_MemoryStore)
            {
                assert(hwi->IsBinary());
                genConsumeReg(hwi->GetOp(1));
            }
            else
            {
                assert(hwi->IsUnary());
            }
        }

        return;
    }
#endif // FEATURE_HW_INTRINSICS

    assert(tree->OperIsLeaf());
}

void CodeGen::genConsumeOperands(GenTreeOp* tree)
{
    GenTree* firstOp  = tree->gtOp1;
    GenTree* secondOp = tree->gtOp2;

    if (firstOp != nullptr)
    {
        genConsumeRegs(firstOp);
    }
    if (secondOp != nullptr)
    {
        genConsumeRegs(secondOp);
    }
}

#ifdef FEATURE_HW_INTRINSICS
void CodeGen::genConsumeHWIntrinsicOperands(GenTreeHWIntrinsic* node)
{
    for (GenTreeHWIntrinsic::Use& use : node->Uses())
    {
        genConsumeRegs(use.GetNode());
    }
}
#endif

//------------------------------------------------------------------------
// genPutArgStkFieldList: Generate code for a putArgStk whose source is a GT_FIELD_LIST
//
// Arguments:
//    fieldList     - The list of fields to store to the stack
//    outArgLclNum  - The local variable where outgoing arguments are stored
//    outArgLclOffs - The offset of the argument in the argument area
//    outArgLclSize - The size of the argument area
//
// Notes:
//    The x86 version of this is in codegenxarch.cpp, and doesn't take an
//    outArgLclNum, as it pushes its args onto the stack.
//
//    For fast tail calls the outgoing argument area is actually the method's
//    own incoming argument area.
//
#ifndef TARGET_X86
void CodeGen::genPutArgStkFieldList(GenTreePutArgStk* putArg,
                                    unsigned          outArgLclNum,
                                    unsigned outArgLclOffs DEBUGARG(unsigned outArgLclSize))
{
    regNumber tmpReg = putArg->AvailableTempRegCount() ? putArg->GetSingleTempReg() : REG_NA;

    for (GenTreeFieldList::Use& use : putArg->GetOp(0)->AsFieldList()->Uses())
    {
        unsigned dstOffset = outArgLclOffs + use.GetOffset();

        GenTree*  src     = use.GetNode();
        var_types srcType = use.GetType();
        regNumber srcReg;

        assert((dstOffset + varTypeSize(srcType)) <= outArgLclSize);

#ifdef FEATURE_SIMD
        if (srcType == TYP_SIMD12)
        {
            genStoreSIMD12(GenAddrMode(outArgLclNum, dstOffset), src, tmpReg);
            continue;
        }
#endif

#ifdef TARGET_ARM64
        if (src->isContained())
        {
            assert(src->IsIntegralConst(0) || src->IsDblConPositiveZero());
            srcReg = REG_ZR;
        }
        else
#endif
        {
            srcReg = genConsumeReg(src);
        }

        GetEmitter()->emitIns_S_R(ins_Store(srcType), emitTypeSize(srcType), srcReg, outArgLclNum, dstOffset);
    }
}
#endif // !TARGET_X86

void CodeGen::ConsumeStructStore(
    GenTree* store, ClassLayout* layout, regNumber dstReg, regNumber srcReg, regNumber sizeReg)
{
    assert(store->OperIs(GT_STORE_OBJ, GT_STORE_BLK, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    // We have to consume the registers, and perform any copies, in the actual execution order: dst, src, size.
    //
    // Note that the register allocator ensures that the registers ON THE NODES will not interfere
    // with one another if consumed (i.e. reloaded or moved to their ASSIGNED reg) in execution order.
    // Further, it ensures that they will not interfere with one another if they are then copied
    // to the REQUIRED register (if a fixed register requirement) in execution order.  This requires,
    // then, that we first consume all the operands, then do any necessary moves.

    GenTree* dstAddr = nullptr;
    GenTree* src;

    if (store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
    {
        src = store->AsLclVarCommon()->GetOp(0);
    }
    else
    {
        dstAddr = store->AsIndir()->GetAddr();
        src     = store->AsIndir()->GetValue();

        genConsumeReg(dstAddr);
    }

    if (src->OperIs(GT_INIT_VAL))
    {
        assert(src->isContained());

        src = src->AsUnOp()->GetOp(0);
    }
    else if (src->OperIs(GT_IND, GT_OBJ, GT_BLK))
    {
        assert(src->isContained());

        src = src->AsIndir()->GetAddr();
    }
    else if (src->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        assert(src->isContained());
    }
    else
    {
        assert(src->OperIs(GT_CNS_INT));
    }

    if (!src->isContained())
    {
        genConsumeReg(src);
    }

    // Copy registers as needed

    if (dstAddr != nullptr)
    {
        genCopyRegIfNeeded(dstAddr, dstReg);
    }

    if (!src->isContained())
    {
        genCopyRegIfNeeded(src, srcReg);
    }

    if (dstAddr == nullptr)
    {
        assert(store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

        unsigned lclNum  = store->AsLclVarCommon()->GetLclNum();
        unsigned lclOffs = store->AsLclVarCommon()->GetLclOffs();

        GetEmitter()->emitIns_R_S(INS_lea, EA_PTRSIZE, dstReg, lclNum, lclOffs);
    }

    if (src->isContained())
    {
        assert(src->OperIs(GT_LCL_VAR, GT_LCL_FLD));

        unsigned lclNum  = src->AsLclVarCommon()->GetLclNum();
        unsigned lclOffs = src->AsLclVarCommon()->GetLclOffs();

        GetEmitter()->emitIns_R_S(INS_lea, EA_PTRSIZE, srcReg, lclNum, lclOffs);
    }

    if (sizeReg != REG_NA)
    {
        assert(store->HasTempReg(sizeReg));

        genSetRegToIcon(sizeReg, layout->GetSize());
    }
}

void CodeGen::ConsumeDynBlk(GenTreeDynBlk* store, regNumber dstReg, regNumber srcReg, regNumber sizeReg)
{
    GenTree* addr  = store->GetAddr();
    GenTree* value = store->GetValue();
    GenTree* size  = store->GetSize();

    genConsumeReg(addr);
    genConsumeReg(value);
    genConsumeReg(size);

    genCopyRegIfNeeded(addr, dstReg);
    genCopyRegIfNeeded(value, srcReg);
    genCopyRegIfNeeded(size, sizeReg);
}

//-------------------------------------------------------------------------
// genSpillLocal: Generate the actual spill of a local var.
//
// Arguments:
//     varNum    - The variable number of the local to be spilled.
//                 It may be a local field.
//     type      - The type of the local.
//     lclNode   - The node being spilled. Note that for a multi-reg local,
//                 the gtLclNum will be that of the parent struct.
//     regNum    - The register that 'varNum' is currently in.
//
// Return Value:
//     None.
//
void CodeGen::genSpillLocal(unsigned varNum, var_types type, GenTreeLclVar* lclNode, regNumber regNum)
{
    const LclVarDsc* varDsc = compiler->lvaGetDesc(varNum);
    assert(!varDsc->lvNormalizeOnStore() || (type == varDsc->GetActualRegisterType()));

    // We have a register candidate local that is marked with SPILL.
    // This flag generally means that we need to spill this local.
    // The exception is the case of a use of an EH/spill-at-single-def var use that is being "spilled"
    // to the stack, indicated by SPILL (note that all EH lclVar defs are always
    // spilled, i.e. write-thru. Likewise, single-def vars that are spilled at its definitions).
    // An EH or single-def var use is always valid on the stack (so we don't need to actually spill it),
    // but the SPILL flag records the fact that the register value is going dead.
    if (((lclNode->gtFlags & GTF_VAR_DEF) != 0) || (!varDsc->IsAlwaysAliveInMemory()))
    {
        // Store local variable to its home location.
        // Ensure that lclVar stores are typed correctly.
        GetEmitter()->emitIns_S_R(ins_Store(type, compiler->lvaIsSimdTypedLocalAligned(varNum)), emitTypeSize(type),
                                  regNum, varNum, 0);
    }
}

//-------------------------------------------------------------------------
// genProduceReg: do liveness update for register produced by the current
// node in codegen after code has been emitted for it.
//
// Arguments:
//     tree   -  Gentree node
//
// Return Value:
//     None.
void CodeGen::genProduceReg(GenTree* tree)
{
#ifndef TARGET_64BIT
    assert(!tree->IsMultiRegOpLong());
#endif
    assert((tree->gtDebugFlags & GTF_DEBUG_NODE_CG_PRODUCED) == 0);
    INDEBUG(tree->gtDebugFlags |= GTF_DEBUG_NODE_CG_PRODUCED;)

    if (tree->IsAnyRegSpill())
    {
        // Code for GT_COPY node gets generated as part of consuming regs by its parent.
        // A GT_COPY node in turn produces reg result and it should never be marked to
        // spill.
        //
        // Similarly GT_RELOAD node gets generated as part of consuming regs by its
        // parent and should never be marked for spilling.
        noway_assert(!tree->IsCopyOrReload());

        if (genIsRegCandidateLclVar(tree))
        {
            GenTreeLclVar*   lclNode   = tree->AsLclVar();
            const LclVarDsc* varDsc    = compiler->lvaGetDesc(lclNode);
            const unsigned   varNum    = lclNode->GetLclNum();
            const var_types  spillType = varDsc->GetRegisterType(lclNode);
            genSpillLocal(varNum, spillType, lclNode, tree->GetRegNum());
        }
        else if (tree->IsMultiRegLclVar())
        {
            assert(compiler->lvaEnregMultiRegVars);

            GenTreeLclVar*   lclNode  = tree->AsLclVar();
            const LclVarDsc* varDsc   = compiler->lvaGetDesc(lclNode);
            const unsigned   regCount = lclNode->GetFieldCount(compiler);

            for (unsigned i = 0; i < regCount; ++i)
            {
                if (lclNode->IsRegSpill(i))
                {
                    const regNumber reg         = lclNode->GetRegNum(i);
                    const unsigned  fieldVarNum = varDsc->lvFieldLclStart + i;
                    const var_types spillType   = compiler->lvaGetDesc(fieldVarNum)->GetRegisterType();
                    genSpillLocal(fieldVarNum, spillType, lclNode, reg);
                }
            }
        }
        else
        {
            // In case of multi-reg call node, spill flag on call node
            // indicates that one or more of its allocated regs need to
            // be spilled.  Call node needs to be further queried to
            // know which of its result regs needs to be spilled.
            if (tree->IsMultiRegCall())
            {
                regSet.SpillNodeRegs(tree, tree->AsCall()->GetRegCount());
            }
#ifdef TARGET_ARM
            else if (GenTreePutArgSplit* argSplit = tree->IsPutArgSplit())
            {
                regSet.SpillNodeRegs(tree, argSplit->GetRegCount());
            }
#endif
            else
            {
                regSet.SpillNodeReg(tree, 0);
            }

            return;
        }
    }

    if (tree->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
    {
        genUpdateLife(tree->AsLclVarCommon());
    }

    // If we've produced a register, mark it as a pointer, as needed.
    if (tree->gtHasReg())
    {
        // We only mark the register in the following cases:
        // 1. It is not a register candidate local. In this case, we're producing a
        //    register from a local, but the local is not a register candidate. Thus,
        //    we must be loading it as a temp register, and any "last use" flag on
        //    the register wouldn't be relevant.
        // 2. The register candidate local is going dead. There's no point to mark
        //    the register as live, with a GC pointer, if the variable is dead.
        if (!genIsRegCandidateLclVar(tree) || ((tree->gtFlags & GTF_VAR_DEATH) == 0))
        {
            // Multi-reg nodes will produce more than one register result.
            // Mark all the regs produced by the node.
            if (tree->IsMultiRegCall())
            {
                GenTreeCall* call = tree->AsCall();

                for (unsigned i = 0; i < call->GetRegCount(); ++i)
                {
                    regNumber reg  = call->GetRegNum(i);
                    var_types type = call->GetRegType(i);
                    gcInfo.gcMarkRegPtrVal(reg, type);
                }
            }
            else if (tree->IsCopyOrReloadOfMultiRegCall())
            {
                // we should never see reload of multi-reg call here
                // because GT_RELOAD gets generated in reg consuming path.
                noway_assert(tree->OperGet() == GT_COPY);

                // A multi-reg GT_COPY node produces those regs to which
                // copy has taken place.
                const GenTreeCopyOrReload* copy = tree->AsCopyOrReload();
                const GenTreeCall*         call = copy->GetOp(0)->AsCall();

                for (unsigned i = 0; i < call->GetRegCount(); ++i)
                {
                    var_types type  = call->GetRegType(i);
                    regNumber toReg = copy->GetRegNum(i);

                    if (toReg != REG_NA)
                    {
                        gcInfo.gcMarkRegPtrVal(toReg, type);
                    }
                }
            }
            else if (tree->IsMultiRegLclVar())
            {
                assert(compiler->lvaEnregMultiRegVars);
                GenTreeLclVar* lclNode  = tree->AsLclVar();
                LclVarDsc*     varDsc   = compiler->lvaGetDesc(lclNode->GetLclNum());
                unsigned       regCount = varDsc->lvFieldCnt;
                for (unsigned i = 0; i < regCount; i++)
                {
                    if (!lclNode->IsLastUse(i))
                    {
                        regNumber reg = lclNode->GetRegNum(i);
                        if (reg != REG_NA)
                        {
                            var_types type = compiler->lvaGetDesc(varDsc->lvFieldLclStart + i)->TypeGet();
                            gcInfo.gcMarkRegPtrVal(reg, type);
                        }
                    }
                }
            }
            else
            {
                gcInfo.gcMarkRegPtrVal(tree->GetRegNum(), tree->TypeGet());
            }
        }
    }
}

#ifndef TARGET_64BIT
void CodeGen::DefLongRegs(GenTreeMultiRegOp* node)
{
    assert(node->TypeIs(TYP_LONG) && (node->GetRegCount() == 2));
    assert((node->GetRegType(0) == TYP_INT) && (node->GetRegType(1) == TYP_INT));
    assert((node->gtDebugFlags & GTF_DEBUG_NODE_CG_PRODUCED) == 0);
    INDEBUG(node->gtDebugFlags |= GTF_DEBUG_NODE_CG_PRODUCED;)

    if (node->IsRegSpill(0))
    {
        regSet.SpillNodeReg(node, 0);
    }

    if (node->IsRegSpill(1))
    {
        regSet.SpillNodeReg(node, 1);
    }

    gcInfo.gcMarkRegSetNpt(genRegMask(node->GetRegNum(0)) | genRegMask(node->GetRegNum(1)));
}
#endif // TARGET_64BIT

// transfer gc/byref status of src reg to dst reg
void CodeGen::genTransferRegGCState(regNumber dst, regNumber src)
{
    regMaskTP srcMask = genRegMask(src);
    regMaskTP dstMask = genRegMask(dst);

    if (gcInfo.gcRegGCrefSetCur & srcMask)
    {
        gcInfo.gcMarkRegSetGCref(dstMask);
    }
    else if (gcInfo.gcRegByrefSetCur & srcMask)
    {
        gcInfo.gcMarkRegSetByref(dstMask);
    }
    else
    {
        gcInfo.gcMarkRegSetNpt(dstMask);
    }
}

// generates an ip-relative call or indirect call via reg ('call reg')
//     pass in 'addr' for a relative call or 'base' for a indirect register call
//     methHnd - optional, only used for pretty printing
//     retSize - emitter type of return for GC purposes, should be EA_BYREF, EA_GCREF, or EA_PTRSIZE(not GC)
//
// clang-format off
void CodeGen::genEmitCall(int                   callType,
                          CORINFO_METHOD_HANDLE methHnd,
                          INDEBUG_LDISASM_COMMA(CORINFO_SIG_INFO* sigInfo)
                          void*                 addr
                          X86_ARG(int argSize),
                          emitAttr              retSize
                          MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(emitAttr secondRetSize),
                          IL_OFFSETX            ilOffset,
                          regNumber             base,
                          bool                  isJump)
{
#if !defined(TARGET_X86)
    int argSize = 0;
#endif // !defined(TARGET_X86)
    GetEmitter()->emitIns_Call(emitter::EmitCallType(callType),
                               methHnd,
                               INDEBUG_LDISASM_COMMA(sigInfo)
                               addr,
                               argSize,
                               retSize
                               MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(secondRetSize),
                               gcInfo.gcVarPtrSetCur,
                               gcInfo.gcRegGCrefSetCur,
                               gcInfo.gcRegByrefSetCur,
                               ilOffset, base, REG_NA, 0, 0, isJump);
}
// clang-format on

// generates an indirect call via addressing mode (call []) given an indir node
//     methHnd - optional, only used for pretty printing
//     retSize - emitter type of return for GC purposes, should be EA_BYREF, EA_GCREF, or EA_PTRSIZE(not GC)
//
// clang-format off
void CodeGen::genEmitCall(int                   callType,
                          CORINFO_METHOD_HANDLE methHnd,
                          INDEBUG_LDISASM_COMMA(CORINFO_SIG_INFO* sigInfo)
                          GenTreeIndir*         indir
                          X86_ARG(int argSize),
                          emitAttr              retSize
                          MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(emitAttr secondRetSize),
                          IL_OFFSETX            ilOffset)
{
#if !defined(TARGET_X86)
    int argSize = 0;
#endif // !defined(TARGET_X86)
    genConsumeAddress(indir->Addr());

    GetEmitter()->emitIns_Call(emitter::EmitCallType(callType),
                               methHnd,
                               INDEBUG_LDISASM_COMMA(sigInfo)
                               nullptr,
                               argSize,
                               retSize
                               MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(secondRetSize),
                               gcInfo.gcVarPtrSetCur,
                               gcInfo.gcRegGCrefSetCur,
                               gcInfo.gcRegByrefSetCur,
                               ilOffset,
                               (indir->Base()  != nullptr) ? indir->Base()->GetRegNum()  : REG_NA,
                               (indir->Index() != nullptr) ? indir->Index()->GetRegNum() : REG_NA,
                               indir->Scale(),
                               indir->Offset());
}
// clang-format on

//------------------------------------------------------------------------
// genCodeForCast: Generates the code for GT_CAST.
//
// Arguments:
//    cast - the GT_CAST node.
//
void CodeGen::genCodeForCast(GenTreeCast* cast)
{
    if (varTypeIsFloating(cast->GetType()) && varTypeIsFloating(cast->GetOp(0)->GetType()))
    {
        genFloatToFloatCast(cast);
    }
    else if (varTypeIsFloating(cast->GetOp(0)->GetType()))
    {
        genFloatToIntCast(cast);
    }
    else if (varTypeIsFloating(cast->GetType()))
    {
        genIntToFloatCast(cast);
    }
#ifndef TARGET_64BIT
    else if (varTypeIsLong(cast->GetOp(0)->GetType()))
    {
        genLongToIntCast(cast);
    }
#endif
    else
    {
        genIntToIntCast(cast);
    }
}

CodeGen::GenIntCastDesc::GenIntCastDesc(GenTreeCast* cast)
{
    GenTree* src = cast->GetOp(0);

    const var_types srcType      = genActualType(src->TypeGet());
    const bool      srcUnsigned  = cast->IsUnsigned();
    const unsigned  srcSize      = genTypeSize(srcType);
    const var_types castType     = cast->gtCastType;
    const bool      castUnsigned = varTypeIsUnsigned(castType);
    const unsigned  castSize     = genTypeSize(castType);
    const var_types dstType      = genActualType(cast->TypeGet());
    const unsigned  dstSize      = genTypeSize(dstType);
    const bool      overflow     = cast->gtOverflow();

    assert((srcSize == 4) || (srcSize == genTypeSize(TYP_I_IMPL)));
    assert((dstSize == 4) || (dstSize == genTypeSize(TYP_I_IMPL)));

    assert(dstSize == genTypeSize(genActualType(castType)));

    if (castSize < 4) // Cast to small int type
    {
        if (overflow)
        {
            m_checkKind    = CHECK_SMALL_INT_RANGE;
            m_checkSrcSize = srcSize;
            // Since these are small int types we can compute the min and max
            // values of the castType without risk of integer overflow.
            const int castNumBits = (castSize * 8) - (castUnsigned ? 0 : 1);
            m_checkSmallIntMax    = (1 << castNumBits) - 1;
            m_checkSmallIntMin    = (castUnsigned | srcUnsigned) ? 0 : (-m_checkSmallIntMax - 1);

            m_extendKind    = COPY;
            m_extendSrcSize = dstSize;
        }
        else
        {
            m_checkKind = CHECK_NONE;

            // Casting to a small type really means widening from that small type to INT/LONG.
            m_extendKind    = castUnsigned ? ZERO_EXTEND_SMALL_INT : SIGN_EXTEND_SMALL_INT;
            m_extendSrcSize = castSize;
        }
    }
#ifdef TARGET_64BIT
    // castType cannot be (U)LONG on 32 bit targets, such casts should have been decomposed.
    // srcType cannot be a small int type since it's the "actual type" of the cast operand.
    // This means that widening casts do not occur on 32 bit targets.
    else if (castSize > srcSize) // (U)INT to (U)LONG widening cast
    {
        assert((srcSize == 4) && (castSize == 8));

        if (overflow && !srcUnsigned && castUnsigned)
        {
            // Widening from INT to ULONG, check if the value is positive
            m_checkKind    = CHECK_POSITIVE;
            m_checkSrcSize = 4;

            // This is the only overflow checking cast that requires changing the
            // source value (by zero extending), all others copy the value as is.
            assert((srcType == TYP_INT) && (castType == TYP_ULONG));
            m_extendKind    = ZERO_EXTEND_INT;
            m_extendSrcSize = 4;
        }
        else
        {
            m_checkKind = CHECK_NONE;

            m_extendKind    = srcUnsigned ? ZERO_EXTEND_INT : SIGN_EXTEND_INT;
            m_extendSrcSize = 4;
        }
    }
    else if (castSize < srcSize) // (U)LONG to (U)INT narrowing cast
    {
        assert((srcSize == 8) && (castSize == 4));

        if (overflow)
        {
            if (castUnsigned) // (U)LONG to UINT cast
            {
                m_checkKind = CHECK_UINT_RANGE;
            }
            else if (srcUnsigned) // ULONG to INT cast
            {
                m_checkKind = CHECK_POSITIVE_INT_RANGE;
            }
            else // LONG to INT cast
            {
                m_checkKind = CHECK_INT_RANGE;
            }

            m_checkSrcSize = 8;
        }
        else
        {
            m_checkKind = CHECK_NONE;
        }

        m_extendKind    = COPY;
        m_extendSrcSize = 4;
    }
#endif
    else // if (castSize == srcSize) // Sign changing or same type cast
    {
        assert(castSize == srcSize);

        if (overflow && (srcUnsigned != castUnsigned))
        {
            m_checkKind    = CHECK_POSITIVE;
            m_checkSrcSize = srcSize;
        }
        else
        {
            m_checkKind = CHECK_NONE;
        }

        m_extendKind    = COPY;
        m_extendSrcSize = srcSize;
    }

    if (src->isUsedFromMemory())
    {
        bool     memUnsigned = varTypeIsUnsigned(src->GetType());
        unsigned memSize     = genTypeSize(src->GetType());

        if (m_checkKind != CHECK_NONE)
        {
            // For overflow checking casts the memory load is performed as usual and
            // the cast only checks if the resulting TYP_(U)INT/TYP_(U)LONG value is
            // within the cast type's range.
            //
            // There is one specific case that normally requires sign extending the
            // loaded value - casting TYP_INT to TYP_ULONG. But this isn't needed:
            //   - the overflow check guarantees that the TYP_INT value is positive
            //     so zero extend can be used instead
            //   - this case is 64 bit specific and both x64 and arm64 zero extend
            //     while loading (even when using SIGN_EXTEND_SMALL_INT because the
            //     the value is positive)
            //
            // Other TYP_(U)INT/TYP_(U)LONG widening casts do not require overflow
            // checks so they are not handled here.

            if (memSize < 4)
            {
                m_loadKind = memUnsigned ? LOAD_ZERO_EXTEND_SMALL_INT : LOAD_SIGN_EXTEND_SMALL_INT;
            }
            else
            {
                m_loadKind = LOAD;
            }

            m_loadSrcSize = memSize;

            m_extendKind = COPY;
        }
        else
        {
            if (castSize <= memSize)
            {
                // If we have a narrowing cast then we can narrow the memory load itself.
                // The upper bits contained in the wider memory location and the sign/zero
                // extension bits that a small type load would have produced are anyway
                // discarded by the cast.
                //
                // Handle the sign changing cast as well, just to pick up the sign of the
                // cast type (see the memSize < 4 case below).
                //
                // This effectively turns all casts into widening or sign changing casts.
                memSize     = castSize;
                memUnsigned = castUnsigned;
            }

            if (memSize < 4)
            {
                m_loadKind    = memUnsigned ? LOAD_ZERO_EXTEND_SMALL_INT : LOAD_SIGN_EXTEND_SMALL_INT;
                m_loadSrcSize = memSize;

                // Most of the time the load itself is sufficient, even on 64 bit where we
                // may need to widen directly to 64 bit (CodeGen needs to ensure that 64 bit
                // instructions are used on 64 bit targets). But there are 2 exceptions that
                // involve loading signed values and then zero extending:

                // Loading a TYP_BYTE and then casting to TYP_USHORT - bits 8-15 will contain
                // the sign bit of the original value while bits 16-31/63 will be 0. There's
                // no single instruction that does this so we'll need to emit an extra zero
                // extend to zero out bits 16-31/63.
                if ((memSize == 1) && !memUnsigned && (castType == TYP_USHORT))
                {
                    assert(m_extendKind == ZERO_EXTEND_SMALL_INT);
                    assert(m_extendSrcSize == 2);
                }
#ifdef TARGET_64BIT
                // Loading a small signed value and then zero extending to TYP_LONG - on x64
                // this requires a 32 bit MOVSX but the emitter lacks support for it so we'll
                // need to emit a 32 bit MOV to zero out the upper 32 bits. This kind of
                // signed/unsigned mix should be rare.
                else if (!memUnsigned && (castSize == 8) && srcUnsigned)
                {
                    assert(m_extendKind == ZERO_EXTEND_INT);
                    assert(m_extendSrcSize == 4);
                }
#endif
                // Various other combinations do not need extra instructions. For example:
                // - Unsigned value load and sign extending cast - if the value is unsigned
                //   then sign extending is in fact zero extending.
                // - TYP_SHORT value load and cast to TYP_UINT - that's a TYP_INT to TYP_UINT
                //   cast that's basically a NOP.
                else
                {
                    m_extendKind = COPY;
                }
            }
#ifdef TARGET_64BIT
            else if ((memSize == 4) && !srcUnsigned && (castSize == 8))
            {
                m_loadKind    = LOAD_SIGN_EXTEND_INT;
                m_loadSrcSize = memSize;

                m_extendKind = COPY;
            }
#endif
            else
            {
                m_loadKind    = LOAD;
                m_loadSrcSize = memSize;

                m_extendKind = COPY;
            }
        }
    }
}

#ifndef TARGET_64BIT

void CodeGen::GenStoreLclVarLong(GenTreeLclVar* store)
{
    LclVarDsc* lcl = compiler->lvaGetDesc(store);

    assert(lcl->GetType() == TYP_LONG);
    assert(!lcl->IsPromoted());

    GenTree*  src = store->GetOp(0);
    regNumber loSrcReg;
    regNumber hiSrcReg;

    if (src->OperIs(GT_LONG))
    {
        assert(src->isContained());

        loSrcReg = genConsumeReg(src->AsOp()->GetOp(0));
        hiSrcReg = genConsumeReg(src->AsOp()->GetOp(1));
    }
    else
    {
        assert(src->GetMultiRegCount(compiler) == 2);

        genConsumeRegs(src);

        loSrcReg = src->GetRegNum(0);
        hiSrcReg = src->GetRegNum(1);

        assert(genIsValidIntReg(loSrcReg));
        assert(genIsValidIntReg(hiSrcReg));
    }

    GetEmitter()->emitIns_S_R(ins_Store(TYP_INT), EA_4BYTE, loSrcReg, store->GetLclNum(), 0);
    GetEmitter()->emitIns_S_R(ins_Store(TYP_INT), EA_4BYTE, hiSrcReg, store->GetLclNum(), 4);
}

#endif

//------------------------------------------------------------------------
// genCodeForJumpTrue: Generate code for a GT_JTRUE node.
//
// Arguments:
//    jtrue - The node
//
void CodeGen::genCodeForJumpTrue(GenTreeOp* jtrue)
{
    assert(compiler->compCurBB->bbJumpKind == BBJ_COND);
    assert(jtrue->OperIs(GT_JTRUE));

    GenTreeOp*   relop     = jtrue->gtGetOp1()->AsOp();
    GenCondition condition = GenCondition::FromRelop(relop);

    if (condition.PreferSwap())
    {
        condition = GenCondition::Swap(condition);
    }

#if defined(TARGET_XARCH)
    if ((condition.GetCode() == GenCondition::FNEU) &&
        (relop->gtGetOp1()->GetRegNum() == relop->gtGetOp2()->GetRegNum()) &&
        !relop->gtGetOp1()->isUsedFromSpillTemp() && !relop->gtGetOp2()->isUsedFromSpillTemp())
    {
        // For floating point, `x != x` is a common way of
        // checking for NaN. So, in the case where both
        // operands are the same, we can optimize codegen
        // to only do a single check.

        condition = GenCondition(GenCondition::P);
    }
#endif

    inst_JCC(condition, compiler->compCurBB->bbJumpDest);
}

//------------------------------------------------------------------------
// genCodeForJcc: Generate code for a GT_JCC node.
//
// Arguments:
//    jcc - The node
//
void CodeGen::genCodeForJcc(GenTreeCC* jcc)
{
    assert(compiler->compCurBB->bbJumpKind == BBJ_COND);
    assert(jcc->OperIs(GT_JCC));

    inst_JCC(jcc->gtCondition, compiler->compCurBB->bbJumpDest);
}

//------------------------------------------------------------------------
// inst_JCC: Generate a conditional branch instruction sequence.
//
// Arguments:
//   condition - The branch condition
//   target    - The basic block to jump to when the condition is true
//
void CodeGen::inst_JCC(GenCondition condition, BasicBlock* target)
{
    const GenConditionDesc& desc = GenConditionDesc::Get(condition);

    if (desc.oper == GT_NONE)
    {
        inst_JMP(desc.jumpKind1, target);
    }
    else if (desc.oper == GT_OR)
    {
        inst_JMP(desc.jumpKind1, target);
        inst_JMP(desc.jumpKind2, target);
    }
    else // if (desc.oper == GT_AND)
    {
        BasicBlock* labelNext = genCreateTempLabel();
        inst_JMP(emitter::emitReverseJumpKind(desc.jumpKind1), labelNext);
        inst_JMP(desc.jumpKind2, target);
        genDefineTempLabel(labelNext);
    }
}

//------------------------------------------------------------------------
// genCodeForSetcc: Generate code for a GT_SETCC node.
//
// Arguments:
//    setcc - The node
//
void CodeGen::genCodeForSetcc(GenTreeCC* setcc)
{
    assert(setcc->OperIs(GT_SETCC));

    inst_SETCC(setcc->gtCondition, setcc->TypeGet(), setcc->GetRegNum());
    genProduceReg(setcc);
}

void CodeGen::genCodeForLclAddr(GenTreeLclVarCommon* node)
{
    assert(node->OperIs(GT_LCL_FLD_ADDR, GT_LCL_VAR_ADDR));
    assert(node->TypeIs(TYP_BYREF, TYP_I_IMPL));

    inst_RV_TT(INS_lea, emitTypeSize(node->GetType()), node->GetRegNum(), node);
    genProduceReg(node);
}

#ifdef DEBUG
bool CodeGen::IsValidSourceType(var_types instrType, var_types sourceType)
{
    switch (varActualType(instrType))
    {
        case TYP_INT:
        case TYP_LONG:
        case TYP_REF:
        case TYP_BYREF:
            return varTypeIsIntegralOrI(sourceType) &&
                   (varTypeSize(varActualType(sourceType)) >= varTypeSize(instrType));

        case TYP_FLOAT:
        case TYP_DOUBLE:
            return sourceType == instrType;

#ifdef FEATURE_SIMD
        case TYP_SIMD8:
        case TYP_SIMD12:
        case TYP_SIMD16:
        case TYP_SIMD32:
            return varTypeIsSIMD(sourceType) && (varTypeSize(sourceType) >= varTypeSize(instrType));
#endif

        default:
            return false;
    }
}
#endif
