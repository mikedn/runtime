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
#include "lsra.h"

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

    assert(regSet.rsSpillChk());

#if !FEATURE_FIXED_OUT_ARGS
    // We initialize the stack level before first "BasicBlock" code is generated in case we need to report stack
    // variable needs home and so its stack offset.
    SetStackLevel(0);
#endif

    m_liveness.Begin();
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
#ifdef DEBUG
    if (compiler->opts.disAsm)
    {
        DumpDisasmHeader();
    }

    if (compiler->opts.disAsm || compiler->opts.dspCode || verbose)
    {
        compiler->lvaTableDump();
    }

    // You have to be careful if you create basic blocks from now on
    compiler->fgSafeBasicBlockCreation = false;

#ifdef TARGET_XARCH
    // Check stack pointer on return stress mode is not compatible with fully interruptible GC. REVIEW: why?
    // It is also not compatible with any function that makes a tailcall: we aren't smart enough to only
    // insert the SP check in the non-tailcall returns.
    if ((GetInterruptible() || compiler->compTailCallUsed) && (compiler->lvaReturnSpCheck != BAD_VAR_NUM))
    {
        compiler->lvaReturnSpCheck = BAD_VAR_NUM;
    }
#ifdef TARGET_X86
    // Check stack pointer on call stress mode is not compatible with fully interruptible GC. REVIEW: why?
    if (GetInterruptible() && (compiler->lvaCallSpCheck != BAD_VAR_NUM))
    {
        compiler->lvaCallSpCheck = BAD_VAR_NUM;
    }
#endif // TARGET_X86
#endif // TARGET_XARCH
#endif // DEBUG

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

        gcInfo.ClearLiveLclRegs();
        gcInfo.gcRegGCrefSetCur = RBM_NONE;
        gcInfo.gcRegByrefSetCur = RBM_NONE;

        m_lsra->recordVarLocationsAtStartOfBB(block);

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

                if (varDsc->TypeIs(TYP_REF))
                {
                    newRegGCrefSet |= varDsc->lvRegMask();
                }
                else if (varDsc->TypeIs(TYP_BYREF))
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

        gcInfo.SetLiveLclRegs(newLiveRegSet);

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

#if !FEATURE_FIXED_OUT_ARGS
        assert(genStackLevel == 0);
        genAdjustStackLevel(block);
        unsigned savedStkLvl = genStackLevel;
#endif

        // Needed when jitting debug code
        siBeginBlock(block);

        // BBF_INTERNAL blocks don't correspond to any single IL instruction.
        if (compiler->opts.compDbgInfo && (block->bbFlags & BBF_INTERNAL) &&
            !compiler->fgBBisScratch(block)) // If the block is the distinguished first scratch block, then no need to
                                             // emit a NO_MAPPING entry, immediately after the prolog.
        {
            genIPmappingAdd((IL_OFFSETX)ICorDebugInfo::NO_MAPPING, true);
        }

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

        // Traverse the block in linear order, generating code for each node as we
        // as we encounter it.

        IL_OFFSETX currentILOffset = BAD_IL_OFFSET;
        bool       firstMapping    = true;

        for (GenTree* node : LIR::AsRange(block))
        {
            if (GenTreeILOffset* ilOffset = node->IsILOffset())
            {
                genEnsureCodeEmitted(currentILOffset);
                currentILOffset = ilOffset->gtStmtILoffsx;
                genIPmappingAdd(currentILOffset, firstMapping);
                firstMapping = false;
            }
            else
            {
                genCodeForTreeNode(node);

                if (node->gtHasReg() && node->IsUnusedValue())
                {
                    UseRegs(node);
                }
            }
        }

#ifdef DEBUG
        // The following set of register spill checks and GC pointer tracking checks used to be
        // performed at statement boundaries. Now, with LIR, there are no statements, so they are
        // performed at the end of each block.
        // TODO: could these checks be performed more frequently? E.g., at each location where
        // the register allocator says there are no live non-variable registers. Perhaps this could
        // be done by using the map maintained by LSRA (operandToLocationInfoMap) to mark a node
        // somehow when, after the execution of that node, there will be no live non-variable registers.

        assert(regSet.rsSpillChk());

        /* Make sure we didn't bungle pointer register tracking */

        regMaskTP ptrRegs       = gcInfo.gcRegGCrefSetCur | gcInfo.gcRegByrefSetCur;
        regMaskTP liveLclRegs   = gcInfo.GetLiveLclRegs();
        regMaskTP nonVarPtrRegs = ptrRegs & ~liveLclRegs;

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

        if (nonVarPtrRegs != RBM_NONE)
        {
            printf("Regset after " FMT_BB " gcr=", block->bbNum);
            printRegMaskInt(gcInfo.gcRegGCrefSetCur & ~liveLclRegs);
            emitter::emitDispRegSet(gcInfo.gcRegGCrefSetCur & ~liveLclRegs);
            printf(", byr=");
            printRegMaskInt(gcInfo.gcRegByrefSetCur & ~liveLclRegs);
            emitter::emitDispRegSet(gcInfo.gcRegByrefSetCur & ~liveLclRegs);
            printf(", regVars=");
            printRegMaskInt(liveLclRegs);
            emitter::emitDispRegSet(liveLclRegs);
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

#if !FEATURE_FIXED_OUT_ARGS
        SubtractStackLevel(savedStkLvl);
#endif

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
            if (varDsc->IsRegCandidate())
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

#if !FEATURE_FIXED_OUT_ARGS
        // Both stacks should always be empty on exit from a basic block.
        noway_assert(genStackLevel == 0);
#endif

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

    INDEBUG(regSet.tmpEnd());

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

void CodeGen::SpillRegCandidateLclVar(GenTreeLclVar* lclVar)
{
    LclVarDsc* lcl = compiler->lvaGetDesc(lclVar);

    assert(lcl->IsRegCandidate());
    assert(lclVar->IsRegSpill(0));

    // We don't actually need to spill if it is already living in memory
    bool needsSpill = ((lclVar->gtFlags & GTF_VAR_DEF) == 0) && (lcl->GetRegNum() != REG_STK);

    if (needsSpill)
    {
        // We should only have both SPILL (i.e. the flag causing this method to be called) and
        // SPILLED on a write-thru/single-def def, for which we should not be calling this method.
        assert(!lclVar->IsRegSpilled(0));

        unsigned lclNum = lclVar->GetLclNum();

        // If this is a write-thru or a single-def variable, we don't actually spill at a use,
        // but we will kill the var in the reg (below).
        if (!lcl->IsAlwaysAliveInMemory())
        {
            assert(lcl->GetRegNum() == lclVar->GetRegNum());

            // In order for a local to have been allocated to a register, it must not have been address
            // exposed, and can therefore be store-normalized (rather than load-normalized).
            // In fact, not performing store normalization can lead to problems on architectures where
            // a local may be allocated to a register that is not addressable at the granularity of the
            // local's defined type (e.g. x86).
            var_types   type = lcl->GetActualRegisterType();
            instruction ins  = ins_Store(type, IsSimdLocalAligned(lclNum));

            GetEmitter()->emitIns_S_R(ins, emitTypeSize(type), lclVar->GetRegNum(), lclNum, 0);
        }

        // Remove the live var from the register.
        genUpdateRegLife(lcl, /*isBorn*/ false, /*isDying*/ true DEBUGARG(lclVar));
        gcInfo.gcMarkRegSetNpt(lcl->lvRegMask());

        if (lcl->HasStackGCPtrLiveness())
        {
            if (!VarSetOps::IsMember(compiler, gcInfo.gcVarPtrSetCur, lcl->GetLivenessBitIndex()))
            {
                JITDUMP("GC pointer V%02u becoming live on stack\n", lclNum);
            }
            else
            {
                JITDUMP("GC pointer V%02u continuing live on stack\n", lclNum);
            }

            VarSetOps::AddElemD(compiler, gcInfo.gcVarPtrSetCur, lcl->GetLivenessBitIndex());
        }
    }

    lclVar->SetRegSpill(0, false);

    // If this is NOT a write-thru, reset the var location.
    if (!lclVar->IsRegSpilled(0))
    {
        lcl->SetRegNum(REG_STK);
    }
    else
    {
        // We only have SPILL and SPILLED on a def of a write-thru lclVar
        // or a single-def var that is to be spilled at its definition.
        assert(lcl->IsAlwaysAliveInMemory() && ((lclVar->gtFlags & GTF_VAR_DEF) != 0));
    }

#ifdef USING_VARIABLE_LIVE_RANGE
    if (needsSpill)
    {
        // We need this after "lvRegNum" has change because now we are sure that varDsc->lvIsInReg() is false.
        // "SiVarLoc" constructor uses the "LclVarDsc" of the variable.
        varLiveKeeper->siUpdateVariableLiveRange(lcl, lclVar->GetLclNum());
    }
#endif
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

regNumber CodeGen::UseReg(GenTree* node)
{
    assert(node->isUsedFromReg() && !node->IsMultiRegNode());

    if (GenTreeLclVar* lclVar = IsRegCandidateLclVar(node))
    {
        return UseRegCandidateLclVar(lclVar);
    }

    if (node->OperIs(GT_COPY))
    {
        CopyReg(node->AsCopyOrReload());
    }
    else
    {
        UnspillRegIfNeeded(node);
    }

    if (node->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        genUpdateLife(node->AsLclVarCommon());
    }

    gcInfo.gcMarkRegSetNpt(genRegMask(node->GetRegNum()));

    genCheckConsumeNode(node);

    return node->GetRegNum();
}

regNumber CodeGen::UseRegCandidateLclVar(GenTreeLclVar* node)
{
    assert(IsRegCandidateLclVar(node));

    LclVarDsc* lcl = compiler->lvaGetDesc(node);

    // Handle the case where we have a lclVar that needs to be copied before use (i.e. because it
    // interferes with one of the other sources (or the target, if it's a "delayed use" register)).
    // TODO-Cleanup: This is a special copyReg case in LSRA - consider eliminating these and
    // always using GT_COPY to make the lclVar location explicit.
    // Note that we have to do this before calling genUpdateLife because otherwise if we spill it
    // the lvRegNum will be set to REG_STK and we will lose track of what register currently holds
    // the lclVar (normally when a lclVar is spilled it is then used from its former register
    // location, which matches the GetRegNum() on the node).
    // (Note that it doesn't matter if we call this before or after UnspillRegIfNeeded
    // because if it's on the stack it will always get reloaded into tree->GetRegNum()).
    if (lcl->GetRegNum() != REG_STK)
    {
        var_types dstType = lcl->GetRegisterType(node->AsLclVar());
        inst_Mov(dstType, node->GetRegNum(), lcl->GetRegNum(), /* canSkip */ true);
    }

    if (node->IsAnyRegSpilled())
    {
        UnspillRegCandidateLclVar(node);
    }

    genUpdateLife(node);

    assert(node->GetRegNum() != REG_NA);

    if (lcl->GetRegNum() == REG_STK)
    {
        // We have loaded this into a register only temporarily
        gcInfo.gcMarkRegSetNpt(genRegMask(node->GetRegNum()));
    }
    else if (node->IsLastUse(0))
    {
        gcInfo.gcMarkRegSetNpt(genRegMask(lcl->GetRegNum()));
    }

    genCheckConsumeNode(node);

    return node->GetRegNum();
}

// This will copy the register produced by this node's source, to the register
// allocated to this GT_COPY node. It has some special handling for these cases:
//  - when the source and target registers are in different register files
//    (note that this is *not* a conversion).
//  - when the source is a lclVar whose home location is being moved to a new
//    register (rather than just being copied for temporary use).
//
void CodeGen::CopyReg(GenTreeCopyOrReload* copy)
{
    assert(copy->OperIs(GT_COPY) && !copy->IsMultiRegNode() && !copy->IsAnyRegSpill());

    GenTree*  src     = copy->GetOp(0);
    regNumber srcReg  = UseReg(src);
    regNumber dstReg  = copy->GetRegNum();
    var_types dstType = copy->GetType();

    inst_Mov(dstType, dstReg, srcReg, /* canSkip */ false);

    if (src->OperIs(GT_LCL_VAR))
    {
        // If it is a last use, the local will be killed by UseReg, as usual, and DefReg will
        // appropriately set the gcInfo for the copied value.
        // If not, there are two cases we need to handle:
        // - If this is a TEMPORARY copy (indicated by the GTF_VAR_DEATH flag) the variable
        //   will remain live in its original register.
        //   DefReg will appropriately set the gcInfo for the copied value,
        //   and UseReg will reset it.
        // - Otherwise, we need to update register info for the lclVar.

        if (!src->IsLastUse(0) && !copy->IsLastUse(0))
        {
            LclVarDsc* lcl = compiler->lvaGetDesc(src->AsLclVar());

            // If we didn't just spill it (in UseReg, above), then update the register info
            if (lcl->GetRegNum() != REG_STK)
            {
                genUpdateRegLife(lcl, /*isBorn*/ false, /*isDying*/ true DEBUGARG(src));
                gcInfo.gcMarkRegSetNpt(genRegMask(src->GetRegNum()));
                lcl->SetRegNum(copy->GetRegNum());
#ifdef USING_VARIABLE_LIVE_RANGE
                varLiveKeeper->siUpdateVariableLiveRange(lcl, src->AsLclVar()->GetLclNum());
#endif
                genUpdateRegLife(lcl, /*isBorn*/ true, /*isDying*/ false DEBUGARG(copy));
            }
        }
    }

    gcInfo.gcMarkRegPtrVal(dstReg, dstType);
}

// Reload the value into a register, if needed
//
// In the normal case, the value will be reloaded into the register it
// was originally computed into. However, if that register is not available,
// the register allocator will have allocated a different register, and
// inserted a GT_RELOAD to indicate the register into which it should be
// reloaded.
//
// A GT_RELOAD never has a reg candidate lclVar or multi-reg lclVar as its child.
// This is because register candidates locals always have distinct tree nodes
// for uses and definitions. (This is unlike non-register candidate locals which
// may be "defined" by a GT_LCL_VAR node that loads it into a register. It may
// then have a GT_RELOAD inserted if it needs a different register, though this
// is unlikely to happen except in stress modes.)
//
void CodeGen::UnspillRegIfNeeded(GenTree* node)
{
    assert(!node->IsMultiRegNode() && !IsRegCandidateLclVar(node));

    GenTree* unspillNode = node->OperIs(GT_RELOAD) ? node->AsUnOp()->GetOp(0) : node;

    if (unspillNode->IsAnyRegSpilled())
    {
        UnspillNodeReg(unspillNode, node->GetRegNum(), 0);
    }
}

void CodeGen::UnspillRegCandidateLclVar(GenTreeLclVar* node)
{
    assert(IsRegCandidateLclVar(node) && node->IsAnyRegSpilled());

    // Reset spilled flag, since we are going to load a local variable from its home location.
    node->SetRegSpilled(0, false);

    LclVarDsc* lcl     = compiler->lvaGetDesc(node);
    var_types  regType = lcl->GetRegisterType(node);

    assert(regType != TYP_UNDEF);

#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
    // TODO-MIKE-Review: This stuff is dubious...

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
    var_types lclActualType = lcl->GetActualRegisterType();
    assert(lclActualType != TYP_UNDEF);
    if (regType != lclActualType && !varTypeIsGC(regType) && !lcl->lvNormalizeOnLoad())
    {
        assert(!varTypeIsGC(lcl->GetType()));
        regType = lclActualType;
    }
#endif

    regNumber dstReg = node->GetRegNum();
    unsigned  lclNum = node->GetLclNum();

    instruction ins = ins_Load(regType, IsSimdLocalAligned(lclNum));
    GetEmitter()->emitIns_R_S(ins, emitTypeSize(regType), dstReg, lclNum, 0);

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

    if (!node->IsRegSpill(0))
    {
        lcl->SetRegNum(dstReg);

#ifdef USING_VARIABLE_LIVE_RANGE
        // We want "VariableLiveRange" inclusive on the beginning and exclusive on the ending.
        // For that we shouldn't report an update of the variable location if is becoming dead
        // on the same native offset.
        if (!node->IsLastUse(0))
        {
            varLiveKeeper->siUpdateVariableLiveRange(lcl, lclNum);
        }
#endif

        if (!lcl->IsAlwaysAliveInMemory())
        {
#ifdef DEBUG
            if (VarSetOps::IsMember(compiler, gcInfo.gcVarPtrSetCur, lcl->GetLivenessBitIndex()))
            {
                JITDUMP("Removing V%02u from gcVarPtrSetCur\n", lclNum);
            }
#endif

            VarSetOps::RemoveElemD(compiler, gcInfo.gcVarPtrSetCur, lcl->GetLivenessBitIndex());
        }

        JITDUMP("V%02u in reg %s is becoming live at [%06u]\n", lclNum, getRegName(lcl->GetRegNum()), node->GetID());

        gcInfo.AddLiveLclRegs(genGetRegMask(lcl));
    }

    gcInfo.gcMarkRegPtrVal(dstReg, regType);
}

regNumber CodeGen::UseReg(GenTree* node, unsigned regIndex)
{
    assert(node->IsMultiRegNode() && !node->gtSkipReloadOrCopy()->IsMultiRegLclVar());

    regNumber reg = node->GetRegNum(regIndex);

    if (node->OperIs(GT_COPY))
    {
        reg = CopyReg(node->AsCopyOrReload(), regIndex);
    }
    else if (reg == REG_NA)
    {
        assert(node->OperIs(GT_RELOAD));
        reg = node->AsUnOp()->GetOp(0)->GetRegNum(regIndex);
        node->SetRegNum(regIndex, reg);
    }

    assert(reg != REG_NA);

    UnspillRegIfNeeded(node, regIndex);

    // TODO-MIKE-Review: This kills ALL the multireg node's registers.
    // Seems unnecessary and confusing, it's likely enough to kill
    // only the specific register we're dealing with now. Oh well,
    // the whole GC info tracking is a bunch of crap to begin with.
    gcInfo.gcMarkRegSetNpt(node->gtGetRegMask());

    return reg;
}

// This will copy the corresponding register produced by this node's source, to
// the register allocated to the register specified by this GT_COPY node.
// A multireg copy doesn't support moving between register files, as the GT_COPY
// node does not retain separate types for each index.
//
regNumber CodeGen::CopyReg(GenTreeCopyOrReload* copy, unsigned regIndex)
{
    assert(copy->OperIs(GT_COPY));
    assert(!copy->IsAnyRegSpill());

    GenTree* src = copy->GetOp(0);

    assert(src->IsMultiRegNode() && !src->IsMultiRegLclVar());
    assert(regIndex < src->GetMultiRegCount(compiler));

    // TODO-MIKE-Cleanup: This is recursive for no obvious reason...
    UseReg(src, regIndex);

    regNumber srcReg = src->GetRegNum(regIndex);
    regNumber dstReg = copy->GetRegNum(regIndex);

    // Not all registers of a multireg COPY need copying.
    if (dstReg == REG_NA)
    {
        copy->SetRegNum(regIndex, srcReg);
        return srcReg;
    }

    assert(srcReg != dstReg);

    var_types type = src->GetMultiRegType(compiler, regIndex);
    inst_Mov(type, dstReg, srcReg, /* canSkip */ false);

    gcInfo.gcMarkRegPtrVal(dstReg, type);

    return dstReg;
}

// Reload a MultiReg source value into a register, if needed
//
// It must *not* be a GT_LCL_VAR (those are handled separately).
// In the normal case, the value will be reloaded into the register it
// was originally computed into. However, if that register is not available,
// the register allocator will have allocated a different register, and
// inserted a GT_RELOAD to indicate the register into which it should be
// reloaded.
//
void CodeGen::UnspillRegIfNeeded(GenTree* node, unsigned regIndex)
{
    assert(node->IsMultiRegNode() && !node->gtSkipReloadOrCopy()->IsMultiRegLclVar());

    GenTree* unspillNode = node->OperIs(GT_RELOAD) ? node->AsUnOp()->GetOp(0) : node;

    if (!unspillNode->IsRegSpilled(regIndex))
    {
        return;
    }

    regNumber reg = node->GetRegNum(regIndex);

    if (reg == REG_NA)
    {
        assert(node->IsCopyOrReload());
        reg = unspillNode->GetRegNum(regIndex);
        node->SetRegNum(regIndex, reg);
    }

    UnspillNodeReg(unspillNode, reg, regIndex);
}

void CodeGen::UseRegs(GenTree* node)
{
    if (!node->IsMultiRegNode())
    {
        UseReg(node);
        return;
    }

    assert(!node->gtSkipReloadOrCopy()->OperIs(GT_LCL_VAR));

    if (node->OperIs(GT_COPY))
    {
        CopyRegs(node->AsCopyOrReload());
    }

    UnspillRegsIfNeeded(node);

    gcInfo.gcMarkRegSetNpt(node->gtGetRegMask());

    genCheckConsumeNode(node);
}

void CodeGen::UnspillRegsIfNeeded(GenTree* node)
{
    GenTree* unspillNode = node->OperIs(GT_RELOAD) ? node->AsUnOp()->GetOp(0) : node;

    assert(unspillNode->IsMultiRegNode() && !unspillNode->IsMultiRegLclVar());

    if (unspillNode->IsAnyRegSpilled())
    {
        for (unsigned i = 0, count = node->GetMultiRegCount(compiler); i < count; ++i)
        {
            UnspillRegIfNeeded(node, i);
        }
    }
}

void CodeGen::CopyRegs(GenTreeCopyOrReload* copy)
{
    assert(copy->OperIs(GT_COPY) && copy->IsMultiRegNode());

    // Register allocation assumes that any reload and copy are done in operand order.
    // That is, we can have:
    //     (reg0, reg1) = COPY(V0, V1) where V0 is in reg1 and V1 is in memory
    // The register allocation model assumes that copies are done one by one:
    //     reg0 = V0 ; V1 can't be in reg0 because it is still live,
    //     reg1 = V1
    // There should never be any circular dependencies, and we will check that here.

    GenTree* src      = copy->GetOp(0);
    unsigned regCount = src->GetMultiRegCount(compiler);

#ifdef DEBUG
    regMaskTP busyRegs = RBM_NONE;

    for (unsigned i = 0; i < regCount; ++i)
    {
        if (!src->IsRegSpilled(i))
        {
            busyRegs |= genRegMask(src->GetRegNum(i));
        }
    }
#endif

    for (unsigned i = 0; i < regCount; ++i)
    {
        INDEBUG(regNumber srcReg =) src->GetRegNum(i);
        INDEBUG(regNumber dstReg =) CopyReg(copy, i);

#ifdef DEBUG
        if (dstReg != srcReg)
        {
            assert((busyRegs & genRegMask(dstReg)) == 0);
            busyRegs &= ~genRegMask(srcReg);
        }

        busyRegs |= genRegMask(dstReg);
#endif
    }
}

regNumber CodeGen::genConsumeReg(GenTree* node)
{
    return UseReg(node);
}

void CodeGen::genConsumeAddress(GenTree* addr)
{
    if (!addr->isContained())
    {
        UseReg(addr);
    }
    else if (GenTreeAddrMode* am = addr->IsAddrMode())
    {
        if (GenTree* base = am->GetBase())
        {
            UseReg(base);
        }

        if (GenTree* index = am->GetIndex())
        {
            UseReg(index);
        }
    }
}

bool CodeGen::IsValidContainedLcl(GenTreeLclVarCommon* node)
{
    // A contained local must be living on stack and marked as reg optional,
    // or not be a register candidate.
    // TODO-MIKE-Review: If it's reg optional it probably needs to be spilled too...
    LclVarDsc* lcl = compiler->lvaGetDesc(node);

    return (lcl->GetRegNum() == REG_STK) && (node->IsRegOptional() || !lcl->IsRegCandidate());
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
        UseReg(tree);
        return;
    }

    if (tree->OperIsIndir())
    {
        genConsumeAddress(tree->AsIndir()->GetAddr());
        return;
    }

    if (tree->IsAddrMode())
    {
        genConsumeAddress(tree);
        return;
    }

    if (tree->OperIs(GT_BITCAST))
    {
        UseReg(tree->AsUnOp()->GetOp(0));
        return;
    }

    if (tree->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        IsValidContainedLcl(tree->AsLclVarCommon());
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
                UseReg(hwi->GetOp(1));
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

#ifdef TARGET_XARCH
        GetEmitter()->emitIns_R_I(INS_mov, EA_4BYTE, sizeReg, static_cast<ssize_t>(layout->GetSize()));
#else
        instGen_Set_Reg_To_Imm(EA_4BYTE, sizeReg, static_cast<ssize_t>(layout->GetSize()));
#endif
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

void CodeGen::SpillNodeReg(GenTree* node, var_types regType, unsigned regIndex)
{
    assert(!node->IsMultiRegLclVar());
    assert(!varTypeIsMultiReg(regType));
    assert(node->IsRegSpill(regIndex));

    regNumber reg  = node->GetRegNum(regIndex);
    TempDsc*  temp = regSet.AllocSpillTemp(node, reg, regType);

    JITDUMP("Spilling register %s after [%06u]\n", getRegName(reg), node->GetID());

    regType          = temp->GetType();
    instruction ins  = ins_Store(regType);
    emitAttr    attr = emitActualTypeSize(regType);

    GetEmitter()->emitIns_S_R(ins, attr, reg, temp->tdTempNum(), 0);

    node->SetRegSpill(regIndex, false);
    node->SetRegSpilled(regIndex, true);

    gcInfo.gcMarkRegSetNpt(genRegMask(reg));
}

#ifdef TARGET_X86
void CodeGen::SpillST0(GenTree* node)
{
    var_types type = node->GetType();
    regNumber reg  = node->GetRegNum();
    TempDsc*  temp = regSet.AllocSpillTemp(node, reg, type);

    JITDUMP("Spilling register ST0 after [%06u]\n", node->GetID());

    GetEmitter()->emitIns_S(INS_fstp, emitTypeSize(type), temp->tdTempNum(), 0);

    node->SetRegSpill(0, false);
    node->SetRegSpilled(0, true);
}
#endif // TARGET_X86

void CodeGen::UnspillNodeReg(GenTree* node, regNumber reg, unsigned regIndex)
{
    assert(!node->IsCopyOrReload());
    assert(!node->IsMultiRegLclVar());

    regNumber         oldReg = node->GetRegNum(regIndex);
    RegSet::SpillDsc* prevDsc;
    RegSet::SpillDsc* spillDsc = regSet.rsGetSpillInfo(node, oldReg, &prevDsc);
    TempDsc*          temp     = regSet.rsGetSpillTempWord(oldReg, spillDsc, prevDsc);

    node->SetRegSpilled(regIndex, false);

    JITDUMP("Unspilling register %s from [%06u]\n", getRegName(oldReg), node->GetID());

    var_types   regType = temp->GetType();
    instruction ins     = ins_Load(regType);
    emitAttr    attr    = emitActualTypeSize(regType);

    GetEmitter()->emitIns_R_S(ins, attr, reg, temp->GetTempNum(), 0);

    regSet.tmpRlsTemp(temp);

    gcInfo.gcMarkRegPtrVal(reg, regType);
}

#ifdef TARGET_X86
void CodeGen::UnspillST0(GenTree* node)
{
    regNumber         oldReg = node->GetRegNum();
    RegSet::SpillDsc* prevDsc;
    RegSet::SpillDsc* spillDsc = regSet.rsGetSpillInfo(node, oldReg, &prevDsc);
    TempDsc*          temp     = regSet.rsGetSpillTempWord(oldReg, spillDsc, prevDsc);

    node->SetRegSpilled(0, false);

    JITDUMP("Unspilling ST0 from [%06u]\n", getRegName(oldReg), node->GetID());

    var_types regType = temp->GetType();
    GetEmitter()->emitIns_S(INS_fld, emitTypeSize(regType), temp->GetTempNum(), 0);
    regSet.tmpRlsTemp(temp);
}
#endif // TARGET_X86

void CodeGen::genProduceReg(GenTree* node)
{
    DefReg(node);
}

void CodeGen::DefReg(GenTree* node)
{
    assert(!node->OperIs(GT_STORE_LCL_FLD, GT_STORE_LCL_VAR, GT_LCL_VAR, GT_CALL));
#if FEATURE_ARG_SPLIT
    assert(!node->IsPutArgSplit());
#endif
#ifndef TARGET_64BIT
    assert(!node->IsMultiRegOpLong());
#endif
    assert(!node->IsCopyOrReload());
    assert((node->gtDebugFlags & GTF_DEBUG_NODE_CG_PRODUCED) == 0);
    INDEBUG(node->gtDebugFlags |= GTF_DEBUG_NODE_CG_PRODUCED;)

    if (node->IsAnyRegSpill())
    {
        SpillNodeReg(node, node->GetType(), 0);
    }
    // TODO-MIKE-Review: This check is likely bogus, nodes that use this function
    // likely always have a reg...
    else if (node->GetRegNum() != REG_NA)
    {
        gcInfo.gcMarkRegPtrVal(node->GetRegNum(), node->GetType());
    }
}

void CodeGen::DefLclVarReg(GenTreeLclVar* lclVar)
{
    assert(lclVar->OperIs(GT_LCL_VAR, GT_STORE_LCL_VAR) && !lclVar->IsMultiReg());
    assert((lclVar->gtDebugFlags & GTF_DEBUG_NODE_CG_PRODUCED) == 0);
    INDEBUG(lclVar->gtDebugFlags |= GTF_DEBUG_NODE_CG_PRODUCED;)

    LclVarDsc* lcl = compiler->lvaGetDesc(lclVar);

    assert(!lcl->IsIndependentPromoted());

    if (lclVar->IsAnyRegSpill())
    {
        if (lcl->IsRegCandidate())
        {
            unsigned  lclNum    = lclVar->GetLclNum();
            var_types spillType = lcl->GetRegisterType(lclVar);
            SpillLclVarReg(lclNum, spillType, lclVar, lclVar->GetRegNum());
        }
        else
        {
            SpillNodeReg(lclVar, lclVar->GetType(), 0);

            return;
        }
    }

    if (lclVar->OperIs(GT_STORE_LCL_VAR))
    {
        genUpdateLife(lclVar);
    }

    if ((lclVar->GetRegNum() != REG_NA) && (!lcl->IsRegCandidate() || !lclVar->IsLastUse(0)))
    {
        gcInfo.gcMarkRegPtrVal(lclVar->GetRegNum(), lclVar->GetType());
    }
}

void CodeGen::SpillLclVarReg(unsigned lclNum, var_types type, GenTreeLclVar* lclVar, regNumber reg)
{
    assert(lclVar->OperIs(GT_STORE_LCL_VAR, GT_LCL_VAR));

    LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);
    assert(!lcl->lvNormalizeOnStore() || (type == lcl->GetActualRegisterType()));

    // We have a register candidate local that is marked with SPILL.
    // This flag generally means that we need to spill this local.
    // The exception is the case of a use of an EH/spill-at-single-def var use that is being "spilled"
    // to the stack, indicated by SPILL (note that all EH lclVar defs are always
    // spilled, i.e. write-thru. Likewise, single-def vars that are spilled at its definitions).
    // An EH or single-def var use is always valid on the stack (so we don't need to actually spill it),
    // but the SPILL flag records the fact that the register value is going dead.
    if (lclVar->OperIs(GT_LCL_VAR) && lcl->IsAlwaysAliveInMemory())
    {
        return;
    }

    GetEmitter()->emitIns_S_R(ins_Store(type, IsSimdLocalAligned(lclNum)), emitTypeSize(type), reg, lclNum, 0);
}

#if FEATURE_ARG_SPLIT
void CodeGen::DefPutArgSplitRegs(GenTreePutArgSplit* arg)
{
    assert((arg->gtDebugFlags & GTF_DEBUG_NODE_CG_PRODUCED) == 0);
    INDEBUG(arg->gtDebugFlags |= GTF_DEBUG_NODE_CG_PRODUCED;)

    if (arg->IsAnyRegSpill())
    {
        for (unsigned i = 0; i < arg->GetRegCount(); ++i)
        {
            if (arg->IsRegSpill(i))
            {
                SpillNodeReg(arg, arg->GetRegType(i), i);
            }
        }
    }
    else
    {
        // TODO-MIKE-Review: It looks like they forgot about "other regs" and also
        // passed the wrong type. It probably doesn't matter as arg registers get
        // killed anyway but still...
        // The spill check is also dubious, it should probably done for each reg,
        // it's not an all or nothing case. But then it's unlikely that these regs
        // ever need spilling.
        gcInfo.gcMarkRegPtrVal(arg->GetRegNum(), arg->GetType());
    }
}
#endif // FEATURE_ARG_SPLIT

void CodeGen::DefCallRegs(GenTreeCall* call)
{
    assert((call->gtDebugFlags & GTF_DEBUG_NODE_CG_PRODUCED) == 0);
    INDEBUG(call->gtDebugFlags |= GTF_DEBUG_NODE_CG_PRODUCED;)

    if (call->IsAnyRegSpill())
    {
        // TODO-MIKE-Cleanup: There should not be any multireg distinction here, it's
        // just that non-struct calls usually don't have reg count/types set. That can
        // probably be easily done in call lowering or LSRA build.
        if (call->IsMultiRegCall())
        {
            for (unsigned i = 0; i < call->GetRegCount(); ++i)
            {
                if (call->IsRegSpill(i))
                {
                    SpillNodeReg(call, call->GetRegType(i), i);
                }
            }
        }
        else
        {
            var_types regType = call->GetType();

            if (regType == TYP_STRUCT)
            {
                regType = call->GetRegType(0);
            }

            SpillNodeReg(call, regType, 0);
        }
    }
    else
    {
        if (call->IsMultiRegCall())
        {
            for (unsigned i = 0; i < call->GetRegCount(); ++i)
            {
                gcInfo.gcMarkRegPtrVal(call->GetRegNum(i), call->GetRegType(i));
            }
        }
        else
        {
            gcInfo.gcMarkRegPtrVal(call->GetRegNum(), call->GetType());
        }
    }
}

#ifndef TARGET_64BIT
void CodeGen::DefLongRegs(GenTree* node)
{
    assert(node->IsMultiRegOpLong());
    assert((node->gtDebugFlags & GTF_DEBUG_NODE_CG_PRODUCED) == 0);
    INDEBUG(node->gtDebugFlags |= GTF_DEBUG_NODE_CG_PRODUCED;)

    if (node->IsRegSpill(0))
    {
        SpillNodeReg(node, TYP_INT, 0);
    }

    if (node->IsRegSpill(1))
    {
        SpillNodeReg(node, TYP_INT, 1);
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

    // TODO-MIKE-Review: Shouldn't this simply be EA_PTRSIZE?
    emitAttr attr = emitTypeSize(node->GetType());

    GetEmitter()->emitIns_R_S(INS_lea, attr, node->GetRegNum(), node->GetLclNum(), node->GetLclOffs());
    DefReg(node);
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

// Returns true if the TYP_SIMD locals on stack are aligned at their
// preferred byte boundary specified by lvaGetSimdTypedLocalPreferredAlignment().
//
// As per the Intel manual, the preferred alignment for AVX vectors is
// 32-bytes. It is not clear whether additional stack space used in
// aligning stack is worth the benefit and for now will use 16-byte
// alignment for AVX 256-bit vectors with unaligned load/stores to/from
// memory. On x86, the stack frame is aligned to 4 bytes. We need to extend
// existing support for double (8-byte) alignment to 16 or 32 byte
// alignment for frames with local SIMD vars, if that is determined to be
// profitable.
//
// On Amd64 and SysV, RSP+8 is aligned on entry to the function (before
// prolog has run). This means that in RBP-based frames RBP will be 16-byte
// aligned. For RSP-based frames these are only sometimes aligned, depending
// on the frame size.
//
bool CodeGen::IsSimdLocalAligned(unsigned lclNum)
{
#ifndef FEATURE_SIMD
    return false;
#else
    LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

    if (!varTypeIsSIMD(lcl->GetType()))
    {
        return false;
    }

    int alignment = compiler->lvaGetSimdTypedLocalPreferredAlignment(lcl);

    if (alignment > STACK_ALIGN)
    {
        return false;
    }

    bool rbpBased;
    int  off = compiler->lvaFrameAddress(lclNum, &rbpBased);
    // On SysV and Winx64 ABIs RSP+8 will be 16-byte aligned at the
    // first instruction of a function. If our frame is RBP based
    // then RBP will always be 16 bytes aligned, so we can simply
    // check the offset.
    if (rbpBased)
    {
        return (off % alignment) == 0;
    }

    // For RSP-based frame the alignment of RSP depends on our
    // locals. rsp+8 is aligned on entry and we just subtract frame
    // size so it is not hard to compute. Note that the compiler
    // tries hard to make sure the frame size means RSP will be
    // 16-byte aligned, but for leaf functions without locals (i.e.
    // frameSize = 0) it will not be.
    int frameSize = genTotalFrameSize();
    return ((8 - frameSize + off) % alignment) == 0;
#endif
}
