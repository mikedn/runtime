// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "emit.h"
#include "codegen.h"
#include "lsra.h"
#include "unwind.h"

void CodeGen::InitLclBlockLiveInRegs()
{
    BasicBlock* firstBlock = compiler->fgFirstBB;
    VarToRegMap varRegMap  = m_lsra->GetBlockLiveInRegMap(firstBlock);

    if (varRegMap == nullptr)
    {
        assert(compiler->lvaTrackedCount == 0);
        return;
    }

    JITDUMP("Initializing local regs at start of " FMT_BB "\n", firstBlock->bbNum);

    for (VarSetOps::Enumerator en(compiler, firstBlock->bbLiveIn); en.MoveNext();)
    {
        LclVarDsc* lcl = compiler->lvaGetDescByTrackedIndex(en.Current());

        if (lcl->IsRegCandidate())
        {
            regNumber regNum = static_cast<regNumber>(varRegMap[en.Current()]);
            lcl->SetRegNum(regNum);
            JITDUMP("  V%02u (%s)", lcl->GetLclNum(), getRegName(regNum));
        }
    }

    JITDUMP("\n");
}

void CodeGen::genMarkLabelsForCodegen()
{
    assert(!compiler->fgSafeBasicBlockCreation);

    JITDUMP("\nMark label blocks\n");

#ifdef DEBUG
    for (BasicBlock* const block : compiler->Blocks())
    {
        assert((block->bbFlags & BBF_HAS_LABEL) == 0);
    }
#endif

    // The first block is special; it always needs a label. This is to properly set up GC info.
    JITDUMP("  " FMT_BB ": first block\n", compiler->fgFirstBB->bbNum);
    compiler->fgFirstBB->bbFlags |= BBF_HAS_LABEL;

    for (BasicBlock* const block : compiler->Blocks())
    {
        block->emitLabel = nullptr;
#ifdef TARGET_ARM
        block->unwindNopEmitLabel = nullptr;
#endif

        switch (block->bbJumpKind)
        {
            case BBJ_COND:
#if FEATURE_LOOP_ALIGN
                if (block->bbJumpDest->isLoopAlign() && (block->bbNext != nullptr))
                {
                    // In the emitter, we need to calculate the loop size from `block->bbJumpDest` through
                    // `block` (inclusive). Thus, we need to ensure there is a label on the lexical fall-through
                    // block, even if one is not otherwise needed, to be able to calculate the size of this
                    // loop (loop size is calculated by walking the instruction groups; see emitter::GetLoopSize()).

                    JITDUMP("  " FMT_BB ": alignment end-of-loop\n", block->bbNext->bbNum);
                    block->bbNext->bbFlags |= BBF_HAS_LABEL;
                }
                FALLTHROUGH;
#endif
            case BBJ_ALWAYS:
            case BBJ_EHCATCHRET:
                JITDUMP("  " FMT_BB ": branch target\n", block->bbJumpDest->bbNum);
                block->bbJumpDest->bbFlags |= BBF_HAS_LABEL;
                break;

            case BBJ_SWITCH:
                for (BasicBlock* const bTarget : block->SwitchTargets())
                {
                    JITDUMP("  " FMT_BB ": switch case\n", bTarget->bbNum);
                    bTarget->bbFlags |= BBF_HAS_LABEL;
                }

                // The current implementation of switch tables requires the first block
                // to have a label so it can generate offsets to the switch label targets.
                // TODO-CQ: remove this when switches have been re-implemented to not use this.
                JITDUMP("  " FMT_BB ": switch table base offset\n", compiler->fgFirstBB->bbNum);
                compiler->fgFirstBB->bbFlags |= BBF_HAS_LABEL;
                break;

            case BBJ_CALLFINALLY:
#if FEATURE_EH_CALLFINALLY_THUNKS
                // For callfinally thunks, we need to mark the block following the callfinally/always pair,
                // as that's needed for identifying the range of the "duplicate finally" region in EH data.
                BasicBlock* bbToLabel;
                bbToLabel = block->bbNext;

                if (block->IsCallFinallyAlwaysPairHead())
                {
                    bbToLabel = bbToLabel->bbNext;
                }

                if (bbToLabel != nullptr)
                {
                    JITDUMP("  " FMT_BB ": callfinally thunk region end\n", bbToLabel->bbNum);
                    bbToLabel->bbFlags |= BBF_HAS_LABEL;
                }
#endif // FEATURE_EH_CALLFINALLY_THUNKS
                // The finally target itself will get marked by walking the EH table, below, and marking
                // all handler begins.
                break;

            case BBJ_EHFINALLYRET:
            case BBJ_EHFILTERRET:
            case BBJ_RETURN:
            case BBJ_THROW:
            case BBJ_NONE:
                break;

            default:
                unreached();
        }

        if (BasicBlock* prevBlock = block->bbPrev)
        {
            if (!prevBlock->bbFallsThrough())
            {
                // TODO-MIKE-Cleanup: Some dead blocks aren't removed. If they don't have a label we
                // may end up with an insGroup without GC information and crash due to null gcLcls.
                // Ideally such blocks should be removed but for now just avoid crashing.

                JITDUMP("  " FMT_BB ": potentially unreachable block\n", block->bbNum);
                block->bbFlags |= BBF_HAS_LABEL;
            }
            else if ((prevBlock->GetKind() == BBJ_COND) && (prevBlock->bbWeight != block->bbWeight))
            {
                // TODO-MIKE-Review: What's this for? Just to show the different weight in disassembly?!?

                JITDUMP("  " FMT_BB ": weight difference " FMT_WT " -> " FMT_WT "\n", block->bbNum, prevBlock->bbWeight,
                        block->bbWeight);
                block->bbFlags |= BBF_HAS_LABEL;
            }
        }
    }

    // TODO-MIKE-Review: Wouldn't the first cold block be a jump target anyway? How else could it be reached?
    // Anyway, hot/cold splitting is not enabled so this is basically dead code.
    if (BasicBlock* firstColdBlock = compiler->fgFirstColdBlock)
    {
        noway_assert(!firstColdBlock->bbPrev->bbFallsThrough());

        JITDUMP("  " FMT_BB ": first cold block\n", firstColdBlock->bbNum);
        firstColdBlock->bbFlags |= BBF_HAS_LABEL;
    }

    // Walk all the throw helper blocks and mark them, since jumps to them don't appear the flow graph.
    for (ThrowHelperBlock* helper = compiler->m_throwHelperBlockList; helper != nullptr; helper = helper->next)
    {
        JITDUMP("  " FMT_BB ": throw helper block\n", helper->block->bbNum);
        helper->block->bbFlags |= BBF_HAS_LABEL;
    }

    for (EHblkDsc* const HBtab : EHClauses(compiler))
    {
        JITDUMP("  " FMT_BB ": try begin\n", HBtab->ebdTryBeg->bbNum);
        HBtab->ebdTryBeg->bbFlags |= BBF_HAS_LABEL;

        JITDUMP("  " FMT_BB ": handler begin\n", HBtab->ebdHndBeg->bbNum);
        HBtab->ebdHndBeg->bbFlags |= BBF_HAS_LABEL;

        if (BasicBlock* tryEnd = HBtab->ebdTryLast->bbNext)
        {
            tryEnd->bbFlags |= BBF_HAS_LABEL;
            JITDUMP("  " FMT_BB ": try end\n", tryEnd->bbNum);
        }

        if (BasicBlock* handlerEnd = HBtab->ebdHndLast->bbNext)
        {
            handlerEnd->bbFlags |= BBF_HAS_LABEL;
            JITDUMP("  " FMT_BB ": handler end\n", handlerEnd->bbNum);
        }

        if (HBtab->HasFilter())
        {
            HBtab->ebdFilter->bbFlags |= BBF_HAS_LABEL;
            JITDUMP("  " FMT_BB ": filter begin\n", HBtab->ebdFilter->bbNum);
        }
    }

    unsigned funcletIndex   = 0;
    bool     isInColdRegion = false;

    for (BasicBlock* block : compiler->Blocks())
    {
        if ((block->bbFlags & BBF_HAS_LABEL) != 0)
        {
#ifdef FEATURE_EH_FUNCLETS
            if ((block->bbFlags & BBF_FUNCLET_BEG) != 0)
            {
                funcletIndex = funGetFuncIdx(block);
            }
#endif

            if (block == compiler->fgFirstColdBlock)
            {
                isInColdRegion = true;
            }

            block->emitLabel = GetEmitter()->CreateBlockLabel(block, funcletIndex);

            if (isInColdRegion)
            {
                block->emitLabel->igFlags |= IGF_COLD;
            }
        }
        else
        {
            assert(block != compiler->fgFirstColdBlock);
        }
    }
}

void CodeGen::genCodeForBBlist()
{
    JITDUMP("\nGenerating code\n");

#ifdef DEBUG
    if (compiler->opts.disAsm)
    {
        DumpDisasmHeader();
    }

    if (compiler->opts.disAsm || compiler->verbose)
    {
        compiler->lvaTableDump();
        JITDUMP("\n");
    }
#endif

    assert((compiler->fgFirstBBScratch == nullptr) || (compiler->fgFirstBB == compiler->fgFirstBBScratch));
    assert(genPendingCallLabel == nullptr);
#if !FEATURE_FIXED_OUT_ARGS
    assert(genStackLevel == 0);
#endif

    genMarkLabelsForCodegen();
    GetEmitter()->Begin();
    liveness.Begin();

    unsigned nextEnterScope = 0;
    unsigned nextExitScope  = 0;

    Placeholder* firstPlaceholder = nullptr;
    Placeholder* lastPlaceholder  = nullptr;

    for (BasicBlock* block = compiler->fgFirstBB; block != nullptr; block = block->bbNext)
    {
        JITDUMP("\n=============== Generating ");
        DBEXEC(compiler->verbose, block->dspBlockHeader(compiler, true, true));

        assert(LIR::AsRange(block).CheckLIR(compiler));

        liveness.BeginBlockCodeGen(this, block, m_lsra->GetBlockLiveInRegMap(block));

#ifdef TARGET_ARM
        genInsertNopForUnwinder(block);
#endif

#ifdef FEATURE_EH_FUNCLETS
        genUpdateCurrentFunclet(block);
#endif

        m_currentBlock = block;

        if (insGroup* label = block->emitLabel)
        {
            GetEmitter()->DefineBlockLabel(label);
            GetEmitter()->SetLabelGCLiveness(label);
        }
#if FEATURE_LOOP_ALIGN
        else
        {
            assert(!GetEmitter()->EndsWithAlignInstr());
        }
#endif

#ifdef FEATURE_EH_FUNCLETS
        assert(currentFuncletIndex == GetEmitter()->GetCurrentInsGroup()->GetFuncletIndex());
#endif

#if !FEATURE_FIXED_OUT_ARGS
        assert(genStackLevel == 0);

        if (block->IsThrowHelperBlock())
        {
            SetThrowHelperBlockStackLevel(block);
        }

        const unsigned savedStkLvl = genStackLevel;
#endif

        if (compiler->opts.compDbgInfo)
        {
            if ((compiler->info.compVarScopesCount != 0) && compiler->opts.OptimizationDisabled())
            {
                liveness.StartUntrackedVarsRanges(this, block, &nextEnterScope, &nextExitScope);
            }

            // BBF_INTERNAL blocks don't correspond to any single IL instruction.
            // If the block is the distinguished first scratch block, then there's
            // no need to emit a NO_MAPPING entry, immediately after the prolog.
            if (((block->bbFlags & BBF_INTERNAL) != 0) && !compiler->fgBBisScratch(block))
            {
                genIPmappingAdd(ICorDebugInfo::NO_MAPPING, true);
            }
        }

#ifdef FEATURE_EH_FUNCLETS
        if ((block->bbFlags & BBF_FUNCLET_BEG) != 0)
        {
            insGroup*    prolog = GetEmitter()->ReserveFuncletProlog(block);
            Placeholder* ph     = new (compiler, CMK_Codegen) Placeholder(prolog);

            if (lastPlaceholder == nullptr)
            {
                firstPlaceholder = ph;
            }
            else
            {
                lastPlaceholder->next = ph;
            }

            lastPlaceholder = ph;
        }
#endif

        // TODO-MIKE-Cleanup: We have to do this here rather than in DefineBlockLabel
        // partly because ReserveFuncletProlog is stealing the insGroup created by
        // DefineBlockLabel and partly due to temp labels, which aren't real basic
        // blocks (and DO NOT kill spill temps).
        GetEmitter()->GetCurrentInsGroup()->igFlags |= IGF_BASIC_BLOCK;

        // Emit poisoning into scratch BB that comes right after prolog.
        // We cannot emit this code in the prolog as it might make the prolog too large.
        if (compiler->compShouldPoisonFrame() && compiler->fgBBisScratch(block))
        {
            genPoisonFrame(liveness.GetLiveLclRegs());
        }

        INDEBUG(AssignUseOrder(block));

        IL_OFFSETX currentILOffset = BAD_IL_OFFSET;
        bool       firstMapping    = true;

        for (GenTree* node : LIR::AsRange(block))
        {
            DBEXEC(compiler->verbose, compiler->gtDispLIRNode(node));

            // Validate that all the operands for the current node are used in order.
            // This is important because LSRA ensures that any necessary copies will be
            // handled correctly.
            INDEBUG(lastConsumedNode = nullptr);

            if (GenTreeILOffset* ilOffset = node->IsILOffset())
            {
                if (compiler->opts.compDbgInfo)
                {
                    if (compiler->opts.compDbgCode)
                    {
                        genEnsureCodeEmitted(currentILOffset);
                    }

                    currentILOffset = ilOffset->gtStmtILoffsx;
                    genIPmappingAdd(currentILOffset, firstMapping);
                    firstMapping = false;
                }
            }
            else if (node->IsReuseRegVal())
            {
                JITDUMP("Node is marked ReuseReg\n");
                assert(node->OperIs(GT_CNS_INT, GT_CNS_DBL) || node->IsHWIntrinsicZero());
            }
            else if (node->isContained())
            {
                JITDUMP("Node is contained\n")
            }
            else
            {
                GenNode(node, block);

                if (node->gtHasReg() && node->IsUnusedValue())
                {
                    UseRegs(node);
                }
            }

            JITDUMP("--------------------------------------------------------------------------------\n");
        }

        // Nodes do not have uses accross blocks so no spill temps should be live at the end of a block.
        assert(spillTemps.GetDefCount() == 0);

        INDEBUG(liveness.VerifyLiveGCRegs(block));

#ifdef DEBUG
        if (block->bbNext == nullptr)
        {
#if defined(TARGET_AMD64) && defined(LATE_DISASM)
            // Unit testing of the emitter: generate a bunch of instructions into the last block
            // (it's as good as any, but better than the prologue, which can only be a single instruction
            // group) then use COMPlus_JitLateDisasm=* to see if the late disassembler
            // thinks the instructions are the same as we do.
            genAmd64EmitterUnitTests();
#elif defined(TARGET_ARM64)
            genArm64EmitterUnitTests();
#endif
        }
#endif // DEBUG

        if (compiler->opts.compDbgInfo)
        {
            if (compiler->opts.compDbgCode)
            {
                genEnsureCodeEmitted(currentILOffset);
            }

            bool isLastBlockProcessed = (block->bbNext == nullptr) ||
                                        (block->IsCallFinallyAlwaysPairHead() && (block->bbNext->bbNext == nullptr));

            if (isLastBlockProcessed)
            {
                liveness.EndCodeGen(this);
            }
        }

#if !FEATURE_FIXED_OUT_ARGS
        SubtractStackLevel(savedStkLvl);
        noway_assert(genStackLevel == 0);
#endif

        INDEBUG(liveness.VerifyLiveRegVars(block));
        bool hasEpilog = false;

        switch (block->GetKind())
        {
            case BBJ_RETURN:
                genExitCode(block);
                hasEpilog = true;
                break;

#ifdef FEATURE_EH_FUNCLETS
            case BBJ_EHCATCHRET:
                genEHCatchRet(block);
                FALLTHROUGH;
            case BBJ_EHFINALLYRET:
            case BBJ_EHFILTERRET:
                hasEpilog = true;
                break;
#else
            case BBJ_EHFINALLYRET:
            case BBJ_EHFILTERRET:
                genEHFinallyOrFilterRet(block);
                break;
#endif

            case BBJ_NONE:
#ifdef TARGET_AMD64
                // On AMD64, we need to generate a NOP after a call that is the last instruction of the block,
                // in several situations, to support proper exception handling semantics. This is mostly to
                // ensure that when the stack walker computes an instruction pointer for a frame, that the
                // instruction pointer is in the correct EH region.
                //
                // 1. If the call instruction is in a different EH region as the instruction that follows it.
                // 2. If the call immediately precedes an OS epilog. Note that what the JIT or VM consider
                //    an epilog might be slightly different from what the OS considers an epilog, and it is
                //    the OS-reported epilog that matters here.
                //
                // We handle case #1 here, and case #2 when handling return blocks.

                // Note: we may be generating a few too many NOPs for the case of call preceding an epilog.
                // Technically, if the next block is a BBJ_RETURN, an epilog will be generated, but there
                // may be some instructions generated before the OS epilog starts, such as a GS cookie check.
                // We only need the NOP if we're not going to generate any more code as part of the block end.

                if (GetEmitter()->IsLastInsCall())
                {
                    if (block->bbNext == nullptr)
                    {
                        // Call immediately before the end of the code; we should never get here.
                        GetEmitter()->emitIns(INS_BREAKPOINT);
                    }
                    else if (!BasicBlock::sameEHRegion(block, block->bbNext))
                    {
                        // We need the NOP for EH.
                        GetEmitter()->emitIns(INS_nop);
                    }
                }
#endif // TARGET_AMD64
                break;

            case BBJ_THROW:
                // If we have a throw at the end of a function or funclet, we need to emit another instruction
                // afterwards to help the OS unwinder determine the correct context during unwind. We insert an
                // unexecuted breakpoint instruction in several situations following a throw instruction:
                // 1. If the throw is the last instruction of the function or funclet. This helps
                //    the OS unwinder determine the correct context during an unwind from the
                //    thrown exception.
                // 2. If this is this is the last block of the hot section.
                // 3. If the subsequent block is a special throw block.
                // 4. On AMD64, if the next block is in a different EH region.

                if ((block->bbNext == nullptr) || (block->bbNext->bbFlags & BBF_FUNCLET_BEG) ||
                    !BasicBlock::sameEHRegion(block, block->bbNext) ||
                    (!isFramePointerUsed() && block->bbNext->IsThrowHelperBlock()) ||
                    block->bbNext == compiler->fgFirstColdBlock)
                {
                    GetEmitter()->emitIns(INS_BREAKPOINT); // This should never get executed
                }
                // Do likewise for blocks that end in DOES_NOT_RETURN calls
                // that were not caught by the above rules. This ensures that
                // gc register liveness doesn't change across call instructions
                // in fully-interruptible mode.
                else
                {
                    GenTree* call = block->lastNode();

                    if ((call != nullptr) && call->IsCall() && call->AsCall()->IsNoReturn())
                    {
                        GetEmitter()->emitIns(INS_BREAKPOINT); // This should never get executed
                    }
                }
                break;

            case BBJ_CALLFINALLY:
                GenCallFinally(block);

#ifdef TARGET_ARM
                assert((block->bbFlags & BBF_RETLESS_CALL) == 0);
#else
                if ((block->bbFlags & BBF_RETLESS_CALL) == 0)
#endif
                {
                    // The BBJ_ALWAYS is used because the BBJ_CALLFINALLY can't point to the
                    // jump target using bbJumpDest - that is already used to point
                    // to the finally block. So just skip past the BBJ_ALWAYS unless the
                    // block is RETLESS.
                    assert(block->IsCallFinallyAlwaysPairHead());
                    block = block->bbNext;

                    JITDUMP("\n=============== Skipping finally return ");
                    DBEXEC(compiler->verbose, block->dspBlockHeader(compiler, true, true));
                }
                break;

            case BBJ_SWITCH:
#ifdef TARGET_AMD64
                assert(!GetEmitter()->IsLastInsCall());
#endif
                break;

            case BBJ_ALWAYS:
                assert(!block->IsThrowHelperBlock());

                if (block->bbJumpDest == block->bbNext)
                {
#ifdef TARGET_AMD64
                    // We need to have another instruction after a call if a different EH region follows,
                    // but we can't properly check the EH region in this case because the next block may
                    // be in the same EH region and also be a "jump to next" block which goes into another
                    // EH region, or an empty block that falls through to another EH region, or perhaps a
                    // block that isn't empty but becomes empty due to redundant mov elimination in the
                    // emitter etc. Such cases are rare but they do happen, at least in minopts, where the
                    // front end doesn't optimize the flow graph. And such failures can be rather subtle
                    // and not easily caught by tests. So just insert a nop anytime EH is present in the
                    // method. Anyway it's better than the old code, which kept a useless "jump to next".
                    if (GetEmitter()->IsLastInsCall() && compiler->fgHasEH())
                    {
                        GetEmitter()->emitIns(INS_nop);
                    }
#endif

                    break;
                }

#ifdef TARGET_ARMARCH
                GetEmitter()->emitIns_J(INS_b, block->bbJumpDest->emitLabel);
#else
                GetEmitter()->emitIns_J(INS_jmp, block->bbJumpDest->emitLabel);
#endif
                FALLTHROUGH;
            case BBJ_COND:
#ifdef TARGET_AMD64
                assert(!GetEmitter()->IsLastInsCall());
#endif
#if FEATURE_LOOP_ALIGN
                // This is the last place where we operate on blocks and after this, we operate
                // on IG. Hence, if we know that the destination of "block" is the first block
                // of a loop and needs alignment (it has BBF_LOOP_ALIGN), then "block" represents
                // end of the loop. Propagate that information on the IG through "igLoopBackEdge".
                //
                // During emitter, this information will be used to calculate the loop size.
                // Depending on the loop size, decision of whether to align a loop or not will be taken.

                if (block->bbJumpDest->isLoopAlign() && block->bbJumpDest->emitLabel->IsDefined())
                {
                    GetEmitter()->SetLoopBackEdge(block->bbJumpDest->emitLabel);
                }
#endif // FEATURE_LOOP_ALIGN
                break;

            default:
                unreached();
        }

        if (hasEpilog)
        {
            insGroup*    epilog = GetEmitter()->ReserveEpilog(block);
            Placeholder* ph     = new (compiler, CMK_Codegen) Placeholder(epilog);

            if (lastPlaceholder == nullptr)
            {
                firstPlaceholder = ph;
            }
            else
            {
                lastPlaceholder->next = ph;
            }

            lastPlaceholder = ph;
        }

        if (BasicBlock* next = block->bbNext)
        {
#if FEATURE_LOOP_ALIGN
            // If next block is the first block of a loop (identified by BBF_LOOP_ALIGN),
            // then need to add align instruction in current "block". Also mark the
            // corresponding IG with IGF_LOOP_ALIGN to know that there will be align
            // instructions at the end of that IG.
            //
            // For non-adaptive alignment, add alignment instruction of size depending on the
            // compJitAlignLoopBoundary.
            // For adaptive alignment, alignment instruction will always be of 15 bytes.

            if (next->isLoopAlign())
            {
                GetEmitter()->AlignLoop();
            }
#endif

            if (hasEpilog)
            {
                // TODO-MIKE-Review: This seems dodgy. If another block follows then it's
                // supposed to have a label (unless it is unreachable, in which case we'll
                // just require it to have a label too) and the label will be defined when
                // we visit that block. But this code is kind of messed up and does that
                // after calling liveness.BeginBlockCodeGen, which needs the current IG to
                // generate debug info, so currentIG needs to be valid.
                // Note that doing this here can result in the label having the wrong funclet
                // index, but we'll call this again when the block is visited, which fixes it.
                GetEmitter()->DefineBlockLabel(next->emitLabel);
            }
        }
    }

    m_currentBlock = nullptr;
    liveness.End(this);

    genFnProlog();

#ifdef FEATURE_EH_FUNCLETS
    if (compFuncInfoCount > 1)
    {
        genCaptureFuncletPrologEpilogInfo();
    }
#endif

    GeneratePrologEpilog(firstPlaceholder);
    GetEmitter()->ShortenBranches();
#if FEATURE_LOOP_ALIGN
    GetEmitter()->LoopAlignAdjustments();
#endif

    JITDUMP("\nHot code size = 0x%X bytes\nCold code size = 0x%X bytes\n", GetEmitter()->GetHotCodeSize(),
            GetEmitter()->GetColdCodeSize());
}

void CodeGen::GeneratePrologEpilog(Placeholder* firstPlaceholder)
{
#ifdef DEBUG
    unsigned epilogCount = 0;
#ifdef FEATURE_EH_FUNCLETS
    unsigned funcletPrologCount = 0;
    unsigned funcletEpilogCount = 0;
#endif
#endif

    Emitter& emit = *GetEmitter();

    for (Placeholder* ph = firstPlaceholder; ph != nullptr; ph = ph->next)
    {
        insGroup* ig = ph->ig;

        liveness.BeginPrologEpilogCodeGen();

#ifdef JIT32_GCENCODER
        assert(ig->IsMainEpilog());
        emit.BeginGCEpilog();
#endif

        BasicBlock* block = emit.BeginPrologEpilog(ig);

#ifdef FEATURE_EH_FUNCLETS
        if (ig->IsFuncletProlog())
        {
            INDEBUG(++funcletPrologCount);
            genFuncletProlog(block);
        }
        else if (ig->IsFuncletEpilog())
        {
            INDEBUG(++funcletEpilogCount);
            genFuncletEpilog();
        }
        else
#endif
        {
            assert(ig->IsMainEpilog());
            INDEBUG(++epilogCount);
            genFnEpilog(block);
        }

        emit.EndPrologEpilog();
#ifdef JIT32_GCENCODER
        emit.EndGCEpilog(ig);
#endif
    }

    emit.RecomputeIGOffsets();

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("\n1 prolog, %u epilog(s)", epilogCount);
#ifdef FEATURE_EH_FUNCLETS
        printf(", %u funclet prolog(s), %u funclet epilog(s)", funcletPrologCount, funcletEpilogCount);
#endif
        printf("\n");
        emit.PrintIGList(false);
    }
#endif

#ifdef FEATURE_EH_FUNCLETS
    assert(funcletPrologCount == compiler->ehFuncletCount());
#endif
}

void CodeGen::genExitCode(BasicBlock* block)
{
    if (compiler->opts.compDbgInfo)
    {
        genIPmappingAdd(ICorDebugInfo::EPILOG, true);
    }

    if (compiler->getNeedsGSSecurityCookie())
    {
#ifdef TARGET_XARCH
        EpilogGSCookieCheck((block->bbFlags & BBF_HAS_JMP) != 0);
#else
        EpilogGSCookieCheck();
#endif
    }
}

#ifdef FEATURE_EH_FUNCLETS

void CodeGen::genUpdateCurrentFunclet(BasicBlock* block)
{
    if ((block->bbFlags & BBF_FUNCLET_BEG) != 0)
    {
        const FuncInfoDsc& func = funSetCurrentFunc(block->emitLabel->GetFuncletIndex());

        if (func.kind == FUNC_FILTER)
        {
            assert(compiler->ehGetDsc(func.ehIndex)->ebdFilter == block);
        }
        else
        {
            assert(func.kind == FUNC_HANDLER);
            assert(compiler->ehGetDsc(func.ehIndex)->ebdHndBeg == block);
        }
    }
    else
    {
        const FuncInfoDsc& func = funCurrentFunc();

        if (func.kind == FUNC_FILTER)
        {
            assert(compiler->ehGetDsc(func.ehIndex)->InFilterRegionBBRange(block));
        }
        else if (func.kind == FUNC_HANDLER)
        {
            assert(compiler->ehGetDsc(func.ehIndex)->InHndRegionBBRange(block));
        }
        else
        {
            assert(func.kind == FUNC_ROOT);
            assert(!block->hasHndIndex());
        }
    }
}

#endif // FEATURE_EH_FUNCLETS

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                         Register Management                               XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

bool CodeGen::SpillRegCandidateLclVar(GenTreeLclVar* lclVar)
{
    LclVarDsc* lcl = lclVar->GetLcl();

    assert(lcl->IsRegCandidate());
    assert(lclVar->OperIs(GT_LCL_LOAD, GT_LCL_STORE));
    assert(lclVar->IsRegSpill(0));

    // We don't actually need to spill if it is already living in memory
    bool needsSpill = lclVar->OperIs(GT_LCL_LOAD) && (lcl->GetRegNum() != REG_STK);

    if (needsSpill)
    {
        // We should only have both SPILL (i.e. the flag causing this method to be called) and
        // SPILLED on a write-thru/single-def def, for which we should not be calling this method.
        assert(!lclVar->IsRegSpilled(0));

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
            instruction ins  = ins_Store(type, IsSimdLocalAligned(lcl));

            GetEmitter()->emitIns_S_R(ins, emitTypeSize(type), lclVar->GetRegNum(), GetStackAddrMode(lcl, 0));
        }

        liveness.Spill(lcl, lclVar);
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
        assert(lcl->IsAlwaysAliveInMemory() && lclVar->OperIs(GT_LCL_STORE));
    }

    return needsSpill;
}

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

    if (node->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD))
    {
        liveness.UpdateLife(this, node->AsLclVarCommon());
    }

    liveness.RemoveGCRegs(genRegMask(node->GetRegNum()));

    INDEBUG(VerifyUseOrder(node));

    return node->GetRegNum();
}

regNumber CodeGen::UseRegCandidateLclVar(GenTreeLclVar* node)
{
    assert(IsRegCandidateLclVar(node));

    LclVarDsc* lcl = node->GetLcl();

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

    liveness.UpdateLife(this, node);

    assert(node->GetRegNum() != REG_NA);

    if (lcl->GetRegNum() == REG_STK)
    {
        // We have loaded this into a register only temporarily
        liveness.RemoveGCRegs(genRegMask(node->GetRegNum()));
    }
    else if (node->IsLastUse(0))
    {
        liveness.RemoveGCRegs(genRegMask(lcl->GetRegNum()));
    }

    INDEBUG(VerifyUseOrder(node));

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

    // If it is a last use, the local will be killed by UseReg, as usual, and DefReg will
    // appropriately set the GC liveness for the copied value.
    // If not, there are two cases we need to handle:
    // - If this is a TEMPORARY copy (indicated by the GTF_VAR_DEATH flag) the variable
    //   will remain live in its original register.
    //   DefReg will appropriately set the GC liveness for the copied value,
    //   and UseReg will reset it.
    // - Otherwise, we need to update register info for the lclVar.

    if (src->OperIs(GT_LCL_LOAD) && !src->IsLastUse(0) && !copy->IsLastUse(0))
    {
        LclVarDsc* lcl = src->AsLclLoad()->GetLcl();

        if (lcl->GetRegNum() != REG_STK)
        {
            liveness.MoveReg(this, lcl, src->AsLclLoad(), copy);
            return;
        }
    }

    liveness.SetGCRegType(dstReg, dstType);
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

    LclVarDsc* lcl     = node->GetLcl();
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

    instruction ins = ins_Load(regType, IsSimdLocalAligned(lcl));
    GetEmitter()->emitIns_R_S(ins, emitTypeSize(regType), dstReg, GetStackAddrMode(lcl, 0));

    liveness.Unspill(this, lcl, node, dstReg, regType);
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
    liveness.RemoveGCRegs(node->gtGetRegMask());

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

    liveness.SetGCRegType(dstReg, type);

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

    liveness.RemoveGCRegs(node->gtGetRegMask());

    INDEBUG(VerifyUseOrder(node));
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

#ifdef DEBUG
bool CodeGen::IsValidContainedLcl(GenTreeLclVarCommon* node)
{
    // A contained local must be living on stack and marked as reg optional,
    // or not be a register candidate.
    // TODO-MIKE-Review: If it's reg optional it probably needs to be spilled too...
    LclVarDsc* lcl = node->GetLcl();

    return (lcl->GetRegNum() == REG_STK) && (node->IsRegOptional() || !lcl->IsRegCandidate());
}
#endif

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

    if (tree->IsIndir())
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

    if (tree->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD))
    {
        assert(IsValidContainedLcl(tree->AsLclVarCommon()));
        liveness.UpdateLife(this, tree->AsLclVarCommon());

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

void CodeGen::ConsumeStructStore(
    GenTree* store, ClassLayout* layout, regNumber dstReg, regNumber srcReg, regNumber sizeReg)
{
    assert(store->OperIs(GT_IND_STORE_OBJ, GT_IND_STORE_BLK, GT_LCL_STORE, GT_LCL_STORE_FLD));

    // We have to consume the registers, and perform any copies, in the actual execution order: dst, src, size.
    //
    // Note that the register allocator ensures that the registers ON THE NODES will not interfere
    // with one another if consumed (i.e. reloaded or moved to their ASSIGNED reg) in execution order.
    // Further, it ensures that they will not interfere with one another if they are then copied
    // to the REQUIRED register (if a fixed register requirement) in execution order.  This requires,
    // then, that we first consume all the operands, then do any necessary moves.

    GenTree* dstAddr = nullptr;
    GenTree* src;

    if (store->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD))
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
    else if (src->OperIs(GT_IND_LOAD, GT_IND_LOAD_OBJ, GT_IND_LOAD_BLK))
    {
        assert(src->isContained());

        src = src->AsIndir()->GetAddr();
    }
    else if (src->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD))
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
        assert(store->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD));

        GetEmitter()->emitIns_R_S(INS_lea, EA_PTRSIZE, dstReg, GetStackAddrMode(store->AsLclVarCommon()));
    }

    if (src->isContained())
    {
        assert(src->OperIs(GT_LCL_LOAD, GT_LCL_LOAD_FLD));

        GetEmitter()->emitIns_R_S(INS_lea, EA_PTRSIZE, srcReg, GetStackAddrMode(src->AsLclVarCommon()));
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

    regNumber  reg  = node->GetRegNum(regIndex);
    SpillTemp* temp = spillTemps.DefSpillTemp(node, regIndex, regType);

    JITDUMP("Spilling register %s after [%06u]\n", getRegName(reg), node->GetID());

    regType          = temp->GetType();
    instruction ins  = ins_Store(regType);
    emitAttr    attr = emitActualTypeSize(regType);

    GetEmitter()->emitIns_S_R(ins, attr, reg, GetStackAddrMode(temp));

    node->SetRegSpill(regIndex, false);
    node->SetRegSpilled(regIndex, true);

    liveness.RemoveGCRegs(genRegMask(reg));
}

#ifdef TARGET_X86
void CodeGen::SpillST0(GenTree* node)
{
    var_types  type = node->GetType();
    SpillTemp* temp = spillTemps.DefSpillTemp(node, 0, type);

    JITDUMP("Spilling register ST0 after [%06u]\n", node->GetID());

    GetEmitter()->emitIns_S(INS_fstp, emitTypeSize(type), GetStackAddrMode(temp));

    node->SetRegSpill(0, false);
    node->SetRegSpilled(0, true);
}
#endif // TARGET_X86

void CodeGen::UnspillNodeReg(GenTree* node, regNumber reg, unsigned regIndex)
{
    assert(!node->IsCopyOrReload());
    assert(!node->IsMultiRegLclVar());

    regNumber  oldReg = node->GetRegNum(regIndex);
    SpillTemp* temp   = spillTemps.UseSpillTemp(node, regIndex);

    node->SetRegSpilled(regIndex, false);

    JITDUMP("Unspilling register %s from [%06u]\n", getRegName(oldReg), node->GetID());

    var_types   regType = temp->GetType();
    instruction ins     = ins_Load(regType);
    emitAttr    attr    = emitActualTypeSize(regType);

    GetEmitter()->emitIns_R_S(ins, attr, reg, GetStackAddrMode(temp));

    liveness.SetGCRegType(reg, regType);
}

#ifdef TARGET_X86
void CodeGen::UnspillST0(GenTree* node)
{
    regNumber  oldReg = node->GetRegNum();
    SpillTemp* temp   = spillTemps.UseSpillTemp(node, 0);

    node->SetRegSpilled(0, false);

    JITDUMP("Unspilling ST0 from [%06u]\n", getRegName(oldReg), node->GetID());

    var_types regType = temp->GetType();
    GetEmitter()->emitIns_S(INS_fld, emitTypeSize(regType), GetStackAddrMode(temp));
}
#endif // TARGET_X86

void CodeGen::genProduceReg(GenTree* node)
{
    DefReg(node);
}

void CodeGen::DefReg(GenTree* node)
{
    assert(!node->OperIs(GT_LCL_STORE_FLD, GT_LCL_STORE, GT_LCL_LOAD, GT_CALL));
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
        liveness.SetGCRegType(node->GetRegNum(), node->GetType());
    }
}

void CodeGen::DefLclVarReg(GenTreeLclVar* lclVar)
{
    assert(lclVar->OperIs(GT_LCL_LOAD, GT_LCL_STORE) && !lclVar->IsMultiReg());
    assert((lclVar->gtDebugFlags & GTF_DEBUG_NODE_CG_PRODUCED) == 0);
    INDEBUG(lclVar->gtDebugFlags |= GTF_DEBUG_NODE_CG_PRODUCED;)

    LclVarDsc* lcl = lclVar->GetLcl();

    assert(!lcl->IsIndependentPromoted());

    if (lclVar->IsAnyRegSpill())
    {
        if (!lcl->IsRegCandidate())
        {
            SpillNodeReg(lclVar, lclVar->GetType(), 0);
            // TODO-MIKE-Review: Shouldn't this call UpdateLife too? Not being
            // a register candidate does not imply lack of liveness.
            return;
        }

        SpillLclVarReg(lcl, lclVar);
    }

    if (lclVar->OperIs(GT_LCL_STORE))
    {
        liveness.UpdateLife(this, lclVar);
    }

    if ((lclVar->GetRegNum() != REG_NA) && (!lcl->IsRegCandidate() || !lclVar->IsLastUse(0)))
    {
        liveness.SetGCRegType(lclVar->GetRegNum(), lclVar->GetType());
    }
}

void CodeGen::SpillLclVarReg(LclVarDsc* lcl, GenTreeLclVar* lclVar)
{
    assert(lclVar->OperIs(GT_LCL_STORE, GT_LCL_LOAD));

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

    var_types type = lcl->GetRegisterType(lclVar);

    assert(!lcl->lvNormalizeOnStore() || (type == lcl->GetActualRegisterType()));

    GetEmitter()->emitIns_S_R(ins_Store(type, IsSimdLocalAligned(lcl)), emitTypeSize(type), lclVar->GetRegNum(),
                              GetStackAddrMode(lcl, 0));
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
        liveness.SetGCRegType(arg->GetRegNum(), arg->GetType());
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
                liveness.SetGCRegType(call->GetRegNum(i), call->GetRegType(i));
            }
        }
        else
        {
            liveness.SetGCRegType(call->GetRegNum(), call->GetType());
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

    liveness.RemoveGCRegs(genRegMask(node->GetRegNum(0)) | genRegMask(node->GetRegNum(1)));
}
#endif // TARGET_64BIT

#ifdef DEBUG
void CodeGen::AssignUseOrder(BasicBlock* block)
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
            AssignUseOrder(operand, useNum);
        }
    }
}

void CodeGen::AssignUseOrder(GenTree* const operand, int& useNum) const
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
            AssignUseOrder(op, useNum);
        }
    }
}

void CodeGen::VerifyUseOrder(GenTree* const node)
{
    assert(node != nullptr);

    if (compiler->verbose)
    {
        if (node->gtUseNum == -1)
        {
            // nothing wrong if the node was not consumed
        }
        else if ((node->gtDebugFlags & GTF_DEBUG_NODE_CG_CONSUMED) != 0)
        {
            printf("Node was consumed twice:\n");
            compiler->gtDispLIRNode(node);
        }
        else if ((lastConsumedNode != nullptr) && (node->gtUseNum < lastConsumedNode->gtUseNum))
        {
            printf("Nodes were consumed out-of-order:\n");
            compiler->gtDispLIRNode(lastConsumedNode);
            compiler->gtDispLIRNode(node);
        }
    }

    assert((node->OperGet() == GT_CATCH_ARG) || ((node->gtDebugFlags & GTF_DEBUG_NODE_CG_CONSUMED) == 0));
    assert((lastConsumedNode == nullptr) || (node->gtUseNum == -1) || (node->gtUseNum > lastConsumedNode->gtUseNum));

    node->gtDebugFlags |= GTF_DEBUG_NODE_CG_CONSUMED;
    lastConsumedNode = node;
}
#endif
