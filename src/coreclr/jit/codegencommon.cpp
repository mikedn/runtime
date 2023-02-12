// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX Code Generator Common:                                                    XX
XX   Methods common to all architectures and register allocation strategies  XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

// TODO-Cleanup: There are additional methods in CodeGen*.cpp that are almost
// identical, and which should probably be moved here.

#include "jitpch.h"
#include "codegen.h"
#include "emit.h"
#include "patchpointinfo.h"
#include "lsra.h"

void Compiler::codeGenInit()
{
    codeGen = new (this, CMK_Codegen) CodeGen(this);
}

CodeGenInterface::CodeGenInterface(Compiler* compiler) : compiler(compiler), spillTemps(compiler)
{
}

CodeGen::CodeGen(Compiler* compiler) : CodeGenInterface(compiler), liveness(compiler)
{
    m_cgEmitter = new (compiler->getAllocator()) emitter(compiler, this, compiler->info.compCompHnd);

#ifdef LATE_DISASM
    getDisAssembler().disInit(compiler);
#endif
}

//------------------------------------------------------------------------
// genMarkLabelsForCodegen: Mark labels required for codegen.
//
// Mark all blocks that require a label with BBF_HAS_LABEL. These are either blocks that are:
// 1. the target of jumps (fall-through flow doesn't require a label),
// 2. referenced labels such as for "switch" codegen,
// 3. needed to denote the range of EH regions to the VM.
// 4. needed to denote the range of code for alignment processing.
//
// No labels will be in the IR before now, but future codegen might annotate additional blocks
// with this flag, such as "switch" codegen, or codegen-created blocks from genCreateTempLabel().
// Also, the alignment processing code marks BBJ_COND fall-through labels elsewhere.
//
// To report exception handling information to the VM, we need the size of the exception
// handling regions. To compute that, we need to emit labels for the beginning block of
// an EH region, and the block that immediately follows a region. Go through the EH
// table and mark all these blocks with BBF_HAS_LABEL to make this happen.
//
// This code is closely couple with genReportEH() in the sense that any block
// that this procedure has determined it needs to have a label has to be selected
// using the same logic both here and in genReportEH(), so basically any time there is
// a change in the way we handle EH reporting, we have to keep the logic of these two
// methods 'in sync'.
//
// No blocks should be added or removed after this.
//
void CodeGen::genMarkLabelsForCodegen()
{
    assert(!compiler->fgSafeBasicBlockCreation);

    JITDUMP("Mark labels for codegen\n");

#ifdef DEBUG
    // No label flags should be set before this.
    for (BasicBlock* const block : compiler->Blocks())
    {
        assert((block->bbFlags & BBF_HAS_LABEL) == 0);
    }
#endif // DEBUG

    // The first block is special; it always needs a label. This is to properly set up GC info.
    JITDUMP("  " FMT_BB " : first block\n", compiler->fgFirstBB->bbNum);
    compiler->fgFirstBB->bbFlags |= BBF_HAS_LABEL;

    // The current implementation of switch tables requires the first block to have a label so it
    // can generate offsets to the switch label targets.
    // (This is duplicative with the fact we always set the first block with a label above.)
    // TODO-CQ: remove this when switches have been re-implemented to not use this.
    if (compiler->fgHasSwitch)
    {
        JITDUMP("  " FMT_BB " : function has switch; mark first block\n", compiler->fgFirstBB->bbNum);
        compiler->fgFirstBB->bbFlags |= BBF_HAS_LABEL;
    }

    for (BasicBlock* const block : compiler->Blocks())
    {
        switch (block->bbJumpKind)
        {
            case BBJ_ALWAYS: // This will also handle the BBJ_ALWAYS of a BBJ_CALLFINALLY/BBJ_ALWAYS pair.
            case BBJ_COND:
            case BBJ_EHCATCHRET:
                JITDUMP("  " FMT_BB " : branch target\n", block->bbJumpDest->bbNum);
                block->bbJumpDest->bbFlags |= BBF_HAS_LABEL;
                break;

            case BBJ_SWITCH:
                for (BasicBlock* const bTarget : block->SwitchTargets())
                {
                    JITDUMP("  " FMT_BB " : branch target\n", bTarget->bbNum);
                    bTarget->bbFlags |= BBF_HAS_LABEL;
                }
                break;

            case BBJ_CALLFINALLY:
                // The finally target itself will get marked by walking the EH table, below, and marking
                // all handler begins.
                CLANG_FORMAT_COMMENT_ANCHOR;

#if FEATURE_EH_CALLFINALLY_THUNKS
                {
                    // For callfinally thunks, we need to mark the block following the callfinally/always pair,
                    // as that's needed for identifying the range of the "duplicate finally" region in EH data.
                    BasicBlock* bbToLabel = block->bbNext;
                    if (block->isBBCallAlwaysPair())
                    {
                        bbToLabel = bbToLabel->bbNext; // skip the BBJ_ALWAYS
                    }
                    if (bbToLabel != nullptr)
                    {
                        JITDUMP("  " FMT_BB " : callfinally thunk region end\n", bbToLabel->bbNum);
                        bbToLabel->bbFlags |= BBF_HAS_LABEL;
                    }
                }
#endif // FEATURE_EH_CALLFINALLY_THUNKS

                break;

            case BBJ_EHFINALLYRET:
            case BBJ_EHFILTERRET:
            case BBJ_RETURN:
            case BBJ_THROW:
            case BBJ_NONE:
                break;

            default:
                noway_assert(!"Unexpected bbJumpKind");
                break;
        }
    }

    // Walk all the throw helper blocks and mark them, since jumps to them don't appear the flow graph.
    for (ThrowHelperBlock* helper = compiler->m_throwHelperBlockList; helper != nullptr; helper = helper->next)
    {
        JITDUMP("  " FMT_BB " : throw helper block\n", helper->block->bbNum);
        helper->block->bbFlags |= BBF_HAS_LABEL;
    }

    for (EHblkDsc* const HBtab : EHClauses(compiler))
    {
        HBtab->ebdTryBeg->bbFlags |= BBF_HAS_LABEL;
        HBtab->ebdHndBeg->bbFlags |= BBF_HAS_LABEL;

        JITDUMP("  " FMT_BB " : try begin\n", HBtab->ebdTryBeg->bbNum);
        JITDUMP("  " FMT_BB " : hnd begin\n", HBtab->ebdHndBeg->bbNum);

        if (HBtab->ebdTryLast->bbNext != nullptr)
        {
            HBtab->ebdTryLast->bbNext->bbFlags |= BBF_HAS_LABEL;
            JITDUMP("  " FMT_BB " : try end\n", HBtab->ebdTryLast->bbNext->bbNum);
        }

        if (HBtab->ebdHndLast->bbNext != nullptr)
        {
            HBtab->ebdHndLast->bbNext->bbFlags |= BBF_HAS_LABEL;
            JITDUMP("  " FMT_BB " : hnd end\n", HBtab->ebdHndLast->bbNext->bbNum);
        }

        if (HBtab->HasFilter())
        {
            HBtab->ebdFilter->bbFlags |= BBF_HAS_LABEL;
            JITDUMP("  " FMT_BB " : filter begin\n", HBtab->ebdFilter->bbNum);
        }
    }

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("*************** After genMarkLabelsForCodegen()\n");
        compiler->fgDispBasicBlocks();
    }
#endif // DEBUG
}

void CodeGen::genUpdateLife(GenTreeLclVarCommon* node)
{
    liveness.UpdateLife(this, node);
}

//----------------------------------------------------------------------
// compHelperCallKillSet: Gets a register mask that represents the kill set for a helper call.
// Not all JIT Helper calls follow the standard ABI on the target architecture.
//
// TODO-CQ: Currently this list is incomplete (not all helpers calls are
//          enumerated) and not 100% accurate (some killsets are bigger than
//          what they really are).
//          There's some work to be done in several places in the JIT to
//          accurately track the registers that are getting killed by
//          helper calls:
//              a) LSRA needs several changes to accomodate more precise killsets
//                 for every helper call it sees (both explicitly [easy] and
//                 implicitly [hard])
//              b) Currently for AMD64, when we generate code for a helper call
//                 we're independently over-pessimizing the killsets of the call
//                 (independently from LSRA) and this needs changes
//                 both in CodeGenAmd64.cpp and emitx86.cpp.
//
//                 The best solution for this problem would be to try to centralize
//                 the killset information in a single place but then make the
//                 corresponding changes so every code generation phase is in sync
//                 about this.
//
//         The interim solution is to only add known helper calls that don't
//         follow the AMD64 ABI and actually trash registers that are supposed to be non-volatile.
//
// Arguments:
//   helper - The helper being inquired about
//
// Return Value:
//   Mask of register kills -- registers whose values are no longer guaranteed to be the same.
//
regMaskTP Compiler::compHelperCallKillSet(CorInfoHelpFunc helper)
{
    switch (helper)
    {
        case CORINFO_HELP_ASSIGN_BYREF:
#if defined(TARGET_AMD64)
            return RBM_RSI | RBM_RDI | RBM_CALLEE_TRASH_NOGC;
#elif defined(TARGET_ARMARCH)
            return RBM_CALLEE_TRASH_WRITEBARRIER_BYREF;
#elif defined(TARGET_X86)
            return RBM_ESI | RBM_EDI | RBM_ECX;
#else
            NYI("Model kill set for CORINFO_HELP_ASSIGN_BYREF on target arch");
            return RBM_CALLEE_TRASH;
#endif

#if defined(TARGET_ARMARCH)
        case CORINFO_HELP_ASSIGN_REF:
        case CORINFO_HELP_CHECKED_ASSIGN_REF:
            return RBM_CALLEE_TRASH_WRITEBARRIER;
#endif

        case CORINFO_HELP_PROF_FCN_ENTER:
#ifdef RBM_PROFILER_ENTER_TRASH
            return RBM_PROFILER_ENTER_TRASH;
#else
            NYI("Model kill set for CORINFO_HELP_PROF_FCN_ENTER on target arch");
#endif

        case CORINFO_HELP_PROF_FCN_LEAVE:
#ifdef RBM_PROFILER_LEAVE_TRASH
            return RBM_PROFILER_LEAVE_TRASH;
#else
            NYI("Model kill set for CORINFO_HELP_PROF_FCN_LEAVE on target arch");
#endif

        case CORINFO_HELP_PROF_FCN_TAILCALL:
#ifdef RBM_PROFILER_TAILCALL_TRASH
            return RBM_PROFILER_TAILCALL_TRASH;
#else
            NYI("Model kill set for CORINFO_HELP_PROF_FCN_TAILCALL on target arch");
#endif

#ifdef TARGET_X86
        case CORINFO_HELP_ASSIGN_REF_EAX:
        case CORINFO_HELP_ASSIGN_REF_ECX:
        case CORINFO_HELP_ASSIGN_REF_EBX:
        case CORINFO_HELP_ASSIGN_REF_EBP:
        case CORINFO_HELP_ASSIGN_REF_ESI:
        case CORINFO_HELP_ASSIGN_REF_EDI:

        case CORINFO_HELP_CHECKED_ASSIGN_REF_EAX:
        case CORINFO_HELP_CHECKED_ASSIGN_REF_ECX:
        case CORINFO_HELP_CHECKED_ASSIGN_REF_EBX:
        case CORINFO_HELP_CHECKED_ASSIGN_REF_EBP:
        case CORINFO_HELP_CHECKED_ASSIGN_REF_ESI:
        case CORINFO_HELP_CHECKED_ASSIGN_REF_EDI:
            return RBM_EDX;

#ifdef FEATURE_USE_ASM_GC_WRITE_BARRIERS
        case CORINFO_HELP_ASSIGN_REF:
        case CORINFO_HELP_CHECKED_ASSIGN_REF:
            return RBM_EAX | RBM_EDX;
#endif // FEATURE_USE_ASM_GC_WRITE_BARRIERS
#endif

        case CORINFO_HELP_STOP_FOR_GC:
            return RBM_STOP_FOR_GC_TRASH;

        case CORINFO_HELP_INIT_PINVOKE_FRAME:
            return RBM_INIT_PINVOKE_FRAME_TRASH;

        default:
            return RBM_CALLEE_TRASH;
    }
}

//------------------------------------------------------------------------
// genOffsetOfMDArrayLowerBound: Returns the offset from the Array object to the
//   lower bound for the given dimension.
//
// Arguments:
//    elemType  - the element type of the array
//    rank      - the rank of the array
//    dimension - the dimension for which the lower bound offset will be returned.
//
// Return Value:
//    The offset.

unsigned CodeGen::genOffsetOfMDArrayLowerBound(var_types elemType, unsigned rank, unsigned dimension)
{
    // Note that the lower bound and length fields of the Array object are always TYP_INT, even on 64-bit targets.
    return compiler->eeGetArrayDataOffset(elemType) + genTypeSize(TYP_INT) * (dimension + rank);
}

//------------------------------------------------------------------------
// genOffsetOfMDArrayLength: Returns the offset from the Array object to the
//   attr for the given dimension.
//
// Arguments:
//    elemType  - the element type of the array
//    rank      - the rank of the array
//    dimension - the dimension for which the lower bound offset will be returned.
//
// Return Value:
//    The offset.

unsigned CodeGen::genOffsetOfMDArrayDimensionSize(var_types elemType, unsigned rank, unsigned dimension)
{
    // Note that the lower bound and length fields of the Array object are always TYP_INT, even on 64-bit targets.
    return compiler->eeGetArrayDataOffset(elemType) + genTypeSize(TYP_INT) * dimension;
}

/*****************************************************************************
 *
 *  The following can be used to create basic blocks that serve as labels for
 *  the emitter. Use with caution - these are not real basic blocks!
 *
 */

// inline
BasicBlock* CodeGen::genCreateTempLabel()
{
#ifdef DEBUG
    // These blocks don't affect FP
    compiler->fgSafeBasicBlockCreation = true;
#endif

    BasicBlock* block = compiler->bbNewBasicBlock(BBJ_NONE);

#ifdef DEBUG
    compiler->fgSafeBasicBlockCreation = false;
#endif

    JITDUMP("Mark " FMT_BB " as label: codegen temp block\n", block->bbNum);
    block->bbFlags |= BBF_HAS_LABEL;

    // Use coldness of current block, as this label will
    // be contained in it.
    block->bbFlags |= (compiler->compCurBB->bbFlags & BBF_COLD);

#ifdef DEBUG
#ifdef UNIX_X86_ABI
    block->bbTgtStkDepth = (genStackLevel - curNestedAlignment) / sizeof(int);
#else
#if !FEATURE_FIXED_OUT_ARGS
    block->bbTgtStkDepth = genStackLevel / sizeof(int);
#endif
#endif
#endif

    return block;
}

void CodeGen::genLogLabel(BasicBlock* bb)
{
#ifdef DEBUG
    if (compiler->opts.dspCode)
    {
        printf("\n      L_M%03u_" FMT_BB ":\n", compiler->compMethodID, bb->bbNum);
    }
#endif
}

// genDefineTempLabel: Define a label based on the current GC info tracked by
// the code generator.
//
// Arguments:
//     label - A label represented as a basic block. These are created with
//     genCreateTempLabel and are not normal basic blocks.
//
// Notes:
//     The label will be defined with the current GC info tracked by the code
//     generator. When the emitter sees this label it will thus remove any temporary
//     GC refs it is tracking in registers. For example, a call might produce a ref
//     in RAX which the emitter would track but which would not be tracked in
//     codegen's GC info since codegen would immediately copy it from RAX into its
//     home.
//
void CodeGen::genDefineTempLabel(BasicBlock* label)
{
    genLogLabel(label);
    label->bbEmitCookie = GetEmitter()->emitAddLabel(INDEBUG(label));
}

// genDefineInlineTempLabel: Define an inline label that does not affect the GC
// info.
//
// Arguments:
//     label - A label represented as a basic block. These are created with
//     genCreateTempLabel and are not normal basic blocks.
//
// Notes:
//     The emitter will continue to track GC info as if there was no label.
//
void CodeGen::genDefineInlineTempLabel(BasicBlock* label)
{
    genLogLabel(label);
    label->bbEmitCookie = GetEmitter()->emitAddInlineLabel();
}

bool AddrMode::IsIndexScale(size_t value)
{
    return (value == 1) || (value == 2) || (value == 4) || (value == 8);
}

bool AddrMode::IsIndexShift(ssize_t value)
{
    return (0 < value) && (value < 4);
}

unsigned AddrMode::GetMulIndexScale(GenTree* node)
{
    if (GenTreeIntCon* intCon = node->IsIntCon())
    {
        if (IsIndexScale(intCon->GetValue()) && (intCon->GetValue() != 1))
        {
            return intCon->GetUInt32Value();
        }
    }

    return 0;
}

unsigned AddrMode::GetLshIndexScale(GenTree* node)
{
    if (GenTreeIntCon* intCon = node->IsIntCon())
    {
        if (IsIndexShift(intCon->GetValue()))
        {
            return 1u << intCon->GetValue();
        }
    }

    return 0;
}

unsigned AddrMode::GetIndexScale(GenTree* node)
{
    // In minopts we may get CNS_INT * CNS_INT, leave it alone.
    if (!node->OperIs(GT_LSH, GT_MUL) || node->AsOp()->GetOp(0)->IsIntCon())
    {
        return 0;
    }

    if (node->OperIs(GT_LSH))
    {
        return GetLshIndexScale(node->AsOp()->GetOp(1));
    }

    return node->gtOverflow() ? 0 : GetMulIndexScale(node->AsOp()->GetOp(1));
}

void AddrMode::AddNode(GenTree* node)
{
    // We keep increasing nodeCount even if there's no room left to add a new node
    // so we can distinguish between having exactly countof(nodes) nodes and having
    // more than that and implicitly discarding them.
    if (++nodeCount <= _countof(nodes))
    {
        nodes[nodeCount - 1] = node;
    }
}

bool AddrMode::HasTooManyNodes() const
{
    return nodeCount > _countof(nodes);
}

GenTree* AddrMode::ExtractOffset(Compiler* compiler, GenTree* op)
{
    GenTree* val = op->SkipComma();

    while (val->OperIs(GT_ADD) && !val->gtOverflow())
    {
        GenTree*       offs    = val->AsOp()->GetOp(1);
        GenTreeIntCon* offsVal = offs->SkipComma()->IsIntCon();

        if ((offsVal == nullptr) || !FitsIn<int32_t>(offsVal->GetValue() + offset))
        {
            break;
        }

        // TODO-MIKE-Review: Shouldn't this assert be an if?
        assert(!offsVal->AsIntCon()->ImmedValNeedsReloc(compiler));

        offset = static_cast<int32_t>(offset + offsVal->GetValue());

        while (op != val)
        {
            AddNode(op);
            op = op->AsOp()->GetOp(1);
        }

        AddNode(op);

        while (offs != offsVal)
        {
            AddNode(offs);
            offs = offs->AsOp()->GetOp(1);
        }

        AddNode(offs);

        op  = op->AsOp()->GetOp(0);
        val = op->SkipComma();
    }

    return op;
}

GenTree* AddrMode::ExtractScale(GenTree* index)
{
    while (unsigned newScale = GetIndexScale(index))
    {
        if (!IsIndexScale(scale * newScale))
        {
            break;
        }

        scale = scale * newScale;
        AddNode(index);
        AddNode(index->AsOp()->GetOp(1));
        index = index->AsOp()->GetOp(0);
    }

    return index;
}

// Take an address expression and try to find the best set of components to
// form an address mode; returns true if this is successful.
void AddrMode::Extract(Compiler* compiler)
{
    base = ExtractOffset(compiler, base);

    if (base->OperIs(GT_ADD) && !base->gtOverflow()
#ifndef TARGET_XARCH
        && (offset == 0)
#endif
            )
    {
        AddNode(base);
        index = base->AsOp()->GetOp(1);
        base  = base->AsOp()->GetOp(0);
        scale = 1;

#ifdef TARGET_XARCH
        base  = ExtractOffset(compiler, base);
        index = ExtractOffset(compiler, index);
#endif

        // Index should not be a GC pointer
        if (varTypeIsGC(index->GetType()))
        {
            noway_assert(!varTypeIsGC(base->GetType()));
            std::swap(base, index);
        }
    }

#ifdef TARGET_XARCH
    // TODO-ARM64-CQ, TODO-ARM-CQ: For now we don't try to create a scaled index.
    if (GetIndexScale(base) != 0)
    {
        std::swap(base, index);
        scale = 1;
    }

    if (index != nullptr)
    {
        index = ExtractScale(index);
    }
#endif

    assert((base != nullptr) || ((index != nullptr) && (scale > 1)));
}

/*****************************************************************************
 *
 *  Generate an exit sequence for a return from a method (note: when compiling
 *  for speed there might be multiple exit points).
 */

void CodeGen::genExitCode(BasicBlock* block)
{
    /* Just wrote the first instruction of the epilog - inform debugger
       Note that this may result in a duplicate IPmapping entry, and
       that this is ok  */

    // For non-optimized debuggable code, there is only one epilog.
    genIPmappingAdd((IL_OFFSETX)ICorDebugInfo::EPILOG, true);

    if (compiler->getNeedsGSSecurityCookie())
    {
        bool jmpEpilog = ((block->bbFlags & BBF_HAS_JMP) != 0);

#ifdef TARGET_XARCH
        genEmitGSCookieCheck(jmpEpilog);
#else
        genEmitGSCookieCheck();
#endif
    }

    GetEmitter()->emitCreatePlaceholderIG(IGPT_EPILOG, block);
}

//------------------------------------------------------------------------
// genJumpToThrowHlpBlk: Generate code for an out-of-line exception.
//
// Notes:
//   For code that uses throw helper blocks, we share the helper blocks created by fgGetThrowHelperBlock.
//   Otherwise, we generate the 'throw' inline.
//
// Arguments:
//   jumpKind - jump kind to generate;
//   codeKind - the special throw-helper kind;
//   failBlk  - optional fail target block, if it is already known;
//
void CodeGen::genJumpToThrowHlpBlk(emitJumpKind jumpKind, ThrowHelperKind codeKind, BasicBlock* failBlk)
{
    bool useThrowHlpBlk = compiler->fgUseThrowHelperBlocks();
#if defined(UNIX_X86_ABI) && defined(FEATURE_EH_FUNCLETS)
    // Inline exception-throwing code in funclet to make it possible to unwind funclet frames.
    useThrowHlpBlk = useThrowHlpBlk && (compiler->funCurrentFunc()->funKind == FUNC_ROOT);
#endif // UNIX_X86_ABI && FEATURE_EH_FUNCLETS

    if (useThrowHlpBlk)
    {
        // For code with throw helper blocks, find and use the helper block for
        // raising the exception. The block may be shared by other trees too.

        BasicBlock* excpRaisingBlock;

        if (failBlk != nullptr)
        {
            // We already know which block to jump to. Use that.
            excpRaisingBlock = failBlk;

#ifdef DEBUG
            ThrowHelperBlock* helper = compiler->fgFindThrowHelperBlock(codeKind, compiler->compCurBB);

            assert(excpRaisingBlock == helper->block);
#if !FEATURE_FIXED_OUT_ARGS
            assert(helper->stackLevelSet || isFramePointerUsed());
#endif
#endif
        }
        else
        {
            ThrowHelperBlock* helper = compiler->fgFindThrowHelperBlock(codeKind, compiler->compCurBB);

            PREFIX_ASSUME_MSG((helper != nullptr), ("ERROR: failed to find exception throw block"));
            excpRaisingBlock = helper->block;
#if !FEATURE_FIXED_OUT_ARGS
            assert(helper->stackLevelSet || isFramePointerUsed());
#endif
        }

        noway_assert(excpRaisingBlock != nullptr);

        // Jump to the exception-throwing block on error.
        inst_JMP(jumpKind, excpRaisingBlock);
    }
    else
    {
        // The code to throw the exception will be generated inline, and
        //  we will jump around it in the normal non-exception case.

        BasicBlock*  tgtBlk          = nullptr;
        emitJumpKind reverseJumpKind = emitter::emitReverseJumpKind(jumpKind);
        if (reverseJumpKind != jumpKind)
        {
            tgtBlk = genCreateTempLabel();
            inst_JMP(reverseJumpKind, tgtBlk);
        }

        genEmitHelperCall(Compiler::GetThrowHelperCall(codeKind));

        // Define the spot for the normal non-exception case to jump to.
        if (tgtBlk != nullptr)
        {
            assert(reverseJumpKind != jumpKind);
            genDefineTempLabel(tgtBlk);
        }
    }
}

/*****************************************************************************
 *
 * The last operation done was generating code for "tree" and that would
 * have set the flags. Check if the operation caused an overflow.
 */

// inline
void CodeGen::genCheckOverflow(GenTree* tree)
{
    // Overflow-check should be asked for this tree
    noway_assert(tree->gtOverflow());

    const var_types type = tree->TypeGet();

    // Overflow checks can only occur for the non-small types: (i.e. TYP_INT,TYP_LONG)
    noway_assert(!varTypeIsSmall(type));

    emitJumpKind jumpKind;

#ifdef TARGET_ARM64
    if (tree->OperGet() == GT_MUL)
    {
        jumpKind = EJ_ne;
    }
    else
#endif
    {
        bool isUnsignedOverflow = ((tree->gtFlags & GTF_UNSIGNED) != 0);

#if defined(TARGET_XARCH)

        jumpKind = isUnsignedOverflow ? EJ_jb : EJ_jo;

#elif defined(TARGET_ARMARCH)

        jumpKind = isUnsignedOverflow ? EJ_lo : EJ_vs;

        if (jumpKind == EJ_lo)
        {
            if (tree->OperGet() != GT_SUB)
            {
                jumpKind = EJ_hs;
            }
        }

#endif // defined(TARGET_ARMARCH)
    }

    genJumpToThrowHlpBlk(jumpKind, ThrowHelperKind::Overflow);
}

#if defined(FEATURE_EH_FUNCLETS)

/*****************************************************************************
 *
 *  Update the current funclet as needed by calling genUpdateCurrentFunclet().
 *  For non-BBF_FUNCLET_BEG blocks, it asserts that the current funclet
 *  is up-to-date.
 *
 */

void CodeGen::genUpdateCurrentFunclet(BasicBlock* block)
{
    if (block->bbFlags & BBF_FUNCLET_BEG)
    {
        compiler->funSetCurrentFunc(compiler->funGetFuncIdx(block));
        if (compiler->funCurrentFunc()->funKind == FUNC_FILTER)
        {
            assert(compiler->ehGetDsc(compiler->funCurrentFunc()->funEHIndex)->ebdFilter == block);
        }
        else
        {
            // We shouldn't see FUNC_ROOT
            assert(compiler->funCurrentFunc()->funKind == FUNC_HANDLER);
            assert(compiler->ehGetDsc(compiler->funCurrentFunc()->funEHIndex)->ebdHndBeg == block);
        }
    }
    else
    {
        assert(compiler->compCurrFuncIdx <= compiler->compFuncInfoCount);
        if (compiler->funCurrentFunc()->funKind == FUNC_FILTER)
        {
            assert(compiler->ehGetDsc(compiler->funCurrentFunc()->funEHIndex)->InFilterRegionBBRange(block));
        }
        else if (compiler->funCurrentFunc()->funKind == FUNC_ROOT)
        {
            assert(!block->hasHndIndex());
        }
        else
        {
            assert(compiler->funCurrentFunc()->funKind == FUNC_HANDLER);
            assert(compiler->ehGetDsc(compiler->funCurrentFunc()->funEHIndex)->InHndRegionBBRange(block));
        }
    }
}

#if defined(TARGET_ARM)
void CodeGen::genInsertNopForUnwinder(BasicBlock* block)
{
    // If this block is the target of a finally return, we need to add a preceding NOP, in the same EH region,
    // so the unwinder doesn't get confused by our "movw lr, xxx; movt lr, xxx; b Lyyy" calling convention that
    // calls the funclet during non-exceptional control flow.
    if (block->bbFlags & BBF_FINALLY_TARGET)
    {
        assert(block->bbFlags & BBF_HAS_LABEL);

#ifdef DEBUG
        if (compiler->verbose)
        {
            printf("\nEmitting finally target NOP predecessor for " FMT_BB "\n", block->bbNum);
        }
#endif
        // Create a label that we'll use for computing the start of an EH region, if this block is
        // at the beginning of such a region. If we used the existing bbEmitCookie as is for
        // determining the EH regions, then this NOP would end up outside of the region, if this
        // block starts an EH region. If we pointed the existing bbEmitCookie here, then the NOP
        // would be executed, which we would prefer not to do.

        block->bbUnwindNopEmitCookie = GetEmitter()->emitAddLabel(INDEBUG(block));

        instGen(INS_nop);
    }
}
#endif

#endif // FEATURE_EH_FUNCLETS

void DoPhase(CodeGen* codeGen, Phases phaseId, void (CodeGen::*action)())
{
    class CodeGenPhase final : public Phase<CodeGenPhase>
    {
        CodeGen* codeGen;
        void (CodeGen::*action)();

    public:
        CodeGenPhase(CodeGen* codeGen, Phases phase, void (CodeGen::*action)())
            : Phase(codeGen->GetCompiler(), phase), codeGen(codeGen), action(action)
        {
        }

        PhaseStatus DoPhase()
        {
            (codeGen->*action)();
            return PhaseStatus::MODIFIED_EVERYTHING;
        }
    } phase(codeGen, phaseId, action);

    phase.Run();
}

void CodeGen::genGenerateCode(void** nativeCode, uint32_t* nativeCodeSize)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In genGenerateCode()\n");
        compiler->fgDispBasicBlocks(compiler->verboseTrees);
    }
#endif

    codePtr          = nativeCode;
    nativeSizeOfCode = nativeCodeSize;

    DoPhase(this, PHASE_LINEAR_SCAN, &CodeGen::genAllocateRegisters);
    DoPhase(this, PHASE_GENERATE_CODE, &CodeGen::genGenerateMachineCode);
    DoPhase(this, PHASE_EMIT_CODE, &CodeGen::genEmitMachineCode);
    DoPhase(this, PHASE_EMIT_GCEH, &CodeGen::genEmitUnwindDebugGCandEH);

#if TRACK_LSRA_STATS
    if (JitConfig.DisplayLsraStats() == 2)
    {
        m_lsra->dumpLsraStatsCsv(jitstdout);
    }
#endif
}

void CodeGen::genAllocateRegisters()
{
    m_lsra = new (compiler, CMK_LSRA) LinearScan(compiler);
    m_lsra->doLinearScan();

    regMaskTP modifiedRegs = m_lsra->GetAllocatedRegs();

#ifdef TARGET_ARMARCH
    reservedRegs = m_lsra->GetReservedRegs();
    modifiedRegs |= reservedRegs;
#endif

    calleeSavedModifiedRegs = modifiedRegs & RBM_CALLEE_SAVED;

    spillTemps.PreAllocateTemps(m_lsra->GetTypeSpillCounts());
}

void CodeGen::genGenerateMachineCode()
{
#ifdef DEBUG
    compiler->fgBBcountAtCodegen = compiler->fgBBcount;
    compiler->fgDebugCheckLinks();
    compiler->fgDebugCheckBBlist();
#endif

    genFinalizeFrame();

    GetEmitter()->emitBegFN();

    genCodeForBBlist();
    genGeneratePrologsAndEpilogs();

    GetEmitter()->emitJumpDistBind();
#if FEATURE_LOOP_ALIGN
    GetEmitter()->emitLoopAlignAdjustments();
#endif
}

#ifdef DEBUG
void CodeGen::DumpDisasmHeader() const
{
    printf("; Assembly listing for method %s\n", compiler->info.compFullName);

    printf("; Emitting ");

    if (compiler->compCodeOpt() == SMALL_CODE)
    {
        printf("SMALL_CODE");
    }
    else if (compiler->compCodeOpt() == FAST_CODE)
    {
        printf("FAST_CODE");
    }
    else
    {
        printf("BLENDED_CODE");
    }

    printf(" for ");

#if defined(TARGET_AMD64)
    printf("X64 CPU with %s", compiler->canUseVexEncoding() ? "AVX" : "SSE2");
#elif defined(TARGET_ARM64)
    printf("generic ARM64 CPU");
#elif defined(TARGET_X86)
    printf("generic X86 CPU");
#elif defined(TARGET_ARM)
    printf("generic ARM CPU");
#else
#error Unknown target
#endif

#if defined(TARGET_WINDOWS)
    printf(" - Windows");
#elif defined(TARGET_UNIX)
    printf(" - Unix");
#endif

    printf("\n");

    if (compiler->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_TIER0))
    {
        printf("; Tier-0 compilation\n");
    }
    else if (compiler->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_TIER1))
    {
        printf("; Tier-1 compilation\n");
    }
    else if (compiler->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_READYTORUN))
    {
        printf("; ReadyToRun compilation\n");
    }

    if (compiler->opts.IsOSR())
    {
        printf("; OSR variant for entry point 0x%x\n", compiler->info.compILEntry);
    }

    if ((compiler->opts.optFlags & CLFLG_MAXOPT) == CLFLG_MAXOPT)
    {
        printf("; optimized code\n");
    }
    else if (compiler->opts.compDbgCode)
    {
        printf("; debuggable code\n");
    }
    else if (compiler->opts.MinOpts())
    {
        printf("; MinOpts code\n");
    }
    else
    {
        printf("; unknown optimization flags\n");
    }

    if (compiler->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_BBINSTR))
    {
        printf("; instrumented for collecting profile data\n");
    }
    else if (compiler->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_BBOPT) && compiler->fgHaveProfileData())
    {
        printf("; optimized using profile data\n");
    }

#if DOUBLE_ALIGN
    if (doDoubleAlign())
    {
        printf("; double-aligned frame\n");
    }
    else
#endif
    {
        printf("; %s based frame\n", isFramePointerUsed() ? STR_FPBASE : STR_SPBASE);
    }

    if (GetInterruptible())
    {
        printf("; fully interruptible\n");
    }
    else
    {
        printf("; partially interruptible\n");
    }

    if (compiler->fgHaveProfileData())
    {
        printf("; with PGO: edge weights are %s, and fgCalledCount is " FMT_WT "\n",
               compiler->fgHaveValidEdgeWeights ? "valid" : "invalid", compiler->fgCalledCount);
    }

    if (compiler->fgPgoFailReason != nullptr)
    {
        printf("; %s\n", compiler->fgPgoFailReason);
    }

    if ((compiler->fgPgoInlineePgo + compiler->fgPgoInlineeNoPgo + compiler->fgPgoInlineeNoPgoSingleBlock) > 0)
    {
        printf("; %u inlinees with PGO data; %u single block inlinees; %u inlinees without PGO data\n",
               compiler->fgPgoInlineePgo, compiler->fgPgoInlineeNoPgoSingleBlock, compiler->fgPgoInlineeNoPgo);
    }

    if (compiler->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_ALT_JIT))
    {
        printf("; invoked as altjit\n");
    }
}
#endif // DEBUG

//----------------------------------------------------------------------
// genEmitMachineCode -- emit the actual machine instruction code
//
void CodeGen::genEmitMachineCode()
{
    /* Compute the size of the code sections that we are going to ask the VM
       to allocate. Note that this might not be precisely the size of the
       code we emit, though it's fatal if we emit more code than the size we
       compute here.
       (Note: an example of a case where we emit less code would be useful.)
    */

    GetEmitter()->emitComputeCodeSizes();

#ifdef DEBUG
    unsigned instrCount;

    // Code to test or stress our ability to run a fallback compile.
    // We trigger the fallback here, before asking the VM for any memory,
    // because if not, we will leak mem, as the current codebase can't free
    // the mem after the emitter asks the VM for it. As this is only a stress
    // mode, we only want the functionality, and don't care about the relative
    // ugliness of having the failure here.
    if (!compiler->jitFallbackCompile)
    {
        // Use COMPlus_JitNoForceFallback=1 to prevent NOWAY assert testing from happening,
        // especially that caused by enabling JIT stress.
        if (!JitConfig.JitNoForceFallback())
        {
            if (JitConfig.JitForceFallback() || compiler->compStressCompile(Compiler::STRESS_GENERIC_VARN, 5))
            {
                JITDUMP("\n\n*** forcing no-way fallback -- current jit request will be abandoned ***\n\n");
                NO_WAY_NOASSERT("Stress failure");
            }
        }
    }

#endif // DEBUG

    /* We've finished collecting all the unwind information for the function. Now reserve
       space for it from the VM.
    */

    compiler->unwindReserve();

    codeSize = GetEmitter()->emitEndCodeGen(&prologSize,
#ifdef JIT32_GCENCODER
                                            &epilogSize,
#endif
                                            codePtr, &coldCodePtr, &consPtr DEBUGARG(&instrCount));

#ifdef DEBUG
    assert(compiler->compCodeGenDone == false);

    /* We're done generating code for this function */
    compiler->compCodeGenDone = true;
#endif

#if defined(DEBUG) || defined(LATE_DISASM)
    // Add code size information into the Perf Score
    // All compPerfScore calculations must be performed using doubles
    compiler->info.compPerfScore += ((double)compiler->info.compTotalHotCodeSize * (double)PERFSCORE_CODESIZE_COST_HOT);
    compiler->info.compPerfScore +=
        ((double)compiler->info.compTotalColdCodeSize * (double)PERFSCORE_CODESIZE_COST_COLD);
#endif // DEBUG || LATE_DISASM

#ifdef DEBUG
    if (compiler->opts.disAsm || verbose)
    {
        printf("\n; Total bytes of code %d, prolog size %d, PerfScore %.2f, instruction count %d, allocated bytes for "
               "code %d",
               codeSize, prologSize, compiler->info.compPerfScore, instrCount,
               GetEmitter()->emitTotalHotCodeSize + GetEmitter()->emitTotalColdCodeSize);

#if TRACK_LSRA_STATS
        if (JitConfig.DisplayLsraStats() == 3)
        {
            m_lsra->dumpLsraStatsSummary(jitstdout);
        }
#endif // TRACK_LSRA_STATS

        printf(" (MethodHash=%08x) for method %s\n", compiler->info.compMethodHash(), compiler->info.compFullName);

        printf("; ============================================================\n\n");
        printf(""); // in our logic this causes a flush
    }

    if (verbose)
    {
        printf("*************** After end code gen, before unwindEmit()\n");
        GetEmitter()->emitDispIGlist(true);
    }
#endif

#ifdef DEBUG_ARG_SLOTS
    // Check our max stack level. Needed for fgGetThrowHelperBlock.
    // We need to relax the assert as our estimation won't include code-gen
    // stack changes (which we know don't affect fgGetThrowHelperBlock).
    // NOTE: after emitEndCodeGen (including here), emitMaxStackDepth is a
    // count of DWORD-sized arguments, NOT argument size in bytes.
    {
        unsigned maxAllowedStackDepth = compiler->fgGetPtrArgCntMax() + // Max number of pointer-sized stack arguments.
                                        compiler->compHndBBtabCount +   // Return address for locally-called finallys
                                        2 + // longs/doubles may be transferred via stack, etc
                                        (compiler->compTailCallUsed ? 4 : 0); // CORINFO_HELP_TAILCALL args
#ifdef UNIX_X86_ABI
        // Convert maxNestedAlignment to DWORD count before adding to maxAllowedStackDepth.
        assert(maxNestedAlignment % sizeof(int) == 0);
        maxAllowedStackDepth += maxNestedAlignment / sizeof(int);
#endif
        assert(GetEmitter()->emitMaxStackDepth <= maxAllowedStackDepth);
    }
#endif // DEBUG_ARG_SLOTS

    *nativeSizeOfCode                 = codeSize;
    compiler->info.compNativeCodeSize = (UNATIVE_OFFSET)codeSize;

    // printf("%6u bytes of code generated for %s.%s\n", codeSize, compiler->info.compFullName);

    // Make sure that the x86 alignment and cache prefetch optimization rules
    // were obeyed.

    // Don't start a method in the last 7 bytes of a 16-byte alignment area
    //   unless we are generating SMALL_CODE
    // noway_assert( (((unsigned)(*codePtr) % 16) <= 8) || (compiler->compCodeOpt() == SMALL_CODE));
}

//----------------------------------------------------------------------
// genEmitUnwindDebugGCandEH: emit unwind, debug, gc, and EH info
//
void CodeGen::genEmitUnwindDebugGCandEH()
{
    /* Now that the code is issued, we can finalize and emit the unwind data */

    compiler->unwindEmit(*codePtr, coldCodePtr);

    /* Finalize the line # tracking logic after we know the exact block sizes/offsets */

    genIPmappingGen();

    /* Finalize the Local Var info in terms of generated code */

    genSetScopeInfo();

#if defined(USING_VARIABLE_LIVE_RANGE) && defined(DEBUG)
    if (compiler->verbose)
    {
        varLiveKeeper->dumpLvaVariableLiveRanges();
    }
#endif // defined(USING_VARIABLE_LIVE_RANGE) && defined(DEBUG)

#ifdef LATE_DISASM
    unsigned finalHotCodeSize;
    unsigned finalColdCodeSize;
    if (compiler->fgFirstColdBlock != nullptr)
    {
        // We did some hot/cold splitting. The hot section is always padded out to the
        // size we thought it would be, but the cold section is not.
        assert(codeSize <= compiler->info.compTotalHotCodeSize + compiler->info.compTotalColdCodeSize);
        assert(compiler->info.compTotalHotCodeSize > 0);
        assert(compiler->info.compTotalColdCodeSize > 0);
        finalHotCodeSize  = compiler->info.compTotalHotCodeSize;
        finalColdCodeSize = codeSize - finalHotCodeSize;
    }
    else
    {
        // No hot/cold splitting
        assert(codeSize <= compiler->info.compTotalHotCodeSize);
        assert(compiler->info.compTotalHotCodeSize > 0);
        assert(compiler->info.compTotalColdCodeSize == 0);
        finalHotCodeSize  = codeSize;
        finalColdCodeSize = 0;
    }
    getDisAssembler().disAsmCode((BYTE*)*codePtr, finalHotCodeSize, (BYTE*)coldCodePtr, finalColdCodeSize);
#endif // LATE_DISASM

    /* Report any exception handlers to the VM */

    genReportEH();

#ifdef JIT32_GCENCODER
    INDEBUG(void* infoPtr =)
    GetEmitter()->gcInfo.CreateAndStoreGCInfo(this, codeSize, prologSize, epilogSize);
#else
    GetEmitter()->gcInfo.CreateAndStoreGCInfo(codeSize, prologSize);
#endif

#ifdef DEBUG
    FILE* dmpf = jitstdout;

    compiler->opts.dmpHex = false;
    if (!strcmp(compiler->info.compMethodName, "<name of method you want the hex dump for"))
    {
        FILE*   codf;
        errno_t ec = fopen_s(&codf, "C:\\JIT.COD", "at"); // NOTE: file append mode
        if (ec != 0)
        {
            assert(codf);
            dmpf                  = codf;
            compiler->opts.dmpHex = true;
        }
    }
    if (compiler->opts.dmpHex)
    {
        size_t consSize = GetEmitter()->emitDataSize();

        fprintf(dmpf, "Generated code for %s:\n", compiler->info.compFullName);
        fprintf(dmpf, "\n");

        if (codeSize)
        {
            fprintf(dmpf, "    Code  at %p [%04X bytes]\n", dspPtr(*codePtr), codeSize);
        }
        if (consSize)
        {
            fprintf(dmpf, "    Const at %p [%04X bytes]\n", dspPtr(consPtr), consSize);
        }
#ifdef JIT32_GCENCODER
        if (compInfoBlkSize != 0)
        {
            fprintf(dmpf, "    Info  at %p [%04X bytes]\n", dspPtr(infoPtr), compInfoBlkSize);
        }
#endif

        fprintf(dmpf, "\n");

        fflush(dmpf);
    }

    if (dmpf != jitstdout)
    {
        fclose(dmpf);
    }

#endif // DEBUG

#if DISPLAY_SIZES

    size_t dataSize = GetEmitter()->emitDataSize();
    grossVMsize += compiler->info.compILCodeSize;
    totalNCsize += codeSize + dataSize + compInfoBlkSize;
    grossNCsize += codeSize + dataSize;

#endif // DISPLAY_SIZES
}

/*****************************************************************************
 *
 *  Report EH clauses to the VM
 */

void CodeGen::genReportEH()
{
    if (compiler->compHndBBtabCount == 0)
    {
        return;
    }

#ifdef DEBUG
    if (compiler->opts.dspEHTable)
    {
        printf("*************** EH table for %s\n", compiler->info.compFullName);
    }
#endif // DEBUG

    unsigned XTnum;

    bool isCoreRTABI = compiler->IsTargetAbi(CORINFO_CORERT_ABI);

    unsigned EHCount = compiler->compHndBBtabCount;

#if defined(FEATURE_EH_FUNCLETS)
    // Count duplicated clauses. This uses the same logic as below, where we actually generate them for reporting to the
    // VM.
    unsigned duplicateClauseCount = 0;
    unsigned enclosingTryIndex;

    // Duplicate clauses are not used by CoreRT ABI
    if (!isCoreRTABI)
    {
        for (XTnum = 0; XTnum < compiler->compHndBBtabCount; XTnum++)
        {
            for (enclosingTryIndex = compiler->ehTrueEnclosingTryIndexIL(XTnum); // find the true enclosing try index,
                                                                                 // ignoring 'mutual protect' trys
                 enclosingTryIndex != EHblkDsc::NO_ENCLOSING_INDEX;
                 enclosingTryIndex = compiler->ehGetEnclosingTryIndex(enclosingTryIndex))
            {
                ++duplicateClauseCount;
            }
        }
        EHCount += duplicateClauseCount;
    }

#if FEATURE_EH_CALLFINALLY_THUNKS
    unsigned clonedFinallyCount = 0;

    // Duplicate clauses are not used by CoreRT ABI
    if (!isCoreRTABI)
    {
        // We don't keep track of how many cloned finally there are. So, go through and count.
        // We do a quick pass first through the EH table to see if there are any try/finally
        // clauses. If there aren't, we don't need to look for BBJ_CALLFINALLY.

        bool anyFinallys = false;
        for (EHblkDsc* const HBtab : EHClauses(compiler))
        {
            if (HBtab->HasFinallyHandler())
            {
                anyFinallys = true;
                break;
            }
        }
        if (anyFinallys)
        {
            for (BasicBlock* const block : compiler->Blocks())
            {
                if (block->bbJumpKind == BBJ_CALLFINALLY)
                {
                    ++clonedFinallyCount;
                }
            }

            EHCount += clonedFinallyCount;
        }
    }
#endif // FEATURE_EH_CALLFINALLY_THUNKS

#endif // FEATURE_EH_FUNCLETS

#ifdef DEBUG
    if (compiler->opts.dspEHTable)
    {
#if defined(FEATURE_EH_FUNCLETS)
#if FEATURE_EH_CALLFINALLY_THUNKS
        printf("%d EH table entries, %d duplicate clauses, %d cloned finallys, %d total EH entries reported to VM\n",
               compiler->compHndBBtabCount, duplicateClauseCount, clonedFinallyCount, EHCount);
        assert(compiler->compHndBBtabCount + duplicateClauseCount + clonedFinallyCount == EHCount);
#else  // !FEATURE_EH_CALLFINALLY_THUNKS
        printf("%d EH table entries, %d duplicate clauses, %d total EH entries reported to VM\n",
               compiler->compHndBBtabCount, duplicateClauseCount, EHCount);
        assert(compiler->compHndBBtabCount + duplicateClauseCount == EHCount);
#endif // !FEATURE_EH_CALLFINALLY_THUNKS
#else  // !FEATURE_EH_FUNCLETS
        printf("%d EH table entries, %d total EH entries reported to VM\n", compiler->compHndBBtabCount, EHCount);
        assert(compiler->compHndBBtabCount == EHCount);
#endif // !FEATURE_EH_FUNCLETS
    }
#endif // DEBUG

    // Tell the VM how many EH clauses to expect.
    compiler->eeSetEHcount(EHCount);

    XTnum = 0; // This is the index we pass to the VM

    for (EHblkDsc* const HBtab : EHClauses(compiler))
    {
        UNATIVE_OFFSET tryBeg, tryEnd, hndBeg, hndEnd, hndTyp;

        tryBeg = compiler->ehCodeOffset(HBtab->ebdTryBeg);
        hndBeg = compiler->ehCodeOffset(HBtab->ebdHndBeg);

        tryEnd = (HBtab->ebdTryLast == compiler->fgLastBB) ? compiler->info.compNativeCodeSize
                                                           : compiler->ehCodeOffset(HBtab->ebdTryLast->bbNext);
        hndEnd = (HBtab->ebdHndLast == compiler->fgLastBB) ? compiler->info.compNativeCodeSize
                                                           : compiler->ehCodeOffset(HBtab->ebdHndLast->bbNext);

        if (HBtab->HasFilter())
        {
            hndTyp = compiler->ehCodeOffset(HBtab->ebdFilter);
        }
        else
        {
            hndTyp = HBtab->ebdTyp;
        }

        CORINFO_EH_CLAUSE_FLAGS flags = ToCORINFO_EH_CLAUSE_FLAGS(HBtab->ebdHandlerType);

        if (isCoreRTABI && (XTnum > 0))
        {
            // For CoreRT, CORINFO_EH_CLAUSE_SAMETRY flag means that the current clause covers same
            // try block as the previous one. The runtime cannot reliably infer this information from
            // native code offsets because of different try blocks can have same offsets. Alternative
            // solution to this problem would be inserting extra nops to ensure that different try
            // blocks have different offsets.
            if (EHblkDsc::ebdIsSameTry(HBtab, HBtab - 1))
            {
                // The SAMETRY bit should only be set on catch clauses. This is ensured in IL, where only 'catch' is
                // allowed to be mutually-protect. E.g., the C# "try {} catch {} catch {} finally {}" actually exists in
                // IL as "try { try {} catch {} catch {} } finally {}".
                assert(HBtab->HasCatchHandler());
                flags = (CORINFO_EH_CLAUSE_FLAGS)(flags | CORINFO_EH_CLAUSE_SAMETRY);
            }
        }

        // Note that we reuse the CORINFO_EH_CLAUSE type, even though the names of
        // the fields aren't accurate.

        CORINFO_EH_CLAUSE clause;
        clause.ClassToken    = hndTyp; /* filter offset is passed back here for filter-based exception handlers */
        clause.Flags         = flags;
        clause.TryOffset     = tryBeg;
        clause.TryLength     = tryEnd;
        clause.HandlerOffset = hndBeg;
        clause.HandlerLength = hndEnd;

        assert(XTnum < EHCount);

        // Tell the VM about this EH clause.
        compiler->eeSetEHinfo(XTnum, &clause);

        ++XTnum;
    }

#if defined(FEATURE_EH_FUNCLETS)
    // Now output duplicated clauses.
    //
    // If a funclet has been created by moving a handler out of a try region that it was originally nested
    // within, then we need to report a "duplicate" clause representing the fact that an exception in that
    // handler can be caught by the 'try' it has been moved out of. This is because the original 'try' region
    // descriptor can only specify a single, contiguous protected range, but the funclet we've moved out is
    // no longer contiguous with the original 'try' region. The new EH descriptor will have the same handler
    // region as the enclosing try region's handler region. This is the sense in which it is duplicated:
    // there is now a "duplicate" clause with the same handler region as another, but a different 'try'
    // region.
    //
    // For example, consider this (capital letters represent an unknown code sequence, numbers identify a
    // try or handler region):
    //
    // A
    // try (1) {
    //   B
    //   try (2) {
    //     C
    //   } catch (3) {
    //     D
    //   } catch (4) {
    //     E
    //   }
    //   F
    // } catch (5) {
    //   G
    // }
    // H
    //
    // Here, we have try region (1) BCDEF protected by catch (5) G, and region (2) C protected
    // by catch (3) D and catch (4) E. Note that catch (4) E does *NOT* protect the code "D".
    // This is an example of 'mutually protect' regions. First, we move handlers (3) and (4)
    // to the end of the code. However, (3) and (4) are nested inside, and protected by, try (1). Again
    // note that (3) is not nested inside (4), despite ebdEnclosingTryIndex indicating that.
    // The code "D" and "E" won't be contiguous with the protected region for try (1) (which
    // will, after moving catch (3) AND (4), be BCF). Thus, we need to add a new EH descriptor
    // representing try (1) protecting the new funclets catch (3) and (4).
    // The code will be generated as follows:
    //
    // ABCFH // "main" code
    // D // funclet
    // E // funclet
    // G // funclet
    //
    // The EH regions are:
    //
    //  C -> D
    //  C -> E
    //  BCF -> G
    //  D -> G // "duplicate" clause
    //  E -> G // "duplicate" clause
    //
    // Note that we actually need to generate one of these additional "duplicate" clauses for every
    // region the funclet is nested in. Take this example:
    //
    //  A
    //  try (1) {
    //      B
    //      try (2,3) {
    //          C
    //          try (4) {
    //              D
    //              try (5,6) {
    //                  E
    //              } catch {
    //                  F
    //              } catch {
    //                  G
    //              }
    //              H
    //          } catch {
    //              I
    //          }
    //          J
    //      } catch {
    //          K
    //      } catch {
    //          L
    //      }
    //      M
    //  } catch {
    //      N
    //  }
    //  O
    //
    // When we pull out funclets, we get the following generated code:
    //
    // ABCDEHJMO // "main" function
    // F // funclet
    // G // funclet
    // I // funclet
    // K // funclet
    // L // funclet
    // N // funclet
    //
    // And the EH regions we report to the VM are (in order; main clauses
    // first in most-to-least nested order, funclets ("duplicated clauses")
    // last, in most-to-least nested) are:
    //
    //  E -> F
    //  E -> G
    //  DEH -> I
    //  CDEHJ -> K
    //  CDEHJ -> L
    //  BCDEHJM -> N
    //  F -> I // funclet clause #1 for F
    //  F -> K // funclet clause #2 for F
    //  F -> L // funclet clause #3 for F
    //  F -> N // funclet clause #4 for F
    //  G -> I // funclet clause #1 for G
    //  G -> K // funclet clause #2 for G
    //  G -> L // funclet clause #3 for G
    //  G -> N // funclet clause #4 for G
    //  I -> K // funclet clause #1 for I
    //  I -> L // funclet clause #2 for I
    //  I -> N // funclet clause #3 for I
    //  K -> N // funclet clause #1 for K
    //  L -> N // funclet clause #1 for L
    //
    // So whereas the IL had 6 EH clauses, we need to report 19 EH clauses to the VM.
    // Note that due to the nature of 'mutually protect' clauses, it would be incorrect
    // to add a clause "F -> G" because F is NOT protected by G, but we still have
    // both "F -> K" and "F -> L" because F IS protected by both of those handlers.
    //
    // The overall ordering of the clauses is still the same most-to-least nesting
    // after front-to-back start offset. Because we place the funclets at the end
    // these new clauses should also go at the end by this ordering.
    //

    if (duplicateClauseCount > 0)
    {
        unsigned  reportedDuplicateClauseCount = 0; // How many duplicated clauses have we reported?
        unsigned  XTnum2;
        EHblkDsc* HBtab;
        for (XTnum2 = 0, HBtab = compiler->compHndBBtab; XTnum2 < compiler->compHndBBtabCount; XTnum2++, HBtab++)
        {
            unsigned enclosingTryIndex;

            EHblkDsc* fletTab = compiler->ehGetDsc(XTnum2);

            for (enclosingTryIndex = compiler->ehTrueEnclosingTryIndexIL(XTnum2); // find the true enclosing try index,
                                                                                  // ignoring 'mutual protect' trys
                 enclosingTryIndex != EHblkDsc::NO_ENCLOSING_INDEX;
                 enclosingTryIndex = compiler->ehGetEnclosingTryIndex(enclosingTryIndex))
            {
                // The funclet we moved out is nested in a try region, so create a new EH descriptor for the funclet
                // that will have the enclosing try protecting the funclet.

                noway_assert(XTnum2 < enclosingTryIndex); // the enclosing region must be less nested, and hence have a
                                                          // greater EH table index

                EHblkDsc* encTab = compiler->ehGetDsc(enclosingTryIndex);

                // The try region is the handler of the funclet. Note that for filters, we don't protect the
                // filter region, only the filter handler region. This is because exceptions in filters never
                // escape; the VM swallows them.

                BasicBlock* bbTryBeg  = fletTab->ebdHndBeg;
                BasicBlock* bbTryLast = fletTab->ebdHndLast;

                BasicBlock* bbHndBeg  = encTab->ebdHndBeg; // The handler region is the same as the enclosing try
                BasicBlock* bbHndLast = encTab->ebdHndLast;

                UNATIVE_OFFSET tryBeg, tryEnd, hndBeg, hndEnd, hndTyp;

                tryBeg = compiler->ehCodeOffset(bbTryBeg);
                hndBeg = compiler->ehCodeOffset(bbHndBeg);

                tryEnd = (bbTryLast == compiler->fgLastBB) ? compiler->info.compNativeCodeSize
                                                           : compiler->ehCodeOffset(bbTryLast->bbNext);
                hndEnd = (bbHndLast == compiler->fgLastBB) ? compiler->info.compNativeCodeSize
                                                           : compiler->ehCodeOffset(bbHndLast->bbNext);

                if (encTab->HasFilter())
                {
                    hndTyp = compiler->ehCodeOffset(encTab->ebdFilter);
                }
                else
                {
                    hndTyp = encTab->ebdTyp;
                }

                CORINFO_EH_CLAUSE_FLAGS flags = ToCORINFO_EH_CLAUSE_FLAGS(encTab->ebdHandlerType);

                // Tell the VM this is an extra clause caused by moving funclets out of line.
                flags = (CORINFO_EH_CLAUSE_FLAGS)(flags | CORINFO_EH_CLAUSE_DUPLICATE);

                // Note that the JIT-EE interface reuses the CORINFO_EH_CLAUSE type, even though the names of
                // the fields aren't really accurate. For example, we set "TryLength" to the offset of the
                // instruction immediately after the 'try' body. So, it really could be more accurately named
                // "TryEndOffset".

                CORINFO_EH_CLAUSE clause;
                clause.ClassToken = hndTyp; /* filter offset is passed back here for filter-based exception handlers */
                clause.Flags      = flags;
                clause.TryOffset  = tryBeg;
                clause.TryLength  = tryEnd;
                clause.HandlerOffset = hndBeg;
                clause.HandlerLength = hndEnd;

                assert(XTnum < EHCount);

                // Tell the VM about this EH clause (a duplicated clause).
                compiler->eeSetEHinfo(XTnum, &clause);

                ++XTnum;
                ++reportedDuplicateClauseCount;

#ifndef DEBUG
                if (duplicateClauseCount == reportedDuplicateClauseCount)
                {
                    break; // we've reported all of them; no need to continue looking
                }
#endif // !DEBUG

            } // for each 'true' enclosing 'try'
        }     // for each EH table entry

        assert(duplicateClauseCount == reportedDuplicateClauseCount);
    } // if (duplicateClauseCount > 0)

#if FEATURE_EH_CALLFINALLY_THUNKS
    if (clonedFinallyCount > 0)
    {
        unsigned reportedClonedFinallyCount = 0;
        for (BasicBlock* const block : compiler->Blocks())
        {
            if (block->bbJumpKind == BBJ_CALLFINALLY)
            {
                UNATIVE_OFFSET hndBeg, hndEnd;

                hndBeg = compiler->ehCodeOffset(block);

                // How big is it? The BBJ_ALWAYS has a null bbEmitCookie! Look for the block after, which must be
                // a label or jump target, since the BBJ_CALLFINALLY doesn't fall through.
                BasicBlock* bbLabel = block->bbNext;
                if (block->isBBCallAlwaysPair())
                {
                    bbLabel = bbLabel->bbNext; // skip the BBJ_ALWAYS
                }
                if (bbLabel == nullptr)
                {
                    hndEnd = compiler->info.compNativeCodeSize;
                }
                else
                {
                    assert(bbLabel->bbEmitCookie != nullptr);
                    hndEnd = compiler->ehCodeOffset(bbLabel);
                }

                CORINFO_EH_CLAUSE clause;
                clause.ClassToken = 0; // unused
                clause.Flags      = (CORINFO_EH_CLAUSE_FLAGS)(CORINFO_EH_CLAUSE_FINALLY | CORINFO_EH_CLAUSE_DUPLICATE);
                clause.TryOffset  = hndBeg;
                clause.TryLength  = hndBeg;
                clause.HandlerOffset = hndBeg;
                clause.HandlerLength = hndEnd;

                assert(XTnum < EHCount);

                // Tell the VM about this EH clause (a cloned finally clause).
                compiler->eeSetEHinfo(XTnum, &clause);

                ++XTnum;
                ++reportedClonedFinallyCount;

#ifndef DEBUG
                if (clonedFinallyCount == reportedClonedFinallyCount)
                {
                    break; // we're done; no need to keep looking
                }
#endif        // !DEBUG
            } // block is BBJ_CALLFINALLY
        }     // for each block

        assert(clonedFinallyCount == reportedClonedFinallyCount);
    }  // if (clonedFinallyCount > 0)
#endif // FEATURE_EH_CALLFINALLY_THUNKS

#endif // FEATURE_EH_FUNCLETS

    assert(XTnum == EHCount);
}

bool CodeGenInterface::UseOptimizedWriteBarriers()
{
#if defined(TARGET_X86) && NOGC_WRITE_BARRIERS
    return true;
#else
    return false;
#endif
}

CorInfoHelpFunc CodeGenInterface::GetWriteBarrierHelperCall(GCInfo::WriteBarrierForm wbf)
{
    assert(wbf != GCInfo::WBF_NoBarrier);

    return (wbf == GCInfo::WBF_BarrierUnchecked) ? CORINFO_HELP_ASSIGN_REF : CORINFO_HELP_CHECKED_ASSIGN_REF;
}

void CodeGen::genGCWriteBarrier(GenTreeStoreInd* store, GCInfo::WriteBarrierForm wbf)
{
    genEmitHelperCall(GetWriteBarrierHelperCall(wbf), EA_PTRSIZE);
}

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                           Prolog / Epilog                                 XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

void CodeGen::PrologMoveParams(regNumber initReg, bool* initRegZeroed)
{
    if (paramRegState.intRegLiveIn != RBM_NONE)
    {
        // If we need an extra register to shuffle around the incoming registers
        // we will use xtraReg (initReg) and set the xtraRegClobbered flag,
        // if we don't need to use the xtraReg then this flag will stay false
        regNumber xtraReg;
        bool      xtraRegClobbered = false;

        if ((genRegMask(initReg) & RBM_ARG_REGS) != 0)
        {
            xtraReg = initReg;
        }
        else
        {
            xtraReg        = REG_SCRATCH;
            *initRegZeroed = false;
        }

        genPrologMoveParamRegs(paramRegState.intRegCount, paramRegState.intRegLiveIn, false, xtraReg,
                               &xtraRegClobbered);

        if (xtraRegClobbered)
        {
            *initRegZeroed = false;
        }
    }

#ifndef TARGET_X86
    if (paramRegState.floatRegLiveIn != RBM_NONE)
    {
        bool xtraRegClobbered = false;

        genPrologMoveParamRegs(paramRegState.floatRegCount, paramRegState.floatRegLiveIn, true, REG_NA,
                               &xtraRegClobbered);

        // TODO-MIKE-Review: This should probably be done only for integer registers.
        if (xtraRegClobbered)
        {
            *initRegZeroed = false;
        }
    }
#endif

    genPrologEnregisterIncomingStackParams();
}

// Generates code for moving incoming register arguments to their
// assigned location, in the function prolog.
void CodeGen::genPrologMoveParamRegs(
    unsigned regCount, regMaskTP regLiveIn, bool isFloat, regNumber tempReg, bool* tempRegClobbered)
{
    assert(generatingProlog);
    assert(regLiveIn != RBM_NONE);

    JITDUMP("*************** In genPrologMoveParamRegs() for %s regs\n", isFloat ? "float" : "int");

    // If a method has 3 args (and no fixed return buffer) then argMax is 3 and valid indexes are 0,1,2
    // If a method has a fixed return buffer (on ARM64) then argMax gets set to 9 and valid index are 0-8
    //
    // The paramRegs can always have unused entries,
    //    for example if an architecture always increments the arg register number but uses either
    //    an integer register or a floating point register to hold the next argument
    //    then with a mix of float and integer args you could have:
    //
    //    sampleMethod(int i, float x, int j, float y, int k, float z);
    //          r0, r2 and r4 as valid integer arguments with argMax as 5
    //      and f1, f3 and f5 and valid floating point arguments with argMax as 6
    //    The first one is isFloat==false and the second one is isFloat==true
    //
    //    If a fixed return buffer (in r8) was also present then the first one would become:
    //          r0, r2, r4 and r8 as valid integer arguments with argMax as 9

    // If necessary we will select a correct xtraReg for circular floating point args later.
    if (isFloat)
    {
        assert(tempReg == REG_NA);

        noway_assert(regCount <= MAX_FLOAT_REG_ARG);
    }
    else // we are doing the integer registers
    {
        noway_assert(regCount <= MAX_REG_ARG);

#ifdef TARGET_ARM64
        regCount = RET_BUFF_ARGNUM + 1;

        assert(regCount == MAX_REG_ARG + 1);
#endif
    }

    // Construct a table with the register arguments, for detecting circular and
    // non-circular dependencies between the register arguments. A dependency is when
    // an argument register Rn needs to be moved to register Rm that is also an argument
    // register. The table is constructed in the order the arguments are passed in
    // registers: the first register argument is in paramRegs[0], the second in
    // paramRegs[1], etc. Note that on ARM, a TYP_DOUBLE takes two entries, starting
    // at an even index. The paramRegs is indexed from 0 to paramRegCount - 1.
    // Note that due to an extra argument register for ARM64 (REG_ARG_RET_BUFF)
    // we have increased the allocated size of the paramRegs by one.

    ParamRegInfo paramRegs[max(MAX_REG_ARG + 1, MAX_FLOAT_REG_ARG)]{};

    regMaskTP liveParamRegs = genPrologBuildParamRegsTable(paramRegs, regCount, regLiveIn, isFloat, tempReg);

    if (liveParamRegs != RBM_NONE)
    {
        genPrologMarkParamRegsCircularDependencies(paramRegs, regCount, liveParamRegs);
    }

    liveParamRegs = genPrologSpillParamRegs(paramRegs, regCount, regLiveIn);

    if (liveParamRegs != RBM_NONE)
    {
        genPrologMoveParamRegs(paramRegs, regCount, liveParamRegs, isFloat, tempReg, tempRegClobbered);
    }
}

// Map a parameter register to a parameter register index.
unsigned genGetParamRegIndex(regNumber regNum)
{
    if (emitter::isFloatReg(regNum))
    {
        assert(isValidFloatArgReg(regNum));

        return regNum - FIRST_FP_ARGREG;
    }

#ifdef TARGET_ARMARCH
    assert(isValidIntArgReg(regNum));

    return regNum - REG_ARG_0;
#elif defined(TARGET_XARCH)
    switch (regNum)
    {
        case REG_ARG_0:
            return 0;
        case REG_ARG_1:
            return 1;
#ifdef TARGET_AMD64
        case REG_ARG_2:
            return 2;
        case REG_ARG_3:
            return 3;
#endif
#ifdef UNIX_AMD64_ABI
        case REG_ARG_4:
            return 4;
        case REG_ARG_5:
            return 5;
#endif
        default:
            assert(!"Invalid int param reg");
            return UINT32_MAX;
    }
#endif // TARGET_XARCH
}

regMaskTP CodeGen::genPrologBuildParamRegsTable(
    ParamRegInfo* paramRegs, unsigned paramRegCount, regMaskTP liveParamRegs, bool isFloat, regNumber tempReg)
{
    for (unsigned lclNum = 0; lclNum < compiler->lvaCount; ++lclNum)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

        if (!lcl->IsParam() || !lcl->IsRegParam())
        {
            continue;
        }

        // When we have a promoted struct we have two possible locals that can represent the param
        // in paramRegs, either the original TYP_STRUCT argument or the promoted field local.
        // We will use the field if the promotion is independent, otherwise we'll use the original
        // STRUCT parameter.
        if (lcl->IsPromoted() || lcl->IsPromotedField())
        {
            LclVarDsc* parentLcl = lcl;

            if (lcl->IsPromotedField())
            {
                assert(!lcl->IsPromoted());
                parentLcl = compiler->lvaGetDesc(lcl->GetPromotedFieldParentLclNum());
            }

            if (parentLcl->IsIndependentPromoted())
            {
                // For register arguments that are independent promoted structs we put the promoted field
                // lclNum in the paramRegs.
                if (lcl->IsPromoted())
                {
                    continue;
                }
            }
            else
            {
                // For register arguments that are not independent promoted structs we put the parent struct
                // lclNum in the paramRegs.
                if (lcl->IsPromotedField())
                {
                    continue;
                }
            }
        }

        unsigned regCount = 0;
        unsigned paramRegIndex;

#ifdef UNIX_AMD64_ABI
        if (varTypeIsStruct(lcl->GetType()))
        {
            lcl->GetLayout()->EnsureSysVAmd64AbiInfo(compiler);

            unsigned firstRegIndex = 0;

            for (unsigned regIndex = 0; regIndex < lcl->GetLayout()->GetSysVAmd64AbiRegCount(); regIndex++)
            {
                regNumber regNum = lcl->GetParamReg(regIndex);

                if (emitter::isFloatReg(regNum) != isFloat)
                {
                    continue;
                }

                var_types regType;

                if (lcl->TypeIs(TYP_SIMD12))
                {
                    // For SIMD12 the second eightbyte has FLOAT type, we want to widen that
                    // to DOUBLE so that we store 16 bytes instead of 12 if we need to spill
                    // the parameter. SIMD12 operations are ultimately SIMD16 operations and
                    // some of them may expect the extra 4 bytes to be 0.

                    // TODO-MIKE-Review: Yeah, and as usual something is messed up. If we do
                    // not spill we explicitly zero out the extra 4 bytes. If the param is
                    // passed on stack we also zero out by storing 0 to those upper 4 bytes.
                    // But if we spill we don't zero out, we store whatever we get in the 2
                    // param XMM registers. And then ARM64 doesn't seem to zero out anything.

                    regType = TYP_DOUBLE;
                }
                else
                {
                    regType = varActualType(lcl->GetLayout()->GetSysVAmd64AbiRegType(regIndex));
                }

                paramRegIndex = genGetParamRegIndex(regNum);

                if (regCount == 0)
                {
                    firstRegIndex = paramRegIndex;
                }

                noway_assert(paramRegIndex < paramRegCount);
                noway_assert(paramRegs[paramRegIndex].type == TYP_UNDEF);

                paramRegs[paramRegIndex].lclNum   = lclNum;
                paramRegs[paramRegIndex].regIndex = static_cast<uint8_t>(regIndex);
                paramRegs[paramRegIndex].type     = regType;

                regCount++;
            }

            if (regCount == 0)
            {
                continue;
            }

            paramRegIndex = firstRegIndex;
        }
        else
#endif // UNIX_AMD64_ABI
        {
            if (emitter::isFloatReg(lcl->GetParamReg()) != isFloat)
            {
                continue;
            }

            var_types regType = lcl->GetType();

#ifdef TARGET_ARMARCH
            if (lcl->IsHfaRegParam())
            {
#if defined(TARGET_WINDOWS) && defined(TARGET_ARM64)
                assert(!compiler->info.compIsVarArgs);
#endif
                regType = lcl->GetLayout()->GetHfaElementType();
            }
#if defined(TARGET_WINDOWS) && defined(TARGET_ARM64)
            else if (compiler->info.compIsVarArgs && varTypeIsSIMD(regType))
            {
                regType = TYP_LONG;
            }
#endif
            else
            {
                regType = compiler->mangleVarArgsType(regType);
            }
#endif // TARGET_ARMARCH

            paramRegIndex = genGetParamRegIndex(lcl->GetParamReg());
            regCount      = lcl->GetParamRegCount();

            noway_assert(paramRegIndex + regCount <= paramRegCount);

            for (unsigned i = 0; i < regCount; i++)
            {
                noway_assert(paramRegs[paramRegIndex + i].type == TYP_UNDEF);

                paramRegs[paramRegIndex + i].lclNum   = lclNum;
                paramRegs[paramRegIndex + i].regIndex = static_cast<uint8_t>(i);
                paramRegs[paramRegIndex + i].type     = regType;
            }
        }

        for (unsigned i = 0; i < regCount; i++)
        {
            var_types regType = paramRegs[paramRegIndex + i].type;
            regNumber regNum  = genMapRegArgNumToRegNum(paramRegIndex + i, regType);
            regMaskTP regMask = genRegMask(regNum);

#ifndef UNIX_AMD64_ABI
            assert((i > 0) || (regNum == lcl->GetParamReg()));
#endif

            if ((liveParamRegs & regMask) == RBM_NONE)
            {
                if (lcl->HasLiveness() && !lcl->TypeIs(TYP_STRUCT))
                {
                    // We may now see some tracked locals with zero refs.
                    // See Lowering::DoPhase. Tolerate these.

                    if (lcl->GetRefCount() > 0)
                    {
                        noway_assert(
                            !VarSetOps::IsMember(compiler, compiler->fgFirstBB->bbLiveIn, lcl->GetLivenessBitIndex()));
                    }
                }
                else
                {
#ifdef TARGET_X86
                    noway_assert(lcl->TypeIs(TYP_STRUCT));
#else
                    // For LSRA, it may not be in liveParamRegs if it has a zero
                    // refcnt.  This is in contrast with the non-LSRA case in which all
                    // non-tracked args are assumed live on entry.

                    noway_assert((lcl->GetRefCount() == 0) || lcl->TypeIs(TYP_STRUCT) ||
                                 (lcl->IsAddressExposed() && compiler->info.compIsVarArgs) ||
                                 (lcl->IsAddressExposed() && compiler->opts.UseSoftFP()));
#endif
                }

                paramRegs[paramRegIndex + i].processed = true;
                liveParamRegs &= ~genRegMask(regNum);

                continue;
            }

#ifdef TARGET_ARM
            // On the ARM when the lcl is a struct arg (or pre-spilled due to varargs) the initReg/xtraReg
            // could be equal to the param reg. The pre-spilled registers are also not considered live either
            // since they've already been spilled.
            if ((preSpillParamRegs & regMask) == RBM_NONE)
#endif
            {
#ifndef UNIX_AMD64_ABI
                noway_assert(tempReg != lcl->GetParamReg() + i);
#endif
                noway_assert((liveParamRegs & regMask) != RBM_NONE);
            }

            paramRegs[paramRegIndex + i].writeThru = lcl->lvIsInReg() && lcl->lvLiveInOutOfHndlr;
            paramRegs[paramRegIndex + i].stackArg  = !lcl->lvIsInReg();

            // If it goes on the stack or in a register that doesn't hold
            // an argument anymore -> CANNOT form a circular dependency.

            if (!lcl->lvIsInReg() || ((regMask & liveParamRegs) == RBM_NONE))
            {
                liveParamRegs &= ~regMask;

                continue;
            }

            // Will trash another argument -> possible dependency
            // We may need several passes after the table is constructed
            // to decide on that.
            // Maybe the argument stays in the register (IDEAL)

            if ((i == 0) && (lcl->GetRegNum() == regNum))
            {
                liveParamRegs &= ~regMask;

                continue;
            }

#ifdef TARGET_ARM
            if ((i == 1) && lcl->TypeIs(TYP_DOUBLE) && (REG_NEXT(lcl->GetRegNum()) == regNum))
            {
                liveParamRegs &= ~regMask;

                continue;
            }
#endif

            paramRegs[paramRegIndex + i].circular = true;
        }
    }

    return liveParamRegs;
}

void CodeGen::genPrologMarkParamRegsCircularDependencies(ParamRegInfo* paramRegs,
                                                         unsigned      paramRegCount,
                                                         regMaskTP     liveParamRegs)
{
    assert(liveParamRegs != RBM_NONE);

    // Find the circular dependencies for the argument registers, if any.
    // A circular dependency is a set of registers R1, R2, ..., Rn
    // such that R1->R2 (that is, R1 needs to be moved to R2), R2->R3, ..., Rn->R1.

    bool change = true;

    // Possible circular dependencies still exist; the previous pass was not enough
    // to filter them out. Use a "sieve" strategy to find all circular dependencies.
    while (change)
    {
        change = false;

        for (unsigned paramRegIndex = 0; paramRegIndex < paramRegCount; paramRegIndex++)
        {
            if (!paramRegs[paramRegIndex].circular)
            {
                continue;
            }

            if (paramRegs[paramRegIndex].type == TYP_UNDEF) // Not a register argument
            {
                continue;
            }

            unsigned   lclNum = paramRegs[paramRegIndex].lclNum;
            LclVarDsc* lcl    = compiler->lvaGetDesc(lclNum);

            // Cannot possibly have stack arguments.
            noway_assert(lcl->lvIsInReg() && !paramRegs[paramRegIndex].stackArg);

            const var_types lclRegType = lcl->GetRegisterType();
            var_types       regType    = paramRegs[paramRegIndex].type;
            regNumber       regNum     = genMapRegArgNumToRegNum(paramRegIndex, regType);
            regNumber       destRegNum = REG_NA;

            if (varTypeIsStruct(lcl->GetType()) && lcl->IsIndependentPromoted())
            {
                destRegNum =
                    compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(paramRegs[paramRegIndex].regIndex))->GetRegNum();
            }
            else if (paramRegs[paramRegIndex].regIndex == 0)
            {
                destRegNum = lcl->GetRegNum();
            }
#ifdef TARGET_ARM64
            else if (lcl->IsHfaRegParam())
            {
                // This must be a SIMD type that's fully enregistered, but is passed as an HFA.
                // Each field will be inserted into the same destination register.
                assert(varTypeIsSIMD(lcl->GetType()) && !lcl->GetLayout()->IsOpaqueVector());
                assert(paramRegs[paramRegIndex].regIndex < lcl->GetLayout()->GetHfaRegCount());
                assert(paramRegIndex != 0);
                assert(paramRegs[paramRegIndex - 1].lclNum == lclNum);

                liveParamRegs &= ~genRegMask(regNum);
                paramRegs[paramRegIndex].circular = false;

                change = true;

                continue;
            }
#endif // TARGET_ARM64
#ifdef UNIX_AMD64_ABI
            else
            {
                assert(paramRegs[paramRegIndex].regIndex == 1);
                assert(paramRegIndex > 0);
                assert(paramRegs[paramRegIndex - 1].regIndex == 0);
                assert(paramRegs[paramRegIndex - 1].lclNum == lclNum);
                assert((lclRegType == TYP_SIMD12) || (lclRegType == TYP_SIMD16));

                liveParamRegs &= ~genRegMask(regNum);
                paramRegs[paramRegIndex].circular = false;

                change = true;

                continue;
            }
#endif // UNIX_AMD64_ABI
#ifndef TARGET_64BIT
            // TODO-MIKE-Cleanup: This is likely dead code.
            else if ((paramRegs[paramRegIndex].regIndex == 1) && lcl->TypeIs(TYP_LONG))
            {
                destRegNum = REG_STK;
            }
            else
            {
                assert(paramRegs[paramRegIndex].regIndex == 1);
                assert(lcl->TypeIs(TYP_DOUBLE));

                destRegNum = REG_NEXT(lcl->GetRegNum());
            }
#endif // !TARGET_64BIT

            noway_assert(destRegNum != REG_NA);

            if (genRegMask(destRegNum) & liveParamRegs)
            {
                // We are trashing a live argument register - record it.

                unsigned destRegArgNum = genGetParamRegIndex(destRegNum);
                noway_assert(destRegArgNum < paramRegCount);
                paramRegs[destRegArgNum].trashBy = paramRegIndex;
            }
            else
            {
                // Argument goes to a free register.
                paramRegs[paramRegIndex].circular = false;
                // Mark the argument register as free.
                liveParamRegs &= ~genRegMask(regNum);

                change = true;
            }
        }
    }

    // At this point, everything that has the "circular" flag
    // set to "true" forms a circular dependency.

    if (liveParamRegs != RBM_NONE)
    {
        JITDUMP("Circular dependencies found while home-ing the incoming arguments.\n");
    }

    // LSRA allocates registers to incoming parameters in order and will not overwrite
    // a register still holding a live parameter.

    noway_assert(((liveParamRegs & RBM_FLTARG_REGS) == RBM_NONE) &&
                 "Homing of float argument registers with circular dependencies not implemented.");
}

regMaskTP CodeGen::genPrologSpillParamRegs(ParamRegInfo* paramRegs, unsigned paramRegCount, regMaskTP liveParamRegs)
{
    // Now move the arguments to their locations.
    // First consider ones that go on the stack since they may free some registers.
    // Also home writeThru args, since they're also homed to the stack.

    for (unsigned paramRegIndex = 0; paramRegIndex < paramRegCount; paramRegIndex++)
    {
        if (paramRegs[paramRegIndex].processed)
        {
            continue;
        }

        if (paramRegs[paramRegIndex].type == TYP_UNDEF) // Not a register argument
        {
            continue;
        }

        unsigned   lclNum = paramRegs[paramRegIndex].lclNum;
        LclVarDsc* lcl    = compiler->lvaGetDesc(lclNum);

        // If this arg is never on the stack, go to the next one.
        if (!paramRegs[paramRegIndex].stackArg && !paramRegs[paramRegIndex].writeThru)
        {
#ifndef TARGET_64BIT
            if (lcl->TypeIs(TYP_LONG))
            {
                if (paramRegs[paramRegIndex].regIndex == 0)
                {
                    continue;
                }
            }
            else
#endif
            {
                continue;
            }
        }

#ifdef TARGET_ARM
        if (lcl->TypeIs(TYP_DOUBLE) && (paramRegs[paramRegIndex].regIndex == 1))
        {
            // We handled the entire double when processing the first half.
            continue;
        }
#endif

        noway_assert(!paramRegs[paramRegIndex].circular);
        noway_assert(!lcl->lvIsInReg() || lcl->lvLiveInOutOfHndlr ||
                     (lcl->TypeIs(TYP_LONG) && (paramRegs[paramRegIndex].regIndex == 1)));

        var_types storeType = TYP_UNDEF;
        unsigned  slotSize  = REGSIZE_BYTES;

        if (varTypeIsStruct(lcl->GetType()))
        {
            storeType = TYP_I_IMPL; // Default store type for a struct type is a pointer sized integer

#if FEATURE_MULTIREG_ARGS
            // Must be <= MAX_PASS_MULTIREG_BYTES or else it wouldn't be passed in registers
            noway_assert(lcl->GetLayout()->GetSize() <= MAX_PASS_MULTIREG_BYTES);
#endif

#ifdef UNIX_AMD64_ABI
            storeType = paramRegs[paramRegIndex].type;
#else

            if (lcl->IsHfaRegParam())
            {
#ifdef TARGET_ARM
                // On ARM32 the storeType for HFA args is always TYP_FLOAT
                storeType = TYP_FLOAT;
#else
                storeType = lcl->GetLayout()->GetHfaElementType();
#endif
                slotSize  = static_cast<unsigned>(emitActualTypeSize(storeType));
            }
#endif // !UNIX_AMD64_ABI
        }
        else
        {
            storeType = varActualType(lcl->GetType());

#ifdef TARGET_ARMARCH
            storeType = compiler->mangleVarArgsType(storeType);
#endif
        }

        emitAttr size = emitActualTypeSize(storeType);

#ifdef TARGET_X86
        noway_assert(varTypeSize(storeType) == REGSIZE_BYTES);
#endif

        regNumber srcRegNum = genMapRegArgNumToRegNum(paramRegIndex, storeType);

        // Stack argument - if the ref count is 0 don't care about it

        if (!lcl->lvOnFrame)
        {
            noway_assert(lcl->GetRefCount() == 0);
        }
        else
        {
            unsigned baseOffset = paramRegs[paramRegIndex].regIndex * slotSize;

            GetEmitter()->emitIns_S_R(ins_Store(storeType), size, srcRegNum, lclNum, baseOffset);

#ifndef UNIX_AMD64_ABI
            // TODO-MIKE-Cleanup: This should be valid on unix-x64 too.
            // Check if we are writing past the end of the struct
            if (varTypeIsStruct(lcl->GetType()))
            {
                assert(baseOffset + EA_SIZE_IN_BYTES(size) <= lcl->GetFrameSize());
            }
#endif

#ifdef USING_SCOPE_INFO
            if (paramRegs[paramRegIndex].slot == 1)
            {
                psiMoveToStack(lclNum);
            }
#endif
        }

        // Mark the argument as processed, and set it as no longer live in srcRegNum,
        // unless it is a writeThru var, in which case we home it to the stack, but
        // don't mark it as processed until below.

        if (!paramRegs[paramRegIndex].writeThru)
        {
            paramRegs[paramRegIndex].processed = true;
            liveParamRegs &= ~genRegMask(srcRegNum);

#ifdef TARGET_ARM
            if (storeType == TYP_DOUBLE)
            {
                paramRegs[paramRegIndex + 1].processed = true;
                liveParamRegs &= ~genRegMask(REG_NEXT(srcRegNum));
            }
#endif
        }
    }

    return liveParamRegs;
}

static regMaskTP genMapArgNumToRegMask(unsigned argNum, var_types type)
{
    if (varTypeUsesFloatArgReg(type))
    {
        regMaskTP result = genMapFloatRegArgNumToRegMask(argNum);

#ifdef TARGET_ARM
        if (type == TYP_DOUBLE)
        {
            assert((result & RBM_DBL_REGS) != 0);
            result |= (result << 1);
        }
#endif

        return result;
    }

    return genMapIntRegArgNumToRegMask(argNum);
}

void CodeGen::genPrologMoveParamRegs(ParamRegInfo* paramRegs,
                                     unsigned      paramRegCount,
                                     regMaskTP     liveParamRegs,
                                     bool          isFloat,
                                     regNumber     tempReg,
                                     bool*         tempRegClobbered)
{
    assert(liveParamRegs != RBM_NONE);

    // Process any circular dependencies

    instruction insCopy = INS_mov;

    if (isFloat)
    {
#ifndef UNIX_AMD64_ABI
        if (compiler->opts.UseHfa())
#endif
        {
            insCopy = ins_Copy(TYP_DOUBLE);

            // Compute tempReg here when we have a float argument
            assert(tempReg == REG_NA);

            regMaskTP fpAvailMask = RBM_FLT_CALLEE_TRASH & ~liveParamRegs;

            if (compiler->opts.UseHfa())
            {
                fpAvailMask &= RBM_ALLDOUBLE;
            }

            if (fpAvailMask == RBM_NONE)
            {
                fpAvailMask = RBM_ALLFLOAT & ~liveParamRegs;

                if (compiler->opts.UseHfa())
                {
                    fpAvailMask &= RBM_ALLDOUBLE;
                }
            }

            assert(fpAvailMask != RBM_NONE);

            // We pick the lowest avail register number
            tempReg = genRegNumFromMask(genFindLowestBit(fpAvailMask));
        }

#ifdef TARGET_X86
        // This case shouldn't occur on x86 since NYI gets converted to an assert
        NYI("Homing circular FP registers via xtraReg");
#endif
    }

    for (unsigned paramRegIndex = 0; paramRegIndex < paramRegCount; paramRegIndex++)
    {
        if (!paramRegs[paramRegIndex].circular || paramRegs[paramRegIndex].processed)
        {
            continue;
        }

        if (paramRegs[paramRegIndex].type == TYP_UNDEF) // Not a register argument
        {
            continue;
        }

        unsigned beginRegIndex = paramRegIndex;
        unsigned destRegIndex  = paramRegIndex;
        unsigned srcRegIndex   = paramRegs[paramRegIndex].trashBy;
        noway_assert(srcRegIndex < paramRegCount);

        unsigned   destLclNum = paramRegs[destRegIndex].lclNum;
        LclVarDsc* destLcl    = compiler->lvaGetDesc(destLclNum);
        unsigned   srcLclNum  = paramRegs[srcRegIndex].lclNum;
        LclVarDsc* srcLcl     = compiler->lvaGetDesc(srcLclNum);

#ifdef TARGET_XARCH
        if (destRegIndex == paramRegs[srcRegIndex].trashBy)
        {
            // Only 2 registers form the circular dependency - use "xchg".

            noway_assert(varTypeSize(varActualType(srcLcl->GetType())) <= REGSIZE_BYTES);
            noway_assert(destLcl->GetParamReg() == srcLcl->GetRegNum());

            // Set "size" to indicate GC if one and only one of the operands is a pointer.
            // RATIONALE: If both are pointers, nothing changes in the GC pointer tracking.
            // If only one is a pointer we have to "swap" the registers in the GC reg
            // pointer mask

            emitAttr size = EA_PTRSIZE;

            if (varTypeGCtype(srcLcl->GetType()) != varTypeGCtype(destLcl->GetType()))
            {
                size = EA_GCREF;
            }

            GetEmitter()->emitIns_R_R(INS_xchg, size, srcLcl->GetRegNum(), srcLcl->GetParamReg());

            paramRegs[destRegIndex].processed = true;
            paramRegs[srcRegIndex].processed  = true;

            liveParamRegs &= ~genRegMask(srcLcl->GetParamReg());
            liveParamRegs &= ~genRegMask(destLcl->GetParamReg());

#ifdef USING_SCOPE_INFO
            psiMoveToReg(srcLclNum);
            psiMoveToReg(destLclNum);
#endif
            continue;
        }
#endif // TARGET_XARCH

        var_types destMemType = destLcl->GetType();
        emitAttr  size        = EA_PTRSIZE;

#ifdef TARGET_ARM
        bool cycleAllDouble = true; // assume the best

        unsigned iter = beginRegIndex;
        do
        {
            if (!compiler->lvaGetDesc(paramRegs[iter].lclNum)->TypeIs(TYP_DOUBLE))
            {
                cycleAllDouble = false;
                break;
            }

            iter = paramRegs[iter].trashBy;
        } while (iter != beginRegIndex);

        // We may treat doubles as floats for ARM because we could have partial circular
        // dependencies of a float with a lo/hi part of the double. We mark the
        // trashBy values for each slot of the double, so let the circular dependency
        // logic work its way out for floats rather than doubles. If a cycle has all
        // doubles, then optimize so that instead of two vmov.f32's to move a double,
        // we can use one vmov.f64.

        if (!cycleAllDouble && destMemType == TYP_DOUBLE)
        {
            destMemType = TYP_FLOAT;
        }
#endif // TARGET_ARM

        if (destMemType == TYP_REF)
        {
            size = EA_GCREF;
        }
        else if (destMemType == TYP_BYREF)
        {
            size = EA_BYREF;
        }
        else if (destMemType == TYP_DOUBLE)
        {
            size = EA_8BYTE;
        }
        else if (destMemType == TYP_FLOAT)
        {
            size = EA_4BYTE;
        }

        // Move the dest reg to the extra reg

        assert(tempReg != REG_NA);

        regNumber begRegNum = genMapRegArgNumToRegNum(beginRegIndex, destMemType);
        GetEmitter()->emitIns_Mov(insCopy, size, tempReg, begRegNum, /* canSkip */ false);

        *tempRegClobbered = true;
#ifdef USING_SCOPE_INFO
        psiMoveToReg(destLclNum, tempReg);
#endif

        // Start moving everything to the right place.
        while (srcRegIndex != beginRegIndex)
        {
            // mov dest, src

            regNumber destRegNum = genMapRegArgNumToRegNum(destRegIndex, destMemType);
            regNumber srcRegNum  = genMapRegArgNumToRegNum(srcRegIndex, destMemType);

            GetEmitter()->emitIns_Mov(insCopy, size, destRegNum, srcRegNum, /* canSkip */ false);

            // Mark src as processed.
            noway_assert(srcRegIndex < paramRegCount);
            paramRegs[srcRegIndex].processed = true;

#ifdef TARGET_ARM
            if (size == EA_8BYTE)
            {
                paramRegs[srcRegIndex + 1].processed = true;
            }
#endif

            liveParamRegs &= ~genMapArgNumToRegMask(srcRegIndex, destMemType);

            // Move to the next pair.
            destRegIndex = srcRegIndex;
            srcRegIndex  = paramRegs[srcRegIndex].trashBy;

            destLcl     = srcLcl;
            destMemType = destLcl->GetType();

#ifdef TARGET_ARM
            if (!cycleAllDouble && (destMemType == TYP_DOUBLE))
            {
                destMemType = TYP_FLOAT;
            }
#endif

            srcLclNum = paramRegs[srcRegIndex].lclNum;
            srcLcl    = compiler->lvaGetDesc(srcLclNum);

            if (destMemType == TYP_REF)
            {
                size = EA_GCREF;
            }
            else if (destMemType == TYP_DOUBLE)
            {
                size = EA_8BYTE;
            }
            else
            {
                size = EA_4BYTE;
            }
        }

        // Take care of the first register.

        noway_assert(srcRegIndex == beginRegIndex);

        // move the dest reg (begReg) in the extra reg
        regNumber destRegNum = genMapRegArgNumToRegNum(destRegIndex, destMemType);
        GetEmitter()->emitIns_Mov(insCopy, size, destRegNum, tempReg, /* canSkip */ false);

#ifdef USING_SCOPE_INFO
        psiMoveToReg(srcLclNum);
#endif

        paramRegs[srcRegIndex].processed = true;
#ifdef TARGET_ARM
        if (size == EA_8BYTE)
        {
            paramRegs[srcRegIndex + 1].processed = true;
        }
#endif

        liveParamRegs &= ~genMapArgNumToRegMask(srcRegIndex, destMemType);
    }

    // Finally take care of the remaining arguments that must be enregistered.
    while (liveParamRegs != RBM_NONE)
    {
        regMaskTP liveParamRegsBefore = liveParamRegs;

        for (unsigned paramRegIndex = 0; paramRegIndex < paramRegCount; paramRegIndex++)
        {
            if (paramRegs[paramRegIndex].processed)
            {
                continue;
            }

            if (paramRegs[paramRegIndex].type == TYP_UNDEF) // Not a register argument
            {
                continue;
            }

            unsigned   lclNum = paramRegs[paramRegIndex].lclNum;
            LclVarDsc* lcl    = compiler->lvaGetDesc(lclNum);

            noway_assert(lcl->lvIsInReg() && !paramRegs[paramRegIndex].circular);
#ifdef TARGET_X86
            // On x86 we don't enregister args that are not pointer sized.
            noway_assert(varTypeSize(lcl->GetActualRegisterType()) == REGSIZE_BYTES);
#endif

            const var_types lclRegType = lcl->GetRegisterType();
            const var_types regType    = paramRegs[paramRegIndex].type;
            const regNumber regNum     = genMapRegArgNumToRegNum(paramRegIndex, regType);

            // Register argument - hopefully it stays in the same register.
            regNumber destRegNum  = REG_NA;
            var_types destMemType = lcl->GetRegisterType();

            if (paramRegs[paramRegIndex].regIndex == 0)
            {
                destRegNum = lcl->GetRegNum();

#ifdef TARGET_ARM
                if ((destMemType == TYP_DOUBLE) && paramRegs[paramRegIndex + 1].processed)
                {
                    // The second half of the double has already been processed! Treat this as a single.
                    destMemType = TYP_FLOAT;
                }
#endif
            }
#ifdef TARGET_ARM
            else if ((paramRegs[paramRegIndex].regIndex == 1) && (destMemType == TYP_LONG))
            {
                assert(lcl->TypeIs(TYP_DOUBLE, TYP_LONG));

                if (lcl->TypeIs(TYP_DOUBLE))
                {
                    destRegNum = regNum;
                }
                else
                {
                    // TODO-MIKE-Cleanup: This is likely dead code.
                    destRegNum = REG_STK;
                }

                assert(destRegNum != REG_STK);
            }
            else
            {
                assert(paramRegs[paramRegIndex].regIndex == 1);
                assert(destMemType == TYP_DOUBLE);

                // For doubles, we move the entire double using the paramRegIndex representing
                // the first half of the double. There are two things we won't do:
                // (1) move the double when the 1st half of the destination is free but the
                // 2nd half is occupied, and (2) move the double when the 2nd half of the
                // destination is free but the 1st half is occupied. Here we consider the
                // case where the first half can't be moved initially because its target is
                // still busy, but the second half can be moved. We wait until the entire
                // double can be moved, if possible. For example, we have F0/F1 double moving to F2/F3,
                // and F2 single moving to F16. When we process F0, its target F2 is busy,
                // so we skip it on the first pass. When we process F1, its target F3 is
                // available. However, we want to move F0/F1 all at once, so we skip it here.
                // We process F2, which frees up F2. The next pass through, we process F0 and
                // F2/F3 are empty, so we move it. Note that if half of a double is involved
                // in a circularity with a single, then we will have already moved that half
                // above, so we go ahead and move the remaining half as a single.
                // Because there are no circularities left, we are guaranteed to terminate.

                assert(paramRegIndex > 0);
                assert(paramRegs[paramRegIndex - 1].regIndex == 0);

                if (!paramRegs[paramRegIndex - 1].processed)
                {
                    // The first half of the double hasn't been processed; try to be processed at the same time
                    continue;
                }

                // The first half of the double has been processed but the second half hasn't!
                // This could happen for double F2/F3 moving to F0/F1, and single F0 moving to F2.
                // In that case, there is a F0/F2 loop that is not a double-only loop. The circular
                // dependency logic above will move them as singles, leaving just F3 to move. Treat
                // it as a single to finish the shuffling.

                destMemType = TYP_FLOAT;
                destRegNum  = REG_NEXT(lcl->GetRegNum());
            }
#endif // !TARGET_ARM
#if defined(UNIX_AMD64_ABI) || defined(TARGET_ARM64)
            else
            {
                assert(paramRegs[paramRegIndex].regIndex == 1);
                assert(paramRegIndex > 0);
                assert(paramRegs[paramRegIndex - 1].regIndex == 0);
                assert((lclRegType == TYP_SIMD12) || (lclRegType == TYP_SIMD16));

                destRegNum = lcl->GetRegNum();

                noway_assert(regNum != destRegNum);

                continue;
            }
#endif // defined(UNIX_AMD64_ABI) || defined(TARGET_ARM64)

            noway_assert(destRegNum != REG_NA);

            if (destRegNum != regNum)
            {
                // Cannot trash a currently live register argument.
                // Skip this one until its target will be free
                // which is guaranteed to happen since we have no circular dependencies.

                regMaskTP destMask = genRegMask(destRegNum);

#ifdef TARGET_ARM
                // Don't process the double until both halves of the destination are clear.
                if (destMemType == TYP_DOUBLE)
                {
                    assert((destMask & RBM_DBL_REGS) != 0);
                    destMask |= genRegMask(REG_NEXT(destRegNum));
                }
#endif

                if ((destMask & liveParamRegs) != RBM_NONE)
                {
                    continue;
                }

                // Move it to the new register

                emitAttr size = emitActualTypeSize(destMemType);

#ifdef TARGET_ARM64
                if (varTypeIsSIMD(lcl->GetType()) && (paramRegIndex < paramRegCount - 1) &&
                    (paramRegs[paramRegIndex + 1].regIndex == 1))
                {
                    // For a SIMD type that is passed in two integer registers,
                    // Limit the copy below to the first 8 bytes from the first integer register.
                    // Handle the remaining 8 bytes from the second slot in the code further below
                    assert(EA_SIZE(size) >= 8);

                    size = EA_8BYTE;
                }
#endif

                inst_Mov(destMemType, destRegNum, regNum, /* canSkip */ false, size);

#ifdef USING_SCOPE_INFO
                psiMoveToReg(lclNum);
#endif
            }

            paramRegs[paramRegIndex].processed = true;
            liveParamRegs &= ~genRegMask(regNum);

#if FEATURE_MULTIREG_ARGS
            unsigned regCount = 1;

#ifdef UNIX_AMD64_ABI
            if (varTypeIsStruct(lcl->GetType()) && (paramRegIndex < paramRegCount - 1) &&
                (paramRegs[paramRegIndex + 1].regIndex == 1))
            {
                regCount = 2;

                regNumber nextRegNum = genMapRegArgNumToRegNum(paramRegIndex + 1, paramRegs[paramRegIndex + 1].type);
                noway_assert(paramRegs[paramRegIndex + 1].lclNum == lclNum);

                if (lcl->TypeIs(TYP_SIMD12) && compiler->compOpportunisticallyDependsOn(InstructionSet_SSE41))
                {
                    GetEmitter()->emitIns_R_R_I(INS_insertps, EA_16BYTE, destRegNum, nextRegNum, 0x28);
                }
                else
                {
                    if (lcl->TypeIs(TYP_SIMD12))
                    {
                        // Zero out the upper element of Vector3 since unmanaged callers
                        // don't do it (the native ABI doesn't require it).
                        GetEmitter()->emitIns_R_I(INS_pslldq, EA_16BYTE, nextRegNum, 12);
                        GetEmitter()->emitIns_R_I(INS_psrldq, EA_16BYTE, nextRegNum, 12);
                    }

                    GetEmitter()->emitIns_R_R(INS_movlhps, EA_16BYTE, destRegNum, nextRegNum);
                }

                // Set destRegNum to regNum so that we skip the setting of the register below,
                // but mark paramRegIndex as processed and clear regNum from the live mask.
                destRegNum = regNum;
            }
#endif // UNIX_AMD64_ABI

#ifdef TARGET_ARMARCH
#ifdef TARGET_ARM
            if (destMemType == TYP_DOUBLE)
            {
                regCount = 2;
            }
#endif

            if (lcl->IsHfaRegParam())
            {
                // This includes both fixed-size SIMD types that are independently promoted, as well
                // as other HFA structs.
                regCount = lcl->GetLayout()->GetHfaRegCount();

                if (paramRegIndex < paramRegCount - regCount + 1)
                {
                    if (lcl->IsIndependentPromoted())
                    {
                        // For an HFA type that is passed in multiple registers and promoted, we copy each field to its
                        // destination register.
                        for (unsigned i = 0; i < regCount; i++)
                        {
                            destRegNum = compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(i))->GetRegNum();

                            ParamRegInfo& nextParamReg = paramRegs[paramRegIndex + i];
                            regNumber     nextRegNum   = genMapRegArgNumToRegNum(paramRegIndex + i, nextParamReg.type);

                            noway_assert(nextParamReg.lclNum == lclNum);
                            noway_assert(genIsValidFloatReg(nextRegNum));
                            noway_assert(genIsValidFloatReg(destRegNum));

                            GetEmitter()->emitIns_Mov(INS_mov, EA_8BYTE, destRegNum, nextRegNum, /* canSkip */ false);
                        }
                    }
#ifdef TARGET_ARM64
                    else
                    {
                        // For a SIMD type that is passed in multiple registers but enregistered as a vector,
                        // the code above copies the first argument register into the lower 4 or 8 bytes
                        // of the target register. Here we must handle the subsequent fields by
                        // inserting them into the upper bytes of the target SIMD floating point register.

                        for (unsigned i = 1; i < regCount; i++)
                        {
                            ParamRegInfo& nextParamReg = paramRegs[paramRegIndex + i];
                            regNumber     nextRegNum   = genMapRegArgNumToRegNum(paramRegIndex + i, nextParamReg.type);

                            noway_assert(nextParamReg.lclNum == lclNum);
                            noway_assert(genIsValidFloatReg(nextRegNum));
                            noway_assert(genIsValidFloatReg(destRegNum));

                            GetEmitter()->emitIns_R_R_I_I(INS_mov, EA_4BYTE, destRegNum, nextRegNum, i, 0);
                        }
                    }
#endif // TARGET_ARM64
                }
            }
#endif // TARGET_ARMARCH

            // Mark the rest of the argument registers corresponding to this multi-reg type as
            // being processed and no longer live.
            for (unsigned regSlot = 1; regSlot < regCount; regSlot++)
            {
                ParamRegInfo& nextParamReg = paramRegs[paramRegIndex + regSlot];

                assert(!nextParamReg.processed);

                nextParamReg.processed = true;

                regNumber nextRegNum = genMapRegArgNumToRegNum(paramRegIndex + regSlot, nextParamReg.type);
                liveParamRegs &= ~genRegMask(nextRegNum);
            }
#endif // FEATURE_MULTIREG_ARGS
        }

        noway_assert(liveParamRegsBefore != liveParamRegs); // if it doesn't change, we have an infinite loop
    }
}

void CodeGen::genPrologEnregisterIncomingStackParams()
{
    JITDUMP("*************** In genPrologEnregisterIncomingStackParams()\n");

    assert(!compiler->opts.IsOSR());
    assert(generatingProlog);

    for (unsigned lclNum = 0; lclNum < compiler->lvaCount; lclNum++)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

        if (!lcl->IsParam())
        {
            continue;
        }

        // If it's a register argument then it's already been taken care of.
        // But, on ARM when under a profiler, we would have prespilled a register
        // parameter and hence here we need to load it from its prespilled location.
        bool isPrespilledForProfiling = false;
#if defined(TARGET_ARM) && defined(PROFILING_SUPPORTED)
        isPrespilledForProfiling = compiler->compIsProfilerHookNeeded() && lcl->IsPreSpilledRegParam(preSpillParamRegs);
#endif

        if (lcl->IsRegParam() && !isPrespilledForProfiling)
        {
            continue;
        }

        if (!lcl->lvIsInReg())
        {
            continue;
        }

        if (!VarSetOps::IsMember(compiler, compiler->fgFirstBB->bbLiveIn, lcl->GetLivenessBitIndex()))
        {
            continue;
        }

        regNumber regNum = lcl->GetParamInitialReg();
        assert(regNum != REG_STK);
        var_types regType = lcl->GetActualRegisterType();

        GetEmitter()->emitIns_R_S(ins_Load(regType), emitTypeSize(regType), regNum, lclNum, 0);

#ifdef USING_SCOPE_INFO
        psiMoveToReg(lclNum);
#endif
    }
}

void CodeGen::MarkStackLocals()
{
    for (unsigned lclNum = 0; lclNum < compiler->lvaCount; lclNum++)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

        lcl->lvFramePointerBased = isFramePointerUsed();

#if DOUBLE_ALIGN
        if (doDoubleAlign())
        {
            noway_assert(!isFramePointerUsed());

            if (lcl->IsParam() && !lcl->IsRegParam())
            {
                lcl->lvFramePointerBased = true;
            }
        }
#endif

        // X86 varargs methods must not contain direct references to parameters
        // other than 'this', the arglist parameter (which is not a GC pointer)
        // and the struct return buffer parameter, if present. We cannot report
        // any other parameters to the GC becaue they do not have correct frame
        // offsets.
        if (compiler->lvaIsX86VarargsStackParam(lclNum))
        {
            assert((lcl->GetRefCount() == 0) && !lcl->lvRegister);

            lcl->lvOnFrame  = false;
            lcl->lvMustInit = false;

            continue;
        }

        if (lcl->IsDependentPromotedField(compiler))
        {
            noway_assert(!lcl->lvRegister);

            lcl->lvOnFrame = true;

            continue;
        }

        if (lcl->GetRefCount() == 0)
        {
            // Unreferenced locals will get a frame location if they're address exposed.
            // TODO-MIKE-Review: Why? Probably because AX is sometimes used simply to
            // block optimizations and require frame allocation. Sounds like "implicitly
            // referenced" should be used instead.

            assert(!compiler->opts.compDbgCode);
            assert(!lcl->lvRegister);
#if FEATURE_FIXED_OUT_ARGS
            // lvaOutgoingArgSpaceVar is implicitly referenced.
            assert(lclNum != compiler->lvaOutgoingArgSpaceVar);
#endif

            if (lcl->IsAddressExposed())
            {
                lcl->lvOnFrame = true;
            }
            else
            {
                lcl->lvOnFrame  = false;
                lcl->lvMustInit = false;
            }

            continue;
        }

        // It must be in a register, on frame, or have zero references.
        noway_assert(lcl->lvIsInReg() || lcl->lvOnFrame || (lcl->GetRefCount() == 0));
        // We can't have both lvRegister and lvOnFrame
        noway_assert(!lcl->lvRegister || !lcl->lvOnFrame);

        if (varTypeIsGC(lcl->GetType()) && lcl->lvTracked && (!lcl->IsParam() || lcl->IsRegParam()))
        {
            lcl->SetHasGCLiveness();
        }
    }
}

// We have to decide whether we're going to use block initialization in
// the prolog before we assign final stack offsets because when we may
// need additional callee-saved registers which need to be saved on the
// frame, thus increasing the frame size.
//
// We'll count the number of locals we have to initialize, and if there
// are lots of them we'll use block initialization. Thus, the local
// variable table must have accurate register location information for
// enregistered locals for their register state on entry to the function.
//
// At the same time we set lvMustInit for locals (enregistered or on stack)
// that must be initialized (e.g. initialize memory (compInitMem), untracked
// pointers or disable DFA)
void CodeGen::CheckUseBlockInit()
{
    assert(!generatingProlog);

    const bool compInitMem = compiler->info.compInitMem;
    // The number of int-sized stack slots that need to be initialized.
    unsigned slotCount = 0;

    for (unsigned lclNum = 0; lclNum < compiler->lvaCount; lclNum++)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

        if (!lcl->lvIsInReg() && !lcl->lvOnFrame)
        {
            noway_assert(lcl->GetRefCount() == 0);
            continue;
        }

        // Initialization of OSR locals must be handled specially
        if (compiler->lvaIsOSRLocal(lclNum))
        {
            lcl->lvMustInit = false;
            continue;
        }

        if (compiler->lvaIsNeverZeroInitializedInProlog(lclNum))
        {
            continue;
        }

        if (lcl->IsDependentPromotedField(compiler))
        {
            // For dependent promotion, the whole struct should have been initialized
            // by the parent struct. No need to set the lvMustInit bit in the fields.
            continue;
        }

        if (lcl->lvHasExplicitInit)
        {
            lcl->lvMustInit = false;
            continue;
        }

        const bool isTemp   = lcl->lvIsTemp;
        const bool hasGCPtr = lcl->HasGCPtr();

        if (isTemp && !hasGCPtr)
        {
            lcl->lvMustInit = false;
            continue;
        }

        if (!compInitMem && !hasGCPtr && !lcl->lvMustInit)
        {
            continue;
        }

        const bool isTracked = lcl->lvTracked;
        bool       blockInit = false;

        if (isTracked && (lcl->lvMustInit ||
                          VarSetOps::IsMember(compiler, compiler->fgFirstBB->bbLiveIn, lcl->GetLivenessBitIndex())))
        {
            if (!lcl->lvOnFrame || (lcl->lvIsInReg() && !lcl->lvLiveInOutOfHndlr))
            {
                // We only need to init the register the local is in.
                lcl->lvMustInit = true;

                continue;
            }

            blockInit = true;
        }
        else if (lcl->lvOnFrame)
        {
            if (hasGCPtr && !isTracked)
            {
                JITDUMP("must init V%02u because it has a GC ref\n", lclNum);

                blockInit = true;
            }
            else if (hasGCPtr && varTypeIsStruct(lcl->GetType()))
            {
                // TODO-1stClassStructs: support precise liveness reporting for such structs.
                JITDUMP("must init a tracked V%02u because it a struct with a GC ref\n", lclNum);

                blockInit = true;
            }
            else if (!isTracked)
            {
                assert(!hasGCPtr && !isTemp);

                if (compInitMem)
                {
                    JITDUMP("must init V%02u because compInitMem is set and it is not a temp\n", lclNum);

                    blockInit = true;
                }
            }
        }

        if (blockInit)
        {
            lcl->lvMustInit = true;
            slotCount += roundUp(lcl->GetFrameSize(), REGSIZE_BYTES) / 4;
        }
    }

    if (!spillTemps.TrackGCSpillTemps())
    {
        for (SpillTemp& temp : spillTemps)
        {
            if (varTypeIsGC(temp.GetType()))
            {
                slotCount++;
            }
        }
    }

    // Decide if we will do block initialization in the prolog, or use
    // a series of individual stores.
    //
    // Primary factor is the number of slots that need zeroing. We've
    // been counting by 4 byte slots above. We assume for now we can
    // only zero register width bytes per store.
    //
    // Current heuristic is to use block init when more than 4 stores
    // are required.
    //
    // TODO: Consider taking into account the presence of large structs
    // that potentially only need some fields set to zero.
    //
    // Compiler::fgVarNeedsExplicitZeroInit relies on this logic to
    // find structs that are guaranteed to be block initialized.
    // If this logic changes, Compiler::fgVarNeedsExplicitZeroInit needs
    // to be modified.
    CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef TARGET_AMD64
    // We can clear using aligned XMM stores so the threshold is lower.
    genUseBlockInit = slotCount > 4;
#elif defined(TARGET_64BIT)
    genUseBlockInit = slotCount > 8;
#else
    genUseBlockInit = slotCount > 4;
#endif

    genInitStkLclCnt = slotCount;

    JITDUMP("Found %u MustInit int-sized stack slots, %susing block init\n", slotCount, genUseBlockInit ? "" : "not ");
}

// Record the stack frame ranges that will cover all of the GC tracked
// and untracked pointer variables.
// Also find which registers will need to be zero-initialized. This
// sometimes happens in IL_STUBs that we generate or VB code, they
// rely on .localsinit rather than explicit initialization of locals.
void CodeGen::MarkGCTrackedSlots(int&       minBlockInitOffset,
                                 int&       maxBlockInitOffset,
                                 regMaskTP& initRegs ARM_ARG(regMaskTP& initDblRegs))
{
    minBlockInitOffset = INT_MAX;
    maxBlockInitOffset = INT_MIN;
    initRegs           = RBM_NONE;
    ARM_ONLY(initDblRegs = RBM_NONE);

    int minGCTrackedOffset = INT_MAX;
    int maxGCTrackedOffset = INT_MIN;

    for (unsigned lclNum = 0; lclNum < compiler->lvaCount; lclNum++)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

        if (lcl->IsParam() && !lcl->IsRegParam())
        {
            continue;
        }

        if (!lcl->lvIsInReg() && !lcl->lvOnFrame)
        {
            noway_assert(lcl->GetRefCount() == 0);
            continue;
        }

        int offset = lcl->GetStackOffset();

        if (lcl->HasGCSlotLiveness())
        {
            minGCTrackedOffset = Min(minGCTrackedOffset, offset);
            maxGCTrackedOffset = Max(maxGCTrackedOffset, offset);
        }

        if (!lcl->lvMustInit)
        {
            continue;
        }

        bool isInReg    = lcl->lvIsInReg();
        bool isInMemory = !isInReg || lcl->lvLiveInOutOfHndlr;

        if (isInMemory)
        {
            minBlockInitOffset = Min(minBlockInitOffset, offset);
            maxBlockInitOffset = Max(maxBlockInitOffset, offset + static_cast<int>(lcl->GetFrameSize()));
        }

        if (!isInReg)
        {
            continue;
        }

        // Note that lvIsInReg will only be accurate for variables that are actually live-in to
        // the first block. This will include all possibly-uninitialized locals, whose liveness
        // will naturally propagate up to the entry block. However, we also set lvMustInit for
        // locals that are live-in to a finally block, and those may not be live-in to the first
        // block. For those, we don't want to initialize the register, as it will not actually be
        // occupying it on entry.
        // TODO-MIKE-Review: So why would lvIsInReg be true for such variables?

        if (!VarSetOps::IsMember(compiler, compiler->fgFirstBB->bbLiveIn, lcl->GetLivenessBitIndex()))
        {
            assert(compiler->lvaEnregEHVars && lcl->lvLiveInOutOfHndlr);

            continue;
        }

        assert(!lcl->TypeIs(TYP_STRUCT));
#ifndef TARGET_64BIT
        assert(!lcl->TypeIs(TYP_LONG));
#endif

#ifdef TARGET_ARM
        if (lcl->TypeIs(TYP_DOUBLE))
        {
            initDblRegs |= genRegMaskFloat(lcl->GetRegNum());
        }
        else
#endif
        {
            initRegs |= genRegMask(lcl->GetRegNum());
        }
    }

    int minGCSpillTempOffset = INT_MAX;
    int maxGCSpillTempOffset = INT_MIN;

    for (SpillTemp& temp : spillTemps)
    {
        if (!varTypeIsGC(temp.GetType()))
        {
            continue;
        }

        int offset = temp.GetOffset();

        minGCSpillTempOffset = Min(minGCSpillTempOffset, offset);
        maxGCSpillTempOffset = Max(maxGCSpillTempOffset, offset + REGSIZE_BYTES);
    }

    if (spillTemps.TrackGCSpillTemps())
    {
        minGCTrackedOffset = Min(minGCTrackedOffset, minGCSpillTempOffset);
        maxGCTrackedOffset = Max(maxGCTrackedOffset, maxGCSpillTempOffset);
    }
    else
    {
        minBlockInitOffset = Min(minBlockInitOffset, minGCSpillTempOffset);
        maxBlockInitOffset = Max(maxBlockInitOffset, maxGCSpillTempOffset);
    }

    // TODO-Cleanup: Add suitable assert for the OSR case.
    assert(compiler->opts.IsOSR() || ((genInitStkLclCnt > 0) == (maxBlockInitOffset != INT_MIN)));

    if (genUseBlockInit)
    {
        JITDUMP("Block init slot offsets in [%d..%d)\n", minBlockInitOffset, maxBlockInitOffset + REGSIZE_BYTES);
    }

    if (maxGCTrackedOffset != INT_MIN)
    {
        JITDUMP("%u tracked GC refs in frame range ", (maxGCTrackedOffset - minGCTrackedOffset) / REGSIZE_BYTES + 1);
#ifdef TARGET_ARMARCH
        JITDUMP("[%s,#%d] - [%s,#%d]\n", GetEmitter()->emitGetFrameReg(), minGCTrackedOffset,
                GetEmitter()->emitGetFrameReg(), maxGCTrackedOffset);
#else
        JITDUMP("[%s%c%02XH] - [%s%c%02XH]\n", GetEmitter()->emitGetFrameReg(), minGCTrackedOffset < 0 ? '-' : '+',
                abs(minGCTrackedOffset), GetEmitter()->emitGetFrameReg(), maxGCTrackedOffset < 0 ? '-' : '+',
                abs(maxGCTrackedOffset));
#endif

        GetEmitter()->gcInfo.SetTrackedStackSlotRange(minGCTrackedOffset, maxGCTrackedOffset + REGSIZE_BYTES);
    }
    else
    {
        JITDUMP("No tracked GC refs\n");
    }
}

void CodeGen::PrologZeroInitUntrackedLocals(regNumber initReg, bool* initRegZeroed)
{
    assert(genInitStkLclCnt > 0);
    // initReg is not a live incoming param reg
    assert((genRegMask(initReg) & paramRegState.intRegLiveIn) == RBM_NONE);

#ifdef TARGET_ARM64
    auto GetZeroReg = []() { return REG_ZR; };
#else
    auto GetZeroReg = [this, initReg, initRegZeroed]() {
        if (!*initRegZeroed)
        {
            instGen_Set_Reg_To_Zero(EA_PTRSIZE, initReg);
            *initRegZeroed = true;
        }

        return initReg;
    };
#endif

    LclVarDsc* varDsc;
    unsigned   varNum;

    for (varNum = 0, varDsc = compiler->lvaTable; varNum < compiler->lvaCount; varNum++, varDsc++)
    {
        if (!varDsc->lvMustInit)
        {
            continue;
        }

        // TODO-Review: I'm not sure that we're correctly handling the mustInit case for
        // partially-enregistered vars in the case where we don't use a block init.
        noway_assert(varDsc->lvIsInReg() || varDsc->lvOnFrame);

        // lvMustInit can only be set for GC types or TYP_STRUCT types
        // or when compInitMem is true
        // or when in debug code

        noway_assert(varTypeIsGC(varDsc->TypeGet()) || (varDsc->TypeGet() == TYP_STRUCT) ||
                     compiler->info.compInitMem || compiler->opts.compDbgCode);

        if (!varDsc->lvOnFrame)
        {
            continue;
        }

        if (varDsc->TypeIs(TYP_STRUCT) && !compiler->info.compInitMem && varDsc->HasGCPtr())
        {
            // We only initialize the GC variables in the TYP_STRUCT
            ClassLayout* layout = varDsc->GetLayout();

            for (unsigned i = 0; i < layout->GetSlotCount(); i++)
            {
                if (layout->IsGCPtr(i))
                {
                    GetEmitter()->emitIns_S_R(ins_Store(TYP_I_IMPL), EA_PTRSIZE, GetZeroReg(), varNum,
                                              i * REGSIZE_BYTES);
                }
            }
        }
        else
        {
            regNumber zeroReg = GetZeroReg();

            // zero out the whole thing rounded up to a single stack slot size
            unsigned lclSize = roundUp(varDsc->GetFrameSize(), 4);
            unsigned i;
            for (i = 0; i + REGSIZE_BYTES <= lclSize; i += REGSIZE_BYTES)
            {
                GetEmitter()->emitIns_S_R(ins_Store(TYP_I_IMPL), EA_PTRSIZE, zeroReg, varNum, i);
            }

#ifdef TARGET_64BIT
            assert(i == lclSize || (i + sizeof(int) == lclSize));
            if (i != lclSize)
            {
                GetEmitter()->emitIns_S_R(ins_Store(TYP_INT), EA_4BYTE, zeroReg, varNum, i);
                i += sizeof(int);
            }
#endif // TARGET_64BIT
            assert(i == lclSize);
        }
    }

    if (!spillTemps.TrackGCSpillTemps())
    {
        for (SpillTemp& temp : spillTemps)
        {
            if (!varTypeIsGC(temp.GetType()))
            {
                continue;
            }

            GetEmitter()->emitIns_S_R(ins_Store(TYP_I_IMPL), EA_PTRSIZE, GetZeroReg(), temp.GetNum(), 0);
        }
    }
}

void CodeGen::PrologInitOsrLocals()
{
    // Initialize args and locals for OSR. Note this may include promoted fields.
    if (compiler->opts.IsOSR())
    {
        PatchpointInfo* patchpointInfo = compiler->info.compPatchpointInfo;

        // basic sanity checks (make sure we're OSRing the right method)
        assert(patchpointInfo->NumberOfLocals() == compiler->info.compLocalsCount);

        const int      originalFrameSize = patchpointInfo->FpToSpDelta();
        const unsigned patchpointInfoLen = patchpointInfo->NumberOfLocals();

        for (unsigned varNum = 0; varNum < compiler->lvaCount; varNum++)
        {
            if (!compiler->lvaIsOSRLocal(varNum))
            {
                continue;
            }

            LclVarDsc* const varDsc = compiler->lvaGetDesc(varNum);

            if (!varDsc->lvIsInReg())
            {
                JITDUMP("---OSR--- V%02u in memory\n", varNum);
                continue;
            }

            if (!VarSetOps::IsMember(compiler, compiler->fgFirstBB->bbLiveIn, varDsc->lvVarIndex))
            {
                JITDUMP("---OSR--- V%02u (reg) not live at entry\n", varNum);
                continue;
            }

            int      fieldOffset = 0;
            unsigned lclNum      = varNum;

            if (varDsc->lvIsStructField)
            {
                lclNum = varDsc->lvParentLcl;
                assert(lclNum < patchpointInfoLen);

                fieldOffset = varDsc->lvFldOffset;
                JITDUMP("---OSR--- V%02u is promoted field of V%02u at offset %d\n", varNum, lclNum, fieldOffset);
            }

            // Note we are always reading from the original frame here
            const var_types lclTyp  = varActualType(varDsc->GetType());
            const emitAttr  size    = emitTypeSize(lclTyp);
            const int       stkOffs = patchpointInfo->Offset(lclNum) + fieldOffset;

            // Original frames always use frame pointers, so
            // stkOffs is the original frame-relative offset
            // to the variable.
            //
            // We need to determine the stack or frame-pointer relative
            // offset for this variable in the current frame.
            //
            // If current frame does not use a frame pointer, we need to
            // add the SP-to-FP delta of this frame and the SP-to-FP delta
            // of the original frame; that translates from this frame's
            // stack pointer the old frame frame pointer.
            //
            // We then add the original frame's frame-pointer relative
            // offset (note this offset is usually negative -- the stack
            // grows down, so locals are below the frame pointer).
            //
            // /-----original frame-----/
            // / return address         /
            // / saved RBP   --+        /  <--- Original frame ptr   --+
            // / ...           |        /                              |
            // / ...       (stkOffs)    /                              |
            // / ...           |        /                              |
            // / variable    --+        /                              |
            // / ...                    /                (original frame sp-fp delta)
            // / ...                    /                              |
            // /-----OSR frame ---------/                              |
            // / pseudo return address  /                            --+
            // / ...                    /                              |
            // / ...                    /                    (this frame sp-fp delta)
            // / ...                    /                              |
            // /------------------------/  <--- Stack ptr            --+
            //
            // If the current frame is using a frame pointer, we need to
            // add the SP-to-FP delta of/ the original frame and then add
            // the original frame's frame-pointer relative offset.
            //
            // /-----original frame-----/
            // / return address         /
            // / saved RBP   --+        /  <--- Original frame ptr   --+
            // / ...           |        /                              |
            // / ...       (stkOffs)    /                              |
            // / ...           |        /                              |
            // / variable    --+        /                              |
            // / ...                    /                (original frame sp-fp delta)
            // / ...                    /                              |
            // /-----OSR frame ---------/                              |
            // / pseudo return address  /                            --+
            // / saved RBP              /  <--- Frame ptr            --+
            // / ...                    /
            // / ...                    /
            // / ...                    /
            // /------------------------/

            int offset = originalFrameSize + stkOffs;

            if (isFramePointerUsed())
            {
                // also adjust for saved RPB on this frame
                offset += TARGET_POINTER_SIZE;
            }
            else
            {
                offset += genSPtoFPdelta();
            }

            JITDUMP("---OSR--- V%02u (reg) old rbp offset %d old frame %d this frame sp-fp %d new offset %d (%02xH)\n",
                    varNum, stkOffs, originalFrameSize, genSPtoFPdelta(), offset, offset);

            // TODO-MIKE-Review: Is this called on ARM64? emitIns_R_AR is not implemented...
            GetEmitter()->emitIns_R_AR(ins_Load(lclTyp), size, varDsc->GetRegNum(), genFramePointerReg(), offset);
        }
    }
}

/*-----------------------------------------------------------------------------
 *
 *  Save the generic context argument.
 *
 *  We need to do this within the "prolog" in case anyone tries to inspect
 *  the param-type-arg/this (which can be done after the prolog) using
 *  ICodeManager::GetParamTypeArg().
 */

void CodeGen::PrologReportGenericContextArg(regNumber initReg, bool* pInitRegZeroed)
{
    // For OSR the original method has set this up for us.
    assert(!compiler->opts.IsOSR());

    bool reportArg = compiler->lvaReportParamTypeArg();

    // We should report either generic context arg or "this" when used so.
    if (!reportArg)
    {
#ifndef JIT32_GCENCODER
        if (!compiler->lvaKeepAliveAndReportThis())
#endif
        {
            return;
        }
    }

    // For JIT32_GCENCODER, we won't be here if reportArg is false.
    unsigned contextArg = reportArg ? compiler->info.compTypeCtxtArg : compiler->info.compThisArg;

    noway_assert(contextArg != BAD_VAR_NUM);
    LclVarDsc* varDsc = compiler->lvaGetDesc(contextArg);

    // We are still in the prolog and compiler->info.compTypeCtxtArg has not been
    // moved to its final home location. So we need to use it from the
    // incoming location.

    regNumber reg;

    bool isPrespilledForProfiling = false;
#if defined(TARGET_ARM) && defined(PROFILING_SUPPORTED)
    isPrespilledForProfiling = compiler->compIsProfilerHookNeeded() && varDsc->IsPreSpilledRegParam(preSpillParamRegs);
#endif

    // Load from the argument register only if it is not prespilled.
    if (varDsc->IsRegParam() && !isPrespilledForProfiling)
    {
        reg = varDsc->GetParamReg();
    }
    else
    {
#if !defined(TARGET_X86) && !defined(TARGET_ARM)
        // On most targets we have enough param regs that the type context param
        // is always passed in a register.
        unreached();
#else
        if (isFramePointerUsed())
        {
#ifdef TARGET_ARM
            // On ARM both `this` and TypeCtxtArg are always reg params but due
            // to pre-spilling done for profiler we need to load from the stack,
            // the offset should be in the pre-spill param area, right above FP
            // and LR.
            noway_assert((2 * REGSIZE_BYTES <= varDsc->GetStackOffset()) &&
                         (size_t(varDsc->GetStackOffset()) < GetPreSpillSize() + 2 * REGSIZE_BYTES));
#else
            // It cannot be `this` since that's passed in a register so it has to be TypeCtxtArg
            // which is always the last parameter (and we know that the frame pointer is also
            // pushed, in addition to the return address).
            noway_assert(varDsc->GetStackOffset() == 2 * REGSIZE_BYTES);
#endif
        }

        // We will just use the initReg since it is an available register
        // and we are probably done using it anyway...
        reg             = initReg;
        *pInitRegZeroed = false;

        // mov reg, [compiler->info.compTypeCtxtArg]
        GetEmitter()->emitIns_R_AR(ins_Load(TYP_I_IMPL), EA_PTRSIZE, reg, genFramePointerReg(),
                                   varDsc->GetStackOffset());
#endif // defined(TARGET_X86) || defined(TARGET_ARM)
    }

#if defined(TARGET_ARM64)
    genInstrWithConstant(ins_Store(TYP_I_IMPL), EA_PTRSIZE, reg, genFramePointerReg(), cachedGenericContextArgOffset,
                         rsGetRsvdReg());
#elif defined(TARGET_ARM)
    // ARM's emitIns_R_R_I automatically uses the reserved register if necessary.
    GetEmitter()->emitIns_R_R_I(ins_Store(TYP_I_IMPL), EA_PTRSIZE, reg, genFramePointerReg(),
                                cachedGenericContextArgOffset);
#else  // !ARM64 !ARM
    // mov [ebp-lvaCachedGenericContextArgOffset()], reg
    GetEmitter()->emitIns_AR_R(ins_Store(TYP_I_IMPL), EA_PTRSIZE, reg, genFramePointerReg(),
                               cachedGenericContextArgOffset);
#endif // !ARM64 !ARM
}

/*****************************************************************************

Esp frames :
----------

These instructions are just a reordering of the instructions used today.

push ebp
push esi
push edi
push ebx
sub esp, LOCALS_SIZE / push dummyReg if LOCALS_SIZE=sizeof(void*)
...
add esp, LOCALS_SIZE / pop dummyReg
pop ebx
pop edi
pop esi
pop ebp
ret

Ebp frames :
----------

The epilog does "add esp, LOCALS_SIZE" instead of "mov ebp, esp".
Everything else is similar, though in a different order.

The security object will no longer be at a fixed offset. However, the
offset can still be determined by looking up the GC-info and determining
how many callee-saved registers are pushed.

push ebp
mov ebp, esp
push esi
push edi
push ebx
sub esp, LOCALS_SIZE / push dummyReg if LOCALS_SIZE=sizeof(void*)
...
add esp, LOCALS_SIZE / pop dummyReg
pop ebx
pop edi
pop esi
(mov esp, ebp if there are no callee-saved registers)
pop ebp
ret

Double-aligned frame :
--------------------

LOCALS_SIZE_ADJUSTED needs to include an unused DWORD if an odd number
of callee-saved registers are pushed on the stack so that the locals
themselves are qword-aligned. The instructions are the same as today,
just in a different order.

push ebp
mov ebp, esp
and esp, 0xFFFFFFFC
push esi
push edi
push ebx
sub esp, LOCALS_SIZE_ADJUSTED / push dummyReg if LOCALS_SIZE=sizeof(void*)
...
add esp, LOCALS_SIZE_ADJUSTED / pop dummyReg
pop ebx
pop edi
pop esi
pop ebp
mov esp, ebp
pop ebp
ret

localloc (with ebp) frames :
--------------------------

The instructions are the same as today, just in a different order.
Also, today the epilog does "lea esp, [ebp-LOCALS_SIZE-calleeSavedRegsPushedSize]"
which will change to "lea esp, [ebp-calleeSavedRegsPushedSize]".

push ebp
mov ebp, esp
push esi
push edi
push ebx
sub esp, LOCALS_SIZE / push dummyReg if LOCALS_SIZE=sizeof(void*)
...
lea esp, [ebp-calleeSavedRegsPushedSize]
pop ebx
pop edi
pop esi
(mov esp, ebp if there are no callee-saved registers)
pop ebp
ret

*****************************************************************************/

void CodeGen::UpdateParamsWithInitialReg()
{
    auto setParamReg = [](LclVarDsc* lcl) {
        assert(lcl->IsParam());

        if (lcl->IsRegCandidate())
        {
            lcl->SetRegNum(lcl->GetParamInitialReg());
        }
    };

    for (unsigned lclNum = 0; lclNum < compiler->info.compArgsCount; lclNum++)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

        if (lcl->lvPromotedStruct())
        {
            for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); i++)
            {
                setParamReg(compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(i)));
            }
        }
        else
        {
            setParamReg(lcl);
        }
    }
}

// Finalize the frame size and offset assignments.
// No changes can be made to the modified register set after this,
// since that can affect how many callee-saved registers get saved.
void CodeGen::genFinalizeFrame()
{
    JITDUMP("Finalizing stack frame\n");

    // Initializations need to happen based on the var locations at the start
    // of the first basic block, so load those up. In particular, the determination
    // of whether or not to use block init in the prolog is dependent on the variable
    // locations on entry to the function.
    UpdateLclBlockLiveInRegs(compiler->fgFirstBB);

    MarkStackLocals();
    CheckUseBlockInit();

    // Mark various registers as "modified" for special code generation scenarios:
    // Edit & Continue, P/Invoke calls, stack probing, profiler hooks etc.

    const regMaskTP modifiedRegs = calleeSavedModifiedRegs;

    noway_assert(!IsFramePointerRequired() || ((modifiedRegs & RBM_FPBASE) == RBM_NONE));
#if ETW_EBP_FRAMED
    noway_assert((modifiedRegs & RBM_FPBASE) == RBM_NONE);
#endif

    regMaskTP specialRegs = RBM_NONE;

#ifdef TARGET_X86
    if (compiler->compTailCallUsed)
    {
        // If we are generating a helper-based tailcall, we've set the tailcall helper "flags"
        // argument to "1", indicating to the tailcall helper that we've saved the callee-saved
        // registers (ebx, esi, edi). So, we need to make sure all the callee-saved registers
        // actually get saved.

        specialRegs |= RBM_INT_CALLEE_SAVED & ~RBM_FPBASE;
    }
#endif

#ifdef TARGET_ARM
    if (genUseBlockInit)
    {
        // If we are using block init on ARM, then we may need save R4/R5/R6
        // so that we can use them during zero-initialization process.

        regMaskTP liveRegs = paramRegState.intRegLiveIn;

        liveRegs &= ~preSpillParamRegs;

        // Don't count the secret stub param, it will no longer be live when
        // we do block init.
        if (compiler->info.compPublishStubParam)
        {
            liveRegs &= ~RBM_SECRET_STUB_PARAM;
        }

        unsigned liveRegCount = genCountBits(liveRegs);

        if (liveRegCount >= 2)
        {
            specialRegs |= RBM_R4;

            if (liveRegCount >= 3)
            {
                specialRegs |= RBM_R5;

                if (liveRegCount >= 4)
                {
                    specialRegs |= RBM_R6;
                }
            }
        }
    }
#endif // TARGET_ARM

#if defined(TARGET_ARM) || defined(TARGET_XARCH)
    if (lclFrameSize >= compiler->eeGetPageSize())
    {
        // Make sure that callee-saved registers used by the stack probing helper call are saved.
        specialRegs |= RBM_STACK_PROBE_HELPER_ARG | RBM_STACK_PROBE_HELPER_TRASH;
        ARM_ONLY(specialRegs |= RBM_STACK_PROBE_HELPER_CALL_TARGET);
    }
#endif

    if (compiler->compMethodRequiresPInvokeFrame())
    {
        noway_assert(isFramePointerUsed());

        // If we have any P/Invoke calls, we might trash everything.
        specialRegs |= RBM_INT_CALLEE_SAVED & ~RBM_FPBASE;
    }

#ifdef UNIX_AMD64_ABI
    if (compiler->compIsProfilerHookNeeded())
    {
        // On Unix x64 we also save R14 and R15 for ELT profiler hook generation.
        specialRegs |= RBM_PROFILER_ENTER_ARG_0 | RBM_PROFILER_ENTER_ARG_1;
    }
#endif

    if (compiler->opts.compDbgEnC)
    {
        noway_assert(isFramePointerUsed());

#ifdef TARGET_AMD64
        // On x64 we always save only RSI and RDI for EnC.
        // TODO-MIKE-Review: This assumes that compDbgEnC implies no PInvoke frame
        // and relies on the VM also requesting debug code and not requesting EnC
        // in PInvoke IL stubs.
        noway_assert(((modifiedRegs | specialRegs) & ~(RBM_CALLEE_TRASH | RBM_RSI | RBM_RDI)) == RBM_NONE);

        specialRegs |= RBM_RSI | RBM_RDI;
#else
        // We save all callee-saved registers so the saved reg area size is consistent.
        specialRegs |= RBM_INT_CALLEE_SAVED & ~RBM_FPBASE;
#endif
    }

    regMaskTP pushedRegs = (modifiedRegs | specialRegs) & RBM_CALLEE_SAVED;

#ifdef TARGET_ARMARCH
    if (isFramePointerUsed())
    {
        pushedRegs |= RBM_FPBASE;
    }

    // We always push LR currently, even in leaf frames.
    pushedRegs |= RBM_LR;
#endif

#ifdef TARGET_ARM
    regMaskTP pushedFloatRegs = pushedRegs & RBM_ALLFLOAT;
    regMaskTP pushedIntRegs   = pushedRegs & ~RBM_ALLFLOAT;

    if ((pushedFloatRegs != RBM_NONE) ||
        (compiler->opts.MinOpts() && ((reservedRegs & pushedRegs & RBM_OPT_RSVD) != RBM_NONE)))
    {
        if ((genCountBits(GetPreSpillRegs() | pushedIntRegs) % 2) != 0)
        {
            // Try to keep the stack double-aligned for VPUSH by pushing
            // one more non-volatile int register, if available.
            // TODO-MIKE-Review: What do minopts and reserved regs have to do with this?

            regNumber extraIntReg = REG_R4;

            while ((pushedIntRegs & genRegMask(extraIntReg)) != RBM_NONE)
            {
                extraIntReg = REG_NEXT(extraIntReg);
            }

            if (extraIntReg < REG_R11)
            {
                pushedRegs |= genRegMask(extraIntReg);
                specialRegs |= genRegMask(extraIntReg);
            }
        }
    }

    // We currently make a single VPUSH/VPOP with consecutive, double-sized
    // registers, we'll mark more registers as modified if needed.

    if (pushedFloatRegs != RBM_NONE)
    {
        regMaskTP contiguousFloatRegs = genRegMaskFloat(REG_F16, TYP_DOUBLE);

        while (pushedFloatRegs > contiguousFloatRegs)
        {
            contiguousFloatRegs <<= 2;
            contiguousFloatRegs |= genRegMaskFloat(REG_F16, TYP_DOUBLE);
        }

        if (pushedFloatRegs != contiguousFloatRegs)
        {
            regMaskTP extraFloatRegs = contiguousFloatRegs - pushedFloatRegs;
            pushedRegs |= extraFloatRegs;
            specialRegs |= extraFloatRegs;
        }
    }
#endif // TARGET_ARM

    assert((specialRegs & RBM_FPBASE) == RBM_NONE);

    calleeSavedModifiedRegs = pushedRegs & RBM_CALLEE_SAVED;

#ifdef WINDOWS_AMD64_ABI
    pushedRegs &= ~RBM_ALLFLOAT;
#endif

    calleeRegsPushed = genCountBits(pushedRegs);

#ifdef DEBUG
    if (verbose)
    {
        printf("Special regs: ");
        dspRegMask(specialRegs);
        printf("\nCallee-saved registers pushed: %u ", calleeRegsPushed);
        dspRegMask(pushedRegs);
        printf("\n");
    }
#endif

    UpdateParamsWithInitialReg();

    compiler->lvaAssignFrameOffsets(Compiler::FINAL_FRAME_LAYOUT);
}

regNumber CodeGen::PrologChooseInitReg(regMaskTP initRegs)
{
    regMaskTP excludeRegs = paramRegState.intRegLiveIn | RBM_ALLFLOAT;
#ifdef TARGET_ARMARCH
    excludeRegs |= reservedRegs;
#endif

    // TODO-MIKE-Cleanup: This is bogus, the P/Invoke frame helper call
    // is in the first block, not in prolog.
    if (compiler->compMethodRequiresPInvokeFrame())
    {
        excludeRegs |= RBM_PINVOKE_FRAME;

        assert((!compiler->opts.ShouldUsePInvokeHelpers()) || (compiler->lvaPInvokeFrameListVar == BAD_VAR_NUM));

        if (!compiler->opts.ShouldUsePInvokeHelpers())
        {
            excludeRegs |= (RBM_PINVOKE_TCB | RBM_PINVOKE_SCRATCH);

            LclVarDsc* lcl = compiler->lvaGetDesc(compiler->lvaPInvokeFrameListVar);

            if (lcl->lvRegister)
            {
                excludeRegs |= genRegMask(lcl->GetRegNum());
            }
        }
    }

    regMaskTP candidateRegs = initRegs & ~excludeRegs;

    if (candidateRegs == RBM_NONE)
    {
        candidateRegs = (RBM_INT_CALLEE_TRASH | (calleeSavedModifiedRegs & RBM_ALLINT)) & ~excludeRegs;
    }

    return candidateRegs == RBM_NONE ? REG_SCRATCH : genRegNumFromMask(genFindLowestBit(candidateRegs));
}

// Generates code for a function prolog.
//
// NOTE REGARDING CHANGES THAT IMPACT THE DEBUGGER:
//
// The debugger relies on decoding ARM instructions to be able to successfully step through code. It does not
// implement decoding all ARM instructions. It only implements decoding the instructions which the JIT emits, and
// only instructions which result in control not going to the next instruction. Basically, any time execution would
// not continue at the next instruction (such as B, BL, BX, BLX, POP{pc}, etc.), the debugger has to be able to
// decode that instruction. If any of this is changed on ARM, the debugger team needs to be notified so that it
// can ensure stepping isn't broken. This is also a requirement for x86 and amd64.
//
// If any changes are made in the prolog, epilog, calls, returns, and branches, it is a good idea to notify the
// debugger team to ensure that stepping still works.
//
// ARM stepping code is here: debug\ee\arm\armwalker.cpp, vm\arm\armsinglestepper.cpp.
//
void CodeGen::genFnProlog()
{
    JITDUMP("*************** In genFnProlog()\n");

    ScopedSetVariable<bool> _setGeneratingProlog(&generatingProlog, true);
    compiler->funSetCurrentFunc(0);
    GetEmitter()->emitBegProlog();
    compiler->unwindBegProlog();
    liveness.BeginPrologEpilogCodeGen();

    // Do this so we can put the prolog instruction group ahead of other instruction groups.
    genIPmappingAddToFront(static_cast<IL_OFFSETX>(ICorDebugInfo::PROLOG));

#ifdef DEBUG
    if (compiler->opts.dspCode)
    {
        printf("\n__prolog:\n");
    }
#endif

    if (compiler->opts.compScopeInfo && (compiler->info.compVarScopesCount > 0))
    {
        // Create new scopes for the method-parameters for the prolog-block.
        psiBegProlog();
    }

#ifdef TARGET_XARCH
    // For OSR there is a "phantom prolog" to account for the actions taken
    // in the original frame that impact RBP and RSP on entry to the OSR method.
    if (compiler->opts.IsOSR())
    {
        PatchpointInfo* patchpointInfo    = compiler->info.compPatchpointInfo;
        const int       originalFrameSize = patchpointInfo->FpToSpDelta();

        compiler->unwindPush(REG_FPBASE);
        compiler->unwindAllocStack(originalFrameSize);
    }
#endif

#ifdef DEBUG
    if (compiler->compJitHaltMethod())
    {
        // Put a nop first because the debugger and other tools are likely to
        // put an int3 at the beginning and we don't want to confuse them.
        instGen(INS_nop);
        instGen(INS_BREAKPOINT);

#ifdef TARGET_ARMARCH
        // Avoid asserts in the unwind info because these instructions aren't accounted for.
        compiler->unwindPadding();
#endif
    }

#ifdef FEATURE_EH_FUNCLETS
    // We cannot force 0-initialization of the PSPSym as it will overwrite the real value.
    assert((compiler->lvaPSPSym == BAD_VAR_NUM) || !compiler->lvaGetDesc(compiler->lvaPSPSym)->lvMustInit);
#endif
#endif // DEBUG

    int       minBlockInitOffset;
    int       maxBlockInitOffset;
    regMaskTP initRegs;
#ifdef TARGET_ARM
    regMaskTP initDblRegs;
#endif

    MarkGCTrackedSlots(minBlockInitOffset, maxBlockInitOffset, initRegs ARM_ARG(initDblRegs));

#ifdef TARGET_ARM
    // On the ARM we will spill any incoming struct args in the first instruction in the prolog
    // Ditto for all enregistered user arguments in a varargs method.
    // These registers will be available to use for the initReg.  We just remove
    // all of these registers from the rsCalleeRegArgMaskLiveIn.
    paramRegState.intRegLiveIn &= ~preSpillParamRegs;

    if (regMaskTP preSpillRegs = GetPreSpillRegs())
    {
        GetEmitter()->emitIns_I(INS_push, EA_4BYTE, static_cast<int>(preSpillRegs));
        compiler->unwindPushMaskInt(preSpillRegs);
    }
#endif

#ifdef TARGET_XARCH
#ifdef TARGET_AMD64
    if (compiler->info.compIsVarArgs && !compiler->opts.IsOSR())
    {
        GetEmitter()->PrologSpillParamRegsToShadowSlots();
    }
#endif

    if (IsFramePointerRequired())
    {
        GetEmitter()->emitIns_R(INS_push, EA_PTRSIZE, REG_FPBASE);
        compiler->unwindPush(REG_FPBASE);

#ifdef USING_SCOPE_INFO
        psiAdjustStackLevel(REGSIZE_BYTES);
#endif

#ifndef TARGET_AMD64
        PrologEstablishFramePointer(0, /*reportUnwindData*/ true);
#endif

#if DOUBLE_ALIGN
        if (doDoubleAlign())
        {
            noway_assert(!isFramePointerUsed());

            GetEmitter()->emitIns_R_I(INS_and, EA_4BYTE, REG_ESP, -8);
        }
#endif
    }
#endif // TARGET_XARCH

    regNumber initReg = PrologChooseInitReg(initRegs);

    // Track if initReg holds non-zero value. Start conservative and assume it has non-zero value.
    // If initReg is ever set to zero, this variable is set to true and zero initializing initReg
    // will be skipped.
    bool initRegZeroed = false;

#ifdef TARGET_ARM64
    PrologPushCalleeSavedRegisters(initReg, &initRegZeroed);
#endif

#ifdef TARGET_ARM
    PrologPushCalleeSavedRegisters();

    bool needToEstablishFP        = false;
    int  afterLclFrameSPtoFPdelta = 0;

    if (isFramePointerUsed())
    {
        needToEstablishFP = true;

        // If the local frame is small enough, we establish the frame pointer after the OS-reported prolog.
        // This makes the prolog and epilog match, giving us smaller unwind data. If the frame size is
        // too big, we go ahead and do it here.

        int SPtoFPdelta          = (calleeRegsPushed - 2) * REGSIZE_BYTES;
        afterLclFrameSPtoFPdelta = SPtoFPdelta + lclFrameSize;

        if (!emitter::emitIns_valid_imm_for_add_sp(afterLclFrameSPtoFPdelta))
        {
            PrologEstablishFramePointer(SPtoFPdelta, /*reportUnwindData*/ true);
            needToEstablishFP = false;
        }
    }

    if (genStackAllocRegisterMask(lclFrameSize, calleeSavedModifiedRegs) == RBM_NONE)
    {
        PrologAllocLclFrame(lclFrameSize, initReg, &initRegZeroed, paramRegState.intRegLiveIn);
    }

    if (compiler->compLocallocUsed)
    {
        GetEmitter()->emitIns_Mov(INS_mov, EA_4BYTE, REG_SAVED_LOCALLOC_SP, REG_SPBASE, /* canSkip */ false);
        compiler->unwindSetFrameReg(REG_SAVED_LOCALLOC_SP, 0);
    }

    if (needToEstablishFP)
    {
        PrologEstablishFramePointer(afterLclFrameSPtoFPdelta, /*reportUnwindData*/ false);
    }
#endif

#ifdef TARGET_XARCH
    PrologPushCalleeSavedRegisters();
    PrologAllocLclFrame(lclFrameSize, initReg, &initRegZeroed, paramRegState.intRegLiveIn);
    PrologPreserveCalleeSavedFloatRegs(lclFrameSize);

#ifdef TARGET_AMD64
    if (isFramePointerUsed())
    {
        const bool reportUnwindData = compiler->compLocallocUsed || compiler->opts.compDbgEnC;
        PrologEstablishFramePointer(genSPtoFPdelta(), reportUnwindData);
    }
#endif
#endif // TARGET_XARCH

    if (compiler->info.compPublishStubParam)
    {
        assert((paramRegState.intRegLiveIn & RBM_SECRET_STUB_PARAM) != RBM_NONE);

#ifdef TARGET_XARCH
        GetEmitter()->emitIns_AR_R(INS_mov, EA_PTRSIZE, REG_SECRET_STUB_PARAM, genFramePointerReg(),
                                   compiler->lvaGetDesc(compiler->lvaStubArgumentVar)->GetStackOffset());
#else
        GetEmitter()->emitIns_S_R(INS_str, EA_PTRSIZE, REG_SECRET_STUB_PARAM, compiler->lvaStubArgumentVar, 0);
#endif

        // It's no longer live; clear it out so it can be used after this in the prolog
        paramRegState.intRegLiveIn &= ~RBM_SECRET_STUB_PARAM;
    }

    if (genUseBlockInit)
    {
        PrologBlockInitLocals(minBlockInitOffset, maxBlockInitOffset, initReg, &initRegZeroed);
    }
    else if (genInitStkLclCnt > 0)
    {
        PrologZeroInitUntrackedLocals(initReg, &initRegZeroed);
    }

    if (compiler->opts.IsOSR())
    {
        PrologInitOsrLocals();
    }

#ifdef FEATURE_EH_FUNCLETS
    if (compiler->lvaPSPSym != BAD_VAR_NUM)
    {
        PrologSetPSPSym(initReg, &initRegZeroed);
    }
#else
    // When compInitMem is true the PrologBlockInitLocals will zero out the shadow SP slots.
    if (compiler->ehNeedsShadowSPslots() && !compiler->info.compInitMem)
    {
        // The last slot is reserved for ICodeManager::FixContext(ppEndRegion)
        unsigned filterEndOffsetSlotOffs =
            compiler->lvaGetDesc(compiler->lvaShadowSPslotsVar)->GetBlockSize() - REGSIZE_BYTES;

        // Zero out the slot for nesting level 0
        unsigned firstSlotOffs = filterEndOffsetSlotOffs - REGSIZE_BYTES;

        if (!initRegZeroed)
        {
            instGen_Set_Reg_To_Zero(EA_PTRSIZE, initReg);
            initRegZeroed = true;
        }

        GetEmitter()->emitIns_S_R(INS_mov, EA_PTRSIZE, initReg, compiler->lvaShadowSPslotsVar, firstSlotOffs);
    }
#endif // !FEATURE_EH_FUNCLETS

    if (!compiler->opts.IsOSR())
    {
        PrologReportGenericContextArg(initReg, &initRegZeroed);
    }

#ifdef JIT32_GCENCODER
    if (compiler->lvaLocAllocSPvar != BAD_VAR_NUM)
    {
        GetEmitter()->emitIns_S_R(INS_mov, EA_4BYTE, REG_ESP, compiler->lvaLocAllocSPvar, 0);
    }
#endif

    if (compiler->getNeedsGSSecurityCookie() &&
        (!compiler->opts.IsOSR() || !compiler->info.compPatchpointInfo->HasSecurityCookie()))
    {
        PrologSetGSSecurityCookie(initReg, &initRegZeroed);
    }

#ifdef PROFILING_SUPPORTED
    if (!compiler->opts.IsOSR())
    {
        PrologProfilingEnterCallback(initReg, &initRegZeroed);
    }
#endif

    if (!GetInterruptible())
    {
        // The 'real' prolog ends here for non-interruptible methods.
        // For fully-interruptible methods, we extend the prolog so that
        // we do not need to track GC inforation while shuffling the
        // arguments.
        GetEmitter()->emitMarkPrologEnd();
    }

#ifdef UNIX_AMD64_ABI
    // The unused bits of Vector3 arguments must be cleared
    // since native compiler doesn't initize the upper bits to zeros.
    //
    // TODO-Cleanup: This logic can be implemented in
    // genPrologMoveParamRegs() for argument registers and
    // genPrologEnregisterIncomingStackParams() for stack arguments.
    PrologClearVector3StackParamUpperBits();
#endif

    UpdateParamsWithInitialReg();

    // Home incoming arguments and generate any required inits.
    // OSR handles this by moving the values from the original frame.
    if (!compiler->opts.IsOSR())
    {
        PrologMoveParams(initReg, &initRegZeroed);
    }

    if ((initRegs ARM_ONLY(| initDblRegs)) != RBM_NONE)
    {
        PrologZeroRegs(initRegs, initRegZeroed ? initReg : REG_NA ARM_ARG(initDblRegs));
    }

    // Increase the prolog size here only if fully interruptible.

    if (GetInterruptible())
    {
        GetEmitter()->emitMarkPrologEnd();
    }
    if (compiler->opts.compScopeInfo && (compiler->info.compVarScopesCount > 0))
    {
        psiEndProlog();
    }

#ifdef TARGET_X86
    if (compiler->info.compIsVarArgs && (compiler->lvaGetDesc(compiler->lvaVarargsBaseOfStkArgs)->GetRefCount() > 0))
    {
        PrologInitVarargsStackParamsBaseOffset();
    }
#endif

#if defined(DEBUG) && defined(TARGET_XARCH)
    if (compiler->lvaReturnSpCheck != BAD_VAR_NUM)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(compiler->lvaReturnSpCheck);
        assert(lcl->lvOnFrame && lcl->lvDoNotEnregister);
        GetEmitter()->emitIns_S_R(INS_mov, EA_PTRSIZE, REG_SPBASE, compiler->lvaReturnSpCheck, 0);
    }
#endif

    GetEmitter()->emitEndProlog();
    compiler->unwindEndProlog();
}

#ifdef FEATURE_EH_FUNCLETS

/*-----------------------------------------------------------------------------
 *
 *  Set the main function PSPSym value in the frame.
 *  Funclets use different code to load the PSP sym and save it in their frame.
 *  See the document "X64 and ARM ABIs.docx" for a full description of the PSPSym.
 *  The PSPSym section of that document is copied here.
 *
 ***********************************
 *  The name PSPSym stands for Previous Stack Pointer Symbol.  It is how a funclet
 *  accesses locals from the main function body.
 *
 *  First, two definitions.
 *
 *  Caller-SP is the value of the stack pointer in a function's caller before the call
 *  instruction is executed. That is, when function A calls function B, Caller-SP for B
 *  is the value of the stack pointer immediately before the call instruction in A
 *  (calling B) was executed. Note that this definition holds for both AMD64, which
 *  pushes the return value when a call instruction is executed, and for ARM, which
 *  doesn't. For AMD64, Caller-SP is the address above the call return address.
 *
 *  Initial-SP is the initial value of the stack pointer after the fixed-size portion of
 *  the frame has been allocated. That is, before any "alloca"-type allocations.
 *
 *  The PSPSym is a pointer-sized local variable in the frame of the main function and
 *  of each funclet. The value stored in PSPSym is the value of Initial-SP/Caller-SP
 *  for the main function.  The stack offset of the PSPSym is reported to the VM in the
 *  GC information header.  The value reported in the GC information is the offset of the
 *  PSPSym from Initial-SP/Caller-SP. (Note that both the value stored, and the way the
 *  value is reported to the VM, differs between architectures. In particular, note that
 *  most things in the GC information header are reported as offsets relative to Caller-SP,
 *  but PSPSym on AMD64 is one (maybe the only) exception.)
 *
 *  The VM uses the PSPSym to find other locals it cares about (such as the generics context
 *  in a funclet frame). The JIT uses it to re-establish the frame pointer register, so that
 *  the frame pointer is the same value in a funclet as it is in the main function body.
 *
 *  When a funclet is called, it is passed the Establisher Frame Pointer. For AMD64 this is
 *  true for all funclets and it is passed as the first argument in RCX, but for ARM this is
 *  only true for first pass funclets (currently just filters) and it is passed as the second
 *  argument in R1. The Establisher Frame Pointer is a stack pointer of an interesting "parent"
 *  frame in the exception processing system. For the CLR, it points either to the main function
 *  frame or a dynamically enclosing funclet frame from the same function, for the funclet being
 *  invoked. The value of the Establisher Frame Pointer is Initial-SP on AMD64, Caller-SP on ARM.
 *
 *  Using the establisher frame, the funclet wants to load the value of the PSPSym. Since we
 *  don't know if the Establisher Frame is from the main function or a funclet, we design the
 *  main function and funclet frame layouts to place the PSPSym at an identical, small, constant
 *  offset from the Establisher Frame in each case. (This is also required because we only report
 *  a single offset to the PSPSym in the GC information, and that offset must be valid for the main
 *  function and all of its funclets). Then, the funclet uses this known offset to compute the
 *  PSPSym address and read its value. From this, it can compute the value of the frame pointer
 *  (which is a constant offset from the PSPSym value) and set the frame register to be the same
 *  as the parent function. Also, the funclet writes the value of the PSPSym to its own frame's
 *  PSPSym. This "copying" of the PSPSym happens for every funclet invocation, in particular,
 *  for every nested funclet invocation.
 *
 *  On ARM, for all second pass funclets (finally, fault, catch, and filter-handler) the VM
 *  restores all non-volatile registers to their values within the parent frame. This includes
 *  the frame register (R11). Thus, the PSPSym is not used to recompute the frame pointer register
 *  in this case, though the PSPSym is copied to the funclet's frame, as for all funclets.
 *
 *  Catch, Filter, and Filter-handlers also get an Exception object (GC ref) as an argument
 *  (REG_EXCEPTION_OBJECT).  On AMD64 it is the second argument and thus passed in RDX.  On
 *  ARM this is the first argument and passed in R0.
 *
 *  (Note that the JIT64 source code contains a comment that says, "The current CLR doesn't always
 *  pass the correct establisher frame to the funclet. Funclet may receive establisher frame of
 *  funclet when expecting that of original routine." It indicates this is the reason that a PSPSym
 *  is required in all funclets as well as the main function, whereas if the establisher frame was
 *  correctly reported, the PSPSym could be omitted in some cases.)
 ***********************************
 */
void CodeGen::PrologSetPSPSym(regNumber initReg, bool* pInitRegZeroed)
{
    assert(compiler->lvaPSPSym != BAD_VAR_NUM);
    noway_assert(isFramePointerUsed());

#if defined(TARGET_ARM)

    // We either generate:
    //     add     r1, r11, 8
    //     str     r1, [reg + PSPSymOffset]
    // or:
    //     add     r1, sp, 76
    //     str     r1, [reg + PSPSymOffset]
    // depending on the smallest encoding

    int SPtoCallerSPdelta = -genCallerSPtoInitialSPdelta();

    int       callerSPOffs;
    regNumber regBase;

    if (emitter::emitIns_valid_imm_for_add_sp(SPtoCallerSPdelta))
    {
        // use the "add <reg>, sp, imm" form

        callerSPOffs = SPtoCallerSPdelta;
        regBase      = REG_SPBASE;
    }
    else
    {
        // use the "add <reg>, r11, imm" form

        int FPtoCallerSPdelta = -genCallerSPtoFPdelta();
        noway_assert(emitter::emitIns_valid_imm_for_add(FPtoCallerSPdelta));

        callerSPOffs = FPtoCallerSPdelta;
        regBase      = REG_FPBASE;
    }

    // We will just use the initReg since it is an available register
    // and we are probably done using it anyway...
    regNumber regTmp = initReg;
    *pInitRegZeroed  = false;

    GetEmitter()->emitIns_R_R_I(INS_add, EA_PTRSIZE, regTmp, regBase, callerSPOffs);
    GetEmitter()->emitIns_S_R(INS_str, EA_PTRSIZE, regTmp, compiler->lvaPSPSym, 0);

#elif defined(TARGET_ARM64)

    int SPtoCallerSPdelta = -genCallerSPtoInitialSPdelta();

    // We will just use the initReg since it is an available register
    // and we are probably done using it anyway...
    regNumber regTmp = initReg;
    *pInitRegZeroed  = false;

    GetEmitter()->emitIns_R_R_Imm(INS_add, EA_PTRSIZE, regTmp, REG_SPBASE, SPtoCallerSPdelta);
    GetEmitter()->emitIns_S_R(INS_str, EA_PTRSIZE, regTmp, compiler->lvaPSPSym, 0);

#elif defined(TARGET_AMD64)

    // The PSP sym value is Initial-SP, not Caller-SP!
    // We assume that RSP is Initial-SP when this function is called. That is, the stack frame
    // has been established.
    //
    // We generate:
    //     mov     [rbp-20h], rsp       // store the Initial-SP (our current rsp) in the PSPsym

    GetEmitter()->emitIns_S_R(ins_Store(TYP_I_IMPL), EA_PTRSIZE, REG_SPBASE, compiler->lvaPSPSym, 0);

#else // TARGET*

    NYI("Set function PSP sym");

#endif // TARGET*
}

#endif // FEATURE_EH_FUNCLETS

/*****************************************************************************
 *
 *  Generates code for all the function and funclet prologs and epilogs.
 */

void CodeGen::genGeneratePrologsAndEpilogs()
{
#ifdef DEBUG
    if (verbose)
    {
        printf("*************** Before prolog / epilog generation\n");
        compiler->lvaTableDump();
        GetEmitter()->emitDispIGlist(false);
    }
#endif

    // Before generating the prolog, we need to reset the variable locations to what they will be on entry.
    // This affects our code that determines which untracked locals need to be zero initialized.
    UpdateLclBlockLiveInRegs(compiler->fgFirstBB);

    genFnProlog();
#ifdef FEATURE_EH_FUNCLETS
    // Capture the data we're going to use in the funclet prolog and epilog generation. This is
    // information computed during codegen, or during function prolog generation, like
    // frame offsets. It must run after main function prolog generation.
    genCaptureFuncletPrologEpilogInfo();
#endif
    GetEmitter()->emitGeneratePrologEpilog();

#ifdef DEBUG
    if (verbose)
    {
        printf("*************** After prolog / epilog generation\n");
        GetEmitter()->emitDispIGlist(false);
    }
#endif
}

/*****************************************************************************
 *                          genSetScopeInfo
 *
 * This function should be called only after the sizes of the emitter blocks
 * have been finalized.
 */

void CodeGen::genSetScopeInfo()
{
    if (!compiler->opts.compScopeInfo)
    {
        return;
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In genSetScopeInfo()\n");
    }
#endif

    unsigned varsLocationsCount = 0;

#ifdef USING_SCOPE_INFO
    if (compiler->info.compVarScopesCount > 0)
    {
        varsLocationsCount = siScopeCnt + psiScopeCnt;
    }
#else // USING_SCOPE_INFO

#ifdef USING_VARIABLE_LIVE_RANGE
    varsLocationsCount = (unsigned int)varLiveKeeper->getLiveRangesCount();
#endif // USING_VARIABLE_LIVE_RANGE

#endif // USING_SCOPE_INFO

    if (varsLocationsCount == 0)
    {
        // No variable home to report
        eeSetLVcount(0);
        eeSetLVdone();
        return;
    }

    noway_assert(compiler->opts.compScopeInfo && (compiler->info.compVarScopesCount > 0));

    // Initialize the table where the reported variables' home will be placed.
    eeSetLVcount(varsLocationsCount);

#ifdef LATE_DISASM
    genTrnslLocalVarCount = varsLocationsCount;
    if (varsLocationsCount)
    {
        genTrnslLocalVarInfo = new (compiler, CMK_DebugOnly) TrnslLocalVarInfo[varsLocationsCount];
    }
#endif

#ifdef USING_SCOPE_INFO
    genSetScopeInfoUsingsiScope();
#else // USING_SCOPE_INFO
#ifdef USING_VARIABLE_LIVE_RANGE
    // We can have one of both flags defined, both, or none. Specially if we need to compare both
    // both results. But we cannot report both to the debugger, since there would be overlapping
    // intervals, and may not indicate the same variable location.

    genSetScopeInfoUsingVariableRanges();

#endif // USING_VARIABLE_LIVE_RANGE
#endif // USING_SCOPE_INFO

    eeSetLVdone();
}

#ifdef USING_SCOPE_INFO
void CodeGen::genSetScopeInfoUsingsiScope()
{
    noway_assert(psiOpenScopeList.scNext == nullptr);

    // Record the scopes found for the parameters over the prolog.
    // The prolog needs to be treated differently as a variable may not
    // have the same info in the prolog block as is given by compiler->lvaTable.
    // eg. A register parameter is actually on the stack, before it is loaded to reg.

    CodeGen::psiScope* scopeP;
    unsigned           i;

    for (i = 0, scopeP = psiScopeList.scNext; i < psiScopeCnt; i++, scopeP = scopeP->scNext)
    {
        noway_assert(scopeP != nullptr);
        noway_assert(scopeP->scStartLoc.Valid());
        noway_assert(scopeP->scEndLoc.Valid());

        UNATIVE_OFFSET startOffs = scopeP->scStartLoc.CodeOffset(GetEmitter());
        UNATIVE_OFFSET endOffs   = scopeP->scEndLoc.CodeOffset(GetEmitter());

        unsigned varNum = scopeP->scSlotNum;
        noway_assert(startOffs <= endOffs);

        // The range may be 0 if the prolog is empty. For such a case,
        // report the liveness of arguments to span at least the first
        // instruction in the method. This will be incorrect (except on
        // entry to the method) if the very first instruction of the method
        // is part of a loop. However, this should happen
        // very rarely, and the incorrectness is worth being able to look
        // at the argument on entry to the method.
        if (startOffs == endOffs)
        {
            noway_assert(startOffs == 0);
            endOffs++;
        }

        siVarLoc varLoc = scopeP->getSiVarLoc();

        genSetScopeInfo(i, startOffs, endOffs - startOffs, varNum, scopeP->scLVnum, true, &varLoc);
    }

    // Record the scopes for the rest of the method.
    // Check that the LocalVarInfo scopes look OK
    noway_assert(siOpenScopeList.scNext == nullptr);

    CodeGen::siScope* scopeL;

    for (i = 0, scopeL = siScopeList.scNext; i < siScopeCnt; i++, scopeL = scopeL->scNext)
    {
        noway_assert(scopeL != nullptr);
        noway_assert(scopeL->scStartLoc.Valid());
        noway_assert(scopeL->scEndLoc.Valid());

        // Find the start and end IP

        UNATIVE_OFFSET startOffs = scopeL->scStartLoc.CodeOffset(GetEmitter());
        UNATIVE_OFFSET endOffs   = scopeL->scEndLoc.CodeOffset(GetEmitter());

        noway_assert(scopeL->scStartLoc != scopeL->scEndLoc);

        LclVarDsc* varDsc = compiler->lvaGetDesc(scopeL->scVarNum);
        siVarLoc   varLoc = getSiVarLoc(varDsc, scopeL);

        genSetScopeInfo(psiScopeCnt + i, startOffs, endOffs - startOffs, scopeL->scVarNum, scopeL->scLVnum, false,
                        &varLoc);
    }
}
#endif // USING_SCOPE_INFO

#ifdef USING_VARIABLE_LIVE_RANGE
//------------------------------------------------------------------------
// genSetScopeInfoUsingVariableRanges: Call "genSetScopeInfo" with the
//  "VariableLiveRanges" created for the arguments, special arguments and
//  IL local variables.
//
// Notes:
//  This function is called from "genSetScopeInfo" once the code is generated
//  and we want to send debug info to the debugger.
//
void CodeGen::genSetScopeInfoUsingVariableRanges()
{
    unsigned int liveRangeIndex = 0;

    for (unsigned int varNum = 0; varNum < compiler->info.compLocalsCount; varNum++)
    {
        LclVarDsc* varDsc = compiler->lvaGetDesc(varNum);

        if (compiler->compMap2ILvarNum(varNum) != (unsigned int)ICorDebugInfo::UNKNOWN_ILNUM)
        {
            VariableLiveKeeper::LiveRangeList* liveRanges = nullptr;

            for (int rangeIndex = 0; rangeIndex < 2; rangeIndex++)
            {
                if (rangeIndex == 0)
                {
                    liveRanges = varLiveKeeper->getLiveRangesForVarForProlog(varNum);
                }
                else
                {
                    liveRanges = varLiveKeeper->getLiveRangesForVarForBody(varNum);
                }
                for (VariableLiveKeeper::VariableLiveRange& liveRange : *liveRanges)
                {
                    UNATIVE_OFFSET startOffs = liveRange.m_StartEmitLocation.CodeOffset(GetEmitter());
                    UNATIVE_OFFSET endOffs   = liveRange.m_EndEmitLocation.CodeOffset(GetEmitter());

                    if (varDsc->IsParam() && (startOffs == endOffs))
                    {
                        // If the length is zero, it means that the prolog is empty. In that case,
                        // CodeGen::genSetScopeInfo will report the liveness of all arguments
                        // as spanning the first instruction in the method, so that they can
                        // at least be inspected on entry to the method.
                        endOffs++;
                    }

                    genSetScopeInfo(liveRangeIndex, startOffs, endOffs - startOffs, varNum,
                                    varNum /* I dont know what is the which in eeGetLvInfo */, true,
                                    &liveRange.m_VarLocation);
                    liveRangeIndex++;
                }
            }
        }
    }
}
#endif // USING_VARIABLE_LIVE_RANGE

//------------------------------------------------------------------------
// genSetScopeInfo: Record scope information for debug info
//
// Arguments:
//    which
//    startOffs - the starting offset for this scope
//    length    - the length of this scope
//    varNum    - the lclVar for this scope info
//    LVnum
//    avail     - a bool indicating if it has a home
//    varLoc    - the position (reg or stack) of the variable
//
// Notes:
//    Called for every scope info piece to record by the main genSetScopeInfo()

void CodeGen::genSetScopeInfo(unsigned       which,
                              UNATIVE_OFFSET startOffs,
                              UNATIVE_OFFSET length,
                              unsigned       varNum,
                              unsigned       LVnum,
                              bool           avail,
                              siVarLoc*      varLoc)
{
    // We need to do some mapping while reporting back these variables.

    unsigned ilVarNum = compiler->compMap2ILvarNum(varNum);
    noway_assert((int)ilVarNum != ICorDebugInfo::UNKNOWN_ILNUM);

#ifdef TARGET_X86
    // Non-x86 platforms are allowed to access all arguments directly
    // so we don't need this code.

    // Is this a varargs function?
    if (compiler->info.compIsVarArgs && (varNum != compiler->lvaVarargsHandleArg) &&
        (varNum < compiler->info.compArgsCount) && !compiler->lvaGetDesc(varNum)->IsRegParam())
    {
        noway_assert(varLoc->vlType == VLT_STK || varLoc->vlType == VLT_STK2);

        // All stack arguments (except the varargs handle) have to be
        // accessed via the varargs cookie. Discard generated info,
        // and just find its position relative to the varargs handle

        PREFIX_ASSUME(compiler->lvaVarargsHandleArg < compiler->info.compArgsCount);
        if (!compiler->lvaTable[compiler->lvaVarargsHandleArg].lvOnFrame)
        {
            noway_assert(!compiler->opts.compDbgCode);
            return;
        }

        // Can't check compiler->lvaTable[varNum].lvOnFrame as we don't set it for
        // arguments of vararg functions to avoid reporting them to GC.
        noway_assert(!compiler->lvaTable[varNum].lvRegister);
        unsigned cookieOffset = compiler->lvaTable[compiler->lvaVarargsHandleArg].GetStackOffset();
        unsigned varOffset    = compiler->lvaTable[varNum].GetStackOffset();

        noway_assert(cookieOffset < varOffset);
        unsigned offset     = varOffset - cookieOffset;
        unsigned stkArgSize = paramsStackSize;
        noway_assert(offset < stkArgSize);
        offset = stkArgSize - offset;

        varLoc->vlType                   = VLT_FIXED_VA;
        varLoc->vlFixedVarArg.vlfvOffset = offset;
    }

#endif // TARGET_X86

#ifdef LATE_DISASM
    const char* name = nullptr;

    for (unsigned scopeNum = 0; scopeNum < compiler->info.compVarScopesCount; scopeNum++)
    {
        if (LVnum == compiler->info.compVarScopes[scopeNum].vsdLVnum)
        {
            name = compiler->info.compVarScopes[scopeNum].vsdName;
        }
    }

    TrnslLocalVarInfo& tlvi = genTrnslLocalVarInfo[which];

    tlvi.tlviVarNum    = ilVarNum;
    tlvi.tlviLVnum     = LVnum;
    tlvi.tlviName      = name;
    tlvi.tlviStartPC   = startOffs;
    tlvi.tlviLength    = length;
    tlvi.tlviAvailable = avail;
    tlvi.tlviVarLoc    = *varLoc;
#endif

    eeSetLVinfo(which, startOffs, length, ilVarNum, *varLoc);
}

#ifdef LATE_DISASM
const char* CodeGen::siRegVarName(size_t offs, size_t size, unsigned reg)
{
    if (!compiler->opts.compScopeInfo)
        return nullptr;

    if (compiler->info.compVarScopesCount == 0)
        return nullptr;

    TrnslLocalVarInfo* info = genTrnslLocalVarInfo;

    noway_assert((genTrnslLocalVarCount == 0) || (info != nullptr));

    for (unsigned i = 0; i < genTrnslLocalVarCount; i++)
    {
        if ((info[i].tlviVarLoc.vlIsInReg((regNumber)reg)) && (info[i].tlviAvailable == true) &&
            (info[i].tlviStartPC <= offs + size) && (info[i].tlviStartPC + info[i].tlviLength > offs))
        {
            return info[i].tlviName ? info[i].tlviName : nullptr;
        }
    }

    return nullptr;
}

const char* CodeGen::siStackVarName(size_t offs, size_t size, unsigned reg, unsigned stkOffs)
{
    if (!compiler->opts.compScopeInfo)
        return nullptr;

    if (compiler->info.compVarScopesCount == 0)
        return nullptr;

    TrnslLocalVarInfo* info = genTrnslLocalVarInfo;

    noway_assert((genTrnslLocalVarCount == 0) || (info != nullptr));

    for (unsigned i = 0; i < genTrnslLocalVarCount; i++)
    {
        if ((info[i].tlviVarLoc.vlIsOnStack((regNumber)reg, stkOffs)) && (info[i].tlviAvailable == true) &&
            (info[i].tlviStartPC <= offs + size) && (info[i].tlviStartPC + info[i].tlviLength > offs))
        {
            return info[i].tlviName ? info[i].tlviName : nullptr;
        }
    }

    return nullptr;
}
#endif // LATE_DISASM

#ifdef DEBUG

/*****************************************************************************
 *  Display a IPmappingDsc. Pass -1 as mappingNum to not display a mapping number.
 */

void CodeGen::genIPmappingDisp(unsigned mappingNum, IPmappingDsc* ipMapping)
{
    if (mappingNum != unsigned(-1))
    {
        printf("%d: ", mappingNum);
    }

    IL_OFFSETX offsx = ipMapping->ipmdILoffsx;

    if (offsx == BAD_IL_OFFSET)
    {
        printf("???");
    }
    else
    {
        eeDispILOffs(jitGetILoffsAny(offsx));

        if (jitIsStackEmpty(offsx))
        {
            printf(" STACK_EMPTY");
        }

        if (jitIsCallInstruction(offsx))
        {
            printf(" CALL_INSTRUCTION");
        }
    }

    printf(" ");
    ipMapping->ipmdNativeLoc.Print(compiler->compMethodID);
    // We can only call this after code generation. Is there any way to tell when it's legal to call?
    // printf(" [%x]", ipMapping->ipmdNativeLoc.CodeOffset(GetEmitter()));

    if (ipMapping->ipmdIsLabel)
    {
        printf(" label");
    }

    printf("\n");
}

void CodeGen::genIPmappingListDisp()
{
    unsigned      mappingNum = 0;
    IPmappingDsc* ipMapping;

    for (ipMapping = genIPmappingList; ipMapping != nullptr; ipMapping = ipMapping->ipmdNext)
    {
        genIPmappingDisp(mappingNum, ipMapping);
        ++mappingNum;
    }
}

#endif // DEBUG

/*****************************************************************************
 *
 *  Append an IPmappingDsc struct to the list that we're maintaining
 *  for the debugger.
 *  Record the instr offset as being at the current code gen position.
 */

void CodeGen::genIPmappingAdd(IL_OFFSETX offsx, bool isLabel)
{
    if (!compiler->opts.compDbgInfo)
    {
        return;
    }

    assert(offsx != BAD_IL_OFFSET);

    switch ((int)offsx) // Need the cast since offs is unsigned and the case statements are comparing to signed.
    {
        case ICorDebugInfo::PROLOG:
        case ICorDebugInfo::EPILOG:
            break;

        default:

            if (offsx != (IL_OFFSETX)ICorDebugInfo::NO_MAPPING)
            {
                noway_assert(jitGetILoffs(offsx) <= compiler->info.compILCodeSize);
            }

            // Ignore this one if it's the same IL offset as the last one we saw.
            // Note that we'll let through two identical IL offsets if the flag bits
            // differ, or two identical "special" mappings (e.g., PROLOG).
            if ((genIPmappingLast != nullptr) && (offsx == genIPmappingLast->ipmdILoffsx))
            {
                JITDUMP("genIPmappingAdd: ignoring duplicate IL offset 0x%x\n", offsx);
                return;
            }
            break;
    }

    /* Create a mapping entry and append it to the list */

    IPmappingDsc* addMapping = compiler->getAllocator(CMK_DebugInfo).allocate<IPmappingDsc>(1);
    addMapping->ipmdNativeLoc.CaptureLocation(GetEmitter());
    addMapping->ipmdILoffsx = offsx;
    addMapping->ipmdIsLabel = isLabel;
    addMapping->ipmdNext    = nullptr;

    if (genIPmappingList != nullptr)
    {
        assert(genIPmappingLast != nullptr);
        assert(genIPmappingLast->ipmdNext == nullptr);
        genIPmappingLast->ipmdNext = addMapping;
    }
    else
    {
        assert(genIPmappingLast == nullptr);
        genIPmappingList = addMapping;
    }

    genIPmappingLast = addMapping;

#ifdef DEBUG
    if (verbose)
    {
        printf("Added IP mapping: ");
        genIPmappingDisp(unsigned(-1), addMapping);
    }
#endif // DEBUG
}

/*****************************************************************************
 *
 *  Prepend an IPmappingDsc struct to the list that we're maintaining
 *  for the debugger.
 *  Record the instr offset as being at the current code gen position.
 */
void CodeGen::genIPmappingAddToFront(IL_OFFSETX offsx)
{
    if (!compiler->opts.compDbgInfo)
    {
        return;
    }

    assert(offsx != BAD_IL_OFFSET);
    assert(generatingProlog); // We only ever do this during prolog generation.

    switch ((int)offsx) // Need the cast since offs is unsigned and the case statements are comparing to signed.
    {
        case ICorDebugInfo::NO_MAPPING:
        case ICorDebugInfo::PROLOG:
        case ICorDebugInfo::EPILOG:
            break;

        default:
            noway_assert(jitGetILoffs(offsx) <= compiler->info.compILCodeSize);
            break;
    }

    /* Create a mapping entry and prepend it to the list */

    IPmappingDsc* addMapping = compiler->getAllocator(CMK_DebugInfo).allocate<IPmappingDsc>(1);
    addMapping->ipmdNativeLoc.CaptureLocation(GetEmitter());
    addMapping->ipmdILoffsx = offsx;
    addMapping->ipmdIsLabel = true;
    addMapping->ipmdNext    = nullptr;

    addMapping->ipmdNext = genIPmappingList;
    genIPmappingList     = addMapping;

    if (genIPmappingLast == nullptr)
    {
        genIPmappingLast = addMapping;
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("Added IP mapping to front: ");
        genIPmappingDisp(unsigned(-1), addMapping);
    }
#endif // DEBUG
}

/*****************************************************************************/

C_ASSERT(IL_OFFSETX(ICorDebugInfo::NO_MAPPING) != IL_OFFSETX(BAD_IL_OFFSET));
C_ASSERT(IL_OFFSETX(ICorDebugInfo::PROLOG) != IL_OFFSETX(BAD_IL_OFFSET));
C_ASSERT(IL_OFFSETX(ICorDebugInfo::EPILOG) != IL_OFFSETX(BAD_IL_OFFSET));

C_ASSERT(IL_OFFSETX(BAD_IL_OFFSET) > MAX_IL_OFFSET);
C_ASSERT(IL_OFFSETX(ICorDebugInfo::NO_MAPPING) > MAX_IL_OFFSET);
C_ASSERT(IL_OFFSETX(ICorDebugInfo::PROLOG) > MAX_IL_OFFSET);
C_ASSERT(IL_OFFSETX(ICorDebugInfo::EPILOG) > MAX_IL_OFFSET);

//------------------------------------------------------------------------
// jitGetILoffs: Returns the IL offset portion of the IL_OFFSETX type.
//      Asserts if any ICorDebugInfo distinguished value (like ICorDebugInfo::NO_MAPPING)
//      is seen; these are unexpected here. Also asserts if passed BAD_IL_OFFSET.
//
// Arguments:
//    offsx - the IL_OFFSETX value with the IL offset to extract.
//
// Return Value:
//    The IL offset.

IL_OFFSET jitGetILoffs(IL_OFFSETX offsx)
{
    assert(offsx != BAD_IL_OFFSET);

    switch ((int)offsx) // Need the cast since offs is unsigned and the case statements are comparing to signed.
    {
        case ICorDebugInfo::NO_MAPPING:
        case ICorDebugInfo::PROLOG:
        case ICorDebugInfo::EPILOG:
            unreached();

        default:
            return IL_OFFSET(offsx & ~IL_OFFSETX_BITS);
    }
}

//------------------------------------------------------------------------
// jitGetILoffsAny: Similar to jitGetILoffs(), but passes through ICorDebugInfo
//      distinguished values. Asserts if passed BAD_IL_OFFSET.
//
// Arguments:
//    offsx - the IL_OFFSETX value with the IL offset to extract.
//
// Return Value:
//    The IL offset.

IL_OFFSET jitGetILoffsAny(IL_OFFSETX offsx)
{
    assert(offsx != BAD_IL_OFFSET);

    switch ((int)offsx) // Need the cast since offs is unsigned and the case statements are comparing to signed.
    {
        case ICorDebugInfo::NO_MAPPING:
        case ICorDebugInfo::PROLOG:
        case ICorDebugInfo::EPILOG:
            return IL_OFFSET(offsx);

        default:
            return IL_OFFSET(offsx & ~IL_OFFSETX_BITS);
    }
}

//------------------------------------------------------------------------
// jitIsStackEmpty: Does the IL offset have the stack empty bit set?
//      Asserts if passed BAD_IL_OFFSET.
//
// Arguments:
//    offsx - the IL_OFFSETX value to check
//
// Return Value:
//    'true' if the stack empty bit is set; 'false' otherwise.

bool jitIsStackEmpty(IL_OFFSETX offsx)
{
    assert(offsx != BAD_IL_OFFSET);

    switch ((int)offsx) // Need the cast since offs is unsigned and the case statements are comparing to signed.
    {
        case ICorDebugInfo::NO_MAPPING:
        case ICorDebugInfo::PROLOG:
        case ICorDebugInfo::EPILOG:
            return true;

        default:
            return (offsx & IL_OFFSETX_STKBIT) == 0;
    }
}

//------------------------------------------------------------------------
// jitIsCallInstruction: Does the IL offset have the call instruction bit set?
//      Asserts if passed BAD_IL_OFFSET.
//
// Arguments:
//    offsx - the IL_OFFSETX value to check
//
// Return Value:
//    'true' if the call instruction bit is set; 'false' otherwise.

bool jitIsCallInstruction(IL_OFFSETX offsx)
{
    assert(offsx != BAD_IL_OFFSET);

    switch ((int)offsx) // Need the cast since offs is unsigned and the case statements are comparing to signed.
    {
        case ICorDebugInfo::NO_MAPPING:
        case ICorDebugInfo::PROLOG:
        case ICorDebugInfo::EPILOG:
            return false;

        default:
            return (offsx & IL_OFFSETX_CALLINSTRUCTIONBIT) != 0;
    }
}

/*****************************************************************************/

void CodeGen::genEnsureCodeEmitted(IL_OFFSETX offsx)
{
    if (!compiler->opts.compDbgCode)
    {
        return;
    }

    if (offsx == BAD_IL_OFFSET)
    {
        return;
    }

    /* If other IL were offsets reported, skip */

    if (genIPmappingLast == nullptr)
    {
        return;
    }

    if (genIPmappingLast->ipmdILoffsx != offsx)
    {
        return;
    }

    /* offsx was the last reported offset. Make sure that we generated native code */

    if (genIPmappingLast->ipmdNativeLoc.IsCurrentLocation(GetEmitter()))
    {
        instGen(INS_nop);
    }
}

/*****************************************************************************
 *
 *  Shut down the IP-mapping logic, report the info to the EE.
 */

void CodeGen::genIPmappingGen()
{
    if (!compiler->opts.compDbgInfo)
    {
        return;
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In genIPmappingGen()\n");
    }
#endif

    if (genIPmappingList == nullptr)
    {
        eeSetLIcount(0);
        eeSetLIdone();
        return;
    }

    IPmappingDsc*  tmpMapping;
    IPmappingDsc*  prevMapping;
    unsigned       mappingCnt;
    UNATIVE_OFFSET lastNativeOfs;

    /* First count the number of distinct mapping records */

    mappingCnt    = 0;
    lastNativeOfs = UNATIVE_OFFSET(~0);

    for (prevMapping = nullptr, tmpMapping = genIPmappingList; tmpMapping != nullptr; tmpMapping = tmpMapping->ipmdNext)
    {
        IL_OFFSETX srcIP = tmpMapping->ipmdILoffsx;

        // Managed RetVal - since new sequence points are emitted to identify IL calls,
        // make sure that those are not filtered and do not interfere with filtering of
        // other sequence points.
        if (jitIsCallInstruction(srcIP))
        {
            mappingCnt++;
            continue;
        }

        UNATIVE_OFFSET nextNativeOfs = tmpMapping->ipmdNativeLoc.CodeOffset(GetEmitter());

        if (nextNativeOfs != lastNativeOfs)
        {
            mappingCnt++;
            lastNativeOfs = nextNativeOfs;
            prevMapping   = tmpMapping;
            continue;
        }

        /* If there are mappings with the same native offset, then:
           o If one of them is NO_MAPPING, ignore it
           o If one of them is a label, report that and ignore the other one
           o Else report the higher IL offset
         */

        PREFIX_ASSUME(prevMapping != nullptr); // We would exit before if this was true
        if (prevMapping->ipmdILoffsx == (IL_OFFSETX)ICorDebugInfo::NO_MAPPING)
        {
            // If the previous entry was NO_MAPPING, ignore it
            prevMapping->ipmdNativeLoc.Init();
            prevMapping = tmpMapping;
        }
        else if (srcIP == (IL_OFFSETX)ICorDebugInfo::NO_MAPPING)
        {
            // If the current entry is NO_MAPPING, ignore it
            // Leave prevMapping unchanged as tmpMapping is no longer valid
            tmpMapping->ipmdNativeLoc.Init();
        }
        else if (srcIP == (IL_OFFSETX)ICorDebugInfo::EPILOG || srcIP == 0)
        {
            // counting for special cases: see below
            mappingCnt++;
            prevMapping = tmpMapping;
        }
        else
        {
            noway_assert(prevMapping != nullptr);
            noway_assert(!prevMapping->ipmdNativeLoc.Valid() ||
                         lastNativeOfs == prevMapping->ipmdNativeLoc.CodeOffset(GetEmitter()));

            /* The previous block had the same native offset. We have to
               discard one of the mappings. Simply reinitialize ipmdNativeLoc
               and prevMapping will be ignored later. */

            if (prevMapping->ipmdIsLabel)
            {
                // Leave prevMapping unchanged as tmpMapping is no longer valid
                tmpMapping->ipmdNativeLoc.Init();
            }
            else
            {
                prevMapping->ipmdNativeLoc.Init();
                prevMapping = tmpMapping;
            }
        }
    }

    /* Tell them how many mapping records we've got */

    eeSetLIcount(mappingCnt);

    /* Now tell them about the mappings */

    mappingCnt    = 0;
    lastNativeOfs = UNATIVE_OFFSET(~0);

    for (tmpMapping = genIPmappingList; tmpMapping != nullptr; tmpMapping = tmpMapping->ipmdNext)
    {
        // Do we have to skip this record ?
        if (!tmpMapping->ipmdNativeLoc.Valid())
        {
            continue;
        }

        UNATIVE_OFFSET nextNativeOfs = tmpMapping->ipmdNativeLoc.CodeOffset(GetEmitter());
        IL_OFFSETX     srcIP         = tmpMapping->ipmdILoffsx;

        if (jitIsCallInstruction(srcIP))
        {
            eeSetLIinfo(mappingCnt++, nextNativeOfs, jitGetILoffs(srcIP), jitIsStackEmpty(srcIP), true);
        }
        else if (nextNativeOfs != lastNativeOfs)
        {
            eeSetLIinfo(mappingCnt++, nextNativeOfs, jitGetILoffsAny(srcIP), jitIsStackEmpty(srcIP), false);
            lastNativeOfs = nextNativeOfs;
        }
        else if (srcIP == (IL_OFFSETX)ICorDebugInfo::EPILOG || srcIP == 0)
        {
            // For the special case of an IL instruction with no body
            // followed by the epilog (say ret void immediately preceding
            // the method end), we put two entries in, so that we'll stop
            // at the (empty) ret statement if the user tries to put a
            // breakpoint there, and then have the option of seeing the
            // epilog or not based on SetUnmappedStopMask for the stepper.
            eeSetLIinfo(mappingCnt++, nextNativeOfs, jitGetILoffsAny(srcIP), jitIsStackEmpty(srcIP), false);
        }
    }

#if 0
    // TODO-Review:
    //This check is disabled.  It is always true that any time this check asserts, the debugger would have a
    //problem with IL source level debugging.  However, for a C# file, it only matters if things are on
    //different source lines.  As a result, we have all sorts of latent problems with how we emit debug
    //info, but very few actual ones.  Whenever someone wants to tackle that problem in general, turn this
    //assert back on.
    if (compiler->opts.compDbgCode)
    {
        //Assert that the first instruction of every basic block with more than one incoming edge has a
        //different sequence point from each incoming block.
        //
        //It turns out that the only thing we really have to assert is that the first statement in each basic
        //block has an IL offset and appears in eeBoundaries.
        for (BasicBlock* const block : compiler->Blocks())
        {
            Statement* stmt = block->firstStmt();
            if ((block->bbRefs > 1) && (stmt != nullptr))
            {
                bool found = false;
                if (stmt->GetILOffsetX() != BAD_IL_OFFSET)
                {
                    IL_OFFSET ilOffs = jitGetILoffs(stmt->GetILOffsetX());
                    for (unsigned i = 0; i < eeBoundariesCount; ++i)
                    {
                        if (eeBoundaries[i].ilOffset == ilOffs)
                        {
                            found = true;
                            break;
                        }
                    }
                }
                noway_assert(found && "A basic block that is a jump target did not start a new sequence point.");
            }
        }
    }
#endif // 0

    eeSetLIdone();
}

/*============================================================================
 *
 *   These are empty stubs to help the late dis-assembler to compile
 *   if the late disassembler is being built into a non-DEBUG build.
 *
 *============================================================================
 */

#if defined(LATE_DISASM)
#if !defined(DEBUG)

/* virtual */
const char* CodeGen::siRegVarName(size_t offs, size_t size, unsigned reg)
{
    return NULL;
}

/* virtual */
const char* CodeGen::siStackVarName(size_t offs, size_t size, unsigned reg, unsigned stkOffs)
{
    return NULL;
}

/*****************************************************************************/
#endif // !defined(DEBUG)
#endif // defined(LATE_DISASM)
/*****************************************************************************/

void CodeGen::genRetFilt(GenTree* retfilt)
{
    assert(retfilt->OperIs(GT_RETFILT));

    assert((compiler->compCurBB->bbJumpKind == BBJ_EHFILTERRET) ||
           (compiler->compCurBB->bbJumpKind == BBJ_EHFINALLYRET));

    if (retfilt->TypeIs(TYP_VOID))
    {
        return;
    }

    assert(retfilt->TypeIs(TYP_INT));

    GenTree*  src    = retfilt->AsUnOp()->GetOp(0);
    regNumber srcReg = genConsumeReg(src);

    GetEmitter()->emitIns_Mov(INS_mov, EA_4BYTE, REG_INTRET, srcReg, /* canSkip */ true);
}

#ifndef TARGET_64BIT

void CodeGen::genLongReturn(GenTree* src)
{
#ifdef TARGET_X86
    if (src->TypeIs(TYP_DOUBLE))
    {
        regNumber srcReg = genConsumeReg(src);

        GetEmitter()->emitIns_Mov(INS_movd, EA_4BYTE, REG_RAX, srcReg, /* canSkip */ false);
        // TODO-MIKE-Review: This is cheating, the source register is modified without
        // LSRA knowing about it. Shouldn't matter since this is "last use" but you never
        // known...
        // Also, there's a good chance that the value is spilled and we could load the INT
        // registers straight from memory (this happens in UnmanagedCallersOnly methods and
        // there's normally a reverse PInvoke helper call just before return that may result
        // in spilling).
        GetEmitter()->emitIns_R_R_I(INS_shufps, EA_16BYTE, srcReg, srcReg, 0x55);
        GetEmitter()->emitIns_Mov(INS_movd, EA_4BYTE, REG_RDX, srcReg, /* canSkip */ false);

        return;
    }
#endif

    assert(src->OperIs(GT_LONG));

    regNumber srcReg0 = genConsumeReg(src->AsOp()->GetOp(0));
    regNumber srcReg1 = genConsumeReg(src->AsOp()->GetOp(1));

    assert((srcReg0 != REG_NA) && (srcReg1 != REG_NA));

    GetEmitter()->emitIns_Mov(INS_mov, EA_4BYTE, REG_LNGRET_LO, srcReg0, /* canSkip */ true);
    GetEmitter()->emitIns_Mov(INS_mov, EA_4BYTE, REG_LNGRET_HI, srcReg1, /* canSkip */ true);
}

#endif // !TARGET_64BIT

void CodeGen::genReturn(GenTree* ret)
{
    assert(ret->OperIs(GT_RETURN));

    // Normally RETURN nodes appears at the end of RETURN blocks but sometimes the frontend fails
    // to properly cleanup after an unconditional throw and we end up with a THROW block having an
    // unreachable RETURN node at the end.
    assert((compiler->compCurBB->bbJumpKind == BBJ_RETURN) || (compiler->compCurBB->bbJumpKind == BBJ_THROW));
#if defined(FEATURE_EH_FUNCLETS)
    assert(compiler->funCurrentFunc()->funKind == FUNC_ROOT);
#endif

    var_types retType = ret->GetType();

    if (retType == TYP_VOID)
    {
        assert(ret->AsUnOp()->gtOp1 == nullptr);
    }
#ifndef TARGET_64BIT
    else if (retType == TYP_LONG)
    {
        genLongReturn(ret->AsUnOp()->GetOp(0));
    }
#endif
#ifdef TARGET_X86
    else if (varTypeIsFloating(retType))
    {
        genFloatReturn(ret->AsUnOp()->GetOp(0));
    }
#endif
#ifdef TARGET_ARM
    else if (varTypeIsFloating(retType) && (compiler->opts.compUseSoftFP || compiler->info.compIsVarArgs))
    {
        genFloatReturn(ret->AsUnOp()->GetOp(0));
    }
#endif
#ifndef WINDOWS_AMD64_ABI
    else if (compiler->info.retDesc.GetRegCount() > 1)
    {
        genMultiRegStructReturn(ret->AsUnOp()->GetOp(0));
    }
#endif
    else
    {
        assert(!varTypeIsSmall(retType) && (retType != TYP_STRUCT));

        GenTree* src = ret->AsUnOp()->GetOp(0);

        regNumber srcReg = genConsumeReg(src);
        noway_assert(srcReg != REG_NA);

        regNumber retReg = compiler->info.retDesc.GetRegNum(0);

        inst_Mov(retType, retReg, srcReg, /* canSkip */ true, emitTypeSize(retType));
    }

    // Usually the epilog code follow right after the return code and since epilogs
    // aren't interruptuble we don't need to report GC pointers in return registers.
    // But there are all sorts of special cases that need extra code inserted after
    // the RETURN code (GS checks, SP checks, profiler calls, EH NOPs) and such code
    // is interruptible and may have calls and temp labels.

    const ReturnTypeDesc& retDesc = compiler->info.retDesc;

    for (unsigned i = 0; i < retDesc.GetRegCount(); ++i)
    {
        if (varTypeIsGC(retDesc.GetRegType(i)))
        {
            liveness.SetGCRegType(retDesc.GetRegNum(i), retDesc.GetRegType(i));
        }
    }

#ifdef PROFILING_SUPPORTED
    // Reason for not materializing Leave callback as a GT_PROF_HOOK node after GT_RETURN:
    // In flowgraph and other places assert that the last node of a block marked as
    // BBJ_RETURN is either a GT_RETURN or GT_JMP or a tail call.  It would be nice to
    // maintain such an invariant irrespective of whether profiler hook needed or not.
    // Also, there is not much to be gained by materializing it as an explicit node.
    //
    // There should be a single return block while generating profiler ELT callbacks,
    // so we just look for that block to trigger insertion of the profile hook.
    if ((compiler->compCurBB == compiler->genReturnBB) && compiler->compIsProfilerHookNeeded())
    {
        genProfilingLeaveCallback(CORINFO_HELP_PROF_FCN_LEAVE);
    }
#endif // PROFILING_SUPPORTED

#if defined(DEBUG) && defined(TARGET_XARCH)
    if (compiler->lvaReturnSpCheck != BAD_VAR_NUM)
    {
        genStackPointerCheck(compiler->lvaReturnSpCheck);
    }
#endif // defined(DEBUG) && defined(TARGET_XARCH)
}

#ifndef WINDOWS_AMD64_ABI

void CodeGen::genMultiRegStructReturn(GenTree* src)
{
    const ReturnTypeDesc& retDesc = compiler->info.retDesc;

    assert(retDesc.GetRegCount() > 1);

    if (GenTreeFieldList* list = src->IsFieldList())
    {
        unsigned regIndex = 0;

        for (GenTreeFieldList::Use& use : list->Uses())
        {
            // TODO-MIKE-Review: Is this correct? Shouldn't we first "consume" all the regs
            // and then move as necessary? This is how calls do it but calls have PUTARG_REG
            // nodes so that register constraints are placed on defs rather than uses.
            // We could use PUTARG_REG in this case but it would be good to find at least a
            // case where the current implementation fails so we know there's some kind of
            // coverage for this.
            // Keep it as is for until non-SIMD multi reg struct returns are changed to also
            // use FIELD_LIST.

            regNumber srcReg = UseReg(use.GetNode());
            regNumber retReg = retDesc.GetRegNum(regIndex++);

            if (srcReg != retReg)
            {
                GetEmitter()->emitIns_Mov(ins_Copy(use.GetType()), emitActualTypeSize(use.GetType()), retReg, srcReg,
                                          /* canSkip */ true);
            }
        }

        return;
    }

    UseRegs(src);

    GenTreeCall* call = src->gtSkipReloadOrCopy()->AsCall();

    assert(call->GetRegCount() == retDesc.GetRegCount());

    for (unsigned i = 0; i < retDesc.GetRegCount(); ++i)
    {
        regNumber srcReg  = src->GetRegNum(i);
        var_types retType = retDesc.GetRegType(i);
        regNumber retReg  = retDesc.GetRegNum(i);

        inst_Mov(retType, retReg, srcReg, /* canSkip */ true);
    }
}

#endif // !WINDOWS_AMD64_ABI

#ifndef TARGET_64BIT

void CodeGen::GenStoreLclVarLong(GenTreeLclVar* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR) && store->TypeIs(TYP_LONG));
    assert(compiler->lvaGetDesc(store)->TypeIs(TYP_LONG) && !compiler->lvaGetDesc(store)->IsIndependentPromoted());

    GenTree*  src = store->GetOp(0);
    regNumber srcRegs[2];

    if (src->OperIs(GT_LONG))
    {
        assert(src->isContained());

        srcRegs[0] = UseReg(src->AsOp()->GetOp(0));
        srcRegs[1] = UseReg(src->AsOp()->GetOp(1));
    }
    else
    {
        srcRegs[0] = UseReg(src, 0);
        srcRegs[1] = UseReg(src, 1);
    }

    GetEmitter()->emitIns_S_R(ins_Store(TYP_INT), EA_4BYTE, srcRegs[0], store->GetLclNum(), 0);
    GetEmitter()->emitIns_S_R(ins_Store(TYP_INT), EA_4BYTE, srcRegs[1], store->GetLclNum(), 4);

    genUpdateLife(store);
}

#endif

void CodeGen::GenStoreLclVarMultiReg(GenTreeLclVar* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR) && store->IsMultiReg());
    // Store spilling is achieved by not assigning a register to the node.
    assert(!store->IsAnyRegSpill());

    GenTree* src = store->GetOp(0);
    assert(src->IsMultiRegNode());

    LclVarDsc* lcl = compiler->lvaGetDesc(store);
    assert(lcl->IsIndependentPromoted() && !lcl->IsRegCandidate());

    unsigned regCount = lcl->GetPromotedFieldCount();
    assert(regCount == src->GetMultiRegCount(compiler));

    GenTree* value   = src->gtSkipReloadOrCopy();
    bool     hasRegs = false;

    for (unsigned i = 0; i < regCount; ++i)
    {
        regNumber  srcReg      = UseReg(src, i);
        var_types  srcType     = value->GetMultiRegType(compiler, i);
        unsigned   fieldLclNum = lcl->GetPromotedFieldLclNum(i);
        LclVarDsc* fieldLcl    = compiler->lvaGetDesc(fieldLclNum);
        var_types  fieldType   = fieldLcl->TypeGet();
        regNumber  fieldReg    = store->GetRegNum(i);

        if (fieldReg != REG_NA)
        {
            hasRegs = true;
            inst_Mov(fieldType, fieldReg, srcReg, /* canSkip */ true);

            if (!store->IsLastUse(i))
            {
                liveness.SetGCRegType(fieldReg, fieldType);
            }
        }
        else
        {
            fieldReg = REG_STK;
        }

        if (!store->IsLastUse(i) && ((fieldReg == REG_STK) || fieldLcl->IsAlwaysAliveInMemory()))
        {
            instruction ins = ins_StoreFromSrc(srcReg, fieldType);
            GetEmitter()->emitIns_S_R(ins, emitTypeSize(fieldType), srcReg, fieldLclNum, 0);
        }

        fieldLcl->SetRegNum(fieldReg);
    }

    liveness.UpdateLifeMultiReg(this, store);
}

#if defined(DEBUG) && defined(TARGET_XARCH)

//------------------------------------------------------------------------
// genStackPointerCheck: Generate code to check the stack pointer against a saved value.
// This is a debug check.
//
// Arguments:
//    lvaStackPointerVar  - The local variable number that holds the value of the stack pointer
//                          we are comparing against.
//
void CodeGen::genStackPointerCheck(unsigned lvaStackPointerVar)
{
    LclVarDsc* lcl = compiler->lvaGetDesc(lvaStackPointerVar);
    assert(lcl->lvOnFrame && lcl->lvDoNotEnregister);

    GetEmitter()->emitIns_S_R(INS_cmp, EA_PTRSIZE, REG_SPBASE, lvaStackPointerVar, 0);

    BasicBlock* sp_check = genCreateTempLabel();
    GetEmitter()->emitIns_J(INS_je, sp_check);
    instGen(INS_BREAKPOINT);
    genDefineTempLabel(sp_check);
}

#endif // defined(DEBUG) && defined(TARGET_XARCH)

#if !FEATURE_FIXED_OUT_ARGS
unsigned CodeGenInterface::getCurrentStackLevel() const
{
    return genStackLevel;
}
#endif

#ifdef USING_VARIABLE_LIVE_RANGE
#ifdef DEBUG
//------------------------------------------------------------------------
//                      VariableLiveRanges dumpers
//------------------------------------------------------------------------

// Dump "VariableLiveRange" when code has not been generated and we don't have so the assembly native offset
// but at least "emitLocation"s and "siVarLoc"
void CodeGenInterface::VariableLiveKeeper::VariableLiveRange::dumpVariableLiveRange(
    const CodeGenInterface* codeGen) const
{
    codeGen->dumpSiVarLoc(&m_VarLocation);

    printf(" [");
    m_StartEmitLocation.Print(codeGen->GetCompiler()->compMethodID);
    printf(", ");
    if (m_EndEmitLocation.Valid())
    {
        m_EndEmitLocation.Print(codeGen->GetCompiler()->compMethodID);
    }
    else
    {
        printf("...");
    }
    printf("]");
}

// Dump "VariableLiveRange" when code has been generated and we have the assembly native offset of each "emitLocation"
void CodeGenInterface::VariableLiveKeeper::VariableLiveRange::dumpVariableLiveRange(
    emitter* emit, const CodeGenInterface* codeGen) const
{
    assert(emit != nullptr);

    // "VariableLiveRanges" are created setting its location ("m_VarLocation") and the initial native offset
    // ("m_StartEmitLocation")
    codeGen->dumpSiVarLoc(&m_VarLocation);

    // If this is an open "VariableLiveRange", "m_EndEmitLocation" is non-valid and print -1
    UNATIVE_OFFSET endAssemblyOffset = m_EndEmitLocation.Valid() ? m_EndEmitLocation.CodeOffset(emit) : -1;

    printf(" [%X, %X)", m_StartEmitLocation.CodeOffset(emit), m_EndEmitLocation.CodeOffset(emit));
}

//------------------------------------------------------------------------
//                      LiveRangeDumper
//------------------------------------------------------------------------
//------------------------------------------------------------------------
// resetDumper: If the the "liveRange" has its last "VariableLiveRange" closed, it makes
//  the "LiveRangeDumper" points to end of "liveRange" (nullptr). In other case,
//  it makes the "LiveRangeDumper" points to the last "VariableLiveRange" of
//  "liveRange", which is opened.
//
// Arguments:
//  liveRanges - the "LiveRangeList" of the "VariableLiveDescriptor" we want to
//      udpate its "LiveRangeDumper".
//
// Notes:
//  This method is expected to be called once a the code for a BasicBlock has been
//  generated and all the new "VariableLiveRange"s of the variable during this block
//  has been dumped.
void CodeGenInterface::VariableLiveKeeper::LiveRangeDumper::resetDumper(const LiveRangeList* liveRanges)
{
    // There must have reported something in order to reset
    assert(m_hasLiveRangestoDump);

    if (liveRanges->back().m_EndEmitLocation.Valid())
    {
        // the last "VariableLiveRange" is closed and the variable
        // is no longer alive
        m_hasLiveRangestoDump = false;
    }
    else
    {
        // the last "VariableLiveRange" remains opened because it is
        // live at "BasicBlock"s "bbLiveOut".
        m_StartingLiveRange = liveRanges->backPosition();
    }
}

//------------------------------------------------------------------------
// setDumperStartAt: Make "LiveRangeDumper" instance points the last "VariableLiveRange"
// added so we can starts dumping from there after the actual "BasicBlock"s code is generated.
//
// Arguments:
//  liveRangeIt - an iterator to a position in "VariableLiveDescriptor::m_VariableLiveRanges"
//
// Return Value:
//  A const pointer to the "LiveRangeList" containing all the "VariableLiveRange"s
//  of the variable with index "varNum".
//
// Notes:
//  "varNum" should be always a valid inde ("varnum" < "m_LiveDscCount")
void CodeGenInterface::VariableLiveKeeper::LiveRangeDumper::setDumperStartAt(const LiveRangeListIterator liveRangeIt)
{
    m_hasLiveRangestoDump = true;
    m_StartingLiveRange   = liveRangeIt;
}

//------------------------------------------------------------------------
// getStartForDump: Return an iterator to the first "VariableLiveRange" edited/added
//  during the current "BasicBlock"
//
// Return Value:
//  A LiveRangeListIterator to the first "VariableLiveRange" in "LiveRangeList" which
//  was used during last "BasicBlock".
//
CodeGenInterface::VariableLiveKeeper::LiveRangeListIterator CodeGenInterface::VariableLiveKeeper::LiveRangeDumper::
    getStartForDump() const
{
    return m_StartingLiveRange;
}

//------------------------------------------------------------------------
// hasLiveRangesToDump: Retutn wheter at least a "VariableLiveRange" was alive during
//  the current "BasicBlock"'s code generation
//
// Return Value:
//  A boolean indicating indicating if there is at least a "VariableLiveRange"
//  that has been used for the variable during last "BasicBlock".
//
bool CodeGenInterface::VariableLiveKeeper::LiveRangeDumper::hasLiveRangesToDump() const
{
    return m_hasLiveRangestoDump;
}
#endif // DEBUG

//------------------------------------------------------------------------
//                      VariableLiveDescriptor
//------------------------------------------------------------------------

CodeGenInterface::VariableLiveKeeper::VariableLiveDescriptor::VariableLiveDescriptor(CompAllocator allocator)
{
    // Initialize an empty list
    m_VariableLiveRanges = new (allocator) LiveRangeList(allocator);

    INDEBUG(m_VariableLifeBarrier = new (allocator) LiveRangeDumper(m_VariableLiveRanges));
}

//------------------------------------------------------------------------
// hasVariableLiveRangeOpen: Return true if the variable is still alive,
//  false in other case.
//
bool CodeGenInterface::VariableLiveKeeper::VariableLiveDescriptor::hasVariableLiveRangeOpen() const
{
    return !m_VariableLiveRanges->empty() && !m_VariableLiveRanges->back().m_EndEmitLocation.Valid();
}

//------------------------------------------------------------------------
// getLiveRanges: Return the list of variable locations for this variable.
//
// Return Value:
//  A const LiveRangeList* pointing to the first variable location if it has
//  any or the end of the list in other case.
//
CodeGenInterface::VariableLiveKeeper::LiveRangeList* CodeGenInterface::VariableLiveKeeper::VariableLiveDescriptor::
    getLiveRanges() const
{
    return m_VariableLiveRanges;
}

//------------------------------------------------------------------------
// startLiveRangeFromEmitter: Report this variable as being born in "varLocation"
//  since the instruction where "emit" is located.
//
// Arguments:
//  varLocation  - the home of the variable.
//  emit - an emitter* instance located at the first instruction from
//  where "varLocation" becomes valid.
//
// Assumptions:
//  This variable is being born so it should be dead.
//
// Notes:
//  The position of "emit" matters to ensure intervals inclusive of the
//  beginning and exclusive of the end.
//
void CodeGenInterface::VariableLiveKeeper::VariableLiveDescriptor::startLiveRangeFromEmitter(
    CodeGenInterface::siVarLoc varLocation, emitter* emit) const
{
    noway_assert(emit != nullptr);

    // Is the first "VariableLiveRange" or the previous one has been closed so its "m_EndEmitLocation" is valid
    noway_assert(m_VariableLiveRanges->empty() || m_VariableLiveRanges->back().m_EndEmitLocation.Valid());

    if (!m_VariableLiveRanges->empty() &&
        siVarLoc::Equals(&varLocation, &(m_VariableLiveRanges->back().m_VarLocation)) &&
        m_VariableLiveRanges->back().m_EndEmitLocation.IsPreviousInsNum(emit))
    {
        JITDUMP("Extending debug range...\n");

        // The variable is being born just after the instruction at which it died.
        // In this case, i.e. an update of the variable's value, we coalesce the live ranges.
        m_VariableLiveRanges->back().m_EndEmitLocation.Init();
    }
    else
    {
        JITDUMP("New debug range: %s\n",
                m_VariableLiveRanges->empty()
                    ? "first"
                    : siVarLoc::Equals(&varLocation, &(m_VariableLiveRanges->back().m_VarLocation))
                          ? "new var or location"
                          : "not adjacent");
        // Creates new live range with invalid end
        m_VariableLiveRanges->emplace_back(varLocation, emitLocation(), emitLocation());
        m_VariableLiveRanges->back().m_StartEmitLocation.CaptureLocation(emit);
    }

#ifdef DEBUG
    if (!m_VariableLifeBarrier->hasLiveRangesToDump())
    {
        m_VariableLifeBarrier->setDumperStartAt(m_VariableLiveRanges->backPosition());
    }
#endif // DEBUG

    // startEmitLocationendEmitLocation has to be Valid and endEmitLocationendEmitLocation  not
    noway_assert(m_VariableLiveRanges->back().m_StartEmitLocation.Valid());
    noway_assert(!m_VariableLiveRanges->back().m_EndEmitLocation.Valid());
}

//------------------------------------------------------------------------
// endLiveRangeAtEmitter: Report this variable as becoming dead since the
//  instruction where "emit" is located.
//
// Arguments:
//  emit - an emitter* instance located at the first instruction from
//   this variable becomes dead.
//
// Assumptions:
//  This variable is becoming dead so it should be alive.
//
// Notes:
//  The position of "emit" matters to ensure intervals inclusive of the
//  beginning and exclusive of the end.
//
void CodeGenInterface::VariableLiveKeeper::VariableLiveDescriptor::endLiveRangeAtEmitter(emitter* emit) const
{
    noway_assert(emit != nullptr);
    noway_assert(hasVariableLiveRangeOpen());

    // Using [close, open) ranges so as to not compute the size of the last instruction
    m_VariableLiveRanges->back().m_EndEmitLocation.CaptureLocation(emit);

    // No m_EndEmitLocation has to be Valid
    noway_assert(m_VariableLiveRanges->back().m_EndEmitLocation.Valid());
}

//------------------------------------------------------------------------
// UpdateLiveRangeAtEmitter: Report this variable as changing its variable
//  home to "varLocation" since the instruction where "emit" is located.
//
// Arguments:
//  varLocation  - the new variable location.
//  emit - an emitter* instance located at the first instruction from
//   where "varLocation" becomes valid.
//
// Assumptions:
//  This variable is being born so it should be dead.
//
// Notes:
//  The position of "emit" matters to ensure intervals inclusive of the
//  beginning and exclusive of the end.
//
void CodeGenInterface::VariableLiveKeeper::VariableLiveDescriptor::updateLiveRangeAtEmitter(
    CodeGenInterface::siVarLoc varLocation, emitter* emit) const
{
    // This variable is changing home so it has been started before during this block
    noway_assert(m_VariableLiveRanges != nullptr && !m_VariableLiveRanges->empty());

    // And its last m_EndEmitLocation has to be invalid
    noway_assert(!m_VariableLiveRanges->back().m_EndEmitLocation.Valid());

    // If we are reporting again the same home, that means we are doing something twice?
    // noway_assert(! CodeGenInterface::siVarLoc::Equals(&m_VariableLiveRanges->back().m_VarLocation, varLocation));

    // Close previous live range
    endLiveRangeAtEmitter(emit);

    startLiveRangeFromEmitter(varLocation, emit);
}

#ifdef DEBUG
void CodeGenInterface::VariableLiveKeeper::VariableLiveDescriptor::dumpAllRegisterLiveRangesForBlock(
    emitter* emit, const CodeGenInterface* codeGen) const
{
    bool first = true;
    for (LiveRangeListIterator it = m_VariableLiveRanges->begin(); it != m_VariableLiveRanges->end(); it++)
    {
        if (!first)
        {
            printf("; ");
        }
        it->dumpVariableLiveRange(emit, codeGen);
        first = false;
    }
}

void CodeGenInterface::VariableLiveKeeper::VariableLiveDescriptor::dumpRegisterLiveRangesForBlockBeforeCodeGenerated(
    const CodeGenInterface* codeGen) const
{
    bool first = true;
    for (LiveRangeListIterator it = m_VariableLifeBarrier->getStartForDump(); it != m_VariableLiveRanges->end(); it++)
    {
        if (!first)
        {
            printf("; ");
        }
        it->dumpVariableLiveRange(codeGen);
        first = false;
    }
}

// Returns true if a live range for this variable has been recorded
bool CodeGenInterface::VariableLiveKeeper::VariableLiveDescriptor::hasVarLiveRangesToDump() const
{
    return !m_VariableLiveRanges->empty();
}

// Returns true if a live range for this variable has been recorded from last call to EndBlock
bool CodeGenInterface::VariableLiveKeeper::VariableLiveDescriptor::hasVarLiveRangesFromLastBlockToDump() const
{
    return m_VariableLifeBarrier->hasLiveRangesToDump();
}

// Reset the barrier so as to dump only next block changes on next block
void CodeGenInterface::VariableLiveKeeper::VariableLiveDescriptor::endBlockLiveRanges()
{
    // make "m_VariableLifeBarrier->m_StartingLiveRange" now points to nullptr for printing purposes
    m_VariableLifeBarrier->resetDumper(m_VariableLiveRanges);
}
#endif // DEBUG

//------------------------------------------------------------------------
//                      VariableLiveKeeper
//------------------------------------------------------------------------
// Initialize structures for VariableLiveRanges
void CodeGenInterface::initializeVariableLiveKeeper()
{
    CompAllocator allocator = compiler->getAllocator(CMK_VariableLiveRanges);

    int amountTrackedVariables = compiler->opts.compDbgInfo ? compiler->info.compLocalsCount : 0;
    int amountTrackedArgs      = compiler->opts.compDbgInfo ? compiler->info.compArgsCount : 0;

    varLiveKeeper = new (allocator) VariableLiveKeeper(amountTrackedVariables, amountTrackedArgs, compiler, allocator);
}

CodeGenInterface::VariableLiveKeeper* CodeGenInterface::getVariableLiveKeeper() const
{
    return varLiveKeeper;
};

//------------------------------------------------------------------------
// VariableLiveKeeper: Create an instance of the object in charge of managing
//  VariableLiveRanges and intialize the array "m_vlrLiveDsc".
//
// Arguments:
//    totalLocalCount   - the count of args, special args and IL Local
//      variables in the method.
//    argsCount         - the count of args and special args in the method.
//    compiler          - a compiler instance
//
CodeGenInterface::VariableLiveKeeper::VariableLiveKeeper(unsigned int  totalLocalCount,
                                                         unsigned int  argsCount,
                                                         Compiler*     comp,
                                                         CompAllocator allocator)
    : m_LiveDscCount(totalLocalCount)
    , m_LiveArgsCount(argsCount)
    , m_Compiler(comp)
    , m_LastBasicBlockHasBeenEmited(false)
{
    if (m_LiveDscCount > 0)
    {
        // Allocate memory for "m_vlrLiveDsc" and initialize each "VariableLiveDescriptor"
        m_vlrLiveDsc          = allocator.allocate<VariableLiveDescriptor>(m_LiveDscCount);
        m_vlrLiveDscForProlog = allocator.allocate<VariableLiveDescriptor>(m_LiveDscCount);

        for (unsigned int varNum = 0; varNum < m_LiveDscCount; varNum++)
        {
            new (m_vlrLiveDsc + varNum) VariableLiveDescriptor(allocator);
            new (m_vlrLiveDscForProlog + varNum) VariableLiveDescriptor(allocator);
        }
    }
}

//------------------------------------------------------------------------
// siStartOrCloseVariableLiveRange: Reports the given variable as beign born
//  or becoming dead.
//
// Arguments:
//    varDsc    - the variable for which a location changed will be reported
//    varNum    - the index of the variable in the "compiler->lvaTable"
//    isBorn    - whether the variable is being born from where the emitter is located.
//    isDying   - whether the variable is dying from where the emitter is located.
//
// Assumptions:
//    The emitter should be located on the first instruction from where is true that
//    the variable becoming valid (when isBorn is true) or invalid (when isDying is true).
//
// Notes:
//    This method is being called when the variable is being born,
//    becoming dead, or both.
//
void CodeGenInterface::VariableLiveKeeper::siStartOrCloseVariableLiveRange(const LclVarDsc* varDsc,
                                                                           unsigned int     varNum,
                                                                           bool             isBorn,
                                                                           bool             isDying)
{
    noway_assert(varDsc != nullptr);

    // Only the variables that exists in the IL, "this", and special arguments
    // are reported.
    if (m_Compiler->opts.compDbgInfo && varNum < m_LiveDscCount)
    {
        if (isBorn && !isDying)
        {
            // "varDsc" is valid from this point
            siStartVariableLiveRange(varDsc, varNum);
        }
        if (isDying && !isBorn)
        {
            // this variable live range is no longer valid from this point
            siEndVariableLiveRange(varNum);
        }
    }
}

//------------------------------------------------------------------------
// siStartVariableLiveRange: Reports the given variable as being born.
//
// Arguments:
//    varDsc    - the variable for which a location changed will be reported
//    varNum    - the index of the variable to report home in lvLiveDsc
//
// Assumptions:
//    The emitter should be pointing to the first instruction from where the VariableLiveRange is
//    becoming valid.
//    The given "varDsc" should have its VariableRangeLists initialized.
//
// Notes:
//    This method should be called on every place a Variable is becoming alive.
void CodeGenInterface::VariableLiveKeeper::siStartVariableLiveRange(const LclVarDsc* varDsc, unsigned int varNum)
{
    noway_assert(varDsc != nullptr);

    // Only the variables that exists in the IL, "this", and special arguments
    // are reported.
    if (m_Compiler->opts.compDbgInfo && varNum < m_LiveDscCount)
    {
        // Build siVarLoc for this born "varDsc"
        CodeGenInterface::siVarLoc varLocation =
            m_Compiler->codeGen->getSiVarLoc(varDsc
#if !FEATURE_FIXED_OUT_ARGS
                                             ,
                                             m_Compiler->codeGen->getCurrentStackLevel()
#endif
                                                 );

        VariableLiveDescriptor* varLiveDsc = &m_vlrLiveDsc[varNum];
        // this variable live range is valid from this point
        varLiveDsc->startLiveRangeFromEmitter(varLocation, m_Compiler->GetEmitter());
    }
}

//------------------------------------------------------------------------
// siEndVariableLiveRange: Reports the variable as becoming dead.
//
// Arguments:
//    varNum    - the index of the variable at m_vlrLiveDsc or lvaTable in that
//       is becoming dead.
//
// Assumptions:
//    The given variable should be alive.
//    The emitter should be pointing to the first instruction from where the VariableLiveRange is
//    becoming invalid.
//
// Notes:
//    This method should be called on every place a Variable is becoming dead.
void CodeGenInterface::VariableLiveKeeper::siEndVariableLiveRange(unsigned int varNum)
{
    // Only the variables that exists in the IL, "this", and special arguments
    // will be reported.

    // This method is being called from genUpdateLife, and that one is called after
    // code for BasicBlock have been generated, but the emitter has no longer
    // a valid IG so we don't report the close of a "VariableLiveRange" after code is
    // emitted.

    if (m_Compiler->opts.compDbgInfo && varNum < m_LiveDscCount && !m_LastBasicBlockHasBeenEmited)
    {
        // this variable live range is no longer valid from this point
        m_vlrLiveDsc[varNum].endLiveRangeAtEmitter(m_Compiler->GetEmitter());
    }
}

//------------------------------------------------------------------------
// siUpdateVariableLiveRange: Reports the change of variable location for the
//  given variable.
//
// Arguments:
//    varDsc    - the variable for which tis home has changed.
//    varNum    - the index of the variable to report home in lvLiveDsc
//
// Assumptions:
//    The given variable should be alive.
//    The emitter should be pointing to the first instruction from where
//    the new variable location is becoming valid.
//
void CodeGenInterface::VariableLiveKeeper::siUpdateVariableLiveRange(const LclVarDsc* varDsc, unsigned int varNum)
{
    noway_assert(varDsc != nullptr);

    // Only the variables that exists in the IL, "this", and special arguments
    // will be reported. This are locals and arguments, and are counted in
    // "info.compLocalsCount".

    // This method is being called when the prolog is being generated, and
    // the emitter has no longer a valid IG so we don't report the close of
    //  a "VariableLiveRange" after code is emitted.
    if (m_Compiler->opts.compDbgInfo && varNum < m_LiveDscCount && !m_LastBasicBlockHasBeenEmited)
    {
        // Build the location of the variable
        CodeGenInterface::siVarLoc siVarLoc =
            m_Compiler->codeGen->getSiVarLoc(varDsc
#if !FEATURE_FIXED_OUT_ARGS
                                             ,
                                             m_Compiler->codeGen->getCurrentStackLevel()
#endif
                                                 );

        // Report the home change for this variable
        VariableLiveDescriptor* varLiveDsc = &m_vlrLiveDsc[varNum];
        varLiveDsc->updateLiveRangeAtEmitter(siVarLoc, m_Compiler->GetEmitter());
    }
}

//------------------------------------------------------------------------
// siEndAllVariableLiveRange: Reports the set of variables as becoming dead.
//
// Arguments:
//    newLife    - the set of variables that are becoming dead.
//
// Assumptions:
//    All the variables in the set are alive.
//
// Notes:
//    This method is called when the last block being generated to killed all
//    the live variables and set a flag to avoid reporting variable locations for
//    on next calls to method that update variable liveness.
void CodeGenInterface::VariableLiveKeeper::siEndAllVariableLiveRange(VARSET_VALARG_TP varsToClose)
{
    if (m_Compiler->opts.compDbgInfo)
    {
        if (m_Compiler->lvaTrackedCount > 0 || !m_Compiler->opts.OptimizationDisabled())
        {
            VarSetOps::Iter iter(m_Compiler, varsToClose);
            unsigned        varIndex = 0;
            while (iter.NextElem(&varIndex))
            {
                unsigned int varNum = m_Compiler->lvaTrackedIndexToLclNum(varIndex);
                siEndVariableLiveRange(varNum);
            }
        }
        else
        {
            // It seems we are jitting debug code, so we don't have variable
            //  liveness info
            siEndAllVariableLiveRange();
        }
    }

    m_LastBasicBlockHasBeenEmited = true;
}

//------------------------------------------------------------------------
// siEndAllVariableLiveRange: Reports all live variables as dead.
//
// Notes:
//    This overload exists for the case we are jitting code compiled in
//    debug mode. When that happen we don't have variable liveness info
//    as "BaiscBlock::bbLiveIn" or "BaiscBlock::bbLiveOut" and there is no
//    tracked variable.
//
void CodeGenInterface::VariableLiveKeeper::siEndAllVariableLiveRange()
{
    // TODO: we can improve this keeping a set for the variables with
    // open VariableLiveRanges

    for (unsigned int varNum = 0; varNum < m_LiveDscCount; varNum++)
    {
        const VariableLiveDescriptor* varLiveDsc = m_vlrLiveDsc + varNum;
        if (varLiveDsc->hasVariableLiveRangeOpen())
        {
            siEndVariableLiveRange(varNum);
        }
    }
}

//------------------------------------------------------------------------
// getLiveRangesForVarForBody: Return the "VariableLiveRange" that correspond to
//  the given "varNum".
//
// Arguments:
//  varNum  - the index of the variable in m_vlrLiveDsc, which is the same as
//      in lvaTable.
//
// Return Value:
//  A const pointer to the list of variable locations reported for the variable.
//
// Assumptions:
//  This variable should be an argument, a special argument or an IL local
//  variable.
CodeGenInterface::VariableLiveKeeper::LiveRangeList* CodeGenInterface::VariableLiveKeeper::getLiveRangesForVarForBody(
    unsigned int varNum) const
{
    // There should be at least one variable for which its liveness is tracked
    noway_assert(varNum < m_LiveDscCount);

    return m_vlrLiveDsc[varNum].getLiveRanges();
}

//------------------------------------------------------------------------
// getLiveRangesForVarForProlog: Return the "VariableLiveRange" that correspond to
//  the given "varNum".
//
// Arguments:
//  varNum  - the index of the variable in m_vlrLiveDsc, which is the same as
//      in lvaTable.
//
// Return Value:
//  A const pointer to the list of variable locations reported for the variable.
//
// Assumptions:
//  This variable should be an argument, a special argument or an IL local
//  variable.
CodeGenInterface::VariableLiveKeeper::LiveRangeList* CodeGenInterface::VariableLiveKeeper::getLiveRangesForVarForProlog(
    unsigned int varNum) const
{
    // There should be at least one variable for which its liveness is tracked
    noway_assert(varNum < m_LiveDscCount);

    return m_vlrLiveDscForProlog[varNum].getLiveRanges();
}

//------------------------------------------------------------------------
// getLiveRangesCount: Returns the count of variable locations reported for the tracked
//  variables, which are arguments, special arguments, and local IL variables.
//
// Return Value:
//    size_t - the count of variable locations
//
// Notes:
//    This method is being called from "genSetScopeInfo" to know the count of
//    "varResultInfo" that should be created on eeSetLVcount.
//
size_t CodeGenInterface::VariableLiveKeeper::getLiveRangesCount() const
{
    size_t liveRangesCount = 0;

    if (m_Compiler->opts.compDbgInfo)
    {
        for (unsigned int varNum = 0; varNum < m_LiveDscCount; varNum++)
        {
            for (int i = 0; i < 2; i++)
            {
                VariableLiveDescriptor* varLiveDsc = (i == 0 ? m_vlrLiveDscForProlog : m_vlrLiveDsc) + varNum;

                if (m_Compiler->compMap2ILvarNum(varNum) != (unsigned int)ICorDebugInfo::UNKNOWN_ILNUM)
                {
                    liveRangesCount += varLiveDsc->getLiveRanges()->size();
                }
            }
        }
    }
    return liveRangesCount;
}

//------------------------------------------------------------------------
// psiStartVariableLiveRange: Reports the given variable as being born.
//
// Arguments:
//  varLcation  - the variable location
//  varNum      - the index of the variable in "compiler->lvaTable" or
//      "VariableLivekeeper->m_vlrLiveDsc"
//
// Notes:
//  This function is expected to be called from "psiBegProlog" during
//  prolog code generation.
//
void CodeGenInterface::VariableLiveKeeper::psiStartVariableLiveRange(CodeGenInterface::siVarLoc varLocation,
                                                                     unsigned int               varNum)
{
    // This descriptor has to correspond to a parameter. The first slots in lvaTable
    // are arguments and special arguments.
    noway_assert(varNum < m_LiveArgsCount);

    VariableLiveDescriptor* varLiveDsc = &m_vlrLiveDscForProlog[varNum];
    varLiveDsc->startLiveRangeFromEmitter(varLocation, m_Compiler->GetEmitter());
}

//------------------------------------------------------------------------
// psiClosePrologVariableRanges: Report all the parameters as becoming dead.
//
// Notes:
//  This function is expected to be called from preffix "psiEndProlog" after
//  code for prolog has been generated.
//
void CodeGenInterface::VariableLiveKeeper::psiClosePrologVariableRanges()
{
    noway_assert(m_LiveArgsCount <= m_LiveDscCount);

    for (unsigned int varNum = 0; varNum < m_LiveArgsCount; varNum++)
    {
        VariableLiveDescriptor* varLiveDsc = m_vlrLiveDscForProlog + varNum;

        if (varLiveDsc->hasVariableLiveRangeOpen())
        {
            varLiveDsc->endLiveRangeAtEmitter(m_Compiler->GetEmitter());
        }
    }
}

#ifdef DEBUG
void CodeGenInterface::VariableLiveKeeper::dumpBlockVariableLiveRanges(const BasicBlock* block)
{
    assert(block != nullptr);

    bool hasDumpedHistory = false;

    printf("\nVariable Live Range History Dump for " FMT_BB "\n", block->bbNum);

    if (m_Compiler->opts.compDbgInfo)
    {
        for (unsigned int varNum = 0; varNum < m_LiveDscCount; varNum++)
        {
            VariableLiveDescriptor* varLiveDsc = m_vlrLiveDsc + varNum;

            if (varLiveDsc->hasVarLiveRangesFromLastBlockToDump())
            {
                hasDumpedHistory = true;
                m_Compiler->gtDispLclVar(varNum, false);
                printf(": ");
                varLiveDsc->dumpRegisterLiveRangesForBlockBeforeCodeGenerated(m_Compiler->codeGen);
                varLiveDsc->endBlockLiveRanges();
                printf("\n");
            }
        }
    }

    if (!hasDumpedHistory)
    {
        printf("..None..\n");
    }
}

void CodeGenInterface::VariableLiveKeeper::dumpLvaVariableLiveRanges() const
{
    bool hasDumpedHistory = false;

    printf("VARIABLE LIVE RANGES:\n");

    if (m_Compiler->opts.compDbgInfo)
    {
        for (unsigned int varNum = 0; varNum < m_LiveDscCount; varNum++)
        {
            VariableLiveDescriptor* varLiveDsc = m_vlrLiveDsc + varNum;

            if (varLiveDsc->hasVarLiveRangesToDump())
            {
                hasDumpedHistory = true;
                m_Compiler->gtDispLclVar(varNum, false);
                printf(": ");
                varLiveDsc->dumpAllRegisterLiveRangesForBlock(m_Compiler->GetEmitter(), m_Compiler->codeGen);
                printf("\n");
            }
        }
    }

    if (!hasDumpedHistory)
    {
        printf("..None..\n");
    }
}
#endif // DEBUG
#endif // USING_VARIABLE_LIVE_RANGE

//-----------------------------------------------------------------------------
// genPoisonFrame: Generate code that places a recognizable value into address exposed variables.
//
// Remarks:
//   This function emits code to poison address exposed non-zero-inited local variables. We expect this function
//   to be called when emitting code for the scratch BB that comes right after the prolog.
//   The variables are poisoned using 0xcdcdcdcd.
void CodeGen::genPoisonFrame(regMaskTP regLiveIn)
{
    assert(compiler->compShouldPoisonFrame());
    assert((regLiveIn & genRegMask(REG_SCRATCH)) == 0);

    // The first time we need to poison something we will initialize a register to the largest immediate cccccccc that
    // we can fit.
    regNumber immReg = REG_NA;
#ifdef TARGET_64BIT
    const ssize_t imm = 0xcdcdcdcdcdcdcdcd;
#else
    const int imm = 0xcdcdcdcd;
#endif

    for (unsigned varNum = 0; varNum < compiler->info.compLocalsCount; varNum++)
    {
        LclVarDsc* varDsc = compiler->lvaGetDesc(varNum);

        if (varDsc->IsParam() || varDsc->lvMustInit || !varDsc->IsAddressExposed())
        {
            continue;
        }

        assert(varDsc->lvOnFrame);

        if (immReg == REG_NA)
        {
            immReg = REG_SCRATCH;

#ifdef TARGET_XARCH
            GetEmitter()->emitIns_R_I(INS_mov, EA_PTRSIZE, immReg, imm);
#else
            instGen_Set_Reg_To_Imm(EA_PTRSIZE, immReg, imm);
#endif
        }

// For 64-bit we check if the local is 8-byte aligned. For 32-bit, we assume everything is always 4-byte aligned.
#ifdef TARGET_64BIT
        bool fpBased;
        int  addr = compiler->lvaFrameAddress((int)varNum, &fpBased);
#else
        int addr = 0;
#endif
        int size = varDsc->GetFrameSize();
        int end  = addr + size;
        for (int offs = addr; offs < end;)
        {
#ifdef TARGET_64BIT
            if ((offs % 8) == 0 && end - offs >= 8)
            {
                GetEmitter()->emitIns_S_R(ins_Store(TYP_LONG), EA_8BYTE, immReg, (int)varNum, offs - addr);
                offs += 8;
                continue;
            }
#endif

            assert((offs % 4) == 0 && end - offs >= 4);
            GetEmitter()->emitIns_S_R(ins_Store(TYP_INT), EA_4BYTE, immReg, (int)varNum, offs - addr);
            offs += 4;
        }
    }
}
