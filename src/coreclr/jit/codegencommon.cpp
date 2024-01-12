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
#include "unwind.h"

void Compiler::codeGenInit()
{
    codeGen = new (this, CMK_Codegen) CodeGen(this);
}

CodeGenInterface::CodeGenInterface(Compiler* compiler) : compiler(compiler), spillTemps(compiler)
{
}

void CodeGenInterface::genGenerateCode(void** nativeCode, uint32_t* nativeCodeSize)
{
    static_cast<CodeGen*>(this)->genGenerateCode(nativeCode, nativeCodeSize);
}

unsigned CodeGenInterface::GetHotCodeSize() const
{
    return GetEmitter()->GetHotCodeSize();
}

unsigned CodeGenInterface::GetColdCodeSize() const
{
    return GetEmitter()->GetColdCodeSize();
}

unsigned CodeGenInterface::GetCodeSize() const
{
    return GetEmitter()->GetCodeSize();
}

#ifdef JIT32_GCENCODER
unsigned CodeGenInterface::GetGCInfoSize() const
{
    return static_cast<const CodeGen*>(this)->gcInfoSize;
}
#endif

#if defined(DEBUG) || defined(LATE_DISASM)
double CodeGenInterface::GetPerfScore() const
{
    return GetEmitter()->GetPerfScore();
}
#endif

#ifdef LATE_DISASM
const char* CodeGenInterface::siRegVarName(size_t offs, size_t size, unsigned reg)
{
    static_cast<CodeGen*>(this)->siRegVarName(offs, size, reg);
}

const char* CodeGenInterface::siStackVarName(size_t offs, size_t size, unsigned reg, unsigned stkOffs)
{
    static_cast<CodeGen*>(this)->siStackVarName(offs, size, reg, stkOffs);
}
#endif

CodeGen::CodeGen(Compiler* compiler) : CodeGenInterface(compiler), liveness(compiler)
{
    m_cgEmitter = new (compiler->getAllocator()) emitter(compiler, this, compiler->info.compCompHnd);

#ifdef LATE_DISASM
    getDisAssembler().disInit(compiler);
#endif
}

#ifdef TARGET_XARCH
void CodeGenInterface::SetUseVEXEncoding(bool value)
{
    m_cgEmitter->SetUseVEXEncoding(value);
}
#endif

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

#ifdef FEATURE_EH_FUNCLETS

// Update the current funclet as needed by calling genUpdateCurrentFunclet().
// For non-BBF_FUNCLET_BEG blocks, it asserts that the current funclet
// is up-to-date.
void CodeGen::genUpdateCurrentFunclet(BasicBlock* block)
{
    if ((block->bbFlags & BBF_FUNCLET_BEG) != 0)
    {
        const FuncInfoDsc& func = funSetCurrentFunc(funGetFuncIdx(block));

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
    if (compiler->verbose)
    {
        printf("*************** In genGenerateCode()\n");
        compiler->fgDispBasicBlocks(compiler->verboseTrees);
    }
#endif

#ifdef FEATURE_EH_FUNCLETS
    DoPhase(this, PHASE_CREATE_FUNCLETS, &CodeGen::genCreateFunclets);
#endif
    DoPhase(this, PHASE_LINEAR_SCAN, &CodeGen::genAllocateRegisters);
    DoPhase(this, PHASE_GENERATE_CODE, &CodeGen::genGenerateMachineCode);
    DoPhase(this, PHASE_EMIT_CODE, &CodeGen::genEmitMachineCode);
    DoPhase(this, PHASE_EMIT_GCEH, &CodeGen::genEmitUnwindDebugGCandEH);

#ifdef LATE_DISASM
    getDisAssembler().disAsmCode(GetEmitter()->GetHotCodeAddr(), GetEmitter()->GetHotCodeSize(),
                                 GetEmitter()->GetColdCodeAddr(), GetEmitter()->GetColdCodeSize());
#endif

#if DISPLAY_SIZES
    grossVMsize += compiler->info.compILCodeSize;
    totalNCsize += GetEmitter()->GetCodeSize() + GetEmitter()->emitDataSize() + gcInfoSize;
    grossNCsize += GetEmitter()->GetCodeSize() + GetEmitter()->emitDataSize();
#endif

#if TRACK_LSRA_STATS
    if (JitConfig.DisplayLsraStats() == 2)
    {
        m_lsra->dumpLsraStatsCsv(jitstdout);
    }
#endif

    *nativeCode     = GetEmitter()->GetHotCodeAddr();
    *nativeCodeSize = GetEmitter()->GetCodeSize();
}

#ifdef FEATURE_EH_FUNCLETS
void CodeGen::genCreateFunclets()
{
    JITDUMP("*************** In genCreateFunclets()\n");

    const unsigned funcCount = compiler->ehFuncletCount() + 1;

    if (!FitsIn<uint16_t>(funcCount))
    {
        IMPL_LIMITATION("Too many funclets");
    }

    FuncInfoDsc* funcInfo = compiler->getAllocator(CMK_UnwindInfo).allocate<FuncInfoDsc>(funcCount);
    new (&funcInfo[0]) FuncInfoDsc(FUNC_ROOT, 0);

    unsigned funcIndex = 1;

    for (unsigned ehIndex = 0; ehIndex < compiler->compHndBBtabCount; ehIndex++)
    {
        EHblkDsc* ehClause = compiler->ehGetDsc(ehIndex);

        if (ehClause->HasFilter())
        {
            assert(funcIndex < funcCount);

            new (&funcInfo[funcIndex++]) FuncInfoDsc(FUNC_FILTER, ehIndex);
        }

        assert(funcIndex < funcCount);

        ehClause->ebdFuncIndex = static_cast<uint16_t>(funcIndex);
        new (&funcInfo[funcIndex++]) FuncInfoDsc(FUNC_HANDLER, ehIndex);
    }

    assert(funcIndex == funcCount);

    compFuncInfos     = funcInfo;
    compFuncInfoCount = static_cast<uint16_t>(funcCount);
}

// Get the FuncInfoDsc for the given funclet.
// This is only valid after funclets are created.
FuncInfoDsc& CodeGen::funGetFunc(unsigned index)
{
    assert(compFuncInfos != nullptr);
    assert(index < compFuncInfoCount);
    return compFuncInfos[index];
}

// Get the funcIdx for the EH funclet that begins with block.
// It is only valid for blocks marked with BBF_FUNCLET_BEG because
// otherwise we would have to do a more expensive check to determine
// if this should return the filter funclet or the filter handler funclet.
unsigned CodeGen::funGetFuncIdx(BasicBlock* block)
{
    assert(compFuncInfos != nullptr);
    assert((block->bbFlags & BBF_FUNCLET_BEG) != 0);

    EHblkDsc* eh      = compiler->ehGetDsc(block->getHndIndex());
    unsigned  funcIdx = eh->ebdFuncIndex;

    if (eh->ebdHndBeg != block)
    {
        // If this is a filter EH clause, but we want the funclet
        // for the filter (not the filter handler), it is the previous one
        noway_assert(eh->HasFilter());
        noway_assert(eh->ebdFilter == block);

        assert(funGetFunc(funcIdx).kind == FUNC_HANDLER);
        assert(funGetFunc(funcIdx).ehIndex == funGetFunc(funcIdx - 1).ehIndex);
        assert(funGetFunc(funcIdx - 1).kind == FUNC_FILTER);

        funcIdx--;
    }

    return funcIdx;
}
#endif // FEATURE_EH_FUNCLETS

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
    compiler->fgSafeBasicBlockCreation = false;
    compiler->fgDebugCheckLinks();
    compiler->fgDebugCheckBBlist();
#endif

    genFinalizeFrame();
    genCodeForBBlist();
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

void CodeGen::genEmitMachineCode()
{
#ifdef DEBUG
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

#ifdef FEATURE_EH_FUNCLETS
    unwindReserve();
#endif

    Emitter& emit = *GetEmitter();
    emit.emitEndCodeGen();

#ifdef DEBUG
    if (compiler->opts.disAsm || compiler->verbose)
    {
        printf("\n; Total bytes of code %d, prolog size %d, PerfScore %.2f, instruction count %d, allocated bytes for "
               "code %d",
               emit.GetCodeSize(), emit.GetPrologSize(), emit.GetPerfScore(), emit.GetInstrCount(),
               emit.GetHotCodeSize() + emit.GetColdCodeSize());

#if TRACK_LSRA_STATS
        if (JitConfig.DisplayLsraStats() == 3)
        {
            m_lsra->dumpLsraStatsSummary(jitstdout);
        }
#endif

        printf(" (MethodHash=%08x) for method %s\n", compiler->info.compMethodHash(), compiler->info.compFullName);
        printf("; ============================================================\n\n");
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
        assert(maxNestedAlignment % 4 == 0);
        maxAllowedStackDepth += maxNestedAlignment / 4;
#endif

        assert(GetEmitter()->emitMaxStackDepth <= maxAllowedStackDepth);
    }
#endif // DEBUG_ARG_SLOTS

    // Make sure that the x86 alignment and cache prefetch optimization rules were
    // obeyed - don't start a method in the last 7 bytes of a 16-byte alignment area
    // unless we are generating SMALL_CODE.
    // TODO-MIKE-Review: What's up with this?
    // noway_assert((reinterpret_cast<size_t>(codePtr) % 16 <= 8) || (compiler->compCodeOpt() == SMALL_CODE));
}

void CodeGen::genEmitUnwindDebugGCandEH()
{
#ifdef FEATURE_EH_FUNCLETS
    unwindEmit();
#endif

    if (compiler->opts.compDbgInfo)
    {
        genIPmappingGen();
        genSetScopeInfo();
    }

    genReportEH();
    GetEmitter()->GetGCInfo().CreateAndStoreGCInfo(this);
}

void CodeGen::genReportEH()
{
    if (compiler->compHndBBtabCount == 0)
    {
        return;
    }

    DBEXEC(compiler->opts.dspEHTable, printf("*************** EH table for %s\n", compiler->info.compFullName));

    bool     isCoreRTABI = compiler->IsTargetAbi(CORINFO_CORERT_ABI);
    unsigned EHCount     = compiler->compHndBBtabCount;

#ifdef FEATURE_EH_FUNCLETS
    // Count duplicated clauses. This uses the same logic as below,
    // where we actually generate them for reporting to the VM.
    unsigned duplicateClauseCount = 0;

    if (!isCoreRTABI)
    {
        for (unsigned XTnum = 0; XTnum < compiler->compHndBBtabCount; XTnum++)
        {
            // find the true enclosing try index, ignoring 'mutual protect' trys
            for (unsigned enclosingTryIndex = compiler->ehTrueEnclosingTryIndexIL(XTnum);
                 enclosingTryIndex != EHblkDsc::NO_ENCLOSING_INDEX;
                 enclosingTryIndex = compiler->ehGetEnclosingTryIndex(enclosingTryIndex))
            {
                duplicateClauseCount++;
            }
        }

        EHCount += duplicateClauseCount;
    }

#if !FEATURE_EH_CALLFINALLY_THUNKS
    const unsigned clonedFinallyCount = 0;
#else
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
                    clonedFinallyCount++;
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
        printf("%u EH table entries, ", compiler->compHndBBtabCount);
#ifndef FEATURE_EH_FUNCLETS
        assert(compiler->compHndBBtabCount == EHCount);
#else // FEATURE_EH_FUNCLETS
        printf("%u duplicate clauses, ", duplicateClauseCount);
#if FEATURE_EH_CALLFINALLY_THUNKS
        printf("%u cloned finallys, ", clonedFinallyCount);
#endif
        assert(compiler->compHndBBtabCount + duplicateClauseCount + clonedFinallyCount == EHCount);
#endif // FEATURE_EH_FUNCLETS
        printf("%u total EH entries reported to VM\n", EHCount);
    }
#endif // DEBUG

    eeSetEHcount(EHCount);

    uint32_t codeSize = GetEmitter()->GetCodeSize();
    unsigned XTnum    = 0; // This is the index we pass to the VM

    for (EHblkDsc* const HBtab : EHClauses(compiler))
    {
        uint32_t tryBeg = ehCodeOffset(HBtab->ebdTryBeg);
        uint32_t hndBeg = ehCodeOffset(HBtab->ebdHndBeg);
        uint32_t tryEnd = HBtab->ebdTryLast == compiler->fgLastBB ? codeSize : ehCodeOffset(HBtab->ebdTryLast->bbNext);
        uint32_t hndEnd = HBtab->ebdHndLast == compiler->fgLastBB ? codeSize : ehCodeOffset(HBtab->ebdHndLast->bbNext);
        uint32_t hndTyp = HBtab->HasFilter() ? ehCodeOffset(HBtab->ebdFilter) : HBtab->ebdTyp;

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
        eeSetEHinfo(XTnum, &clause);

        ++XTnum;
    }

#ifdef FEATURE_EH_FUNCLETS
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
        unsigned reportedDuplicateClauseCount = 0; // How many duplicated clauses have we reported?

        for (unsigned XTnum2 = 0; XTnum2 < compiler->compHndBBtabCount; XTnum2++)
        {
            EHblkDsc* fletTab = compiler->ehGetDsc(XTnum2);

            // find the true enclosing try index, ignoring 'mutual protect' trys
            for (unsigned enclosingTryIndex = compiler->ehTrueEnclosingTryIndexIL(XTnum2);
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

                uint32_t tryBeg = ehCodeOffset(bbTryBeg);
                uint32_t hndBeg = ehCodeOffset(bbHndBeg);
                uint32_t tryEnd = bbTryLast == compiler->fgLastBB ? codeSize : ehCodeOffset(bbTryLast->bbNext);
                uint32_t hndEnd = bbHndLast == compiler->fgLastBB ? codeSize : ehCodeOffset(bbHndLast->bbNext);
                uint32_t hndTyp = encTab->HasFilter() ? ehCodeOffset(encTab->ebdFilter) : encTab->ebdTyp;

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
                eeSetEHinfo(XTnum, &clause);

                XTnum++;
                reportedDuplicateClauseCount++;

#ifndef DEBUG
                if (duplicateClauseCount == reportedDuplicateClauseCount)
                {
                    break; // we've reported all of them; no need to continue looking
                }
#endif // !DEBUG
            }
        }

        assert(duplicateClauseCount == reportedDuplicateClauseCount);
    }

#if FEATURE_EH_CALLFINALLY_THUNKS
    if (clonedFinallyCount > 0)
    {
        unsigned reportedClonedFinallyCount = 0;

        for (BasicBlock* const block : compiler->Blocks())
        {
            if (block->bbJumpKind == BBJ_CALLFINALLY)
            {
                // How big is it? The BBJ_ALWAYS has a null label! Look for the block after, which
                // must be a label or jump target, since the BBJ_CALLFINALLY doesn't fall through.
                BasicBlock* bbLabel = block->bbNext;

                if (block->isBBCallAlwaysPair())
                {
                    bbLabel = bbLabel->bbNext; // skip the BBJ_ALWAYS
                }

                uint32_t hndBeg = ehCodeOffset(block);
                uint32_t hndEnd = bbLabel == nullptr ? codeSize : ehCodeOffset(bbLabel);

                CORINFO_EH_CLAUSE clause;
                clause.ClassToken = 0; // unused
                clause.Flags      = (CORINFO_EH_CLAUSE_FLAGS)(CORINFO_EH_CLAUSE_FINALLY | CORINFO_EH_CLAUSE_DUPLICATE);
                clause.TryOffset  = hndBeg;
                clause.TryLength  = hndBeg;
                clause.HandlerOffset = hndBeg;
                clause.HandlerLength = hndEnd;

                assert(XTnum < EHCount);

                eeSetEHinfo(XTnum, &clause);

                XTnum++;
                reportedClonedFinallyCount++;

#ifndef DEBUG
                if (clonedFinallyCount == reportedClonedFinallyCount)
                {
                    break; // we're done; no need to keep looking
                }
#endif
            }
        }

        assert(clonedFinallyCount == reportedClonedFinallyCount);
    }
#endif // FEATURE_EH_CALLFINALLY_THUNKS
#endif // FEATURE_EH_FUNCLETS

    assert(XTnum == EHCount);
}

void CodeGen::eeSetEHcount(unsigned cEH)
{
    JITDUMP("setEHcount(cEH=%u)\n", cEH);

    if (compiler->info.compMatchedVM)
    {
        compiler->info.compCompHnd->setEHcount(cEH);
    }
}

void CodeGen::eeSetEHinfo(unsigned EHnumber, const CORINFO_EH_CLAUSE* clause)
{
    DBEXEC(compiler->opts.dspEHTable, dispOutgoingEHClause(EHnumber, *clause));

    if (compiler->info.compMatchedVM)
    {
        compiler->info.compCompHnd->setEHinfo(EHnumber, clause);
    }
}

#ifdef DEBUG
void CodeGen::dispOutgoingEHClause(unsigned num, const CORINFO_EH_CLAUSE& clause)
{
    if (compiler->opts.dspDiffable)
    {
        printf("EH#%u: try [%s..%s) handled by [%s..%s) ", num, GetEmitter()->emitOffsetToLabel(clause.TryOffset),
               GetEmitter()->emitOffsetToLabel(clause.TryLength), GetEmitter()->emitOffsetToLabel(clause.HandlerOffset),
               GetEmitter()->emitOffsetToLabel(clause.HandlerLength));
    }
    else
    {
        printf("EH#%u: try [%04X..%04X) handled by [%04X..%04X) ", num, dspOffset(clause.TryOffset),
               dspOffset(clause.TryLength), dspOffset(clause.HandlerOffset), dspOffset(clause.HandlerLength));
    }

    // Note: the flags field is kind of weird. It should be compared for equality
    // to determine the type of clause, even though it looks like a bitfield. In
    // Particular, CORINFO_EH_CLAUSE_NONE is zero, so you can "&" to check it.
    // You do need to mask off the bits, though, because CORINFO_EH_CLAUSE_DUPLICATE
    // is and'ed in.
    const DWORD CORINFO_EH_CLAUSE_TYPE_MASK = 0x7;
    switch (clause.Flags & CORINFO_EH_CLAUSE_TYPE_MASK)
    {
        case CORINFO_EH_CLAUSE_NONE:
            printf("(class: %04X)", clause.ClassToken);
            break;
        case CORINFO_EH_CLAUSE_FILTER:
            if (compiler->opts.dspDiffable)
            {
                printf("filter at [%s..%s)", GetEmitter()->emitOffsetToLabel(clause.ClassToken),
                       GetEmitter()->emitOffsetToLabel(clause.HandlerOffset));
            }
            else
            {
                printf("filter at [%04X..%04X)", dspOffset(clause.ClassToken), dspOffset(clause.HandlerOffset));
            }
            break;
        case CORINFO_EH_CLAUSE_FINALLY:
            printf("(finally)");
            break;
        case CORINFO_EH_CLAUSE_FAULT:
            printf("(fault)");
            break;
        default:
            printf("(UNKNOWN type %u!)", clause.Flags & CORINFO_EH_CLAUSE_TYPE_MASK);
            assert(!"unknown type");
            break;
    }

    if ((clause.TryOffset == clause.TryLength) && (clause.TryOffset == clause.HandlerOffset) &&
        ((clause.Flags & (CORINFO_EH_CLAUSE_DUPLICATE | CORINFO_EH_CLAUSE_FINALLY)) ==
         (CORINFO_EH_CLAUSE_DUPLICATE | CORINFO_EH_CLAUSE_FINALLY)))
    {
        printf(" cloned finally");
    }
    else if (clause.Flags & CORINFO_EH_CLAUSE_DUPLICATE)
    {
        printf(" duplicated");
    }
    else if (clause.Flags & CORINFO_EH_CLAUSE_SAMETRY)
    {
        printf(" same try");
    }
    printf("\n");
}
#endif // DEBUG

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
    if (IsFloatReg(regNum))
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

                if (IsFloatReg(regNum) != isFloat)
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
            if (IsFloatReg(lcl->GetParamReg()) != isFloat)
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

#ifdef TARGET_ARM64
                emitAttr size = emitActualTypeSize(destMemType);

                if (varTypeIsSIMD(lcl->GetType()) && (paramRegIndex < paramRegCount - 1) &&
                    (paramRegs[paramRegIndex + 1].regIndex == 1))
                {
                    // For a SIMD type that is passed in two integer registers,
                    // Limit the copy below to the first 8 bytes from the first integer register.
                    // Handle the remaining 8 bytes from the second slot in the code further below
                    assert(EA_SIZE(size) >= 8);

                    size = EA_8BYTE;
                }

                GetEmitter()->emitIns_Mov(ins_Copy(regNum, destMemType), size, destRegNum, regNum, /*canSkip*/ false);
#else
                inst_Mov(destMemType, destRegNum, regNum, /*canSkip*/ false);
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

        GetEmitter()->GetGCInfo().SetTrackedStackSlotRange(minGCTrackedOffset, maxGCTrackedOffset + REGSIZE_BYTES);
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

#ifdef TARGET_XARCH
            GetEmitter()->emitIns_R_AR(ins_Load(lclTyp), size, varDsc->GetRegNum(), genFramePointerReg(), offset);
#else
            // TODO-MIKE-Review: Looks like ARM64 doesn't support OSR.
            NYI("OSR local init");
#endif
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
#if defined(TARGET_X86)
        if (isFramePointerUsed())
        {
            // It cannot be `this` since that's passed in a register so it has to be TypeCtxtArg
            // which is always the last parameter (and we know that the frame pointer is also
            // pushed, in addition to the return address).
            noway_assert(varDsc->GetStackOffset() == 2 * REGSIZE_BYTES);
        }

        reg             = initReg;
        *pInitRegZeroed = false;

        GetEmitter()->emitIns_R_AR(INS_mov, EA_4BYTE, reg, genFramePointerReg(), varDsc->GetStackOffset());
#elif defined(TARGET_ARM)
        if (isFramePointerUsed())
        {
            // On ARM both `this` and TypeCtxtArg are always reg params but due
            // to pre-spilling done for profiler we need to load from the stack,
            // the offset should be in the pre-spill param area, right above FP
            // and LR.
            noway_assert((2 * REGSIZE_BYTES <= varDsc->GetStackOffset()) &&
                         (size_t(varDsc->GetStackOffset()) < GetPreSpillSize() + 2 * REGSIZE_BYTES));
        }

        reg             = initReg;
        *pInitRegZeroed = false;

        GetEmitter()->emitIns_R_R_I(INS_ldr, EA_4BYTE, reg, genFramePointerReg(), varDsc->GetStackOffset());
#else
        // On other targets we have enough param regs that the type
        // context param is always passed in a register.
        unreached();
#endif
    }

#if defined(TARGET_ARM64)
    genInstrWithConstant(INS_str, EA_8BYTE, reg, genFramePointerReg(), cachedGenericContextArgOffset, rsGetRsvdReg());
#elif defined(TARGET_ARM)
    GetEmitter()->emitIns_R_R_I(INS_str, EA_4BYTE, reg, genFramePointerReg(), cachedGenericContextArgOffset);
#elif defined(TARGET_XARCH)
    GetEmitter()->emitIns_AR_R(INS_mov, EA_PTRSIZE, reg, genFramePointerReg(), cachedGenericContextArgOffset);
#else
#error Unknown target
#endif
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
    InitLclBlockLiveInRegs();

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
    if (compiler->verbose)
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

    if (compiler->getNeedsGSSecurityCookie())
    {
        compiler->info.compCompHnd->getGSCookie(&m_gsCookieVal, &m_gsCookieAddr);
    }

#ifdef DEBUG
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
    JITDUMP("\n=============== Generating prolog\n");

    // Before generating the prolog, we need to reset the variable locations to what they will be on entry.
    // This affects our code that determines which untracked locals need to be zero initialized.
    InitLclBlockLiveInRegs();

    ScopedSetVariable<bool> _setGeneratingProlog(&generatingProlog, true);
    funSetCurrentFunc(0);
    GetEmitter()->emitBegProlog();
    unwindBegProlog();
    liveness.BeginPrologEpilogCodeGen();

#ifdef TARGET_XARCH
    // For OSR there is a "phantom prolog" to account for the actions taken
    // in the original frame that impact RBP and RSP on entry to the OSR method.
    if (compiler->opts.IsOSR())
    {
        PatchpointInfo* patchpointInfo    = compiler->info.compPatchpointInfo;
        const int       originalFrameSize = patchpointInfo->FpToSpDelta();

        unwindPush(REG_FPBASE);
        unwindAllocStack(originalFrameSize);
    }
#endif

#ifdef DEBUG
    if (compiler->compJitHaltMethod())
    {
        // Put a nop first because the debugger and other tools are likely to
        // put an int3 at the beginning and we don't want to confuse them.
        GetEmitter()->emitIns(INS_nop);
        GetEmitter()->emitIns(INS_BREAKPOINT);

#ifdef TARGET_ARMARCH
        // Avoid asserts in the unwind info because these instructions aren't accounted for.
        unwindPadding();
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
        unwindPushMaskInt(preSpillRegs);
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
        unwindPush(REG_FPBASE);

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
        unwindSetFrameReg(REG_SAVED_LOCALLOC_SP);
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

    if (compiler->opts.compDbgInfo && (compiler->info.compVarScopesCount > 0))
    {
        // TODO-MIKE-Review: The prolog doesn't actually end here, or to be more precise,
        // the first block doesn't start after this point. So why limit the prolog param
        // debug info ranges to this point?
        liveness.CreatePrologDbgInfoRanges(this);
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
    unwindEndProlog();
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
    genFnProlog();
#ifdef FEATURE_EH_FUNCLETS
    // Capture the data we're going to use in the funclet prolog and epilog generation. This is
    // information computed during codegen, or during function prolog generation, like
    // frame offsets. It must run after main function prolog generation.
    genCaptureFuncletPrologEpilogInfo();
#endif
    GetEmitter()->emitGeneratePrologEpilog();
}

#ifdef DEBUG
static void PrintDbgInfoVars(Compiler* compiler, const ICorDebugInfo::NativeVarInfo* vars, unsigned count);
#endif

struct VarResultInfo
{
    uint32_t      startOffset;
    uint32_t      endOffset;
    uint32_t      varNumber;
    DbgInfoVarLoc loc;
};

void CodeGen::genSetScopeInfo()
{
    JITDUMP("*************** In genSetScopeInfo()\n");

    assert(compiler->opts.compDbgInfo);

    unsigned count = liveness.GetDbgInfoRangeCount();

    JITDUMP("DbgInfoVarRange count is %u\n", count);

    if (count == 0)
    {
        DBEXEC(compiler->verbose || compiler->opts.dspDebugInfo, PrintDbgInfoVars(compiler, nullptr, 0));

        return;
    }

    assert(compiler->info.compVarScopesCount > 0);

#ifdef LATE_DISASM
    genTrnslLocalVarCount = count;
    genTrnslLocalVarInfo  = new (compiler, CMK_DebugOnly) TrnslLocalVarInfo[count];
#endif

    VarResultInfo* ranges =
        static_cast<VarResultInfo*>(compiler->info.compCompHnd->allocateArray(count * sizeof(VarResultInfo)));

    genSetScopeInfoUsingVariableRanges(ranges);

    ICorDebugInfo::NativeVarInfo* vars = reinterpret_cast<ICorDebugInfo::NativeVarInfo*>(ranges);
    DBEXEC(compiler->verbose || compiler->opts.dspDebugInfo, PrintDbgInfoVars(compiler, vars, count));
    compiler->info.compCompHnd->setVars(compiler->info.compMethodHnd, count, vars);
}

//------------------------------------------------------------------------
// genSetScopeInfoUsingVariableRanges: Call "genSetScopeInfo" with the
//  "VariableLiveRanges" created for the arguments, special arguments and
//  IL local variables.
//
// Notes:
//  This function is called from "genSetScopeInfo" once the code is generated
//  and we want to send debug info to the debugger.
//
void CodeGen::genSetScopeInfoUsingVariableRanges(VarResultInfo* vars)
{
    unsigned liveRangeIndex = 0;

    for (unsigned lclNum = 0; lclNum < compiler->info.compLocalsCount; lclNum++)
    {
        LclVarDsc* lcl      = compiler->lvaGetDesc(lclNum);
        unsigned   ilVarNum = compiler->compMap2ILvarNum(lclNum);

        if (ilVarNum == ICorDebugInfo::UNKNOWN_ILNUM)
        {
            continue;
        }

        for (DbgInfoVarRange* range = liveness.GetDbgInfoRanges(lclNum); range != nullptr; range = range->next)
        {
            uint32_t startOffs = range->startOffset.GetCodeOffset();
            uint32_t endOffs   = range->endOffset.GetCodeOffset();

            if (lcl->IsParam() && (startOffs == endOffs))
            {
                // If the length is zero, it means that the prolog is empty. In that case,
                // CodeGen::genSetScopeInfo will report the liveness of all arguments
                // as spanning the first instruction in the method, so that they can
                // at least be inspected on entry to the method.
                endOffs++;
            }

            genSetScopeInfo(vars, liveRangeIndex, startOffs, endOffs, lclNum, ilVarNum, &range->location);
            liveRangeIndex++;
        }
    }
}

void CodeGen::genSetScopeInfo(VarResultInfo* vars,
                              unsigned       index,
                              uint32_t       startOffs,
                              uint32_t       endOffs,
                              unsigned       lclNum,
                              unsigned       ilVarNum,
                              DbgInfoVarLoc* varLoc)
{
    assert(ilVarNum != ICorDebugInfo::UNKNOWN_ILNUM);

#ifdef TARGET_X86
    if (compiler->info.compIsVarArgs && (lclNum != compiler->lvaVarargsHandleArg) &&
        (lclNum < compiler->info.compArgsCount) && !compiler->lvaGetDesc(lclNum)->IsRegParam())
    {
        noway_assert((varLoc->vlType == DbgInfoVarLoc::VLT_STK) || (varLoc->vlType == DbgInfoVarLoc::VLT_STK2));

        // All stack arguments (except the varargs handle) have to be
        // accessed via the varargs cookie. Discard generated info,
        // and just find its position relative to the varargs handle

        assert(compiler->lvaVarargsHandleArg < compiler->info.compArgsCount);

        LclVarDsc* varargHandleLcl = compiler->lvaGetDesc(compiler->lvaVarargsHandleArg);

        if (!varargHandleLcl->lvOnFrame)
        {
            noway_assert(!compiler->opts.compDbgCode);
            return;
        }

        LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

        // Can't check varLcl->lvOnFrame as we don't set it for params
        // of vararg functions to avoid reporting them to GC.
        noway_assert(!lcl->lvRegister);

        unsigned cookieOffset = varargHandleLcl->GetStackOffset();
        unsigned varOffset    = lcl->GetStackOffset();

        noway_assert(cookieOffset < varOffset);
        unsigned offset     = varOffset - cookieOffset;
        unsigned stkArgSize = paramsStackSize;
        noway_assert(offset < stkArgSize);
        offset = stkArgSize - offset;

        varLoc->vlType                   = DbgInfoVarLoc::VLT_FIXED_VA;
        varLoc->vlFixedVarArg.vlfvOffset = offset;
    }
#endif // TARGET_X86

#ifdef LATE_DISASM
    TrnslLocalVarInfo& tlvi = genTrnslLocalVarInfo[index];

    tlvi.tlviName    = gtGetLclVarName(lclNum);
    tlvi.tlviStartPC = startOffs;
    tlvi.tlviEndPC   = endOffs;
    tlvi.tlviVarLoc  = *varLoc;
#endif

    vars[index].startOffset = startOffs;
    vars[index].endOffset   = endOffs;
    vars[index].varNumber   = ilVarNum;
    vars[index].loc         = *varLoc;
}

// Check every siVarLocType and siVarLoc are what ICodeDebugInfo is expecting
// so we can reinterpret siVarLoc as ICodeDebugInfo::VarLoc.
#ifdef TARGET_X86
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::REGNUM_EAX) == REG_EAX);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::REGNUM_ECX) == REG_ECX);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::REGNUM_EDX) == REG_EDX);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::REGNUM_EBX) == REG_EBX);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::REGNUM_ESP) == REG_ESP);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::REGNUM_EBP) == REG_EBP);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::REGNUM_ESI) == REG_ESI);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::REGNUM_EDI) == REG_EDI);
#endif

static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::VLT_REG) == DbgInfoVarLoc::VLT_REG);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::VLT_STK) == DbgInfoVarLoc::VLT_STK);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::VLT_REG_REG) == DbgInfoVarLoc::VLT_REG_REG);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::VLT_REG_STK) == DbgInfoVarLoc::VLT_REG_STK);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::VLT_STK_REG) == DbgInfoVarLoc::VLT_STK_REG);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::VLT_STK2) == DbgInfoVarLoc::VLT_STK2);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::VLT_FPSTK) == DbgInfoVarLoc::VLT_FPSTK);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::VLT_FIXED_VA) == DbgInfoVarLoc::VLT_FIXED_VA);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::VLT_COUNT) == DbgInfoVarLoc::VLT_COUNT);
static_assert_no_msg(static_cast<unsigned>(ICorDebugInfo::VLT_INVALID) == DbgInfoVarLoc::VLT_INVALID);

static_assert_no_msg(sizeof(ICorDebugInfo::VarLoc) == sizeof(DbgInfoVarLoc));
static_assert_no_msg(sizeof(VarResultInfo) == sizeof(ICorDebugInfo::NativeVarInfo));

#ifdef LATE_DISASM

const char* CodeGen::siRegVarName(size_t offs, size_t size, unsigned reg)
{
#ifdef DEBUG
    if (!compiler->opts.compDbgInfo || (compiler->info.compVarScopesCount == 0))
    {
        return nullptr;
    }

    TrnslLocalVarInfo* info = genTrnslLocalVarInfo;

    noway_assert((genTrnslLocalVarCount == 0) || (info != nullptr));

    for (unsigned i = 0; i < genTrnslLocalVarCount; i++)
    {
        if ((info[i].tlviVarLoc.IsInReg((regNumber)reg)) && (info[i].tlviStartPC <= offs + size) &&
            (info[i].tlviEndPC > offs))
        {
            return info[i].tlviName;
        }
    }
#endif // DEBUG

    return nullptr;
}

const char* CodeGen::siStackVarName(size_t offs, size_t size, unsigned reg, unsigned stkOffs)
{
#ifdef DEBUG
    if (!compiler->opts.compDbgInfo || (compiler->info.compVarScopesCount == 0))
    {
        return nullptr;
    }

    TrnslLocalVarInfo* info = genTrnslLocalVarInfo;

    noway_assert((genTrnslLocalVarCount == 0) || (info != nullptr));

    for (unsigned i = 0; i < genTrnslLocalVarCount; i++)
    {
        if ((info[i].tlviVarLoc.IsOnStack((regNumber)reg, stkOffs)) && (info[i].tlviStartPC <= offs + size) &&
            (info[i].tlviEndPC > offs))
        {
            return info[i].tlviName;
        }
    }
#endif // DEBUG

    return nullptr;
}
#endif // LATE_DISASM

#ifdef DEBUG

static void PrintDbgInfoVar(const ICorDebugInfo::NativeVarInfo& var)
{
    const char* name = nullptr;

    if (var.varNumber == ICorDebugInfo::VARARGS_HND_ILNUM)
    {
        name = "varargsHandle";
    }
    else if (var.varNumber == ICorDebugInfo::RETBUF_ILNUM)
    {
        name = "retBuff";
    }
    else if (var.varNumber == ICorDebugInfo::TYPECTXT_ILNUM)
    {
        name = "typeCtx";
    }

    printf("%3d(%10s) : From %08Xh to %08Xh, in ", var.varNumber, (name == nullptr) ? "UNKNOWN" : name, var.startOffset,
           var.endOffset);

    switch (var.loc.vlType)
    {
        case ICorDebugInfo::VLT_REG:
        case ICorDebugInfo::VLT_REG_BYREF:
        case ICorDebugInfo::VLT_REG_FP:
            printf("%s", getRegName(var.loc.vlReg.vlrReg));
            if (var.loc.vlType == ICorDebugInfo::VLT_REG_BYREF)
            {
                printf(" byref");
            }
            break;

        case ICorDebugInfo::VLT_STK:
        case ICorDebugInfo::VLT_STK_BYREF:
            if ((int)var.loc.vlStk.vlsBaseReg != (int)ICorDebugInfo::REGNUM_AMBIENT_SP)
            {
                printf("%s[%d] (1 slot)", getRegName(var.loc.vlStk.vlsBaseReg), var.loc.vlStk.vlsOffset);
            }
            else
            {
                printf(STR_SPBASE "'[%d] (1 slot)", var.loc.vlStk.vlsOffset);
            }
            if (var.loc.vlType == ICorDebugInfo::VLT_REG_BYREF)
            {
                printf(" byref");
            }
            break;

        case ICorDebugInfo::VLT_REG_REG:
            printf("%s-%s", getRegName(var.loc.vlRegReg.vlrrReg1), getRegName(var.loc.vlRegReg.vlrrReg2));
            break;

#ifndef TARGET_AMD64
        case ICorDebugInfo::VLT_REG_STK:
            if ((int)var.loc.vlRegStk.vlrsStk.vlrssBaseReg != (int)ICorDebugInfo::REGNUM_AMBIENT_SP)
            {
                printf("%s-%s[%d]", getRegName(var.loc.vlRegStk.vlrsReg),
                       getRegName(var.loc.vlRegStk.vlrsStk.vlrssBaseReg), var.loc.vlRegStk.vlrsStk.vlrssOffset);
            }
            else
            {
                printf("%s-" STR_SPBASE "'[%d]", getRegName(var.loc.vlRegStk.vlrsReg),
                       var.loc.vlRegStk.vlrsStk.vlrssOffset);
            }
            break;

        case ICorDebugInfo::VLT_STK_REG:
            unreached(); // unexpected

        case ICorDebugInfo::VLT_STK2:
            if ((int)var.loc.vlStk2.vls2BaseReg != (int)ICorDebugInfo::REGNUM_AMBIENT_SP)
            {
                printf("%s[%d] (2 slots)", getRegName(var.loc.vlStk2.vls2BaseReg), var.loc.vlStk2.vls2Offset);
            }
            else
            {
                printf(STR_SPBASE "'[%d] (2 slots)", var.loc.vlStk2.vls2Offset);
            }
            break;

        case ICorDebugInfo::VLT_FPSTK:
            printf("ST(L-%d)", var.loc.vlFPstk.vlfReg);
            break;

        case ICorDebugInfo::VLT_FIXED_VA:
            printf("fxd_va[%d]", var.loc.vlFixedVarArg.vlfvOffset);
            break;
#endif // !TARGET_AMD64

        default:
            unreached();
    }

    printf("\n");
}

static void PrintDbgInfoVars(Compiler* compiler, const ICorDebugInfo::NativeVarInfo* vars, unsigned count)
{
    BitVecTraits varTraits(compiler->lvaCount, compiler);
    BitVec       uniqueVars = BitVecOps::MakeEmpty(&varTraits);
    unsigned     varCount   = 0;

    for (unsigned i = 0; i < count; i++)
    {
        if ((vars[i].varNumber < compiler->lvaCount) &&
            BitVecOps::TryAddElemD(&varTraits, uniqueVars, vars[i].varNumber))
        {
            varCount++;
        }
    }

    printf("; Variable debug info: %d live ranges, %d vars for method %s\n", count, varCount,
           compiler->info.compFullName);

    for (unsigned i = 0; i < count; i++)
    {
        PrintDbgInfoVar(vars[i]);
    }
}

#endif // DEBUG

struct ILMapping
{
    ILMapping* next;
    union {
        emitLocation nativeLoc;
        uint32_t     nativeOffset;
    };
    IL_OFFSETX ilOffsetX;
    bool       isLabel;

    ILMapping(IL_OFFSETX ilOffsetX, bool isLabel)
        : next(nullptr), nativeLoc(nullptr), ilOffsetX(ilOffsetX), isLabel(isLabel)
    {
    }

    ILMapping(ILMapping* next, uint32_t nativeOffset, IL_OFFSETX ilOffsetX, bool isLabel)
        : next(next), nativeOffset(nativeOffset), ilOffsetX(ilOffsetX), isLabel(isLabel)
    {
    }

    IL_OFFSET GetILOffset() const
    {
        assert(ilOffsetX != BAD_IL_OFFSET);

        switch (ilOffsetX)
        {
            case ICorDebugInfo::NO_MAPPING:
            case ICorDebugInfo::PROLOG:
            case ICorDebugInfo::EPILOG:
                return static_cast<IL_OFFSET>(ilOffsetX);
            default:
                return static_cast<IL_OFFSET>(ilOffsetX & ~IL_OFFSETX_BITS);
        }
    }

    bool IsStackEmpty() const
    {
        assert(ilOffsetX != BAD_IL_OFFSET);

        switch (ilOffsetX)
        {
            case ICorDebugInfo::NO_MAPPING:
            case ICorDebugInfo::PROLOG:
            case ICorDebugInfo::EPILOG:
                return true;
            default:
                return (ilOffsetX & IL_OFFSETX_STKBIT) == 0;
        }
    }

    bool IsCallInstruction() const
    {
        assert(ilOffsetX != BAD_IL_OFFSET);

        switch (ilOffsetX)
        {
            case ICorDebugInfo::NO_MAPPING:
            case ICorDebugInfo::PROLOG:
            case ICorDebugInfo::EPILOG:
                return false;
            default:
                return (ilOffsetX & IL_OFFSETX_CALLINSTRUCTIONBIT) != 0;
        }
    }
};

#ifdef DEBUG

static void PrintILOffset(IL_OFFSET offs)
{
    if (offs == ICorDebugInfo::PROLOG)
    {
        printf("PROLOG");
    }
    else if (offs == ICorDebugInfo::EPILOG)
    {
        printf("EPILOG");
    }
    else if (offs == ICorDebugInfo::NO_MAPPING)
    {
        printf("NO_MAP");
    }
    else
    {
        printf("0x%04X", offs);
    }
}

static void PrintILOffsetMapping(const ILMapping& mapping)
{
    IL_OFFSETX offsx = mapping.ilOffsetX;

    if (offsx == BAD_IL_OFFSET)
    {
        printf("???");
    }
    else
    {
        PrintILOffset(mapping.GetILOffset());

        if (mapping.IsStackEmpty())
        {
            printf(" STACK_EMPTY");
        }

        if (mapping.IsCallInstruction())
        {
            printf(" CALL_INSTRUCTION");
        }
    }

    printf(" ");
    mapping.nativeLoc.Print();

    if (mapping.isLabel)
    {
        printf(" label");
    }

    printf("\n");
}

#endif // DEBUG

void CodeGen::genIPmappingAdd(IL_OFFSETX offsx, bool isLabel)
{
    assert(compiler->opts.compDbgInfo);
    assert(offsx != BAD_IL_OFFSET);

    if ((offsx != ICorDebugInfo::PROLOG) && (offsx != ICorDebugInfo::EPILOG))
    {
        if (offsx != ICorDebugInfo::NO_MAPPING)
        {
            noway_assert(jitGetILoffs(offsx) <= compiler->info.compILCodeSize);
        }

        // Ignore this one if it's the same IL offset as the last one we saw.
        // Note that we'll let through two identical IL offsets if the flag bits
        // differ, or two identical "special" mappings (e.g., PROLOG).
        if ((lastILMapping != nullptr) && (offsx == lastILMapping->ilOffsetX))
        {
            JITDUMP("Debug info: Ignoring duplicate IL offset 0x%x\n", offsx);
            return;
        }
    }

    ILMapping* mapping = new (compiler, CMK_DebugInfo) ILMapping(offsx, isLabel);
    mapping->nativeLoc.CaptureLocation(GetEmitter());

    if (firstILMapping != nullptr)
    {
        assert(lastILMapping != nullptr);
        assert(lastILMapping->next == nullptr);

        lastILMapping->next = mapping;
    }
    else
    {
        assert(lastILMapping == nullptr);

        firstILMapping = mapping;
    }

    lastILMapping = mapping;

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("Debug Info: IL ");
        PrintILOffsetMapping(*mapping);
    }
#endif
}

static_assert_no_msg(IL_OFFSETX(ICorDebugInfo::NO_MAPPING) != IL_OFFSETX(BAD_IL_OFFSET));
static_assert_no_msg(IL_OFFSETX(ICorDebugInfo::PROLOG) != IL_OFFSETX(BAD_IL_OFFSET));
static_assert_no_msg(IL_OFFSETX(ICorDebugInfo::EPILOG) != IL_OFFSETX(BAD_IL_OFFSET));
static_assert_no_msg(IL_OFFSETX(BAD_IL_OFFSET) > MAX_IL_OFFSET);
static_assert_no_msg(IL_OFFSETX(ICorDebugInfo::NO_MAPPING) > MAX_IL_OFFSET);
static_assert_no_msg(IL_OFFSETX(ICorDebugInfo::PROLOG) > MAX_IL_OFFSET);
static_assert_no_msg(IL_OFFSETX(ICorDebugInfo::EPILOG) > MAX_IL_OFFSET);

#ifdef DEBUG

static void PrintOffsetMapping(const ICorDebugInfo::OffsetMapping& mapping)
{
    printf("IL offs ");

    PrintILOffset(mapping.ilOffset);
    printf(" : 0x%08X", mapping.nativeOffset);

    if (mapping.source != 0)
    {
        printf(" ( ");

        if ((mapping.source & ICorDebugInfo::STACK_EMPTY) != 0)
        {
            printf("STACK_EMPTY ");
        }

        if ((mapping.source & ICorDebugInfo::CALL_INSTRUCTION) != 0)
        {
            printf("CALL_INSTRUCTION ");
        }

        // We don't expect to see any other bits.
        assert((mapping.source & ~(ICorDebugInfo::STACK_EMPTY | ICorDebugInfo::CALL_INSTRUCTION)) == 0);

        printf(")");
    }

    printf("\n");
}

static void PrintOffsetMappings(const ICorDebugInfo::OffsetMapping* mappings, unsigned count)
{
    printf("IP mapping count : %d\n", count);

    for (unsigned i = 0; i < count; i++)
    {
        PrintOffsetMapping(mappings[i]);
    }

    printf("\n");
}

#endif // DEBUG

void CodeGen::genEnsureCodeEmitted(IL_OFFSETX offsx)
{
    assert(compiler->opts.compDbgCode);

    // When generating debug code we need to ensure that some
    // native code is generated for every reported IL offset.

    if ((lastILMapping != nullptr) && (lastILMapping->ilOffsetX == offsx) &&
        GetEmitter()->IsCurrentLocation(lastILMapping->nativeLoc))
    {
        GetEmitter()->emitIns(INS_nop);
    }
}

void CodeGen::genIPmappingGen()
{
    JITDUMP("\n*************** In genIPmappingGen()\n");

    assert(compiler->opts.compDbgInfo);

    unsigned mappingCount     = 1; // There's at least one mapping for PROLOG.
    uint32_t prevNativeOffset = 0;

    ILMapping prolog(firstILMapping, 0, ICorDebugInfo::PROLOG, true);

    for (ILMapping *mapping = firstILMapping, *prev = &prolog; mapping != nullptr; mapping = mapping->next)
    {
        mapping->nativeOffset = mapping->nativeLoc.GetCodeOffset();

        // Managed RetVal - since new sequence points are emitted to identify IL calls,
        // make sure that those are not filtered and do not interfere with filtering of
        // other sequence points.
        if (mapping->IsCallInstruction())
        {
            mappingCount++;

            continue;
        }

        uint32_t nextNativeOffset = mapping->nativeOffset;

        if (nextNativeOffset != prevNativeOffset)
        {
            mappingCount++;
            prevNativeOffset = nextNativeOffset;
            prev             = mapping;

            continue;
        }

        assert(prev != nullptr); // We would exit before if this was true

        // If there are mappings with the same native offset, then:
        //  - If one of them is NO_MAPPING, ignore it
        //  - If one of them is a label, report that and ignore the other one
        //  - Else report the higher IL offset

        if (prev->ilOffsetX == ICorDebugInfo::NO_MAPPING)
        {
            // If the previous entry was NO_MAPPING, ignore it
            prev->nativeOffset = UINT32_MAX;
            prev               = mapping;

            continue;
        }

        if (mapping->ilOffsetX == ICorDebugInfo::NO_MAPPING)
        {
            // If the current entry is NO_MAPPING, ignore it
            // Leave prev unchanged as mapping is no longer valid
            mapping->nativeOffset = UINT32_MAX;

            continue;
        }

        if ((mapping->ilOffsetX == ICorDebugInfo::EPILOG) || (mapping->ilOffsetX == 0))
        {
            // counting for special cases: see below
            mappingCount++;
            prev = mapping;

            continue;
        }

        noway_assert(prev != nullptr);
        noway_assert((prev->nativeOffset == UINT32_MAX) || (prevNativeOffset == prev->nativeOffset));

        // The previous block had the same native offset. We have to discard one of the
        // mappings. Simply set nativeOffset to an invalid value.

        if (prev->isLabel)
        {
            // Leave prev unchanged as mapping is no longer valid
            mapping->nativeOffset = UINT32_MAX;
        }
        else
        {
            prev->nativeOffset = UINT32_MAX;
            prev               = mapping;
        }
    }

    ICorDebugInfo::OffsetMapping* mappings = static_cast<ICorDebugInfo::OffsetMapping*>(
        compiler->info.compCompHnd->allocateArray(mappingCount * sizeof(mappings[0])));

    mappings[0].nativeOffset = 0;
    mappings[0].ilOffset     = ICorDebugInfo::PROLOG;
    mappings[0].source       = ICorDebugInfo::STACK_EMPTY;

    unsigned mappingIndex = 1;
    prevNativeOffset      = 0;

    for (ILMapping* mapping = firstILMapping; mapping != nullptr; mapping = mapping->next)
    {
        if (mapping->nativeOffset == UINT32_MAX)
        {
            continue;
        }

        uint32_t   nextNativeOffset = mapping->nativeOffset;
        IL_OFFSETX ilOffsetX        = mapping->ilOffsetX;
        auto       source           = ICorDebugInfo::SOURCE_TYPE_INVALID;

        if (mapping->IsCallInstruction())
        {
            source = static_cast<ICorDebugInfo::SourceTypes>(source | ICorDebugInfo::CALL_INSTRUCTION);
        }
        else if (nextNativeOffset != prevNativeOffset)
        {
            prevNativeOffset = nextNativeOffset;
        }
        else if ((ilOffsetX == ICorDebugInfo::EPILOG) || (ilOffsetX == 0))
        {
            // For the special case of an IL instruction with no body
            // followed by the epilog (say ret void immediately preceding
            // the method end), we put two entries in, so that we'll stop
            // at the (empty) ret statement if the user tries to put a
            // breakpoint there, and then have the option of seeing the
            // epilog or not based on SetUnmappedStopMask for the stepper.
        }
        else
        {
            continue;
        }

        assert(mappingIndex < mappingCount);

        if (mapping->IsStackEmpty())
        {
            source = static_cast<ICorDebugInfo::SourceTypes>(source | ICorDebugInfo::STACK_EMPTY);
        }

        ICorDebugInfo::OffsetMapping& corMapping = mappings[mappingIndex++];

        corMapping.nativeOffset = nextNativeOffset;
        corMapping.ilOffset     = mapping->GetILOffset();
        corMapping.source       = source;
    }

#if 0
    // TODO-Review:
    // This check is disabled. It is always true that any time this check asserts, the debugger would have
    // a problem with IL source level debugging.  However, for a C# file, it only matters if things are on
    // different source lines. As a result, we have all sorts of latent problems with how we emit debug
    // info, but very few actual ones. Whenever someone wants to tackle that problem in general, turn this
    // assert back on.

    if (compiler->opts.compDbgCode)
    {
        // Assert that the first instruction of every basic block with more than one incoming edge
        // has a different sequence point from each incoming block.
        // It turns out that the only thing we really have to assert is that the first statement in
        // each basic block has an IL offset and appears in mappings.

        for (BasicBlock* block : compiler->Blocks())
        {
            Statement* stmt = block->firstStmt();

            if ((block->bbRefs > 1) && (stmt != nullptr))
            {
                bool found = false;

                if (stmt->GetILOffsetX() != BAD_IL_OFFSET)
                {
                    IL_OFFSET ilOffs = jitGetILoffs(stmt->GetILOffsetX());

                    for (unsigned i = 0; i < mappingCount; ++i)
                    {
                        if (mappings[i].ilOffset == ilOffs)
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

    DBEXEC(compiler->verbose || compiler->opts.dspDebugInfo, PrintOffsetMappings(mappings, mappingCount));

    compiler->info.compCompHnd->setBoundaries(compiler->info.compMethodHnd, mappingCount, mappings);
}

#ifndef TARGET_X86
// Generate code for a putArgStk whose source is a FIELD_LIST
// The x86 version of this is in codegenxarch.cpp, and doesn't take
// an outArgLclNum, as it pushes its args onto the stack.
// For fast tail calls the outgoing argument area is actually the
// method's own incoming argument area.
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

void CodeGen::GenRetFilt(GenTree* retfilt, BasicBlock* block)
{
    assert(retfilt->OperIs(GT_RETFILT));
    assert((block->bbJumpKind == BBJ_EHFILTERRET) || (block->bbJumpKind == BBJ_EHFINALLYRET));

    if (retfilt->TypeIs(TYP_VOID))
    {
        return;
    }

    assert(retfilt->TypeIs(TYP_INT));

    regNumber srcReg = UseReg(retfilt->AsUnOp()->GetOp(0));

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

void CodeGen::GenReturn(GenTree* ret, BasicBlock* block)
{
    assert(ret->OperIs(GT_RETURN));

    // Normally RETURN nodes appears at the end of RETURN blocks but sometimes the frontend fails
    // to properly cleanup after an unconditional throw and we end up with a THROW block having an
    // unreachable RETURN node at the end.
    assert((block->bbJumpKind == BBJ_RETURN) || (block->bbJumpKind == BBJ_THROW));
#ifdef FEATURE_EH_FUNCLETS
    assert(funCurrentFunc().kind == FUNC_ROOT);
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

        inst_Mov(retType, retReg, srcReg, /* canSkip */ true);
    }

    // Usually the epilog code follow right after the return code and since epilogs
    // aren't interruptible we don't need to report GC pointers in return registers.
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
    if ((block == compiler->genReturnBB) && compiler->compIsProfilerHookNeeded())
    {
        genProfilingLeaveCallback(CORINFO_HELP_PROF_FCN_LEAVE);
    }
#endif // PROFILING_SUPPORTED

#if defined(DEBUG) && defined(TARGET_XARCH)
    if (compiler->lvaReturnSpCheck != BAD_VAR_NUM)
    {
        genStackPointerCheck();
    }
#endif
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

    liveness.UpdateLife(this, store);
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

instruction CodeGen::ins_StoreFromSrc(regNumber srcReg, var_types dstType, bool aligned /*=false*/)
{
    assert(srcReg != REG_NA);

    bool dstIsFloatType = varTypeUsesFloatReg(dstType);
    bool srcIsFloatReg  = genIsValidFloatReg(srcReg);

    if (srcIsFloatReg == dstIsFloatType)
    {
        return ins_Store(dstType, aligned);
    }

    // We know that we are writing to memory, so make the destination type same
    // as the source type.
    var_types dstTypeForStore = TYP_UNDEF;

    switch (varTypeSize(dstType))
    {
        case 4:
            dstTypeForStore = srcIsFloatReg ? TYP_FLOAT : TYP_INT;
            break;
#ifdef TARGET_64BIT
        case 8:
            dstTypeForStore = srcIsFloatReg ? TYP_DOUBLE : TYP_LONG;
            break;
#endif
        default:
            assert(!"unexpected write to the stack.");
            break;
    }

    return ins_Store(dstTypeForStore, aligned);
}

void CodeGen::inst_Mov(var_types dstType, regNumber dstReg, regNumber srcReg, bool canSkip)
{
    GetEmitter()->emitIns_Mov(ins_Copy(srcReg, dstType), emitActualTypeSize(dstType), dstReg, srcReg, canSkip);
}

void CodeGen::genCopyRegIfNeeded(GenTree* node, regNumber needReg)
{
    assert((node->GetRegNum() != REG_NA) && (needReg != REG_NA));
    assert(!node->isUsedFromSpillTemp());

    inst_Mov(node->TypeGet(), needReg, node->GetRegNum(), /* canSkip */ true);
}

bool CodeGen::IsLocalMemoryOperand(GenTree* op, unsigned* lclNum, unsigned* lclOffs)
{
    if (op->isUsedFromSpillTemp())
    {
        assert(op->IsRegOptional());
        assert(op->IsRegSpilled(0));

        SpillTemp* temp = spillTemps.UseSpillTemp(op, 0);

        *lclNum  = temp->GetNum();
        *lclOffs = 0;

        return true;
    }

    assert(op->isContained());

    if (op->OperIs(GT_LCL_FLD))
    {
        *lclNum  = op->AsLclFld()->GetLclNum();
        *lclOffs = op->AsLclFld()->GetLclOffs();

        return true;
    }

    if (op->OperIs(GT_LCL_VAR))
    {
        assert(op->IsRegOptional() || !compiler->lvaGetDesc(op->AsLclVar())->IsRegCandidate());

        *lclNum  = op->AsLclVar()->GetLclNum();
        *lclOffs = 0;

        return true;
    }

    return false;
}

CodeGen::GenIntCastDesc::GenIntCastDesc(GenTreeCast* cast)
{
    GenTree* src = cast->GetOp(0);

    const var_types srcType      = varActualType(src->GetType());
    const bool      srcUnsigned  = cast->IsUnsigned();
    const unsigned  srcSize      = varTypeSize(srcType);
    const var_types castType     = cast->GetCastType();
    const bool      castUnsigned = varTypeIsUnsigned(castType);
    const unsigned  castSize     = varTypeSize(castType);
    const var_types dstType      = varActualType(cast->GetType());
    const unsigned  dstSize      = varTypeSize(dstType);
    const bool      overflow     = cast->gtOverflow();

    assert(cast->GetType() == varCastType(castType));
    assert((srcSize == 4) || (srcSize == varTypeSize(TYP_I_IMPL)));
    assert((dstSize == 4) || (dstSize == varTypeSize(TYP_I_IMPL)));

    assert(dstSize == varTypeSize(varActualType(castType)));

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

void CodeGen::GenCast(GenTreeCast* cast)
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

void CodeGen::GenJTrue(GenTreeUnOp* jtrue, BasicBlock* block)
{
    assert(jtrue->OperIs(GT_JTRUE));
    assert(block->bbJumpKind == BBJ_COND);

    GenTreeOp*   relop     = jtrue->GetOp(0)->AsOp();
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

    inst_JCC(condition, block->bbJumpDest);
}

void CodeGen::GenJCC(GenTreeCC* jcc, BasicBlock* block)
{
    assert(jcc->OperIs(GT_JCC));
    assert(block->KindIs(BBJ_COND));

    inst_JCC(jcc->GetCondition(), block->bbJumpDest);
}

void CodeGen::GenSetCC(GenTreeCC* setcc)
{
    assert(setcc->OperIs(GT_SETCC));

    inst_SETCC(setcc->GetCondition(), setcc->GetType(), setcc->GetRegNum());
    genProduceReg(setcc);
}

void CodeGen::GenLclAddr(GenTreeLclAddr* addr)
{
    assert(addr->TypeIs(TYP_BYREF, TYP_I_IMPL));

    // TODO-MIKE-Review: Shouldn't this simply be EA_PTRSIZE?
    emitAttr attr = emitTypeSize(addr->GetType());

    GetEmitter()->emitIns_R_S(INS_lea, attr, addr->GetRegNum(), addr->GetLclNum(), addr->GetLclOffs());
    DefReg(addr);
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

/*****************************************************************************
 * Determine the emitter code label for a block, for unwind purposes.
 */

insGroup* CodeGen::ehEmitLabel(BasicBlock* block)
{
    noway_assert(block != nullptr);

    insGroup* label;

#ifdef TARGET_ARM
    if ((block->bbFlags & BBF_FINALLY_TARGET) != 0)
    {
        // Use the offset of the beginning of the NOP padding, not the main block.
        // This might include loop head padding, too, if this is a loop head.
        label = block->unwindNopEmitLabel;
    }
    else
#endif
    {
        label = block->emitLabel;
    }

    noway_assert(label != nullptr);
    return label;
}

/*****************************************************************************
 * Determine the emitter code offset for a block. If the block is a finally
 * target, choose the offset of the NOP padding that precedes the block.
 */

uint32_t CodeGen::ehCodeOffset(BasicBlock* block)
{
    return ehEmitLabel(block)->GetCodeOffset();
}
