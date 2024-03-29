// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "instr.h"
#include "emit.h"
#include "codegen.h"
#include "gcinfotypes.h"
#include "unwind.h"
#ifdef LATE_DISASM
#include "disasm.h"
#endif

EmitterBase::EmitterBase(Compiler* compiler, CodeGen* codeGen, ICorJitInfo* jitInfo)
    : emitComp(compiler)
    , codeGen(codeGen)
#ifdef LATE_DISASM
    , disasm(new (emitComp, CMK_DebugOnly) DisAssembler(compiler, codeGen))
#endif
{
}

bool EmitterBase::InDifferentRegions(insGroup* ig1, insGroup* ig2)
{
    return ig1->IsCold() != ig2->IsCold();
}

static CodePos GetCodePos(unsigned num, unsigned codeOffset)
{
    assert(num <= UINT16_MAX);
    assert(codeOffset <= UINT16_MAX);

    return static_cast<CodePos>(num | (codeOffset << 16));
}

static unsigned GetInsNumFromCodePos(CodePos codePos)
{
    return static_cast<uint32_t>(codePos) & UINT16_MAX;
}

static unsigned GetInsOffsetFromCodePos(CodePos codePos)
{
    return static_cast<uint32_t>(codePos) >> 16;
}

CodePos EmitterBase::emitCurCodePos() const
{
    return GetCodePos(emitCurIGinsCnt, emitCurIGsize);
}

bool EmitterBase::IsCurrentLocation(const emitLocation& loc) const
{
    assert(loc.Valid());

    // TODO-MIKE-Review: This doesn't handle the group boundary case.
    return (loc.GetIG() == emitCurIG) && (loc.GetInsNum() == emitCurIGinsCnt);
}

bool EmitterBase::IsPreviousLocation(const emitLocation& loc) const
{
    assert(loc.Valid());

    insGroup* ig     = loc.GetIG();
    unsigned  insNum = loc.GetInsNum();

    if (ig == emitCurIG)
    {
        return insNum + 1 == emitCurIGinsCnt;
    }

    if (ig->igNext == emitCurIG)
    {
        return ((insNum == ig->igInsCnt) && (emitCurIGinsCnt == 1)) ||
               ((insNum + 1 == ig->igInsCnt) && (emitCurIGinsCnt == 0));
    }

    return false;
}

void emitLocation::CaptureLocation(const EmitterBase* emit)
{
    ig      = emit->emitCurIG;
    codePos = emit->emitCurCodePos();

    assert(Valid());
}

unsigned emitLocation::GetInsNum() const
{
    return GetInsNumFromCodePos(codePos);
}

uint32_t emitLocation::GetCodeOffset() const
{
    return ig->GetCodeOffset(codePos);
}

#ifdef DEBUG

void emitLocation::Print(const char* suffix) const
{
    unsigned insNum = GetInsNumFromCodePos(codePos);
    unsigned insOfs = GetInsOffsetFromCodePos(codePos);
    printf("(" FMT_IG ", ins %u, ofs %u)%s", ig->GetId(), insNum, insOfs, suffix == nullptr ? "" : suffix);
}

const char* EmitterBase::emitIfName(insFormat format)
{
    static const char* const ifNames[]{
#define IF_DEF(en, ...) "IF_" #en,
#include "emitfmts.h"
    };

    static char errBuff[32];

    if (format < _countof(ifNames))
    {
        return ifNames[format];
    }

    sprintf_s(errBuff, sizeof(errBuff), "??%u??", format);
    return errBuff;
}

#endif

const uint16_t emitTypeSizes[]{
#define DEF_TP(tn, nm, jitType, sz, sze, asze, al, tf) sze,
#include "typelist.h"
};

const uint16_t emitTypeActSz[]{
#define DEF_TP(tn, nm, jitType, sz, sze, asze, al, tf) asze,
#include "typelist.h"
};

void* EmitterBase::emitGetMem(size_t sz)
{
    assert(sz % sizeof(int) == 0);

    return emitComp->getAllocator(CMK_InstDesc).allocate<char>(sz);
}

#ifdef DEBUG
bool EmitterBase::IsCodeAligned(unsigned offset)
{
    return ((offset & (CODE_ALIGN - 1)) == 0);
}
#endif

insGroup* EmitterBase::emitAllocIG(unsigned num)
{
    assert(IsCodeAligned(emitCurCodeOffset));

    insGroup* ig = static_cast<insGroup*>(emitGetMem(sizeof(insGroup)));
    ig->igNext   = nullptr;
    ig->igData   = nullptr;
    ig->igNum    = num;
    ig->igOffs   = emitCurCodeOffset;
#ifdef FEATURE_EH_FUNCLETS
    ig->igFuncIdx = 0;
#endif
    ig->igSize    = 0;
    ig->igFlags   = 0;
    ig->igInsCnt  = 0;
    ig->gcLcls    = VarSetOps::UninitVal();
    ig->refRegs   = RBM_NONE;
    ig->byrefRegs = RBM_NONE;
#if FEATURE_LOOP_ALIGN
    ig->igLoopBackEdge = nullptr;
#endif

#if defined(DEBUG) || defined(LATE_DISASM)
    ig->igWeight    = getCurrentBlockWeight();
    ig->igPerfScore = 0.0;
#endif
#ifdef DEBUG
    ig->tryIndex = 0;
    new (&ig->igBlocks) jitstd::list<BasicBlock*>(emitComp->getAllocator(CMK_DebugOnly));
#endif

    return ig;
}

void EmitterBase::emitNewIG()
{
    assert((emitIGlast == emitCurIG) || (emitCurIG == nullptr));

    insGroup* ig = emitAllocIG(emitIGlast->igNum + 1);
    ig->igFlags |= (emitIGlast->igFlags & IGF_COLD);
#ifdef FEATURE_EH_FUNCLETS
    ig->igFuncIdx = emitIGlast->GetFuncletIndex();
#endif
#ifdef DEBUG
    ig->tryIndex = emitIGlast->tryIndex;
#endif

    emitIGlast->igNext = ig;
    emitIGlast         = ig;
    emitForceNewIG     = false;

    emitGenIG(ig);
}

void EmitterBase::emitAppendIG(insGroup* ig)
{
    assert(ig->igNum == 0);

    ig->igNum  = emitIGlast->igNum + 1;
    ig->igOffs = emitCurCodeOffset;

    emitIGlast->igNext = ig;
    emitIGlast         = ig;
    emitForceNewIG     = false;

    emitGenIG(ig);

    emitCurLabel = ig;
}

void EmitterBase::emitGenIG(insGroup* ig)
{
    assert((ig->igFlags & IGF_PLACEHOLDER) == 0);
    assert(emitCurIGjmpList == nullptr);
#if FEATURE_LOOP_ALIGN
    assert(emitCurIGAlignList == nullptr);
#endif

#if !FEATURE_FIXED_OUT_ARGS
    ig->igStkLvl = emitCurStackLvl;
#endif

    if (emitNoGCIG)
    {
        ig->igFlags |= IGF_NOGCINTERRUPT;
    }

    JITDUMP(FMT_IG ": offs %06XH, funclet %02u, weight %s\n", ig->GetId(), ig->igOffs, ig->GetFuncletIndex(),
            refCntWtd2str(ig->igWeight));

    emitCurIG         = ig;
    emitCurIGinsCnt   = 0;
    emitCurIGsize     = 0;
    emitCurIGfreeNext = emitCurIGfreeBase;
}

void EmitterBase::emitExtendIG()
{
    assert(!IsMainProlog(emitCurIG) && !emitCurIG->IsPrologOrEpilog());

    emitFinishIG(true);
    emitNewIG();

    emitCurIG->igFlags |= IGF_EXTEND;
}

void EmitterBase::emitFinishIG(bool extend)
{
    assert(emitCurIGfreeNext <= emitCurIGfreeEndp);

    size_t instrSize = emitCurIGfreeNext - emitCurIGfreeBase;
    size_t dataSize  = roundUp(instrSize);

    insGroup* ig = emitCurIG;
    assert((ig->igFlags & IGF_PLACEHOLDER) == 0);

    noway_assert(emitCurIGinsCnt < UINT8_MAX);
    noway_assert(emitCurIGsize < UINT16_MAX);

    // TODO-MIKE-Cleanup: Prologs can be empty, the memory allocator doesn't like 0 sized allocations.
    uint8_t* data = static_cast<uint8_t*>(emitGetMem(dataSize == 0 ? sizeof(void*) : dataSize));
    memcpy(data, emitCurIGfreeBase, instrSize);
    ig->igData   = data;
    ig->igInsCnt = static_cast<uint8_t>(emitCurIGinsCnt);
    ig->igSize   = static_cast<uint16_t>(emitCurIGsize);

    uint8_t* lastInsData = reinterpret_cast<uint8_t*>(emitLastIns);

    if ((emitCurIGfreeBase <= lastInsData) && (lastInsData < emitCurIGfreeNext))
    {
        emitLastIns = reinterpret_cast<instrDesc*>(data + (lastInsData - emitCurIGfreeBase));
    }

    emitCurCodeOffset += emitCurIGsize;
    assert(IsCodeAligned(emitCurCodeOffset));

#if FEATURE_LOOP_ALIGN
    if (emitCurIGAlignList != nullptr)
    {
        MoveAlignInstrList(ig);
    }
#endif

    if (emitCurIGjmpList != nullptr)
    {
        MoveJumpInstrList(ig);
    }

    emitCurIGfreeNext = emitCurIGfreeBase;

    JITDUMP(FMT_IG ": offs %06XH, size %04XH, funclet %02u, weight %s\n", ig->GetId(), ig->igOffs, ig->igSize,
            ig->GetFuncletIndex(), refCntWtd2str(ig->igWeight));
}

#if FEATURE_LOOP_ALIGN
void EmitterBase::MoveAlignInstrList(insGroup* ig)
{
    assert(emitCurIGAlignList != nullptr);

    instrDescAlign* list = nullptr;
    instrDescAlign* last = nullptr;

    while (emitCurIGAlignList != nullptr)
    {
        instrDescAlign* instr = emitCurIGAlignList;
        emitCurIGAlignList    = instr->idaNext;

        size_t          instrOffs = reinterpret_cast<uint8_t*>(instr) - emitCurIGfreeBase;
        instrDescAlign* newInstr  = reinterpret_cast<instrDescAlign*>(ig->igData + instrOffs);

        assert(newInstr->idIns() == INS_align);
        assert(newInstr->idaIG == ig);
        assert(newInstr->idaNext == instr->idaNext);

        newInstr->idaNext = list;
        list              = newInstr;

        if (last == nullptr)
        {
            last = newInstr;
        }
    }

    assert(last != nullptr);

    if (emitAlignList == nullptr)
    {
        assert(emitAlignLast == nullptr);

        last->idaNext = emitAlignList;
        emitAlignList = list;
    }
    else
    {
        last->idaNext          = nullptr;
        emitAlignLast->idaNext = list;
    }

    emitAlignLast = last;
}
#endif // FEATURE_LOOP_ALIGN

void EmitterBase::MoveJumpInstrList(insGroup* ig)
{
    assert(emitCurIGjmpList != nullptr);

    instrDescJmp* list = nullptr;
    instrDescJmp* last = nullptr;

    while (emitCurIGjmpList != nullptr)
    {
        instrDescJmp* instr = emitCurIGjmpList;
        emitCurIGjmpList    = instr->idjNext;

        size_t        instrOffs = reinterpret_cast<uint8_t*>(instr) - emitCurIGfreeBase;
        instrDescJmp* newInstr  = reinterpret_cast<instrDescJmp*>(ig->igData + instrOffs);

        assert(newInstr->idIns() == instr->idIns());
        assert(newInstr->idjIG == ig);
        assert(newInstr->idjNext == instr->idjNext);

        // Make sure the jumps are correctly ordered
        assert((last == nullptr) || (last->idjOffs > newInstr->idjOffs));
        // We don't generate any jumps in method epilogs and funclet prologs/epilogs,
        // these are generated out of order and we'd need to reorder the jumps.
        assert(!ig->IsFuncletPrologOrEpilog() && !ig->IsMainEpilog());

        newInstr->idjNext = list;
        list              = newInstr;

        if (last == nullptr)
        {
            last = newInstr;
        }
    }

    assert(last != nullptr);

    bool isPrologJump = IsMainProlog(ig);

    if ((emitJumpList == nullptr) || isPrologJump)
    {
        last->idjNext = emitJumpList;
        emitJumpList  = list;
    }
    else
    {
        last->idjNext         = nullptr;
        emitJumpLast->idjNext = list;
    }

    if (!isPrologJump || (emitJumpLast == nullptr))
    {
        emitJumpLast = last;
    }
}

#ifndef JIT32_GCENCODER
void EmitterBase::emitDisableGC()
{
    emitNoGCIG = true;

    if (emitCurIGnonEmpty())
    {
        emitExtendIG();
    }
    else
    {
        emitCurIG->igFlags |= IGF_NOGCINTERRUPT;
    }
}

void EmitterBase::emitEnableGC()
{
    emitNoGCIG = false;

    // The next time an instruction needs to be generated, force a new instruction group.
    // It will be an extend group in that case. Note that the next thing we see might be
    // a label, which will force a non-extend group.
    //
    // Note that we can't just create a new instruction group here, because we don't know
    // if there are going to be any instructions added to it, and we don't support empty
    // instruction groups.
    emitForceNewIG = true;
}
#endif // !JIT32_GCENCODER

void EmitterBase::emitBegFN()
{
#if !FEATURE_FIXED_OUT_ARGS
    emitCntStackDepth = 4;
#endif

#ifdef PSEUDORANDOM_NOP_INSERTION
    emitEnableRandomNops();
    emitComp->info.compRNG.Init(emitComp->info.compChecksum);
    emitNextNop = emitNextRandomNop();
#endif

#ifdef TARGET_ARMARCH
    // The only place where this limited instruction group size is a problem is the prolog,
    // where we only support a single instruction group. We should really fix that.
    // ARM32 and ARM64 both can require a bigger prolog instruction group. One scenario is
    // where a function uses all the incoming integer and single-precision floating-point
    // arguments, and must store them all to the frame on entry. If the frame is very large,
    // we generate ugly code like "movw r10, 0x488; add r10, sp; vstr s0, [r10]" for each
    // store, which eats up our insGroup buffer.
    constexpr size_t IG_BUFFER_SIZE = 100 * sizeof(EmitterBase::instrDesc) + 14 * sizeof(instrDescSmall);
#else
    constexpr size_t IG_BUFFER_SIZE = 50 * sizeof(EmitterBase::instrDesc) + 14 * sizeof(instrDescSmall);
#endif
    emitCurIGfreeBase = static_cast<uint8_t*>(emitGetMem(IG_BUFFER_SIZE));
    emitCurIGfreeEndp = emitCurIGfreeBase + IG_BUFFER_SIZE;

    // Create the first IG, it will be used for the prolog.
    emitIGfirst = emitAllocIG(1);
    emitIGfirst->igFlags |= IGF_PROLOG;
    emitIGlast = emitIGfirst;
    emitCurIG  = emitIGfirst;

    JITDUMP(FMT_IG ": offs %06XH, funclet %02u, weight %s\n", emitCurIG->GetId(), emitCurIG->igOffs,
            emitCurIG->GetFuncletIndex(), refCntWtd2str(emitCurIG->igWeight));

    // Append another group, to start generating the method body
    emitAppendIG(emitComp->fgFirstBB->emitLabel);
}

#ifdef PSEUDORANDOM_NOP_INSERTION
int EmitterBase::emitNextRandomNop()
{
    return emitComp->info.compRNG.Next(1, 9);
}
#endif

#if defined(DEBUG) || defined(LATE_DISASM)

float Encoder::insEvaluateExecutionCost(instrDesc* id)
{
    assert(id->idInsFmt() != IF_GC_REG);

    insExecutionCharacteristics result        = getInsExecutionCharacteristics(id);
    float                       throughput    = result.insThroughput;
    float                       latency       = result.insLatency;
    unsigned                    memAccessKind = result.insMemoryAccessKind;

    // Check for PERFSCORE_THROUGHPUT_ILLEGAL and PERFSCORE_LATENCY_ILLEGAL.
    // Note that 0.0 throughput is allowed for pseudo-instructions in
    // the instrDesc list that won't actually generate code.
    assert(throughput >= 0.0);
    assert(latency >= 0.0);

    if (memAccessKind == PERFSCORE_MEMORY_WRITE)
    {
        // We assume that we won't read back from memory for the next WR_GENERAL cycles
        // Thus we normally won't pay latency costs for writes.
        latency = max(0.0f, latency - PERFSCORE_LATENCY_WR_GENERAL);
    }
    else if (latency >= 1.0) // Otherwise, If we aren't performing a memory write
    {
        // We assume that the processor's speculation will typically eliminate one cycle of latency
        latency -= 1.0;
    }

    return max(throughput, latency);
}

void Encoder::perfScoreUnhandledInstruction(instrDesc* id, insExecutionCharacteristics* pResult)
{
#ifdef DEBUG
    printf("PerfScore: unhandled instruction: %s, format %s", insName(id->idIns()),
           EmitterBase::emitIfName(id->idInsFmt()));
    assert(!"PerfScore: unhandled instruction");
#endif

    pResult->insThroughput = PERFSCORE_THROUGHPUT_1C;
    pResult->insLatency    = PERFSCORE_LATENCY_1C;
}

BasicBlock::weight_t EmitterBase::getCurrentBlockWeight()
{
    if (BasicBlock* block = codeGen->GetCurrentBlock())
    {
        return block->getBBWeight(emitComp);
    }

    // prolog or epilog case, so just use the standard weight
    return BB_UNITY_WEIGHT;
}
#endif // defined(DEBUG) || defined(LATE_DISASM)

void EmitterBase::dispIns(instrDesc* id)
{
#ifdef DEBUG
    assert(id->idDebugOnlyInfo()->idSize == id->GetDescSize());
    assert(emitCurIGfreeNext - reinterpret_cast<uint8_t*>(id) == static_cast<ssize_t>(id->GetDescSize()));
#ifdef TARGET_XARCH
    assert((id->idCodeSize() != 0) || id->InstrHasNoCode());
#endif
#if !FEATURE_FIXED_OUT_ARGS
    assert(emitCurStackLvl <= INT32_MAX);
#endif

    emitInsSanityCheck(id);

    if (emitComp->verbose)
    {
        JITDUMP("IN%04X: %06X ", id->idDebugOnlyInfo()->idNum, emitCurCodeOffset + emitCurIGsize);
        static_cast<ArchEmitter*>(this)->PrintIns(id);
    }
#endif
}

void EmitterBase::appendToCurIG(instrDesc* id)
{
    emitCurIGsize += id->idCodeSize();
}

#ifdef DEBUG

void Encoder::PrintInsAddr(const uint8_t* code) const
{
    if (emitComp->opts.disAddr)
    {
        printf(FMT_ADDR, DBG_ADDR(code));
    }

    if (emitComp->opts.dspGCtbls)
    {
        printf("%06X", emitCurCodeOffs(code));
    }
    else
    {
        printf("      ");
    }
}

#endif // DEBUG

EmitterBase::instrDescSmall* EmitterBase::emitAllocAnyInstr(unsigned sz, bool updateLastIns)
{
    assert(sz >= sizeof(instrDescSmall));

#ifdef DEBUG
    // Under STRESS_EMITTER, put every instruction in its own instruction group.
    // We can't do this for a prolog, epilog, funclet prolog, or funclet epilog,
    // because those are generated out of order. We currently have a limitation
    // where the jump shortening pass uses the instruction group number to determine
    // if something is earlier or later in the code stream. This implies that
    // these groups cannot be more than a single instruction group. Note that
    // the prolog/epilog placeholder groups ARE generated in order, and are
    // re-used. But generating additional groups would not work.
    if (emitComp->compStressCompile(Compiler::STRESS_EMITTER, 1) && (emitCurIGinsCnt != 0) &&
        !emitCurIG->IsPrologOrEpilog())
    {
        emitExtendIG();
    }
#endif

#ifdef PSEUDORANDOM_NOP_INSERTION
    // TODO-ARM-Bug?: PSEUDORANDOM_NOP_INSERTION is not defined for TARGET_ARM
    //     ARM - This is currently broken on TARGET_ARM
    //     When nopSize is odd we misalign emitCurIGsize
    //
    if (!emitComp->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT) && !emitInInstrumentation &&
        !emitCurIG->IsPrologOrEpilog() &&
        emitRandomNops // sometimes we turn off where exact codegen is needed (pinvoke inline)
        )
    {
        if (emitNextNop == 0)
        {
            int nopSize           = 4;
            emitInInstrumentation = true;
            instrDesc* idnop      = emitNewInstr();
            emitInInstrumentation = false;
            idnop->idInsFmt(IF_NONE);
            idnop->idIns(INS_nop);
#ifdef TARGET_XARCH
            idnop->idCodeSize(nopSize);
#else
#error "Undefined target for pseudorandom NOP insertion"
#endif

            emitCurIGsize += nopSize;
            emitNextNop = emitNextRandomNop();
        }
        else
        {
            emitNextNop--;
        }
    }
#endif // PSEUDORANDOM_NOP_INSERTION

    assert(IsCodeAligned(emitCurIGsize));

    if ((emitCurIGfreeNext + sz >= emitCurIGfreeEndp) || emitForceNewIG)
    {
        emitExtendIG();
    }

    instrDescSmall* id = reinterpret_cast<instrDescSmall*>(emitCurIGfreeNext);

    if (updateLastIns)
    {
        emitLastIns      = static_cast<instrDesc*>(id);
        emitLastInsLabel = emitCurLabel;
    }

    emitCurIGfreeNext += sz;
    emitCurIGinsCnt++;

#ifdef DEBUG
    if (BasicBlock* block = codeGen->GetCurrentBlock())
    {
        if (emitCurIG->igBlocks.empty() || (emitCurIG->igBlocks.back() != block))
        {
            emitCurIG->igBlocks.push_back(block);
        }
    }
#endif

    return id;
}

#ifdef DEBUG

// Make sure the code offsets of all instruction groups look reasonable.
// Note: It checks that each instruction group starts right after the previous ig.
// For the first cold ig offset is also should be the last hot ig + its size.
// emitCurCodeOffs maintains distance for the split case to look like they are consistent.
// Also it checks total code size.
void EmitterBase::emitCheckIGoffsets()
{
    size_t currentOffset = 0;

    for (insGroup* tempIG = emitIGfirst; tempIG != nullptr; tempIG = tempIG->igNext)
    {
        assert(IsCodeAligned(tempIG->igOffs));

        if (tempIG->igOffs != currentOffset)
        {
            printf("Block #%u has offset %08X, expected %08X\n", tempIG->GetId(), tempIG->igOffs, currentOffset);
            assert(!"bad block offset");
        }

        currentOffset += tempIG->igSize;
    }

    if ((GetCodeSize() != 0) && (GetCodeSize() != currentOffset))
    {
        printf("Total code size is %08X, expected %08X\n", GetCodeSize(), currentOffset);

        assert(!"bad total code size");
    }
}

#endif // DEBUG

void EmitterBase::emitBegProlog()
{
    assert(codeGen->generatingProlog);

    if (emitCurIGnonEmpty())
    {
        emitFinishIG();
    }
    else
    {
        assert(emitCurIG == nullptr);
    }

#if !FEATURE_FIXED_OUT_ARGS
    // Don't measure stack depth inside the prolog, it's misleading.
    assert(emitCurStackLvl == 0);
    emitCntStackDepth = 0;
#endif

    emitNoGCIG     = true;
    emitForceNewIG = false;

    emitGenIG(GetProlog());
}

unsigned EmitterBase::emitGetCurrentPrologCodeSize()
{
    assert(IsMainProlog(emitCurIG) || emitCurIG->IsFuncletProlog());

    return emitCurIGsize;
}

void EmitterBase::MarkMainPrologNoGCEnd()
{
    assert(codeGen->generatingProlog);
    assert(IsMainProlog(emitCurIG));

    mainPrologNoGCEndCodePos = emitCurCodePos();
}

void EmitterBase::EndMainProlog()
{
    assert(codeGen->generatingProlog);

    emitNoGCIG = false;

    emitFinishIG();

#if !FEATURE_FIXED_OUT_ARGS
    emitCurStackLvl   = 0;
    emitCntStackDepth = 4;
#endif
}

// For AMD64 the maximum prolog/epilog size supported on the OS is 256 bytes
// Since it is incorrect for us to be jumping across funclet prolog/epilogs
// we will use the following estimate as the maximum placeholder size.
static constexpr unsigned MAX_PLACEHOLDER_IG_SIZE = 256;

#ifdef FEATURE_EH_FUNCLETS
insGroup* EmitterBase::ReserveFuncletProlog(BasicBlock* block)
{
    assert(!IsMainProlog(emitCurIG));
    assert(codeGen->funGetFuncIdx(block) == codeGen->GetCurrentFuncletIndex());

    // We should already have an empty group added by DefineBlockLabel for the first
    // block in the funclet. We'll use that for the funclet prolog and create another
    // one for the funclet body.
    assert(!emitCurIGnonEmpty());
    assert(emitCurIG->GetFuncletIndex() == codeGen->GetCurrentFuncletIndex());

    // Currently, no registers are live on entry to the prolog, except maybe
    // the exception object. There might be some live stack vars, but they
    // cannot be accessed until after the frame pointer is re-established.
    // In order to potentially prevent emitting a death before the prolog
    // and a birth right after it, we just report it as live during the
    // prolog, and rely on the prolog being non-interruptible. Trust
    // genCodeForBBlist to correctly initialize all the sets.
    //
    // We might need to relax these asserts if the VM ever starts
    // restoring any registers, then we could have live-in reg vars.
    noway_assert((emitCurIG->refRegs & RBM_EXCEPTION_OBJECT) == emitCurIG->refRegs);
    noway_assert(emitCurIG->byrefRegs == RBM_NONE);

    JITDUMP("Reserving " FMT_IG " for block " FMT_BB " funclet prolog\n", emitCurIG->GetId(), block->bbNum);

    if (emitComp->opts.compDbgInfo)
    {
        codeGen->genIPmappingAdd(ICorDebugInfo::PROLOG, true);
    }

    insGroup* ig = emitCurIG;
    ig->igPhData = block;
    ig->igFlags |= IGF_PLACEHOLDER | IGF_PROLOG;

    // We don't know what code size the placeholder insGroup will have,
    // just use an estimate large enough to accommodate any placeholder.
    assert(emitCurIGsize == 0);
    emitCurIGsize = MAX_PLACEHOLDER_IG_SIZE;
    emitCurCodeOffset += MAX_PLACEHOLDER_IG_SIZE;

    emitNewIG();

    // Nothing is really live in the prolog, since it's not interruptible, but if
    // we kill everything at the start of the prolog we may end up creating new
    // live ranges for whatever GC locals happen to be live before the funclet and
    // inside the funclet so may as well pretend that whatever is live at entry
    // is also live inside prolog. So the group following the prolog is really an
    // extension, since GC liveness does not change.
    emitCurIG->igFlags |= IGF_EXTEND;

    return ig;
}
#endif // FEATURE_EH_FUNCLETS

insGroup* EmitterBase::ReserveEpilog(BasicBlock* block)
{
    assert(!IsMainProlog(emitCurIG));

#ifdef TARGET_AMD64
    // We're about to create an epilog. If the last instruction we output was a 'call',
    // then we need to insert a NOP, to allow for proper exception handling behavior.
    if (static_cast<X86Emitter*>(this)->IsLastInsCall())
    {
        static_cast<X86Emitter*>(this)->emitIns(INS_nop);
    }
#endif

#ifdef FEATURE_EH_FUNCLETS
    assert(block->KindIs(BBJ_RETURN, BBJ_EHCATCHRET, BBJ_EHFINALLYRET, BBJ_EHFILTERRET));
    assert(emitCurIG->GetFuncletIndex() == codeGen->GetCurrentFuncletIndex());
    const bool isFunclet = !block->KindIs(BBJ_RETURN);
#else
    assert(block->KindIs(BBJ_RETURN));
    const bool isFunclet   = false;
#endif

    if (emitCurIGnonEmpty())
    {
        emitExtendIG();
    }
    else
    {
        emitCurIG->igFlags |= IGF_EXTEND;
        // We may be "stealing" the insGroup created for an empty basic
        // block, to avoid confusion remove the basic block flag.
        emitCurIG->igFlags &= ~IGF_BASIC_BLOCK;
    }

    JITDUMP("Reserving " FMT_IG " for block " FMT_BB " %sepilog\n", emitCurIG->GetId(), block->bbNum,
            isFunclet ? "funclet " : "");

    // We assume that the epilog is the end of any currently in progress no-GC region.
    // If a block after the epilog needs to be no-GC, it needs to call emitDisableGC
    // directly. This behavior is depended upon by the fast tailcall implementation,
    // which disables GC at the beginning of argument setup, but assumes that after
    // the epilog it will be re-enabled.
    emitNoGCIG = false;

#ifdef FEATURE_EH_FUNCLETS
    // Add the appropriate IP mapping debugging record for this placeholder
    // group. genExitCode() adds the mapping for main function epilogs.
    if (emitComp->opts.compDbgInfo && isFunclet)
    {
        codeGen->genIPmappingAdd(ICorDebugInfo::EPILOG, true);
    }
#endif

    insGroup* ig = emitCurIG;
    ig->igPhData = block;
    ig->igFlags |= IGF_PLACEHOLDER | IGF_EPILOG;

    // We don't know what code size the placeholder insGroup will have,
    // just use an estimate large enough to accommodate any placeholder.
    assert(emitCurIGsize == 0);
    emitCurIGsize = MAX_PLACEHOLDER_IG_SIZE;
    emitCurCodeOffset += MAX_PLACEHOLDER_IG_SIZE;

    emitCurIG = nullptr;

    return ig;
}

BasicBlock* EmitterBase::BeginPrologEpilog(insGroup* ig)
{
    assert((ig->igFlags & IGF_PLACEHOLDER) != 0);
    assert(!emitCurIGnonEmpty());

    JITDUMP("\n=============== Generating%s%s\n", ig->GetFuncletIndex() == 0 ? "" : " funclet",
            ig->IsProlog() ? " prolog" : " epilog");

    BasicBlock* block = ig->igPhData;
    ig->igFlags &= ~IGF_PLACEHOLDER;
    ig->igPhData = nullptr;

    emitNoGCIG     = true;
    emitForceNewIG = false;

    codeGen->funSetCurrentFunc(ig->GetFuncletIndex());

    emitGenIG(ig);

#if !FEATURE_FIXED_OUT_ARGS
    // Don't measure stack depth inside the prolog / epilog, it's misleading.
    assert(emitCurStackLvl == 0);
    emitCntStackDepth = 0;
#endif

    return block;
}

void EmitterBase::EndPrologEpilog()
{
    emitNoGCIG = false;

    assert(emitCurIGnonEmpty());
    emitFinishIG();

    assert(emitCurIGsize <= MAX_PLACEHOLDER_IG_SIZE);

#if !FEATURE_FIXED_OUT_ARGS
    emitCurStackLvl   = 0;
    emitCntStackDepth = TARGET_POINTER_SIZE;
#endif

    emitCurIG = nullptr;
}

#ifdef JIT32_GCENCODER
void EmitterBase::BeginGCEpilog()
{
    epilogCount++;

    Epilog* e = new (emitComp, CMK_GC) Epilog();

    if (lastEpilog != nullptr)
    {
        lastEpilog->next = e;
    }
    else
    {
        firstEpilog = e;
    }

    lastEpilog = e;
}

void EmitterBase::MarkGCEpilogStart() const
{
    assert(lastEpilog != nullptr);
    lastEpilog->startLoc.CaptureLocation(this);
}

void EmitterBase::MarkGCEpilogExit()
{
    assert(codeGen->generatingEpilog);
    epilogExitLoc.CaptureLocation(this);
}

void EmitterBase::EndGCEpilog(insGroup* ig)
{
    assert(lastEpilog != nullptr);

    // Note: We compute all this before instructions are actually encoded,
    // thus these may not be the final code offsets. But we only care about
    // the distance between these locations and we don't expect instructions
    // that are part of the epilog to change size (e.g. there are no branches).

    // TODO-MIKE-Review: Given the very low supported epilog count, we could
    // store the exit location in Epilog and deal with all this at the
    // end of instruction encoding. Anyway we need to get the start offset
    // again at that point.

    uint32_t startOffset = lastEpilog->startLoc.GetCodeOffset();
    uint32_t exitOffset  = epilogExitLoc.GetCodeOffset();
    uint32_t endOffset   = ig->GetCodeOffset(emitCurCodePos());

    uint32_t newCommonSize = exitOffset - startOffset;
    // All epilogs must be identical, this the exception of the "exit" instruction.
    assert((epilogCommonSize == 0) || (epilogCommonSize == newCommonSize));
    epilogCommonSize = newCommonSize;

    unsigned newExitSize = endOffset - exitOffset;
    assert(newExitSize != 0);

    if (epilogExitSize == 0)
    {
        epilogExitSize = newExitSize;
    }
    else if (newExitSize < epilogExitSize)
    {
        // We expect either the epilog to be the same every time, with the exception
        // of the "exit" instruction, which may be either RET or JMP (for tail calls).
        // We take the minimum size of all exits to include in the common epilog size.
        // This ONLY works because the only instruction is the last one and thus a
        // slight underestimation of the epilog size is harmless (since the EIP can
        // not be between instructions).

        // RET can have 1 byte, JMP can have 6 bytes.
        assert(epilogExitSize - newExitSize <= 5);

        epilogExitSize = newExitSize;
    }
}

bool EmitterBase::HasSingleEpilogAtEnd()
{
    return (epilogCount == 1) && emitIGlast->IsMainEpilog(); // This wouldn't work for funclets
}

#endif // JIT32_GCENCODER

insGroup* EmitterBase::CreateBlockLabel(BasicBlock* block, unsigned funcletIndex)
{
    insGroup* ig = emitAllocIG(0);
#ifdef FEATURE_EH_FUNCLETS
    assert(funcletIndex <= UINT16_MAX);
    ig->igFuncIdx = static_cast<uint16_t>(funcletIndex);
#endif
#if defined(DEBUG) || defined(LATE_DISASM)
    ig->igWeight = block->getBBWeight(emitComp);
#endif
#ifdef DEBUG
    ig->tryIndex = block->bbTryIndex;
#endif
    return ig;
}

void EmitterBase::DefineBlockLabel(insGroup* label)
{
    assert(!IsMainProlog(emitCurIG));

    if (label->IsDefined())
    {
        assert(emitCurIG == label);
        assert(emitCurLabel == label);
    }
    else
    {
        if (emitCurIGnonEmpty())
        {
            emitFinishIG();
        }

        emitAppendIG(label);
        emitCurLabel = label;
    }

    if (label->IsCold() && (emitFirstColdIG == nullptr))
    {
        JITDUMP("\nThis is the start of the cold region of the method\n");
        emitFirstColdIG = label;
    }
}

insGroup* EmitterBase::CreateTempLabel()
{
    insGroup* label = emitAllocIG(0);
    label->igFlags |= (emitCurIG->igFlags & IGF_COLD);
#ifdef FEATURE_EH_FUNCLETS
    label->igFuncIdx = emitCurIG->GetFuncletIndex();
#endif
#ifdef DEBUG
    label->tryIndex = emitCurIG->tryIndex;
#endif
    return label;
}

void EmitterBase::DefineTempLabel(insGroup* label)
{
    assert(!IsMainProlog(emitCurIG));
#ifdef FEATURE_EH_FUNCLETS
    assert(label->GetFuncletIndex() == emitCurIG->GetFuncletIndex());
#endif

    emitFinishIG();
    emitAppendIG(label);
#if defined(DEBUG) || defined(LATE_DISASM)
    label->igWeight = getCurrentBlockWeight();
#endif
    SetLabelGCLiveness(label);
}

insGroup* EmitterBase::DefineTempLabel()
{
    assert(!IsMainProlog(emitCurIG));

    if (emitCurIGnonEmpty())
    {
        emitFinishIG();
        emitNewIG();
    }

    emitCurLabel = emitCurIG;
    SetLabelGCLiveness(emitCurIG);

    return emitCurIG;
}

void EmitterBase::SetLabelGCLiveness(insGroup* label)
{
    assert(!label->IsExtension());

    label->gcLcls    = VarSetOps::MakeCopy(emitComp, codeGen->liveness.GetGCLiveSet());
    label->refRegs   = static_cast<uint32_t>(codeGen->liveness.GetGCRegs(TYP_REF));
    label->byrefRegs = static_cast<uint32_t>(codeGen->liveness.GetGCRegs(TYP_BYREF));

#ifdef DEBUG
    if (emitComp->verbose)
    {
        printf(FMT_IG ", gc-lcls ", label->GetId());
        dumpConvertedVarSet(emitComp, label->gcLcls);
        printf(", ref-regs");
        DumpRegSet(label->refRegs);
        printf(", byref-regs");
        DumpRegSet(label->byrefRegs);
        printf("\n");
    }
#endif
}

insGroup* EmitterBase::DefineInlineTempLabel()
{
    assert(!IsMainProlog(emitCurIG));

    if (emitCurIGnonEmpty())
    {
        emitExtendIG();
    }

    emitCurLabel = emitCurIG;

    return emitCurIG;
}

void EmitterBase::DefineInlineTempLabel(insGroup* label)
{
    assert(!IsMainProlog(emitCurIG));
#ifdef FEATURE_EH_FUNCLETS
    assert(label->GetFuncletIndex() == emitCurIG->GetFuncletIndex());
#endif

    emitFinishIG(true);
    emitAppendIG(label);
#if defined(DEBUG) || defined(LATE_DISASM)
    label->igWeight = getCurrentBlockWeight();
#endif
    label->igFlags |= IGF_EXTEND;
}

#ifdef DEBUG

void AsmPrinter::PrintLabel(insGroup* ig)
{
    printf("G_M%03u_IG%02u", compiler->compMethodID, ig->GetId());
}

static const char* emitLabelString(Compiler* compiler, insGroup* ig)
{
    const int       TEMP_BUFFER_LEN = 40;
    static unsigned curBuf          = 0;
    static char     buf[4][TEMP_BUFFER_LEN];
    const char*     retbuf;

    sprintf_s(buf[curBuf], TEMP_BUFFER_LEN, "G_M%03u_IG%02u", compiler->compMethodID, ig->GetId());
    retbuf = buf[curBuf];
    curBuf = (curBuf + 1) % 4;
    return retbuf;
}

#endif // DEBUG

#ifdef TARGET_ARMARCH

// Given an instruction group, find the array of instructions (instrDesc) and
// number of instructions in the array. If the IG is the current IG, we assume
// that igData does NOT hold the instructions; they are unsaved and pointed
// to by emitCurIGfreeBase.
// This function can't be called for placeholder groups, which have no instrDescs.
void EmitterBase::emitGetInstrDescs(insGroup* ig, instrDesc** id, int* insCnt)
{
    assert((ig->igFlags & IGF_PLACEHOLDER) == 0);

    if (ig == emitCurIG)
    {
        *id     = reinterpret_cast<instrDesc*>(emitCurIGfreeBase);
        *insCnt = emitCurIGinsCnt;
    }
    else
    {
        *id     = reinterpret_cast<instrDesc*>(ig->igData);
        *insCnt = ig->igInsCnt;
    }

    assert(*id);
}

// Given a location (an 'emitLocation'), find the instruction group (IG) and
// instruction descriptor (instrDesc) corresponding to that location. Returns
// 'true' if there is an instruction, 'false' if there is no instruction
// (i.e., we're at the end of the instruction list). Also, optionally return
// the number of instructions that follow that instruction in the IG (in *pinsRemaining,
// if pinsRemaining is non-NULL), which can be used for iterating over the
// remaining instrDescs in the IG.
//
// We assume that emitCurIG points to the end of the instructions we care about.
// For the prologs or epilogs, it points to the last IG of the prolog or epilog
// that is being generated. For body code gen, it points to the place we are currently
// adding code, namely, the end of currently generated code.
bool EmitterBase::emitGetLocationInfo(const emitLocation& emitLoc, insGroup** pig, instrDesc** pid, int* pinsRemaining)
{
    assert(emitLoc.Valid());
    assert(pig != nullptr);
    assert(pid != nullptr);

    insGroup*  ig = emitLoc.GetIG();
    instrDesc* id;
    int        insNum = emitLoc.GetInsNum();
    int        insCnt;

    emitGetInstrDescs(ig, &id, &insCnt);
    assert(insNum <= insCnt);

    // There is a special-case: if the insNum points to the end, then we "wrap" and
    // consider that the instruction it is pointing at is actually the first instruction
    // of the next non-empty IG (which has its own valid emitLocation). This handles the
    // case where you capture a location, then the next instruction creates a new IG.

    if (insNum == insCnt)
    {
        if (ig == emitCurIG)
        {
            // No instructions beyond the current location.
            return false;
        }

        for (ig = ig->igNext; ig; ig = ig->igNext)
        {
            emitGetInstrDescs(ig, &id, &insCnt);

            if (insCnt > 0)
            {
                insNum = 0; // Pretend the index is 0 -- the first instruction
                break;
            }

            if (ig == emitCurIG)
            {
                // There aren't any instructions in the current IG, and this is
                // the current location, so we're at the end.
                return false;
            }
        }

        if (ig == nullptr)
        {
            // 'ig' can't be NULL, or we went past the current IG represented by 'emitCurIG'.
            // Perhaps 'loc' was corrupt coming in?
            noway_assert(!"corrupt EmitterBase location");
            return false;
        }
    }

    // Now find the instrDesc within this group that corresponds to the location

    assert(insNum < insCnt);

    int i;
    for (i = 0; i != insNum; ++i)
    {
        castto(id, uint8_t*) += id->GetDescSize();
    }

    // Return the info we found

    *pig = ig;
    *pid = id;

    if (pinsRemaining)
    {
        *pinsRemaining = insCnt - insNum - 1;
    }

    return true;
}

bool EmitterBase::GetNextInstr(insGroup*& ig, instrDesc*& id, int& insRemaining)
{
    if (insRemaining > 0)
    {
        id = reinterpret_cast<instrDesc*>(reinterpret_cast<uint8_t*>(id) + id->GetDescSize());
        insRemaining--;

        return true;
    }

    if (ig == emitCurIG)
    {
        return false;
    }

    for (ig = ig->igNext; ig != nullptr; ig = ig->igNext)
    {
        int insCount;
        emitGetInstrDescs(ig, &id, &insCount);

        if (insCount > 0)
        {
            insRemaining = insCount - 1;

            return true;
        }

        if (ig == emitCurIG)
        {
            return false;
        }
    }

    return false;
}

void EmitterBase::WalkInstr(const emitLocation& fromLoc, WalkInstrCallback callback, void* context)
{
    insGroup*  ig;
    instrDesc* id;
    int        insRemaining;

    if (!emitGetLocationInfo(fromLoc, &ig, &id, &insRemaining))
    {
        return;
    }

    do
    {
        (*callback)(id, context);
    } while (GetNextInstr(ig, id, insRemaining));
}

void EmitterBase::emitUnwindNopPadding(const emitLocation& fromLoc)
{
    WalkInstr(fromLoc, [](instrDesc* id,
                          void* context) { static_cast<CodeGen*>(context)->unwindNop(ARM_ONLY(id->idCodeSize())); },
              codeGen);
}

#endif // TARGET_ARMARCH

EmitterBase::instrDesc* ArchEmitter::emitNewInstrCall(CORINFO_METHOD_HANDLE methodHandle,
                                                      emitAttr              retRegAttr
#if MULTIREG_HAS_SECOND_GC_RET
                                                      ,
                                                      emitAttr retReg2Attr
#endif
#ifdef TARGET_X86
                                                      ,
                                                      int argSlotCount
#endif
#ifdef TARGET_XARCH
                                                      ,
                                                      int32_t disp
#endif
                                                      )
{
    CorInfoHelpFunc helper       = Compiler::eeGetHelperNum(methodHandle);
    bool            isNoGCHelper = (helper != CORINFO_HELP_UNDEF) && GCInfo::IsNoGCHelper(helper);
    regMaskTP       savedRegs    = isNoGCHelper ? GCInfo::GetNoGCHelperCalleeSavedRegs(helper) : RBM_CALLEE_SAVED;
    VARSET_TP       gcLcls       = codeGen->liveness.GetGCLiveSet();
    regMaskTP       refRegs      = codeGen->liveness.GetGCRegs(TYP_REF) & savedRegs;
    regMaskTP       byrefRegs    = codeGen->liveness.GetGCRegs(TYP_BYREF) & savedRegs;

#ifdef DEBUG
    if (emitComp->verbose)
    {
        if (isNoGCHelper)
        {
            printf("NoGC Call: saved regs");
            DumpRegSet(savedRegs);
            printf("\n");
        }

        printf("Call: gc-lcls ");
        dumpConvertedVarSet(emitComp, gcLcls);
        printf(", ref-regs");
        DumpRegSet(refRegs);
        printf(", byref-regs");
        DumpRegSet(byrefRegs);
        printf("\n");
    }
#endif

    if (retRegAttr == EA_UNKNOWN)
    {
        retRegAttr = EA_PTRSIZE;
    }

    instrDesc* id;

    if (!VarSetOps::IsEmpty(emitComp, gcLcls) || ((refRegs & RBM_CALLEE_TRASH) != RBM_NONE) || (byrefRegs != RBM_NONE)
#if MULTIREG_HAS_SECOND_GC_RET
        || EA_IS_GCREF_OR_BYREF(retReg2Attr)
#endif
#ifdef TARGET_X86
        || !instrDesc::fitsInSmallCns(argSlotCount)
#endif
#ifdef TARGET_XARCH
        || emitAddrMode::IsLargeDisp(disp)
#endif
            )
    {
        if (EA_IS_GCREF(retRegAttr))
        {
            refRegs |= RBM_INTRET;
        }
        else if (EA_IS_BYREF(retRegAttr))
        {
            byrefRegs |= RBM_INTRET;
        }

#if MULTIREG_HAS_SECOND_GC_RET
        if (EA_IS_GCREF(retReg2Attr))
        {
            refRegs |= RBM_INTRET_1;
        }
        else if (EA_IS_BYREF(retReg2Attr))
        {
            byrefRegs |= RBM_INTRET_1;
        }
#endif

        instrDescCGCA* idc = emitAllocInstrCGCA();
        idc->idSetIsLargeCall();
        idc->idcGCvars    = VarSetOps::MakeCopy(emitComp, gcLcls);
        idc->idcGcrefRegs = refRegs;
        idc->idcByrefRegs = byrefRegs;
        idc->idOpSize(EA_SIZE(retRegAttr));
        idc->idGCref(EA_GC_TYPE(retRegAttr));
#ifdef TARGET_XARCH
        idc->idcDisp = disp;
#endif
#ifdef TARGET_X86
        idc->idcArgCnt = argSlotCount;
#endif
        id = idc;
    }
    else
    {
        if (VarSetOps::MayBeUninit(emitEmptyGCrefVars))
        {
            emitEmptyGCrefVars = VarSetOps::MakeEmpty(emitComp);
        }

#ifdef TARGET_X86
        id = emitNewInstrCns(argSlotCount);
#else
        id                 = emitNewInstr();
#endif
        id->idOpSize(EA_SIZE(retRegAttr));
        id->idGCref(EA_GC_TYPE(retRegAttr));
        EncodeCallGCRegs(refRegs, id);
#ifdef TARGET_XARCH
        id->idAddr()->iiaAddrMode.disp = disp;
        assert(id->idAddr()->iiaAddrMode.disp == disp);
#endif
    }

    id->idSetIsNoGC(isNoGCHelper);

    return id;
}

#ifdef DEBUG

void insGroup::Print(Compiler* compiler) const
{
    const insGroup* ig = this;

    char buff[40];
    sprintf_s(buff, _countof(buff), FMT_IG ": ", ig->GetId());
    printf("%s", buff);

    char     separator = ';';
    unsigned flags     = ig->igFlags;

    if (compiler->verbose)
    {
        printf("%c func %u, offs %06XH, size %04XH", separator, ig->GetFuncletIndex(), ig->igOffs, ig->igSize);
        separator = ',';

        if ((flags & IGF_UPD_ISZ) != 0)
        {
            printf("%c update-size", separator);
            separator = ',';
        }
    }

    if (ig->IsMainProlog())
    {
        printf("%c prolog", separator);
        separator = ',';
    }
    else if (ig->IsMainEpilog())
    {
        printf("%c epilog", separator);
        separator = ',';
    }
#ifdef FEATURE_EH_FUNCLETS
    else if (ig->IsFuncletProlog())
    {
        printf("%c funclet-prolog", separator);
        separator = ',';
    }
    else if (ig->IsFuncletEpilog())
    {
        printf("%c funclet-epilog", separator);
        separator = ',';
    }
#endif

    if ((flags & IGF_BASIC_BLOCK) != 0)
    {
        printf("%c block", separator);
        separator = ',';
    }

    if (flags & IGF_PLACEHOLDER)
    {
        printf("%c placeholder " FMT_BB, separator, ig->igPhData->bbNum);
        separator = ',';
    }

    if (flags & IGF_EXTEND)
    {
        printf("%c extend", separator);
        separator = ',';
    }

    if (flags & IGF_NOGCINTERRUPT)
    {
        printf("%c nogc", separator);
        separator = ',';
    }

    if (flags & IGF_LOOP_ALIGN)
    {
        printf("%c align", separator);
        separator = ',';
    }

    if (!ig->IsExtension() && !ig->IsMainProlog())
    {
        if (ig->gcLcls != VarSetOps::UninitVal())
        {
            printf("%c gc-lcls ", separator);
            dumpConvertedVarSet(compiler, ig->gcLcls);
            separator = ',';
        }

        if (ig->refRegs != RBM_NONE)
        {
            printf("%c ref-regs", separator);
            DumpRegSet(ig->refRegs);
            separator = ',';
        }

        if (ig->byrefRegs != RBM_NONE)
        {
            printf("%c byref-regs", separator);
            DumpRegSet(ig->byrefRegs);
            separator = ',';
        }
    }

    if ((ig->igFlags & IGF_PLACEHOLDER) != 0)
    {
        printf("\n");

        return;
    }

#if FEATURE_LOOP_ALIGN
    if (ig->igLoopBackEdge != nullptr)
    {
        printf("%c loop " FMT_IG, separator, ig->igLoopBackEdge->GetId());
        separator = ',';
    }
#endif

    if (compiler->verbose)
    {
        for (auto block : ig->igBlocks)
        {
            printf("%c " FMT_BB, separator, block->bbNum);
            separator = ',';
        }
    }

    printf("\n");
}

void EmitterBase::emitDispIGInstrs(insGroup* ig)
{
    if (ig->igInsCnt != 0)
    {
        printf("\n");

        uint8_t* ins = ig->igData;
        unsigned ofs = ig->igOffs;

        for (unsigned i = 0; i < ig->igInsCnt; i++)
        {
            instrDesc* id = reinterpret_cast<instrDesc*>(ins);
            JITDUMP("IN%04X: %06X ", id->idDebugOnlyInfo()->idNum, ofs);
            static_cast<ArchEmitter*>(this)->PrintIns(id);
            ins += id->GetDescSize();
            ofs += id->idCodeSize();
        }

        printf("\n");
    }
}

void EmitterBase::emitDispIGlist(bool dispInstr)
{
    for (insGroup* ig = emitIGfirst; ig != nullptr; ig = ig->igNext)
    {
        ig->Print(emitComp);

        if (dispInstr)
        {
            emitDispIGInstrs(ig);
        }
    }
}

const char* EmitterBase::emitGetFrameReg()
{
    return codeGen->isFramePointerUsed() ? STR_FPBASE : STR_SPBASE;
}

#endif // DEBUG

size_t Encoder::emitIssue1Instr(insGroup* ig, instrDesc* id, uint8_t** dp)
{
    assert(id->idInsFmt() != IF_GC_REG);

#ifdef DEBUG
    if (JitConfig.JitEmitPrintRefRegs() != 0)
    {
        printf("Before emitOutputInstr for IN%04X\n", id->idDebugOnlyInfo()->idNum);
        printf("  REF regs");
        DumpRegSet(gcInfo.GetLiveRegs(GCT_GCREF));
        printf("\n  BYREF regs");
        DumpRegSet(gcInfo.GetLiveRegs(GCT_BYREF));
        printf("\n");
    }

    if (emitComp->compDebugBreak &&
        static_cast<unsigned>(JitConfig.JitBreakEmitOutputInstr()) == id->idDebugOnlyInfo()->idNum)
    {
        assert(!"JitBreakEmitOutputInstr reached");
    }
#endif

    uint8_t* instrCodeAddr = *dp;
    size_t   instrDescSize = emitOutputInstr(ig, id, dp);

#if defined(DEBUG) || defined(LATE_DISASM)
    double insExecCost  = insEvaluateExecutionCost(id);
    double insPerfScore = (static_cast<double>(ig->igWeight) / BB_UNITY_WEIGHT) * insExecCost;
    ig->igPerfScore += insPerfScore;
#endif

    uint32_t actualSize    = static_cast<uint32_t>(*dp - instrCodeAddr);
    uint32_t estimatedSize = id->idCodeSize();

    assert((actualSize != 0) || id->InstrHasNoCode());

    noway_assert(actualSize == estimatedSize);

#ifdef DEBUG
    if (instrDescSize != id->GetDescSize())
    {
        printf("IN%04X %s: expected size %u, actual size %u\n", id->idDebugOnlyInfo()->idNum,
               EmitterBase::emitIfName(id->idInsFmt()), instrDescSize, id->GetDescSize());

        assert(instrDescSize == id->GetDescSize());
    }
#endif

    return instrDescSize;
}

// Update the offsets of all the instruction groups (note: please don't be
// lazy and call this routine frequently, it walks the list of instruction
// groups and thus it isn't cheap).
void EmitterBase::emitRecomputeIGoffsets()
{
    unsigned offs = 0;

    for (insGroup* ig = emitIGfirst; ig != nullptr; ig = ig->igNext)
    {
        ig->igOffs = offs;
        assert(IsCodeAligned(ig->igOffs));
        offs += ig->igSize;
    }

    INDEBUG(emitCheckIGoffsets());
}

#ifdef DEBUG
void AsmPrinter::PrintHandleComment(void* handle, HandleKind kind)
{
    if (handle == nullptr)
    {
        return;
    }

#ifdef TARGET_XARCH
    const char* commentPrefix = "      ;";
#else
    const char* commentPrefix = "      //";
#endif

    const char* str = nullptr;

    if (kind == HandleKind::String)
    {
        const WCHAR* wstr = compiler->eeGetCPString(handle);

        // NOTE: eGetCPString always returns nullptr on Linux/ARM
        if (wstr == nullptr)
        {
            str = "string handle";
        }
        else
        {
            const size_t actualLen = wcslen(wstr);
            const size_t maxLength = 63;
            const size_t newLen    = min(maxLength, actualLen);

            // +1 for null terminator
            WCHAR buf[maxLength + 1] = {0};
            wcsncpy(buf, wstr, newLen);
            for (size_t i = 0; i < newLen; i++)
            {
                // Escape \n and \r symbols
                if (buf[i] == L'\n' || buf[i] == L'\r')
                {
                    buf[i] = L' ';
                }
            }
            if (actualLen > maxLength)
            {
                // Append "..." for long strings
                buf[maxLength - 3] = L'.';
                buf[maxLength - 2] = L'.';
                buf[maxLength - 1] = L'.';
            }
            printf("%s \"%S\"", commentPrefix, buf);
        }
    }
    else if (kind == HandleKind::Class)
    {
        if (compiler->opts.compReloc)
        {
            // TODO-MIKE-Cleanup: Sometimes the JIT generates code that accesses runtime
            // class members, and then constant-folds the resulting address expression
            // producing a constant address that's still marked as a class handle but
            // points somewhere inside the runtime class.
            // The folding is correct but we need to change the handle kind to something
            // else so we don't try to get the name. For now just ignore class handles in
            // the JIT case (in pre-JIT we need relocs, and those prevent constant folding).
            str = compiler->eeGetClassName(static_cast<CORINFO_CLASS_HANDLE>(handle));
        }
        else
        {
            str = "class handle";
        }
    }
#ifndef TARGET_XARCH
    // These are less useful for xarch:
    else if (kind == HandleKind::Field)
    {
        str = compiler->eeGetFieldName(static_cast<CORINFO_FIELD_HANDLE>(handle));
    }
    else if (kind == HandleKind::Method)
    {
        str = compiler->eeGetMethodFullName(static_cast<CORINFO_METHOD_HANDLE>(handle));
    }
    else if (kind == HandleKind::ConstData)
    {
        str = "const ptr";
    }
    else if (kind == HandleKind::MutableData)
    {
        str = "mutable data";
    }
    else if (kind == HandleKind::Static)
    {
        str = "static address";
    }
    else if (kind == HandleKind::MethodAddr)
    {
        str = "function address";
    }
    else if (kind == HandleKind::Token)
    {
        str = "token handle";
    }
    else
    {
        str = "unknown";
    }
#endif // TARGET_XARCH

    if (str != nullptr)
    {
        printf("%s %s", commentPrefix, str);
    }
}
#endif // DEBUG

#if FEATURE_LOOP_ALIGN

// Insert an align instruction at the end of emitCurIG and mark it as
// IGF_LOOP_ALIGN to indicate that next IG is a loop needing alignment.
void EmitterBase::emitLoopAlignment()
{
    assert(emitComp->opts.alignLoops);

    // After an epilog we don't have a suitable IG to insert the align instruction.
    // We can't put it into the epilog, as it may affect the epilog size we report
    // to the VM (and we can't do it anyway because at this point the epilog is
    // only a placeholder and can't have any instructions in it). And we don't want
    // to put it into the next IG because that belongs to the loop and the NOP
    // would be executed on every iteration. So we pull an IG out of the hat.
    if (emitCurIG == nullptr)
    {
        assert(emitIGlast->IsEpilog());

        emitNewIG();
        emitCurIG->igFlags |= IGF_EXTEND;
    }

    uint16_t paddingBytes;

    if ((emitComp->opts.compJitAlignLoopBoundary > 16) && (!emitComp->opts.compJitAlignLoopAdaptive))
    {
        paddingBytes = emitComp->opts.compJitAlignLoopBoundary;
        static_cast<X86Emitter*>(this)->emitLongLoopAlign(paddingBytes);
    }
    else
    {
        paddingBytes = MAX_ENCODED_SIZE;
        static_cast<X86Emitter*>(this)->emitLoopAlign(paddingBytes);
    }

    // Mark this IG as need alignment so during EmitterBase we can check the instruction count heuristics of
    // all IGs that follows this IG and participate in a loop.
    emitCurIG->igFlags |= IGF_LOOP_ALIGN;

    JITDUMP("Adding 'align' instruction of %d bytes in " FMT_IG ".\n", paddingBytes, emitCurIG->GetId());

    INDEBUG(emitComp->loopAlignCandidates++);
}

// Checks if current IG ends with loop align instruction.
// Returns true if current IG ends with align instruction.
bool EmitterBase::emitEndsWithAlignInstr()
{
    return emitCurIG->isLoopAlign();
}

// Starting from loopHeaderIg, find the size of the smallest possible loop
// such that it doesn't exceed the maxLoopSize.
//
// igLoopHeader    - The header IG of a loop
// maxLoopSize     - Maximum loop size. If the loop is bigger than this value, we will just
//                   return this value.
// isAlignAdjusted - Determine if adjustments are done to the align instructions or not.
//                   During generating code, it is 'false' (because we haven't adjusted the size yet).
//                   During outputting code, it is 'true'.
//
// Returns the size of a loop in bytes.
//
unsigned EmitterBase::getLoopSize(insGroup* igLoopHeader, unsigned maxLoopSize DEBUG_ARG(bool isAlignAdjusted))
{
    unsigned loopSize = 0;

    for (insGroup* igInLoop = igLoopHeader; igInLoop != nullptr; igInLoop = igInLoop->igNext)
    {
        loopSize += igInLoop->igSize;
        if (igInLoop->isLoopAlign())
        {
            // If igInLoop is marked as "IGF_LOOP_ALIGN", the basic block flow detected a loop start.
            // If the loop was formed because of forward jumps like the loop IG18 below, the backedge is not
            // set for them and such loops are not aligned. For such cases, the loop size threshold will never
            // be met and we would break as soon as loopSize > maxLoopSize.
            //
            // IG05:
            //      ...
            //      jmp IG18
            // ...
            // IG18:
            //      ...
            //      jne IG05
            //
            // If igInLoop is a legitimate loop, and igInLoop's next IG is also a loop that needs alignment,
            // then igInLoop should be the last IG of the current loop and should have backedge to current
            // loop header.
            //
            // Below, IG05 is the last IG of loop IG04-IG05 and its backedge points to IG04.
            //
            // IG03:
            //      ...
            //      align
            // IG04:
            //      ...
            //      ...
            // IG05:
            //      ...
            //      jne IG04
            //      align     ; <---
            // IG06:
            //      ...
            //      jne IG06
            //
            //
            assert((igInLoop->igLoopBackEdge == nullptr) || (igInLoop->igLoopBackEdge == igLoopHeader));

#ifdef DEBUG
            if (isAlignAdjusted)
            {
                // If this IG is already align adjusted, get the adjusted padding already calculated.
                instrDescAlign* alignInstr      = emitAlignList;
                bool            foundAlignInstr = false;

                // Find the alignInstr for igInLoop IG.
                for (; alignInstr != nullptr; alignInstr = alignInstr->idaNext)
                {
                    if (alignInstr->idaIG == igInLoop)
                    {
                        foundAlignInstr = true;
                        break;
                    }
                }
                assert(foundAlignInstr);

                unsigned adjustedPadding = 0;
                if (emitComp->opts.compJitAlignLoopAdaptive)
                {
                    adjustedPadding = alignInstr->idCodeSize();
                }
                else
                {
                    instrDescAlign* alignInstrToAdj = alignInstr;
                    for (; alignInstrToAdj != nullptr && alignInstrToAdj->idaIG == alignInstr->idaIG;
                         alignInstrToAdj = alignInstrToAdj->idaNext)
                    {
                        adjustedPadding += alignInstrToAdj->idCodeSize();
                    }
                }

                loopSize -= adjustedPadding;
            }
            else
#endif
            {
                // The current loop size should exclude the align instruction size reserved for next loop.
                loopSize -= emitComp->opts.compJitAlignPaddingLimit;
            }
        }
        if ((igInLoop->igLoopBackEdge == igLoopHeader) || (loopSize > maxLoopSize))
        {
            break;
        }
    }

    return loopSize;
}

// Sets igLoopBackEdge field, if not already set and if currIG has back-edge to dstIG.
//
// Despite we align only inner most loop, we might see intersected loops because of control flow
// re-arrangement like adding a split edge in LSRA.
//
// If there is an intersection of current loop with last loop that is already marked as align,
// then *do not align* one of the loop that completely encloses the other one. Or if they both intersect,
// then *do not align* either of them because since the flow is complicated enough that aligning one of them
// will not improve the performance.
//
void EmitterBase::emitSetLoopBackEdge(insGroup* dstIG)
{
    assert(dstIG->IsDefined() && (dstIG->igNum <= emitCurIG->igNum));

    bool alignCurrentLoop = true;
    bool alignLastLoop    = true;

    unsigned currLoopStart = dstIG->igNum;
    unsigned currLoopEnd   = emitCurIG->igNum;

    // Only mark back-edge if current loop starts after the last inner loop ended.
    if (emitLastLoopEnd < currLoopStart)
    {
        emitCurIG->igLoopBackEdge = dstIG;

        JITDUMP("** " FMT_IG " jumps back to " FMT_IG " forming a loop.\n", currLoopEnd, currLoopStart);

        emitLastLoopStart = currLoopStart;
        emitLastLoopEnd   = currLoopEnd;
    }
    else if (currLoopStart == emitLastLoopStart)
    {
        // Note: If current and last loop starts at same point,
        // retain the alignment flag of the smaller loop.
        //               |
        //         .---->|<----.
        //   last  |     |     |
        //   loop  |     |     | current
        //         .---->|     | loop
        //               |     |
        //               |-----.
        //
    }
    else if ((currLoopStart < emitLastLoopStart) && (emitLastLoopEnd < currLoopEnd))
    {
        // if current loop completely encloses last loop,
        // then current loop should not be aligned.
        alignCurrentLoop = false;
    }
    else if ((emitLastLoopStart < currLoopStart) && (currLoopEnd < emitLastLoopEnd))
    {
        // if last loop completely encloses current loop,
        // then last loop should not be aligned.
        alignLastLoop = false;
    }
    else
    {
        // The loops intersect and should not align either of the loops
        alignLastLoop    = false;
        alignCurrentLoop = false;
    }

    if (!alignLastLoop || !alignCurrentLoop)
    {
        instrDescAlign* alignInstr     = emitAlignList;
        bool            markedLastLoop = alignLastLoop;
        bool            markedCurrLoop = alignCurrentLoop;
        while ((alignInstr != nullptr))
        {
            // Find the IG before current loop and clear the IGF_LOOP_ALIGN flag
            if (!alignCurrentLoop && (alignInstr->idaIG->igNext == dstIG))
            {
                assert(!markedCurrLoop);
                alignInstr->idaIG->igFlags &= ~IGF_LOOP_ALIGN;
                markedCurrLoop = true;
                JITDUMP("** Skip alignment for current loop " FMT_IG " ~ " FMT_IG
                        " because it encloses an aligned loop " FMT_IG " ~ " FMT_IG ".\n",
                        currLoopStart, currLoopEnd, emitLastLoopStart, emitLastLoopEnd);
            }

            // Find the IG before the last loop and clear the IGF_LOOP_ALIGN flag
            if (!alignLastLoop && (alignInstr->idaIG->igNext != nullptr) &&
                (alignInstr->idaIG->igNext->igNum == emitLastLoopStart))
            {
                assert(!markedLastLoop);
                assert(alignInstr->idaIG->isLoopAlign());
                alignInstr->idaIG->igFlags &= ~IGF_LOOP_ALIGN;
                markedLastLoop = true;
                JITDUMP("** Skip alignment for aligned loop " FMT_IG " ~ " FMT_IG
                        " because it encloses the current loop " FMT_IG " ~ " FMT_IG ".\n ",
                        emitLastLoopStart, emitLastLoopEnd, currLoopStart, currLoopEnd);
            }

            if (markedLastLoop && markedCurrLoop)
            {
                break;
            }

            alignInstr = alignInstr->idaNext;
        }

        assert(markedLastLoop && markedCurrLoop);
    }
}

// Walk all the align instructions and update them with actual padding needed.
//
// For IGs that have align instructions in the end, calculate the actual offset
// of loop start and determine how much padding is needed. Based on that, update
// the igOffs, igSize and emitTotalCodeSize.
//
void EmitterBase::emitLoopAlignAdjustments()
{
    // no align instructions
    if (emitAlignList == nullptr)
    {
        return;
    }

    JITDUMP("*************** In emitLoopAlignAdjustments()\n");
    JITDUMP("compJitAlignLoopAdaptive       = %s\n", dspBool(emitComp->opts.compJitAlignLoopAdaptive));
    JITDUMP("compJitAlignLoopBoundary       = %u\n", emitComp->opts.compJitAlignLoopBoundary);
    JITDUMP("compJitAlignLoopMinBlockWeight = %u\n", emitComp->opts.compJitAlignLoopMinBlockWeight);
    JITDUMP("compJitAlignLoopForJcc         = %s\n", dspBool(emitComp->opts.compJitAlignLoopForJcc));
    JITDUMP("compJitAlignLoopMaxCodeSize    = %u\n", emitComp->opts.compJitAlignLoopMaxCodeSize);
    JITDUMP("compJitAlignPaddingLimit       = %u\n", emitComp->opts.compJitAlignPaddingLimit);

    unsigned estimatedPaddingNeeded = emitComp->opts.compJitAlignPaddingLimit;

    unsigned        alignBytesRemoved = 0;
    unsigned        loopIGOffset      = 0;
    instrDescAlign* alignInstr        = emitAlignList;

    for (; alignInstr != nullptr; alignInstr = alignInstr->idaNext)
    {
        assert(alignInstr->idIns() == INS_align);

        insGroup* alignIG = alignInstr->idaIG;

        loopIGOffset = alignIG->igOffs + alignIG->igSize;

        // igSize also includes INS_align instruction, take it off.
        loopIGOffset -= estimatedPaddingNeeded;

        // IG can be marked as not needing alignment if during setting igLoopBackEdge, it is detected
        // that the igLoopBackEdge encloses an IG that is marked for alignment.
        unsigned actualPaddingNeeded =
            alignIG->isLoopAlign() ? emitCalculatePaddingForLoopAlignment(alignIG, loopIGOffset DEBUG_ARG(false)) : 0;

        assert(estimatedPaddingNeeded >= actualPaddingNeeded);

        unsigned short diff = (unsigned short)(estimatedPaddingNeeded - actualPaddingNeeded);

        if (diff != 0)
        {
            alignIG->igSize -= diff;
            alignBytesRemoved += diff;

            // Update the flags
            alignIG->igFlags |= IGF_UPD_ISZ;
            if (actualPaddingNeeded == 0)
            {
                alignIG->igFlags &= ~IGF_LOOP_ALIGN;
            }

            if (emitComp->opts.compJitAlignLoopAdaptive)
            {
                assert(actualPaddingNeeded < MAX_ENCODED_SIZE);
                alignInstr->idCodeSize(actualPaddingNeeded);
            }
            else
            {
                unsigned paddingToAdj = actualPaddingNeeded;

#ifdef DEBUG
                int instrAdjusted =
                    (emitComp->opts.compJitAlignLoopBoundary + (MAX_ENCODED_SIZE - 1)) / MAX_ENCODED_SIZE;
#endif
                // Adjust the padding amount in all align instructions in this IG
                instrDescAlign *alignInstrToAdj = alignInstr, *prevAlignInstr = nullptr;
                for (; alignInstrToAdj != nullptr && alignInstrToAdj->idaIG == alignInstr->idaIG;
                     alignInstrToAdj = alignInstrToAdj->idaNext)
                {
                    unsigned newPadding = min(paddingToAdj, MAX_ENCODED_SIZE);
                    alignInstrToAdj->idCodeSize(newPadding);
                    paddingToAdj -= newPadding;
                    prevAlignInstr = alignInstrToAdj;
#ifdef DEBUG
                    instrAdjusted--;
#endif
                }
                assert(paddingToAdj == 0);
                assert(instrAdjusted == 0);

                // fast forward the align instruction to next IG
                alignInstr = prevAlignInstr;
            }

            JITDUMP("Adjusted alignment of " FMT_IG " from %u to %u.\n", alignIG->GetId(), estimatedPaddingNeeded,
                    actualPaddingNeeded);
            JITDUMP("Adjusted size of " FMT_IG " from %u to %u.\n", alignIG->GetId(), (alignIG->igSize + diff),
                    alignIG->igSize);
        }

        // Adjust the offset of all IGs starting from next IG until we reach the IG having the next
        // align instruction or the end of IG list.
        insGroup* adjOffIG     = alignIG->igNext;
        insGroup* adjOffUptoIG = alignInstr->idaNext != nullptr ? alignInstr->idaNext->idaIG : emitIGlast;
        while ((adjOffIG != nullptr) && (adjOffIG->igNum <= adjOffUptoIG->igNum))
        {
            JITDUMP("Adjusted offset of " FMT_IG " from %04X to %04X\n", adjOffIG->GetId(), adjOffIG->igOffs,
                    (adjOffIG->igOffs - alignBytesRemoved));
            adjOffIG->igOffs -= alignBytesRemoved;
            adjOffIG = adjOffIG->igNext;
        }
    }

    INDEBUG(emitCheckIGoffsets());
}

//-----------------------------------------------------------------------------
//  emitCalculatePaddingForLoopAlignment: Calculate the padding to insert at the
//    end of 'ig' so the loop that starts after 'ig' is aligned.
//
//  Arguments:
//       ig              - The IG having 'align' instruction in the end.
//       offset          - The offset at which the IG that follows 'ig' starts.
//       isAlignAdjusted - Determine if adjustments are done to the align instructions or not.
//                         During generating code, it is 'false' (because we haven't adjusted the size yet).
//                         During outputting code, it is 'true'.
//
//  Returns: Padding amount.
//    0 means no padding is needed, either because loop is already aligned or it
//    is too expensive to align loop and hence it will not be aligned.
//
//  Notes:
//     Below are the steps (in this order) to calculate the padding amount.
//     1. If loop is already aligned to desired boundary, then return 0. // already aligned
//     2. If loop size exceed maximum allowed loop size, then return 0.  // already aligned
//
// For adaptive loop alignment:
//     3a. Calculate paddingNeeded and maxPaddingAmount to align to 32B boundary.
//     3b. If paddingNeeded > maxPaddingAmount, then recalculate to align to 16B boundary.
//     3b. If paddingNeeded == 0, then return 0. // already aligned at 16B
//     3c. If paddingNeeded > maxPaddingAmount, then return 0. // expensive to align
//     3d. If the loop already fits in minimum 32B blocks, then return 0. // already best aligned
//     3e. return paddingNeeded.
//
// For non-adaptive loop alignment:
//     3a. Calculate paddingNeeded.
//     3b. If the loop already fits in minimum alignmentBoundary blocks, then return 0. // already best aligned
//     3c. return paddingNeeded.
//
unsigned EmitterBase::emitCalculatePaddingForLoopAlignment(insGroup* ig, size_t offset DEBUG_ARG(bool isAlignAdjusted))
{
    assert(ig->isLoopAlign());
    unsigned alignmentBoundary = emitComp->opts.compJitAlignLoopBoundary;

    // No padding if loop is already aligned
    if ((offset & (alignmentBoundary - 1)) == 0)
    {
        JITDUMP(";; Skip alignment: 'Loop at " FMT_IG " already aligned at %dB boundary.'\n", ig->igNext->GetId(),
                alignmentBoundary);
        return 0;
    }

    unsigned maxLoopSize          = 0;
    int      maxLoopBlocksAllowed = 0;

    if (emitComp->opts.compJitAlignLoopAdaptive)
    {
        // For adaptive, adjust the loop size depending on the alignment boundary
        maxLoopBlocksAllowed = genLog2((unsigned)alignmentBoundary) - 1;
        maxLoopSize          = alignmentBoundary * maxLoopBlocksAllowed;
    }
    else
    {
        // For non-adaptive, just take whatever is supplied using COMPlus_ variables
        maxLoopSize = emitComp->opts.compJitAlignLoopMaxCodeSize;
    }

    unsigned loopSize = getLoopSize(ig->igNext, maxLoopSize DEBUG_ARG(isAlignAdjusted));

    // No padding if loop is big
    if (loopSize > maxLoopSize)
    {
        JITDUMP(";; Skip alignment: 'Loop at " FMT_IG " is big. LoopSize= %d, MaxLoopSize= %d.'\n", ig->igNext->GetId(),
                loopSize, maxLoopSize);
        return 0;
    }

    unsigned paddingToAdd           = 0;
    unsigned minBlocksNeededForLoop = (loopSize + alignmentBoundary - 1) / alignmentBoundary;
    bool     skipPadding            = false;

    if (emitComp->opts.compJitAlignLoopAdaptive)
    {
        // adaptive loop alignment
        unsigned nMaxPaddingBytes = (1 << (maxLoopBlocksAllowed - minBlocksNeededForLoop + 1)) - 1;
        unsigned nPaddingBytes    = (-(int)(size_t)offset) & (alignmentBoundary - 1);

        // Check if the alignment exceeds maxPadding limit
        if (nPaddingBytes > nMaxPaddingBytes)
        {
            // Cannot align to 32B, so try to align to 16B boundary.
            alignmentBoundary >>= 1;
            nMaxPaddingBytes = 1 << (maxLoopBlocksAllowed - minBlocksNeededForLoop + 1);
            nPaddingBytes    = (-(int)(size_t)offset) & (alignmentBoundary - 1);

            // Check if the loop is already at new alignment boundary
            if (nPaddingBytes == 0)
            {
                skipPadding = true;
                JITDUMP(";; Skip alignment: 'Loop at " FMT_IG " already aligned at %uB boundary.'\n",
                        ig->igNext->GetId(), alignmentBoundary);
            }
            // Check if the alignment exceeds new maxPadding limit
            else if (nPaddingBytes > nMaxPaddingBytes)
            {
                skipPadding = true;
                JITDUMP(";; Skip alignment: 'Loop at " FMT_IG " PaddingNeeded= %d, MaxPadding= %d, LoopSize= %d, "
                        "AlignmentBoundary= %dB.'\n",
                        ig->igNext->GetId(), nPaddingBytes, nMaxPaddingBytes, loopSize, alignmentBoundary);
            }
        }

        // If within maxPaddingLimit
        if (!skipPadding)
        {
            // Padding is needed only if loop starts at or after the current offset.
            // Otherwise, the loop just fits in minBlocksNeededForLoop and so can skip alignment.
            size_t extraBytesNotInLoop =
                (size_t)(emitComp->opts.compJitAlignLoopBoundary * minBlocksNeededForLoop) - loopSize;
            size_t currentOffset = (size_t)offset % alignmentBoundary;

            if (currentOffset > extraBytesNotInLoop)
            {
                // Padding is needed only if loop starts at or after the current offset and hence might not
                // fit in minBlocksNeededForLoop
                paddingToAdd = nPaddingBytes;
            }
            else
            {
                // Otherwise, the loop just fits in minBlocksNeededForLoop and so can skip alignment.
                JITDUMP(";; Skip alignment: 'Loop at " FMT_IG " is aligned to fit in %d blocks of %d chunks.'\n",
                        ig->igNext->GetId(), minBlocksNeededForLoop, alignmentBoundary);
            }
        }
    }
    else
    {
        // non-adaptive loop alignment
        unsigned extraBytesNotInLoop = (alignmentBoundary * minBlocksNeededForLoop) - loopSize;
        unsigned currentOffset       = (size_t)offset % alignmentBoundary;

#ifdef DEBUG
        // Mitigate JCC erratum by making sure the jmp doesn't fall on the boundary
        if (emitComp->opts.compJitAlignLoopForJcc)
        {
            // TODO: See if extra padding we might end up adding to mitigate JCC erratum is worth doing?
            currentOffset++;
        }
#endif

        if (currentOffset > extraBytesNotInLoop)
        {
            // Padding is needed only if loop starts at or after the current offset and hence might not
            // fit in minBlocksNeededForLoop
            paddingToAdd = (-(int)(size_t)offset) & (alignmentBoundary - 1);
        }
        else
        {
            // Otherwise, the loop just fits in minBlocksNeededForLoop and so can skip alignment.
            JITDUMP(";; Skip alignment: 'Loop at " FMT_IG " is aligned to fit in %d blocks of %d chunks.'\n",
                    ig->igNext->GetId(), minBlocksNeededForLoop, alignmentBoundary);
        }
    }

    JITDUMP(";; Calculated padding to add %d bytes to align " FMT_IG " at %dB boundary.\n", paddingToAdd,
            ig->igNext->GetId(), alignmentBoundary);

    // Either no padding is added because it is too expensive or the offset gets aligned
    // to the alignment boundary
    assert(paddingToAdd == 0 || (((offset + paddingToAdd) & (alignmentBoundary - 1)) == 0));

    return paddingToAdd;
}

#endif // FEATURE_LOOP_ALIGN

#ifdef DEBUG
void EmitterBase::VerifyCallFinally(insGroup* tgtIG) const
{
    assert(tgtIG != nullptr);

#ifdef FEATURE_EH_FUNCLETS
    // We don't record enough information to determine this accurately, so instead
    // we assume that any branch to the very start of a finally is OK.

    // No branches back to the root method
    assert(tgtIG->GetFuncletIndex() > 0);
    const FuncInfoDsc& tgtFunc = codeGen->funGetFunc(tgtIG->GetFuncletIndex());
    assert(tgtFunc.kind == FUNC_HANDLER);
    EHblkDsc* tgtEH = emitComp->ehGetDsc(tgtFunc.ehIndex);

    // Only branches to finallys (not faults, catches, filters, etc.)
    assert(tgtEH->HasFinallyHandler());

    // Only to the first block of the finally (which is properly marked)
    BasicBlock* tgtBlk = tgtEH->ebdHndBeg;
    assert(tgtBlk->bbFlags & BBF_FUNCLET_BEG);

    // And now we made it back to where we started
    assert(tgtIG == tgtBlk->emitLabel);
    assert(tgtIG->GetFuncletIndex() == codeGen->funGetFuncIdx(tgtBlk));
#endif // FEATURE_EH_FUNCLETS
}

void EmitterBase::VerifyCatchRet(insGroup* tgtIG) const
{
    assert(tgtIG != nullptr);

#ifdef FEATURE_EH_FUNCLETS
    // Again there isn't enough information to prove this correct
    // so just allow a 'branch' to any other 'parent' funclet

    const FuncInfoDsc& jmpFunc = codeGen->funGetFunc(emitCurIG->GetFuncletIndex());
    assert(jmpFunc.kind == FUNC_HANDLER);
    EHblkDsc* jmpEH = emitComp->ehGetDsc(jmpFunc.ehIndex);

    // Only branches out of catches
    assert(jmpEH->HasCatchHandler());

    const FuncInfoDsc& tgtFunc = codeGen->funGetFunc(tgtIG->GetFuncletIndex());
    if (tgtFunc.kind == FUNC_HANDLER)
    {
        // An outward chain to the containing funclet/EH handler
        // Note that it might be anywhere within nested try bodies
        assert(jmpEH->ebdEnclosingHndIndex == tgtFunc.ehIndex);
    }
    else
    {
        // This funclet is 'top level' and so it is branching back to the
        // root function, and should have no containing EH handlers
        // but it could be nested within try bodies...
        assert(tgtFunc.kind == FUNC_ROOT);
        assert(jmpEH->ebdEnclosingHndIndex == EHblkDsc::NO_ENCLOSING_INDEX);
    }
#endif // FEATURE_EH_FUNCLETS
}
#endif // DEBUG

void Encoder::emitEndCodeGen(ArchEmitter& emit)
{
    CorJitAllocMemFlag allocMemFlag = CORJIT_ALLOCMEM_DEFAULT_CODE_ALIGN;

#ifdef TARGET_X86
    // These are the heuristics we use to decide whether or not to force the
    // code to be 16-byte aligned.
    //
    // 1. For ngen code with IBC data, use 16-byte alignment if the method
    //    has been called more than ScenarioHotWeight times.
    // 2. For JITed code and ngen code without IBC data, use 16-byte alignment
    //    when the code is 16 bytes or smaller. We align small getters/setters
    //    because of they are penalized heavily on certain hardware when not 16-byte
    //    aligned (VSWhidbey #373938). To minimize size impact of this optimization,
    //    we do not align large methods because of the penalty is amortized for them.

    if (emitComp->fgHaveProfileData())
    {
        const float scenarioHotWeight = 256.0f;
        if (emitComp->fgCalledCount > (scenarioHotWeight * emitComp->fgProfileRunsCount()))
        {
            allocMemFlag = CORJIT_ALLOCMEM_FLG_16BYTE_ALIGN;
        }
    }
    else if (hotCodeSize <= 16)
    {
        allocMemFlag = CORJIT_ALLOCMEM_FLG_16BYTE_ALIGN;
    }
#endif

#ifdef TARGET_XARCH
    // For x64/x86, align methods that are "optimizations enabled" to 32 byte
    // boundaries if they are larger than 16 bytes and contain a loop.
    if (emitComp->opts.OptimizationEnabled() && !emitComp->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT) &&
        (hotCodeSize > 16) && emitComp->fgHasLoops)
    {
        allocMemFlag = CORJIT_ALLOCMEM_FLG_32BYTE_ALIGN;
    }
#endif

    // This restricts the .rodata alignment to: 1, 2, 4, 8, 16, or 32 bytes
    // Alignments greater than 32 would require VM support in ICorJitInfo::allocMem
    assert(isPow2(roData.align) && (roData.align <= 32));

    if (roData.align == 16)
    {
        allocMemFlag = static_cast<CorJitAllocMemFlag>(allocMemFlag | CORJIT_ALLOCMEM_FLG_RODATA_16BYTE_ALIGN);
    }
    else if (roData.align == 32)
    {
        allocMemFlag = static_cast<CorJitAllocMemFlag>(allocMemFlag | CORJIT_ALLOCMEM_FLG_RODATA_32BYTE_ALIGN);
    }

    AllocMemArgs args{};

#ifdef TARGET_ARM64
    // For arm64, we want to allocate JIT data always adjacent to code similar to what native compiler does.
    // This way allows us to use a single `ldr` to access such data like float constant/jmp table.

    if (firstColdIG != nullptr)
    {
        // JIT data might be far away from the cold code.
        NYI_ARM64("Need to handle fix-up to data from cold code.");
    }

    uint32_t roDataAlignmentDelta = 0;

    if (roData.size && (roData.align == TARGET_POINTER_SIZE))
    {
        uint32_t roDataAlignment = TARGET_POINTER_SIZE; // 8 Byte align by default.
        roDataAlignmentDelta     = ALIGN_UP(hotCodeSize, roDataAlignment) - hotCodeSize;
        assert((roDataAlignmentDelta == 0) || (roDataAlignmentDelta == 4));
    }

    args.hotCodeSize = hotCodeSize + roDataAlignmentDelta + roData.size;
#else
    args.hotCodeSize       = hotCodeSize;
    args.roDataSize        = roData.size;
#endif
    args.coldCodeSize = GetColdCodeSize();
    args.xcptnsCount  = emitComp->compHndBBtabCount;
    args.flag         = allocMemFlag;

    jitInfo->allocMem(&args);

    hotCodeBlock             = static_cast<uint8_t*>(args.hotCodeBlock);
    uint8_t* hotCodeBlockRW  = static_cast<uint8_t*>(args.hotCodeBlockRW);
    coldCodeBlock            = static_cast<uint8_t*>(args.coldCodeBlock);
    uint8_t* coldCodeBlockRW = static_cast<uint8_t*>(args.coldCodeBlockRW);
#ifdef TARGET_ARM64
    roDataBlock            = hotCodeBlock + hotCodeSize + roDataAlignmentDelta;
    uint8_t* roDataBlockRW = hotCodeBlockRW + hotCodeSize + roDataAlignmentDelta;
#else
    roDataBlock            = static_cast<uint8_t*>(args.roDataBlock);
    uint8_t* roDataBlockRW = static_cast<uint8_t*>(args.roDataBlockRW);
#endif

    assert(((allocMemFlag & CORJIT_ALLOCMEM_FLG_32BYTE_ALIGN) == 0) ||
           ((reinterpret_cast<size_t>(hotCodeBlock) & 31) == 0));

    codeGen->hotCodeBlock  = hotCodeBlock;
    codeGen->coldCodeBlock = coldCodeBlock;

#ifdef DEBUG
    double   totalPerfScore = 0.0;
    double   blockPerfScore = 0.0;
    unsigned instrCount     = 0;
#endif

    // Make sure that the x86 alignment and cache prefetch optimization rules were
    // obeyed - don't start a method in the last 7 bytes of a 16-byte alignment area
    // unless we are generating SMALL_CODE.
    // TODO-MIKE-Review: What's up with this?
    // noway_assert((reinterpret_cast<size_t>(hotCodeBlock) % 16 <= 8) || (emitComp->compCodeOpt() == SMALL_CODE));

    uint8_t* code   = hotCodeBlock;
    writeableOffset = hotCodeBlockRW - hotCodeBlock;

    for (insGroup *ig = firstIG, *prevIG = nullptr; ig != nullptr; prevIG = ig, ig = ig->igNext)
    {
        assert((ig->igFlags & IGF_PLACEHOLDER) == 0);

        if (ig == firstColdIG)
        {
            assert(emitCurCodeOffs(code) == hotCodeSize);
            assert(coldCodeBlock != nullptr);

            code            = coldCodeBlock;
            writeableOffset = coldCodeBlockRW - coldCodeBlock;
        }

#ifdef DEBUG
        if (emitComp->verbose || emitComp->opts.disAsm)
        {
            if (ig == firstColdIG)
            {
                printf("\n************** Beginning of cold code **************\n");
            }

            if (emitComp->verbose || emitComp->opts.disasmWithGC)
            {
                printf("\n");
                ig->Print(emitComp);
            }
            else if (!ig->IsExtension() || ig->IsPrologOrEpilog() || (ig->IsNoGC() != prevIG->IsNoGC()))
            {
                printf("\n%s:", emitLabelString(emitComp, ig));

                if (!emitComp->opts.disDiffable)
                {
                    printf("              ;; offset=%04XH", emitCurCodeOffs(code));
                }

                printf("\n");
            }
        }
#endif // DEBUG

        const uint32_t codeOffs = ig->igOffs;
        noway_assert(codeOffs == emitCurCodeOffs(code));
        assert(EmitterBase::IsCodeAligned(codeOffs));

#if !FEATURE_FIXED_OUT_ARGS
        if (ig->igStkLvl != stackLevel)
        {
            // We are pushing stuff implicitly at this label.
            assert(ig->igStkLvl > stackLevel);
            emitStackPushN(codeOffs, (ig->igStkLvl - stackLevel) / REGSIZE_BYTES);
        }
#endif

        if (!ig->IsExtension() && (ig != firstIG))
        {
            gcInfo.SetLiveLclStackSlots(ig->GetGCLcls(), codeOffs);

            if (ig->IsBasicBlock() && codeGen->spillTemps.TrackGCSpillTemps())
            {
                // This is an approximation, all spill temps are definitely dead at the start of a block
                // but we don't always create an insGroup when we fall through from one block to the next.
                // Still, it's better than just keeping spill temps alive until the end of the method and
                // is consistent with the way GC locals are handled.
                gcInfo.KillTrackedSpillTemps(codeOffs);
            }

            regMaskTP refRegs = ig->GetRefRegs();

            if (gcInfo.GetLiveRegs(GCT_GCREF) != refRegs)
            {
                gcInfo.SetLiveRegs(GCT_GCREF, refRegs, codeOffs);
            }

            regMaskTP byrefRegs = ig->GetByrefRegs();

            if (gcInfo.GetLiveRegs(GCT_BYREF) != byrefRegs)
            {
                gcInfo.SetLiveRegs(GCT_BYREF, byrefRegs, codeOffs);
            }

#ifdef DEBUG
            if (emitComp->verbose || emitComp->opts.disasmWithGC)
            {
                char header[128];
                GetGCDeltaDumpHeader(header, _countof(header));
                gcInfo.DumpDelta(header);
            }
#endif
        }

        currentIG     = ig;
        instrDesc* id = reinterpret_cast<instrDesc*>(ig->igData);
        INDEBUG(const uint8_t* const igCode = code);

        for (unsigned i = 0, count = ig->igInsCnt; i < count; i++)
        {
#ifdef DEBUG
            const uint8_t*   curInstrAddr = code;
            const instrDesc* curInstrDesc = id;
#endif
            size_t instrDescSize;

            if (id->idInsFmt() == IF_GC_REG)
            {
                emitGCregLiveUpd(id->idGCref(), id->idReg1(), code);
                assert(id->idIsSmallDsc());
                instrDescSize = sizeof(instrDescSmall);
            }
            else
            {
                instrDescSize = emitIssue1Instr(ig, id, &code);
            }

            id = reinterpret_cast<instrDesc*>(reinterpret_cast<uint8_t*>(id) + instrDescSize);

#ifdef DEBUG
            if (emitComp->verbose || emitComp->opts.disasmWithGC)
            {
                char header[128];
                GetGCDeltaDumpHeader(header, _countof(header));
                gcInfo.DumpDelta(header);
            }

            if ((emitComp->verbose || emitComp->opts.disAsm) && (emitComp->opts.disAddr || emitComp->opts.disAlignment))
            {
                PrintAlignmentBoundary(reinterpret_cast<size_t>(curInstrAddr), reinterpret_cast<size_t>(code),
                                       curInstrDesc, i + 1 < count ? id : nullptr);
            }
#endif // DEBUG

#ifdef TARGET_AMD64
            // We can't have a call at the end of the try region, the unwinder needs
            // an extra instruction to understand that the call is inside the region.
            // TODO-MIKE-Fix: This has a problem with a prolog profiler helper call on linux-x64.
            assert((curInstrDesc->idIns() != INS_call) || (i < count - 1) || (ig->tryIndex == ig->igNext->tryIndex));
#endif
        }

        currentIG = nullptr;
        assert(ig->igSize == code - igCode);

#ifdef DEBUG
        instrCount += ig->igInsCnt;
        totalPerfScore += ig->igPerfScore;
        blockPerfScore += ig->igPerfScore;

        if (emitComp->verbose || (emitComp->opts.disAsm && ((ig->igNext == nullptr) || !ig->igNext->IsExtension() ||
                                                            ig->IsProlog() || ig->igNext->IsEpilog())))
        {
            printf("\t\t\t\t\t\t;; bbWeight=%s PerfScore %.2f", refCntWtd2str(ig->igWeight), blockPerfScore);
            blockPerfScore = 0.0;
        }
#endif
    }

    assert(totalCodeSize == emitCurCodeOffs(code));
#if !FEATURE_FIXED_OUT_ARGS
    assert(stackLevel == 0);
#endif

    gcInfo.End(emitCurCodeOffs(code));

    if (roData.size != 0)
    {
        OutputRoData(roDataBlockRW);
    }

#ifdef DEBUG
    if (emitComp->opts.disAsm)
    {
        printf("\n");
    }

    // Add code size information into the Perf Score
    totalPerfScore += static_cast<double>(hotCodeSize) * PERFSCORE_CODESIZE_COST_HOT;
    totalPerfScore += static_cast<double>(GetColdCodeSize()) * PERFSCORE_CODESIZE_COST_COLD;

    emit.perfScore  = totalPerfScore;
    emit.instrCount = instrCount;
#endif
}

#ifdef LATE_DISASM
void EmitterBase::disSetMethod(size_t addr, CORINFO_METHOD_HANDLE methHnd)
{
    disasm->disSetMethod(addr, methHnd);
}

void EmitterBase::Disassemble()
{
    disasm->disAsmCode(GetHotCodeAddr(), GetHotCodeSize(), GetColdCodeAddr(), GetColdCodeSize());
}
#endif

unsigned EmitterBase::emitFindInsNum(const insGroup* ig, const instrDesc* instr)
{
    uint8_t* insData = ig->igData;

    for (unsigned i = 0, count = ig->igInsCnt; i < count; i++)
    {
        instrDesc* id = reinterpret_cast<instrDesc*>(insData);

        if (id == instr)
        {
            return i;
        }

        insData += id->GetDescSize();
    }

    unreached();
}

// We've been asked for the code offset of an instruction but alas one or
// more instruction sizes in the block have been mis-predicted, so we have
// to find the true offset by looking for the instruction within the group.
uint32_t insGroup::FindInsOffset(unsigned insNum) const
{
    assert(insNum <= igInsCnt);

    uint8_t* insData = igData;
    uint32_t insOffs = 0;

    for (unsigned i = 0; i < insNum; i++)
    {
        EmitterBase::instrDesc* id = reinterpret_cast<EmitterBase::instrDesc*>(insData);
        insOffs += id->idCodeSize();
        insData += id->GetDescSize();
    }

    return insOffs;
}

uint32_t insGroup::GetCodeOffset(CodePos codePos) const
{
    uint32_t insOffs;
    unsigned insNum = GetInsNumFromCodePos(codePos);

    if (insNum == 0)
    {
        insOffs = 0;
    }
    else if (insNum == igInsCnt)
    {
        insOffs = igSize;
    }
    else if ((igFlags & IGF_UPD_ISZ) != 0)
    {
        insOffs = FindInsOffset(insNum);
    }
    else
    {
        insOffs = GetInsOffsetFromCodePos(codePos);
        assert(insOffs == FindInsOffset(GetInsNumFromCodePos(codePos)));
    }

    return igOffs + insOffs;
}

unsigned Encoder::emitCurCodeOffs(const uint8_t* dst) const
{
    size_t distance;

    if ((dst >= hotCodeBlock) && (dst <= hotCodeBlock + hotCodeSize))
    {
        distance = dst - hotCodeBlock;
    }
    else
    {
        assert(firstColdIG != nullptr);
        assert(coldCodeBlock != nullptr);
        assert((dst >= coldCodeBlock) && (dst <= coldCodeBlock + GetColdCodeSize()));

        distance = dst - coldCodeBlock + hotCodeSize;
    }

    noway_assert(distance <= UINT_MAX);

    return static_cast<unsigned>(distance);
}

uint8_t* Encoder::emitOffsetToPtr(unsigned offset) const
{
    if (offset < hotCodeSize)
    {
        return hotCodeBlock + offset;
    }
    else
    {
        assert(offset < totalCodeSize);

        return coldCodeBlock + (offset - hotCodeSize);
    }
}

uint8_t* Encoder::emitDataOffsetToPtr(unsigned offset) const
{
    assert(offset < roData.size);
    return roDataBlock + offset;
}

#ifdef DEBUG

bool Encoder::emitJumpCrossHotColdBoundary(size_t srcOffset, size_t dstOffset) const
{
    assert(srcOffset < totalCodeSize);
    assert(dstOffset < totalCodeSize);

    return (srcOffset < hotCodeSize) != (dstOffset < hotCodeSize);
}
#endif // DEBUG

ConstData* EmitterBase::CreateBlockLabelTable(BasicBlock** blocks, unsigned count, bool relative)
{
    DataSection* section = CreateLabelTable(count, relative);
    insGroup**   labels  = reinterpret_cast<insGroup**>(section->data.GetData());

    for (unsigned i = 0; i < count; i++)
    {
        BasicBlock* target = blocks[i];
        noway_assert(target->emitLabel != nullptr);
        labels[i] = target->emitLabel;
    }

    return &section->data;
}

ConstData* EmitterBase::CreateTempLabelTable(insGroup*** labels, unsigned count, bool relative)
{
    DataSection* section = CreateLabelTable(count, true);
    *labels              = reinterpret_cast<insGroup**>(section->data.GetData());

    for (unsigned i = 0; i < count; i++)
    {
        (*labels)[i] = CreateTempLabel();
    }

    return &section->data;
}

EmitterBase::DataSection* EmitterBase::CreateLabelTable(unsigned count, bool relative)
{
    DataSection* section = static_cast<DataSection*>(emitGetMem(sizeof(DataSection) + count * sizeof(insGroup*)));

    section->next        = nullptr;
    section->data.offset = roData.size;
    section->data.size   = count * (relative ? 4 : TARGET_POINTER_SIZE);
    section->data.kind   = relative ? ConstData::LabelRel32 : ConstData::LabelAddr;
    INDEBUG(section->data.type = TYP_UNDEF);

    if (roData.last != nullptr)
    {
        roData.last->next = section;
    }
    else
    {
        roData.first = section;
    }

    roData.last = section;
    roData.size += section->data.size;

    return section;
}

ConstData* EmitterBase::GetFloatConst(double value, var_types type)
{
    const void* data;
    uint32_t    size;
    float       f;

    if (type == TYP_FLOAT)
    {
        f    = static_cast<float>(value);
        data = &f;
        size = 4;
    }
    else
    {
        assert(type == TYP_DOUBLE);

        data = &value;
        size = 8;
    }

    uint32_t align = size;

#ifdef TARGET_XARCH
    if (emitComp->compCodeOpt() == SMALL_CODE)
    {
        // TODO-MIKE-Review: Is this .rodata size optimization worth it?
        // The cost of accessing misaligned data that straddles cache lines is pretty high.
        align = ConstData::MinAlign;
    }
#endif

    return GetConst(data, size, align DEBUGARG(type));
}

ConstData* EmitterBase::GetConst(const void* data, unsigned size, unsigned align DEBUGARG(var_types type))
{
    if (DataSection* section = roData.Find(data, size, align))
    {
#ifdef DEBUG
        if ((section->data.type != type) && (section->data.size == size) && varTypeIsFloating(type))
        {
            section->data.type = type;
        }
#endif

        return &section->data;
    }

    return &CreateConst(data, size, align DEBUGARG(type))->data;
}

EmitterBase::DataSection* EmitterBase::CreateConst(const void* data,
                                                   uint32_t    size,
                                                   uint32_t align DEBUGARG(var_types type))
{
    return CreateConstSection(data, size, align DEBUGARG(type));
}

EmitterBase::DataSection* EmitterBase::CreateConstSection(const void* data,
                                                          uint32_t    size,
                                                          uint32_t align DEBUGARG(var_types type))
{
    assert((size != 0) && (size % ConstData::MinAlign == 0));
    assert(isPow2(align) && (align <= ConstData::MaxAlign));

    uint32_t offset = roData.size;

    if ((offset % align != 0) && (align > ConstData::MinAlign))
    {
        uint8_t zeros[ConstData::MaxAlign]{};
        CreateConst(&zeros, align - offset % align, ConstData::MinAlign DEBUGARG(TYP_INT));
        offset = roData.size;
    }

    assert(offset % align == 0);

    DataSection* section = static_cast<DataSection*>(emitGetMem(sizeof(DataSection) + size));

    section->next        = nullptr;
    section->data.offset = offset;
    section->data.size   = size;
    section->data.kind   = ConstData::Const;
    INDEBUG(section->data.type = type);
    memcpy(section->data.GetData(), data, size);

    if (roData.last != nullptr)
    {
        roData.last->next = section;
    }
    else
    {
        roData.first = section;
    }

    roData.last = section;
    roData.size += size;
    roData.align = Max(roData.align, align);

    return section;
}

EmitterBase::DataSection* EmitterBase::RoData::Find(const void* data, uint32_t size, uint32_t align) const
{
    // We're doing a linear search, limit it to avoid poor throughput.
    const unsigned MaxCount = 64;
    unsigned       index    = 0;

    assert(isPow2(align));

    for (DataSection *section = first; (section != nullptr) && (index < MaxCount); section = section->next, index++)
    {
        if ((section->data.kind == ConstData::Const) && (section->data.size >= size) &&
            ((section->data.offset & (align - 1)) == 0))
        {
            if (memcmp(data, section->data.GetData(), size) == 0)
            {
                return section;
            }
        }
    }

    return nullptr;
}

void Encoder::OutputRoData(uint8_t* dst)
{
    JITDUMP("\nEmitting data sections: %u total bytes\n", roData.size);

#ifdef DEBUG
    if (emitComp->opts.disAsm)
    {
        PrintRoData();
    }
#endif

    for (DataSection* section = roData.first; section != nullptr; section = section->next)
    {
        const ConstData& data = section->data;
        JITDUMP("RWD%02u: ; size %u", data.offset, data.size);

        if (data.kind == ConstData::LabelAddr)
        {
            JITDUMP(", label addr\n");
            assert((data.size != 0) && (data.size % TARGET_POINTER_SIZE == 0));

            target_size_t* dstRW  = reinterpret_cast<target_size_t*>(dst + data.offset);
            insGroup**     labels = reinterpret_cast<insGroup**>(data.GetData());

            for (unsigned i = 0, count = data.size / TARGET_POINTER_SIZE; i < count; i++)
            {
                insGroup* label = labels[i];
                size_t    addr  = reinterpret_cast<size_t>(emitOffsetToPtr(label->igOffs));
                ARM_ONLY(addr |= 1); // Set the Thumb bit.
                dstRW[i] = static_cast<target_size_t>(addr);

                if (emitComp->opts.compReloc)
                {
                    emitRecordRelocation(&dstRW[i], reinterpret_cast<void*>(addr), IMAGE_REL_BASED_HIGHLOW);
                }

                JITDUMP("  " FMT_IG ": 0x%p\n", label->GetId(), dstRW[i]);
            }

            continue;
        }

        if (data.kind == ConstData::LabelRel32)
        {
            JITDUMP(", label rel32\n");

            uint32_t*  dstRW     = reinterpret_cast<uint32_t*>(dst + data.offset);
            insGroup** labels    = reinterpret_cast<insGroup**>(data.GetData());
            insGroup*  baseLabel = emitComp->fgFirstBB->emitLabel;

            for (unsigned i = 0, count = data.size / 4; i < count; i++)
            {
                insGroup* label = labels[i];
                assert(FitsIn<uint32_t>(label->igOffs - baseLabel->igOffs));
                dstRW[i] = label->igOffs - baseLabel->igOffs;
                JITDUMP("  " FMT_IG ": 0x%x\n", labels[i]->GetId(), dstRW[i]);
            }

            continue;
        }

        assert(data.kind == ConstData::Const);

        memcpy(dst + data.offset, data.GetData(), data.size);

#ifdef DEBUG
        if (emitComp->verbose)
        {
            printf("\n\t");

            for (uint32_t offset = 0; offset < data.size; offset++)
            {
                printf("%02x ", static_cast<uint8_t*>(data.GetData())[offset]);

                if (((offset + 1) % 16 == 0) && (offset + 1 != data.size))
                {
                    printf("\n\t");
                }
            }

            switch (data.type)
            {
                case TYP_FLOAT:
                    printf(" ; %.9gf", *static_cast<float*>(data.GetData()));
                    break;
                case TYP_DOUBLE:
                    printf(" ; %.17g", *static_cast<double*>(data.GetData()));
                    break;
                default:
                    break;
            }

            printf("\n");
        }
#endif // DEBUG
    }
}

#ifdef DEBUG

void Encoder::PrintRoData() const
{
    printf("\n");

    for (DataSection* section = roData.first; section != nullptr; section = section->next)
    {
        const ConstData& data = section->data;

        const char* labelFormat = "%-7s";
        char        labelName[64];
        sprintf_s(labelName, _countof(labelName), "RWD%02u", data.offset);
        printf(labelFormat, labelName);

        if ((data.kind == ConstData::LabelRel32) || (data.kind == ConstData::LabelAddr))
        {
            bool       relative   = data.kind == ConstData::LabelRel32;
            unsigned   labelCount = data.size / (relative ? 4 : TARGET_POINTER_SIZE);
            insGroup** labels     = reinterpret_cast<insGroup**>(data.GetData());
            insGroup*  baseLabel  = emitComp->fgFirstBB->emitLabel;

            for (unsigned i = 0; i < labelCount; i++)
            {
                if (i > 0)
                {
                    printf(labelFormat, "");
                }

                insGroup* label = labels[i];

                const char* baseLabelName = emitLabelString(emitComp, baseLabel);
                const char* labelName     = emitLabelString(emitComp, label);

                if (emitComp->opts.disDiffable)
                {
                    if (relative)
                    {
                        printf("\tdd\t%s - %s\n", labelName, baseLabelName);
                    }
                    else
                    {
#ifdef TARGET_64BIT
                        printf("\tdq\t%s\n", labelName);
#else
                        printf("\tdd\t%s\n", labelName);
#endif
                    }
                }
                else
                {
                    if (relative)
                    {
                        printf("\tdd\t%08Xh", label->igOffs - baseLabel->igOffs);
                    }
                    else
                    {
                        size_t addr = reinterpret_cast<size_t>(emitOffsetToPtr(label->igOffs));
#ifdef TARGET_64BIT
                        printf("\tdq\t%016llXh", addr);
#else
                        printf("\tdd\t%08Xh", static_cast<uint32_t>(addr));
#endif
                    }

                    printf(" ; case %s\n", labelName);
                }
            }

            continue;
        }

        assert(data.kind == ConstData::Const);

        unsigned elemSize = varTypeSize(data.type);

        if (elemSize == 0)
        {
            if ((data.size % 8) == 0)
            {
                elemSize = 8;
            }
            else if ((data.size % 4) == 0)
            {
                elemSize = 4;
            }
            else if ((data.size % 2) == 0)
            {
                elemSize = 2;
            }
            else
            {
                elemSize = 1;
            }
        }

        for (uint32_t offset = 0; offset < data.size;)
        {
            uint8_t* bytes = static_cast<uint8_t*>(data.GetData());

            if (data.type == TYP_FLOAT)
            {
                assert(data.size >= 4);
                uint32_t bits = reinterpret_cast<uint32_t&>(bytes[offset]);
                offset += 4;
                printf("\tdd\t%08Xh\t", bits);
                printf("\t; %.9gf\n", jitstd::bit_cast<float>(bits));

                continue;
            }

            if (data.type == TYP_DOUBLE)
            {
                assert(data.size >= 8);
                uint64_t bits = reinterpret_cast<uint64_t&>(bytes[offset]);
                offset += 8;
                printf("\tdq\t%016llXh", bits);
                printf("\t; %.17g\n", jitstd::bit_cast<double>(bits));

                continue;
            }

            switch (elemSize)
            {
                case 1:
                    printf("\tdb\t");
                    for (unsigned j = 0; j < 16 && offset < data.size; j++, offset++)
                    {
                        printf("%s%02Xh", j ? ", " : "", reinterpret_cast<uint8_t&>(bytes[offset]));
                    }
                    break;
                case 2:
                    assert(data.size % 2 == 0);
                    printf("\tdw\t");
                    for (unsigned j = 0; j < 12 && offset < data.size; j++, offset += 2)
                    {
                        printf("%s%04Xh", j ? ", " : "", reinterpret_cast<uint16_t&>(bytes[offset]));
                    }
                    break;
                case 12:
                case 4:
                    assert(data.size % 4 == 0);
                    printf("\tdd\t");
                    for (unsigned j = 0; j < 6 && offset < data.size; j++, offset += 4)
                    {
                        printf("%s%08Xh", j ? ", " : "", reinterpret_cast<uint32_t&>(bytes[offset]));
                    }
                    break;
                case 32:
                case 16:
                case 8:
                    assert(data.size % 8 == 0);
                    printf("\tdq\t");
                    for (unsigned j = 0; j < 4 && offset < data.size; j++, offset += 8)
                    {
                        printf("%s%016llXh", j ? ", " : "", reinterpret_cast<uint64_t&>(bytes[offset]));
                    }
                    break;
                default:
                    printf("???");
                    break;
            }

            printf("\n");
        }
    }
}
#endif // DEBUG

void Encoder::emitRecordRelocation(void* location, void* target, uint16_t relocType, int32_t addlDelta)
{
    // If we're an unmatched altjit, don't tell the VM anything. We still
    // record the relocation for late disassembly; maybe we'll need it?
    if (emitComp->info.compMatchedVM)
    {
        void* locationRW = static_cast<uint8_t*>(location) + writeableOffset;
        jitInfo->recordRelocation(location, locationRW, target, relocType, 0, addlDelta);
    }

#ifdef LATE_DISASM
    disasm->disRecordRelocation((size_t)location, (size_t)target);
#endif
}

void Encoder::emitRecordCallSite(unsigned instrOffset, CORINFO_SIG_INFO* callSig, CORINFO_METHOD_HANDLE methodHandle)
{
#ifdef DEBUG
    // Since CORINFO_SIG_INFO is a heavyweight structure, in most cases we can
    // lazily obtain it here using the given method handle (we only save the sig
    // info when we explicitly need it, i.e. for CALLI calls, vararg calls, and
    // tail calls).
    CORINFO_SIG_INFO sigInfo;

    if (callSig == nullptr)
    {
        assert(methodHandle != nullptr);

        if (Compiler::eeGetHelperNum(methodHandle) == CORINFO_HELP_UNDEF)
        {
            emitComp->eeGetMethodSig(methodHandle, &sigInfo);
            callSig = &sigInfo;
        }
    }

    jitInfo->recordCallSite(instrOffset, callSig, methodHandle);
#endif // DEBUG
}

#ifdef DEBUG

// Given a code offset, return a string representing a label for that offset.
// If the code offset is just after the end of the code of the function, the label
// will be "END". If the code offset doesn't correspond to any known offset, the label
// will be "UNKNOWN". The strings are returned from static buffers. This function
// rotates amongst four such static buffers (there are cases where this function is
// called four times to provide data for a single printf()).
const char* EmitterBase::emitOffsetToLabel(unsigned offs)
{
    const size_t    TEMP_BUFFER_LEN = 40;
    static unsigned curBuf          = 0;
    static char     buf[4][TEMP_BUFFER_LEN];
    char*           retbuf;
    unsigned        nextof = 0;

    for (insGroup* ig = emitIGfirst; ig != nullptr; ig = ig->igNext)
    {
        // There is an eventual unused space after the last actual hot block
        // before the first allocated cold block.
        assert((nextof == ig->igOffs) || (ig == emitFirstColdIG));

        if (ig->igOffs == offs)
        {
            return emitLabelString(emitComp, ig);
        }

        if (ig->igOffs > offs)
        {
            // We went past the requested offset but didn't find it.
            sprintf_s(buf[curBuf], TEMP_BUFFER_LEN, "UNKNOWN");
            retbuf = buf[curBuf];
            curBuf = (curBuf + 1) % 4;

            return retbuf;
        }

        nextof = ig->igOffs + ig->igSize;
    }

    if (nextof == offs)
    {
        // It's a pseudo-label to the end.
        sprintf_s(buf[curBuf], TEMP_BUFFER_LEN, "END");
        retbuf = buf[curBuf];
        curBuf = (curBuf + 1) % 4;

        return retbuf;
    }

    sprintf_s(buf[curBuf], TEMP_BUFFER_LEN, "UNKNOWN");
    retbuf = buf[curBuf];
    curBuf = (curBuf + 1) % 4;

    return retbuf;
}

#endif // DEBUG

void Encoder::emitGCvarLiveUpd(int offs, GCtype gcType, uint8_t* addr DEBUGARG(int varNum))
{
    assert(gcType != GCT_NONE);
    assert((varNum < 0) || (emitComp->lvaGetDesc(static_cast<unsigned>(varNum))->HasGCSlotLiveness()));
#if FEATURE_FIXED_OUT_ARGS
    assert(static_cast<unsigned>(varNum) != emitComp->lvaOutgoingArgSpaceVar);
#endif

    unsigned index = gcInfo.GetTrackedStackSlotIndex(offs);

    if (!gcInfo.IsLiveTrackedStackSlot(index))
    {
        gcInfo.BeginStackSlotLifetime(gcType, index, emitCurCodeOffs(addr), offs);
    }
}

#if FEATURE_FIXED_OUT_ARGS

void Encoder::emitGCargLiveUpd(int offs, GCtype gcType, uint8_t* addr DEBUGARG(int varNum))
{
    assert(abs(offs) % REGSIZE_BYTES == 0);
    assert(gcType != GCT_NONE);
    assert(static_cast<unsigned>(varNum) == emitComp->lvaOutgoingArgSpaceVar);

    if (gcInfo.IsFullyInterruptible())
    {
        gcInfo.AddCallArgStore(emitCurCodeOffs(addr), offs, gcType);
    }
}

#endif // FEATURE_FIXED_OUT_ARGS

size_t Encoder::emitRecordGCCall(instrDesc* id, uint8_t* callAddr, uint8_t* callEndAddr)
{
    regMaskTP refRegs;
    regMaskTP byrefRegs;
    VARSET_TP gcLcls;
    X86_ONLY(int argCount;)
    size_t sz;

    if (id->idIsLargeCall())
    {
        instrDescCGCA* idCall = static_cast<instrDescCGCA*>(id);

        refRegs   = idCall->idcGcrefRegs;
        byrefRegs = idCall->idcByrefRegs;
        gcLcls    = idCall->idcGCvars;
        X86_ONLY(argCount = idCall->idcArgCnt);

        sz = sizeof(instrDescCGCA);
    }
    else
    {
        assert(!id->idIsLargeCns());
#ifdef TARGET_XARCH
        assert(!id->idIsLargeDsp());
#endif

        refRegs   = ArchEmitter::DecodeCallGCRegs(id);
        byrefRegs = RBM_NONE;
        gcLcls    = emptyVarSet;
        X86_ONLY(argCount = id->idSmallCns());

        if (id->idGCref() == GCT_GCREF)
        {
            refRegs |= RBM_INTRET;
        }
        else if (id->idGCref() == GCT_BYREF)
        {
            byrefRegs |= RBM_INTRET;
        }

        sz = sizeof(instrDesc);
    }

    unsigned callOffs    = emitCurCodeOffs(callAddr);
    unsigned callEndOffs = callOffs + static_cast<unsigned>(callEndAddr - callAddr);

    if (!currentIG->IsMainEpilog())
    {
        // We update tracked stack slot GC info before the call as they cannot
        // be used by the call (they'd need to be address exposed, thus untracked).
        // Killing stack slots before the call helps with boundary conditions if
        // the call is CORINFO_HELP_THROW.
        // If we ever track aliased locals (which could be used by the call), we
        // would have to keep the corresponding stack slots alive past the call.
        gcInfo.SetLiveLclStackSlots(gcLcls, callOffs);

#ifdef DEBUG
        // And we have to dump the delta here, so it appears before the call instruction
        // in disassembly, instead of appearing after like all other GC info deltas.
        if (emitComp->verbose || emitComp->opts.disasmWithGC)
        {
            char header[128];
            GetGCDeltaDumpHeader(header, _countof(header));
            gcInfo.DumpStackSlotLifetimeDelta(header);
        }
#endif

        if (refRegs != gcInfo.GetLiveRegs(GCT_GCREF))
        {
            gcInfo.SetLiveRegs(GCT_GCREF, refRegs, callEndOffs);
        }

        if (byrefRegs != gcInfo.GetLiveRegs(GCT_BYREF))
        {
            gcInfo.SetLiveRegs(GCT_BYREF, byrefRegs, callEndOffs);
        }
    }

#ifdef JIT32_GCENCODER
    bool isNoGC = id->idIsNoGC();

    if (!isNoGC || (argCount != 0))
    {
        // For callee-pop, all arguments will be popped after the call.
        // For caller-pop, any GC arguments will go dead after the call.

        if (argCount < 0)
        {
            emitStackKillArgs(callEndOffs, -argCount);
        }
        else
        {
            emitStackPopArgs(callEndOffs, argCount);
        }
    }

    if (!isNoGC && gcInfo.ReportCallSites())
    {
        gcInfo.AddCallSite(stackLevel / TARGET_POINTER_SIZE, callEndOffs);
    }
#else
    if (!id->idIsNoGC())
    {
        if (gcInfo.IsFullyInterruptible())
        {
            gcInfo.AddCallArgsKill(callEndOffs);
        }
        else
        {
            gcInfo.AddCallSite(callOffs, callEndOffs);
        }
    }
#endif

#if defined(DEBUG) && defined(TARGET_XARCH)
    if (id->idIns() == INS_call)
    {
        emitRecordCallSite(callOffs, id->idDebugOnlyInfo()->idCallSig,
                           static_cast<CORINFO_METHOD_HANDLE>(id->idDebugOnlyInfo()->idHandle));
    }
#endif

    return sz;
}

void Encoder::emitGCregLiveUpd(GCtype gcType, RegNum reg, uint8_t* addr)
{
    if (!currentIG->IsMainEpilog())
    {
        gcInfo.AddLiveReg(gcType, reg, emitCurCodeOffs(addr));
    }
}

void Encoder::emitGCregDeadUpd(RegNum reg, uint8_t* addr)
{
    if (!currentIG->IsMainEpilog())
    {
        gcInfo.RemoveLiveReg(reg, emitCurCodeOffs(addr));
    }
}

#ifdef FEATURE_EH_FUNCLETS

void Encoder::emitGCregDeadAll(uint8_t* addr)
{
    if (!currentIG->IsMainEpilog())
    {
        gcInfo.RemoveAllLiveRegs(emitCurCodeOffs(addr));
    }
}
#endif // FEATURE_EH_FUNCLETS

#ifdef JIT32_GCENCODER

void Encoder::emitStackPush(unsigned codeOffs, GCtype type)
{
    gcInfo.StackPush(type, stackLevel / TARGET_POINTER_SIZE, codeOffs);
    stackLevel += TARGET_POINTER_SIZE;
}

void Encoder::emitStackPushN(unsigned codeOffs, unsigned count)
{
    gcInfo.StackPushMultiple(count, stackLevel / TARGET_POINTER_SIZE, codeOffs);
    stackLevel += count * TARGET_POINTER_SIZE;
}

void Encoder::emitStackPop(unsigned codeOffs, unsigned count)
{
    gcInfo.StackPop(count, stackLevel / TARGET_POINTER_SIZE, codeOffs, false);
    stackLevel -= count * TARGET_POINTER_SIZE;
}

void Encoder::emitStackPopArgs(unsigned codeOffs, unsigned count)
{
    gcInfo.StackPop(count, stackLevel / TARGET_POINTER_SIZE, codeOffs, true);
    stackLevel -= count * TARGET_POINTER_SIZE;
}

void Encoder::emitStackKillArgs(unsigned codeOffs, unsigned count)
{
    gcInfo.StackKill(count, stackLevel / TARGET_POINTER_SIZE, codeOffs);
}

#endif // JIT32_GCENCODER

#ifdef DEBUG

void Encoder::GetGCDeltaDumpHeader(char* buffer, size_t count)
{
// Interleaved GC info dumping.
// We'll attempt to line this up with the opcode, which indented differently for
// diffable and non-diffable dumps.
// This is approximate, and is better tuned for disassembly than for jitdumps.
// See PrintHexCode().
#ifdef TARGET_AMD64
    constexpr int basicIndent     = 7;
    constexpr int hexEncodingSize = 21;
#elif defined(TARGET_X86)
    constexpr int basicIndent     = 7;
    constexpr int hexEncodingSize = 13;
#elif defined(TARGET_ARM64)
    constexpr int basicIndent     = 12;
    constexpr int hexEncodingSize = 14;
#elif defined(TARGET_ARM)
    constexpr int basicIndent     = 12;
    constexpr int hexEncodingSize = 11;
#endif

    int indent = emitComp->opts.disDiffable ? basicIndent : basicIndent + hexEncodingSize;
    sprintf_s(buffer, count, "%.*s; ", indent, "                             ");
}

#endif // DEBUG
