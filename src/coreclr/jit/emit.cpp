// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                              emit.cpp                                     XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#include "hostallocator.h"
#include "instr.h"
#include "emit.h"
#include "codegen.h"
#include "gcinfotypes.h"

emitter::emitter(Compiler* compiler, CodeGen* codeGen, ICorJitInfo* jitInfo)
    : emitComp(compiler)
    , gcInfo(compiler)
    , codeGen(codeGen)
    , emitCmpHandle(jitInfo)
#ifdef DEBUG
    , debugGCSlotChanges(compiler->getAllocator(CMK_DebugOnly))
#endif
{
}

/*****************************************************************************
 *
 *  Represent an emitter location.
 */

void emitLocation::CaptureLocation(emitter* emit)
{
    ig      = emit->emitCurIG;
    codePos = emit->emitCurOffset();

    assert(Valid());
}

bool emitLocation::IsCurrentLocation(emitter* emit) const
{
    assert(Valid());
    return (ig == emit->emitCurIG) && (codePos == emit->emitCurOffset());
}

UNATIVE_OFFSET emitLocation::CodeOffset(emitter* emit) const
{
    assert(Valid());
    return emit->emitCodeOffset(ig, codePos);
}

int emitLocation::GetInsNum() const
{
    return emitGetInsNumFromCodePos(codePos);
}

// Get the instruction offset in the current instruction group, which must be a funclet prolog group.
// This is used to find an instruction offset used in unwind data.
// TODO-AMD64-Bug?: We only support a single main function prolog group, but allow for multiple funclet prolog
// groups (not that we actually use that flexibility, since the funclet prolog will be small). How to
// handle that?
UNATIVE_OFFSET emitLocation::GetFuncletPrologOffset(emitter* emit) const
{
    assert(ig->igFuncIdx != 0);
    assert((ig->igFlags & IGF_FUNCLET_PROLOG) != 0);
    assert(ig == emit->emitCurIG);

    return emit->emitCurIGsize;
}

//------------------------------------------------------------------------
// IsPreviousInsNum: Returns true if the emitter is on the next instruction
//  of the same group as this emitLocation.
//
// Arguments:
//  emit - an emitter* instance
//
bool emitLocation::IsPreviousInsNum(emitter* emit) const
{
    assert(Valid());

    // Within the same IG?
    if (ig == emit->emitCurIG)
    {
        return (emitGetInsNumFromCodePos(codePos) == emitGetInsNumFromCodePos(emit->emitCurOffset()) - 1);
    }

    // Spanning an IG boundary?
    if (ig->igNext == emit->emitCurIG)
    {
        return (emitGetInsNumFromCodePos(codePos) == ig->igInsCnt) && (emit->emitCurIGinsCnt == 1);
    }

    return false;
}

#ifdef DEBUG

void emitLocation::Print(LONG compMethodID) const
{
    unsigned insNum = emitGetInsNumFromCodePos(codePos);
    unsigned insOfs = emitGetInsOfsFromCodePos(codePos);
    printf("(G_M%03u_IG%02u,ins#%d,ofs#%d)", compMethodID, ig->igNum, insNum, insOfs);
}

//-----------------------------------------------------------------------------
// genInsDisplayName: Get a fully-formed instruction display name. This only handles
// the xarch case of prepending a "v", not the arm case of appending an "s".
// This can be called up to four times in a single 'printf' before the static buffers
// get reused.
//
// Returns:
//    String with instruction name
//
const char* emitter::genInsDisplayName(instrDesc* id)
{
    instruction ins  = id->idIns();
    const char* name = insName(ins);

#ifdef TARGET_XARCH
    const int       TEMP_BUFFER_LEN = 40;
    static unsigned curBuf          = 0;
    static char     buf[4][TEMP_BUFFER_LEN];
    const char*     retbuf;

    if (IsAVXInstruction(ins) && !IsBMIInstruction(ins))
    {
        sprintf_s(buf[curBuf], TEMP_BUFFER_LEN, "v%s", name);
        retbuf = buf[curBuf];
        curBuf = (curBuf + 1) % 4;
        return retbuf;
    }
#endif // TARGET_XARCH

    return name;
}

#endif // DEBUG

/*****************************************************************************
 *
 *  Return the name of an instruction format.
 */

#if defined(DEBUG) || EMITTER_STATS

const char* emitter::emitIfName(unsigned f)
{
    static const char* const ifNames[] = {
#define IF_DEF(en, op1, op2) "IF_" #en,
#include "emitfmts.h"
    };

    static char errBuff[32];

    if (f < _countof(ifNames))
    {
        return ifNames[f];
    }

    sprintf_s(errBuff, sizeof(errBuff), "??%u??", f);
    return errBuff;
}

#endif

/*****************************************************************************/

#if EMITTER_STATS

static unsigned totAllocdSize;
static unsigned totActualSize;

unsigned emitter::emitIFcounts[emitter::IF_COUNT];

static unsigned  emitSizeBuckets[] = {100, 1024 * 1, 1024 * 2, 1024 * 3, 1024 * 4, 1024 * 5, 1024 * 10, 0};
static Histogram emitSizeTable(emitSizeBuckets);

static unsigned  GCrefsBuckets[] = {0, 1, 2, 5, 10, 20, 50, 128, 256, 512, 1024, 0};
static Histogram GCrefsTable(GCrefsBuckets);

static unsigned  stkDepthBuckets[] = {0, 1, 2, 5, 10, 16, 32, 128, 1024, 0};
static Histogram stkDepthTable(stkDepthBuckets);

size_t emitter::emitSizeMethod;

size_t   emitter::emitTotMemAlloc;
unsigned emitter::emitTotalInsCnt;
unsigned emitter::emitCurPrologInsCnt;
size_t   emitter::emitCurPrologIGSize;
unsigned emitter::emitMaxPrologInsCnt;
size_t   emitter::emitMaxPrologIGSize;
unsigned emitter::emitTotalIGcnt;
unsigned emitter::emitTotalPhIGcnt;
unsigned emitter::emitTotalIGjmps;
unsigned emitter::emitTotalIGptrs;
unsigned emitter::emitTotalIGicnt;
size_t   emitter::emitTotalIGsize;
unsigned emitter::emitTotalIGmcnt;
unsigned emitter::emitTotalIGExtend;

unsigned emitter::emitTotalIDescSmallCnt;
unsigned emitter::emitTotalIDescCnt;
unsigned emitter::emitTotalIDescJmpCnt;
unsigned emitter::emitTotalIDescCnsCnt;
unsigned emitter::emitTotalIDescDspCnt;
unsigned emitter::emitTotalIDescCnsDspCnt;
#ifdef TARGET_XARCH
unsigned emitter::emitTotalIDescAmdCnt;
unsigned emitter::emitTotalIDescCnsAmdCnt;
#endif // TARGET_XARCH
unsigned emitter::emitTotalIDescCGCACnt;
#ifdef TARGET_ARM
unsigned emitter::emitTotalIDescRelocCnt;
#endif // TARGET_ARM

unsigned emitter::emitSmallDspCnt;
unsigned emitter::emitLargeDspCnt;

unsigned emitter::emitSmallCnsCnt;
unsigned emitter::emitLargeCnsCnt;
unsigned emitter::emitSmallCns[SMALL_CNS_TSZ];

unsigned emitter::emitTotalDescAlignCnt;

void emitterStats(FILE* fout)
{
    if (totAllocdSize > 0)
    {
        assert(totActualSize <= totAllocdSize);

        fprintf(fout, "\nTotal allocated code size = %u\n", totAllocdSize);

        if (totActualSize < totAllocdSize)
        {
            fprintf(fout, "Total generated code size = %u  ", totActualSize);

            fprintf(fout, "(%4.3f%% waste)", 100 * ((totAllocdSize - totActualSize) / (double)totActualSize));
            fprintf(fout, "\n");
        }

        assert(emitter::emitTotalInsCnt > 0);

        fprintf(fout, "Average of %4.2f bytes of code generated per instruction\n",
                (double)totActualSize / emitter::emitTotalInsCnt);
    }

    fprintf(fout, "\nInstruction format frequency table:\n\n");

    unsigned f, ic = 0, dc = 0;

    for (f = 0; f < emitter::IF_COUNT; f++)
    {
        ic += emitter::emitIFcounts[f];
    }

    for (f = 0; f < emitter::IF_COUNT; f++)
    {
        unsigned c = emitter::emitIFcounts[f];

        if ((c > 0) && (1000 * c >= ic))
        {
            dc += c;
            fprintf(fout, "          %-14s %8u (%5.2f%%)\n", emitter::emitIfName(f), c, 100.0 * c / ic);
        }
    }

    fprintf(fout, "         ---------------------------------\n");
    fprintf(fout, "          %-14s %8u (%5.2f%%)\n", "Total shown", dc, 100.0 * dc / ic);

    if (emitter::emitTotalIGmcnt > 0)
    {
        fprintf(fout, "\n");
        fprintf(fout, "Total of %8u methods\n", emitter::emitTotalIGmcnt);
        fprintf(fout, "Total of %8u insGroup\n", emitter::emitTotalIGcnt);
        fprintf(fout, "Total of %8u insPlaceholderGroupData\n", emitter::emitTotalPhIGcnt);
        fprintf(fout, "Total of %8u extend insGroup\n", emitter::emitTotalIGExtend);
        fprintf(fout, "Total of %8u instructions\n", emitter::emitTotalIGicnt);
        fprintf(fout, "Total of %8u jumps\n", emitter::emitTotalIGjmps);
        fprintf(fout, "Total of %8u GC livesets\n", emitter::emitTotalIGptrs);
        fprintf(fout, "\n");
        fprintf(fout, "Max prolog instrDesc count: %8u\n", emitter::emitMaxPrologInsCnt);
        fprintf(fout, "Max prolog insGroup size  : %8zu\n", emitter::emitMaxPrologIGSize);
        fprintf(fout, "\n");
        fprintf(fout, "Average of %8.1lf insGroup     per method\n",
                (double)emitter::emitTotalIGcnt / emitter::emitTotalIGmcnt);
        fprintf(fout, "Average of %8.1lf insPhGroup   per method\n",
                (double)emitter::emitTotalPhIGcnt / emitter::emitTotalIGmcnt);
        fprintf(fout, "Average of %8.1lf extend IG    per method\n",
                (double)emitter::emitTotalIGExtend / emitter::emitTotalIGmcnt);
        fprintf(fout, "Average of %8.1lf instructions per method\n",
                (double)emitter::emitTotalIGicnt / emitter::emitTotalIGmcnt);
        fprintf(fout, "Average of %8.1lf desc.  bytes per method\n",
                (double)emitter::emitTotalIGsize / emitter::emitTotalIGmcnt);
        fprintf(fout, "Average of %8.1lf jumps        per method\n",
                (double)emitter::emitTotalIGjmps / emitter::emitTotalIGmcnt);
        fprintf(fout, "Average of %8.1lf GC livesets  per method\n",
                (double)emitter::emitTotalIGptrs / emitter::emitTotalIGmcnt);
        fprintf(fout, "\n");
        fprintf(fout, "Average of %8.1lf instructions per group \n",
                (double)emitter::emitTotalIGicnt / emitter::emitTotalIGcnt);
        fprintf(fout, "Average of %8.1lf desc.  bytes per group \n",
                (double)emitter::emitTotalIGsize / emitter::emitTotalIGcnt);
        fprintf(fout, "Average of %8.1lf jumps        per group \n",
                (double)emitter::emitTotalIGjmps / emitter::emitTotalIGcnt);
        fprintf(fout, "\n");
        fprintf(fout, "Average of %8.1lf bytes        per instrDesc\n",
                (double)emitter::emitTotalIGsize / emitter::emitTotalIGicnt);
        fprintf(fout, "\n");
        fprintf(fout, "A total of %8zu desc.  bytes\n", emitter::emitTotalIGsize);
        fprintf(fout, "\n");

        fprintf(fout, "Total instructions:    %8u\n", emitter::emitTotalInsCnt);
        fprintf(fout, "Total small instrDesc: %8u (%5.2f%%)\n", emitter::emitTotalIDescSmallCnt,
                100.0 * emitter::emitTotalIDescSmallCnt / emitter::emitTotalInsCnt);
        fprintf(fout, "Total instrDesc:       %8u (%5.2f%%)\n", emitter::emitTotalIDescCnt,
                100.0 * emitter::emitTotalIDescCnt / emitter::emitTotalInsCnt);
        fprintf(fout, "Total instrDescJmp:    %8u (%5.2f%%)\n", emitter::emitTotalIDescJmpCnt,
                100.0 * emitter::emitTotalIDescJmpCnt / emitter::emitTotalInsCnt);
        fprintf(fout, "Total instrDescCns:    %8u (%5.2f%%)\n", emitter::emitTotalIDescCnsCnt,
                100.0 * emitter::emitTotalIDescCnsCnt / emitter::emitTotalInsCnt);
        fprintf(fout, "Total instrDescDsp:    %8u (%5.2f%%)\n", emitter::emitTotalIDescDspCnt,
                100.0 * emitter::emitTotalIDescDspCnt / emitter::emitTotalInsCnt);
        fprintf(fout, "Total instrDescCnsDsp: %8u (%5.2f%%)\n", emitter::emitTotalIDescCnsDspCnt,
                100.0 * emitter::emitTotalIDescCnsDspCnt / emitter::emitTotalInsCnt);
#ifdef TARGET_XARCH
        fprintf(fout, "Total instrDescAmd:    %8u (%5.2f%%)\n", emitter::emitTotalIDescAmdCnt,
                100.0 * emitter::emitTotalIDescAmdCnt / emitter::emitTotalInsCnt);
        fprintf(fout, "Total instrDescCnsAmd: %8u (%5.2f%%)\n", emitter::emitTotalIDescCnsAmdCnt,
                100.0 * emitter::emitTotalIDescCnsAmdCnt / emitter::emitTotalInsCnt);
#endif // TARGET_XARCH
        fprintf(fout, "Total instrDescCGCA:   %8u (%5.2f%%)\n", emitter::emitTotalIDescCGCACnt,
                100.0 * emitter::emitTotalIDescCGCACnt / emitter::emitTotalInsCnt);
#ifdef TARGET_ARM
        fprintf(fout, "Total instrDescReloc:  %8u (%5.2f%%)\n", emitter::emitTotalIDescRelocCnt,
                100.0 * emitter::emitTotalIDescRelocCnt / emitter::emitTotalInsCnt);
#endif // TARGET_ARM
        fprintf(fout, "Total instrDescAlign:  %8u (%5.2f%%)\n", emitter::emitTotalDescAlignCnt,
                100.0 * emitter::emitTotalDescAlignCnt / emitter::emitTotalInsCnt);

        fprintf(fout, "\n");
    }

    fprintf(fout, "Descriptor size distribution:\n");
    emitSizeTable.dump(fout);
    fprintf(fout, "\n");

    fprintf(fout, "GC ref frame variable counts:\n");
    GCrefsTable.dump(fout);
    fprintf(fout, "\n");

    fprintf(fout, "Max. stack depth distribution:\n");
    stkDepthTable.dump(fout);
    fprintf(fout, "\n");

    if ((emitter::emitSmallCnsCnt > 0) || (emitter::emitLargeCnsCnt > 0))
    {
        fprintf(fout, "SmallCnsCnt = %6u\n", emitter::emitSmallCnsCnt);
        fprintf(fout, "LargeCnsCnt = %6u (%3u %% of total)\n", emitter::emitLargeCnsCnt,
                100 * emitter::emitLargeCnsCnt / (emitter::emitLargeCnsCnt + emitter::emitSmallCnsCnt));
    }

    // Print out the most common small constants.
    if (emitter::emitSmallCnsCnt > 0)
    {
        fprintf(fout, "\n\n");
        fprintf(fout, "Common small constants >= %2u, <= %2u\n", ID_MIN_SMALL_CNS, ID_MAX_SMALL_CNS);

        unsigned m = emitter::emitSmallCnsCnt / 1000 + 1;

        for (int i = ID_MIN_SMALL_CNS; (i <= ID_MAX_SMALL_CNS) && (i < SMALL_CNS_TSZ); i++)
        {
            unsigned c = emitter::emitSmallCns[i - ID_MIN_SMALL_CNS];
            if (c >= m)
            {
                if (i == SMALL_CNS_TSZ - 1)
                {
                    fprintf(fout, "cns[>=%4d] = %u\n", i, c);
                }
                else
                {
                    fprintf(fout, "cns[%4d] = %u\n", i, c);
                }
            }
        }
    }

    fprintf(fout, "%8zu bytes allocated in the emitter\n", emitter::emitTotMemAlloc);
}

#endif // EMITTER_STATS

/*****************************************************************************/

const uint16_t emitTypeSizes[]{
#define DEF_TP(tn, nm, jitType, sz, sze, asze, al, tf) sze,
#include "typelist.h"
};

const uint16_t emitTypeActSz[]{
#define DEF_TP(tn, nm, jitType, sz, sze, asze, al, tf) asze,
#include "typelist.h"
};

/*****************************************************************************
 *
 *  Allocate memory.
 */

void* emitter::emitGetMem(size_t sz)
{
    assert(sz % sizeof(int) == 0);

#if EMITTER_STATS
    emitTotMemAlloc += sz;
#endif

    return emitComp->getAllocator(CMK_InstDesc).allocate<char>(sz);
}

void emitter::emitGenIG(insGroup* ig)
{
    /* Set the "current IG" value */

    emitCurIG = ig;

#if !FEATURE_FIXED_OUT_ARGS
    ig->igStkLvl = emitCurStackLvl;
#endif

    if (emitNoGCIG)
    {
        ig->igFlags |= IGF_NOGCINTERRUPT;
    }

    /* Prepare to issue instructions */

    emitCurIGinsCnt = 0;
    emitCurIGsize   = 0;

    assert(emitCurIGjmpList == nullptr);

#if FEATURE_LOOP_ALIGN
    assert(emitCurIGAlignList == nullptr);
#endif

    /* Allocate the temp instruction buffer if we haven't done so */

    if (emitCurIGfreeBase == nullptr)
    {
        emitIGbuffSize    = SC_IG_BUFFER_SIZE;
        emitCurIGfreeBase = (BYTE*)emitGetMem(emitIGbuffSize);
    }

    emitCurIGfreeNext = emitCurIGfreeBase;
    emitCurIGfreeEndp = emitCurIGfreeBase + emitIGbuffSize;
}

/*****************************************************************************
 *
 *  Finish and save the current IG.
 */

insGroup* emitter::emitSavIG(bool emitAdd)
{
    insGroup* ig;
    BYTE*     id;

    size_t sz;
    size_t gs;

    assert(emitCurIGfreeNext <= emitCurIGfreeEndp);

    // Get hold of the IG descriptor

    ig = emitCurIG;
    assert(ig);

    // Compute how much code we've generated

    sz = emitCurIGfreeNext - emitCurIGfreeBase;

    // Compute the total size we need to allocate

    gs = roundUp(sz);

    // Do we need space for GC?

    if (!(ig->igFlags & IGF_EXTEND))
    {
        // Is the initial set of live GC vars different from the previous one?

        if (emitForceStoreGCState || !VarSetOps::Equal(emitComp, emitPrevGCrefVars, emitInitGCrefVars))
        {
            // Remember that we will have a new set of live GC variables

            ig->igFlags |= IGF_GC_VARS;

#if EMITTER_STATS
            emitTotalIGptrs++;
#endif

            // We'll allocate extra space to record the liveset

            gs += sizeof(VARSET_TP);
        }

        // Is the initial set of live Byref regs different from the previous one?

        // Remember that we will have a new set of live GC variables

        ig->igFlags |= IGF_BYREF_REGS;

        // We'll allocate extra space (DWORD aligned) to record the GC regs

        gs += sizeof(int);
    }

    // Allocate space for the instructions and optional liveset

    id = (BYTE*)emitGetMem(gs);

    // Do we need to store the byref regs

    if (ig->igFlags & IGF_BYREF_REGS)
    {
        // Record the byref regs in front the of the instructions

        *castto(id, unsigned*)++ = (unsigned)emitInitByrefRegs;
    }

    // Do we need to store the liveset?

    if (ig->igFlags & IGF_GC_VARS)
    {
        // Record the liveset in front the of the instructions
        VarSetOps::AssignNoCopy(emitComp, (*castto(id, VARSET_TP*)), VarSetOps::MakeEmpty(emitComp));
        VarSetOps::Assign(emitComp, (*castto(id, VARSET_TP*)++), emitInitGCrefVars);
    }

    // Record the collected instructions

    assert((ig->igFlags & IGF_PLACEHOLDER) == 0);
    ig->igData = id;

    memcpy(id, emitCurIGfreeBase, sz);

#ifdef DEBUG
    if (false && emitComp->verbose) // this is not useful in normal dumps (hence it is normally under if (false))
    {
        // If there's an error during emission, we may want to connect the post-copy address
        // of an instrDesc with the pre-copy address (the one that was originally created).  This
        // printing enables that.
        printf("copying instruction group from [0x%x..0x%x) to [0x%x..0x%x).\n", dspPtr(emitCurIGfreeBase),
               dspPtr(emitCurIGfreeBase + sz), dspPtr(id), dspPtr(id + sz));
    }
#endif

    // Record how many instructions and bytes of code this group contains

    noway_assert((BYTE)emitCurIGinsCnt == emitCurIGinsCnt);
    noway_assert((unsigned short)emitCurIGsize == emitCurIGsize);

    ig->igInsCnt = (BYTE)emitCurIGinsCnt;
    ig->igSize   = (unsigned short)emitCurIGsize;
    emitCurCodeOffset += emitCurIGsize;
    assert(IsCodeAligned(emitCurCodeOffset));

#if EMITTER_STATS
    emitTotalIGicnt += emitCurIGinsCnt;
    emitTotalIGsize += sz;
    emitSizeMethod += sz;

    if (emitIGisInProlog(ig))
    {
        emitCurPrologInsCnt += emitCurIGinsCnt;
        emitCurPrologIGSize += sz;

        // Keep track of the maximums.
        if (emitCurPrologInsCnt > emitMaxPrologInsCnt)
        {
            emitMaxPrologInsCnt = emitCurPrologInsCnt;
        }
        if (emitCurPrologIGSize > emitMaxPrologIGSize)
        {
            emitMaxPrologIGSize = emitCurPrologIGSize;
        }
    }
#endif

    // Record the live GC register set - if and only if it is not an extension
    // block, in which case the GC register sets are inherited from the previous
    // block.

    if (!(ig->igFlags & IGF_EXTEND))
    {
        ig->igGCregs = (regMaskSmall)emitInitGCrefRegs;
    }

    if (!emitAdd)
    {
        // Update the previous recorded live GC ref sets, but not if if we are
        // starting an "overflow" buffer. Note that this is only used to
        // determine whether we need to store or not store the GC ref sets for
        // the next IG, which is dependent on exactly what the state of the
        // emitter GC ref sets will be when the next IG is processed in the
        // emitter.

        VarSetOps::Assign(emitComp, emitPrevGCrefVars, emitThisGCrefVars);
        emitPrevGCrefRegs = emitThisGCrefRegs;
        emitPrevByrefRegs = emitThisByrefRegs;

        emitForceStoreGCState = false;
    }

#ifdef DEBUG
    if (emitComp->opts.dspCode)
    {
        printf("\n      %s:", emitLabelString(ig));
        if (emitComp->verbose)
        {
            printf("        ; offs=%06XH, funclet=%02u, bbWeight=%s", ig->igOffs, ig->igFuncIdx,
                   refCntWtd2str(ig->igWeight));
        }
        else
        {
            printf("        ; funclet=%02u", ig->igFuncIdx);
        }
        printf("\n");
    }
#endif

#if FEATURE_LOOP_ALIGN
    // Did we have any align instructions in this group?
    if (emitCurIGAlignList)
    {
        instrDescAlign* list = nullptr;
        instrDescAlign* last = nullptr;

        // Move align instructions to the global list, update their 'next' links
        do
        {
            // Grab the jump and remove it from the list

            instrDescAlign* oa = emitCurIGAlignList;
            emitCurIGAlignList = oa->idaNext;

            // Figure out the address of where the align got copied

            size_t          of = (BYTE*)oa - emitCurIGfreeBase;
            instrDescAlign* na = (instrDescAlign*)(ig->igData + of);

            assert(na->idaIG == ig);
            assert(na->idIns() == oa->idIns());
            assert(na->idaNext == oa->idaNext);
            assert(na->idIns() == INS_align);

            na->idaNext = list;
            list        = na;

            if (last == nullptr)
            {
                last = na;
            }
        } while (emitCurIGAlignList);

        // Should have at least one align instruction
        assert(last);

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

#endif
    // Did we have any jumps in this group?

    if (emitCurIGjmpList)
    {
        instrDescJmp* list = nullptr;
        instrDescJmp* last = nullptr;

        // Move jumps to the global list, update their 'next' links

        do
        {
            // Grab the jump and remove it from the list

            instrDescJmp* oj = emitCurIGjmpList;
            emitCurIGjmpList = oj->idjNext;

            // Figure out the address of where the jump got copied

            size_t        of = (BYTE*)oj - emitCurIGfreeBase;
            instrDescJmp* nj = (instrDescJmp*)(ig->igData + of);

            assert(nj->idjIG == ig);
            assert(nj->idIns() == oj->idIns());
            assert(nj->idjNext == oj->idjNext);

            // Make sure the jumps are correctly ordered

            assert(last == nullptr || last->idjOffs > nj->idjOffs);

            if (ig->igFlags & IGF_FUNCLET_PROLOG)
            {
                // Our funclet prologs have short jumps, if the prolog would ever have
                // long jumps, then we'd have to insert the list in sorted order than
                // just append to the emitJumpList.
                noway_assert(nj->idjShort);
                if (nj->idjShort)
                {
                    continue;
                }
            }

            // Append the new jump to the list

            nj->idjNext = list;
            list        = nj;

            if (last == nullptr)
            {
                last = nj;
            }
        } while (emitCurIGjmpList);

        if (last != nullptr)
        {
            // Append the jump(s) from this IG to the global list
            bool prologJump = (ig == emitPrologIG);
            if ((emitJumpList == nullptr) || prologJump)
            {
                last->idjNext = emitJumpList;
                emitJumpList  = list;
            }
            else
            {
                last->idjNext         = nullptr;
                emitJumpLast->idjNext = list;
            }

            if (!prologJump || (emitJumpLast == nullptr))
            {
                emitJumpLast = last;
            }
        }
    }

    // Fix the last instruction field

    if (sz != 0)
    {
        assert(emitLastIns != nullptr);
        assert(emitCurIGfreeBase <= (BYTE*)emitLastIns);
        assert((BYTE*)emitLastIns < emitCurIGfreeBase + sz);
        emitLastIns = (instrDesc*)((BYTE*)id + ((BYTE*)emitLastIns - (BYTE*)emitCurIGfreeBase));
    }

    // Reset the buffer free pointers

    emitCurIGfreeNext = emitCurIGfreeBase;

    return ig;
}

void emitter::emitBegFN()
{
    emitPrevGCrefVars = VarSetOps::MakeEmpty(emitComp);
    emitInitGCrefVars = VarSetOps::MakeEmpty(emitComp);
    emitThisGCrefVars = VarSetOps::MakeEmpty(emitComp);
#ifdef DEBUG
    emitChkAlign =
        (emitComp->compCodeOpt() != SMALL_CODE) && !emitComp->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT);
#endif

#if EMITTER_STATS
    emitTotalIGmcnt++;
    emitSizeMethod      = 0;
    emitCurPrologInsCnt = 0;
    emitCurPrologIGSize = 0;
#endif

#if !FEATURE_FIXED_OUT_ARGS
    emitCntStackDepth = 4;
#endif

#ifdef PSEUDORANDOM_NOP_INSERTION
    emitEnableRandomNops();
    emitComp->info.compRNG.Init(emitComp->info.compChecksum);
    emitNextNop = emitNextRandomNop();
#endif

    // Create the first IG, it will be used for the prolog.

    emitNxtIGnum = 1;

    insGroup* ig = emitAllocIG();
    ig->igNext   = nullptr;

    emitPrologIG = ig;
    emitIGlist   = ig;
    emitIGlast   = ig;
    emitCurIG    = ig;

    // Append another group, to start generating the method body

    emitNewIG();
}

#ifdef PSEUDORANDOM_NOP_INSERTION
int emitter::emitNextRandomNop()
{
    return emitComp->info.compRNG.Next(1, 9);
}
#endif

// member function iiaIsJitDataOffset for idAddrUnion, defers to Compiler::eeIsJitDataOffs
bool emitter::instrDesc::idAddrUnion::iiaIsJitDataOffset() const
{
    return Compiler::eeIsJitDataOffs(iiaFieldHnd);
}

// member function iiaGetJitDataOffset for idAddrUnion, defers to Compiler::eeGetJitDataOffs
int emitter::instrDesc::idAddrUnion::iiaGetJitDataOffset() const
{
    assert(iiaIsJitDataOffset());
    return Compiler::eeGetJitDataOffs(iiaFieldHnd);
}

#if defined(DEBUG) || defined(LATE_DISASM)

//----------------------------------------------------------------------------------------
// insEvaluateExecutionCost:
//    Returns the estimated execution cost for the current instruction
//
// Arguments:
//    id  - The current instruction descriptor to be evaluated
//
// Return Value:
//    calls getInsExecutionCharacteristics and uses the result
//    to compute an estimated execution cost
//
float emitter::insEvaluateExecutionCost(instrDesc* id)
{
    insExecutionCharacteristics result        = getInsExecutionCharacteristics(id);
    float                       throughput    = result.insThroughput;
    float                       latency       = result.insLatency;
    unsigned                    memAccessKind = result.insMemoryAccessKind;

    // Check for PERFSCORE_THROUGHPUT_ILLEGAL and PERFSCORE_LATENCY_ILLEGAL.
    // Note that 0.0 throughput is allowed for pseudo-instructions in the instrDesc list that won't actually
    // generate code.
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
        //
        latency -= 1.0;
    }

    return max(throughput, latency);
}

//------------------------------------------------------------------------------------
// perfScoreUnhandledInstruction:
//    Helper method used to report an unhandled instruction
//
// Arguments:
//    id  - The current instruction descriptor to be evaluated
//    pResult - pointer to struct holding the instruction characteristics
//              if we return these are updated with default values
//
// Notes:
//     We print the instruction and instruction group
//     and instead of returning we will assert
//
//     This method asserts with a debug/checked build
//     and returns default latencies of 1 cycle otherwise.
//
void emitter::perfScoreUnhandledInstruction(instrDesc* id, insExecutionCharacteristics* pResult)
{
#ifdef DEBUG
    printf("PerfScore: unhandled instruction: %s, format %s", genInsDisplayName(id), emitIfName(id->idInsFmt()));
    assert(!"PerfScore: unhandled instruction");
#endif
    pResult->insThroughput = PERFSCORE_THROUGHPUT_1C;
    pResult->insLatency    = PERFSCORE_LATENCY_1C;
}

#endif // defined(DEBUG) || defined(LATE_DISASM)

//----------------------------------------------------------------------------------------
// getCurrentBlockWeight: Return the block weight for the currently active block
//
// Arguments:
//    None
//
// Return Value:
//    The block weight for the current block
//
// Notes:
//    The current block is recorded in emitComp->compCurBB by
//    CodeGen::genCodeForBBlist() as it walks the blocks.
//    When we are in the prolog/epilog this value is nullptr.
//
BasicBlock::weight_t emitter::getCurrentBlockWeight()
{
    // If we have a non-null compCurBB, then use it to get the current block weight
    if (emitComp->compCurBB != nullptr)
    {
        return emitComp->compCurBB->getBBWeight(emitComp);
    }
    else // we have a null compCurBB
    {
        // prolog or epilog case, so just use the standard weight
        return BB_UNITY_WEIGHT;
    }
}

void emitter::dispIns(instrDesc* id)
{
#ifdef DEBUG
    emitInsSanityCheck(id);

    if (emitComp->opts.dspCode)
    {
        emitDispIns(id, true);
    }

#if !FEATURE_FIXED_OUT_ARGS
    assert((int)emitCurStackLvl >= 0);
#endif

    size_t sz = emitSizeOfInsDsc(id);
    assert(id->idDebugOnlyInfo()->idSize == sz);
#endif // DEBUG

#if EMITTER_STATS
    emitIFcounts[id->idInsFmt()]++;
#endif
}

void emitter::appendToCurIG(instrDesc* id)
{
    emitCurIGsize += id->idCodeSize();
}

/*****************************************************************************
 *
 *  Display (optionally) an instruction offset.
 */

#ifdef DEBUG

void emitter::emitDispInsAddr(BYTE* code)
{
    if (emitComp->opts.disAddr)
    {
        printf(FMT_ADDR, DBG_ADDR(code));
    }
}

void emitter::emitDispInsOffs(unsigned offs, bool doffs)
{
    if (doffs)
    {
        printf("%06X", offs);
    }
    else
    {
        printf("      ");
    }
}

#endif // DEBUG

/*****************************************************************************
 *
 *  The following series of methods allocates instruction descriptors.
 */

void* emitter::emitAllocAnyInstr(unsigned sz, emitAttr opsz)
{
    instrDesc* id;

#ifdef DEBUG
    // Under STRESS_EMITTER, put every instruction in its own instruction group.
    // We can't do this for a prolog, epilog, funclet prolog, or funclet epilog,
    // because those are generated out of order. We currently have a limitation
    // where the jump shortening pass uses the instruction group number to determine
    // if something is earlier or later in the code stream. This implies that
    // these groups cannot be more than a single instruction group. Note that
    // the prolog/epilog placeholder groups ARE generated in order, and are
    // re-used. But generating additional groups would not work.
    if (emitComp->compStressCompile(Compiler::STRESS_EMITTER, 1) && emitCurIGinsCnt && !emitIGisInProlog(emitCurIG) &&
        !emitIGisInEpilog(emitCurIG)
#if defined(FEATURE_EH_FUNCLETS)
        && !emitIGisInFuncletProlog(emitCurIG) && !emitIGisInFuncletEpilog(emitCurIG)
#endif // FEATURE_EH_FUNCLETS
            )
    {
        emitNxtIG(true);
    }
#endif

#ifdef PSEUDORANDOM_NOP_INSERTION
    // TODO-ARM-Bug?: PSEUDORANDOM_NOP_INSERTION is not defined for TARGET_ARM
    //     ARM - This is currently broken on TARGET_ARM
    //     When nopSize is odd we misalign emitCurIGsize
    //
    if (!emitComp->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT) && !emitInInstrumentation &&
        !emitIGisInProlog(emitCurIG) && // don't do this in prolog or epilog
        !emitIGisInEpilog(emitCurIG) &&
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
#if defined(TARGET_XARCH)
            idnop->idCodeSize(nopSize);
#else
#error "Undefined target for pseudorandom NOP insertion"
#endif

            emitCurIGsize += nopSize;
            emitNextNop = emitNextRandomNop();
        }
        else
            emitNextNop--;
    }
#endif // PSEUDORANDOM_NOP_INSERTION

    assert(IsCodeAligned(emitCurIGsize));

    /* Make sure we have enough space for the new instruction */

    if ((emitCurIGfreeNext + sz >= emitCurIGfreeEndp) || emitForceNewIG)
    {
        emitNxtIG(true);
    }

    /* Grab the space for the instruction */

    emitLastIns = id = (instrDesc*)emitCurIGfreeNext;
    emitCurIGfreeNext += sz;

    assert(sz >= sizeof(void*));
    memset(id, 0, sz);

    // These fields should have been zero-ed by the above
    assert(id->idReg1() == regNumber(0));
    assert(id->idReg2() == regNumber(0));
#ifdef TARGET_XARCH
    assert(id->idCodeSize() == 0);
#endif

    emitInsCount++;

    INDEBUG(id->idDebugOnlyInfo(new (emitComp, CMK_DebugOnly) instrDescDebugInfo(emitInsCount, sz)));

    /* Store the size and handle the two special values
       that indicate GCref and ByRef */

    if (EA_IS_GCREF(opsz))
    {
        /* A special value indicates a GCref pointer value */

        id->idGCref(GCT_GCREF);
        id->idOpSize(EA_PTRSIZE);
    }
    else if (EA_IS_BYREF(opsz))
    {
        /* A special value indicates a Byref pointer value */

        id->idGCref(GCT_BYREF);
        id->idOpSize(EA_PTRSIZE);
    }
    else
    {
        id->idGCref(GCT_NONE);
        id->idOpSize(EA_SIZE(opsz));
    }

    // Amd64: ip-relative addressing is supported even when not generating relocatable ngen code
    if (EA_IS_DSP_RELOC(opsz)
#ifndef TARGET_AMD64
        && emitComp->opts.compReloc
#endif // TARGET_AMD64
        )
    {
        /* Mark idInfo()->idDspReloc to remember that the            */
        /* address mode has a displacement that is relocatable       */
        id->idSetIsDspReloc();
    }

    if (EA_IS_CNS_RELOC(opsz) && emitComp->opts.compReloc)
    {
        /* Mark idInfo()->idCnsReloc to remember that the            */
        /* instruction has an immediate constant that is relocatable */
        id->idSetIsCnsReloc();
    }

#if EMITTER_STATS
    emitTotalInsCnt++;
#endif

    /* Update the instruction count */

    emitCurIGinsCnt++;

#ifdef DEBUG
    if (emitComp->compCurBB != emitCurIG->lastGeneratedBlock)
    {
        emitCurIG->igBlocks.push_back(emitComp->compCurBB);
        emitCurIG->lastGeneratedBlock = emitComp->compCurBB;
    }
#endif // DEBUG

    return id;
}

#ifdef DEBUG

//------------------------------------------------------------------------
// emitCheckIGoffsets: Make sure the code offsets of all instruction groups look reasonable.
//
// Note: It checks that each instruction group starts right after the previous ig.
// For the first cold ig offset is also should be the last hot ig + its size.
// emitCurCodeOffs maintains distance for the split case to look like they are consistent.
// Also it checks total code size.
//
void emitter::emitCheckIGoffsets()
{
    size_t currentOffset = 0;

    for (insGroup* tempIG = emitIGlist; tempIG != nullptr; tempIG = tempIG->igNext)
    {
        if (tempIG->igOffs != currentOffset)
        {
            printf("Block #%u has offset %08X, expected %08X\n", tempIG->igNum, tempIG->igOffs, currentOffset);
            assert(!"bad block offset");
        }

        currentOffset += tempIG->igSize;
    }

    if (emitTotalCodeSize != 0 && emitTotalCodeSize != currentOffset)
    {
        printf("Total code size is %08X, expected %08X\n", emitTotalCodeSize, currentOffset);

        assert(!"bad total code size");
    }
}

#endif // DEBUG

/*****************************************************************************
 *
 *  Begin generating a method prolog.
 */

void emitter::emitBegProlog()
{
    assert(codeGen->generatingProlog);

#if !FEATURE_FIXED_OUT_ARGS
    // Don't measure stack depth inside the prolog, it's misleading.

    emitCntStackDepth = 0;

    assert(emitCurStackLvl == 0);
#endif

    emitNoGCIG     = true;
    emitForceNewIG = false;

    /* Switch to the pre-allocated prolog IG */

    emitGenIG(emitPrologIG);

    /* Nothing is live on entry to the prolog */

    // These were initialized to Empty at the start of compilation.
    VarSetOps::ClearD(emitComp, emitInitGCrefVars);
    VarSetOps::ClearD(emitComp, emitPrevGCrefVars);
    emitInitGCrefRegs = RBM_NONE;
    emitPrevGCrefRegs = RBM_NONE;
    emitInitByrefRegs = RBM_NONE;
    emitPrevByrefRegs = RBM_NONE;
}

/*****************************************************************************
 *
 *  Return the code offset of the current location in the prolog.
 */

unsigned emitter::emitGetPrologOffsetEstimate()
{
    /* For now only allow a single prolog ins group */

    assert(emitPrologIG);
    assert(emitPrologIG == emitCurIG);

    return emitCurIGsize;
}

/*****************************************************************************
 *
 *  Mark the code offset of the current location as the end of the prolog,
 *  so it can be used later to compute the actual size of the prolog.
 */

void emitter::emitMarkPrologEnd()
{
    assert(codeGen->generatingProlog);

    /* For now only allow a single prolog ins group */

    assert(emitPrologIG);
    assert(emitPrologIG == emitCurIG);

    emitPrologEndPos = emitCurOffset();
}

/*****************************************************************************
 *
 *  Finish generating a method prolog.
 */

void emitter::emitEndProlog()
{
    assert(codeGen->generatingProlog);

    emitNoGCIG = false;

    /* Save the prolog IG if non-empty or if only one block */

    if (emitCurIGnonEmpty() || emitCurIG == emitPrologIG)
    {
        emitSavIG();
    }

#if !FEATURE_FIXED_OUT_ARGS
    emitCurStackLvl   = 0;
    emitCntStackDepth = 4;
#endif
}

void emitter::emitCreatePlaceholderIG(insGroupPlaceholderType igType, BasicBlock* igBB)
{
    bool last;

#ifdef FEATURE_EH_FUNCLETS
    if (igType == IGPT_FUNCLET_PROLOG)
    {
        if (emitCurIGnonEmpty())
        {
            emitNxtIG(false);
        }

        VARSET_TP GCvars    = codeGen->liveness.GetGCLiveSet();
        regMaskTP gcrefRegs = codeGen->liveness.GetGCRegs(TYP_REF);
        regMaskTP byrefRegs = codeGen->liveness.GetGCRegs(TYP_BYREF);

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

        noway_assert((gcrefRegs & RBM_EXCEPTION_OBJECT) == gcrefRegs);
        noway_assert(byrefRegs == RBM_NONE);

        VarSetOps::Assign(emitComp, emitInitGCrefVars, GCvars);
        emitInitGCrefRegs = gcrefRegs;
        emitInitByrefRegs = byrefRegs;

        VarSetOps::Assign(emitComp, emitThisGCrefVars, GCvars);
        emitThisGCrefRegs = gcrefRegs;
        emitThisByrefRegs = byrefRegs;

        last = false;
    }
    else
    {
        assert((igType == IGPT_EPILOG) || (igType == IGPT_FUNCLET_EPILOG));
#else
    assert(igType == IGPT_EPILOG);
    {
#endif

#ifdef TARGET_AMD64
        emitOutputPreEpilogNOP();
#endif

        if (emitCurIGnonEmpty())
        {
            emitNxtIG(true);
        }

        last = igBB->bbNext == nullptr;
    }

    /* Convert the group to a placeholder group */

    insGroup* igPh = emitCurIG;

    igPh->igFlags |= IGF_PLACEHOLDER;

    /* Note that we might be re-using a previously created but empty IG. In this
     * case, we need to make sure any re-used fields, such as igFuncIdx, are correct.
     */

    igPh->igFuncIdx = emitComp->compCurrFuncIdx;

    /* Create a separate block of memory to store placeholder information.
     * We could use unions to put some of this into the insGroup itself, but we don't
     * want to grow the insGroup, and it's difficult to make sure the
     * insGroup fields are getting set and used elsewhere.
     */

    igPh->igPhData = new (emitComp, CMK_InstDesc) insPlaceholderGroupData;

    igPh->igPhData->igPhNext = nullptr;
    igPh->igPhData->igPhType = igType;
    igPh->igPhData->igPhBB   = igBB;

    igPh->igPhData->igPhPrevGCrefVars = VarSetOps::MakeCopy(emitComp, emitPrevGCrefVars);
    igPh->igPhData->igPhPrevGCrefRegs = emitPrevGCrefRegs;
    igPh->igPhData->igPhPrevByrefRegs = emitPrevByrefRegs;

    igPh->igPhData->igPhInitGCrefVars = VarSetOps::MakeCopy(emitComp, emitInitGCrefVars);
    igPh->igPhData->igPhInitGCrefRegs = emitInitGCrefRegs;
    igPh->igPhData->igPhInitByrefRegs = emitInitByrefRegs;

#if EMITTER_STATS
    emitTotalPhIGcnt += 1;
#endif

    // Mark function prologs and epilogs properly in the igFlags bits. These bits
    // will get used and propagated when the placeholder is converted to a non-placeholder
    // during prolog/epilog generation.

    if (igType == IGPT_EPILOG)
    {
        igPh->igFlags |= IGF_EPILOG;
    }
#if defined(FEATURE_EH_FUNCLETS)
    else if (igType == IGPT_FUNCLET_PROLOG)
    {
        igPh->igFlags |= IGF_FUNCLET_PROLOG;
    }
    else if (igType == IGPT_FUNCLET_EPILOG)
    {
        igPh->igFlags |= IGF_FUNCLET_EPILOG;
    }
#endif // FEATURE_EH_FUNCLETS

    /* Link it into the placeholder list */

    if (emitPlaceholderList)
    {
        emitPlaceholderLast->igPhData->igPhNext = igPh;
    }
    else
    {
        emitPlaceholderList = igPh;
    }

    emitPlaceholderLast = igPh;

    // Give an estimated size of this placeholder IG and
    // increment emitCurCodeOffset since we are not calling emitNewIG()
    //
    emitCurIGsize += MAX_PLACEHOLDER_IG_SIZE;
    emitCurCodeOffset += emitCurIGsize;

#if defined(FEATURE_EH_FUNCLETS)
    // Add the appropriate IP mapping debugging record for this placeholder
    // group. genExitCode() adds the mapping for main function epilogs.
    if (emitComp->opts.compDbgInfo)
    {
        if (igType == IGPT_FUNCLET_PROLOG)
        {
            codeGen->genIPmappingAdd((IL_OFFSETX)ICorDebugInfo::PROLOG, true);
        }
        else if (igType == IGPT_FUNCLET_EPILOG)
        {
            codeGen->genIPmappingAdd((IL_OFFSETX)ICorDebugInfo::EPILOG, true);
        }
    }
#endif // FEATURE_EH_FUNCLETS

    /* Start a new IG if more code follows */

    if (last)
    {
        emitCurIG = nullptr;
    }
    else
    {
        if (igType == IGPT_EPILOG
#if defined(FEATURE_EH_FUNCLETS)
            || igType == IGPT_FUNCLET_EPILOG
#endif // FEATURE_EH_FUNCLETS
            )
        {
            // If this was an epilog, then assume this is the end of any currently in progress
            // no-GC region. If a block after the epilog needs to be no-GC, it needs to call
            // emitter::emitDisableGC() directly. This behavior is depended upon by the fast
            // tailcall implementation, which disables GC at the beginning of argument setup,
            // but assumes that after the epilog it will be re-enabled.
            emitNoGCIG = false;
        }

        emitNewIG();

        // We don't know what the GC ref state will be at the end of the placeholder
        // group. So, force the next IG to store all the GC ref state variables;
        // don't omit them because emitPrev* is the same as emitInit*, because emitPrev*
        // will be inaccurate. (Note that, currently, GCrefRegs and ByrefRegs are always
        // saved anyway.)
        //
        // There is no need to re-initialize the emitPrev* variables, as they won't be used
        // with emitForceStoreGCState==true, and will be re-initialized just before
        // emitForceStoreGCState is set to false;

        emitForceStoreGCState = true;

        /* The group after the placeholder group doesn't get the "propagate" flags */

        emitCurIG->igFlags &= ~IGF_PROPAGATE_MASK;
    }

#ifdef DEBUG
    if (emitComp->verbose)
    {
        printf("*************** After placeholder IG creation\n");
        emitDispIGlist(false);
    }
#endif
}

/*****************************************************************************
 *
 *  Generate all prologs and epilogs
 */

void emitter::emitGeneratePrologEpilog()
{
#ifdef DEBUG
    unsigned prologCnt = 0;
    unsigned epilogCnt = 0;
#if defined(FEATURE_EH_FUNCLETS)
    unsigned funcletPrologCnt = 0;
    unsigned funcletEpilogCnt = 0;
#endif // FEATURE_EH_FUNCLETS
#endif // DEBUG

    insGroup* igPh;
    insGroup* igPhNext;

    // Generating the prolog/epilog is going to destroy the placeholder group,
    // so save the "next" pointer before that happens.

    for (igPh = emitPlaceholderList; igPh != nullptr; igPh = igPhNext)
    {
        assert(igPh->igFlags & IGF_PLACEHOLDER);

        igPhNext = igPh->igPhData->igPhNext;

        BasicBlock* igPhBB = igPh->igPhData->igPhBB;

        switch (igPh->igPhData->igPhType)
        {
            case IGPT_EPILOG:
                INDEBUG(++epilogCnt);
                emitBegFnEpilog(igPh);
                codeGen->genFnEpilog(igPhBB);
                emitEndFnEpilog();
                break;
#ifdef FEATURE_EH_FUNCLETS
            case IGPT_FUNCLET_PROLOG:
                INDEBUG(++funcletPrologCnt);
                emitBegFuncletProlog(igPh);
                codeGen->genFuncletProlog(igPhBB);
                emitEndFuncletProlog();
                break;
            case IGPT_FUNCLET_EPILOG:
                INDEBUG(++funcletEpilogCnt);
                emitBegFuncletEpilog(igPh);
                codeGen->genFuncletEpilog();
                emitEndFuncletEpilog();
                break;
#endif // FEATURE_EH_FUNCLETS
            default:
                unreached();
        }
    }

#ifdef DEBUG
    if (emitComp->verbose)
    {
        printf("%d prologs, %d epilogs", prologCnt, epilogCnt);
#if defined(FEATURE_EH_FUNCLETS)
        printf(", %d funclet prologs, %d funclet epilogs", funcletPrologCnt, funcletEpilogCnt);
#endif // FEATURE_EH_FUNCLETS
        printf("\n");

// prolog/epilog code doesn't use this yet
// noway_assert(prologCnt == 1);
// noway_assert(epilogCnt == emitEpilogCnt); // Is this correct?
#if defined(FEATURE_EH_FUNCLETS)
        assert(funcletPrologCnt == emitComp->ehFuncletCount());
#endif // FEATURE_EH_FUNCLETS
    }
#endif // DEBUG
}

/*****************************************************************************
 *
 *  Begin all prolog and epilog generation
 */

void emitter::emitStartPrologEpilogGeneration()
{
    /* Save the current IG if it's non-empty */

    if (emitCurIGnonEmpty())
    {
        emitSavIG();
    }
    else
    {
        assert(emitCurIG == nullptr);
    }
}

/*****************************************************************************
 *
 *  Finish all prolog and epilog generation
 */

void emitter::emitFinishPrologEpilogGeneration()
{
    /* Update the offsets of all the blocks */

    emitRecomputeIGoffsets();

    /* We should not generate any more code after this */

    emitCurIG = nullptr;
}

/*****************************************************************************
 *
 *  Common code for prolog / epilog beginning. Convert the placeholder group to actual code IG,
 *  and set it as the current group.
 */

void emitter::emitBegPrologEpilog(insGroup* igPh)
{
    assert(igPh->igFlags & IGF_PLACEHOLDER);

    /* Save the current IG if it's non-empty */

    if (emitCurIGnonEmpty())
    {
        emitSavIG();
    }

    /* Convert the placeholder group to a normal group.
     * We need to be very careful to re-initialize the IG properly.
     * It turns out, this means we only need to clear the placeholder bit
     * and clear the igPhData field, and emitGenIG() will do the rest,
     * since in the placeholder IG we didn't touch anything that is set by emitAllocIG().
     */

    igPh->igFlags &= ~IGF_PLACEHOLDER;
    emitNoGCIG     = true;
    emitForceNewIG = false;

    /* Set up the GC info that we stored in the placeholder */

    VarSetOps::Assign(emitComp, emitPrevGCrefVars, igPh->igPhData->igPhPrevGCrefVars);
    emitPrevGCrefRegs = igPh->igPhData->igPhPrevGCrefRegs;
    emitPrevByrefRegs = igPh->igPhData->igPhPrevByrefRegs;

    VarSetOps::Assign(emitComp, emitThisGCrefVars, igPh->igPhData->igPhInitGCrefVars);
    VarSetOps::Assign(emitComp, emitInitGCrefVars, igPh->igPhData->igPhInitGCrefVars);
    emitThisGCrefRegs = emitInitGCrefRegs = igPh->igPhData->igPhInitGCrefRegs;
    emitThisByrefRegs = emitInitByrefRegs = igPh->igPhData->igPhInitByrefRegs;

    igPh->igPhData = nullptr;

    /* Create a non-placeholder group pointer that we'll now use */

    insGroup* ig = igPh;

    /* Set the current function using the function index we stored */

    emitComp->funSetCurrentFunc(ig->igFuncIdx);

    /* Set the new IG as the place to generate code */

    emitGenIG(ig);

#if !FEATURE_FIXED_OUT_ARGS
    // Don't measure stack depth inside the prolog / epilog, it's misleading.

    emitCntStackDepth = 0;

    assert(emitCurStackLvl == 0);
#endif
}

/*****************************************************************************
 *
 *  Common code for end of prolog / epilog
 */

void emitter::emitEndPrologEpilog()
{
    emitNoGCIG = false;

    /* Save the IG if non-empty */

    if (emitCurIGnonEmpty())
    {
        emitSavIG();
    }

    assert(emitCurIGsize <= MAX_PLACEHOLDER_IG_SIZE);

#if !FEATURE_FIXED_OUT_ARGS
    // Reset the stack depth values.

    emitCurStackLvl   = 0;
    emitCntStackDepth = sizeof(int);
#endif
}

/*****************************************************************************
 *
 *  Begin generating a main function epilog.
 */

void emitter::emitBegFnEpilog(insGroup* igPh)
{
    emitEpilogCnt++;

    emitBegPrologEpilog(igPh);

#ifdef JIT32_GCENCODER

    EpilogList* el = new (emitComp, CMK_GC) EpilogList();

    if (emitEpilogLast != nullptr)
    {
        emitEpilogLast->elNext = el;
    }
    else
    {
        emitEpilogList = el;
    }

    emitEpilogLast = el;

#endif // JIT32_GCENCODER
}

/*****************************************************************************
 *
 *  Finish generating a funclet epilog.
 */

void emitter::emitEndFnEpilog()
{
    emitEndPrologEpilog();

#ifdef JIT32_GCENCODER
    assert(emitEpilogLast != nullptr);

    UNATIVE_OFFSET epilogBegCodeOffset          = emitEpilogLast->elLoc.CodeOffset(this);
    UNATIVE_OFFSET epilogExitSeqStartCodeOffset = emitExitSeqBegLoc.CodeOffset(this);
    UNATIVE_OFFSET newSize                      = epilogExitSeqStartCodeOffset - epilogBegCodeOffset;

    /* Compute total epilog size */
    assert(emitEpilogSize == 0 || emitEpilogSize == newSize); // All epilogs must be identical
    emitEpilogSize = newSize;

    UNATIVE_OFFSET epilogEndCodeOffset = emitCodeOffset(emitCurIG, emitCurOffset());
    assert(epilogExitSeqStartCodeOffset != epilogEndCodeOffset);

    newSize = epilogEndCodeOffset - epilogExitSeqStartCodeOffset;
    if (newSize < emitExitSeqSize)
    {
        // We expect either the epilog to be the same every time, or that
        // one will be a ret or a ret <n> and others will be a jmp addr or jmp [addr];
        // we make the epilogs the minimum of these.  Note that this ONLY works
        // because the only instruction is the last one and thus a slight
        // underestimation of the epilog size is harmless (since the EIP
        // can not be between instructions).
        assert(emitEpilogCnt == 1 ||
               (emitExitSeqSize - newSize) <= 5 // delta between size of various forms of jmp (size is either 6 or 5),
                                                // and various forms of ret (size is either 1 or 3). The combination can
                                                // be anything between 1 and 5.
               );
        emitExitSeqSize = newSize;
    }
#endif // JIT32_GCENCODER
}

#if defined(FEATURE_EH_FUNCLETS)

/*****************************************************************************
 *
 *  Begin generating a funclet prolog.
 */

void emitter::emitBegFuncletProlog(insGroup* igPh)
{
    emitBegPrologEpilog(igPh);
}

/*****************************************************************************
 *
 *  Finish generating a funclet prolog.
 */

void emitter::emitEndFuncletProlog()
{
    emitEndPrologEpilog();
}

/*****************************************************************************
 *
 *  Begin generating a funclet epilog.
 */

void emitter::emitBegFuncletEpilog(insGroup* igPh)
{
    emitBegPrologEpilog(igPh);
}

/*****************************************************************************
 *
 *  Finish generating a funclet epilog.
 */

void emitter::emitEndFuncletEpilog()
{
    emitEndPrologEpilog();
}

#endif // FEATURE_EH_FUNCLETS

#ifdef JIT32_GCENCODER

//
// emitter::emitStartEpilog:
//   Mark the current position so that we can later compute the total epilog size.
//
void emitter::emitStartEpilog()
{
    assert(emitEpilogLast != nullptr);
    emitEpilogLast->elLoc.CaptureLocation(this);
}

/*****************************************************************************
 *
 *  Return non-zero if the current method only has one epilog, which is
 *  at the very end of the method body.
 */

bool emitter::emitHasEpilogEnd()
{
    if (emitEpilogCnt == 1 && (emitIGlast->igFlags & IGF_EPILOG)) // This wouldn't work for funclets
        return true;
    else
        return false;
}

#endif // JIT32_GCENCODER

#ifdef TARGET_XARCH

/*****************************************************************************
 *
 *  Mark the beginning of the epilog exit sequence by remembering our position.
 */

void emitter::emitStartExitSeq()
{
    assert(codeGen->generatingEpilog);

    emitExitSeqBegLoc.CaptureLocation(this);
}

#endif // TARGET_XARCH

/*****************************************************************************
 *
 *  The code generator tells us the range of GC ref locals through this
 *  method. Needless to say, locals and temps should be allocated so that
 *  the size of the range is as small as possible.
 *
 * offsLo - The FP offset from which the GC pointer range starts.
 * offsHi - The FP offset at which the GC pointer region ends (exclusive).
 */

void emitter::emitSetFrameRangeGCRs(int offsLo, int offsHi)
{
    assert(offsHi > offsLo);
    assert(offsLo % REGSIZE_BYTES == 0);
    assert(offsHi % REGSIZE_BYTES == 0);

    //  A total of    47254 methods compiled.
    //
    //  GC ref frame variable counts:
    //
    //      <=         0 ===>  43175 count ( 91% of total)
    //       1 ..      1 ===>   2367 count ( 96% of total)
    //       2 ..      2 ===>    887 count ( 98% of total)
    //       3 ..      5 ===>    579 count ( 99% of total)
    //       6 ..     10 ===>    141 count ( 99% of total)
    //      11 ..     20 ===>     40 count ( 99% of total)
    //      21 ..     50 ===>     42 count ( 99% of total)
    //      51 ..    128 ===>     15 count ( 99% of total)
    //     129 ..    256 ===>      4 count ( 99% of total)
    //     257 ..    512 ===>      4 count (100% of total)
    //     513 ..   1024 ===>      0 count (100% of total)

    emitGCrFrameOffsMin = offsLo;
    emitGCrFrameOffsMax = offsHi;
    emitGCrFrameOffsCnt = (offsHi - offsLo) / REGSIZE_BYTES;
}

/*****************************************************************************
 *
 *  A conversion table used to map an operand size value (in bytes) into its
 *  small encoding (0 through 3), and vice versa.
 */

const emitter::opSize emitter::emitSizeEncode[] = {
    emitter::OPSZ1, emitter::OPSZ2,  OPSIZE_INVALID, emitter::OPSZ4,  OPSIZE_INVALID, OPSIZE_INVALID, OPSIZE_INVALID,
    emitter::OPSZ8, OPSIZE_INVALID,  OPSIZE_INVALID, OPSIZE_INVALID,  OPSIZE_INVALID, OPSIZE_INVALID, OPSIZE_INVALID,
    OPSIZE_INVALID, emitter::OPSZ16, OPSIZE_INVALID, OPSIZE_INVALID,  OPSIZE_INVALID, OPSIZE_INVALID, OPSIZE_INVALID,
    OPSIZE_INVALID, OPSIZE_INVALID,  OPSIZE_INVALID, OPSIZE_INVALID,  OPSIZE_INVALID, OPSIZE_INVALID, OPSIZE_INVALID,
    OPSIZE_INVALID, OPSIZE_INVALID,  OPSIZE_INVALID, emitter::OPSZ32,
};

const emitAttr emitter::emitSizeDecode[emitter::OPSZ_COUNT] = {EA_1BYTE, EA_2BYTE,  EA_4BYTE,
                                                               EA_8BYTE, EA_16BYTE, EA_32BYTE};

// Returns true if garbage collection won't happen within the helper call.
// There is no need to record live pointers for such call sites.
bool emitter::emitNoGChelper(CorInfoHelpFunc helpFunc)
{
    switch (helpFunc)
    {
        case CORINFO_HELP_UNDEF:
            return false;

        case CORINFO_HELP_PROF_FCN_LEAVE:
        case CORINFO_HELP_PROF_FCN_ENTER:
        case CORINFO_HELP_PROF_FCN_TAILCALL:
        case CORINFO_HELP_LLSH:
        case CORINFO_HELP_LRSH:
        case CORINFO_HELP_LRSZ:

//  case CORINFO_HELP_LMUL:
//  case CORINFO_HELP_LDIV:
//  case CORINFO_HELP_LMOD:
//  case CORINFO_HELP_ULDIV:
//  case CORINFO_HELP_ULMOD:

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
#endif

        case CORINFO_HELP_ASSIGN_REF:
        case CORINFO_HELP_CHECKED_ASSIGN_REF:
        case CORINFO_HELP_ASSIGN_BYREF:

        case CORINFO_HELP_GETSHARED_GCSTATIC_BASE_NOCTOR:
        case CORINFO_HELP_GETSHARED_NONGCSTATIC_BASE_NOCTOR:

        case CORINFO_HELP_INIT_PINVOKE_FRAME:
            return true;

        default:
            return false;
    }
}

bool emitter::emitNoGChelper(CORINFO_METHOD_HANDLE methHnd)
{
    CorInfoHelpFunc helpFunc = Compiler::eeGetHelperNum(methHnd);
    if (helpFunc == CORINFO_HELP_UNDEF)
    {
        return false;
    }
    return emitNoGChelper(helpFunc);
}

/*****************************************************************************
 *
 *  Mark the current spot as having a label.
 */

void* emitter::emitAddLabel(bool isFinallyTarget DEBUG_ARG(BasicBlock* block))
{
    /* Create a new IG if the current one is non-empty */

    if (emitCurIGnonEmpty())
    {
        emitNxtIG(false);
    }
#if defined(DEBUG) || defined(LATE_DISASM)
    else
    {
        emitCurIG->igWeight    = getCurrentBlockWeight();
        emitCurIG->igPerfScore = 0.0;
    }
#endif

#if defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
    if (isFinallyTarget)
    {
        emitCurIG->igFlags |= IGF_FINALLY_TARGET;
    }
#endif

    VARSET_TP GCvars    = codeGen->liveness.GetGCLiveSet();
    regMaskTP gcrefRegs = codeGen->liveness.GetGCRegs(TYP_REF);
    regMaskTP byrefRegs = codeGen->liveness.GetGCRegs(TYP_BYREF);

    VarSetOps::Assign(emitComp, emitInitGCrefVars, GCvars);
    emitInitGCrefRegs = gcrefRegs;
    emitInitByrefRegs = byrefRegs;

    VarSetOps::Assign(emitComp, emitThisGCrefVars, GCvars);
    emitThisGCrefRegs = gcrefRegs;
    emitThisByrefRegs = byrefRegs;

#ifdef DEBUG
    JITDUMP("Mapped " FMT_BB " to %s\n", block->bbNum, emitLabelString(emitCurIG));

    if (EMIT_GC_VERBOSE)
    {
        printf("Label: IG%02u, GCvars ", emitCurIG->igNum);
        dumpConvertedVarSet(emitComp, GCvars);
        printf(", gcrefRegs");
        emitDispRegSet(gcrefRegs);
        printf(", byrefRegs");
        emitDispRegSet(byrefRegs);
        printf("\n");
    }
#endif

    return emitCurIG;
}

void* emitter::emitAddInlineLabel()
{
    if (emitCurIGnonEmpty())
    {
        emitNxtIG(true);
    }

    return emitCurIG;
}

#ifdef DEBUG

//-----------------------------------------------------------------------------
// emitPrintLabel: Print the assembly label for an insGroup. We could use emitter::emitLabelString()
// to be consistent, but that seems silly.
//
void emitter::emitPrintLabel(insGroup* ig)
{
    printf("G_M%03u_IG%02u", emitComp->compMethodID, ig->igNum);
}

//-----------------------------------------------------------------------------
// emitLabelString: Return label string for an insGroup, for use in debug output.
// This can be called up to four times in a single 'printf' before the static buffers
// get reused.
//
// Returns:
//    String with insGroup label
//
const char* emitter::emitLabelString(insGroup* ig)
{
    const int       TEMP_BUFFER_LEN = 40;
    static unsigned curBuf          = 0;
    static char     buf[4][TEMP_BUFFER_LEN];
    const char*     retbuf;

    sprintf_s(buf[curBuf], TEMP_BUFFER_LEN, "G_M%03u_IG%02u", emitComp->compMethodID, ig->igNum);
    retbuf = buf[curBuf];
    curBuf = (curBuf + 1) % 4;
    return retbuf;
}

#endif // DEBUG

#ifdef TARGET_ARMARCH

// Does the argument location point to an IG at the end of a function or funclet?
// We can ignore the codePos part of the location, since it doesn't affect the
// determination. If 'emitLocNextFragment' is non-NULL, it indicates the first
// IG of the next fragment, so it represents a function end.
bool emitter::emitIsFuncEnd(emitLocation* emitLoc, emitLocation* emitLocNextFragment /* = NULL */)
{
    assert(emitLoc);

    insGroup* ig = emitLoc->GetIG();
    assert(ig);

    // Are we at the end of the IG list?
    if ((emitLocNextFragment != NULL) && (ig->igNext == emitLocNextFragment->GetIG()))
        return true;

    // Safety check
    if (ig->igNext == NULL)
        return true;

    // Is the next IG the start of a funclet prolog?
    if (ig->igNext->igFlags & IGF_FUNCLET_PROLOG)
        return true;

#if defined(FEATURE_EH_FUNCLETS)

    // Is the next IG a placeholder group for a funclet prolog?
    if ((ig->igNext->igFlags & IGF_PLACEHOLDER) && (ig->igNext->igPhData->igPhType == IGPT_FUNCLET_PROLOG))
    {
        return true;
    }

#endif // FEATURE_EH_FUNCLETS

    return false;
}

/*****************************************************************************
 *
 * Split the region from 'startLoc' to 'endLoc' into fragments by calling
 * a callback function to indicate the beginning of a fragment. The initial code,
 * starting at 'startLoc', doesn't get a callback, but the first code fragment,
 * about 'maxSplitSize' bytes out does, as does the beginning of each fragment
 * after that. There is no callback for the end (only the beginning of the last
 * fragment gets a callback). A fragment must contain at least one instruction
 * group. It should be smaller than 'maxSplitSize', although it may be larger to
 * satisfy the "at least one instruction group" rule. Do not split prologs or
 * epilogs. (Currently, prologs exist in a single instruction group at the main
 * function beginning, so they aren't split. Funclets, however, might span IGs,
 * so we can't split in between them.)
 *
 * Note that the locations must be the start of instruction groups; the part of
 * the location indicating offset within a group must be zero.
 *
 * If 'startLoc' is NULL, it means the start of the code.
 * If 'endLoc'   is NULL, it means the end   of the code.
 */

void emitter::emitSplit(emitLocation*         startLoc,
                        emitLocation*         endLoc,
                        UNATIVE_OFFSET        maxSplitSize,
                        void*                 context,
                        emitSplitCallbackType callbackFunc)
{
    insGroup*      igStart = (startLoc == NULL) ? emitIGlist : startLoc->GetIG();
    insGroup*      igEnd   = (endLoc == NULL) ? NULL : endLoc->GetIG();
    insGroup*      igPrev;
    insGroup*      ig;
    insGroup*      igLastReported;
    insGroup*      igLastCandidate;
    UNATIVE_OFFSET curSize;
    UNATIVE_OFFSET candidateSize;

    for (igPrev = NULL, ig = igLastReported = igStart, igLastCandidate = NULL, candidateSize = 0, curSize = 0;
         ig != igEnd && ig != NULL; igPrev = ig, ig = ig->igNext)
    {
        // Keep looking until we've gone past the maximum split size
        if (curSize >= maxSplitSize)
        {
            bool reportCandidate = true;

            // Is there a candidate?
            if (igLastCandidate == NULL)
            {
#ifdef DEBUG
                if (EMITVERBOSE)
                    printf("emitSplit: can't split at IG%02u; we don't have a candidate to report\n", ig->igNum);
#endif
                reportCandidate = false;
            }

            // Don't report the same thing twice (this also happens for the first block, since igLastReported is
            // initialized to igStart).
            if (igLastCandidate == igLastReported)
            {
#ifdef DEBUG
                if (EMITVERBOSE)
                    printf("emitSplit: can't split at IG%02u; we already reported it\n", igLastCandidate->igNum);
#endif
                reportCandidate = false;
            }

            // Report it!
            if (reportCandidate)
            {
#ifdef DEBUG
                if (EMITVERBOSE && (candidateSize >= maxSplitSize))
                    printf("emitSplit: split at IG%02u is size %d, larger than requested maximum size of %d\n",
                           igLastCandidate->igNum, candidateSize, maxSplitSize);
#endif

                // hand memory ownership to the callback function
                emitLocation* pEmitLoc = new (emitComp, CMK_Unknown) emitLocation(igLastCandidate);
                callbackFunc(context, pEmitLoc);
                igLastReported  = igLastCandidate;
                igLastCandidate = NULL;
                curSize -= candidateSize;
            }
        }

        // Update the current candidate to be this block, if it isn't in the middle of a
        // prolog or epilog, which we can't split. All we know is that certain
        // IGs are marked as prolog or epilog. We don't actually know if two adjacent
        // IGs are part of the *same* prolog or epilog, so we have to assume they are.

        if (igPrev && (((igPrev->igFlags & IGF_FUNCLET_PROLOG) && (ig->igFlags & IGF_FUNCLET_PROLOG)) ||
                       ((igPrev->igFlags & IGF_EPILOG) && (ig->igFlags & IGF_EPILOG))))
        {
            // We can't update the candidate
        }
        else
        {
            igLastCandidate = ig;
            candidateSize   = curSize;
        }

        curSize += ig->igSize;

    } // end for loop
}

/*****************************************************************************
 *
 * Given an instruction group, find the array of instructions (instrDesc) and
 * number of instructions in the array. If the IG is the current IG, we assume
 * that igData does NOT hold the instructions; they are unsaved and pointed
 * to by emitCurIGfreeBase.
 *
 * This function can't be called for placeholder groups, which have no instrDescs.
 */

void emitter::emitGetInstrDescs(insGroup* ig, instrDesc** id, int* insCnt)
{
    assert(!(ig->igFlags & IGF_PLACEHOLDER));
    if (ig == emitCurIG)
    {
        *id     = (instrDesc*)emitCurIGfreeBase;
        *insCnt = emitCurIGinsCnt;
    }
    else
    {
        *id     = (instrDesc*)ig->igData;
        *insCnt = ig->igInsCnt;
    }

    assert(*id);
}

/*****************************************************************************
 *
 * Given a location (an 'emitLocation'), find the instruction group (IG) and
 * instruction descriptor (instrDesc) corresponding to that location. Returns
 * 'true' if there is an instruction, 'false' if there is no instruction
 * (i.e., we're at the end of the instruction list). Also, optionally return
 * the number of instructions that follow that instruction in the IG (in *pinsRemaining,
 * if pinsRemaining is non-NULL), which can be used for iterating over the
 * remaining instrDescs in the IG.
 *
 * We assume that emitCurIG points to the end of the instructions we care about.
 * For the prologs or epilogs, it points to the last IG of the prolog or epilog
 * that is being generated. For body code gen, it points to the place we are currently
 * adding code, namely, the end of currently generated code.
 */

bool emitter::emitGetLocationInfo(emitLocation* emitLoc,
                                  insGroup**    pig,
                                  instrDesc**   pid,
                                  int*          pinsRemaining /* = NULL */)
{
    assert(emitLoc != nullptr);
    assert(emitLoc->Valid());
    assert(emitLoc->GetIG() != nullptr);
    assert(pig != nullptr);
    assert(pid != nullptr);

    insGroup*  ig = emitLoc->GetIG();
    instrDesc* id;
    int        insNum = emitLoc->GetInsNum();
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

        if (ig == NULL)
        {
            // 'ig' can't be NULL, or we went past the current IG represented by 'emitCurIG'.
            // Perhaps 'loc' was corrupt coming in?
            noway_assert(!"corrupt emitter location");
            return false;
        }
    }

    // Now find the instrDesc within this group that corresponds to the location

    assert(insNum < insCnt);

    int i;
    for (i = 0; i != insNum; ++i)
    {
        castto(id, BYTE*) += emitSizeOfInsDsc(id);
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

/*****************************************************************************
 *
 * Compute the next instrDesc, either in this IG, or in a subsequent IG. 'id'
 * will point to this instrDesc. 'ig' and 'insRemaining' will also be updated.
 * Returns true if there is an instruction, or false if we've iterated over all
 * the instructions up to the current instruction (based on 'emitCurIG').
 */

bool emitter::emitNextID(insGroup*& ig, instrDesc*& id, int& insRemaining)
{
    if (insRemaining > 0)
    {
        castto(id, BYTE*) += emitSizeOfInsDsc(id);
        --insRemaining;
        return true;
    }

    // We're out of instrDesc in 'ig'. Is this the current IG? If so, we're done.

    if (ig == emitCurIG)
    {
        return false;
    }

    for (ig = ig->igNext; ig; ig = ig->igNext)
    {
        int insCnt;
        emitGetInstrDescs(ig, &id, &insCnt);

        if (insCnt > 0)
        {
            insRemaining = insCnt - 1;
            return true;
        }

        if (ig == emitCurIG)
        {
            return false;
        }
    }

    return false;
}

/*****************************************************************************
 *
 * Walk instrDesc's from the location given by 'locFrom', up to the current location.
 * For each instruction, call the callback function 'processFunc'. 'context' is simply
 * passed through to the callback function.
 */

void emitter::emitWalkIDs(emitLocation* locFrom, emitProcessInstrFunc_t processFunc, void* context)
{
    insGroup*  ig;
    instrDesc* id;
    int        insRemaining;

    if (!emitGetLocationInfo(locFrom, &ig, &id, &insRemaining))
        return; // no instructions at the 'from' location

    do
    {
        // process <<id>>
        (*processFunc)(id, context);

    } while (emitNextID(ig, id, insRemaining));
}

/*****************************************************************************
 *
 * A callback function for emitWalkIDs() that calls Compiler::unwindNop().
 */

void emitter::emitGenerateUnwindNop(instrDesc* id, void* context)
{
    Compiler* comp = (Compiler*)context;
#if defined(TARGET_ARM)
    comp->unwindNop(id->idCodeSize());
#elif defined(TARGET_ARM64)
    comp->unwindNop();
#endif // defined(TARGET_ARM64)
}

/*****************************************************************************
 *
 * emitUnwindNopPadding: call unwindNop() for every instruction from a given
 * location 'emitLoc' up to the current location.
 */

void emitter::emitUnwindNopPadding(emitLocation* locFrom, Compiler* comp)
{
    emitWalkIDs(locFrom, emitGenerateUnwindNop, comp);
}

#endif // TARGET_ARMARCH

#if defined(TARGET_ARM)

/*****************************************************************************
 *
 * Return the instruction size in bytes for the instruction at the specified location.
 * This is used to assert that the unwind code being generated on ARM has the
 * same size as the instruction for which it is being generated (since on ARM
 * the unwind codes have a one-to-one relationship with instructions, and the
 * unwind codes have an implicit instruction size that must match the instruction size.)
 * An instruction must exist at the specified location.
 */

unsigned emitter::emitGetInstructionSize(emitLocation* emitLoc)
{
    insGroup*  ig;
    instrDesc* id;

    bool anyInstrs = emitGetLocationInfo(emitLoc, &ig, &id);
    assert(anyInstrs); // There better be an instruction at this location (otherwise, we're at the end of the
                       // instruction list)
    return id->idCodeSize();
}

#endif // defined(TARGET_ARM)

/*****************************************************************************/
#ifdef DEBUG
/*****************************************************************************
 *
 *  Returns the name for the register to use to access frame based variables
 */

const char* emitter::emitGetFrameReg()
{
    if (codeGen->isFramePointerUsed())
    {
        return STR_FPBASE;
    }
    else
    {
        return STR_SPBASE;
    }
}

/*****************************************************************************
 *
 *  Display a register set in a readable form.
 */

void emitter::emitDispRegSet(regMaskTP regs)
{
    regNumber reg;
    bool      sp = false;

    printf(" {");

    for (reg = REG_FIRST; reg < ACTUAL_REG_COUNT; reg = REG_NEXT(reg))
    {
        if ((regs & genRegMask(reg)) == 0)
        {
            continue;
        }

        if (sp)
        {
            printf(" ");
        }
        else
        {
            sp = true;
        }

        printf("%s", emitRegName(reg));
    }

    printf("}");
}

void emitter::emitDispRegSetDiff(const char* name, regMaskTP from, regMaskTP to)
{
    printf("%s{ ", name);

    for (regNumber reg = REG_FIRST; reg < ACTUAL_REG_COUNT; reg = REG_NEXT(reg))
    {
        regMaskTP mask    = genRegMask(reg);
        bool      fromBit = (from & mask) != 0;
        bool      toBit   = (to & mask) != 0;

        if (!fromBit && !toBit)
        {
            continue;
        }

        const char* s = "";

        if (fromBit != toBit)
        {
            s = toBit ? "+" : "-";
        }

        printf("%s%s ", s, emitRegName(reg));
    }

    printf("}\n");
}

/*****************************************************************************
 *
 *  Display the current GC ref variable set in a readable form.
 */

void emitter::emitDispVarSet()
{
    unsigned vn;
    int      of;
    bool     sp = false;

    for (vn = 0, of = emitGCrFrameOffsMin; vn < emitGCrFrameOffsCnt; vn += 1, of += TARGET_POINTER_SIZE)
    {
        if (emitGCrFrameLiveTab[vn])
        {
            if (sp)
            {
                printf(" ");
            }
            else
            {
                sp = true;
            }

            printf("[%s", emitGetFrameReg());

            if (of < 0)
            {
                printf("-%02XH", -of);
            }
            else if (of > 0)
            {
                printf("+%02XH", +of);
            }

            printf("]");
        }
    }

    if (!sp)
    {
        printf("none");
    }
}

/*****************************************************************************/
#endif // DEBUG

#if MULTIREG_HAS_SECOND_GC_RET
//------------------------------------------------------------------------
// emitSetSecondRetRegGCType: Sets the GC type of the second return register for instrDescCGCA struct.
//
// Arguments:
//    id            - The large call instr descriptor to set the second GC return register type on.
//    secondRetSize - The EA_SIZE for second return register type.
//
// Return Value:
//    None
//

void emitter::emitSetSecondRetRegGCType(instrDescCGCA* id, emitAttr secondRetSize)
{
    if (EA_IS_GCREF(secondRetSize))
    {
        id->idSecondGCref(GCT_GCREF);
    }
    else if (EA_IS_BYREF(secondRetSize))
    {
        id->idSecondGCref(GCT_BYREF);
    }
    else
    {
        id->idSecondGCref(GCT_NONE);
    }
}
#endif // MULTIREG_HAS_SECOND_GC_RET

emitter::instrDesc* emitter::emitNewInstrCall(CORINFO_METHOD_HANDLE methodHandle,
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
    VARSET_VALARG_TP GCvars    = codeGen->liveness.GetGCLiveSet();
    regMaskTP        gcrefRegs = codeGen->liveness.GetGCRegs(TYP_REF);
    regMaskTP        byrefRegs = codeGen->liveness.GetGCRegs(TYP_BYREF);

    regMaskTP savedSet = emitGetGCRegsSavedOrModified(methodHandle);
    gcrefRegs &= savedSet;
    byrefRegs &= savedSet;

#ifdef DEBUG
    if (EMIT_GC_VERBOSE)
    {
        printf("Call: GCvars ");
        dumpConvertedVarSet(emitComp, GCvars);
        printf(", gcrefRegs");
        emitDispRegSet(gcrefRegs);
        printf(", byrefRegs");
        emitDispRegSet(byrefRegs);
        printf("\n");
    }
#endif

    if (retRegAttr == EA_UNKNOWN)
    {
        retRegAttr = EA_PTRSIZE;
    }

    instrDesc* id;

    if (!VarSetOps::IsEmpty(emitComp, GCvars) || ((gcrefRegs & RBM_CALLEE_TRASH) != RBM_NONE) || (byrefRegs != RBM_NONE)
#if MULTIREG_HAS_SECOND_GC_RET
        || EA_IS_GCREF_OR_BYREF(retReg2Attr)
#endif
#ifdef TARGET_X86
        || (argSlotCount > ID_MAX_SMALL_CNS) || (argSlotCount < 0)
#endif
#ifdef TARGET_XARCH
        || (disp < AM_DISP_MIN) || (disp > AM_DISP_MAX)
#endif
            )
    {
        instrDescCGCA* idc = emitAllocInstrCGCA(retRegAttr);
        idc->idSetIsLargeCall();
        idc->idcGCvars    = VarSetOps::MakeCopy(emitComp, GCvars);
        idc->idcGcrefRegs = gcrefRegs;
        idc->idcByrefRegs = byrefRegs;
#ifdef TARGET_XARCH
        idc->idcDisp = disp;
#endif
#ifdef TARGET_X86
        idc->idcArgCnt = argSlotCount;
#endif
#if MULTIREG_HAS_SECOND_GC_RET
        emitSetSecondRetRegGCType(idc, retReg2Attr);
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
        id = emitNewInstrCns(retRegAttr, argSlotCount);
#else
        id                   = emitAllocInstr(retRegAttr);
#endif

        emitEncodeCallGCregs(gcrefRegs, id);

#ifdef TARGET_XARCH
        id->idAddr()->iiaAddrMode.amDisp = disp;
        assert(id->idAddr()->iiaAddrMode.amDisp == disp);
#endif
    }

    VarSetOps::Assign(emitComp, emitThisGCrefVars, GCvars);
    emitThisGCrefRegs = gcrefRegs;
    emitThisByrefRegs = byrefRegs;

    id->idSetIsNoGC(emitNoGChelper(methodHandle));

    return id;
}

ID_OPS emitter::GetFormatOp(insFormat format)
{
    static const ID_OPS ops[]{
#define IF_DEF(en, op1, op2) ID_OP_##op2,
#include "emitfmts.h"
    };

    assert(format < _countof(ops));
    return ops[format];
}

//------------------------------------------------------------------------
// Interleaved GC info dumping.
// We'll attempt to line this up with the opcode, which indented differently for
// diffable and non-diffable dumps.
// This is approximate, and is better tuned for disassembly than for jitdumps.
// See emitDispInsHex().
#ifdef TARGET_AMD64
const size_t basicIndent     = 7;
const size_t hexEncodingSize = 21;
#elif defined(TARGET_X86)
const size_t basicIndent     = 7;
const size_t hexEncodingSize = 13;
#elif defined(TARGET_ARM64)
const size_t basicIndent     = 12;
const size_t hexEncodingSize = 19;
#elif defined(TARGET_ARM)
const size_t basicIndent     = 12;
const size_t hexEncodingSize = 11;
#endif

#ifdef DEBUG
//------------------------------------------------------------------------
// emitDispGCDeltaTitle: Print an appropriately indented title for a GC info delta
//
// Arguments:
//    title - The type of GC info delta we're printing
//
void emitter::emitDispGCDeltaTitle(const char* title)
{
    size_t indent = emitComp->opts.disDiffable ? basicIndent : basicIndent + hexEncodingSize;
    printf("%.*s; %s", indent, "                             ", title);
}

//------------------------------------------------------------------------
// emitDispGCRegDelta: Print a delta for GC registers
//
// Arguments:
//    title    - The type of GC info delta we're printing
//    prevRegs - The live GC registers before the recent instruction.
//    curRegs  - The live GC registers after the recent instruction.
//
void emitter::emitDispGCRegDelta(const char* title, regMaskTP prevRegs, regMaskTP curRegs)
{
    if (prevRegs != curRegs)
    {
        emitDispGCDeltaTitle(title);
        regMaskTP sameRegs    = prevRegs & curRegs;
        regMaskTP removedRegs = prevRegs - sameRegs;
        regMaskTP addedRegs   = curRegs - sameRegs;
        if (removedRegs != RBM_NONE)
        {
            printf(" -");
            dspRegMask(removedRegs);
        }
        if (addedRegs != RBM_NONE)
        {
            printf(" +");
            dspRegMask(addedRegs);
        }
        printf("\n");
    }
}

void emitter::emitDispGCVarDelta()
{
    if (!debugGCSlotChanges.Empty())
    {
        emitDispGCDeltaTitle("GC slots:");

        while (!debugGCSlotChanges.Empty())
        {
            GCStackSlotLifetime* lifetime = debugGCSlotChanges.Pop();
            int                  offset   = lifetime->slotOffset & ~OFFSET_MASK;

#ifdef TARGET_ARMARCH
            printf(" %c[%s,#%d]", lifetime->endCodeOffs == 0 ? '+' : '-', emitGetFrameReg(), offset);
#else
            printf(" %c[%s%c%02XH]", lifetime->endCodeOffs == 0 ? '+' : '-', emitGetFrameReg(), offset < 0 ? '-' : '+',
                   abs(offset));
#endif
        }

        printf("\n");
    }
}

void emitter::emitDispRegPtrListDelta()
{
    // Dump any deltas for outgoing args; these aren't captured in the other sets.
    if (debugPrevRegPtrDsc == gcInfo.GetLastRegArgChange())
    {
        return;
    }

    for (GCRegArgChange* dsc = (debugPrevRegPtrDsc == nullptr) ? gcInfo.GetFirstRegArgChange()
                                                               : debugPrevRegPtrDsc->next;
         dsc != nullptr; dsc = dsc->next)
    {
        // Reg changes are reflected in the register sets debugPrevGCrefRegs/emitThisGCrefRegs
        // and debugPrevByrefRegs/emitThisByrefRegs, and dumped using those sets.
        if (dsc->kind == GCInfo::RegArgChangeKind::RegChange)
        {
            continue;
        }

        emitDispGCDeltaTitle(GCtypeStr(dsc->gcType));

        switch (dsc->kind)
        {
#if FEATURE_FIXED_OUT_ARGS
            case GCInfo::RegArgChangeKind::StoreArg:
                printf(" arg store %d", dsc->argOffset);
                break;
            case GCInfo::RegArgChangeKind::KillArgs:
                printf(" args kill");
                break;
#else
            case GCInfo::RegArgChangeKind::PushArg:
                printf(" arg push %u", dsc->argOffset);
                break;
            case GCInfo::RegArgChangeKind::PopArgs:
                printf(" arg pop %u", dsc->argOffset);
                break;
            case GCInfo::RegArgChangeKind::KillArgs:
                printf(" arg kill %u", dsc->argOffset);
                break;
#endif
            default:
                printf(" arg ???");
                break;
        }

        printf("\n");
    }

    debugPrevRegPtrDsc = gcInfo.GetLastRegArgChange();
}

//------------------------------------------------------------------------
// emitDispGCInfoDelta: Print a delta for GC info
//
void emitter::emitDispGCInfoDelta()
{
    emitDispGCRegDelta("gcrRegs", debugPrevGCrefRegs, emitThisGCrefRegs);
    emitDispGCRegDelta("byrRegs", debugPrevByrefRegs, emitThisByrefRegs);
    debugPrevGCrefRegs = emitThisGCrefRegs;
    debugPrevByrefRegs = emitThisByrefRegs;
    emitDispGCVarDelta();
    emitDispRegPtrListDelta();
}

/*****************************************************************************
 *
 *  Display the current instruction group list.
 */

void emitter::emitDispIGflags(unsigned flags)
{
    if (flags & IGF_GC_VARS)
    {
        printf(", gcvars");
    }
    if (flags & IGF_BYREF_REGS)
    {
        printf(", byref");
    }
#if defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
    if (flags & IGF_FINALLY_TARGET)
    {
        printf(", ftarget");
    }
#endif // defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
    if (flags & IGF_FUNCLET_PROLOG)
    {
        printf(", funclet prolog");
    }
    if (flags & IGF_FUNCLET_EPILOG)
    {
        printf(", funclet epilog");
    }
    if (flags & IGF_EPILOG)
    {
        printf(", epilog");
    }
    if (flags & IGF_NOGCINTERRUPT)
    {
        printf(", nogc");
    }
    if (flags & IGF_UPD_ISZ)
    {
        printf(", isz");
    }
    if (flags & IGF_EXTEND)
    {
        printf(", extend");
    }
    if (flags & IGF_LOOP_ALIGN)
    {
        printf(", align");
    }
}

void emitter::emitDispIG(insGroup* ig, insGroup* igPrev, bool verbose)
{
    const int TEMP_BUFFER_LEN = 40;
    char      buff[TEMP_BUFFER_LEN];

    sprintf_s(buff, TEMP_BUFFER_LEN, "%s:        ", emitLabelString(ig));
    printf("%s; ", buff);

    // We dump less information when we're only interleaving GC info with a disassembly listing,
    // than we do in the jitdump case. (Note that the verbose argument to this method is
    // distinct from the verbose on Compiler.)
    bool jitdump = emitComp->verbose;

    if (jitdump && ((igPrev == nullptr) || (igPrev->igFuncIdx != ig->igFuncIdx)))
    {
        printf("func=%02u, ", ig->igFuncIdx);
    }

    if (ig->igFlags & IGF_PLACEHOLDER)
    {
        insGroup* igPh = ig;

        const char* pszType;
        switch (igPh->igPhData->igPhType)
        {
            case IGPT_EPILOG:
                pszType = "epilog";
                break;
#ifdef FEATURE_EH_FUNCLETS
            case IGPT_FUNCLET_PROLOG:
                pszType = "funclet prolog";
                break;
            case IGPT_FUNCLET_EPILOG:
                pszType = "funclet epilog";
                break;
#endif
            default:
                pszType = "UNKNOWN";
                break;
        }
        printf("%s placeholder, next placeholder=", pszType);
        if (igPh->igPhData->igPhNext)
        {
            printf("IG%02u ", igPh->igPhData->igPhNext->igNum);
        }
        else
        {
            printf("<END>");
        }

        if (igPh->igPhData->igPhBB != nullptr)
        {
            printf(", %s", igPh->igPhData->igPhBB->dspToString());
        }

        emitDispIGflags(igPh->igFlags);

        if (ig == emitCurIG)
        {
            printf(" <-- Current IG");
        }
        if (igPh == emitPlaceholderList)
        {
            printf(" <-- First placeholder");
        }
        if (igPh == emitPlaceholderLast)
        {
            printf(" <-- Last placeholder");
        }
        printf("\n");

        printf("%*s;   PrevGCVars ", strlen(buff), "");
        dumpConvertedVarSet(emitComp, igPh->igPhData->igPhPrevGCrefVars);
        printf(", PrevGCrefRegs");
        emitDispRegSet(igPh->igPhData->igPhPrevGCrefRegs);
        printf(", PrevByrefRegs");
        emitDispRegSet(igPh->igPhData->igPhPrevByrefRegs);
        printf("\n");

        printf("%*s;   InitGCVars ", strlen(buff), "");
        dumpConvertedVarSet(emitComp, igPh->igPhData->igPhInitGCrefVars);
        printf(", InitGCrefRegs");
        emitDispRegSet(igPh->igPhData->igPhInitGCrefRegs);
        printf(", InitByrefRegs");
        emitDispRegSet(igPh->igPhData->igPhInitByrefRegs);
        printf("\n");

        assert(!(ig->igFlags & IGF_GC_VARS));
        assert(!(ig->igFlags & IGF_BYREF_REGS));
    }
    else
    {
        const char* separator = "";

        if (jitdump)
        {
            printf("%soffs=%06XH, size=%04XH", separator, ig->igOffs, ig->igSize);
            separator = ", ";
        }

        if (emitComp->compCodeGenDone)
        {
            printf("%sbbWeight=%s PerfScore %.2f", separator, refCntWtd2str(ig->igWeight), ig->igPerfScore);
            separator = ", ";
        }

        if (ig->igFlags & IGF_GC_VARS)
        {
            printf("%sgcVars ", separator);
            dumpConvertedVarSet(emitComp, ig->igGCvars());
            separator = ", ";
        }

        if (!(ig->igFlags & IGF_EXTEND))
        {
            printf("%sgcrefRegs", separator);
            emitDispRegSet(ig->igGCregs);
            separator = ", ";
        }

        if (ig->igFlags & IGF_BYREF_REGS)
        {
            printf("%sbyrefRegs", separator);
            emitDispRegSet(ig->igByrefRegs());
            separator = ", ";
        }

#if FEATURE_LOOP_ALIGN
        if (ig->igLoopBackEdge != nullptr)
        {
            printf("%sloop=IG%02u", separator, ig->igLoopBackEdge->igNum);
            separator = ", ";
        }
#endif // FEATURE_LOOP_ALIGN

        if (jitdump && !ig->igBlocks.empty())
        {
            for (auto block : ig->igBlocks)
            {
                printf("%s%s", separator, block->dspToString());
                separator = ", ";
            }
        }

        emitDispIGflags(ig->igFlags);

        if (ig == emitCurIG)
        {
            printf(" <-- Current IG");
        }
        if (ig == emitPrologIG)
        {
            printf(" <-- Prolog IG");
        }
        printf("\n");

        if (verbose)
        {
            BYTE*          ins = ig->igData;
            UNATIVE_OFFSET ofs = ig->igOffs;
            unsigned       cnt = ig->igInsCnt;

            if (cnt)
            {
                printf("\n");

                do
                {
                    instrDesc* id = (instrDesc*)ins;

                    emitDispIns(id, false, true, false, ofs, nullptr, 0, ig);

                    ins += emitSizeOfInsDsc(id);
                    ofs += id->idCodeSize();
                } while (--cnt);

                printf("\n");
            }
        }
    }
}

void emitter::emitDispIGlist(bool verbose)
{
    insGroup* ig;
    insGroup* igPrev;

    for (igPrev = nullptr, ig = emitIGlist; ig; igPrev = ig, ig = ig->igNext)
    {
        emitDispIG(ig, igPrev, verbose);
    }
}

void emitter::emitDispGCinfo()
{
    printf("Emitter GC tracking info:");

    printf("\n  emitPrevGCrefVars ");
    dumpConvertedVarSet(emitComp, emitPrevGCrefVars);
    printf("\n  emitPrevGCrefRegs");
    emitDispRegSet(emitPrevGCrefRegs);
    printf("\n  emitPrevByrefRegs");
    emitDispRegSet(emitPrevByrefRegs);

    printf("\n  emitInitGCrefVars ");
    dumpConvertedVarSet(emitComp, emitInitGCrefVars);
    printf("\n  emitInitGCrefRegs");
    emitDispRegSet(emitInitGCrefRegs);
    printf("\n  emitInitByrefRegs");
    emitDispRegSet(emitInitByrefRegs);

    printf("\n  emitThisGCrefVars ");
    dumpConvertedVarSet(emitComp, emitThisGCrefVars);
    printf("\n  emitThisGCrefRegs");
    emitDispRegSet(emitThisGCrefRegs);
    printf("\n  emitThisByrefRegs");
    emitDispRegSet(emitThisByrefRegs);

    printf("\n\n");
}

#endif // DEBUG

/*****************************************************************************
 *
 *  Issue the given instruction. Basically, this is just a thin wrapper around
 *  emitOutputInstr() that does a few debug checks.
 */

size_t emitter::emitIssue1Instr(insGroup* ig, instrDesc* id, BYTE** dp)
{
    size_t is;

    /* Record the beginning offset of the instruction */

    BYTE* curInsAdr = *dp;

    /* Issue the next instruction */

    // printf("[S=%02u] " , emitCurStackLvl);

    is = emitOutputInstr(ig, id, dp);

#if defined(DEBUG) || defined(LATE_DISASM)
    float insExeCost = insEvaluateExecutionCost(id);
    // All compPerfScore calculations must be performed using doubles
    double insPerfScore = (double)(ig->igWeight / (double)BB_UNITY_WEIGHT) * insExeCost;
    emitComp->info.compPerfScore += insPerfScore;
    ig->igPerfScore += insPerfScore;
#endif // defined(DEBUG) || defined(LATE_DISASM)

// If we're generating a full pointer map and the stack
// is empty, there better not be any "pending" argument
// push entries.

#if !FEATURE_FIXED_OUT_ARGS
    assert(!emitFullGCinfo || (emitCurStackLvl != 0) || (u2.emitGcArgTrackCnt == 0));
#endif

    /* Did the size of the instruction match our expectations? */

    UNATIVE_OFFSET actualSize = (UNATIVE_OFFSET)(*dp - curInsAdr);

    unsigned estimatedSize = id->idCodeSize();
    if (actualSize != estimatedSize)
    {
        // It is fatal to under-estimate the instruction size, except for alignment instructions
        noway_assert(estimatedSize >= actualSize);

#if FEATURE_LOOP_ALIGN
        // Should never over-estimate align instruction or any instruction before the last align instruction of a method
        assert(id->idIns() != INS_align && emitCurIG->igNum > emitLastAlignedIgNum);
#endif

#if DEBUG_EMIT
        if (EMITVERBOSE)
        {
            printf("Instruction predicted size = %u, actual = %u\n", estimatedSize, actualSize);
        }
#endif // DEBUG_EMIT

        // Add the shrinkage to the ongoing offset adjustment. This needs to happen during the
        // processing of an instruction group, and not only at the beginning of an instruction
        // group, or else the difference of IG sizes between debug and release builds can cause
        // debug/non-debug asm diffs.
        int offsShrinkage = estimatedSize - actualSize;
        JITDUMP("Increasing size adj %d by %d => %d\n", emitOffsAdj, offsShrinkage, emitOffsAdj + offsShrinkage);
        emitOffsAdj += offsShrinkage;

        /* The instruction size estimate wasn't accurate; remember this */

        ig->igFlags |= IGF_UPD_ISZ;
#if defined(TARGET_XARCH)
        id->idCodeSize(actualSize);
#elif defined(TARGET_ARM)
// This is done as part of emitSetShortJump();
// insSize isz = emitInsSize(id->idInsFmt());
// id->idInsSize(isz);
#else
        /* It is fatal to over-estimate the instruction size */
        IMPL_LIMITATION("Over-estimated instruction size");
#endif
    }

#ifdef DEBUG
    /* Make sure the instruction descriptor size also matches our expectations */
    if (is != emitSizeOfInsDsc(id))
    {
        printf("%s at %u: Expected size = %u , actual size = %u\n", emitIfName(id->idInsFmt()),
               id->idDebugOnlyInfo()->idNum, is, emitSizeOfInsDsc(id));
        assert(is == emitSizeOfInsDsc(id));
    }
#endif // DEBUG

    return is;
}

/*****************************************************************************
 *
 *  Update the offsets of all the instruction groups (note: please don't be
 *  lazy and call this routine frequently, it walks the list of instruction
 *  groups and thus it isn't cheap).
 */

void emitter::emitRecomputeIGoffsets()
{
    UNATIVE_OFFSET offs;
    insGroup*      ig;

    for (ig = emitIGlist, offs = 0; ig; ig = ig->igNext)
    {
        ig->igOffs = offs;
        assert(IsCodeAligned(ig->igOffs));
        offs += ig->igSize;
    }

    /* Set the total code size */

    emitTotalCodeSize = offs;

#ifdef DEBUG
    emitCheckIGoffsets();
#endif
}

//----------------------------------------------------------------------------------------
// emitDispCommentForHandle:
//    Displays a comment for a handle, e.g. displays a raw string for GTF_ICON_STR_HDL
//    or a class name for GTF_ICON_CLASS_HDL
//
// Arguments:
//    handle - a constant value to display a comment for
//    flags  - a flag that the describes the handle
//
void emitter::emitDispCommentForHandle(size_t handle, GenTreeFlags flag)
{
#ifdef DEBUG
    if (handle == 0)
    {
        return;
    }

#ifdef TARGET_XARCH
    const char* commentPrefix = "      ;";
#else
    const char* commentPrefix = "      //";
#endif

    flag &= GTF_ICON_HDL_MASK;
    const char* str = nullptr;

    if (flag == GTF_ICON_STR_HDL)
    {
        const WCHAR* wstr = emitComp->eeGetCPString(handle);
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
    else if (flag == GTF_ICON_CLASS_HDL)
    {
        str = emitComp->eeGetClassName(reinterpret_cast<CORINFO_CLASS_HANDLE>(handle));
    }
#ifndef TARGET_XARCH
    // These are less useful for xarch:
    else if (flag == GTF_ICON_CONST_PTR)
    {
        str = "const ptr";
    }
    else if (flag == GTF_ICON_GLOBAL_PTR)
    {
        str = "global ptr";
    }
    else if (flag == GTF_ICON_FIELD_HDL)
    {
        str = emitComp->eeGetFieldName(reinterpret_cast<CORINFO_FIELD_HANDLE>(handle));
    }
    else if (flag == GTF_ICON_STATIC_HDL)
    {
        str = "static handle";
    }
    else if (flag == GTF_ICON_METHOD_HDL)
    {
        str = emitComp->eeGetMethodFullName(reinterpret_cast<CORINFO_METHOD_HANDLE>(handle));
    }
    else if (flag == GTF_ICON_FTN_ADDR)
    {
        str = "function address";
    }
    else if (flag == GTF_ICON_TOKEN_HDL)
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
#endif // DEBUG
}

/*****************************************************************************
 *  Bind targets of relative jumps to choose the smallest possible encoding.
 *  X86 and AMD64 have a small and large encoding.
 *  ARM has a small, medium, and large encoding. The large encoding is a pseudo-op
 *      to handle greater range than the conditional branch instructions can handle.
 *  ARM64 has a small and large encoding for both conditional branch and loading label addresses.
 *      The large encodings are pseudo-ops that represent a multiple instruction sequence, similar to ARM. (Currently
 *      NYI).
 */

void emitter::emitJumpDistBind()
{
#ifdef DEBUG
    if (emitComp->verbose)
    {
        printf("*************** In emitJumpDistBind()\n");
    }
    if (EMIT_INSTLIST_VERBOSE)
    {
        printf("\nInstruction list before jump distance binding:\n\n");
        emitDispIGlist(true);
    }
#endif

    instrDescJmp* jmp;

    UNATIVE_OFFSET minShortExtra; // The smallest offset greater than that required for a jump to be converted
                                  // to a small jump. If it is small enough, we will iterate in hopes of
                                  // converting those jumps we missed converting the first (or second...) time.

#if defined(TARGET_ARM)
    UNATIVE_OFFSET minMediumExtra; // Same as 'minShortExtra', but for medium-sized jumps.
#endif                             // TARGET_ARM

    UNATIVE_OFFSET adjIG;
    UNATIVE_OFFSET adjLJ;
    insGroup*      lstIG;
#ifdef DEBUG
    insGroup* prologIG = emitPrologIG;
#endif // DEBUG

    int jmp_iteration = 1;

/*****************************************************************************/
/* If we iterate to look for more jumps to shorten, we start again here.     */
/*****************************************************************************/

AGAIN:

#ifdef DEBUG
    emitCheckIGoffsets();
#endif

/*
    In the following loop we convert all jump targets from "BasicBlock *"
    to "insGroup *" values. We also estimate which jumps will be short.
 */

#ifdef DEBUG
    insGroup*     lastIG = nullptr;
    instrDescJmp* lastLJ = nullptr;
#endif

    lstIG         = nullptr;
    adjLJ         = 0;
    adjIG         = 0;
    minShortExtra = (UNATIVE_OFFSET)-1;

#if defined(TARGET_ARM)
    minMediumExtra = (UNATIVE_OFFSET)-1;
#endif // TARGET_ARM

    for (jmp = emitJumpList; jmp; jmp = jmp->idjNext)
    {
        insGroup* jmpIG;
        insGroup* tgtIG;

        UNATIVE_OFFSET jsz; // size of the jump instruction in bytes

        UNATIVE_OFFSET ssz = 0; // small  jump size
        NATIVE_OFFSET  nsd = 0; // small  jump max. neg distance
        NATIVE_OFFSET  psd = 0; // small  jump max. pos distance

#if defined(TARGET_ARM)
        UNATIVE_OFFSET msz = 0; // medium jump size
        NATIVE_OFFSET  nmd = 0; // medium jump max. neg distance
        NATIVE_OFFSET  pmd = 0; // medium jump max. pos distance
        NATIVE_OFFSET  mextra;  // How far beyond the medium jump range is this jump offset?
#endif                          // TARGET_ARM

        NATIVE_OFFSET  extra;           // How far beyond the short jump range is this jump offset?
        UNATIVE_OFFSET srcInstrOffs;    // offset of the source instruction of the jump
        UNATIVE_OFFSET srcEncodingOffs; // offset of the source used by the instruction set to calculate the relative
                                        // offset of the jump
        UNATIVE_OFFSET dstOffs;
        NATIVE_OFFSET  jmpDist; // the relative jump distance, as it will be encoded
        UNATIVE_OFFSET oldSize;
        UNATIVE_OFFSET sizeDif;

#ifdef TARGET_XARCH
        assert((jmp->idInsFmt() == IF_LABEL) || (jmp->idInsFmt() == IF_RWR_LABEL));

        /* Figure out the smallest size we can end up with */

        if (jmp->idInsFmt() == IF_LABEL)
        {
            if (emitIsCondJump(jmp))
            {
                ssz = JCC_SIZE_SMALL;
                nsd = JCC_DIST_SMALL_MAX_NEG;
                psd = JCC_DIST_SMALL_MAX_POS;
            }
            else
            {
                ssz = JMP_SIZE_SMALL;
                nsd = JMP_DIST_SMALL_MAX_NEG;
                psd = JMP_DIST_SMALL_MAX_POS;
            }
        }
#endif // TARGET_XARCH

#ifdef TARGET_ARM
        assert((jmp->idInsFmt() == IF_T2_J1) || (jmp->idInsFmt() == IF_T2_J2) || (jmp->idInsFmt() == IF_T1_I) ||
               (jmp->idInsFmt() == IF_T1_K) || (jmp->idInsFmt() == IF_T1_M) || (jmp->idInsFmt() == IF_T2_M1) ||
               (jmp->idInsFmt() == IF_T2_N1) || (jmp->idInsFmt() == IF_T1_J3) || (jmp->idInsFmt() == IF_LARGEJMP));

        /* Figure out the smallest size we can end up with */

        if (emitIsCondJump(jmp))
        {
            ssz = JCC_SIZE_SMALL;
            nsd = JCC_DIST_SMALL_MAX_NEG;
            psd = JCC_DIST_SMALL_MAX_POS;

            msz = JCC_SIZE_MEDIUM;
            nmd = JCC_DIST_MEDIUM_MAX_NEG;
            pmd = JCC_DIST_MEDIUM_MAX_POS;
        }
        else if (emitIsCmpJump(jmp))
        {
            ssz = JMP_SIZE_SMALL;
            nsd = 0;
            psd = 126;
        }
        else if (emitIsUncondJump(jmp))
        {
            ssz = JMP_SIZE_SMALL;
            nsd = JMP_DIST_SMALL_MAX_NEG;
            psd = JMP_DIST_SMALL_MAX_POS;
        }
        else if (emitIsLoadLabel(jmp))
        {
            ssz = LBL_SIZE_SMALL;
            nsd = LBL_DIST_SMALL_MAX_NEG;
            psd = LBL_DIST_SMALL_MAX_POS;
        }
        else
        {
            assert(!"Unknown jump instruction");
        }
#endif // TARGET_ARM

#ifdef TARGET_ARM64
        /* Figure out the smallest size we can end up with */

        if (emitIsCondJump(jmp))
        {
            ssz         = JCC_SIZE_SMALL;
            bool isTest = (jmp->idIns() == INS_tbz) || (jmp->idIns() == INS_tbnz);

            nsd = (isTest) ? TB_DIST_SMALL_MAX_NEG : JCC_DIST_SMALL_MAX_NEG;
            psd = (isTest) ? TB_DIST_SMALL_MAX_POS : JCC_DIST_SMALL_MAX_POS;
        }
        else if (emitIsUncondJump(jmp))
        {
            // Nothing to do; we don't shrink these.
            assert(jmp->idjShort);
            ssz = JMP_SIZE_SMALL;
        }
        else if (emitIsLoadLabel(jmp))
        {
            ssz = LBL_SIZE_SMALL;
            nsd = LBL_DIST_SMALL_MAX_NEG;
            psd = LBL_DIST_SMALL_MAX_POS;
        }
        else if (emitIsLoadConstant(jmp))
        {
            ssz = LDC_SIZE_SMALL;
            nsd = LDC_DIST_SMALL_MAX_NEG;
            psd = LDC_DIST_SMALL_MAX_POS;
        }
        else
        {
            assert(!"Unknown jump instruction");
        }
#endif // TARGET_ARM64

/* Make sure the jumps are properly ordered */

#ifdef DEBUG
        assert(lastLJ == nullptr || lastIG != jmp->idjIG || lastLJ->idjOffs < jmp->idjOffs);
        lastLJ = (lastIG == jmp->idjIG) ? jmp : nullptr;

        assert(lastIG == nullptr || lastIG->igNum <= jmp->idjIG->igNum || jmp->idjIG == prologIG ||
               emitNxtIGnum > unsigned(0xFFFF)); // igNum might overflow
        lastIG = jmp->idjIG;
#endif // DEBUG

        /* Get hold of the current jump size */

        jsz = jmp->idCodeSize();

        /* Get the group the jump is in */

        jmpIG = jmp->idjIG;

        /* Are we in a group different from the previous jump? */

        if (lstIG != jmpIG)
        {
            /* Were there any jumps before this one? */

            if (lstIG)
            {
                /* Adjust the offsets of the intervening blocks */

                do
                {
                    lstIG = lstIG->igNext;
                    assert(lstIG);
#ifdef DEBUG
                    if (EMITVERBOSE)
                    {
                        printf("Adjusted offset of " FMT_BB " from %04X to %04X\n", lstIG->igNum, lstIG->igOffs,
                               lstIG->igOffs - adjIG);
                    }
#endif // DEBUG
                    lstIG->igOffs -= adjIG;
                    assert(IsCodeAligned(lstIG->igOffs));
                } while (lstIG != jmpIG);
            }

            /* We've got the first jump in a new group */

            adjLJ = 0;
            lstIG = jmpIG;
        }

        /* Apply any local size adjustment to the jump's relative offset */

        jmp->idjOffs -= adjLJ;

        // If this is a jump via register, the instruction size does not change, so we are done.
        CLANG_FORMAT_COMMENT_ANCHOR;

#if defined(TARGET_ARM64)
        // JIT code and data will be allocated together for arm64 so the relative offset to JIT data is known.
        // In case such offset can be encodeable for `ldr` (+-1MB), shorten it.
        if (jmp->idAddr()->iiaIsJitDataOffset())
        {
            // Reference to JIT data
            assert(jmp->idIsBound());
            UNATIVE_OFFSET srcOffs = jmpIG->igOffs + jmp->idjOffs;

            int doff = jmp->idAddr()->iiaGetJitDataOffset();
            assert(doff >= 0);
            ssize_t imm = emitGetInsSC(jmp);
            assert((imm >= 0) && (imm < 0x1000)); // 0x1000 is arbitrary, currently 'imm' is always 0

            unsigned dataOffs = (unsigned)(doff + imm);
            assert(dataOffs < emitDataSize());

            // Conservately assume JIT data starts after the entire code size.
            // TODO-ARM64: we might consider only hot code size which will be computed later in emitComputeCodeSizes().
            assert(emitTotalCodeSize > 0);
            UNATIVE_OFFSET maxDstOffs = emitTotalCodeSize + dataOffs;

            // Check if the distance is within the encoding length.
            jmpDist = maxDstOffs - srcOffs;
            extra   = jmpDist - psd;
            if (extra <= 0)
            {
                goto SHORT_JMP;
            }

            // Keep the large form.
            continue;
        }
#endif

        /* Have we bound this jump's target already? */

        if (jmp->idIsBound())
        {
            /* Does the jump already have the smallest size? */

            if (jmp->idjShort)
            {
                assert(jmp->idCodeSize() == ssz);

                // We should not be jumping/branching across funclets/functions
                emitCheckFuncletBranch(jmp, jmpIG);

                continue;
            }

            tgtIG = jmp->idAddr()->iiaIGlabel;
        }
        else
        {
            /* First time we've seen this label, convert its target */
            CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef DEBUG
            if (EMITVERBOSE)
            {
                printf("Binding: ");
                emitDispIns(jmp);
                printf("Binding L_M%03u_" FMT_BB, emitComp->compMethodID, jmp->idAddr()->iiaBBlabel->bbNum);
            }
#endif // DEBUG

            tgtIG = (insGroup*)emitCodeGetCookie(jmp->idAddr()->iiaBBlabel);

#ifdef DEBUG
            if (EMITVERBOSE)
            {
                if (tgtIG)
                {
                    printf(" to %s\n", emitLabelString(tgtIG));
                }
                else
                {
                    printf("-- ERROR, no emitter cookie for " FMT_BB "; it is probably missing BBF_HAS_LABEL.\n",
                           jmp->idAddr()->iiaBBlabel->bbNum);
                }
            }
            assert(tgtIG);
#endif // DEBUG

            /* Record the bound target */

            jmp->idAddr()->iiaIGlabel = tgtIG;
            jmp->idSetIsBound();
        }

        // We should not be jumping/branching across funclets/functions
        emitCheckFuncletBranch(jmp, jmpIG);

#ifdef TARGET_XARCH
        /* Done if this is not a variable-sized jump */

        if ((jmp->idIns() == INS_push) || (jmp->idIns() == INS_mov) || (jmp->idIns() == INS_call) ||
            (jmp->idIns() == INS_push_hide))
        {
            continue;
        }
#endif
#ifdef TARGET_ARM
        if ((jmp->idIns() == INS_push) || (jmp->idIns() == INS_mov) || (jmp->idIns() == INS_movt) ||
            (jmp->idIns() == INS_movw))
        {
            continue;
        }
#endif
#ifdef TARGET_ARM64
        // There is only one size of unconditional branch; we don't support functions larger than 2^28 bytes (our branch
        // range).
        if (emitIsUncondJump(jmp))
        {
            continue;
        }
#endif

        /*
            In the following distance calculations, if we're not actually
            scheduling the code (i.e. reordering instructions), we can
            use the actual offset of the jump (rather than the beg/end of
            the instruction group) since the jump will not be moved around
            and thus its offset is accurate.

            First we need to figure out whether this jump is a forward or
            backward one; to do this we simply look at the ordinals of the
            group that contains the jump and the target.
         */

        srcInstrOffs = jmpIG->igOffs + jmp->idjOffs;

        /* Note that the destination is always the beginning of an IG, so no need for an offset inside it */
        dstOffs = tgtIG->igOffs;

#if defined(TARGET_ARM)
        srcEncodingOffs =
            srcInstrOffs + 4; // For relative branches, ARM PC is always considered to be the instruction address + 4
#elif defined(TARGET_ARM64)
        srcEncodingOffs =
            srcInstrOffs; // For relative branches, ARM64 PC is always considered to be the instruction address
#else
        srcEncodingOffs = srcInstrOffs + ssz; // Encoding offset of relative offset for small branch
#endif

        if (jmpIG->igNum < tgtIG->igNum)
        {
            /* Forward jump */

            /* Adjust the target offset by the current delta. This is a worst-case estimate, as jumps between
               here and the target could be shortened, causing the actual distance to shrink.
             */

            dstOffs -= adjIG;

            /* Compute the distance estimate */

            jmpDist = dstOffs - srcEncodingOffs;

            /* How much beyond the max. short distance does the jump go? */

            extra = jmpDist - psd;

#if DEBUG_EMIT
            assert(jmp->idDebugOnlyInfo() != nullptr);
            if (jmp->idDebugOnlyInfo()->idNum == (unsigned)INTERESTING_JUMP_NUM || INTERESTING_JUMP_NUM == 0)
            {
                if (INTERESTING_JUMP_NUM == 0)
                {
                    printf("[1] Jump %u:\n", jmp->idDebugOnlyInfo()->idNum);
                }
                printf("[1] Jump  block is at %08X\n", jmpIG->igOffs);
                printf("[1] Jump reloffset is %04X\n", jmp->idjOffs);
                printf("[1] Jump source is at %08X\n", srcEncodingOffs);
                printf("[1] Label block is at %08X\n", dstOffs);
                printf("[1] Jump  dist. is    %04X\n", jmpDist);
                if (extra > 0)
                {
                    printf("[1] Dist excess [S] = %d  \n", extra);
                }
            }
            if (EMITVERBOSE)
            {
                printf("Estimate of fwd jump [%08X/%03u]: %04X -> %04X = %04X\n", dspPtr(jmp),
                       jmp->idDebugOnlyInfo()->idNum, srcInstrOffs, dstOffs, jmpDist);
            }
#endif // DEBUG_EMIT

            if (extra <= 0)
            {
                /* This jump will be a short one */
                goto SHORT_JMP;
            }
        }
        else
        {
            /* Backward jump */

            /* Compute the distance estimate */

            jmpDist = srcEncodingOffs - dstOffs;

            /* How much beyond the max. short distance does the jump go? */

            extra = jmpDist + nsd;

#if DEBUG_EMIT
            assert(jmp->idDebugOnlyInfo() != nullptr);
            if (jmp->idDebugOnlyInfo()->idNum == (unsigned)INTERESTING_JUMP_NUM || INTERESTING_JUMP_NUM == 0)
            {
                if (INTERESTING_JUMP_NUM == 0)
                {
                    printf("[2] Jump %u:\n", jmp->idDebugOnlyInfo()->idNum);
                }
                printf("[2] Jump  block is at %08X\n", jmpIG->igOffs);
                printf("[2] Jump reloffset is %04X\n", jmp->idjOffs);
                printf("[2] Jump source is at %08X\n", srcEncodingOffs);
                printf("[2] Label block is at %08X\n", dstOffs);
                printf("[2] Jump  dist. is    %04X\n", jmpDist);
                if (extra > 0)
                {
                    printf("[2] Dist excess [S] = %d  \n", extra);
                }
            }
            if (EMITVERBOSE)
            {
                printf("Estimate of bwd jump [%08X/%03u]: %04X -> %04X = %04X\n", dspPtr(jmp),
                       jmp->idDebugOnlyInfo()->idNum, srcInstrOffs, dstOffs, jmpDist);
            }
#endif // DEBUG_EMIT

            if (extra <= 0)
            {
                /* This jump will be a short one */
                goto SHORT_JMP;
            }
        }

        /* We arrive here if the jump couldn't be made short, at least for now */

        /* We had better not have eagerly marked the jump as short
         * in emitIns_J(). If we did, then it has to be able to stay short
         * as emitIns_J() uses the worst case scenario, and blocks can
         * only move closer together after that.
         */
        assert(jmp->idjShort == 0);

        /* Keep track of the closest distance we got */

        if (minShortExtra > (unsigned)extra)
        {
            minShortExtra = (unsigned)extra;
        }

#if defined(TARGET_ARM)

        // If we're here, we couldn't convert to a small jump.
        // Handle conversion to medium-sized conditional jumps.
        // 'srcInstrOffs', 'srcEncodingOffs', 'dstOffs', 'jmpDist' have already been computed
        // and don't need to be recomputed.

        if (emitIsCondJump(jmp))
        {
            if (jmpIG->igNum < tgtIG->igNum)
            {
                /* Forward jump */

                /* How much beyond the max. medium distance does the jump go? */

                mextra = jmpDist - pmd;

#if DEBUG_EMIT
                assert(jmp->idDebugOnlyInfo() != NULL);
                if (jmp->idDebugOnlyInfo()->idNum == (unsigned)INTERESTING_JUMP_NUM || INTERESTING_JUMP_NUM == 0)
                {
                    if (mextra > 0)
                    {
                        if (INTERESTING_JUMP_NUM == 0)
                            printf("[6] Jump %u:\n", jmp->idDebugOnlyInfo()->idNum);
                        printf("[6] Dist excess [S] = %d  \n", mextra);
                    }
                }
#endif // DEBUG_EMIT

                if (mextra <= 0)
                {
                    /* This jump will be a medium one */
                    goto MEDIUM_JMP;
                }
            }
            else
            {
                /* Backward jump */

                /* How much beyond the max. medium distance does the jump go? */

                mextra = jmpDist + nmd;

#if DEBUG_EMIT
                assert(jmp->idDebugOnlyInfo() != NULL);
                if (jmp->idDebugOnlyInfo()->idNum == (unsigned)INTERESTING_JUMP_NUM || INTERESTING_JUMP_NUM == 0)
                {
                    if (mextra > 0)
                    {
                        if (INTERESTING_JUMP_NUM == 0)
                            printf("[7] Jump %u:\n", jmp->idDebugOnlyInfo()->idNum);
                        printf("[7] Dist excess [S] = %d  \n", mextra);
                    }
                }
#endif // DEBUG_EMIT

                if (mextra <= 0)
                {
                    /* This jump will be a medium one */
                    goto MEDIUM_JMP;
                }
            }

            /* We arrive here if the jump couldn't be made medium, at least for now */

            /* Keep track of the closest distance we got */

            if (minMediumExtra > (unsigned)mextra)
                minMediumExtra = (unsigned)mextra;
        }

#endif // TARGET_ARM

        /*****************************************************************************
         * We arrive here if the jump must stay long, at least for now.
         * Go try the next one.
         */

        continue;

    /*****************************************************************************/
    /* Handle conversion to short jump                                           */
    /*****************************************************************************/

    SHORT_JMP:

        /* Try to make this jump a short one */

        emitSetShortJump(jmp);

        if (!jmp->idjShort)
        {
            continue; // This jump must be kept long
        }

        /* This jump is becoming either short or medium */

        oldSize = jsz;
        jsz     = ssz;
        assert(oldSize >= jsz);
        sizeDif = oldSize - jsz;

#if defined(TARGET_XARCH)
        jmp->idCodeSize(jsz);
#elif defined(TARGET_ARM)
#if 0
        // This is done as part of emitSetShortJump():
        insSize isz = emitInsSize(jmp->idInsFmt());
        jmp->idInsSize(isz);
#endif
#elif defined(TARGET_ARM64)
        // The size of IF_LARGEJMP/IF_LARGEADR/IF_LARGELDC are 8 or 12.
        // All other code size is 4.
        assert((sizeDif == 4) || (sizeDif == 8));
#else
#error Unsupported or unset target architecture
#endif

        goto NEXT_JMP;

#if defined(TARGET_ARM)

    /*****************************************************************************/
    /* Handle conversion to medium jump                                          */
    /*****************************************************************************/

    MEDIUM_JMP:

        /* Try to make this jump a medium one */

        emitSetMediumJump(jmp);

        if (jmp->idCodeSize() > msz)
        {
            continue; // This jump wasn't shortened
        }
        assert(jmp->idCodeSize() == msz);

        /* This jump is becoming medium */

        oldSize = jsz;
        jsz     = msz;
        assert(oldSize >= jsz);
        sizeDif = oldSize - jsz;

        goto NEXT_JMP;

#endif // TARGET_ARM

    /*****************************************************************************/

    NEXT_JMP:

        /* Make sure the size of the jump is marked correctly */

        assert((0 == (jsz | jmpDist)) || (jsz == jmp->idCodeSize()));

#ifdef DEBUG
        if (EMITVERBOSE)
        {
            printf("Shrinking jump [%08X/%03u]\n", dspPtr(jmp), jmp->idDebugOnlyInfo()->idNum);
        }
#endif
        noway_assert((unsigned short)sizeDif == sizeDif);

        adjIG += sizeDif;
        adjLJ += sizeDif;
        jmpIG->igSize -= (unsigned short)sizeDif;
        emitTotalCodeSize -= sizeDif;

        /* The jump size estimate wasn't accurate; flag its group */

        jmpIG->igFlags |= IGF_UPD_ISZ;

    } // end for each jump

    /* Did we shorten any jumps? */

    if (adjIG)
    {
        /* Adjust offsets of any remaining blocks */

        assert(lstIG);

        for (;;)
        {
            lstIG = lstIG->igNext;
            if (!lstIG)
            {
                break;
            }
#ifdef DEBUG
            if (EMITVERBOSE)
            {
                printf("Adjusted offset of " FMT_BB " from %04X to %04X\n", lstIG->igNum, lstIG->igOffs,
                       lstIG->igOffs - adjIG);
            }
#endif // DEBUG
            lstIG->igOffs -= adjIG;
            assert(IsCodeAligned(lstIG->igOffs));
        }

#ifdef DEBUG
        emitCheckIGoffsets();
#endif

        /* Is there a chance of other jumps becoming short? */
        CLANG_FORMAT_COMMENT_ANCHOR;
#ifdef DEBUG
#if defined(TARGET_ARM)
        if (EMITVERBOSE)
            printf("Total shrinkage = %3u, min extra short jump size = %3u, min extra medium jump size = %u\n", adjIG,
                   minShortExtra, minMediumExtra);
#else
        if (EMITVERBOSE)
        {
            printf("Total shrinkage = %3u, min extra jump size = %3u\n", adjIG, minShortExtra);
        }
#endif
#endif

        if ((minShortExtra <= adjIG)
#if defined(TARGET_ARM)
            || (minMediumExtra <= adjIG)
#endif // TARGET_ARM
                )
        {
            jmp_iteration++;

#ifdef DEBUG
            if (EMITVERBOSE)
            {
                printf("Iterating branch shortening. Iteration = %d\n", jmp_iteration);
            }
#endif

            goto AGAIN;
        }
    }
#ifdef DEBUG
    if (EMIT_INSTLIST_VERBOSE)
    {
        printf("\nLabels list after the jump dist binding:\n\n");
        emitDispIGlist(false);
    }

    emitCheckIGoffsets();
#endif // DEBUG
}

#if FEATURE_LOOP_ALIGN

//-----------------------------------------------------------------------------
// emitLoopAlignment: Insert an align instruction at the end of emitCurIG and
//                    mark it as IGF_LOOP_ALIGN to indicate that next IG  is a
//                    loop needing alignment.
//
void emitter::emitLoopAlignment()
{
    unsigned short paddingBytes;

    if ((emitComp->opts.compJitAlignLoopBoundary > 16) && (!emitComp->opts.compJitAlignLoopAdaptive))
    {
        paddingBytes = emitComp->opts.compJitAlignLoopBoundary;
        emitLongLoopAlign(paddingBytes);
    }
    else
    {
        paddingBytes = MAX_ENCODED_SIZE;
        emitLoopAlign(paddingBytes);
    }

    // Mark this IG as need alignment so during emitter we can check the instruction count heuristics of
    // all IGs that follows this IG and participate in a loop.
    emitCurIG->igFlags |= IGF_LOOP_ALIGN;

    JITDUMP("Adding 'align' instruction of %d bytes in %s.\n", paddingBytes, emitLabelString(emitCurIG));

#ifdef DEBUG
    emitComp->loopAlignCandidates++;
#endif // DEBUG
}

//-----------------------------------------------------------------------------
//  emitEndsWithAlignInstr: Checks if current IG ends with loop align instruction.
//
//  Returns:  true if current IG ends with align instruction.
//
bool emitter::emitEndsWithAlignInstr()
{
    return emitCurIG->isLoopAlign();
}

//-----------------------------------------------------------------------------
//  getLoopSize: Starting from loopHeaderIg, find the size of the smallest possible loop
//               such that it doesn't exceed the maxLoopSize.
//
//  Arguments:
//       igLoopHeader    - The header IG of a loop
//       maxLoopSize     - Maximum loop size. If the loop is bigger than this value, we will just
//                         return this value.
//       isAlignAdjusted - Determine if adjustments are done to the align instructions or not.
//                         During generating code, it is 'false' (because we haven't adjusted the size yet).
//                         During outputting code, it is 'true'.
//
//  Returns:  size of a loop in bytes.
//
unsigned emitter::getLoopSize(insGroup* igLoopHeader, unsigned maxLoopSize DEBUG_ARG(bool isAlignAdjusted))
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
                    if (alignInstr->idaIG->igNum == igInLoop->igNum)
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

//-----------------------------------------------------------------------------
// emitSetLoopBackEdge : Sets igLoopBackEdge field, if not already set and
//                       if currIG has back-edge to dstIG.
//
// Notes:
//    Despite we align only inner most loop, we might see intersected loops because of control flow
//    re-arrangement like adding a split edge in LSRA.
//
//    If there is an intersection of current loop with last loop that is already marked as align,
//    then *do not align* one of the loop that completely encloses the other one. Or if they both intersect,
//    then *do not align* either of them because since the flow is complicated enough that aligning one of them
//    will not improve the performance.
//
void emitter::emitSetLoopBackEdge(BasicBlock* loopTopBlock)
{
    insGroup* dstIG            = (insGroup*)loopTopBlock->bbEmitCookie;
    bool      alignCurrentLoop = true;
    bool      alignLastLoop    = true;

    // With (dstIG != nullptr), ensure that only back edges are tracked.
    // If there is forward jump, dstIG is not yet generated.
    //
    // We don't rely on (block->bbJumpDest->bbNum <= block->bbNum) because the basic
    // block numbering is not guaranteed to be sequential.
    if ((dstIG != nullptr) && (dstIG->igNum <= emitCurIG->igNum))
    {
        unsigned currLoopStart = dstIG->igNum;
        unsigned currLoopEnd   = emitCurIG->igNum;

        // Only mark back-edge if current loop starts after the last inner loop ended.
        if (emitLastLoopEnd < currLoopStart)
        {
            emitCurIG->igLoopBackEdge = dstIG;

            JITDUMP("** IG%02u jumps back to IG%02u forming a loop.\n", currLoopEnd, currLoopStart);

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
                    JITDUMP("** Skip alignment for current loop IG%02u ~ IG%02u because it encloses an aligned loop "
                            "IG%02u ~ IG%02u.\n",
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
                    JITDUMP("** Skip alignment for aligned loop IG%02u ~ IG%02u because it encloses the current loop "
                            "IG%02u ~ IG%02u.\n",
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
}

//-----------------------------------------------------------------------------
//  emitLoopAlignAdjustments: Walk all the align instructions and update them
//    with actual padding needed.
//
//  Notes:
//     For IGs that have align instructions in the end, calculate the actual offset
//     of loop start and determine how much padding is needed. Based on that, update
//     the igOffs, igSize and emitTotalCodeSize.
//
void emitter::emitLoopAlignAdjustments()
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
            emitTotalCodeSize -= diff;

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

            JITDUMP("Adjusted alignment of %s from %u to %u.\n", emitLabelString(alignIG), estimatedPaddingNeeded,
                    actualPaddingNeeded);
            JITDUMP("Adjusted size of %s from %u to %u.\n", emitLabelString(alignIG), (alignIG->igSize + diff),
                    alignIG->igSize);
        }

        // Adjust the offset of all IGs starting from next IG until we reach the IG having the next
        // align instruction or the end of IG list.
        insGroup* adjOffIG     = alignIG->igNext;
        insGroup* adjOffUptoIG = alignInstr->idaNext != nullptr ? alignInstr->idaNext->idaIG : emitIGlast;
        while ((adjOffIG != nullptr) && (adjOffIG->igNum <= adjOffUptoIG->igNum))
        {
            JITDUMP("Adjusted offset of %s from %04X to %04X\n", emitLabelString(adjOffIG), adjOffIG->igOffs,
                    (adjOffIG->igOffs - alignBytesRemoved));
            adjOffIG->igOffs -= alignBytesRemoved;
            adjOffIG = adjOffIG->igNext;
        }

        if (actualPaddingNeeded > 0)
        {
            // Record the last IG that has align instruction. No overestimation
            // adjustment will be done after emitLastAlignedIgNum.
            emitLastAlignedIgNum = alignIG->igNum;
        }
    }

#ifdef DEBUG
    emitCheckIGoffsets();
#endif
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
unsigned emitter::emitCalculatePaddingForLoopAlignment(insGroup* ig, size_t offset DEBUG_ARG(bool isAlignAdjusted))
{
    assert(ig->isLoopAlign());
    unsigned alignmentBoundary = emitComp->opts.compJitAlignLoopBoundary;

    // No padding if loop is already aligned
    if ((offset & (alignmentBoundary - 1)) == 0)
    {
        JITDUMP(";; Skip alignment: 'Loop at %s already aligned at %dB boundary.'\n", emitLabelString(ig->igNext),
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
        JITDUMP(";; Skip alignment: 'Loop at %s is big. LoopSize= %d, MaxLoopSize= %d.'\n", emitLabelString(ig->igNext),
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
                JITDUMP(";; Skip alignment: 'Loop at %s already aligned at %uB boundary.'\n",
                        emitLabelString(ig->igNext), alignmentBoundary);
            }
            // Check if the alignment exceeds new maxPadding limit
            else if (nPaddingBytes > nMaxPaddingBytes)
            {
                skipPadding = true;
                JITDUMP(";; Skip alignment: 'Loop at %s PaddingNeeded= %d, MaxPadding= %d, LoopSize= %d, "
                        "AlignmentBoundary= %dB.'\n",
                        emitLabelString(ig->igNext), nPaddingBytes, nMaxPaddingBytes, loopSize, alignmentBoundary);
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
                JITDUMP(";; Skip alignment: 'Loop at %s is aligned to fit in %d blocks of %d chunks.'\n",
                        emitLabelString(ig->igNext), minBlocksNeededForLoop, alignmentBoundary);
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
            JITDUMP(";; Skip alignment: 'Loop at %s is aligned to fit in %d blocks of %d chunks.'\n",
                    emitLabelString(ig->igNext), minBlocksNeededForLoop, alignmentBoundary);
        }
    }

    JITDUMP(";; Calculated padding to add %d bytes to align %s at %dB boundary.\n", paddingToAdd,
            emitLabelString(ig->igNext), alignmentBoundary);

    // Either no padding is added because it is too expensive or the offset gets aligned
    // to the alignment boundary
    assert(paddingToAdd == 0 || (((offset + paddingToAdd) & (alignmentBoundary - 1)) == 0));

    return paddingToAdd;
}

#endif // FEATURE_LOOP_ALIGN

void emitter::emitCheckFuncletBranch(instrDesc* jmp, insGroup* jmpIG)
{
#ifdef DEBUG
    // We should not be jumping/branching across funclets/functions
    // Except possibly a 'call' to a finally funclet for a local unwind
    // or a 'return' from a catch handler (that can go just about anywhere)
    // This routine attempts to validate that any branches across funclets
    // meets one of those criteria...
    assert(jmp->idIsBound());

#ifdef TARGET_XARCH
    // An lea of a code address (for constant data stored with the code)
    // is treated like a jump for emission purposes but is not really a jump so
    // we don't have to check anything here.
    if (jmp->idIns() == INS_lea)
    {
        return;
    }
#endif

    if (jmp->idAddr()->iiaHasInstrCount())
    {
        // Too hard to figure out funclets from just an instruction count
        // You're on your own!
        return;
    }

#ifdef TARGET_ARM64
    // No interest if it's not jmp.
    if (emitIsLoadLabel(jmp) || emitIsLoadConstant(jmp))
    {
        return;
    }
#endif // TARGET_ARM64

    insGroup* tgtIG = jmp->idAddr()->iiaIGlabel;
    assert(tgtIG);
    if (tgtIG->igFuncIdx != jmpIG->igFuncIdx)
    {
        if (jmp->idDebugOnlyInfo()->idFinallyCall)
        {
            // We don't record enough information to determine this accurately, so instead
            // we assume that any branch to the very start of a finally is OK.

            // No branches back to the root method
            assert(tgtIG->igFuncIdx > 0);
            FuncInfoDsc* tgtFunc = emitComp->funGetFunc(tgtIG->igFuncIdx);
            assert(tgtFunc->funKind == FUNC_HANDLER);
            EHblkDsc* tgtEH = emitComp->ehGetDsc(tgtFunc->funEHIndex);

            // Only branches to finallys (not faults, catches, filters, etc.)
            assert(tgtEH->HasFinallyHandler());

            // Only to the first block of the finally (which is properly marked)
            BasicBlock* tgtBlk = tgtEH->ebdHndBeg;
            assert(tgtBlk->bbFlags & BBF_FUNCLET_BEG);

            // And now we made it back to where we started
            assert(tgtIG == emitCodeGetCookie(tgtBlk));
            assert(tgtIG->igFuncIdx == emitComp->funGetFuncIdx(tgtBlk));
        }
        else if (jmp->idDebugOnlyInfo()->idCatchRet)
        {
            // Again there isn't enough information to prove this correct
            // so just allow a 'branch' to any other 'parent' funclet

            FuncInfoDsc* jmpFunc = emitComp->funGetFunc(jmpIG->igFuncIdx);
            assert(jmpFunc->funKind == FUNC_HANDLER);
            EHblkDsc* jmpEH = emitComp->ehGetDsc(jmpFunc->funEHIndex);

            // Only branches out of catches
            assert(jmpEH->HasCatchHandler());

            FuncInfoDsc* tgtFunc = emitComp->funGetFunc(tgtIG->igFuncIdx);
            assert(tgtFunc);
            if (tgtFunc->funKind == FUNC_HANDLER)
            {
                // An outward chain to the containing funclet/EH handler
                // Note that it might be anywhere within nested try bodies
                assert(jmpEH->ebdEnclosingHndIndex == tgtFunc->funEHIndex);
            }
            else
            {
                // This funclet is 'top level' and so it is branching back to the
                // root function, and should have no containing EH handlers
                // but it could be nested within try bodies...
                assert(tgtFunc->funKind == FUNC_ROOT);
                assert(jmpEH->ebdEnclosingHndIndex == EHblkDsc::NO_ENCLOSING_INDEX);
            }
        }
        else
        {
            printf("Hit an illegal branch between funclets!");
            assert(tgtIG->igFuncIdx == jmpIG->igFuncIdx);
        }
    }
#endif // DEBUG
}

/*****************************************************************************
 *
 *  Compute the code sizes that we're going to use to allocate the code buffers.
 *
 *  This sets:
 *
 *      emitTotalHotCodeSize
 *      emitTotalColdCodeSize
 *      Compiler::info.compTotalHotCodeSize
 *      Compiler::info.compTotalColdCodeSize
 */

void emitter::emitComputeCodeSizes()
{
    assert((emitComp->fgFirstColdBlock == nullptr) == (emitFirstColdIG == nullptr));

    if (emitFirstColdIG)
    {
        emitTotalHotCodeSize  = emitFirstColdIG->igOffs;
        emitTotalColdCodeSize = emitTotalCodeSize - emitTotalHotCodeSize;
    }
    else
    {
        emitTotalHotCodeSize  = emitTotalCodeSize;
        emitTotalColdCodeSize = 0;
    }

    emitComp->info.compTotalHotCodeSize  = emitTotalHotCodeSize;
    emitComp->info.compTotalColdCodeSize = emitTotalColdCodeSize;

#ifdef DEBUG
    if (emitComp->verbose)
    {
        printf("\nHot  code size = 0x%X bytes\n", emitTotalHotCodeSize);
        printf("Cold code size = 0x%X bytes\n", emitTotalColdCodeSize);
    }
#endif
}

//------------------------------------------------------------------------
// emitEndCodeGen: called at end of code generation to create code, data, and gc info
//
// Arguments:
//    prologSize [OUT] - prolog size in bytes
//    epilogSize [OUT] - epilog size in bytes (see notes)
//    codeAddr [OUT] - address of the code buffer
//    coldCodeAddr [OUT] - address of the cold code buffer (if any)
//    consAddr [OUT] - address of the read only constant buffer (if any)
//
// Notes:
//    Currently, in methods with multiple epilogs, all epilogs must have the same
//    size. epilogSize is the size of just one of these epilogs, not the cumulative
//    size of all of the method's epilogs.
//
// Returns:
//    size of the method code, in bytes
//
unsigned emitter::emitEndCodeGen(unsigned* prologSize,
                                 unsigned* epilogSize,
                                 void**    codeAddr,
                                 void**    coldCodeAddr,
                                 void** consAddr DEBUGARG(unsigned* instrCount))
{
    JITDUMP("*************** In emitEndCodeGen()\n");

    BYTE* consBlock;
    BYTE* consBlockRW;
    BYTE* codeBlock;
    BYTE* codeBlockRW;
    BYTE* coldCodeBlock;
    BYTE* coldCodeBlockRW;
    BYTE* cp;

    assert(emitCurIG == nullptr);

    emitCodeBlock = nullptr;
    emitConsBlock = nullptr;

    emitOffsAdj = 0;

    emitFullyInt   = codeGen->GetInterruptible();
    emitFullGCinfo = codeGen->GetInterruptible() || !codeGen->isFramePointerUsed();

#if EMITTER_STATS
    GCrefsTable.record(emitGCrFrameOffsCnt);
    emitSizeTable.record(static_cast<unsigned>(emitSizeMethod));
    stkDepthTable.record(emitMaxStackDepth);
#endif // EMITTER_STATS

#if !FEATURE_FIXED_OUT_ARGS
#ifdef UNIX_X86_ABI
    emitFullArgInfo = !codeGen->isFramePointerUsed() || codeGen->GetInterruptible();
#else
    emitFullArgInfo = !codeGen->isFramePointerUsed();
#endif
    emitSimpleStkUsed         = true;
    u1.emitSimpleStkMask      = 0;
    u1.emitSimpleByrefStkMask = 0;
#endif

#if !FEATURE_FIXED_OUT_ARGS
    // Convert max. stack depth from # of bytes to # of entries.

    unsigned maxStackDepthIn4ByteElements = emitMaxStackDepth / REGSIZE_BYTES;
    JITDUMP("Converting emitMaxStackDepth from bytes (%d) to elements (%d)\n", emitMaxStackDepth,
            maxStackDepthIn4ByteElements);
    emitMaxStackDepth = maxStackDepthIn4ByteElements;
#endif

#if !FEATURE_FIXED_OUT_ARGS
    if ((emitMaxStackDepth > MAX_SIMPLE_STK_DEPTH) || emitFullGCinfo)
    {
        emitSimpleStkUsed = false;

        if (emitMaxStackDepth > sizeof(u2.emitArgTrackLcl))
        {
            u2.emitArgTrackTab = (BYTE*)emitGetMem(roundUp(emitMaxStackDepth));
        }
        else
        {
            u2.emitArgTrackTab = (BYTE*)u2.emitArgTrackLcl;
        }

        u2.emitArgTrackTop   = u2.emitArgTrackTab;
        u2.emitGcArgTrackCnt = 0;
    }
#endif // !FEATURE_FIXED_OUT_ARGS

    if (emitEpilogCnt == 0)
    {
        /* No epilogs, make sure the epilog size is set to 0 */

        emitEpilogSize = 0;

#ifdef TARGET_XARCH
        emitExitSeqSize = 0;
#endif // TARGET_XARCH
    }

    /* Return the size of the epilog to the caller */

    *epilogSize = emitEpilogSize;

#ifdef TARGET_XARCH
    *epilogSize += emitExitSeqSize;
#endif // TARGET_XARCH

#ifdef DEBUG
    if (EMIT_INSTLIST_VERBOSE)
    {
        printf("\nInstruction list before instruction issue:\n\n");
        emitDispIGlist(true);
    }

    emitCheckIGoffsets();
#endif

    /* Allocate the code block (and optionally the data blocks) */

    // If we're doing procedure splitting and we found cold blocks, then
    // allocate hot and cold buffers.  Otherwise only allocate a hot
    // buffer.

    coldCodeBlock = nullptr;

    CorJitAllocMemFlag allocMemFlag = CORJIT_ALLOCMEM_DEFAULT_CODE_ALIGN;

#ifdef TARGET_X86
    //
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
    //
    if (emitComp->fgHaveProfileData())
    {
        const float scenarioHotWeight = 256.0f;
        if (emitComp->fgCalledCount > (scenarioHotWeight * emitComp->fgProfileRunsCount()))
        {
            allocMemFlag = CORJIT_ALLOCMEM_FLG_16BYTE_ALIGN;
        }
    }
    else
    {
        if (emitTotalHotCodeSize <= 16)
        {
            allocMemFlag = CORJIT_ALLOCMEM_FLG_16BYTE_ALIGN;
        }
    }
#endif

#ifdef TARGET_XARCH
    // For x64/x86, align methods that are "optimizations enabled" to 32 byte boundaries if
    // they are larger than 16 bytes and contain a loop.
    //
    if (emitComp->opts.OptimizationEnabled() && !emitComp->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT) &&
        (emitTotalHotCodeSize > 16) && emitComp->fgHasLoops)
    {
        allocMemFlag = CORJIT_ALLOCMEM_FLG_32BYTE_ALIGN;
    }
#endif

    // This restricts the emitConsDsc.alignment to: 1, 2, 4, 8, 16, or 32 bytes
    // Alignments greater than 32 would require VM support in ICorJitInfo::allocMem
    assert(isPow2(emitConsDsc.alignment) && (emitConsDsc.alignment <= 32));

    if (emitConsDsc.alignment == 16)
    {
        allocMemFlag = static_cast<CorJitAllocMemFlag>(allocMemFlag | CORJIT_ALLOCMEM_FLG_RODATA_16BYTE_ALIGN);
    }
    else if (emitConsDsc.alignment == 32)
    {
        allocMemFlag = static_cast<CorJitAllocMemFlag>(allocMemFlag | CORJIT_ALLOCMEM_FLG_RODATA_32BYTE_ALIGN);
    }

    AllocMemArgs args;
    memset(&args, 0, sizeof(args));

#ifdef TARGET_ARM64
    // For arm64, we want to allocate JIT data always adjacent to code similar to what native compiler does.
    // This way allows us to use a single `ldr` to access such data like float constant/jmp table.
    if (emitTotalColdCodeSize > 0)
    {
        // JIT data might be far away from the cold code.
        NYI_ARM64("Need to handle fix-up to data from cold code.");
    }

    UNATIVE_OFFSET roDataAlignmentDelta = 0;
    if (emitConsDsc.dsdOffs && (emitConsDsc.alignment == TARGET_POINTER_SIZE))
    {
        UNATIVE_OFFSET roDataAlignment = TARGET_POINTER_SIZE; // 8 Byte align by default.
        roDataAlignmentDelta = (UNATIVE_OFFSET)ALIGN_UP(emitTotalHotCodeSize, roDataAlignment) - emitTotalHotCodeSize;
        assert((roDataAlignmentDelta == 0) || (roDataAlignmentDelta == 4));
    }

    args.hotCodeSize  = emitTotalHotCodeSize + roDataAlignmentDelta + emitConsDsc.dsdOffs;
    args.coldCodeSize = emitTotalColdCodeSize;
    args.roDataSize   = 0;
    args.xcptnsCount  = emitComp->compHndBBtabCount;
    args.flag         = allocMemFlag;

    emitCmpHandle->allocMem(&args);

    codeBlock       = (BYTE*)args.hotCodeBlock;
    codeBlockRW     = (BYTE*)args.hotCodeBlockRW;
    coldCodeBlock   = (BYTE*)args.coldCodeBlock;
    coldCodeBlockRW = (BYTE*)args.coldCodeBlockRW;

    consBlock   = codeBlock + emitTotalHotCodeSize + roDataAlignmentDelta;
    consBlockRW = codeBlockRW + emitTotalHotCodeSize + roDataAlignmentDelta;

#else

    args.hotCodeSize  = emitTotalHotCodeSize;
    args.coldCodeSize = emitTotalColdCodeSize;
    args.roDataSize   = emitConsDsc.dsdOffs;
    args.xcptnsCount  = emitComp->compHndBBtabCount;
    args.flag         = allocMemFlag;

    emitCmpHandle->allocMem(&args);

    codeBlock       = (BYTE*)args.hotCodeBlock;
    codeBlockRW     = (BYTE*)args.hotCodeBlockRW;
    coldCodeBlock   = (BYTE*)args.coldCodeBlock;
    coldCodeBlockRW = (BYTE*)args.coldCodeBlockRW;
    consBlock       = (BYTE*)args.roDataBlock;
    consBlockRW     = (BYTE*)args.roDataBlockRW;

#endif

#ifdef DEBUG
    if ((allocMemFlag & CORJIT_ALLOCMEM_FLG_32BYTE_ALIGN) != 0)
    {
        assert(((size_t)codeBlock & 31) == 0);
    }
#endif

    // if (emitConsDsc.dsdOffs)
    //     printf("Cons=%08X\n", consBlock);

    /* Give the block addresses to the caller and other functions here */

    *codeAddr = emitCodeBlock = codeBlock;
    *coldCodeAddr = emitColdCodeBlock = coldCodeBlock;
    *consAddr = emitConsBlock = consBlock;

#if !FEATURE_FIXED_OUT_ARGS
    // Nothing has been pushed on the stack.

    emitCurStackLvl = 0;
#endif

    /* Assume no live GC ref variables on entry */

    VarSetOps::ClearD(emitComp, emitThisGCrefVars); // This is initialized to Empty at the start of codegen.
#if defined(DEBUG) && defined(JIT32_ENCODER)
    VarSetOps::ClearD(emitComp, debugThisGCRefVars);
    VarSetOps::ClearD(emitComp, debugPrevGCRefVars);
    debugPrevRegPtrDsc = nullptr;
#endif
    emitThisGCrefRegs = emitThisByrefRegs = RBM_NONE;
    emitThisGCrefVset                     = true;

#ifdef DEBUG

    emitIssuing = true;

    // We don't use these after this point

    VarSetOps::AssignNoCopy(emitComp, emitPrevGCrefVars, VarSetOps::UninitVal());
    emitPrevGCrefRegs = emitPrevByrefRegs = 0xBAADFEED;

    VarSetOps::AssignNoCopy(emitComp, emitInitGCrefVars, VarSetOps::UninitVal());
    emitInitGCrefRegs = emitInitByrefRegs = 0xBAADFEED;

#endif

#ifdef JIT32_GCENCODER
    if (emitComp->lvaKeepAliveAndReportThis())
    {
        assert(emitComp->lvaIsOriginalThisArg(0));
        LclVarDsc* thisDsc = &emitComp->lvaTable[0];

        // If "this" (which is passed in as a register argument in REG_ARG_0)
        // is enregistered, we normally spot the "mov REG_ARG_0 -> thisReg"
        // in the prolog and note the location of "this" at that point.
        // However, if 'this' is enregistered into REG_ARG_0 itself, no code
        // will be generated in the prolog, so we explicitly need to note
        // the location of "this" here.
        // NOTE that we can do this even if "this" is not enregistered in
        // REG_ARG_0, and it will result in more accurate "this" info over the
        // prolog. However, as methods are not interruptible over the prolog,
        // we try to save space by avoiding that.

        if (thisDsc->lvRegister)
        {
            emitSyncThisObjReg = thisDsc->GetRegNum();

            if (emitFullGCinfo && (emitSyncThisObjReg == REG_ARG_0) &&
                ((codeGen->paramRegState.intRegLiveIn & RBM_ARG_0) != RBM_NONE))
            {
                emitGCregLiveSet(GCT_GCREF, RBM_ARG_0, emitCodeBlock, true);
            }
        }
    }
#endif // JIT32_GCENCODER

    if (emitGCrFrameOffsCnt != 0)
    {
        // Allocate and clear emitGCrFrameLiveTab[]. This is the table
        // mapping "stkOffs -> varPtrDsc". It holds a pointer to
        // the liveness descriptor that was created when the
        // variable became alive. When the variable becomes dead, the
        // descriptor will be appended to the liveness descriptor list, and
        // the entry in emitGCrFrameLiveTab[] will be made NULL.
        //
        // Note that if all GC refs are assigned consecutively,
        // emitGCrFrameLiveTab[] can be only as big as the number of GC refs
        // present, instead of lvaTrackedCount.

        size_t siz          = emitGCrFrameOffsCnt * sizeof(*emitGCrFrameLiveTab);
        emitGCrFrameLiveTab = static_cast<GCStackSlotLifetime**>(emitGetMem(roundUp(siz)));
        memset(emitGCrFrameLiveTab, 0, siz);

#if defined(JIT32_GCENCODER) && !defined(FEATURE_EH_FUNCLETS)
        if (emitComp->lvaKeepAliveAndReportThis())
        {
            assert(emitComp->lvaIsOriginalThisArg(emitComp->info.compThisArg));

            LclVarDsc* thisParam = emitComp->lvaGetDesc(emitComp->info.compThisArg);

            if (thisParam->HasGCSlotLiveness())
            {
                emitSyncThisObjOffs = thisParam->GetStackOffset();
            }
        }
#endif
    }

#ifdef DEBUG
    if (emitComp->verbose)
    {
        printf("\n***************************************************************************\n");
        printf("Instructions as they come out of the scheduler\n\n");
    }
#endif

    /* Issue all instruction groups in order */
    cp              = codeBlock;
    writeableOffset = codeBlockRW - codeBlock;

#define DEFAULT_CODE_BUFFER_INIT 0xcc

#ifdef DEBUG
    *instrCount = 0;
#endif
    for (insGroup* ig = emitIGlist; ig != nullptr; ig = ig->igNext)
    {
        assert(!(ig->igFlags & IGF_PLACEHOLDER)); // There better not be any placeholder groups left

        /* Is this the first cold block? */
        if (ig == emitFirstColdIG)
        {
            assert(emitCurCodeOffs(cp) == emitTotalHotCodeSize);

            assert(coldCodeBlock);
            cp              = coldCodeBlock;
            writeableOffset = coldCodeBlockRW - coldCodeBlock;
#ifdef DEBUG
            if (emitComp->opts.disAsm || emitComp->verbose)
            {
                printf("\n************** Beginning of cold code **************\n");
            }
#endif
        }

        /* Are we overflowing? */
        if (ig->igNext && (ig->igNum + 1 != ig->igNext->igNum))
        {
            NO_WAY("Too many instruction groups");
        }

        // If this instruction group is returned to from a funclet implementing a finally,
        // on architectures where it is necessary generate GC info for the current instruction as
        // if it were the instruction following a call.
        emitGenGCInfoIfFuncletRetTarget(ig, cp);

        instrDesc* id = (instrDesc*)ig->igData;

#ifdef DEBUG
        /* Print the IG label, but only if it is a branch label */

        if (emitComp->opts.disAsm || emitComp->verbose)
        {
            if (emitComp->verbose || emitComp->opts.disasmWithGC)
            {
                printf("\n");
                emitDispIG(ig); // Display the flags, IG data, etc.
            }
            else
            {
                printf("\n%s:", emitLabelString(ig));
                if (!emitComp->opts.disDiffable)
                {
                    printf("              ;; offset=%04XH", emitCurCodeOffs(cp));
                }
                printf("\n");
            }
        }
#endif // DEBUG

        BYTE* bp = cp;

        /* Record the actual offset of the block, noting the difference */

        int newOffsAdj = ig->igOffs - emitCurCodeOffs(cp);

#if DEBUG_EMIT
#ifdef DEBUG
        // Under DEBUG, only output under verbose flag.
        if (emitComp->verbose)
#endif // DEBUG
        {
            if (newOffsAdj != 0)
            {
                printf("Block predicted offs = %08X, actual = %08X -> size adj = %d\n", ig->igOffs, emitCurCodeOffs(cp),
                       newOffsAdj);
            }
            if (emitOffsAdj != newOffsAdj)
            {
                printf("Block expected size adj %d not equal to actual size adj %d (probably some instruction size was "
                       "underestimated but not included in the running `emitOffsAdj` count)\n",
                       emitOffsAdj, newOffsAdj);
            }
        }
        // Make it noisy in DEBUG if these don't match. In release, the noway_assert below checks the
        // fatal condition.
        assert(emitOffsAdj == newOffsAdj);
#endif // DEBUG_EMIT

        // We can't have over-estimated the adjustment, or we might have underestimated a jump distance.
        noway_assert(emitOffsAdj <= newOffsAdj);

        emitOffsAdj = newOffsAdj;
        assert(emitOffsAdj >= 0);

        ig->igOffs = emitCurCodeOffs(cp);
        assert(IsCodeAligned(ig->igOffs));

#if !FEATURE_FIXED_OUT_ARGS
        if (ig->igStkLvl != emitCurStackLvl)
        {
            /* We are pushing stuff implicitly at this label */

            assert((unsigned)ig->igStkLvl > (unsigned)emitCurStackLvl);
            emitStackPushN(cp, (ig->igStkLvl - (unsigned)emitCurStackLvl) / sizeof(int));
        }
#endif

        /* Update current GC information for IG's that do not extend the previous IG */

        if (!(ig->igFlags & IGF_EXTEND))
        {
            /* Is there a new set of live GC ref variables? */

            if (ig->igFlags & IGF_GC_VARS)
            {
                emitUpdateLiveGCvars(ig->igGCvars(), cp);
            }
            else if (!emitThisGCrefVset)
            {
                emitUpdateLiveGCvars(emitThisGCrefVars, cp);
            }

            /* Update the set of live GC ref registers */

            {
                regMaskTP GCregs = ig->igGCregs;

                if (GCregs != emitThisGCrefRegs)
                {
                    emitUpdateLiveGCregs(GCT_GCREF, GCregs, cp);
                }
            }

            /* Is there a new set of live byref registers? */

            if (ig->igFlags & IGF_BYREF_REGS)
            {
                unsigned byrefRegs = ig->igByrefRegs();

                if (byrefRegs != emitThisByrefRegs)
                {
                    emitUpdateLiveGCregs(GCT_BYREF, byrefRegs, cp);
                }
            }
#ifdef DEBUG
            if (EMIT_GC_VERBOSE || emitComp->opts.disasmWithGC)
            {
                emitDispGCInfoDelta();
            }
#endif // DEBUG
        }
        else
        {
            // These are not set for "overflow" groups
            assert(!(ig->igFlags & IGF_GC_VARS));
            assert(!(ig->igFlags & IGF_BYREF_REGS));
        }

        /* Issue each instruction in order */

        emitCurIG = ig;

        for (unsigned cnt = ig->igInsCnt; cnt > 0; cnt--)
        {
#ifdef DEBUG
            size_t     curInstrAddr = (size_t)cp;
            instrDesc* curInstrDesc = id;
#endif

            castto(id, BYTE*) += emitIssue1Instr(ig, id, &cp);

#ifdef DEBUG
            // Print the alignment boundary
            if ((emitComp->opts.disAsm || emitComp->verbose) && (emitComp->opts.disAddr || emitComp->opts.disAlignment))
            {
                size_t      afterInstrAddr   = (size_t)cp;
                instruction curIns           = curInstrDesc->idIns();
                bool        isJccAffectedIns = false;

#if defined(TARGET_XARCH)

                // Determine if this instruction is part of a set that matches the Intel jcc erratum characteristic
                // described here:
                // https://www.intel.com/content/dam/support/us/en/documents/processors/mitigations-jump-conditional-code-erratum.pdf
                // This is the case when a jump instruction crosses a 32-byte boundary, or ends on a 32-byte boundary.
                // "Jump instruction" in this case includes conditional jump (jcc), macro-fused op-jcc (where 'op' is
                // one of cmp, test, add, sub, and, inc, or dec), direct unconditional jump, indirect jump,
                // direct/indirect call, and return.

                size_t jccAlignBoundary     = 32;
                size_t jccAlignBoundaryMask = jccAlignBoundary - 1;
                size_t jccLastBoundaryAddr  = afterInstrAddr & ~jccAlignBoundaryMask;

                if (curInstrAddr < jccLastBoundaryAddr)
                {
                    isJccAffectedIns = IsJccInstruction(curIns) || IsJmpInstruction(curIns) || (curIns == INS_call) ||
                                       (curIns == INS_ret);

                    // For op-Jcc there are two cases: (1) curIns is the jcc, in which case the above condition
                    // already covers us. (2) curIns is the `op` and the next instruction is the `jcc`. Note that
                    // we will never have a `jcc` as the first instruction of a group, so we don't need to worry
                    // about looking ahead to the next group after a an `op` of `op-Jcc`.

                    if (!isJccAffectedIns && (cnt > 1))
                    {
                        // The current `id` is valid, namely, there is another instruction in this group.
                        instruction nextIns = id->idIns();
                        if (((curIns == INS_cmp) || (curIns == INS_test) || (curIns == INS_add) ||
                             (curIns == INS_sub) || (curIns == INS_and) || (curIns == INS_inc) ||
                             (curIns == INS_dec)) &&
                            IsJccInstruction(nextIns))
                        {
                            isJccAffectedIns = true;
                        }
                    }

                    if (isJccAffectedIns)
                    {
                        unsigned bytesCrossedBoundary = (unsigned)(afterInstrAddr & jccAlignBoundaryMask);
                        printf("; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ (%s: %d ; jcc erratum) %dB boundary "
                               "...............................\n",
                               genInsDisplayName(curInstrDesc), bytesCrossedBoundary, jccAlignBoundary);
                    }
                }

#endif // TARGET_XARCH

                // Jcc affected instruction boundaries were printed above; handle other cases here.
                if (!isJccAffectedIns)
                {
                    size_t alignBoundaryMask = (size_t)emitComp->opts.compJitAlignLoopBoundary - 1;
                    size_t lastBoundaryAddr  = afterInstrAddr & ~alignBoundaryMask;

                    // draw boundary if beforeAddr was before the lastBoundary.
                    if (curInstrAddr < lastBoundaryAddr)
                    {
                        // Indicate if instruction is at the alignment boundary or is split
                        unsigned bytesCrossedBoundary = (unsigned)(afterInstrAddr & alignBoundaryMask);
                        if (bytesCrossedBoundary != 0)
                        {
                            printf("; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ (%s: %d)", genInsDisplayName(curInstrDesc),
                                   bytesCrossedBoundary);
                        }
                        else
                        {
                            printf("; ...............................");
                        }
                        printf(" %dB boundary ...............................\n",
                               emitComp->opts.compJitAlignLoopBoundary);
                    }
                }
            }
#endif // DEBUG
        }

#ifdef DEBUG
        if (emitComp->opts.disAsm || emitComp->verbose)
        {
            printf("\t\t\t\t\t\t;; bbWeight=%s PerfScore %.2f", refCntWtd2str(ig->igWeight), ig->igPerfScore);
        }
        *instrCount += ig->igInsCnt;
#endif // DEBUG

        emitCurIG = nullptr;

        assert(ig->igSize >= cp - bp);

        // Is it the last ig in the hot part?
        bool lastHotIG = (emitFirstColdIG != nullptr && ig->igNext == emitFirstColdIG);
        if (lastHotIG)
        {
            unsigned actualHotCodeSize    = emitCurCodeOffs(cp);
            unsigned allocatedHotCodeSize = emitTotalHotCodeSize;
            assert(actualHotCodeSize <= allocatedHotCodeSize);
            if (actualHotCodeSize < allocatedHotCodeSize)
            {
                // The allocated chunk is bigger than used, fill in unused space in it.
                unsigned unusedSize = allocatedHotCodeSize - emitCurCodeOffs(cp);
                for (unsigned i = 0; i < unusedSize; ++i)
                {
                    *cp++ = DEFAULT_CODE_BUFFER_INIT;
                }
                assert(allocatedHotCodeSize == emitCurCodeOffs(cp));
            }
        }

        assert((ig->igSize >= cp - bp) || lastHotIG);
        ig->igSize = (unsigned short)(cp - bp);
    }

#if !FEATURE_FIXED_OUT_ARGS
    assert(emitCurStackLvl == 0);
#endif

    // Output any initialized data we may have.
    if (emitConsDsc.dsdOffs != 0)
    {
        emitOutputDataSec(&emitConsDsc, consBlock);
    }

    unsigned endCodeOffs = emitCurCodeOffs(cp);

    // Make sure all GC ref variables are marked as dead.
    for (unsigned i = 0; i < emitGCrFrameOffsCnt; i++)
    {
        if (emitGCrFrameLiveTab[i] != nullptr)
        {
            emitGCvarDeadSet(emitGCrFrameOffsMin + i * REGSIZE_BYTES, endCodeOffs, i);
        }
    }

    // No GC registers are live any more.
    if (emitThisByrefRegs)
    {
        emitUpdateLiveGCregs(GCT_BYREF, RBM_NONE, cp);
    }

    if (emitThisGCrefRegs)
    {
        emitUpdateLiveGCregs(GCT_GCREF, RBM_NONE, cp);
    }

    // Patch any forward jumps.
    if (emitFwdJumps)
    {
        for (instrDescJmp* jmp = emitJumpList; jmp != nullptr; jmp = jmp->idjNext)
        {
#ifdef TARGET_XARCH
            assert((jmp->idInsFmt() == IF_LABEL) || (jmp->idInsFmt() == IF_RWR_LABEL));
#endif
            insGroup* tgt = jmp->idAddr()->iiaIGlabel;

            if (jmp->idjTemp.idjAddr == nullptr)
            {
                continue;
            }

            if (jmp->idjOffs != tgt->igOffs)
            {
                BYTE* adr = jmp->idjTemp.idjAddr;
                int   adj = jmp->idjOffs - tgt->igOffs;
#ifdef TARGET_ARM
                // On Arm, the offset is encoded in unit of 2 bytes.
                adj >>= 1;
#endif

#if DEBUG_EMIT
                if ((jmp->idDebugOnlyInfo()->idNum == (unsigned)INTERESTING_JUMP_NUM) || (INTERESTING_JUMP_NUM == 0))
                {
#ifdef TARGET_ARM
                    printf("[5] This output is broken for ARM, since it doesn't properly decode the jump offsets of "
                           "the instruction at adr\n");
#endif

                    if (INTERESTING_JUMP_NUM == 0)
                    {
                        printf("[5] Jump %u:\n", jmp->idDebugOnlyInfo()->idNum);
                    }

                    if (jmp->idjShort)
                    {
                        printf("[5] Jump        is at %08X\n", (adr + 1 - emitCodeBlock));
                        printf("[5] Jump distance is  %02X - %02X = %02X\n", *(BYTE*)adr, adj, *(BYTE*)adr - adj);
                    }
                    else
                    {
                        printf("[5] Jump        is at %08X\n", (adr + 4 - emitCodeBlock));
                        printf("[5] Jump distance is  %08X - %02X = %08X\n", *(int*)adr, adj, *(int*)adr - adj);
                    }
                }
#endif // DEBUG_EMIT

                if (jmp->idjShort)
                {
                    // Patch Forward Short Jump
                    CLANG_FORMAT_COMMENT_ANCHOR;
#if defined(TARGET_XARCH)
                    *(BYTE*)(adr + writeableOffset) -= (BYTE)adj;
#elif defined(TARGET_ARM)
                    // The following works because the jump offset is in the low order bits of the instruction.
                    // Presumably we could also just call "emitOutputLJ(NULL, adr, jmp)", like for long jumps?
                    *(short int*)(adr + writeableOffset) -= (short)adj;
#elif defined(TARGET_ARM64)
                    assert(!jmp->idAddr()->iiaHasInstrCount());
                    emitOutputLJ(NULL, adr, jmp);
#else
#error Unsupported or unset target architecture
#endif
                }
                else
                {
                    // Patch Forward non-Short Jump
                    CLANG_FORMAT_COMMENT_ANCHOR;
#if defined(TARGET_XARCH)
                    *(int*)(adr + writeableOffset) -= adj;
#elif defined(TARGET_ARMARCH)
                    assert(!jmp->idAddr()->iiaHasInstrCount());
                    emitOutputLJ(NULL, adr, jmp);
#else
#error Unsupported or unset target architecture
#endif
                }
            }
        }
    }

#ifdef DEBUG
    if (emitComp->opts.disAsm)
    {
        printf("\n");
    }
#endif

    unsigned actualCodeSize = emitCurCodeOffs(cp);
    assert(emitTotalCodeSize >= actualCodeSize);

#if EMITTER_STATS
    totAllocdSize += emitTotalCodeSize;
    totActualSize += actualCodeSize;
#endif

    // Fill in eventual unused space, but do not report this space as used.
    // If you add this padding during the emitIGlist loop, then it will
    // emit offsets after the loop with wrong value (for example for GC ref variables).
    unsigned unusedSize = emitTotalCodeSize - actualCodeSize;

    JITDUMP("Allocated method code size = %4u , actual size = %4u, unused size = %4u\n", emitTotalCodeSize,
            actualCodeSize, unusedSize);

    BYTE* cpRW = cp + writeableOffset;
    for (unsigned i = 0; i < unusedSize; ++i)
    {
        *cpRW++ = DEFAULT_CODE_BUFFER_INIT;
    }
    cp = cpRW - writeableOffset;
    assert(emitTotalCodeSize == emitCurCodeOffs(cp));

    // Total code size is sum of all IG->size and doesn't include padding in the last IG.
    emitTotalCodeSize = actualCodeSize;

#ifdef DEBUG

    // Make sure these didn't change during the "issuing" phase

    assert(VarSetOps::MayBeUninit(emitPrevGCrefVars));
    assert(emitPrevGCrefRegs == 0xBAADFEED);
    assert(emitPrevByrefRegs == 0xBAADFEED);

    assert(VarSetOps::MayBeUninit(emitInitGCrefVars));
    assert(emitInitGCrefRegs == 0xBAADFEED);
    assert(emitInitByrefRegs == 0xBAADFEED);

    if (EMIT_INSTLIST_VERBOSE)
    {
        printf("\nLabels list after the end of codegen:\n\n");
        emitDispIGlist(false);
    }

    emitCheckIGoffsets();

#endif // DEBUG

    // Assign the real prolog size
    *prologSize = emitCodeOffset(emitPrologIG, emitPrologEndPos);

    /* Return the amount of code we've generated */

    return actualCodeSize;
}

// See specification comment at the declaration.
void emitter::emitGenGCInfoIfFuncletRetTarget(insGroup* ig, BYTE* cp)
{
#if defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
    // We only emit this GC information on targets where finally's are implemented via funclets,
    // and the finally is invoked, during non-exceptional execution, via a branch with a predefined
    // link register, rather than a "true call" for which we would already generate GC info.  Currently,
    // this means precisely ARM.
    if (ig->igFlags & IGF_FINALLY_TARGET)
    {
        // We don't actually have a call instruction in this case, so we don't have
        // a real size for that instruction.  We'll use 1.
        emitRecordGCCallPop(cp, /*callInstrSize*/ 1);

        /* Do we need to record a call location for GC purposes? */
        if (!emitFullGCinfo)
        {
            emitRecordGCcall(cp, /*callInstrSize*/ 1);
        }
    }
#endif // defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
}

/*****************************************************************************
 *
 *  We have an instruction in an insGroup and we need to know the
 *  instruction number for this instruction
 */

unsigned emitter::emitFindInsNum(insGroup* ig, instrDesc* idMatch)
{
    instrDesc* id = (instrDesc*)ig->igData;

    // Check if we are the first instruction in the group
    if (id == idMatch)
    {
        return 0;
    }

    /* Walk the list of instructions until we find a match */
    unsigned insNum       = 0;
    unsigned insRemaining = ig->igInsCnt;

    while (insRemaining > 0)
    {
        castto(id, BYTE*) += emitSizeOfInsDsc(id);
        insNum++;
        insRemaining--;

        if (id == idMatch)
        {
            return insNum;
        }
    }
    assert(!"emitFindInsNum failed");
    return -1;
}

/*****************************************************************************
 *
 *  We've been asked for the code offset of an instruction but alas one or
 *  more instruction sizes in the block have been mis-predicted, so we have
 *  to find the true offset by looking for the instruction within the group.
 */

UNATIVE_OFFSET emitter::emitFindOffset(insGroup* ig, unsigned insNum)
{
    instrDesc*     id = (instrDesc*)ig->igData;
    UNATIVE_OFFSET of = 0;

#ifdef DEBUG
    /* Make sure we were passed reasonable arguments */
    assert(ig && ig->igSelf == ig);
    assert(ig->igInsCnt >= insNum);
#endif

    /* Walk the instruction list until all are counted */

    while (insNum > 0)
    {
        of += id->idCodeSize();

        castto(id, BYTE*) += emitSizeOfInsDsc(id);

        insNum--;
    }

    return of;
}

//---------------------------------------------------------------------------
// emitDataGenBeg:
//   - Allocate space for a constant or block of the size and alignment requested
//     Returns the offset in the data section to use
//
// Arguments:
//    size       - The size in bytes of the constant or block
//    alignment  - The requested alignment for the data
//    dataType   - The type of the constant int/float/etc
//
// Note: This method only allocate the space for the constant or block.  It doesn't
//       initialize the value. You call emitDataGenData to initialize the value.
//
UNATIVE_OFFSET emitter::emitDataGenBeg(unsigned size, unsigned alignment, var_types dataType)
{
    unsigned     secOffs;
    dataSection* secDesc;

    assert(emitDataSecCur == nullptr);

    // The size must not be zero and must be a multiple of MIN_DATA_ALIGN
    // Additionally, MIN_DATA_ALIGN is the minimum alignment that will
    // actually be used. That is, if the user requests an alignment
    // less than MIN_DATA_ALIGN, they will get  something that is at least
    // MIN_DATA_ALIGN. We allow smaller alignment to be specified since it is
    // simpler to allow it than to check and block it.
    //
    assert((size != 0) && ((size % dataSection::MIN_DATA_ALIGN) == 0));
    assert(isPow2(alignment) && (alignment <= dataSection::MAX_DATA_ALIGN));

    /* Get hold of the current offset */
    secOffs = emitConsDsc.dsdOffs;

    if (((secOffs % alignment) != 0) && (alignment > dataSection::MIN_DATA_ALIGN))
    {
        // As per the above comment, the minimum alignment is actually (MIN_DATA_ALIGN)
        // bytes so we don't need to make any adjustments if the requested
        // alignment is less than MIN_DATA_ALIGN.
        //
        // The maximum requested alignment is tracked and the memory allocator
        // will end up ensuring offset 0 is at an address matching that
        // alignment.  So if the requested alignment is greater than MIN_DATA_ALIGN,
        // we need to pad the space out so the offset is a multiple of the requested.
        //
        uint8_t zeros[dataSection::MAX_DATA_ALIGN] = {}; // auto initialize to all zeros

        unsigned  zeroSize  = alignment - (secOffs % alignment);
        unsigned  zeroAlign = dataSection::MIN_DATA_ALIGN;
        var_types zeroType  = TYP_INT;

        emitBlkConst(&zeros, zeroSize, zeroAlign, zeroType);
        secOffs = emitConsDsc.dsdOffs;
    }

    assert((secOffs % alignment) == 0);
    emitConsDsc.alignment = max(emitConsDsc.alignment, alignment);

    /* Advance the current offset */
    emitConsDsc.dsdOffs += size;

    /* Allocate a data section descriptor and add it to the list */

    secDesc = emitDataSecCur = (dataSection*)emitGetMem(roundUp(sizeof(*secDesc) + size));

    secDesc->dsSize = size;

    secDesc->dsType = dataSection::data;

    secDesc->dsDataType = dataType;

    secDesc->dsNext = nullptr;

    if (emitConsDsc.dsdLast)
    {
        emitConsDsc.dsdLast->dsNext = secDesc;
    }
    else
    {
        emitConsDsc.dsdList = secDesc;
    }
    emitConsDsc.dsdLast = secDesc;

    return secOffs;
}

//  Start generating a constant data section for the current function
//  populated with BasicBlock references.
//  You can choose the references to be either absolute pointers, or
//  4-byte relative addresses.
//  Currently the relative references are relative to the start of the
//  first block (this is somewhat arbitrary)

UNATIVE_OFFSET emitter::emitBBTableDataGenBeg(unsigned numEntries, bool relativeAddr)
{
    unsigned     secOffs;
    dataSection* secDesc;

    assert(emitDataSecCur == nullptr);

    UNATIVE_OFFSET emittedSize;

    if (relativeAddr)
    {
        emittedSize = numEntries * 4;
    }
    else
    {
        emittedSize = numEntries * TARGET_POINTER_SIZE;
    }

    /* Get hold of the current offset */

    secOffs = emitConsDsc.dsdOffs;

    /* Advance the current offset */

    emitConsDsc.dsdOffs += emittedSize;

    /* Allocate a data section descriptor and add it to the list */

    secDesc = emitDataSecCur = (dataSection*)emitGetMem(roundUp(sizeof(*secDesc) + numEntries * sizeof(BasicBlock*)));

    secDesc->dsSize = emittedSize;

    secDesc->dsType = relativeAddr ? dataSection::blockRelative32 : dataSection::blockAbsoluteAddr;

    secDesc->dsDataType = TYP_UNKNOWN;

    secDesc->dsNext = nullptr;

    if (emitConsDsc.dsdLast)
    {
        emitConsDsc.dsdLast->dsNext = secDesc;
    }
    else
    {
        emitConsDsc.dsdList = secDesc;
    }

    emitConsDsc.dsdLast = secDesc;

    return secOffs;
}

/*****************************************************************************
 *
 *  Emit the given block of bits into the current data section.
 */

void emitter::emitDataGenData(unsigned offs, const void* data, UNATIVE_OFFSET size)
{
    assert(emitDataSecCur && (emitDataSecCur->dsSize >= offs + size));

    assert(emitDataSecCur->dsType == dataSection::data);

    memcpy(emitDataSecCur->dsCont + offs, data, size);
}

/*****************************************************************************
 *
 *  Emit the address of the given basic block into the current data section.
 */

void emitter::emitDataGenData(unsigned index, BasicBlock* label)
{
    assert(emitDataSecCur != nullptr);
    assert(emitDataSecCur->dsType == dataSection::blockAbsoluteAddr ||
           emitDataSecCur->dsType == dataSection::blockRelative32);

    unsigned emittedElemSize = emitDataSecCur->dsType == dataSection::blockAbsoluteAddr ? TARGET_POINTER_SIZE : 4;

    assert(emitDataSecCur->dsSize >= emittedElemSize * (index + 1));

    ((BasicBlock**)(emitDataSecCur->dsCont))[index] = label;
}

/*****************************************************************************
 *
 *  We're done generating a data section.
 */

void emitter::emitDataGenEnd()
{

#ifdef DEBUG
    assert(emitDataSecCur);
    emitDataSecCur = nullptr;
#endif
}

//---------------------------------------------------------------------------
// emitDataGenFind:
//   - Returns the offset of an existing constant in the data section
//     or INVALID_UNATIVE_OFFSET if there was no matching constant
//
// Arguments:
//    cnsAddr    - A pointer to the value of the constant that we need
//    cnsSize    - The size in bytes of the constant
//    alignment  - The requested alignment for the data
//    dataType   - The type of the constant int/float/etc
//
UNATIVE_OFFSET emitter::emitDataGenFind(const void* cnsAddr, unsigned cnsSize, unsigned alignment, var_types dataType)
{
    UNATIVE_OFFSET cnum     = INVALID_UNATIVE_OFFSET;
    unsigned       cmpCount = 0;
    unsigned       curOffs  = 0;
    dataSection*   secDesc  = emitConsDsc.dsdList;
    while (secDesc != nullptr)
    {
        // Search the existing secDesc entries

        // We can match as smaller 'cnsSize' value at the start of a larger 'secDesc->dsSize' block
        // We match the bit pattern, so the dataType can be different
        // Only match constants when the dsType is 'data'
        //
        if ((secDesc->dsType == dataSection::data) && (secDesc->dsSize >= cnsSize) && ((curOffs % alignment) == 0))
        {
            if (memcmp(cnsAddr, secDesc->dsCont, cnsSize) == 0)
            {
                cnum = curOffs;

                // We also might want to update the dsDataType
                //
                if ((secDesc->dsDataType != dataType) && (secDesc->dsSize == cnsSize))
                {
                    // If the subsequent dataType is floating point then change the original dsDataType
                    //
                    if (varTypeIsFloating(dataType))
                    {
                        secDesc->dsDataType = dataType;
                    }
                }
                break;
            }
        }

        curOffs += secDesc->dsSize;
        secDesc = secDesc->dsNext;

        if (++cmpCount > 64)
        {
            // If we don't find a match in the first 64, then we just add the new constant
            // This prevents an O(n^2) search cost
            break;
        }
    }

    return cnum;
}

//---------------------------------------------------------------------------
// emitDataConst:
//   - Returns the valid offset in the data section to use for the constant
//     described by the arguments to this method
//
// Arguments:
//    cnsAddr    - A pointer to the value of the constant that we need
//    cnsSize    - The size in bytes of the constant
//    alignment  - The requested alignment for the data
//    dataType   - The type of the constant int/float/etc
//
//
// Notes:  we call the method emitDataGenFind() to see if we already have
//   a matching constant that can be reused.
//
UNATIVE_OFFSET emitter::emitDataConst(const void* cnsAddr, unsigned cnsSize, unsigned cnsAlign, var_types dataType)
{
    UNATIVE_OFFSET cnum = emitDataGenFind(cnsAddr, cnsSize, cnsAlign, dataType);

    if (cnum == INVALID_UNATIVE_OFFSET)
    {
        cnum = emitDataGenBeg(cnsSize, cnsAlign, dataType);
        emitDataGenData(0, cnsAddr, cnsSize);
        emitDataGenEnd();
    }
    return cnum;
}

//------------------------------------------------------------------------
// emitBlkConst: Create a data section constant of arbitrary size.
//
// Arguments:
//    cnsAddr   - pointer to the block of data to be placed in the data section
//    cnsSize   - total size of the block of data in bytes
//    cnsAlign  - alignment of the data in bytes
//    elemType  - The type of the elements in the constant
//
// Return Value:
//    A field handle representing the data offset to access the constant.
//
CORINFO_FIELD_HANDLE emitter::emitBlkConst(const void* cnsAddr, unsigned cnsSize, unsigned cnsAlign, var_types elemType)
{
    UNATIVE_OFFSET cnum = emitDataGenBeg(cnsSize, cnsAlign, elemType);
    emitDataGenData(0, cnsAddr, cnsSize);
    emitDataGenEnd();

    return emitComp->eeFindJitDataOffs(cnum);
}

//------------------------------------------------------------------------
// emitFltOrDblConst: Create a float or double data section constant.
//
// Arguments:
//    constValue - constant value
//    attr       - constant size
//
// Return Value:
//    A field handle representing the data offset to access the constant.
//
// Notes:
//    If attr is EA_4BYTE then the double value is converted to a float value.
//    If attr is EA_8BYTE then 8 byte alignment is automatically requested.
//
CORINFO_FIELD_HANDLE emitter::emitFltOrDblConst(double constValue, emitAttr attr)
{
    assert((attr == EA_4BYTE) || (attr == EA_8BYTE));

    void*     cnsAddr;
    float     f;
    var_types dataType;

    if (attr == EA_4BYTE)
    {
        f        = forceCastToFloat(constValue);
        cnsAddr  = &f;
        dataType = TYP_FLOAT;
    }
    else
    {
        cnsAddr  = &constValue;
        dataType = TYP_DOUBLE;
    }

    // Access to inline data is 'abstracted' by a special type of static member
    // (produced by eeFindJitDataOffs) which the emitter recognizes as being a reference
    // to constant data, not a real static field.

    unsigned cnsSize  = (attr == EA_4BYTE) ? sizeof(float) : sizeof(double);
    unsigned cnsAlign = cnsSize;

#ifdef TARGET_XARCH
    if (emitComp->compCodeOpt() == SMALL_CODE)
    {
        // Some platforms don't require doubles to be aligned and so
        // we can use a smaller alignment to help with smaller code

        cnsAlign = dataSection::MIN_DATA_ALIGN;
    }
#endif // TARGET_XARCH

    UNATIVE_OFFSET cnum = emitDataConst(cnsAddr, cnsSize, cnsAlign, dataType);
    return emitComp->eeFindJitDataOffs(cnum);
}

/*****************************************************************************
 *
 *  Output the given data section at the specified address.
 */

void emitter::emitOutputDataSec(dataSecDsc* sec, BYTE* dst)
{
#ifdef DEBUG
    if (EMITVERBOSE)
    {
        printf("\nEmitting data sections: %u total bytes\n", sec->dsdOffs);
    }

    if (emitComp->opts.disAsm)
    {
        emitDispDataSec(sec);
    }

    unsigned secNum = 0;
#endif

    assert(dst);
    assert(sec->dsdOffs);
    assert(sec->dsdList);

    /* Walk and emit the contents of all the data blocks */

    dataSection* dsc;
    size_t       curOffs = 0;

    for (dsc = sec->dsdList; dsc; dsc = dsc->dsNext)
    {
        size_t dscSize = dsc->dsSize;

        BYTE* dstRW = dst + writeableOffset;

        // absolute label table
        if (dsc->dsType == dataSection::blockAbsoluteAddr)
        {
            JITDUMP("  section %u, size %u, block absolute addr\n", secNum++, dscSize);

            assert(dscSize && dscSize % TARGET_POINTER_SIZE == 0);
            size_t         numElems = dscSize / TARGET_POINTER_SIZE;
            target_size_t* bDstRW   = (target_size_t*)dstRW;
            for (unsigned i = 0; i < numElems; i++)
            {
                BasicBlock* block = ((BasicBlock**)dsc->dsCont)[i];

                // Convert the BasicBlock* value to an IG address
                insGroup* lab = (insGroup*)emitCodeGetCookie(block);

                // Append the appropriate address to the destination
                BYTE* target = emitOffsetToPtr(lab->igOffs);

#ifdef TARGET_ARM
                target = (BYTE*)((size_t)target | 1); // Or in thumb bit
#endif
                bDstRW[i] = (target_size_t)(size_t)target;
                if (emitComp->opts.compReloc)
                {
                    emitRecordRelocation(&(bDstRW[i]), target, IMAGE_REL_BASED_HIGHLOW);
                }

                JITDUMP("  " FMT_BB ": 0x%p\n", block->bbNum, bDstRW[i]);
            }
        }
        // relative label table
        else if (dsc->dsType == dataSection::blockRelative32)
        {
            JITDUMP("  section %u, size %u, block relative addr\n", secNum++, dscSize);

            size_t    numElems = dscSize / 4;
            unsigned* uDstRW   = (unsigned*)dstRW;
            insGroup* labFirst = (insGroup*)emitCodeGetCookie(emitComp->fgFirstBB);

            for (unsigned i = 0; i < numElems; i++)
            {
                BasicBlock* block = ((BasicBlock**)dsc->dsCont)[i];

                // Convert the BasicBlock* value to an IG address
                insGroup* lab = (insGroup*)emitCodeGetCookie(block);

                assert(FitsIn<uint32_t>(lab->igOffs - labFirst->igOffs));
                uDstRW[i] = lab->igOffs - labFirst->igOffs;

                JITDUMP("  " FMT_BB ": 0x%x\n", block->bbNum, uDstRW[i]);
            }
        }
        else
        {
            // Simple binary data: copy the bytes to the target
            assert(dsc->dsType == dataSection::data);

            memcpy(dstRW, dsc->dsCont, dscSize);

#ifdef DEBUG
            if (EMITVERBOSE)
            {
                printf("  section %3u, size %2u, RWD%2u:\t", secNum++, dscSize, curOffs);

                for (size_t i = 0; i < dscSize; i++)
                {
                    printf("%02x ", dsc->dsCont[i]);
                    if ((((i + 1) % 16) == 0) && (i + 1 != dscSize))
                    {
                        printf("\n\t\t\t\t\t");
                    }
                }
                switch (dsc->dsDataType)
                {
                    case TYP_FLOAT:
                        printf(" ; float  %9.6g", (double)*reinterpret_cast<float*>(&dsc->dsCont));
                        break;
                    case TYP_DOUBLE:
                        printf(" ; double %12.9g", *reinterpret_cast<double*>(&dsc->dsCont));
                        break;
                    default:
                        break;
                }
                printf("\n");
            }
#endif // DEBUG
        }

        curOffs += dscSize;
        dst += dscSize;
    }
}

#ifdef DEBUG

//------------------------------------------------------------------------
// emitDispDataSec: Dump a data section to stdout.
//
// Arguments:
//    section - the data section description
//
// Notes:
//    The output format attempts to mirror typical assembler syntax.
//    Data section entries lack type information so float/double entries
//    are displayed as if they are integers/longs.
//
void emitter::emitDispDataSec(dataSecDsc* section)
{
    printf("\n");

    unsigned offset = 0;

    for (dataSection* data = section->dsdList; data != nullptr; data = data->dsNext)
    {
        const char* labelFormat = "%-7s";
        char        label[64];
        sprintf_s(label, _countof(label), "RWD%02u", offset);
        printf(labelFormat, label);
        offset += data->dsSize;

        if ((data->dsType == dataSection::blockRelative32) || (data->dsType == dataSection::blockAbsoluteAddr))
        {
            insGroup* igFirst    = static_cast<insGroup*>(emitCodeGetCookie(emitComp->fgFirstBB));
            bool      isRelative = (data->dsType == dataSection::blockRelative32);
            size_t    blockCount = data->dsSize / (isRelative ? 4 : TARGET_POINTER_SIZE);

            for (unsigned i = 0; i < blockCount; i++)
            {
                if (i > 0)
                {
                    printf(labelFormat, "");
                }

                BasicBlock* block = reinterpret_cast<BasicBlock**>(data->dsCont)[i];
                insGroup*   ig    = static_cast<insGroup*>(emitCodeGetCookie(block));

                const char* blockLabel = emitLabelString(ig);
                const char* firstLabel = emitLabelString(igFirst);

                if (isRelative)
                {
                    if (emitComp->opts.disDiffable)
                    {
                        printf("\tdd\t%s - %s\n", blockLabel, firstLabel);
                    }
                    else
                    {
                        printf("\tdd\t%08Xh", ig->igOffs - igFirst->igOffs);
                    }
                }
                else
                {
#ifndef TARGET_64BIT
                    // We have a 32-BIT target
                    if (emitComp->opts.disDiffable)
                    {
                        printf("\tdd\t%s\n", blockLabel);
                    }
                    else
                    {
                        printf("\tdd\t%08Xh", (uint32_t)(size_t)emitOffsetToPtr(ig->igOffs));
                    }
#else  // TARGET_64BIT
                    // We have a 64-BIT target
                    if (emitComp->opts.disDiffable)
                    {
                        printf("\tdq\t%s\n", blockLabel);
                    }
                    else
                    {
                        printf("\tdq\t%016llXh", reinterpret_cast<uint64_t>(emitOffsetToPtr(ig->igOffs)));
                    }
#endif // TARGET_64BIT
                }

                if (!emitComp->opts.disDiffable)
                {
                    printf(" ; case %s\n", blockLabel);
                }
            }
        }
        else
        {
            assert(data->dsType == dataSection::data);
            unsigned elemSize = genTypeSize(data->dsDataType);
            if (elemSize == 0)
            {
                if ((data->dsSize % 8) == 0)
                {
                    elemSize = 8;
                }
                else if ((data->dsSize % 4) == 0)
                {
                    elemSize = 4;
                }
                else if ((data->dsSize % 2) == 0)
                {
                    elemSize = 2;
                }
                else
                {
                    elemSize = 1;
                }
            }

            for (unsigned i = 0; i < data->dsSize;)
            {
                switch (data->dsDataType)
                {
                    case TYP_FLOAT:
                        assert(data->dsSize >= 4);
                        printf("\tdd\t%08Xh\t", *reinterpret_cast<uint32_t*>(&data->dsCont[i]));
                        printf("\t; %.9gf", *reinterpret_cast<float*>(&data->dsCont[i]));
                        i += 4;
                        break;

                    case TYP_DOUBLE:
                        assert(data->dsSize >= 8);
                        printf("\tdq\t%016llXh", *reinterpret_cast<uint64_t*>(&data->dsCont[i]));
                        printf("\t; %.17g", *reinterpret_cast<double*>(&data->dsCont[i]));
                        i += 8;
                        break;

                    default:
                        switch (elemSize)
                        {
                            case 1:
                                printf("\tdb\t");
                                for (unsigned j = 0; j < 16 && i < data->dsSize; j++, i++)
                                {
                                    printf("%s%02Xh", j ? ", " : "", *reinterpret_cast<uint8_t*>(&data->dsCont[i]));
                                }
                                break;
                            case 2:
                                assert((data->dsSize % 2) == 0);
                                printf("\tdw\t");
                                for (unsigned j = 0; j < 12 && i < data->dsSize; j++, i += 2)
                                {
                                    printf("%s%04Xh", j ? ", " : "", *reinterpret_cast<uint16_t*>(&data->dsCont[i]));
                                }
                                break;
                            case 12:
                            case 4:
                                assert((data->dsSize % 4) == 0);
                                printf("\tdd\t");
                                for (unsigned j = 0; j < 6 && i < data->dsSize; j++, i += 4)
                                {
                                    printf("%s%08Xh", j ? ", " : "", *reinterpret_cast<uint32_t*>(&data->dsCont[i]));
                                }
                                break;
                            case 32:
                            case 16:
                            case 8:
                                assert((data->dsSize % 8) == 0);
                                printf("\tdq\t");
                                for (unsigned j = 0; j < 4 && i < data->dsSize; j++, i += 8)
                                {
                                    printf("%s%016llXh", j ? ", " : "", *reinterpret_cast<uint64_t*>(&data->dsCont[i]));
                                }
                                break;
                            default:
                                assert(!"unexpected elemSize");
                                break;
                        }
                }
                printf("\n");
            }
        }
    }
}
#endif

// Record the fact that the given variable now contains a live GC ref.
void emitter::emitGCvarLiveSet(int slotOffs, GCtype gcType, unsigned codeOffs, unsigned index)
{
    assert(emitIssuing);
    assert(abs(slotOffs) % REGSIZE_BYTES == 0);
    assert(gcType != GCT_NONE);
    assert(index < emitGCrFrameOffsCnt);
    assert(emitGCrFrameLiveTab[index] == nullptr);

#if defined(JIT32_GCENCODER) && !defined(FEATURE_EH_FUNCLETS)
    if (slotOffs == emitSyncThisObjOffs)
    {
        slotOffs |= this_OFFSET_FLAG;
    }
#endif

    if (gcType == GCT_BYREF)
    {
        slotOffs |= byref_OFFSET_FLAG;
    }

    GCStackSlotLifetime* lifetime = gcInfo.BeginStackSlotLifetime(slotOffs, codeOffs);
    emitGCrFrameLiveTab[index]    = lifetime;
    INDEBUG(debugGCSlotChanges.Push(lifetime));

    // The "global" live GC variable mask is no longer up-to-date.
    emitThisGCrefVset = false;
}

// Record the fact that the given variable no longer contains a live GC ref.
void emitter::emitGCvarDeadSet(int slotOffs, unsigned codeOffs, unsigned index)
{
    assert(emitIssuing);
    assert(abs(slotOffs) % REGSIZE_BYTES == 0);
    assert(index < emitGCrFrameOffsCnt);
    assert(emitGCrFrameLiveTab[index] != nullptr);

    GCStackSlotLifetime* lifetime = emitGCrFrameLiveTab[index];
    gcInfo.EndStackSlotLifetime(lifetime DEBUGARG(slotOffs), codeOffs);
    emitGCrFrameLiveTab[index] = nullptr;
    INDEBUG(debugGCSlotChanges.Push(lifetime));

    // The "global" live GC variable mask is no longer up-to-date.
    emitThisGCrefVset = false;
}

// Record a new set of live GC ref variables.
void emitter::emitUpdateLiveGCvars(VARSET_VALARG_TP vars, BYTE* addr)
{
    assert(emitIssuing);

    // Don't track GC changes in epilogs.
    if (emitIGisInEpilog(emitCurIG))
    {
        return;
    }

    if (emitThisGCrefVset && VarSetOps::Equal(emitComp, emitThisGCrefVars, vars))
    {
        return;
    }

    VarSetOps::Assign(emitComp, emitThisGCrefVars, vars);

    if (emitGCrFrameOffsCnt == 0)
    {
        emitThisGCrefVset = true;

        return;
    }

    unsigned codeOffs = emitCurCodeOffs(addr);

    for (unsigned trackedLclIndex = 0, count = emitComp->lvaTrackedCount; trackedLclIndex < count; trackedLclIndex++)
    {
        unsigned   lclNum = emitComp->lvaTrackedIndexToLclNum(trackedLclIndex);
        LclVarDsc* lcl    = emitComp->lvaGetDesc(lclNum);

        if (!lcl->HasGCSlotLiveness())
        {
            continue;
        }

        assert(varTypeIsGC(lcl->GetType()));

        int offs = lcl->GetStackOffset();

        assert(abs(offs) % REGSIZE_BYTES == 0);
        assert((emitGCrFrameOffsMin <= offs) && (offs < emitGCrFrameOffsMax));

        unsigned index = (offs - emitGCrFrameOffsMin) / REGSIZE_BYTES;
        assert(index < emitGCrFrameOffsCnt);

        if (VarSetOps::IsMember(emitComp, vars, trackedLclIndex))
        {
            if (emitGCrFrameLiveTab[index] == nullptr)
            {
                emitGCvarLiveSet(offs, lcl->TypeIs(TYP_BYREF) ? GCT_BYREF : GCT_GCREF, codeOffs, index);
            }
        }
        else
        {
            if (emitGCrFrameLiveTab[index] != nullptr)
            {
#if defined(JIT32_GCENCODER) && !defined(FEATURE_EH_FUNCLETS)
                assert(!emitComp->lvaKeepAliveAndReportThis() || (offs != emitSyncThisObjOffs));
#endif

                emitGCvarDeadSet(offs, codeOffs, index);
            }
        }
    }

    emitThisGCrefVset = true;
}

// Record a call location for GC purposes (we know that this is a method that
// will not be fully interruptible).
void emitter::emitRecordGCcall(BYTE* codePos, unsigned callInstrLength)
{
    assert(emitIssuing);
    assert(!emitFullGCinfo);

#ifdef JIT32_GCENCODER
    regMaskTP regs = (emitThisGCrefRegs | emitThisByrefRegs) & ~RBM_INTRET;

    if (regs == RBM_NONE)
    {
        if (emitCurStackLvl == 0)
        {
            return;
        }

        if (emitSimpleStkUsed)
        {
            if (u1.emitSimpleStkMask == 0)
            {
                return;
            }
        }
        else
        {
            if (u2.emitGcArgTrackCnt == 0)
            {
                return;
            }
        }
    }
#endif // JIT32_GCENCODER

    unsigned codeOffs = emitCurCodeOffs(codePos);

#ifdef DEBUG
    if (EMIT_GC_VERBOSE)
    {
#if FEATURE_FIXED_OUT_ARGS
        printf("; Call at %04X GCvars ", codeOffs - callInstrLength);
#else
        printf("; Call at %04X [stk=%u], GCvars ", codeOffs - callInstrLength, emitCurStackLvl);
#endif
        emitDispVarSet();
        printf(", gcrefRegs");
        emitDispRegSet(emitThisGCrefRegs);
        printf(", byrefRegs");
        emitDispRegSet(emitThisByrefRegs);

        printf("\n");
    }
#endif

    GCCallSite* call = gcInfo.AddCallSite(codeOffs, emitThisGCrefRegs, emitThisByrefRegs);

#ifndef JIT32_GCENCODER
    call->callInstrLength = static_cast<uint8_t>(callInstrLength);
#endif

#if !FEATURE_FIXED_OUT_ARGS
    noway_assert(FitsIn<uint16_t>(emitCurStackLvl / 4));

    if (emitSimpleStkUsed)
    {
        call->argCount     = 0;
        call->argMask      = u1.emitSimpleStkMask;
        call->byrefArgMask = u1.emitSimpleByrefStkMask;

        return;
    }

    call->argCount = u2.emitGcArgTrackCnt;

    if (call->argCount == 0)
    {
        call->argMask      = RBM_NONE;
        call->byrefArgMask = RBM_NONE;

        return;
    }

    call->argTable = new (emitComp, CMK_GC) unsigned[u2.emitGcArgTrackCnt];

    unsigned gcArgs = 0;
    unsigned stkLvl = emitCurStackLvl / sizeof(int);

    for (unsigned i = 0; i < stkLvl; i++)
    {
        GCtype gcType = static_cast<GCtype>(u2.emitArgTrackTab[stkLvl - i - 1]);

        if (gcType != GCT_NONE)
        {
            call->argTable[gcArgs] = i * TARGET_POINTER_SIZE;

            if (gcType == GCT_BYREF)
            {
                call->argTable[gcArgs] |= byref_OFFSET_FLAG;
            }

            gcArgs++;
        }
    }

    assert(u2.emitGcArgTrackCnt == gcArgs);
#endif //! FEATURE_FIXED_OUT_ARGS
}

/*****************************************************************************
 *
 *  Record a new set of live GC ref registers.
 */

void emitter::emitUpdateLiveGCregs(GCtype gcType, regMaskTP regs, BYTE* addr)
{
    assert(emitIssuing);

    // Don't track GC changes in epilogs
    if (emitIGisInEpilog(emitCurIG))
    {
        return;
    }

    regMaskTP life;
    regMaskTP dead;
    regMaskTP chg;

    assert(gcType != GCT_NONE);

    regMaskTP& emitThisXXrefRegs = (gcType == GCT_GCREF) ? emitThisGCrefRegs : emitThisByrefRegs;
    regMaskTP& emitThisYYrefRegs = (gcType == GCT_GCREF) ? emitThisByrefRegs : emitThisGCrefRegs;
    assert(emitThisXXrefRegs != regs);

    if (emitFullGCinfo)
    {
        /* Figure out which GC registers are becoming live/dead at this point */

        dead = (emitThisXXrefRegs & ~regs);
        life = (~emitThisXXrefRegs & regs);

        /* Can't simultaneously become live and dead at the same time */

        assert((dead | life) != 0);
        assert((dead & life) == 0);

        /* Compute the 'changing state' mask */

        chg = (dead | life);

        do
        {
            regMaskTP bit = genFindLowestBit(chg);
            regNumber reg = genRegNumFromMask(bit);

            if (life & bit)
            {
                emitGCregLiveUpd(gcType, reg, addr);
            }
            else
            {
                emitGCregDeadUpd(reg, addr);
            }

            chg -= bit;
        } while (chg);

        assert(emitThisXXrefRegs == regs);
    }
    else
    {
        emitThisYYrefRegs &= ~regs; // Kill the regs from the other GC type (if live)
        emitThisXXrefRegs = regs;   // Mark them as live in the requested GC type
    }

    // The 2 GC reg masks can't be overlapping

    assert((emitThisGCrefRegs & emitThisByrefRegs) == 0);
}

#ifdef JIT32_GCENCODER
void emitter::emitGCregLiveSet(GCtype gcType, regMaskTP regs, BYTE* addr, bool isThis)
#else
void emitter::emitGCregLiveSet(GCtype gcType, regMaskTP regs, BYTE* addr)
#endif
{
    assert(emitIssuing);
#ifdef JIT32_GCENCODER
    assert(emitFullyInt || (isThis && emitFullGCinfo));
#else
    assert(emitFullyInt);
#endif
    assert(((emitThisGCrefRegs | emitThisByrefRegs) & regs) == RBM_NONE);

#ifdef JIT32_GCENCODER
    gcInfo.AddLiveRegs(gcType, regs, emitCurCodeOffs(addr), isThis);
#else
    gcInfo.AddLiveRegs(gcType, regs, emitCurCodeOffs(addr));
#endif
}

void emitter::emitGCregDeadSet(GCtype gcType, regMaskTP regs, BYTE* addr)
{
    assert(emitIssuing);
    assert(emitFullyInt);
    assert(((emitThisGCrefRegs | emitThisByrefRegs) & regs) != RBM_NONE);

    gcInfo.RemoveLiveRegs(gcType, regs, emitCurCodeOffs(addr));
}

/*****************************************************************************
 *
 *  Emit an 8-bit integer as code.
 */

unsigned char emitter::emitOutputByte(BYTE* dst, ssize_t val)
{
    BYTE* dstRW = dst + writeableOffset;
    *castto(dstRW, unsigned char*) = (unsigned char)val;

#ifdef DEBUG
#ifdef TARGET_AMD64
    // if we're emitting code bytes, ensure that we've already emitted the rex prefix!
    assert(((val & 0xFF00000000LL) == 0) || ((val & 0xFFFFFFFF00000000LL) == 0xFFFFFFFF00000000LL));
#endif // TARGET_AMD64
#endif

    return sizeof(unsigned char);
}

/*****************************************************************************
 *
 *  Emit a 16-bit integer as code.
 */

unsigned char emitter::emitOutputWord(BYTE* dst, ssize_t val)
{
    BYTE* dstRW = dst + writeableOffset;
    MISALIGNED_WR_I2(dstRW, (short)val);

#ifdef DEBUG
#ifdef TARGET_AMD64
    // if we're emitting code bytes, ensure that we've already emitted the rex prefix!
    assert(((val & 0xFF00000000LL) == 0) || ((val & 0xFFFFFFFF00000000LL) == 0xFFFFFFFF00000000LL));
#endif // TARGET_AMD64
#endif

    return sizeof(short);
}

/*****************************************************************************
 *
 *  Emit a 32-bit integer as code.
 */

unsigned char emitter::emitOutputLong(BYTE* dst, ssize_t val)
{
    BYTE* dstRW = dst + writeableOffset;
    MISALIGNED_WR_I4(dstRW, (int)val);

#ifdef DEBUG
#ifdef TARGET_AMD64
    // if we're emitting code bytes, ensure that we've already emitted the rex prefix!
    assert(((val & 0xFF00000000LL) == 0) || ((val & 0xFFFFFFFF00000000LL) == 0xFFFFFFFF00000000LL));
#endif // TARGET_AMD64
#endif

    return sizeof(int);
}

/*****************************************************************************
 *
 *  Emit a pointer-sized integer as code.
 */

unsigned char emitter::emitOutputSizeT(BYTE* dst, ssize_t val)
{
    BYTE* dstRW = dst + writeableOffset;
#if !defined(TARGET_64BIT)
    MISALIGNED_WR_I4(dstRW, (int)val);
#else
    MISALIGNED_WR_ST(dstRW, val);
#endif

    return TARGET_POINTER_SIZE;
}

//------------------------------------------------------------------------
// Wrappers to emitOutputByte, emitOutputWord, emitOutputLong, emitOutputSizeT
// that take unsigned __int64 or size_t type instead of ssize_t. Used on RyuJIT/x86.
//
// Arguments:
//    dst - passed through
//    val - passed through
//
// Return Value:
//    Same as wrapped function.
//

#if !defined(HOST_64BIT)
#if defined(TARGET_X86)
unsigned char emitter::emitOutputByte(BYTE* dst, size_t val)
{
    return emitOutputByte(dst, (ssize_t)val);
}

unsigned char emitter::emitOutputWord(BYTE* dst, size_t val)
{
    return emitOutputWord(dst, (ssize_t)val);
}

unsigned char emitter::emitOutputLong(BYTE* dst, size_t val)
{
    return emitOutputLong(dst, (ssize_t)val);
}

unsigned char emitter::emitOutputSizeT(BYTE* dst, size_t val)
{
    return emitOutputSizeT(dst, (ssize_t)val);
}

unsigned char emitter::emitOutputByte(BYTE* dst, unsigned __int64 val)
{
    return emitOutputByte(dst, (ssize_t)val);
}

unsigned char emitter::emitOutputWord(BYTE* dst, unsigned __int64 val)
{
    return emitOutputWord(dst, (ssize_t)val);
}

unsigned char emitter::emitOutputLong(BYTE* dst, unsigned __int64 val)
{
    return emitOutputLong(dst, (ssize_t)val);
}

unsigned char emitter::emitOutputSizeT(BYTE* dst, unsigned __int64 val)
{
    return emitOutputSizeT(dst, (ssize_t)val);
}
#endif // defined(TARGET_X86)
#endif // !defined(HOST_64BIT)

/*****************************************************************************
 *
 *  Given a block cookie and a code position, return the actual code offset;
 *  this can only be called at the end of code generation.
 */

UNATIVE_OFFSET emitter::emitCodeOffset(void* blockPtr, unsigned codePos)
{
    insGroup* ig;

    UNATIVE_OFFSET of;
    unsigned       no = emitGetInsNumFromCodePos(codePos);

    /* Make sure we weren't passed some kind of a garbage thing */

    ig = (insGroup*)blockPtr;
#ifdef DEBUG
    assert(ig && ig->igSelf == ig);
#endif

    /* The first and last offsets are always easy */

    if (no == 0)
    {
        of = 0;
    }
    else if (no == ig->igInsCnt)
    {
        of = ig->igSize;
    }
    else if (ig->igFlags & IGF_UPD_ISZ)
    {
        /*
            Some instruction sizes have changed, so we'll have to figure
            out the instruction offset "the hard way".
         */

        of = emitFindOffset(ig, no);
    }
    else
    {
        /* All instructions correctly predicted, the offset stays the same */

        of = emitGetInsOfsFromCodePos(codePos);

        // printf("[IG=%02u;ID=%03u;OF=%04X] <= %08X\n", ig->igNum, emitGetInsNumFromCodePos(codePos), of, codePos);

        /* Make sure the offset estimate is accurate */

        assert(of == emitFindOffset(ig, emitGetInsNumFromCodePos(codePos)));
    }

    return ig->igOffs + of;
}

/*****************************************************************************
 *
 *  Record the fact that the given register now contains a live GC ref.
 */

void emitter::emitGCregLiveUpd(GCtype gcType, regNumber reg, BYTE* addr)
{
    assert(emitIssuing);

    // Don't track GC changes in epilogs
    if (emitIGisInEpilog(emitCurIG))
    {
        return;
    }

    assert(gcType != GCT_NONE);

    regMaskTP regMask = genRegMask(reg);

    regMaskTP& emitThisXXrefRegs = (gcType == GCT_GCREF) ? emitThisGCrefRegs : emitThisByrefRegs;
    regMaskTP& emitThisYYrefRegs = (gcType == GCT_GCREF) ? emitThisByrefRegs : emitThisGCrefRegs;

    if ((emitThisXXrefRegs & regMask) == RBM_NONE)
    {
        // If the register was holding the other GC type, that type should go dead now.

        if ((emitThisYYrefRegs & regMask) != RBM_NONE)
        {
            emitGCregDeadUpd(reg, addr);
        }

#ifdef JIT32_GCENCODER
        // For synchronized methods, "this" is always alive and in the same register.
        // However, if we generate any code after the epilog block (where "this"
        // goes dead), "this" will come alive again. We need to notice that.
        // Note that we only expect isThis to be true at an insGroup boundary.

        bool isThis = (reg == emitSyncThisObjReg);

        if (emitFullyInt || (emitFullGCinfo && isThis))
        {
            emitGCregLiveSet(gcType, regMask, addr, isThis);
        }
#else
        if (emitFullyInt)
        {
            emitGCregLiveSet(gcType, regMask, addr);
        }
#endif

        emitThisXXrefRegs |= regMask;
    }

    // The 2 GC reg masks can't be overlapping

    assert((emitThisGCrefRegs & emitThisByrefRegs) == RBM_NONE);
}

#ifdef FEATURE_EH_FUNCLETS
void emitter::emitGCregDeadAll(BYTE* addr)
{
    assert(emitIssuing);

    if (emitIGisInEpilog(emitCurIG))
    {
        return;
    }

    assert((emitThisByrefRegs & emitThisGCrefRegs) == RBM_NONE);

    if (emitFullyInt)
    {
        if (emitThisGCrefRegs != RBM_NONE)
        {
            emitGCregDeadSet(GCT_GCREF, emitThisGCrefRegs, addr);
        }

        if (emitThisByrefRegs != RBM_NONE)
        {
            emitGCregDeadSet(GCT_BYREF, emitThisByrefRegs, addr);
        }
    }

    emitThisGCrefRegs = RBM_NONE;
    emitThisByrefRegs = RBM_NONE;
}
#endif // FEATURE_EH_FUNCLETS

/*****************************************************************************
 *
 *  Record the fact that the given register no longer contains a live GC ref.
 */

void emitter::emitGCregDeadUpd(regNumber reg, BYTE* addr)
{
    assert(emitIssuing);

    // Don't track GC changes in epilogs
    if (emitIGisInEpilog(emitCurIG))
    {
        return;
    }

    regMaskTP regMask = genRegMask(reg);

    if ((emitThisGCrefRegs & regMask) != 0)
    {
        assert((emitThisByrefRegs & regMask) == 0);

        if (emitFullyInt)
        {
            emitGCregDeadSet(GCT_GCREF, regMask, addr);
        }

        emitThisGCrefRegs &= ~regMask;
    }
    else if ((emitThisByrefRegs & regMask) != 0)
    {
        if (emitFullyInt)
        {
            emitGCregDeadSet(GCT_BYREF, regMask, addr);
        }

        emitThisByrefRegs &= ~regMask;
    }
}

#if FEATURE_FIXED_OUT_ARGS

void emitter::emitGCargLiveUpd(int offs, GCtype gcType, BYTE* addr DEBUGARG(unsigned lclNum))
{
    assert(abs(offs) % REGSIZE_BYTES == 0);
    assert(gcType != GCT_NONE);
    assert(lclNum == emitComp->lvaOutgoingArgSpaceVar);

    if (!emitFullyInt)
    {
        return;
    }

    GCRegArgChange* change = gcInfo.AddRegArgChange();
    change->codeOffs       = emitCurCodeOffs(addr);
    change->argOffset      = offs;
    change->kind           = GCInfo::RegArgChangeKind::StoreArg;
    change->gcType         = gcType;
#ifdef JIT32_GCENCODER
    change->isCall = false;
    change->isThis = false;
#endif
}

void emitter::emitRecordGCCallPop(BYTE* addr, unsigned callInstrLength)
{
    assert(emitIssuing);
    assert((0 < callInstrLength) && (callInstrLength <= 16));

    if (!emitFullGCinfo)
    {
        emitRecordGCcall(addr, callInstrLength);

        return;
    }

    unsigned codeOffs = emitCurCodeOffs(addr);

#ifndef JIT32_GCENCODER
    if (!emitFullyInt)
    {
        GCCallSite* callSite      = gcInfo.AddCallSite(codeOffs, emitThisGCrefRegs, emitThisByrefRegs);
        callSite->callInstrLength = static_cast<uint8_t>(callInstrLength);

        return;
    }
#endif

#ifdef JIT32_GCENCODER
    unsigned gcrefRegs = 0;
    unsigned byrefRegs = 0;

    for (unsigned i = 0; i < CNT_CALLEE_SAVED; i++)
    {
        regMaskTP calleeSaved = GCInfo::calleeSaveOrder[i];

        if ((emitThisGCrefRegs & calleeSaved) != RBM_NONE)
        {
            gcrefRegs |= (1 << i);
        }

        if ((emitThisByrefRegs & calleeSaved) != RBM_NONE)
        {
            byrefRegs |= (1 << i);
        }
    }
#endif

    GCRegArgChange* change = gcInfo.AddRegArgChange();
    change->codeOffs       = codeOffs;
    change->argOffset      = 0;
    change->kind           = GCInfo::RegArgChangeKind::KillArgs;
    change->gcType         = GCT_GCREF;
#ifdef JIT32_GCENCODER
    change->isCall        = true;
    change->isThis        = false;
    change->callRefRegs   = gcrefRegs;
    change->callByrefRegs = byrefRegs;
#endif
}

#endif // FEATURE_FIXED_OUT_ARGS

void emitter::emitGCvarLiveUpd(int offs, GCtype gcType, BYTE* addr DEBUGARG(unsigned lclNum))
{
    assert(abs(offs) % REGSIZE_BYTES == 0);
    assert((emitGCrFrameOffsMin <= offs) && (offs < emitGCrFrameOffsMax));
    assert(gcType != GCT_NONE);
    assert(emitComp->lvaGetDesc(lclNum)->HasGCSlotLiveness());
#if FEATURE_FIXED_OUT_ARGS
    assert(lclNum != emitComp->lvaOutgoingArgSpaceVar);
#endif

    unsigned index = (offs - emitGCrFrameOffsMin) / REGSIZE_BYTES;
    assert(index < emitGCrFrameOffsCnt);

    if (emitGCrFrameLiveTab[index] == nullptr)
    {
        emitGCvarLiveSet(offs, gcType, emitCurCodeOffs(addr), index);
    }
}

/*****************************************************************************
 *
 *  Allocate a new IG and link it in to the global list after the current IG
 */

insGroup* emitter::emitAllocAndLinkIG()
{
    insGroup* ig = emitAllocIG();

    assert(emitCurIG);

    emitInsertIGAfter(emitCurIG, ig);

    /* Propagate some IG flags from the current group to the new group */

    ig->igFlags |= (emitCurIG->igFlags & IGF_PROPAGATE_MASK);

    /* Set the new IG as the current IG */

    emitCurIG = ig;

    return ig;
}

/*****************************************************************************
 *
 *  Allocate an instruction group descriptor and assign it the next index.
 */

insGroup* emitter::emitAllocIG()
{
    insGroup* ig;

    /* Allocate a group descriptor */

    size_t sz = sizeof(insGroup);
    ig        = (insGroup*)emitGetMem(sz);

#ifdef DEBUG
    ig->igSelf = ig;
#endif

#if EMITTER_STATS
    emitTotalIGcnt += 1;
    emitTotalIGsize += sz;
    emitSizeMethod += sz;
#endif

    /* Do basic initialization */

    emitInitIG(ig);

    return ig;
}

/*****************************************************************************
 *
 *  Initialize an instruction group
 */

void emitter::emitInitIG(insGroup* ig)
{
    /* Assign the next available index to the instruction group */

    ig->igNum = emitNxtIGnum;

    emitNxtIGnum++;

    /* Record the (estimated) code offset of the group */

    ig->igOffs = emitCurCodeOffset;
    assert(IsCodeAligned(ig->igOffs));

    /* Set the current function index */

    ig->igFuncIdx = emitComp->compCurrFuncIdx;

    ig->igFlags = 0;

#if defined(DEBUG) || defined(LATE_DISASM)
    ig->igWeight    = getCurrentBlockWeight();
    ig->igPerfScore = 0.0;
#endif

    /* Zero out some fields to avoid printing garbage in JitDumps. These
       really only need to be set in DEBUG, but do it in all cases to make
       sure we act the same in non-DEBUG builds.
    */

    ig->igSize   = 0;
    ig->igGCregs = RBM_NONE;
    ig->igInsCnt = 0;

#if FEATURE_LOOP_ALIGN
    ig->igLoopBackEdge = nullptr;
#endif

#ifdef DEBUG
    ig->lastGeneratedBlock = nullptr;
    // Explicitly call init, since IGs don't actually have a constructor.
    ig->igBlocks.jitstd::list<BasicBlock*>::init(emitComp->getAllocator(CMK_LoopOpt));
#endif
}

/*****************************************************************************
 *
 *  Insert instruction group 'ig' after 'igInsertAfterIG'
 */

void emitter::emitInsertIGAfter(insGroup* insertAfterIG, insGroup* ig)
{
    assert(emitIGlist);
    assert(emitIGlast);

    ig->igNext            = insertAfterIG->igNext;
    insertAfterIG->igNext = ig;

    if (emitIGlast == insertAfterIG)
    {
        // If we are inserting at the end, then update the 'last' pointer
        emitIGlast = ig;
    }
}

/*****************************************************************************
 *
 *  Save the current IG and start a new one.
 */

void emitter::emitNxtIG(bool extend)
{
    /* Right now we don't allow multi-IG prologs */

    assert(emitCurIG != emitPrologIG);

    /* First save the current group */

    emitSavIG(extend);

    /* Start generating the new group */

    emitNewIG();

    /* If this is an emitter added block, flag it */

    if (extend)
    {
        emitCurIG->igFlags |= IGF_EXTEND;

#if EMITTER_STATS
        emitTotalIGExtend++;
#endif // EMITTER_STATS
    }

    // We've created a new IG; no need to force another one.
    emitForceNewIG = false;

#ifdef DEBUG
    // We haven't written any code into the IG yet, so clear our record of the last block written to the IG.
    emitCurIG->lastGeneratedBlock = nullptr;
#endif
}

cnsval_ssize_t emitter::emitGetInsSC(instrDesc* id)
{
    if (id->idIsLargeCns())
    {
        return static_cast<instrDescCns*>(id)->idcCnsVal;
    }
    else
    {
        return id->idSmallCns();
    }
}

#ifdef TARGET_ARM

BYTE* emitter::emitGetInsRelocValue(instrDesc* id)
{
    return ((instrDescReloc*)id)->idrRelocVal;
}

#endif // TARGET_ARM

#if !FEATURE_FIXED_OUT_ARGS

// Record a push of a single dword on the stack.
void emitter::emitStackPush(BYTE* addr, GCtype gcType)
{
    if (emitSimpleStkUsed)
    {
        assert(!emitFullGCinfo); // Simple stk not used for emitFullGCinfo
        assert(emitCurStackLvl / sizeof(int) < MAX_SIMPLE_STK_DEPTH);

        u1.emitSimpleStkMask <<= 1;
        u1.emitSimpleStkMask |= (gcType != GCT_NONE);

        u1.emitSimpleByrefStkMask <<= 1;
        u1.emitSimpleByrefStkMask |= (gcType == GCT_BYREF);

        assert((u1.emitSimpleStkMask & u1.emitSimpleByrefStkMask) == u1.emitSimpleByrefStkMask);
    }
    else
    {
        emitStackPushLargeStk(addr, gcType, 1);
    }

    emitCurStackLvl += sizeof(int);
}

// Record a push of a bunch of non-GC dwords on the stack.
void emitter::emitStackPushN(BYTE* addr, unsigned count)
{
    assert(count);

    if (emitSimpleStkUsed)
    {
        assert(!emitFullGCinfo); // Simple stk not used for emitFullGCinfo

        u1.emitSimpleStkMask <<= count;
        u1.emitSimpleByrefStkMask <<= count;
    }
    else
    {
        emitStackPushLargeStk(addr, GCT_NONE, count);
    }

    emitCurStackLvl += count * sizeof(int);
}

// Record a pop of the given number of dwords from the stack.
void emitter::emitStackPop(BYTE* addr, bool isCall, unsigned callInstrSize, unsigned count)
{
    assert(!isCall || callInstrSize > 0);
#ifdef TARGET_X86
    assert(emitCurStackLvl / sizeof(int) >= count);

    if (count != 0)
    {
        if (emitSimpleStkUsed)
        {
            assert(!emitFullGCinfo); // Simple stk not used for emitFullGCinfo

            if (count >= MAX_SIMPLE_STK_DEPTH)
            {
                u1.emitSimpleStkMask      = 0;
                u1.emitSimpleByrefStkMask = 0;
            }
            else
            {
                u1.emitSimpleStkMask >>= count;
                u1.emitSimpleByrefStkMask >>= count;
            }
        }
        else
        {
            emitStackPopLargeStk(addr, isCall, callInstrSize, count);
        }

        emitCurStackLvl -= count * sizeof(int);
    }
    else
#endif // TARGET_X86
    {
        assert(isCall);

        // For the general encoder we do the call below always when it's a call, to ensure that the call is
        // recorded (when we're doing the ptr reg map for a non-fully-interruptible method).
        if (emitFullGCinfo
#ifndef JIT32_GCENCODER
            || (emitFullGCinfo && !emitFullyInt && isCall)
#endif
                )
        {
            emitStackPopLargeStk(addr, isCall, callInstrSize, 0);
        }
    }
}

// Record a push of a single word on the stack for a full pointer map.
void emitter::emitStackPushLargeStk(BYTE* addr, GCtype gcType, unsigned count)
{
    assert(count != 0);
    assert(!emitSimpleStkUsed);

    S_UINT32 level(emitCurStackLvl / sizeof(int));

    for (unsigned i = 0; i < count; i++)
    {
        assert(level.IsOverflow() || u2.emitArgTrackTop == u2.emitArgTrackTab + level.Value());
        *u2.emitArgTrackTop++ = (BYTE)gcType;
        assert(u2.emitArgTrackTop <= u2.emitArgTrackTab + emitMaxStackDepth);

        if (emitFullArgInfo || (gcType != GCT_NONE))
        {
            if (emitFullGCinfo)
            {
                if (level.IsOverflow() || !FitsIn<uint16_t>(level.Value()))
                {
                    IMPL_LIMITATION("Too many/too big arguments to encode GC information");
                }

                GCRegArgChange* change = gcInfo.AddRegArgChange();
                change->codeOffs       = emitCurCodeOffs(addr);
                change->argOffset      = static_cast<uint16_t>(level.Value());
                change->kind           = GCInfo::RegArgChangeKind::PushArg;
                change->gcType         = gcType;
                change->isCall         = false;
                change->isThis         = false;
            }

            u2.emitGcArgTrackCnt++;
        }

        level += 1;
        assert(!level.IsOverflow());
    }
}

// Record a pop of the given number of words from the stack for a full ptr map.
void emitter::emitStackPopLargeStk(BYTE* addr, bool isCall, unsigned callInstrSize, unsigned count)
{
    assert(emitIssuing);
#ifdef JIT32_GCENCODER
    // For the general encoder, we always need to record calls, so we make this call
    // even when emitSimpleStkUsed is true.
    assert(!emitSimpleStkUsed);
#endif

    S_UINT16 argRecCnt(0); // arg count for ESP, ptr-arg count for EBP

    /* Count how many pointer records correspond to this "pop" */

    for (unsigned argStkCnt = count; argStkCnt != 0; argStkCnt--)
    {
        assert(u2.emitArgTrackTop > u2.emitArgTrackTab);

        GCtype gcType = static_cast<GCtype>(*--u2.emitArgTrackTop);

        if (emitFullArgInfo || (gcType != GCT_NONE))
        {
            argRecCnt += 1;
        }
    }

    assert(u2.emitArgTrackTop >= u2.emitArgTrackTab);
    assert(u2.emitArgTrackTop == u2.emitArgTrackTab + emitCurStackLvl / sizeof(int) - count);
    noway_assert(!argRecCnt.IsOverflow());

    // We're about to pop the corresponding arg records
    u2.emitGcArgTrackCnt -= argRecCnt.Value();

#ifdef JIT32_GCENCODER
    // For the general encoder, we always have to record calls, so we don't take this early return.
    if (!emitFullGCinfo)
    {
        return;
    }
#endif

    // Do we have any interesting (i.e., callee-saved) registers live here?

    unsigned gcrefRegs = 0;
    unsigned byrefRegs = 0;

    // We make a bitmask whose bits correspond to callee-saved register indices (in the sequence
    // of callee-saved registers only).
    for (unsigned calleeSavedRegIdx = 0; calleeSavedRegIdx < CNT_CALLEE_SAVED; calleeSavedRegIdx++)
    {
        regMaskTP calleeSavedRbm = GCInfo::calleeSaveOrder[calleeSavedRegIdx];
        if (emitThisGCrefRegs & calleeSavedRbm)
        {
            gcrefRegs |= (1 << calleeSavedRegIdx);
        }
        if (emitThisByrefRegs & calleeSavedRbm)
        {
            byrefRegs |= (1 << calleeSavedRegIdx);
        }
    }

#ifdef JIT32_GCENCODER
    // For the general encoder, we always have to record calls, so we don't take this early return.    /* Are there any
    // args to pop at this call site?

    if (argRecCnt.Value() == 0)
    {
        // Or do we have a partially interruptible EBP-less frame, and any
        // of EDI,ESI,EBX,EBP are live, or is there an outer/pending call?
        CLANG_FORMAT_COMMENT_ANCHOR;

#if !FPO_INTERRUPTIBLE
        if (emitFullyInt || (gcrefRegs == 0 && byrefRegs == 0 && u2.emitGcArgTrackCnt == 0))
#endif
            return;
    }
#endif // JIT32_GCENCODER

    // Only calls may pop more than one value.
    // _cdecl calls accomplish this popping via a post-call-instruction SP adjustment.
    // The "rpdCall" field below should be interpreted as "the instruction accomplishes
    // call-related popping, even if it's not itself a call".  Therefore, we don't just
    // use the "isCall" input argument, which means that the instruction actually is a call --
    // we use the OR of "isCall" or the "pops more than one value."
    bool isCallRelatedPop = (argRecCnt.Value() > 1);

    GCRegArgChange* change = gcInfo.AddRegArgChange();
    change->codeOffs       = emitCurCodeOffs(addr);
    change->argOffset      = argRecCnt.Value();
    change->kind           = GCInfo::RegArgChangeKind::PopArgs;
    change->gcType         = GCT_GCREF;
    change->isCall         = isCall || isCallRelatedPop;
    change->isThis         = false;
    change->callRefRegs    = gcrefRegs;
    change->callByrefRegs  = byrefRegs;

#ifndef JIT32_GCENCODER
    if (change->isCall)
    {
        assert(isCall || callInstrSize == 0);
        change->callInstrLength = static_cast<uint8_t>(callInstrSize);
    }
#endif
}

// For caller-pop arguments, we report the arguments as pending arguments.
// However, any GC arguments are now dead, so we need to report them as non-GC.
void emitter::emitStackKillArgs(BYTE* addr, unsigned count, unsigned callInstrSize)
{
    assert(count > 0);

    if (emitSimpleStkUsed)
    {
        assert(!emitFullGCinfo); // Simple stk not used for emitFullGCInfo

        // We don't need to report this to the GC info, but we do need
        // to kill mark the ptrs on the stack as non-GC.

        assert(emitCurStackLvl / sizeof(int) >= count);

        for (unsigned lvl = 0; lvl < count; lvl++)
        {
            u1.emitSimpleStkMask &= ~(1 << lvl);
            u1.emitSimpleByrefStkMask &= ~(1 << lvl);
        }

        return;
    }

    BYTE*    argTrackTop = u2.emitArgTrackTop;
    S_UINT16 gcCnt(0);

    for (unsigned i = 0; i < count; i++)
    {
        assert(argTrackTop > u2.emitArgTrackTab);

        --argTrackTop;

        GCtype gcType = static_cast<GCtype>(*argTrackTop);

        if (gcType != GCT_NONE)
        {
            *argTrackTop = GCT_NONE;
            gcCnt += 1;
        }
    }

    noway_assert(!gcCnt.IsOverflow());

    /* We're about to kill the corresponding (pointer) arg records */

    if (!emitFullArgInfo)
    {
        u2.emitGcArgTrackCnt -= gcCnt.Value();
    }

    if (!emitFullGCinfo)
    {
        return;
    }

    // Right after the call, the arguments are still sitting on the
    // stack, but they are effectively dead. For fully-interruptible
    // methods, we need to report that.

    if (gcCnt.Value() != 0)
    {
        GCRegArgChange* change = gcInfo.AddRegArgChange();
        change->codeOffs       = emitCurCodeOffs(addr);
        change->argOffset      = gcCnt.Value();
        change->kind           = GCInfo::RegArgChangeKind::KillArgs;
        change->gcType         = GCT_GCREF;
        change->isThis         = false;
    }

    // Now that ptr args have been marked as non-ptrs, we need to
    // record the call itself as one that has no arguments.

    emitStackPopLargeStk(addr, true, callInstrSize, 0);
}

#endif // !FEATURE_FIXED_OUT_ARGS

// A helper for recording a relocation with the EE.
void emitter::emitRecordRelocation(void* location,            /* IN */
                                   void* target,              /* IN */
                                   WORD  fRelocType,          /* IN */
                                   WORD  slotNum /* = 0 */,   /* IN */
                                   INT32 addlDelta /* = 0 */) /* IN */
{
    assert(slotNum == 0); // It is unused on all supported platforms.

    // If we're an unmatched altjit, don't tell the VM anything. We still record the relocation for
    // late disassembly; maybe we'll need it?
    if (emitComp->info.compMatchedVM)
    {
        void* locationRW = (BYTE*)location + writeableOffset;
        emitCmpHandle->recordRelocation(location, locationRW, target, fRelocType, slotNum, addlDelta);
    }

#ifdef LATE_DISASM
    codeGen->getDisAssembler().disRecordRelocation((size_t)location, (size_t)target);
#endif
}

#ifdef TARGET_ARM
// A helper for handling a Thumb-Mov32 of position-independent (PC-relative) value
//
// This routine either records relocation for the location with the EE,
// or creates a virtual relocation entry to perform offset fixup during
// compilation without recording it with EE - depending on which of
// absolute/relocative relocations mode are used for code section.
void emitter::emitHandlePCRelativeMov32(void* location, /* IN */
                                        void* target)   /* IN */
{
    if (emitComp->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_RELATIVE_CODE_RELOCS))
    {
        emitRecordRelocation(location, target, IMAGE_REL_BASED_REL_THUMB_MOV32_PCREL);
    }
    else
    {
        emitRecordRelocation(location, target, IMAGE_REL_BASED_THUMB_MOV32);
    }
}
#endif // TARGET_ARM

// A helper for recording a call site with the EE.
void emitter::emitRecordCallSite(ULONG                 instrOffset,  /* IN */
                                 CORINFO_SIG_INFO*     callSig,      /* IN */
                                 CORINFO_METHOD_HANDLE methodHandle) /* IN */
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

    emitCmpHandle->recordCallSite(instrOffset, callSig, methodHandle);
#endif // DEBUG
}

#ifdef DEBUG

/*****************************************************************************
 *  Given a code offset, return a string representing a label for that offset.
 *  If the code offset is just after the end of the code of the function, the
 *  label will be "END". If the code offset doesn't correspond to any known
 *  offset, the label will be "UNKNOWN". The strings are returned from static
 *  buffers. This function rotates amongst four such static buffers (there are
 *  cases where this function is called four times to provide data for a single
 *  printf()).
 */

const char* emitter::emitOffsetToLabel(unsigned offs)
{
    const size_t    TEMP_BUFFER_LEN = 40;
    static unsigned curBuf          = 0;
    static char     buf[4][TEMP_BUFFER_LEN];
    char*           retbuf;

    UNATIVE_OFFSET nextof = 0;

    for (insGroup* ig = emitIGlist; ig != nullptr; ig = ig->igNext)
    {
        // There is an eventual unused space after the last actual hot block
        // before the first allocated cold block.
        assert((nextof == ig->igOffs) || (ig == emitFirstColdIG));

        if (ig->igOffs == offs)
        {
            return emitLabelString(ig);
        }
        else if (ig->igOffs > offs)
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
    else
    {
        sprintf_s(buf[curBuf], TEMP_BUFFER_LEN, "UNKNOWN");
        retbuf = buf[curBuf];
        curBuf = (curBuf + 1) % 4;
        return retbuf;
    }
}

#endif // DEBUG

//------------------------------------------------------------------------
// emitGetGCRegsSavedOrModified: Returns the set of registers that keeps gcrefs and byrefs across the call.
//
// Notes: it returns union of two sets:
//        1) registers that could contain GC/byRefs before the call and call doesn't touch them;
//        2) registers that contain GC/byRefs before the call and call modifies them, but they still
//           contain GC/byRefs.
//
// Arguments:
//   methHnd - the method handler of the call.
//
// Return value:
//   the saved set of registers.
//
regMaskTP emitter::emitGetGCRegsSavedOrModified(CORINFO_METHOD_HANDLE methHnd)
{
    // Is it a helper with a special saved set?
    bool isNoGCHelper = emitNoGChelper(methHnd);
    if (isNoGCHelper)
    {
        CorInfoHelpFunc helpFunc = Compiler::eeGetHelperNum(methHnd);

        // Get the set of registers that this call kills and remove it from the saved set.
        regMaskTP savedSet = RBM_ALLINT & ~emitGetGCRegsKilledByNoGCCall(helpFunc);

#ifdef DEBUG
        if (emitComp->verbose)
        {
            printf("NoGC Call: saved regs");
            emitDispRegSet(savedSet);
            printf("\n");
        }
#endif
        return savedSet;
    }
    else
    {
        // This is the saved set of registers after a normal call.
        return RBM_CALLEE_SAVED;
    }
}

//----------------------------------------------------------------------
// emitGetGCRegsKilledByNoGCCall: Gets a register mask that represents the set of registers that no longer
// contain GC or byref pointers, for "NO GC" helper calls. This is used by the emitter when determining
// what registers to remove from the current live GC/byref sets (and thus what to report as dead in the
// GC info). Note that for the CORINFO_HELP_ASSIGN_BYREF helper, in particular, the kill set reported by
// compHelperCallKillSet() doesn't match this kill set. compHelperCallKillSet() reports the dst/src
// address registers as killed for liveness purposes, since their values change. However, they still are
// valid byref pointers after the call, so the dst/src address registers are NOT reported as killed here.
//
// Note: This list may not be complete and defaults to the default RBM_CALLEE_TRASH_NOGC registers.
//
// Arguments:
//   helper - The helper being inquired about
//
// Return Value:
//   Mask of GC register kills
//
regMaskTP emitter::emitGetGCRegsKilledByNoGCCall(CorInfoHelpFunc helper)
{
    assert(emitNoGChelper(helper));
    regMaskTP result;
    switch (helper)
    {
        case CORINFO_HELP_ASSIGN_BYREF:
#if defined(TARGET_X86)
            // This helper only trashes ECX.
            result = RBM_ECX;
            break;
#elif defined(TARGET_AMD64)
            // This uses and defs RDI and RSI.
            result = RBM_CALLEE_TRASH_NOGC & ~(RBM_RDI | RBM_RSI);
            break;
#elif defined(TARGET_ARMARCH)
            result = RBM_CALLEE_GCTRASH_WRITEBARRIER_BYREF;
            break;
#else
            assert(!"unknown arch");
#endif

        case CORINFO_HELP_PROF_FCN_ENTER:
            result = RBM_PROFILER_ENTER_TRASH;
            break;

        case CORINFO_HELP_PROF_FCN_LEAVE:
#if defined(TARGET_ARM)
            // profiler scratch remains gc live
            result = RBM_PROFILER_LEAVE_TRASH & ~RBM_PROFILER_RET_SCRATCH;
#else
            result = RBM_PROFILER_LEAVE_TRASH;
#endif
            break;

        case CORINFO_HELP_PROF_FCN_TAILCALL:
            result = RBM_PROFILER_TAILCALL_TRASH;
            break;

#if defined(TARGET_ARMARCH)
        case CORINFO_HELP_ASSIGN_REF:
        case CORINFO_HELP_CHECKED_ASSIGN_REF:
            result = RBM_CALLEE_GCTRASH_WRITEBARRIER;
            break;
#endif // defined(TARGET_ARMARCH)

#if defined(TARGET_X86)
        case CORINFO_HELP_INIT_PINVOKE_FRAME:
            result = RBM_INIT_PINVOKE_FRAME_TRASH;
            break;
#endif // defined(TARGET_X86)

        default:
            result = RBM_CALLEE_TRASH_NOGC;
            break;
    }

    // compHelperCallKillSet returns a superset of the registers which values are not guranteed to be the same
    // after the call, if a register loses its GC or byref it has to be in the compHelperCallKillSet set as well.
    assert((result & emitComp->compHelperCallKillSet(helper)) == result);

    return result;
}
