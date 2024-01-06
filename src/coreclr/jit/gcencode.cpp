// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "gcinfotypes.h"
#ifndef JIT32_GCENCODER
#include "gcinfoencoder.h"
#endif
#include "patchpointinfo.h"
#include "codegen.h"
#include "jitstd/algorithm.h"

static ReturnKind GetReturnKind(const CompiledMethodInfo& info)
{
#ifdef TARGET_X86
    if (varTypeIsFloating(info.compRetType))
    {
        return RT_Float;
    }
#endif

    if (info.compRetBuffArg != BAD_VAR_NUM)
    {
        // The ABI may require to return the buffer address (a BYREF)
        // but the JIT doesn't use it.
        return RT_Scalar;
    }

    const ReturnTypeDesc& retDesc = info.retDesc;

    auto TypeToReturnKind = [](var_types type) {
        switch (type)
        {
            case TYP_REF:
                return RT_Object;
            case TYP_BYREF:
                return RT_ByRef;
            default:
                return RT_Scalar;
        }
    };

    if (retDesc.GetRegCount() == 1)
    {
        return TypeToReturnKind(info.retDesc.GetRegType(0));
    }

    if (retDesc.GetRegCount() == 2)
    {
        ReturnKind r0 = TypeToReturnKind(retDesc.GetRegType(0));
        ReturnKind r1 = TypeToReturnKind(retDesc.GetRegType(1));

        return GetStructReturnKind(r0, r1);
    }

    for (unsigned i = 0; i < retDesc.GetRegCount(); i++)
    {
        assert(!varTypeIsGC(retDesc.GetRegType(i)));
    }

    return RT_Scalar;
}

#ifdef JIT32_GCENCODER

class GCEncoder
{
    using StackSlotLifetime = GCInfo::StackSlotLifetime;
    using RegArgChangeKind  = GCInfo::RegArgChangeKind;
    using RegArgChange      = GCInfo::RegArgChange;
    using CallSite          = GCInfo::CallSite;

    CodeGen* const           codeGen;
    Compiler* const          compiler;
    unsigned const           codeSize;
    unsigned const           prologSize;
    unsigned const           epilogSize;
    ReturnKind const         returnKind;
    StackSlotLifetime* const firstStackSlotLifetime;
    RegArgChange* const      firstRegArgChange;
    CallSite* const          firstCallSite;
    bool const               isFullyInterruptible;
    unsigned                 untrackedStackSlotCount;
    unsigned                 trackedStackSlotLifetimeCount;
    unsigned                 trackedThisLclNum = BAD_VAR_NUM;
    regNumber                syncThisReg;

public:
    GCEncoder(CodeGen*           codeGen,
              unsigned           codeSize,
              unsigned           prologSize,
              unsigned           epilogSize,
              ReturnKind         returnKind,
              StackSlotLifetime* firstStackSlotLifetime,
              RegArgChange*      firstRegArgChange,
              CallSite*          firstCallSite,
              bool               isFullyInterruptible,
              regNumber          syncThisReg)
        : codeGen(codeGen)
        , compiler(codeGen->GetCompiler())
        , codeSize(codeSize)
        , prologSize(prologSize)
        , epilogSize(epilogSize)
        , returnKind(returnKind)
        , firstStackSlotLifetime(firstStackSlotLifetime)
        , firstRegArgChange(firstRegArgChange)
        , firstCallSite(firstCallSite)
        , isFullyInterruptible(isFullyInterruptible)
        , syncThisReg(syncThisReg)
    {
    }

    void CreateAndStoreGCInfo();

private:
    unsigned GetUntrackedStackSlotCount();
    unsigned GetTrackedStackSlotLifetimeCount();
    size_t MakeRegPtrTable(uint8_t* dest, int mask, size_t* pArgTabOffset);
    unsigned AddUntrackedStackSlots(uint8_t* dest, int mask);
    unsigned AddTrackedStackSlots(uint8_t* dest, int mask);
    unsigned AddFullyInterruptibleSlots(uint8_t* dest, int mask);
    unsigned AddPartiallyInterruptibleSlotsFrameless(uint8_t* dest, int mask);
    unsigned AddPartiallyInterruptibleSlotsFramed(uint8_t* dest, int mask);
    size_t PtrTableSize(size_t* pArgTabOffset);
    uint8_t* PtrTableSave(uint8_t* destPtr, size_t* pArgTabOffset);
    size_t InfoBlockHdrSave(uint8_t* dest, int mask, regMaskTP savedRegs, InfoHdr* header, int* s_cached);

#if DUMP_GC_TABLES
    size_t InfoBlockHdrDump(const uint8_t* table, InfoHdr* header, unsigned* methodSize);
    size_t DumpPtrTable(const uint8_t* table, const InfoHdr& header, unsigned methodSize);
#endif
};

void GCInfo::CreateAndStoreGCInfo(CodeGen* codeGen, unsigned codeSize, unsigned prologSize, unsigned epilogSize)
{
#ifdef FEATURE_EH_FUNCLETS
    if (compiler->ehAnyFunclets())
    {
        MarkFilterStackSlotsPinned(codeGen);
    }
#endif

    GCEncoder encoder(codeGen, codeSize, prologSize, epilogSize, GetReturnKind(compiler->info), firstStackSlotLifetime,
                      firstRegArgChange, firstCallSite, isFullyInterruptible, syncThisReg);
    encoder.CreateAndStoreGCInfo();
}

void GCEncoder::CreateAndStoreGCInfo()
{
    untrackedStackSlotCount       = GetUntrackedStackSlotCount();
    trackedStackSlotLifetimeCount = GetTrackedStackSlotLifetimeCount();

    BYTE    headerBuf[64];
    InfoHdr header;

    int s_cached;

    size_t headerSize = InfoBlockHdrSave(headerBuf, 0, codeGen->calleeSavedModifiedRegs, &header, &s_cached);

    size_t argTabOffset = 0;
    size_t ptrMapSize   = PtrTableSize(&argTabOffset);

#if DISPLAY_SIZES
    if (isFullyInterruptible)
    {
        gcHeaderISize += headerSize;
        gcPtrMapISize += ptrMapSize;
    }
    else
    {
        gcHeaderNSize += headerSize;
        gcPtrMapNSize += ptrMapSize;
    }
#endif // DISPLAY_SIZES

    size_t infoBlockSize = headerSize + ptrMapSize;

    /* Allocate the info block for the method */

    BYTE* infoBlkAddr = (BYTE*)compiler->info.compCompHnd->allocGCInfo(infoBlockSize);

    /* Fill in the info block and return it to the caller */

    void* infoPtr = infoBlkAddr;

    /* Create the method info block: header followed by GC tracking tables */

    infoBlkAddr += InfoBlockHdrSave(infoBlkAddr, -1, codeGen->calleeSavedModifiedRegs, &header, &s_cached);

    assert(infoBlkAddr == (BYTE*)infoPtr + headerSize);
    infoBlkAddr = PtrTableSave(infoBlkAddr, &argTabOffset);
    assert(infoBlkAddr == (BYTE*)infoPtr + headerSize + ptrMapSize);

#ifdef DEBUG

    if (0)
    {
        BYTE*  temp = (BYTE*)infoPtr;
        size_t size = infoBlkAddr - temp;
        BYTE*  ptab = temp + headerSize;

        noway_assert(size == headerSize + ptrMapSize);

        printf("Method info block - header [%zu bytes]:", headerSize);

        for (unsigned i = 0; i < size; i++)
        {
            if (temp == ptab)
            {
                printf("\nMethod info block - ptrtab [%u bytes]:", ptrMapSize);
                printf("\n    %04X: %*c", i & ~0xF, 3 * (i & 0xF), ' ');
            }
            else
            {
                if (!(i % 16))
                    printf("\n    %04X: ", i);
            }

            printf("%02X ", *temp++);
        }

        printf("\n");
    }

#endif // DEBUG

#if DUMP_GC_TABLES
    if (compiler->opts.dspGCtbls)
    {
        const BYTE* base = (BYTE*)infoPtr;
        size_t      size;
        unsigned    methodSize;
        InfoHdr     dumpHeader;

        printf("GC Info for method %s\n", compiler->info.compFullName);
        printf("GC info size = %3u\n", infoBlockSize);

        size = InfoBlockHdrDump(base, &dumpHeader, &methodSize);
        // printf("size of header encoding is %3u\n", size);
        printf("\n");

        base += size;
        size = DumpPtrTable(base, dumpHeader, methodSize);
        // printf("size of pointer table is %3u\n", size);
        printf("\n");
        noway_assert(infoBlkAddr == (base + size));
    }
#endif // DUMP_GC_TABLES

    /* Make sure we ended up generating the expected number of bytes */

    noway_assert(infoBlkAddr == (BYTE*)infoPtr + infoBlockSize);

    codeGen->compInfoBlkSize = infoBlockSize;
}

static unsigned char encodeUnsigned(BYTE* dest, unsigned value)
{
    unsigned char size = 1;
    unsigned      tmp  = value;
    while (tmp > 0x7F)
    {
        tmp >>= 7;
        assert(size < 6); // Invariant.
        size++;
    }
    if (dest)
    {
        // write the bytes starting at the end of dest in LSB to MSB order
        BYTE* p    = dest + size;
        BYTE  cont = 0; // The last byte has no continuation flag
        while (value > 0x7F)
        {
            *--p = cont | (value & 0x7f);
            value >>= 7;
            cont = 0x80; // Non last bytes have a continuation flag
        }
        *--p = cont | (BYTE)value; // Now write the first byte
        assert(p == dest);
    }
    return size;
}

static unsigned char encodeUDelta(BYTE* dest, unsigned value, unsigned lastValue)
{
    assert(value >= lastValue);
    return encodeUnsigned(dest, value - lastValue);
}

static unsigned char encodeSigned(BYTE* dest, int val)
{
    unsigned char size  = 1;
    unsigned      value = val;
    BYTE          neg   = 0;
    if (val < 0)
    {
        value = -val;
        neg   = 0x40;
    }
    unsigned tmp = value;
    while (tmp > 0x3F)
    {
        tmp >>= 7;
        assert(size < 16); // Definitely sufficient for unsigned.  Fits in an unsigned char, certainly.
        size++;
    }
    if (dest)
    {
        // write the bytes starting at the end of dest in LSB to MSB order
        BYTE* p    = dest + size;
        BYTE  cont = 0; // The last byte has no continuation flag
        while (value > 0x3F)
        {
            *--p = cont | (value & 0x7f);
            value >>= 7;
            cont = 0x80; // Non last bytes have a continuation flag
        }
        *--p = neg | cont | (BYTE)value; // Now write the first byte
        assert(p == dest);
    }
    return size;
}

unsigned GCEncoder::GetUntrackedStackSlotCount()
{
    unsigned int untrackedCount = 0;

    for (unsigned lclNum = 0; lclNum < compiler->lvaCount; lclNum++)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

        if (lcl->IsDependentPromotedField(compiler) || !lcl->lvOnFrame || lcl->HasGCSlotLiveness())
        {
            continue;
        }

        if (varTypeIsGC(lcl->GetType()))
        {
#ifndef FEATURE_EH_FUNCLETS
            if (compiler->lvaIsOriginalThisParam(lclNum) && compiler->lvaKeepAliveAndReportThis())
            {
                // "this" is untracked, but encoding of untracked variables does not support
                // reporting "this". So report it as a tracked variable with a liveness extending
                // over the entire method.
                //
                // TODO-x86-Cleanup: the semantic here is not clear, it would be useful to check
                // different cases and add a description where "this" is saved and how it is
                // tracked in each of them:
                // 1) when FEATURE_EH_FUNCLETS defined (x86 Linux);
                // 2) when FEATURE_EH_FUNCLETS not defined, lvaKeepAliveAndReportThis == true,
                //    compJmpOpUsed == true;
                // 3) when there is RegArgChange for "this", but keepThisAlive == true;
                // etc.

                trackedThisLclNum = lclNum;

                continue;
            }
#endif

#ifdef DEBUG
            if (compiler->verbose)
            {
                printf("GCINFO: untracked %s slot at [%s", varTypeName(lcl->GetType()),
                       codeGen->GetEmitter()->emitGetFrameReg());

                if (lcl->GetStackOffset() != 0)
                {
                    printf("%c%02XH", lcl->GetStackOffset() < 0 ? '-' : '+', abs(lcl->GetStackOffset()));
                }

                printf("]\n");
            }
#endif

            untrackedCount++;
        }
        else if (lcl->TypeIs(TYP_STRUCT))
        {
            untrackedCount += lcl->GetLayout()->GetGCPtrCount();
        }
    }

    if (!compiler->codeGen->spillTemps.TrackGCSpillTemps())
    {
        for (SpillTemp& temp : compiler->codeGen->spillTemps)
        {
            if (!varTypeIsGC(temp.GetType()))
            {
                continue;
            }

#ifdef DEBUG
            if (compiler->verbose)
            {
                printf("GCINFO: untracked %s slot at [%s", varTypeName(temp.GetType()),
                       codeGen->GetEmitter()->emitGetFrameReg());

                if (temp.GetOffset() != 0)
                {
                    printf("%c%02XH", temp.GetOffset() < 0 ? '-' : '+', abs(temp.GetOffset()));
                }

                printf("]\n");
            }
#endif

            untrackedCount++;
        }
    }

    JITDUMP("GCINFO: untrckVars = %u\n", untrackedCount);

    return untrackedCount;
}

unsigned GCEncoder::GetTrackedStackSlotLifetimeCount()
{
    unsigned stackSlotLifetimeCount = 0;

    if (trackedThisLclNum != BAD_VAR_NUM)
    {
        stackSlotLifetimeCount++;
    }

    for (StackSlotLifetime* lifetime = firstStackSlotLifetime; lifetime != nullptr; lifetime = lifetime->next)
    {
        if (lifetime->beginCodeOffs == lifetime->endCodeOffs)
        {
            continue;
        }

        stackSlotLifetimeCount++;
    }

    JITDUMP("GCINFO: trackdLcls = %u\n", stackSlotLifetimeCount);

    return stackSlotLifetimeCount;
}

BYTE* GCEncoder::PtrTableSave(uint8_t* destPtr, size_t* argTabOffset)
{
    return destPtr + MakeRegPtrTable(destPtr, -1, argTabOffset);
}

// (see jit.h) #define REGEN_SHORTCUTS 0
// To Regenerate the compressed info header shortcuts, define REGEN_SHORTCUTS
// and use the following command line pipe/filter to give you the 128
// most useful encodings.
//
// find . -name regen.txt | xargs cat | grep InfoHdr | sort | uniq -c | sort -r | head -128

// (see jit.h) #define REGEN_CALLPAT 0
// To Regenerate the compressed info header shortcuts, define REGEN_CALLPAT
// and use the following command line pipe/filter to give you the 80
// most useful encodings.
//
// find . -name regen.txt | xargs cat | grep CallSite | sort | uniq -c | sort -r | head -80

#if REGEN_SHORTCUTS || REGEN_CALLPAT
static FILE*     logFile = NULL;
CRITICAL_SECTION logFileLock;
#endif

#if REGEN_CALLPAT
static void regenLog(unsigned codeDelta,
                     unsigned argMask,
                     unsigned regMask,
                     unsigned argCnt,
                     unsigned byrefArgMask,
                     unsigned byrefRegMask,
                     BYTE*    base,
                     unsigned enSize)
{
    CallPattern pat;

    pat.fld.argCnt    = (argCnt < 0xff) ? argCnt : 0xff;
    pat.fld.regMask   = (regMask < 0xff) ? regMask : 0xff;
    pat.fld.argMask   = (argMask < 0xff) ? argMask : 0xff;
    pat.fld.codeDelta = (codeDelta < 0xff) ? codeDelta : 0xff;

    if (logFile == NULL)
    {
        logFile = fopen("regen.txt", "a");
        InitializeCriticalSection(&logFileLock);
    }

    assert(((enSize > 0) && (enSize < 256)) && ((pat.val & 0xffffff) != 0xffffff));

    EnterCriticalSection(&logFileLock);

    fprintf(logFile, "CallSite( 0x%08x, 0x%02x%02x, 0x", pat.val, byrefArgMask, byrefRegMask);

    while (enSize > 0)
    {
        fprintf(logFile, "%02x", *base++);
        enSize--;
    }
    fprintf(logFile, "),\n");
    fflush(logFile);

    LeaveCriticalSection(&logFileLock);
}
#endif

#if REGEN_SHORTCUTS
static void regenLog(unsigned encoding, InfoHdr* header, InfoHdr* state)
{
    if (logFile == NULL)
    {
        logFile = fopen("regen.txt", "a");
        InitializeCriticalSection(&logFileLock);
    }

    EnterCriticalSection(&logFileLock);

    fprintf(logFile, "InfoHdr( %2d, %2d, %1d, %1d, %1d,"
                     " %1d, %1d, %1d, %1d, %1d,"
                     " %1d, %1d, %1d, %1d, %1d, %1d,"
                     " %1d, %1d, %1d,"
                     " %1d, %2d, %2d,"
                     " %2d, %2d, %2d, %2d, %2d, %2d), \n",
            state->prologSize, state->epilogSize, state->epilogCount, state->epilogAtEnd, state->ediSaved,
            state->esiSaved, state->ebxSaved, state->ebpSaved, state->ebpFrame, state->interruptible,
            state->doubleAlign, state->security, state->handlers, state->localloc, state->editNcontinue, state->varargs,
            state->profCallbacks, state->genericsContext, state->genericsContextIsMethodDesc, state->returnKind,
            state->argCount, state->frameSize,
            (state->untrackedCnt <= SET_UNTRACKED_MAX) ? state->untrackedCnt : HAS_UNTRACKED,
            (state->varPtrTableSize == 0) ? 0 : HAS_VARPTR,
            (state->gsCookieOffset == INVALID_GS_COOKIE_OFFSET) ? 0 : HAS_GS_COOKIE_OFFSET,
            (state->syncStartOffset == INVALID_SYNC_OFFSET) ? 0 : HAS_SYNC_OFFSET,
            (state->syncStartOffset == INVALID_SYNC_OFFSET) ? 0 : HAS_SYNC_OFFSET,
            (state->revPInvokeOffset == INVALID_REV_PINVOKE_OFFSET) ? 0 : HAS_REV_PINVOKE_FRAME_OFFSET);

    fflush(logFile);

    LeaveCriticalSection(&logFileLock);
}
#endif

#ifdef _MSC_VER
// TODO-MIKE-Cleanup: The following code has a ton of warnings caused by
// implicit conversion from int to char/short, it might be nice to fix it
// instead of disabling the warning.
#pragma warning(disable : 4244)
#endif

// Given the four parameters return the index into the callPatternTable[]
// that is used to encoding these four items.  If an exact match cannot
// found then ignore the codeDelta and search the table again for a near
// match.
// Returns 0..79 for an exact match or (delta<<8) | (0..79) for a near match.
// A near match will be encoded using two bytes, the first byte will
// skip the adjustment delta that prevented an exact match and the
// rest of the delta plus the other three items are encoded in the
// second byte.
static int LookupCallPattern(unsigned argCnt, unsigned regMask, unsigned argMask, unsigned codeDelta)
{
    if ((argCnt <= CP_MAX_ARG_CNT) && (argMask <= CP_MAX_ARG_MASK))
    {
        CallPattern pat;

        pat.fld.argCnt    = argCnt;
        pat.fld.regMask   = regMask; // EBP,EBX,ESI,EDI
        pat.fld.argMask   = argMask;
        pat.fld.codeDelta = codeDelta;

        bool     codeDeltaOK = (pat.fld.codeDelta == codeDelta);
        unsigned bestDelta2  = 0xff;
        unsigned bestPattern = 0xff;
        unsigned patval      = pat.val;
        assert(sizeof(CallPattern) == sizeof(unsigned));

        const unsigned* curp = &callPatternTable[0];
        for (unsigned inx = 0; inx < 80; inx++, curp++)
        {
            unsigned curval = *curp;
            if ((patval == curval) && codeDeltaOK)
                return inx;

            if (((patval ^ curval) & 0xffffff) == 0)
            {
                unsigned delta2 = codeDelta - (curval >> 24);
                if (delta2 < bestDelta2)
                {
                    bestDelta2  = delta2;
                    bestPattern = inx;
                }
            }
        }

        if (bestPattern != 0xff)
        {
            return (bestDelta2 << 8) | bestPattern;
        }
    }
    return -1;
}

static bool InitNeeded3(unsigned cur, unsigned tgt, unsigned max, unsigned* hint)
{
    assert(cur != tgt);

    unsigned tmp = tgt;
    unsigned nib = 0;
    unsigned cnt = 0;

    while (tmp > max)
    {
        nib = tmp & 0x07;
        tmp >>= 3;
        if (tmp == cur)
        {
            *hint = nib;
            return false;
        }
        cnt++;
    }

    *hint = tmp;
    return true;
}

static bool InitNeeded4(unsigned cur, unsigned tgt, unsigned max, unsigned* hint)
{
    assert(cur != tgt);

    unsigned tmp = tgt;
    unsigned nib = 0;
    unsigned cnt = 0;

    while (tmp > max)
    {
        nib = tmp & 0x0f;
        tmp >>= 4;
        if (tmp == cur)
        {
            *hint = nib;
            return false;
        }
        cnt++;
    }

    *hint = tmp;
    return true;
}

static int BigEncoding3(unsigned cur, unsigned tgt, unsigned max)
{
    assert(cur != tgt);

    unsigned tmp = tgt;
    unsigned nib = 0;
    unsigned cnt = 0;

    while (tmp > max)
    {
        nib = tmp & 0x07;
        tmp >>= 3;
        if (tmp == cur)
            break;
        cnt++;
    }
    return cnt;
}

static int BigEncoding4(unsigned cur, unsigned tgt, unsigned max)
{
    assert(cur != tgt);

    unsigned tmp = tgt;
    unsigned nib = 0;
    unsigned cnt = 0;

    while (tmp > max)
    {
        nib = tmp & 0x0f;
        tmp >>= 4;
        if (tmp == cur)
            break;
        cnt++;
    }
    return cnt;
}

static BYTE EncodeHeaderNext(const InfoHdr& header, InfoHdr* state, BYTE& codeSet)
{
    BYTE encoding = 0xff;
    codeSet       = 1; // codeSet is 1 or 2, depending on whether the returned encoding
                       // corresponds to InfoHdrAdjust, or InfoHdrAdjust2 enumerations.

    if (state->argCount != header.argCount)
    {
        // We have one-byte encodings for 0..8
        if (header.argCount <= SET_ARGCOUNT_MAX)
        {
            state->argCount = header.argCount;
            encoding        = SET_ARGCOUNT + header.argCount;
            goto DO_RETURN;
        }
        else
        {
            unsigned hint;
            if (InitNeeded4(state->argCount, header.argCount, SET_ARGCOUNT_MAX, &hint))
            {
                assert(hint <= SET_ARGCOUNT_MAX);
                state->argCount = hint;
                encoding        = SET_ARGCOUNT + hint;
                goto DO_RETURN;
            }
            else
            {
                assert(hint <= 0xf);
                state->argCount <<= 4;
                state->argCount += hint;
                encoding = NEXT_FOUR_ARGCOUNT + hint;
                goto DO_RETURN;
            }
        }
    }

    if (state->frameSize != header.frameSize)
    {
        // We have one-byte encodings for 0..7
        if (header.frameSize <= SET_FRAMESIZE_MAX)
        {
            state->frameSize = header.frameSize;
            encoding         = SET_FRAMESIZE + header.frameSize;
            goto DO_RETURN;
        }
        else
        {
            unsigned hint;
            if (InitNeeded4(state->frameSize, header.frameSize, SET_FRAMESIZE_MAX, &hint))
            {
                assert(hint <= SET_FRAMESIZE_MAX);
                state->frameSize = hint;
                encoding         = SET_FRAMESIZE + hint;
                goto DO_RETURN;
            }
            else
            {
                assert(hint <= 0xf);
                state->frameSize <<= 4;
                state->frameSize += hint;
                encoding = NEXT_FOUR_FRAMESIZE + hint;
                goto DO_RETURN;
            }
        }
    }

    if ((state->epilogCount != header.epilogCount) || (state->epilogAtEnd != header.epilogAtEnd))
    {
        if (header.epilogCount > SET_EPILOGCNT_MAX)
            IMPL_LIMITATION("More than SET_EPILOGCNT_MAX epilogs");

        state->epilogCount = header.epilogCount;
        state->epilogAtEnd = header.epilogAtEnd;
        encoding           = SET_EPILOGCNT + header.epilogCount * 2;
        if (header.epilogAtEnd)
            encoding++;
        goto DO_RETURN;
    }

    if (state->varPtrTableSize != header.varPtrTableSize)
    {
        assert(state->varPtrTableSize == 0 || state->varPtrTableSize == HAS_VARPTR);

        if (state->varPtrTableSize == 0)
        {
            state->varPtrTableSize = HAS_VARPTR;
            encoding               = FLIP_VAR_PTR_TABLE_SZ;
            goto DO_RETURN;
        }
        else if (header.varPtrTableSize == 0)
        {
            state->varPtrTableSize = 0;
            encoding               = FLIP_VAR_PTR_TABLE_SZ;
            goto DO_RETURN;
        }
    }

    if (state->untrackedCnt != header.untrackedCnt)
    {
        assert(state->untrackedCnt <= SET_UNTRACKED_MAX || state->untrackedCnt == HAS_UNTRACKED);

        // We have one-byte encodings for 0..3
        if (header.untrackedCnt <= SET_UNTRACKED_MAX)
        {
            state->untrackedCnt = header.untrackedCnt;
            encoding            = SET_UNTRACKED + header.untrackedCnt;
            goto DO_RETURN;
        }
        else if (state->untrackedCnt != HAS_UNTRACKED)
        {
            state->untrackedCnt = HAS_UNTRACKED;
            encoding            = FFFF_UNTRACKED_CNT;
            goto DO_RETURN;
        }
    }

    if (state->epilogSize != header.epilogSize)
    {
        // We have one-byte encodings for 0..10
        if (header.epilogSize <= SET_EPILOGSIZE_MAX)
        {
            state->epilogSize = header.epilogSize;
            encoding          = SET_EPILOGSIZE + header.epilogSize;
            goto DO_RETURN;
        }
        else
        {
            unsigned hint;
            if (InitNeeded3(state->epilogSize, header.epilogSize, SET_EPILOGSIZE_MAX, &hint))
            {
                assert(hint <= SET_EPILOGSIZE_MAX);
                state->epilogSize = hint;
                encoding          = SET_EPILOGSIZE + hint;
                goto DO_RETURN;
            }
            else
            {
                assert(hint <= 0x7);
                state->epilogSize <<= 3;
                state->epilogSize += hint;
                encoding = NEXT_THREE_EPILOGSIZE + hint;
                goto DO_RETURN;
            }
        }
    }

    if (state->prologSize != header.prologSize)
    {
        // We have one-byte encodings for 0..16
        if (header.prologSize <= SET_PROLOGSIZE_MAX)
        {
            state->prologSize = header.prologSize;
            encoding          = SET_PROLOGSIZE + header.prologSize;
            goto DO_RETURN;
        }
        else
        {
            unsigned hint;
            assert(SET_PROLOGSIZE_MAX > 15);
            if (InitNeeded3(state->prologSize, header.prologSize, 15, &hint))
            {
                assert(hint <= 15);
                state->prologSize = hint;
                encoding          = SET_PROLOGSIZE + hint;
                goto DO_RETURN;
            }
            else
            {
                assert(hint <= 0x7);
                state->prologSize <<= 3;
                state->prologSize += hint;
                encoding = NEXT_THREE_PROLOGSIZE + hint;
                goto DO_RETURN;
            }
        }
    }

    if (state->ediSaved != header.ediSaved)
    {
        state->ediSaved = header.ediSaved;
        encoding        = FLIP_EDI_SAVED;
        goto DO_RETURN;
    }

    if (state->esiSaved != header.esiSaved)
    {
        state->esiSaved = header.esiSaved;
        encoding        = FLIP_ESI_SAVED;
        goto DO_RETURN;
    }

    if (state->ebxSaved != header.ebxSaved)
    {
        state->ebxSaved = header.ebxSaved;
        encoding        = FLIP_EBX_SAVED;
        goto DO_RETURN;
    }

    if (state->ebpSaved != header.ebpSaved)
    {
        state->ebpSaved = header.ebpSaved;
        encoding        = FLIP_EBP_SAVED;
        goto DO_RETURN;
    }

    if (state->ebpFrame != header.ebpFrame)
    {
        state->ebpFrame = header.ebpFrame;
        encoding        = FLIP_EBP_FRAME;
        goto DO_RETURN;
    }

    if (state->interruptible != header.interruptible)
    {
        state->interruptible = header.interruptible;
        encoding             = FLIP_INTERRUPTIBLE;
        goto DO_RETURN;
    }

#if DOUBLE_ALIGN
    if (state->doubleAlign != header.doubleAlign)
    {
        state->doubleAlign = header.doubleAlign;
        encoding           = FLIP_DOUBLE_ALIGN;
        goto DO_RETURN;
    }
#endif

    if (state->security != header.security)
    {
        state->security = header.security;
        encoding        = FLIP_SECURITY;
        goto DO_RETURN;
    }

    if (state->handlers != header.handlers)
    {
        state->handlers = header.handlers;
        encoding        = FLIP_HANDLERS;
        goto DO_RETURN;
    }

    if (state->localloc != header.localloc)
    {
        state->localloc = header.localloc;
        encoding        = FLIP_LOCALLOC;
        goto DO_RETURN;
    }

    if (state->editNcontinue != header.editNcontinue)
    {
        state->editNcontinue = header.editNcontinue;
        encoding             = FLIP_EDITnCONTINUE;
        goto DO_RETURN;
    }

    if (state->varargs != header.varargs)
    {
        state->varargs = header.varargs;
        encoding       = FLIP_VARARGS;
        goto DO_RETURN;
    }

    if (state->profCallbacks != header.profCallbacks)
    {
        state->profCallbacks = header.profCallbacks;
        encoding             = FLIP_PROF_CALLBACKS;
        goto DO_RETURN;
    }

    if (state->genericsContext != header.genericsContext)
    {
        state->genericsContext = header.genericsContext;
        encoding               = FLIP_HAS_GENERICS_CONTEXT;
        goto DO_RETURN;
    }

    if (state->genericsContextIsMethodDesc != header.genericsContextIsMethodDesc)
    {
        state->genericsContextIsMethodDesc = header.genericsContextIsMethodDesc;
        encoding                           = FLIP_GENERICS_CONTEXT_IS_METHODDESC;
        goto DO_RETURN;
    }

    if (state->returnKind != header.returnKind)
    {
        state->returnKind = header.returnKind;
        codeSet           = 2; // Two byte encoding
        encoding          = header.returnKind;
        _ASSERTE(encoding < SET_RET_KIND_MAX);
        goto DO_RETURN;
    }

    if (state->gsCookieOffset != header.gsCookieOffset)
    {
        assert(state->gsCookieOffset == INVALID_GS_COOKIE_OFFSET || state->gsCookieOffset == HAS_GS_COOKIE_OFFSET);

        if (state->gsCookieOffset == INVALID_GS_COOKIE_OFFSET)
        {
            // header.gsCookieOffset is non-zero. We can set it
            // to zero using FLIP_HAS_GS_COOKIE
            state->gsCookieOffset = HAS_GS_COOKIE_OFFSET;
            encoding              = FLIP_HAS_GS_COOKIE;
            goto DO_RETURN;
        }
        else if (header.gsCookieOffset == INVALID_GS_COOKIE_OFFSET)
        {
            state->gsCookieOffset = INVALID_GS_COOKIE_OFFSET;
            encoding              = FLIP_HAS_GS_COOKIE;
            goto DO_RETURN;
        }
    }

    if (state->syncStartOffset != header.syncStartOffset)
    {
        assert(state->syncStartOffset == INVALID_SYNC_OFFSET || state->syncStartOffset == HAS_SYNC_OFFSET);

        if (state->syncStartOffset == INVALID_SYNC_OFFSET)
        {
            // header.syncStartOffset is non-zero. We can set it
            // to zero using FLIP_SYNC
            state->syncStartOffset = HAS_SYNC_OFFSET;
            encoding               = FLIP_SYNC;
            goto DO_RETURN;
        }
        else if (header.syncStartOffset == INVALID_SYNC_OFFSET)
        {
            state->syncStartOffset = INVALID_SYNC_OFFSET;
            encoding               = FLIP_SYNC;
            goto DO_RETURN;
        }
    }

    if (state->revPInvokeOffset != header.revPInvokeOffset)
    {
        assert(state->revPInvokeOffset == INVALID_REV_PINVOKE_OFFSET ||
               state->revPInvokeOffset == HAS_REV_PINVOKE_FRAME_OFFSET);

        if (state->revPInvokeOffset == INVALID_REV_PINVOKE_OFFSET)
        {
            // header.revPInvokeOffset is non-zero.
            state->revPInvokeOffset = HAS_REV_PINVOKE_FRAME_OFFSET;
            encoding                = FLIP_REV_PINVOKE_FRAME;
            goto DO_RETURN;
        }
        else if (header.revPInvokeOffset == INVALID_REV_PINVOKE_OFFSET)
        {
            state->revPInvokeOffset = INVALID_REV_PINVOKE_OFFSET;
            encoding                = FLIP_REV_PINVOKE_FRAME;
            goto DO_RETURN;
        }
    }

DO_RETURN:
    _ASSERTE(encoding < MORE_BYTES_TO_FOLLOW);
    if (!state->isHeaderMatch(header))
        encoding |= MORE_BYTES_TO_FOLLOW;

    return encoding;
}

static int MeasureDistance(const InfoHdr& header, const InfoHdrSmall* p, int closeness)
{
    int distance = 0;

    if (p->untrackedCnt != header.untrackedCnt)
    {
        if (header.untrackedCnt > 3)
        {
            if (p->untrackedCnt != HAS_UNTRACKED)
                distance += 1;
        }
        else
        {
            distance += 1;
        }
        if (distance >= closeness)
            return distance;
    }

    if (p->varPtrTableSize != header.varPtrTableSize)
    {
        if (header.varPtrTableSize != 0)
        {
            if (p->varPtrTableSize != HAS_VARPTR)
                distance += 1;
        }
        else
        {
            assert(p->varPtrTableSize == HAS_VARPTR);
            distance += 1;
        }
        if (distance >= closeness)
            return distance;
    }

    if (p->frameSize != header.frameSize)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;

        // We have one-byte encodings for 0..7
        if (header.frameSize > SET_FRAMESIZE_MAX)
        {
            distance += BigEncoding4(p->frameSize, header.frameSize, SET_FRAMESIZE_MAX);
            if (distance >= closeness)
                return distance;
        }
    }

    if (p->argCount != header.argCount)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;

        // We have one-byte encodings for 0..8
        if (header.argCount > SET_ARGCOUNT_MAX)
        {
            distance += BigEncoding4(p->argCount, header.argCount, SET_ARGCOUNT_MAX);
            if (distance >= closeness)
                return distance;
        }
    }

    if (p->prologSize != header.prologSize)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;

        // We have one-byte encodings for 0..16
        if (header.prologSize > SET_PROLOGSIZE_MAX)
        {
            assert(SET_PROLOGSIZE_MAX > 15);
            distance += BigEncoding3(p->prologSize, header.prologSize, 15);
            if (distance >= closeness)
                return distance;
        }
    }

    if (p->epilogSize != header.epilogSize)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
        // We have one-byte encodings for 0..10
        if (header.epilogSize > SET_EPILOGSIZE_MAX)
        {
            distance += BigEncoding3(p->epilogSize, header.epilogSize, SET_EPILOGSIZE_MAX);
            if (distance >= closeness)
                return distance;
        }
    }

    if ((p->epilogCount != header.epilogCount) || (p->epilogAtEnd != header.epilogAtEnd))
    {
        distance += 1;
        if (distance >= closeness)
            return distance;

        if (header.epilogCount > SET_EPILOGCNT_MAX)
            IMPL_LIMITATION("More than SET_EPILOGCNT_MAX epilogs");
    }

    if (p->ediSaved != header.ediSaved)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    if (p->esiSaved != header.esiSaved)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    if (p->ebxSaved != header.ebxSaved)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    if (p->ebpSaved != header.ebpSaved)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    if (p->ebpFrame != header.ebpFrame)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    if (p->interruptible != header.interruptible)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

#if DOUBLE_ALIGN
    if (p->doubleAlign != header.doubleAlign)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }
#endif

    if (p->security != header.security)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    if (p->handlers != header.handlers)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    if (p->localloc != header.localloc)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    if (p->editNcontinue != header.editNcontinue)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    if (p->varargs != header.varargs)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    if (p->profCallbacks != header.profCallbacks)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    if (p->genericsContext != header.genericsContext)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    if (p->genericsContextIsMethodDesc != header.genericsContextIsMethodDesc)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    if (p->returnKind != header.returnKind)
    {
        // Setting the ReturnKind requires two bytes of encoding.
        distance += 2;
        if (distance >= closeness)
            return distance;
    }

    if (header.gsCookieOffset != INVALID_GS_COOKIE_OFFSET)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    if (header.syncStartOffset != INVALID_SYNC_OFFSET)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    if (header.revPInvokeOffset != INVALID_REV_PINVOKE_OFFSET)
    {
        distance += 1;
        if (distance >= closeness)
            return distance;
    }

    return distance;
}

static int InfoHdrLookup[IH_MAX_PROLOG_SIZE + 2];

void InitGCEncoderLookupTable()
{
    const InfoHdrSmall* p  = &infoHdrShortcut[0];
    int                 lo = -1;
    int                 hi = 0;
    int                 n;

    for (n = 0; n < 128; n++, p++)
    {
        if (p->prologSize != lo)
        {
            if (p->prologSize < lo)
            {
                assert(p->prologSize == 0);
                hi = IH_MAX_PROLOG_SIZE;
            }
            else
                hi = p->prologSize;

            assert(hi <= IH_MAX_PROLOG_SIZE);

            while (lo < hi)
                InfoHdrLookup[++lo] = n;

            if (lo == IH_MAX_PROLOG_SIZE)
                break;
        }
    }

    assert(lo == IH_MAX_PROLOG_SIZE);
    assert(InfoHdrLookup[IH_MAX_PROLOG_SIZE] < 128);

    while (p->prologSize == lo)
    {
        n++;
        if (n >= 128)
            break;
        p++;
    }

    InfoHdrLookup[++lo] = n;

#ifdef DEBUG
    //
    // We do some other DEBUG only validity checks here
    //
    assert(callCommonDelta[0] < callCommonDelta[1]);
    assert(callCommonDelta[1] < callCommonDelta[2]);
    assert(callCommonDelta[2] < callCommonDelta[3]);
    assert(sizeof(CallPattern) == sizeof(unsigned));
    unsigned maxMarks = 0;
    for (unsigned inx = 0; inx < 80; inx++)
    {
        CallPattern pat;
        pat.val = callPatternTable[inx];

        assert(pat.fld.codeDelta <= CP_MAX_CODE_DELTA);
        if (pat.fld.codeDelta == CP_MAX_CODE_DELTA)
            maxMarks |= 0x01;

        assert(pat.fld.argCnt <= CP_MAX_ARG_CNT);
        if (pat.fld.argCnt == CP_MAX_ARG_CNT)
            maxMarks |= 0x02;

        assert(pat.fld.argMask <= CP_MAX_ARG_MASK);
        if (pat.fld.argMask == CP_MAX_ARG_MASK)
            maxMarks |= 0x04;
    }
    assert(maxMarks == 0x07);
#endif
}

constexpr int NO_CACHED_HEADER = -1;

static BYTE EncodeHeaderFirst(const InfoHdr& header, InfoHdr* state, int* more, int* pCached)
{
    // First try the cached value for an exact match, if there is one
    //
    int                 n = *pCached;
    const InfoHdrSmall* p;

    if (n != NO_CACHED_HEADER)
    {
        p = &infoHdrShortcut[n];
        if (p->isHeaderMatch(header))
        {
            // exact match found
            GetInfoHdr(n, state);
            *more = 0;
            return n;
        }
    }

    // Next search the table for an exact match
    // Only search entries that have a matching prolog size
    // Note: lo and hi are saved here as they specify the
    // range of entries that have the correct prolog size
    //
    unsigned psz = header.prologSize;
    int      lo  = 0;
    int      hi  = 0;

    if (psz <= IH_MAX_PROLOG_SIZE)
    {
        lo = InfoHdrLookup[psz];
        hi = InfoHdrLookup[psz + 1];
        p  = &infoHdrShortcut[lo];
        for (n = lo; n < hi; n++, p++)
        {
            assert(psz == p->prologSize);
            if (p->isHeaderMatch(header))
            {
                // exact match found
                GetInfoHdr(n, state);
                *pCached = n; // cache the value
                *more    = 0;
                return n;
            }
        }
    }

    //
    // no exact match in infoHdrShortcut[]
    //
    // find the nearest entry in the table
    //
    int nearest   = -1;
    int closeness = 255; // (i.e. not very close)

    //
    // Calculate the minimum acceptable distance
    // if we find an entry that is at least this close
    // we will stop the search and use that value
    //
    int min_acceptable_distance = 1;

    if (header.frameSize > SET_FRAMESIZE_MAX)
    {
        ++min_acceptable_distance;
        if (header.frameSize > 32)
            ++min_acceptable_distance;
    }
    if (header.argCount > SET_ARGCOUNT_MAX)
    {
        ++min_acceptable_distance;
        if (header.argCount > 32)
            ++min_acceptable_distance;
    }

    // First try the cached value
    // and see if it meets the minimum acceptable distance
    //
    if (*pCached != NO_CACHED_HEADER)
    {
        p            = &infoHdrShortcut[*pCached];
        int distance = MeasureDistance(header, p, closeness);
        assert(distance > 0);
        if (distance <= min_acceptable_distance)
        {
            GetInfoHdr(*pCached, state);
            *more = distance;
            return 0x80 | *pCached;
        }
        else
        {
            closeness = distance;
            nearest   = *pCached;
        }
    }

    // Then try the ones pointed to by [lo..hi),
    // (i.e. the ones that have the correct prolog size)
    //
    p = &infoHdrShortcut[lo];
    for (n = lo; n < hi; n++, p++)
    {
        if (n == *pCached)
            continue; // already tried this one
        int distance = MeasureDistance(header, p, closeness);
        assert(distance > 0);
        if (distance <= min_acceptable_distance)
        {
            GetInfoHdr(n, state);
            *pCached = n; // Cache this value
            *more    = distance;
            return 0x80 | n;
        }
        else if (distance < closeness)
        {
            closeness = distance;
            nearest   = n;
        }
    }

    int last = InfoHdrLookup[IH_MAX_PROLOG_SIZE + 1];
    assert(last <= 128);

    // Then try all the rest [0..last-1]
    p = &infoHdrShortcut[0];
    for (n = 0; n < last; n++, p++)
    {
        if (n == *pCached)
            continue; // already tried this one
        if ((n >= lo) && (n < hi))
            continue; // already tried these
        int distance = MeasureDistance(header, p, closeness);
        assert(distance > 0);
        if (distance <= min_acceptable_distance)
        {
            GetInfoHdr(n, state);
            *pCached = n; // Cache this value
            *more    = distance;
            return 0x80 | n;
        }
        else if (distance < closeness)
        {
            closeness = distance;
            nearest   = n;
        }
    }

    //
    // If we reach here then there was no adjacent neighbor
    //  in infoHdrShortcut[], closeness indicate how many extra
    //  bytes we will need to encode this item.
    //
    assert((nearest >= 0) && (nearest <= 127));
    GetInfoHdr(nearest, state);
    *pCached = nearest; // Cache this value
    *more    = closeness;
    return 0x80 | nearest;
}

// Write the initial part of the method info block. This is called twice;
// first to compute the size needed for the info (mask=0), the second time
// to actually generate the contents of the table (mask=-1,dest!=NULL).
size_t GCEncoder::InfoBlockHdrSave(BYTE* dest, int mask, regMaskTP savedRegs, InfoHdr* header, int* pCached)
{
    JITDUMP("*************** In InfoBlockHdrSave()\n");

    size_t size = 0;

#if VERIFY_GC_TABLES
    *castto(dest, unsigned short*)++ = 0xFEEF;
    size += sizeof(short);
#endif

    /* Write the method size first (using between 1 and 5 bytes) */
    CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef DEBUG
    if (compiler->verbose && (mask != 0))
    {
        printf("GCINFO: methodSize = %04X\n", codeSize);
        printf("GCINFO: prologSize = %04X\n", prologSize);
        printf("GCINFO: epilogSize = %04X\n", epilogSize);
    }
#endif

    size_t methSz = encodeUnsigned(dest, codeSize);
    size += methSz;
    dest += methSz & mask;

    //
    // New style InfoBlk Header
    //
    // Typically only uses one-byte to store everything.
    //

    if (mask == 0)
    {
        memset(header, 0, sizeof(InfoHdr));
        *pCached = NO_CACHED_HEADER;
    }

    assert(FitsIn<unsigned char>(prologSize));
    header->prologSize = static_cast<unsigned char>(prologSize);

    assert(FitsIn<unsigned char>(epilogSize));
    header->epilogSize = static_cast<unsigned char>(epilogSize);

    header->epilogCount = codeGen->GetEmitter()->emitGetEpilogCnt();
    if (header->epilogCount != codeGen->GetEmitter()->emitGetEpilogCnt())
    {
        IMPL_LIMITATION("emitGetEpilogCnt() does not fit in InfoHdr::epilogCount");
    }

    header->epilogAtEnd = codeGen->GetEmitter()->emitHasEpilogEnd();

    if ((savedRegs & RBM_EDI) != RBM_NONE)
    {
        header->ediSaved = 1;
    }

    if ((savedRegs & RBM_ESI) != RBM_NONE)
    {
        header->esiSaved = 1;
    }

    if ((savedRegs & RBM_EBX) != RBM_NONE)
    {
        header->ebxSaved = 1;
    }

    header->interruptible = isFullyInterruptible;

    if (!compiler->codeGen->isFramePointerUsed())
    {
#if DOUBLE_ALIGN
        if (compiler->codeGen->doDoubleAlign())
        {
            header->ebpSaved = true;
            assert((savedRegs & RBM_EBP) == RBM_NONE);
        }
#endif
        if ((savedRegs & RBM_EBP) != RBM_NONE)
        {
            header->ebpSaved = true;
        }
    }
    else
    {
        header->ebpSaved = true;
        header->ebpFrame = true;
    }

#if DOUBLE_ALIGN
    header->doubleAlign = compiler->codeGen->doDoubleAlign();
#endif

    header->security = false;

    header->handlers = compiler->ehHasCallableHandlers();
    header->localloc = compiler->compLocallocUsed;

    header->varargs         = compiler->info.compIsVarArgs;
    header->profCallbacks   = compiler->info.compProfilerCallback;
    header->editNcontinue   = compiler->opts.compDbgEnC;
    header->genericsContext = compiler->lvaReportParamTypeArg();
    header->genericsContextIsMethodDesc =
        (header->genericsContext != 0) &&
        ((compiler->info.compMethodInfo->options & CORINFO_GENERICS_CTXT_FROM_METHODDESC) != 0);

    _ASSERTE(IsValidReturnKind(returnKind) && "Return Kind must be valid");
    _ASSERTE(!IsStructReturnKind(returnKind) && "Struct Return Kinds Unexpected for JIT32");
    _ASSERTE(((int)returnKind < (int)SET_RET_KIND_MAX) && "ReturnKind has no legal encoding");
    header->returnKind = returnKind;

    header->gsCookieOffset = INVALID_GS_COOKIE_OFFSET;
    if (compiler->getNeedsGSSecurityCookie())
    {
        assert(compiler->lvaGSSecurityCookie != BAD_VAR_NUM);
        int stkOffs            = compiler->lvaTable[compiler->lvaGSSecurityCookie].GetStackOffset();
        header->gsCookieOffset = codeGen->isFramePointerUsed() ? -stkOffs : stkOffs;
        assert(header->gsCookieOffset != INVALID_GS_COOKIE_OFFSET);
    }

    header->syncStartOffset = INVALID_SYNC_OFFSET;
    header->syncEndOffset   = INVALID_SYNC_OFFSET;
#ifndef UNIX_X86_ABI
    // JIT is responsible for synchronization on funclet-based EH model that x86/Linux uses.
    if (compiler->info.compFlags & CORINFO_FLG_SYNCH)
    {
        assert(codeGen->syncStartEmitCookie != NULL);
        header->syncStartOffset = codeGen->syncStartEmitCookie->GetCodeOffset();
        assert(header->syncStartOffset != INVALID_SYNC_OFFSET);

        assert(codeGen->syncEndEmitCookie != NULL);
        header->syncEndOffset = codeGen->syncEndEmitCookie->GetCodeOffset();
        assert(header->syncEndOffset != INVALID_SYNC_OFFSET);

        assert(header->syncStartOffset < header->syncEndOffset);
        // synchronized methods can't have more than 1 epilog
        assert(header->epilogCount <= 1);
    }
#endif

    header->revPInvokeOffset = INVALID_REV_PINVOKE_OFFSET;
    if (compiler->opts.IsReversePInvoke())
    {
        assert(compiler->lvaReversePInvokeFrameVar != BAD_VAR_NUM);
        int stkOffs              = compiler->lvaTable[compiler->lvaReversePInvokeFrameVar].GetStackOffset();
        header->revPInvokeOffset = codeGen->isFramePointerUsed() ? -stkOffs : stkOffs;
        assert(header->revPInvokeOffset != INVALID_REV_PINVOKE_OFFSET);
    }

    assert(compiler->codeGen->paramsStackSize % REGSIZE_BYTES == 0);
    unsigned argCount = codeGen->paramsStackSize / REGSIZE_BYTES;
    assert(argCount <= UINT16_MAX);
    header->argCount = static_cast<uint16_t>(argCount);

    assert(compiler->codeGen->lclFrameSize % REGSIZE_BYTES == 0);
    header->frameSize = codeGen->lclFrameSize / 4;

    if (mask == 0)
    {
        header->untrackedCnt    = untrackedStackSlotCount;
        header->varPtrTableSize = trackedStackSlotLifetimeCount;
    }

    //
    // If the high-order bit of headerEncoding is set
    // then additional bytes will update the InfoHdr state
    // until the fully state is encoded
    //
    InfoHdr state;
    int     more           = 0;
    BYTE    headerEncoding = EncodeHeaderFirst(*header, &state, &more, pCached);
    ++size;
    if (mask)
    {
#if REGEN_SHORTCUTS
        regenLog(headerEncoding, header, &state);
#endif
        *dest++ = headerEncoding;

        BYTE encoding = headerEncoding;
        BYTE codeSet  = 1;
        while (encoding & MORE_BYTES_TO_FOLLOW)
        {
            encoding = EncodeHeaderNext(*header, &state, codeSet);

#if REGEN_SHORTCUTS
            regenLog(headerEncoding, header, &state);
#endif
            _ASSERTE(codeSet == 1 || codeSet == 2 && "Encoding must correspond to InfoHdrAdjust or InfoHdrAdjust2");
            if (codeSet == 2)
            {
                *dest++ = NEXT_OPCODE | MORE_BYTES_TO_FOLLOW;
                ++size;
            }

            *dest++ = encoding;
            ++size;
        }
    }
    else
    {
        size += more;
    }

    if (header->untrackedCnt > SET_UNTRACKED_MAX)
    {
        unsigned count = header->untrackedCnt;
        unsigned sz    = encodeUnsigned(mask ? dest : NULL, count);
        size += sz;
        dest += (sz & mask);
    }

    if (header->varPtrTableSize != 0)
    {
        unsigned count = header->varPtrTableSize;
        unsigned sz    = encodeUnsigned(mask ? dest : NULL, count);
        size += sz;
        dest += (sz & mask);
    }

    if (header->gsCookieOffset != INVALID_GS_COOKIE_OFFSET)
    {
        assert(mask == 0 || state.gsCookieOffset == HAS_GS_COOKIE_OFFSET);
        unsigned offset = header->gsCookieOffset;
        unsigned sz     = encodeUnsigned(mask ? dest : NULL, offset);
        size += sz;
        dest += (sz & mask);
    }

    if (header->syncStartOffset != INVALID_SYNC_OFFSET)
    {
        assert(mask == 0 || state.syncStartOffset == HAS_SYNC_OFFSET);

        {
            unsigned offset = header->syncStartOffset;
            unsigned sz     = encodeUnsigned(mask ? dest : NULL, offset);
            size += sz;
            dest += (sz & mask);
        }

        {
            unsigned offset = header->syncEndOffset;
            unsigned sz     = encodeUnsigned(mask ? dest : NULL, offset);
            size += sz;
            dest += (sz & mask);
        }
    }

    if (header->revPInvokeOffset != INVALID_REV_PINVOKE_OFFSET)
    {
        assert(mask == 0 || state.revPInvokeOffset == HAS_REV_PINVOKE_FRAME_OFFSET);
        unsigned offset = header->revPInvokeOffset;
        unsigned sz     = encodeUnsigned(mask ? dest : NULL, offset);
        size += sz;
        dest += (sz & mask);
    }

    if (header->epilogCount)
    {
        /* Generate table unless one epilog at the end of the method */

        if (header->epilogAtEnd == 0 || header->epilogCount != 1)
        {
#if VERIFY_GC_TABLES
            *castto(dest, unsigned short*)++ = 0xFACE;
            size += sizeof(short);
#endif

            /* Simply write a sorted array of offsets using encodeUDelta */

            uint8_t* epilogTable      = mask ? dest : nullptr;
            unsigned epilogPrevOffset = 0;
            unsigned epilogTableSize  = 0;

            codeGen->GetEmitter()->EnumerateEpilogs([&](unsigned offset) {
                unsigned encodedSize = encodeUDelta(epilogTable, offset, epilogPrevOffset);

                epilogPrevOffset = offset;
                epilogTableSize += encodedSize;

                if (epilogTable != nullptr)
                {
                    epilogTable += encodedSize;
                }
            });

            /* Add the size of the epilog table to the total size */

            size += epilogTableSize;
            dest += (epilogTableSize & mask);
        }
    }

#if DISPLAY_SIZES
    if (mask)
    {
        if (isFullyInterruptible)
        {
            genMethodICnt++;
        }
        else
        {
            genMethodNCnt++;
        }
    }
#endif // DISPLAY_SIZES

    return size;
}

// Return the size of the pointer tracking tables.
size_t GCEncoder::PtrTableSize(size_t* pArgTabOffset)
{
    BYTE temp[16 + 1];
#ifdef DEBUG
    temp[16] = 0xAB; // Set some marker
#endif

    /* Compute the total size of the tables */

    size_t size = MakeRegPtrTable(temp, 0, pArgTabOffset);

    assert(temp[16] == 0xAB); // Check that marker didnt get overwritten

    return size;
}

// Encode the callee-saved registers into 3 bits.
static unsigned gceEncodeCalleeSavedRegs(unsigned regs)
{
    unsigned encodedRegs = 0;

    if (regs & RBM_EBX)
        encodedRegs |= 0x04;
    if (regs & RBM_ESI)
        encodedRegs |= 0x02;
    if (regs & RBM_EDI)
        encodedRegs |= 0x01;

    return encodedRegs;
}

// Is the next entry for a byref pointer. If so, emit the prefix for the
// interruptible encoding. Check only for pushes and registers
static BYTE* gceByrefPrefixI(GCInfo::RegArgChange* change, BYTE* dest)
{
    // For registers, we don't need a prefix if it is going dead.
    assert((change->kind != GCInfo::RegArgChangeKind::RemoveRegs) || (change->regs == RBM_NONE));

    if ((change->kind == GCInfo::RegArgChangeKind::AddRegs) || (change->kind == GCInfo::RegArgChangeKind::PushArg))
    {
        if (change->gcType == GCT_BYREF)
        {
            *dest++ = 0xBF;
        }
    }

    return dest;
}

typedef unsigned pasMaskType;

static constexpr unsigned BITS_IN_pasMask     = sizeof(pasMaskType) * CHAR_BIT;
static constexpr unsigned HIGHEST_pasMask_BIT = ((pasMaskType)0x1) << (BITS_IN_pasMask - 1);

class PendingArgsStack
{
    unsigned    pasMaxDepth;
    unsigned    pasDepth           = 0;
    pasMaskType pasBottomMask      = 0;       // The first 32 args
    pasMaskType pasByrefBottomMask = 0;       // byref qualifier for pasBottomMask
    BYTE*       pasTopArray        = nullptr; // More than 32 args are represented here
    unsigned    pasPtrsInTopArray  = 0;       // How many GCptrs here

public:
    PendingArgsStack(unsigned maxDepth, Compiler* pComp) : pasMaxDepth(maxDepth)
    {
        if (pasMaxDepth > BITS_IN_pasMask)
        {
            pasTopArray = pComp->getAllocator(CMK_Unknown).allocate<BYTE>(pasMaxDepth - BITS_IN_pasMask);
        }
    }

    unsigned CurDepth()
    {
        return pasDepth;
    }

    pasMaskType ArgMask()
    {
        assert(pasDepth <= BITS_IN_pasMask);
        return pasBottomMask;
    }

    pasMaskType ByrefArgMask()
    {
        assert(pasDepth <= BITS_IN_pasMask);
        return pasByrefBottomMask;
    }

    void Push(GCtype gcType)
    {
        assert(pasDepth < pasMaxDepth);

        if (pasDepth < BITS_IN_pasMask)
        {
            /* Shift the mask */

            pasBottomMask <<= 1;
            pasByrefBottomMask <<= 1;

            if (gcType != GCT_NONE)
            {
                pasBottomMask |= 1;

                if (gcType == GCT_BYREF)
                    pasByrefBottomMask |= 1;
            }
        }
        else
        {
            /* Push on array */

            pasTopArray[pasDepth - BITS_IN_pasMask] = (BYTE)gcType;

            if (gcType)
                pasPtrsInTopArray++;
        }

        pasDepth++;
    }

    void Pop(unsigned count)
    {
        assert(pasDepth >= count);

        /* First pop from array (if applicable) */

        for (/**/; (pasDepth > BITS_IN_pasMask) && count; pasDepth--, count--)
        {
            unsigned topIndex = pasDepth - BITS_IN_pasMask - 1;

            GCtype topArg = (GCtype)pasTopArray[topIndex];

            if (topArg != GCT_NONE)
            {
                pasPtrsInTopArray--;
            }
        }

        if (count == 0)
        {
            return;
        }

        /* Now un-shift the mask */

        assert(pasPtrsInTopArray == 0);
        assert(count <= BITS_IN_pasMask);

        if (count == BITS_IN_pasMask) // (x>>32) is a nop on x86. So special-case it
        {
            pasBottomMask = pasByrefBottomMask = 0;
            pasDepth                           = 0;
        }
        else
        {
            pasBottomMask >>= count;
            pasByrefBottomMask >>= count;
            pasDepth -= count;
        }
    }

    // Kill (but don't pop) the top 'gcCount' args
    void Kill(unsigned gcCount)
    {
        assert(gcCount != 0);

        /* First kill args in array (if any) */

        for (unsigned curPos = pasDepth; (curPos > BITS_IN_pasMask) && gcCount; curPos--)
        {
            unsigned curIndex = curPos - BITS_IN_pasMask - 1;

            GCtype curArg = (GCtype)pasTopArray[curIndex];

            if (curArg != GCT_NONE)
            {
                pasTopArray[curIndex] = GCT_NONE;
                pasPtrsInTopArray--;
                gcCount--;
            }
        }

        /* Now kill bits from the mask */

        assert(pasPtrsInTopArray == 0);
        assert(gcCount <= BITS_IN_pasMask);

        for (unsigned bitPos = 1; gcCount; bitPos <<= 1)
        {
            assert(pasBottomMask != 0);

            if (pasBottomMask & bitPos)
            {
                pasBottomMask &= ~bitPos;
                pasByrefBottomMask &= ~bitPos;
                --gcCount;
            }
            else
            {
                assert(bitPos != HIGHEST_pasMask_BIT);
            }
        }
    }

    // Used for the case where there are more than BITS_IN_pasMask args on stack,
    // but none are any pointers. May avoid reporting anything to GCinfo
    bool HasGCptrs()
    {
        if (pasDepth <= BITS_IN_pasMask)
            return pasBottomMask != 0;
        else
            return pasBottomMask != 0 || pasPtrsInTopArray != 0;
    }

    // Iterates over mask and array to return total count.
    // Use only when you are going to emit a table of the offsets
    unsigned EnumGCoffsCount()
    {
        /* Should only be used in the worst case, when just the mask can't be used */

        assert(pasDepth > BITS_IN_pasMask && HasGCptrs());

        /* Count number of set bits in mask */

        unsigned count = 0;

        for (pasMaskType mask = 0x1, i = 0; i < BITS_IN_pasMask; mask <<= 1, i++)
        {
            if (mask & pasBottomMask)
                count++;
        }

        return count + pasPtrsInTopArray;
    }

    static constexpr unsigned pasENUM_START = UINT32_MAX;
    static constexpr unsigned pasENUM_LAST  = UINT32_MAX - 1;
    static constexpr unsigned pasENUM_END   = UINT32_MAX - 2;

    // Initalize enumeration by passing in iter=pasENUM_START.
    // Continue by passing in the return value as the new value of iter
    // End of enumeration when pasENUM_END is returned
    // If return value != pasENUM_END, *offs is set to the offset for GCinfo
    unsigned EnumGCoffs(unsigned iter, unsigned* offs)
    {
        if (iter == pasENUM_LAST)
            return pasENUM_END;

        unsigned i = (iter == pasENUM_START) ? pasDepth : iter;

        for (/**/; i > BITS_IN_pasMask; i--)
        {
            GCtype curArg = (GCtype)pasTopArray[i - BITS_IN_pasMask - 1];

            if (curArg != GCT_NONE)
            {
                unsigned offset;

                offset = (pasDepth - i) * TARGET_POINTER_SIZE;

                if (curArg == GCT_BYREF)
                {
                    offset |= byref_OFFSET_FLAG;
                }

                *offs = offset;
                return i - 1;
            }
        }

        if (!pasBottomMask)
            return pasENUM_END;

        // Have we already processed some of the bits in pasBottomMask ?

        i = (iter == pasENUM_START || iter >= BITS_IN_pasMask) ? 0     // no
                                                               : iter; // yes

        for (pasMaskType mask = 0x1 << i; mask; i++, mask <<= 1)
        {
            if (mask & pasBottomMask)
            {
                unsigned lvl =
                    (pasDepth > BITS_IN_pasMask) ? (pasDepth - BITS_IN_pasMask) : 0; // How many in pasTopArray[]
                lvl += i;

                unsigned offset;
                offset = lvl * TARGET_POINTER_SIZE;
                if (mask & pasByrefBottomMask)
                    offset |= byref_OFFSET_FLAG;

                *offs = offset;

                unsigned remMask = -int(mask << 1);
                return ((pasBottomMask & remMask) ? (i + 1) : pasENUM_LAST);
            }
        }

        assert(!"Shouldnt reach here");
        return pasENUM_END;
    }
};

// Generate the register pointer map, and return its total size in bytes. If
// 'mask' is 0, we don't actually store any data in 'dest' (except for one
// entry, which is never more than 10 bytes), so this can be used to merely
// compute the size of the table.
size_t GCEncoder::MakeRegPtrTable(BYTE* dest, int mask, size_t* pArgTabOffset)
{
    size_t totalSize = 0;

    /* The mask should be all 0's or all 1's */

    assert(mask == 0 || mask == -1);

    /* Start computing the total size of the table */

    bool emitArgTabOffset = (trackedStackSlotLifetimeCount != 0) || (untrackedStackSlotCount > SET_UNTRACKED_MAX);
    if (mask != 0 && emitArgTabOffset)
    {
        assert(*pArgTabOffset <= UINT32_MAX);
        unsigned sz = encodeUnsigned(dest, static_cast<unsigned>(*pArgTabOffset));
        dest += sz;
        totalSize += sz;
    }

#if VERIFY_GC_TABLES
    if (mask)
    {
        *(short*)dest = (short)0xBEEF;
        dest += sizeof(short);
    }

    totalSize += sizeof(short);
#endif

    if (untrackedStackSlotCount != 0)
    {
        unsigned slotSize = AddUntrackedStackSlots(dest, mask);
        totalSize += slotSize;
        dest += mask & slotSize;
    }

#if VERIFY_GC_TABLES
    if (mask)
    {
        *(short*)dest = (short)0xCAFE;
        dest += sizeof(short);
    }

    totalSize += sizeof(short);
#endif

    /**************************************************************************
     *
     *  Generate the table of stack pointer variable lifetimes.
     *
     **************************************************************************
     */

    // First we check for the most common case - no lifetimes at all.

    if (trackedStackSlotLifetimeCount != 0)
    {
        unsigned slotSize = AddTrackedStackSlots(dest, mask);
        totalSize += slotSize;
        dest += mask & slotSize;
    }

    if (pArgTabOffset != nullptr)
    {
        *pArgTabOffset = totalSize;
    }

#if VERIFY_GC_TABLES
    if (mask)
    {
        *(short*)dest = (short)0xBABE;
        dest += sizeof(short);
    }
    totalSize += sizeof(short);
#endif

    if (!mask && emitArgTabOffset)
    {
        assert(*pArgTabOffset <= UINT32_MAX);
        totalSize += encodeUnsigned(nullptr, static_cast<unsigned>(*pArgTabOffset));
    }

    unsigned slotSize;

    if (isFullyInterruptible)
    {
        slotSize = AddFullyInterruptibleSlots(dest, mask);
    }
    else if (compiler->codeGen->isFramePointerUsed())
    {
        slotSize = AddPartiallyInterruptibleSlotsFramed(dest, mask);
    }
    else
    {
        slotSize = AddPartiallyInterruptibleSlotsFrameless(dest, mask);
    }

    totalSize += slotSize;
    dest += mask & slotSize;

    /* Terminate the table with 0xFF */

    *dest = 0xFF;
    dest -= mask;
    totalSize++;

#if VERIFY_GC_TABLES
    if (mask)
    {
        *(short*)dest = (short)0xBEEB;
        dest += sizeof(short);
    }
    totalSize += sizeof(short);
#endif

#if MEASURE_PTRTAB_SIZE
    if (mask)
    {
        s_gcTotalPtrTabSize += totalSize;
    }
#endif

    return totalSize;
}

unsigned GCEncoder::AddUntrackedStackSlots(uint8_t* dest, const int mask)
{
    unsigned totalSize  = 0;
    int      lastoffset = 0;

    for (unsigned lclNum = 0; lclNum < compiler->lvaCount; lclNum++)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

        if (lcl->IsDependentPromotedField(compiler) || !lcl->lvOnFrame || lcl->HasGCSlotLiveness())
        {
            continue;
        }

        if (varTypeIsGC(lcl->GetType()))
        {
#ifndef FEATURE_EH_FUNCLETS
            if (lclNum == trackedThisLclNum)
            {
                continue;
            }
#endif

            int offset = lcl->GetStackOffset();

#if DOUBLE_ALIGN
            if (compiler->codeGen->doDoubleAlign() && lcl->IsParam() && !lcl->IsRegParam())
            {
                offset += compiler->codeGen->genTotalFrameSize();
            }
#endif

            if (lcl->TypeIs(TYP_BYREF))
            {
                offset |= byref_OFFSET_FLAG;
            }

            if (lcl->IsPinning())
            {
                offset |= pinned_OFFSET_FLAG;
            }

            int offsetDelta = lastoffset - offset;
            lastoffset      = offset;

            if (mask == 0)
            {
                totalSize += encodeSigned(nullptr, offsetDelta);
            }
            else
            {
                unsigned size = encodeSigned(dest, offsetDelta);
                dest += size;
                totalSize += size;
            }
        }
        else if (lcl->TypeIs(TYP_STRUCT) && lcl->HasGCPtr())
        {
            int lclOffset = lcl->GetStackOffset();

#if DOUBLE_ALIGN
            if (compiler->codeGen->doDoubleAlign() && lcl->IsParam() && !lcl->IsRegParam())
            {
                lclOffset += compiler->codeGen->genTotalFrameSize();
            }
#endif

            ClassLayout* layout = lcl->GetLayout();

            for (unsigned i = 0, slots = layout->GetSlotCount(); i < slots; i++)
            {
                if (!layout->IsGCPtr(i))
                {
                    continue;
                }

                unsigned slotOffset = lclOffset + i * TARGET_POINTER_SIZE;

                if (layout->GetGCPtrType(i) == TYP_BYREF)
                {
                    slotOffset |= byref_OFFSET_FLAG;
                }

                int offsetDelta = lastoffset - slotOffset;
                lastoffset      = slotOffset;

                if (mask == 0)
                {
                    totalSize += encodeSigned(nullptr, offsetDelta);
                }
                else
                {
                    unsigned size = encodeSigned(dest, offsetDelta);
                    dest += size;
                    totalSize += size;
                }
            }
        }
    }

    if (!compiler->codeGen->spillTemps.TrackGCSpillTemps())
    {
        for (SpillTemp& temp : compiler->codeGen->spillTemps)
        {
            if (!varTypeIsGC(temp.GetType()))
            {
                continue;
            }

            int offset = temp.GetOffset();

            if (temp.GetType() == TYP_BYREF)
            {
                offset |= byref_OFFSET_FLAG;
            }

            int offsetDelta = lastoffset - offset;
            lastoffset      = offset;

            if (mask == 0)
            {
                totalSize += encodeSigned(nullptr, offsetDelta);
            }
            else
            {
                unsigned size = encodeSigned(dest, offsetDelta);
                dest += size;
                totalSize += size;
            }
        }
    }

    return totalSize;
}

unsigned GCEncoder::AddTrackedStackSlots(uint8_t* dest, const int mask)
{
    unsigned totalSize = 0;

#ifndef FEATURE_EH_FUNCLETS
    if (trackedThisLclNum != BAD_VAR_NUM)
    {
        // Encoding of untracked variables does not support reporting
        // "this". So report it as a tracked variable with a liveness
        // extending over the entire method.

        assert(compiler->lvaGetDesc(trackedThisLclNum)->TypeIs(TYP_REF));

        unsigned slotOffset = compiler->lvaGetDesc(trackedThisLclNum)->GetStackOffset();

        slotOffset = abs(static_cast<int>(slotOffset));
        slotOffset |= this_OFFSET_FLAG;

        unsigned sz = encodeUnsigned(mask ? dest : nullptr, slotOffset);
        sz += encodeUDelta(mask ? (dest + sz) : nullptr, 0, 0);
        sz += encodeUDelta(mask ? (dest + sz) : nullptr, codeSize, 0);

        dest += (sz & mask);
        totalSize += sz;
    }
#endif // !FEATURE_EH_FUNCLETS

    unsigned lastOffset = 0;

    for (StackSlotLifetime* lifetime = firstStackSlotLifetime; lifetime != nullptr; lifetime = lifetime->next)
    {
        const unsigned begOffs = lifetime->beginCodeOffs;
        const unsigned endOffs = lifetime->endCodeOffs;

        if (endOffs == begOffs)
        {
            continue;
        }

        unsigned lowBits  = lifetime->slotOffset & OFFSET_MASK;
        unsigned slotOffs = abs(static_cast<int>(lifetime->slotOffset & ~OFFSET_MASK));
        slotOffs |= lowBits;

        unsigned sz = encodeUnsigned(mask ? dest : nullptr, slotOffs);
        sz += encodeUDelta(mask ? (dest + sz) : nullptr, begOffs, lastOffset);
        sz += encodeUDelta(mask ? (dest + sz) : nullptr, endOffs, begOffs);

        dest += (sz & mask);
        totalSize += sz;
        lastOffset = begOffs;
    }

    return totalSize;
}

unsigned GCEncoder::AddFullyInterruptibleSlots(uint8_t* dest, const int mask)
{
    assert(isFullyInterruptible);

    unsigned ptrRegs    = 0;
    unsigned lastOffset = 0;
    unsigned totalSize  = 0;

    for (RegArgChange* change = firstRegArgChange; change != nullptr; change = change->next)
    {
        /*
            Encoding table for methods that are fully interruptible

            The encoding used is as follows:

            ptr reg dead    00RRRDDD    [RRR != 100]
            ptr reg live    01RRRDDD    [RRR != 100]

        non-ptr arg push    10110DDD                    [SSS == 110]
            ptr arg push    10SSSDDD                    [SSS != 110] && [SSS != 111]
            ptr arg pop     11CCCDDD    [CCC != 000] && [CCC != 110] && [CCC != 111]
            little skip     11000DDD    [CCC == 000]
            bigger skip     11110BBB                    [CCC == 110]

            The values used in the above encodings are as follows:

              DDD                 code offset delta from previous entry (0-7)
              BBB                 bigger delta 000=8,001=16,010=24,...,111=64
              RRR                 register number (EAX=000,ECX=001,EDX=010,EBX=011,
                                    EBP=101,ESI=110,EDI=111), ESP=100 is reserved
              SSS                 argument offset from base of stack. This is
                                    redundant for frameless methods as we can
                                    infer it from the previous pushes+pops. However,
                                    for EBP-methods, we only report GC pushes, and
                                    so we need SSS
              CCC                 argument count being popped (includes only ptrs for EBP methods)

            The following are the 'large' versions:

              large delta skip        10111000 [0xB8] , encodeUnsigned(delta)

              large     ptr arg push  11111000 [0xF8] , encodeUnsigned(pushCount)
              large non-ptr arg push  11111001 [0xF9] , encodeUnsigned(pushCount)
              large     ptr arg pop   11111100 [0xFC] , encodeUnsigned(popCount)
              large         arg dead  11111101 [0xFD] , encodeUnsigned(popCount) for caller-pop args.
                                                          Any GC args go dead after the call,
                                                          but are still sitting on the stack

              this pointer prefix     10111100 [0xBC]   the next encoding is a ptr live
                                                          or a ptr arg push
                                                          and contains the this pointer

              interior or by-ref      10111111 [0xBF]   the next encoding is a ptr live
                   pointer prefix                         or a ptr arg push
                                                          and contains an interior
                                                          or by-ref pointer


              The value 11111111 [0xFF] indicates the end of the table.
        */

        uint8_t* base       = dest;
        unsigned nextOffset = change->codeOffs;
        assert(nextOffset >= lastOffset);
        unsigned codeDelta = nextOffset - lastOffset;

        // If the code delta is between 8 and (64+7),
        // generate a 'bigger delta' encoding

        if ((codeDelta >= 8) && (codeDelta <= (64 + 7)))
        {
            unsigned biggerDelta = ((codeDelta - 8) & 0x38) + 8;
            *dest++              = 0xF0 | ((biggerDelta - 8) >> 3);
            lastOffset += biggerDelta;
            codeDelta &= 0x07;
        }

        // If the code delta is still bigger than 7,
        // generate a 'large code delta' encoding

        if (codeDelta > 7)
        {
            *dest++ = 0xB8;
            dest += encodeUnsigned(dest, codeDelta);
            codeDelta = 0;

            /* Remember the new 'last' offset */

            lastOffset = nextOffset;
        }

        if ((change->kind != RegArgChangeKind::AddRegs) && (change->kind != RegArgChangeKind::RemoveRegs))
        {
            if (change->kind == RegArgChangeKind::KillArgs)
            {
                if (codeDelta)
                {
                    /*
                        Use the small encoding:
                        little delta skip       11000DDD    [0xC0]
                     */

                    assert((codeDelta & 0x7) == codeDelta);
                    *dest++ = 0xC0 | (BYTE)codeDelta;

                    /* Remember the new 'last' offset */

                    lastOffset = nextOffset;
                }

                /* Caller-pop arguments are dead after call but are still
                   sitting on the stack */

                *dest++ = 0xFD;
                assert(change->argOffset != 0);
                dest += encodeUnsigned(dest, change->argOffset);
            }
            else if (change->gcType == GCT_NONE)
            {
                assert(change->kind == RegArgChangeKind::PushArg);
                assert((codeDelta & 0x7) == codeDelta);
#ifndef UNIX_X86_ABI
                assert(!compiler->codeGen->isFramePointerUsed());
#endif

                // Encoding: non-ptr arg push 10110DDD [0xB0] (push of sizeof(int))
                *dest++ = static_cast<uint8_t>(0xB0 | codeDelta);

                lastOffset = nextOffset;
            }
            else if (change->argOffset < 6)
            {
                unsigned isPop = (change->kind == RegArgChangeKind::PopArgs) || (change->kind == RegArgChangeKind::Pop);

                assert((change->argOffset != 0) || !isPop);

                dest = gceByrefPrefixI(change, dest);
                // Encoding: ptr arg push 10SSSDDD [SSS != 110] && [SSS != 111]
                // Encoding: ptr arg pop  11CCCDDD [CCC != 110] && [CCC != 111]
                *dest++ = static_cast<uint8_t>(0x80 | codeDelta | (change->argOffset << 3) | (isPop << 6));

                lastOffset = nextOffset;
            }
            else
            {
                unsigned isPop = (change->kind == RegArgChangeKind::PopArgs) || (change->kind == RegArgChangeKind::Pop);

                assert((change->argOffset != 0) || !isPop);

                if (codeDelta != 0)
                {
                    assert((codeDelta & 0x7) == codeDelta);

                    // Encoding: little delta skip       11000DDD    [0xC0]
                    *dest++ = static_cast<uint8_t>(0xC0 | codeDelta);
                }

                dest = gceByrefPrefixI(change, dest);
                // Encoding: large ptr arg push  11111000 [0xF8]
                // Encoding: large ptr arg pop   11111100 [0xFC]
                *dest++ = 0xF8 | (isPop << 2);
                dest += encodeUnsigned(dest, change->argOffset);

                lastOffset = nextOffset;
            }
        }
        else if (change->kind == RegArgChangeKind::RemoveRegs)
        {
            // Record any registers that are becoming dead.

            unsigned regMask = change->regs & ptrRegs;

            while (regMask) // EAX,ECX,EDX,EBX,---,EBP,ESI,EDI
            {
                unsigned  tmpMask;
                regNumber regNum;

                /* Get hold of the next register bit */

                tmpMask = genFindLowestReg(regMask);
                assert(tmpMask);

                /* Remember the new state of this register */

                ptrRegs &= ~tmpMask;

                /* Figure out which register the next bit corresponds to */

                regNum = genRegNumFromMask(tmpMask);
                assert(regNum <= 7);

                /* Reserve ESP, regNum==4 for future use */

                assert(regNum != 4);

                /*
                    Generate a small encoding:

                        ptr reg dead        00RRRDDD
                 */

                assert((codeDelta & 0x7) == codeDelta);
                *dest++ = 0x00 | regNum << 3 | (BYTE)codeDelta;

                /* Turn the bit we've just generated off and continue */

                regMask -= tmpMask; // EAX,ECX,EDX,EBX,---,EBP,ESI,EDI

                /* Remember the new 'last' offset */

                lastOffset = nextOffset;

                /* Any entries that follow will be at the same offset */

                codeDelta = 0;
            }
        }
        else
        {
            assert(change->kind == RegArgChangeKind::AddRegs);

            /* Record any registers that are becoming live */

            unsigned regMask = change->regs & ~ptrRegs;

            while (regMask) // EAX,ECX,EDX,EBX,---,EBP,ESI,EDI
            {
                unsigned  tmpMask;
                regNumber regNum;

                /* Get hold of the next register bit */

                tmpMask = genFindLowestReg(regMask);
                assert(tmpMask);

                /* Remember the new state of this register */

                ptrRegs |= tmpMask;

                /* Figure out which register the next bit corresponds to */

                regNum = genRegNumFromMask(tmpMask);
                assert(regNum <= 7);

                /*
                    Generate a small encoding:

                        ptr reg live        01RRRDDD
                 */

                dest = gceByrefPrefixI(change, dest);

                if (regNum == syncThisReg)
                {
                    // Mark with 'this' pointer prefix
                    *dest++ = 0xBC;
                    // Can only have one bit set in regMask
                    assert(regMask == tmpMask);
                }

                assert((codeDelta & 0x7) == codeDelta);
                *dest++ = 0x40 | (regNum << 3) | (BYTE)codeDelta;

                /* Turn the bit we've just generated off and continue */

                regMask -= tmpMask; // EAX,ECX,EDX,EBX,---,EBP,ESI,EDI

                /* Remember the new 'last' offset */

                lastOffset = nextOffset;

                /* Any entries that follow will be at the same offset */

                codeDelta = 0;
            }
        }

        /* Keep track of the total amount of generated stuff */

        totalSize += dest - base;

        /* Go back to the buffer start if we're not generating a table */

        if (!mask)
            dest = base;
    }

    return totalSize;
}

unsigned GCEncoder::AddPartiallyInterruptibleSlotsFramed(uint8_t* dest, const int mask)
{
    assert(!isFullyInterruptible);
    assert(compiler->codeGen->isFramePointerUsed());

    /*
        Encoding table for methods with an EBP frame and
                           that are not fully interruptible

        The encoding used is as follows:

        this pointer encodings:

           01000000          this pointer in EBX
           00100000          this pointer in ESI
           00010000          this pointer in EDI

        tiny encoding:

           0bsdDDDD
                             requires code delta > 0 & delta < 16 (4-bits)
                             requires pushed argmask == 0

             where    DDDD   is code delta
                         b   indicates that register EBX is a live pointer
                         s   indicates that register ESI is a live pointer
                         d   indicates that register EDI is a live pointer


        small encoding:

           1DDDDDDD bsdAAAAA

                             requires code delta     < 120 (7-bits)
                             requires pushed argmask <  64 (5-bits)

             where DDDDDDD   is code delta
                     AAAAA   is the pushed args mask
                         b   indicates that register EBX is a live pointer
                         s   indicates that register ESI is a live pointer
                         d   indicates that register EDI is a live pointer

        medium encoding

           0xFD aaaaaaaa AAAAdddd bseDDDDD

                             requires code delta     <  512  (9-bits)
                             requires pushed argmask < 2048 (12-bits)

             where    DDDDD  is the upper 5-bits of the code delta
                       dddd  is the low   4-bits of the code delta
                       AAAA  is the upper 4-bits of the pushed arg mask
                   aaaaaaaa  is the low   8-bits of the pushed arg mask
                          b  indicates that register EBX is a live pointer
                          s  indicates that register ESI is a live pointer
                          e  indicates that register EDI is a live pointer

        medium encoding with interior pointers

           0xF9 DDDDDDDD bsdAAAAAA iiiIIIII

                             requires code delta     < 256 (8-bits)
                             requires pushed argmask <  64 (5-bits)

             where  DDDDDDD  is the code delta
                          b  indicates that register EBX is a live pointer
                          s  indicates that register ESI is a live pointer
                          d  indicates that register EDI is a live pointer
                      AAAAA  is the pushed arg mask
                        iii  indicates that EBX,EDI,ESI are interior pointers
                      IIIII  indicates that bits in the arg mask are interior
                             pointers

        large encoding

           0xFE [0BSD0bsd][32-bit code delta][32-bit argMask]

                          b  indicates that register EBX is a live pointer
                          s  indicates that register ESI is a live pointer
                          d  indicates that register EDI is a live pointer
                          B  indicates that register EBX is an interior pointer
                          S  indicates that register ESI is an interior pointer
                          D  indicates that register EDI is an interior pointer
                             requires pushed  argmask < 32-bits

        large encoding  with interior pointers

           0xFA [0BSD0bsd][32-bit code delta][32-bit argMask][32-bit interior pointer mask]


                          b  indicates that register EBX is a live pointer
                          s  indicates that register ESI is a live pointer
                          d  indicates that register EDI is a live pointer
                          B  indicates that register EBX is an interior pointer
                          S  indicates that register ESI is an interior pointer
                          D  indicates that register EDI is an interior pointer
                             requires pushed  argmask < 32-bits
                             requires pushed iArgmask < 32-bits


        huge encoding        This is the only encoding that supports
                             a pushed argmask which is greater than
                             32-bits.

           0xFB [0BSD0bsd][32-bit code delta]
                [32-bit table count][32-bit table size]
                [pushed ptr offsets table...]

                         b   indicates that register EBX is a live pointer
                         s   indicates that register ESI is a live pointer
                         d   indicates that register EDI is a live pointer
                         B   indicates that register EBX is an interior pointer
                         S   indicates that register ESI is an interior pointer
                         D   indicates that register EDI is an interior pointer
                         the list count is the number of entries in the list
                         the list size gives the byte-length of the list
                         the offsets in the list are variable-length
    */

    // If "this" is enregistered, note it. We do this explicitly here because we
    // don't record any reg liveness changes in partially interruptible methods.

    unsigned lastOffset = 0;
    unsigned totalSize  = 0;

    if (syncThisReg != REG_NA)
    {
        unsigned thisPtrRegEnc = gceEncodeCalleeSavedRegs(genRegMask(syncThisReg)) << 4;

        // TODO-MIKE-Review: Probably this should be an assert...
        if (thisPtrRegEnc != 0)
        {
            totalSize += 1;
            if (mask)
                *dest++ = thisPtrRegEnc;
        }
    }

    for (CallSite* call = firstCallSite; call != nullptr; call = call->next)
    {
        BYTE*    base = dest;
        unsigned nextOffset;

        nextOffset = call->codeOffs;

        DWORD codeDelta = nextOffset - lastOffset;
        assert((int)codeDelta >= 0);
        lastOffset = nextOffset;

        unsigned gcrefRegMask = 0;
        unsigned byrefRegMask = 0;

        gcrefRegMask |= gceEncodeCalleeSavedRegs(call->refRegs);
        byrefRegMask |= gceEncodeCalleeSavedRegs(call->byrefRegs);

        assert((gcrefRegMask & byrefRegMask) == 0);

        unsigned regMask = gcrefRegMask | byrefRegMask;

        bool byref = (byrefRegMask | call->byrefArgMask) != 0;

        /* Check for the really large argument offset case */
        /* The very rare Huge encodings */

        if (call->argCount != 0)
        {
            unsigned argNum;
            unsigned argCnt    = call->argCount;
            DWORD    argBytes  = 0;
            BYTE*    pArgBytes = nullptr;

            if (mask != 0)
            {
                *dest++       = 0xFB;
                *dest++       = (byrefRegMask << 4) | regMask;
                *(DWORD*)dest = codeDelta;
                dest += sizeof(DWORD);
                *(DWORD*)dest = argCnt;
                dest += sizeof(DWORD);
                // skip the byte-size for now. Just note where it will go
                pArgBytes = dest;
                dest += sizeof(DWORD);
            }

            for (argNum = 0; argNum < argCnt; argNum++)
            {
                unsigned eltSize;
                eltSize = encodeUnsigned(dest, call->argTable[argNum]);
                argBytes += eltSize;
                if (mask)
                    dest += eltSize;
            }

            if (mask == 0)
            {
                dest = base + 1 + 1 + 3 * sizeof(DWORD) + argBytes;
            }
            else
            {
                assert(dest == pArgBytes + sizeof(argBytes) + argBytes);
                *(DWORD*)pArgBytes = argBytes;
            }
        }

        /* Check if we can use a tiny encoding */
        else if ((codeDelta < 16) && (codeDelta != 0) && (call->argMask == 0) && !byref)
        {
            *dest++ = (regMask << 4) | (BYTE)codeDelta;
        }

        /* Check if we can use the small encoding */
        else if ((codeDelta < 0x79) && (call->argMask <= 0x1F) && !byref)
        {
            *dest++ = 0x80 | (BYTE)codeDelta;
            *dest++ = call->argMask | (regMask << 5);
        }

        /* Check if we can use the medium encoding */
        else if (codeDelta <= 0x01FF && call->argMask <= 0x0FFF && !byref)
        {
            *dest++ = 0xFD;
            *dest++ = call->argMask;
            *dest++ = ((call->argMask >> 4) & 0xF0) | ((BYTE)codeDelta & 0x0F);
            *dest++ = (regMask << 5) | (BYTE)((codeDelta >> 4) & 0x1F);
        }

        /* Check if we can use the medium encoding with byrefs */
        else if (codeDelta <= 0x0FF && call->argMask <= 0x01F)
        {
            *dest++ = 0xF9;
            *dest++ = (BYTE)codeDelta;
            *dest++ = (regMask << 5) | call->argMask;
            *dest++ = (byrefRegMask << 5) | call->byrefArgMask;
        }

        /* We'll use the large encoding */
        else if (!byref)
        {
            *dest++       = 0xFE;
            *dest++       = (byrefRegMask << 4) | regMask;
            *(DWORD*)dest = codeDelta;
            dest += sizeof(DWORD);
            *(DWORD*)dest = call->argMask;
            dest += sizeof(DWORD);
        }

        /* We'll use the large encoding with byrefs */
        else
        {
            *dest++       = 0xFA;
            *dest++       = (byrefRegMask << 4) | regMask;
            *(DWORD*)dest = codeDelta;
            dest += sizeof(DWORD);
            *(DWORD*)dest = call->argMask;
            dest += sizeof(DWORD);
            *(DWORD*)dest = call->byrefArgMask;
            dest += sizeof(DWORD);
        }

        /* Keep track of the total amount of generated stuff */

        totalSize += dest - base;

        /* Go back to the buffer start if we're not generating a table */

        if (!mask)
            dest = base;
    }

    return totalSize;
}

unsigned GCEncoder::AddPartiallyInterruptibleSlotsFrameless(uint8_t* dest, const int mask)
{
    assert(!isFullyInterruptible);
    assert(!compiler->codeGen->isFramePointerUsed());

    unsigned totalSize = 0;

    if (syncThisReg != REG_NA)
    {
        assert(trackedThisLclNum == BAD_VAR_NUM);

        uint8_t regEncoding;

        switch (syncThisReg)
        {
            case REG_EDI:
                regEncoding = 0;
                break;
            case REG_ESI:
                regEncoding = 1;
                break;
            case REG_EBX:
                regEncoding = 2;
                break;
            case REG_EBP:
                regEncoding = 3;
                break;
            default:
                // TODO-MIKE-Review: Should there be an assert/unreached here?
                //
                // It looks like if `this` has to be reported then it has to be
                // in a callee-saved register. But we don't require this in LSRA
                // so we can end up with `this` in the original reg param, ECX.
                //
                // Thing is, most scenarios that require `this` reporting involve
                // calls and then `this` can only be in a callee-saved register,
                // otherwise it would be spilled.
                //
                // But there are cases where lvaKeepAliveAndReportThis returns
                // true and there are no calls in the method and `this` happily
                // ends up in ECX due to the lack of LSRA restrictions. Which one
                // is wrong - lvaKeepAliveAndReportThis or LSRA?
                // AddPartiallyInterruptibleSlotsFramed has the same issue.
                regEncoding = 4;
                break;
        }

        if (regEncoding < 4)
        {
            *dest++ = 0xF4 | regEncoding;
            totalSize++;
        }
    }

    unsigned         lastOffset = 0;
    PendingArgsStack pasStk(codeGen->GetEmitter()->GetMaxStackDepth(), compiler);

    for (RegArgChange* change = firstRegArgChange; change != nullptr; change = change->next)
    {
        /*
         *    Encoding table for methods without an EBP frame and
         *     that are not fully interruptible
         *
         *               The encoding used is as follows:
         *
         *  push     000DDDDD                     ESP push one item with 5-bit delta
         *  push     00100000 [pushCount]         ESP push multiple items
         *  reserved 0010xxxx                     xxxx != 0000
         *  reserved 0011xxxx
         *  skip     01000000 [Delta]             Skip Delta, arbitrary sized delta
         *  skip     0100DDDD                     Skip small Delta, for call (DDDD != 0)
         *  pop      01CCDDDD                     ESP pop  CC items with 4-bit delta (CC != 00)
         *  call     1PPPPPPP                     Call Pattern, P=[0..79]
         *  call     1101pbsd DDCCCMMM            Call RegMask=pbsd,ArgCnt=CCC,
         *                                        ArgMask=MMM Delta=commonDelta[DD]
         *  call     1110pbsd [ArgCnt] [ArgMask]  Call ArgCnt,RegMask=pbsd,ArgMask
         *  call     11111000 [PBSDpbsd][32-bit delta][32-bit ArgCnt]
         *                    [32-bit PndCnt][32-bit PndSize][PndOffs...]
         *  iptr     11110000 [IPtrMask]          Arbitrary Interior Pointer Mask
         *  thisptr  111101RR                     This pointer is in Register RR
         *                                        00=EDI,01=ESI,10=EBX,11=EBP
         *  reserved 111100xx                     xx  != 00
         *  reserved 111110xx                     xx  != 00
         *  reserved 11111xxx                     xxx != 000 && xxx != 111(EOT)
         *
         *   The value 11111111 [0xFF] indicates the end of the table. (EOT)
         *
         *  An offset (at which stack-walking is performed) without an explicit encoding
         *  is assumed to be a trivial call-site (no GC registers, stack empty before and
         *  after) to avoid having to encode all trivial calls.
         *
         * Note on the encoding used for interior pointers
         *
         *   The iptr encoding must immediately precede a call encoding.  It is used
         *   to transform a normal GC pointer addresses into an interior pointers for
         *   GC purposes.  The mask supplied to the iptr encoding is read from the
         *   least signicant bit to the most signicant bit. (i.e the lowest bit is
         *   read first)
         *
         *   p   indicates that register EBP is a live pointer
         *   b   indicates that register EBX is a live pointer
         *   s   indicates that register ESI is a live pointer
         *   d   indicates that register EDI is a live pointer
         *   P   indicates that register EBP is an interior pointer
         *   B   indicates that register EBX is an interior pointer
         *   S   indicates that register ESI is an interior pointer
         *   D   indicates that register EDI is an interior pointer
         *
         *   As an example the following sequence indicates that EDI.ESI and the
         *   second pushed pointer in ArgMask are really interior pointers.  The
         *   pointer in ESI in a normal pointer:
         *
         *   iptr 11110000 00010011           => read Interior Ptr, Interior Ptr,
         *                                       Normal Ptr, Normal Ptr, Interior Ptr
         *
         *   call 11010011 DDCCC011 RRRR=1011 => read EDI is a GC-pointer,
         *                                            ESI is a GC-pointer.
         *                                            EBP is a GC-pointer
         *                           MMM=0011 => read two GC-pointers arguments
         *                                         on the stack (nested call)
         *
         *   Since the call instruction mentions 5 GC-pointers we list them in
         *   the required order:  EDI, ESI, EBP, 1st-pushed pointer, 2nd-pushed pointer
         *
         *   And we apply the Interior Pointer mask mmmm=10011 to the five GC-pointers
         *   we learn that EDI and ESI are interior GC-pointers and that
         *   the second push arg is an interior GC-pointer.
         */

        BYTE* base = dest;

        unsigned nextOffset = change->codeOffs;
        assert(nextOffset >= lastOffset);
        unsigned codeDelta = nextOffset - lastOffset;

        bool usePopEncoding;

#if REGEN_CALLPAT
        // Must initialize this flag to true when REGEN_CALLPAT is on
        usePopEncoding         = true;
        unsigned origCodeDelta = codeDelta;
#endif

        if (change->kind == RegArgChangeKind::KillArgs)
        {
            pasStk.Kill(change->argOffset);
        }
        else if (change->kind == RegArgChangeKind::PushArg)
        {
            lastOffset = nextOffset;

            if (codeDelta >= 32)
            {
                /* use encoding: */
                /*   skip    01000000 [Delta] */
                *dest++ = 0x40;
                dest += encodeUnsigned(dest, codeDelta - 31);
                codeDelta = 31;
            }

            assert(codeDelta < 32);

            /* use encoding: */
            /*   push    000DDDDD ESP push one item, 5-bit delta */

            *dest++ = (BYTE)codeDelta;

            /* adjust argMask for this push */

            pasStk.Push(change->gcType);
        }
        else if (change->kind == RegArgChangeKind::Pop)
        {
            lastOffset = nextOffset;

            assert(change->argOffset == 1);

            if (codeDelta >= 16)
            {
                /* use encoding: */
                /*   skip    01000000 [Delta] */
                *dest++ = 0x40;
                dest += encodeUnsigned(dest, codeDelta - 15);
                codeDelta = 15;
            }

            /* use encoding: */
            /*   pop1    0101DDDD  ESP pop one item, 4-bit delta */

            *dest++ = 0x50 | (BYTE)codeDelta;

            /* adjust argMask for this pop */
            pasStk.Pop(1);
        }
        else
        {
            assert(change->kind == RegArgChangeKind::PopArgs);

            lastOffset = nextOffset;

            unsigned callArgCnt   = change->argOffset;
            unsigned gcrefRegMask = change->callRefRegs;

            unsigned byrefRegMask = change->callByrefRegs;

            assert((gcrefRegMask & byrefRegMask) == 0);

            unsigned regMask = gcrefRegMask | byrefRegMask;

            /* adjust argMask for this call-site */
            pasStk.Pop(callArgCnt);

            /* Do we have to use the fat encoding */

            if (pasStk.CurDepth() > BITS_IN_pasMask && pasStk.HasGCptrs())
            {
                /* use fat encoding:
                 *   11111000 [PBSDpbsd][32-bit delta][32-bit ArgCnt]
                 *            [32-bit PndCnt][32-bit PndSize][PndOffs...]
                 */

                DWORD pndCount = pasStk.EnumGCoffsCount();
                DWORD pndSize  = 0;
                BYTE* pPndSize = nullptr;

                if (mask)
                {
                    *dest++       = 0xF8;
                    *dest++       = (byrefRegMask << 4) | regMask;
                    *(DWORD*)dest = codeDelta;
                    dest += sizeof(DWORD);
                    *(DWORD*)dest = callArgCnt;
                    dest += sizeof(DWORD);
                    *(DWORD*)dest = pndCount;
                    dest += sizeof(DWORD);
                    pPndSize = dest;
                    dest += sizeof(DWORD); // Leave space for pndSize
                }

                unsigned offs, iter;

                for (iter = pasStk.EnumGCoffs(PendingArgsStack::pasENUM_START, &offs); pndCount;
                     iter = pasStk.EnumGCoffs(iter, &offs), pndCount--)
                {
                    unsigned eltSize = encodeUnsigned(dest, offs);

                    pndSize += eltSize;
                    if (mask)
                        dest += eltSize;
                }
                assert(iter == PendingArgsStack::pasENUM_END);

                if (mask == 0)
                {
                    dest = base + 2 + 4 * sizeof(DWORD) + pndSize;
                }
                else
                {
                    assert(pPndSize + sizeof(pndSize) + pndSize == dest);
                    *(DWORD*)pPndSize = pndSize;
                }

                goto NEXT_RPD;
            }

            unsigned argMask      = 0;
            unsigned byrefArgMask = 0;

            if (pasStk.HasGCptrs())
            {
                assert(pasStk.CurDepth() <= BITS_IN_pasMask);

                argMask      = pasStk.ArgMask();
                byrefArgMask = pasStk.ByrefArgMask();
            }

            /* Shouldn't be reporting trivial call-sites */

            assert(regMask || argMask || callArgCnt || pasStk.CurDepth());

// Emit IPtrMask if needed

#define CHK_NON_INTRPT_ESP_IPtrMask                                                                                    \
                                                                                                                       \
    if (byrefRegMask || byrefArgMask)                                                                                  \
    {                                                                                                                  \
        *dest++        = 0xF0;                                                                                         \
        unsigned imask = (byrefArgMask << 4) | byrefRegMask;                                                           \
        dest += encodeUnsigned(dest, imask);                                                                           \
    }

            /* When usePopEncoding is true:
             *  this is not an interesting call site
             *   because nothing is live here.
             */
            usePopEncoding = ((callArgCnt < 4) && (regMask == 0) && (argMask == 0));

            if (!usePopEncoding)
            {
                int pattern = LookupCallPattern(callArgCnt, regMask, argMask, codeDelta);
                if (pattern != -1)
                {
                    if (pattern > 0xff)
                    {
                        codeDelta = pattern >> 8;
                        pattern &= 0xff;
                        if (codeDelta >= 16)
                        {
                            /* use encoding: */
                            /*   skip 01000000 [Delta] */
                            *dest++ = 0x40;
                            dest += encodeUnsigned(dest, codeDelta);
                            codeDelta = 0;
                        }
                        else
                        {
                            /* use encoding: */
                            /*   skip 0100DDDD  small delta=DDDD */
                            *dest++ = 0x40 | (BYTE)codeDelta;
                        }
                    }

                    // Emit IPtrMask if needed
                    CHK_NON_INTRPT_ESP_IPtrMask;

                    assert((pattern >= 0) && (pattern < 80));
                    *dest++ = 0x80 | pattern;
                    goto NEXT_RPD;
                }

                /* See if we can use 2nd call encoding
                 *     1101RRRR DDCCCMMM encoding */

                if ((callArgCnt <= 7) && (argMask <= 7))
                {
                    unsigned inx; // callCommonDelta[] index
                    unsigned maxCommonDelta = callCommonDelta[3];

                    if (codeDelta > maxCommonDelta)
                    {
                        if (codeDelta > maxCommonDelta + 15)
                        {
                            /* use encoding: */
                            /*   skip    01000000 [Delta] */
                            *dest++ = 0x40;
                            dest += encodeUnsigned(dest, codeDelta - maxCommonDelta);
                        }
                        else
                        {
                            /* use encoding: */
                            /*   skip 0100DDDD  small delta=DDDD */
                            *dest++ = 0x40 | (BYTE)(codeDelta - maxCommonDelta);
                        }

                        codeDelta = maxCommonDelta;
                        inx       = 3;
                        goto EMIT_2ND_CALL_ENCODING;
                    }

                    for (inx = 0; inx < 4; inx++)
                    {
                        if (codeDelta == callCommonDelta[inx])
                        {
                        EMIT_2ND_CALL_ENCODING:
                            // Emit IPtrMask if needed
                            CHK_NON_INTRPT_ESP_IPtrMask;

                            *dest++ = 0xD0 | regMask;
                            *dest++ = (inx << 6) | (callArgCnt << 3) | argMask;
                            goto NEXT_RPD;
                        }
                    }

                    unsigned minCommonDelta = callCommonDelta[0];

                    if ((codeDelta > minCommonDelta) && (codeDelta < maxCommonDelta))
                    {
                        assert((minCommonDelta + 16) > maxCommonDelta);
                        /* use encoding: */
                        /*   skip 0100DDDD  small delta=DDDD */
                        *dest++ = 0x40 | (BYTE)(codeDelta - minCommonDelta);

                        codeDelta = minCommonDelta;
                        inx       = 0;
                        goto EMIT_2ND_CALL_ENCODING;
                    }
                }
            }

            if (codeDelta >= 16)
            {
                unsigned i = (usePopEncoding ? 15 : 0);
                /* use encoding: */
                /*   skip    01000000 [Delta]  arbitrary sized delta */
                *dest++ = 0x40;
                dest += encodeUnsigned(dest, codeDelta - i);
                codeDelta = i;
            }

            if ((codeDelta > 0) || usePopEncoding)
            {
                if (usePopEncoding)
                {
                    /* use encoding: */
                    /*   pop 01CCDDDD  ESP pop CC items, 4-bit delta */
                    if (callArgCnt || codeDelta)
                        *dest++ = (BYTE)(0x40 | (callArgCnt << 4) | codeDelta);
                    goto NEXT_RPD;
                }
                else
                {
                    /* use encoding: */
                    /*   skip 0100DDDD  small delta=DDDD */
                    *dest++ = 0x40 | (BYTE)codeDelta;
                }
            }

            // Emit IPtrMask if needed
            CHK_NON_INTRPT_ESP_IPtrMask;

            /* use encoding:                                   */
            /*   call 1110RRRR [ArgCnt] [ArgMask]              */

            *dest++ = 0xE0 | regMask;
            dest += encodeUnsigned(dest, callArgCnt);

            dest += encodeUnsigned(dest, argMask);
        }

    /*  We ignore the register live/dead information, since the
     *  rpdCallRegMask contains all the liveness information
     *  that we need
     */
    NEXT_RPD:

        totalSize += dest - base;

        /* Go back to the buffer start if we're not generating a table */

        if (!mask)
            dest = base;

#if REGEN_CALLPAT
        if ((mask == -1) && (usePopEncoding == false) && ((dest - base) > 0))
            regenLog(origCodeDelta, argMask, regMask, callArgCnt, byrefArgMask, byrefRegMask, base, (dest - base));
#endif
    }

    /* Verify that we pop every arg that was pushed and that argMask is 0 */

    assert(pasStk.CurDepth() == 0);

    return totalSize;
}

#if DUMP_GC_TABLES
#include "gcdump.h"

#if VERIFY_GC_TABLES
const bool verifyGCTables = true;
#else
const bool verifyGCTables = false;
#endif

size_t GCEncoder::InfoBlockHdrDump(const uint8_t* table, InfoHdr* header, unsigned* methodSize)
{
    GCDump gcDump(GCINFO_VERSION);
    gcDump.gcPrintf = gcDump_logf;
    printf("Method info block:\n");
    return gcDump.DumpInfoHdr(table, header, methodSize, verifyGCTables);
}

size_t GCEncoder::DumpPtrTable(const uint8_t* table, const InfoHdr& header, unsigned methodSize)
{
    printf("Pointer table:\n");
    GCDump gcDump(GCINFO_VERSION);
    gcDump.gcPrintf = gcDump_logf;
    return gcDump.DumpGCTable(table, header, methodSize, verifyGCTables);
}

#endif // DUMP_GC_TABLES

#else // !JIT32_GCENCODER

class RegSlotIdKey
{
    unsigned key;

public:
    RegSlotIdKey(regNumber regNum, GcSlotFlags flags)
        : key(static_cast<unsigned>(regNum) | (static_cast<unsigned>(flags) << 16))
    {
    }

    static unsigned GetHashCode(RegSlotIdKey k)
    {
        return k.key;
    }

    static bool Equals(RegSlotIdKey x, RegSlotIdKey y)
    {
        return x.key == y.key;
    }
};

class StackSlotIdKey
{
    uint64_t key;

public:
    StackSlotIdKey(int offset, GcSlotFlags flags, GcStackSlotBase base)
        : key(static_cast<unsigned>(offset) | (static_cast<uint64_t>(flags) << 32) |
              (static_cast<uint64_t>(base) << 48))
    {
    }

    static unsigned GetHashCode(StackSlotIdKey k)
    {
        return static_cast<unsigned>((k.key >> 32) ^ k.key);
    }

    static bool Equals(StackSlotIdKey x, StackSlotIdKey y)
    {
        return x.key == y.key;
    }
};

#if defined(DEBUG) || DUMP_GC_TABLES
#define GC_ENCODER_LOGGING
#endif

class GCEncoder : private GcInfoEncoder
{
    Compiler* compiler;
    JitHashTable<RegSlotIdKey, RegSlotIdKey, GcSlotId>     regSlotMap;
    JitHashTable<StackSlotIdKey, StackSlotIdKey, GcSlotId> stackSlotMap;
    unsigned   callSiteCount = 0;
    bool       hasSlotIds    = false;
    bool const isFullyInterruptible;

    using StackSlotLifetime = GCInfo::StackSlotLifetime;
    using RegArgChangeKind  = GCInfo::RegArgChangeKind;
    using RegSet            = GCInfo::RegSet;
    using RegArgChange      = GCInfo::RegArgChange;
    using CallSite          = GCInfo::CallSite;

#ifdef GC_ENCODER_LOGGING
    static const char* const StackSlotBaseNames[];
    static const char* const SlotFlagsNames[];

    struct StackSlotLog
    {
        int             offset;
        GcStackSlotBase base : 8;
        GcSlotFlags     flags : 8;
        GcSlotState     state : 8;
        unsigned        codeOffs;

        StackSlotLog(const GcSlotDesc& desc, GcSlotState state, UINT32 codeOffs)
            : offset(desc.Slot.Stack.SpOffset)
            , base(desc.Slot.Stack.Base)
            , flags(desc.Flags)
            , state(state)
            , codeOffs(codeOffs)
        {
            assert(!desc.IsRegister());
        }

        bool SlotEquals(const StackSlotLog& other) const
        {
            return (offset == other.offset) && (base == other.base);
        }
    };

    struct RegSlotLog
    {
        regNumber   regNum;
        GcSlotFlags flags : 8;
        GcSlotState state : 8;
        unsigned    codeOffs;

        RegSlotLog(const GcSlotDesc& desc, GcSlotState state, UINT32 codeOffs)
            : regNum(static_cast<regNumber>(desc.Slot.RegisterNumber))
            , flags(desc.Flags)
            , state(state)
            , codeOffs(codeOffs)
        {
            assert(desc.IsRegister());
        }

        bool SlotEquals(const RegSlotLog& other) const
        {
            return regNum == other.regNum;
        }
    };

    struct InterruptibleRange
    {
        unsigned start;
        unsigned end;

        InterruptibleRange(unsigned start, unsigned end) : start(start), end(end)
        {
        }
    };

    bool const                         log;
    bool const                         diffableLog = true;
    jitstd::vector<StackSlotLog>       stackSlotLog;
    jitstd::vector<RegSlotLog>         regSlotLog;
    jitstd::vector<InterruptibleRange> interruptibleRanges;
    unsigned                           codeSize;
#endif // GC_ENCODER_LOGGING

public:
    GCEncoder(Compiler* compiler, CompIAllocator* encoderAlloc, bool isFullyInterruptible)
        : GcInfoEncoder(compiler->info.compCompHnd, compiler->info.compMethodInfo, encoderAlloc, NOMEM)
        , compiler(compiler)
        , regSlotMap(compiler->getAllocator(CMK_GC))
        , stackSlotMap(compiler->getAllocator(CMK_GC))
        , isFullyInterruptible(isFullyInterruptible)
#ifdef GC_ENCODER_LOGGING
        , log(INDEBUG(compiler->verbose || compiler->opts.dspGCtbls) || (JitConfig.JitGCInfoLogging() != 0))
        , stackSlotLog(compiler->getAllocator(CMK_DebugOnly))
        , regSlotLog(compiler->getAllocator(CMK_DebugOnly))
        , interruptibleRanges(compiler->getAllocator(CMK_DebugOnly))
#endif
    {
    }

    void SetHeaderInfo(unsigned codeSize, unsigned prologSize, ReturnKind returnKind);
    void AddTrackedStackSlots(StackSlotLifetime* firstStackSlotLifetime);
    void AddRegSlotChange(unsigned codeOffset, GcSlotState slotState, RegSet regs, RegSet byrefRegs);
    void AddCallArgStackSlot(RegArgChange* argChange);
    void RemoveCallArgStackSlots(unsigned codeOffset, RegArgChange* firstArgChange, RegArgChange* killArgsChange);
    void AddUntrackedStackSlots();
    void AddFullyInterruptibleSlots(RegArgChange* firstRegArgChange);
    void AddFullyInterruptibleRanges(Emitter* emitter, unsigned codeSize, unsigned prologSize);
    void AddPartiallyInterruptibleSlots(CallSite* firstCallSite);

    void FinalizeSlotIds()
    {
        GcInfoEncoder::FinalizeSlotIds();
        hasSlotIds = true;
    }

    void Store()
    {
#ifdef GC_ENCODER_LOGGING
        if (log && diffableLog)
        {
            DumpRegSlotLog();
            DumpStackSlotLog();
        }
#endif

        Build();
        Emit();
    }

private:
#ifdef GC_ENCODER_LOGGING
    template <typename... T>
    void Log(const char* message, T... args) const
    {
        if (log)
        {
            printf(message, args...);
        }
    }

    GcSlotId GetStackSlotId(INT32 spOffset, GcSlotFlags flags, GcStackSlotBase spBase)
    {
        GcSlotId newSlotId = GcInfoEncoder::GetStackSlotId(spOffset, flags, spBase);

        if (!diffableLog)
        {
            Log("Stack slot id for offset %d (%s0x%x) (%s) %s = %d.\n", spOffset, spOffset < 0 ? "-" : "",
                abs(spOffset), StackSlotBaseNames[spBase], SlotFlagsNames[flags & 7], newSlotId);
        }

        return newSlotId;
    }

    GcSlotId GetRegisterSlotId(UINT32 regNum, GcSlotFlags flags)
    {
        GcSlotId newSlotId = GcInfoEncoder::GetRegisterSlotId(regNum, flags);

        if (!diffableLog)
        {
            Log("Register slot id for reg %s %s = %d.\n", getRegName(regNum), SlotFlagsNames[flags & 7], newSlotId);
        }

        return newSlotId;
    }

    void SetSlotState(UINT32 instructionOffset, GcSlotId slotId, GcSlotState slotState)
    {
        GcInfoEncoder::SetSlotState(instructionOffset, slotId, slotState);

        if (!diffableLog)
        {
            Log("Set state of slot %d at instr offset 0x%x to %s.\n", slotId, instructionOffset,
                (slotState == GC_SLOT_LIVE ? "Live" : "Dead"));
        }
        else
        {
            const GcSlotDesc& desc = GetSlotDesc(slotId);

            if (desc.IsRegister())
            {
                regSlotLog.emplace_back(desc, slotState, instructionOffset);
            }
            else
            {
                stackSlotLog.emplace_back(desc, slotState, instructionOffset);
            }
        }
    }

    void DefineCallSites(UINT32* pCallSites, BYTE* pCallSiteSizes, UINT32 numCallSites)
    {
        GcInfoEncoder::DefineCallSites(pCallSites, pCallSiteSizes, numCallSites);

        if (!diffableLog || (numCallSites != 0))
        {
            Log("Defining %u call sites:\n", numCallSites);
        }

        for (UINT32 k = 0; k < numCallSites; k++)
        {
            if (!diffableLog)
            {
                Log("    Offset 0x%x, size %d.\n", pCallSites[k], pCallSiteSizes[k]);
            }
            else
            {
                Log("    %06X..%06X\n", pCallSites[k], pCallSites[k] + pCallSiteSizes[k]);
            }
        }
    }

    void DefineInterruptibleRange(UINT32 startInstructionOffset, UINT32 length)
    {
        GcInfoEncoder::DefineInterruptibleRange(startInstructionOffset, length);

        if (!diffableLog)
        {
            Log("Defining interruptible range: [0x%x, 0x%x).\n", startInstructionOffset,
                startInstructionOffset + length);
        }
        else
        {
            Log("    %06X..%06X\n", startInstructionOffset, startInstructionOffset + length);

            interruptibleRanges.emplace_back(startInstructionOffset, startInstructionOffset + length);
        }
    }

    void SetCodeLength(UINT32 length)
    {
        GcInfoEncoder::SetCodeLength(length);
        Log("Set code length to %d.\n", length);
        codeSize = length;
    }

    void SetReturnKind(ReturnKind returnKind)
    {
        GcInfoEncoder::SetReturnKind(returnKind);
        Log("Set ReturnKind to %s.\n", ReturnKindToString(returnKind));
    }

    void SetStackBaseRegister(UINT32 registerNumber)
    {
        GcInfoEncoder::SetStackBaseRegister(registerNumber);
        Log("Set stack base register to %s.\n", getRegName(registerNumber));
    }

    void SetPrologSize(UINT32 prologSize)
    {
        GcInfoEncoder::SetPrologSize(prologSize);
        Log("Set prolog size 0x%x.\n", prologSize);
    }

    void SetGSCookieStackSlot(INT32 spOffsetGSCookie, UINT32 validRangeStart, UINT32 validRangeEnd)
    {
        GcInfoEncoder::SetGSCookieStackSlot(spOffsetGSCookie, validRangeStart, validRangeEnd);
        Log("Set GS Cookie stack slot to %d, valid from 0x%x to 0x%x.\n", spOffsetGSCookie, validRangeStart,
            validRangeEnd);
    }

    void SetPSPSymStackSlot(INT32 spOffsetPSPSym)
    {
        GcInfoEncoder::SetPSPSymStackSlot(spOffsetPSPSym);
        Log("Set PSPSym stack slot to %d.\n", spOffsetPSPSym);
    }

    void SetGenericsInstContextStackSlot(INT32 spOffsetGenericsContext, GENERIC_CONTEXTPARAM_TYPE type)
    {
        GcInfoEncoder::SetGenericsInstContextStackSlot(spOffsetGenericsContext, type);
        Log("Set generic instantiation context stack slot to %d, type is %s.\n", spOffsetGenericsContext,
            (type == GENERIC_CONTEXTPARAM_THIS
                 ? "THIS"
                 : (type == GENERIC_CONTEXTPARAM_MT ? "MT" : (type == GENERIC_CONTEXTPARAM_MD ? "MD" : "UNKNOWN!"))));
    }

    void SetSecurityObjectStackSlot(INT32 spOffset)
    {
        GcInfoEncoder::SetSecurityObjectStackSlot(spOffset);
        Log("Set security object stack slot to %d.\n", spOffset);
    }

    void SetIsVarArg()
    {
        GcInfoEncoder::SetIsVarArg();
        Log("SetIsVarArg.\n");
    }

#ifdef TARGET_AMD64
    void SetWantsReportOnlyLeaf()
    {
        GcInfoEncoder::SetWantsReportOnlyLeaf();
        Log("Set WantsReportOnlyLeaf.\n");
    }
#endif

#ifdef TARGET_ARMARCH
    void SetHasTailCalls()
    {
        GcInfoEncoder::SetHasTailCalls();
        Log("Set HasTailCalls.\n");
    }
#endif

    void SetSizeOfStackOutgoingAndScratchArea(UINT32 size)
    {
        GcInfoEncoder::SetSizeOfStackOutgoingAndScratchArea(size);
        Log("Set Outgoing stack arg area size to %d.\n", size);
    }

    template <typename Slot>
    void DumpSlotRange(const jitstd::vector<Slot>& log, size_t i)
    {
        const Slot& live = log[i];

        // TODO-MIKE-Review: For dump purposes the slot flags are ignored, so that
        // we see a single slot for ref, byref and pinned. This complicates sorting,
        // a range can end at offset x and then a new range can start at the same
        // offset x (e.g. ref 2..6 byref 6..12) so we sort the 2 entries for x such
        // that the dead entry comes before the live one.
        //
        // But it looks like we can also have empty ranges (e.g. ref 6..6) and the
        // dead-live order is wrong, it should be the other way around. So we look
        // for a dead entry that follows the live entry AND has higher code offset.
        // This avoids dealing with empty ranges where the dead entry occurs before
        // the live entry.
        //
        // Perhaps we can avoid creating empty ranges in the first place?
        // It seems that they appear in partially interruptible code, when a call
        // follows another call.
        //
        // Also, it would make more sense to simply force stable sorting instead
        // of sorting by slot state, the original order should be correct. And it
        // is, for live-dead pairs. But then the pairs can be out of order, due to
        // MarkFilterStackSlotsPinned which simply inserts new pairs at the start
        // of the list.

        unsigned liveCodeOffs = live.codeOffs;
        unsigned deadCodeOffs = live.codeOffs;

        for (size_t j = i + 1; (j < log.size()) && log[j].SlotEquals(live); j++)
        {
            const Slot& dead = log[j];

            if ((dead.state == GC_SLOT_DEAD) && (dead.codeOffs > live.codeOffs))
            {
                deadCodeOffs = dead.codeOffs;
                break;
            }
        }

        if (liveCodeOffs == deadCodeOffs)
        {
            return;
        }

        if (!interruptibleRanges.empty())
        {
            DumpInterruptibleSlotRanges(liveCodeOffs, deadCodeOffs, SlotFlagsNames[live.flags & 7]);
        }
        else
        {
            printf("    %06X..%06X %s\n", liveCodeOffs, deadCodeOffs, SlotFlagsNames[live.flags & 7]);
        }
    }

    void DumpInterruptibleSlotRanges(unsigned live, unsigned dead, const char* type)
    {
        // Dump only portions of a slot's live range that are within interruptible ranges.
        // It doesn't matter what is reported inside a non-interruptible range and the
        // emitter's handling of epilogs (and funclet prologs) was kind of messed up and
        // results in slots changing state either at the start or the end of an epilog,
        // depending on whatever side of the bed the emitter woke up.

        auto& ranges = interruptibleRanges;
        auto  range  = upper_bound(ranges.begin(), ranges.end(), live,
                                 [](unsigned offs, const InterruptibleRange& range) { return offs < range.end; });

        if ((range != ranges.end()) && (dead > range->start))
        {
            printf("    %06X..%06X %s\n", Max(live, range->start), Min(dead, range->end), type);

            while ((++range != ranges.end()) && (dead > range->start))
            {
                printf("    %06X..%06X %s\n", range->start, Min(dead, range->end), type);
            }
        }
    }

    void DumpRegSlotLog()
    {
        jitstd::sort(regSlotLog.begin(), regSlotLog.end(), [](const RegSlotLog& x, const RegSlotLog& y) {
            if (x.regNum != y.regNum)
                return x.regNum < y.regNum;
            if (x.codeOffs != y.codeOffs)
                return x.codeOffs < y.codeOffs;
            return x.state < y.state;
        });

        regNumber prevRegNum = REG_NA;

        for (size_t i = 0; i < regSlotLog.size(); i++)
        {
            const RegSlotLog& slot = regSlotLog[i];

            if (slot.state == GC_SLOT_LIVE)
            {
                if (slot.regNum != prevRegNum)
                {
                    printf("Defining %s slot live ranges:\n", getRegName(slot.regNum));
                    prevRegNum = slot.regNum;
                }

                DumpSlotRange(regSlotLog, i);
            }
        }
    }

    void DumpStackSlotLog()
    {
        for (unsigned i = 0; i < GetSlotCount(); i++)
        {
            const GcSlotDesc& slot = GetSlotDesc(i);

            if (slot.IsUntracked())
            {
                stackSlotLog.emplace_back(slot, GC_SLOT_LIVE, 0);
                stackSlotLog.emplace_back(slot, GC_SLOT_DEAD, UINT_MAX);
            }
        }

        jitstd::sort(stackSlotLog.begin(), stackSlotLog.end(), [](const StackSlotLog& x, const StackSlotLog& y) {
            if (x.base != y.base)
                return x.base > y.base;
            if (x.offset != y.offset)
                return x.offset > y.offset;
            if (x.codeOffs != y.codeOffs)
                return x.codeOffs < y.codeOffs;
            return x.state < y.state;
        });

        GcStackSlotBase prevBase   = static_cast<GcStackSlotBase>(INT_MIN);
        int             prevOffset = INT_MIN;

        for (size_t i = 0; i < stackSlotLog.size(); i++)
        {
            const StackSlotLog& slot = stackSlotLog[i];

            if (slot.state != GC_SLOT_LIVE)
            {
                continue;
            }

            if ((slot.base != prevBase) || (slot.offset != prevOffset))
            {
                const char* baseName;

                switch (slot.base)
                {
                    case GC_FRAMEREG_REL:
                        baseName = getRegName(REG_FPBASE);
                        break;
                    case GC_SP_REL:
                        baseName = getRegName(REG_SPBASE);
                        break;
                    default:
                        baseName = "???";
                        break;
                }

#ifdef TARGET_ARMARCH
                printf("Defining [%s,#%d] slot live ranges:\n", baseName, slot.offset);
#else
                printf("Defining [%s%c%02XH] slot live ranges:\n", baseName, slot.offset < 0 ? '-' : '+',
                       abs(slot.offset));
#endif

                prevOffset = slot.offset;
                prevBase   = slot.base;
            }

            if ((slot.flags & GC_SLOT_UNTRACKED) == 0)
            {
                DumpSlotRange(stackSlotLog, i);
            }
            else
            {
                printf("    untracked\n");
            }
        }
    }
#endif // GC_ENCODER_LOGGING
};

#ifdef GC_ENCODER_LOGGING
const char* const GCEncoder::StackSlotBaseNames[]{"caller.sp", "sp", "frame"};

const char* const GCEncoder::SlotFlagsNames[]{"(ref)",
                                              "(byref)",
                                              "(ref, pinned)",
                                              "(byref, pinned)",
                                              "(ref, untracked)",
                                              "(byref, untracked)",
                                              "(ref, pinned, untracked)",
                                              "(byref, pinned, untracked)"};
#endif // GC_ENCODER_LOGGING

void GCEncoder::SetHeaderInfo(unsigned codeSize, unsigned prologSize, ReturnKind returnKind)
{
    SetCodeLength(codeSize);
    SetReturnKind(returnKind);

    if (compiler->codeGen->isFramePointerUsed())
    {
        SetStackBaseRegister(REG_FPBASE);
    }

    if (compiler->info.compIsVarArgs)
    {
        SetIsVarArg();
    }

    if (compiler->lvaReportParamTypeArg())
    {
        assert(compiler->info.compTypeCtxtArg != BAD_VAR_NUM);

        GENERIC_CONTEXTPARAM_TYPE ctxtParamType;

        if ((compiler->info.compMethodInfo->options & CORINFO_GENERICS_CTXT_FROM_METHODDESC) != 0)
        {
            ctxtParamType = GENERIC_CONTEXTPARAM_MD;
        }
        else
        {
            assert((compiler->info.compMethodInfo->options & CORINFO_GENERICS_CTXT_FROM_METHODTABLE) != 0);

            ctxtParamType = GENERIC_CONTEXTPARAM_MT;
        }

        const int offset = compiler->lvaToCallerSPRelativeOffset(compiler->codeGen->cachedGenericContextArgOffset,
                                                                 compiler->codeGen->isFramePointerUsed());

#ifdef DEBUG
        if (compiler->opts.IsOSR())
        {
            // Sanity check the offset vs saved patchpoint info.
            // PP info has FP relative offset, to get to caller SP we need to
            // subtract off 2 register slots (saved FP, saved RA).

            const PatchpointInfo* const ppInfo    = compiler->info.compPatchpointInfo;
            const int                   osrOffset = ppInfo->GenericContextArgOffset() - 2 * REGSIZE_BYTES;
            assert(offset == osrOffset);
        }
#endif

        SetGenericsInstContextStackSlot(offset, ctxtParamType);
    }
    else if (compiler->lvaKeepAliveAndReportThis())
    {
        assert(compiler->info.compThisArg != BAD_VAR_NUM);

        // OSR can report the root method's frame slot, if that method reported context.
        // If not, the OSR frame will have saved the needed context.
        bool useRootFrameSlot = true;

        if (compiler->opts.IsOSR())
        {
            const PatchpointInfo* const ppInfo = compiler->info.compPatchpointInfo;

            useRootFrameSlot = ppInfo->HasKeptAliveThis();
        }

        const int offset =
            compiler->lvaToCallerSPRelativeOffset(compiler->codeGen->cachedGenericContextArgOffset,
                                                  compiler->codeGen->isFramePointerUsed(), useRootFrameSlot);

#ifdef DEBUG
        if (compiler->opts.IsOSR() && useRootFrameSlot)
        {
            // Sanity check the offset vs saved patchpoint info.
            // PP info has FP relative offset, to get to caller SP we need to
            // subtract off 2 register slots (saved FP, saved RA).

            const PatchpointInfo* const ppInfo    = compiler->info.compPatchpointInfo;
            const int                   osrOffset = ppInfo->KeptAliveThisOffset() - 2 * REGSIZE_BYTES;
            assert(offset == osrOffset);
        }
#endif

        SetGenericsInstContextStackSlot(offset, GENERIC_CONTEXTPARAM_THIS);
    }

    if (compiler->getNeedsGSSecurityCookie())
    {
        assert(compiler->lvaGSSecurityCookie != BAD_VAR_NUM);

        // The offset is FP-relative, and the using code expects caller-sp relative, so translate.
        const int offset = compiler->lvaGetCallerSPRelativeOffset(compiler->lvaGSSecurityCookie);

        // The code offset ranges assume that the GS Cookie slot is initialized in the prolog, and is valid
        // through the remainder of the method.  We will not query for the GS Cookie while we're in an epilog,
        // so the question of where in the epilog it becomes invalid is moot.
        SetGSCookieStackSlot(offset, prologSize, codeSize);
    }
    else if (compiler->lvaReportParamTypeArg() || compiler->lvaKeepAliveAndReportThis())
    {
        SetPrologSize(prologSize);
    }

#ifdef FEATURE_EH_FUNCLETS
    if (compiler->lvaPSPSym != BAD_VAR_NUM)
    {
#ifdef TARGET_AMD64
        SetPSPSymStackSlot(compiler->lvaGetPSPSymInitialSPRelativeOffset());
#else
        SetPSPSymStackSlot(compiler->lvaGetCallerSPRelativeOffset(compiler->lvaPSPSym));
#endif
    }

#ifdef TARGET_AMD64
    if (compiler->ehAnyFunclets())
    {
        // Set this to avoid double-reporting the parent frame (unlike JIT64)
        SetWantsReportOnlyLeaf();
    }
#endif
#endif // FEATURE_EH_FUNCLETS

#ifdef TARGET_ARMARCH
    if (compiler->codeGen->GetHasTailCalls())
    {
        SetHasTailCalls();
    }
#endif

#if FEATURE_FIXED_OUT_ARGS
    SetSizeOfStackOutgoingAndScratchArea(compiler->codeGen->outgoingArgSpaceSize);
#endif

#if defined(TARGET_ARM64) || defined(TARGET_AMD64)
    if (compiler->opts.compDbgEnC)
    {
        // what we have to preserve is called the "frame header" (see comments in VM\eetwain.cpp)
        // which is:
        //  -return address
        //  -saved off RBP
        //  -saved 'this' pointer and bool for synchronized methods

        // 4 slots for RBP + return address + RSI + RDI
        int preservedAreaSize = 4 * REGSIZE_BYTES;

        if ((compiler->info.compFlags & CORINFO_FLG_SYNCH) != 0)
        {
            if ((compiler->info.compFlags & CORINFO_FLG_STATIC) == 0)
            {
                preservedAreaSize += REGSIZE_BYTES;
            }

#ifdef TARGET_ARM64
            // bool for synchronized methods
            preservedAreaSize += 1;
#else
            preservedAreaSize += 4;
#endif
        }

        SetSizeOfEditAndContinuePreservedArea(preservedAreaSize);
    }
#endif

    if (compiler->opts.IsReversePInvoke())
    {
        LclVarDsc* reversePInvokeFrameLcl = compiler->lvaGetDesc(compiler->lvaReversePInvokeFrameVar);
        SetReversePInvokeFrameSlot(reversePInvokeFrameLcl->GetStackOffset());
    }

#if DISPLAY_SIZES
    (isFullyInterruptible ? genMethodICnt : genMethodNCnt)++;
#endif
}

void GCEncoder::AddUntrackedStackSlots()
{
    for (unsigned lclNum = 0; lclNum < compiler->lvaCount; lclNum++)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

        if (lcl->IsDependentPromotedField(compiler) || !lcl->lvOnFrame || lcl->HasGCSlotLiveness())
        {
            continue;
        }

        if (varTypeIsGC(lcl->GetType()))
        {
            GcSlotFlags slotFlags = GC_SLOT_UNTRACKED;

            if (lcl->TypeIs(TYP_BYREF))
            {
                slotFlags = static_cast<GcSlotFlags>(slotFlags | GC_SLOT_INTERIOR);
            }

            if (lcl->IsPinning())
            {
                slotFlags = static_cast<GcSlotFlags>(slotFlags | GC_SLOT_PINNED);
            }

            GcStackSlotBase slotBase = lcl->lvFramePointerBased ? GC_FRAMEREG_REL : GC_SP_REL;

            GetStackSlotId(lcl->GetStackOffset(), slotFlags, slotBase);
        }
        else if (lcl->TypeIs(TYP_STRUCT) && lcl->HasGCPtr())
        {
            GcStackSlotBase slotBase  = lcl->lvFramePointerBased ? GC_FRAMEREG_REL : GC_SP_REL;
            int             lclOffset = lcl->GetStackOffset();

#if DOUBLE_ALIGN
            if (compiler->genDoubleAlign() && lcl->IsParam() && !lcl->IsRegParam())
            {
                lclOffset += compiler->codeGen->genTotalFrameSize();
            }
#endif

            ClassLayout* layout = lcl->GetLayout();

            for (unsigned i = 0, slots = layout->GetSlotCount(); i < slots; i++)
            {
                if (!layout->IsGCPtr(i))
                {
                    continue;
                }

                GcSlotFlags slotFlags = GC_SLOT_UNTRACKED;

                if (layout->GetGCPtrType(i) == TYP_BYREF)
                {
                    slotFlags = static_cast<GcSlotFlags>(slotFlags | GC_SLOT_INTERIOR);
                }

                GetStackSlotId(lclOffset + i * TARGET_POINTER_SIZE, slotFlags, slotBase);
            }
        }
    }

    GcStackSlotBase slotBase = compiler->codeGen->isFramePointerUsed() ? GC_FRAMEREG_REL : GC_SP_REL;

    if (!compiler->codeGen->spillTemps.TrackGCSpillTemps())
    {
        for (const SpillTemp& temp : compiler->codeGen->spillTemps)
        {
            if (!varTypeIsGC(temp.GetType()))
            {
                continue;
            }

            GcSlotFlags slotFlags = GC_SLOT_UNTRACKED;

            if (temp.GetType() == TYP_BYREF)
            {
                slotFlags = static_cast<GcSlotFlags>(slotFlags | GC_SLOT_INTERIOR);
            }

            GetStackSlotId(temp.GetOffset(), slotFlags, slotBase);
        }
    }

    if (compiler->lvaKeepAliveAndReportThis())
    {
        assert(!compiler->lvaReportParamTypeArg());
        assert(compiler->lvaGetDesc(compiler->info.compThisArg)->TypeIs(TYP_REF));

        GetStackSlotId(compiler->codeGen->cachedGenericContextArgOffset, GC_SLOT_UNTRACKED, slotBase);
    }
}

void GCEncoder::AddFullyInterruptibleSlots(RegArgChange* firstRegArgChange)
{
    assert(isFullyInterruptible);

    RegSet        gcRegs         = RBM_NONE;
    RegArgChange* firstArgChange = nullptr;

    for (RegArgChange* change = firstRegArgChange; change != nullptr; change = change->next)
    {
        if (change->kind == RegArgChangeKind::StoreArg)
        {
            AddCallArgStackSlot(change);

            if (firstArgChange == nullptr)
            {
                firstArgChange = change;
            }
        }
        else if (change->kind == RegArgChangeKind::KillArgs)
        {
            if (hasSlotIds)
            {
                RemoveCallArgStackSlots(change->codeOffs, firstArgChange, change);
            }

            firstArgChange = nullptr;
        }
        else if (change->kind == RegArgChangeKind::AddRegs)
        {
            assert(change->gcType != GCT_NONE);

            RegSet regs      = change->regs & ~gcRegs;
            RegSet byrefRegs = change->gcType == GCT_BYREF ? regs : RBM_NONE;
            AddRegSlotChange(change->codeOffs, GC_SLOT_LIVE, regs, byrefRegs);
            gcRegs |= regs;
        }
        else
        {
            assert(change->kind == RegArgChangeKind::RemoveRegs);
            assert(change->gcType != GCT_NONE);

            RegSet regs      = change->regs & gcRegs;
            RegSet byrefRegs = change->gcType == GCT_BYREF ? regs : RBM_NONE;
            AddRegSlotChange(change->codeOffs, GC_SLOT_DEAD, regs, byrefRegs);
            gcRegs &= ~regs;
        }
    }
}

void GCEncoder::AddFullyInterruptibleRanges(Emitter* emitter, unsigned codeSize, unsigned prologSize)
{
    assert(isFullyInterruptible);
    assert(prologSize <= codeSize);

    unsigned prevOffset = prologSize;

#ifdef GC_ENCODER_LOGGING
    Log("Defining interruptible ranges:\n");
#endif

    emitter->EnumerateNoGCInsGroups([&](unsigned funcletIndex, unsigned offset, unsigned size) {
        if (offset < prevOffset)
        {
            // We're still in the main method prolog, which has already had it's interruptible range reported.
            assert(funcletIndex == 0);
            assert(offset + size <= prevOffset);
        }
        else
        {
            assert(offset >= prevOffset);

            if (offset > prevOffset)
            {
                DefineInterruptibleRange(prevOffset, offset - prevOffset);
            }

            prevOffset = offset + size;
        }
    });

    if (prevOffset < codeSize)
    {
        DefineInterruptibleRange(prevOffset, codeSize - prevOffset);
    }
}

void GCEncoder::AddPartiallyInterruptibleSlots(CallSite* firstCallSite)
{
    assert(!isFullyInterruptible);

    unsigned  callSiteIndex = 0;
    unsigned* callSites     = nullptr;
    uint8_t*  callSiteSizes = nullptr;

    if (hasSlotIds)
    {
        if (callSiteCount == 0)
        {
            return;
        }

        callSites     = new (compiler, CMK_GC) unsigned[callSiteCount];
        callSiteSizes = new (compiler, CMK_GC) uint8_t[callSiteCount];
    }

    // TODO-MIKE-Review: Probably this should check if there are any tracked slots, instead of
    // trying to deduce that from other conditions that imply that all slots are untracked.

    const bool noTrackedGCSlots = compiler->codeGen->isFramePointerUsed() && compiler->opts.MinOpts() &&
                                  !compiler->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT) &&
                                  !JitConfig.JitMinOptsTrackGCrefs();

    for (CallSite* call = firstCallSite; call != nullptr; call = call->next)
    {
        RegSet refRegs   = call->refRegs & RBM_CALLEE_SAVED;
        RegSet byrefRegs = call->byrefRegs & RBM_CALLEE_SAVED;

        assert((refRegs & byrefRegs) == RBM_NONE);

        RegSet gcRegs = refRegs | byrefRegs;

        if (noTrackedGCSlots && (gcRegs == RBM_NONE))
        {
            continue;
        }

        unsigned callOffs    = call->codeOffs;
        unsigned callEndOffs = call->codeEndOffs;

        assert(callEndOffs - callOffs <= UINT8_MAX);

        if (callSites != nullptr)
        {
            callSites[callSiteIndex]     = callOffs;
            callSiteSizes[callSiteIndex] = static_cast<uint8_t>(callEndOffs - callOffs);
        }

        callSiteIndex++;

        AddRegSlotChange(callOffs, GC_SLOT_LIVE, gcRegs, byrefRegs);
        AddRegSlotChange(callEndOffs, GC_SLOT_DEAD, gcRegs, byrefRegs);
    }

    if (hasSlotIds)
    {
        assert(callSiteIndex == callSiteCount);
        DefineCallSites(callSites, callSiteSizes, callSiteIndex);
    }
    else
    {
        callSiteCount = callSiteIndex;
    }
}

void GCEncoder::AddRegSlotChange(unsigned codeOffset, GcSlotState slotState, RegSet regs, RegSet byrefRegs)
{
    assert((byrefRegs & ~regs) == RBM_NONE);

    for (RegSet regMask; regs != RBM_NONE; regs &= ~regMask)
    {
        regMask = genFindLowestBit(regs);
        assert(regMask != RBM_NONE);

        regNumber reg = genRegNumFromMask(static_cast<regMaskTP>(regMask));

        assert(reg != REG_SPBASE);

        GcSlotFlags slotFlags = GC_SLOT_BASE;

        if ((regMask & byrefRegs) != RBM_NONE)
        {
            slotFlags = static_cast<GcSlotFlags>(slotFlags | GC_SLOT_INTERIOR);
        }

        RegSlotIdKey slotKey(reg, slotFlags);
        GcSlotId     slotId;
        bool         found = regSlotMap.Lookup(slotKey, &slotId);

        if (!hasSlotIds)
        {
            if (!found)
            {
                regSlotMap.Set(slotKey, GetRegisterSlotId(reg, slotFlags));
            }
        }
        else
        {
            assert(found);

            SetSlotState(codeOffset, slotId, slotState);
        }
    }
}

void GCEncoder::AddTrackedStackSlots(StackSlotLifetime* firstStackSlotLifetime)
{
    GcStackSlotBase slotBaseReg = compiler->codeGen->isFramePointerUsed() ? GC_FRAMEREG_REL : GC_SP_REL;

    for (StackSlotLifetime* lifetime = firstStackSlotLifetime; lifetime != nullptr; lifetime = lifetime->next)
    {
        const unsigned beginCodeOffs = lifetime->beginCodeOffs;
        const unsigned endCodeOffs   = lifetime->endCodeOffs;

        if (beginCodeOffs == endCodeOffs)
        {
            continue;
        }

        int slotOffs = static_cast<int>(lifetime->slotOffset);

        GcSlotFlags slotFlags = GC_SLOT_BASE;

        if ((slotOffs & byref_OFFSET_FLAG) != 0)
        {
            slotFlags = static_cast<GcSlotFlags>(slotFlags | GC_SLOT_INTERIOR);
        }

        if ((slotOffs & pinned_OFFSET_FLAG) != 0)
        {
            slotFlags = static_cast<GcSlotFlags>(slotFlags | GC_SLOT_PINNED);
        }

        slotOffs &= ~OFFSET_MASK;

        StackSlotIdKey slotKey(slotOffs, slotFlags, slotBaseReg);
        GcSlotId       slotId;
        bool           found = stackSlotMap.Lookup(slotKey, &slotId);

        if (!hasSlotIds)
        {
            if (!found)
            {
                stackSlotMap.Set(slotKey, GetStackSlotId(slotOffs, slotFlags, slotBaseReg));
            }
        }
        else
        {
            assert(found);

            SetSlotState(beginCodeOffs, slotId, GC_SLOT_LIVE);
            SetSlotState(endCodeOffs, slotId, GC_SLOT_DEAD);
        }
    }
}

void GCEncoder::AddCallArgStackSlot(RegArgChange* argChange)
{
    assert(argChange->gcType != GCT_NONE);
    assert(argChange->kind == RegArgChangeKind::StoreArg);
    assert(isFullyInterruptible);

    GcSlotFlags    slotFlags = argChange->gcType == GCT_BYREF ? GC_SLOT_INTERIOR : GC_SLOT_BASE;
    StackSlotIdKey slotKey(argChange->argOffset, slotFlags, GC_SP_REL);
    GcSlotId       slotId;
    bool           found = stackSlotMap.Lookup(slotKey, &slotId);

    if (!hasSlotIds)
    {
        if (!found)
        {
            stackSlotMap.Set(slotKey, GetStackSlotId(argChange->argOffset, slotFlags, GC_SP_REL));
        }
    }
    else
    {
        assert(found);
        SetSlotState(argChange->codeOffs, slotId, GC_SLOT_LIVE);
    }
}

void GCEncoder::RemoveCallArgStackSlots(unsigned codeOffset, RegArgChange* firstArgChange, RegArgChange* killArgsChange)
{
    assert(isFullyInterruptible);

    for (RegArgChange* change = firstArgChange; change != killArgsChange; change = change->next)
    {
        if ((change->kind == RegArgChangeKind::AddRegs) || (change->kind == RegArgChangeKind::RemoveRegs))
        {
            continue;
        }

        assert(change->gcType != GCT_NONE);
        assert(change->kind == RegArgChangeKind::StoreArg);

        GcSlotFlags slotFlags = change->gcType == GCT_BYREF ? GC_SLOT_INTERIOR : GC_SLOT_BASE;
        GcSlotId    slotId;
        bool        found = stackSlotMap.Lookup({change->argOffset, slotFlags, GC_SP_REL}, &slotId);
        assert(found);
        SetSlotState(codeOffset, slotId, GC_SLOT_DEAD);
    }
}

void GCInfo::CreateAndStoreGCInfo(CodeGen* codeGen, unsigned codeSize, unsigned prologSize)
{
#ifdef DEBUG
    // Tracked variables can't be pinned, and the encoding takes advantage of that by
    // using the same bit for 'pinned' and 'this'. Since we don't track 'this', we should
    // never see either flag here. Check it now before we potentially add some pinned flags.
    for (StackSlotLifetime* lifetime = firstStackSlotLifetime; lifetime != nullptr; lifetime = lifetime->next)
    {
        const unsigned flags = lifetime->slotOffset & OFFSET_MASK;

        assert((flags & pinned_OFFSET_FLAG) == 0);
        assert((flags & this_OFFSET_FLAG) == 0);
    }
#endif

    if (compiler->ehAnyFunclets())
    {
        MarkFilterStackSlotsPinned(codeGen);
    }

    CompIAllocator encoderAlloc(compiler->getAllocator(CMK_GC));
    GCEncoder      encoder(compiler, &encoderAlloc, isFullyInterruptible);

    encoder.SetHeaderInfo(codeSize, prologSize, GetReturnKind(compiler->info));
    encoder.AddUntrackedStackSlots();
    encoder.AddTrackedStackSlots(firstStackSlotLifetime);

    if (isFullyInterruptible)
    {
        encoder.AddFullyInterruptibleSlots(firstRegArgChange);
        encoder.FinalizeSlotIds();

        encoder.AddTrackedStackSlots(firstStackSlotLifetime);
        encoder.AddFullyInterruptibleSlots(firstRegArgChange);
        encoder.AddFullyInterruptibleRanges(codeGen->GetEmitter(), codeSize, prologSize);
    }
    else
    {
        encoder.AddPartiallyInterruptibleSlots(firstCallSite);
        encoder.FinalizeSlotIds();

        encoder.AddTrackedStackSlots(firstStackSlotLifetime);
        encoder.AddPartiallyInterruptibleSlots(firstCallSite);
    }

    encoder.Store();
}

#endif // !JIT32_GCENCODER
