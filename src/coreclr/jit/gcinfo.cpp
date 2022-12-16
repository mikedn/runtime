// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "gcinfo.h"
#include "emit.h"
#include "jitgcinfo.h"
#ifdef TARGET_AMD64
#include "gcinfoencoder.h" //this includes a LOT of other files too
#endif
#include "codegen.h"

#if MEASURE_PTRTAB_SIZE
size_t GCInfo::s_gcRegPtrDscSize   = 0;
size_t GCInfo::s_gcTotalPtrTabSize = 0;
#endif

GCInfo::WriteBarrierForm GCInfo::GetWriteBarrierForm(GenTreeStoreInd* store)
{
    if (!store->TypeIs(TYP_REF))
    {
        // Only object references need write barriers.
        // A managed pointer cannot be stored in the managed heap so we'll
        // treat it as any other value that doesn't require a write barrier.
        return WBF_NoBarrier;
    }

    if (store->GetValue()->OperIsConst())
    {
        // Constant values (normally null since there aren't any other
        // TYP_REF constants) cannot represent GC heap objects so no
        // write barrier is needed.
        return WBF_NoBarrier;
    }

    if ((store->gtFlags & GTF_IND_TGT_NOT_HEAP) != 0)
    {
        // This indirection is not from to the heap.
        // This case occurs for stack-allocated objects.
        return WBF_NoBarrier;
    }

    WriteBarrierForm form = GetWriteBarrierFormFromAddress(store->GetAddr());

    if (form == WBF_BarrierUnknown)
    {
        // If we can't figure out where the address is then use TGT_HEAP to
        // select between checked and unchecked barriers.

        form = ((store->gtFlags & GTF_IND_TGT_HEAP) != 0) ? WBF_BarrierUnchecked : WBF_BarrierChecked;
    }

    return form;
}

/*****************************************************************************
 *
 *  Allocate a new pointer register set / pointer argument entry and append
 *  it to the list.
 */

GCInfo::regPtrDsc* GCInfo::gcRegPtrAllocDsc()
{
    regPtrDsc* regPtrNext;

    assert(compiler->codeGen->IsFullPtrRegMapRequired());

    /* Allocate a new entry and initialize it */

    regPtrNext = new (compiler, CMK_GC) regPtrDsc;

    regPtrNext->rpdIsThis = false;

    regPtrNext->rpdOffs = 0;
    regPtrNext->rpdNext = nullptr;

    // Append the entry to the end of the list.
    if (gcRegPtrLast == nullptr)
    {
        assert(gcRegPtrList == nullptr);
        gcRegPtrList = gcRegPtrLast = regPtrNext;
    }
    else
    {
        assert(gcRegPtrList != nullptr);
        gcRegPtrLast->rpdNext = regPtrNext;
        gcRegPtrLast          = regPtrNext;
    }

#if MEASURE_PTRTAB_SIZE
    s_gcRegPtrDscSize += sizeof(*regPtrNext);
#endif

    return regPtrNext;
}

#ifdef JIT32_GCENCODER

/*****************************************************************************
 *
 *  Compute the various counts that get stored in the info block header.
 */

void GCInfo::gcCountForHeader(UNALIGNED unsigned int* pUntrackedCount, UNALIGNED unsigned int* pVarPtrTableSize)
{
    unsigned   varNum;
    LclVarDsc* varDsc;

    bool         keepThisAlive  = false; // did we track "this" in a synchronized method?
    unsigned int untrackedCount = 0;

    // Count the untracked locals and non-enregistered args.

    for (varNum = 0, varDsc = compiler->lvaTable; varNum < compiler->lvaCount; varNum++, varDsc++)
    {
        if (varDsc->IsDependentPromotedField(compiler))
        {
            // A dependent promoted struct field must be reported through its parent local.
            continue;
        }

        if (varTypeIsGC(varDsc->TypeGet()))
        {
            if (!gcIsUntrackedLocalOrNonEnregisteredArg(varNum, &keepThisAlive))
            {
                continue;
            }

#ifdef DEBUG
            if (compiler->verbose)
            {
                int offs = varDsc->GetStackOffset();

                printf("GCINFO: untrckd %s lcl at [%s", varTypeGCstring(varDsc->TypeGet()),
                       compiler->GetEmitter()->emitGetFrameReg());

                if (offs < 0)
                {
                    printf("-%02XH", -offs);
                }
                else if (offs > 0)
                {
                    printf("+%02XH", +offs);
                }

                printf("]\n");
            }
#endif

            untrackedCount++;
        }
        else if ((varDsc->TypeGet() == TYP_STRUCT) && varDsc->lvOnFrame)
        {
            untrackedCount += varDsc->GetLayout()->GetGCPtrCount();
        }
    }

    // Also count spill temps that hold pointers.

    for (SpillTemp& temp : compiler->codeGen->spillTemps)
    {
        if (!varTypeIsGC(temp.GetType()))
        {
            continue;
        }

#ifdef DEBUG
        if (compiler->verbose)
        {
            int offs = temp.GetOffset();

            printf("GCINFO: untrck %s Temp at [%s", varTypeGCstring(varDsc->TypeGet()),
                   compiler->GetEmitter()->emitGetFrameReg());

            if (offs < 0)
            {
                printf("-%02XH", -offs);
            }
            else if (offs > 0)
            {
                printf("+%02XH", +offs);
            }

            printf("]\n");
        }
#endif

        untrackedCount++;
    }

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("GCINFO: untrckVars = %u\n", untrackedCount);
    }
#endif

    *pUntrackedCount = untrackedCount;

    // Count the number of entries in the table of non-register pointer variable lifetimes.

    unsigned int varPtrTableSize = 0;

    if (keepThisAlive)
    {
        varPtrTableSize++;
    }

    if (gcVarPtrList != nullptr)
    {
        // We'll use a delta encoding for the lifetime offsets.

        for (varPtrDsc* varTmp = gcVarPtrList; varTmp != nullptr; varTmp = varTmp->vpdNext)
        {
            // Special case: skip any 0-length lifetimes.

            if (varTmp->vpdBegOfs == varTmp->vpdEndOfs)
            {
                continue;
            }

            varPtrTableSize++;
        }
    }

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("GCINFO: trackdLcls = %u\n", varPtrTableSize);
    }
#endif

    *pVarPtrTableSize = varPtrTableSize;
}

//------------------------------------------------------------------------
// gcIsUntrackedLocalOrNonEnregisteredArg: Check if this varNum with GC type
// corresponds to an untracked local or argument that was not fully enregistered.
//
//
// Arguments:
//   varNum - the variable number to check;
//   pKeepThisAlive - if !FEATURE_EH_FUNCLETS and the argument != nullptr remember
//   if `this` should be kept alive and considered tracked.
//
// Return value:
//   true if it an untracked pointer value.
//
bool GCInfo::gcIsUntrackedLocalOrNonEnregisteredArg(unsigned varNum, bool* pKeepThisAlive)
{
    LclVarDsc* varDsc = compiler->lvaGetDesc(varNum);

    assert(!varDsc->IsDependentPromotedField(compiler));
    assert(varTypeIsGC(varDsc->GetType()));

    if (!varDsc->IsParam())
    {
        // If is pinned, it must be an untracked local.
        assert(!varDsc->lvPinned || !varDsc->lvTracked);

        if (varDsc->lvTracked || !varDsc->lvOnFrame)
        {
            return false;
        }
    }
    else
    {
        // Stack-passed arguments which are not enregistered are always reported in this "untracked stack pointers"
        // section of the GC info even if lvTracked==true.

        // Has this argument been fully enregistered?
        if (!varDsc->lvOnFrame)
        {
            // If a CEE_JMP has been used, then we need to report all the arguments even if they are enregistered, since
            // we will be using this value in JMP call.  Note that this is subtle as we require that argument offsets
            // are always fixed up properly even if lvRegister is set .
            if (!compiler->compJmpOpUsed)
            {
                return false;
            }
        }
        else if (varDsc->IsRegParam() && varDsc->HasLiveness())
        {
            // If this register-passed arg is tracked, then it has been allocated space near the other pointer variables
            // and we have accurate life-time info. It will be reported with gcVarPtrList in the "tracked-pointer"
            // section.
            return false;
        }
    }

#if !defined(FEATURE_EH_FUNCLETS)
    if (compiler->lvaIsOriginalThisArg(varNum) && compiler->lvaKeepAliveAndReportThis())
    {
        // "this" is in the untracked variable area, but encoding of untracked variables does not support reporting
        // "this". So report it as a tracked variable with a liveness extending over the entire method.
        //
        // TODO-x86-Cleanup: the semantic here is not clear, it would be useful to check different cases and
        // add a description where "this" is saved and how it is tracked in each of them:
        // 1) when FEATURE_EH_FUNCLETS defined (x86 Linux);
        // 2) when FEATURE_EH_FUNCLETS not defined, lvaKeepAliveAndReportThis == true, compJmpOpUsed == true;
        // 3) when there is regPtrDsc for "this", but keepThisAlive == true;
        // etc.

        if (pKeepThisAlive != nullptr)
        {
            *pKeepThisAlive = true;
        }
        return false;
    }
#endif // !FEATURE_EH_FUNCLETS
    return true;
}

/*****************************************************************************
 *
 *  Shutdown the 'pointer value' register tracking logic and save the necessary
 *  info (which will be used at runtime to locate all pointers) at the specified
 *  address. The number of bytes written to 'destPtr' must be identical to that
 *  returned from gcPtrTableSize().
 */

BYTE* GCInfo::gcPtrTableSave(BYTE* destPtr, const InfoHdr& header, unsigned codeSize, size_t* pArgTabOffset)
{
    /* Write the tables to the info block */

    return destPtr + gcMakeRegPtrTable(destPtr, -1, header, codeSize, pArgTabOffset);
}

/*****************************************************************************
 *
 *  Helper passed to genEmitter.emitGenEpilogLst() to generate
 *  the table of epilogs.
 */

/* static */ size_t GCInfo::gcRecordEpilog(void* pCallBackData, unsigned offset)
{
    GCInfo* gcInfo = (GCInfo*)pCallBackData;

    assert(gcInfo);

    size_t result = encodeUDelta(gcInfo->gcEpilogTable, offset, gcInfo->gcEpilogPrevOffset);

    if (gcInfo->gcEpilogTable)
        gcInfo->gcEpilogTable += result;

    gcInfo->gcEpilogPrevOffset = offset;

    return result;
}

#endif // JIT32_GCENCODER

GCInfo::WriteBarrierForm GCInfo::GetWriteBarrierFormFromAddress(GenTree* addr)
{
    if (addr->IsIntegralConst(0))
    {
        // If the address is null it doesn't need a write barrier. Other constants
        // typically need write barriers, usually they're GC statics.
        return GCInfo::WBF_NoBarrier;
    }

    if (!addr->TypeIs(TYP_BYREF))
    {
        // Normally object references should be stored to the GC heap via managed pointers.
        //
        // If it is an unmanaged pointer then it's not tracked so its value may very well
        // be bogus. If it's an object reference then it means that we're trying to store
        // an object reference into the method table pointer field of an object...
        //
        // There's also the special case of GC statics - in some cases the static address
        // is an unmanaged pointer (a constant) but a write barrier is still required.
        //
        // To keep things simple and safe just emit a checked barrier in all cases.

        return GCInfo::WBF_BarrierChecked;
    }

    for (addr = addr->gtSkipReloadOrCopy(); addr->OperIs(GT_ADD, GT_LEA); addr = addr->gtSkipReloadOrCopy())
    {
        GenTree* op1 = addr->AsOp()->gtOp1;
        GenTree* op2 = addr->AsOp()->gtOp2;

        if ((op1 != nullptr) && op1->TypeIs(TYP_BYREF, TYP_REF))
        {
            assert((op2 == nullptr) || !op2->TypeIs(TYP_BYREF, TYP_REF));

            addr = op1;
        }
        else if ((op2 != nullptr) && op2->TypeIs(TYP_BYREF, TYP_REF))
        {
            addr = op2;
        }
        else
        {
            // At least one operand has to be a GC pointer, otherwise it means that
            // we're dealing with unmanaged pointers pointing into the GC heap...
            return GCInfo::WBF_BarrierUnknown;
        }
    }

    if (addr->TypeIs(TYP_REF))
    {
        // If we found an object reference then this should be a store the GC heap,
        // unless we're dealing with weird code that converts an unmanaged pointer
        // to TYP_REF...

        return GCInfo::WBF_BarrierUnchecked;
    }

    if (addr->OperIs(GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR))
    {
        // No need for a GC barrier when writing to a local variable.
        return GCInfo::WBF_NoBarrier;
    }

    return GCInfo::WBF_BarrierUnknown;
}

const regMaskTP GCInfo::raRbmCalleeSaveOrder[]{RBM_CALLEE_SAVED_ORDER};

regMaskSmall GCInfo::RegMaskFromCalleeSavedMask(unsigned short calleeSaveMask)
{
    regMaskSmall res = 0;
    for (int i = 0; i < CNT_CALLEE_SAVED; i++)
    {
        if ((calleeSaveMask & ((regMaskTP)1 << i)) != 0)
        {
            res |= raRbmCalleeSaveOrder[i];
        }
    }
    return res;
}

#ifdef JIT32_GCENCODER

void* CodeGen::genCreateAndStoreGCInfo(unsigned codeSize,
                                       unsigned prologSize,
                                       unsigned epilogSize DEBUGARG(void* codePtr))
{
    BYTE    headerBuf[64];
    InfoHdr header;

    int s_cached;

#ifdef FEATURE_EH_FUNCLETS
    // We should do this before gcInfoBlockHdrSave since varPtrTableSize must be finalized before it
    if (compiler->ehAnyFunclets())
    {
        gcInfo.gcMarkFilterVarsPinned();
    }
#endif

#ifdef DEBUG
    size_t headerSize =
#endif
        compInfoBlkSize = gcInfo.gcInfoBlockHdrSave(headerBuf, 0, codeSize, prologSize, epilogSize,
                                                    calleeSavedModifiedRegs, &header, &s_cached);

    size_t argTabOffset = 0;
    size_t ptrMapSize   = gcInfo.gcPtrTableSize(header, codeSize, &argTabOffset);

#if DISPLAY_SIZES

    if (GetInterruptible())
    {
        gcHeaderISize += compInfoBlkSize;
        gcPtrMapISize += ptrMapSize;
    }
    else
    {
        gcHeaderNSize += compInfoBlkSize;
        gcPtrMapNSize += ptrMapSize;
    }

#endif // DISPLAY_SIZES

    compInfoBlkSize += ptrMapSize;

    /* Allocate the info block for the method */

    BYTE* infoBlkAddr = (BYTE*)compiler->info.compCompHnd->allocGCInfo(compInfoBlkSize);

#if 0 // VERBOSE_SIZES
    // TODO-X86-Cleanup: 'dataSize', below, is not defined

//  if  (compInfoBlkSize > codeSize && compInfoBlkSize > 100)
    {
        printf("[%7u VM, %7u+%7u/%7u x86 %03u/%03u%%] %s.%s\n",
            compiler->info.compILCodeSize,
            compInfoBlkSize,
            codeSize + dataSize,
            codeSize + dataSize - prologSize - epilogSize,
            100 * (codeSize + dataSize) / compiler->info.compILCodeSize,
            100 * (codeSize + dataSize + compInfoBlkSize) / compiler->info.compILCodeSize,
            compiler->info.compClassName,
            compiler->info.compMethodName);
    }

#endif

    /* Fill in the info block and return it to the caller */

    void* infoPtr = infoBlkAddr;

    /* Create the method info block: header followed by GC tracking tables */

    infoBlkAddr += gcInfo.gcInfoBlockHdrSave(infoBlkAddr, -1, codeSize, prologSize, epilogSize, calleeSavedModifiedRegs,
                                             &header, &s_cached);

    assert(infoBlkAddr == (BYTE*)infoPtr + headerSize);
    infoBlkAddr = gcInfo.gcPtrTableSave(infoBlkAddr, header, codeSize, &argTabOffset);
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
        printf("GC info size = %3u\n", compInfoBlkSize);

        size = gcInfo.gcInfoBlockHdrDump(base, &dumpHeader, &methodSize);
        // printf("size of header encoding is %3u\n", size);
        printf("\n");

        base += size;
        size = gcInfo.gcDumpPtrTable(base, dumpHeader, methodSize);
        // printf("size of pointer table is %3u\n", size);
        printf("\n");
        noway_assert(infoBlkAddr == (base + size));
    }
#endif // DUMP_GC_TABLES

    /* Make sure we ended up generating the expected number of bytes */

    noway_assert(infoBlkAddr == (BYTE*)infoPtr + compInfoBlkSize);

    return infoPtr;
}

#else

void CodeGen::genCreateAndStoreGCInfo(unsigned codeSize, unsigned prologSize DEBUGARG(void* codePtr))
{
    IAllocator*    allowZeroAlloc = new (compiler, CMK_GC) CompIAllocator(compiler->getAllocatorGC());
    GcInfoEncoder* gcInfoEncoder  = new (compiler, CMK_GC)
        GcInfoEncoder(compiler->info.compCompHnd, compiler->info.compMethodInfo, allowZeroAlloc, NOMEM);
    assert(gcInfoEncoder != nullptr);

    gcInfo.gcInfoBlockHdrSave(gcInfoEncoder, codeSize, prologSize);

    unsigned callCnt = 0;
    gcInfo.gcMakeRegPtrTable(gcInfoEncoder, codeSize, prologSize, GCInfo::MAKE_REG_PTR_MODE_ASSIGN_SLOTS, &callCnt);
    gcInfoEncoder->FinalizeSlotIds();
    gcInfo.gcMakeRegPtrTable(gcInfoEncoder, codeSize, prologSize, GCInfo::MAKE_REG_PTR_MODE_DO_WORK, &callCnt);

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

        gcInfoEncoder->SetSizeOfEditAndContinuePreservedArea(preservedAreaSize);
    }
#endif

    if (compiler->opts.IsReversePInvoke())
    {
        LclVarDsc* reversePInvokeFrameLcl = compiler->lvaGetDesc(compiler->lvaReversePInvokeFrameVar);
        gcInfoEncoder->SetReversePInvokeFrameSlot(reversePInvokeFrameLcl->GetStackOffset());
    }

    gcInfoEncoder->Build();
    gcInfoEncoder->Emit();
}

#endif
