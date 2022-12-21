// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "jitgcinfo.h"
#include "codegen.h"

#if MEASURE_PTRTAB_SIZE
size_t GCInfo::s_gcRegPtrDscSize;
size_t GCInfo::s_gcTotalPtrTabSize;
#endif

GCInfo::GCInfo(Compiler* compiler)
    : compiler(compiler)
#ifndef JIT32_GCENCODER
    , regSlotMap(compiler->getAllocator(CMK_GC))
    , stackSlotMap(compiler->getAllocator(CMK_GC))
#endif
{
}

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

GCInfo::StackSlotLifetime* GCInfo::BeginStackSlotLifetime(int slotOffs, unsigned codeOffs)
{
    StackSlotLifetime* lifetime = new (compiler, CMK_GC) StackSlotLifetime(slotOffs, codeOffs);

    if (firstStackSlotLifetime == nullptr)
    {
        assert(lastStackSlotLifetime == nullptr);

        firstStackSlotLifetime = lifetime;
    }
    else
    {
        lastStackSlotLifetime->next = lifetime;
    }

    lastStackSlotLifetime = lifetime;

    return lifetime;
}

void GCInfo::EndStackSlotLifetime(StackSlotLifetime* lifetime DEBUGARG(int slotOffs), unsigned codeOffs)
{
    assert(lifetime->endCodeOffs == 0);
    assert(static_cast<int>(lifetime->slotOffset & ~OFFSET_MASK) == slotOffs);

    lifetime->endCodeOffs = codeOffs;
}

GCInfo::RegArgChange* GCInfo::AddRegArgChange()
{
    assert(compiler->codeGen->IsFullPtrRegMapRequired());

    RegArgChange* change = new (compiler, CMK_GC) RegArgChange;

    if (firstRegArgChange == nullptr)
    {
        assert(lastRegArgChange == nullptr);

        firstRegArgChange = change;
    }
    else
    {
        lastRegArgChange->next = change;
    }

    lastRegArgChange = change;

#if MEASURE_PTRTAB_SIZE
    s_gcRegPtrDscSize += sizeof(*change);
#endif

    return change;
}

const regMaskTP GCInfo::calleeSaveOrder[]{RBM_CALLEE_SAVED_ORDER};

regMaskSmall GCInfo::RegMaskFromCalleeSavedMask(uint16_t calleeSaveMask)
{
    regMaskSmall res = 0;
    for (int i = 0; i < CNT_CALLEE_SAVED; i++)
    {
        if ((calleeSaveMask & ((regMaskTP)1 << i)) != 0)
        {
            res |= calleeSaveOrder[i];
        }
    }
    return res;
}

GCInfo::CallSite* GCInfo::AddCallSite(unsigned codeOffs, regMaskTP refRegs, regMaskTP byrefRegs)
{
    CallSite* call = new (compiler, CMK_GC) CallSite;

    if (firstCallSite == nullptr)
    {
        assert(lastCallSite == nullptr);

        firstCallSite = call;
    }
    else
    {
        lastCallSite->next = call;
    }

    lastCallSite = call;

    call->refRegs   = static_cast<regMaskSmall>(refRegs);
    call->byrefRegs = static_cast<regMaskSmall>(byrefRegs);
    call->codeOffs  = codeOffs;

    return call;
}

#ifdef JIT32_GCENCODER

void* GCInfo::CreateAndStoreGCInfo(CodeGen* codeGen,
                                   unsigned codeSize,
                                   unsigned prologSize,
                                   unsigned epilogSize DEBUGARG(void* codePtr))
{
    BYTE    headerBuf[64];
    InfoHdr header;

    int s_cached;

#ifdef FEATURE_EH_FUNCLETS
    // We should do this before InfoBlockHdrSave since varPtrTableSize must be finalized before it
    if (compiler->ehAnyFunclets())
    {
        MarkFilterStackSlotsPinned();
    }
#endif

#ifdef DEBUG
    size_t headerSize =
#endif
        codeGen->compInfoBlkSize = InfoBlockHdrSave(headerBuf, 0, codeSize, prologSize, epilogSize,
                                                    codeGen->calleeSavedModifiedRegs, &header, &s_cached);

    size_t argTabOffset = 0;
    size_t ptrMapSize   = PtrTableSize(header, codeSize, &argTabOffset);

#if DISPLAY_SIZES

    if (codeGen->GetInterruptible())
    {
        gcHeaderISize += codeGen->compInfoBlkSize;
        gcPtrMapISize += ptrMapSize;
    }
    else
    {
        gcHeaderNSize += codeGen->compInfoBlkSize;
        gcPtrMapNSize += ptrMapSize;
    }

#endif // DISPLAY_SIZES

    codeGen->compInfoBlkSize += ptrMapSize;

    /* Allocate the info block for the method */

    BYTE* infoBlkAddr = (BYTE*)compiler->info.compCompHnd->allocGCInfo(codeGen->compInfoBlkSize);

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

    infoBlkAddr += InfoBlockHdrSave(infoBlkAddr, -1, codeSize, prologSize, epilogSize, codeGen->calleeSavedModifiedRegs,
                                    &header, &s_cached);

    assert(infoBlkAddr == (BYTE*)infoPtr + headerSize);
    infoBlkAddr = PtrTableSave(infoBlkAddr, header, codeSize, &argTabOffset);
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
        printf("GC info size = %3u\n", codeGen->compInfoBlkSize);

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

    noway_assert(infoBlkAddr == (BYTE*)infoPtr + codeGen->compInfoBlkSize);

    return infoPtr;
}

#else

void GCInfo::CreateAndStoreGCInfo(unsigned codeSize, unsigned prologSize DEBUGARG(void* codePtr))
{
    CompIAllocator encoderAlloc(compiler->getAllocator(CMK_GC));
    GcInfoEncoder  encoder(compiler->info.compCompHnd, compiler->info.compMethodInfo, &encoderAlloc, NOMEM);

    InfoBlockHdrSave(encoder, codeSize, prologSize);
    unsigned callSiteCount = 0;
    MakeRegPtrTable(encoder, codeSize, prologSize, MakeRegPtrMode::AssignSlots, &callSiteCount);
    encoder.FinalizeSlotIds();
    MakeRegPtrTable(encoder, codeSize, prologSize, MakeRegPtrMode::DoWork, &callSiteCount);

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

        encoder.SetSizeOfEditAndContinuePreservedArea(preservedAreaSize);
    }
#endif

    if (compiler->opts.IsReversePInvoke())
    {
        LclVarDsc* reversePInvokeFrameLcl = compiler->lvaGetDesc(compiler->lvaReversePInvokeFrameVar);
        encoder.SetReversePInvokeFrameSlot(reversePInvokeFrameLcl->GetStackOffset());
    }

    encoder.Build();
    encoder.Emit();
}

#endif
