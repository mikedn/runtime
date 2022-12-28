// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#ifdef JIT32_GCENCODER
#include "gcinfotypes.h"
#endif

class GCInfo
{
    friend class CodeGenInterface;
    friend class CodeGen;

public:
    struct StackSlotLifetime
    {
        StackSlotLifetime* next = nullptr;

        unsigned slotOffset;
        unsigned beginCodeOffs;
        unsigned endCodeOffs;

        StackSlotLifetime(int slotOffset, unsigned beginCodeOffset)
            : slotOffset(slotOffset)
            , beginCodeOffs(beginCodeOffset)
#ifdef DEBUG
            , endCodeOffs(0)
#endif
        {
        }

        StackSlotLifetime(unsigned slotOffset, unsigned beginCodeOffset, unsigned endCodeOffset)
            : slotOffset(slotOffset), beginCodeOffs(beginCodeOffset), endCodeOffs(endCodeOffset)
        {
        }
    };

    enum class RegArgChangeKind : unsigned
    {
        RegChange,
#ifdef JIT32_GCENCODER
        PushArg,
        PopArgs,
#else
        StoreArg,
#endif
        KillArgs
    };

    struct RegArgChange
    {
        RegArgChange* next = nullptr;
        unsigned      codeOffs;

        RegArgChangeKind kind : 8;
        GCtype           gcType : 8;
#ifdef JIT32_GCENCODER
        unsigned isThis : 1;
        unsigned isCall : 1;
        unsigned callRefRegs : CNT_CALLEE_SAVED;
        unsigned callByrefRegs : CNT_CALLEE_SAVED;
#endif

        union {
            struct
            {
                regMaskSmall addRegs;
                regMaskSmall removeRegs;
            };

#ifdef JIT32_GCENCODER
            uint16_t argOffset;
#else
            int   argOffset;
#endif
        };
    };

    struct CallSite
    {
        CallSite*    next = nullptr;
        regMaskSmall refRegs;
        regMaskSmall byrefRegs;
        unsigned     codeOffs;
#ifndef JIT32_GCENCODER
        uint8_t callInstrLength;
#endif

#if !FEATURE_FIXED_OUT_ARGS
        uint16_t argCount;

        union {
            struct // if argCount == 0
            {
                unsigned argMask;
                unsigned byrefArgMask;
            };

            unsigned* argTable; // if argCount != 0
        };
#endif
    };

private:
    Compiler* const    compiler;
    StackSlotLifetime* firstStackSlotLifetime = nullptr;
    StackSlotLifetime* lastStackSlotLifetime  = nullptr;
    RegArgChange*      firstRegArgChange      = nullptr;
    RegArgChange*      lastRegArgChange       = nullptr;
    CallSite*          firstCallSite          = nullptr;
    CallSite*          lastCallSite           = nullptr;

public:
    enum WriteBarrierForm
    {
        WBF_NoBarrier,        // No barrier is required
        WBF_BarrierUnknown,   // A barrier is required, no information on checked/unchecked.
        WBF_BarrierChecked,   // A checked barrier is required.
        WBF_BarrierUnchecked, // An unchecked barrier is required.
    };

    static WriteBarrierForm GetWriteBarrierForm(GenTreeStoreInd* store);
    static WriteBarrierForm GetWriteBarrierFormFromAddress(GenTree* addr);

#ifdef JIT32_GCENCODER
    // The following table determines the order in which callee-saved registers
    // are encoded in GC information at call sites..
    static const regMaskTP calleeSaveOrder[];
#endif

    // This method takes a "compact" bitset of the callee-saved registers, and "expands" it to a full register mask.
    static regMaskSmall RegMaskFromCalleeSavedMask(uint16_t calleeSaveMask);

#if MEASURE_PTRTAB_SIZE
    static size_t s_gcRegPtrDscSize;
    static size_t s_gcTotalPtrTabSize;
#endif

    GCInfo(Compiler* compiler);

    StackSlotLifetime* BeginStackSlotLifetime(int slotOffs, unsigned codeOffs);
    void EndStackSlotLifetime(StackSlotLifetime* lifetime DEBUGARG(int slotOffs), unsigned codeOffs);

    RegArgChange* AddRegArgChange();
#ifdef JIT32_GCENCODER
    RegArgChange* AddLiveRegs(GCtype gcType, regMaskTP regs, unsigned codeOffs, bool isThis);
#else
    RegArgChange* AddLiveRegs(GCtype gcType, regMaskTP regs, unsigned codeOffs);
#endif
    RegArgChange* RemoveLiveRegs(GCtype gcType, regMaskTP regs, unsigned codeOffs);

#ifdef JIT32_GCENCODER
    RegArgChange* AddCallArgPush(unsigned codeOffs, unsigned stackLevel, GCtype gcType);
    RegArgChange* AddCallArgsKill(unsigned codeOffs, unsigned argCount);
    RegArgChange* AddCallArgsPop(
        unsigned codeOffs, unsigned argCount, bool isCall, unsigned refRegs, unsigned byrefRegs);
#else
    RegArgChange* AddCallArgStore(unsigned codeOffs, int argOffs, GCtype gcType);
    RegArgChange* AddCallArgsKill(unsigned codeOffs);
#endif

    RegArgChange* GetFirstRegArgChange() const
    {
        return firstRegArgChange;
    }

    RegArgChange* GetLastRegArgChange() const
    {
        return lastRegArgChange;
    }

#ifdef JIT32_GCENCODER
    CallSite* AddCallSite(unsigned codeOffs, regMaskTP refRegs, regMaskTP byrefRegs);
#else
    CallSite* AddCallSite(unsigned codeOffs, unsigned length, regMaskTP refRegs, regMaskTP byrefRegs);
#endif

#ifdef JIT32_GCENCODER
    void* CreateAndStoreGCInfo(class CodeGen* codeGen, unsigned codeSize, unsigned prologSize, unsigned epilogSize);
#else
    void CreateAndStoreGCInfo(unsigned codeSize, unsigned prologSize);
#endif

private:
#if !defined(JIT32_GCENCODER) || defined(FEATURE_EH_FUNCLETS)
    void MarkFilterStackSlotsPinned();
    void InsertSplitStackSlotLifetime(StackSlotLifetime* desc, StackSlotLifetime* begin);
    INDEBUG(void DumpStackSlotLifetime(const char* message, StackSlotLifetime* desc) const;)
#endif
};

#ifdef JIT32_GCENCODER
void InitGCEncoderLookupTable();
#endif
