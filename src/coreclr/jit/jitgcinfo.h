// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "gcinfotypes.h"

class GCEncoder;

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
        Pop,
        Push,
        Kill
    };

    struct RegArgChange
    {
        RegArgChange* next = nullptr;
        unsigned      codeOffs;

        union {
            struct
            {
                regMaskSmall addRegs;
                regMaskSmall removeRegs;
            };

            uint16_t argOffset;
        };

        RegArgChangeKind kind : 2;
        GCtype           gcType : 2;
        unsigned         isArg : 1;
        unsigned         isCall : 1;
        unsigned         isThis : 1;
        unsigned         callRefRegs : CNT_CALLEE_SAVED;
        unsigned         callByrefRegs : CNT_CALLEE_SAVED;
#ifndef JIT32_GCENCODER
        unsigned callInstrLength : 4;
#endif

#ifndef JIT32_GCENCODER
        bool IsCallInstr() const
        {
            return isCall && (callInstrLength != 0);
        }
#endif
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

#ifdef JIT32_GCENCODER
    uint8_t* gcEpilogTable = nullptr;
    unsigned gcEpilogPrevOffset;
#endif

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

    // The following table determines the order in which callee-saved registers
    // are encoded in GC information at call sites..
    static const regMaskTP calleeSaveOrder[];

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

    RegArgChange* GetFirstRegArgChange() const
    {
        return firstRegArgChange;
    }

    RegArgChange* GetLastRegArgChange() const
    {
        return lastRegArgChange;
    }

    CallSite* AddCallSite(unsigned codeOffs, regMaskTP refRegs, regMaskTP byrefRegs);

#ifdef JIT32_GCENCODER
    static void InitEncoderLookupTable();

    void* CreateAndStoreGCInfo(class CodeGen* codeGen,
                               unsigned       codeSize,
                               unsigned       prologSize,
                               unsigned epilogSize DEBUGARG(void* codePtr));

private:
    void CountForHeader(unsigned* pUntrackedCount, unsigned* pVarPtrTableSize);
    bool IsUntrackedLocalOrNonEnregisteredArg(unsigned lclNum, bool* keepThisAlive = nullptr);
    size_t MakeRegPtrTable(uint8_t* dest, int mask, const InfoHdr& header, unsigned codeSize, size_t* pArgTabOffset);
    size_t PtrTableSize(const InfoHdr& header, unsigned codeSize, size_t* pArgTabOffset);
    BYTE* PtrTableSave(uint8_t* destPtr, const InfoHdr& header, unsigned codeSize, size_t* pArgTabOffset);
    size_t InfoBlockHdrSave(uint8_t*  dest,
                            int       mask,
                            unsigned  methodSize,
                            unsigned  prologSize,
                            unsigned  epilogSize,
                            regMaskTP savedRegs,
                            InfoHdr*  header,
                            int*      s_cached);

    static size_t RecordEpilog(void* pCallBackData, unsigned offset);

#if DUMP_GC_TABLES
    size_t InfoBlockHdrDump(const uint8_t* table, InfoHdr* header, unsigned* methodSize);
    size_t DumpPtrTable(const uint8_t* table, const InfoHdr& header, unsigned methodSize);
#endif

#else
    void CreateAndStoreGCInfo(unsigned codeSize, unsigned prologSize DEBUGARG(void* codePtr));

private:
    void AddTrackedStackSlots(GCEncoder& encoder);
    void InfoRecordGCRegStateChange(GCEncoder&    encoder,
                                    unsigned      codeOffset,
                                    GcSlotState   slotState,
                                    regMaskSmall  regs,
                                    regMaskSmall  byrefRegs,
                                    regMaskSmall* newRegs = nullptr);
    void InfoRecordGCStackArgLive(GCEncoder& encoder, RegArgChange* argChange);
    void InfoRecordGCStackArgsDead(GCEncoder&    encoder,
                                   unsigned      codeOffset,
                                   RegArgChange* firstArgChange,
                                   RegArgChange* lastArgChange);
    void AddUntrackedStackSlots(GCEncoder& encoder);
    void AddFullyInterruptibleSlots(GCEncoder& encoder);
    void AddFullyInterruptibleRanges(GCEncoder& encoder, unsigned codeSize, unsigned prologSize);
    void AddPartiallyInterruptibleSlots(GCEncoder& encoder);

    void InfoBlockHdrSave(GCEncoder& encoder, unsigned methodSize, unsigned prologSize);
#endif

#if !defined(JIT32_GCENCODER) || defined(FEATURE_EH_FUNCLETS)
    // This method expands the tracked stack variables lifetimes so that any lifetimes within filters
    // are reported as pinned.
    void MarkFilterStackSlotsPinned();

    // Insert a StackSlotLifetime that was generated by splitting lifetimes.
    void InsertSplitStackSlotLifetime(StackSlotLifetime* desc, StackSlotLifetime* begin);

    INDEBUG(void DumpStackSlotLifetime(const char* message, StackSlotLifetime* desc) const;)
#endif

    ReturnKind GetReturnKind() const;
};
