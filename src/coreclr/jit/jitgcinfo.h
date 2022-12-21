// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "gcinfotypes.h"
#ifndef JIT32_GCENCODER
#include "gcinfoencoder.h"
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
#ifndef JIT32_GCENCODER
    class RegSlotIdKey
    {
        unsigned key;

    public:
        RegSlotIdKey()
        {
        }

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
        StackSlotIdKey()
        {
        }

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
#endif // !JIT32_GCENCODER

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
#else
    JitHashTable<RegSlotIdKey, RegSlotIdKey, GcSlotId>     regSlotMap;
    JitHashTable<StackSlotIdKey, StackSlotIdKey, GcSlotId> stackSlotMap;
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
    bool IsUntrackedLocalOrNonEnregisteredArg(unsigned varNum, bool* pThisKeptAliveIsInUntracked = nullptr);
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
    enum class MakeRegPtrMode
    {
        AssignSlots,
        DoWork
    };

    // This method has two modes.  In the "assign slots" mode, it figures out what stack locations are
    // used to contain GC references, and whether those locations contain byrefs or pinning references,
    // building up mappings from tuples of <offset X byref/pinning> to the corresponding slot id.
    // In the "do work" mode, we use these slot ids to actually declare live ranges to the encoder.
    void MakeVarPtrTable(GcInfoEncoder* gcInfoEncoder, MakeRegPtrMode mode);

    // At instruction offset "instrOffset," the set of registers indicated by "regMask" is becoming live or dead,
    // depending on whether "newState" is "GC_SLOT_DEAD" or "GC_SLOT_LIVE".  The subset of registers whose corresponding
    // bits are set in "byRefMask" contain by-refs rather than regular GC pointers. "*pPtrRegs" is the set of
    // registers currently known to contain pointers.  If "mode" is "ASSIGN_SLOTS", computes and records slot
    // ids for the registers.  If "mode" is "DO_WORK", informs "gcInfoEncoder" about the state transition,
    // using the previously assigned slot ids, and updates "*pPtrRegs" appropriately.
    void InfoRecordGCRegStateChange(GcInfoEncoder* gcInfoEncoder,
                                    MakeRegPtrMode mode,
                                    unsigned       instrOffset,
                                    regMaskSmall   regMask,
                                    GcSlotState    newState,
                                    regMaskSmall   byRefMask,
                                    regMaskSmall*  pPtrRegs);

    void InfoRecordGCStackArgLive(GcInfoEncoder* gcInfoEncoder, MakeRegPtrMode mode, RegArgChange* genStackPtr);

    // Walk all the pushes between genStackPtrFirst (inclusive) and genStackPtrLast (exclusive)
    // and mark them as going dead at instrOffset
    void InfoRecordGCStackArgsDead(GcInfoEncoder* gcInfoEncoder,
                                   unsigned       instrOffset,
                                   RegArgChange*  genStackPtrFirst,
                                   RegArgChange*  genStackPtrLast);

    // This method has two modes.  In the "assign slots" mode, it figures out what registers and stack
    // locations are used to contain GC references, and whether those locations contain byrefs or pinning
    // references, building up mappings from tuples of <reg/offset X byref/pinning> to the corresponding
    // slot id (in the two member fields declared above).  In the "do work" mode, we use these slot ids to
    // actually declare live ranges to the encoder.
    void MakeRegPtrTable(GcInfoEncoder* gcInfoEncoder,
                         unsigned       codeSize,
                         unsigned       prologSize,
                         MakeRegPtrMode mode,
                         unsigned*      callCntRef);

    void InfoBlockHdrSave(GcInfoEncoder* gcInfoEncoder, unsigned methodSize, unsigned prologSize);
#endif

#if !defined(JIT32_GCENCODER) || defined(FEATURE_EH_FUNCLETS)
    // This method expands the tracked stack variables lifetimes so that any lifetimes within filters
    // are reported as pinned.
    void MarkFilterStackSlotsPinned();

    // Insert a StackSlotLifetime that was generated by splitting lifetimes.
    void InsertSplitStackSlotLifetime(StackSlotLifetime* desc, StackSlotLifetime* begin);

    INDEBUG(void DumpStackSlotLifetime(StackSlotLifetime* desc) const;)
#endif

    ReturnKind GetReturnKind() const;
};
