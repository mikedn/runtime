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
        Pop,
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
            unsigned argOffset;
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
#ifdef JIT32_GCENCODER
        unsigned argCount;

        union {
            struct // if argCount == 0
            {
                unsigned argMask;
                unsigned byrefArgMask;
            };

            unsigned* argTable; // if argCount != 0
        };
#else
        uint8_t   callInstrLength;
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
    VARSET_TP          liveLcls               = VarSetOps::UninitVal();
    regMaskTP          liveRefRegs            = RBM_NONE;
    regMaskTP          liveByrefRegs          = RBM_NONE;
    bool               isFullyInterruptible   = false;
#ifdef JIT32_GCENCODER
    bool isFramePointerUsed = false;
#endif
#ifdef DEBUG
    ArrayStack<StackSlotLifetime*> deltaStackSlotLifetime;
    RegArgChange*                  deltaRegArgChangeBase = nullptr;
    regMaskTP                      deltaRefRegsBase      = RBM_NONE;
    regMaskTP                      deltaByrefRegsBase    = RBM_NONE;
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

#if MEASURE_PTRTAB_SIZE
    static size_t s_gcRegPtrDscSize;
    static size_t s_gcTotalPtrTabSize;
#endif

    GCInfo(Compiler* compiler);

    void Init();

    bool IsFullyInterruptible() const
    {
        return isFullyInterruptible;
    }

#ifdef JIT32_GCENCODER
    bool ReportCallSites() const
    {
        return !ReportRegArgChanges();
    }

    bool ReportRegArgChanges() const
    {
        return isFullyInterruptible || ReportNonGCArgChanges();
    }

    bool ReportNonGCArgChanges() const
    {
        return !isFramePointerUsed
#ifdef UNIX_X86_ABI
               // UNIX_X86_ABI uses GC info for unwinding so we need to report all arguments,
               // even if the GC itself needs only the GC arguments in fully interruptible code.
               || isFullyInterruptible
#endif
            ;
    }
#endif

    // TODO-MIKE-Cleanup: This should be const.
    VARSET_TP& GetLiveLcls()
    {
        return liveLcls;
    }

    regMaskTP GetAllLiveRegs() const
    {
        return liveRefRegs | liveByrefRegs;
    }

    // TODO-MIKE-Cleanup: This should be const.
    regMaskTP& GetLiveRegs(GCtype type)
    {
        assert(type != GCT_NONE);

        return type == GCT_GCREF ? liveRefRegs : liveByrefRegs;
    }

    GCtype GetRegType(regNumber reg) const
    {
        regMaskTP mask = genRegMask(reg);

        if ((liveRefRegs & mask) != RBM_NONE)
        {
            return GCT_GCREF;
        }

        if ((liveByrefRegs & mask) != RBM_NONE)
        {
            return GCT_BYREF;
        }

        return GCT_NONE;
    }

    StackSlotLifetime* BeginStackSlotLifetime(int slotOffs, unsigned codeOffs);
    void EndStackSlotLifetime(StackSlotLifetime* lifetime DEBUGARG(int slotOffs), unsigned codeOffs);

    RegArgChange* AddRegArgChange();
#ifdef JIT32_GCENCODER
    RegArgChange* AddLiveRegs(GCtype gcType, regMaskTP regs, unsigned codeOffs, bool isThis);
    void AddLiveReg(GCtype type, regNumber reg, unsigned codeOffs, bool isThis);
#else
    RegArgChange* AddLiveRegs(GCtype gcType, regMaskTP regs, unsigned codeOffs);
    void AddLiveReg(GCtype type, regNumber reg, unsigned codeOffs);
#endif
    RegArgChange* RemoveLiveRegs(GCtype gcType, regMaskTP regs, unsigned codeOffs);
    void RemoveLiveReg(regNumber reg, unsigned codeOffs);
    void RemoveAllLiveRegs(unsigned codeOffs);

#ifdef JIT32_GCENCODER
    RegArgChange* AddCallArgPush(unsigned codeOffs, unsigned stackLevel, GCtype gcType);
    RegArgChange* AddCallArgsKill(unsigned codeOffs, unsigned argCount);
    RegArgChange* AddCallArgsPop(unsigned codeOffs, unsigned argCount, bool isCall);
    CallSite* AddCallSite(unsigned codeOffs);
    void* CreateAndStoreGCInfo(class CodeGen* codeGen, unsigned codeSize, unsigned prologSize, unsigned epilogSize);
#else
    RegArgChange* AddCallArgStore(unsigned codeOffs, int argOffs, GCtype gcType);
    RegArgChange* AddCallArgsKill(unsigned codeOffs);
    CallSite* AddCallSite(unsigned codeOffs, unsigned length);
    void CreateAndStoreGCInfo(unsigned codeSize, unsigned prologSize);
#endif

#ifdef DEBUG
    void DumpStackSlotLifetimeDelta(const char* header);
    void DumpDelta(const char* header);
#endif

private:
#if !defined(JIT32_GCENCODER) || defined(FEATURE_EH_FUNCLETS)
    void MarkFilterStackSlotsPinned();
    void InsertSplitStackSlotLifetime(StackSlotLifetime* desc, StackSlotLifetime* begin);
    INDEBUG(void DumpStackSlotLifetime(const char* message, StackSlotLifetime* desc) const;)
#endif

#ifdef DEBUG
    void DumpRegDelta(const char* header, GCtype type, regMaskTP baseRegs, regMaskTP diffRegs);
    void DumpArgDelta(const char* header);
#endif
};

#ifdef JIT32_GCENCODER
void InitGCEncoderLookupTable();
#endif
