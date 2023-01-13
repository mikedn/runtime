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
        AddRegs,
        RemoveRegs,
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
        unsigned callRefRegs : CNT_CALLEE_SAVED;
        unsigned callByrefRegs : CNT_CALLEE_SAVED;
#endif

        union {
            regMaskSmall regs;
#ifdef JIT32_GCENCODER
            unsigned argOffset;
#else
            int argOffset;
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
        uint8_t codeLength;
#endif
    };

    Compiler* const    compiler;
    StackSlotLifetime* firstStackSlotLifetime          = nullptr;
    StackSlotLifetime* lastStackSlotLifetime           = nullptr;
    RegArgChange*      firstRegArgChange               = nullptr;
    RegArgChange*      lastRegArgChange                = nullptr;
    CallSite*          firstCallSite                   = nullptr;
    CallSite*          lastCallSite                    = nullptr;
    VARSET_TP          liveLcls                        = VarSetOps::UninitVal();
    regMaskTP          liveRefRegs                     = RBM_NONE;
    regMaskTP          liveByrefRegs                   = RBM_NONE;
    bool               isFullyInterruptible            = false;
    bool               stackSlotLifetimesMatchLiveLcls = true;
#ifndef JIT32_GCENCODER
    bool hasArgStores = false;
#else
    bool        isFramePointerUsed = false;
    bool        useArgsBitStack    = false;

    static constexpr unsigned ArgsBitStackMaxDepth = sizeof(unsigned) * CHAR_BIT;

    union {
        struct
        {
            unsigned gcMask;
            unsigned byrefMask;
        } argsBitStack;

        struct
        {
            unsigned reportCount;
            unsigned maxCount;
            uint8_t* types;
            uint8_t  inlineStorage[16];
        } argsStack;
    };

    regNumber syncThisReg             = REG_NA;
#ifndef FEATURE_EH_FUNCLETS
    int       syncThisStackSlotOffset = INT_MIN;
#endif
#endif // JIT32_GCENCODER
    int                 minTrackedStackSlotOffset = 0;
    int                 maxTrackedStackSlotOffset = 0;
    unsigned            trackedStackSlotCount     = 0;
    StackSlotLifetime** liveTrackedStackSlots     = nullptr;
#ifdef DEBUG
    ArrayStack<StackSlotLifetime*> deltaStackSlotLifetime;
    RegArgChange*                  deltaRegArgChangeBase = nullptr;
    CallSite*                      deltaCallSiteBase     = nullptr;
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
    static bool IsNoGCHelper(CorInfoHelpFunc helper);
    static regMaskTP GetNoGCHelperCalleeKilledRegs(CorInfoHelpFunc helper);
    static regMaskTP GetNoGCHelperCalleeSavedRegs(CorInfoHelpFunc helper);

#if MEASURE_PTRTAB_SIZE
    static size_t s_gcRegPtrDscSize;
    static size_t s_gcTotalPtrTabSize;
#endif

    GCInfo(Compiler* compiler);

    void SetTrackedStackSlotRange(int minOffset, int maxOffset)
    {
        assert(maxOffset > minOffset);
        assert(minOffset % TARGET_POINTER_SIZE == 0);
        assert(maxOffset % TARGET_POINTER_SIZE == 0);

        minTrackedStackSlotOffset = minOffset;
        maxTrackedStackSlotOffset = maxOffset;
        trackedStackSlotCount     = (maxOffset - minOffset) / TARGET_POINTER_SIZE;
    }

#ifdef JIT32_GCENCODER
    void Begin(unsigned maxStackDepth);
#else
    void      Begin();
#endif
    void End(unsigned codeOffs);

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

    bool HasTrackedStackSlots() const
    {
        return trackedStackSlotCount != 0;
    }

    unsigned GetTrackedStackSlotIndex(int offset) const
    {
        assert((minTrackedStackSlotOffset <= offset) && (offset < maxTrackedStackSlotOffset));
        assert(abs(offset) % TARGET_POINTER_SIZE == 0);

        return (offset - minTrackedStackSlotOffset) / TARGET_POINTER_SIZE;
    }

    bool IsLiveTrackedStackSlot(unsigned index) const
    {
        assert(index < trackedStackSlotCount);
        return liveTrackedStackSlots[index] != nullptr;
    }

    regMaskTP GetAllLiveRegs() const
    {
        return liveRefRegs | liveByrefRegs;
    }

    regMaskTP GetLiveRegs(GCtype type) const
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

#ifdef JIT32_GCENCODER
    regNumber GetSyncThisReg() const
    {
        return syncThisReg;
    }
#endif

    void BeginStackSlotLifetime(GCtype type, unsigned index, unsigned codeOffs, int slotOffs);
    void EndStackSlotLifetime(unsigned index, unsigned codeOffs DEBUGARG(int slotOffs));
    void SetLiveStackSlots(VARSET_TP newLiveLcls, unsigned codeOffs);
    void UpdateStackSlotLifetimes(unsigned codeOffs);

    void AddLiveRegs(GCtype type, regMaskTP regs, unsigned codeOffs);
    void RemoveLiveRegs(GCtype type, regMaskTP regs, unsigned codeOffs);

    void AddLiveReg(GCtype type, regNumber reg, unsigned codeOffs);
    void SetLiveRegs(GCtype type, regMaskTP regs, unsigned codeOffs);
    void RemoveLiveReg(regNumber reg, unsigned codeOffs);
    void RemoveAllLiveRegs(unsigned codeOffs);

#ifdef JIT32_GCENCODER
    void StackPush(GCtype type, unsigned stackLevel, unsigned codeOffs);
    void StackPushMultiple(unsigned count, unsigned stackLevel, unsigned codeOffs);
    void StackKill(unsigned count, unsigned stackLevel, unsigned codeOffs);
    void StackPop(unsigned count, unsigned stackLevel, unsigned codeOffs, bool isCall);

    void AddCallSite(unsigned stackLevel, unsigned codeOffs);

    void* CreateAndStoreGCInfo(class CodeGen* codeGen, unsigned codeSize, unsigned prologSize, unsigned epilogSize);
#else
    void AddCallArgStore(unsigned codeOffs, int argOffs, GCtype gcType);
    void AddCallArgsKill(unsigned codeOffs);
    void AddCallSite(unsigned callOffs, unsigned callEndOffs);
    void CreateAndStoreGCInfo(unsigned codeSize, unsigned prologSize);
#endif

#ifdef DEBUG
    void DumpStackSlotLifetimeDelta(const char* header);
    void DumpDelta(const char* header);
#endif

private:
    RegArgChange* AddRegArgChange();

#ifdef JIT32_GCENCODER
    void AddCallArgPush(GCtype type, unsigned stackLevel, unsigned codeOffs);
    void AddCallArgsKill(unsigned count, unsigned codeOffs);
    void AddCallArgsPop(unsigned count, unsigned codeOffs, bool isCall);
#endif

#if !defined(JIT32_GCENCODER) || defined(FEATURE_EH_FUNCLETS)
    void MarkFilterStackSlotsPinned();
    void InsertSplitStackSlotLifetime(StackSlotLifetime* desc, StackSlotLifetime* begin);
    INDEBUG(void DumpStackSlotLifetime(const char* message, StackSlotLifetime* desc) const;)
#endif

#ifdef DEBUG
    void DumpRegDelta(const char* header, GCtype type, regMaskTP baseRegs, regMaskTP diffRegs);
    void DumpRegArgChangeDelta(const char* header);
    void DumpCallSiteDelta(const char* header);
#endif
};

#ifdef JIT32_GCENCODER
void InitGCEncoderLookupTable();
#endif
