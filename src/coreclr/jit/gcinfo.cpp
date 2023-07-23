// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "jitgcinfo.h"
#include "gcinfotypes.h"

#if MEASURE_PTRTAB_SIZE
size_t GCInfo::s_gcRegPtrDscSize;
size_t GCInfo::s_gcTotalPtrTabSize;
#endif

GCInfo::GCInfo(Compiler* compiler)
    : compiler(compiler)
#ifdef DEBUG
    , deltaStackSlotLifetime(compiler->getAllocator(CMK_DebugOnly))
#endif
{
}

#ifdef JIT32_GCENCODER
void GCInfo::Begin(unsigned maxStackDepth)
#else
void GCInfo::Begin()
#endif
{
    liveLcls = VarSetOps::MakeEmpty(compiler);

    isFullyInterruptible = compiler->codeGen->GetInterruptible();

    if (trackedStackSlotCount != 0)
    {
        liveTrackedStackSlots = compiler->getAllocator(CMK_GC).allocate<StackSlotLifetime*>(trackedStackSlotCount);
        memset(liveTrackedStackSlots, 0, trackedStackSlotCount * sizeof(liveTrackedStackSlots[0]));
    }

#ifdef JIT32_GCENCODER
    isFramePointerUsed = compiler->codeGen->isFramePointerUsed();

    if (compiler->lvaKeepAliveAndReportThis())
    {
        assert(compiler->lvaIsOriginalThisParam(0));

        LclVarDsc* thisLcl = compiler->lvaGetDesc(0u);

        if (thisLcl->lvRegister)
        {
            assert(!thisLcl->HasGCSlotLiveness());

            syncThisReg = thisLcl->GetRegNum();
        }
#ifndef FEATURE_EH_FUNCLETS
        else if (thisLcl->HasGCSlotLiveness())
        {
            assert(trackedStackSlotCount != 0);

            syncThisStackSlotOffset = thisLcl->GetStackOffset();
        }
#endif
    }

    if (ReportCallSites() && (maxStackDepth <= ArgsBitStackMaxDepth))
    {
        useArgsBitStack = true;

        argsBitStack.gcMask    = 0;
        argsBitStack.byrefMask = 0;
    }
    else
    {
        argsStack.reportCount = 0;
        argsStack.maxCount    = maxStackDepth;

        if (maxStackDepth <= _countof(argsStack.inlineStorage))
        {
            argsStack.types = argsStack.inlineStorage;
        }
        else
        {
            argsStack.types = compiler->getAllocator(CMK_GC).allocate<uint8_t>(maxStackDepth);
        }
    }
#endif // JIT32_GCENCODER
}

void GCInfo::End(unsigned codeOffs)
{
#if defined(JIT32_GCENCODER) && !defined(FEATURE_EH_FUNCLETS)
    // We can let the "this" slot go dead now.
    int savedSyncThisStackSlotOffset = syncThisStackSlotOffset;
    syncThisStackSlotOffset          = INT_MIN;
#endif

    for (unsigned i = 0; i < trackedStackSlotCount; i++)
    {
        if (liveTrackedStackSlots[i] != nullptr)
        {
            EndStackSlotLifetime(i, codeOffs DEBUGARG(minTrackedStackSlotOffset + i * TARGET_POINTER_SIZE));
        }
    }

#if defined(JIT32_GCENCODER) && !defined(FEATURE_EH_FUNCLETS)
    // Restore just in case we need it later.
    syncThisStackSlotOffset = savedSyncThisStackSlotOffset;
#endif

    if (liveByrefRegs != RBM_NONE)
    {
        SetLiveRegs(GCT_BYREF, RBM_NONE, codeOffs);
    }

    if (liveRefRegs != RBM_NONE)
    {
        SetLiveRegs(GCT_GCREF, RBM_NONE, codeOffs);
    }
}

void GCInfo::BeginStackSlotLifetime(GCtype type, unsigned index, unsigned codeOffs, int slotOffs)
{
    assert(type != GCT_NONE);
    assert(index < trackedStackSlotCount);
    assert(liveTrackedStackSlots[index] == nullptr);
    assert(abs(slotOffs) % TARGET_POINTER_SIZE == 0);
    assert((minTrackedStackSlotOffset <= slotOffs) && (slotOffs < maxTrackedStackSlotOffset));

    if (type == GCT_BYREF)
    {
        slotOffs |= byref_OFFSET_FLAG;
    }

#if defined(JIT32_GCENCODER) && !defined(FEATURE_EH_FUNCLETS)
    if (slotOffs == syncThisStackSlotOffset)
    {
        slotOffs |= this_OFFSET_FLAG;
    }
#endif

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

    liveTrackedStackSlots[index]    = lifetime;
    lastStackSlotLifetime           = lifetime;
    stackSlotLifetimesMatchLiveLcls = false;

    INDEBUG(deltaStackSlotLifetime.Push(lifetime));
}

void GCInfo::EndStackSlotLifetime(unsigned index, unsigned codeOffs DEBUGARG(int slotOffs))
{
    assert(index < trackedStackSlotCount);
#if defined(JIT32_GCENCODER) && !defined(FEATURE_EH_FUNCLETS)
    assert(slotOffs != syncThisStackSlotOffset);
#endif

    StackSlotLifetime* lifetime = liveTrackedStackSlots[index];

    assert(lifetime->endCodeOffs == 0);
    assert(codeOffs >= lifetime->beginCodeOffs);
    assert(static_cast<int>(lifetime->slotOffset & ~OFFSET_MASK) == slotOffs);

    lifetime->endCodeOffs           = codeOffs;
    liveTrackedStackSlots[index]    = nullptr;
    stackSlotLifetimesMatchLiveLcls = false;

    INDEBUG(deltaStackSlotLifetime.Push(lifetime));
}

void GCInfo::SetLiveLclStackSlots(VARSET_TP newLiveLcls, unsigned codeOffs)
{
    if (trackedStackSlotCount == 0)
    {
        return;
    }

    if (stackSlotLifetimesMatchLiveLcls && VarSetOps::Equal(compiler, liveLcls, newLiveLcls))
    {
        return;
    }

    for (unsigned trackedLclIndex = 0, count = compiler->lvaTrackedCount; trackedLclIndex < count; trackedLclIndex++)
    {
        LclVarDsc* lcl = compiler->lvaGetDescByTrackedIndex(trackedLclIndex);

        if (!lcl->HasGCSlotLiveness())
        {
            continue;
        }

        assert(varTypeIsGC(lcl->GetType()));

        int      offs    = lcl->GetStackOffset();
        unsigned index   = GetTrackedStackSlotIndex(offs);
        bool     isLive  = VarSetOps::IsMember(compiler, newLiveLcls, trackedLclIndex);
        bool     wasLive = IsLiveTrackedStackSlot(index);

        if (!wasLive && isLive)
        {
            BeginStackSlotLifetime(lcl->TypeIs(TYP_BYREF) ? GCT_BYREF : GCT_GCREF, index, codeOffs, offs);
        }
        else if (wasLive && !isLive)
        {
            EndStackSlotLifetime(index, codeOffs DEBUGARG(offs));
        }
    }

    liveLcls                        = newLiveLcls;
    stackSlotLifetimesMatchLiveLcls = true;
}

void GCInfo::KillTrackedSpillTemps(unsigned codeOffs)
{
    assert(compiler->codeGen->spillTemps.TrackGCSpillTemps());

    for (const SpillTemp& temp : compiler->codeGen->spillTemps)
    {
        if (!varTypeIsGC(temp.GetType()))
        {
            continue;
        }

        unsigned index = GetTrackedStackSlotIndex(temp.GetOffset());

        if (IsLiveTrackedStackSlot(index))
        {
            EndStackSlotLifetime(index, codeOffs DEBUGARG(temp.GetOffset()));
        }
    }
}

GCInfo::RegArgChange* GCInfo::AddRegArgChange()
{
#ifdef JIT32_GCENCODER
    assert(isFullyInterruptible || !isFramePointerUsed);
#else
    assert(isFullyInterruptible);
#endif

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

void GCInfo::AddLiveRegs(GCtype type, regMaskTP regs, unsigned codeOffs)
{
    assert(type != GCT_NONE);
    assert((GetAllLiveRegs() & regs) == RBM_NONE);
    assert(isFullyInterruptible);

    RegArgChange* change = AddRegArgChange();
    change->codeOffs     = codeOffs;
    change->kind         = RegArgChangeKind::AddRegs;
    change->gcType       = type;
    change->regs         = static_cast<RegSet>(regs);
}

void GCInfo::RemoveLiveRegs(GCtype type, regMaskTP regs, unsigned codeOffs)
{
    assert(type != GCT_NONE);
    assert((GetAllLiveRegs() & regs) != RBM_NONE);
    assert(isFullyInterruptible);

    RegArgChange* change = AddRegArgChange();
    change->codeOffs     = codeOffs;
    change->kind         = RegArgChangeKind::RemoveRegs;
    change->gcType       = type;
    change->regs         = static_cast<RegSet>(regs);
}

void GCInfo::AddLiveReg(GCtype type, regNumber reg, unsigned codeOffs)
{
    assert((liveRefRegs & liveByrefRegs) == RBM_NONE);
    assert(type != GCT_NONE);

    regMaskTP& typeRegs  = (type == GCT_GCREF) ? liveRefRegs : liveByrefRegs;
    regMaskTP& otherRegs = (type == GCT_GCREF) ? liveByrefRegs : liveRefRegs;
    regMaskTP  regMask   = genRegMask(reg);

    if ((typeRegs & regMask) == RBM_NONE)
    {
        if ((otherRegs & regMask) != RBM_NONE)
        {
            if (isFullyInterruptible)
            {
                RemoveLiveRegs(type == GCT_GCREF ? GCT_BYREF : GCT_GCREF, regMask, codeOffs);
            }

            otherRegs &= ~regMask;
        }

        if (isFullyInterruptible)
        {
            AddLiveRegs(type, regMask, codeOffs);
        }

        typeRegs |= regMask;
    }
}

void GCInfo::SetLiveRegs(GCtype type, regMaskTP regs, unsigned codeOffs)
{
    assert((liveRefRegs & liveByrefRegs) == RBM_NONE);
    assert(type != GCT_NONE);

    regMaskTP& typeRegs  = (type == GCT_GCREF) ? liveRefRegs : liveByrefRegs;
    regMaskTP& otherRegs = (type == GCT_GCREF) ? liveByrefRegs : liveRefRegs;

    assert(typeRegs != regs);

    if (isFullyInterruptible)
    {
        regMaskTP dead = typeRegs & ~regs;
        regMaskTP life = ~typeRegs & regs;

        assert((dead | life) != 0);
        assert((dead & life) == 0);

        // TODO-MIKE-Cleanup: This is messed up. RegArgChange always supported
        // a register set but this code always generates one RegArgChange per
        // register. It would be pretty easy to fix this but that results in
        // GC info diffs because the order changes. For now just handle what
        // appears to be the most common case - killing a bunch of regs after
        // calls - and avoid the cases that do generate GC info diffs.
        // The GC info dumps used for diffing are actually dumb because they
        // are sensitive to ordering, and ordering doesn't matter here.
        if ((dead != RBM_NONE) && (life == RBM_NONE))
        {
            RemoveLiveRegs(type, dead, codeOffs);
            typeRegs &= ~dead;
            dead = RBM_NONE;
        }

        regMaskTP change = (dead | life);

        while (change != RBM_NONE)
        {
            regMaskTP regMask = genFindLowestReg(change);
            regNumber reg     = genRegNumFromMask(regMask);

            if ((life & regMask) != RBM_NONE)
            {
                if ((otherRegs & regMask) != RBM_NONE)
                {
                    RemoveLiveRegs(type == GCT_GCREF ? GCT_BYREF : GCT_GCREF, regMask, codeOffs);
                    otherRegs &= ~regMask;
                }

                AddLiveRegs(type, regMask, codeOffs);
                typeRegs |= regMask;
            }
            else
            {
                RemoveLiveRegs(type, regMask, codeOffs);
                typeRegs &= ~regMask;
            }

            change &= ~regMask;
        }

        assert(typeRegs == regs);
    }
    else
    {
        typeRegs = regs;
        otherRegs &= ~regs;
    }

    assert((liveRefRegs & liveByrefRegs) == RBM_NONE);
}

void GCInfo::RemoveLiveReg(regNumber reg, unsigned codeOffs)
{
    assert((liveRefRegs & liveByrefRegs) == RBM_NONE);

    regMaskTP regMask = genRegMask(reg);

    if (isFullyInterruptible)
    {
        if ((liveRefRegs & regMask) != RBM_NONE)
        {
            RemoveLiveRegs(GCT_GCREF, regMask, codeOffs);
            liveRefRegs &= ~regMask;
        }
        else if ((liveByrefRegs & regMask) != RBM_NONE)
        {
            RemoveLiveRegs(GCT_BYREF, regMask, codeOffs);
            liveByrefRegs &= ~regMask;
        }
    }
    else
    {
        liveRefRegs &= ~regMask;
        liveByrefRegs &= ~regMask;
    }
}

void GCInfo::RemoveAllLiveRegs(unsigned codeOffs)
{
    assert((liveRefRegs & liveByrefRegs) == RBM_NONE);

    if (isFullyInterruptible)
    {
        if (liveRefRegs != RBM_NONE)
        {
            RemoveLiveRegs(GCT_GCREF, liveRefRegs, codeOffs);
        }

        if (liveByrefRegs != RBM_NONE)
        {
            RemoveLiveRegs(GCT_BYREF, liveByrefRegs, codeOffs);
        }
    }

    liveRefRegs   = RBM_NONE;
    liveByrefRegs = RBM_NONE;
}

#ifdef JIT32_GCENCODER

void GCInfo::StackPush(GCtype type, unsigned stackLevel, unsigned codeOffs)
{
    if (useArgsBitStack)
    {
        argsBitStack.gcMask    = (argsBitStack.gcMask << 1) | (type != GCT_NONE);
        argsBitStack.byrefMask = (argsBitStack.byrefMask << 1) | (type == GCT_BYREF);

        return;
    }

    noway_assert(stackLevel < argsStack.maxCount);

    argsStack.types[stackLevel] = static_cast<uint8_t>(type);

    if ((type != GCT_NONE) || ReportNonGCArgChanges())
    {
        argsStack.reportCount++;

        if (ReportRegArgChanges())
        {
            AddCallArgPush(type, stackLevel, codeOffs);
        }
    }
}

void GCInfo::StackPushMultiple(unsigned count, unsigned stackLevel, unsigned codeOffs)
{
    assert(count != 0);

    if (useArgsBitStack)
    {
        argsBitStack.gcMask <<= count;
        argsBitStack.byrefMask <<= count;

        return;
    }

    noway_assert((count <= argsStack.maxCount) && (stackLevel <= argsStack.maxCount - count));

    for (unsigned i = 0; i < count; i++)
    {
        argsStack.types[stackLevel + i] = GCT_NONE;
    }

    if (ReportNonGCArgChanges())
    {
        argsStack.reportCount += count;

        if (ReportRegArgChanges())
        {
            for (unsigned i = 0; i < count; i++)
            {
                AddCallArgPush(GCT_NONE, stackLevel + i, codeOffs);
            }
        }
    }
}

void GCInfo::StackKill(unsigned count, unsigned stackLevel, unsigned codeOffs)
{
    assert((0 < count) && (count <= stackLevel));

    if (useArgsBitStack)
    {
        argsBitStack.gcMask    = (count >= ArgsBitStackMaxDepth) ? 0 : (argsBitStack.gcMask & ~((1u << count) - 1u));
        argsBitStack.byrefMask = (count >= ArgsBitStackMaxDepth) ? 0 : (argsBitStack.byrefMask & ~((1u << count) - 1u));

        return;
    }

    unsigned gcCount = 0;

    for (unsigned i = 0; i < count; i++)
    {
        GCtype type = static_cast<GCtype>(argsStack.types[stackLevel - i - 1]);

        if (type != GCT_NONE)
        {
            argsStack.types[stackLevel - i - 1] = GCT_NONE;
            gcCount++;
        }
    }

    assert(gcCount <= argsStack.reportCount);

    if (!ReportNonGCArgChanges())
    {
        argsStack.reportCount -= gcCount;
    }

    if (ReportRegArgChanges())
    {
        if (gcCount != 0)
        {
            AddCallArgsKill(gcCount, codeOffs);
        }

        AddCallArgsPop(0, codeOffs, true);
    }
}

void GCInfo::StackPop(unsigned count, unsigned stackLevel, unsigned codeOffs, bool isCall)
{
    assert((count != 0) || isCall);
    assert(count <= stackLevel);

    if (useArgsBitStack)
    {
        argsBitStack.gcMask    = (count >= ArgsBitStackMaxDepth) ? 0 : (argsBitStack.gcMask >> count);
        argsBitStack.byrefMask = (count >= ArgsBitStackMaxDepth) ? 0 : (argsBitStack.byrefMask >> count);

        return;
    }

    if ((count == 0) && !ReportRegArgChanges())
    {
        return;
    }

    assert(stackLevel <= argsStack.maxCount);

    unsigned reportCount = 0;

    if (ReportNonGCArgChanges())
    {
        reportCount = count;
    }
    else
    {
        for (unsigned i = 0; i < count; i++)
        {
            if (static_cast<GCtype>(argsStack.types[stackLevel - i - 1]) != GCT_NONE)
            {
                reportCount++;
            }
        }
    }

    assert(reportCount <= argsStack.reportCount);
    argsStack.reportCount -= reportCount;

    if (ReportRegArgChanges())
    {
        AddCallArgsPop(reportCount, codeOffs, isCall);
    }
}

void GCInfo::AddCallArgPush(GCtype type, unsigned stackLevel, unsigned codeOffs)
{
    RegArgChange* change = AddRegArgChange();
    change->codeOffs     = codeOffs;
    change->argOffset    = stackLevel;
    change->kind         = RegArgChangeKind::PushArg;
    change->gcType       = type;
}

void GCInfo::AddCallArgsKill(unsigned count, unsigned codeOffs)
{
    RegArgChange* change = AddRegArgChange();
    change->codeOffs     = codeOffs;
    change->argOffset    = count;
    change->kind         = RegArgChangeKind::KillArgs;
    change->gcType       = GCT_GCREF;
}

void GCInfo::AddCallArgsPop(unsigned count, unsigned codeOffs, bool isCall)
{
    if (count == 0)
    {
        // We don't need to pop anything but may need to report GC registers, if any.

        if (isFullyInterruptible)
        {
            return;
        }

        if (((GetAllLiveRegs() & RBM_CALLEE_SAVED) == RBM_NONE) && (argsStack.reportCount == 0))
        {
            return;
        }
    }

    // Only calls may pop more than one value.
    // cdecl calls accomplish this popping via a post-call "ADD SP, imm" instruction,
    // we treat that as "isCall" too.
    isCall |= count > 1;

    // We only care about callee-saved registers and there are only 4 of them on x86,
    // we can save space in RegArgChange by "compressing" regMaskTP to just 4 bits.

    unsigned callRefRegs   = 0;
    unsigned callByrefRegs = 0;

    if (isCall)
    {
        static const regMaskTP calleeSaveOrder[]{RBM_CALLEE_SAVED_ORDER};

        for (unsigned i = 0; i < _countof(calleeSaveOrder); i++)
        {
            regMaskTP reg = calleeSaveOrder[i];

            if ((liveRefRegs & reg) != RBM_NONE)
            {
                callRefRegs |= (1 << i);
            }

            if ((liveByrefRegs & reg) != RBM_NONE)
            {
                callByrefRegs |= (1 << i);
            }
        }
    }

    RegArgChange* change  = AddRegArgChange();
    change->codeOffs      = codeOffs;
    change->argOffset     = count;
    change->kind          = isCall ? RegArgChangeKind::PopArgs : RegArgChangeKind::Pop;
    change->gcType        = GCT_GCREF;
    change->callRefRegs   = callRefRegs;
    change->callByrefRegs = callByrefRegs;
}

void GCInfo::AddCallSite(unsigned stackLevel, unsigned codeOffs)
{
    assert(ReportCallSites());

    regMaskTP regs = GetAllLiveRegs() & ~RBM_INTRET;

    if ((regs == RBM_NONE) &&
        ((stackLevel == 0) || (useArgsBitStack ? (argsBitStack.gcMask == 0) : (argsStack.reportCount == 0))))
    {
        return;
    }

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

    call->refRegs   = static_cast<RegSet>(liveRefRegs);
    call->byrefRegs = static_cast<RegSet>(liveByrefRegs);
    call->codeOffs  = codeOffs;

#if !FEATURE_FIXED_OUT_ARGS
    if (useArgsBitStack)
    {
        call->argCount     = 0;
        call->argMask      = argsBitStack.gcMask;
        call->byrefArgMask = argsBitStack.byrefMask;

        return;
    }

    if (argsStack.reportCount == 0)
    {
        call->argCount     = 0;
        call->argMask      = RBM_NONE;
        call->byrefArgMask = RBM_NONE;

        return;
    }

    call->argCount = argsStack.reportCount;
    call->argTable = new (compiler, CMK_GC) unsigned[argsStack.reportCount];

    unsigned gcArgCount = 0;

    for (unsigned i = 0; i < stackLevel; i++)
    {
        GCtype type = static_cast<GCtype>(argsStack.types[stackLevel - i - 1]);

        if (type != GCT_NONE)
        {
            unsigned offset = i * TARGET_POINTER_SIZE;

            if (type == GCT_BYREF)
            {
                offset |= byref_OFFSET_FLAG;
            }

            call->argTable[gcArgCount++] = offset;
        }
    }

    assert(argsStack.reportCount == gcArgCount);
#endif // !FEATURE_FIXED_OUT_ARGS
}

#else // !JIT32_GCENCODER

void GCInfo::AddCallArgStore(unsigned codeOffs, int argOffs, GCtype gcType)
{
    assert(gcType != GCT_NONE);
    assert(abs(argOffs) % REGSIZE_BYTES == 0);

    RegArgChange* change = AddRegArgChange();
    change->codeOffs     = codeOffs;
    change->argOffset    = argOffs;
    change->kind         = RegArgChangeKind::StoreArg;
    change->gcType       = gcType;

    hasArgStores = true;
}

void GCInfo::AddCallArgsKill(unsigned codeOffs)
{
    if (!hasArgStores)
    {
        return;
    }

    hasArgStores = false;

    RegArgChange* change = AddRegArgChange();
    change->codeOffs     = codeOffs;
    change->kind         = RegArgChangeKind::KillArgs;
}

void GCInfo::AddCallSite(unsigned callOffs, unsigned callEndOffs)
{
    assert(!isFullyInterruptible);

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

    call->refRegs     = static_cast<RegSet>(liveRefRegs);
    call->byrefRegs   = static_cast<RegSet>(liveByrefRegs);
    call->codeOffs    = callOffs;
    call->codeEndOffs = callEndOffs;
}

#endif // !JIT32_GCENCODER

#if !defined(JIT32_GCENCODER) || defined(FEATURE_EH_FUNCLETS)

// Walk all lifetimes and make it so that anything live in a filter is marked
// as pinned (often by splitting the lifetime so that *only* the filter region
// is pinned). This should only be called once (after generating all lifetimes,
// but before slot ids are finalized.
//
// DevDiv 376329 - The VM has to double report filters and their parent frame
// because they occur during the 1st pass and the parent frame doesn't go dead
// until we start unwinding in the 2nd pass.
//
// Untracked locals will only be reported in non-filter funclets and the
// parent.
// Registers can't be double reported by 2 frames since they're different.
// That just leaves stack variables which might be double reported.
//
// Technically double reporting is only a problem when the GC has to relocate a
// reference. So we avoid that problem by marking all live tracked stack
// variables as pinned inside the filter.  Thus if they are double reported, it
// won't be a problem since they won't be double relocated.
//
void GCInfo::MarkFilterStackSlotsPinned()
{
    assert(compiler->ehAnyFunclets());

    CompAllocator alloc = compiler->getAllocator(CMK_GC);

    for (EHblkDsc* const ehClause : EHClauses(compiler))
    {
        if (!ehClause->HasFilter())
        {
            continue;
        }

        const unsigned filterBegin = compiler->ehCodeOffset(ehClause->ebdFilter);
        const unsigned filterEnd   = compiler->ehCodeOffset(ehClause->ebdHndBeg);

        for (StackSlotLifetime* lifetime = firstStackSlotLifetime; lifetime != nullptr; lifetime = lifetime->next)
        {
            if ((lifetime->slotOffset & pinned_OFFSET_FLAG) != 0)
            {
                continue;
            }

            const unsigned slotBegin = lifetime->beginCodeOffs;
            const unsigned slotEnd   = lifetime->endCodeOffs;

            if ((slotEnd == slotBegin) || (slotEnd <= filterBegin) || (filterEnd <= slotBegin))
            {
                continue;
            }

            if ((filterBegin <= slotBegin) && (slotEnd <= filterEnd))
            {
                // The lifetime is completely within the filter, so just add the pinned flag.

                DBEXEC(compiler->verbose, DumpStackSlotLifetime("Pinning lifetime for filter.\nOld: ", lifetime));
                lifetime->slotOffset |= pinned_OFFSET_FLAG;
                DBEXEC(compiler->verbose, DumpStackSlotLifetime("New: ", lifetime));

                continue;
            }

            DBEXEC(compiler->verbose, DumpStackSlotLifetime("Splitting lifetime for filter.\nOld: ", lifetime));

            const int slotOffset = lifetime->slotOffset;

            if (slotEnd <= filterEnd)
            {
                // The lifetime started before the filter and ends somewhere inside it, so
                // we only create a new lifetime, and then adjust the original lifetime to
                // end before the filter.

                StackSlotLifetime* filterLifetime = new (alloc) StackSlotLifetime(slotOffset, filterBegin, slotEnd);

                lifetime->endCodeOffs = filterBegin;
                filterLifetime->slotOffset |= pinned_OFFSET_FLAG;

                InsertSplitStackSlotLifetime(filterLifetime, lifetime);

                DBEXEC(compiler->verbose, DumpStackSlotLifetime("New (1 of 2): ", lifetime));
                DBEXEC(compiler->verbose, DumpStackSlotLifetime("New (2 of 2): ", filterLifetime));

                continue;
            }

            if (slotBegin >= filterBegin)
            {
                // The lifetime starts inside the filter and ends somewhere after it, pin
                // it and create a new lifetime for the range that's outside the filter.

                StackSlotLifetime* postFilterLifetime = new (alloc) StackSlotLifetime(slotOffset, filterEnd, slotEnd);

                lifetime->slotOffset |= pinned_OFFSET_FLAG;
                lifetime->endCodeOffs = filterEnd;

                InsertSplitStackSlotLifetime(postFilterLifetime, lifetime);

                DBEXEC(compiler->verbose, DumpStackSlotLifetime("New (1 of 2): ", lifetime));
                DBEXEC(compiler->verbose, DumpStackSlotLifetime("New (2 of 2): ", postFilterLifetime));

                continue;
            }

            assert((slotBegin < filterBegin) && (slotEnd > filterEnd));

            // The lifetime starts before AND ends after the filter, so we need
            // to create 2 new lifetimes:
            //     (1) a pinned one for the filter
            //     (2) a regular one for after the filter
            // and then adjust the original lifetime to end before the filter.

            StackSlotLifetime* filterLifetime     = new (alloc) StackSlotLifetime(slotOffset, filterBegin, filterEnd);
            StackSlotLifetime* postFilterLifetime = new (alloc) StackSlotLifetime(slotOffset, filterEnd, slotEnd);

            filterLifetime->slotOffset |= pinned_OFFSET_FLAG;
            lifetime->endCodeOffs = filterBegin;

            InsertSplitStackSlotLifetime(filterLifetime, lifetime);
            InsertSplitStackSlotLifetime(postFilterLifetime, lifetime);

            DBEXEC(compiler->verbose, DumpStackSlotLifetime("New (1 of 3): ", lifetime));
            DBEXEC(compiler->verbose, DumpStackSlotLifetime("New (2 of 3): ", filterLifetime));
            DBEXEC(compiler->verbose, DumpStackSlotLifetime("New (3 of 3): ", postFilterLifetime));
        }
    }
}

void GCInfo::InsertSplitStackSlotLifetime(StackSlotLifetime* newLifetime, StackSlotLifetime* after)
{
    assert(newLifetime->beginCodeOffs >= after->beginCodeOffs);

#ifdef JIT32_GCENCODER
    // JIT32_GCENCODER requires the stack slot lifetime list to be sorted by beginCodeOffs.
    // Even if the newLifetime beginCodeOffs is greater than after's beginCodeOffs, there
    // could be other lifetimes between after and newLifetime that we need skip.

    StackSlotLifetime* next = after->next;

    while ((next != nullptr) && (next->beginCodeOffs < newLifetime->beginCodeOffs))
    {
        after = next;
        next  = next->next;
    }
#endif // JIT32_GCENCODER

    newLifetime->next = after->next;
    after->next       = newLifetime;
}

#ifdef DEBUG

const char* GetGCTypeName(GCtype gcType);

void GCInfo::DumpStackSlotLifetime(const char* message, StackSlotLifetime* lifetime) const
{
    printf("%s", message);

    const int    offs   = (lifetime->slotOffset & ~OFFSET_MASK);
    const GCtype gcType = (lifetime->slotOffset & byref_OFFSET_FLAG) ? GCT_BYREF : GCT_GCREF;
    const bool   isPin  = (lifetime->slotOffset & pinned_OFFSET_FLAG) != 0;

    printf("[%08X] %s%s var at [%s", dspPtr(lifetime), GetGCTypeName(gcType), isPin ? "pinned-ptr" : "",
           compiler->codeGen->isFramePointerUsed() ? STR_FPBASE : STR_SPBASE);

    if (offs < 0)
    {
        printf("-%02XH", -offs);
    }
    else if (offs > 0)
    {
        printf("+%02XH", +offs);
    }

    printf("] live from %04X to %04X\n", lifetime->beginCodeOffs, lifetime->endCodeOffs);
}

#endif // DEBUG

#endif // !defined(JIT32_GCENCODER) || defined(FEATURE_EH_FUNCLETS)

#ifdef DEBUG

const char* GetGCTypeName(GCtype gcType)
{
    switch (gcType)
    {
        case GCT_NONE:
            return "non-gc";
        case GCT_GCREF:
            return "ref";
        case GCT_BYREF:
            return "byref";
        default:
            return "???";
    }
}

void GCInfo::DumpRegDelta(const char* header, GCtype type, regMaskTP baseRegs, regMaskTP diffRegs)
{
    if (baseRegs != diffRegs)
    {
        printf("%s%s-regs ", header, GetGCTypeName(type));
        emitter::emitDispRegSetDiff("", baseRegs, diffRegs);
    }
}

void GCInfo::DumpRegArgChangeDelta(const char* header)
{
    if (deltaRegArgChangeBase == lastRegArgChange)
    {
        return;
    }

    RegArgChange* base = (deltaRegArgChangeBase == nullptr) ? firstRegArgChange : deltaRegArgChangeBase->next;
#if FEATURE_FIXED_OUT_ARGS
    const char* spRegName = getRegName(REG_SPBASE);
#endif

    for (RegArgChange* change = base; change != nullptr; change = change->next)
    {
        printf("%s", header);

        switch (change->kind)
        {
            case RegArgChangeKind::AddRegs:
                printf("def-%s-regs ", GetGCTypeName(change->gcType));
                dspRegMask(change->regs);
                break;
            case RegArgChangeKind::RemoveRegs:
                printf("kill-%s-regs ", GetGCTypeName(change->gcType));
                dspRegMask(change->regs);
                break;
#if FEATURE_FIXED_OUT_ARGS
            case RegArgChangeKind::StoreArg:
#ifdef TARGET_ARMARCH
                printf("def-%s-arg [%s,#%d]", GetGCTypeName(change->gcType), spRegName, change->argOffset);
#else
                printf("def-%s-arg [%s%c%02XH]", GetGCTypeName(change->gcType), spRegName,
                       change->argOffset < 0 ? '-' : '+', abs(change->argOffset));
#endif
                break;
            case RegArgChangeKind::KillArgs:
                printf("kill-args");
                break;
#else
            case RegArgChangeKind::PushArg:
                printf("push-%s %u", GetGCTypeName(change->gcType), change->argOffset);
                break;
            case RegArgChangeKind::PopArgs:
                printf("pop %u", change->argOffset);
                break;
            case RegArgChangeKind::Pop:
                printf("pop");
                break;
            case RegArgChangeKind::KillArgs:
                printf("kill-args %u", change->argOffset);
                break;
#endif
            default:
                printf("???");
                break;
        }

        printf("\n");
    }

    deltaRegArgChangeBase = lastRegArgChange;
}

void GCInfo::DumpCallSiteDelta(const char* header)
{
    if (deltaCallSiteBase == lastCallSite)
    {
        return;
    }

    CallSite* base = (deltaCallSiteBase == nullptr) ? firstCallSite : deltaCallSiteBase->next;

    for (CallSite* call = base; call != nullptr; call = call->next)
    {
        printf("%scall-site", header);

        if (call->refRegs != RBM_NONE)
        {
            printf(" ref-regs ");
            dspRegMask(call->refRegs);
        }

        if (call->byrefRegs != RBM_NONE)
        {
            printf(" byref-regs ");
            dspRegMask(call->byrefRegs);
        }

#ifdef JIT32_GCENCODER
        if ((call->argCount != 0) || (call->argMask != 0))
        {
            unsigned  count   = call->argCount;
            unsigned* offsets = call->argTable;
            unsigned  bitOffsets[sizeof(call->argMask) * CHAR_BIT];

            if (count == 0)
            {
                for (unsigned i = 0, j = 0; i < _countof(bitOffsets); i++)
                {
                    if ((call->argMask & (1 << i)) != 0)
                    {
                        bitOffsets[count] = i * TARGET_POINTER_SIZE;

                        if ((call->byrefArgMask & (1 << i)) != 0)
                        {
                            bitOffsets[count] |= byref_OFFSET_FLAG;
                        }

                        count++;
                    }
                }

                offsets = bitOffsets;
            }

            unsigned refCount   = 0;
            unsigned byrefCount = 0;

            for (unsigned i = 0; i < count; i++)
            {
                (((offsets[i] & byref_OFFSET_FLAG) != 0) ? byrefCount : refCount)++;
            }

            if (refCount != 0)
            {
                printf(" ref-args {");

                for (unsigned i = 0; i < count; i++)
                {
                    if ((offsets[i] & byref_OFFSET_FLAG) == 0)
                    {
                        printf(" %u", offsets[i]);
                    }
                }

                printf(" }");
            }

            if (byrefCount != 0)
            {
                printf(" byref-args {");

                for (unsigned i = 0; i < count; i++)
                {
                    if ((offsets[i] & byref_OFFSET_FLAG) != 0)
                    {
                        printf(" %u", offsets[i] & ~byref_OFFSET_FLAG);
                    }
                }

                printf(" }");
            }
        }
#endif

        printf("\n");
    }

    deltaCallSiteBase = lastCallSite;
}

void GCInfo::DumpStackSlotLifetimeDelta(const char* header)
{
    if (deltaStackSlotLifetime.Empty())
    {
        return;
    }

    const char* frameRegName = getRegName(compiler->codeGen->isFramePointerUsed() ? REG_FPBASE : REG_SPBASE);

    while (!deltaStackSlotLifetime.Empty())
    {
        StackSlotLifetime* lifetime = deltaStackSlotLifetime.Pop();

        int         offset   = lifetime->slotOffset & ~OFFSET_MASK;
        const char* delta    = lifetime->endCodeOffs == 0 ? "def" : "kill";
        const char* typeName = GetGCTypeName((lifetime->slotOffset & byref_OFFSET_FLAG) != 0 ? GCT_BYREF : GCT_GCREF);

#ifdef TARGET_ARMARCH
        printf("%s%s-%s-slot [%s,#%d]\n", header, delta, typeName, frameRegName, offset);
#else
        printf("%s%s-%s-slot [%s%c%02XH]\n", header, delta, typeName, frameRegName, offset < 0 ? '-' : '+',
               abs(offset));
#endif
    }
}

void GCInfo::DumpDelta(const char* header)
{
    if (!isFullyInterruptible)
    {
        DumpRegDelta(header, GCT_GCREF, deltaRefRegsBase, liveRefRegs);
        deltaRefRegsBase = liveRefRegs;
        DumpRegDelta(header, GCT_BYREF, deltaByrefRegsBase, liveByrefRegs);
        deltaByrefRegsBase = liveByrefRegs;
    }

    DumpStackSlotLifetimeDelta(header);
    DumpRegArgChangeDelta(header);
    DumpCallSiteDelta(header);
}

#endif // DEBUG

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
        return WBF_NoBarrier;
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

        return WBF_BarrierChecked;
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
            return WBF_BarrierUnknown;
        }
    }

    if (addr->TypeIs(TYP_REF))
    {
        // If we found an object reference then this should be a store the GC heap,
        // unless we're dealing with weird code that converts an unmanaged pointer
        // to TYP_REF...

        return WBF_BarrierUnchecked;
    }

    if (addr->OperIs(GT_LCL_ADDR))
    {
        // No need for a GC barrier when writing to a local variable.
        return WBF_NoBarrier;
    }

    return WBF_BarrierUnknown;
}

// Returns true if garbage collection won't happen within the helper call.
// There is no need to record live pointers for such call sites.
bool GCInfo::IsNoGCHelper(CorInfoHelpFunc helper)
{
    switch (helper)
    {
        case CORINFO_HELP_PROF_FCN_LEAVE:
        case CORINFO_HELP_PROF_FCN_ENTER:
        case CORINFO_HELP_PROF_FCN_TAILCALL:

#ifndef TARGET_64BIT
        // case CORINFO_HELP_LMUL:
        // case CORINFO_HELP_LDIV:
        // case CORINFO_HELP_LMOD:
        // case CORINFO_HELP_ULDIV:
        // case CORINFO_HELP_ULMOD:
        case CORINFO_HELP_LLSH:
        case CORINFO_HELP_LRSH:
        case CORINFO_HELP_LRSZ:
#endif

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

// Gets the set of registers that are killed by a no-GC helper call. This is used when determining
// what registers to remove from the current live GC/byref sets (and thus what to report as dead in
// the GC info). Note that for the CORINFO_HELP_ASSIGN_BYREF helper, in particular, the kill set
// reported by compHelperCallKillSet doesn't match this kill set. compHelperCallKillSet reports the
// dst/src address registers as killed for liveness purposes, since their values change. However,
// they still are valid byref pointers after the call, so the dst/src address registers are NOT
// reported as killed here.
//
// Note: This list may not be complete and defaults to the default RBM_CALLEE_TRASH_NOGC registers.
//
regMaskTP GCInfo::GetNoGCHelperCalleeKilledRegs(CorInfoHelpFunc helper)
{
    assert(GCInfo::IsNoGCHelper(helper));

    regMaskTP result;

    switch (helper)
    {
        case CORINFO_HELP_PROF_FCN_ENTER:
            result = RBM_PROFILER_ENTER_TRASH;
            break;

        case CORINFO_HELP_PROF_FCN_LEAVE:
#ifdef TARGET_ARM
            // profiler scratch remains gc live
            result = RBM_PROFILER_LEAVE_TRASH & ~RBM_PROFILER_RET_SCRATCH;
#else
            result = RBM_PROFILER_LEAVE_TRASH;
#endif
            break;

        case CORINFO_HELP_PROF_FCN_TAILCALL:
            result = RBM_PROFILER_TAILCALL_TRASH;
            break;

        case CORINFO_HELP_ASSIGN_BYREF:
#if defined(TARGET_X86)
            result = RBM_ECX;
#elif defined(TARGET_AMD64)
            result = RBM_CALLEE_TRASH_NOGC & ~(RBM_RDI | RBM_RSI);
#elif defined(TARGET_ARMARCH)
            result = RBM_CALLEE_GCTRASH_WRITEBARRIER_BYREF;
#else
#error Unknown target
#endif
            break;

#ifdef TARGET_ARMARCH
        case CORINFO_HELP_ASSIGN_REF:
        case CORINFO_HELP_CHECKED_ASSIGN_REF:
            result = RBM_CALLEE_GCTRASH_WRITEBARRIER;
            break;
#endif

#ifdef TARGET_X86
        case CORINFO_HELP_INIT_PINVOKE_FRAME:
            result = RBM_INIT_PINVOKE_FRAME_TRASH;
            break;
#endif

        default:
            result = RBM_CALLEE_TRASH_NOGC;
            break;
    }

    // compHelperCallKillSet returns a superset of the registers which values are not guranteed to be the same
    // after the call, if a register loses its GC or byref it has to be in the compHelperCallKillSet set as well.
    assert((result & Compiler::compHelperCallKillSet(helper)) == result);

    return result;
}

regMaskTP GCInfo::GetNoGCHelperCalleeSavedRegs(CorInfoHelpFunc helper)
{
    return RBM_ALLINT & ~GetNoGCHelperCalleeKilledRegs(helper);
}
