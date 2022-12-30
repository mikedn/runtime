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

void GCInfo::Init()
{
    isFullyInterruptible = compiler->codeGen->GetInterruptible();
#ifdef JIT32_GCENCODER
    isFramePointerUsed = compiler->codeGen->isFramePointerUsed();
#endif

    liveLcls = VarSetOps::MakeEmpty(compiler);
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

    INDEBUG(deltaStackSlotLifetime.Push(lifetime));

    return lifetime;
}

void GCInfo::EndStackSlotLifetime(StackSlotLifetime* lifetime DEBUGARG(int slotOffs), unsigned codeOffs)
{
    assert(lifetime->endCodeOffs == 0);
    assert(static_cast<int>(lifetime->slotOffset & ~OFFSET_MASK) == slotOffs);

    lifetime->endCodeOffs = codeOffs;

    INDEBUG(deltaStackSlotLifetime.Push(lifetime));
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

#ifdef JIT32_GCENCODER
GCInfo::RegArgChange* GCInfo::AddLiveRegs(GCtype gcType, regMaskTP regs, unsigned codeOffs, bool isThis)
#else
GCInfo::RegArgChange* GCInfo::AddLiveRegs(GCtype gcType, regMaskTP regs, unsigned codeOffs)
#endif
{
    assert(gcType != GCT_NONE);
    assert((GetAllLiveRegs() & regs) == RBM_NONE);
#ifdef JIT32_GCENCODER
    assert(isFullyInterruptible || (isThis && ReportRegArgChanges()));
    assert(!isThis || compiler->lvaKeepAliveAndReportThis());
#else
    assert(isFullyInterruptible);
#endif

    RegArgChange* change = AddRegArgChange();
    change->codeOffs     = codeOffs;
    change->kind         = RegArgChangeKind::RegChange;
    change->gcType       = gcType;
    change->addRegs      = static_cast<regMaskSmall>(regs);
    change->removeRegs   = RBM_NONE;
#ifdef JIT32_GCENCODER
    change->isThis = isThis;
#endif
    return change;
}

#ifdef JIT32_GCENCODER
void GCInfo::AddLiveReg(GCtype type, regNumber reg, unsigned codeOffs, bool isThis)
#else
void GCInfo::AddLiveReg(GCtype type, regNumber reg, unsigned codeOffs)
#endif
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

#ifdef JIT32_GCENCODER
        // For synchronized methods, "this" is always alive and in the same register.
        // However, if we generate any code after the epilog block (where "this"
        // goes dead), "this" will come alive again. We need to notice that.
        // Note that we only expect isThis to be true at an insGroup boundary.

        if (isFullyInterruptible || (ReportRegArgChanges() && isThis))
        {
            AddLiveRegs(type, regMask, codeOffs, isThis);
        }
#else
        if (isFullyInterruptible)
        {
            AddLiveRegs(type, regMask, codeOffs);
        }
#endif

        typeRegs |= regMask;
    }
}

GCInfo::RegArgChange* GCInfo::RemoveLiveRegs(GCtype gcType, regMaskTP regs, unsigned codeOffs)
{
    assert(gcType != GCT_NONE);
    assert((GetAllLiveRegs() & regs) != RBM_NONE);
    assert(isFullyInterruptible);

    RegArgChange* change = AddRegArgChange();
    change->codeOffs     = codeOffs;
    change->kind         = RegArgChangeKind::RegChange;
    change->gcType       = gcType;
    change->addRegs      = RBM_NONE;
    change->removeRegs   = static_cast<regMaskSmall>(regs);
#ifdef JIT32_GCENCODER
    change->isThis = false;
#endif
    return change;
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
GCInfo::RegArgChange* GCInfo::AddCallArgPush(unsigned codeOffs, unsigned stackLevel, GCtype gcType)
{
    RegArgChange* change = AddRegArgChange();
    change->codeOffs     = codeOffs;
    change->argOffset    = stackLevel;
    change->kind         = RegArgChangeKind::PushArg;
    change->gcType       = gcType;
    change->isThis       = false;
    return change;
}

GCInfo::RegArgChange* GCInfo::AddCallArgsKill(unsigned codeOffs, unsigned argCount)
{
    RegArgChange* change = AddRegArgChange();
    change->codeOffs     = codeOffs;
    change->argOffset    = argCount;
    change->kind         = RegArgChangeKind::KillArgs;
    change->gcType       = GCT_GCREF;
    change->isThis       = false;
    return change;
}

GCInfo::RegArgChange* GCInfo::AddCallArgsPop(unsigned codeOffs, unsigned argCount, bool isCall)
{
    // Only calls may pop more than one value.
    // cdecl calls accomplish this popping via a post-call "ADD SP, imm" instruction,
    // we treat that as "isCall" too.
    isCall |= argCount > 1;

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
    change->argOffset     = argCount;
    change->kind          = isCall ? RegArgChangeKind::PopArgs : RegArgChangeKind::Pop;
    change->gcType        = GCT_GCREF;
    change->isThis        = false;
    change->callRefRegs   = callRefRegs;
    change->callByrefRegs = callByrefRegs;
    return change;
}
#else
GCInfo::RegArgChange* GCInfo::AddCallArgStore(unsigned codeOffs, int argOffs, GCtype gcType)
{
    assert(gcType != GCT_NONE);
    assert(abs(argOffs) % REGSIZE_BYTES == 0);

    RegArgChange* change = AddRegArgChange();
    change->codeOffs     = codeOffs;
    change->argOffset    = argOffs;
    change->kind         = RegArgChangeKind::StoreArg;
    change->gcType       = gcType;
    return change;
}

GCInfo::RegArgChange* GCInfo::AddCallArgsKill(unsigned codeOffs)
{
    RegArgChange* change = AddRegArgChange();
    change->codeOffs     = codeOffs;
    change->argOffset    = 0;
    change->kind         = RegArgChangeKind::KillArgs;
    change->gcType       = GCT_GCREF;
    return change;
}
#endif

#ifdef JIT32_GCENCODER
GCInfo::CallSite* GCInfo::AddCallSite(unsigned codeOffs)
#else
GCInfo::CallSite* GCInfo::AddCallSite(unsigned codeOffs, unsigned length)
#endif
{
#ifdef JIT32_GCENCODER
    assert(ReportCallSites());
#else
    assert(!isFullyInterruptible);
    assert((0 < length) && (length <= 16));
#endif

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

    call->refRegs   = static_cast<regMaskSmall>(liveRefRegs);
    call->byrefRegs = static_cast<regMaskSmall>(liveByrefRegs);
    call->codeOffs  = codeOffs;
#ifndef JIT32_GCENCODER
    call->callInstrLength = static_cast<uint8_t>(length);
#endif

    return call;
}

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
            const unsigned slotBegin = lifetime->beginCodeOffs;
            const unsigned slotEnd   = lifetime->endCodeOffs;

            if ((slotEnd == slotBegin) || (slotEnd <= filterBegin) || (filterEnd <= slotBegin))
            {
                continue;
            }

#ifndef JIT32_GCENCODER
            // Because there is no nesting within filters, nothing should be already pinned.
            // For JIT32_GCENCODER, we should not do this check as slot lifetimes are sorted
            // sorted by beginCodeOffs, which means that we could see some lifetimes that
            // were already pinned by previous splitting.
            assert((lifetime->slotOffset & pinned_OFFSET_FLAG) == 0);
#endif

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
#ifndef JIT32_GCENCODER
                // The lifetime starts inside the filter and ends somewhere after it, so
                // we create a new lifetime for the part inside the filter and adjust the
                // start of the original lifetime to be the end of the filter.

                StackSlotLifetime* filterLifetime = new (alloc) StackSlotLifetime(slotOffset, slotBegin, filterEnd);

                filterLifetime->slotOffset |= pinned_OFFSET_FLAG;
                lifetime->beginCodeOffs = filterEnd;

                InsertSplitStackSlotLifetime(filterLifetime, lifetime);

                DBEXEC(compiler->verbose, DumpStackSlotLifetime("New (1 of 2): ", filterLifetime));
                DBEXEC(compiler->verbose, DumpStackSlotLifetime("New (2 of 2): ", lifetime));
#else
                // JIT32_GCENCODER requires lifetime to be sorted so we need to do it the other
                // way around.

                StackSlotLifetime* postFilterLifetime = new (alloc) StackSlotLifetime(slotOffset, filterEnd, slotEnd);

                lifetime->slotOffset |= pinned_OFFSET_FLAG;
                lifetime->endCodeOffs = filterEnd;

                InsertSplitStackSlotLifetime(postFilterLifetime, lifetime);

                DBEXEC(compiler->verbose, DumpStackSlotLifetime("New (1 of 2): ", lifetime));
                DBEXEC(compiler->verbose, DumpStackSlotLifetime("New (2 of 2): ", postFilterLifetime));
#endif

                continue;
            }

            assert((slotBegin < filterBegin) && (slotEnd > filterEnd));

            // The lifetime is starts before AND ends after the filter, so we need
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

// Insert StackSlotLifetimes that were created by splitting lifetimes.
// From MarkFilterStackSlotsPinned, we may have created one or two StackSlotLifetimes
// due to splitting lifetimes and these newly created StackSlotLifetimes should be
// inserted in firstStackSlotLifetime.
// However the semantics of this call depend on the architecture.
//
// x86-GCInfo requires the stack slot lifetime list to be sorted by beginCodeOffs.
// Every time inserting an entry we should keep the order of entries.
// So this function searches for a proper insertion point from "begin" then "newLifetime"
// gets inserted.
//
// For other architectures(ones that uses GCInfo{En|De}coder), we don't need any sort.
// So the argument "begin" is unused and "desc" will be inserted at the front of the list.
//
void GCInfo::InsertSplitStackSlotLifetime(StackSlotLifetime* newLifetime, StackSlotLifetime* after)
{
#ifndef JIT32_GCENCODER
    (void)after;

    newLifetime->next      = firstStackSlotLifetime;
    firstStackSlotLifetime = newLifetime;
#else
    assert(newLifetime->beginCodeOffs >= after->beginCodeOffs);

    StackSlotLifetime* next = after->next;
    StackSlotLifetime* prev = after;

    while ((next != nullptr) && (next->beginCodeOffs < newLifetime->beginCodeOffs))
    {
        prev = next;
        next = next->next;
    }

    newLifetime->next = prev->next;
    prev->next        = newLifetime;
#endif // JIT32_GCENCODER
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
    if (baseRegs == diffRegs)
    {
        return;
    }

    regMaskTP sameRegs    = baseRegs & diffRegs;
    regMaskTP removedRegs = baseRegs & ~sameRegs;
    regMaskTP addedRegs   = diffRegs & ~sameRegs;

    if (removedRegs != RBM_NONE)
    {
        printf("%skill-%s-regs ", header, GetGCTypeName(type));
        dspRegMask(removedRegs);
    }

    if (addedRegs != RBM_NONE)
    {
        printf("%sdef-%s-regs ", header, GetGCTypeName(type));
        dspRegMask(addedRegs);
    }

    printf("\n");
}

void GCInfo::DumpArgDelta(const char* header)
{
    if (deltaRegArgChangeBase == lastRegArgChange)
    {
        return;
    }

    RegArgChange* base      = (deltaRegArgChangeBase == nullptr) ? firstRegArgChange : deltaRegArgChangeBase->next;
    const char*   spRegName = getRegName(REG_SPBASE);

    for (RegArgChange* change = base; change != nullptr; change = change->next)
    {
        // Reg changes are reflected in the register sets deltaRefRegsBase/liveRefRegs
        // and deltaByrefRegsBase/liveByrefRegs, and dumped using those sets.
        if (change->kind == RegArgChangeKind::RegChange)
        {
            continue;
        }

        printf("%s", header);

        switch (change->kind)
        {
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
        printf("%s%s-%s-slot [%s,#%d]", header, delta, typeName, frameRegName, offset);
#else
        printf("%s%s-%s-slot [%s%c%02XH]", header, delta, typeName, frameRegName, offset < 0 ? '-' : '+', abs(offset));
#endif
    }

    printf("\n");
}

void GCInfo::DumpDelta(const char* header)
{
    DumpRegDelta(header, GCT_GCREF, deltaRefRegsBase, liveRefRegs);
    deltaRefRegsBase = liveRefRegs;
    DumpRegDelta(header, GCT_BYREF, deltaByrefRegsBase, liveByrefRegs);
    deltaByrefRegsBase = liveByrefRegs;
    DumpStackSlotLifetimeDelta(header);
    DumpArgDelta(header);
}

#endif // DEBUG
