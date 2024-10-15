// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "lsra.h"
#include "jitgcinfo.h"

void RefInfoList::Add(RefPosition* ref, GenTree* node, Compiler* compiler)
{
    RefInfoListNode* def = AllocDef(ref, node, compiler);
    assert(def->next == nullptr);

    if (tail == nullptr)
    {
        assert(head == nullptr);
        head = def;
    }
    else
    {
        tail->next = def;
    }

    tail = def;
}

RefPosition* RefInfoList::Remove(GenTree* node, unsigned regIndex)
{
    RefInfoListNode* prevDef = nullptr;

    for (RefInfoListNode* def = head; def != nullptr; def = def->next)
    {
        if ((def->node == node) && (def->ref->getMultiRegIdx() == regIndex))
        {
            RefPosition* ref = def->ref;
            Unlink(def, prevDef);
            FreeDef(def);
            return ref;
        }

        prevDef = def;
    }

    assert(!"Reg def not found");
    unreached();
}

void RefInfoList::Unlink(RefInfoListNode* def, RefInfoListNode* prevDef)
{
    RefInfoListNode* next = def->next;

    if (prevDef == nullptr)
    {
        head = next;
    }
    else
    {
        prevDef->next = next;
    }

    if (next == nullptr)
    {
        tail = prevDef;
    }

    def->next = nullptr;
}

RefInfoListNode* RefInfoList::AllocDef(RefPosition* ref, GenTree* node, Compiler* compiler)
{
    RefInfoListNode* head = freeList;

    if (head == nullptr)
    {
        head = compiler->getAllocator(CMK_LSRA).allocate<RefInfoListNode>(1);
    }
    else
    {
        freeList = head->next;
    }

    head->ref  = ref;
    head->node = node;
    head->next = nullptr;

    return head;
}

void RefInfoList::FreeDef(RefInfoListNode* def)
{
    def->next = freeList;
    freeList  = def;
}

Interval* LinearScan::newInterval(var_types regType)
{
    intervals.emplace_back(regType, allRegs(regType));
    Interval* newInt = &intervals.back();

    INDEBUG(newInt->intervalIndex = static_cast<unsigned>(intervals.size() - 1);)
    DBEXEC(VERBOSE, newInt->dump());
    return newInt;
}

RefPosition* LinearScan::newRefPositionRaw(LsraLocation location, GenTree* node, RefType refType)
{
    refPositions.emplace_back(curBBNum, location, node, refType);
    RefPosition* newRP = &refPositions.back();
    INDEBUG(newRP->rpNum = static_cast<unsigned>(refPositions.size() - 1));
    return newRP;
}

// Resolve the situation where we have conflicting def and use
// register requirements on a single-def, single-use interval.
//
// Assumptions:
//    The two RefPositions are for the same interval, which is a tree-temp.
//
// Notes:
//    We require some special handling for the case where the use is a "delayRegFree" case of a fixedReg.
//    In that case, if we change the registerAssignment on the useRefPosition, we will lose the fact that,
//    even if we assign a different register (and rely on codegen to do the copy), that fixedReg also needs
//    to remain busy until the Def register has been allocated.  In that case, we don't allow Case 1 or Case 4
//    below.
//    Here are the cases we consider (in this order):
//    1. If The defRefPosition specifies a single register, and there are no conflicting
//       FixedReg uses of it between the def and use, we use that register, and the code generator
//       will insert the copy.  Note that it cannot be in use because there is a FixedRegRef for the def.
//    2. If the useRefPosition specifies a single register, and it is not in use, and there are no
//       conflicting FixedReg uses of it between the def and use, we use that register, and the code generator
//       will insert the copy.
//    3. If the defRefPosition specifies a single register (but there are conflicts, as determined
//       in 1.), and there are no conflicts with the useRefPosition register (if it's a single register),
//      we set the register requirements on the defRefPosition to the use registers, and the
//       code generator will insert a copy on the def.  We can't rely on the code generator to put a copy
//       on the use if it has multiple possible candidates, as it won't know which one has been allocated.
//    4. If the useRefPosition specifies a single register, and there are no conflicts with the register
//       on the defRefPosition, we leave the register requirements on the defRefPosition as-is, and set
//       the useRefPosition to the def registers, for similar reasons to case #3.
//    5. If both the defRefPosition and the useRefPosition specify single registers, but both have conflicts,
//       We set the candidates on defRefPosition to be all regs of the appropriate type, and since they are
//       single registers, codegen can insert the copy.
//    6. Finally, if the RefPositions specify disjoint subsets of the registers (or the use is fixed but
//       has a conflict), we must insert a copy.  The copy will be inserted before the use if the
//       use is not fixed (in the fixed case, the code generator will insert the use).
//
// TODO-CQ: We get bad register allocation in case #3 in the situation where no register is
// available for the lifetime.  We end up allocating a register that must be spilled, and it probably
// won't be the register that is actually defined by the target instruction.  So, we have to copy it
// and THEN spill it.  In this case, we should be using the def requirement.  But we need to change
// the interface to this method a bit to make that work (e.g. returning a candidate set to use, but
// leaving the registerAssignment as-is on the def, so that if we find that we need to spill anyway
// we can use the fixed-reg on the def.
void LinearScan::resolveConflictingDefAndUse(Interval* interval, RefPosition* defRefPosition)
{
    assert(!interval->isLocalVar);

    RefPosition* useRefPosition   = defRefPosition->nextRefPosition;
    regMaskTP    defRegAssignment = defRefPosition->registerAssignment;
    regMaskTP    useRegAssignment = useRefPosition->registerAssignment;
    RegRecord*   defRegRecord     = nullptr;
    RegRecord*   useRegRecord     = nullptr;
    regNumber    defReg           = REG_NA;
    regNumber    useReg           = REG_NA;
    bool         defRegConflict   = ((defRegAssignment & useRegAssignment) == RBM_NONE);
    bool         useRegConflict   = defRegConflict;

    // If the useRefPosition is a "delayRegFree", we can't change the registerAssignment
    // on it, or we will fail to ensure that the fixedReg is busy at the time the target
    // (of the node that uses this interval) is allocated.
    bool canChangeUseAssignment = !useRefPosition->isFixedRegRef || !useRefPosition->delayRegFree;

    INDEBUG(dumpLsraAllocationEvent(LSRA_EVENT_DEFUSE_CONFLICT));
    if (!canChangeUseAssignment)
    {
        INDEBUG(dumpLsraAllocationEvent(LSRA_EVENT_DEFUSE_FIXED_DELAY_USE));
    }
    if (defRefPosition->isFixedRegRef && !defRegConflict)
    {
        defReg       = defRefPosition->assignedReg();
        defRegRecord = getRegisterRecord(defReg);
        if (canChangeUseAssignment)
        {
            RefPosition* currFixedRegRefPosition = defRegRecord->recentRefPosition;
            assert(currFixedRegRefPosition != nullptr &&
                   currFixedRegRefPosition->nodeLocation == defRefPosition->nodeLocation);

            if (currFixedRegRefPosition->nextRefPosition == nullptr ||
                currFixedRegRefPosition->nextRefPosition->nodeLocation > useRefPosition->getRefEndLocation())
            {
                // This is case #1.  Use the defRegAssignment
                INDEBUG(dumpLsraAllocationEvent(LSRA_EVENT_DEFUSE_CASE1));
                useRefPosition->registerAssignment = defRegAssignment;
                return;
            }
            else
            {
                defRegConflict = true;
            }
        }
    }
    if (useRefPosition->isFixedRegRef && !useRegConflict)
    {
        useReg       = useRefPosition->assignedReg();
        useRegRecord = getRegisterRecord(useReg);

        // We know that useRefPosition is a fixed use, so the nextRefPosition must not be null.
        RefPosition* nextFixedRegRefPosition = useRegRecord->getNextRefPosition();
        assert(nextFixedRegRefPosition != nullptr &&
               nextFixedRegRefPosition->nodeLocation <= useRefPosition->nodeLocation);

        // First, check to see if there are any conflicting FixedReg references between the def and use.
        if (nextFixedRegRefPosition->nodeLocation == useRefPosition->nodeLocation)
        {
            // OK, no conflicting FixedReg references.
            // Now, check to see whether it is currently in use.
            if (useRegRecord->assignedInterval != nullptr)
            {
                RefPosition* possiblyConflictingRef         = useRegRecord->assignedInterval->recentRefPosition;
                LsraLocation possiblyConflictingRefLocation = possiblyConflictingRef->getRefEndLocation();
                if (possiblyConflictingRefLocation >= defRefPosition->nodeLocation)
                {
                    useRegConflict = true;
                }
            }
            if (!useRegConflict)
            {
                // This is case #2.  Use the useRegAssignment
                INDEBUG(dumpLsraAllocationEvent(LSRA_EVENT_DEFUSE_CASE2, interval));
                defRefPosition->registerAssignment = useRegAssignment;
                return;
            }
        }
        else
        {
            useRegConflict = true;
        }
    }
    if (defRegRecord != nullptr && !useRegConflict)
    {
        // This is case #3.
        INDEBUG(dumpLsraAllocationEvent(LSRA_EVENT_DEFUSE_CASE3, interval));
        defRefPosition->registerAssignment = useRegAssignment;
        return;
    }
    if (useRegRecord != nullptr && !defRegConflict && canChangeUseAssignment)
    {
        // This is case #4.
        INDEBUG(dumpLsraAllocationEvent(LSRA_EVENT_DEFUSE_CASE4, interval));
        useRefPosition->registerAssignment = defRegAssignment;
        return;
    }
    if (defRegRecord != nullptr && useRegRecord != nullptr)
    {
        // This is case #5.
        INDEBUG(dumpLsraAllocationEvent(LSRA_EVENT_DEFUSE_CASE5, interval));
        RegisterType regType = interval->registerType;
        assert((getRegisterType(interval, defRefPosition) == regType) &&
               (getRegisterType(interval, useRefPosition) == regType));
        regMaskTP candidates               = allRegs(regType);
        defRefPosition->registerAssignment = candidates;
        defRefPosition->isFixedRegRef      = false;
        return;
    }
    INDEBUG(dumpLsraAllocationEvent(LSRA_EVENT_DEFUSE_CASE6, interval));
    return;
}

// Set register preferences for an interval based on the given RefPosition
// This is slightly more general than its name applies, and updates preferences
// not just for callee-save registers.
void LinearScan::applyCalleeSaveHeuristics(RefPosition* rp)
{
#ifdef TARGET_AMD64
    if (compiler->opts.compDbgEnC)
    {
        // We only use RSI and RDI for EnC code, so we don't want to favor callee-save regs.
        return;
    }
#endif // TARGET_AMD64

    Interval* theInterval = rp->getInterval();

#ifdef DEBUG
    if (!doReverseCallerCallee())
#endif // DEBUG
    {
        // Set preferences so that this register set will be preferred for earlier refs
        theInterval->mergeRegisterPreferences(rp->registerAssignment);
    }
}

// Ensure that we have consistent def/use on SDSU temps.
// There are a couple of cases where this may over-constrain allocation:
// 1. In the case of a non-commutative rmw def (in which the rmw source must be delay-free), or
// 2. In the case where the defining node requires a temp distinct from the target (also a
//    delay-free case).
// In those cases, if we propagate a single-register restriction from the consumer to the producer
// the delayed uses will not see a fixed reference in the PhysReg at that position, and may
// incorrectly allocate that register.
// TODO-CQ: This means that we may often require a copy at the use of this node's result.
// This case could be moved to BuildRefPositionsForNode, at the point where the def RefPosition is
// created, causing a RefTypeFixedReg to be added at that location. This, however, results in
// more PhysReg RefPositions (a throughput impact), and a large number of diffs that require
// further analysis to determine benefit.
// See Issue #11274.
void LinearScan::checkConflictingDefUse(RefPosition* useRP)
{
    assert(useRP->refType == RefTypeUse);
    Interval* theInterval = useRP->getInterval();
    assert(!theInterval->isLocalVar);

    RefPosition* defRP = theInterval->firstRefPosition;

    // All defs must have a valid treeNode, but we check it below to be conservative.
    assert(defRP->treeNode != nullptr);
    regMaskTP prevAssignment = defRP->registerAssignment;
    regMaskTP newAssignment  = (prevAssignment & useRP->registerAssignment);
    if (newAssignment != RBM_NONE)
    {
        if (!isSingleRegister(newAssignment) || !theInterval->hasInterferingUses)
        {
            defRP->registerAssignment = newAssignment;
        }
    }
    else
    {
        theInterval->hasConflictingDefUse = true;
    }
}

// Update the Interval based on the given RefPosition.
// This is called at the time when 'rp' has just been created, so it becomes
// the nextRefPosition of the recentRefPosition, and both the recentRefPosition
// and lastRefPosition of its referent.
void LinearScan::associateRefPosWithInterval(RefPosition* rp)
{
    Referenceable* theReferent = rp->referent;

    assert(theReferent != nullptr);

    if (rp->isIntervalRef())
    {
        Interval* theInterval = rp->getInterval();

        applyCalleeSaveHeuristics(rp);

        if (theInterval->isLocalVar)
        {
            if (RefTypeIsUse(rp->refType))
            {
                RefPosition* const prevRP = theInterval->recentRefPosition;
                if ((prevRP != nullptr) && (prevRP->bbNum == rp->bbNum))
                {
                    prevRP->lastUse = false;
                }
            }

            rp->lastUse = (rp->refType != RefTypeExpUse) && (rp->refType != RefTypeParamDef) &&
                          (rp->refType != RefTypeZeroInit) && !extendLifetimes();
        }
        else if (rp->refType == RefTypeUse)
        {
            checkConflictingDefUse(rp);
            rp->lastUse = true;
        }
    }

    LinkRefPosition(rp);
}

void LinearScan::LinkRefPosition(RefPosition* rp)
{
    Referenceable* referent = rp->referent;

    if (RefPosition* prev = referent->recentRefPosition)
    {
        prev->nextRefPosition = rp;
    }
    else
    {
        referent->firstRefPosition = rp;
    }

    referent->recentRefPosition = rp;
    referent->lastRefPosition   = rp;
}

RefPosition* LinearScan::newRegRefPosition(RegNum reg, LsraLocation location, RefType refType)
{
    assert((refType == RefTypeFixedReg) || (refType == RefTypeKill));

    RegRecord* regRecord = getRegisterRecord(reg);

    RefPosition* newRP = newRefPositionRaw(location, nullptr, refType);
    newRP->setReg(regRecord);
    newRP->registerAssignment = genRegMask(reg);

    // We can't have two RefPositions on a RegRecord at the same location, unless they are different types.
    assert((regRecord->lastRefPosition == nullptr) || (regRecord->lastRefPosition->nodeLocation < location) ||
           (regRecord->lastRefPosition->refType != refType));

    LinkRefPosition(newRP);

    DBEXEC(VERBOSE, newRP->dump(this));

    return newRP;
}

RefPosition* LinearScan::newBlockRefPosition(LsraLocation location)
{
    RefPosition* newRP = newRefPositionRaw(location, nullptr, RefTypeBB);
    DBEXEC(VERBOSE, newRP->dump(this));
    return newRP;
}

RefPosition* LinearScan::newKillGCRegsRefPosition(LsraLocation location, GenTree* node, regMaskTP mask)
{
    RefPosition* newRP        = newRefPositionRaw(location, node, RefTypeKillGCRefs);
    newRP->isFixedRegRef      = isSingleRegister(mask);
    newRP->registerAssignment = mask;
    DBEXEC(VERBOSE, newRP->dump(this));
    return newRP;
}

RefPosition* LinearScan::newRefPosition(
    Interval* interval, LsraLocation location, RefType refType, GenTree* node, regMaskTP mask, unsigned regIndex)
{
    assert(interval != nullptr);

    if (mask == RBM_NONE)
    {
        mask = allRegs(interval->registerType);
    }

#ifdef DEBUG
    if (regType(interval->registerType) == FloatRegisterType)
    {
        // In the case we're using floating point registers we must make sure
        // this flag was set previously in the compiler since this will mandate
        // whether LSRA will take into consideration FP reg killsets.
        assert(compiler->compFloatingPointUsed || ((mask & RBM_FLT_CALLEE_SAVED) == 0));
    }
#endif

    // If this reference is constrained to a single register, add a RefTypeFixedReg
    // at this location so that its availability can be more accurately determined.

    bool isFixedRegister = isSingleRegister(mask);
    bool insertFixedRef  = false;

    if (isFixedRegister)
    {
        // Insert a RefTypeFixedReg for any normal def or use, but not
        // an internal use (it will already have a FixedRef for the def).
        if ((refType == RefTypeDef) || ((refType == RefTypeUse) && !interval->isInternal))
        {
            insertFixedRef = true;
        }
    }

    if (insertFixedRef)
    {
        RefPosition* pos = newRegRefPosition(genRegNumFromMask(mask), location, RefTypeFixedReg);
        assert((allRegs(interval->registerType) & mask) != RBM_NONE);
    }

    RefPosition* newRP = newRefPositionRaw(location, node, refType);
    newRP->setInterval(interval);
    newRP->isFixedRegRef = isFixedRegister;

#ifndef TARGET_AMD64
    // We don't need this for AMD because the PInvoke method epilog code is explicit
    // at register allocation time.
    if (interval->isLocalVar && compiler->compMethodRequiresPInvokeFrame() &&
        (interval->getLocalVar(compiler)->GetLclNum() == compiler->genReturnLocal))
    {
        mask &= ~(RBM_PINVOKE_TCB | RBM_PINVOKE_FRAME);
        noway_assert(mask != RBM_NONE);
    }
#endif

    newRP->registerAssignment = mask;
    newRP->setMultiRegIdx(regIndex);

    associateRefPosWithInterval(newRP);

    if (RefTypeIsDef(newRP->refType))
    {
        // TODO-MIKE-Review: Disabling single def reg stuff for multireg stores,
        // codegen doesn't seem to have proper spilling support for this case.
        interval->isSingleDef = (interval->firstRefPosition == newRP) && (node == nullptr || !node->IsMultiRegLclVar());
    }

    DBEXEC(VERBOSE, newRP->dump(this));
    return newRP;
}

void LinearScan::newRegKillRefPositions(regMaskTP mask, LsraLocation currentLoc)
{
    for (RegNum reg = REG_FIRST; mask; reg = REG_NEXT(reg), mask >>= 1)
    {
        if (mask & 1)
        {
            RefPosition* pos = newRegRefPosition(reg, currentLoc, RefTypeKill);
            pos->lastUse     = true;
        }
    }
}

// Determine the liveness kill set for a IND_STORE node.
// If the IND_STORE will generate a write barrier, determine the specific kill
// set required by the case-specific, platform-specific write barrier. If no
// write barrier is required, the kill set will be RBM_NONE.
regMaskTP LinearScan::getKillSetForStoreInd(GenTreeIndStore* tree)
{
    regMaskTP killMask = RBM_NONE;

    GenTree* data = tree->GetValue();

    GCInfo::WriteBarrierForm writeBarrierForm = GCInfo::GetWriteBarrierForm(tree);
    if (writeBarrierForm != GCInfo::WBF_NoBarrier)
    {
        if (GCInfo::UseOptimizedWriteBarriers())
        {
            // We can't determine the exact helper to be used at this point, because it depends on
            // the allocated register for the `data` operand. However, all the (x86) optimized
            // helpers have the same kill set: EDX. And note that currently, only x86 can return
            // `true` for UseOptimizedWriteBarriers().
            killMask = RBM_CALLEE_TRASH_NOGC;
        }
        else
        {
            // Figure out which helper we're going to use, and then get the kill set for that helper.
            killMask = compiler->compHelperCallKillSet(GCInfo::GetWriteBarrierHelperCall(writeBarrierForm));
        }
    }
    return killMask;
}

#ifdef TARGET_XARCH
regMaskTP LinearScan::getKillSetForShiftRotate(GenTreeOp* node)
{
    assert(node->OperIsShiftOrRotate());

    if (!node->GetOp(1)->isContained())
    {
        return RBM_RCX;
    }

    return RBM_NONE;
}

regMaskTP LinearScan::getKillSetForMul(GenTreeOp* node)
{
#ifdef TARGET_64BIT
    assert(node->OperIs(GT_MUL, GT_SMULH, GT_UMULH, GT_OVF_SMUL, GT_OVF_UMUL));
#else
    assert(node->OperIs(GT_MUL, GT_SMULH, GT_UMULH, GT_OVF_SMUL, GT_OVF_UMUL, GT_SMULL, GT_UMULL));
#endif

    if (!node->OperIs(GT_MUL, GT_OVF_SMUL))
    {
        return RBM_RAX | RBM_RDX;
    }

    return RBM_NONE;
}

regMaskTP LinearScan::getKillSetForModDiv(GenTreeOp* node)
{
    assert(node->OperIs(GT_MOD, GT_DIV, GT_UMOD, GT_UDIV));

    return RBM_RAX | RBM_RDX;
}
#endif // TARGET_XARCH

regMaskTP LinearScan::getKillSetForCall(GenTreeCall* call)
{
#ifdef TARGET_X86
    if (compiler->compFloatingPointUsed)
    {
        if (call->TypeIs(TYP_DOUBLE))
        {
            needDoubleTmpForFPCall = true;
        }
        else if (call->TypeIs(TYP_FLOAT))
        {
            needFloatTmpForFPCall = true;
        }
    }
#endif

#if defined(TARGET_X86) || defined(TARGET_ARM)
    if (call->IsHelperCall())
    {
        return compiler->compHelperCallKillSet(compiler->eeGetHelperNum(call->GetMethodHandle()));
    }
#endif

    // If the method does not use FP registers, we can ignore the FP kills
    IntRegMask killMask = compiler->compFloatingPointUsed ? RBM_CALLEE_TRASH : RBM_INT_CALLEE_TRASH;

#ifdef TARGET_ARM
    if (call->IsVirtualStub())
    {
        killMask |= genRegMask(compiler->info.virtualStubParamRegNum);
    }
#else
    // Verify that the special virtual stub call register is in the kill mask.
    // We don't just add it unconditionally to the killMask because for most
    // architectures it is already in the RBM_CALLEE_TRASH set, and we don't
    // want to introduce extra checks and calls in this hot function.
    assert(!call->IsVirtualStub() || ((killMask & genRegMask(compiler->info.virtualStubParamRegNum)) != RBM_NONE));
#endif

    return killMask;
}

regMaskTP LinearScan::getKillSetForStructStore(StructStoreKind kind)
{
    switch (kind)
    {
#if defined(UNIX_AMD64_ABI) || defined(TARGET_ARM64)
        case StructStoreKind::UnrollRegsWB:
            return compiler->compHelperCallKillSet(CORINFO_HELP_CHECKED_ASSIGN_REF);
#endif

        case StructStoreKind::UnrollCopyWB:
#ifdef TARGET_XARCH
        case StructStoreKind::UnrollCopyWBRepMovs:
#endif
            return compiler->compHelperCallKillSet(CORINFO_HELP_ASSIGN_BYREF);

        case StructStoreKind::UnrollInit:
        case StructStoreKind::UnrollCopy:
#if FEATURE_MULTIREG_RET
        case StructStoreKind::UnrollRegs:
#endif
            return RBM_NONE;

#ifndef TARGET_X86
        case StructStoreKind::MemSet:
            return compiler->compHelperCallKillSet(CORINFO_HELP_MEMSET);
        case StructStoreKind::MemCpy:
            return compiler->compHelperCallKillSet(CORINFO_HELP_MEMCPY);
#endif

#ifdef TARGET_XARCH
        case StructStoreKind::RepStos:
            return RBM_RCX | RBM_RDI;
        case StructStoreKind::RepMovs:
            return RBM_RCX | RBM_RDI | RBM_RSI;
#endif

        default:
            unreached();
    }
}

#ifdef FEATURE_HW_INTRINSICS
regMaskTP LinearScan::getKillSetForHWIntrinsic(GenTreeHWIntrinsic* node)
{
    regMaskTP killMask = RBM_NONE;
#ifdef TARGET_XARCH
    switch (node->GetIntrinsic())
    {
        case NI_SSE2_MaskMove:
            // maskmovdqu uses edi as the implicit address register.
            // Although it is set as the srcCandidate on the address, if there is also a fixed
            // assignment for the definition of the address, resolveConflictingDefAndUse() may
            // change the register assignment on the def or use of a tree temp (SDSU) when there
            // is a conflict, and the FixedRef on edi won't be sufficient to ensure that another
            // Interval will not be allocated there.
            // Issue #17674 tracks this.
            killMask = RBM_EDI;
            break;

        default:
            // Leave killMask as RBM_NONE
            break;
    }
#endif // TARGET_XARCH
    return killMask;
}
#endif // FEATURE_HW_INTRINSICS

regMaskTP LinearScan::getKillSetForReturn()
{
    return compiler->compIsProfilerHookNeeded() ? compiler->compHelperCallKillSet(CORINFO_HELP_PROF_FCN_LEAVE)
                                                : RBM_NONE;
}

regMaskTP LinearScan::getKillSetForProfilerHook()
{
    return compiler->compIsProfilerHookNeeded() ? compiler->compHelperCallKillSet(CORINFO_HELP_PROF_FCN_TAILCALL)
                                                : RBM_NONE;
}

#ifdef DEBUG
regMaskTP LinearScan::getKillSetForNode(GenTree* tree)
{
    switch (tree->GetOper())
    {
#ifdef TARGET_XARCH
        case GT_LSH:
        case GT_RSH:
        case GT_RSZ:
        case GT_ROL:
        case GT_ROR:
#ifdef TARGET_X86
        case GT_LSH_HI:
        case GT_RSH_LO:
#endif
            return getKillSetForShiftRotate(tree->AsOp());
        case GT_MUL:
        case GT_SMULH:
        case GT_UMULH:
        case GT_OVF_SMUL:
        case GT_OVF_UMUL:
#ifdef TARGET_X86
        case GT_SMULL:
        case GT_UMULL:
#endif
            return getKillSetForMul(tree->AsOp());
        case GT_MOD:
        case GT_DIV:
        case GT_UMOD:
        case GT_UDIV:
            return getKillSetForModDiv(tree->AsOp());
#endif // TARGET_XARCH

        case GT_LCL_STORE:
        case GT_LCL_STORE_FLD:
            if (tree->TypeIs(TYP_STRUCT) && !tree->AsLclVarCommon()->GetOp(0)->IsCall())
            {
                ClassLayout* layout = tree->OperIs(GT_LCL_STORE) ? tree->AsLclStore()->GetLcl()->GetLayout()
                                                                 : tree->AsLclStoreFld()->GetLayout(compiler);
                return getKillSetForStructStore(GetStructStoreKind(true, layout, tree->AsLclVarCommon()->GetOp(0)));
            }

            return RBM_NONE;

        case GT_IND_STORE_OBJ:
        case GT_IND_STORE_BLK:
            return getKillSetForStructStore(tree->AsBlk()->GetKind());
        case GT_COPY_BLK:
        case GT_INIT_BLK:
            return getKillSetForStructStore(tree->AsDynBlk()->GetKind());
        case GT_RETURNTRAP:
            return compiler->compHelperCallKillSet(CORINFO_HELP_STOP_FOR_GC);
        case GT_CALL:
            return getKillSetForCall(tree->AsCall());
        case GT_IND_STORE:
            return getKillSetForStoreInd(tree->AsIndStore());

#ifdef PROFILING_SUPPORTED
        // If this method requires profiler ELT hook then mark these nodes as killing
        // callee trash registers (excluding RAX and XMM0). The reason for this is that
        // profiler callback would trash these registers. See vm\amd64\asmhelpers.asm for
        // more details.
        case GT_RETURN:
            return getKillSetForReturn();
        case GT_PROF_HOOK:
            return getKillSetForProfilerHook();
#endif

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            return getKillSetForHWIntrinsic(tree->AsHWIntrinsic());
#endif

        default:
            return RBM_NONE;
    }
}
#endif // DEBUG

// Given some tree node add refpositions for all the registers this node kills
//
// Arguments:
//    tree       - the tree for which kill positions should be generated
//    currentLoc - the location at which the kills should be added
//    killMask   - The mask of registers killed by this node
//
// Return Value:
//    true       - kills were inserted
//    false      - no kills were inserted
//
// Notes:
//    The return value is needed because if we have any kills, we need to make sure that
//    all defs are located AFTER the kills.  On the other hand, if there aren't kills,
//    the multiple defs for a regPair are in different locations.
//    If we generate any kills, we will mark all currentLiveVars as being preferenced
//    to avoid the killed registers.  This is somewhat conservative.
//
//    This method can add kills even if killMask is RBM_NONE, if this tree is one of the
//    special cases that signals that we can't permit callee save registers to hold GC refs.
bool LinearScan::buildKillPositionsForNode(GenTree* tree, LsraLocation currentLoc, regMaskTP killMask)
{
    bool insertedKills = false;

    if (killMask != RBM_NONE)
    {
        // The killMask identifies a set of registers that will be used during codegen.
        // Mark these as modified here, so when we do final frame layout, we'll know about
        // all these registers. This is especially important if killMask contains
        // callee-saved registers, which affect the frame size since we need to save/restore them.
        // In the case where we have a copyBlk with GC pointers, can need to call the
        // CORINFO_HELP_ASSIGN_BYREF helper, which kills callee-saved RSI and RDI, if
        // LSRA doesn't assign RSI/RDI, they wouldn't get marked as modified until codegen,
        // which is too late.
        m_allocateRegs |= killMask;

        newRegKillRefPositions(killMask, currentLoc);

        // TODO-CQ: It appears to be valuable for both fp and int registers to avoid killing the callee
        // save regs on infrequently executed paths.  However, it results in a large number of asmDiffs,
        // many of which appear to be regressions (because there is more spill on the infrequently path),
        // but are not really because the frequent path becomes smaller.  Validating these diffs will need
        // to be done before making this change.
        // Also note that we avoid setting callee-save preferences for floating point. This may need
        // revisiting, and note that it doesn't currently apply to SIMD types, only float or double.
        // if (!currentBlock->isRunRarely())
        if (enregisterLocalVars)
        {
            for (VarSetOps::Enumerator e(compiler, currentLiveVars); e.MoveNext();)
            {
                const unsigned varIndex = e.Current();
                LclVarDsc*     varDsc   = compiler->lvaGetDescByTrackedIndex(varIndex);
#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
                if (Compiler::varTypeNeedsPartialCalleeSave(varDsc->GetRegisterType()))
                {
                    if (!VarSetOps::IsMember(compiler, largeVectorCalleeSaveCandidateVars, varIndex))
                    {
                        continue;
                    }
                }
                else
#endif // FEATURE_PARTIAL_SIMD_CALLEE_SAVE
                    if (varTypeIsFloating(varDsc) &&
                        !VarSetOps::IsMember(compiler, fpCalleeSaveCandidateVars, varIndex))
                {
                    continue;
                }
                Interval*  interval   = getIntervalForLocalVar(varIndex);
                const bool isCallKill = ((killMask == RBM_INT_CALLEE_TRASH) || (killMask == RBM_CALLEE_TRASH));

                if (isCallKill)
                {
                    interval->preferCalleeSave = true;
                }

                // We are more conservative about allocating callee-saves registers to write-thru vars, since
                // a call only requires reloading after (not spilling before). So we record (above) the fact
                // that we'd prefer a callee-save register, but we don't update the preferences at this point.
                // See the "heuristics for writeThru intervals" in 'buildIntervals()'.
                if (!interval->isWriteThru || !isCallKill)
                {
                    regMaskTP newPreferences = allRegs(interval->registerType) & (~killMask);

                    if (newPreferences != RBM_NONE)
                    {
                        interval->updateRegisterPreferences(newPreferences);
                    }
                    else
                    {
                        // If there are no callee-saved registers, the call could kill all the registers.
                        // This is a valid state, so in that case assert should not trigger. The RA will spill in order
                        // to free a register later.
                        assert(compiler->opts.compDbgEnC || (calleeSaveRegs(varDsc->GetType()) == RBM_NONE));
                    }
                }
            }
        }

        insertedKills = true;
    }

    if (compiler->killGCRefs(tree))
    {
        newKillGCRegsRefPosition(currentLoc, tree, allRegs(TYP_REF) & ~RBM_ARG_REGS);
        insertedKills = true;
    }

    return insertedKills;
}

// Check whether a MultiReg node should remain a candidate MultiReg
// When identifying candidates, the register allocator will only retain
// promoted fields of a multi-reg local as candidates if all of its fields
// are candidates. This is because of the added complexity of dealing with a
// def or use of a multi-reg lclVar when only some of the fields have liveness
// info.
// At the time we determine whether a multi-reg lclVar can still be handled
// as such, we've already completed Lowering, so during the build phase of
// LSRA we have to reset the GTF_VAR_MULTIREG flag if necessary as we visit
// each node.
bool LinearScan::IsCandidateLclVarMultiReg(GenTreeLclStore* store)
{
    if (!store->IsMultiReg())
    {
        return false;
    }

    LclVarDsc* varDsc = store->GetLcl();
    assert(varDsc->IsPromoted());

    bool isMultiReg = varDsc->IsIndependentPromoted();

    if (!isMultiReg)
    {
        store->ClearMultiReg();
    }

#ifdef DEBUG
    for (LclVarDsc* fieldLcl : compiler->PromotedFields(varDsc))
    {
        assert(fieldLcl->IsRegCandidate() == isMultiReg);
    }
#endif

    return isMultiReg;
}

// Check whether a LCL_LOAD node is a candidate or contained.
// We handle candidate variables differently from non-candidate ones.
// If it is a candidate, we will simply add a use of it at its parent/consumer.
// Otherwise, for a use we need to actually add the appropriate references for loading
// or storing the variable.
//
// A candidate lclVar won't actually get used until the appropriate ancestor node
// is processed, unless this is marked "isLocalDefUse" because it is a stack-based argument
// to a call or an orphaned dead node.
//
// Also, because we do containment analysis before we redo dataflow and identify register
// candidates, the containment analysis only uses !lvDoNotEnregister to estimate register
// candidates.
// If there is a lclVar that is estimated during Lowering to be register candidate but turns
// out not to be, if a use was marked regOptional it should now be marked contained instead.
bool LinearScan::checkContainedOrCandidateLclVar(GenTreeLclLoad* load)
{
    assert(!load->IsMultiReg());
    // We shouldn't be calling this if this node was already contained.
    assert(!load->isContained());

    bool isCandidate = load->GetLcl()->IsRegCandidate();

    if (!isCandidate && load->IsRegOptional())
    {
        load->ClearRegOptional();
        load->SetContained();

        return true;
    }

    return isCandidate;
}

RefPosition* LinearScan::defineNewInternalTemp(GenTree* tree, RegisterType regType, regMaskTP regMask)
{
    Interval* current   = newInterval(regType);
    current->isInternal = true;
    RefPosition* newDef = newRefPosition(current, currentLoc, RefTypeDef, tree, regMask, 0);
    assert(internalCount < _countof(internalDefs));
    internalDefs[internalCount++] = newDef;
    return newDef;
}

RefPosition* LinearScan::buildInternalIntRegisterDefForNode(GenTree* tree, regMaskTP internalCands)
{
    // The candidate set should contain only integer registers.
    assert((internalCands & ~allRegs(TYP_INT)) == RBM_NONE);

    RefPosition* defRefPosition = defineNewInternalTemp(tree, IntRegisterType, internalCands);
    return defRefPosition;
}

RefPosition* LinearScan::buildInternalFloatRegisterDefForNode(GenTree* tree, regMaskTP internalCands)
{
    // The candidate set should contain only float registers.
    assert((internalCands & ~allRegs(TYP_FLOAT)) == RBM_NONE);

    return defineNewInternalTemp(tree, FloatRegisterType, internalCands);
}

void LinearScan::buildInternalRegisterUses()
{
    assert(internalCount <= _countof(internalDefs));

    for (int i = 0; i < internalCount; i++)
    {
        RefPosition* def  = internalDefs[i];
        regMaskTP    mask = def->registerAssignment;
        RefPosition* use  = newRefPosition(def->getInterval(), currentLoc, RefTypeUse, def->treeNode, mask, 0);

        if (setInternalRegsDelayFree)
        {
            setDelayFree(use);
        }
    }
}

#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
void LinearScan::makeUpperVectorInterval(unsigned varIndex)
{
    Interval* lclVarInterval = getIntervalForLocalVar(varIndex);
    assert(Compiler::varTypeNeedsPartialCalleeSave(lclVarInterval->registerType));
    Interval* newInt        = newInterval(LargeVectorSaveType);
    newInt->relatedInterval = lclVarInterval;
    newInt->isUpperVector   = true;
}

Interval* LinearScan::getUpperVectorInterval(unsigned varIndex)
{
    // TODO-Throughput: Consider creating a map from varIndex to upperVector interval.
    for (Interval& interval : intervals)
    {
        if (interval.isLocalVar)
        {
            continue;
        }
        noway_assert(interval.isUpperVector);
        if (interval.relatedInterval->getVarIndex(compiler) == varIndex)
        {
            return &interval;
        }
    }
    unreached();
}

void LinearScan::buildUpperVectorSaveRefPositions(GenTree* tree, LsraLocation currentLoc, regMaskTP fpCalleeKillSet)
{
    if (enregisterLocalVars && !VarSetOps::IsEmpty(compiler, largeVectorVars))
    {
        // We assume that the kill set includes at least some callee-trash registers, but
        // that it doesn't include any callee-save registers.
        assert((fpCalleeKillSet & RBM_FLT_CALLEE_TRASH) != RBM_NONE);
        assert((fpCalleeKillSet & RBM_FLT_CALLEE_SAVED) == RBM_NONE);

        // We only need to save the upper half of any large vector vars that are currently live.
        for (VarSetOps::EnumOp<VarSetOps::IntersectionOp> e(compiler, currentLiveVars, largeVectorVars); e.MoveNext();)
        {
            Interval* varInterval = getIntervalForLocalVar(e.Current());
            if (!varInterval->isPartiallySpilled)
            {
                Interval*    upperVectorInterval = getUpperVectorInterval(e.Current());
                RefPosition* pos =
                    newRefPosition(upperVectorInterval, currentLoc, RefTypeUpperVectorSave, tree, RBM_FLT_CALLEE_SAVED);
                varInterval->isPartiallySpilled = true;
#ifdef TARGET_XARCH
                pos->regOptional = true;
#endif
            }
        }
    }

    // For any non-lclVar intervals that are live at this point (i.e. in the DefList), we will also create
    // a RefTypeUpperVectorSave. For now these will all be spilled at this point, as we don't currently
    // have a mechanism to communicate any non-lclVar intervals that need to be restored.
    // TODO-CQ: We could consider adding such a mechanism, but it's unclear whether this rare
    // case of a large vector temp live across a call is worth the added complexity.
    for (RefInfoListNode* def = defList.Begin(); def != nullptr; def = def->next)
    {
        const GenTree* defNode = def->node;
        var_types      regType = defNode->GetType();

        if (regType == TYP_STRUCT)
        {
            assert(defNode->OperIs(GT_LCL_LOAD, GT_CALL));

            if (defNode->OperIs(GT_LCL_LOAD))
            {
                regType = defNode->AsLclLoad()->GetLcl()->GetRegisterType();
            }
            else
            {
                // TODO-MIKE-Review: This change came from main and it's not
                // clear how it works for unix-x64 ABI.
                regType = defNode->AsCall()->GetRetDesc()->GetRegType(0);
            }

            assert((regType != TYP_STRUCT) && (regType != TYP_UNDEF));
        }

        if (Compiler::varTypeNeedsPartialCalleeSave(regType))
        {
            // In the rare case where such an interval is live across nested calls, we don't need to insert another.
            if (def->ref->getInterval()->recentRefPosition->refType != RefTypeUpperVectorSave)
            {
                newRefPosition(def->ref->getInterval(), currentLoc, RefTypeUpperVectorSave, tree, RBM_FLT_CALLEE_SAVED);
            }
        }
    }
}

void LinearScan::buildUpperVectorRestoreRefPosition(Interval* lclVarInterval, LsraLocation currentLoc, GenTree* node)
{
    if (lclVarInterval->isPartiallySpilled)
    {
        unsigned     varIndex            = lclVarInterval->getVarIndex(compiler);
        Interval*    upperVectorInterval = getUpperVectorInterval(varIndex);
        RefPosition* pos = newRefPosition(upperVectorInterval, currentLoc, RefTypeUpperVectorRestore, node, RBM_NONE);
        lclVarInterval->isPartiallySpilled = false;
#ifdef TARGET_XARCH
        pos->regOptional = true;
#endif
    }
}
#endif // FEATURE_PARTIAL_SIMD_CALLEE_SAVE

#ifdef DEBUG
// Computes the number of registers defined by a node.
//
// For most nodes, this is simple:
// - Nodes that do not produce values (e.g. stores and other void-typed
//   nodes) and nodes that immediately use the registers they define
//   produce no registers
// - Nodes that are marked as defining N registers define N registers.
//
// For contained nodes, however, things are more complicated: for purposes
// of bookkeeping, a contained node is treated as producing the transitive
// closure of the registers produced by its sources.
unsigned LinearScan::ComputeOperandDstCount(GenTree* operand) const
{
    // GT_ARGPLACE is the only non-LIR node that is currently in the trees at this stage, though
    // note that it is not in the linear order. It seems best to check for !IsLIR() rather than
    // GT_ARGPLACE directly, since it's that characteristic that makes it irrelevant for this method.
    if (!operand->IsLIR())
    {
        return 0;
    }

    if (operand->isContained())
    {
        unsigned dstCount = 0;

        for (GenTree* op : operand->Operands())
        {
            dstCount += ComputeOperandDstCount(op);
        }

        return dstCount;
    }

    if (operand->IsUnusedValue())
    {
        // Operands that define an unused value do not produce any registers.
        return 0;
    }

    if (operand->IsValue())
    {
        // Operands that are values and are not contained consume all of their operands
        // and produce one or more registers.
        return GetRegisterDstCount(operand);
    }

    // This must be one of the operand types that are neither contained nor produce a value.
    // Stores and void-typed operands may be encountered when processing call nodes, which contain
    // pointers to argument setup stores.
    assert(operand->OperIsStore() || operand->IsPutArgStk() || operand->OperIsCompare() || operand->OperIs(GT_CMP) ||
           operand->TypeIs(TYP_VOID));

    return 0;
}

unsigned LinearScan::GetRegisterDstCount(GenTree* node) const
{
    assert(!node->isContained());
    return node->IsMultiRegNode() ? node->GetMultiRegCount(compiler) : node->IsValue();
}

// Computes the number of registers available as sources for a node.
// This is simply the sum of the number of registers produced by each
// operand to the node.
unsigned LinearScan::ComputeAvailableSrcCount(GenTree* node) const
{
    unsigned numSources = 0;

    for (GenTree* operand : node->Operands())
    {
        numSources += ComputeOperandDstCount(operand);
    }

    return numSources;
}
#endif // DEBUG

void LinearScan::buildRefPositionsForNode(GenTree* tree, LsraLocation currentLoc)
{
    assert(!tree->OperIs(GT_ARGPLACE));

    tree->ClearTempRegs();
    tree->ClearRegSpillSet();

#ifdef DEBUG
    if (compiler->verbose)
    {
        dumpDefList();
        compiler->gtDispLIRNode(tree);
    }
#endif

    if (tree->isContained())
    {
#ifdef TARGET_XARCH
        // On XArch we can have contained candidate lclVars if they are part of a RMW
        // address computation. In this case we need to check whether it is a last use.
        if (tree->OperIs(GT_LCL_LOAD) && ((tree->gtFlags & GTF_VAR_DEATH) != 0))
        {
            LclVarDsc* lcl = tree->AsLclLoad()->GetLcl();

            if (lcl->IsRegCandidate())
            {
                VarSetOps::RemoveElemD(compiler, currentLiveVars, lcl->GetLivenessBitIndex());
            }
        }
#else
        assert(!isCandidateLclVar(tree));
#endif
        JITDUMP("Contained\n");

        return;
    }

#ifdef TARGET_XARCH
    if (varTypeUsesFloatReg(tree->GetType()))
    {
        SetContainsAVXFlags();
    }
#endif

    clearBuildState();

    // If we are constraining the registers for allocation, we will modify all the RefPositions
    // we've built for this node after we've created them. In order to do that, we'll remember
    // the last RefPosition prior to those created for this node.
    INDEBUG(RefPositionIterator refPositionMark = refPositions.backPosition());
    // Currently "produce" below is unused, but need to strengthen an assert to check
    // if produce is as expected. See https://github.com/dotnet/runtime/issues/8678
    // int oldDefListCount = defList.Count();

    // We make a final determination about whether a LCL_LOAD is a candidate or contained
    // after liveness. In either case we don't build any uses or defs. Otherwise, this is a
    // load of a stack-based local into a register and we'll fall through to the general
    // local case below.
    if (!tree->OperIs(GT_LCL_LOAD) || !checkContainedOrCandidateLclVar(tree->AsLclLoad()))
    {
        BuildNode(tree);
    }

    // int newDefListCount = defList.Count();
    // unsigned produce = newDefListCount - oldDefListCount;

    assert(
        // RegOptional LCL_VARs may become contained.
        ((nodeDefCount == 0) && tree->isContained()) ||
        // A reg candidate store is not a value so GetRegisterDstCount returns 0, but it does define a register.
        ((nodeDefCount == 1) && tree->OperIs(GT_LCL_STORE) && tree->AsLclStore()->GetLcl()->IsRegCandidate()) ||
        // A reg candidate load is a value so GetRegisterDstCount returns 1, but it does not define a new register.
        ((nodeDefCount == 0) && tree->OperIs(GT_LCL_LOAD) && tree->AsLclLoad()->GetLcl()->IsRegCandidate()) ||
        (nodeDefCount == GetRegisterDstCount(tree)));

    assert((nodeUseCount == 0) || (ComputeAvailableSrcCount(tree) == nodeUseCount));

#ifdef DEBUG
    // If we are constraining registers, modify all the RefPositions we've just built to specify the
    // minimum reg count required.
    if ((getStressLimitRegs() != LSRA_LIMIT_NONE) || (getSelectionHeuristics() != LSRA_SELECT_DEFAULT))
    {
        BuildStressConstraints(tree, refPositionMark);
    }
#endif

    JITDUMP("\n");
}

#ifdef DEBUG
void LinearScan::BuildStressConstraints(GenTree* tree, RefPositionIterator refPositionMark)
{
    assert((getStressLimitRegs() != LSRA_LIMIT_NONE) || (getSelectionHeuristics() != LSRA_SELECT_DEFAULT));

    // The number of registers required for a tree node is the sum of
    //   { RefTypeUses } + { RefTypeDef for the node itself } + specialPutArgCount
    // This is the minimum set of registers that needs to be ensured in the candidate set of ref positions created.
    //
    // First, we count them.
    unsigned minRegCount = 0;

    RefPositionIterator iter = refPositionMark;
    for (iter++; iter != refPositions.end(); iter++)
    {
        RefPosition* rp = &(*iter);
        if (rp->isIntervalRef())
        {
            if ((rp->refType == RefTypeUse) || ((rp->refType == RefTypeDef) && !rp->getInterval()->isInternal))
            {
                minRegCount++;
            }
#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
            else if (rp->refType == RefTypeUpperVectorSave)
            {
                minRegCount++;
            }
#endif
            if (rp->getInterval()->isSpecialPutArg)
            {
                minRegCount++;
            }
        }
    }

    if (tree->OperIsPutArgSplit())
    {
        // While we have attempted to account for any "specialPutArg" defs above, we're only looking at RefPositions
        // created for this node. We must be defining at least one register in the PutArgSplit, so conservatively
        // add one less than the maximum number of registers args to 'minRegCount'.
        minRegCount += MAX_REG_ARG - 1;
    }
    for (refPositionMark++; refPositionMark != refPositions.end(); refPositionMark++)
    {
        RefPosition* rp                = &(*refPositionMark);
        unsigned     minRegCountForRef = minRegCount;
        if (RefTypeIsUse(rp->refType) && rp->delayRegFree)
        {
            // If delayRegFree, then Use will interfere with the destination of the consuming node.
            // Therefore, we also need add the kill set of the consuming node to minRegCount.
            //
            // For example consider the following IR on x86, where v01 and v02
            // are method args coming in ecx and edx respectively.
            //   GT_DIV(v01, v02)
            //
            // For GT_DIV, the minRegCount will be 3 without adding kill set of GT_DIV node.
            //
            // Assume further JitStressRegs=2, which would constrain candidates to callee trashable
            // regs { eax, ecx, edx } on use positions of v01 and v02.  LSRA allocates ecx for v01.
            // The use position of v02 cannot be allocated a reg since it is marked delay-reg free and
            // {eax,edx} are getting killed before the def of GT_DIV.  For this reason, minRegCount for
            // the use position of v02 also needs to take into account the kill set of its consuming node.
            if (regMaskTP killMask = getKillSetForNode(tree))
            {
                minRegCountForRef += genCountBits(killMask);
            }
        }
        else if ((rp->refType) == RefTypeDef && (rp->getInterval()->isSpecialPutArg))
        {
            minRegCountForRef++;
        }

        rp->minRegCandidateCount = minRegCountForRef;
        if (rp->IsActualRef() && doReverseCallerCallee())
        {
            Interval* interval       = rp->getInterval();
            regMaskTP oldAssignment  = rp->registerAssignment;
            regMaskTP calleeSaveMask = calleeSaveRegs(interval->registerType);
            rp->registerAssignment   = getConstrainedRegMask(oldAssignment, calleeSaveMask, minRegCountForRef);
            if ((rp->registerAssignment != oldAssignment) && (rp->refType == RefTypeUse) && !interval->isLocalVar)
            {
                checkConflictingDefUse(rp);
            }
        }
    }
}
#endif // DEBUG

// Handle lclVars that are live-in to the first block
//
// Prior to calling this method, 'currentLiveVars' must be set to the set of register
// candidate variables that are liveIn to the first block.
// For each register candidate that is live-in to the first block:
// - If it is a GC ref, or if compInitMem is set, a ZeroInit RefPosition will be created.
// - Otherwise, it will be marked as spilled, since it will not be assigned a register
//   on entry and will be loaded from memory on the undefined path.
//   Note that, when the compInitMem option is not set, we may encounter these on
//   paths that are protected by the same condition as an earlier def. However, since
//   we don't do the analysis to determine this - and couldn't rely on always identifying
//   such cases even if we tried - we must conservatively treat the undefined path as
//   being possible. This is a relatively rare case, so the introduced conservatism is
//   not expected to warrant the analysis required to determine the best placement of
//   an initialization.
//
void LinearScan::insertZeroInitRefPositions()
{
    assert(enregisterLocalVars);

    for (VarSetOps::Enumerator e(compiler, currentLiveVars); e.MoveNext();)
    {
        LclVarDsc* lcl = compiler->lvaGetDescByTrackedIndex(e.Current());
        assert(lcl->IsRegCandidate());

        if (lcl->IsParam())
        {
            continue;
        }

        JITDUMP("V%02u was live in to first block:", lcl->GetLclNum());

        Interval* interval = getIntervalForLocalVar(e.Current());
        if (compiler->info.compInitMem || varTypeIsGC(lcl->GetType()))
        {
            lcl->lvMustInit = true;

            // OSR will handle init of locals and promoted fields thereof
            if (compiler->lvaIsOSRLocal(lcl))
            {
                JITDUMP(" will be initialized by OSR\n");
                // setIntervalAsSpilled(interval);
                lcl->lvMustInit = false;
            }

            JITDUMP(" creating ZeroInit\n");
            RefPosition* pos = newRefPosition(interval, MinLocation, RefTypeZeroInit, nullptr /* theTreeNode */,
                                              allRegs(interval->registerType));
            pos->setRegOptional(true);
        }
        else
        {
            setIntervalAsSpilled(interval);
            JITDUMP(" marking as spilled\n");
        }
    }

    // We must also insert zero-inits for any finallyVars if they are refs or if compInitMem is true.
    if (compiler->lvaEnregEHVars && (compiler->compHndBBtabCount > 0))
    {
        for (VarSetOps::Enumerator e(compiler, finallyVars); e.MoveNext();)
        {
            LclVarDsc* varDsc = compiler->lvaGetDescByTrackedIndex(e.Current());

            if (!varDsc->IsParam() && varDsc->IsRegCandidate())
            {
                JITDUMP("V%02u is a finally var:", varDsc->GetLclNum());
                Interval* interval = getIntervalForLocalVar(e.Current());
                if (compiler->info.compInitMem || varTypeIsGC(varDsc->GetType()))
                {
                    if (interval->recentRefPosition == nullptr)
                    {
                        JITDUMP(" creating ZeroInit\n");
                        RefPosition* pos = newRefPosition(interval, MinLocation, RefTypeZeroInit,
                                                          nullptr /* theTreeNode */, allRegs(interval->registerType));
                        pos->setRegOptional(true);
                        varDsc->lvMustInit = true;
                    }
                    else
                    {
                        // We must only generate one entry RefPosition for each Interval. Since this is not
                        // a parameter, it can't be RefTypeParamDef, so it must be RefTypeZeroInit, which
                        // we must have generated for the live-in case above.
                        assert(interval->recentRefPosition->refType == RefTypeZeroInit);
                        JITDUMP(" already ZeroInited\n");
                    }
                }
            }
        }
    }
}

void LinearScan::AddLiveParamRegs(LclVarDsc* lcl)
{
    assert(lcl->IsRegParam());

    regMaskTP mask = RBM_NONE;

    for (unsigned i = 0, count = lcl->GetParamRegCount(); i < count; i++)
    {
        mask |= genRegMask(lcl->GetParamReg(i));
    }

    compiler->codeGen->paramRegState.intRegLiveIn |= mask & ~RBM_ALLFLOAT;
    compiler->codeGen->paramRegState.floatRegLiveIn |= mask & RBM_ALLFLOAT;
}

void LinearScan::buildIntervals()
{
    JITDUMP("\nbuildIntervals ========\n");

#ifdef DEBUG
    if (VERBOSE)
    {
        printf("\n-----------------\n");
        printf("LIVENESS:\n");
        printf("-----------------\n");
        for (BasicBlock* const block : compiler->Blocks())
        {
            printf(FMT_BB " use def in out\n", block->bbNum);
            dumpConvertedVarSet(compiler, block->bbVarUse);
            printf("\n");
            dumpConvertedVarSet(compiler, block->bbVarDef);
            printf("\n");
            dumpConvertedVarSet(compiler, block->bbLiveIn);
            printf("\n");
            dumpConvertedVarSet(compiler, block->bbLiveOut);
            printf("\n");
        }
    }
#endif // DEBUG

    identifyCandidates();

    // Figure out if we're going to use a frame pointer. We need to do this before building
    // the ref positions, because those objects will embed the frame register in various register masks
    // if the frame pointer is not reserved. If we decide to have a frame pointer, setFrameType() will
    // remove the frame pointer from the masks.
    setFrameType();

    BlockSet visited = setBlockSequence();

    DBEXEC(VERBOSE, TupleStyleDump(LSRA_DUMP_PRE));

    JITDUMP("\nbuildIntervals second part ========\n");

    currentLoc = 0;
    // Next, create ParamDef RefPositions for all the tracked parameters, in order of their varIndex.
    // Assign these RefPositions to the (nonexistent) BB0.
    curBBNum = 0;

    for (unsigned varIndex = 0; varIndex < compiler->lvaTrackedCount; varIndex++)
    {
        LclVarDsc* argDsc = compiler->lvaGetDescByTrackedIndex(varIndex);

        assert(argDsc->HasLiveness() && !argDsc->IsPromoted());

        if (!argDsc->IsParam() || (argDsc->GetRefCount() == 0))
        {
            continue;
        }

        if (argDsc->IsRegParam())
        {
            AddLiveParamRegs(argDsc);
        }

        if (argDsc->IsRegCandidate())
        {
            Interval*       interval = getIntervalForLocalVar(varIndex);
            const var_types regType  = argDsc->GetRegisterType();
            regMaskTP       mask     = allRegs(regType);

            if (argDsc->IsRegParam())
            {
                // Set this interval as currently assigned to that register
                regNumber inArgReg = argDsc->GetParamReg();
                assert(inArgReg < REG_COUNT);
                mask = genRegMask(inArgReg);
                assignPhysReg(inArgReg, interval);
                INDEBUG(registersToDump |= getRegMask(inArgReg, interval->registerType));
            }

            RefPosition* pos = newRefPosition(interval, MinLocation, RefTypeParamDef, nullptr, mask);
            pos->setRegOptional(true);
        }
    }

    // Now set up the reg state for the non-tracked args
    // (We do this here because we want to generate the ParamDef RefPositions in tracked
    // order, so that loop doesn't hit the non-tracked args)

    for (LclVarDsc* paramLcl : compiler->Params())
    {
        if (paramLcl->IsPromotedStruct())
        {
            for (LclVarDsc* fieldLcl : compiler->PromotedFields(paramLcl))
            {
                assert(fieldLcl->IsParam());

                if (!fieldLcl->HasLiveness() && fieldLcl->IsRegParam())
                {
                    AddLiveParamRegs(fieldLcl);
                }
            }
        }
        else
        {
            noway_assert(paramLcl->IsParam());

            if (!paramLcl->HasLiveness() && paramLcl->IsRegParam())
            {
                AddLiveParamRegs(paramLcl);
            }
        }
    }

    // If there is a secret stub param, it is also live in
    if (compiler->info.compPublishStubParam)
    {
        compiler->codeGen->paramRegState.intRegLiveIn |= RBM_SECRET_STUB_PARAM;
    }

    BasicBlock* prevBlock = nullptr;

    // Allocate currentLiveVars. We will set it to the current live-in at the entry to each block
    // (this will include the incoming args on the first block).
    currentLiveVars = VarSetOps::Alloc(compiler);

    VARSET_TP expUseSet = VarSetOps::UninitVal();
    VARSET_TP newLiveIn = VarSetOps::UninitVal();

    if (enregisterLocalVars)
    {
        expUseSet = VarSetOps::Alloc(compiler);
        newLiveIn = VarSetOps::Alloc(compiler);
    }

    BlockSetOps::ClearD(compiler, visited);
    BlockSetOps::AddElemD(compiler, visited, blockSequence[0]->bbNum);

    for (unsigned blockSeqIndex = 0, blockSeqCount = bbSeqCount; blockSeqIndex < blockSeqCount; blockSeqIndex++)
    {
        BasicBlock* block = blockSequence[blockSeqIndex];
        JITDUMP("\nNEW BLOCK " FMT_BB "\n", block->bbNum);
        curBBNum = block->bbNum;

        INDEBUG(bool predBlockIsAllocated = false);
        BasicBlock* predBlock = findPredBlockForLiveIn(block, prevBlock, visited DEBUGARG(&predBlockIsAllocated));
        if (predBlock != nullptr)
        {
            JITDUMP("\n\nSetting " FMT_BB " as the predecessor for determining incoming variable registers of " FMT_BB
                    "\n",
                    predBlock->bbNum, block->bbNum);
            assert(predBlock->bbNum <= bbNumMaxBeforeResolution);
            blockInfo[block->bbNum].predBBNum = predBlock->bbNum;
        }

        if (enregisterLocalVars)
        {
            VarSetOps::Intersection(compiler, currentLiveVars, registerCandidateVars, block->bbLiveIn);

            if (block == compiler->fgFirstBB)
            {
                insertZeroInitRefPositions();
                // The first real location is at 1; 0 is for the entry.
                currentLoc = 1;
            }

            // For blocks that don't have EHBoundaryIn, we need DummyDefs for cases where "predBlock" isn't
            // really a predecessor.
            // Note that it's possible to have uses of uninitialized variables, in which case even the first
            // block may require DummyDefs, which we are not currently adding - this means that these variables
            // will always be considered to be in memory on entry (and reloaded when the use is encountered).
            // TODO-CQ: Consider how best to tune this.  Currently, if we create DummyDefs for uninitialized
            // variables (which may actually be initialized along the dynamically executed paths, but not
            // on all static paths), we wind up with excessive liveranges for some of these variables.

            if (!blockInfo[block->bbNum].hasEHBoundaryIn)
            {
                // Any lclVars live-in on a non-EH boundary edge are resolution candidates.
                VarSetOps::UnionD(compiler, resolutionCandidateVars, currentLiveVars);

                if (block != compiler->fgFirstBB)
                {
                    if (predBlock == nullptr)
                    {
                        VarSetOps::Assign(compiler, newLiveIn, currentLiveVars);
                    }
                    else
                    {
                        VarSetOps::Diff(compiler, newLiveIn, currentLiveVars, predBlock->bbLiveOut);
                    }

                    if (compiler->compHndBBtabCount > 0)
                    {
                        // Don't create dummy defs for EH vars; we'll load them from the stack as/when needed.
                        VarSetOps::DiffD(compiler, newLiveIn, exceptVars);
                    }

                    // Create dummy def RefPositions

                    if (!VarSetOps::IsEmpty(compiler, newLiveIn))
                    {
                        // If we are using locations from a predecessor, we should never require DummyDefs.
                        assert(!predBlockIsAllocated);

                        JITDUMP("Creating dummy definitions\n");
                        for (VarSetOps::Enumerator e(compiler, newLiveIn); e.MoveNext();)
                        {
                            // Add a dummyDef for any candidate vars that are in the "newLiveIn" set.
                            LclVarDsc* varDsc = compiler->lvaGetDescByTrackedIndex(e.Current());
                            assert(varDsc->IsRegCandidate());
                            Interval*    interval = getIntervalForLocalVar(e.Current());
                            RefPosition* pos      = newRefPosition(interval, currentLoc, RefTypeDummyDef, nullptr,
                                                              allRegs(interval->registerType));
                            pos->setRegOptional(true);
                        }
                        JITDUMP("Finished creating dummy definitions\n\n");
                    }
                }
            }
        }

        // Add a dummy RefPosition to mark the block boundary.
        // Note that we do this AFTER adding the exposed uses above, because the
        // register positions for those exposed uses need to be recorded at
        // this point.

        RefPosition* pos = newBlockRefPosition(currentLoc);
        currentLoc += 2;
        JITDUMP("\n");

        if (firstColdLoc == MaxLocation)
        {
            if (block->isRunRarely())
            {
                firstColdLoc = currentLoc;
                JITDUMP("firstColdLoc = %d\n", firstColdLoc);
            }
        }
        else
        {
            // TODO: We'd like to assert the following but we don't currently ensure that only
            // "RunRarely" blocks are contiguous.
            // (The funclets will generally be last, but we don't follow layout order, so we
            // don't have to preserve that in the block sequence.)
            // assert(block->isRunRarely());
        }

        // For frame poisoning we generate code into scratch BB right after prolog since
        // otherwise the prolog might become too large. In this case we will put the poison immediate
        // into the scratch register, so it will be killed here.
        if (compiler->compShouldPoisonFrame() && compiler->fgFirstBBisScratch() && block == compiler->fgFirstBB)
        {
            newRegKillRefPositions(genRegMask(REG_SCRATCH), currentLoc + 1);
            currentLoc += 2;
        }

        for (GenTree* node : LIR::AsRange(block))
        {
#ifdef DEBUG
            node->gtSeqNum = currentLoc;
#endif

            buildRefPositionsForNode(node, currentLoc);

#ifdef DEBUG
            if (currentLoc > maxNodeLocation)
            {
                maxNodeLocation = currentLoc;
            }
#endif

            // We increment the location of each tree node by 2 so that the node definition,
            // if any, is at a new location and doesn't interfere with the uses.
            // For multi-reg local stores, the 'BuildStoreLclVarMultiReg' method will further
            // increment the location by 2 for each destination register beyond the first.
            currentLoc += 2;
        }

#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
        // At the end of each block, create upperVectorRestores for any largeVectorVars that may be
        // partiallySpilled (during the build phase all intervals will be marked isPartiallySpilled if
        // they *may) be partially spilled at any point.
        if (enregisterLocalVars)
        {
            for (VarSetOps::Enumerator e(compiler, largeVectorVars); e.MoveNext();)
            {
                Interval* lclVarInterval = getIntervalForLocalVar(e.Current());
                buildUpperVectorRestoreRefPosition(lclVarInterval, currentLoc, nullptr);
            }
        }
#endif // FEATURE_PARTIAL_SIMD_CALLEE_SAVE

        BlockSetOps::AddElemD(compiler, visited, block->bbNum);

#ifdef DEBUG
        if (!defList.IsEmpty())
        {
            dumpDefList();
            assert(!"Found unused reg defs at the end of block");
        }
#endif

        if (enregisterLocalVars)
        {
            // Insert exposed uses for a lclVar that is live-out of 'block' but not live-in to the
            // next block, or any unvisited successors.
            // This will address lclVars that are live on a backedge, as well as those that are kept
            // live at a GT_JMP.
            //
            // Blocks ending with "jmp method" are marked as BBJ_HAS_JMP,
            // and jmp call is represented using GT_JMP node which is a leaf node.
            // Liveness phase keeps all the arguments of the method live till the end of
            // block by adding them to liveout set of the block containing GT_JMP.
            //
            // The target of a GT_JMP implicitly uses all the current method arguments, however
            // there are no actual references to them.  This can cause LSRA to assert, because
            // the variables are live but it sees no references.  In order to correctly model the
            // liveness of these arguments, we add dummy exposed uses, in the same manner as for
            // backward branches.  This will happen automatically via expUseSet.
            //
            // Note that a block ending with GT_JMP has no successors and hence the variables
            // for which dummy use ref positions are added are arguments of the method.

            VarSetOps::Intersection(compiler, expUseSet, block->bbLiveOut, registerCandidateVars);

            if (blockSeqIndex + 1 < blockSeqCount)
            {
                VarSetOps::DiffD(compiler, expUseSet, blockSequence[blockSeqIndex + 1]->bbLiveIn);
            }

            for (BasicBlock* succ : block->GetAllSuccs(compiler))
            {
                if (VarSetOps::IsEmpty(compiler, expUseSet))
                {
                    break;
                }

                if (!BlockSetOps::IsMember(compiler, visited, succ->bbNum))
                {
                    VarSetOps::DiffD(compiler, expUseSet, succ->bbLiveIn);
                }
            }

            JITDUMP("Exposed uses:");
            for (VarSetOps::Enumerator e(compiler, expUseSet); e.MoveNext();)
            {
                Interval*    interval = getIntervalForLocalVar(e.Current());
                RefPosition* pos =
                    newRefPosition(interval, currentLoc, RefTypeExpUse, nullptr, allRegs(interval->registerType));
                pos->setRegOptional(true);
                JITDUMP(" " FMT_LCL, compiler->lvaGetDescByTrackedIndex(e.Current())->GetLclNum());
            }
            JITDUMP("\n");

            // Clear the "last use" flag on any vars that are live-out from this block.

            for (VarSetOps::Enumerator e(compiler, block->bbLiveOut); e.MoveNext();)
            {
                if (Interval* interval = HasLclInterval(e.Current()))
                {
                    RefPosition* const lastRP = interval->lastRefPosition;
                    // We should be able to assert that lastRP is non-null if it is live-out,
                    // but sometimes liveness lies.
                    if ((lastRP != nullptr) && (lastRP->bbNum == block->bbNum))
                    {
                        lastRP->lastUse = false;
                    }
                }
            }

#ifdef DEBUG
            checkLastUses(block);

            if (VERBOSE)
            {
                printf("use: ");
                dumpConvertedVarSet(compiler, block->bbVarUse);
                printf("\ndef: ");
                dumpConvertedVarSet(compiler, block->bbVarDef);
                printf("\n");
            }
#endif // DEBUG
        }

        prevBlock = block;
    }

    if (enregisterLocalVars)
    {
        if (compiler->lvaKeepAliveAndReportThis())
        {
            // If we need to KeepAliveAndReportThis, add a dummy exposed use of it at the end

            LclVarDsc* lcl = compiler->lvaGetDesc(compiler->info.GetThisParamLclNum());

            if (lcl->IsRegCandidate())
            {
                JITDUMP("Adding exposed use of this, for lvaKeepAliveAndReportThis\n");
                Interval*    interval = getIntervalForLocalVar(lcl->GetLivenessBitIndex());
                RefPosition* pos =
                    newRefPosition(interval, currentLoc, RefTypeExpUse, nullptr, allRegs(interval->registerType));
                pos->setRegOptional(true);
            }
        }
        // Adjust heuristics for writeThru intervals.
        if (compiler->compHndBBtabCount > 0)
        {
            for (VarSetOps::Enumerator e(compiler, exceptVars); e.MoveNext();)
            {
                LclVarDsc* varDsc   = compiler->lvaGetDescByTrackedIndex(e.Current());
                Interval*  interval = getIntervalForLocalVar(e.Current());
                assert(interval->isWriteThru);
                BasicBlock::weight_t weight = varDsc->lvRefCntWtd();

                // We'd like to only allocate registers for EH vars that have enough uses
                // to compensate for the additional registers being live (and for the possibility
                // that we may have to insert an additional copy).
                // However, we don't currently have that information available. Instead, we'll
                // aggressively assume that these vars are defined once, at their first RefPosition.
                //
                RefPosition* firstRefPosition = interval->firstRefPosition;

                // Incoming reg args are given an initial weight of 2 * BB_UNITY_WEIGHT
                // (see lvaComputeRefCounts(); this may be reviewed/changed in future).
                //
                BasicBlock::weight_t initialWeight = (firstRefPosition->refType == RefTypeParamDef)
                                                         ? (2 * BB_UNITY_WEIGHT)
                                                         : blockInfo[firstRefPosition->bbNum].weight;
                weight -= initialWeight;

                // If the remaining weight is less than the initial weight, we'd like to allocate it only
                // opportunistically, but we don't currently have a mechanism to do so.
                // For now, we'll just avoid using callee-save registers if the weight is too low.
                if (interval->preferCalleeSave)
                {
                    // The benefit of a callee-save register isn't as high as it would be for a normal arg.
                    // We'll have at least the cost of saving & restoring the callee-save register,
                    // so we won't break even until we have at least 4 * BB_UNITY_WEIGHT.
                    // Given that we also don't have a good way to tell whether the variable is live
                    // across a call in the non-EH code, we'll be extra conservative about this.
                    // Note that for writeThru intervals we don't update the preferences to be only callee-save.
                    unsigned calleeSaveCount =
                        (varTypeUsesFloatReg(interval->registerType)) ? CNT_CALLEE_SAVED_FLOAT : CNT_CALLEE_ENREG;
                    if ((weight <= (BB_UNITY_WEIGHT * 7)) || varDsc->lvVarIndex >= calleeSaveCount)
                    {
                        // If this is relatively low weight, don't prefer callee-save at all.
                        interval->preferCalleeSave = false;
                    }
                    else
                    {
                        // In other cases, we'll add in the callee-save regs to the preferences, but not clear
                        // the non-callee-save regs . We also handle this case specially in tryAllocateFreeReg().
                        interval->registerPreferences |= calleeSaveRegs(interval->registerType);
                    }
                }
            }
        }

#ifdef DEBUG
        if (getLsraExtendLifeTimes())
        {
            for (LclVarDsc* lcl : compiler->LivenessLocals())
            {
                if (lcl->IsRegCandidate())
                {
                    JITDUMP("Adding exposed use of V%02u for LsraExtendLifetimes\n", lcl->GetLclNum());
                    Interval*    interval = getIntervalForLocalVar(lcl->GetLivenessBitIndex());
                    RefPosition* pos =
                        newRefPosition(interval, currentLoc, RefTypeExpUse, nullptr, allRegs(interval->registerType));
                    pos->setRegOptional(true);
                }
            }
        }
#endif // DEBUG
    }

    // If the last block has successors, create a RefTypeBB to record what's live

    if (prevBlock->NumSucc(compiler) > 0)
    {
        newBlockRefPosition(currentLoc);
    }

#ifdef DEBUG
    // Make sure we don't have any blocks that were not visited
    for (BasicBlock* const block : compiler->Blocks())
    {
        assert(BlockSetOps::IsMember(compiler, visited, block->bbNum));
    }

    if (VERBOSE)
    {
        lsraDumpIntervals("BEFORE VALIDATING INTERVALS");
        dumpRefPositions("BEFORE VALIDATING INTERVALS");
    }
    validateIntervals();

#endif // DEBUG
}

//------------------------------------------------------------------------
// findPredBlockForLiveIn: Determine which block should be used for the register locations of the live-in variables.
//
// Arguments:
//    block                 - The block for which we're selecting a predecesor.
//    prevBlock             - The previous block in in allocation order.
//    pPredBlockIsAllocated - A debug-only argument that indicates whether any of the predecessors have been seen
//                            in allocation order.
//
// Return Value:
//    The selected predecessor.
//
// Assumptions:
//    in DEBUG, caller initializes *pPredBlockIsAllocated to false, and it will be set to true if the block
//    returned is in fact a predecessor.
//
// Notes:
//    This will select a predecessor based on the heuristics obtained by getLsraBlockBoundaryLocations(), which can be
//    one of:
//      LSRA_BLOCK_BOUNDARY_PRED    - Use the register locations of a predecessor block (default)
//      LSRA_BLOCK_BOUNDARY_LAYOUT  - Use the register locations of the previous block in layout order.
//                                    This is the only case where this actually returns a different block.
//      LSRA_BLOCK_BOUNDARY_ROTATE  - Rotate the register locations from a predecessor.
//                                    For this case, the block returned is the same as for LSRA_BLOCK_BOUNDARY_PRED, but
//                                    the register locations will be "rotated" to stress the resolution and allocation
//                                    code.

BasicBlock* LinearScan::findPredBlockForLiveIn(BasicBlock* block,
                                               BasicBlock* prevBlock,
                                               BlockSet visited DEBUGARG(bool* pPredBlockIsAllocated))
{
    BasicBlock* predBlock = nullptr;
    assert(*pPredBlockIsAllocated == false);

    // Blocks with exception flow on entry use no predecessor blocks, as all incoming vars
    // are on the stack.
    if (blockInfo[block->bbNum].hasEHBoundaryIn)
    {
        JITDUMP("\n\nIncoming EH boundary; ");
        return nullptr;
    }

    if (block == compiler->fgFirstBB)
    {
        return nullptr;
    }

    if (block->bbPreds == nullptr)
    {
        assert((block != compiler->fgFirstBB) || (prevBlock != nullptr));
        JITDUMP("\n\nNo predecessor; ");

        // Some throw blocks do not have predecessor. For such blocks, we want to return the fact
        // that predecessor is indeed null instead of returning the prevBlock. Returning prevBlock
        // will be wrong, because LSRA would think that the variable is live in registers based on
        // the lexical flow, but that won't be true according to the control flow.
        // Example:
        //
        // IG05:
        //      ...         ; V01 is in 'rdi'
        //      JNE IG07
        //      ...
        // IG06:
        //      ...
        //      ...         ; V01 is in 'rbx'
        //      JMP IG08
        // IG07:
        //      ...         ; LSRA thinks V01 is in 'rbx' if IG06 is set as previous block of IG07.
        //      ....
        //      CALL CORINFO_HELP_RNGCHKFAIL
        //      ...
        // IG08:
        //      ...
        //      ...
        if (block->bbJumpKind == BBJ_THROW)
        {
            JITDUMP(" - throw block; ");
            return nullptr;
        }

        // We may have unreachable blocks, due to optimization.
        // We don't want to set the predecessor as null in this case, since that will result in
        // unnecessary DummyDefs, and possibly result in inconsistencies requiring resolution
        // (since these unreachable blocks can have reachable successors).
        return prevBlock;
    }

#ifdef DEBUG
    if (getLsraBlockBoundaryLocations() == LSRA_BLOCK_BOUNDARY_LAYOUT)
    {
        if (prevBlock != nullptr)
        {
            predBlock = prevBlock;
        }
    }
    else
#endif // DEBUG
    {
        predBlock = block->GetUniquePred(compiler);
        if (predBlock != nullptr)
        {
            // We should already have returned null if this block has a single incoming EH boundary edge.
            assert(!predBlock->hasEHBoundaryOut());
            if (BlockSetOps::IsMember(compiler, visited, predBlock->bbNum))
            {
                if (predBlock->bbJumpKind == BBJ_COND)
                {
                    // Special handling to improve matching on backedges.
                    BasicBlock* otherBlock = (block == predBlock->bbNext) ? predBlock->bbJumpDest : predBlock->bbNext;
                    noway_assert(otherBlock != nullptr);
                    if (BlockSetOps::IsMember(compiler, visited, otherBlock->bbNum) &&
                        !blockInfo[otherBlock->bbNum].hasEHBoundaryIn)
                    {
                        // This is the case when we have a conditional branch where one target has already
                        // been visited.  It would be best to use the same incoming regs as that block,
                        // so that we have less likelihood of having to move registers.
                        // For example, in determining the block to use for the starting register locations for
                        // "block" in the following example, we'd like to use the same predecessor for "block"
                        // as for "otherBlock", so that both successors of predBlock have the same locations, reducing
                        // the likelihood of needing a split block on a backedge:
                        //
                        //   otherPred
                        //       |
                        //   otherBlock <-+
                        //     . . .      |
                        //                |
                        //   predBlock----+
                        //       |
                        //     block
                        //
                        if (blockInfo[otherBlock->bbNum].hasEHBoundaryIn)
                        {
                            return nullptr;
                        }
                        else
                        {
                            for (BasicBlock* const otherPred : otherBlock->PredBlocks())
                            {
                                if (otherPred->bbNum == blockInfo[otherBlock->bbNum].predBBNum)
                                {
                                    predBlock = otherPred;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
            else
            {
                predBlock = nullptr;
            }
        }
        else
        {
            for (BasicBlock* const candidatePredBlock : block->PredBlocks())
            {
                if (BlockSetOps::IsMember(compiler, visited, candidatePredBlock->bbNum))
                {
                    if ((predBlock == nullptr) || (predBlock->bbWeight < candidatePredBlock->bbWeight))
                    {
                        predBlock = candidatePredBlock;
                        INDEBUG(*pPredBlockIsAllocated = true;)
                    }
                }
            }
        }
        if (predBlock == nullptr)
        {
            predBlock = prevBlock;
            assert(predBlock != nullptr);
            JITDUMP("\n\nNo allocated predecessor; ");
        }
    }
    return predBlock;
}

#ifdef DEBUG
void LinearScan::validateIntervals()
{
    if (enregisterLocalVars)
    {
        for (unsigned i = 0; i < compiler->lvaTrackedCount; i++)
        {
            LclVarDsc* lcl = compiler->lvaGetDescByTrackedIndex(i);
            if (!lcl->IsRegCandidate())
            {
                continue;
            }
            Interval* interval = getIntervalForLocalVar(i);
            assert(interval->getLocalVar(compiler) == lcl);
            bool     defined      = false;
            unsigned lastUseBBNum = 0;
            JITDUMP("-----------------\n");
            for (RefPosition* ref = interval->firstRefPosition; ref != nullptr; ref = ref->nextRefPosition)
            {
                if (VERBOSE)
                {
                    ref->dump(this);
                }
                RefType refType = ref->refType;
                if (!defined && RefTypeIsUse(refType) && (lastUseBBNum == ref->bbNum))
                {
                    if (!ref->lastUse)
                    {
                        if (compiler->info.compMethodName != nullptr)
                        {
                            JITDUMP("%s: ", compiler->info.compMethodName);
                        }
                        JITDUMP("LocalVar V%02u: undefined use at %u\n", lcl->GetLclNum(), ref->nodeLocation);
                        assert(false);
                    }
                }

                // For single-def intervals, the only the first refposition should be a RefTypeDef
                if (interval->isSingleDef && RefTypeIsDef(refType))
                {
                    assert(ref == interval->firstRefPosition);
                }

                // Note that there can be multiple last uses if they are on disjoint paths,
                // so we can't really check the lastUse flag
                if (ref->lastUse)
                {
                    defined      = false;
                    lastUseBBNum = ref->bbNum;
                }
                if (RefTypeIsDef(refType))
                {
                    defined = true;
                }
            }
        }
    }
}
#endif // DEBUG

#if defined(TARGET_XARCH) || defined(FEATURE_HW_INTRINSICS)
// Set a  preference relationship between the given Interval and a Use RefPosition.
//
// This is called when we would like tgtPrefUse and this def to get the same register.
// This is only desirable if the use is a last use, which it is if it is a non-local,
// *or* if it is a lastUse.
// Note that we don't yet have valid lastUse information in the RefPositions that we're building
// (every RefPosition is set as a lastUse until we encounter a new use), so we have to rely on the treeNode.
// This may be called for multiple uses, in which case 'interval' will only get preferenced at most
// to the first one (if it didn't already have a 'relatedInterval'.
//
void setTgtPref(Interval* interval, RefPosition* tgtPrefUse)
{
    if (tgtPrefUse != nullptr)
    {
        Interval* useInterval = tgtPrefUse->getInterval();
        if (!useInterval->isLocalVar || (tgtPrefUse->treeNode == nullptr) ||
            ((tgtPrefUse->treeNode->gtFlags & GTF_VAR_DEATH) != 0))
        {
            // Set the use interval as related to the interval we're defining.
            useInterval->assignRelatedIntervalIfUnassigned(interval);
        }
    }
}
#endif // TARGET_XARCH || FEATURE_HW_INTRINSICS

RefPosition* LinearScan::BuildDef(GenTree* node, regMaskTP regCandidates)
{
    assert(!node->isContained());
    assert(!node->IsMultiRegNode());

    return BuildDef(node, getDefType(node), regCandidates, 0);
}

RefPosition* LinearScan::BuildDef(GenTree* node, var_types regType, regMaskTP regCandidates, unsigned regIndex)
{
    INDEBUG(nodeDefCount++);

    if (regCandidates != RBM_NONE)
    {
        // TODO-MIKE-Cleanup: This ignores regIndex...
        assert((node->GetRegNum() == REG_NA) || (regCandidates == genRegMask(node->GetRegNum(regIndex))));
    }

    if (varTypeUsesFloatReg(regType))
    {
        compiler->compFloatingPointUsed = true;
    }

    Interval* interval = newInterval(regType);

    if (node->GetRegNum() != REG_NA)
    {
        if (!node->IsMultiRegNode() || (regIndex == 0))
        {
            assert((regCandidates == RBM_NONE) || (regCandidates == genRegMask(node->GetRegNum())));
            regCandidates = genRegMask(node->GetRegNum());
        }
        else
        {
            assert(isSingleRegister(regCandidates));
        }
    }
#ifdef TARGET_X86
    // TODO-MIKE-Review: This is dubious, only stores need this and stores aren't defs.
    // SETCC also needs byte regs but it already provides suitable regCandidates. So
    // what the crap is this code doing here?!?
    // This also ignores regIndex but presumably this doesn't matter, on x86 the only
    // multireg case already uses byte regs - call returns.
    else if (varTypeIsByte(node->GetType()))
    {
        if (regCandidates == RBM_NONE)
        {
            regCandidates = allRegs(TYP_INT);
        }

        regCandidates &= ~RBM_NON_BYTE_REGS;
        assert(regCandidates != RBM_NONE);
    }
#endif // TARGET_X86

    if (pendingDelayFree)
    {
        interval->hasInterferingUses = true;
        // pendingDelayFree = false;
    }

    RefPosition* defRefPosition = newRefPosition(interval, currentLoc + 1, RefTypeDef, node, regCandidates, regIndex);

    if (node->IsUnusedValue())
    {
        defRefPosition->isLocalDefUse = true;
        defRefPosition->lastUse       = true;
    }
    else
    {
        defList.Add(defRefPosition, node, compiler);
    }

#if defined(TARGET_XARCH) || defined(FEATURE_HW_INTRINSICS)
    setTgtPref(interval, tgtPrefUse);
    setTgtPref(interval, tgtPrefUse2);
#endif

#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
    assert(!interval->isPartiallySpilled);
#endif

    return defRefPosition;
}

void LinearScan::BuildKills(GenTree* tree, regMaskTP killMask)
{
    assert(killMask == getKillSetForNode(tree));

    // Call this even when killMask is RBM_NONE, as we have to check for some special cases
    buildKillPositionsForNode(tree, currentLoc + 1, killMask);

#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
    if (killMask != RBM_NONE)
    {
        // Build RefPositions to account for the fact that, even in a callee-save register, the upper half of any large
        // vector will be killed by a call.
        // We actually need to find any calls that kill the upper-half of the callee-save vector registers.
        // But we will use as a proxy any node that kills floating point registers.
        // (Note that some calls are masquerading as other nodes at this point so we can't just check for calls.)
        // We call this unconditionally for such nodes, as we will create RefPositions for any large vector tree temps
        // even if 'enregisterLocalVars' is false, or 'liveLargeVectors' is empty, though currently the allocation
        // phase will fully (rather than partially) spill those, so we don't need to build the UpperVectorRestore
        // RefPositions in that case.
        // This must be done after the kills, so that we know which large vectors are still live.
        //
        if ((killMask & RBM_FLT_CALLEE_TRASH) != RBM_NONE)
        {
            buildUpperVectorSaveRefPositions(tree, currentLoc + 1, killMask);
        }
    }
#endif // FEATURE_PARTIAL_SIMD_CALLEE_SAVE
}

RefPosition* LinearScan::BuildUse(GenTree* operand, regMaskTP candidates, int regIndex)
{
    assert(!operand->isContained());

    INDEBUG(nodeUseCount++);

    Interval* interval;
    bool      regOptional = operand->IsRegOptional();

    if (IsRegCandidateLclLoad(operand))
    {
        interval = getIntervalForLocalVarNode(operand->AsLclLoad());

        // We have only approximate last-use information at this point.  This is because the
        // execution order doesn't actually reflect the true order in which the localVars
        // are referenced - but the order of the RefPositions will, so we recompute it after
        // RefPositions are built.
        // Use the old value for setting currentLiveVars - note that we do this with the
        // not-quite-correct setting of lastUse.  However, this is OK because
        // 1) this is only for preferencing, which doesn't require strict correctness, and
        // 2) the cases where these out-of-order uses occur should not overlap a kill.
        // TODO-Throughput: clean this up once we have the execution order correct.  At that point
        // we can update currentLiveVars at the same place that we create the RefPosition.
        if ((operand->gtFlags & GTF_VAR_DEATH) != 0)
        {
            VarSetOps::RemoveElemD(compiler, currentLiveVars, interval->getVarIndex(compiler));
        }

#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
        buildUpperVectorRestoreRefPosition(interval, currentLoc, operand);
#endif
    }
    else if (operand->IsMultiRegLclVar())
    {
        LclVarDsc* lcl      = operand->AsLclLoad()->GetLcl();
        LclVarDsc* fieldLcl = compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(regIndex));

        interval = getIntervalForLocalVar(fieldLcl->GetLivenessBitIndex());

        if (operand->AsLclLoad()->IsLastUse(regIndex))
        {
            VarSetOps::RemoveElemD(compiler, currentLiveVars, fieldLcl->GetLivenessBitIndex());
        }

#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
        buildUpperVectorRestoreRefPosition(interval, currentLoc, operand);
#endif
    }
    else
    {
        RefPosition* ref = defList.Remove(operand, regIndex);
        assert(ref->multiRegIdx == regIndex);
        interval = ref->getInterval();
        operand  = nullptr;
    }

    RefPosition* useRefPos = newRefPosition(interval, currentLoc, RefTypeUse, operand, candidates, regIndex);
    useRefPos->setRegOptional(regOptional);
    return useRefPos;
}

void LinearScan::BuildAddrUses(GenTree* addr, regMaskTP candidates)
{
    if (!addr->isContained())
    {
        BuildUse(addr, candidates);
    }
    else if (GenTreeAddrMode* addrMode = addr->IsAddrMode())
    {
        BuildAddrModeUses(addr->IsAddrMode(), candidates);
    }
}

unsigned LinearScan::BuildAddrModeUses(GenTreeAddrMode* addrMode, regMaskTP candidates)
{
    unsigned useCount = 0;

    if (GenTree* base = addrMode->GetBase())
    {
        BuildUse(base, candidates);
        useCount++;
    }

    if (GenTree* index = addrMode->GetIndex())
    {
        BuildUse(index, candidates);
        useCount++;
    }

    return useCount;
}

#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
void LinearScan::BuildDelayFreeUse(GenTree* op, GenTree* rmwNode, regMaskTP candidates)
{
    assert(!op->isContained());

    RefPosition* use = BuildUse(op, candidates);

    Interval* rmwInterval  = nullptr;
    bool      rmwIsLastUse = false;

    if ((rmwNode != nullptr) && IsRegCandidateLclLoad(rmwNode))
    {
        GenTreeLclLoad* load = rmwNode->AsLclLoad();

        rmwInterval = getIntervalForLocalVarNode(load);
        assert(!load->IsMultiReg());
        rmwIsLastUse = load->IsLastUse(0);
    }

    if ((use->getInterval() != rmwInterval) || (!rmwIsLastUse && !use->lastUse))
    {
        setDelayFree(use);
    }
}
#endif // defined(TARGET_XARCH) || defined(TARGET_ARM64)

void LinearScan::setDelayFree(RefPosition* use)
{
    use->delayRegFree = true;
    pendingDelayFree  = true;
}

void LinearScan::BuildStoreLclVarDef(GenTreeLclStore* store, LclVarDsc* lcl, RefPosition* singleUseRef, unsigned index)
{
    assert(lcl->lvTracked);

    Interval* varDefInterval = getIntervalForLocalVar(lcl->lvVarIndex);

    // TODO-MIKE-Review: Use of GTF_VAR_DEATH on multireg nodes is dubious...
    if ((store->gtFlags & GTF_VAR_DEATH) == 0)
    {
        VarSetOps::AddElemD(compiler, currentLiveVars, lcl->lvVarIndex);
    }

    if (singleUseRef != nullptr)
    {
        Interval* srcInterval = singleUseRef->getInterval();

        if (srcInterval->relatedInterval == nullptr)
        {
            // Preference the source to the dest, unless this is a non-last-use localVar.
            // Note that the last-use info is not correct, but it is a better approximation than preferencing
            // the source to the dest, if the source's lifetime extends beyond the dest.
            if (!srcInterval->isLocalVar || ((singleUseRef->treeNode->gtFlags & GTF_VAR_DEATH) != 0))
            {
                srcInterval->assignRelatedInterval(varDefInterval);
            }
        }
        else if (!srcInterval->isLocalVar)
        {
            // Preference the source to dest, if src is not a local var.
            srcInterval->assignRelatedInterval(varDefInterval);
        }
    }

    regMaskTP defCandidates = RBM_NONE;
    var_types type          = lcl->GetRegisterType();

#ifdef TARGET_X86
    defCandidates = varTypeIsByte(type) ? allByteRegs() : allRegs(type);
#else
    defCandidates = allRegs(type);
#endif

    RefPosition* def = newRefPosition(varDefInterval, currentLoc + 1, RefTypeDef, store, defCandidates, index);

    INDEBUG(nodeDefCount++);

    if (varDefInterval->isWriteThru)
    {
        // We always make write-thru defs reg-optional, as we can store them if they don't
        // get a register.
        def->regOptional = true;
    }

#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
    if (Compiler::varTypeNeedsPartialCalleeSave(varDefInterval->registerType))
    {
        varDefInterval->isPartiallySpilled = false;
    }
#endif
}

void LinearScan::BuildStoreLclVarMultiReg(GenTreeLclStore* store)
{
    assert(store->IsMultiReg());

    LclVarDsc* lcl = store->GetLcl();
    assert(lcl->IsIndependentPromoted());
    unsigned regCount = lcl->GetPromotedFieldCount();

    GenTree* src = store->GetValue();
    assert(src->IsMultiRegNode());
    assert(regCount == src->GetMultiRegCount(compiler));

    // For multi-reg local stores of multi-reg sources, the code generator will read each source
    // register, and then move it, if needed, to the destination register. These nodes have
    // 2*N locations where N is the number of registers, so that the liveness can
    // be reflected accordingly.

    for (unsigned i = 0; i < regCount; ++i)
    {
        LclVarDsc* fieldLcl = compiler->lvaGetDesc(lcl->GetPromotedFieldLclNum(i));
        assert(fieldLcl->IsRegCandidate());

        regMaskTP srcCandidates = RBM_NONE;
#ifdef TARGET_X86
        if (varTypeIsByte(fieldLcl->GetType()))
        {
            srcCandidates = allByteRegs();
        }
#endif
        BuildUse(src, srcCandidates, i);
        BuildStoreLclVarDef(store, fieldLcl, nullptr, i);

        if (i < (regCount - 1))
        {
            currentLoc += 2;
        }
    }
}

void LinearScan::BuildLclStore(GenTreeLclStore* store)
{
    if (IsCandidateLclVarMultiReg(store))
    {
        BuildStoreLclVarMultiReg(store);

        return;
    }

    LclVarDsc* lcl = store->GetLcl();
    GenTree*   src = store->GetValue();

    if (store->TypeIs(TYP_STRUCT) && !src->IsCall())
    {
        ClassLayout*    layout = lcl->GetLayout();
        StructStoreKind kind   = GetStructStoreKind(true, layout, src);
        BuildStructStore(store, kind, layout);

        return;
    }

    BuildLclStoreCommon(store);
}

void LinearScan::BuildLclStoreFld(GenTreeLclStoreFld* store)
{
    if (store->TypeIs(TYP_STRUCT))
    {
        ClassLayout*    layout = store->GetLayout(compiler);
        StructStoreKind kind   = GetStructStoreKind(true, layout, store->GetValue());
        BuildStructStore(store, kind, layout);

        return;
    }

    BuildLclStoreCommon(store);
}

void LinearScan::BuildLclStoreCommon(GenTreeLclVarCommon* store)
{
    assert(store->OperIs(GT_LCL_STORE, GT_LCL_STORE_FLD));

    GenTree* src = store->GetOp(0);

#ifdef FEATURE_SIMD
    if (store->TypeIs(TYP_SIMD12))
    {
        if (src->isContained() && src->OperIs(GT_IND_LOAD, GT_LCL_LOAD_FLD, GT_LCL_LOAD))
        {
            BuildInternalIntDef(store);

            if (src->OperIs(GT_IND_LOAD))
            {
                BuildAddrUses(src->AsIndLoad()->GetAddr());
            }

            BuildInternalUses();

            return;
        }

        if (!src->OperIs(GT_CNS_INT))
        {
            // Need an additional register to extract upper 4 bytes of Vector3.
            BuildInternalFloatDef(store, allSIMDRegs());
        }
    }
#endif

    LclVarDsc*   lcl          = store->GetLcl();
    RefPosition* singleUseRef = nullptr;

    if (src->IsMultiRegNode())
    {
        for (unsigned i = 0, count = src->GetMultiRegCount(compiler); i < count; ++i)
        {
            BuildUse(src, RBM_NONE, i);
        }

#ifdef TARGET_X86
        if (lcl->IsRegCandidate() && src->IsCall() && src->TypeIs(TYP_SIMD8))
        {
            BuildInternalFloatDef(store, allSIMDRegs());
            setInternalRegsDelayFree = true;
        }
#endif
    }
#ifndef TARGET_64BIT
    else if (varTypeIsLong(src->GetType()))
    {
        // GT_MUL_LONG is handled by the IsMultiRegNode case above.
        assert(src->OperIs(GT_LONG));
        assert(src->isContained());
        assert(!src->AsOp()->GetOp(0)->isContained() && !src->AsOp()->GetOp(1)->isContained());

        BuildUse(src->AsOp()->GetOp(0));
        BuildUse(src->AsOp()->GetOp(1));
    }
#endif
    else if (src->isContained())
    {
#ifdef TARGET_XARCH
        if (src->OperIs(GT_BITCAST))
        {
            GenTree*  bitCastSrc   = src->AsUnOp()->GetOp(0);
            var_types registerType = bitCastSrc->GetType();

            singleUseRef = BuildUse(bitCastSrc, allRegs(registerType));

            Interval* srcInterval = singleUseRef->getInterval();
            assert(srcInterval->registerType == registerType);
        }
        else if (src->OperIsRMWMemOp())
        {
            if (src->OperIsBinary() && !src->AsOp()->GetOp(1)->isContained())
            {
                regMaskTP regs = src->OperIsShiftOrRotate() ? RBM_RCX : RBM_NONE;
                BuildUse(src->AsOp()->GetOp(1), regs);

                if (src->OperIsShiftOrRotate())
                {
                    buildKillPositionsForNode(store, currentLoc + 1, RBM_RCX);
                }
            }
        }
        else if (varTypeIsSIMD(store->GetType()))
        {
            // This is the zero-init case, and we need a register to hold the zero.
            // (On Arm64 we can just store REG_ZR.)

            assert(src->IsHWIntrinsicZero());
            singleUseRef = BuildUse(src->AsHWIntrinsic()->GetOp(0));
        }
#endif // TARGET_XARCH
    }
    else
    {
        regMaskTP srcCandidates = RBM_NONE;
#ifdef TARGET_X86
        if (varTypeIsByte(lcl->GetRegisterType(store)))
        {
            srcCandidates = allByteRegs();
        }
#endif

        singleUseRef = BuildUse(src, srcCandidates);
    }

#ifdef TARGET_ARM
    if (store->OperIs(GT_LCL_STORE_FLD) && store->AsLclStoreFld()->IsOffsetMisaligned())
    {
        BuildInternalIntDef(store);

        if (store->TypeIs(TYP_DOUBLE))
        {
            BuildInternalIntDef(store);
            BuildInternalIntDef(store);
        }
    }
#endif

    BuildInternalUses();

    if (lcl->IsRegCandidate())
    {
        BuildStoreLclVarDef(store->AsLclStore(), lcl, singleUseRef, 0);
    }
}

void LinearScan::BuildStoreDynBlk(GenTreeDynBlk* store)
{
#ifdef TARGET_X86
    assert((store->GetKind() == StructStoreKind::RepStos) || (store->GetKind() == StructStoreKind::RepMovs));
    regMaskTP dstRegMask  = RBM_RDI;
    regMaskTP srcRegMask  = store->GetKind() == StructStoreKind::RepStos ? RBM_RAX : RBM_RSI;
    regMaskTP sizeRegMask = RBM_RCX;
#else
    assert((store->GetKind() == StructStoreKind::MemSet) || (store->GetKind() == StructStoreKind::MemCpy));
    regMaskTP dstRegMask  = RBM_ARG_0;
    regMaskTP srcRegMask  = RBM_ARG_1;
    regMaskTP sizeRegMask = RBM_ARG_2;
#endif

    BuildUse(store->GetAddr(), dstRegMask);
    BuildUse(store->GetValue(), srcRegMask);
    BuildUse(store->GetSize(), sizeRegMask);
    BuildKills(store, getKillSetForStructStore(store->GetKind()));
}

void LinearScan::BuildReturn(GenTreeUnOp* ret)
{
    if (ret->TypeIs(TYP_VOID))
    {
        return;
    }

    GenTree* src = ret->GetOp(0);

#ifdef TARGET_X86
    if (ret->TypeIs(TYP_DOUBLE, TYP_FLOAT) || (ret->TypeIs(TYP_LONG) && src->TypeIs(TYP_DOUBLE)))
    {
        BuildUse(src);

        return;
    }
#endif

#ifndef TARGET_64BIT
    if (ret->TypeIs(TYP_LONG))
    {
        assert(src->OperIs(GT_LONG) && src->isContained());

        BuildUse(src->AsOp()->GetOp(0), RBM_LNGRET_LO);
        BuildUse(src->AsOp()->GetOp(1), RBM_LNGRET_HI);

        return;
    }
#endif

#if FEATURE_MULTIREG_RET
    if (GenTreeFieldList* list = src->IsFieldList())
    {
        assert(list->isContained());

        unsigned useCount = 0;

        for (GenTreeFieldList::Use& use : list->Uses())
        {
            BuildUse(use.GetNode(), genRegMask(compiler->info.retDesc.GetRegNum(useCount++)));
        }

        assert(useCount == compiler->info.retDesc.GetRegCount());

        return;
    }
#endif

    if (src->isContained())
    {
        return;
    }

    const ReturnTypeDesc& retDesc = compiler->info.retDesc;

#if FEATURE_MULTIREG_RET
    if (retDesc.GetRegCount() > 1)
    {
        noway_assert(src->IsMultiRegCall());
        assert(varTypeIsStruct(ret->GetType()));

        if (GenTreeCall* call = src->IsCall())
        {
            assert(retDesc.GetRegCount() == call->GetRegCount());

            for (unsigned i = 0; i < retDesc.GetRegCount(); i++)
            {
                assert(varTypeUsesFloatReg(retDesc.GetRegType(i)) == varTypeUsesFloatReg(call->GetRegType(i)));

                BuildUse(src, genRegMask(retDesc.GetRegNum(i)), i);
            }
        }

        return;
    }
#endif // FEATURE_MULTIREG_RET

    BuildUse(src, genRegMask(retDesc.GetRegNum(0)));
}

bool LinearScan::supportsSpecialPutArg()
{
#if defined(DEBUG) && defined(TARGET_X86)
    // On x86, `LSRA_LIMIT_CALLER` is too restrictive to allow the use of special put args: this stress mode
    // leaves only three registers allocatable--eax, ecx, and edx--of which the latter two are also used for the
    // first two integral arguments to a call. This can leave us with too few registers to succesfully allocate in
    // situations like the following:
    //
    //     t1026 =    lclVar    ref    V52 tmp35        u:3 REG NA <l:$3a1, c:$98d>
    //
    //             /--*  t1026  ref
    //     t1352 = *  putarg_reg ref    REG NA
    //
    //      t342 =    lclVar    int    V14 loc6         u:4 REG NA $50c
    //
    //      t343 =    const     int    1 REG NA $41
    //
    //             /--*  t342   int
    //             +--*  t343   int
    //      t344 = *  +         int    REG NA $495
    //
    //      t345 =    lclVar    int    V04 arg4         u:2 REG NA $100
    //
    //             /--*  t344   int
    //             +--*  t345   int
    //      t346 = *  %         int    REG NA $496
    //
    //             /--*  t346   int
    //     t1353 = *  putarg_reg int    REG NA
    //
    //     t1354 =    lclVar    ref    V52 tmp35         (last use) REG NA
    //
    //             /--*  t1354  ref
    //     t1355 = *  lea(b+0)  byref  REG NA
    //
    // Here, the first `putarg_reg` would normally be considered a special put arg, which would remove `ecx` from the
    // set of allocatable registers, leaving only `eax` and `edx`. The allocator will then fail to allocate a register
    // for the def of `t345` if arg4 is not a register candidate: the corresponding ref position will be constrained to
    // { `ecx`, `ebx`, `esi`, `edi` }, which `LSRA_LIMIT_CALLER` will further constrain to `ecx`, which will not be
    // available due to the special put arg.
    return getStressLimitRegs() != LSRA_LIMIT_CALLER;
#else
    return true;
#endif
}

void LinearScan::BuildPutArgReg(GenTreeUnOp* putArg)
{
    assert(putArg->OperIs(GT_PUTARG_REG));

    regNumber argReg = putArg->GetRegNum();
    assert(argReg != REG_NA);

    GenTree* src = putArg->GetOp(0);

    // To avoid redundant moves, have the argument operand computed in the
    // register in which the argument is passed to the call.
    regMaskTP    argRegMask = genRegMask(argReg);
    RefPosition* use        = BuildUse(src, argRegMask);

    bool isSpecialPutArg = false;

    if (supportsSpecialPutArg() && IsRegCandidateLclLoad(src) && ((src->gtFlags & GTF_VAR_DEATH) == 0))
    {
        // This is the case for a "pass-through" copy of a lclVar.  In the case where it is a non-last-use,
        // we don't want the def of the copy to kill the lclVar register, if it is assigned the same register
        // (which is actually what we hope will happen).
        JITDUMP("Setting PUTARG_REG as a pass-through of a non-last use store\n");

        assert(use->getInterval()->isLocalVar);

        // Preference the destination to the interval of the first register defined by the first operand.
        isSpecialPutArg = true;
    }

#ifdef TARGET_ARM
    if (putArg->TypeIs(TYP_LONG))
    {
        assert(src->OperIs(GT_BITCAST));

        regMaskTP nextArgRegMask = genRegMask(REG_NEXT(argReg));

        BuildUse(src, nextArgRegMask, 1);

        BuildDef(putArg, TYP_INT, argRegMask, 0);
        BuildDef(putArg, TYP_INT, nextArgRegMask, 1);
    }
    else
#endif
    {
        RefPosition* def = BuildDef(putArg, argRegMask);

        if (isSpecialPutArg)
        {
            def->getInterval()->isSpecialPutArg = true;
            def->getInterval()->assignRelatedInterval(use->getInterval());
        }
    }
}

void LinearScan::BuildGCWriteBarrier(GenTreeIndStore* store)
{
    GenTree* addr = store->GetAddr();
    GenTree* src  = store->GetValue();

    assert(!addr->isContained() && !src->isContained());

    regMaskTP addrCandidates = RBM_ARG_0;
    regMaskTP srcCandidates  = RBM_ARG_1;

#if defined(TARGET_ARM64)
    addrCandidates = RBM_WRITE_BARRIER_DST;
    srcCandidates  = RBM_WRITE_BARRIER_SRC;
#elif defined(TARGET_X86) && NOGC_WRITE_BARRIERS
    if (GCInfo::UseOptimizedWriteBarriers())
    {
        addrCandidates = RBM_WRITE_BARRIER;
        srcCandidates  = RBM_WRITE_BARRIER_SRC;
    }
#endif

    BuildUse(addr, addrCandidates);
    BuildUse(src, srcCandidates);

    regMaskTP killMask = getKillSetForStoreInd(store);
    buildKillPositionsForNode(store, currentLoc + 1, killMask);
}

void LinearScan::BuildInstr(GenTreeInstr* instr)
{
    for (GenTreeInstr::Use& use : instr->Uses())
    {
        BuildUse(use.GetNode());
    }

    if (!instr->TypeIs(TYP_VOID))
    {
        BuildDef(instr);
    }
}

void LinearScan::BuildKeepAlive(GenTreeUnOp* node)
{
    GenTree* op = node->GetOp(0);

    if (op->isContained())
    {
        // Lowering marks the operand as reg optional so we can end up
        // with a contained LCL_LOAD here, but nothing else (no indirs).
        assert(op->IsLclLoad());
    }
    else
    {
        BuildUse(op);
    }
}
