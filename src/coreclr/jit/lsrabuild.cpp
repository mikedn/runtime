// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "lsra.h"

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

RefPosition* LinearScan::newRefPositionRaw(LsraLocation nodeLocation, GenTree* treeNode, RefType refType)
{
    refPositions.emplace_back(curBBNum, nodeLocation, treeNode, refType);
    RefPosition* newRP = &refPositions.back();
#ifdef DEBUG
    newRP->rpNum = static_cast<unsigned>(refPositions.size() - 1);
#endif // DEBUG
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

    if (theReferent != nullptr)
    {
        // All RefPositions except the dummy ones at the beginning of blocks

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

        RefPosition* prevRP = theReferent->recentRefPosition;
        if (prevRP != nullptr)
        {
            prevRP->nextRefPosition = rp;
        }
        else
        {
            theReferent->firstRefPosition = rp;
        }
        theReferent->recentRefPosition = rp;
        theReferent->lastRefPosition   = rp;
    }
    else
    {
        assert((rp->refType == RefTypeBB) || (rp->refType == RefTypeKillGCRefs));
    }
}

RefPosition* LinearScan::newRefPosition(
    regNumber reg, LsraLocation theLocation, RefType theRefType, GenTree* theTreeNode, regMaskTP mask)
{
    RefPosition* newRP = newRefPositionRaw(theLocation, theTreeNode, theRefType);

    RegRecord* regRecord = getRegisterRecord(reg);
    newRP->setReg(regRecord);
    newRP->registerAssignment = mask;

    newRP->setMultiRegIdx(0);
    newRP->setRegOptional(false);

    // We can't have two RefPositions on a RegRecord at the same location, unless they are different types.
    assert((regRecord->lastRefPosition == nullptr) || (regRecord->lastRefPosition->nodeLocation < theLocation) ||
           (regRecord->lastRefPosition->refType != theRefType));
    associateRefPosWithInterval(newRP);

    DBEXEC(VERBOSE, newRP->dump(this));
    return newRP;
}

RefPosition* LinearScan::newRefPosition(Interval*    theInterval,
                                        LsraLocation theLocation,
                                        RefType      theRefType,
                                        GenTree*     theTreeNode,
                                        regMaskTP    mask,
                                        unsigned     multiRegIdx)
{
    if (theInterval != nullptr)
    {
        if (mask == RBM_NONE)
        {
            mask = allRegs(theInterval->registerType);
        }
    }
    else
    {
        assert(theRefType == RefTypeBB || theRefType == RefTypeKillGCRefs);
    }
#ifdef DEBUG
    if (theInterval != nullptr && regType(theInterval->registerType) == FloatRegisterType)
    {
        // In the case we're using floating point registers we must make sure
        // this flag was set previously in the compiler since this will mandate
        // whether LSRA will take into consideration FP reg killsets.
        assert(compiler->compFloatingPointUsed || ((mask & RBM_FLT_CALLEE_SAVED) == 0));
    }
#endif // DEBUG

    // If this reference is constrained to a single register (and it's not a dummy
    // or Kill reftype already), add a RefTypeFixedReg at this location so that its
    // availability can be more accurately determined

    bool isFixedRegister = isSingleRegister(mask);
    bool insertFixedRef  = false;
    if (isFixedRegister)
    {
        // Insert a RefTypeFixedReg for any normal def or use (not ParamDef or BB),
        // but not an internal use (it will already have a FixedRef for the def).
        if ((theRefType == RefTypeDef) || ((theRefType == RefTypeUse) && !theInterval->isInternal))
        {
            insertFixedRef = true;
        }
    }

    if (insertFixedRef)
    {
        regNumber    physicalReg = genRegNumFromMask(mask);
        RefPosition* pos         = newRefPosition(physicalReg, theLocation, RefTypeFixedReg, nullptr, mask);
        assert(theInterval != nullptr);
        assert((allRegs(theInterval->registerType) & mask) != 0);
    }

    RefPosition* newRP = newRefPositionRaw(theLocation, theTreeNode, theRefType);

    newRP->setInterval(theInterval);

    // Spill info
    newRP->isFixedRegRef = isFixedRegister;

#ifndef TARGET_AMD64
    // We don't need this for AMD because the PInvoke method epilog code is explicit
    // at register allocation time.
    if (theInterval != nullptr && theInterval->isLocalVar && compiler->compMethodRequiresPInvokeFrame() &&
        theInterval->varNum == compiler->genReturnLocal)
    {
        mask &= ~(RBM_PINVOKE_TCB | RBM_PINVOKE_FRAME);
        noway_assert(mask != RBM_NONE);
    }
#endif // !TARGET_AMD64
    newRP->registerAssignment = mask;

    newRP->setMultiRegIdx(multiRegIdx);
    newRP->setRegOptional(false);

    associateRefPosWithInterval(newRP);

    if (RefTypeIsDef(newRP->refType))
    {
        assert(theInterval != nullptr);
        // TODO-MIKE-Review: Disabling single def reg stuff for multireg stores, codegen doesn't
        // seem to have proper spilling support for this case.
        theInterval->isSingleDef =
            theInterval->firstRefPosition == newRP && (theTreeNode == nullptr || !theTreeNode->IsMultiRegLclVar());
    }

    DBEXEC(VERBOSE, newRP->dump(this));
    return newRP;
}

// Adds RefPositions of the given type for all the registers in 'mask'.
void LinearScan::addRefsForPhysRegMask(regMaskTP mask, LsraLocation currentLoc, RefType refType, bool isLastUse)
{
    for (regNumber reg = REG_FIRST; mask; reg = REG_NEXT(reg), mask >>= 1)
    {
        if (mask & 1)
        {
            // This assumes that these are all "special" RefTypes that
            // don't need to be recorded on the tree (hence treeNode is nullptr)
            RefPosition* pos = newRefPosition(reg, currentLoc, refType, nullptr,
                                              genRegMask(reg)); // This MUST occupy the physical register (obviously)

            if (isLastUse)
            {
                pos->lastUse = true;
            }
        }
    }
}

// Determine the liveness kill set for a GT_STOREIND node.
// If the GT_STOREIND will generate a write barrier, determine the specific kill
// set required by the case-specific, platform-specific write barrier. If no
// write barrier is required, the kill set will be RBM_NONE.
regMaskTP LinearScan::getKillSetForStoreInd(GenTreeStoreInd* tree)
{
    regMaskTP killMask = RBM_NONE;

    GenTree* data = tree->GetValue();

    GCInfo::WriteBarrierForm writeBarrierForm = GCInfo::GetWriteBarrierForm(tree);
    if (writeBarrierForm != GCInfo::WBF_NoBarrier)
    {
        if (CodeGenInterface::UseOptimizedWriteBarriers())
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
            killMask = compiler->compHelperCallKillSet(CodeGenInterface::GetWriteBarrierHelperCall(writeBarrierForm));
        }
    }
    return killMask;
}

regMaskTP LinearScan::getKillSetForShiftRotate(GenTreeOp* shiftNode)
{
    regMaskTP killMask = RBM_NONE;
#ifdef TARGET_XARCH
    assert(shiftNode->OperIsShiftOrRotate());
    GenTree* shiftBy = shiftNode->gtGetOp2();
    if (!shiftBy->isContained())
    {
        killMask = RBM_RCX;
    }
#endif // TARGET_XARCH
    return killMask;
}

regMaskTP LinearScan::getKillSetForMul(GenTreeOp* mulNode)
{
    regMaskTP killMask = RBM_NONE;
#ifdef TARGET_XARCH
    assert(mulNode->OperIsMul());
    if (!mulNode->OperIs(GT_MUL) || (((mulNode->gtFlags & GTF_UNSIGNED) != 0) && mulNode->gtOverflowEx()))
    {
        killMask = RBM_RAX | RBM_RDX;
    }
#endif // TARGET_XARCH
    return killMask;
}

regMaskTP LinearScan::getKillSetForModDiv(GenTreeOp* node)
{
    assert(node->OperIs(GT_MOD, GT_DIV, GT_UMOD, GT_UDIV) && varTypeIsIntegral(node->GetType()));

#ifdef TARGET_XARCH
    return RBM_RAX | RBM_RDX;
#else
    return RBM_NONE;
#endif
}

regMaskTP LinearScan::getKillSetForCall(GenTreeCall* call)
{
    regMaskTP killMask = RBM_NONE;
#ifdef TARGET_X86
    if (compiler->compFloatingPointUsed)
    {
        if (call->TypeGet() == TYP_DOUBLE)
        {
            needDoubleTmpForFPCall = true;
        }
        else if (call->TypeGet() == TYP_FLOAT)
        {
            needFloatTmpForFPCall = true;
        }
    }
#endif // TARGET_X86
#if defined(TARGET_X86) || defined(TARGET_ARM)
    if (call->IsHelperCall())
    {
        CorInfoHelpFunc helpFunc = compiler->eeGetHelperNum(call->gtCallMethHnd);
        killMask                 = compiler->compHelperCallKillSet(helpFunc);
    }
    else
#endif // defined(TARGET_X86) || defined(TARGET_ARM)
    {
        // if there is no FP used, we can ignore the FP kills
        if (compiler->compFloatingPointUsed)
        {
            killMask = RBM_CALLEE_TRASH;
        }
        else
        {
            killMask = RBM_INT_CALLEE_TRASH;
        }

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
        assert(!call->IsVirtualStub() || ((killMask & genRegMask(compiler->info.virtualStubParamRegNum)) != 0));
#endif
    }
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
    regMaskTP killMask = RBM_NONE;
    switch (tree->OperGet())
    {
        case GT_LSH:
        case GT_RSH:
        case GT_RSZ:
        case GT_ROL:
        case GT_ROR:
#ifdef TARGET_X86
        case GT_LSH_HI:
        case GT_RSH_LO:
#endif
            killMask = getKillSetForShiftRotate(tree->AsOp());
            break;

        case GT_MUL:
        case GT_MULHI:
#if !defined(TARGET_64BIT)
        case GT_MUL_LONG:
#endif
            killMask = getKillSetForMul(tree->AsOp());
            break;

        case GT_MOD:
        case GT_DIV:
        case GT_UMOD:
        case GT_UDIV:
            killMask = getKillSetForModDiv(tree->AsOp());
            break;

        case GT_STORE_LCL_VAR:
        case GT_STORE_LCL_FLD:
            if (tree->TypeIs(TYP_STRUCT) && !tree->AsLclVarCommon()->GetOp(0)->IsCall())
            {
                ClassLayout* layout = tree->OperIs(GT_STORE_LCL_VAR)
                                          ? compiler->lvaGetDesc(tree->AsLclVar())->GetLayout()
                                          : tree->AsLclFld()->GetLayout(compiler);
                StructStoreKind kind = GetStructStoreKind(true, layout, tree->AsLclVarCommon()->GetOp(0));
                killMask             = getKillSetForStructStore(kind);
            }
            break;

        case GT_STORE_OBJ:
        case GT_STORE_BLK:
            killMask = getKillSetForStructStore(tree->AsBlk()->GetKind());
            break;

        case GT_COPY_BLK:
        case GT_INIT_BLK:
            killMask = getKillSetForStructStore(tree->AsDynBlk()->GetKind());
            break;

        case GT_RETURNTRAP:
            killMask = compiler->compHelperCallKillSet(CORINFO_HELP_STOP_FOR_GC);
            break;

        case GT_CALL:
            killMask = getKillSetForCall(tree->AsCall());

            break;
        case GT_STOREIND:
            killMask = getKillSetForStoreInd(tree->AsStoreInd());
            break;

#if defined(PROFILING_SUPPORTED)
        // If this method requires profiler ELT hook then mark these nodes as killing
        // callee trash registers (excluding RAX and XMM0). The reason for this is that
        // profiler callback would trash these registers. See vm\amd64\asmhelpers.asm for
        // more details.
        case GT_RETURN:
            killMask = getKillSetForReturn();
            break;

        case GT_PROF_HOOK:
            killMask = getKillSetForProfilerHook();
            break;
#endif // PROFILING_SUPPORTED

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            killMask = getKillSetForHWIntrinsic(tree->AsHWIntrinsic());
            break;
#endif // FEATURE_HW_INTRINSICS

        default:
            // for all other 'tree->OperGet()' kinds, leave 'killMask' = RBM_NONE
            break;
    }
    return killMask;
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

        addRefsForPhysRegMask(killMask, currentLoc, RefTypeKill, true);

        // TODO-CQ: It appears to be valuable for both fp and int registers to avoid killing the callee
        // save regs on infrequently executed paths.  However, it results in a large number of asmDiffs,
        // many of which appear to be regressions (because there is more spill on the infrequently path),
        // but are not really because the frequent path becomes smaller.  Validating these diffs will need
        // to be done before making this change.
        // Also note that we avoid setting callee-save preferences for floating point. This may need
        // revisiting, and note that it doesn't currently apply to SIMD types, only float or double.
        // if (!blockSequence[curBBSeqNum]->isRunRarely())
        if (enregisterLocalVars)
        {
            VarSetOps::Iter iter(compiler, currentLiveVars);
            unsigned        varIndex = 0;
            while (iter.NextElem(&varIndex))
            {
                LclVarDsc* varDsc = compiler->lvaGetDescByTrackedIndex(varIndex);
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
        newRefPosition(nullptr, currentLoc, RefTypeKillGCRefs, tree, (allRegs(TYP_REF) & ~RBM_ARG_REGS));
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
bool LinearScan::IsCandidateLclVarMultiReg(GenTreeLclVar* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR));

    if (!store->IsMultiReg())
    {
        return false;
    }

    LclVarDsc* varDsc = compiler->lvaGetDesc(store);
    assert(varDsc->IsPromoted());

    bool isMultiReg = varDsc->IsIndependentPromoted();

    if (!isMultiReg)
    {
        store->ClearMultiReg();
    }

#ifdef DEBUG
    for (unsigned int i = 0; i < varDsc->GetPromotedFieldCount(); i++)
    {
        LclVarDsc* fieldLcl = compiler->lvaGetDesc(varDsc->GetPromotedFieldLclNum(i));

        assert(fieldLcl->IsRegCandidate() == isMultiReg);
    }
#endif

    return isMultiReg;
}

// Check whether a GT_LCL_VAR node is a candidate or contained.
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
bool LinearScan::checkContainedOrCandidateLclVar(GenTreeLclVar* lclNode)
{
    assert(lclNode->OperIs(GT_LCL_VAR) && !lclNode->IsMultiReg());
    // We shouldn't be calling this if this node was already contained.
    assert(!lclNode->isContained());

    bool isCandidate = compiler->lvaGetDesc(lclNode)->IsRegCandidate();

    if (!isCandidate && lclNode->IsRegOptional())
    {
        lclNode->ClearRegOptional();
        lclNode->SetContained();

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
            use->delayRegFree = true;
            pendingDelayFree  = true;
        }
    }
    // internalCount = 0;
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
        VARSET_TP       liveLargeVectors(VarSetOps::Intersection(compiler, currentLiveVars, largeVectorVars));
        VarSetOps::Iter iter(compiler, liveLargeVectors);
        unsigned        varIndex = 0;
        while (iter.NextElem(&varIndex))
        {
            Interval* varInterval = getIntervalForLocalVar(varIndex);
            if (!varInterval->isPartiallySpilled)
            {
                Interval*    upperVectorInterval = getUpperVectorInterval(varIndex);
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
            assert(defNode->OperIs(GT_LCL_VAR, GT_CALL));

            if (defNode->OperIs(GT_LCL_VAR))
            {
                regType = compiler->lvaGetDesc(defNode->AsLclVar())->GetRegisterType();
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
unsigned LinearScan::ComputeOperandDstCount(GenTree* operand)
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
        return operand->GetRegisterDstCount(compiler);
    }

    // This must be one of the operand types that are neither contained nor produce a value.
    // Stores and void-typed operands may be encountered when processing call nodes, which contain
    // pointers to argument setup stores.
    assert(operand->OperIsStore() || operand->OperIsPutArgStk() || operand->OperIsCompare() ||
           operand->OperIs(GT_CMP) || operand->TypeIs(TYP_VOID));

    return 0;
}

// Computes the number of registers available as sources for a node.
// This is simply the sum of the number of registers produced by each
// operand to the node.
unsigned LinearScan::ComputeAvailableSrcCount(GenTree* node)
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
    if (VERBOSE)
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
        if (tree->OperIs(GT_LCL_VAR) && ((tree->gtFlags & GTF_VAR_DEATH) != 0))
        {
            LclVarDsc* lcl = compiler->lvaGetDesc(tree->AsLclVar());

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

    // We make a final determination about whether a GT_LCL_VAR is a candidate or contained
    // after liveness. In either case we don't build any uses or defs. Otherwise, this is a
    // load of a stack-based local into a register and we'll fall through to the general
    // local case below.
    if (!tree->OperIs(GT_LCL_VAR) || !checkContainedOrCandidateLclVar(tree->AsLclVar()))
    {
        BuildNode(tree);
    }

    // int newDefListCount = defList.Count();
    // unsigned produce = newDefListCount - oldDefListCount;

    assert(
        // RegOptional LCL_VARs may become contained.
        ((nodeDefCount == 0) && tree->isContained()) ||
        // A reg candidate store is not a value so GetRegisterDstCount returns 0, but it does define a register.
        ((nodeDefCount == 1) && tree->OperIs(GT_STORE_LCL_VAR) &&
         compiler->lvaGetDesc(tree->AsLclVar())->IsRegCandidate()) ||
        // A reg candidate load is a value so GetRegisterDstCount returns 1, but it does not define a new register.
        ((nodeDefCount == 0) && tree->OperIs(GT_LCL_VAR) && compiler->lvaGetDesc(tree->AsLclVar())->IsRegCandidate()) ||
        (nodeDefCount == tree->GetRegisterDstCount(compiler)));

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
        RefPosition* newRefPosition = &(*iter);
        if (newRefPosition->isIntervalRef())
        {
            if ((newRefPosition->refType == RefTypeUse) ||
                ((newRefPosition->refType == RefTypeDef) && !newRefPosition->getInterval()->isInternal))
            {
                minRegCount++;
            }
#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
            else if (newRefPosition->refType == RefTypeUpperVectorSave)
            {
                minRegCount++;
            }
#endif
            if (newRefPosition->getInterval()->isSpecialPutArg)
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
        RefPosition* newRefPosition    = &(*refPositionMark);
        unsigned     minRegCountForRef = minRegCount;
        if (RefTypeIsUse(newRefPosition->refType) && newRefPosition->delayRegFree)
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
            regMaskTP killMask = getKillSetForNode(tree);
            if (killMask != RBM_NONE)
            {
                minRegCountForRef += genCountBits(killMask);
            }
        }
        else if ((newRefPosition->refType) == RefTypeDef && (newRefPosition->getInterval()->isSpecialPutArg))
        {
            minRegCountForRef++;
        }

        newRefPosition->minRegCandidateCount = minRegCountForRef;
        if (newRefPosition->IsActualRef() && doReverseCallerCallee())
        {
            Interval* interval       = newRefPosition->getInterval();
            regMaskTP oldAssignment  = newRefPosition->registerAssignment;
            regMaskTP calleeSaveMask = calleeSaveRegs(interval->registerType);
            newRefPosition->registerAssignment =
                getConstrainedRegMask(oldAssignment, calleeSaveMask, minRegCountForRef);
            if ((newRefPosition->registerAssignment != oldAssignment) && (newRefPosition->refType == RefTypeUse) &&
                !interval->isLocalVar)
            {
                checkConflictingDefUse(newRefPosition);
            }
        }
    }
}
#endif // DEBUG

void LinearScan::buildPhysRegRecords()
{
    static const regNumber lsraRegOrder[]{REG_VAR_ORDER};
    static const regNumber lsraRegOrderFlt[]{REG_VAR_ORDER_FLT};

    for (regNumber reg = REG_FIRST; reg < ACTUAL_REG_COUNT; reg = REG_NEXT(reg))
    {
        RegRecord* curr = &physRegs[reg];
        curr->init(reg);
    }
    for (unsigned int i = 0; i < ArrLen(lsraRegOrder); i++)
    {
        regNumber  reg  = lsraRegOrder[i];
        RegRecord* curr = &physRegs[reg];
        curr->regOrder  = (unsigned char)i;
    }
    for (unsigned int i = 0; i < ArrLen(lsraRegOrderFlt); i++)
    {
        regNumber  reg  = lsraRegOrderFlt[i];
        RegRecord* curr = &physRegs[reg];
        curr->regOrder  = (unsigned char)i;
    }
}

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
#ifdef DEBUG
    VARSET_TP expectedLiveVars(VarSetOps::Intersection(compiler, registerCandidateVars, compiler->fgFirstBB->bbLiveIn));
    assert(VarSetOps::Equal(compiler, currentLiveVars, expectedLiveVars));
#endif //  DEBUG

    // insert defs for this, then a block boundary

    VarSetOps::Iter iter(compiler, currentLiveVars);
    unsigned        varIndex = 0;
    while (iter.NextElem(&varIndex))
    {
        LclVarDsc* varDsc = compiler->lvaGetDescByTrackedIndex(varIndex);

        if (!varDsc->IsParam() && varDsc->IsRegCandidate())
        {
            JITDUMP("V%02u was live in to first block:", compiler->lvaTrackedIndexToLclNum(varIndex));
            Interval* interval = getIntervalForLocalVar(varIndex);
            if (compiler->info.compInitMem || varTypeIsGC(varDsc->TypeGet()))
            {
                varDsc->lvMustInit = true;

                // OSR will handle init of locals and promoted fields thereof
                if (compiler->lvaIsOSRLocal(compiler->lvaTrackedIndexToLclNum(varIndex)))
                {
                    JITDUMP(" will be initialized by OSR\n");
                    // setIntervalAsSpilled(interval);
                    varDsc->lvMustInit = false;
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
    }

    // We must also insert zero-inits for any finallyVars if they are refs or if compInitMem is true.
    if (compiler->lvaEnregEHVars)
    {
        VarSetOps::Iter iter(compiler, finallyVars);
        unsigned        varIndex = 0;
        while (iter.NextElem(&varIndex))
        {
            LclVarDsc* varDsc = compiler->lvaGetDescByTrackedIndex(varIndex);

            if (!varDsc->IsParam() && varDsc->IsRegCandidate())
            {
                JITDUMP("V%02u is a finally var:", compiler->lvaTrackedIndexToLclNum(varIndex));
                Interval* interval = getIntervalForLocalVar(varIndex);
                if (compiler->info.compInitMem || varTypeIsGC(varDsc->TypeGet()))
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
    BasicBlock* block;

    JITDUMP("\nbuildIntervals ========\n");

    // Build (empty) records for all of the physical registers
    buildPhysRegRecords();

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

    DBEXEC(VERBOSE, TupleStyleDump(LSRA_DUMP_PRE));

    // second part:
    JITDUMP("\nbuildIntervals second part ========\n");
    currentLoc = 0;
    // TODO-Cleanup: This duplicates prior behavior where entry (ParamDef) RefPositions were
    // being assigned the bbNum of the last block traversed in the 2nd phase of Lowering.
    // Previously, the block sequencing was done for the (formerly separate) Build pass,
    // and the curBBNum was left as the last block sequenced. This block was then used to set the
    // weight for the entry (ParamDef) RefPositions. It would be logical to set this to the
    // normalized entry weight (compiler->fgCalledCount), but that results in a net regression.
    if (!blockSequencingDone)
    {
        setBlockSequence();
    }

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

    for (unsigned argNum = 0; argNum < compiler->info.compArgsCount; argNum++)
    {
        LclVarDsc* argDsc = compiler->lvaGetDesc(argNum);

        if (argDsc->lvPromotedStruct())
        {
            for (unsigned fieldVarNum = argDsc->lvFieldLclStart;
                 fieldVarNum < argDsc->lvFieldLclStart + argDsc->lvFieldCnt; ++fieldVarNum)
            {
                LclVarDsc* fieldVarDsc = compiler->lvaGetDesc(fieldVarNum);

                noway_assert(fieldVarDsc->IsParam());

                if (!fieldVarDsc->HasLiveness() && fieldVarDsc->IsRegParam())
                {
                    AddLiveParamRegs(fieldVarDsc);
                }
            }
        }
        else
        {
            noway_assert(argDsc->IsParam());

            if (!argDsc->HasLiveness() && argDsc->IsRegParam())
            {
                AddLiveParamRegs(argDsc);
            }
        }
    }

    // If there is a secret stub param, it is also live in
    if (compiler->info.compPublishStubParam)
    {
        compiler->codeGen->paramRegState.intRegLiveIn |= RBM_SECRET_STUB_PARAM;
    }

    BasicBlock* predBlock = nullptr;
    BasicBlock* prevBlock = nullptr;

    // Initialize currentLiveVars to the empty set.  We will set it to the current
    // live-in at the entry to each block (this will include the incoming args on
    // the first block).
    VarSetOps::AssignNoCopy(compiler, currentLiveVars, VarSetOps::MakeEmpty(compiler));

    for (block = startBlockSequence(); block != nullptr; block = moveToNextBlock())
    {
        JITDUMP("\nNEW BLOCK " FMT_BB "\n", block->bbNum);

        bool predBlockIsAllocated = false;
        predBlock                 = findPredBlockForLiveIn(block, prevBlock DEBUGARG(&predBlockIsAllocated));
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
            VarSetOps::AssignNoCopy(compiler, currentLiveVars,
                                    VarSetOps::Intersection(compiler, registerCandidateVars, block->bbLiveIn));

            if (block == compiler->fgFirstBB)
            {
                insertZeroInitRefPositions();
                // The first real location is at 1; 0 is for the entry.
                currentLoc = 1;
            }

            // For blocks that don't have EHBoundaryIn, we need DummyDefs for cases where "predBlock" isn't
            // really a predecessor.
            // Note that it's possible to have uses of unitialized variables, in which case even the first
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
                    VARSET_TP newLiveIn(VarSetOps::MakeCopy(compiler, currentLiveVars));
                    if (predBlock != nullptr)
                    {
                        // Compute set difference: newLiveIn = currentLiveVars - predBlock->bbLiveOut
                        VarSetOps::DiffD(compiler, newLiveIn, predBlock->bbLiveOut);
                    }
                    // Don't create dummy defs for EH vars; we'll load them from the stack as/when needed.
                    VarSetOps::DiffD(compiler, newLiveIn, exceptVars);

                    // Create dummy def RefPositions

                    if (!VarSetOps::IsEmpty(compiler, newLiveIn))
                    {
                        // If we are using locations from a predecessor, we should never require DummyDefs.
                        assert(!predBlockIsAllocated);

                        JITDUMP("Creating dummy definitions\n");
                        VarSetOps::Iter iter(compiler, newLiveIn);
                        unsigned        varIndex = 0;
                        while (iter.NextElem(&varIndex))
                        {
                            // Add a dummyDef for any candidate vars that are in the "newLiveIn" set.
                            LclVarDsc* varDsc = compiler->lvaGetDescByTrackedIndex(varIndex);
                            assert(varDsc->IsRegCandidate());
                            Interval*    interval = getIntervalForLocalVar(varIndex);
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

        RefPosition* pos = newRefPosition((Interval*)nullptr, currentLoc, RefTypeBB, nullptr, RBM_NONE);
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
            addRefsForPhysRegMask(genRegMask(REG_SCRATCH), currentLoc + 1, RefTypeKill, true);
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
            VarSetOps::Iter largeVectorVarsIter(compiler, largeVectorVars);
            unsigned        largeVectorVarIndex = 0;
            while (largeVectorVarsIter.NextElem(&largeVectorVarIndex))
            {
                Interval* lclVarInterval = getIntervalForLocalVar(largeVectorVarIndex);
                buildUpperVectorRestoreRefPosition(lclVarInterval, currentLoc, nullptr);
            }
        }
#endif // FEATURE_PARTIAL_SIMD_CALLEE_SAVE

        // Note: the visited set is cleared in LinearScan::doLinearScan()
        markBlockVisited(block);

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

            VARSET_TP expUseSet(VarSetOps::MakeCopy(compiler, block->bbLiveOut));
            VarSetOps::IntersectionD(compiler, expUseSet, registerCandidateVars);
            BasicBlock* nextBlock = getNextBlock();
            if (nextBlock != nullptr)
            {
                VarSetOps::DiffD(compiler, expUseSet, nextBlock->bbLiveIn);
            }
            for (BasicBlock* succ : block->GetAllSuccs(compiler))
            {
                if (VarSetOps::IsEmpty(compiler, expUseSet))
                {
                    break;
                }

                if (isBlockVisited(succ))
                {
                    continue;
                }
                VarSetOps::DiffD(compiler, expUseSet, succ->bbLiveIn);
            }

            if (!VarSetOps::IsEmpty(compiler, expUseSet))
            {
                JITDUMP("Exposed uses:");
                VarSetOps::Iter iter(compiler, expUseSet);
                unsigned        varIndex = 0;
                while (iter.NextElem(&varIndex))
                {
                    unsigned   varNum = compiler->lvaTrackedToVarNum[varIndex];
                    LclVarDsc* varDsc = compiler->lvaTable + varNum;
                    assert(varDsc->IsRegCandidate());
                    Interval*    interval = getIntervalForLocalVar(varIndex);
                    RefPosition* pos =
                        newRefPosition(interval, currentLoc, RefTypeExpUse, nullptr, allRegs(interval->registerType));
                    pos->setRegOptional(true);
                    JITDUMP(" V%02u", varNum);
                }
                JITDUMP("\n");
            }

            // Clear the "last use" flag on any vars that are live-out from this block.
            VARSET_TP       bbLiveDefs(VarSetOps::Intersection(compiler, registerCandidateVars, block->bbLiveOut));
            VarSetOps::Iter iter(compiler, bbLiveDefs);
            unsigned        varIndex = 0;
            while (iter.NextElem(&varIndex))
            {
                unsigned         varNum = compiler->lvaTrackedToVarNum[varIndex];
                LclVarDsc* const varDsc = &compiler->lvaTable[varNum];
                assert(varDsc->IsRegCandidate());
                RefPosition* const lastRP = getIntervalForLocalVar(varIndex)->lastRefPosition;
                // We should be able to assert that lastRP is non-null if it is live-out, but sometimes liveness
                // lies.
                if ((lastRP != nullptr) && (lastRP->bbNum == block->bbNum))
                {
                    lastRP->lastUse = false;
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
            unsigned keepAliveVarNum = compiler->info.compThisArg;
            assert(compiler->info.compIsStatic == false);
            LclVarDsc* varDsc = compiler->lvaTable + keepAliveVarNum;
            if (varDsc->IsRegCandidate())
            {
                JITDUMP("Adding exposed use of this, for lvaKeepAliveAndReportThis\n");
                Interval*    interval = getIntervalForLocalVar(varDsc->lvVarIndex);
                RefPosition* pos =
                    newRefPosition(interval, currentLoc, RefTypeExpUse, nullptr, allRegs(interval->registerType));
                pos->setRegOptional(true);
            }
        }
        // Adjust heuristics for writeThru intervals.
        if (compiler->compHndBBtabCount > 0)
        {
            VarSetOps::Iter iter(compiler, exceptVars);
            unsigned        varIndex = 0;
            while (iter.NextElem(&varIndex))
            {
                unsigned   varNum   = compiler->lvaTrackedToVarNum[varIndex];
                LclVarDsc* varDsc   = compiler->lvaTable + varNum;
                Interval*  interval = getIntervalForLocalVar(varIndex);
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
            for (unsigned lclNum = 0; lclNum < compiler->lvaCount; lclNum++)
            {
                LclVarDsc* varDsc = compiler->lvaGetDesc(lclNum);
                if (varDsc->IsRegCandidate())
                {
                    JITDUMP("Adding exposed use of V%02u for LsraExtendLifetimes\n", lclNum);
                    Interval*    interval = getIntervalForLocalVar(varDsc->lvVarIndex);
                    RefPosition* pos =
                        newRefPosition(interval, currentLoc, RefTypeExpUse, nullptr, allRegs(interval->registerType));
                    pos->setRegOptional(true);
                }
            }
        }
#endif // DEBUG
    }

    // If the last block has successors, create a RefTypeBB to record
    // what's live

    if (prevBlock->NumSucc(compiler) > 0)
    {
        RefPosition* pos = newRefPosition((Interval*)nullptr, currentLoc, RefTypeBB, nullptr, RBM_NONE);
    }

#ifdef DEBUG
    // Make sure we don't have any blocks that were not visited
    for (BasicBlock* const block : compiler->Blocks())
    {
        assert(isBlockVisited(block));
    }

    if (VERBOSE)
    {
        lsraDumpIntervals("BEFORE VALIDATING INTERVALS");
        dumpRefPositions("BEFORE VALIDATING INTERVALS");
    }
    validateIntervals();

#endif // DEBUG
}

#ifdef DEBUG
void LinearScan::validateIntervals()
{
    if (enregisterLocalVars)
    {
        for (unsigned i = 0; i < compiler->lvaTrackedCount; i++)
        {
            if (!compiler->lvaGetDescByTrackedIndex(i)->IsRegCandidate())
            {
                continue;
            }
            Interval* interval = getIntervalForLocalVar(i);

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
                        JITDUMP("LocalVar V%02u: undefined use at %u\n", interval->varNum, ref->nodeLocation);
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

    if (isCandidateLclVar(operand))
    {
        interval = getIntervalForLocalVarNode(operand->AsLclVar());

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
            unsigned varIndex = interval->getVarIndex(compiler);
            VarSetOps::RemoveElemD(compiler, currentLiveVars, varIndex);
        }
#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
        buildUpperVectorRestoreRefPosition(interval, currentLoc, operand);
#endif
    }
    else if (operand->IsMultiRegLclVar())
    {
        LclVarDsc* varDsc      = compiler->lvaGetDesc(operand->AsLclVar()->GetLclNum());
        LclVarDsc* fieldVarDsc = compiler->lvaGetDesc(varDsc->lvFieldLclStart + regIndex);
        interval               = getIntervalForLocalVar(fieldVarDsc->lvVarIndex);
        if (operand->AsLclVar()->IsLastUse(regIndex))
        {
            VarSetOps::RemoveElemD(compiler, currentLiveVars, fieldVarDsc->lvVarIndex);
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

void LinearScan::BuildOperandUses(GenTree* node, regMaskTP candidates)
{
    if (!node->isContained())
    {
        BuildUse(node, candidates);
    }
#ifndef TARGET_64BIT
    else if (node->OperIs(GT_LONG))
    {
        BuildUse(node->AsOp()->GetOp(0));
        BuildUse(node->AsOp()->GetOp(1));
    }
#endif
    else if (node->OperIsIndir())
    {
        BuildAddrUses(node->AsIndir()->GetAddr(), candidates);
    }
    else if (node->OperIs(GT_LEA))
    {
        BuildAddrUses(node, candidates);
    }
#ifdef FEATURE_HW_INTRINSICS
    else if (GenTreeHWIntrinsic* hwi = node->IsHWIntrinsic())
    {
        if (hwi->OperIsMemoryLoad())
        {
            BuildAddrUses(hwi->GetOp(0));
        }
        // TODO-MIKE-Review: What is this for?
        else if (hwi->GetNumOps() >= 1)
        {
            BuildUse(hwi->GetOp(0), candidates);
        }
    }
#endif // FEATURE_HW_INTRINSICS
}

void LinearScan::setDelayFree(RefPosition* use)
{
    use->delayRegFree = true;
    pendingDelayFree  = true;
}

void LinearScan::BuildDelayFreeUses(GenTree* node, GenTree* rmwNode, regMaskTP candidates)
{
    Interval* rmwInterval  = nullptr;
    bool      rmwIsLastUse = false;

    if ((rmwNode != nullptr) && isCandidateLclVar(rmwNode))
    {
        rmwInterval = getIntervalForLocalVarNode(rmwNode->AsLclVar());
        // Note: we don't handle multi-reg vars here. It's not clear that there
        // are any cases where we'd encounter a multi-reg var in an RMW context.
        assert(!rmwNode->AsLclVar()->IsMultiReg());
        rmwIsLastUse = rmwNode->AsLclVar()->IsLastUse(0);
    }

    auto BuildDelayFreeUse = [this, rmwInterval, rmwIsLastUse](GenTree* operand, regMaskTP candidates) {
        RefPosition* use = BuildUse(operand, candidates);

        if ((use->getInterval() != rmwInterval) || (!rmwIsLastUse && !use->lastUse))
        {
            setDelayFree(use);
        }
    };

    if (!node->isContained())
    {
        BuildDelayFreeUse(node, candidates);

        return;
    }

#ifdef FEATURE_HW_INTRINSICS
    if (GenTreeHWIntrinsic* hwIntrinsicNode = node->IsHWIntrinsic())
    {
        BuildDelayFreeUse(hwIntrinsicNode->GetOp(0), candidates);

        return;
    }
#endif

    if (GenTreeIndir* indir = node->IsIndir())
    {
        GenTree* addr = indir->GetAddr();

        if (!addr->isContained())
        {
            // TODO-MIKE-Review: Using "candidates" here and below is likely bogus.
            // The caller usually cares only about the case of a non contained
            // operand, it doesn't know or care about whatever registers an address
            // mode needs. Some callers pass candidates such as XMM0 or "byte regs"
            // on x86...

            BuildDelayFreeUse(addr, candidates);
        }
        else if (GenTreeAddrMode* const addrMode = addr->IsAddrMode())
        {
            if (GenTree* base = addrMode->GetBase())
            {
                BuildDelayFreeUse(base, candidates);
            }

            if (GenTree* index = addrMode->GetIndex())
            {
                BuildDelayFreeUse(index, candidates);
            }
        }

        return;
    }
}

void LinearScan::BuildStoreLclVarDef(GenTreeLclVar* store, LclVarDsc* lcl, RefPosition* singleUseRef, unsigned index)
{
    assert(store->OperIs(GT_STORE_LCL_VAR));
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

void LinearScan::BuildStoreLclVarMultiReg(GenTreeLclVar* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR) && store->IsMultiReg());

    LclVarDsc* lcl = compiler->lvaGetDesc(store);
    assert(lcl->IsIndependentPromoted());
    unsigned regCount = lcl->GetPromotedFieldCount();

    GenTree* src = store->GetOp(0);
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

void LinearScan::BuildStoreLclVar(GenTreeLclVar* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR));

    if (IsCandidateLclVarMultiReg(store))
    {
        BuildStoreLclVarMultiReg(store);

        return;
    }

    LclVarDsc* lcl = compiler->lvaGetDesc(store);
    GenTree*   src = store->GetOp(0);

    if (store->TypeIs(TYP_STRUCT) && !src->IsCall())
    {
        ClassLayout*    layout = lcl->GetLayout();
        StructStoreKind kind   = GetStructStoreKind(true, layout, src);
        BuildStructStore(store, kind, layout);

        return;
    }

    BuildStoreLcl(store);
}

void LinearScan::BuildStoreLclFld(GenTreeLclFld* store)
{
    assert(store->OperIs(GT_STORE_LCL_FLD));

    if (store->TypeIs(TYP_STRUCT))
    {
        ClassLayout*    layout = store->AsLclFld()->GetLayout(compiler);
        StructStoreKind kind   = GetStructStoreKind(true, layout, store->GetOp(0));
        BuildStructStore(store, kind, layout);

        return;
    }

    BuildStoreLcl(store);
}

void LinearScan::BuildStoreLcl(GenTreeLclVarCommon* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    GenTree* src = store->GetOp(0);

#ifdef FEATURE_SIMD
    if (store->TypeIs(TYP_SIMD12))
    {
        if (src->isContained() && src->OperIs(GT_IND, GT_LCL_FLD, GT_LCL_VAR))
        {
            BuildInternalIntDef(store);

            if (src->OperIs(GT_IND))
            {
                BuildAddrUses(src->AsIndir()->GetAddr());
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

    LclVarDsc*   lcl          = compiler->lvaGetDesc(store);
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
    if (store->OperIs(GT_STORE_LCL_FLD) && store->AsLclFld()->IsOffsetMisaligned())
    {
        BuildInternalIntDef(store); // to generate address.
        BuildInternalIntDef(store); // to move float into an int reg.

        if (store->TypeIs(TYP_DOUBLE))
        {
            BuildInternalIntDef(store); // to move the second half into an int reg.
        }
    }
#endif

    BuildInternalUses();

    if (lcl->IsRegCandidate())
    {
        BuildStoreLclVarDef(store->AsLclVar(), lcl, singleUseRef, 0);
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

    if (supportsSpecialPutArg() && isCandidateLclVar(src) && ((src->gtFlags & GTF_VAR_DEATH) == 0))
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

void LinearScan::BuildGCWriteBarrier(GenTreeStoreInd* store)
{
    GenTree* addr = store->GetAddr();
    GenTree* src  = store->GetValue();

    // In the case where we are doing a helper assignment, even if the dst
    // is an indir through an lea, we need to actually instantiate the
    // lea in a register
    assert(!addr->isContained() && !src->isContained());

    regMaskTP addrCandidates = RBM_ARG_0;
    regMaskTP srcCandidates  = RBM_ARG_1;

#if defined(TARGET_ARM64)
    // the 'addr' goes into x14 (REG_WRITE_BARRIER_DST)
    // the 'src'  goes into x15 (REG_WRITE_BARRIER_SRC)
    //
    addrCandidates = RBM_WRITE_BARRIER_DST;
    srcCandidates  = RBM_WRITE_BARRIER_SRC;
#elif defined(TARGET_X86) && NOGC_WRITE_BARRIERS
    if (CodeGenInterface::UseOptimizedWriteBarriers())
    {
        // Special write barrier:
        // op1 (addr) goes into REG_WRITE_BARRIER (rdx) and
        // op2 (src) goes into any int register.
        addrCandidates = RBM_WRITE_BARRIER;
        srcCandidates  = RBM_WRITE_BARRIER_SRC;
    }
#endif // defined(TARGET_X86) && NOGC_WRITE_BARRIERS

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
