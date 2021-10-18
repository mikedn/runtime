// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                          GCInfo                                           XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#include "gcinfo.h"
#include "emit.h"
#include "jitgcinfo.h"

#ifdef TARGET_AMD64
#include "gcinfoencoder.h" //this includes a LOT of other files too
#endif

/*****************************************************************************/
/*****************************************************************************/

/*****************************************************************************/

extern int JITGcBarrierCall;

/*****************************************************************************/

#if MEASURE_PTRTAB_SIZE
/* static */ size_t GCInfo::s_gcRegPtrDscSize   = 0;
/* static */ size_t GCInfo::s_gcTotalPtrTabSize = 0;
#endif // MEASURE_PTRTAB_SIZE

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                          GCInfo                                           XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

GCInfo::GCInfo(Compiler* theCompiler) : compiler(theCompiler)
{
    regSet         = nullptr;
    gcVarPtrList   = nullptr;
    gcVarPtrLast   = nullptr;
    gcRegPtrList   = nullptr;
    gcRegPtrLast   = nullptr;
    gcPtrArgCnt    = 0;
    gcCallDescList = nullptr;
    gcCallDescLast = nullptr;
#ifdef JIT32_GCENCODER
    gcEpilogTable = nullptr;
#else  // !JIT32_GCENCODER
    m_regSlotMap   = nullptr;
    m_stackSlotMap = nullptr;
#endif // JIT32_GCENCODER
}

/*****************************************************************************/
/*****************************************************************************
 *  Reset tracking info at the start of a basic block.
 */

void GCInfo::gcResetForBB()
{
    gcRegGCrefSetCur = RBM_NONE;
    gcRegByrefSetCur = RBM_NONE;
    VarSetOps::AssignNoCopy(compiler, gcVarPtrSetCur, VarSetOps::MakeEmpty(compiler));
}

#ifdef DEBUG

/*****************************************************************************
 *
 *  Print the changes in the gcRegGCrefSetCur sets.
 */

void GCInfo::gcDspGCrefSetChanges(regMaskTP gcRegGCrefSetNew DEBUGARG(bool forceOutput))
{
    if (compiler->verbose)
    {
        if (forceOutput || (gcRegGCrefSetCur != gcRegGCrefSetNew))
        {
            printf("GC regs: ");
            if (gcRegGCrefSetCur == gcRegGCrefSetNew)
            {
                printf("(unchanged) ");
            }
            else
            {
                printRegMaskInt(gcRegGCrefSetCur);
                compiler->GetEmitter()->emitDispRegSet(gcRegGCrefSetCur);
                printf(" => ");
            }
            printRegMaskInt(gcRegGCrefSetNew);
            compiler->GetEmitter()->emitDispRegSet(gcRegGCrefSetNew);
            printf("\n");
        }
    }
}

/*****************************************************************************
 *
 *  Print the changes in the gcRegByrefSetCur sets.
 */

void GCInfo::gcDspByrefSetChanges(regMaskTP gcRegByrefSetNew DEBUGARG(bool forceOutput))
{
    if (compiler->verbose)
    {
        if (forceOutput || (gcRegByrefSetCur != gcRegByrefSetNew))
        {
            printf("Byref regs: ");
            if (gcRegByrefSetCur == gcRegByrefSetNew)
            {
                printf("(unchanged) ");
            }
            else
            {
                printRegMaskInt(gcRegByrefSetCur);
                compiler->GetEmitter()->emitDispRegSet(gcRegByrefSetCur);
                printf(" => ");
            }
            printRegMaskInt(gcRegByrefSetNew);
            compiler->GetEmitter()->emitDispRegSet(gcRegByrefSetNew);
            printf("\n");
        }
    }
}

#endif // DEBUG

/*****************************************************************************
 *
 *  Mark the set of registers given by the specified mask as holding
 *  GCref pointer values.
 */

void GCInfo::gcMarkRegSetGCref(regMaskTP regMask DEBUGARG(bool forceOutput))
{
    // This set of registers are going to hold REFs.
    // Make sure they were not holding BYREFs.
    assert((gcRegByrefSetCur & regMask) == 0);

    regMaskTP gcRegByrefSetNew = gcRegByrefSetCur & ~regMask; // Clear it if set in Byref mask
    regMaskTP gcRegGCrefSetNew = gcRegGCrefSetCur | regMask;  // Set it in GCref mask

    INDEBUG(gcDspGCrefSetChanges(gcRegGCrefSetNew, forceOutput));
    INDEBUG(gcDspByrefSetChanges(gcRegByrefSetNew));

    gcRegByrefSetCur = gcRegByrefSetNew;
    gcRegGCrefSetCur = gcRegGCrefSetNew;
}

/*****************************************************************************
 *
 *  Mark the set of registers given by the specified mask as holding
 *  Byref pointer values.
 */

void GCInfo::gcMarkRegSetByref(regMaskTP regMask DEBUGARG(bool forceOutput))
{
    regMaskTP gcRegByrefSetNew = gcRegByrefSetCur | regMask;  // Set it in Byref mask
    regMaskTP gcRegGCrefSetNew = gcRegGCrefSetCur & ~regMask; // Clear it if set in GCref mask

    INDEBUG(gcDspGCrefSetChanges(gcRegGCrefSetNew));
    INDEBUG(gcDspByrefSetChanges(gcRegByrefSetNew, forceOutput));

    gcRegByrefSetCur = gcRegByrefSetNew;
    gcRegGCrefSetCur = gcRegGCrefSetNew;
}

/*****************************************************************************
 *
 *  Mark the set of registers given by the specified mask as holding
 *  non-pointer values.
 */

void GCInfo::gcMarkRegSetNpt(regMaskTP regMask DEBUGARG(bool forceOutput))
{
    /* NOTE: don't unmark any live register variables */

    regMaskTP gcRegByrefSetNew = gcRegByrefSetCur & ~(regMask & ~regSet->GetMaskVars());
    regMaskTP gcRegGCrefSetNew = gcRegGCrefSetCur & ~(regMask & ~regSet->GetMaskVars());

    INDEBUG(gcDspGCrefSetChanges(gcRegGCrefSetNew, forceOutput));
    INDEBUG(gcDspByrefSetChanges(gcRegByrefSetNew, forceOutput));

    gcRegByrefSetCur = gcRegByrefSetNew;
    gcRegGCrefSetCur = gcRegGCrefSetNew;
}

/*****************************************************************************
 *
 *  Mark the specified register as now holding a value of the given type.
 */

void GCInfo::gcMarkRegPtrVal(regNumber reg, var_types type)
{
    regMaskTP regMask = genRegMask(reg);

    switch (type)
    {
        case TYP_REF:
            gcMarkRegSetGCref(regMask);
            break;
        case TYP_BYREF:
            gcMarkRegSetByref(regMask);
            break;
        default:
            gcMarkRegSetNpt(regMask);
            break;
    }
}

/*****************************************************************************/

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

    WriteBarrierForm form = gcWriteBarrierFormFromTargetAddress(store->GetAddr());

    if (form == WBF_BarrierUnknown)
    {
        // If we can't figure out where the address is then use TGT_HEAP to
        // select between checked and unchecked barriers.

        form = ((store->gtFlags & GTF_IND_TGT_HEAP) != 0) ? WBF_BarrierUnchecked : WBF_BarrierChecked;
    }

    return form;
}

/*****************************************************************************
 *
 *  Initialize the non-register pointer variable tracking logic.
 */

void GCInfo::gcVarPtrSetInit()
{
    VarSetOps::AssignNoCopy(compiler, gcVarPtrSetCur, VarSetOps::MakeEmpty(compiler));

    /* Initialize the list of lifetime entries */
    gcVarPtrList = gcVarPtrLast = nullptr;
}

/*****************************************************************************
 *
 *  Allocate a new pointer register set / pointer argument entry and append
 *  it to the list.
 */

GCInfo::regPtrDsc* GCInfo::gcRegPtrAllocDsc()
{
    regPtrDsc* regPtrNext;

    assert(compiler->IsFullPtrRegMapRequired());

    /* Allocate a new entry and initialize it */

    regPtrNext = new (compiler, CMK_GC) regPtrDsc;

    regPtrNext->rpdIsThis = false;

    regPtrNext->rpdOffs = 0;
    regPtrNext->rpdNext = nullptr;

    // Append the entry to the end of the list.
    if (gcRegPtrLast == nullptr)
    {
        assert(gcRegPtrList == nullptr);
        gcRegPtrList = gcRegPtrLast = regPtrNext;
    }
    else
    {
        assert(gcRegPtrList != nullptr);
        gcRegPtrLast->rpdNext = regPtrNext;
        gcRegPtrLast          = regPtrNext;
    }

#if MEASURE_PTRTAB_SIZE
    s_gcRegPtrDscSize += sizeof(*regPtrNext);
#endif

    return regPtrNext;
}

#ifdef JIT32_GCENCODER

/*****************************************************************************
 *
 *  Compute the various counts that get stored in the info block header.
 */

void GCInfo::gcCountForHeader(UNALIGNED unsigned int* pUntrackedCount, UNALIGNED unsigned int* pVarPtrTableSize)
{
    unsigned   varNum;
    LclVarDsc* varDsc;

    bool         keepThisAlive  = false; // did we track "this" in a synchronized method?
    unsigned int untrackedCount = 0;

    // Count the untracked locals and non-enregistered args.

    for (varNum = 0, varDsc = compiler->lvaTable; varNum < compiler->lvaCount; varNum++, varDsc++)
    {
        if (varDsc->IsDependentPromotedField(compiler))
        {
            // A dependent promoted struct field must be reported through its parent local.
            continue;
        }

        if (varTypeIsGC(varDsc->TypeGet()))
        {
            if (!gcIsUntrackedLocalOrNonEnregisteredArg(varNum, &keepThisAlive))
            {
                continue;
            }

#ifdef DEBUG
            if (compiler->verbose)
            {
                int offs = varDsc->GetStackOffset();

                printf("GCINFO: untrckd %s lcl at [%s", varTypeGCstring(varDsc->TypeGet()),
                       compiler->GetEmitter()->emitGetFrameReg());

                if (offs < 0)
                {
                    printf("-%02XH", -offs);
                }
                else if (offs > 0)
                {
                    printf("+%02XH", +offs);
                }

                printf("]\n");
            }
#endif

            untrackedCount++;
        }
        else if ((varDsc->TypeGet() == TYP_STRUCT) && varDsc->lvOnFrame)
        {
            untrackedCount += varDsc->GetLayout()->GetGCPtrCount();
        }
    }

    // Also count spill temps that hold pointers.

    assert(regSet->tmpAllFree());
    for (TempDsc* tempThis = regSet->tmpListBeg(); tempThis != nullptr; tempThis = regSet->tmpListNxt(tempThis))
    {
        if (varTypeIsGC(tempThis->tdTempType()) == false)
        {
            continue;
        }

#ifdef DEBUG
        if (compiler->verbose)
        {
            int offs = tempThis->tdTempOffs();

            printf("GCINFO: untrck %s Temp at [%s", varTypeGCstring(varDsc->TypeGet()),
                   compiler->GetEmitter()->emitGetFrameReg());

            if (offs < 0)
            {
                printf("-%02XH", -offs);
            }
            else if (offs > 0)
            {
                printf("+%02XH", +offs);
            }

            printf("]\n");
        }
#endif

        untrackedCount++;
    }

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("GCINFO: untrckVars = %u\n", untrackedCount);
    }
#endif

    *pUntrackedCount = untrackedCount;

    // Count the number of entries in the table of non-register pointer variable lifetimes.

    unsigned int varPtrTableSize = 0;

    if (keepThisAlive)
    {
        varPtrTableSize++;
    }

    if (gcVarPtrList != nullptr)
    {
        // We'll use a delta encoding for the lifetime offsets.

        for (varPtrDsc* varTmp = gcVarPtrList; varTmp != nullptr; varTmp = varTmp->vpdNext)
        {
            // Special case: skip any 0-length lifetimes.

            if (varTmp->vpdBegOfs == varTmp->vpdEndOfs)
            {
                continue;
            }

            varPtrTableSize++;
        }
    }

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("GCINFO: trackdLcls = %u\n", varPtrTableSize);
    }
#endif

    *pVarPtrTableSize = varPtrTableSize;
}

//------------------------------------------------------------------------
// gcIsUntrackedLocalOrNonEnregisteredArg: Check if this varNum with GC type
// corresponds to an untracked local or argument that was not fully enregistered.
//
//
// Arguments:
//   varNum - the variable number to check;
//   pKeepThisAlive - if !FEATURE_EH_FUNCLETS and the argument != nullptr remember
//   if `this` should be kept alive and considered tracked.
//
// Return value:
//   true if it an untracked pointer value.
//
bool GCInfo::gcIsUntrackedLocalOrNonEnregisteredArg(unsigned varNum, bool* pKeepThisAlive)
{
    LclVarDsc* varDsc = compiler->lvaGetDesc(varNum);

    assert(!varDsc->IsDependentPromotedField(compiler));
    assert(varTypeIsGC(varDsc->GetType()));

    if (!varDsc->IsParam())
    {
        // If is pinned, it must be an untracked local.
        assert(!varDsc->lvPinned || !varDsc->lvTracked);

        if (varDsc->lvTracked || !varDsc->lvOnFrame)
        {
            return false;
        }
    }
    else
    {
        // Stack-passed arguments which are not enregistered are always reported in this "untracked stack pointers"
        // section of the GC info even if lvTracked==true.

        // Has this argument been fully enregistered?
        if (!varDsc->lvOnFrame)
        {
            // If a CEE_JMP has been used, then we need to report all the arguments even if they are enregistered, since
            // we will be using this value in JMP call.  Note that this is subtle as we require that argument offsets
            // are always fixed up properly even if lvRegister is set .
            if (!compiler->compJmpOpUsed)
            {
                return false;
            }
        }
        else if (varDsc->lvIsRegArg && varDsc->lvTracked)
        {
            // If this register-passed arg is tracked, then it has been allocated space near the other pointer variables
            // and we have accurate life-time info. It will be reported with gcVarPtrList in the "tracked-pointer"
            // section.
            return false;
        }
    }

#if !defined(FEATURE_EH_FUNCLETS)
    if (compiler->lvaIsOriginalThisArg(varNum) && compiler->lvaKeepAliveAndReportThis())
    {
        // "this" is in the untracked variable area, but encoding of untracked variables does not support reporting
        // "this". So report it as a tracked variable with a liveness extending over the entire method.
        //
        // TODO-x86-Cleanup: the semantic here is not clear, it would be useful to check different cases and
        // add a description where "this" is saved and how it is tracked in each of them:
        // 1) when FEATURE_EH_FUNCLETS defined (x86 Linux);
        // 2) when FEATURE_EH_FUNCLETS not defined, lvaKeepAliveAndReportThis == true, compJmpOpUsed == true;
        // 3) when there is regPtrDsc for "this", but keepThisAlive == true;
        // etc.

        if (pKeepThisAlive != nullptr)
        {
            *pKeepThisAlive = true;
        }
        return false;
    }
#endif // !FEATURE_EH_FUNCLETS
    return true;
}

/*****************************************************************************
 *
 *  Shutdown the 'pointer value' register tracking logic and save the necessary
 *  info (which will be used at runtime to locate all pointers) at the specified
 *  address. The number of bytes written to 'destPtr' must be identical to that
 *  returned from gcPtrTableSize().
 */

BYTE* GCInfo::gcPtrTableSave(BYTE* destPtr, const InfoHdr& header, unsigned codeSize, size_t* pArgTabOffset)
{
    /* Write the tables to the info block */

    return destPtr + gcMakeRegPtrTable(destPtr, -1, header, codeSize, pArgTabOffset);
}

#endif // JIT32_GCENCODER

/*****************************************************************************
 *
 *  Initialize the 'pointer value' register/argument tracking logic.
 */

void GCInfo::gcRegPtrSetInit()
{
    gcRegGCrefSetCur = gcRegByrefSetCur = 0;

    if (compiler->IsFullPtrRegMapRequired())
    {
        gcRegPtrList = gcRegPtrLast = nullptr;
    }
    else
    {
        /* Initialize the 'call descriptor' list */
        gcCallDescList = gcCallDescLast = nullptr;
    }
}

#ifdef JIT32_GCENCODER

/*****************************************************************************
 *
 *  Helper passed to genEmitter.emitGenEpilogLst() to generate
 *  the table of epilogs.
 */

/* static */ size_t GCInfo::gcRecordEpilog(void* pCallBackData, unsigned offset)
{
    GCInfo* gcInfo = (GCInfo*)pCallBackData;

    assert(gcInfo);

    size_t result = encodeUDelta(gcInfo->gcEpilogTable, offset, gcInfo->gcEpilogPrevOffset);

    if (gcInfo->gcEpilogTable)
        gcInfo->gcEpilogTable += result;

    gcInfo->gcEpilogPrevOffset = offset;

    return result;
}

#endif // JIT32_GCENCODER

GCInfo::WriteBarrierForm GCInfo::gcWriteBarrierFormFromTargetAddress(GenTree* addr)
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

//------------------------------------------------------------------------
// gcUpdateForRegVarMove: Update the masks when a variable is moved
//
// Arguments:
//    srcMask - The register mask for the register(s) from which it is being moved
//    dstMask - The register mask for the register(s) to which it is being moved
//    type    - The type of the variable
//
// Return Value:
//    None
//
// Notes:
//    This is called during codegen when a var is moved due to an LSRA_ASG.
//    It is also called by LinearScan::recordVarLocationAtStartOfBB() which is in turn called by
//    CodeGen::genCodeForBBList() at the block boundary.

void GCInfo::gcUpdateForRegVarMove(regMaskTP srcMask, regMaskTP dstMask, LclVarDsc* varDsc)
{
    var_types type    = varDsc->TypeGet();
    bool      isGCRef = (type == TYP_REF);
    bool      isByRef = (type == TYP_BYREF);

    if (srcMask != RBM_NONE)
    {
        regSet->RemoveMaskVars(srcMask);
        if (isGCRef)
        {
            assert((gcRegByrefSetCur & srcMask) == 0);
            gcRegGCrefSetCur &= ~srcMask;
            gcRegGCrefSetCur |= dstMask; // safe if no dst, i.e. RBM_NONE
        }
        else if (isByRef)
        {
            assert((gcRegGCrefSetCur & srcMask) == 0);
            gcRegByrefSetCur &= ~srcMask;
            gcRegByrefSetCur |= dstMask; // safe if no dst, i.e. RBM_NONE
        }
    }
    else if (isGCRef || isByRef)
    {
        // In this case, we are moving it from the stack to a register,
        // so remove it from the set of live stack gc refs
        VarSetOps::RemoveElemD(compiler, gcVarPtrSetCur, varDsc->lvVarIndex);
    }
    if (dstMask != RBM_NONE)
    {
        regSet->AddMaskVars(dstMask);
        // If the source is a reg, then the gc sets have been set appropriately
        // Otherwise, we have to determine whether to set them
        if (srcMask == RBM_NONE)
        {
            if (isGCRef)
            {
                gcRegGCrefSetCur |= dstMask;
            }
            else if (isByRef)
            {
                gcRegByrefSetCur |= dstMask;
            }
        }
    }
    else if (isGCRef || isByRef)
    {
        VarSetOps::AddElemD(compiler, gcVarPtrSetCur, varDsc->lvVarIndex);
    }
}

/*****************************************************************************/
/*****************************************************************************/
