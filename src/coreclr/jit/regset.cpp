// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                           RegSet                                          XX
XX                                                                           XX
XX  Represents the register set, and their states during code generation     XX
XX  Can select an unused register, keeps track of the contents of the        XX
XX  registers, and can spill registers                                       XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#include "emit.h"

/*****************************************************************************/

#ifdef TARGET_ARM64
const regMaskSmall regMasks[] = {
#define REGDEF(name, rnum, mask, xname, wname) mask,
#include "register.h"
};
#else // !TARGET_ARM64
const regMaskSmall regMasks[] = {
#define REGDEF(name, rnum, mask, sname) mask,
#include "register.h"
};
#endif

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                          RegSet                                           XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

//------------------------------------------------------------------------
// verifyRegUsed: verify that the register is marked as used.
//
// Arguments:
//    reg - The register to verify.
//
// Return Value:
//   None.
//
// Assumptions:
//    The caller must have ensured that the register is already marked
//    as used.
//
// Notes:
//     This method is intended to be called during code generation, and
//     should simply validate that the register (or registers) have
//     already been added to the modified set.

void RegSet::verifyRegUsed(regNumber reg)
{
    // TODO-Cleanup: we need to identify the places where the register
    //               is not marked as used when this is called.
    rsSetRegsModified(genRegMask(reg));
}

//------------------------------------------------------------------------
// verifyRegistersUsed: verify that the registers are marked as used.
//
// Arguments:
//    regs - The registers to verify.
//
// Return Value:
//   None.
//
// Assumptions:
//    The caller must have ensured that the registers are already marked
//    as used.
//
// Notes:
//     This method is intended to be called during code generation, and
//     should simply validate that the register (or registers) have
//     already been added to the modified set.

void RegSet::verifyRegistersUsed(regMaskTP regMask)
{
    if (m_rsCompiler->opts.OptimizationDisabled())
    {
        return;
    }

    if (regMask == RBM_NONE)
    {
        return;
    }

    // TODO-Cleanup: we need to identify the places where the registers
    //               are not marked as used when this is called.
    rsSetRegsModified(regMask);
}

void RegSet::rsClearRegsModified()
{
    assert(m_rsCompiler->lvaDoneFrameLayout < Compiler::FINAL_FRAME_LAYOUT);

#ifdef DEBUG
    if (m_rsCompiler->verbose)
    {
        printf("Clearing modified regs.\n");
    }
    rsModifiedRegsMaskInitialized = true;
#endif // DEBUG

    rsModifiedRegsMask = RBM_NONE;
}

void RegSet::rsSetRegsModified(regMaskTP mask DEBUGARG(bool suppressDump))
{
    assert(mask != RBM_NONE);
    assert(rsModifiedRegsMaskInitialized);

    // We can't update the modified registers set after final frame layout (that is, during code
    // generation and after). Ignore prolog and epilog generation: they call register tracking to
    // modify rbp, for example, even in functions that use rbp as a frame pointer. Make sure normal
    // code generation isn't actually adding to set of modified registers.
    // Frame layout is only affected by callee-saved registers, so only ensure that callee-saved
    // registers aren't modified after final frame layout.
    assert((m_rsCompiler->lvaDoneFrameLayout < Compiler::FINAL_FRAME_LAYOUT) || m_rsCompiler->compGeneratingProlog ||
           m_rsCompiler->compGeneratingEpilog ||
           (((rsModifiedRegsMask | mask) & RBM_CALLEE_SAVED) == (rsModifiedRegsMask & RBM_CALLEE_SAVED)));

#ifdef DEBUG
    if (m_rsCompiler->verbose && !suppressDump)
    {
        if (rsModifiedRegsMask != (rsModifiedRegsMask | mask))
        {
            printf("Marking regs modified: ");
            dspRegMask(mask);
            printf(" (");
            dspRegMask(rsModifiedRegsMask);
            printf(" => ");
            dspRegMask(rsModifiedRegsMask | mask);
            printf(")\n");
        }
    }
#endif // DEBUG

    rsModifiedRegsMask |= mask;
}

void RegSet::rsRemoveRegsModified(regMaskTP mask)
{
    assert(mask != RBM_NONE);
    assert(rsModifiedRegsMaskInitialized);

    // See comment in rsSetRegsModified().
    assert((m_rsCompiler->lvaDoneFrameLayout < Compiler::FINAL_FRAME_LAYOUT) || m_rsCompiler->compGeneratingProlog ||
           m_rsCompiler->compGeneratingEpilog ||
           (((rsModifiedRegsMask & ~mask) & RBM_CALLEE_SAVED) == (rsModifiedRegsMask & RBM_CALLEE_SAVED)));

#ifdef DEBUG
    if (m_rsCompiler->verbose)
    {
        printf("Removing modified regs: ");
        dspRegMask(mask);
        if (rsModifiedRegsMask == (rsModifiedRegsMask & ~mask))
        {
            printf(" (unchanged)");
        }
        else
        {
            printf(" (");
            dspRegMask(rsModifiedRegsMask);
            printf(" => ");
            dspRegMask(rsModifiedRegsMask & ~mask);
            printf(")");
        }
        printf("\n");
    }
#endif // DEBUG

    rsModifiedRegsMask &= ~mask;
}

void RegSet::SetMaskVars(regMaskTP newMaskVars)
{
    DBEXEC(m_rsCompiler->verbose,
           m_rsCompiler->GetEmitter()->emitDispRegSetDiff("Live regs: ", _rsMaskVars, newMaskVars);)

    _rsMaskVars = newMaskVars;
}

/*****************************************************************************/

RegSet::RegSet(Compiler* compiler, GCInfo& gcInfo) : m_rsCompiler(compiler), m_rsGCInfo(gcInfo)
{
    /* Initialize the spill logic */

    rsSpillInit();

    /* Initialize the argument register count */
    // TODO-Cleanup: Consider moving intRegState and floatRegState to RegSet.  They used
    // to be initialized here, but are now initialized in the CodeGen constructor.
    // intRegState.rsCurRegArgNum   = 0;
    // loatRegState.rsCurRegArgNum = 0;

    rsMaskResvd = RBM_NONE;

#ifdef TARGET_ARMARCH
    rsMaskCalleeSaved = RBM_NONE;
#endif // TARGET_ARMARCH

#ifdef TARGET_ARM
    rsMaskPreSpillRegArg = RBM_NONE;
    rsMaskPreSpillAlign  = RBM_NONE;
#endif

#ifdef DEBUG
    rsModifiedRegsMaskInitialized = false;
#endif // DEBUG
}

/*****************************************************************************
 *
 *  Finds the SpillDsc corresponding to 'tree' assuming it was spilled from 'reg'.
 */

RegSet::SpillDsc* RegSet::rsGetSpillInfo(GenTree* tree, regNumber reg, SpillDsc** pPrevDsc)
{
    /* Normally, trees are unspilled in the order of being spilled due to
       the post-order walking of trees during code-gen. However, this will
       not be true for something like a GT_ARR_ELEM node */

    SpillDsc* prev;
    SpillDsc* dsc;
    for (prev = nullptr, dsc = rsSpillDesc[reg]; dsc != nullptr; prev = dsc, dsc = dsc->spillNext)
    {
        if (dsc->spillTree == tree)
        {
            break;
        }
    }

    if (pPrevDsc)
    {
        *pPrevDsc = prev;
    }

    return dsc;
}

TempDsc* RegSet::AllocSpillTemp(GenTree* node, regNumber reg, var_types type)
{
    TempDsc* temp = tmpGetTemp(type);

    SpillDsc* spill  = SpillDsc::alloc(this);
    spill->spillTemp = temp;
    spill->spillTree = node;
    spill->spillNext = rsSpillDesc[reg];
    rsSpillDesc[reg] = spill;

    return temp;
}

void RegSet::SpillNodeReg(GenTree* node, unsigned regIndex)
{
    assert((node->gtFlags & GTF_SPILL) != 0);

    bool      isMultiReg = false;
    var_types type       = node->GetType();

    if (node->IsMultiRegCall())
    {
        isMultiReg = true;
        type       = node->AsCall()->GetRegType(regIndex);
    }
    else if (node->IsCall() && (type == TYP_STRUCT))
    {
        type = node->AsCall()->GetRegType(0);
    }
#ifdef TARGET_ARM
    else if (GenTreePutArgSplit* putArgSplit = node->IsPutArgSplit())
    {
        isMultiReg = true;
        type       = putArgSplit->GetRegType(regIndex);
    }
    else if (GenTreeMultiRegOp* multiRegOp = node->IsMultiRegOpLong())
    {
        isMultiReg = true;
        type       = multiRegOp->GetRegType(regIndex);
    }
#endif // TARGET_ARM
    else
    {
        assert(!node->IsMultiRegLclVar());
        assert(!varTypeIsMultiReg(type));
    }

    regNumber reg  = node->GetRegNum(regIndex);
    TempDsc*  temp = AllocSpillTemp(node, reg, type);

    JITDUMP("Spilling register %s after [%06u]\n", m_rsCompiler->compRegVarName(reg), node->GetID());

    m_rsCompiler->codeGen->spillReg(varTypeUsesFloatReg(type) ? type : temp->tdTempType(), temp, reg);

    if (isMultiReg)
    {
        GenTreeFlags regFlags = node->GetRegSpillFlags(regIndex);
        assert((regFlags & GTF_SPILL) != 0);
        regFlags &= ~GTF_SPILL;
        regFlags |= GTF_SPILLED;
        node->SetRegSpillFlags(regIndex, regFlags);
    }
    else
    {
        node->gtFlags &= ~GTF_SPILL;
    }

    node->gtFlags |= GTF_SPILLED;

    m_rsGCInfo.gcMarkRegSetNpt(genRegMask(reg));

    INDEBUG(rsNeededSpillReg = true;)
}

void RegSet::SpillNodeRegs(GenTree* node, unsigned regCount)
{
    for (unsigned i = 0; i < regCount; ++i)
    {
        if ((node->GetRegSpillFlags(i) & GTF_SPILL) != 0)
        {
            SpillNodeReg(node, i);
        }
    }

    node->gtFlags &= ~GTF_SPILL;
}

#ifdef TARGET_X86
void RegSet::SpillST0(GenTree* node)
{
    var_types type = node->GetType();
    regNumber reg  = node->GetRegNum();
    TempDsc*  temp = AllocSpillTemp(node, reg, type);

    JITDUMP("Spilling register ST0 after [%06u]\n", node->GetID());

    m_rsCompiler->codeGen->GetEmitter()->emitIns_S(INS_fstp, emitTypeSize(type), temp->tdTempNum(), 0);

    node->gtFlags &= ~GTF_SPILL;
    node->gtFlags |= GTF_SPILLED;
}
#endif // TARGET_X86

TempDsc* RegSet::UnspillNodeReg(GenTree* node, unsigned regIndex)
{
    regNumber oldReg = node->GetRegNum(regIndex);

    SpillDsc* prevDsc;
    SpillDsc* spillDsc = rsGetSpillInfo(node, oldReg, &prevDsc);
    TempDsc*  temp     = rsGetSpillTempWord(oldReg, spillDsc, prevDsc);

    if (node->IsMultiRegCall() ARM_ONLY(|| node->IsPutArgSplit() || node->IsMultiRegOpLong()))
    {
        GenTreeFlags flags = node->GetRegSpillFlags(regIndex);
        flags &= ~GTF_SPILLED;
        node->SetRegSpillFlags(regIndex, flags);
    }
    else
    {
        assert(!node->IsMultiRegLclVar());
        node->gtFlags &= ~GTF_SPILLED;
    }

    JITDUMP("Unspilling register %s from [%06u]\n", m_rsCompiler->compRegVarName(oldReg), node->GetID());

    return temp;
}

/*****************************************************************************
 *
 *  Get the temp that was spilled from the given register (and free its
 *  spill descriptor while we're at it). Returns the temp (i.e. local var)
 */

TempDsc* RegSet::rsGetSpillTempWord(regNumber reg, SpillDsc* dsc, SpillDsc* prevDsc)
{
    assert((prevDsc == nullptr) || (prevDsc->spillNext == dsc));

    /* Remove this spill entry from the register's list */

    (prevDsc ? prevDsc->spillNext : rsSpillDesc[reg]) = dsc->spillNext;

    /* Remember which temp the value is in */

    TempDsc* temp = dsc->spillTemp;

    SpillDsc::freeDsc(this, dsc);

    /* return the temp variable */

    return temp;
}

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                           TempsInfo                                       XX
XX                                                                           XX
XX  The temporary lclVars allocated by the compiler for code generation      XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

void RegSet::tmpInit()
{
    tmpCount = 0;
    tmpSize  = 0;
#ifdef DEBUG
    tmpGetCount = 0;
#endif

    memset(tmpFree, 0, sizeof(tmpFree));
    memset(tmpUsed, 0, sizeof(tmpUsed));
}

/* static */
var_types RegSet::tmpNormalizeType(var_types type)
{
    type = genActualType(type);

#if defined(FEATURE_SIMD)
    // We always spill SIMD12 to a 16-byte SIMD16 temp.
    // This is because we don't have a single instruction to store 12 bytes, so we want
    // to ensure that we always have the full 16 bytes for loading & storing the value.
    // We also allocate non-argument locals as 16 bytes; see lvSize().
    if (type == TYP_SIMD12)
    {
        type = TYP_SIMD16;
    }
#endif // defined(FEATURE_SIMD) && !defined(TARGET_64BIT)

    return type;
}

/*****************************************************************************
 *
 *  Allocate a temp of the given size (and type, if tracking pointers for
 *  the garbage collector).
 */

TempDsc* RegSet::tmpGetTemp(var_types type)
{
    type          = tmpNormalizeType(type);
    unsigned size = genTypeSize(type);

    // If TYP_STRUCT ever gets in here we do bad things (tmpSlot returns -1)
    noway_assert(size >= sizeof(int));

    /* Find the slot to search for a free temp of the right size */

    unsigned slot = tmpSlot(size);

    /* Look for a temp with a matching type */

    TempDsc** last = &tmpFree[slot];
    TempDsc*  temp;

    for (temp = *last; temp; last = &temp->tdNext, temp = *last)
    {
        /* Does the type match? */

        if (temp->tdTempType() == type)
        {
            /* We have a match -- remove it from the free list */

            *last = temp->tdNext;
            break;
        }
    }

    noway_assert(temp != nullptr);

#ifdef DEBUG
    if (m_rsCompiler->verbose)
    {
        printf("Using temp #%u, slot %u, size = %u\n", -temp->tdTempNum(), slot, temp->tdTempSize());
    }
    tmpGetCount++;
#endif

    temp->tdNext  = tmpUsed[slot];
    tmpUsed[slot] = temp;

    return temp;
}

/*****************************************************************************
 * Preallocate 'count' temps of type 'type'. This type must be a normalized
 * type (by the definition of tmpNormalizeType()).
 *
 * This is used at the end of LSRA, which knows precisely the maximum concurrent
 * number of each type of spill temp needed, before code generation. Code generation
 * then uses these preallocated temp. If code generation ever asks for more than
 * has been preallocated, it is a fatal error.
 */

void RegSet::tmpPreAllocateTemps(var_types type, unsigned count)
{
    assert(type == tmpNormalizeType(type));
    unsigned size = genTypeSize(type);

    // If TYP_STRUCT ever gets in here we do bad things (tmpSlot returns -1)
    noway_assert(size >= sizeof(int));

    // Find the slot to search for a free temp of the right size.
    // Note that slots are shared by types of the identical size (e.g., TYP_REF and TYP_LONG on AMD64),
    // so we can't assert that the slot is empty when we get here.

    unsigned slot = tmpSlot(size);

    for (unsigned i = 0; i < count; i++)
    {
        tmpCount++;
        tmpSize += size;

#ifdef TARGET_ARM
        if (type == TYP_DOUBLE)
        {
            // Adjust tmpSize to accommodate possible alignment padding.
            // Note that at this point the offsets aren't yet finalized, so we don't yet know if it will be required.
            tmpSize += TARGET_POINTER_SIZE;
        }
#endif // TARGET_ARM

        TempDsc* temp = new (m_rsCompiler, CMK_Unknown) TempDsc(-((int)tmpCount), size, type);

#ifdef DEBUG
        if (m_rsCompiler->verbose)
        {
            printf("pre-allocated temp #%u, slot %u, size = %u\n", -temp->tdTempNum(), slot, temp->tdTempSize());
        }
#endif // DEBUG

        // Add it to the front of the appropriate slot list.
        temp->tdNext  = tmpFree[slot];
        tmpFree[slot] = temp;
    }
}

/*****************************************************************************
 *
 *  Release the given temp.
 */

void RegSet::tmpRlsTemp(TempDsc* temp)
{
    assert(temp != nullptr);

    unsigned slot;

    /* Add the temp to the 'free' list */

    slot = tmpSlot(temp->tdTempSize());

#ifdef DEBUG
    if (m_rsCompiler->verbose)
    {
        printf("release temp #%u, slot %u, size = %u\n", -temp->tdTempNum(), slot, temp->tdTempSize());
    }
    assert(tmpGetCount);
    tmpGetCount--;
#endif

    // Remove it from the 'used' list.

    TempDsc** last = &tmpUsed[slot];
    TempDsc*  t;
    for (t = *last; t != nullptr; last = &t->tdNext, t = *last)
    {
        if (t == temp)
        {
            /* Found it! -- remove it from the 'used' list */

            *last = t->tdNext;
            break;
        }
    }
    assert(t != nullptr); // We better have found it!

    // Add it to the free list.

    temp->tdNext  = tmpFree[slot];
    tmpFree[slot] = temp;
}

/*****************************************************************************
 *  Given a temp number, find the corresponding temp.
 *
 *  When looking for temps on the "free" list, this can only be used after code generation. (This is
 *  simply because we have an assert to that effect in tmpListBeg(); we could relax that, or hoist
 *  the assert to the appropriate callers.)
 *
 *  When looking for temps on the "used" list, this can be used any time.
 */
TempDsc* RegSet::tmpFindNum(int tnum, TEMP_USAGE_TYPE usageType /* = TEMP_USAGE_FREE */) const
{
    assert(tnum < 0); // temp numbers are negative

    for (TempDsc* temp = tmpListBeg(usageType); temp != nullptr; temp = tmpListNxt(temp, usageType))
    {
        if (temp->tdTempNum() == tnum)
        {
            return temp;
        }
    }

    return nullptr;
}

/*****************************************************************************
 *
 *  A helper function is used to iterate over all the temps.
 */

TempDsc* RegSet::tmpListBeg(TEMP_USAGE_TYPE usageType /* = TEMP_USAGE_FREE */) const
{
    TempDsc* const* tmpLists;
    if (usageType == TEMP_USAGE_FREE)
    {
        tmpLists = tmpFree;
    }
    else
    {
        tmpLists = tmpUsed;
    }

    // Return the first temp in the slot for the smallest size
    unsigned slot = 0;
    while (slot < (TEMP_SLOT_COUNT - 1) && tmpLists[slot] == nullptr)
    {
        slot++;
    }
    TempDsc* temp = tmpLists[slot];

    return temp;
}

/*****************************************************************************
 * Used with tmpListBeg() to iterate over the list of temps.
 */

TempDsc* RegSet::tmpListNxt(TempDsc* curTemp, TEMP_USAGE_TYPE usageType /* = TEMP_USAGE_FREE */) const
{
    assert(curTemp != nullptr);

    TempDsc* temp = curTemp->tdNext;
    if (temp == nullptr)
    {
        unsigned size = curTemp->tdTempSize();

        // If there are no more temps in the list, check if there are more
        // slots (for bigger sized temps) to walk.

        TempDsc* const* tmpLists;
        if (usageType == TEMP_USAGE_FREE)
        {
            tmpLists = tmpFree;
        }
        else
        {
            tmpLists = tmpUsed;
        }

        while (size < TEMP_MAX_SIZE && temp == nullptr)
        {
            size += sizeof(int);
            unsigned slot = tmpSlot(size);
            temp          = tmpLists[slot];
        }

        assert((temp == nullptr) || (temp->tdTempSize() == size));
    }

    return temp;
}

#ifdef DEBUG
/*****************************************************************************
 * Return 'true' if all allocated temps are free (not in use).
 */
bool RegSet::tmpAllFree() const
{
    // The 'tmpGetCount' should equal the number of things in the 'tmpUsed' lists. This is a convenient place
    // to assert that.
    unsigned usedCount = 0;
    for (TempDsc* temp = tmpListBeg(TEMP_USAGE_USED); temp != nullptr; temp = tmpListNxt(temp, TEMP_USAGE_USED))
    {
        ++usedCount;
    }
    assert(usedCount == tmpGetCount);

    if (tmpGetCount != 0)
    {
        return false;
    }

    for (unsigned i = 0; i < _countof(tmpUsed); i++)
    {
        if (tmpUsed[i] != nullptr)
        {
            return false;
        }
    }

    return true;
}

#endif // DEBUG

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX  Register-related utility functions                                       XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

/*****************************************************************************
 *
 *  Given a register that is an argument register
 *   returns the next argument register
 *
 *  Note: that this method will return a non arg register
 *   when given REG_ARG_LAST
 *
 */

regNumber genRegArgNext(regNumber argReg)
{
    assert(isValidIntArgReg(argReg) || isValidFloatArgReg(argReg));

    switch (argReg)
    {

#ifdef TARGET_AMD64
#ifdef UNIX_AMD64_ABI

        // Linux x64 ABI: REG_RDI, REG_RSI, REG_RDX, REG_RCX, REG_R8, REG_R9
        case REG_ARG_0:       // REG_RDI
            return REG_ARG_1; // REG_RSI
        case REG_ARG_1:       // REG_RSI
            return REG_ARG_2; // REG_RDX
        case REG_ARG_2:       // REG_RDX
            return REG_ARG_3; // REG_RCX
        case REG_ARG_3:       // REG_RCX
            return REG_ARG_4; // REG_R8

#else // !UNIX_AMD64_ABI

        // Windows x64 ABI: REG_RCX, REG_RDX, REG_R8, REG_R9
        case REG_ARG_1:       // REG_RDX
            return REG_ARG_2; // REG_R8

#endif // !UNIX_AMD64_ABI
#endif // TARGET_AMD64

        default:
            return REG_NEXT(argReg);
    }
}

/*****************************************************************************
 *
 *  The following table determines the order in which callee-saved registers
 *  are encoded in GC information at call sites (perhaps among other things).
 *  In any case, they establish a mapping from ordinal callee-save reg "indices" to
 *  register numbers and corresponding bitmaps.
 */

const regNumber raRegCalleeSaveOrder[] = {REG_CALLEE_SAVED_ORDER};
const regMaskTP raRbmCalleeSaveOrder[] = {RBM_CALLEE_SAVED_ORDER};

regMaskSmall genRegMaskFromCalleeSavedMask(unsigned short calleeSaveMask)
{
    regMaskSmall res = 0;
    for (int i = 0; i < CNT_CALLEE_SAVED; i++)
    {
        if ((calleeSaveMask & ((regMaskTP)1 << i)) != 0)
        {
            res |= raRbmCalleeSaveOrder[i];
        }
    }
    return res;
}

/*****************************************************************************
 *
 *  Initializes the spill code. Should be called once per function compiled.
 */

// inline
void RegSet::rsSpillInit()
{
    /* Clear out the spill and multi-use tables */

    memset(rsSpillDesc, 0, sizeof(rsSpillDesc));

    INDEBUG(rsNeededSpillReg = false;)

    /* We don't have any descriptors allocated */

    rsSpillFree = nullptr;
}

/*****************************************************************************
 *
 *  Shuts down the spill code. Should be called once per function compiled.
 */

// inline
void RegSet::rsSpillDone()
{
    rsSpillChk();
}

/*****************************************************************************
 *
 *  Begin tracking spills - should be called each time before a pass is made
 *  over a function body.
 */

// inline
void RegSet::rsSpillBeg()
{
    rsSpillChk();
}

/*****************************************************************************
 *
 *  Finish tracking spills - should be called each time after a pass is made
 *  over a function body.
 */

// inline
void RegSet::rsSpillEnd()
{
    rsSpillChk();
}

//****************************************************************************
//  Create a new SpillDsc or get one off the free list
//

// inline
RegSet::SpillDsc* RegSet::SpillDsc::alloc(RegSet* regSet)
{
    RegSet::SpillDsc*  spill;
    RegSet::SpillDsc** pSpill;

    pSpill = &(regSet->rsSpillFree);

    // Allocate spill structure
    if (*pSpill)
    {
        spill   = *pSpill;
        *pSpill = spill->spillNext;
    }
    else
    {
        spill = regSet->m_rsCompiler->getAllocator().allocate<SpillDsc>(1);
    }
    return spill;
}

//****************************************************************************
//  Free a SpillDsc and return it to the rsSpillFree list
//

// inline
void RegSet::SpillDsc::freeDsc(RegSet* regSet, RegSet::SpillDsc* spillDsc)
{
    spillDsc->spillNext = regSet->rsSpillFree;
    regSet->rsSpillFree = spillDsc;
}

/*****************************************************************************
 *
 *  Make sure no spills are currently active - used for debugging of the code
 *  generator.
 */

#ifdef DEBUG

// inline
void RegSet::rsSpillChk()
{
    // All grabbed temps should have been released
    assert(tmpGetCount == 0);

    for (regNumber reg = REG_FIRST; reg < REG_COUNT; reg = REG_NEXT(reg))
    {
        assert(rsSpillDesc[reg] == nullptr);
    }
}

#else

// inline
void RegSet::rsSpillChk()
{
}

#endif
