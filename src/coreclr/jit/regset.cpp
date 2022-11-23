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

void RegSet::rsSetRegsModified(regMaskTP mask DEBUGARG(bool suppressDump))
{
    assert(mask != RBM_NONE);

    // We can't update the modified registers set after final frame layout (that is, during code
    // generation and after). Ignore prolog and epilog generation: they call register tracking to
    // modify rbp, for example, even in functions that use rbp as a frame pointer. Make sure normal
    // code generation isn't actually adding to set of modified registers.
    // Frame layout is only affected by callee-saved registers, so only ensure that callee-saved
    // registers aren't modified after final frame layout.
    assert((m_rsCompiler->lvaDoneFrameLayout < Compiler::FINAL_FRAME_LAYOUT) ||
           m_rsCompiler->codeGen->generatingProlog || m_rsCompiler->codeGen->generatingEpilog ||
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

    // See comment in rsSetRegsModified().
    assert((m_rsCompiler->lvaDoneFrameLayout < Compiler::FINAL_FRAME_LAYOUT) ||
           m_rsCompiler->codeGen->generatingProlog || m_rsCompiler->codeGen->generatingEpilog ||
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

// Finds the SpillDsc corresponding to 'tree' assuming it was spilled from 'reg'.
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

var_types RegSet::tmpNormalizeType(var_types type)
{
#ifdef FEATURE_SIMD
    if (type == TYP_SIMD12)
    {
        return TYP_SIMD16;
    }
#endif

    return varActualType(type);
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

// Preallocate 'count' temps of type 'type'. This type must be a normalized
// type (by the definition of tmpNormalizeType()).
//
// This is used at the end of LSRA, which knows precisely the maximum concurrent
// number of each type of spill temp needed, before code generation. Code generation
// then uses these preallocated temp. If code generation ever asks for more than
// has been preallocated, it is a fatal error.
void RegSet::tmpPreAllocateTemps(var_types type, unsigned count)
{
    assert(type == tmpNormalizeType(type));
    unsigned size = varTypeSize(type);

    // If TYP_STRUCT ever gets in here we do bad things (tmpSlot returns -1)
    noway_assert(size >= sizeof(int));

    // Find the slot to search for a free temp of the right size.
    // Note that slots are shared by types of the identical size (e.g., TYP_REF and TYP_LONG on AMD64),
    // so we can't assert that the slot is empty when we get here.

    unsigned slot = tmpSlot(size);

    for (unsigned i = 0; i < count; i++)
    {
        tmpCount++;

        TempDsc* temp = new (m_rsCompiler, CMK_Unknown) TempDsc(-((int)tmpCount), size, type);
        temp->tdNext  = tmpFree[slot];
        tmpFree[slot] = temp;

        JITDUMP("pre-allocated temp #%u, slot %u, size = %u\n", -temp->tdTempNum(), slot, temp->tdTempSize());
    }
}

// Release the given temp.
void RegSet::tmpRlsTemp(TempDsc* temp)
{
    assert(temp != nullptr);

    // Add the temp to the 'free' list

    unsigned slot = tmpSlot(temp->tdTempSize());

    JITDUMP("release temp #%u, slot %u, size = %u\n", -temp->tdTempNum(), slot, temp->tdTempSize());
    assert(tmpGetCount != 0);
    INDEBUG(tmpGetCount--);

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

unsigned RegSet::tmpSlot(unsigned size)
{
    noway_assert(size >= sizeof(int));
    noway_assert(size <= TEMP_MAX_SIZE);
    assert((size % sizeof(int)) == 0);

    assert(size < UINT32_MAX);
    return size / sizeof(int) - 1;
}

void RegSet::tmpEnd()
{
#ifdef DEBUG
    if (m_rsCompiler->verbose && (tmpCount > 0))
    {
        printf("%d tmps used\n", tmpCount);
    }
#endif
}

void RegSet::tmpDone()
{
#ifdef DEBUG
    unsigned count;
    TempDsc* temp;

    assert(tmpAllFree());
    for (temp = tmpListBeg(), count = temp ? 1 : 0; temp; temp = tmpListNxt(temp), count += temp ? 1 : 0)
    {
        assert(temp->tdLegalOffset());
    }

    // Make sure that all the temps were released
    assert(count == tmpCount);
    assert(tmpGetCount == 0);
#endif
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

// The following table determines the order in which callee-saved registers
// are encoded in GC information at call sites (perhaps among other things).
// In any case, they establish a mapping from ordinal callee-save reg "indices" to
// register numbers and corresponding bitmaps.
const regMaskTP raRbmCalleeSaveOrder[]{RBM_CALLEE_SAVED_ORDER};

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
