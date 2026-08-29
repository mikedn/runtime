// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

class BasicBlock;
class Compiler;

// The following holds the table of exception handlers.

enum EHHandlerType
{
    EH_HANDLER_CATCH = 0x1, // Don't use zero (to aid debugging uninitialized memory)
    EH_HANDLER_FILTER,
    EH_HANDLER_FAULT,
    EH_HANDLER_FINALLY,
    EH_HANDLER_FAULT_WAS_FINALLY
};

// ToCORINFO_EH_CLAUSE_FLAGS: Convert an internal EHHandlerType to a CORINFO_EH_CLAUSE_FLAGS value
// to pass back to the VM.
inline CORINFO_EH_CLAUSE_FLAGS ToCORINFO_EH_CLAUSE_FLAGS(EHHandlerType type)
{
    switch (type)
    {
    case EH_HANDLER_CATCH:
        return CORINFO_EH_CLAUSE_NONE;
    case EH_HANDLER_FILTER:
        return CORINFO_EH_CLAUSE_FILTER;
    case EH_HANDLER_FAULT:
    case EH_HANDLER_FAULT_WAS_FINALLY:
        return CORINFO_EH_CLAUSE_FAULT;
    case EH_HANDLER_FINALLY:
        return CORINFO_EH_CLAUSE_FINALLY;
    default:
        unreached();
    }
}

// ToEHHandlerType: Convert a CORINFO_EH_CLAUSE_FLAGS value obtained from the VM in the EH clause structure
// to the internal EHHandlerType type.
inline EHHandlerType ToEHHandlerType(CORINFO_EH_CLAUSE_FLAGS flags)
{
    if (flags & CORINFO_EH_CLAUSE_FAULT)
    {
        return EH_HANDLER_FAULT;
    }
    else if (flags & CORINFO_EH_CLAUSE_FINALLY)
    {
        return EH_HANDLER_FINALLY;
    }
    else if (flags & CORINFO_EH_CLAUSE_FILTER)
    {
        return EH_HANDLER_FILTER;
    }
    else
    {
        // If it's none of the others, assume it is a try/catch.
        /* XXX Fri 11/7/2008
         * The VM (and apparently VC) stick in extra bits in the flags field. We ignore any flags
         * we don't know about.
         */
        return EH_HANDLER_CATCH;
    }
}

struct EHblkDsc
{
    BasicBlock* ebdTryBeg;  // First block of the try
    BasicBlock* ebdTryLast; // Last block of the try
    BasicBlock* ebdHndBeg;  // First block of the handler
    BasicBlock* ebdHndLast; // Last block of the handler
    union {
        BasicBlock* ebdFilter; // First block of filter,          if HasFilter()
        unsigned    ebdTyp;    // Exception type (a class token), otherwise
    };

    EHHandlerType ebdHandlerType;

#ifndef FEATURE_EH_FUNCLETS
    // How nested is the try/handler within other *handlers* - 0 for outermost clauses,
    // 1 for nesting with a handler, etc.
    uint16_t ebdHandlerNestingLevel;
#endif

    static const uint16_t NO_ENCLOSING_INDEX = UINT16_MAX;

    // The index of the enclosing outer try region, NO_ENCLOSING_INDEX if none.
    // Be careful of 'mutually protect' catch and filter clauses (multiple
    // handlers with the same try region): the try regions 'nest' so we set
    // ebdEnclosingTryIndex, but the inner catch is *NOT* nested within the outer catch!
    // That is, if the "inner catch" throws an exception, it won't be caught by
    // the "outer catch" for mutually protect handlers.
    uint16_t ebdEnclosingTryIndex;

    // The index of the enclosing outer handler region, NO_ENCLOSING_INDEX if none.
    uint16_t ebdEnclosingHndIndex;

#ifdef FEATURE_EH_FUNCLETS
    // After funclets are created, this is the index of corresponding FuncInfoDsc
    // Special case for Filter/Filter-handler:
    //   Like the IL the filter funclet immediately precedes the filter-handler funclet.
    //   So this index points to the filter-handler funclet. If you want the filter
    //   funclet index, just subtract 1.
    uint16_t ebdFuncIndex;
#endif

    IL_OFFSET ebdTryBegOffset; // IL offsets of EH try/end regions as they are imported
    IL_OFFSET ebdTryEndOffset;
    IL_OFFSET ebdFilterBegOffset; // only set if HasFilter()
    IL_OFFSET ebdHndBegOffset;
    IL_OFFSET ebdHndEndOffset;

    // Returns the last block of the filter. Assumes the EH clause is a try/filter/filter-handler type.
    BasicBlock* BBFilterLast() const;

    bool HasCatchHandler() const;
    bool HasFilter() const;
    bool HasFinallyHandler() const;
    bool HasFaultHandler() const;
    bool HasFinallyOrFaultHandler() const;

    // Returns the block to which control will flow if an (otherwise-uncaught) exception is raised
    // in the try.  This is normally "ebdHndBeg", unless the try region has a filter, in which case that is returned.
    // (This is, in some sense, the "true handler," at least in the sense of control flow.  Note
    // that we model the transition from a filter to its handler as normal, non-exceptional control flow.)
    BasicBlock* ExFlowBlock() const;

    bool InTryRegionILRange(BasicBlock* pBlk) const;
    bool InFilterRegionILRange(BasicBlock* pBlk) const;
    bool InHndRegionILRange(BasicBlock* pBlk) const;

    bool InTryRegionBBRange(BasicBlock* pBlk) const;
    bool InFilterRegionBBRange(BasicBlock* pBlk) const;
    bool InHndRegionBBRange(BasicBlock* pBlk) const;

    IL_OFFSET ebdTryBegOffs() const;
    IL_OFFSET ebdTryEndOffs() const;
    IL_OFFSET ebdFilterBegOffs() const;
    IL_OFFSET ebdFilterEndOffs() const;
    IL_OFFSET ebdHndBegOffs() const;
    IL_OFFSET ebdHndEndOffs() const;

    static bool ebdIsSameILTry(EHblkDsc* h1, EHblkDsc* h2); // Same 'try' region? Compare IL range.

    // Return the region index of the most nested EH region that encloses this region, or NO_ENCLOSING_INDEX
    // if this region is directly in the main function body. Set '*inTryRegion' to 'true' if this region is
    // most nested within a 'try' region, or 'false' if this region is most nested within a handler. (Note
    // that filters cannot contain nested EH regions.)
    unsigned ebdGetEnclosingRegionIndex(bool* inTryRegion) const;

    static bool ebdIsSameTry(EHblkDsc* h1, EHblkDsc* h2); // Same 'try' region? Compare begin/last blocks.
    bool ebdIsSameTry(Compiler* comp, unsigned t2);
    bool ebdIsSameTry(BasicBlock* ebdTryBeg, BasicBlock* ebdTryLast) const;

#ifdef DEBUG
    void DispEntry(unsigned num) const; // Display this table entry
#endif

private:
    static bool InBBRange(BasicBlock* pBlk, BasicBlock* pStart, BasicBlock* pEnd);
};
