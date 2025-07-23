
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

// LsraLocation tracks the linearized order of the nodes.
// Each node is assigned two LsraLocations - one for all the uses and all but the last
// def, and a second location for the last def (if any)

using LsraLocation = unsigned;

constexpr LsraLocation MinLocation = 0;
constexpr LsraLocation MaxLocation = UINT_MAX;

using RegisterType = var_types;

constexpr RegisterType IntRegisterType   = TYP_INT;
constexpr RegisterType FloatRegisterType = TYP_FLOAT;

inline regMaskTP calleeSaveRegs(RegisterType rt)
{
    return varTypeIsIntegralOrI(rt) ? RBM_INT_CALLEE_SAVED : RBM_FLT_CALLEE_SAVED;
}

inline regMaskTP callerSaveRegs(RegisterType rt)
{
    return varTypeIsIntegralOrI(rt) ? RBM_INT_CALLEE_TRASH : RBM_FLT_CALLEE_TRASH;
}

class Interval;
class RefPosition;

struct RefInfoListNode
{
    RefPosition*     ref;
    GenTree*         node;
    RefInfoListNode* next;
};

class RefInfoList final
{
    friend class RefInfoListNodePool;

    RefInfoListNode* head     = nullptr;
    RefInfoListNode* tail     = nullptr;
    RefInfoListNode* freeList = nullptr;

public:
    RefInfoListNode* Begin() const
    {
        return head;
    }

    void Add(RefPosition* ref, GenTree* node, Compiler* compiler);
    RefPosition* Remove(GenTree* node, unsigned regIndex);

#ifdef DEBUG
    bool IsEmpty() const
    {
        return head == nullptr;
    }

    unsigned Count() const
    {
        unsigned count = 0;
        for (RefInfoListNode* def = head; def != nullptr; def = def->next)
        {
            count++;
        }
        return count;
    }
#endif // DEBUG

private:
    RefInfoListNode* AllocDef(RefPosition* ref, GenTree* node, Compiler* compiler);
    void Unlink(RefInfoListNode* def, RefInfoListNode* prevDef);
    void FreeDef(RefInfoListNode* def);
};

#if TRACK_LSRA_STATS
enum LsraStat : unsigned
{
#define LSRA_STAT_DEF(enum_name, enum_str) enum_name,
#include "lsra_stats.h"
#undef LSRA_STAT_DEF
#define REG_SEL_DEF(name, ...) STAT_##name,
#include "lsra_score.h"
    COUNT
};

struct LsraBlockStats
{
    unsigned stats[LsraStat::COUNT];
};
#endif // TRACK_LSRA_STATS

struct LsraBlockInfo
{
    // The block to use as predecessor to use for the register location of live-in variables.
    // null for fgFirstBB.
    BasicBlock*          predBlock;
    BasicBlock::weight_t weight;
    bool                 hasCriticalInEdge : 1;
    bool                 hasCriticalOutEdge : 1;
    bool                 hasEHBoundaryIn : 1;
    bool                 hasEHBoundaryOut : 1;
    bool                 hasEHPred : 1;
};

enum RegisterScore
{
    NONE = 0,
#define REG_SEL_DEF(name, score, ...) name = score,
#include "lsra_score.h"
};

// This is sort of a bit mask
// The low order 2 bits will be 1 for defs, and 2 for uses
enum RefType : uint8_t
{
#define DEF_REFTYPE(memberName, memberValue, shortName) memberName = memberValue,
#include "lsra_reftypes.h"
#undef DEF_REFTYPE
};

inline bool RefTypeIsUse(RefType refType)
{
    return (refType & RefTypeUse) == RefTypeUse;
}

inline bool RefTypeIsDef(RefType refType)
{
    return (refType & RefTypeDef) == RefTypeDef;
}

class Referenceable
{
public:
    RefPosition* firstRefPosition  = nullptr;
    RefPosition* recentRefPosition = nullptr;
    RefPosition* lastRefPosition   = nullptr;

    // A linked list of RefPositions.  These are only traversed in the forward
    // direction, and are not moved, so they don't need to be doubly linked
    // (see RefPosition).

    // Get the position of the next reference which is at or greater than
    // the current location (relies upon recentRefPosition being updated
    // during traversal).
    RefPosition* getNextRefPosition() const;
    LsraLocation getNextRefLocation() const;

    void LinkRefPosition(RefPosition* ref);
};

class RegRecord : public Referenceable
{
public:
    // interval to which this register is currently allocated.
    // If the interval is inactive (isActive == false) then it is not currently live,
    // and the register can be unassigned (i.e. setting assignedInterval to nullptr)
    // without spilling the register.
    Interval* assignedInterval = nullptr;
    // Interval to which this register was previously allocated, and which was unassigned
    // because it was inactive.  This register will be reassigned to this Interval when
    // assignedInterval becomes inactive.
    Interval* previousInterval = nullptr;

    RegNum   regNum;
    unsigned regOrder;
    float    spillCost;

    RegRecord(RegNum reg) : regNum(reg)
    {
        assert((REG_FIRST <= reg) && (reg <= REG_LAST));
    }

    RegRecord(const RegRecord&) = delete;
    RegRecord& operator=(const RegRecord&) = delete;

    RegisterType registerType() const
    {
        return IsFloatReg(regNum) ? FloatRegisterType : IntRegisterType;
    }

    bool IsAssigned(ARM_ONLY(RegisterType newRegType)) const;

#ifdef TARGET_ARM
    RegRecord* GetDoublePairNextReg() const
    {
        assert(genIsValidDoubleReg(regNum));
        // We assume that all RegRecord objects are stored in an array.
        return const_cast<RegRecord*>(this + 1);
    }

    RegRecord* GetDoublePairPrevReg() const
    {
        assert(genIsValidFloatReg(regNum) && genIsValidDoubleReg(REG_PREV(regNum)));
        assert(this[-1].regNum == REG_PREV(regNum));
        // We assume that all RegRecord objects are stored in an array.
        return const_cast<RegRecord*>(this - 1);
    }

    RegRecord* GetDoublePairOtherReg() const
    {
        return genIsValidDoubleReg(regNum) ? GetDoublePairNextReg() : GetDoublePairPrevReg();
    }
#endif

#ifdef DEBUG
    void dump() const;
    void tinyDump() const;
#endif
};

class RegisterSelection;

using VarToRegMap                = RegNumSmall*;
using IntervalList               = jitstd::list<Interval>;
using RefPositionList            = jitstd::list<RefPosition>;
using RefPositionIterator        = jitstd::list<RefPosition>::iterator;
using RefPositionReverseIterator = jitstd::list<RefPosition>::reverse_iterator;

// OPTION 1: The algorithm as described in "Optimized Interval Splitting in a
// Linear Scan Register Allocator".  It is driven by iterating over the Interval
// lists.  In this case, we need multiple IntervalLists, and Intervals will be
// moved between them so they must be easily updated.

// OPTION 2: The algorithm is driven by iterating over the RefPositions.  In this
// case, we only need a single IntervalList, and it won't be updated.
// The RefPosition must refer to its Interval, and we need to be able to traverse
// to the next RefPosition in code order
// THIS IS THE OPTION CURRENTLY BEING PURSUED

class LinearScan
{
    friend class RefPosition;
    friend class Interval;
    friend class RegisterSelection;

    Compiler* const      compiler;
    PhasedVar<regMaskTP> availableRegs{RBM_ALLINT | RBM_ALLFLOAT};

#ifdef DEBUG
    const bool verbose;
#endif

public:
    LinearScan(Compiler* compiler);

    void Run();

    regMaskTP GetAllocatedRegs() const
    {
        return m_allocateRegs;
    }

    const unsigned* GetTypeSpillCounts() const
    {
        return maxSpill;
    }

#ifdef TARGET_ARMARCH
    regMaskTP GetReservedRegs() const
    {
        return m_reservedRegs;
    }
#endif

    VarToRegMap GetBlockLiveInRegMap(BasicBlock* bb) const;

#if TRACK_LSRA_STATS
    void dumpLsraStatsCsv(FILE* file) const;
    void dumpLsraStatsSummary(FILE* file) const;
#endif

private:
    static bool isSingleRegister(regMaskTP regMask)
    {
        return genExactlyOneBit(regMask);
    }

    void buildIntervals();
    void identifyCandidates();
    bool IsRegCandidate(LclVarDsc* lcl);
    void identifyCandidatesExceptionDataflow();

#ifdef DEBUG
    void checkLastUses(BasicBlock* block);
    unsigned GetRegisterDstCount(GenTree* node) const;
    unsigned ComputeOperandDstCount(GenTree* operand) const;
    unsigned ComputeAvailableSrcCount(GenTree* node) const;
#endif

    void setFrameType();

    // This is where the actual assignment is done
    void allocateRegisters();

    // This is the resolution phase, where cross-block mismatches are fixed up
    void resolveRegisters();

    void writeRegisters(RefPosition* currentRefPosition, GenTree* node);

    // Insert a copy in the case where a value node must be moved to a different
    // register at the point of use, or it is reloaded to a different register
    // than the one it was spilled from
    void insertCopyOrReload(BasicBlock* block, GenTree* value, unsigned regIndex, RefPosition* refPosition);

#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
    void makeUpperVectorInterval(unsigned varIndex);
    Interval* getUpperVectorInterval(unsigned varIndex);

    // Save the upper half of a vector that lives in a callee-save register at the point of a call.
    void InsertUpperVectorSpill(GenTree*     node,
                                RefPosition* refPosition,
                                Interval*    upperVectorInterval,
                                BasicBlock*  block);
    // Restore the upper half of a vector that's been partially spilled prior to a use in 'node'.
    void InsertUpperVectorUnspill(GenTree*     node,
                                  RefPosition* refPosition,
                                  Interval*    upperVectorInterval,
                                  BasicBlock*  block);
#endif // FEATURE_PARTIAL_SIMD_CALLEE_SAVE

    // resolve along one block-block edge
    enum ResolveType
    {
        ResolveSplit,
        ResolveJoin,
        ResolveCritical,
        ResolveSharedCritical,
        ResolveTypeCount
    };

#ifdef TARGET_ARM
    void InsertDoubleRegCopy(BasicBlock*  block,
                             GenTree*     insertionPoint,
                             Interval**   intervals,
                             RegNumSmall* location,
                             RegNum       toReg,
                             RegNum fromReg DEBUG_ARG(ResolveType resolveType));
#endif
    void handleOutgoingCriticalEdges(BasicBlock* block, VARSET_TP outResolutionSet);
    void resolveEdge(BasicBlock* fromBlock, BasicBlock* toBlock, ResolveType resolveType, VARSET_TP liveSet);
    void resolveEdges();

    // Keep track of how many temp locations we'll need for spill
    void updateMaxSpill(RefPosition* refPosition);
    void recordMaxSpill();

#ifdef DEBUG
    //------------------------------------------------------------------------
    // Should we stress lsra? This uses the COMPlus_JitStressRegs variable.
    //
    // The mask bits are currently divided into fields in which each non-zero value
    // is a distinct stress option (e.g. 0x3 is not a combination of 0x1 and 0x2).
    // However, subject to possible constraints (to be determined), the different
    // fields can be combined (e.g. 0x7 is a combination of 0x3 and 0x4).
    // Note that the field values are declared in a public enum, but the actual bits are
    // only accessed via accessors.

    unsigned lsraStressMask;

    // This controls the registers available for allocation
    enum LsraStressLimitRegs
    {
        LSRA_LIMIT_NONE      = 0,
        LSRA_LIMIT_CALLEE    = 0x1,
        LSRA_LIMIT_CALLER    = 0x2,
        LSRA_LIMIT_SMALL_SET = 0x3,
        LSRA_LIMIT_MASK      = 0x3
    };

    // When LSRA_LIMIT_SMALL_SET is specified, it is desirable to select a "mixed" set of caller- and callee-save
    // registers, so as to get different coverage than limiting to callee or caller.
    // At least for x86 and AMD64, and potentially other architecture that will support SIMD,
    // we need a minimum of 5 fp regs in order to support the InitN intrinsic for Vector4.
    // Hence the "SmallFPSet" has 5 elements.
    CLANG_FORMAT_COMMENT_ANCHOR;

#if defined(TARGET_AMD64)
    static const regMaskTP LsraLimitSmallIntSet =
        RBM_EAX | RBM_ECX | RBM_EBX | RBM_ETW_FRAMED_EBP |
#ifdef UNIX_AMD64_ABI
        // On System V the RDI and RSI are not callee saved. Use R12 ans R13 as callee saved registers.
        RBM_R12 | RBM_R13;
#else
        // On Windows Amd64 use the RDI and RSI as callee saved registers.
        RBM_ESI | RBM_EDI;
#endif
    static const regMaskTP LsraLimitSmallFPSet = RBM_XMM0 | RBM_XMM1 | RBM_XMM2 | RBM_XMM6 | RBM_XMM7;
#elif defined(TARGET_ARM)
    // On ARM, we may need two registers to set up the target register for a virtual call,
    // so we need to have at least the maximum number of arg registers, plus 2.
    static const regMaskTP LsraLimitSmallIntSet = RBM_R0 | RBM_R1 | RBM_R2 | RBM_R3 | RBM_R4 | RBM_R5;
    static const regMaskTP LsraLimitSmallFPSet  = RBM_F0 | RBM_F1 | RBM_F2 | RBM_F16 | RBM_F17;
#elif defined(TARGET_ARM64)
    static const regMaskTP LsraLimitSmallIntSet = RBM_R0 | RBM_R1 | RBM_R2 | RBM_R19 | RBM_R20;
    static const regMaskTP LsraLimitSmallFPSet  = RBM_V0 | RBM_V1 | RBM_V2 | RBM_V8 | RBM_V9;
#elif defined(TARGET_X86)
    static const regMaskTP LsraLimitSmallIntSet = RBM_EAX | RBM_ECX | RBM_EDI;
    static const regMaskTP LsraLimitSmallFPSet  = RBM_XMM0 | RBM_XMM1 | RBM_XMM2 | RBM_XMM6 | RBM_XMM7;
#else
#error Unsupported or unset target architecture
#endif

    LsraStressLimitRegs getStressLimitRegs() const
    {
        return static_cast<LsraStressLimitRegs>(lsraStressMask & LSRA_LIMIT_MASK);
    }

    static regMaskTP getConstrainedRegMask(regMaskTP regMaskActual, regMaskTP regMaskConstrain, unsigned minRegCount);
    regMaskTP stressLimitRegs(RefPosition* refPosition, regMaskTP mask);

    // This controls the heuristics used to select registers
    // These can be combined.
    enum LsraSelect
    {
        LSRA_SELECT_DEFAULT               = 0,
        LSRA_SELECT_REVERSE_HEURISTICS    = 0x04,
        LSRA_SELECT_REVERSE_CALLER_CALLEE = 0x08,
        LSRA_SELECT_NEAREST               = 0x10,
        LSRA_SELECT_MASK                  = 0x1c
    };

    LsraSelect getSelectionHeuristics() const
    {
        return static_cast<LsraSelect>(lsraStressMask & LSRA_SELECT_MASK);
    }

    bool doReverseSelect() const
    {
        return (lsraStressMask & LSRA_SELECT_REVERSE_HEURISTICS) != 0;
    }

    bool doReverseCallerCallee() const
    {
        return (lsraStressMask & LSRA_SELECT_REVERSE_CALLER_CALLEE) != 0;
    }

    bool doSelectNearest() const
    {
        return (lsraStressMask & LSRA_SELECT_NEAREST) != 0;
    }

    // This controls the order in which basic blocks are visited during allocation
    enum LsraTraversalOrder
    {
        LSRA_TRAVERSE_LAYOUT     = 0x20,
        LSRA_TRAVERSE_PRED_FIRST = 0x40,
        LSRA_TRAVERSE_RANDOM     = 0x60, // NYI
        LSRA_TRAVERSE_DEFAULT    = LSRA_TRAVERSE_PRED_FIRST,
        LSRA_TRAVERSE_MASK       = 0x60
    };

    LsraTraversalOrder getLsraTraversalOrder() const
    {
        if ((lsraStressMask & LSRA_TRAVERSE_MASK) == 0)
        {
            return LSRA_TRAVERSE_DEFAULT;
        }

        return static_cast<LsraTraversalOrder>(lsraStressMask & LSRA_TRAVERSE_MASK);
    }

    bool isTraversalLayoutOrder() const
    {
        return getLsraTraversalOrder() == LSRA_TRAVERSE_LAYOUT;
    }

    bool isTraversalPredFirstOrder() const
    {
        return getLsraTraversalOrder() == LSRA_TRAVERSE_PRED_FIRST;
    }

    // This controls whether lifetimes should be extended to the entire method.
    // Note that this has no effect under MinOpts
    enum LsraExtendLifetimes
    {
        LSRA_DONT_EXTEND           = 0,
        LSRA_EXTEND_LIFETIMES      = 0x80,
        LSRA_EXTEND_LIFETIMES_MASK = 0x80
    };

    LsraExtendLifetimes getLsraExtendLifeTimes() const
    {
        return static_cast<LsraExtendLifetimes>(lsraStressMask & LSRA_EXTEND_LIFETIMES_MASK);
    }

    bool extendLifetimes() const
    {
        return getLsraExtendLifeTimes() == LSRA_EXTEND_LIFETIMES;
    }

    // This controls whether variables locations should be set to the previous block in layout order
    // (LSRA_BLOCK_BOUNDARY_LAYOUT), or to that of the highest-weight predecessor (LSRA_BLOCK_BOUNDARY_PRED -
    // the default), or rotated (LSRA_BLOCK_BOUNDARY_ROTATE).
    enum LsraBlockBoundaryLocations
    {
        LSRA_BLOCK_BOUNDARY_PRED   = 0,
        LSRA_BLOCK_BOUNDARY_LAYOUT = 0x100,
        LSRA_BLOCK_BOUNDARY_ROTATE = 0x200,
        LSRA_BLOCK_BOUNDARY_MASK   = 0x300
    };

    LsraBlockBoundaryLocations getLsraBlockBoundaryLocations() const
    {
        return static_cast<LsraBlockBoundaryLocations>(lsraStressMask & LSRA_BLOCK_BOUNDARY_MASK);
    }

    RegNum rotateBlockStartLocation(Interval* interval, RegNum targetReg, regMaskTP availableRegs);

    // This controls whether we always insert a GT_RELOAD instruction after a spill
    // Note that this can be combined with LSRA_SPILL_ALWAYS (or not)
    enum LsraReload
    {
        LSRA_NO_RELOAD_IF_SAME    = 0,
        LSRA_ALWAYS_INSERT_RELOAD = 0x400,
        LSRA_RELOAD_MASK          = 0x400
    };

    LsraReload getLsraReload() const
    {
        return static_cast<LsraReload>(lsraStressMask & LSRA_RELOAD_MASK);
    }

    bool alwaysInsertReload() const
    {
        return getLsraReload() == LSRA_ALWAYS_INSERT_RELOAD;
    }

    // This controls whether we spill everywhere
    enum LsraSpill
    {
        LSRA_DONT_SPILL_ALWAYS = 0,
        LSRA_SPILL_ALWAYS      = 0x800,
        LSRA_SPILL_MASK        = 0x800
    };

    LsraSpill getLsraSpill() const
    {
        return static_cast<LsraSpill>(lsraStressMask & LSRA_SPILL_MASK);
    }

    bool spillAlways() const
    {
        return getLsraSpill() == LSRA_SPILL_ALWAYS;
    }

    // This controls whether RefPositions that lower/codegen indicated as reg optional be
    // allocated a reg at all.
    enum LsraRegOptionalControl
    {
        LSRA_REG_OPTIONAL_DEFAULT  = 0,
        LSRA_REG_OPTIONAL_NO_ALLOC = 0x1000,
        LSRA_REG_OPTIONAL_MASK     = 0x1000
    };

    LsraRegOptionalControl getLsraRegOptionalControl() const
    {
        return static_cast<LsraRegOptionalControl>(lsraStressMask & LSRA_REG_OPTIONAL_MASK);
    }

    bool regOptionalNoAlloc() const
    {
        return getLsraRegOptionalControl() == LSRA_REG_OPTIONAL_NO_ALLOC;
    }

    bool candidatesAreStressLimited() const
    {
        return (lsraStressMask & (LSRA_LIMIT_MASK | LSRA_SELECT_MASK)) != 0;
    }

    // Dump support
    void dumpDefList() const;
    void lsraDumpIntervals(const char* msg) const;
    void dumpRefPositions(const char* msg) const;
    void dumpVarRefPositions(const char* msg) const;

    void verifyFinalAllocation();
    void verifyResolutionMove(GenTree* resolutionNode, LsraLocation currentLocation);
    void VerifyEdgeResolution();
#else  // !DEBUG
    static bool doSelectNearest()
    {
        return false;
    }

    static bool extendLifetimes()
    {
        return false;
    }

    static bool spillAlways()
    {
        return false;
    }

    static bool isTraversalLayoutOrder()
    {
        return false;
    }

    static bool isTraversalPredFirstOrder()
    {
        return true;
    }

    static bool getLsraExtendLifeTimes()
    {
        return false;
    }

    static void SetLsraAdded(GenTree* node)
    {
        // do nothing; checked only under #DEBUG
    }

    static bool candidatesAreStressLimited()
    {
        return false;
    }
#endif // !DEBUG

    // Update allocations at start/end of block
    void unassignIntervalBlockStart(RegRecord* regRecord, VarToRegMap inVarToRegMap);

    // Record variable locations at start/end of block
    void processBlockStartLocations(BasicBlock* current);
    void processBlockEndLocations(BasicBlock* current);

#ifdef TARGET_ARM
    bool isSecondHalfReg(RegRecord* regRec, Interval* interval);
    bool canSpillDoubleReg(RegRecord* physRegRecord, LsraLocation refLocation) const;
    void unassignDoublePhysReg(RegRecord* doubleRegRecord);
#endif
    void updateAssignedInterval(RegRecord* reg, Interval* interval, RegisterType regType);
    void updatePreviousInterval(RegRecord* reg, Interval* interval, RegisterType regType);
    bool canRestorePreviousInterval(RegRecord* regRec, Interval* assignedInterval);
    bool isAssignedToInterval(Interval* interval, RegRecord* regRec);
    bool isRefPositionActive(RefPosition* refPosition, LsraLocation refLocation) const;
    bool canSpillReg(RegRecord* physRegRecord, LsraLocation refLocation) const;
    float getSpillWeight(RegRecord* physRegRecord);

    // insert refpositions representing prolog zero-inits which will be added later
    void insertZeroInitRefPositions();

    void newRegKillRefPositions(regMaskTP mask, LsraLocation location);

    void buildRefPositionsForNode(GenTree* node);
#ifdef DEBUG
    void BuildStressConstraints(GenTree* node, RefPositionIterator refPositionMark);
#endif

#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
    void buildUpperVectorSaveRefPositions(GenTree* node, LsraLocation location, regMaskTP fpCalleeKillSet);
    void buildUpperVectorRestoreRefPosition(Interval* lclVarInterval, LsraLocation location, GenTree* node);
#endif

    void AddLiveParamRegs(LclVarDsc* lcl);

    bool IsCandidateLclRef(GenTree* node) const
    {
        return node->OperIs(GT_LCL_LOAD, GT_LCL_STORE) && node->AsLclVar()->GetLcl()->IsRegCandidate();
    }

    bool IsRegCandidateLclLoad(GenTree* node) const
    {
        return node->OperIs(GT_LCL_LOAD) && node->AsLclLoad()->GetLcl()->IsRegCandidate();
    }

    // Helpers for getKillSetForNode().
    regMaskTP getKillSetForStoreInd(GenTreeIndStore* store);
#ifdef TARGET_XARCH
    regMaskTP getKillSetForShiftRotate(GenTreeOp* node);
    regMaskTP getKillSetForMul(GenTreeOp* node);
    regMaskTP getKillSetForModDiv(GenTreeOp* node);
#endif
    regMaskTP getKillSetForCall(GenTreeCall* call);
    regMaskTP getKillSetForStructStore(StructStoreKind kind);
    regMaskTP getKillSetForReturn();
    regMaskTP getKillSetForProfilerHook();
#ifdef FEATURE_HW_INTRINSICS
    regMaskTP getKillSetForHWIntrinsic(GenTreeHWIntrinsic* node);
#endif

#ifdef DEBUG
    // Return the registers killed by the given node.
    // This is used only for an assert, and for stress, so it is only defined under DEBUG.
    // Otherwise, the Build methods should obtain the killMask from the appropriate method above.
    regMaskTP getKillSetForNode(GenTree* node);
#endif

    // Given some node add refpositions for all the registers this node kills
    bool buildKillPositionsForNode(GenTree* node, LsraLocation location, regMaskTP killMask);
    bool KillGCRefs(GenTree* tree) const;

    regMaskTP allRegs(RegisterType rt) const;
    regMaskTP allIntRegs() const;
    regMaskTP allByteRegs() const;
    regMaskTP allFloatRegs() const;
    regMaskTP internalFloatRegCandidates() const;

    void makeRegisterInactive(RegRecord* physRegRecord);
    void freeRegister(RegRecord* physRegRecord);
    void freeRegisters(regMaskTP regsToFree);

    // Get the type that this node defines.
    var_types getDefType(GenTree* node) const;

    // Managing internal registers during the BuildNode process.
    RefPosition* defineNewInternalTemp(GenTree* node, RegisterType regType, regMaskTP regMask);
    RefPosition* buildInternalIntRegisterDefForNode(GenTree* node, regMaskTP regMask = RBM_NONE);
    RefPosition* buildInternalFloatRegisterDefForNode(GenTree* node, regMaskTP regMask = RBM_NONE);
    void buildInternalRegisterUses();

    RefPosition* BuildInternalIntDef(GenTree* node, regMaskTP regMask = RBM_NONE)
    {
        return buildInternalIntRegisterDefForNode(node, regMask);
    }

    RefPosition* BuildInternalFloatDef(GenTree* node, regMaskTP regMask = RBM_NONE)
    {
        return buildInternalFloatRegisterDefForNode(node, regMask);
    }

    void BuildInternalUses()
    {
        if (internalCount > 0)
        {
            buildInternalRegisterUses();
        }
    }

    void writeLocalReg(GenTreeLclVar* lclNode, LclVarDsc* lcl, RegNum reg);
    void clearLocalReg(GenTreeLclVar* lclNode, LclVarDsc* lcl);
    void resolveLocalRef(BasicBlock* block, GenTreeLclVar* node, RefPosition* currentRefPosition);

    void InsertRegCopy(BasicBlock* block, GenTree* before, Interval* interval, RegNum toReg, RegNum fromReg);
    void InsertRegLoad(BasicBlock* block, GenTree* before, Interval* interval, RegNum toReg);
    void InsertRegStore(BasicBlock* block, GenTree* before, Interval* interval, RegNum fromReg);
#ifdef TARGET_XARCH
    void InsertRegSwap(
        BasicBlock* block, GenTree* before, Interval* interval1, RegNum reg1, Interval* interval2, RegNum reg2);
#endif

    Interval* newInterval(var_types regType);

    Interval* HasLclInterval(unsigned index) const
    {
        assert(index < compiler->lvaTrackedCount);
        return localVarIntervals[index];
    }

    Interval* getIntervalForLocalVar(unsigned varIndex) const
    {
        assert(varIndex < compiler->lvaTrackedCount);
        assert(localVarIntervals[varIndex] != nullptr);
        return localVarIntervals[varIndex];
    }

    Interval* getIntervalForLocalVarNode(GenTreeLclVar* lclRef) const
    {
        return getIntervalForLocalVar(lclRef->GetLcl()->GetLivenessBitIndex());
    }

    RegRecord* GetRegRecord(RegNum regNum)
    {
        assert((REG_FIRST <= regNum) && (regNum <= REG_LAST) && (regNum < _countof(physRegs)));
        return &physRegs[regNum];
    }

    const RegRecord* GetRegRecord(RegNum regNum) const
    {
        // TODO-MIKE-Review: Do we really need a RegRecord for STK?!?
        assert((REG_FIRST <= regNum) && (regNum <= REG_STK) && (regNum < _countof(physRegs)));
        return &physRegs[regNum];
    }

    RefPosition* newRefPositionRaw(LsraLocation location, GenTree* node, RefType refType);
    RefPosition* newRegRefPosition(RegNum reg, LsraLocation location, RefType refType);
    RefPosition* newBlockRefPosition(LsraLocation location);
    RefPosition* newKillGCRegsRefPosition(LsraLocation location, GenTree* node, regMaskTP mask);
    RefPosition* newRefPosition(Interval*    interval,
                                LsraLocation location,
                                RefType      refType,
                                GenTree*     node     = nullptr,
                                regMaskTP    mask     = RBM_NONE,
                                unsigned     regIndex = 0);

    void applyCalleeSaveHeuristics(RefPosition* rp);

    void checkConflictingDefUse(RefPosition* rp);

    void associateRefPosWithInterval(RefPosition* rp);

    BasicBlock::weight_t getWeight(const RefPosition* refPos) const;

    RegNum allocateReg(Interval* current, RefPosition* refPosition DEBUG_ARG(RegisterScore* registerScore));
    RegNum assignCopyReg(RefPosition* refPosition);

    bool isSpillCandidate(Interval* current, RefPosition* refPosition, RegRecord* reg) const;
    void checkAndAssignInterval(RegRecord* regRec, Interval* interval);
    void assignPhysReg(RegRecord* regRec, Interval* interval);

    void checkAndClearInterval(RegRecord* reg, RefPosition* spillRefPosition);
    void unassignPhysReg(RegRecord* reg ARM_ARG(RegisterType newRegType));
    void unassignPhysReg(RegRecord* reg, RefPosition* spillRefPosition);
    void unassignPhysRegRecentRef(RegRecord* reg);
    void unassignPhysRegNoSpill(RegRecord* reg);

    void setIntervalAsSpilled(Interval* interval);
    void setIntervalAsSplit(Interval* interval);
    void spillInterval(Interval* interval, RefPosition* fromRefPosition DEBUGARG(RefPosition* toRefPosition));

    void spillGCRefs(RefPosition* killRefPosition);

    // When we split edges, we create new blocks, and instead of expanding the VarToRegMaps, we
    // rely on the property that the "in" map is the same as the "from" block of the edge, and the
    // "out" map is the same as the "to" block of the edge (by construction).
    // So, for any block whose bbNum is greater than bbNumMaxBeforeResolution, we use the
    // splitBBNumToTargetBBNumMap.
    // TODO-Throughput: We may want to look into the cost/benefit tradeoff of doing this vs. expanding
    // the arrays.

    unsigned bbNumMaxBeforeResolution;

    struct SplitEdgeInfo
    {
        unsigned fromBBNum;
        unsigned toBBNum;
    };

    using SplitBBNumToTargetBBNumMap                       = JitHashMap<unsigned, SplitEdgeInfo>;
    SplitBBNumToTargetBBNumMap* splitBBNumToTargetBBNumMap = nullptr;
    SplitBBNumToTargetBBNumMap* getSplitBBNumToTargetBBNumMap();
    SplitEdgeInfo getSplitEdgeInfo(unsigned bbNum) const;
    void initVarRegMaps();
    void setInVarRegForBB(unsigned bbNum, unsigned trackedVarIndex, RegNum reg);
    VarToRegMap getInVarToRegMap(BasicBlock* block) const;
    VarToRegMap getInVarToRegMap(unsigned bbNum) const;
    VarToRegMap getOutVarToRegMap(BasicBlock* block) const;
    void setVarReg(VarToRegMap map, unsigned trackedVarIndex, RegNum reg);
    RegNum getVarReg(VarToRegMap map, unsigned trackedVarIndex) const;

    RegNum getTempRegForResolution(BasicBlock* fromBlock, BasicBlock* toBlock, var_types type);

#ifdef DEBUG
    void dumpVarToRegMap(VarToRegMap map) const;
    void dumpInVarToRegMap(BasicBlock* block) const;
    void dumpOutVarToRegMap(BasicBlock* block) const;

    // There are three points at which a tuple-style dump is produced, and each
    // differs slightly:
    //   - In LSRA_DUMP_PRE, it does a simple dump of each node, with indications of what
    //     nodes are consumed.
    //   - In LSRA_DUMP_REFPOS, which is after the intervals are built, but before
    //     register allocation, each node is dumped, along with all of the RefPositions,
    //     The Intervals are identified as Lnnn for lclVar intervals, Innn for for other
    //     intervals, and Tnnn for internal temps.
    //   - In LSRA_DUMP_POST, which is after register allocation, the registers are
    //     shown.

    enum LsraTupleDumpMode
    {
        LSRA_DUMP_PRE,
        LSRA_DUMP_REFPOS,
        LSRA_DUMP_POST
    };

    void lsraGetOperandString(GenTree* node, LsraTupleDumpMode mode, char* buffer, unsigned bufferSize) const;
    void lsraDispNode(GenTree* node, LsraTupleDumpMode mode) const;
    void DumpOperandDefs(GenTree* operand, bool& first, LsraTupleDumpMode mode) const;
    void TupleStyleDump(LsraTupleDumpMode mode);

    LsraLocation maxNodeLocation = 0;

    // Width of various fields - used to create a streamlined dump during allocation that shows the
    // state of all the registers in columns.
    int regColumnWidth;
    int regTableIndent;

    const char* columnSeparator;
    const char* line;
    const char* leftBox;
    const char* middleBox;
    const char* rightBox;

    static const int MAX_FORMAT_CHARS = 12;
    char             intervalNameFormat[MAX_FORMAT_CHARS];
    char             regNameFormat[MAX_FORMAT_CHARS];
    char             shortRefPositionFormat[MAX_FORMAT_CHARS];
    char             emptyRefPositionFormat[MAX_FORMAT_CHARS];
    char             indentFormat[MAX_FORMAT_CHARS];
    static const int MAX_LEGEND_FORMAT_CHARS = 25;
    char             bbRefPosFormat[MAX_LEGEND_FORMAT_CHARS];
    char             legendFormat[MAX_LEGEND_FORMAT_CHARS];

    // How many rows have we printed since last printing a "title row"?
    static const int MAX_ROWS_BETWEEN_TITLES = 50;
    int              rowCountSinceLastTitle;
    // Current mask of registers being printed in the dump.
    regMaskTP lastDumpedRegisters;
    regMaskTP registersToDump;
    int       lastUsedRegNumIndex;

    bool shouldDumpReg(RegNum regNum) const
    {
        return (registersToDump & genRegMask(regNum)) != RBM_NONE;
    }

    void dumpRegRecordHeader();
    void dumpRegRecordTitle();
    void dumpRegRecordTitleIfNeeded();
    void dumpRegRecordTitleLines();
    void dumpRegRecords();
    void dumpNewBlock(BasicBlock* currentBlock, LsraLocation location);
    // An abbreviated RefPosition dump for printing with column-based register state
    void dumpRefPositionShort(RefPosition* refPosition, BasicBlock* currentBlock);
    // Print the number of spaces occupied by a dumpRefPositionShort()
    void dumpEmptyRefPosition() const;
    // A dump of Referent, in exactly regColumnWidth characters
    void dumpIntervalName(Interval* interval) const;

    // Events during the allocation phase that cause some dump output
    enum LsraDumpEvent
    {
        // Conflicting def/use
        LSRA_EVENT_DEFUSE_CONFLICT,
        LSRA_EVENT_DEFUSE_FIXED_DELAY_USE,
        LSRA_EVENT_DEFUSE_CASE1,
        LSRA_EVENT_DEFUSE_CASE2,
        LSRA_EVENT_DEFUSE_CASE3,
        LSRA_EVENT_DEFUSE_CASE4,
        LSRA_EVENT_DEFUSE_CASE5,
        LSRA_EVENT_DEFUSE_CASE6,

        // Spilling
        LSRA_EVENT_SPILL,
        LSRA_EVENT_SPILL_EXTENDED_LIFETIME,
        LSRA_EVENT_RESTORE_PREVIOUS_INTERVAL,
        LSRA_EVENT_RESTORE_PREVIOUS_INTERVAL_AFTER_SPILL,
        LSRA_EVENT_DONE_KILL_GC_REFS,
        LSRA_EVENT_NO_GC_KILLS,

        // Block boundaries
        LSRA_EVENT_START_BB,
        LSRA_EVENT_END_BB,

        // Miscellaneous
        LSRA_EVENT_FREE_REGS,
        LSRA_EVENT_UPPER_VECTOR_SAVE,
        LSRA_EVENT_UPPER_VECTOR_RESTORE,

        // Characteristics of the current RefPosition
        LSRA_EVENT_INCREMENT_RANGE_END, // ???
        LSRA_EVENT_LAST_USE,
        LSRA_EVENT_LAST_USE_DELAYED,
        LSRA_EVENT_NEEDS_NEW_REG,

        // Allocation decisions
        LSRA_EVENT_FIXED_REG,
        LSRA_EVENT_EXP_USE,
        LSRA_EVENT_ZERO_REF,
        LSRA_EVENT_NO_ENTRY_REG_ALLOCATED,
        LSRA_EVENT_KEPT_ALLOCATION,
        LSRA_EVENT_COPY_REG,
        LSRA_EVENT_MOVE_REG,
        LSRA_EVENT_ALLOC_REG,
        LSRA_EVENT_NO_REG_ALLOCATED,
        LSRA_EVENT_RELOAD,
        LSRA_EVENT_SPECIAL_PUTARG,
        LSRA_EVENT_REUSE_REG,
    };

    void dumpLsraAllocationEvent(LsraDumpEvent event,
                                 Interval*     interval      = nullptr,
                                 RegNum        reg           = REG_NA,
                                 BasicBlock*   currentBlock  = nullptr,
                                 RegisterScore registerScore = NONE);

    void ValidateLocalIntervals();

    // This is used for dumping
    RefPosition* activeRefPosition = nullptr;
#endif // DEBUG

#if TRACK_LSRA_STATS
    unsigned        regCandidateVarCount;
    LsraStat        firstRegSelStat = STAT_FREE;
    LsraBlockStats* blockStats      = nullptr;

    void updateLsraStat(LsraStat stat, unsigned currentBBNum);
    void dumpLsraStats(FILE* file) const;
#define INTRACK_STATS(x) x
#else
#define INTRACK_STATS(x)
#endif

    CompAllocator getAllocator(Compiler* comp) const
    {
        return comp->getAllocator(CMK_LSRA);
    }

    IntervalList intervals;

    // Map from tracked variable index to Interval*.
    Interval** localVarIntervals;

#if DOUBLE_ALIGN
    bool doDoubleAlign = false;
#endif

    // A map from bbNum to the block information used during register allocation.
    LsraBlockInfo* blockInfo = nullptr;

    BasicBlock* findPredBlockForLiveIn(BasicBlock* block,
                                       BasicBlock* prevBlock,
                                       BlockSet visited DEBUGARG(bool* pPredBlockIsAllocated));

    // The order in which the blocks will be allocated.
    // This is any array of BasicBlock*, in the order in which they should be traversed.
    BasicBlock** blockSequence = nullptr;

    BlockSet setBlockSequence();
    bool compareBlocksForSequencing(BasicBlock* block1, BasicBlock* block2, bool useBlockWeights);
    BasicBlock* blockSequenceWorkList = nullptr;

#ifdef DEBUG
    // LSRA must not change number of blocks and blockSetVersion that it initializes at start.
    unsigned blockSetVersion;
#endif

    void addToBlockSequenceWorkList(BlockSet sequencedBlockSet, BasicBlock* block, BlockSet& predSet);

    // Indicates whether the allocation pass has been completed.
    bool allocationPassComplete = false;
    // True if the method contains any critical edges.
    bool hasCriticalEdges = false;
    // True if there are any register candidate lclVars available for allocation.
    const bool enregisterLocalVars;

    // The bbNum of the block being currently allocated or resolved.
    unsigned curBBNum;
    // The current location
    LsraLocation currentLoc;
    // The first location in a cold or funclet block.
    LsraLocation firstColdLoc = MaxLocation;
    // The number of blocks that we've sequenced.
    unsigned bbSeqCount = 0;
    // The Location of the start of the current block.
    LsraLocation curBBStartLocation;

    // Ordered list of RefPositions
    RefPositionList refPositions;

    // Per-block variable location mappings: an array indexed by block number that yields a
    // pointer to an array of RegNum, one per variable.
    VarToRegMap* inVarToRegMaps  = nullptr;
    VarToRegMap* outVarToRegMaps = nullptr;

    // A temporary VarToRegMap used during the resolution of critical edges.
    VarToRegMap sharedCriticalVarToRegMap = nullptr;

    // The set of all register candidates. Note that this may be a subset of tracked vars.
    VARSET_TP registerCandidateVars;
    // Current set of live register candidate vars, used during building of RefPositions to determine
    // whether to preference to callee-save.
    VARSET_TP currentLiveVars;
    // Set of variables that may require resolution across an edge.
    // This is first constructed during interval building, to contain all the lclVars that are live at BB edges.
    // Then, any lclVar that is always in the same register is removed from the set.
    VARSET_TP resolutionCandidateVars;
    // This set contains all the lclVars that are ever spilled or split.
    VARSET_TP splitOrSpilledVars;
    // Set of floating point variables to consider for callee-save registers.
    VARSET_TP fpCalleeSaveCandidateVars;
    // Set of variables exposed on EH flow edges.
    VARSET_TP exceptVars = VarSetOps::UninitVal();
    // Set of variables exposed on finally edges. These must be zero-init if they are refs or if compInitMem is true.
    VARSET_TP finallyVars = VarSetOps::UninitVal();

#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE

#if defined(TARGET_AMD64)
    static const var_types LargeVectorSaveType = TYP_SIMD16;
#elif defined(TARGET_ARM64)
    static const var_types LargeVectorSaveType  = TYP_DOUBLE;
#endif

    // Set of large vector (TYP_SIMD32 on AVX) variables.
    VARSET_TP largeVectorVars;
    // Set of large vector (TYP_SIMD32 on AVX) variables to consider for callee-save registers.
    VARSET_TP largeVectorCalleeSaveCandidateVars;
#endif // FEATURE_PARTIAL_SIMD_CALLEE_SAVE

    //-----------------------------------------------------------------------
    // Register status
    //-----------------------------------------------------------------------

    regMaskTP m_AvailableRegs;
    regMaskTP m_allocateRegs = RBM_NONE;
#ifdef TARGET_ARMARCH
    regMaskTP m_reservedRegs = RBM_NONE;
#endif

    static RegNum getRegForType(RegNum reg, var_types regType)
    {
#ifdef TARGET_ARM
        if ((regType == TYP_DOUBLE) && !genIsValidDoubleReg(reg))
        {
            reg = REG_PREV(reg);
        }
#endif // TARGET_ARM
        return reg;
    }

    static regMaskTP getRegMask(RegNum reg, var_types regType)
    {
        reg               = getRegForType(reg, regType);
        regMaskTP regMask = genRegMask(reg);

#ifdef TARGET_ARM
        if (regType == TYP_DOUBLE)
        {
            assert(genIsValidDoubleReg(reg));
            regMask |= (regMask << 1);
        }
#endif

        return regMask;
    }

    void resetAvailableRegs()
    {
        m_AvailableRegs          = allIntRegs() | allFloatRegs();
        m_RegistersWithConstants = RBM_NONE;
    }

    bool isRegAvailable(RegNum reg, var_types regType) const
    {
        regMaskTP regMask = getRegMask(reg, regType);
        return (m_AvailableRegs & regMask) == regMask;
    }

    void setRegsInUse(regMaskTP regMask)
    {
        m_AvailableRegs &= ~regMask;
    }

    void setRegInUse(RegNum reg, var_types regType)
    {
        regMaskTP regMask = getRegMask(reg, regType);
        setRegsInUse(regMask);
    }

    void makeRegsAvailable(regMaskTP regMask)
    {
        m_AvailableRegs |= regMask;
    }

    void makeRegAvailable(RegNum reg, var_types regType)
    {
        regMaskTP regMask = getRegMask(reg, regType);
        makeRegsAvailable(regMask);
    }

    void clearNextIntervalRef(RegNum reg, var_types regType);
    void updateNextIntervalRef(RegNum reg, Interval* interval);

    void clearSpillCost(RegRecord* reg, var_types regType) const;
    void updateSpillCost(RegRecord* reg, Interval* interval) const;

    regMaskTP m_RegistersWithConstants;

    void clearConstantReg(RegNum reg, var_types regType)
    {
        m_RegistersWithConstants &= ~getRegMask(reg, regType);
    }

    void setConstantReg(RegNum reg, var_types regType)
    {
        m_RegistersWithConstants |= getRegMask(reg, regType);
    }

    bool isRegConstant(RegNum reg, var_types regType) const
    {
        regMaskTP regMask = getRegMask(getRegForType(reg, regType), regType);
        return (m_RegistersWithConstants & regMask) == regMask;
    }

    regMaskTP getMatchingConstants(regMaskTP mask, Interval* currentInterval, RefPosition* refPosition);
    bool isMatchingConstant(GenTree* node, GenTree* regNode);

    regMaskTP fixedRegs;
    void updateNextFixedRef(RegRecord* regRecord, RefPosition* nextRefPosition);

    LsraLocation GetNextFixedRef(RegNum reg) const
    {
        assert((REG_FIRST <= reg) && (reg <= REG_LAST));
        return nextFixedRef[reg];
    }

    LsraLocation GetNextFixedRef(RegNum reg, var_types regType) const;

    LsraLocation GetNextIntervalRef(RegNum reg) const
    {
        assert((REG_FIRST <= reg) && (reg <= REG_LAST));
        return nextIntervalRef[reg];
    }

    LsraLocation GetNextIntervalRef(RegNum reg, var_types regType) const;

    regMaskTP regsBusyUntilKill;
    regMaskTP regsInUseThisLocation;
    regMaskTP regsInUseNextLocation;

    bool isRegBusy(RegNum reg, var_types regType) const
    {
        regMaskTP regMask = getRegMask(reg, regType);
        return (regsBusyUntilKill & regMask) != RBM_NONE;
    }

    void setRegBusyUntilKill(RegNum reg, var_types regType)
    {
        regsBusyUntilKill |= getRegMask(reg, regType);
    }

    void clearRegBusyUntilKill(RegNum reg)
    {
        regsBusyUntilKill &= ~genRegMask(reg);
    }

    bool isRegInUse(RegNum reg, var_types regType) const
    {
        return (regsInUseThisLocation & getRegMask(reg, regType)) != RBM_NONE;
    }

    void resetRegState()
    {
        resetAvailableRegs();
        regsBusyUntilKill = RBM_NONE;
    }

    bool conflictingFixedRegReference(RegNum regNum, RefPosition* refPosition) const;

    // This method should not be used and is here to retain old behavior.
    // It should be replaced by isRegAvailable().
    // See comment in allocateReg();
    bool isFree(RegRecord* regRecord) const;

    //-----------------------------------------------------------------------
    // Build methods
    //-----------------------------------------------------------------------

    // The following keep track of information about internal (temporary register) intervals
    // during the building of a single node.
    RefPosition* internalDefs[4];
    int          internalCount            = 0;
    bool         setInternalRegsDelayFree = false;

    // When a RefTypeUse is marked as 'delayRegFree', we also want to mark the RefTypeDef
    // in the next Location as 'hasInterferingUses'. This is accomplished by setting this
    // 'pendingDelayFree' to true as they are created, and clearing it as a new node is
    // handled in 'BuildNode'.
    bool pendingDelayFree = false;

    // When Def RefPositions are built for a node, their RefInfoListNode
    // (GenTree* to RefPosition* mapping) is placed in the defList.
    // As the consuming node is handled, it removes the RefInfoListNode from the
    // defList, use the interval associated with the corresponding Def RefPosition and
    // use it to build the Use RefPosition.
    RefInfoList defList;

    // As we build uses, we may want to preference the next definition (i.e. the register produced
    // by the current node) to the same register as one of its uses. This is done by setting
    // 'tgtPrefUse' to that RefPosition.
    RefPosition* tgtPrefUse  = nullptr;
    RefPosition* tgtPrefUse2 = nullptr;

#ifdef TARGET_X86
    bool needFloatTmpForFPCall  = false;
    bool needDoubleTmpForFPCall = false;
#endif

#ifdef DEBUG
    unsigned nodeUseCount;
    unsigned nodeDefCount;
#endif

    // This method clears the "build state" before starting to handle a new node.
    void clearBuildState()
    {
        tgtPrefUse               = nullptr;
        tgtPrefUse2              = nullptr;
        internalCount            = 0;
        setInternalRegsDelayFree = false;
        pendingDelayFree         = false;
#ifdef DEBUG
        nodeUseCount = 0;
        nodeDefCount = 0;
#endif
    }

    bool IsRegCandidateLclStoreMultiReg(GenTreeLclStore* store);
    bool IsLclLoad(GenTreeLclLoad* load);

    RefPosition* BuildDef(GenTree* node, regMaskTP regCandidates = RBM_NONE);
    RefPosition* BuildDef(GenTree* node, var_types regType, regMaskTP regCandidates, unsigned regIndex);
    RefPosition* BuildUse(GenTree* operand, regMaskTP candidates = RBM_NONE, unsigned regIndex = 0);
    void setDelayFree(RefPosition* use);
    void BuildKills(GenTree* node, regMaskTP killMask);
#ifdef TARGET_XARCH
    RefPosition* BuildOperandUses(GenTree* node X86_ARG(regMaskTP candidates = RBM_NONE));
    void BuildDelayFreeUse(GenTree* op, GenTree* rmwNode = nullptr, regMaskTP candidates = RBM_NONE);
    void BuildDelayFreeOperandUses(GenTree* node, GenTree* rmwNode = nullptr, regMaskTP candidates = RBM_NONE);
#ifdef DEBUG
    bool isRMWRegOper(GenTreeOp* node);
#endif
    void BuildRMWUses(GenTreeOp* node);
#endif
    void BuildAddrUses(GenTree* addr, regMaskTP candidates = RBM_NONE);
    unsigned BuildAddrModeUses(GenTreeAddrMode* addrMode, regMaskTP candidates = RBM_NONE);

    void BuildNode(GenTree* node);
    void BuildReturn(GenTreeUnOp* ret);
    void BuildPutArgReg(GenTreeUnOp* node);
    void BuildPutArgStk(GenTreePutArgStk* node);
#if TARGET_ARM
    void BuildPutArgSplit(GenTreePutArgSplit* node);
#endif
    void BuildCall(GenTreeCall* call);
    void BuildBoundsChk(GenTreeBoundsChk* node);
    void BuildOvfTruncate(GenTreeUnOp* node);
    void BuildOvfUnsigned(GenTreeUnOp* node);
    void BuildConv(GenTreeUnOp* cast);
    void BuildOvfConv(GenTreeUnOp* cast);
#ifdef TARGET_ARM64
    void BuildIntExtend(GenTreeUnOp* node);
#endif
    void BuildCmp(GenTreeOp* cmp);
    void BuildLclHeap(GenTreeUnOp* node);
    void BuildAddrMode(GenTreeAddrMode* node);
    void BuildCmpXchg(GenTreeCmpXchg* cmpxchg);
    void BuildInterlocked(GenTreeOp* node);
    void BuildKeepAlive(GenTreeUnOp* node);
    void BuildInstr(GenTreeInstr* instr);
    void BuildStructStore(GenTree* store, StructStoreKind kind, ClassLayout* layout);
    void BuildStructStoreUnrollRegsWB(GenTreeIndStoreObj* store, ClassLayout* layout);
    void BuildStoreDynBlk(GenTreeDynBlk* store);
    void BuildLclStoreDef(GenTreeLclStore* store, LclVarDsc* lcl, RefPosition* singleUseRef, unsigned index);
    void BuildLclStoreMultiReg(GenTreeLclStore* store);
    void BuildLclStore(GenTreeLclStore* store);
    void BuildLclStoreFld(GenTreeLclStoreFld* store);
    void BuildLclStoreCommon(GenTreeLclRef* store);
    void BuildGCWriteBarrier(GenTreeIndStore* store);
#ifdef TARGET_XARCH
    void BuildLoadInd(GenTreeIndir* load);
    void BuildIndStore(GenTreeIndir* store);
    void BuildShiftRotate(GenTreeOp* node);
    void BuildDivMod(GenTreeOp* node);
    void BuildMul(GenTreeOp* mul);
    void BuildMulLong(GenTreeOp* mul);
    void BuildIntrinsic(GenTreeIntrinsic* intrinsic);
#else
    void BuildIndir(GenTreeIndir* indir);
#endif
#ifdef TARGET_ARM
    void BuildShiftLong(GenTreeOp* node);
#endif
#ifdef FEATURE_HW_INTRINSICS
    void BuildHWIntrinsic(GenTreeHWIntrinsic* node);
#ifdef TARGET_ARM64
    void BuildDelayFreeUse(GenTree* op, GenTree* rmwNode = nullptr, regMaskTP candidates = RBM_NONE);
    void BuildHWIntrinsicGetElement(GenTreeHWIntrinsic* node);
#endif
#endif

    bool supportsSpecialPutArg();
#ifdef WINDOWS_AMD64_ABI
    bool HandleFloatVarArgs(GenTreeCall* call, GenTree* argNode);
#endif
#ifdef TARGET_XARCH
    void SetContainsAVXFlags(unsigned sizeOfSIMDVector = 0);
#endif

    RegRecord    physRegs[REG_COUNT];
    LsraLocation nextFixedRef[REG_COUNT];
    LsraLocation nextIntervalRef[REG_COUNT];

    // max simultaneous spill locations used of every type
    unsigned maxSpill[TYP_COUNT]{};
    unsigned currentSpill[TYP_COUNT]{};
};

class Interval : public Referenceable
{
public:
    Interval(RegisterType registerType, regMaskTP registerPreferences)
        : registerPreferences(registerPreferences)
        , registerType(registerType)
        , isLocalVar(false)
        , isSplit(false)
        , isSpilled(false)
        , isInternal(false)
        , isStructField(false)
        , isPromotedStruct(false)
        , hasConflictingDefUse(false)
        , hasInterferingUses(false)
        , isSpecialPutArg(false)
        , preferCalleeSave(false)
        , isConstant(false)
        , isWriteThru(false)
        , isSingleDef(false)
#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
        , isUpperVector(false)
        , isPartiallySpilled(false)
#endif
    {
    }

    Interval(const Interval&) = delete;
    Interval& operator=(const Interval&) = delete;

    // The relatedInterval is:
    //  - for any other interval, it is the interval to which this interval
    //    is currently preferenced (e.g. because they are related by a copy)
    Interval* relatedInterval = nullptr;

    // The assignedReg is the RecRecord for the register to which this interval
    // has been assigned at some point - if the interval is active, this is the
    // register it currently occupies.
    RegRecord* assignedReg = nullptr;

    // Fixed registers for which this Interval has a preference
    regMaskTP registerPreferences;

    unsigned varIndex = 0; // index into the lvaTracked array

    // The register to which it is currently assigned.
    RegNum physReg = REG_NA;

    const RegisterType registerType;

    // Is this Interval currently in a register and live?
    bool isActive = false;

    bool isLocalVar : 1;
    // Indicates whether this interval has been assigned to different registers
    bool isSplit : 1;
    // Indicates whether this interval is ever spilled
    bool isSpilled : 1;
    // indicates an interval representing the internal requirements for
    // generating code for a node (temp registers internal to the node)
    // Note that this interval may live beyond a node in the ARR_LENREF/IND_LOAD
    // case (though never lives beyond a stmt)
    bool isInternal : 1;
    // true if this is a LocalVar for a struct field
    bool isStructField : 1;
    // true iff this is a GT_LDOBJ for a fully promoted (PROMOTION_TYPE_INDEPENDENT) struct
    bool isPromotedStruct : 1;
    // true if this is an SDSU interval for which the def and use have conflicting register
    // requirements
    bool hasConflictingDefUse : 1;
    // true if this interval's defining node has "delayRegFree" uses, either due to it being an RMW instruction,
    // OR because it requires an internal register that differs from the target.
    bool hasInterferingUses : 1;

    // True if this interval is defined by a putArg, whose source is a non-last-use lclVar.
    // During allocation, this flag will be cleared if the source is not already in the required register.
    // Otherwise, we will leave the register allocated to the lclVar, but mark the RegRecord as
    // isBusyUntilKill, so that it won't be reused if the lclVar goes dead before the call.
    bool isSpecialPutArg : 1;

    // True if this interval interferes with a call.
    bool preferCalleeSave : 1;

    // True if this interval is defined by a constant node that may be reused and/or may be
    // able to reuse a constant that's already in a register.
    bool isConstant : 1;

    // True if this interval is associated with a lclVar that is written to memory at each definition.
    bool isWriteThru : 1;

    // True if this interval has a single definition.
    bool isSingleDef : 1;

#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
    // True if this is a special interval for saving the upper half of a large vector.
    bool isUpperVector : 1;
    // True if this interval has been partially spilled
    bool isPartiallySpilled : 1;
#endif

#ifdef DEBUG
    unsigned intervalIndex = 0;
#endif

    LclVarDsc* getLocalVar(Compiler* comp) const
    {
        assert(isLocalVar);
        return comp->lvaGetDescByTrackedIndex(varIndex);
    }

    // Get the local tracked variable "index" (lvVarIndex), used in bitmasks.
    unsigned getVarIndex() const
    {
        assert(isLocalVar);
        return varIndex;
    }

    void assignRelatedInterval(Interval* newRelatedInterval)
    {
#ifdef DEBUG
        if (JitTls::GetCompiler()->verbose)
        {
            printf("Assigning related ");
            newRelatedInterval->microDump();
            printf(" to ");
            microDump();
            printf("\n");
        }
#endif

        relatedInterval = newRelatedInterval;
    }

    // Assign the related interval, but only if it isn't already assigned.
    bool assignRelatedIntervalIfUnassigned(Interval* newRelatedInterval);

    // Get the current preferences for this Interval.
    // Note that when we have an assigned register we don't necessarily update the
    // registerPreferences to that register, as there may be multiple, possibly disjoint,
    // definitions. This method will return the current assigned register if any, or
    // the 'registerPreferences' otherwise.
    regMaskTP getCurrentPreferences() const
    {
        return assignedReg == nullptr ? registerPreferences : genRegMask(assignedReg->regNum);
    }

    void mergeRegisterPreferences(regMaskTP preferences);
    void updateRegisterPreferences(regMaskTP preferences);

    bool IsUpperVector() const
    {
#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
        return isUpperVector;
#else
        return false;
#endif
    }

#ifdef DEBUG
    void dump() const;
    void tinyDump() const;
    void microDump() const;
#endif
};

class RefPosition
{
public:
    // A RefPosition refers to either an Interval or a RegRecord. 'referent' points to one
    // of these types. If it refers to a RegRecord, then 'isPhysRegRef()' is true. If it
    // refers to an Interval, then 'isPhysRegRef()' is false.
    // referent can never be null.

    Referenceable* referent = nullptr;

    // nextRefPosition is the next in code order.
    // Note that in either case there is no need for these to be doubly linked, as they
    // are only traversed in the forward direction, and are not moved.
    RefPosition* nextRefPosition = nullptr;

    // The remaining fields are common to both options
    GenTree* node;
    unsigned bbNum;

    LsraLocation nodeLocation;

    // Prior to the allocation pass, registerAssignment captures the valid registers
    // for this RefPosition.
    // After the allocation pass, this contains the actual assignment
    regMaskTP registerAssignment = RBM_NONE;

    RefType refType;

    // Used by RefTypeDef/Use positions of a multi-reg call node.
    // Indicates the position of the register that this ref position refers to.
    // The max bits needed is based on max value of MAX_RET_REG_COUNT value
    // across all targets and that happens 4 on on Arm.  Hence index value
    // would be 0..MAX_RET_REG_COUNT-1.
    uint8_t regIndex : 2;

    // Indicates whether this ref position is to be allocated a reg only if profitable. Currently these are the
    // ref positions that lower/codegen has indicated as reg optional and is considered a contained memory operand if
    // no reg is allocated.
    uint8_t regOptional : 1;

    // Last Use - this may be true for multiple RefPositions in the same Interval
    uint8_t lastUse : 1;

    // Spill and Copy info
    //   reload indicates that the value was spilled, and must be reloaded here.
    //   spillAfter indicates that the value is spilled here, so a spill must be added.
    //   singleDefSpill indicates that it is associated with a single-def var and if it
    //      is decided to get spilled, it will be spilled at firstRefPosition def. That
    //      way, the the value of stack will always be up-to-date and no more spills or
    //      resolutions (from reg to stack) will be needed for such single-def var.
    //   copyReg indicates that the value needs to be copied to a specific register,
    //      but that it will also retain its current assigned register.
    //   moveReg indicates that the value needs to be moved to a different register,
    //      and that this will be its new assigned register.
    // A RefPosition may have any flag individually or the following combinations:
    //  - reload and spillAfter (i.e. it remains in memory), but not in combination with copyReg or moveReg
    //    (reload cannot exist with copyReg or moveReg; it should be reloaded into the appropriate reg)
    //  - spillAfter and copyReg (i.e. it must be copied to a new reg for use, but is then spilled)
    //  - spillAfter and moveReg (i.e. it most be both spilled and moved)
    //    NOTE: a moveReg involves an explicit move, and would usually not be needed for a fixed Reg if it is going
    //    to be spilled, because the code generator will do the move to the fixed register, and doesn't need to
    //    record the new register location as the new "home" location of the lclVar. However, if there is a conflicting
    //    use at the same location (e.g. lclVar V1 is in rdx and needs to be in rcx, but V2 needs to be in rdx), then
    //    we need an explicit move.
    //  - copyReg and moveReg must not exist with each other.

    uint8_t reload : 1;
    uint8_t spillAfter : 1;
    uint8_t singleDefSpill : 1;
    uint8_t writeThru : 1; // true if this var is defined in a register and also spilled. spillAfter must NOT be
                           // set.

    uint8_t copyReg : 1;
    uint8_t moveReg : 1; // true if this var is moved to a new register

    uint8_t isPhysRegRef : 1; // true if 'referent' points of a RegRecord, false if it points to an Interval
    uint8_t isFixedRegRef : 1;
    uint8_t isLocalDefUse : 1;

    // delayRegFree indicates that the register should not be freed right away, but instead wait
    // until the next Location after it would normally be freed.  This is used for the case of
    // non-commutative binary operators, where op2 must not be assigned the same register as
    // the target.  We do this by not freeing it until after the target has been defined.
    // Another option would be to actually change the Location of the op2 use until the same
    // Location as the def, but then it could potentially reuse a register that has been freed
    // from the other source(s), e.g. if it's a lastUse or spilled.
    uint8_t delayRegFree : 1;

    // outOfOrder is marked on a (non-def) RefPosition that doesn't follow a definition of the
    // register currently assigned to the Interval.  This happens when we use the assigned
    // register from a predecessor that is not the most recently allocated BasicBlock.
    uint8_t outOfOrder : 1;

#ifdef DEBUG
    // Minimum number registers that needs to be ensured while
    // constraining candidates for this ref position under
    // LSRA stress.
    unsigned minRegCandidateCount = 1;

    // The unique RefPosition number, equal to its index in the
    // refPositions list. Only used for debugging dumps.
    unsigned rpNum = 0;
#endif // DEBUG

    RefPosition(unsigned bbNum, LsraLocation nodeLocation, GenTree* node, RefType refType)
        : node(node)
        , bbNum(bbNum)
        , nodeLocation(nodeLocation)
        , refType(refType)
        , regIndex(0)
        , regOptional(false)
        , lastUse(false)
        , reload(false)
        , spillAfter(false)
        , singleDefSpill(false)
        , writeThru(false)
        , copyReg(false)
        , moveReg(false)
        , isPhysRegRef(false)
        , isFixedRegRef(false)
        , isLocalDefUse(false)
        , delayRegFree(false)
        , outOfOrder(false)
    {
    }

    RefPosition(const RefPosition&) = delete;
    RefPosition& operator=(const RefPosition&) = delete;

    void LinkRefPosition();

    Interval* getInterval() const
    {
        assert(!isPhysRegRef);
        return static_cast<Interval*>(referent);
    }

    void setInterval(Interval* i)
    {
        referent     = i;
        isPhysRegRef = false;
    }

    RegRecord* getReg() const
    {
        assert(isPhysRegRef);
        return static_cast<RegRecord*>(referent);
    }

    void setReg(RegRecord* r)
    {
        referent           = r;
        isPhysRegRef       = true;
        registerAssignment = genRegMask(r->regNum);
    }

    RegNum assignedReg() const
    {
        return registerAssignment == RBM_NONE ? REG_NA : genRegNumFromMask(registerAssignment);
    }

    // Returns true if it is a reference on a node.
    bool IsActualRef() const
    {
        switch (refType)
        {
            case RefTypeDef:
            case RefTypeUse:
#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
            case RefTypeUpperVectorSave:
            case RefTypeUpperVectorRestore:
#endif
                return true;

            // These must always be marked RegOptional.
            case RefTypeExpUse:
            case RefTypeParamDef:
            case RefTypeDummyDef:
            case RefTypeZeroInit:
                assert(RegOptional());
                FALLTHROUGH;
            default:
                return false;
        }
    }

    bool IsPhysRegRef() const
    {
        return (refType == RefTypeFixedReg) || (refType == RefTypeKill);
    }

    void setRegOptional(bool val)
    {
        regOptional = val;
    }

    // Returns true whether this ref position is to be allocated
    // a reg only if it is profitable.
    bool RegOptional() const
    {
        // TODO-CQ: Right now if a ref position is marked as
        // copyreg or movereg, then it is not treated as
        // 'allocate if profitable'. This is an implementation
        // limitation that needs to be addressed.
        return regOptional && !copyReg && !moveReg;
    }

    void SetRegIndex(unsigned index)
    {
        regIndex = index;
        assert(regIndex == index);
    }

    unsigned GetRegIndex() const
    {
        return regIndex;
    }

    LsraLocation getRefEndLocation() const
    {
        return delayRegFree ? nodeLocation + 1 : nodeLocation;
    }

    RefPosition* getRangeEndRef()
    {
        if (lastUse || nextRefPosition == nullptr || spillAfter)
        {
            return this;
        }
        // It would seem to make sense to only return 'nextRefPosition' if it is a lastUse,
        // and otherwise return `lastRefPosition', but that tends to  excessively lengthen
        // the range for heuristic purposes.
        // TODO-CQ: Look into how this might be improved .
        return nextRefPosition;
    }

    LsraLocation getRangeEndLocation()
    {
        return getRangeEndRef()->getRefEndLocation();
    }

    bool isIntervalRef() const
    {
        return !IsPhysRegRef() && (referent != nullptr);
    }

    // isFixedRefOfRegMask indicates that the RefPosition has a fixed assignment to the register
    // specified by the given mask
    bool isFixedRefOfRegMask(regMaskTP regMask) const
    {
        assert(genMaxOneBit(regMask));
        return registerAssignment == regMask;
    }

    // isFixedRefOfReg indicates that the RefPosition has a fixed assignment to the given register
    bool isFixedRefOfReg(RegNum regNum) const
    {
        return isFixedRefOfRegMask(genRegMask(regNum));
    }

#ifdef DEBUG
    void dump(const LinearScan* linearScan) const;
#endif
};
