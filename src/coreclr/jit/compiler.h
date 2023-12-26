// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                           Compiler                                        XX
XX                                                                           XX
XX  Represents the method data we are currently JIT-compiling.               XX
XX  An instance of this class is created for every method we JIT.            XX
XX  This contains all the info needed for the method. So allocating a        XX
XX  a new instance per method makes it thread-safe.                          XX
XX  It should be used to do all the memory management for the compiler run.  XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

/*****************************************************************************/
#ifndef _COMPILER_H_
#define _COMPILER_H_
/*****************************************************************************/

#include "jit.h"
#include "opcode.h"
#include "jithashtable.h"
#include "gentree.h"
#include "lir.h"
#include "block.h"
#include "inline.h"
#include "jiteh.h"
#include "cycletimer.h"
#include "arraystack.h"
#include "hashbv.h"
#include "jitexpandarray.h"
#include "namedintrinsiclist.h"
#include "phase.h"
#ifdef LATE_DISASM
#include "disasm.h"
#endif

#include "codegeninterface.h"
#include "hwintrinsic.h"

#if DUMP_GC_TABLES && defined(JIT32_GCENCODER)
#include "gcdump.h"
#endif

struct InfoHdr;
struct escapeMapping_t;
class emitter;
struct ShadowParamVarInfo;
struct ParamAllocInfo;
class FgStack;
class Instrumentor;
class SpanningTreeVisitor;
class OptBoolsDsc;
struct LoopCloneContext;
struct LoopCloneVisitorInfo;
struct ArrIndex;
class SsaOptimizer;
class SsaBuilder;
class ValueNumbering;
class ValueNumStore;
class CopyPropDomTreeVisitor;
class LoopHoist;
class Cse;
class Lowering;
class Compiler;
INDEBUG(class IndentStack;)

/*****************************************************************************
 *                  Unwind info
 */

#include "unwind.h"

// Declare global operator new overloads that use the compiler's arena allocator
void* __cdecl operator new(size_t n, Compiler* context, CompMemKind cmk);
void* __cdecl operator new[](size_t n, Compiler* context, CompMemKind cmk);

/*****************************************************************************/

/* This is included here and not earlier as it needs the definition of "CSE"
 * which is defined in the section above */

/*****************************************************************************/

unsigned genLog2(unsigned value);
unsigned genLog2(unsigned __int64 value);

unsigned ReinterpretHexAsDecimal(unsigned in);

/*****************************************************************************/

const unsigned FLG_CCTOR = (CORINFO_FLG_CONSTRUCTOR | CORINFO_FLG_STATIC);

#ifdef DEBUG
const int BAD_STK_OFFS = 0xBAADF00D; // for LclVarDsc::lvStkOffs
#endif

// The following holds the Local var info (scope information)
struct VarScopeDsc
{
    unsigned vsdVarNum; // (remapped) LclVarDsc number
    unsigned vsdLVnum;  // 'which' in eeGetLVinfo().
                        // Also, it is the index of this entry in the info.compVarScopes array,
                        // which is useful since the array is also accessed via the
                        // compEnterScopeList and compExitScopeList sorted arrays.

    IL_OFFSET vsdLifeBeg; // instr offset of beg of life
    IL_OFFSET vsdLifeEnd; // instr offset of end of life

    INDEBUG(const char* vsdName;) // name of the var
};

enum RefCountState : uint8_t
{
    RCS_INVALID, // not valid to get/set ref counts
    RCS_MORPH,   // morphing uses the 2 LclVarDsc ref count members for implicit by ref optimizations
    RCS_NORMAL,  // normal ref counts (from lvaMarkRefs onward)
};

class LclVarDsc
{
public:
    LclVarDsc()
    {
        // It is expected that the memory allocated for LclVarDsc is already zeroed.
        assert(lvType == TYP_UNDEF);
        assert(lvClassHnd == NO_CLASS_HANDLE);
    }

    var_types lvType;

    unsigned char lvIsParam : 1;  // is this a parameter?
    unsigned char lvIsRegArg : 1; // is this an argument that was passed by register?
#ifdef TARGET_ARM64
    unsigned char m_paramRegCount : 3;
#elif defined(TARGET_ARM)
    unsigned char m_paramRegCount : 4;
#endif
    unsigned char lvFramePointerBased : 1; // 0 = off of REG_SPBASE (e.g., ESP), 1 = off of REG_FPBASE (e.g., EBP)

    unsigned char lvOnFrame : 1;  // (part of) the variable lives on the frame
    unsigned char lvRegister : 1; // assigned to live in a register? For RyuJIT backend, this is only set if the
                                  // variable is in the same register for the entire function.
    unsigned char lvTracked : 1;  // is this a tracked variable?
    unsigned char m_hasGCLiveness : 1;
    unsigned char m_pinning : 1;

    unsigned char lvMustInit : 1;    // must be initialized
    unsigned char lvAddrExposed : 1; // The address of this variable is "exposed" -- passed as an argument, stored in a
                                     // global location, etc.
                                     // We cannot reason reliably about the value of the variable.
    unsigned char lvDoNotEnregister : 1; // Do not enregister this variable.
    unsigned char lvFieldAccessed : 1;   // The var is a struct local, and a field of the variable is accessed.  Affects
                                         // struct promotion.
    unsigned char lvLiveInOutOfHndlr : 1; // The variable is live in or out of an exception handler, and therefore must
                                          // be on the stack (at least at those boundaries.)
#ifdef DEBUG
    unsigned char lvLclFieldExpr : 1;   // The variable has (STORE_)LCL_FLD accesses.
    unsigned char lvLclBlockOpAddr : 1; // The variable was written to via a block operation.
#endif
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
    unsigned char lvIsImplicitByRefArgTemp : 1;
#endif
    unsigned char m_isSsa : 1; // The variable is in SSA form (set by SsaBuilder)

    unsigned char lvIsCSE : 1;                // Indicates if this LclVar is a CSE variable.
    unsigned char lvHasLdAddrOp : 1;          // has ldloca or ldarga opcode on this local.
    unsigned char lvHasILStoreOp : 1;         // there is at least one STLOC or STARG on this local
    unsigned char lvHasMultipleILStoreOp : 1; // there is more than one STLOC on this local

    unsigned char lvIsTemp : 1; // Short-lifetime compiler temp

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
    unsigned char lvIsImplicitByRef : 1; // Set if the argument is an implicit byref.
#endif

    bool IsParam() const
    {
        return lvIsParam;
    }

    bool IsRegParam() const
    {
        return lvIsRegArg;
    }

    bool IsImplicitByRefParam() const
    {
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
        assert(!lvIsImplicitByRef || lvIsParam);
        assert(!lvIsImplicitByRef || varTypeIsStruct(lvType) || (lvType == TYP_BYREF));

        return lvIsImplicitByRef;
#else
        return false;
#endif
    }

    ClassLayout* GetImplicitByRefParamLayout() const
    {
        assert(IsImplicitByRefParam());
#if defined(TARGET_AMD64) || defined(TARGET_ARM64)
        assert(m_layout != nullptr);
        return m_layout;
#else
        return nullptr;
#endif
    }

#ifdef TARGET_ARM
    bool IsPreSpilledRegParam(regMaskTP preSpillMask) const
    {
        return lvIsRegArg && (preSpillMask & genRegMask(GetParamReg()));
    }
#endif

#ifdef FEATURE_HFA
    void SetIsHfaParam()
    {
        m_isHfa = true;
    }
#endif

    bool IsHfaParam() const
    {
#ifdef FEATURE_HFA
        return m_isHfa;
#else
        return false;
#endif
    }

    bool IsHfaRegParam() const
    {
#ifdef FEATURE_HFA
        return lvIsRegArg && m_isHfa;
#else
        return false;
#endif
    }

    bool IsPinning() const
    {
        return m_pinning;
    }

    bool IsAddressExposed() const
    {
        return lvAddrExposed;
    }

    bool IsSsa() const
    {
        return m_isSsa;
    }

#if OPT_BOOL_OPS
    unsigned char lvIsBoolean : 1; // set if variable is boolean
#endif
    unsigned char lvSingleDef : 1; // variable has a single def
                                   // before lvaMarkLocalVars: identifies ref type locals that can get type updates
                                   // after lvaMarkLocalVars: identifies locals that are suitable for optAddCopies

    unsigned char lvSingleDefRegCandidate : 1; // variable has a single def and hence is a register candidate
                                               // Currently, this is only used to decide if an EH variable can be
                                               // a register candiate or not.

    unsigned char lvDisqualifySingleDefRegCandidate : 1; // tracks variable that are disqualified from register
                                                         // candidancy

    unsigned char lvSpillAtSingleDef : 1; // variable has a single def (as determined by LSRA interval scan)
                                          // and is spilled making it candidate to spill right after the
                                          // first (and only) definition.
                                          // Note: We cannot reuse lvSingleDefRegCandidate because it is set
                                          // in earlier phase and the information might not be appropriate
                                          // in LSRA.

#if ASSERTION_PROP
    unsigned char lvDisqualifyAddCopy : 1; // local isn't a candidate for optAddCopies
#endif

    unsigned char lvHasEHRefs : 1; // local has EH references
    unsigned char lvHasEHUses : 1; // local has EH uses

#ifndef TARGET_64BIT
    unsigned char lvStructDoubleAlign : 1; // Must we double align this struct?
#endif                                     // !TARGET_64BIT
#ifdef TARGET_64BIT
    unsigned char lvQuirkToLong : 1; // Quirk to allocate this LclVar as a 64-bit long
#endif
    unsigned char lvIsPtr : 1; // Might this be used in an address computation? (used by buffer overflow security
                               // checks)
    unsigned char lvIsUnsafeBuffer : 1; // Does this contain an unsafe buffer requiring buffer overflow security checks?
    unsigned char lvPromoted : 1; // True when this local is a promoted struct, a normed struct, or a "split" long on a
                                  // 32-bit target.  For implicit byref parameters, this gets hijacked between
    // lvaRetypeImplicitByRefParams and lvaDemoteImplicitByRefParams to indicate whether
    // references to the arg are being rewritten as references to a promoted shadow local.
    unsigned char lvIsStructField : 1; // Is this local var a field of a promoted struct local?
    unsigned char lvWasStructField : 1;
    unsigned char lvOverlappingFields : 1; // True when we have a struct with possibly overlapping fields
    unsigned char lvContainsHoles : 1;     // True when we have a promoted struct that contains holes
    unsigned char lvCustomLayout : 1;      // True when this struct has "CustomLayout"

    unsigned char lvIsMultiRegArg : 1; // true if this is a multireg LclVar struct used in an argument context
    unsigned char lvIsMultiRegRet : 1; // true if this is a multireg LclVar struct assigned from a multireg call

#ifdef FEATURE_HFA
    unsigned char m_isHfa : 1;
#endif

    unsigned char lvLRACandidate : 1; // Tracked for linear scan register allocation purposes

#ifdef FEATURE_SIMD
    unsigned char lvUsedInSIMDIntrinsic : 1; // This tells lclvar is used for simd intrinsic
#endif

    unsigned char lvClassIsExact : 1;              // lvClassHandle is the exact type
    INDEBUG(unsigned char lvClassInfoUpdated : 1;) // true if this var has updated class handle or exactness

    unsigned char lvImplicitlyReferenced : 1; // true if there are non-IR references to this local (prolog, epilog, gc,
                                              // eh)

    unsigned char lvSuppressedZeroInit : 1; // local needs zero init if we transform tail call to loop

    unsigned char lvHasExplicitInit : 1; // The local is explicitly initialized and doesn't need zero initialization in
                                         // the prolog. If the local has gc pointers, there are no gc-safe points
                                         // between the prolog and the explicit initialization.

    union {
        unsigned lvFieldLclStart; // The index of the local var representing the first field in the promoted
                                  // struct local. For implicit byref parameters, this gets hijacked between
                                  // lvaRetypeImplicitByRefParams and lvaDemoteImplicitByRefParams to point to
                                  // the struct local created to model the parameter's struct promotion, if any.
        unsigned lvParentLcl; // The index of the local var representing the parent (i.e. the promoted struct local).
                              // Valid on promoted struct local fields.
    };

    unsigned char lvFieldCnt; //  Number of fields in the promoted VarDsc.
    unsigned char lvFldOffset;

    void MakePromotedStructField(unsigned parentLclNum, unsigned fieldOffset, FieldSeqNode* fieldSeq)
    {
        assert(fieldOffset <= UINT8_MAX);

        lvIsStructField = true;
        lvParentLcl     = parentLclNum;
        lvFldOffset     = static_cast<uint8_t>(fieldOffset);
        m_fieldSeq      = fieldSeq;
    }

    bool IsPromoted() const
    {
        return lvPromoted;
    }

    bool IsIndependentPromoted() const
    {
        // TODO-Cleanup: return true for arm32.
        return lvPromoted && !lvDoNotEnregister;
    }

    bool IsDependentPromoted() const
    {
        return lvPromoted && !IsIndependentPromoted();
    }

    bool IsPromotedField() const
    {
        return lvIsStructField;
    }

    bool IsDependentPromotedField(Compiler* compiler) const;

    unsigned GetPromotedFieldCount() const
    {
        assert(lvPromoted);
        return lvFieldCnt;
    }

    unsigned GetPromotedFieldLclNum(unsigned ordinal) const
    {
        assert(lvPromoted && (ordinal < lvFieldCnt));
        return lvFieldLclStart + ordinal;
    }

    unsigned GetPromotedFieldParentLclNum() const
    {
        assert(lvIsStructField);
        return lvParentLcl;
    }

    unsigned GetPromotedFieldOffset() const
    {
        assert(lvIsStructField);
        return lvFldOffset;
    }

    FieldSeqNode* GetPromotedFieldSeq() const
    {
        assert(lvIsStructField);
        return m_fieldSeq;
    }

    INDEBUG(char lvSingleDefDisqualifyReason = 'H';)

private:
    regNumberSmall _lvRegNum; // Used to store the register this variable is in (or, the low register of a
                              // register pair). It is set during codegen any time the
                              // variable is enregistered (lvRegister is only set
                              // to non-zero if the variable gets the same register assignment for its entire
                              // lifetime).

#ifdef UNIX_AMD64_ABI
    regNumberSmall m_paramRegs[2]{REG_NA, REG_NA};
#else
    regNumberSmall m_paramRegs[1]{REG_NA};
#endif

    regNumberSmall m_paramInitialReg; // the register into which the argument is loaded at entry

public:
    // The register number is stored in a small format (8 bits), but the getters return and the setters take
    // a full-size (unsigned) format, to localize the casts here.

    /////////////////////

    regNumber GetRegNum() const
    {
        return (regNumber)_lvRegNum;
    }

    void SetRegNum(regNumber reg)
    {
        _lvRegNum = (regNumberSmall)reg;
        assert(_lvRegNum == reg);
    }

    regNumber GetParamReg() const
    {
        return static_cast<regNumber>(m_paramRegs[0]);
    }

#ifdef UNIX_AMD64_ABI
    regNumber GetParamReg(unsigned index) const
    {
        assert(index < _countof(m_paramRegs));

        return static_cast<regNumber>(m_paramRegs[index]);
    }

    void SetParamReg(unsigned index, regNumber reg)
    {
        assert(index < _countof(m_paramRegs));
        assert(reg != REG_STK);

        m_paramRegs[index] = static_cast<regNumberSmall>(reg);
    }

    void SetParamRegs(regNumber reg0, regNumber reg1 = REG_NA)
    {
        assert(lvIsParam);
        assert((reg0 != REG_STK) && (reg0 != REG_NA) && (reg1 != REG_STK));

        lvIsRegArg = true;

        m_paramRegs[0] = static_cast<regNumberSmall>(reg0);
        m_paramRegs[1] = static_cast<regNumberSmall>(reg1);
    }

    void ClearParamRegs()
    {
        assert(lvIsParam);

        lvIsRegArg      = false;
        lvIsMultiRegArg = false;

        m_paramRegs[0] = REG_NA;
        m_paramRegs[1] = REG_NA;
    }
#elif defined(TARGET_XARCH)
    regNumber GetParamReg(unsigned index) const
    {
        assert(index < _countof(m_paramRegs));

        return static_cast<regNumber>(m_paramRegs[index]);
    }

    void SetParamRegs(regNumber reg)
    {
        assert(lvIsParam);
        assert((reg != REG_STK) && (reg != REG_NA));

        lvIsRegArg = true;

        m_paramRegs[0] = static_cast<regNumberSmall>(reg);
    }

    void ClearParamRegs()
    {
        lvIsRegArg      = false;
        lvIsMultiRegArg = false;

        m_paramRegs[0] = REG_NA;
    }
#elif defined(TARGET_ARMARCH)
    regNumber GetParamReg(unsigned index) const
    {
        assert(index < m_paramRegCount);

        return static_cast<regNumber>(m_paramRegs[0] + index);
    }

    void SetParamRegs(regNumber reg0, unsigned regCount = 1)
    {
        assert((reg0 != REG_STK) && (reg0 != REG_NA));
#ifdef TARGET_ARM64
        assert(regCount <= MAX_ARG_REG_COUNT);
#else
        // TODO-MIKE-Cleanup: On ARM we count a DOUBLE reg as 2 FLOAT regs.
        // Can this be avoided somehow? It complicates lvIsMultiRegArg because
        // a HFA with a single DOUBLE element uses 2 regs but lvIsMultiRegArg
        // is false for it.
        assert(regCount <= MAX_ARG_REG_COUNT * 2);
#endif

        lvIsRegArg = true;

        m_paramRegs[0]  = static_cast<regNumberSmall>(reg0);
        m_paramRegCount = regCount;
    }

    void ClearParamRegs()
    {
        lvIsRegArg      = false;
        lvIsMultiRegArg = false;

        m_paramRegs[0]  = REG_NA;
        m_paramRegCount = 0;
    }
#endif // TARGET_ARMARCH

    unsigned GetParamRegCount() const
    {
#ifdef UNIX_AMD64_ABI
        return !lvIsRegArg ? 0 : (1 + (m_paramRegs[1] != REG_NA));
#elif defined(TARGET_XARCH)
        return !lvIsRegArg ? 0 : 1;
#elif defined(TARGET_ARMARCH)
        return m_paramRegCount;
#endif
    }

    regNumber GetParamInitialReg() const
    {
        return static_cast<regNumber>(m_paramInitialReg);
    }

    void SetParamInitialReg(regNumber reg)
    {
        m_paramInitialReg = static_cast<regNumberSmall>(reg);
    }

    // Is this is a SIMD struct which is used for SIMD intrinsic?
    bool lvIsUsedInSIMDIntrinsic() const
    {
#ifdef FEATURE_SIMD
        return lvUsedInSIMDIntrinsic;
#else
        return false;
#endif
    }

    bool IsRegCandidate() const
    {
        return lvLRACandidate != 0;
    }

    bool lvIsInReg() const
    {
        return lvLRACandidate && (GetRegNum() != REG_STK);
    }

    uint16_t lvVarIndex;

    bool HasLiveness() const
    {
        return lvTracked;
    }

    unsigned GetLivenessBitIndex() const
    {
        assert(lvTracked);
        return lvVarIndex;
    }

    void SetHasGCLiveness()
    {
        assert(lvTracked && varTypeIsGC(lvType));
        m_hasGCLiveness = true;
    }

    bool HasGCLiveness() const
    {
        return m_hasGCLiveness;
    }

    bool HasGCSlotLiveness() const
    {
        return m_hasGCLiveness && lvOnFrame;
    }

private:
    uint16_t m_refCount;
    uint32_t m_refWeight;

public:
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
    void     AddImplicitByRefParamAnyRef();
    void     AddImplicitByRefParamCallRef();
    unsigned GetImplicitByRefParamAnyRefCount();
    unsigned GetImplicitByRefParamCallRefCount();
#endif

    unsigned GetRefCount() const;
    void SetRefCount(unsigned count);

    // [[deprecated]]
    unsigned lvRefCnt() const
    {
        return GetRefCount();
    }

    BasicBlock::weight_t GetRefWeight() const;
    void SetRefWeight(BasicBlock::weight_t weight);

    // [[deprecated]]
    BasicBlock::weight_t lvRefCntWtd() const
    {
        return GetRefWeight();
    }

private:
    int lvStkOffs; // stack offset of home in bytes.

public:
    int GetStackOffset() const
    {
        return lvStkOffs;
    }

    void SetStackOffset(int offset)
    {
        lvStkOffs = offset;
    }

    unsigned lvExactSize; // (exact) size of the type in bytes

    // Is this a promoted struct?
    // This method returns true only for structs (including SIMD structs), not for
    // locals that are split on a 32-bit target.
    // It is only necessary to use this:
    //   1) if only structs are wanted, and
    //   2) if Lowering has already been done.
    // Otherwise lvPromoted is valid.
    bool lvPromotedStruct()
    {
#if !defined(TARGET_64BIT)
        return (lvPromoted && !varTypeIsLong(lvType));
#else  // defined(TARGET_64BIT)
        return lvPromoted;
#endif // defined(TARGET_64BIT)
    }

    // TODO-MIKE-Cleanup: Maybe lvImpTypeInfo can be replaced with CORINFO_CLASS_HANDLE
    // since the rest of the bits in typeInfo aren't very useful, they can be recreated
    // from the local's type. Also:
    //   - For primitive type locals this is not supposed to be set/used.
    //   - For struct type locals this is a duplicate of m_layout.
    //   - For REF type locals this is similar to lvClassHnd (but not identical).
    //   - Only "normed type" locals truly need this.
    typeInfo lvImpTypeInfo;

    // class handle for the local or null if not known or not a class,
    // for a struct handle use `GetStructHnd()`.
    CORINFO_CLASS_HANDLE lvClassHnd;

private:
    ClassLayout*  m_layout;   // layout info for structs
    FieldSeqNode* m_fieldSeq; // field sequence for promoted struct fields
public:
#if ASSERTION_PROP
    BlockSet   lvUseBlocks; // Set of blocks that contain uses
    Statement* lvDefStmt;   // Pointer to the statement with the single definition
#endif
private:
    unsigned m_dummy; // Keep the old LclVarDsc size (104 on x64), removing it results in a significant PIN regression
                      // because both MSVC and Clang use a LEA/SHL sequence for the new size (96) instead of IMUL.
                      // In theory LEA/SHL saves one cycle of latency but it's far from clear that the increase in code
                      // size is worth it.

public:
    var_types GetType() const
    {
        return lvType;
    }

    // [[deprecated]]
    var_types TypeGet() const
    {
        return lvType;
    }

    bool TypeIs(var_types type) const
    {
        return lvType == type;
    }

    template <typename... T>
    bool TypeIs(var_types type, T... rest) const
    {
        return TypeIs(type) || TypeIs(rest...);
    }

    void SetType(var_types type)
    {
        assert((TYP_UNDEF < type) && (type < TYP_UNKNOWN) && !varTypeIsStruct(type));
        lvType = type;
    }

    void SetBlockType(unsigned size)
    {
        lvType            = TYP_BLK;
        lvExactSize       = size;
        lvDoNotEnregister = true;
    }

    unsigned GetBlockSize() const
    {
        assert(lvType == TYP_BLK);
        return lvExactSize;
    }

    unsigned GetTypeSize() const
    {
        switch (lvType)
        {
            case TYP_BLK:
                return lvExactSize;
            case TYP_STRUCT:
                return m_layout->GetSize();
            default:
                return varTypeSize(lvType);
        }
    }

    unsigned GetFrameSize() const;

    bool lvNormalizeOnLoad() const
    {
        return varTypeIsSmall(lvType) &&
               // lvIsStructField is treated the same as the aliased local, see fgMorphNormalizeLclVarStore.
               (lvIsParam || lvAddrExposed || lvIsStructField || lvWasStructField);
    }

    bool lvNormalizeOnStore() const
    {
        return varTypeIsSmall(lvType) &&
               // lvIsStructField is treated the same as the aliased local, see fgMorphNormalizeLclVarStore.
               !(lvIsParam || lvAddrExposed || lvIsStructField || lvWasStructField);
    }

    // Returns true if this variable contains GC pointers (including being a GC pointer itself).
    bool HasGCPtr() const
    {
        return varTypeIsGC(lvType) || ((lvType == TYP_STRUCT) && m_layout->HasGCPtr());
    }

    // Returns the layout of a struct variable.
    ClassLayout* GetLayout() const
    {
        assert(varTypeIsStruct(lvType));
        return m_layout;
    }

    // Sets the layout of a struct variable.
    void SetLayout(ClassLayout* layout)
    {
        assert(varTypeIsStruct(lvType));
        m_layout = layout;
    }

    var_types GetRegisterType(const GenTreeLclVarCommon* tree) const;
    var_types GetRegisterType() const;
    var_types GetActualRegisterType() const;

    //-----------------------------------------------------------------------------
    //  IsAlwaysAliveInMemory: Determines if this variable's value is always
    //     up-to-date on stack. This is possible if this is an EH-var or
    //     we decided to spill after single-def.
    //
    bool IsAlwaysAliveInMemory() const
    {
        return lvLiveInOutOfHndlr || lvSpillAtSingleDef;
    }

    INDEBUG(const char* lvReason;)
};

// Information about arrays: their element type and size, and the offset of the first element.
struct ArrayInfo
{
    GenTree*       m_arrayExpr;
    GenTree*       m_elemOffsetExpr;
    GenTreeIntCon* m_elemOffsetConst;
    unsigned       m_elemTypeNum;

    ArrayInfo() : m_arrayExpr(nullptr), m_elemOffsetExpr(nullptr), m_elemOffsetConst(nullptr), m_elemTypeNum(0)
    {
    }
};

// The following enum provides a simple 1:1 mapping to CLR API's
enum API_ICorJitInfo_Names
{
#define DEF_CLR_API(name) API_##name,
#include "ICorJitInfo_API_names.h"
    API_COUNT
};

//---------------------------------------------------------------
// Compilation time.
//

// A "CompTimeInfo" is a structure for tracking the compilation time of one or more methods.
// We divide a compilation into a sequence of contiguous phases, and track the total (per-thread) cycles
// of the compilation, as well as the cycles for each phase.  We also track the number of bytecodes.
// If there is a failure in reading a timer at any point, the "CompTimeInfo" becomes invalid, as indicated
// by "m_timerFailure" being true.
// If FEATURE_JIT_METHOD_PERF is not set, we define a minimal form of this, enough to let other code compile.
struct CompTimeInfo
{
#ifdef FEATURE_JIT_METHOD_PERF
    // The string names of the phases.
    static const char* PhaseNames[];

    static bool PhaseHasChildren[];
    static int  PhaseParent[];
    static bool PhaseReportsIRSize[];

    unsigned         m_byteCodeBytes;
    unsigned __int64 m_totalCycles;
    unsigned __int64 m_invokesByPhase[PHASE_NUMBER_OF];
    unsigned __int64 m_cyclesByPhase[PHASE_NUMBER_OF];
#if MEASURE_CLRAPI_CALLS
    unsigned __int64 m_CLRinvokesByPhase[PHASE_NUMBER_OF];
    unsigned __int64 m_CLRcyclesByPhase[PHASE_NUMBER_OF];
#endif

    unsigned m_nodeCountAfterPhase[PHASE_NUMBER_OF];

    // For better documentation, we call EndPhase on
    // non-leaf phases.  We should also call EndPhase on the
    // last leaf subphase; obviously, the elapsed cycles between the EndPhase
    // for the last leaf subphase and the EndPhase for an ancestor should be very small.
    // We add all such "redundant end phase" intervals to this variable below; we print
    // it out in a report, so we can verify that it is, indeed, very small.  If it ever
    // isn't, this means that we're doing something significant between the end of the last
    // declared subphase and the end of its parent.
    unsigned __int64 m_parentPhaseEndSlop;
    bool             m_timerFailure;

#if MEASURE_CLRAPI_CALLS
    // The following measures the time spent inside each individual CLR API call.
    unsigned         m_allClrAPIcalls;
    unsigned         m_perClrAPIcalls[API_ICorJitInfo_Names::API_COUNT];
    unsigned __int64 m_allClrAPIcycles;
    unsigned __int64 m_perClrAPIcycles[API_ICorJitInfo_Names::API_COUNT];
    unsigned __int32 m_maxClrAPIcycles[API_ICorJitInfo_Names::API_COUNT];
#endif // MEASURE_CLRAPI_CALLS

    CompTimeInfo(unsigned byteCodeBytes);
#endif
};

#ifdef FEATURE_JIT_METHOD_PERF

#if MEASURE_CLRAPI_CALLS
struct WrapICorJitInfo;
#endif

// This class summarizes the JIT time information over the course of a run: the number of methods compiled,
// and the total and maximum timings.  (These are instances of the "CompTimeInfo" type described above).
// The operation of adding a single method's timing to the summary may be performed concurrently by several
// threads, so it is protected by a lock.
// This class is intended to be used as a singleton type, with only a single instance.
class CompTimeSummaryInfo
{
    // This lock protects the fields of all CompTimeSummaryInfo(s) (of which we expect there to be one).
    static CritSecObject s_compTimeSummaryLock;

    int          m_numMethods;
    int          m_totMethods;
    CompTimeInfo m_total;
    CompTimeInfo m_maximum;

    int          m_numFilteredMethods;
    CompTimeInfo m_filtered;

    // This can use what ever data you want to determine if the value to be added
    // belongs in the filtered section (it's always included in the unfiltered section)
    bool IncludedInFilteredData(CompTimeInfo& info);

public:
    // This is the unique CompTimeSummaryInfo object for this instance of the runtime.
    static CompTimeSummaryInfo s_compTimeSummary;

    CompTimeSummaryInfo()
        : m_numMethods(0), m_totMethods(0), m_total(0), m_maximum(0), m_numFilteredMethods(0), m_filtered(0)
    {
    }

    // Assumes that "info" is a completed CompTimeInfo for a compilation; adds it to the summary.
    // This is thread safe.
    void AddInfo(CompTimeInfo& info, bool includePhases);

    // Print the summary information to "f".
    // This is not thread-safe; assumed to be called by only one thread.
    void Print(FILE* f);
};

// A JitTimer encapsulates a CompTimeInfo for a single compilation. It also tracks the start of compilation,
// and when the current phase started.  This is intended to be part of a Compilation object.
//
class JitTimer
{
    unsigned __int64 m_start;         // Start of the compilation.
    unsigned __int64 m_curPhaseStart; // Start of the current phase.
#if MEASURE_CLRAPI_CALLS
    unsigned __int64 m_CLRcallStart;   // Start of the current CLR API call (if any).
    unsigned __int64 m_CLRcallInvokes; // CLR API invokes under current outer so far
    unsigned __int64 m_CLRcallCycles;  // CLR API  cycles under current outer so far.
    int              m_CLRcallAPInum;  // The enum/index of the current CLR API call (or -1).
    static double    s_cyclesPerSec;   // Cached for speedier measurements
#endif
#ifdef DEBUG
    Phases m_lastPhase; // The last phase that was completed (or (Phases)-1 to start).
#endif
    CompTimeInfo m_info; // The CompTimeInfo for this compilation.

    static CritSecObject s_csvLock; // Lock to protect the time log file.
    static FILE*         s_csvFile; // The time log file handle.
    void PrintCsvMethodStats(Compiler* comp);

private:
    void* operator new(size_t);
    void* operator new[](size_t);
    void operator delete(void*);
    void operator delete[](void*);

public:
    // Initialized the timer instance
    JitTimer(unsigned byteCodeSize);

    static JitTimer* Create(Compiler* comp, unsigned byteCodeSize)
    {
        return ::new (comp, CMK_DebugOnly) JitTimer(byteCodeSize);
    }

    static void PrintCsvHeader();

    // Ends the current phase (argument is for a redundant check).
    void EndPhase(Compiler* compiler, Phases phase);

#if MEASURE_CLRAPI_CALLS
    // Start and end a timed CLR API call.
    void CLRApiCallEnter(unsigned apix);
    void CLRApiCallLeave(unsigned apix);
#endif // MEASURE_CLRAPI_CALLS

    // Completes the timing of the current method, which is assumed to have "byteCodeBytes" bytes of bytecode,
    // and adds it to "sum".
    void Terminate(Compiler* comp, CompTimeSummaryInfo& sum, bool includePhases);

    // Attempts to query the cycle counter of the current thread.  If successful, returns "true" and sets
    // *cycles to the cycle counter value.  Otherwise, returns false and sets the "m_timerFailure" flag of
    // "m_info" to true.
    bool GetThreadCycles(unsigned __int64* cycles)
    {
        bool res = CycleTimer::GetThreadCyclesS(cycles);
        if (!res)
        {
            m_info.m_timerFailure = true;
        }
        return res;
    }

    static void Shutdown();
};
#endif // FEATURE_JIT_METHOD_PERF

//------------------- Function/Funclet info -------------------------------
enum FuncKind : BYTE
{
    FUNC_ROOT,    // The main/root function (always id==0)
    FUNC_HANDLER, // a funclet associated with an EH handler (finally, fault, catch, filter handler)
    FUNC_FILTER,  // a funclet associated with an EH filter
    FUNC_COUNT
};

class emitLocation;

struct FuncInfoDsc
{
    FuncKind       funKind;
    BYTE           funFlags;   // Currently unused, just here for padding
    unsigned short funEHIndex; // index, into the ebd table, of innermost EH clause corresponding to this
                               // funclet. It is only valid if funKind field indicates this is a
                               // EH-related funclet: FUNC_HANDLER or FUNC_FILTER

#if defined(TARGET_AMD64)

    // TODO-AMD64-Throughput: make the AMD64 info more like the ARM info to avoid having this large static array.
    emitLocation* startLoc;
    emitLocation* endLoc;
    emitLocation* coldStartLoc; // locations for the cold section, if there is one.
    emitLocation* coldEndLoc;
    UNWIND_INFO   unwindHeader;
    // Maximum of 255 UNWIND_CODE 'nodes' and then the unwind header. If there are an odd
    // number of codes, the VM or Zapper will 4-byte align the whole thing.
    BYTE     unwindCodes[offsetof(UNWIND_INFO, UnwindCode) + (0xFF * sizeof(UNWIND_CODE))];
    unsigned unwindCodeSlot;

#elif defined(TARGET_X86)

#if defined(TARGET_UNIX)
    emitLocation* startLoc;
    emitLocation* endLoc;
    emitLocation* coldStartLoc; // locations for the cold section, if there is one.
    emitLocation* coldEndLoc;
#endif // TARGET_UNIX

#elif defined(TARGET_ARMARCH)

    UnwindInfo  uwi;     // Unwind information for this function/funclet's hot  section
    UnwindInfo* uwiCold; // Unwind information for this function/funclet's cold section
                         //   Note: we only have a pointer here instead of the actual object,
                         //   to save memory in the JIT case (compared to the NGEN case),
                         //   where we don't have any cold section.
                         //   Note 2: we currently don't support hot/cold splitting in functions
                         //   with EH, so uwiCold will be NULL for all funclets.

#if defined(TARGET_UNIX)
    emitLocation* startLoc;
    emitLocation* endLoc;
    emitLocation* coldStartLoc; // locations for the cold section, if there is one.
    emitLocation* coldEndLoc;
#endif // TARGET_UNIX

#endif // TARGET_ARMARCH

#if defined(TARGET_UNIX)
    jitstd::vector<CFI_CODE>* cfiCodes;
#endif // TARGET_UNIX

    // Eventually we may want to move rsModifiedRegsMask, lvaOutgoingArgSize, and anything else
    // that isn't shared between the main function body and funclets.
};

//-------------------------------------------------------------------------
// LoopFlags: flags for the loop table.
//
enum LoopFlags : uint16_t
{
    LPFLG_EMPTY = 0,

    LPFLG_DO_WHILE = 0x0001, // it's a do-while loop (i.e ENTRY is at the TOP)
    LPFLG_ONE_EXIT = 0x0002, // the loop has only one exit
    LPFLG_HAS_CALL = 0x0008,

    LPFLG_VAR_INIT   = 0x0020, // iterator is initialized with a local var (var # found in lpVarInit)
    LPFLG_CONST_INIT = 0x0040, // iterator is initialized with a constant (found in lpConstInit)
    LPFLG_SIMD_LIMIT = 0x0080, // iterator is compared with vector element count (found in lpConstLimit)

    LPFLG_VAR_LIMIT    = 0x0100, // iterator is compared with a local var (var # found in lpVarLimit)
    LPFLG_CONST_LIMIT  = 0x0200, // iterator is compared with a constant (found in lpConstLimit)
    LPFLG_ARRLEN_LIMIT = 0x0400, // iterator is compared with a.len or a[i].len (found in lpArrLenLimit)
    LPFLG_HAS_PREHEAD  = 0x0800, // lpHead is known to be a preHead for this loop

    LPFLG_REMOVED     = 0x1000, // has been removed from the loop table (unrolled or optimized away)
    LPFLG_DONT_UNROLL = 0x2000, // do not unroll this loop
};

inline constexpr LoopFlags operator~(LoopFlags a)
{
    return (LoopFlags)(~(uint16_t)a);
}

inline constexpr LoopFlags operator|(LoopFlags a, LoopFlags b)
{
    return (LoopFlags)((uint16_t)a | (uint16_t)b);
}

inline constexpr LoopFlags operator&(LoopFlags a, LoopFlags b)
{
    return (LoopFlags)((uint16_t)a & (uint16_t)b);
}

inline LoopFlags& operator|=(LoopFlags& a, LoopFlags b)
{
    return a = (LoopFlags)((uint16_t)a | (uint16_t)b);
}

inline LoopFlags& operator&=(LoopFlags& a, LoopFlags b)
{
    return a = (LoopFlags)((uint16_t)a & (uint16_t)b);
}

struct HWIntrinsicInfo;

struct CompiledMethodInfo
{
    ICorJitInfo*          compCompHnd;
    CORINFO_MODULE_HANDLE compScopeHnd;
    CORINFO_CLASS_HANDLE  compClassHnd;
    CORINFO_METHOD_HANDLE compMethodHnd;
    CORINFO_METHOD_INFO*  compMethodInfo;

#if defined(DEBUG) || defined(LATE_DISASM) || DUMP_FLOWGRAPHS
    const char* compMethodName = nullptr;
    const char* compClassName  = nullptr;
    const char* compFullName   = nullptr;
    double      compPerfScore  = 0.0;
    int         compMethodSuperPMIIndex; // useful when debugging under SuperPMI
#endif

#if defined(DEBUG) || defined(INLINE_DATA)
    mutable unsigned compMethodHashPrivate = 0;
    unsigned         compMethodHash() const;
#endif

#ifdef PSEUDORANDOM_NOP_INSERTION
    unsigned  compChecksum;
    CLRRandom compRNG;
#endif

    const uint8_t*  compCode;           // IL code
    PatchpointInfo* compPatchpointInfo; // Patchpoint data for OSR (normally nullptr)

    IL_OFFSET compILCodeSize; // The IL code size
    IL_OFFSET compILEntry;    // The IL entry point (normally 0)

    unsigned compILargsCount; // Number of arguments (incl. implicit but not hidden)
    unsigned compArgsCount;   // Number of arguments (incl. implicit and hidden)
    unsigned compMaxStack;

    // Number of exception-handling clauses read in the method's IL.
    // You should generally use compHndBBtabCount instead: it is the
    // current number of EH clauses (after additions like synchronized
    // methods and funclets, and removals like unreachable code deletion).
    unsigned compXcptnsCount;

    // The following holds the FLG_xxxx flags for the method we're compiling.
    unsigned compFlags;
    // The following holds the class attributes for the method we're compiling.
    unsigned compClassAttr;

    unsigned compRetBuffArg;  // position of hidden return param var (0, 1) (BAD_VAR_NUM means not present);
    unsigned compTypeCtxtArg; // position of hidden param for type context for generic code (CORINFO_CALLCONV_PARAMTYPE)
    unsigned compThisArg;     // position of implicit this pointer param (not to be confused with lvaThisLclNum)
    unsigned compLocalsCount; // Number of vars : args + locals (incl. implicit and     hidden)
    unsigned compTotalHotCodeSize                   = 0; // Total number of bytes of Hot Code in the method
    unsigned compTotalColdCodeSize                  = 0; // Total number of bytes of Cold Code in the method
    unsigned compUnmanagedCallCountWithGCTransition = 0; // count of unmanaged calls with GC transition.
    unsigned compClassProbeCount                    = 0; // Number of class profile probes in this method

    // The native code size, after instructions are issued.
    // This is less than (compTotalHotCodeSize + compTotalColdCodeSize) only if:
    // (1) the code is not hot/cold split, and we issued less code than we expected, or
    // (2) the code is hot/cold split, and we issued less code than we expected
    // in the cold section (the hot section will always be padded out to compTotalHotCodeSize).
    unsigned compNativeCodeSize = 0;

    unsigned     compVarScopesCount;
    VarScopeDsc* compVarScopes;

    CorInfoCallConvExtension compCallConv; // The entry-point calling convention for this method.
    regNumber                virtualStubParamRegNum;

    bool compIsStatic : 1;           // Is the method static (no 'this' pointer)?
    bool compIsVarArgs : 1;          // Does the method have varargs parameters?
    bool compInitMem : 1;            // Is the CORINFO_OPT_INIT_LOCALS bit set in the method info options?
    bool compProfilerCallback : 1;   // JIT inserted a profiler Enter callback
    bool compPublishStubParam : 1;   // EAX captured in prolog will be available through an intrinsic
    bool compHasNextCallRetAddr : 1; // The NextCallReturnAddress intrinsic is used.
    bool compMatchedVM : 1;          // true if the VM is "matched": either the JIT is a cross-compiler
                                     // and the VM expects that, or the JIT is a "self-host" compiler
                                     // (e.g., x86 hosted targeting x86) and the VM expects that.

    var_types      compRetType; // Return type of the method as declared in IL
    ReturnTypeDesc retDesc;
    ClassLayout*   retLayout;

    var_types GetRetSigType() const
    {
        return compRetType;
    }

    ClassLayout* GetRetLayout() const
    {
        assert(varTypeIsStruct(compRetType));
        return retLayout;
    }

    unsigned GetParamCount() const
    {
        return compArgsCount;
    }

    unsigned GetThisParamLclNum() const
    {
        return compThisArg;
    }

    bool ThisParamIsGenericsContext() const
    {
        return (compMethodInfo->options & CORINFO_GENERICS_CTXT_FROM_THIS) != 0;
    }

    INDEBUG(bool SkipMethod() const;)

    CompiledMethodInfo(CORINFO_METHOD_INFO* methodInfo, ICorJitInfo* jitInfo, const CORINFO_EE_INFO* eeInfo);
};

enum codeOptimize : uint8_t
{
    BLENDED_CODE,
    SMALL_CODE,
    FAST_CODE,

    COUNT_OPT_CODE
};

enum OptFlags : uint8_t
{
    CLFLG_REGVAR        = 0x01,
    CLFLG_TREETRANS     = 0x02,
    CLFLG_INLINING      = 0x04,
    CLFLG_STRUCTPROMOTE = 0x08,
    CLFLG_CONSTANTFOLD  = 0x10,

    CLFLG_MINOPT = CLFLG_TREETRANS,
    CLFLG_MAXOPT = CLFLG_REGVAR | CLFLG_TREETRANS | CLFLG_INLINING | CLFLG_STRUCTPROMOTE | CLFLG_CONSTANTFOLD
};

struct CompilerOptions
{
    JitFlags* jitFlags; // all flags passed from the EE

    // The instruction sets that the compiler is allowed to emit.
    uint64_t compSupportsISA;
    // The instruction sets that were reported to the VM as being used by the current method. Subset of
    // compSupportsISA.
    uint64_t compSupportsISAReported;
    // The instruction sets that the compiler is allowed to take advantage of implicitly during optimizations.
    // Subset of compSupportsISA.
    // The instruction sets available in compSupportsISA and not available in compSupportsISAExactly can be only
    // used via explicit hardware intrinsics.
    uint64_t compSupportsISAExactly;

    void setSupportedISAs(CORINFO_InstructionSetFlags isas)
    {
        compSupportsISA = isas.GetFlagsRaw();
    }

#ifdef TARGET_ARM64
    // Decision about whether to save FP/LR registers with callee-saved registers (see
    // COMPlus_JitSaveFpLrWithCalleSavedRegisters).
    int compJitSaveFpLrWithCalleeSavedRegisters;
#endif

    OptFlags     optFlags : 6;
    bool         framePointerRequired : 1;
    codeOptimize compCodeOpt; // what type of code optimizations

// optimize maximally and/or favor speed over size?

#define DEFAULT_MIN_OPTS_CODE_SIZE 60000
#define DEFAULT_MIN_OPTS_INSTR_COUNT 20000
#define DEFAULT_MIN_OPTS_BB_COUNT 2000
#define DEFAULT_MIN_OPTS_LV_NUM_COUNT 2000
#define DEFAULT_MIN_OPTS_LV_REF_COUNT 8000

// Maximun number of locals before turning off the inlining
#define MAX_LV_NUM_COUNT_FOR_INLINING 512

    bool compMinOpts : 1;
    bool compMinOptsIsSet : 1;
#ifdef DEBUG
    mutable bool compMinOptsIsUsed;

    bool MinOpts() const
    {
        assert(compMinOptsIsSet);
        compMinOptsIsUsed = true;
        return compMinOpts;
    }
    bool IsMinOptsSet()
    {
        return compMinOptsIsSet;
    }
#else  // !DEBUG
    bool          MinOpts() const
    {
        return compMinOpts;
    }
    bool IsMinOptsSet()
    {
        return compMinOptsIsSet;
    }
#endif // !DEBUG

    bool OptimizationDisabled() const
    {
        return MinOpts() || compDbgCode;
    }
    bool OptimizationEnabled() const
    {
        return !OptimizationDisabled();
    }

    void SetMinOpts(bool val)
    {
        assert(!compMinOptsIsUsed);
        assert(!compMinOptsIsSet || (compMinOpts == val));
        compMinOpts      = val;
        compMinOptsIsSet = true;
    }

    bool OptEnabled(OptFlags optFlag) const
    {
        return (optFlags & optFlag) != 0;
    }

    bool IsReadyToRun() const
    {
#ifdef FEATURE_READYTORUN_COMPILER
        return jitFlags->IsSet(JitFlags::JIT_FLAG_READYTORUN);
#else
        return false;
#endif
    }

    bool IsOSR() const
    {
#ifdef FEATURE_ON_STACK_REPLACEMENT
        return jitFlags->IsSet(JitFlags::JIT_FLAG_OSR);
#else
        return false;
#endif
    }

    // true if we should use the PINVOKE_{BEGIN,END} helpers instead of generating
    // PInvoke transitions inline. Normally used by R2R, but also used when generating a reverse pinvoke frame, as
    // the current logic for frame setup initializes and pushes
    // the InlinedCallFrame before performing the Reverse PInvoke transition, which is invalid (as frames cannot
    // safely be pushed/popped while the thread is in a preemptive state.).
    bool ShouldUsePInvokeHelpers() const
    {
        return jitFlags->IsSet(JitFlags::JIT_FLAG_USE_PINVOKE_HELPERS) ||
               jitFlags->IsSet(JitFlags::JIT_FLAG_REVERSE_PINVOKE);
    }

    // true if we should use insert the REVERSE_PINVOKE_{ENTER,EXIT} helpers in the method
    // prolog/epilog
    bool IsReversePInvoke() const
    {
        return jitFlags->IsSet(JitFlags::JIT_FLAG_REVERSE_PINVOKE);
    }

    bool IsFramePointerRequired() const
    {
        return framePointerRequired;
    }

    void SetFramePointerRequired()
    {
        framePointerRequired = true;
    }

    bool compScopeInfo : 1; // Generate the LocalVar info ?
    bool compDbgCode : 1;   // Generate debugger-friendly code?
    bool compDbgInfo : 1;   // Gather debugging info?
    bool compDbgEnC : 1;

#ifdef PROFILING_SUPPORTED
    bool compNoPInvokeInlineCB : 1;
    // Whether to emit Enter/Leave/TailCall hooks using a dummy stub (DummyProfilerELTStub()).
    // This option helps make the JIT behave as if it is running under a profiler.
    bool compJitELTHookEnabled : 1;
#else
    static const bool compNoPInvokeInlineCB;
#endif

    bool compReloc : 1;              // Generate relocs for pointers in code, true for all ngen/prejit codegen
    bool compProcedureSplitting : 1; // Separate cold code from hot code
    bool altJit : 1;                 // True if we are an altjit and are compiling this method

#ifdef OPT_CONFIG
    bool optRepeat : 1; // Repeat optimizer phases k times
#endif

#ifdef DEBUG
    // Check arguments and return values to ensure they are sane
    bool compGcChecks : 1;
#ifdef TARGET_XARCH
    // Check stack pointer on return to ensure it is correct.
    bool compStackCheckOnRet : 1;
#endif
    // Check stack pointer after call to ensure it is correct.
    X86_ONLY(bool compStackCheckOnCall : 1;)
    // Whether absolute addr be encoded as RIP relative displacement where possible
    AMD64_ONLY(bool enableRIPRelativeAddressing : 1;)
    bool compProcedureSplittingEH : 1; // Separate cold code from hot code for functions with EH
    bool dspCode : 1;                  // Display native code generated
    bool dspEHTable : 1;               // Display the EH table reported to the VM
    bool dspDebugInfo : 1;             // Display the Debug info reported to the VM
    bool dspInstrs : 1;                // Display the IL instructions intermixed with the native code output
    bool dmpHex : 1;                   // Display raw bytes in hex of native code output
    bool disAsm : 1;                   // Display native code as it is generated
    bool disasmWithGC : 1;             // Display GC info interleaved with disassembly.
    bool disDiffable : 1;              // Makes the Disassembly code 'diff-able'
    bool disAddr : 1;                  // Display process address next to each instruction in disassembly code
    bool disAlignment : 1;             // Display alignment boundaries in disassembly code
    bool dspOrder : 1;                 // Display names of each of the methods that we ngen/jit
    bool dspUnwind : 1;                // Display the unwind info output
    bool dspDiffable : 1;     // Makes the Jit Dump 'diff-able' (currently uses same COMPlus_* flag as disDiffable)
    bool dspGCtbls : 1;       // Display the GC tables
    bool isAltJitPresent : 1; // And AltJit may be present, dump options apply only to it.
#endif
#ifdef LATE_DISASM
    bool doLateDisasm : 1; // Run the late disassembler
#endif

    bool compExpandCallsEarly : 1; // True if we should expand virtual call targets early for this method

#if FEATURE_TAILCALL_OPT
    // Whether optimization of transforming a recursive tail call into a loop is enabled.
    bool compTailCallLoopOpt : 1;
#endif

#if FEATURE_FASTTAILCALL
    // Whether fast tail calls are allowed.
    bool compFastTailCalls : 1;
#endif

    ARM_ONLY(bool compUseSoftFP : 1;)

// Default numbers used to perform loop alignment. All the numbers are choosen
// based on experimenting with various benchmarks.

// Default minimum loop block weight required to enable loop alignment.
#define DEFAULT_ALIGN_LOOP_MIN_BLOCK_WEIGHT 4

// By default a loop will be aligned at 32B address boundary to get better
// performance as per architecture manuals.
#define DEFAULT_ALIGN_LOOP_BOUNDARY 0x20

// For non-adaptive loop alignment, by default, only align a loop whose size is
// at most 3 times the alignment block size. If the loop is bigger than that, it is most
// likely complicated enough that loop alignment will not impact performance.
#define DEFAULT_MAX_LOOPSIZE_FOR_ALIGN DEFAULT_ALIGN_LOOP_BOUNDARY * 3

#ifdef DEBUG
    // Loop alignment variables

    // If set, for non-adaptive alignment, ensure loop jmps are not on or cross alignment boundary.
    bool compJitAlignLoopForJcc : 1;
#endif
    // If set, perform adaptive loop alignment that limits number of padding based on loop size.
    bool compJitAlignLoopAdaptive : 1;

    // For non-adaptive alignment, minimum loop size (in bytes) for which alignment will be done.
    uint16_t compJitAlignLoopMaxCodeSize;

    // Minimum weight needed for the first block of a loop to make it a candidate for alignment.
    uint16_t compJitAlignLoopMinBlockWeight;

    // For non-adaptive alignment, address boundary (power of 2) at which loop alignment should
    // be done. By default, 32B.
    uint16_t compJitAlignLoopBoundary;

    // Padding limit to align a loop.
    uint16_t compJitAlignPaddingLimit;

#if DUMP_GC_TABLES && !defined(DEBUG)
#pragma message("NOTE: this non-debug build has GC ptr table dumping always enabled!")
    static const bool dspGCtbls = true;
#endif

    bool UseSoftFP()
    {
#ifdef TARGET_ARM
        return compUseSoftFP;
#else
        return false;
#endif
    }

    bool UseHfa()
    {
#if defined(TARGET_ARM)
        return !compUseSoftFP;
#elif defined(TARGET_ARM64)
        return true;
#else
        return false;
#endif
    }
};

#ifdef FEATURE_SIMD
class SIMDCoalescingBuffer
{
    Statement* m_firstStmt;
    Statement* m_lastStmt;
    unsigned   m_lclNum;
    unsigned   m_index;

    unsigned IsSimdLocalField(GenTree* node, Compiler* compiler);
    unsigned IsSimdLocalExtract(GenTree* node);

    bool Add(Compiler* compiler, Statement* stmt, GenTreeOp* asg, unsigned simdLclNum);

public:
    SIMDCoalescingBuffer() : m_index(0)
    {
    }

    static bool AreContiguousMemoryLocations(GenTree* l1, GenTree* l2);
    static void ChangeToSIMDMem(Compiler* compiler, GenTree* tree, var_types simdType);

    void Mark(Compiler* compiler, Statement* stmt);
    bool Add(Compiler* compiler, Statement* stmt);
    void Coalesce(Compiler* compiler, BasicBlock* block);

    void Clear()
    {
        m_index = 0;
    }
};
#endif // FEATURE_SIMD

enum class BoxPattern
{
    None,
    BoxUnbox,
    BoxBranch,
    BoxCastBranch,
    BoxCastUnbox
};

struct Importer
{
    struct StackEntry
    {
        GenTree* val;
        typeInfo seTypeInfo;
    };

    struct Stack
    {
        static constexpr unsigned MinSize = 16;

        unsigned const    maxStack;
        unsigned          esStackDepth;
        StackEntry* const esStack;

        Stack(Compiler* compiler);
    };

    // For prefixFlags
    enum
    {
        PREFIX_TAILCALL_EXPLICIT = 0x00000001, // call has "tail" IL prefix
        PREFIX_TAILCALL_IMPLICIT =
            0x00000010, // call is treated as having "tail" prefix even though there is no "tail" IL prefix
        PREFIX_TAILCALL_STRESS =
            0x00000100, // call doesn't "tail" IL prefix but is treated as explicit because of tail call stress
        PREFIX_TAILCALL    = (PREFIX_TAILCALL_EXPLICIT | PREFIX_TAILCALL_IMPLICIT | PREFIX_TAILCALL_STRESS),
        PREFIX_VOLATILE    = 0x00001000,
        PREFIX_UNALIGNED   = 0x00010000,
        PREFIX_CONSTRAINED = 0x00100000,
        PREFIX_READONLY    = 0x01000000
    };

    Compiler* const              comp;
    CORINFO_CONTEXT_HANDLE const impTokenLookupContextHandle;
    InlineInfo* const            impInlineInfo;
    InlineResult* const          compInlineResult;
#ifdef DEBUG
    bool const verbose;
#endif
    CompilerOptions&    opts;
    CompiledMethodInfo& info;

    BasicBlock* currentBlock = nullptr;
    Statement*  impStmtList  = nullptr; // Statements for the BB being imported.
    Statement*  impLastStmt  = nullptr; // The last statement for the current BB.

    bool     impBoxTempInUse = false;
    unsigned impBoxTemp      = BAD_VAR_NUM;

    // IL offset of the stmt currently being imported. It gets set to
    // BAD_IL_OFFSET after it has been set in the appended trees. Then it gets
    // updated at IL offsets for which we have to report mapping info.
    // It also includes flag bits, so use jitGetILoffs()
    // to get the actual IL offset value.
    IL_OFFSETX impCurStmtOffs;

    // A free list of linked list nodes used to represent to-do stacks of basic blocks.
    struct BlockListNode
    {
        BasicBlock*    m_blk;
        BlockListNode* m_next;
        BlockListNode(BasicBlock* blk, BlockListNode* next = nullptr) : m_blk(blk), m_next(next)
        {
        }
        void* operator new(size_t sz, Importer* importer);
    };

    BlockListNode* impBlockListNodeFreeList = nullptr;
    BlockListNode* impPendingBlockStack     = nullptr;

    Stack verCurrentState;

#ifdef FEATURE_SIMD
    SIMDCoalescingBuffer m_impSIMDCoalescingBuffer;
#endif

    IL_OFFSET*                   compStmtOffsets; // sorted
    unsigned                     compStmtOffsetsCount    = 0;
    ICorDebugInfo::BoundaryTypes compStmtOffsetsImplicit = ICorDebugInfo::NO_BOUNDARIES;

    static constexpr unsigned CHECK_SPILL_ALL  = UINT32_MAX;
    static constexpr unsigned CHECK_SPILL_NONE = 0;

    Importer(Compiler* compiler);

    CompAllocator getAllocator(CompMemKind kind = CMK_Generic);

    void InitDebuggingInfo();
    void eeGetStmtOffsets();

    codeOptimize compCodeOpt();
    bool IsTargetAbi(CORINFO_RUNTIME_ABI abi);
    bool      compIsForInlining();
    bool      compDonotInline();
    Compiler* impInlineRoot();
    bool      supportSIMDTypes();
    bool      IsBaselineSimdIsaSupported();
    bool compExactlyDependsOn(CORINFO_InstructionSet isa);
    bool compOpportunisticallyDependsOn(CORINFO_InstructionSet isa);
    bool IsIntrinsicImplementedByUserCall(NamedIntrinsic intrinsicName);
    bool IsMathIntrinsic(NamedIntrinsic intrinsicName);
    bool IsMathIntrinsic(GenTree* tree);
    void setMethodHasExpRuntimeLookup();
    void setMethodHasGuardedDevirtualization();
    INDEBUG(bool compTailCallStress();)

    NamedIntrinsic lookupNamedIntrinsic(CORINFO_METHOD_HANDLE method);
#ifdef FEATURE_HW_INTRINSICS
    NamedIntrinsic impFindSysNumSimdIntrinsic(CORINFO_METHOD_HANDLE method,
                                              const char*           className,
                                              const char*           methodName,
                                              const char*           enclosingClassName);
#endif

    FieldSeqStore* GetFieldSeqStore();
    FieldSeqNode*  GetRefanyTypeField();
    FieldSeqNode*  GetRefanyValueField();
    FieldSeqNode* GetByReferenceValueField(CORINFO_FIELD_HANDLE byRefFieldHandle);

    static bool jitIsBetween(unsigned value, unsigned start, unsigned end);

    void        fgInitBBLookup();
    BasicBlock* fgLookupBB(unsigned offs);

    BasicBlock* bbNewBasicBlock(BBjumpKinds jumpKind);
    BasicBlock* fgNewBBbefore(BBjumpKinds jumpKind, BasicBlock* block, bool extendRegion);
    BasicBlock* fgNewBBafter(BBjumpKinds jumpKind, BasicBlock* block, bool extendRegion);
    BasicBlock* fgNewBBinRegion(BBjumpKinds jumpKind,
                                unsigned    tryIndex,
                                unsigned    hndIndex,
                                BasicBlock* nearBlk,
                                bool        putInFilter = false,
                                bool        runRarely   = false,
                                bool        insertAtEnd = false);
    BasicBlock* fgNewBBinRegion(BBjumpKinds jumpKind,
                                BasicBlock* srcBlk,
                                bool        runRarely   = false,
                                bool        insertAtEnd = false);
    BasicBlock* fgNewBBinRegion(BBjumpKinds jumpKind);

    void fgInsertBBafter(BasicBlock* insertAfter, BasicBlock* block);
    void fgAddCheapPred(BasicBlock* block, BasicBlock* blockPred);
    void fgInsertStmtAtEnd(BasicBlock* block, Statement* stmt);

    bool ehBlockHasExnFlowDsc(BasicBlock* block);
    EHblkDsc* ehGetDsc(unsigned regionIndex);
    bool bbInCatchHandlerILRange(BasicBlock* block);
    bool bbInFilterILRange(BasicBlock* block);
    uint16_t bbFindInnermostCommonTryRegion(BasicBlock* block1, BasicBlock* block2);
    INDEBUG(void fgVerifyHandlerTab();)

    void fgComputeCheapPreds();
    void fgRemovePreds();

    void setNeedsGSSecurityCookie();

    bool tiCompatibleWith(const typeInfo& child, const typeInfo& parent) const;

    void addFatPointerCandidate(GenTreeCall* call);

    void considerGuardedDevirtualization(GenTreeCall*            call,
                                         IL_OFFSETX              iloffset,
                                         bool                    isInterface,
                                         CORINFO_METHOD_HANDLE   baseMethod,
                                         CORINFO_CLASS_HANDLE    baseClass,
                                         CORINFO_CONTEXT_HANDLE* pContextHandle DEBUGARG(CORINFO_CLASS_HANDLE objClass)
                                             DEBUGARG(const char* objClassName));

    void addGuardedDevirtualizationCandidate(GenTreeCall*          call,
                                             CORINFO_METHOD_HANDLE methodHandle,
                                             CORINFO_CLASS_HANDLE  classHandle,
                                             unsigned              methodAttr,
                                             unsigned              classAttr,
                                             unsigned              likelihood);

#ifdef DEBUG
    typeInfo verMakeTypeInfo(CORINFO_CLASS_HANDLE clsHnd);
    typeInfo verMakeTypeInfo(CorInfoType ciType, CORINFO_CLASS_HANDLE clsHnd);

    bool verCheckTailCallConstraint(OPCODE                  opcode,
                                    CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                    CORINFO_RESOLVED_TOKEN* pConstrainedResolvedToken // Is this a "constrained." call
                                    // on a type parameter?
                                    );
#endif

    void Import();

    static OPCODE impGetNonPrefixOpcode(const BYTE* codeAddr, const BYTE* codeEndp);
    static void impValidateMemoryAccessOpcode(OPCODE opcode, bool volatilePrefix);
    static bool impOpcodeIsCallOpcode(OPCODE opcode);

    void impStmtListAppend(Statement* stmt);
    void impStmtListInsertBefore(Statement* stmt, Statement* stmtBefore);
    Statement* impStmtListRemoveLast();
    void impStmtListEnd(BasicBlock* block);
    void impSetBlockStmtList(BasicBlock* block, Statement* firstStmt, Statement* lastStmt);

    void impSetCurrentState(BasicBlock* block);

    INDEBUG(void AppendStmtCheck(GenTree* tree, unsigned chkLevel);)
    void SpillStack(GenTree* tree, unsigned chkLevel);
    Statement* impAppendTree(GenTree* tree, unsigned chkLevel);
    void impSpillAllAppendTree(GenTree* tree);
    void impSpillNoneAppendTree(GenTree* tree);
    void impAppendTempAssign(unsigned lclNum, GenTree* val, unsigned curLevel);
    void impAppendTempAssign(unsigned lclNum, GenTree* val, ClassLayout* layout, unsigned curLevel);
    void impAppendTempAssign(unsigned lclNum, GenTree* val, CORINFO_CLASS_HANDLE structHnd, unsigned curLevel);

    GenTree* impCloneExpr(GenTree* tree, GenTree** clone, unsigned spillCheckLevel DEBUGARG(const char* reason));
    GenTree* impCloneExpr(GenTree*     tree,
                          GenTree**    clone,
                          ClassLayout* layout,
                          unsigned spillCheckLevel DEBUGARG(const char* reason));

    void impMakeMultiUse(GenTree*  tree,
                         unsigned  useCount,
                         GenTree** uses,
                         unsigned spillCheckLevel DEBUGARG(const char* reason));

    template <unsigned useCount>
    void impMakeMultiUse(GenTree* tree,
                         GenTree* (&uses)[useCount],
                         unsigned spillCheckLevel DEBUGARG(const char* reason))
    {
        impMakeMultiUse(tree, useCount, uses, spillCheckLevel DEBUGARG(reason));
    }

    void impMakeMultiUse(GenTree*     tree,
                         unsigned     useCount,
                         GenTree**    uses,
                         ClassLayout* layout,
                         unsigned spillCheckLevel DEBUGARG(const char* reason));

    template <unsigned useCount>
    void impMakeMultiUse(GenTree* tree,
                         GenTree* (&uses)[useCount],
                         ClassLayout* layout,
                         unsigned spillCheckLevel DEBUGARG(const char* reason))
    {
        impMakeMultiUse(tree, useCount, uses, layout, spillCheckLevel DEBUGARG(reason));
    }

    GenTree* impAssignMkRefAny(GenTree* dest, GenTreeOp* mkRefAny, unsigned curLevel);
    GenTree* impAssignStruct(GenTree* dest, GenTree* src, unsigned curLevel);

    GenTree* impGetStructAddr(GenTree* structVal, CORINFO_CLASS_HANDLE structHnd, unsigned curLevel, bool willDeref);

    GenTree* impCanonicalizeStructCallArg(GenTree* arg, ClassLayout* argLayout, unsigned curLevel);

    GenTree* impLookupToTree(CORINFO_RESOLVED_TOKEN* resolvedToken,
                             CORINFO_LOOKUP*         lookup,
                             HandleKind              handleKnd,
                             void*                   compileTimeHandle);

    GenTree* impRuntimeLookupToTree(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                    CORINFO_LOOKUP*         pLookup,
                                    void*                   compileTimeHandle);

    GenTree* impCastClassOrIsInstToTree(GenTree*                op1,
                                        GenTree*                op2,
                                        CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                        bool                    isCastClass);

    GenTree* impOptimizeCastClassOrIsInst(GenTree* op1, CORINFO_RESOLVED_TOKEN* pResolvedToken, bool isCastClass);

    unsigned AdvanceStmtOffset(unsigned nextStmtOffsIndex, unsigned opcodeOffs);
    void impCurStmtOffsSet(IL_OFFSET offs);

    void impNoteBranchOffs();

    unsigned impInitBlockLineInfo(BasicBlock* block);

    GenTree* impCheckForNullPointer(GenTree* obj);
    bool impIsAnySTLOC(OPCODE opcode)
    {
        return ((opcode == CEE_STLOC) || (opcode == CEE_STLOC_S) ||
                ((opcode >= CEE_STLOC_0) && (opcode <= CEE_STLOC_3)));
    }

    GenTreeCall::Use* PopCallArgs(CORINFO_SIG_INFO* sig, GenTree* extraArg = nullptr);

    GenTree* CoerceCallArg(var_types paramType, GenTree* arg);

#ifdef TARGET_X86
    GenTreeCall::Use* ReverseCallArgs(GenTreeCall::Use* args, bool skipReverseCount);
#endif

    IL_OFFSETX GetCallILOffsetX(IL_OFFSET offs);

    //---------------- Spilling the importer stack ----------------------------

    // The maximum number of bytes of IL processed without clean stack state.
    // It allows to limit the maximum tree size and depth.
    static const unsigned MAX_TREE_SIZE = 200;
    bool impCanSpillNow(OPCODE prevOpcode);

    void impSpillStackEntry(unsigned level DEBUGARG(const char* reason));

    void EnsureStackSpilled(bool ignoreLeaves DEBUGARG(const char* reason));
    void SpillCatchArg();
    void impSpillSideEffects(GenTreeFlags spillSideEffects, unsigned chkLevel DEBUGARG(const char* reason));
    void impSpillLclReferences(unsigned lclNum);

    BasicBlock* impPushCatchArgOnStack(BasicBlock* hndBlk, CORINFO_CLASS_HANDLE clsHnd, bool isSingleBlockFilter);
    GenTree* impNewCatchArg();

    bool impBlockIsInALoop(BasicBlock* block);
    void impImportBlockCode(BasicBlock* block);

    void impAddPendingEHSuccessors(BasicBlock* block);

    void impImportBlockPending(BasicBlock* block);
    void impPushPendingBlock(BasicBlock* block);
    bool impIsPendingBlockMember(BasicBlock* block);
    void impSetPendingBlockMember(BasicBlock* block, bool pending);
    BasicBlock* impPopPendingBlock();

    var_types impGetNumericBinaryOpType(genTreeOps oper, bool fUnsigned, GenTree** pOp1, GenTree** pOp2);
    void impAddCompareOpImplicitCasts(bool isUnsigned, GenTree*& op1, GenTree*& op2);
    void impBranchToNextBlock(BasicBlock* block, GenTree* op1, GenTree* op2);

    void ImportSingleBlockMethod(BasicBlock* block);
    void impImportBlock(BasicBlock* block);
    bool impSpillStackAtBlockEnd(BasicBlock* block);

    // Assumes that "block" is a basic block that completes with a non-empty stack. We will assign the values
    // on the stack to local variables (the "spill temp" variables). The successor blocks will assume that
    // its incoming stack contents are in those locals. This requires "block" and its successors to agree on
    // the variables that will be used -- and for all the predecessors of those successors, and the
    // successors of those predecessors, etc. Call such a set of blocks closed under alternating
    // successor/predecessor edges a "spill clique." A block is a "predecessor" or "successor" member of the
    // clique (or, conceivably, both). Each block has a specified sequence of incoming and outgoing spill
    // temps. If "block" already has its outgoing spill temps assigned (they are always a contiguous series
    // of local variable numbers, so we represent them with the base local variable number), returns that.
    // Otherwise, picks a set of spill temps, and propagates this choice to all blocks in the spill clique of
    // which "block" is a member (asserting, in debug mode, that no block in this clique had its spill temps
    // chosen already. More precisely, that the incoming or outgoing spill temps are not chosen, depending
    // on which kind of member of the clique the block is).
    void impSetSpillCliqueState(BasicBlock* block, ImportSpillCliqueState* state);

    // Assumes that "block" is a basic block that completes with a non-empty stack. We have previously
    // assigned the values on the stack to local variables (the "spill temp" variables). The successor blocks
    // will assume that its incoming stack contents are in those locals. This requires "block" and its
    // successors to agree on the variables and their types that will be used.  The CLI spec allows implicit
    // conversions between 'int' and 'native int' or 'float' and 'double' stack types. So one predecessor can
    // push an int and another can push a native int.  For 64-bit we have chosen to implement this by typing
    // the "spill temp" as native int, and then importing (or re-importing as needed) so that all the
    // predecessors in the "spill clique" push a native int (sign-extending if needed), and all the
    // successors receive a native int. Similarly float and double are unified to double.
    // This routine is called after a type-mismatch is detected, and it will walk the spill clique to mark
    // blocks for re-importation as appropriate (both successors, so they get the right incoming type, and
    // predecessors, so they insert an upcast if needed).
    void impReimportSpillClique(BasicBlock* block);

    enum SpillCliqueDir
    {
        SpillCliquePred,
        SpillCliqueSucc
    };

    // Abstract class for receiving a callback while walking a spill clique
    class SpillCliqueWalker
    {
    public:
        virtual void Visit(SpillCliqueDir predOrSucc, BasicBlock* blk) = 0;
    };

    // This is the heart of the algorithm for walking spill cliques. It invokes callback->Visit for each
    // predecessor or successor within the spill clique
    void impWalkSpillCliqueFromPred(BasicBlock* pred, SpillCliqueWalker* callback);

    INDEBUG(bool impIsSpillCliquePredMember(BasicBlock* block);)
    bool impAddSpillCliqueSuccMember(BasicBlock* block);
    bool impAddSpillCliquePredMember(BasicBlock* block);

    void impPushLclVar(unsigned lclNum);
    void impLoadArg(unsigned ilArgNum);
    void impLoadLoc(unsigned ilLclNum);
    bool impInlineReturnInstruction();
    void impReturnInstruction(INDEBUG(bool isTailcall = false));

    void FreeBlockListNode(BlockListNode* node);

#ifdef FEATURE_HW_INTRINSICS
    GenTree* impSIMDPopStack(var_types type);
    GenTree* impPopStackAddrAsVector(var_types type);

    GenTree* impHWIntrinsic(NamedIntrinsic        intrinsic,
                            CORINFO_CLASS_HANDLE  clsHnd,
                            CORINFO_METHOD_HANDLE method,
                            CORINFO_SIG_INFO*     sig,
                            bool                  mustExpand);

    GenTree* impVectorGetElement(ClassLayout* layout, GenTree* op1, GenTree* op2);

    GenTree* impImportSysNumSimdIntrinsic(NamedIntrinsic        intrinsic,
                                          CORINFO_CLASS_HANDLE  clsHnd,
                                          CORINFO_METHOD_HANDLE method,
                                          CORINFO_SIG_INFO*     sig,
                                          bool                  isNewObj);

    GenTree* impVector234TOne(const HWIntrinsicSignature& sig);
    GenTree* impVectorTCount(const HWIntrinsicSignature& sig, ClassLayout* layout);
    GenTree* impVector234TSpecial(NamedIntrinsic              intrinsic,
                                  const HWIntrinsicSignature& sig,
                                  ClassLayout*                layout,
                                  bool                        isNewObj);
    GenTree* impVector234TCreateBroadcast(const HWIntrinsicSignature& sig, ClassLayout* layout, bool isNewObj);
    GenTree* impVector234Create(const HWIntrinsicSignature& sig, ClassLayout* layout, bool isNewObj);
    GenTree* impVector234CreateExtend(const HWIntrinsicSignature& sig, ClassLayout* layout, bool isNewObj);
    GenTree* impVectorTFromArray(const HWIntrinsicSignature& sig, ClassLayout* layout, bool isNewObj);
    GenTree* impAssignSIMDAddr(GenTree* destAddr, GenTree* src);
    GenTree* impGetArrayElementsAsVector(ClassLayout*    layout,
                                         GenTree*        array,
                                         GenTree*        index,
                                         ThrowHelperKind indexThrowKind,
                                         ThrowHelperKind lastIndexThrowKind);
    GenTree* impVector234TCopyTo(const HWIntrinsicSignature& sig, ClassLayout* layout);
    GenTree* impVectorTGetItem(const HWIntrinsicSignature& sig, ClassLayout* layout);
    GenTree* impVectorTMultiply(const HWIntrinsicSignature& sig);
    GenTree* impVector234TInstanceEquals(const HWIntrinsicSignature& sig);
    GenTree* impVector234Dot(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2);
#ifdef TARGET_ARM64
    GenTree* impVector234TEquals(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool notEqual = false);
    GenTree* impVectorT128ConditionalSelect(const HWIntrinsicSignature& sig, GenTree* mask, GenTree* op1, GenTree* op2);
    GenTree* impVectorT128Sum(const HWIntrinsicSignature& sig, GenTree* op1);
    GenTree* impVectorT128Dot(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2);
    GenTree* impVectorT128Narrow(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2);
    GenTree* impVectorT128Widen(const HWIntrinsicSignature& sig);
    GenTree* impVectorT128MinMax(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool isMax);
#endif
#ifdef TARGET_XARCH
    var_types impVectorTUnsignedCompareAdjust(ClassLayout* layout, var_types eltType, GenTree** op1, GenTree** op2);
    GenTree* impVectorT128LongGreaterThanSse2(ClassLayout* layout, GenTree* op1, GenTree* op2, bool lessThan = false);
    GenTree* impVectorT128ULongGreaterThanSse2(ClassLayout* layout, GenTree* op1, GenTree* op2, bool lessThan = false);
    GenTree* impVector234T128Abs(const HWIntrinsicSignature& sig, GenTree* op1);
    GenTree* impVectorT256Abs(const HWIntrinsicSignature& sig, GenTree* op1);
    GenTree* impVectorTAndNot(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2);
    GenTree* impVectorT128LongEquals(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2);
    GenTree* impVectorT128Compare(const HWIntrinsicSignature& sig,
                                  NamedIntrinsic              intrinsic,
                                  GenTree*                    op1,
                                  GenTree*                    op2);
    GenTree* impVectorT256Compare(const HWIntrinsicSignature& sig,
                                  NamedIntrinsic              intrinsic,
                                  GenTree*                    op1,
                                  GenTree*                    op2);
    GenTree* impVectorT128ConvertUInt32ToSingle(const HWIntrinsicSignature& sig, GenTree* op1);
    GenTree* impVectorT256ConvertUInt32ToSingle(const HWIntrinsicSignature& sig, GenTree* op1);
    GenTree* impVectorT128ConvertInt64ToDouble(const HWIntrinsicSignature& sig, GenTree* op1);
    GenTree* impVectorT256ConvertInt64ToDouble(const HWIntrinsicSignature& sig, GenTree* op1);
    GenTree* impVectorT128ConvertUInt64ToDouble(const HWIntrinsicSignature& sig, GenTree* op1);
    GenTree* impVectorT256ConvertUInt64ToDouble(const HWIntrinsicSignature& sig, GenTree* op1);
    GenTree* impVectorT128ConvertDoubleToInt64(const HWIntrinsicSignature& sig);
    GenTree* impVectorT256ConvertDoubleToInt64(const HWIntrinsicSignature& sig);
    GenTree* impVector23Division(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2);
    GenTree* impVectorT128Sum(const HWIntrinsicSignature& sig);
    GenTree* impVectorT128Sum(GenTree* vec, var_types eltType, var_types retType);
    GenTree* impVectorT256Sum(const HWIntrinsicSignature& sig);
    GenTree* impVectorT128Dot(const HWIntrinsicSignature& sig);
    GenTree* impVectorT256Dot(const HWIntrinsicSignature& sig);
    GenTree* impVectorTMultiplyAddAdjacentByte(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2);
    GenTree* impVectorTMultiplyLong(ClassLayout* layout, GenTree* op1, GenTree* op2);
    GenTree* impVectorTMultiplyByte(ClassLayout* layout, GenTree* op1, GenTree* op2);
    GenTree* impVector234TEquals(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool notEqual = false);
    GenTree* impVectorT128MinMax(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool isMax);
    GenTree* impVectorT256MinMax(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool isMax);
    GenTree* impVectorT128Narrow(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2);
    GenTree* impVectorT256Narrow(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2);
    GenTree* impVectorT128ConditionalSelect(const HWIntrinsicSignature& sig, GenTree* mask, GenTree* op1, GenTree* op2);
    GenTree* impVectorT256ConditionalSelect(const HWIntrinsicSignature& sig, GenTree* mask, GenTree* op1, GenTree* op2);
    GenTree* impVectorT128Widen(const HWIntrinsicSignature& sig);
    GenTree* impVectorT256Widen(const HWIntrinsicSignature& sig);
#endif // TARGET_XARCH

    GenTree* impSpecialIntrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig);

    GenTree* impPopArgForHWIntrinsic(var_types paramType, ClassLayout* paramLayout);
    GenTree* impNonConstFallback(NamedIntrinsic intrinsic, var_types simdType, var_types baseType);
    GenTree* addRangeCheckIfNeeded(
        NamedIntrinsic intrinsic, GenTree* immOp, bool mustExpand, int immLowerBound, int immUpperBound);
    GenTree* addRangeCheckForHWIntrinsic(GenTree* immOp, int immLowerBound, int immUpperBound);

#ifdef TARGET_XARCH
    GenTree* impBaseIntrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig);
    GenTree* impSSEIntrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig);
    GenTree* impAvxOrAvx2Intrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig);
    GenTree* impBMI1OrBMI2Intrinsic(NamedIntrinsic intrinsic, const HWIntrinsicSignature& sig);
#endif // TARGET_XARCH
#endif // FEATURE_HW_INTRINSICS
    GenTree* impArrayAccessIntrinsic(
        CORINFO_CLASS_HANDLE clsHnd, CORINFO_SIG_INFO* sig, int memberRef, bool readonlyCall, NamedIntrinsic name);
    GenTree* impInitializeArrayIntrinsic(CORINFO_SIG_INFO* sig);

    GenTree* impMethodPointer(CORINFO_RESOLVED_TOKEN& resolvedToken, CORINFO_CALL_INFO& callInfo);

    GenTree* impTransformThis(GenTree*                thisPtr,
                              CORINFO_RESOLVED_TOKEN* pConstrainedResolvedToken,
                              CORINFO_THIS_TRANSFORM  transform);

    GenTree* impTypeIsAssignable(GenTree* typeTo, GenTree* typeFrom);
    GenTree* impIntrinsic(GenTree*                newobjThis,
                          CORINFO_SIG_INFO*       sig,
                          unsigned                methodFlags,
                          CORINFO_RESOLVED_TOKEN* resolvedToken,
                          bool                    readonlyCall,
                          bool                    tailCall,
                          CORINFO_RESOLVED_TOKEN* contstrainedResolvedToken,
                          CORINFO_CALL_INFO*      callInfo,
                          CorInfoIntrinsics*      pIntrinsicId,
                          bool*                   isSpecialIntrinsic = nullptr);
    GenTree* impMathIntrinsic(CORINFO_METHOD_HANDLE method,
                              CORINFO_SIG_INFO*     sig,
                              var_types             callType,
                              NamedIntrinsic        intrinsicName,
                              bool                  tailCall);

    bool impIsPrimitive(CorInfoType type);

    void impResolveToken(const BYTE* addr, CORINFO_RESOLVED_TOKEN* resolvedToken, CorInfoTokenKind kind);
    CORINFO_CLASS_HANDLE impResolveClassToken(const BYTE* addr, CorInfoTokenKind kind = CORINFO_TOKENKIND_Class);

    void impPushOnStack(GenTree* tree, typeInfo ti = typeInfo());
    StackEntry impPopStack();
    GenTree* impPopStackCoerceArg(var_types signatureType);
    StackEntry& impStackTop(unsigned n = 0);
    unsigned impStackHeight();

    typeInfo impMakeTypeInfo(CorInfoType type, CORINFO_CLASS_HANDLE classHandle);

    GenTree* impImportLdvirtftn(GenTree* thisPtr, CORINFO_RESOLVED_TOKEN* pResolvedToken, CORINFO_CALL_INFO* pCallInfo);

    bool impImportBoxPattern(BoxPattern              pattern,
                             CORINFO_RESOLVED_TOKEN* resolvedToken,
                             const BYTE* codeAddr DEBUGARG(const BYTE* codeEnd));
    void impImportAndPushBox(CORINFO_RESOLVED_TOKEN* pResolvedToken);

    void impImportNewObjArray(CORINFO_RESOLVED_TOKEN* pResolvedToken, CORINFO_CALL_INFO* pCallInfo);

    bool impCanPInvokeInline();
    bool impCanPInvokeInlineCallSite(BasicBlock* block);
    void impCheckForPInvokeCall(
        GenTreeCall* call, CORINFO_METHOD_HANDLE methHnd, CORINFO_SIG_INFO* sig, unsigned mflags, BasicBlock* block);
    GenTreeCall* impImportIndirectCall(CORINFO_SIG_INFO* sig, IL_OFFSETX ilOffset = BAD_IL_OFFSET);
    void PopUnmanagedCallArgs(GenTreeCall* call, CORINFO_SIG_INFO* sig);

    void impInsertHelperCall(const CORINFO_HELPER_DESC& helperCall);
    void impHandleAccessAllowed(CorInfoIsAccessAllowedResult result, const CORINFO_HELPER_DESC& helperCall);

    void ImportArgList();
    void ImportMkRefAny(const BYTE* codeAddr);
    void ImportRefAnyType();
    void ImportRefAnyVal(const BYTE* codeAddr);
    void ImportLocAlloc(BasicBlock* block);
    void ImportIsInst(const BYTE* codeAddr);
    void ImportCastClass(CORINFO_RESOLVED_TOKEN& resolvedToken, bool isUnboxAny);
    void ImportUnbox(CORINFO_RESOLVED_TOKEN& resolvedToken, bool isUnboxAny);
    int ImportBox(const BYTE* codeAddr, const BYTE* codeEnd);
    void ImportLdToken(const BYTE* codeAddr);
    void ImportJmp(const BYTE* codeAddr, BasicBlock* block);
    void ImportLdFtn(const BYTE* codeAddr, CORINFO_RESOLVED_TOKEN& constrainedResolvedToken, int prefixFlags);
    void ImportLdVirtFtn(const BYTE* codeAddr);
    void ImportNewArr(const BYTE* codeAddr, BasicBlock* block);
    void ImportNewObj(const uint8_t* codeAddr, int prefixFlags, BasicBlock* block);
    void ImportCallI(const uint8_t* codeAddr, int prefixFlags);
    void ImportCall(const uint8_t*          codeAddr,
                    OPCODE                  opcode,
                    CORINFO_RESOLVED_TOKEN* constrainedResolvedToken,
                    int                     prefixFlags);
    void ImportCall(const uint8_t*          codeAddr,
                    OPCODE                  opcode,
                    CORINFO_RESOLVED_TOKEN& resolvedToken,
                    CORINFO_RESOLVED_TOKEN* constrainedResolvedToken,
                    CORINFO_CALL_INFO&      callInfo,
                    int                     prefixFlags);
    GenTreeCall* impImportCall(OPCODE                  opcode,
                               CORINFO_RESOLVED_TOKEN* resolvedToken,
                               CORINFO_RESOLVED_TOKEN* constrainedResolvedToken,
                               GenTree*                newObjThis,
                               int                     prefixFlags,
                               CORINFO_CALL_INFO*      callInfo,
                               const uint8_t*          ilAddr);
    GenTree* CreateCallICookie(GenTreeCall* call, CORINFO_SIG_INFO* sig);
    GenTree* CreateVarargsCallArgHandle(GenTreeCall* call, CORINFO_SIG_INFO* sig);
    GenTree* CreateGenericCallTypeArg(GenTreeCall*            call,
                                      CORINFO_CALL_INFO*      callInfo,
                                      CORINFO_RESOLVED_TOKEN* resolvedToken,
                                      CORINFO_RESOLVED_TOKEN* constrainedResolvedToken,
                                      bool                    isReadOnlyCall);
    void SetupTailCall(GenTreeCall*            call,
                       OPCODE                  opcode,
                       int                     prefixFlags,
                       CORINFO_SIG_INFO*       sig,
                       CORINFO_RESOLVED_TOKEN* resolvedToken,
                       CORINFO_METHOD_HANDLE   methodHandle,
                       const char*             tailCallFailReason);

    void addExpRuntimeLookupCandidate(GenTreeCall* call);

    void impInitializeStructCall(GenTreeCall* call, CORINFO_CLASS_HANDLE retClass);
#if FEATURE_MULTIREG_RET
    GenTree* impCanonicalizeMultiRegReturnValue(GenTree* value, CORINFO_CLASS_HANDLE retClass);
#endif
    GenTree* impSpillPseudoReturnBufferCall(GenTreeCall* call);
    GenTree* impInitClass(CORINFO_RESOLVED_TOKEN* pResolvedToken);
    GenTree* impImportStaticReadOnlyField(void* fldAddr, var_types lclTyp);
    GenTreeFieldAddr* impImportFieldAddr(GenTree*                      addr,
                                         const CORINFO_RESOLVED_TOKEN& resolvedToken,
                                         const CORINFO_FIELD_INFO&     fieldInfo);
    GenTree* impImportFieldInstanceAddrHelper(OPCODE                    opcode,
                                              GenTree*                  objPtr,
                                              CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                              const CORINFO_FIELD_INFO& fieldInfo,
                                              var_types                 type,
                                              CORINFO_CLASS_HANDLE      structType);
    GenTree* impImportStaticFieldAddressHelper(OPCODE                    opcode,
                                               CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                               const CORINFO_FIELD_INFO& fieldInfo);
    GenTree* impImportLdSFld(OPCODE                    opcode,
                             CORINFO_RESOLVED_TOKEN*   resolvedToken,
                             const CORINFO_FIELD_INFO& fieldInfo,
                             unsigned                  prefixFlags);

    GenTree* impImportStSFld(GenTree*                  value,
                             CORINFO_CLASS_HANDLE      valueStructType,
                             CORINFO_RESOLVED_TOKEN*   resolvedToken,
                             const CORINFO_FIELD_INFO& fieldInfo,
                             unsigned                  prefixFlags);

    GenTree* impImportStaticFieldAccess(OPCODE                    opcode,
                                        CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                        const CORINFO_FIELD_INFO& fieldInfo);

    GenTree* impConvertFieldStoreValue(var_types storeType, GenTree* value);

    static void impBashVarAddrsToI(GenTree* tree1, GenTree* tree2 = nullptr);

    GenTree* impImplicitIorI4Cast(GenTree* tree, var_types dstTyp);

    GenTree* impImplicitR4orR8Cast(GenTree* tree, var_types dstTyp);

    void impImportLeave(BasicBlock* block);
    void impResetLeaveBlock(BasicBlock* block, IL_OFFSET leaveOffset);
    GenTree* impUnsupportedNamedIntrinsic(CorInfoHelpFunc       helper,
                                          CORINFO_METHOD_HANDLE method,
                                          CORINFO_SIG_INFO*     sig,
                                          bool                  mustExpand);

    bool impInlineIsGuaranteedThisDerefBeforeAnySideEffects(GenTree*          additionalTree,
                                                            GenTreeCall::Use* additionalCallArgs,
                                                            GenTree*          dereferencedAddress);

    void impMarkInlineCandidate(GenTreeCall*           call,
                                CORINFO_CONTEXT_HANDLE exactContextHnd,
                                bool                   exactContextNeedsRuntimeLookup,
                                CORINFO_CALL_INFO*     callInfo);

    void impMarkInlineCandidateHelper(GenTreeCall*           call,
                                      CORINFO_CONTEXT_HANDLE exactContextHnd,
                                      bool                   exactContextNeedsRuntimeLookup,
                                      CORINFO_CALL_INFO*     callInfo);

    bool impCanSkipCovariantStoreCheck(GenTree* value, GenTree* array);

    void impImportInitObj(GenTree* dstAddr, ClassLayout* layout);
    void impImportCpObj(GenTree* dstAddr, GenTree* srcAddr, ClassLayout* layout);
    void impImportInitBlk(unsigned prefixFlags);
    void impImportCpBlk(unsigned prefixFlags);

    GenTree* impImportPop(BasicBlock* block);

    void impImportDup();

    GenTree* impImportTlsFieldAccess(OPCODE                    opcode,
                                     CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                     const CORINFO_FIELD_INFO& fieldInfo);

#ifdef DEBUG
    void fgDispBasicBlocks(BasicBlock* firstBlock, BasicBlock* lastBlock, bool dumpTrees);
    void fgDispBasicBlocks(bool dumpTrees = false);
    void fgDispHandlerTab();
    void gtDispStmt(Statement* stmt);
    void gtDispTree(GenTree* tree);
    static int dspTreeID(GenTree* tree);
    void JitLogEE(unsigned level, const char* fmt, ...);
#endif

    void eeGetCallInfo(CORINFO_RESOLVED_TOKEN* resolvedToken,
                       CORINFO_RESOLVED_TOKEN* constrainedToken,
                       CORINFO_CALLINFO_FLAGS  flags,
                       CORINFO_CALL_INFO*      result);
    void eeGetSig(unsigned               sigTok,
                  CORINFO_MODULE_HANDLE  scope,
                  CORINFO_CONTEXT_HANDLE context,
                  CORINFO_SIG_INFO*      retSig);
    void eeGetCallSiteSig(unsigned               sigTok,
                          CORINFO_MODULE_HANDLE  scope,
                          CORINFO_CONTEXT_HANDLE context,
                          CORINFO_SIG_INFO*      retSig);
    void eeGetMethodSig(CORINFO_METHOD_HANDLE methHnd, CORINFO_SIG_INFO* retSig, CORINFO_CLASS_HANDLE owner = nullptr);
    void eeGetFieldInfo(CORINFO_RESOLVED_TOKEN* resolvedToken, CORINFO_ACCESS_FLAGS flags, CORINFO_FIELD_INFO* result);
    const char* eeGetFieldName(CORINFO_FIELD_HANDLE field, const char** className = nullptr);
    const char* eeGetClassName(CORINFO_CLASS_HANDLE clsHnd);
    const char* eeGetMethodName(CORINFO_METHOD_HANDLE handle, const char** className);
    CORINFO_CLASS_HANDLE eeGetClassFromContext(CORINFO_CONTEXT_HANDLE context);
    static CORINFO_METHOD_HANDLE eeFindHelper(unsigned helper);
    static unsigned eeGetArrayDataOffset(var_types type);
    static unsigned eeGetMDArrayDataOffset(var_types type, unsigned rank);

    bool impIsClassExact(CORINFO_CLASS_HANDLE classHnd);

    GenTree* impTokenToHandle(CORINFO_RESOLVED_TOKEN* resolvedToken,
                              bool                    mustRestoreHandle = false,
                              bool                    importParent      = false,
                              bool*                   runtimeLookup     = nullptr);

    GenTree* impParentClassTokenToHandle(CORINFO_RESOLVED_TOKEN* resolvedToken,
                                         bool                    mustRestoreHandle = false,
                                         bool*                   runtimeLookup     = nullptr);

#ifdef FEATURE_READYTORUN_COMPILER
    GenTreeCall* gtNewReadyToRunHelperCallNode(CORINFO_RESOLVED_TOKEN* resolvedToken,
                                               CorInfoHelpFunc         helper,
                                               var_types               type,
                                               GenTreeCall::Use*       args              = nullptr,
                                               CORINFO_LOOKUP_KIND*    genericLookupKind = nullptr);
#endif

    GenTree* gtNewRuntimeContextTree(CORINFO_RUNTIME_LOOKUP_KIND kind);

    GenTreeCall* gtNewSharedStaticsCctorHelperCall(CORINFO_CLASS_HANDLE cls, CorInfoHelpFunc helper);
    GenTreeCall* gtNewSharedCctorHelperCall(CORINFO_CLASS_HANDLE cls);

    CORINFO_CLASS_HANDLE impGetRefAnyClass();
    CORINFO_CLASS_HANDLE impGetObjectClass();
    CORINFO_CLASS_HANDLE impGetTypeHandleClass();
    var_types            GetRuntimeHandleUnderlyingType();

    CORINFO_CLASS_HANDLE gtGetClassHandle(GenTree* tree, bool* isExact, bool* isNonNull);

    ClassLayout* typGetObjLayout(CORINFO_CLASS_HANDLE classHandle);
    unsigned typGetObjLayoutNum(CORINFO_CLASS_HANDLE classHandle);
    var_types typGetStructType(ClassLayout* layout);
    var_types typGetStructType(CORINFO_CLASS_HANDLE classHandle, var_types* elementType);
    ClassLayout* typGetBlkLayout(unsigned blockSize);
    unsigned typGetLayoutNum(ClassLayout* layout);
    ClassLayout* typGetLayoutByNum(unsigned layoutNum);

    StructPassing abiGetStructReturnType(ClassLayout* layout, CorInfoCallConvExtension callConv, bool isVarArgs);

    unsigned lvaGrabTemp(bool shortLifetime DEBUGARG(const char* reason));
    unsigned lvaGrabTemps(unsigned count DEBUGARG(const char* reason));
    unsigned lvaNewTemp(var_types type, bool shortLifetime DEBUGARG(const char* reason));
    unsigned lvaNewTemp(ClassLayout* layout, bool shortLifetime DEBUGARG(const char* reason));
    unsigned lvaNewTemp(CORINFO_CLASS_HANDLE classHandle, bool shortLifetime DEBUGARG(const char* reason));
    unsigned lvaNewTemp(GenTree* tree, bool shortLifetime DEBUGARG(const char* reason));
    void lvaSetAddressExposed(unsigned lclNum);
    void lvaSetStruct(unsigned lclNum, ClassLayout* layout, bool checkUnsafeBuffer);
    void lvaSetStruct(unsigned lclNum, CORINFO_CLASS_HANDLE classHandle, bool checkUnsafeBuffer);
    void lvaSetClass(unsigned lclNum, CORINFO_CLASS_HANDLE clsHnd, bool isExact = false);
    void lvaSetClass(unsigned lclNum, GenTree* tree, CORINFO_CLASS_HANDLE stackHandle = nullptr);
    void lvaUpdateClass(unsigned lclNum, CORINFO_CLASS_HANDLE clsHnd, bool isExact = false);
    void lvaUpdateClass(unsigned lclNum, GenTree* tree, CORINFO_CLASS_HANDLE stackHandle = nullptr);
    LclVarDsc* lvaGetDesc(unsigned lclNum);
    LclVarDsc* lvaGetDesc(GenTreeLclVarCommon* lclNode);
    LclVarDsc* lvaGetDesc(GenTreeLclAddr* lclAddr);
    bool lvaIsOriginalThisParam(unsigned lclNum);
    bool lvaHaveManyLocals();
    bool fgVarNeedsExplicitZeroInit(unsigned lclNum, bool blockIsInLoop, bool blockIsReturn);
    unsigned compMapILargNum(unsigned ilArgNum);

    Statement* gtNewStmt(GenTree* expr = nullptr, IL_OFFSETX offset = BAD_IL_OFFSET);

    GenTreeLclVar* gtNewLclvNode(unsigned lclNum, var_types type);
    GenTreeLclAddr* gtNewLclVarAddrNode(unsigned lclNum, var_types type = TYP_I_IMPL);
    GenTreeLclFld* gtNewLclFldNode(unsigned lclNum, var_types type, unsigned offset);
    GenTreeIntCon* gtNewIconNode(ssize_t value, var_types type = TYP_INT);
    GenTreeIntCon* gtNewIconNode(unsigned fieldOffset, FieldSeqNode* fieldSeq);
    GenTree* gtNewLconNode(int64_t value);
    GenTreeIntCon* gtNewIconHandleNode(void* value, HandleKind kind, FieldSeqNode* fieldSeq = nullptr);
    GenTreeIntCon* gtNewIconHandleNode(size_t value, HandleKind kind, FieldSeqNode* fieldSeq = nullptr);
    GenTree* gtNewConstLookupTree(void* value, void* pValue, HandleKind handleKind, void* compileTimeHandle);
    GenTree* gtNewIconEmbModHndNode(CORINFO_MODULE_HANDLE modHnd);
    GenTree* gtNewIconEmbClsHndNode(CORINFO_CLASS_HANDLE clsHnd);
    GenTree* gtNewIconEmbMethHndNode(CORINFO_METHOD_HANDLE methHnd);
    GenTree* gtNewIconEmbFldHndNode(CORINFO_FIELD_HANDLE fldHnd);
    GenTree* gtNewIndOfIconHandleNode(var_types type, size_t value, HandleKind handleKind, bool invariant);
    GenTree* gtNewZeroConNode(var_types type);
    GenTree* gtNewOneConNode(var_types type);
    GenTree* gtNewDconNode(double value, var_types type = TYP_DOUBLE);
    GenTreeStrCon* gtNewSconNode(CORINFO_MODULE_HANDLE module, mdToken token);
    GenTree* gtNewNothingNode();
    GenTree* gtUnusedValNode(GenTree* expr);
    GenTreeRetExpr* gtNewRetExpr(GenTreeCall* call);
    GenTreeUnOp* gtNewOperNode(genTreeOps oper, var_types type, GenTree* op1);
    GenTree* gtNewNullCheck(GenTree* addr);
    GenTreeCast* gtNewCastNode(GenTree* op1, bool fromUnsigned, var_types castType);
    GenTreeIndir* gtNewIndir(var_types type, GenTree* addr);
    GenTreeFieldAddr* gtNewFieldAddr(GenTree* addr, CORINFO_FIELD_HANDLE handle, unsigned offset);
    GenTreeFieldAddr* gtNewFieldAddr(GenTree* addr, FieldSeqNode* fieldSeq, unsigned offset);
    GenTreeIndir* gtNewFieldIndir(var_types type, GenTreeFieldAddr* fieldAddr);
    GenTreeIndir* gtNewFieldIndir(var_types type, unsigned layoutNum, GenTreeFieldAddr* fieldAddr);
    GenTreeObj* gtNewObjNode(ClassLayout* layout, GenTree* addr);
    GenTreeObj* gtNewObjNode(var_types type, ClassLayout* layout, GenTree* addr);
    GenTreeIntCon* gtNewStringLiteralLength(GenTreeStrCon* node);
    GenTree* gtNewStringLiteralNode(InfoAccessType iat, void* value);
    GenTreeAllocObj* gtNewAllocObjNode(
        unsigned helper, bool helperHasSideEffects, CORINFO_CLASS_HANDLE clsHnd, var_types type, GenTree* op1);
    GenTreeAllocObj* gtNewAllocObjNode(CORINFO_RESOLVED_TOKEN* resolvedToken, bool useParent);
    GenTreeIndir* gtNewMethodTableLookup(GenTree* obj);
    GenTreeOp* gtNewOperNode(genTreeOps oper, var_types type, GenTree* op1, GenTree* op2);
    GenTreeOp* gtNewCommaNode(GenTree* op1, GenTree* op2, var_types type = TYP_UNDEF);
    GenTreeQmark* gtNewQmarkNode(var_types type, GenTree* cond, GenTree* op1, GenTree* op2);
    GenTreeOp* gtNewAssignNode(GenTree* dst, GenTree* src);
    GenTreeBoundsChk* gtNewBoundsChk(GenTree* index, GenTree* length, ThrowHelperKind kind);
    GenTreeIndexAddr* gtNewArrayIndexAddr(GenTree* arr, GenTree* ind, var_types elemType);
    GenTreeIndexAddr* gtNewStringIndexAddr(GenTree* arr, GenTree* ind);
    GenTreeIndir* gtNewIndexIndir(var_types type, GenTreeIndexAddr* indexAddr);
    GenTreeCall::Use* gtNewCallArgs(GenTree* node);
    GenTreeCall::Use* gtNewCallArgs(GenTree* node1, GenTree* node2);
    GenTreeCall::Use* gtNewCallArgs(GenTree* node1, GenTree* node2, GenTree* node3);
    GenTreeCall::Use* gtNewCallArgs(GenTree* node1, GenTree* node2, GenTree* node3, GenTree* node4);
    GenTreeCall::Use* gtPrependNewCallArg(GenTree* node, GenTreeCall::Use* args);
    GenTreeCall::Use* gtInsertNewCallArgAfter(GenTree* node, GenTreeCall::Use* after);
    GenTreeCall* gtNewHelperCallNode(CorInfoHelpFunc helper, var_types type, GenTreeCall::Use* args = nullptr);
    GenTreeCall* gtNewRuntimeLookupHelperCallNode(CORINFO_RUNTIME_LOOKUP* runtimeLookup,
                                                  GenTree*                context,
                                                  void*                   compileTimeHandle);
    GenTreeCall* gtNewUserCallNode(CORINFO_METHOD_HANDLE handle,
                                   var_types             type,
                                   GenTreeCall::Use*     args,
                                   IL_OFFSETX            ilOffset = BAD_IL_OFFSET);
    GenTreeCall* gtNewIndCallNode(GenTree*          addr,
                                  var_types         type,
                                  GenTreeCall::Use* args,
                                  IL_OFFSETX        ilOffset = BAD_IL_OFFSET);

#ifdef FEATURE_HW_INTRINSICS
    GenTreeHWIntrinsic* gtNewSimdHWIntrinsicNode(var_types      type,
                                                 NamedIntrinsic hwIntrinsicID,
                                                 var_types      baseType,
                                                 unsigned       size);
    GenTreeHWIntrinsic* gtNewSimdHWIntrinsicNode(
        var_types type, NamedIntrinsic hwIntrinsicID, var_types baseType, unsigned size, GenTree* op1);
    GenTreeHWIntrinsic* gtNewSimdHWIntrinsicNode(
        var_types type, NamedIntrinsic hwIntrinsicID, var_types baseType, unsigned size, GenTree* op1, GenTree* op2);
    GenTreeHWIntrinsic* gtNewSimdHWIntrinsicNode(var_types      type,
                                                 NamedIntrinsic hwIntrinsicID,
                                                 var_types      baseType,
                                                 unsigned       size,
                                                 GenTree*       op1,
                                                 GenTree*       op2,
                                                 GenTree*       op3);
    GenTreeHWIntrinsic* gtNewSimdHWIntrinsicNode(var_types      type,
                                                 NamedIntrinsic hwIntrinsicID,
                                                 var_types      baseType,
                                                 unsigned       size,
                                                 GenTree*       op1,
                                                 GenTree*       op2,
                                                 GenTree*       op3,
                                                 GenTree*       op4);
    GenTreeHWIntrinsic* gtNewSimdHWIntrinsicNode(var_types      type,
                                                 NamedIntrinsic hwIntrinsicID,
                                                 var_types      baseType,
                                                 unsigned       size,
                                                 GenTree*       op1,
                                                 GenTree*       op2,
                                                 GenTree*       op3,
                                                 GenTree*       op4,
                                                 GenTree*       op5);
    GenTreeHWIntrinsic* gtNewSimdHWIntrinsicNode(var_types      type,
                                                 NamedIntrinsic hwIntrinsicID,
                                                 var_types      baseType,
                                                 unsigned       size,
                                                 unsigned       numOps,
                                                 GenTree**      ops);
    GenTreeHWIntrinsic* gtNewZeroSimdHWIntrinsicNode(ClassLayout* layout);
    GenTreeHWIntrinsic* gtNewZeroSimdHWIntrinsicNode(var_types type, var_types baseType);
    GenTreeHWIntrinsic* gtNewSimdGetElementNode(var_types simdType,
                                                var_types elementType,
                                                GenTree*  value,
                                                GenTree*  index);
    GenTreeHWIntrinsic* gtNewSimdWithElementNode(
        var_types type, var_types eltType, GenTree* vec, GenTreeIntCon* idx, GenTree* elt);
    GenTreeHWIntrinsic* gtNewScalarHWIntrinsicNode(var_types type, NamedIntrinsic hwIntrinsicID, GenTree* op1);
    GenTreeHWIntrinsic* gtNewScalarHWIntrinsicNode(var_types      type,
                                                   NamedIntrinsic hwIntrinsicID,
                                                   GenTree*       op1,
                                                   GenTree*       op2);
    GenTreeHWIntrinsic* gtNewScalarHWIntrinsicNode(
        var_types type, NamedIntrinsic hwIntrinsicID, GenTree* op1, GenTree* op2, GenTree* op3);
    var_types impGetHWIntrinsicBaseTypeFromArg(NamedIntrinsic    intrinsic,
                                               CORINFO_SIG_INFO* sig,
                                               var_types         baseType,
                                               ClassLayout**     argLayout);

    void lvaRecordSimdIntrinsicUse(GenTree* op);
    void lvaRecordSimdIntrinsicUse(GenTreeLclVar* lclVar);
    void lvaRecordSimdIntrinsicUse(unsigned lclNum);
    void lvaRecordSimdIntrinsicDef(GenTreeLclVar* lclVar, GenTreeHWIntrinsic* src);
    void lvaRecordSimdIntrinsicDef(unsigned lclNum, GenTreeHWIntrinsic* src);
#endif // FEATURE_HW_INTRINSICS

    static GenTreeLclAddr* impIsAddressInLocal(GenTree* tree);
    static GenTreeLclAddr* impIsLocalAddrExpr(GenTree* node);
    bool impHasLclRef(GenTree* tree, unsigned lclNum);
    bool impHasAddressTakenLocals(GenTree* tree);

    void impDevirtualizeCall(GenTreeCall*            call,
                             CORINFO_RESOLVED_TOKEN* resolvedToken,
                             CORINFO_METHOD_HANDLE*  method,
                             unsigned*               methodFlags,
                             CORINFO_CONTEXT_HANDLE* contextHandle,
                             CORINFO_CONTEXT_HANDLE* exactContextHandle,
                             bool                    isExplicitTailCall,
                             IL_OFFSETX              ilOffset = BAD_IL_OFFSET);

    GenTree* gtClone(GenTree* tree, bool complexOK = false);
    GenTree* gtCloneExpr(GenTree* tree);
    bool gtCanSwapOrder(GenTree* op1, GenTree* op2);
    void gtInitStructCopyAsg(GenTreeOp* asg);
    GenTree* gtFoldExpr(GenTree* tree);
    GenTree* gtFoldExprConst(GenTree* tree);
    GenTreeLclVar* fgInsertCommaFormTemp(GenTree** use);
    void gtChangeOperToNullCheck(GenTree* tree);
    bool gtIsRecursiveCall(GenTreeCall* call);
    bool gtIsRecursiveCall(CORINFO_METHOD_HANDLE callMethodHandle);
    GenTree* gtFoldTypeCompare(GenTree* tree);
    GenTree* gtFoldTypeEqualityCall(bool isEq, GenTree* op1, GenTree* op2);
    GenTree* gtOptimizeEnumHasFlag(GenTree* thisOp, GenTree* flagOp);
    CORINFO_CLASS_HANDLE gtGetHelperArgClassHandle(GenTree* array);
    GenTree* gtReverseCond(GenTree* tree);
    GenTreeCall* fgOptimizeDelegateConstructor(GenTreeCall*            call,
                                               CORINFO_CONTEXT_HANDLE* exactContextHnd,
                                               CORINFO_RESOLVED_TOKEN* ldftnToken);
    bool fgAddrCouldBeNull(GenTree* addr);

    void impCheckCanInline(GenTreeCall*           call,
                           CORINFO_METHOD_HANDLE  methodHandle,
                           unsigned               methodAttrs,
                           CORINFO_CONTEXT_HANDLE exactContextHnd,
                           InlineCandidateInfo**  inlineCandidateInfo,
                           InlineResult*          inlineResult);
    unsigned inlGetInlineeLocal(InlineInfo* inlineInfo, unsigned ilLocNum);
    GenTree* inlUseArg(InlineInfo* inlineInfo, unsigned ilArgNum);
    bool inlImportReturn(InlineInfo* inlineInfo, GenTree* op, CORINFO_CLASS_HANDLE retClsHnd);
};

struct ThrowHelperBlock
{
    ThrowHelperBlock*     next;
    BasicBlock* const     block;
    unsigned const        throwIndex;
    ThrowHelperKind const kind;

#if !FEATURE_FIXED_OUT_ARGS
    bool     stackLevelSet = false;
    unsigned stackLevel    = 0;
#endif

    ThrowHelperBlock(ThrowHelperBlock* next, ThrowHelperKind kind, unsigned throwIndex, BasicBlock* block)
        : next(next), block(block), throwIndex(throwIndex), kind(kind)
    {
    }
};

class Compiler
{
    friend class emitter;
    friend class UnwindInfo;
    friend class UnwindFragmentInfo;
    friend class UnwindEpilogInfo;
    friend class JitTimer;
    friend class LinearScan;
    friend class CallInfo;
    friend class Rationalizer;
    friend class PhaseBase;
    friend class Lowering;
    friend class SsaBuilder;
    friend class SsaOptimizer;
    friend class EarlyProp;
    friend class Cse;
    friend class CodeGenInterface;
    friend class CodeGen;
    friend class LclVarDsc;
    friend class LIR;
    friend class ObjectAllocator;
    friend class LocalAddressVisitor;
    friend struct GenTree;
    friend class ClassLayout;
    friend class VNConstPropVisitor;
    friend class StructPromotionHelper;
    friend class AssertionProp;
    friend struct Importer;
    friend class SIMDCoalescingBuffer;
    friend class ValueNumbering;
    friend class LoopHoist;

#ifdef FEATURE_HW_INTRINSICS
    friend struct HWIntrinsicInfo;
#endif // FEATURE_HW_INTRINSICS

#ifndef TARGET_64BIT
    friend class DecomposeLongs;
#endif // !TARGET_64BIT

    /*
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XX                                                                           XX
    XX  Misc structs definitions                                                 XX
    XX                                                                           XX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    */

public:
    ArenaAllocator*  compArenaAllocator;
    hashBvGlobalData hbvGlobalData; // Used by the hashBv bitvector package.

#ifdef DEBUG
    bool verbose      = false;
    bool verboseTrees = false;
    bool verboseSsa   = false; // If true, produce especially verbose dump output in SSA construction.
    int  morphNum     = 0;     // This counts the the trees that have been morphed, allowing us to label each uniquely.

    void makeExtraStructQueries(CORINFO_CLASS_HANDLE structHandle, int level); // Make queries recursively 'level' deep.

    unsigned expensiveDebugCheckLevel;
    unsigned compInlinedCodeSize = 0;
#endif

    //-------------------------------------------------------------------------
    // The following is used for validating format of EH table
    //

    struct EHNodeDsc;
    typedef struct EHNodeDsc* pEHNodeDsc;

    struct EHTree
    {
        EHNodeDsc* ehnTree = nullptr; // root of the tree comprising the EHnodes.
        EHNodeDsc* ehnNext;           // root of the tree comprising the EHnodes.
        bool       fgNeedToSortEHTable = false;

        EHTree(Compiler* compiler, unsigned numEHClauses);
    };

    struct EHNodeDsc
    {
        enum EHBlockType
        {
            TryNode,
            FilterNode,
            HandlerNode,
            FinallyNode,
            FaultNode
        };

        EHBlockType ehnBlockType;   // kind of EH block
        IL_OFFSET   ehnStartOffset; // IL offset of start of the EH block
        IL_OFFSET ehnEndOffset; // IL offset past end of the EH block. (TODO: looks like verInsertEhNode() sets this to
                                // the last IL offset, not "one past the last one", i.e., the range Start to End is
                                // inclusive).
        pEHNodeDsc ehnNext;     // next (non-nested) block in sequential order
        pEHNodeDsc ehnChild;    // leftmost nested block
        union {
            pEHNodeDsc ehnTryNode;     // for filters and handlers, the corresponding try node
            pEHNodeDsc ehnHandlerNode; // for a try node, the corresponding handler node
        };
        pEHNodeDsc ehnFilterNode; // if this is a try node and has a filter, otherwise 0
        pEHNodeDsc ehnEquivalent; // if blockType=tryNode, start offset and end offset is same,

        void ehnSetTryNodeType()
        {
            ehnBlockType = TryNode;
        }
        void ehnSetFilterNodeType()
        {
            ehnBlockType = FilterNode;
        }
        void ehnSetHandlerNodeType()
        {
            ehnBlockType = HandlerNode;
        }
        void ehnSetFinallyNodeType()
        {
            ehnBlockType = FinallyNode;
        }
        void ehnSetFaultNodeType()
        {
            ehnBlockType = FaultNode;
        }

        bool ehnIsTryBlock()
        {
            return ehnBlockType == TryNode;
        }
        bool ehnIsFilterBlock()
        {
            return ehnBlockType == FilterNode;
        }
        bool ehnIsHandlerBlock()
        {
            return ehnBlockType == HandlerNode;
        }
        bool ehnIsFinallyBlock()
        {
            return ehnBlockType == FinallyNode;
        }
        bool ehnIsFaultBlock()
        {
            return ehnBlockType == FaultNode;
        }

        // returns true if there is any overlap between the two nodes
        static bool ehnIsOverlap(pEHNodeDsc node1, pEHNodeDsc node2)
        {
            if (node1->ehnStartOffset < node2->ehnStartOffset)
            {
                return (node1->ehnEndOffset >= node2->ehnStartOffset);
            }
            else
            {
                return (node1->ehnStartOffset <= node2->ehnEndOffset);
            }
        }

        // fails with BADCODE if inner is not completely nested inside outer
        static bool ehnIsNested(pEHNodeDsc inner, pEHNodeDsc outer)
        {
            return ((inner->ehnStartOffset >= outer->ehnStartOffset) && (inner->ehnEndOffset <= outer->ehnEndOffset));
        }
    };

//-------------------------------------------------------------------------
// Exception handling functions
//

#if !defined(FEATURE_EH_FUNCLETS)

    bool ehNeedsShadowSPslots()
    {
        return (info.compXcptnsCount || opts.compDbgEnC);
    }

    // 0 for methods with no EH
    // 1 for methods with non-nested EH, or where only the try blocks are nested
    // 2 for a method with a catch within a catch
    // etc.
    unsigned ehMaxHndNestingCount = 0;

#endif // !FEATURE_EH_FUNCLETS

    static bool jitIsBetween(unsigned value, unsigned start, unsigned end);
    static bool jitIsBetweenInclusive(unsigned value, unsigned start, unsigned end);

    bool bbInCatchHandlerILRange(BasicBlock* blk);
    bool bbInFilterILRange(BasicBlock* blk);
    bool bbInTryRegions(unsigned regionIndex, BasicBlock* blk);
    bool bbInExnFlowRegions(unsigned regionIndex, BasicBlock* blk);
    bool bbInHandlerRegions(unsigned regionIndex, BasicBlock* blk);
    bool bbInCatchHandlerRegions(BasicBlock* tryBlk, BasicBlock* hndBlk);
    unsigned short bbFindInnermostCommonTryRegion(BasicBlock* bbOne, BasicBlock* bbTwo);

    unsigned short bbFindInnermostTryRegionContainingHandlerRegion(unsigned handlerIndex);
    unsigned short bbFindInnermostHandlerRegionContainingTryRegion(unsigned tryIndex);

    // Returns true if "block" is the start of a try region.
    bool bbIsTryBeg(BasicBlock* block);

    // Returns true if "block" is the start of a handler or filter region.
    bool bbIsHandlerBeg(BasicBlock* block);

    bool ehHasCallableHandlers();

    // Return the EH descriptor for the given region index.
    EHblkDsc* ehGetDsc(unsigned regionIndex);

    // Return the EH index given a region descriptor.
    unsigned ehGetIndex(EHblkDsc* ehDsc);

    // Return the EH descriptor index of the enclosing try, for the given region index.
    unsigned ehGetEnclosingTryIndex(unsigned regionIndex);

    // Return the EH descriptor index of the enclosing handler, for the given region index.
    unsigned ehGetEnclosingHndIndex(unsigned regionIndex);

    // Return the EH descriptor for the most nested 'try' region this BasicBlock is a member of (or nullptr if this
    // block is not in a 'try' region).
    EHblkDsc* ehGetBlockTryDsc(BasicBlock* block);

    // Return the EH descriptor for the most nested filter or handler region this BasicBlock is a member of (or nullptr
    // if this block is not in a filter or handler region).
    EHblkDsc* ehGetBlockHndDsc(BasicBlock* block);

    // Return the EH descriptor for the most nested region that may handle exceptions raised in this BasicBlock (or
    // nullptr if this block's exceptions propagate to caller).
    EHblkDsc* ehGetBlockExnFlowDsc(BasicBlock* block);

    EHblkDsc* ehIsBlockTryLast(BasicBlock* block);
    EHblkDsc* ehIsBlockHndLast(BasicBlock* block);
    bool ehIsBlockEHLast(BasicBlock* block);

    bool ehBlockHasExnFlowDsc(BasicBlock* block);

    // Return the region index of the most nested EH region this block is in.
    unsigned ehGetMostNestedRegionIndex(BasicBlock* block, bool* inTryRegion);

    // Find the true enclosing try index, ignoring 'mutual protect' try. Uses IL ranges to check.
    unsigned ehTrueEnclosingTryIndexIL(unsigned regionIndex);

    // Return the index of the most nested enclosing region for a particular EH region. Returns NO_ENCLOSING_INDEX
    // if there is no enclosing region. If the returned index is not NO_ENCLOSING_INDEX, then '*inTryRegion'
    // is set to 'true' if the enclosing region is a 'try', or 'false' if the enclosing region is a handler.
    // (It can never be a filter.)
    unsigned ehGetEnclosingRegionIndex(unsigned regionIndex, bool* inTryRegion);

    // A block has been deleted. Update the EH table appropriately.
    void ehUpdateForDeletedBlock(BasicBlock* block);

    // Determine whether a block can be deleted while preserving the EH normalization rules.
    bool ehCanDeleteEmptyBlock(BasicBlock* block);

    // Update the 'last' pointers in the EH table to reflect new or deleted blocks in an EH region.
    void ehUpdateLastBlocks(BasicBlock* oldLast, BasicBlock* newLast);

    // For a finally handler, find the region index that the BBJ_CALLFINALLY lives in that calls the handler,
    // or NO_ENCLOSING_INDEX if the BBJ_CALLFINALLY lives in the main function body. Normally, the index
    // is the same index as the handler (and the BBJ_CALLFINALLY lives in the 'try' region), but for AMD64 the
    // BBJ_CALLFINALLY lives in the enclosing try or handler region, whichever is more nested, or the main function
    // body. If the returned index is not NO_ENCLOSING_INDEX, then '*inTryRegion' is set to 'true' if the
    // BBJ_CALLFINALLY lives in the returned index's 'try' region, or 'false' if lives in the handler region. (It never
    // lives in a filter.)
    unsigned ehGetCallFinallyRegionIndex(unsigned finallyIndex, bool* inTryRegion);

    // Find the range of basic blocks in which all BBJ_CALLFINALLY will be found that target the 'finallyIndex' region's
    // handler. Set begBlk to the first block, and endBlk to the block after the last block of the range
    // (nullptr if the last block is the last block in the program).
    // Precondition: 'finallyIndex' is the EH region of a try/finally clause.
    void ehGetCallFinallyBlockRange(unsigned finallyIndex, BasicBlock** begBlk, BasicBlock** endBlk);

#ifdef DEBUG
    // Given a BBJ_CALLFINALLY block and the EH region index of the finally it is calling, return
    // 'true' if the BBJ_CALLFINALLY is in the correct EH region.
    bool ehCallFinallyInCorrectRegion(BasicBlock* blockCallFinally, unsigned finallyIndex);
#endif // DEBUG

#if defined(FEATURE_EH_FUNCLETS)
    // Do we need a PSPSym in the main function? For codegen purposes, we only need one
    // if there is a filter that protects a region with a nested EH clause (such as a
    // try/catch nested in the 'try' body of a try/filter/filter-handler). See
    // genFuncletProlog() for more details. However, the VM seems to use it for more
    // purposes, maybe including debugging. Until we are sure otherwise, always create
    // a PSPSym for functions with any EH.
    bool ehNeedsPSPSym() const
    {
#ifdef TARGET_X86
        return false;
#else  // TARGET_X86
        return compHndBBtabCount > 0;
#endif // TARGET_X86
    }

    bool     ehAnyFunclets();  // Are there any funclets in this function?
    unsigned ehFuncletCount(); // Return the count of funclets in the function

    unsigned bbThrowIndex(BasicBlock* blk); // Get the index to use as the cache key for sharing throw blocks

#else  // !FEATURE_EH_FUNCLETS

    bool ehAnyFunclets()
    {
        return false;
    }
    unsigned ehFuncletCount()
    {
        return 0;
    }

    unsigned bbThrowIndex(BasicBlock* blk)
    {
        return blk->bbTryIndex;
    } // Get the index to use as the cache key for sharing throw blocks
#endif // !FEATURE_EH_FUNCLETS

    // Returns a flowList representing the "EH predecessors" of "blk".  These are the normal predecessors of
    // "blk", plus one special case: if "blk" is the first block of a handler, considers the predecessor(s) of the first
    // first block of the corresponding try region to be "EH predecessors".  (If there is a single such predecessor,
    // for example, we want to consider that the immediate dominator of the catch clause start block, so it's
    // convenient to also consider it a predecessor.)
    flowList* BlockPredsWithEH(BasicBlock* blk);

    insGroup* ehEmitLabel(BasicBlock* block);
    UNATIVE_OFFSET ehCodeOffset(BasicBlock* block);

    EHblkDsc* ehInitHndRange(BasicBlock* src, IL_OFFSET* hndBeg, IL_OFFSET* hndEnd, bool* inFilter);

    EHblkDsc* ehInitTryRange(BasicBlock* src, IL_OFFSET* tryBeg, IL_OFFSET* tryEnd);

    EHblkDsc* ehInitHndBlockRange(BasicBlock* blk, BasicBlock** hndBeg, BasicBlock** hndLast, bool* inFilter);

    EHblkDsc* ehInitTryBlockRange(BasicBlock* blk, BasicBlock** tryBeg, BasicBlock** tryLast);

    void fgSetTryBeg(EHblkDsc* handlerTab, BasicBlock* newTryBeg);

    void fgSetTryEnd(EHblkDsc* handlerTab, BasicBlock* newTryLast);

    void fgSetHndEnd(EHblkDsc* handlerTab, BasicBlock* newHndLast);

    void fgSkipRmvdBlocks(EHblkDsc* handlerTab);

    void fgAllocEHTable();

    void fgRemoveEHTableEntry(unsigned XTnum);

#if defined(FEATURE_EH_FUNCLETS)

    EHblkDsc* fgAddEHTableEntry(unsigned XTnum);

#endif // FEATURE_EH_FUNCLETS

#if !FEATURE_EH
    void fgRemoveEH();
#endif // !FEATURE_EH

    bool fgHasEH() const
    {
        // Assert that the EH table has been initialized by now. Note that
        // compHndBBtabAllocCount never decreases; it is a high-water mark
        // of table allocation. In contrast, compHndBBtabCount does shrink
        // if we delete a dead EH region, and if it shrinks to zero, the
        // table pointer compHndBBtab is unreliable.
        assert(compHndBBtabAllocCount >= info.compXcptnsCount);

#ifdef TARGET_X86
        // This case should use the !X86 path. This would require a few more
        // changes for X86 to use compHndBBtabCount (the current number of EH
        // clauses) instead of info.compXcptnsCount (the number of EH clauses
        // in IL), such as in ehNeedsShadowSPslots().
        // This is because sometimes the IL has an EH clause that we delete
        // as statically dead code before we get here, leaving no EH clauses,
        // and thus no requirement to use a frame pointer because of EH.
        return info.compXcptnsCount != 0;
#else
        return compHndBBtabCount != 0;
#endif
    }

    void fgSortEHTable();

    // Causes the EH table to obey some well-formedness conditions, by inserting
    // empty BB's when necessary:
    //   * No block is both the first block of a handler and the first block of a try.
    //   * No block is the first block of multiple 'try' regions.
    //   * No block is the last block of multiple EH regions.
    void fgNormalizeEH();
    bool fgNormalizeEHCase1();
    bool fgNormalizeEHCase2();
    bool fgNormalizeEHCase3();

#ifdef DEBUG
    void dispIncomingEHClause(unsigned num, const CORINFO_EH_CLAUSE& clause);
    void dispOutgoingEHClause(unsigned num, const CORINFO_EH_CLAUSE& clause);
    void fgVerifyHandlerTab();
    void fgDispHandlerTab();
#endif // DEBUG

    void verInitEHTree(unsigned numEHClauses);
    void verInsertEhNode(EHTree& tree, CORINFO_EH_CLAUSE* clause, EHblkDsc* handlerTab);
    void verInsertEhNodeInTree(EHTree& tree, EHNodeDsc* node);
    void verInsertEhNodeParent(EHNodeDsc** ppRoot, EHNodeDsc* node);
    void verCheckNestingLevel(EHNodeDsc* initRoot);

    /*
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XX                                                                           XX
    XX                        GenTree and BasicBlock                             XX
    XX                                                                           XX
    XX  Functions to allocate and display the GenTrees and BasicBlocks           XX
    XX                                                                           XX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    */

    // Functions to create nodes
    Statement* gtNewStmt(GenTree* expr = nullptr, IL_OFFSETX offset = BAD_IL_OFFSET);

    GenTreeUnOp* gtNewOperNode(genTreeOps oper, var_types type, GenTree* op1);
    GenTreeOp* gtNewOperNode(genTreeOps oper, var_types type, GenTree* op1, GenTree* op2);
    GenTreeOp* gtNewCommaNode(GenTree* op1, GenTree* op2, var_types type = TYP_UNDEF);
    GenTreeQmark* gtNewQmarkNode(var_types type, GenTree* cond, GenTree* op1, GenTree* op2);

    GenTree* gtNewLargeOperNode(genTreeOps oper,
                                var_types  type = TYP_I_IMPL,
                                GenTree*   op1  = nullptr,
                                GenTree*   op2  = nullptr);

    GenTreeIntCon* gtNewIconNode(ssize_t value, var_types type = TYP_INT);
    GenTreeIntCon* gtNewIconNode(unsigned fieldOffset, FieldSeqNode* fieldSeq);
    GenTreeIntCon* gtNewIntConFieldOffset(target_size_t fieldOffset, FieldSeqNode* fieldSeq);

    GenTreePhysReg* gtNewPhysRegNode(regNumber reg, var_types type);

    GenTree* gtNewJmpTableNode();

    GenTreeIndir* gtNewIndOfIconHandleNode(var_types type, size_t addr, HandleKind handleKind, bool invariant);
    GenTreeIntCon* gtNewIconHandleNode(void* value, HandleKind kind, FieldSeqNode* fieldSeq = nullptr);
    GenTreeIntCon* gtNewIconHandleNode(size_t value, HandleKind kind, FieldSeqNode* fieldSeq = nullptr);
    GenTree* gtNewConstLookupTree(void*      value,
                                  void*      valueAddr,
                                  HandleKind handleKind,
                                  void* compileTimeHandle DEBUGARG(void* dumpHandle = nullptr));
    GenTree* gtNewConstLookupTree(const CORINFO_CONST_LOOKUP& lookup,
                                  HandleKind                  handleKind,
                                  void* compileTimeHandle DEBUGARG(void* dumpHandle = nullptr));
    GenTree* gtNewIconEmbModHndNode(CORINFO_MODULE_HANDLE modHnd);
    GenTree* gtNewIconEmbClsHndNode(CORINFO_CLASS_HANDLE clsHnd);
    GenTree* gtNewIconEmbMethHndNode(CORINFO_METHOD_HANDLE methHnd);
    GenTree* gtNewIconEmbFldHndNode(CORINFO_FIELD_HANDLE fldHnd);
    GenTree* gtNewStringLiteralNode(InfoAccessType iat, void* pValue);
    GenTreeIntCon* gtNewStringLiteralLength(GenTreeStrCon* node);

    GenTree* gtNewLconNode(int64_t value);

    GenTree* gtNewDconNode(double value, var_types type = TYP_DOUBLE);

    GenTreeStrCon* gtNewSconNode(CORINFO_MODULE_HANDLE module, mdToken token);

    GenTree* gtNewZeroConNode(var_types type);

    GenTree* gtNewOneConNode(var_types type);

    GenTreeLclVar* gtNewStoreLclVar(unsigned lclNum, var_types type, GenTree* value);
    GenTreeLclFld* gtNewStoreLclFld(var_types type, unsigned lclNum, unsigned lclOffs, GenTree* value);

    GenTreeUnOp* gtNewBitCastNode(var_types type, GenTree* arg);

    void gtInitStructCopyAsg(GenTreeOp* asg);

    GenTreeObj* gtNewObjNode(ClassLayout* layout, GenTree* addr);
    GenTreeObj* gtNewObjNode(var_types type, ClassLayout* layout, GenTree* addr);

    GenTreeCall::Use* gtNewCallArgs(GenTree* node);
    GenTreeCall::Use* gtNewCallArgs(GenTree* node1, GenTree* node2);
    GenTreeCall::Use* gtNewCallArgs(GenTree* node1, GenTree* node2, GenTree* node3);
    GenTreeCall::Use* gtNewCallArgs(GenTree* node1, GenTree* node2, GenTree* node3, GenTree* node4);
    GenTreeCall::Use* gtPrependNewCallArg(GenTree* node, GenTreeCall::Use* args);
    GenTreeCall::Use* gtInsertNewCallArgAfter(GenTree* node, GenTreeCall::Use* after);

    GenTreeCall* gtNewCallNode(
        CallKind kind, void* target, var_types type, GenTreeCall::Use* args, IL_OFFSETX ilOffset = BAD_IL_OFFSET);

    GenTreeCall* gtNewUserCallNode(CORINFO_METHOD_HANDLE handle,
                                   var_types             type,
                                   GenTreeCall::Use*     args,
                                   IL_OFFSETX            ilOffset = BAD_IL_OFFSET);

    GenTreeCall* gtNewIndCallNode(GenTree*          addr,
                                  var_types         type,
                                  GenTreeCall::Use* args,
                                  IL_OFFSETX        ilOffset = BAD_IL_OFFSET);

    GenTreeCall* gtNewHelperCallNode(CorInfoHelpFunc helper, var_types type, GenTreeCall::Use* args = nullptr);

    GenTreeCall* gtNewRuntimeLookupHelperCallNode(CORINFO_RUNTIME_LOOKUP* pRuntimeLookup,
                                                  GenTree*                ctxTree,
                                                  void*                   compileTimeHandle);

    GenTreeLclVar* gtNewLclvNode(unsigned lnum, var_types type);
    GenTreeLclVar* gtNewLclVarLargeNode(unsigned lnum, var_types type);

    GenTreeLclAddr* gtNewLclVarAddrNode(unsigned lclNum, var_types type = TYP_I_IMPL);
    GenTreeLclAddr* gtNewLclFldAddrNode(unsigned      lclNum,
                                        unsigned      lclOffs,
                                        FieldSeqNode* fieldSeq,
                                        var_types     type = TYP_I_IMPL);

#ifdef FEATURE_HW_INTRINSICS
    GenTreeHWIntrinsic* gtNewZeroSimdHWIntrinsicNode(ClassLayout* layout);
    GenTreeHWIntrinsic* gtNewZeroSimdHWIntrinsicNode(var_types type, var_types baseType);

    GenTreeHWIntrinsic* NewExtractVectorElement(var_types vecType, var_types eltType, GenTree* vec, unsigned index);
    GenTreeHWIntrinsic* gtNewSimdGetElementNode(var_types simdType,
                                                var_types elementType,
                                                GenTree*  value,
                                                GenTree*  index);

    GenTreeHWIntrinsic* gtNewSimdWithElementNode(
        var_types type, var_types eltType, GenTree* vec, GenTreeIntCon* idx, GenTree* elt);

    GenTreeHWIntrinsic* gtNewSimdHWIntrinsicNode(var_types      type,
                                                 NamedIntrinsic hwIntrinsicID,
                                                 var_types      baseType,
                                                 unsigned       size);
    GenTreeHWIntrinsic* gtNewSimdHWIntrinsicNode(
        var_types type, NamedIntrinsic hwIntrinsicID, var_types baseType, unsigned size, GenTree* op1);
    GenTreeHWIntrinsic* gtNewSimdHWIntrinsicNode(
        var_types type, NamedIntrinsic hwIntrinsicID, var_types baseType, unsigned size, GenTree* op1, GenTree* op2);
    GenTreeHWIntrinsic* gtNewSimdHWIntrinsicNode(var_types      type,
                                                 NamedIntrinsic hwIntrinsicID,
                                                 var_types      baseType,
                                                 unsigned       size,
                                                 GenTree*       op1,
                                                 GenTree*       op2,
                                                 GenTree*       op3);
    GenTreeHWIntrinsic* gtNewSimdHWIntrinsicNode(var_types      type,
                                                 NamedIntrinsic hwIntrinsicID,
                                                 var_types      baseType,
                                                 unsigned       size,
                                                 GenTree*       op1,
                                                 GenTree*       op2,
                                                 GenTree*       op3,
                                                 GenTree*       op4);
    GenTreeHWIntrinsic* gtNewSimdHWIntrinsicNode(var_types      type,
                                                 NamedIntrinsic hwIntrinsicID,
                                                 var_types      baseType,
                                                 unsigned       size,
                                                 GenTree*       op1,
                                                 GenTree*       op2,
                                                 GenTree*       op3,
                                                 GenTree*       op4,
                                                 GenTree*       op5);
    GenTreeHWIntrinsic* gtNewSimdHWIntrinsicNode(var_types      type,
                                                 NamedIntrinsic hwIntrinsicID,
                                                 var_types      baseType,
                                                 unsigned       size,
                                                 unsigned       numOps,
                                                 GenTree**      ops);

    GenTreeHWIntrinsic* gtNewScalarHWIntrinsicNode(var_types type, NamedIntrinsic hwIntrinsicID, GenTree* op1);
    GenTreeHWIntrinsic* gtNewScalarHWIntrinsicNode(var_types      type,
                                                   NamedIntrinsic hwIntrinsicID,
                                                   GenTree*       op1,
                                                   GenTree*       op2);
    GenTreeHWIntrinsic* gtNewScalarHWIntrinsicNode(
        var_types type, NamedIntrinsic hwIntrinsicID, GenTree* op1, GenTree* op2, GenTree* op3);
#endif // FEATURE_HW_INTRINSICS

    GenTreeLclFld* gtNewLclFldNode(unsigned lnum, var_types type, unsigned offset);
    GenTreeRetExpr* gtNewRetExpr(GenTreeCall* call);

    GenTreeIndir* gtNewIndir(var_types type, GenTree* addr);
    GenTreeFlags gtGetIndirExceptionFlags(GenTree* addr);
    GenTreeFieldAddr* gtNewFieldAddr(GenTree* addr, CORINFO_FIELD_HANDLE handle, unsigned offset);
    GenTreeFieldAddr* gtNewFieldAddr(GenTree* addr, FieldSeqNode* fieldSeq, unsigned offset);
    GenTreeIndir* gtNewFieldIndir(var_types type, GenTreeFieldAddr* fieldAddr);
    GenTreeIndir* gtNewFieldIndir(var_types type, unsigned layoutNum, GenTreeFieldAddr* fieldAddr);
    GenTreeFlags gtGetFieldIndirFlags(GenTreeFieldAddr* fieldAddr);

    GenTreeIndexAddr* gtNewArrayIndexAddr(GenTree* arr, GenTree* ind, var_types elemType);
    GenTreeIndexAddr* gtNewStringIndexAddr(GenTree* arr, GenTree* ind);
    GenTreeIndir* gtNewIndexIndir(var_types type, GenTreeIndexAddr* indexAddr);

    GenTreeArrLen* gtNewArrLen(GenTree* arr, uint8_t lenOffs, GenTreeFlags flags);
    GenTreeBoundsChk* gtNewBoundsChk(GenTree* index, GenTree* length, ThrowHelperKind kind);

    GenTree* gtNewNullCheck(GenTree* addr);

    void gtChangeOperToNullCheck(GenTree* tree);

    GenTreeOp* gtNewAssignNode(GenTree* dst, GenTree* src);

    GenTree* gtNewNothingNode();

    GenTree* gtUnusedValNode(GenTree* expr);

    GenTreeCast* gtNewCastNode(GenTree* op1, bool fromUnsigned, var_types toType);

    GenTreeAllocObj* gtNewAllocObjNode(
        unsigned int helper, bool helperHasSideEffects, CORINFO_CLASS_HANDLE clsHnd, var_types type, GenTree* op1);

    GenTree* gtNewRuntimeLookup(CORINFO_GENERIC_HANDLE hnd, CorInfoGenericHandleType hndTyp, GenTree* lookupTree);

    GenTreeIndir* gtNewMethodTableLookup(GenTree* obj);

    //------------------------------------------------------------------------
    // Other GenTree functions

    GenTree* gtClone(GenTree* tree, bool complexOK = false);

    GenTree* gtCloneExpr(GenTree*     tree,
                         GenTreeFlags addFlags = GTF_EMPTY,
                         unsigned     varNum   = BAD_VAR_NUM,
                         int          varVal   = 0);

    Statement* gtCloneStmt(Statement* stmt)
    {
        GenTree* exprClone = gtCloneExpr(stmt->GetRootNode());
        return gtNewStmt(exprClone, stmt->GetILOffsetX());
    }

    // Internal helper for cloning a call
    GenTreeCall* gtCloneExprCallHelper(GenTreeCall* call,
                                       GenTreeFlags addFlags   = GTF_EMPTY,
                                       unsigned     deepVarNum = BAD_VAR_NUM,
                                       int          deepVarVal = 0);

    // Create copy of an inline or guarded devirtualization candidate tree.
    GenTreeCall* gtCloneCandidateCall(GenTreeCall* call);

    void gtUpdateStmtSideEffects(Statement* stmt);
    void gtUpdateAncestorsSideEffects(GenTree* tree);
    void gtUpdateNodeSideEffects(GenTree* node);

    // Returns "true" iff the complexity (not formally defined, but first interpretation
    // is #of nodes in subtree) of "tree" is greater than "limit".
    // (This is somewhat redundant with the "GetCostEx()/GetCostSz()" fields, but can be used
    // before they have been set.)
    bool gtComplexityExceeds(GenTree* tree, unsigned limit);

    static void gtReverseRelop(GenTreeOp* relop);
    GenTree* gtReverseCond(GenTree* tree);

    INDEBUG(unsigned gtHashValue(GenTree* tree);)

    LclVarDsc* gtIsLikelyRegVar(GenTree* tree);

    // Returns true iff tree2 can be executed before tree1.
    bool gtCanSwapOrder(GenTree* tree1, GenTree* tree2);

    static GenTree* gtGetFirstNode(GenTree* tree);
    GenTree* gtSetTreeSeq(GenTree* tree, bool isLIR = false);
    INDEBUG(void gtCheckTreeSeq(GenTree* tree, bool isLIR = false);)
    void gtSetStmtOrder(Statement* stmt);
    void gtSetStmtSeq(Statement* stmt);
    unsigned gtSetOrder(GenTree* tree);
    unsigned gtSetCallArgsOrder(const GenTreeCall::UseList& args);
    void gtSetCosts(GenTree* tree);
    void gtSetCallArgsCosts(const GenTreeCall::UseList& args,
                            bool                        lateArgs,
                            unsigned*                   callCostEx,
                            unsigned*                   callCostSz);
    bool gtMarkAddrMode(GenTree* addr, var_types indirType, unsigned* indirCostEx, unsigned* indirCostSz);

    // Returns "true" iff "node" has any of the side effects in "flags".
    bool gtNodeHasSideEffects(GenTree* node, GenTreeFlags flags, bool ignoreCctors = false);

    // Returns "true" iff "tree" or its (transitive) children have any of the side effects in "flags".
    bool gtTreeHasSideEffects(GenTree* tree, GenTreeFlags flags, bool ignoreCctors = false);

    // Appends 'expr' in front of 'list'
    //    'list' will typically start off as 'nullptr'
    //    when 'list' is non-null a GT_COMMA node is used to insert 'expr'
    GenTree* gtBuildCommaList(GenTree* list, GenTree* expr);

    GenTree* gtExtractSideEffList(GenTree* expr, GenTreeFlags flags = GTF_SIDE_EFFECT, bool ignoreRoot = false);

    // Return true if call is a recursive call; return false otherwise.
    // Note when inlining, this looks for calls back to the root method.
    bool gtIsRecursiveCall(GenTreeCall* call)
    {
        return gtIsRecursiveCall(call->gtCallMethHnd);
    }

    bool gtIsRecursiveCall(CORINFO_METHOD_HANDLE callMethodHandle)
    {
        return (callMethodHandle == impInlineRoot()->info.compMethodHnd);
    }

    //-------------------------------------------------------------------------

    GenTree* gtFoldExpr(GenTree* tree);
    GenTree* gtFoldExprConst(GenTree* tree);
    GenTree* gtFoldExprSpecial(GenTreeOp* tree);
    GenTree* gtFoldBoxNullable(GenTree* tree);
    GenTree* gtFoldExprCompare(GenTree* tree);
    GenTree* gtCreateHandleCompare(genTreeOps             oper,
                                   GenTree*               op1,
                                   GenTree*               op2,
                                   CorInfoInlineTypeCheck typeCheckInliningResult);
    GenTree* gtFoldExprCall(GenTreeCall* call);
    GenTree* gtFoldTypeCompare(GenTree* tree);
    GenTree* gtFoldTypeEqualityCall(bool isEq, GenTree* op1, GenTree* op2);

    // Options to control behavior of gtTryRemoveBoxUpstreamEffects
    enum BoxRemovalOptions
    {
        BR_REMOVE_AND_NARROW, // remove effects, minimize remaining work, return possibly narrowed source tree
        BR_REMOVE_AND_NARROW_WANT_TYPE_HANDLE, // remove effects and minimize remaining work, return type handle tree
        BR_REMOVE_BUT_NOT_NARROW,              // remove effects, return original source tree
        BR_DONT_REMOVE,                        // check if removal is possible, return copy source tree
        BR_DONT_REMOVE_WANT_TYPE_HANDLE,       // check if removal is possible, return type handle tree
        BR_MAKE_LOCAL_COPY                     // revise box to copy to temp local and return local's address
    };

    GenTree* gtTryRemoveBoxUpstreamEffects(GenTreeBox* box, BoxRemovalOptions options = BR_REMOVE_AND_NARROW);
    GenTree* gtOptimizeEnumHasFlag(GenTree* thisOp, GenTree* flagOp);

    // Get the handle for a ref type.
    CORINFO_CLASS_HANDLE gtGetClassHandle(GenTree* tree, bool* pIsExact, bool* pIsNonNull);
    // Get the class handle for an helper call
    CORINFO_CLASS_HANDLE gtGetHelperCallClassHandle(GenTreeCall* call, bool* pIsExact, bool* pIsNonNull);
    // Get the element handle for an array of ref type.
    CORINFO_CLASS_HANDLE gtGetArrayElementClassHandle(GenTree* array);
    // Get a class handle from a helper call argument
    CORINFO_CLASS_HANDLE gtGetHelperArgClassHandle(GenTree* array);
    // Get the class handle for a field
    CORINFO_CLASS_HANDLE gtGetFieldClassHandle(CORINFO_FIELD_HANDLE fieldHnd, bool* pIsExact, bool* pIsNonNull);
    // Check if this tree is a gc static base helper call
    bool gtIsStaticGCBaseHelperCall(GenTree* tree);

//-------------------------------------------------------------------------
// Functions to display the trees

#ifdef DEBUG
    void gtDispNode(GenTree* tree);
    void gtDispNodeHeader(GenTree* tree);
    int dmpNodeFlags(GenTree* node);
    int gtDispFlags(GenTreeFlags flags, GenTreeDebugFlags debugFlags);
    void gtDispConst(GenTree* tree);
    void gtDispLeaf(GenTree* tree);
    void dmpLclVarCommon(GenTreeLclVarCommon* node);
    void dmpSsaDefUse(GenTree* node);
    void dmpExtract(GenTreeExtract* extract);
    void dmpInsert(GenTreeInsert* insert);
    void dmpVarSetDiff(const char* name, VARSET_VALARG_TP from, VARSET_VALARG_TP to);
    void gtDispNodeName(GenTree* tree);
    void dmpNodeRegs(GenTree* node);
    void dmpNodeOperands(GenTree* node);
    void gtDispZeroFieldSeq(GenTree* tree);
    void gtDispCommonEndLine(GenTree* tree);
    void gtDispTree(GenTree* tree, bool header = true, bool operands = true);
    void gtDispTreeRec(GenTree* tree, IndentStack* indentStack, const char* msg, bool topOnly, bool isLIR, bool header);
    void gtGetLclVarNameInfo(unsigned lclNum, const char** ilKindOut, const char** ilNameOut, unsigned* ilNumOut);
    int gtGetLclVarName(unsigned lclNum, char* buf, unsigned buf_remaining);
    int dmpLclName(unsigned lclNum);
    char* gtGetLclVarName(unsigned lclNum);
    void gtDispLclVar(unsigned lclNum, bool padForBiggestDisp = true);
    void gtDispClassLayout(ClassLayout* layout, var_types type);
    void gtDispStmt(Statement* stmt, const char* msg = nullptr);
    void gtDispBlockStmts(BasicBlock* block);
    void gtGetCallArgMsg(GenTreeCall* call, GenTree* arg, unsigned argNum, char* buf, unsigned bufLength);
    void gtGetCallArgMsg(GenTreeCall* call, CallArgInfo* argInfo, GenTree* arg, char* buf, unsigned bufLength);
    void dmpFieldSeqFields(FieldSeqNode* fieldSeq);

    void gtDispRange(LIR::ReadOnlyRange const& range);

    void gtDispTreeRange(LIR::Range& containingRange, GenTree* tree);

    void gtDispLIRNode(GenTree* node);
#endif

    // For tree walks

    struct fgWalkData
    {
        Compiler* compiler;
        void*     pCallbackData; // user-provided data
        GenTree*  parent;        // parent of current node, provided to callback

        fgWalkData(Compiler* compiler, void* callbackData) : compiler(compiler), pCallbackData(callbackData)
        {
        }
    };

    enum fgWalkResult
    {
        WALK_CONTINUE,
        WALK_SKIP_SUBTREES,
        WALK_ABORT
    };

    typedef fgWalkResult(fgWalkPreFn)(GenTree** use, fgWalkData* data);
    typedef fgWalkResult(fgWalkPostFn)(GenTree** use, fgWalkData* data);

    fgWalkResult fgWalkTreePre(GenTree** use, fgWalkPreFn* visitor, void* callbackData = nullptr);
    fgWalkResult fgWalkTree(GenTree**     use,
                            fgWalkPreFn*  preVisitor,
                            fgWalkPostFn* postVisitor,
                            void*         callbackData = nullptr);
    fgWalkResult fgWalkTreePost(GenTree** use, fgWalkPostFn* visitor, void* callbackData = nullptr);

    struct FindLinkData
    {
        GenTree*  nodeToFind;
        GenTree** useEdge;
        GenTree*  user;
    };

    FindLinkData gtFindLink(Statement* stmt, GenTree* node);
    bool impHasCatchArg(GenTree* tree);

    typedef ArrayStack<GenTree*> GenTreeStack;

    static bool gtHasCallOnStack(GenTreeStack* parentStack);

//=========================================================================
// BasicBlock functions
#ifdef DEBUG
    // This is a debug flag we will use to assert when creating block during codegen
    // as this interferes with procedure splitting. If you know what you're doing, set
    // it to true before creating the block. (DEBUG only)
    bool fgSafeBasicBlockCreation = true;
#endif

    BasicBlock* bbNewBasicBlock(BBjumpKinds jumpKind);

    Phases      mostRecentlyActivePhase = PHASE_PRE_IMPORT;        // the most recently active phase
    PhaseChecks activePhaseChecks       = PhaseChecks::CHECK_NONE; // the currently active phase checks

    /*
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XX                                                                           XX
    XX                           LclVarsInfo                                     XX
    XX                                                                           XX
    XX   The variables to be used by the code generator.                         XX
    XX                                                                           XX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    */

    enum FrameLayoutState : uint8_t
    {
        NO_FRAME_LAYOUT,
#ifdef TARGET_ARMARCH
        REGALLOC_FRAME_LAYOUT,
#endif
        FINAL_FRAME_LAYOUT
    };

public:
    RefCountState lvaRefCountState = RCS_INVALID; // Current local ref count state
    bool          lvaEnregEHVars;
    bool          lvaAddressExposedLocalsMarked = false;
#if (defined(TARGET_AMD64) && !defined(UNIX_AMD64_ABI)) || defined(TARGET_ARM64)
    bool lvaHasImplicitByRefParams;
#endif
    bool lvaGenericsContextInUse = false;
    // The highest frame layout state that we've completed. During frame
    // layout calculations, this is the level we are currently computing.
    FrameLayoutState lvaDoneFrameLayout = NO_FRAME_LAYOUT;

    bool lvaLocalVarRefCounted() const
    {
        return lvaRefCountState == RCS_NORMAL;
    }

    unsigned lvaCount;     // total number of locals, which includes function arguments,
                           // special arguments, IL local variables, and JIT temporary variables
    unsigned lvaTableSize; // lvaTable size (>= lvaCount)

    LclVarDsc* lvaTable           = nullptr; // variable descriptor table
    unsigned*  lvaTrackedToVarNum = nullptr;

    unsigned lvaTrackedCount             = 0; // actual # of locals being tracked
    unsigned lvaTrackedCountInSizeTUnits = 0; // min # of size_t's sufficient to hold a bit for all tracked locals

    unsigned lvaCurEpoch = 0; // VarSets are relative to a specific set of tracked var indices.
                              // It that changes, this changes.  VarSets from different epochs
                              // cannot be meaningfully combined.

    unsigned GetCurLVEpoch()
    {
        return lvaCurEpoch;
    }

    // reverse map of tracked number to var number
    unsigned lvaTrackedToVarNumSize = 0;

    void lvaSetImplicitlyReferenced(unsigned lclNum);

    void lvaSetAddressExposed(unsigned lclNum);
    void lvaSetAddressExposed(LclVarDsc* lcl);

    // [[deprecated]]
    void lvaSetVarAddrExposed(unsigned lclNum)
    {
        lvaSetAddressExposed(lclNum);
    }

    void lvaSetLiveInOutOfHandler(unsigned lclNum);

    void lvSetMinOptsDoNotEnreg();

#ifdef DEBUG
    // Reasons why we can't enregister.  Some of these correspond to debug properties of local vars.
    enum DoNotEnregisterReason
    {
        DNER_AddrExposed,
        DNER_IsStruct,
        DNER_LocalField,
        DNER_LiveInOutOfHandler,
        DNER_BlockOp,     // Is read or written via a block operation that explicitly takes the address.
        DNER_IsStructArg, // Is a struct passed as an argument in a way that requires a stack location.
        DNER_DepField,    // It is a field of a dependently promoted struct
        DNER_NoRegVars,   // opts.compFlags & CLFLG_REGVAR is not set
#ifndef TARGET_64BIT
        DNER_LongParamField, // It is a decomposed field of a long parameter.
        DNER_LongUnpromoted,
#endif
#ifdef JIT32_GCENCODER
        DNER_PinningRef,
#endif
        DNER_HasImplicitRefs
    };
#endif

    void lvaSetDoNotEnregister(unsigned lclNum DEBUGARG(DoNotEnregisterReason reason));
    void lvaSetDoNotEnregister(LclVarDsc* lcl DEBUGARG(DoNotEnregisterReason reason));

    // [[deprecated]]
    void lvaSetVarDoNotEnregister(unsigned lclNum DEBUGARG(DoNotEnregisterReason reason))
    {
        lvaSetDoNotEnregister(lclNum DEBUGARG(reason));
    }

    unsigned lvaVarargsHandleArg = BAD_VAR_NUM;
#ifdef TARGET_X86
    // Pointer (computed based on incoming varargs handle) to the start of the stack arguments
    unsigned lvaVarargsBaseOfStkArgs = BAD_VAR_NUM;
#endif

    unsigned lvaPInvokeFrameListVar    = BAD_VAR_NUM; // lclNum for the Frame root
    unsigned lvaInlinedPInvokeFrameVar = BAD_VAR_NUM; // variable representing the InlinedCallFrame
    unsigned lvaReversePInvokeFrameVar = BAD_VAR_NUM; // variable representing the reverse PInvoke frame
    unsigned lvaMonAcquired            = BAD_VAR_NUM; // boolean variable introduced into in synchronized methods
                                                      // that tracks whether the lock has been taken

    // Same as info.compThisArg, except when "this" is address taken or stored to - in which
    // case this is a temp local initialized from the "this" arg at the start of the method.
    unsigned lvaThisLclNum = BAD_VAR_NUM;

#if FEATURE_FIXED_OUT_ARGS
    unsigned lvaOutgoingArgSpaceVar = BAD_VAR_NUM; // TYP_BLK local for fixed outgoing argument space
#endif

    // Variable representing the return address. The helper-based tailcall
    // mechanism passes the address of the return address to a runtime helper
    // where it is used to detect tail-call chains.
    unsigned lvaRetAddrVar = BAD_VAR_NUM;

#if defined(DEBUG) && defined(TARGET_XARCH)
    // Stores SP to confirm it is not corrupted on return.
    unsigned lvaReturnSpCheck = BAD_VAR_NUM;
    // Stores SP to confirm it is not corrupted after every call.
    X86_ONLY(unsigned lvaCallSpCheck = BAD_VAR_NUM;)
#endif

    bool lvaKeepAliveAndReportThis(); // Synchronized instance method of a reference type, or
                                      // CORINFO_GENERICS_CTXT_FROM_THIS?
    bool lvaReportParamTypeArg();     // Exceptions and CORINFO_GENERICS_CTXT_FROM_PARAMTYPEARG?

//-------------------------------------------------------------------------
// All these frame offsets are inter-related and must be kept in sync

#if !defined(FEATURE_EH_FUNCLETS)
    // This is used for the callable handlers
    unsigned lvaShadowSPslotsVar = BAD_VAR_NUM; // TYP_BLK variable for all the shadow SP slots
#endif

#ifdef JIT32_GCENCODER
    // variable which stores the value of ESP after the the last alloca/localloc
    unsigned lvaLocAllocSPvar = BAD_VAR_NUM;
#endif

    unsigned lvaNewObjArrayArgs = BAD_VAR_NUM; // variable with arguments for new MD array helper

    unsigned lvaGetParamAllocSize(LclVarDsc* lcl);
    static unsigned lvaGetParamAlignment(var_types type, bool isFloatHfa);
    void lvaAssignFrameOffsets(FrameLayoutState curState);
    void lvaFixVirtualFrameOffsets();
#ifdef TARGET_ARMARCH
    void lvaAssignParamsVirtualFrameOffsets();
#endif
    void lvaAssignLocalsVirtualFrameOffsets();
    int lvaAllocLocalAndSetVirtualOffset(unsigned lclNum, unsigned size, int stkOffs);
#ifdef TARGET_AMD64
    // Returns true if compCalleeRegsPushed (including RBP if used as frame pointer) is even.
    bool lvaIsCalleeSavedIntRegCountEven();
#endif
    void lvaAlignFrame();
    void lvaAssignPromotedFieldsVirtualFrameOffsets();
    int lvaAllocateTemps(int stkOffs
#ifndef TARGET_64BIT
                         ,
                         bool mustDoubleAlign
#endif
                         );

#ifdef DEBUG
    void lvaDumpRegLocation(unsigned lclNum);
    void lvaDumpFrameLocation(unsigned lclNum);
    void lvaDumpEntry(unsigned lclNum, size_t refCntWtdWidth = 6);
    void lvaTableDump();
#endif

    void lvaIncrementFrameSize(unsigned size);

#ifdef TARGET_ARMARCH
    bool     lvaHasLargeFrameOffset();
    unsigned lvaEstimateFrameSize();
#endif

    // Returns the caller-SP-relative offset for the local variable "varNum."
    int lvaGetCallerSPRelativeOffset(unsigned lclNum);
    // Returns the caller-SP-relative offset for the SP/FP relative offset determined by FP based.
    int lvaToCallerSPRelativeOffset(int offs, bool isFpBased, bool forRootFrame = true) const;
#ifdef TARGET_AMD64
    int lvaGetPSPSymInitialSPRelativeOffset();
#endif

    // True if this is an OSR compilation and this local is potentially
    // located on the original method stack frame.
    bool lvaIsOSRLocal(unsigned varNum);

    //------------------------ For splitting types ----------------------------

    bool lvaInitRetType();
    bool lvaInitLocalsCount();
    void lvaInitTable();
    void lvaInitLocals();
    void lvaInitInline();

    void lvaInitParams(bool hasRetBufParam);
    void lvaInitThisParam(ParamAllocInfo& paramInfo);
    void lvaInitRetBufParam(ParamAllocInfo& paramInfo, bool useFixedRetBufReg);
    void lvaInitGenericsContextParam(ParamAllocInfo& paramInfo);
    void lvaInitVarargsHandleParam(ParamAllocInfo& paramInfo);
    void lvaInitUserParams(ParamAllocInfo& paramInfo, bool skipFirstParam);
    void lvaInitUserParam(ParamAllocInfo& paramInfo, CORINFO_ARG_LIST_HANDLE param);
    void lvaAllocUserParam(ParamAllocInfo& paramInfo, LclVarDsc* lcl);
#ifdef TARGET_ARM
    void lvaAlignPreSpillParams(regMaskTP doubleAlignMask);
#endif

    void lvaInitVarDsc(LclVarDsc* lcl, CorInfoType corType, CORINFO_CLASS_HANDLE typeHnd);

    LclVarDsc* lvaGetDesc(unsigned lclNum)
    {
        assert(lclNum < lvaCount);
        return &lvaTable[lclNum];
    }

    LclVarDsc* lvaGetDesc(const GenTreeLclVarCommon* lclVar)
    {
        assert(lclVar->GetLclNum() < lvaCount);
        return &lvaTable[lclVar->GetLclNum()];
    }

    LclVarDsc* lvaGetDesc(const GenTreeLclAddr* lclAddr)
    {
        assert(lclAddr->GetLclNum() < lvaCount);
        return &lvaTable[lclAddr->GetLclNum()];
    }

    unsigned lvaTrackedIndexToLclNum(unsigned trackedIndex)
    {
        assert(trackedIndex < lvaTrackedCount);
        unsigned lclNum = lvaTrackedToVarNum[trackedIndex];
        assert(lclNum < lvaCount);
        return lclNum;
    }

    LclVarDsc* lvaGetDescByTrackedIndex(unsigned trackedIndex)
    {
        return lvaGetDesc(lvaTrackedIndexToLclNum(trackedIndex));
    }

    bool lvaHaveManyLocals() const;

    unsigned lvaNewTemp(var_types type, bool shortLifetime DEBUGARG(const char* reason));
    unsigned lvaNewTemp(ClassLayout* layout, bool shortLifetime DEBUGARG(const char* reason));
    unsigned lvaNewTemp(CORINFO_CLASS_HANDLE classHandle, bool shortLifetime DEBUGARG(const char* reason));
    unsigned lvaNewTemp(GenTree* tree, bool shortLifetime DEBUGARG(const char* reason));

    unsigned lvaGrabTemp(bool shortLifetime DEBUGARG(const char* reason));
    unsigned lvaGrabTemps(unsigned count DEBUGARG(const char* reason));

    void lvaResizeTable(unsigned newSize);

    void lvaMarkLivenessTrackedLocals();

    void lvaSetImplictlyReferenced();
    void lvaComputeLclRefCounts();
    void lvaComputeRefCountsHIR();
    void lvaComputeRefCountsLIR();
    void lvaAddRef(LclVarDsc* lcl, BasicBlock::weight_t weight, bool propagate = true);

#ifdef DEBUG
    void lvaDispVarSet(VARSET_VALARG_TP set, VARSET_VALARG_TP allVars);
    void lvaDispVarSet(VARSET_VALARG_TP set);
#endif

    int lvaFrameAddress(int varNum, bool* pFPbased);

    bool lvaIsOriginalThisParam(unsigned lclNum);

    bool lvaIsImplicitByRefLocal(unsigned lclNum)
    {
        return lvaGetDesc(lclNum)->IsImplicitByRefParam();
    }

    void lvaSetStruct(unsigned lclNum, CORINFO_CLASS_HANDLE classHandle, bool checkUnsafeBuffer);
    void lvaSetStruct(unsigned lclNum, ClassLayout* layout, bool checkUnsafeBuffer);
    void lvaSetStruct(LclVarDsc* lcl, ClassLayout* layout, bool checkUnsafeBuffer);

    // If the local is TYP_REF, set or update the associated class information.
    void lvaSetClass(unsigned varNum, CORINFO_CLASS_HANDLE clsHnd, bool isExact = false);
    void lvaSetClass(unsigned varNum, GenTree* tree, CORINFO_CLASS_HANDLE stackHandle = nullptr);
    void lvaUpdateClass(unsigned varNum, CORINFO_CLASS_HANDLE clsHnd, bool isExact = false);
    void lvaUpdateClass(unsigned varNum, GenTree* tree, CORINFO_CLASS_HANDLE stackHandle = nullptr);

    bool lvaTempsHaveLargerOffsetThanVars();

    unsigned lvaGSSecurityCookie = BAD_VAR_NUM; // LclVar number
    unsigned lvaStubArgumentVar  = BAD_VAR_NUM; // variable representing the secret stub argument
#ifdef FEATURE_EH_FUNCLETS
    unsigned lvaPSPSym = BAD_VAR_NUM; // variable representing the PSPSym
#endif
    unsigned genReturnLocal = BAD_VAR_NUM; // Local number for the return value when applicable.
    unsigned fgLargeFieldOffsetNullCheckTemps[3];

    unsigned              impSharedStackSize = 0;
    Importer::StackEntry* impSharedStack     = nullptr;
    InlineInfo*           impInlineInfo;
    InlineStrategy*       m_inlineStrategy = nullptr;
    Compiler*             InlineeCompiler  = nullptr; // The Compiler instance for the inlinee
    InlineResult*         compInlineResult = nullptr; // The result of importing the inlinee method.
    CORINFO_CLASS_HANDLE  impPromotableStructTypeCache[2]{};

    // The Compiler* that is the root of the inlining tree of which "this" is a member.
    Compiler* impInlineRoot();

#if defined(DEBUG) || defined(INLINE_DATA)
    unsigned __int64 getInlineCycleCount()
    {
        return m_compCycles;
    }
#endif // defined(DEBUG) || defined(INLINE_DATA)

    //=========================================================================
    //                          PROTECTED
    //=========================================================================

protected:
    //---------------- Local variable ref-counting ----------------------------

    void lvaMarkLclRefs(GenTree* tree, GenTree* user, BasicBlock* block, Statement* stmt);
    bool IsDominatedByExceptionalEntry(BasicBlock* block);

public:
    /*
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XX                                                                           XX
    XX                           Importer                                        XX
    XX                                                                           XX
    XX   Imports the given method and converts it to semantic trees              XX
    XX                                                                           XX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    */

    CORINFO_CLASS_HANDLE impGetRefAnyClass();
    CORINFO_CLASS_HANDLE impGetStringClass();
    CORINFO_CLASS_HANDLE impGetObjectClass();

    Importer::StackEntry* impAllocStack(unsigned size);

    bool impIsThis(GenTree* obj);

    void impAssignCallWithRetBuf(GenTree* dest, GenTreeCall* call);

    BoxPattern impBoxPatternMatch(const BYTE* codeAddr, const BYTE* codeEnd, unsigned* patternSize);

    bool impILConsumesAddr(const BYTE* codeAddr);

    void impResolveToken(const BYTE* addr, CORINFO_RESOLVED_TOKEN* resolvedToken, CorInfoTokenKind kind);

    void impDevirtualizeCall(GenTreeCall*            call,
                             CORINFO_RESOLVED_TOKEN* pResolvedToken,
                             CORINFO_METHOD_HANDLE*  method,
                             unsigned*               methodFlags,
                             CORINFO_CONTEXT_HANDLE* contextHandle,
                             CORINFO_CONTEXT_HANDLE* exactContextHandle,
                             Importer*               importer,
                             bool                    isExplicitTailCall,
                             IL_OFFSETX              ilOffset = BAD_IL_OFFSET);

    void impLateDevirtualizeCall(GenTreeCall* call);
    void impLateDevirtualizeCall(GenTreeCall*            call,
                                 InlineCandidateInfo*    inlineInfo,
                                 CORINFO_METHOD_HANDLE*  methodHnd,
                                 CORINFO_CONTEXT_HANDLE* context);

    //=========================================================================
    //                          PROTECTED
    //=========================================================================

protected:
    CORINFO_CLASS_HANDLE impGetSpecialIntrinsicExactReturnType(CORINFO_METHOD_HANDLE specialIntrinsicHandle);

    NamedIntrinsic lookupNamedIntrinsic(CORINFO_METHOD_HANDLE method);

    NamedIntrinsic impFindSysNumSimdIntrinsic(CORINFO_METHOD_HANDLE method,
                                              const char*           className,
                                              const char*           methodName,
                                              const char*           enclosingClassName);

    bool compSupportsHWIntrinsic(CORINFO_InstructionSet isa);

public:
    void impMakeDiscretionaryInlineObservations(InlineInfo* pInlineInfo, InlineResult* inlineResult);

    GenTree* gtNewRuntimeContextTree(CORINFO_RUNTIME_LOOKUP_KIND kind);

    GenTreeCall* gtNewReadyToRunHelperCallNode(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                               CorInfoHelpFunc         helper,
                                               var_types               type,
                                               GenTreeCall::Use*       args               = nullptr,
                                               CORINFO_LOOKUP_KIND*    pGenericLookupKind = nullptr);

    bool IsIntrinsicImplementedByUserCall(NamedIntrinsic intrinsicName);
    bool IsTargetIntrinsic(NamedIntrinsic intrinsicName);
    bool IsMathIntrinsic(NamedIntrinsic intrinsicName);
    bool IsMathIntrinsic(GenTree* tree);

private:
    //----------------- Importing the method ----------------------------------

    CORINFO_CONTEXT_HANDLE impTokenLookupContextHandle; // The context used for looking up tokens.

#ifdef TARGET_ARMARCH
    var_types mangleVarArgsType(var_types type);
#endif

#ifdef DEBUG
    static unsigned jitTotalMethodCompiled;
    static LONG     jitNestingLevel;
#endif

    static GenTreeLclAddr* impIsAddressInLocal(GenTree* tree);
    static GenTreeLclAddr* impIsLocalAddrExpr(GenTree* node);
    bool impHasLclRef(GenTree* tree, unsigned lclNum);
    bool impHasAddressTakenLocals(GenTree* tree);

    // STATIC inlining decision based on the IL code.
    void impCanInlineIL(CORINFO_METHOD_HANDLE fncHandle,
                        CORINFO_METHOD_INFO*  methInfo,
                        bool                  forceInline,
                        InlineResult*         inlineResult);

    void impCheckCanInline(GenTreeCall*           call,
                           CORINFO_METHOD_HANDLE  fncHandle,
                           unsigned               methAttr,
                           CORINFO_CONTEXT_HANDLE exactContextHnd,
                           InlineCandidateInfo**  ppInlineCandidateInfo,
                           InlineResult*          inlineResult);

    bool impTailCallRetTypeCompatible(GenTreeCall* call, bool allowWidening);

    bool impIsClassExact(CORINFO_CLASS_HANDLE classHnd);

    /*
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XX                                                                           XX
    XX                           FlowGraph                                       XX
    XX                                                                           XX
    XX   Info about the basic-blocks, their contents and the flow analysis       XX
    XX                                                                           XX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    */

public:
    BasicBlock* fgFirstBB        = nullptr; // Beginning of the basic block list
    BasicBlock* fgLastBB         = nullptr; // End of the basic block list
    BasicBlock* fgFirstColdBlock = nullptr; // First block to be placed in the cold section
    BasicBlock* fgEntryBB        = nullptr; // For OSR, the original method's entry point
#if defined(FEATURE_EH_FUNCLETS)
    // First block of outlined funclets (to allow block insertion before the funclets)
    BasicBlock* fgFirstFuncletBB = nullptr;
#endif
    // Block inserted for initialization stuff. Is nullptr if no such block has been created.
    BasicBlock*     fgFirstBBScratch = nullptr;
    BasicBlockList* fgReturnBlocks   = nullptr; // list of BBJ_RETURN blocks
    unsigned        fgEdgeCount      = 0;       // # of control flow edges between the BBs
    unsigned        fgBBcount        = 0;       // # of BBs in the method
#ifdef DEBUG
    unsigned fgBBcountAtCodegen = 0; // # of BBs in the method at the start of codegen
#endif
    unsigned fgBBNumMax   = 0; // The max bbNum that has been assigned to basic blocks
    unsigned fgDomBBcount = 0; // # of BBs for which we have dominator and reachability information

    // After the dominance tree is computed, we cache a DFS preorder number and DFS postorder number to compute
    // dominance queries in O(1). fgDomTreePreOrder and fgDomTreePostOrder are arrays giving the block's preorder and
    // postorder number, respectively. The arrays are indexed by basic block number. (Note that blocks are numbered
    // starting from one. Thus, we always waste element zero. This makes debugging easier and makes the code less likely
    // to suffer from bugs stemming from forgetting to add or subtract one from the block number to form an array
    // index). The arrays are of size fgBBNumMax + 1.
    unsigned* fgDomTreePreOrder;
    unsigned* fgDomTreePostOrder;

    BlockSet fgEnterBlks = BlockSetOps::UninitVal(); // Set of blocks which have a special transfer of control; the
                                                     // "entry" blocks plus EH handler begin blocks.

    jitstd::vector<flowList*>* fgPredListSortVector = nullptr;

    // Allocate array like T* a = new T[fgBBNumMax + 1];
    // Using helper so we don't keep forgetting +1.
    template <typename T>
    T* fgAllocateTypeForEachBlk(CompMemKind cmk)
    {
        return getAllocator(cmk).allocate<T>(fgBBNumMax + 1);
    }

    // BlockSets are relative to a specific set of BasicBlock numbers. If that changes
    // (if the blocks are renumbered), this changes. BlockSets from different epochs
    // cannot be meaningfully combined. Note that new blocks can be created with higher
    // block numbers without changing the basic block epoch. These blocks *cannot*
    // participate in a block set until the blocks are all renumbered, causing the epoch
    // to change. This is useful if continuing to use previous block sets is valuable.
    // If the epoch is zero, then it is uninitialized, and block sets can't be used.
    unsigned fgCurBBEpoch = 0;

    unsigned GetCurBasicBlockEpoch()
    {
        return fgCurBBEpoch;
    }

    // The number of basic blocks in the current epoch. When the blocks are renumbered,
    // this is fgBBcount. As blocks are added, fgBBcount increases, fgCurBBEpochSize remains
    // the same, until a new BasicBlock epoch is created, such as when the blocks are all renumbered.
    unsigned fgCurBBEpochSize = 0;

    // The number of "size_t" elements required to hold a bitset large enough for fgCurBBEpochSize
    // bits. This is precomputed to avoid doing math every time BasicBlockBitSetTraits::GetArrSize() is called.
    unsigned fgBBSetCountInSizeTUnits = 0;

    void NewBasicBlockEpoch()
    {
        INDEBUG(unsigned oldEpochArrSize = fgBBSetCountInSizeTUnits);

        // We have a new epoch. Compute and cache the size needed for new BlockSets.
        fgCurBBEpoch++;
        fgCurBBEpochSize = fgBBNumMax + 1;
        fgBBSetCountInSizeTUnits =
            roundUp(fgCurBBEpochSize, (unsigned)(sizeof(size_t) * 8)) / unsigned(sizeof(size_t) * 8);

#ifdef DEBUG
        // All BlockSet objects are now invalid!
        fgReachabilitySetsValid = false; // the bbReach sets are now invalid!
        fgEnterBlksSetValid     = false; // the fgEnterBlks set is now invalid!

        if (verbose)
        {
            unsigned epochArrSize = BasicBlockBitSetTraits::GetArrSize(this, sizeof(size_t));
            printf("\nNew BlockSet epoch %d, # of blocks (including unused BB00): %u, bitset array size: %u (%s)",
                   fgCurBBEpoch, fgCurBBEpochSize, epochArrSize, (epochArrSize <= 1) ? "short" : "long");
            if ((fgCurBBEpoch != 1) && ((oldEpochArrSize <= 1) != (epochArrSize <= 1)))
            {
                // If we're not just establishing the first epoch, and the epoch array size has changed such that we're
                // going to change our bitset representation from short (just a size_t bitset) to long (a pointer to an
                // array of size_t bitsets), then print that out.
                printf("; NOTE: BlockSet size was previously %s!", (oldEpochArrSize <= 1) ? "short" : "long");
            }
            printf("\n");
        }
#endif // DEBUG
    }

    void EnsureBasicBlockEpoch()
    {
        if (fgCurBBEpochSize != fgBBNumMax + 1)
        {
            NewBasicBlockEpoch();
        }
    }

    void fgEnsureFirstBBisScratch();
    bool fgFirstBBisScratch();
    bool fgBBisScratch(BasicBlock* block);

    void fgExtendEHRegionBefore(BasicBlock* block);
    void fgExtendEHRegionAfter(BasicBlock* block);

    BasicBlock* fgNewBBbefore(BBjumpKinds jumpKind, BasicBlock* block, bool extendRegion);

    BasicBlock* fgNewBBafter(BBjumpKinds jumpKind, BasicBlock* block, bool extendRegion);

    BasicBlock* fgNewBBinRegion(BBjumpKinds jumpKind,
                                unsigned    tryIndex,
                                unsigned    hndIndex,
                                BasicBlock* nearBlk,
                                bool        putInFilter = false,
                                bool        runRarely   = false,
                                bool        insertAtEnd = false);

    BasicBlock* fgNewBBinRegion(BBjumpKinds jumpKind,
                                BasicBlock* srcBlk,
                                bool        runRarely   = false,
                                bool        insertAtEnd = false);

    BasicBlock* fgNewBBinRegion(BBjumpKinds jumpKind);

    BasicBlock* fgNewBBinRegionWorker(BBjumpKinds jumpKind,
                                      BasicBlock* afterBlk,
                                      unsigned    xcptnIndex,
                                      bool        putInTryRegion);

    void fgInsertBBbefore(BasicBlock* insertBeforeBlk, BasicBlock* newBlk);
    void fgInsertBBafter(BasicBlock* insertAfterBlk, BasicBlock* newBlk);
    void fgUnlinkBlock(BasicBlock* block);

#ifdef FEATURE_JIT_METHOD_PERF
    unsigned fgMeasureIR();
#endif // FEATURE_JIT_METHOD_PERF

    bool fgModified         = false; // True if the flow graph has been modified recently
    bool fgComputePredsDone = false; // Have we computed the bbPreds list
    bool fgCheapPredsValid  = false; // Is the bbCheapPreds list valid?
    bool fgDomsComputed     = false; // Have we computed the dominator sets?
    bool fgOptimizedFinally = false; // Did we optimize any try-finallys?
    bool fgHasSwitch        = false; // any BBJ_SWITCH jumps?

#ifdef DEBUG
    bool fgReachabilitySetsValid = false; // Are the bbReach sets valid?
    bool fgEnterBlksSetValid     = false; // Is the fgEnterBlks set valid?
    bool fgLinearOrder           = false;
#endif

    bool fgRemoveRestOfBlock; // true if we know that we will throw

    // The following are boolean flags that keep track of the state of internal data structures

    bool fgStmtListThreaded       = false; // true if the node list is now threaded
    bool fgEdgeWeightsComputed    = false; // true after we have called fgComputeEdgeWeights
    bool fgHaveValidEdgeWeights   = false; // true if we were successful in computing all of the edge weights
    bool fgSlopUsedInEdgeWeights  = false; // true if their was some slop used when computing the edge weights
    bool fgRangeUsedInEdgeWeights = true;  // true if some of the edgeWeight are expressed in Min..Max form
    bool fgNeedsUpdateFlowGraph   = false; // true if we need to run fgUpdateFlowGraph

#ifdef FEATURE_EH_FUNCLETS
    bool fgFuncletsCreated = false; // true if the funclet creation phase has been run
#endif

    bool fgGlobalMorph = false; // indicates if we are during the global morphing phase
                                // since fgMorphTree can be called from several places

    bool fgLoopCallMarked = false; // The following check for loops that don't execute calls
    bool fgHasLoops       = false; // True if this method has any loops, set in fgComputeReachability

#ifdef DEBUG
    bool fgLocalVarLivenessDone = false;
    bool jitFallbackCompile; // Are we doing a fallback compile? That is, have we executed a NO_WAY assert,
                             //   and we are trying to compile again in a "safer", minopts mode?

    bool fgPrintInlinedMethods = false;
#endif

    BasicBlock::weight_t fgCalledCount = BB_ZERO_WEIGHT; // count of the number of times this method was called
                                                         // This is derived from the profile data or is
                                                         // BB_UNITY_WEIGHT when we don't have profile data

    //-------------------------------------------------------------------------

    PhaseStatus fgImport();

    PhaseStatus fgTransformIndirectCalls();

    PhaseStatus fgTransformPatchpoints();

    PhaseStatus fgInline();

    PhaseStatus fgRemoveEmptyTry();

    PhaseStatus fgRemoveEmptyFinally();

    PhaseStatus fgMergeFinallyChains();

    PhaseStatus fgCloneFinally();

    void fgCleanupContinuation(BasicBlock* continuation);

#if defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)

    PhaseStatus fgUpdateFinallyTargetFlags();

    void fgClearAllFinallyTargetBits();

    void fgAddFinallyTargetFlags();

#endif // defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)

    PhaseStatus fgTailMergeThrows();
    void fgTailMergeThrowsFallThroughHelper(BasicBlock* predBlock,
                                            BasicBlock* nonCanonicalBlock,
                                            BasicBlock* canonicalBlock,
                                            flowList*   predEdge);
    void fgTailMergeThrowsJumpToHelper(BasicBlock* predBlock,
                                       BasicBlock* nonCanonicalBlock,
                                       BasicBlock* canonicalBlock,
                                       flowList*   predEdge);

#if defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
    // Sometimes we need to defer updating the BBF_FINALLY_TARGET bit. fgNeedToAddFinallyTargetBits signals
    // when this is necessary.
    bool fgNeedToAddFinallyTargetBits;
#endif // defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)

    bool fgRetargetBranchesToCanonicalCallFinally(BasicBlock*      block,
                                                  BasicBlock*      handler,
                                                  BlockToBlockMap& continuationMap);

    GenTree* gtNewStaticMethodMonitorAddr();

#if defined(FEATURE_EH_FUNCLETS)

    void fgAddSyncMethodEnterExit();

    void fgInsertMonitorCall(BasicBlock* block, CorInfoHelpFunc helper, unsigned thisLclNum);

    void fgConvertSyncReturnToLeave(BasicBlock* block);

#endif // FEATURE_EH_FUNCLETS

    void fgAddReversePInvokeEnterExit();

    // The number of separate return points in the method.
    unsigned fgReturnCount = 0;

    void fgAddInternal();

    bool fgFoldConditional(BasicBlock* block);

    void fgMorphBlocks();
    void fgMorphStmts(BasicBlock* block);

    void fgMergeBlockReturn(BasicBlock* block);

    GenTreeOp* fgIsCommaThrow(GenTree* tree DEBUGARG(bool forFolding));
    GenTreeCall* fgIsThrow(GenTree* tree);
    bool fgMorphBlockStmt(BasicBlock* block, Statement* stmt DEBUGARG(const char* msg));

#ifdef DEBUG
    static fgWalkPreFn fgAssertNoQmark;
    void fgPreExpandQmarkChecks(GenTree* expr);
    void fgPostExpandQmarkChecks();
#endif

    IL_OFFSET fgFindBlockILOffset(BasicBlock* block);

    BasicBlock* fgSplitBlockAtBeginning(BasicBlock* curr);
    BasicBlock* fgSplitBlockAtEnd(BasicBlock* curr);
    BasicBlock* fgSplitBlockAfterStatement(BasicBlock* curr, Statement* stmt);
    BasicBlock* fgSplitBlockAfterNode(BasicBlock* curr, GenTree* node); // for LIR
    BasicBlock* fgSplitEdge(BasicBlock* curr, BasicBlock* succ);

    GenTreeQmark* fgGetTopLevelQmark(GenTree* expr, GenTreeLclVar** destLclVar);
    void fgExpandQmarkForCastInstOf(BasicBlock* block, Statement* stmt);
    void fgExpandQmarkStmt(BasicBlock* block, Statement* stmt);
    void fgExpandQmarkNodes();

    GenTreeCall* gtNewInitThisClassHelperCall();
    GenTreeCall* gtNewSharedCctorHelperCall(CORINFO_CLASS_HANDLE cls);
    GenTreeCall* gtNewSharedStaticsCctorHelperCall(CORINFO_CLASS_HANDLE cls, CorInfoHelpFunc helper);

    void fgLocalVarLiveness();
    void fgLocalVarLivenessUntracked();
    void livInitNewBlock(BasicBlock* block);

    struct LivenessState
    {
        VARSET_TP fgCurUseSet; // vars used by block (before an assignment)
        VARSET_TP fgCurDefSet; // vars assigned by block (before a use)

        bool fgCurMemoryUse : 1;   // True iff the current basic block uses memory.
        bool fgCurMemoryDef : 1;   // True iff the current basic block modifies memory.
        bool fgCurMemoryHavoc : 1; // True if the current basic block is known to set memory to a "havoc" value.
    };

    void fgMarkUseDef(LivenessState& state, GenTreeLclVarCommon* tree);
    void fgPerNodeLocalVarLiveness(LivenessState& state, GenTree* node);
    void fgPerBlockLocalVarLiveness();
    void fgPerBlockLocalVarLivenessLIR();

    VARSET_VALRET_TP fgGetHandlerLiveVars(BasicBlock* block);

    void fgLiveVarAnalysis();

    void fgComputeLifeTrackedLocalUse(VARSET_TP& liveOut, LclVarDsc* lcl, GenTreeLclVarCommon* node);
    bool fgComputeLifeTrackedLocalDef(VARSET_TP&           liveOut,
                                      VARSET_VALARG_TP     keepAlive,
                                      LclVarDsc*           lcl,
                                      GenTreeLclVarCommon* node);
    bool fgComputeLifePromotedLocal(VARSET_TP&           liveOut,
                                    VARSET_VALARG_TP     keepAlive,
                                    LclVarDsc*           lcl,
                                    GenTreeLclVarCommon* node);

    bool fgComputeLifeBlock(VARSET_TP& liveOut, VARSET_VALARG_TP keepAlive, BasicBlock* block);
    bool fgComputeLifeStmt(VARSET_TP& liveOut, VARSET_VALARG_TP keepAlive, Statement* stmt, BasicBlock* block);
    bool fgComputeLifeLIR(VARSET_TP& liveOut, VARSET_VALARG_TP keepAlive, BasicBlock* block);

    GenTree* fgRemoveDeadStore(GenTreeLclVarCommon* store, Statement* stmt, BasicBlock* block);

    void fgInterBlockLocalVarLivenessUntracked();
    bool fgInterBlockLocalVarLiveness();

    // Blocks: convenience methods for enabling range-based `for` iteration over the function's blocks, e.g.:
    // 1.   for (BasicBlock* const block : compiler->Blocks()) ...
    // 2.   for (BasicBlock* const block : compiler->Blocks(startBlock)) ...
    // 3.   for (BasicBlock* const block : compiler->Blocks(startBlock, endBlock)) ...
    // In case (1), the block list can be empty. In case (2), `startBlock` can be nullptr. In case (3),
    // both `startBlock` and `endBlock` must be non-null.
    //
    BasicBlockSimpleList Blocks() const
    {
        return BasicBlockSimpleList(fgFirstBB);
    }

    BasicBlockSimpleList Blocks(BasicBlock* startBlock) const
    {
        return BasicBlockSimpleList(startBlock);
    }

    BasicBlockRangeList Blocks(BasicBlock* startBlock, BasicBlock* endBlock) const
    {
        return BasicBlockRangeList(startBlock, endBlock);
    }

    // Returns "true" if this is a special variable that is never zero initialized in the prolog.
    bool lvaIsNeverZeroInitializedInProlog(unsigned lclNum);

    // Returns "true" if the variable needs explicit zero initialization.
    inline bool fgVarNeedsExplicitZeroInit(unsigned varNum, bool bbInALoop, bool bbIsReturn);

    // The value numbers for this compilation.
    ValueNumStore* vnStore = nullptr;

public:
    bool isTrivialPointerSizedStruct(ClassLayout* layout) const;
    bool isNativePrimitiveStructType(ClassLayout* layout);
    var_types abiGetStructIntegerRegisterType(ClassLayout* layout);
    StructPassing abiGetStructParamType(ClassLayout* layout, bool isVarArg);
    StructPassing abiGetStructReturnType(ClassLayout* layout, CorInfoCallConvExtension callConv, bool isVarArgs);

    bool fgDominate(BasicBlock* b1, BasicBlock* b2); // Return true if b1 dominates b2

    // Dominator computation member functions
    // Not exposed outside Compiler
    bool fgReachable(BasicBlock* b1, BasicBlock* b2); // Returns true if block b1 can reach block b2

    // Compute immediate dominators, the dominator tree and and its pre/post-order travsersal numbers.
    void fgComputeDoms();

    void fgCompDominatedByExceptionalEntryBlocks(BasicBlock** postOrder);

    BlockSet_ValRet_T fgGetDominatorSet(BasicBlock* block); // Returns a set of blocks that dominate the given block.
    // Note: this is relatively slow compared to calling fgDominate(),
    // especially if dealing with a single block versus block check.

    void fgComputeReachabilitySets(); // Compute bbReach sets. (Also sets BBF_GC_SAFE_POINT flag on blocks.)

    void fgComputeEnterBlocksSet(); // Compute the set of entry blocks, 'fgEnterBlks'.

    bool fgRemoveUnreachableBlocks(); // Remove blocks determined to be unreachable by the bbReach sets.

    void fgComputeReachability(); // Perform flow graph node reachability analysis.

    BasicBlock* fgIntersectDom(BasicBlock* a, BasicBlock* b); // Intersect two immediate dominator sets.

    BasicBlock** fgDfsInvPostOrder();
    void fgDfsInvPostOrderHelper(BasicBlock** postOrder, BasicBlock* block, BlockSet& visited, unsigned* count);

    BlockSet_ValRet_T fgDomFindStartNodes(); // Computes which basic blocks don't have incoming edges in the flow graph.
                                             // Returns this as a set.

    INDEBUG(void fgDispDomTree(DomTreeNode* domTree);) // Helper that prints out the Dominator Tree in debug builds.

    void         fgEnsureDomTreeRoot();
    DomTreeNode* fgBuildDomTree(); // Once we compute all the immediate dominator sets for each node in the flow graph
                                   // (performed by fgComputeDoms), this procedure builds the dominance tree represented
                                   // adjacency lists.

    // In order to speed up the queries of the form 'Does A dominates B', we can perform a DFS preorder and postorder
    // traversal of the dominance tree and the dominance query will become A dominates B iif preOrder(A) <= preOrder(B)
    // && postOrder(A) >= postOrder(B) making the computation O(1).
    void fgNumberDomTree(DomTreeNode* domTree);

    // When the flow graph changes, we need to update the block numbers, predecessor lists, reachability sets.
    void fgUpdateChangedFlowGraph(bool computePreds);

public:
    // Compute the predecessors of the blocks in the control flow graph.
    void fgComputePreds();

    // Remove all predecessor information.
    void fgRemovePreds();

    // Compute the cheap flow graph predecessors lists. This is used in some early phases
    // before the full predecessors lists are computed.
    void fgComputeCheapPreds();

private:
    void fgAddCheapPred(BasicBlock* block, BasicBlock* blockPred);

    void fgRemoveCheapPred(BasicBlock* block, BasicBlock* blockPred);

public:
    enum GCPollType
    {
        GCPOLL_NONE,
        GCPOLL_CALL,
        GCPOLL_INLINE
    };

    BasicBlock* fgCreateGCPoll(GCPollType pollType, BasicBlock* block);

    // Requires that "block" is a block that returns from
    // a finally.  Returns the number of successors (jump targets of
    // of blocks in the covered "try" that did a "LEAVE".)
    unsigned fgNSuccsOfFinallyRet(BasicBlock* block);

    // Requires that "block" is a block that returns (in the sense of BBJ_EHFINALLYRET) from
    // a finally.  Returns its "i"th successor (jump targets of
    // of blocks in the covered "try" that did a "LEAVE".)
    // Requires that "i" < fgNSuccsOfFinallyRet(block).
    BasicBlock* fgSuccOfFinallyRet(BasicBlock* block, unsigned i);

private:
    // Factor out common portions of the impls of the methods above.
    void fgSuccOfFinallyRetWork(BasicBlock* block, unsigned i, BasicBlock** bres, unsigned* nres);

public:
    // Invalidate the map of unique switch block successors. For example, since the hash key of the map
    // depends on block numbers, we must invalidate the map when the blocks are renumbered, to ensure that
    // we don't accidentally look up and return the wrong switch data.
    void InvalidateUniqueSwitchSuccMap();

    // Requires "switchBlock" to be a block that ends in a switch.  Returns
    // the corresponding SwitchUniqueSuccSet.
    BBswtDesc* GetDescriptorForSwitch(BasicBlock* switchBlk);

    // The switch block "switchBlk" just had an entry with value "from" modified to the value "to".
    // Update "this" as necessary: if "from" is no longer an element of the jump table of "switchBlk",
    // remove it from "this", and ensure that "to" is a member.
    void UpdateSwitchTableTarget(BasicBlock* switchBlk, BasicBlock* from, BasicBlock* to);

    // Remove the "SwitchUniqueSuccSet" of "switchBlk" in the BlockToSwitchDescMap.
    void fgInvalidateSwitchDescMapEntry(BasicBlock* switchBlk);

    BasicBlock* fgFirstBlockOfHandler(BasicBlock* block);

    flowList* fgGetPredForBlock(BasicBlock* block, BasicBlock* blockPred);

    flowList* fgGetPredForBlock(BasicBlock* block, BasicBlock* blockPred, flowList*** ptrToPred);

    flowList* fgRemoveRefPred(BasicBlock* block, BasicBlock* blockPred);

    flowList* fgRemoveAllRefPreds(BasicBlock* block, BasicBlock* blockPred);

    void fgRemoveBlockAsPred(BasicBlock* block);

    void fgChangeSwitchBlock(BasicBlock* oldSwitchBlock, BasicBlock* newSwitchBlock);

    void fgReplaceSwitchJumpTarget(BasicBlock* blockSwitch, BasicBlock* newTarget, BasicBlock* oldTarget);

    void fgReplaceJumpTarget(BasicBlock* block, BasicBlock* newTarget, BasicBlock* oldTarget);

    void fgReplacePred(BasicBlock* block, BasicBlock* oldPred, BasicBlock* newPred);

    flowList* fgAddRefPred(BasicBlock* block,
                           BasicBlock* blockPred,
                           flowList*   oldEdge           = nullptr,
                           bool        initializingPreds = false); // Only set to 'true' when we are computing preds in
                                                                   // fgComputePreds()

    struct ILStats
    {
        unsigned instrCount;
        unsigned lclRefCount;
    };

    void compCreateBasicBlocks(ILStats& ilStats);
    void compCreateEHTable();
    INDEBUG(void dmpILJumpTargets(FixedBitVect* targets);)

    bool fgIsBetterFallThrough(BasicBlock* bCur, BasicBlock* bAlt);

    bool fgCheckEHCanInsertAfterBlock(BasicBlock* blk, unsigned regionIndex, bool putInTryRegion);

    BasicBlock* fgFindInsertPoint(unsigned    regionIndex,
                                  bool        putInTryRegion,
                                  BasicBlock* startBlk,
                                  BasicBlock* endBlk,
                                  BasicBlock* nearBlk,
                                  BasicBlock* jumpBlk,
                                  bool        runRarely);

    unsigned fgGetNestingLevel(BasicBlock* block, unsigned* pFinallyNesting = nullptr);

    void fgRemoveEmptyBlocks();

    void fgRemoveStmt(BasicBlock* block, Statement* stmt DEBUGARG(bool dumpStmt = true));

    void fgUnlinkStmt(BasicBlock* block, Statement* stmt);

    bool fgMorphRemoveUselessStmt(BasicBlock* block, Statement* stmt);

    void fgCreateLoopPreHeader(unsigned lnum);

    void fgUnreachableBlock(BasicBlock* block);

    void fgRemoveConditionalJump(BasicBlock* block);

    BasicBlock* fgLastBBInMainFunction();

    BasicBlock* fgEndBBAfterMainFunction();

    void fgUnlinkRange(BasicBlock* bBeg, BasicBlock* bEnd);

    void fgRemoveBlock(BasicBlock* block, bool unreachable);

    bool fgCanCompactBlocks(BasicBlock* block, BasicBlock* bNext);

    void fgCompactBlocks(BasicBlock* block, BasicBlock* bNext);

    void fgUpdateLoopsAfterCompacting(BasicBlock* block, BasicBlock* bNext);

    BasicBlock* fgConnectFallThrough(BasicBlock* bSrc, BasicBlock* bDst);

    bool fgRenumberBlocks();

    bool fgExpandRarelyRunBlocks();

    bool fgEhAllowsMoveBlock(BasicBlock* bBefore, BasicBlock* bAfter);

    void fgMoveBlocksAfter(BasicBlock* bStart, BasicBlock* bEnd, BasicBlock* insertAfterBlk);

    enum FG_RELOCATE_TYPE
    {
        FG_RELOCATE_TRY,    // relocate the 'try' region
        FG_RELOCATE_HANDLER // relocate the handler region (including the filter if necessary)
    };
    BasicBlock* fgRelocateEHRange(unsigned regionIndex, FG_RELOCATE_TYPE relocateType);

#if defined(FEATURE_EH_FUNCLETS)
#if defined(TARGET_ARM)
    void fgClearFinallyTargetBit(BasicBlock* block);
#endif // defined(TARGET_ARM)
    bool fgIsIntraHandlerPred(BasicBlock* predBlock, BasicBlock* block);
    bool fgAnyIntraHandlerPreds(BasicBlock* block);
    void fgInsertFuncletPrologBlock(BasicBlock* block);
    void fgCreateFuncletPrologBlocks();
    void fgCreateFunclets();
#else  // !FEATURE_EH_FUNCLETS
    bool fgRelocateEHRegions();
#endif // !FEATURE_EH_FUNCLETS

    bool fgOptimizeUncondBranchToSimpleCond(BasicBlock* block, BasicBlock* target);

    bool fgBlockEndFavorsTailDuplication(BasicBlock* block, unsigned lclNum);

    bool fgBlockIsGoodTailDuplicationCandidate(BasicBlock* block, unsigned* lclNum);

    bool fgOptimizeEmptyBlock(BasicBlock* block);

    bool fgOptimizeBranchToEmptyUnconditional(BasicBlock* block, BasicBlock* bDest);

    bool fgOptimizeBranch(BasicBlock* bJump);

    bool fgOptimizeSwitchBranches(BasicBlock* block, Lowering* lowering);

    bool fgOptimizeBranchToNext(BasicBlock* block, BasicBlock* bNext, BasicBlock* bPrev);

    bool fgOptimizeSwitchJumps();
#ifdef DEBUG
    void fgPrintEdgeWeights();
#endif
    void                 fgComputeBlockAndEdgeWeights();
    BasicBlock::weight_t fgComputeMissingBlockWeights();
    void fgComputeCalledCount(BasicBlock::weight_t returnWeight);
    void fgComputeEdgeWeights();
    bool fgReorderBlocks();
    bool fgIsForwardBranch(BasicBlock* bJump, BasicBlock* bSrc = nullptr);
    bool fgUpdateFlowGraph(Lowering* lowering = nullptr, bool doTailDup = false);

    // method that returns if you should split here
    typedef bool(fgSplitPredicate)(GenTree* tree, GenTree* parent, fgWalkData* data);

    void fgRemoveReturnBlock(BasicBlock* block);

    /* Helper code that has been factored out */
    inline void fgConvertBBToThrowBB(BasicBlock* block);

    bool gtIsSmallIntCastNeeded(GenTree* tree, var_types toType);
    GenTree* fgMorphNormalizeLclVarStore(GenTreeOp* asg);

    void fgLoopCallTest(BasicBlock* srcBB, BasicBlock* dstBB);
    bool fgReachWithoutCall(BasicBlock* srcBB, BasicBlock* dstBB);
    void fgLoopCallMark();

    unsigned fgGetCodeSizeEstimate(BasicBlock* block, unsigned limit);

#if DUMP_FLOWGRAPHS
    enum class PhasePosition
    {
        PrePhase,
        PostPhase
    };
    const char* fgProcessEscapes(const char* nameIn, escapeMapping_t* map);
    static void fgDumpTree(FILE* fgxFile, GenTree* const tree);
    FILE* fgOpenFlowGraphFile(bool* wbDontClose, Phases phase, PhasePosition pos, LPCWSTR type);
    bool fgDumpFlowGraph(Phases phase, PhasePosition pos);
#endif // DUMP_FLOWGRAPHS

#ifdef DEBUG
    void fgDispDoms(BasicBlock** postOrder);
    void fgDispReach();
    void fgDispBBLocalLiveness(BasicBlock* block);
    void fgDispBBLiveness(BasicBlock* block);
    void fgDispBBLiveness();
    void fgTableDispBasicBlock(BasicBlock* block, int ibcColWidth = 0);
    void fgDispBasicBlocks(BasicBlock* firstBlock, BasicBlock* lastBlock, bool dumpTrees);
    void fgDispBasicBlocks(bool dumpTrees = false);
    void fgDumpBlock(BasicBlock* block);
    void fgDumpTrees(BasicBlock* firstBlock, BasicBlock* lastBlock);

    void fgDebugCheckUpdate();
    void fgDebugCheckBBlist(bool checkBBNum = false, bool checkBBRefs = true);
    void fgDebugCheckBlockLinks();
    void fgDebugCheckLinks(bool morphTrees = false);
    void fgDebugCheckStmtsList(BasicBlock* block, bool morphTrees);
    void fgDebugCheckNodeLinks(BasicBlock* block, Statement* stmt);
    void fgDebugCheckNodesUniqueness();
    void fgDebugCheckLoopTable();

    void fgDebugCheckFlags(GenTree* tree);
    void fgDebugCheckTryFinallyExits();
    void fgDebugCheckProfileData();
    bool fgDebugCheckIncomingProfileData(BasicBlock* block);
    bool fgDebugCheckOutgoingProfileData(BasicBlock* block);
#endif

    bool fgProfileWeightsEqual(BasicBlock::weight_t weight1, BasicBlock::weight_t weight2);
    bool fgProfileWeightsConsistent(BasicBlock::weight_t weight1, BasicBlock::weight_t weight2);

    /**************************************************************************
     *                          PROTECTED
     *************************************************************************/

protected:
    //--------------------- Detect the basic blocks ---------------------------

    BasicBlock** fgBBs; // Table of pointers to the BBs

    void        fgInitBBLookup();
    BasicBlock* fgLookupBB(unsigned addr);

    bool fgMayExplicitTailCall();

    FixedBitVect* fgFindJumpTargets(ILStats* ilStats = nullptr);

    void fgMarkBackwardJump(BasicBlock* startBlock, BasicBlock* endBlock);

    void fgLinkBasicBlocks();

    unsigned fgMakeBasicBlocks(FixedBitVect* jumpTarget);

    void fgCheckBasicBlockControlFlow();

    void fgControlFlowPermitted(BasicBlock* blkSrc,
                                BasicBlock* blkDest,
                                bool        IsLeave = false /* is the src a leave block */);

    bool fgFlowToFirstBlockOfInnerTry(BasicBlock* blkSrc, BasicBlock* blkDest, bool sibling);

    void fgObserveInlineConstants(OPCODE opcode, const FgStack& stack, InlineInfo* inlineInfo);

    void fgAdjustForAddressTakenOrStoredThis();

    unsigned fgStressBBProf()
    {
#ifdef DEBUG
        unsigned result = JitConfig.JitStressBBProf();
        if (result == 0)
        {
            if (compStressCompile(STRESS_BB_PROFILE, 15))
            {
                result = 1;
            }
        }
        return result;
#else
        return 0;
#endif
    }

    bool fgHaveProfileData();
    bool fgGetProfileWeightForBasicBlock(IL_OFFSET offset, BasicBlock::weight_t* weight);

    Instrumentor* fgCountInstrumentor = nullptr;
    Instrumentor* fgClassInstrumentor = nullptr;

    PhaseStatus fgPrepareToInstrumentMethod();
    PhaseStatus fgInstrumentMethod();
    PhaseStatus fgIncorporateProfileData();
    void        fgIncorporateBlockCounts();
    void        fgIncorporateEdgeCounts();

    static CORINFO_CLASS_HANDLE getRandomClass(ICorJitInfo::PgoInstrumentationSchema* schema,
                                               UINT32                                 countSchemaItems,
                                               BYTE*                                  pInstrumentationData,
                                               int32_t                                ilOffset,
                                               CLRRandom*                             random);

public:
    const char*                            fgPgoFailReason              = nullptr;
    ICorJitInfo::PgoInstrumentationSchema* fgPgoSchema                  = nullptr;
    BYTE*                                  fgPgoData                    = nullptr;
    ICorJitInfo::PgoSource                 fgPgoSource                  = ICorJitInfo::PgoSource::Unknown;
    UINT32                                 fgPgoSchemaCount             = 0;
    HRESULT                                fgPgoQueryResult             = E_FAIL;
    UINT32                                 fgNumProfileRuns             = 0;
    UINT32                                 fgPgoBlockCounts             = 0;
    UINT32                                 fgPgoEdgeCounts              = 0;
    UINT32                                 fgPgoClassProfiles           = 0;
    unsigned                               fgPgoInlineePgo              = 0;
    unsigned                               fgPgoInlineeNoPgo            = 0;
    unsigned                               fgPgoInlineeNoPgoSingleBlock = 0;

    void WalkSpanningTree(SpanningTreeVisitor* visitor);
    void fgSetProfileWeight(BasicBlock* block, BasicBlock::weight_t weight);
    void fgApplyProfileScale();
    bool fgHaveSufficientProfileData();
    bool fgHaveTrustedProfileData();

    // fgIsUsingProfileWeights - returns true if we have real profile data for this method
    //                           or if we have some fake profile data for the stress mode
    bool fgIsUsingProfileWeights()
    {
        return (fgHaveProfileData() || fgStressBBProf());
    }

    // fgProfileRunsCount - returns total number of scenario runs for the profile data
    //                      or BB_UNITY_WEIGHT_UNSIGNED when we aren't using profile data.
    unsigned fgProfileRunsCount()
    {
        return fgIsUsingProfileWeights() ? fgNumProfileRuns : BB_UNITY_WEIGHT_UNSIGNED;
    }

#ifdef DEBUG
    bool fgBlockContainsStatementBounded(BasicBlock* block, Statement* stmt, bool answerOnBoundExceeded = true);
#endif

public:
    Statement* fgNewStmtAtBeg(BasicBlock* block, GenTree* tree);
    void fgInsertStmtAtEnd(BasicBlock* block, Statement* stmt);
    Statement* fgNewStmtAtEnd(BasicBlock* block, GenTree* tree);
    Statement* fgNewStmtNearEnd(BasicBlock* block, GenTree* tree);

private:
    void fgInsertStmtNearEnd(BasicBlock* block, Statement* stmt);
    void fgInsertStmtAtBeg(BasicBlock* block, Statement* stmt);
    void fgInsertStmtAfter(BasicBlock* block, Statement* insertionPoint, Statement* stmt);

public:
    void fgInsertStmtBefore(BasicBlock* block, Statement* insertionPoint, Statement* stmt);

private:
    Statement* fgInsertStmtListAfter(BasicBlock* block, Statement* stmtAfter, Statement* stmtList);

    GenTreeLclVar* fgInsertCommaFormTemp(GenTree** use);
    GenTree* fgMakeMultiUse(GenTree** ppTree);

private:
    //                  Recognize a bitwise rotation pattern and convert into a GT_ROL or a GT_ROR node.
    GenTree* fgRecognizeAndMorphBitwiseRotation(GenTree* tree);
    bool fgOperIsBitwiseRotationRoot(genTreeOps oper);

#if !defined(TARGET_64BIT)
    //                  Recognize and morph a long multiplication with 32 bit operands.
    GenTreeOp* fgRecognizeAndMorphLongMul(GenTreeOp* mul);
    GenTreeOp* fgMorphLongMul(GenTreeOp* mul);
#endif

private:
#ifndef TARGET_X86
    hashBv* m_abiStructArgTemps      = nullptr;
    hashBv* m_abiStructArgTempsInUse = nullptr;
#endif

    void fgMoveOpsLeft(GenTree* tree);

    bool fgInDifferentRegions(BasicBlock* blk1, BasicBlock* blk2);
    bool fgIsBlockCold(BasicBlock* block);

    GenTree* fgMorphCastIntoHelper(GenTreeCast* cast, int helper);

    GenTreeCall* fgMorphIntoHelperCall(GenTree* tree, int helper, GenTreeCall::Use* args, bool morphArgs = true);

    // A "MorphAddrContext" carries information from the surrounding context.  If we are evaluating a byref address,
    // it is useful to know whether the address will be immediately dereferenced, or whether the address value will
    // be used, perhaps by passing it as an argument to a called method.  This affects how null checking is done:
    // for sufficiently small offsets, we can rely on OS page protection to implicitly null-check addresses that we
    // know will be dereferenced.  To know that reliance on implicit null checking is sound, we must further know that
    // all offsets between the top-level indirection and the bottom are constant, and that their sum is sufficiently
    // small; hence the other fields of MorphAddrContext.
    struct MorphAddrContext
    {
        const bool    isAddressTaken;
        bool          isOffsetConstant;
        target_size_t offset;

        MorphAddrContext(bool isAddressTaken) : isAddressTaken(isAddressTaken), isOffsetConstant(true), offset(0)
        {
        }
    };

    GenTree* fgMorphStringIndexIndir(GenTreeIndexAddr* index, GenTreeStrCon* str);
    GenTree* fgMorphCast(GenTreeCast* cast);
    GenTree* fgMorphCastPost(GenTreeCast* cast);
    void fgInitArgInfo(GenTreeCall* call);
    void fgMorphArgs(GenTreeCall* call);

    GenTree* fgMorphLclVar(GenTreeLclVar* lclVar);

public:
    GenTree* fgMorphIndexAddr(GenTreeIndexAddr* tree);
    bool fgAddrCouldBeNull(GenTree* addr);

private:
    GenTree* fgMorphFieldAddr(GenTreeFieldAddr* field, MorphAddrContext* mac);
#if FEATURE_FASTTAILCALL
    bool fgCanFastTailCall(GenTreeCall* call, const char** failReason);
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
    bool fgCallHasMustCopyByrefParameter(CallInfo* callInfo);
#endif
#endif
    bool fgCheckStmtAfterTailCall(Statement* callStmt);
    GenTree* fgMorphTailCallViaHelpers(GenTreeCall* call, CORINFO_TAILCALL_HELPERS& help, Statement* stmt);
#ifdef TARGET_X86
    bool fgCanTailCallViaJitHelper();
    void fgMorphTailCallViaJitHelper(GenTreeCall* call);
#endif
    GenTree* fgCreateCallDispatcherAndGetResult(GenTreeCall*          origCall,
                                                CORINFO_METHOD_HANDLE callTargetStubHnd,
                                                CORINFO_METHOD_HANDLE dispatcherHnd,
                                                Statement*            stmt);
    GenTree* getRuntimeLookupTree(CORINFO_RUNTIME_LOOKUP_KIND kind,
                                  CORINFO_RUNTIME_LOOKUP&     lookup,
                                  void*                       compileTimeHandle);
    GenTree* getVirtMethodPointerTree(GenTree*                thisPtr,
                                      CORINFO_RESOLVED_TOKEN* resolvedToken,
                                      CORINFO_CALL_INFO*      callInfo);
    GenTree* getTokenHandleTree(CORINFO_RESOLVED_TOKEN* resolvedToken, bool parent);

    GenTree* fgMorphPotentialTailCall(GenTreeCall* call, Statement* stmt);
    GenTree* fgGetStubAddrArg(GenTreeCall* call);
    void fgMorphRecursiveFastTailCallIntoLoop(BasicBlock* block, GenTreeCall* recursiveTailCall);
    void fgMorphCreateLclInit(unsigned lclNum, BasicBlock* block, Statement* beforeStmt, IL_OFFSETX ilOffset);
    Statement* fgAssignRecursiveCallArgToCallerParam(GenTree*       arg,
                                                     fgArgTabEntry* argTabEntry,
                                                     BasicBlock*    block,
                                                     IL_OFFSETX     callILOffset,
                                                     Statement*     tmpAssignmentInsertionPoint,
                                                     Statement*     paramAssignmentInsertionPoint);
    GenTree* fgMorphCall(GenTreeCall* call, Statement* stmt);
    GenTree* fgRemoveArrayStoreHelperCall(GenTreeCall* call, GenTree* value);
    GenTree* fgExpandVirtualVtableCallTarget(GenTreeCall* call);
    GenTree* fgMorphLeaf(GenTree* tree);
    GenTree* fgMorphInitStruct(GenTreeOp* asg);
    GenTree* fgMorphPromoteLocalInitStruct(GenTreeOp* asg, LclVarDsc* destLclVar, GenTree* initVal);
    GenTree* fgMorphInitStructConstant(GenTreeIntCon* initVal,
                                       var_types      type,
                                       bool           extendToActualType,
                                       var_types      simdBaseType);
    GenTree* fgMorphStructComma(GenTree* tree);
    GenTree* fgMorphStructAssignment(GenTreeOp* asg);
#ifdef FEATURE_SIMD
    GenTree* fgMorphPromoteSimdAssignmentSrc(GenTreeOp* asg, unsigned srcLclNum);
    GenTree* fgMorphPromoteSimdAssignmentDst(GenTreeOp* asg, unsigned destLclNum);
#endif
    GenTree* fgMorphDynBlk(GenTreeDynBlk* dynBlk);
    GenTree* fgMorphBlockAssignment(GenTreeOp* asg);
    GenTree* fgMorphCopyStruct(GenTreeOp* asg);
    GenTree* fgMorphPromoteStore(GenTreeOp* store, GenTree* tempStore, GenTree** fieldStores, unsigned fieldCount);
    GenTree* fgMorphQmark(GenTreeQmark* qmark, MorphAddrContext* mac = nullptr);
    GenTree* fgMorphSmpOp(GenTree* tree, MorphAddrContext* mac = nullptr);
    GenTree* fgMorphModToSubMulDiv(GenTreeOp* tree);
    GenTree* fgMorphSmpOpOptional(GenTreeOp* tree);
    GenTree* fgMorphStrCon(GenTreeStrCon* tree, Statement* stmt, BasicBlock* block);
    GenTree* fgMorphAssociative(GenTreeOp* tree);

#ifndef TARGET_64BIT
    enum class MulLongCandidateKind
    {
        None,
        Const,
        Shift,
        Signed,
        Unsigned
    };

    MulLongCandidateKind fgMorphIsMulLongCandidate(GenTreeOp* mul);
    GenTree* fgMorphMulLongCandidate(GenTreeOp* mul, MulLongCandidateKind kind);
#endif

#ifdef FEATURE_HW_INTRINSICS
    GenTree* fgMorphHWIntrinsic(GenTreeHWIntrinsic* tree);
#endif

public:
    GenTree* gtMorphTree(GenTree* tree)
    {
        return fgMorphTree(tree);
    }

private:
    GenTree* fgMorphTree(GenTree* tree, MorphAddrContext* mac = nullptr);
    INDEBUG(void fgMorphClearDebugNodeMorphed(GenTree* tree);)
    void fgMorphTreeDone(GenTree* tree, GenTree* oldTree = nullptr DEBUGARG(int morphNum = 0));

    Statement*  fgGlobalMorphStmt = nullptr;
    BasicBlock* fgMorphBlock      = nullptr;

    unsigned fgGetLargeFieldOffsetNullCheckTemp(var_types type); // We cache one temp per type to be
                                                                 // used when morphing big offset.
    ThrowHelperBlock* m_throwHelperBlockList = nullptr;

public:
    static CorInfoHelpFunc GetThrowHelperCall(ThrowHelperKind kind);

    BasicBlock* fgGetThrowHelperBlock(ThrowHelperKind kind, BasicBlock* throwBlock);
    BasicBlock* fgGetThrowHelperBlock(ThrowHelperKind kind, BasicBlock* throwBlock, unsigned throwIndex);
    ThrowHelperBlock* fgFindThrowHelperBlock(ThrowHelperKind kind, BasicBlock* throwBlock);
    ThrowHelperBlock* fgFindThrowHelperBlock(ThrowHelperKind kind, unsigned throwIndex);
#if !FEATURE_FIXED_OUT_ARGS
    ThrowHelperBlock* fgFindThrowHelperBlock(BasicBlock* block);
#endif

    bool fgUseThrowHelperBlocks() const
    {
        return !opts.compDbgCode;
    }

    void inlReplaceRetExpr(BasicBlock* block, Statement* stmt);
    void inlFoldJTrue(BasicBlock* block);
    bool inlInlineCall(BasicBlock* block, Statement* stmt, GenTreeCall* call);
    void inlInvokeInlineeCompiler(BasicBlock* block, Statement* stmt, GenTreeCall* call, InlineResult* result);
    void inlMain();
    void inlImportInlinee();
    void inlCreateBasicBlocks();
    void inlPostInlineFailureCleanup(const InlineInfo* inlineInfo);
    void inlAnalyzeInlineeReturn(InlineInfo* inlineInfo, unsigned returnBlockCount);
    bool inlImportReturn(Importer& importer, InlineInfo* inlineInfo, GenTree* op2, CORINFO_CLASS_HANDLE retClsHnd);
    void inlUpdateRetSpillTempClass(InlineInfo* inlineInfo);
    unsigned inlCheckInlineDepthAndRecursion(const InlineInfo* inlineInfo);
    bool inlIsSysNumOrSysRtIntrinsicClass(CORINFO_CLASS_HANDLE clsHnd);
    bool inlAnalyzeInlineeSignature(InlineInfo* inlineInfo);
    bool inlAnalyzeInlineeArg(InlineInfo* inlineInfo, unsigned argNum);
    GenTree* inlUseArg(InlineInfo* inlineInfo, unsigned ilArgNum);
    bool inlAnalyzeInlineeLocals(InlineInfo* inlineInfo);
    unsigned inlGetInlineeLocal(InlineInfo* inlineInfo, unsigned ilLocNum);
    unsigned inlAllocInlineeLocal(InlineInfo* inlineInfo, unsigned ilLocNum);
    void inlInsertInlineeCode(InlineInfo* pInlineInfo);
    Statement* inlInsertSingleBlockInlineeStatements(const InlineInfo* inlineInfo, Statement* stmtAfter);
    Statement* inlPrependStatements(InlineInfo* inlineInfo);
    Statement* inlInitInlineeArgs(const InlineInfo* inlineInfo, Statement* afterStmt);
    GenTree* inlAssignStruct(GenTreeLclVar* dest, GenTree* src);
    bool inlCanDiscardArgSideEffects(GenTree* argNode);
    Statement* inlInitInlineeLocals(const InlineInfo* inlineInfo, Statement* afterStmt);
    void inlNullOutInlineeGCLocals(const InlineInfo* inlineInfo, Statement* stmt);
    BasicBlock* inlSplitInlinerBlock(const InlineInfo* inlineInfo, Statement* stmtAfter);
    void inlInsertInlineeBlocks(const InlineInfo* inlineInfo, Statement* stmtAfter);
    void inlPropagateInlineeCompilerState();
    INDEBUG(void inlDebugCheckInlineCandidates();)

private:
    void fgPromoteStructs();

#if (defined(TARGET_AMD64) && !defined(UNIX_AMD64_ABI)) || defined(TARGET_ARM64)
    // Reset the refCount for implicit byrefs.
    void lvaResetImplicitByRefParamsRefCount();
    // Change implicit byrefs' types from struct to pointer, and for any that were
    // promoted, create new promoted struct temps.
    void lvaRetypeImplicitByRefParams();
    // Clear up annotations for any struct promotion temps created for implicit byrefs.
    void lvaDemoteImplicitByRefParams();
#endif

    void fgMarkAddressExposedLocals();

#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64) || defined(TARGET_X86)
    // Rewrite appearances of implicit byrefs (manifest the implied additional level of indirection)
    // or stack params of x86 varargs methods.
    void fgMorphIndirectParams(Statement* stmt);
#endif

    enum TypeProducerKind
    {
        TPK_Unknown = 0, // May not be a RuntimeType
        TPK_Handle  = 1, // RuntimeType via handle
        TPK_GetType = 2, // RuntimeType via Object.get_Type()
        TPK_Null    = 3, // Tree value is null
        TPK_Other   = 4  // RuntimeType via other means
    };

    TypeProducerKind gtGetTypeProducerKind(GenTree* tree);

    bool fgIsBigOffset(size_t offset);

    /*
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XX                                                                           XX
    XX                           Optimizer                                       XX
    XX                                                                           XX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    */

public:
    void optRemoveRangeCheck(GenTreeBoundsChk* check, GenTreeOp* comma, Statement* stmt);

private:
    // Requires "lnum" to be the index of an outermost loop in the loop table.  Traverses the body of that loop,
    // including all nested loops, and records the set of "side effects" of the loop: fields (object instance and
    // static) written to, and SZ-array element type equivalence classes updated.

    // Hoist the expression "expr" out of loop "lnum".
    void optPerformHoistExpr(GenTree* expr, unsigned lnum);

public:
    void optOptimizeBools();

public:
    PhaseStatus optInvertLoops();    // Invert loops so they're entered at top and tested at bottom.
    PhaseStatus optOptimizeLayout(); // Optimize the BasicBlock layout of the method
    PhaseStatus phFindLoops();       // Finds loops and records them in the loop table

    PhaseStatus phCloneLoops();
    void optEnsureUniqueHead(unsigned loopInd, BasicBlock::weight_t ambientWeight);
    PhaseStatus phUnrollLoops(); // Unrolls loops (needs to have cost info)

    // A "LoopDsc" describes a ("natural") loop.  We (currently) require the body of a loop to be a contiguous (in
    // bbNext order) sequence of basic blocks.  (At times, we may require the blocks in a loop to be "properly numbered"
    // in bbNext order; we use comparisons on the bbNum to decide order.)
    // The blocks that define the body are
    //   first <= top <= entry <= bottom   .
    // The "head" of the loop is a block outside the loop that has "entry" as a successor. We only support loops with a
    // single 'head' block. The meanings of these blocks are given in the definitions below. Also see the picture at
    // Compiler::optFindNaturalLoops().
    struct LoopDsc
    {
        BasicBlock* lpHead;  // HEAD of the loop (not part of the looping of the loop) -- has ENTRY as a successor.
        BasicBlock* lpFirst; // FIRST block (in bbNext order) reachable within this loop.  (May be part of a nested
                             // loop, but not the outer loop.)
        BasicBlock* lpTop;   // loop TOP (the back edge from lpBottom reaches here) (in most cases FIRST and TOP are the
                             // same)
        BasicBlock* lpEntry; // the ENTRY in the loop (in most cases TOP or BOTTOM)
        BasicBlock* lpBottom; // loop BOTTOM (from here we have a back edge to the TOP)
        BasicBlock* lpExit;   // if a single exit loop this is the EXIT (in most cases BOTTOM)

        LoopFlags lpFlags;

        uint8_t lpExitCnt; // number of exits from the loop

        LoopNum lpParent;  // The index of the most-nested loop that completely contains this one,
                           // or else NoLoopNum if no such loop exists.
        LoopNum lpChild;   // The index of a nested loop, or else NoLoopNum if no child exists.
                           // (Actually, an "immediately" nested loop --
                           // no other child of this loop is a parent of lpChild.)
        LoopNum lpSibling; // The index of another loop that is an immediate child of lpParent,
                           // or else NoLoopNum.  One can enumerate all the children of a loop
                           // by following "lpChild" then "lpSibling" links.

        /* The following values are set only for iterator loops, i.e. has non null lpIterTree*/

        union {
            int lpConstInit;    // initial constant value of iterator
                                // : Valid if LPFLG_CONST_INIT
            unsigned lpVarInit; // initial local var number to which we initialize the iterator
                                // : Valid if LPFLG_VAR_INIT
        };

        GenTreeLclVar* lpIterTree; // The "i = i <op> const" tree
        GenTreeOp*     lpTestTree; // pointer to the node containing the loop test

        unsigned   lpIterVar() const;   // iterator variable #
        int        lpIterConst() const; // the constant with which the iterator is incremented
        genTreeOps lpIterOper() const;  // the type of the operation on the iterator (ASG_ADD, ASG_SUB, etc.)
        INDEBUG(void VerifyIterator() const;)

        genTreeOps lpTestOper() const; // the type of the comparison between the iterator and the limit (GT_LE, GT_GE,
                                       // etc.)
        bool     lpIsReversed() const; // true if the iterator node is the second operand in the loop condition
        GenTree* lpIterator() const;   // the iterator node in the loop test
        GenTree* lpLimit() const;      // the limit node in the loop test

        // Limit constant value of iterator - loop condition is "i RELOP const"
        // : Valid if LPFLG_CONST_LIMIT
        int lpConstLimit() const;

        // The lclVar # in the loop condition ( "i RELOP lclVar" )
        // : Valid if LPFLG_VAR_LIMIT
        unsigned lpVarLimit() const;

        // Returns "true" iff "*this" contains the blk.
        bool lpContains(BasicBlock* blk) const
        {
            return lpFirst->bbNum <= blk->bbNum && blk->bbNum <= lpBottom->bbNum;
        }
        // Returns "true" iff "*this" (properly) contains the range [first, bottom] (allowing firsts
        // to be equal, but requiring bottoms to be different.)
        bool lpContains(BasicBlock* first, BasicBlock* bottom) const
        {
            return lpFirst->bbNum <= first->bbNum && bottom->bbNum < lpBottom->bbNum;
        }

        // Returns "true" iff "*this" (properly) contains "lp2" (allowing firsts to be equal, but requiring
        // bottoms to be different.)
        bool lpContains(const LoopDsc& lp2) const
        {
            return lpContains(lp2.lpFirst, lp2.lpBottom);
        }

        // Returns "true" iff "*this" is (properly) contained by the range [first, bottom]
        // (allowing firsts to be equal, but requiring bottoms to be different.)
        bool lpContainedBy(BasicBlock* first, BasicBlock* bottom) const
        {
            return first->bbNum <= lpFirst->bbNum && lpBottom->bbNum < bottom->bbNum;
        }

        // Returns "true" iff "*this" is (properly) contained by "lp2"
        // (allowing firsts to be equal, but requiring bottoms to be different.)
        bool lpContainedBy(const LoopDsc& lp2) const
        {
            return lpContains(lp2.lpFirst, lp2.lpBottom);
        }

        // Returns "true" iff "*this" is disjoint from the range [top, bottom].
        bool lpDisjoint(BasicBlock* first, BasicBlock* bottom) const
        {
            return bottom->bbNum < lpFirst->bbNum || lpBottom->bbNum < first->bbNum;
        }
        // Returns "true" iff "*this" is disjoint from "lp2".
        bool lpDisjoint(const LoopDsc& lp2) const
        {
            return lpDisjoint(lp2.lpFirst, lp2.lpBottom);
        }
        // Returns "true" iff the loop is well-formed (see code for defn).
        bool lpWellFormed() const
        {
            return lpFirst->bbNum <= lpTop->bbNum && lpTop->bbNum <= lpEntry->bbNum &&
                   lpEntry->bbNum <= lpBottom->bbNum &&
                   (lpHead->bbNum < lpTop->bbNum || lpHead->bbNum > lpBottom->bbNum);
        }

        // LoopBlocks: convenience method for enabling range-based `for` iteration over all the
        // blocks in a loop, e.g.:
        //    for (BasicBlock* const block : loop->LoopBlocks()) ...
        // Currently, the loop blocks are expected to be in linear, lexical, `bbNext` order
        // from `lpFirst` through `lpBottom`, inclusive. All blocks in this range are considered
        // to be part of the loop.
        //
        BasicBlockRangeList LoopBlocks() const
        {
            return BasicBlockRangeList(lpFirst, lpBottom);
        }
    };

protected:
    bool fgMightHaveLoop(); // returns true if there are any backedges

public:
    LoopDsc* optLoopTable = nullptr; // loop descriptor table
    unsigned optLoopCount = 0;       // number of tracked loops

#ifdef DEBUG
    unsigned char loopAlignCandidates = 0; // number of loops identified for alignment
    unsigned char loopsAligned        = 0; // number of loops actually aligned
#endif

    bool optRecordLoop(BasicBlock* head,
                       BasicBlock* first,
                       BasicBlock* top,
                       BasicBlock* entry,
                       BasicBlock* bottom,
                       BasicBlock* exit,
                       unsigned    exitCnt);

    unsigned optCallCount         = 0; // number of calls made in the method
    unsigned optIndirectCallCount = 0; // number of virtual, interface and indirect calls made in the method
    unsigned optNativeCallCount   = 0; // number of Pinvoke/Native calls made in the method
    unsigned optLoopsCloned       = 0; // number of loops cloned in the current method.

#ifdef DEBUG
    void optPrintLoopInfo(unsigned      loopNum,
                          BasicBlock*   lpHead,
                          BasicBlock*   lpFirst,
                          BasicBlock*   lpTop,
                          BasicBlock*   lpEntry,
                          BasicBlock*   lpBottom,
                          unsigned char lpExitCnt,
                          BasicBlock*   lpExit,
                          unsigned      parentLoop = BasicBlock::NOT_IN_LOOP) const;
    void optPrintLoopInfo(unsigned lnum) const;
    void optPrintLoopRecording(unsigned lnum) const;

    void optCheckPreds();
#endif

    void optSetBlockWeights();

    void optMarkLoopBlocks(BasicBlock* begBlk, BasicBlock* endBlk, bool excludeEndBlk);

    void optUnmarkLoopBlocks(BasicBlock* begBlk, BasicBlock* endBlk);

    void optUpdateLoopsBeforeRemoveBlock(BasicBlock* block, bool skipUnmarkLoop = false);

    bool optIsLoopTestEvalIntoTemp(Statement* testStmt, Statement** newTestStmt);
    unsigned optIsLoopIncrTree(GenTree* incr);
    GenTreeOp* optGetLoopTest(unsigned loopInd, GenTree* test, BasicBlock* from, BasicBlock* to, unsigned iterVar);
    bool optPopulateInitInfo(unsigned loopInd, GenTree* init, unsigned iterVar);
    GenTreeLclVar* optExtractInitTestIncr(
        BasicBlock* head, BasicBlock* bottom, BasicBlock* exit, GenTree** init, GenTree** test);

    void optFindNaturalLoops();

    void optIdentifyLoopsForAlignment();

    // Ensures that all the loops in the loop nest rooted at "loopInd" (an index into the loop table) are 'canonical' --
    // each loop has a unique "top."  Returns "true" iff the flowgraph has been modified.
    bool optCanonicalizeLoopNest(unsigned loopInd);

    // Ensures that the loop "loopInd" (an index into the loop table) is 'canonical' -- it has a unique "top,"
    // unshared with any other loop.  Returns "true" iff the flowgraph has been modified
    bool optCanonicalizeLoop(unsigned loopInd);

public:
    // Requires "l1" to be a valid loop table index, and not "BasicBlock::NOT_IN_LOOP".  Requires "l2" to be
    // a valid loop table index, or else "BasicBlock::NOT_IN_LOOP".  Returns true
    // iff "l2" is not NOT_IN_LOOP, and "l1" contains "l2".
    bool optLoopContains(unsigned l1, unsigned l2);

    // Updates the loop table by changing loop "loopInd", whose head is required
    // to be "from", to be "to".  Also performs this transformation for any
    // loop nested in "loopInd" that shares the same head as "loopInd".
    void optUpdateLoopHead(unsigned loopInd, BasicBlock* from, BasicBlock* to);

    void optRedirectBlock(BasicBlock* blk, BlockToBlockMap* redirectMap, const bool updatePreds = false);

    // Requires that "from" and "to" have the same "bbJumpKind" (perhaps because "to" is a clone
    // of "from".)  Copies the jump destination from "from" to "to".
    void optCopyBlkDest(BasicBlock* from, BasicBlock* to);

    // Returns true if 'block' is an entry block for any loop in 'optLoopTable'
    bool optIsLoopEntry(BasicBlock* block) const;

public:
    // The depth of the loop described by "lnum" (an index into the loop table.) (0 == top level)
    unsigned optLoopDepth(unsigned lnum)
    {
        unsigned par = optLoopTable[lnum].lpParent;
        if (par == BasicBlock::NOT_IN_LOOP)
        {
            return 0;
        }
        else
        {
            return 1 + optLoopDepth(par);
        }
    }

    bool optInvertWhileLoop(BasicBlock* block);

private:
    static bool optIterSmallOverflow(int iterAtExit, var_types incrType);
    static bool optIterSmallUnderflow(int iterAtExit, var_types decrType);

    bool optComputeLoopRep(int        constInit,
                           int        constLimit,
                           int        iterInc,
                           genTreeOps iterOper,
                           var_types  iterType,
                           genTreeOps testOper,
                           bool       unsignedTest,
                           bool       dupCond,
                           unsigned*  iterCount);

public:
    bool optIsVarAssigned(BasicBlock* beg, BasicBlock* end, GenTree* skip, unsigned lclNum);

    bool fgMorphNarrowTree(GenTree* tree, var_types srct, var_types dstt, ValueNumPair vnpNarrow, bool doit);

    /**************************************************************************
     *                       Optimization conditions
     *************************************************************************/

    bool optAvoidIntMult(void);

    bool cseCanSwapOrder(GenTree* tree1, GenTree* tree2);

// String to use for formatting CSE numbers. Note that this is the positive number, e.g., from GET_CSE_INDEX().
#define FMT_CSE "CSE%02u"

#if 0
    unsigned cseValueCount; // Count of CSE values, currently unused, see cseCanSwapOrder.
#endif

#ifdef DEBUG
    unsigned cseFirstLclNum = BAD_VAR_NUM; // The first local variable number that is a CSE
    unsigned cseCount       = 0;           // The total count of CSE temp locals introduced.

    // Returns true if the LclVar was introduced by the CSE phase of the compiler.
    bool lclNumIsTrueCSE(unsigned lclNum) const
    {
        return ((cseCount > 0) && (lclNum >= cseFirstLclNum) && (lclNum < cseFirstLclNum + cseCount));
    }

    unsigned apAssertionCount = 0;
#endif

public:
#define OMF_HAS_NEWARRAY 0x00000001         // Method contains 'new' of an array
#define OMF_HAS_NEWOBJ 0x00000002           // Method contains 'new' of an object type.
#define OMF_HAS_ARRAYREF 0x00000004         // Method contains array element loads or stores.
#define OMF_HAS_NULLCHECK 0x00000008        // Method contains null check.
#define OMF_HAS_FATPOINTER 0x00000010       // Method contains call, that needs fat pointer transformation.
#define OMF_HAS_OBJSTACKALLOC 0x00000020    // Method contains an object allocated on the stack.
#define OMF_HAS_GUARDEDDEVIRT 0x00000040    // Method contains guarded devirtualization candidate
#define OMF_HAS_EXPRUNTIMELOOKUP 0x00000080 // Method contains a runtime lookup to an expandable dictionary.
#define OMF_HAS_PATCHPOINT 0x00000100       // Method contains patchpoints
#define OMF_NEEDS_GCPOLLS 0x00000200        // Method needs GC polls
#ifdef DEBUG
#define OMF_HAS_FROZEN_STRING 0x00000400 // Method has a frozen string (REF constant int), currently only on CoreRT.
#endif

    bool doesMethodHaveFatPointer()
    {
        return (optMethodFlags & OMF_HAS_FATPOINTER) != 0;
    }

    void setMethodHasFatPointer()
    {
        optMethodFlags |= OMF_HAS_FATPOINTER;
    }

    void clearMethodHasFatPointer()
    {
        optMethodFlags &= ~OMF_HAS_FATPOINTER;
    }

#ifdef DEBUG
    bool doesMethodHaveFrozenString() const
    {
        return (optMethodFlags & OMF_HAS_FROZEN_STRING) != 0;
    }

    void setMethodHasFrozenString()
    {
        optMethodFlags |= OMF_HAS_FROZEN_STRING;
    }
#endif

    bool doesMethodHaveGuardedDevirtualization() const
    {
        return (optMethodFlags & OMF_HAS_GUARDEDDEVIRT) != 0;
    }

    void setMethodHasGuardedDevirtualization()
    {
        optMethodFlags |= OMF_HAS_GUARDEDDEVIRT;
    }

    void clearMethodHasGuardedDevirtualization()
    {
        optMethodFlags &= ~OMF_HAS_GUARDEDDEVIRT;
    }

    bool doesMethodHaveExpRuntimeLookup()
    {
        return (optMethodFlags & OMF_HAS_EXPRUNTIMELOOKUP) != 0;
    }

    void setMethodHasExpRuntimeLookup()
    {
        optMethodFlags |= OMF_HAS_EXPRUNTIMELOOKUP;
    }

    void clearMethodHasExpRuntimeLookup()
    {
        optMethodFlags &= ~OMF_HAS_EXPRUNTIMELOOKUP;
    }

    bool doesMethodHavePatchpoints()
    {
        return (optMethodFlags & OMF_HAS_PATCHPOINT) != 0;
    }

    void setMethodHasPatchpoint()
    {
        optMethodFlags |= OMF_HAS_PATCHPOINT;
    }

    unsigned optMethodFlags = 0;

    bool doesMethodHaveNoReturnCalls()
    {
        return optNoReturnCallCount > 0;
    }

    void setMethodHasNoReturnCalls()
    {
        optNoReturnCallCount++;
    }

    unsigned optNoReturnCallCount = 0;

#if ASSERTION_PROP
    /**************************************************************************
     *               Value/Assertion propagation
     *************************************************************************/
protected:
public:
#if LOCAL_ASSERTION_PROP
    struct MorphAssertion;

    static constexpr unsigned morphAssertionMaxCount = 64;
    unsigned                  morphAssertionCount;
    MorphAssertion*           morphAssertionTable; // table that holds info about local assignments
    JitExpandArray<BitVec>*   morphAssertionDep;   // table that holds dependent assertions (assertions
                                                   // using the value of a local var) for each local var

public:
    void morphAssertionInit();
    void morphAssertionDone();
    void morphAssertionGenerate(GenTree* tree);
    GenTree* morphAssertionPropagate(GenTree* tree);
    bool morphAssertionIsNotNull(GenTreeLclVar* lclVar);
    bool morphAssertionIsTypeRange(GenTreeLclVar* lclVar, var_types type);
    void morphAssertionSetCount(unsigned count);
    unsigned morphAssertionTableSize(unsigned count);
    void morphAssertionGetTable(MorphAssertion* table, unsigned count);
    void morphAssertionSetTable(const MorphAssertion* table, unsigned count);
    void morphAssertionMerge(unsigned              elseAssertionCount,
                             const MorphAssertion* elseAssertionTable DEBUGARG(GenTreeQmark* qmark));
    void morphAssertionKill(unsigned lclNum DEBUGARG(GenTreeOp* asg));

private:
    BitVec& morphAssertionGetDependent(unsigned lclNum);
    void morphAssertionGenerateNotNull(GenTree* op1);
    void morphAssertionGenerateEqual(GenTreeLclVar* store, GenTree* value);
    void morphAssertionAdd(MorphAssertion& assertion);
    const MorphAssertion& morphAssertionGet(unsigned index);
    void morphAssertionRemove(unsigned index);
    void morphAssertionKillSingle(unsigned lclNum DEBUGARG(GenTreeOp* asg));
    const MorphAssertion* morphAssertionFindRange(unsigned lclNum);

    GenTree* morphAssertionPropagateLclVar(GenTreeLclVar* lclVar);
    GenTree* morphAssertionPropagateLclFld(GenTreeLclFld* lclFld);
    GenTree* morphAssertionPropagateIndir(GenTreeIndir* indir);
    GenTree* morphAssertionPropagateCast(GenTreeCast* cast);
    GenTree* morphAssertionPropagateCall(GenTreeCall* call);
    GenTree* morphAssertionPropagateRelOp(GenTreeOp* relop);
    GenTree* morphAssertionPropagateLclVarConst(const MorphAssertion& assertion, GenTreeLclVar* lclVar);
    GenTree* morphAssertionPropagateLclVarCopy(const MorphAssertion& assertion, GenTreeLclVar* lclVar);

#ifdef DEBUG
    unsigned morphAssertionId;
    GenTree* morphAssertionCurrentTree;
    void morphAssertionTrace(const MorphAssertion& assertion, GenTree* node, const char* message);
#endif
#endif

public:
    void optAddCopies();
#endif // ASSERTION_PROP

    /*
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XX                                                                           XX
    XX                           RegAlloc                                        XX
    XX                                                                           XX
    XX  Does the register allocation and puts the remaining lclVars on the stack XX
    XX                                                                           XX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    */

public:
#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
#if defined(TARGET_AMD64)
    static bool varTypeNeedsPartialCalleeSave(var_types type)
    {
        assert(type != TYP_STRUCT);
        return (type == TYP_SIMD32);
    }
#elif defined(TARGET_ARM64)
    static bool varTypeNeedsPartialCalleeSave(var_types type)
    {
        assert(type != TYP_STRUCT);
        // ARM64 ABI FP Callee save registers only require Callee to save lower 8 Bytes
        // For SIMD types longer than 8 bytes Caller is responsible for saving and restoring Upper bytes.
        return ((type == TYP_SIMD16) || (type == TYP_SIMD12));
    }
#else // !defined(TARGET_AMD64) && !defined(TARGET_ARM64)
#error("Unknown target architecture for FEATURE_SIMD")
#endif // !defined(TARGET_AMD64) && !defined(TARGET_ARM64)
#endif // FEATURE_PARTIAL_SIMD_CALLEE_SAVE

protected:
    bool rpMustCreateEBPFrame();

private:
    bool lvaIsX86VarargsStackParam(unsigned lclNum);

    /*
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XX                                                                           XX
    XX                           EEInterface                                     XX
    XX                                                                           XX
    XX   Get to the class and method info from the Execution Engine given        XX
    XX   tokens for the class and method                                         XX
    XX                                                                           XX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    */

public:
    // Get handles

    void eeGetCallInfo(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                       CORINFO_RESOLVED_TOKEN* pConstrainedToken,
                       CORINFO_CALLINFO_FLAGS  flags,
                       CORINFO_CALL_INFO*      pResult);

    void eeGetFieldInfo(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                        CORINFO_ACCESS_FLAGS    flags,
                        CORINFO_FIELD_INFO*     pResult);

#if defined(DEBUG) || defined(FEATURE_JIT_METHOD_PERF) || defined(FEATURE_SIMD) || defined(TRACK_LSRA_STATS)

    bool IsSuperPMIException(unsigned code)
    {
        // Copied from NDP\clr\src\ToolBox\SuperPMI\SuperPMI-Shared\ErrorHandling.h

        const unsigned EXCEPTIONCODE_DebugBreakorAV = 0xe0421000;
        const unsigned EXCEPTIONCODE_MC             = 0xe0422000;
        const unsigned EXCEPTIONCODE_LWM            = 0xe0423000;
        const unsigned EXCEPTIONCODE_SASM           = 0xe0424000;
        const unsigned EXCEPTIONCODE_SSYM           = 0xe0425000;
        const unsigned EXCEPTIONCODE_CALLUTILS      = 0xe0426000;
        const unsigned EXCEPTIONCODE_TYPEUTILS      = 0xe0427000;
        const unsigned EXCEPTIONCODE_ASSERT         = 0xe0440000;

        switch (code)
        {
            case EXCEPTIONCODE_DebugBreakorAV:
            case EXCEPTIONCODE_MC:
            case EXCEPTIONCODE_LWM:
            case EXCEPTIONCODE_SASM:
            case EXCEPTIONCODE_SSYM:
            case EXCEPTIONCODE_CALLUTILS:
            case EXCEPTIONCODE_TYPEUTILS:
            case EXCEPTIONCODE_ASSERT:
                return true;
            default:
                return false;
        }
    }

    const char* eeGetMethodName(CORINFO_METHOD_HANDLE hnd, const char** className);
    const char* eeGetMethodFullName(CORINFO_METHOD_HANDLE hnd);
    unsigned compMethodHash(CORINFO_METHOD_HANDLE methodHandle);

    bool eeIsNativeMethod(CORINFO_METHOD_HANDLE method);
    CORINFO_METHOD_HANDLE eeGetMethodHandleForNative(CORINFO_METHOD_HANDLE method);
#endif

    var_types eeGetArgType(CORINFO_ARG_LIST_HANDLE list, CORINFO_SIG_INFO* sig);
    var_types eeGetArgType(CORINFO_ARG_LIST_HANDLE list, CORINFO_SIG_INFO* sig, bool* isPinned);
    CORINFO_CLASS_HANDLE eeGetArgClass(CORINFO_SIG_INFO* sig, CORINFO_ARG_LIST_HANDLE list);
    CORINFO_CLASS_HANDLE eeGetClassFromContext(CORINFO_CONTEXT_HANDLE context);

    // VOM info, method sigs

    void eeGetSig(unsigned               sigTok,
                  CORINFO_MODULE_HANDLE  scope,
                  CORINFO_CONTEXT_HANDLE context,
                  CORINFO_SIG_INFO*      retSig);

    void eeGetCallSiteSig(unsigned               sigTok,
                          CORINFO_MODULE_HANDLE  scope,
                          CORINFO_CONTEXT_HANDLE context,
                          CORINFO_SIG_INFO*      retSig);

    void eeGetMethodSig(CORINFO_METHOD_HANDLE methHnd, CORINFO_SIG_INFO* retSig, CORINFO_CLASS_HANDLE owner = nullptr);

    // Method entry-points, instrs

    const CORINFO_EE_INFO* eeInfo;

    const CORINFO_EE_INFO* eeGetEEInfo()
    {
        return eeInfo;
    }

    // Gets the offset of a SDArray's first element
    static unsigned eeGetArrayDataOffset(var_types type);
    // Gets the offset of a MDArray's first element
    static unsigned eeGetMDArrayDataOffset(var_types type, unsigned rank);

    // Returns the page size for the target machine as reported by the EE.
    target_size_t eeGetPageSize()
    {
        return (target_size_t)eeGetEEInfo()->osPageSize;
    }

    //------------------------------------------------------------------------
    // VirtualStubParam: virtual stub dispatch extra parameter (slot address).
    //
    // It represents Abi and target specific registers for the parameter.
    //
    class VirtualStubParamInfo
    {
        const regNumber regNum;

    public:
        VirtualStubParamInfo(bool isCoreRTABI)
#if defined(TARGET_X86)
            : regNum(REG_EAX)
#elif defined(TARGET_AMD64)
            : regNum(isCoreRTABI ? REG_R10 : REG_R11)
#elif defined(TARGET_ARM)
            : regNum(isCoreRTABI ? REG_R12 : REG_R4)
#elif defined(TARGET_ARM64)
            : regNum(REG_R11)
#else
#error Unsupported or unset target architecture
#endif
        {
        }

        regNumber GetRegNum() const
        {
            return regNum;
        }
    };

    bool IsTargetAbi(CORINFO_RUNTIME_ABI abi)
    {
        return eeGetEEInfo()->targetAbi == abi;
    }

    bool generateCFIUnwindCodes()
    {
#if defined(TARGET_UNIX)
        return IsTargetAbi(CORINFO_CORERT_ABI);
#else
        return false;
#endif
    }

    // Debugging support - Local var info

    void eeGetVars();
    void eeGetVars(ICorDebugInfo::ILVarInfo* varInfoTable, uint32_t varInfoCount, bool extendOthers);

    // ICorJitInfo wrappers

    void eeReserveUnwindInfo(bool isFunclet, bool isColdCode, ULONG unwindSize);

    void eeAllocUnwindInfo(BYTE*          pHotCode,
                           BYTE*          pColdCode,
                           ULONG          startOffset,
                           ULONG          endOffset,
                           ULONG          unwindSize,
                           BYTE*          pUnwindBlock,
                           CorJitFuncKind funcKind);

    void eeSetEHcount(unsigned cEH);

    void eeSetEHinfo(unsigned EHnumber, const CORINFO_EH_CLAUSE* clause);

#ifdef TARGET_AMD64
    bool eeIsRIPRelativeAddress(void* addr);
#endif
#ifdef TARGET_ARM
    bool eeIsThumbBranch24TargetAddress(void* target);
#endif

    // ICorStaticInfo wrapper functions

    bool eeTryResolveToken(CORINFO_RESOLVED_TOKEN* resolvedToken);

#if defined(UNIX_AMD64_ABI)
#ifdef DEBUG
    static void dumpSystemVClassificationType(SystemVClassificationType ct);
#endif // DEBUG

#endif // UNIX_AMD64_ABI

    template <typename ParamType>
    bool eeRunWithErrorTrap(void (*function)(ParamType*), ParamType* param)
    {
        return eeRunWithErrorTrapImp(reinterpret_cast<void (*)(void*)>(function), reinterpret_cast<void*>(param));
    }

    bool eeRunWithErrorTrapImp(void (*function)(void*), void* param);

    template <typename ParamType>
    bool eeRunWithSPMIErrorTrap(void (*function)(ParamType*), ParamType* param)
    {
        return eeRunWithSPMIErrorTrapImp(reinterpret_cast<void (*)(void*)>(function), reinterpret_cast<void*>(param));
    }

    bool eeRunWithSPMIErrorTrapImp(void (*function)(void*), void* param);

    // Utility functions

    const char* eeGetFieldName(CORINFO_FIELD_HANDLE fieldHnd, const char** classNamePtr = nullptr);

    INDEBUG(const WCHAR* eeGetCPString(void* stringHandle);)
    const char* eeGetClassName(CORINFO_CLASS_HANDLE clsHnd);
    const char* eeGetSimpleClassName(CORINFO_CLASS_HANDLE clsHnd);

    static CORINFO_METHOD_HANDLE eeFindHelper(unsigned helper);
    static CorInfoHelpFunc eeGetHelperNum(CORINFO_METHOD_HANDLE method);

    static bool IsSharedStaticHelper(GenTree* tree);
    static bool IsCallGCSafePoint(GenTreeCall* call);

    /*
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XX                                                                           XX
    XX                           CodeGenerator                                   XX
    XX                                                                           XX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    */

    void codeGenInit();

public:
    CodeGenInterface* codeGen = nullptr;

    // Managed RetVal - A side hash table meant to record the mapping from a
    // GT_CALL node to its IL offset.  This info is used to emit sequence points
    // that can be used by debugger to determine the native offset at which the
    // managed RetVal will be available.
    //
    // In fact we can store IL offset in a GT_CALL node.  This was ruled out in
    // favor of a side table for two reasons: 1) We need IL offset for only those
    // GT_CALL nodes (created during importation) that correspond to an IL call and
    // whose return type is other than TYP_VOID. 2) GT_CALL node is a frequently used
    // structure and IL offset is needed only when generating debuggable code. Therefore
    // it is desirable to avoid memory size penalty in retail scenarios.
    typedef JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, IL_OFFSETX> CallSiteILOffsetTable;
    CallSiteILOffsetTable* genCallSite2ILOffsetMap = nullptr;

    BasicBlock* genReturnBB = nullptr; // jumped to when not optimizing for speed.

    // The following properties are part of CodeGenContext.  Getters are provided here for
    // convenience and backward compatibility, but the properties can only be set by invoking
    // the setter on CodeGenContext directly.

    emitter* GetEmitter() const
    {
        return codeGen->GetEmitter();
    }

#if DOUBLE_ALIGN
    bool shouldDoubleAlign(unsigned             refCntStk,
                           unsigned             refCntReg,
                           BasicBlock::weight_t refCntWtdReg,
                           unsigned             refCntStkParam,
                           BasicBlock::weight_t refCntWtdStkDbl);
#endif // DOUBLE_ALIGN

// Things that MAY belong either in CodeGen or CodeGenContext

#if defined(FEATURE_EH_FUNCLETS)
    FuncInfoDsc*   compFuncInfos;
    unsigned short compCurrFuncIdx;
    unsigned short compFuncInfoCount;

    unsigned short compFuncCount()
    {
        assert(fgFuncletsCreated);
        return compFuncInfoCount;
    }

#else // !FEATURE_EH_FUNCLETS

    // This is a no-op when there are no funclets!
    void genUpdateCurrentFunclet(BasicBlock* block)
    {
        return;
    }

    FuncInfoDsc compFuncInfoRoot;

    static const unsigned compCurrFuncIdx = 0;

    unsigned short compFuncCount()
    {
        return 1;
    }

#endif // !FEATURE_EH_FUNCLETS

    FuncInfoDsc* funCurrentFunc();
    void funSetCurrentFunc(unsigned funcIdx);
    FuncInfoDsc* funGetFunc(unsigned funcIdx);
    unsigned int funGetFuncIdx(BasicBlock* block);

    // LIVENESS

    // Gets a register mask that represent the kill set for a helper call since
    // not all JIT Helper calls follow the standard ABI on the target architecture.
    static regMaskTP compHelperCallKillSet(CorInfoHelpFunc helper);

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                           UnwindInfo                                      XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#if !defined(__GNUC__)
#pragma region Unwind information
#endif

public:
    //
    // Infrastructure functions: start/stop/reserve/emit.
    //

    void unwindBegProlog();
    void unwindEndProlog();
    void unwindBegEpilog();
    void unwindEndEpilog();
    void unwindReserve();
    void unwindEmit(void* pHotCode, void* pColdCode);

    //
    // Specific unwind information functions: called by code generation to indicate a particular
    // prolog or epilog unwindable instruction has been generated.
    //

    void unwindPush(regNumber reg);
    void unwindAllocStack(unsigned size);
    void unwindSetFrameReg(regNumber reg, unsigned offset);
    void unwindSaveReg(regNumber reg, unsigned offset);

#if defined(TARGET_ARM)
    void unwindPushMaskInt(regMaskTP mask);
    void unwindPushMaskFloat(regMaskTP mask);
    void unwindPopMaskInt(regMaskTP mask);
    void unwindPopMaskFloat(regMaskTP mask);
    void unwindBranch16();                    // The epilog terminates with a 16-bit branch (e.g., "bx lr")
    void unwindNop(unsigned codeSizeInBytes); // Generate unwind NOP code. 'codeSizeInBytes' is 2 or 4 bytes. Only
                                              // called via unwindPadding().
    void unwindPadding(); // Generate a sequence of unwind NOP codes representing instructions between the last
                          // instruction and the current location.
#endif                    // TARGET_ARM

#if defined(TARGET_ARM64)
    void unwindNop();
    void unwindPadding(); // Generate a sequence of unwind NOP codes representing instructions between the last
                          // instruction and the current location.
    void unwindSaveReg(regNumber reg, int offset);                                // str reg, [sp, #offset]
    void unwindSaveRegPreindexed(regNumber reg, int offset);                      // str reg, [sp, #offset]!
    void unwindSaveRegPair(regNumber reg1, regNumber reg2, int offset);           // stp reg1, reg2, [sp, #offset]
    void unwindSaveRegPairPreindexed(regNumber reg1, regNumber reg2, int offset); // stp reg1, reg2, [sp, #offset]!
    void unwindSaveNext();                                                        // unwind code: save_next
    void unwindReturn(regNumber reg);                                             // ret lr
#endif                                                                            // defined(TARGET_ARM64)

    //
    // Private "helper" functions for the unwind implementation.
    //

private:
#if defined(FEATURE_EH_FUNCLETS)
    void unwindGetFuncLocations(FuncInfoDsc*             func,
                                bool                     getHotSectionData,
                                /* OUT */ emitLocation** ppStartLoc,
                                /* OUT */ emitLocation** ppEndLoc);
#endif // FEATURE_EH_FUNCLETS

    void unwindReserveFunc(FuncInfoDsc* func);
    void unwindEmitFunc(FuncInfoDsc* func, void* pHotCode, void* pColdCode);

#if defined(TARGET_AMD64) || (defined(TARGET_X86) && defined(FEATURE_EH_FUNCLETS))

    void unwindReserveFuncHelper(FuncInfoDsc* func, bool isHotCode);
    void unwindEmitFuncHelper(FuncInfoDsc* func, void* pHotCode, void* pColdCode, bool isHotCode);

#endif // TARGET_AMD64 || (TARGET_X86 && FEATURE_EH_FUNCLETS)

    UNATIVE_OFFSET unwindGetCurrentOffset(FuncInfoDsc* func);

#if defined(TARGET_AMD64)

    void unwindBegPrologWindows();
    void unwindPushWindows(regNumber reg);
    void unwindAllocStackWindows(unsigned size);
    void unwindSetFrameRegWindows(regNumber reg, unsigned offset);
    void unwindSaveRegWindows(regNumber reg, unsigned offset);

#ifdef UNIX_AMD64_ABI
    void unwindSaveRegCFI(regNumber reg, unsigned offset);
#endif // UNIX_AMD64_ABI
#elif defined(TARGET_ARM)

    void unwindPushPopMaskInt(regMaskTP mask, bool useOpsize16);
    void unwindPushPopMaskFloat(regMaskTP mask);

#endif // TARGET_ARM

#if defined(TARGET_UNIX)
    short mapRegNumToDwarfReg(regNumber reg);
    void createCfiCode(FuncInfoDsc* func, UNATIVE_OFFSET codeOffset, UCHAR opcode, short dwarfReg, INT offset = 0);
    void unwindPushPopCFI(regNumber reg);
    void unwindBegPrologCFI();
    void unwindPushPopMaskCFI(regMaskTP regMask, bool isFloat);
    void unwindAllocStackCFI(unsigned size);
    void unwindSetFrameRegCFI(regNumber reg, unsigned offset);
    void unwindEmitFuncCFI(FuncInfoDsc* func, void* pHotCode, void* pColdCode);
#ifdef DEBUG
    void DumpCfiInfo(bool                  isHotCode,
                     UNATIVE_OFFSET        startOffset,
                     UNATIVE_OFFSET        endOffset,
                     DWORD                 cfiCodeBytes,
                     const CFI_CODE* const pCfiCode);
#endif

#endif // TARGET_UNIX

#if !defined(__GNUC__)
#pragma endregion // Note: region is NOT under !defined(__GNUC__)
#endif

    /*
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XX                                                                           XX
    XX                               SIMD                                        XX
    XX                                                                           XX
    XX   Info about SIMD types, methods and the SIMD assembly (i.e. the assembly XX
    XX   that contains the distinguished, well-known SIMD type definitions).     XX
    XX                                                                           XX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    */

    bool IsBaselineSimdIsaSupported()
    {
#ifdef FEATURE_SIMD
#if defined(TARGET_XARCH)
        CORINFO_InstructionSet minimumIsa = InstructionSet_SSE2;
#elif defined(TARGET_ARM64)
        CORINFO_InstructionSet minimumIsa = InstructionSet_AdvSimd;
#else
#error Unsupported platform
#endif // !TARGET_XARCH && !TARGET_ARM64

        return compOpportunisticallyDependsOn(minimumIsa) && JitConfig.EnableHWIntrinsic();
#else
        return false;
#endif
    }

#ifdef FEATURE_SIMD
    // Should we support SIMD intrinsics?
    bool featureSIMD;

    // Should we recognize SIMD types?
    // We always do this on ARM64 to support HVA types.
    bool supportSIMDTypes()
    {
#ifdef TARGET_ARM64
        return true;
#else
        return featureSIMD;
#endif
    }

    void lvaRecordSimdIntrinsicUse(GenTree* op);
    void lvaRecordSimdIntrinsicUse(GenTreeLclVar* lclVar);
    void lvaRecordSimdIntrinsicUse(unsigned lclNum);
    void lvaRecordSimdIntrinsicDef(GenTreeLclVar* lclVar, GenTreeHWIntrinsic* src);
    void lvaRecordSimdIntrinsicDef(unsigned lclNum, GenTreeHWIntrinsic* src);

    // Get the type for the hardware SIMD vector.
    // This is the maximum SIMD type supported for this target.
    var_types GetVectorTSimdType();

    // Get preferred alignment of SIMD type.
    int lvaGetSimdTypedLocalPreferredAlignment(LclVarDsc* lcl);
#endif // FEATURE_SIMD

private:
#ifdef DEBUG
    // Answer the question: Is a particular ISA supported?
    // Use this api when asking the question so that future
    // ISA questions can be asked correctly or when asserting
    // support/nonsupport for an instruction set
    bool compIsaSupportedDebugOnly(CORINFO_InstructionSet isa) const
    {
#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
        return (opts.compSupportsISA & (1ULL << isa)) != 0;
#else
        return false;
#endif
    }
#endif // DEBUG

    bool notifyInstructionSetUsage(CORINFO_InstructionSet isa, bool supported) const;

    // Answer the question: Is a particular ISA allowed to be used implicitly by optimizations?
    // The result of this api call will exactly match the target machine
    // on which the function is executed (except for CoreLib, where there are special rules)
    bool compExactlyDependsOn(CORINFO_InstructionSet isa) const
    {
#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
        uint64_t isaBit = (1ULL << isa);
        if ((opts.compSupportsISAReported & isaBit) == 0)
        {
            if (notifyInstructionSetUsage(isa, (opts.compSupportsISA & isaBit) != 0))
                ((Compiler*)this)->opts.compSupportsISAExactly |= isaBit;
            ((Compiler*)this)->opts.compSupportsISAReported |= isaBit;
        }
        return (opts.compSupportsISAExactly & isaBit) != 0;
#else
        return false;
#endif
    }

    // Answer the question: Is a particular ISA allowed to be used implicitly by optimizations?
    // The result of this api call will match the target machine if the result is true
    // If the result is false, then the target machine may have support for the instruction
    bool compOpportunisticallyDependsOn(CORINFO_InstructionSet isa) const
    {
        if ((opts.compSupportsISA & (1ULL << isa)) != 0)
        {
            return compExactlyDependsOn(isa);
        }
        else
        {
            return false;
        }
    }

    // Answer the question: Is a particular ISA supported for explicit hardware intrinsics?
    bool compHWIntrinsicDependsOn(CORINFO_InstructionSet isa) const
    {
        // Report intent to use the ISA to the EE
        compExactlyDependsOn(isa);
        return ((opts.compSupportsISA & (1ULL << isa)) != 0);
    }

    bool canUseVexEncoding() const
    {
#ifdef TARGET_XARCH
        return compOpportunisticallyDependsOn(InstructionSet_AVX);
#else
        return false;
#endif
    }

    /*
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XX                                                                           XX
    XX                           Compiler                                        XX
    XX                                                                           XX
    XX   Generic info about the compilation and the method being compiled.       XX
    XX   It is responsible for driving the other phases.                         XX
    XX   It is also responsible for all the memory management.                   XX
    XX                                                                           XX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    */

public:
    bool compDoAggressiveInlining = false; // If true, mark every method as CORINFO_FLG_FORCEINLINE
    bool compJmpOpUsed            = false; // Does the method do a JMP
#ifndef TARGET_64BIT
    bool compLongUsed = false; // Does the method use TYP_LONG
#endif
    bool compFloatingPointUsed  = false; // Does the method use TYP_FLOAT or TYP_DOUBLE
    bool compTailCallUsed       = false; // Does the method do a tailcall
    bool compLocallocUsed       = false; // Does the method use localloc.
    bool compQmarkUsed          = false; // Does the method use GT_QMARK
    bool compHasBackwardJump    = false; // Does the method (or some inlinee) have a lexically backwards jump?
    bool compSuppressedZeroInit = false; // There are vars with lvSuppressedZeroInit set

#ifdef DEBUG
    bool     compQmarkRationalized   = false; // Is it allowed to use a GT_QMARK node.
    bool     compSwitchedToOptimized = false; // Codegen initially was Tier0 but jit switched to FullOpts
    bool     compSwitchedToMinOpts   = false; // Codegen initially was Tier1/FullOpts but jit switched to MinOpts
    bool     bRangeAllowStress;
    bool     compCodeGenDone = false;
    unsigned fgStmtLinksTraversed;             // # of links traversed while doing debug checks
    bool     fgNormalizeEHDone        = false; // Has the flowgraph EH normalization phase been done?
    bool     fgNoStructParamPromotion = false; // Set to true to turn off struct promotion of this method's params.
#endif

    bool fgNoStructPromotion       = false; // Set to true to turn off struct promotion for this method.
    bool optLoopsMarked            = false;
    bool csePhase                  = false; // True when we are executing the CSE phase
    bool compRationalIRForm        = false;
    bool compUsesThrowHelper       = false; // There is a call to a THROW_HELPER for the compiled method.
    bool compNeedsGSSecurityCookie = false; // There is an unsafe buffer (or localloc) on the stack.
                                            // Insert cookie on frame and code to check the cookie, like VC++ -GS.
    bool compGSReorderStackLayout = false;  // There is an unsafe buffer on the stack, reorder locals and make local
    // copies of susceptible parameters to avoid buffer overrun attacks through locals/params
    bool getNeedsGSSecurityCookie() const
    {
        return compNeedsGSSecurityCookie;
    }
    void setNeedsGSSecurityCookie()
    {
        compNeedsGSSecurityCookie = true;
    }

    CompilerOptions opts;

    static AssemblyNamesList2* s_pAltJitExcludeAssembliesList;

#ifdef DEBUG
    static AssemblyNamesList2* s_pJitDisasmIncludeAssembliesList;
    static MethodSet*          s_pJitMethodSet;

// silence warning of cast to greater size. It is easier to silence than construct code the compiler is happy with, and
// it is safe in this case
#pragma warning(push)
#pragma warning(disable : 4312)
    template <typename T>
    T dspPtr(T p)
    {
        if (p && opts.disDiffable)
        {
            return T(0xD1FFAB1E);
        }

        return p;
    }

    template <typename T>
    T dspOffset(T o)
    {
        if (o && opts.dspDiffable)
        {
            return T(0xD1FFAB1E);
        }

        return o;
    }
#pragma warning(pop)

    static int dspTreeID(GenTree* tree)
    {
        return tree->gtTreeID;
    }

    const char* pgoSourceToString(ICorJitInfo::PgoSource p);
    const char* devirtualizationDetailToString(CORINFO_DEVIRTUALIZATION_DETAIL detail);

#endif // DEBUG

// clang-format off
#define STRESS_MODES                                                                            \
                                                                                                \
        STRESS_MODE(NONE)                                                                       \
                                                                                                \
        /* "Variations" stress areas which we try to mix up with each other. */                 \
        /* These should not be exhaustively used as they might */                               \
        /* hide/trivialize other areas */                                                       \
                                                                                                \
        STRESS_MODE(REGS)                                                                       \
        STRESS_MODE(DBL_ALN)                                                                    \
        STRESS_MODE(UNROLL_LOOPS)                                                               \
        STRESS_MODE(MAKE_CSE)                                                                   \
        STRESS_MODE(LEGACY_INLINE)                                                              \
        STRESS_MODE(CLONE_EXPR)                                                                 \
        STRESS_MODE(FOLD)                                                                       \
        STRESS_MODE(MERGED_RETURNS)                                                             \
        STRESS_MODE(BB_PROFILE)                                                                 \
        STRESS_MODE(OPT_BOOLS_GC)                                                               \
        STRESS_MODE(REMORPH_TREES)                                                              \
        STRESS_MODE(DO_WHILE_LOOPS)                                                             \
        STRESS_MODE(MIN_OPTS)                                                                   \
        STRESS_MODE(REVERSE_FLAG)     /* Will set GTF_REVERSE_OPS whenever we can */            \
        STRESS_MODE(REVERSE_COMMA)    /* Will reverse commas created  with gtNewCommaNode */    \
        STRESS_MODE(TAILCALL)         /* Will make the call as a tailcall whenever legal */     \
        STRESS_MODE(CATCH_ARG)        /* Will spill catch arg */                                \
        STRESS_MODE(UNSAFE_BUFFER_CHECKS)                                                       \
        STRESS_MODE(NULL_OBJECT_CHECK)                                                          \
        STRESS_MODE(PINVOKE_RESTORE_ESP)                                                        \
        STRESS_MODE(RANDOM_INLINE)                                                              \
        STRESS_MODE(SWITCH_CMP_BR_EXPANSION)                                                    \
        STRESS_MODE(GENERIC_VARN)                                                               \
        STRESS_MODE(PROFILER_CALLBACKS) /* Will generate profiler hooks for ELT callbacks */    \
        STRESS_MODE(BYREF_PROMOTION) /* Change undoPromotion decisions for byrefs */            \
        STRESS_MODE(PROMOTE_FEWER_STRUCTS)/* Don't promote some structs that can be promoted */ \
                                                                                                \
        /* After COUNT_VARN, stress level 2 does all of these all the time */                   \
                                                                                                \
        STRESS_MODE(COUNT_VARN)                                                                 \
                                                                                                \
        /* "Check" stress areas that can be exhaustively used if we */                          \
        /*  dont care about performance at all */                                               \
                                                                                                \
        STRESS_MODE(FORCE_INLINE) /* Treat every method as AggressiveInlining */                \
        STRESS_MODE(CHK_FLOW_UPDATE)                                                            \
        STRESS_MODE(EMITTER)                                                                    \
        STRESS_MODE(FLATFP)                                                                     \
        STRESS_MODE(GENERIC_CHECK)                                                              \
        STRESS_MODE(COUNT)

    enum                compStressArea
    {
#define STRESS_MODE(mode) STRESS_##mode,
        STRESS_MODES
#undef STRESS_MODE
    };
// clang-format on

#ifdef DEBUG
    static const LPCWSTR s_compStressModeNames[STRESS_COUNT + 1];
    bool                 compActiveStressModes[STRESS_COUNT]{};
#endif // DEBUG

#define MAX_STRESS_WEIGHT 100

    bool compStressCompile(compStressArea stressArea, unsigned weightPercentage);
    bool compStressCompileHelper(compStressArea stressArea, unsigned weightPercentage);

#ifdef DEBUG

    bool compInlineStress()
    {
        return compStressCompile(STRESS_LEGACY_INLINE, 50);
    }

    bool compRandomInlineStress()
    {
        return compStressCompile(STRESS_RANDOM_INLINE, 50);
    }

    bool compPromoteFewerStructs(unsigned lclNum);

#endif // DEBUG

#ifdef DEBUG
    bool compTailCallStress()
    {
        // Do not stress tailcalls in IL stubs as the runtime creates several IL
        // stubs to implement the tailcall mechanism, which would then
        // recursively create more IL stubs.
        return !opts.jitFlags->IsSet(JitFlags::JIT_FLAG_IL_STUB) &&
               (JitConfig.TailcallStress() != 0 || compStressCompile(STRESS_TAILCALL, 5));
    }

    const char* compGetTieringName(bool wantShortName = false) const;
    const char* compGetStressMessage() const;
#endif

    codeOptimize compCodeOpt() const
    {
#if 0
        // Switching between size & speed has measurable throughput impact
        // (3.5% on NGen CoreLib when measured). It used to be enabled for
        // DEBUG, but should generate identical code between CHK & RET builds,
        // so that's not acceptable.
        // TODO-Throughput: Figure out what to do about size vs. speed & throughput.
        //                  Investigate the cause of the throughput regression.

        return opts.compCodeOpt;
#else
        return BLENDED_CODE;
#endif
    }

    //--------------------- Info about the procedure --------------------------

    CompiledMethodInfo info;

    bool compEnregLocals()
    {
        return opts.OptEnabled(CLFLG_REGVAR);
    }

    bool compEnregStructLocals()
    {
        return (JitConfig.JitEnregStructLocals() != 0);
    }

    bool compObjectStackAllocation()
    {
        return (JitConfig.JitObjectStackAllocation() != 0);
    }

    // Returns true if the method requires a PInvoke prolog and epilog
    bool compMethodRequiresPInvokeFrame()
    {
        return (info.compUnmanagedCallCountWithGCTransition > 0);
    }

    // Returns true if address-exposed user variables should be poisoned with a recognizable value
    bool compShouldPoisonFrame()
    {
#ifdef FEATURE_ON_STACK_REPLACEMENT
        if (opts.IsOSR())
            return false;
#endif
        return !info.compInitMem && opts.compDbgCode;
    }

#if defined(DEBUG)

    void compDispLocalVars();

#endif // DEBUG

private:
    class ClassLayoutTable* m_classLayoutTable = nullptr;

    class ClassLayoutTable* typCreateClassLayoutTable();
    class ClassLayoutTable* typGetClassLayoutTable();

public:
    static bool typIsLayoutNum(unsigned layoutNum);
    INDEBUG(const char* typGetName(unsigned typeNum);)
    // Get the layout having the specified layout number.
    ClassLayout* typGetLayoutByNum(unsigned layoutNum);
    // Get the layout number of the specified layout.
    unsigned typGetLayoutNum(ClassLayout* layout);
    // Get the layout having the specified size but no class handle.
    ClassLayout* typGetBlkLayout(unsigned blockSize);
    // Get the number of a layout having the specified size but no class handle.
    unsigned typGetBlkLayoutNum(unsigned blockSize);
    // Get the layout for the specified class handle.
    ClassLayout* typGetObjLayout(CORINFO_CLASS_HANDLE classHandle);
    // Get the number of a layout for the specified class handle.
    unsigned typGetObjLayoutNum(CORINFO_CLASS_HANDLE classHandle);
    // Get the struct type for the specified class handle.
    var_types typGetStructType(CORINFO_CLASS_HANDLE classHandle, var_types* elementType = nullptr);
    // Get the struct type for the specified layout.
    var_types typGetStructType(ClassLayout* layout);
    // Get the layout of a STRUCT typed node.
    ClassLayout* typGetStructLayout(GenTree* node);
    // Get the layout of a Vector2/3/4/T/NT type.
    ClassLayout* typGetVectorLayout(GenTree* node);
    ClassLayout* typGetVectorLayout(var_types simdType, var_types elementType);
#ifdef FEATURE_SIMD
    unsigned typGetLargestSimdTypeSize();
#endif

//-------------------------- Global Compiler Data ------------------------------------

#ifdef DEBUG
private:
    static LONG s_compMethodsCount; // to produce unique label names
#endif

public:
#ifdef DEBUG
    LONG     compMethodID;
    unsigned compGenTreeID    = 0;
    unsigned compStatementID  = 0;
    unsigned compBasicBlockID = 0;
#endif

    EHblkDsc* compHndBBtab           = nullptr; // array of EH data
    unsigned  compHndBBtabCount      = 0;       // element count of used elements in EH data array
    unsigned  compHndBBtabAllocCount = 0;       // element count of allocated elements in EH data array

    unsigned compMapILargNum(unsigned ilArgNum);
    unsigned compMapILvarNum(unsigned ilVarNum);
    unsigned compMap2ILvarNum(unsigned varNum) const;

    //-------------------------------------------------------------------------

    static void compStartup();  // One-time initialization
    static void compShutdown(); // One-time finalization

    Compiler(ArenaAllocator*        alloc,
             const CORINFO_EE_INFO* eeInfo,
             CORINFO_METHOD_INFO*   methodInfo,
             ICorJitInfo*           jitInfo,
             InlineInfo*            inlineInfo = nullptr);
    void compInitMethodName();
    void compInit();

    //------------ Some utility functions --------------

    void* compGetHelperFtn(CorInfoHelpFunc ftnNum,         /* IN  */
                           void**          ppIndirection); /* OUT */

    // Components used by the compiler may write unit test suites, and
    // have them run within this method.  They will be run only once per process, and only
    // in debug.  (Perhaps should be under the control of a COMPlus_ flag.)
    // These should fail by asserting.
    INDEBUG(void compDoComponentUnitTestsOnce();)

    CorJitResult compCompileMain(void** nativeCode, uint32_t* nativeCodeSize, JitFlags* jitFlags);
    void compCompile(void** nativeCode, uint32_t* nativeCodeSize, JitFlags* jitFlags);
    void         compCompileFinish();
    CorJitResult compCompileHelper(void** nativeCode, uint32_t* nativeCodeSize, JitFlags* jitFlags);

    void        phRemoveNotImportedBlocks();
    PhaseStatus phMorphAllocObj();
    void        phComputePreds();
    void        phMorph();
    void        phGSCookie();
    void        phAddSpecialLocals();
    void        phImplicitRefLocals();
    void        phRefCountLocals();
    void        phSetEvalOrder();
    void        phSsaLiveness();
    void        phSsaOpt();
    void        phRemoveRedundantZeroInits();
    void        phSetFullyInterruptible();
    void        phUpdateFlowGraph();
    PhaseStatus phInsertGCPolls();
    void        phDetermineFirstColdBlock();
    PhaseStatus phRationalize();
    PhaseStatus phLower();
#if !FEATURE_FIXED_OUT_ARGS
    PhaseStatus phSetThrowHelperBlockStackLevel();
#endif

    ArenaAllocator* compGetArenaAllocator();

    void generatePatchpointInfo();

#if LOOP_HOIST_STATS
    unsigned m_loopsConsidered             = 0;
    bool     m_curLoopHasHoistedExpression = false;
    unsigned m_loopsWithHoistedExpressions = 0;
    unsigned m_totalHoistedExpressions     = 0;

    void AddLoopHoistStats();
    void PrintPerMethodLoopHoistStats();

    static CritSecObject s_loopHoistStatsLock; // This lock protects the data structures below.
    static unsigned      s_loopsConsidered;
    static unsigned      s_loopsWithHoistedExpressions;
    static unsigned      s_totalHoistedExpressions;

    static void PrintAggregateLoopHoistStats(FILE* f);
#endif // LOOP_HOIST_STATS

    bool compIsForInlining() const;

    VarScopeDsc** compEnterScopeList; // List has the offsets where variables enter scope, sorted by instr offset
    VarScopeDsc** compExitScopeList;  // List has the offsets where variables go out of scope, sorted by instr offset

    bool compVarScopeExtended = false;

    void         compInitSortedScopeLists();
    VarScopeDsc* compGetNextEnterScope(unsigned offs, unsigned* nextEnterScope);
    VarScopeDsc* compGetNextExitScope(unsigned offs, unsigned* nextExitScope);
    VarScopeDsc* compGetNextEnterScopeScan(unsigned offs, unsigned* nextEnterScope);
    VarScopeDsc* compGetNextExitScopeScan(unsigned offs, unsigned* nextExitScope);

    bool compIsProfilerHookNeeded();

public:
#ifdef DEBUG
    void compFunctionTraceStart();
    void compFunctionTraceEnd(void* methodCodePtr, ULONG methodCodeSize, bool isNYI);
#endif

protected:
    void compInitAltJit();
    void compInitConfigOptions();
    void compInitOptions();
    INDEBUG(void compDumpOptions();)
    void compInitPgo();
    bool compCanSwitchToOptimized();
    void compSwitchToOptimized();
    void compSetProcessor();
    void compInitDebuggingInfo();
    void compSetOptimizationLevel(const ILStats& ilStats);

#ifdef PROFILING_SUPPORTED
    // Data required for generating profiler Enter/Leave/TailCall hooks

    bool  compProfilerHookNeeded; // Whether profiler Enter/Leave/TailCall hook needs to be generated for the method
    bool  compProfilerMethHndIndirected; // Whether compProfilerHandle is pointer to the handle or is an actual handle
    void* compProfilerMethHnd; // Profiler handle of the method being compiled. Passed as param to ELT callbacks
#endif

    size_t compMaxUncheckedOffsetForNullObject;

public:
    CompAllocator getAllocator(CompMemKind cmk = CMK_Generic)
    {
        return CompAllocator(compArenaAllocator, cmk);
    }

#ifdef DEBUG
    CompAllocator getAllocatorDebugOnly()
    {
        return getAllocator(CMK_DebugOnly);
    }
#endif // DEBUG

public:
// The following is used to track liveness of local variables, initialization
// of valueclass constructors, and type safe use of IL instructions.

#ifdef DEBUG
    // One line log function. Default level is 0. Increasing it gives you
    // more log information

    // levels are currently unused: #define JITDUMP(level,...)                     ();
    void JitLogEE(unsigned level, const char* fmt, ...);

    bool compDebugBreak = false;

    bool compJitHaltMethod();

#endif

    /*
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XX                                                                           XX
    XX                   GS Security checks for unsafe buffers                   XX
    XX                                                                           XX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    */
public:
    struct ShadowParamVarInfo
    {
        FixedBitVect* assignGroup; // the closure set of variables whose values depend on each other
        unsigned      shadowLclNum;

#ifdef DEBUG
        void Print()
        {
            printf("assignGroup [%p]; shadowCopy: %V02u;\n", assignGroup, shadowLclNum);
        }
#endif
    };

    ShadowParamVarInfo* gsShadowVarInfo = nullptr; // Table used by shadow param analysis code

    void gsGSChecksInitCookie();   // Grabs cookie variable
    void gsCopyShadowParams();     // Identify vulnerable params and create dhadow copies
    bool gsFindVulnerableParams(); // Shadow param analysis code
    void gsParamsToShadows();      // Insert copy code and replave param uses by shadow

    static fgWalkPreFn gsMarkPtrsAndAssignGroups; // Shadow param analysis tree-walk
    static fgWalkPreFn gsReplaceShadowParams;     // Shadow param replacement tree-walk

#define DEFAULT_MAX_INLINE_SIZE 100 // Methods with >  DEFAULT_MAX_INLINE_SIZE IL bytes will never be inlined.
                                    // This can be overwritten by setting complus_JITInlineSize env variable.

#define DEFAULT_MAX_INLINE_DEPTH 20 // Methods at more than this level deep will not be inlined

#define DEFAULT_MAX_LOCALLOC_TO_LOCAL_SIZE 32 // fixed locallocs of this size or smaller will convert to local buffers

private:
#ifdef FEATURE_JIT_METHOD_PERF
    JitTimer*                  pCompJitTimer = nullptr; // Timer data structure (by phases) for current compilation.
    static CompTimeSummaryInfo s_compJitTimerSummary;   // Summary of the Timer information for the whole run.

    static LPCWSTR JitTimeLogCsv();        // Retrieve the file name for CSV from ConfigDWORD.
    static LPCWSTR compJitTimeLogFilename; // If a log file for JIT time is desired, filename to write it to.
#endif
    void BeginPhase(Phases phase); // Indicate the start of the given phase.
    void EndPhase(Phases phase);   // Indicate the end of the given phase.

#if MEASURE_CLRAPI_CALLS
    // Thin wrappers that call into JitTimer (if present).
    inline void CLRApiCallEnter(unsigned apix);
    inline void CLRApiCallLeave(unsigned apix);

public:
    inline void CLR_API_Enter(API_ICorJitInfo_Names ename);
    inline void CLR_API_Leave(API_ICorJitInfo_Names ename);

private:
#endif

#if defined(DEBUG) || defined(INLINE_DATA)
    // These variables are associated with maintaining SQM data about compile time.
    unsigned __int64 m_compCyclesAtEndOfInlining; // The thread-virtualized cycle count at the end of the inlining phase
                                                  // in the current compilation.
    unsigned __int64 m_compCycles;                // Net cycle count for current compilation
    DWORD m_compTickCountAtEndOfInlining; // The result of GetTickCount() (# ms since some epoch marker) at the end of
                                          // the inlining phase in the current compilation.
#endif                                    // defined(DEBUG) || defined(INLINE_DATA)

    // Records the SQM-relevant (cycles and tick count).  Should be called after inlining is complete.
    // (We do this after inlining because this marks the last point at which the JIT is likely to cause
    // type-loading and class initialization).
    void RecordStateAtEndOfInlining();
    // Assumes being called at the end of compilation.  Update the SQM state.
    void RecordStateAtEndOfCompilation();

public:
#if FUNC_INFO_LOGGING
    static LPCWSTR compJitFuncInfoFilename; // If a log file for per-function information is required, this is the
                                            // filename to write it to.
    static FILE* compJitFuncInfoFile;       // And this is the actual FILE* to write to.
#endif                                      // FUNC_INFO_LOGGING

#if MEASURE_NOWAY
    void RecordNowayAssert(const char* filename, unsigned line, const char* condStr);
#endif // MEASURE_NOWAY

    // Should we actually fire the noway assert body and the exception handler?
    bool compShouldThrowOnNoway();

    // The "FieldSeqStore", for canonicalizing field sequences.  See the definition of FieldSeqStore for
    // operations.
    FieldSeqStore* m_fieldSeqStore = nullptr;

    FieldSeqStore* GetFieldSeqStore()
    {
        Compiler* compRoot = impInlineRoot();
        if (compRoot->m_fieldSeqStore == nullptr)
        {
            compRoot->m_fieldSeqStore = new (this, CMK_FieldSeqStore) FieldSeqStore(compRoot);
        }
        return compRoot->m_fieldSeqStore;
    }

    typedef JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, FieldSeqNode*> NodeToFieldSeqMap;

    // Some nodes of "TYP_BYREF" or "TYP_I_IMPL" actually represent the address of a field within a struct, but since
    // the offset of the field is zero, there's no "GT_ADD" node.  We normally attach a field sequence to the constant
    // that is added, but what do we do when that constant is zero, and is thus not present?  We use this mechanism to
    // attach the field sequence directly to the address node.
    NodeToFieldSeqMap* m_zeroOffsetFieldMap = nullptr;

    FieldSeqNode* GetZeroOffsetFieldSeq(GenTree* node);
    void CopyZeroOffsetFieldSeq(GenTree* from, GenTree* to);
    void AddZeroOffsetFieldSeq(GenTree* node, FieldSeqNode* fieldSeq);

    // The Refany type is the only struct type whose structure is implicitly assumed by IL.  We need its fields.
    CORINFO_CLASS_HANDLE m_refAnyClass = NO_CLASS_HANDLE;
    FieldSeqNode*        GetRefanyValueField()
    {
        if (m_refAnyClass == nullptr)
        {
            m_refAnyClass = info.compCompHnd->getBuiltinClass(CLASSID_TYPED_BYREF);
        }
        return GetByReferenceValueField(info.compCompHnd->getFieldInClass(m_refAnyClass, 0));
    }
    FieldSeqNode* GetRefanyTypeField()
    {
        if (m_refAnyClass == nullptr)
        {
            m_refAnyClass = info.compCompHnd->getBuiltinClass(CLASSID_TYPED_BYREF);
        }
        return GetFieldSeqStore()->CreateSingleton(info.compCompHnd->getFieldInClass(m_refAnyClass, 1));
    }

    FieldSeqNode* GetByReferenceValueField(CORINFO_FIELD_HANDLE byRefFieldHandle)
    {
        CORINFO_CLASS_HANDLE byRefStructType;
        CorInfoType          byRefType = info.compCompHnd->getFieldType(byRefFieldHandle, &byRefStructType);
        assert((byRefType == CORINFO_TYPE_VALUECLASS) && (byRefStructType != NO_CLASS_HANDLE));
        CORINFO_FIELD_HANDLE valueFieldHandle = info.compCompHnd->getFieldInClass(byRefStructType, 0);
        assert(info.compCompHnd->getFieldOffset(valueFieldHandle) == 0);
        assert(info.compCompHnd->getFieldType(valueFieldHandle) == CORINFO_TYPE_BYREF);
        FieldSeqStore* fieldStore = GetFieldSeqStore();
        return fieldStore->Append(fieldStore->CreateSingleton(byRefFieldHandle), valueFieldHandle);
    }

    const static HelperCallProperties s_helperCallProperties;

    bool abiMorphStackStructArg(CallArgInfo* argInfo, GenTree* arg);
    void abiMorphStackLclArgPromoted(CallArgInfo* argInfo, GenTreeLclVar* arg);
    void abiMorphMkRefAnyToFieldList(CallArgInfo* argInfo, GenTreeOp* mkrefany);
    GenTreeFieldList* abiMakeFieldList(GenTree* arg);
    void abiMorphSingleRegStructArg(CallArgInfo* argInfo, GenTree* arg);
    GenTree* abiMorphSingleRegLclArgPromoted(GenTreeLclVar* arg, var_types argRegType, unsigned argSize);
#ifndef TARGET_X86
    void abiMorphArgs2ndPass(GenTreeCall* call);
    GenTree* abiMorphMkRefAnyToStore(unsigned tempLclNum, GenTreeOp* mkrefany);
#endif
#if FEATURE_MULTIREG_ARGS || FEATURE_MULTIREG_RET
    GenTree* abiMorphMultiRegHfaLclArgPromoted(CallArgInfo* argInfo, GenTreeLclVar* arg);
    GenTree* abiMorphMultiRegLclArgPromoted(CallArgInfo* argInfo, const struct AbiRegFieldMap& map);
    GenTree* abiMorphMultiRegStructArg(CallArgInfo* argInfo, GenTree* arg);
#ifdef FEATURE_SIMD
    GenTree* abiMorphMultiRegSimdArg(CallArgInfo* argInfo, GenTree* arg);
#endif
    GenTree* abiMorphMultiRegLclArg(CallArgInfo* argInfo, GenTreeLclVarCommon* arg);
    GenTree* abiMorphMultiRegObjArg(CallArgInfo* argInfo, GenTreeObj* arg);
    GenTree* abiMakeIndirAddrMultiUse(GenTree** addrInOut, ssize_t* addrOffsetOut, unsigned indirSize);
    GenTree* abiNewMultiLoadIndir(GenTree* addr, ssize_t addrOffset, unsigned indirSize);
    GenTree* abiMorphMultiRegCallArg(CallArgInfo* argInfo, GenTreeCall* arg);
#endif
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64) || defined(TARGET_ARM)
    unsigned abiAllocateStructArgTemp(ClassLayout* argLayout);
    void abiFreeAllStructArgTemps();
#endif
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
    void abiMorphImplicitByRefStructArg(GenTreeCall* call, CallArgInfo* argInfo);
#endif
    void abiMorphStructReturn(GenTreeUnOp* ret, GenTree* val);

    bool killGCRefs(GenTree* tree);
}; // end of class Compiler

template <typename T>
T dspPtr(T p)
{
#ifdef DEBUG
    return JitTls::GetCompiler()->dspPtr(p);
#else
    return p;
#endif
}

template <typename T>
T dspOffset(T o)
{
#ifdef DEBUG
    return JitTls::GetCompiler()->dspOffset(o);
#else
    return o;
#endif
}

// This class is responsible for checking validity and profitability of struct promotion.
// If it is both legal and profitable, then TryPromoteStructLocal promotes the struct and initializes
// nessesary information for fgMorphStructField to use.
class StructPromotionHelper
{
    static constexpr unsigned MaxFieldCount = 4;
#ifndef FEATURE_SIMD
    static constexpr unsigned MaxStructSize = MaxFieldCount * 8;
#elif defined(TARGET_XARCH)
    static constexpr unsigned MaxStructSize = MaxFieldCount * YMM_REGSIZE_BYTES;
#elif defined(TARGET_ARM64)
    static constexpr unsigned MaxStructSize = MaxFieldCount * FP_REGSIZE_BYTES;
#endif
    static constexpr unsigned MaxDepth = min(FieldSeqNode::MaxLength, 4);

    struct FieldInfo
    {
        CORINFO_FIELD_HANDLE fieldSeq[MaxDepth];
        uint8_t              fieldSeqLength;
        var_types            type;
        unsigned             offset;
        ClassLayout*         layout;
    };

    struct StructInfo
    {
        CORINFO_CLASS_HANDLE typeHandle;
        uint8_t              fieldCount;
        bool                 canPromoteStructType = false;
        bool                 containsHoles        = false;
        bool                 customLayout         = false;
        bool                 fieldsSorted         = false;
        FieldInfo            fields[MaxFieldCount];

        StructInfo(CORINFO_CLASS_HANDLE typeHandle, unsigned fieldCount)
            : typeHandle(typeHandle), fieldCount(static_cast<uint8_t>(fieldCount))
        {
            assert(fieldCount <= MaxFieldCount);
        }
    };

    Compiler*  compiler;
    StructInfo info;

public:
    StructPromotionHelper(Compiler* compiler) : compiler(compiler), info(nullptr, 0)
    {
    }

    static constexpr unsigned GetMaxFieldCount()
    {
        return MaxFieldCount;
    }

    bool CanPromoteStructType(CORINFO_CLASS_HANDLE typeHandle);
    bool TryPromoteStructLocal(unsigned lclNum);

private:
    bool CanPromoteStructLocal(unsigned lclNum);
    bool ShouldPromoteStructLocal(unsigned lclNum);
    void PromoteStructLocal(unsigned lclNum);
    void SortFields();

    void GetFieldInfo(unsigned index);
    void GetSingleFieldStructInfo(FieldInfo& field, CORINFO_CLASS_HANDLE structHandle);
};

#ifdef TARGET_ARM64
// TODO-MIKE-Cleanup: It's not clear if storing immediates directly inside GenTreeInstr
// is a good idea. It does avoid the need for "containment" but there's not enough space
// in GenTreeInstr to store a 64 bit bitmask immediate so it has to be stored in its
// encoded form so anyone who cares about the value has to decode it first. But then
// there's not a lot going on post lowering that involves looking at immediates so it's
// not such a big issue.
// And at least in theory, storing the encoded immediate is good for throughput, since
// the rather expensive encoding is done only once, in lowering. Except that currently
// the emitter interface requires decoded immediates, only for the emitter to encode it
// again...
ssize_t DecodeBitmaskImm(unsigned encoded, emitAttr size);
#endif

struct GenTreeInstr : public GenTree
{
    using Use = GenTreeUse;

private:
#if defined(TARGET_ARM64)
    static constexpr unsigned NUM_OPS_BITS = 2;
    static constexpr unsigned INS_BITS     = 9;
    static constexpr unsigned SIZE_BITS    = 9;
    static constexpr unsigned OPT_BITS     = 4;
#elif defined(TARGET_ARM)
    static constexpr unsigned NUM_OPS_BITS  = 2;
    static constexpr unsigned INS_BITS      = 9;
    static constexpr unsigned SIZE_BITS     = 9;
    static constexpr unsigned OPT_BITS      = 3;
#elif defined(TARGET_XARCH)
    // TODO-MIKE-Cleanup: Wishful thinking... it may be nice to use GenTreeInstr on x86/64
    // but compared to ARM there aren't many interesting use cases and x86/64 instructions
    // are problematic due the possibility of having a 32 bit immediate and a 32 bit address
    // mode displacement.
    // There's just not enough room for both imm32 and disp32, no matter how things are packed.
    // Except if we steal some bits from GenTree, value numbers aren't normally needed in and
    // post lowering.
    static constexpr unsigned NUM_OPS_BITS = 2;
    static constexpr unsigned INS_BITS     = 10;
    static constexpr unsigned SIZE_BITS    = 9;
#endif

    static_assert_no_msg(INS_COUNT <= (1 << INS_BITS));
    static_assert_no_msg(EA_BYREF < (1 << SIZE_BITS));
#if defined(TARGET_ARM64)
    static_assert_no_msg(INS_OPTS_SXTX < (1 << OPT_BITS));
#elif defined(TARGET_ARM)
    static_assert_no_msg(INS_OPTS_ROR < (1 << OPT_BITS));
#endif

    unsigned    m_numOps : NUM_OPS_BITS;
    instruction m_ins : INS_BITS;
    // TODO-MIKE-Cleanup: Using emitAttr for size is overkill, all we need is to be able to control
    // the instruction size (32/64 bit) independently from type so we can potentially take advantage
    // of the implicit zero extension that 32 bit instructions perform. But there's no need to do
    // this for GC types so the GC info conveyed by emitAttr isn't necessary.
    emitAttr m_size : SIZE_BITS;
#ifdef TARGET_ARMARCH
    insOpts m_opt : OPT_BITS;
#endif
    unsigned m_imm;

    union {
        Use  m_inlineUses[3];
        Use* m_uses;
    };

public:
    GenTreeInstr(var_types type, instruction ins, GenTree* op1)
        : GenTree(GT_INSTR, type)
        , m_numOps(1)
        , m_ins(ins)
        , m_size(emitActualTypeSize(type))
#ifdef TARGET_ARMARCH
        , m_opt(INS_OPTS_NONE)
#endif
        , m_imm(0)
        , m_inlineUses{op1}
    {
    }

    GenTreeInstr(var_types type, instruction ins, GenTree* op1, GenTree* op2)
        : GenTree(GT_INSTR, type)
        , m_numOps(2)
        , m_ins(ins)
        , m_size(emitActualTypeSize(type))
#ifdef TARGET_ARMARCH
        , m_opt(INS_OPTS_NONE)
#endif
        , m_imm(0)
        , m_inlineUses{op1, op2}
    {
    }

    GenTreeInstr(GenTreeInstr* from, Compiler* compiler)
        : GenTree(from->GetOper(), from->GetType())
        , m_ins(from->m_ins)
        , m_size(from->m_size)
#ifdef TARGET_ARMARCH
        , m_opt(from->m_opt)
#endif
        , m_imm(from->m_imm)
    {
        SetNumOps(from->m_numOps, compiler->getAllocator(CMK_ASTNode));

        for (unsigned i = 0; i < from->m_numOps; i++)
        {
            SetOp(i, compiler->gtCloneExpr(from->GetOp(i)));
        }
    }

    instruction GetIns() const
    {
        return m_ins;
    }

    emitAttr GetSize() const
    {
        return m_size;
    }

#ifdef TARGET_ARMARCH
    insOpts GetOption() const
    {
        return m_opt;
    }

    void SetOption(insOpts opt)
    {
        m_opt = opt;
    }
#endif

    void SetIns(instruction ins)
    {
        assert(ins < INS_COUNT);

        m_ins  = ins;
        m_size = emitActualTypeSize(GetType());
#ifdef TARGET_ARMARCH
        m_opt = INS_OPTS_NONE;
#endif
    }

    void SetIns(instruction ins,
                emitAttr    size
#ifdef TARGET_ARMARCH
                ,
                insOpts opt = INS_OPTS_NONE
#endif
                )
    {
        assert(ins < INS_COUNT);
        assert(size <= EA_BYREF);
#if defined(TARGET_ARM64)
        assert(opt <= INS_OPTS_SXTX);
#elif defined(TARGET_ARM)
        assert(opt <= INS_OPTS_ROR);
#endif

        m_ins  = ins;
        m_size = size;
#ifdef TARGET_ARMARCH
        m_opt = opt;
#endif
    }

    unsigned GetImmediate() const
    {
        return m_imm;
    }

    void SetImmediate(unsigned imm)
    {
        m_imm = imm;
    }

    unsigned GetNumOps() const
    {
        return m_numOps;
    }

    void SetNumOps(unsigned numOps)
    {
        m_numOps = static_cast<uint16_t>(numOps);
        assert(HasInlineUses());

        new (m_inlineUses) Use[numOps]();
    }

    void SetNumOps(unsigned numOps, CompAllocator alloc)
    {
        assert(numOps < UINT16_MAX);
        assert(m_numOps == 0);

        m_numOps = static_cast<uint16_t>(numOps);

        if (HasInlineUses())
        {
            new (m_inlineUses) Use[numOps]();
        }
        else
        {
            m_uses = new (alloc) Use[numOps]();
        }
    }

    GenTree* GetOp(unsigned index) const
    {
        return GetUse(index).GetNode();
    }

    void SetOp(unsigned index, GenTree* node)
    {
        assert(node != nullptr);
        GetUse(index).SetNode(node);
    }

    const Use& GetUse(unsigned index) const
    {
        assert(index < m_numOps);
        return GetUses()[index];
    }

    Use& GetUse(unsigned index)
    {
        assert(index < m_numOps);
        return GetUses()[index];
    }

    IteratorPair<Use*> Uses()
    {
        Use* uses = GetUses();
        return MakeIteratorPair(uses, uses + GetNumOps());
    }

    static bool Equals(GenTreeInstr* instr1, GenTreeInstr* instr2)
    {
        if ((instr1->GetType() != instr2->GetType()) || (instr1->m_ins != instr2->m_ins) ||
            (instr1->m_size != instr2->m_size) ||
#ifdef TARGET_ARMARCH
            (instr1->m_opt != instr2->m_opt) ||
#endif
            (instr1->m_numOps != instr2->m_numOps))
        {
            return false;
        }

        for (unsigned i = 0; i < instr1->m_numOps; i++)
        {
            if (!Compare(instr1->GetOp(i), instr2->GetOp(i)))
            {
                return false;
            }
        }

        return true;
    }

    // Delete some functions inherited from GenTree to avoid accidental use, at least
    // when the node object is accessed via GenTreeInstr* rather than GenTree*.
    GenTree*           gtGetOp1() const          = delete;
    GenTree*           gtGetOp2() const          = delete;
    GenTree*           gtGetOp2IfPresent() const = delete;
    GenTreeUnOp*       AsUnOp()                  = delete;
    const GenTreeUnOp* AsUnOp() const            = delete;
    GenTreeOp*         AsOp()                    = delete;
    const GenTreeOp*   AsOp() const              = delete;

private:
    bool HasInlineUses() const
    {
        return m_numOps <= _countof(m_inlineUses);
    }

    Use* GetUses()
    {
        return HasInlineUses() ? m_inlineUses : m_uses;
    }

    const Use* GetUses() const
    {
        return HasInlineUses() ? m_inlineUses : m_uses;
    }

#if DEBUGGABLE_GENTREE
public:
    GenTreeInstr() : GenTree()
    {
    }
#endif
};

//---------------------------------------------------------------------------------------------------------------------
// GenTreeVisitor: a flexible tree walker implemented using the curiously-recurring-template pattern.
//
// This class implements a configurable walker for IR trees. There are five configuration options (defaults values are
// shown in parentheses):
//
// - ComputeStack (false): when true, the walker will push each node onto the `m_ancestors` stack. "Ancestors" is a bit
//                         of a misnomer, as the first entry will always be the current node.
//
// - DoPreOrder (false): when true, the walker will invoke `TVisitor::PreOrderVisit` with the current node as an
//                       argument before visiting the node's operands.
//
// - DoPostOrder (false): when true, the walker will invoke `TVisitor::PostOrderVisit` with the current node as an
//                        argument after visiting the node's operands.
//
// - DoLclVarsOnly (false): when true, the walker will only invoke `TVisitor::PreOrderVisit` for lclVar nodes.
//                          `DoPreOrder` must be true if this option is true.
//
// - UseExecutionOrder (false): when true, then walker will visit a node's operands in execution order (e.g. if a
//                              binary operator has the `GTF_REVERSE_OPS` flag set, the second operand will be
//                              visited before the first).
//
// At least one of `DoPreOrder` and `DoPostOrder` must be specified.
//
// A simple pre-order visitor might look something like the following:
//
//     class CountingVisitor final : public GenTreeVisitor<CountingVisitor>
//     {
//     public:
//         enum
//         {
//             DoPreOrder = true
//         };
//
//         unsigned m_count;
//
//         CountingVisitor(Compiler* compiler)
//             : GenTreeVisitor<CountingVisitor>(compiler), m_count(0)
//         {
//         }
//
//         Compiler::fgWalkResult PreOrderVisit(GenTree* node)
//         {
//             m_count++;
//         }
//     };
//
// This visitor would then be used like so:
//
//     CountingVisitor countingVisitor(compiler);
//     countingVisitor.WalkTree(root);
//
template <typename TVisitor>
class GenTreeVisitor
{
protected:
    typedef Compiler::fgWalkResult fgWalkResult;

    enum
    {
        ComputeStack      = false,
        DoPreOrder        = false,
        DoPostOrder       = false,
        DoLclVarsOnly     = false,
        UseExecutionOrder = false,
    };

    Compiler*            m_compiler;
    ArrayStack<GenTree*> m_ancestors;

    GenTreeVisitor(Compiler* compiler) : m_compiler(compiler), m_ancestors(compiler->getAllocator(CMK_ArrayStack))
    {
        assert(compiler != nullptr);

        static_assert_no_msg(TVisitor::DoPreOrder || TVisitor::DoPostOrder);
        static_assert_no_msg(!TVisitor::DoLclVarsOnly || TVisitor::DoPreOrder);
    }

    fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
    {
        return fgWalkResult::WALK_CONTINUE;
    }

    fgWalkResult PostOrderVisit(GenTree** use, GenTree* user)
    {
        return fgWalkResult::WALK_CONTINUE;
    }

public:
    fgWalkResult WalkTree(GenTree** use, GenTree* user)
    {
        assert(use != nullptr);

        GenTree* node = *use;

        if (TVisitor::ComputeStack)
        {
            m_ancestors.Push(node);
        }

        fgWalkResult result = fgWalkResult::WALK_CONTINUE;
        if (TVisitor::DoPreOrder && !TVisitor::DoLclVarsOnly)
        {
            result = reinterpret_cast<TVisitor*>(this)->PreOrderVisit(use, user);
            if (result == fgWalkResult::WALK_ABORT)
            {
                return result;
            }

            node = *use;
            if ((node == nullptr) || (result == fgWalkResult::WALK_SKIP_SUBTREES))
            {
                goto DONE;
            }
        }

        switch (node->OperGet())
        {
            // Leaf lclVars
            case GT_LCL_VAR:
            case GT_LCL_FLD:
            case GT_LCL_ADDR:
                if (TVisitor::DoLclVarsOnly)
                {
                    result = reinterpret_cast<TVisitor*>(this)->PreOrderVisit(use, user);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }
                FALLTHROUGH;

            // Leaf nodes
            case GT_LCL_USE:
            case GT_CATCH_ARG:
            case GT_LABEL:
            case GT_METHOD_ADDR:
            case GT_RET_EXPR:
            case GT_CNS_INT:
#ifndef TARGET_64BIT
            case GT_CNS_LNG:
#endif
            case GT_CNS_DBL:
            case GT_CNS_STR:
            case GT_MEMORYBARRIER:
            case GT_JMP:
            case GT_JCC:
            case GT_SETCC:
            case GT_NO_OP:
            case GT_START_NONGC:
            case GT_START_PREEMPTGC:
            case GT_PROF_HOOK:
#ifndef FEATURE_EH_FUNCLETS
            case GT_END_LFIN:
#endif
            case GT_JMPTABLE:
            case GT_CLS_VAR_ADDR:
            case GT_ARGPLACE:
            case GT_PHYSREG:
            case GT_EMITNOP:
            case GT_PINVOKE_PROLOG:
            case GT_PINVOKE_EPILOG:
            case GT_IL_OFFSET:
                break;

            case GT_STORE_LCL_VAR:
            case GT_STORE_LCL_FLD:
                if (TVisitor::DoLclVarsOnly)
                {
                    result = reinterpret_cast<TVisitor*>(this)->PreOrderVisit(use, user);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }
                result = WalkTree(&node->AsUnOp()->gtOp1, node);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                break;

            case GT_NOP:
            case GT_RETURN:
            case GT_RETFILT:
                // These are not always unary.
                if (node->AsUnOp()->gtOp1 == nullptr)
                {
                    break;
                }
                FALLTHROUGH;
            case GT_LCL_DEF:
            case GT_NOT:
            case GT_NEG:
            case GT_FNEG:
            case GT_BSWAP:
            case GT_BSWAP16:
            case GT_COPY:
            case GT_RELOAD:
            case GT_ARR_LENGTH:
            case GT_CAST:
            case GT_BITCAST:
            case GT_EXTRACT:
            case GT_CKFINITE:
            case GT_LCLHEAP:
            case GT_FIELD_ADDR:
            case GT_IND:
            case GT_OBJ:
            case GT_BLK:
            case GT_BOX:
            case GT_ALLOCOBJ:
            case GT_INIT_VAL:
            case GT_JTRUE:
            case GT_SWITCH:
            case GT_NULLCHECK:
            case GT_PUTARG_REG:
            case GT_PUTARG_STK:
            case GT_RETURNTRAP:
            case GT_RUNTIMELOOKUP:
            case GT_KEEPALIVE:
            case GT_INC_SATURATE:
#ifdef FEATURE_SIMD
            case GT_SIMD_UPPER_SPILL:
            case GT_SIMD_UPPER_UNSPILL:
#endif
                assert(node->AsUnOp()->gtOp1 != nullptr);
                result = WalkTree(&node->AsUnOp()->gtOp1, node);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                break;

            // Special nodes
            case GT_PHI:
                for (GenTreePhi::Use& use : node->AsPhi()->Uses())
                {
                    result = WalkTree(&use.NodeRef(), node);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }
                break;

            case GT_FIELD_LIST:
                for (GenTreeFieldList::Use& use : node->AsFieldList()->Uses())
                {
                    result = WalkTree(&use.NodeRef(), node);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }
                break;

#ifdef FEATURE_HW_INTRINSICS
            case GT_HWINTRINSIC:
                if (TVisitor::UseExecutionOrder && node->AsHWIntrinsic()->IsBinary() && node->IsReverseOp())
                {
                    result = WalkTree(&node->AsHWIntrinsic()->GetUse(1).NodeRef(), node);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                    result = WalkTree(&node->AsHWIntrinsic()->GetUse(0).NodeRef(), node);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }
                else
                {
                    for (GenTreeHWIntrinsic::Use& use : node->AsHWIntrinsic()->Uses())
                    {
                        result = WalkTree(&use.NodeRef(), node);
                        if (result == fgWalkResult::WALK_ABORT)
                        {
                            return result;
                        }
                    }
                }
                break;
#endif

            case GT_INSTR:
                for (GenTreeInstr::Use& use : node->AsInstr()->Uses())
                {
                    result = WalkTree(&use.NodeRef(), node);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }
                break;

            case GT_ARR_ELEM:
            {
                GenTreeArrElem* const arrElem = node->AsArrElem();

                result = WalkTree(&arrElem->gtArrObj, arrElem);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }

                const unsigned rank = arrElem->gtArrRank;
                for (unsigned dim = 0; dim < rank; dim++)
                {
                    result = WalkTree(&arrElem->gtArrInds[dim], arrElem);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }
                break;
            }

            case GT_ARR_OFFSET:
            case GT_CMPXCHG:
            case GT_COPY_BLK:
            case GT_INIT_BLK:
            case GT_QMARK:
                result = WalkTree(&node->AsTernaryOp()->gtOp1, node);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                result = WalkTree(&node->AsTernaryOp()->gtOp2, node);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                result = WalkTree(&node->AsTernaryOp()->gtOp3, node);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                break;

            case GT_CALL:
            {
                GenTreeCall* const call = node->AsCall();

                if (call->gtCallThisArg != nullptr)
                {
                    result = WalkTree(&call->gtCallThisArg->NodeRef(), call);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }

                for (GenTreeCall::Use& use : call->Args())
                {
                    result = WalkTree(&use.NodeRef(), call);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }

                for (GenTreeCall::Use& use : call->LateArgs())
                {
                    result = WalkTree(&use.NodeRef(), call);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }

                if (call->IsIndirectCall())
                {
                    if (call->gtCallCookie != nullptr)
                    {
                        result = WalkTree(&call->gtCallCookie, call);
                        if (result == fgWalkResult::WALK_ABORT)
                        {
                            return result;
                        }
                    }

                    result = WalkTree(&call->gtCallAddr, call);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }

                if (call->gtControlExpr != nullptr)
                {
                    result = WalkTree(&call->gtControlExpr, call);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }

                break;
            }

            case GT_LEA:
            case GT_INTRINSIC:
            {
                // LEA and INTRINSIC are fake BINOPs. For LEA either operand may be null,
                // and for INTRINSIC the second operand may be null, makeing it unary.
                // Handle them separately so that real BINOPs do not need extra null checks.
                GenTree** op1Use = &node->AsOp()->gtOp1;
                GenTree** op2Use = &node->AsOp()->gtOp2;

                if (TVisitor::UseExecutionOrder && node->IsReverseOp())
                {
                    std::swap(op1Use, op2Use);
                }

                if (*op1Use != nullptr)
                {
                    result = WalkTree(op1Use, node);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }

                if (*op2Use != nullptr)
                {
                    result = WalkTree(op2Use, node);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }
                break;
            }

            // Binary nodes
            default:
            {
                assert(node->OperIsBinary());

                GenTree** op1Use = &node->AsOp()->gtOp1;
                GenTree** op2Use = &node->AsOp()->gtOp2;
                assert(*op1Use != nullptr);
                assert(*op2Use != nullptr);

                if (TVisitor::UseExecutionOrder && node->IsReverseOp())
                {
                    std::swap(op1Use, op2Use);
                }

                result = WalkTree(op1Use, node);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }

                result = WalkTree(op2Use, node);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                break;
            }
        }

    DONE:
        // Finally, visit the current node
        if (TVisitor::DoPostOrder)
        {
            result = reinterpret_cast<TVisitor*>(this)->PostOrderVisit(use, user);
        }

        if (TVisitor::ComputeStack)
        {
            m_ancestors.Pop();
        }

        return result;
    }
};

template <bool computeStack, bool doPreOrder, bool doPostOrder, bool doLclVarsOnly, bool useExecutionOrder>
class GenericTreeWalker final
    : public GenTreeVisitor<GenericTreeWalker<computeStack, doPreOrder, doPostOrder, doLclVarsOnly, useExecutionOrder>>
{
    Compiler::fgWalkData    walkData;
    Compiler::fgWalkPreFn*  preVisitor;
    Compiler::fgWalkPostFn* postVisitor;

public:
    enum
    {
        ComputeStack      = computeStack,
        DoPreOrder        = doPreOrder,
        DoPostOrder       = doPostOrder,
        DoLclVarsOnly     = doLclVarsOnly,
        UseExecutionOrder = useExecutionOrder,
    };

    GenericTreeWalker(Compiler*               compiler,
                      Compiler::fgWalkPreFn*  preVisitor,
                      Compiler::fgWalkPostFn* postVisitor,
                      void*                   callbackData)
        : GenTreeVisitor<GenericTreeWalker<computeStack, doPreOrder, doPostOrder, doLclVarsOnly, useExecutionOrder>>(
              compiler)
        , walkData(compiler, callbackData)
        , preVisitor(preVisitor)
        , postVisitor(postVisitor)
    {
        assert((preVisitor != nullptr) || (postVisitor != nullptr));
    }

    Compiler::fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
    {
        walkData.parent = user;
        return preVisitor(use, &walkData);
    }

    Compiler::fgWalkResult PostOrderVisit(GenTree** use, GenTree* user)
    {
        walkData.parent = user;
        return postVisitor(use, &walkData);
    }
};

inline Compiler::fgWalkResult Compiler::fgWalkTreePre(GenTree** use, fgWalkPreFn* visitor, void* callbackData)
{
    GenericTreeWalker<false, true, false, false, true> walker(this, visitor, nullptr, callbackData);
    return walker.WalkTree(use, nullptr);
}

inline Compiler::fgWalkResult Compiler::fgWalkTreePost(GenTree** use, fgWalkPostFn* visitor, void* callbackData)
{
    GenericTreeWalker<false, false, true, false, true> walker(this, nullptr, visitor, callbackData);
    return walker.WalkTree(use, nullptr);
}

inline Compiler::fgWalkResult Compiler::fgWalkTree(GenTree**    use,
                                                   fgWalkPreFn* preVisitor,
                                                   fgWalkPreFn* postVisitor,
                                                   void*        callbackData)

{
    assert((preVisitor != nullptr) && (postVisitor != nullptr));

    GenericTreeWalker<true, true, true, false, true> walker(this, preVisitor, postVisitor, callbackData);
    return walker.WalkTree(use, nullptr);
}

// A dominator tree visitor implemented using the curiously-recurring-template pattern, similar to GenTreeVisitor.
template <typename TVisitor>
class DomTreeVisitor
{
protected:
    Compiler* const    m_compiler;
    DomTreeNode* const m_domTree;

    DomTreeVisitor(Compiler* compiler, DomTreeNode* domTree) : m_compiler(compiler), m_domTree(domTree)
    {
    }

    void Begin()
    {
    }

    void PreOrderVisit(BasicBlock* block)
    {
    }

    void PostOrderVisit(BasicBlock* block)
    {
    }

    void End()
    {
    }

public:
    //------------------------------------------------------------------------
    // WalkTree: Walk the dominator tree, starting from fgFirstBB.
    //
    // Notes:
    //    This performs a non-recursive, non-allocating walk of the tree by using
    //    DomTreeNode's firstChild and nextSibling links to locate the children of
    //    a node and BasicBlock's bbIDom parent link to go back up the tree when
    //    no more children are left.
    //
    //    Forests are also supported, provided that all the roots are chained via
    //    DomTreeNode::nextSibling to fgFirstBB.
    //
    void WalkTree()
    {
        static_cast<TVisitor*>(this)->Begin();

        for (BasicBlock *next, *block = m_compiler->fgFirstBB; block != nullptr; block = next)
        {
            static_cast<TVisitor*>(this)->PreOrderVisit(block);

            next = m_domTree[block->bbNum].firstChild;

            if (next != nullptr)
            {
                assert(next->bbIDom == block);
                continue;
            }

            do
            {
                static_cast<TVisitor*>(this)->PostOrderVisit(block);

                next = m_domTree[block->bbNum].nextSibling;

                if (next != nullptr)
                {
                    assert(next->bbIDom == block->bbIDom);
                    break;
                }

                block = block->bbIDom;

            } while (block != nullptr);
        }

        static_cast<TVisitor*>(this)->End();
    }
};

// EHClauses: adapter class for forward iteration of the exception handling table using range-based `for`, e.g.:
//    for (EHblkDsc* const ehDsc : EHClauses(compiler))
//
class EHClauses
{
    EHblkDsc* m_begin;
    EHblkDsc* m_end;

    // Forward iterator for the exception handling table entries. Iteration is in table order.
    //
    class iterator
    {
        EHblkDsc* m_ehDsc;

    public:
        iterator(EHblkDsc* ehDsc) : m_ehDsc(ehDsc)
        {
        }

        EHblkDsc* operator*() const
        {
            return m_ehDsc;
        }

        iterator& operator++()
        {
            ++m_ehDsc;
            return *this;
        }

        bool operator!=(const iterator& i) const
        {
            return m_ehDsc != i.m_ehDsc;
        }
    };

public:
    EHClauses(Compiler* comp) : m_begin(comp->compHndBBtab), m_end(comp->compHndBBtab + comp->compHndBBtabCount)
    {
        assert((m_begin != nullptr) || (m_begin == m_end));
    }

    iterator begin() const
    {
        return iterator(m_begin);
    }

    iterator end() const
    {
        return iterator(m_end);
    }
};

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                   Miscellaneous Compiler stuff                            XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

/*****************************************************************************
 *
 *  Variables to keep track of total code amounts.
 */

#if DISPLAY_SIZES

extern size_t grossVMsize;
extern size_t grossNCsize;
extern size_t totalNCsize;

extern unsigned genMethodICnt;
extern unsigned genMethodNCnt;
extern size_t   gcHeaderISize;
extern size_t   gcPtrMapISize;
extern size_t   gcHeaderNSize;
extern size_t   gcPtrMapNSize;

#endif // DISPLAY_SIZES

/*****************************************************************************
 *
 *  Variables to keep track of basic block counts (more data on 1 BB methods)
 */

#if COUNT_BASIC_BLOCKS
extern Histogram bbCntTable;
extern Histogram bbOneBBSizeTable;
#endif

/*****************************************************************************
 *
 *  Used by optFindNaturalLoops to gather statistical information such as
 *   - total number of natural loops
 *   - number of loops with 1, 2, ... exit conditions
 *   - number of loops that have an iterator (for like)
 *   - number of loops that have a constant iterator
 */

#if COUNT_LOOPS

extern unsigned totalLoopMethods;        // counts the total number of methods that have natural loops
extern unsigned maxLoopsPerMethod;       // counts the maximum number of loops a method has
extern unsigned totalLoopOverflows;      // # of methods that identified more loops than we can represent
extern unsigned totalLoopCount;          // counts the total number of natural loops
extern unsigned totalUnnatLoopCount;     // counts the total number of (not-necessarily natural) loops
extern unsigned totalUnnatLoopOverflows; // # of methods that identified more unnatural loops than we can represent
extern unsigned iterLoopCount;           // counts the # of loops with an iterator (for like)
extern unsigned simpleTestLoopCount;     // counts the # of loops with an iterator and a simple loop condition (iter <
                                         // const)
extern unsigned  constIterLoopCount;     // counts the # of loops with a constant iterator (for like)
extern bool      hasMethodLoops;         // flag to keep track if we already counted a method as having loops
extern unsigned  loopsThisMethod;        // counts the number of loops in the current method
extern bool      loopOverflowThisMethod; // True if we exceeded the max # of loops in the method.
extern Histogram loopCountTable;         // Histogram of loop counts
extern Histogram loopExitCountTable;     // Histogram of loop exit counts

#endif // COUNT_LOOPS

/*****************************************************************************
 * variables to keep track of how many iterations we go in a dataflow pass
 */

#if DATAFLOW_ITER

extern unsigned CSEiterCount; // counts the # of iteration for the CSE dataflow
extern unsigned CFiterCount;  // counts the # of iteration for the Const Folding dataflow

#endif // DATAFLOW_ITER

#if MEASURE_BLOCK_SIZE
extern size_t genFlowNodeSize;
extern size_t genFlowNodeCnt;
#endif // MEASURE_BLOCK_SIZE

#if MEASURE_NODE_SIZE
struct NodeSizeStats
{
    void Init()
    {
        genTreeNodeCnt        = 0;
        genTreeNodeSize       = 0;
        genTreeNodeActualSize = 0;
    }

    // Count of tree nodes allocated.
    unsigned __int64 genTreeNodeCnt;

    // The size we allocate.
    unsigned __int64 genTreeNodeSize;

    // The actual size of the node. Note that the actual size will likely be smaller
    // than the allocated size, but we sometimes use SetOper()/ChangeOper() to change
    // a smaller node to a larger one. TODO-Cleanup: add stats on
    // SetOper()/ChangeOper() usage to quantify this.
    unsigned __int64 genTreeNodeActualSize;
};
extern NodeSizeStats genNodeSizeStats;        // Total node size stats
extern NodeSizeStats genNodeSizeStatsPerFunc; // Per-function node size stats
extern Histogram     genTreeNcntHist;
extern Histogram     genTreeNsizHist;
#endif // MEASURE_NODE_SIZE

/*****************************************************************************
 *  Count fatal errors (including noway_asserts).
 */

#if MEASURE_FATAL
extern unsigned fatal_badCode;
extern unsigned fatal_noWay;
extern unsigned fatal_implLimitation;
extern unsigned fatal_NOMEM;
extern unsigned fatal_noWayAssertBody;
#ifdef DEBUG
extern unsigned fatal_noWayAssertBodyArgs;
#endif // DEBUG
extern unsigned fatal_NYI;
#endif // MEASURE_FATAL

extern const BYTE genTypeSizes[];
extern const BYTE genTypeAlignments[];
extern const BYTE genTypeStSzs[];
extern const BYTE genActualTypes[];

/*****************************************************************************/

#ifdef DEBUG
void dumpConvertedVarSet(Compiler* comp, VARSET_VALARG_TP vars);
#endif // DEBUG

struct AddrMode
{
    GenTree* nodes[8];
    GenTree* base;
    GenTree* index     = nullptr;
    unsigned scale     = 0;
    int32_t  offset    = 0;
    unsigned nodeCount = 0;

    AddrMode(GenTree* base) : base(base)
    {
        assert(base->OperIs(GT_ADD) && !base->gtOverflow());
    }

    void Extract(Compiler* compiler);
    bool HasTooManyNodes() const;

    static bool IsIndexScale(size_t value);
    static bool IsIndexShift(ssize_t value);
    static unsigned GetMulIndexScale(GenTree* node);
    static unsigned GetLshIndexScale(GenTree* node);
    static unsigned GetIndexScale(GenTree* node);

private:
    GenTree* ExtractOffset(Compiler* compiler, GenTree* op);
    GenTree* ExtractScale(GenTree* index);
    void AddNode(GenTree* node);
};

#include "compiler.hpp"

#endif //_COMPILER_H_
