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
#include "varset.h"
#include "jitstd.h"
#include "jithashtable.h"
#include "gentree.h"
#include "lir.h"
#include "block.h"
#include "inline.h"
#include "jiteh.h"
#include "instr.h"
#include "regalloc.h"
#include "sm.h"
#include "cycletimer.h"
#include "blockset.h"
#include "arraystack.h"
#include "hashbv.h"
#include "jitexpandarray.h"
#include "valuenum.h"
#include "jittelemetry.h"
#include "namedintrinsiclist.h"
#ifdef LATE_DISASM
#include "disasm.h"
#endif

#include "codegeninterface.h"
#include "regset.h"
#include "jitgcinfo.h"

#if DUMP_GC_TABLES && defined(JIT32_GCENCODER)
#include "gcdump.h"
#endif

#include "emit.h"

#include "hwintrinsic.h"
#include "simd.h"
#include "simdashwintrinsic.h"

/*****************************************************************************
 *                  Forward declarations
 */

struct InfoHdr;            // defined in GCInfo.h
struct escapeMapping_t;    // defined in fgdiagnostic.cpp
class emitter;             // defined in emit.h
struct ShadowParamVarInfo; // defined in GSChecks.cpp
struct InitVarDscInfo;     // defined in register_arg_convention.h
class FgStack;             // defined in fgbasic.cpp
class Instrumentor;        // defined in fgprofile.cpp
class SpanningTreeVisitor; // defined in fgprofile.cpp
class CSE_DataFlow;        // defined in OptCSE.cpp
#ifdef DEBUG
struct IndentStack;
#endif

class Lowering; // defined in lower.h

// The following are defined in this file, Compiler.h

class Compiler;

/*****************************************************************************
 *                  Unwind info
 */

#include "unwind.h"

/*****************************************************************************/

//
// Declare global operator new overloads that use the compiler's arena allocator
//

// I wanted to make the second argument optional, with default = CMK_Unknown, but that
// caused these to be ambiguous with the global placement new operators.
void* __cdecl operator new(size_t n, Compiler* context, CompMemKind cmk);
void* __cdecl operator new[](size_t n, Compiler* context, CompMemKind cmk);

// Requires the definitions of "operator new" so including "LoopCloning.h" after the definitions.
#include "loopcloning.h"

/*****************************************************************************/

/* This is included here and not earlier as it needs the definition of "CSE"
 * which is defined in the section above */

/*****************************************************************************/

unsigned genLog2(unsigned value);
unsigned genLog2(unsigned __int64 value);

var_types genActualType(var_types type);
var_types genUnsignedType(var_types type);
var_types genSignedType(var_types type);

unsigned ReinterpretHexAsDecimal(unsigned);

/*****************************************************************************/

const unsigned FLG_CCTOR = (CORINFO_FLG_CONSTRUCTOR | CORINFO_FLG_STATIC);

#ifdef DEBUG
const int BAD_STK_OFFS = 0xBAADF00D; // for LclVarDsc::lvStkOffs
#endif

// The following holds the Local var info (scope information)
typedef const char* VarName; // Actual ASCII string
struct VarScopeDsc
{
    unsigned vsdVarNum; // (remapped) LclVarDsc number
    unsigned vsdLVnum;  // 'which' in eeGetLVinfo().
                        // Also, it is the index of this entry in the info.compVarScopes array,
                        // which is useful since the array is also accessed via the
                        // compEnterScopeList and compExitScopeList sorted arrays.

    IL_OFFSET vsdLifeBeg; // instr offset of beg of life
    IL_OFFSET vsdLifeEnd; // instr offset of end of life

#ifdef DEBUG
    VarName vsdName; // name of the var
#endif
};

// This class stores information associated with a LclVar SSA definition.
class LclSsaVarDsc
{
    // The basic block where the definition occurs. Definitions of uninitialized variables
    // are considered to occur at the start of the first basic block (fgFirstBB).
    //
    // TODO-Cleanup: In the case of uninitialized variables the block is set to nullptr by
    // SsaBuilder and changed to fgFirstBB during value numbering. It would be useful to
    // investigate and perhaps eliminate this rather unexpected behavior.
    BasicBlock* m_block;
    // The GT_ASG node that generates the definition, or nullptr for definitions
    // of uninitialized variables.
    GenTreeOp* m_asg;

public:
    LclSsaVarDsc() : m_block(nullptr), m_asg(nullptr)
    {
    }

    LclSsaVarDsc(BasicBlock* block, GenTreeOp* asg) : m_block(block), m_asg(asg)
    {
        assert((asg == nullptr) || asg->OperIs(GT_ASG));
    }

    BasicBlock* GetBlock() const
    {
        return m_block;
    }

    void SetBlock(BasicBlock* block)
    {
        m_block = block;
    }

    GenTreeOp* GetAssignment() const
    {
        return m_asg;
    }

    void SetAssignment(GenTreeOp* asg)
    {
        assert((asg == nullptr) || asg->OperIs(GT_ASG));
        m_asg = asg;
    }

    ValueNumPair m_vnPair;
};

// This class stores information associated with a memory SSA definition.
class SsaMemDef
{
public:
    ValueNumPair m_vnPair;
};

//------------------------------------------------------------------------
// SsaDefArray: A resizable array of SSA definitions.
//
// Unlike an ordinary resizable array implementation, this allows only element
// addition (by calling AllocSsaNum) and has special handling for RESERVED_SSA_NUM
// (basically it's a 1-based array). The array doesn't impose any particular
// requirements on the elements it stores and AllocSsaNum forwards its arguments
// to the array element constructor, this way the array supports both LclSsaVarDsc
// and SsaMemDef elements.
//
template <typename T>
class SsaDefArray
{
    T*       m_array;
    unsigned m_arraySize;
    unsigned m_count;

    static_assert_no_msg(SsaConfig::RESERVED_SSA_NUM == 0);
    static_assert_no_msg(SsaConfig::FIRST_SSA_NUM == 1);

    // Get the minimum valid SSA number.
    unsigned GetMinSsaNum() const
    {
        return SsaConfig::FIRST_SSA_NUM;
    }

    // Increase (double) the size of the array.
    void GrowArray(CompAllocator alloc)
    {
        unsigned oldSize = m_arraySize;
        unsigned newSize = max(2, oldSize * 2);

        T* newArray = alloc.allocate<T>(newSize);

        for (unsigned i = 0; i < oldSize; i++)
        {
            newArray[i] = m_array[i];
        }

        m_array     = newArray;
        m_arraySize = newSize;
    }

public:
    // Construct an empty SsaDefArray.
    SsaDefArray() : m_array(nullptr), m_arraySize(0), m_count(0)
    {
    }

    // Reset the array (used only if the SSA form is reconstructed).
    void Reset()
    {
        m_count = 0;
    }

    // Allocate a new SSA number (starting with SsaConfig::FIRST_SSA_NUM).
    template <class... Args>
    unsigned AllocSsaNum(CompAllocator alloc, Args&&... args)
    {
        if (m_count == m_arraySize)
        {
            GrowArray(alloc);
        }

        unsigned ssaNum    = GetMinSsaNum() + m_count;
        m_array[m_count++] = T(std::forward<Args>(args)...);

        // Ensure that the first SSA number we allocate is SsaConfig::FIRST_SSA_NUM
        assert((ssaNum == SsaConfig::FIRST_SSA_NUM) || (m_count > 1));

        return ssaNum;
    }

    // Get the number of SSA definitions in the array.
    unsigned GetCount() const
    {
        return m_count;
    }

    // Get a pointer to the SSA definition at the specified index.
    T* GetSsaDefByIndex(unsigned index)
    {
        assert(index < m_count);
        return &m_array[index];
    }

    // Check if the specified SSA number is valid.
    bool IsValidSsaNum(unsigned ssaNum) const
    {
        return (GetMinSsaNum() <= ssaNum) && (ssaNum < (GetMinSsaNum() + m_count));
    }

    // Get a pointer to the SSA definition associated with the specified SSA number.
    T* GetSsaDef(unsigned ssaNum)
    {
        assert(ssaNum != SsaConfig::RESERVED_SSA_NUM);
        return GetSsaDefByIndex(ssaNum - GetMinSsaNum());
    }
};

enum RefCountState
{
    RCS_INVALID, // not valid to get/set ref counts
    RCS_EARLY,   // early counts for struct promotion and struct passing
    RCS_NORMAL,  // normal ref counts (from lvaMarkRefs onward)
};

class LclVarDsc
{
public:
    LclVarDsc()
        : _lvArgReg(REG_STK)
#if FEATURE_MULTIREG_ARGS
        , _lvOtherArgReg(REG_STK)
#endif
    {
        // It is expected that the memory allocated for LclVarDsc is already zeroed.
        assert(lvType == TYP_UNDEF);
        assert(lvClassHnd == NO_CLASS_HANDLE);
#if ASSERTION_PROP
        assert(lvRefBlks == BlockSetOps::UninitVal());
#endif
    }

    var_types lvType;

    unsigned char lvIsParam : 1;           // is this a parameter?
    unsigned char lvIsRegArg : 1;          // is this an argument that was passed by register?
    unsigned char lvFramePointerBased : 1; // 0 = off of REG_SPBASE (e.g., ESP), 1 = off of REG_FPBASE (e.g., EBP)

    unsigned char lvOnFrame : 1;  // (part of) the variable lives on the frame
    unsigned char lvRegister : 1; // assigned to live in a register? For RyuJIT backend, this is only set if the
                                  // variable is in the same register for the entire function.
    unsigned char lvTracked : 1;  // is this a tracked variable?
    bool          lvTrackedNonStruct()
    {
        return lvTracked && lvType != TYP_STRUCT;
    }
    unsigned char lvPinned : 1; // is this a pinned variable?

    unsigned char lvMustInit : 1;    // must be initialized
    unsigned char lvAddrExposed : 1; // The address of this variable is "exposed" -- passed as an argument, stored in a
                                     // global location, etc.
                                     // We cannot reason reliably about the value of the variable.
    unsigned char lvDoNotEnregister : 1; // Do not enregister this variable.
    unsigned char lvFieldAccessed : 1;   // The var is a struct local, and a field of the variable is accessed.  Affects
                                         // struct promotion.
    unsigned char lvLiveInOutOfHndlr : 1; // The variable is live in or out of an exception handler, and therefore must
                                          // be on the stack (at least at those boundaries.)

    unsigned char lvInSsa : 1; // The variable is in SSA form (set by SsaBuilder)

#ifdef DEBUG
    // These further document the reasons for setting "lvDoNotEnregister".  (Note that "lvAddrExposed" is one of the
    // reasons;
    // also, lvType == TYP_STRUCT prevents enregistration.  At least one of the reasons should be true.
    unsigned char lvLclFieldExpr : 1;    // The variable is not a struct, but was accessed like one (e.g., reading a
                                         // particular byte from an int).
    unsigned char lvLclBlockOpAddr : 1;  // The variable was written to via a block operation that took its address.
    unsigned char lvLiveAcrossUCall : 1; // The variable is live across an unmanaged call.
#endif
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

#if OPT_BOOL_OPS
    unsigned char lvIsBoolean : 1; // set if variable is boolean
#endif
    unsigned char lvSingleDef : 1; // variable has a single def
                                   // before lvaMarkLocalVars: identifies ref type locals that can get type updates
                                   // after lvaMarkLocalVars: identifies locals that are suitable for optAddCopies

    unsigned char lvEhWriteThruCandidate : 1; // variable has a single def and hence is a register candidate if
                                              // if it is an EH variable

    unsigned char lvDisqualifyForEhWriteThru : 1; // tracks variable that are disqualified from register candidancy

#if ASSERTION_PROP
    unsigned char lvDisqualify : 1;   // variable is no longer OK for add copy optimization
    unsigned char lvVolatileHint : 1; // hint for AssertionProp
#endif

#ifndef TARGET_64BIT
    unsigned char lvStructDoubleAlign : 1; // Must we double align this struct?
#endif                                     // !TARGET_64BIT
#ifdef TARGET_64BIT
    unsigned char lvQuirkToLong : 1; // Quirk to allocate this LclVar as a 64-bit long
#endif
#ifdef TARGET_AMD64
    unsigned char lvQuirkPPPStuct : 1;
#endif
#ifdef DEBUG
    unsigned char lvKeepType : 1;       // Don't change the type of this variable
    unsigned char lvNoLclFldStress : 1; // Can't apply local field stress on this one
#endif
    unsigned char lvIsPtr : 1; // Might this be used in an address computation? (used by buffer overflow security
                               // checks)
    unsigned char lvIsUnsafeBuffer : 1; // Does this contain an unsafe buffer requiring buffer overflow security checks?
    unsigned char lvPromoted : 1; // True when this local is a promoted struct, a normed struct, or a "split" long on a
                                  // 32-bit target.  For implicit byref parameters, this gets hijacked between
    // lvaRetypeImplicitByRefParams and lvaDemoteImplicitByRefParams to indicate whether
    // references to the arg are being rewritten as references to a promoted shadow local.
    unsigned char lvIsStructField : 1;     // Is this local var a field of a promoted struct local?
    unsigned char lvOverlappingFields : 1; // True when we have a struct with possibly overlapping fields
    unsigned char lvContainsHoles : 1;     // True when we have a promoted struct that contains holes
    unsigned char lvCustomLayout : 1;      // True when this struct has "CustomLayout"

    unsigned char lvIsMultiRegArg : 1; // true if this is a multireg LclVar struct used in an argument context
    unsigned char lvIsMultiRegRet : 1; // true if this is a multireg LclVar struct assigned from a multireg call

#ifdef FEATURE_HFA_FIELDS_PRESENT
    unsigned char m_isHfa : 1;
#endif

    unsigned char lvLRACandidate : 1; // Tracked for linear scan register allocation purposes

#ifdef FEATURE_SIMD
    unsigned char lvUsedInSIMDIntrinsic : 1; // This tells lclvar is used for simd intrinsic
    var_types     lvBaseType : 5;            // Note: this only packs because var_types is a typedef of unsigned char
#endif                                       // FEATURE_SIMD
    unsigned char lvRegStruct : 1;           // This is a reg-sized non-field-addressed struct.

    unsigned char lvClassIsExact : 1;              // lvClassHandle is the exact type
    INDEBUG(unsigned char lvClassInfoUpdated : 1;) // true if this var has updated class handle or exactness

    unsigned char lvImplicitlyReferenced : 1; // true if there are non-IR references to this local (prolog, epilog, gc,
                                              // eh)

    unsigned char lvSuppressedZeroInit : 1; // local needs zero init if we transform tail call to loop

    unsigned char lvHasExplicitInit : 1; // The local is explicitly initialized and doesn't need zero initialization in
                                         // the prolog. If the local has gc pointers, there are no gc-safe points
                                         // between the prolog and the explicit initialization.

    // TODO-MIKE-Cleanup/Fix: This is pretty much bogus. Only VN Copy Prop uses this and for the wrong reasons.
    // It assumes that if this is set then the local is live, because "this" is supposed to always be live.
    // Except that isn't really true, "this" is always live only in certain methods (e.g. those that need
    // it for the generic context). Also, if "this" is stored to, a copy of "this" is created and lvIsThisPtr
    // is set on that copy, not on the original "this" local. And that copy doesn't have the same "always live"
    // behavior as the "this" param itself.
    // This is primarily a cleanup issue, LclVarDsc already contains too much information and the last thing
    // it needs is such bogus info.
    // It's also a bug but it requires rather exotic IL code to reproduce it (see copy-prop-test.il test).
    unsigned char lvIsThisPtr : 1;

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

    bool IsPromoted() const
    {
        return lvPromoted;
    }

    bool IsPromotedField() const
    {
        return lvIsStructField;
    }

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

    CORINFO_FIELD_HANDLE GetPromotedFieldHandle() const
    {
        assert(lvIsStructField);
        return lvFieldHnd;
    }

#ifdef DEBUG
    unsigned char lvDisqualifyEHVarReason = 'H';
#endif

#if FEATURE_MULTIREG_ARGS
    regNumber lvRegNumForSlot(unsigned slotNum)
    {
        if (slotNum == 0)
        {
            return (regNumber)_lvArgReg;
        }
        else if (slotNum == 1)
        {
            return GetOtherArgReg();
        }
        else
        {
            assert(false && "Invalid slotNum!");
        }

        unreached();
    }
#endif // FEATURE_MULTIREG_ARGS

    bool lvIsHfa() const
    {
#ifdef FEATURE_HFA_FIELDS_PRESENT
        return m_isHfa;
#else
        return false;
#endif
    }

    bool lvIsHfaRegArg() const
    {
#ifdef FEATURE_HFA_FIELDS_PRESENT
        return lvIsRegArg && lvIsHfa();
#else
        return false;
#endif
    }

private:
    regNumberSmall _lvRegNum; // Used to store the register this variable is in (or, the low register of a
                              // register pair). It is set during codegen any time the
                              // variable is enregistered (lvRegister is only set
                              // to non-zero if the variable gets the same register assignment for its entire
                              // lifetime).
#if !defined(TARGET_64BIT)
    regNumberSmall _lvOtherReg; // Used for "upper half" of long var.
#endif                          // !defined(TARGET_64BIT)

    regNumberSmall _lvArgReg; // The (first) register in which this argument is passed.

#if FEATURE_MULTIREG_ARGS
    regNumberSmall _lvOtherArgReg; // Used for the second part of the struct passed in a register.
                                   // Note this is defined but not used by ARM32
#endif                             // FEATURE_MULTIREG_ARGS

    regNumberSmall _lvArgInitReg; // the register into which the argument is moved at entry

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

/////////////////////

#if defined(TARGET_64BIT)

    regNumber GetOtherReg() const
    {
        assert(!"shouldn't get here"); // can't use "unreached();" because it's NORETURN, which causes C4072
                                       // "unreachable code" warnings
        return REG_NA;
    }

    void SetOtherReg(regNumber reg)
    {
        assert(!"shouldn't get here"); // can't use "unreached();" because it's NORETURN, which causes C4072
                                       // "unreachable code" warnings
    }
#else  // !TARGET_64BIT

    regNumber GetOtherReg() const
    {
        return (regNumber)_lvOtherReg;
    }

    void SetOtherReg(regNumber reg)
    {
        _lvOtherReg = (regNumberSmall)reg;
        assert(_lvOtherReg == reg);
    }
#endif // !TARGET_64BIT

    /////////////////////

    regNumber GetArgReg() const
    {
        return (regNumber)_lvArgReg;
    }

    void SetArgReg(regNumber reg)
    {
        _lvArgReg = (regNumberSmall)reg;
        assert(_lvArgReg == reg);
    }

#if FEATURE_MULTIREG_ARGS

    regNumber GetOtherArgReg() const
    {
        return (regNumber)_lvOtherArgReg;
    }

    void SetOtherArgReg(regNumber reg)
    {
        _lvOtherArgReg = (regNumberSmall)reg;
        assert(_lvOtherArgReg == reg);
    }
#endif // FEATURE_MULTIREG_ARGS

    // Is this is a SIMD struct which is used for SIMD intrinsic?
    bool lvIsUsedInSIMDIntrinsic() const
    {
#ifdef FEATURE_SIMD
        return lvUsedInSIMDIntrinsic;
#else
        return false;
#endif
    }

    /////////////////////

    regNumber GetArgInitReg() const
    {
        return (regNumber)_lvArgInitReg;
    }

    void SetArgInitReg(regNumber reg)
    {
        _lvArgInitReg = (regNumberSmall)reg;
        assert(_lvArgInitReg == reg);
    }

    /////////////////////

    bool lvIsRegCandidate() const
    {
        return lvLRACandidate != 0;
    }

    bool lvIsInReg() const
    {
        return lvIsRegCandidate() && (GetRegNum() != REG_STK);
    }

    regMaskTP lvRegMask() const
    {
        regMaskTP regMask = RBM_NONE;
        if (varTypeUsesFloatReg(TypeGet()))
        {
            if (GetRegNum() != REG_STK)
            {
                regMask = genRegMaskFloat(GetRegNum(), TypeGet());
            }
        }
        else
        {
            if (GetRegNum() != REG_STK)
            {
                regMask = genRegMask(GetRegNum());
            }
        }
        return regMask;
    }

    unsigned short lvVarIndex; // variable tracking index

private:
    unsigned short m_lvRefCnt; // unweighted (real) reference count.  For implicit by reference
                               // parameters, this gets hijacked from lvaResetImplicitByRefRefCount
                               // through lvaDemoteImplicitByRefParams, to provide a static
                               // appearance count (computed during address-exposed analysis)
                               // that abiMakeImplicityByRefStructArgCopy consults during global morph
                               // to determine if eliding its copy is legal.

    BasicBlock::weight_t m_lvRefCntWtd; // weighted reference count

public:
    unsigned short lvRefCnt(RefCountState state = RCS_NORMAL) const;
    void incLvRefCnt(unsigned short delta, RefCountState state = RCS_NORMAL);
    void setLvRefCnt(unsigned short newValue, RefCountState state = RCS_NORMAL);

    BasicBlock::weight_t lvRefCntWtd(RefCountState state = RCS_NORMAL) const;
    void incLvRefCntWtd(BasicBlock::weight_t delta, RefCountState state = RCS_NORMAL);
    void setLvRefCntWtd(BasicBlock::weight_t newValue, RefCountState state = RCS_NORMAL);

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

    unsigned lvSize() const;

    unsigned lvSlotNum; // original slot # (if remapped)

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

    CORINFO_FIELD_HANDLE lvFieldHnd; // field handle for promoted struct fields

private:
    ClassLayout* m_layout; // layout info for structs

public:
#if ASSERTION_PROP
    BlockSet   lvRefBlks;          // Set of blocks that contain refs
    Statement* lvDefStmt;          // Pointer to the statement with the single definition
    void       lvaDisqualifyVar(); // Call to disqualify a local variable from use in optAddCopies
#endif
    var_types GetType() const
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
        lvType      = TYP_BLK;
        lvExactSize = size;
    }

    unsigned GetBlockSize() const
    {
        assert(lvType == TYP_BLK);
        return lvExactSize;
    }

    unsigned GetSize() const
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

    var_types TypeGet() const
    {
        return (var_types)lvType;
    }
    bool lvStackAligned() const
    {
        assert(lvIsStructField);
        return ((lvFldOffset % TARGET_POINTER_SIZE) == 0);
    }
    bool lvNormalizeOnLoad() const
    {
        return varTypeIsSmall(TypeGet()) &&
               // lvIsStructField is treated the same as the aliased local, see fgMorphNormalizeLclVarStore.
               (lvIsParam || lvAddrExposed || lvIsStructField);
    }

    bool lvNormalizeOnStore() const
    {
        return varTypeIsSmall(TypeGet()) &&
               // lvIsStructField is treated the same as the aliased local, see fgMorphNormalizeLclVarStore.
               !(lvIsParam || lvAddrExposed || lvIsStructField);
    }

    void incRefCnts(BasicBlock::weight_t weight,
                    Compiler*            pComp,
                    RefCountState        state     = RCS_NORMAL,
                    bool                 propagate = true);
    bool IsFloatRegType() const
    {
        return varTypeUsesFloatReg(lvType) || lvIsHfaRegArg();
    }

    void SetIsHfa(bool isHfa)
    {
#ifdef FEATURE_HFA_FIELDS_PRESENT
        m_isHfa = isHfa;
#endif
    }

    var_types lvaArgType();

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

    SsaDefArray<LclSsaVarDsc> lvPerSsaData;

    // Returns the address of the per-Ssa data for the given ssaNum (which is required
    // not to be the SsaConfig::RESERVED_SSA_NUM, which indicates that the variable is
    // not an SSA variable).
    LclSsaVarDsc* GetPerSsaData(unsigned ssaNum)
    {
        return lvPerSsaData.GetSsaDef(ssaNum);
    }

    var_types GetRegisterType(const GenTreeLclVarCommon* tree) const;

    var_types GetRegisterType() const;

    var_types GetActualRegisterType() const;

    bool IsEnregisterable() const
    {
        return GetRegisterType() != TYP_UNDEF;
    }

    bool CanBeReplacedWithItsField(Compiler* comp) const;

#ifdef DEBUG
public:
    const char* lvReason;

    void PrintVarReg() const
    {
        printf("%s", getRegName(GetRegNum()));
    }
#endif // DEBUG

}; // class LclVarDsc

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

/*****************************************************************************
 *
 *  The following keeps track of temporaries allocated in the stack frame
 *  during code-generation (after register allocation). These spill-temps are
 *  only used if we run out of registers while evaluating a tree.
 *
 *  These are different from the more common temps allocated by lvaGrabTemp().
 */

class TempDsc
{
public:
    TempDsc* tdNext;

private:
    int tdOffs;
#ifdef DEBUG
    static const int BAD_TEMP_OFFSET = 0xDDDDDDDD; // used as a sentinel "bad value" for tdOffs in DEBUG
#endif                                             // DEBUG

    int       tdNum;
    BYTE      tdSize;
    var_types tdType;

public:
    TempDsc(int _tdNum, unsigned _tdSize, var_types _tdType) : tdNum(_tdNum), tdSize((BYTE)_tdSize), tdType(_tdType)
    {
#ifdef DEBUG
        // temps must have a negative number (so they have a different number from all local variables)
        assert(tdNum < 0);
        tdOffs = BAD_TEMP_OFFSET;
#endif // DEBUG
        if (tdNum != _tdNum)
        {
            IMPL_LIMITATION("too many spill temps");
        }
    }

#ifdef DEBUG
    bool tdLegalOffset() const
    {
        return tdOffs != BAD_TEMP_OFFSET;
    }
#endif // DEBUG

    int tdTempOffs() const
    {
        assert(tdLegalOffset());
        return tdOffs;
    }
    void tdSetTempOffs(int offs)
    {
        tdOffs = offs;
        assert(tdLegalOffset());
    }
    void tdAdjustTempOffs(int offs)
    {
        tdOffs += offs;
        assert(tdLegalOffset());
    }

    int tdTempNum() const
    {
        assert(tdNum < 0);
        return tdNum;
    }
    unsigned tdTempSize() const
    {
        return tdSize;
    }
    var_types tdTempType() const
    {
        return tdType;
    }
};

// interface to hide linearscan implementation from rest of compiler
class LinearScanInterface
{
public:
    virtual void doLinearScan()                                = 0;
    virtual void recordVarLocationsAtStartOfBB(BasicBlock* bb) = 0;
    virtual bool willEnregisterLocalVars() const               = 0;
#if TRACK_LSRA_STATS
    virtual void dumpLsraStatsCsv(FILE* file) = 0;
#endif // TRACK_LSRA_STATS
};

LinearScanInterface* getLinearScanAllocator(Compiler* comp);

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

// This enumeration names the phases into which we divide compilation.  The phases should completely
// partition a compilation.
enum Phases
{
#define CompPhaseNameMacro(enum_nm, string_nm, short_nm, hasChildren, parent, measureIR) enum_nm,
#include "compphases.h"
    PHASE_NUMBER_OF
};

extern const char*   PhaseNames[];
extern const char*   PhaseEnums[];
extern const LPCWSTR PhaseShortNames[];

// Specify which checks should be run after each phase
//
enum class PhaseChecks
{
    CHECK_NONE,
    CHECK_ALL
};

// Specify compiler data that a phase might modify
enum class PhaseStatus : unsigned
{
    MODIFIED_NOTHING,
    MODIFIED_EVERYTHING
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
        return ::new (comp, CMK_Unknown) JitTimer(byteCodeSize);
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

    LPFLG_DO_WHILE  = 0x0001, // it's a do-while loop (i.e ENTRY is at the TOP)
    LPFLG_ONE_EXIT  = 0x0002, // the loop has only one exit
    LPFLG_ITER      = 0x0004, // loop of form: for (i = icon or lclVar; test_condition(); i++)
    LPFLG_HOISTABLE = 0x0008, // the loop is in a form that is suitable for hoisting expressions

    LPFLG_CONST      = 0x0010, // loop of form: for (i=icon;i<icon;i++){ ... } - constant loop
    LPFLG_VAR_INIT   = 0x0020, // iterator is initialized with a local var (var # found in lpVarInit)
    LPFLG_CONST_INIT = 0x0040, // iterator is initialized with a constant (found in lpConstInit)
    LPFLG_SIMD_LIMIT = 0x0080, // iterator is compared with vector element count (found in lpConstLimit)

    LPFLG_VAR_LIMIT    = 0x0100, // iterator is compared with a local var (var # found in lpVarLimit)
    LPFLG_CONST_LIMIT  = 0x0200, // iterator is compared with a constant (found in lpConstLimit)
    LPFLG_ARRLEN_LIMIT = 0x0400, // iterator is compared with a.len or a[i].len (found in lpArrLenLimit)
    LPFLG_HAS_PREHEAD  = 0x0800, // lpHead is known to be a preHead for this loop

    LPFLG_REMOVED     = 0x1000, // has been removed from the loop table (unrolled or optimized away)
    LPFLG_DONT_UNROLL = 0x2000, // do not unroll this loop
    LPFLG_ASGVARS_YES = 0x4000, // "lpAsgVars" has been computed
    LPFLG_ASGVARS_INC = 0x8000, // "lpAsgVars" is incomplete -- vars beyond those representable in an AllVarSet
                                // type are assigned to.
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

/*
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX   The big guy. The sections are currently organized as :                  XX
XX                                                                           XX
XX    o  GenTree and BasicBlock                                              XX
XX    o  LclVarsInfo                                                         XX
XX    o  Importer                                                            XX
XX    o  FlowGraph                                                           XX
XX    o  Optimizer                                                           XX
XX    o  RegAlloc                                                            XX
XX    o  EEInterface                                                         XX
XX    o  TempsInfo                                                           XX
XX    o  RegSet                                                              XX
XX    o  GCInfo                                                              XX
XX    o  Instruction                                                         XX
XX    o  ScopeInfo                                                           XX
XX    o  PrologScopeInfo                                                     XX
XX    o  CodeGenerator                                                       XX
XX    o  UnwindInfo                                                          XX
XX    o  Compiler                                                            XX
XX    o  typeInfo                                                            XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

struct HWIntrinsicInfo;

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
    friend class Phase;
    friend class Lowering;
    friend class CSE_DataFlow;
    friend class CSE_Heuristic;
    friend class CodeGenInterface;
    friend class CodeGen;
    friend class LclVarDsc;
    friend class TempDsc;
    friend class LIR;
    friend class ObjectAllocator;
    friend class LocalAddressVisitor;
    friend struct GenTree;
    friend class ClassLayout;
    friend class VNConstPropVisitor;

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
    hashBvGlobalData hbvGlobalData; // Used by the hashBv bitvector package.

#ifdef DEBUG
    bool verbose;
    bool verboseTrees;
    bool shouldUseVerboseTrees();
    bool asciiTrees; // If true, dump trees using only ASCII characters
    bool shouldDumpASCIITrees();
    bool verboseSsa; // If true, produce especially verbose dump output in SSA construction.
    bool shouldUseVerboseSsa();
    bool treesBeforeAfterMorph; // If true, print trees before/after morphing (paired by an intra-compilation id:
    int  morphNum;              // This counts the the trees that have been morphed, allowing us to label each uniquely.
    bool doExtraSuperPmiQueries;
    void makeExtraStructQueries(CORINFO_CLASS_HANDLE structHandle, int level); // Make queries recursively 'level' deep.

    const char* VarNameToStr(VarName name)
    {
        return name;
    }

    DWORD expensiveDebugCheckLevel;
#endif

    //-------------------------------------------------------------------------
    // The following is used for validating format of EH table
    //

    struct EHNodeDsc;
    typedef struct EHNodeDsc* pEHNodeDsc;

    EHNodeDsc* ehnTree; // root of the tree comprising the EHnodes.
    EHNodeDsc* ehnNext; // root of the tree comprising the EHnodes.

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
    unsigned ehMaxHndNestingCount;

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

    // Returns true iff "block" is where control flows if an exception is raised in the
    // try region, and sets "*regionIndex" to the index of the try for the handler.
    // Differs from "IsHandlerBeg" in the case of filters, where this is true for the first
    // block of the filter, but not for the filter's handler.
    bool bbIsExFlowBlock(BasicBlock* block, unsigned* regionIndex);

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

    // This table is useful for memoization of the method above.
    typedef JitHashTable<BasicBlock*, JitPtrKeyFuncs<BasicBlock>, flowList*> BlockToFlowListMap;
    BlockToFlowListMap* m_blockToEHPreds;
    BlockToFlowListMap* GetBlockToEHPreds()
    {
        if (m_blockToEHPreds == nullptr)
        {
            m_blockToEHPreds = new (getAllocator()) BlockToFlowListMap(getAllocator());
        }
        return m_blockToEHPreds;
    }

    void* ehEmitCookie(BasicBlock* block);
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

    bool fgNeedToSortEHTable;

    void verInitEHTree(unsigned numEHClauses);
    void verInsertEhNode(CORINFO_EH_CLAUSE* clause, EHblkDsc* handlerTab);
    void verInsertEhNodeInTree(EHNodeDsc** ppRoot, EHNodeDsc* node);
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

    // For unary opers.
    GenTree* gtNewOperNode(genTreeOps oper, var_types type, GenTree* op1, bool doSimplifications = true);
    GenTree* gtNewAddrNode(GenTree* location, var_types type = TYP_BYREF);

    // For binary opers.
    GenTreeOp* gtNewOperNode(genTreeOps oper, var_types type, GenTree* op1, GenTree* op2);
    GenTreeOp* gtNewCommaNode(GenTree* op1, GenTree* op2, var_types type = TYP_UNDEF);

    GenTreeQmark* gtNewQmarkNode(var_types type, GenTree* cond, GenTree* op1, GenTree* op2);

    GenTree* gtNewLargeOperNode(genTreeOps oper,
                                var_types  type = TYP_I_IMPL,
                                GenTree*   op1  = nullptr,
                                GenTree*   op2  = nullptr);

    GenTreeIntCon* gtNewIconNode(ssize_t value, var_types type = TYP_INT);
    GenTreeIntCon* gtNewIconNode(unsigned fieldOffset, FieldSeqNode* fieldSeq);

    GenTree* gtNewPhysRegNode(regNumber reg, var_types type);

    GenTree* gtNewJmpTableNode();

    GenTree* gtNewIndOfIconHandleNode(var_types indType, size_t value, GenTreeFlags iconFlags, bool isInvariant);

    GenTreeIntCon* gtNewIconHandleNode(size_t value, GenTreeFlags flags, FieldSeqNode* fields = nullptr);

    GenTreeFlags gtTokenToIconFlags(unsigned token);

    GenTree* gtNewIconEmbHndNode(void* value, void* pValue, GenTreeFlags flags, void* compileTimeHandle);

    GenTree* gtNewIconEmbScpHndNode(CORINFO_MODULE_HANDLE scpHnd);
    GenTree* gtNewIconEmbClsHndNode(CORINFO_CLASS_HANDLE clsHnd);
    GenTree* gtNewIconEmbMethHndNode(CORINFO_METHOD_HANDLE methHnd);
    GenTree* gtNewIconEmbFldHndNode(CORINFO_FIELD_HANDLE fldHnd);

    GenTree* gtNewStringLiteralNode(InfoAccessType iat, void* pValue);
    GenTreeIntCon* gtNewStringLiteralLength(GenTreeStrCon* node);

    GenTree* gtNewLconNode(int64_t value);

    GenTree* gtNewDconNode(double value, var_types type = TYP_DOUBLE);

    GenTree* gtNewSconNode(int CPX, CORINFO_MODULE_HANDLE scpHandle);

    GenTree* gtNewZeroConNode(var_types type);

    GenTree* gtNewOneConNode(var_types type);

    GenTreeLclVar* gtNewStoreLclVar(unsigned dstLclNum, GenTree* src);

    GenTreeUnOp* gtNewBitCastNode(var_types type, GenTree* arg);

    void gtInitStructCopyAsg(GenTreeOp* asg);

    GenTreeObj* gtNewObjNode(CORINFO_CLASS_HANDLE structHnd, GenTree* addr);
    GenTreeObj* gtNewObjNode(ClassLayout* layout, GenTree* addr);
    GenTreeObj* gtNewObjNode(var_types type, ClassLayout* layout, GenTree* addr);

    GenTreeCall::Use* gtNewCallArgs(GenTree* node);
    GenTreeCall::Use* gtNewCallArgs(GenTree* node1, GenTree* node2);
    GenTreeCall::Use* gtNewCallArgs(GenTree* node1, GenTree* node2, GenTree* node3);
    GenTreeCall::Use* gtNewCallArgs(GenTree* node1, GenTree* node2, GenTree* node3, GenTree* node4);
    GenTreeCall::Use* gtPrependNewCallArg(GenTree* node, GenTreeCall::Use* args);
    GenTreeCall::Use* gtInsertNewCallArgAfter(GenTree* node, GenTreeCall::Use* after);

    GenTreeCall* gtNewCallNode(gtCallTypes           callType,
                               CORINFO_METHOD_HANDLE handle,
                               var_types             type,
                               GenTreeCall::Use*     args,
                               IL_OFFSETX            ilOffset = BAD_IL_OFFSET);

    GenTreeCall* gtNewIndCallNode(GenTree*          addr,
                                  var_types         type,
                                  GenTreeCall::Use* args,
                                  IL_OFFSETX        ilOffset = BAD_IL_OFFSET);

    GenTreeCall* gtNewHelperCallNode(unsigned helper, var_types type, GenTreeCall::Use* args = nullptr);

    GenTreeCall* gtNewRuntimeLookupHelperCallNode(CORINFO_RUNTIME_LOOKUP* pRuntimeLookup,
                                                  GenTree*                ctxTree,
                                                  void*                   compileTimeHandle);

    GenTreeLclVar* gtNewLclvNode(unsigned lnum, var_types type DEBUGARG(IL_OFFSETX ILoffs = BAD_IL_OFFSET));
    GenTreeLclVar* gtNewLclLNode(unsigned lnum, var_types type DEBUGARG(IL_OFFSETX ILoffs = BAD_IL_OFFSET));

    GenTreeLclVar* gtNewLclVarAddrNode(unsigned lclNum, var_types type = TYP_I_IMPL);
    GenTreeLclFld* gtNewLclFldAddrNode(unsigned      lclNum,
                                       unsigned      lclOffs,
                                       FieldSeqNode* fieldSeq,
                                       var_types     type = TYP_I_IMPL);

#ifdef FEATURE_HW_INTRINSICS
    GenTreeHWIntrinsic* gtNewZeroSimdHWIntrinsicNode(ClassLayout* layout);
    GenTreeHWIntrinsic* gtNewZeroSimdHWIntrinsicNode(var_types type, var_types baseType);

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
    var_types impGetHWIntrinsicBaseTypeFromArg(NamedIntrinsic    intrinsic,
                                               CORINFO_SIG_INFO* sig,
                                               var_types         baseType,
                                               ClassLayout**     argLayout);
#endif // FEATURE_HW_INTRINSICS

    GenTreeLclFld* gtNewLclFldNode(unsigned lnum, var_types type, unsigned offset);
    GenTreeRetExpr* gtNewRetExpr(GenTreeCall* call, var_types type);

    GenTreeField* gtNewFieldRef(var_types typ, CORINFO_FIELD_HANDLE fldHnd, GenTree* obj = nullptr, DWORD offset = 0);

    GenTreeIndex* gtNewArrayIndex(var_types type, GenTree* arr, GenTree* ind);
    GenTreeIndex* gtNewStringIndex(GenTree* arr, GenTree* ind);

    GenTreeArrLen* gtNewArrLen(GenTree* arr, uint8_t lenOffs, BasicBlock* block);
    GenTreeBoundsChk* gtNewArrBoundsChk(GenTree* index, GenTree* length, SpecialCodeKind kind);

    GenTreeIndir* gtNewIndir(var_types typ, GenTree* addr);

    GenTree* gtNewNullCheck(GenTree* addr, BasicBlock* basicBlock);

    void gtChangeOperToNullCheck(GenTree* tree, BasicBlock* block);

    GenTreeOp* gtNewAssignNode(GenTree* dst, GenTree* src);

    GenTree* gtNewTempAssign(unsigned tmp, GenTree* val);

    GenTree* gtNewNothingNode();

    GenTree* gtUnusedValNode(GenTree* expr);

    GenTreeCast* gtNewCastNode(var_types typ, GenTree* op1, bool fromUnsigned, var_types castType);

    GenTreeAllocObj* gtNewAllocObjNode(
        unsigned int helper, bool helperHasSideEffects, CORINFO_CLASS_HANDLE clsHnd, var_types type, GenTree* op1);

    GenTreeAllocObj* gtNewAllocObjNode(CORINFO_RESOLVED_TOKEN* pResolvedToken, bool useParent);

    GenTree* gtNewRuntimeLookup(CORINFO_GENERIC_HANDLE hnd, CorInfoGenericHandleType hndTyp, GenTree* lookupTree);

    GenTreeIndir* gtNewMethodTableLookup(GenTree* obj);

    //------------------------------------------------------------------------
    // Other GenTree functions

    GenTree* gtClone(GenTree* tree, bool complexOK = false);

    // If `tree` is a lclVar with lclNum `varNum`, return an IntCns with value `varVal`; otherwise,
    // create a copy of `tree`, adding specified flags, replacing uses of lclVar `deepVarNum` with
    // IntCnses with value `deepVarVal`.
    GenTree* gtCloneExpr(
        GenTree* tree, GenTreeFlags addFlags, unsigned varNum, int varVal, unsigned deepVarNum, int deepVarVal);

    // Create a copy of `tree`, optionally adding specifed flags, and optionally mapping uses of local
    // `varNum` to int constants with value `varVal`.
    GenTree* gtCloneExpr(GenTree*     tree,
                         GenTreeFlags addFlags = GTF_EMPTY,
                         unsigned     varNum   = BAD_VAR_NUM,
                         int          varVal   = 0)
    {
        return gtCloneExpr(tree, addFlags, varNum, varVal, varNum, varVal);
    }

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

    void gtUpdateSideEffects(Statement* stmt, GenTree* tree);

    void gtUpdateTreeAncestorsSideEffects(GenTree* tree);

    void gtUpdateStmtSideEffects(Statement* stmt);

    void gtUpdateNodeSideEffects(GenTree* tree);

    void gtUpdateNodeOperSideEffects(GenTree* tree);

    // Returns "true" iff the complexity (not formally defined, but first interpretation
    // is #of nodes in subtree) of "tree" is greater than "limit".
    // (This is somewhat redundant with the "GetCostEx()/GetCostSz()" fields, but can be used
    // before they have been set.)
    bool gtComplexityExceeds(GenTree* tree, unsigned limit);

    bool gtCompareTree(GenTree* op1, GenTree* op2);

    GenTree* gtReverseCond(GenTree* tree);

    bool gtHasRef(GenTree* tree, ssize_t lclNum);

    bool gtHasAddressTakenLocals(GenTree* tree);

    unsigned gtSetCallArgsOrder(const GenTreeCall::UseList& args, bool lateArgs, int* callCostEx, int* callCostSz);

    void gtWalkOp(GenTree** op1, GenTree** op2, GenTree* base, bool constOnly);

#ifdef DEBUG
    unsigned gtHashValue(GenTree* tree);

    GenTree* gtWalkOpEffectiveVal(GenTree* op);
#endif

    void gtPrepareCost(GenTree* tree);
    bool gtIsLikelyRegVar(GenTree* tree);

    // Returns true iff the secondNode can be swapped with firstNode.
    bool gtCanSwapOrder(GenTree* firstNode, GenTree* secondNode);

    // Given an address expression, compute its costs and addressing mode opportunities,
    // and mark addressing mode candidates as GTF_DONT_CSE.
    // TODO-Throughput - Consider actually instantiating these early, to avoid
    // having to re-run the algorithm that looks for them (might also improve CQ).
    bool gtMarkAddrMode(GenTree* addr, int* costEx, int* costSz, var_types type);

    unsigned gtSetEvalOrder(GenTree* tree);

    void gtSetStmtInfo(Statement* stmt);

    // Returns "true" iff "node" has any of the side effects in "flags".
    bool gtNodeHasSideEffects(GenTree* node, unsigned flags);

    // Returns "true" iff "tree" or its (transitive) children have any of the side effects in "flags".
    bool gtTreeHasSideEffects(GenTree* tree, unsigned flags);

    // Appends 'expr' in front of 'list'
    //    'list' will typically start off as 'nullptr'
    //    when 'list' is non-null a GT_COMMA node is used to insert 'expr'
    GenTree* gtBuildCommaList(GenTree* list, GenTree* expr);

    void gtExtractSideEffList(GenTree*  expr,
                              GenTree** pList,
                              unsigned  flags      = GTF_SIDE_EFFECT,
                              bool      ignoreRoot = false);

    // Static fields of struct types (and sometimes the types that those are reduced to) are represented by having the
    // static field contain an object pointer to the boxed struct.  This simplifies the GC implementation...but
    // complicates the JIT somewhat.  This predicate returns "true" iff a node with type "fieldNodeType", representing
    // the given "fldHnd", is such an object pointer.
    bool gtIsStaticFieldPtrToBoxedStruct(var_types fieldNodeType, CORINFO_FIELD_HANDLE fldHnd);

    // If returns "true", "addr" may represent the address of a static or instance field
    // (or a field of such a field, in the case of an object field of type struct).
    // If returns "true", then either "*pObj" is set to the object reference,
    // or "*pStatic" is set to the baseAddr or offset to be added to the "*pFldSeq"
    // Only one of "*pObj" or "*pStatic" will be set, the other one will be null.
    // The boolean return value only indicates that "this" *may* be a field address
    // -- the field sequence must also be checked.
    // If it is a field address, the field sequence will be a sequence of length >= 1,
    // starting with an instance or static field, and optionally continuing with struct fields.
    bool optIsFieldAddr(GenTree* addr, GenTree** pObj, GenTree** pStatic, FieldSeqNode** pFldSeq);

    // Requires "indir" to be a GT_IND.
    // Returns true if it is an array index expression. If it returns true, sets *arrayInfo to the
    // array information.
    bool optIsArrayElem(GenTreeIndir* indir, ArrayInfo* arrayInfo);
    bool optIsArrayElemAddr(GenTree* addr, ArrayInfo* arrayInfo);

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
    GenTree*
#ifdef __clang__
        // TODO-Amd64-Unix: Remove this when the clang optimizer is fixed and/or the method implementation is
        // refactored in a simpler code. This is a workaround for a bug in the clang-3.5 optimizer. The issue is that in
        // release build the optimizer is mistyping (or just wrongly decides to use 32 bit operation for a corner case
        // of MIN_LONG) the args of the (ltemp / lval2) to int (it does a 32 bit div operation instead of 64 bit) - see
        // the implementation of the method in gentree.cpp. For the case of lval1 and lval2 equal to MIN_LONG
        // (0x8000000000000000) this results in raising a SIGFPE. The method implementation is rather complex. Disable
        // optimizations for now.
        __attribute__((optnone))
#endif // __clang__
        gtFoldExprConst(GenTree* tree);
    GenTree* gtFoldExprSpecial(GenTree* tree);
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

    GenTree* gtTryRemoveBoxUpstreamEffects(GenTree* tree, BoxRemovalOptions options = BR_REMOVE_AND_NARROW);
    GenTree* gtOptimizeEnumHasFlag(GenTree* thisOp, GenTree* flagOp);

    //-------------------------------------------------------------------------
    // Get the handle, if any.
    CORINFO_CLASS_HANDLE gtGetStructHandle(GenTree* tree);
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
    void gtDispNode(GenTree* tree, IndentStack* indentStack, __in_z const char* msg, bool isLIR);
    int gtDispNodeHeader(GenTree* tree, IndentStack* indentStack, int msgLength);

    void gtDispConst(GenTree* tree);
    void gtDispLeaf(GenTree* tree, IndentStack* indentStack);
    void gtDispNodeName(GenTree* tree);
#if FEATURE_MULTIREG_RET
    unsigned gtDispRegCount(GenTree* tree);
#endif
    void gtDispRegVal(GenTree* tree);
    void gtDispZeroFieldSeq(GenTree* tree);
    void gtDispVN(GenTree* tree);
    void gtDispCommonEndLine(GenTree* tree);

    enum IndentInfo
    {
        IINone,
        IIArc,
        IIArcTop,
        IIArcBottom,
        IIEmbedded,
        IIError,
        IndentInfoCount
    };
    void gtDispChild(GenTree*             child,
                     IndentStack*         indentStack,
                     IndentInfo           arcType,
                     __in_opt const char* msg     = nullptr,
                     bool                 topOnly = false);
    void gtDispTree(GenTree*             tree,
                    IndentStack*         indentStack = nullptr,
                    __in_opt const char* msg         = nullptr,
                    bool                 topOnly     = false,
                    bool                 isLIR       = false);
    void gtGetLclVarNameInfo(unsigned lclNum, const char** ilKindOut, const char** ilNameOut, unsigned* ilNumOut);
    int gtGetLclVarName(unsigned lclNum, char* buf, unsigned buf_remaining);
    char* gtGetLclVarName(unsigned lclNum);
    void gtDispLclVar(unsigned lclNum, bool padForBiggestDisp = true);
    void gtDispLclVarStructType(unsigned lclNum);
    void gtDispClassLayout(ClassLayout* layout, var_types type);
    void gtDispStmt(Statement* stmt, const char* msg = nullptr);
    void gtDispBlockStmts(BasicBlock* block);
    void gtGetCallArgMsg(GenTreeCall* call, GenTree* arg, unsigned argNum, char* buf, unsigned bufLength);
    void gtGetCallArgMsg(GenTreeCall* call, CallArgInfo* argInfo, GenTree* arg, char* buf, unsigned bufLength);
    void gtDispFieldSeq(FieldSeqNode* pfsn);

    void gtDispRange(LIR::ReadOnlyRange const& range);

    void gtDispTreeRange(LIR::Range& containingRange, GenTree* tree);

    void gtDispLIRNode(GenTree* node, const char* prefixMsg = nullptr);
#endif

    // For tree walks

    enum fgWalkResult
    {
        WALK_CONTINUE,
        WALK_SKIP_SUBTREES,
        WALK_ABORT
    };
    struct fgWalkData;
    typedef fgWalkResult(fgWalkPreFn)(GenTree** pTree, fgWalkData* data);
    typedef fgWalkResult(fgWalkPostFn)(GenTree** pTree, fgWalkData* data);

#ifdef DEBUG
    static fgWalkPreFn gtAssertColonCond;
#endif
    static fgWalkPreFn gtMarkColonCond;
    static fgWalkPreFn gtClearColonCond;

    struct FindLinkData
    {
        GenTree*  nodeToFind;
        GenTree** result;
        GenTree*  parent;
    };

    FindLinkData gtFindLink(Statement* stmt, GenTree* node);
    bool gtHasCatchArg(GenTree* tree);

    typedef ArrayStack<GenTree*> GenTreeStack;

    static bool gtHasCallOnStack(GenTreeStack* parentStack);

//=========================================================================
// BasicBlock functions
#ifdef DEBUG
    // This is a debug flag we will use to assert when creating block during codegen
    // as this interferes with procedure splitting. If you know what you're doing, set
    // it to true before creating the block. (DEBUG only)
    bool fgSafeBasicBlockCreation;
#endif

    BasicBlock* bbNewBasicBlock(BBjumpKinds jumpKind);

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

    //
    // For both PROMOTION_TYPE_NONE and PROMOTION_TYPE_DEPENDENT the struct will
    // be placed in the stack frame and it's fields must be laid out sequentially.
    //
    // For PROMOTION_TYPE_INDEPENDENT each of the struct's fields is replaced by
    //  a local variable that can be enregistered or placed in the stack frame.
    //  The fields do not need to be laid out sequentially
    //
    enum lvaPromotionType
    {
        PROMOTION_TYPE_NONE,        // The struct local is not promoted
        PROMOTION_TYPE_INDEPENDENT, // The struct local is promoted,
                                    //   and its field locals are independent of its parent struct local.
        PROMOTION_TYPE_DEPENDENT    // The struct local is promoted,
                                    //   but its field locals depend on its parent struct local.
    };

    /*****************************************************************************/

    enum FrameLayoutState
    {
        NO_FRAME_LAYOUT,
        INITIAL_FRAME_LAYOUT,
        PRE_REGALLOC_FRAME_LAYOUT,
        REGALLOC_FRAME_LAYOUT,
        TENTATIVE_FRAME_LAYOUT,
        FINAL_FRAME_LAYOUT
    };

public:
    RefCountState lvaRefCountState; // Current local ref count state

    bool lvaLocalVarRefCounted() const
    {
        return lvaRefCountState == RCS_NORMAL;
    }

    bool     lvaTrackedFixed; // true: We cannot add new 'tracked' variable
    unsigned lvaCount;        // total number of locals, which includes function arguments,
                              // special arguments, IL local variables, and JIT temporary variables

    LclVarDsc* lvaTable;     // variable descriptor table
    unsigned   lvaTableSize; // lvaTable size (>= lvaCount)

    unsigned lvaTrackedCount;             // actual # of locals being tracked
    unsigned lvaTrackedCountInSizeTUnits; // min # of size_t's sufficient to hold a bit for all the locals being tracked

#ifdef DEBUG
    VARSET_TP lvaTrackedVars; // set of tracked variables
#endif
#ifndef TARGET_64BIT
    VARSET_TP lvaLongVars; // set of long (64-bit) variables
#endif
    VARSET_TP lvaFloatVars; // set of floating-point (32-bit and 64-bit) variables

    unsigned lvaCurEpoch; // VarSets are relative to a specific set of tracked var indices.
                          // It that changes, this changes.  VarSets from different epochs
                          // cannot be meaningfully combined.

    unsigned GetCurLVEpoch()
    {
        return lvaCurEpoch;
    }

    // reverse map of tracked number to var number
    unsigned  lvaTrackedToVarNumSize;
    unsigned* lvaTrackedToVarNum;

#if DOUBLE_ALIGN
#ifdef DEBUG
    // # of procs compiled a with double-aligned stack
    static unsigned s_lvaDoubleAlignedProcsCount;
#endif
#endif

    // Getters and setters for address-exposed and do-not-enregister local var properties.
    bool lvaVarAddrExposed(unsigned varNum);
    void lvaSetVarAddrExposed(unsigned varNum);
    void lvaSetVarLiveInOutOfHandler(unsigned varNum);
    bool lvaVarDoNotEnregister(unsigned varNum);

    bool lvaEnregEHVars;
    bool lvaEnregMultiRegVars;

#ifdef DEBUG
    // Reasons why we can't enregister.  Some of these correspond to debug properties of local vars.
    enum DoNotEnregisterReason
    {
        DNER_AddrExposed,
        DNER_IsStruct,
        DNER_LocalField,
        DNER_LiveInOutOfHandler,
        DNER_LiveAcrossUnmanagedCall,
        DNER_BlockOp,     // Is read or written via a block operation that explicitly takes the address.
        DNER_IsStructArg, // Is a struct passed as an argument in a way that requires a stack location.
        DNER_DepField,    // It is a field of a dependently promoted struct
        DNER_NoRegVars,   // opts.compFlags & CLFLG_REGVAR is not set
        DNER_MinOptsGC,   // It is a GC Ref and we are compiling MinOpts
#if !defined(TARGET_64BIT)
        DNER_LongParamField, // It is a decomposed field of a long parameter.
#endif
#ifdef JIT32_GCENCODER
        DNER_PinningRef,
#endif
    };

#endif
    void lvaSetVarDoNotEnregister(unsigned varNum DEBUGARG(DoNotEnregisterReason reason));

    unsigned lvaVarargsHandleArg;
#ifdef TARGET_X86
    unsigned lvaVarargsBaseOfStkArgs; // Pointer (computed based on incoming varargs handle) to the start of the stack
                                      // arguments
#endif                                // TARGET_X86

    unsigned lvaInlinedPInvokeFrameVar; // variable representing the InlinedCallFrame
    unsigned lvaReversePInvokeFrameVar; // variable representing the reverse PInvoke frame
#if FEATURE_FIXED_OUT_ARGS
    unsigned lvaPInvokeFrameRegSaveVar; // variable representing the RegSave for PInvoke inlining.
#endif
    unsigned lvaMonAcquired; // boolean variable introduced into in synchronized methods
                             // that tracks whether the lock has been taken

    unsigned lvaArg0Var; // The lclNum of arg0. Normally this will be info.compThisArg.
                         // However, if there is a "ldarga 0" or "starg 0" in the IL,
                         // we will redirect all "ldarg(a) 0" and "starg 0" to this temp.

#if FEATURE_FIXED_OUT_ARGS
    unsigned            lvaOutgoingArgSpaceVar;  // TYP_BLK local for fixed outgoing argument space
    PhasedVar<unsigned> lvaOutgoingArgSpaceSize; // size of fixed outgoing argument space
#endif                                           // FEATURE_FIXED_OUT_ARGS

    static unsigned GetOutgoingArgByteSize(unsigned sizeWithoutPadding)
    {
        return roundUp(sizeWithoutPadding, TARGET_POINTER_SIZE);
    }

    // Variable representing the return address. The helper-based tailcall
    // mechanism passes the address of the return address to a runtime helper
    // where it is used to detect tail-call chains.
    unsigned lvaRetAddrVar;

#if defined(DEBUG) && defined(TARGET_XARCH)

    unsigned lvaReturnSpCheck; // Stores SP to confirm it is not corrupted on return.

#endif // defined(DEBUG) && defined(TARGET_XARCH)

#if defined(DEBUG) && defined(TARGET_X86)

    unsigned lvaCallSpCheck; // Stores SP to confirm it is not corrupted after every call.

#endif // defined(DEBUG) && defined(TARGET_X86)

#if (defined(TARGET_AMD64) && !defined(UNIX_AMD64_ABI)) || defined(TARGET_ARM64)
    bool lvaHasImplicitByRefParams;
#endif

    bool lvaGenericsContextInUse;

    bool lvaKeepAliveAndReportThis(); // Synchronized instance method of a reference type, or
                                      // CORINFO_GENERICS_CTXT_FROM_THIS?
    bool lvaReportParamTypeArg();     // Exceptions and CORINFO_GENERICS_CTXT_FROM_PARAMTYPEARG?

//-------------------------------------------------------------------------
// All these frame offsets are inter-related and must be kept in sync

#if !defined(FEATURE_EH_FUNCLETS)
    // This is used for the callable handlers
    unsigned lvaShadowSPslotsVar; // TYP_BLK variable for all the shadow SP slots
#endif                            // FEATURE_EH_FUNCLETS

    int lvaCachedGenericContextArgOffs;
    int lvaCachedGenericContextArgOffset(); // For CORINFO_CALLCONV_PARAMTYPE and if generic context is passed as
                                            // THIS pointer

#ifdef JIT32_GCENCODER

    unsigned lvaLocAllocSPvar; // variable which stores the value of ESP after the the last alloca/localloc

#endif // JIT32_GCENCODER

    unsigned lvaNewObjArrayArgs; // variable with arguments for new MD array helper

    // TODO-Review: Prior to reg predict we reserve 24 bytes for Spill temps.
    //              after the reg predict we will use a computed maxTmpSize
    //              which is based upon the number of spill temps predicted by reg predict
    //              All this is necessary because if we under-estimate the size of the spill
    //              temps we could fail when encoding instructions that reference stack offsets for ARM.
    //
    // Pre codegen max spill temp size.
    static const unsigned MAX_SPILL_TEMP_SIZE = 24;

    //-------------------------------------------------------------------------

    unsigned lvaGetMaxSpillTempSize();
#ifdef TARGET_ARM
    bool lvaIsPreSpilled(unsigned lclNum, regMaskTP preSpillMask);
#endif // TARGET_ARM
    void lvaAssignFrameOffsets(FrameLayoutState curState);
    void lvaFixVirtualFrameOffsets();
    void lvaUpdateArgWithInitialReg(LclVarDsc* varDsc);
    void lvaUpdateArgsWithInitialReg();
    void lvaAssignVirtualFrameOffsetsToArgs();
#ifdef UNIX_AMD64_ABI
    int lvaAssignVirtualFrameOffsetToArg(unsigned lclNum, unsigned argSize, int argOffs, int* callerArgOffset);
#else  // !UNIX_AMD64_ABI
    int lvaAssignVirtualFrameOffsetToArg(unsigned lclNum, unsigned argSize, int argOffs);
#endif // !UNIX_AMD64_ABI
    void lvaAssignVirtualFrameOffsetsToLocals();
    int lvaAllocLocalAndSetVirtualOffset(unsigned lclNum, unsigned size, int stkOffs);
#ifdef TARGET_AMD64
    // Returns true if compCalleeRegsPushed (including RBP if used as frame pointer) is even.
    bool lvaIsCalleeSavedIntRegCountEven();
#endif
    void lvaAlignFrame();
    void lvaAssignFrameOffsetsToPromotedStructs();
    int lvaAllocateTemps(int stkOffs, bool mustDoubleAlign);

#ifdef DEBUG
    void lvaDumpRegLocation(unsigned lclNum);
    void lvaDumpFrameLocation(unsigned lclNum);
    void lvaDumpEntry(unsigned lclNum, FrameLayoutState curState, size_t refCntWtdWidth = 6);
    void lvaTableDump(FrameLayoutState curState = NO_FRAME_LAYOUT); // NO_FRAME_LAYOUT means use the current frame
                                                                    // layout state defined by lvaDoneFrameLayout
#endif

// Limit frames size to 1GB. The maximum is 2GB in theory - make it intentionally smaller
// to avoid bugs from borderline cases.
#define MAX_FrameSize 0x3FFFFFFF
    void lvaIncrementFrameSize(unsigned size);

    unsigned lvaFrameSize(FrameLayoutState curState);

    // Returns the caller-SP-relative offset for the SP/FP relative offset determined by FP based.
    int lvaToCallerSPRelativeOffset(int offs, bool isFpBased) const;

    // Returns the caller-SP-relative offset for the local variable "varNum."
    int lvaGetCallerSPRelativeOffset(unsigned varNum);

    int lvaToInitialSPRelativeOffset(unsigned offset, bool isFpBased);
    int lvaGetInitialSPRelativeOffset(unsigned varNum);

    // True if this is an OSR compilation and this local is potentially
    // located on the original method stack frame.
    bool lvaIsOSRLocal(unsigned varNum);

    //------------------------ For splitting types ----------------------------

    void lvaInitTypeRef();

    void lvaInitArgs(InitVarDscInfo* varDscInfo);
    void lvaInitThisPtr(InitVarDscInfo* varDscInfo);
    void lvaInitRetBuffArg(InitVarDscInfo* varDscInfo, bool useFixedRetBufReg);
    void lvaInitUserArgs(InitVarDscInfo* varDscInfo, unsigned skipArgs, unsigned takeArgs);
    void lvaInitGenericsCtxt(InitVarDscInfo* varDscInfo);
    void lvaInitVarArgsHandle(InitVarDscInfo* varDscInfo);

    void lvaInitVarDsc(LclVarDsc* varDsc, unsigned varNum, CorInfoType corInfoType, CORINFO_CLASS_HANDLE typeHnd);

    void lvaInit();

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

    unsigned lvaLclSize(unsigned varNum);

    bool lvaHaveManyLocals() const;

    unsigned lvaNewTemp(var_types type, bool shortLifetime DEBUGARG(const char* reason));
    unsigned lvaNewTemp(ClassLayout* layout, bool shortLifetime DEBUGARG(const char* reason));
    unsigned lvaNewTemp(CORINFO_CLASS_HANDLE classHandle, bool shortLifetime DEBUGARG(const char* reason));
    unsigned lvaNewTemp(GenTree* tree, bool shortLifetime DEBUGARG(const char* reason));

    unsigned lvaGrabTemp(bool shortLifetime DEBUGARG(const char* reason));
    unsigned lvaGrabTemps(unsigned count DEBUGARG(const char* reason));
    unsigned lvaGrabTempWithImplicitUse(bool shortLifetime DEBUGARG(const char* reason));

    void lvaResizeTable(unsigned newSize);

    void lvaSortByRefCount();

    void lvaMarkLocalVars(); // Local variable ref-counting
    void lvaComputeRefCounts(bool isRecompute, bool setSlotNumbers);
    void lvaMarkLocalVars(BasicBlock* block, bool isRecompute);

    void lvaAllocOutgoingArgSpaceVar(); // Set up lvaOutgoingArgSpaceVar

    VARSET_VALRET_TP lvaStmtLclMask(Statement* stmt);

#ifdef DEBUG
    struct lvaStressLclFldArgs
    {
        Compiler* m_pCompiler;
        bool      m_bFirstPass;
    };

    static fgWalkPreFn lvaStressLclFldCB;
    void               lvaStressLclFld();

    void lvaDispVarSet(VARSET_VALARG_TP set, VARSET_VALARG_TP allVars);
    void lvaDispVarSet(VARSET_VALARG_TP set);

#ifdef TARGET_ARMARCH
    unsigned lvaStressLclFldGetAlignment(GenTreeLclVar* lclNode);
#endif
#endif

#ifdef TARGET_ARM
    int lvaFrameAddress(int varNum, bool mustBeFPBased, regNumber* pBaseReg, int addrModeOffset, bool isFloatUsage);
#else
    int lvaFrameAddress(int varNum, bool* pFPbased);
#endif

    bool lvaIsParameter(unsigned varNum);
    bool lvaIsRegArgument(unsigned varNum);
    bool lvaIsOriginalThisArg(unsigned varNum); // Is this varNum the original this argument?
    bool lvaIsOriginalThisReadOnly();           // return true if there is no place in the code
                                                // that writes to arg0

    bool lvaIsImplicitByRefLocal(unsigned varNum)
    {
        return lvaGetDesc(varNum)->IsImplicitByRefParam();
    }

    bool lvaIsMultiRegStructParam(LclVarDsc* lcl);

    void lvaSetStruct(unsigned lclNum, ClassLayout* layout, bool checkUnsafeBuffer);
    void lvaSetStruct(unsigned lclNum, CORINFO_CLASS_HANDLE classHandle, bool checkUnsafeBuffer);

    // If the local is TYP_REF, set or update the associated class information.
    void lvaSetClass(unsigned varNum, CORINFO_CLASS_HANDLE clsHnd, bool isExact = false);
    void lvaSetClass(unsigned varNum, GenTree* tree, CORINFO_CLASS_HANDLE stackHandle = nullptr);
    void lvaUpdateClass(unsigned varNum, CORINFO_CLASS_HANDLE clsHnd, bool isExact = false);
    void lvaUpdateClass(unsigned varNum, GenTree* tree, CORINFO_CLASS_HANDLE stackHandle = nullptr);

#define MAX_NumOfFieldsInPromotableStruct 4 // Maximum number of fields in promotable struct

    // Info about struct type fields.
    struct lvaStructFieldInfo
    {
        CORINFO_FIELD_HANDLE fldHnd;
        unsigned             fldOffset;
        unsigned             fldSize;
        CORINFO_CLASS_HANDLE fldTypeHnd;
        var_types            fldType;

        lvaStructFieldInfo() : fldHnd(nullptr), fldOffset(0), fldSize(0), fldTypeHnd(nullptr), fldType(TYP_UNDEF)
        {
        }
    };

    // Info about a struct type, instances of which may be candidates for promotion.
    struct lvaStructPromotionInfo
    {
        CORINFO_CLASS_HANDLE typeHnd;
        bool                 canPromote;
        bool                 containsHoles;
        bool                 customLayout;
        bool                 fieldsSorted;
        unsigned char        fieldCnt;
        lvaStructFieldInfo   fields[MAX_NumOfFieldsInPromotableStruct];

        lvaStructPromotionInfo(CORINFO_CLASS_HANDLE typeHnd = nullptr)
            : typeHnd(typeHnd)
            , canPromote(false)
            , containsHoles(false)
            , customLayout(false)
            , fieldsSorted(false)
            , fieldCnt(0)
        {
        }
    };

    // This class is responsible for checking validity and profitability of struct promotion.
    // If it is both legal and profitable, then TryPromoteStructVar promotes the struct and initializes
    // nessesary information for fgMorphStructField to use.
    class StructPromotionHelper
    {
    public:
        StructPromotionHelper(Compiler* compiler);

        bool CanPromoteStructType(CORINFO_CLASS_HANDLE typeHnd);
        bool TryPromoteStructVar(unsigned lclNum);
        void Clear()
        {
            structPromotionInfo.typeHnd = NO_CLASS_HANDLE;
        }

#ifdef DEBUG
        void CheckRetypedAsScalar(CORINFO_FIELD_HANDLE fieldHnd, var_types requestedType);
#endif // DEBUG

#ifdef TARGET_ARM
        bool GetRequiresScratchVar();
#endif // TARGET_ARM

    private:
        bool CanPromoteStructVar(unsigned lclNum);
        bool ShouldPromoteStructVar(unsigned lclNum);
        void PromoteStructVar(unsigned lclNum);
        void SortStructFields();

        void GetFieldInfo(CORINFO_CLASS_HANDLE classHandle, unsigned index);
        void TryPromoteSingleFieldStruct(lvaStructFieldInfo& outerFieldInfo);

    private:
        Compiler*              compiler;
        lvaStructPromotionInfo structPromotionInfo;

#ifdef DEBUG
        typedef JitHashTable<CORINFO_FIELD_HANDLE, JitPtrKeyFuncs<CORINFO_FIELD_STRUCT_>, var_types>
                                 RetypedAsScalarFieldsMap;
        RetypedAsScalarFieldsMap retypedFieldsMap;
#endif // DEBUG
    };

    StructPromotionHelper* structPromotionHelper;

#if !defined(TARGET_64BIT)
    void lvaPromoteLongVars();
#endif // !defined(TARGET_64BIT)
    unsigned lvaGetFieldLocal(const LclVarDsc* varDsc, unsigned int fldOffset);
    lvaPromotionType lvaGetPromotionType(const LclVarDsc* varDsc);
    lvaPromotionType lvaGetPromotionType(unsigned varNum);
    lvaPromotionType lvaGetParentPromotionType(const LclVarDsc* varDsc);
    lvaPromotionType lvaGetParentPromotionType(unsigned varNum);
    bool lvaIsFieldOfDependentlyPromotedStruct(const LclVarDsc* varDsc);
    bool lvaIsGCTracked(const LclVarDsc* varDsc);

#if defined(FEATURE_SIMD)
    bool lvaMapSimd12ToSimd16(const LclVarDsc* varDsc)
    {
        assert(varDsc->lvType == TYP_SIMD12);
        assert(varDsc->GetSize() == 12);

#if defined(TARGET_64BIT) && !defined(OSX_ARM64_ABI)
        assert(varDsc->lvSize() == 16);
#endif // defined(TARGET_64BIT)

        // We make local variable SIMD12 types 16 bytes instead of just 12.
        // lvSize() will return 16 bytes for SIMD12, even for fields.
        // However, we can't do that mapping if the var is a dependently promoted struct field.
        // Such a field must remain its exact size within its parent struct unless it is a single
        // field *and* it is the only field in a struct of 16 bytes.
        if (varDsc->lvSize() != 16)
        {
            return false;
        }
        if (lvaIsFieldOfDependentlyPromotedStruct(varDsc))
        {
            LclVarDsc* parentVarDsc = lvaGetDesc(varDsc->lvParentLcl);
            return (parentVarDsc->lvFieldCnt == 1) && (parentVarDsc->lvSize() == 16);
        }
        return true;
    }
#endif // defined(FEATURE_SIMD)

    unsigned lvaGSSecurityCookie; // LclVar number
    bool     lvaTempsHaveLargerOffsetThanVars();

    // Returns "true" iff local variable "lclNum" is in SSA form.
    bool lvaInSsa(unsigned lclNum)
    {
        assert(lclNum < lvaCount);
        return lvaTable[lclNum].lvInSsa;
    }

    unsigned lvaStubArgumentVar; // variable representing the secret stub argument coming in EAX

#if defined(FEATURE_EH_FUNCLETS)
    unsigned lvaPSPSym; // variable representing the PSPSym
#endif

    InlineInfo*     impInlineInfo;
    InlineStrategy* m_inlineStrategy;

    // The Compiler* that is the root of the inlining tree of which "this" is a member.
    Compiler* impInlineRoot();

#if defined(DEBUG) || defined(INLINE_DATA)
    unsigned __int64 getInlineCycleCount()
    {
        return m_compCycles;
    }
#endif // defined(DEBUG) || defined(INLINE_DATA)

    bool fgNoStructPromotion;      // Set to TRUE to turn off struct promotion for this method.
    bool fgNoStructParamPromotion; // Set to TRUE to turn off struct promotion for parameters this method.

    //=========================================================================
    //                          PROTECTED
    //=========================================================================

protected:
    //---------------- Local variable ref-counting ----------------------------

    void lvaMarkLclRefs(GenTree* tree, GenTree* user, BasicBlock* block, Statement* stmt, bool isRecompute);
    bool IsDominatedByExceptionalEntry(BasicBlock* block);
    void SetVolatileHint(LclVarDsc* varDsc);

    // Keeps the mapping from SSA #'s to VN's for the implicit memory variables.
    SsaDefArray<SsaMemDef> lvMemoryPerSsaData;

public:
    // Returns the address of the per-Ssa data for memory at the given ssaNum (which is required
    // not to be the SsaConfig::RESERVED_SSA_NUM, which indicates that the variable is
    // not an SSA variable).
    SsaMemDef* GetMemoryPerSsaData(unsigned ssaNum)
    {
        return lvMemoryPerSsaData.GetSsaDef(ssaNum);
    }

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

private:
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

    static void impValidateMemoryAccessOpcode(const BYTE* codeAddr, const BYTE* codeEndp, bool volatilePrefix);
    static OPCODE impGetNonPrefixOpcode(const BYTE* codeAddr, const BYTE* codeEndp);
    static bool impOpcodeIsCallOpcode(OPCODE opcode);

public:
    void impInit();
    void impImport();

    CORINFO_CLASS_HANDLE impGetRefAnyClass();
    CORINFO_CLASS_HANDLE impGetRuntimeArgumentHandle();
    CORINFO_CLASS_HANDLE impGetTypeHandleClass();
    CORINFO_CLASS_HANDLE impGetStringClass();
    CORINFO_CLASS_HANDLE impGetObjectClass();

    // Returns underlying type of handles returned by ldtoken instruction
    var_types GetRuntimeHandleUnderlyingType()
    {
        // RuntimeTypeHandle is backed by raw pointer on CoreRT and by object reference on other runtimes
        return IsTargetAbi(CORINFO_CORERT_ABI) ? TYP_I_IMPL : TYP_REF;
    }

    void impDevirtualizeCall(GenTreeCall*            call,
                             CORINFO_METHOD_HANDLE*  method,
                             unsigned*               methodFlags,
                             CORINFO_CONTEXT_HANDLE* contextHandle,
                             CORINFO_CONTEXT_HANDLE* exactContextHandle,
                             bool                    isLateDevirtualization,
                             bool                    isExplicitTailCall,
                             IL_OFFSETX              ilOffset = BAD_IL_OFFSET);

    void impLateDevirtualizeCall(GenTreeCall* call);

    //=========================================================================
    //                          PROTECTED
    //=========================================================================

protected:
    //-------------------- Stack manipulation ---------------------------------

    unsigned impStkSize; // Size of the full stack

#define SMALL_STACK_SIZE 16 // number of elements in impSmallStack

    bool impIsPrimitive(CorInfoType type);
    bool impILConsumesAddr(const BYTE* codeAddr);

    void impResolveToken(const BYTE* addr, CORINFO_RESOLVED_TOKEN* pResolvedToken, CorInfoTokenKind kind);

    void impPushOnStack(GenTree* tree, typeInfo ti);
    StackEntry impPopStack();
    GenTree* impPopStackCoerceArg(var_types signatureType);
    StackEntry& impStackTop(unsigned n = 0);
    unsigned impStackHeight();

    typeInfo impMakeTypeInfo(CorInfoType type, CORINFO_CLASS_HANDLE classHandle);

    GenTree* impImportLdvirtftn(GenTree* thisPtr, CORINFO_RESOLVED_TOKEN* pResolvedToken, CORINFO_CALL_INFO* pCallInfo);

    int impBoxPatternMatch(CORINFO_RESOLVED_TOKEN* pResolvedToken, const BYTE* codeAddr, const BYTE* codeEndp);
    void impImportAndPushBox(CORINFO_RESOLVED_TOKEN* pResolvedToken);

    void impImportNewObjArray(CORINFO_RESOLVED_TOKEN* pResolvedToken, CORINFO_CALL_INFO* pCallInfo);

    bool impCanPInvokeInline();
    bool impCanPInvokeInlineCallSite(BasicBlock* block);
    void impCheckForPInvokeCall(
        GenTreeCall* call, CORINFO_METHOD_HANDLE methHnd, CORINFO_SIG_INFO* sig, unsigned mflags, BasicBlock* block);
    GenTreeCall* impImportIndirectCall(CORINFO_SIG_INFO* sig, IL_OFFSETX ilOffset = BAD_IL_OFFSET);
    void impPopArgsForUnmanagedCall(GenTree* call, CORINFO_SIG_INFO* sig);

    void impInsertHelperCall(CORINFO_HELPER_DESC* helperCall);
    void impHandleAccessAllowed(CorInfoIsAccessAllowedResult result, CORINFO_HELPER_DESC* helperCall);
    void impHandleAccessAllowedInternal(CorInfoIsAccessAllowedResult result, CORINFO_HELPER_DESC* helperCall);

    var_types impImportCall(OPCODE                  opcode,
                            CORINFO_RESOLVED_TOKEN* pResolvedToken,
                            CORINFO_RESOLVED_TOKEN* pConstrainedResolvedToken, // Is this a "constrained." call on a
                                                                               // type parameter?
                            GenTree*           newobjThis,
                            int                prefixFlags,
                            CORINFO_CALL_INFO* callInfo,
                            IL_OFFSET          rawILOffset);

    CORINFO_CLASS_HANDLE impGetSpecialIntrinsicExactReturnType(CORINFO_METHOD_HANDLE specialIntrinsicHandle);

    void impInitializeStructCall(GenTreeCall* call, CORINFO_CLASS_HANDLE retClass);
#if FEATURE_MULTIREG_RET
    GenTree* impCanonicalizeMultiRegCall(GenTreeCall* call);
    GenTree* impCanonicalizeMultiRegReturnValue(GenTree* value, CORINFO_CLASS_HANDLE retClass);
#endif
    GenTree* impSpillPseudoReturnBufferCall(GenTreeCall* call);

    GenTree* impInitClass(CORINFO_RESOLVED_TOKEN* pResolvedToken);

    GenTree* impImportStaticReadOnlyField(void* fldAddr, var_types lclTyp);

    GenTree* impImportFieldAccess(GenTree*                  objPtr,
                                  CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                  const CORINFO_FIELD_INFO& fieldInfo,
                                  CORINFO_ACCESS_FLAGS      accessFlags,
                                  var_types                 type,
                                  CORINFO_CLASS_HANDLE      structType);

    GenTree* impImportStaticFieldAddressHelper(CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                               const CORINFO_FIELD_INFO& fieldInfo,
                                               CORINFO_ACCESS_FLAGS      accessFlags);

    GenTree* impImportStaticFieldAccess(CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                        const CORINFO_FIELD_INFO& fieldInfo,
                                        CORINFO_ACCESS_FLAGS      accessFlags,
                                        var_types                 type);

    static void impBashVarAddrsToI(GenTree* tree1, GenTree* tree2 = nullptr);

    GenTree* impImplicitIorI4Cast(GenTree* tree, var_types dstTyp);

    GenTree* impImplicitR4orR8Cast(GenTree* tree, var_types dstTyp);

    void impImportLeave(BasicBlock* block);
    void impResetLeaveBlock(BasicBlock* block, unsigned jmpAddr);
    GenTree* impTypeIsAssignable(GenTree* typeTo, GenTree* typeFrom);
    GenTree* impIntrinsic(GenTree*                newobjThis,
                          CORINFO_CLASS_HANDLE    clsHnd,
                          CORINFO_METHOD_HANDLE   method,
                          CORINFO_SIG_INFO*       sig,
                          unsigned                methodFlags,
                          int                     memberRef,
                          bool                    readonlyCall,
                          bool                    tailCall,
                          CORINFO_RESOLVED_TOKEN* pContstrainedResolvedToken,
                          CORINFO_THIS_TRANSFORM  constraintCallThisTransform,
                          CorInfoIntrinsics*      pIntrinsicID,
                          bool*                   isSpecialIntrinsic = nullptr);
    GenTree* impMathIntrinsic(CORINFO_METHOD_HANDLE method,
                              CORINFO_SIG_INFO*     sig,
                              var_types             callType,
                              NamedIntrinsic        intrinsicName,
                              bool                  tailCall);
    NamedIntrinsic lookupNamedIntrinsic(CORINFO_METHOD_HANDLE method);
    GenTree* impUnsupportedNamedIntrinsic(unsigned              helper,
                                          CORINFO_METHOD_HANDLE method,
                                          CORINFO_SIG_INFO*     sig,
                                          bool                  mustExpand);

#ifdef FEATURE_HW_INTRINSICS
    GenTree* impHWIntrinsic(NamedIntrinsic        intrinsic,
                            CORINFO_CLASS_HANDLE  clsHnd,
                            CORINFO_METHOD_HANDLE method,
                            CORINFO_SIG_INFO*     sig,
                            bool                  mustExpand);

    GenTree* impVectorGetElement(ClassLayout* layout, GenTree* op1, GenTree* op2);

    NamedIntrinsic impFindSysNumSimdIntrinsic(CORINFO_METHOD_HANDLE method,
                                              const char*           className,
                                              const char*           methodName,
                                              const char*           enclosingClassName);

    GenTree* impImportSysNumSimdIntrinsic(NamedIntrinsic        intrinsic,
                                          CORINFO_CLASS_HANDLE  clsHnd,
                                          CORINFO_METHOD_HANDLE method,
                                          CORINFO_SIG_INFO*     sig,
                                          bool                  isNewObj);

protected:
    bool compSupportsHWIntrinsic(CORINFO_InstructionSet isa);

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
                                         SpecialCodeKind indexThrowKind,
                                         SpecialCodeKind lastIndexThrowKind);
    GenTree* impVector234TCopyTo(const HWIntrinsicSignature& sig, ClassLayout* layout);
    GenTree* impVectorTGetItem(const HWIntrinsicSignature& sig, ClassLayout* layout);
    GenTree* impVectorTMultiply(const HWIntrinsicSignature& sig);
    GenTree* impVector234TInstanceEquals(const HWIntrinsicSignature& sig);
    GenTree* impVector234Dot(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2);
#ifdef TARGET_ARM64
    GenTree* impVector234TEquals(const HWIntrinsicSignature& sig, GenTree* op1, GenTree* op2, bool notEqual = false);
    GenTree* impVectorT128ConditionalSelect(const HWIntrinsicSignature& sig, GenTree* mask, GenTree* op1, GenTree* op2);
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
    GenTree* impArrayAccessIntrinsic(CORINFO_CLASS_HANDLE clsHnd,
                                     CORINFO_SIG_INFO*    sig,
                                     int                  memberRef,
                                     bool                 readonlyCall,
                                     CorInfoIntrinsics    intrinsicID);
    GenTree* impInitializeArrayIntrinsic(CORINFO_SIG_INFO* sig);

    GenTree* impMethodPointer(CORINFO_RESOLVED_TOKEN* pResolvedToken, CORINFO_CALL_INFO* pCallInfo);

    GenTree* impTransformThis(GenTree*                thisPtr,
                              CORINFO_RESOLVED_TOKEN* pConstrainedResolvedToken,
                              CORINFO_THIS_TRANSFORM  transform);

    //----------------- Manipulating the trees and stmts ----------------------

    Statement* impStmtList; // Statements for the BB being imported.
    Statement* impLastStmt; // The last statement for the current BB.

public:
    enum
    {
        CHECK_SPILL_ALL  = -1,
        CHECK_SPILL_NONE = -2
    };

    void impBeginTreeList();
    void impEndTreeList(BasicBlock* block, Statement* firstStmt, Statement* lastStmt);
    void impEndTreeList(BasicBlock* block);
    void impAppendStmtCheck(Statement* stmt, unsigned chkLevel);
    void impAppendStmt(Statement* stmt, unsigned chkLevel);
    void impAppendStmt(Statement* stmt);
    void impInsertStmtBefore(Statement* stmt, Statement* stmtBefore);
    Statement* impAppendTree(GenTree* tree, unsigned chkLevel, IL_OFFSETX offset);
    void impInsertTreeBefore(GenTree* tree, IL_OFFSETX offset, Statement* stmtBefore);
    void impAppendTempAssign(unsigned lclNum, GenTree* val, unsigned curLevel);
    void impAppendTempAssign(unsigned lclNum, GenTree* val, ClassLayout* layout, unsigned curLevel);
    void impAppendTempAssign(unsigned lclNum, GenTree* val, CORINFO_CLASS_HANDLE structHnd, unsigned curLevel);

    Statement* impExtractLastStmt();

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

    GenTree* impAssignStructAddr(GenTree* dest, GenTree* src, ClassLayout* layout, unsigned curLevel);

    GenTree* impGetStructAddr(GenTree* structVal, CORINFO_CLASS_HANDLE structHnd, unsigned curLevel, bool willDeref);

    GenTree* impCanonicalizeStructCallArg(GenTree* arg, ClassLayout* argLayout, unsigned curLevel);

    GenTree* impTokenToHandle(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                              bool*                   pRuntimeLookup    = nullptr,
                              bool                    mustRestoreHandle = false,
                              bool                    importParent      = false);

    GenTree* impParentClassTokenToHandle(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                         bool*                   pRuntimeLookup    = nullptr,
                                         bool                    mustRestoreHandle = false)
    {
        return impTokenToHandle(pResolvedToken, pRuntimeLookup, mustRestoreHandle, true);
    }

    GenTree* impLookupToTree(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                             CORINFO_LOOKUP*         pLookup,
                             GenTreeFlags            flags,
                             void*                   compileTimeHandle);

    GenTree* getRuntimeContextTree(CORINFO_RUNTIME_LOOKUP_KIND kind);

    GenTree* impRuntimeLookupToTree(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                    CORINFO_LOOKUP*         pLookup,
                                    void*                   compileTimeHandle);

    GenTree* impReadyToRunLookupToTree(CORINFO_CONST_LOOKUP* pLookup, GenTreeFlags flags, void* compileTimeHandle);

    GenTreeCall* impReadyToRunHelperToTree(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                           CorInfoHelpFunc         helper,
                                           var_types               type,
                                           GenTreeCall::Use*       args               = nullptr,
                                           CORINFO_LOOKUP_KIND*    pGenericLookupKind = nullptr);

    GenTree* impCastClassOrIsInstToTree(GenTree*                op1,
                                        GenTree*                op2,
                                        CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                        bool                    isCastClass);

    GenTree* impOptimizeCastClassOrIsInst(GenTree* op1, CORINFO_RESOLVED_TOKEN* pResolvedToken, bool isCastClass);

    bool IsIntrinsicImplementedByUserCall(NamedIntrinsic intrinsicName);
    bool IsTargetIntrinsic(NamedIntrinsic intrinsicName);
    bool IsMathIntrinsic(NamedIntrinsic intrinsicName);
    bool IsMathIntrinsic(GenTree* tree);

private:
    //----------------- Importing the method ----------------------------------

    CORINFO_CONTEXT_HANDLE impTokenLookupContextHandle; // The context used for looking up tokens.

#ifdef DEBUG
    unsigned    impCurOpcOffs;
    const char* impCurOpcName;

    // For displaying instrs with generated native code (-n:B)
    Statement* impLastILoffsStmt; // oldest stmt added for which we did not call SetLastILOffset().
    void       impNoteLastILoffs();
#endif

    /* IL offset of the stmt currently being imported. It gets set to
       BAD_IL_OFFSET after it has been set in the appended trees. Then it gets
       updated at IL offsets for which we have to report mapping info.
       It also includes flag bits, so use jitGetILoffs()
       to get the actual IL offset value.
    */

    IL_OFFSETX impCurStmtOffs;
    void impCurStmtOffsSet(IL_OFFSET offs);

    void impNoteBranchOffs();

    unsigned impInitBlockLineInfo();

    GenTree* impCheckForNullPointer(GenTree* obj);
    bool impIsThis(GenTree* obj);
    bool impIsLDFTN_TOKEN(const BYTE* delegateCreateStart, const BYTE* newobjCodeAddr);
    bool impIsDUP_LDVIRTFTN_TOKEN(const BYTE* delegateCreateStart, const BYTE* newobjCodeAddr);
    bool impIsAnySTLOC(OPCODE opcode)
    {
        return ((opcode == CEE_STLOC) || (opcode == CEE_STLOC_S) ||
                ((opcode >= CEE_STLOC_0) && (opcode <= CEE_STLOC_3)));
    }

    GenTreeCall::Use* impPopCallArgs(unsigned count, CORINFO_SIG_INFO* sig, GenTreeCall::Use* prefixArgs = nullptr);

    bool impCheckImplicitArgumentCoercion(var_types sigType, var_types nodeType) const;

    GenTreeCall::Use* impPopReverseCallArgs(unsigned count, CORINFO_SIG_INFO* sig, unsigned skipReverseCount = 0);

    /*
     * Get current IL offset with stack-empty info incoporated
     */
    IL_OFFSETX impCurILOffset(IL_OFFSET offs, bool callInstruction = false);

    //---------------- Spilling the importer stack ----------------------------

    // The maximum number of bytes of IL processed without clean stack state.
    // It allows to limit the maximum tree size and depth.
    static const unsigned MAX_TREE_SIZE = 200;
    bool impCanSpillNow(OPCODE prevOpcode);

    // We keep a byte-per-block map (dynamically extended) in the top-level Compiler object of a compilation.
    JitExpandArray<bool> impPendingBlockMembers;

    // Return the byte for "b" (allocating/extending impPendingBlockMembers if necessary.)
    // Operates on the map in the top-level ancestor.
    bool impIsPendingBlockMember(BasicBlock* blk)
    {
        return impInlineRoot()->impPendingBlockMembers.Get(blk->bbInd());
    }

    // Set the byte for "b" to "val" (allocating/extending impPendingBlockMembers if necessary.)
    // Operates on the map in the top-level ancestor.
    void impSetPendingBlockMember(BasicBlock* blk, bool val)
    {
        impInlineRoot()->impPendingBlockMembers.Set(blk->bbInd(), val);
    }

    bool impCanReimport;

    void impSpillStackEntry(unsigned level DEBUGARG(const char* reason));

    void impSpillStackEnsure(bool spillLeaves = false);
    void impEvalSideEffects();
    void impSpillSpecialSideEff();
    void impSpillSideEffects(bool spillGlobEffects, unsigned chkLevel DEBUGARG(const char* reason));
    void               impSpillValueClasses();
    void               impSpillEvalStack();
    static fgWalkPreFn impFindValueClasses;
    void impSpillLclRefs(ssize_t lclNum);

    BasicBlock* impPushCatchArgOnStack(BasicBlock* hndBlk, CORINFO_CLASS_HANDLE clsHnd, bool isSingleBlockFilter);
    GenTree* impNewCatchArg();

    bool impBlockIsInALoop(BasicBlock* block);
    void impImportBlockCode(BasicBlock* block);

    void impAddPendingEHSuccessors(BasicBlock* block);

    void impImportBlockPending(BasicBlock* block);
    void impPushPendingBlock(BasicBlock* block);
    BasicBlock* impPopPendingBlock();

    var_types impGetNumericBinaryOpType(genTreeOps oper, bool fUnsigned, GenTree** pOp1, GenTree** pOp2);

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

    // When we compute a "spill clique" (see above) these byte-maps are allocated to have a byte per basic
    // block, and represent the predecessor and successor members of the clique currently being computed.
    // *** Access to these will need to be locked in a parallel compiler.
    JitExpandArray<uint8_t> impSpillCliqueMembers;

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

    bool impIsSpillCliqueMember(SpillCliqueDir predOrSucc, BasicBlock* block);
    bool impAddSpillCliqueMember(SpillCliqueDir predOrSucc, BasicBlock* block);

    void impPushLclVar(unsigned lclNum, IL_OFFSET offset);
    void impLoadArg(unsigned ilArgNum, IL_OFFSET offset);
    void impLoadLoc(unsigned ilLclNum, IL_OFFSET offset);
    bool impInlineReturnInstruction();
    void impReturnInstruction(int prefixFlags, OPCODE* opcode);

    // A free list of linked list nodes used to represent to-do stacks of basic blocks.
    struct BlockListNode
    {
        BasicBlock*    m_blk;
        BlockListNode* m_next;
        BlockListNode(BasicBlock* blk, BlockListNode* next = nullptr) : m_blk(blk), m_next(next)
        {
        }
        void* operator new(size_t sz, Compiler* comp);
    };
    BlockListNode* impBlockListNodeFreeList;
    BlockListNode* impPendingBlockStack;

    void FreeBlockListNode(BlockListNode* node);

    var_types mangleVarArgsType(var_types type);

#if FEATURE_VARARG
    regNumber getCallArgIntRegister(regNumber floatReg);
    regNumber getCallArgFloatRegister(regNumber intReg);
#endif // FEATURE_VARARG

#if defined(DEBUG)
    static unsigned jitTotalMethodCompiled;
#endif

#ifdef DEBUG
    static LONG jitNestingLevel;
#endif // DEBUG

    static GenTreeLclVar* impIsAddressInLocal(GenTree* tree);

    void impMakeDiscretionaryInlineObservations(InlineInfo* pInlineInfo, InlineResult* inlineResult);

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

    bool impTailCallRetTypeCompatible(GenTreeCall* call);

    bool impIsTailCallILPattern(
        bool tailPrefixed, OPCODE curOpcode, const BYTE* codeAddrOfNextOpcode, const BYTE* codeEnd, bool isRecursive);

    bool impIsImplicitTailCallCandidate(
        OPCODE curOpcode, const BYTE* codeAddrOfNextOpcode, const BYTE* codeEnd, int prefixFlags, bool isRecursive);

    bool impIsClassExact(CORINFO_CLASS_HANDLE classHnd);
    bool impCanSkipCovariantStoreCheck(GenTree* value, GenTree* array);

    CORINFO_RESOLVED_TOKEN* impAllocateToken(const CORINFO_RESOLVED_TOKEN& token);

    GenTree* impImportInitObj(GenTree* dstAddr, ClassLayout* layout);
    GenTree* impImportCpObj(GenTree* dstAddr, GenTree* srcAddr, ClassLayout* layout);
    GenTree* impImportInitBlk(GenTree* dstAddr, GenTree* initValue, GenTree* size, bool isVolatile);
    GenTree* impImportCpBlk(GenTree* dstAddr, GenTree* srcAddr, GenTree* size, bool isVolatile);

    GenTree* impImportPop(BasicBlock* block);

    void impImportDup();

    GenTree* impImportTlsFieldAccess(CORINFO_RESOLVED_TOKEN*   resolvedToken,
                                     const CORINFO_FIELD_INFO& fieldInfo,
                                     CORINFO_ACCESS_FLAGS      accessFlags,
                                     var_types                 type);

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
    BasicBlock* fgFirstBB;        // Beginning of the basic block list
    BasicBlock* fgLastBB;         // End of the basic block list
    BasicBlock* fgFirstColdBlock; // First block to be placed in the cold section
    BasicBlock* fgEntryBB;        // For OSR, the original method's entry point
#if defined(FEATURE_EH_FUNCLETS)
    BasicBlock* fgFirstFuncletBB; // First block of outlined funclets (to allow block insertion before the funclets)
#endif
    BasicBlock* fgFirstBBScratch;   // Block inserted for initialization stuff. Is nullptr if no such block has been
                                    // created.
    BasicBlockList* fgReturnBlocks; // list of BBJ_RETURN blocks
    unsigned        fgEdgeCount;    // # of control flow edges between the BBs
    unsigned        fgBBcount;      // # of BBs in the method
#ifdef DEBUG
    unsigned fgBBcountAtCodegen; // # of BBs in the method at the start of codegen
#endif
    unsigned     fgBBNumMax;       // The max bbNum that has been assigned to basic blocks
    unsigned     fgDomBBcount;     // # of BBs for which we have dominator and reachability information
    BasicBlock** fgBBInvPostOrder; // The flow graph stored in an array sorted in topological order, needed to compute
                                   // dominance. Indexed by block number. Size: fgBBNumMax + 1.

    // After the dominance tree is computed, we cache a DFS preorder number and DFS postorder number to compute
    // dominance queries in O(1). fgDomTreePreOrder and fgDomTreePostOrder are arrays giving the block's preorder and
    // postorder number, respectively. The arrays are indexed by basic block number. (Note that blocks are numbered
    // starting from one. Thus, we always waste element zero. This makes debugging easier and makes the code less likely
    // to suffer from bugs stemming from forgetting to add or subtract one from the block number to form an array
    // index). The arrays are of size fgBBNumMax + 1.
    unsigned* fgDomTreePreOrder;
    unsigned* fgDomTreePostOrder;

    // Dominator tree used by SSA construction and copy propagation (the two are expected to use the same tree
    // in order to avoid the need for SSA reconstruction and an "out of SSA" phase).
    DomTreeNode* fgSsaDomTree;

    bool fgBBVarSetsInited;

    // Allocate array like T* a = new T[fgBBNumMax + 1];
    // Using helper so we don't keep forgetting +1.
    template <typename T>
    T* fgAllocateTypeForEachBlk(CompMemKind cmk = CMK_Unknown)
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
    unsigned fgCurBBEpoch;

    unsigned GetCurBasicBlockEpoch()
    {
        return fgCurBBEpoch;
    }

    // The number of basic blocks in the current epoch. When the blocks are renumbered,
    // this is fgBBcount. As blocks are added, fgBBcount increases, fgCurBBEpochSize remains
    // the same, until a new BasicBlock epoch is created, such as when the blocks are all renumbered.
    unsigned fgCurBBEpochSize;

    // The number of "size_t" elements required to hold a bitset large enough for fgCurBBEpochSize
    // bits. This is precomputed to avoid doing math every time BasicBlockBitSetTraits::GetArrSize() is called.
    unsigned fgBBSetCountInSizeTUnits;

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

    BasicBlock* fgNewBasicBlock(BBjumpKinds jumpKind);
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

    bool fgModified;         // True if the flow graph has been modified recently
    bool fgComputePredsDone; // Have we computed the bbPreds list
    bool fgCheapPredsValid;  // Is the bbCheapPreds list valid?
    bool fgDomsComputed;     // Have we computed the dominator sets?
    bool fgOptimizedFinally; // Did we optimize any try-finallys?

    bool fgHasSwitch; // any BBJ_SWITCH jumps?

    BlockSet fgEnterBlks; // Set of blocks which have a special transfer of control; the "entry" blocks plus EH handler
                          // begin blocks.

#ifdef DEBUG
    bool fgReachabilitySetsValid; // Are the bbReach sets valid?
    bool fgEnterBlksSetValid;     // Is the fgEnterBlks set valid?
#endif                            // DEBUG

    bool fgRemoveRestOfBlock; // true if we know that we will throw
    bool fgStmtRemoved;       // true if we remove statements -> need new DFA

    // There are two modes for ordering of the trees.
    //  - In FGOrderTree, the dominant ordering is the tree order, and the nodes contained in
    //    each tree and sub-tree are contiguous, and can be traversed (in gtNext/gtPrev order)
    //    by traversing the tree according to the order of the operands.
    //  - In FGOrderLinear, the dominant ordering is the linear order.

    enum FlowGraphOrder
    {
        FGOrderTree,
        FGOrderLinear
    };
    FlowGraphOrder fgOrder;

    // The following are boolean flags that keep track of the state of internal data structures

    bool                 fgStmtListThreaded;       // true if the node list is now threaded
    bool                 fgCanRelocateEHRegions;   // true if we are allowed to relocate the EH regions
    bool                 fgEdgeWeightsComputed;    // true after we have called fgComputeEdgeWeights
    bool                 fgHaveValidEdgeWeights;   // true if we were successful in computing all of the edge weights
    bool                 fgSlopUsedInEdgeWeights;  // true if their was some slop used when computing the edge weights
    bool                 fgRangeUsedInEdgeWeights; // true if some of the edgeWeight are expressed in Min..Max form
    bool                 fgNeedsUpdateFlowGraph;   // true if we need to run fgUpdateFlowGraph
    BasicBlock::weight_t fgCalledCount;            // count of the number of times this method was called
                                                   // This is derived from the profile data
                                                   // or is BB_UNITY_WEIGHT when we don't have profile data

#if defined(FEATURE_EH_FUNCLETS)
    bool fgFuncletsCreated; // true if the funclet creation phase has been run
#endif                      // FEATURE_EH_FUNCLETS

    bool fgGlobalMorph; // indicates if we are during the global morphing phase
                        // since fgMorphTree can be called from several places

    bool     impBoxTempInUse; // the temp below is valid and available
    unsigned impBoxTemp;      // a temporary that is used for boxing

#ifdef DEBUG
    bool jitFallbackCompile; // Are we doing a fallback compile? That is, have we executed a NO_WAY assert,
                             //   and we are trying to compile again in a "safer", minopts mode?
#endif

#if defined(DEBUG)
    unsigned impInlinedCodeSize;
    bool     fgPrintInlinedMethods;
#endif

    jitstd::vector<flowList*>* fgPredListSortVector;

    //-------------------------------------------------------------------------

    void fgInit();

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

    GenTree* fgGetCritSectOfStaticMethod();

#if defined(FEATURE_EH_FUNCLETS)

    void fgAddSyncMethodEnterExit();

    void fgInsertMonitorCall(BasicBlock* block, CorInfoHelpFunc helper, unsigned thisLclNum);

    void fgConvertSyncReturnToLeave(BasicBlock* block);

#endif // FEATURE_EH_FUNCLETS

    void fgAddReversePInvokeEnterExit();

    // The number of separate return points in the method.
    unsigned fgReturnCount;

    void fgAddInternal();

    bool fgFoldConditional(BasicBlock* block);

    void fgMorphStmts(BasicBlock* block);
    void fgMorphBlocks();

    void fgMergeBlockReturn(BasicBlock* block);

    bool fgMorphBlockStmt(BasicBlock* block, Statement* stmt DEBUGARG(const char* msg));

    void fgSetOptions();

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

    Statement* fgNewStmtFromTree(GenTree* tree, BasicBlock* block, IL_OFFSETX offs);
    Statement* fgNewStmtFromTree(GenTree* tree);
    Statement* fgNewStmtFromTree(GenTree* tree, BasicBlock* block);
    Statement* fgNewStmtFromTree(GenTree* tree, IL_OFFSETX offs);

    GenTreeQmark* fgGetTopLevelQmark(GenTree* expr, GenTreeLclVar** destLclVar);
    void fgExpandQmarkForCastInstOf(BasicBlock* block, Statement* stmt);
    void fgExpandQmarkStmt(BasicBlock* block, Statement* stmt);
    void fgExpandQmarkNodes();

    // Do "simple lowering."  This functionality is (conceptually) part of "general"
    // lowering that is distributed between fgMorph and the lowering phase of LSRA.
    void fgSimpleLowering();

    GenTree* fgInitThisClass();

    GenTreeCall* fgGetStaticsCCtorHelper(CORINFO_CLASS_HANDLE cls, CorInfoHelpFunc helper);

    GenTreeCall* fgGetSharedCCtor(CORINFO_CLASS_HANDLE cls);

    bool backendRequiresLocalVarLifetimes()
    {
        return !opts.MinOpts() || m_pLinearScan->willEnregisterLocalVars();
    }

    void fgLocalVarLiveness();

    void fgLocalVarLivenessInit();

    void fgPerNodeLocalVarLiveness(GenTree* node);
    void fgPerBlockLocalVarLiveness();

    VARSET_VALRET_TP fgGetHandlerLiveVars(BasicBlock* block);

    void fgLiveVarAnalysis(bool updateInternalOnly = false);

    void fgComputeLifeCall(VARSET_TP& life, GenTreeCall* call);

    void fgComputeLifeTrackedLocalUse(VARSET_TP& life, LclVarDsc& varDsc, GenTreeLclVarCommon* node);
    bool fgComputeLifeTrackedLocalDef(VARSET_TP&           life,
                                      VARSET_VALARG_TP     keepAliveVars,
                                      LclVarDsc&           varDsc,
                                      GenTreeLclVarCommon* node);
    bool fgComputeLifeUntrackedLocal(VARSET_TP&           life,
                                     VARSET_VALARG_TP     keepAliveVars,
                                     LclVarDsc&           varDsc,
                                     GenTreeLclVarCommon* lclVarNode);
    bool fgComputeLifeLocal(VARSET_TP& life, VARSET_VALARG_TP keepAliveVars, GenTree* lclVarNode);

    void fgComputeLife(VARSET_TP&       life,
                       GenTree*         startNode,
                       GenTree*         endNode,
                       VARSET_VALARG_TP volatileVars,
                       bool* pStmtInfoDirty DEBUGARG(bool* treeModf));

    void fgComputeLifeLIR(VARSET_TP& life, BasicBlock* block, VARSET_VALARG_TP volatileVars);

    bool fgTryRemoveNonLocal(GenTree* node, LIR::Range* blockRange);

    void fgRemoveDeadStoreLIR(GenTree* store, BasicBlock* block);
    bool fgRemoveDeadStore(GenTree**        pTree,
                           LclVarDsc*       varDsc,
                           VARSET_VALARG_TP life,
                           bool*            doAgain,
                           bool* pStmtInfoDirty DEBUGARG(bool* treeModf));

    void fgInterBlockLocalVarLiveness();

    // The presence of a partial definition presents some difficulties for SSA: this is both a use of some SSA name
    // of "x", and a def of a new SSA name for "x".  The tree only has one local variable for "x", so it has to choose
    // whether to treat that as the use or def.  It chooses the "use", and thus the old SSA name.  This map allows us
    // to record/recover the "def" SSA number, given the lcl var node for "x" in such a tree.
    typedef JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, unsigned> NodeToUnsignedMap;
    NodeToUnsignedMap* m_opAsgnVarDefSsaNums;
    NodeToUnsignedMap* GetOpAsgnVarDefSsaNums()
    {
        if (m_opAsgnVarDefSsaNums == nullptr)
        {
            m_opAsgnVarDefSsaNums = new (getAllocator()) NodeToUnsignedMap(getAllocator());
        }
        return m_opAsgnVarDefSsaNums;
    }

    // Requires value numbering phase to have completed. Returns the value number ("gtVN") of the
    // "tree," EXCEPT in the case of GTF_VAR_USEASG, because the tree node's gtVN member is the
    // "use" VN. Performs a lookup into the map of (use asg tree -> def VN.) to return the "def's"
    // VN.
    inline ValueNum GetUseAsgDefVNOrTreeVN(GenTree* tree);

    // Requires that "lcl" has the GTF_VAR_DEF flag set.  Returns the SSA number of "lcl".
    // Except: assumes that lcl is a def, and if it is
    // a partial def (GTF_VAR_USEASG), looks up and returns the SSA number for the "def",
    // rather than the "use" SSA number recorded in the tree "lcl".
    inline unsigned GetSsaNumForLocalVarDef(GenTree* lcl);

    // Performs SSA conversion.
    void fgSsaBuild();

    // Reset any data structures to the state expected by "fgSsaBuild", so it can be run again.
    void fgResetForSsa();

    unsigned fgSsaPassesCompleted; // Number of times fgSsaBuild has been run.

    // Returns "true" if this is a special variable that is never zero initialized in the prolog.
    inline bool fgVarIsNeverZeroInitializedInProlog(unsigned varNum);

    // Returns "true" if the variable needs explicit zero initialization.
    inline bool fgVarNeedsExplicitZeroInit(unsigned varNum, bool bbInALoop, bool bbIsReturn);

    // The value numbers for this compilation.
    ValueNumStore* vnStore;

public:
    ValueNumStore* GetValueNumStore()
    {
        return vnStore;
    }

    // Do value numbering (assign a value number to each
    // tree node).
    void fgValueNumber();

    // Computes new GcHeap VN via the assignment H[elemTypeEq][arrVN][inx][fldSeq] = rhsVN.
    // Assumes that "elemTypeEq" is the (equivalence class rep) of the array element type.
    // The 'indType' is the indirection type of the lhs of the assignment and will typically
    // match the element type of the array or fldSeq.  When this type doesn't match
    // or if the fldSeq is 'NotAField' we invalidate the array contents H[elemTypeEq][arrVN]
    //
    ValueNum fgValueNumberArrIndexAssign(const VNFuncApp& elemAddr, ValueNum rhsVN, var_types indType);

    // Requires that "tree" is a GT_IND marked as an array index, and that its address argument
    // has been parsed to yield the other input arguments.  If evaluation of the address
    // can raise exceptions, those should be captured in the exception set "excVN."
    // Assumes that "elemTypeEq" is the (equivalence class rep) of the array element type.
    // Marks "tree" with the VN for H[elemTypeEq][arrVN][inx][fldSeq] (for the liberal VN; a new unique
    // VN for the conservative VN.)  Also marks the tree's argument as the address of an array element.
    // The type tree->TypeGet() will typically match the element type of the array or fldSeq.
    // When this type doesn't match or if the fldSeq is 'NotAField' we return a new unique VN

    // Requires "funcApp" to be a VNF_PtrToArrElem, and "addrXvn" to represent the exception set thrown
    // by evaluating the array index expression "tree".  Returns the value number resulting from
    // dereferencing the array in the current GcHeap state.  If "tree" is non-null, it must be the
    // "GT_IND" that does the dereference, and it is given the returned value number.
    ValueNum fgValueNumberArrIndexVal(const VNFuncApp& elemAddr, ValueNum addrXvn, var_types indType);

    // Compute the value number for a byref-exposed load of the given type via the given pointerVN.
    ValueNum fgValueNumberByrefExposedLoad(var_types type, ValueNum pointerVN);

    unsigned fgVNPassesCompleted; // Number of times fgValueNumber has been run.

    // Utility functions for fgValueNumber.

    // Perform value-numbering for the trees in "blk".
    void fgValueNumberBlock(BasicBlock* blk);

    // Requires that "entryBlock" is the entry block of loop "loopNum", and that "loopNum" is the
    // innermost loop of which "entryBlock" is the entry.  Returns the value number that should be
    // assumed for the memoryKind at the start "entryBlk".
    ValueNum fgMemoryVNForLoopSideEffects(MemoryKind memoryKind, BasicBlock* entryBlock, unsigned loopNum);

    // Called when an operation (performed by "tree", described by "msg") may cause the GcHeap to be mutated.
    // As GcHeap is a subset of ByrefExposed, this will also annotate the ByrefExposed mutation.
    void fgMutateGcHeap(GenTree* tree DEBUGARG(const char* msg));

    // Called when an operation (performed by "tree", described by "msg") may cause an address-exposed local to be
    // mutated.
    void fgMutateAddressExposedLocal(GenTree* tree DEBUGARG(const char* msg));

    // For a GC heap store at curTree, record the new curMemoryVN's and update curTree's MemorySsaMap.
    // As GcHeap is a subset of ByrefExposed, this will also record the ByrefExposed store.
    void recordGcHeapStore(GenTree* curTree, ValueNum gcHeapVN DEBUGARG(const char* msg));

    // For a store to an address-exposed local at curTree, record the new curMemoryVN and update curTree's MemorySsaMap.
    void recordAddressExposedLocalStore(GenTree* curTree, ValueNum memoryVN DEBUGARG(const char* msg));

    // Tree caused an update in the current memory VN.  If "tree" has an associated heap SSA #, record that
    // value in that SSA #.
    void fgValueNumberRecordMemorySsa(MemoryKind memoryKind, GenTree* tree);

    // The input 'tree' is a leaf node that is a constant
    // Assign the proper value number to the tree
    void fgValueNumberTreeConst(GenTree* tree);

    // Assumes that all inputs to "tree" have had value numbers assigned; assigns a VN to tree.
    // (With some exceptions: the VN of the lhs of an assignment is assigned as part of the
    // assignment.)
    void fgValueNumberTree(GenTree* tree);

    // Does value-numbering for a block assignment.
    void fgValueNumberBlockAssignment(GenTree* tree);

    // Does value-numbering for a cast tree.
    void fgValueNumberCastTree(GenTree* tree);

    // Does value-numbering for a bitcast tree.
    void fgValueNumberBitCastTree(GenTreeUnOp* tree);

    // Does value-numbering for an intrinsic tree.
    void fgValueNumberIntrinsic(GenTree* tree);

#ifdef FEATURE_HW_INTRINSICS
    // Does value-numbering for a GT_HWINTRINSIC tree
    void fgValueNumberHWIntrinsic(GenTreeHWIntrinsic* node);
#endif // FEATURE_HW_INTRINSICS

    // Does value-numbering for a call.  We interpret some helper calls.
    void fgValueNumberCall(GenTreeCall* call);

    // Does value-numbering for a helper "call" that has a VN function symbol "vnf".
    void fgValueNumberHelperCallFunc(GenTreeCall* call, VNFunc vnf, ValueNumPair vnpExc);

    // Requires "helpCall" to be a helper call.  Assigns it a value number;
    // we understand the semantics of some of the calls.  Returns "true" if
    // the call may modify the heap (we assume arbitrary memory side effects if so).
    bool fgValueNumberHelperCall(GenTreeCall* helpCall);

    // Requires that "helpFunc" is one of the pure Jit Helper methods.
    // Returns the corresponding VNFunc to use for value numbering
    VNFunc fgValueNumberJitHelperMethodVNFunc(CorInfoHelpFunc helpFunc);

    // Adds the exception set for the current tree node which has a memory indirection operation
    void fgValueNumberAddExceptionSetForIndirection(GenTree* tree, GenTree* baseAddr);

    // Adds the exception sets for the current tree node which is performing a division or modulus operation
    void fgValueNumberAddExceptionSetForDivision(GenTree* tree);

    // Adds the exception set for the current tree node which is performing a overflow checking operation
    void fgValueNumberAddExceptionSetForOverflow(GenTree* tree);

    // Adds the exception set for the current tree node which is performing a bounds check operation
    void fgValueNumberAddExceptionSetForBoundsCheck(GenTree* tree);

    // Adds the exception set for the current tree node which is performing a ckfinite operation
    void fgValueNumberAddExceptionSetForCkFinite(GenTree* tree);

    // Adds the exception sets for the current tree node
    void fgValueNumberAddExceptionSet(GenTree* tree);

    // These are the current value number for the memory implicit variables while
    // doing value numbering.  These are the value numbers under the "liberal" interpretation
    // of memory values; the "conservative" interpretation needs no VN, since every access of
    // memory yields an unknown value.
    ValueNum fgCurMemoryVN[MemoryKindCount];

    bool isTrivialPointerSizedStruct(ClassLayout* layout) const;
    bool isNativePrimitiveStructType(ClassLayout* layout);
    var_types abiGetStructIntegerRegisterType(ClassLayout* layout);
    StructPassing abiGetStructParamType(ClassLayout* layout, bool isVarArg);
    StructPassing abiGetStructReturnType(ClassLayout* layout, CorInfoCallConvExtension callConv);

#ifdef DEBUG
    // Print a representation of "vnp" or "vn" on standard output.
    // If "level" is non-zero, we also print out a partial expansion of the value.
    void vnpPrint(ValueNumPair vnp, unsigned level);
    void vnPrint(ValueNum vn, unsigned level);
#endif

    bool fgDominate(BasicBlock* b1, BasicBlock* b2); // Return true if b1 dominates b2

    // Dominator computation member functions
    // Not exposed outside Compiler
protected:
    bool fgReachable(BasicBlock* b1, BasicBlock* b2); // Returns true if block b1 can reach block b2

    // Compute immediate dominators, the dominator tree and and its pre/post-order travsersal numbers.
    void fgComputeDoms();

    void fgCompDominatedByExceptionalEntryBlocks();

    BlockSet_ValRet_T fgGetDominatorSet(BasicBlock* block); // Returns a set of blocks that dominate the given block.
    // Note: this is relatively slow compared to calling fgDominate(),
    // especially if dealing with a single block versus block check.

    void fgComputeReachabilitySets(); // Compute bbReach sets. (Also sets BBF_GC_SAFE_POINT flag on blocks.)

    void fgComputeEnterBlocksSet(); // Compute the set of entry blocks, 'fgEnterBlks'.

    bool fgRemoveUnreachableBlocks(); // Remove blocks determined to be unreachable by the bbReach sets.

    void fgComputeReachability(); // Perform flow graph node reachability analysis.

    BasicBlock* fgIntersectDom(BasicBlock* a, BasicBlock* b); // Intersect two immediate dominator sets.

    void fgDfsInvPostOrder(); // In order to compute dominance using fgIntersectDom, the flow graph nodes must be
                              // processed in topological sort, this function takes care of that.

    void fgDfsInvPostOrderHelper(BasicBlock* block, BlockSet& visited, unsigned* count);

    BlockSet_ValRet_T fgDomFindStartNodes(); // Computes which basic blocks don't have incoming edges in the flow graph.
                                             // Returns this as a set.

    INDEBUG(void fgDispDomTree(DomTreeNode* domTree);) // Helper that prints out the Dominator Tree in debug builds.

    DomTreeNode* fgBuildDomTree(); // Once we compute all the immediate dominator sets for each node in the flow graph
                                   // (performed by fgComputeDoms), this procedure builds the dominance tree represented
                                   // adjacency lists.

    // In order to speed up the queries of the form 'Does A dominates B', we can perform a DFS preorder and postorder
    // traversal of the dominance tree and the dominance query will become A dominates B iif preOrder(A) <= preOrder(B)
    // && postOrder(A) >= postOrder(B) making the computation O(1).
    void fgNumberDomTree(DomTreeNode* domTree);

    // When the flow graph changes, we need to update the block numbers, predecessor lists, reachability sets, and
    // dominators.
    void fgUpdateChangedFlowGraph(const bool computePreds = true, const bool computeDoms = true);

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

    // Initialize the per-block variable sets (used for liveness analysis).
    void fgInitBlockVarSets();

    PhaseStatus fgInsertGCPolls();
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
    // For many purposes, it is desirable to be able to enumerate the *distinct* targets of a switch statement,
    // skipping duplicate targets.  (E.g., in flow analyses that are only interested in the set of possible targets.)
    // SwitchUniqueSuccSet contains the non-duplicated switch targets.
    // (Code that modifies the jump table of a switch has an obligation to call Compiler::UpdateSwitchTableTarget,
    // which in turn will call the "UpdateTarget" method of this type if a SwitchUniqueSuccSet has already
    // been computed for the switch block.  If a switch block is deleted or is transformed into a non-switch,
    // we leave the entry associated with the block, but it will no longer be accessed.)
    struct SwitchUniqueSuccSet
    {
        unsigned     numDistinctSuccs; // Number of distinct targets of the switch.
        BasicBlock** nonDuplicates;    // Array of "numDistinctSuccs", containing all the distinct switch target
                                       // successors.

        // The switch block "switchBlk" just had an entry with value "from" modified to the value "to".
        // Update "this" as necessary: if "from" is no longer an element of the jump table of "switchBlk",
        // remove it from "this", and ensure that "to" is a member.  Use "alloc" to do any required allocation.
        void UpdateTarget(CompAllocator alloc, BasicBlock* switchBlk, BasicBlock* from, BasicBlock* to);
    };

    typedef JitHashTable<BasicBlock*, JitPtrKeyFuncs<BasicBlock>, SwitchUniqueSuccSet> BlockToSwitchDescMap;

private:
    // Maps BasicBlock*'s that end in switch statements to SwitchUniqueSuccSets that allow
    // iteration over only the distinct successors.
    BlockToSwitchDescMap* m_switchDescMap;

public:
    BlockToSwitchDescMap* GetSwitchDescMap(bool createIfNull = true)
    {
        if ((m_switchDescMap == nullptr) && createIfNull)
        {
            m_switchDescMap = new (getAllocator()) BlockToSwitchDescMap(getAllocator());
        }
        return m_switchDescMap;
    }

    // Invalidate the map of unique switch block successors. For example, since the hash key of the map
    // depends on block numbers, we must invalidate the map when the blocks are renumbered, to ensure that
    // we don't accidentally look up and return the wrong switch data.
    void InvalidateUniqueSwitchSuccMap()
    {
        m_switchDescMap = nullptr;
    }

    // Requires "switchBlock" to be a block that ends in a switch.  Returns
    // the corresponding SwitchUniqueSuccSet.
    SwitchUniqueSuccSet GetDescriptorForSwitch(BasicBlock* switchBlk);

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

    void fgFindBasicBlocks();

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

    bool fgCheckRemoveStmt(BasicBlock* block, Statement* stmt);

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

    bool fgOptimizeSwitchBranches(BasicBlock* block);

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

    void fgDetermineFirstColdBlock();

    bool fgIsForwardBranch(BasicBlock* bJump, BasicBlock* bSrc = nullptr);

    bool fgUpdateFlowGraph(bool doTailDup = false);

    void fgFindOperOrder();

    // method that returns if you should split here
    typedef bool(fgSplitPredicate)(GenTree* tree, GenTree* parent, fgWalkData* data);

    void fgSetBlockOrder();

    void fgRemoveReturnBlock(BasicBlock* block);

    /* Helper code that has been factored out */
    inline void fgConvertBBToThrowBB(BasicBlock* block);

    bool gtIsSmallIntCastNeeded(GenTree* tree, var_types toType);
    GenTree* fgMorphNormalizeLclVarStore(GenTreeOp* asg);

    // The following check for loops that don't execute calls
    bool fgLoopCallMarked;

    void fgLoopCallTest(BasicBlock* srcBB, BasicBlock* dstBB);
    void fgLoopCallMark();

    void fgMarkLoopHead(BasicBlock* block);

    unsigned fgGetCodeEstimate(BasicBlock* block);

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
    void fgDispDoms();
    void fgDispReach();
    void fgDispBBLiveness(BasicBlock* block);
    void fgDispBBLiveness();
    void fgTableDispBasicBlock(BasicBlock* block, int ibcColWidth = 0);
    void fgDispBasicBlocks(BasicBlock* firstBlock, BasicBlock* lastBlock, bool dumpTrees);
    void fgDispBasicBlocks(bool dumpTrees = false);
    void fgDumpStmtTree(Statement* stmt, unsigned bbNum);
    void fgDumpBlock(BasicBlock* block);
    void fgDumpTrees(BasicBlock* firstBlock, BasicBlock* lastBlock);

    static fgWalkPreFn fgStress64RsltMulCB;
    void               fgStress64RsltMul();
    void               fgDebugCheckUpdate();
    void fgDebugCheckBBlist(bool checkBBNum = false, bool checkBBRefs = true);
    void fgDebugCheckBlockLinks();
    void fgDebugCheckLinks(bool morphTrees = false);
    void fgDebugCheckStmtsList(BasicBlock* block, bool morphTrees);
    void fgDebugCheckNodeLinks(BasicBlock* block, Statement* stmt);
    void fgDebugCheckNodesUniqueness();
    void fgDebugCheckLoopTable();

    void fgDebugCheckFlags(GenTree* tree);
    void fgDebugCheckDispFlags(GenTree* tree, GenTreeFlags dispFlags, GenTreeDebugFlags debugFlags);
    void fgDebugCheckFlagsHelper(GenTree* tree, GenTreeFlags treeFlags, GenTreeFlags chkFlags);
    void fgDebugCheckTryFinallyExits();
    void fgDebugCheckProfileData();
    bool fgDebugCheckIncomingProfileData(BasicBlock* block);
    bool fgDebugCheckOutgoingProfileData(BasicBlock* block);
#endif

    bool fgProfileWeightsEqual(BasicBlock::weight_t weight1, BasicBlock::weight_t weight2);
    bool fgProfileWeightsConsistent(BasicBlock::weight_t weight1, BasicBlock::weight_t weight2);

    static GenTree* fgGetFirstNode(GenTree* tree);

    //--------------------- Walking the trees in the IR -----------------------

    struct fgWalkData
    {
        Compiler*     compiler;
        fgWalkPreFn*  wtprVisitorFn;
        fgWalkPostFn* wtpoVisitorFn;
        void*         pCallbackData; // user-provided data
        GenTree*      parent;        // parent of current node, provided to callback
        GenTreeStack* parentStack;   // stack of parent nodes, if asked for
        bool          wtprLclsOnly;  // whether to only visit lclvar nodes
#ifdef DEBUG
        bool printModified; // callback can use this
#endif
    };

    fgWalkResult fgWalkTreePre(GenTree**    pTree,
                               fgWalkPreFn* visitor,
                               void*        pCallBackData = nullptr,
                               bool         lclVarsOnly   = false,
                               bool         computeStack  = false);

    fgWalkResult fgWalkTree(GenTree**     pTree,
                            fgWalkPreFn*  preVisitor,
                            fgWalkPostFn* postVisitor,
                            void*         pCallBackData = nullptr);

    void fgWalkAllTreesPre(fgWalkPreFn* visitor, void* pCallBackData);

    //----- Postorder

    fgWalkResult fgWalkTreePost(GenTree**     pTree,
                                fgWalkPostFn* visitor,
                                void*         pCallBackData = nullptr,
                                bool          computeStack  = false);

    // An fgWalkPreFn that looks for expressions that have inline throws in
    // minopts mode. Basically it looks for tress with gtOverflowEx() or
    // GTF_IND_RNGCHK.  It returns WALK_ABORT if one is found.  It
    // returns WALK_SKIP_SUBTREES if GTF_EXCEPT is not set (assumes flags
    // properly propagated to parent trees).  It returns WALK_CONTINUE
    // otherwise.
    static fgWalkResult fgChkThrowCB(GenTree** pTree, Compiler::fgWalkData* data);
    static fgWalkResult fgChkLocAllocCB(GenTree** pTree, Compiler::fgWalkData* data);

    /**************************************************************************
     *                          PROTECTED
     *************************************************************************/

protected:
    friend class SsaBuilder;
    friend struct ValueNumberState;

    //--------------------- Detect the basic blocks ---------------------------

    BasicBlock** fgBBs; // Table of pointers to the BBs

    void        fgInitBBLookup();
    BasicBlock* fgLookupBB(unsigned addr);

    bool fgCanSwitchToOptimized();
    void fgSwitchToOptimized();

    bool fgMayExplicitTailCall();

    void fgFindJumpTargets(const BYTE* codeAddr, IL_OFFSET codeSize, FixedBitVect* jumpTarget);

    void fgMarkBackwardJump(BasicBlock* startBlock, BasicBlock* endBlock);

    void fgLinkBasicBlocks();

    unsigned fgMakeBasicBlocks(const BYTE* codeAddr, IL_OFFSET codeSize, FixedBitVect* jumpTarget);

    void fgCheckBasicBlockControlFlow();

    void fgControlFlowPermitted(BasicBlock* blkSrc,
                                BasicBlock* blkDest,
                                bool        IsLeave = false /* is the src a leave block */);

    bool fgFlowToFirstBlockOfInnerTry(BasicBlock* blkSrc, BasicBlock* blkDest, bool sibling);

    void fgObserveInlineConstants(OPCODE opcode, const FgStack& stack, bool isInlining);

    void fgAdjustForAddressExposedOrWrittenThis();

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

    Instrumentor* fgCountInstrumentor;
    Instrumentor* fgClassInstrumentor;

    PhaseStatus fgPrepareToInstrumentMethod();
    PhaseStatus fgInstrumentMethod();
    PhaseStatus fgIncorporateProfileData();
    void        fgIncorporateBlockCounts();
    void        fgIncorporateEdgeCounts();

public:
    const char*                            fgPgoFailReason;
    bool                                   fgPgoDisabled;
    ICorJitInfo::PgoInstrumentationSchema* fgPgoSchema;
    BYTE*                                  fgPgoData;
    UINT32                                 fgPgoSchemaCount;
    HRESULT                                fgPgoQueryResult;
    UINT32                                 fgNumProfileRuns;
    UINT32                                 fgPgoBlockCounts;
    UINT32                                 fgPgoEdgeCounts;
    UINT32                                 fgPgoClassProfiles;
    unsigned                               fgPgoInlineePgo;
    unsigned                               fgPgoInlineeNoPgo;
    unsigned                               fgPgoInlineeNoPgoSingleBlock;

    void WalkSpanningTree(SpanningTreeVisitor* visitor);
    void fgSetProfileWeight(BasicBlock* block, BasicBlock::weight_t weight);
    void fgApplyProfileScale();

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

//-------- Insert a statement at the start or end of a basic block --------

#ifdef DEBUG
public:
    static bool fgBlockContainsStatementBounded(BasicBlock* block, Statement* stmt, bool answerOnBoundExceeded = true);
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

    GenTree* fgInsertCommaFormTemp(GenTree** use);
    GenTree* fgMakeMultiUse(GenTree** ppTree);

private:
    //                  Recognize a bitwise rotation pattern and convert into a GT_ROL or a GT_ROR node.
    GenTree* fgRecognizeAndMorphBitwiseRotation(GenTree* tree);
    bool fgOperIsBitwiseRotationRoot(genTreeOps oper);

    //-------- Determine the order in which the trees will be evaluated -------

    GenTree* fgSetTreeSeq(GenTree* tree, bool isLIR = false);
    void fgCheckTreeSeq(GenTree* tree, bool isLIR = false);
    void fgSetStmtSeq(Statement* stmt);
    void fgSetBlockOrder(BasicBlock* block);

    //------------------------- Morphing --------------------------------------

    unsigned fgPtrArgCntMax;

public:
    //------------------------------------------------------------------------
    // fgGetPtrArgCntMax: Return the maximum number of pointer-sized stack arguments that calls inside this method
    // can push on the stack. This value is calculated during morph.
    //
    // Return Value:
    //    Returns fgPtrArgCntMax, that is a private field.
    //
    unsigned fgGetPtrArgCntMax() const
    {
        return fgPtrArgCntMax;
    }

    //------------------------------------------------------------------------
    // fgSetPtrArgCntMax: Set the maximum number of pointer-sized stack arguments that calls inside this method
    // can push on the stack. This function is used during StackLevelSetter to fix incorrect morph calculations.
    //
    void fgSetPtrArgCntMax(unsigned argCntMax)
    {
        fgPtrArgCntMax = argCntMax;
    }

    bool compCanEncodePtrArgCntMax();

private:
#ifndef TARGET_X86
    hashBv* m_abiStructArgTemps;
    hashBv* m_abiStructArgTempsInUse;
#endif

#if REARRANGE_ADDS
    void fgMoveOpsLeft(GenTree* tree);
#endif

    bool fgIsCommaThrow(GenTree* tree, bool forFolding = false);

    bool fgIsThrow(GenTree* tree);

    bool fgInDifferentRegions(BasicBlock* blk1, BasicBlock* blk2);
    bool fgIsBlockCold(BasicBlock* block);

    GenTree* fgMorphCastIntoHelper(GenTreeCast* cast, int helper);

    GenTree* fgMorphIntoHelperCall(GenTree* tree, int helper, GenTreeCall::Use* args, bool morphArgs = true);

    // A "MorphAddrContext" carries information from the surrounding context.  If we are evaluating a byref address,
    // it is useful to know whether the address will be immediately dereferenced, or whether the address value will
    // be used, perhaps by passing it as an argument to a called method.  This affects how null checking is done:
    // for sufficiently small offsets, we can rely on OS page protection to implicitly null-check addresses that we
    // know will be dereferenced.  To know that reliance on implicit null checking is sound, we must further know that
    // all offsets between the top-level indirection and the bottom are constant, and that their sum is sufficiently
    // small; hence the other fields of MorphAddrContext.
    enum MorphAddrContextKind
    {
        MACK_Ind,
        MACK_Addr,
    };
    struct MorphAddrContext
    {
        MorphAddrContextKind m_kind;
        bool                 m_allConstantOffsets; // Valid only for "m_kind == MACK_Ind".  True iff all offsets between
                                                   // top-level indirection and here have been constants.
        size_t m_totalOffset; // Valid only for "m_kind == MACK_Ind", and if "m_allConstantOffsets" is true.
                              // In that case, is the sum of those constant offsets.

        MorphAddrContext(MorphAddrContextKind kind) : m_kind(kind), m_allConstantOffsets(true), m_totalOffset(0)
        {
        }
    };

#ifdef FEATURE_SIMD
    class SIMDCoalescingBuffer
    {
        Statement* m_firstStmt;
        Statement* m_lastStmt;
        unsigned   m_lclNum;
        unsigned   m_index;

        unsigned IsSimdLocalField(GenTree* node);
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

    SIMDCoalescingBuffer m_impSIMDCoalescingBuffer;
#endif // FEATURE_SIMD

    GenTree* fgMorphArrayIndex(GenTreeIndex* tree);
    GenTree* fgMorphCast(GenTreeCast* cast);
    void fgInitArgInfo(GenTreeCall* call);
    GenTreeCall* fgMorphArgs(GenTreeCall* call);

    GenTree* fgMorphLocalVar(GenTree* tree, bool forceRemorph);

public:
    bool fgAddrCouldBeNull(GenTree* addr);

private:
    GenTree* fgMorphField(GenTree* tree, MorphAddrContext* mac);
    bool fgCanFastTailCall(GenTreeCall* call, const char** failReason);
#if FEATURE_FASTTAILCALL
    bool fgCallHasMustCopyByrefParameter(CallInfo* callInfo);
#endif
    bool     fgCheckStmtAfterTailCall();
    GenTree* fgMorphTailCallViaHelpers(GenTreeCall* call, CORINFO_TAILCALL_HELPERS& help);
    bool fgCanTailCallViaJitHelper();
    void fgMorphTailCallViaJitHelper(GenTreeCall* call);
    GenTree* fgCreateCallDispatcherAndGetResult(GenTreeCall*          origCall,
                                                CORINFO_METHOD_HANDLE callTargetStubHnd,
                                                CORINFO_METHOD_HANDLE dispatcherHnd);
    GenTree* getLookupTree(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                           CORINFO_LOOKUP*         pLookup,
                           GenTreeFlags            handleFlags,
                           void*                   compileTimeHandle);
    GenTree* getRuntimeLookupTree(CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                  CORINFO_LOOKUP*         pLookup,
                                  void*                   compileTimeHandle);
    GenTree* getVirtMethodPointerTree(GenTree*                thisPtr,
                                      CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                      CORINFO_CALL_INFO*      pCallInfo);
    GenTree* getTokenHandleTree(CORINFO_RESOLVED_TOKEN* pResolvedToken, bool parent);

    GenTree* fgMorphPotentialTailCall(GenTreeCall* call);
    GenTree* fgGetStubAddrArg(GenTreeCall* call);
    void fgMorphRecursiveFastTailCallIntoLoop(BasicBlock* block, GenTreeCall* recursiveTailCall);
    Statement* fgAssignRecursiveCallArgToCallerParam(GenTree*       arg,
                                                     fgArgTabEntry* argTabEntry,
                                                     BasicBlock*    block,
                                                     IL_OFFSETX     callILOffset,
                                                     Statement*     tmpAssignmentInsertionPoint,
                                                     Statement*     paramAssignmentInsertionPoint);
    GenTree* fgMorphCall(GenTreeCall* call);
    GenTree* fgExpandVirtualVtableCallTarget(GenTreeCall* call);

    GenTree* fgOptimizeDelegateConstructor(GenTreeCall*            call,
                                           CORINFO_CONTEXT_HANDLE* ExactContextHnd,
                                           CORINFO_RESOLVED_TOKEN* ldftnToken);
    GenTree* fgMorphLeaf(GenTree* tree);
    void fgAssignSetVarDef(GenTree* tree);
    GenTree* fgMorphInitBlock(GenTreeOp* asg);
    GenTree* fgMorphPromoteLocalInitBlock(LclVarDsc* destLclVar, GenTree* initVal);
    GenTree* fgMorphInitBlockConstant(GenTreeIntCon* initVal,
                                      var_types      type,
                                      bool           extendToActualType,
                                      var_types      simdBaseType);
    GenTree* fgMorphBlkNode(GenTree* tree, bool isDest);
    GenTree* fgMorphStructAssignment(GenTreeOp* asg);
#ifdef FEATURE_SIMD
    GenTreeOp* fgMorphPromoteSimdAssignmentSrc(GenTreeOp* asg, unsigned srcLclNum);
    GenTreeOp* fgMorphPromoteSimdAssignmentDst(GenTreeOp* asg, unsigned destLclNum);
#endif
    GenTree* fgMorphCopyBlock(GenTreeOp* asg);
    GenTree* fgMorphForRegisterFP(GenTree* tree);
    GenTree* fgMorphSmpOp(GenTree* tree, MorphAddrContext* mac = nullptr);
    GenTree* fgMorphRetInd(GenTreeUnOp* ret);
    GenTree* fgMorphModToSubMulDiv(GenTreeOp* tree);
    GenTree* fgMorphSmpOpOptional(GenTreeOp* tree);
    GenTree* fgMorphConst(GenTree* tree);

    GenTreeLclVar* fgMorphTryFoldObjAsLclVar(GenTreeObj* obj);
    GenTree* fgMorphAssociative(GenTreeOp* tree);

#ifdef FEATURE_HW_INTRINSICS
    GenTree* fgMorphHWIntrinsic(GenTreeHWIntrinsic* tree);
#endif

public:
    GenTree* fgMorphTree(GenTree* tree, MorphAddrContext* mac = nullptr);

    INDEBUG(void fgMorphClearDebugNodeMorphed(GenTree* tree);)

private:
#if LOCAL_ASSERTION_PROP
    void fgKillDependentAssertionsSingle(unsigned lclNum DEBUGARG(GenTree* tree));
    void fgKillDependentAssertions(unsigned lclNum DEBUGARG(GenTree* tree));
#endif
    void fgMorphTreeDone(GenTree* tree, GenTree* oldTree = nullptr DEBUGARG(int morphNum = 0));

    Statement* fgMorphStmt;

    unsigned fgGetLargeFieldOffsetNullCheckTemp(var_types type); // We cache one temp per type to be
                                                                 // used when morphing big offset.

    //----------------------- Liveness analysis -------------------------------

    VARSET_TP fgCurUseSet; // vars used     by block (before an assignment)
    VARSET_TP fgCurDefSet; // vars assigned by block (before a use)

    MemoryKindSet fgCurMemoryUse;   // True iff the current basic block uses memory.
    MemoryKindSet fgCurMemoryDef;   // True iff the current basic block modifies memory.
    MemoryKindSet fgCurMemoryHavoc; // True if  the current basic block is known to set memory to a "havoc" value.

    bool byrefStatesMatchGcHeapStates; // True iff GcHeap and ByrefExposed memory have all the same def points.

    void fgMarkUseDef(GenTreeLclVarCommon* tree);

    void fgBeginScopeLife(VARSET_TP* inScope, VarScopeDsc* var);
    void fgEndScopeLife(VARSET_TP* inScope, VarScopeDsc* var);

    void fgMarkInScope(BasicBlock* block, VARSET_VALARG_TP inScope);
    void fgUnmarkInScope(BasicBlock* block, VARSET_VALARG_TP unmarkScope);

    void fgExtendDbgScopes();
    void fgExtendDbgLifetimes();

#ifdef DEBUG
    void fgDispDebugScopes();
#endif // DEBUG

    //-------------------------------------------------------------------------
    //
    //  The following keeps track of any code we've added for things like array
    //  range checking or explicit calls to enable GC, and so on.
    //
public:
    struct AddCodeDsc
    {
        AddCodeDsc*     acdNext;
        BasicBlock*     acdDstBlk; // block  to  which we jump
        unsigned        acdData;
        SpecialCodeKind acdKind; // what kind of a special block is this?
#if !FEATURE_FIXED_OUT_ARGS
        bool     acdStkLvlInit; // has acdStkLvl value been already set?
        unsigned acdStkLvl;     // stack level in stack slots.
#endif                          // !FEATURE_FIXED_OUT_ARGS
    };

private:
    static unsigned acdHelper(SpecialCodeKind codeKind);

    AddCodeDsc* fgAddCodeList;
    bool        fgAddCodeModf;
    bool        fgRngChkThrowAdded;
    AddCodeDsc* fgExcptnTargetCache[SCK_COUNT];

    BasicBlock* fgGetRngChkTarget(BasicBlock* block, SpecialCodeKind kind);

    BasicBlock* fgAddCodeRef(BasicBlock* srcBlk, unsigned refData, SpecialCodeKind kind);

public:
    AddCodeDsc* fgFindExcptnTarget(SpecialCodeKind kind, unsigned refData);

    bool fgUseThrowHelperBlocks();

    AddCodeDsc* fgGetAdditionalCodeDescriptors()
    {
        return fgAddCodeList;
    }

    void inlReplaceRetExpr(Statement* stmt);
    void inlFoldJTrue(BasicBlock* block);
    bool inlInlineCall(Statement* stmt, GenTreeCall* call);
    void inlInvokeInlineeCompiler(Statement* stmt, GenTreeCall* call, InlineResult* result);
    void inlAnalyzeInlineeReturn(InlineInfo* inlineInfo, unsigned returnBlockCount);
    bool inlImportReturn(InlineInfo* inlineInfo, GenTree* op2, CORINFO_CLASS_HANDLE retClsHnd);
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
    bool inlCanDiscardArgSideEffects(GenTree* argNode);
    Statement* inlInitInlineeLocals(const InlineInfo* inlineInfo, Statement* afterStmt);
    void inlNullOutInlineeGCLocals(const InlineInfo* inlineInfo, Statement* stmt);
    BasicBlock* inlSplitInlinerBlock(const InlineInfo* inlineInfo, Statement* stmtAfter);
    void inlInsertInlineeBlocks(const InlineInfo* inlineInfo, Statement* stmtAfter);
    void inlPropagateInlineeCompilerState();
    INDEBUG(void inlDebugCheckInlineCandidates();)

private:
    bool fgIsCodeAdded();

    bool fgIsThrowHlpBlk(BasicBlock* block);

#if !FEATURE_FIXED_OUT_ARGS
    unsigned fgThrowHlpBlkStkLevel(BasicBlock* block);
#endif // !FEATURE_FIXED_OUT_ARGS

    unsigned fgLargeFieldOffsetNullCheckTemps[TYP_COUNT];

#ifdef DEBUG
    void               CheckNoTransformableIndirectCallsRemain();
    static fgWalkPreFn fgDebugCheckForTransformableIndirectCalls;
#endif

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

#if (defined(TARGET_AMD64) && !defined(UNIX_AMD64_ABI)) || defined(TARGET_ARM64) || defined(TARGET_X86)
    // Rewrite appearances of implicit byrefs (manifest the implied additional level of indirection)
    // or stack params of x86 varargs methods.
    void fgMorphIndirectParams(Statement* stmt);
#endif

    static fgWalkPreFn  fgUpdateSideEffectsPre;
    static fgWalkPostFn fgUpdateSideEffectsPost;

    enum TypeProducerKind
    {
        TPK_Unknown = 0, // May not be a RuntimeType
        TPK_Handle  = 1, // RuntimeType via handle
        TPK_GetType = 2, // RuntimeType via Object.get_Type()
        TPK_Null    = 3, // Tree value is null
        TPK_Other   = 4  // RuntimeType via other means
    };

    TypeProducerKind gtGetTypeProducerKind(GenTree* tree);
    bool gtIsTypeHandleToRuntimeTypeHelper(GenTreeCall* call);
    bool gtIsTypeHandleToRuntimeTypeHandleHelper(GenTreeCall* call, CorInfoHelpFunc* pHelper = nullptr);
    bool gtIsActiveCSE_Candidate(GenTree* tree);

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
    void optInit();

    GenTree* Compiler::optRemoveRangeCheck(GenTreeBoundsChk* check, GenTree* comma, Statement* stmt);
    GenTree* Compiler::optRemoveStandaloneRangeCheck(GenTreeBoundsChk* check, Statement* stmt);
    void Compiler::optRemoveCommaBasedRangeCheck(GenTree* comma, Statement* stmt);
    bool optIsRangeCheckRemovable(GenTree* tree);

protected:
    static fgWalkPreFn optValidRangeCheckIndex;

    /**************************************************************************
     *
     *************************************************************************/

protected:
    // Do hoisting for all loops.
    void optHoistLoopCode();

    // To represent sets of VN's that have already been hoisted in outer loops.
    typedef JitHashTable<ValueNum, JitSmallPrimitiveKeyFuncs<ValueNum>, bool> VNToBoolMap;
    typedef VNToBoolMap VNSet;

    struct LoopHoistContext
    {
    private:
        // The set of variables hoisted in the current loop (or nullptr if there are none).
        VNSet* m_pHoistedInCurLoop;

    public:
        // Value numbers of expressions that have been hoisted in parent loops in the loop nest.
        VNSet m_hoistedInParentLoops;
        // Value numbers of expressions that have been hoisted in the current (or most recent) loop in the nest.
        // Previous decisions on loop-invariance of value numbers in the current loop.
        VNToBoolMap m_curLoopVnInvariantCache;

        VNSet* GetHoistedInCurLoop(Compiler* comp)
        {
            if (m_pHoistedInCurLoop == nullptr)
            {
                m_pHoistedInCurLoop = new (comp->getAllocatorLoopHoist()) VNSet(comp->getAllocatorLoopHoist());
            }
            return m_pHoistedInCurLoop;
        }

        VNSet* ExtractHoistedInCurLoop()
        {
            VNSet* res          = m_pHoistedInCurLoop;
            m_pHoistedInCurLoop = nullptr;
            return res;
        }

        LoopHoistContext(Compiler* comp)
            : m_pHoistedInCurLoop(nullptr)
            , m_hoistedInParentLoops(comp->getAllocatorLoopHoist())
            , m_curLoopVnInvariantCache(comp->getAllocatorLoopHoist())
        {
        }
    };

    // Do hoisting for loop "lnum" (an index into the optLoopTable), and all loops nested within it.
    // Tracks the expressions that have been hoisted by containing loops by temporary recording their
    // value numbers in "m_hoistedInParentLoops".  This set is not modified by the call.
    void optHoistLoopNest(unsigned lnum, LoopHoistContext* hoistCtxt);

    // Do hoisting for a particular loop ("lnum" is an index into the optLoopTable.)
    // Assumes that expressions have been hoisted in containing loops if their value numbers are in
    // "m_hoistedInParentLoops".
    //
    void optHoistThisLoop(unsigned lnum, LoopHoistContext* hoistCtxt);

    // Hoist all expressions in "blk" that are invariant in loop "lnum" (an index into the optLoopTable)
    // outside of that loop.  Exempt expressions whose value number is in "m_hoistedInParentLoops"; add VN's of hoisted
    // expressions to "hoistInLoop".
    void optHoistLoopBlocks(unsigned loopNum, ArrayStack<BasicBlock*>* blocks, LoopHoistContext* hoistContext);

    // Return true if the tree looks profitable to hoist out of loop 'lnum'.
    bool optIsProfitableToHoistableTree(GenTree* tree, unsigned lnum);

    // Performs the hoisting 'tree' into the PreHeader for loop 'lnum'
    void optHoistCandidate(GenTree* tree, unsigned lnum, LoopHoistContext* hoistCtxt);

    // Returns true iff the ValueNum "vn" represents a value that is loop-invariant in "lnum".
    //   Constants and init values are always loop invariant.
    //   VNPhi's connect VN's to the SSA definition, so we can know if the SSA def occurs in the loop.
    bool optVNIsLoopInvariant(ValueNum vn, unsigned lnum, VNToBoolMap* recordedVNs);

    // If "blk" is the entry block of a natural loop, returns true and sets "*pLnum" to the index of the loop
    // in the loop table.
    bool optBlockIsLoopEntry(BasicBlock* blk, unsigned* pLnum);

    // Records the set of "side effects" of all loops: fields (object instance and static)
    // written to, and SZ-array element type equivalence classes updated.
    void optComputeLoopSideEffects();

private:
    // Requires "lnum" to be the index of an outermost loop in the loop table.  Traverses the body of that loop,
    // including all nested loops, and records the set of "side effects" of the loop: fields (object instance and
    // static) written to, and SZ-array element type equivalence classes updated.
    void optComputeLoopNestSideEffects(unsigned lnum);

    // Given a loop number 'lnum' mark it and any nested loops as having 'memoryHavoc'
    void optRecordLoopNestsMemoryHavoc(unsigned lnum, MemoryKindSet memoryHavoc);

    // Add the side effects of "blk" (which is required to be within a loop) to all loops of which it is a part.
    // Returns false if we encounter a block that is not marked as being inside a loop.
    //
    bool optComputeLoopSideEffectsOfBlock(BasicBlock* blk);

    // Hoist the expression "expr" out of loop "lnum".
    void optPerformHoistExpr(GenTree* expr, unsigned lnum);

public:
    void optOptimizeBools();

private:
    GenTree* optIsBoolCond(GenTree* condBranch, GenTree** compPtr, bool* boolPtr);
#ifdef DEBUG
    void optOptimizeBoolsGcStress(BasicBlock* condBlock);
#endif
public:
    PhaseStatus optInvertLoops();    // Invert loops so they're entered at top and tested at bottom.
    PhaseStatus optOptimizeLayout(); // Optimize the BasicBlock layout of the method
    PhaseStatus optFindLoops();      // Finds loops and records them in the loop table

    PhaseStatus optCloneLoops();
    void optCloneLoop(unsigned loopInd, LoopCloneContext* context);
    void optEnsureUniqueHead(unsigned loopInd, BasicBlock::weight_t ambientWeight);
    PhaseStatus optUnrollLoops(); // Unrolls loops (needs to have cost info)
    void        optRemoveRedundantZeroInits();

protected:
    // This enumeration describes what is killed by a call.

    enum callInterf
    {
        CALLINT_NONE,       // no interference                               (most helpers)
        CALLINT_REF_INDIRS, // kills GC ref indirections                     (SETFIELD OBJ)
        CALLINT_SCL_INDIRS, // kills non GC ref indirections                 (SETFIELD non-OBJ)
        CALLINT_ALL_INDIRS, // kills both GC ref and non GC ref indirections (SETFIELD STRUCT)
        CALLINT_ALL,        // kills everything                              (normal method call)
    };

public:
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

        callInterf   lpAsgCall;     // "callInterf" for calls in the loop
        ALLVARSET_TP lpAsgVars;     // set of vars assigned within the loop (all vars, not just tracked)
        varRefKinds  lpAsgInds : 8; // set of inds modified within the loop

        LoopFlags lpFlags;

        unsigned char lpExitCnt; // number of exits from the loop

        unsigned char lpParent;  // The index of the most-nested loop that completely contains this one,
                                 // or else BasicBlock::NOT_IN_LOOP if no such loop exists.
        unsigned char lpChild;   // The index of a nested loop, or else BasicBlock::NOT_IN_LOOP if no child exists.
                                 // (Actually, an "immediately" nested loop --
                                 // no other child of this loop is a parent of lpChild.)
        unsigned char lpSibling; // The index of another loop that is an immediate child of lpParent,
                                 // or else BasicBlock::NOT_IN_LOOP.  One can enumerate all the children of a loop
                                 // by following "lpChild" then "lpSibling" links.

        bool lpLoopHasMemoryHavoc[MemoryKindCount]; // The loop contains an operation that we assume has arbitrary
                                                    // memory side effects.  If this is set, the fields below
                                                    // may not be accurate (since they become irrelevant.)
        bool lpContainsCall;                        // True if executing the loop body *may* execute a call

        VARSET_TP lpVarInOut;  // The set of variables that are IN or OUT during the execution of this loop
        VARSET_TP lpVarUseDef; // The set of variables that are USE or DEF during the execution of this loop

        int lpHoistedExprCount; // The register count for the non-FP expressions from inside this loop that have been
                                // hoisted
        int lpLoopVarCount;     // The register count for the non-FP LclVars that are read/written inside this loop
        int lpVarInOutCount;    // The register count for the non-FP LclVars that are alive inside or across this loop

        int lpHoistedFPExprCount; // The register count for the FP expressions from inside this loop that have been
                                  // hoisted
        int lpLoopVarFPCount;     // The register count for the FP LclVars that are read/written inside this loop
        int lpVarInOutFPCount;    // The register count for the FP LclVars that are alive inside or across this loop

        typedef JitHashTable<CORINFO_FIELD_HANDLE, JitPtrKeyFuncs<struct CORINFO_FIELD_STRUCT_>, bool> FieldHandleSet;
        FieldHandleSet* lpFieldsModified; // This has entries (mappings to "true") for all static field and object
                                          // instance fields modified
                                          // in the loop.

        // The set of array element types that are modified in the loop.
        typedef JitHashTable<unsigned, JitSmallPrimitiveKeyFuncs<unsigned>, bool> TypeNumSet;
        TypeNumSet* lpArrayElemTypesModified;

        // Adds the variable liveness information for 'blk' to 'this' LoopDsc
        void AddVariableLiveness(Compiler* comp, BasicBlock* blk);

        inline void AddModifiedField(Compiler* comp, CORINFO_FIELD_HANDLE fldHnd);
        inline void AddModifiedElemType(Compiler* comp, unsigned elemTypeNum);

        /* The following values are set only for iterator loops, i.e. has the flag LPFLG_ITER set */

        GenTree*   lpIterTree;          // The "i = i <op> const" tree
        unsigned   lpIterVar() const;   // iterator variable #
        int        lpIterConst() const; // the constant with which the iterator is incremented
        genTreeOps lpIterOper() const;  // the type of the operation on the iterator (ASG_ADD, ASG_SUB, etc.)
        void       VERIFY_lpIterTree() const;

        var_types lpIterOperType() const; // For overflow instructions

        union {
            int lpConstInit;    // initial constant value of iterator
                                // : Valid if LPFLG_CONST_INIT
            unsigned lpVarInit; // initial local var number to which we initialize the iterator
                                // : Valid if LPFLG_VAR_INIT
        };

        // The following is for LPFLG_ITER loops only (i.e. the loop condition is "i RELOP const or var"

        GenTree*   lpTestTree;         // pointer to the node containing the loop test
        genTreeOps lpTestOper() const; // the type of the comparison between the iterator and the limit (GT_LE, GT_GE,
                                       // etc.)
        void VERIFY_lpTestTree() const;

        bool     lpIsReversed() const; // true if the iterator node is the second operand in the loop condition
        GenTree* lpIterator() const;   // the iterator node in the loop test
        GenTree* lpLimit() const;      // the limit node in the loop test

        // Limit constant value of iterator - loop condition is "i RELOP const"
        // : Valid if LPFLG_CONST_LIMIT
        int lpConstLimit() const;

        // The lclVar # in the loop condition ( "i RELOP lclVar" )
        // : Valid if LPFLG_VAR_LIMIT
        unsigned lpVarLimit() const;

        // The array length in the loop condition ( "i RELOP arr.len" or "i RELOP arr[i][j].len" )
        // : Valid if LPFLG_ARRLEN_LIMIT
        bool lpArrLenLimit(Compiler* comp, ArrIndex* index) const;

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
    };

protected:
    bool fgMightHaveLoop(); // returns true if there are any backedges
    bool fgHasLoops;        // True if this method has any loops, set in fgComputeReachability

public:
    LoopDsc*      optLoopTable; // loop descriptor table
    unsigned char optLoopCount; // number of tracked loops

#ifdef DEBUG
    unsigned char loopAlignCandidates; // number of loops identified for alignment
    unsigned char loopsAligned;        // number of loops actually aligned
#endif                                 // DEBUG

    bool optRecordLoop(BasicBlock*   head,
                       BasicBlock*   first,
                       BasicBlock*   top,
                       BasicBlock*   entry,
                       BasicBlock*   bottom,
                       BasicBlock*   exit,
                       unsigned char exitCnt);

protected:
    unsigned optCallCount;         // number of calls made in the method
    unsigned optIndirectCallCount; // number of virtual, interface and indirect calls made in the method
    unsigned optNativeCallCount;   // number of Pinvoke/Native calls made in the method
    unsigned optLoopsCloned;       // number of loops cloned in the current method.

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
    bool optCheckIterInLoopTest(unsigned loopInd, GenTree* test, BasicBlock* from, BasicBlock* to, unsigned iterVar);
    bool optComputeIterInfo(GenTree* incr, BasicBlock* from, BasicBlock* to, unsigned* pIterVar);
    bool optPopulateInitInfo(unsigned loopInd, GenTree* init, unsigned iterVar);
    bool optExtractInitTestIncr(
        BasicBlock* head, BasicBlock* bottom, BasicBlock* exit, GenTree** ppInit, GenTree** ppTest, GenTree** ppIncr);

    void optFindNaturalLoops();

    void optIdentifyLoopsForAlignment();

    // Ensures that all the loops in the loop nest rooted at "loopInd" (an index into the loop table) are 'canonical' --
    // each loop has a unique "top."  Returns "true" iff the flowgraph has been modified.
    bool optCanonicalizeLoopNest(unsigned char loopInd);

    // Ensures that the loop "loopInd" (an index into the loop table) is 'canonical' -- it has a unique "top,"
    // unshared with any other loop.  Returns "true" iff the flowgraph has been modified
    bool optCanonicalizeLoop(unsigned char loopInd);

    // Requires "l1" to be a valid loop table index, and not "BasicBlock::NOT_IN_LOOP".  Requires "l2" to be
    // a valid loop table index, or else "BasicBlock::NOT_IN_LOOP".  Returns true
    // iff "l2" is not NOT_IN_LOOP, and "l1" contains "l2".
    bool optLoopContains(unsigned l1, unsigned l2);

    // Updates the loop table by changing loop "loopInd", whose head is required
    // to be "from", to be "to".  Also performs this transformation for any
    // loop nested in "loopInd" that shares the same head as "loopInd".
    void optUpdateLoopHead(unsigned loopInd, BasicBlock* from, BasicBlock* to);

    void optRedirectBlock(BasicBlock* blk, BlockToBlockMap* redirectMap, const bool updatePreds = false);

    // Marks the containsCall information to "lnum" and any parent loops.
    void AddContainsCallAllContainingLoops(unsigned lnum);

    // Adds the variable liveness information from 'blk' to "lnum" and any parent loops.
    void AddVariableLivenessAllContainingLoops(unsigned lnum, BasicBlock* blk);

    // Adds "fldHnd" to the set of modified fields of "lnum" and any parent loops.
    void AddModifiedFieldAllContainingLoops(unsigned lnum, CORINFO_FIELD_HANDLE fldHnd);

    // Adds "elemType" to the set of modified array element types of "lnum" and any parent loops.
    void AddModifiedElemTypeAllContainingLoops(unsigned lnum, unsigned elemTypeNum);

    // Requires that "from" and "to" have the same "bbJumpKind" (perhaps because "to" is a clone
    // of "from".)  Copies the jump destination from "from" to "to".
    void optCopyBlkDest(BasicBlock* from, BasicBlock* to);

    // Returns true if 'block' is an entry block for any loop in 'optLoopTable'
    bool optIsLoopEntry(BasicBlock* block) const;

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

    // Struct used in optInvertWhileLoop to count interesting constructs to boost the profitability score.
    struct OptInvertCountTreeInfoType
    {
        int sharedStaticHelperCount;
        int arrayLengthCount;
    };

    static fgWalkResult optInvertCountTreeInfo(GenTree** pTree, fgWalkData* data);

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

    static fgWalkPreFn optIsVarAssgCB;

protected:
    bool optIsVarAssigned(BasicBlock* beg, BasicBlock* end, GenTree* skip, unsigned var);

    bool optIsVarAssgLoop(unsigned lnum, unsigned var);

    int optIsSetAssgLoop(unsigned lnum, ALLVARSET_VALARG_TP vars, varRefKinds inds = VR_NONE);

    bool optNarrowTree(GenTree* tree, var_types srct, var_types dstt, ValueNumPair vnpNarrow, bool doit);

    /**************************************************************************
     *                       Optimization conditions
     *************************************************************************/

    bool optAvoidIntMult(void);

protected:
    //  The following is the upper limit on how many expressions we'll keep track
    //  of for the CSE analysis.
    //
    static const unsigned MAX_CSE_CNT = EXPSET_SZ;

    static const int MIN_CSE_COST = 2;

    // BitVec trait information only used by the optCSE_canSwap() method, for the  CSE_defMask and CSE_useMask.
    // This BitVec uses one bit per CSE candidate
    BitVecTraits* cseMaskTraits; // one bit per CSE candidate

    // BitVec trait information for computing CSE availability using the CSE_DataFlow algorithm.
    // Two bits are allocated per CSE candidate to compute CSE availability
    // plus an extra bit to handle the initial unvisited case.
    // (See CSE_DataFlow::EndMerge for an explaination of why this is necessary)
    //
    // The two bits per CSE candidate have the following meanings:
    //     11 - The CSE is available, and is also available when considering calls as killing availability.
    //     10 - The CSE is available, but is not available when considering calls as killing availability.
    //     00 - The CSE is not available
    //     01 - An illegal combination
    //
    BitVecTraits* cseLivenessTraits;

    EXPSET_TP cseCallKillsMask; // Computed once - A mask that is used to kill available CSEs at callsites

    /* Generic list of nodes - used by the CSE logic */

    struct treeStmtLst
    {
        treeStmtLst* tslNext;
        GenTree*     tslTree;  // tree node
        Statement*   tslStmt;  // statement containing the tree
        BasicBlock*  tslBlock; // block containing the statement

        treeStmtLst(GenTree* tree, Statement* stmt, BasicBlock* block)
            : tslNext(nullptr), tslTree(tree), tslStmt(stmt), tslBlock(block)
        {
        }
    };

    // The following logic keeps track of expressions via a simple hash table.

    struct CSEdsc
    {
        CSEdsc*  csdNextInBucket;  // used by the hash table
        size_t   csdHashKey;       // the orginal hashkey
        ssize_t  csdConstDefValue; // When we CSE similar constants, this is the value that we use as the def
        ValueNum csdConstDefVN;    // When we CSE similar constants, this is the ValueNumber that we use for the LclVar
                                   // assignment
        unsigned csdIndex;         // 1..optCSECandidateCount
        bool     csdIsSharedConst; // true if this CSE is a shared const
        bool     csdLiveAcrossCall;

        unsigned short csdDefCount; // definition   count
        unsigned short csdUseCount; // use          count  (excluding the implicit uses at defs)

        BasicBlock::weight_t csdDefWtCnt; // weighted def count
        BasicBlock::weight_t csdUseWtCnt; // weighted use count  (excluding the implicit uses at defs)

        GenTree*    csdTree;  // treenode containing the 1st occurrence
        Statement*  csdStmt;  // stmt containing the 1st occurrence
        BasicBlock* csdBlock; // block containing the 1st occurrence

        treeStmtLst* csdTreeList; // list of matching tree nodes: head
        treeStmtLst* csdTreeLast; // list of matching tree nodes: tail

        ClassLayout* csdLayout; // Layout needed to create struct typed CSE temps.

        ValueNum defExcSetPromise; // The exception set that is now required for all defs of this CSE.
                                   // This will be set to NoVN if we decide to abandon this CSE

        ValueNum defExcSetCurrent; // The set of exceptions we currently can use for CSE uses.

        ValueNum defConservNormVN; // if all def occurrences share the same conservative normal value
                                   // number, this will reflect it; otherwise, NoVN.
                                   // not used for shared const CSE's

        CSEdsc(size_t hashKey, GenTree* tree, Statement* stmt, BasicBlock* block)
            : csdNextInBucket(nullptr)
            , csdHashKey(hashKey)
            , csdConstDefValue(0)
            , csdConstDefVN(ValueNumStore::VNForNull())
            , csdIndex(0)
            , csdIsSharedConst(false)
            , csdLiveAcrossCall(false)
            , csdDefCount(0)
            , csdUseCount(0)
            , csdDefWtCnt(0)
            , csdUseWtCnt(0)
            , csdTree(tree)
            , csdStmt(stmt)
            , csdBlock(block)
            , csdTreeList(nullptr)
            , csdTreeLast(nullptr)
            , csdLayout(nullptr)
            , defExcSetPromise(ValueNumStore::VNForEmptyExcSet())
            , defExcSetCurrent(ValueNumStore::VNForNull())
            , defConservNormVN(ValueNumStore::VNForNull())
        {
        }
    };

    static const size_t s_optCSEhashSizeInitial;
    static const size_t s_optCSEhashGrowthFactor;
    static const size_t s_optCSEhashBucketSize;
    size_t              optCSEhashSize;                 // The current size of hashtable
    size_t              optCSEhashCount;                // Number of entries in hashtable
    size_t              optCSEhashMaxCountBeforeResize; // Number of entries before resize
    CSEdsc**            optCSEhash;
    CSEdsc**            optCSEtab;

    typedef JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, GenTree*> NodeToNodeMap;

    NodeToNodeMap* optCseCheckedBoundMap; // Maps bound nodes to ancestor compares that should be
                                          // re-numbered with the bound to improve range check elimination

    // Given a compare, look for a cse candidate checked bound feeding it and add a map entry if found.
    void optCseUpdateCheckedBoundMap(GenTree* compare);

    void optCSEstop();

    CSEdsc* optCSEfindDsc(unsigned index);
    bool optUnmarkCSE(GenTree* tree);

    // user defined callback data for the tree walk function optCSE_MaskHelper()
    struct optCSE_MaskData
    {
        EXPSET_TP CSE_defMask;
        EXPSET_TP CSE_useMask;
    };

    // Treewalk helper for optCSE_DefMask and optCSE_UseMask
    static fgWalkPreFn optCSE_MaskHelper;

    // This function walks all the node for an given tree
    // and return the mask of CSE definitions and uses for the tree
    //
    void optCSE_GetMaskData(GenTree* tree, optCSE_MaskData* pMaskData);

    // Given a binary tree node return true if it is safe to swap the order of evaluation for op1 and op2.
    bool optCSE_canSwap(GenTree* firstNode, GenTree* secondNode);
    bool optCSE_canSwap(GenTree* tree);

    struct optCSEcostCmpEx
    {
        bool operator()(const CSEdsc* op1, const CSEdsc* op2);
    };
    struct optCSEcostCmpSz
    {
        bool operator()(const CSEdsc* op1, const CSEdsc* op2);
    };

    void optCleanupCSEs();

#ifdef DEBUG
    void optEnsureClearCSEInfo();
#endif // DEBUG

    static bool Is_Shared_Const_CSE(size_t key)
    {
        return ((key & TARGET_SIGN_BIT) != 0);
    }

    // returns the encoded key
    static size_t Encode_Shared_Const_CSE_Value(size_t key)
    {
        return TARGET_SIGN_BIT | (key >> CSE_CONST_SHARED_LOW_BITS);
    }

    // returns the orginal key
    static size_t Decode_Shared_Const_CSE_Value(size_t enckey)
    {
        assert(Is_Shared_Const_CSE(enckey));
        return (enckey & ~TARGET_SIGN_BIT) << CSE_CONST_SHARED_LOW_BITS;
    }

    /**************************************************************************
     *                   Value Number based CSEs
     *************************************************************************/

public:
    void optOptimizeValnumCSEs();

protected:
    void     optValnumCSE_Init();
    unsigned optValnumCSE_Index(GenTree* tree, Statement* stmt);
    unsigned optValnumCSE_Locate();
    void     optValnumCSE_InitDataFlow();
    void     optValnumCSE_DataFlow();
    void     optValnumCSE_Availablity();
    void     optValnumCSE_Heuristic();

    bool                 optDoCSE;             // True when we have found a duplicate CSE tree
    bool                 optValnumCSE_phase;   // True when we are executing the optValnumCSE_phase
    unsigned             optCSECandidateTotal; // Grand total of CSE candidates for both Lexical and ValNum
    unsigned             optCSECandidateCount; // Count of CSE's candidates, reset for Lexical and ValNum CSE's
    unsigned             optCSEstart;          // The first local variable number that is a CSE
    unsigned             optCSEcount;          // The total count of CSE's introduced.
    BasicBlock::weight_t optCSEweight;         // The weight of the current block when we are doing PerformCSE

    bool optIsCSEcandidate(GenTree* tree);

    // lclNumIsTrueCSE returns true if the LclVar was introduced by the CSE phase of the compiler
    //
    bool lclNumIsTrueCSE(unsigned lclNum) const
    {
        return ((optCSEcount > 0) && (lclNum >= optCSEstart) && (lclNum < optCSEstart + optCSEcount));
    }

    //  lclNumIsCSE returns true if the LclVar should be treated like a CSE with regards to constant prop.
    //
    bool lclNumIsCSE(unsigned lclNum) const
    {
        return lvaTable[lclNum].lvIsCSE;
    }

#ifdef DEBUG
    bool optConfigDisableCSE();
    bool optConfigDisableCSE2();
#endif
    void optOptimizeCSEs();

    struct isVarAssgDsc
    {
        GenTree*     ivaSkip;
        ALLVARSET_TP ivaMaskVal; // Set of variables assigned to.  This is a set of all vars, not tracked vars.
#ifdef DEBUG
        void* ivaSelf;
#endif
        unsigned    ivaVar;            // Variable we are interested in, or -1
        varRefKinds ivaMaskInd;        // What kind of indirect assignments are there?
        callInterf  ivaMaskCall;       // What kind of calls are there?
        bool        ivaMaskIncomplete; // Variables not representable in ivaMaskVal were assigned to.
    };

    static callInterf optCallInterf(GenTreeCall* call);

public:
    // VN based copy propagation.
    typedef ArrayStack<GenTree*> GenTreePtrStack;
    typedef JitHashTable<unsigned, JitSmallPrimitiveKeyFuncs<unsigned>, GenTreePtrStack*> LclNumToGenTreePtrStack;

    // Kill set to track variables with intervening definitions.
    VARSET_TP optCopyPropKillSet;

    // Copy propagation functions.
    void optCopyProp(BasicBlock* block, Statement* stmt, GenTree* tree, LclNumToGenTreePtrStack* curSsaName);
    void optBlockCopyPropPopStacks(BasicBlock* block, LclNumToGenTreePtrStack* curSsaName);
    void optBlockCopyProp(BasicBlock* block, LclNumToGenTreePtrStack* curSsaName);
    unsigned optIsSsaLocal(GenTree* tree);
    int optCopyProp_LclVarScore(LclVarDsc* lclVarDsc, LclVarDsc* copyVarDsc, bool preferOp2);
    void optVnCopyProp();
    INDEBUG(void optDumpCopyPropStack(LclNumToGenTreePtrStack* curSsaName));

    /**************************************************************************
     *               Early value propagation
     *************************************************************************/
    struct SSAName
    {
        unsigned m_lvNum;
        unsigned m_ssaNum;

        SSAName(unsigned lvNum, unsigned ssaNum) : m_lvNum(lvNum), m_ssaNum(ssaNum)
        {
        }

        static unsigned GetHashCode(SSAName ssaNm)
        {
            return (ssaNm.m_lvNum << 16) | (ssaNm.m_ssaNum);
        }

        static bool Equals(SSAName ssaNm1, SSAName ssaNm2)
        {
            return (ssaNm1.m_lvNum == ssaNm2.m_lvNum) && (ssaNm1.m_ssaNum == ssaNm2.m_ssaNum);
        }
    };

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
#define OMF_HAS_FROZEN_STRING 0x00000400    // Method has a frozen string (REF constant int), currently only on CoreRT.

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

    void addFatPointerCandidate(GenTreeCall* call);

    bool doesMethodHaveFrozenString() const
    {
        return (optMethodFlags & OMF_HAS_FROZEN_STRING) != 0;
    }

    void setMethodHasFrozenString()
    {
        optMethodFlags |= OMF_HAS_FROZEN_STRING;
    }

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

    void addExpRuntimeLookupCandidate(GenTreeCall* call);

    bool doesMethodHavePatchpoints()
    {
        return (optMethodFlags & OMF_HAS_PATCHPOINT) != 0;
    }

    void setMethodHasPatchpoint()
    {
        optMethodFlags |= OMF_HAS_PATCHPOINT;
    }

    unsigned optMethodFlags;

    bool doesMethodHaveNoReturnCalls()
    {
        return optNoReturnCallCount > 0;
    }

    void setMethodHasNoReturnCalls()
    {
        optNoReturnCallCount++;
    }

    unsigned optNoReturnCallCount;

    // Recursion bound controls how far we can go backwards tracking for a SSA value.
    // No throughput diff was found with backward walk bound between 3-8.
    static const int optEarlyPropRecurBound = 5;

    enum class optPropKind
    {
        OPK_INVALID,
        OPK_ARRAYLEN,
        OPK_NULLCHECK
    };

    typedef JitHashTable<unsigned, JitSmallPrimitiveKeyFuncs<unsigned>, GenTree*> LocalNumberToNullCheckTreeMap;

    bool gtIsVtableRef(GenTree* tree);
    GenTree* getArrayLengthFromAllocation(GenTree* tree DEBUGARG(BasicBlock* block));
    GenTree* getObjectHandleNodeFromAllocation(GenTree* tree DEBUGARG(BasicBlock* block));
    GenTree* optPropGetValueRec(unsigned lclNum, unsigned ssaNum, optPropKind valueKind, int walkDepth);
    GenTree* optPropGetValue(unsigned lclNum, unsigned ssaNum, optPropKind valueKind);
    GenTree* optEarlyPropRewriteTree(GenTree* tree, LocalNumberToNullCheckTreeMap* nullCheckMap);
    bool optDoEarlyPropForBlock(BasicBlock* block);
    bool optDoEarlyPropForFunc();
    void optEarlyProp();
    void optFoldNullCheck(GenTree* tree, LocalNumberToNullCheckTreeMap* nullCheckMap);
    GenTree* optFindNullCheckToFold(GenTree* tree, LocalNumberToNullCheckTreeMap* nullCheckMap);
    bool optIsNullCheckFoldingLegal(GenTree*    tree,
                                    GenTree*    nullCheckTree,
                                    GenTree**   nullCheckParent,
                                    Statement** nullCheckStmt);
    bool optCanMoveNullCheckPastTree(GenTree* tree,
                                     unsigned nullCheckLclNum,
                                     bool     isInsideTry,
                                     bool     checkSideEffectSummary);
#if DEBUG
    void optCheckFlagsAreSet(unsigned    methodFlag,
                             const char* methodFlagStr,
                             unsigned    bbFlag,
                             const char* bbFlagStr,
                             GenTree*    tree,
                             BasicBlock* basicBlock);
#endif

    // Redundant branch opts
    //
    PhaseStatus optRedundantBranches();
    bool optRedundantBranch(BasicBlock* const block);
    bool optJumpThread(BasicBlock* const block, BasicBlock* const domBlock);
    bool optReachable(BasicBlock* const fromBlock, BasicBlock* const toBlock, BasicBlock* const excludedBlock);

#if ASSERTION_PROP
    /**************************************************************************
     *               Value/Assertion propagation
     *************************************************************************/
public:
    // Data structures for assertion prop
    BitVecTraits* apTraits;
    ASSERT_TP     apFull;

    enum optAssertionKind
    {
        OAK_INVALID,
        OAK_EQUAL,
        OAK_NOT_EQUAL,
        OAK_SUBRANGE,
        OAK_NO_THROW,
        OAK_COUNT
    };

    enum optOp1Kind
    {
        O1K_INVALID,
        O1K_LCLVAR,
        O1K_ARR_BND,
        O1K_BOUND_OPER_BND,
        O1K_BOUND_LOOP_BND,
        O1K_CONSTANT_LOOP_BND,
        O1K_EXACT_TYPE,
        O1K_SUBTYPE,
        O1K_VALUE_NUMBER,
        O1K_COUNT
    };

    enum optOp2Kind
    {
        O2K_INVALID,
        O2K_LCLVAR_COPY,
        O2K_IND_CNS_INT,
        O2K_CONST_INT,
        O2K_CONST_LONG,
        O2K_CONST_DOUBLE,
        O2K_ARR_LEN,
        O2K_SUBRANGE,
        O2K_COUNT
    };
    struct AssertionDsc
    {
        optAssertionKind assertionKind;
        struct SsaVar
        {
            unsigned lclNum; // assigned to or property of this local var number
            unsigned ssaNum;
        };
        struct ArrBnd
        {
            ValueNum vnIdx;
            ValueNum vnLen;
        };
        struct AssertionDscOp1
        {
            optOp1Kind kind; // a normal LclVar, or Exact-type or Subtype
            ValueNum   vn;
            union {
                SsaVar lcl;
                ArrBnd bnd;
            };
        } op1;
        struct AssertionDscOp2
        {
            optOp2Kind kind; // a const or copy assignment
            ValueNum   vn;
            struct IntVal
            {
                ssize_t      iconVal;   // integer
                unsigned     padding;   // unused; ensures iconFlags does not overlap lconVal
                GenTreeFlags iconFlags; // gtFlags
            };
            struct Range // integer subrange
            {
                ssize_t loBound;
                ssize_t hiBound;
            };
            union {
                SsaVar  lcl;
                IntVal  u1;
                __int64 lconVal;
                double  dconVal;
                Range   u2;
            };
        } op2;

        bool IsCheckedBoundArithBound()
        {
            return ((assertionKind == OAK_EQUAL || assertionKind == OAK_NOT_EQUAL) && op1.kind == O1K_BOUND_OPER_BND);
        }
        bool IsCheckedBoundBound()
        {
            return ((assertionKind == OAK_EQUAL || assertionKind == OAK_NOT_EQUAL) && op1.kind == O1K_BOUND_LOOP_BND);
        }
        bool IsConstantBound()
        {
            return ((assertionKind == OAK_EQUAL || assertionKind == OAK_NOT_EQUAL) &&
                    op1.kind == O1K_CONSTANT_LOOP_BND);
        }
        bool IsBoundsCheckNoThrow()
        {
            return ((assertionKind == OAK_NO_THROW) && (op1.kind == O1K_ARR_BND));
        }

        bool IsCopyAssertion()
        {
            return ((assertionKind == OAK_EQUAL) && (op1.kind == O1K_LCLVAR) && (op2.kind == O2K_LCLVAR_COPY));
        }

        bool IsConstantInt32Assertion()
        {
            return ((assertionKind == OAK_EQUAL) || (assertionKind == OAK_NOT_EQUAL)) && (op2.kind == O2K_CONST_INT);
        }

        static bool SameKind(AssertionDsc* a1, AssertionDsc* a2)
        {
            return a1->assertionKind == a2->assertionKind && a1->op1.kind == a2->op1.kind &&
                   a1->op2.kind == a2->op2.kind;
        }

        static bool ComplementaryKind(optAssertionKind kind, optAssertionKind kind2)
        {
            if (kind == OAK_EQUAL)
            {
                return kind2 == OAK_NOT_EQUAL;
            }
            else if (kind == OAK_NOT_EQUAL)
            {
                return kind2 == OAK_EQUAL;
            }
            return false;
        }

        static ssize_t GetLowerBoundForIntegralType(var_types type)
        {
            switch (type)
            {
                case TYP_BYTE:
                    return SCHAR_MIN;
                case TYP_SHORT:
                    return SHRT_MIN;
                case TYP_INT:
                    return INT_MIN;
                case TYP_BOOL:
                case TYP_UBYTE:
                case TYP_USHORT:
                case TYP_UINT:
                    return 0;
                default:
                    unreached();
            }
        }
        static ssize_t GetUpperBoundForIntegralType(var_types type)
        {
            switch (type)
            {
                case TYP_BOOL:
                    return 1;
                case TYP_BYTE:
                    return SCHAR_MAX;
                case TYP_SHORT:
                    return SHRT_MAX;
                case TYP_INT:
                    return INT_MAX;
                case TYP_UBYTE:
                    return UCHAR_MAX;
                case TYP_USHORT:
                    return USHRT_MAX;
                case TYP_UINT:
                    return UINT_MAX;
                default:
                    unreached();
            }
        }

        bool HasSameOp1(AssertionDsc* that, bool vnBased)
        {
            if (op1.kind != that->op1.kind)
            {
                return false;
            }
            else if (op1.kind == O1K_ARR_BND)
            {
                assert(vnBased);
                return (op1.bnd.vnIdx == that->op1.bnd.vnIdx) && (op1.bnd.vnLen == that->op1.bnd.vnLen);
            }
            else
            {
                return ((vnBased && (op1.vn == that->op1.vn)) ||
                        (!vnBased && (op1.lcl.lclNum == that->op1.lcl.lclNum)));
            }
        }

        bool HasSameOp2(AssertionDsc* that, bool vnBased)
        {
            if (op2.kind != that->op2.kind)
            {
                return false;
            }
            switch (op2.kind)
            {
                case O2K_IND_CNS_INT:
                case O2K_CONST_INT:
                    return ((op2.u1.iconVal == that->op2.u1.iconVal) && (op2.u1.iconFlags == that->op2.u1.iconFlags));

                case O2K_CONST_LONG:
                    return (op2.lconVal == that->op2.lconVal);

                case O2K_CONST_DOUBLE:
                    // exact match because of positive and negative zero.
                    return (memcmp(&op2.dconVal, &that->op2.dconVal, sizeof(double)) == 0);

                case O2K_LCLVAR_COPY:
                case O2K_ARR_LEN:
                    return (op2.lcl.lclNum == that->op2.lcl.lclNum) &&
                           (!vnBased || op2.lcl.ssaNum == that->op2.lcl.ssaNum);

                case O2K_SUBRANGE:
                    return ((op2.u2.loBound == that->op2.u2.loBound) && (op2.u2.hiBound == that->op2.u2.hiBound));

                case O2K_INVALID:
                    // we will return false
                    break;

                default:
                    assert(!"Unexpected value for op2.kind in AssertionDsc.");
                    break;
            }
            return false;
        }

        bool Complementary(AssertionDsc* that, bool vnBased)
        {
            return ComplementaryKind(assertionKind, that->assertionKind) && HasSameOp1(that, vnBased) &&
                   HasSameOp2(that, vnBased);
        }

        bool Equals(AssertionDsc* that, bool vnBased)
        {
            if (assertionKind != that->assertionKind)
            {
                return false;
            }
            else if (assertionKind == OAK_NO_THROW)
            {
                assert(op2.kind == O2K_INVALID);
                return HasSameOp1(that, vnBased);
            }
            else
            {
                return HasSameOp1(that, vnBased) && HasSameOp2(that, vnBased);
            }
        }
    };

protected:
    static fgWalkPreFn optAddCopiesCallback;
    static fgWalkPreFn optVNAssertionPropStmtVisitor;
    unsigned           optAddCopyLclNum;
    GenTree*           optAddCopyAsgnNode;

    bool optLocalAssertionProp; // indicates that we are performing local assertion prop
    bool optVNAssertionPropStmtMorphPending;
#ifdef DEBUG
    GenTree* optAssertionPropCurrentTree;
#endif
    AssertionIndex*            optComplementaryAssertionMap;
    JitExpandArray<ASSERT_TP>* optAssertionDep; // table that holds dependent assertions (assertions
                                                // using the value of a local var) for each local var
    AssertionDsc*  optAssertionTabPrivate;      // table that holds info about value assignments
    AssertionIndex optAssertionCount;           // total number of assertions in the assertion table
    AssertionIndex optMaxAssertionCount;

public:
    GenTree* optVNConstantPropJTrue(BasicBlock* block, GenTreeUnOp* jtrue);
    GenTree* optVNConstantPropExtractSideEffects(GenTree* tree);

    AssertionIndex GetAssertionCount()
    {
        return optAssertionCount;
    }
    ASSERT_TP* bbJtrueAssertionOut;
    typedef JitHashTable<ValueNum, JitSmallPrimitiveKeyFuncs<ValueNum>, ASSERT_TP> ValueNumToAssertsMap;
    ValueNumToAssertsMap* optValueNumToAsserts;

    // Assertion prop helpers.
    ASSERT_TP& GetAssertionDep(unsigned lclNum);
    AssertionDsc* optGetAssertion(AssertionIndex assertIndex);
    void optAssertionInit(bool isLocalProp);
    void optAssertionTraitsInit(AssertionIndex assertionCount);
#if LOCAL_ASSERTION_PROP
    void optAssertionReset(AssertionIndex limit);
    void optAssertionRemove(AssertionIndex index);
#endif

    // Assertion prop data flow functions.
    void optVNAssertionProp();
    bool optIsTreeKnownIntValue(bool vnBased, GenTree* tree, ssize_t* pConstant, GenTreeFlags* pIconFlags);
    ASSERT_TP* optInitAssertionDataflowFlags();
    ASSERT_TP* optComputeAssertionGen();

    // Assertion Gen functions.
    void optAssertionGen(GenTree* tree);
    AssertionIndex optAssertionGenPhiDefn(GenTree* tree);
    AssertionInfo optCreateJTrueBoundsAssertion(GenTree* tree);
    AssertionInfo optAssertionGenJtrue(GenTree* tree);
    AssertionIndex optCreateJtrueAssertions(GenTree*                   op1,
                                            GenTree*                   op2,
                                            Compiler::optAssertionKind assertionKind,
                                            bool                       helperCallArgs = false);
    AssertionIndex optFindComplementary(AssertionIndex assertionIndex);
    void optMapComplementary(AssertionIndex assertionIndex, AssertionIndex index);

    // Assertion creation functions.
    AssertionIndex optCreateAssertion(GenTree*         op1,
                                      GenTree*         op2,
                                      optAssertionKind assertionKind,
                                      bool             helperCallArgs = false);
    void optCreateComplementaryAssertion(AssertionIndex assertionIndex,
                                         GenTree*       op1,
                                         GenTree*       op2,
                                         bool           helperCallArgs = false);

    bool optAssertionVnInvolvesNan(AssertionDsc* assertion);
    AssertionIndex optAddAssertion(AssertionDsc* assertion);
    void optAddVnAssertionMapping(ValueNum vn, AssertionIndex index);
#ifdef DEBUG
    void optPrintVnAssertionMapping();
#endif
    ASSERT_TP optGetVnMappedAssertions(ValueNum vn);

    // Used for respective assertion propagations.
    AssertionIndex optAssertionIsSubrange(GenTree*         tree,
                                          var_types        fromType,
                                          var_types        toType,
                                          ASSERT_VALARG_TP assertions);
    AssertionIndex optAssertionIsSubtype(GenTree* tree, GenTree* methodTableArg, ASSERT_VALARG_TP assertions);
    AssertionIndex optAssertionIsNonNullInternal(GenTree* op, ASSERT_VALARG_TP assertions DEBUGARG(bool* pVnBased));
    bool optAssertionIsNonNull(GenTree*         op,
                               ASSERT_VALARG_TP assertions DEBUGARG(bool* pVnBased) DEBUGARG(AssertionIndex* pIndex));

    // Used for Relop propagation.
    AssertionIndex optGlobalAssertionIsEqualOrNotEqual(ASSERT_VALARG_TP assertions, GenTree* op1, GenTree* op2);
    AssertionIndex optGlobalAssertionIsEqualOrNotEqualZero(ASSERT_VALARG_TP assertions, GenTree* op1);
    AssertionIndex optLocalAssertionIsEqualOrNotEqual(
        optOp1Kind op1Kind, unsigned lclNum, optOp2Kind op2Kind, ssize_t cnsVal, ASSERT_VALARG_TP assertions);

    // Assertion prop for lcl var functions.
    bool optAssertionProp_LclVarTypeCheck(GenTree* tree, LclVarDsc* lclVarDsc, LclVarDsc* copyVarDsc);
    GenTree* optCopyAssertionProp(AssertionDsc*        curAssertion,
                                  GenTreeLclVarCommon* tree,
                                  Statement* stmt DEBUGARG(AssertionIndex index));
    GenTree* optConstantAssertionProp(AssertionDsc*        curAssertion,
                                      GenTreeLclVarCommon* tree,
                                      Statement* stmt DEBUGARG(AssertionIndex index));

    // Assertion propagation functions.
    GenTree* optAssertionProp(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt, BasicBlock* block);
    GenTree* optAssertionProp_LclVar(ASSERT_VALARG_TP assertions, GenTreeLclVarCommon* tree, Statement* stmt);
    GenTree* optAssertionProp_Ind(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt);
    GenTree* optAssertionProp_Cast(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt);
    GenTree* optAssertionProp_Call(ASSERT_VALARG_TP assertions, GenTreeCall* call, Statement* stmt);
    GenTree* optAssertionProp_RelOp(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt);
    GenTree* optAssertionProp_Comma(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt);
    GenTree* optAssertionProp_BndsChk(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt);
    GenTree* optAssertionPropGlobal_RelOp(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt);
    GenTree* optAssertionPropLocal_RelOp(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt);
    GenTree* optAssertionProp_Update(GenTree* newTree, GenTree* tree, Statement* stmt);
    GenTree* optNonNullAssertionProp_Call(ASSERT_VALARG_TP assertions, GenTreeCall* call);

    // Implied assertion functions.
    void optImpliedAssertions(AssertionIndex assertionIndex, ASSERT_TP& activeAssertions);
    void optImpliedByTypeOfAssertions(ASSERT_TP& activeAssertions);
    void optImpliedByCopyAssertion(AssertionDsc* copyAssertion, AssertionDsc* depAssertion, ASSERT_TP& result);
    void optImpliedByConstAssertion(AssertionDsc* curAssertion, ASSERT_TP& result);

#ifdef DEBUG
    void optPrintAssertion(AssertionDsc* newAssertion, AssertionIndex assertionIndex = 0);
    void optDebugCheckAssertion(AssertionDsc* assertion);
    void optDebugCheckAssertions(AssertionIndex AssertionIndex);
#endif
    void optAddCopies();
#endif // ASSERTION_PROP

    /**************************************************************************
     *                          Range checks
     *************************************************************************/

public:
    struct LoopCloneVisitorInfo
    {
        LoopCloneContext* context;
        unsigned          loopNum;
        Statement*        stmt;
        LoopCloneVisitorInfo(LoopCloneContext* context, unsigned loopNum, Statement* stmt)
            : context(context), loopNum(loopNum), stmt(nullptr)
        {
        }
    };

    bool optIsStackLocalInvariant(unsigned loopNum, unsigned lclNum);
    bool optExtractArrIndex(GenTree* tree, ArrIndex* result, unsigned lhsNum);
    bool optReconstructArrIndex(GenTree* tree, ArrIndex* result, unsigned lhsNum);
    bool optIdentifyLoopOptInfo(unsigned loopNum, LoopCloneContext* context);
    static fgWalkPreFn optCanOptimizeByLoopCloningVisitor;
    fgWalkResult optCanOptimizeByLoopCloning(GenTree* tree, LoopCloneVisitorInfo* info);
    bool optObtainLoopCloningOpts(LoopCloneContext* context);
    bool optIsLoopClonable(unsigned loopInd);

    bool optLoopCloningEnabled();

#ifdef DEBUG
    void optDebugLogLoopCloning(BasicBlock* block, Statement* insertBefore);
#endif
    void optPerformStaticOptimizations(unsigned loopNum, LoopCloneContext* context DEBUGARG(bool fastPath));
    bool optComputeDerefConditions(unsigned loopNum, LoopCloneContext* context);
    bool optDeriveLoopCloningConditions(unsigned loopNum, LoopCloneContext* context);
    BasicBlock* optInsertLoopChoiceConditions(LoopCloneContext* context,
                                              unsigned          loopNum,
                                              BasicBlock*       head,
                                              BasicBlock*       slow);

protected:
    ssize_t optGetArrayRefScaleAndIndex(GenTree* mul, GenTree** pIndex DEBUGARG(bool bRngChk));

    bool optReachWithoutCall(BasicBlock* srcBB, BasicBlock* dstBB);

protected:
    bool optLoopsMarked;

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
    regNumber raUpdateRegStateForArg(RegState* regState, LclVarDsc* argDsc);

    void raMarkStkVars();

#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
#if defined(TARGET_AMD64)
    static bool varTypeNeedsPartialCalleeSave(var_types type)
    {
        return (type == TYP_SIMD32);
    }
#elif defined(TARGET_ARM64)
    static bool varTypeNeedsPartialCalleeSave(var_types type)
    {
        // ARM64 ABI FP Callee save registers only require Callee to save lower 8 Bytes
        // For SIMD types longer than 8 bytes Caller is responsible for saving and restoring Upper bytes.
        return ((type == TYP_SIMD16) || (type == TYP_SIMD12));
    }
#else // !defined(TARGET_AMD64) && !defined(TARGET_ARM64)
#error("Unknown target architecture for FEATURE_SIMD")
#endif // !defined(TARGET_AMD64) && !defined(TARGET_ARM64)
#endif // FEATURE_PARTIAL_SIMD_CALLEE_SAVE

protected:
    // Some things are used by both LSRA and regpredict allocators.

    FrameType rpFrameType;
    bool      rpMustCreateEBPCalled; // Set to true after we have called rpMustCreateEBPFrame once

    bool rpMustCreateEBPFrame(INDEBUG(const char** wbReason));

private:
    Lowering*            m_pLowering;   // Lowering; needed to Lower IR that's added or modified after Lowering.
    LinearScanInterface* m_pLinearScan; // Linear Scan allocator

    /* raIsVarargsStackArg is called by raMaskStkVars and by
       lvaComputeRefCounts.  It identifies the special case
       where a varargs function has a parameter passed on the
       stack, other than the special varargs handle.  Such parameters
       require special treatment, because they cannot be tracked
       by the GC (their offsets in the stack are not known
       at compile time).
    */

    bool raIsVarargsStackArg(unsigned lclNum)
    {
#ifdef TARGET_X86

        LclVarDsc* varDsc = &lvaTable[lclNum];

        assert(varDsc->lvIsParam);

        return (info.compIsVarArgs && !varDsc->lvIsRegArg && (lclNum != lvaVarargsHandleArg));

#else // TARGET_X86

        return false;

#endif // TARGET_X86
    }

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

    // Get the flags

    bool eeIsValueClass(CORINFO_CLASS_HANDLE clsHnd);

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
    unsigned eeGetArgSize(CORINFO_ARG_LIST_HANDLE list, CORINFO_SIG_INFO* sig);
    static unsigned eeGetArgAlignment(var_types type, bool isFloatHfa);

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

    CORINFO_METHOD_HANDLE eeMarkNativeTarget(CORINFO_METHOD_HANDLE method);

    CORINFO_EE_INFO eeInfo;
    bool            eeInfoInitialized;

    CORINFO_EE_INFO* eeGetEEInfo();

    // Gets the offset of a SDArray's first element
    unsigned eeGetArrayDataOffset(var_types type);
    // Gets the offset of a MDArray's first element
    unsigned eeGetMDArrayDataOffset(var_types type, unsigned rank);

    GenTree* eeGetPInvokeCookie(CORINFO_SIG_INFO* szMetaSig);

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
    public:
        VirtualStubParamInfo(bool isCoreRTABI)
        {
#if defined(TARGET_X86)
            reg     = REG_EAX;
            regMask = RBM_EAX;
#elif defined(TARGET_AMD64)
            if (isCoreRTABI)
            {
                reg     = REG_R10;
                regMask = RBM_R10;
            }
            else
            {
                reg     = REG_R11;
                regMask = RBM_R11;
            }
#elif defined(TARGET_ARM)
            if (isCoreRTABI)
            {
                reg     = REG_R12;
                regMask = RBM_R12;
            }
            else
            {
                reg     = REG_R4;
                regMask = RBM_R4;
            }
#elif defined(TARGET_ARM64)
            reg     = REG_R11;
            regMask = RBM_R11;
#else
#error Unsupported or unset target architecture
#endif
        }

        regNumber GetReg() const
        {
            return reg;
        }

        _regMask_enum GetRegMask() const
        {
            return regMask;
        }

    private:
        regNumber     reg;
        _regMask_enum regMask;
    };

    VirtualStubParamInfo* virtualStubParamInfo;

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

    // Debugging support - Line number info

    void eeGetStmtOffsets();

    unsigned eeBoundariesCount;

    struct boundariesDsc
    {
        UNATIVE_OFFSET nativeIP;
        IL_OFFSET      ilOffset;
        unsigned       sourceReason;
    } * eeBoundaries; // Boundaries to report to EE
    void eeSetLIcount(unsigned count);
    void eeSetLIinfo(unsigned which, UNATIVE_OFFSET offs, unsigned srcIP, bool stkEmpty, bool callInstruction);
    void eeSetLIdone();

#ifdef DEBUG
    static void eeDispILOffs(IL_OFFSET offs);
    static void eeDispLineInfo(const boundariesDsc* line);
    void eeDispLineInfos();
#endif // DEBUG

    // Debugging support - Local var info

    void eeGetVars();

    unsigned eeVarsCount;

    struct VarResultInfo
    {
        UNATIVE_OFFSET             startOffset;
        UNATIVE_OFFSET             endOffset;
        DWORD                      varNumber;
        CodeGenInterface::siVarLoc loc;
    } * eeVars;
    void eeSetLVcount(unsigned count);
    void eeSetLVinfo(unsigned                          which,
                     UNATIVE_OFFSET                    startOffs,
                     UNATIVE_OFFSET                    length,
                     unsigned                          varNum,
                     const CodeGenInterface::siVarLoc& loc);
    void eeSetLVdone();

#ifdef DEBUG
    void eeDispVar(ICorDebugInfo::NativeVarInfo* var);
    void eeDispVars(CORINFO_METHOD_HANDLE ftn, ULONG32 cVars, ICorDebugInfo::NativeVarInfo* vars);
#endif // DEBUG

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

    WORD eeGetRelocTypeHint(void* target);

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

    // Utility functions

    const char* eeGetFieldName(CORINFO_FIELD_HANDLE fieldHnd, const char** classNamePtr = nullptr);

#if defined(DEBUG)
    const WCHAR* eeGetCPString(size_t stringHandle);
#endif

    const char* eeGetClassName(CORINFO_CLASS_HANDLE clsHnd);

    static CORINFO_METHOD_HANDLE eeFindHelper(unsigned helper);
    static CorInfoHelpFunc eeGetHelperNum(CORINFO_METHOD_HANDLE method);

    static fgWalkPreFn CountSharedStaticHelper;
    static bool IsSharedStaticHelper(GenTree* tree);
    static bool IsTreeAlwaysHoistable(GenTree* tree);
    static bool IsGcSafePoint(GenTree* tree);

    static CORINFO_FIELD_HANDLE eeFindJitDataOffs(unsigned jitDataOffs);
    // returns true/false if 'field' is a Jit Data offset
    static bool eeIsJitDataOffs(CORINFO_FIELD_HANDLE field);
    // returns a number < 0 if 'field' is not a Jit Data offset, otherwise the data offset (limited to 2GB)
    static int eeGetJitDataOffs(CORINFO_FIELD_HANDLE field);

    /*****************************************************************************/

    /*
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XX                                                                           XX
    XX                           CodeGenerator                                   XX
    XX                                                                           XX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    */

public:
    CodeGenInterface* codeGen;

    //  The following holds information about instr offsets in terms of generated code.

    struct IPmappingDsc
    {
        IPmappingDsc* ipmdNext;      // next line# record
        emitLocation  ipmdNativeLoc; // the emitter location of the native code corresponding to the IL offset
        IL_OFFSETX    ipmdILoffsx;   // the instr offset
        bool          ipmdIsLabel;   // Can this code be a branch label?
    };

    // Record the instr offset mapping to the generated code

    IPmappingDsc* genIPmappingList;
    IPmappingDsc* genIPmappingLast;

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
    CallSiteILOffsetTable* genCallSite2ILOffsetMap;

    unsigned    genReturnLocal; // Local number for the return value when applicable.
    BasicBlock* genReturnBB;    // jumped to when not optimizing for speed.

    // The following properties are part of CodeGenContext.  Getters are provided here for
    // convenience and backward compatibility, but the properties can only be set by invoking
    // the setter on CodeGenContext directly.

    emitter* GetEmitter() const
    {
        return codeGen->GetEmitter();
    }

    bool isFramePointerUsed() const
    {
        return codeGen->isFramePointerUsed();
    }

    bool GetInterruptible()
    {
        return codeGen->GetInterruptible();
    }
    void SetInterruptible(bool value)
    {
        codeGen->SetInterruptible(value);
    }

#ifdef TARGET_ARMARCH

    bool GetHasTailCalls()
    {
        return codeGen->GetHasTailCalls();
    }
    void SetHasTailCalls(bool value)
    {
        codeGen->SetHasTailCalls(value);
    }
#endif // TARGET_ARMARCH

#if DOUBLE_ALIGN
    const bool genDoubleAlign()
    {
        return codeGen->doDoubleAlign();
    }
    DWORD getCanDoubleAlign();
    bool shouldDoubleAlign(unsigned             refCntStk,
                           unsigned             refCntReg,
                           BasicBlock::weight_t refCntWtdReg,
                           unsigned             refCntStkParam,
                           BasicBlock::weight_t refCntWtdStkDbl);
#endif // DOUBLE_ALIGN

    bool IsFullPtrRegMapRequired()
    {
        return codeGen->IsFullPtrRegMapRequired();
    }
    void SetFullPtrRegMapRequired(bool value)
    {
        codeGen->SetFullPtrRegMapRequired(value);
    }

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

    VARSET_TP compCurLife;     // current live variables
    GenTree*  compCurLifeTree; // node after which compCurLife has been computed

    // Compare the given "newLife" with last set of live variables and update
    // codeGen "gcInfo", siScopes, "regSet" with the new variable's homes/liveness.
    template <bool ForCodeGen>
    void compChangeLife(VARSET_VALARG_TP newLife);

    // Update the GC's masks, register's masks and reports change on variable's homes given a set of
    // current live variables if changes have happened since "compCurLife".
    template <bool ForCodeGen>
    inline void compUpdateLife(VARSET_VALARG_TP newLife);

    // Gets a register mask that represent the kill set for a helper call since
    // not all JIT Helper calls follow the standard ABI on the target architecture.
    regMaskTP compHelperCallKillSet(CorInfoHelpFunc helper);

    // If "tree" is a indirection (GT_IND, or GT_OBJ) whose arg is an ADDR, whose arg is a LCL_VAR, return that LCL_VAR
    // node, else NULL.
    static GenTreeLclVar* fgIsIndirOfAddrOfLocal(GenTree* tree);

    // This map is indexed by GT_OBJ nodes that are address of promoted struct variables, which
    // have been annotated with the GTF_VAR_DEATH flag.  If such a node is *not* mapped in this
    // table, one may assume that all the (tracked) field vars die at this GT_OBJ.  Otherwise,
    // the node maps to a pointer to a VARSET_TP, containing set bits for each of the tracked field
    // vars of the promoted struct local that go dead at the given node (the set bits are the bits
    // for the tracked var indices of the field vars, as in a live var set).
    //
    // The map is allocated on demand so all map operations should use one of the following three
    // wrapper methods.

    NodeToVarsetPtrMap* m_promotedStructDeathVars;

    NodeToVarsetPtrMap* GetPromotedStructDeathVars()
    {
        if (m_promotedStructDeathVars == nullptr)
        {
            m_promotedStructDeathVars = new (getAllocator()) NodeToVarsetPtrMap(getAllocator());
        }
        return m_promotedStructDeathVars;
    }

    void ClearPromotedStructDeathVars()
    {
        if (m_promotedStructDeathVars != nullptr)
        {
            m_promotedStructDeathVars->RemoveAll();
        }
    }

    bool LookupPromotedStructDeathVars(GenTree* tree, VARSET_TP** bits)
    {
        *bits       = nullptr;
        bool result = false;

        if (m_promotedStructDeathVars != nullptr)
        {
            result = m_promotedStructDeathVars->Lookup(tree, bits);
        }

        return result;
    }

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

    GenTree* impSIMDPopStack(var_types type);
    GenTree* impPopStackAddrAsVector(var_types type);

    void lvaRecordSimdIntrinsicUse(GenTree* op);
    void lvaRecordSimdIntrinsicUse(GenTreeLclVar* lclVar);
    void lvaRecordSimdIntrinsicUse(unsigned lclNum);
    void lvaRecordSimdIntrinsicDef(GenTreeLclVar* lclVar, GenTreeHWIntrinsic* src);

    // Get the type for the hardware SIMD vector.
    // This is the maximum SIMD type supported for this target.
    var_types GetVectorTSimdType();

    // Get preferred alignment of SIMD type.
    int lvaGetSimdTypedLocalPreferredAlignment(LclVarDsc* lcl);
#endif // FEATURE_SIMD

private:
    bool lvaIsSimdTypedLocalAligned(unsigned varNum);

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
    Compiler* InlineeCompiler; // The Compiler instance for the inlinee

    InlineResult* compInlineResult; // The result of importing the inlinee method.

    bool compDoAggressiveInlining; // If true, mark every method as CORINFO_FLG_FORCEINLINE
    bool compJmpOpUsed;            // Does the method do a JMP
    bool compLongUsed;             // Does the method use TYP_LONG
    bool compFloatingPointUsed;    // Does the method use TYP_FLOAT or TYP_DOUBLE
    bool compTailCallUsed;         // Does the method do a tailcall
    bool compLocallocUsed;         // Does the method use localloc.
    bool compLocallocOptimized;    // Does the method have an optimized localloc
    bool compQmarkUsed;            // Does the method use GT_QMARK/GT_COLON
    bool compQmarkRationalized;    // Is it allowed to use a GT_QMARK/GT_COLON node.
    bool compHasBackwardJump;      // Does the method (or some inlinee) have a lexically backwards jump?
    bool compSwitchedToOptimized;  // Codegen initially was Tier0 but jit switched to FullOpts
    bool compSwitchedToMinOpts;    // Codegen initially was Tier1/FullOpts but jit switched to MinOpts
    bool compSuppressedZeroInit;   // There are vars with lvSuppressedZeroInit set

// NOTE: These values are only reliable after
//       the importing is completely finished.

#ifdef DEBUG
    // State information - which phases have completed?
    // These are kept together for easy discoverability

    bool    bRangeAllowStress;
    bool    compCodeGenDone;
    int64_t compNumStatementLinksTraversed; // # of links traversed while doing debug checks
    bool    fgNormalizeEHDone;              // Has the flowgraph EH normalization phase been done?
    size_t  compSizeEstimate;               // The estimated size of the method as per `gtSetEvalOrder`.
    size_t  compCycleEstimate;              // The estimated cycle count of the method as per `gtSetEvalOrder`
#endif                                      // DEBUG

    bool fgLocalVarLivenessDone; // Note that this one is used outside of debug.
    bool fgLocalVarLivenessChanged;
    bool compLSRADone;
    bool compRationalIRForm;

    bool compUsesThrowHelper; // There is a call to a THROW_HELPER for the compiled method.

    bool compGeneratingProlog;
    bool compGeneratingEpilog;
    bool compNeedsGSSecurityCookie; // There is an unsafe buffer (or localloc) on the stack.
                                    // Insert cookie on frame and code to check the cookie, like VC++ -GS.
    bool compGSReorderStackLayout;  // There is an unsafe buffer on the stack, reorder locals and make local
    // copies of susceptible parameters to avoid buffer overrun attacks through locals/params
    bool getNeedsGSSecurityCookie() const
    {
        return compNeedsGSSecurityCookie;
    }
    void setNeedsGSSecurityCookie()
    {
        compNeedsGSSecurityCookie = true;
    }

    FrameLayoutState lvaDoneFrameLayout; // The highest frame layout state that we've completed. During
                                         // frame layout calculations, this is the level we are currently
                                         // computing.

    //---------------------------- JITing options -----------------------------

    enum codeOptimize
    {
        BLENDED_CODE,
        SMALL_CODE,
        FAST_CODE,

        COUNT_OPT_CODE
    };

    struct Options
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

        unsigned compFlags; // method attributes
        unsigned instrCount;
        unsigned lvRefCount;

        codeOptimize compCodeOpt; // what type of code optimizations

        bool compUseCMOV;

// optimize maximally and/or favor speed over size?

#define DEFAULT_MIN_OPTS_CODE_SIZE 60000
#define DEFAULT_MIN_OPTS_INSTR_COUNT 20000
#define DEFAULT_MIN_OPTS_BB_COUNT 2000
#define DEFAULT_MIN_OPTS_LV_NUM_COUNT 2000
#define DEFAULT_MIN_OPTS_LV_REF_COUNT 8000

// Maximun number of locals before turning off the inlining
#define MAX_LV_NUM_COUNT_FOR_INLINING 512

        bool compMinOpts;
        bool compMinOptsIsSet;
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
        bool MinOpts() const
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

        // true if the CLFLG_* for an optimization is set.
        bool OptEnabled(unsigned optFlag)
        {
            return !!(compFlags & optFlag);
        }

#ifdef FEATURE_READYTORUN_COMPILER
        bool IsReadyToRun()
        {
            return jitFlags->IsSet(JitFlags::JIT_FLAG_READYTORUN);
        }
#else
        bool IsReadyToRun()
        {
            return false;
        }
#endif

#ifdef FEATURE_ON_STACK_REPLACEMENT
        bool IsOSR() const
        {
            return jitFlags->IsSet(JitFlags::JIT_FLAG_OSR);
        }
#else
        bool IsOSR() const
        {
            return false;
        }
#endif

        // true if we should use the PINVOKE_{BEGIN,END} helpers instead of generating
        // PInvoke transitions inline.
        bool ShouldUsePInvokeHelpers()
        {
            return jitFlags->IsSet(JitFlags::JIT_FLAG_USE_PINVOKE_HELPERS);
        }

        // true if we should use insert the REVERSE_PINVOKE_{ENTER,EXIT} helpers in the method
        // prolog/epilog
        bool IsReversePInvoke()
        {
            return jitFlags->IsSet(JitFlags::JIT_FLAG_REVERSE_PINVOKE);
        }

        bool compScopeInfo; // Generate the LocalVar info ?
        bool compDbgCode;   // Generate debugger-friendly code?
        bool compDbgInfo;   // Gather debugging info?
        bool compDbgEnC;

#ifdef PROFILING_SUPPORTED
        bool compNoPInvokeInlineCB;
#else
        static const bool compNoPInvokeInlineCB;
#endif

#ifdef DEBUG
        bool compGcChecks; // Check arguments and return values to ensure they are sane
#endif

#if defined(DEBUG) && defined(TARGET_XARCH)

        bool compStackCheckOnRet; // Check stack pointer on return to ensure it is correct.

#endif // defined(DEBUG) && defined(TARGET_XARCH)

#if defined(DEBUG) && defined(TARGET_X86)

        bool compStackCheckOnCall; // Check stack pointer after call to ensure it is correct. Only for x86.

#endif // defined(DEBUG) && defined(TARGET_X86)

        bool compReloc; // Generate relocs for pointers in code, true for all ngen/prejit codegen

#ifdef DEBUG
#if defined(TARGET_XARCH)
        bool compEnablePCRelAddr; // Whether absolute addr be encoded as PC-rel offset by RyuJIT where possible
#endif
#endif // DEBUG

#ifdef UNIX_AMD64_ABI
        // This flag  is indicating if there is a need to align the frame.
        // On AMD64-Windows, if there are calls, 4 slots for the outgoing ars are allocated, except for
        // FastTailCall. This slots makes the frame size non-zero, so alignment logic will be called.
        // On AMD64-Unix, there are no such slots. There is a possibility to have calls in the method with frame size of
        // 0. The frame alignment logic won't kick in. This flags takes care of the AMD64-Unix case by remembering that
        // there are calls and making sure the frame alignment logic is executed.
        bool compNeedToAlignFrame;
#endif // UNIX_AMD64_ABI

        bool compProcedureSplitting; // Separate cold code from hot code

        bool genFPorder; // Preserve FP order (operations are non-commutative)
        bool genFPopt;   // Can we do frame-pointer-omission optimization?
        bool altJit;     // True if we are an altjit and are compiling this method

#ifdef OPT_CONFIG
        bool optRepeat; // Repeat optimizer phases k times
#endif

#ifdef DEBUG
        bool compProcedureSplittingEH; // Separate cold code from hot code for functions with EH
        bool dspCode;                  // Display native code generated
        bool dspEHTable;               // Display the EH table reported to the VM
        bool dspDebugInfo;             // Display the Debug info reported to the VM
        bool dspInstrs;                // Display the IL instructions intermixed with the native code output
        bool dspLines;                 // Display source-code lines intermixed with native code output
        bool dmpHex;                   // Display raw bytes in hex of native code output
        bool varNames;                 // Display variables names in native code output
        bool disAsm;                   // Display native code as it is generated
        bool disAsmSpilled;            // Display native code when any register spilling occurs
        bool disasmWithGC;             // Display GC info interleaved with disassembly.
        bool disDiffable;              // Makes the Disassembly code 'diff-able'
        bool disAddr;                  // Display process address next to each instruction in disassembly code
        bool disAsm2;                  // Display native code after it is generated using external disassembler
        bool dspOrder;                 // Display names of each of the methods that we ngen/jit
        bool dspUnwind;                // Display the unwind info output
        bool dspDiffable;     // Makes the Jit Dump 'diff-able' (currently uses same COMPlus_* flag as disDiffable)
        bool compLongAddress; // Force using large pseudo instructions for long address
                              // (IF_LARGEJMP/IF_LARGEADR/IF_LARGLDC)
        bool dspGCtbls;       // Display the GC tables
#endif

        bool compExpandCallsEarly; // True if we should expand virtual call targets early for this method

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
        bool compJitAlignLoopForJcc;
#endif
        // For non-adaptive alignment, minimum loop size (in bytes) for which alignment will be done.
        unsigned short compJitAlignLoopMaxCodeSize;

        // Minimum weight needed for the first block of a loop to make it a candidate for alignment.
        unsigned short compJitAlignLoopMinBlockWeight;

        // For non-adaptive alignment, address boundary (power of 2) at which loop alignment should
        // be done. By default, 32B.
        unsigned short compJitAlignLoopBoundary;

        // Padding limit to align a loop.
        unsigned short compJitAlignPaddingLimit;

        // If set, perform adaptive loop alignment that limits number of padding based on loop size.
        bool compJitAlignLoopAdaptive;

#ifdef LATE_DISASM
        bool doLateDisasm; // Run the late disassembler
#endif                     // LATE_DISASM

#if DUMP_GC_TABLES && !defined(DEBUG)
#pragma message("NOTE: this non-debug build has GC ptr table dumping always enabled!")
        static const bool dspGCtbls = true;
#endif

#ifdef PROFILING_SUPPORTED
        // Whether to emit Enter/Leave/TailCall hooks using a dummy stub (DummyProfilerELTStub()).
        // This option helps make the JIT behave as if it is running under a profiler.
        bool compJitELTHookEnabled;
#endif // PROFILING_SUPPORTED

#if FEATURE_TAILCALL_OPT
        // Whether opportunistic or implicit tail call optimization is enabled.
        bool compTailCallOpt;
        // Whether optimization of transforming a recursive tail call into a loop is enabled.
        bool compTailCallLoopOpt;
#endif

#if FEATURE_FASTTAILCALL
        // Whether fast tail calls are allowed.
        bool compFastTailCalls;
#endif // FEATURE_FASTTAILCALL

#if defined(TARGET_ARM64)
        // Decision about whether to save FP/LR registers with callee-saved registers (see
        // COMPlus_JitSaveFpLrWithCalleSavedRegisters).
        int compJitSaveFpLrWithCalleeSavedRegisters;
#endif // defined(TARGET_ARM64)

#ifdef CONFIGURABLE_ARM_ABI
        bool compUseSoftFP = false;
#else
#ifdef ARM_SOFTFP
        static const bool compUseSoftFP = true;
#else  // !ARM_SOFTFP
        static const bool compUseSoftFP = false;
#endif // ARM_SOFTFP
#endif // CONFIGURABLE_ARM_ABI
    } opts;

    static bool                s_pAltJitExcludeAssembliesListInitialized;
    static AssemblyNamesList2* s_pAltJitExcludeAssembliesList;

#ifdef DEBUG
    static bool                s_pJitDisasmIncludeAssembliesListInitialized;
    static AssemblyNamesList2* s_pJitDisasmIncludeAssembliesList;

    static bool       s_pJitFunctionFileInitialized;
    static MethodSet* s_pJitMethodSet;
#endif // DEBUG

#ifdef DEBUG
// silence warning of cast to greater size. It is easier to silence than construct code the compiler is happy with, and
// it is safe in this case
#pragma warning(push)
#pragma warning(disable : 4312)

    template <typename T>
    T dspPtr(T p)
    {
        return (p == ZERO) ? ZERO : (opts.dspDiffable ? T(0xD1FFAB1E) : p);
    }

    template <typename T>
    T dspOffset(T o)
    {
        return (o == ZERO) ? ZERO : (opts.dspDiffable ? T(0xD1FFAB1E) : o);
    }
#pragma warning(pop)

    static int dspTreeID(GenTree* tree)
    {
        return tree->gtTreeID;
    }

    static void printStmtID(Statement* stmt)
    {
        assert(stmt != nullptr);
        printf(FMT_STMT, stmt->GetID());
    }

    static void printTreeID(GenTree* tree)
    {
        if (tree == nullptr)
        {
            printf("[------]");
        }
        else
        {
            printf("[%06d]", dspTreeID(tree));
        }
    }

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
        STRESS_MODE(LCL_FLDS)                                                                   \
        STRESS_MODE(UNROLL_LOOPS)                                                               \
        STRESS_MODE(MAKE_CSE)                                                                   \
        STRESS_MODE(LEGACY_INLINE)                                                              \
        STRESS_MODE(CLONE_EXPR)                                                                 \
        STRESS_MODE(USE_CMOV)                                                                   \
        STRESS_MODE(FOLD)                                                                       \
        STRESS_MODE(MERGED_RETURNS)                                                             \
        STRESS_MODE(BB_PROFILE)                                                                 \
        STRESS_MODE(OPT_BOOLS_GC)                                                               \
        STRESS_MODE(REMORPH_TREES)                                                              \
        STRESS_MODE(64RSLT_MUL)                                                                 \
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
        STRESS_MODE(CHK_REIMPORT)                                                               \
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
    BYTE                 compActiveStressModes[STRESS_COUNT];
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

    bool compTailCallStress()
    {
#ifdef DEBUG
        // Do not stress tailcalls in IL stubs as the runtime creates several IL
        // stubs to implement the tailcall mechanism, which would then
        // recursively create more IL stubs.
        return !opts.jitFlags->IsSet(JitFlags::JIT_FLAG_IL_STUB) &&
               (JitConfig.TailcallStress() != 0 || compStressCompile(STRESS_TAILCALL, 5));
#else
        return false;
#endif
    }

    const char* compGetTieringName(bool wantShortName = false) const;
    const char* compGetStressMessage() const;

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

    struct Info
    {
        COMP_HANDLE           compCompHnd;
        CORINFO_MODULE_HANDLE compScopeHnd;
        CORINFO_CLASS_HANDLE  compClassHnd;
        CORINFO_METHOD_HANDLE compMethodHnd;
        CORINFO_METHOD_INFO*  compMethodInfo;

        bool hasCircularClassConstraints;
        bool hasCircularMethodConstraints;

#if defined(DEBUG) || defined(LATE_DISASM) || DUMP_FLOWGRAPHS

        const char* compMethodName;
        const char* compClassName;
        const char* compFullName;
        double      compPerfScore;
        int         compMethodSuperPMIIndex; // useful when debugging under SuperPMI

#endif // defined(DEBUG) || defined(LATE_DISASM) || DUMP_FLOWGRAPHS

#if defined(DEBUG) || defined(INLINE_DATA)
        // Method hash is logically const, but computed
        // on first demand.
        mutable unsigned compMethodHashPrivate;
        unsigned         compMethodHash() const;
#endif // defined(DEBUG) || defined(INLINE_DATA)

#ifdef PSEUDORANDOM_NOP_INSERTION
        // things for pseudorandom nop insertion
        unsigned  compChecksum;
        CLRRandom compRNG;
#endif

        // The following holds the FLG_xxxx flags for the method we're compiling.
        unsigned compFlags;

        // The following holds the class attributes for the method we're compiling.
        unsigned compClassAttr;

        const BYTE*     compCode;
        IL_OFFSET       compILCodeSize;     // The IL code size
        IL_OFFSET       compILImportSize;   // Estimated amount of IL actually imported
        IL_OFFSET       compILEntry;        // The IL entry point (normally 0)
        PatchpointInfo* compPatchpointInfo; // Patchpoint data for OSR (normally nullptr)
        UNATIVE_OFFSET  compNativeCodeSize; // The native code size, after instructions are issued. This
        // is less than (compTotalHotCodeSize + compTotalColdCodeSize) only if:
        // (1) the code is not hot/cold split, and we issued less code than we expected, or
        // (2) the code is hot/cold split, and we issued less code than we expected
        // in the cold section (the hot section will always be padded out to compTotalHotCodeSize).

        bool compIsStatic : 1;           // Is the method static (no 'this' pointer)?
        bool compIsVarArgs : 1;          // Does the method have varargs parameters?
        bool compInitMem : 1;            // Is the CORINFO_OPT_INIT_LOCALS bit set in the method info options?
        bool compProfilerCallback : 1;   // JIT inserted a profiler Enter callback
        bool compPublishStubParam : 1;   // EAX captured in prolog will be available through an intrinsic
        bool compHasNextCallRetAddr : 1; // The NextCallReturnAddress intrinsic is used.

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

        unsigned compILargsCount; // Number of arguments (incl. implicit but not hidden)
        unsigned compArgsCount;   // Number of arguments (incl. implicit and     hidden)

        unsigned GetParamCount() const
        {
            return compArgsCount;
        }

#if FEATURE_FASTTAILCALL
        unsigned compArgStackSize; // Incoming argument stack size in bytes
#endif                             // FEATURE_FASTTAILCALL

        unsigned compRetBuffArg; // position of hidden return param var (0, 1) (BAD_VAR_NUM means not present);
        int compTypeCtxtArg; // position of hidden param for type context for generic code (CORINFO_CALLCONV_PARAMTYPE)
        unsigned       compThisArg; // position of implicit this pointer param (not to be confused with lvaArg0Var)
        unsigned       compILlocalsCount; // Number of vars : args + locals (incl. implicit but not hidden)
        unsigned       compLocalsCount;   // Number of vars : args + locals (incl. implicit and     hidden)
        unsigned       compMaxStack;
        UNATIVE_OFFSET compTotalHotCodeSize;  // Total number of bytes of Hot Code in the method
        UNATIVE_OFFSET compTotalColdCodeSize; // Total number of bytes of Cold Code in the method

        unsigned compUnmanagedCallCountWithGCTransition; // count of unmanaged calls with GC transition.

        CorInfoCallConvExtension compCallConv; // The entry-point calling convention for this method.

        unsigned compLvFrameListRoot; // lclNum for the Frame root
        unsigned compXcptnsCount;     // Number of exception-handling clauses read in the method's IL.
                                      // You should generally use compHndBBtabCount instead: it is the
                                      // current number of EH clauses (after additions like synchronized
        // methods and funclets, and removals like unreachable code deletion).

        Target::ArgOrder compArgOrder;

        bool compMatchedVM; // true if the VM is "matched": either the JIT is a cross-compiler
                            // and the VM expects that, or the JIT is a "self-host" compiler
                            // (e.g., x86 hosted targeting x86) and the VM expects that.

        /*  The following holds IL scope information about local variables.
         */

        unsigned     compVarScopesCount;
        VarScopeDsc* compVarScopes;

        /* The following holds information about instr offsets for
         * which we need to report IP-mappings
         */

        IL_OFFSET*                   compStmtOffsets; // sorted
        unsigned                     compStmtOffsetsCount;
        ICorDebugInfo::BoundaryTypes compStmtOffsetsImplicit;

#define CPU_X86 0x0100 // The generic X86 CPU
#define CPU_X86_PENTIUM_4 0x0110

#define CPU_X64 0x0200       // The generic x64 CPU
#define CPU_AMD_X64 0x0210   // AMD x64 CPU
#define CPU_INTEL_X64 0x0240 // Intel x64 CPU

#define CPU_ARM 0x0300   // The generic ARM CPU
#define CPU_ARM64 0x0400 // The generic ARM64 CPU

        unsigned genCPU; // What CPU are we running on

        // Number of class profile probes in this method
        unsigned compClassProbeCount;

    } info;

    bool compEnregStructLocals()
    {
        return (JitConfig.JitEnregStructLocals() != 0);
    }

    // Returns true if the method requires a PInvoke prolog and epilog
    bool compMethodRequiresPInvokeFrame()
    {
        return (info.compUnmanagedCallCountWithGCTransition > 0);
    }

#if defined(DEBUG)

    void compDispLocalVars();

#endif // DEBUG

private:
    class ClassLayoutTable* m_classLayoutTable;

    class ClassLayoutTable* typCreateClassLayoutTable();
    class ClassLayoutTable* typGetClassLayoutTable();

public:
    bool typIsLayoutNum(unsigned layoutNum);
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
    unsigned compGenTreeID;
    unsigned compStatementID;
    unsigned compBasicBlockID;
#endif

    BasicBlock* compCurBB;   // the current basic block in process
    Statement*  compCurStmt; // the current statement in process

    //  The following is used to create the 'method JIT info' block.
    size_t compInfoBlkSize;
    BYTE*  compInfoBlkAddr;

    EHblkDsc* compHndBBtab;           // array of EH data
    unsigned  compHndBBtabCount;      // element count of used elements in EH data array
    unsigned  compHndBBtabAllocCount; // element count of allocated elements in EH data array

#if defined(TARGET_X86)

    //-------------------------------------------------------------------------
    //  Tracking of region covered by the monitor in synchronized methods
    void* syncStartEmitCookie; // the emitter cookie for first instruction after the call to MON_ENTER
    void* syncEndEmitCookie;   // the emitter cookie for first instruction after the call to MON_EXIT

#endif // !TARGET_X86

    Phases      mostRecentlyActivePhase; // the most recently active phase
    PhaseChecks activePhaseChecks;       // the currently active phase checks

    //-------------------------------------------------------------------------
    //  The following keeps track of how many bytes of local frame space we've
    //  grabbed so far in the current function, and how many argument bytes we
    //  need to pop when we return.
    //

    unsigned compLclFrameSize; // secObject+lclBlk+locals+temps

    // Count of callee-saved regs we pushed in the prolog.
    // Does not include EBP for isFramePointerUsed() and double-aligned frames.
    // In case of Amd64 this doesn't include float regs saved on stack.
    unsigned compCalleeRegsPushed;

#if defined(TARGET_XARCH)
    // Mask of callee saved float regs on stack.
    regMaskTP compCalleeFPRegsSavedMask;
#endif
#ifdef TARGET_AMD64
// Quirk for VS debug-launch scenario to work:
// Bytes of padding between save-reg area and locals.
#define VSQUIRK_STACK_PAD (2 * REGSIZE_BYTES)
    unsigned compVSQuirkStackPaddingNeeded;
#endif

    unsigned compArgSize; // total size of arguments in bytes (including register args (lvIsRegArg))

    unsigned compMapILargNum(unsigned ILargNum);      // map accounting for hidden args
    unsigned compMapILvarNum(unsigned ILvarNum);      // map accounting for hidden args
    unsigned compMap2ILvarNum(unsigned varNum) const; // map accounting for hidden args

    //-------------------------------------------------------------------------

    static void compStartup();  // One-time initialization
    static void compShutdown(); // One-time finalization

    void compInit(ArenaAllocator*       pAlloc,
                  CORINFO_METHOD_HANDLE methodHnd,
                  COMP_HANDLE           compHnd,
                  CORINFO_METHOD_INFO*  methodInfo,
                  InlineInfo*           inlineInfo = nullptr);
    void compDone();

    static void compDisplayStaticSizes(FILE* fout);

    //------------ Some utility functions --------------

    void* compGetHelperFtn(CorInfoHelpFunc ftnNum,         /* IN  */
                           void**          ppIndirection); /* OUT */

    // Components used by the compiler may write unit test suites, and
    // have them run within this method.  They will be run only once per process, and only
    // in debug.  (Perhaps should be under the control of a COMPlus_ flag.)
    // These should fail by asserting.
    INDEBUG(void compDoComponentUnitTestsOnce();)

    int compCompile(CORINFO_MODULE_HANDLE classPtr,
                    void**                methodCodePtr,
                    uint32_t*             methodCodeSize,
                    JitFlags*             compileFlags);
    void compCompileFinish();
    int compCompileHelper(CORINFO_MODULE_HANDLE classPtr,
                          COMP_HANDLE           compHnd,
                          CORINFO_METHOD_INFO*  methodInfo,
                          void**                methodCodePtr,
                          uint32_t*             methodCodeSize,
                          JitFlags*             compileFlag);

    ArenaAllocator* compGetArenaAllocator();

    void generatePatchpointInfo();

#if MEASURE_MEM_ALLOC
    static bool s_dspMemStats; // Display per-phase memory statistics for every function
#endif                         // MEASURE_MEM_ALLOC

#if LOOP_HOIST_STATS
    unsigned m_loopsConsidered;
    bool     m_curLoopHasHoistedExpression;
    unsigned m_loopsWithHoistedExpressions;
    unsigned m_totalHoistedExpressions;

    void AddLoopHoistStats();
    void PrintPerMethodLoopHoistStats();

    static CritSecObject s_loopHoistStatsLock; // This lock protects the data structures below.
    static unsigned      s_loopsConsidered;
    static unsigned      s_loopsWithHoistedExpressions;
    static unsigned      s_totalHoistedExpressions;

    static void PrintAggregateLoopHoistStats(FILE* f);
#endif // LOOP_HOIST_STATS

    bool compIsForInlining() const;
    bool compDonotInline();

#ifdef DEBUG
    // Get the default fill char value we randomize this value when JitStress is enabled.
    static unsigned char compGetJitDefaultFill(Compiler* comp);

    const char* compLocalVarName(unsigned varNum, unsigned offs);
    VarName compVarName(regNumber reg, bool isFloatReg = false);
    const char* compRegVarName(regNumber reg, bool displayVar = false, bool isFloatReg = false);
    void compDspSrcLinesByNativeIP(UNATIVE_OFFSET curIP);
    void compDspSrcLinesByLineNum(unsigned line, bool seek = false);
#endif // DEBUG

    //-------------------------------------------------------------------------

    struct VarScopeListNode
    {
        VarScopeDsc*             data;
        VarScopeListNode*        next;
        static VarScopeListNode* Create(VarScopeDsc* value, CompAllocator alloc)
        {
            VarScopeListNode* node = new (alloc) VarScopeListNode;
            node->data             = value;
            node->next             = nullptr;
            return node;
        }
    };

    struct VarScopeMapInfo
    {
        VarScopeListNode*       head;
        VarScopeListNode*       tail;
        static VarScopeMapInfo* Create(VarScopeListNode* node, CompAllocator alloc)
        {
            VarScopeMapInfo* info = new (alloc) VarScopeMapInfo;
            info->head            = node;
            info->tail            = node;
            return info;
        }
    };

    // Max value of scope count for which we would use linear search; for larger values we would use hashtable lookup.
    static const unsigned MAX_LINEAR_FIND_LCL_SCOPELIST = 32;

    typedef JitHashTable<unsigned, JitSmallPrimitiveKeyFuncs<unsigned>, VarScopeMapInfo*> VarNumToScopeDscMap;

    // Map to keep variables' scope indexed by varNum containing it's scope dscs at the index.
    VarNumToScopeDscMap* compVarScopeMap;

    VarScopeDsc* compFindLocalVar(unsigned varNum, unsigned lifeBeg, unsigned lifeEnd);

    VarScopeDsc* compFindLocalVar(unsigned varNum, unsigned offs);

    VarScopeDsc* compFindLocalVarLinear(unsigned varNum, unsigned offs);

    void compInitVarScopeMap();

    VarScopeDsc** compEnterScopeList; // List has the offsets where variables
                                      // enter scope, sorted by instr offset
    unsigned compNextEnterScope;

    VarScopeDsc** compExitScopeList; // List has the offsets where variables
                                     // go out of scope, sorted by instr offset
    unsigned compNextExitScope;

    void compInitScopeLists();

    void compResetScopeLists();

    VarScopeDsc* compGetNextEnterScope(unsigned offs, bool scan = false);

    VarScopeDsc* compGetNextExitScope(unsigned offs, bool scan = false);

    void compProcessScopesUntil(unsigned   offset,
                                VARSET_TP* inScope,
                                void (Compiler::*enterScopeFn)(VARSET_TP* inScope, VarScopeDsc*),
                                void (Compiler::*exitScopeFn)(VARSET_TP* inScope, VarScopeDsc*));

#ifdef DEBUG
    void compDispScopeLists();
#endif // DEBUG

    bool compIsProfilerHookNeeded();

    //-------------------------------------------------------------------------
    /*               Statistical Data Gathering                               */

    void compJitStats(); // call this function and enable
                         // various ifdef's below for statistical data

#if CALL_ARG_STATS
    void        compCallArgStats();
    static void compDispCallArgStats(FILE* fout);
#endif

    //-------------------------------------------------------------------------

protected:
#ifdef DEBUG
    bool skipMethod();
#endif

    ArenaAllocator* compArenaAllocator;

public:
    void compFunctionTraceStart();
    void compFunctionTraceEnd(void* methodCodePtr, ULONG methodCodeSize, bool isNYI);

protected:
    size_t compMaxUncheckedOffsetForNullObject;

    void compInitOptions(JitFlags* compileFlags);

    void compSetProcessor();
    void compInitDebuggingInfo();
    void compSetOptimizationLevel();
#ifdef TARGET_ARMARCH
    bool compRsvdRegCheck(FrameLayoutState curState);
#endif
    void compCompile(void** methodCodePtr, uint32_t* methodCodeSize, JitFlags* compileFlags);

    // Clear annotations produced during optimizations; to be used between iterations when repeating opts.
    void ResetOptAnnotations();

    // Regenerate loop descriptors; to be used between iterations when repeating opts.
    void RecomputeLoopInfo();

#ifdef PROFILING_SUPPORTED
    // Data required for generating profiler Enter/Leave/TailCall hooks

    bool  compProfilerHookNeeded; // Whether profiler Enter/Leave/TailCall hook needs to be generated for the method
    void* compProfilerMethHnd;    // Profiler handle of the method being compiled. Passed as param to ELT callbacks
    bool  compProfilerMethHndIndirected; // Whether compProfilerHandle is pointer to the handle or is an actual handle
#endif

#ifdef TARGET_AMD64
    void compQuirkForPPP(); // Check if this method should be Quirked for the PPP issue
#endif

public:
    // Assumes called as part of process shutdown; does any compiler-specific work associated with that.
    static void ProcessShutdownWork(ICorStaticInfo* statInfo);

    CompAllocator getAllocator(CompMemKind cmk = CMK_Generic)
    {
        return CompAllocator(compArenaAllocator, cmk);
    }

    CompAllocator getAllocatorGC()
    {
        return getAllocator(CMK_GC);
    }

    CompAllocator getAllocatorLoopHoist()
    {
        return getAllocator(CMK_LoopHoist);
    }

#ifdef DEBUG
    CompAllocator getAllocatorDebugOnly()
    {
        return getAllocator(CMK_DebugOnly);
    }
#endif // DEBUG

public:
    // Returns TRUE if child is equal to or a subtype of parent.
    INDEBUG(bool tiCompatibleWith(const typeInfo& pChild, const typeInfo& pParent) const;)

    // The following is used to track liveness of local variables, initialization
    // of valueclass constructors, and type safe use of IL instructions.

    // dynamic state info needed for verification
    EntryState verCurrentState;

    void impSetCurrentState(BasicBlock* block);

#ifdef DEBUG
    typeInfo verMakeTypeInfo(CORINFO_CLASS_HANDLE clsHnd);
    typeInfo verMakeTypeInfo(CorInfoType ciType, CORINFO_CLASS_HANDLE clsHnd);

    bool verCheckTailCallConstraint(OPCODE                  opcode,
                                    CORINFO_RESOLVED_TOKEN* pResolvedToken,
                                    CORINFO_RESOLVED_TOKEN* pConstrainedResolvedToken // Is this a "constrained." call
                                                                                      // on a type parameter?
                                    );

    // One line log function. Default level is 0. Increasing it gives you
    // more log information

    // levels are currently unused: #define JITDUMP(level,...)                     ();
    void JitLogEE(unsigned level, const char* fmt, ...);

    bool compDebugBreak;

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

    GSCookie*           gsGlobalSecurityCookieAddr; // Address of global cookie for unsafe buffer checks
    GSCookie            gsGlobalSecurityCookieVal;  // Value of global cookie if addr is NULL
    ShadowParamVarInfo* gsShadowVarInfo;            // Table used by shadow param analysis code

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
    JitTimer*                  pCompJitTimer;         // Timer data structure (by phases) for current compilation.
    static CompTimeSummaryInfo s_compJitTimerSummary; // Summary of the Timer information for the whole run.

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

#ifndef FEATURE_TRACELOGGING
    // Should we actually fire the noway assert body and the exception handler?
    bool compShouldThrowOnNoway();
#else  // FEATURE_TRACELOGGING
    // Should we actually fire the noway assert body and the exception handler?
    bool compShouldThrowOnNoway(const char* filename, unsigned line);

    // Telemetry instance to use per method compilation.
    JitTelemetry compJitTelemetry;

    // Get common parameters that have to be logged with most telemetry data.
    void compGetTelemetryDefaults(const char** assemblyName,
                                  const char** scopeName,
                                  const char** methodName,
                                  unsigned*    methodHash);
#endif // !FEATURE_TRACELOGGING

    // The "FieldSeqStore", for canonicalizing field sequences.  See the definition of FieldSeqStore for
    // operations.
    FieldSeqStore* m_fieldSeqStore;

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
    NodeToFieldSeqMap* m_zeroOffsetFieldMap;

    NodeToFieldSeqMap* GetZeroOffsetFieldMap()
    {
        // Don't need to worry about inlining here
        if (m_zeroOffsetFieldMap == nullptr)
        {
            // Create a CompAllocator that labels sub-structure with CMK_ZeroOffsetFieldMap, and use that for
            // allocation.
            CompAllocator ialloc(getAllocator(CMK_ZeroOffsetFieldMap));
            m_zeroOffsetFieldMap = new (ialloc) NodeToFieldSeqMap(ialloc);
        }
        return m_zeroOffsetFieldMap;
    }

    // Requires that "op1" is a node of type "TYP_BYREF" or "TYP_I_IMPL".  We are dereferencing this with the fields in
    // "fieldSeq", whose offsets are required all to be zero.  Ensures that any field sequence annotation currently on
    // "op1" or its components is augmented by appending "fieldSeq".  In practice, if "op1" is a GT_LCL_FLD, it has
    // a field sequence as a member; otherwise, it may be the addition of an a byref and a constant, where the const
    // has a field sequence -- in this case "fieldSeq" is appended to that of the constant; otherwise, we
    // record the the field sequence using the ZeroOffsetFieldMap described above.
    //
    // One exception above is that "op1" is a node of type "TYP_REF" where "op1" is a GT_LCL_VAR.
    // This happens when System.Object vtable pointer is a regular field at offset 0 in System.Private.CoreLib in
    // CoreRT. Such case is handled same as the default case.
    void fgAddFieldSeqForZeroOffset(GenTree* op1, FieldSeqNode* fieldSeq);

    NodeToUnsignedMap* m_memorySsaMap[MemoryKindCount];

    // In some cases, we want to assign intermediate SSA #'s to memory states, and know what nodes create those memory
    // states. (We do this for try blocks, where, if the try block doesn't do a call that loses track of the memory
    // state, all the possible memory states are possible initial states of the corresponding catch block(s).)
    NodeToUnsignedMap* GetMemorySsaMap(MemoryKind memoryKind)
    {
        if (memoryKind == GcHeap && byrefStatesMatchGcHeapStates)
        {
            // Use the same map for GCHeap and ByrefExposed when their states match.
            memoryKind = ByrefExposed;
        }

        assert(memoryKind < MemoryKindCount);
        Compiler* compRoot = impInlineRoot();
        if (compRoot->m_memorySsaMap[memoryKind] == nullptr)
        {
            CompAllocator ialloc(getAllocator(CMK_SSA));
            compRoot->m_memorySsaMap[memoryKind] = new (ialloc) NodeToUnsignedMap(ialloc);
        }
        return compRoot->m_memorySsaMap[memoryKind];
    }

    // The Refany type is the only struct type whose structure is implicitly assumed by IL.  We need its fields.
    CORINFO_CLASS_HANDLE m_refAnyClass;
    CORINFO_FIELD_HANDLE GetRefanyDataField()
    {
        if (m_refAnyClass == nullptr)
        {
            m_refAnyClass = info.compCompHnd->getBuiltinClass(CLASSID_TYPED_BYREF);
        }
        return info.compCompHnd->getFieldInClass(m_refAnyClass, 0);
    }
    CORINFO_FIELD_HANDLE GetRefanyTypeField()
    {
        if (m_refAnyClass == nullptr)
        {
            m_refAnyClass = info.compCompHnd->getBuiltinClass(CLASSID_TYPED_BYREF);
        }
        return info.compCompHnd->getFieldInClass(m_refAnyClass, 1);
    }

#if VARSET_COUNTOPS
    static BitSetSupport::BitSetOpCounter m_varsetOpCounter;
#endif
#if ALLVARSET_COUNTOPS
    static BitSetSupport::BitSetOpCounter m_allvarsetOpCounter;
#endif

    static HelperCallProperties s_helperCallProperties;

    bool abiMorphStackStructArg(CallArgInfo* argInfo, GenTree* arg);
    void abiMorphStackLclArgPromoted(CallArgInfo* argInfo, GenTreeLclVar* arg);
    void abiMorphMkRefAnyToFieldList(CallArgInfo* argInfo, GenTreeOp* mkrefany);
    GenTreeFieldList* abiMakeFieldList(GenTree* arg);
    void abiMorphSingleRegStructArg(CallArgInfo* argInfo, GenTree* arg);
    GenTree* abiMorphSingleRegLclArgPromoted(GenTreeLclVar* arg, var_types argRegType, unsigned argSize);
#ifndef TARGET_X86
    void abiMorphArgs2ndPass(GenTreeCall* call);
    GenTree* abiMorphMkRefAnyToStore(unsigned tempLclNum, GenTreeOp* mkrefany);
#if FEATURE_MULTIREG_ARGS
    bool abiCanMorphMultiRegLclArgPromoted(CallArgInfo* argInfo, LclVarDsc* lcl);
    GenTree* abiMorphMultiRegLclArgPromoted(CallArgInfo* argInfo, LclVarDsc* lcl);
    GenTree* abiMorphMultiRegStructArg(CallArgInfo* argInfo, GenTree* arg);
#ifdef FEATURE_SIMD
    GenTree* abiMorphMultiRegSimdArg(CallArgInfo* argInfo, GenTree* arg);
#endif
    GenTree* abiMorphMultiRegLclArg(CallArgInfo* argInfo, GenTreeLclVarCommon* arg);
    GenTree* abiMorphMultiRegObjArg(CallArgInfo* argInfo, GenTreeObj* arg);
    GenTree* abiMakeIndirAddrMultiUse(GenTree** addrInOut, ssize_t* addrOffsetOut, unsigned indirSize);
    GenTree* abiNewMultiLoadIndir(GenTree* addr, ssize_t addrOffset, unsigned indirSize);
#endif
    unsigned abiAllocateStructArgTemp(ClassLayout* argLayout);
    void abiFreeAllStructArgTemps();
#if TARGET_64BIT
    void abiMorphImplicitByRefStructArg(GenTreeCall* call, CallArgInfo* argInfo);
#endif
#endif // !TARGET_X86
#if defined(TARGET_AMD64) || defined(TARGET_ARM64)
    void abiMorphReturnSimdLclPromoted(GenTreeUnOp* ret, GenTree* val);
#endif

    bool killGCRefs(GenTree* tree);
}; // end of class Compiler

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
    static constexpr unsigned NUM_OPS_BITS = 2;
    static constexpr unsigned INS_BITS     = 9;
    static constexpr unsigned SIZE_BITS    = 9;
    static constexpr unsigned OPT_BITS     = 3;
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

    static_assert_no_msg(INS_count <= (1 << INS_BITS));
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
        assert(ins < INS_count);

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
        assert(ins < INS_count);
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
            case GT_LCL_VAR_ADDR:
            case GT_LCL_FLD_ADDR:
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
            case GT_CATCH_ARG:
            case GT_LABEL:
            case GT_FTN_ADDR:
            case GT_RET_EXPR:
            case GT_CNS_INT:
            case GT_CNS_LNG:
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
#if !defined(FEATURE_EH_FUNCLETS)
            case GT_END_LFIN:
#endif // !FEATURE_EH_FUNCLETS
            case GT_PHI_ARG:
            case GT_JMPTABLE:
            case GT_CLS_VAR:
            case GT_CLS_VAR_ADDR:
            case GT_ARGPLACE:
            case GT_PHYSREG:
            case GT_EMITNOP:
            case GT_PINVOKE_PROLOG:
            case GT_PINVOKE_EPILOG:
            case GT_IL_OFFSET:
                break;

            // Lclvar unary operators
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
                FALLTHROUGH;

            // Standard unary operators
            case GT_NOT:
            case GT_NEG:
            case GT_BSWAP:
            case GT_BSWAP16:
            case GT_COPY:
            case GT_RELOAD:
            case GT_ARR_LENGTH:
            case GT_CAST:
            case GT_BITCAST:
            case GT_CKFINITE:
            case GT_LCLHEAP:
            case GT_ADDR:
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
            case GT_NOP:
            case GT_RETURN:
            case GT_RETFILT:
            case GT_RUNTIMELOOKUP:
            case GT_KEEPALIVE:
            case GT_INC_SATURATE:
#ifdef FEATURE_SIMD
            case GT_SIMD_UPPER_SPILL:
            case GT_SIMD_UPPER_UNSPILL:
#endif
            {
                GenTreeUnOp* const unOp = node->AsUnOp();
                if (unOp->gtOp1 != nullptr)
                {
                    result = WalkTree(&unOp->gtOp1, unOp);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }
                break;
            }

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

            case GT_CMPXCHG:
            {
                GenTreeCmpXchg* const cmpXchg = node->AsCmpXchg();

                result = WalkTree(&cmpXchg->gtOpLocation, cmpXchg);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                result = WalkTree(&cmpXchg->gtOpValue, cmpXchg);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                result = WalkTree(&cmpXchg->gtOpComparand, cmpXchg);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                break;
            }

            case GT_ARR_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
            case GT_HW_INTRINSIC_CHK:
#endif
                result = WalkTree(&node->AsBoundsChk()->gtIndex, node);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                result = WalkTree(&node->AsBoundsChk()->gtArrLen, node);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                break;

            case GT_FIELD:
                result = WalkTree(&node->AsField()->gtFldObj, node);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
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
            {
                GenTreeArrOffs* const arrOffs = node->AsArrOffs();

                result = WalkTree(&arrOffs->gtOffset, arrOffs);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                result = WalkTree(&arrOffs->gtIndex, arrOffs);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                result = WalkTree(&arrOffs->gtArrObj, arrOffs);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                break;
            }

            case GT_DYN_BLK:
            {
                GenTreeDynBlk* const dynBlock = node->AsDynBlk();

                GenTree** op1Use = &dynBlock->gtOp1;
                GenTree** op2Use = &dynBlock->gtDynamicSize;

                if (TVisitor::UseExecutionOrder && dynBlock->gtEvalSizeFirst)
                {
                    std::swap(op1Use, op2Use);
                }

                result = WalkTree(op1Use, dynBlock);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                result = WalkTree(op2Use, dynBlock);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                break;
            }

            case GT_STORE_DYN_BLK:
            {
                GenTreeDynBlk* const dynBlock = node->AsDynBlk();

                GenTree** op1Use = &dynBlock->gtOp1;
                GenTree** op2Use = &dynBlock->gtOp2;
                GenTree** op3Use = &dynBlock->gtDynamicSize;

                if (TVisitor::UseExecutionOrder)
                {
                    if (dynBlock->IsReverseOp())
                    {
                        std::swap(op1Use, op2Use);
                    }
                    if (dynBlock->gtEvalSizeFirst)
                    {
                        std::swap(op3Use, op2Use);
                        std::swap(op2Use, op1Use);
                    }
                }

                result = WalkTree(op1Use, dynBlock);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                result = WalkTree(op2Use, dynBlock);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                result = WalkTree(op3Use, dynBlock);
                if (result == fgWalkResult::WALK_ABORT)
                {
                    return result;
                }
                break;
            }

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

                if (call->gtCallType == CT_INDIRECT)
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

            // Binary nodes
            default:
            {
                assert(node->OperIsBinary());

                GenTreeOp* const op = node->AsOp();

                GenTree** op1Use = &op->gtOp1;
                GenTree** op2Use = &op->gtOp2;

                if (TVisitor::UseExecutionOrder && node->IsReverseOp())
                {
                    std::swap(op1Use, op2Use);
                }

                if (*op1Use != nullptr)
                {
                    result = WalkTree(op1Use, op);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
                }

                if (*op2Use != nullptr)
                {
                    result = WalkTree(op2Use, op);
                    if (result == fgWalkResult::WALK_ABORT)
                    {
                        return result;
                    }
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
public:
    enum
    {
        ComputeStack      = computeStack,
        DoPreOrder        = doPreOrder,
        DoPostOrder       = doPostOrder,
        DoLclVarsOnly     = doLclVarsOnly,
        UseExecutionOrder = useExecutionOrder,
    };

private:
    Compiler::fgWalkData* m_walkData;

public:
    GenericTreeWalker(Compiler::fgWalkData* walkData)
        : GenTreeVisitor<GenericTreeWalker<computeStack, doPreOrder, doPostOrder, doLclVarsOnly, useExecutionOrder>>(
              walkData->compiler)
        , m_walkData(walkData)
    {
        assert(walkData != nullptr);

        if (computeStack)
        {
            walkData->parentStack = &this->m_ancestors;
        }
    }

    Compiler::fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
    {
        m_walkData->parent = user;
        return m_walkData->wtprVisitorFn(use, m_walkData);
    }

    Compiler::fgWalkResult PostOrderVisit(GenTree** use, GenTree* user)
    {
        m_walkData->parent = user;
        return m_walkData->wtpoVisitorFn(use, m_walkData);
    }
};

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

// foreach_block: An iterator over all blocks in the function.
//    __compiler: the Compiler* object
//    __block   : a BasicBlock*, already declared, that gets updated each iteration.

#define foreach_block(__compiler, __block)                                                                             \
    for ((__block) = (__compiler)->fgFirstBB; (__block); (__block) = (__block)->bbNext)

/*****************************************************************************/
/*****************************************************************************/

#ifdef DEBUG
void dumpConvertedVarSet(Compiler* comp, VARSET_VALARG_TP vars);
#endif // DEBUG

#include "compiler.hpp" // All the shared inline functions

/*****************************************************************************/
#endif //_COMPILER_H_
/*****************************************************************************/
