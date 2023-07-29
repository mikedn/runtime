// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                          GenTree                                          XX
XX                                                                           XX
XX  This is the node in the semantic tree graph. It represents the operation XX
XX  corresponding to the node, and other information during code-gen.        XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#pragma once

#include "vartype.h"
#include "target.h"
#include "valuenumtype.h"
#include "jitstd/new.h"
#include "jitstd/utility.h"
#include "jithashtable.h"
#include "namedintrinsiclist.h"
#include "layout.h"

// Debugging GenTree is much easier if we add a magic virtual function to make the debugger able to figure out what type
// it's got. This is enabled by default in DEBUG. To enable it in RET builds (temporarily!), you need to change the
// build to define DEBUGGABLE_GENTREE=1, as well as pass /OPT:NOICF to the linker (or else all the vtables get merged,
// making the debugging value supplied by them useless).
#ifndef DEBUGGABLE_GENTREE
#ifdef DEBUG
#define DEBUGGABLE_GENTREE 1
#else
#define DEBUGGABLE_GENTREE 0
#endif
#endif

enum genTreeOps : uint8_t
{
#define GTNODE(n, s, k) GT_##n,
#include "gtlist.h"

    GT_COUNT,

#ifdef TARGET_64BIT
    // GT_CNS_NATIVELONG is the gtOper symbol for GT_CNS_LNG or GT_CNS_INT, depending on the target.
    // For the 64-bit targets we will only use GT_CNS_INT as it used to represent all the possible sizes
    GT_CNS_NATIVELONG = GT_CNS_INT,
#else
    // For the 32-bit targets we use a GT_CNS_LNG to hold a 64-bit integer constant and GT_CNS_INT for all others.
    // In the future when we retarget the JIT for x86 we should consider eliminating GT_CNS_LNG
    GT_CNS_NATIVELONG = GT_CNS_LNG,
#endif
};

enum GenTreeKinds
{
    GTK_SPECIAL   = 0x0000, // Node may have operands and does not use GenTree(Un)Op
    GTK_LEAF      = 0x0001, // Node has no operands
    GTK_UNOP      = 0x0002, // Node struct is GenTreeUnOp or a derived struct that does not add new operands
    GTK_BINOP     = 0x0004, // Node struct is GenTreeOp or a derived struct that doesn't add new operands
    GTK_EXOP      = 0x0008, // Node uses a GenTree(Un)Op derived struct that does not add new operands
    GTK_COMMUTE   = 0x0010, // Node is commutative
    GTK_NOVALUE   = 0x0020, // Node does not produce a value
    GTK_NOTLIR    = 0x0040, // Node is not allowed in LIR
    GTK_NOCONTAIN = 0x0080, // Node cannot be contained
    GTK_VN        = 0x0100, // Oper can be used as VNFunc

    GTK_SMPOP    = GTK_UNOP | GTK_BINOP,
    GTK_KINDMASK = GTK_LEAF | GTK_UNOP | GTK_BINOP,
};

enum CallKind
{
    CT_USER_FUNC,
    CT_HELPER,
    CT_INDIRECT
};

enum class ThrowHelperKind : uint8_t
{
    IndexOutOfRange,       // throw IndexOutOfRangeException
    DivideByZero,          // throw DivideByZeroException
    Overflow,              // throw OverflowException
    Arithmetic = Overflow, // throw OverflowException
    Argument,              // throw ArgumentException
    ArgumentOutOfRange,    // throw ArgumentOutOfRangeException
};

#ifdef DEBUG
/*****************************************************************************
*
*  TargetHandleTypes are used to determine the type of handle present inside GenTreeIntCon node.
*  The values are such that they don't overlap with helper's or user function's handle.
*/
enum TargetHandleType : BYTE
{
    THT_Unknown                  = 2,
    THT_GSCookieCheck            = 4,
    THT_SetGSCookie              = 6,
    THT_IntializeArrayIntrinsics = 8
};
#endif
/*****************************************************************************/

struct BasicBlock;
enum BasicBlockFlags : uint64_t;
struct InlineCandidateInfo;
struct GuardedDevirtualizationCandidateInfo;
struct ClassProfileCandidateInfo;

typedef unsigned AssertionIndex;

static const AssertionIndex NO_ASSERTION_INDEX = 0;

class AssertionInfo
{
    // true if the assertion holds on the bbNext edge instead of the bbJumpDest edge (for GT_JTRUE nodes)
    uint16_t m_isNextEdgeAssertion : 1;
    // 1-based index of the assertion
    uint16_t m_assertionIndex : 15;

    AssertionInfo(bool isNextEdgeAssertion, AssertionIndex assertionIndex)
        : m_isNextEdgeAssertion(isNextEdgeAssertion), m_assertionIndex(static_cast<uint16_t>(assertionIndex))
    {
        assert(assertionIndex < (1u << 15));
    }

public:
    AssertionInfo() : AssertionInfo(false, 0)
    {
    }

    AssertionInfo(AssertionIndex assertionIndex) : AssertionInfo(false, assertionIndex)
    {
    }

    static AssertionInfo ForNextEdge(AssertionIndex assertionIndex)
    {
        // Ignore the edge information if there's no assertion
        bool isNextEdge = (assertionIndex != NO_ASSERTION_INDEX);
        return AssertionInfo(isNextEdge, assertionIndex);
    }

    void Clear()
    {
        m_isNextEdgeAssertion = 0;
        m_assertionIndex      = NO_ASSERTION_INDEX;
    }

    bool HasAssertion() const
    {
        return m_assertionIndex != NO_ASSERTION_INDEX;
    }

    AssertionIndex GetAssertionIndex() const
    {
        return m_assertionIndex;
    }

    bool IsNextEdgeAssertion() const
    {
        return m_isNextEdgeAssertion;
    }
};

// FIELD_ADDR nodes are morphed into ADDs. We'd like to preserve the more abstract
// information, and will therefore annotate such lowered nodes with FieldSeq's.
// A FieldSeq represents a (possibly) empty sequence of fields. The fields are
// in the order in which they are dereferenced. The first field may be an object
// field or a struct field; all subsequent fields must be struct fields.
struct FieldSeqNode
{
    friend class FieldSeqStore;

    // The boxed value field identifies the value part of a boxed value object.
    // Used with static struct fields which are implemented as static reference
    // fields pointing to boxed structs allocated by the runtime.
    static const CORINFO_FIELD_HANDLE BoxedValuePseudoFieldHandle;

    static constexpr unsigned MaxLength = 8;

private:
    // This is a special distinguished FieldSeqNode indicating that a constant does *not*
    // represent a valid field sequence.  This is "infectious", in the sense that appending it
    // (on either side) to any field sequence yields the "NotAField()" sequence.
    static FieldSeqNode s_notAField;
    // Dummy variable to provide an address for the "Boxed Value" pseudo field handle.
    // Since field handles are really pointers this should guarantee that the boxed
    // pseudo field handle doesn't conflict with any real field handles.
    static int BoxedValuePseudoFieldStruct;

    CORINFO_FIELD_HANDLE m_fieldHnd;
    FieldSeqNode*        m_next;

    FieldSeqNode(CORINFO_FIELD_HANDLE fieldHnd) : m_fieldHnd(fieldHnd), m_next(nullptr)
    {
    }

    FieldSeqNode(CORINFO_FIELD_HANDLE fieldHnd, FieldSeqNode* next) : m_fieldHnd(fieldHnd), m_next(next)
    {
    }

    FieldSeqNode(FieldSeqNode* p, FieldSeqNode* n) : m_fieldHnd(p->m_fieldHnd), m_next(n)
    {
    }

public:
    static FieldSeqNode* NotAField()
    {
        return &s_notAField;
    }

    bool IsBoxedValueField() const
    {
        return m_fieldHnd == BoxedValuePseudoFieldHandle;
    }

    bool IsArrayElement() const
    {
        return (reinterpret_cast<uintptr_t>(m_fieldHnd) & 1) != 0;
    }

    bool IsField() const
    {
        return (this != &s_notAField) && !IsBoxedValueField() && !IsArrayElement();
    }

    CORINFO_FIELD_HANDLE GetFieldHandle() const
    {
        assert(IsField());

        return m_fieldHnd;
    }

    unsigned GetArrayElementTypeNum() const
    {
        assert(IsArrayElement());

        return static_cast<unsigned>((reinterpret_cast<uintptr_t>(m_fieldHnd) >> 9));
    }

    uint8_t GetArrayDataOffs() const
    {
        assert(IsArrayElement());

        return static_cast<uint8_t>(reinterpret_cast<uintptr_t>(m_fieldHnd) >> 1);
    }

    FieldSeqNode* GetNext() const
    {
        return m_next;
    }

    FieldSeqNode* GetTail()
    {
        FieldSeqNode* tail = this;
        while (tail->m_next != nullptr)
        {
            tail = tail->m_next;
        }
        return tail;
    }

    FieldSeqNode* RemovePrefix(FieldSeqNode* prefix)
    {
        FieldSeqNode* tail = this;

        // We can probably do this for array elements but we don't need it right now.
        // At the same time, we should not do it for "not a field" so assert.
        assert(tail->IsField());
        assert(prefix->IsField());

        while ((tail != nullptr) && (prefix != nullptr) && (tail->m_fieldHnd == prefix->m_fieldHnd))
        {
            tail   = tail->m_next;
            prefix = prefix->m_next;
        }

        return tail;
    }

    // Make sure this provides methods that allow it to be used as a KeyFuncs type in SimplerHash.
    static int GetHashCode(FieldSeqNode fsn)
    {
        return static_cast<int>(reinterpret_cast<intptr_t>(fsn.m_fieldHnd)) ^
               static_cast<int>(reinterpret_cast<intptr_t>(fsn.m_next));
    }

    static bool Equals(const FieldSeqNode& fsn1, const FieldSeqNode& fsn2)
    {
        return fsn1.m_fieldHnd == fsn2.m_fieldHnd && fsn1.m_next == fsn2.m_next;
    }
};

using FieldSeq = FieldSeqNode;

// This class canonicalizes field sequences.
class FieldSeqStore
{
    typedef JitHashTable<FieldSeqNode, /*KeyFuncs*/ FieldSeqNode, FieldSeqNode*> FieldSeqNodeCanonMap;

    Compiler*             m_compiler;
    CompAllocator         m_alloc;
    FieldSeqNodeCanonMap* m_canonMap;

public:
    FieldSeqStore(Compiler* compiler);

    // Returns the (canonical in the store) singleton field sequence for the given handle.
    FieldSeqNode* CreateSingleton(CORINFO_FIELD_HANDLE fieldHnd);

    FieldSeqNode* GetBoxedValuePseudoField()
    {
        return CreateSingleton(FieldSeqNode::BoxedValuePseudoFieldHandle);
    }

    FieldSeqNode* GetArrayElement(unsigned elementTypeNum, uint8_t dataOffs);

    static FieldSeqNode* NotAField()
    {
        return &FieldSeqNode::s_notAField;
    }

    // Returns the (canonical in the store) field sequence representing the concatenation of
    // the sequences represented by "a" and "b".  Assumes that "a" and "b" are canonical; that is,
    // they are the results of CreateSingleton, NotAField, or Append calls.  If either of the arguments
    // are the "NotAField" value, so is the result.
    FieldSeqNode* Append(FieldSeqNode* a, FieldSeqNode* b);
    FieldSeqNode* Append(FieldSeqNode* a, CORINFO_FIELD_HANDLE b);

    FieldSeqNode* FoldAdd(const struct GenTreeIntCon* i1, const struct GenTreeIntCon* i2);

    INDEBUG(void DebugCheck(FieldSeqNode* f);)
};

class GenTreeUseEdgeIterator;
class GenTreeOperandIterator;

struct Statement;

// clang-format off
enum GenTreeFlags : unsigned
{
    GTF_EMPTY = 0,

    // Side effects (of tree in HIR, of node in LIR)

    GTF_NONE                  = 0,
    GTF_ASG                   = 0x00000001, // Tree contains an assignment, store or intrinsic with similar effect
    GTF_CALL                  = 0x00000002, // Tree contains a call
    GTF_EXCEPT                = 0x00000004, // Tree might throw an exception (OverflowException, NullReferenceException etc.)
    GTF_GLOB_REF              = 0x00000008, // Tree loads/stores from/to aliased memory (GC heap, address exposed locals etc.)
    GTF_ORDER_SIDEEFF         = 0x00000010, // Tree children cannot be reordered

    GTF_PERSISTENT_SIDE_EFFECTS = GTF_ASG | GTF_CALL,
    GTF_SIDE_EFFECT             = GTF_PERSISTENT_SIDE_EFFECTS | GTF_EXCEPT,
    GTF_GLOB_EFFECT             = GTF_SIDE_EFFECT | GTF_GLOB_REF,
    GTF_ALL_EFFECT              = GTF_GLOB_EFFECT | GTF_ORDER_SIDEEFF,

    // Common flags
                            
    GTF_REVERSE_OPS           = 0x00000020, // Second operand must be evaluated before the first operand
    GTF_MAKE_CSE              = 0x00000040, // Hoisted expression - try hard to CSE this expression
    GTF_DONT_CSE              = 0x00000080, // Do not CSE this expression
    GTF_BOOLEAN               = 0x00000100, // Value is known to be 0 or 1
    GTF_UNSIGNED              = 0x00000200, // CAST - treat source operand as unsigned
                                            // ADD/SUB/MUL - unsigned overflow check (meaningless without GTF_OVERFLOW)
    GTF_CONTAINED             = 0x00000400, // Node is contained (executed as part of its user)
    GTF_NOREG_AT_USE          = 0x00000800, // Value is used from spilled temp without reloading into a register
    GTF_REUSE_REG_VAL         = 0x00001000, // Destination register already contains the produced value so code
                                            // generation can be skipped. Only used with constants.
    GTF_SET_FLAGS             = 0x00002000, // Generated instruction must set the condition flags
    GTF_USE_FLAGS             = 0x00004000, // Generated instruction uses the condition flags
    GTF_COMMON_MASK           = 0x0000FFFF, // Mask of all the flags above

    GTF_SPECIFIC_MASK         = 0xFFFF0000, // Mask of all the flags below

    // LCL_VAR & co. specific flags
                              
    GTF_VAR_DEATH             = 0x04000000, // Last-use of a local (LCL_VAR|FLD or dead stores if they're not removed)
    GTF_VAR_FIELD_DEATH0      = 0x04000000, // Last-use bits for up to 4 promoted fields
    GTF_VAR_FIELD_DEATH_MASK  = 0x3C000000,
    GTF_VAR_MULTIREG          = 0x02000000, // Struct or (on 32-bit platforms) LONG local store with a multireg source
                                            // (CALLs and some LONG operations on 32 bit - MUL_LONG, BITCAST)
                                            // returns its result in multiple registers such as a long multiply)
    GTF_VAR_CLONED            = 0x00400000, // Node has been cloned (used by inliner to detect single use params)
    GTF_VAR_CONTEXT           = 0x00200000, // Node is part of a runtime lookup tree (LCL_VAR)
                              
    // CALL specific flags

    GTF_CALL_UNMANAGED        = 0x80000000, // Call to unmanaged code
    GTF_CALL_INLINE_CANDIDATE = 0x40000000, // Inline candidate

    GTF_CALL_VIRT_KIND_MASK   = 0x30000000, // Mask of the below call kinds
    GTF_CALL_NONVIRT          = 0x00000000, // Non virtual call
    GTF_CALL_VIRT_STUB        = 0x10000000, // Stub-dispatch virtual call
    GTF_CALL_VIRT_VTABLE      = 0x20000000, // Vtable-based virtual call

    GTF_CALL_NULLCHECK        = 0x08000000, // Null check `this`
#ifdef TARGET_X86
    GTF_CALL_POP_ARGS         = 0x04000000, // Caller pops arguments
#endif
    GTF_CALL_HOISTABLE        = 0x02000000, // Hoistable call (known helper calls)

    // MEMORYBARRIER specific flags

    GTF_MEMORYBARRIER_LOAD    = 0x40000000, // Load barrier (instead of full barrier)

    // INDEX_ADDR specific flags

    GTF_INX_RNGCHK            = 0x80000000, // The array index must be range-checked

    // BOUNDS_CHECK specific flags

    GTF_BOUND_VALID           = 0x80000000, // Index is known to be valid

    // ARR_LENGTH specific flags

    GTF_ARRLEN_NONFAULTING    = 0x20000000, // Array reference is known to be non-null (same as GT_IND_NONFAULTING).

    // IND & co. specific flags

    GTF_IND_TGT_NOT_HEAP      = 0x80000000, // Address is known to point outside the GC heap
    GTF_IND_VOLATILE          = 0x40000000, // Volatile load/store
    GTF_IND_NONFAULTING       = 0x20000000, // Address is known to be non-null
    GTF_IND_TGT_HEAP          = 0x10000000, // Address is known to point inside the GC heap
    GTF_IND_UNALIGNED         = 0x02000000, // Unaligned load/store (1 byte aligned)
    GTF_IND_INVARIANT         = 0x01000000, // Load produces the same value throughout the method
    GTF_IND_NONNULL           = 0x00400000, // Load always produces a non-null value

    // CNS_INT specific flags

    GTF_ICON_HDL_MASK         = 0xF0000000, // Mask for flags below
    GTF_ICON_MODULE_HDL       = 0x10000000, // Module handle
    GTF_ICON_CLASS_HDL        = 0x20000000, // Class handle
    GTF_ICON_METHOD_HDL       = 0x30000000, // Method handle
    GTF_ICON_FIELD_HDL        = 0x40000000, // Field handle
    GTF_ICON_STATIC_HDL       = 0x50000000, // Static field address
    GTF_ICON_STR_HDL          = 0x60000000, // String handle
    GTF_ICON_CONST_PTR        = 0x70000000, // Immutable data address, (e.g. IAT_PPVALUE)
    GTF_ICON_GLOBAL_PTR       = 0x80000000, // Mutable data address (e.g. from the VM state)
    GTF_ICON_VARG_HDL         = 0x90000000, // Vararg cookie handle
    GTF_ICON_PINVKI_HDL       = 0xA0000000, // P/Invoke calli handle
    GTF_ICON_TOKEN_HDL        = 0xB0000000, // Token handle (other than class, method or field)
#ifdef WINDOWS_X86_ABI
    GTF_ICON_TLS_HDL          = 0xC0000000, // TLS ref with offset
#endif
    GTF_ICON_FTN_ADDR         = 0xD0000000, // Method address
    GTF_ICON_CIDMID_HDL       = 0xE0000000, // Class ID or module ID
    GTF_ICON_BBC_PTR          = 0xF0000000, // Address of basic block instrumentation count

    GTF_ICON_SIMD_COUNT       = 0x04000000, // Vector<T>.Count
    GTF_ICON_INITCLASS        = 0x02000000, // Address of a static field that requires initialization
                                            // (same as GTF_CLS_VAR_INITCLASS)

    // CLS_VAR_ADDR specific flags

    GTF_CLS_VAR_INITCLASS     = 0x20000000, // Address of a static field that requires initialization

    // RETURN specific flags

    GTF_RET_MERGED            = 0x80000000, // This is a return generated during epilog merging

    // QMARK specific flags

    GTF_QMARK_CAST_INSTOF     = 0x80000000, // Top (not nested) level QMARK created for castclass or instanceof

    // EQ, NE, LT, LE, GT, GE flags

    GTF_RELOP_NAN_UN          = 0x80000000, // Unordered floating point compare
    GTF_RELOP_JMP_USED        = 0x40000000, // Node is used by a JTRUE
    GTF_RELOP_ZTT             = 0x08000000, // Loop test cloned for converting while-loops into do-while
                                            // with explicit "loop test" in the header block.

    // JCMP specific flags

    GTF_JCMP_EQ               = 0x80000000, // CBZ/TBZ (rather than CBNZ/TBNZ)
    GTF_JCMP_TST              = 0x40000000, // TBZ/TBNZ (rather than CBZ/CBNZ)

    // Unary/Binary op specific flags

    GTF_OVERFLOW              = 0x10000000, // ADD/SUB/MUL/CAST - overflow check
    GTF_DIV_BY_CNS_OPT        = 0x80000000, // DIV - division by constant optimization
    GTF_ADDRMODE_NO_CSE       = 0x80000000, // ADD/MUL/LSH/COMMA - address mode component, do not CSE
                                            // (unlike GTF_DONT_CSE this does not block constant propagation)
};

constexpr GenTreeFlags operator ~(GenTreeFlags a)
{
    return static_cast<GenTreeFlags>(~static_cast<unsigned>(a));
}

constexpr GenTreeFlags operator |(GenTreeFlags a, GenTreeFlags b)
{
    return static_cast<GenTreeFlags>(static_cast<unsigned>(a) | static_cast<unsigned>(b));
}

constexpr GenTreeFlags operator &(GenTreeFlags a, GenTreeFlags b)
{
    return static_cast<GenTreeFlags>(static_cast<unsigned>(a) & static_cast<unsigned>(b));
}

constexpr GenTreeFlags operator ^(GenTreeFlags a, GenTreeFlags b)
{
    return static_cast<GenTreeFlags>(static_cast<unsigned>(a) ^ static_cast<unsigned>(b));
}

inline GenTreeFlags& operator |=(GenTreeFlags& a, GenTreeFlags b)
{
    return a = a | b;
}

inline GenTreeFlags& operator &=(GenTreeFlags& a, GenTreeFlags b)
{
    return a = a & b;
}

inline GenTreeFlags& operator ^=(GenTreeFlags& a, GenTreeFlags b)
{
    return a = a ^ b;
}

// Can any side-effects be observed externally, say by a caller method?
// For assignments, only assignments to global memory can be observed
// externally, whereas simple assignments to local variables can not.
//
// Be careful when using this inside a "try" protected region as the
// order of assignments to local variables would need to be preserved
// wrt side effects if the variables are alive on entry to the
// "catch/finally" region. In such cases, even assignments to locals
// will have to be restricted.
#define GTF_GLOBALLY_VISIBLE_SIDE_EFFECTS(flags) \
    (((flags) & (GTF_CALL | GTF_EXCEPT)) || (((flags) & (GTF_ASG | GTF_GLOB_REF)) == (GTF_ASG | GTF_GLOB_REF)))

#if defined(DEBUG)

enum GenTreeDebugFlags : unsigned 
{
    GTF_DEBUG_NONE              = 0x00000000, 

    GTF_DEBUG_NODE_MORPHED      = 0x00000001, // Node has been morphed (in the global morphing phase)
    GTF_DEBUG_NODE_SMALL        = 0x00000002,
    GTF_DEBUG_NODE_LARGE        = 0x00000004,
    GTF_DEBUG_NODE_CG_PRODUCED  = 0x00000008, // DefReg has been called on this node
    GTF_DEBUG_NODE_CG_CONSUMED  = 0x00000010, // UseReg has been called on this node
    GTF_DEBUG_NODE_LSRA_ADDED   = 0x00000020, // This node was added by LSRA

    GTF_DEBUG_NODE_MASK         = 0x0000003F, // These flags are all node (rather than operation) properties.

    GTF_DEBUG_HAS_COSTS         = 0x00000100,

    GTF_DEBUG_HAS_REG_0         = 0x01000000, // One bit for each def reg
    GTF_DEBUG_HAS_REGS_MASK     = 0xFF000000,
};

inline constexpr GenTreeDebugFlags operator ~(GenTreeDebugFlags a)
{
    return (GenTreeDebugFlags)(~(unsigned int)a);
}

inline constexpr GenTreeDebugFlags operator |(GenTreeDebugFlags a, GenTreeDebugFlags b)
{
    return (GenTreeDebugFlags)((unsigned int)a | (unsigned int)b);
}

inline constexpr GenTreeDebugFlags operator &(GenTreeDebugFlags a, GenTreeDebugFlags b)
{
    return (GenTreeDebugFlags)((unsigned int)a & (unsigned int)b);
}

inline GenTreeDebugFlags& operator |=(GenTreeDebugFlags& a, GenTreeDebugFlags b)
{
    return a = (GenTreeDebugFlags)((unsigned int)a | (unsigned int)b);
}

inline GenTreeDebugFlags& operator &=(GenTreeDebugFlags& a, GenTreeDebugFlags b)
{
    return a = (GenTreeDebugFlags)((unsigned int)a & (unsigned int)b);
}

#endif // defined(DEBUG)

// clang-format on

using RegSpillSet = uint8_t;

static_assert_no_msg(sizeof(RegSpillSet) * 8 >= MAX_MULTIREG_COUNT * 2);

constexpr RegSpillSet RegSpillSetMask = (1 << (MAX_MULTIREG_COUNT * 2)) - 1;

constexpr RegSpillSet AllRegSpillSet = 0x55 & RegSpillSetMask;

constexpr RegSpillSet GetRegSpillSet(unsigned regIndex)
{
    return 1 << (regIndex * 2);
}

constexpr RegSpillSet AllRegSpilledSet = (0x55 << 1) & RegSpillSetMask;

constexpr RegSpillSet GetRegSpilledSet(unsigned regIndex)
{
    return 1 << (regIndex * 2 + 1);
}

#ifndef HOST_64BIT
#include <pshpack4.h>
#endif

#define FMT_TREEID "[%06u]"

enum CseInfo : int16_t
{
    NoCse = 0
};

constexpr bool IsCseUse(CseInfo info)
{
    return info > 0;
}

constexpr bool IsCseDef(CseInfo info)
{
    return info < 0;
}

constexpr unsigned GetCseIndex(CseInfo info)
{
    return info > 0 ? info : -info;
}

#define MAX_COST UCHAR_MAX
#define IND_COST_EX 3 // execution cost for an indirection

// Forward declarations of the subtypes
#define GTSTRUCT_0(fn, en) struct GenTree##fn;
#define GTSTRUCT_1(fn, en) struct GenTree##fn;
#define GTSTRUCT_2(fn, en, en2) struct GenTree##fn;
#define GTSTRUCT_3(fn, en, en2, en3) struct GenTree##fn;
#define GTSTRUCT_4(fn, en, en2, en3, en4) struct GenTree##fn;
#define GTSTRUCT_N(fn, ...) struct GenTree##fn;
#define GTSTRUCT_2_SPECIAL(fn, en, en2) GTSTRUCT_2(fn, en, en2)
#define GTSTRUCT_3_SPECIAL(fn, en, en2, en3) GTSTRUCT_3(fn, en, en2, en3)
#include "gtstructs.h"

struct GenTree
{
    static const uint8_t s_gtNodeSizes[];
#if NODEBASH_STATS || MEASURE_NODE_SIZE || COUNT_AST_OPERS
    static const uint8_t s_gtTrueSizes[];
#endif
#if COUNT_AST_OPERS
    static LONG s_gtNodeCounts[];
#endif

    genTreeOps gtOper;
    var_types  gtType;

private:
    union {
        // Valid only during CSE, 0 or the CSE index (negated if it's a def).
        CseInfo m_cseInfo = NoCse;
#if ASSERTION_PROP
        // Valid only during global assertion propagation.
        AssertionInfo m_assertionInfo;
#endif
        // Valid only during LSRA/CodeGen
        RegSpillSet m_defRegsSpillSet;
    };

    // Used for nodes that are in LIR. See LIR::Flags in lir.h for the various flags.
    uint8_t m_LIRFlags = 0;
    uint8_t m_costEx; // estimate of expression execution cost
    uint8_t m_costSz; // estimate of expression code size cost

    // The registers defined by the node.
    regNumberSmall m_defRegs[MAX_MULTIREG_COUNT]{static_cast<regNumberSmall>(REG_NA)
#ifndef TARGET_64BIT
                                                     ,
                                                 static_cast<regNumberSmall>(REG_NA)
#endif
    };

    ValueNumPair m_vnp;

public:
    GenTreeFlags gtFlags = GTF_EMPTY;
    regMaskTP    gtRsvdRegs;
    GenTree*     gtNext = nullptr;
    GenTree*     gtPrev = nullptr;
#ifdef DEBUG
    GenTreeDebugFlags gtDebugFlags = GTF_DEBUG_NONE;
    unsigned          gtTreeID;
    unsigned          gtSeqNum   = 0;       // liveness traversal order within the current statement
    int               gtUseNum   = -1;      // use-ordered traversal within the function
    genTreeOps        gtOperSave = GT_NONE; // Only used to save gtOper when we destroy a node, to aid debugging.
#endif

// We use GT_STRUCT_0 only for the category of simple ops.
#define GTSTRUCT_0(fn, en)                                                                                             \
    GenTree##fn* As##fn()                                                                                              \
    {                                                                                                                  \
        assert(OperIsSimple());                                                                                        \
        return reinterpret_cast<GenTree##fn*>(this);                                                                   \
    }                                                                                                                  \
    const GenTree##fn* As##fn() const                                                                                  \
    {                                                                                                                  \
        assert(OperIsSimple());                                                                                        \
        return reinterpret_cast<const GenTree##fn*>(this);                                                             \
    }

#define GTSTRUCT_N(fn, ...)                                                                                            \
    GenTree##fn* As##fn()                                                                                              \
    {                                                                                                                  \
        assert(OperIs(__VA_ARGS__));                                                                                   \
        return reinterpret_cast<GenTree##fn*>(this);                                                                   \
    }                                                                                                                  \
    const GenTree##fn* As##fn() const                                                                                  \
    {                                                                                                                  \
        assert(OperIs(__VA_ARGS__));                                                                                   \
        return reinterpret_cast<const GenTree##fn*>(this);                                                             \
    }                                                                                                                  \
    GenTree##fn* Is##fn()                                                                                              \
    {                                                                                                                  \
        return OperIs(__VA_ARGS__) ? reinterpret_cast<GenTree##fn*>(this) : nullptr;                                   \
    }                                                                                                                  \
    const GenTree##fn* Is##fn() const                                                                                  \
    {                                                                                                                  \
        return OperIs(__VA_ARGS__) ? reinterpret_cast<const GenTree##fn*>(this) : nullptr;                             \
    }

#define GTSTRUCT_1(fn, en) GTSTRUCT_N(fn, en)
#define GTSTRUCT_2(fn, en, en2) GTSTRUCT_N(fn, en, en2)
#define GTSTRUCT_3(fn, en, en2, en3) GTSTRUCT_N(fn, en, en2, en3)
#define GTSTRUCT_4(fn, en, en2, en3, en4) GTSTRUCT_N(fn, en, en2, en3, en4)
#define GTSTRUCT_2_SPECIAL(fn, en, en2) GTSTRUCT_2(fn, en, en2)
#define GTSTRUCT_3_SPECIAL(fn, en, en2, en3) GTSTRUCT_3(fn, en, en2, en3)

#include "gtstructs.h"

#undef GTSTRUCT_0
#undef GTSTRUCT_1
#undef GTSTRUCT_2
#undef GTSTRUCT_3
#undef GTSTRUCT_4
#undef GTSTRUCT_N
#undef GTSTRUCT_2_SPECIAL
#undef GTSTRUCT_3_SPECIAL

    genTreeOps OperGet() const
    {
        return gtOper;
    }
    var_types TypeGet() const
    {
        return gtType;
    }

    genTreeOps GetOper() const
    {
        return gtOper;
    }

    var_types GetType() const
    {
        return gtType;
    }

    void SetType(var_types type)
    {
        assert(((TYP_UNDEF < type) && (type < TYP_UNKNOWN)) && (type != TYP_UINT) && (type != TYP_ULONG));
        gtType = type;
    }

    CseInfo GetCseInfo() const
    {
        return m_cseInfo;
    }

    bool HasCseInfo() const
    {
        return m_cseInfo != NoCse;
    }

    void SetCseInfo(CseInfo num)
    {
        m_cseInfo = num;
    }

    void ClearCseInfo()
    {
        m_cseInfo = NoCse;
    }

#if ASSERTION_PROP
    bool GeneratesAssertion() const
    {
        return m_assertionInfo.HasAssertion();
    }

    void ClearAssertionInfo()
    {
        m_assertionInfo.Clear();
    }

    AssertionInfo GetAssertionInfo() const
    {
        return m_assertionInfo;
    }

    void SetAssertionInfo(AssertionInfo info)
    {
        m_assertionInfo = info;
    }
#endif

#ifdef DEBUG
    bool HasCosts() const
    {
        return (gtDebugFlags & GTF_DEBUG_HAS_COSTS) != 0;
    }
#endif

    unsigned GetCostEx() const
    {
        assert((gtDebugFlags & GTF_DEBUG_HAS_COSTS) != 0);
        return m_costEx;
    }

    unsigned GetCostSz() const
    {
        assert((gtDebugFlags & GTF_DEBUG_HAS_COSTS) != 0);
        return m_costSz;
    }

    void SetCosts(unsigned costEx, unsigned costSz)
    {
        assert(costEx != UINT32_MAX); // looks bogus
        assert(costSz != UINT32_MAX); // looks bogus

        m_costEx = (costEx > UINT8_MAX) ? UINT8_MAX : static_cast<uint8_t>(costEx);
        m_costSz = (costSz > UINT8_MAX) ? UINT8_MAX : static_cast<uint8_t>(costSz);
        INDEBUG(gtDebugFlags |= GTF_DEBUG_HAS_COSTS;)
    }

    void CopyCosts(const GenTree* const tree)
    {
        m_costEx = tree->m_costEx;
        m_costSz = tree->m_costSz;
        INDEBUG(gtDebugFlags = (gtDebugFlags & ~GTF_DEBUG_HAS_COSTS) | (tree->gtDebugFlags & GTF_DEBUG_HAS_COSTS);)
    }

    // The register number is stored in a small format (8 bits), but the getters return and the setters take
    // a full-size (unsigned) format, to localize the casts here.

    INDEBUG(bool canBeContained() const;)

    // for codegen purposes, is this node a subnode of its parent
    bool isContained() const;

    bool isContainedIntOrIImmed()
    {
        return IsContainedIntCon();
    }

    GenTreeIntCon* IsContainedIntCon()
    {
        return isContained() && IsIntCon() && !isUsedFromSpillTemp() ? AsIntCon() : nullptr;
    }

    bool isContainedFltOrDblImmed() const
    {
        return isContained() && (OperGet() == GT_CNS_DBL);
    }

    bool isUsedFromSpillTemp() const
    {
        // If spilled and no reg at use, then it is used from the spill temp location rather than being reloaded.
        return IsAnyRegSpilled() && ((gtFlags & GTF_NOREG_AT_USE) != 0);
    }

    bool isMemoryOp() const
    {
        return OperIs(GT_IND, GT_STOREIND, GT_LCL_FLD, GT_STORE_LCL_FLD);
    }

    bool isUsedFromMemory() const
    {
        return isUsedFromSpillTemp() || (isContained() && (isMemoryOp() || OperIs(GT_LCL_VAR, GT_CNS_DBL)));
    }

    bool isUsedFromReg() const
    {
        return !isUsedFromSpillTemp() && !isContained();
    }

#ifdef DEBUG
    bool HasRegs() const
    {
        return (gtDebugFlags & GTF_DEBUG_HAS_REGS_MASK) != 0;
    }

    bool HasReg(unsigned i) const
    {
        return (gtDebugFlags & (GTF_DEBUG_HAS_REG_0 << i)) != 0;
    }
#endif

    void ClearOtherRegs()
    {
        for (unsigned i = 1; i < _countof(m_defRegs); ++i)
        {
            SetRegNum(i, REG_NA);
        }
    }

    regNumber GetRegNum() const
    {
        // TODO-Cleanup: This should be called on nodes that do not have a register assigned.
        // However, a lot of code calls it an instead checks for REG_NA.
        // assert((gtDebugFlags & GTF_DEBUG_HAS_REG_0) != 0);

        regNumber reg = static_cast<regNumber>(m_defRegs[0]);
        assert(!HasReg(0) || (REG_FIRST <= reg && reg <= REG_COUNT));
        return reg;
    }

    void SetRegNum(regNumber reg)
    {
        assert(REG_FIRST <= reg && reg <= REG_COUNT);

        m_defRegs[0] = static_cast<regNumberSmall>(reg);
        INDEBUG(gtDebugFlags |= GTF_DEBUG_HAS_REG_0;)
    }

    regNumber GetRegNum(unsigned i) const
    {
        assert(i < _countof(m_defRegs));

        regNumber reg = static_cast<regNumber>(m_defRegs[i]);
        assert(!HasReg(0) || (REG_FIRST <= reg && reg <= REG_COUNT));
        return reg;
    }

    void SetRegNum(unsigned i, regNumber reg)
    {
        assert(i < _countof(m_defRegs));
        assert(REG_FIRST <= reg && reg <= REG_COUNT);

        m_defRegs[i] = static_cast<regNumberSmall>(reg);
        INDEBUG(gtDebugFlags |= static_cast<GenTreeDebugFlags>(GTF_DEBUG_HAS_REG_0 << i););
    }

    bool gtHasReg() const;

    INDEBUG(int GetRegisterDstCount(Compiler* compiler) const;)

    regMaskTP gtGetRegMask() const;

    bool IsRegSpill(unsigned i) const
    {
        return (m_defRegsSpillSet & GetRegSpillSet(i)) != 0;
    }

    bool IsAnyRegSpill() const
    {
        return (m_defRegsSpillSet & AllRegSpillSet) != 0;
    }

    void SetRegSpill(unsigned i, bool spill)
    {
        if (spill)
        {
            m_defRegsSpillSet |= GetRegSpillSet(i);
        }
        else
        {
            m_defRegsSpillSet &= ~GetRegSpillSet(i);
        }
    }

    bool IsRegSpilled(unsigned i) const
    {
        return (m_defRegsSpillSet & GetRegSpilledSet(i)) != 0;
    }

    bool IsAnyRegSpilled() const
    {
        return (m_defRegsSpillSet & AllRegSpilledSet) != 0;
    }

    void SetRegSpilled(unsigned i, bool spilled)
    {
        if (spilled)
        {
            m_defRegsSpillSet |= GetRegSpilledSet(i);
        }
        else
        {
            m_defRegsSpillSet &= ~GetRegSpilledSet(i);
        }
    }

    void ClearRegSpillSet()
    {
        m_defRegsSpillSet = 0;
    }

    unsigned AvailableTempRegCount(regMaskTP mask = (regMaskTP)-1) const;
    regNumber GetSingleTempReg(regMaskTP mask = (regMaskTP)-1);
    regNumber ExtractTempReg(regMaskTP mask = (regMaskTP)-1);
    bool HasTempReg(regNumber reg) const;

    ValueNumPair GetVNP() const
    {
        return m_vnp;
    }

    ValueNum GetLiberalVN() const
    {
        return m_vnp.GetLiberal();
    }

    void SetLiberalVN(ValueNum vn)
    {
        m_vnp.SetLiberal(vn);
    }

    void SetConservativeVN(ValueNum vn)
    {
        m_vnp.SetConservative(vn);
    }

    ValueNum GetConservativeVN() const
    {
        return m_vnp.GetConservative();
    }

    void SetVNP(ValueNumPair vnp)
    {
        m_vnp = vnp;
    }

    ValueNum GetVN(ValueNumKind vnk) const
    {
        return m_vnp.Get(vnk);
    }

    void SetVN(ValueNumKind vnk, ValueNum vn)
    {
        m_vnp.Set(vnk, vn);
    }

    GenTreeFlags GetSideEffects() const
    {
        return gtFlags & GTF_ALL_EFFECT;
    }

    bool HasAllSideEffects(GenTreeFlags sideEffects) const
    {
        return (gtFlags & sideEffects) == sideEffects;
    }

    bool HasAnySideEffect(GenTreeFlags sideEffects) const
    {
        return (gtFlags & sideEffects) != GTF_NONE;
    }

    void SetSideEffects(GenTreeFlags sideEffects)
    {
        assert((sideEffects & ~GTF_ALL_EFFECT) == 0);
        gtFlags = (gtFlags & ~GTF_ALL_EFFECT) | sideEffects;
    }

    void AddSideEffects(GenTreeFlags sideEffects)
    {
        assert((sideEffects & ~GTF_ALL_EFFECT) == 0);
        gtFlags |= sideEffects;
    }

    static GenTreeKinds OperKind(genTreeOps gtOper);

    GenTreeKinds OperKind() const
    {
        return OperKind(gtOper);
    }

    static bool IsExOp(unsigned opKind)
    {
        return (opKind & GTK_EXOP) != 0;
    }

    bool IsValue() const
    {
        if ((OperKind(gtOper) & GTK_NOVALUE) != 0)
        {
            return false;
        }

        if (gtType == TYP_VOID)
        {
            // These are the only operators which can produce either VOID or non-VOID results.
            assert(OperIs(GT_NOP, GT_CALL, GT_COMMA, GT_INSTR) || OperIsCompare() || OperIsLong() ||
                   OperIsHWIntrinsic());
            return false;
        }

        return true;
    }

    bool IsLIR() const
    {
        if ((OperKind(gtOper) & GTK_NOTLIR) != 0)
        {
            return false;
        }

        if (gtOper == GT_NOP)
        {
            // NOPs may only be present in LIR if they do not produce a value.
            return IsNothingNode();
        }

        // All other nodes are assumed to be correct.
        return true;
    }

    bool HasImplicitFlagsDef() const
    {
        return (gtFlags & GTF_SET_FLAGS) != 0;
    }

    bool HasImplicitFlagsUse() const
    {
        return (gtFlags & GTF_USE_FLAGS) != 0;
    }

    // LIR flags
    //   These helper methods, along with the flag values they manipulate, are defined in lir.h
    //
    // UnusedValue indicates that, although this node produces a value, it is unused.
    void SetUnusedValue();
    void ClearUnusedValue();
    bool IsUnusedValue() const;
    // RegOptional indicates that codegen can still generate code even if it isn't allocated a register.
    bool IsRegOptional() const;
    void SetRegOptional();
    void ClearRegOptional();

    void SetLIRMark();
    bool HasLIRMark() const;
    void ClearLIRMark();

    bool TypeIs(var_types type) const
    {
        return gtType == type;
    }

    template <typename... T>
    bool TypeIs(var_types type, T... rest) const
    {
        return TypeIs(type) || TypeIs(rest...);
    }

    static bool StaticOperIs(genTreeOps operCompare, genTreeOps oper)
    {
        return operCompare == oper;
    }

    template <typename... T>
    static bool StaticOperIs(genTreeOps operCompare, genTreeOps oper, T... rest)
    {
        return StaticOperIs(operCompare, oper) || StaticOperIs(operCompare, rest...);
    }

    bool OperIs(genTreeOps oper) const
    {
        return OperGet() == oper;
    }

    template <typename... T>
    bool OperIs(genTreeOps oper, T... rest) const
    {
        return OperIs(oper) || OperIs(rest...);
    }

    static bool OperIsConst(genTreeOps gtOper)
    {
        // TODO-MIKE-Cleanup: Including GT_CNS_STR in "const" operator is as dumb as it gets.
        return (gtOper == GT_CNS_INT) || (gtOper == GT_CNS_LNG) || (gtOper == GT_CNS_DBL) || (gtOper == GT_CNS_STR);
    }

    bool OperIsConst() const
    {
        return OperIsConst(gtOper);
    }

    static bool OperIsLeaf(genTreeOps gtOper)
    {
        return (OperKind(gtOper) & GTK_LEAF) != 0;
    }

    bool OperIsLeaf() const
    {
        return (OperKind(gtOper) & GTK_LEAF) != 0;
    }

    static bool OperIsCompare(genTreeOps gtOper)
    {
        return (gtOper == GT_EQ) || (gtOper == GT_NE) || (gtOper == GT_LT) || (gtOper == GT_LE) || (gtOper == GT_GE) ||
               (gtOper == GT_GT) || (gtOper == GT_TEST_EQ) || (gtOper == GT_TEST_NE);
    }

    bool IsConstInitVal() const
    {
        return OperIs(GT_CNS_INT) || (OperIs(GT_INIT_VAL) && gtGetOp1()->OperIs(GT_CNS_INT));
    }

    bool OperIsPutArgSplit() const
    {
#if FEATURE_ARG_SPLIT
        return gtOper == GT_PUTARG_SPLIT;
#else // !FEATURE_ARG_SPLIT
        return false;
#endif
    }

    bool OperIsPutArgStk() const
    {
        return gtOper == GT_PUTARG_STK;
    }

    bool OperIsPutArgStkOrSplit() const
    {
        return OperIsPutArgStk() || OperIsPutArgSplit();
    }

    bool OperIsPutArgReg() const
    {
        return gtOper == GT_PUTARG_REG;
    }

    bool OperIsPutArg() const
    {
        return OperIsPutArgStk() || OperIsPutArgReg() || OperIsPutArgSplit();
    }

    bool IsMultiRegOpLong() const
    {
#ifdef TARGET_64BIT
        return nullptr;
#else
        return TypeIs(TYP_LONG) && OperIs(GT_MUL_LONG, GT_BITCAST, GT_PUTARG_REG);
#endif
    }

    bool OperIsCompare() const
    {
        return OperIsCompare(gtOper);
    }

    static bool OperIsShift(genTreeOps gtOper)
    {
        return (gtOper == GT_LSH) || (gtOper == GT_RSH) || (gtOper == GT_RSZ);
    }

    bool OperIsShift() const
    {
        return OperIsShift(OperGet());
    }

    static bool OperIsShiftLong(genTreeOps gtOper)
    {
#ifdef TARGET_64BIT
        return false;
#else
        return (gtOper == GT_LSH_HI) || (gtOper == GT_RSH_LO);
#endif
    }

    bool OperIsShiftLong() const
    {
        return OperIsShiftLong(OperGet());
    }

    static bool OperIsRotate(genTreeOps gtOper)
    {
        return (gtOper == GT_ROL) || (gtOper == GT_ROR);
    }

    bool OperIsRotate() const
    {
        return OperIsRotate(OperGet());
    }

    static bool OperIsShiftOrRotate(genTreeOps gtOper)
    {
        return OperIsShift(gtOper) || OperIsRotate(gtOper) || OperIsShiftLong(gtOper);
    }

    bool OperIsShiftOrRotate() const
    {
        return OperIsShiftOrRotate(OperGet());
    }

    static bool OperIsMul(genTreeOps gtOper)
    {
        return (gtOper == GT_MUL) || (gtOper == GT_MULHI)
#if !defined(TARGET_64BIT)
               || (gtOper == GT_MUL_LONG)
#endif
            ;
    }

    bool OperIsMul() const
    {
        return OperIsMul(gtOper);
    }

#ifdef TARGET_XARCH
    static bool OperIsRMWMemOp(genTreeOps gtOper)
    {
        // Return if binary op is one of the supported operations for RMW of memory.
        return gtOper == GT_ADD || gtOper == GT_SUB || gtOper == GT_AND || gtOper == GT_OR || gtOper == GT_XOR ||
               gtOper == GT_NOT || gtOper == GT_NEG || OperIsShift(gtOper) || OperIsRotate(gtOper);
    }
    bool OperIsRMWMemOp() const
    {
        // Return if binary op is one of the supported operations for RMW of memory.
        return OperIsRMWMemOp(gtOper);
    }
#endif // TARGET_XARCH

    static bool OperIsUnary(genTreeOps gtOper)
    {
        return (OperKind(gtOper) & GTK_UNOP) != 0;
    }

    bool OperIsUnary() const
    {
        return OperIsUnary(gtOper);
    }

    static bool OperIsBinary(genTreeOps gtOper)
    {
        return (OperKind(gtOper) & GTK_BINOP) != 0;
    }

    bool OperIsBinary() const
    {
        return OperIsBinary(gtOper);
    }

    static bool OperIsSimple(genTreeOps gtOper)
    {
        return (OperKind(gtOper) & GTK_SMPOP) != 0;
    }

    static bool OperIsSpecial(genTreeOps gtOper)
    {
        return ((OperKind(gtOper) & GTK_KINDMASK) == GTK_SPECIAL);
    }

    bool OperIsSimple() const
    {
        return OperIsSimple(gtOper);
    }

#ifdef FEATURE_HW_INTRINSICS
    bool isCommutativeHWIntrinsic() const;
    bool isContainableHWIntrinsic() const;
    bool isRMWHWIntrinsic(Compiler* comp);
#else
    bool isCommutativeHWIntrinsic() const
    {
        return false;
    }

    bool isContainableHWIntrinsic() const
    {
        return false;
    }

    bool isRMWHWIntrinsic(Compiler* comp)
    {
        return false;
    }
#endif // FEATURE_HW_INTRINSICS

    static bool OperIsCommutative(genTreeOps gtOper)
    {
        return (OperKind(gtOper) & GTK_COMMUTE) != 0;
    }

    bool OperIsCommutative()
    {
        return OperIsCommutative(gtOper) || (OperIsHWIntrinsic(gtOper) && isCommutativeHWIntrinsic());
    }

    static bool OperMayOverflow(genTreeOps gtOper)
    {
        return ((gtOper == GT_ADD) || (gtOper == GT_SUB) || (gtOper == GT_MUL) || (gtOper == GT_CAST)
#if !defined(TARGET_64BIT)
                || (gtOper == GT_ADD_HI) || (gtOper == GT_SUB_HI)
#endif
                    );
    }

    bool OperMayOverflow() const
    {
        return OperMayOverflow(gtOper);
    }

    static bool OperIsIndir(genTreeOps gtOper)
    {
        return (gtOper == GT_IND) || (gtOper == GT_STOREIND) || (gtOper == GT_NULLCHECK) || (gtOper == GT_BLK) ||
               (gtOper == GT_OBJ) || (gtOper == GT_STORE_BLK) || (gtOper == GT_STORE_OBJ);
    }

    static bool OperIsIndirOrArrLength(genTreeOps gtOper)
    {
        return OperIsIndir(gtOper) || (gtOper == GT_ARR_LENGTH);
    }

    bool OperIsIndir() const
    {
        return OperIsIndir(gtOper);
    }

    bool OperIsIndirOrArrLength() const
    {
        return OperIsIndirOrArrLength(gtOper);
    }

    bool OperIsImplicitIndir() const;

    static bool OperIsAtomicOp(genTreeOps gtOper)
    {
        switch (gtOper)
        {
            case GT_XADD:
            case GT_XORR:
            case GT_XAND:
            case GT_XCHG:
            case GT_LOCKADD:
            case GT_CMPXCHG:
                return true;
            default:
                return false;
        }
    }

    bool OperIsAtomicOp() const
    {
        return OperIsAtomicOp(gtOper);
    }

    bool OperIsStore() const
    {
        return OperIsStore(gtOper);
    }

    static bool OperIsStore(genTreeOps oper)
    {
        return (oper == GT_STOREIND) || (oper == GT_STORE_LCL_VAR) || (oper == GT_STORE_LCL_FLD) ||
               (oper == GT_STORE_OBJ) || (oper == GT_STORE_BLK) || (oper == GT_COPY_BLK) || (oper == GT_INIT_BLK) ||
               OperIsAtomicOp(oper);
    }

    static bool OperIsHWIntrinsic(genTreeOps gtOper)
    {
#ifdef FEATURE_HW_INTRINSICS
        return gtOper == GT_HWINTRINSIC;
#else
        return false;
#endif // FEATURE_HW_INTRINSICS
    }

    bool OperIsHWIntrinsic() const
    {
        return OperIsHWIntrinsic(gtOper);
    }

    // This is here for cleaner GT_LONG #ifdefs.
    static bool OperIsLong(genTreeOps gtOper)
    {
#if defined(TARGET_64BIT)
        return false;
#else
        return gtOper == GT_LONG;
#endif
    }

    bool OperIsLong() const
    {
        return OperIsLong(gtOper);
    }

    bool OperIsConditionalJump() const
    {
        return (gtOper == GT_JTRUE) || (gtOper == GT_JCMP) || (gtOper == GT_JCC);
    }

    bool IsControlFlow() const
    {
        switch (gtOper)
        {
            case GT_JTRUE:
            case GT_JCMP:
            case GT_JCC:
            case GT_SWITCH:
            case GT_LABEL:
            case GT_CALL:
            case GT_JMP:
            case GT_RETURN:
            case GT_RETFILT:
#ifndef FEATURE_EH_FUNCLETS
            case GT_END_LFIN:
#endif
                return true;
            default:
                return false;
        }
    }

#ifdef DEBUG
    bool NullOp1Legal() const
    {
        assert(OperIsSimple(gtOper));
        switch (gtOper)
        {
            case GT_LEA:
            case GT_RETFILT:
            case GT_NOP:
                return true;
            case GT_RETURN:
                return gtType == TYP_VOID;
            default:
                return false;
        }
    }

    bool NullOp2Legal() const
    {
        assert(OperIsSimple(gtOper));
        if (!OperIsBinary(gtOper))
        {
            return true;
        }
        switch (gtOper)
        {
            case GT_INTRINSIC:
            case GT_LEA:
#if defined(TARGET_ARM)
            case GT_PUTARG_REG:
#endif // defined(TARGET_ARM)
                return true;
            default:
                return false;
        }
    }

    static inline bool RequiresNonNullOp2(genTreeOps oper);
#endif // DEBUG

    bool IsDblConPositiveZero() const;
    bool IsHWIntrinsicZero() const;
    bool IsIntegralConst(ssize_t constVal) const;

    bool IsIntCon(ssize_t value) const;

    inline GenTree* gtGetOp1() const;

    // Directly return op2. Asserts the node is binary. Might return nullptr if the binary node allows
    // a nullptr op2, such as GT_LEA. This is more efficient than gtGetOp2IfPresent() if you know what
    // node type you have.
    inline GenTree* gtGetOp2() const;

    // The returned pointer might be nullptr if the node is not binary, or if non-null op2 is not required.
    inline GenTree* gtGetOp2IfPresent() const;

    // Find the use of a node within this node.
    GenTree** FindUse(GenTree* def);

    bool HasUse(GenTree* def)
    {
        return FindUse(def) != nullptr;
    }

    // Find the user of this node, and optionally capture the use so that it can be modified.
    GenTree* FindUser(GenTree*** use = nullptr);

    void ReplaceOperand(GenTree** useEdge, GenTree* replacement);

    inline GenTree* gtEffectiveVal();

    GenTree* SkipComma();

    // Tunnel through any GT_RET_EXPRs
    inline GenTree* SkipRetExpr();

    // Return the child of this node if it is a GT_RELOAD or GT_COPY; otherwise simply return the node itself
    inline GenTree* gtSkipReloadOrCopy();

    // Returns true if it is a call node returning its value in more than one register
    inline bool IsMultiRegCall() const;

    // Returns true if it is a struct lclVar node residing in multiple registers.
    inline bool IsMultiRegLclVar() const;

    // Returns true if it is a node returning its value in more than one register
    bool IsMultiRegNode() const;

    // Returns the number of registers defined by a multireg node.
    unsigned GetMultiRegCount(Compiler* compiler) const;

    // Returns the type of the regIndex'th register defined by a multi-reg node.
    var_types GetMultiRegType(Compiler* compiler, unsigned regIndex);

    // Last-use information for either GenTreeLclVar or GenTreeCopyOrReload nodes.
    bool IsLastUse(unsigned regIndex);
    bool HasLastUse();
    void SetLastUse(unsigned regIndex, bool lastUse);

    // Returns true if it is a GT_COPY or GT_RELOAD of a multi-reg call node
    inline bool IsCopyOrReloadOfMultiRegCall() const;

    bool OperRequiresAsgFlag() const;

    bool OperRequiresCallFlag(Compiler* comp) const;

    bool OperMayThrow(Compiler* comp) const;

    size_t GetNodeSize() const;

    void ReplaceWith(GenTree* src, Compiler* comp);

    static genTreeOps ReverseRelop(genTreeOps relop);

    static genTreeOps SwapRelop(genTreeOps relop);

    //---------------------------------------------------------------------

    static bool Compare(GenTree* op1, GenTree* op2, bool swapOK = false);

//---------------------------------------------------------------------

#if defined(DEBUG) || NODEBASH_STATS || MEASURE_NODE_SIZE || COUNT_AST_OPERS || DUMP_FLOWGRAPHS
    static const char* OpName(genTreeOps op);
#endif

#if MEASURE_NODE_SIZE
    static const char* OpStructName(genTreeOps op);
#endif

    //---------------------------------------------------------------------

    bool IsNothingNode() const;
    void ChangeToNothingNode();

    enum ValueNumberUpdate
    {
        CLEAR_VN,
        PRESERVE_VN
    };

    void SetOperRaw(genTreeOps oper);
    void SetOper(genTreeOps oper, ValueNumberUpdate vnUpdate = CLEAR_VN);
    void SetOperResetFlags(genTreeOps oper);
    void ChangeOper(genTreeOps oper, ValueNumberUpdate vnUpdate = CLEAR_VN);
    void ChangeOperUnchecked(genTreeOps oper);
    void ChangeOperConst(genTreeOps oper);
    GenTreeIntCon* ChangeToIntCon(ssize_t value);
    GenTreeIntCon* ChangeToIntCon(var_types type, ssize_t value);
#ifndef TARGET_64BIT
    GenTreeLngCon* ChangeToLngCon(int64_t value);
#endif
    GenTreeDblCon* ChangeToDblCon(double value);
    GenTreeDblCon* ChangeToDblCon(var_types type, double value);
    GenTreeFieldList* ChangeToFieldList();
    GenTreeLclFld* ChangeToLclFld(var_types type, unsigned lclNum, unsigned offset, FieldSeqNode* fieldSeq);
    GenTreeLclAddr* ChangeToLclAddr(var_types type, unsigned lclNum);
    GenTreeLclAddr* ChangeToLclAddr(var_types type, unsigned lclNum, unsigned offset, FieldSeqNode* fieldSeq);
    GenTreeAddrMode* ChangeToAddrMode(GenTree* base, GenTree* index, unsigned scale, int offset);

    void ChangeType(var_types newType)
    {
        var_types oldType = gtType;
        gtType            = newType;
        GenTree* node     = this;
        while (node->gtOper == GT_COMMA)
        {
            node = node->gtGetOp2();
            if (node->gtType != newType)
            {
                assert(node->gtType == oldType);
                node->gtType = newType;
            }
        }
    }

#if NODEBASH_STATS
    static void RecordOperBashing(genTreeOps operOld, genTreeOps operNew);
    static void ReportOperBashing(FILE* fp);
#endif

    bool IsPartialLclFld(Compiler* comp);
    GenTreeLclAddr* IsLocalAddrExpr();

    // Determine if this tree represents an indirection for an implict byref parameter,
    // and if so return the tree for the parameter.
    GenTreeLclVar* IsImplicitByrefIndir(Compiler* compiler);

    void SetContained()
    {
        assert(IsValue());
        gtFlags |= GTF_CONTAINED;
        assert(isContained());
    }

    void SetContained(bool contained)
    {
        if (contained)
        {
            SetContained();
        }
        else
        {
            ClearContained();
        }
    }

    void ClearContained()
    {
        assert(IsValue());
        gtFlags &= ~GTF_CONTAINED;
        ClearRegOptional();
    }

    bool CanCSE() const
    {
        return ((gtFlags & GTF_DONT_CSE) == 0);
    }

    void SetDoNotCSE()
    {
        gtFlags |= GTF_DONT_CSE;
    }

    void ClearDoNotCSE()
    {
        gtFlags &= ~GTF_DONT_CSE;
    }

    bool IsReverseOp() const
    {
        return (gtFlags & GTF_REVERSE_OPS) ? true : false;
    }

    void SetReverseOps(bool reverseOps)
    {
        gtFlags = (gtFlags & ~GTF_REVERSE_OPS) | (reverseOps ? GTF_REVERSE_OPS : GTF_EMPTY);
    }

    bool IsUnsigned() const
    {
        return ((gtFlags & GTF_UNSIGNED) != 0);
    }

    void SetUnsigned()
    {
        assert(OperIs(GT_ADD, GT_SUB, GT_MUL, GT_CAST));
        gtFlags |= GTF_UNSIGNED;
    }

    void ClearUnsigned()
    {
        assert(OperIs(GT_ADD, GT_SUB, GT_MUL, GT_CAST));
        gtFlags &= ~GTF_UNSIGNED;
    }

    void SetOverflow()
    {
        assert(OperMayOverflow());
        gtFlags |= GTF_OVERFLOW;
    }

    void ClearOverflow()
    {
        assert(OperMayOverflow());
        gtFlags &= ~GTF_OVERFLOW;
    }

    bool IsCnsIntOrI() const;
    bool IsIntegralConst() const;
    bool IsIntCnsFitsInI32();
    bool IsCnsFltOrDbl() const;
    bool IsCnsNonZeroFltOrDbl();

    bool IsIconHandle() const
    {
        assert(gtOper == GT_CNS_INT);
        return (gtFlags & GTF_ICON_HDL_MASK) != 0;
    }

    bool IsIconHandle(GenTreeFlags handleType) const
    {
        assert(gtOper == GT_CNS_INT);
        assert((handleType & GTF_ICON_HDL_MASK) != 0); // check that handleType is one of the valid GTF_ICON_* values
        assert((handleType & ~GTF_ICON_HDL_MASK) == 0);
        return (gtFlags & GTF_ICON_HDL_MASK) == handleType;
    }

    bool IsHelperCall();

    bool gtOverflow() const;
    bool gtOverflowEx() const;

    bool IsPhiDef() const;

    // Because of the fact that we hid the assignment operator of "BitSet" (in DEBUG),
    // we can't synthesize an assignment operator.
    // TODO-Cleanup: Could change this w/o liveset on tree nodes
    // (This is also necessary for the VTable trick.)
    GenTree()
    {
    }

    // Returns an iterator that will produce the use edge to each operand of this node. Differs
    // from the sequence of nodes produced by a loop over `GetChild` in its handling of call, phi,
    // and block op nodes.
    GenTreeUseEdgeIterator UseEdgesBegin();
    GenTreeUseEdgeIterator UseEdgesEnd();

    IteratorPair<GenTreeUseEdgeIterator> UseEdges();

    // Returns an iterator that will produce each operand of this node. Differs from the sequence
    // of nodes produced by a loop over `GetChild` in its handling of call, phi, and block op
    // nodes.
    GenTreeOperandIterator OperandsBegin();
    GenTreeOperandIterator OperandsEnd();

    // Returns a range that will produce the operands of this node in use order.
    IteratorPair<GenTreeOperandIterator> Operands();

    enum class VisitResult
    {
        Abort    = false,
        Continue = true
    };

    // Visits each operand of this node. The operand must be either a lambda, function, or functor with the signature
    // `GenTree::VisitResult VisitorFunction(GenTree* operand)`. Here is a simple example:
    //
    //     unsigned operandCount = 0;
    //     node->VisitOperands([&](GenTree* operand) -> GenTree::VisitResult)
    //     {
    //         operandCount++;
    //         return GenTree::VisitResult::Continue;
    //     });
    //
    // This function is generally more efficient that the operand iterator and should be preferred over that API for
    // hot code, as it affords better opportunities for inlining and acheives shorter dynamic path lengths when
    // deciding how operands need to be accessed.
    //
    // Note that this function does not respect `GTF_REVERSE_OPS` and `gtEvalSizeFirst`. This is always safe in LIR,
    // but may be dangerous in HIR if for some reason you need to visit operands in the order in which they will
    // execute.
    template <typename TVisitor>
    void VisitOperands(TVisitor visitor);

private:
    template <typename TVisitor>
    void VisitBinOpOperands(TVisitor visitor);

public:
    bool Precedes(GenTree* other);

    bool IsReuseRegValCandidate()
    {
        return OperIsConst() || IsHWIntrinsicZero();
    }

    bool IsReuseRegVal()
    {
        // This can be extended to non-constant nodes, but not to local or indir nodes.
        return ((gtFlags & GTF_REUSE_REG_VAL) != 0) && IsReuseRegValCandidate();
    }

    void SetReuseRegVal()
    {
        assert(IsReuseRegValCandidate());
        gtFlags |= GTF_REUSE_REG_VAL;
    }

    void ResetReuseRegVal()
    {
        assert(IsReuseRegValCandidate());
        gtFlags &= ~GTF_REUSE_REG_VAL;
    }

    void SetIndirExceptionFlags(Compiler* comp);

#if MEASURE_NODE_SIZE
    static void DumpNodeSizes(FILE* fp);
#endif

#ifdef DEBUG
    unsigned GetID() const
    {
        return gtTreeID;
    }

private:
    GenTree& operator=(const GenTree& gt)
    {
        assert(!"Don't copy");
        return *this;
    }
#endif // DEBUG

#if DEBUGGABLE_GENTREE
    // In DEBUG builds, add a dummy virtual method, to give the debugger run-time type information.
    virtual void DummyVirt()
    {
    }

    typedef void* VtablePtr;

    VtablePtr GetVtableForOper(genTreeOps oper);
    void SetVtableForOper(genTreeOps oper);

    static VtablePtr s_vtablesForOpers[GT_COUNT];
    static VtablePtr s_vtableForOp;
#endif // DEBUGGABLE_GENTREE

public:
    inline void* operator new(size_t sz, class Compiler*, genTreeOps oper);

    inline GenTree(genTreeOps oper, var_types type DEBUGARG(bool largeNode = false));

    GenTree(const GenTree* copyFrom) : GenTree(copyFrom->gtOper, copyFrom->gtType)
    {
    }
};

// Represents a list of fields constituting a struct, when it is passed as an argument.
//
struct GenTreeFieldList : public GenTree
{
    class Use
    {
        GenTree*  m_node;
        Use*      m_next;
        uint16_t  m_offset;
        var_types m_type;

    public:
        Use(GenTree* node, unsigned offset, var_types type)
            : m_node(node), m_next(nullptr), m_offset(static_cast<uint16_t>(offset)), m_type(type)
        {
            // We can save space on 32 bit hosts by storing the offset as uint16_t. Struct promotion
            // only accepts structs which are much smaller than that - 128 bytes = max 4 fields * max
            // SIMD vector size (32 bytes).
            assert(offset <= UINT16_MAX);
        }

        GenTree*& NodeRef()
        {
            return m_node;
        }

        GenTree* GetNode() const
        {
            return m_node;
        }

        void SetNode(GenTree* node)
        {
            assert(node != nullptr);
            m_node = node;
        }

        Use*& NextRef()
        {
            return m_next;
        }

        Use* GetNext() const
        {
            return m_next;
        }

        void SetNext(Use* next)
        {
            m_next = next;
        }

        unsigned GetOffset() const
        {
            return m_offset;
        }

        var_types GetType() const
        {
            return m_type;
        }

        void SetType(var_types type)
        {
            m_type = type;
        }
    };

    class UseIterator
    {
        Use* use;

    public:
        UseIterator(Use* use) : use(use)
        {
        }

        Use& operator*()
        {
            return *use;
        }

        Use* operator->()
        {
            return use;
        }

        void operator++()
        {
            use = use->GetNext();
        }

        bool operator==(const UseIterator& other)
        {
            return use == other.use;
        }

        bool operator!=(const UseIterator& other)
        {
            return use != other.use;
        }
    };

    class UseList
    {
        Use* m_head;
        Use* m_tail;

    public:
        UseList() : m_head(nullptr), m_tail(nullptr)
        {
        }

        Use* GetHead() const
        {
            return m_head;
        }

        UseIterator begin() const
        {
            return m_head;
        }

        UseIterator end() const
        {
            return nullptr;
        }

        void AddUse(Use* newUse)
        {
            assert(newUse->GetNext() == nullptr);

            if (m_head == nullptr)
            {
                m_head = newUse;
            }
            else
            {
                m_tail->SetNext(newUse);
            }

            m_tail = newUse;
        }

        void InsertUse(Use* insertAfter, Use* newUse)
        {
            assert(newUse->GetNext() == nullptr);

            newUse->SetNext(insertAfter->GetNext());
            insertAfter->SetNext(newUse);

            if (m_tail == insertAfter)
            {
                m_tail = newUse;
            }
        }

        void Reverse()
        {
            m_tail = m_head;
            m_head = nullptr;

            for (Use *next, *use = m_tail; use != nullptr; use = next)
            {
                next = use->GetNext();
                use->SetNext(m_head);
                m_head = use;
            }
        }

        bool IsSorted() const
        {
            unsigned offset = 0;
            for (GenTreeFieldList::Use& use : *this)
            {
                if (use.GetOffset() < offset)
                {
                    return false;
                }
                offset = use.GetOffset();
            }
            return true;
        }
    };

private:
    UseList m_uses;

public:
    GenTreeFieldList() : GenTree(GT_FIELD_LIST, TYP_STRUCT)
    {
        SetContained();
    }

    UseList& Uses()
    {
        return m_uses;
    }

    // Add a new field use to the end of the use list and update side effect flags.
    void AddField(Compiler* compiler, GenTree* node, unsigned offset, var_types type);
    // Add a new field use to the end of the use list without updating side effect flags.
    void AddFieldLIR(Compiler* compiler, GenTree* node, unsigned offset, var_types type);
    // Insert a new field use after the specified use and update side effect flags.
    void InsertField(Compiler* compiler, Use* insertAfter, GenTree* node, unsigned offset, var_types type);
    // Insert a new field use after the specified use without updating side effect flags.
    void InsertFieldLIR(Compiler* compiler, Use* insertAfter, GenTree* node, unsigned offset, var_types type);

    void ClearFields()
    {
        m_uses = UseList();
    }

    //--------------------------------------------------------------------------
    // Equals: Check if 2 FIELD_LIST nodes are equal.
    //
    // Arguments:
    //    list1 - The first FIELD_LIST node
    //    list2 - The second FIELD_LIST node
    //
    // Return Value:
    //    true if the 2 FIELD_LIST nodes have the same type, number of uses, and the
    //    uses are equal.
    //
    static bool Equals(GenTreeFieldList* list1, GenTreeFieldList* list2)
    {
        assert(list1->TypeGet() == TYP_STRUCT);
        assert(list2->TypeGet() == TYP_STRUCT);

        UseIterator i1   = list1->Uses().begin();
        UseIterator end1 = list1->Uses().end();
        UseIterator i2   = list2->Uses().begin();
        UseIterator end2 = list2->Uses().end();

        for (; (i1 != end1) && (i2 != end2); ++i1, ++i2)
        {
            if (!Compare(i1->GetNode(), i2->GetNode()) || (i1->GetOffset() != i2->GetOffset()) ||
                (i1->GetType() != i2->GetType()))
            {
                return false;
            }
        }

        return (i1 == end1) && (i2 == end2);
    }
};

//------------------------------------------------------------------------
// GenTreeUseEdgeIterator: an iterator that will produce each use edge of a GenTree node in the order in which
//                         they are used.
//
// Operand iteration is common enough in the back end of the compiler that the implementation of this type has
// traded some simplicity for speed:
// - As much work as is reasonable is done in the constructor rather than during operand iteration
// - Node-specific functionality is handled by a small class of "advance" functions called by operator++
//   rather than making operator++ itself handle all nodes
// - Some specialization has been performed for specific node types/shapes (e.g. the advance function for
//   binary nodes is specialized based on whether or not the node has the GTF_REVERSE_OPS flag set)
//
// Valid values of this type may be obtained by calling `GenTree::UseEdgesBegin` and `GenTree::UseEdgesEnd`.
//
class GenTreeUseEdgeIterator final
{
    friend class GenTreeOperandIterator;
    friend GenTreeUseEdgeIterator GenTree::UseEdgesBegin();
    friend GenTreeUseEdgeIterator GenTree::UseEdgesEnd();

    enum
    {
        CALL_INSTANCE     = 0,
        CALL_ARGS         = 1,
        CALL_LATE_ARGS    = 2,
        CALL_CONTROL_EXPR = 3,
        CALL_COOKIE       = 4,
        CALL_ADDRESS      = 5,
        CALL_TERMINAL     = 6,
    };

    typedef void (GenTreeUseEdgeIterator::*AdvanceFn)();

    AdvanceFn m_advance;
    GenTree*  m_node;
    GenTree** m_edge;
    // Pointer sized state storage, GenTreeArgList* or GenTreePhi::Use* or GenTreeCall::Use* currently.
    void* m_statePtr;
    // Integer sized state storage, usually the operand index for non-list based nodes.
    int m_state;

    GenTreeUseEdgeIterator(GenTree* node);

    // Advance functions for special nodes
    void AdvanceTernaryOp();
    void AdvanceArrElem();
    void AdvanceFieldList();
    void AdvancePhi();
#ifdef FEATURE_HW_INTRINSICS
    void AdvanceHWIntrinsic();
    void AdvanceHWIntrinsicReverseOp();
#endif
    void AdvanceInstr();

    template <bool ReverseOperands>
    void           AdvanceBinOp();
    void           SetEntryStateForBinOp();

    // The advance function for call nodes
    template <int state>
    void          AdvanceCall();

    void Terminate();

public:
    GenTreeUseEdgeIterator();

    inline GenTree** operator*()
    {
        assert(m_state != -1);
        return m_edge;
    }

    inline GenTree** operator->()
    {
        assert(m_state != -1);
        return m_edge;
    }

    inline bool operator==(const GenTreeUseEdgeIterator& other) const
    {
        if (m_state == -1 || other.m_state == -1)
        {
            return m_state == other.m_state;
        }

        return (m_node == other.m_node) && (m_edge == other.m_edge) && (m_statePtr == other.m_statePtr) &&
               (m_state == other.m_state);
    }

    inline bool operator!=(const GenTreeUseEdgeIterator& other) const
    {
        return !(operator==(other));
    }

    GenTreeUseEdgeIterator& operator++();
};

//------------------------------------------------------------------------
// GenTreeOperandIterator: an iterator that will produce each operand of a
//                         GenTree node in the order in which they are
//                         used. This uses `GenTreeUseEdgeIterator` under
//                         the covers and comes with the same caveats
//                         w.r.t. `GetChild`.
//
// Note: valid values of this type may be obtained by calling
// `GenTree::OperandsBegin` and `GenTree::OperandsEnd`.
class GenTreeOperandIterator final
{
    friend GenTreeOperandIterator GenTree::OperandsBegin();
    friend GenTreeOperandIterator GenTree::OperandsEnd();

    GenTreeUseEdgeIterator m_useEdges;

    GenTreeOperandIterator(GenTree* node) : m_useEdges(node)
    {
    }

public:
    GenTreeOperandIterator() : m_useEdges()
    {
    }

    inline GenTree* operator*()
    {
        return *(*m_useEdges);
    }

    inline GenTree* operator->()
    {
        return *(*m_useEdges);
    }

    inline bool operator==(const GenTreeOperandIterator& other) const
    {
        return m_useEdges == other.m_useEdges;
    }

    inline bool operator!=(const GenTreeOperandIterator& other) const
    {
        return !(operator==(other));
    }

    inline GenTreeOperandIterator& operator++()
    {
        ++m_useEdges;
        return *this;
    }
};

/*****************************************************************************/
// In the current design, we never instantiate GenTreeUnOp: it exists only to be
// used as a base class.  For unary operators, we instantiate GenTreeOp, with a NULL second
// argument.  We check that this is true dynamically.  We could tighten this and get static
// checking, but that would entail accessing the first child of a unary operator via something
// like gtUnOp.gtOp1 instead of AsOp()->gtOp1.
struct GenTreeUnOp : public GenTree
{
    GenTree* gtOp1;

protected:
    GenTreeUnOp(genTreeOps oper, var_types type DEBUGARG(bool largeNode = false))
        : GenTree(oper, type DEBUGARG(largeNode)), gtOp1(nullptr)
    {
    }

    GenTreeUnOp(genTreeOps oper, var_types type, GenTree* op1 DEBUGARG(bool largeNode = false))
        : GenTree(oper, type DEBUGARG(largeNode)), gtOp1(op1)
    {
        assert(op1 != nullptr || NullOp1Legal());
        if (op1 != nullptr)
        { // Propagate effects flags from child.
            gtFlags |= op1->gtFlags & GTF_ALL_EFFECT;
        }
    }

    GenTreeUnOp(const GenTreeUnOp* copyFrom) : GenTree(copyFrom), gtOp1(copyFrom->gtOp1)
    {
    }

public:
    GenTree* GetOp(unsigned index) const
    {
        switch (index)
        {
            case 0:
                assert(gtOp1 != nullptr);
                return gtOp1;
            default:
                unreached();
        }
    }

    void SetOp(unsigned index, GenTree* op)
    {
        assert(op != nullptr);

        switch (index)
        {
            case 0:
                gtOp1 = op;
                return;
            default:
                unreached();
        }
    }

#if DEBUGGABLE_GENTREE
    GenTreeUnOp() : GenTree(), gtOp1(nullptr)
    {
    }
#endif
};

struct GenTreeOp : public GenTreeUnOp
{
    GenTree* gtOp2;

    GenTreeOp(genTreeOps oper, var_types type, GenTree* op1, GenTree* op2 DEBUGARG(bool largeNode = false))
        : GenTreeUnOp(oper, type, op1 DEBUGARG(largeNode)), gtOp2(op2)
    {
        // comparisons are always integral types
        assert(!GenTree::OperIsCompare(oper) || varTypeIsIntegral(type));
        // Binary operators, with a few exceptions, require a non-nullptr
        // second argument.
        assert(op2 != nullptr || NullOp2Legal());
        // Unary operators, on the other hand, require a null second argument.
        assert(!OperIsUnary(oper) || op2 == nullptr);
        // Propagate effects flags from child.  (UnOp handled this for first child.)
        if (op2 != nullptr)
        {
            gtFlags |= op2->gtFlags & GTF_ALL_EFFECT;
        }
    }

    // A small set of types are unary operators with optional arguments.  We use
    // this constructor to build those.
    GenTreeOp(genTreeOps oper, var_types type DEBUGARG(bool largeNode = false))
        : GenTreeUnOp(oper, type DEBUGARG(largeNode)), gtOp2(nullptr)
    {
        // Unary operators with optional arguments:
        assert((oper == GT_NOP) || (oper == GT_RETURN) || (oper == GT_RETFILT));
    }

    GenTreeOp(const GenTreeOp* copyFrom) : GenTreeUnOp(copyFrom), gtOp2(copyFrom->gtOp2)
    {
    }

    GenTree* GetOp(unsigned index) const
    {
        switch (index)
        {
            case 0:
                assert(gtOp1 != nullptr);
                return gtOp1;
            case 1:
                assert(gtOp2 != nullptr);
                return gtOp2;
            default:
                unreached();
        }
    }

    void SetOp(unsigned index, GenTree* op)
    {
        assert(op != nullptr);

        switch (index)
        {
            case 0:
                gtOp1 = op;
                return;
            case 1:
                gtOp2 = op;
                return;
            default:
                unreached();
        }
    }

    // returns true if we will use the division by constant optimization for this node.
    bool UsesDivideByConstOptimized(Compiler* comp);

    // checks if we will use the division by constant optimization this node
    // then sets the flag GTF_DIV_BY_CNS_OPT and GTF_DONT_CSE on the constant
    void CheckDivideByConstOptimized(Compiler* comp);

    // True if this node is marked as using the division by constant optimization
    bool MarkedDivideByConstOptimized() const
    {
        return (gtFlags & GTF_DIV_BY_CNS_OPT) != 0;
    }

#if DEBUGGABLE_GENTREE
    GenTreeOp() : GenTreeUnOp(), gtOp2(nullptr)
    {
    }
#endif
};

struct GenTreeVal : public GenTree
{
    size_t gtVal1;

    GenTreeVal(genTreeOps oper, var_types type, ssize_t val) : GenTree(oper, type), gtVal1(val)
    {
    }
#if DEBUGGABLE_GENTREE
    GenTreeVal() : GenTree()
    {
    }
#endif
};

struct GenTreeIntConCommon : public GenTree
{
    inline INT64 LngValue() const;
    inline void SetLngValue(INT64 val);
    inline ssize_t IconValue() const;
    inline void SetIconValue(ssize_t val);
    inline INT64 IntegralValue() const;

    GenTreeIntConCommon(genTreeOps oper, var_types type DEBUGARG(bool largeNode = false))
        : GenTree(oper, type DEBUGARG(largeNode))
    {
    }

    bool FitsInI8() // IconValue() fits into 8-bit signed storage
    {
        return FitsInI8(IconValue());
    }

    static bool FitsInI8(ssize_t val) // Constant fits into 8-bit signed storage
    {
        return (int8_t)val == val;
    }

    bool FitsInI32() // IconValue() fits into 32-bit signed storage
    {
        return FitsInI32(IconValue());
    }

    static bool FitsInI32(ssize_t val) // Constant fits into 32-bit signed storage
    {
#ifdef TARGET_64BIT
        return (int32_t)val == val;
#else
        return true;
#endif
    }

    bool ImmedValCanBeFolded(Compiler* comp, genTreeOps op);

#if DEBUGGABLE_GENTREE
    GenTreeIntConCommon() : GenTree()
    {
    }
#endif
};

// node representing a read from a physical register
struct GenTreePhysReg : public GenTree
{
    // physregs need a field beyond GetRegNum() because
    // GetRegNum() indicates the destination (and can be changed)
    // whereas reg indicates the source
    regNumber gtSrcReg;
    GenTreePhysReg(regNumber r, var_types type = TYP_I_IMPL) : GenTree(GT_PHYSREG, type), gtSrcReg(r)
    {
    }
#if DEBUGGABLE_GENTREE
    GenTreePhysReg() : GenTree()
    {
    }
#endif
};

const char* dmpGetHandleKindName(GenTreeFlags flags);

/* gtIntCon -- integer constant (GT_CNS_INT) */
struct GenTreeIntCon : public GenTreeIntConCommon
{
    /*
     * This is the GT_CNS_INT struct definition.
     * It's used to hold for both int constants and pointer handle constants.
     * For the 64-bit targets we will only use GT_CNS_INT as it used to represent all the possible sizes
     * For the 32-bit targets we use a GT_CNS_LNG to hold a 64-bit integer constant and GT_CNS_INT for all others.
     * In the future when we retarget the JIT for x86 we should consider eliminating GT_CNS_LNG
     */
    ssize_t gtIconVal; // Must overlap and have the same offset with the gtIconVal field in GenTreeLngCon below.

    /* The InitializeArray intrinsic needs to go back to the newarray statement
       to find the class handle of the array so that we can get its size.  However,
       in ngen mode, the handle in that statement does not correspond to the compile
       time handle (rather it lets you get a handle at run-time).  In that case, we also
       need to store a compile time handle, which goes in this gtCompileTimeHandle field.
    */
    ssize_t gtCompileTimeHandle;

    // If this constant represents the offset of one or more fields, "m_fieldSeq" represents that
    // sequence of fields.
    FieldSeqNode* m_fieldSeq;

    // If the value represents target address, holds the method handle to that target which is used
    // to fetch target method name and display in the disassembled code.
    INDEBUG(size_t gtTargetHandle = 0;)

    GenTreeIntCon(var_types type, ssize_t value)
        : GenTreeIntConCommon(GT_CNS_INT, type)
        , gtIconVal(value)
        , gtCompileTimeHandle(0)
        , m_fieldSeq(FieldSeqStore::NotAField())
    {
    }

    GenTreeIntCon(var_types type, ssize_t value, FieldSeqNode* fieldSeq)
        : GenTreeIntConCommon(GT_CNS_INT, type), gtIconVal(value), gtCompileTimeHandle(0), m_fieldSeq(fieldSeq)
    {
        assert(fieldSeq != nullptr);
    }

    GenTreeIntCon(const GenTreeIntCon* copyFrom)
        : GenTreeIntConCommon(GT_CNS_INT, copyFrom->GetType())
        , gtIconVal(copyFrom->gtIconVal)
        , gtCompileTimeHandle(copyFrom->gtCompileTimeHandle)
        , m_fieldSeq(copyFrom->m_fieldSeq)
#ifdef DEBUG
        , gtTargetHandle(copyFrom->gtTargetHandle)
#endif
    {
    }

    ssize_t GetValue() const
    {
        return gtIconVal;
    }

    ssize_t GetValue(var_types type) const
    {
        switch (type)
        {
            case TYP_BYTE:
                return static_cast<int8_t>(gtIconVal);
            case TYP_UBYTE:
            case TYP_BOOL:
                return static_cast<uint8_t>(gtIconVal);
            case TYP_SHORT:
                return static_cast<int16_t>(gtIconVal);
            case TYP_USHORT:
                return static_cast<uint16_t>(gtIconVal);
#ifdef TARGET_64BIT
            case TYP_INT:
                return static_cast<int32_t>(gtIconVal);
            case TYP_UINT:
                // Disallow UINT for now as it's not needed and it's not clear if we should
                // sign extend or zero extend. Sign extend seems to make more sense - if we
                // actually cast this value to UINT using a CAST then the result would be a
                // 32 bit value that's considered to have type INT and if we store that in
                // in an IntCon we'd have to sign extend.
                assert(false);
                return static_cast<int32_t>(gtIconVal);
#endif
            default:
                assert(varTypeIsI(type) || (varTypeIsStruct(type) && (gtIconVal == 0)));
                return gtIconVal;
        }
    }

    size_t GetUnsignedValue() const
    {
        return static_cast<size_t>(gtIconVal);
    }

    uint8_t GetUInt8Value() const
    {
        return static_cast<uint8_t>(gtIconVal & 0xFF);
    }

    uint16_t GetUInt16Value() const
    {
        return static_cast<uint16_t>(gtIconVal);
    }

    uint32_t GetUInt32Value() const
    {
        return static_cast<uint32_t>(gtIconVal);
    }

    int32_t GetInt32Value() const
    {
        return static_cast<int32_t>(gtIconVal);
    }

#ifdef TARGET_64BIT
    int64_t GetInt64Value() const
    {
        return gtIconVal;
    }

    uint64_t GetUInt64Value() const
    {
        return static_cast<uint64_t>(gtIconVal);
    }
#endif

    void SetValue(ssize_t value)
    {
        gtIconVal  = value;
        m_fieldSeq = FieldSeqStore::NotAField();
    }

    void SetValue(var_types type, ssize_t value)
    {
#ifdef TARGET_64BIT
        if (type == TYP_INT)
        {
            value = static_cast<int32_t>(value);
        }
#endif

        gtType     = type;
        gtIconVal  = value;
        m_fieldSeq = FieldSeqStore::NotAField();
    }

    void SetValue(unsigned offset, FieldSeqNode* fieldSeq)
    {
        gtIconVal  = offset;
        m_fieldSeq = fieldSeq;
    }

    FieldSeqNode* GetFieldSeq() const
    {
        return m_fieldSeq;
    }

    void SetFieldSeq(FieldSeqNode* fieldSeq)
    {
        m_fieldSeq = fieldSeq;
    }

    bool IsHandle() const
    {
        return (gtFlags & GTF_ICON_HDL_MASK) != 0;
    }

    GenTreeFlags GetHandleKind() const
    {
        return gtFlags & GTF_ICON_HDL_MASK;
    }

    void SetHandleKind(GenTreeFlags kind)
    {
        assert((kind & ~GTF_ICON_HDL_MASK) == 0);

        gtFlags = (gtFlags & ~GTF_ICON_HDL_MASK) | (kind & GTF_ICON_HDL_MASK);
    }

    bool ImmedValNeedsReloc(Compiler* comp);

#ifdef TARGET_XARCH
    bool AddrNeedsReloc(Compiler* comp);
    bool FitsInAddrBase(Compiler* comp);
#endif

#ifdef TARGET_64BIT
    void TruncateOrSignExtend32()
    {
        gtIconVal = INT32(gtIconVal);
    }
#endif // TARGET_64BIT

#if DEBUGGABLE_GENTREE
    GenTreeIntCon() : GenTreeIntConCommon()
    {
    }
#endif
};

/* gtLngCon -- long    constant (GT_CNS_LNG) */

struct GenTreeLngCon : public GenTreeIntConCommon
{
    int64_t gtLconVal; // Must overlap and have the same offset with the gtIconVal field in GenTreeIntCon above.

    int32_t LoVal() const
    {
        return static_cast<int32_t>(gtLconVal & 0xffffffff);
    }

    int32_t HiVal() const
    {
        return static_cast<int32_t>(gtLconVal >> 32);
    }

    GenTreeLngCon(int64_t val) : GenTreeIntConCommon(GT_CNS_NATIVELONG, TYP_LONG)
    {
        SetLngValue(val);
    }

    int64_t GetValue() const
    {
        return gtLconVal;
    }

    uint64_t GetUInt64Value() const
    {
        return static_cast<uint64_t>(gtLconVal);
    }

    void SetValue(int64_t value)
    {
        gtLconVal = value;
    }

#if DEBUGGABLE_GENTREE
    GenTreeLngCon() : GenTreeIntConCommon()
    {
    }
#endif
};

inline INT64 GenTreeIntConCommon::LngValue() const
{
#ifndef TARGET_64BIT
    assert(gtOper == GT_CNS_LNG);
    return AsLngCon()->gtLconVal;
#else
    return IconValue();
#endif
}

inline void GenTreeIntConCommon::SetLngValue(INT64 val)
{
#ifndef TARGET_64BIT
    assert(gtOper == GT_CNS_LNG);
    AsLngCon()->gtLconVal = val;
#else
    // Compile time asserts that these two fields overlap and have the same offsets:  gtIconVal and gtLconVal
    C_ASSERT(offsetof(GenTreeLngCon, gtLconVal) == offsetof(GenTreeIntCon, gtIconVal));
    C_ASSERT(sizeof(AsLngCon()->gtLconVal) == sizeof(AsIntCon()->gtIconVal));

    SetIconValue(ssize_t(val));
#endif
}

inline ssize_t GenTreeIntConCommon::IconValue() const
{
    assert(gtOper == GT_CNS_INT); //  We should never see a GT_CNS_LNG for a 64-bit target!
    return AsIntCon()->gtIconVal;
}

inline void GenTreeIntConCommon::SetIconValue(ssize_t val)
{
    assert(gtOper == GT_CNS_INT); //  We should never see a GT_CNS_LNG for a 64-bit target!
    AsIntCon()->gtIconVal = val;
}

inline INT64 GenTreeIntConCommon::IntegralValue() const
{
#ifdef TARGET_64BIT
    return LngValue();
#else
    return gtOper == GT_CNS_LNG ? LngValue() : (INT64)IconValue();
#endif // TARGET_64BIT
}

/* gtDblCon -- double  constant (GT_CNS_DBL) */

struct GenTreeDblCon : public GenTree
{
    double gtDconVal;

    GenTreeDblCon(double val, var_types type = TYP_DOUBLE) : GenTree(GT_CNS_DBL, type), gtDconVal(val)
    {
        assert(varTypeIsFloating(type));
    }

    double GetValue() const
    {
        return gtDconVal;
    }

    double GetDoubleValue() const
    {
        assert(gtType == TYP_DOUBLE);
        return gtDconVal;
    }

    float GetFloatValue() const
    {
        assert(gtType == TYP_FLOAT);
        return static_cast<float>(gtDconVal);
    }

    uint64_t GetBits() const
    {
        return jitstd::bit_cast<uint64_t>(gtDconVal);
    }

    uint64_t GetDoubleBits() const
    {
        assert(gtType == TYP_DOUBLE);
        return jitstd::bit_cast<uint64_t>(gtDconVal);
    }

    uint32_t GetFloatBits() const
    {
        assert(gtType == TYP_FLOAT);
        return jitstd::bit_cast<uint32_t>(static_cast<float>(gtDconVal));
    }

    void SetValue(double value)
    {
        gtDconVal = value;
    }

    bool IsPositiveZero() const
    {
        return GetBits() == 0;
    }

#if DEBUGGABLE_GENTREE
    GenTreeDblCon() = default;
#endif
};

struct GenTreeStrCon : public GenTree
{
    unsigned              gtSconCPX;
    CORINFO_MODULE_HANDLE gtScpHnd;

    // Because this node can come from an inlined method we need to
    // have the scope handle, since it will become a helper call.
    GenTreeStrCon(unsigned sconCPX, CORINFO_MODULE_HANDLE mod DEBUGARG(bool largeNode = false))
        : GenTree(GT_CNS_STR, TYP_REF DEBUGARG(largeNode)), gtSconCPX(sconCPX), gtScpHnd(mod)
    {
    }

#if DEBUGGABLE_GENTREE
    GenTreeStrCon() = default;
#endif
};

// Common supertype of LCL_VAR, LCL_FLD, REG_VAR, PHI_ARG
// This inherits from UnOp because lclvar stores are Unops
struct GenTreeLclVarCommon : public GenTreeUnOp
{
private:
    unsigned m_lclNum;
    unsigned m_unused; // to preserve TREE_NODE_SZ_SMALL, which is sizeof(GenTreeLclFld)

protected:
    GenTreeLclVarCommon(const GenTreeLclVarCommon* copyFrom)
        : GenTreeUnOp(copyFrom->GetOper(), copyFrom->GetType()), m_lclNum(copyFrom->m_lclNum)
    {
    }

public:
    GenTreeLclVarCommon(genTreeOps oper, var_types type, unsigned lclNum DEBUGARG(bool largeNode = false))
        : GenTreeUnOp(oper, type DEBUGARG(largeNode))
    {
        SetLclNum(lclNum);
    }

    unsigned GetLclNum() const
    {
        return m_lclNum;
    }

    void SetLclNum(unsigned lclNum)
    {
        assert(lclNum != BAD_VAR_NUM);

        m_lclNum = lclNum;
    }

    uint16_t GetLclOffs() const;

#if DEBUGGABLE_GENTREE
    GenTreeLclVarCommon() = default;
#endif
};

// GenTreeLclVar - load/store of local variable
struct GenTreeLclVar : public GenTreeLclVarCommon
{
    bool IsMultiReg() const
    {
        return ((gtFlags & GTF_VAR_MULTIREG) != 0);
    }
    void ClearMultiReg()
    {
        gtFlags &= ~GTF_VAR_MULTIREG;
    }
    void SetMultiReg()
    {
        assert(OperIs(GT_STORE_LCL_VAR));
        gtFlags |= GTF_VAR_MULTIREG;
    }

    unsigned GetMultiRegCount(Compiler* compiler) const;
    var_types GetMultiRegType(Compiler* compiler, unsigned regIndex);

    GenTreeLclVar(var_types type, unsigned lclNum DEBUGARG(bool largeNode = false))
        : GenTreeLclVarCommon(GT_LCL_VAR, type, lclNum DEBUGARG(largeNode))
    {
    }

    GenTreeLclVar(var_types type, unsigned lclNum, GenTree* value DEBUGARG(bool largeNode = false))
        : GenTreeLclVarCommon(GT_STORE_LCL_VAR, type, lclNum DEBUGARG(largeNode))
    {
        gtFlags |= GTF_ASG | value->GetSideEffects();
        SetOp(0, value);
    }

    GenTreeLclVar(GenTreeLclVar* copyFrom) : GenTreeLclVarCommon(copyFrom)
    {
    }

#if DEBUGGABLE_GENTREE
    GenTreeLclVar() = default;
#endif
};

// GenTreeLclFld - load/store of local variable field
struct GenTreeLclFld : public GenTreeLclVarCommon
{
private:
    uint16_t      m_lclOffs;   // offset into the variable to access
    uint16_t      m_layoutNum; // the class layout number for struct typed nodes
    FieldSeqNode* m_fieldSeq;  // This LclFld node represents some sequences of accesses.

public:
    GenTreeLclFld(var_types type, unsigned lclNum, unsigned lclOffs)
        : GenTreeLclVarCommon(GT_LCL_FLD, type, lclNum)
        , m_lclOffs(static_cast<uint16_t>(lclOffs))
        , m_layoutNum(0)
        , m_fieldSeq(FieldSeqStore::NotAField())
    {
        assert(lclOffs <= UINT16_MAX);
    }

    GenTreeLclFld(var_types type, unsigned lclNum, unsigned lclOffs, GenTree* value)
        : GenTreeLclVarCommon(GT_STORE_LCL_FLD, type, lclNum)
        , m_lclOffs(static_cast<uint16_t>(lclOffs))
        , m_layoutNum(0)
        , m_fieldSeq(FieldSeqStore::NotAField())
    {
        assert(lclOffs <= UINT16_MAX);

        gtFlags |= GTF_ASG | value->GetSideEffects();
        SetOp(0, value);
    }

    GenTreeLclFld(const GenTreeLclFld* copyFrom)
        : GenTreeLclVarCommon(copyFrom)
        , m_lclOffs(copyFrom->m_lclOffs)
        , m_layoutNum(copyFrom->m_layoutNum)
        , m_fieldSeq(copyFrom->m_fieldSeq)
    {
    }

    uint16_t GetLclOffs() const
    {
        return m_lclOffs;
    }

    void SetLclOffs(unsigned lclOffs)
    {
        assert(lclOffs <= UINT16_MAX);
        m_lclOffs = static_cast<uint16_t>(lclOffs);
    }

    uint16_t GetLayoutNum() const
    {
        return varTypeIsStruct(GetType()) ? m_layoutNum : 0;
    }

    void SetLayoutNum(unsigned layoutNum)
    {
        assert(layoutNum <= UINT16_MAX);
        assert((layoutNum == 0) || varTypeIsStruct(GetType()));
        m_layoutNum = static_cast<uint16_t>(layoutNum);
    }

    ClassLayout* GetLayout(Compiler* compiler) const;
    void SetLayout(ClassLayout* layout, Compiler* compiler);

    FieldSeqNode* GetFieldSeq() const
    {
        return m_fieldSeq;
    }

    bool HasFieldSeq() const
    {
        return (m_fieldSeq != nullptr) && (m_fieldSeq != FieldSeqNode::NotAField());
    }

    void SetFieldSeq(FieldSeqNode* fieldSeq)
    {
        assert((fieldSeq == nullptr) || !fieldSeq->IsArrayElement());
        m_fieldSeq = fieldSeq;
    }

    static bool Equals(GenTreeLclFld* f1, GenTreeLclFld* f2)
    {
        assert((f1->OperGet() == f2->OperGet()) && (f1->GetType() == f2->GetType()));
        return (f1->GetLclNum() == f2->GetLclNum()) && (f1->m_lclOffs == f2->m_lclOffs) &&
               ((f1->m_layoutNum == f2->m_layoutNum) || !varTypeIsStruct(f1->GetType()));
    }

#ifdef TARGET_ARM
    bool IsOffsetMisaligned() const;
#endif

#if DEBUGGABLE_GENTREE
    GenTreeLclFld() = default;
#endif
};

struct GenTreeLclAddr : public GenTreeLclVarCommon
{
private:
    uint16_t      m_lclOffs;
    FieldSeqNode* m_fieldSeq;

public:
    GenTreeLclAddr(var_types type, unsigned lclNum, unsigned lclOffs)
        : GenTreeLclVarCommon(GT_LCL_ADDR, type, lclNum), m_lclOffs(static_cast<uint16_t>(lclOffs)), m_fieldSeq(nullptr)
    {
        assert(lclOffs <= UINT16_MAX);
    }

    GenTreeLclAddr(const GenTreeLclAddr* copyFrom)
        : GenTreeLclVarCommon(copyFrom), m_lclOffs(copyFrom->m_lclOffs), m_fieldSeq(copyFrom->m_fieldSeq)
    {
    }

    uint16_t GetLclOffs() const
    {
        return m_lclOffs;
    }

    void SetLclOffs(unsigned lclOffs)
    {
        assert(lclOffs <= UINT16_MAX);
        m_lclOffs = static_cast<uint16_t>(lclOffs);
    }

    FieldSeqNode* GetFieldSeq() const
    {
        return m_fieldSeq;
    }

    bool HasFieldSeq() const
    {
        return (m_fieldSeq != nullptr) && (m_fieldSeq != FieldSeqNode::NotAField());
    }

    void SetFieldSeq(FieldSeqNode* fieldSeq)
    {
        assert((fieldSeq == nullptr) || !fieldSeq->IsArrayElement());
        m_fieldSeq = fieldSeq;
    }

    static bool Equals(GenTreeLclAddr* a1, GenTreeLclAddr* a2)
    {
        assert((a1->GetOper() == a2->GetOper()) && (a1->GetType() == a2->GetType()));
        return (a1->GetLclNum() == a2->GetLclNum()) && (a1->m_lclOffs == a2->m_lclOffs);
    }

#if DEBUGGABLE_GENTREE
    GenTreeLclAddr() = default;
#endif
};

struct GenTreeLclUse;

class LclUses;

struct GenTreeLclDef final : public GenTreeUnOp
{
    friend LclUses;

private:
    unsigned       m_lclNum;
    BasicBlock*    m_block;
    GenTreeLclUse* m_uses = nullptr;

public:
    GenTreeLclDef(GenTree* value, BasicBlock* block, unsigned lclNum)
        : GenTreeUnOp(GT_LCL_DEF, value->GetType(), value), m_lclNum(lclNum), m_block(block)
    {
        gtFlags |= GTF_ASG;
    }

    GenTreeLclDef(const GenTreeLclDef* copyFrom)
        : GenTreeUnOp(GT_LCL_DEF, copyFrom->GetType(), copyFrom->gtOp1), m_lclNum(copyFrom->m_lclNum), m_block(nullptr)
    {
        // TODO-MIKE-Consider: Maybe this should just assert. SSA defs can't really
        // be cloned, at least not with the simplistic CloneExpr mechanism. Existing
        // code is unlikely to clone assignments to begin with.
    }

    void Init()
    {
        m_uses = nullptr;
    }

    unsigned GetLclNum() const
    {
        return m_lclNum;
    }

    void SetLclNum(unsigned lclNum)
    {
        assert(lclNum != BAD_VAR_NUM);

        m_lclNum = lclNum;
    }

    GenTree* GetValue() const
    {
        return gtOp1;
    }

    void SetValue(GenTree* value)
    {
        gtOp1 = value;
    }

    BasicBlock* GetBlock() const
    {
        return m_block;
    }

    void SetBlock(BasicBlock* block)
    {
        m_block = block;
    }

    void AddUse(GenTreeLclUse* use);
    void RemoveUse(GenTreeLclUse* use);
    LclUses Uses();

    GenTreeLclUse* GetUseList() const
    {
        return m_uses;
    }

#if DEBUGGABLE_GENTREE
    GenTreeLclDef() = default;
#endif
};

struct GenTreeLclUse final : public GenTree
{
    friend GenTreeLclDef;
    friend LclUses;

private:
    // TODO-MIKE-Cleanup: SSA uses currently don't need the basic block but it could
    // be useful to have it around. SSA PHI args need the predecessor block and we
    // don't have more room for it so we just store it here, as if the PHI arg occurs
    // in the predecessor. Depending on how you look at it that may actually be true,
    // but it could be confusing.
    BasicBlock* m_block;

    GenTreeLclDef* m_def     = nullptr;
    GenTreeLclUse* m_nextUse = nullptr;
    GenTreeLclUse* m_prevUse = nullptr;

public:
    GenTreeLclUse(GenTreeLclDef* def, BasicBlock* block) : GenTree(GT_LCL_USE, def->GetType()), m_block(block)
    {
        def->AddUse(this);
    }

    GenTreeLclUse(const GenTreeLclUse* copyFrom) : GenTree(GT_LCL_USE, copyFrom->GetType()), m_block(copyFrom->m_block)
    {
        copyFrom->m_def->AddUse(this);
    }

    void Init()
    {
        m_def     = nullptr;
        m_nextUse = nullptr;
        m_prevUse = nullptr;
    }

    GenTreeLclDef* GetDef() const
    {
        return m_def;
    }

    BasicBlock* GetBlock() const
    {
        return m_block;
    }

    void SetBlock(BasicBlock* block)
    {
        m_block = block;
    }

    GenTreeLclUse* GetNextUse() const
    {
        return m_nextUse;
    }

#if DEBUGGABLE_GENTREE
    GenTreeLclUse() = default;
#endif
};

class LclUses
{
    GenTreeLclUse* m_uses;

public:
    LclUses(GenTreeLclUse* uses) : m_uses(uses)
    {
    }

    class iterator
    {
        GenTreeLclUse* m_use;

    public:
        iterator(GenTreeLclUse* use) : m_use(use)
        {
        }

        GenTreeLclUse* operator*()
        {
            return m_use;
        }

        bool operator==(const iterator& other) const
        {
            return m_use == other.m_use;
        }

        bool operator!=(const iterator& other) const
        {
            return m_use != other.m_use;
        }

        void operator++()
        {
            m_use = m_use->m_nextUse;
            if (m_use == m_use->m_def->m_uses)
            {
                m_use = nullptr;
            }
        }
    };

    iterator begin()
    {
        return iterator(m_uses);
    }

    iterator end()
    {
        return iterator(nullptr);
    }
};

inline LclUses GenTreeLclDef::Uses()
{
    return LclUses(m_uses);
}

struct GenTreePhi final : public GenTree
{
    class Use
    {
        GenTree* m_node;
        Use*     m_next;

    public:
        Use(GenTreeLclUse* node, Use* next = nullptr) : m_node(node), m_next(next)
        {
        }

        GenTree*& NodeRef()
        {
            return m_node;
        }

        GenTreeLclUse* GetNode() const
        {
            return m_node->AsLclUse();
        }

        void SetNode(GenTreeLclUse* node)
        {
            m_node = node;
        }

        Use*& NextRef()
        {
            return m_next;
        }

        Use* GetNext() const
        {
            return m_next;
        }
    };

    class UseIterator
    {
        Use* m_use;

    public:
        UseIterator(Use* use) : m_use(use)
        {
        }

        Use& operator*() const
        {
            return *m_use;
        }

        Use* operator->() const
        {
            return m_use;
        }

        UseIterator& operator++()
        {
            m_use = m_use->GetNext();
            return *this;
        }

        bool operator==(const UseIterator& i) const
        {
            return m_use == i.m_use;
        }

        bool operator!=(const UseIterator& i) const
        {
            return m_use != i.m_use;
        }
    };

    class UseList
    {
        Use* m_uses;

    public:
        UseList(Use* uses) : m_uses(uses)
        {
        }

        UseIterator begin() const
        {
            return UseIterator(m_uses);
        }

        UseIterator end() const
        {
            return UseIterator(nullptr);
        }
    };

    Use* m_uses;

    GenTreePhi(var_types type) : GenTree(GT_PHI, type), m_uses(nullptr)
    {
    }

    UseList Uses() const
    {
        return UseList(m_uses);
    }

    static bool Equals(GenTreePhi* phi1, GenTreePhi* phi2);

#if DEBUGGABLE_GENTREE
    GenTreePhi() = default;
#endif
};

class FieldInfo
{
    // TODO-MIKE-Cleanup: For now these are 16 bit to be consistent with LCL_FLD,
    // but EXTRACT/INSERT have enough space to make these 32 bit.
    uint16_t  m_typeNum;
    uint16_t  m_offset;
    FieldSeq* m_fieldSeq;

public:
    FieldInfo(unsigned typeNum, unsigned offset, FieldSeq* fieldSeq)
        : m_typeNum(static_cast<uint16_t>(typeNum)), m_offset(static_cast<uint16_t>(offset)), m_fieldSeq(fieldSeq)
    {
        assert(typeNum <= UINT16_MAX);
        assert(offset <= UINT16_MAX);
    }

    FieldInfo(const FieldInfo& field) = default;

    var_types GetType() const
    {
        return varTypeFromTypeNum(m_typeNum);
    }

    uint16_t GetTypeNum() const
    {
        return m_typeNum;
    }

    uint16_t GetLayoutNum() const
    {
        return m_typeNum < TYP_COUNT ? 0 : m_typeNum;
    }

    uint16_t GetOffset() const
    {
        return m_offset;
    }

    FieldSeqNode* GetFieldSeq() const
    {
        return m_fieldSeq;
    }

    bool HasFieldSeq() const
    {
        return (m_fieldSeq != nullptr) && (m_fieldSeq != FieldSeqNode::NotAField());
    }

#if DEBUGGABLE_GENTREE
    FieldInfo() = default;
#endif
};

struct GenTreeInsert final : public GenTreeOp
{
private:
    FieldInfo m_field;

public:
    GenTreeInsert(
        GenTree* fieldValue, GenTree* structValue, unsigned fieldTypeNum, unsigned fieldOffset, FieldSeq* fieldSeq)
        : GenTreeOp{GT_INSERT, structValue->GetType(), fieldValue, structValue}
        , m_field{fieldTypeNum, fieldOffset, fieldSeq}
    {
    }

    GenTreeInsert(const GenTreeInsert* copyFrom)
        : GenTreeOp{GT_INSERT, copyFrom->GetType(), copyFrom->gtOp1, copyFrom->gtOp2}, m_field{copyFrom->m_field}
    {
    }

    GenTree* GetFieldValue() const
    {
        return gtOp1;
    }

    void SetFieldValue(GenTree* value)
    {
        gtOp1 = value;
    }

    GenTree* GetStructValue() const
    {
        return gtOp2;
    }

    void SetStructValue(GenTree* value)
    {
        gtOp2 = value;
    }

    const FieldInfo& GetField() const
    {
        return m_field;
    }

    void SetField(unsigned typeNum, unsigned offset, FieldSeq* fieldSeq)
    {
        m_field = {typeNum, offset, fieldSeq};
    }

#if DEBUGGABLE_GENTREE
    GenTreeInsert() = default;
#endif
};

struct GenTreeExtract final : public GenTreeUnOp
{
private:
    FieldInfo m_field;

public:
    GenTreeExtract(GenTree* structValue, unsigned fieldTypeNum, unsigned fieldOffset, FieldSeqNode* fieldSeq)
        : GenTreeUnOp{GT_EXTRACT, varTypeFromTypeNum(fieldTypeNum), structValue}
        , m_field{fieldTypeNum, fieldOffset, fieldSeq}
    {
    }

    GenTreeExtract(const GenTreeExtract* copyFrom)
        : GenTreeUnOp{GT_EXTRACT, copyFrom->GetType(), copyFrom->gtOp1}, m_field{copyFrom->m_field}
    {
    }

    GenTree* GetStructValue() const
    {
        return gtOp1;
    }

    void SetStructValue(GenTree* value)
    {
        gtOp1 = value;
    }

    const FieldInfo& GetField() const
    {
        return m_field;
    }

    void SetField(unsigned typeNum, unsigned offset, FieldSeq* fieldSeq)
    {
        m_field = {typeNum, offset, fieldSeq};
    }

    ClassLayout* GetLayout(Compiler* compiler) const;

#if DEBUGGABLE_GENTREE
    GenTreeExtract() = default;
#endif
};

struct GenTreeCast : public GenTreeOp
{
private:
    var_types castType;

public:
    GenTreeCast(var_types castType, GenTree* op, bool fromUnsigned DEBUGARG(bool largeNode = false))
        : GenTreeOp(GT_CAST, varCastType(castType), op, nullptr DEBUGARG(largeNode)), castType(castType)
    {
        assert(varTypeIsArithmetic(castType));
        // We do not allow casts from floating point types to be treated as from
        // unsigned to avoid bugs related to wrong GTF_UNSIGNED in case the
        // operand's type changes.
        assert(!varTypeIsFloating(op->GetType()) || !fromUnsigned);

        gtFlags |= fromUnsigned ? GTF_UNSIGNED : GTF_EMPTY;
    }

    GenTreeCast(const GenTreeCast* copyFrom DEBUGARG(bool largeNode = false))
        : GenTreeOp(GT_CAST, copyFrom->GetType(), copyFrom->GetOp(0), nullptr DEBUGARG(largeNode))
        , castType(copyFrom->castType)
    {
        gtFlags |= copyFrom->gtFlags & GTF_UNSIGNED;
    }

    var_types GetCastType() const
    {
        return castType;
    }

    void SetCastType(var_types type)
    {
        assert(varTypeIsArithmetic(type));

        castType = type;
        SetType(varCastType(type));
    }

#if DEBUGGABLE_GENTREE
    GenTreeCast() = default;
#endif
};

// GT_BOX nodes are place markers for boxed values.  The "real" tree
// for most purposes is in gtBoxOp.
struct GenTreeBox : public GenTreeUnOp
{
    // This is the statement that contains the assignment tree when the node is an inlined GT_BOX on a value
    // type
    Statement* gtAsgStmtWhenInlinedBoxValue;
    // And this is the statement that copies from the value being boxed to the box payload
    Statement* gtCopyStmtWhenInlinedBoxValue;

    GenTreeBox(var_types  type,
               GenTree*   boxOp,
               Statement* asgStmtWhenInlinedBoxValue,
               Statement* copyStmtWhenInlinedBoxValue)
        : GenTreeUnOp(GT_BOX, type, boxOp)
        , gtAsgStmtWhenInlinedBoxValue(asgStmtWhenInlinedBoxValue)
        , gtCopyStmtWhenInlinedBoxValue(copyStmtWhenInlinedBoxValue)
    {
    }

#if DEBUGGABLE_GENTREE
    GenTreeBox() : GenTreeUnOp()
    {
    }
#endif
};

struct GenTreeFieldAddr : public GenTreeUnOp
{
    FieldSeqNode* m_fieldSeq;
    unsigned      m_offset;
    bool          m_mayOverlap;
    uint16_t      m_layoutNum;
#ifdef FEATURE_READYTORUN_COMPILER
    void* m_r2rFieldLookupAddr;
#endif

public:
    GenTreeFieldAddr(GenTree* addr, FieldSeqNode* fieldSeq, unsigned offset)
        : GenTreeUnOp(GT_FIELD_ADDR, varTypeAddrAdd(addr->GetType()), addr)
        , m_fieldSeq(fieldSeq)
        , m_offset(offset)
        , m_mayOverlap(false)
        , m_layoutNum(0)
#ifdef FEATURE_READYTORUN_COMPILER
        , m_r2rFieldLookupAddr(nullptr)
#endif
    {
        assert(varTypeIsI(addr->GetType()));
        assert(fieldSeq != nullptr);
        gtFlags |= addr->GetSideEffects();
    }

    GenTreeFieldAddr(const GenTreeFieldAddr* copyFrom)
        : GenTreeUnOp(GT_FIELD_ADDR, copyFrom->GetType(), copyFrom->GetAddr())
        , m_fieldSeq(copyFrom->m_fieldSeq)
        , m_offset(copyFrom->m_offset)
        , m_mayOverlap(copyFrom->m_mayOverlap)
        , m_layoutNum(copyFrom->m_layoutNum)
#ifdef FEATURE_READYTORUN_COMPILER
        , m_r2rFieldLookupAddr(copyFrom->m_r2rFieldLookupAddr)
#endif
    {
    }

    GenTree* GetAddr() const
    {
        return gtOp1;
    }

    void SetAddr(GenTree* addr)
    {
        assert(varTypeIsI(addr->GetType()));
        gtOp1 = addr;
    }

    FieldSeqNode* GetFieldSeq() const
    {
        return m_fieldSeq;
    }

    unsigned GetOffset() const
    {
        return m_offset;
    }

    void SetOffset(unsigned offset, FieldSeqNode* fieldSeq)
    {
        m_offset   = offset;
        m_fieldSeq = fieldSeq;
    }

    bool MayOverlap() const
    {
        return m_mayOverlap;
    }

    void SetMayOverlap()
    {
        m_mayOverlap = true;
    }

    uint16_t GetLayoutNum() const
    {
        return m_layoutNum;
    }

    void SetLayoutNum(unsigned layoutNum)
    {
        assert(layoutNum <= UINT16_MAX);
        m_layoutNum = static_cast<uint16_t>(layoutNum);
    }

    ClassLayout* GetLayout(Compiler* compiler) const;
    void SetLayout(ClassLayout* layout, Compiler* compiler);

#ifdef FEATURE_READYTORUN_COMPILER
    void* GetR2RFieldLookupAddr() const
    {
        return m_r2rFieldLookupAddr;
    }

    void SetR2RFieldLookupAddr(void* addr)
    {
        m_r2rFieldLookupAddr = addr;
    }
#endif

#if DEBUGGABLE_GENTREE
    GenTreeFieldAddr() : GenTreeUnOp()
    {
    }
#endif
};

// gtCall   -- method call      (GT_CALL)
enum class InlineObservation;

//------------------------------------------------------------------------
// GenTreeCallFlags: a bitmask of flags for GenTreeCall stored in gtCallMoreFlags.
//
// clang-format off
enum GenTreeCallFlags : unsigned int
{
    GTF_CALL_M_EMPTY                   = 0,

    GTF_CALL_M_EXPLICIT_TAILCALL       = 0x00000001, // the call is "tail" prefixed and importer has performed tail call checks
    GTF_CALL_M_TAILCALL                = 0x00000002, // the call is a tailcall
    GTF_CALL_M_VARARGS                 = 0x00000004, // the call uses varargs ABI
    GTF_CALL_M_RETBUFFARG              = 0x00000008, // call has a return buffer argument
    GTF_CALL_M_DELEGATE_INV            = 0x00000010, // call to Delegate.Invoke
    GTF_CALL_M_NOGCCHECK               = 0x00000020, // not a call for computing full interruptability and therefore no GC check is required.
    GTF_CALL_M_SPECIAL_INTRINSIC       = 0x00000040, // function that could be optimized as an intrinsic
                                                     // in special cases. Used to optimize fast way out in morphing
    GTF_CALL_M_UNMGD_THISCALL          = 0x00000080, // "this" pointer (first argument) should be enregistered (only for GTF_CALL_UNMANAGED)
    GTF_CALL_M_VIRTSTUB_REL_INDIRECT   = 0x00000080, // the virtstub is indirected through a relative address (only for GTF_CALL_VIRT_STUB)
#ifdef TARGET_X86
    GTF_CALL_M_TAILCALL_VIA_JIT_HELPER = 0x00000200, // call is a tail call dispatched via tail call JIT helper.
#endif

#if FEATURE_TAILCALL_OPT
    GTF_CALL_M_IMPLICIT_TAILCALL       = 0x00000400, // call is an opportunistic tail call and importer has performed tail call checks
    GTF_CALL_M_TAILCALL_TO_LOOP        = 0x00000800, // call is a fast recursive tail call that can be converted into a loop
#endif

    GTF_CALL_M_PINVOKE                 = 0x00001000, // call is a pinvoke.  This mirrors VM flag CORINFO_FLG_PINVOKE.
                                                     // A call marked as Pinvoke is not necessarily a GT_CALL_UNMANAGED. For e.g.
                                                     // an IL Stub dynamically generated for a PInvoke declaration is flagged as
                                                     // a Pinvoke but not as an unmanaged call. See impCheckForPInvokeCall() to
                                                     // know when these flags are set.

#if defined(FEATURE_READYTORUN_COMPILER) && defined(TARGET_ARMARCH)
    GTF_CALL_M_R2R_REL_INDIRECT        = 0x00002000, // ready to run call is indirected through a relative address
#endif
    GTF_CALL_M_DOES_NOT_RETURN         = 0x00004000, // call does not return
    GTF_CALL_M_WRAPPER_DELEGATE_INV    = 0x00008000, // call is in wrapper delegate
    GTF_CALL_M_FAT_POINTER_CHECK       = 0x00010000, // CoreRT managed calli needs transformation, that checks
                                                     // special bit in calli address. If it is set, then it is necessary
                                                     // to restore real function address and load hidden argument
                                                     // as the first argument for calli. It is CoreRT replacement for instantiating
                                                     // stubs, because executable code cannot be generated at runtime.
    GTF_CALL_M_HELPER_SPECIAL_DCE      = 0x00020000, // this helper call can be removed if it is part of a comma and
                                                     // the comma result is unused.
    GTF_CALL_M_DEVIRTUALIZED           = 0x00040000, // this call was devirtualized
    GTF_CALL_M_UNBOXED                 = 0x00080000, // this call was optimized to use the unboxed entry point
    GTF_CALL_M_GUARDED_DEVIRT          = 0x00100000, // this call is a candidate for guarded devirtualization
    GTF_CALL_M_GUARDED_DEVIRT_CHAIN    = 0x00200000, // this call is a candidate for chained guarded devirtualization
    GTF_CALL_M_GUARDED                 = 0x00400000, // this call was transformed by guarded devirtualization
    GTF_CALL_M_ALLOC_SIDE_EFFECTS      = 0x00800000, // this is a call to an allocator with side effects
    GTF_CALL_M_SUPPRESS_GC_TRANSITION  = 0x01000000, // suppress the GC transition (i.e. during a pinvoke) but a separate GC safe point is required.
    GTF_CALL_M_EXP_RUNTIME_LOOKUP      = 0x02000000, // this call needs to be tranformed into CFG for the dynamic dictionary expansion feature.
    GTF_CALL_M_STRESS_TAILCALL         = 0x04000000, // the call is NOT "tail" prefixed but GTF_CALL_M_EXPLICIT_TAILCALL was added because of tail call stress mode
    GTF_CALL_M_EXPANDED_EARLY          = 0x08000000, // the Virtual Call target address is expanded and placed in gtControlExpr in Morph rather than in Lower

};

inline constexpr GenTreeCallFlags operator ~(GenTreeCallFlags a)
{
    return (GenTreeCallFlags)(~(unsigned int)a);
}

inline constexpr GenTreeCallFlags operator |(GenTreeCallFlags a, GenTreeCallFlags b)
{
    return (GenTreeCallFlags)((unsigned int)a | (unsigned int)b);
}

inline constexpr GenTreeCallFlags operator &(GenTreeCallFlags a, GenTreeCallFlags b)
{
    return (GenTreeCallFlags)((unsigned int)a & (unsigned int)b);
}

inline GenTreeCallFlags& operator |=(GenTreeCallFlags& a, GenTreeCallFlags b)
{
    return a = (GenTreeCallFlags)((unsigned int)a | (unsigned int)b);
}

inline GenTreeCallFlags& operator &=(GenTreeCallFlags& a, GenTreeCallFlags b)
{
    return a = (GenTreeCallFlags)((unsigned int)a & (unsigned int)b);
}

// clang-format on

// Return type descriptor of a GT_CALL node.
enum structPassingKind : uint8_t
{
    SPK_Unknown,
    SPK_PrimitiveType, // The struct is passed/returned in a single register.
#if FEATURE_MULTIREG_RET
    SPK_ByValue,      // The struct is passed/returned in multiple registers.
    SPK_ByValueAsHfa, // The struct is passed/returned as an HFA in multiple registers.
#endif
    SPK_ByReference // The struct is passed/returned by reference to a copy/buffer.
};

struct StructPassing
{
    structPassingKind kind;
    var_types         type;

    StructPassing(structPassingKind kind, var_types type) : kind(kind), type(type)
    {
        assert(kind != SPK_Unknown);
        assert((type != TYP_UNDEF) || (kind != SPK_PrimitiveType));
    }
};

// Return type descriptor for the compiled method or a GT_CALL node.
//
// x64 Unix, Arm64, Arm32 and x86 allow a value to be returned in multiple
// registers. For such calls this struct provides the following info
// on their return type
//    - type of value returned in each return register
//    - ABI return register numbers in which the value is returned
//    - count of return registers in which the value is returned
//
struct ReturnTypeDesc
{
private:
#if FEATURE_MULTIREG_RET
    uint8_t m_regCount;
#else
    // Use only one bit as a hint to the compiler that the only possible values
    // are 0 and 1. Not all the multi-reg return related code is under ifdef and
    // without this PIN shows a ~0.25% regression.
    uint8_t m_regCount : 1;
#endif
    var_types m_regType[MAX_RET_REG_COUNT];

public:
    ReturnTypeDesc() : m_regCount(0)
    {
    }

#if FEATURE_MULTIREG_RET
    void InitializeStruct(Compiler* comp, ClassLayout* retLayout);
#endif

    void InitializePrimitive(var_types regType);

    void InitializeLong();

    void Reset()
    {
        m_regCount = 0;
    }

    unsigned GetRegCount() const
    {
        return m_regCount;
    }

    var_types GetRegType(unsigned i) const
    {
        assert(i < m_regCount);
        return m_regType[i];
    }

    regNumber GetRegNum(unsigned i) const;
};

class TailCallSiteInfo
{
    bool                   m_isCallvirt : 1;
    bool                   m_isCalli : 1;
    CORINFO_SIG_INFO       m_sig;
    CORINFO_RESOLVED_TOKEN m_token;

public:
    TailCallSiteInfo(const CORINFO_SIG_INFO* sig) : m_sig(*sig)
    {
    }

    bool IsCallvirt()
    {
        return m_isCallvirt;
    }

    bool IsCalli()
    {
        return m_isCalli;
    }

    CORINFO_RESOLVED_TOKEN* GetToken()
    {
        assert(!IsCalli());
        return &m_token;
    }

    CORINFO_SIG_INFO* GetSig()
    {
        return &m_sig;
    }

    void SetCalli()
    {
        m_isCallvirt = false;
        m_isCalli    = true;
    }

    void SetCallvirt(CORINFO_RESOLVED_TOKEN* token)
    {
        m_isCallvirt = true;
        m_isCalli    = false;
        m_token      = *token;
    }

    void SetCall(CORINFO_RESOLVED_TOKEN* token)
    {
        m_isCallvirt = false;
        m_isCalli    = false;
        m_token      = *token;
    }
};

class CallInfo;
class CallArgInfo;

struct GenTreeCall final : public GenTree
{
    class Use
    {
        GenTree* m_node;
        Use*     m_next;
        unsigned m_sigTypeNum;

    public:
        Use(GenTree* node, Use* next = nullptr)
            : m_node(node)
            , m_next(next)
            // Always record the type of node at call arg's creation. PopCallArgs will override this with
            // the actual signature type but for helper calls there is no signature information so we'll just
            // whatever we have. Helper calls usually don't have struct params so this should work most of the
            // time. Hopefully helper calls also don't have small int params, otherwise this will get messy on
            // osx-arm64. Use the actual type of the node as it's more likely to be correct (and consistently
            // incorrect if it's an osx-arm64 small int param), since a small int typed node doesn't imply that
            // the call param is also small int (e.g. we may have a BYTE indir and an INT param).
            , m_sigTypeNum(static_cast<unsigned>(varActualType(node->GetType())))
        {
            assert(node != nullptr);
        }

        GenTree*& NodeRef()
        {
            return m_node;
        }

        GenTree* GetNode() const
        {
            assert(m_node != nullptr);
            return m_node;
        }

        void SetNode(GenTree* node)
        {
            assert(node != nullptr);
            m_node = node;
        }

        Use*& NextRef()
        {
            return m_next;
        }

        Use* GetNext() const
        {
            return m_next;
        }

        void SetNext(Use* next)
        {
            m_next = next;
        }

        unsigned GetSigTypeNum() const
        {
            return m_sigTypeNum;
        }

        void SetSigTypeNum(unsigned typeNum)
        {
            m_sigTypeNum = typeNum;
        }
    };

    class UseIterator
    {
        Use* m_use;

    public:
        UseIterator(Use* use) : m_use(use)
        {
        }

        Use& operator*() const
        {
            return *m_use;
        }

        Use* operator->() const
        {
            return m_use;
        }

        Use* GetUse() const
        {
            return m_use;
        }

        UseIterator& operator++()
        {
            m_use = m_use->GetNext();
            return *this;
        }

        bool operator==(const UseIterator& i) const
        {
            return m_use == i.m_use;
        }

        bool operator!=(const UseIterator& i) const
        {
            return m_use != i.m_use;
        }
    };

    class UseList
    {
        Use* m_uses;

    public:
        UseList(Use* uses) : m_uses(uses)
        {
        }

        UseIterator begin() const
        {
            return UseIterator(m_uses);
        }

        UseIterator end() const
        {
            return UseIterator(nullptr);
        }
    };

    Use* gtCallThisArg;  // The instance argument ('this' pointer)
    Use* gtCallArgs;     // The list of arguments in original evaluation order
    Use* gtCallLateArgs; // On x86:     The register arguments in an optimal order
                         // On ARM/x64: - also includes any outgoing arg space arguments
                         //             - that were evaluated into a temp LclVar

    union {
        // only used for CALLI unmanaged calls (CT_INDIRECT)
        GenTree* gtCallCookie;
        // gtInlineCandidateInfo is only used when inlining methods
        InlineCandidateInfo*                  gtInlineCandidateInfo;
        GuardedDevirtualizationCandidateInfo* gtGuardedDevirtualizationCandidateInfo;
        ClassProfileCandidateInfo*            gtClassProfileCandidateInfo;
        void*                                 gtStubCallStubAddr; // GTF_CALL_VIRT_STUB - these are never inlined
        CORINFO_GENERIC_HANDLE compileTimeHelperArgumentHandle; // Used to track type handle argument of dynamic helpers
        void*                  gtDirectCallAddress; // Used to pass direct call address between lower and codegen
    };

    union {
        CORINFO_METHOD_HANDLE gtCallMethHnd; // CT_USER_FUNC
        GenTree*              gtCallAddr;    // CT_INDIRECT
    };

    // expression evaluated after args are placed which determines the control target
    GenTree* gtControlExpr;

    CallInfo* fgArgInfo;

#ifdef FEATURE_READYTORUN_COMPILER
    // Call target lookup info for method call from a Ready To Run module
    // TODO-MIKE-Cleanup: This wastes 3/7 bytes due to useless enum bits and internal padding.
    CORINFO_CONST_LOOKUP gtEntryPoint;
#endif

    union {
        TailCallSiteInfo* tailCallInfo;
        // Only used for unmanaged calls, which cannot be tail-called
        CorInfoCallConvExtension unmgdCallConv;
    };

    ClassLayout* m_retLayout; // The layout of the return (struct) type.

    GenTreeCallFlags gtCallMoreFlags;

    uint8_t gtCallType : 3;   // value from the CallKind enumeration
    uint8_t m_retSigType : 5; // Signature return type

    ReturnTypeDesc m_retDesc;

#if defined(DEBUG) || defined(INLINE_DATA)
    // For non-inline candidates, track the first observation
    // that blocks candidacy.
    InlineObservation gtInlineObservation;

    // IL offset of the call wrt its parent method.
    IL_OFFSET gtRawILOffset;
#endif // defined(DEBUG) || defined(INLINE_DATA)

    // Used to register callsites with the EE
    INDEBUG(CORINFO_SIG_INFO* callSig;)

public:
    GenTreeCall(var_types type, CallKind kind, Use* args)
        : GenTree(GT_CALL, varActualType(type))
        , gtCallThisArg(nullptr)
        , gtCallArgs(args)
        , gtCallLateArgs(nullptr)
        , gtControlExpr(nullptr)
        , fgArgInfo(nullptr)
        , tailCallInfo(nullptr)
        , m_retLayout(nullptr)
        , gtCallMoreFlags(GTF_CALL_M_EMPTY)
        , gtCallType(static_cast<uint8_t>(kind))
        , m_retSigType(type)
#ifdef DEBUG
        , callSig(nullptr)
#endif
    {
        gtFlags |= (GTF_CALL | GTF_GLOB_REF);

        for (Use& use : UseList(args))
        {
            gtFlags |= use.GetNode()->GetSideEffects();
        }
    }

    GenTreeCall(const GenTreeCall* copyFrom)
        : GenTree(GT_CALL, copyFrom->GetType())
        , gtCallThisArg(nullptr)
        , gtCallArgs(nullptr)
        , gtCallLateArgs(nullptr)
        , gtControlExpr(nullptr)
        , fgArgInfo(nullptr)
        , m_retLayout(copyFrom->m_retLayout)
        , gtCallMoreFlags(copyFrom->gtCallMoreFlags)
        , gtCallType(copyFrom->gtCallType)
        , m_retSigType(copyFrom->m_retSigType)
        , m_retDesc(copyFrom->m_retDesc)
    {
    }

    CallInfo* GetInfo() const
    {
        return fgArgInfo;
    }

    void SetInfo(CallInfo* info)
    {
        fgArgInfo = info;
    }

    void ResetArgInfo();

    UseList Args()
    {
        return UseList(gtCallArgs);
    }

    UseList LateArgs()
    {
        return UseList(gtCallLateArgs);
    }

    GenTree* GetThisArg() const;
    GenTree* GetArgNodeByArgNum(unsigned argNum) const;
    CallArgInfo* GetArgInfoByArgNum(unsigned argNum) const;
    CallArgInfo* GetArgInfoByArgNode(GenTree* node) const;
    CallArgInfo* GetArgInfoByLateArgUse(Use* use) const;

    var_types GetRetSigType() const
    {
        return static_cast<var_types>(m_retSigType);
    }

    void SetRetSigType(var_types type)
    {
        m_retSigType = type;
    }

    ClassLayout* GetRetLayout() const
    {
        return m_retLayout;
    }

    void SetRetLayout(ClassLayout* layout)
    {
        assert((layout == nullptr) || layout->IsValueClass());
        assert((layout == nullptr) || varTypeIsStruct(GetRetSigType()));

        m_retLayout = layout;
    }

    unsigned GetRegCount() const
    {
        return m_retDesc.GetRegCount();
    }

    var_types GetRegType(unsigned i) const
    {
        return m_retDesc.GetRegType(i);
    }

    ReturnTypeDesc* GetRetDesc()
    {
        return &m_retDesc;
    }

    const ReturnTypeDesc* GetRetDesc() const
    {
        return &m_retDesc;
    }

    regMaskTP GetOtherRegMask() const;

    bool IsUnmanaged() const
    {
        return (gtFlags & GTF_CALL_UNMANAGED) != 0;
    }

    bool NeedsNullCheck() const
    {
        return (gtFlags & GTF_CALL_NULLCHECK) != 0;
    }

#ifdef TARGET_X86
    bool CallerPop() const
    {
        return (gtFlags & GTF_CALL_POP_ARGS) != 0;
    }
#endif

    bool IsVirtual() const
    {
        return (gtFlags & GTF_CALL_VIRT_KIND_MASK) != GTF_CALL_NONVIRT;
    }

    bool IsVirtualStub() const
    {
        return (gtFlags & GTF_CALL_VIRT_KIND_MASK) == GTF_CALL_VIRT_STUB;
    }

    bool IsVirtualVtable() const
    {
        return (gtFlags & GTF_CALL_VIRT_KIND_MASK) == GTF_CALL_VIRT_VTABLE;
    }

    bool IsInlineCandidate() const
    {
        return (gtFlags & GTF_CALL_INLINE_CANDIDATE) != 0;
    }

    void ClearInlineCandidate()
    {
        gtFlags &= ~GTF_CALL_INLINE_CANDIDATE;
    }

    bool HasNonStandardAddedArgs(Compiler* compiler) const;
    int GetNonStandardAddedArgCount(Compiler* compiler) const;

    // Returns true if this call uses a return buffer argument.
    bool HasRetBufArg() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_RETBUFFARG) != 0;
    }

    // Returns true if this call must be transformed as if having a return buffer
    // arg even if the actual signature and ABI conventions indicates otherwise
    // (HasRetBufArg == false).
    bool TreatAsHasRetBufArg() const;

#ifdef TARGET_ARM64
    bool HasFixedRetBufArg() const
    {
        return HasRetBufArg()
#ifdef TARGET_WINDOWS
               && !callConvIsInstanceMethodCallConv(GetUnmanagedCallConv())
#endif
            ;
    }
#endif

    bool HasMultiRegRetVal() const
    {
        return m_retDesc.GetRegCount() > 1;
    }

    // Returns true if VM has flagged this method as CORINFO_FLG_PINVOKE.
    bool IsPInvoke() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_PINVOKE) != 0;
    }

    // Note that the distinction of whether tail prefixed or an implicit tail call
    // is maintained on a call node till fgMorphCall() after which it will be
    // either a tail call (i.e. IsTailCall() is true) or a non-tail call.
    bool IsTailPrefixedCall() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_EXPLICIT_TAILCALL) != 0;
    }

    // Returns true if this call didn't have an explicit tail. prefix in the IL
    // but was marked as an explicit tail call because of tail call stress mode.
    bool IsStressTailCall() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_STRESS_TAILCALL) != 0;
    }

    // This method returning "true" implies that tail call flowgraph morhphing has
    // performed final checks and committed to making a tail call.
    bool IsTailCall() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_TAILCALL) != 0;
    }

    // This method returning "true" implies that importer has performed tail call checks
    // and providing a hint that this can be converted to a tail call.
    bool CanTailCall() const
    {
        return IsTailPrefixedCall() || IsImplicitTailCall();
    }

#ifdef TARGET_X86
    // Check whether this is a tailcall dispatched via JIT helper. We only use
    // this mechanism on x86 as it is faster than our other more general
    // tailcall mechanism.
    bool IsTailCallViaJitHelper() const
    {
        return IsTailCall() && ((gtCallMoreFlags & GTF_CALL_M_TAILCALL_VIA_JIT_HELPER) != 0);
    }

    bool IsFastTailCall() const
    {
        return false;
    }
#else
    bool    IsFastTailCall() const
    {
#if FEATURE_FASTTAILCALL
        return IsTailCall();
#else
        return false;
#endif
    }
#endif

    // Returns true if this is marked for opportunistic tail calling.
    // That is, can be tail called though not explicitly prefixed with "tail" prefix.
    bool IsImplicitTailCall() const
    {
#if FEATURE_TAILCALL_OPT
        return (gtCallMoreFlags & GTF_CALL_M_IMPLICIT_TAILCALL) != 0;
#else
        return false;
#endif
    }
    bool IsTailCallConvertibleToLoop() const
    {
#if FEATURE_TAILCALL_OPT
        return (gtCallMoreFlags & GTF_CALL_M_TAILCALL_TO_LOOP) != 0;
#else
        return false;
#endif
    }

    bool IsDelegateInvoke() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_DELEGATE_INV) != 0;
    }
    bool IsVirtualStubRelativeIndir() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_VIRTSTUB_REL_INDIRECT) != 0;
    }

#ifdef FEATURE_READYTORUN_COMPILER
#ifdef TARGET_ARMARCH
    bool IsR2ROrVirtualStubRelativeIndir() const
    {
        return IsR2RRelativeIndir() || (IsVirtualStub() && IsVirtualStubRelativeIndir());
    }

    bool IsR2RRelativeIndir() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_R2R_REL_INDIRECT) != 0;
    }
#endif

    void setEntryPoint(const CORINFO_CONST_LOOKUP& entryPoint)
    {
        gtEntryPoint = entryPoint;

#ifdef TARGET_ARMARCH
        if (gtEntryPoint.accessType == IAT_PVALUE)
        {
            gtCallMoreFlags |= GTF_CALL_M_R2R_REL_INDIRECT;
        }
#endif
    }
#endif // FEATURE_READYTORUN_COMPILER

    bool IsVarargs() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_VARARGS) != 0;
    }

    bool IsNoReturn() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_DOES_NOT_RETURN) != 0;
    }

    bool IsFatPointerCandidate() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_FAT_POINTER_CHECK) != 0;
    }

    bool IsPure(Compiler* compiler) const;

    bool HasSideEffects(Compiler* compiler, bool ignoreExceptions = false, bool ignoreCctors = false) const;

    void ClearFatPointerCandidate()
    {
        gtCallMoreFlags &= ~GTF_CALL_M_FAT_POINTER_CHECK;
    }

    void SetFatPointerCandidate()
    {
        gtCallMoreFlags |= GTF_CALL_M_FAT_POINTER_CHECK;
    }

    bool IsDevirtualized() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_DEVIRTUALIZED) != 0;
    }

    bool IsGuarded() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_GUARDED) != 0;
    }

    bool IsUnboxed() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_UNBOXED) != 0;
    }

    bool IsSuppressGCTransition() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_SUPPRESS_GC_TRANSITION) != 0;
    }

    bool RequiresPInvokeFrame() const
    {
        return IsUnmanaged() && !IsSuppressGCTransition();
    }

    bool IsGuardedDevirtualizationCandidate() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_GUARDED_DEVIRT) != 0;
    }

    void ClearGuardedDevirtualizationCandidate()
    {
        gtCallMoreFlags &= ~GTF_CALL_M_GUARDED_DEVIRT;
    }

    void SetGuardedDevirtualizationCandidate()
    {
        gtCallMoreFlags |= GTF_CALL_M_GUARDED_DEVIRT;
    }

    void SetIsGuarded()
    {
        gtCallMoreFlags |= GTF_CALL_M_GUARDED;
    }

    void SetExpRuntimeLookup()
    {
        gtCallMoreFlags |= GTF_CALL_M_EXP_RUNTIME_LOOKUP;
    }

    void ClearExpRuntimeLookup()
    {
        gtCallMoreFlags &= ~GTF_CALL_M_EXP_RUNTIME_LOOKUP;
    }

    bool IsExpRuntimeLookup() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_EXP_RUNTIME_LOOKUP) != 0;
    }

    void SetExpandedEarly()
    {
        gtCallMoreFlags |= GTF_CALL_M_EXPANDED_EARLY;
    }

    void ClearExpandedEarly()
    {
        gtCallMoreFlags &= ~GTF_CALL_M_EXPANDED_EARLY;
    }

    bool IsExpandedEarly() const
    {
        return (gtCallMoreFlags & GTF_CALL_M_EXPANDED_EARLY) != 0;
    }

    bool IsUserCall() const
    {
        return gtCallType == CT_USER_FUNC;
    }

    bool IsHelperCall() const
    {
        return gtCallType == CT_HELPER;
    }

    bool IsIndirectCall() const
    {
        return gtCallType == CT_INDIRECT;
    }

    bool IsHelperCall(CORINFO_METHOD_HANDLE callMethHnd) const
    {
        return IsHelperCall() && (callMethHnd == gtCallMethHnd);
    }

    bool IsHelperCall(Compiler* compiler, unsigned helper) const;

    bool AreArgsComplete() const;

    CorInfoCallConvExtension GetUnmanagedCallConv() const
    {
        return IsUnmanaged() ? unmgdCallConv : CorInfoCallConvExtension::Managed;
    }

    CORINFO_METHOD_HANDLE GetMethodHandle() const
    {
        return gtCallMethHnd;
    }

    InlineCandidateInfo* GetInlineCandidateInfo() const
    {
        assert(IsInlineCandidate());
        return gtInlineCandidateInfo;
    }

    bool IsTypeHandleToRuntimeTypeHelperCall() const;
    bool IsTypeHandleToRuntimeTypeHandleHelperCall() const;

    static bool Equals(GenTreeCall* c1, GenTreeCall* c2);

#if DEBUGGABLE_GENTREE
    GenTreeCall() : GenTree()
    {
    }
#endif
};

class CallArgInfo
{
public:
    GenTreeCall::Use* use; // Points to the argument's GenTreeCall::Use in gtCallArgs or gtCallThisArg.

private:
    GenTreeCall::Use* m_lateUse; // Points to the argument's GenTreeCall::Use in gtCallLateArgs, if any.

    unsigned m_argNum; // The original argument number, also specifies the IL argument evaluation order

    unsigned m_slotNum;   // When an argument is passed in the OutArg area this is the slot number in the OutArg area
    unsigned m_slotCount; // Count of number of slots that this argument uses

    unsigned m_tempLclNum; // the LclVar number if we had to force evaluation of this arg

    var_types m_argType; // The type used to pass this argument. This is generally the original argument type, but when
                         // a struct is passed as a scalar type, this is that type.
                         // Note that if a struct is passed by reference, this will still be the struct type.

    bool m_tempNeeded : 1; // True when we force this argument's evaluation into a temp LclVar
#if FEATURE_FIXED_OUT_ARGS
    bool m_placeholderNeeded : 1; // True when we must replace this argument with a placeholder node
#endif
    bool m_isNonStandard : 1; // True if it is an arg that is passed in a reg other than a standard arg reg, or is
                              // forced to be on the stack despite its arg list position.
#ifdef TARGET_64BIT
    bool m_isImplicitByRef : 1;
#endif
    bool m_isReturn : 1;

    // Count of registers used by this argument.
    // Note that on ARM, if we have a double HFA, this reflects the number of DOUBLE registers.
    uint8_t m_regCount;

#ifdef UNIX_AMD64_ABI
    // On unix-x64 arg registers may have different types so we need to store all of them.
    var_types      m_regTypes[MAX_ARG_REG_COUNT];
    regNumberSmall m_regNums[MAX_ARG_REG_COUNT];
#else
#ifdef FEATURE_HFA
    var_types m_regType;
#endif
    // Other multireg targets (ARM, ARM64) always use the same register type so it's enough
    // to store only the first register in arg info, the rest can be computed on the fly.
    regNumberSmall m_regNum;
#endif

public:
    CallArgInfo(unsigned argNum, GenTreeCall::Use* use, unsigned regCount, bool isReturn = false)
        : use(use)
        , m_lateUse(nullptr)
        , m_argNum(argNum)
        , m_slotNum(0)
        , m_slotCount(0)
        , m_tempLclNum(BAD_VAR_NUM)
        , m_argType(TYP_UNDEF)
        , m_tempNeeded(false)
#if FEATURE_FIXED_OUT_ARGS
        , m_placeholderNeeded(false)
#endif
        , m_isNonStandard(false)
#ifdef TARGET_64BIT
        , m_isImplicitByRef(false)
#endif
        , m_isReturn(isReturn)
        , m_regCount(static_cast<uint8_t>(regCount))
#ifdef FEATURE_HFA
        , m_regType(TYP_I_IMPL)
#endif
    {
        assert(regCount <= static_cast<unsigned>(isReturn ? MAX_RET_REG_COUNT : MAX_ARG_REG_COUNT));
    }

    // Get the use that coresponds to this argument.
    // This is the "real" argument use and not the use of the setup tree.
    GenTreeCall::Use* GetUse() const
    {
        return m_lateUse == nullptr ? use : m_lateUse;
    }

    GenTree* GetNode() const
    {
        return GetUse()->GetNode();
    }

    unsigned GetSigTypeNum() const
    {
        return use->GetSigTypeNum();
    }

    void SetNode(GenTree* node)
    {
        GetUse()->SetNode(node);
    }

    GenTreeCall::Use* GetLateUse() const
    {
        return m_lateUse;
    }

    void SetLateUse(GenTreeCall::Use* lateUse)
    {
        assert(lateUse != nullptr);
        m_lateUse = lateUse;
    }

    bool HasLateUse() const
    {
        return m_lateUse != nullptr;
    }

    unsigned GetArgNum() const
    {
        return m_argNum;
    }

    var_types GetArgType() const
    {
        return m_argType;
    }

    void SetArgType(var_types argType)
    {
        m_argType = argType;
    }

    bool HasTemp() const
    {
        return m_tempLclNum != BAD_VAR_NUM;
    }

    unsigned GetTempLclNum() const
    {
        return m_tempLclNum;
    }

    void SetTempLclNum(unsigned lclNum)
    {
        assert(m_tempLclNum == BAD_VAR_NUM);
        m_tempLclNum = lclNum;
    }

    bool IsTempNeeded() const
    {
        return m_tempNeeded;
    }

    void SetTempNeeded()
    {
        m_tempNeeded = true;
    }

    bool IsPlaceholderNeeded() const
    {
#if FEATURE_FIXED_OUT_ARGS
        return m_placeholderNeeded;
#else
        return false;
#endif
    }

#if FEATURE_FIXED_OUT_ARGS
    void SetPlaceholderNeeded()
    {
        assert(m_slotCount != 0);
        m_placeholderNeeded = true;
    }
#endif

    bool IsNonStandard() const
    {
        return m_isNonStandard;
    }

    void SetNonStandard(bool isNonStandard)
    {
        m_isNonStandard = isNonStandard;
    }

#ifdef UNIX_AMD64_ABI
    var_types GetRegType(unsigned i) const
    {
        assert(i < m_regCount);
        return m_regTypes[i];
    }

    void SetRegType(unsigned i, var_types type)
    {
        assert(i < m_regCount);
        m_regTypes[i] = type;
    }
#elif defined(FEATURE_HFA)
    var_types GetRegType(unsigned i = 0) const
    {
        assert(i < m_regCount);
        return m_regType;
    }

    void SetRegType(var_types type)
    {
        assert(m_regCount > 0);
        m_regType = type;
    }
#else
    var_types GetRegType(unsigned i = 0) const
    {
        assert(i < m_regCount);
        return TYP_I_IMPL;
    }

    void SetRegType(var_types type)
    {
    }
#endif

    regNumber GetRegNum(unsigned i = 0) const
    {
        assert(i < m_regCount);
#if defined(TARGET_ARM) && defined(FEATURE_HFA)
        if (m_regType == TYP_DOUBLE)
        {
            return static_cast<regNumber>(m_regNum + i * 2);
        }
#endif
#ifdef WINDOWS_X86_ABI
        if (m_isReturn)
        {
            return i == 0 ? REG_RAX : REG_RDX;
        }
#endif
#ifdef UNIX_AMD64_ABI
        return static_cast<regNumber>(m_regNums[i]);
#else
        return static_cast<regNumber>(m_regNum + i);
#endif
    }

    void SetRegNum(unsigned i, regNumber regNum)
    {
#ifdef UNIX_AMD64_ABI
        assert(i < m_regCount);
        m_regNums[i] = static_cast<regNumberSmall>(regNum);
#else
        m_regNum = static_cast<regNumberSmall>(regNum);
#endif
    }

    unsigned GetSlotNum() const
    {
        return m_slotNum;
    }

    void SetSlots(unsigned firstSlot, unsigned slotCount)
    {
        m_slotNum   = firstSlot;
        m_slotCount = slotCount;
    }

    bool IsHfaArg()
    {
#ifdef FEATURE_HFA
        return m_regType != TYP_I_IMPL;
#else
        return false;
#endif
    }

    bool IsSplit() const
    {
#ifdef FEATURE_ARG_SPLIT
        return (m_regCount != 0) && (m_slotCount != 0);
#else
        return false;
#endif
    }

    bool IsSingleRegOrSlot()
    {
        return m_regCount + m_slotCount == 1;
    }

    unsigned GetRegCount()
    {
        return m_regCount;
    }

    unsigned GetSlotCount()
    {
        return m_slotCount;
    }

    bool IsImplicitByRef() const
    {
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
        return m_isImplicitByRef;
#else
        return false;
#endif
    }

    void SetIsImplicitByRef(bool isImplicitByRef)
    {
// UNIX_AMD64_ABI has implicit by-ref parameters but they're C++ specific
// and thus not expected to appear in CLR programs.
#if defined(WINDOWS_AMD64_ABI) || defined(TARGET_ARM64)
        m_isImplicitByRef = isImplicitByRef;
#else
        assert(!isImplicitByRef);
#endif
    }

    INDEBUG(void Dump() const;)
};

typedef CallArgInfo fgArgTabEntry;

class CallInfo
{
    CallArgInfo** argTable;         // variable sized array of per argument descrption: (i.e. argTable[argTableSize])
    INDEBUG(unsigned argTableSize;) // size of argTable array (equal to the argCount when done with fgMorphArgs)
    unsigned argCount;              // Updatable arg count value
    unsigned nextSlotNum;           // Updatable slot count value

#if defined(UNIX_X86_ABI)
    unsigned stkSizeBytes;  // Size of stack used by this call, in bytes. Calculated during fgMorphArgs().
    unsigned padStkAlign;   // Stack alignment in bytes required before arguments are pushed for this call.
                            // Computed dynamically during codegen, based on stkSizeBytes and the current
                            // stack level (genStackLevel) when the first stack adjustment is made for
                            // this call.
    bool alignmentDone : 1; // Updateable flag, set to 'true' after we've done any required alignment.
#endif
    bool hasRegArgs : 1;   // true if we have one or more register arguments
    bool argsComplete : 1; // marker for state

    void SortArgs(Compiler* compiler, GenTreeCall* call, CallArgInfo** argTable);
    void EvalArgsToTemps(Compiler* compiler, GenTreeCall* call, CallArgInfo** argTable);

public:
    CallInfo(class Compiler* comp, GenTreeCall* call, unsigned argCount);
    CallInfo(class Compiler* comp, GenTreeCall* newCall, GenTreeCall* oldCall);

    void AddArg(CallArgInfo* argInfo);

    unsigned AllocateStackSlots(unsigned slotCount, unsigned alignment);

    void ArgsComplete(class Compiler* compiler, GenTreeCall* call);

    unsigned GetArgCount() const
    {
        return argCount;
    }

    CallArgInfo* GetArgInfo(unsigned i) const
    {
        assert(i < argCount);
        return argTable[i];
    }

    unsigned GetNextSlotNum() const
    {
        return nextSlotNum;
    }

    bool HasRegArgs() const
    {
        return hasRegArgs;
    }

    bool HasStackArgs() const
    {
        return nextSlotNum != INIT_ARG_STACK_SLOT;
    }

    bool AreArgsComplete() const
    {
        return argsComplete;
    }

#if defined(UNIX_X86_ABI)
    void ComputeStackAlignment(unsigned curStackLevelInBytes)
    {
        padStkAlign = AlignmentPad(curStackLevelInBytes, STACK_ALIGN);
    }

    unsigned GetStkAlign() const
    {
        return padStkAlign;
    }

    void SetStkSizeBytes(unsigned newStkSizeBytes)
    {
        stkSizeBytes = newStkSizeBytes;
    }

    unsigned GetStkSizeBytes() const
    {
        return stkSizeBytes;
    }

    bool IsStkAlignmentDone() const
    {
        return alignmentDone;
    }

    void SetStkAlignmentDone()
    {
        alignmentDone = true;
    }
#endif // defined(UNIX_X86_ABI)

    void Dump() const;
};

typedef CallInfo fgArgInfo;

struct GenTreeTernaryOp : public GenTreeOp
{
    GenTree* gtOp3;

    GenTreeTernaryOp(
        genTreeOps oper, var_types type, GenTree* op1, GenTree* op2, GenTree* op3 DEBUGARG(bool largeNode = false))
        : GenTreeOp(oper, type, op1, op2 DEBUGARG(largeNode)), gtOp3(op3)
    {
        assert(op1 != nullptr);
        assert(op2 != nullptr);
        assert(op3 != nullptr);

        gtFlags |= op3->GetSideEffects();
    }

    GenTreeTernaryOp(GenTreeTernaryOp* copyFrom) : GenTreeOp(copyFrom), gtOp3(copyFrom->gtOp3)
    {
    }

    GenTree* GetOp(unsigned index) const
    {
        switch (index)
        {
            case 0:
                assert(gtOp1 != nullptr);
                return gtOp1;
            case 1:
                assert(gtOp2 != nullptr);
                return gtOp2;
            case 2:
                assert(gtOp3 != nullptr);
                return gtOp3;
            default:
                unreached();
        }
    }

    void SetOp(unsigned index, GenTree* op)
    {
        assert(op != nullptr);

        switch (index)
        {
            case 0:
                gtOp1 = op;
                return;
            case 1:
                gtOp2 = op;
                return;
            case 2:
                gtOp3 = op;
                return;
            default:
                unreached();
        }
    }

    static bool Equals(GenTreeTernaryOp* x, GenTreeTernaryOp* y)
    {
        return (x->GetOper() == y->GetOper()) && (x->GetType() == y->GetType()) && Compare(x->gtOp1, y->gtOp1) &&
               Compare(x->gtOp2, y->gtOp2) && Compare(x->gtOp3, y->gtOp3);
    }

    // Delete some inherited functions to avoid accidental use, at least when
    // the node is accessed via GenTreeTernaryOp* rather than GenTree/Un/Op*.
    GenTree*           gtGetOp1() const          = delete;
    GenTree*           gtGetOp2() const          = delete;
    GenTree*           gtGetOp2IfPresent() const = delete;
    GenTreeUnOp*       AsUnOp()                  = delete;
    const GenTreeUnOp* AsUnOp() const            = delete;
    GenTreeOp*         AsOp()                    = delete;
    const GenTreeOp*   AsOp() const              = delete;

#if DEBUGGABLE_GENTREE
    GenTreeTernaryOp() : GenTreeOp()
    {
    }
#endif
};

struct GenTreeCmpXchg : public GenTreeTernaryOp
{
    GenTreeCmpXchg(var_types type, GenTree* addr, GenTree* value, GenTree* compareValue)
        : GenTreeTernaryOp(GT_CMPXCHG, type, addr, value, compareValue)
    {
        // There's no reason to do a compare-exchange on a local location,
        // so we'll assume that all of these have global effects.
        // TODO-MIKE-Review: Shouldn't this also have GTF_EXCEPT?
        gtFlags |= GTF_GLOB_REF | GTF_ASG;
    }

    GenTreeCmpXchg(GenTreeCmpXchg* copyFrom) : GenTreeTernaryOp(copyFrom)
    {
    }

    GenTree* GetAddr() const
    {
        return gtOp1;
    }

    GenTree* GetValue() const
    {
        return gtOp2;
    }

    GenTree* GetCompareValue() const
    {
        return gtOp3;
    }

#if DEBUGGABLE_GENTREE
    GenTreeCmpXchg() : GenTreeTernaryOp()
    {
    }
#endif
};

struct GenTreeFptrVal : public GenTree
{
    CORINFO_METHOD_HANDLE gtFptrMethod;

#ifdef FEATURE_READYTORUN_COMPILER
    CORINFO_CONST_LOOKUP gtEntryPoint;
#endif

    GenTreeFptrVal(var_types type, CORINFO_METHOD_HANDLE meth) : GenTree(GT_FTN_ADDR, type), gtFptrMethod(meth)
    {
#ifdef FEATURE_READYTORUN_COMPILER
        gtEntryPoint.addr       = nullptr;
        gtEntryPoint.accessType = IAT_VALUE;
#endif
    }
#if DEBUGGABLE_GENTREE
    GenTreeFptrVal() : GenTree()
    {
    }
#endif
};

struct GenTreeQmark : public GenTreeTernaryOp
{
    GenTreeQmark(var_types type, GenTree* condExpr, GenTree* thenExpr, GenTree* elseExpr)
        : GenTreeTernaryOp(GT_QMARK, type, condExpr, elseExpr, thenExpr)
    {
        assert(condExpr->TypeIs(TYP_INT));
    }

    GenTreeQmark(GenTreeQmark* copyFrom) : GenTreeTernaryOp(copyFrom)
    {
    }

    GenTree* GetCondition() const
    {
        return gtOp1;
    }

    void SetCondition(GenTree* condExpr)
    {
        assert(condExpr->TypeIs(TYP_INT));
        gtOp1 = condExpr;
    }

    GenTree* GetThen() const
    {
        return gtOp3;
    }

    void SetThen(GenTree* thenExpr)
    {
        gtOp3 = thenExpr;
    }

    GenTree* GetElse() const
    {
        return gtOp2;
    }

    void SetElse(GenTree* elseExpr)
    {
        gtOp2 = elseExpr;
    }

#if DEBUGGABLE_GENTREE
    GenTreeQmark() : GenTreeTernaryOp()
    {
    }
#endif
};

/* gtIntrinsic   -- intrinsic   (possibly-binary op [NULL op2 is allowed] with an additional field) */

struct GenTreeIntrinsic : public GenTreeOp
{
private:
    NamedIntrinsic m_intrinsicName;

public:
    CORINFO_METHOD_HANDLE gtMethodHandle; // Method handle of the method which is treated as an intrinsic.
#ifdef FEATURE_READYTORUN_COMPILER
    // Call target lookup info for method call from a Ready To Run module
    CORINFO_CONST_LOOKUP gtEntryPoint;
#endif

    GenTreeIntrinsic(var_types type, GenTree* op1, NamedIntrinsic intrinsicName, CORINFO_METHOD_HANDLE methodHandle)
        : GenTreeOp(GT_INTRINSIC, type, op1, nullptr), m_intrinsicName(intrinsicName), gtMethodHandle(methodHandle)
    {
        assert(intrinsicName != NI_Illegal);
    }

    GenTreeIntrinsic(
        var_types type, GenTree* op1, GenTree* op2, NamedIntrinsic intrinsicName, CORINFO_METHOD_HANDLE methodHandle)
        : GenTreeOp(GT_INTRINSIC, type, op1, op2), m_intrinsicName(intrinsicName), gtMethodHandle(methodHandle)
    {
        assert(intrinsicName != NI_Illegal);
    }

    GenTreeIntrinsic(const GenTreeIntrinsic* copyFrom)
        : GenTreeOp(copyFrom)
        , m_intrinsicName(copyFrom->m_intrinsicName)
        , gtMethodHandle(copyFrom->gtMethodHandle)
#ifdef FEATURE_READYTORUN_COMPILER
        , gtEntryPoint(copyFrom->gtEntryPoint)
#endif
    {
    }

    NamedIntrinsic GetIntrinsic() const
    {
        return m_intrinsicName;
    }

#if DEBUGGABLE_GENTREE
    GenTreeIntrinsic() : GenTreeOp()
    {
    }
#endif
};

class GenTreeUse
{
    friend struct GenTreeHWIntrinsic;
    friend struct GenTreeInstr;

    GenTree* m_node;

    GenTreeUse(GenTree* node = nullptr) : m_node(node)
    {
    }

    GenTreeUse(GenTreeUse&&)      = default;
    GenTreeUse(const GenTreeUse&) = delete;
    GenTreeUse& operator=(const GenTreeUse&) = delete;

public:
    GenTree*& NodeRef()
    {
        return m_node;
    }

    GenTree* GetNode() const
    {
        assert(m_node != nullptr);
        return m_node;
    }

    void SetNode(GenTree* node)
    {
        assert(node != nullptr);
        m_node = node;
    }
};

#ifdef FEATURE_HW_INTRINSICS

struct GenTreeHWIntrinsic : public GenTree
{
    using Use = GenTreeUse;

private:
    NamedIntrinsic m_intrinsic;
    var_types      m_simdBaseType;
    uint8_t        m_simdSize;
    var_types      m_auxiliaryType;
    uint8_t        m_numOps;
    union {
        Use  m_inlineUses[3];
        Use* m_uses;
    };

public:
    GenTreeHWIntrinsic(var_types type, NamedIntrinsic intrinsic, var_types baseType, unsigned size)
        : GenTree(GT_HWINTRINSIC, type)
        , m_intrinsic(intrinsic)
        , m_simdBaseType(baseType)
        , m_simdSize(static_cast<uint8_t>(size))
        , m_auxiliaryType(TYP_UNDEF)
        , m_numOps(0)
    {
        assert(size < UINT8_MAX);
    }

    GenTreeHWIntrinsic(var_types type, NamedIntrinsic intrinsic, var_types baseType, unsigned size, GenTree* op1)
        : GenTree(GT_HWINTRINSIC, type)
        , m_intrinsic(intrinsic)
        , m_simdBaseType(baseType)
        , m_simdSize(static_cast<uint8_t>(size))
        , m_auxiliaryType(TYP_UNDEF)
        , m_numOps(1)
        , m_inlineUses{op1}
    {
        assert(size < UINT8_MAX);

        if (OperIsMemoryStore())
        {
            gtFlags |= (GTF_GLOB_REF | GTF_ASG);
        }

        gtFlags |= op1->gtFlags & GTF_ALL_EFFECT;
    }

    GenTreeHWIntrinsic(
        var_types type, NamedIntrinsic intrinsic, var_types baseType, unsigned size, GenTree* op1, GenTree* op2)
        : GenTree(GT_HWINTRINSIC, type)
        , m_intrinsic(intrinsic)
        , m_simdBaseType(baseType)
        , m_simdSize(static_cast<uint8_t>(size))
        , m_auxiliaryType(TYP_UNDEF)
        , m_numOps(2)
        , m_inlineUses{op1, op2}
    {
        assert(size < UINT8_MAX);

        if (OperIsMemoryStore())
        {
            gtFlags |= (GTF_GLOB_REF | GTF_ASG);
        }

        gtFlags |= op1->gtFlags & GTF_ALL_EFFECT;
        gtFlags |= op2->gtFlags & GTF_ALL_EFFECT;
    }

    GenTreeHWIntrinsic(var_types      type,
                       NamedIntrinsic intrinsic,
                       var_types      baseType,
                       unsigned       size,
                       GenTree*       op1,
                       GenTree*       op2,
                       GenTree*       op3)
        : GenTree(GT_HWINTRINSIC, type)
        , m_intrinsic(intrinsic)
        , m_simdBaseType(baseType)
        , m_simdSize(static_cast<uint8_t>(size))
        , m_auxiliaryType(TYP_UNDEF)
        , m_numOps(3)
        , m_inlineUses{op1, op2, op3}
    {
        assert(size < UINT8_MAX);

        if (OperIsMemoryStore())
        {
            gtFlags |= (GTF_GLOB_REF | GTF_ASG);
        }

        gtFlags |= op1->gtFlags & GTF_ALL_EFFECT;
        gtFlags |= op2->gtFlags & GTF_ALL_EFFECT;
        gtFlags |= op3->gtFlags & GTF_ALL_EFFECT;
    }

    GenTreeHWIntrinsic(const GenTreeHWIntrinsic* copyFrom, CompAllocator alloc)
        : GenTree(GT_HWINTRINSIC, copyFrom->GetType())
        , m_intrinsic(copyFrom->m_intrinsic)
        , m_simdBaseType(copyFrom->m_simdBaseType)
        , m_simdSize(copyFrom->m_simdSize)
        , m_auxiliaryType(copyFrom->m_auxiliaryType)
        , m_numOps(0)
    {
        SetNumOps(copyFrom->GetNumOps(), alloc);

        for (unsigned i = 0; i < copyFrom->GetNumOps(); i++)
        {
            SetOp(i, copyFrom->GetOp(i));
            gtFlags |= GetOp(i)->GetSideEffects();
        }
    }

    NamedIntrinsic GetIntrinsic() const
    {
        return m_intrinsic;
    }

    void SetIntrinsic(NamedIntrinsic intrinsic)
    {
        assert(intrinsic != NI_Illegal);
        m_intrinsic = intrinsic;
    }

    void SetIntrinsic(NamedIntrinsic intrinsic, unsigned numOps)
    {
        SetIntrinsic(intrinsic);
        SetNumOps(numOps);
    }

    void SetIntrinsic(NamedIntrinsic intrinsic, var_types simdBaseType, unsigned numOps)
    {
        SetIntrinsic(intrinsic);
        SetSimdBaseType(simdBaseType);
        SetNumOps(numOps);
    }

    void SetIntrinsic(NamedIntrinsic intrinsic, var_types simdBaseType, unsigned simdSize, unsigned numOps)
    {
        SetIntrinsic(intrinsic);
        SetSimdBaseType(simdBaseType);
        SetSimdSize(simdSize);
        SetNumOps(numOps);
        SetAuxiliaryType(TYP_UNDEF);
    }

    var_types GetSimdBaseType() const
    {
        return m_simdBaseType;
    }

    void SetSimdBaseType(var_types type)
    {
        assert(varTypeIsIntegral(type) || varTypeIsFloating(type));
        m_simdBaseType = type;
    }

    unsigned GetSimdSize() const
    {
        return m_simdSize;
    }

    void SetSimdSize(unsigned size)
    {
        assert(size <= UINT8_MAX);
        m_simdSize = static_cast<uint8_t>(size);
    }

    bool IsSimd() const
    {
        return m_simdSize != 0;
    }

    unsigned GetNumOps() const
    {
        return m_numOps;
    }

    void SetNumOps(unsigned numOps)
    {
        assert(numOps <= _countof(m_inlineUses));

        m_numOps = static_cast<uint8_t>(numOps);

        assert(HasInlineUses());
        new (m_inlineUses) Use[numOps]();
    }

    void SetNumOps(unsigned numOps, CompAllocator alloc)
    {
        assert(numOps < UINT8_MAX);
        assert(m_numOps == 0);

        m_numOps = static_cast<uint8_t>(numOps);

        if (HasInlineUses())
        {
            new (m_inlineUses) Use[numOps]();
        }
        else
        {
            m_uses = new (alloc) Use[numOps]();
        }
    }

    bool IsUnary() const
    {
        return m_numOps == 1;
    }

    bool IsBinary() const
    {
        return m_numOps == 2;
    }

    bool IsTernary() const
    {
        return m_numOps == 3;
    }

    GenTree* GetOp(unsigned index) const
    {
        return GetUse(index).GetNode();
    }

    GenTree* GetLastOp() const
    {
        return m_numOps == 0 ? nullptr : GetOp(m_numOps - 1);
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

    var_types GetAuxiliaryType() const
    {
        return m_auxiliaryType;
    }

    void SetAuxiliaryType(var_types type)
    {
        m_auxiliaryType = type;
    }

    static bool Equals(GenTreeHWIntrinsic* simd1, GenTreeHWIntrinsic* simd2)
    {
        if ((simd1->TypeGet() != simd2->TypeGet()) || (simd1->m_intrinsic != simd2->m_intrinsic) ||
            (simd1->m_simdBaseType != simd2->m_simdBaseType) || (simd1->m_simdSize != simd2->m_simdSize) ||
            (simd1->m_auxiliaryType != simd2->m_auxiliaryType) || (simd1->m_numOps != simd2->m_numOps))
        {
            return false;
        }

        for (unsigned i = 0; i < simd1->m_numOps; i++)
        {
            if (!Compare(simd1->GetOp(i), simd2->GetOp(i)))
            {
                return false;
            }
        }

        return true;
    }

    bool OperIsMemoryLoad() const;  // Returns true for the HW Intrinsic instructions that have MemoryLoad semantics,
                                    // false otherwise
    bool OperIsMemoryStore() const; // Returns true for the HW Intrinsic instructions that have MemoryStore semantics,
                                    // false otherwise
    bool OperIsMemoryLoadOrStore() const; // Returns true for the HW Intrinsic instructions that have MemoryLoad or
                                          // MemoryStore semantics, false otherwise

    // Delete some functions inherited from GenTree to avoid accidental use, at least
    // when the node object is accessed via GenTreeHWIntrinsic* rather than GenTree*.
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

public:
#if DEBUGGABLE_GENTREE
    GenTreeHWIntrinsic() : GenTree()
    {
    }
#endif
};
#endif // FEATURE_HW_INTRINSICS

// Computes the address of an array element. Also checks if the array index is valid.
struct GenTreeIndexAddr : public GenTreeOp
{
private:
    BasicBlock* m_throwBlock;
    uint8_t     m_dataOffs;
    uint16_t    m_elemTypeNum;
    unsigned    m_elemSize;

public:
    GenTreeIndexAddr(GenTree* array, GenTree* index, uint8_t lenOffs, uint8_t dataOffs, var_types elemType)
        : GenTreeOp(GT_INDEX_ADDR, TYP_BYREF, array, index)
        , m_throwBlock(nullptr)
        , m_dataOffs(dataOffs)
        , m_elemTypeNum(static_cast<uint16_t>(elemType))
        , m_elemSize(varTypeSize(elemType))
    {
        // The offset of length is always the same for both strings and arrays.
        assert(lenOffs == TARGET_POINTER_SIZE);

        INDEBUG(if (!JitConfig.JitSkipArrayBoundCheck()))
        {
            gtFlags |= GTF_INX_RNGCHK | GTF_EXCEPT;
        }

        gtFlags |= array->GetSideEffects() | index->GetSideEffects();
    }

    GenTreeIndexAddr(const GenTreeIndexAddr* copyFrom)
        : GenTreeOp(GT_INDEX_ADDR, TYP_BYREF, copyFrom->gtOp1, copyFrom->gtOp2)
        , m_throwBlock(copyFrom->m_throwBlock)
        , m_dataOffs(copyFrom->m_dataOffs)
        , m_elemTypeNum(copyFrom->m_elemTypeNum)
        , m_elemSize(copyFrom->m_elemSize)
    {
    }

    GenTree* GetArray() const
    {
        return gtOp1;
    }

    void SetArray(GenTree* array)
    {
        assert(array->TypeIs(TYP_REF));
        gtOp1 = array;
    }

    GenTree* GetIndex() const
    {
        return gtOp2;
    }

    void SetIndex(GenTree* index)
    {
        assert(varTypeIsIntegral(index->GetType()));
        gtOp2 = index;
    }

    BasicBlock* GetThrowBlock() const
    {
        return m_throwBlock;
    }

    void SetThrowBlock(BasicBlock* block)
    {
        m_throwBlock = block;
    }

    uint8_t GetLenOffs() const
    {
        return TARGET_POINTER_SIZE;
    }

    uint8_t GetDataOffs() const
    {
        return m_dataOffs;
    }

    unsigned GetElemSize() const
    {
        return m_elemSize;
    }

    void SetElemSize(unsigned size)
    {
        m_elemSize = size;
    }

    unsigned GetElemTypeNum() const
    {
        return m_elemTypeNum;
    }

    void SetElemTypeNum(unsigned typeNum)
    {
        assert(typeNum <= UINT16_MAX);
        m_elemTypeNum = static_cast<uint16_t>(typeNum);
    }

    ClassLayout* GetLayout(Compiler* compiler) const;

#if DEBUGGABLE_GENTREE
    GenTreeIndexAddr() : GenTreeOp()
    {
    }
#endif
};

struct GenTreeArrLen : public GenTreeUnOp
{
    static_assert_no_msg(GTF_ARRLEN_NONFAULTING == GTF_IND_NONFAULTING);

    GenTreeArrLen(GenTree* arr, uint8_t lenOffs) : GenTreeUnOp(GT_ARR_LENGTH, TYP_INT, arr)
    {
        // The offset of length is always the same for both strings and arrays.
        assert(lenOffs == TARGET_POINTER_SIZE);
    }

    GenTreeArrLen(const GenTreeArrLen* copyFrom) : GenTreeUnOp(GT_ARR_LENGTH, copyFrom->GetType(), copyFrom->GetArray())
    {
    }

    GenTree* GetArray() const
    {
        return gtOp1;
    }

    uint8_t GetLenOffs() const
    {
        return TARGET_POINTER_SIZE;
    }

#if DEBUGGABLE_GENTREE
    GenTreeArrLen() : GenTreeUnOp()
    {
    }
#endif
};

struct GenTreeBoundsChk : public GenTreeOp
{
    BasicBlock*     m_throwBlock;
    ThrowHelperKind m_throwKind;

    GenTreeBoundsChk(GenTree* index, GenTree* length, ThrowHelperKind kind)
        : GenTreeOp(GT_BOUNDS_CHECK, TYP_VOID, index, length), m_throwBlock(nullptr), m_throwKind(kind)
    {
        gtFlags |= GTF_EXCEPT | index->GetSideEffects() | length->GetSideEffects();
    }

    GenTreeBoundsChk(const GenTreeBoundsChk* copyFrom)
        : GenTreeOp(copyFrom), m_throwBlock(copyFrom->m_throwBlock), m_throwKind(copyFrom->m_throwKind)
    {
    }

    GenTree* GetIndex() const
    {
        return gtOp1;
    }

    void SetIndex(GenTree* index)
    {
        assert(varTypeIsIntegral(index->GetType()));
        gtOp1 = index;
    }

    GenTree* GetLength() const
    {
        return gtOp2;
    }

    void SetLength(GenTree* length)
    {
        assert(varTypeIsIntegral(length->GetType()));
        gtOp2 = length;
    }

    GenTree* GetArray() const
    {
        return gtOp2->IsArrLen() ? gtOp2->AsArrLen()->GetArray() : nullptr;
    }

    ThrowHelperKind GetThrowKind() const
    {
        return m_throwKind;
    }

    BasicBlock* GetThrowBlock() const
    {
        return m_throwBlock;
    }

    void SetThrowBlock(BasicBlock* block)
    {
        m_throwBlock = block;
    }

    static bool Equals(const GenTreeBoundsChk* c1, const GenTreeBoundsChk* c2)
    {
        return (c1->GetOper() == c2->GetOper()) && (c1->m_throwKind == c2->m_throwKind) &&
               Compare(c1->gtOp1, c2->gtOp1) && Compare(c1->gtOp2, c2->gtOp2);
    }

#if DEBUGGABLE_GENTREE
    GenTreeBoundsChk() = default;
#endif
};

struct GenTreeArrElem : public GenTree
{
    static constexpr unsigned MaxRank     = 3;
    static constexpr unsigned MaxElemSize = UINT8_MAX;

    GenTree*  gtArrObj;
    GenTree*  gtArrInds[MaxRank];
    uint8_t   gtArrRank;
    uint8_t   gtArrElemSize;
    var_types gtArrElemType;

    GenTreeArrElem(var_types type, GenTree* arr, unsigned rank, unsigned elemSize, var_types elemType, GenTree** inds)
        : GenTree(GT_ARR_ELEM, type)
        , gtArrObj(arr)
        , gtArrRank(static_cast<uint8_t>(rank))
        , gtArrElemSize(static_cast<uint8_t>(elemSize))
        , gtArrElemType(elemType)
    {
        assert(rank <= MaxRank);
        assert(elemSize <= MaxElemSize);

        gtFlags |= (arr->gtFlags & GTF_ALL_EFFECT);
        for (unsigned i = 0; i < rank; i++)
        {
            gtArrInds[i] = inds[i];
            gtFlags |= (inds[i]->gtFlags & GTF_ALL_EFFECT);
        }
        gtFlags |= GTF_EXCEPT;
    }

    unsigned GetRank() const
    {
        return static_cast<unsigned>(gtArrRank);
    }

    GenTree* GetArray() const
    {
        return gtArrObj;
    }

    GenTree* GetIndex(unsigned i) const
    {
        assert(i < gtArrRank);
        return gtArrInds[i];
    }

    unsigned GetNumOps() const
    {
        return 1 + gtArrRank;
    }

    GenTree* GetOp(unsigned i) const
    {
        assert(i <= gtArrRank);
        return i == 0 ? gtArrObj : gtArrInds[i - 1];
    }

#if DEBUGGABLE_GENTREE
    GenTreeArrElem() = default;
#endif
};

//--------------------------------------------
//
// GenTreeArrIndex (gtArrIndex): Expression to bounds-check the index for one dimension of a
//    multi-dimensional or non-zero-based array., and compute the effective index
//    (i.e. subtracting the lower bound).
//
// Notes:
//    This node is similar in some ways to GenTreeBoundsChk, which ONLY performs the check.
//    The reason that this node incorporates the check into the effective index computation is
//    to avoid duplicating the codegen, as the effective index is required to compute the
//    offset anyway.
//    TODO-CQ: Enable optimization of the lower bound and length by replacing this:
//                /--*  <arrObj>
//                +--*  <index0>
//             +--* ArrIndex[i, ]
//    with something like:
//                   /--*  <arrObj>
//                /--*  ArrLowerBound[i, ]
//                |  /--*  <arrObj>
//                +--*  ArrLen[i, ]    (either generalize GT_ARR_LENGTH or add a new node)
//                +--*  <index0>
//             +--* ArrIndex[i, ]
//    Which could, for example, be optimized to the following when known to be within bounds:
//                /--*  TempForLowerBoundDim0
//                +--*  <index0>
//             +--* - (GT_SUB)
//
struct GenTreeArrIndex : public GenTreeOp
{
    uint8_t   gtCurrDim;
    uint8_t   gtArrRank;
    var_types gtArrElemType;

    GenTreeArrIndex(
        var_types type, GenTree* arrObj, GenTree* indexExpr, unsigned currDim, unsigned arrRank, var_types elemType)
        : GenTreeOp(GT_ARR_INDEX, type, arrObj, indexExpr)
        , gtCurrDim(static_cast<uint8_t>(currDim))
        , gtArrRank(static_cast<uint8_t>(arrRank))
        , gtArrElemType(elemType)
    {
        gtFlags |= GTF_EXCEPT;
    }

    GenTree*& ArrObj()
    {
        return gtOp1;
    }

    GenTree*& IndexExpr()
    {
        return gtOp2;
    }

#if DEBUGGABLE_GENTREE
    GenTreeArrIndex() = default;
#endif
};

//--------------------------------------------
//
// GenTreeArrOffset (gtArrOffset): Expression to compute the accumulated offset for the address
//    of an element of a multi-dimensional or non-zero-based array.
//
// Notes:
//    The result of this expression is (gtOffset * dimSize) + gtIndex
//    where dimSize is the length/stride/size of the dimension, and is obtained from gtArrObj.
//    This node is generated in conjunction with the GenTreeArrIndex node, which computes the
//    effective index for a single dimension.  The sub-trees can be separately optimized, e.g.
//    within a loop body where the expression for the 0th dimension may be invariant.
//
//    Here is an example of how the tree might look for a two-dimension array reference:
//                /--*  const 0
//                |  /--* <arrObj>
//                |  +--* <index0>
//                +--* ArrIndex[i, ]
//                +--*  <arrObj>
//             /--| arrOffs[i, ]
//             |  +--*  <arrObj>
//             |  +--*  <index1>
//             +--* ArrIndex[*,j]
//             +--*  <arrObj>
//          /--| arrOffs[*,j]
//    TODO-CQ: see comment on GenTreeArrIndex for how its representation may change.  When that
//    is done, we will also want to replace the <arrObj> argument to arrOffs with the
//    ArrLen as for GenTreeArrIndex.
//
struct GenTreeArrOffs : public GenTreeTernaryOp
{
    uint8_t   gtCurrDim;
    uint8_t   gtArrRank;
    var_types gtArrElemType;

    GenTreeArrOffs(var_types type,
                   GenTree*  offset,
                   GenTree*  index,
                   GenTree*  arrObj,
                   unsigned  currDim,
                   unsigned  rank,
                   var_types elemType)
        : GenTreeTernaryOp(GT_ARR_OFFSET, type, offset, index, arrObj)
        , gtCurrDim(static_cast<uint8_t>(currDim))
        , gtArrRank(static_cast<uint8_t>(rank))
        , gtArrElemType(elemType)
    {
        assert(index->gtFlags & GTF_EXCEPT);
        gtFlags |= GTF_EXCEPT;
    }

    GenTreeArrOffs(GenTreeArrOffs* copyFrom)
        : GenTreeTernaryOp(copyFrom)
        , gtCurrDim(copyFrom->gtCurrDim)
        , gtArrRank(copyFrom->gtArrRank)
        , gtArrElemType(copyFrom->gtArrElemType)
    {
    }

    // The accumulated offset for lower dimensions
    GenTree* GetOffset() const
    {
        return gtOp1;
    }

    // The effective index for the current dimension
    GenTree* GetIndex() const
    {
        return gtOp2;
    }

    // The array object reference
    GenTree* GetArray() const
    {
        return gtOp3;
    }

#if DEBUGGABLE_GENTREE
    GenTreeArrOffs() = default;
#endif
};

struct GenTreeAddrMode : public GenTreeOp
{
private:
    unsigned m_scale;
    int      m_offset;

public:
    // Address is Base + Index*Scale + Offset.
    // These are the legal patterns:
    //
    //      Base                                // Base != nullptr && Index == nullptr && Scale == 0 && Offset == 0
    //      Base + Index*Scale                  // Base != nullptr && Index != nullptr && Scale != 0 && Offset == 0
    //      Base + Offset                       // Base != nullptr && Index == nullptr && Scale == 0 && Offset != 0
    //      Base + Index*Scale + Offset         // Base != nullptr && Index != nullptr && Scale != 0 && Offset != 0
    //             Index*Scale                  // Base == nullptr && Index != nullptr && Scale >  1 && Offset == 0
    //             Index*Scale + Offset         // Base == nullptr && Index != nullptr && Scale >  1 && Offset != 0
    //                           Offset         // Base == nullptr && Index == nullptr && Scale == 0 && Offset != 0
    //
    // So, for example:
    //      1. Base + Index is legal with Scale==1
    //      2. If Index is null, Scale should be zero (or unintialized / unused)
    //      3. If Scale==1, then we should have "Base" instead of "Index*Scale", and "Base + Offset" instead of
    //         "Index*Scale + Offset".

    GenTreeAddrMode(GenTree* base, int32_t offset)
        : GenTreeOp(GT_LEA, varTypeAddrAdd(base->GetType()), base, nullptr), m_scale(0), m_offset(offset)
    {
        assert(base != nullptr);
    }

    GenTreeAddrMode(var_types type, GenTree* base, GenTree* index, unsigned scale, int32_t offset)
        : GenTreeOp(GT_LEA, type, base, index), m_scale(scale), m_offset(offset)
    {
        assert((base != nullptr) || (index != nullptr));
    }

    GenTreeAddrMode(const GenTreeAddrMode* copyFrom)
        : GenTreeOp(GT_LEA, copyFrom->GetType(), copyFrom->GetBase(), copyFrom->GetIndex())
        , m_scale(copyFrom->m_scale)
        , m_offset(copyFrom->m_offset)
    {
    }

    bool HasBase() const
    {
        return gtOp1 != nullptr;
    }

    GenTree* GetBase() const
    {
        return gtOp1;
    }

    void SetBase(GenTree* base)
    {
        gtOp1 = base;
    }

    bool HasIndex() const
    {
        return gtOp2 != nullptr;
    }

    GenTree* GetIndex() const
    {
        return gtOp2;
    }

    void SetIndex(GenTree* index)
    {
        gtOp2 = index;
    }

    unsigned GetScale() const
    {
        return m_scale;
    }

    void SetScale(unsigned scale)
    {
        m_scale = scale;
    }

    int32_t GetOffset() const
    {
        return m_offset;
    }

    void SetOffset(int32_t offset)
    {
        m_offset = offset;
    }

#if DEBUGGABLE_GENTREE
    GenTreeAddrMode() : GenTreeOp()
    {
    }
#endif
};

// Indir is just an op, no additional data, but some additional abstractions
struct GenTreeIndir : public GenTreeOp
{
    GenTree*& Addr()
    {
        return gtOp1;
    }

    GenTree* GetAddr() const
    {
        return gtOp1;
    }

    void SetAddr(GenTree* addr)
    {
        assert(addr != nullptr);
        assert(varTypeIsI(addr->GetType()));
        gtOp1 = addr;
    }

    GenTree* GetValue() const
    {
        return gtOp2;
    }

    void SetValue(GenTree* value)
    {
        assert(OperIs(GT_STOREIND, GT_STORE_OBJ, GT_STORE_BLK));
        assert(value != nullptr);
        gtOp2 = value;
    }

    GenTreeIndir(genTreeOps oper, var_types type, GenTree* addr, GenTree* value = nullptr)
        : GenTreeOp(oper, type, addr, value)
    {
    }

    GenTreeIndir(GenTreeIndir* copyFrom) : GenTreeOp(copyFrom)
    {
    }

    // True if this indirection is a volatile memory operation.
    bool IsVolatile() const
    {
        return (gtFlags & GTF_IND_VOLATILE) != 0;
    }

    void SetVolatile()
    {
        gtFlags |= GTF_IND_VOLATILE;
    }

    // True if this indirection is an unaligned memory operation.
    bool IsUnaligned() const
    {
        return (gtFlags & GTF_IND_UNALIGNED) != 0;
    }

    void SetUnaligned()
    {
        gtFlags |= GTF_IND_UNALIGNED;
    }

    bool IsInvariant() const
    {
        return (gtFlags & GTF_IND_INVARIANT) != 0;
    }

#if DEBUGGABLE_GENTREE
    GenTreeIndir() : GenTreeOp()
    {
    }
#endif
};

enum class StructStoreKind : uint8_t
{
    Invalid,
#ifndef TARGET_X86
    MemSet,
    MemCpy,
#endif
#ifdef TARGET_XARCH
    RepStos,
    RepMovs,
#endif
    UnrollInit,
    UnrollCopy,
    UnrollCopyWB,
#ifdef TARGET_XARCH
    UnrollCopyWBRepMovs,
#endif
    UnrollRegs,
#if defined(UNIX_AMD64_ABI) || defined(TARGET_ARM64)
    UnrollRegsWB,
#endif

#ifdef TARGET_X86
    // TODO-X86-CQ: Investigate whether a helper call would be beneficial on x86
    LargeInit = RepStos,
    LargeCopy = RepMovs,
#else
    LargeInit = MemSet,
    LargeCopy = MemCpy,
#endif
};

// This is the base type for all of the nodes that represent block or struct values.
// Since it can be a store, it includes gtBlkOpKind to specify the type of code
// generation that will be used for the block operation.

struct GenTreeBlk : public GenTreeIndir
{
private:
    ClassLayout*    m_layout;
    StructStoreKind m_kind;

protected:
    GenTreeBlk(genTreeOps oper, var_types type, GenTree* addr, ClassLayout* layout)
        : GenTreeIndir(oper, type, addr, nullptr), m_layout(layout), m_kind(StructStoreKind::Invalid)
    {
        assert((oper == GT_OBJ) || (oper == GT_BLK));
        assert(layout != nullptr);
    }

    GenTreeBlk(genTreeOps oper, var_types type, GenTree* addr, GenTree* value, ClassLayout* layout)
        : GenTreeIndir(oper, type, addr, value), m_layout(layout), m_kind(StructStoreKind::Invalid)
    {
        assert((oper == GT_STORE_OBJ) || (oper == GT_STORE_BLK));
        assert(layout != nullptr);
    }

public:
    GenTreeBlk(GenTree* addr, ClassLayout* layout)
        : GenTreeIndir(GT_BLK, TYP_STRUCT, addr, nullptr), m_layout(layout), m_kind(StructStoreKind::Invalid)
    {
        assert(layout->IsBlockLayout());

        gtFlags |= GTF_GLOB_REF | GTF_EXCEPT;
    }

    GenTreeBlk(GenTreeBlk* copyFrom)
        : GenTreeIndir(copyFrom), m_layout(copyFrom->m_layout), m_kind(StructStoreKind::Invalid)
    {
    }

    ClassLayout* GetLayout() const
    {
        return m_layout;
    }

    void SetLayout(ClassLayout* layout)
    {
        assert(layout != nullptr);
        m_layout = layout;
    }

    StructStoreKind GetKind() const
    {
        return m_kind;
    }

    void SetKind(StructStoreKind kind)
    {
        m_kind = kind;
    }

#if DEBUGGABLE_GENTREE
    GenTreeBlk() : GenTreeIndir()
    {
    }
#endif
};

struct GenTreeObj : public GenTreeBlk
{
    GenTreeObj(var_types type, GenTree* addr, ClassLayout* layout) : GenTreeBlk(GT_OBJ, type, addr, layout)
    {
        assert(!layout->IsBlockLayout());

        // By default, indirs are assumed to access aliased memory.
        gtFlags |= GTF_GLOB_REF;
    }

    GenTreeObj(var_types type, GenTree* addr, GenTree* value, ClassLayout* layout)
        : GenTreeBlk(GT_STORE_OBJ, type, addr, value, layout)
    {
        assert(!layout->IsBlockLayout());

        // By default, indirs are assumed to access aliased memory.
        gtFlags |= GTF_ASG | GTF_GLOB_REF;
    }

    GenTreeObj(GenTreeObj* copyFrom) : GenTreeBlk(copyFrom)
    {
    }

#if DEBUGGABLE_GENTREE
    GenTreeObj() : GenTreeBlk()
    {
    }
#endif
};

// This node is used for block values that have a dynamic size.
// Note that such a value can never have GC pointers.

struct GenTreeDynBlk : public GenTreeTernaryOp
{
    GenTreeDynBlk(genTreeOps oper, GenTree* addr, GenTree* value, GenTree* size)
        : GenTreeTernaryOp(oper, TYP_VOID, addr, value, size)
    {
        assert((oper == GT_COPY_BLK) || (oper == GT_INIT_BLK));
        assert(varTypeIsIntegralOrI(addr->GetType()));
        assert(oper == GT_COPY_BLK ? varTypeIsIntegralOrI(value->GetType()) : varTypeIsIntegral(value->GetType()));
        assert(varTypeIsIntegral(size->GetType()));

        gtFlags |= GTF_ASG | GTF_EXCEPT | GTF_GLOB_REF;
    }

    GenTreeDynBlk(GenTreeDynBlk* copyFrom) : GenTreeTernaryOp(copyFrom)
    {
    }

    GenTree* GetAddr() const
    {
        return gtOp1;
    }

    void SetAddr(GenTree* addr)
    {
        assert(varTypeIsIntOrI(addr->GetType()));
        gtOp1 = addr;
    }

    GenTree* GetValue() const
    {
        return gtOp2;
    }

    void SetValue(GenTree* value)
    {
        assert(varTypeIsIntOrI(value->GetType()));
        gtOp2 = value;
    }

    GenTree* GetSize() const
    {
        return gtOp3;
    }

    void SetSize(GenTree* size)
    {
        assert(varTypeIsIntegral(size->GetType()));
        gtOp3 = size;
    }

    bool IsVolatile() const
    {
        return (gtFlags & GTF_IND_VOLATILE) != 0;
    }

    void SetVolatile(bool isVolatile)
    {
        gtFlags = isVolatile ? (gtFlags | GTF_IND_VOLATILE) : (gtFlags & ~GTF_IND_VOLATILE);
    }

    bool IsUnaligned() const
    {
        return (gtFlags & GTF_IND_UNALIGNED) != 0;
    }

    void SetUnaligned(bool isUnaligned)
    {
        gtFlags = isUnaligned ? (gtFlags | GTF_IND_UNALIGNED) : (gtFlags & ~GTF_IND_UNALIGNED);
    }

    StructStoreKind GetKind() const
    {
#ifdef TARGET_X86
        return gtOper == GT_INIT_BLK ? StructStoreKind::RepStos : StructStoreKind::RepMovs;
#else
        return gtOper == GT_INIT_BLK ? StructStoreKind::MemSet : StructStoreKind::MemCpy;
#endif
    }

#if DEBUGGABLE_GENTREE
    GenTreeDynBlk() : GenTreeTernaryOp()
    {
    }
#endif
};

struct GenTreeStoreInd : public GenTreeIndir
{
    GenTreeStoreInd(var_types type, GenTree* addr, GenTree* value) : GenTreeIndir(GT_STOREIND, type, addr, value)
    {
    }

#if DEBUGGABLE_GENTREE
    GenTreeStoreInd() : GenTreeIndir()
    {
    }
#endif
};

/* gtRetExp -- Place holder for the return expression from an inline candidate (GT_RET_EXPR) */

struct GenTreeRetExpr : public GenTree
{
private:
    GenTreeCall*    m_call;
    GenTree*        m_retExpr;
    BasicBlockFlags m_retBlockIRSummary;

public:
    GenTreeRetExpr(GenTreeCall* call);

    GenTreeCall* GetCall() const
    {
        return m_call;
    }

    ClassLayout* GetLayout() const
    {
        return m_call->GetRetLayout();
    }

    GenTree* GetRetExpr() const
    {
        return m_retExpr;
    }

    BasicBlockFlags GetRetBlockIRSummary() const
    {
        return m_retBlockIRSummary;
    }

    void SetRetExpr(GenTree* expr)
    {
        assert(expr != nullptr);
        m_retExpr = expr;
    }

    void SetRetExpr(GenTree* expr, BasicBlockFlags blockIRSummary)
    {
        assert(expr != nullptr);
        m_retExpr           = expr;
        m_retBlockIRSummary = blockIRSummary;
    }

#if DEBUGGABLE_GENTREE
    GenTreeRetExpr() : GenTree()
    {
    }
#endif
};

class InlineContext;

struct GenTreeILOffset : public GenTree
{
    IL_OFFSETX gtStmtILoffsx; // instr offset (if available)

    GenTreeILOffset(IL_OFFSETX offset) : GenTree(GT_IL_OFFSET, TYP_VOID), gtStmtILoffsx(offset)
    {
    }

#if DEBUGGABLE_GENTREE
    GenTreeILOffset() : GenTree(GT_IL_OFFSET, TYP_VOID)
    {
    }
#endif
};

// GenTreeList: adapter class for forward iteration of the execution order GenTree linked list
// using range-based `for`, normally used via Statement::TreeList(), e.g.:
//    for (GenTree* const tree : stmt->TreeList()) ...
//
class GenTreeList
{
    GenTree* m_trees;

    // Forward iterator for the execution order GenTree linked list (using `gtNext` pointer).
    //
    class iterator
    {
        GenTree* m_tree;

    public:
        iterator(GenTree* tree) : m_tree(tree)
        {
        }

        GenTree* operator*() const
        {
            return m_tree;
        }

        iterator& operator++()
        {
            m_tree = m_tree->gtNext;
            return *this;
        }

        bool operator!=(const iterator& i) const
        {
            return m_tree != i.m_tree;
        }
    };

public:
    GenTreeList(GenTree* trees) : m_trees(trees)
    {
    }

    iterator begin() const
    {
        return iterator(m_trees);
    }

    iterator end() const
    {
        return iterator(nullptr);
    }
};

// We use the following format when printing the Statement number: Statement->GetID()
// This define is used with string concatenation to put this in printf format strings  (Note that %u means unsigned int)
#define FMT_STMT "STMT%05u"

struct Statement
{
public:
    Statement(GenTree* expr, IL_OFFSETX offset DEBUGARG(unsigned stmtID))
        : m_rootNode(expr)
        , m_treeList(nullptr)
        , m_next(nullptr)
        , m_prev(nullptr)
        , m_inlineContext(nullptr)
        , m_ILOffsetX(offset)
#ifdef DEBUG
        , m_stmtID(stmtID)
#endif
        , m_compilerAdded(false)
    {
        assert(expr != nullptr);
    }

    GenTree* GetRootNode() const
    {
        return m_rootNode;
    }

    GenTree** GetRootNodePointer()
    {
        return &m_rootNode;
    }

    void SetRootNode(GenTree* treeRoot)
    {
        m_rootNode = treeRoot;
    }

    // [[deprecated]]
    GenTree* GetTreeList() const
    {
        return m_treeList;
    }

    GenTree* GetNodeList() const
    {
        return m_treeList;
    }

    void SetNodeList(GenTree* list)
    {
        m_treeList = list;
    }

    // [[deprecated]]
    void SetTreeList(GenTree* treeHead)
    {
        m_treeList = treeHead;
    }

    // [[deprecated]]
    GenTreeList TreeList() const
    {
        return GenTreeList(GetTreeList());
    }

    GenTreeList Nodes() const
    {
        return GenTreeList(m_treeList);
    }

    InlineContext* GetInlineContext() const
    {
        return m_inlineContext;
    }

    void SetInlineContext(InlineContext* inlineContext)
    {
        m_inlineContext = inlineContext;
    }

    IL_OFFSETX GetILOffsetX() const
    {
        return m_ILOffsetX;
    }

    void SetILOffsetX(IL_OFFSETX offsetX)
    {
        m_ILOffsetX = offsetX;
    }

#ifdef DEBUG
    unsigned GetID() const
    {
        return m_stmtID;
    }
#endif

    Statement* GetNextStmt() const
    {
        return m_next;
    }

    void SetNextStmt(Statement* nextStmt)
    {
        m_next = nextStmt;
    }

    Statement* GetPrevStmt() const
    {
        return m_prev;
    }

    void SetPrevStmt(Statement* prevStmt)
    {
        m_prev = prevStmt;
    }

    bool IsCompilerAdded() const
    {
        return m_compilerAdded;
    }

    void SetCompilerAdded()
    {
        m_compilerAdded = true;
    }

    unsigned GetCostSz() const
    {
        return m_rootNode->GetCostSz();
    }

    unsigned GetCostEx() const
    {
        return m_rootNode->GetCostEx();
    }

private:
    // The root of the expression tree.
    // Note: It will be the last node in evaluation order.
    GenTree* m_rootNode;

    // The tree list head (for forward walks in evaluation order).
    // The value is `nullptr` until we have set the sequencing of the nodes.
    GenTree* m_treeList;

    // The statement nodes are doubly-linked. The first statement node in a block points
    // to the last node in the block via its `m_prev` link. Note that the last statement node
    // does not point to the first: it's `m_next == nullptr`; that is, the list is not fully circular.
    Statement* m_next;
    Statement* m_prev;

    InlineContext* m_inlineContext; // The inline context for this statement.

    IL_OFFSETX m_ILOffsetX; // The instr offset (if available).

    INDEBUG(unsigned m_stmtID;)

    bool m_compilerAdded; // Was the statement created by optimizer?
};

// StatementList: adapter class for forward iteration of the statement linked list using range-based `for`,
// normally used via BasicBlock::Statements(), e.g.:
//    for (Statement* const stmt : block->Statements()) ...
// or:
//    for (Statement* const stmt : block->NonPhiStatements()) ...
//
class StatementList
{
    Statement* m_stmts;

    // Forward iterator for the statement linked list.
    //
    class iterator
    {
        Statement* m_stmt;

    public:
        iterator(Statement* stmt) : m_stmt(stmt)
        {
        }

        Statement* operator*() const
        {
            return m_stmt;
        }

        iterator& operator++()
        {
            m_stmt = m_stmt->GetNextStmt();
            return *this;
        }

        bool operator!=(const iterator& i) const
        {
            return m_stmt != i.m_stmt;
        }
    };

public:
    StatementList(Statement* stmts) : m_stmts(stmts)
    {
    }

    iterator begin() const
    {
        return iterator(m_stmts);
    }

    iterator end() const
    {
        return iterator(nullptr);
    }
};

struct GenTreeClsVar : public GenTree
{
    CORINFO_FIELD_HANDLE gtClsVarHnd;

private:
    FieldSeqNode* m_fieldSeq;

public:
    GenTreeClsVar(CORINFO_FIELD_HANDLE fieldHandle, FieldSeqNode* fieldSeq = nullptr)
        : GenTree(GT_CLS_VAR_ADDR, TYP_I_IMPL), gtClsVarHnd(fieldHandle), m_fieldSeq(fieldSeq)
    {
    }

    GenTreeClsVar(const GenTreeClsVar* copyFrom)
        : GenTree(copyFrom->GetOper(), copyFrom->GetType())
        , gtClsVarHnd(copyFrom->gtClsVarHnd)
        , m_fieldSeq(copyFrom->m_fieldSeq)
    {
    }

    CORINFO_FIELD_HANDLE GetFieldHandle() const
    {
        return gtClsVarHnd;
    }

    FieldSeqNode* GetFieldSeq() const
    {
        assert((m_fieldSeq == nullptr) || (m_fieldSeq->GetFieldHandle() == gtClsVarHnd));
        return m_fieldSeq;
    }

    void SetFieldHandle(CORINFO_FIELD_HANDLE fieldHandle, FieldSeqNode* fieldSeq)
    {
        // TODO-MIKE-Consider: The field sequence is pretty much pointless since it should
        // always be a singleton for the field handle we already have. Storing it in the
        // node only avoids a hashtable lookup in those not so many places that care about
        // field sequences.

        assert(fieldHandle != nullptr);
        assert(fieldSeq->GetFieldHandle() == fieldHandle);
        assert(fieldSeq->GetNext() == nullptr);

        gtClsVarHnd = fieldHandle;
        m_fieldSeq  = fieldSeq;
    }

#if DEBUGGABLE_GENTREE
    GenTreeClsVar() : GenTree()
    {
    }
#endif
};

// Argument passed on stack (GT_PUTARG_STK)

struct GenTreePutArgStk : public GenTreeUnOp
{
#ifdef TARGET_XARCH
    // Instruction selection: during codegen time, what code sequence we will be using
    // to encode this operation.
    // TODO-Throughput: The following information should be obtained from the child
    // block node.
    enum class Kind : uint8_t{Invalid,    RepInstr,     RepInstrZero, Unroll,
                              UnrollZero, RepInstrXMM,  GCUnroll,     GCUnrollXMM,
#ifdef TARGET_X86
                              Push,       PushAllSlots, PushZero
#endif
    };
#endif // TARGET_XARCH

private:
    CallArgInfo* m_argInfo;
#if defined(DEBUG) || defined(UNIX_X86_ABI)
    GenTreeCall* m_call; // the call node to which this argument belongs
#endif
#if FEATURE_FASTTAILCALL
    // Whether this arg needs to be placed in incoming arg area.
    // By default this is false and will be placed in out-going arg area.
    // Fast tail calls set this to true.
    bool m_putInIncomingArgArea;
#endif
#ifdef TARGET_XARCH
    Kind m_kind;
#endif

public:
    GenTreePutArgStk(GenTree* arg, CallArgInfo* argInfo, GenTreeCall* call, genTreeOps oper = GT_PUTARG_STK)
        : GenTreeUnOp(oper, TYP_VOID, arg)
        , m_argInfo(argInfo)
#if defined(DEBUG) || defined(UNIX_X86_ABI)
        , m_call(call)
#endif
#if FEATURE_FASTTAILCALL
        , m_putInIncomingArgArea(call->IsFastTailCall())
#endif
#ifdef TARGET_XARCH
        , m_kind(Kind::Invalid)
#endif
    {
#if defined(TARGET_AMD64) && defined(TARGET_WINDOWS)
        assert(argInfo->GetSlotCount() == 1);
#endif
    }

    CallArgInfo* GetArgInfo() const
    {
        return m_argInfo;
    }

#if defined(DEBUG) || defined(UNIX_X86_ABI)
    GenTreeCall* GetCall() const
    {
        return m_call;
    }
#endif

#if FEATURE_FASTTAILCALL
    bool PutInIncomingArgArea() const
    {
        return m_putInIncomingArgArea;
    }
#endif

    unsigned GetSlotNum() const
    {
        return m_argInfo->GetSlotCount();
    }

    unsigned GetSlotOffset() const
    {
        return m_argInfo->GetSlotNum() * REGSIZE_BYTES;
    }

    unsigned GetSlotCount() const
    {
#if !(defined(TARGET_AMD64) && defined(TARGET_WINDOWS))
        return m_argInfo->GetSlotCount();
#else
        return 1;
#endif
    }

    unsigned GetArgSize() const
    {
        return GetSlotCount() * REGSIZE_BYTES;
    }

#if defined(FEATURE_SIMD) && defined(TARGET_X86)
    // Return true if this is a PutArgStk of a SIMD12 struct.
    // This is needed because such values are re-typed to SIMD16, and the type of PutArgStk is VOID.
    unsigned IsSIMD12() const
    {
        return varTypeIsSIMD(gtOp1->GetType()) && (m_argInfo->GetSlotCount() == 3);
    }
#endif

#ifdef TARGET_XARCH
    Kind GetKind() const
    {
        return m_kind;
    }

    void SetKind(Kind kind)
    {
        m_kind = kind;
    }
#endif

#if DEBUGGABLE_GENTREE
    GenTreePutArgStk() : GenTreeUnOp()
    {
    }
#endif
};

#if FEATURE_ARG_SPLIT
// Represent the struct argument: split value in register(s) and stack
struct GenTreePutArgSplit : public GenTreePutArgStk
{
private:
#if defined(TARGET_ARM)
    constexpr static unsigned MAX_SPLIT_ARG_REGS = MAX_ARG_REG_COUNT;
#elif defined(TARGET_ARM64)
    constexpr static unsigned MAX_SPLIT_ARG_REGS = 1;
#else
#error Unknown FEATURE_ARG_SPLIT target.
#endif
    var_types m_regType[MAX_SPLIT_ARG_REGS];

public:
    GenTreePutArgSplit(GenTree* arg, CallArgInfo* argInfo, GenTreeCall* call)
        : GenTreePutArgStk(arg, argInfo, call, GT_PUTARG_SPLIT)
    {
        assert((0 < argInfo->GetRegCount()) && (argInfo->GetRegCount() <= MAX_SPLIT_ARG_REGS));

#ifdef TARGET_ARM
        SetType(argInfo->GetRegCount() > 1 ? TYP_STRUCT : varActualType(argInfo->GetRegType(0)));
        ClearOtherRegs();
#else
        assert(argInfo->GetSlotCount() == 1);
        SetType(varActualType(argInfo->GetRegType(0)));
#endif
    }

    unsigned GetRegCount() const
    {
#ifdef TARGET_ARM
        return GetArgInfo()->GetRegCount();
#else
        return 1;
#endif
    }

    var_types GetRegType(unsigned index) const
    {
        assert(index < GetRegCount());
#ifdef TARGET_ARM
        return m_regType[index];
#else
        return m_regType[0];
#endif
    }

    void SetRegType(unsigned index, var_types type)
    {
        assert(index < GetRegCount());
#ifdef TARGET_ARM
        m_regType[index] = type;
#else
        m_regType[0] = type;
#endif
    }

    unsigned GetArgSize() const
    {
#ifdef TARGET_ARM
        return (GetSlotCount() + GetRegCount()) * REGSIZE_BYTES;
#else
        return 2 * REGSIZE_BYTES;
#endif
    }

#if DEBUGGABLE_GENTREE
    GenTreePutArgSplit() : GenTreePutArgStk()
    {
    }
#endif
};
#endif // FEATURE_ARG_SPLIT

struct GenTreeCopyOrReload : public GenTreeUnOp
{
    GenTreeCopyOrReload(genTreeOps oper, var_types type, GenTree* op1) : GenTreeUnOp(oper, type, op1)
    {
        assert(type != TYP_STRUCT || op1->IsMultiRegNode());
        SetRegNum(REG_NA);
        ClearOtherRegs();
    }

#if DEBUGGABLE_GENTREE
    GenTreeCopyOrReload() : GenTreeUnOp()
    {
    }
#endif
};

// Represents GT_ALLOCOBJ node

struct GenTreeAllocObj final : public GenTreeUnOp
{
    unsigned int         gtNewHelper; // Value returned by ICorJitInfo::getNewHelper
    bool                 gtHelperHasSideEffects;
    CORINFO_CLASS_HANDLE gtAllocObjClsHnd;
#ifdef FEATURE_READYTORUN_COMPILER
    CORINFO_CONST_LOOKUP gtEntryPoint;
#endif

    GenTreeAllocObj(
        var_types type, unsigned int helper, bool helperHasSideEffects, CORINFO_CLASS_HANDLE clsHnd, GenTree* op)
        : GenTreeUnOp(GT_ALLOCOBJ, type, op DEBUGARG(/*largeNode*/ TRUE))
        , // This node in most cases will be changed to a call node
        gtNewHelper(helper)
        , gtHelperHasSideEffects(helperHasSideEffects)
        , gtAllocObjClsHnd(clsHnd)
    {
#ifdef FEATURE_READYTORUN_COMPILER
        gtEntryPoint.addr = nullptr;
#endif
    }
#if DEBUGGABLE_GENTREE
    GenTreeAllocObj() : GenTreeUnOp()
    {
    }
#endif
};

// Represents GT_RUNTIMELOOKUP node

struct GenTreeRuntimeLookup final : public GenTreeUnOp
{
    CORINFO_GENERIC_HANDLE   gtHnd;
    CorInfoGenericHandleType gtHndType;

    GenTreeRuntimeLookup(CORINFO_GENERIC_HANDLE hnd, CorInfoGenericHandleType hndTyp, GenTree* tree)
        : GenTreeUnOp(GT_RUNTIMELOOKUP, tree->gtType, tree DEBUGARG(/*largeNode*/ FALSE)), gtHnd(hnd), gtHndType(hndTyp)
    {
        assert(hnd != nullptr);
    }
#if DEBUGGABLE_GENTREE
    GenTreeRuntimeLookup() : GenTreeUnOp()
    {
    }
#endif

    // Return reference to the actual tree that does the lookup
    GenTree*& Lookup()
    {
        return gtOp1;
    }

    bool IsClassHandle() const
    {
        return gtHndType == CORINFO_HANDLETYPE_CLASS;
    }
    bool IsMethodHandle() const
    {
        return gtHndType == CORINFO_HANDLETYPE_METHOD;
    }
    bool IsFieldHandle() const
    {
        return gtHndType == CORINFO_HANDLETYPE_FIELD;
    }

    // Note these operations describe the handle that is input to the
    // lookup, not the handle produced by the lookup.
    CORINFO_CLASS_HANDLE GetClassHandle() const
    {
        assert(IsClassHandle());
        return (CORINFO_CLASS_HANDLE)gtHnd;
    }
    CORINFO_METHOD_HANDLE GetMethodHandle() const
    {
        assert(IsMethodHandle());
        return (CORINFO_METHOD_HANDLE)gtHnd;
    }
    CORINFO_FIELD_HANDLE GetFieldHandle() const
    {
        assert(IsMethodHandle());
        return (CORINFO_FIELD_HANDLE)gtHnd;
    }
};

// Represents the condition of a GT_JCC or GT_SETCC node.

struct GenCondition
{
    // clang-format off
    enum Code : unsigned char
    {
        OperMask  = 7,
        Unsigned  = 8,
        Unordered = Unsigned,
        Float     = 16,

        // 0 would be the encoding of "signed EQ" but since equality is sign insensitive
        // we'll use 0 as invalid/uninitialized condition code. This will also leave 1
        // as a spare code.
        NONE = 0,

        SLT  = 2,
        SLE  = 3,
        SGE  = 4,
        SGT  = 5,
        S    = 6,
        NS   = 7,

        EQ   = Unsigned | 0,    // = 8
        NE   = Unsigned | 1,    // = 9
        ULT  = Unsigned | SLT,  // = 10
        ULE  = Unsigned | SLE,  // = 11
        UGE  = Unsigned | SGE,  // = 12
        UGT  = Unsigned | SGT,  // = 13
        C    = Unsigned | S,    // = 14
        NC   = Unsigned | NS,   // = 15

        FEQ  = Float | 0,       // = 16
        FNE  = Float | 1,       // = 17
        FLT  = Float | SLT,     // = 18
        FLE  = Float | SLE,     // = 19
        FGE  = Float | SGE,     // = 20
        FGT  = Float | SGT,     // = 21
        O    = Float | S,       // = 22
        NO   = Float | NS,      // = 23

        FEQU = Unordered | FEQ, // = 24
        FNEU = Unordered | FNE, // = 25
        FLTU = Unordered | FLT, // = 26
        FLEU = Unordered | FLE, // = 27
        FGEU = Unordered | FGE, // = 28
        FGTU = Unordered | FGT, // = 29
        P    = Unordered | O,   // = 30
        NP   = Unordered | NO,  // = 31
    };
    // clang-format on

private:
    Code m_code;

public:
    Code GetCode() const
    {
        return m_code;
    }

    bool IsFlag() const
    {
        return (m_code & OperMask) >= S;
    }

    bool IsUnsigned() const
    {
        return (ULT <= m_code) && (m_code <= UGT);
    }

    bool IsFloat() const
    {
        return !IsFlag() && (m_code & Float) != 0;
    }

    bool IsUnordered() const
    {
        return !IsFlag() && (m_code & (Float | Unordered)) == (Float | Unordered);
    }

    bool Is(Code cond) const
    {
        return m_code == cond;
    }

    template <typename... TRest>
    bool Is(Code c, TRest... rest) const
    {
        return Is(c) || Is(rest...);
    }

    // Indicate whether the condition should be swapped in order to avoid generating
    // multiple branches. This happens for certain floating point conditions on XARCH,
    // see GenConditionDesc and its associated mapping table for more details.
    bool PreferSwap() const
    {
#ifdef TARGET_XARCH
        return Is(GenCondition::FLT, GenCondition::FLE, GenCondition::FGTU, GenCondition::FGEU);
#else
        return false;
#endif
    }

    const char* Name() const
    {
        // clang-format off
        static const char* names[]
        {
            "NONE", "???",  "SLT",  "SLE",  "SGE",  "SGT",  "S", "NS",
            "UEQ",  "UNE",  "ULT",  "ULE",  "UGE",  "UGT",  "C", "NC",
            "FEQ",  "FNE",  "FLT",  "FLE",  "FGE",  "FGT",  "O", "NO",
            "FEQU", "FNEU", "FLTU", "FLEU", "FGEU", "FGTU", "P", "NP"
        };
        // clang-format on

        assert(m_code < _countof(names));
        return names[m_code];
    }

    GenCondition() : m_code()
    {
    }

    GenCondition(Code cond) : m_code(cond)
    {
    }

    bool operator==(Code code) const
    {
        return m_code == code;
    }

    bool operator!=(Code code) const
    {
        return m_code != code;
    }

    static_assert((GT_NE - GT_EQ) == (NE & ~Unsigned), "bad relop");
    static_assert((GT_LT - GT_EQ) == SLT, "bad relop");
    static_assert((GT_LE - GT_EQ) == SLE, "bad relop");
    static_assert((GT_GE - GT_EQ) == SGE, "bad relop");
    static_assert((GT_GT - GT_EQ) == SGT, "bad relop");
    static_assert((GT_TEST_NE - GT_TEST_EQ) == (NE & ~Unsigned), "bad relop");

    static GenCondition FromRelop(GenTree* relop)
    {
        assert(relop->OperIsCompare());

        if (varTypeIsFloating(relop->gtGetOp1()))
        {
            return FromFloatRelop(relop);
        }
        else
        {
            return FromIntegralRelop(relop);
        }
    }

    static GenCondition FromFloatRelop(GenTree* relop)
    {
        assert(varTypeIsFloating(relop->gtGetOp1()) && varTypeIsFloating(relop->gtGetOp2()));

        return FromFloatRelop(relop->OperGet(), (relop->gtFlags & GTF_RELOP_NAN_UN) != 0);
    }

    static GenCondition FromFloatRelop(genTreeOps oper, bool isUnordered)
    {
        assert(GenTree::OperIsCompare(oper));

        unsigned code = oper - GT_EQ;
        assert(code <= SGT);
        code |= Float;

        if (isUnordered)
        {
            code |= Unordered;
        }

        return GenCondition(static_cast<Code>(code));
    }

    static GenCondition FromIntegralRelop(GenTree* relop)
    {
        assert(!varTypeIsFloating(relop->gtGetOp1()) && !varTypeIsFloating(relop->gtGetOp2()));

        return FromIntegralRelop(relop->OperGet(), relop->IsUnsigned());
    }

    static GenCondition FromIntegralRelop(genTreeOps oper, bool isUnsigned)
    {
        assert(GenTree::OperIsCompare(oper));

        // GT_TEST_EQ/NE are special, they need to be mapped as GT_EQ/NE
        unsigned code = oper - ((oper >= GT_TEST_EQ) ? GT_TEST_EQ : GT_EQ);

        if (isUnsigned || (code <= 1)) // EQ/NE are treated as unsigned
        {
            code |= Unsigned;
        }

        return GenCondition(static_cast<Code>(code));
    }

    static GenCondition Reverse(GenCondition condition)
    {
        // clang-format off
        static const Code reverse[]
        {
        //  EQ    NE    LT    LE    GE    GT    F   NF
            NONE, NONE, SGE,  SGT,  SLT,  SLE,  NS, S,
            NE,   EQ,   UGE,  UGT,  ULT,  ULE,  NC, C,
            FNEU, FEQU, FGEU, FGTU, FLTU, FLEU, NO, O,
            FNE,  FEQ,  FGE,  FGT,  FLT,  FGT,  NP, P
        };
        // clang-format on

        assert(condition.m_code < _countof(reverse));
        return GenCondition(reverse[condition.m_code]);
    }

    static GenCondition Swap(GenCondition condition)
    {
        // clang-format off
        static const Code swap[]
        {
        //  EQ    NE    LT    LE    GE    GT    F  NF
            NONE, NONE, SGT,  SGE,  SLE,  SLT,  S, NS,
            EQ,   NE,   UGT,  UGE,  ULE,  ULT,  C, NC,
            FEQ,  FNE,  FGT,  FGE,  FLE,  FLT,  O, NO,
            FEQU, FNEU, FGTU, FGEU, FLEU, FLTU, P, NP
        };
        // clang-format on

        assert(condition.m_code < _countof(swap));
        return GenCondition(swap[condition.m_code]);
    }
};

// Represents a GT_JCC or GT_SETCC node.

struct GenTreeCC final : public GenTree
{
private:
    GenCondition condition;

public:
    GenTreeCC(genTreeOps oper, GenCondition condition, var_types type = TYP_VOID)
        : GenTree(oper, type DEBUGARG(/*largeNode*/ false)), condition(condition)
    {
        assert(OperIs(GT_JCC, GT_SETCC));
    }

    GenCondition GetCondition() const
    {
        return condition;
    }

    void SetCondition(GenCondition cond)
    {
        condition = cond;
    }

#if DEBUGGABLE_GENTREE
    GenTreeCC() = default;
#endif
};

//------------------------------------------------------------------------
// IsDblConPositiveZero: Checks whether this is a floating point constant with value +0.0
//
// Return Value:
//    Returns true iff the tree is an GT_CNS_DBL, with value of +0.0.

inline bool GenTree::IsDblConPositiveZero() const
{
    return OperIs(GT_CNS_DBL) && AsDblCon()->IsPositiveZero();
}

inline bool GenTree::IsHWIntrinsicZero() const
{
#ifdef FEATURE_HW_INTRINSICS
    if (OperIs(GT_HWINTRINSIC))
    {
        NamedIntrinsic intrinsic = AsHWIntrinsic()->GetIntrinsic();
        return (intrinsic == NI_Vector128_get_Zero)
#if defined(TARGET_XARCH)
               || (intrinsic == NI_Vector256_get_Zero)
#elif defined(TARGET_ARM64)
               || (intrinsic == NI_Vector64_get_Zero)
#endif
            ;
    }
#endif
    return false;
}

//------------------------------------------------------------------------
// IsIntegralConst: Checks whether this is a constant node with the given value
//
// Arguments:
//    constVal - the value of interest
//
// Return Value:
//    Returns true iff the tree is an integral constant opcode, with
//    the given value.
//
// Notes:
//    Like gtIconVal, the argument is of ssize_t, so cannot check for
//    long constants in a target-independent way.

inline bool GenTree::IsIntegralConst(ssize_t constVal) const
{
    if ((gtOper == GT_CNS_INT) && (AsIntConCommon()->IconValue() == constVal))
    {
        return true;
    }

    if ((gtOper == GT_CNS_LNG) && (AsIntConCommon()->LngValue() == constVal))
    {
        return true;
    }

    return false;
}

inline bool GenTree::IsIntCon(ssize_t value) const
{
    return (gtOper == GT_CNS_INT) && (AsIntCon()->GetValue() == value);
}

inline GenTree* GenTree::gtGetOp1() const
{
    return AsOp()->gtOp1;
}

#ifdef DEBUG
inline bool GenTree::RequiresNonNullOp2(genTreeOps oper)
{
    return GenTree::OperIsBinary(oper) && (oper != GT_LEA) && (oper != GT_INTRINSIC);
}
#endif

inline GenTree* GenTree::gtGetOp2() const
{
    assert(OperIsBinary());

    GenTree* op2 = AsOp()->gtOp2;
    assert((op2 != nullptr) || !RequiresNonNullOp2(gtOper));
    return op2;
}

inline GenTree* GenTree::gtGetOp2IfPresent() const
{
    GenTree* op2 = OperIsBinary() ? AsOp()->gtOp2 : nullptr;
    assert((op2 != nullptr) || !RequiresNonNullOp2(gtOper));
    return op2;
}

inline GenTree* GenTree::gtEffectiveVal()
{
    for (GenTree* effectiveVal = this;;)
    {
        if (effectiveVal->OperIs(GT_COMMA))
        {
            effectiveVal = effectiveVal->AsOp()->GetOp(1);
        }
        else if (effectiveVal->OperIs(GT_NOP) && (effectiveVal->AsUnOp()->GetOp(0) != nullptr))
        {
            effectiveVal = effectiveVal->AsUnOp()->GetOp(0);
        }
        else
        {
            return effectiveVal;
        }
    }
}

inline GenTree* GenTree::SkipComma()
{
    GenTree* node = this;
    while (node->OperIs(GT_COMMA))
    {
        node = node->AsOp()->GetOp(1);
    }
    return node;
}

inline GenTree* GenTree::SkipRetExpr()
{
    GenTree* value = this;

    while (GenTreeRetExpr* retExpr = value->IsRetExpr())
    {
        value = retExpr->GetRetExpr();
    }

    return value;
}

inline GenTree* GenTree::gtSkipReloadOrCopy()
{
    // There can be only one reload or copy (we can't have a reload/copy of a reload/copy)
    if (gtOper == GT_RELOAD || gtOper == GT_COPY)
    {
        assert(gtGetOp1()->OperGet() != GT_RELOAD && gtGetOp1()->OperGet() != GT_COPY);
        return gtGetOp1();
    }
    return this;
}

inline bool GenTree::IsMultiRegCall() const
{
    return IsCall() && AsCall()->HasMultiRegRetVal();
}

inline bool GenTree::IsMultiRegLclVar() const
{
    return OperIs(GT_STORE_LCL_VAR) && AsLclVar()->IsMultiReg();
}

inline bool GenTree::IsMultiRegNode() const
{
    if (const GenTreeUnOp* copy = IsCopyOrReload())
    {
        return copy->GetOp(0)->IsMultiRegNode();
    }

#if FEATURE_ARG_SPLIT
    if (IsPutArgSplit())
    {
        // Treat as "multi-reg" even if it has a single reg, node's type is always
        // STRUCT and we need to make sure we get the correct register type.
        return true;
    }
#endif

#ifndef TARGET_64BIT
    if (IsMultiRegOpLong())
    {
        return true;
    }
#endif

#if FEATURE_MULTIREG_RET
    if (const GenTreeCall* call = IsCall())
    {
        return call->GetRegCount() > 1;
    }
#endif

    if (OperIs(GT_STORE_LCL_VAR))
    {
        return AsLclVar()->IsMultiReg();
    }

    return false;
}

inline unsigned GenTree::GetMultiRegCount(Compiler* compiler) const
{
    if (const GenTreeUnOp* copy = IsCopyOrReload())
    {
        return copy->GetOp(0)->GetMultiRegCount(compiler);
    }

#if FEATURE_ARG_SPLIT
    if (const GenTreePutArgSplit* arg = IsPutArgSplit())
    {
        return arg->GetRegCount();
    }
#endif

#ifndef TARGET_64BIT
    if (IsMultiRegOpLong())
    {
        return 2;
    }
#endif

#if FEATURE_MULTIREG_RET
    if (const GenTreeCall* call = IsCall())
    {
        return call->GetRegCount();
    }
#endif

    if (OperIs(GT_STORE_LCL_VAR))
    {
        return AsLclVar()->GetMultiRegCount(compiler);
    }

    assert(!"GetMultiRegCount called with non-multireg node");
    return 1;
}

inline var_types GenTree::GetMultiRegType(Compiler* compiler, unsigned regIndex)
{
#if FEATURE_ARG_SPLIT
    if (GenTreePutArgSplit* arg = IsPutArgSplit())
    {
        return arg->GetRegType(regIndex);
    }
#endif

#ifndef TARGET_64BIT
    if (IsMultiRegOpLong())
    {
        return TYP_INT;
    }
#endif

#if FEATURE_MULTIREG_RET
    if (IsMultiRegCall())
    {
        return AsCall()->GetRegType(regIndex);
    }
#endif

    if (OperIs(GT_STORE_LCL_VAR))
    {
        if (TypeIs(TYP_LONG))
        {
            return TYP_INT;
        }

        // TODO-MIKE-Review: Hmm, what about Vector2/3/4?
        assert(TypeIs(TYP_STRUCT));

        return AsLclVar()->GetMultiRegType(compiler, regIndex);
    }

    unreached();
}

constexpr GenTreeFlags GetLastUseFlag(unsigned regIndex)
{
    assert(regIndex < 4);
    return static_cast<GenTreeFlags>(GTF_VAR_FIELD_DEATH0 << regIndex);
}

inline bool GenTree::IsLastUse(unsigned regIndex)
{
    assert(OperIs(GT_LCL_VAR, GT_STORE_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_FLD, GT_COPY, GT_RELOAD));

    return (gtFlags & GetLastUseFlag(regIndex)) != 0;
}

inline bool GenTree::HasLastUse()
{
    assert(OperIs(GT_LCL_VAR, GT_STORE_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_FLD, GT_COPY, GT_RELOAD));

    return (gtFlags & GTF_VAR_FIELD_DEATH_MASK) != 0;
}

inline void GenTree::SetLastUse(unsigned regIndex, bool lastUse)
{
    assert(OperIs(GT_LCL_VAR, GT_STORE_LCL_VAR, GT_LCL_FLD, GT_STORE_LCL_FLD, GT_COPY, GT_RELOAD));

    if (lastUse)
    {
        gtFlags |= GetLastUseFlag(regIndex);
    }
    else
    {
        gtFlags &= ~GetLastUseFlag(regIndex);
    }
}

//-----------------------------------------------------------------------------------
// IsCopyOrReloadOfMultiRegCall: whether this is a GT_COPY or GT_RELOAD of a multi-reg
// call node.
//
// Arguments:
//     None
//
// Return Value:
//     Returns true if this GenTree is a copy or reload of multi-reg call node.
inline bool GenTree::IsCopyOrReloadOfMultiRegCall() const
{
    if (IsCopyOrReload())
    {
        return gtGetOp1()->IsMultiRegCall();
    }

    return false;
}

inline bool GenTree::IsCnsIntOrI() const
{
    return (gtOper == GT_CNS_INT);
}

inline bool GenTree::IsIntegralConst() const
{
#ifdef TARGET_64BIT
    return IsIntCon();
#else
    return ((gtOper == GT_CNS_INT) || (gtOper == GT_CNS_LNG));
#endif
}

// Is this node an integer constant that fits in a 32-bit signed integer (INT32)
inline bool GenTree::IsIntCnsFitsInI32()
{
#ifdef TARGET_64BIT
    return IsIntCon() && AsIntCon()->FitsInI32();
#else
    return IsIntCon();
#endif
}

inline bool GenTree::IsCnsFltOrDbl() const
{
    return OperGet() == GT_CNS_DBL;
}

inline bool GenTree::IsCnsNonZeroFltOrDbl()
{
    if (OperGet() == GT_CNS_DBL)
    {
        double constValue = AsDblCon()->gtDconVal;
        return *(__int64*)&constValue != 0;
    }

    return false;
}

inline bool GenTree::IsHelperCall()
{
    return OperGet() == GT_CALL && AsCall()->gtCallType == CT_HELPER;
}

#ifndef HOST_64BIT
#include <poppack.h>
#endif

const size_t TREE_NODE_SZ_SMALL = sizeof(GenTreeLclFld);
const size_t TREE_NODE_SZ_LARGE = sizeof(GenTreeCall);
