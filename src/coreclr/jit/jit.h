// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*****************************************************************************/
#ifndef _JIT_H_
#define _JIT_H_
/*****************************************************************************/

//
// clr.sln only defines _DEBUG
// The jit uses DEBUG rather than _DEBUG
// So we make sure that _DEBUG implies DEBUG
//
#ifdef _DEBUG
#ifndef DEBUG
#define DEBUG 1
#endif
#endif

// Clang-format messes with the indentation of comments if they directly precede an
// ifdef. This macro allows us to anchor the comments to the regular flow of code.
#define CLANG_FORMAT_COMMENT_ANCHOR ;

#ifdef _MSC_VER
#define CHECK_STRUCT_PADDING 0 // Set this to '1' to enable warning C4820 "'bytes' bytes padding added after
                               // construct 'member_name'" on interesting structs/classes
#else
#define CHECK_STRUCT_PADDING 0 // Never enable it for non-MSFT compilers
#endif

#if defined(HOST_X86)
#if defined(HOST_ARM)
#error Cannot define both HOST_X86 and HOST_ARM
#endif
#if defined(HOST_AMD64)
#error Cannot define both HOST_X86 and HOST_AMD64
#endif
#if defined(HOST_ARM64)
#error Cannot define both HOST_X86 and HOST_ARM64
#endif
#elif defined(HOST_AMD64)
#if defined(HOST_X86)
#error Cannot define both HOST_AMD64 and HOST_X86
#endif
#if defined(HOST_ARM)
#error Cannot define both HOST_AMD64 and HOST_ARM
#endif
#if defined(HOST_ARM64)
#error Cannot define both HOST_AMD64 and HOST_ARM64
#endif
#elif defined(HOST_ARM)
#if defined(HOST_X86)
#error Cannot define both HOST_ARM and HOST_X86
#endif
#if defined(HOST_AMD64)
#error Cannot define both HOST_ARM and HOST_AMD64
#endif
#if defined(HOST_ARM64)
#error Cannot define both HOST_ARM and HOST_ARM64
#endif
#elif defined(HOST_ARM64)
#if defined(HOST_X86)
#error Cannot define both HOST_ARM64 and HOST_X86
#endif
#if defined(HOST_AMD64)
#error Cannot define both HOST_ARM64 and HOST_AMD64
#endif
#if defined(HOST_ARM)
#error Cannot define both HOST_ARM64 and HOST_ARM
#endif
#else
#error Unsupported or unset host architecture
#endif

#if defined(TARGET_X86)
#if defined(TARGET_ARM)
#error Cannot define both TARGET_X86 and TARGET_ARM
#endif
#if defined(TARGET_AMD64)
#error Cannot define both TARGET_X86 and TARGET_AMD64
#endif
#if defined(TARGET_ARM64)
#error Cannot define both TARGET_X86 and TARGET_ARM64
#endif
#elif defined(TARGET_AMD64)
#if defined(TARGET_X86)
#error Cannot define both TARGET_AMD64 and TARGET_X86
#endif
#if defined(TARGET_ARM)
#error Cannot define both TARGET_AMD64 and TARGET_ARM
#endif
#if defined(TARGET_ARM64)
#error Cannot define both TARGET_AMD64 and TARGET_ARM64
#endif
#elif defined(TARGET_ARM)
#if defined(TARGET_X86)
#error Cannot define both TARGET_ARM and TARGET_X86
#endif
#if defined(TARGET_AMD64)
#error Cannot define both TARGET_ARM and TARGET_AMD64
#endif
#if defined(TARGET_ARM64)
#error Cannot define both TARGET_ARM and TARGET_ARM64
#endif
#elif defined(TARGET_ARM64)
#if defined(TARGET_X86)
#error Cannot define both TARGET_ARM64 and TARGET_X86
#endif
#if defined(TARGET_AMD64)
#error Cannot define both TARGET_ARM64 and TARGET_AMD64
#endif
#if defined(TARGET_ARM)
#error Cannot define both TARGET_ARM64 and TARGET_ARM
#endif
#else
#error Unsupported or unset target architecture
#endif

#ifdef TARGET_64BIT
#ifdef TARGET_X86
#error Cannot define both TARGET_X86 and TARGET_64BIT
#endif // TARGET_X86
#ifdef TARGET_ARM
#error Cannot define both TARGET_ARM and TARGET_64BIT
#endif // TARGET_ARM
#endif // TARGET_64BIT

#if defined(TARGET_X86) || defined(TARGET_AMD64)
#define TARGET_XARCH
#endif

#if defined(TARGET_ARM) || defined(TARGET_ARM64)
#define TARGET_ARMARCH
#endif

// If the UNIX_AMD64_ABI is defined make sure that TARGET_AMD64 is also defined.
#if defined(UNIX_AMD64_ABI)
#if !defined(TARGET_AMD64)
#error When UNIX_AMD64_ABI is defined you must define TARGET_AMD64 defined as well.
#endif
#endif

// If the UNIX_X86_ABI is defined make sure that TARGET_X86 is also defined.
#if defined(UNIX_X86_ABI)
#if !defined(TARGET_X86)
#error When UNIX_X86_ABI is defined you must define TARGET_X86 defined as well.
#endif
#endif

// --------------------------------------------------------------------------------
// IMAGE_FILE_MACHINE_TARGET
// --------------------------------------------------------------------------------

#if defined(TARGET_X86)
#define IMAGE_FILE_MACHINE_TARGET IMAGE_FILE_MACHINE_I386
#elif defined(TARGET_AMD64)
#define IMAGE_FILE_MACHINE_TARGET IMAGE_FILE_MACHINE_AMD64
#elif defined(TARGET_ARM)
#define IMAGE_FILE_MACHINE_TARGET IMAGE_FILE_MACHINE_ARMNT
#elif defined(TARGET_ARM64)
#define IMAGE_FILE_MACHINE_TARGET IMAGE_FILE_MACHINE_ARM64 // 0xAA64
#else
#error Unsupported or unset target architecture
#endif

#ifdef TARGET_UNIX
#define CORINFO_OS_TARGET CORINFO_UNIX
#else
#define CORINFO_OS_TARGET CORINFO_WINNT
#endif

#include "corhdr.h"
#include "corjit.h"
#include "jitee.h"

#define __OPERATOR_NEW_INLINE 1 // indicate that I will define these
#define __PLACEMENT_NEW_INLINE  // don't bring in the global placement new, it is easy to make a mistake
                                // with our new(compiler*) pattern.

#include "utilcode.h" // this defines assert as _ASSERTE
#include "host.h"     // this redefines assert for the JIT to use assertAbort
#include "utils.h"

#ifdef DEBUG
#define INDEBUG(x) x
#define DEBUGARG(x) , x
#else
#define INDEBUG(x)
#define DEBUGARG(x)
#endif

#if defined(UNIX_AMD64_ABI)
#define UNIX_AMD64_ABI_ONLY_ARG(x) , x
#define UNIX_AMD64_ABI_ONLY(x) x
#else // !defined(UNIX_AMD64_ABI)
#define UNIX_AMD64_ABI_ONLY_ARG(x)
#define UNIX_AMD64_ABI_ONLY(x)
#endif // defined(UNIX_AMD64_ABI)

// Arm64 Windows supports FEATURE_ARG_SPLIT, note this is different from
// the official Arm64 ABI.
// Case: splitting 16 byte struct between x7 and stack
#if defined(TARGET_ARM) || (defined(TARGET_ARM64) && defined(TARGET_WINDOWS))
#define FEATURE_ARG_SPLIT 1
#else
#define FEATURE_ARG_SPLIT 0
#endif // (defined(TARGET_ARM) || (defined(TARGET_WINDOWS) && defined(TARGET_ARM64)))

#if defined(UNIX_AMD64_ABI)
#define UNIX_AMD64_ABI_ONLY_ARG(x) , x
#define UNIX_AMD64_ABI_ONLY(x) x
#else // !defined(UNIX_AMD64_ABI)
#define UNIX_AMD64_ABI_ONLY_ARG(x)
#define UNIX_AMD64_ABI_ONLY(x)
#endif // defined(UNIX_AMD64_ABI)

#if defined(UNIX_AMD64_ABI) || defined(TARGET_ARM64)
#define MULTIREG_HAS_SECOND_GC_RET 1
#define MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(x) , x
#define MULTIREG_HAS_SECOND_GC_RET_ONLY(x) x
#else // !defined(UNIX_AMD64_ABI)
#define MULTIREG_HAS_SECOND_GC_RET 0
#define MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(x)
#define MULTIREG_HAS_SECOND_GC_RET_ONLY(x)
#endif // defined(UNIX_AMD64_ABI)

#define REGEN_SHORTCUTS 0
#define REGEN_CALLPAT 0

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                          jit.h                                            XX
XX                                                                           XX
XX   Interface of the JIT with jit.cpp                                       XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

/*****************************************************************************/
#if defined(DEBUG)
#include "log.h"

#define INFO6 LL_INFO10000   // Did Jit or Inline succeeded?
#define INFO7 LL_INFO100000  // NYI stuff
#define INFO8 LL_INFO1000000 // Weird failures

#endif // DEBUG

typedef class ICorJitInfo* COMP_HANDLE;

const CORINFO_CLASS_HANDLE NO_CLASS_HANDLE = (CORINFO_CLASS_HANDLE) nullptr;

/*****************************************************************************/

// We define two IL offset types, as follows:
//
// IL_OFFSET:  either a distinguished value, or an IL offset.
// IL_OFFSETX: either a distinguished value, or the top two bits are a flags, and the remaining bottom
//             bits are a IL offset.
//
// In both cases, the set of legal distinguished values is:
//     BAD_IL_OFFSET             -- A unique illegal IL offset number. Note that it must be different from
//                                  the ICorDebugInfo values, below, and must also not be a legal IL offset.
//     ICorDebugInfo::NO_MAPPING -- The IL offset corresponds to no source code (such as EH step blocks).
//     ICorDebugInfo::PROLOG     -- The IL offset indicates a prolog
//     ICorDebugInfo::EPILOG     -- The IL offset indicates an epilog
//
// The IL offset must be in the range [0 .. 0x3fffffff]. This is because we steal
// the top two bits in IL_OFFSETX for flags, but we want the maximum range to be the same
// for both types. The IL value can't be larger than the maximum IL offset of the function
// being compiled.
//
// Blocks and statements never store one of the ICorDebugInfo values, even for IL_OFFSETX types. These are
// only stored in the ILMapping struct, ilOffsetX field.

typedef unsigned IL_OFFSET;

const IL_OFFSET BAD_IL_OFFSET = 0x80000000;
const IL_OFFSET MAX_IL_OFFSET = 0x3fffffff;

typedef unsigned IL_OFFSETX;                                 // IL_OFFSET with stack-empty or call-instruction bit
const IL_OFFSETX IL_OFFSETX_STKBIT             = 0x80000000; // Note: this bit is set when the stack is NOT empty!
const IL_OFFSETX IL_OFFSETX_CALLINSTRUCTIONBIT = 0x40000000; // Set when the IL offset is for a call instruction.
const IL_OFFSETX IL_OFFSETX_BITS               = IL_OFFSETX_STKBIT | IL_OFFSETX_CALLINSTRUCTIONBIT;

IL_OFFSET jitGetILoffs(IL_OFFSETX offsx);

const unsigned BAD_VAR_NUM = UINT_MAX;

// This is the same as the above, but it's used in absolute contexts (i.e. offset from the start).  Also,
// this is used for native code sizes.
typedef unsigned UNATIVE_OFFSET;

typedef ptrdiff_t ssize_t;

/*****************************************************************************/

#include "vartype.h"

/*****************************************************************************/

// Late disassembly is OFF by default. Can be turned ON by
// adding /DLATE_DISASM=1 on the command line.
// Always OFF in the non-debug version

#if defined(LATE_DISASM) && (LATE_DISASM == 0)
#undef LATE_DISASM
#endif

/*****************************************************************************/

/*****************************************************************************/

#define ASSERTION_PROP 1 // Enable value/assertion propagation

#define LOCAL_ASSERTION_PROP 1 // Enable local assertion propagation

//=============================================================================

#define OPT_BOOL_OPS 1 // optimize boolean operations

//=============================================================================

#define DUMP_FLOWGRAPHS DEBUG // Support for creating Xml Flowgraph reports in *.fgx files

#define HANDLER_ENTRY_MUST_BE_IN_HOT_SECTION 1 // if 1 we must have all handler entry points in the Hot code section

/*****************************************************************************/

#define VPTR_OFFS 0 // offset of vtable pointer from obj ptr

/*****************************************************************************/

#define DUMP_GC_TABLES DEBUG
#define VERIFY_GC_TABLES 0

#define FUNC_INFO_LOGGING 1 // Support dumping function info to a file. In retail, only NYIs, with no function name,
                            // are dumped.

#define COUNT_BASIC_BLOCKS 0  // Create a histogram of basic block sizes, and a histogram of IL sizes in the simple
                              // case of single block methods.
#define COUNT_LOOPS 0         // Collect stats about loops, such as the total number of natural loops, a histogram of
                              // the number of loop exits, etc.
#define DATAFLOW_ITER 0       // Count iterations in lexical CSE and constant folding dataflow.
#define DISPLAY_SIZES 0       // Display generated code, data, and GC information sizes.
#define MEASURE_BLOCK_SIZE 0  // Collect stats about basic block and flowList node sizes and memory allocations.
#define MEASURE_FATAL 0       // Count the number of calls to fatal(), including NYIs and noway_asserts.
#define MEASURE_NODE_SIZE 0   // Collect stats about GenTree node allocations.
#define MEASURE_PTRTAB_SIZE 0 // Collect stats about GC pointer table allocations.
#define NODEBASH_STATS 0      // Collect stats on changed gtOper values in GenTree's.
#define COUNT_AST_OPERS 0     // Display use counts for GenTree operators.

#define VERBOSE_SIZES 0 // Always display GC info sizes. If set, DISPLAY_SIZES must also be set.

#ifdef DEBUG
#define MEASURE_MEM_ALLOC 1 // Collect memory allocation stats.
#define LOOP_HOIST_STATS 1  // Collect loop hoisting stats.
#define TRACK_LSRA_STATS 1  // Collect LSRA stats
#else
#define MEASURE_MEM_ALLOC 0 // You can set this to 1 to get memory stats in retail, as well
#define LOOP_HOIST_STATS 0  // You can set this to 1 to get loop hoist stats in retail, as well
#define TRACK_LSRA_STATS 0  // You can set this to 1 to get LSRA stats in retail, as well
#endif

// Timing calls to clr.dll is only available under certain conditions.
#ifndef FEATURE_JIT_METHOD_PERF
#define MEASURE_CLRAPI_CALLS 0 // Can't time these calls without METHOD_PERF.
#endif
#ifdef DEBUG
#define MEASURE_CLRAPI_CALLS 0 // No point in measuring DEBUG code.
#endif
#if !defined(HOST_X86) && !defined(HOST_AMD64)
#define MEASURE_CLRAPI_CALLS 0 // Cycle counters only hooked up on x86/x64.
#endif
#if !defined(_MSC_VER) && !defined(__GNUC__)
#define MEASURE_CLRAPI_CALLS 0 // Only know how to do this with VC and Clang.
#endif

// If none of the above set the flag to 0, it's available.
#ifndef MEASURE_CLRAPI_CALLS
#define MEASURE_CLRAPI_CALLS 0 // Set to 1 to measure time in ICorJitInfo calls.
#endif

/*****************************************************************************/
/* Portability Defines */
/*****************************************************************************/
#ifdef TARGET_X86
#define JIT32_GCENCODER
#endif

#ifdef DEBUG
#define JITDUMP(...)                                                                                                   \
    {                                                                                                                  \
        if (JitTls::GetCompiler()->verbose)                                                                            \
            logf(__VA_ARGS__);                                                                                         \
    }
#define JITDUMPTREE(tree, ...)                                                                                         \
    {                                                                                                                  \
        if (JitTls::GetCompiler()->verbose)                                                                            \
        {                                                                                                              \
            logf(__VA_ARGS__);                                                                                         \
            JitTls::GetCompiler()->gtDispTree(tree);                                                                   \
        }                                                                                                              \
    }
#define DBG_SSA_JITDUMP(...)                                                                                           \
    {                                                                                                                  \
        if (JitTls::GetCompiler()->verboseSsa)                                                                         \
            logf(__VA_ARGS__);                                                                                         \
    }
#define JITLOG(x)                                                                                                      \
    {                                                                                                                  \
        JitLogEE x;                                                                                                    \
    }
#define JITLOG_THIS(t, x)                                                                                              \
    {                                                                                                                  \
        (t)->JitLogEE x;                                                                                               \
    }
#define DBEXEC(flg, expr)                                                                                              \
    if (flg)                                                                                                           \
    {                                                                                                                  \
        expr;                                                                                                          \
    }
#define DISPNODE(t)                                                                                                    \
    if (JitTls::GetCompiler()->verbose)                                                                                \
        JitTls::GetCompiler()->gtDispLIRNode(t);
#define DISPTREE(t)                                                                                                    \
    if (JitTls::GetCompiler()->verbose)                                                                                \
        JitTls::GetCompiler()->gtDispTree(t);
#define DISPSTMT(t)                                                                                                    \
    if (JitTls::GetCompiler()->verbose)                                                                                \
        JitTls::GetCompiler()->gtDispStmt(t);
#define DISPRANGE(range)                                                                                               \
    if (JitTls::GetCompiler()->verbose)                                                                                \
        JitTls::GetCompiler()->gtDispRange(range);
#define DISPTREERANGE(range, t)                                                                                        \
    if (JitTls::GetCompiler()->verbose)                                                                                \
        JitTls::GetCompiler()->gtDispTreeRange(range, t);
#define DISPBLOCK(b)                                                                                                   \
    if (JitTls::GetCompiler()->verbose)                                                                                \
        JitTls::GetCompiler()->fgTableDispBasicBlock(b);
#define VERBOSE JitTls::GetCompiler()->verbose
#else // !DEBUG
#define JITDUMP(...)
#define JITDUMPTREE(...)
#define DBG_SSA_JITDUMP(...)
#define JITLOG(x)
#define JITLOG_THIS(t, x)
#define DBEXEC(flg, expr)
#define DISPNODE(t)
#define DISPTREE(t)
#define DISPSTMT(t)
#define DISPRANGE(range)
#define DISPTREERANGE(range, t)
#define DISPBLOCK(b)
#define VERBOSE 0
#endif // !DEBUG

/*****************************************************************************
 *
 * Double alignment. This aligns ESP to 0 mod 8 in function prolog, then uses ESP
 * to reference locals, EBP to reference parameters.
 * It only makes sense if frameless method support is on.
 * (frameless method support is now always on)
 */

#ifdef TARGET_X86
#define DOUBLE_ALIGN 1 // permit the double alignment of ESP in prolog,
                       //  and permit the double alignment of local offsets
#else
#define DOUBLE_ALIGN 0 // no special handling for double alignment
#endif

#define castto(var, typ) (*(typ*)&var)

inline size_t roundUp(size_t size, size_t mult = sizeof(size_t))
{
    assert(mult && ((mult & (mult - 1)) == 0)); // power of two test

    return (size + (mult - 1)) & ~(mult - 1);
}

#ifdef HOST_64BIT
inline unsigned int roundUp(unsigned size, unsigned mult)
{
    return (unsigned int)roundUp((size_t)size, (size_t)mult);
}
#endif // HOST_64BIT

constexpr unsigned unsigned_abs(int x)
{
    return static_cast<unsigned>(x < 0 ? -x : x);
}

#ifdef TARGET_64BIT
inline size_t unsigned_abs(ssize_t x)
{
    return ((size_t)abs(x));
}
#endif // TARGET_64BIT

/*****************************************************************************/

#if COUNT_BASIC_BLOCKS || COUNT_LOOPS || MEASURE_NODE_SIZE || MEASURE_MEM_ALLOC

#define HISTOGRAM_MAX_SIZE_COUNT 64

class Histogram
{
public:
    Histogram(const unsigned* const sizeTable);

    void dump(FILE* output);
    void record(unsigned size);

private:
    unsigned              m_sizeCount;
    const unsigned* const m_sizeTable;
    unsigned              m_counts[HISTOGRAM_MAX_SIZE_COUNT];
};

#endif // COUNT_BASIC_BLOCKS || COUNT_LOOPS || MEASURE_NODE_SIZE

#if CHECK_STRUCT_PADDING
#pragma warning(push)
#pragma warning(default : 4820) // 'bytes' bytes padding added after construct 'member_name'
#endif

#include "error.h"
#include "alloc.h"
#include "target.h"

#if FEATURE_TAILCALL_OPT
// Enable tail call opt for the following IL pattern
//
//     call someFunc
//     jmp/jcc RetBlock
//     ...
//  RetBlock:
//     ret
#define FEATURE_TAILCALL_OPT_SHARED_RETURN 1
#else // !FEATURE_TAILCALL_OPT
#define FEATURE_TAILCALL_OPT_SHARED_RETURN 0
#endif // !FEATURE_TAILCALL_OPT

#ifdef TARGET_XARCH
#define FEATURE_LOOP_ALIGN 1
#else
#define FEATURE_LOOP_ALIGN 0
#endif

class Compiler;

class JitTls
{
#ifdef DEBUG
    Compiler*    m_compiler;
    Compiler*    m_logCompiler;
    ICorJitInfo* m_jitInfo;
    JitTls*      m_next;
#endif

public:
    JitTls(ICorJitInfo* jitInfo);
    ~JitTls();

#ifdef DEBUG
    static Compiler* GetLogCompiler();
    static void SetLogCompiler(Compiler* compiler);
    static ICorJitInfo* GetJitInfo();
#endif

    static Compiler* GetCompiler();
    static void SetCompiler(Compiler* compiler);
};

extern "C" CORINFO_CLASS_HANDLE WINAPI getLikelyClass(ICorJitInfo::PgoInstrumentationSchema* schema,
                                                      UINT32                                 countSchemaItems,
                                                      BYTE*                                  pInstrumentationData,
                                                      int32_t                                ilOffset,
                                                      UINT32*                                pLikelihood,
                                                      UINT32*                                pNumberOfClasses);

#endif //_JIT_H_
