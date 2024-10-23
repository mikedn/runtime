// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

// The jit uses DEBUG rather than _DEBUG
// So we make sure that _DEBUG implies DEBUG
#ifdef _DEBUG
#ifndef DEBUG
#define DEBUG 1
#endif
#endif

// Clang-format messes with the indentation of comments if they directly precede an
// ifdef. This macro allows us to anchor the comments to the regular flow of code.
#define CLANG_FORMAT_COMMENT_ANCHOR ;

#if defined(HOST_X86) + defined(HOST_AMD64) + defined(HOST_ARM) + defined(HOST_ARM64) != 1
#error No or multiple HOSTs defined
#endif

#if defined(TARGET_X86) + defined(TARGET_AMD64) + defined(TARGET_ARM) + defined(TARGET_ARM64) != 1
#error No or multiple TARGETs defined
#endif

#if !(defined(TARGET_64BIT) ? (defined(TARGET_AMD64) || defined(TARGET_ARM64))                                         \
                            : (defined(TARGET_X86) || defined(TARGET_ARM)))
#error Invalid target bitness
#endif

#if defined(UNIX_AMD64_ABI) && !defined(TARGET_AMD64)
#error Invalid UNIX_AMD64_ABI target
#endif

#if defined(UNIX_X86_ABI) && !defined(TARGET_X86)
#error Invali8d UNIX_X86_ABI target
#endif

#if defined(TARGET_X86) || defined(TARGET_AMD64)
#define TARGET_XARCH
#endif

#if defined(TARGET_ARM) || defined(TARGET_ARM64)
#define TARGET_ARMARCH
#endif

#include "corhdr.h"
#include "corjit.h"
#include "jitee.h"

#define __PLACEMENT_NEW_INLINE // don't bring in the global placement new, it is easy to make a mistake
                               // with our new(compiler*) pattern.

#include "utilcode.h"
#include "host.h"
#include "vartypesdef.h"
#include "utils.h"

#ifdef DEBUG
#define INDEBUG(x) x
#define DEBUGARG(x) , x
#else
#define INDEBUG(x)
#define DEBUGARG(x)
#endif

#if defined(TARGET_ARM) || (defined(TARGET_ARM64) && defined(TARGET_WINDOWS))
#define FEATURE_ARG_SPLIT 1
#else
#define FEATURE_ARG_SPLIT 0
#endif

#define REGEN_SHORTCUTS 0
#define REGEN_CALLPAT 0

#ifdef DEBUG
#include "log.h"
#endif

constexpr CORINFO_CLASS_HANDLE NO_CLASS_HANDLE = nullptr;

// We define two IL offset types, as follows:
// IL_OFFSET:  An actual IL offset or BAD_IL_OFFSET.
// IL_OFFSETX: An IL offset where the top 2 bits are reserved for other information.
// Because of the 2 reserved bits in IL_OFFSETX the IL offset must be less than 0x40000000.

using IL_OFFSET = unsigned;

constexpr IL_OFFSET BAD_IL_OFFSET = 0x80000000;
constexpr IL_OFFSET MAX_IL_OFFSET = 0x3fffffff;

using IL_OFFSETX = unsigned;

constexpr IL_OFFSETX IL_OFFSETX_STKBIT             = 0x80000000; // Note: this bit is set when the stack is NOT empty!
constexpr IL_OFFSETX IL_OFFSETX_CALLINSTRUCTIONBIT = 0x40000000; // Set when the IL offset is for a call instruction.
constexpr IL_OFFSETX IL_OFFSETX_BITS               = IL_OFFSETX_STKBIT | IL_OFFSETX_CALLINSTRUCTIONBIT;

inline IL_OFFSET jitGetILoffs(IL_OFFSETX offsx)
{
    assert(offsx != BAD_IL_OFFSET);

    return static_cast<IL_OFFSET>(offsx & ~IL_OFFSETX_BITS);
}

using ssize_t = ptrdiff_t;

#include "vartype.h"

#if defined(LATE_DISASM) && (LATE_DISASM == 0)
#undef LATE_DISASM
#endif

#define ASSERTION_PROP 1
#define LOCAL_ASSERTION_PROP 1
#define OPT_BOOL_OPS 1
#define HANDLER_ENTRY_MUST_BE_IN_HOT_SECTION 1

#define FUNC_INFO_LOGGING 1 // Support dumping function info to a file. In retail, only NYIs, with no function name,
                            // are dumped.

#define DUMP_FLOWGRAPHS DEBUG
#define DUMP_GC_TABLES DEBUG

#define VERIFY_GC_TABLES 0
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
#define NODEBASH_STATS 0      // Collect stats on changed oper values in GenTree's.
#define COUNT_AST_OPERS 0     // Display use counts for GenTree operators.
#define VERBOSE_SIZES 0       // Always display GC info sizes. If set, DISPLAY_SIZES must also be set.

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

#ifdef TARGET_X86
#define JIT32_GCENCODER
// Double alignment. This aligns ESP to 0 mod 8 in function prolog, then uses ESP
// to reference locals, EBP to reference parameters.
#define DOUBLE_ALIGN 1
#else
#define DOUBLE_ALIGN 0
#endif

#ifdef DEBUG
#define JITDUMP(...)                                                                                                   \
    if (JitTls::GetCompiler()->verbose)                                                                                \
        jitprintf(__VA_ARGS__);

#define JITDUMPTREE(tree, ...)                                                                                         \
    if (JitTls::GetCompiler()->verbose)                                                                                \
    {                                                                                                                  \
        jitprintf(__VA_ARGS__);                                                                                        \
        JitTls::GetCompiler()->gtDispTree(tree);                                                                       \
    }

#define JITDUMPRANGE(range, t, ...)                                                                                    \
    if (JitTls::GetCompiler()->verbose)                                                                                \
    {                                                                                                                  \
        jitprintf(__VA_ARGS__);                                                                                        \
        JitTls::GetCompiler()->gtDispTreeRange(range, t);                                                              \
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
#else // !DEBUG
#define JITDUMP(...)
#define JITDUMPTREE(...)
#define JITDUMPRANGE(...)
#define DBEXEC(flg, expr)
#define DISPNODE(t)
#define DISPTREE(t)
#define DISPSTMT(t)
#define DISPRANGE(range)
#define DISPTREERANGE(range, t)
#define DISPBLOCK(b)
#endif // !DEBUG

#if COUNT_BASIC_BLOCKS || COUNT_LOOPS || MEASURE_NODE_SIZE || MEASURE_MEM_ALLOC

class Histogram
{
public:
    Histogram(const unsigned* const sizeTable);

    void dump(FILE* output);
    void record(unsigned size);

private:
    unsigned              m_sizeCount;
    const unsigned* const m_sizeTable;
    unsigned              m_counts[64];
};

#endif // COUNT_BASIC_BLOCKS || COUNT_LOOPS || MEASURE_NODE_SIZE

#include "error.h"
#include "alloc.h"
#include "target.h"

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
                                                      uint32_t                               countSchemaItems,
                                                      uint8_t*                               instrumentationData,
                                                      int32_t                                ilOffset,
                                                      uint32_t*                              likelihood,
                                                      uint32_t*                              numberOfClasses);
