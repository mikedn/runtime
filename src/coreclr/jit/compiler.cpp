// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                          Compiler                                         XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/
#include "jitpch.h"
#include "hostallocator.h"
#include "emit.h"
#include "valuenum.h"
#include "patchpointinfo.h"
#include "jitstd/algorithm.h"

extern ICorJitHost* g_jitHost;

AssemblyNamesList2* Compiler::s_pAltJitExcludeAssembliesList;
#ifdef DEBUG
unsigned            Compiler::jitTotalMethodCompiled;
LONG                Compiler::jitNestingLevel;
AssemblyNamesList2* Compiler::s_pJitDisasmIncludeAssembliesList;
MethodSet*          Compiler::s_pJitMethodSet;
#endif

/*****************************************************************************
 *
 *  Little helpers to grab the current cycle counter value; this is done
 *  differently based on target architecture, host toolchain, etc. The
 *  main thing is to keep the overhead absolutely minimal; in fact, on
 *  x86/x64 we use RDTSC even though it's not thread-safe; GetThreadCycles
 *  (which is monotonous) is just too expensive.
 */
#ifdef FEATURE_JIT_METHOD_PERF

#if defined(HOST_X86) || defined(HOST_AMD64)

#if defined(_MSC_VER)

#include <intrin.h>
inline bool _our_GetThreadCycles(unsigned __int64* cycleOut)
{
    *cycleOut = __rdtsc();
    return true;
}

#elif defined(__GNUC__)

inline bool _our_GetThreadCycles(unsigned __int64* cycleOut)
{
    uint32_t hi, lo;
    __asm__ __volatile__("rdtsc" : "=a"(lo), "=d"(hi));
    *cycleOut = (static_cast<unsigned __int64>(hi) << 32) | static_cast<unsigned __int64>(lo);
    return true;
}

#else // neither _MSC_VER nor __GNUC__

// The following *might* work - might as well try.
#define _our_GetThreadCycles(cp) GetThreadCycles(cp)

#endif

#elif defined(HOST_ARM) || defined(HOST_ARM64)

// If this doesn't work please see ../gc/gc.cpp for additional ARM
// info (and possible solutions).
#define _our_GetThreadCycles(cp) GetThreadCycles(cp)

#else // not x86/x64 and not ARM

// Don't know what this target is, but let's give it a try; if
// someone really wants to make this work, please add the right
// code here.
#define _our_GetThreadCycles(cp) GetThreadCycles(cp)

#endif // which host OS

const uint8_t genTypeSizes[]{
#define DEF_TP(tn, nm, jitType, sz, sze, asze, al, tf) sz,
#include "typelist.h"
};

const uint8_t genTypeAlignments[]{
#define DEF_TP(tn, nm, jitType, sz, sze, asze, al, tf) al,
#include "typelist.h"
};

const uint8_t genActualTypes[]{
#define DEF_TP(tn, nm, jitType, sz, sze, asze, al, tf) jitType,
#include "typelist.h"
};

#endif // FEATURE_JIT_METHOD_PERF
#ifdef DEBUG

void Compiler::JitLogEE(unsigned level, const char* fmt, ...)
{
    va_list args;

    if (verbose)
    {
        va_start(args, fmt);
        vflogf(jitstdout, fmt, args);
        va_end(args);
    }

    va_start(args, fmt);
    vlogf(level, fmt, args);
    va_end(args);
}

#endif // DEBUG

#if defined(DEBUG) || MEASURE_NODE_SIZE || MEASURE_BLOCK_SIZE || DISPLAY_SIZES
static unsigned genMethodCnt;  // total number of methods JIT'ted
unsigned        genMethodICnt; // number of interruptible methods
unsigned        genMethodNCnt; // number of non-interruptible methods
static unsigned genSmallMethodsNeedingExtraMemoryCnt = 0;
#endif

#if MEASURE_NODE_SIZE
NodeSizeStats genNodeSizeStats;
NodeSizeStats genNodeSizeStatsPerFunc;

unsigned  genTreeNcntHistBuckets[] = {10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 5000, 10000, 0};
Histogram genTreeNcntHist(genTreeNcntHistBuckets);

unsigned  genTreeNsizHistBuckets[] = {1000, 5000, 10000, 50000, 100000, 500000, 1000000, 0};
Histogram genTreeNsizHist(genTreeNsizHistBuckets);
#endif // MEASURE_NODE_SIZE

/*****************************************************************************/
#if MEASURE_MEM_ALLOC

unsigned  memAllocHistBuckets[] = {64, 128, 192, 256, 512, 1024, 4096, 8192, 0};
Histogram memAllocHist(memAllocHistBuckets);
unsigned  memUsedHistBuckets[] = {16, 32, 64, 128, 192, 256, 512, 1024, 4096, 8192, 0};
Histogram memUsedHist(memUsedHistBuckets);

#endif // MEASURE_MEM_ALLOC

/*****************************************************************************
 *
 *  Variables to keep track of total code amounts.
 */

#if DISPLAY_SIZES

size_t grossVMsize; // Total IL code size
size_t grossNCsize; // Native code + data size
size_t totalNCsize; // Native code + data + GC info size (TODO-Cleanup: GC info size only accurate for JIT32_GCENCODER)
size_t gcHeaderISize; // GC header      size: interruptible methods
size_t gcPtrMapISize; // GC pointer map size: interruptible methods
size_t gcHeaderNSize; // GC header      size: non-interruptible methods
size_t gcPtrMapNSize; // GC pointer map size: non-interruptible methods

#endif // DISPLAY_SIZES

#if COUNT_BASIC_BLOCKS

//          --------------------------------------------------
//          Basic block count frequency table:
//          --------------------------------------------------
//              <=         1 ===>  26872 count ( 56% of total)
//               2 ..      2 ===>    669 count ( 58% of total)
//               3 ..      3 ===>   4687 count ( 68% of total)
//               4 ..      5 ===>   5101 count ( 78% of total)
//               6 ..     10 ===>   5575 count ( 90% of total)
//              11 ..     20 ===>   3028 count ( 97% of total)
//              21 ..     50 ===>   1108 count ( 99% of total)
//              51 ..    100 ===>    182 count ( 99% of total)
//             101 ..   1000 ===>     34 count (100% of total)
//            1001 ..  10000 ===>      0 count (100% of total)
//          --------------------------------------------------

unsigned  bbCntBuckets[] = {1, 2, 3, 5, 10, 20, 50, 100, 1000, 10000, 0};
Histogram bbCntTable(bbCntBuckets);

/* Histogram for the IL opcode size of methods with a single basic block */

unsigned  bbSizeBuckets[] = {1, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 0};
Histogram bbOneBBSizeTable(bbSizeBuckets);

#endif // COUNT_BASIC_BLOCKS

/*****************************************************************************
 *
 *  Used by optFindNaturalLoops to gather statistical information such as
 *   - total number of natural loops
 *   - number of loops with 1, 2, ... exit conditions
 *   - number of loops that have an iterator (for like)
 *   - number of loops that have a constant iterator
 */

#if COUNT_LOOPS

unsigned totalLoopMethods;        // counts the total number of methods that have natural loops
unsigned maxLoopsPerMethod;       // counts the maximum number of loops a method has
unsigned totalLoopOverflows;      // # of methods that identified more loops than we can represent
unsigned totalLoopCount;          // counts the total number of natural loops
unsigned totalUnnatLoopCount;     // counts the total number of (not-necessarily natural) loops
unsigned totalUnnatLoopOverflows; // # of methods that identified more unnatural loops than we can represent
unsigned iterLoopCount;           // counts the # of loops with an iterator (for like)
unsigned simpleTestLoopCount;     // counts the # of loops with an iterator and a simple loop condition (iter < const)
unsigned constIterLoopCount;      // counts the # of loops with a constant iterator (for like)
bool     hasMethodLoops;          // flag to keep track if we already counted a method as having loops
unsigned loopsThisMethod;         // counts the number of loops in the current method
bool     loopOverflowThisMethod;  // True if we exceeded the max # of loops in the method.

/* Histogram for number of loops in a method */

unsigned  loopCountBuckets[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 0};
Histogram loopCountTable(loopCountBuckets);

/* Histogram for number of loop exits */

unsigned  loopExitCountBuckets[] = {0, 1, 2, 3, 4, 5, 6, 0};
Histogram loopExitCountTable(loopExitCountBuckets);

#endif // COUNT_LOOPS

///////////////////////////////////////////////////////////////////////////////
//
// MEASURE_NOWAY: code to measure and rank dynamic occurrences of noway_assert.
// (Just the appearances of noway_assert, whether the assert is true or false.)
// This might help characterize the cost of noway_assert in non-DEBUG builds,
// or determine which noway_assert should be simple DEBUG-only asserts.
//
///////////////////////////////////////////////////////////////////////////////

#if MEASURE_NOWAY

struct FileLine
{
    char*    m_file;
    unsigned m_line;
    char*    m_condStr;

    FileLine() : m_file(nullptr), m_line(0), m_condStr(nullptr)
    {
    }

    FileLine(const char* file, unsigned line, const char* condStr) : m_line(line)
    {
        size_t newSize = (strlen(file) + 1) * sizeof(char);
        m_file         = HostAllocator::getHostAllocator().allocate<char>(newSize);
        strcpy_s(m_file, newSize, file);

        newSize   = (strlen(condStr) + 1) * sizeof(char);
        m_condStr = HostAllocator::getHostAllocator().allocate<char>(newSize);
        strcpy_s(m_condStr, newSize, condStr);
    }

    FileLine(const FileLine& other)
    {
        m_file    = other.m_file;
        m_line    = other.m_line;
        m_condStr = other.m_condStr;
    }

    // GetHashCode() and Equals() are needed by JitHashTable

    static unsigned GetHashCode(FileLine fl)
    {
        assert(fl.m_file != nullptr);
        unsigned code = fl.m_line;
        for (const char* p = fl.m_file; *p != '\0'; p++)
        {
            code += *p;
        }
        // Could also add condStr.
        return code;
    }

    static bool Equals(FileLine fl1, FileLine fl2)
    {
        return (fl1.m_line == fl2.m_line) && (0 == strcmp(fl1.m_file, fl2.m_file));
    }
};

typedef JitHashTable<FileLine, FileLine, size_t, HostAllocator> FileLineToCountMap;
FileLineToCountMap* NowayAssertMap;

void Compiler::RecordNowayAssert(const char* filename, unsigned line, const char* condStr)
{
    if (NowayAssertMap == nullptr)
    {
        NowayAssertMap = new (HostAllocator::getHostAllocator()) FileLineToCountMap(HostAllocator::getHostAllocator());
    }
    FileLine fl(filename, line, condStr);
    size_t*  pCount = NowayAssertMap->LookupPointer(fl);
    if (pCount == nullptr)
    {
        NowayAssertMap->Set(fl, 1);
    }
    else
    {
        ++(*pCount);
    }
}

void RecordNowayAssertGlobal(const char* filename, unsigned line, const char* condStr)
{
    if ((JitConfig.JitMeasureNowayAssert() == 1) && (JitTls::GetCompiler() != nullptr))
    {
        JitTls::GetCompiler()->RecordNowayAssert(filename, line, condStr);
    }
}

struct NowayAssertCountMap
{
    size_t   count;
    FileLine fl;

    NowayAssertCountMap() : count(0)
    {
    }

    struct compare
    {
        bool operator()(const NowayAssertCountMap& elem1, const NowayAssertCountMap& elem2)
        {
            return (ssize_t)elem2.count < (ssize_t)elem1.count; // sort in descending order
        }
    };
};

void DisplayNowayAssertMap()
{
    if (NowayAssertMap != nullptr)
    {
        FILE* fout;

        LPCWSTR strJitMeasureNowayAssertFile = JitConfig.JitMeasureNowayAssertFile();
        if (strJitMeasureNowayAssertFile != nullptr)
        {
            fout = _wfopen(strJitMeasureNowayAssertFile, W("a"));
            if (fout == nullptr)
            {
                fprintf(jitstdout, "Failed to open JitMeasureNowayAssertFile \"%ws\"\n", strJitMeasureNowayAssertFile);
                return;
            }
        }
        else
        {
            fout = jitstdout;
        }

        // Iterate noway assert map, create sorted table by occurrence, dump it.
        unsigned             count = NowayAssertMap->GetCount();
        NowayAssertCountMap* nacp  = new NowayAssertCountMap[count];
        unsigned             i     = 0;

        for (FileLineToCountMap::KeyIterator iter = NowayAssertMap->Begin(), end = NowayAssertMap->End();
             !iter.Equal(end); ++iter)
        {
            nacp[i].count = iter.GetValue();
            nacp[i].fl    = iter.Get();
            ++i;
        }

        jitstd::sort(nacp, nacp + count, NowayAssertCountMap::compare());

        if (fout == jitstdout)
        {
            // Don't output the header if writing to a file, since we'll be appending to existing dumps in that case.
            fprintf(fout, "\nnoway_assert counts:\n");
            fprintf(fout, "count, file, line, text\n");
        }

        for (i = 0; i < count; i++)
        {
            fprintf(fout, "%u, %s, %u, \"%s\"\n", nacp[i].count, nacp[i].fl.m_file, nacp[i].fl.m_line,
                    nacp[i].fl.m_condStr);
        }

        if (fout != jitstdout)
        {
            fclose(fout);
            fout = nullptr;
        }
    }
}

#endif // MEASURE_NOWAY

/*****************************************************************************
 * variables to keep track of how many iterations we go in a dataflow pass
 */

#if DATAFLOW_ITER

unsigned CSEiterCount; // counts the # of iteration for the CSE dataflow
unsigned CFiterCount;  // counts the # of iteration for the Const Folding dataflow

#endif // DATAFLOW_ITER

#if MEASURE_BLOCK_SIZE
size_t genFlowNodeSize;
size_t genFlowNodeCnt;
#endif // MEASURE_BLOCK_SIZE

/*****************************************************************************/
// We keep track of methods we've already compiled.

/*****************************************************************************
 *  Declare the statics
 */

#ifdef DEBUG
/* static */
LONG Compiler::s_compMethodsCount = 0; // to produce unique label names
#endif

#ifndef PROFILING_SUPPORTED
const bool Compiler::Options::compNoPInvokeInlineCB = false;
#endif

void Compiler::compStartup()
{
#if DISPLAY_SIZES
    grossVMsize = grossNCsize = totalNCsize = 0;
#endif

#ifdef JIT32_GCENCODER
    InitGCEncoderLookupTable();
#endif

    ValueNumStore::InitValueNumStoreStatics();

    compDisplayStaticSizes(jitstdout);
}

void Compiler::compShutdown()
{
    if (s_pAltJitExcludeAssembliesList != nullptr)
    {
        s_pAltJitExcludeAssembliesList->~AssemblyNamesList2();
        s_pAltJitExcludeAssembliesList = nullptr;
    }

#ifdef DEBUG
    if (s_pJitDisasmIncludeAssembliesList != nullptr)
    {
        s_pJitDisasmIncludeAssembliesList->~AssemblyNamesList2();
        s_pJitDisasmIncludeAssembliesList = nullptr;
    }
#endif

#if MEASURE_NOWAY
    DisplayNowayAssertMap();
#endif // MEASURE_NOWAY

#if defined(DEBUG) || defined(INLINE_DATA)
    // Finish reading and/or writing inline xml
    if (JitConfig.JitInlineDumpXmlFile() != nullptr)
    {
        FILE* file = _wfopen(JitConfig.JitInlineDumpXmlFile(), W("a"));
        if (file != nullptr)
        {
            InlineStrategy::FinalizeXml(file);
            fclose(file);
        }
        else
        {
            InlineStrategy::FinalizeXml();
        }
    }
#endif // defined(DEBUG) || defined(INLINE_DATA)

#if defined(DEBUG) || MEASURE_NODE_SIZE || MEASURE_BLOCK_SIZE || DISPLAY_SIZES
    if (genMethodCnt == 0)
    {
        return;
    }
#endif

#if NODEBASH_STATS
    GenTree::ReportOperBashing(jitstdout);
#endif

    // Where should we write our statistics output?
    FILE* fout = jitstdout;

#ifdef FEATURE_JIT_METHOD_PERF
    if (compJitTimeLogFilename != nullptr)
    {
        FILE* jitTimeLogFile = _wfopen(compJitTimeLogFilename, W("a"));
        if (jitTimeLogFile != nullptr)
        {
            CompTimeSummaryInfo::s_compTimeSummary.Print(jitTimeLogFile);
            fclose(jitTimeLogFile);
        }
    }

    JitTimer::Shutdown();
#endif // FEATURE_JIT_METHOD_PERF

#if COUNT_AST_OPERS

    // Add up all the counts so that we can show percentages of total
    unsigned gtc = 0;
    for (unsigned op = 0; op < GT_COUNT; op++)
        gtc += GenTree::s_gtNodeCounts[op];

    if (gtc > 0)
    {
        unsigned rem_total = gtc;
        unsigned rem_large = 0;
        unsigned rem_small = 0;

        unsigned tot_large = 0;
        unsigned tot_small = 0;

        fprintf(fout, "\nGenTree operator counts (approximate):\n\n");

        for (unsigned op = 0; op < GT_COUNT; op++)
        {
            unsigned siz = GenTree::s_gtTrueSizes[op];
            unsigned cnt = GenTree::s_gtNodeCounts[op];
            double   pct = 100.0 * cnt / gtc;

            if (siz > TREE_NODE_SZ_SMALL)
                tot_large += cnt;
            else
                tot_small += cnt;

            // Let's not show anything below a threshold
            if (pct >= 0.5)
            {
                fprintf(fout, "    GT_%-17s   %7u (%4.1lf%%) %3u bytes each\n", GenTree::OpName((genTreeOps)op), cnt,
                        pct, siz);
                rem_total -= cnt;
            }
            else
            {
                if (siz > TREE_NODE_SZ_SMALL)
                    rem_large += cnt;
                else
                    rem_small += cnt;
            }
        }
        if (rem_total > 0)
        {
            fprintf(fout, "    All other GT_xxx ...   %7u (%4.1lf%%) ... %4.1lf%% small + %4.1lf%% large\n", rem_total,
                    100.0 * rem_total / gtc, 100.0 * rem_small / gtc, 100.0 * rem_large / gtc);
        }
        fprintf(fout, "    -----------------------------------------------------\n");
        fprintf(fout, "    Total    .......   %11u --ALL-- ... %4.1lf%% small + %4.1lf%% large\n", gtc,
                100.0 * tot_small / gtc, 100.0 * tot_large / gtc);
        fprintf(fout, "\n");
    }

#endif // COUNT_AST_OPERS

#if DISPLAY_SIZES

    if (grossVMsize && grossNCsize)
    {
        fprintf(fout, "\n");
        fprintf(fout, "--------------------------------------\n");
        fprintf(fout, "Function and GC info size stats\n");
        fprintf(fout, "--------------------------------------\n");

        fprintf(fout, "[%7u VM, %8u %6s %4u%%] %s\n", grossVMsize, grossNCsize, Target::g_tgtCPUName,
                100 * grossNCsize / grossVMsize, "Total (excluding GC info)");

        fprintf(fout, "[%7u VM, %8u %6s %4u%%] %s\n", grossVMsize, totalNCsize, Target::g_tgtCPUName,
                100 * totalNCsize / grossVMsize, "Total (including GC info)");

        if (gcHeaderISize || gcHeaderNSize)
        {
            fprintf(fout, "\n");

            fprintf(fout, "GC tables   : [%7uI,%7uN] %7u byt  (%u%% of IL, %u%% of %s).\n",
                    gcHeaderISize + gcPtrMapISize, gcHeaderNSize + gcPtrMapNSize, totalNCsize - grossNCsize,
                    100 * (totalNCsize - grossNCsize) / grossVMsize, 100 * (totalNCsize - grossNCsize) / grossNCsize,
                    Target::g_tgtCPUName);

            fprintf(fout, "GC headers  : [%7uI,%7uN] %7u byt, [%4.1fI,%4.1fN] %4.1f byt/meth\n", gcHeaderISize,
                    gcHeaderNSize, gcHeaderISize + gcHeaderNSize, (float)gcHeaderISize / (genMethodICnt + 0.001),
                    (float)gcHeaderNSize / (genMethodNCnt + 0.001),
                    (float)(gcHeaderISize + gcHeaderNSize) / genMethodCnt);

            fprintf(fout, "GC ptr maps : [%7uI,%7uN] %7u byt, [%4.1fI,%4.1fN] %4.1f byt/meth\n", gcPtrMapISize,
                    gcPtrMapNSize, gcPtrMapISize + gcPtrMapNSize, (float)gcPtrMapISize / (genMethodICnt + 0.001),
                    (float)gcPtrMapNSize / (genMethodNCnt + 0.001),
                    (float)(gcPtrMapISize + gcPtrMapNSize) / genMethodCnt);
        }
        else
        {
            fprintf(fout, "\n");

            fprintf(fout, "GC tables   take up %u bytes (%u%% of instr, %u%% of %6s code).\n",
                    totalNCsize - grossNCsize, 100 * (totalNCsize - grossNCsize) / grossVMsize,
                    100 * (totalNCsize - grossNCsize) / grossNCsize, Target::g_tgtCPUName);
        }
    }

#endif // DISPLAY_SIZES

#if COUNT_BASIC_BLOCKS
    fprintf(fout, "--------------------------------------------------\n");
    fprintf(fout, "Basic block count frequency table:\n");
    fprintf(fout, "--------------------------------------------------\n");
    bbCntTable.dump(fout);
    fprintf(fout, "--------------------------------------------------\n");

    fprintf(fout, "\n");

    fprintf(fout, "--------------------------------------------------\n");
    fprintf(fout, "IL method size frequency table for methods with a single basic block:\n");
    fprintf(fout, "--------------------------------------------------\n");
    bbOneBBSizeTable.dump(fout);
    fprintf(fout, "--------------------------------------------------\n");
#endif // COUNT_BASIC_BLOCKS

#if COUNT_LOOPS

    fprintf(fout, "\n");
    fprintf(fout, "---------------------------------------------------\n");
    fprintf(fout, "Loop stats\n");
    fprintf(fout, "---------------------------------------------------\n");
    fprintf(fout, "Total number of methods with loops is %5u\n", totalLoopMethods);
    fprintf(fout, "Total number of              loops is %5u\n", totalLoopCount);
    fprintf(fout, "Maximum number of loops per method is %5u\n", maxLoopsPerMethod);
    fprintf(fout, "# of methods overflowing nat loop table is %5u\n", totalLoopOverflows);
    fprintf(fout, "Total number of 'unnatural' loops is %5u\n", totalUnnatLoopCount);
    fprintf(fout, "# of methods overflowing unnat loop limit is %5u\n", totalUnnatLoopOverflows);
    fprintf(fout, "Total number of loops with an         iterator is %5u\n", iterLoopCount);
    fprintf(fout, "Total number of loops with a simple   iterator is %5u\n", simpleTestLoopCount);
    fprintf(fout, "Total number of loops with a constant iterator is %5u\n", constIterLoopCount);

    fprintf(fout, "--------------------------------------------------\n");
    fprintf(fout, "Loop count frequency table:\n");
    fprintf(fout, "--------------------------------------------------\n");
    loopCountTable.dump(fout);
    fprintf(fout, "--------------------------------------------------\n");
    fprintf(fout, "Loop exit count frequency table:\n");
    fprintf(fout, "--------------------------------------------------\n");
    loopExitCountTable.dump(fout);
    fprintf(fout, "--------------------------------------------------\n");

#endif // COUNT_LOOPS

#if DATAFLOW_ITER

    fprintf(fout, "---------------------------------------------------\n");
    fprintf(fout, "Total number of iterations in the CSE dataflow loop is %5u\n", CSEiterCount);
    fprintf(fout, "Total number of iterations in the  CF dataflow loop is %5u\n", CFiterCount);

#endif // DATAFLOW_ITER

#if MEASURE_NODE_SIZE

    fprintf(fout, "\n");
    fprintf(fout, "---------------------------------------------------\n");
    fprintf(fout, "GenTree node allocation stats\n");
    fprintf(fout, "---------------------------------------------------\n");

    fprintf(fout, "Allocated %6I64u tree nodes (%7I64u bytes total, avg %4I64u bytes per method)\n",
            genNodeSizeStats.genTreeNodeCnt, genNodeSizeStats.genTreeNodeSize,
            genNodeSizeStats.genTreeNodeSize / genMethodCnt);

    fprintf(fout, "Allocated %7I64u bytes of unused tree node space (%3.2f%%)\n",
            genNodeSizeStats.genTreeNodeSize - genNodeSizeStats.genTreeNodeActualSize,
            (float)(100 * (genNodeSizeStats.genTreeNodeSize - genNodeSizeStats.genTreeNodeActualSize)) /
                genNodeSizeStats.genTreeNodeSize);

    fprintf(fout, "\n");
    fprintf(fout, "---------------------------------------------------\n");
    fprintf(fout, "Distribution of per-method GenTree node counts:\n");
    genTreeNcntHist.dump(fout);

    fprintf(fout, "\n");
    fprintf(fout, "---------------------------------------------------\n");
    fprintf(fout, "Distribution of per-method GenTree node  allocations (in bytes):\n");
    genTreeNsizHist.dump(fout);

#endif // MEASURE_NODE_SIZE

#if MEASURE_BLOCK_SIZE

    fprintf(fout, "\n");
    fprintf(fout, "---------------------------------------------------\n");
    fprintf(fout, "BasicBlock and flowList/BasicBlockList allocation stats\n");
    fprintf(fout, "---------------------------------------------------\n");

    fprintf(fout, "Allocated %6u basic blocks (%7u bytes total, avg %4u bytes per method)\n", BasicBlock::s_Count,
            BasicBlock::s_Size, BasicBlock::s_Size / genMethodCnt);
    fprintf(fout, "Allocated %6u flow nodes (%7u bytes total, avg %4u bytes per method)\n", genFlowNodeCnt,
            genFlowNodeSize, genFlowNodeSize / genMethodCnt);

#endif // MEASURE_BLOCK_SIZE

#if MEASURE_MEM_ALLOC

    if (JitConfig.DisplayMemStats())
    {
        fprintf(fout, "\nAll allocations:\n");
        ArenaAllocator::dumpAggregateMemStats(jitstdout);

        fprintf(fout, "\nLargest method:\n");
        ArenaAllocator::dumpMaxMemStats(jitstdout);

        fprintf(fout, "\n");
        fprintf(fout, "---------------------------------------------------\n");
        fprintf(fout, "Distribution of total memory allocated per method (in KB):\n");
        memAllocHist.dump(fout);

        fprintf(fout, "\n");
        fprintf(fout, "---------------------------------------------------\n");
        fprintf(fout, "Distribution of total memory used      per method (in KB):\n");
        memUsedHist.dump(fout);
    }

#endif // MEASURE_MEM_ALLOC

#if LOOP_HOIST_STATS
#ifdef DEBUG // Always display loop stats in retail
    if (JitConfig.DisplayLoopHoistStats() != 0)
#endif // DEBUG
    {
        PrintAggregateLoopHoistStats(jitstdout);
    }
#endif // LOOP_HOIST_STATS

#if MEASURE_PTRTAB_SIZE

    fprintf(fout, "\n");
    fprintf(fout, "---------------------------------------------------\n");
    fprintf(fout, "GC pointer table stats\n");
    fprintf(fout, "---------------------------------------------------\n");

    fprintf(fout, "Reg pointer descriptor size (internal): %8u (avg %4u per method)\n", GCInfo::s_gcRegPtrDscSize,
            GCInfo::s_gcRegPtrDscSize / genMethodCnt);

    fprintf(fout, "Total pointer table size: %8u (avg %4u per method)\n", GCInfo::s_gcTotalPtrTabSize,
            GCInfo::s_gcTotalPtrTabSize / genMethodCnt);

#endif // MEASURE_PTRTAB_SIZE

#if MEASURE_NODE_SIZE || MEASURE_BLOCK_SIZE || MEASURE_PTRTAB_SIZE || DISPLAY_SIZES

    if (genMethodCnt != 0)
    {
        fprintf(fout, "\n");
        fprintf(fout, "A total of %6u methods compiled", genMethodCnt);
#if DISPLAY_SIZES
        if (genMethodICnt || genMethodNCnt)
        {
            fprintf(fout, " (%u interruptible, %u non-interruptible)", genMethodICnt, genMethodNCnt);
        }
#endif // DISPLAY_SIZES
        fprintf(fout, ".\n");
    }

#endif // MEASURE_NODE_SIZE || MEASURE_BLOCK_SIZE || MEASURE_PTRTAB_SIZE || DISPLAY_SIZES

#if EMITTER_STATS
    emitterStats(fout);
#endif

#if MEASURE_FATAL
    fprintf(fout, "\n");
    fprintf(fout, "---------------------------------------------------\n");
    fprintf(fout, "Fatal errors stats\n");
    fprintf(fout, "---------------------------------------------------\n");
    fprintf(fout, "   badCode:             %u\n", fatal_badCode);
    fprintf(fout, "   noWay:               %u\n", fatal_noWay);
    fprintf(fout, "   implLimitation:      %u\n", fatal_implLimitation);
    fprintf(fout, "   NOMEM:               %u\n", fatal_NOMEM);
    fprintf(fout, "   noWayAssertBody:     %u\n", fatal_noWayAssertBody);
#ifdef DEBUG
    fprintf(fout, "   noWayAssertBodyArgs: %u\n", fatal_noWayAssertBodyArgs);
#endif // DEBUG
    fprintf(fout, "   NYI:                 %u\n", fatal_NYI);
#endif // MEASURE_FATAL
}

void Compiler::compDisplayStaticSizes(FILE* fout)
{
#if MEASURE_NODE_SIZE
    GenTree::DumpNodeSizes(fout);
#endif
}

INDEBUG(ConfigMethodRange fJitStressRange;)

CompiledMethodInfo::CompiledMethodInfo(CORINFO_METHOD_INFO*   methodInfo,
                                       ICorJitInfo*           jitInfo,
                                       const CORINFO_EE_INFO* eeInfo)
    : compCompHnd(jitInfo)
    , compScopeHnd(methodInfo->scope)
    , compMethodHnd(methodInfo->ftn)
    , compMethodInfo(methodInfo)
    , compCode(methodInfo->ILCode)
    , compILCodeSize(methodInfo->ILCodeSize)
    , compMaxStack(methodInfo->maxStack)
    , compXcptnsCount(methodInfo->EHcount)
#if defined(TARGET_X86)
    , virtualStubParamRegNum(REG_EAX)
#elif defined(TARGET_AMD64)
    , virtualStubParamRegNum(eeInfo->targetAbi == CORINFO_CORERT_ABI ? REG_R10 : REG_R11)
#elif defined(TARGET_ARM)
    , virtualStubParamRegNum(eeInfo->targetAbi == CORINFO_CORERT_ABI ? REG_R12 : REG_R4)
#elif defined(TARGET_ARM64)
    , virtualStubParamRegNum(REG_R11)
#else
#error Unsupported or unset target architecture
#endif
    , compIsVarArgs(false)
    , compHasNextCallRetAddr(false)
{
}

Compiler::Compiler(ArenaAllocator*        alloc,
                   const CORINFO_EE_INFO* eeInfo,
                   CORINFO_METHOD_INFO*   methodInfo,
                   ICorJitInfo*           jitInfo,
                   InlineInfo*            inlineInfo)
    : compArenaAllocator(alloc), impInlineInfo(inlineInfo), eeInfo(eeInfo), opts(), info(methodInfo, jitInfo, eeInfo)
{
}

void Compiler::compInitMethodName()
{
#if defined(DEBUG) || defined(LATE_DISASM) || DUMP_FLOWGRAPHS
    // Initialize the method name and related info, as it is used early in determining whether to
    // apply stress modes, and which ones to apply.

    const char* classNamePtr;
    const char* methodName = eeGetMethodName(info.compMethodHnd, &classNamePtr);
    size_t      len        = strlen(classNamePtr) + 1;
    char*       className  = getAllocator(CMK_DebugOnly).allocate<char>(len);
    strcpy_s(className, len, classNamePtr);

    info.compMethodName          = methodName;
    info.compClassName           = className;
    info.compFullName            = eeGetMethodFullName(info.compMethodHnd);
    info.compMethodSuperPMIIndex = g_jitHost->getIntConfigValue(W("SuperPMIMethodContextNumber"), -1);
#endif

    // Opt-in to jit stress based on method hash ranges.
    // Note the default (with JitStressRange not set) is that all
    // methods will be subject to stress.
    assert(!fJitStressRange.Error());
    INDEBUG(bRangeAllowStress = fJitStressRange.Contains(info.compMethodHash()));
}

void Compiler::compInit()
{
    assert(!compIsForInlining());

    m_inlineStrategy = new (this, CMK_Inlining) InlineStrategy(this);

    for (unsigned i = 0; i < _countof(fgLargeFieldOffsetNullCheckTemps); i++)
    {
        fgLargeFieldOffsetNullCheckTemps[i] = BAD_VAR_NUM;
    }

    codeGenInit();

#if MEASURE_NODE_SIZE
    genNodeSizeStatsPerFunc.Init();
#endif
#ifdef DEBUG
    switch (JitConfig.JitNoStructPromotion())
    {
        case 0:
            break;
        case 1:
            fgNoStructPromotion = true;
            break;
        case 2:
            fgNoStructParamPromotion = true;
            break;
        default:
            unreached();
    }
#endif
}

void* Compiler::compGetHelperFtn(CorInfoHelpFunc ftnNum,        /* IN  */
                                 void**          ppIndirection) /* OUT */
{
    void* addr;

    if (info.compMatchedVM)
    {
        addr = info.compCompHnd->getHelperFtn(ftnNum, ppIndirection);
    }
    else
    {
        // If we don't have a matched VM, we won't get valid results when asking for a helper function.
        addr = UlongToPtr(0xCA11CA11); // "callcall"
    }

    return addr;
}

#ifdef DEBUG
static bool DidComponentUnitTests = false;

void Compiler::compDoComponentUnitTestsOnce()
{
    if (!JitConfig.RunComponentUnitTests())
    {
        return;
    }

    if (!DidComponentUnitTests)
    {
        DidComponentUnitTests = true;
        ValueNumStore::RunTests(this);
        BitSetSupport::TestSuite(getAllocatorDebugOnly());
    }
}

//------------------------------------------------------------------------
// compGetJitDefaultFill:
//
// Return Value:
//    An unsigned char value used to initizalize memory allocated by the JIT.
//    The default value is taken from COMPLUS_JitDefaultFill,  if is not set
//    the value will be 0xdd.  When JitStress is active a random value based
//    on the method hash is used.
//
// Notes:
//    Note that we can't use small values like zero, because we have some
//    asserts that can fire for such values.
//
// static
unsigned char Compiler::compGetJitDefaultFill(Compiler* comp)
{
    unsigned char defaultFill = (unsigned char)JitConfig.JitDefaultFill();

    if (comp != nullptr && comp->compStressCompile(STRESS_GENERIC_VARN, 50))
    {
        unsigned temp;
        temp = comp->info.compMethodHash();
        temp = (temp >> 16) ^ temp;
        temp = (temp >> 8) ^ temp;
        temp = temp & 0xff;
        // asserts like this: assert(!IsUninitialized(stkLvl));
        // mean that small values for defaultFill are problematic
        // so we make the value larger in that case.
        if (temp < 0x20)
        {
            temp |= 0x80;
        }

        // Make a misaligned pointer value to reduce probability of getting a valid value and firing
        // assert(!IsUninitialized(pointer)).
        temp |= 0x1;

        defaultFill = (unsigned char)temp;
    }

    return defaultFill;
}

#endif // DEBUG

void Compiler::compSetProcessor()
{
    assert(!compIsForInlining());

    //
    // NOTE: This function needs to be kept in sync with EEJitManager::SetCpuInfo() in vm\codeman.cpp
    //

    const JitFlags& jitFlags = *opts.jitFlags;

    // The VM will set the ISA flags depending on actual hardware support.
    // We then select which ISAs to leave enabled based on the JIT config.
    // The exception to this is the dummy Vector64/128/256 ISAs, which must be added explicitly.
    CORINFO_InstructionSetFlags instructionSetFlags = jitFlags.GetInstructionSetFlags();
    opts.compSupportsISA                            = 0;
    opts.compSupportsISAReported                    = 0;
    opts.compSupportsISAExactly                     = 0;

#ifdef TARGET_XARCH
    if (JitConfig.EnableHWIntrinsic())
    {
        // Dummy ISAs for simplifying the JIT code
        instructionSetFlags.AddInstructionSet(InstructionSet_Vector128);
        instructionSetFlags.AddInstructionSet(InstructionSet_Vector256);
    }

    if (!JitConfig.EnableSSE())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_SSE);
#ifdef TARGET_AMD64
        instructionSetFlags.RemoveInstructionSet(InstructionSet_SSE_X64);
#endif
    }

    if (!JitConfig.EnableSSE2())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_SSE2);
#ifdef TARGET_AMD64
        instructionSetFlags.RemoveInstructionSet(InstructionSet_SSE2_X64);
#endif
    }

    if (!JitConfig.EnableAES())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_AES);
    }

    if (!JitConfig.EnablePCLMULQDQ())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_PCLMULQDQ);
    }

    // We need to additionally check that COMPlus_EnableSSE3_4 is set, as that
    // is a prexisting config flag that controls the SSE3+ ISAs
    if (!JitConfig.EnableSSE3() || !JitConfig.EnableSSE3_4())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_SSE3);
    }

    if (!JitConfig.EnableSSSE3())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_SSSE3);
    }

    if (!JitConfig.EnableSSE41())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_SSE41);
#ifdef TARGET_AMD64
        instructionSetFlags.RemoveInstructionSet(InstructionSet_SSE41_X64);
#endif
    }

    if (!JitConfig.EnableSSE42())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_SSE42);
#ifdef TARGET_AMD64
        instructionSetFlags.RemoveInstructionSet(InstructionSet_SSE42_X64);
#endif
    }

    if (!JitConfig.EnablePOPCNT())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_POPCNT);
#ifdef TARGET_AMD64
        instructionSetFlags.RemoveInstructionSet(InstructionSet_POPCNT_X64);
#endif
    }

    if (!JitConfig.EnableAVX())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_AVX);
    }

    if (!JitConfig.EnableFMA())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_FMA);
    }

    if (!JitConfig.EnableAVX2())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_AVX2);
    }

    if (!JitConfig.EnableAVXVNNI())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_AVXVNNI);
    }

    if (!JitConfig.EnableLZCNT())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_LZCNT);
#ifdef TARGET_AMD64
        instructionSetFlags.RemoveInstructionSet(InstructionSet_LZCNT_X64);
#endif // TARGET_AMD64
    }

    if (!JitConfig.EnableBMI1())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_BMI1);
#ifdef TARGET_AMD64
        instructionSetFlags.RemoveInstructionSet(InstructionSet_BMI1_X64);
#endif // TARGET_AMD64
    }

    if (!JitConfig.EnableBMI2())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_BMI2);
#ifdef TARGET_AMD64
        instructionSetFlags.RemoveInstructionSet(InstructionSet_BMI2_X64);
#endif // TARGET_AMD64
    }

#endif // TARGET_XARCH
#if defined(TARGET_ARM64)
    if (JitConfig.EnableHWIntrinsic())
    {
        // Dummy ISAs for simplifying the JIT code
        instructionSetFlags.AddInstructionSet(InstructionSet_Vector64);
        instructionSetFlags.AddInstructionSet(InstructionSet_Vector128);
    }

    if (!JitConfig.EnableArm64Aes())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_Aes);
    }

    if (!JitConfig.EnableArm64Atomics())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_Atomics);
    }

    if (!JitConfig.EnableArm64Crc32())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_Crc32);
        instructionSetFlags.RemoveInstructionSet(InstructionSet_Crc32_Arm64);
    }

    if (!JitConfig.EnableArm64Sha1())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_Sha1);
    }

    if (!JitConfig.EnableArm64Sha256())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_Sha256);
    }

    if (!JitConfig.EnableArm64AdvSimd())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_AdvSimd);
        instructionSetFlags.RemoveInstructionSet(InstructionSet_AdvSimd_Arm64);
    }
#endif

    instructionSetFlags = EnsureInstructionSetFlagsAreValid(instructionSetFlags);
    opts.setSupportedISAs(instructionSetFlags);

#ifdef TARGET_XARCH
    if (canUseVexEncoding())
    {
        codeGen->GetEmitter()->SetUseVEXEncoding(true);
        // Assume each JITted method does not contain AVX instruction at first
        codeGen->GetEmitter()->SetContainsAVX(false);
        codeGen->GetEmitter()->SetContains256bitAVX(false);
    }
#endif // TARGET_XARCH

#ifdef FEATURE_SIMD
    featureSIMD = jitFlags.IsSet(JitFlags::JIT_FLAG_FEATURE_SIMD);
#endif
}

bool Compiler::notifyInstructionSetUsage(CORINFO_InstructionSet isa, bool supported) const
{
    const char* isaString = InstructionSetToString(isa);
    JITDUMP("Notify VM instruction set (%s) %s be supported.\n", isaString, supported ? "must" : "must not");
    return info.compCompHnd->notifyInstructionSetUsage(isa, supported);
}

#ifdef PROFILING_SUPPORTED
// A Dummy routine to receive Enter/Leave/Tailcall profiler callbacks.
// These are used when complus_JitEltHookEnabled=1
#ifdef TARGET_AMD64
void DummyProfilerELTStub(UINT_PTR ProfilerHandle, UINT_PTR callerSP)
{
    return;
}
#else  //! TARGET_AMD64
void DummyProfilerELTStub(UINT_PTR ProfilerHandle)
{
    return;
}
#endif //! TARGET_AMD64

#endif // PROFILING_SUPPORTED

bool Compiler::compShouldThrowOnNoway()
{
    // In min opts, we don't want the noway assert to go through the exception
    // path. Instead we want it to just silently go through codegen for
    // compat reasons.
    return !opts.MinOpts();
}

// ConfigInteger does not offer an option for decimal flags.  Any numbers are interpreted as hex.
// I could add the decimal option to ConfigInteger or I could write a function to reinterpret this
// value as the user intended.
unsigned ReinterpretHexAsDecimal(unsigned in)
{
    // ex: in: 0x100 returns: 100
    unsigned result = 0;
    unsigned index  = 1;

    // default value
    if (in == INT_MAX)
    {
        return in;
    }

    while (in)
    {
        unsigned digit = in % 16;
        in >>= 4;
        assert(digit < 10);
        result += digit * index;
        index *= 10;
    }
    return result;
}

void Compiler::compInitAltJit()
{
    assert(!compIsForInlining());

    const JitConfigValues::MethodSet& altJitMethods =
        opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT) ? JitConfig.AltJitNgen() : JitConfig.AltJit();

    // Some options don't affect the real jit when an altjit is present. The real jit has no way to know
    // if an altjit is present so we simply assume it is present if the altjit method list is not empty.
    INDEBUG(opts.isAltJitPresent = !altJitMethods.isEmpty());

    if (!opts.jitFlags->IsSet(JitFlags::JIT_FLAG_ALT_JIT))
    {
        return;
    }

#ifdef DEBUG
    opts.altJit = altJitMethods.contains(info.compMethodName, info.compClassName, &info.compMethodInfo->args) &&
                  ((JitConfig.AltJitLimit() == 0) ||
                   (Compiler::jitTotalMethodCompiled < ReinterpretHexAsDecimal(JitConfig.AltJitLimit())));
#else
    // In release mode, you either get all methods or no methods. You must use "*" as the parameter,
    // or we ignore it. Partially, this is because we haven't computed and stored the method and
    // class name except in debug, and it might be expensive to do so.
    opts.altJit                         = (altJitMethods.list() != nullptr) && (strcmp(altJitMethods.list(), "*") == 0);
#endif

    if (!opts.altJit)
    {
        return;
    }

    const WCHAR* altJitExcludeAssemblies = JitConfig.AltJitExcludeAssemblies();

    if (altJitExcludeAssemblies != nullptr)
    {
        if (s_pAltJitExcludeAssembliesList == nullptr)
        {
            s_pAltJitExcludeAssembliesList = new (HostAllocator::getHostAllocator())
                AssemblyNamesList2(altJitExcludeAssemblies, HostAllocator::getHostAllocator());
        }

        if (!s_pAltJitExcludeAssembliesList->IsEmpty() &&
            s_pAltJitExcludeAssembliesList->IsInList(info.compCompHnd->getAssemblyName(
                info.compCompHnd->getModuleAssembly(info.compCompHnd->getClassModule(info.compClassHnd)))))
        {
            opts.altJit = false;
        }
    }
}

void Compiler::compInitConfigOptions()
{
    assert(!compIsForInlining());

#ifdef DEBUG
    opts.compJitAlignLoopAdaptive       = JitConfig.JitAlignLoopAdaptive() == 1;
    opts.compJitAlignLoopBoundary       = static_cast<uint16_t>(JitConfig.JitAlignLoopBoundary());
    opts.compJitAlignLoopMinBlockWeight = static_cast<uint16_t>(JitConfig.JitAlignLoopMinBlockWeight());
    opts.compJitAlignLoopForJcc         = JitConfig.JitAlignLoopForJcc() == 1;
    opts.compJitAlignLoopMaxCodeSize    = static_cast<uint16_t>(JitConfig.JitAlignLoopMaxCodeSize());
#else
    opts.compJitAlignLoopAdaptive       = true;
    opts.compJitAlignLoopBoundary       = DEFAULT_ALIGN_LOOP_BOUNDARY;
    opts.compJitAlignLoopMinBlockWeight = DEFAULT_ALIGN_LOOP_MIN_BLOCK_WEIGHT;
    opts.compJitAlignLoopMaxCodeSize    = DEFAULT_MAX_LOOPSIZE_FOR_ALIGN;
#endif

    if (opts.compJitAlignLoopAdaptive)
    {
        opts.compJitAlignPaddingLimit = (opts.compJitAlignLoopBoundary >> 1) - 1;
    }
    else
    {
        opts.compJitAlignPaddingLimit = opts.compJitAlignLoopBoundary - 1;
    }

    assert(isPow2(opts.compJitAlignLoopBoundary));

#if FEATURE_TAILCALL_OPT
    opts.compTailCallLoopOpt = JitConfig.TailCallLoopOpt() != 0;
#endif
#if FEATURE_FASTTAILCALL
    opts.compFastTailCalls = JitConfig.FastTailCalls() != 0;
#endif

#ifdef DEBUG
    const WCHAR* functionFileName = JitConfig.JitFunctionFile();

    if ((functionFileName != nullptr) && (s_pJitMethodSet == nullptr))
    {
        s_pJitMethodSet =
            new (HostAllocator::getHostAllocator()) MethodSet(functionFileName, HostAllocator::getHostAllocator());
    }

    if (!opts.isAltJitPresent || opts.altJit)
    {
        const auto& cfg          = JitConfig;
        const auto  className    = info.compClassName;
        const auto  methodName   = info.compMethodName;
        const auto  methodParams = &info.compMethodInfo->args;

        if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT))
        {
            opts.dspOrder     = (cfg.NgenOrder() & 1) == 1;
            opts.dspGCtbls    = cfg.NgenGCDump().contains(methodName, className, methodParams);
            opts.disAsm       = cfg.NgenDisasm().contains(methodName, className, methodParams);
            opts.dspUnwind    = cfg.NgenUnwindDump().contains(methodName, className, methodParams);
            opts.dspEHTable   = cfg.NgenEHDump().contains(methodName, className, methodParams);
            opts.dspDebugInfo = cfg.NgenDebugDump().contains(methodName, className, methodParams);
        }
        else
        {
            bool         disEnabled       = true;
            const WCHAR* disasmAssemblies = cfg.JitDisasmAssemblies();

            if (disasmAssemblies != nullptr)
            {
                if (s_pJitDisasmIncludeAssembliesList == nullptr)
                {
                    s_pJitDisasmIncludeAssembliesList = new (HostAllocator::getHostAllocator())
                        AssemblyNamesList2(disasmAssemblies, HostAllocator::getHostAllocator());
                }

                if (!s_pJitDisasmIncludeAssembliesList->IsEmpty() &&
                    !s_pJitDisasmIncludeAssembliesList->IsInList(info.compCompHnd->getAssemblyName(
                        info.compCompHnd->getModuleAssembly(info.compCompHnd->getClassModule(info.compClassHnd)))))
                {
                    disEnabled = false;
                }
            }

            if (disEnabled)
            {
                opts.dspOrder     = (cfg.JitOrder() & 1) == 1;
                opts.dspGCtbls    = cfg.JitGCDump().contains(methodName, className, methodParams);
                opts.disAsm       = cfg.JitDisasm().contains(methodName, className, methodParams);
                opts.dspUnwind    = cfg.JitUnwindDump().contains(methodName, className, methodParams);
                opts.dspEHTable   = cfg.JitEHDump().contains(methodName, className, methodParams);
                opts.dspDebugInfo = cfg.JitDebugDump().contains(methodName, className, methodParams);
            }
        }

        if (opts.disAsm && cfg.JitDisasmWithGC())
        {
            opts.disasmWithGC = true;
        }

#ifdef LATE_DISASM
        if (cfg.JitLateDisasm().contains(methodName, className, methodParams))
        {
            opts.doLateDisasm = true;
            codeGen->getDisAssembler().disOpenForLateDisAsm(methodName, className, methodParams->pSig);
        }
#endif

        opts.disDiffable  = cfg.DiffableDasm() != 0;
        opts.dspDiffable  = cfg.DiffableDasm() != 0;
        opts.disAddr      = cfg.JitDasmWithAddress() != 0;
        opts.disAlignment = cfg.JitDasmWithAlignmentBoundaries() != 0;

        const auto& dumpNameSet = opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT) ? cfg.NgenDump() : cfg.JitDump();
        const int   dumpHash = opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT) ? cfg.NgenHashDump() : cfg.JitHashDump();

        if (dumpNameSet.contains(methodName, className, methodParams) ||
            ((dumpHash != -1) && (static_cast<unsigned>(dumpHash) == info.compMethodHash())))
        {
            verbose      = true;
            verboseTrees = cfg.JitDumpVerboseTrees() == 1;
            verboseSsa   = cfg.JitDumpVerboseSsa() == 1;

            opts.dspCode    = true;
            opts.dspEHTable = true;
            opts.dspGCtbls  = true;
            opts.dspUnwind  = true;

            codeGen->setVerbose();
        }

        opts.optRepeat = cfg.JitOptRepeat().contains(methodName, className, methodParams);
    }

    if (verbose ||
        JitConfig.JitDebugBreak().contains(info.compMethodName, info.compClassName, &info.compMethodInfo->args) ||
        JitConfig.JitBreak().contains(info.compMethodName, info.compClassName, &info.compMethodInfo->args))
    {
        compDebugBreak = true;
    }

    expensiveDebugCheckLevel = JitConfig.JitExpensiveDebugCheckLevel();

    // If we're in a stress mode that modifies the flowgraph, make 1 the default.
    if ((expensiveDebugCheckLevel == 0) && (fgStressBBProf() || compStressCompile(STRESS_DO_WHILE_LOOPS, 30)))
    {
        expensiveDebugCheckLevel = 1;
    }

    opts.compGcChecks = (JitConfig.JitGCChecks() != 0) || compStressCompile(STRESS_GENERIC_VARN, 5);

#ifdef TARGET_XARCH
    enum
    {
        STACK_CHECK_ON_RETURN = 0x1,
        STACK_CHECK_ON_CALL   = 0x2,
        STACK_CHECK_ALL       = 0x3
    };

    int jitStackChecks = JitConfig.JitStackChecks();
    if (compStressCompile(STRESS_GENERIC_VARN, 5))
    {
        jitStackChecks = STACK_CHECK_ALL;
    }
    opts.compStackCheckOnRet = (jitStackChecks & STACK_CHECK_ON_RETURN) != 0;
    X86_ONLY(opts.compStackCheckOnCall = (jitStackChecks & STACK_CHECK_ON_CALL) != 0);

    opts.compEnablePCRelAddr = JitConfig.EnablePCRelAddr() != 0;
#endif // TARGET_XARCH
#endif // DEBUG
}

void Compiler::compInitOptions()
{
    assert(!compIsForInlining());

    JitFlags* jitFlags = opts.jitFlags;

    opts.optFlags = CLFLG_MAXOPT; // Default value is for full optimization

    if (jitFlags->IsSet(JitFlags::JIT_FLAG_DEBUG_CODE) || jitFlags->IsSet(JitFlags::JIT_FLAG_MIN_OPT) ||
        jitFlags->IsSet(JitFlags::JIT_FLAG_TIER0))
    {
        opts.optFlags = CLFLG_MINOPT;
    }
    // Don't optimize .cctors (except prejit) or if we're an inlinee
    else if (!jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT) && ((info.compFlags & FLG_CCTOR) == FLG_CCTOR))
    {
        opts.optFlags = CLFLG_MINOPT;
    }

    // Default value is to generate a blend of size and speed optimizations
    //
    opts.compCodeOpt = BLENDED_CODE;

    // If the EE sets SIZE_OPT or if we are compiling a Class constructor
    // we will optimize for code size at the expense of speed
    //
    if (jitFlags->IsSet(JitFlags::JIT_FLAG_SIZE_OPT) || ((info.compFlags & FLG_CCTOR) == FLG_CCTOR))
    {
        opts.compCodeOpt = SMALL_CODE;
    }
    //
    // If the EE sets SPEED_OPT we will optimize for speed at the expense of code size
    //
    else if (jitFlags->IsSet(JitFlags::JIT_FLAG_SPEED_OPT) ||
             (jitFlags->IsSet(JitFlags::JIT_FLAG_TIER1) && !jitFlags->IsSet(JitFlags::JIT_FLAG_MIN_OPT)))
    {
        opts.compCodeOpt = FAST_CODE;
        assert(!jitFlags->IsSet(JitFlags::JIT_FLAG_SIZE_OPT));
    }

    //-------------------------------------------------------------------------

    opts.compDbgCode = jitFlags->IsSet(JitFlags::JIT_FLAG_DEBUG_CODE);
    opts.compDbgInfo = jitFlags->IsSet(JitFlags::JIT_FLAG_DEBUG_INFO);
    opts.compDbgEnC  = jitFlags->IsSet(JitFlags::JIT_FLAG_DEBUG_EnC);

#if REGEN_SHORTCUTS || REGEN_CALLPAT
    // We never want to have debugging enabled when regenerating GC encoding patterns
    opts.compDbgCode = false;
    opts.compDbgInfo = false;
    opts.compDbgEnC  = false;
#endif

    compSetProcessor();

    lvaEnregEHVars = compEnregLocals() && JitConfig.EnableEHWriteThru();

#if DEBUG
    if (lvaEnregEHVars)
    {
        unsigned methHash   = info.compMethodHash();
        char*    lostr      = getenv("JitEHWTHashLo");
        unsigned methHashLo = 0;
        bool     dump       = false;
        if (lostr != nullptr)
        {
            sscanf_s(lostr, "%x", &methHashLo);
            dump = true;
        }
        char*    histr      = getenv("JitEHWTHashHi");
        unsigned methHashHi = UINT32_MAX;
        if (histr != nullptr)
        {
            sscanf_s(histr, "%x", &methHashHi);
            dump = true;
        }
        if (methHash < methHashLo || methHash > methHashHi)
        {
            lvaEnregEHVars = false;
        }
        else if (dump)
        {
            printf("Enregistering EH Vars for method %s, hash = 0x%x.\n", info.compFullName, info.compMethodHash());
            printf(""); // flush
        }
    }

    if (verbose)
    {
        printf("****** START compiling %s (MethodHash=%08x)\n", info.compFullName, info.compMethodHash());
        printf("Generating code for %s %s\n", Target::g_tgtPlatformName, Target::g_tgtCPUName);
        printf(""); // in our logic this causes a flush
    }

    if (JitConfig.JitBreak().contains(info.compMethodName, info.compClassName, &info.compMethodInfo->args))
    {
        assert(!"JitBreak reached");
    }

    unsigned jitHashBreakVal = (unsigned)JitConfig.JitHashBreak();
    if ((jitHashBreakVal != (DWORD)-1) && (jitHashBreakVal == info.compMethodHash()))
    {
        assert(!"JitHashBreak reached");
    }
#endif // DEBUG

#ifdef PROFILING_SUPPORTED
    opts.compNoPInvokeInlineCB = jitFlags->IsSet(JitFlags::JIT_FLAG_PROF_NO_PINVOKE_INLINE);

    // Cache the profiler handle
    if (jitFlags->IsSet(JitFlags::JIT_FLAG_PROF_ENTERLEAVE))
    {
        bool hookNeeded;
        bool indirected;
        info.compCompHnd->GetProfilingHandle(&hookNeeded, &compProfilerMethHnd, &indirected);
        compProfilerHookNeeded        = hookNeeded;
        compProfilerMethHndIndirected = indirected;
    }
    else
    {
        compProfilerHookNeeded        = false;
        compProfilerMethHnd           = nullptr;
        compProfilerMethHndIndirected = false;
    }

    // Honour COMPlus_JitELTHookEnabled or STRESS_PROFILER_CALLBACKS stress mode
    // only if VM has not asked us to generate profiler hooks in the first place.
    // That is, override VM only if it hasn't asked for a profiler callback for this method.
    // Don't run this stress mode when pre-JITing, as we would need to emit a relocation
    // for the call to the fake ELT hook, which wouldn't make sense, as we can't store that
    // in the pre-JIT image.
    if (!compProfilerHookNeeded)
    {
        if ((JitConfig.JitELTHookEnabled() != 0) ||
            (!jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT) && compStressCompile(STRESS_PROFILER_CALLBACKS, 5)))
        {
            opts.compJitELTHookEnabled = true;
        }
    }

    // TBD: Exclude PInvoke stubs
    if (opts.compJitELTHookEnabled)
    {
        compProfilerMethHnd           = (void*)DummyProfilerELTStub;
        compProfilerMethHndIndirected = false;
    }
#endif // PROFILING_SUPPORTED

    ARM_ONLY(opts.compUseSoftFP = jitFlags->IsSet(JitFlags::JIT_FLAG_SOFTFP_ABI) || JitConfig.JitSoftFP();)

    opts.compScopeInfo = opts.compDbgInfo;
    opts.compReloc     = jitFlags->IsSet(JitFlags::JIT_FLAG_RELOC);

#ifndef TARGET_ARM64
    // TODO-ARM64-NYI: enable hot/cold splitting
    if (jitFlags->IsSet(JitFlags::JIT_FLAG_PROCSPLIT))
    {
        // Note that opts.compdbgCode is true under ngen for checked assemblies!
        opts.compProcedureSplitting = !opts.compDbgCode;

#ifdef DEBUG
        // JitForceProcedureSplitting is used to force procedure splitting on checked assemblies.
        // This is useful for debugging on a checked build. Note that we still only do procedure
        // splitting in the zapper.
        if (JitConfig.JitForceProcedureSplitting().contains(info.compMethodName, info.compClassName,
                                                            &info.compMethodInfo->args))
        {
            opts.compProcedureSplitting = true;
        }

        // JitNoProcedureSplitting will always disable procedure splitting.
        if (JitConfig.JitNoProcedureSplitting().contains(info.compMethodName, info.compClassName,
                                                         &info.compMethodInfo->args))
        {
            opts.compProcedureSplitting = false;
        }

        // JitNoProcedureSplittingEH will disable procedure splitting in functions with EH.
        if (!JitConfig.JitNoProcedureSplittingEH().contains(info.compMethodName, info.compClassName,
                                                            &info.compMethodInfo->args))
        {
            opts.compProcedureSplittingEH = true;
        }
#endif
    }
#endif // !TARGET_ARM64

#ifdef DEBUG
#ifdef TARGET_ARM64
    if ((s_pJitMethodSet == nullptr) || s_pJitMethodSet->IsActiveMethod(info.compFullName, info.compMethodHash()))
    {
        opts.compJitSaveFpLrWithCalleeSavedRegisters = JitConfig.JitSaveFpLrWithCalleeSavedRegisters();
    }
#endif

    if (compStressCompile(STRESS_NULL_OBJECT_CHECK, 30))
    {
        compMaxUncheckedOffsetForNullObject = (size_t)JitConfig.JitMaxUncheckedOffset();

        if (verbose)
        {
            printf("STRESS_NULL_OBJECT_CHECK: compMaxUncheckedOffsetForNullObject=0x%X\n",
                   compMaxUncheckedOffsetForNullObject);
        }
    }

    if (verbose)
    {
        compDumpOptions();
    }
#endif
}

#ifdef DEBUG
void Compiler::compDumpOptions()
{
    // If we are compiling for a specific tier, make that very obvious in the output.
    // Note that we don't expect multiple TIER flags to be set at one time, but there
    // is nothing preventing that.
    if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_TIER0))
    {
        printf("OPTIONS: Tier-0 compilation (set COMPlus_TieredCompilation=0 to disable)\n");
    }
    if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_TIER1))
    {
        printf("OPTIONS: Tier-1 compilation\n");
    }
    if (compSwitchedToOptimized)
    {
        printf("OPTIONS: Tier-0 compilation, switched to FullOpts\n");
    }
    if (compSwitchedToMinOpts)
    {
        printf("OPTIONS: Tier-1/FullOpts compilation, switched to MinOpts\n");
    }

    if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_OSR))
    {
        printf("OPTIONS: OSR variant with entry point 0x%x\n", info.compILEntry);
    }

    printf("OPTIONS: compCodeOpt = %s\n", (opts.compCodeOpt == BLENDED_CODE)
                                              ? "BLENDED_CODE"
                                              : (opts.compCodeOpt == SMALL_CODE)
                                                    ? "SMALL_CODE"
                                                    : (opts.compCodeOpt == FAST_CODE) ? "FAST_CODE" : "UNKNOWN_CODE");

    printf("OPTIONS: compDbgCode = %s\n", dspBool(opts.compDbgCode));
    printf("OPTIONS: compDbgInfo = %s\n", dspBool(opts.compDbgInfo));
    printf("OPTIONS: compDbgEnC  = %s\n", dspBool(opts.compDbgEnC));
    printf("OPTIONS: compProcedureSplitting   = %s\n", dspBool(opts.compProcedureSplitting));
    printf("OPTIONS: compProcedureSplittingEH = %s\n", dspBool(opts.compProcedureSplittingEH));

    if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_BBOPT) && fgHaveProfileData())
    {
        printf("OPTIONS: optimized using %s profile data\n", pgoSourceToString(fgPgoSource));
    }

    if (fgPgoFailReason != nullptr)
    {
        printf("OPTIONS: %s\n", fgPgoFailReason);
    }

    if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT))
    {
        printf("OPTIONS: Jit invoked for ngen\n");
    }
}
#endif // DEBUG

void Compiler::compInitPgo()
{
    if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_BBOPT))
    {
        fgPgoQueryResult = info.compCompHnd->getPgoInstrumentationResults(info.compMethodHnd, &fgPgoSchema,
                                                                          &fgPgoSchemaCount, &fgPgoData, &fgPgoSource);

        // a failed result that also has a non-NULL fgPgoSchema
        // indicates that the ILSize for the method no longer matches
        // the ILSize for the method when profile data was collected.
        //
        // We will discard the IBC data in this case
        //
        if (FAILED(fgPgoQueryResult))
        {
            fgPgoFailReason = (fgPgoSchema != nullptr) ? "No matching PGO data" : "No PGO data";
            fgPgoData       = nullptr;
            fgPgoSchema     = nullptr;
        }
        // Optionally, disable use of profile data.
        //
        else if (JitConfig.JitDisablePgo() > 0)
        {
            fgPgoFailReason  = "PGO data available, but JitDisablePgo > 0";
            fgPgoQueryResult = E_FAIL;
            fgPgoData        = nullptr;
            fgPgoSchema      = nullptr;
        }
#ifdef DEBUG
        // Optionally, enable use of profile data for only some methods.
        //
        else
        {
            static ConfigMethodRange JitEnablePgoRange;
            JitEnablePgoRange.EnsureInit(JitConfig.JitEnablePgoRange());

            // Base this decision on the root method hash, so a method either sees all available
            // profile data (including that for inlinees), or none of it.
            //
            const unsigned hash = impInlineRoot()->info.compMethodHash();
            if (!JitEnablePgoRange.Contains(hash))
            {
                fgPgoFailReason  = "PGO data available, but method hash NOT within JitEnablePgoRange";
                fgPgoQueryResult = E_FAIL;
                fgPgoData        = nullptr;
                fgPgoSchema      = nullptr;
            }
        }

        // A successful result implies a non-NULL fgPgoSchema
        //
        if (SUCCEEDED(fgPgoQueryResult))
        {
            assert(fgPgoSchema != nullptr);
        }

        // A failed result implies a NULL fgPgoSchema
        //   see implementation of Compiler::fgHaveProfileData()
        //
        if (FAILED(fgPgoQueryResult))
        {
            assert(fgPgoSchema == nullptr);
        }
#endif
    }
}

//------------------------------------------------------------------------
// Determines if conditions are met to allow switching the opt level to optimized
//
// Return Value:
//    True if the opt level may be switched from tier 0 to optimized, false otherwise
//
// Assumptions:
//    - compInitOptions has been called
//    - compSetOptimizationLevel has not been called
//
// Notes:
//    This method is to be called at some point before compSetOptimizationLevel() to determine if the opt level may be
//    changed based on information gathered in early phases.
//
bool Compiler::compCanSwitchToOptimized()
{
    bool result = opts.jitFlags->IsSet(JitFlags::JIT_FLAG_TIER0) && !opts.jitFlags->IsSet(JitFlags::JIT_FLAG_MIN_OPT) &&
                  !opts.compDbgCode && !compIsForInlining();
    if (result)
    {
        // Ensure that it would be safe to change the opt level
        assert(opts.optFlags == CLFLG_MINOPT);
        assert(!opts.IsMinOptsSet());
    }

    return result;
}

//------------------------------------------------------------------------
// Switch the opt level from tier 0 to optimized
//
// Assumptions:
//    - compCanSwitchToOptimized is true
//    - compSetOptimizationLevel has not been called
//
// Notes:
//    This method is to be called at some point before compSetOptimizationLevel() to switch the opt level to optimized
//    based on information gathered in early phases.
//
void Compiler::compSwitchToOptimized()
{
    assert(compCanSwitchToOptimized());

    // Switch to optimized and re-init options
    JITDUMP("****\n**** JIT Tier0 jit request switching to Tier1 because of loop\n****\n");
    assert(opts.jitFlags->IsSet(JitFlags::JIT_FLAG_TIER0));
    opts.jitFlags->Clear(JitFlags::JIT_FLAG_TIER0);
    opts.jitFlags->Clear(JitFlags::JIT_FLAG_BBINSTR);

    INDEBUG(compSwitchedToOptimized = true);

    compInitOptions();

    // Notify the VM of the change
    info.compCompHnd->setMethodAttribs(info.compMethodHnd, CORINFO_FLG_SWITCHED_TO_OPTIMIZED);
}

#ifdef DEBUG

bool Compiler::compJitHaltMethod()
{
    /* This method returns true when we use an INS_BREAKPOINT to allow us to step into the generated native code */
    /* Note that this these two "Jit" environment variables also work for ngen images */

    if (JitConfig.JitHalt().contains(info.compMethodName, info.compClassName, &info.compMethodInfo->args))
    {
        return true;
    }

    /* Use this Hash variant when there are a lot of method with the same name and different signatures */

    unsigned fJitHashHaltVal = (unsigned)JitConfig.JitHashHalt();
    if ((fJitHashHaltVal != (unsigned)-1) && (fJitHashHaltVal == info.compMethodHash()))
    {
        return true;
    }

    return false;
}

/*****************************************************************************
 * Should we use a "stress-mode" for the given stressArea. We have different
 *   areas to allow the areas to be mixed in different combinations in
 *   different methods.
 * 'weight' indicates how often (as a percentage) the area should be stressed.
 *    It should reflect the usefulness:overhead ratio.
 */

const LPCWSTR Compiler::s_compStressModeNames[STRESS_COUNT + 1] = {
#define STRESS_MODE(mode) W("STRESS_") W(#mode),

    STRESS_MODES
#undef STRESS_MODE
};

//------------------------------------------------------------------------
// compStressCompile: determine if a stress mode should be enabled
//
// Arguments:
//   stressArea - stress mode to possibly enable
//   weight - percent of time this mode should be turned on
//     (range 0 to 100); weight 0 effectively disables
//
// Returns:
//   true if this stress mode is enabled
//
// Notes:
//   Methods may be excluded from stress via name or hash.
//
//   Particular stress modes may be disabled or forcibly enabled.
//
//   With JitStress=2, some stress modes are enabled regardless of weight;
//   these modes are the ones after COUNT_VARN in the enumeration.
//
//   For other modes or for nonzero JitStress values, stress will be
//   enabled selectively for roughly weight% of methods.
//
bool Compiler::compStressCompile(compStressArea stressArea, unsigned weight)
{
    // This can be called early, before info is fully set up.
    if ((info.compMethodName == nullptr) || (info.compFullName == nullptr))
    {
        return false;
    }

    // Inlinees defer to the root method for stress, so that we can
    // more easily isolate methods that cause stress failures.
    if (compIsForInlining())
    {
        return impInlineRoot()->compStressCompile(stressArea, weight);
    }

    const bool doStress = compStressCompileHelper(stressArea, weight);

    if (doStress && !compActiveStressModes[stressArea])
    {
        if (verbose)
        {
            printf("\n\n*** JitStress: %ws ***\n\n", s_compStressModeNames[stressArea]);
        }
        compActiveStressModes[stressArea] = true;
    }

    return doStress;
}

//------------------------------------------------------------------------
// compStressCompileHelper: helper to determine if a stress mode should be enabled
//
// Arguments:
//   stressArea - stress mode to possibly enable
//   weight - percent of time this mode should be turned on
//     (range 0 to 100); weight 0 effectively disables
//
// Returns:
//   true if this stress mode is enabled
//
// Notes:
//   See compStressCompile
//
bool Compiler::compStressCompileHelper(compStressArea stressArea, unsigned weight)
{
    if (!bRangeAllowStress)
    {
        return false;
    }

    if (!JitConfig.JitStressOnly().isEmpty() &&
        !JitConfig.JitStressOnly().contains(info.compMethodName, info.compClassName, &info.compMethodInfo->args))
    {
        return false;
    }

    // Does user explicitly prevent using this STRESS_MODE through the command line?
    const WCHAR* strStressModeNamesNot = JitConfig.JitStressModeNamesNot();
    if ((strStressModeNamesNot != nullptr) &&
        (wcsstr(strStressModeNamesNot, s_compStressModeNames[stressArea]) != nullptr))
    {
        return false;
    }

    // Does user explicitly set this STRESS_MODE through the command line?
    const WCHAR* strStressModeNames = JitConfig.JitStressModeNames();
    if (strStressModeNames != nullptr)
    {
        if (wcsstr(strStressModeNames, s_compStressModeNames[stressArea]) != nullptr)
        {
            return true;
        }

        // This stress mode name did not match anything in the stress
        // mode allowlist. If user has requested only enable mode,
        // don't allow this stress mode to turn on.
        const bool onlyEnableMode = JitConfig.JitStressModeNamesOnly() != 0;

        if (onlyEnableMode)
        {
            return false;
        }
    }

    // 0:   No stress (Except when explicitly set in complus_JitStressModeNames)
    // !=2: Vary stress. Performance will be slightly/moderately degraded
    // 2:   Check-all stress. Performance will be REALLY horrible
    const int stressLevel = getJitStressLevel();

    assert(weight <= MAX_STRESS_WEIGHT);

    // Check for boundary conditions
    if (stressLevel == 0 || weight == 0)
    {
        return false;
    }

    // Should we allow unlimited stress ?
    if ((stressArea > STRESS_COUNT_VARN) && (stressLevel == 2))
    {
        return true;
    }

    if (weight == MAX_STRESS_WEIGHT)
    {
        return true;
    }

    // Get a hash which can be compared with 'weight'
    assert(stressArea != 0);
    const unsigned hash = (info.compMethodHash() ^ stressArea ^ stressLevel) % MAX_STRESS_WEIGHT;

    assert(hash < MAX_STRESS_WEIGHT && weight <= MAX_STRESS_WEIGHT);
    return (hash < weight);
}

//------------------------------------------------------------------------
// compPromoteFewerStructs: helper to determine if the local
//   should not be promoted under a stress mode.
//
// Arguments:
//   lclNum - local number to test
//
// Returns:
//   true if this local should not be promoted.
//
// Notes:
//   Reject ~50% of the potential promotions if STRESS_PROMOTE_FEWER_STRUCTS is active.
//
bool Compiler::compPromoteFewerStructs(unsigned lclNum)
{
    bool       rejectThisPromo = false;
    const bool promoteLess     = compStressCompile(STRESS_PROMOTE_FEWER_STRUCTS, 50);
    if (promoteLess)
    {

        rejectThisPromo = (((info.compMethodHash() ^ lclNum) & 1) == 0);
    }
    return rejectThisPromo;
}

#endif // DEBUG

void Compiler::compInitDebuggingInfo()
{
    assert(!compIsForInlining());

    JITDUMP("*************** In compInitDebuggingInfo() for %s\n", info.compFullName);

    info.compVarScopesCount = 0;
    compEnterScopeList      = nullptr;
    compExitScopeList       = nullptr;

    if (opts.compScopeInfo)
    {
        eeGetVars();

        if ((info.compVarScopesCount != 0) && opts.compDbgCode)
        {
            // TODO-MIKE-Review: This was done for fgExtendDbgLifetimes which is gone now.
            // Can it be removed? Other places may rely on this block being present so...

            fgEnsureFirstBBisScratch();
            fgNewStmtAtEnd(fgFirstBB, gtNewNothingNode());

            JITDUMP("Debuggable code - Add " FMT_BB " to perform initialization of variables\n", fgFirstBB->bbNum);
        }
    }
}

void Importer::InitDebuggingInfo()
{
    // We can only report debug info for EnC at places where the stack is empty.
    // Actually, at places where there are not live temps. Else, we won't be able
    // to map between the old and the new versions correctly as we won't have
    // any info for the live temps.

    assert(!opts.compDbgEnC || !opts.compDbgInfo ||
           ((compStmtOffsetsImplicit & ~ICorDebugInfo::STACK_EMPTY_BOUNDARIES) == 0));

    if (opts.compDbgInfo)
    {
        eeGetStmtOffsets();

#ifdef DEBUG
        if (verbose)
        {
            printf("info.compStmtOffsetsCount    = %d\n", compStmtOffsetsCount);
            printf("info.compStmtOffsetsImplicit = %04Xh", compStmtOffsetsImplicit);

            if (compStmtOffsetsImplicit)
            {
                printf(" ( ");
                if (compStmtOffsetsImplicit & ICorDebugInfo::STACK_EMPTY_BOUNDARIES)
                {
                    printf("STACK_EMPTY ");
                }
                if (compStmtOffsetsImplicit & ICorDebugInfo::NOP_BOUNDARIES)
                {
                    printf("NOP ");
                }
                if (compStmtOffsetsImplicit & ICorDebugInfo::CALL_SITE_BOUNDARIES)
                {
                    printf("CALL_SITE ");
                }
                printf(")");
            }
            printf("\n");
            IL_OFFSET* pOffs = compStmtOffsets;
            for (unsigned i = 0; i < compStmtOffsetsCount; i++, pOffs++)
            {
                printf("%02d) IL_%04Xh\n", i, *pOffs);
            }
        }
#endif
    }
}

void Compiler::compSetOptimizationLevel(const ILStats& ilStats)
{
    assert(!compIsForInlining());

#pragma warning(suppress : 4101)
    unsigned jitMinOpts;

    bool theMinOptsValue = false;

    if (opts.optFlags == CLFLG_MINOPT)
    {
        JITLOG((LL_INFO100, "CLFLG_MINOPT set for method %s\n", info.compFullName));
        theMinOptsValue = true;
    }

#ifdef DEBUG
    jitMinOpts = JitConfig.JitMinOpts();

    if (!theMinOptsValue && (jitMinOpts > 0))
    {
        // jitTotalMethodCompiled does not include the method that is being compiled now, so make +1.
        unsigned methodCount     = Compiler::jitTotalMethodCompiled + 1;
        unsigned methodCountMask = methodCount & 0xFFF;
        unsigned kind            = (jitMinOpts & 0xF000000) >> 24;
        switch (kind)
        {
            default:
                if (jitMinOpts <= methodCount)
                {
                    if (verbose)
                    {
                        printf(" Optimizations disabled by JitMinOpts and methodCount\n");
                    }
                    theMinOptsValue = true;
                }
                break;
            case 0xD:
            {
                unsigned firstMinopts  = (jitMinOpts >> 12) & 0xFFF;
                unsigned secondMinopts = (jitMinOpts >> 0) & 0xFFF;

                if ((firstMinopts == methodCountMask) || (secondMinopts == methodCountMask))
                {
                    if (verbose)
                    {
                        printf("0xD: Optimizations disabled by JitMinOpts and methodCountMask\n");
                    }
                    theMinOptsValue = true;
                }
            }
            break;
            case 0xE:
            {
                unsigned startMinopts = (jitMinOpts >> 12) & 0xFFF;
                unsigned endMinopts   = (jitMinOpts >> 0) & 0xFFF;

                if ((startMinopts <= methodCountMask) && (endMinopts >= methodCountMask))
                {
                    if (verbose)
                    {
                        printf("0xE: Optimizations disabled by JitMinOpts and methodCountMask\n");
                    }
                    theMinOptsValue = true;
                }
            }
            break;
            case 0xF:
            {
                unsigned bitsZero = (jitMinOpts >> 12) & 0xFFF;
                unsigned bitsOne  = (jitMinOpts >> 0) & 0xFFF;

                if (((methodCountMask & bitsOne) == bitsOne) && ((~methodCountMask & bitsZero) == bitsZero))
                {
                    if (verbose)
                    {
                        printf("0xF: Optimizations disabled by JitMinOpts and methodCountMask\n");
                    }
                    theMinOptsValue = true;
                }
            }
            break;
        }
    }

    if (!theMinOptsValue)
    {
        if (JitConfig.JitMinOptsName().contains(info.compMethodName, info.compClassName, &info.compMethodInfo->args))
        {
            theMinOptsValue = true;
        }
    }

#if 0
    // The code in this #if can be used to debug optimization issues according to method hash.
    // To use, uncomment, rebuild and set environment variables minoptshashlo and minoptshashhi.
#ifdef DEBUG
    unsigned methHash = info.compMethodHash();
    char* lostr = getenv("minoptshashlo");
    unsigned methHashLo = 0;
    if (lostr != nullptr)
    {
        sscanf_s(lostr, "%x", &methHashLo);
        char* histr = getenv("minoptshashhi");
        unsigned methHashHi = UINT32_MAX;
        if (histr != nullptr)
        {
            sscanf_s(histr, "%x", &methHashHi);
            if (methHash >= methHashLo && methHash <= methHashHi)
            {
                printf("MinOpts for method %s, hash = %08x.\n",
                    info.compFullName, methHash);
                printf("");         // in our logic this causes a flush
                theMinOptsValue = true;
            }
        }
    }
#endif
#endif

    if (compStressCompile(STRESS_MIN_OPTS, 5))
    {
        theMinOptsValue = true;
    }
    // For PREJIT we never drop down to MinOpts
    // unless unless CLFLG_MINOPT is set
    else if (!opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT))
    {
        if ((unsigned)JitConfig.JitMinOptsCodeSize() < info.compILCodeSize)
        {
            JITLOG((LL_INFO10, "IL Code Size exceeded, using MinOpts for method %s\n", info.compFullName));
            theMinOptsValue = true;
        }
        else if ((unsigned)JitConfig.JitMinOptsInstrCount() < ilStats.instrCount)
        {
            JITLOG((LL_INFO10, "IL instruction count exceeded, using MinOpts for method %s\n", info.compFullName));
            theMinOptsValue = true;
        }
        else if ((unsigned)JitConfig.JitMinOptsBbCount() < fgBBcount)
        {
            JITLOG((LL_INFO10, "Basic Block count exceeded, using MinOpts for method %s\n", info.compFullName));
            theMinOptsValue = true;
        }
        else if ((unsigned)JitConfig.JitMinOptsLvNumCount() < lvaCount)
        {
            JITLOG((LL_INFO10, "Local Variable Num count exceeded, using MinOpts for method %s\n", info.compFullName));
            theMinOptsValue = true;
        }
        else if ((unsigned)JitConfig.JitMinOptsLvRefCount() < ilStats.lclRefCount)
        {
            JITLOG((LL_INFO10, "Local Variable Ref count exceeded, using MinOpts for method %s\n", info.compFullName));
            theMinOptsValue = true;
        }
        if (theMinOptsValue == true)
        {
            JITLOG((LL_INFO10000, "IL Code Size,Instr %4d,%4d, Basic Block count %3d, Local Variable Num,Ref count "
                                  "%3d,%3d for method %s\n",
                    info.compILCodeSize, ilStats.instrCount, fgBBcount, lvaCount, ilStats.lclRefCount,
                    info.compFullName));
            if (JitConfig.JitBreakOnMinOpts() != 0)
            {
                assert(!"MinOpts enabled");
            }
        }
    }
#else  // !DEBUG
    // Retail check if we should force Minopts due to the complexity of the method
    // For PREJIT we never drop down to MinOpts
    // unless unless CLFLG_MINOPT is set
    if (!theMinOptsValue && !opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT) &&
        ((DEFAULT_MIN_OPTS_CODE_SIZE < info.compILCodeSize) || (DEFAULT_MIN_OPTS_INSTR_COUNT < ilStats.instrCount) ||
         (DEFAULT_MIN_OPTS_BB_COUNT < fgBBcount) || (DEFAULT_MIN_OPTS_LV_NUM_COUNT < lvaCount) ||
         (DEFAULT_MIN_OPTS_LV_REF_COUNT < ilStats.lclRefCount)))
    {
        theMinOptsValue = true;
    }
#endif // DEBUG

    JITLOG((LL_INFO10000,
            "IL Code Size,Instr %4d,%4d, Basic Block count %3d, Local Variable Num,Ref count %3d,%3d for method %s\n",
            info.compILCodeSize, ilStats.instrCount, fgBBcount, lvaCount, ilStats.lclRefCount, info.compFullName));

#if 0
    // The code in this #if has been useful in debugging loop cloning issues, by
    // enabling selective enablement of the loop cloning optimization according to
    // method hash.
#ifdef DEBUG
    if (!theMinOptsValue)
    {
        unsigned methHash = info.compMethodHash();
        char* lostr = getenv("opthashlo");
        unsigned methHashLo = 0;
        if (lostr != NULL)
        {
            sscanf_s(lostr, "%x", &methHashLo);
            // methHashLo = (unsigned(atoi(lostr)) << 2);  // So we don't have to use negative numbers.
        }
        char* histr = getenv("opthashhi");
        unsigned methHashHi = UINT32_MAX;
        if (histr != NULL)
        {
            sscanf_s(histr, "%x", &methHashHi);
            // methHashHi = (unsigned(atoi(histr)) << 2);  // So we don't have to use negative numbers.
        }
        if (methHash < methHashLo || methHash > methHashHi)
        {
            theMinOptsValue = true;
        }
        else
        {
            printf("Doing optimization in  in %s (0x%x).\n", info.compFullName, methHash);
        }
    }
#endif
#endif

    // Set the MinOpts value
    opts.SetMinOpts(theMinOptsValue);

    // Notify the VM if MinOpts is being used when not requested
    if (theMinOptsValue && !opts.jitFlags->IsSet(JitFlags::JIT_FLAG_TIER0) &&
        !opts.jitFlags->IsSet(JitFlags::JIT_FLAG_MIN_OPT) && !opts.compDbgCode)
    {
        info.compCompHnd->setMethodAttribs(info.compMethodHnd, CORINFO_FLG_SWITCHED_TO_MIN_OPT);
        opts.jitFlags->Clear(JitFlags::JIT_FLAG_TIER1);
        INDEBUG(compSwitchedToMinOpts = true);
    }

    JITDUMP("OPTIONS: opts.MinOpts() == %s\n", opts.MinOpts() ? "true" : "false");

    if (opts.OptimizationDisabled())
    {
        opts.optFlags = CLFLG_MINOPT;
    }

    if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT))
    {
        // The JIT doesn't currently support loop alignment for prejitted images.
        // (The JIT doesn't know the final address of the code, hence
        // it can't align code based on unknown addresses.)

        codeGen->SetAlignLoops(false); // loop alignment not supported for prejitted code
    }
    else
    {
        codeGen->SetAlignLoops(JitConfig.JitAlignLoops() == 1);
    }

#if TARGET_ARM
    // A single JitStress=1 Linux ARM32 test fails when we expand virtual calls early
    // JIT\HardwareIntrinsics\General\Vector128_1\Vector128_1_ro
    opts.compExpandCallsEarly = (JitConfig.JitExpandCallsEarly() == 2);
#else
    opts.compExpandCallsEarly      = (JitConfig.JitExpandCallsEarly() != 0);
#endif
}

#ifdef DEBUG
//------------------------------------------------------------------------
// compGetTieringName: get a string describing tiered compilation settings
//   for this method
//
// Arguments:
//   wantShortName - true if a short name is ok (say for using in file names)
//
// Returns:
//   String describing tiering decisions for this method, including cases
//   where the jit codegen will differ from what the runtime requested.
//
const char* Compiler::compGetTieringName(bool wantShortName) const
{
    const bool tier0 = opts.jitFlags->IsSet(JitFlags::JIT_FLAG_TIER0);
    const bool tier1 = opts.jitFlags->IsSet(JitFlags::JIT_FLAG_TIER1);
    assert(!tier0 || !tier1); // We don't expect multiple TIER flags to be set at one time.

    if (tier0)
    {
        return "Tier0";
    }
    else if (tier1)
    {
        if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_OSR))
        {
            return "Tier1-OSR";
        }
        else
        {
            return "Tier1";
        }
    }
    else if (opts.OptimizationEnabled())
    {
        if (compSwitchedToOptimized)
        {
            return wantShortName ? "Tier0-FullOpts" : "Tier-0 switched to FullOpts";
        }
        else
        {
            return "FullOpts";
        }
    }
    else if (opts.MinOpts())
    {
        if (compSwitchedToMinOpts)
        {
            if (compSwitchedToOptimized)
            {
                return wantShortName ? "Tier0-FullOpts-MinOpts" : "Tier-0 switched to FullOpts, then to MinOpts";
            }
            else
            {
                return wantShortName ? "Tier0-MinOpts" : "Tier-0 switched MinOpts";
            }
        }
        else
        {
            return "MinOpts";
        }
    }
    else if (opts.compDbgCode)
    {
        return "Debug";
    }
    else
    {
        return wantShortName ? "Unknown" : "Unknown optimization level";
    }
}

//------------------------------------------------------------------------
// compGetStressMessage: get a string describing jitstress capability
//   for this method
//
// Returns:
//   An empty string if stress is not enabled, else a string describing
//   if this method is subject to stress or is excluded by name or hash.
//
const char* Compiler::compGetStressMessage() const
{
    // Add note about stress where appropriate
    const char* stressMessage = "";

    // Is stress enabled via mode name or level?
    if ((JitConfig.JitStressModeNames() != nullptr) || (getJitStressLevel() > 0))
    {
        // Is the method being jitted excluded from stress via range?
        if (bRangeAllowStress)
        {
            // Or is it excluded via name?
            if (!JitConfig.JitStressOnly().isEmpty() ||
                !JitConfig.JitStressOnly().contains(info.compMethodName, info.compClassName,
                                                    &info.compMethodInfo->args))
            {
                // Not excluded -- stress can happen
                stressMessage = " JitStress";
            }
            else
            {
                stressMessage = " NoJitStress(Only)";
            }
        }
        else
        {
            stressMessage = " NoJitStress(Range)";
        }
    }

    return stressMessage;
}

void Compiler::compFunctionTraceStart()
{
    assert(!compIsForInlining());

    if ((JitConfig.JitFunctionTrace() != 0) && !opts.disDiffable)
    {
        LONG newJitNestingLevel = InterlockedIncrement(&Compiler::jitNestingLevel);
        if (newJitNestingLevel <= 0)
        {
            printf("{ Illegal nesting level %d }\n", newJitNestingLevel);
        }

        for (LONG i = 0; i < newJitNestingLevel - 1; i++)
        {
            printf("  ");
        }
        printf("{ Start Jitting Method %4d %s (MethodHash=%08x) %s\n", Compiler::jitTotalMethodCompiled,
               info.compFullName, info.compMethodHash(),
               compGetTieringName()); /* } editor brace matching workaround for this printf */
    }
}

void Compiler::compFunctionTraceEnd(void* methodCodePtr, ULONG methodCodeSize, bool isNYI)
{
    assert(!compIsForInlining());

    if ((JitConfig.JitFunctionTrace() != 0) && !opts.disDiffable)
    {
        LONG newJitNestingLevel = InterlockedDecrement(&Compiler::jitNestingLevel);
        if (newJitNestingLevel < 0)
        {
            printf("{ Illegal nesting level %d }\n", newJitNestingLevel);
        }

        for (LONG i = 0; i < newJitNestingLevel; i++)
        {
            printf("  ");
        }

        // Note: that is incorrect if we are compiling several methods at the same time.
        unsigned methodNumber = Compiler::jitTotalMethodCompiled - 1;

        /* { editor brace-matching workaround for following printf */
        printf("} Jitted Method %4d at" FMT_ADDR "method %s size %08x%s%s\n", methodNumber, DBG_ADDR(methodCodePtr),
               info.compFullName, methodCodeSize, isNYI ? " NYI" : "", opts.altJit ? " altjit" : "");
    }
}
#endif // DEBUG

//------------------------------------------------------------------------
// BeginPhase: begin execution of a phase
//
// Arguments:
//    phase - the phase that is about to begin
//
void Compiler::BeginPhase(Phases phase)
{
    mostRecentlyActivePhase = phase;
}

//------------------------------------------------------------------------
// EndPhase: finish execution of a phase
//
// Arguments:
//    phase - the phase that has just finished
//
void Compiler::EndPhase(Phases phase)
{
#if defined(FEATURE_JIT_METHOD_PERF)
    if (pCompJitTimer != nullptr)
    {
        pCompJitTimer->EndPhase(this, phase);
    }
#endif

    mostRecentlyActivePhase = phase;
}

void Compiler::compCompile(void** nativeCode, uint32_t* nativeCodeSize, JitFlags* jitFlags)
{
    assert(!compIsForInlining());

    DoPhase(this, PHASE_INCPROFILE, &Compiler::fgIncorporateProfileData);

    if (jitFlags->IsSet(JitFlags::JIT_FLAG_BBINSTR))
    {
        DoPhase(this, PHASE_IBCPREP, &Compiler::fgPrepareToInstrumentMethod);
    }

    DoPhase(this, PHASE_IMPORTATION, &Compiler::fgImport);

    if (jitFlags->IsSet(JitFlags::JIT_FLAG_BBINSTR))
    {
        DoPhase(this, PHASE_IBCINSTR, &Compiler::fgInstrumentMethod);
    }

    DoPhase(this, PHASE_INDXCALL, &Compiler::fgTransformIndirectCalls);
    DoPhase(this, PHASE_PATCHPOINTS, &Compiler::fgTransformPatchpoints);

#if !FEATURE_EH
    // If we aren't yet supporting EH in a compiler bring-up, remove as many EH handlers as possible,
    // so we can pass tests that contain try/catch EH, but don't actually throw any exceptions.
    fgRemoveEH();
#endif // !FEATURE_EH

    DoPhase(this, PHASE_REMOVE_NOT_IMPORTED, &Compiler::phRemoveNotImportedBlocks);
    DoPhase(this, PHASE_MORPH_INLINE, &Compiler::fgInline);

    RecordStateAtEndOfInlining();

    DoPhase(this, PHASE_ALLOCATE_OBJECTS, &Compiler::phMorphAllocObj);
    DoPhase(this, PHASE_MORPH_ADD_INTERNAL, &Compiler::fgAddInternal);

    if (opts.OptimizationEnabled() && (compHndBBtabCount != 0))
    {
        DoPhase(this, PHASE_EMPTY_TRY, &Compiler::fgRemoveEmptyTry);
        DoPhase(this, PHASE_EMPTY_FINALLY, &Compiler::fgRemoveEmptyFinally);
        DoPhase(this, PHASE_MERGE_FINALLY_CHAINS, &Compiler::fgMergeFinallyChains);
        DoPhase(this, PHASE_CLONE_FINALLY, &Compiler::fgCloneFinally);
#if defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
        DoPhase(this, PHASE_UPDATE_FINALLY_FLAGS, &Compiler::fgUpdateFinallyTargetFlags);
#endif
    }

    DoPhase(this, PHASE_COMPUTE_PREDS, &Compiler::phComputePreds);

    if (opts.OptimizationEnabled())
    {
        DoPhase(this, PHASE_MERGE_THROWS, &Compiler::fgTailMergeThrows);
        DoPhase(this, PHASE_EARLY_UPDATE_FLOW_GRAPH, &Compiler::phUpdateFlowGraph);
    }

    DoPhase(this, PHASE_PROMOTE_STRUCTS, &Compiler::fgPromoteStructs);
    DoPhase(this, PHASE_STR_ADRLCL, &Compiler::fgMarkAddressExposedLocals);
    DoPhase(this, PHASE_MORPH_GLOBAL, &Compiler::phMorph);

    if (getNeedsGSSecurityCookie())
    {
        DoPhase(this, PHASE_GS_COOKIE, &Compiler::phGSCookie);
    }

    DoPhase(this, PHASE_COMPUTE_EDGE_WEIGHTS, &Compiler::fgComputeBlockAndEdgeWeights);
#ifdef FEATURE_EH_FUNCLETS
    DoPhase(this, PHASE_CREATE_FUNCLETS, &Compiler::fgCreateFunclets);
#endif

    if (opts.OptimizationEnabled())
    {
        DoPhase(this, PHASE_INVERT_LOOPS, &Compiler::optInvertLoops);
        DoPhase(this, PHASE_OPTIMIZE_LAYOUT, &Compiler::optOptimizeLayout);
        DoPhase(this, PHASE_COMPUTE_REACHABILITY, &Compiler::fgComputeReachability);
        DoPhase(this, PHASE_COMPUTE_DOMINATORS, &Compiler::fgComputeDoms);
        DoPhase(this, PHASE_FIND_LOOPS, &Compiler::optFindLoops);
        DoPhase(this, PHASE_CLONE_LOOPS, &Compiler::optCloneLoops);
        DoPhase(this, PHASE_UNROLL_LOOPS, &Compiler::optUnrollLoops);
    }

    INDEBUG(fgDebugCheckLinks());

    DoPhase(this, PHASE_ADD_LOCAL_VARS, &Compiler::phAddSpecialLocals);

    if (!opts.OptimizationEnabled())
    {
        DoPhase(this, PHASE_IMPLICIT_REF_LOCAL_VARS, &Compiler::phImplicitRefLocals);
    }
    else
    {
        DoPhase(this, PHASE_REF_COUNT_LOCAL_VARS, &Compiler::phRefCountLocals);
#if ASSERTION_PROP
        DoPhase(this, PHASE_ADD_COPIES, &Compiler::optAddCopies);
#endif

        DoPhase(this, PHASE_OPTIMIZE_BOOLS, &Compiler::optOptimizeBools);

        // optOptimizeBools() might have changed the number of blocks;
        // the dominators/reachability might be bad.
        // TODO-MIKE-Review: So should fgDomsComputed be set to false?
    }

    DoPhase(this, PHASE_FIND_OPER_ORDER, &Compiler::phFindOperOrder);
    DoPhase(this, PHASE_SET_BLOCK_ORDER, &Compiler::phSetBlockOrder);

    // TODO-MIKE-Review: Can this be done after the SSA optimizations? Those can remove
    // dead code and we may end up with fully interruptible code for no reason.
    // But this depends on BBF_LOOP_HEAD, which is set only by fgComputeReachability.
    // And optRemoveRedundantZeroInits depends on the code not being fully interruptible.
    DoPhase(this, PHASE_SET_FULLY_INTERRUPTIBLE, &Compiler::phSetFullyInterruptible);

    if (opts.OptimizationEnabled())
    {
#ifdef OPT_CONFIG
        const bool     doSsa           = (JitConfig.JitDoSsa() != 0);
        const bool     doEarlyProp     = doSsa && (JitConfig.JitDoEarlyProp() != 0);
        const bool     doValueNum      = doSsa && (JitConfig.JitDoValueNumber() != 0);
        const bool     doLoopHoisting  = doValueNum && (JitConfig.JitDoLoopHoisting() != 0);
        const bool     doCopyProp      = doValueNum && (JitConfig.JitDoCopyProp() != 0);
        const bool     doBranchOpt     = doValueNum && (JitConfig.JitDoRedundantBranchOpts() != 0);
        const bool     doCse           = doValueNum && (JitConfig.JitNoCSE() == 0);
        const bool     doAssertionProp = doValueNum && (JitConfig.JitDoAssertionProp() != 0);
        const bool     doRangeAnalysis = doAssertionProp && (JitConfig.JitDoRangeAnalysis() != 0);
        const unsigned iterationCount  = !opts.optRepeat ? 1 : static_cast<unsigned>(JitConfig.JitOptRepeatCount());

        for (unsigned iteration = 0; iteration < iterationCount; iteration++)
#else
        const bool doSsa           = true;
        const bool doEarlyProp     = true;
        const bool doValueNum      = true;
        const bool doLoopHoisting  = true;
        const bool doCopyProp      = true;
        const bool doBranchOpt     = true;
        const bool doCse           = true;
        const bool doAssertionProp = true;
        const bool doRangeAnalysis = true;
#endif
        {
#ifdef OPT_CONFIG
            if (iteration != 0)
            {
                fgSsaReset();
            }
#endif

            if (doSsa)
            {
                DoPhase(this, PHASE_BUILD_SSA, &Compiler::fgSsaBuild);
            }

            if (doEarlyProp)
            {
                DoPhase(this, PHASE_EARLY_PROP, &Compiler::optEarlyProp);
            }

            if (doValueNum)
            {
                DoPhase(this, PHASE_VALUE_NUMBER, &Compiler::fgValueNumber);
            }

            if (doLoopHoisting)
            {
                DoPhase(this, PHASE_HOIST_LOOP_CODE, &Compiler::optHoistLoopCode);
            }

            if (doCopyProp)
            {
                DoPhase(this, PHASE_VN_COPY_PROP, &Compiler::optVnCopyProp);
            }

            if (doBranchOpt)
            {
                DoPhase(this, PHASE_OPTIMIZE_BRANCHES, &Compiler::optRedundantBranches);
            }

            if (doCse)
            {
                DoPhase(this, PHASE_OPTIMIZE_VALNUM_CSES, &Compiler::cseMain);
            }

#if ASSERTION_PROP
            if (doAssertionProp)
            {
                DoPhase(this, PHASE_ASSERTION_PROP_MAIN, &Compiler::apMain);
            }

            if (doRangeAnalysis)
            {
                DoPhase(this, PHASE_OPTIMIZE_INDEX_CHECKS, &Compiler::phRemoveRangeCheck);
            }
#endif // ASSERTION_PROP

            if (doSsa)
            {
                DoPhase(this, PHASE_DESTROY_SSA, &Compiler::fgSsaDestroy);
            }

            if (fgModified)
            {
                DoPhase(this, PHASE_OPT_UPDATE_FLOW_GRAPH, &Compiler::phUpdateFlowGraph);
                DoPhase(this, PHASE_COMPUTE_EDGE_WEIGHTS2, &Compiler::fgComputeEdgeWeights);
            }
        }

        ssaForm        = false;
        fgDomsComputed = false;
    }

    assert(!fgDomsComputed);

    if ((optMethodFlags & OMF_NEEDS_GCPOLLS) != 0)
    {
        DoPhase(this, PHASE_INSERT_GC_POLLS, &Compiler::phInsertGCPolls);
    }

    if (opts.compProcedureSplitting)
    {
        DoPhase(this, PHASE_DETERMINE_FIRST_COLD_BLOCK, &Compiler::phDetermineFirstColdBlock);
    }

    DoPhase(this, PHASE_RATIONALIZE, &Compiler::phRationalize);
    DoPhase(this, PHASE_LOWERING, &Compiler::phLower);
#if !FEATURE_FIXED_OUT_ARGS
    DoPhase(this, PHASE_STACK_LEVEL_SETTER, &Compiler::phSetThrowHelperBlockStackLevel);
#endif

    codeGen->genGenerateCode(nativeCode, nativeCodeSize);

    mostRecentlyActivePhase = PHASE_POST_EMIT;

#ifdef FEATURE_JIT_METHOD_PERF
    if (pCompJitTimer != nullptr)
    {
#if MEASURE_CLRAPI_CALLS
        EndPhase(PHASE_CLR_API);
#else
        EndPhase(PHASE_POST_EMIT);
#endif
        pCompJitTimer->Terminate(this, CompTimeSummaryInfo::s_compTimeSummary, true);
    }
#endif

    if (doesMethodHavePatchpoints())
    {
        generatePatchpointInfo();
    }

    RecordStateAtEndOfCompilation();

    INDEBUG(++Compiler::jitTotalMethodCompiled);
}

//------------------------------------------------------------------------
// generatePatchpointInfo: allocate and fill in patchpoint info data,
//    and report it to the VM
//
void Compiler::generatePatchpointInfo()
{
    assert(doesMethodHavePatchpoints());

    // Patchpoints are only found in Tier0 code, which is unoptimized, and so
    // should always have frame pointer.
    assert(codeGen->isFramePointerUsed());

    // Allocate patchpoint info storage from runtime, and fill in initial bits of data.
    const unsigned        patchpointInfoSize = PatchpointInfo::ComputeSize(info.compLocalsCount);
    PatchpointInfo* const patchpointInfo     = (PatchpointInfo*)info.compCompHnd->allocateArray(patchpointInfoSize);

    // The +TARGET_POINTER_SIZE here is to account for the extra slot the runtime
    // creates when it simulates calling the OSR method (the "pseudo return address" slot).
    patchpointInfo->Initialize(info.compLocalsCount, codeGen->genSPtoFPdelta() + TARGET_POINTER_SIZE);

    JITDUMP("--OSR--- FP-SP delta is %d\n", patchpointInfo->FpToSpDelta());

    // We record offsets for all the "locals" here. Could restrict
    // this to just the IL locals with some extra logic, and save a bit of space,
    // but would need to adjust all consumers, too.
    for (unsigned lclNum = 0; lclNum < info.compLocalsCount; lclNum++)
    {
        LclVarDsc* const varDsc = lvaGetDesc(lclNum);

        // We expect all these to have stack homes, and be FP relative
        assert(varDsc->lvOnFrame);
        assert(varDsc->lvFramePointerBased);

        // Record FramePtr relative offset (no localloc yet)
        patchpointInfo->SetOffset(lclNum, varDsc->GetStackOffset());

        // Note if IL stream contained an address-of that potentially leads to exposure.
        // This bit of IL may be skipped by OSR partial importation.
        if (varDsc->lvHasLdAddrOp)
        {
            patchpointInfo->SetIsExposed(lclNum);
        }

        JITDUMP("--OSR-- V%02u is at offset %d%s\n", lclNum, patchpointInfo->Offset(lclNum),
                patchpointInfo->IsExposed(lclNum) ? " (exposed)" : "");
    }

    // Special offsets
    //
    if (lvaReportParamTypeArg())
    {
        const int offset = codeGen->cachedGenericContextArgOffset;
        patchpointInfo->SetGenericContextArgOffset(offset);
        JITDUMP("--OSR-- cached generic context offset is FP %d\n", patchpointInfo->GenericContextArgOffset());
    }

    if (lvaKeepAliveAndReportThis())
    {
        const int offset = codeGen->cachedGenericContextArgOffset;
        patchpointInfo->SetKeptAliveThisOffset(offset);
        JITDUMP("--OSR-- kept-alive this offset is FP %d\n", patchpointInfo->KeptAliveThisOffset());
    }

    if (compGSReorderStackLayout)
    {
        assert(lvaGSSecurityCookie != BAD_VAR_NUM);
        LclVarDsc* const varDsc = lvaGetDesc(lvaGSSecurityCookie);
        patchpointInfo->SetSecurityCookieOffset(varDsc->GetStackOffset());
        JITDUMP("--OSR-- security cookie V%02u offset is FP %d\n", lvaGSSecurityCookie,
                patchpointInfo->SecurityCookieOffset());
    }

    // Register this with the runtime.
    info.compCompHnd->setPatchpointInfo(patchpointInfo);
}

#ifdef DEBUG

bool CompiledMethodInfo::SkipMethod() const
{
    static ConfigMethodRange fJitRange;
    fJitRange.EnsureInit(JitConfig.JitRange());
    assert(!fJitRange.Error());

    // Normally JitConfig.JitRange() is null, we don't want to skip
    // jitting any methods.
    //
    // So, the logic below relies on the fact that a null range string
    // passed to ConfigMethodRange represents the set of all methods.

    if (!fJitRange.Contains(compMethodHash()))
    {
        return true;
    }

    if (JitConfig.JitExclude().contains(compMethodName, compClassName, &compMethodInfo->args))
    {
        return true;
    }

    if (!JitConfig.JitInclude().isEmpty() &&
        !JitConfig.JitInclude().contains(compMethodName, compClassName, &compMethodInfo->args))
    {
        return true;
    }

    return false;
}

#endif

CorJitResult Compiler::compCompileMain(void** nativeCode, uint32_t* nativeCodeSize, JitFlags* jitFlags)
{
    // Verification isn't supported
    assert(jitFlags->IsSet(JitFlags::JIT_FLAG_SKIP_VERIFICATION));
    assert(!jitFlags->IsSet(JitFlags::JIT_FLAG_IMPORT_ONLY));

    assert(!compIsForInlining());

    assert(s_helperCallProperties.IsPure(CORINFO_HELP_GETSHARED_GCSTATIC_BASE));
    assert(!s_helperCallProperties.IsPure(CORINFO_HELP_GETFIELDOBJ)); // quick sanity check

    INDEBUG(compDoComponentUnitTestsOnce());

#ifdef FEATURE_JIT_METHOD_PERF
    static bool checkedForJitTimeLog = false;

    if (!checkedForJitTimeLog)
    {
        // Call into VM to get the config strings. FEATURE_JIT_METHOD_PERF is enabled for
        // retail builds. Do not call the regular Config helper here as it would pull
        // in a copy of the config parser into the clrjit.dll.
        InterlockedCompareExchangeT(&Compiler::compJitTimeLogFilename,
                                    (LPCWSTR)info.compCompHnd->getJitTimeLogFilename(), NULL);

        // At a process or module boundary clear the file and start afresh.
        JitTimer::PrintCsvHeader();

        checkedForJitTimeLog = true;
    }

    if ((Compiler::compJitTimeLogFilename != nullptr) || (JitTimeLogCsv() != nullptr))
    {
        pCompJitTimer = JitTimer::Create(this, info.compMethodInfo->ILCodeSize);
    }
#endif // FEATURE_JIT_METHOD_PERF

    // Set this early so we can use it without relying on random memory values
    INDEBUG(verbose = false);

#if FUNC_INFO_LOGGING
    LPCWSTR tmpJitFuncInfoFilename = JitConfig.JitFuncInfoFile();

    if (tmpJitFuncInfoFilename != nullptr)
    {
        LPCWSTR oldFuncInfoFileName =
            InterlockedCompareExchangeT(&compJitFuncInfoFilename, tmpJitFuncInfoFilename, nullptr);

        if (oldFuncInfoFileName == nullptr)
        {
            assert(compJitFuncInfoFile == nullptr);
            compJitFuncInfoFile = _wfopen(compJitFuncInfoFilename, W("a"));

            if (compJitFuncInfoFile == nullptr)
            {
#if defined(DEBUG) && !defined(HOST_UNIX) // no 'perror' in the PAL
                perror("Failed to open JitFuncInfoLogFile");
#endif
            }
        }
    }
#endif // FUNC_INFO_LOGGING

    if (jitFlags->IsSet(JitFlags::JIT_FLAG_OSR))
    {
        info.compPatchpointInfo = info.compCompHnd->getOSRInfo(&info.compILEntry);

        assert(info.compPatchpointInfo != nullptr);
    }

    info.compMatchedVM = (info.compCompHnd->getExpectedTargetArchitecture() == IMAGE_FILE_MACHINE_TARGET) &&
                         (eeGetEEInfo()->osType == CORINFO_OS_TARGET);

    // If we are not compiling for a matched VM, then we are getting JIT flags that don't match our target
    // architecture. The two main examples here are an ARM targeting altjit hosted on x86 and an ARM64
    // targeting altjit hosted on x64. (Though with cross-bitness work, the host doesn't necessarily need
    // to be of the same bitness.) In these cases, we need to fix up the JIT flags to be appropriate for
    // the target, as the VM's expected target may overlap bit flags with different meaning to our target.
    // Note that it might be better to do this immediately when setting the JIT flags in CILJit::compileMethod()
    // (when JitFlags::SetFromFlags() is called), but this is close enough. (To move this logic to
    // CILJit::compileMethod() would require moving the info.compMatchedVM computation there as well.)

    if (!info.compMatchedVM)
    {
#ifdef TARGET_ARM
// Currently there are no ARM flags that conflict with other flags.
#endif

#ifdef TARGET_ARM64
        // The x86/x64 architecture capabilities flags overlap with the ARM64 ones. Set a reasonable architecture
        // target default. Currently this is disabling all ARM64 architecture features except FP and SIMD, but this
        // should be altered to possibly enable all of them, when they are known to all work.
        CORINFO_InstructionSetFlags defaultArm64Flags;
        defaultArm64Flags.AddInstructionSet(InstructionSet_ArmBase);
        defaultArm64Flags.AddInstructionSet(InstructionSet_AdvSimd);
        defaultArm64Flags.Set64BitInstructionSetVariants();
        jitFlags->SetInstructionSetFlags(defaultArm64Flags);
#endif
    }

    compMaxUncheckedOffsetForNullObject = eeGetEEInfo()->maxUncheckedOffsetForNullObject;

    info.compProfilerCallback = false; // Assume false until we are told to hook this method.

    // Set the context for token lookup.
    impTokenLookupContextHandle = METHOD_BEING_COMPILED_CONTEXT();

    info.compClassHnd  = info.compCompHnd->getMethodClass(info.compMethodHnd);
    info.compClassAttr = info.compCompHnd->getClassAttribs(info.compClassHnd);

#ifdef DEBUG
    if (JitConfig.EnableExtraSuperPmiQueries())
    {
        // This call to getClassModule/getModuleAssembly/getAssemblyName fails in crossgen2 due
        // to these APIs being unimplemented. So disable this extra info for pre-jit mode.
        // See https://github.com/dotnet/runtime/issues/48888.

        if (!jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT))
        {
            // Get the assembly name, to aid finding any particular SuperPMI method context function.
            info.compCompHnd->getAssemblyName(
                info.compCompHnd->getModuleAssembly(info.compCompHnd->getClassModule(info.compClassHnd)));

            // Fetch class names for the method's generic parameters.
            CORINFO_SIG_INFO sig;
            info.compCompHnd->getMethodSig(info.compMethodHnd, &sig, nullptr);

            for (unsigned i = 0; i < sig.sigInst.classInstCount; i++)
            {
                eeGetClassName(sig.sigInst.classInst[i]);
            }

            for (unsigned i = 0; i < sig.sigInst.methInstCount; i++)
            {
                eeGetClassName(sig.sigInst.methInst[i]);
            }
        }
    }

    if (info.SkipMethod())
    {
        return CORJIT_SKIPPED;
    }
#endif // DEBUG

    struct Param : ErrorTrapParam
    {
        Compiler*    compiler;
        void**       nativeCode;
        uint32_t*    nativeCodeSize;
        JitFlags*    jitFlags;
        CorJitResult result = CORJIT_INTERNALERROR;
    } param;

    param.jitInfo        = info.compCompHnd;
    param.compiler       = this;
    param.nativeCode     = nativeCode;
    param.nativeCodeSize = nativeCodeSize;
    param.jitFlags       = jitFlags;

    PAL_TRY(Param&, p, param)
    {
        p.result = p.compiler->compCompileHelper(p.nativeCode, p.nativeCodeSize, p.jitFlags);
    }
    PAL_FINALLY
    {
    }
    PAL_ENDTRY

    return param.result;
}

#if defined(DEBUG) || defined(INLINE_DATA)
//------------------------------------------------------------------------
// compMethodHash: get hash code for currently jitted method
//
// Returns:
//    Hash based on method's full name
//
unsigned CompiledMethodInfo::compMethodHash() const
{
    if (compMethodHashPrivate == 0)
    {
        // compMethodHashPrivate = compCompHnd->getMethodHash(compMethodHnd);
        assert(compFullName != nullptr);
        assert(*compFullName != 0);
        COUNT_T hash = HashStringA(compFullName); // Use compFullName to generate the hash, as it contains the signature
                                                  // and return type
        compMethodHashPrivate = hash;
    }
    return compMethodHashPrivate;
}

//------------------------------------------------------------------------
// compMethodHash: get hash code for specified method
//
// Arguments:
//    methodHnd - method of interest
//
// Returns:
//    Hash based on method's full name
//
unsigned Compiler::compMethodHash(CORINFO_METHOD_HANDLE methodHnd)
{
    // If this is the root method, delegate to the caching version
    //
    if (methodHnd == info.compMethodHnd)
    {
        return info.compMethodHash();
    }

    // Else compute from scratch. Might consider caching this too.
    //
    unsigned    methodHash = 0;
    const char* calleeName = eeGetMethodFullName(methodHnd);

    if (calleeName != nullptr)
    {
        methodHash = HashStringA(calleeName);
    }
    else
    {
        methodHash = info.compCompHnd->getMethodHash(methodHnd);
    }

    return methodHash;
}

#endif // defined(DEBUG) || defined(INLINE_DATA)

void Compiler::compCompileFinish()
{
#if FUNC_INFO_LOGGING
    if (compJitFuncInfoFile != nullptr)
    {
        assert(!compIsForInlining());
#ifdef DEBUG // We only have access to info.compFullName in DEBUG builds.
        fprintf(compJitFuncInfoFile, "%s\n", info.compFullName);
#elif FEATURE_SIMD
        fprintf(compJitFuncInfoFile, " %s\n", eeGetMethodFullName(info.compMethodHnd));
#endif
        fprintf(compJitFuncInfoFile, ""); // in our logic this causes a flush
    }
#endif // FUNC_INFO_LOGGING

#if defined(DEBUG) || MEASURE_NODE_SIZE || MEASURE_BLOCK_SIZE || DISPLAY_SIZES
    genMethodCnt++;
#endif

#if MEASURE_MEM_ALLOC
    {
        compArenaAllocator->finishMemStats();
        memAllocHist.record((unsigned)((compArenaAllocator->getTotalBytesAllocated() + 1023) / 1024));
        memUsedHist.record((unsigned)((compArenaAllocator->getTotalBytesUsed() + 1023) / 1024));
    }

#ifdef DEBUG
    if (verbose || JitConfig.DisplayMemStats())
    {
        printf("\nAllocations for %s (MethodHash=%08x)\n", info.compFullName, info.compMethodHash());
        compArenaAllocator->dumpMemStats(jitstdout);
    }
#endif // DEBUG
#endif // MEASURE_MEM_ALLOC

#if LOOP_HOIST_STATS
    AddLoopHoistStats();
#endif // LOOP_HOIST_STATS

#if MEASURE_NODE_SIZE
    genTreeNcntHist.record(static_cast<unsigned>(genNodeSizeStatsPerFunc.genTreeNodeCnt));
    genTreeNsizHist.record(static_cast<unsigned>(genNodeSizeStatsPerFunc.genTreeNodeSize));
#endif

#if defined(DEBUG)
    // Small methods should fit in ArenaAllocator::getDefaultPageSize(), or else
    // we should bump up ArenaAllocator::getDefaultPageSize()

    if ((info.compILCodeSize <= 32) &&     // Is it a reasonably small method?
        (info.compNativeCodeSize < 512) && // Some trivial methods generate huge native code. eg. pushing a single huge
                                           // struct
        (compInlinedCodeSize <= 128) &&    // Is the the inlining reasonably bounded?
                                           // Small methods cannot meaningfully have a big number of locals
                                           // or arguments. We always track arguments at the start of
                                           // the prolog which requires memory
        (info.compLocalsCount <= 32) && (!opts.MinOpts()) && // We may have too many local variables, etc
        (getJitStressLevel() == 0) &&                        // We need extra memory for stress
        !opts.optRepeat &&                                   // We need extra memory to repeat opts
        !compArenaAllocator->bypassHostAllocator() && // ArenaAllocator::getDefaultPageSize() is artificially low for
                                                      // DirectAlloc
        // Factor of 2x is because data-structures are bigger under DEBUG
        (compArenaAllocator->getTotalBytesAllocated() > (2 * ArenaAllocator::getDefaultPageSize())) &&
        // RyuJIT backend needs memory tuning! TODO-Cleanup: remove this case when memory tuning is complete.
        (compArenaAllocator->getTotalBytesAllocated() > (10 * ArenaAllocator::getDefaultPageSize())) &&
        !verbose) // We allocate lots of memory to convert sets to strings for JitDump
    {
        genSmallMethodsNeedingExtraMemoryCnt++;

        // Less than 1% of all methods should run into this.
        // We cannot be more strict as there are always degenerate cases where we
        // would need extra memory (like huge structs as locals - see lvaSetStruct()).
        assert((genMethodCnt < 500) || (genSmallMethodsNeedingExtraMemoryCnt < (genMethodCnt / 100)));
    }
#endif // DEBUG

#if defined(DEBUG) || defined(INLINE_DATA)

    m_inlineStrategy->DumpData();

    if (JitConfig.JitInlineDumpXmlFile() != nullptr)
    {
        FILE* file = _wfopen(JitConfig.JitInlineDumpXmlFile(), W("a"));
        if (file != nullptr)
        {
            m_inlineStrategy->DumpXml(file);
            fclose(file);
        }
        else
        {
            m_inlineStrategy->DumpXml();
        }
    }
    else
    {
        m_inlineStrategy->DumpXml();
    }

#endif

#ifdef DEBUG
    if (opts.dspOrder)
    {
        // mdMethodDef __stdcall CEEInfo::getMethodDefFromMethod(CORINFO_METHOD_HANDLE hMethod)
        mdMethodDef currentMethodToken = info.compCompHnd->getMethodDefFromMethod(info.compMethodHnd);

        static bool headerPrinted = false;
        if (!headerPrinted)
        {
            // clang-format off
            headerPrinted = true;
            printf("         |  Profiled   | Method   |   Method has    |   calls   | Num |LclV |AProp| CSE |   Perf  |bytes | %3s codesize| \n", Target::g_tgtCPUName);
            printf(" mdToken |  CNT |  RGN |    Hash  | EH | FRM | LOOP | NRM | IND | BBs | Cnt | Cnt | Cnt |  Score  |  IL  |   HOT | CLD | method name \n");
            printf("---------+------+------+----------+----+-----+------+-----+-----+-----+-----+-----+-----+---------+------+-------+-----+\n");
            //      06001234 | 1234 |  HOT | 0f1e2d3c | EH | ebp | LOOP |  15 |   6 |  12 |  17 |  12 |   8 | 1234.56 |  145 |  1234 | 123 | System.Example(int)
            // clang-format on
        }

        printf("%08X | ", currentMethodToken);

        if (fgHaveProfileData())
        {
            if (fgCalledCount < 1000)
            {
                printf("%4.0f | ", fgCalledCount);
            }
            else if (fgCalledCount < 1000000)
            {
                printf("%3.0fK | ", fgCalledCount / 1000);
            }
            else
            {
                printf("%3.0fM | ", fgCalledCount / 1000000);
            }
        }
        else
        {
            printf("     | ");
        }

        CorInfoRegionKind regionKind = info.compMethodInfo->regionKind;

        if (opts.altJit)
        {
            printf("ALT | ");
        }
        else if (regionKind == CORINFO_REGION_NONE)
        {
            printf("     | ");
        }
        else if (regionKind == CORINFO_REGION_HOT)
        {
            printf(" HOT | ");
        }
        else if (regionKind == CORINFO_REGION_COLD)
        {
            printf("COLD | ");
        }
        else if (regionKind == CORINFO_REGION_JIT)
        {
            printf(" JIT | ");
        }
        else
        {
            printf("UNKN | ");
        }

        printf("%08x | ", info.compMethodHash());

        if (compHndBBtabCount > 0)
        {
            printf("EH | ");
        }
        else
        {
            printf("   | ");
        }

        if (codeGen->isFramePointerUsed())
        {
            printf("%3s | ", STR_FPBASE);
        }
#if DOUBLE_ALIGN
        else if (codeGen->doDoubleAlign())
        {
            printf("dbl | ");
        }
#endif
        else
        {
            printf("%3s | ", STR_SPBASE);
        }

        if (fgHasLoops)
        {
            printf("LOOP |");
        }
        else
        {
            printf("     |");
        }

        printf(" %3d |", optCallCount);
        printf(" %3d |", optIndirectCallCount);
        printf(" %3d |", fgBBcountAtCodegen);
        printf(" %3d |", lvaCount);

        if (opts.MinOpts())
        {
            printf("  MinOpts  |");
        }
        else
        {
            printf(" %3d |", apAssertionCount);
            printf(" %3d |", cseCount);
        }

        if (info.compPerfScore < 9999.995)
        {
            printf(" %7.2f |", info.compPerfScore);
        }
        else
        {
            printf(" %7.0f |", info.compPerfScore);
        }

        printf(" %4d |", info.compMethodInfo->ILCodeSize);
        printf(" %5d |", info.compTotalHotCodeSize);
        printf(" %3d |", info.compTotalColdCodeSize);

        printf(" %s\n", eeGetMethodFullName(info.compMethodHnd));
        printf(""); // in our logic this causes a flush
    }

    if (verbose)
    {
        printf("****** DONE compiling %s\n", info.compFullName);
        printf(""); // in our logic this causes a flush
    }

    // Only call _DbgBreakCheck when we are jitting, not when we are ngen-ing
    // For ngen the int3 or breakpoint instruction will be right at the
    // start of the ngen method and we will stop when we execute it.
    //
    if (!opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT))
    {
        if (compJitHaltMethod())
        {
#if !defined(HOST_UNIX)
            // TODO-UNIX: re-enable this when we have an OS that supports a pop-up dialog

            // Don't do an assert, but just put up the dialog box so we get just-in-time debugger
            // launching.  When you hit 'retry' it will continue and naturally stop at the INT 3
            // that the JIT put in the code
            _DbgBreakCheck(__FILE__, __LINE__, "JitHalt");
#endif
        }
    }
#endif // DEBUG
}

#ifdef PSEUDORANDOM_NOP_INSERTION
// this is zlib adler32 checksum.  source came from windows base

#define BASE 65521L // largest prime smaller than 65536
#define NMAX 5552
// NMAX is the largest n such that 255n(n+1)/2 + (n+1)(BASE-1) <= 2^32-1

#define DO1(buf, i)                                                                                                    \
    {                                                                                                                  \
        s1 += buf[i];                                                                                                  \
        s2 += s1;                                                                                                      \
    }
#define DO2(buf, i)                                                                                                    \
    DO1(buf, i);                                                                                                       \
    DO1(buf, i + 1);
#define DO4(buf, i)                                                                                                    \
    DO2(buf, i);                                                                                                       \
    DO2(buf, i + 2);
#define DO8(buf, i)                                                                                                    \
    DO4(buf, i);                                                                                                       \
    DO4(buf, i + 4);
#define DO16(buf)                                                                                                      \
    DO8(buf, 0);                                                                                                       \
    DO8(buf, 8);

unsigned adler32(unsigned adler, char* buf, unsigned int len)
{
    unsigned int s1 = adler & 0xffff;
    unsigned int s2 = (adler >> 16) & 0xffff;
    int          k;

    if (buf == NULL)
        return 1L;

    while (len > 0)
    {
        k = len < NMAX ? len : NMAX;
        len -= k;
        while (k >= 16)
        {
            DO16(buf);
            buf += 16;
            k -= 16;
        }
        if (k != 0)
            do
            {
                s1 += *buf++;
                s2 += s1;
            } while (--k);
        s1 %= BASE;
        s2 %= BASE;
    }
    return (s2 << 16) | s1;
}
#endif

unsigned getMethodBodyChecksum(__in_z char* code, int size)
{
#ifdef PSEUDORANDOM_NOP_INSERTION
    return adler32(0, code, size);
#else
    return 0;
#endif
}

CorJitResult Compiler::compCompileHelper(void** nativeCode, uint32_t* nativeCodeSize, JitFlags* jitFlags)
{
    assert(!compIsForInlining());

    CORINFO_METHOD_HANDLE methodHnd = info.compMethodHnd;

    if (info.compILCodeSize == 0)
    {
        BADCODE("code size is zero");
    }

    info.compFlags = info.compCompHnd->getMethodAttribs(info.compMethodHnd);
#ifdef PSEUDORANDOM_NOP_INSERTION
    info.compChecksum = getMethodBodyChecksum((char*)methodInfo->ILCode, methodInfo->ILCodeSize);
#endif

    opts.jitFlags = jitFlags;
    compInitAltJit();
    compInitConfigOptions();
    compInitOptions();

    if (!opts.altJit && opts.jitFlags->IsSet(JitFlags::JIT_FLAG_ALT_JIT))
    {
        // We're an altjit, but the COMPlus_AltJit configuration did not say to compile this method,
        // so skip it.
        return CORJIT_SKIPPED;
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("IL to import:\n");
        dumpILRange(info.compCode, info.compILCodeSize);
    }
#endif

    if (JitConfig.JitAggressiveInlining())
    {
        compDoAggressiveInlining = true;
    }

    if (compDoAggressiveInlining)
    {
        info.compFlags |= CORINFO_FLG_FORCEINLINE;
    }

#ifdef DEBUG
    if (compStressCompile(STRESS_FORCE_INLINE, 0))
    {
        info.compFlags |= CORINFO_FLG_FORCEINLINE;
    }
#endif

    info.compIsStatic         = (info.compFlags & CORINFO_FLG_STATIC) != 0;
    info.compInitMem          = (info.compMethodInfo->options & CORINFO_OPT_INIT_LOCALS) != 0;
    info.compPublishStubParam = opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PUBLISH_SECRET_PARAM);

    if (opts.IsReversePInvoke())
    {
        bool unused;
        info.compCallConv = info.compCompHnd->getUnmanagedCallConv(info.compMethodInfo->ftn, nullptr, &unused);
    }
    else
    {
        info.compCallConv = CorInfoCallConvExtension::Managed;
    }

    switch (info.compMethodInfo->args.getCallConv())
    {
        case CORINFO_CALLCONV_NATIVEVARARG:
        case CORINFO_CALLCONV_VARARG:
            info.compIsVarArgs = true;
            break;
        default:
            break;
    }

    lvaInitTable();
    compInitDebuggingInfo();
    compInitPgo();

    ILStats ilStats;

    if (!opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT))
    {
        // We are jitting the root method.
        compCreateBasicBlocks(ilStats);
    }
    else
    {
        // We're prejitting the root method. We also will analyze it as
        // a potential inline candidate.
        InlineResult prejitResult(this, methodHnd, "prejit");

        // Profile data allows us to avoid early "too many IL bytes" outs.
        prejitResult.NoteBool(InlineObservation::CALLSITE_HAS_PROFILE, fgHaveSufficientProfileData());

        // Do the initial inline screen.
        impCanInlineIL(methodHnd, info.compMethodInfo, (info.compFlags & CORINFO_FLG_FORCEINLINE) != 0, &prejitResult);

        // Temporarily install the prejitResult as the
        // compInlineResult so it's available to fgFindJumpTargets
        // and can accumulate more observations as the IL is
        // scanned.
        //
        // We don't pass prejitResult in as a parameter to avoid
        // potential aliasing confusion -- the other call to
        // compCreateBasicBlocksmay have set up compInlineResult and
        // the code in fgFindJumpTargets references that data
        // member extensively.
        assert(compInlineResult == nullptr);
        assert(impInlineInfo == nullptr);
        compInlineResult = &prejitResult;

        // Find the basic blocks. We must do this regardless of
        // inlineability, since we are prejitting this method.
        //
        // This will also update the status of this method as
        // an inline candidate.
        compCreateBasicBlocks(ilStats);

        // Undo the temporary setup.
        assert(compInlineResult == &prejitResult);
        compInlineResult = nullptr;

        // If still a viable, discretionary inline, assess
        // profitability.
        if (prejitResult.IsDiscretionaryCandidate())
        {
            prejitResult.DetermineProfitability(info.compMethodInfo);
        }

        m_inlineStrategy->NotePrejitDecision(prejitResult);

        // Handle the results of the inline analysis.
        if (prejitResult.IsFailure())
        {
            // This method is a bad inlinee according to our
            // analysis.  We will let the InlineResult destructor
            // mark it as noinline in the prejit image to save the
            // jit some work.
            //
            // This decision better not be context-dependent.
            assert(prejitResult.IsNever());
        }
        else
        {
            // This looks like a viable inline candidate.  Since
            // we're not actually inlining, don't report anything.
            prejitResult.SetReported();
        }
    }

    if (compHasBackwardJump && (info.compFlags & CORINFO_FLG_DISABLE_TIER0_FOR_LOOPS) != 0 &&
        compCanSwitchToOptimized())
    {
        // Method likely has a loop, switch to the OptimizedTier to avoid spending too much time running slower code
        compSwitchToOptimized();
    }

    compSetOptimizationLevel(ilStats);

#if COUNT_BASIC_BLOCKS
    bbCntTable.record(fgBBcount);

    if (fgBBcount == 1)
    {
        bbOneBBSizeTable.record(methodInfo->ILCodeSize);
    }
#endif // COUNT_BASIC_BLOCKS

#ifdef DEBUG
    if (verbose)
    {
        printf("Basic block list for '%s'\n", info.compFullName);
        fgDispBasicBlocks();
    }

    if (opts.disAsm || verbose)
    {
        compMethodID = ~info.compMethodHash() & 0xffff;
    }
    else
    {
        compMethodID = InterlockedIncrement(&s_compMethodsCount);
    }

    if (JitConfig.DumpJittedMethods() == 1)
    {
        printf("Compiling %4d %s::%s, IL size = %u, hash=0x%08x %s%s%s\n", Compiler::jitTotalMethodCompiled,
               info.compClassName, info.compMethodName, info.compILCodeSize, info.compMethodHash(),
               compGetTieringName(), opts.IsOSR() ? " OSR" : "", compGetStressMessage());
    }
#endif

    INDEBUG(compFunctionTraceStart());
    compCompile(nativeCode, nativeCodeSize, jitFlags);
    INDEBUG(compFunctionTraceEnd(*nativeCode, *nativeCodeSize, false));
    JITDUMP("Method code size: %u\n", *nativeCodeSize);
    compCompileFinish();

    // Did we just compile for a target architecture that the VM isn't expecting? If so, the VM
    // can't used the generated code (and we better be an AltJit!).

    if (!info.compMatchedVM)
    {
        return CORJIT_SKIPPED;
    }

#ifdef DEBUG
    if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_ALT_JIT) && (JitConfig.RunAltJitCode() == 0))
    {
        return CORJIT_SKIPPED;
    }
#endif

    return CORJIT_OK;
}

/*****************************************************************************/

#if MEASURE_CLRAPI_CALLS

struct WrapICorJitInfo : public ICorJitInfo
{
private:
    Compiler*    wrapComp;
    ICorJitInfo* wrapHnd; // the "real thing"

    WrapICorJitInfo(Compiler* compiler) : wrapComp(compiler), wrapHnd(compiler->info.compCompHnd)
    {
    }

public:
    static void WrapJitInfo(Compiler* compiler)
    {
        if (JitConfig.JitEECallTimingInfo() != 0)
        {
            // If you get a build error here due to 'WrapICorJitInfo' being
            // an abstract class, it's very likely that the wrapper bodies
            // in ICorJitInfo_API_wrapper.hpp are no longer in sync with
            // the EE interface; please be kind and update the header file.
            compiler->info.compCompHnd = new (compiler) WrapICorJitInfo(compiler);
        }
    }

#include "ICorJitInfo_API_wrapper.hpp"
};

#endif // MEASURE_CLRAPI_CALLS

/*****************************************************************************/

// Compile a single method

CorJitResult jitNativeCode(ICorJitInfo*         jitInfo,
                           CORINFO_METHOD_INFO* methodInfo,
                           void**               nativeCode,
                           uint32_t*            nativeCodeSize,
                           JitFlags*            jitFlags)
{
    bool jitFallbackCompile = false;

START:
    struct Param : ErrorTrapParam
    {
        ArenaAllocator       allocator;
        Compiler*            compiler     = nullptr;
        Compiler*            prevCompiler = nullptr;
        bool                 jitFallbackCompile;
        CORINFO_METHOD_INFO* methodInfo;
        void**               nativeCode;
        uint32_t*            nativeCodeSize;
        JitFlags*            jitFlags;
        CorJitResult         result = CORJIT_INTERNALERROR;
        CORINFO_EE_INFO      eeInfo;
    } param;

    param.jitInfo            = jitInfo;
    param.jitFallbackCompile = jitFallbackCompile;
    param.methodInfo         = methodInfo;
    param.nativeCode         = nativeCode;
    param.nativeCodeSize     = nativeCodeSize;
    param.jitFlags           = jitFlags;

    PAL_TRY(Param&, p, param)
    {
        NestedErrorTrapParam<Param&> param2(p);

        PAL_TRY(NestedErrorTrapParam<Param&>&, p2, param2)
        {
            Param& p = p2.param;

            p.jitInfo->getEEInfo(&p.eeInfo);

            p.compiler = static_cast<Compiler*>(p.allocator.allocateMemory(sizeof(Compiler)));
            new (p.compiler) Compiler(&p.allocator, &p.eeInfo, p.methodInfo, p.jitInfo);
            p.prevCompiler = JitTls::GetCompiler();
            JitTls::SetCompiler(p.compiler);
            INDEBUG(JitTls::SetLogCompiler(p.compiler));

#if MEASURE_CLRAPI_CALLS
            if (!p.jitFallbackCompile)
            {
                WrapICorJitInfo::WrapJitInfo(p.compiler);
            }
#endif

            p.compiler->compInitMethodName();
            p.compiler->compInit();
            INDEBUG(p.compiler->jitFallbackCompile = p.jitFallbackCompile;)
            p.result = p.compiler->compCompileMain(p.nativeCode, p.nativeCodeSize, p.jitFlags);
        }
        PAL_FINALLY
        {
            // If OOM is thrown when allocating the Compiler object, we will
            // end up here and p.compiler will be nullptr.

            if (p.compiler != nullptr)
            {
                p.compiler->info.compCode = nullptr;

                assert(JitTls::GetCompiler() == p.compiler);
                JitTls::SetCompiler(p.prevCompiler);
            }

            p.allocator.destroy();
        }
        PAL_ENDTRY
    }
    PAL_EXCEPT_FILTER(JitErrorTrapFilter)
    {
        param.result = param.error;
    }
    PAL_ENDTRY

    CorJitResult result = param.result;

    if (!jitFallbackCompile &&
        ((result == CORJIT_INTERNALERROR) || (result == CORJIT_RECOVERABLEERROR) || (result == CORJIT_IMPLLIMITATION)))
    {
        // If we failed the JIT, reattempt with debuggable code.
        jitFallbackCompile = true;

        // Update the flags for 'safer' code generation.
        jitFlags->Set(JitFlags::JIT_FLAG_MIN_OPT);
        jitFlags->Clear(JitFlags::JIT_FLAG_SIZE_OPT);
        jitFlags->Clear(JitFlags::JIT_FLAG_SPEED_OPT);

        goto START;
    }

    return result;
}

// JIT time end to end, and by phases.

#ifdef FEATURE_JIT_METHOD_PERF
// Static variables
CritSecObject       CompTimeSummaryInfo::s_compTimeSummaryLock;
CompTimeSummaryInfo CompTimeSummaryInfo::s_compTimeSummary;
#if MEASURE_CLRAPI_CALLS
double JitTimer::s_cyclesPerSec = CachedCyclesPerSecond();
#endif
#endif // FEATURE_JIT_METHOD_PERF

#if defined(FEATURE_JIT_METHOD_PERF) || DUMP_FLOWGRAPHS
const char* PhaseNames[] = {
#define CompPhaseNameMacro(enum_nm, string_nm, short_nm, hasChildren, parent, measureIR) string_nm,
#include "compphases.h"
};

const char* PhaseEnums[] = {
#define CompPhaseNameMacro(enum_nm, string_nm, short_nm, hasChildren, parent, measureIR) #enum_nm,
#include "compphases.h"
};

const LPCWSTR PhaseShortNames[] = {
#define CompPhaseNameMacro(enum_nm, string_nm, short_nm, hasChildren, parent, measureIR) W(short_nm),
#include "compphases.h"
};
#endif // defined(FEATURE_JIT_METHOD_PERF) || DUMP_FLOWGRAPHS

#ifdef FEATURE_JIT_METHOD_PERF
bool PhaseHasChildren[] = {
#define CompPhaseNameMacro(enum_nm, string_nm, short_nm, hasChildren, parent, measureIR) hasChildren,
#include "compphases.h"
};

int PhaseParent[] = {
#define CompPhaseNameMacro(enum_nm, string_nm, short_nm, hasChildren, parent, measureIR) parent,
#include "compphases.h"
};

bool PhaseReportsIRSize[] = {
#define CompPhaseNameMacro(enum_nm, string_nm, short_nm, hasChildren, parent, measureIR) measureIR,
#include "compphases.h"
};

CompTimeInfo::CompTimeInfo(unsigned byteCodeBytes)
    : m_byteCodeBytes(byteCodeBytes)
    , m_totalCycles(0)
    , m_parentPhaseEndSlop(0)
    , m_timerFailure(false)
#if MEASURE_CLRAPI_CALLS
    , m_allClrAPIcalls(0)
    , m_allClrAPIcycles(0)
#endif
{
    for (int i = 0; i < PHASE_NUMBER_OF; i++)
    {
        m_invokesByPhase[i] = 0;
        m_cyclesByPhase[i]  = 0;
#if MEASURE_CLRAPI_CALLS
        m_CLRinvokesByPhase[i] = 0;
        m_CLRcyclesByPhase[i]  = 0;
#endif
    }

#if MEASURE_CLRAPI_CALLS
    assert(ARRAYSIZE(m_perClrAPIcalls) == API_ICorJitInfo_Names::API_COUNT);
    assert(ARRAYSIZE(m_perClrAPIcycles) == API_ICorJitInfo_Names::API_COUNT);
    assert(ARRAYSIZE(m_maxClrAPIcycles) == API_ICorJitInfo_Names::API_COUNT);
    for (int i = 0; i < API_ICorJitInfo_Names::API_COUNT; i++)
    {
        m_perClrAPIcalls[i]  = 0;
        m_perClrAPIcycles[i] = 0;
        m_maxClrAPIcycles[i] = 0;
    }
#endif
}

bool CompTimeSummaryInfo::IncludedInFilteredData(CompTimeInfo& info)
{
    return false; // info.m_byteCodeBytes < 10;
}

//------------------------------------------------------------------------
// CompTimeSummaryInfo::AddInfo: Record timing info from one compile.
//
// Arguments:
//    info          - The timing information to record.
//    includePhases - If "true", the per-phase info in "info" is valid,
//                    which means that a "normal" compile has ended; if
//                    the value is "false" we are recording the results
//                    of a partial compile (typically an import-only run
//                    on behalf of the inliner) in which case the phase
//                    info is not valid and so we only record EE call
//                    overhead.
void CompTimeSummaryInfo::AddInfo(CompTimeInfo& info, bool includePhases)
{
    if (info.m_timerFailure)
    {
        return; // Don't update if there was a failure.
    }

    CritSecHolder timeLock(s_compTimeSummaryLock);

    if (includePhases)
    {
        bool includeInFiltered = IncludedInFilteredData(info);

        m_numMethods++;

        // Update the totals and maxima.
        m_total.m_byteCodeBytes += info.m_byteCodeBytes;
        m_maximum.m_byteCodeBytes = max(m_maximum.m_byteCodeBytes, info.m_byteCodeBytes);
        m_total.m_totalCycles += info.m_totalCycles;
        m_maximum.m_totalCycles = max(m_maximum.m_totalCycles, info.m_totalCycles);

#if MEASURE_CLRAPI_CALLS
        // Update the CLR-API values.
        m_total.m_allClrAPIcalls += info.m_allClrAPIcalls;
        m_maximum.m_allClrAPIcalls = max(m_maximum.m_allClrAPIcalls, info.m_allClrAPIcalls);
        m_total.m_allClrAPIcycles += info.m_allClrAPIcycles;
        m_maximum.m_allClrAPIcycles = max(m_maximum.m_allClrAPIcycles, info.m_allClrAPIcycles);
#endif

        if (includeInFiltered)
        {
            m_numFilteredMethods++;
            m_filtered.m_byteCodeBytes += info.m_byteCodeBytes;
            m_filtered.m_totalCycles += info.m_totalCycles;
            m_filtered.m_parentPhaseEndSlop += info.m_parentPhaseEndSlop;
        }

        for (int i = 0; i < PHASE_NUMBER_OF; i++)
        {
            m_total.m_invokesByPhase[i] += info.m_invokesByPhase[i];
            m_total.m_cyclesByPhase[i] += info.m_cyclesByPhase[i];

#if MEASURE_CLRAPI_CALLS
            m_total.m_CLRinvokesByPhase[i] += info.m_CLRinvokesByPhase[i];
            m_total.m_CLRcyclesByPhase[i] += info.m_CLRcyclesByPhase[i];
#endif

            if (includeInFiltered)
            {
                m_filtered.m_invokesByPhase[i] += info.m_invokesByPhase[i];
                m_filtered.m_cyclesByPhase[i] += info.m_cyclesByPhase[i];
#if MEASURE_CLRAPI_CALLS
                m_filtered.m_CLRinvokesByPhase[i] += info.m_CLRinvokesByPhase[i];
                m_filtered.m_CLRcyclesByPhase[i] += info.m_CLRcyclesByPhase[i];
#endif
            }
            m_maximum.m_cyclesByPhase[i] = max(m_maximum.m_cyclesByPhase[i], info.m_cyclesByPhase[i]);

#if MEASURE_CLRAPI_CALLS
            m_maximum.m_CLRcyclesByPhase[i] = max(m_maximum.m_CLRcyclesByPhase[i], info.m_CLRcyclesByPhase[i]);
#endif
        }
        m_total.m_parentPhaseEndSlop += info.m_parentPhaseEndSlop;
        m_maximum.m_parentPhaseEndSlop = max(m_maximum.m_parentPhaseEndSlop, info.m_parentPhaseEndSlop);
    }
#if MEASURE_CLRAPI_CALLS
    else
    {
        m_totMethods++;

        // Update the "global" CLR-API values.
        m_total.m_allClrAPIcalls += info.m_allClrAPIcalls;
        m_maximum.m_allClrAPIcalls = max(m_maximum.m_allClrAPIcalls, info.m_allClrAPIcalls);
        m_total.m_allClrAPIcycles += info.m_allClrAPIcycles;
        m_maximum.m_allClrAPIcycles = max(m_maximum.m_allClrAPIcycles, info.m_allClrAPIcycles);

        // Update the per-phase CLR-API values.
        m_total.m_invokesByPhase[PHASE_CLR_API] += info.m_allClrAPIcalls;
        m_maximum.m_invokesByPhase[PHASE_CLR_API] =
            max(m_maximum.m_perClrAPIcalls[PHASE_CLR_API], info.m_allClrAPIcalls);
        m_total.m_cyclesByPhase[PHASE_CLR_API] += info.m_allClrAPIcycles;
        m_maximum.m_cyclesByPhase[PHASE_CLR_API] =
            max(m_maximum.m_cyclesByPhase[PHASE_CLR_API], info.m_allClrAPIcycles);
    }

    for (int i = 0; i < API_ICorJitInfo_Names::API_COUNT; i++)
    {
        m_total.m_perClrAPIcalls[i] += info.m_perClrAPIcalls[i];
        m_maximum.m_perClrAPIcalls[i] = max(m_maximum.m_perClrAPIcalls[i], info.m_perClrAPIcalls[i]);

        m_total.m_perClrAPIcycles[i] += info.m_perClrAPIcycles[i];
        m_maximum.m_perClrAPIcycles[i] = max(m_maximum.m_perClrAPIcycles[i], info.m_perClrAPIcycles[i]);

        m_maximum.m_maxClrAPIcycles[i] = max(m_maximum.m_maxClrAPIcycles[i], info.m_maxClrAPIcycles[i]);
    }
#endif
}

// Static
LPCWSTR Compiler::compJitTimeLogFilename = nullptr;

void CompTimeSummaryInfo::Print(FILE* f)
{
    if (f == nullptr)
    {
        return;
    }
    // Otherwise...
    double countsPerSec = CachedCyclesPerSecond();
    if (countsPerSec == 0.0)
    {
        fprintf(f, "Processor does not have a high-frequency timer.\n");
        return;
    }

    double totTime_ms = 0.0;

    fprintf(f, "JIT Compilation time report:\n");
    fprintf(f, "  Compiled %d methods.\n", m_numMethods);
    if (m_numMethods != 0)
    {
        fprintf(f, "  Compiled %d bytecodes total (%d max, %8.2f avg).\n", m_total.m_byteCodeBytes,
                m_maximum.m_byteCodeBytes, (double)m_total.m_byteCodeBytes / (double)m_numMethods);
        totTime_ms = ((double)m_total.m_totalCycles / countsPerSec) * 1000.0;
        fprintf(f, "  Time: total: %10.3f Mcycles/%10.3f ms\n", ((double)m_total.m_totalCycles / 1000000.0),
                totTime_ms);
        fprintf(f, "          max: %10.3f Mcycles/%10.3f ms\n", ((double)m_maximum.m_totalCycles) / 1000000.0,
                ((double)m_maximum.m_totalCycles / countsPerSec) * 1000.0);
        fprintf(f, "          avg: %10.3f Mcycles/%10.3f ms\n",
                ((double)m_total.m_totalCycles) / 1000000.0 / (double)m_numMethods, totTime_ms / (double)m_numMethods);

        const char* extraHdr1 = "";
        const char* extraHdr2 = "";
#if MEASURE_CLRAPI_CALLS
        bool extraInfo = (JitConfig.JitEECallTimingInfo() != 0);
        if (extraInfo)
        {
            extraHdr1 = "    CLRs/meth   % in CLR";
            extraHdr2 = "-----------------------";
        }
#endif

        fprintf(f, "\n  Total time by phases:\n");
        fprintf(f, "     PHASE                          inv/meth   Mcycles    time (ms)  %% of total    max (ms)%s\n",
                extraHdr1);
        fprintf(f, "     ---------------------------------------------------------------------------------------%s\n",
                extraHdr2);

        // Ensure that at least the names array and the Phases enum have the same number of entries:
        assert(_countof(PhaseNames) == PHASE_NUMBER_OF);
        for (int i = 0; i < PHASE_NUMBER_OF; i++)
        {
            double phase_tot_ms = (((double)m_total.m_cyclesByPhase[i]) / countsPerSec) * 1000.0;
            double phase_max_ms = (((double)m_maximum.m_cyclesByPhase[i]) / countsPerSec) * 1000.0;

#if MEASURE_CLRAPI_CALLS
            // Skip showing CLR API call info if we didn't collect any
            if (i == PHASE_CLR_API && !extraInfo)
                continue;
#endif

            // Indent nested phases, according to depth.
            int ancPhase = PhaseParent[i];
            while (ancPhase != -1)
            {
                fprintf(f, "  ");
                ancPhase = PhaseParent[ancPhase];
            }
            fprintf(f, "     %-30s %6.2f  %10.2f   %9.3f   %8.2f%%    %8.3f", PhaseNames[i],
                    ((double)m_total.m_invokesByPhase[i]) / ((double)m_numMethods),
                    ((double)m_total.m_cyclesByPhase[i]) / 1000000.0, phase_tot_ms, (phase_tot_ms * 100.0 / totTime_ms),
                    phase_max_ms);

#if MEASURE_CLRAPI_CALLS
            if (extraInfo && i != PHASE_CLR_API)
            {
                double nest_tot_ms  = (((double)m_total.m_CLRcyclesByPhase[i]) / countsPerSec) * 1000.0;
                double nest_percent = nest_tot_ms * 100.0 / totTime_ms;
                double calls_per_fn = ((double)m_total.m_CLRinvokesByPhase[i]) / ((double)m_numMethods);

                if (nest_percent > 0.1 || calls_per_fn > 10)
                    fprintf(f, "       %5.1f   %8.2f%%", calls_per_fn, nest_percent);
            }
#endif
            fprintf(f, "\n");
        }

        // Show slop if it's over a certain percentage of the total
        double pslop_pct = 100.0 * m_total.m_parentPhaseEndSlop * 1000.0 / countsPerSec / totTime_ms;
        if (pslop_pct >= 1.0)
        {
            fprintf(f, "\n  'End phase slop' should be very small (if not, there's unattributed time): %9.3f Mcycles = "
                       "%3.1f%% of total.\n\n",
                    m_total.m_parentPhaseEndSlop / 1000000.0, pslop_pct);
        }
    }
    if (m_numFilteredMethods > 0)
    {
        fprintf(f, "  Compiled %d methods that meet the filter requirement.\n", m_numFilteredMethods);
        fprintf(f, "  Compiled %d bytecodes total (%8.2f avg).\n", m_filtered.m_byteCodeBytes,
                (double)m_filtered.m_byteCodeBytes / (double)m_numFilteredMethods);
        double totTime_ms = ((double)m_filtered.m_totalCycles / countsPerSec) * 1000.0;
        fprintf(f, "  Time: total: %10.3f Mcycles/%10.3f ms\n", ((double)m_filtered.m_totalCycles / 1000000.0),
                totTime_ms);
        fprintf(f, "          avg: %10.3f Mcycles/%10.3f ms\n",
                ((double)m_filtered.m_totalCycles) / 1000000.0 / (double)m_numFilteredMethods,
                totTime_ms / (double)m_numFilteredMethods);

        fprintf(f, "  Total time by phases:\n");
        fprintf(f, "     PHASE                            inv/meth Mcycles    time (ms)  %% of total\n");
        fprintf(f, "     --------------------------------------------------------------------------------------\n");
        // Ensure that at least the names array and the Phases enum have the same number of entries:
        assert(_countof(PhaseNames) == PHASE_NUMBER_OF);
        for (int i = 0; i < PHASE_NUMBER_OF; i++)
        {
            double phase_tot_ms = (((double)m_filtered.m_cyclesByPhase[i]) / countsPerSec) * 1000.0;
            // Indent nested phases, according to depth.
            int ancPhase = PhaseParent[i];
            while (ancPhase != -1)
            {
                fprintf(f, "  ");
                ancPhase = PhaseParent[ancPhase];
            }
            fprintf(f, "     %-30s  %5.2f  %10.2f   %9.3f   %8.2f%%\n", PhaseNames[i],
                    ((double)m_filtered.m_invokesByPhase[i]) / ((double)m_numFilteredMethods),
                    ((double)m_filtered.m_cyclesByPhase[i]) / 1000000.0, phase_tot_ms,
                    (phase_tot_ms * 100.0 / totTime_ms));
        }

        double fslop_ms = m_filtered.m_parentPhaseEndSlop * 1000.0 / countsPerSec;
        if (fslop_ms > 1.0)
        {
            fprintf(f, "\n  'End phase slop' should be very small (if not, there's unattributed time): %9.3f Mcycles = "
                       "%3.1f%% of total.\n\n",
                    m_filtered.m_parentPhaseEndSlop / 1000000.0, fslop_ms);
        }
    }

#if MEASURE_CLRAPI_CALLS
    if (m_total.m_allClrAPIcalls > 0 && m_total.m_allClrAPIcycles > 0)
    {
        fprintf(f, "\n");
        if (m_totMethods > 0)
            fprintf(f, "  Imported %u methods.\n\n", m_numMethods + m_totMethods);

        fprintf(f, "     CLR API                                   # calls   total time    max time     avg time   %% "
                   "of total\n");
        fprintf(f, "     -------------------------------------------------------------------------------");
        fprintf(f, "---------------------\n");

        static const char* APInames[] = {
#define DEF_CLR_API(name) #name,
#include "ICorJitInfo_API_names.h"
        };

        unsigned shownCalls  = 0;
        double   shownMillis = 0.0;
#ifdef DEBUG
        unsigned checkedCalls  = 0;
        double   checkedMillis = 0.0;
#endif

        for (unsigned pass = 0; pass < 2; pass++)
        {
            for (unsigned i = 0; i < API_ICorJitInfo_Names::API_COUNT; i++)
            {
                unsigned calls = m_total.m_perClrAPIcalls[i];
                if (calls == 0)
                    continue;

                unsigned __int64 cycles = m_total.m_perClrAPIcycles[i];
                double           millis = 1000.0 * cycles / countsPerSec;

                // Don't show the small fry to keep the results manageable
                if (millis < 0.5)
                {
                    // We always show the following API because it is always called
                    // exactly once for each method and its body is the simplest one
                    // possible (it just returns an integer constant), and therefore
                    // it can be used to measure the overhead of adding the CLR API
                    // timing code. Roughly speaking, on a 3GHz x64 box the overhead
                    // per call should be around 40 ns when using RDTSC, compared to
                    // about 140 ns when using GetThreadCycles() under Windows.
                    if (i != API_ICorJitInfo_Names::API_getExpectedTargetArchitecture)
                        continue;
                }

                // In the first pass we just compute the totals.
                if (pass == 0)
                {
                    shownCalls += m_total.m_perClrAPIcalls[i];
                    shownMillis += millis;
                    continue;
                }

                unsigned __int32 maxcyc = m_maximum.m_maxClrAPIcycles[i];
                double           max_ms = 1000.0 * maxcyc / countsPerSec;

                fprintf(f, "     %-40s", APInames[i]);                                 // API name
                fprintf(f, " %8u %9.1f ms", calls, millis);                            // #calls, total time
                fprintf(f, " %8.1f ms  %8.1f ns", max_ms, 1000000.0 * millis / calls); // max, avg time
                fprintf(f, "     %5.1f%%\n", 100.0 * millis / shownMillis);            // % of total

#ifdef DEBUG
                checkedCalls += m_total.m_perClrAPIcalls[i];
                checkedMillis += millis;
#endif
            }
        }

#ifdef DEBUG
        assert(checkedCalls == shownCalls);
        assert(checkedMillis == shownMillis);
#endif

        if (shownCalls > 0 || shownMillis > 0)
        {
            fprintf(f, "     -------------------------");
            fprintf(f, "---------------------------------------------------------------------------\n");
            fprintf(f, "     Total for calls shown above              %8u %10.1f ms", shownCalls, shownMillis);
            if (totTime_ms > 0.0)
                fprintf(f, " (%4.1lf%% of overall JIT time)", shownMillis * 100.0 / totTime_ms);
            fprintf(f, "\n");
        }
        fprintf(f, "\n");
    }
#endif

    fprintf(f, "\n");
}

JitTimer::JitTimer(unsigned byteCodeSize) : m_info(byteCodeSize)
{
#if MEASURE_CLRAPI_CALLS
    m_CLRcallInvokes = 0;
    m_CLRcallCycles  = 0;
#endif

#ifdef DEBUG
    m_lastPhase = (Phases)-1;
#if MEASURE_CLRAPI_CALLS
    m_CLRcallAPInum = -1;
#endif
#endif

    unsigned __int64 threadCurCycles;
    if (_our_GetThreadCycles(&threadCurCycles))
    {
        m_start         = threadCurCycles;
        m_curPhaseStart = threadCurCycles;
    }
}

void JitTimer::EndPhase(Compiler* compiler, Phases phase)
{
    // Otherwise...
    // We re-run some phases currently, so this following assert doesn't work.
    // assert((int)phase > (int)m_lastPhase);  // We should end phases in increasing order.

    unsigned __int64 threadCurCycles;
    if (_our_GetThreadCycles(&threadCurCycles))
    {
        unsigned __int64 phaseCycles = (threadCurCycles - m_curPhaseStart);

        // If this is not a leaf phase, the assumption is that the last subphase must have just recently ended.
        // Credit the duration to "slop", the total of which should be very small.
        if (PhaseHasChildren[phase])
        {
            m_info.m_parentPhaseEndSlop += phaseCycles;
        }
        else
        {
            // It is a leaf phase.  Credit duration to it.
            m_info.m_invokesByPhase[phase]++;
            m_info.m_cyclesByPhase[phase] += phaseCycles;

#if MEASURE_CLRAPI_CALLS
            // Record the CLR API timing info as well.
            m_info.m_CLRinvokesByPhase[phase] += m_CLRcallInvokes;
            m_info.m_CLRcyclesByPhase[phase] += m_CLRcallCycles;
#endif

            // Credit the phase's ancestors, if any.
            int ancPhase = PhaseParent[phase];
            while (ancPhase != -1)
            {
                m_info.m_cyclesByPhase[ancPhase] += phaseCycles;
                ancPhase = PhaseParent[ancPhase];
            }

#if MEASURE_CLRAPI_CALLS
            const Phases lastPhase = PHASE_CLR_API;
#else
            const Phases lastPhase = PHASE_NUMBER_OF;
#endif
            if (phase + 1 == lastPhase)
            {
                m_info.m_totalCycles = (threadCurCycles - m_start);
            }
            else
            {
                m_curPhaseStart = threadCurCycles;
            }
        }

        if ((JitConfig.JitMeasureIR() != 0) && PhaseReportsIRSize[phase])
        {
            m_info.m_nodeCountAfterPhase[phase] = compiler->fgMeasureIR();
        }
        else
        {
            m_info.m_nodeCountAfterPhase[phase] = 0;
        }
    }

#ifdef DEBUG
    m_lastPhase = phase;
#endif
#if MEASURE_CLRAPI_CALLS
    m_CLRcallInvokes = 0;
    m_CLRcallCycles  = 0;
#endif
}

#if MEASURE_CLRAPI_CALLS

//------------------------------------------------------------------------
// JitTimer::CLRApiCallEnter: Start the stopwatch for an EE call.
//
// Arguments:
//    apix - The API index - an "enum API_ICorJitInfo_Names" value.
//

void JitTimer::CLRApiCallEnter(unsigned apix)
{
    assert(m_CLRcallAPInum == -1); // Nested calls not allowed
    m_CLRcallAPInum = apix;

    // If we can't get the cycles, we'll just ignore this call
    if (!_our_GetThreadCycles(&m_CLRcallStart))
        m_CLRcallStart = 0;
}

//------------------------------------------------------------------------
// JitTimer::CLRApiCallLeave: compute / record time spent in an EE call.
//
// Arguments:
//    apix - The API's "enum API_ICorJitInfo_Names" value; this value
//           should match the value passed to the most recent call to
//           "CLRApiCallEnter" (i.e. these must come as matched pairs),
//           and they also may not nest.
//

void JitTimer::CLRApiCallLeave(unsigned apix)
{
    // Make sure we're actually inside a measured CLR call.
    assert(m_CLRcallAPInum != -1);
    m_CLRcallAPInum = -1;

    // Ignore this one if we don't have a valid starting counter.
    if (m_CLRcallStart != 0)
    {
        if (JitConfig.JitEECallTimingInfo() != 0)
        {
            unsigned __int64 threadCurCycles;
            if (_our_GetThreadCycles(&threadCurCycles))
            {
                // Compute the cycles spent in the call.
                threadCurCycles -= m_CLRcallStart;

                // Add the cycles to the 'phase' and bump its use count.
                m_info.m_cyclesByPhase[PHASE_CLR_API] += threadCurCycles;
                m_info.m_invokesByPhase[PHASE_CLR_API] += 1;

                // Add the values to the "per API" info.
                m_info.m_allClrAPIcycles += threadCurCycles;
                m_info.m_allClrAPIcalls += 1;

                m_info.m_perClrAPIcalls[apix] += 1;
                m_info.m_perClrAPIcycles[apix] += threadCurCycles;
                m_info.m_maxClrAPIcycles[apix] = max(m_info.m_maxClrAPIcycles[apix], (unsigned __int32)threadCurCycles);

                // Subtract the cycles from the enclosing phase by bumping its start time
                m_curPhaseStart += threadCurCycles;

                // Update the running totals.
                m_CLRcallInvokes += 1;
                m_CLRcallCycles += threadCurCycles;
            }
        }

        m_CLRcallStart = 0;
    }

    assert(m_CLRcallAPInum != -1); // No longer in this API call.
    m_CLRcallAPInum = -1;
}

#endif // MEASURE_CLRAPI_CALLS

CritSecObject JitTimer::s_csvLock;

// It's expensive to constantly open and close the file, so open it once and close it
// when the process exits. This should be accessed under the s_csvLock.
FILE* JitTimer::s_csvFile = nullptr;

LPCWSTR Compiler::JitTimeLogCsv()
{
    LPCWSTR jitTimeLogCsv = JitConfig.JitTimeLogCsv();
    return jitTimeLogCsv;
}

void JitTimer::PrintCsvHeader()
{
    LPCWSTR jitTimeLogCsv = Compiler::JitTimeLogCsv();
    if (jitTimeLogCsv == nullptr)
    {
        return;
    }

    CritSecHolder csvLock(s_csvLock);

    if (s_csvFile == nullptr)
    {
        s_csvFile = _wfopen(jitTimeLogCsv, W("a"));
    }
    if (s_csvFile != nullptr)
    {
        // Seek to the end of the file s.t. `ftell` doesn't lie to us on Windows
        fseek(s_csvFile, 0, SEEK_END);

        // Write the header if the file is empty
        if (ftell(s_csvFile) == 0)
        {
            fprintf(s_csvFile, "\"Method Name\",");
            fprintf(s_csvFile, "\"Assembly or SPMI Index\",");
            fprintf(s_csvFile, "\"IL Bytes\",");
            fprintf(s_csvFile, "\"Basic Blocks\",");
            fprintf(s_csvFile, "\"Min Opts\",");
            fprintf(s_csvFile, "\"Loops\",");
            fprintf(s_csvFile, "\"Loops Cloned\",");
#if FEATURE_LOOP_ALIGN
#ifdef DEBUG
            fprintf(s_csvFile, "\"Alignment Candidates\",");
            fprintf(s_csvFile, "\"Loops Aligned\",");
#endif // DEBUG
#endif // FEATURE_LOOP_ALIGN
            for (int i = 0; i < PHASE_NUMBER_OF; i++)
            {
                fprintf(s_csvFile, "\"%s\",", PhaseNames[i]);
                if ((JitConfig.JitMeasureIR() != 0) && PhaseReportsIRSize[i])
                {
                    fprintf(s_csvFile, "\"Node Count After %s\",", PhaseNames[i]);
                }
            }

            InlineStrategy::DumpCsvHeader(s_csvFile);

            fprintf(s_csvFile, "\"Executable Code Bytes\",");
#ifdef JIT32_GCENCODER
            fprintf(s_csvFile, "\"GC Info Bytes\",");
#endif
            fprintf(s_csvFile, "\"Total Bytes Allocated\",");
            fprintf(s_csvFile, "\"Total Cycles\",");
            fprintf(s_csvFile, "\"CPS\"\n");

            fflush(s_csvFile);
        }
    }
}

void JitTimer::PrintCsvMethodStats(Compiler* comp)
{
    LPCWSTR jitTimeLogCsv = Compiler::JitTimeLogCsv();
    if (jitTimeLogCsv == nullptr)
    {
        return;
    }

// eeGetMethodFullName uses locks, so don't enter crit sec before this call.
#if defined(DEBUG) || defined(LATE_DISASM)
    // If we already have computed the name because for some reason we're generating the CSV
    // for a DEBUG build (presumably not for the time info), just re-use it.
    const char* methName = comp->info.compFullName;
#else
    const char*          methName  = comp->eeGetMethodFullName(comp->info.compMethodHnd);
#endif

    // Try and access the SPMI index to report in the data set.
    //
    // If the jit is not hosted under SPMI this will return the
    // default value of zero.
    //
    // Query the jit host directly here instead of going via the
    // config cache, since value will change for each method.
    int index = g_jitHost->getIntConfigValue(W("SuperPMIMethodContextNumber"), -1);

    CritSecHolder csvLock(s_csvLock);

    if (s_csvFile == nullptr)
    {
        return;
    }

    fprintf(s_csvFile, "\"%s\",", methName);
    if (index != 0)
    {
        fprintf(s_csvFile, "%d,", index);
    }
    else
    {
        const char* methodAssemblyName = comp->info.compCompHnd->getAssemblyName(
            comp->info.compCompHnd->getModuleAssembly(comp->info.compCompHnd->getClassModule(comp->info.compClassHnd)));
        fprintf(s_csvFile, "\"%s\",", methodAssemblyName);
    }
    fprintf(s_csvFile, "%u,", comp->info.compILCodeSize);
    fprintf(s_csvFile, "%u,", comp->fgBBcount);
    fprintf(s_csvFile, "%u,", comp->opts.MinOpts());
    fprintf(s_csvFile, "%u,", comp->optLoopCount);
    fprintf(s_csvFile, "%u,", comp->optLoopsCloned);
#if FEATURE_LOOP_ALIGN
#ifdef DEBUG
    fprintf(s_csvFile, "%u,", comp->loopAlignCandidates);
    fprintf(s_csvFile, "%u,", comp->loopsAligned);
#endif // DEBUG
#endif // FEATURE_LOOP_ALIGN
    unsigned __int64 totCycles = 0;
    for (int i = 0; i < PHASE_NUMBER_OF; i++)
    {
        if (!PhaseHasChildren[i])
        {
            totCycles += m_info.m_cyclesByPhase[i];
        }
        fprintf(s_csvFile, "%I64u,", m_info.m_cyclesByPhase[i]);

        if ((JitConfig.JitMeasureIR() != 0) && PhaseReportsIRSize[i])
        {
            fprintf(s_csvFile, "%u,", m_info.m_nodeCountAfterPhase[i]);
        }
    }

    comp->m_inlineStrategy->DumpCsvData(s_csvFile);

    fprintf(s_csvFile, "%u,", comp->info.compNativeCodeSize);

#ifdef JIT32_GCENCODER
    if (comp->codeGen != nullptr)
    {
        fprintf(s_csvFile, "%Iu,", comp->codeGen->compInfoBlkSize);
    }
#endif

    fprintf(s_csvFile, "%Iu,", comp->compGetArenaAllocator()->getTotalBytesAllocated());
    fprintf(s_csvFile, "%I64u,", m_info.m_totalCycles);
    fprintf(s_csvFile, "%f\n", CachedCyclesPerSecond());

    fflush(s_csvFile);
}

// Perform process shutdown actions.
//
// static
void JitTimer::Shutdown()
{
    CritSecHolder csvLock(s_csvLock);
    if (s_csvFile != nullptr)
    {
        fclose(s_csvFile);
    }
}

// Completes the timing of the current method, and adds it to "sum".
void JitTimer::Terminate(Compiler* comp, CompTimeSummaryInfo& sum, bool includePhases)
{
    if (includePhases)
    {
        PrintCsvMethodStats(comp);
    }

    sum.AddInfo(m_info, includePhases);
}
#endif // FEATURE_JIT_METHOD_PERF

#if LOOP_HOIST_STATS
// Static fields.
CritSecObject Compiler::s_loopHoistStatsLock; // Default constructor.
unsigned      Compiler::s_loopsConsidered             = 0;
unsigned      Compiler::s_loopsWithHoistedExpressions = 0;
unsigned      Compiler::s_totalHoistedExpressions     = 0;

// static
void Compiler::PrintAggregateLoopHoistStats(FILE* f)
{
    fprintf(f, "\n");
    fprintf(f, "---------------------------------------------------\n");
    fprintf(f, "Loop hoisting stats\n");
    fprintf(f, "---------------------------------------------------\n");

    double pctWithHoisted = 0.0;
    if (s_loopsConsidered > 0)
    {
        pctWithHoisted = 100.0 * (double(s_loopsWithHoistedExpressions) / double(s_loopsConsidered));
    }
    double exprsPerLoopWithExpr = 0.0;
    if (s_loopsWithHoistedExpressions > 0)
    {
        exprsPerLoopWithExpr = double(s_totalHoistedExpressions) / double(s_loopsWithHoistedExpressions);
    }
    fprintf(f, "Considered %d loops.  Of these, we hoisted expressions out of %d (%6.2f%%).\n", s_loopsConsidered,
            s_loopsWithHoistedExpressions, pctWithHoisted);
    fprintf(f, "  A total of %d expressions were hoisted, an average of %5.2f per loop-with-hoisted-expr.\n",
            s_totalHoistedExpressions, exprsPerLoopWithExpr);
}

void Compiler::AddLoopHoistStats()
{
    CritSecHolder statsLock(s_loopHoistStatsLock);

    s_loopsConsidered += m_loopsConsidered;
    s_loopsWithHoistedExpressions += m_loopsWithHoistedExpressions;
    s_totalHoistedExpressions += m_totalHoistedExpressions;
}

void Compiler::PrintPerMethodLoopHoistStats()
{
    double pctWithHoisted = 0.0;
    if (m_loopsConsidered > 0)
    {
        pctWithHoisted = 100.0 * (double(m_loopsWithHoistedExpressions) / double(m_loopsConsidered));
    }
    double exprsPerLoopWithExpr = 0.0;
    if (m_loopsWithHoistedExpressions > 0)
    {
        exprsPerLoopWithExpr = double(m_totalHoistedExpressions) / double(m_loopsWithHoistedExpressions);
    }
    printf("Considered %d loops.  Of these, we hoisted expressions out of %d (%5.2f%%).\n", m_loopsConsidered,
           m_loopsWithHoistedExpressions, pctWithHoisted);
    printf("  A total of %d expressions were hoisted, an average of %5.2f per loop-with-hoisted-expr.\n",
           m_totalHoistedExpressions, exprsPerLoopWithExpr);
}
#endif // LOOP_HOIST_STATS

//------------------------------------------------------------------------
// RecordStateAtEndOfInlining: capture timing data (if enabled) after
// inlining as completed.
//
// Note:
// Records data needed for SQM and inlining data dumps.  Should be
// called after inlining is complete.  (We do this after inlining
// because this marks the last point at which the JIT is likely to
// cause type-loading and class initialization).

void Compiler::RecordStateAtEndOfInlining()
{
#if defined(DEBUG) || defined(INLINE_DATA)

    m_compCyclesAtEndOfInlining    = 0;
    m_compTickCountAtEndOfInlining = 0;
    bool b                         = CycleTimer::GetThreadCyclesS(&m_compCyclesAtEndOfInlining);
    if (!b)
    {
        return; // We don't have a thread cycle counter.
    }
    m_compTickCountAtEndOfInlining = GetTickCount();

#endif // defined(DEBUG) || defined(INLINE_DATA)
}

//------------------------------------------------------------------------
// RecordStateAtEndOfCompilation: capture timing data (if enabled) after
// compilation is completed.

void Compiler::RecordStateAtEndOfCompilation()
{
#if defined(DEBUG) || defined(INLINE_DATA)

    // Common portion
    m_compCycles = 0;
    unsigned __int64 compCyclesAtEnd;
    bool             b = CycleTimer::GetThreadCyclesS(&compCyclesAtEnd);
    if (!b)
    {
        return; // We don't have a thread cycle counter.
    }
    assert(compCyclesAtEnd >= m_compCyclesAtEndOfInlining);

    m_compCycles = compCyclesAtEnd - m_compCyclesAtEndOfInlining;

#endif // defined(DEBUG) || defined(INLINE_DATA)
}

#if FUNC_INFO_LOGGING
// static
LPCWSTR Compiler::compJitFuncInfoFilename = nullptr;

// static
FILE* Compiler::compJitFuncInfoFile = nullptr;
#endif // FUNC_INFO_LOGGING

#ifdef DEBUG

// dumpConvertedVarSet() dumps the varset bits that are tracked
// variable indices, and we convert them to variable numbers, sort the variable numbers, and
// print them as variable numbers. To do this, we use a temporary set indexed by
// variable number. We can't use the "all varset" type because it is still size-limited, and might
// not be big enough to handle all possible variable numbers.
void dumpConvertedVarSet(Compiler* comp, VARSET_VALARG_TP vars)
{
    BYTE* pVarNumSet; // trivial set: one byte per varNum, 0 means not in set, 1 means in set.

    size_t varNumSetBytes = comp->lvaCount * sizeof(BYTE);
    pVarNumSet            = (BYTE*)_alloca(varNumSetBytes);
    memset(pVarNumSet, 0, varNumSetBytes); // empty the set

    if (!VarSetOps::MayBeUninit(vars))
    {
        VarSetOps::Iter iter(comp, vars);
        for (unsigned varIndex = 0; iter.NextElem(&varIndex);)
        {
            pVarNumSet[comp->lvaTrackedIndexToLclNum(varIndex)] = 1;
        }
    }

    bool first = true;
    printf("{");
    for (size_t varNum = 0; varNum < comp->lvaCount; varNum++)
    {
        if (pVarNumSet[varNum] == 1)
        {
            if (!first)
            {
                printf(" ");
            }
            printf("V%02u", varNum);
            first = false;
        }
    }
    printf("}");
}

void Compiler::dmpVarSetDiff(const char* name, VARSET_VALARG_TP from, VARSET_VALARG_TP to)
{
    bool* fromBits = static_cast<bool*>(_alloca(lvaCount * sizeof(bool)));
    memset(fromBits, 0, lvaCount * sizeof(bool));
    bool* toBits = static_cast<bool*>(_alloca(lvaCount * sizeof(bool)));
    memset(toBits, 0, lvaCount * sizeof(bool));

    for (VarSetOps::Enumerator e(this, from); e.MoveNext();)
    {
        fromBits[lvaTrackedIndexToLclNum(e.Current())] = true;
    }

    for (VarSetOps::Enumerator e(this, to); e.MoveNext();)
    {
        toBits[lvaTrackedIndexToLclNum(e.Current())] = true;
    }

    printf("%s{ ", name);

    for (unsigned i = 0; i < lvaCount; i++)
    {
        if (!fromBits[i] && !toBits[i])
        {
            continue;
        }

        const char* s = "";

        if (fromBits[i] != toBits[i])
        {
            s = toBits[i] ? "+" : "-";
        }

        printf("%sV%02u ", s, i);
    }

    printf("}\n");
}

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                          Debugging helpers                                XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

/*****************************************************************************/
/* The following functions are intended to be called from the debugger, to dump
 * various data structures.
 *
 * The versions that start with 'c' take a Compiler* as the first argument.
 * The versions that start with 'd' use the tlsCompiler, so don't require a Compiler*.
 *
 * Summary:
 *      cBlock,      dBlock         : Display a basic block (call fgTableDispBasicBlock()).
 *      cBlocks,     dBlocks        : Display all the basic blocks of a function (call fgDispBasicBlocks()).
 *      cBlocksV,    dBlocksV       : Display all the basic blocks of a function (call fgDispBasicBlocks(true)).
 *                                    "V" means "verbose", and will dump all the trees.
 *      cStmt,       dStmt          : Display a Statement (call gtDispStmt()).
 *      cTree,       dTree          : Display a tree (call gtDispTree()).
 *      cTreeLIR,    dTreeLIR       : Display a tree in LIR form (call gtDispLIRNode()).
 *      cTrees,      dTrees         : Display all the trees in a function (call fgDumpTrees()).
 *      cEH,         dEH            : Display the EH handler table (call fgDispHandlerTab()).
 *      cVar,        dVar           : Display a local variable given its number (call lvaDumpEntry()).
 *      cVarDsc,     dVarDsc        : Display a local variable given a LclVarDsc* (call lvaDumpEntry()).
 *      cVars,       dVars          : Display the local variable table (call lvaTableDump()).
 *      cBlockCheapPreds, dBlockCheapPreds : Display a block's cheap predecessors (call block->dspCheapPreds()).
 *      cBlockPreds, dBlockPreds    : Display a block's predecessors (call block->dspPreds()).
 *      cBlockSuccs, dBlockSuccs    : Display a block's successors (call block->dspSuccs(compiler)).
 *      cReach,      dReach         : Display all block reachability (call fgDispReach()).
 *      cDoms,       dDoms          : Display all block dominators (call fgDispDoms()).
 *      cLiveness,   dLiveness      : Display per-block variable liveness (call fgDispBBLiveness()).
 *      cCVarSet,    dCVarSet       : Display a "converted" VARSET_TP: the varset is assumed to be tracked variable
 *                                    indices. These are converted to variable numbers and sorted. (Calls
 *                                    dumpConvertedVarSet()).
 *      cLoop,       dLoop          : Display the blocks of a loop, including the trees.
 *      cTreeFlags,  dTreeFlags     : Display tree flags
 *
 * The following don't require a Compiler* to work:
 *      dRegMask                    : Display a regMaskTP (call dspRegMask(mask)).
 *      dBlockList                  : Display a BasicBlockList*.
 */

void cBlock(Compiler* comp, BasicBlock* block)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *Block %u\n", sequenceNumber++);
    comp->fgTableDispBasicBlock(block);
}

void cBlocks(Compiler* comp)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *Blocks %u\n", sequenceNumber++);
    comp->fgDispBasicBlocks();
}

void cBlocksV(Compiler* comp)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *BlocksV %u\n", sequenceNumber++);
    comp->fgDispBasicBlocks(true);
}

void cStmt(Compiler* comp, Statement* statement)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *Stmt %u\n", sequenceNumber++);
    comp->gtDispStmt(statement, ">>>");
}

void cTree(Compiler* comp, GenTree* tree)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *Tree %u\n", sequenceNumber++);
    comp->gtDispTree(tree, nullptr, ">>>");
}

void cTreeLIR(Compiler* comp, GenTree* tree)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *TreeLIR %u\n", sequenceNumber++);
    comp->gtDispLIRNode(tree);
}

void cTrees(Compiler* comp)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *Trees %u\n", sequenceNumber++);
    comp->fgDumpTrees(comp->fgFirstBB, nullptr);
}

void cEH(Compiler* comp)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *EH %u\n", sequenceNumber++);
    comp->fgDispHandlerTab();
}

void cVar(Compiler* comp, unsigned lclNum)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *Var %u\n", sequenceNumber++);
    comp->lvaDumpEntry(lclNum, Compiler::FINAL_FRAME_LAYOUT);
}

void cVarDsc(Compiler* comp, LclVarDsc* varDsc)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *VarDsc %u\n", sequenceNumber++);
    unsigned lclNum = (unsigned)(varDsc - comp->lvaTable);
    comp->lvaDumpEntry(lclNum, Compiler::FINAL_FRAME_LAYOUT);
}

void cVars(Compiler* comp)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *Vars %u\n", sequenceNumber++);
    comp->lvaTableDump();
}

void cBlockCheapPreds(Compiler* comp, BasicBlock* block)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *BlockCheapPreds %u\n",
           sequenceNumber++);
    block->dspCheapPreds();
}

void cBlockPreds(Compiler* comp, BasicBlock* block)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *BlockPreds %u\n", sequenceNumber++);
    block->dspPreds();
}

void cBlockSuccs(Compiler* comp, BasicBlock* block)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *BlockSuccs %u\n", sequenceNumber++);
    block->dspSuccs(comp);
}

void cReach(Compiler* comp)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *Reach %u\n", sequenceNumber++);
    comp->fgDispReach();
}

void cDoms(Compiler* comp)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *Doms %u\n", sequenceNumber++);
    comp->fgDispDoms();
}

void cLiveness(Compiler* comp)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *Liveness %u\n", sequenceNumber++);
    comp->fgDispBBLiveness();
}

void cCVarSet(Compiler* comp, VARSET_VALARG_TP vars)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== dCVarSet %u\n", sequenceNumber++);
    dumpConvertedVarSet(comp, vars);
    printf("\n"); // dumpConvertedVarSet() doesn't emit a trailing newline
}

void cLoop(Compiler* comp, Compiler::LoopDsc* loop)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== Loop %u\n", sequenceNumber++);
    printf("HEAD   " FMT_BB "\n", loop->lpHead->bbNum);
    printf("FIRST  " FMT_BB "\n", loop->lpFirst->bbNum);
    printf("TOP    " FMT_BB "\n", loop->lpTop->bbNum);
    printf("ENTRY  " FMT_BB "\n", loop->lpEntry->bbNum);
    if (loop->lpExitCnt == 1)
    {
        printf("EXIT   " FMT_BB "\n", loop->lpExit->bbNum);
    }
    else
    {
        printf("EXITS  %u\n", loop->lpExitCnt);
    }
    printf("BOTTOM " FMT_BB "\n", loop->lpBottom->bbNum);

    comp->fgDispBasicBlocks(loop->lpHead, loop->lpBottom, true);
}

void dBlock(BasicBlock* block)
{
    cBlock(JitTls::GetCompiler(), block);
}

void dBlocks()
{
    cBlocks(JitTls::GetCompiler());
}

void dBlocksV()
{
    cBlocksV(JitTls::GetCompiler());
}

void dStmt(Statement* statement)
{
    cStmt(JitTls::GetCompiler(), statement);
}

void dTree(GenTree* tree)
{
    cTree(JitTls::GetCompiler(), tree);
}

void dTreeLIR(GenTree* tree)
{
    cTreeLIR(JitTls::GetCompiler(), tree);
}

void dTrees()
{
    cTrees(JitTls::GetCompiler());
}

void dEH()
{
    cEH(JitTls::GetCompiler());
}

void dVar(unsigned lclNum)
{
    cVar(JitTls::GetCompiler(), lclNum);
}

void dVarDsc(LclVarDsc* varDsc)
{
    cVarDsc(JitTls::GetCompiler(), varDsc);
}

void dVars()
{
    cVars(JitTls::GetCompiler());
}

void dBlockPreds(BasicBlock* block)
{
    cBlockPreds(JitTls::GetCompiler(), block);
}

void dBlockCheapPreds(BasicBlock* block)
{
    cBlockCheapPreds(JitTls::GetCompiler(), block);
}

void dBlockSuccs(BasicBlock* block)
{
    cBlockSuccs(JitTls::GetCompiler(), block);
}

void dReach()
{
    cReach(JitTls::GetCompiler());
}

void dDoms()
{
    cDoms(JitTls::GetCompiler());
}

void dLiveness()
{
    cLiveness(JitTls::GetCompiler());
}

void dCVarSet(VARSET_VALARG_TP vars)
{
    cCVarSet(JitTls::GetCompiler(), vars);
}

void dLoop(Compiler::LoopDsc* loop)
{
    cLoop(JitTls::GetCompiler(), loop);
}

void dRegMask(regMaskTP mask)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== dRegMask %u\n", sequenceNumber++);
    dspRegMask(mask);
    printf("\n"); // dspRegMask() doesn't emit a trailing newline
}

void dBlockList(BasicBlockList* list)
{
    printf("WorkList: ");
    while (list != nullptr)
    {
        printf(FMT_BB " ", list->block->bbNum);
        list = list->next;
    }
    printf("\n");
}

// Global variables available in debug mode.  That are set by debug APIs for finding
// Trees, Stmts, and/or Blocks using id or bbNum.
// That can be used in watch window or as a way to get address of fields for data break points.

GenTree*    dbTree;
Statement*  dbStmt;
BasicBlock* dbTreeBlock;
BasicBlock* dbBlock;

// Debug APIs for finding Trees, Stmts, and/or Blocks.
// As a side effect, they set the debug variables above.

GenTree* dFindTree(GenTree* tree, unsigned id)
{
    if (tree == nullptr)
    {
        return nullptr;
    }

    if (tree->gtTreeID == id)
    {
        dbTree = tree;
        return tree;
    }

    for (GenTree* child : tree->Operands())
    {
        child = dFindTree(child, id);
        if (child != nullptr)
        {
            return child;
        }
    }

    return nullptr;
}

GenTree* dFindTree(unsigned id)
{
    Compiler* comp = JitTls::GetCompiler();
    GenTree*  tree;

    dbTreeBlock = nullptr;
    dbTree      = nullptr;

    for (BasicBlock* const block : comp->Blocks())
    {
        for (Statement* const stmt : block->Statements())
        {
            tree = dFindTree(stmt->GetRootNode(), id);
            if (tree != nullptr)
            {
                dbTreeBlock = block;
                return tree;
            }
        }
    }

    return nullptr;
}

Statement* dFindStmt(unsigned id)
{
    Compiler* comp = JitTls::GetCompiler();

    dbStmt = nullptr;

    unsigned stmtId = 0;
    for (BasicBlock* const block : comp->Blocks())
    {
        for (Statement* const stmt : block->Statements())
        {
            stmtId++;
            if (stmtId == id)
            {
                dbStmt = stmt;
                return stmt;
            }
        }
    }

    return nullptr;
}

BasicBlock* dFindBlock(unsigned bbNum)
{
    Compiler*   comp  = JitTls::GetCompiler();
    BasicBlock* block = nullptr;

    dbBlock = nullptr;
    for (block = comp->fgFirstBB; block != nullptr; block = block->bbNext)
    {
        if (block->bbNum == bbNum)
        {
            dbBlock = block;
            break;
        }
    }

    return block;
}

Compiler::LoopDsc* dFindLoop(unsigned loopNum)
{
    Compiler* comp = JitTls::GetCompiler();

    if (loopNum >= comp->optLoopCount)
    {
        printf("loopNum %u out of range\n");
        return nullptr;
    }

    return &comp->optLoopTable[loopNum];
}

void cTreeFlags(Compiler* comp, GenTree* tree)
{
    int chars = 0;

    if (tree->gtFlags != 0)
    {
        chars += printf("flags=");

        // Node flags
        CLANG_FORMAT_COMMENT_ANCHOR;

#if defined(DEBUG)
        if (tree->gtDebugFlags & GTF_DEBUG_NODE_LARGE)
        {
            chars += printf("[NODE_LARGE]");
        }
        if (tree->gtDebugFlags & GTF_DEBUG_NODE_SMALL)
        {
            chars += printf("[NODE_SMALL]");
        }
        if (tree->gtDebugFlags & GTF_DEBUG_NODE_MORPHED)
        {
            chars += printf("[MORPHED]");
        }
#endif // defined(DEBUG)

        // Operator flags

        genTreeOps op = tree->OperGet();
        switch (op)
        {
            case GT_LCL_VAR:
            case GT_LCL_FLD:
            case GT_STORE_LCL_FLD:
            case GT_STORE_LCL_VAR:
                if (tree->gtFlags & GTF_VAR_DEF)
                {
                    chars += printf("[VAR_DEF]");
                }
                if (tree->gtFlags & GTF_VAR_USEASG)
                {
                    chars += printf("[VAR_USEASG]");
                }
                if (tree->gtFlags & GTF_VAR_ITERATOR)
                {
                    chars += printf("[VAR_ITERATOR]");
                }
                if (tree->gtFlags & GTF_VAR_DEATH)
                {
                    chars += printf("[VAR_DEATH]");
                }
                FALLTHROUGH;
            case GT_LCL_ADDR:
                if (tree->gtFlags & GTF_VAR_CLONED)
                {
                    chars += printf("[VAR_CLONED]");
                }
                break;

            case GT_NOP:
                break;

            case GT_NO_OP:
                break;

            case GT_INDEX_ADDR:
                if (tree->gtFlags & GTF_INX_RNGCHK)
                {
                    chars += printf("[INX_RNGCHK]");
                }
                break;

            case GT_IND:
            case GT_STOREIND:
            case GT_OBJ:
            case GT_STORE_OBJ:
            case GT_BLK:
            case GT_STORE_BLK:
                if (tree->AsIndir()->IsVolatile())
                {
                    chars += printf("[IND_VOLATILE]");
                }
                if (tree->AsIndir()->IsUnaligned())
                {
                    chars += printf("[IND_UNALIGNED]");
                }
                if (tree->gtFlags & GTF_IND_TGT_HEAP)
                {
                    chars += printf("[IND_TGT_HEAP]");
                }
                if (tree->gtFlags & GTF_IND_TGT_NOT_HEAP)
                {
                    chars += printf("[IND_TGT_NOT_HEAP]");
                }
                if (tree->gtFlags & GTF_IND_ASG_LHS)
                {
                    chars += printf("[IND_ASG_LHS]");
                }
                if (tree->gtFlags & GTF_IND_INVARIANT)
                {
                    chars += printf("[IND_INVARIANT]");
                }
                if (tree->gtFlags & GTF_IND_NONNULL)
                {
                    chars += printf("[IND_NONNULL]");
                }
                break;

            case GT_COPY_BLK:
            case GT_INIT_BLK:
                if (tree->AsDynBlk()->IsVolatile())
                {
                    chars += printf("[IND_VOLATILE]");
                }
                if (tree->AsDynBlk()->IsUnaligned())
                {
                    chars += printf("[BLK_UNALIGNED]");
                }
                break;

            case GT_ADD:
            case GT_LSH:
            case GT_MUL:
            case GT_COMMA:
                if ((tree->gtFlags & GTF_ADDRMODE_NO_CSE) != 0)
                {
                    chars += printf("[ADDRMODE_NO_CSE]");
                }
                break;

            case GT_MOD:
            case GT_UMOD:
                break;

            case GT_EQ:
            case GT_NE:
            case GT_LT:
            case GT_LE:
            case GT_GT:
            case GT_GE:

                if (tree->gtFlags & GTF_RELOP_NAN_UN)
                {
                    chars += printf("[RELOP_NAN_UN]");
                }
                if (tree->gtFlags & GTF_RELOP_JMP_USED)
                {
                    chars += printf("[RELOP_JMP_USED]");
                }
                break;

            case GT_QMARK:

                if (tree->gtFlags & GTF_QMARK_CAST_INSTOF)
                {
                    chars += printf("[QMARK_CAST_INSTOF]");
                }
                break;

            case GT_CNS_INT:

            {
                unsigned handleKind = (tree->gtFlags & GTF_ICON_HDL_MASK);

                switch (handleKind)
                {

                    case GTF_ICON_MODULE_HDL:

                        chars += printf("[ICON_MODULE_HDL]");
                        break;

                    case GTF_ICON_CLASS_HDL:

                        chars += printf("[ICON_CLASS_HDL]");
                        break;

                    case GTF_ICON_METHOD_HDL:

                        chars += printf("[ICON_METHOD_HDL]");
                        break;

                    case GTF_ICON_FIELD_HDL:

                        chars += printf("[ICON_FIELD_HDL]");
                        break;

                    case GTF_ICON_STATIC_HDL:

                        chars += printf("[ICON_STATIC_HDL]");
                        break;

                    case GTF_ICON_STR_HDL:

                        chars += printf("[ICON_STR_HDL]");
                        break;

                    case GTF_ICON_CONST_PTR:

                        chars += printf("[ICON_CONST_PTR]");
                        break;

                    case GTF_ICON_GLOBAL_PTR:

                        chars += printf("[ICON_GLOBAL_PTR]");
                        break;

                    case GTF_ICON_VARG_HDL:

                        chars += printf("[ICON_VARG_HDL]");
                        break;

                    case GTF_ICON_PINVKI_HDL:

                        chars += printf("[ICON_PINVKI_HDL]");
                        break;

                    case GTF_ICON_TOKEN_HDL:

                        chars += printf("[ICON_TOKEN_HDL]");
                        break;
#ifdef WINDOWS_X86_ABI
                    case GTF_ICON_TLS_HDL:
                        chars += printf("[ICON_TLD_HDL]");
                        break;
#endif
                    case GTF_ICON_FTN_ADDR:

                        chars += printf("[ICON_FTN_ADDR]");
                        break;

                    case GTF_ICON_CIDMID_HDL:

                        chars += printf("[ICON_CIDMID_HDL]");
                        break;

                    case GTF_ICON_BBC_PTR:

                        chars += printf("[ICON_BBC_PTR]");
                        break;
                }
            }
            break;

            case GT_CALL:

                if (tree->gtFlags & GTF_CALL_UNMANAGED)
                {
                    chars += printf("[CALL_UNMANAGED]");
                }
                if (tree->gtFlags & GTF_CALL_INLINE_CANDIDATE)
                {
                    chars += printf("[CALL_INLINE_CANDIDATE]");
                }
                if (!tree->AsCall()->IsVirtual())
                {
                    chars += printf("[CALL_NONVIRT]");
                }
                if (tree->AsCall()->IsVirtualVtable())
                {
                    chars += printf("[CALL_VIRT_VTABLE]");
                }
                if (tree->AsCall()->IsVirtualStub())
                {
                    chars += printf("[CALL_VIRT_STUB]");
                }
                if (tree->gtFlags & GTF_CALL_NULLCHECK)
                {
                    chars += printf("[CALL_NULLCHECK]");
                }
#ifdef TARGET_X86
                if (tree->gtFlags & GTF_CALL_POP_ARGS)
                {
                    chars += printf("[CALL_POP_ARGS]");
                }
#endif
                if (tree->gtFlags & GTF_CALL_HOISTABLE)
                {
                    chars += printf("[CALL_HOISTABLE]");
                }

                // More flags associated with calls.

                {
                    GenTreeCall* call = tree->AsCall();

                    if (call->gtCallMoreFlags & GTF_CALL_M_EXPLICIT_TAILCALL)
                    {
                        chars += printf("[CALL_M_EXPLICIT_TAILCALL]");
                    }
                    if (call->gtCallMoreFlags & GTF_CALL_M_TAILCALL)
                    {
                        chars += printf("[CALL_M_TAILCALL]");
                    }
                    if (call->gtCallMoreFlags & GTF_CALL_M_VARARGS)
                    {
                        chars += printf("[CALL_M_VARARGS]");
                    }
                    if (call->gtCallMoreFlags & GTF_CALL_M_RETBUFFARG)
                    {
                        chars += printf("[CALL_M_RETBUFFARG]");
                    }
                    if (call->gtCallMoreFlags & GTF_CALL_M_DELEGATE_INV)
                    {
                        chars += printf("[CALL_M_DELEGATE_INV]");
                    }
                    if (call->gtCallMoreFlags & GTF_CALL_M_NOGCCHECK)
                    {
                        chars += printf("[CALL_M_NOGCCHECK]");
                    }
                    if (call->gtCallMoreFlags & GTF_CALL_M_SPECIAL_INTRINSIC)
                    {
                        chars += printf("[CALL_M_SPECIAL_INTRINSIC]");
                    }

                    if (call->IsUnmanaged())
                    {
                        if (call->gtCallMoreFlags & GTF_CALL_M_UNMGD_THISCALL)
                        {
                            chars += printf("[CALL_M_UNMGD_THISCALL]");
                        }
                    }
                    else if (call->IsVirtualStub())
                    {
                        if (call->gtCallMoreFlags & GTF_CALL_M_VIRTSTUB_REL_INDIRECT)
                        {
                            chars += printf("[CALL_M_VIRTSTUB_REL_INDIRECT]");
                        }
                    }

#ifdef TARGET_X86
                    if (call->gtCallMoreFlags & GTF_CALL_M_TAILCALL_VIA_JIT_HELPER)
                    {
                        chars += printf("[CALL_M_TAILCALL_VIA_JIT_HELPER]");
                    }
#endif
#if FEATURE_TAILCALL_OPT
                    if (call->gtCallMoreFlags & GTF_CALL_M_IMPLICIT_TAILCALL)
                    {
                        chars += printf("[CALL_M_IMPLICIT_TAILCALL]");
                    }
#endif
                    if (call->gtCallMoreFlags & GTF_CALL_M_PINVOKE)
                    {
                        chars += printf("[CALL_M_PINVOKE]");
                    }

                    if (call->IsFatPointerCandidate())
                    {
                        chars += printf("[CALL_FAT_POINTER_CANDIDATE]");
                    }

                    if (call->IsGuarded())
                    {
                        chars += printf("[CALL_GUARDED]");
                    }

                    if (call->IsExpRuntimeLookup())
                    {
                        chars += printf("[CALL_EXP_RUNTIME_LOOKUP]");
                    }
                }
                break;
            default:

            {
                unsigned flags = (tree->gtFlags & (~(unsigned)(GTF_COMMON_MASK | GTF_OVERFLOW)));
                if (flags != 0)
                {
                    chars += printf("[%08X]", flags);
                }
            }
            break;
        }

        // Common flags.

        if (tree->gtFlags & GTF_ASG)
        {
            chars += printf("[ASG]");
        }
        if (tree->gtFlags & GTF_CALL)
        {
            chars += printf("[CALL]");
        }
        switch (op)
        {
            case GT_MUL:
            case GT_CAST:
            case GT_ADD:
            case GT_SUB:
                if (tree->gtFlags & GTF_OVERFLOW)
                {
                    chars += printf("[OVERFLOW]");
                }
                break;
            default:
                break;
        }
        if (tree->gtFlags & GTF_EXCEPT)
        {
            chars += printf("[EXCEPT]");
        }
        if (tree->gtFlags & GTF_GLOB_REF)
        {
            chars += printf("[GLOB_REF]");
        }
        if (tree->gtFlags & GTF_ORDER_SIDEEFF)
        {
            chars += printf("[ORDER_SIDEEFF]");
        }
        if (tree->gtFlags & GTF_REVERSE_OPS)
        {
            if (op != GT_LCL_VAR)
            {
                chars += printf("[REVERSE_OPS]");
            }
        }
        if (tree->gtFlags & GTF_IND_NONFAULTING)
        {
            if (tree->OperIsIndirOrArrLength())
            {
                chars += printf("[IND_NONFAULTING]");
            }
        }
        if (tree->gtFlags & GTF_MAKE_CSE)
        {
            chars += printf("[MAKE_CSE]");
        }
        if (tree->gtFlags & GTF_DONT_CSE)
        {
            chars += printf("[DONT_CSE]");
        }
        if (tree->gtFlags & GTF_BOOLEAN)
        {
            chars += printf("[BOOLEAN]");
        }
        if (tree->gtFlags & GTF_UNSIGNED)
        {
            chars += printf("[SMALL_UNSIGNED]");
        }
        if (tree->gtFlags & GTF_REUSE_REG_VAL)
        {
            if (op == GT_CNS_INT)
            {
                chars += printf("[REUSE_REG_VAL]");
            }
        }
    }
}

void dTreeFlags(GenTree* tree)
{
    cTreeFlags(JitTls::GetCompiler(), tree);
}

#endif // DEBUG

#if VARSET_COUNTOPS
// static
BitSetSupport::BitSetOpCounter Compiler::m_varsetOpCounter("VarSetOpCounts.log");
#endif
#if ALLVARSET_COUNTOPS
// static
BitSetSupport::BitSetOpCounter Compiler::m_allvarsetOpCounter("AllVarSetOpCounts.log");
#endif

// static
const HelperCallProperties Compiler::s_helperCallProperties;

/*****************************************************************************/
/*****************************************************************************/

//------------------------------------------------------------------------
// killGCRefs:
// Given some tree node return does it need all GC refs to be spilled from
// callee save registers.
//
// Arguments:
//    tree       - the tree for which we ask about gc refs.
//
// Return Value:
//    true       - tree kills GC refs on callee save registers
//    false      - tree doesn't affect GC refs on callee save registers
bool Compiler::killGCRefs(GenTree* tree)
{
    if (tree->IsCall())
    {
        GenTreeCall* call = tree->AsCall();
        if (call->IsUnmanaged())
        {
            return true;
        }

        if (call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_JIT_PINVOKE_BEGIN))
        {
            assert(opts.ShouldUsePInvokeHelpers());
            return true;
        }
    }
    else if (tree->OperIs(GT_START_PREEMPTGC))
    {
        return true;
    }

    return false;
}

//------------------------------------------------------------------------
// lvaIsOSRLocal: check if this local var is one that requires special
//     treatment for OSR compilations.
//
// Arguments:
//    varNum     - variable of interest
//
// Return Value:
//    true       - this is an OSR compile and this local requires special treatment
//    false      - not an OSR compile, or not an interesting local for OSR

bool Compiler::lvaIsOSRLocal(unsigned varNum)
{
    if (!opts.IsOSR())
    {
        return false;
    }

    if (varNum < info.compLocalsCount)
    {
        return true;
    }

    LclVarDsc* varDsc = lvaGetDesc(varNum);

    if (varDsc->lvIsStructField)
    {
        return (varDsc->lvParentLcl < info.compLocalsCount);
    }

    return false;
}

//------------------------------------------------------------------------------
// gtChangeOperToNullCheck: helper to change tree oper to a NULLCHECK.
//
// Notes:
//    the function should not be called after lowering for platforms that do not support
//    emitting NULLCHECK nodes, like arm32. Use `Lowering::TransformUnusedIndirection`
//    that handles it and calls this function when appropriate.
//
void Compiler::gtChangeOperToNullCheck(GenTree* tree)
{
    assert(tree->OperIs(GT_FIELD_ADDR, GT_IND, GT_OBJ, GT_BLK));

    // TODO-MIKE-Cleanup: There are multiple places that have special handling for FIELD_ADDR.
    // All that could probably done here instead. See impImportPop, inlInitInlineeArgs and
    // gtTryRemoveBoxUpstreamEffects.

    tree->ChangeOper(GT_NULLCHECK);
    tree->SetType(TYP_INT);
}

#if defined(DEBUG)
//------------------------------------------------------------------------------
// devirtualizationDetailToString: describe the detailed devirtualization reason
//
// Arguments:
//    detail - detail to describe
//
// Returns:
//    descriptive string
//
const char* Compiler::devirtualizationDetailToString(CORINFO_DEVIRTUALIZATION_DETAIL detail)
{
    switch (detail)
    {
        case CORINFO_DEVIRTUALIZATION_UNKNOWN:
            return "unknown";
        case CORINFO_DEVIRTUALIZATION_SUCCESS:
            return "success";
        case CORINFO_DEVIRTUALIZATION_FAILED_CANON:
            return "object class was canonical";
        case CORINFO_DEVIRTUALIZATION_FAILED_COM:
            return "object class was com";
        case CORINFO_DEVIRTUALIZATION_FAILED_CAST:
            return "object class could not be cast to interface class";
        case CORINFO_DEVIRTUALIZATION_FAILED_LOOKUP:
            return "interface method could not be found";
        case CORINFO_DEVIRTUALIZATION_FAILED_DIM:
            return "interface method was default interface method";
        case CORINFO_DEVIRTUALIZATION_FAILED_SUBCLASS:
            return "object not subclass of base class";
        case CORINFO_DEVIRTUALIZATION_FAILED_SLOT:
            return "virtual method installed via explicit override";
        case CORINFO_DEVIRTUALIZATION_FAILED_BUBBLE:
            return "devirtualization crossed version bubble";
        case CORINFO_DEVIRTUALIZATION_MULTIPLE_IMPL:
            return "object class has multiple implementations of interface";
        case CORINFO_DEVIRTUALIZATION_FAILED_BUBBLE_CLASS_DECL:
            return "decl method is defined on class and decl method not in version bubble, and decl method not in "
                   "type closest to version bubble";
        case CORINFO_DEVIRTUALIZATION_FAILED_BUBBLE_INTERFACE_DECL:
            return "decl method is defined on interface and not in version bubble, and implementation type not "
                   "entirely defined in bubble";
        case CORINFO_DEVIRTUALIZATION_FAILED_BUBBLE_IMPL:
            return "object class not defined within version bubble";
        case CORINFO_DEVIRTUALIZATION_FAILED_BUBBLE_IMPL_NOT_REFERENCEABLE:
            return "object class cannot be referenced from R2R code due to missing tokens";
        case CORINFO_DEVIRTUALIZATION_FAILED_DUPLICATE_INTERFACE:
            return "crossgen2 virtual method algorithm and runtime algorithm differ in the presence of duplicate "
                   "interface implementations";
        case CORINFO_DEVIRTUALIZATION_FAILED_DECL_NOT_REPRESENTABLE:
            return "Decl method cannot be represented in R2R image";
        default:
            return "undefined";
    }
}
#endif // defined(DEBUG)
