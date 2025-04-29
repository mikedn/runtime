// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "hostallocator.h"
#include "patchpointinfo.h"
#include "jitstd/algorithm.h"
#include "jitgcinfo.h"

extern ICorJitHost* g_jitHost;

static AssemblyNamesList2* s_pAltJitExcludeAssembliesList;
#ifdef DEBUG
static AssemblyNamesList2* s_pJitDisasmIncludeAssembliesList;
static MethodSet*          s_pJitMethodSet;
static LONG                s_jitNestingLevel;

unsigned Compiler::jitTotalMethodCompiled;
#endif

#if defined(DEBUG) || MEASURE_NODE_SIZE || MEASURE_BLOCK_SIZE || DISPLAY_SIZES
static unsigned genMethodCnt;  // total number of methods JIT'ted
unsigned        genMethodICnt; // number of interruptible methods
unsigned        genMethodNCnt; // number of non-interruptible methods
static unsigned genSmallMethodsNeedingExtraMemoryCnt = 0;
#endif

#if MEASURE_NODE_SIZE
NodeSizeStats         genNodeSizeStats;
NodeSizeStats         genNodeSizeStatsPerFunc;
static const unsigned genTreeNcntHistBuckets[]{10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 5000, 10000, 0};
Histogram             genTreeNcntHist(genTreeNcntHistBuckets);
static const unsigned genTreeNsizHistBuckets[]{1000, 5000, 10000, 50000, 100000, 500000, 1000000, 0};
Histogram             genTreeNsizHist(genTreeNsizHistBuckets);
#endif

#if MEASURE_MEM_ALLOC
static const unsigned memAllocHistBuckets[]{64, 128, 192, 256, 512, 1024, 4096, 8192, 0};
Histogram             memAllocHist(memAllocHistBuckets);
static const unsigned memUsedHistBuckets[]{16, 32, 64, 128, 192, 256, 512, 1024, 4096, 8192, 0};
Histogram             memUsedHist(memUsedHistBuckets);
#endif

#if DISPLAY_SIZES
// Variables to keep track of total code amounts.
size_t grossVMsize; // Total IL code size
size_t grossNCsize; // Native code + data size
size_t totalNCsize; // Native code + data + GC info size (TODO-Cleanup: GC info size only accurate for JIT32_GCENCODER)
size_t gcHeaderISize; // GC header      size: interruptible methods
size_t gcPtrMapISize; // GC pointer map size: interruptible methods
size_t gcHeaderNSize; // GC header      size: non-interruptible methods
size_t gcPtrMapNSize; // GC pointer map size: non-interruptible methods
#endif

#if COUNT_BASIC_BLOCKS

// --------------------------------------------------
// Basic block count frequency table:
// --------------------------------------------------
//     <=         1 ===>  26872 count ( 56% of total)
//      2 ..      2 ===>    669 count ( 58% of total)
//      3 ..      3 ===>   4687 count ( 68% of total)
//      4 ..      5 ===>   5101 count ( 78% of total)
//      6 ..     10 ===>   5575 count ( 90% of total)
//     11 ..     20 ===>   3028 count ( 97% of total)
//     21 ..     50 ===>   1108 count ( 99% of total)
//     51 ..    100 ===>    182 count ( 99% of total)
//    101 ..   1000 ===>     34 count (100% of total)
//   1001 ..  10000 ===>      0 count (100% of total)
// --------------------------------------------------
static const unsigned bbCntBuckets[]{1, 2, 3, 5, 10, 20, 50, 100, 1000, 10000, 0};
Histogram             bbCntTable(bbCntBuckets);
// Histogram for the IL opcode size of methods with a single basic block
static const unsigned bbSizeBuckets[]{1, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 0};
Histogram             bbOneBBSizeTable(bbSizeBuckets);
#endif

#if COUNT_LOOPS
// Used by optFindNaturalLoops to gather statistical information such as
//  - total number of natural loops
//  - number of loops with 1, 2, ... exit conditions
//  - number of loops that have an iterator (for like)
//  - number of loops that have a constant iterator
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
// Histogram for number of loops in a method
static const unsigned loopCountBuckets[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 0};
Histogram             loopCountTable(loopCountBuckets);
// Histogram for number of loop exits
static const unsigned loopExitCountBuckets[]{0, 1, 2, 3, 4, 5, 6, 0};
Histogram             loopExitCountTable(loopExitCountBuckets);
#endif

#if MEASURE_NOWAY

// Code to measure and rank dynamic occurrences of noway_assert (just the appearances
// of noway_assert, whether the assert is true or false). This might help characterize
// the cost of noway_assert in non-DEBUG builds, or determine which noway_assert should
// be simple DEBUG-only asserts.

struct FileLine
{
    char*    m_file    = nullptr;
    unsigned m_line    = 0;
    char*    m_condStr = nullptr;

    FileLine() = default;

    FileLine(const char* file, unsigned line, const char* condStr) : m_line(line)
    {
        size_t newSize = strlen(file) + 1;
        m_file         = HostAllocator::getHostAllocator().allocate<char>(newSize);
        strcpy_s(m_file, newSize, file);

        newSize   = strlen(condStr) + 1;
        m_condStr = HostAllocator::getHostAllocator().allocate<char>(newSize);
        strcpy_s(m_condStr, newSize, condStr);
    }

    FileLine(const FileLine& other) = default;

    static unsigned GetHashCode(const FileLine& fl)
    {
        unsigned code = fl.m_line;

        for (const char* p = fl.m_file; *p != '\0'; p++)
        {
            code += *p;
        }

        return code;
    }

    static bool Equals(const FileLine& fl1, const FileLine& fl2)
    {
        return (fl1.m_line == fl2.m_line) && (strcmp(fl1.m_file, fl2.m_file) == 0);
    }
};

using FileLineToCountMap = JitHashMap<FileLine, size_t, FileLine, HostAllocator>;
static FileLineToCountMap* NowayAssertMap;

void Compiler::RecordNowayAssert(const char* filename, unsigned line, const char* condStr)
{
    if (NowayAssertMap == nullptr)
    {
        NowayAssertMap = new (HostAllocator::getHostAllocator()) FileLineToCountMap(HostAllocator::getHostAllocator());
    }

    // TODO-MIKE-Review: This stuff is not thread safe...
    (*NowayAssertMap->Emplace({filename, line, condStr}, 0))++;
}

void RecordNowayAssertGlobal(const char* filename, unsigned line, const char* condStr)
{
    if ((JitConfig.JitMeasureNowayAssert() == 1) && (JitTls::GetCompiler() != nullptr))
    {
        JitTls::GetCompiler()->RecordNowayAssert(filename, line, condStr);
    }
}

static void DisplayNowayAssertMap()
{
    if (NowayAssertMap == nullptr)
    {
        return;
    }

    FILE* fout = jitstdout;

    if (LPCWSTR strJitMeasureNowayAssertFile = JitConfig.JitMeasureNowayAssertFile())
    {
        fout = _wfopen(strJitMeasureNowayAssertFile, W("a"));

        if (fout == nullptr)
        {
            printf("Failed to open JitMeasureNowayAssertFile \"%ws\"\n", strJitMeasureNowayAssertFile);
            return;
        }
    }

    struct NowayAssertCountMap
    {
        FileLine fl;
        size_t   count;
    };

    // Iterate noway assert map, create sorted table by occurrence, dump it.
    unsigned             count = NowayAssertMap->GetCount();
    NowayAssertCountMap* nacp  = NowayAssertMap->GetAllocator().allocate<NowayAssertCountMap>(count);
    unsigned             i     = 0;

    for (const auto & [ fl, count ] : *NowayAssertMap)
    {
        new (&nacp[i++]) NowayAssertCountMap{fl, count};
    }

    jitstd::sort(nacp, nacp + count, [](const auto& x, const auto& y) { return y.count < x.count; });

    if (fout == jitstdout)
    {
        // Don't output the header if writing to a file, since we'll be appending to existing dumps in that case.
        fprintf(fout, "\nnoway_assert counts:\n");
        fprintf(fout, "count, file, line, text\n");
    }

    for (unsigned i = 0; i < count; i++)
    {
        fprintf(fout, "%u, %s, %u, \"%s\"\n", nacp[i].count, nacp[i].fl.m_file, nacp[i].fl.m_line,
                nacp[i].fl.m_condStr);
    }

    if (fout != jitstdout)
    {
        fclose(fout);
    }
}

#endif // MEASURE_NOWAY

#if LOOP_HOIST_STATS
static CritSecObject s_loopHoistStatsLock;
static unsigned      s_loopsConsidered;
static unsigned      s_loopsWithHoistedExpressions;
static unsigned      s_totalHoistedExpressions;

static void PrintAggregateLoopHoistStats(FILE* f)
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

void Compiler::AddLoopHoistStats() const
{
    CritSecHolder statsLock(s_loopHoistStatsLock);

    s_loopsConsidered += m_loopsConsidered;
    s_loopsWithHoistedExpressions += m_loopsWithHoistedExpressions;
    s_totalHoistedExpressions += m_totalHoistedExpressions;
}
#endif // LOOP_HOIST_STATS

#if DATAFLOW_ITER
// Variables to keep track of how many iterations we go in a dataflow pass
unsigned CSEiterCount; // counts the # of iteration for the CSE dataflow
unsigned CFiterCount;  // counts the # of iteration for the Const Folding dataflow
#endif

#if MEASURE_BLOCK_SIZE
size_t genFlowNodeSize;
size_t genFlowNodeCnt;
#endif

#if FUNC_INFO_LOGGING
static LPCWSTR compJitFuncInfoFilename;
FILE*          Compiler::compJitFuncInfoFile;
#endif

#ifdef DEBUG
ConfigMethodRange fJitStressRange;
#endif

void Compiler::compStartup()
{
#ifdef JIT32_GCENCODER
    InitGCEncoderLookupTable();
#endif

#if DISPLAY_SIZES
    grossVMsize = grossNCsize = totalNCsize = 0;
#endif

#if MEASURE_NODE_SIZE
    GenTree::DumpNodeSizes(jitstdout);
#endif
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
#endif

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

#ifdef FEATURE_JIT_METHOD_PERF
    if (compJitTimeLogFilename != nullptr)
    {
        if (FILE* jitTimeLogFile = _wfopen(compJitTimeLogFilename, W("a")))
        {
            CompTimeSummaryInfo::s_compTimeSummary.Print(jitTimeLogFile);
            fclose(jitTimeLogFile);
        }
    }

    JitTimer::Shutdown();
#endif // FEATURE_JIT_METHOD_PERF

    FILE* const fout = jitstdout;

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

        fprintf(fout, "[%7u VM, %8u %6s %4u%%] %s\n", grossVMsize, grossNCsize, Target::CpuName(),
                100 * grossNCsize / grossVMsize, "Total (excluding GC info)");

        fprintf(fout, "[%7u VM, %8u %6s %4u%%] %s\n", grossVMsize, totalNCsize, Target::CpuName(),
                100 * totalNCsize / grossVMsize, "Total (including GC info)");

        if (gcHeaderISize || gcHeaderNSize)
        {
            fprintf(fout, "\n");

            fprintf(fout, "GC tables   : [%7uI,%7uN] %7u byt  (%u%% of IL, %u%% of %s).\n",
                    gcHeaderISize + gcPtrMapISize, gcHeaderNSize + gcPtrMapNSize, totalNCsize - grossNCsize,
                    100 * (totalNCsize - grossNCsize) / grossVMsize, 100 * (totalNCsize - grossNCsize) / grossNCsize,
                    Target::CpuName());

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
    , compProfilerCallback(false)
    , compHasNextCallRetAddr(false)
    , compVarScopeExtended(false)
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
#if defined(DEBUG) || defined(LATE_DISASM) || defined(DUMP_FLOWGRAPHS)
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

static CORINFO_InstructionSetFlags FilterInstructionSet(
    CORINFO_InstructionSetFlags instructionSetFlags ARM64_ARG(bool matchedVM))
{
// NOTE: This function needs to be kept in sync with EEJitManager::SetCpuInfo() in vm\codeman.cpp

// The VM will set the ISA flags depending on actual hardware support.
// We then select which ISAs to leave enabled based on the JIT config.
// The exception to this is the dummy Vector64/128/256 ISAs, which must be added explicitly.

#ifdef TARGET_XARCH
    if (JitConfig.EnableHWIntrinsic())
    {
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
    // is a pre-existing config flag that controls the SSE3+ ISAs
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
#endif
    }

    if (!JitConfig.EnableBMI1())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_BMI1);
#ifdef TARGET_AMD64
        instructionSetFlags.RemoveInstructionSet(InstructionSet_BMI1_X64);
#endif
    }

    if (!JitConfig.EnableBMI2())
    {
        instructionSetFlags.RemoveInstructionSet(InstructionSet_BMI2);
#ifdef TARGET_AMD64
        instructionSetFlags.RemoveInstructionSet(InstructionSet_BMI2_X64);
#endif
    }
#endif // TARGET_XARCH

#ifdef TARGET_ARM64
    if (!matchedVM)
    {
        // The x86/x64 architecture capabilities flags overlap with the ARM64 ones. Set a reasonable architecture
        // target default. Currently this is disabling all ARM64 architecture features except FP and SIMD, but this
        // should be altered to possibly enable all of them, when they are known to all work.
        instructionSetFlags = {};
        instructionSetFlags.AddInstructionSet(InstructionSet_ArmBase);
        instructionSetFlags.AddInstructionSet(InstructionSet_AdvSimd);
        instructionSetFlags.Set64BitInstructionSetVariants();
    }

    if (JitConfig.EnableHWIntrinsic())
    {
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
#endif // TARGET_ARM64

    return EnsureInstructionSetFlagsAreValid(instructionSetFlags);
}

void Compiler::compSetProcessor()
{
    assert(!compIsForInlining());

#ifdef TARGET_XARCH
    codeGen->SetUseVEXEncoding(canUseVexEncoding());
#endif
}

bool Compiler::notifyInstructionSetUsage(CORINFO_InstructionSet isa, bool supported) const
{
    const char* isaString = InstructionSetToString(isa);
    JITDUMP("Notify VM instruction set (%s) %s be supported.\n", isaString, supported ? "must" : "must not");
    return info.compCompHnd->notifyInstructionSetUsage(isa, supported);
}

#ifdef PROFILING_SUPPORTED
// A dummy function to receive Enter/Leave/Tailcall profiler callbacks.
// These are used when JitEltHookEnabled=1
static void DummyProfilerELTStub(UINT_PTR ProfilerHandle AMD64_ARG(UINT_PTR callerSP))
{
}
#endif

bool Compiler::compShouldThrowOnNoway() const
{
    // In min opts, we don't want the noway assert to go through the exception
    // path. Instead we want it to just silently go through codegen for
    // compat reasons.
    return !opts.MinOpts();
}

void Compiler::compInitAltJit()
{
    assert(!compIsForInlining());

    const JitConfigValues::MethodSet& altJitMethods =
        opts.IsJitFlagSet(JitFlags::JIT_FLAG_PREJIT) ? JitConfig.AltJitNgen() : JitConfig.AltJit();

    // Some options don't affect the real jit when an altjit is present. The real jit has no way to know
    // if an altjit is present so we simply assume it is present if the altjit method list is not empty.
    INDEBUG(opts.isAltJitPresent = !altJitMethods.isEmpty());

    if (!opts.IsJitFlagSet(JitFlags::JIT_FLAG_ALT_JIT))
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

        if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_PREJIT))
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
        }
#endif

        opts.disDiffable  = cfg.DiffableDasm() != 0;
        opts.dspDiffable  = cfg.DiffableDasm() != 0;
        opts.disAddr      = cfg.JitDasmWithAddress() != 0;
        opts.disAlignment = cfg.JitDasmWithAlignmentBoundaries() != 0;

        const auto& dumpNameSet = opts.IsJitFlagSet(JitFlags::JIT_FLAG_PREJIT) ? cfg.NgenDump() : cfg.JitDump();
        const int   dumpHash    = opts.IsJitFlagSet(JitFlags::JIT_FLAG_PREJIT) ? cfg.NgenHashDump() : cfg.JitHashDump();

        if (dumpNameSet.contains(methodName, className, methodParams) ||
            ((dumpHash != -1) && (static_cast<unsigned>(dumpHash) == info.compMethodHash())))
        {
            verbose      = true;
            verboseTrees = cfg.JitDumpVerboseTrees() == 1;
            verboseSsa   = cfg.JitDumpVerboseSsa() == 1;

            opts.dspEHTable = true;
            opts.dspGCtbls  = true;
            opts.dspUnwind  = true;
        }

        // TODO-MIKE-SSA: This doesn't work with new SSA because it transforms
        // assignments into stores and doesn't accept stores as input.
        // opts.optRepeat = cfg.JitOptRepeat().contains(methodName, className, methodParams);
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
    AMD64_ONLY(opts.enableRIPRelativeAddressing = JitConfig.EnablePCRelAddr() != 0);
#endif // TARGET_XARCH
#endif // DEBUG
}

void Compiler::compInitOptions()
{
    assert(!compIsForInlining());

    opts.optFlags = CLFLG_MAXOPT; // Default value is for full optimization

    if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_DEBUG_CODE) || opts.IsJitFlagSet(JitFlags::JIT_FLAG_MIN_OPT) ||
        opts.IsJitFlagSet(JitFlags::JIT_FLAG_TIER0))
    {
        opts.optFlags = CLFLG_MINOPT;
    }
    // Don't optimize .cctors (except prejit) or if we're an inlinee
    else if (!opts.IsJitFlagSet(JitFlags::JIT_FLAG_PREJIT) && ((info.compFlags & FLG_CCTOR) == FLG_CCTOR))
    {
        opts.optFlags = CLFLG_MINOPT;
    }

    // Default value is to generate a blend of size and speed optimizations
    opts.compCodeOpt = BLENDED_CODE;

    // If the EE sets SIZE_OPT or if we are compiling a Class constructor
    // we will optimize for code size at the expense of speed
    if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_SIZE_OPT) || ((info.compFlags & FLG_CCTOR) == FLG_CCTOR))
    {
        opts.compCodeOpt = SMALL_CODE;
    }
    // If the EE sets SPEED_OPT we will optimize for speed at the expense of code size
    else if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_SPEED_OPT) ||
             (opts.IsJitFlagSet(JitFlags::JIT_FLAG_TIER1) && !opts.IsJitFlagSet(JitFlags::JIT_FLAG_MIN_OPT)))
    {
        opts.compCodeOpt = FAST_CODE;
        assert(!opts.IsJitFlagSet(JitFlags::JIT_FLAG_SIZE_OPT));
    }

    opts.compDbgCode = opts.IsJitFlagSet(JitFlags::JIT_FLAG_DEBUG_CODE);
    opts.compDbgInfo = opts.IsJitFlagSet(JitFlags::JIT_FLAG_DEBUG_INFO);
    opts.compDbgEnC  = opts.IsJitFlagSet(JitFlags::JIT_FLAG_DEBUG_EnC);

#if REGEN_SHORTCUTS || REGEN_CALLPAT
    // We never want to have debugging enabled when regenerating GC encoding patterns
    opts.compDbgCode = false;
    opts.compDbgInfo = false;
    opts.compDbgEnC  = false;
#endif

    opts.lvaEnregEHVars = compEnregLocals() && JitConfig.EnableEHWriteThru();

#if DEBUG
    if (opts.lvaEnregEHVars)
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
            opts.lvaEnregEHVars = false;
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
        printf("Generating code for %s %s\n", Target::PlatformName(), Target::CpuName());
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
    if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_PROF_ENTERLEAVE))
    {
        bool hookNeeded;
        bool indirected;
        info.compCompHnd->GetProfilingHandle(&hookNeeded, &opts.compProfilerMethHnd, &indirected);
        opts.compProfilerHookNeeded = hookNeeded;

        // TODO-MIKE-Review: All the compProfilerMethHndIndirected code is dead,
        // CrossGen2 does not support profiling like NGen did.
        opts.compProfilerMethHndIndirected = indirected;
    }
    else
    {
        opts.compProfilerHookNeeded        = false;
        opts.compProfilerMethHndIndirected = false;
        opts.compProfilerMethHnd           = nullptr;
    }

    // Honor COMPlus_JitELTHookEnabled or STRESS_PROFILER_CALLBACKS stress mode
    // only if VM has not asked us to generate profiler hooks in the first place.
    // That is, override VM only if it hasn't asked for a profiler callback for this method.
    // Don't run this stress mode when pre-JITing, as we would need to emit a relocation
    // for the call to the fake ELT hook, which wouldn't make sense, as we can't store that
    // in the pre-JIT image.
    if (!opts.compProfilerHookNeeded &&
        (JitConfig.JitELTHookEnabled() ||
         (!opts.IsJitFlagSet(JitFlags::JIT_FLAG_PREJIT) && compStressCompile(STRESS_PROFILER_CALLBACKS, 5))))
    {
        opts.compJitELTHookEnabled = true;

        // TBD: Exclude PInvoke stubs
        opts.compProfilerMethHnd           = (void*)DummyProfilerELTStub;
        opts.compProfilerMethHndIndirected = false;
    }
#endif // PROFILING_SUPPORTED

    ARM_ONLY(opts.compUseSoftFP = opts.IsJitFlagSet(JitFlags::JIT_FLAG_SOFTFP_ABI) || JitConfig.JitSoftFP();)

    opts.compReloc = opts.IsJitFlagSet(JitFlags::JIT_FLAG_RELOC);

#ifndef TARGET_ARM64
    // TODO-ARM64-NYI: enable hot/cold splitting
    if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_PROCSPLIT))
    {
        // Note that opts.compDbgCode is true under NGen for checked assemblies!
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
        // eeInfo is this Compiler's own copy so we can modify it
        const_cast<CORINFO_EE_INFO*>(eeInfo)->maxUncheckedOffsetForNullObject =
            static_cast<size_t>(JitConfig.JitMaxUncheckedOffset());

        JITDUMP("STRESS_NULL_OBJECT_CHECK: maxUncheckedOffsetForNullObject=0x%X\n",
                eeInfo->maxUncheckedOffsetForNullObject);
    }

    if (verbose)
    {
        compDumpOptions();
    }
#endif
}

void Compiler::compInitPgo()
{
    if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_BBOPT))
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

// Estimates conservatively for an explicit tail call,
// if the importer may actually use a tail call.
//
// Return Value:
//    - False if a tail call will not be generated
//    - True if a tail call *may* be generated
//
// Assumptions:
//    - compInitOptions() has been called
//    - info.compIsVarArgs has been initialized
//    - An explicit tail call has been seen
//    - compSetOptimizationLevel() has not been called
//
bool Compiler::compMayExplicitTailCall()
{
    assert(!compIsForInlining());

    if (info.IsSynchronized())
    {
        return false;
    }

    if (opts.IsReversePInvoke())
    {
        return false;
    }

#if !FEATURE_FIXED_OUT_ARGS
    if (info.compIsVarArgs)
    {
        return false;
    }
#endif

    return true;
}

// Determines if conditions are met to allow switching the opt level to optimized
// This method is to be called at some point before compSetOptimizationLevel to determine
// if the opt level may be changed based on information gathered in early phases.
// It is assumed that compInitOptions has already been called.
bool Compiler::compCanSwitchToOptimized() const
{
    assert(!compIsForInlining());

    bool result = opts.IsJitFlagSet(JitFlags::JIT_FLAG_TIER0) && !opts.IsJitFlagSet(JitFlags::JIT_FLAG_MIN_OPT) &&
                  !opts.compDbgCode;
    if (result)
    {
        // Ensure that it would be safe to change the opt level
        assert(opts.optFlags == CLFLG_MINOPT);
        assert(!opts.compMinOptsIsSet);
    }

    return result;
}

// Switch the opt level from tier 0 to optimized
// This method is to be called at some point before compSetOptimizationLevel to switch
// the opt level to optimized based on information gathered in early phases.
void Compiler::compSwitchToOptimized()
{
    JITDUMP("****\n**** JIT Tier0 jit request switching to Tier1 because of loop\n****\n");

    assert(compCanSwitchToOptimized());
    assert(opts.IsJitFlagSet(JitFlags::JIT_FLAG_TIER0));

    opts.ClearJitFlag(JitFlags::JIT_FLAG_TIER0);
    opts.ClearJitFlag(JitFlags::JIT_FLAG_BBINSTR);

    INDEBUG(compSwitchedToOptimized = true);

    compInitOptions();

    info.compCompHnd->setMethodAttribs(info.compMethodHnd, CORINFO_FLG_SWITCHED_TO_OPTIMIZED);
}

void Compiler::compInitDebuggingInfo()
{
    assert(!compIsForInlining());

    JITDUMP("*************** In compInitDebuggingInfo() for %s\n", info.compFullName);

    info.compVarScopesCount = 0;
    compEnterScopeList      = nullptr;
    compExitScopeList       = nullptr;

    if (opts.compDbgInfo)
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

void Compiler::compSetOptimizationLevel(const ILStats& ilStats)
{
    assert(!compIsForInlining());

    bool theMinOptsValue = false;

    if (opts.optFlags == CLFLG_MINOPT)
    {
        JITLOG(LL_INFO100, "CLFLG_MINOPT set for method %s\n", info.compFullName);
        theMinOptsValue = true;
    }

#ifdef DEBUG
    unsigned jitMinOpts = JitConfig.JitMinOpts();

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
                    JITDUMP(" Optimizations disabled by JitMinOpts and methodCount\n");
                    theMinOptsValue = true;
                }
                break;
            case 0xD:
            {
                unsigned firstMinopts  = (jitMinOpts >> 12) & 0xFFF;
                unsigned secondMinopts = (jitMinOpts >> 0) & 0xFFF;

                if ((firstMinopts == methodCountMask) || (secondMinopts == methodCountMask))
                {
                    JITDUMP("0xD: Optimizations disabled by JitMinOpts and methodCountMask\n");
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
                    JITDUMP("0xE: Optimizations disabled by JitMinOpts and methodCountMask\n");
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
                    JITDUMP("0xF: Optimizations disabled by JitMinOpts and methodCountMask\n");
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
    else if (!opts.IsJitFlagSet(JitFlags::JIT_FLAG_PREJIT))
    {
        if (JitConfig.JitMinOptsCodeSize() < info.compILCodeSize)
        {
            JITLOG(LL_INFO10, "IL Code Size exceeded, using MinOpts for method %s\n", info.compFullName);
            theMinOptsValue = true;
        }
        else if (JitConfig.JitMinOptsInstrCount() < ilStats.instrCount)
        {
            JITLOG(LL_INFO10, "IL instruction count exceeded, using MinOpts for method %s\n", info.compFullName);
            theMinOptsValue = true;
        }
        else if (JitConfig.JitMinOptsBbCount() < fgBBcount)
        {
            JITLOG(LL_INFO10, "Basic Block count exceeded, using MinOpts for method %s\n", info.compFullName);
            theMinOptsValue = true;
        }
        else if (JitConfig.JitMinOptsLvNumCount() < lvaCount)
        {
            JITLOG(LL_INFO10, "Local Variable Num count exceeded, using MinOpts for method %s\n", info.compFullName);
            theMinOptsValue = true;
        }
        else if (JitConfig.JitMinOptsLvRefCount() < ilStats.lclRefCount)
        {
            JITLOG(LL_INFO10, "Local Variable Ref count exceeded, using MinOpts for method %s\n", info.compFullName);
            theMinOptsValue = true;
        }

        if (theMinOptsValue)
        {
            JITLOG(LL_INFO10000, "IL Code Size,Instr %4d,%4d, Basic Block count %3d, Local Variable Num,Ref count "
                                 "%3d,%3d for method %s\n",
                   info.compILCodeSize, ilStats.instrCount, fgBBcount, lvaCount, ilStats.lclRefCount,
                   info.compFullName);
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
    if (!theMinOptsValue && !opts.IsJitFlagSet(JitFlags::JIT_FLAG_PREJIT) &&
        ((DEFAULT_MIN_OPTS_CODE_SIZE < info.compILCodeSize) || (DEFAULT_MIN_OPTS_INSTR_COUNT < ilStats.instrCount) ||
         (DEFAULT_MIN_OPTS_BB_COUNT < fgBBcount) || (DEFAULT_MIN_OPTS_LV_NUM_COUNT < lvaCount) ||
         (DEFAULT_MIN_OPTS_LV_REF_COUNT < ilStats.lclRefCount)))
    {
        theMinOptsValue = true;
    }
#endif // DEBUG

    JITLOG(LL_INFO10000,
           "IL Code Size,Instr %4d,%4d, Basic Block count %3d, Local Variable Num,Ref count %3d,%3d for method %s\n",
           info.compILCodeSize, ilStats.instrCount, fgBBcount, lvaCount, ilStats.lclRefCount, info.compFullName);

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
    if (theMinOptsValue && !opts.IsJitFlagSet(JitFlags::JIT_FLAG_TIER0) &&
        !opts.IsJitFlagSet(JitFlags::JIT_FLAG_MIN_OPT) && !opts.compDbgCode)
    {
        info.compCompHnd->setMethodAttribs(info.compMethodHnd, CORINFO_FLG_SWITCHED_TO_MIN_OPT);
        opts.ClearJitFlag(JitFlags::JIT_FLAG_TIER1);
        INDEBUG(compSwitchedToMinOpts = true);
    }

    JITDUMP("OPTIONS: opts.MinOpts() == %s\n", opts.MinOpts() ? "true" : "false");

    if (opts.OptimizationDisabled())
    {
        opts.optFlags = CLFLG_MINOPT;
    }

    if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_PREJIT))
    {
        // The JIT doesn't currently support loop alignment for prejitted images.
        // (The JIT doesn't know the final address of the code, hence
        // it can't align code based on unknown addresses.)

        opts.alignLoops = false; // loop alignment not supported for prejitted code
    }
    else
    {
        opts.alignLoops = JitConfig.JitAlignLoops() == 1;
    }
}

void Compiler::BeginPhase(Phases phase)
{
    mostRecentlyActivePhase = phase;
}

void Compiler::EndPhase(Phases phase)
{
#ifdef FEATURE_JIT_METHOD_PERF
    if (pCompJitTimer != nullptr)
    {
        pCompJitTimer->EndPhase(this, phase);
    }
#endif

    mostRecentlyActivePhase = phase;
}

void Compiler::compCompile(void** nativeCode, uint32_t* nativeCodeSize)
{
    assert(!compIsForInlining());

    DoPhase(this, PHASE_INCPROFILE, &Compiler::phIncorporateProfileData);

    if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_BBINSTR))
    {
        DoPhase(this, PHASE_IBCPREP, &Compiler::fgPrepareToInstrumentMethod);
    }

    DoPhase(this, PHASE_IMPORTATION, &Compiler::phImport);

    if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_BBINSTR))
    {
        DoPhase(this, PHASE_IBCINSTR, &Compiler::phInstrumentMethod);
    }

    DoPhase(this, PHASE_INDXCALL, &Compiler::phTransformIndirectCalls);
    DoPhase(this, PHASE_PATCHPOINTS, &Compiler::phTransformPatchpoints);

#if !FEATURE_EH
    // If we aren't yet supporting EH in a compiler bring-up, remove as many EH handlers as possible,
    // so we can pass tests that contain try/catch EH, but don't actually throw any exceptions.
    fgRemoveEH();
#endif

    DoPhase(this, PHASE_REMOVE_NOT_IMPORTED, &Compiler::phRemoveNotImportedBlocks);
    DoPhase(this, PHASE_MORPH_INLINE, &Compiler::phInline);

    RecordStateAtEndOfInlining();

    DoPhase(this, PHASE_ALLOCATE_OBJECTS, &Compiler::phMorphAllocObj);
    DoPhase(this, PHASE_MORPH_ADD_INTERNAL, &Compiler::phAddInternal);

    if (opts.OptimizationEnabled() && (compHndBBtabCount != 0))
    {
        DoPhase(this, PHASE_EMPTY_TRY, &Compiler::phRemoveEmptyTry);
        DoPhase(this, PHASE_EMPTY_FINALLY, &Compiler::phRemoveEmptyFinally);
        DoPhase(this, PHASE_MERGE_FINALLY_CHAINS, &Compiler::phMergeFinallyChains);
        DoPhase(this, PHASE_CLONE_FINALLY, &Compiler::phCloneFinally);
#ifdef TARGET_ARM
        DoPhase(this, PHASE_UPDATE_FINALLY_FLAGS, &Compiler::phUpdateFinallyTargetFlags);
#endif
    }

    DoPhase(this, PHASE_COMPUTE_PREDS, &Compiler::phComputePreds);

    if (opts.OptimizationEnabled())
    {
        DoPhase(this, PHASE_MERGE_THROWS, &Compiler::phTailMergeThrows);
        DoPhase(this, PHASE_EARLY_UPDATE_FLOW_GRAPH, &Compiler::phUpdateFlowGraph);
    }

    DoPhase(this, PHASE_PROMOTE_STRUCTS, &Compiler::phPromoteStructs);
    DoPhase(this, PHASE_STR_ADRLCL, &Compiler::phMarkAddressExposedLocals);
    DoPhase(this, PHASE_MORPH_GLOBAL, &Compiler::phGlobalMorph);

    if (getNeedsGSSecurityCookie())
    {
        DoPhase(this, PHASE_GS_COOKIE, &Compiler::phGSCookie);
    }

    DoPhase(this, PHASE_COMPUTE_BLOCK_WEIGHTS, &Compiler::phComputeBlockWeights);
    DoPhase(this, PHASE_COMPUTE_EDGE_WEIGHTS1, &Compiler::phComputeEdgeWeights);
#ifdef FEATURE_EH_FUNCLETS
    DoPhase(this, PHASE_RELOCATE_FUNCLETS, &Compiler::phRelocateFunclets);
#endif

    if (opts.OptimizationEnabled())
    {
        DoPhase(this, PHASE_INVERT_LOOPS, &Compiler::phInvertLoops);
        DoPhase(this, PHASE_OPTIMIZE_LAYOUT, &Compiler::phOptimizeLayout);
        DoPhase(this, PHASE_COMPUTE_REACHABILITY, &Compiler::phComputeReachability);
        DoPhase(this, PHASE_COMPUTE_DOMINATORS, &Compiler::phComputeDoms);
        DoPhase(this, PHASE_FIND_LOOPS, &Compiler::phFindLoops);
        DoPhase(this, PHASE_CLONE_LOOPS, &Compiler::phCloneLoops);
        DoPhase(this, PHASE_UNROLL_LOOPS, &Compiler::phUnrollLoops);
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
        DoPhase(this, PHASE_ADD_COPIES, &Compiler::optAddCopies);
        DoPhase(this, PHASE_OPTIMIZE_BOOLS, &Compiler::phOptimizeBools);

        // phOptimizeBools might have changed the number of blocks;
        // the dominators/reachability might be bad.
        // TODO-MIKE-Review: So should fgDomsComputed be set to false?
    }

    DoPhase(this, PHASE_SET_EVAL_ORDER, &Compiler::phSetEvalOrder);

    // TODO-MIKE-Review: Can this be done after the SSA optimizations? Those can remove
    // dead code and we may end up with fully interruptible code for no reason.
    // But this depends on BBF_LOOP_HEAD, which is set only by phComputeReachability.
    // And optRemoveRedundantZeroInits depends on the code not being fully interruptible.
    DoPhase(this, PHASE_SET_FULLY_INTERRUPTIBLE, &Compiler::phSetFullyInterruptible);

    if (opts.OptimizationEnabled()
#ifdef OPT_CONFIG
        && (JitConfig.JitDoSsa() != 0)
#endif
            )
    {
        phSsaOpt();

        if (fgModified)
        {
            DoPhase(this, PHASE_OPT_UPDATE_FLOW_GRAPH, &Compiler::phUpdateFlowGraph);
            DoPhase(this, PHASE_COMPUTE_EDGE_WEIGHTS2, &Compiler::phComputeEdgeWeights);
        }

        // TODO-MIKE-Cleanup: These should be inside phSsaOpt.
        fgDomsComputed = false;
        vnStore        = nullptr;
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
        LclVarDsc* const lcl = lvaGetDesc(lclNum);

        // We expect all these to have stack homes, and be FP relative
        assert(lcl->lvOnFrame);
        assert(lcl->lvFramePointerBased);

        // Record FramePtr relative offset (no localloc yet)
        patchpointInfo->SetOffset(lclNum, lcl->GetStackOffset());

        // Note if IL stream contained an address-of that potentially leads to exposure.
        // This bit of IL may be skipped by OSR partial importation.
        if (lcl->lvHasLdAddrOp)
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
        patchpointInfo->SetSecurityCookieOffset(lvaGetDesc(lvaGSSecurityCookie)->GetStackOffset());
        JITDUMP("--OSR-- security cookie V%02u offset is FP %d\n", lvaGSSecurityCookie,
                patchpointInfo->SecurityCookieOffset());
    }

    // Register this with the runtime.
    info.compCompHnd->setPatchpointInfo(patchpointInfo);
}

CorJitResult Compiler::compCompileMain(void** nativeCode, uint32_t* nativeCodeSize, JitFlags* jitFlags)
{
    assert(!compIsForInlining());

    // Verification isn't supported
    assert(jitFlags->IsSet(JitFlags::JIT_FLAG_SKIP_VERIFICATION));
    assert(!jitFlags->IsSet(JitFlags::JIT_FLAG_IMPORT_ONLY));

    assert(HelperCallProperties::IsPure(CORINFO_HELP_GETSHARED_GCSTATIC_BASE));
    assert(!HelperCallProperties::IsPure(CORINFO_HELP_GETFIELDOBJ)); // quick sanity check

    INDEBUG(compDoComponentUnitTestsOnce());

#ifdef FEATURE_JIT_METHOD_PERF
    static bool checkedForJitTimeLog = false;

    if (!checkedForJitTimeLog)
    {
        // Call into VM to get the config strings. FEATURE_JIT_METHOD_PERF is enabled for
        // retail builds. Do not call the regular Config helper here as it would pull
        // in a copy of the config parser into the clrjit.dll.
        InterlockedCompareExchangeT(&Compiler::compJitTimeLogFilename,
                                    (LPCWSTR)info.compCompHnd->getJitTimeLogFilename(), nullptr);

        // At a process or module boundary clear the file and start afresh.
        JitTimer::PrintCsvHeader();

        checkedForJitTimeLog = true;
    }

    if ((Compiler::compJitTimeLogFilename != nullptr) || (JitTimeLogCsv() != nullptr))
    {
        pCompJitTimer = JitTimer::Create(this, info.compMethodInfo->ILCodeSize);
    }
#endif // FEATURE_JIT_METHOD_PERF

#if FUNC_INFO_LOGGING
    if (LPCWSTR tmpJitFuncInfoFilename = JitConfig.JitFuncInfoFile())
    {
        LPCWSTR oldFuncInfoFileName =
            InterlockedCompareExchangeT(&compJitFuncInfoFilename, tmpJitFuncInfoFilename, nullptr);

        if (oldFuncInfoFileName == nullptr)
        {
            assert(compJitFuncInfoFile == nullptr);
            compJitFuncInfoFile = _wfopen(compJitFuncInfoFilename, W("a"));

            if (compJitFuncInfoFile == nullptr)
            {
#if defined(DEBUG) && !defined(HOST_UNIX)
                perror("Failed to open JitFuncInfoLogFile");
#endif
            }
        }
    }
#endif // FUNC_INFO_LOGGING

    {
        constexpr uint32_t IMAGE_FILE_MACHINE_TARGET =
#if defined(TARGET_X86)
            IMAGE_FILE_MACHINE_I386
#elif defined(TARGET_AMD64)
            IMAGE_FILE_MACHINE_AMD64
#elif defined(TARGET_ARM)
            IMAGE_FILE_MACHINE_ARMNT
#elif defined(TARGET_ARM64)
            IMAGE_FILE_MACHINE_ARM64
#else
#error Unsupported or unset target architecture
#endif
            ;

        constexpr CORINFO_OS CORINFO_OS_TARGET =
#ifdef TARGET_UNIX
            CORINFO_UNIX
#else
            CORINFO_WINNT
#endif
            ;

        info.compMatchedVM = (info.compCompHnd->getExpectedTargetArchitecture() == IMAGE_FILE_MACHINE_TARGET) &&
                             (eeGetEEInfo()->osType == CORINFO_OS_TARGET);
    }

    opts.SetJitFlags(jitFlags->GetFlagRaw(),
                     FilterInstructionSet(jitFlags->GetInstructionSetFlags() ARM64_ARG(info.compMatchedVM))
                         .GetFlagsRaw());

    if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_OSR))
    {
        info.compPatchpointInfo = info.compCompHnd->getOSRInfo(&info.compILEntry);

        assert(info.compPatchpointInfo != nullptr);
    }

    info.compClassHnd  = info.compCompHnd->getMethodClass(info.compMethodHnd);
    info.compClassAttr = info.compCompHnd->getClassAttribs(info.compClassHnd);

#ifdef DEBUG
    if (JitConfig.EnableExtraSuperPmiQueries())
    {
        // This call to getClassModule/getModuleAssembly/getAssemblyName fails in crossgen2 due
        // to these APIs being unimplemented. So disable this extra info for pre-jit mode.
        // See https://github.com/dotnet/runtime/issues/48888.

        if (!opts.IsJitFlagSet(JitFlags::JIT_FLAG_PREJIT))
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
        CorJitResult result = CORJIT_INTERNALERROR;
    } param;

    param.jitInfo        = info.compCompHnd;
    param.compiler       = this;
    param.nativeCode     = nativeCode;
    param.nativeCodeSize = nativeCodeSize;

    PAL_TRY(Param&, p, param)
    {
        p.result = p.compiler->compCompileHelper(p.nativeCode, p.nativeCodeSize);
    }
    PAL_FINALLY
    {
    }
    PAL_ENDTRY

    return param.result;
}

CorJitResult Compiler::compCompileHelper(void** nativeCode, uint32_t* nativeCodeSize)
{
    assert(!compIsForInlining());

    CORINFO_METHOD_HANDLE methodHnd = info.compMethodHnd;

    if (info.compILCodeSize == 0)
    {
        BADCODE("code size is zero");
    }

    info.compFlags = info.compCompHnd->getMethodAttribs(info.compMethodHnd);
#ifdef PSEUDORANDOM_NOP_INSERTION
    info.compChecksum = getMethodBodyChecksum(methodInfo->ILCode, methodInfo->ILCodeSize);
#endif

    compInitAltJit();
    compInitConfigOptions();
    compSetProcessor();
    compInitOptions();

    if (!opts.altJit && opts.IsJitFlagSet(JitFlags::JIT_FLAG_ALT_JIT))
    {
        // We're an altjit, but the COMPlus_AltJit configuration did not say to compile this method,
        // so skip it.
        return CORJIT_SKIPPED;
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("IL to import:\n");
        DumpILRange(info.compCode, info.compILCodeSize);
    }
#endif

    if (JitConfig.JitAggressiveInlining())
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
    info.compPublishStubParam = opts.IsJitFlagSet(JitFlags::JIT_FLAG_PUBLISH_SECRET_PARAM);

    if (opts.IsReversePInvoke())
    {
        bool unused;
        info.compCallConv = info.compCompHnd->getUnmanagedCallConv(info.compMethodInfo->ftn, nullptr, &unused);
    }
    else
    {
        info.compCallConv = CorInfoCallConvExtension::Managed;
    }

    if (info.compMethodInfo->args.isVarArg())
    {
        info.compIsVarArgs = true;
    }

    lvaInitTable();
    compInitDebuggingInfo();
    compInitPgo();

    ILStats ilStats;

    if (!opts.IsJitFlagSet(JitFlags::JIT_FLAG_PREJIT))
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
        impCanInlineIL(methodHnd, info.compMethodInfo, info.compFlags, &prejitResult);

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

    if (
        // Method has an explicit tail call that may run like a loop or may not be generated as a tail
        // call in tier 0, switch to optimized to avoid spending too much time running slower code and
        // to avoid stack overflow from recursion
        ((impHasExplicitTailCall && compMayExplicitTailCall()) ||
         // Method likely has a loop, switch to the OptimizedTier to avoid spending too much time running slower code
         (compHasBackwardJump && ((info.compFlags & CORINFO_FLG_DISABLE_TIER0_FOR_LOOPS) != 0))) &&
        compCanSwitchToOptimized())
    {

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
        static LONG s_compMethodsCount;

        compMethodID = static_cast<unsigned>(InterlockedIncrement(&s_compMethodsCount));
    }

    if (JitConfig.DumpJittedMethods() == 1)
    {
        printf("Compiling %4d %s::%s, IL size = %u, hash=0x%08x %s%s%s\n", Compiler::jitTotalMethodCompiled,
               info.compClassName, info.compMethodName, info.compILCodeSize, info.compMethodHash(),
               compGetTieringName(), opts.IsOSR() ? " OSR" : "", compGetStressMessage());
    }
#endif

    INDEBUG(compFunctionTraceStart());
    compCompile(nativeCode, nativeCodeSize);
    INDEBUG(compFunctionTraceEnd(*nativeCode, *nativeCodeSize, false));
    compCompileFinish();

    // Did we just compile for a target architecture that the VM isn't expecting? If so, the VM
    // can't used the generated code (and we better be an AltJit!).

    if (!info.compMatchedVM)
    {
        return CORJIT_SKIPPED;
    }

#ifdef DEBUG
    if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_ALT_JIT) && (JitConfig.RunAltJitCode() == 0))
    {
        return CORJIT_SKIPPED;
    }
#endif

    return CORJIT_OK;
}

// Records data needed for inlining data dumps. Should be
// called after inlining is complete. (We do this after inlining
// because this marks the last point at which the JIT is likely to
// cause type-loading and class initialization).
void Compiler::RecordStateAtEndOfInlining()
{
#if defined(DEBUG) || defined(INLINE_DATA)
    m_compCyclesAtEndOfInlining    = 0;
    m_compTickCountAtEndOfInlining = 0;

    if (CycleTimer::GetThreadCyclesS(&m_compCyclesAtEndOfInlining))
    {
        m_compTickCountAtEndOfInlining = GetTickCount();
    }
#endif
}

// Capture timing data (if enabled) after compilation is completed.
void Compiler::RecordStateAtEndOfCompilation()
{
#if defined(DEBUG) || defined(INLINE_DATA)
    m_compCycles = 0;

    if (uint64_t compCyclesAtEnd; CycleTimer::GetThreadCyclesS(&compCyclesAtEnd))
    {
        assert(compCyclesAtEnd >= m_compCyclesAtEndOfInlining);

        m_compCycles = compCyclesAtEnd - m_compCyclesAtEndOfInlining;
    }
#endif
}

void Compiler::compCompileFinish()
{
#if FUNC_INFO_LOGGING
    if (FILE* funcInfoFile = compJitFuncInfoFile)
    {
        assert(!compIsForInlining());
        fprintf(funcInfoFile, "%s\n",
#ifdef DEBUG
                info.compFullName
#else
                eeGetMethodFullName(info.compMethodHnd)
#endif
                );
        fflush(funcInfoFile);
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
#endif

#if MEASURE_NODE_SIZE
    genTreeNcntHist.record(static_cast<unsigned>(genNodeSizeStatsPerFunc.genTreeNodeCnt));
    genTreeNsizHist.record(static_cast<unsigned>(genNodeSizeStatsPerFunc.genTreeNodeSize));
#endif

#ifdef DEBUG
    // Small methods should fit in ArenaAllocator::getDefaultPageSize(), or else
    // we should bump up ArenaAllocator::getDefaultPageSize()

    if ((info.compILCodeSize <= 32) &&    // Is it a reasonably small method?
        (codeGen->GetCodeSize() < 512) && // Some trivial methods generate huge native code. eg. pushing a single
        // huge struct
        (compInlinedCodeSize <= 128) && // Is the the inlining reasonably bounded?
        // Small methods cannot meaningfully have a big number of locals
        // or arguments. We always track arguments at the start of
        // the prolog which requires memory
        (info.compLocalsCount <= 32) && (!opts.MinOpts()) && // We may have too many local variables, etc
        (JitConfig.JitStress() == 0) &&                      // We need extra memory for stress
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
            printf("         |  Profiled   | Method   |   Method has    |   calls   | Num |LclV |AProp| CSE |   Perf  |bytes | %3s codesize| \n", Target::CpuName());
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
        printf(" %3d |", fgBBcount);
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

        if (codeGen->GetPerfScore() < 9999.995)
        {
            printf(" %7.2f |", codeGen->GetPerfScore());
        }
        else
        {
            printf(" %7.0f |", codeGen->GetPerfScore());
        }

        printf(" %4d |", info.compMethodInfo->ILCodeSize);
        printf(" %5d |", codeGen->GetHotCodeSize());
        printf(" %3d |", codeGen->GetColdCodeSize());

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
    if (!opts.IsJitFlagSet(JitFlags::JIT_FLAG_PREJIT))
    {
        if (compJitHaltMethod())
        {
#ifndef HOST_UNIX
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

#if defined(DEBUG) || defined(INLINE_DATA)
// Get a hash code of the currently jitted method's full name.
unsigned CompiledMethodInfo::compMethodHash() const
{
    if (compMethodHashPrivate == 0)
    {
        assert((compFullName != nullptr) && (compFullName[0] != 0));
        // Use compFullName to generate the hash, as it contains the signature and return type
        compMethodHashPrivate = HashStringA(compFullName);
    }

    return compMethodHashPrivate;
}

// Get a hash code of the currently jitted method's full name.
unsigned Compiler::compMethodHash(CORINFO_METHOD_HANDLE methodHnd)
{
    if (methodHnd == info.compMethodHnd)
    {
        return info.compMethodHash();
    }

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

#if defined(FEATURE_JIT_METHOD_PERF) || defined(DUMP_FLOWGRAPHS)
const char* PhaseNames[]{
#define CompPhaseNameMacro(enum_nm, string_nm, short_nm, hasChildren, parent, measureIR) string_nm,
#include "compphases.h"
};

const LPCWSTR PhaseShortNames[]{
#define CompPhaseNameMacro(enum_nm, string_nm, short_nm, hasChildren, parent, measureIR) W(short_nm),
#include "compphases.h"
};
#endif

#ifdef FEATURE_JIT_METHOD_PERF
bool PhaseHasChildren[]{
#define CompPhaseNameMacro(enum_nm, string_nm, short_nm, hasChildren, parent, measureIR) hasChildren,
#include "compphases.h"
};

int PhaseParent[]{
#define CompPhaseNameMacro(enum_nm, string_nm, short_nm, hasChildren, parent, measureIR) parent,
#include "compphases.h"
};

bool PhaseReportsIRSize[]{
#define CompPhaseNameMacro(enum_nm, string_nm, short_nm, hasChildren, parent, measureIR) measureIR,
#include "compphases.h"
};

CritSecObject       CompTimeSummaryInfo::s_compTimeSummaryLock;
CompTimeSummaryInfo CompTimeSummaryInfo::s_compTimeSummary;
#if MEASURE_CLRAPI_CALLS
double JitTimer::s_cyclesPerSec = CachedCyclesPerSecond();
#endif

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

LPCWSTR Compiler::compJitTimeLogFilename;

void CompTimeSummaryInfo::Print(FILE* f) const
{
    if (f == nullptr)
    {
        return;
    }

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

        static const char* APInames[]{
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

                uint64_t cycles = m_total.m_perClrAPIcycles[i];
                double   millis = 1000.0 * cycles / countsPerSec;

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

                uint32_t maxcyc = m_maximum.m_maxClrAPIcycles[i];
                double   max_ms = 1000.0 * maxcyc / countsPerSec;

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

// Little helpers to grab the current cycle counter value; this is done
// differently based on target architecture, host toolchain, etc. The
// main thing is to keep the overhead absolutely minimal; in fact, on
// x86/x64 we use RDTSC even though it's not thread-safe; GetThreadCycles
// (which is monotonous) is just too expensive.

#if defined(HOST_X86) || defined(HOST_AMD64)
#if defined(_MSC_VER)

#include <intrin.h>
static bool _our_GetThreadCycles(uint64_t* cycleOut)
{
    *cycleOut = __rdtsc();
    return true;
}

#elif defined(__GNUC__)

static bool _our_GetThreadCycles(uint64_t* cycleOut)
{
    uint32_t hi, lo;
    __asm__ __volatile__("rdtsc" : "=a"(lo), "=d"(hi));
    *cycleOut = (static_cast<uint64_t>(hi) << 32) | static_cast<uint64_t>(lo);
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
#endif

JitTimer::JitTimer(unsigned byteCodeSize) : m_info(byteCodeSize)
{
    uint64_t threadCurCycles;

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

    uint64_t threadCurCycles;
    if (_our_GetThreadCycles(&threadCurCycles))
    {
        uint64_t phaseCycles = (threadCurCycles - m_curPhaseStart);

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
            uint64_t threadCurCycles;
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
                m_info.m_maxClrAPIcycles[apix] = max(m_info.m_maxClrAPIcycles[apix], (uint32_t)threadCurCycles);

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

void JitTimer::PrintCsvMethodStats(Compiler* comp) const
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
    uint64_t totCycles = 0;
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

    if (comp->codeGen != nullptr)
    {
        fprintf(s_csvFile, "%u,", comp->codeGen->GetCodeSize());
#ifdef JIT32_GCENCODER
        fprintf(s_csvFile, "%u,", comp->codeGen->GetGCInfoSize());
#endif
    }

    fprintf(s_csvFile, "%Iu,", comp->compGetArenaAllocator()->getTotalBytesAllocated());
    fprintf(s_csvFile, "%I64u,", m_info.m_totalCycles);
    fprintf(s_csvFile, "%f\n", CachedCyclesPerSecond());

    fflush(s_csvFile);
}

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

static unsigned adler32(unsigned adler, uint8_t* buf, unsigned int len)
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

static unsigned getMethodBodyChecksum(uint8_t* code, int size)
{
    return adler32(0, code, size);
}
#endif // PSEUDORANDOM_NOP_INSERTION

#ifdef DEBUG

void Compiler::compDumpOptions()
{
    // If we are compiling for a specific tier, make that very obvious in the output.
    // Note that we don't expect multiple TIER flags to be set at one time, but there
    // is nothing preventing that.
    if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_TIER0))
    {
        printf("OPTIONS: Tier-0 compilation (set COMPlus_TieredCompilation=0 to disable)\n");
    }
    if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_TIER1))
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

    if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_OSR))
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

    if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_BBOPT) && fgHaveProfileData())
    {
        printf("OPTIONS: optimized using %s profile data\n", pgoSourceToString(fgPgoSource));
    }

    if (fgPgoFailReason != nullptr)
    {
        printf("OPTIONS: %s\n", fgPgoFailReason);
    }

    if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_PREJIT))
    {
        printf("OPTIONS: Jit invoked for ngen\n");
    }
}

void Compiler::compDoComponentUnitTestsOnce()
{
    static bool DidComponentUnitTests;

    if (!JitConfig.RunComponentUnitTests())
    {
        return;
    }

    if (!DidComponentUnitTests)
    {
        DidComponentUnitTests = true;
        RunValueNumStoreTests(this);
        BitSetSupport::TestSuite(getAllocator(CMK_DebugOnly));
    }
}

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

// Should we use a "stress-mode" for the given stressArea. We have different
//   areas to allow the areas to be mixed in different combinations in
//   different methods.
// 'weight' indicates how often (as a percentage) the area should be stressed.
//    It should reflect the usefulness:overhead ratio.
const LPCWSTR Compiler::s_compStressModeNames[STRESS_COUNT + 1]{
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
        JITDUMP("\n\n*** JitStress: %ws ***\n\n", s_compStressModeNames[stressArea]);
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
    const int stressLevel = JitConfig.JitStress();

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

// Helper to determine if the local should not be promoted under a stress mode.
// Rejects ~50% of the potential promotions if STRESS_PROMOTE_FEWER_STRUCTS is active.
bool Compiler::compPromoteFewerStructs(LclVarDsc* lcl)
{
    return compStressCompile(STRESS_PROMOTE_FEWER_STRUCTS, 50) &&
           (((info.compMethodHash() ^ lcl->GetLclNum()) & 1) == 0);
}

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
    const bool tier0 = opts.IsJitFlagSet(JitFlags::JIT_FLAG_TIER0);
    const bool tier1 = opts.IsJitFlagSet(JitFlags::JIT_FLAG_TIER1);
    assert(!tier0 || !tier1); // We don't expect multiple TIER flags to be set at one time.

    if (tier0)
    {
        return "Tier0";
    }
    else if (tier1)
    {
        if (opts.IsJitFlagSet(JitFlags::JIT_FLAG_OSR))
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
    if ((JitConfig.JitStressModeNames() != nullptr) || (JitConfig.JitStress() > 0))
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
        LONG newJitNestingLevel = InterlockedIncrement(&s_jitNestingLevel);
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
        LONG newJitNestingLevel = InterlockedDecrement(&s_jitNestingLevel);
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

// dumpConvertedVarSet() dumps the varset bits that are tracked
// variable indices, and we convert them to variable numbers, sort the variable numbers, and
// print them as variable numbers. To do this, we use a temporary set indexed by
// variable number. We can't use the "all varset" type because it is still size-limited, and might
// not be big enough to handle all possible variable numbers.
void dumpConvertedVarSet(Compiler* comp, VARSET_TP vars)
{
    bool* lclSet = static_cast<bool*>(_alloca(comp->lvaCount * sizeof(bool)));
    memset(lclSet, 0, comp->lvaCount * sizeof(bool));

    if (!VarSetOps::MayBeUninit(vars))
    {
        for (VarSetOps::Enumerator e(comp, vars); e.MoveNext();)
        {
            lclSet[comp->lvaGetDescByTrackedIndex(e.Current())->GetLclNum()] = true;
        }
    }

    bool first = true;
    printf("{");

    for (size_t lclNum = 0; lclNum < comp->lvaCount; lclNum++)
    {
        if (lclSet[lclNum])
        {
            printf("%sV%02u", first ? "" : " ", lclNum);
            first = false;
        }
    }

    printf("}");
}

void Compiler::dmpVarSetDiff(const char* name, VARSET_TP from, VARSET_TP to)
{
    bool* fromBits = static_cast<bool*>(_alloca(lvaCount * sizeof(bool)));
    memset(fromBits, 0, lvaCount * sizeof(bool));
    bool* toBits = static_cast<bool*>(_alloca(lvaCount * sizeof(bool)));
    memset(toBits, 0, lvaCount * sizeof(bool));

    for (VarSetOps::Enumerator e(this, from); e.MoveNext();)
    {
        fromBits[lvaGetDescByTrackedIndex(e.Current())->GetLclNum()] = true;
    }

    for (VarSetOps::Enumerator e(this, to); e.MoveNext();)
    {
        toBits[lvaGetDescByTrackedIndex(e.Current())->GetLclNum()] = true;
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

// The following functions are intended to be called from the debugger, to dump
// various data structures.
//
// The versions that start with 'c' take a Compiler* as the first argument.
// The versions that start with 'd' use the tlsCompiler, so don't require a Compiler*.
//
// Summary:
//      cBlock,      dBlock         : Display a basic block.
//      cBlocks,     dBlocks        : Display all the basic blocks of a function.
//      cBlocksV,    dBlocksV       : Display all the basic blocks of a function.
//                                    "V" means "verbose", and will dump all the trees.
//      cStmt,       dStmt          : Display a statement.
//      cTree,       dTree          : Display a tree.
//      cTreeLIR,    dTreeLIR       : Display a tree in LIR form.
//      cTrees,      dTrees         : Display all the trees in a function.
//      cEH,         dEH            : Display the EH handler table.
//      cVar,        dVar           : Display a local variable given its number.
//      cVarDsc,     dVarDsc        : Display a local variable given a LclVarDsc*.
//      cVars,       dVars          : Display the local variable table.
//      cBlockCheapPreds, dBlockCheapPreds : Display a block's cheap predecessors.
//      cBlockPreds, dBlockPreds    : Display a block's predecessors.
//      cBlockSuccs, dBlockSuccs    : Display a block's successors.
//      cReach,      dReach         : Display all block reachability.
//      cLiveness,   dLiveness      : Display per-block variable liveness.
//      cCVarSet,    dCVarSet       : Display a "converted" VARSET_TP: the varset is assumed to be tracked variable
//                                    indices. These are converted to variable numbers and sorted.
//      cLoop,       dLoop          : Display the blocks of a loop, including the trees.
//      cTreeFlags,  dTreeFlags     : Display tree flags
//
// The following don't require a Compiler* to work:
//      dRegMask                    : Display a regMaskTP.
//      dBlockList                  : Display a BasicBlockList*.

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
    comp->gtDispTree(tree);
}

void cTreeLIR(Compiler* comp, GenTree* tree)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *TreeLIR %u\n", sequenceNumber++);
    comp->dmpLIRNode(tree);
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
    comp->lvaDumpEntry(comp->lvaGetDesc(lclNum));
}

void cVarDsc(Compiler* comp, LclVarDsc* lcl)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *VarDsc %u\n", sequenceNumber++);
    comp->lvaDumpEntry(lcl);
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

void cLiveness(Compiler* comp)
{
    static unsigned sequenceNumber = 0; // separate calls with a number to indicate this function has been called
    printf("===================================================================== *Liveness %u\n", sequenceNumber++);
    comp->fgDispBBLiveness();
}

void cCVarSet(Compiler* comp, VARSET_TP vars)
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

void dVarDsc(LclVarDsc* lcl)
{
    cVarDsc(JitTls::GetCompiler(), lcl);
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

void dLiveness()
{
    cLiveness(JitTls::GetCompiler());
}

void dCVarSet(VARSET_TP vars)
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

    if (tree->GetID() == id)
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
    GenTreeFlags flags = tree->gtFlags;

    if (flags == GTF_NONE)
    {
        return;
    }

    printf("flags=");

#ifdef DEBUG
    if (tree->gtDebugFlags & GTF_DEBUG_NODE_LARGE)
    {
        printf("[NODE_LARGE]");
    }
    if (tree->gtDebugFlags & GTF_DEBUG_NODE_MORPHED)
    {
        printf("[MORPHED]");
    }
#endif

    switch (tree->GetOper())
    {
        case GT_LCL_LOAD:
        case GT_LCL_STORE:
        case GT_LCL_LOAD_FLD:
        case GT_LCL_STORE_FLD:
            if (flags & GTF_LCL_LAST_USE_MASK)
            {
                printf("[VAR_DEATH]");
            }
            FALLTHROUGH;
        case GT_LCL_ADDR:
            if (flags & GTF_VAR_CLONED)
            {
                printf("[VAR_CLONED]");
            }
            break;

        case GT_INDEX_ADDR:
            if (flags & GTF_INX_RNGCHK)
            {
                printf("[INX_RNGCHK]");
            }
            break;

        case GT_IND_LOAD:
        case GT_IND_STORE:
        case GT_IND_LOAD_OBJ:
        case GT_IND_STORE_OBJ:
        case GT_IND_LOAD_BLK:
        case GT_IND_STORE_BLK:
            if (tree->AsIndir()->IsVolatile())
            {
                printf("[IND_VOLATILE]");
            }
            if (tree->AsIndir()->IsUnaligned())
            {
                printf("[IND_UNALIGNED]");
            }
            if (flags & GTF_IND_TGT_HEAP)
            {
                printf("[IND_TGT_HEAP]");
            }
            if (flags & GTF_IND_TGT_NOT_HEAP)
            {
                printf("[IND_TGT_NOT_HEAP]");
            }
            if (flags & GTF_IND_INVARIANT)
            {
                printf("[IND_INVARIANT]");
            }
            if (flags & GTF_IND_NONNULL)
            {
                printf("[IND_NONNULL]");
            }
            FALLTHROUGH;
        case GT_ARR_LENGTH:
        case GT_NULLCHECK:
            if (flags & GTF_IND_NONFAULTING)
            {
                printf("[IND_NONFAULTING]");
            }
            break;

        case GT_COPY_BLK:
        case GT_INIT_BLK:
            if (tree->AsDynBlk()->IsVolatile())
            {
                printf("[IND_VOLATILE]");
            }
            if (tree->AsDynBlk()->IsUnaligned())
            {
                printf("[BLK_UNALIGNED]");
            }
            break;

        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GT:
        case GT_GE:
            if (tree->IsRelopUnordered())
            {
                printf("[RELOP_NAN_UN]");
            }
            if (tree->IsRelopUnsigned())
            {
                printf("[RELOP_UNSIGNED]");
            }
            break;

        case GT_QMARK:
            if (flags & GTF_QMARK_CAST_INSTOF)
            {
                printf("[QMARK_CAST_INSTOF]");
            }
            break;

        case GT_CNS_INT:
            switch (tree->AsIntCon()->GetHandleKind())
            {
                case HandleKind::Module:
                    printf("[ICON_MODULE]");
                    break;
                case HandleKind::Class:
                    printf("[ICON_CLASS]");
                    break;
                case HandleKind::Method:
                    printf("[ICON_METHOD]");
                    break;
                case HandleKind::Field:
                    printf("[ICON_FIELD]");
                    break;
                case HandleKind::Static:
                    printf("[ICON_STATIC]");
                    break;
                case HandleKind::String:
                    printf("[ICON_STRING]");
                    break;
                case HandleKind::ConstData:
                    printf("[ICON_CONST_DATA]");
                    break;
                case HandleKind::MutableData:
                    printf("[ICON_MUTABLE_DATA]");
                    break;
                case HandleKind::Token:
                    printf("[ICON_TOKEN]");
                    break;
                case HandleKind::MethodAddr:
                    printf("[ICON_METHOD_ADDR]");
                    break;
                case HandleKind::BlockCount:
                    printf("[ICON_BLOCK_COUNT]");
                    break;
#ifdef WINDOWS_X86_ABI
                case HandleKind::TLS:
                    printf("[ICON_TLS]");
                    break;
#endif
                default:
                    break;
            }
            break;

        case GT_CALL:
        {
            GenTreeCall* call = tree->AsCall();

            if (flags & GTF_CALL_INLINE_CANDIDATE)
            {
                printf("[CALL_INLINE_CANDIDATE]");
            }
            if (!call->IsVirtual())
            {
                printf("[CALL_NONVIRT]");
            }
            if (call->IsVirtualVtable())
            {
                printf("[CALL_VIRT_VTABLE]");
            }
            if (call->IsVirtualStub())
            {
                printf("[CALL_VIRT_STUB]");
            }
            if (call->IsDelegateInvoke())
            {
                printf("[CALL_DELEGATE_INVOKE]");
            }
            if (tree->AsCall()->HasNullCheck())
            {
                printf("[CALL_NULLCHECK]");
            }
            if (flags & GTF_CALL_HOISTABLE)
            {
                printf("[CALL_HOISTABLE]");
            }

            GenTreeCallFlags callFlags = call->gtCallMoreFlags;

            if (callFlags & GTF_CALL_M_EXPLICIT_TAILCALL)
            {
                printf("[CALL_M_EXPLICIT_TAILCALL]");
            }
            if (callFlags & GTF_CALL_M_TAILCALL)
            {
                printf("[CALL_M_TAILCALL]");
            }
            if (callFlags & GTF_CALL_M_VARARGS)
            {
                printf("[CALL_M_VARARGS]");
            }
            if (callFlags & GTF_CALL_M_REQUIRES_RETBUFF_ARG)
            {
                printf("[CALL_M_REQUIRES_RETBUFFARG]");
            }
            if (callFlags & GTF_CALL_M_HAS_RETBUFF_ARG)
            {
                printf("[CALL_M_HAS_RETBUFFARG]");
            }
            if (callFlags & GTF_CALL_M_NOGCCHECK)
            {
                printf("[CALL_M_NOGCCHECK]");
            }
            if (callFlags & GTF_CALL_M_SPECIAL_INTRINSIC)
            {
                printf("[CALL_M_SPECIAL_INTRINSIC]");
            }
#if FEATURE_TAILCALL_OPT
            if (callFlags & GTF_CALL_M_IMPLICIT_TAILCALL)
            {
                printf("[CALL_M_IMPLICIT_TAILCALL]");
            }
#endif
            if (callFlags & GTF_CALL_M_PINVOKE)
            {
                printf("[CALL_M_PINVOKE]");
            }

            if (call->IsFatPointerCandidate())
            {
                printf("[CALL_FAT_POINTER_CANDIDATE]");
            }

            if (call->IsGuarded())
            {
                printf("[CALL_GUARDED]");
            }

            if (call->IsExpRuntimeLookup())
            {
                printf("[CALL_EXP_RUNTIME_LOOKUP]");
            }
        }
        break;

        case GT_NOP:
        case GT_NO_OP:
        case GT_ADD:
        case GT_MUL:
        case GT_LSH:
        case GT_COMMA:
        case GT_MOD:
        case GT_UMOD:
            break;

        default:
            if (GenTreeFlags unknownFlags = flags & ~GTF_COMMON_MASK)
            {
                printf("[%08X]", unknownFlags);
            }
            break;
    }

    if (flags & GTF_ASG)
    {
        printf("[ASG]");
    }
    if (flags & GTF_CALL)
    {
        printf("[CALL]");
    }
    if (flags & GTF_EXCEPT)
    {
        printf("[EXCEPT]");
    }
    if (flags & GTF_GLOB_REF)
    {
        printf("[GLOB_REF]");
    }
    if (flags & GTF_ORDER_SIDEEFF)
    {
        printf("[ORDER_SIDEEFF]");
    }
    if (flags & GTF_REVERSE_OPS)
    {
        printf("[REVERSE_OPS]");
    }
    if (flags & GTF_MAKE_CSE)
    {
        printf("[MAKE_CSE]");
    }
    if (flags & GTF_DONT_CSE)
    {
        printf("[DONT_CSE]");
    }
    if (flags & GTF_NO_CSE)
    {
        printf("[NO_CSE]");
    }
    if (flags & GTF_BOOLEAN)
    {
        printf("[BOOLEAN]");
    }
    if (flags & GTF_REUSE_REG_VAL)
    {
        printf("[REUSE_REG_VAL]");
    }
}

void dTreeFlags(GenTree* tree)
{
    cTreeFlags(JitTls::GetCompiler(), tree);
}

#endif // DEBUG
