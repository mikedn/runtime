// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#if !defined(CONFIG_INT) || !defined(CONFIG_STRING) || !defined(CONFIG_METHODSET)
#error CONFIG_INT, CONFIG_STRING, and CONFIG_METHODSET must be defined before including this file.
#endif

#ifdef CONFIG_DECL
#define DEFAULT_MAX_INLINE_SIZE 100           // Methods with > DEFAULT_MAX_INLINE_SIZE IL bytes will never be inlined.
                                              // This can be overwritten by setting complus_JITInlineSize env variable.
#define DEFAULT_MAX_INLINE_DEPTH 20           // Methods at more than this level deep will not be inlined
#define DEFAULT_MAX_LOCALLOC_TO_LOCAL_SIZE 32 // fixed locallocs of this size or smaller will convert to local buffers

#define DEFAULT_MIN_OPTS_CODE_SIZE 60000
#define DEFAULT_MIN_OPTS_INSTR_COUNT 20000
#define DEFAULT_MIN_OPTS_BB_COUNT 2000
#define DEFAULT_MIN_OPTS_LV_NUM_COUNT 2000
#define DEFAULT_MIN_OPTS_LV_REF_COUNT 8000

// Default numbers used to perform loop alignment. All the numbers are chosen
// based on experimenting with various benchmarks.
//
// Default minimum loop block weight required to enable loop alignment.
#define DEFAULT_ALIGN_LOOP_MIN_BLOCK_WEIGHT 4
// By default a loop will be aligned at 32B address boundary to get better
// performance as per architecture manuals.
#define DEFAULT_ALIGN_LOOP_BOUNDARY 0x20
// For non-adaptive loop alignment, by default, only align a loop whose size is
// at most 3 times the alignment block size. If the loop is bigger than that, it is most
// likely complicated enough that loop alignment will not impact performance.
#define DEFAULT_MAX_LOOPSIZE_FOR_ALIGN DEFAULT_ALIGN_LOOP_BOUNDARY * 3

// Maximum number of locals before turning off the inlining
#define MAX_LV_NUM_COUNT_FOR_INLINING 512

static constexpr int DefaultVNMapSelectBudget = 100;
#endif

#ifdef DEBUG
#define OPT_CONFIG
#endif

#ifdef DEBUG

CONFIG_UNSIGNED(AltJitLimit, "AltJitLimit", 0)               // Max number of functions to use altjit for (decimal)
CONFIG_BOOL(AltJitSkipOnAssert, "AltJitSkipOnAssert", false) // If AltJit hits an assert, fall back to the fallback
                                                             // JIT. Useful in conjunction with
                                                             // COMPlus_ContinueOnAssert=1
CONFIG_BOOL(DebugBreakOnVerificationFailure, "DebugBreakOnVerificationFailure", false) // Halts the jit on
                                                                                       // verification failure
CONFIG_BOOL(DiffableDasm, "JitDiffableDasm", false)                                    // Make the disassembly diff-able
CONFIG_BOOL(JitDasmWithAddress, "JitDasmWithAddress", false)   // Print the process address next to each instruction of
                                                               // the disassembly
CONFIG_BOOL(DisplayLoopHoistStats, "JitLoopHoistStats", false) // Display JIT loop hoisting statistics
CONFIG_UNSIGNED(DisplayLsraStats, "JitLsraStats", 0)           // Display JIT Linear Scan Register Allocator statistics
                                                               // If set to "1", display the stats in textual format.
                                                               // If set to "2", display the stats in csv format.
                                                               // If set to "3", display the stats in summarize format.
                                                               // Recommended to use with JitStdOutFile flag.
CONFIG_STRING(JitLsraOrdering, "JitLsraOrdering")              // LSRA heuristics ordering
CONFIG_BOOL(DumpJittedMethods, "DumpJittedMethods", false)     // Prints all jitted methods to the console
#ifdef TARGET_AMD64
CONFIG_BOOL(EnablePCRelAddr, "JitEnablePCRelAddr", true) // Use RIP relative addressing
#endif
CONFIG_UNSIGNED(JitBreakEmitOutputInstr, "JitBreakEmitOutputInstr", UINT_MAX)
CONFIG_UNSIGNED(JitBreakMorphTree, "JitBreakMorphTree", UINT_MAX)
CONFIG_BOOL(JitBreakOnBadCode, "JitBreakOnBadCode", false)
CONFIG_BOOL(JitBreakOnMinOpts, "JitBreakOnMinOpts", false) // Halt if jit switches to MinOpts
CONFIG_BOOL(JitCloneLoops, "JitCloneLoops", true)       // If 0, don't clone. Otherwise clone loops for optimizations.
CONFIG_UNSIGNED(JitDefaultFill, "JitDefaultFill", 0xdd) // In debug builds, initialize the memory allocated by the
                                                        // arena allocator with this byte.

// Minimum weight needed for the first block of a loop to make it a candidate for alignment.
CONFIG_UNSIGNED(JitAlignLoopMinBlockWeight, "JitAlignLoopMinBlockWeight", DEFAULT_ALIGN_LOOP_MIN_BLOCK_WEIGHT)
// For non-adaptive alignment, minimum loop size (in bytes) for which alignment will be done.
// Defaults to 3 blocks of 32 bytes chunks = 96 bytes.
CONFIG_UNSIGNED(JitAlignLoopMaxCodeSize, "JitAlignLoopMaxCodeSize", DEFAULT_MAX_LOOPSIZE_FOR_ALIGN)
// For non-adaptive alignment, address boundary (power of 2) at which loop
// alignment should be done. By default, 32B.
CONFIG_UNSIGNED(JitAlignLoopBoundary, "JitAlignLoopBoundary", DEFAULT_ALIGN_LOOP_BOUNDARY)
// If set, for non-adaptive alignment, ensure loop jmps are not on or cross alignment boundary.
CONFIG_BOOL(JitAlignLoopForJcc, "JitAlignLoopForJcc", false)
// If set, perform adaptive loop alignment that limits number of padding based on loop size.
CONFIG_BOOL(JitAlignLoopAdaptive, "JitAlignLoopAdaptive", true)

// Print the alignment boundaries in disassembly.
CONFIG_BOOL(JitDasmWithAlignmentBoundaries, "JitDasmWithAlignmentBoundaries", false)

CONFIG_BOOL(JitDirectAlloc, "JitDirectAlloc", false)
CONFIG_UNSIGNED(JitDoubleAlign, "JitDoubleAlign", 1)
CONFIG_BOOL(JitDumpASCII, "JitDumpASCII", true)                // Uses only ASCII characters in tree dumps
CONFIG_BOOL(JitDumpVerboseSsa, "JitDumpVerboseSsa", false)     // Produce especially verbose dump output for SSA
CONFIG_BOOL(JitDumpVerboseTrees, "JitDumpVerboseTrees", false) // Enable more verbose tree dumps
CONFIG_BOOL(JitEmitPrintRefRegs, "JitEmitPrintRefRegs", false)
CONFIG_BOOL(JitEnableDevirtualization, "JitEnableDevirtualization", true)         // Enable devirtualization in importer
CONFIG_BOOL(JitEnableLateDevirtualization, "JitEnableLateDevirtualization", true) // Enable devirtualization after
                                                                                  // inlining
CONFIG_UNSIGNED(JitExpensiveDebugCheckLevel, "JitExpensiveDebugCheckLevel", 0)    // Level indicates how much checking
                                                                                  // beyond the default to do in debug
                                                                                  // builds (currently 1-2)
CONFIG_BOOL(JitForceFallback, "JitForceFallback", false) // Set to non-zero to test NOWAY assert by forcing a retry
CONFIG_BOOL(JitFullyInt, "JitFullyInt", false)           // Forces Fully interruptible code
CONFIG_BOOL(JitFunctionTrace, "JitFunctionTrace", false) // If non-zero, print JIT start/end logging
CONFIG_BOOL(JitGCChecks, "JitGCChecks", false)
CONFIG_BOOL(JitGCInfoLogging, "JitGCInfoLogging", false) // If true, prints GCInfo-related output to standard output.
CONFIG_UNSIGNED(JitHashBreak, "JitHashBreak", UINT_MAX)  // Same as JitBreak, but for a method hash
CONFIG_UNSIGNED(JitHashDump, "JitHashDump", UINT_MAX)    // Same as JitDump, but for a method hash
CONFIG_UNSIGNED(JitHashHalt, "JitHashHalt", UINT_MAX)    // Same as JitHalt, but for a method hash
CONFIG_INT(JitInlineAdditionalMultiplier, "JitInlineAdditionalMultiplier", 0)
CONFIG_UNSIGNED(JitInlineSize, "JitInlineSize", DEFAULT_MAX_INLINE_SIZE)
CONFIG_UNSIGNED(JitInlineDepth, "JitInlineDepth", DEFAULT_MAX_INLINE_DEPTH)
// Force using the large pseudo instruction form for long address
CONFIG_BOOL(JitLongAddress, "JitLongAddress", false)
CONFIG_UNSIGNED(JitMaxUncheckedOffset, "JitMaxUncheckedOffset", 8)
CONFIG_UNSIGNED(JitMinOpts, "JITMinOpts", 0) // Forces MinOpts
CONFIG_UNSIGNED(JitMinOptsBbCount, "JitMinOptsBbCount", DEFAULT_MIN_OPTS_BB_COUNT)
CONFIG_UNSIGNED(JitMinOptsCodeSize, "JitMinOptsCodeSize", DEFAULT_MIN_OPTS_CODE_SIZE)
CONFIG_UNSIGNED(JitMinOptsInstrCount, "JitMinOptsInstrCount", DEFAULT_MIN_OPTS_INSTR_COUNT)
CONFIG_UNSIGNED(JitMinOptsLvNumCount, "JitMinOptsLvNumcount", DEFAULT_MIN_OPTS_LV_NUM_COUNT)
CONFIG_UNSIGNED(JitMinOptsLvRefCount, "JitMinOptsLvRefcount", DEFAULT_MIN_OPTS_LV_REF_COUNT)
CONFIG_BOOL(JitNoCSE, "JitNoCSE", false)
CONFIG_UNSIGNED(JitNoCSE2, "JitNoCSE2", 0)
// Set to non-zero to prevent NOWAY assert testing.
// Overrides COMPlus_JitForceFallback and JIT stress flags.
CONFIG_BOOL(JitNoForceFallback, "JitNoForceFallback", false)

CONFIG_BOOL(JitNoInline, "JitNoInline", false)                 // Disables inlining of all methods
CONFIG_BOOL(JitNoMemoryBarriers, "JitNoMemoryBarriers", false) // If true, don't generate memory barriers
// Disables struct promotion 1 - for all, 2 - for params.
CONFIG_UNSIGNED(JitNoStructPromotion, "JitNoStructPromotion", 0)
CONFIG_BOOL(JitNoUnroll, "JitNoUnroll", false)
CONFIG_UNSIGNED(JitOrder, "JitOrder", 0)
CONFIG_BOOL(JitQueryCurrentStaticFieldClass, "JitQueryCurrentStaticFieldClass", true)
CONFIG_BOOL(JitReportFastTailCallDecisions, "JitReportFastTailCallDecisions", false)
CONFIG_BOOL(JitPInvokeEnabled, "JITPInvokeEnabled", true)
CONFIG_METHODSET(JitPrintInlinedMethods, "JitPrintInlinedMethods")
CONFIG_METHODSET(JitPrintDevirtualizedMethods, "JitPrintDevirtualizedMethods")
CONFIG_UNSIGNED(JitProfileChecks, "JitProfileChecks", 0) // 1 enable in dumps, 2 assert if issues found
CONFIG_BOOL(JitRequired, "JITRequired", false)
CONFIG_INT(JitStackAllocToLocalSize, "JitStackAllocToLocalSize", DEFAULT_MAX_LOCALLOC_TO_LOCAL_SIZE)
CONFIG_BOOL(JitSkipArrayBoundCheck, "JitSkipArrayBoundCheck", false)
CONFIG_BOOL(JitSlowDebugChecksEnabled, "JitSlowDebugChecksEnabled", true) // Turn on slow debug checks
// On ARM, use this as the maximum function/funclet size for creating
// function fragments (and creating multiple RUNTIME_FUNCTION entries)
CONFIG_UNSIGNED(JitSplitFunctionSize, "JitSplitFunctionSize", 0)
// Perturb order of processing of blocks in SSA; 0 = no stress; 1 =
// use method hash; * = supplied value as random hash
CONFIG_UNSIGNED(JitSsaStress, "JitSsaStress", 0)
CONFIG_UNSIGNED(JitStackChecks, "JitStackChecks", 0)
CONFIG_STRING(JitStdOutFile, "JitStdOutFile") // If set, sends JIT's stdout output to this file.
CONFIG_INT(JitStress, "JitStress", 0)         // Internal Jit stress mode: 0 = no stress, 2 = all stress, other = vary
                                              // stress based on a hash of the method and this value
CONFIG_UNSIGNED(JitStressBBProf, "JitStressBBProf", 0)               // Internal Jit stress mode
CONFIG_UNSIGNED(JitStressBiasedCSE, "JitStressBiasedCSE", 0x101)     // Internal Jit stress mode: decimal bias value
                                                                     // between (0,100) to perform CSE on a candidate.
                                                                     // 100% = All CSEs. 0% = 0 CSE. (> 100) means no
                                                                     // stress.
CONFIG_BOOL(JitStressModeNamesOnly, "JitStressModeNamesOnly", false) // Internal Jit stress: if nonzero, only enable
                                                                     // stress modes listed in JitStressModeNames
CONFIG_UNSIGNED(JitStressRegs, "JitStressRegs", 0)
CONFIG_UNSIGNED(JitVNMapSelLimit, "JitVNMapSelLimit", 0) // If non-zero, assert if # of VNF_MapSelect applications
                                                         // considered reaches this
CONFIG_UNSIGNED(NgenHashDump, "NgenHashDump", UINT_MAX)  // same as JitHashDump, but for ngen
CONFIG_UNSIGNED(NgenOrder, "NgenOrder", 0)
CONFIG_BOOL(RunAltJitCode, "RunAltJitCode", true) // If non-zero, and the compilation succeeds for an AltJit, then
                                                  // use the code. If zero, then we always throw away the generated
                                                  // code and fall back to the default compiler.
CONFIG_BOOL(RunComponentUnitTests, "JitComponentUnitTests", false) // Run JIT component unit tests
CONFIG_BOOL(ShouldInjectFault, "InjectFault", false)
CONFIG_BOOL(StressCOMCall, "StressCOMCall", false)
CONFIG_BOOL(TailcallStress, "TailcallStress", false)

// If true, display each tree before/after morphing
CONFIG_BOOL(TreesBeforeAfterMorph, "JitDumpBeforeAfterMorph", false)

CONFIG_METHODSET(JitBreak, "JitBreak") // Stops in the importer when compiling a specified method
CONFIG_METHODSET(JitDebugBreak, "JitDebugBreak")
CONFIG_METHODSET(JitDisasm, "JitDisasm")                  // Dumps disassembly for specified method
CONFIG_STRING(JitDisasmAssemblies, "JitDisasmAssemblies") // Only show JitDisasm and related info for methods
                                                          // from this semicolon-delimited list of assemblies.
CONFIG_BOOL(JitDisasmWithGC, "JitDisasmWithGC", false)    // Dump interleaved GC Info for any method disassembled.
CONFIG_METHODSET(JitDump, "JitDump")                      // Dumps trees for specified method
CONFIG_METHODSET(JitEHDump, "JitEHDump")                  // Dump the EH table for the method, as reported to the VM
CONFIG_METHODSET(JitExclude, "JitExclude")
CONFIG_METHODSET(JitForceProcedureSplitting, "JitForceProcedureSplitting")
CONFIG_METHODSET(JitGCDump, "JitGCDump")
CONFIG_METHODSET(JitDebugDump, "JitDebugDump")
CONFIG_METHODSET(JitHalt, "JitHalt") // Emits break instruction into jitted code
CONFIG_METHODSET(JitImportBreak, "JitImportBreak")
CONFIG_METHODSET(JitInclude, "JitInclude")
CONFIG_METHODSET(JitLateDisasm, "JitLateDisasm")
CONFIG_METHODSET(JitMinOptsName, "JITMinOptsName")                       // Forces MinOpts for a named function
CONFIG_METHODSET(JitNoProcedureSplitting, "JitNoProcedureSplitting")     // Disallow procedure splitting for specified
                                                                         // methods
CONFIG_METHODSET(JitNoProcedureSplittingEH, "JitNoProcedureSplittingEH") // Disallow procedure splitting for
                                                                         // specified methods if they contain
                                                                         // exception handling
CONFIG_METHODSET(JitStressOnly, "JitStressOnly") // Internal Jit stress mode: stress only the specified method(s)
CONFIG_METHODSET(JitUnwindDump, "JitUnwindDump") // Dump the unwind codes for the method

CONFIG_METHODSET(NgenDisasm, "NgenDisasm") // Same as JitDisasm, but for ngen
CONFIG_METHODSET(NgenDump, "NgenDump")     // Same as JitDump, but for ngen
CONFIG_METHODSET(NgenEHDump, "NgenEHDump") // Dump the EH table for the method, as reported to the VM
CONFIG_METHODSET(NgenGCDump, "NgenGCDump")
CONFIG_METHODSET(NgenDebugDump, "NgenDebugDump")
CONFIG_METHODSET(NgenUnwindDump, "NgenUnwindDump") // Dump the unwind codes for the method

CONFIG_METHODSET(JitDumpFg, "JitDumpFg")        // Dumps Xml/Dot Flowgraph for specified method
CONFIG_STRING(JitDumpFgDir, "JitDumpFgDir")     // Directory for Xml/Dot flowgraph dump(s)
CONFIG_STRING(JitDumpFgFile, "JitDumpFgFile")   // Filename for Xml/Dot flowgraph dump(s) (default: "default")
CONFIG_STRING(JitDumpFgPhase, "JitDumpFgPhase") // Phase-based Xml/Dot flowgraph support. Set to the short name of a
                                                // phase to see the flowgraph after that phase. Leave unset to dump
                                                // after COLD-BLK (determine first cold block) or set to * for all
                                                // phases

// Same as JitDumpFgPhase, but specifies to dump pre-phase, not post-phase.
CONFIG_STRING(JitDumpFgPrePhase, "JitDumpFgPrePhase")
CONFIG_BOOL(JitDumpFgDot, "JitDumpFgDot", true)      // 0 == dump XML format; non-zero == dump DOT format
CONFIG_BOOL(JitDumpFgEH, "JitDumpFgEH", false)       // 0 == no EH regions; non-zero == include EH regions
CONFIG_BOOL(JitDumpFgLoops, "JitDumpFgLoops", false) // 0 == no loop regions; non-zero == include loop regions

CONFIG_BOOL(JitDumpFgConstrained, "JitDumpFgConstrained", true) // 0 == don't constrain to mostly linear layout;
                                                                // non-zero == force mostly lexical block
                                                                // linear layout
CONFIG_BOOL(JitDumpFgBlockID, "JitDumpFgBlockID", false)        // 0 == display block with bbNum; 1 == display with both
                                                                // bbNum and bbID

CONFIG_STRING(JitLateDisasmTo, "JitLateDisasmTo")
CONFIG_STRING(JitRange, "JitRange")
CONFIG_STRING(JitStressModeNames, "JitStressModeNames")       // Internal Jit stress mode: stress using the given set of
                                                              // stress mode names, e.g. STRESS_REGS, STRESS_TAILCALL
CONFIG_STRING(JitStressModeNamesNot, "JitStressModeNamesNot") // Internal Jit stress mode: do NOT stress using the
                                                              // given set of stress mode names, e.g. STRESS_REGS,
                                                              // STRESS_TAILCALL
CONFIG_STRING(JitStressRange, "JitStressRange")               // Internal Jit stress mode

CONFIG_METHODSET(NgenDumpFg, "NgenDumpFg")      // Ngen Xml/Dot flowgraph dump support
CONFIG_STRING(NgenDumpFgDir, "NgenDumpFgDir")   // Ngen Xml/Dot flowgraph dump support
CONFIG_STRING(NgenDumpFgFile, "NgenDumpFgFile") // Ngen Xml/Dot flowgraph dump support

CONFIG_BOOL(EnableIncompleteISAClass, "EnableIncompleteISAClass", false) // Enable testing not-yet-implemented
                                                                         // intrinsic classes

CONFIG_BOOL(JitELTHookEnabled, "JitELTHookEnabled", false) // If true, emit Enter/Leave/TailCall callbacks

#endif // DEBUG

#if FEATURE_LOOP_ALIGN
CONFIG_BOOL(JitAlignLoops, "JitAlignLoops", true) // If set, align inner loops
#else
CONFIG_BOOL(JitAlignLoops, "JitAlignLoops", false)
#endif

#ifdef FEATURE_ENABLE_NO_RANGE_CHECKS
CONFIG_BOOL(JitNoRangeChks, "JitNoRngChks", false) // If true, don't generate range checks
#endif

// AltJitAssertOnNYI should be 0 on targets where JIT is under development or bring up stage,
// so as to facilitate fallback to main JIT on hitting a NYI.
#if defined(TARGET_ARM64) || defined(TARGET_X86)
CONFIG_UNSIGNED(AltJitAssertOnNYI, "AltJitAssertOnNYI", 0)
#else
CONFIG_UNSIGNED(AltJitAssertOnNYI, "AltJitAssertOnNYI", 1)
#endif

CONFIG_BOOL(EnableEHWriteThru, "EnableEHWriteThru", true) // Enable the register allocator to support EH-write thru:
                                                          // partial enregistration of vars exposed on EH boundaries

#if defined(TARGET_AMD64) || defined(TARGET_X86) || defined(TARGET_ARM64)
CONFIG_BOOL(EnableHWIntrinsic, "EnableHWIntrinsic", true)
#endif

#if defined(TARGET_AMD64) || defined(TARGET_X86)
CONFIG_BOOL(EnableSSE, "EnableSSE", true)
CONFIG_BOOL(EnableSSE2, "EnableSSE2", true)
CONFIG_BOOL(EnableSSE3, "EnableSSE3", true)
CONFIG_BOOL(EnableSSSE3, "EnableSSSE3", true)
CONFIG_BOOL(EnableSSE41, "EnableSSE41", true)
CONFIG_BOOL(EnableSSE42, "EnableSSE42", true)
CONFIG_BOOL(EnableAVX, "EnableAVX", true)
CONFIG_BOOL(EnableAVX2, "EnableAVX2", true)
CONFIG_BOOL(EnableAVXVNNI, "EnableAVXVNNI", true)
CONFIG_BOOL(EnableFMA, "EnableFMA", true)
CONFIG_BOOL(EnableAES, "EnableAES", true)
CONFIG_BOOL(EnableBMI1, "EnableBMI1", true)
CONFIG_BOOL(EnableBMI2, "EnableBMI2", true)
CONFIG_BOOL(EnableLZCNT, "EnableLZCNT", true)
CONFIG_BOOL(EnablePCLMULQDQ, "EnablePCLMULQDQ", true)
CONFIG_BOOL(EnablePOPCNT, "EnablePOPCNT", true)
#endif

// clang-format off

#ifdef TARGET_ARM64
CONFIG_BOOL(EnableArm64Aes,          "EnableArm64Aes", true)
CONFIG_BOOL(EnableArm64Atomics,      "EnableArm64Atomics", true)
CONFIG_BOOL(EnableArm64Crc32,        "EnableArm64Crc32", true)
CONFIG_BOOL(EnableArm64Dcpop,        "EnableArm64Dcpop", true)
CONFIG_BOOL(EnableArm64Dp,           "EnableArm64Dp", true)
CONFIG_BOOL(EnableArm64Fcma,         "EnableArm64Fcma", true)
CONFIG_BOOL(EnableArm64Fp,           "EnableArm64Fp", true)
CONFIG_BOOL(EnableArm64Fp16,         "EnableArm64Fp16", true)
CONFIG_BOOL(EnableArm64Jscvt,        "EnableArm64Jscvt", true)
CONFIG_BOOL(EnableArm64Lrcpc,        "EnableArm64Lrcpc", true)
CONFIG_BOOL(EnableArm64Pmull,        "EnableArm64Pmull", true)
CONFIG_BOOL(EnableArm64Sha1,         "EnableArm64Sha1", true)
CONFIG_BOOL(EnableArm64Sha256,       "EnableArm64Sha256", true)
CONFIG_BOOL(EnableArm64Sha512,       "EnableArm64Sha512", true)
CONFIG_BOOL(EnableArm64Sha3,         "EnableArm64Sha3", true)
CONFIG_BOOL(EnableArm64AdvSimd,      "EnableArm64AdvSimd", true)
CONFIG_BOOL(EnableArm64AdvSimd_v81,  "EnableArm64AdvSimd_v81", true)
CONFIG_BOOL(EnableArm64AdvSimd_Fp16, "EnableArm64AdvSimd_Fp16", true)
CONFIG_BOOL(EnableArm64Sm3,          "EnableArm64Sm3", true)
CONFIG_BOOL(EnableArm64Sm4,          "EnableArm64Sm4", true)
CONFIG_BOOL(EnableArm64Sve,          "EnableArm64Sve", true)
#endif // defined(TARGET_ARM64)

#ifdef TARGET_ARM
CONFIG_BOOL(JitSoftFP, "JitSoftFP", false)
#endif

// clang-format on

// Default 0, enable the CSE of Constants, including nearby offsets. (only for ARM64)
// If 1, disable all the CSE of Constants
// If 2, enable the CSE of Constants but don't combine with nearby offsets. (only for ARM64)
// If 3, enable the CSE of Constants including nearby offsets. (all platforms)
// If 4, enable the CSE of Constants but don't combine with nearby offsets. (all platforms)
CONFIG_UNSIGNED(JitConstCSE, "JitConstCSE", 0)

#ifdef DEBUG
CONFIG_BOOL(JitEnableNoWayAssert, "JitEnableNoWayAssert", true)
#else
CONFIG_BOOL(JitEnableNoWayAssert, "JitEnableNoWayAssert", false)
#endif

#if defined(TARGET_AMD64) || defined(TARGET_X86)
#define JitMinOptsTrackGCrefs_Default false // Not tracking GC refs in MinOpts is new behavior
#else
#define JitMinOptsTrackGCrefs_Default true
#endif
CONFIG_BOOL(JitMinOptsTrackGCrefs, "JitMinOptsTrackGCrefs", JitMinOptsTrackGCrefs_Default) // Track GC roots

// The following should be wrapped inside "#if MEASURE_MEM_ALLOC / #endif", but
// some files include this one without bringing in the definitions from "jit.h"
// so we don't always know what the "true" value of that flag should be. For now
// we take the easy way out and always include the flag, even in release builds
// (normally MEASURE_MEM_ALLOC is off for release builds but if it's toggled on
// for release in "jit.h" the flag would be missing for some includers).
// TODO-Cleanup: need to make 'MEASURE_MEM_ALLOC' well-defined here at all times.
CONFIG_BOOL(DisplayMemStats, "JitMemStats", false) // Display JIT memory usage statistics

CONFIG_BOOL(JitAggressiveInlining, "JitAggressiveInlining", false) // Aggressive inlining of all methods
CONFIG_INT(JitInlineSIMDMultiplier, "JitInlineSIMDMultiplier", 3)

CONFIG_UNSIGNED(JitMaxLocalsToTrack, "JitMaxLocalsToTrack", 1024)

#ifdef FEATURE_ENABLE_NO_RANGE_CHECKS
CONFIG_BOOL(JitNoRngChks, "JitNoRngChks", false) // If true, don't generate range checks
#endif

#ifdef OPT_CONFIG
CONFIG_BOOL(JitDoAssertionProp, "JitDoAssertionProp", true) // Perform assertion propagation optimization
CONFIG_BOOL(JitDoCopyProp, "JitDoCopyProp", true)         // Perform copy propagation on variables that appear redundant
CONFIG_BOOL(JitDoEarlyProp, "JitDoEarlyProp", true)       // Perform Early Value Propagation
CONFIG_BOOL(JitDoLoopHoisting, "JitDoLoopHoisting", true) // Perform loop hoisting on loop invariant values
CONFIG_BOOL(JitDoLoopInversion, "JitDoLoopInversion", true)             // Perform loop inversion on "for/while" loops
CONFIG_BOOL(JitDoRangeAnalysis, "JitDoRangeAnalysis", true)             // Perform range check analysis
CONFIG_BOOL(JitDoRedundantBranchOpts, "JitDoRedundantBranchOpts", true) // Perform redundant branch optimizations
CONFIG_BOOL(JitDoSsa, "JitDoSsa", true) // Perform Static Single Assignment (SSA) numbering on the variables
CONFIG_BOOL(JitDoValueNumber, "JitDoValueNumber", true) // Perform value numbering on method expressions

CONFIG_METHODSET(JitOptRepeat, "JitOptRepeat")             // Runs optimizer multiple times on the method
CONFIG_UNSIGNED(JitOptRepeatCount, "JitOptRepeatCount", 2) // Number of times to repeat opts when repeating
#endif                                                     // defined(OPT_CONFIG)

// Max # of MapSelect's considered for a particular top-level invocation.
CONFIG_INT(JitVNMapSelBudget, "JitVNMapSelBudget", DefaultVNMapSelectBudget)

CONFIG_BOOL(TailCallLoopOpt, "TailCallLoopOpt", true) // Convert recursive tail calls to loops
CONFIG_METHODSET(AltJit, "AltJit")         // Enables AltJit and selectively limits it to the specified methods.
CONFIG_METHODSET(AltJitNgen, "AltJitNgen") // Enables AltJit for NGEN and selectively limits it
                                           // to the specified methods.

CONFIG_STRING(AltJitExcludeAssemblies, "AltJitExcludeAssemblies") // Do not use AltJit on this
                                                                  // semicolon-delimited list of assemblies.

CONFIG_BOOL(JitMeasureIR, "JitMeasureIR", false) // If set, measure the IR size after some phases and report it in
                                                 // the time log.

CONFIG_STRING(JitFuncInfoFile, "JitFuncInfoLogFile") // If set, gather JIT function info and write to this file.
CONFIG_STRING(JitTimeLogCsv, "JitTimeLogCsv")        // If set, gather JIT throughput data and write to a CSV file. This
                                                     // mode must be used in internal retail builds.
CONFIG_BOOL(TailCallOpt, "TailCallOpt", true)
// If set, allow fast tail calls; otherwise allow only helper-based
// calls for explicit tail calls.
CONFIG_BOOL(FastTailCalls, "FastTailCalls", true)

// Set to true to measure noway_assert usage. Only valid if MEASURE_NOWAY is defined.
CONFIG_BOOL(JitMeasureNowayAssert, "JitMeasureNowayAssert", false)
// Set to file to write noway_assert usage to a file (if not
// set: stdout). Only valid if MEASURE_NOWAY is defined.
CONFIG_STRING(JitMeasureNowayAssertFile, "JitMeasureNowayAssertFile")
#ifdef DEBUG
// Make extra queries to somewhat future-proof SuperPmi method contexts.
CONFIG_BOOL(EnableExtraSuperPmiQueries, "EnableExtraSuperPmiQueries", false)
#endif

#if defined(DEBUG) || defined(INLINE_DATA)
CONFIG_UNSIGNED(JitInlineDumpData, "JitInlineDumpData", 0)
CONFIG_UNSIGNED(JitInlineDumpXml, "JitInlineDumpXml", 0) // 1 = full xml (+ failures in DEBUG)
                                                         // 2 = only methods with inlines (+ failures in DEBUG)
                                                         // 3 = only methods with inlines, no failures
CONFIG_STRING(JitInlineDumpXmlFile, "JitInlineDumpXmlFile")
CONFIG_BOOL(JitInlinePolicyDumpXml, "JitInlinePolicyDumpXml", false)
CONFIG_INT(JitInlineLimit, "JitInlineLimit", -1)
CONFIG_BOOL(JitInlinePolicyDiscretionary, "JitInlinePolicyDiscretionary", false)
CONFIG_BOOL(JitInlinePolicyFull, "JitInlinePolicyFull", false)
CONFIG_BOOL(JitInlinePolicySize, "JitInlinePolicySize", false)
CONFIG_INT(JitInlinePolicyRandom, "JitInlinePolicyRandom", 0) // nonzero enables; value is the external random
                                                              // seed
CONFIG_BOOL(JitInlinePolicyReplay, "JitInlinePolicyReplay", false)
CONFIG_STRING(JitNoInlineRange, "JitNoInlineRange")
CONFIG_STRING(JitInlineReplayFile, "JitInlineReplayFile")
#endif // defined(DEBUG) || defined(INLINE_DATA)

// Extended version of DefaultPolicy that includes a more precise IL scan,
// relies on PGO if it exists and generally is more aggressive.
CONFIG_BOOL(JitExtDefaultPolicy, "JitExtDefaultPolicy", true)
CONFIG_UNSIGNED(JitExtDefaultPolicyMaxIL, "JitExtDefaultPolicyMaxIL", 0x80)
CONFIG_UNSIGNED(JitExtDefaultPolicyMaxILProf, "JitExtDefaultPolicyMaxILProf", 0x400)
CONFIG_UNSIGNED(JitExtDefaultPolicyMaxBB, "JitExtDefaultPolicyMaxBB", 7)

// Inliner uses the following formula for PGO-driven decisions:
//
//    BM = BM * ((1.0 - ProfTrust) + ProfWeight * ProfScale)
//
// Where BM is a benefit multiplier composed from various observations (e.g. "const arg makes a branch foldable").
// If a profile data can be trusted for 100% we can safely just give up on inlining anything inside cold blocks
// (except the cases where inlining in cold blocks improves type info/escape analysis for the whole caller).
// For now, it's only applied for dynamic PGO.
CONFIG_DOUBLE(JitExtDefaultPolicyProfTrust, "JitExtDefaultPolicyProfTrust", 7)
CONFIG_DOUBLE(JitExtDefaultPolicyProfScale, "JitExtDefaultPolicyProfScale", 42)

CONFIG_BOOL(JitInlinePolicyModel, "JitInlinePolicyModel", false)
CONFIG_BOOL(JitInlinePolicyProfile, "JitInlinePolicyProfile", false)
CONFIG_DOUBLE(JitInlinePolicyProfileThreshold, "JitInlinePolicyProfileThreshold", 40)

CONFIG_BOOL(JitObjectStackAllocation, "JitObjectStackAllocation", false)

CONFIG_BOOL(JitEECallTimingInfo, "JitEECallTimingInfo", false)

#ifdef DEBUG
CONFIG_BOOL(JitEnableFinallyCloning, "JitEnableFinallyCloning", true)
CONFIG_BOOL(JitEnableRemoveEmptyTry, "JitEnableRemoveEmptyTry", true)
#endif

// Overall master enable for Guarded Devirtualization.
CONFIG_BOOL(JitEnableGuardedDevirtualization, "JitEnableGuardedDevirtualization", true)

// Various policies for GuardedDevirtualization
CONFIG_UNSIGNED(JitGuardedDevirtualizationChainLikelihood, "JitGuardedDevirtualizationChainLikelihood", 75)
CONFIG_UNSIGNED(JitGuardedDevirtualizationChainStatements, "JitGuardedDevirtualizationChainStatements", 4)
#ifdef DEBUG
CONFIG_STRING(JitGuardedDevirtualizationRange, "JitGuardedDevirtualizationRange")
CONFIG_INT(JitRandomGuardedDevirtualization, "JitRandomGuardedDevirtualization", 0)
#endif

// Enable insertion of patchpoints into Tier0 methods with loops.
CONFIG_BOOL(TC_OnStackReplacement, "TC_OnStackReplacement", false)
// Initial patchpoint counter value used by jitted code
CONFIG_INT(TC_OnStackReplacement_InitialCounter, "TC_OnStackReplacement_InitialCounter", 1000)

// Profile instrumentation options
CONFIG_BOOL(JitMinimalJitProfiling, "JitMinimalJitProfiling", true)
CONFIG_BOOL(JitMinimalPrejitProfiling, "JitMinimalPrejitProfiling", false)
CONFIG_BOOL(JitClassProfiling, "JitClassProfiling", true)
CONFIG_BOOL(JitEdgeProfiling, "JitEdgeProfiling", true)
CONFIG_BOOL(JitCollect64BitCounts, "JitCollect64BitCounts", false) // Collect counts as 64-bit values.

// Profile consumption options
CONFIG_BOOL(JitDisablePgo, "JitDisablePgo", false) // Ignore pgo data for all methods
#ifdef DEBUG
CONFIG_STRING(JitEnablePgoRange, "JitEnablePgoRange") // Enable pgo data for only some methods
CONFIG_BOOL(JitCrossCheckDevirtualizationAndPGO, "JitCrossCheckDevirtualizationAndPGO", false)
CONFIG_BOOL(JitNoteFailedExactDevirtualization, "JitNoteFailedExactDevirtualization", false)
#endif

CONFIG_BOOL(JitEnregStructLocals, "JitEnregStructLocals", false) // Allow to enregister locals with struct type.

// Expand Call targets early (in the global morph phase)
CONFIG_UNSIGNED(JitExpandCallsEarly, "JitExpandCallsEarly", 1)

#ifdef DEBUG
// JitFunctionFile: Name of a file that contains a list of functions. If the currently compiled function is in the
// file, certain other JIT config variables will be active. If the currently compiled function is not in the file,
// the specific JIT config variables will not be active.
//
// Functions are approximately in the format output by JitFunctionTrace, e.g.:
//
// System.CLRConfig:GetBoolValue(ref,byref):bool (MethodHash=3c54d35e)
//   -- use the MethodHash, not the function name
//
// System.CLRConfig:GetBoolValue(ref,byref):bool
//   -- use just the name
//
// Lines with leading ";" "#" or "//" are ignored.
//
// If this is unset, then the JIT config values have their normal behavior.
//
CONFIG_STRING(JitFunctionFile, "JitFunctionFile")

#ifdef TARGET_ARM64
// JitSaveFpLrWithCalleeSavedRegisters:
//    0: use default frame type decision
//    1: disable frames that save FP/LR registers with the callee-saved registers (at the top of the frame)
//    2: force all frames to use the frame types that save FP/LR registers with the callee-saved registers (at the top
//    of the frame)
CONFIG_UNSIGNED(JitSaveFpLrWithCalleeSavedRegisters, "JitSaveFpLrWithCalleeSavedRegisters", 0)
#endif
#endif // DEBUG

#undef CONFIG_BOOL
#undef CONFIG_INT
#undef CONFIG_UNSIGNED
#undef CONFIG_DOUBLE
#undef CONFIG_STRING
#undef CONFIG_METHODSET
