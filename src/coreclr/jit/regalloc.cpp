// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                           RegAlloc                                        XX
XX                                                                           XX
XX  Does the register allocation and puts the remaining lclVars on the stack XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif
#include "regalloc.h"

#if DOUBLE_ALIGN
DWORD Compiler::getCanDoubleAlign()
{
#ifdef DEBUG
    if (compStressCompile(STRESS_DBL_ALN, 20))
        return MUST_DOUBLE_ALIGN;

    return JitConfig.JitDoubleAlign();
#else
    return DEFAULT_DOUBLE_ALIGN;
#endif
}

//------------------------------------------------------------------------
// shouldDoubleAlign: Determine whether to double-align the frame
//
// Arguments:
//    refCntStk       - sum of     ref counts for all stack based variables
//    refCntEBP       - sum of     ref counts for EBP enregistered variables
//    refCntWtdEBP    - sum of wtd ref counts for EBP enregistered variables
//    refCntStkParam  - sum of     ref counts for all stack based parameters
//    refCntWtdStkDbl - sum of wtd ref counts for stack based doubles (including structs
//                      with double fields).
//
// Return Value:
//    Returns true if this method estimates that a double-aligned frame would be beneficial
//
// Notes:
//    The impact of a double-aligned frame is computed as follows:
//    - We save a byte of code for each parameter reference (they are frame-pointer relative)
//    - We pay a byte of code for each non-parameter stack reference.
//    - We save the misalignment penalty and possible cache-line crossing penalty.
//      This is estimated as 0 for SMALL_CODE, 16 for FAST_CODE and 4 otherwise.
//    - We pay 7 extra bytes for:
//        MOV EBP,ESP,
//        LEA ESP,[EBP-offset]
//        AND ESP,-8 to double align ESP
//    - We pay one extra memory reference for each variable that could have been enregistered in EBP (refCntWtdEBP).
//
//    If the misalignment penalty is estimated to be less than the bytes used, we don't double align.
//    Otherwise, we compare the weighted ref count of ebp-enregistered variables against double the
//    ref count for double-aligned values.
//
bool Compiler::shouldDoubleAlign(unsigned             refCntStk,
                                 unsigned             refCntEBP,
                                 BasicBlock::weight_t refCntWtdEBP,
                                 unsigned             refCntStkParam,
                                 BasicBlock::weight_t refCntWtdStkDbl)
{
    bool           doDoubleAlign        = false;
    const unsigned DBL_ALIGN_SETUP_SIZE = 7;

    unsigned bytesUsed         = refCntStk + refCntEBP - refCntStkParam + DBL_ALIGN_SETUP_SIZE;
    unsigned misaligned_weight = 4;

    if (compCodeOpt() == SMALL_CODE)
        misaligned_weight = 0;

    if (compCodeOpt() == FAST_CODE)
        misaligned_weight *= 4;

    JITDUMP("\nDouble alignment:\n");
    JITDUMP("  Bytes that could be saved by not using EBP frame: %i\n", bytesUsed);
    JITDUMP("  Sum of weighted ref counts for EBP enregistered variables: %f\n", refCntWtdEBP);
    JITDUMP("  Sum of weighted ref counts for weighted stack based doubles: %f\n", refCntWtdStkDbl);

    if (((BasicBlock::weight_t)bytesUsed) > ((refCntWtdStkDbl * misaligned_weight) / BB_UNITY_WEIGHT))
    {
        JITDUMP("    Predicting not to double-align ESP to save %d bytes of code.\n", bytesUsed);
    }
    else if (refCntWtdEBP > refCntWtdStkDbl * 2)
    {
        // TODO-CQ: On P4 2 Proc XEON's, SciMark.FFT degrades if SciMark.FFT.transform_internal is
        // not double aligned.
        // Here are the numbers that make this not double-aligned.
        //     refCntWtdStkDbl = 0x164
        //     refCntWtdEBP    = 0x1a4
        // We think we do need to change the heuristic to be in favor of double-align.

        JITDUMP("    Predicting not to double-align ESP to allow EBP to be used to enregister variables.\n");
    }
    else
    {
        // OK we passed all of the benefit tests, so we'll predict a double aligned frame.
        JITDUMP("    Predicting to create a double-aligned frame\n");
        doDoubleAlign = true;
    }
    return doDoubleAlign;
}
#endif // DOUBLE_ALIGN

bool Compiler::rpMustCreateEBPFrame()
{
    bool result = false;
    INDEBUG(const char* reason = nullptr);

    if (opts.OptimizationDisabled())
    {
        INDEBUG(reason = "Debug Code");
        result = true;
    }
#ifndef TARGET_AMD64
    else if (opts.jitFlags->IsSet(JitFlags::JIT_FLAG_FRAMED))
    {
        // The VM sets JitFlags::JIT_FLAG_FRAMED for two reasons:
        // (1) the COMPlus_JitFramed variable is set, or
        // (2) the function is marked "noinline".
        // The reason for #2 is that people mark functions noinline to ensure they
        // show up on in a stack walk. But for AMD64, we don't need a frame pointer
        // for the frame to show up in stack walk.
        INDEBUG(reason = "JIT_FLAG_FRAMED");
        result = true;
    }
#endif
#if ETW_EBP_FRAMED
    else if (info.compMethodInfo->ILCodeSize > DEFAULT_MAX_INLINE_SIZE)
    {
        INDEBUG(reason = "IL Code Size");
        result = true;
    }
    else if (fgBBcount > 3)
    {
        INDEBUG(reason = "BasicBlock Count");
        result = true;
    }
    else if (fgHasLoops)
    {
        INDEBUG(reason = "Method has Loops");
        result = true;
    }
    else if (optCallCount >= 2)
    {
        INDEBUG(reason = "Call Count");
        result = true;
    }
    else if (optIndirectCallCount >= 1)
    {
        INDEBUG(reason = "Indirect Call");
        result = true;
    }
#endif
    else if (optNativeCallCount != 0)
    {
        // VM wants to identify the containing frame of an InlinedCallFrame always
        // via the frame register never the stack register so we need a frame.
        INDEBUG(reason = "Uses PInvoke");
        result = true;
    }
#ifdef TARGET_ARM64
    else
    {
        // TODO-ARM64-NYI: This is temporary: force a frame pointer-based frame
        // until genFnProlog can handle non-frame pointer frames.
        INDEBUG(reason = "Temporary ARM64 force frame pointer");
        result = true;
    }
#endif // TARGET_ARM64

#ifdef DEBUG
    if (result)
    {
        JITDUMP("; Decided to create an EBP based frame, reason = '%s'\n", reason);
    }
#endif

    return result;
}
