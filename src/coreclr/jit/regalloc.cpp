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

// The code to set the regState for each arg is outlined for shared use
// by linear scan. (It is not shared for System V AMD64 platform.)
regNumber Compiler::raUpdateRegStateForArg(RegState* regState, LclVarDsc* argDsc)
{
    regNumber inArgReg  = argDsc->GetArgReg();
    regMaskTP inArgMask = genRegMask(inArgReg);

    if (regState->rsIsFloat)
    {
        noway_assert(inArgMask & RBM_FLTARG_REGS);
    }
    else //  regState is for the integer registers
    {
#ifdef TARGET_ARM64
        // This might be the fixed return buffer register argument (on ARM64)
        // We check and allow inArgReg to be REG_ARG_RET_BUFF
        if (inArgReg == REG_ARG_RET_BUFF)
        {
            // We should have a TYP_BYREF or TYP_I_IMPL arg and not a TYP_STRUCT arg
            noway_assert(argDsc->TypeIs(TYP_BYREF, TYP_I_IMPL));
            // We should have recorded the variable number for the return buffer arg
            noway_assert(info.compRetBuffArg != BAD_VAR_NUM);
        }
        else
#endif
        {
            noway_assert(inArgMask & RBM_ARG_REGS);
        }
    }

    regState->rsCalleeRegArgMaskLiveIn |= inArgMask;

#ifdef TARGET_ARM
    if (argDsc->TypeIs(TYP_DOUBLE))
    {
        if (info.compIsVarArgs || opts.compUseSoftFP)
        {
            assert((inArgReg == REG_R0) || (inArgReg == REG_R2));
            assert(!regState->rsIsFloat);
        }
        else
        {
            assert(regState->rsIsFloat);
            assert(emitter::isDoubleReg(inArgReg));
        }
        regState->rsCalleeRegArgMaskLiveIn |= genRegMask((regNumber)(inArgReg + 1));
    }
    else if (argDsc->TypeIs(TYP_LONG))
    {
        assert((inArgReg == REG_R0) || (inArgReg == REG_R2));
        assert(!regState->rsIsFloat);
        regState->rsCalleeRegArgMaskLiveIn |= genRegMask((regNumber)(inArgReg + 1));
    }
#endif // TARGET_ARM

#if FEATURE_MULTIREG_ARGS
    if (varTypeIsStruct(argDsc->GetType()))
    {
        if (argDsc->lvIsHfaRegArg())
        {
            assert(regState->rsIsFloat);
            unsigned regCount = argDsc->GetLayout()->GetHfaRegCount();

            for (unsigned i = 1; i < regCount; i++)
            {
                assert(inArgReg + i <= LAST_FP_ARGREG);
                regState->rsCalleeRegArgMaskLiveIn |= genRegMask(static_cast<regNumber>(inArgReg + i));
            }
        }
        else
        {
            assert(!regState->rsIsFloat);
            unsigned cSlots = argDsc->lvSize() / TARGET_POINTER_SIZE;
            for (unsigned i = 1; i < cSlots; i++)
            {
                regNumber nextArgReg = (regNumber)(inArgReg + i);
                if (nextArgReg > REG_ARG_LAST)
                {
                    break;
                }
                regState->rsCalleeRegArgMaskLiveIn |= genRegMask(nextArgReg);
            }
        }
    }
#endif // FEATURE_MULTIREG_ARGS

    return inArgReg;
}

/****************************************************************************/
/* Returns true when we must create an EBP frame
   This is used to force most managed methods to have EBP based frames
   which allows the ETW kernel stackwalker to walk the stacks of managed code
   this allows the kernel to perform light weight profiling
 */
bool Compiler::rpMustCreateEBPFrame(INDEBUG(const char** wbReason))
{
    bool result = false;
#ifdef DEBUG
    const char* reason = nullptr;
#endif

#if ETW_EBP_FRAMED
    if (!result && opts.OptimizationDisabled())
    {
        INDEBUG(reason = "Debug Code");
        result = true;
    }
    if (!result && (info.compMethodInfo->ILCodeSize > DEFAULT_MAX_INLINE_SIZE))
    {
        INDEBUG(reason = "IL Code Size");
        result = true;
    }
    if (!result && (fgBBcount > 3))
    {
        INDEBUG(reason = "BasicBlock Count");
        result = true;
    }
    if (!result && fgHasLoops)
    {
        INDEBUG(reason = "Method has Loops");
        result = true;
    }
    if (!result && (optCallCount >= 2))
    {
        INDEBUG(reason = "Call Count");
        result = true;
    }
    if (!result && (optIndirectCallCount >= 1))
    {
        INDEBUG(reason = "Indirect Call");
        result = true;
    }
#endif // ETW_EBP_FRAMED

    // VM wants to identify the containing frame of an InlinedCallFrame always
    // via the frame register never the stack register so we need a frame.
    if (!result && (optNativeCallCount != 0))
    {
        INDEBUG(reason = "Uses PInvoke");
        result = true;
    }

#ifdef TARGET_ARM64
    // TODO-ARM64-NYI: This is temporary: force a frame pointer-based frame until genFnProlog can handle non-frame
    // pointer frames.
    if (!result)
    {
        INDEBUG(reason = "Temporary ARM64 force frame pointer");
        result = true;
    }
#endif // TARGET_ARM64

#ifdef DEBUG
    if ((result == true) && (wbReason != nullptr))
    {
        *wbReason = reason;
    }
#endif

    return result;
}

void Compiler::raMarkStkVars()
{
    for (unsigned lclNum = 0; lclNum < lvaCount; lclNum++)
    {
        LclVarDsc* lcl = lvaGetDesc(lclNum);

        // X86 varargs methods must not contain direct references to parameters
        // other than 'this', the arglist parameter (which is not a GC pointer)
        // and the struct return buffer parameter, if present. We cannot report
        // any other parameters to the GC becaue they do not have correct frame
        // offsets.
        if (lvaIsX86VarargsStackParam(lclNum))
        {
            assert((lcl->lvRefCnt() == 0) && !lcl->lvRegister);

            lcl->lvOnFrame  = false;
            lcl->lvMustInit = false;

            goto NOT_STK;
        }

        if (lcl->IsDependentPromotedField(this))
        {
            noway_assert(!lcl->lvRegister);

            lcl->lvOnFrame = true;

            goto ON_STK;
        }

        // Fully enregistered variables don't need any frame space.
        if (lcl->lvRegister)
        {
            goto NOT_STK;
        }

        if (lcl->lvRefCnt() != 0)
        {
            if (lcl->lvOnFrame)
            {
                goto ON_STK;
            }
            else
            {
                goto NOT_STK;
            }
        }

        // Unreferenced locals will get a frame location if they're address exposed.
        // TODO-MIKE-Review: Why? Probably because AX is sometimes used simply to
        // block optimizations and require frame allocation. Sounds like "implicitly
        // referenced" should be used instead.

        assert(!opts.compDbgCode);
#if FEATURE_FIXED_OUT_ARGS
        // lvaOutgoingArgSpaceVar is implicitly referenced.
        assert(lclNum != lvaOutgoingArgSpaceVar);
#endif

        if (lcl->IsAddressExposed())
        {
            lcl->lvOnFrame = true;

            goto ON_STK;
        }
        else
        {
            lcl->lvOnFrame  = false;
            lcl->lvMustInit = false;

            goto NOT_STK;
        }

    ON_STK:
#if FEATURE_FIXED_OUT_ARGS
        noway_assert((lclNum == lvaOutgoingArgSpaceVar) || lvaLclSize(lclNum) != 0);
#else
        noway_assert(lvaLclSize(lclNum) != 0);
#endif

    NOT_STK:;
        lcl->lvFramePointerBased = codeGen->isFramePointerUsed();

#if DOUBLE_ALIGN
        if (codeGen->doDoubleAlign())
        {
            noway_assert(!codeGen->isFramePointerUsed());

            if (lcl->IsParam() && !lcl->IsRegParam())
            {
                lcl->lvFramePointerBased = true;
            }
        }
#endif

        // It must be in a register, on frame, or have zero references.
        noway_assert(lcl->lvIsInReg() || lcl->lvOnFrame || (lcl->lvRefCnt() == 0));
        // We can't have both lvRegister and lvOnFrame
        noway_assert(!lcl->lvRegister || !lcl->lvOnFrame);
    }
}
