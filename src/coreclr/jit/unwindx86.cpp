// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                              UnwindInfo                                   XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#include "codegen.h"

#ifndef TARGET_X86
#error "This should be included only for x86"
#endif // TARGET_X86

#if defined(TARGET_UNIX)
short CodeGen::mapRegNumToDwarfReg(regNumber reg)
{
    short dwarfReg = DWARF_REG_ILLEGAL;

    NYI("CFI codes");

    return dwarfReg;
}
#endif // TARGET_UNIX

void CodeGen::unwindBegProlog()
{
}

void CodeGen::unwindEndProlog()
{
}

void CodeGen::unwindBegEpilog()
{
}

void CodeGen::unwindEndEpilog()
{
}

void CodeGen::unwindPush(regNumber reg)
{
}

void CodeGen::unwindAllocStack(unsigned size)
{
}

void CodeGen::unwindSetFrameReg(regNumber reg, unsigned offset)
{
}

void CodeGen::unwindSaveReg(regNumber reg, unsigned offset)
{
}

// Ask the VM to reserve space for the unwind information
// for the function and all its funclets. Called once, just before asking the VM
// for memory and emitting the generated code. Calls unwindReserveFunc() to handle
// the main function and each of the funclets, in turn.
//
void CodeGen::unwindReserve()
{
#if defined(FEATURE_EH_FUNCLETS)
    assert(!generatingProlog);
    assert(!generatingEpilog);

    assert(compFuncInfoCount > 0);
    for (unsigned funcIdx = 0; funcIdx < compFuncInfoCount; funcIdx++)
    {
        unwindReserveFunc(funGetFunc(funcIdx));
    }
#endif
}

// Report all the unwind information to the VM.
//
// Arguments:
//    pHotCode  - Pointer to the beginning of the memory with the function and funclet hot  code.
//    pColdCode - Pointer to the beginning of the memory with the function and funclet cold code.
//
void CodeGen::unwindEmit(void* pHotCode, void* pColdCode)
{
#if defined(FEATURE_EH_FUNCLETS)
    assert(!generatingProlog);
    assert(!generatingEpilog);

    assert(compFuncInfoCount > 0);
    for (unsigned funcIdx = 0; funcIdx < compFuncInfoCount; funcIdx++)
    {
        unwindEmitFunc(funGetFunc(funcIdx), pHotCode, pColdCode);
    }
#endif // FEATURE_EH_FUNCLETS
}

#if defined(FEATURE_EH_FUNCLETS)
// Reserve the unwind information from the VM for a
// given main function or funclet.
//
// Arguments:
//    func - The main function or funclet to reserve unwind info for.
//
void CodeGen::unwindReserveFunc(FuncInfoDsc* func)
{
    unwindReserveFuncHelper(func, true);

    if (fgFirstColdBlock != nullptr)
    {
        unwindReserveFuncHelper(func, false);
    }
}

// Reserve the unwind information from the VM for a
// given main function or funclet, for either the hot or the cold section.
//
// Arguments:
//    func      - The main function or funclet to reserve unwind info for.
//    isHotCode - 'true' to reserve the hot section, 'false' to reserve the cold section.
//
void CodeGen::unwindReserveFuncHelper(FuncInfoDsc* func, bool isHotCode)
{
    bool isFunclet  = (func->funKind != FUNC_ROOT);
    bool isColdCode = !isHotCode;

    eeReserveUnwindInfo(isFunclet, isColdCode, sizeof(UNWIND_INFO));
}

// Report the unwind information to the VM for a
// given main function or funclet. Reports the hot section, then the cold
// section if necessary.
//
// Arguments:
//    func      - The main function or funclet to reserve unwind info for.
//    pHotCode  - Pointer to the beginning of the memory with the function and funclet hot  code.
//    pColdCode - Pointer to the beginning of the memory with the function and funclet cold code.
//
void CodeGen::unwindEmitFunc(FuncInfoDsc* func, void* pHotCode, void* pColdCode)
{
    // Verify that the JIT enum is in sync with the JIT-EE interface enum
    static_assert_no_msg(FUNC_ROOT == (FuncKind)CORJIT_FUNC_ROOT);
    static_assert_no_msg(FUNC_HANDLER == (FuncKind)CORJIT_FUNC_HANDLER);
    static_assert_no_msg(FUNC_FILTER == (FuncKind)CORJIT_FUNC_FILTER);

    unwindEmitFuncHelper(func, pHotCode, pColdCode, true);

    if (pColdCode != nullptr)
    {
        unwindEmitFuncHelper(func, pHotCode, pColdCode, false);
    }
}

// Report the unwind information to the VM for a
// given main function or funclet, for either the hot or cold section.
//
// Arguments:
//    func      - The main function or funclet to reserve unwind info for.
//    pHotCode  - Pointer to the beginning of the memory with the function and funclet hot  code.
//    pColdCode - Pointer to the beginning of the memory with the function and funclet cold code.
//                Ignored if 'isHotCode' is true.
//    isHotCode - 'true' to report the hot section, 'false' to report the cold section.
//
void CodeGen::unwindEmitFuncHelper(FuncInfoDsc* func, void* pHotCode, void* pColdCode, bool isHotCode)
{
    uint32_t startOffset;
    uint32_t endOffset;

    if (isHotCode)
    {
        emitLocation* startLoc;
        emitLocation* endLoc;

        unwindGetFuncLocations(func, true, &startLoc, &endLoc);

        if (startLoc == nullptr)
        {
            startOffset = 0;
        }
        else
        {
            startOffset = startLoc->CodeOffset(GetEmitter());
        }

        if (endLoc == nullptr)
        {
            endOffset = compiler->info.compNativeCodeSize;
        }
        else
        {
            endOffset = endLoc->CodeOffset(GetEmitter());
        }
    }
    else
    {
        emitLocation* coldStartLoc;
        emitLocation* coldEndLoc;

        assert(fgFirstColdBlock != nullptr);
        assert(func->funKind == FUNC_ROOT); // No splitting of funclets.

        unwindGetFuncLocations(func, false, &coldStartLoc, &coldEndLoc);

        if (coldStartLoc == nullptr)
        {
            startOffset = 0;
        }
        else
        {
            startOffset = coldStartLoc->CodeOffset(GetEmitter());
        }

        if (coldEndLoc == nullptr)
        {
            endOffset = compiler->info.compNativeCodeSize;
        }
        else
        {
            endOffset = coldEndLoc->CodeOffset(GetEmitter());
        }
    }

    // Adjust for cold or hot code:
    // 1. The VM doesn't want the cold code pointer unless this is cold code.
    // 2. The startOffset and endOffset need to be from the base of the hot section for hot code
    //    and from the base of the cold section for cold code

    if (isHotCode)
    {
        assert(endOffset <= compiler->info.compTotalHotCodeSize);
        pColdCode = nullptr;
    }
    else
    {
        assert(startOffset >= compiler->info.compTotalHotCodeSize);
        startOffset -= compiler->info.compTotalHotCodeSize;
        endOffset -= compiler->info.compTotalHotCodeSize;
    }

    UNWIND_INFO unwindInfo;

    unwindInfo.FunctionLength = (uint32_t)(endOffset - startOffset);

    compiler->eeAllocUnwindInfo(pHotCode, pColdCode, startOffset, endOffset, sizeof(UNWIND_INFO), &unwindInfo,
                                static_cast<CorJitFuncKind>(func->funKind));
}
#endif // FEATURE_EH_FUNCLETS
