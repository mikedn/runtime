// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "codegen.h"

#ifdef TARGET_X86

void CodeGen::unwindBegProlog()
{
    assert(generatingProlog);
}

void CodeGen::unwindBegEpilog()
{
    assert(generatingEpilog);
}

void CodeGen::unwindPush(RegNum reg)
{
}

void CodeGen::unwindAllocStack(unsigned size)
{
}

void CodeGen::unwindSetFrameReg(RegNum reg, unsigned offset)
{
}

void CodeGen::unwindSaveReg(RegNum reg, unsigned offset)
{
}

#ifdef FEATURE_EH_FUNCLETS
void CodeGen::unwindReserve()
{
    assert(!generatingProlog);
    assert(!generatingEpilog);
    assert(compFuncInfoCount > 0);

    for (unsigned i = 0; i < compFuncInfoCount; i++)
    {
        unwindReserveFunc(funGetFunc(i));
    }
}

void CodeGen::unwindEmit()
{
    assert(!generatingProlog);
    assert(!generatingEpilog);
    assert(compFuncInfoCount > 0);

    for (unsigned i = 0; i < compFuncInfoCount; i++)
    {
        unwindEmitFunc(funGetFunc(i), codePtr, coldCodePtr);
    }
}

void CodeGen::unwindReserveFunc(FuncInfoDsc* func)
{
    unwindReserveFuncRegion(func, true);

    if (fgFirstColdBlock != nullptr)
    {
        unwindReserveFuncRegion(func, false);
    }
}

void CodeGen::unwindReserveFuncRegion(FuncInfoDsc* func, bool isHotCode)
{
    eeReserveUnwindInfo(func->funKind != FUNC_ROOT, isHotCode, sizeof(UNWIND_INFO));
}

void CodeGen::unwindEmitFunc(FuncInfoDsc* func, void* hotCode, void* coldCode)
{
    unwindEmitFuncRegion(func, hotCode, coldCode, true);

    if (coldCode != nullptr)
    {
        unwindEmitFuncRegion(func, hotCode, coldCode, false);
    }
}

void CodeGen::unwindEmitFuncRegion(FuncInfoDsc* func, bool isHotCode)
{
    uint32_t startOffset;
    uint32_t endOffset;

    if (isHotCode)
    {
        unwindGetFuncHotRange(func, &startOffset, &endOffset);
    }
    else
    {
        unwindGetFuncColdRange(func, &startOffset, &endOffset);
    }

    UNWIND_INFO unwindInfo;
    unwindInfo.FunctionLength = static_cast<uint32_t>(endOffset - startOffset);

    eeAllocUnwindInfo(func->funKind, isHotCode, startOffset, endOffset, sizeof(UNWIND_INFO), &unwindInfo);
}
#endif // FEATURE_EH_FUNCLETS

#ifdef TARGET_UNIX
int16_t CodeGen::mapRegNumToDwarfReg(RegNum reg)
{
    unreached();
}
#endif // TARGET_UNIX

#endif // TARGET_X86
