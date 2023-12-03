// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_X86
#include "target.h"

const char* Target::CpuName()
{
    return "x86";
}

const regNumber intArgRegs[]{REG_ECX, REG_EDX};
const regMaskTP intArgMasks[]{RBM_ECX, RBM_EDX};

const regNumber longShiftHelperArgRegs[]{REG_EAX, REG_EDX, REG_ECX};
const regNumber initPInvokeFrameArgRegs[]{REG_PINVOKE_FRAME};

#if defined(DEBUG) || defined(LATE_DISASM) || DUMP_GC_TABLES

const char* getRegName(RegNum reg)
{
    static const char* const names[]{
#define REGDEF(name, rnum, mask, sname) sname,
#include "register.h"
        "NA", "???"};
    static_assert_no_msg(REG_NA == _countof(names) - 2);

    return names[Min<size_t>(reg, _countof(names) - 1)];
}

#endif

#ifdef DEBUG

const char* RegName(RegNum reg, emitAttr attr)
{
    if (reg <= REG_EDI)
    {
        if ((attr == EA_1BYTE) && (reg <= REG_EBX))
        {
            return &"al\0cl\0dl\0bl"[reg * 3];
        }

        if (attr == EA_2BYTE)
        {
            return getRegName(reg) + 1;
        }
    }
    else if ((attr == EA_32BYTE) && (REG_XMM0 <= reg) && (reg <= REG_XMM7))
    {
        return &"ymm0\0ymm1\0ymm2\0ymm3\0ymm4\0ymm5\0ymm6\0ymm7"[(reg - REG_XMM0) * 5];
    }

    return getRegName(reg);
}

#endif // DEBUG
#endif // TARGET_X86
