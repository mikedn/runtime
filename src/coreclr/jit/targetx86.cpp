// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*****************************************************************************/

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#if defined(TARGET_X86)

#include "target.h"

const char* Target::g_tgtCPUName = "x86";

// clang-format off
const regNumber intArgRegs [] {REG_ECX, REG_EDX};
const regMaskTP intArgMasks[] {RBM_ECX, RBM_EDX};

const regNumber longShiftHelperArgRegs[] { REG_EAX, REG_EDX, REG_ECX };
const regNumber initPInvokeFrameArgRegs[] { REG_PINVOKE_FRAME };
// clang-format on

const regMaskTP regMasks[]{
#define REGDEF(name, rnum, mask, sname) mask,
#include "register.h"
};

#ifdef DEBUG
static bool IsXmmReg(regNumber reg)
{
    return (reg >= REG_XMM0) && (reg <= REG_XMM7);
}

static const char* XmmRegName(regNumber reg, emitAttr size)
{
    static const char* const xmmNames[]{
#define REGDEF(name, rnum, mask, sname) "x" sname,
#include "register.h"
    };
    static const char* const ymmNames[]{
#define REGDEF(name, rnum, mask, sname) "y" sname,
#include "register.h"
    };

    if (!IsXmmReg(reg))
    {
        return "???";
    }

    return size == EA_32BYTE ? ymmNames[reg] : xmmNames[reg];
}

const char* RegName(regNumber reg, emitAttr attr)
{
    if (IsXmmReg(reg))
    {
        return XmmRegName(reg, attr);
    }

    static char          rb[2][128];
    static unsigned char rbc = 0;

    const char* rn = getRegName(reg);

    assert(strlen(rn) >= 3);

    switch (EA_SIZE(attr))
    {
        case EA_2BYTE:
            rn++;
            break;

        case EA_1BYTE:
            rbc        = (rbc + 1) % 2;
            rb[rbc][0] = rn[1];
            rb[rbc][1] = 'l';
            strcpy_s(&rb[rbc][2], sizeof(rb[0]) - 2, rn + 3);

            rn = rb[rbc];
            break;

        default:
            break;
    }

#if 0
    // The following is useful if you want register names to be tagged with * or ^ representing gcref or byref, respectively,
    // however it's possibly not interesting most of the time.
    if (EA_IS_GCREF(attr) || EA_IS_BYREF(attr))
    {
        if (rn != rb[rbc])
        {
            rbc = (rbc + 1) % 2;
            strcpy_s(rb[rbc], sizeof(rb[rbc]), rn);
            rn = rb[rbc];
        }

        if (EA_IS_GCREF(attr))
        {
            strcat_s(rb[rbc], sizeof(rb[rbc]), "*");
        }
        else if (EA_IS_BYREF(attr))
        {
            strcat_s(rb[rbc], sizeof(rb[rbc]), "^");
        }
    }
#endif // 0

    return rn;
}
#endif // DEBUG

#endif // TARGET_X86
