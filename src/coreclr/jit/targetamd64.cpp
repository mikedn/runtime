// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*****************************************************************************/

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#if defined(TARGET_AMD64)

#include "target.h"

const char* Target::g_tgtCPUName = "x64";

// clang-format off
#ifdef UNIX_AMD64_ABI
const regNumber intArgRegs [] = { REG_EDI, REG_ESI, REG_EDX, REG_ECX, REG_R8, REG_R9 };
const regMaskTP intArgMasks[] = { RBM_EDI, RBM_ESI, RBM_EDX, RBM_ECX, RBM_R8, RBM_R9 };
const regNumber fltArgRegs [] = { REG_XMM0, REG_XMM1, REG_XMM2, REG_XMM3, REG_XMM4, REG_XMM5, REG_XMM6, REG_XMM7 };
const regMaskTP fltArgMasks[] = { RBM_XMM0, RBM_XMM1, RBM_XMM2, RBM_XMM3, RBM_XMM4, RBM_XMM5, RBM_XMM6, RBM_XMM7 };
#else // !UNIX_AMD64_ABI
const regNumber intArgRegs [] = { REG_ECX, REG_EDX, REG_R8, REG_R9 };
const regMaskTP intArgMasks[] = { RBM_ECX, RBM_EDX, RBM_R8, RBM_R9 };
const regNumber fltArgRegs [] = { REG_XMM0, REG_XMM1, REG_XMM2, REG_XMM3 };
const regMaskTP fltArgMasks[] = { RBM_XMM0, RBM_XMM1, RBM_XMM2, RBM_XMM3 };
#endif // !UNIX_AMD64_ABI
// clang-format on

const regMaskTP regMasks[]{
#define REGDEF(name, rnum, mask, sname) mask,
#include "register.h"
};

#ifdef DEBUG
static bool IsXmmReg(regNumber reg)
{
    return (reg >= REG_XMM0) && (reg <= REG_XMM15);
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

    char suffix = '\0';

    switch (EA_SIZE(attr))
    {
        case EA_4BYTE:
            if (reg > REG_R15)
            {
                break;
            }

            if (reg > REG_RDI)
            {
                suffix = 'd';
                goto APPEND_SUFFIX;
            }
            rbc        = (rbc + 1) % 2;
            rb[rbc][0] = 'e';
            rb[rbc][1] = rn[1];
            rb[rbc][2] = rn[2];
            rb[rbc][3] = 0;
            rn         = rb[rbc];
            break;

        case EA_2BYTE:
            if (reg > REG_RDI)
            {
                suffix = 'w';
                goto APPEND_SUFFIX;
            }
            rn++;
            break;

        case EA_1BYTE:
            if (reg > REG_RDI)
            {
                suffix = 'b';
            APPEND_SUFFIX:
                rbc        = (rbc + 1) % 2;
                rb[rbc][0] = rn[0];
                rb[rbc][1] = rn[1];
                if (rn[2])
                {
                    assert(rn[3] == 0);
                    rb[rbc][2] = rn[2];
                    rb[rbc][3] = suffix;
                    rb[rbc][4] = 0;
                }
                else
                {
                    rb[rbc][2] = suffix;
                    rb[rbc][3] = 0;
                }
            }
            else
            {
                rbc        = (rbc + 1) % 2;
                rb[rbc][0] = rn[1];
                if (reg < 4)
                {
                    rb[rbc][1] = 'l';
                    rb[rbc][2] = 0;
                }
                else
                {
                    rb[rbc][1] = rn[2];
                    rb[rbc][2] = 'l';
                    rb[rbc][3] = 0;
                }
            }

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

#endif // TARGET_AMD64
