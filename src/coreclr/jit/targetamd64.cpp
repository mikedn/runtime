// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_AMD64
#include "target.h"

const char* Target::CpuName()
{
    return "x64";
}

#ifdef UNIX_AMD64_ABI
const regNumber intArgRegs[]{REG_EDI, REG_ESI, REG_EDX, REG_ECX, REG_R8, REG_R9};
const regMaskTP intArgMasks[]{RBM_EDI, RBM_ESI, RBM_EDX, RBM_ECX, RBM_R8, RBM_R9};

const regNumber fltArgRegs[]{REG_XMM0, REG_XMM1, REG_XMM2, REG_XMM3, REG_XMM4, REG_XMM5, REG_XMM6, REG_XMM7};
const regMaskTP fltArgMasks[]{RBM_XMM0, RBM_XMM1, RBM_XMM2, RBM_XMM3, RBM_XMM4, RBM_XMM5, RBM_XMM6, RBM_XMM7};
#else
const regNumber intArgRegs[]{REG_ECX, REG_EDX, REG_R8, REG_R9};
const regMaskTP intArgMasks[]{RBM_ECX, RBM_EDX, RBM_R8, RBM_R9};

const regNumber fltArgRegs[]{REG_XMM0, REG_XMM1, REG_XMM2, REG_XMM3};
const regMaskTP fltArgMasks[]{RBM_XMM0, RBM_XMM1, RBM_XMM2, RBM_XMM3};
#endif

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
    if (reg <= REG_R15)
    {
        if (attr <= 4)
        {
            static const char* const names[][3]{{"al", "ax", "eax"},      {"cl", "cx", "ecx"},
                                                {"dl", "dx", "edx"},      {"bl", "bx", "ebx"},
                                                {"spl", "sp", "esp"},     {"bpl", "bp", "ebp"},
                                                {"sil", "si", "esi"},     {"dil", "di", "edi"},
                                                {"r8b", "r8w", "r8d"},    {"r9b", "r9w", "r9d"},
                                                {"r10b", "r10w", "r10d"}, {"r11b", "r11w", "r11d"},
                                                {"r12b", "r12w", "r12d"}, {"r13b", "r13w", "r13d"},
                                                {"r14b", "r14w", "r14d"}, {"r15b", "r15w", "r15d"}};

            return names[reg][attr / 2];
        }
    }
    else if ((attr == EA_32BYTE) && (REG_XMM0 <= reg) && (reg <= REG_XMM15))
    {
        static const char* const names[]{"ymm0", "ymm1", "ymm2",  "ymm3",  "ymm4",  "ymm5",  "ymm6",  "ymm7",
                                         "ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15"};

        return names[reg - REG_XMM0];
    }

    return getRegName(reg);
}

#endif // DEBUG
#endif // TARGET_AMD64
