// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_ARM64
#include "target.h"

const char* Target::CpuName()
{
    return "arm64";
}

const regNumber intArgRegs[]{REG_R0, REG_R1, REG_R2, REG_R3, REG_R4, REG_R5, REG_R6, REG_R7};
const regMaskTP intArgMasks[]{RBM_R0, RBM_R1, RBM_R2, RBM_R3, RBM_R4, RBM_R5, RBM_R6, RBM_R7};

const regNumber fltArgRegs[]{REG_V0, REG_V1, REG_V2, REG_V3, REG_V4, REG_V5, REG_V6, REG_V7};
const regMaskTP fltArgMasks[]{RBM_V0, RBM_V1, RBM_V2, RBM_V3, RBM_V4, RBM_V5, RBM_V6, RBM_V7};

#if defined(DEBUG) || defined(LATE_DISASM) || DUMP_GC_TABLES
const char* getRegName(regNumber reg)
{
    static const char* const names[]{
#define REGDEF(name, rnum, mask, xname, wname) xname,
#include "registerarm64.h"
        "NA", "???"};
    static_assert_no_msg(REG_NA == _countof(names) - 2);

    return names[Min<size_t>(reg, _countof(names) - 1)];
}
#endif

#ifdef DEBUG

static bool isVectorRegister(regNumber reg)
{
    return (reg >= REG_FP_FIRST && reg <= REG_FP_LAST);
}

const char* RegName(regNumber reg, emitAttr size)
{
    static const char* const xRegNames[]{
#define REGDEF(name, rnum, mask, xname, wname) xname,
#include "registerarm64.h"
    };

    static const char* const wRegNames[]{
#define REGDEF(name, rnum, mask, xname, wname) wname,
#include "registerarm64.h"
    };

    static const char* const qRegNames[]{"q0",  "q1",  "q2",  "q3",  "q4",  "q5",  "q6",  "q7",  "q8",  "q9",  "q10",
                                         "q11", "q12", "q13", "q14", "q15", "q16", "q17", "q18", "q19", "q20", "q21",
                                         "q22", "q23", "q24", "q25", "q26", "q27", "q28", "q29", "q30", "q31"};
    static const char* const hRegNames[]{"h0",  "h1",  "h2",  "h3",  "h4",  "h5",  "h6",  "h7",  "h8",  "h9",  "h10",
                                         "h11", "h12", "h13", "h14", "h15", "h16", "h17", "h18", "h19", "h20", "h21",
                                         "h22", "h23", "h24", "h25", "h26", "h27", "h28", "h29", "h30", "h31"};
    static const char* const bRegNames[]{"b0",  "b1",  "b2",  "b3",  "b4",  "b5",  "b6",  "b7",  "b8",  "b9",  "b10",
                                         "b11", "b12", "b13", "b14", "b15", "b16", "b17", "b18", "b19", "b20", "b21",
                                         "b22", "b23", "b24", "b25", "b26", "b27", "b28", "b29", "b30", "b31"};

    assert(reg < REG_COUNT);

    const char* rn = nullptr;

    if (size == EA_8BYTE)
    {
        rn = xRegNames[reg];
    }
    else if (size == EA_4BYTE)
    {
        rn = wRegNames[reg];
    }
    else if (isVectorRegister(reg))
    {
        if (size == EA_16BYTE)
        {
            rn = qRegNames[reg - REG_V0];
        }
        else if (size == EA_2BYTE)
        {
            rn = hRegNames[reg - REG_V0];
        }
        else if (size == EA_1BYTE)
        {
            rn = bRegNames[reg - REG_V0];
        }
    }

    assert(rn != nullptr);

    return rn;
}

#endif // DEBUG
#endif // TARGET_ARM64
