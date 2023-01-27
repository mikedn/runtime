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

#endif // TARGET_X86
