// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifndef REGDEF
#error Must define REGDEF macro before including this file
#endif

#ifndef REGALIAS
#define REGALIAS(...)
#endif

REGDEF(R0, "r0")
REGDEF(R1, "r1")
REGDEF(R2, "r2")
REGDEF(R3, "r3")
REGDEF(R4, "r4")
REGDEF(R5, "r5")
REGDEF(R6, "r6")
REGDEF(R7, "r7")
REGDEF(R8, "r8")
REGDEF(R9, "r9")
REGDEF(R10, "r10")
REGDEF(R11, "fp")
REGDEF(R12, "r12")
REGDEF(SP, "sp")
REGDEF(LR, "lr")
REGDEF(PC, "pc")

REGALIAS(FP, R11)
REGALIAS(R13, SP)
REGALIAS(R14, LR)
REGALIAS(R15, PC)
REGALIAS(INT_FIRST, R0)
REGALIAS(INT_LAST, LR)

REGDEF(F0, "f0")
REGDEF(F1, "f1")
REGDEF(F2, "f2")
REGDEF(F3, "f3")
REGDEF(F4, "f4")
REGDEF(F5, "f5")
REGDEF(F6, "f6")
REGDEF(F7, "f7")
REGDEF(F8, "f8")
REGDEF(F9, "f9")
REGDEF(F10, "f10")
REGDEF(F11, "f11")
REGDEF(F12, "f12")
REGDEF(F13, "f13")
REGDEF(F14, "f14")
REGDEF(F15, "f15")
REGDEF(F16, "f16")
REGDEF(F17, "f17")
REGDEF(F18, "f18")
REGDEF(F19, "f19")
REGDEF(F20, "f20")
REGDEF(F21, "f21")
REGDEF(F22, "f22")
REGDEF(F23, "f23")
REGDEF(F24, "f24")
REGDEF(F25, "f25")
REGDEF(F26, "f26")
REGDEF(F27, "f27")
REGDEF(F28, "f28")
REGDEF(F29, "f29")
REGDEF(F30, "f30")
REGDEF(F31, "f31")

REGALIAS(FP_FIRST, F0)
REGALIAS(FP_LAST, F31)

#undef REGDEF
#undef REGALIAS
