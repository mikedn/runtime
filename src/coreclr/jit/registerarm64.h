// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifndef REGDEF
#error Must define REGDEF macro before including this file
#endif

#ifndef REGALIAS
#define REGALIAS(...)
#endif

REGDEF(R0, "x0", "w0")
REGDEF(R1, "x1", "w1")
REGDEF(R2, "x2", "w2")
REGDEF(R3, "x3", "w3")
REGDEF(R4, "x4", "w4")
REGDEF(R5, "x5", "w5")
REGDEF(R6, "x6", "w6")
REGDEF(R7, "x7", "w7")
REGDEF(R8, "x8", "w8")
REGDEF(R9, "x9", "w9")
REGDEF(R10, "x10", "w10")
REGDEF(R11, "x11", "w11")
REGDEF(R12, "x12", "w12")
REGDEF(R13, "x13", "w13")
REGDEF(R14, "x14", "w14")
REGDEF(R15, "x15", "w15")
REGDEF(IP0, "xip0", "wip0")
REGDEF(IP1, "xip1", "wip1")
REGDEF(PR, "xpr", "wpr")
REGDEF(R19, "x19", "w19")
REGDEF(R20, "x20", "w20")
REGDEF(R21, "x21", "w21")
REGDEF(R22, "x22", "w22")
REGDEF(R23, "x23", "w23")
REGDEF(R24, "x24", "w24")
REGDEF(R25, "x25", "w25")
REGDEF(R26, "x26", "w26")
REGDEF(R27, "x27", "w27")
REGDEF(R28, "x28", "w28")
REGDEF(FP, "fp", "w29")
REGDEF(LR, "lr", "w30")
REGDEF(ZR, "xzr", "wzr")

REGALIAS(R16, IP0)
REGALIAS(R17, IP1)
REGALIAS(R18, PR)
REGALIAS(R29, FP)
REGALIAS(R30, LR)
REGALIAS(INT_FIRST, R0)
REGALIAS(INT_LAST, ZR)

REGDEF(V0, "d0", "s0")
REGDEF(V1, "d1", "s1")
REGDEF(V2, "d2", "s2")
REGDEF(V3, "d3", "s3")
REGDEF(V4, "d4", "s4")
REGDEF(V5, "d5", "s5")
REGDEF(V6, "d6", "s6")
REGDEF(V7, "d7", "s7")
REGDEF(V8, "d8", "s8")
REGDEF(V9, "d9", "s9")
REGDEF(V10, "d10", "s10")
REGDEF(V11, "d11", "s11")
REGDEF(V12, "d12", "s12")
REGDEF(V13, "d13", "s13")
REGDEF(V14, "d14", "s14")
REGDEF(V15, "d15", "s15")
REGDEF(V16, "d16", "s16")
REGDEF(V17, "d17", "s17")
REGDEF(V18, "d18", "s18")
REGDEF(V19, "d19", "s19")
REGDEF(V20, "d20", "s20")
REGDEF(V21, "d21", "s21")
REGDEF(V22, "d22", "s22")
REGDEF(V23, "d23", "s23")
REGDEF(V24, "d24", "s24")
REGDEF(V25, "d25", "s25")
REGDEF(V26, "d26", "s26")
REGDEF(V27, "d27", "s27")
REGDEF(V28, "d28", "s28")
REGDEF(V29, "d29", "s29")
REGDEF(V30, "d30", "s30")
REGDEF(V31, "d31", "s31")

REGALIAS(FP_FIRST, V0)
REGALIAS(FP_LAST, V31)

#undef RMASK
#undef VMASK
#undef VBASE
#undef NBASE
#undef REGDEF
#undef REGALIAS
