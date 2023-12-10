// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifndef REGDEF
#error Must define REGDEF macro before including this file
#endif

#ifndef REGALIAS
#define REGALIAS(...)
#endif

#define RMASK(x) (1ull << (x))
#define VBASE(x) (32 + (x))
#define VMASK(x) (1ull << VBASE(x))
// The registers with values 64 (NBASE) and above are not real register numbers
#define NBASE(x) (64 + (x))

// clang-format off
REGDEF(R0,   0, RMASK( 0), "x0" , "w0")
REGDEF(R1,   1, RMASK( 1), "x1" , "w1")
REGDEF(R2,   2, RMASK( 2), "x2" , "w2")
REGDEF(R3,   3, RMASK( 3), "x3" , "w3")
REGDEF(R4,   4, RMASK( 4), "x4" , "w4")
REGDEF(R5,   5, RMASK( 5), "x5" , "w5")
REGDEF(R6,   6, RMASK( 6), "x6" , "w6")
REGDEF(R7,   7, RMASK( 7), "x7" , "w7")
REGDEF(R8,   8, RMASK( 8), "x8" , "w8")
REGDEF(R9,   9, RMASK( 9), "x9" , "w9")
REGDEF(R10, 10, RMASK(10), "x10", "w10")
REGDEF(R11, 11, RMASK(11), "x11", "w11")
REGDEF(R12, 12, RMASK(12), "x12", "w12")
REGDEF(R13, 13, RMASK(13), "x13", "w13")
REGDEF(R14, 14, RMASK(14), "x14", "w14")
REGDEF(R15, 15, RMASK(15), "x15", "w15")
REGDEF(IP0, 16, RMASK(16), "xip0","wip0")
REGDEF(IP1, 17, RMASK(17), "xip1","wip1")
REGDEF(PR,  18, RMASK(18), "xpr", "wpr")
REGDEF(R19, 19, RMASK(19), "x19", "w19")
REGDEF(R20, 20, RMASK(20), "x20", "w20")
REGDEF(R21, 21, RMASK(21), "x21", "w21")
REGDEF(R22, 22, RMASK(22), "x22", "w22")
REGDEF(R23, 23, RMASK(23), "x23", "w23")
REGDEF(R24, 24, RMASK(24), "x24", "w24")
REGDEF(R25, 25, RMASK(25), "x25", "w25")
REGDEF(R26, 26, RMASK(26), "x26", "w26")
REGDEF(R27, 27, RMASK(27), "x27", "w27")
REGDEF(R28, 28, RMASK(28), "x28", "w28")
REGDEF(FP,  29, RMASK(29), "fp" , "w29")
REGDEF(LR,  30, RMASK(30), "lr" , "w30")
REGDEF(ZR,  31, RMASK(31), "xzr", "wzr")

REGALIAS(R16, IP0)
REGALIAS(R17, IP1)
REGALIAS(R18, PR)
REGALIAS(R29, FP)
REGALIAS(R30, LR)

REGDEF(V0,  VBASE( 0), VMASK(0),  "d0",  "s0")
REGDEF(V1,  VBASE( 1), VMASK(1),  "d1",  "s1")
REGDEF(V2,  VBASE( 2), VMASK(2),  "d2",  "s2")
REGDEF(V3,  VBASE( 3), VMASK(3),  "d3",  "s3")
REGDEF(V4,  VBASE( 4), VMASK(4),  "d4",  "s4")
REGDEF(V5,  VBASE( 5), VMASK(5),  "d5",  "s5")
REGDEF(V6,  VBASE( 6), VMASK(6),  "d6",  "s6")
REGDEF(V7,  VBASE( 7), VMASK(7),  "d7",  "s7")
REGDEF(V8,  VBASE( 8), VMASK(8),  "d8",  "s8")
REGDEF(V9,  VBASE( 9), VMASK(9),  "d9",  "s9")
REGDEF(V10, VBASE(10), VMASK(10), "d10", "s10")
REGDEF(V11, VBASE(11), VMASK(11), "d11", "s11")
REGDEF(V12, VBASE(12), VMASK(12), "d12", "s12")
REGDEF(V13, VBASE(13), VMASK(13), "d13", "s13")
REGDEF(V14, VBASE(14), VMASK(14), "d14", "s14")
REGDEF(V15, VBASE(15), VMASK(15), "d15", "s15")
REGDEF(V16, VBASE(16), VMASK(16), "d16", "s16")
REGDEF(V17, VBASE(17), VMASK(17), "d17", "s17")
REGDEF(V18, VBASE(18), VMASK(18), "d18", "s18")
REGDEF(V19, VBASE(19), VMASK(19), "d19", "s19")
REGDEF(V20, VBASE(20), VMASK(20), "d20", "s20")
REGDEF(V21, VBASE(21), VMASK(21), "d21", "s21")
REGDEF(V22, VBASE(22), VMASK(22), "d22", "s22")
REGDEF(V23, VBASE(23), VMASK(23), "d23", "s23")
REGDEF(V24, VBASE(24), VMASK(24), "d24", "s24")
REGDEF(V25, VBASE(25), VMASK(25), "d25", "s25")
REGDEF(V26, VBASE(26), VMASK(26), "d26", "s26")
REGDEF(V27, VBASE(27), VMASK(27), "d27", "s27")
REGDEF(V28, VBASE(28), VMASK(28), "d28", "s28")
REGDEF(V29, VBASE(29), VMASK(29), "d29", "s29")
REGDEF(V30, VBASE(30), VMASK(30), "d30", "s30")
REGDEF(V31, VBASE(31), VMASK(31), "d31", "s31")

REGDEF(SP,  NBASE(0), 0, "sp",  "wsp?")
REGDEF(STK, NBASE(1), 0, "STK", "STK")
// clang-format on

#undef RMASK
#undef VMASK
#undef VBASE
#undef NBASE
#undef REGDEF
#undef REGALIAS
