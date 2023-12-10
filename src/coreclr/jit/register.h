// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifndef REGDEF
#error Must define REGDEF macro before including this file
#endif

#ifndef REGALIAS
#define REGALIAS(...)
#endif

#ifdef TARGET_XARCH

#ifdef TARGET_AMD64
#define XMMNUM(x) (16 + (x))
#else
#define XMMNUM(x) (8 + (x))
#endif
#define RMASK(x) (1u << (x))
#define XMMMASK(x) (1u << XMMNUM(x))

// clang-format off
#ifdef TARGET_X86

REGDEF(EAX, 0, RMASK(0), "eax")
REGDEF(ECX, 1, RMASK(1), "ecx")
REGDEF(EDX, 2, RMASK(2), "edx")
REGDEF(EBX, 3, RMASK(3), "ebx")
REGDEF(ESP, 4, RMASK(4), "esp")
REGDEF(EBP, 5, RMASK(5), "ebp")
REGDEF(ESI, 6, RMASK(6), "esi")
REGDEF(EDI, 7, RMASK(7), "edi")

REGALIAS(RAX, EAX)
REGALIAS(RCX, ECX)
REGALIAS(RDX, EDX)
REGALIAS(RBX, EBX)
REGALIAS(RSP, ESP)
REGALIAS(RBP, EBP)
REGALIAS(RSI, ESI)
REGALIAS(RDI, EDI)

#else // TARGET_AMD64

REGDEF(RAX,  0, RMASK( 0), "rax")
REGDEF(RCX,  1, RMASK( 1), "rcx")
REGDEF(RDX,  2, RMASK( 2), "rdx")
REGDEF(RBX,  3, RMASK( 3), "rbx")
REGDEF(RSP,  4, RMASK( 4), "rsp")
REGDEF(RBP,  5, RMASK( 5), "rbp")
REGDEF(RSI,  6, RMASK( 6), "rsi")
REGDEF(RDI,  7, RMASK( 7), "rdi")
REGDEF(R8,   8, RMASK( 8), "r8")
REGDEF(R9,   9, RMASK( 9), "r9")
REGDEF(R10, 10, RMASK(10), "r10")
REGDEF(R11, 11, RMASK(11), "r11")
REGDEF(R12, 12, RMASK(12), "r12")
REGDEF(R13, 13, RMASK(13), "r13")
REGDEF(R14, 14, RMASK(14), "r14")
REGDEF(R15, 15, RMASK(15), "r15")

REGALIAS(EAX, RAX)
REGALIAS(ECX, RCX)
REGALIAS(EDX, RDX)
REGALIAS(EBX, RBX)
REGALIAS(ESP, RSP)
REGALIAS(EBP, RBP)
REGALIAS(ESI, RSI)
REGALIAS(EDI, RDI)

#endif // TARGET_AMD64

REGDEF(XMM0, XMMNUM(0), XMMMASK(0), "xmm0")
REGDEF(XMM1, XMMNUM(1), XMMMASK(1), "xmm1")
REGDEF(XMM2, XMMNUM(2), XMMMASK(2), "xmm2")
REGDEF(XMM3, XMMNUM(3), XMMMASK(3), "xmm3")
REGDEF(XMM4, XMMNUM(4), XMMMASK(4), "xmm4")
REGDEF(XMM5, XMMNUM(5), XMMMASK(5), "xmm5")
REGDEF(XMM6, XMMNUM(6), XMMMASK(6), "xmm6")
REGDEF(XMM7, XMMNUM(7), XMMMASK(7), "xmm7")

#ifdef TARGET_AMD64
REGDEF(XMM8,  XMMNUM(8),  XMMMASK(8),  "xmm8")
REGDEF(XMM9,  XMMNUM(9),  XMMMASK(9),  "xmm9")
REGDEF(XMM10, XMMNUM(10), XMMMASK(10), "xmm10")
REGDEF(XMM11, XMMNUM(11), XMMMASK(11), "xmm11")
REGDEF(XMM12, XMMNUM(12), XMMMASK(12), "xmm12")
REGDEF(XMM13, XMMNUM(13), XMMMASK(13), "xmm13")
REGDEF(XMM14, XMMNUM(14), XMMMASK(14), "xmm14")
REGDEF(XMM15, XMMNUM(15), XMMMASK(15), "xmm15")
#endif // TARGET_AMD64

#ifdef TARGET_X86
REGDEF(STK, XMMNUM(8), 0, "STK")
#else
REGDEF(STK, XMMNUM(16), 0, "STK")
#endif

// clang-format on
#elif defined(TARGET_ARM)
#include "registerarm.h"
#elif defined(TARGET_ARM64)
#include "registerarm64.h"
#else
#error Unsupported or unset target architecture
#endif

#undef REGDEF
#undef REGALIAS
#undef XMMMASK
