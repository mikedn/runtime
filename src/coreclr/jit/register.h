// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#if !defined(REGDEF) && !defined(REGALIAS)
#error Must define REGDEF or REGALIAS before including this file
#endif

#ifndef REGALIAS
#define REGALIAS(...)
#endif

#ifndef REGDEF
#define REGDEF(...)
#endif

#ifdef TARGET_XARCH

#ifdef TARGET_X86

REGDEF(EAX, "eax")
REGDEF(ECX, "ecx")
REGDEF(EDX, "edx")
REGDEF(EBX, "ebx")
REGDEF(ESP, "esp")
REGDEF(EBP, "ebp")
REGDEF(ESI, "esi")
REGDEF(EDI, "edi")

REGALIAS(RAX, EAX)
REGALIAS(RCX, ECX)
REGALIAS(RDX, EDX)
REGALIAS(RBX, EBX)
REGALIAS(RSP, ESP)
REGALIAS(RBP, EBP)
REGALIAS(RSI, ESI)
REGALIAS(RDI, EDI)

#else // TARGET_AMD64

REGDEF(RAX, "rax")
REGDEF(RCX, "rcx")
REGDEF(RDX, "rdx")
REGDEF(RBX, "rbx")
REGDEF(RSP, "rsp")
REGDEF(RBP, "rbp")
REGDEF(RSI, "rsi")
REGDEF(RDI, "rdi")
REGDEF(R8, "r8")
REGDEF(R9, "r9")
REGDEF(R10, "r10")
REGDEF(R11, "r11")
REGDEF(R12, "r12")
REGDEF(R13, "r13")
REGDEF(R14, "r14")
REGDEF(R15, "r15")

REGALIAS(EAX, RAX)
REGALIAS(ECX, RCX)
REGALIAS(EDX, RDX)
REGALIAS(EBX, RBX)
REGALIAS(ESP, RSP)
REGALIAS(EBP, RBP)
REGALIAS(ESI, RSI)
REGALIAS(EDI, RDI)

#endif // TARGET_AMD64

REGDEF(XMM0, "xmm0")
REGDEF(XMM1, "xmm1")
REGDEF(XMM2, "xmm2")
REGDEF(XMM3, "xmm3")
REGDEF(XMM4, "xmm4")
REGDEF(XMM5, "xmm5")
REGDEF(XMM6, "xmm6")
REGDEF(XMM7, "xmm7")

#ifdef TARGET_AMD64
REGDEF(XMM8, "xmm8")
REGDEF(XMM9, "xmm9")
REGDEF(XMM10, "xmm10")
REGDEF(XMM11, "xmm11")
REGDEF(XMM12, "xmm12")
REGDEF(XMM13, "xmm13")
REGDEF(XMM14, "xmm14")
REGDEF(XMM15, "xmm15")
#endif // TARGET_AMD64

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
