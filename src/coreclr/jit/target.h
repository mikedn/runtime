// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

// Native Varargs are not supported on Unix (all architectures) and win-arm
// On unix-arm64 the native varargs calling convention is identical to the
// non-varargs one so the JIT allows such calls to be generated. But the
// runtime (ArgIterator in particular) does not support it.
#if (defined(TARGET_WINDOWS) && !defined(TARGET_ARM)) || defined(TARGET_ARM64)
#define FEATURE_VARARG 1
#else
#define FEATURE_VARARG 0
#endif

#if defined(TARGET_X86)
#define TARGET_READABLE_NAME "X86"
#elif defined(TARGET_AMD64)
#define TARGET_READABLE_NAME "AMD64"
#elif defined(TARGET_ARM)
#define TARGET_READABLE_NAME "ARM"
#elif defined(TARGET_ARM64)
#define TARGET_READABLE_NAME "ARM64"
#else
#error Unsupported or unset target architecture
#endif

#if defined(TARGET_ARM)
using RegNumIntType = uint32_t;
using RegSetIntType = uint64_t;
#elif defined(TARGET_ARM64)
using RegNumIntType = uint32_t;
using RegSetIntType = uint64_t;
#elif defined(TARGET_AMD64)
using RegNumIntType = uint32_t;
using RegSetIntType = uint32_t;
#elif defined(TARGET_X86)
using RegNumIntType = uint32_t;
using RegSetIntType = uint32_t;
#else
#error Unsupported target architecture
#endif

// In the following enum declaration, the following REG_XXX are created beyond
// the "real" registers:
//    REG_SP  - (ARM64 only) Used only by CodeGen & Emitter
//    REG_STK - Used to indicate something evaluated onto the stack.
//    REG_NA  - Used to indicate that a register is either not yet assigned or not required.
//
enum RegNum : RegNumIntType
{
#define REGDEF(name, ...) REG_##name,
#include "register.h"
#ifdef TARGET_ARM64
    REG_SP,
#endif
    REG_STK,
    REG_NA,
    REG_FIRST = 0,
#ifdef TARGET_ARM64
    REG_LAST = REG_STK - 2,
#else
    REG_LAST        = REG_STK - 1,
#endif
    REG_COUNT = REG_LAST + 1,
#define REGALIAS(alias, name) REG_##alias = REG_##name,
#include "register.h"
    REG_INT_COUNT = REG_INT_LAST - REG_INT_FIRST + 1,
};

using regNumber   = RegNum;
using RegNumSmall = uint8_t;

using regMaskTP = RegSetIntType;
using RegSet    = RegSetIntType;

static_assert_no_msg(static_cast<RegNum>(static_cast<RegNumSmall>(REG_NA)) == REG_NA);

#define REG_NEXT(reg) static_cast<RegNum>((reg) + 1)
#define REG_PREV(reg) static_cast<RegNum>((reg)-1)

constexpr RegSet GetRegSetBit(RegNum reg)
{
    // ARM64 has the special SP reg, which is only used by the instruction encoder and
    // it's never allocated by register allocator and not included in any register sets
    // (which are limited to 64 bits).
    return (reg >= sizeof(RegSet) * 8) ? 0 : (RegSet(1) << reg);
}

enum RegMask : RegSetIntType
{
    RBM_NONE = 0,
#define REGDEF(name, ...) RBM_##name = GetRegSetBit(REG_##name),
#include "register.h"
#define REGALIAS(alias, name) RBM_##alias = RBM_##name,
#include "register.h"
    RBM_ALL = ~RBM_NONE
};

static_assert_no_msg(REG_FIRST == 0);
static_assert_no_msg(REG_INT_FIRST < REG_INT_LAST);
static_assert_no_msg(REG_FP_FIRST < REG_FP_LAST);

#define LEA_AVAILABLE 1

// The pseudorandom nop insertion is not necessary for current scenarios
// #define PSEUDORANDOM_NOP_INSERTION

#if defined(TARGET_X86)
#include "targetx86.h"
#elif defined(TARGET_AMD64)
#include "targetamd64.h"
#elif defined(TARGET_ARM)
#include "targetarm.h"
#elif defined(TARGET_ARM64)
#include "targetarm64.h"
#else
#error Unsupported or unset target architecture
#endif

// Opportunistic tail call feature converts non-tail prefixed calls into
// tail calls where possible. It requires fast tail calling mechanism for
// performance. Otherwise, we are better off not converting non-tail prefixed
// calls into tail calls.
static_assert_no_msg((FEATURE_TAILCALL_OPT == 0) || (FEATURE_FASTTAILCALL == 1));

class Target
{
public:
    static const char* CpuName();
    static const char* PlatformName();
};

#if defined(DEBUG) || defined(LATE_DISASM) || DUMP_GC_TABLES
const char* getRegName(RegNum reg);
// This is for gcencode.cpp and disasm.cpp that don't use the RegNum type
const char* getRegName(unsigned reg);
#endif

#ifdef DEBUG
enum emitAttr : unsigned;
const char* RegName(RegNum reg, enum emitAttr attr);
void dspRegMask(regMaskTP regMask, size_t minSiz = 0);
void DumpRegSet(regMaskTP regs);
void DumpRegSetDiff(const char* name, regMaskTP from, regMaskTP to);
#endif

inline bool isByteReg(RegNum reg)
{
#ifdef TARGET_X86
    return reg <= REG_EBX;
#else
    return true;
#endif
}

regMaskTP genRegMask(RegNum reg);
#ifdef TARGET_ARM
regMaskTP genRegMaskDouble(RegNum reg);
#endif

// Return true if the register is a valid integer register
inline bool genIsValidIntReg(RegNum reg)
{
    return (REG_INT_FIRST <= reg) && (reg <= REG_INT_LAST);
}

// Return true if the register is a valid floating point register
inline bool genIsValidFloatReg(RegNum reg)
{
    return (REG_FP_FIRST <= reg) && (reg <= REG_FP_LAST);
}

#ifdef TARGET_ARM
// Return true if the register is a valid floating point double register
inline bool genIsValidDoubleReg(RegNum reg)
{
    return genIsValidFloatReg(reg) && (((reg - REG_F0) & 0x1) == 0);
}
#endif

// Returns the full mask of all possible integer registers
// Note this includes the fixed return buffer register on Arm64
inline regMaskTP fullIntArgRegMask()
{
#ifdef TARGET_ARM64
    return RBM_ARG_REGS | RBM_ARG_RET_BUFF;
#else
    return RBM_ARG_REGS;
#endif
}

// Returns true if the register is a valid integer argument register
// Note this method also returns true on Arm64 when 'reg' is the RetBuff register
inline bool isValidIntArgReg(RegNum reg)
{
    return (genRegMask(reg) & fullIntArgRegMask()) != RBM_NONE;
}

// Returns true if the register is a valid floating-point argument register
inline bool isValidFloatArgReg(RegNum reg)
{
#ifdef TARGET_X86
    return false;
#else
    return (FIRST_FP_ARGREG <= reg) && (reg <= LAST_FP_ARGREG);
#endif
}

// Map a register number to a register mask.
regMaskTP genRegMask(RegNum reg);

#ifdef TARGET_ARM
// Map a register number to a floating-point register mask.
inline regMaskTP genRegMaskDouble(RegNum reg)
{
    assert(genIsValidDoubleReg(reg));
    return GetRegSetBit(reg) | GetRegSetBit(REG_NEXT(reg));
}
#endif

inline regMaskTP genRegMask(RegNum reg, var_types type)
{
    return ARM_ONLY(type == TYP_DOUBLE ? genRegMaskDouble(reg) :) genRegMask(reg);
}

// Return the lowest bit that is set in the given register mask.
inline regMaskTP genFindLowestReg(regMaskTP value)
{
    return static_cast<regMaskTP>(genFindLowestBit(value));
}

// Maps a single register mask to a register number.
inline RegNum genRegNumFromMask(regMaskTP mask)
{
    assert(mask != 0); // Must have one bit set, so can't have a mask of zero
    RegNum regNum = static_cast<RegNum>(genLog2(mask));
    assert(genRegMask(regNum) == mask);
    return regNum;
}

#ifdef WINDOWS_AMD64_ABI
// For varargs calls on win-x64 we need to pass floating point register arguments in 2 registers:
// the XMM reg that's normally used to pass a floating point arg and the GPR that's normally used
// to pass an integer argument at the same position.
inline RegNum MapVarargsParamFloatRegToIntReg(RegNum floatReg)
{
    switch (floatReg)
    {
        case REG_XMM0:
            return REG_RCX;
        case REG_XMM1:
            return REG_RDX;
        case REG_XMM2:
            return REG_R8;
        case REG_XMM3:
            return REG_R9;
        default:
            unreached();
    }
}

inline RegNum MapVarargsParamIntRegToFloatReg(RegNum intReg)
{
    switch (intReg)
    {
        case REG_RCX:
            return REG_XMM0;
        case REG_RDX:
            return REG_XMM1;
        case REG_R8:
            return REG_XMM2;
        case REG_R9:
            return REG_XMM3;
        default:
            unreached();
    }
}
#endif // WINDOWS_AMD64_ABI

inline RegNum GetIntArgReg(unsigned index)
{
#ifdef TARGET_ARM64
    if (index == RET_BUFF_ARGNUM)
    {
        return REG_ARG_RET_BUFF;
    }
#endif

    assert(index < _countof(intArgRegs));

    return intArgRegs[index];
}

inline RegNum GetFloatArgReg(unsigned index)
{
#ifndef TARGET_X86
    assert(index < _countof(fltArgRegs));

    return fltArgRegs[index];
#else
    assert(!"no x86 float arg regs\n");
    return REG_NA;
#endif
}

__forceinline RegNum genMapRegArgNumToRegNum(unsigned index, var_types type)
{
    if (varTypeUsesFloatArgReg(type))
    {
        return GetFloatArgReg(index);
    }
    else
    {
        return GetIntArgReg(index);
    }
}

// Map a register argument number ("RegArgNum") to a register mask of the associated register.
// Note that for floating-pointer registers, only the low register for a register pair
// (for a double on ARM) is returned.
inline regMaskTP genMapIntRegArgNumToRegMask(unsigned argNum)
{
    assert(argNum < _countof(intArgMasks));

    return intArgMasks[argNum];
}

inline regMaskTP genMapFloatRegArgNumToRegMask(unsigned argNum)
{
#ifndef TARGET_X86
    assert(argNum < _countof(fltArgMasks));

    return fltArgMasks[argNum];
#else
    assert(!"no x86 float arg regs\n");
    return RBM_NONE;
#endif
}

#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
#if defined(TARGET_AMD64)
inline bool varTypeNeedsPartialCalleeSave(var_types type)
{
    assert(type != TYP_STRUCT);
    return (type == TYP_SIMD32);
}
#elif defined(TARGET_ARM64)
inline bool varTypeNeedsPartialCalleeSave(var_types type)
{
    assert(type != TYP_STRUCT);
    // ARM64 ABI FP Callee save registers only require callee to save lower 8 bytes.
    // For vector types longer than 8 bytes caller is responsible for saving and restoring upper bytes.
    return (type == TYP_SIMD16) || (type == TYP_SIMD12);
}
#else
#error Unsupported or unset target architecture
#endif
#endif // FEATURE_PARTIAL_SIMD_CALLEE_SAVE

// Some sanity checks on some of the register masks
// Stack pointer is never part of RBM_ALLINT
static_assert_no_msg((RBM_ALLINT & RBM_SPBASE) == RBM_NONE);
static_assert_no_msg((RBM_INT_CALLEE_SAVED & RBM_SPBASE) == RBM_NONE);

#if ETW_EBP_FRAMED
// Frame pointer isn't either if we're supporting ETW frame chaining
static_assert_no_msg((RBM_ALLINT & RBM_FPBASE) == RBM_NONE);
static_assert_no_msg((RBM_INT_CALLEE_SAVED & RBM_FPBASE) == RBM_NONE);
#endif

#ifdef TARGET_64BIT

typedef uint64_t target_size_t;
typedef int64_t  target_ssize_t;
#define TARGET_SIGN_BIT (1ULL << 63)
#define TARGET_SIZE_MIN UINT64_MIN
#define TARGET_SIZE_MAX UINT64_MAX
#define TARGET_SSIZE_MIN INT64_MIN
#define TARGET_SSIZE_MAX INT64_MAX

#else // !TARGET_64BIT

typedef uint32_t target_size_t;
typedef int32_t  target_ssize_t;
#define TARGET_SIGN_BIT (1ULL << 31)
#define TARGET_SIZE_MIN UINT32_MIN
#define TARGET_SIZE_MAX UINT32_MAX
#define TARGET_SSIZE_MIN INT32_MIN
#define TARGET_SSIZE_MAX INT32_MAX

#endif // !TARGET_64BIT

static_assert_no_msg(sizeof(target_size_t) == TARGET_POINTER_SIZE);
static_assert_no_msg(sizeof(target_ssize_t) == TARGET_POINTER_SIZE);

#if FEATURE_TAILCALL_OPT
#define FEATURE_TAILCALL_OPT_SHARED_RETURN 1
#else
#define FEATURE_TAILCALL_OPT_SHARED_RETURN 0
#endif

#ifdef TARGET_XARCH
#define FEATURE_LOOP_ALIGN 1
#else
#define FEATURE_LOOP_ALIGN 0
#endif

#if defined(UNIX_AMD64_ABI) || defined(TARGET_ARM64)
#define MULTIREG_HAS_SECOND_GC_RET 1
#else
#define MULTIREG_HAS_SECOND_GC_RET 0
#endif

#ifdef UNIX_AMD64_ABI
#define UNIX_AMD64_ABI_ONLY_ARG(x) , x
#define UNIX_AMD64_ABI_ONLY(x) x
#else
#define UNIX_AMD64_ABI_ONLY_ARG(x)
#define UNIX_AMD64_ABI_ONLY(x)
#endif
