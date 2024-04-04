// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*****************************************************************************/
#ifndef TARGET_H_
#define TARGET_H_

// Native Varargs are not supported on Unix (all architectures) and Windows ARM
#if defined(TARGET_WINDOWS) && !defined(TARGET_ARM)
#define FEATURE_VARARG 1
#else
#define FEATURE_VARARG 0
#endif

/*****************************************************************************/
// The following are human readable names for the target architectures
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

/*****************************************************************************/
// The following are intended to capture only those #defines that cannot be replaced
// with static const members of Target

#if defined(TARGET_ARM)
using IntRegNum  = uint32_t;
using IntRegMask = uint64_t;
#elif defined(TARGET_ARM64)
using IntRegNum  = uint32_t;
using IntRegMask = uint64_t;
#elif defined(TARGET_AMD64)
using IntRegNum  = uint32_t;
using IntRegMask = uint32_t;
#elif defined(TARGET_X86)
using IntRegNum  = uint32_t;
using IntRegMask = uint32_t;
#else
#error Unsupported target architecture
#endif

// Each register list in register.h must declare REG_STK as the last value.
// In the following enum declaration, the following REG_XXX are created beyond
// the "real" registers:
//    ACTUAL_REG_COUNT - The number of physical registers.
//    REG_STK          - Used to indicate something evaluated onto the stack.
//    REG_COUNT        - The number of physical register + REG_STK. This is the count of values that may
//                       be assigned during register allocation.
//    REG_NA           - Used to indicate that a register is either not yet assigned or not required.
//

enum RegNum : IntRegNum
{
#define REGDEF(name, num, mask, ...) REG_##name = num,
#define REGALIAS(alias, name) REG_##alias = REG_##name,
#include "register.h"
    REG_COUNT,
    REG_NA           = REG_COUNT,
    ACTUAL_REG_COUNT = REG_STK // everything but REG_STK (only real regs)
};

enum RegMask : IntRegMask
{
    RBM_NONE = 0,
#define REGDEF(name, num, mask, ...) RBM_##name = mask,
#define REGALIAS(alias, name) RBM_##alias = RBM_##name,
#include "register.h"
};

using regNumber      = RegNum;
using regNumberSmall = uint8_t;
using RegNumSmall    = uint8_t;
using regMaskTP      = IntRegMask;

static_assert_no_msg(static_cast<regNumber>(static_cast<regNumberSmall>(REG_COUNT)) == REG_COUNT);

/*****************************************************************************/

#define LEA_AVAILABLE 1

/*****************************************************************************/

// The pseudorandom nop insertion is not necessary for current scenarios
// #define PSEUDORANDOM_NOP_INSERTION

/*****************************************************************************/

// clang-format off
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

C_ASSERT(REG_FIRST == 0);
C_ASSERT(REG_INT_FIRST < REG_INT_LAST);
C_ASSERT(REG_FP_FIRST  < REG_FP_LAST);

// Opportunistic tail call feature converts non-tail prefixed calls into
// tail calls where possible. It requires fast tail calling mechanism for
// performance. Otherwise, we are better off not converting non-tail prefixed
// calls into tail calls.
C_ASSERT((FEATURE_TAILCALL_OPT == 0) || (FEATURE_FASTTAILCALL == 1));

/*****************************************************************************/

#define BITS_PER_BYTE              8
#define RBM_ALL(type) (varTypeUsesFloatReg(type) ? RBM_ALLFLOAT : RBM_ALLINT)

// clang-format on

class Target
{
public:
    static const char* CpuName();
    static const char* PlatformName();
};

#if defined(DEBUG) || defined(LATE_DISASM) || DUMP_GC_TABLES
const char* getRegName(unsigned reg); // this is for gcencode.cpp and disasm.cpp that don't use
                                      // the regNumber type
const char* getRegName(regNumber reg);
#endif // defined(DEBUG) || defined(LATE_DISASM) || DUMP_GC_TABLES

#ifdef DEBUG
enum emitAttr : unsigned;
const char* RegName(regNumber reg, enum emitAttr attr);
void dspRegMask(regMaskTP regMask, size_t minSiz = 0);
void DumpRegSet(regMaskTP regs);
void DumpRegSetDiff(const char* name, regMaskTP from, regMaskTP to);
#endif

#ifdef TARGET_X86
inline bool isByteReg(regNumber reg)
{
    return (reg <= REG_EBX);
}
#else
inline bool isByteReg(regNumber reg)
{
    return true;
}
#endif

inline regMaskTP genRegMask(regNumber reg);
inline regMaskTP genRegMaskFloat(regNumber reg, var_types type = TYP_DOUBLE);

/*****************************************************************************
 * Return true if the register number is valid
 */
inline bool genIsValidReg(regNumber reg)
{
    /* It's safest to perform an unsigned comparison in case reg is negative */
    return ((unsigned)reg < (unsigned)REG_COUNT);
}

/*****************************************************************************
 * Return true if the register is a valid integer register
 */
inline bool genIsValidIntReg(regNumber reg)
{
    return reg >= REG_INT_FIRST && reg <= REG_INT_LAST;
}

/*****************************************************************************
 * Return true if the register is a valid floating point register
 */
inline bool genIsValidFloatReg(regNumber reg)
{
    return reg >= REG_FP_FIRST && reg <= REG_FP_LAST;
}

#ifdef TARGET_ARM

/*****************************************************************************
 * Return true if the register is a valid floating point double register
 */
inline bool genIsValidDoubleReg(regNumber reg)
{
    return genIsValidFloatReg(reg) && (((reg - REG_FP_FIRST) & 0x1) == 0);
}

#endif // TARGET_ARM

//-------------------------------------------------------------------------------------------
// fullIntArgRegMask:
//     Returns the full mask of all possible integer registers
//     Note this includes the fixed return buffer register on Arm64
//
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
inline bool isValidIntArgReg(regNumber reg)
{
    return (genRegMask(reg) & fullIntArgRegMask()) != 0;
}

// Returns true if the register is a valid floating-point argument register
inline bool isValidFloatArgReg(regNumber reg)
{
    return (reg >= FIRST_FP_ARGREG) && (reg <= LAST_FP_ARGREG);
}

/*****************************************************************************
 *
 *  Can the register hold the argument type?
 */

#ifdef TARGET_ARM
inline bool floatRegCanHoldType(regNumber reg, var_types type)
{
    assert(genIsValidFloatReg(reg));
    if (type == TYP_DOUBLE)
    {
        return ((reg - REG_F0) % 2) == 0;
    }
    else
    {
        // Can be TYP_STRUCT for HFA. It's not clear that's correct; what about
        // HFA of double? We wouldn't be asserting the right alignment, and
        // callers like genRegMaskFloat() wouldn't be generating the right mask.

        assert((type == TYP_FLOAT) || (type == TYP_STRUCT));
        return true;
    }
}
#else
// AMD64: xmm registers can hold any float type
// x86: FP stack can hold any float type
// ARM64: Floating-point/SIMD registers can hold any type.
inline bool floatRegCanHoldType(regNumber reg, var_types type)
{
    return true;
}
#endif

/*****************************************************************************
 *
 *  Map a register number to a register mask.
 */

extern const regMaskTP regMasks[REG_COUNT];

inline regMaskTP genRegMask(regNumber reg)
{
    assert((unsigned)reg < ArrLen(regMasks));
#ifdef TARGET_AMD64
    // shift is faster than a L1 hit on modern x86
    // (L1 latency on sandy bridge is 4 cycles for [base] and 5 for [base + index*c] )
    // the reason this is AMD-only is because the x86 BE will try to get reg masks for REG_STK
    // and the result needs to be zero.
    regMaskTP result = 1 << reg;
    assert(result == regMasks[reg]);
    return result;
#else
    return regMasks[reg];
#endif
}

/*****************************************************************************
 *
 *  Map a register number to a floating-point register mask.
 */

inline regMaskTP genRegMaskFloat(regNumber reg, var_types type /* = TYP_DOUBLE */)
{
#if defined(TARGET_AMD64) || defined(TARGET_ARM64) || defined(TARGET_X86)
    assert(genIsValidFloatReg(reg));
    assert((unsigned)reg < ArrLen(regMasks));
    return regMasks[reg];
#elif defined(TARGET_ARM)
    assert(floatRegCanHoldType(reg, type));
    assert(reg >= REG_F0 && reg <= REG_F31);

    if (type == TYP_DOUBLE)
    {
        return regMasks[reg] | regMasks[reg + 1];
    }
    else
    {
        return regMasks[reg];
    }
#else
#error Unsupported or unset target architecture
#endif
}

//------------------------------------------------------------------------
// genRegMask: Given a register, and its type, generate the appropriate regMask
//
// Arguments:
//    regNum   - the register of interest
//    type     - the type of regNum (i.e. the type it is being used as)
//
// Return Value:
//    This will usually return the same value as genRegMask(regNum), but
//    on architectures where multiple registers are used for certain types
//    (e.g. TYP_DOUBLE on ARM), it will return a regMask that includes
//    all the registers.
//    Registers that are used in pairs, but separately named (e.g. TYP_LONG
//    on ARM) will return just the regMask for the given register.
//
// Assumptions:
//    For registers that are used in pairs, the caller will be handling
//    each member of the pair separately.
//
inline regMaskTP genRegMask(regNumber regNum, var_types type)
{
#ifndef TARGET_ARM
    return genRegMask(regNum);
#else
    regMaskTP regMask = RBM_NONE;

    if (varTypeUsesFloatReg(type))
    {
        regMask = genRegMaskFloat(regNum, type);
    }
    else
    {
        regMask = genRegMask(regNum);
    }
    return regMask;
#endif
}

// If the WINDOWS_AMD64_ABI is defined make sure that TARGET_AMD64 is also defined.
#if defined(WINDOWS_AMD64_ABI)
#if !defined(TARGET_AMD64)
#error When WINDOWS_AMD64_ABI is defined you must define TARGET_AMD64 defined as well.
#endif
#endif

#ifdef WINDOWS_AMD64_ABI
// For varargs calls on win-x64 we need to pass floating point register arguments in 2 registers:
// the XMM reg that's normally used to pass a floating point arg and the GPR that's normally used
// to pass an integer argument at the same position.
inline regNumber MapVarargsParamFloatRegToIntReg(regNumber floatReg)
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

inline regNumber MapVarargsParamIntRegToFloatReg(regNumber intReg)
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

/*****************************************************************************/
// Some sanity checks on some of the register masks
// Stack pointer is never part of RBM_ALLINT
C_ASSERT((RBM_ALLINT & RBM_SPBASE) == RBM_NONE);
C_ASSERT((RBM_INT_CALLEE_SAVED & RBM_SPBASE) == RBM_NONE);

#if ETW_EBP_FRAMED
// Frame pointer isn't either if we're supporting ETW frame chaining
C_ASSERT((RBM_ALLINT & RBM_FPBASE) == RBM_NONE);
C_ASSERT((RBM_INT_CALLEE_SAVED & RBM_FPBASE) == RBM_NONE);
#endif
/*****************************************************************************/

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

C_ASSERT(sizeof(target_size_t) == TARGET_POINTER_SIZE);
C_ASSERT(sizeof(target_ssize_t) == TARGET_POINTER_SIZE);

#endif // TARGET_H_
