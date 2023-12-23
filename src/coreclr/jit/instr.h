// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifndef INSTR_H
#define INSTR_H

#define BAD_CODE 0x0BADC0DE // better not match a real encoding!

enum instruction : unsigned
{
#if defined(TARGET_XARCH)
#define INST0(id, ...) INS_##id,
#define INST1(id, ...) INS_##id,
#define INST2(id, ...) INS_##id,
#define INST3(id, ...) INS_##id,
#define INST4(id, ...) INS_##id,
#define INST5(id, ...) INS_##id,
#include "instrsxarch.h"
    INS_none,
#define INST0(...)
#define INST1(...)
#define INST2(...)
#define INST3(...)
#define INST4(...)
#define INST5(...)
#define INSTA(id, val) INS_##id = INS_##val,
#include "instrsxarch.h"
    INS_BREAKPOINT = INS_int3,

#elif defined(TARGET_ARM)
#define INST1(id, ...) INS_##id,
#define INST2(id, ...) INS_##id,
#define INST3(id, ...) INS_##id,
#define INST4(id, ...) INS_##id,
#define INST5(id, ...) INS_##id,
#define INST6(id, ...) INS_##id,
#define INST8(id, ...) INS_##id,
#define INST9(id, ...) INS_##id,
#include "instrsarm.h"
    INS_lea, // Not a real instruction. It is used for load the address of stack locals
    INS_none,
    INS_MULADD     = INS_mla,
    INS_ABS        = INS_vabs,
    INS_SQRT       = INS_vsqrt,
    INS_BREAKPOINT = INS_bkpt,

#elif defined(TARGET_ARM64)
#define INST1(id, ...) INS_##id,
#define INST2(id, ...) INS_##id,
#define INST3(id, ...) INS_##id,
#define INST4(id, ...) INS_##id,
#define INST5(id, ...) INS_##id,
#define INST6(id, ...) INS_##id,
#define INST9(id, ...) INS_##id,
#include "instrsarm64.h"
    INS_lea, // Not a real instruction. It is used for load the address of stack locals
    INS_none,
    INS_MULADD     = INS_madd,
    INS_ABS        = INS_fabs,
    INS_SQRT       = INS_fsqrt,
#ifdef TARGET_UNIX
    INS_BREAKPOINT = INS_brk,
#else
    INS_BREAKPOINT = INS_bkpt,
#endif

#else
#error Unsupported target architecture
#endif
    INS_COUNT = INS_none
};

INDEBUG(const char* insName(instruction ins);)

enum emitJumpKind
{
    EJ_NONE,
    EJ_jmp,
#define CC_DEF(en, ...) EJ_##en,
#include "emitjmps.h"
};

#ifdef TARGET_ARM

enum insFlags : unsigned
{
    INS_FLAGS_NOT_SET   = 0x00,
    INS_FLAGS_SET       = 0x01,
    INS_FLAGS_DONT_CARE = 0x02,
};

enum insOpts : unsigned
{
    INS_OPTS_NONE,
    INS_OPTS_LDST_PRE_DEC,
    INS_OPTS_LDST_POST_INC,

    INS_OPTS_RRX,
    INS_OPTS_LSL,
    INS_OPTS_LSR,
    INS_OPTS_ASR,
    INS_OPTS_ROR
};

INDEBUG(const char* insOptsName(insOpts opt);)

#endif // TARGET_ARM
#ifdef TARGET_ARM64

enum insOpts : unsigned
{
    INS_OPTS_NONE,

    INS_OPTS_PRE_INDEX,
    INS_OPTS_POST_INDEX,

    INS_OPTS_LSL12,

    INS_OPTS_LSL = 4,
    INS_OPTS_LSR,
    INS_OPTS_ASR,
    INS_OPTS_ROR,

    INS_OPTS_UXTB = 8,
    INS_OPTS_UXTH,
    INS_OPTS_UXTW,
    INS_OPTS_UXTX,
    INS_OPTS_SXTB,
    INS_OPTS_SXTH,
    INS_OPTS_SXTW,
    INS_OPTS_SXTX,

    INS_OPTS_8B = 16,
    INS_OPTS_16B,
    INS_OPTS_4H,
    INS_OPTS_8H,
    INS_OPTS_2S,
    INS_OPTS_4S,
    INS_OPTS_1D,
    INS_OPTS_2D,

    INS_OPTS_MSL, // Vector Immediate (shifting ones variant)

    INS_OPTS_S_TO_4BYTE, // Single to INT32
    INS_OPTS_D_TO_4BYTE, // Double to INT32

    INS_OPTS_S_TO_8BYTE, // Single to INT64
    INS_OPTS_D_TO_8BYTE, // Double to INT64

    INS_OPTS_4BYTE_TO_S, // INT32 to Single
    INS_OPTS_4BYTE_TO_D, // INT32 to Double

    INS_OPTS_8BYTE_TO_S, // INT64 to Single
    INS_OPTS_8BYTE_TO_D, // INT64 to Double

    INS_OPTS_S_TO_D, // Single to Double
    INS_OPTS_D_TO_S, // Double to Single

    INS_OPTS_H_TO_S, // Half to Single
    INS_OPTS_H_TO_D, // Half to Double

    INS_OPTS_S_TO_H, // Single to Half
    INS_OPTS_D_TO_H, // Double to Half
};

INDEBUG(const char* insOptsName(insOpts opt);)

enum insCond : unsigned
{
    INS_COND_EQ,
    INS_COND_NE,
    INS_COND_HS,
    INS_COND_LO,

    INS_COND_MI,
    INS_COND_PL,
    INS_COND_VS,
    INS_COND_VC,

    INS_COND_HI,
    INS_COND_LS,
    INS_COND_GE,
    INS_COND_LT,

    INS_COND_GT,
    INS_COND_LE,
};

enum insCflags : unsigned
{
    INS_FLAGS_NONE,
    INS_FLAGS_V,
    INS_FLAGS_C,
    INS_FLAGS_CV,

    INS_FLAGS_Z,
    INS_FLAGS_ZV,
    INS_FLAGS_ZC,
    INS_FLAGS_ZCV,

    INS_FLAGS_N,
    INS_FLAGS_NV,
    INS_FLAGS_NC,
    INS_FLAGS_NCV,

    INS_FLAGS_NZ,
    INS_FLAGS_NZV,
    INS_FLAGS_NZC,
    INS_FLAGS_NZCV,
};

enum insBarrier : unsigned
{
    INS_BARRIER_OSHLD = 1,
    INS_BARRIER_OSHST = 2,
    INS_BARRIER_OSH   = 3,

    INS_BARRIER_NSHLD = 5,
    INS_BARRIER_NSHST = 6,
    INS_BARRIER_NSH   = 7,

    INS_BARRIER_ISHLD = 9,
    INS_BARRIER_ISHST = 10,
    INS_BARRIER_ISH   = 11,

    INS_BARRIER_LD = 13,
    INS_BARRIER_ST = 14,
    INS_BARRIER_SY = 15,
};
#endif // TARGET_ARM64

enum GCtype : unsigned
{
    GCT_NONE,
    GCT_GCREF,
    GCT_BYREF
};

#undef EA_UNKNOWN
enum emitAttr : unsigned
{
    EA_UNKNOWN   = 0x000,
    EA_1BYTE     = 0x001,
    EA_2BYTE     = 0x002,
    EA_4BYTE     = 0x004,
    EA_8BYTE     = 0x008,
    EA_16BYTE    = 0x010,
    EA_32BYTE    = 0x020,
    EA_SIZE_MASK = 0x03F,

#ifdef TARGET_64BIT
    EA_PTRSIZE = EA_8BYTE,
#else
    EA_PTRSIZE     = EA_4BYTE,
#endif

    EA_GCREF_FLG = GCT_GCREF << 7,
    EA_GCREF     = EA_PTRSIZE | EA_GCREF_FLG,
    EA_BYREF_FLG = GCT_BYREF << 7,
    EA_BYREF     = EA_PTRSIZE | EA_BYREF_FLG,

#ifdef TARGET_XARCH
    // TODO-MIKE-Cleanup: These aren't used anymore, remove? In theory, x86 could still use
    // these, as it can put a reloc pretty much anywhere an imm32/disp32 is available.
    // All other targets are far more restrictive in this regard and are better off using
    // specific emitter function overloads.
    EA_DSP_RELOC_FLG = 1 << 9,
    EA_PTR_DSP_RELOC = EA_PTRSIZE | EA_DSP_RELOC_FLG,
    EA_CNS_RELOC_FLG = 1 << 10,
    EA_PTR_CNS_RELOC = EA_PTRSIZE | EA_CNS_RELOC_FLG,
#endif
};

#define EA_ATTR(x) static_cast<emitAttr>(x)
#define EA_SIZE(x) static_cast<emitAttr>((x)&EA_SIZE_MASK)
#define EA_SIZE_IN_BYTES(x) ((x)&EA_SIZE_MASK)
#define EA_GC_TYPE(x) static_cast<GCtype>(((x) >> 7) & 3)
#define EA_IS_GCREF_OR_BYREF(x) (EA_GC_TYPE(x) != GCT_NONE)
#define EA_IS_GCREF(x) (EA_GC_TYPE(x) == GCT_GCREF)
#define EA_IS_BYREF(x) (EA_GC_TYPE(x) == GCT_BYREF)
#ifdef TARGET_XARCH
#define EA_IS_DSP_RELOC(x) (((x)&EA_DSP_RELOC_FLG) != 0)
#define EA_IS_CNS_RELOC(x) (((x)&EA_CNS_RELOC_FLG) != 0)
#define EA_IS_RELOC(x) (EA_IS_DSP_RELOC(x) || EA_IS_CNS_RELOC(x))
#endif

extern const uint16_t emitTypeSizes[TYP_COUNT];
extern const uint16_t emitTypeActSz[TYP_COUNT];

inline emitAttr emitTypeSize(var_types type)
{
    assert(type < _countof(emitTypeSizes));
    assert(emitTypeSizes[type] != EA_UNKNOWN);
    return static_cast<emitAttr>(emitTypeSizes[type]);
}

inline emitAttr emitActualTypeSize(var_types type)
{
    assert(type < _countof(emitTypeActSz));
    assert(emitTypeActSz[type] != EA_UNKNOWN);
    return static_cast<emitAttr>(emitTypeActSz[type]);
}

#ifdef FEATURE_SIMD
constexpr emitAttr emitVecTypeSize(unsigned size)
{
    switch (size)
    {
        case 8:
            return EA_8BYTE;
        case 12:
        case 16:
            return EA_16BYTE;
#ifdef TARGET_XARCH
        case 32:
            return EA_32BYTE;
#endif
        default:
            unreached();
    }
}
#endif

#endif // INSTR_H
