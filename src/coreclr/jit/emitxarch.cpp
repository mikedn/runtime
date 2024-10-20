// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_XARCH

#include "instr.h"
#include "emit.h"
#include "codegen.h"

static bool IsJccInstruction(instruction ins)
{
    return (INS_FIRST_JCC <= ins) && (ins <= INS_LAST_JCC);
}

#ifdef DEBUG
static bool IsJmpInstruction(instruction ins)
{
    return AMD64_ONLY((ins == INS_rex_jmp) ||)(ins == INS_i_jmp) || (ins == INS_jmp) || (ins == INS_l_jmp);
}
#endif

instruction JumpKindToSetcc(emitJumpKind kind)
{
    assert((EJ_o <= kind) && (kind <= EJ_g));

    static const instruction map[]{
        INS_none, INS_none,
#define CC_DEF(cc, rev) INS_set##cc,
#include "emitjmps.h"
    };

    assert(kind < _countof(map));
    return map[kind];
}

instruction JumpKindToJcc(emitJumpKind kind)
{
    static const instruction map[]{
        INS_nop, INS_jmp,
#define CC_DEF(cc, rev) INS_j##cc,
#include "emitjmps.h"
    };

    assert(kind < _countof(map));
    return map[kind];
}

emitJumpKind ReverseJumpKind(emitJumpKind kind)
{
    static const uint8_t map[]{
        EJ_NONE, EJ_jmp,
#define CC_DEF(en, rev) EJ_##rev,
#include "emitjmps.h"
    };

    assert(kind < _countof(map));
    return static_cast<emitJumpKind>(map[kind]);
}

static bool IsDisp8(ssize_t disp)
{
    return (-128 <= disp) && (disp <= 127);
}

static bool IsImm8(ssize_t imm)
{
#ifdef TARGET_X86
    // When cross compiling for x86 we risk getting garbage in the upper 32 bits of imm,
    // we can ignore that since we know we can't really have 64 bit operations on x86.
    // TODO-MIKE-Review: This may happen on x64 too, but to fix it properly we'd need
    // to know the instruction operand size (i.e. it needs to be 32 bit, not 64 bit).
    imm = static_cast<int32_t>(imm);
#endif
    return (-128 <= imm) && (imm <= 127);
}

#ifdef TARGET_AMD64
static bool IsDisp32(ssize_t disp)
{
    return (INT32_MIN <= disp) && (disp <= INT32_MAX);
}

static bool IsImm32(ssize_t imm)
{
    return (INT32_MIN <= imm) && (imm <= INT32_MAX);
}
#endif

static bool IsShiftCL(instruction ins)
{
    return (INS_FIRST_SHIFT <= ins) && (ins <= INS_LAST_SHIFT_CL);
}

static bool IsShiftImm(instruction ins)
{
    return (INS_LAST_SHIFT_1 < ins) && (ins <= INS_LAST_SHIFT_IMM);
}

static bool IsCmov(instruction ins)
{
    return (INS_FIRST_CMOV <= ins) && (ins <= INS_LAST_CMOV);
}

#ifdef DEBUG
static bool HasImplicitRegPairDest(instruction ins)
{
    return (ins == INS_mulEAX) || (ins == INS_imulEAX) || (ins == INS_div) || (ins == INS_idiv);
}

static bool IsAvxBlendv(instruction ins)
{
    return ins == INS_vblendvps || ins == INS_vblendvpd || ins == INS_vpblendvb;
}

static bool IsSse41Blendv(instruction ins)
{
    return ins == INS_blendvps || ins == INS_blendvpd || ins == INS_pblendvb;
}
#endif

static instruction MapSse41BlendvToAvxBlendv(instruction ins)
{
    assert(IsAvxBlendv(ins) || IsSse41Blendv(ins));

    switch (ins)
    {
        case INS_blendvps:
            return INS_vblendvps;
        case INS_blendvpd:
            return INS_vblendvpd;
        case INS_pblendvb:
            return INS_vpblendvb;
        default:
            return ins;
    }
}

static bool IsSSEOrAVXOrBMIInstruction(instruction ins)
{
    return (ins >= INS_FIRST_SSE_INSTRUCTION) && (ins <= INS_LAST_VEX_INSTRUCTION);
}

static bool IsFMAInstruction(instruction ins)
{
    return (ins >= INS_FIRST_FMA_INSTRUCTION) && (ins <= INS_LAST_FMA_INSTRUCTION);
}

static bool IsAVXVNNIInstruction(instruction ins)
{
    return (ins >= INS_FIRST_AVXVNNI_INSTRUCTION) && (ins <= INS_LAST_AVXVNNI_INSTRUCTION);
}

static bool IsPrefetch(instruction ins)
{
    return (ins == INS_prefetcht0) || (ins == INS_prefetcht1) || (ins == INS_prefetcht2) || (ins == INS_prefetchnta);
}

bool X86Emitter::UseVEXEncoding() const
{
    return useVEXEncodings;
}

enum insFlags : uint32_t
{
    INS_FLAGS_None = 0,

    Reads_OF     = 1 << 0,
    Reads_SF     = 1 << 1,
    Reads_ZF     = 1 << 2,
    Reads_PF     = 1 << 3,
    Reads_CF     = 1 << 4,
    Reads_DF     = 1 << 5,
    Writes_OF    = 1 << 6,
    Writes_SF    = 1 << 7,
    Writes_ZF    = 1 << 8,
    Writes_AF    = 1 << 9,
    Writes_PF    = 1 << 10,
    Writes_CF    = 1 << 11,
    Resets_OF    = 1 << 12,
    Resets_SF    = 1 << 13,
    Resets_AF    = 1 << 14,
    Resets_PF    = 1 << 15,
    Resets_CF    = 1 << 16,
    Undefined_OF = 1 << 17,
    Undefined_SF = 1 << 18,
    Undefined_ZF = 1 << 19,
    Undefined_AF = 1 << 20,
    Undefined_PF = 1 << 21,
    Undefined_CF = 1 << 22,

#ifdef TARGET_AMD64
    INS_Flags_RexW = 1 << 24,
#endif
    INS_Flags_VexDstDstSrc = 1 << 25,
    INS_Flags_VexDstSrcSrc = 1 << 26,
};

static insFlags InsFlags(instruction ins)
{
    enum : uint32_t
    {
        None              = INS_FLAGS_None,
        IncDecFlags       = Writes_OF | Writes_SF | Writes_ZF | Writes_AF | Writes_PF,
        AddSubFlags       = Writes_OF | Writes_SF | Writes_ZF | Writes_AF | Writes_PF | Writes_CF,
        AddSubCarryFlags  = Writes_OF | Writes_SF | Writes_ZF | Writes_AF | Writes_PF | Writes_CF | Reads_CF,
        BitwiseFlags      = Writes_SF | Writes_ZF | Undefined_AF | Writes_PF | Resets_CF | Resets_OF,
        ImulFlags         = Writes_OF | Writes_CF | Undefined_SF | Undefined_ZF | Undefined_AF | Undefined_PF,
        BitTestFlags      = Writes_CF | Undefined_OF | Undefined_SF | Undefined_ZF | Undefined_AF | Undefined_PF,
        BitScanFlags      = Writes_ZF | Undefined_OF | Undefined_SF | Undefined_AF | Undefined_PF | Undefined_CF,
        BzhiFlags         = Resets_OF | Writes_SF | Writes_ZF | Undefined_AF | Undefined_PF | Writes_CF,
        FComFlags         = Writes_PF | Writes_CF | Writes_ZF | Resets_OF | Resets_SF | Resets_AF,
        ZCntFlags         = Writes_ZF | Undefined_OF | Undefined_SF | Undefined_AF | Undefined_PF | Writes_CF,
        PopCntFlags       = Resets_OF | Resets_SF | Writes_ZF | Resets_AF | Resets_PF | Resets_CF,
        Rotate1Flags      = Writes_CF | Writes_OF,
        RotateNFlags      = Writes_CF | Undefined_OF,
        RotateCarry1Flags = Writes_CF | Reads_CF | Writes_OF,
        RotateCarryNFlags = Writes_CF | Reads_CF | Undefined_OF,
        Shift1Flags       = Writes_SF | Writes_ZF | Undefined_AF | Writes_PF | Writes_CF | Writes_OF,
        ShiftNFlags       = Writes_SF | Writes_ZF | Undefined_AF | Writes_PF | Writes_CF | Undefined_OF,
        DivFlags          = Undefined_OF | Undefined_SF | Undefined_ZF | Undefined_AF | Undefined_PF | Undefined_CF,
        CcFlags_o         = Reads_OF,
        CcFlags_no        = Reads_OF,
        CcFlags_b         = Reads_CF,
        CcFlags_ae        = Reads_CF,
        CcFlags_e         = Reads_ZF,
        CcFlags_ne        = Reads_ZF,
        CcFlags_be        = Reads_ZF | Reads_CF,
        CcFlags_a         = Reads_ZF | Reads_CF,
        CcFlags_s         = Reads_SF,
        CcFlags_ns        = Reads_SF,
        CcFlags_p         = Reads_PF,
        CcFlags_np        = Reads_PF,
        CcFlags_l         = Reads_OF | Reads_SF,
        CcFlags_ge        = Reads_OF | Reads_SF,
        CcFlags_le        = Reads_OF | Reads_SF | Reads_ZF,
        CcFlags_g         = Reads_OF | Reads_SF | Reads_ZF,
        DirFlags          = Reads_DF,

#ifdef TARGET_AMD64
        RexW = INS_Flags_RexW,
#else
        RexW = 0,
#endif
        VexDstSrcSrc = INS_Flags_VexDstSrcSrc,
        VexDstDstSrc = INS_Flags_VexDstDstSrc,
    };

    static const uint32_t flags[]{
#define INST0(id, nm, um, mr, flags) flags,
#define INST1(id, nm, um, mr, flags) flags,
#define INST2(id, nm, um, mr, mi, flags) flags,
#define INST3(id, nm, um, mr, mi, rm, flags) flags,
#define INST4(id, nm, um, mr, mi, rm, a4, flags) flags,
#define INST5(id, nm, um, mr, mi, rm, a4, rr, flags) flags,
#include "instrsxarch.h"
    };

    assert(ins < _countof(flags));
    return static_cast<insFlags>(flags[ins]);
}

static bool DoesWriteZeroFlag(instruction ins)
{
    return (InsFlags(ins) & Writes_ZF) != 0;
}

static bool DoesResetOverflowAndCarryFlags(instruction ins)
{
    return (InsFlags(ins) & (Resets_OF | Resets_CF)) == (Resets_OF | Resets_CF);
}

// Checks if the instruction has a "reg, reg/mem, imm" or "reg/mem, reg, imm"
// form for the legacy, VEX, and EVEX encodings.
// That is, the instruction takes two operands, one of which is immediate,
// and it does not need to encode any data in the VEX.vvvv field.
static bool IsSseDstSrcImm(instruction ins)
{
    switch (ins)
    {
        case INS_aeskeygenassist:
        case INS_extractps:
        case INS_pextrb:
        case INS_pextrw:
        case INS_pextrd:
        case INS_pextrq:
        case INS_pshufd:
        case INS_pshufhw:
        case INS_pshuflw:
        case INS_roundpd:
        case INS_roundps:
            return true;
        default:
            return false;
    }
}

// Returns true if the AVX instruction is a binary operator that requires 3 operands.
// When we emit an instruction with only two operands, we will duplicate the destination
// as a source.
// TODO-XArch-Cleanup: This is a temporary solution for now. Eventually this needs to
// be formalized by adding an additional field to instruction table to
// to indicate whether a 3-operand instruction.
static bool IsVexDstDstSrc(instruction ins, bool vexAvailable)
{
    return ((InsFlags(ins) & INS_Flags_VexDstDstSrc) != 0) && vexAvailable;
}

bool X86Emitter::IsVexDstDstSrc(instruction ins) const
{
    return ::IsVexDstDstSrc(ins, UseVEXEncoding());
}

// Returns true if the AVX instruction requires 3 operands that duplicate the source
// register in the vvvv field.
// TODO-XArch-Cleanup: This is a temporary solution for now. Eventually this needs to
// be formalized by adding an additional field to instruction table to
// to indicate whether a 3-operand instruction.
static bool IsVexDstSrcSrc(instruction ins, bool vexAvailable)
{
    return ((InsFlags(ins) & INS_Flags_VexDstSrcSrc) != 0) && vexAvailable;
}

bool X86Emitter::IsVexDstSrcSrc(instruction ins) const
{
    return ::IsVexDstSrcSrc(ins, UseVEXEncoding());
}

#ifdef DEBUG
static bool IsVexTernary(instruction ins, bool vexAvailable)
{
    return IsVexDstDstSrc(ins, vexAvailable) || IsVexDstSrcSrc(ins, vexAvailable);
}

bool X86Emitter::IsVexTernary(instruction ins) const
{
    return ::IsVexTernary(ins, UseVEXEncoding());
}

static bool IsReallyVexTernary(instruction ins, bool vexAvailable)
{
    // TODO-MIKE-Cleanup: The VexDstSrcSrc/DstDstSrc are incorrectly placed on instructions
    // when they're in fact a property of the encoding. movss & co. are not DstSrcSrc in
    // their mem,reg forms. Actually movss isn't DstSrcSrc even in its reg,mem form, only
    // in its reg,reg form.
    return IsVexTernary(ins, vexAvailable) &&
           !((ins == INS_movss) || (ins == INS_movsd) || (ins == INS_movlps) || (ins == INS_movlpd) ||
             (ins == INS_movhps) || (ins == INS_movhpd));
}

bool X86Emitter::IsReallyVexTernary(instruction ins) const
{
    return ::IsReallyVexTernary(ins, UseVEXEncoding());
}
#endif

#if defined(TARGET_X86) || defined(DEBUG)
static bool IsX87LdSt(instruction ins)
{
#ifdef TARGET_X86
    return (ins == INS_fld) || (ins == INS_fstp);
#else
    return false;
#endif
}
#endif

bool X86Emitter::AreFlagsAlwaysModified(instrDesc* id)
{
    instruction ins = id->idIns();

    return !IsShiftCL(ins) && (!IsShiftImm(ins) || id->idIsLargeCns() || (id->idSmallCns() != 0));
}

// Check if some previously emitted instruction set the upper 32 bits of reg to zero.
// Currently only looks back one instruction.
bool X86Emitter::AreUpper32BitsZero(RegNum reg)
{
    instrDesc* id = GetLastInsInCurrentBlock();

    if (id == nullptr)
    {
        return false;
    }

    // This isn't meant to be a comprehensive check. Just look for what
    // seems to be common.
    switch (id->idInsFmt())
    {
        case IF_RWR_CNS:
        case IF_RRW_CNS:
        case IF_RWR_RRD:
        case IF_RRW_RRD:
        case IF_RWR_MRD:
        case IF_RWR_SRD:
        case IF_RWR_ARD:

            // Bail if not writing to the right register
            if (id->idReg1() != reg)
            {
                return false;
            }

            // Bail if movsx, we always have movsx sign extend to 8 bytes
            if (id->idIns() == INS_movsx)
            {
                return false;
            }

#ifdef TARGET_AMD64
            if (id->idIns() == INS_movsxd)
            {
                return false;
            }
#endif

            // movzx always zeroes the upper 32 bits.
            if (id->idIns() == INS_movzx)
            {
                return true;
            }

            // Else rely on operation size.
            return (id->idOpSize() == EA_4BYTE);

        default:
            break;
    }

    return false;
}

// Checks if the previous instruction set the SZ, and optionally OC, flags to the same
// values as if there were a compare to 0
// Currently only looks back one instruction.
bool X86Emitter::AreFlagsSetToZeroCmp(RegNum reg, emitAttr opSize, genTreeOps treeOps)
{
    assert(reg != REG_NA);

    instrDesc* id = GetLastInsInCurrentBlock();

    if (id == nullptr)
    {
        return false;
    }

    // make sure op1 is a reg
    switch (id->idInsFmt())
    {
        case IF_RWR_CNS:
        case IF_RRW_CNS:
        case IF_RWR_RRD:
        case IF_RRW_RRD:
        case IF_RWR_MRD:
        case IF_RWR_SRD:
        case IF_RRW_SRD:
        case IF_RWR_ARD:
        case IF_RRW_ARD:
        case IF_RWR:
        case IF_RRD:
        case IF_RRW:
            break;
        default:
            return false;
    }

    if (id->idReg1() != reg)
    {
        return false;
    }

    // Certain instruction like and, or and xor modifies exactly same flags
    // as "test" instruction.
    // They reset OF and CF to 0 and modifies SF, ZF and PF.
    if (DoesResetOverflowAndCarryFlags(id->idIns()))
    {
        return id->idOpSize() == opSize;
    }

    if ((treeOps == GT_EQ) || (treeOps == GT_NE))
    {
        if (DoesWriteZeroFlag(id->idIns()) && AreFlagsAlwaysModified(id))
        {
            return id->idOpSize() == opSize;
        }
    }

    return false;
}

static bool TakesVexPrefix(instruction ins, bool vexAvailable)
{
    return vexAvailable && (INS_FIRST_VEX_INSTRUCTION <= ins) && (ins <= INS_LAST_VEX_INSTRUCTION);
}

bool X86Emitter::TakesVexPrefix(instruction ins) const
{
    return ::TakesVexPrefix(ins, UseVEXEncoding());
}

constexpr unsigned PrefixesBitOffset = 16;
constexpr unsigned MmmBitOffset      = 16;
constexpr unsigned MmmBitMask        = 7;
constexpr unsigned PpBitOffset       = 19;
constexpr unsigned HasVexBitOffset   = 21;
constexpr unsigned RexBitOffset      = 24;
constexpr unsigned VexBitOffset      = 32;
constexpr unsigned VexLBitOffset     = 34;
constexpr unsigned VexVvvvBitOffset  = 35;

using code_t = X86Emitter::code_t;

static bool IsMap0F(code_t code)
{
    return ((code >> MmmBitOffset) & MmmBitMask) == 1;
}

static bool HasVexPrefix(code_t code)
{
    return ((code >> HasVexBitOffset) & 1) != 0;
}

static bool hasRexPrefix(code_t code)
{
#ifdef TARGET_AMD64
    return ((code >> RexBitOffset) & 0xFF) != 0;
#else
    return false;
#endif
}

static bool HasRexW(uint64_t code)
{
#ifdef TARGET_AMD64
    return ((code >> RexBitOffset) & 8) != 0;
#else
    return false;
#endif
}

#ifdef TARGET_AMD64
static bool TakesRexWPrefix(instruction ins)
{
    return (InsFlags(ins) & INS_Flags_RexW) != 0;
}
#endif

static bool TakesRexWPrefix(instruction ins, emitAttr attr)
{
#ifdef TARGET_X86
    return false;
#else
    return (EA_SIZE(attr) == EA_8BYTE) && TakesRexWPrefix(ins);
#endif
}

static bool IsExtendedReg(RegNum reg)
{
#ifdef TARGET_AMD64
    static_assert_no_msg(REG_R8 == 0x08);
    static_assert_no_msg(REG_XMM8 == 0x18);

    return (reg <= REG_XMM15) && ((reg & 0x08) != 0);
#else
    return false;
#endif
}

static bool IsExtendedByteReg(RegNum reg)
{
#ifdef TARGET_AMD64
    return (REG_RSP <= reg) && (reg <= REG_R15);
#else
    return false;
#endif
}

static bool IsExtendedReg(RegNum reg, emitAttr attr)
{
#ifdef TARGET_AMD64
    return IsExtendedReg(reg) || ((attr == EA_1BYTE) && IsExtendedByteReg(reg));
#else
    return false;
#endif
}

const char* insName(instruction ins)
{
    static const char* const insNames[]{
#define INST0(id, nm, ...) nm,
#define INST1(id, nm, ...) nm,
#define INST2(id, nm, ...) nm,
#define INST3(id, nm, ...) nm,
#define INST4(id, nm, ...) nm,
#define INST5(id, nm, ...) nm,
#include "instrsxarch.h"
    };

    return ins < _countof(insNames) ? insNames[ins] : "???";
}

enum InsUpdateMode
{
    IUM_RD,
    IUM_WR,
    IUM_RW,
};

static InsUpdateMode GetUpdateMode(instruction ins)
{
    static const uint8_t emitInsModeFmtTab[]{
#define INST0(id, nm, um, ...) um,
#define INST1(id, nm, um, ...) um,
#define INST2(id, nm, um, ...) um,
#define INST3(id, nm, um, ...) um,
#define INST4(id, nm, um, ...) um,
#define INST5(id, nm, um, ...) um,
#include "instrsxarch.h"
    };

    assert(ins < _countof(emitInsModeFmtTab));

    return static_cast<InsUpdateMode>(emitInsModeFmtTab[ins]);
}

static insFormat MapFormat(instruction ins, insFormat base)
{
    assert(IF_RRD + IUM_RD == IF_RRD);
    assert(IF_RRD + IUM_WR == IF_RWR);
    assert(IF_RRD + IUM_RW == IF_RRW);

    return static_cast<insFormat>(base + GetUpdateMode(ins));
}

const static uint32_t insCodesRM[]{
#define INST0(...)
#define INST1(...)
#define INST2(...)
#define INST3(id, nm, um, mr, mi, rm, ...) rm,
#define INST4(id, nm, um, mr, mi, rm, ...) rm,
#define INST5(id, nm, um, mr, mi, rm, ...) rm,
#include "instrsxarch.h"
};

// Returns the "REG, RM" encoding of the given instruction.
static size_t GetCodeRM(instruction ins)
{
    assert(ins < _countof(insCodesRM));
    assert(insCodesRM[ins] != BAD_CODE);

    return insCodesRM[ins];
}

const static uint32_t insCodesMI[]{
#define INST0(...)
#define INST1(...)
#define INST2(id, nm, um, mr, mi, ...) mi,
#define INST3(id, nm, um, mr, mi, ...) mi,
#define INST4(id, nm, um, mr, mi, ...) mi,
#define INST5(id, nm, um, mr, mi, ...) mi,
#include "instrsxarch.h"
};

// Returns true iff the give instruction has an "RM, IMM" encoding.
static bool HasCodeMI(instruction ins)
{
    assert(ins < _countof(insCodesMI));

    return insCodesMI[ins] != BAD_CODE;
}

static size_t GetCodeMI(instruction ins)
{
    assert(ins < _countof(insCodesMI));
    assert(insCodesMI[ins] != BAD_CODE);

    return insCodesMI[ins];
}

const static uint32_t insCodesMR[]{
#define INST0(id, nm, um, mr, ...)
#define INST1(id, nm, um, mr, ...) mr,
#define INST2(id, nm, um, mr, ...) mr,
#define INST3(id, nm, um, mr, ...) mr,
#define INST4(id, nm, um, mr, ...) mr,
#define INST5(id, nm, um, mr, ...) mr,
#include "instrsxarch.h"
};

static bool HasCodeMR(instruction ins)
{
    assert(ins < _countof(insCodesMR));

    return insCodesMR[ins] != BAD_CODE;
}

// Returns the "RM, REG" or "RM" encoding of the given instruction.
static size_t GetCodeMR(instruction ins)
{
    assert(ins < _countof(insCodesMR));
    assert(insCodesMR[ins] != BAD_CODE);

    return insCodesMR[ins];
}

static unsigned ScaleEncoding(unsigned scale)
{
    assert((scale == 0) || (scale == 1) || (scale == 2) || (scale == 4) || (scale == 8));

    static constexpr uint8_t scales[]{
        0x00, // 0
        0x00, // 1
        0x01, // 2
        0x00, // 3
        0x02, // 4
        0x00, // 5
        0x00, // 6
        0x00, // 7
        0x03, // 8
    };

    return scales[scale];
}

static bool BaseRegRequiresSIB(RegNum base)
{
#ifdef TARGET_AMD64
    return base == REG_ESP || base == REG_R12;
#else
    return base == REG_ESP;
#endif
}

static bool BaseRegRequiresDisp(RegNum base)
{
#ifdef TARGET_AMD64
    return base == REG_EBP || base == REG_R13;
#else
    return base == REG_EBP;
#endif
}

#ifdef TARGET_X86
// When encoding instructions that operate on byte registers on x86
// we have to ensure that we use a low register (EAX, EBX, ECX or EDX).
static bool VerifyEncodable(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2 = REG_NA)
{
    if (attr != EA_1BYTE)
    {
        return true;
    }

    // These instructions destination register is always 32 bit.
    if ((ins != INS_movsx) && (ins != INS_movzx) && (ins != INS_crc32))
    {
        if (!isByteReg(reg1))
        {
            return false;
        }
    }

    return (reg2 == REG_NA) || isByteReg(reg2);
}
#endif // TARGET_X86

static unsigned emitInsSize(code_t code)
{
    assert(!HasVexPrefix(code));

    unsigned size = 2;

    if (uint32_t prefixes = (code >> PrefixesBitOffset) & 0xFF)
    {
        if ((prefixes >> 3) != 0)
        {
            size++;
        }

        size++;

        if ((prefixes & 7) > 1)
        {
            size++;
        }
    }

    return size;
}

unsigned X86Emitter::EncodingSize(instruction ins, emitAttr size, code_t code, bool isRR)
{
    assert(ins != INS_invalid);
    assert(!TakesVexPrefix(ins));
    assert(!HasVexPrefix(code));

    if (ins == INS_crc32)
    {
        return (size == EA_2BYTE) + 4 + 1;
    }

    // Most 16-bit operand instructions will need a 0x66 prefix.
    unsigned sz = (size == EA_2BYTE) && (ins != INS_movzx) && (ins != INS_movsx);

    // For reg,reg forms the RM byte cannot contain an opcode extension so it
    // must be 0, unless this is a 4-byte opcode, which doesn't have a RM byte.

    if (!isRR || ((code & 0xFF00) == 0) || IsSSEOrAVXOrBMIInstruction(ins))
    {
        sz += emitInsSize(code);
    }
    else
    {
        sz += 4 + 1;
    }

    return sz;
}

unsigned X86Emitter::EncodingSizeR(instruction ins, emitAttr size, RegNum reg)
{
    if ((ins == INS_push) || (ins == INS_push_hide) || (ins == INS_pop) || (ins == INS_pop_hide))
    {
        assert(size == EA_PTRSIZE);

        return 1 + IsExtendedReg(reg);
    }

    // TODO-MIKE-Review: Does this really need special casing?
    if ((INS_seto <= ins) && (ins <= INS_setg))
    {
        static_assert_no_msg(INS_seto + 0xF == INS_setg);
        assert(size == EA_1BYTE);

        return 3 + IsExtendedByteReg(reg);
    }

#ifdef TARGET_X86
    if (((ins == INS_inc) || (ins == INS_dec)) && (size != EA_1BYTE))
    {
        return 1;
    }
#endif

    code_t code = GetCodeMR(ins);

    assert(!TakesVexPrefix(ins));
    assert((ins != INS_movsx) && (ins != INS_movzx));
    assert(((code >> PrefixesBitOffset) & 0xFF) == 0);

    return (size == EA_2BYTE) + (hasRexPrefix(code) || IsExtendedReg(reg, size) || TakesRexWPrefix(ins, size)) + 2;
}

unsigned X86Emitter::EncodingSizeRI(instruction ins, emitAttr size, RegNum reg, ssize_t imm)
{
    if (IsShiftImm(ins))
    {
        assert(imm != 1);

        return (size == EA_2BYTE) + AMD64_ONLY((size == EA_8BYTE || IsExtendedReg(reg, size)) +) 2 + 1;
    }

    if (ins == INS_mov)
    {
#ifdef TARGET_AMD64
        if (size == EA_8BYTE)
        {
            return 1 + 1 + 8;
        }

#endif
        // TODO-MIKE-Review: There should be no need to generate 16 bit mov instructions.
        return (size == EA_2BYTE) + AMD64_ONLY(IsExtendedReg(reg, size) +) 1 + 4;
    }

    code_t code = GetCodeMI(ins);

    if (IsSSEOrAVXOrBMIInstruction(ins))
    {
        assert((INS_psrldq <= ins) && (ins <= INS_psrad));
        assert(size != EA_1BYTE);
        assert(!hasRexPrefix(code));
        assert(!TakesRexWPrefix(ins, size));
        assert(IsMap0F(code));

        if (TakesVexPrefix(ins))
        {
            return 2 + IsExtendedReg(reg) + 1 + 1 + 1;
        }

        return IsExtendedReg(reg) + emitInsSize(code) + 1;
    }

    assert(!TakesVexPrefix(ins));
    assert((INS_add <= ins) && (ins <= INS_test));

    unsigned sz = (size == EA_2BYTE)AMD64_ONLY(+(size == EA_8BYTE || IsExtendedReg(reg, size)));

    if (IsImm8(imm) && (ins != INS_test))
    {
        return sz + ((reg == REG_EAX) && (size == EA_1BYTE) ? 1 : 2) + 1;
    }

    sz += (reg == REG_EAX) ? 1 : 2;
#ifdef TARGET_AMD64
    sz += Min(4u, EA_SIZE_IN_BYTES(size));
#else
    sz += EA_SIZE_IN_BYTES(size);
#endif

    return sz;
}

unsigned X86Emitter::EncodingSizeRR(instruction ins, emitAttr size, RegNum reg1, RegNum reg2)
{
    if (TakesVexPrefix(ins))
    {
        code_t code;
        RegNum bReg;

        assert((ins != INS_movd) || (IsFloatReg(reg1) != IsFloatReg(reg2)));

        if ((ins == INS_movd) && IsFloatReg(reg2))
        {
            code = GetCodeMR(ins);
            bReg = reg1;
        }
        else
        {
            code = GetCodeRM(ins);
            bReg = reg2;
        }

        return 2 + (HasRexW(code) || !IsMap0F(code) || TakesRexWPrefix(ins, size) || IsExtendedReg(bReg)) + 1 + 1;
    }

    // TODO-MIKE-Cleanup: This doesn't use the correct code for movd, it
    // just happens to work because because both codes have the same length.
    code_t   code = GetCodeRM(ins);
    unsigned sz   = EncodingSize(ins, size, code, true);
    sz += hasRexPrefix(code) || IsExtendedReg(reg1, size) || IsExtendedReg(reg2, size) ||
          (TakesRexWPrefix(ins, size) && ((ins != INS_xor) || (reg1 != reg2)));
    return sz;
}

unsigned X86Emitter::EncodingSizeRRI(instruction ins, emitAttr size, RegNum reg1, RegNum reg2)
{
    if (TakesVexPrefix(ins))
    {
        RegNum bReg;
        code_t code;

        if (HasCodeMR(ins))
        {
            code = GetCodeMR(ins);
            bReg = reg1;
        }
        else if (HasCodeMI(ins))
        {
            assert((INS_psrldq <= ins) && (ins <= INS_psrad));

            code = GetCodeMI(ins);
            bReg = reg2;
        }
        else
        {
            code = GetCodeRM(ins);
            bReg = reg2;
        }

        return 2 + (HasRexW(code) || !IsMap0F(code) || TakesRexWPrefix(ins, size) || IsExtendedReg(bReg)) + 1 + 1;
    }

    code_t code;

    switch (ins)
    {
        case INS_pextrb:
        case INS_pextrd:
        case INS_pextrq:
        case INS_pextrw_sse41:
        case INS_extractps:
        case INS_vextractf128:
        case INS_vextracti128:
        case INS_shld:
        case INS_shrd:
            code = GetCodeMR(ins);
            break;
        case INS_psrldq:
        case INS_pslldq:
            code = GetCodeMI(ins);
            break;
        default:
            code = GetCodeRM(ins);
            break;
    }

    unsigned sz = EncodingSize(ins, size, code);
    sz += hasRexPrefix(code) || IsExtendedReg(reg1, size) || IsExtendedReg(reg2, size) || TakesRexWPrefix(ins, size);
    return sz;
}

unsigned X86Emitter::EncodingSizeRRR(instruction ins, emitAttr size, RegNum reg3)
{
    assert(TakesVexPrefix(ins));

    code_t code = GetCodeRM(ins);

    return 2 + (HasRexW(code) || !IsMap0F(code) || TakesRexWPrefix(ins, size) || IsExtendedReg(reg3)) + 1 + 1;
}

unsigned X86Emitter::EncodingSizeSV(instrDesc* id, code_t code)
{
    instruction ins  = id->idIns();
    emitAttr    size = id->idOpSize();

    unsigned sz;

    if (TakesVexPrefix(ins))
    {
        sz = 2 + (HasRexW(code) || !IsMap0F(code) || TakesRexWPrefix(ins, size)) + 1 + 1;
    }
    else
    {
        sz = EncodingSize(ins, size, code);
        sz += hasRexPrefix(code) || IsExtendedReg(id->idReg1(), size) || IsExtendedReg(id->idReg2(), size) ||
              TakesRexWPrefix(ins, size);
    }

    bool ebpBased = id->idAddr()->isEbpBased;
    int  disp     = id->idAddr()->lclOffset;

    if (!ebpBased)
    {
#if !FEATURE_FIXED_OUT_ARGS
        disp += stackLevel;
#endif
        assert(disp >= 0);

        // ESP based addressing modes always require a SIB byte.
        sz++;
    }

    // EBP based addressing modes always require displacement.
    if (ebpBased || (disp != 0))
    {
        sz += IsDisp8(disp) ? 1 : 4;
    }

    return sz;
}

unsigned X86Emitter::EncodingSizeAM(instrDesc* id, code_t code)
{
    instruction ins      = id->idIns();
    emitAttr    size     = id->idOpSize();
    ssize_t     disp     = (ins == INS_call) ? id->GetCallDisp() : id->GetAmDisp();
    bool        hasDisp8 = ((int8_t)disp == disp) && !id->idIsDspReloc();
    bool        hasDisp  = (disp != 0) || id->idIsDspReloc();
    RegNum      baseReg  = id->idAddr()->iiaAddrMode.base;
    RegNum      indexReg = id->idAddr()->iiaAddrMode.index;

    // BT supports 16 bit operands and this code doesn't handle the necessary 66 prefix.
    assert(ins != INS_bt);
    assert((IF_ARD <= id->idInsFmt()) && (id->idInsFmt() <= IF_AWR_RRD_CNS));
    assert(!id->HasFSPrefix());

#ifdef TARGET_X86
    // Special case: "mov eax, [addr]" and "mov [addr], eax" have smaller encoding.
    if ((ins == INS_mov) && ((id->idInsFmt() == IF_RWR_ARD) || (id->idInsFmt() == IF_AWR_RRD)) &&
        (id->idReg1() == REG_EAX) && (baseReg == REG_NA) && (indexReg == REG_NA))
    {
        return (size == EA_2BYTE) + 1 + 4;
    }
#endif

    unsigned sz;

    if (TakesVexPrefix(ins))
    {
        sz = 2 + (HasRexW(code) || !IsMap0F(code) || TakesRexWPrefix(ins, size) || IsExtendedReg(baseReg) ||
                  IsExtendedReg(indexReg)) +
             1 + 1;
    }
    else
    {
        sz = EncodingSize(ins, size, code);
        sz += hasRexPrefix(code) || IsExtendedReg(baseReg) || IsExtendedReg(indexReg) ||
              ((ins != INS_call) && (IsExtendedReg(id->idReg1(), size) || IsExtendedReg(id->idReg2(), size))) ||
              TakesRexWPrefix(ins, size);
    }

    if ((baseReg == REG_NA) && (indexReg == REG_NA))
    {
        sz += 4;

#ifdef TARGET_AMD64
        if (!id->idIsDspReloc())
        {
            sz++;
        }
#endif

        return sz;
    }

    if (indexReg == REG_NA)
    {
        if (BaseRegRequiresSIB(baseReg))
        {
            sz++;
        }

        if (hasDisp || BaseRegRequiresDisp(baseReg))
        {
            sz += hasDisp8 ? 1 : 4;
        }

        return sz;
    }

    sz++;

    if (id->idAddr()->iiaAddrMode.scale != 0)
    {
        if (baseReg == REG_NA)
        {
            sz += 4;
        }
        else if (hasDisp || BaseRegRequiresDisp(baseReg))
        {
            sz += hasDisp8 ? 1 : 4;
        }

        return sz;
    }

    // When we are using the SIB or VSIB format with EBP or R13 as a base, we must emit at least
    // a 1 byte displacement (this is a special case in the encoding to allow for the case of no
    // base register at all). In order to avoid this when we have no scaling, we can reverse the
    // registers so that we don't have to add that extra byte. However, we can't do that if the
    // index register is a vector, such as for a gather instruction.

    if (!hasDisp && BaseRegRequiresDisp(baseReg) && !BaseRegRequiresDisp(indexReg) && !IsFloatReg(indexReg))
    {
        std::swap(baseReg, indexReg);
        id->idAddr()->iiaAddrMode.base  = baseReg;
        id->idAddr()->iiaAddrMode.index = indexReg;
    }

    if (hasDisp || BaseRegRequiresDisp(baseReg))
    {
        sz += hasDisp8 ? 1 : 4;
    }

    return sz;
}

unsigned X86Emitter::EncodingSizeCV(instrDesc* id, code_t code)
{
    instruction ins  = id->idIns();
    emitAttr    size = id->idOpSize();

    unsigned sz;

    if (TakesVexPrefix(ins))
    {
        sz = 2 + (HasRexW(code) || !IsMap0F(code) || TakesRexWPrefix(ins, size)) + 1 + 1;
    }
    else
    {
        sz = EncodingSize(ins, size, code);
        sz += hasRexPrefix(code) || IsExtendedReg(id->idReg1(), size) || IsExtendedReg(id->idReg2(), size) ||
              TakesRexWPrefix(ins, size);
    }

    return sz + 4;
}

static unsigned ImmEncodingSize(instruction ins, emitAttr attr, int32_t imm)
{
    if (IsShiftImm(ins))
    {
        return 1;
    }

    // BT mem,imm might be useful but it requires special handling of the immediate value
    // (it is always encoded in a byte). Let's not complicate things until this is needed.
    assert(ins != INS_bt);

    if ((static_cast<int8_t>(imm) == imm) && (ins != INS_mov) && (ins != INS_test) && !EA_IS_CNS_RELOC(attr))
    {
        return 1;
    }

    assert(!IsSSEOrAVXOrBMIInstruction(ins));

    return Min(EA_SIZE_IN_BYTES(attr), 4u);
}

template <typename T>
T* X86Emitter::AllocInstr(bool updateLastIns)
{
    instrDescSmall* id = AllocAnyInstr(sizeof(T), updateLastIns);
    memset(id, 0, sizeof(T));
    INDEBUG(id->idDebugOnlyInfo(new (compiler, CMK_DebugOnly) instrDescDebugInfo(++nextInstrNum, sizeof(T))));

    return static_cast<T*>(id);
}

instrDesc* X86Emitter::NewInstr()
{
    return AllocInstr<instrDesc>();
}

instrDescJmp* X86Emitter::NewInstrJmp()
{
    instrDescJmp* jmp = AllocInstr<instrDescJmp>();
    jmp->idjIG        = currentIG;
    jmp->idjOffs      = currentIGCodeSize;
    jmp->idjNext      = currentIGJumps;
    currentIGJumps    = jmp;
    return jmp;
}

instrDescCGCA* X86Emitter::AllocInstrCGCA()
{
    return AllocInstr<instrDescCGCA>();
}

instrDesc* X86Emitter::NewInstrSmall()
{
    instrDescSmall* id = AllocInstr<instrDescSmall>();
    id->idSetIsSmallDsc();
    return static_cast<instrDesc*>(id);
}

instrDesc* X86Emitter::NewInstrSmall(ssize_t cns)
{
    if (!instrDesc::fitsInSmallCns(cns))
    {
        instrDescCns* id = AllocInstr<instrDescCns>();
        id->idSetIsLargeCns();
        id->idcCnsVal = cns;
        return id;
    }

    instrDesc* id = NewInstrSmall();
    id->idSmallCns(cns);
    return id;
}

instrDesc* X86Emitter::NewInstrCns(int32_t cns)
{
    if (!instrDesc::fitsInSmallCns(cns))
    {
        instrDescCns* id = AllocInstr<instrDescCns>();
        id->idSetIsLargeCns();
        id->idcCnsVal = cns;
        return id;
    }

    instrDesc* id = NewInstr();
    id->idSmallCns(cns);
    return id;
}

#ifdef TARGET_X86
instrDesc* X86Emitter::NewInstrDsp(int32_t disp)
{
    if (disp == 0)
    {
        return NewInstr();
    }

    instrDescAmd* id = AllocInstr<instrDescAmd>();
    id->idSetIsLargeDsp();
    id->idaAmdVal = disp;
    return id;
}
#endif // TARGET_X86

instrDesc* X86Emitter::NewInstrAMDisp(ssize_t disp)
{
    if (emitAddrMode::IsLargeDisp(disp))
    {
        instrDescAmd* id = AllocInstr<instrDescAmd>();
        id->idSetIsLargeDsp();
        INDEBUG(id->idAddr()->iiaAddrMode.disp = emitAddrMode::LargeDispMarker);
        id->idaAmdVal = disp;

        return id;
    }

    instrDesc* id                  = NewInstr();
    id->idAddr()->iiaAddrMode.disp = disp;
    assert(id->idAddr()->iiaAddrMode.disp == disp); // make sure the value fit
    return id;
}

instrDesc* X86Emitter::NewInstrAMDispImm(ssize_t disp, int32_t imm)
{
    if (!emitAddrMode::IsLargeDisp(disp))
    {
        instrDesc* id                  = NewInstrCns(imm);
        id->idAddr()->iiaAddrMode.disp = disp;
        assert(id->idAddr()->iiaAddrMode.disp == disp); // make sure the value fit

        return id;
    }

    if (instrDesc::fitsInSmallCns(imm))
    {
        instrDescAmd* id = AllocInstr<instrDescAmd>();
        id->idSetIsLargeDsp();
        INDEBUG(id->idAddr()->iiaAddrMode.disp = emitAddrMode::LargeDispMarker);
        id->idaAmdVal = disp;
        id->idSmallCns(imm);

        return id;
    }

    instrDescCnsAmd* id = AllocInstr<instrDescCnsAmd>();
    id->idSetIsLargeCns();
    id->idcCnsVal = imm;
    id->idSetIsLargeDsp();
    INDEBUG(id->idAddr()->iiaAddrMode.disp = emitAddrMode::LargeDispMarker);
    id->idacAmdVal = disp;

    return id;
}

instrDesc* X86Emitter::NewInstrGCReg(emitAttr attr, RegNum reg)
{
    assert(EA_IS_GCREF_OR_BYREF(attr));
    assert(IsGeneralRegister(reg));

    if ((codeGen->liveness.GetGCRegs(attr) & genRegMask(reg)) != RBM_NONE)
    {
        return nullptr;
    }

    instrDesc* id = static_cast<instrDesc*>(AllocInstr<instrDescSmall>(false));
    id->idSetIsSmallDsc();
    id->idIns(INS_mov);
    id->idInsFmt(IF_GC_REG);
    id->idGCref(EA_GC_TYPE(attr));
    id->idReg1(reg);
    id->idReg2(reg);

    return id;
}

void X86Emitter::emitLoopAlign(uint16_t paddingBytes)
{
    assert(paddingBytes <= instrDesc::MAX_ENCODED_SIZE);
    paddingBytes = min(paddingBytes, instrDesc::MAX_ENCODED_SIZE);

    instrDescAlign* id = AllocInstr<instrDescAlign>();
    id->idIns(INS_align);
    id->idCodeSize(paddingBytes);
    id->idaIG   = currentIG;
    id->idaNext = currentIGAligns;

    currentIGCodeSize += paddingBytes;
    currentIGAligns = id;
}

void X86Emitter::emitLongLoopAlign(uint16_t alignmentBoundary)
{
    uint16_t nPaddingBytes    = alignmentBoundary - 1;
    uint16_t nAlignInstr      = (nPaddingBytes + (instrDesc::MAX_ENCODED_SIZE - 1)) / instrDesc::MAX_ENCODED_SIZE;
    uint16_t instrDescSize    = nAlignInstr * sizeof(instrDescAlign);
    uint16_t insAlignCount    = nPaddingBytes / instrDesc::MAX_ENCODED_SIZE;
    uint16_t lastInsAlignSize = nPaddingBytes % instrDesc::MAX_ENCODED_SIZE;

    // Ensure that all align instructions fall in same IG.
    if (instrBufferFree + instrDescSize >= instrBufferEnd)
    {
        forceNewIG = true;
    }

    while (insAlignCount)
    {
        emitLoopAlign(instrDesc::MAX_ENCODED_SIZE);
        insAlignCount--;
    }

    emitLoopAlign(lastInsAlignSize);
}

void X86Emitter::emitIns_Nop(unsigned size)
{
    assert(size <= instrDesc::MAX_ENCODED_SIZE);

    instrDesc* id = NewInstr();
    id->idIns(INS_nop);
    id->idInsFmt(IF_NONE);

    id->idCodeSize(size);
    PrintInstr(id);
    currentIGCodeSize += size;
}

void X86Emitter::emitIns_Lock()
{
    instrDesc* id = NewInstr();
    id->idIns(INS_lock);
    id->idInsFmt(IF_NONE);

    id->idCodeSize(1);
    PrintInstr(id);
    currentIGCodeSize++;

#ifdef PSEUDORANDOM_NOP_INSERTION
    if (nextRandomNop == 0)
    {
        nextRandomNop = 1;
    }
#endif
}

void X86Emitter::emitIns(instruction ins)
{
    assert((ins == INS_int3) || (ins == INS_leave) || (ins == INS_nop) || (ins == INS_ret) || (ins == INS_vzeroupper) ||
           (ins == INS_lfence) || (ins == INS_mfence) || (ins == INS_sfence));

    instrDesc* id = NewInstr();
    id->idIns(ins);
    id->idInsFmt(IF_NONE);

    code_t code = GetCodeMR(ins);
    assert((code >> 24) == 0);

    unsigned sz;

    if ((code & 0x00FF0000) != 0)
    {
        sz = 3;
    }
    else
    {
        assert((code & 0xFF00) == 0);
        sz = 1;
    }

    // vzeroupper includes its 2-byte VEX prefix in its MR code.
    assert((ins != INS_vzeroupper) || (sz == 3));

    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns(instruction ins, emitAttr attr)
{
    assert((ins == INS_cdq) || (ins == INS_movs) || (ins == INS_stos) || (ins == INS_rep_movs) ||
           (ins == INS_rep_stos));
    assert((attr == EA_1BYTE) || (attr == EA_4BYTE)AMD64_ONLY(|| (attr == EA_8BYTE)));

    instrDesc* id = NewInstr();
    id->idIns(ins);
    id->idOpSize(attr);
    id->idInsFmt(IF_NONE);

    size_t code = GetCodeMR(ins);
    assert((code >> PrefixesBitOffset) == 0);

    unsigned sz = 1 + ((code & 0xFF00) != 0)AMD64_ONLY(+(attr == EA_8BYTE));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::SetInstrLclAddrMode(instrDesc* id, StackAddrMode s)
{
    id->SetVarAddr(INDEBUG(s));

    bool ebpBased;
    int  offset = compiler->lvaFrameAddress(s.varNum, &ebpBased) + s.varOffs;

    id->idAddr()->lclOffset  = offset;
    id->idAddr()->isEbpBased = ebpBased;

    if ((id->idGCref() != GCT_NONE) && (id->idInsFmt() == IF_SWR_RRD))
    {
        if (s.varNum < 0)
        {
            assert(s.varOffs == 0);
            id->idAddr()->isTrackedGCSlotStore = codeGen->spillTemps.TrackGCSpillTemps();
        }
#if FEATURE_FIXED_OUT_ARGS
        else if (static_cast<unsigned>(s.varNum) == compiler->lvaOutgoingArgSpaceVar)
        {
            id->idAddr()->isGCArgStore = true;
        }
#endif
        else if ((s.varOffs == 0) && (compiler->lvaGetDesc(static_cast<unsigned>(s.varNum))->HasGCSlotLiveness()))
        {
            id->idAddr()->isTrackedGCSlotStore = true;
        }
    }
}

bool X86Emitter::IntConNeedsReloc(GenTreeIntCon* con)
{
#ifdef TARGET_AMD64
    if (compiler->opts.compReloc)
    {
        // Only handles need relocations and can be RIP relative in crossgen mode.
        return con->IsIconHandle();
    }

    // At JIT time we try to use RIP relative addressing by default but that can fail
    // if a lot of code is generated. For the runtime to detect such failures we need
    // to call recordRelocation (which may also perform relocation for code addresses
    // by means of jump stubs).
    return compiler->eeIsRIPRelativeAddress(reinterpret_cast<void*>(con->GetValue()));
#else
    return con->ImmedValNeedsReloc(compiler);
#endif
}

ssize_t X86Emitter::GetAddrModeDisp(GenTree* addr)
{
    if (!addr->isContained())
    {
        return 0;
    }

    if (GenTreeIntCon* intConAddr = addr->IsIntCon())
    {
        return intConAddr->GetValue();
    }

    return addr->AsAddrMode()->GetOffset();
}

void X86Emitter::SetInstrAddrMode(instrDesc* id, GenTree* addr)
{
    if (!addr->isContained())
    {
        id->idAddr()->iiaAddrMode.base  = addr->GetRegNum();
        id->idAddr()->iiaAddrMode.index = REG_NA;
        id->idAddr()->iiaAddrMode.scale = 0;
        assert(id->GetAmDisp() == 0);

        return;
    }

    if (GenTreeIntCon* intConAddr = addr->IsIntCon())
    {
        // Absolute addresses marked as contained should fit within the base of addr mode.
        AMD64_ONLY(assert(compiler->IsRIPRelativeAddress(intConAddr)));

        id->idSetIsDspReloc(IntConNeedsReloc(intConAddr));
        id->idAddr()->iiaAddrMode.base  = REG_NA;
        id->idAddr()->iiaAddrMode.index = REG_NA;
        id->idAddr()->iiaAddrMode.scale = 0;
        // The displacement must have already been set by the caller.
        assert(id->GetAmDisp() == intConAddr->GetValue());

        INDEBUG(id->idDebugOnlyInfo()->dispHandleKind = intConAddr->GetHandleKind());

        return;
    }

    GenTreeAddrMode* addrMode = addr->AsAddrMode();

    if (GenTree* base = addrMode->GetBase())
    {
        RegNum baseReg = base->GetRegNum();
        assert(baseReg != REG_NA);
        id->idAddr()->iiaAddrMode.base = baseReg;
    }
    else
    {
        id->idAddr()->iiaAddrMode.base = REG_NA;
    }

    if (GenTree* index = addrMode->GetIndex())
    {
        RegNum indexReg = index->GetRegNum();
        assert(indexReg != REG_NA);
        id->idAddr()->iiaAddrMode.index = indexReg;
        id->idAddr()->iiaAddrMode.scale = ScaleEncoding(addrMode->GetScale());
    }
    else
    {
        id->idAddr()->iiaAddrMode.index = REG_NA;
        id->idAddr()->iiaAddrMode.scale = 0;
    }

    // The displacement must have already been set by the caller.
    assert(id->GetAmDisp() == addrMode->GetOffset());
}

// Takes care of storing all incoming register parameters into
// its corresponding shadow space (defined by the win-x64 ABI).
void X86Emitter::PrologSpillParamRegsToShadowSlots()
{
    assert(codeGen->generatingProlog);

    for (unsigned argNum = 0; argNum < MAX_REG_ARG; ++argNum)
    {
        RegNum argReg = intArgRegs[argNum];

        // The offsets for the shadow space start at RSP + 8
        // (right before the caller return address)
        int offset = (argNum + 1) * EA_PTRSIZE;

        instrDesc* id = NewInstrAMDisp(offset);
        id->idIns(INS_mov);
        id->idOpSize(EA_PTRSIZE);
        id->idInsFmt(IF_AWR_RRD);
        id->idAddr()->iiaAddrMode.base  = REG_SPBASE;
        id->idAddr()->iiaAddrMode.index = REG_NA;
        id->idAddr()->iiaAddrMode.scale = 0;
        id->idReg1(argReg);

        unsigned sz = EncodingSizeAM(id, GetCodeMR(INS_mov));
        id->idCodeSize(sz);
        currentIGCodeSize += sz;
    }
}

void X86Emitter::emitIns_A(instruction ins, emitAttr attr, GenTree* addr)
{
    if (GenTreeConstAddr* constAddr = addr->IsConstAddr())
    {
        emitIns_C(ins, attr, constAddr->GetData());
        return;
    }

    // TODO-MIKE-Cleanup: IND with GT_LCL_ADDR address is nonsense.

    if (GenTreeLclAddr* lclAddr = addr->IsLclAddr())
    {
        assert(lclAddr->GetLcl()->IsAddressExposed());
        emitIns_S(ins, attr, codeGen->GetStackAddrMode(lclAddr));

        return;
    }

    instrDesc* id = NewInstrAMDisp(GetAddrModeDisp(addr));
    id->idIns(ins);
    id->idInsFmt(MapFormat(ins, IF_ARD));
    id->idOpSize(EA_SIZE(attr));
    id->idGCref(EA_GC_TYPE(attr));
    SetInstrAddrMode(id, addr);

    unsigned sz = EncodingSizeAM(id, GetCodeMR(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;

#if !FEATURE_FIXED_OUT_ARGS
    UpdateStackLevel(ins);
#endif
}

void X86Emitter::emitIns_A_I(instruction ins, emitAttr attr, GenTree* addr, int32_t imm)
{
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));

    if (GenTreeConstAddr* constAddr = addr->IsConstAddr())
    {
        emitIns_C_I(ins, attr, constAddr->GetData(), imm);
        return;
    }

    if (GenTreeLclAddr* lclAddr = addr->IsLclAddr())
    {
        assert(lclAddr->GetLcl()->IsAddressExposed());
        emitIns_S_I(ins, attr, codeGen->GetStackAddrMode(lclAddr), imm);

        return;
    }

    instrDesc* id = NewInstrAMDispImm(GetAddrModeDisp(addr), imm);
    id->idIns(ins);
    id->idInsFmt(MapFormat(ins, IF_ARD_CNS));
    id->idOpSize(EA_SIZE(attr));
    INDEBUG(id->idGCref(EA_GC_TYPE(attr)));
    X86_ONLY(id->idSetIsCnsReloc(EA_IS_CNS_RELOC(attr) && compiler->opts.compReloc));
    SetInstrAddrMode(id, addr);

    unsigned sz = EncodingSizeAM(id, GetCodeMI(ins)) + ImmEncodingSize(ins, attr, imm);
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_A_R(instruction ins, emitAttr attr, GenTree* addr, RegNum reg)
{
    assert(!IsReallyVexTernary(ins));

    if (GenTreeConstAddr* constAddr = addr->IsConstAddr())
    {
        emitIns_C_R(ins, attr, constAddr->GetData(), reg);
        return;
    }

    if (GenTreeLclAddr* lclAddr = addr->IsLclAddr())
    {
        assert(lclAddr->GetLcl()->IsAddressExposed());
        emitIns_S_R(ins, attr, reg, codeGen->GetStackAddrMode(lclAddr));

        return;
    }

    instrDesc* id = NewInstrAMDisp(GetAddrModeDisp(addr));
    id->idIns(ins);
    id->idInsFmt(MapFormat(ins, IF_ARD_RRD));
    id->idOpSize(EA_SIZE(attr));
    INDEBUG(id->idGCref(EA_GC_TYPE(attr)));
    id->idReg1(reg);
    SetInstrAddrMode(id, addr);

    unsigned sz = EncodingSizeAM(id, GetCodeMR(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitInsRMW_A(instruction ins, emitAttr attr, GenTree* addr)
{
    instrDesc* id = NewInstrAMDisp(GetAddrModeDisp(addr));
    id->idIns(ins);
    id->idInsFmt(IF_ARW);
    id->idOpSize(EA_SIZE(attr));
    INDEBUG(id->idGCref(EA_GC_TYPE(attr)));
    SetInstrAddrMode(id, addr);

    unsigned sz = EncodingSizeAM(id, GetCodeMR(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitInsRMW_A_I(instruction ins, emitAttr attr, GenTree* addr, int32_t imm)
{
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));

    instrDesc* id = NewInstrAMDispImm(GetAddrModeDisp(addr), imm);
    id->idIns(ins);
    id->idInsFmt(IF_ARW_CNS);
    id->idOpSize(EA_SIZE(attr));
    INDEBUG(id->idGCref(EA_GC_TYPE(attr)));
    SetInstrAddrMode(id, addr);

    unsigned sz = EncodingSizeAM(id, GetCodeMI(ins)) + ImmEncodingSize(ins, attr, imm);
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitInsRMW_A_R(instruction ins, emitAttr attr, GenTree* addr, RegNum reg)
{
    instrDesc* id = NewInstrAMDisp(GetAddrModeDisp(addr));
    id->idIns(ins);
    id->idInsFmt(IF_ARW_RRD);
    id->idOpSize(EA_SIZE(attr));
    INDEBUG(id->idGCref(EA_GC_TYPE(attr)));
    id->idReg1(reg);
    SetInstrAddrMode(id, addr);

    unsigned sz = EncodingSizeAM(id, GetCodeMR(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R(instruction ins, emitAttr attr, RegNum reg)
{
    X86_ONLY(noway_assert(VerifyEncodable(ins, attr, reg)));

    emitAttr size = EA_SIZE(attr);
    assert(size <= EA_PTRSIZE);

    instrDesc* id = NewInstrSmall();
    id->idIns(ins);
    id->idOpSize(size);
    id->idInsFmt(MapFormat(ins, IF_RRD));
    id->idGCref(EA_GC_TYPE(attr));
    id->idReg1(reg);

    unsigned sz = EncodingSizeR(ins, size, reg);
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;

#if !FEATURE_FIXED_OUT_ARGS
    UpdateStackLevel(ins);
#endif
}

void X86Emitter::emitIns_R_H(instruction ins, RegNum reg, void* addr DEBUGARG(HandleKind handleKind))
{
    assert(ins == INS_mov);
    assert(genIsValidIntReg(reg) && (reg != REG_RSP));

    instrDesc* id = NewInstrSmall(reinterpret_cast<ssize_t>(addr));
    id->idIns(ins);
    id->idOpSize(EA_PTRSIZE);
    id->idInsFmt(IF_RWR_CNS);
    id->idSetIsCnsReloc(compiler->opts.compReloc);
    id->idReg1(reg);
    INDEBUG(id->idDebugOnlyInfo()->idHandleKind = handleKind);

#ifdef TARGET_AMD64
    // Because it has to be relocatable this has to be a "mov reg, imm64", we can't narrow it
    // down to imm32. And since it's always a 64 bit operation it always has a REX prefix.
    unsigned size = 10;
#else
    unsigned size = 5;
#endif
    id->idCodeSize(size);
    PrintInstr(id);
    currentIGCodeSize += size;
}

void X86Emitter::emitIns_R_I(instruction ins, emitAttr attr, RegNum reg, ssize_t imm DEBUGARG(HandleKind handleKind))
{
    // BT reg,imm might be useful but it requires special handling of the immediate value
    // (it is always encoded in a byte). Let's not complicate things until this is needed.
    assert(ins != INS_bt);
    assert(!EA_IS_RELOC(attr));
    X86_ONLY(noway_assert(VerifyEncodable(ins, attr, reg)));

    emitAttr size = EA_SIZE(attr);
    // Allow emitting SSE2/AVX SIMD instructions of R_I form that can specify EA_16BYTE or EA_32BYTE
    assert((size <= EA_PTRSIZE) || IsSSEOrAVXOrBMIInstruction(ins));
    // mov reg, imm64 is the only opcode which takes a full 8 byte immediate
    // all other opcodes take a sign-extended 4-byte immediate
    AMD64_ONLY(noway_assert((size < EA_8BYTE) || (ins == INS_mov) || IsImm32(imm)));

#ifdef TARGET_AMD64
    // mov reg, imm64 is equivalent to mov reg, imm32 if the high order bits are all 0
    // and this isn't a reloc constant.
    // TODO-MIKE-Review: This doesn't check for relocs as the comment claims, the reloc
    // bit was stripped by EA_SIZE above.
    if ((ins == INS_mov) && (size == EA_8BYTE) && (0 <= imm) && (imm <= UINT_MAX))
    {
        attr = EA_4BYTE;
        size = EA_4BYTE;
    }
#endif

    instrDesc* id = NewInstrSmall(imm);
    id->idIns(ins);
    id->idOpSize(size);
    id->idInsFmt(MapFormat(ins, IF_RRD_CNS));
    id->idGCref(EA_GC_TYPE(attr));
    id->idReg1(reg);
    INDEBUG(id->idDebugOnlyInfo()->idHandleKind = handleKind);

    unsigned sz = EncodingSizeRI(ins, size, reg, imm);
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;

#if !FEATURE_FIXED_OUT_ARGS
    if (reg == REG_ESP)
    {
        UpdateStackLevel(ins, imm);
    }
#endif
}

#ifdef TARGET_X86
void X86Emitter::emitIns_H(instruction ins, void* addr)
{
    assert((ins == INS_push) || (ins == INS_push_hide));

    instrDesc* id = NewInstrSmall(reinterpret_cast<ssize_t>(addr));
    id->idIns(ins);
    id->idOpSize(EA_PTRSIZE);
    id->idInsFmt(IF_CNS);
    id->idSetIsCnsReloc(compiler->opts.compReloc);

    id->idCodeSize(5);
    PrintInstr(id);
    currentIGCodeSize += 5;

#if !FEATURE_FIXED_OUT_ARGS
    UpdateStackLevel(ins);
#endif
}
#endif

#ifdef WINDOWS_X86_ABI
void X86Emitter::emitInsMov_R_FS(RegNum reg, int32_t disp)
{
    assert(genIsValidIntReg(reg));

    instrDesc* id = NewInstrAMDisp(disp);
    id->idIns(INS_mov);
    id->idOpSize(EA_4BYTE);
    id->idInsFmt(MapFormat(INS_mov, IF_RRD_ARD));
    id->SetHasFSPrefix();
    id->idReg1(reg);
    id->idAddr()->iiaAddrMode.base  = REG_NA;
    id->idAddr()->iiaAddrMode.index = REG_NA;

    unsigned sz = 1 + (reg == REG_EAX ? 1 : 2) + 4;
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}
#endif // WINDOWS_X86_ABI

void X86Emitter::emitIns_I(instruction ins, emitAttr attr, int32_t imm)
{
    instrDesc* id = NewInstrSmall(imm);
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt(IF_CNS);
    id->idSetIsCnsReloc(EA_IS_CNS_RELOC(attr) && compiler->opts.compReloc);

    unsigned sz;

#ifdef TARGET_AMD64
    // On x64 only LCLHEAP uses push, to allocate and initialize stack memory.
    assert((ins == INS_push_hide) && (attr == EA_8BYTE) && (imm == 0));

    sz = 2;
#else
    if (ins == INS_ret)
    {
        assert((0 <= imm) && (imm <= UINT16_MAX));
        sz = 3;
    }
    else
    {
        assert((ins == INS_push) || (ins == INS_push_hide));
        sz = !EA_IS_RELOC(attr) && IsImm8(imm) ? 2 : 5;
    }
#endif

    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;

#if !FEATURE_FIXED_OUT_ARGS
    UpdateStackLevel(ins);
#endif
}

void X86Emitter::emitIns_C(instruction ins, emitAttr attr, ConstData* data)
{
    instrDesc* id = NewInstr();
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt(MapFormat(ins, IF_MRD));
    INDEBUG(id->idGCref(EA_GC_TYPE(attr)));
    id->idSetIsDspReloc();
    id->SetConstData(data);

    unsigned sz = EncodingSizeCV(id, GetCodeMR(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;

#if !FEATURE_FIXED_OUT_ARGS
    UpdateStackLevel(ins);
#endif
}

bool IsMovIns(instruction ins)
{
    switch (ins)
    {
        case INS_mov:
        case INS_movapd:
        case INS_movaps:
        case INS_movd:
        case INS_movdqa:
        case INS_movdqu:
        case INS_movsd:
        case INS_movss:
        case INS_movsx:
        case INS_movupd:
        case INS_movups:
        case INS_movzx:
#ifdef TARGET_AMD64
        case INS_movq:
        case INS_movsxd:
#endif
            return true;
        default:
            return false;
    }
}

// Check if the current `mov` instruction is redundant and can be omitted.
// A `mov` is redundant in following 3 cases:
//
//     1. Move to same register on TARGET_AMD64
//       (Except 4-byte movement like "mov eax, eax" which zeros out upper bits of eax register)
//
//         mov rax, rax
//
//    2. Move that is identical to last instruction emitted.
//
//         mov rax, rbx  # <-- last instruction
//         mov rax, rbx  # <-- current instruction can be omitted.
//
//    3. Opposite Move as that of last instruction emitted.
//
//         mov rax, rbx  # <-- last instruction
//         mov rbx, rax  # <-- current instruction can be omitted.
//
bool X86Emitter::IsRedundantMov(instruction ins, emitAttr size, RegNum dst, RegNum src, bool canIgnoreSideEffects)
{
    assert(IsMovIns(ins));

    if (canIgnoreSideEffects && (dst == src))
    {
        // These elisions used to be explicit even when optimizations were disabled

        // Some instructions have a side effect and shouldn't be skipped
        // however existing codepaths were skipping these instructions in
        // certain scenarios and so we skip them as well for back-compat
        // when canIgnoreSideEffects is true (see below for which have a
        // side effect).
        //
        // Long term, these paths should be audited and should likely be
        // replaced with copies rather than extensions.
        return true;
    }

    if (!compiler->opts.OptimizationEnabled())
    {
        // The remaining move elisions should only happen if optimizations are enabled
        return false;
    }

    // TODO-XArch-CQ: There are places where the fact that an instruction zero-extends
    // is not an important detail, such as when "regular" floating-point code is generated
    //
    // This differs from cases like HWIntrinsics that deal with the entire vector and so
    // they need to be "aware" that a given move impacts the upper-bits.
    //
    // Ideally we can detect this difference, likely via canIgnoreSideEffects, and allow
    // the below optimizations for those scenarios as well.

    // Track whether the instruction has a zero/sign-extension or clearing of the upper-bits as a side-effect
    bool hasSideEffect = false;

    switch (ins)
    {
        case INS_mov:
            // non EA_PTRSIZE moves may zero-extend the source
            hasSideEffect = (size != EA_PTRSIZE);
            break;
        case INS_movapd:
        case INS_movaps:
        case INS_movdqa:
        case INS_movdqu:
        case INS_movupd:
        case INS_movups:
            // non EA_32BYTE moves clear the upper bits under VEX encoding
            hasSideEffect = UseVEXEncoding() && (size != EA_32BYTE);
            break;
        case INS_movd:
#ifdef TARGET_AMD64
        case INS_movq:
#endif
            // Clears the upper bits
            hasSideEffect = true;
            break;
        case INS_movsd:
        case INS_movss:
            // Clears the upper bits under VEX encoding
            hasSideEffect = UseVEXEncoding();
            break;
        case INS_movsx:
        case INS_movzx:
#ifdef TARGET_AMD64
        case INS_movsxd:
#endif
            // Sign/Zero-extends the source
            hasSideEffect = true;
            break;
        default:
            unreached();
    }

    // Check if we are already in the correct register and don't have a side effect
    if ((dst == src) && !hasSideEffect)
    {
        JITDUMP("\n -- suppressing mov because src and dst is same register and the mov has no side-effects.\n");
        return true;
    }

    instrDesc* lastIns = GetLastInsInCurrentBlock();

    // TODO-XArch-CQ: Certain instructions, such as movaps vs movups, are equivalent in
    // functionality even if their actual identifier differs and we should optimize these

    if ((lastIns == nullptr) || (lastIns->idIns() != ins) || (lastIns->idOpSize() != size) ||
        (lastIns->idInsFmt() != IF_RWR_RRD))
    {
        return false;
    }

    RegNum lastDst = lastIns->idReg1();
    RegNum lastSrc = lastIns->idReg2();

    // Check if we did same move in last instruction, side effects don't matter since they already happened
    if ((lastDst == dst) && (lastSrc == src))
    {
        JITDUMP("\n -- suppressing mov because last instruction already moved from src to dst register.\n");
        return true;
    }

    // Check if we did a switched mov in the last instruction  and don't have a side effect
    if ((lastDst == src) && (lastSrc == dst) && !hasSideEffect)
    {
        JITDUMP("\n -- suppressing mov because last instruction already moved from dst to src register and the mov has "
                "no side-effects.\n");
        return true;
    }

    return false;
}

void X86Emitter::emitIns_Mov(instruction ins, emitAttr attr, RegNum dstReg, RegNum srcReg, bool canSkip)
{
    assert(IsMovIns(ins));

    if (EA_IS_GCREF_OR_BYREF(attr) && (ins == INS_mov) && (dstReg == srcReg))
    {
        NewInstrGCReg(attr, dstReg);

        return;
    }

#if DEBUG
    switch (ins)
    {
        case INS_mov:
        case INS_movsx:
        case INS_movzx:
#ifdef TARGET_AMD64
        case INS_movsxd:
#endif
            assert(IsGeneralRegister(dstReg) && IsGeneralRegister(srcReg));
            break;
        case INS_movapd:
        case INS_movaps:
        case INS_movdqa:
        case INS_movdqu:
            // TODO-MIKE-Review: CodeGen is messed up and passes EA_4/8BYTE for float/double reg
            // copies, even the instruction is really copying at least 16 bytes, this confuses
            // the redundant mov elimination code.
            if (EA_SIZE_IN_BYTES(attr) < 16)
            {
                attr = EA_16BYTE;
            }
            FALLTHROUGH;
        case INS_movsd:
        case INS_movss:
        case INS_movupd:
        case INS_movups:
#ifdef TARGET_AMD64
        case INS_movq:
#endif
            assert(IsFloatReg(dstReg) && IsFloatReg(srcReg));
            break;
        case INS_movd:
            assert(IsFloatReg(dstReg) != IsFloatReg(srcReg));
            break;
        default:
            unreached();
    }
#endif

    X86_ONLY(noway_assert(VerifyEncodable(ins, attr, dstReg, srcReg)));
    // TODO-MIKE-Review: movss/movsd are actually IF_RRW_RRD, but the table says
    // they're IF_RWR_RRD. And they actually are, in their memory load form. But
    // it doesn't really matter, we only care about WR vs. RW when updating GC
    // liveness and that doesn't apply to movss/movsd.
    assert(MapFormat(ins, IF_RRD_RRD) == IF_RWR_RRD);

    if (IsRedundantMov(ins, attr, dstReg, srcReg, canSkip))
    {
        return;
    }

    instrDesc* id = NewInstrSmall();
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt(IF_RWR_RRD);
    id->idGCref(EA_GC_TYPE(attr));
    id->idReg1(dstReg);
    id->idReg2(srcReg);

    unsigned sz = EncodingSizeRR(ins, EA_SIZE(attr), dstReg, srcReg);
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2)
{
    assert(!HasImplicitRegPairDest(ins) && (ins != INS_imuli));
    assert(!IsMovIns(ins));

    X86_ONLY(noway_assert(VerifyEncodable(ins, attr, reg1, reg2)));

    instrDesc* id = NewInstrSmall();
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt((ins == INS_xchg) ? IF_RRW_RRW : MapFormat(ins, IF_RRD_RRD));
    id->idGCref(EA_GC_TYPE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);

    unsigned sz = EncodingSizeRR(ins, EA_SIZE(attr), reg1, reg2);
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_R_I(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, int32_t imm)
{
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));

    instrDesc* id = NewInstrSmall(imm);
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt(IF_RRW_RRD_CNS);
    id->idGCref(EA_GC_TYPE(attr));
    X86_ONLY(id->idSetIsCnsReloc(EA_IS_CNS_RELOC(attr) && compiler->opts.compReloc));
    id->idReg1(reg1);
    id->idReg2(reg2);

    unsigned sz = EncodingSizeRRI(ins, EA_SIZE(attr), reg1, reg2) + ImmEncodingSize(ins, attr, imm);
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_AR(instruction ins, emitAttr attr, RegNum base, int32_t disp)
{
    assert(IsPrefetch(ins) && (attr == EA_1BYTE));

    instrDesc* id = NewInstrAMDisp(disp);
    id->idIns(ins);
    id->idInsFmt(IF_ARD);
    id->idAddr()->iiaAddrMode.base  = base;
    id->idAddr()->iiaAddrMode.index = REG_NA;

    unsigned sz = EncodingSizeAM(id, GetCodeMR(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_AR_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum base, int32_t disp)
{
    assert(IsVexTernary(ins) && !EA_IS_GCREF_OR_BYREF(attr));

    instrDesc* id = NewInstrAMDisp(disp);
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    INDEBUG(id->idGCref(EA_GC_TYPE(attr)));
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idInsFmt(IF_AWR_RRD_RRD);
    id->idAddr()->iiaAddrMode.base  = base;
    id->idAddr()->iiaAddrMode.index = REG_NA;

    unsigned sz = EncodingSizeAM(id, GetCodeMR(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_A(instruction ins, emitAttr attr, RegNum reg, GenTree* addr)
{
    assert(!HasImplicitRegPairDest(ins) && (ins != INS_imuli));

    if (GenTreeConstAddr* constAddr = addr->IsConstAddr())
    {
        emitIns_R_C(ins, attr, reg, constAddr->GetData());
        return;
    }

    if (GenTreeLclAddr* lclAddr = addr->IsLclAddr())
    {
        assert(lclAddr->GetLcl()->IsAddressExposed());
        emitIns_R_S(ins, attr, reg, codeGen->GetStackAddrMode(lclAddr));

        return;
    }

    instrDesc* id = NewInstrAMDisp(GetAddrModeDisp(addr));
    id->idIns(ins);
    id->idInsFmt(MapFormat(ins, IF_RRD_ARD));
    id->idOpSize(EA_SIZE(attr));
    id->idGCref(EA_GC_TYPE(attr));
    id->idReg1(reg);
    SetInstrAddrMode(id, addr);

    unsigned sz = EncodingSizeAM(id, GetCodeRM(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_A_I(instruction ins, emitAttr attr, RegNum reg1, GenTree* addr, int32_t imm)
{
    assert(IsSSEOrAVXOrBMIInstruction(ins) || (ins == INS_imuli));
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));
    assert(!EA_IS_GCREF_OR_BYREF(attr));

    if (GenTreeConstAddr* constAddr = addr->IsConstAddr())
    {
        emitIns_R_C_I(ins, attr, reg1, constAddr->GetData(), imm);
        return;
    }

    X86_ONLY(noway_assert(VerifyEncodable(ins, attr, reg1)));

    instrDesc* id = NewInstrAMDispImm(GetAddrModeDisp(addr), imm);
    id->idIns(ins);
    id->idInsFmt(IF_RRW_ARD_CNS);
    id->idOpSize(EA_SIZE(attr));
    id->idGCref(EA_GC_TYPE(attr));
    X86_ONLY(id->idSetIsCnsReloc(EA_IS_CNS_RELOC(attr) && compiler->opts.compReloc));
    id->idReg1(reg1);
    SetInstrAddrMode(id, addr);

    unsigned sz = EncodingSizeAM(id, GetCodeRM(ins)) + ImmEncodingSize(ins, attr, imm);
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_C_I(instruction ins, emitAttr attr, RegNum reg1, ConstData* data, int32_t imm)
{
    assert(IsSSEOrAVXOrBMIInstruction(ins) || (ins == INS_imuli));
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));
    assert(!EA_IS_GCREF_OR_BYREF(attr));
    X86_ONLY(noway_assert(VerifyEncodable(ins, attr, reg1)));

    instrDesc* id = NewInstrCns(imm);
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    X86_ONLY(id->idSetIsCnsReloc(EA_IS_CNS_RELOC(attr) && compiler->opts.compReloc));
    id->idInsFmt(IF_RRW_MRD_CNS);
    id->idReg1(reg1);
    id->idSetIsDspReloc();
    id->SetConstData(data);

    unsigned sz = EncodingSizeCV(id, GetCodeRM(ins)) + ImmEncodingSize(ins, attr, imm);
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_S_I(instruction ins, emitAttr attr, RegNum reg1, StackAddrMode s, int32_t imm)
{
    assert(IsSSEOrAVXOrBMIInstruction(ins) || (ins == INS_imuli));
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));
    assert(!EA_IS_GCREF_OR_BYREF(attr));

    X86_ONLY(noway_assert(VerifyEncodable(ins, attr, reg1)));

    instrDesc* id = NewInstrCns(imm);
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    X86_ONLY(id->idSetIsCnsReloc(EA_IS_CNS_RELOC(attr) && compiler->opts.compReloc));
    id->idInsFmt(IF_RRW_SRD_CNS);
    id->idReg1(reg1);
    SetInstrLclAddrMode(id, s);

    unsigned sz = EncodingSizeSV(id, GetCodeRM(ins)) + ImmEncodingSize(ins, attr, imm);
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_R_A(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, GenTree* addr)
{
    if (GenTreeConstAddr* constAddr = addr->IsConstAddr())
    {
        emitIns_R_R_C(ins, attr, reg1, reg2, constAddr->GetData());
        return;
    }

    assert(IsVexTernary(ins) && !EA_IS_GCREF_OR_BYREF(attr));

    instrDesc* id = NewInstrAMDisp(GetAddrModeDisp(addr));
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_ARD);
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);
    SetInstrAddrMode(id, addr);

    unsigned sz = EncodingSizeAM(id, GetCodeRM(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

#ifdef DEBUG
static bool IsAVX2GatherInstruction(instruction ins)
{
    return (INS_FIRST_AVX2_GATHER_INSTRUCTION <= ins) && (ins <= INS_LAST_AVX2_GATHER_INSTRUCTION);
}
#endif

void X86Emitter::emitIns_R_AR_R(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum base, RegNum index, int scale, int32_t disp)
{
    assert(IsAVX2GatherInstruction(ins) && !EA_IS_GCREF_OR_BYREF(attr));

    instrDesc* id = NewInstrAMDisp(disp);
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idInsFmt(IF_RWR_ARD_RRD);
    id->idAddr()->iiaAddrMode.base  = base;
    id->idAddr()->iiaAddrMode.index = index;
    id->idAddr()->iiaAddrMode.scale = ScaleEncoding(scale);

    unsigned sz = EncodingSizeAM(id, GetCodeRM(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_R_C(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, ConstData* data)
{
    assert(IsVexTernary(ins) && !EA_IS_GCREF_OR_BYREF(attr));

    instrDesc* id = NewInstr();
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt(IF_RWR_RRD_MRD);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idSetIsDspReloc();
    id->SetConstData(data);

    unsigned sz = EncodingSizeCV(id, GetCodeRM(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3)
{
    assert(IsVexTernary(ins) && !EA_IS_GCREF_OR_BYREF(attr));

    instrDesc* id = NewInstr();
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt(IF_RWR_RRD_RRD);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idReg3(reg3);

    unsigned sz = EncodingSizeRRR(ins, EA_SIZE(attr), reg3);
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_R_S(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, StackAddrMode s)
{
    assert(IsVexTernary(ins) && !EA_IS_GCREF_OR_BYREF(attr));

    instrDesc* id = NewInstr();
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt(IF_RWR_RRD_SRD);
    id->idReg1(reg1);
    id->idReg2(reg2);
    SetInstrLclAddrMode(id, s);

    unsigned sz = EncodingSizeSV(id, GetCodeRM(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_R_A_I(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, GenTree* addr, int32_t imm)
{
    if (GenTreeConstAddr* constAddr = addr->IsConstAddr())
    {
        emitIns_R_R_C_I(ins, attr, reg1, reg2, constAddr->GetData(), imm);
        return;
    }

    assert(IsVexTernary(ins));
    assert(!EA_IS_CNS_RELOC(attr) && !EA_IS_GCREF_OR_BYREF(attr));
    assert(IsImm8(imm));

    instrDesc* id = NewInstrAMDispImm(GetAddrModeDisp(addr), imm);
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_ARD_CNS);
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);
    SetInstrAddrMode(id, addr);

    unsigned sz = EncodingSizeAM(id, GetCodeRM(ins)) + 1;
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_R_C_I(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, ConstData* data, int32_t imm)
{
    assert(IsVexTernary(ins));
    assert(!EA_IS_CNS_RELOC(attr) && !EA_IS_GCREF_OR_BYREF(attr));
    assert(IsImm8(imm));

    instrDesc* id = NewInstrCns(imm);
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt(IF_RWR_RRD_MRD_CNS);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idSetIsDspReloc();
    id->SetConstData(data);

    unsigned sz = EncodingSizeCV(id, GetCodeRM(ins)) + 1;
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_R_R_I(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, int32_t imm)
{
    assert(IsVexTernary(ins));
    assert(!EA_IS_CNS_RELOC(attr) && !EA_IS_GCREF_OR_BYREF(attr));
    assert(IsImm8(imm));

    instrDesc* id = NewInstrCns(imm);
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt(IF_RWR_RRD_RRD_CNS);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idReg3(reg3);

    unsigned sz = EncodingSizeRRR(ins, EA_SIZE(attr), reg3) + 1;
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_R_S_I(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, StackAddrMode s, int32_t imm)
{
    assert(IsVexTernary(ins));
    assert(!EA_IS_CNS_RELOC(attr) && !EA_IS_GCREF_OR_BYREF(attr));
    assert(IsImm8(imm));

    instrDesc* id = NewInstrCns(imm);
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt(IF_RWR_RRD_SRD_CNS);
    id->idReg1(reg1);
    id->idReg2(reg2);
    SetInstrLclAddrMode(id, s);

    unsigned sz = EncodingSizeSV(id, GetCodeRM(ins)) + 1;
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

// Encodes a XMM register into imm[7:4] for use by a SIMD instruction.
static int8_t EncodeXmmRegAsImm(RegNum reg)
{
    // AVX/AVX2 supports 4-reg format for vblendvps/vblendvpd/vpblendvb,
    // which encodes the fourth register into imm8[7:4]
    assert(reg >= REG_XMM0);

    int imm = (reg - REG_XMM0) << 4;
    assert((imm >= 0) && (imm <= 255));
    return static_cast<int8_t>(imm);
}

void X86Emitter::emitIns_R_R_A_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, GenTree* addr)
{
    assert(UseVEXEncoding());
    assert(IsAvxBlendv(ins));
    assert(!EA_IS_CNS_RELOC(attr) && !EA_IS_GCREF_OR_BYREF(attr));

    if (GenTreeConstAddr* constAddr = addr->IsConstAddr())
    {
        emitIns_R_R_C_R(MapSse41BlendvToAvxBlendv(ins), attr, reg1, reg2, reg3, constAddr->GetData());
        return;
    }

    instrDesc* id = NewInstrAMDispImm(GetAddrModeDisp(addr), EncodeXmmRegAsImm(reg3));
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_ARD_RRD);
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);
    SetInstrAddrMode(id, addr);

    unsigned sz = EncodingSizeAM(id, GetCodeRM(ins)) + 1;
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_R_C_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, ConstData* data)
{
    assert(UseVEXEncoding());
    assert(IsAvxBlendv(ins));
    assert(!EA_IS_CNS_RELOC(attr) && !EA_IS_GCREF_OR_BYREF(attr));

    instrDesc* id = NewInstrCns(EncodeXmmRegAsImm(reg3));
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idInsFmt(IF_RWR_RRD_MRD_RRD);
    id->idSetIsDspReloc();
    id->SetConstData(data);

    unsigned sz = EncodingSizeCV(id, GetCodeRM(ins)) + 1;
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_R_S_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, StackAddrMode s)
{
    assert(UseVEXEncoding());
    assert(IsAvxBlendv(ins));
    assert(!EA_IS_CNS_RELOC(attr) && !EA_IS_GCREF_OR_BYREF(attr));

    instrDesc* id = NewInstrCns(EncodeXmmRegAsImm(reg3));
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idInsFmt(IF_RWR_RRD_SRD_RRD);
    SetInstrLclAddrMode(id, s);

    unsigned sz = EncodingSizeSV(id, GetCodeRM(ins)) + 1;
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_R_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, RegNum reg4)
{
    assert(IsAvxBlendv(ins));
    assert(UseVEXEncoding());
    assert(!EA_IS_CNS_RELOC(attr) && !EA_IS_GCREF_OR_BYREF(attr));

    instrDesc* id = NewInstrCns(EncodeXmmRegAsImm(reg4));
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt(IF_RWR_RRD_RRD_RRD);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idReg3(reg3);

    unsigned sz = EncodingSizeRRR(ins, EA_SIZE(attr), reg3) + 1;
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_C(instruction ins, emitAttr attr, RegNum reg, ConstData* data)
{
    assert(!HasImplicitRegPairDest(ins) && (ins != INS_imuli));
    X86_ONLY(noway_assert(VerifyEncodable(ins, attr, reg)));

    instrDesc* id = NewInstr();
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt(MapFormat(ins, IF_RRD_MRD));
    id->idGCref(EA_GC_TYPE(attr));
    id->idReg1(reg);
    id->idSetIsDspReloc();
    id->SetConstData(data);

    unsigned sz;

#ifdef TARGET_X86
    // Special case: "mov eax, [addr]" is smaller.
    // This case is not enabled for amd64 as it always uses RIP relative addressing
    // and it results in smaller instruction size than encoding 64-bit addr in the
    // instruction.
    if ((ins == INS_mov) && (reg == REG_EAX))
    {
        sz = (attr == EA_2BYTE) + 1 + 4;
    }
    else
#endif
    {
        sz = EncodingSizeCV(id, GetCodeRM(ins));
    }

    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_C_R(instruction ins, emitAttr attr, ConstData* data, RegNum reg)
{
    X86_ONLY(noway_assert(VerifyEncodable(ins, attr, reg)));

    emitAttr size = EA_SIZE(attr);

#ifdef TARGET_X86
    // For x86 it is valid to storeind a double sized operand in an xmm reg to memory
    assert(size <= EA_8BYTE);
#else
    assert(size <= EA_PTRSIZE);
#endif

    instrDesc* id = NewInstr();
    id->idIns(ins);
    id->idOpSize(size);
    id->idInsFmt(MapFormat(ins, IF_MRD_RRD));
    id->idReg1(reg);
    id->idSetIsDspReloc();
    id->SetConstData(data);

    unsigned sz;

#ifdef TARGET_X86
    // Special case: "mov [addr], EAX" is smaller.
    // This case is not enable for amd64 as it always uses RIP relative addressing
    // and it will result in smaller instruction size than encoding 64-bit addr in
    // the instruction.
    if ((ins == INS_mov) && (reg == REG_EAX))
    {
        sz = (size == EA_2BYTE) + 1 + 4;
    }
    else
#endif // TARGET_X86
    {
        sz = EncodingSizeCV(id, GetCodeMR(ins));
    }

    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_C_I(instruction ins, emitAttr attr, ConstData* data, int32_t imm)
{
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));

    instrDesc* id = NewInstrCns(imm);
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    INDEBUG(id->idGCref(EA_GC_TYPE(attr)));
    X86_ONLY(id->idSetIsCnsReloc(EA_IS_CNS_RELOC(attr) && compiler->opts.compReloc));
    id->idInsFmt(MapFormat(ins, IF_MRD_CNS));
    id->idSetIsDspReloc();
    id->SetConstData(data);

    unsigned sz = EncodingSizeCV(id, GetCodeMI(ins)) + ImmEncodingSize(ins, attr, imm);
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_L(RegNum reg, insGroup* label)
{
#ifdef DEBUG
    if (codeGen->GetCurrentBlock()->bbJumpKind == BBJ_EHCATCHRET)
    {
        VerifyCatchRet(label);
    }
#endif

    instrDescJmp* id = NewInstrJmp();
#ifdef TARGET_X86
    id->idIns(INS_mov);
#else
    id->idIns(INS_lea);
#endif
    id->idOpSize(EA_PTRSIZE);
    id->idInsFmt(IF_RWR_LABEL);
    id->idReg1(reg);
    id->SetLabel(label);
    id->idSetIsCnsReloc(compiler->opts.compReloc AMD64_ONLY(&&InDifferentRegions(currentIG, label)));

#ifdef TARGET_X86
    unsigned sz = 1 + 4; // 0xB8 IMM32
#else
    unsigned sz = 1 + 1 + 1 + 4; // REX 0x8D RM DISP32
#endif
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

#ifdef TARGET_X86
void X86Emitter::emitIns_R_L(RegNum reg, ConstData* data)
{
    instrDescJmp* id = NewInstrJmp();
    id->idIns(INS_mov);
    id->idOpSize(EA_PTRSIZE);
    id->idInsFmt(IF_RWR_LABEL);
    id->idReg1(reg);
    id->SetConstData(data);
    id->idSetIsCnsReloc(compiler->opts.compReloc);

    unsigned sz = 1 + 4; // 0xB8 IMM32
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}
#endif // TARGET_X86

void X86Emitter::emitIns_R_AH(instruction ins, RegNum reg, void* addr)
{
#ifdef TARGET_X86
    assert(ins == INS_mov);
#else
    assert((ins == INS_mov) || (ins == INS_lea));
#endif
    assert(genIsValidIntReg(reg));

    instrDesc* id = NewInstrAMDisp(reinterpret_cast<ssize_t>(addr));
    id->idIns(ins);
    id->idOpSize(EA_PTRSIZE);
    id->idInsFmt(IF_RWR_ARD);
    id->idReg1(reg);
    id->idAddr()->iiaAddrMode.base  = REG_NA;
    id->idAddr()->iiaAddrMode.index = REG_NA;
    // On x64 RIP relative addressing is always used and that needs relocs.
    // TODO-MIKE-Review: Erm, normally RIP-relative does NOT need relocs...
    id->idSetIsDspReloc(X86_ONLY(compiler->opts.compReloc));

    unsigned sz = EncodingSizeAM(id, GetCodeRM(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_S_R_I(instruction ins, emitAttr attr, StackAddrMode s, RegNum reg, int32_t imm)
{
    assert(ins == INS_vextracti128 || ins == INS_vextractf128);
    assert(attr == EA_32BYTE);
    assert(reg != REG_NA);
    assert((imm == 0) || (imm == 1));

    instrDesc* id = NewInstrCns(imm);
    id->idIns(ins);
    id->idOpSize(EA_32BYTE);
    id->idInsFmt(IF_SWR_RRD_CNS);
    id->idReg1(reg);
    SetInstrLclAddrMode(id, s);

    unsigned sz = EncodingSizeSV(id, GetCodeMR(ins)) + 1;
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_A_R_I(instruction ins, emitAttr attr, GenTree* addr, RegNum reg, int32_t imm)
{
    if (GenTreeConstAddr* constAddr = addr->IsConstAddr())
    {
        emitIns_C_R_I(ins, attr, constAddr->GetData(), reg, imm);
        return;
    }

    assert((ins == INS_vextracti128) || (ins == INS_vextractf128));
    assert(attr == EA_32BYTE);
    assert(reg != REG_NA);
    assert((imm == 0) || (imm == 1));

    instrDesc* id = NewInstrAMDispImm(GetAddrModeDisp(addr), imm);
    id->idIns(ins);
    id->idInsFmt(IF_AWR_RRD_CNS);
    id->idOpSize(EA_32BYTE);
    id->idReg1(reg);
    SetInstrAddrMode(id, addr);

    unsigned size = EncodingSizeAM(id, GetCodeMR(ins)) + 1;
    id->idCodeSize(size);
    PrintInstr(id);
    currentIGCodeSize += size;
}

void X86Emitter::emitIns_C_R_I(instruction ins, emitAttr attr, ConstData* data, RegNum reg, int32_t imm)
{
    assert((ins == INS_vextracti128) || (ins == INS_vextractf128));
    assert(attr == EA_32BYTE);
    assert(reg != REG_NA);
    assert((imm == 0) || (imm == 1));

    instrDesc* id = NewInstrCns(imm);
    id->idIns(ins);
    id->idOpSize(EA_32BYTE);
    id->idInsFmt(IF_MWR_RRD_CNS);
    id->idReg1(reg);
    id->idSetIsDspReloc();
    id->SetConstData(data);

    unsigned size = EncodingSizeCV(id, GetCodeMR(ins)) + 1;
    id->idCodeSize(size);
    PrintInstr(id);
    currentIGCodeSize += size;
}

void X86Emitter::emitIns_ARX_I(
    instruction ins, emitAttr attr, RegNum base, RegNum index, unsigned scale, int32_t disp, int32_t imm)
{
    assert(!IsX87LdSt(ins) && (EA_SIZE(attr) <= EA_8BYTE));
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));

    instrDesc* id = NewInstrAMDispImm(disp, imm);
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    X86_ONLY(id->idSetIsCnsReloc(EA_IS_CNS_RELOC(attr) && compiler->opts.compReloc));
    id->idInsFmt(MapFormat(ins, IF_ARD_CNS));
    id->idAddr()->iiaAddrMode.base  = base;
    id->idAddr()->iiaAddrMode.index = index;
    id->idAddr()->iiaAddrMode.scale = ScaleEncoding(scale);

    unsigned sz = EncodingSizeAM(id, GetCodeMI(ins)) + ImmEncodingSize(ins, attr, imm);
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_AR(instruction ins, emitAttr attr, RegNum reg, RegNum base, int32_t disp)
{
    emitIns_R_ARX(ins, attr, reg, base, REG_NA, 1, disp);
}

void X86Emitter::emitIns_R_ARX(
    instruction ins, emitAttr attr, RegNum reg, RegNum base, RegNum index, unsigned scale, int32_t disp)
{
    assert(!IsX87LdSt(ins) && (EA_SIZE(attr) <= EA_32BYTE) && (reg != REG_NA));
    X86_ONLY(noway_assert(VerifyEncodable(ins, attr, reg)));

    if ((ins == INS_lea) && (reg == base) && (index == REG_NA) && (disp == 0))
    {
        // Maybe the X86Emitter is not the common place for this optimization, but it's a better choke point
        // for all the emitIns(ins, tree), we would have to be analyzing at each call site
        //
        return;
    }

    instrDesc* id = NewInstrAMDisp(disp);
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt(MapFormat(ins, IF_RRD_ARD));
    id->idGCref(EA_GC_TYPE(attr));
    id->idReg1(reg);
    id->idAddr()->iiaAddrMode.base  = base;
    id->idAddr()->iiaAddrMode.index = index;
    id->idAddr()->iiaAddrMode.scale = ScaleEncoding(scale);

    unsigned sz = EncodingSizeAM(id, GetCodeRM(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_ARX(instruction ins, emitAttr attr, RegNum base, RegNum index, unsigned scale, int32_t disp)
{
    emitIns_ARX_R(ins, attr, REG_NA, base, index, scale, disp);
}

void X86Emitter::emitIns_AR_R(instruction ins, emitAttr attr, RegNum reg, RegNum base, int32_t disp)
{
    emitIns_ARX_R(ins, attr, reg, base, REG_NA, 1, disp);
}

void X86Emitter::emitIns_ARX_R(
    instruction ins, emitAttr attr, RegNum reg, RegNum base, RegNum index, unsigned scale, int32_t disp)
{
    assert(!IsReallyVexTernary(ins));

    instrDesc* id = NewInstrAMDisp(disp);
    insFormat  fmt;

    if (reg == REG_NA)
    {
        fmt = MapFormat(ins, IF_ARD);
    }
    else
    {
        fmt = MapFormat(ins, IF_ARD_RRD);

        X86_ONLY(noway_assert(VerifyEncodable(ins, attr, reg)));
        assert(!IsX87LdSt(ins) && (EA_SIZE(attr) <= EA_32BYTE));

        id->idReg1(reg);
    }

    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idGCref(EA_GC_TYPE(attr));
    id->idInsFmt(fmt);
    id->idAddr()->iiaAddrMode.base  = base;
    id->idAddr()->iiaAddrMode.index = index;
    id->idAddr()->iiaAddrMode.scale = ScaleEncoding(scale);

    unsigned sz = EncodingSizeAM(id, GetCodeMR(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;

#if !FEATURE_FIXED_OUT_ARGS
    UpdateStackLevel(ins);
#endif
}

void X86Emitter::emitIns_SIMD_R_R_I(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, int32_t imm)
{
    if (UseVEXEncoding() || IsSseDstSrcImm(ins))
    {
        emitIns_R_R_I(ins, attr, reg1, reg2, imm);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_I(ins, attr, reg1, imm);
    }
}

void X86Emitter::emitIns_SIMD_R_R_A(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, GenTree* addr)
{
    if (GenTreeConstAddr* constAddr = addr->IsConstAddr())
    {
        emitIns_SIMD_R_R_C(ins, attr, reg1, reg2, constAddr->GetData());
    }
    else if (UseVEXEncoding())
    {
        emitIns_R_R_A(ins, attr, reg1, reg2, addr);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_A(ins, attr, reg1, addr);
    }
}

void X86Emitter::emitIns_SIMD_R_R_C(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, ConstData* data)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_C(ins, attr, reg1, reg2, data);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_C(ins, attr, reg1, data);
    }
}

void X86Emitter::emitIns_SIMD_R_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_R(ins, attr, reg1, reg2, reg3);
    }
    else
    {
        // Ensure we aren't overwriting op2
        assert((reg3 != reg1) || (reg2 == reg1));

        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);

        if (IsMovIns(ins))
        {
            emitIns_Mov(ins, attr, reg1, reg3, /* canSkip */ false);
        }
        else
        {
            emitIns_R_R(ins, attr, reg1, reg3);
        }
    }
}

void X86Emitter::emitIns_SIMD_R_R_S(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, StackAddrMode s)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_S(ins, attr, reg1, reg2, s);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_S(ins, attr, reg1, s);
    }
}

void X86Emitter::emitIns_SIMD_R_R_A_I(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, GenTree* addr, int32_t imm)
{
    if (GenTreeConstAddr* constAddr = addr->IsConstAddr())
    {
        emitIns_SIMD_R_R_C_I(ins, attr, reg1, reg2, constAddr->GetData(), imm);
    }
    else if (UseVEXEncoding())
    {
        emitIns_R_R_A_I(ins, attr, reg1, reg2, addr, imm);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_A_I(ins, attr, reg1, addr, imm);
    }
}

void X86Emitter::emitIns_SIMD_R_R_C_I(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, ConstData* data, int32_t imm)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_C_I(ins, attr, reg1, reg2, data, imm);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_C_I(ins, attr, reg1, data, imm);
    }
}

void X86Emitter::emitIns_SIMD_R_R_R_I(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, int32_t imm)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_R_I(ins, attr, reg1, reg2, reg3, imm);
    }
    else
    {
        // Ensure we aren't overwriting op2
        assert((reg3 != reg1) || (reg2 == reg1));

        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_R_I(ins, attr, reg1, reg3, imm);
    }
}

void X86Emitter::emitIns_SIMD_R_R_S_I(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, StackAddrMode s, int32_t imm)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_S_I(ins, attr, reg1, reg2, s, imm);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_S_I(ins, attr, reg1, s, imm);
    }
}

void X86Emitter::emitIns_SIMD_R_R_R_A(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, GenTree* addr)
{
    if (GenTreeConstAddr* constAddr = addr->IsConstAddr())
    {
        emitIns_SIMD_R_R_R_C(ins, attr, reg1, reg2, reg3, constAddr->GetData());
        return;
    }

    assert(IsFMAInstruction(ins) || IsAVXVNNIInstruction(ins));
    assert(UseVEXEncoding());

    // Ensure we aren't overwriting op2
    assert((reg3 != reg1) || (reg2 == reg1));

    emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
    emitIns_R_R_A(ins, attr, reg1, reg3, addr);
}

void X86Emitter::emitIns_SIMD_R_R_R_C(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, ConstData* data)
{
    assert(IsFMAInstruction(ins));
    assert(UseVEXEncoding());

    // Ensure we aren't overwriting op2
    assert((reg3 != reg1) || (reg2 == reg1));

    emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
    emitIns_R_R_C(ins, attr, reg1, reg3, data);
}

void X86Emitter::emitIns_SIMD_R_R_R_R(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, RegNum reg4)
{
    if (IsFMAInstruction(ins) || IsAVXVNNIInstruction(ins))
    {
        assert(UseVEXEncoding());

        // Ensure we aren't overwriting op2 or op3
        assert((reg3 != reg1) || (reg2 == reg1));
        assert((reg4 != reg1) || (reg2 == reg1));

        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_R_R(ins, attr, reg1, reg3, reg4);
    }
    else if (UseVEXEncoding())
    {
        emitIns_R_R_R_R(MapSse41BlendvToAvxBlendv(ins), attr, reg1, reg2, reg3, reg4);
    }
    else
    {
        assert(IsSse41Blendv(ins));

        // Ensure we aren't overwriting op1 or op2
        assert((reg2 != REG_XMM0) || (reg4 == REG_XMM0));
        assert((reg3 != REG_XMM0) || (reg4 == REG_XMM0));

        // SSE4.1 blendv* hardcode the mask vector (op3) in XMM0
        emitIns_Mov(INS_movaps, attr, REG_XMM0, reg4, /* canSkip */ true);

        // Ensure we aren't overwriting op2 or oop3 (which should be REG_XMM0)
        assert((reg3 != reg1) || (reg2 == reg1));
        assert(reg1 != REG_XMM0);

        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_R(ins, attr, reg1, reg3);
    }
}

void X86Emitter::emitIns_SIMD_R_R_R_S(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, StackAddrMode s)
{
    assert(IsFMAInstruction(ins) || IsAVXVNNIInstruction(ins));
    assert(UseVEXEncoding());

    // Ensure we aren't overwriting op2
    assert((reg3 != reg1) || (reg2 == reg1));

    emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
    emitIns_R_R_S(ins, attr, reg1, reg3, s);
}

void X86Emitter::emitIns_SIMD_R_R_A_R(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, GenTree* addr)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_A_R(MapSse41BlendvToAvxBlendv(ins), attr, reg1, reg2, reg3, addr);
    }
    else
    {
        assert(IsSse41Blendv(ins));
        assert((reg1 != REG_XMM0) && (reg2 != REG_XMM0));

        emitIns_Mov(INS_movaps, attr, REG_XMM0, reg3, /* canSkip */ true);
        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_A(ins, attr, reg1, addr);
    }
}

void X86Emitter::emitIns_SIMD_R_R_S_R(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, StackAddrMode s)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_S_R(MapSse41BlendvToAvxBlendv(ins), attr, reg1, reg2, reg3, s);
    }
    else
    {
        assert(IsSse41Blendv(ins));
        assert((reg1 != REG_XMM0) && (reg2 != REG_XMM0));

        emitIns_Mov(INS_movaps, attr, REG_XMM0, reg3, /* canSkip */ true);
        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_S(ins, attr, reg1, s);
    }
}

void X86Emitter::emitIns_S(instruction ins, emitAttr attr, StackAddrMode s)
{
    instrDesc* id = NewInstr();
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idGCref(EA_GC_TYPE(attr));
    id->idInsFmt(MapFormat(ins, IF_SRD));
    SetInstrLclAddrMode(id, s);

    unsigned sz = EncodingSizeSV(id, GetCodeMR(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;

#if !FEATURE_FIXED_OUT_ARGS
    UpdateStackLevel(ins);
#endif
}

void X86Emitter::emitIns_S_R(instruction ins, emitAttr attr, RegNum reg, StackAddrMode s)
{
    assert(!IsReallyVexTernary(ins));
    X86_ONLY(assert((attr != EA_1BYTE) || isByteReg(reg)));

    instrDesc* id = NewInstr();
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt(MapFormat(ins, IF_SRD_RRD));
    INDEBUG(id->idGCref(EA_GC_TYPE(attr)));
    id->idReg1(reg);
    SetInstrLclAddrMode(id, s);

    unsigned sz = EncodingSizeSV(id, GetCodeMR(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_R_S(instruction ins, emitAttr attr, RegNum reg, StackAddrMode s)
{
    assert(!HasImplicitRegPairDest(ins) && (ins != INS_imuli));
    X86_ONLY(noway_assert(VerifyEncodable(ins, attr, reg)));

    instrDesc* id = NewInstr();
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    id->idInsFmt(MapFormat(ins, IF_RRD_SRD));
    id->idGCref(EA_GC_TYPE(attr));
    id->idReg1(reg);
    SetInstrLclAddrMode(id, s);

    unsigned sz = EncodingSizeSV(id, GetCodeRM(ins));
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

void X86Emitter::emitIns_S_I(instruction ins, emitAttr attr, StackAddrMode s, int32_t imm)
{
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));

    instrDesc* id = NewInstrCns(imm);
    id->idIns(ins);
    id->idOpSize(EA_SIZE(attr));
    X86_ONLY(id->idSetIsCnsReloc(EA_IS_CNS_RELOC(attr) && compiler->opts.compReloc));
    id->idInsFmt(MapFormat(ins, IF_SRD_CNS));
    INDEBUG(id->idGCref(EA_GC_TYPE(attr)));
    SetInstrLclAddrMode(id, s);

    unsigned sz = EncodingSizeSV(id, GetCodeMI(ins)) + ImmEncodingSize(ins, attr, imm);
    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

#define JMP_JCC_SIZE_SMALL (2)
#define JMP_SIZE_LARGE (5)
#define JCC_SIZE_LARGE (6)
#define PUSH_INST_SIZE (5)
#define CALL_INST_SIZE (5)

#ifdef TARGET_X86
void X86Emitter::emitIns_L(instruction ins, insGroup* label)
{
    assert(ins == INS_push_hide);
    assert(label != nullptr);

    instrDescJmp* id = NewInstrJmp();
    id->idIns(ins);
    id->idOpSize(EA_4BYTE);
    id->idInsFmt(IF_LABEL);
    id->idSetIsCnsReloc(compiler->opts.compReloc);
    id->SetLabel(label);

    id->idCodeSize(PUSH_INST_SIZE);
    PrintInstr(id);
    currentIGCodeSize += PUSH_INST_SIZE;
}
#endif // TARGET_X86

#ifdef TARGET_AMD64
void X86Emitter::emitIns_CallFinally(insGroup* label)
{
    assert(codeGen->GetCurrentBlock()->bbJumpKind == BBJ_CALLFINALLY);
    INDEBUG(VerifyCallFinally(label));

    instrDescJmp* id = NewInstrJmp();
    id->idSetIsCnsReloc(compiler->opts.compReloc && InDifferentRegions(currentIG, label));
    id->idIns(INS_call);
    id->idInsFmt(IF_LABEL);
    id->SetLabel(label);

    id->idCodeSize(CALL_INST_SIZE);
    PrintInstr(id);
    currentIGCodeSize += CALL_INST_SIZE;
}
#endif // TARGET_AMD64

void X86Emitter::emitIns_J(instruction ins, int instrCount)
{
    assert(IsMainProlog(currentIG));
    assert(IsJccInstruction(ins));
    assert(instrCount < 0);

    instrDescJmp* id = NewInstrJmp();
    id->idIns(ins);
    id->idInsFmt(IF_LABEL);
    id->SetInstrCount(instrCount);

    id->idCodeSize(JMP_JCC_SIZE_SMALL);
    PrintInstr(id);
    currentIGCodeSize += JMP_JCC_SIZE_SMALL;
}

void X86Emitter::emitIns_J(instruction ins, insGroup* label)
{
    assert((ins == INS_jmp) || IsJccInstruction(ins));
    assert(currentIG->GetFuncletIndex() == label->GetFuncletIndex());

    instrDescJmp* id = NewInstrJmp();
    id->idIns(ins);
    id->idInsFmt(IF_LABEL);
    id->SetLabel(label);
    id->idSetIsCnsReloc(compiler->opts.compReloc && InDifferentRegions(currentIG, label));

    unsigned sz = ins == INS_jmp ? JMP_SIZE_LARGE : JCC_SIZE_LARGE;

    if (label->IsDefined() && !id->idIsCnsReloc())
    {
        // This is a backward jump, we can determine now if it's going to be short.

        uint32_t jumpOffs = currentCodeOffset + currentIGCodeSize + JMP_JCC_SIZE_SMALL;
        int32_t  distance = jumpOffs - label->igOffs;
        int32_t  overflow = distance + -128;

        assert(distance > 0);

        if (overflow <= 0)
        {
            sz = JMP_JCC_SIZE_SMALL;
        }
    }

    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;
}

ssize_t instrDesc::GetImm() const
{
    return _idLargeCns ? static_cast<const instrDescCns*>(this)->idcCnsVal : _idSmallCns;
}

ssize_t instrDesc::GetMemDisp() const
{
    if (!_idLargeDsp)
    {
        return 0;
    }
    else if (_idLargeCns)
    {
        return static_cast<const instrDescCnsAmd*>(this)->idacAmdVal;
    }
    else
    {
        return static_cast<const instrDescAmd*>(this)->idaAmdVal;
    }
}

ssize_t instrDesc::GetAmDisp() const
{
    if (!_idLargeDsp)
    {
        return idAddr()->iiaAddrMode.disp;
    }
    else if (_idLargeCns)
    {
        return static_cast<const instrDescCnsAmd*>(this)->idacAmdVal;
    }
    else
    {
        return static_cast<const instrDescAmd*>(this)->idaAmdVal;
    }
}

ssize_t instrDesc::GetCallDisp() const
{
    if (_idLargeCall)
    {
        return static_cast<const instrDescCGCA*>(this)->idcDisp;
    }
    else
    {
        assert(!_idLargeDsp);
        assert(!_idLargeCns);

        return idAddr()->iiaAddrMode.disp;
    }
}

#if !FEATURE_FIXED_OUT_ARGS
void X86Emitter::SetStackLevel(unsigned level)
{
    assert((level != 0) && (level < INT32_MAX) && (level % REGSIZE_BYTES == 0));
    assert(stackLevel == 0);
    assert(currentIG->igStkLvl == 0);
    assert(instrBufferFree == instrBufferBase);

    stackLevel = currentIG->igStkLvl = level;

    if (maxStackDepth < stackLevel)
    {
        maxStackDepth = stackLevel;
        JITDUMP("Max stack depth: %u\n", maxStackDepth);
    }
}

void X86Emitter::UpdateStackLevel(instruction ins)
{
    if (ins == INS_push)
    {
        stackLevel += stackSlotSize;

        if (maxStackDepth < stackLevel)
        {
            maxStackDepth = stackLevel;
            JITDUMP("Max stack depth: %u\n", maxStackDepth);
        }
    }
    else if (ins == INS_pop)
    {
        stackLevel -= stackSlotSize;
        assert(stackLevel < INT32_MAX);
    }
}

void X86Emitter::UpdateStackLevel(instruction ins, ssize_t val)
{
    // If we're in the prolog or epilog, or otherwise not tracking the stack depth, just return.
    if (stackSlotSize == 0)
    {
        return;
    }

    if (ins == INS_sub)
    {
        S_UINT32 newStackLvl(stackLevel);
        newStackLvl += S_UINT32(val);
        noway_assert(!newStackLvl.IsOverflow());

        stackLevel = newStackLvl.Value();

        if (maxStackDepth < stackLevel)
        {
            maxStackDepth = stackLevel;
            JITDUMP("Max stack depth: %u\n", maxStackDepth);
        }
    }
    else if (ins == INS_add)
    {
        S_UINT32 newStackLvl = S_UINT32(stackLevel) - S_UINT32(val);
        noway_assert(!newStackLvl.IsOverflow());

        stackLevel = newStackLvl.Value();
    }
}

#endif // !FEATURE_FIXED_OUT_ARGS

// Add a call instruction (direct or indirect).
//
// argSize < 0 means that the caller will pop the arguments
//
// EC_FUNC_TOKEN       : addr is the method address
// EC_FUNC_TOKEN_INDIR : addr is the indirect method address
// EC_INDIR_R          : call ireg (addr has to be null)
// EC_INDIR_ARD        : call [ireg + xreg * xmul + disp] (addr has to be null)
//
void X86Emitter::emitIns_Call(EmitCallType          kind,
                              CORINFO_METHOD_HANDLE methodHandle,
#ifdef DEBUG
                              CORINFO_SIG_INFO* sigInfo,
#endif
                              void* addr,
#ifdef TARGET_X86
                              int32_t argSize,
#endif
                              emitAttr retRegAttr,
#ifdef UNIX_AMD64_ABI
                              emitAttr retReg2Attr,
#endif
                              RegNum   amBase,
                              RegNum   amIndex,
                              unsigned amScale,
                              int32_t  amDisp,
                              bool     isJump)
{
    assert((kind != EC_FUNC_TOKEN && kind != EC_FUNC_TOKEN_INDIR) ||
           (amBase == REG_NA && amIndex == REG_NA && amScale == 0 && amDisp == 0));
    assert(kind < EC_INDIR_R || kind == EC_INDIR_ARD || addr == nullptr);
    assert(kind != EC_INDIR_R || (genIsValidIntReg(amBase) && amIndex == REG_NA && amScale == 0 && amDisp == 0));

#ifdef TARGET_X86
    // Our stack level should be always greater than the bytes of arguments we push.
    assert(static_cast<unsigned>(abs(argSize)) <= codeGen->genStackLevel);
    assert(argSize % REGSIZE_BYTES == 0);

    int32_t argSlotCount = argSize / REGSIZE_BYTES;
#endif

    instrDesc* id = NewInstrCall(methodHandle, retRegAttr,
#ifdef UNIX_AMD64_ABI
                                 retReg2Attr,
#endif
#ifdef TARGET_X86
                                 argSlotCount,
#endif
                                 (kind == EC_INDIR_ARD) ? amDisp : 0);

    if (!isJump)
    {
        id->idIns(INS_call);
    }
    else if (kind == EC_FUNC_TOKEN)
    {
        id->idIns(INS_l_jmp);
    }
    else
    {
        assert((kind == EC_FUNC_TOKEN_INDIR) || (kind == EC_INDIR_ARD));

        id->idIns(INS_i_jmp);
    }

    unsigned sz;

    if (kind == EC_INDIR_R)
    {
        id->idInsFmt(IF_RRD);
        // Move the GC regs info to the unused address mode bits.
        id->idAddr()->iiaAddrMode.base  = id->idReg1();
        id->idAddr()->iiaAddrMode.index = id->idReg2();
        id->idReg1(amBase);

        sz = 2 + IsExtendedReg(amBase);
    }
    else if (kind == EC_INDIR_ARD)
    {
        assert(amBase != REG_NA);

        id->idInsFmt(IF_ARD);
        id->idAddr()->iiaAddrMode.base  = amBase;
        id->idAddr()->iiaAddrMode.index = amIndex;
        id->idAddr()->iiaAddrMode.scale = ScaleEncoding(amScale);

        sz = EncodingSizeAM(id, GetCodeMR(INS_call));
    }
    else if (kind == EC_FUNC_TOKEN_INDIR)
    {
        assert(addr != nullptr);

        id->idInsFmt(IF_METHPTR);
        id->SetAddr(addr);

        sz = 6;

        // Since this is an indirect call through a pointer and we don't
        // currently pass in emitAttr into this function, we query codegen
        // whether addr needs a reloc.
        if (compiler->opts.compReloc AMD64_ONLY(|| !IsImm32(reinterpret_cast<intptr_t>(addr))))
        {
            id->idSetIsDspReloc();
        }
        else
        {
            // This addr mode requires an extra SIB byte.
            AMD64_ONLY(sz++);
        }
    }
    else
    {
        assert(kind == EC_FUNC_TOKEN);
        assert(addr != nullptr);

        id->idInsFmt(IF_METHOD);
        id->SetAddr(addr);

        // Direct call to a method and no addr indirection is needed.
        // On x64 all direct code addresses go through relocation so that VM will
        // setup a jump stub if addr cannot be encoded as RIP relative offset.
        X86_ONLY(if (compiler->opts.compReloc))
        {
            id->idSetIsDspReloc();
        }

        sz = 5;
    }

#ifdef DEBUG
    id->idDebugOnlyInfo()->idHandle  = methodHandle;
    id->idDebugOnlyInfo()->idCallSig = sigInfo;
#endif

    id->idCodeSize(sz);
    PrintInstr(id);
    currentIGCodeSize += sz;

#ifdef LATE_DISASM
    if (addr != nullptr)
    {
        AddDisasmMethodAddr(reinterpret_cast<size_t>(addr), methodHandle);
    }
#endif

#if !FEATURE_FIXED_OUT_ARGS
    if (stackSlotSize && (argSize > 0))
    {
        noway_assert(argSize <= static_cast<int32_t>(stackLevel));

        stackLevel -= argSize;
        assert(stackLevel <= INT_MAX);
    }
#endif
}

void EmitterBase::EncodeCallGCRegs(regMaskTP regs, instrDesc* id)
{
    static_assert_no_msg(instrDesc::RegBits >= 4);
    assert((regs & RBM_CALLEE_TRASH) == RBM_NONE);

    unsigned encoded = 0;

    if ((regs & RBM_ESI) != RBM_NONE)
        encoded |= 0x01;
    if ((regs & RBM_EDI) != RBM_NONE)
        encoded |= 0x02;
    if ((regs & RBM_EBX) != RBM_NONE)
        encoded |= 0x04;
#ifdef TARGET_AMD64
    if ((regs & RBM_RBP) != RBM_NONE)
        encoded |= 0x08;
#endif

    id->idReg1(static_cast<RegNum>(encoded));

#ifdef TARGET_AMD64
    encoded = 0;

    if ((regs & RBM_R12) != RBM_NONE)
        encoded |= 0x01;
    if ((regs & RBM_R13) != RBM_NONE)
        encoded |= 0x02;
    if ((regs & RBM_R14) != RBM_NONE)
        encoded |= 0x04;
    if ((regs & RBM_R15) != RBM_NONE)
        encoded |= 0x08;

    id->idReg2(static_cast<RegNum>(encoded));
#endif
}

unsigned EmitterBase::DecodeCallGCRegs(instrDesc* id)
{
    unsigned encoded;

    if (id->idInsFmt() == IF_RRD)
    {
        encoded = id->idAddr()->iiaAddrMode.base | (id->idAddr()->iiaAddrMode.index << 8);
    }
    else
    {
        assert((id->idInsFmt() == IF_ARD) || (id->idInsFmt() == IF_METHOD) || (id->idInsFmt() == IF_METHPTR));

        encoded = id->idReg1() | (id->idReg2() << 8);
    }

    unsigned regs = 0;

    if ((encoded & 0x01) != 0)
        regs |= RBM_ESI;
    if ((encoded & 0x02) != 0)
        regs |= RBM_EDI;
    if ((encoded & 0x04) != 0)
        regs |= RBM_EBX;
#ifdef TARGET_AMD64
    if ((encoded & 0x08) != 0)
        regs |= RBM_RBP;

    if ((encoded & 0x0100) != 0)
        regs |= RBM_R12;
    if ((encoded & 0x0200) != 0)
        regs |= RBM_R13;
    if ((encoded & 0x0400) != 0)
        regs |= RBM_R14;
    if ((encoded & 0x0800) != 0)
        regs |= RBM_R15;
#endif

    return regs;
}

void EmitterBase::ShortenBranches()
{
    if (firstJump == nullptr)
    {
        return;
    }

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("\nInstruction groups before jump shortening:\n\n");
        PrintIGList(true);
    }
#endif

AGAIN:
    INDEBUG(VerifyIGOffsets());

    uint32_t      minDistanceOverflow  = UINT32_MAX;
    uint32_t      totalSizeReduction   = 0;
    uint32_t      instrIGSizeReduction = 0;
    instrDescJmp* previousInstr        = nullptr;
    insGroup*     previousInstrIG      = firstJump->idjIG;

    for (instrDescJmp *instr = firstJump; instr != nullptr; previousInstr = instr, instr = instr->idjNext)
    {
        insGroup* instrIG = instr->idjIG;

        if (previousInstrIG == instrIG)
        {
            instr->idjOffs -= instrIGSizeReduction;

            assert((previousInstr == nullptr) || (instr->idjOffs >= previousInstr->idjOffs));
        }
        else
        {
            instrIGSizeReduction = 0;

            for (insGroup* ig = previousInstrIG->igNext; ig != instrIG->igNext; ig = ig->igNext)
            {
                JITDUMP(FMT_IG " moved back from %04X", ig->GetId(), ig->igOffs);
                ig->igOffs -= totalSizeReduction;
                JITDUMP(" to % 04X\n", ig->igOffs);
            }

            assert(instrIG->igOffs >= previousInstrIG->igOffs);

            previousInstrIG = instrIG;
        }

        if (!IsJccInstruction(instr->idIns()) && (instr->idIns() != INS_jmp))
        {
            continue;
        }

        if (instr->idIsCnsReloc() || !instr->HasLabel())
        {
            continue;
        }

        uint32_t currentSize = instr->idCodeSize();

        if (currentSize == 0)
        {
            continue;
        }

        uint32_t  instrOffs    = instrIG->igOffs + instr->idjOffs;
        uint32_t  instrEndOffs = instrOffs + JMP_JCC_SIZE_SMALL;
        insGroup* label        = instr->GetLabel();
        uint32_t  labelOffs    = label->igOffs;
        int32_t   distanceOverflow;

        if (label->igNum > instrIG->igNum)
        {
            labelOffs -= totalSizeReduction;

            distanceOverflow = (labelOffs - instrEndOffs) - 127;
        }
        else
        {
            distanceOverflow = (instrEndOffs - labelOffs) - 128;
        }

        JITDUMP("Jump IN%04X from %04X +%u (" FMT_IG ") to %04X (" FMT_IG "), distance %d, overflow %d%s\n",
                instr->idDebugOnlyInfo()->idNum, instrOffs, JMP_JCC_SIZE_SMALL, instrIG->GetId(), labelOffs,
                label->GetId(), labelOffs - instrEndOffs, distanceOverflow, distanceOverflow <= 0 ? ", short" : "");

        if (distanceOverflow > 0)
        {
            minDistanceOverflow = Min(minDistanceOverflow, static_cast<uint32_t>(distanceOverflow));

            continue;
        }

        unsigned newSize;

        if (instrOffs + currentSize == labelOffs)
        {
            // Removing a "jump to next" could produce another "jump to next", we need to force another pass
            // to eliminate that too. Ideally we'd traverse the jump list backwards, but it's a forward only
            // list and given the rarity of such nested jumps it's hard to justify the extra code and memory
            // required to traverse the list both ways.
            minDistanceOverflow = 0;

            newSize = 0;
        }
        else if (currentSize > JMP_JCC_SIZE_SMALL)
        {
            newSize = JMP_JCC_SIZE_SMALL;
        }
        else
        {
            continue;
        }

        instr->idCodeSize(newSize);

        uint32_t sizeReduction = currentSize - newSize;
        instrIG->igSize -= static_cast<uint16_t>(sizeReduction);
        instrIG->igFlags |= IGF_UPD_ISZ;
        instrIGSizeReduction += sizeReduction;
        totalSizeReduction += sizeReduction;
    }

    if (totalSizeReduction != 0)
    {
        for (insGroup* ig = previousInstrIG->igNext; ig != nullptr; ig = ig->igNext)
        {
            JITDUMP(FMT_IG " moved back from %04X", ig->GetId(), ig->igOffs);
            ig->igOffs -= totalSizeReduction;
            JITDUMP(" to % 04X\n", ig->igOffs);
        }

        JITDUMP("Total size reduction %u, min distance overflow %u\n", totalSizeReduction, minDistanceOverflow);

        if (minDistanceOverflow <= totalSizeReduction)
        {
            JITDUMP("Iterating branch shortening\n");

            goto AGAIN;
        }

        INDEBUG(VerifyIGOffsets());
    }

#ifdef DEBUG
    if (compiler->verbose)
    {
        printf("\nLabels list after the jump shortening:\n\n");
        PrintIGList(false);
    }
#endif
}

enum ID_OPS : uint8_t
{
    ID_OP_NONE,    // no additional arguments
    ID_OP_CNS,     // constant     operand
    ID_OP_DSP,     // displacement operand
    ID_OP_DSP_CNS, // displacement + constant
    ID_OP_AMD,     // addrmode with dsp
    ID_OP_AMD_CNS, // addrmode with dsp + constant
    ID_OP_JMP,     // local jump
    ID_OP_CALL,    // direct method call
};

static ID_OPS GetFormatOp(insFormat format)
{
    static const ID_OPS ops[]{
#define IF_DEF(en, op1, op2, ...) ID_OP_##op2,
#include "emitfmtsxarch.h"
    };

    assert(format < _countof(ops));
    return ops[format];
}

#ifdef DEBUG
void EmitterBase::VerifyInstr(instrDesc* id)
{
    // make certain you only try to put relocs on things that can have them.

    ID_OPS idOp = GetFormatOp(id->idInsFmt());

    if (id->idIsDspReloc())
    {
        assert(idOp == ID_OP_NONE || idOp == ID_OP_AMD || idOp == ID_OP_DSP || idOp == ID_OP_DSP_CNS ||
               idOp == ID_OP_AMD_CNS || idOp == ID_OP_CALL || idOp == ID_OP_JMP);
    }

    if (id->idIsCnsReloc())
    {
        assert(idOp == ID_OP_CNS || idOp == ID_OP_AMD_CNS || idOp == ID_OP_DSP_CNS || idOp == ID_OP_CALL ||
               idOp == ID_OP_JMP);
    }
}
#endif

size_t instrDescSmall::GetDescSize() const
{
    if (_idSmallDsc)
    {
        return sizeof(instrDescSmall);
    }

    if (_idLargeCall)
    {
        return sizeof(instrDescCGCA);
    }

    switch (GetFormatOp(_idInsFmt))
    {
        case ID_OP_NONE:
            if (_idIns == INS_align)
            {
                return sizeof(instrDescAlign);
            }
            break;

        case ID_OP_JMP:
            return sizeof(instrDescJmp);

        case ID_OP_CALL:
        case ID_OP_DSP:
        case ID_OP_DSP_CNS:
        case ID_OP_AMD:
        case ID_OP_AMD_CNS:
            if (_idLargeCns && _idLargeDsp)
            {
                return sizeof(instrDescCnsAmd);
            }
            FALLTHROUGH;
        case ID_OP_CNS:
            if (_idLargeDsp || _idLargeCns)
            {
                static_assert_no_msg(sizeof(instrDescAmd) == sizeof(instrDescCns));
                return sizeof(instrDescCns);
            }
            break;

        default:
            unreached();
    }

    return sizeof(instrDesc);
}

#ifdef DEBUG

static void PrintHexCode(const uint8_t* code, size_t size)
{
    constexpr size_t minSize = 6 AMD64_ONLY(+4);
    constexpr size_t maxSize = 15;

    assert(size <= maxSize);
    char  buffer[1 + maxSize * 2 + 1];
    char* p = buffer;
    *p++    = ' ';

    static const char digits[] = "0123456789ABCDEF";

    for (size_t i = 0; i < size; i++)
    {
        *p++ = digits[code[i] >> 4];
        *p++ = digits[code[i] & 15];
    }

    while (size < minSize)
    {
        *p++ = ' ';
        *p++ = ' ';
        size++;
    }

    *p++ = 0;

    printf("%s", buffer);
}

class X86AsmPrinter final : public AsmPrinter
{
    const bool asmfm;
    const bool useVex;
#if !FEATURE_FIXED_OUT_ARGS
    int stackLevel = 0;
#endif

public:
    X86AsmPrinter(Compiler* compiler, CodeGen* codeGen, bool asmfm, bool useVex)
        : AsmPrinter(compiler, codeGen), asmfm(asmfm), useVex(useVex)
    {
    }

    void Print(instrDesc* id) const
    {
        PrintInstr(id);
    }

#if !FEATURE_FIXED_OUT_ARGS
    void SetStackLevel(int level)
    {
        stackLevel = level;
    }
#endif

private:
    static const char* GetSizeOperator(emitAttr attr)
    {
        switch (attr)
        {
            case EA_UNKNOWN:
                return "";
            case EA_1BYTE:
                return "byte ptr ";
            case EA_2BYTE:
                return "word ptr ";
            case EA_4BYTE:
                return "dword ptr ";
            case EA_8BYTE:
                return "qword ptr ";
            case EA_16BYTE:
                return "xmmword ptr ";
            case EA_32BYTE:
                return "ymmword ptr ";
            case EA_GCREF:
                return "gword ptr ";
            case EA_BYREF:
                return "bword ptr ";
            default:
                return "??? ";
        }
    }

    void PrintConstDataLabel(instrDesc* id, emitAttr size) const
    {
        ConstData* data = id->GetConstData();

        printf("%s[RWD%02u", GetSizeOperator(size), data->offset);

        if (ssize_t offs = id->GetMemDisp())
        {
            printf("%+Id", offs);
        }

        printf("]");
    }

    void PrintFrameRef(instrDesc* id, emitAttr size) const
    {
        int varNum  = id->idDebugOnlyInfo()->varNum;
        int varOffs = id->idDebugOnlyInfo()->varOffs;

        printf("%s[", GetSizeOperator(size));

        if (!asmfm)
        {
            printf("%c%02d", varNum < 0 ? 'T' : 'V', abs(varNum));

            if (varOffs != 0)
            {
                printf("%c0x%X", varOffs < 0 ? '-' : '+', abs(varOffs));
            }

            printf(" ");
        }

        bool ebpBased;
        int  disp = compiler->lvaFrameAddress(varNum, &ebpBased) + varOffs;

        printf("%s", getRegName(ebpBased ? REG_EBP : REG_ESP));

#if !FEATURE_FIXED_OUT_ARGS
        if (!ebpBased)
        {
            unsigned level = stackLevel;

            if (id->idIns() == INS_pop)
            {
                level -= REGSIZE_BYTES;
            }

            if (level != 0)
            {
                printf("+%02XH", level);
            }
        }
#endif

        if (disp != 0)
        {
            printf("%c%02XH", disp < 0 ? '-' : '+', abs(disp));
        }

        printf("]");
    }

    void PrintImm(instrDesc* id) const
    {
        if (id->idIsCnsReloc())
        {
            PrintReloc(id->GetImm());
            return;
        }

        ssize_t imm = id->GetImm();

        // Munge any pointers if we want diff-able disassembly
        if (compiler->opts.disDiffable)
        {
            ssize_t top14bits = (imm >> 18);
            if ((top14bits != 0) && (top14bits != -1))
            {
                imm = 0xD1FFAB1E;
            }
        }

        if ((imm > -1000) && (imm < 1000))
        {
            printf("%d", imm);
        }
        else if ((imm > 0) || (imm < -0xFFFFFF))
        {
            printf("0x%" AMD64_ONLY("I") "X", imm);
        }
        else
        {
            printf("-0x%" AMD64_ONLY("I") "X", -imm);
        }

        if (id->idDebugOnlyInfo()->idHandleKind != HandleKind::None)
        {
            PrintHandleComment(reinterpret_cast<void*>(id->GetImm()), id->idDebugOnlyInfo()->idHandleKind);
        }
    }

    void PrintReloc(ssize_t value, const char* prefix = "") const
    {
        printf("%s%s0x%IX", prefix, compiler->opts.compReloc ? "reloc " : "", compiler->dspPtr(value));
    }

    void PrintAddrMode(instrDesc* id, emitAttr size) const
    {
        auto        am        = id->idAddr()->iiaAddrMode;
        ssize_t     disp      = id->idIns() == INS_call ? id->GetCallDisp() : id->GetAmDisp();
        bool        frameRef  = false;
        const char* separator = "";

        printf("%s%s[", GetSizeOperator(size), id->HasFSPrefix() ? "fs:" : "");

        if (am.base != REG_NA)
        {
            printf("%s", getRegName(am.base));

            separator = "+";
            frameRef  = (am.base == REG_ESP) || ((am.base == REG_EBP) && codeGen->isFramePointerUsed());
        }

        if (am.index != REG_NA)
        {
            if (am.scale != 0)
            {
                printf("%s%d", separator, 1 << am.scale);
                separator = "*";
            }

            printf("%s%s", separator, getRegName(am.index));
            separator = "+";
        }

        if (id->idIsDspReloc() && (id->idIns() != INS_i_jmp))
        {
            PrintReloc(disp, separator);
        }
        // Munge any pointers if we want diff-able disassembly
        // It's assumed to be a pointer when disp is outside of the range (-1M, +1M); top bits are not 0 or -1
        else if (!frameRef && compiler->opts.disDiffable && (static_cast<size_t>((disp >> 20) + 1) > 1))
        {
            printf("%s0x%IX", separator, compiler->dspPtr(disp));
        }
        else if (disp > 0)
        {
            if (frameRef)
            {
                printf("%s%02XH", separator, disp);
            }
            else if (disp < 1000)
            {
                printf("%s%d", separator, disp);
            }
            else if (disp <= 0xFFFF)
            {
                printf("%s0x%04X", separator, disp);
            }
            else
            {
                printf("%s0x%08IX", separator, disp);
            }
        }
        else if (disp < 0)
        {
            if (frameRef)
            {
                printf("-%02XH", -disp);
            }
            else if (disp > -1000)
            {
                printf("-%d", -disp);
            }
            else if (disp >= -0xFFFF)
            {
                printf("-0x%04X", -disp);
            }
            else if (disp < -0xFFFFFF)
            {
                printf("%s0x%08X", separator, disp);
            }
            else
            {
                printf("-0x%08IX", -disp);
            }
        }
        else if (separator[0] == 0)
        {
            printf("0");
        }

        printf("]");

        if (id->idIns() == INS_mov)
        {
            if (id->idDebugOnlyInfo()->dispHandleKind == HandleKind::String)
            {
                PrintHandleComment(reinterpret_cast<void*>(disp), HandleKind::String);
            }
        }
        else if ((id->idIns() == INS_call) || (id->idIns() == INS_i_jmp))
        {
            if (id->idDebugOnlyInfo()->idHandle != nullptr)
            {
                printf("%s", compiler->eeGetMethodFullName(
                                 static_cast<CORINFO_METHOD_HANDLE>(id->idDebugOnlyInfo()->idHandle)));
            }
        }
    }

    void PrintShiftCL(instruction ins) const
    {
        if (IsShiftCL(ins))
        {
            printf(", cl");
        }
    }

    void PrintLabel(instrDescJmp* id) const
    {
        if (id->idCodeSize() == JMP_JCC_SIZE_SMALL)
        {
            printf("SHORT ");
        }

        if (id->HasInstrCount())
        {
            printf("%3d instr", id->GetInstrCount());
        }
#ifdef TARGET_X86
        else if (id->HasConstData())
        {
            printf("RWD%02u", id->GetConstData()->offset);
        }
#endif
        else
        {
            AsmPrinter::PrintLabel(id->GetLabel());
        }
    }

    void PrintMethod(instrDesc* id) const
    {
        if (id->idInsFmt() == IF_METHPTR)
        {
            printf("[");
        }

        printf("%s",
               compiler->eeGetMethodFullName(static_cast<CORINFO_METHOD_HANDLE>(id->idDebugOnlyInfo()->idHandle)));

        if (id->idInsFmt() == IF_METHPTR)
        {
            printf("]");
        }
    }

    const char* GetInsName(instrDesc* id) const
    {
        instruction ins  = id->idIns();
        const char* name = insName(ins);

        static unsigned curBuf = 0;
        static char     buf[4][40];

        if ((INS_FIRST_VEX_INSTRUCTION <= ins) && (ins <= INS_LAST_SSE_INSTRUCTION) && useVex)
        {
            auto& retbuf = buf[curBuf++ % _countof(buf)];
            sprintf_s(retbuf, _countof(retbuf), "v%s", name);
            return retbuf;
        }

        if ((ins == INS_stos) || (ins == INS_movs) || (ins == INS_rep_stos) || (ins == INS_rep_movs))
        {
            static const char types[]{'b', 'w', 'd', 'q', '?'};

            auto& retbuf = buf[curBuf++ % _countof(buf)];
            char  type   = types[Min<size_t>(BitPosition(id->idOpSize()), _countof(types) - 1)];
            sprintf_s(retbuf, _countof(retbuf), "%s%c", name, type);
            return retbuf;
        }

        return name;
    }

    void PrintInstr(instrDesc* id) const
    {
        if (id->idInsFmt() == IF_GC_REG)
        {
            printf(".gcreg    %s", RegName(id->idReg1(), id->idGCref() == GCT_GCREF ? EA_GCREF : EA_BYREF));
            return;
        }

        const char* insName = GetInsName(id);
        printf(" %-9s", insName);

#ifndef HOST_UNIX
        if (strnlen_s(insName, 10) >= 9)
#else
        if (strnlen(insName, 10) >= 9)
#endif
        {
            printf(" ");
        }

        instruction ins = id->idIns();
        emitAttr    attr;
        emitAttr    mattr;

        if (id->idGCref() == GCT_GCREF)
        {
            attr  = EA_GCREF;
            mattr = attr;
        }
        else if (id->idGCref() == GCT_BYREF)
        {
            attr  = EA_BYREF;
            mattr = attr;
        }
        else
        {
            attr = id->idOpSize();

            switch (ins)
            {
                case INS_vextractf128:
                case INS_vextracti128:
                case INS_vinsertf128:
                case INS_vinserti128:
                    mattr = EA_16BYTE;
                    break;
                case INS_pextrb:
                case INS_pinsrb:
                    mattr = EA_1BYTE;
                    break;
                case INS_pextrw:
                case INS_pextrw_sse41:
                case INS_pinsrw:
                    mattr = EA_2BYTE;
                    break;
                case INS_extractps:
                case INS_insertps:
                case INS_pextrd:
                case INS_pinsrd:
                    mattr = EA_4BYTE;
                    break;
                case INS_pextrq:
                case INS_pinsrq:
                    mattr = EA_8BYTE;
                    break;
                case INS_lea:
                    mattr = EA_UNKNOWN;
                    break;
                default:
                    mattr = attr;
                    break;
            }
        }

        emitAttr attr1 = attr;
        emitAttr attr2 = attr;
        emitAttr attr3 = attr;

        switch (ins)
        {
            case INS_movsx:
            case INS_movzx:
#ifdef TARGET_AMD64
            case INS_movsxd:
#endif
                attr1 = EA_PTRSIZE;
                break;
            case INS_crc32:
                attr1 = attr == EA_8BYTE ? EA_8BYTE : EA_4BYTE;
                break;
            case INS_extractps:
            case INS_pextrb:
            case INS_pextrw:
            case INS_pextrw_sse41:
            case INS_pextrd:
            case INS_pmovmskb:
                attr1 = EA_4BYTE;
                break;
            case INS_pextrq:
                attr1 = EA_8BYTE;
                break;
            case INS_vextractf128:
            case INS_vextracti128:
            case INS_cvtsi2ss:
            case INS_cvtsi2sd:
                attr1 = EA_16BYTE;
                break;
            case INS_cvttsd2si:
            case INS_cvtss2si:
            case INS_cvtsd2si:
            case INS_cvttss2si:
                attr2 = EA_16BYTE;
                break;
            default:
                break;
        }

        switch (id->idInsFmt())
        {
            case IF_CNS:
                PrintImm(id);
                break;

            case IF_ARD:
            case IF_AWR:
            case IF_ARW:
                PrintAddrMode(id, mattr);
                PrintShiftCL(ins);
                break;

            case IF_RRD_ARD:
            case IF_RWR_ARD:
            case IF_RRW_ARD:
                printf("%s, ", RegName(id->idReg1(), attr1));
                PrintAddrMode(id, mattr);
                break;

            case IF_RRW_ARD_CNS:
            case IF_RWR_ARD_CNS:
                printf("%s, ", RegName(id->idReg1(), attr));
                PrintAddrMode(id, mattr);
                printf(", ");
                PrintImm(id);
                break;

            case IF_AWR_RRD_CNS:
                PrintAddrMode(id, EA_16BYTE);
                printf(", %s, ", RegName(id->idReg1(), attr));
                PrintImm(id);
                break;

            case IF_RWR_RRD_ARD:
                printf("%s, %s, ", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr));
                PrintAddrMode(id, mattr);
                break;

            case IF_RWR_ARD_RRD:
                if ((ins == INS_vpgatherqd) || (ins == INS_vgatherqps))
                {
                    attr1 = EA_16BYTE;
                    attr2 = EA_16BYTE;
                }

                printf("%s, ", RegName(id->idReg1(), attr1));
                PrintAddrMode(id, EA_4BYTE);
                printf(", %s", RegName(id->idReg2(), attr2));
                break;

            case IF_RWR_RRD_ARD_CNS:
                printf("%s, %s, ", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr));
                PrintAddrMode(id, mattr);
                printf(", ");
                PrintImm(id);
                break;

            case IF_RWR_RRD_ARD_RRD:
                printf("%s, %s, ", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr));
                PrintAddrMode(id, EA_UNKNOWN);
                printf(", %s", RegName(id->idReg4(), attr));
                break;

            case IF_ARD_RRD:
            case IF_AWR_RRD:
            case IF_ARW_RRD:
                PrintAddrMode(id, mattr);
                printf(", %s", RegName(id->idReg1(), attr));
                break;

            case IF_AWR_RRD_RRD:
                PrintAddrMode(id, mattr);
                printf(", %s, %s", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr));
                break;

            case IF_ARD_CNS:
            case IF_AWR_CNS:
            case IF_ARW_CNS:
                PrintAddrMode(id, mattr);
                printf(", ");
                PrintImm(id);
                break;

            case IF_SRD:
            case IF_SWR:
            case IF_SRW:
                PrintFrameRef(id, mattr);
                PrintShiftCL(ins);
                break;

            case IF_SRD_RRD:
            case IF_SWR_RRD:
            case IF_SRW_RRD:
                PrintFrameRef(id, mattr);
                printf(", %s", RegName(id->idReg1(), attr));
                break;

            case IF_SRD_CNS:
            case IF_SWR_CNS:
            case IF_SRW_CNS:
                PrintFrameRef(id, mattr);
                printf(", ");
                PrintImm(id);
                break;

            case IF_SWR_RRD_CNS:
                PrintFrameRef(id, mattr);
                printf(", %s, ", RegName(id->idReg1(), attr));
                PrintImm(id);
                break;

            case IF_RRD_SRD:
            case IF_RWR_SRD:
            case IF_RRW_SRD:
                printf("%s, ", RegName(id->idReg1(), attr1));
                PrintFrameRef(id, mattr);
                break;

            case IF_RRW_SRD_CNS:
            case IF_RWR_SRD_CNS:
                printf("%s, ", RegName(id->idReg1(), attr));
                PrintFrameRef(id, mattr);
                printf(", ");
                PrintImm(id);
                break;

            case IF_RWR_RRD_SRD:
                printf("%s, %s, ", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr));
                PrintFrameRef(id, mattr);
                break;

            case IF_RWR_RRD_SRD_CNS:
                printf("%s, %s, ", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr));
                PrintFrameRef(id, mattr);
                printf(", ");
                PrintImm(id);
                break;

            case IF_RWR_RRD_SRD_RRD:
                printf("%s, %s, ", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr));
                PrintFrameRef(id, EA_UNKNOWN);
                printf(", %s", RegName(id->idReg4(), attr));
                break;

            case IF_RRD_RRD:
            case IF_RWR_RRD:
            case IF_RRW_RRD:
            case IF_RRW_RRW:
                printf("%s, %s", RegName(id->idReg1(), attr1), RegName(id->idReg2(), attr2));
                break;

            case IF_RWR_RRD_RRD:
                if ((ins == INS_bextr) || (ins == INS_bzhi))
                {
                    // BMI bextr and bzhi encodes the reg2 in VEX.vvvv and reg3 in modRM,
                    // which is different from most of other instructions
                    printf("%s, %s, %s", RegName(id->idReg1(), attr), RegName(id->idReg3(), attr),
                           RegName(id->idReg2(), attr));
                }
                else
                {
                    printf("%s, %s, %s", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr),
                           RegName(id->idReg3(), attr));
                }
                break;

            case IF_RWR_RRD_RRD_RRD:
                printf("%s, %s, %s, %s", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr),
                       RegName(id->idReg3(), attr), RegName(id->idReg4(), attr));
                break;

            case IF_RWR_RRD_RRD_CNS:
                switch (ins)
                {
                    case INS_vinsertf128:
                    case INS_vinserti128:
                        attr3 = EA_16BYTE;
                        break;
                    case INS_pinsrb:
                    case INS_pinsrw:
                    case INS_pinsrd:
                        attr3 = EA_4BYTE;
                        break;
                    case INS_pinsrq:
                        attr3 = EA_8BYTE;
                        break;
                    default:
                        break;
                }

                printf("%s, %s, %s, ", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr),
                       RegName(id->idReg3(), attr3));
                PrintImm(id);
                break;

            case IF_RRW_RRD_CNS:
                switch (ins)
                {
                    case INS_pinsrb:
                    case INS_pinsrw:
                    case INS_pinsrd:
                        attr2 = EA_4BYTE;
                        break;
                    case INS_pinsrq:
                        attr2 = EA_8BYTE;
                        break;
                    default:
                        break;
                }

                printf("%s, %s, ", RegName(id->idReg1(), attr1), RegName(id->idReg2(), attr2));
                PrintImm(id);
                break;

            case IF_RRD:
            case IF_RWR:
            case IF_RRW:
                printf("%s", RegName(id->idReg1(), attr));
                PrintShiftCL(ins);
                break;

            case IF_RRD_MRD:
            case IF_RWR_MRD:
            case IF_RRW_MRD:
                printf("%s, ", RegName(id->idReg1(), attr1));
                PrintConstDataLabel(id, mattr);
                break;

            case IF_RRW_MRD_CNS:
            case IF_RWR_MRD_CNS:
                printf("%s, ", RegName(id->idReg1(), attr));
                PrintConstDataLabel(id, mattr);
                printf(", ");
                PrintImm(id);
                break;

            case IF_MWR_RRD_CNS:
                PrintConstDataLabel(id, EA_16BYTE);
                printf(", %s, ", RegName(id->idReg1(), attr));
                PrintImm(id);
                break;

            case IF_RWR_RRD_MRD:
                printf("%s, %s, ", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr));
                PrintConstDataLabel(id, mattr);
                break;

            case IF_RWR_RRD_MRD_CNS:
                printf("%s, %s, ", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr));
                PrintConstDataLabel(id, mattr);
                printf(", ");
                PrintImm(id);
                break;

            case IF_RWR_RRD_MRD_RRD:
                printf("%s, %s, ", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr));
                PrintConstDataLabel(id, EA_UNKNOWN);
                printf(", %s", RegName(id->idReg4(), attr));
                break;

            case IF_MRD_RRD:
            case IF_MWR_RRD:
            case IF_MRW_RRD:
                PrintConstDataLabel(id, mattr);
                printf(", %s", RegName(id->idReg1(), attr));
                break;

            case IF_MRD_CNS:
            case IF_MWR_CNS:
            case IF_MRW_CNS:
                PrintConstDataLabel(id, mattr);
                printf(", ");
                PrintImm(id);
                break;

            case IF_MRD:
            case IF_MWR:
            case IF_MRW:
                PrintConstDataLabel(id, mattr);
                PrintShiftCL(ins);
                break;

            case IF_RRD_CNS:
            case IF_RWR_CNS:
            case IF_RRW_CNS:
                printf("%s, ", RegName(id->idReg1(), attr));
                PrintImm(id);
                break;

            case IF_RWR_LABEL:
                printf("%s, ", RegName(id->idReg1(), attr));
                PrintLabel(static_cast<instrDescJmp*>(id));
                break;

            case IF_LABEL:
                PrintLabel(static_cast<instrDescJmp*>(id));
                break;

            case IF_METHOD:
            case IF_METHPTR:
                PrintMethod(id);
                break;

            case IF_NONE:
                if (ins == INS_align)
                {
                    printf("[%d bytes]", id->idCodeSize());
                }
                break;

            default:
                printf("???");
                break;
        }
    }
};

void X86Emitter::PrintInstr(instrDesc* id)
{
    if (compiler->verbose)
    {
        X86AsmPrinter printer(compiler, codeGen, false, UseVEXEncoding());
#if !FEATURE_FIXED_OUT_ARGS
        printer.SetStackLevel(stackLevel);
#endif
        printer.Print(id);
        printf("\n");
    }
}

void Encoder::PrintAlignmentBoundary(size_t           instrAddr,
                                     size_t           instrEndAddr,
                                     const instrDesc* instr,
                                     const instrDesc* nextInstr)
{
    // Determine if this instruction is part of a set that matches the Intel jcc erratum characteristic
    // described here:
    // https://www.intel.com/content/dam/support/us/en/documents/processors/mitigations-jump-conditional-code-erratum.pdf
    // This is the case when a jump instruction crosses a 32-byte boundary, or ends on a 32-byte boundary.
    // "Jump instruction" in this case includes conditional jump (jcc), macro-fused op-jcc (where 'op' is
    // one of cmp, test, add, sub, and, inc, or dec), direct unconditional jump, indirect jump,
    // direct/indirect call, and return.

    const size_t jccAlignment    = 32;
    const size_t jccBoundaryAddr = instrEndAddr & ~(jccAlignment - 1);

    if (instrAddr < jccBoundaryAddr)
    {
        const instruction ins = instr->idIns();
        bool isJccAffectedIns = IsJccInstruction(ins) || IsJmpInstruction(ins) || (ins == INS_call) || (ins == INS_ret);

        // For op-Jcc there are two cases: (1) ins is the jcc, in which case the above condition
        // already covers us. (2) ins is the `op` and the next instruction is the `jcc`. Note that
        // we will never have a `jcc` as the first instruction of a group, so we don't need to worry
        // about looking ahead to the next group after a an `op` of `op-Jcc`.

        if (!isJccAffectedIns && (nextInstr != nullptr))
        {
            const instruction nextIns = nextInstr->idIns();
            isJccAffectedIns          = IsJccInstruction(nextIns) &&
                               ((ins == INS_cmp) || (ins == INS_test) || (ins == INS_add) || (ins == INS_sub) ||
                                (ins == INS_and) || (ins == INS_inc) || (ins == INS_dec));
        }

        if (isJccAffectedIns)
        {
            const size_t bytesCrossedBoundary = instrEndAddr & (jccAlignment - 1);

            printf("; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ (%s: %d ; jcc erratum) %dB boundary "
                   "...............................\n",
                   insName(ins), bytesCrossedBoundary, jccAlignment);

            return;
        }
    }

    const size_t alignment    = compiler->opts.compJitAlignLoopBoundary;
    const size_t boundaryAddr = instrEndAddr & ~(alignment - 1);

    if (instrAddr < boundaryAddr)
    {
        // Indicate if instruction is at the alignment boundary or is split
        const size_t bytesCrossedBoundary = instrEndAddr & (alignment - 1);

        if (bytesCrossedBoundary != 0)
        {
            printf("; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ (%s: %d)", insName(instr->idIns()), bytesCrossedBoundary);
        }
        else
        {
            printf("; ...............................");
        }

        printf(" %dB boundary ...............................\n", alignment);
    }
}
#endif // DEBUG

// Returns the base encoding of the given CPU instruction.
static size_t GetCodeJ(instruction ins)
{
    const static uint32_t codes[]{
#define INST0(id, nm, um, mr, ...) mr,
#define INST1(...)
#define INST2(...)
#define INST3(...)
#define INST4(...)
#define INST5(...)
#include "instrsxarch.h"
    };

    assert((INS_FIRST_JMP <= ins) && (ins <= INS_LAST_JMP));
    assert(codes[ins - INS_FIRST_JMP] != BAD_CODE);

    return codes[ins - INS_FIRST_JMP];
}

// Returns the "AL/AX/EAX, IMM" accumulator encoding of the given instruction.
static size_t GetCodeA(instruction ins)
{
    const static uint32_t codes[]{
#define INST0(...)
#define INST1(...)
#define INST2(...)
#define INST3(...)
#define INST4(id, nm, um, mr, mi, rm, a4, flags) a4,
#define INST5(id, nm, um, mr, mi, rm, a4, rr, flags) a4,
#include "instrsxarch.h"
    };

    assert(ins < _countof(codes));
    assert(codes[ins] != BAD_CODE);

    return codes[ins];
}

// Returns the "REG, REG" or "REG" encoding of the given instruction.
static size_t GetCodeRR(instruction ins)
{
    const static uint32_t codes[]{
#define INST0(...)
#define INST1(...)
#define INST2(...)
#define INST3(...)
#define INST4(...)
#define INST5(id, nm, um, mr, mi, rm, a4, rr, flags) rr,
#include "instrsxarch.h"
    };

    assert(ins < _countof(codes));
    assert(codes[ins] != BAD_CODE);

    return codes[ins];
}

#ifdef TARGET_AMD64

static code_t AddRexWPrefix(code_t code)
{
    return code | (0x8ull << RexBitOffset);
}

static code_t AddRexRPrefix(code_t code)
{
    return code | (0x4ull << RexBitOffset);
}

static code_t AddRexXPrefix(code_t code)
{
    return code | (0x2ull << RexBitOffset);
}

static code_t AddRexBPrefix(code_t code)
{
    return code | (0x1ull << RexBitOffset);
}

// Adds REX prefix (0x40) without W, R, X or B bits set
static code_t AddRexPrefix(code_t code)
{
    assert(!HasVexPrefix(code));

    return code | (0x40ull << RexBitOffset);
}

#endif // TARGET_AMD64

static code_t AddVexPrefix(instruction ins, code_t code, emitAttr attr)
{
    assert(!HasVexPrefix(code));

    code |= 1ull << HasVexBitOffset;

    if (attr == EA_32BYTE)
    {
        code |= 1ull << VexLBitOffset;
    }

    return code;
}

static_assert_no_msg((REG_RAX & 0x7) == 0);
static_assert_no_msg((REG_XMM0 & 0x7) == 0);

// Returns bits to be encoded in instruction for the given register.
static unsigned RegEncoding(RegNum reg)
{
    assert(reg < REG_STK);
    return static_cast<unsigned>(reg & 0x7);
}

static unsigned RegVvvvEncoding(RegNum reg)
{
    assert(reg < REG_STK);
#ifdef TARGET_AMD64
    return static_cast<unsigned>(reg & 0xF);
#else
    return static_cast<unsigned>(reg & 0x7);
#endif
}

// Returns an encoding for the specified register to be used in the bits 0-2 of an opcode.
static unsigned EncodeReg012(instruction ins, RegNum reg, emitAttr size, code_t* code)
{
#ifdef TARGET_AMD64
    if (IsExtendedReg(reg))
    {
        *code = AddRexBPrefix(*code);
    }
    else if ((size == EA_1BYTE) && (reg > REG_RBX))
    {
        // We are assuming that we only use/encode SPL, BPL, SIL and DIL
        // not the corresponding AH, CH, DH, or BH
        *code = AddRexPrefix(*code);
    }
#endif // TARGET_AMD64

    return RegEncoding(reg);
}

// Returns an encoding for the specified register to be used in the bits 3-5 of an opcode.
static unsigned EncodeReg345(instruction ins, RegNum reg, emitAttr size, code_t* code)
{
#ifdef TARGET_AMD64
    if (IsExtendedReg(reg))
    {
        *code = AddRexRPrefix(*code);
    }
    else if ((size == EA_1BYTE) && (reg > REG_RBX))
    {
        // We are assuming that we only use/encode SPL, BPL, SIL and DIL
        // not the corresponding AH, CH, DH, or BH
        *code = AddRexPrefix(*code);
    }
#endif // TARGET_AMD64

    return RegEncoding(reg) << 3;
}

static bool IsBMIRegExtInstruction(instruction ins)
{
    return (ins == INS_blsi) || (ins == INS_blsmsk) || (ins == INS_blsr);
}

static code_t SetRMReg(instruction ins, RegNum reg, emitAttr size, code_t code)
{
    assert(!IsBMIRegExtInstruction(ins));
    code |= EncodeReg345(ins, reg, size, &code) << 8;
    return code;
}

static code_t SetVexVvvv(RegNum reg, emitAttr size, code_t code)
{
    assert(HasVexPrefix(code));

    code_t regBits = RegVvvvEncoding(reg);
    assert(regBits <= 0xF);
    return code | (regBits << VexVvvvBitOffset);
}

// Returns the "byte ptr [r/m]" opcode with the mod/RM field set to the given register.
static code_t EncodeRMReg(instruction ins, RegNum reg, emitAttr size, code_t code)
{
    assert((code & 0xC000) == 0);
    code |= 0xC000;
    code |= EncodeReg012(ins, reg, size, &code) << 8;
    return code;
}

#ifdef DEBUG
static bool HasWBit(instruction ins)
{
    // TODO-MIKE-Cleanup: It's probably best to make this a flag,
    // or at least ensure that the instruction order is correct.
    return ((INS_add <= ins) && (ins <= INS_movsx)) || ((INS_rol <= ins) && (ins <= INS_stos)) || (ins == INS_crc32);
}
#endif

static bool HasSBit(instruction ins)
{
    return ((INS_add <= ins) && (ins <= INS_cmp)) || (ins == INS_imuli);
}

class X86Encoder final : public Encoder
{
    const bool useVEXEncodings;

public:
    X86Encoder(X86Emitter& emit, GCInfo& gcInfo) : Encoder(emit, gcInfo), useVEXEncodings(emit.useVEXEncodings)
    {
    }

    size_t EncodeInstr(insGroup* ig, instrDesc* id, uint8_t** dp);

private:
    size_t Write8(uint8_t* dst, ssize_t val);
    size_t Write16(uint8_t* dst, ssize_t val);
    size_t Write32(uint8_t* dst, ssize_t val);
#ifdef TARGET_AMD64
    size_t Write64(uint8_t* dst, int64_t val);
#endif

#if defined(TARGET_X86) && !defined(HOST_64BIT)
    size_t Write8(uint8_t* dst, uint32_t val);
    size_t Write16(uint8_t* dst, uint32_t val);
    size_t Write32(uint8_t* dst, uint32_t val);

    size_t Write8(uint8_t* dst, uint64_t val);
    size_t Write16(uint8_t* dst, uint64_t val);
    size_t Write32(uint8_t* dst, uint64_t val);
#endif

    size_t EncodeImm(uint8_t* dst, instrDesc* id, size_t size, ssize_t imm);

    uint8_t* EncodeAlign(insGroup* ig, instrDesc* id, uint8_t* dst);

    uint8_t* EncodeOpcode(uint8_t* dst, instrDesc* id, code_t& code);
    uint8_t* EncodeAM(uint8_t* dst, instrDesc* id, code_t code, ssize_t* imm = nullptr);
    uint8_t* EncodeSV(uint8_t* dst, instrDesc* id, code_t code, ssize_t* imm = nullptr);
    uint8_t* EncodeCV(uint8_t* dst, instrDesc* id, code_t code, ssize_t* imm = nullptr);

    uint8_t* EncodeR(uint8_t* dst, instrDesc* id);
    uint8_t* EncodeRI(uint8_t* dst, instrDesc* id);
    uint8_t* EncodeRR(uint8_t* dst, instrDesc* id);
    uint8_t* EncodeI(uint8_t* dst, instrDesc* id);
    uint8_t* EncodeRRR(uint8_t* dst, instrDesc* id);
    uint8_t* EncodeRRI(uint8_t* dst, instrDesc* id);
    uint8_t* EncodeRoDataAddr(uint8_t* dst, instrDescJmp* id, insGroup* ig);
#ifdef TARGET_X86
    uint8_t* EncodePushLabel(uint8_t* dst, instrDescJmp* id, insGroup* ig);
#endif
    uint8_t* EncodeJump(uint8_t* dst, instrDescJmp* id, insGroup* ig);
    uint8_t* EncodeCall(uint8_t* dst, instrDesc* id);
    uint8_t* EncodeNoOperands(uint8_t* dst, instrDesc* id);

    size_t EncodeVexPrefix(uint8_t* dst, code_t code DEBUGARG(instruction ins));
#ifdef TARGET_AMD64
    size_t EncodeRexPrefix(uint8_t* dst, code_t code);
#endif
    size_t EncodeRexPrefixIfNeeded(uint8_t* dst, code_t code);
    size_t EncodeRexOrVexPrefixIfNeeded(uint8_t* dst, code_t code DEBUGARG(instruction ins));
    size_t EncodePrefixesIfNeeded(uint8_t* dst, code_t code);

    code_t AddVexPrefixIfNeeded(instruction ins, code_t code, emitAttr size);

    bool UseVEXEncoding() const
    {
        return useVEXEncodings;
    }

    bool TakesVexPrefix(instruction ins) const
    {
        return ::TakesVexPrefix(ins, useVEXEncodings);
    }

    bool IsVexDstDstSrc(instruction ins) const
    {
        return ::IsVexDstDstSrc(ins, useVEXEncodings);
    }

    bool IsVexDstSrcSrc(instruction ins) const
    {
        return ::IsVexDstSrcSrc(ins, useVEXEncodings);
    }

#ifdef DEBUG
    bool IsVexTernary(instruction ins) const
    {
        return ::IsVexTernary(ins, useVEXEncodings);
    }

    bool IsReallyVexTernary(instruction ins) const
    {
        return ::IsReallyVexTernary(ins, useVEXEncodings);
    }

    void PrintInstr(instrDesc* id, uint8_t* code, size_t sz);
#endif
};

#ifdef DEBUG
void X86Encoder::PrintInstr(instrDesc* id, uint8_t* code, size_t sz)
{
    JITDUMP("IN%04X: ", id->idDebugOnlyInfo()->idNum);

    PrintInsAddr(code);

    if (!compiler->opts.disDiffable)
    {
        PrintHexCode(code, sz);
    }

    X86AsmPrinter printer(compiler, codeGen, true, UseVEXEncoding());
#if !FEATURE_FIXED_OUT_ARGS
    printer.SetStackLevel(stackLevel);
#endif
    printer.Print(id);

    if (sz != id->idCodeSize())
    {
        JITDUMP(" (ECS:%d, ACS:%d)", id->idCodeSize(), sz);
    }

    printf("\n");
}

#endif // DEBUG

size_t X86Encoder::Write8(uint8_t* dst, ssize_t val)
{
    *(dst + writeableOffset) = static_cast<uint8_t>(val);
    return 1;
}

size_t X86Encoder::Write16(uint8_t* dst, ssize_t val)
{
    *reinterpret_cast<int16_t*>(dst + writeableOffset) = static_cast<int16_t>(val);
    return 2;
}

size_t X86Encoder::Write32(uint8_t* dst, ssize_t val)
{
    *reinterpret_cast<int32_t*>(dst + writeableOffset) = static_cast<int32_t>(val);
    return 4;
}

#ifdef TARGET_AMD64
size_t X86Encoder::Write64(uint8_t* dst, int64_t val)
{
    *reinterpret_cast<int64_t*>(dst + writeableOffset) = val;
    return 8;
}
#endif

#if defined(TARGET_X86) && !defined(HOST_64BIT)
size_t X86Encoder::Write8(uint8_t* dst, uint32_t val)
{
    return Write8(dst, static_cast<int32_t>(val));
}

size_t X86Encoder::Write16(uint8_t* dst, uint32_t val)
{
    return Write16(dst, static_cast<int32_t>(val));
}

size_t X86Encoder::Write32(uint8_t* dst, uint32_t val)
{
    return Write32(dst, static_cast<int32_t>(val));
}

size_t X86Encoder::Write8(uint8_t* dst, uint64_t val)
{
    return Write8(dst, static_cast<int32_t>(val));
}

size_t X86Encoder::Write16(uint8_t* dst, uint64_t val)
{
    return Write16(dst, static_cast<int32_t>(val));
}

size_t X86Encoder::Write32(uint8_t* dst, uint64_t val)
{
    return Write32(dst, static_cast<int32_t>(val));
}
#endif // defined(TARGET_X86) && !defined(HOST_64BIT)

size_t X86Encoder::EncodeVexPrefix(uint8_t* dst, code_t code DEBUGARG(instruction ins))
{
    assert(TakesVexPrefix(ins) && HasVexPrefix(code));
    // There should be some prefixes (opcdoe map 0 doesn't use VEX).
    assert(((code >> PrefixesBitOffset) & 0xFF) != 0);

    uint32_t vvvvl = (code >> VexBitOffset) & (0x1F << 2);

    // VEX2 can be used if REX.W, REX.X, REX.B are all 0 and VEX.mmm is 1 (opcode map 0F).

    constexpr unsigned RexRDelta = RexBitOffset - 5;
    constexpr unsigned RexWDelta = RexBitOffset - 4;

    if ((code & ((0x0Bull << RexBitOffset) | (7ull << MmmBitOffset))) == (1ull << MmmBitOffset))
    {
        uint32_t vex21 = (((code >> RexRDelta) & 0x80) | vvvvl | ((code >> PpBitOffset) & 3)) ^ (0x1F << 3);
        Write16(dst, 0xC5 | (vex21 << 8));

        return 2;
    }

    uint32_t vex31 = (((code >> RexRDelta) & 0xE0) | ((code >> MmmBitOffset) & 7)) ^ (0x07 << 5);
    Write16(dst, 0xC4 | (vex31 << 8));
    uint32_t vex32 = (((code >> RexWDelta) & 0x80) | vvvvl | ((code >> PpBitOffset) & 3)) ^ (0x0F << 3);
    Write8(dst + 2, vex32);

    return 3;
}

#ifdef TARGET_AMD64
size_t X86Encoder::EncodeRexPrefix(uint8_t* dst, code_t code)
{
    assert(!HasVexPrefix(code));
    uint32_t rex = (code >> RexBitOffset) & 0xFF;
    assert(((code >> PpBitOffset) & 3) == 0); // Can't emit REX prefix before other prefixes.
    return Write8(dst, rex | 0x40);
}
#endif // TARGET_AMD64

size_t X86Encoder::EncodeRexPrefixIfNeeded(uint8_t* dst, code_t code)
{
    assert(!HasVexPrefix(code));

#ifdef TARGET_AMD64
    if (((code >> RexBitOffset) & 0xFF) != 0)
    {
        return EncodeRexPrefix(dst, code);
    }
#endif

    return 0;
}

size_t X86Encoder::EncodeRexOrVexPrefixIfNeeded(uint8_t* dst, code_t code DEBUGARG(instruction ins))
{
    if (HasVexPrefix(code))
    {
        return EncodeVexPrefix(dst, code DEBUGARG(ins));
    }

    if ((code >> PrefixesBitOffset) != 0)
    {
        return EncodePrefixesIfNeeded(dst, code);
    }

    return 0;
}

size_t X86Encoder::EncodePrefixesIfNeeded(uint8_t* dst, code_t code)
{
    uint8_t* start = dst;

    if (uint32_t pp = (code >> PpBitOffset) & 3)
    {
        static const uint8_t prefixMap[]{0, 0x66, 0xF3, 0xF2};

        dst += Write8(dst, prefixMap[pp]);
    }

#ifdef TARGET_AMD64
    if (uint32_t rex = (code >> RexBitOffset) & 0xFF)
    {
        code &= UINT_MAX;
        dst += Write8(dst, rex | 0x40);
    }
#endif

    if (uint32_t map = (code >> MmmBitOffset) & 7)
    {
        if (map == 1)
        {
            dst += Write8(dst, 0x0F);
        }
        else
        {
            assert((map == 2) || (map == 3));

            dst += Write16(dst, map == 2 ? 0x380F : 0x3A0F);
        }
    }

    return dst - start;
}

size_t X86Encoder::EncodeImm(uint8_t* dst, instrDesc* id, size_t size, ssize_t imm)
{
    switch (size)
    {
        case 1:
            return Write8(dst, imm);
        case 2:
            return Write16(dst, imm);
#ifdef TARGET_AMD64
        case 8:
            noway_assert(IsImm32(imm) && !id->idIsCnsReloc());
            return Write32(dst, imm);
#endif
        default:
            assert(size == 4);
            Write32(dst, imm);

            if (id->idIsCnsReloc())
            {
                RecordRelocation(dst, reinterpret_cast<void*>(imm), IMAGE_REL_BASED_HIGHLOW);
            }

            return 4;
    }
}

static uint8_t* EncodeNop(uint8_t* dstRW, size_t nBytes)
{
    assert(nBytes <= instrDesc::MAX_ENCODED_SIZE);

#ifdef TARGET_X86
    // TODO-X86-CQ: when VIA C3 CPU's are out of circulation, switch to the
    // more efficient real NOP: 0x0F 0x1F +modR/M
    // Also can't use AMD recommended, multiple size prefixes (i.e. 0x66 0x66 0x90 for 3 byte NOP)
    // because debugger and msdis don't like it, so maybe VIA doesn't either
    // So instead just stick to repeating single byte nops
    memset(dstRW, 0x90, nBytes);
    dstRW += nBytes;
#else
    switch (nBytes)
    {
        case 2:
            *dstRW++ = 0x66;
            FALLTHROUGH;
        case 1:
            *dstRW++ = 0x90;
            break;
        case 0:
            break;
        case 3:
            *dstRW++ = 0x0F;
            *dstRW++ = 0x1F;
            *dstRW++ = 0x00;
            break;
        case 4:
            *dstRW++ = 0x0F;
            *dstRW++ = 0x1F;
            *dstRW++ = 0x40;
            *dstRW++ = 0x00;
            break;
        case 6:
            *dstRW++ = 0x66;
            FALLTHROUGH;
        case 5:
            *dstRW++ = 0x0F;
            *dstRW++ = 0x1F;
            *dstRW++ = 0x44;
            *dstRW++ = 0x00;
            *dstRW++ = 0x00;
            break;
        case 7:
            *dstRW++ = 0x0F;
            *dstRW++ = 0x1F;
            *dstRW++ = 0x80;
            *dstRW++ = 0x00;
            *dstRW++ = 0x00;
            *dstRW++ = 0x00;
            *dstRW++ = 0x00;
            break;
        case 15:
            // More than 3 prefixes is slower than just 2 NOPs
            dstRW = EncodeNop(EncodeNop(dstRW, 7), 8);
            break;
        case 14:
            // More than 3 prefixes is slower than just 2 NOPs
            dstRW = EncodeNop(EncodeNop(dstRW, 7), 7);
            break;
        case 13:
            // More than 3 prefixes is slower than just 2 NOPs
            dstRW = EncodeNop(EncodeNop(dstRW, 5), 8);
            break;
        case 12:
            // More than 3 prefixes is slower than just 2 NOPs
            dstRW = EncodeNop(EncodeNop(dstRW, 4), 8);
            break;
        case 11:
            *dstRW++ = 0x66;
            FALLTHROUGH;
        case 10:
            *dstRW++ = 0x66;
            FALLTHROUGH;
        case 9:
            *dstRW++ = 0x66;
            FALLTHROUGH;
        case 8:
            *dstRW++ = 0x0F;
            *dstRW++ = 0x1F;
            *dstRW++ = 0x84;
            *dstRW++ = 0x00;
            *dstRW++ = 0x00;
            *dstRW++ = 0x00;
            *dstRW++ = 0x00;
            *dstRW++ = 0x00;
            break;
    }
#endif // TARGET_AMD64

    return dstRW;
}

uint8_t* X86Encoder::EncodeAlign(insGroup* ig, instrDesc* id, uint8_t* dst)
{
    assert(compiler->opts.alignLoops);

    // IG can be marked as not needing alignment after emitting align instruction
    // In such case, skip outputting alignment.
    if (!ig->isLoopAlign())
    {
        // If the IG is not marked as need alignment, then the code size
        // should be zero i.e. no padding needed.
        assert(id->idCodeSize() == 0);

        return dst;
    }

    unsigned paddingToAdd = id->idCodeSize();

    // Either things are already aligned or align them here.
    assert((paddingToAdd == 0) || (((size_t)dst & (compiler->opts.compJitAlignLoopBoundary - 1)) != 0));

    // Padding amount should not exceed the alignment boundary
    assert(0 <= paddingToAdd && paddingToAdd < compiler->opts.compJitAlignLoopBoundary);

#ifdef DEBUG
    unsigned paddingNeeded = emit.CalculatePaddingForLoopAlignment(ig, (size_t)dst, true);

    // For non-adaptive, padding size is spread in multiple instructions, so don't bother checking
    if (compiler->opts.compJitAlignLoopAdaptive)
    {
        assert(paddingToAdd == paddingNeeded);
    }

    compiler->loopsAligned++;
#endif

    return EncodeNop(dst + writeableOffset, paddingToAdd) - writeableOffset;
}

uint8_t* X86Encoder::EncodeOpcode(uint8_t* dst, instrDesc* id, code_t& code)
{
    instruction ins  = id->idIns();
    emitAttr    size = id->idOpSize();

    if (size == EA_1BYTE)
    {
        if (!IsPrefetch(ins))
        {
            assert(HasWBit(ins) && ((code & 1) != 0));
            code ^= 1;
        }
    }
    else if (size == EA_2BYTE)
    {
        if ((ins != INS_movzx) && (ins != INS_movsx))
        {
            assert(!IsSSEOrAVXOrBMIInstruction(ins));

            // We need to emit 0x66 now, instead of adding it to code's prefixes,
            // crc32 already has a mandatory prefix and can also have a size prefix.
            dst += Write8(dst, 0x66);
        }
    }
#ifdef TARGET_X86
    else if (IsX87LdSt(ins))
    {
        assert(size == EA_4BYTE || size == EA_8BYTE);

        if (size == EA_8BYTE)
        {
            code += 4;
        }
    }
#endif
#ifdef TARGET_AMD64
    else if (size == EA_8BYTE)
    {
        if (TakesRexWPrefix(ins))
        {
            code = AddRexWPrefix(code);
        }
    }
#endif
    else
    {
        assert((size == EA_4BYTE) || IsSSEOrAVXOrBMIInstruction(ins));
    }

    dst += EncodeRexOrVexPrefixIfNeeded(dst, code DEBUGARG(ins));

    return dst;
}

uint8_t* X86Encoder::EncodeAM(uint8_t* dst, instrDesc* id, code_t code, ssize_t* imm)
{
    instruction ins      = id->idIns();
    RegNum      baseReg  = id->idAddr()->iiaAddrMode.base;
    RegNum      indexReg = id->idAddr()->iiaAddrMode.index;

    // BT/CMOV support 16 bit operands and this code doesn't add the necessary 66 prefix.
    // BT with memory operands is practically useless and CMOV is not currently generated.
    assert((ins != INS_bt) && !IsCmov(ins));
    assert(TakesVexPrefix(ins) == HasVexPrefix(code));

    if (id->HasFSPrefix())
    {
        dst += Write8(dst, 0x64);
    }

#ifdef TARGET_X86
    // Special case: "mov eax, [addr]" and "mov [addr], eax" have smaller encoding.
    if ((ins == INS_mov) && ((id->idInsFmt() == IF_RWR_ARD) || (id->idInsFmt() == IF_AWR_RRD)) &&
        (id->idReg1() == REG_EAX) && (baseReg == REG_NA) && (indexReg == REG_NA))
    {
        assert(imm == nullptr);

        if (id->idOpSize() == EA_2BYTE)
        {
            dst += Write8(dst, 0x66);
        }

        dst += Write8(dst, 0xA0 | ((id->idInsFmt() == IF_AWR_RRD) << 1) | (id->idOpSize() != EA_1BYTE));

        ssize_t disp = id->GetAmDisp();
        dst += Write32(dst, disp);

        if (id->idIsDspReloc())
        {
            RecordRelocation(dst - 4, reinterpret_cast<void*>(disp), IMAGE_REL_BASED_HIGHLOW, 0);
        }

        if (id->idInsFmt() == IF_RWR_ARD)
        {
            if (id->idGCref() != GCT_NONE)
            {
                AddGCLiveReg(id->idGCref(), id->idReg1(), dst);
            }
            else
            {
                RemoveGCLiveReg(id->idReg1(), dst);
            }
        }

        return dst;
    }
#endif // TARGET_X86

#ifdef TARGET_AMD64
    if (IsExtendedReg(baseReg))
    {
        code = AddRexBPrefix(code);
    }

    if (IsExtendedReg(indexReg))
    {
        code = AddRexXPrefix(code);
    }
#endif

    ssize_t  disp;
    unsigned immSize = 0;

    if (ins == INS_call)
    {
        dst += EncodeRexPrefixIfNeeded(dst, code);

        // The displacement field is in an unusual place for calls
        disp = id->GetCallDisp();
    }
    else
    {
        if (imm != nullptr)
        {
            immSize = IsShiftImm(ins) ? 1 : EA_SIZE_IN_BYTES(id->idOpSize());

            // 1. Instructions like "ADD rm16/32/64, imm16/32" have an alternate encoding if the imm fits in 8 bits.
            // 2. MOV/TEST do not have such encodings, they always need an imm16/32.
            // 3. Shifts and SSE/AVX instructions only have imm8 encodings, independent of the first operand size.
            if ((immSize > 1) && IsImm8(*imm) && !id->idIsCnsReloc() && (ins != INS_mov) && (ins != INS_test))
            {
                if (HasSBit(ins))
                {
                    code |= 2;
                }

                immSize = 1;
            }
        }

        dst = EncodeOpcode(dst, id, code);

        disp = id->GetAmDisp();
    }

    if ((indexReg == REG_NA) && (baseReg == REG_NA))
    {
        if (!id->idIsDspReloc())
        {
#ifdef TARGET_AMD64
            noway_assert(!compiler->opts.compReloc);
            noway_assert(!compiler->eeIsRIPRelativeAddress(reinterpret_cast<void*>(disp)));
            noway_assert(IsDisp32(disp));

            dst += Write16(dst, code | 0x0400);
            dst += Write8(dst, 0x25);
#else
            dst += Write16(dst, code | 0x0500);
#endif

            dst += Write32(dst, disp);
        }
        else
        {
            dst += Write16(dst, code | 0x0500);

            // For emitting relocation, we also need to take into account
            // the additional bytes of code emitted for imm.
            int32_t immRelocDelta = 0;

            if (imm != nullptr)
            {
#ifdef TARGET_AMD64
                noway_assert((immSize < 8) || (IsImm32(*imm) && !id->idIsCnsReloc()));
                immRelocDelta = -static_cast<int32_t>(Min(immSize, 4u));
#else
                noway_assert(immSize <= 4);
                immRelocDelta = -static_cast<int32_t>(immSize);
#endif
            }

#ifdef TARGET_AMD64
            // We emit zero on x64, to avoid the assert in Write32
            dst += Write32(dst, 0);
            RecordRelocation(dst - 4, reinterpret_cast<void*>(disp), IMAGE_REL_BASED_REL32, immRelocDelta);
#else
            dst += Write32(dst, disp);
            RecordRelocation(dst - 4, reinterpret_cast<void*>(disp), IMAGE_REL_BASED_HIGHLOW, immRelocDelta);
#endif
        }
    }
    else
    {
        bool hasDisp  = (disp != 0) || BaseRegRequiresDisp(baseReg);
        bool hasDisp8 = IsDisp8(disp) && !id->idIsDspReloc();

        if (indexReg == REG_NA)
        {
            code |= RegEncoding(baseReg) << 8;

            if (hasDisp)
            {
                code |= hasDisp8 ? 0x4000 : 0x8000;
            }

            dst += Write16(dst, code);

            if (BaseRegRequiresSIB(baseReg))
            {
                dst += Write8(dst, 0x24);
            }
        }
        else
        {
            unsigned scale = id->idAddr()->iiaAddrMode.scale;

            code |= RegEncoding(REG_RSP) << 8;

            if ((scale != 0) && (baseReg == REG_NA))
            {
                baseReg  = REG_RBP;
                hasDisp  = true;
                hasDisp8 = false;
            }
            else if (hasDisp)
            {
                code |= hasDisp8 ? 0x4000 : 0x8000;
            }

            dst += Write16(dst, code);
            dst += Write8(dst, RegEncoding(baseReg) | (RegEncoding(indexReg) << 3) | (scale << 6));
        }

        if (hasDisp)
        {
            if (hasDisp8)
            {
                dst += Write8(dst, disp);
            }
            else
            {
                dst += Write32(dst, disp);

                if (id->idIsDspReloc())
                {
                    // We don't expect something like "mov dword ptr [rax*4+reloc], 42"
                    // so this code doesn't attempt to account for the imm reloc delta.
                    assert(imm == nullptr);

                    RecordRelocation(dst - 4, reinterpret_cast<void*>(disp), IMAGE_REL_BASED_HIGHLOW);
                }
            }
        }
    }

    if (imm != nullptr)
    {
        dst += EncodeImm(dst, id, immSize, *imm);
    }

    if (id->idGCref())
    {
        switch (id->idInsFmt())
        {
            case IF_RWR_ARD:
                AddGCLiveReg(id->idGCref(), id->idReg1(), dst);
                break;
            case IF_RRW_ARD:
                assert(id->idGCref() == GCT_BYREF && (ins == INS_add || ins == INS_sub));
                AddGCLiveReg(GCT_BYREF, id->idReg1(), dst);
                break;
            case IF_ARW_RRD:
            case IF_ARW_CNS:
                assert(id->idGCref() == GCT_BYREF && (ins == INS_add || ins == INS_sub));
                break;
            case IF_ARD:
            case IF_AWR:
            case IF_ARW:
            case IF_ARD_RRD:
            case IF_ARD_CNS:
            case IF_AWR_RRD:
            case IF_AWR_CNS:
            case IF_AWR_RRD_RRD:
            case IF_RRD_ARD:
                break;
            default:
                INDEBUG(printf("%s", EmitterBase::GetFormatName(id->idInsFmt())));
                assert(!"unexpected GC ref instruction format");
        }

        // mul can never produce a GC ref
        assert(ins != INS_mulEAX && ins != INS_imulEAX && ins != INS_imuli);
    }
    else
    {
        switch (id->idInsFmt())
        {
            case IF_RWR_ARD:
            case IF_RRW_ARD:
            case IF_RRW_ARD_CNS:
            case IF_RWR_RRD_ARD:
            case IF_RWR_ARD_CNS:
            case IF_RWR_ARD_RRD:
            case IF_RWR_RRD_ARD_CNS:
            case IF_RWR_RRD_ARD_RRD:
                if (IsGeneralRegister(id->idReg1()))
                {
                    RemoveGCLiveReg(id->idReg1(), dst);
                }
                break;
            case IF_ARD:
                if ((ins == INS_mulEAX) || (ins == INS_imulEAX))
                {
                    RemoveGCLiveReg(REG_EAX, dst);
                    RemoveGCLiveReg(REG_EDX, dst);
                }
                break;
            default:
                assert((ins != INS_mulEAX) && (ins != INS_imulEAX));
                break;
        }
    }

    return dst;
}

uint8_t* X86Encoder::EncodeSV(uint8_t* dst, instrDesc* id, code_t code, ssize_t* imm)
{
    instruction ins = id->idIns();

    assert(ins != INS_imul || id->idReg1() == REG_EAX || id->idOpSize() == EA_4BYTE || id->idOpSize() == EA_8BYTE);
    // BT/CMOV support 16 bit operands and this code doesn't add the necessary 66 prefix.
    // BT with memory operands is practically useless and CMOV is not currently generated.
    assert((ins != INS_bt) && !IsCmov(ins));
    assert(TakesVexPrefix(ins) == HasVexPrefix(code));
    assert(!id->HasFSPrefix());

    unsigned immSize = 0;

    if (imm != nullptr)
    {
        immSize = IsShiftImm(ins) ? 1 : EA_SIZE_IN_BYTES(id->idOpSize());

        // 1. Instructions like "ADD rm16/32/64, imm16/32" have an alternate encoding if the imm fits in 8 bits.
        // 2. MOV/TEST do not have such encodings, they always need an imm16/32.
        // 3. Shifts and SSE/AVX instructions only have imm8 encodings, independent of the first operand size.
        if ((immSize > 1) && IsImm8(*imm) && !id->idIsCnsReloc() && (ins != INS_mov) && (ins != INS_test))
        {
            if (HasSBit(ins))
            {
                code |= 2;
            }

            immSize = 1;
        }
    }

    dst = EncodeOpcode(dst, id, code);

    assert(!id->idIsDspReloc());

    bool ebpBased  = id->idAddr()->isEbpBased;
    int  lclOffset = id->idAddr()->lclOffset;
    int  disp      = lclOffset;

#if !FEATURE_FIXED_OUT_ARGS
    if (!ebpBased)
    {
        disp += stackLevel;

        if (ins == INS_pop)
        {
            // The offset in "pop [ESP+xxx]" is relative to the new ESP value
            disp -= REGSIZE_BYTES;
        }
    }
#endif

    bool hasDisp8 = IsDisp8(disp);
    code |= ebpBased ? 0x0500 : 0x0400;

    if ((disp != 0) || ebpBased)
    {
        code |= hasDisp8 ? 0x4000 : 0x8000;
    }

    dst += Write16(dst, code);

    if (!ebpBased)
    {
        dst += Write8(dst, 0x24);
    }

    if ((disp != 0) || ebpBased)
    {
        if (hasDisp8)
        {
            dst += Write8(dst, disp);
        }
        else
        {
            dst += Write32(dst, disp);
        }
    }

    if (imm != nullptr)
    {
        dst += EncodeImm(dst, id, immSize, *imm);
    }

    if (id->idAddr()->isTrackedGCSlotStore
#if FEATURE_FIXED_OUT_ARGS
        || id->idAddr()->isGCArgStore
#endif
        )
    {
        assert((id->idIns() == INS_mov) && (id->idInsFmt() == IF_SWR_RRD));

        INDEBUG(int varNum = id->idDebugOnlyInfo()->varNum);

#if FEATURE_FIXED_OUT_ARGS
        if (id->idAddr()->isGCArgStore)
        {
            AddGCLiveCallArg(lclOffset, id->idGCref(), dst DEBUGARG(varNum));
        }
        else
#endif
        {
            AddGCLiveSlot(lclOffset, id->idGCref(), dst DEBUGARG(varNum));
        }

        return dst;
    }

    if (id->idGCref() != GCT_NONE)
    {
        switch (id->idInsFmt())
        {
            case IF_RRW_SRD:
                assert((id->idGCref() == GCT_BYREF) && ((ins == INS_add) || (ins == INS_sub)));
                FALLTHROUGH;
            case IF_RWR_SRD:
                AddGCLiveReg(id->idGCref(), id->idReg1(), dst);
                break;

            case IF_SRD:
            case IF_SRD_CNS:
            case IF_SRD_RRD:
            case IF_RRD_SRD:
            // Constants aren't GC pointers.
            case IF_SWR_CNS:
            // We assume that since we also read it was written previously so it's already live.
            case IF_SRW:
            case IF_SRW_CNS:
            case IF_SRW_RRD:
            // Already handled above.
            case IF_SWR:
            case IF_SWR_RRD:
                break;
            default:
                INDEBUG(printf("%s", EmitterBase::GetFormatName(id->idInsFmt())));
                assert(!"unexpected GC ref instruction format");
        }
    }
    else
    {
        switch (id->idInsFmt())
        {
            case IF_RRW_SRD:
            case IF_RRW_SRD_CNS:
            case IF_RWR_SRD:
            case IF_RWR_RRD_SRD:
            case IF_RWR_SRD_CNS:
            case IF_RWR_RRD_SRD_CNS:
            case IF_RWR_RRD_SRD_RRD:
                if (IsGeneralRegister(id->idReg1()))
                {
                    RemoveGCLiveReg(id->idReg1(), dst);
                }
                break;
            case IF_SRD:
                if ((ins == INS_mulEAX) || (ins == INS_imulEAX))
                {
                    RemoveGCLiveReg(REG_EAX, dst);
                    RemoveGCLiveReg(REG_EDX, dst);
                }
                break;
            default:
                assert((ins != INS_mulEAX) && (ins != INS_imulEAX));
                break;
        }
    }

    return dst;
}

uint8_t* X86Encoder::EncodeCV(uint8_t* dst, instrDesc* id, code_t code, ssize_t* imm)
{
    instruction ins  = id->idIns();
    ConstData*  data = id->GetConstData();
    ssize_t     disp = id->GetMemDisp();

    // BT/CMOV support 16 bit operands and this code doesn't add the necessary 66 prefix.
    // BT with memory operands is practically useless and CMOV is not currently generated.
    assert((ins != INS_bt) && !IsCmov(ins));
    assert(TakesVexPrefix(ins) == HasVexPrefix(code));
    assert(!id->HasFSPrefix());

    size_t addr = reinterpret_cast<size_t>(roDataBlock) + data->offset;

#ifdef DEBUG
    size_t align;

    switch (ins)
    {
        case INS_cvttss2si:
        case INS_cvtss2sd:
        case INS_vbroadcastss:
        case INS_insertps:
        case INS_movss:
            align = 4;
            break;
        case INS_vbroadcastsd:
        case INS_movsd:
            align = 8;
            break;
        case INS_vinsertf128:
        case INS_vinserti128:
            align = 16;
            break;
        case INS_lea:
            align = 1;
            break;
        default:
            align = EA_SIZE_IN_BYTES(id->idOpSize());
            break;
    }

    // Check that the offset is properly aligned (i.e. the ddd in [ddd])
    // When SMALL_CODE is set, we only expect 4-byte alignment, otherwise
    // we expect the same alignment as the size of the constant.
    // TODO-MIKE-Review: Figure out why this check is disabled in crossgen.
    assert(((addr & (align - 1)) == 0) || ((compiler->compCodeOpt() == SMALL_CODE) && ((addr & 3) == 0)) ||
           compiler->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT));
#endif // DEBUG

    disp += addr;

    unsigned immSize = 0;

#ifdef TARGET_X86
    // Special case: "mov eax, [addr]" and "mov [addr], eax" have smaller encoding.
    // x64 currently never uses the moffset format, it uses only RIP relative addressing.
    if ((ins == INS_mov) && (id->idReg1() == REG_EAX) && (imm == nullptr))
    {
        assert((id->idInsFmt() == IF_RWR_MRD) || (id->idInsFmt() == IF_MWR_RRD));

        if (id->idOpSize() == EA_2BYTE)
        {
            dst += Write8(dst, 0x66);
        }

        dst += Write8(dst, 0xA0 | ((id->idInsFmt() == IF_MWR_RRD) << 1) | (id->idOpSize() != EA_1BYTE));
    }
    else
#endif // TARGET_X86
    {
        if (imm != nullptr)
        {
            immSize = IsShiftImm(ins) ? 1 : EA_SIZE_IN_BYTES(id->idOpSize());

            // 1. Instructions like "ADD rm16/32/64, imm16/32" have an alternate encoding if the imm fits in 8 bits.
            // 2. MOV/TEST do not have such encodings, they always need an imm16/32.
            // 3. Shifts and SSE/AVX instructions only have imm8 encodings, independent of the first operand size.
            if ((immSize > 1) && IsImm8(*imm) && !id->idIsCnsReloc() && (ins != INS_mov) && (ins != INS_test))
            {
                if (HasSBit(ins))
                {
                    code |= 2;
                }

                immSize = 1;
            }
        }

        dst = EncodeOpcode(dst, id, code);
        dst += Write16(dst, code | 0x0500);
    }

    if (!id->idIsDspReloc())
    {
#ifdef TARGET_AMD64
        // All static field and data section constant accesses should be marked as relocatable
        unreached();
#else
        dst += Write32(dst, disp);
#endif
    }
    else
    {
        // For emitting relocation, we also need to take into account
        // the additional bytes of code emitted for imm.
        int32_t immRelocDelta = 0;

        if (imm != nullptr)
        {
#ifdef TARGET_AMD64
            noway_assert((immSize < 8) || (IsImm32(*imm) && !id->idIsCnsReloc()));
            immRelocDelta = -static_cast<int32_t>(Min(immSize, 4u));
#else
            noway_assert(immSize <= 4);
            immRelocDelta = -static_cast<int32_t>(immSize);
#endif
        }

#ifdef TARGET_AMD64
        // We emit zero on x64, to avoid the assert in Write32
        dst += Write32(dst, 0);
        RecordRelocation(dst - 4, reinterpret_cast<void*>(disp), IMAGE_REL_BASED_REL32, immRelocDelta);
#else
        dst += Write32(dst, disp);
        RecordRelocation(dst - 4, reinterpret_cast<void*>(disp), IMAGE_REL_BASED_HIGHLOW, immRelocDelta);
#endif
    }

    if (imm != nullptr)
    {
        dst += EncodeImm(dst, id, immSize, *imm);
    }

    if (id->idGCref())
    {
        switch (id->idInsFmt())
        {
            case IF_RWR_MRD:
                AddGCLiveReg(id->idGCref(), id->idReg1(), dst);
                break;
            case IF_RRW_MRD:
                assert(id->idGCref() == GCT_BYREF);
                assert((ins == INS_add) || (ins == INS_sub));
                AddGCLiveReg(GCT_BYREF, id->idReg1(), dst);
                break;
            case IF_MRD:
            case IF_MRW:
            case IF_MWR:
            case IF_RRD_MRD:
            case IF_MRD_RRD:
            case IF_MWR_RRD:
            case IF_MRW_RRD:
            case IF_MRD_CNS:
            case IF_MWR_CNS:
            case IF_MRW_CNS:
                break;
            default:
                INDEBUG(printf("%s", EmitterBase::GetFormatName(id->idInsFmt())));
                assert(!"unexpected GC ref instruction format");
        }
    }
    else
    {
        switch (id->idInsFmt())
        {
            case IF_RRW_MRD:
            case IF_RRW_MRD_CNS:
            case IF_RWR_MRD:
            case IF_RWR_RRD_MRD:
            case IF_RWR_MRD_CNS:
            case IF_RWR_RRD_MRD_CNS:
            case IF_RWR_RRD_MRD_RRD:
                if (IsGeneralRegister(id->idReg1()))
                {
                    RemoveGCLiveReg(id->idReg1(), dst);
                }
                break;
            case IF_MRD:
                if ((ins == INS_mulEAX) || (ins == INS_imulEAX))
                {
                    RemoveGCLiveReg(REG_EAX, dst);
                    RemoveGCLiveReg(REG_EDX, dst);
                }
                break;
            default:
                assert((ins != INS_mulEAX) && (ins != INS_imulEAX));
                break;
        }
    }

    return dst;
}

uint8_t* X86Encoder::EncodeR(uint8_t* dst, instrDesc* id)
{
    assert(!id->HasFSPrefix());

    instruction ins  = id->idIns();
    RegNum      reg  = id->idReg1();
    emitAttr    size = id->idOpSize();

    switch (ins)
    {
        code_t code;

        case INS_call:
            code = EncodeRMReg(INS_call, reg, EA_PTRSIZE, GetCodeMR(INS_call));
            dst += EncodeRexPrefixIfNeeded(dst, code);
            dst += Write16(dst, code);
            // Calls use a different mechanism to update GC info so we can skip the normal handling.
            return dst;

        case INS_inc:
        case INS_dec:
            if (size == EA_2BYTE)
            {
                dst += Write8(dst, 0x66);
            }

#ifdef TARGET_X86
            if (size != EA_1BYTE)
            {
                code = GetCodeRR(ins == INS_inc ? INS_inc_s : INS_dec_s);
                code |= RegEncoding(reg);
                dst += Write8(dst, code);

                break;
            }
#endif

            code = GetCodeMR(ins);

            if (size == EA_1BYTE)
            {
                assert(HasWBit(ins) && ((code & 1) != 0));
                code ^= 1;
            }
#ifdef TARGET_AMD64
            else if (size == EA_8BYTE)
            {
                code = AddRexWPrefix(code);
            }
#endif

            code |= (0xC0ull | EncodeReg012(ins, reg, size, &code)) << 8;
            dst += EncodeRexPrefixIfNeeded(dst, code);
            dst += Write16(dst, code);
            break;

        case INS_pop:
        case INS_pop_hide:
        case INS_push:
        case INS_push_hide:
            assert(size == EA_PTRSIZE);
            assert(!TakesVexPrefix(ins));
            AMD64_ONLY(assert(!TakesRexWPrefix(ins)));

            code = GetCodeRR(ins);
            code |= EncodeReg012(ins, reg, size, &code);
            dst += EncodeRexPrefixIfNeeded(dst, code);
            dst += Write8(dst, code);
            break;

        case INS_bswap:
            assert(size >= EA_4BYTE && size <= EA_PTRSIZE); // 16-bit BSWAP is undefined

            code = GetCodeRR(ins);

#ifdef TARGET_AMD64
            if (size == EA_8BYTE)
            {
                code = AddRexWPrefix(code);
            }
#endif

            // The Intel instruction set reference for BSWAP states that extended registers
            // should be enabled via REX.R, but per Vol. 2A, Sec. 2.2.1.2 (see also Figure 2-7),
            // REX.B should instead be used if the register is encoded in the opcode byte itself.
            // Therefore the default logic of EncodeReg012 is correct for this case.
            code |= EncodeReg012(ins, reg, size, &code) << 8;
            dst += EncodeRexPrefixIfNeeded(dst, code);
            dst += Write16(dst, code);
            break;

        case INS_seto:
        case INS_setno:
        case INS_setb:
        case INS_setae:
        case INS_sete:
        case INS_setne:
        case INS_setbe:
        case INS_seta:
        case INS_sets:
        case INS_setns:
        case INS_setp:
        case INS_setnp:
        case INS_setl:
        case INS_setge:
        case INS_setle:
        case INS_setg:
            assert(id->idGCref() == GCT_NONE);
            assert(size == EA_1BYTE);

            code = EncodeRMReg(ins, reg, EA_1BYTE, GetCodeMR(ins));
            dst += EncodeRexPrefixIfNeeded(dst, code);
            dst += Write8(dst, 0x0F);
            dst += Write16(dst, code & 0x0000FFFF);
            break;

        case INS_mulEAX:
        case INS_imulEAX:
            RemoveGCLiveReg(REG_EAX, dst);
            RemoveGCLiveReg(REG_EDX, dst);
            FALLTHROUGH;
        default:
            assert(!IsSSEOrAVXOrBMIInstruction(ins));
            assert(!TakesVexPrefix(ins));
            assert(id->idGCref() == GCT_NONE);

            code = EncodeRMReg(ins, reg, size, GetCodeMR(ins));

            if (size == EA_1BYTE)
            {
                assert(HasWBit(ins) && ((code & 1) != 0));
                code ^= 1;
            }
            else if (size == EA_2BYTE)
            {
                dst += Write8(dst, 0x66);
            }
#ifdef TARGET_AMD64
            else if ((size == EA_8BYTE) && TakesRexWPrefix(ins))
            {
                code = AddRexWPrefix(code);
            }
#endif

            dst += EncodeRexPrefixIfNeeded(dst, code);
            dst += Write16(dst, code);
            break;
    }

    switch (id->idInsFmt())
    {
        case IF_RWR:
            if (id->idGCref())
            {
                AddGCLiveReg(id->idGCref(), id->idReg1(), dst);
            }
            else
            {
                RemoveGCLiveReg(id->idReg1(), dst);
            }
            break;

        case IF_RRW:
            if (id->idGCref())
            {
                assert(ins == INS_inc || ins == INS_dec);

                // We would like to assert that the reg must currently be holding either a gcref or a byref.
                // However, we can see cases where a LCLHEAP generates a non-gcref value into a register,
                // and the first instruction we generate after the LCLHEAP is an `inc` that is typed as
                // byref. We'll properly create the byref gcinfo when this happens.
                // assert((gcInfo.GetAllLiveRegs() & genRegMask(reg)) != RBM_NONE);

                assert(id->idGCref() == GCT_BYREF);

                AddGCLiveReg(GCT_BYREF, id->idReg1(), dst);
            }
            else
            {
                assert((gcInfo.GetLiveRegs(GCT_GCREF) & genRegMask(reg)) == RBM_NONE);
            }
            break;

        case IF_RRD:
            break;
        default:
            INDEBUG(printf("%s", EmitterBase::GetFormatName(id->idInsFmt())));
            assert(!"unexpected instruction format");
            break;
    }

    return dst;
}

X86Emitter::code_t X86Encoder::AddVexPrefixIfNeeded(instruction ins, code_t code, emitAttr size)
{
    if (TakesVexPrefix(ins))
    {
        code = AddVexPrefix(ins, code, size);
    }

    return code;
}

uint8_t* X86Encoder::EncodeRR(uint8_t* dst, instrDesc* id)
{
    assert(!id->HasFSPrefix());

    instruction ins  = id->idIns();
    emitAttr    size = id->idOpSize();
    RegNum      reg1 = id->idReg1();
    RegNum      reg2 = id->idReg2();
    RegNum      rReg = reg1;
    RegNum      bReg = reg2;
    code_t      code;

    if (IsSSEOrAVXOrBMIInstruction(ins))
    {
        assert((ins != INS_movd) || (IsFloatReg(reg1) != IsFloatReg(reg2)));

        if ((ins != INS_movd) || IsFloatReg(reg1))
        {
            code = GetCodeRM(ins);
        }
        else
        {
            code = GetCodeMR(ins);
        }

        code = AddVexPrefixIfNeeded(ins, code, size);

#ifdef TARGET_AMD64
        if ((size == EA_8BYTE) && TakesRexWPrefix(ins))
        {
            code = AddRexWPrefix(code);
        }
#endif

        if (TakesVexPrefix(ins))
        {
            // In case of AVX instructions that take 3 operands, we generally want to encode reg1
            // as first source.  In this case, reg1 is both a source and a destination.
            // The exception is the "merge" 3-operand case, where we have a move instruction, such
            // as movss, and we want to merge the source with itself.
            //
            // TODO-XArch-CQ: Eventually we need to support 3 operand instruction formats. For
            // now we use the single source as source1 and source2.
            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(reg1, size, code);

                if (IsBMIRegExtInstruction(ins))
                {
                    // TODO-MIKE-Cleanup: These instructions put the destination register in VEX.vvvv and use
                    // the reg field of the RM byte as an opcode extension, which the rest of the code does
                    // not expect. Can avoid going back and forth between extension and register? Maybe after
                    // 4-byte opcodes are gone and we always have a RM byte.

                    rReg = static_cast<RegNum>((code >> 11) & 7);
                    code &= ~0xFF00ull;
                }
            }
            else if (IsVexDstSrcSrc(ins))
            {
                code = SetVexVvvv(reg2, size, code);
            }
        }

        if (ins == INS_movd)
        {
            assert(IsFloatReg(reg1) != IsFloatReg(reg2));

            if (IsFloatReg(reg2))
            {
                std::swap(bReg, rReg);
            }
        }
    }
    else if ((ins == INS_movsx) || (ins == INS_movzx))
    {
        assert(!HasCodeMI(ins) && !HasCodeMR(ins));
        assert(size < EA_4BYTE);

        code = GetCodeRM(ins);

        if (size == EA_1BYTE)
        {
            assert(HasWBit(ins) && ((code & 1) != 0));
            code ^= 1;
        }

        AMD64_ONLY(assert((ins == INS_movsx) == (((code >> (RexBitOffset + 3)) & 1) != 0)));
    }
#ifdef TARGET_AMD64
    else if (ins == INS_movsxd)
    {
        assert(!HasCodeMI(ins) && !HasCodeMR(ins));

        code = GetCodeRM(ins);
        assert(((code >> (RexBitOffset + 3)) & 1) != 0);
    }
#endif
    else if (ins == INS_imul)
    {
        assert(!TakesVexPrefix(ins));
        assert(size != EA_1BYTE);

        code = GetCodeRM(ins);

        // TODO-MIKE-Cleanup: There should be no need to generate a 16 bit imul.
        if (size == EA_2BYTE)
        {
            assert(((code >> PpBitOffset) & 3) == 0);
            code |= 1ull << PpBitOffset;
        }
#ifdef TARGET_AMD64
        else if (size == EA_8BYTE)
        {
            code = AddRexWPrefix(code);
        }
#endif
    }
    else if (ins == INS_bt)
    {
        assert(!TakesVexPrefix(ins));
        assert(size != EA_1BYTE);

        code = GetCodeMR(ins);

        // TODO-MIKE-Cleanup: There should be no need to generate a 16 bit bt.
        if (size == EA_2BYTE)
        {
            assert(((code >> PpBitOffset) & 3) == 0);
            code |= 1ull << PpBitOffset;
        }
#ifdef TARGET_AMD64
        else if (size == EA_8BYTE)
        {
            code = AddRexWPrefix(code);
        }
#endif

        std::swap(rReg, bReg);
    }
    else if ((ins == INS_bsf) || (ins == INS_bsr) || (ins == INS_crc32) || (ins == INS_lzcnt) || (ins == INS_popcnt) ||
             (ins == INS_tzcnt) || IsCmov(ins))
    {
        assert(!HasCodeMI(ins) && !HasCodeMR(ins) && !TakesVexPrefix(ins));

        code = GetCodeRM(ins);

        if (size == EA_1BYTE)
        {
            noway_assert(ins == INS_crc32);
            code ^= 1;
        }
        else if (size == EA_2BYTE)
        {
            // We need to emit 0x66 now, instead of adding it to code's prefixes,
            // crc32 already has a mandatory prefix and can also have a size prefix.
            dst += Write8(dst, 0x66);
        }
#ifdef TARGET_AMD64
        else if (size == EA_8BYTE)
        {
            code = AddRexWPrefix(code);
        }
#endif
    }
    else
    {
        assert(!TakesVexPrefix(ins));

        code = GetCodeMR(ins);

        if (ins != INS_test)
        {
            code |= 2;
        }

        switch (size)
        {
            case EA_1BYTE:
#ifdef TARGET_X86
                noway_assert(RBM_BYTE_REGS & genRegMask(reg1));
                noway_assert(RBM_BYTE_REGS & genRegMask(reg2));
#endif
                assert(HasWBit(ins) && ((code & 1) != 0));
                code ^= 1;
                break;

            case EA_2BYTE:
                assert(((code >> PpBitOffset) & 3) == 0);
                code |= 1ull << PpBitOffset;
                break;
            case EA_4BYTE:
                break;
#ifdef TARGET_AMD64
            case EA_8BYTE:
                // TODO-AMD64-CQ: Better way to not emit REX.W when we don't need it
                // Don't need to zero out the high bits explicitly
                if ((ins != INS_xor) || (reg1 != reg2))
                {
                    code = AddRexWPrefix(code);
                }
                else
                {
                    id->idOpSize(EA_4BYTE);
                }
                break;
#endif // TARGET_AMD64

            default:
                assert(!"unexpected size");
        }
    }

    assert((code & 0xFF00) == 0);

    code |= (0xC0 | EncodeReg345(ins, rReg, size, &code) | EncodeReg012(ins, bReg, size, &code)) << 8;
    dst += EncodeRexOrVexPrefixIfNeeded(dst, code DEBUGARG(ins));
    dst += Write16(dst, code);

    if (id->idGCref())
    {
        switch (id->idInsFmt())
        {
            case IF_RWR_RRD:
                AddGCLiveReg(id->idGCref(), reg1, dst);
                break;

            case IF_RRW_RRD:
                switch (id->idIns())
                {
                    // This must be one of the following cases:
                    //
                    // xor reg, reg        to assign NULL
                    //
                    // and r1 , r2         if (ptr1 && ptr2) ...
                    // or  r1 , r2         if (ptr1 || ptr2) ...
                    //
                    // add r1 , r2         to compute a normal byref
                    // sub r1 , r2         to compute a strange byref (VC only)

                    case INS_xor:
                        assert(reg1 == reg2);
                        AddGCLiveReg(id->idGCref(), reg1, dst);
                        break;
                    case INS_or:
                    case INS_and:
                        RemoveGCLiveReg(reg1, dst);
                        break;
                    case INS_add:
                    case INS_sub:
                        assert(id->idGCref() == GCT_BYREF);
#if 0
                        // Due to elided register moves, we can't have the following assert.
                        // For example, consider:
                        //    t85 = LCL_VAR byref V01 arg1 rdx (last use) REG rdx
                        //        /--*  t85    byref                                                       
                        //        *  STORE_LCL_VAR byref  V40 tmp31 rdx REG rdx                 
                        // Here, V01 is type `long` on entry, then is stored as a byref. But because
                        // the register allocator assigned the same register, no instruction was
                        // generated, and we only (currently) make gcref/byref changes in X86Emitter GC info
                        // when an instruction is generated. We still generate correct GC info, as this
                        // instruction, if writing a GC ref even through reading a long, will go live here.
                        // These situations typically occur due to unsafe casting, such as with Span<T>.

                        regMaskTP regMask;
                        regMask = genRegMask(reg1) | genRegMask(reg2);

                        // r1/r2 could have been a GCREF as GCREF + int=BYREF
                        //                               or BYREF+/-int=BYREF
                        assert((((regMask & gcInfo.GetLiveRegs(GCT_GCREF) != RBM_NONE) && (ins == INS_add)) ||
                               (((regMask & gcInfo.GetLiveRegs(GCT_BYREF) != RBM_NONE) && (ins == INS_add || ins == INS_sub)));
#endif // 0
                        AddGCLiveReg(GCT_BYREF, reg1, dst);
                        break;

                    default:
                        INDEBUG(printf("%s", EmitterBase::GetFormatName(id->idInsFmt())));
                        assert(!"unexpected GC base update instruction");
                }
                break;

            case IF_RRW_RRW:
                assert(id->idIns() == INS_xchg);

                GCtype gc1;
                gc1 = gcInfo.GetRegType(reg1);
                GCtype gc2;
                gc2 = gcInfo.GetRegType(reg2);

                if (gc1 != gc2)
                {
                    if (gc1 != GCT_NONE)
                    {
                        RemoveGCLiveReg(reg1, dst);
                    }

                    if (gc2 != GCT_NONE)
                    {
                        RemoveGCLiveReg(reg2, dst);
                    }

                    if (gc1 != GCT_NONE)
                    {
                        AddGCLiveReg(gc1, reg2, dst);
                    }

                    if (gc2 != GCT_NONE)
                    {
                        AddGCLiveReg(gc2, reg1, dst);
                    }
                }
                break;

            case IF_RRD_RRD:
                break;

            default:
                INDEBUG(printf("%s", EmitterBase::GetFormatName(id->idInsFmt())));
                assert(!"unexpected GC ref instruction format");
        }
    }
    else
    {
        switch (id->idInsFmt())
        {
            // TODO-MIKE-Review: What about IF_RRW_RRW (xchg)?
            case IF_RRW_RRD:
            case IF_RWR_RRD:
                if (IsGeneralRegister(reg1))
                {
                    RemoveGCLiveReg(reg1, dst);
                }
                break;
            default:
                assert((ins != INS_mulEAX) && (ins != INS_imulEAX) && (ins != INS_imuli));
                break;
        }
    }

    return dst;
}

uint8_t* X86Encoder::EncodeRRR(uint8_t* dst, instrDesc* id)
{
    assert(IsVexTernary(id->idIns()));
    assert((id->idInsFmt() == IF_RWR_RRD_RRD) || (id->idInsFmt() == IF_RWR_RRD_RRD_CNS) ||
           (id->idInsFmt() == IF_RWR_RRD_RRD_RRD));
    assert(!id->HasFSPrefix());

    instruction ins  = id->idIns();
    RegNum      reg1 = id->idReg1();
    RegNum      reg2 = id->idReg2();
    RegNum      reg3 = id->idReg3();
    emitAttr    size = id->idOpSize();

    code_t code = GetCodeRM(ins);
    assert((code & 0xFF00) == 0);
    code = AddVexPrefix(ins, code, size);

#ifdef TARGET_AMD64
    if ((size == EA_8BYTE) && TakesRexWPrefix(ins))
    {
        code = AddRexWPrefix(code);
    }
#endif

    code |= (0xC0 | EncodeReg345(ins, reg1, size, &code) | EncodeReg012(ins, reg3, size, &code)) << 8;
    code = SetVexVvvv(reg2, size, code);
    dst += EncodeVexPrefix(dst, code DEBUGARG(ins));
    dst += Write16(dst, code);

    if (IsGeneralRegister(id->idReg1()))
    {
        noway_assert(!id->idGCref());
        RemoveGCLiveReg(id->idReg1(), dst);
    }

    return dst;
}

uint8_t* X86Encoder::EncodeRRI(uint8_t* dst, instrDesc* id)
{
    assert(!id->HasFSPrefix());

    instruction ins  = id->idIns();
    emitAttr    size = id->idOpSize();

    RegNum bReg;
    RegNum rReg;
    code_t code;

    if (HasCodeMR(ins))
    {
        code = GetCodeMR(ins);
        code = AddVexPrefixIfNeeded(ins, code, size);

        bReg = id->idReg1();
        rReg = id->idReg2();
    }
    else if (HasCodeMI(ins))
    {
        assert((INS_psrldq <= ins) && (ins <= INS_psrad) && UseVEXEncoding());

        code = GetCodeMI(ins);
        code = AddVexPrefixIfNeeded(ins, code, size);

        bReg = id->idReg2();
        rReg = static_cast<RegNum>((code >> 11) & 7);
    }
    else
    {
        code = GetCodeRM(ins);
        code = AddVexPrefixIfNeeded(ins, code, size);

        bReg = id->idReg2();
        rReg = id->idReg1();

        if ((ins == INS_imuli) && (id->idOpSize() > EA_1BYTE))
        {
            // Don't bother with 16 bit imul, which needs the 66 prefix, we only need 32/64 bit.
            assert(id->idOpSize() != EA_2BYTE);

            code |= 1;

            if (IsImm8(id->GetImm()))
            {
                code |= 2;
            }
        }
    }

    assert(((code & 0x00FF0000) != 0) || (id->idIns() == INS_imuli));

#ifdef TARGET_AMD64
    if ((size == EA_8BYTE) && TakesRexWPrefix(ins))
    {
        code = AddRexWPrefix(code);
    }
#endif

    if (TakesVexPrefix(ins))
    {
        if (IsVexDstDstSrc(ins))
        {
            // This code will have to change when we support 3 operands.
            // For now, we always overload this source with the destination (always reg1).
            // (Though we will need to handle the few ops that can have the 'vvvv' bits as destination,
            // e.g. pslldq, when/if we support those instructions with 2 registers.)
            // (see x64 manual Table 2-9. Instructions with a VEX.vvvv destination)
            code = SetVexVvvv(id->idReg1(), size, code);
        }
        else if (IsVexDstSrcSrc(ins))
        {
            // This is a "merge" move instruction.
            code = SetVexVvvv(id->idReg2(), size, code);
        }
    }

    assert(((code & 0xFF00) == 0) || ((INS_psrldq <= ins) && (ins <= INS_psrad)));

    code |= (0xC0 | EncodeReg345(ins, rReg, size, &code) | EncodeReg012(ins, bReg, size, &code)) << 8;
    dst += EncodeRexOrVexPrefixIfNeeded(dst, code DEBUGARG(ins));
    dst += Write16(dst, code);

    if ((ins == INS_imuli) && ((code & 0x02) == 0))
    {
        dst += Write32(dst, id->GetImm());
    }
    else
    {
        dst += Write8(dst, id->GetImm());
    }

    assert(id->idGCref() == GCT_NONE);

    if (IsGeneralRegister(id->idReg1()))
    {
        RemoveGCLiveReg(id->idReg1(), dst);
    }

    return dst;
}

uint8_t* X86Encoder::EncodeRI(uint8_t* dst, instrDesc* id)
{
    assert(!id->HasFSPrefix());

    emitAttr    size = id->idOpSize();
    instruction ins  = id->idIns();
    RegNum      reg  = id->idReg1();
    ssize_t     imm  = id->GetImm();

    // BT reg,imm might be useful but it requires special handling of the immediate value
    // (it is always encoded in a byte). Let's not complicate things until this is needed.
    assert(ins != INS_bt);

    if (IsSSEOrAVXOrBMIInstruction(ins))
    {
        assert((INS_psrldq <= ins) && (ins <= INS_psrad));
        assert(id->idGCref() == GCT_NONE);
        assert(IsImm8(imm));

        code_t code = GetCodeMI(ins);
        assert((code & 0x00FF0000) != 0);

        code = AddVexPrefixIfNeeded(ins, code, size);
        code = EncodeRMReg(ins, reg, size, code);

        if (TakesVexPrefix(ins))
        {
            code = SetVexVvvv(reg, size, code);
        }

        dst += EncodeRexOrVexPrefixIfNeeded(dst, code DEBUGARG(ins));
        dst += Write16(dst, code);
        dst += Write8(dst, imm);

        return dst;
    }

    X86_ONLY(noway_assert(VerifyEncodable(ins, size, reg)));

    if (ins == INS_mov)
    {
        assert(id->idInsFmt() == IF_RWR_CNS);

        // TODO-MIKE-Cleanup: 0xB8 could go into the RR encoding table. But there
        // is little point in doing that since we need to special case this anyway.
        // Or perhaps there's a way to avoid this special casing?
        code_t code = 0xB8;
        code |= EncodeReg012(ins, reg, size, &code);

        assert(!TakesVexPrefix(ins));

#ifdef TARGET_AMD64
        if (size == EA_8BYTE)
        {
            code = AddRexWPrefix(code);
        }
#endif

        dst += EncodeRexPrefixIfNeeded(dst, code);
        dst += Write8(dst, code);

#ifdef TARGET_X86
        assert(size == EA_4BYTE);
        dst += Write32(dst, imm);

        if (id->idIsCnsReloc())
        {
            RecordRelocation(dst - 4, reinterpret_cast<void*>(imm), IMAGE_REL_BASED_HIGHLOW);
        }
#else
        if (size == EA_4BYTE)
        {
            dst += Write32(dst, imm);
            assert(!id->idIsCnsReloc());
        }
        else
        {
            assert(size == EA_8BYTE);
            dst += Write64(dst, imm);

            if (id->idIsCnsReloc())
            {
                RecordRelocation(dst - 8, reinterpret_cast<void*>(imm), IMAGE_REL_BASED_DIR64);
            }
        }
#endif

        // TODO-MIKE-Cleanup: A constant can't be a GC ref so we should not need to check idGCref.
        // But of course, it's messed up so constants are sometimes byrefs. Probably we could simply
        // ignore that and just call RemoveGCLiveReg, but we may get asserts later on because the
        // GC info is out of sync.

        if (id->idGCref())
        {
            AddGCLiveReg(id->idGCref(), reg, dst);
        }
        else
        {
            RemoveGCLiveReg(reg, dst);
        }

        return dst;
    }

    if (IsShiftImm(ins))
    {
        assert(!TakesVexPrefix(ins));

        code_t code = EncodeRMReg(ins, id->idReg1(), size, GetCodeMI(ins));

        if (size == EA_1BYTE)
        {
            assert(HasWBit(ins) && ((code & 1) != 0));
            code ^= 1;
        }
        else if (size == EA_2BYTE)
        {
            dst += Write8(dst, 0x66);
        }
#ifdef TARGET_AMD64
        else if (size == EA_8BYTE)
        {
            code = AddRexWPrefix(code);
        }
#endif

        dst += EncodeRexPrefixIfNeeded(dst, code);
        dst += Write16(dst, code);
        dst += Write8(dst, id->GetImm());

        assert(!id->idGCref());
        RemoveGCLiveReg(id->idReg1(), dst);

        return dst;
    }

    // This has to be one of the instructions that have the special EAX,imm encodings.
    assert(GetCodeA(ins) != BAD_CODE);

    size_t immSize = EA_SIZE_IN_BYTES(size);
    bool   useImm8 = IsImm8(imm) && !id->idIsCnsReloc() && (size != EA_1BYTE) && (ins != INS_test);
    bool   useACC  = (reg == REG_EAX) && !useImm8;
    code_t code;

    if (useACC)
    {
        code = GetCodeA(ins);
    }
    else
    {
        code = GetCodeMI(ins);
        code = EncodeRMReg(ins, reg, size, code);

        if (useImm8)
        {
            code |= 0x2; // Set the 's' bit to use a sign-extended immediate byte.
            immSize = 1;
        }
    }

    if (size == EA_1BYTE)
    {
        assert(HasWBit(ins) && ((code & 1) != 0));
        code ^= 1;
    }
    else if (size == EA_2BYTE)
    {
        dst += Write8(dst, 0x66);
    }
#ifdef TARGET_AMD64
    else if (size == EA_8BYTE)
    {
        code = AddRexWPrefix(code);
    }
#endif

    dst += EncodeRexPrefixIfNeeded(dst, code);

    if (useACC)
    {
        dst += Write8(dst, code);
    }
    else
    {
        dst += Write16(dst, code);
    }

    dst += EncodeImm(dst, id, immSize, imm);

    if (id->idGCref())
    {
        assert(ins != INS_mulEAX && ins != INS_imulEAX && ins != INS_imuli);

        switch (id->idInsFmt())
        {
            case IF_RWR_CNS:
                AddGCLiveReg(id->idGCref(), id->idReg1(), dst);
                break;

            case IF_RRW_CNS:
                assert(id->idGCref() == GCT_BYREF);
#ifdef DEBUG
                regMaskTP regMask;
                regMask = genRegMask(reg);

                // FIXNOW review the other places and relax the assert there too
                // The reg must currently be holding either a gcref or a byref
                // GCT_GCREF+int = GCT_BYREF, and GCT_BYREF+/-int = GCT_BYREF
                if ((gcInfo.GetLiveRegs(GCT_GCREF) & regMask) != RBM_NONE)
                {
                    assert(ins == INS_add);
                }

                if ((gcInfo.GetLiveRegs(GCT_BYREF) & regMask) != RBM_NONE)
                {
                    assert(ins == INS_add || ins == INS_sub);
                }
#endif
                AddGCLiveReg(GCT_BYREF, id->idReg1(), dst);
                break;

            case IF_RRD_CNS:
                break;
            default:
                INDEBUG(printf("%s", EmitterBase::GetFormatName(id->idInsFmt())));
                assert(!"unexpected GC ref instruction format");
        }
    }
    else
    {
        switch (id->idInsFmt())
        {
            case IF_RRW_CNS:
            case IF_RWR_CNS:
                assert(ins != INS_imuli);
                RemoveGCLiveReg(id->idReg1(), dst);
                break;
            case IF_RRD_CNS:
                assert(ins != INS_mulEAX && ins != INS_imulEAX && ins != INS_imuli);
                break;
            default:
                INDEBUG(printf("%s", EmitterBase::GetFormatName(id->idInsFmt())));
                assert(!"unexpected GC ref instruction format");
        }
    }

    return dst;
}

uint8_t* X86Encoder::EncodeI(uint8_t* dst, instrDesc* id)
{
    assert(!id->HasFSPrefix());

    instruction ins  = id->idIns();
    emitAttr    size = id->idOpSize();
    ssize_t     val  = id->GetImm();

#ifdef TARGET_AMD64
    assert((ins == INS_push_hide) && (size == EA_8BYTE) && (val == 0) && !id->idIsCnsReloc());
    dst += Write16(dst, 0x006A);
#else
    if (ins == INS_ret)
    {
        assert((val != 0) && !id->idIsCnsReloc());

        dst += Write8(dst, GetCodeMI(ins));
        dst += Write16(dst, val);
    }
    else
    {
        assert((ins == INS_push) || (ins == INS_push_hide));

        code_t code = GetCodeMI(ins);

        if (IsImm8(val) && !id->idIsCnsReloc())
        {
            dst += Write8(dst, code | 2);
            dst += Write8(dst, val);
        }
        else
        {
            dst += Write8(dst, code);
            dst += Write32(dst, val);

            if (id->idIsCnsReloc())
            {
                RecordRelocation(dst - 4, reinterpret_cast<void*>(val), IMAGE_REL_BASED_HIGHLOW);
            }
        }
    }
#endif // TARGET_X86

    return dst;
}

uint8_t* X86Encoder::EncodeRoDataAddr(uint8_t* dst, instrDescJmp* id, insGroup* ig)
{
    assert(id->idInsFmt() == IF_RWR_LABEL);
    assert(id->idOpSize() == EA_PTRSIZE);
    assert(id->idGCref() == GCT_NONE);
    assert(!id->HasFSPrefix());

#ifdef TARGET_X86
    assert(id->idIns() == INS_mov);

    uint8_t* labelAddr;

    if (id->HasConstData())
    {
        labelAddr = roDataBlock + id->GetConstData()->offset;
    }
    else
    {
        unsigned instrOffs = GetCodeOffset(dst);
        uint8_t* instrAddr = GetCodeAddr(instrOffs);
        unsigned labelOffs = id->GetLabel()->igOffs;

        labelAddr = GetCodeAddr(labelOffs);
    }

    assert((0 <= id->idReg1()) && id->idReg1() <= 7);

    dst += Write8(dst, 0xB8 + id->idReg1());

    if (id->idIsCnsReloc())
    {
        dst += Write32(dst, 0);
        RecordRelocation(dst - 4, labelAddr, IMAGE_REL_BASED_HIGHLOW);
    }
    else
    {
        dst += Write32(dst, reinterpret_cast<size_t>(labelAddr));
    }
#else
    assert(id->idIns() == INS_lea);

    unsigned instrOffs = GetCodeOffset(dst);
    uint8_t* instrAddr = GetCodeAddr(instrOffs);
    unsigned labelOffs = id->GetLabel()->igOffs;
    uint8_t* labelAddr = GetCodeAddr(labelOffs);

    code_t code = 0x8D;
    assert(GetCodeRM(INS_lea) == code);
    code = AddRexWPrefix(code);
    code = SetRMReg(INS_lea, id->idReg1(), EA_PTRSIZE, code);
    dst += EncodeRexPrefix(dst, code);
    dst += Write16(dst, code | 0x0500);

    if (id->idIsCnsReloc())
    {
        dst += Write32(dst, 0);
        RecordRelocation(dst - 4, labelAddr, IMAGE_REL_BASED_REL32);
    }
    else
    {
        ssize_t distance = labelAddr - instrAddr - id->idCodeSize();
        assert(IsImm32(distance));
        dst += Write32(dst, distance);
    }
#endif

    RemoveGCLiveReg(id->idReg1(), dst);

    return dst;
}

#ifdef TARGET_X86
uint8_t* X86Encoder::EncodePushLabel(uint8_t* dst, instrDescJmp* id, insGroup* ig)
{
    assert(id->idIns() == INS_push_hide);
    assert(id->idInsFmt() == IF_LABEL);
    assert(id->idGCref() == GCT_NONE);
    assert(!id->HasInstrCount());
    assert(!id->HasFSPrefix());

    unsigned labelOffs = id->GetLabel()->igOffs;
    uint8_t* labelAddr = GetCodeAddr(labelOffs);

    assert(GetCodeMI(INS_push) == 0x68);
    dst += Write8(dst, 0x68);

    if (id->idIsCnsReloc())
    {
        dst += Write32(dst, 0);
        RecordRelocation(dst - 4, labelAddr, IMAGE_REL_BASED_HIGHLOW);
    }
    else
    {
        dst += Write32(dst, reinterpret_cast<ssize_t>(labelAddr));
    }

    return dst;
}
#endif // TARGET_X86

uint8_t* X86Encoder::EncodeJump(uint8_t* dst, instrDescJmp* id, insGroup* ig)
{
    assert(id->idInsFmt() == IF_LABEL);
    assert(id->idGCref() == GCT_NONE);
    assert(!id->HasFSPrefix());

    unsigned labelOffs;

    if (id->HasInstrCount())
    {
        assert(ig != nullptr);

        int      instrCount   = id->GetInstrCount();
        unsigned jumpInstrNum = ig->FindInsNum(id);

        assert((instrCount >= 0) || (jumpInstrNum + 1 >= static_cast<unsigned>(-instrCount)));

        labelOffs = ig->igOffs + ig->FindInsOffset(jumpInstrNum + 1 + instrCount);
    }
    else
    {
        labelOffs = id->GetLabel()->igOffs;
    }

    uint8_t*    labelAddr = GetCodeAddr(labelOffs);
    unsigned    instrOffs = GetCodeOffset(dst);
    uint8_t*    instrAddr = GetCodeAddr(instrOffs);
    ssize_t     distance  = labelAddr - instrAddr - id->idCodeSize();
    instruction ins       = id->idIns();

    if (id->idCodeSize() == JMP_JCC_SIZE_SMALL)
    {
        assert(!id->idIsCnsReloc());
        assert(!IsHotColdJump(instrOffs, labelOffs));
        assert(ins != INS_call);
        assert(IsImm8(distance));

        dst += Write8(dst, GetCodeJ(ins));
        dst += Write8(dst, distance);
    }
    else if (id->idCodeSize() == 0)
    {
        assert(distance == 0);
    }
    else
    {
        AMD64_ONLY(assert(IsImm32(distance)));

        if (ins == INS_jmp)
        {
            dst += Write8(dst, 0xE9);
        }
#ifdef FEATURE_EH_FUNCLETS
        else if (ins == INS_call)
        {
            dst += Write8(dst, 0xE8);
        }
#endif
        else
        {
            code_t code = GetCodeJ(ins);
            assert((0x70 <= code) && (code <= 0x7F));
            dst += Write16(dst, 0x0F | ((code + 0x10) << 8));
        }

        dst += Write32(dst, distance);

        if (id->idIsCnsReloc())
        {
#ifdef TARGET_AMD64
            RecordRelocation(dst - 4, dst + distance, IMAGE_REL_BASED_REL32);
#else
            RecordRelocation(dst - 4, dst + distance, IMAGE_REL_BASED_HIGHLOW);
#endif
        }

#ifdef FEATURE_EH_FUNCLETS
        // Calls to "finally" handlers kill all registers.
        // TODO-MIKE-Review: It's not entirely clear why is this needed. Such calls are
        // always at the end of the block and the following block should have the correct
        // CG reg liveness. Sometimes, a nop is inserted after such calls, but then it's
        // inserted in a no-GC region so it shouldn't matter if some GC regs are killed
        // later than they really are. In fact, we already kill "later", the regs that
        // could possibly be live after the call are callee saved registers, and these
        // are practically dead before the call.
        // But the weirdest thing is that ARM64 doesn't appear to be doing this.
        // It certainly doesn't call RemoveAllGCLiveRegs but perhaps it achieves the same
        // effect by other means?
        // Anyway, even if this code is useless removing it results in GC info diffs.
        if ((ins == INS_call) && (gcInfo.GetAllLiveRegs() != RBM_NONE))
        {
            RemoveAllGCLiveRegs(dst);
        }
#else
        assert(ins != INS_call);
#endif
    }

    return dst;
}

uint8_t* X86Encoder::EncodeCall(uint8_t* dst, instrDesc* id)
{
    assert(!id->HasFSPrefix());

    instruction ins = id->idIns();

    void* addr = id->GetAddr();

    if (id->idInsFmt() == IF_METHPTR)
    {
        code_t code = GetCodeMR(ins);

        if (id->idIsDspReloc())
        {
            dst += Write16(dst, code | 0x0500);

#ifdef TARGET_AMD64
            dst += Write32(dst, 0);
            RecordRelocation(dst - 4, addr, IMAGE_REL_BASED_REL32);
#else
            dst += Write32(dst, reinterpret_cast<ssize_t>(addr));
            RecordRelocation(dst - 4, addr, IMAGE_REL_BASED_HIGHLOW);
#endif
        }
        else
        {
#ifdef TARGET_X86
            dst += Write16(dst, code | 0x0500);
#else
            noway_assert(!compiler->opts.compReloc);
            noway_assert(!compiler->eeIsRIPRelativeAddress(addr));
            noway_assert(IsDisp32(reinterpret_cast<intptr_t>(addr)));

            dst += Write16(dst, code | 0x0400);
            dst += Write8(dst, 0x25);
#endif

            dst += Write32(dst, reinterpret_cast<intptr_t>(addr));
        }
    }
    else
    {
        assert(id->idInsFmt() == IF_METHOD);

        dst += Write8(dst, ins == INS_l_jmp ? GetCodeJ(ins) : GetCodeMI(ins));

#ifdef TARGET_AMD64
        // For x64 we always go through recordRelocation since we may need jump stubs,
        // we'll just use offset 0 since recordRelocation overwrites it anyway.
        noway_assert(id->idIsDspReloc());

        const ssize_t offset = 0;
#else
        const ssize_t offset = reinterpret_cast<uint8_t*>(addr) - dst - 4;
#endif
        dst += Write32(dst, offset);

        if (id->idIsDspReloc())
        {
            RecordRelocation(dst - 4, addr, IMAGE_REL_BASED_REL32);
        }
    }

    return dst;
}

uint8_t* X86Encoder::EncodeNoOperands(uint8_t* dst, instrDesc* id)
{
    assert(!id->HasFSPrefix());

    instruction ins = id->idIns();

    if (ins == INS_nop)
    {
        return EncodeNop(dst + writeableOffset, id->idCodeSize()) - writeableOffset;
    }

    assert(!TakesVexPrefix(ins));
    assert(id->idOpSize() != EA_2BYTE);
    assert(id->idGCref() == GCT_NONE);

    code_t code = GetCodeMR(ins);
    assert((code >> 24) == 0);

    if ((ins == INS_rep_stos) || (ins == INS_rep_movs))
    {
        dst += Write8(dst, code);
        code >>= 8;
        ins = ins == INS_rep_stos ? INS_stos : INS_movs;
    }

    if (ins == INS_cdq)
    {
        RemoveGCLiveReg(REG_EDX, dst);
    }
    else if ((id->idOpSize() == EA_1BYTE) && ((ins == INS_stos) || (ins == INS_movs)))
    {
        assert(HasWBit(ins) && ((code & 1) != 0));
        code ^= 1;
    }

#ifdef TARGET_AMD64
    if (id->idOpSize() == EA_8BYTE)
    {
        assert((ins == INS_cdq) || (ins == INS_stos) || (ins == INS_movs));

        code = AddRexWPrefix(code);
        dst += EncodeRexPrefix(dst, code);
        INDEBUG(code &= ~(0xFFull << RexBitOffset));
    }
#endif

    if ((code >> MmmBitOffset) == 1)
    {
        dst += Write8(dst, 0x0F);
        dst += Write16(dst, code);
    }
    else if ((code >> 16) == 0xC5)
    {
        dst += Write8(dst, 0xC5);
        dst += Write16(dst, code);
    }
    else
    {
        assert((code & 0xFFFFFF00) == 0);
        dst += Write8(dst, code);
    }

    return dst;
}

void EmitterBase::Encode(GCInfo& gcInfo)
{
    X86Emitter& emit = *static_cast<X86Emitter*>(this);
    X86Encoder  encoder(emit, gcInfo);
    encoder.Encode(emit);
}

size_t Encoder::ArchEncodeInstr(insGroup* ig, instrDesc* id, uint8_t** dp)
{
    return static_cast<X86Encoder*>(this)->EncodeInstr(ig, id, dp);
}

size_t X86Encoder::EncodeInstr(insGroup* ig, instrDesc* id, uint8_t** dp)
{
    uint8_t*    dst  = *dp;
    size_t      sz   = sizeof(instrDesc);
    instruction ins  = id->idIns();
    emitAttr    size = id->idOpSize();

    assert((ins != INS_imul) || (size >= EA_4BYTE)); // Has no 'w' bit
    assert(((dst >= hotCodeBlock) && (dst < hotCodeBlock + hotCodeSize)) ||
           ((dst >= coldCodeBlock) && (dst < coldCodeBlock + GetColdCodeSize())));

    switch (id->idInsFmt())
    {
        code_t  code;
        ssize_t cnsVal;

        /********************************************************************/
        /*                        No operands                               */
        /********************************************************************/
        case IF_NONE:
            if (ins == INS_align)
            {
                dst = EncodeAlign(ig, id, dst);
                sz  = sizeof(instrDescAlign);
                break;
            }

            dst = EncodeNoOperands(dst, id);
            break;

        /********************************************************************/
        /*                Simple constant, local label, method              */
        /********************************************************************/

        case IF_CNS:
            dst = EncodeI(dst, id);
            sz  = id->GetDescSize();
            break;

        case IF_RWR_LABEL:
            dst = EncodeRoDataAddr(dst, static_cast<instrDescJmp*>(id), ig);
            sz  = sizeof(instrDescJmp);
            break;

        case IF_LABEL:
#ifdef TARGET_X86
            if (id->idIns() == INS_push_hide)
            {
                dst = EncodePushLabel(dst, static_cast<instrDescJmp*>(id), ig);
            }
            else
#endif
            {
                dst = EncodeJump(dst, static_cast<instrDescJmp*>(id), ig);
            }
            sz = sizeof(instrDescJmp);
            break;

        case IF_METHOD:
        case IF_METHPTR:
            dst = EncodeCall(dst, id);
            sz  = RecordGCCall(id, *dp, dst);
            break;

        /********************************************************************/
        /*                      One register operand                        */
        /********************************************************************/

        case IF_RRD:
        case IF_RWR:
        case IF_RRW:
            dst = EncodeR(dst, id);

            if (ins == INS_call)
            {
                sz = RecordGCCall(id, *dp, dst);
            }
            else
            {
                sz = sizeof(instrDescSmall);
            }
            break;

        /********************************************************************/
        /*                 Register and register/constant                   */
        /********************************************************************/

        case IF_RRD_RRD:
        case IF_RWR_RRD:
        case IF_RRW_RRD:
        case IF_RRW_RRW:
            dst = EncodeRR(dst, id);
            sz  = sizeof(instrDescSmall);
            break;

        case IF_RRD_CNS:
        case IF_RWR_CNS:
        case IF_RRW_CNS:
            dst = EncodeRI(dst, id);
            sz  = id->GetDescSize();
            break;

        case IF_RWR_RRD_RRD:
            dst = EncodeRRR(dst, id);
            sz  = id->GetDescSize();
            break;
        case IF_RWR_RRD_RRD_CNS:
        case IF_RWR_RRD_RRD_RRD:
            dst = EncodeRRR(dst, id);
            dst += Write8(dst, id->GetImm());
            sz = id->GetDescSize();
            break;

        case IF_RRW_RRD_CNS:
            dst = EncodeRRI(dst, id);
            sz  = id->GetDescSize();
            break;

        /********************************************************************/
        /*                      Address mode operand                        */
        /********************************************************************/

        case IF_ARD:
        case IF_AWR:
        case IF_ARW:
            assert(!TakesVexPrefix(ins));

            dst = EncodeAM(dst, id, GetCodeMR(ins));

            if (ins == INS_call)
            {
                sz = RecordGCCall(id, *dp, dst);
            }
            else
            {
                sz = id->GetDescSize();
            }
            break;

        case IF_ARD_CNS:
        case IF_AWR_CNS:
        case IF_ARW_CNS:
            assert(!TakesVexPrefix(ins));

            cnsVal = id->GetImm();
            dst    = EncodeAM(dst, id, GetCodeMI(ins), &cnsVal);
            sz     = id->GetDescSize();
            break;

        case IF_ARD_RRD:
        case IF_AWR_RRD:
        case IF_ARW_RRD:
            assert(!IsReallyVexTernary(ins));

            code = GetCodeMR(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);
            code = SetRMReg(ins, id->idReg1(), size, code);
            dst  = EncodeAM(dst, id, code);
            sz   = id->GetDescSize();
            break;

        case IF_RRD_ARD:
        case IF_RWR_ARD:
        case IF_RRW_ARD:
            code = GetCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(id->idReg1(), size, code);
            }

            if (!IsBMIRegExtInstruction(ins))
            {
                code = SetRMReg(ins, id->idReg1(), size, code);
            }

            dst = EncodeAM(dst, id, code);
            sz  = id->GetDescSize();
            break;

        case IF_RRW_ARD_CNS:
        case IF_RWR_ARD_CNS:
            assert(IsSSEOrAVXOrBMIInstruction(ins) || (ins == INS_imuli));

            code = GetCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(id->idReg1(), size, code);
            }

            code   = SetRMReg(ins, id->idReg1(), size, code);
            cnsVal = id->GetImm();
            dst    = EncodeAM(dst, id, code, &cnsVal);
            sz     = id->GetDescSize();
            break;

        case IF_AWR_RRD_CNS:
            assert(ins == INS_vextracti128 || ins == INS_vextractf128);
            assert(UseVEXEncoding());
            assert(!IsVexTernary(ins));

            code   = GetCodeMR(ins);
            code   = AddVexPrefix(ins, code, size);
            code   = SetRMReg(ins, id->idReg1(), size, code);
            cnsVal = id->GetImm();
            dst    = EncodeAM(dst, id, code, &cnsVal);
            sz     = id->GetDescSize();
            break;

        case IF_AWR_RRD_RRD:
            assert(IsVexDstDstSrc(ins));

            code = GetCodeMR(ins);
            code = AddVexPrefix(ins, code, size);
            code = SetVexVvvv(id->idReg1(), size, code);
            code = SetRMReg(ins, id->idReg2(), size, code);
            dst  = EncodeAM(dst, id, code);
            sz   = id->GetDescSize();
            break;

        case IF_RWR_ARD_RRD:
        case IF_RWR_RRD_ARD:
            assert(IsVexTernary(ins));

            code = GetCodeRM(ins);
            code = AddVexPrefix(ins, code, size);
            code = SetVexVvvv(id->idReg2(), size, code);
            code = SetRMReg(ins, id->idReg1(), size, code);
            dst  = EncodeAM(dst, id, code);
            sz   = id->GetDescSize();
            break;

        case IF_RWR_RRD_ARD_CNS:
        case IF_RWR_RRD_ARD_RRD:
            assert(IsVexTernary(ins));

            code   = GetCodeRM(ins);
            code   = AddVexPrefix(ins, code, size);
            code   = SetVexVvvv(id->idReg2(), size, code);
            code   = SetRMReg(ins, id->idReg1(), size, code);
            cnsVal = id->GetImm();
            dst    = EncodeAM(dst, id, code, &cnsVal);
            sz     = id->GetDescSize();
            break;

        /********************************************************************/
        /*                      Stack-based operand                         */
        /********************************************************************/

        case IF_SRD:
        case IF_SWR:
        case IF_SRW:
            assert(!TakesVexPrefix(ins));
            assert(ins != INS_pop_hide);

            dst = EncodeSV(dst, id, GetCodeMR(ins));
            if (ins == INS_call)
            {
                sz = RecordGCCall(id, *dp, dst);
            }
            break;

        case IF_SRD_CNS:
        case IF_SWR_CNS:
        case IF_SRW_CNS:
            assert(!TakesVexPrefix(ins));

            cnsVal = id->GetImm();
            dst    = EncodeSV(dst, id, GetCodeMI(ins), &cnsVal);
            sz     = id->GetDescSize();
            break;

        case IF_SRD_RRD:
        case IF_SWR_RRD:
        case IF_SRW_RRD:
            assert(!IsReallyVexTernary(ins));

            code = GetCodeMR(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);
            code = SetRMReg(ins, id->idReg1(), size, code);
            dst  = EncodeSV(dst, id, code);
            sz   = id->GetDescSize();
            break;

        case IF_RRD_SRD:
        case IF_RWR_SRD:
        case IF_RRW_SRD:
            code = GetCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(id->idReg1(), size, code);
            }

            if (!IsBMIRegExtInstruction(ins))
            {
                code = SetRMReg(ins, id->idReg1(), size, code);
            }

            dst = EncodeSV(dst, id, code);
            sz  = id->GetDescSize();
            break;

        case IF_RRW_SRD_CNS:
        case IF_RWR_SRD_CNS:
            assert(IsSSEOrAVXOrBMIInstruction(ins) || (ins == INS_imuli));

            code = GetCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(id->idReg1(), size, code);
            }

            code   = SetRMReg(ins, id->idReg1(), size, code);
            cnsVal = id->GetImm();
            dst    = EncodeSV(dst, id, code, &cnsVal);
            sz     = id->GetDescSize();
            break;

        case IF_SWR_RRD_CNS:
            assert(ins == INS_vextracti128 || ins == INS_vextractf128);
            assert(UseVEXEncoding());
            assert(!IsVexTernary(ins));

            code   = GetCodeMR(ins);
            code   = AddVexPrefix(ins, code, size);
            code   = SetRMReg(ins, id->idReg1(), size, code);
            cnsVal = id->GetImm();
            dst    = EncodeSV(dst, id, code, &cnsVal);
            sz     = id->GetDescSize();
            break;

        // case IF_SWR_RRD_RRD:
        // This format is used by vmaskmovps & co. and currently we can't
        // generate such instructions, that store to a local variable.
        // But there's probably nothing fundamentally impossible about this,
        // it's just that currently all stores to locals are using
        // STORE_LCL_VAR.
        //
        //
        //
        //

        // case IF_RWR_SRD_RRD: - This format is used only by gather instructions.
        case IF_RWR_RRD_SRD:
            assert(IsVexTernary(ins));

            code = GetCodeRM(ins);
            code = AddVexPrefix(ins, code, size);
            code = SetVexVvvv(id->idReg2(), size, code);
            code = SetRMReg(ins, id->idReg1(), size, code);
            dst  = EncodeSV(dst, id, code);
            sz   = id->GetDescSize();
            break;

        case IF_RWR_RRD_SRD_CNS:
        case IF_RWR_RRD_SRD_RRD:
            assert(IsVexTernary(ins));

            code   = GetCodeRM(ins);
            code   = AddVexPrefix(ins, code, size);
            code   = SetVexVvvv(id->idReg2(), size, code);
            code   = SetRMReg(ins, id->idReg1(), size, code);
            cnsVal = id->GetImm();
            dst    = EncodeSV(dst, id, code, &cnsVal);
            sz     = id->GetDescSize();
            break;

        /********************************************************************/
        /*                    Direct memory address                         */
        /********************************************************************/

        case IF_MRD:
        case IF_MRW:
        case IF_MWR:
            assert(!TakesVexPrefix(ins));
            noway_assert(ins != INS_call);

            dst = EncodeCV(dst, id, GetCodeMR(ins));
            sz  = id->GetDescSize();
            break;

        case IF_MRD_CNS:
        case IF_MWR_CNS:
        case IF_MRW_CNS:
            assert(!TakesVexPrefix(ins));

            cnsVal = id->GetImm();
            dst    = EncodeCV(dst, id, GetCodeMI(ins), &cnsVal);
            sz     = id->GetDescSize();
            break;

        case IF_MRD_RRD:
        case IF_MWR_RRD:
        case IF_MRW_RRD:
            assert(!IsReallyVexTernary(ins));

            code = GetCodeMR(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);
            code = SetRMReg(ins, id->idReg1(), size, code);
            dst  = EncodeCV(dst, id, code);
            sz   = id->GetDescSize();
            break;

        case IF_RRD_MRD:
        case IF_RWR_MRD:
        case IF_RRW_MRD:
            code = GetCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(id->idReg1(), size, code);
            }

            if (!IsBMIRegExtInstruction(ins))
            {
                code = SetRMReg(ins, id->idReg1(), size, code);
            }

            dst = EncodeCV(dst, id, code);
            sz  = id->GetDescSize();
            break;

        case IF_RRW_MRD_CNS:
        case IF_RWR_MRD_CNS:
            assert(IsSSEOrAVXOrBMIInstruction(ins) || (ins == INS_imuli));

            code = GetCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(id->idReg1(), size, code);
            }

            code   = SetRMReg(ins, id->idReg1(), size, code);
            cnsVal = id->GetImm();
            dst    = EncodeCV(dst, id, code, &cnsVal);
            sz     = id->GetDescSize();
            break;

        case IF_MWR_RRD_CNS:
            assert(ins == INS_vextracti128 || ins == INS_vextractf128);
            assert(UseVEXEncoding());
            assert(!IsVexTernary(ins));

            code   = GetCodeMR(ins);
            code   = AddVexPrefix(ins, code, size);
            code   = SetRMReg(ins, id->idReg1(), size, code);
            cnsVal = id->GetImm();
            dst    = EncodeCV(dst, id, code, &cnsVal);
            sz     = id->GetDescSize();
            break;

        // case IF_MWR_RRD_RRD:
        // This format is used by vmaskmovps & co. and currently we can't
        // generate such instructions, that store to a static field.
        // But there's probably nothing fundamentally impossible about this,
        // it's just it likely needs .NET 8's struct statics to be of any
        // use.
        //
        //
        //
        //

        // case IF_RWR_MRD_RRD: - This format is used only by gather instructions.
        case IF_RWR_RRD_MRD:
            assert(IsVexTernary(ins));

            code = GetCodeRM(ins);
            code = AddVexPrefix(ins, code, size);
            code = SetVexVvvv(id->idReg2(), size, code);
            code = SetRMReg(ins, id->idReg1(), size, code);
            dst  = EncodeCV(dst, id, code);
            sz   = id->GetDescSize();
            break;

        case IF_RWR_RRD_MRD_CNS:
        case IF_RWR_RRD_MRD_RRD:
            assert(IsVexTernary(ins));

            code   = GetCodeRM(ins);
            code   = AddVexPrefix(ins, code, size);
            code   = SetVexVvvv(id->idReg2(), size, code);
            code   = SetRMReg(ins, id->idReg1(), size, code);
            cnsVal = id->GetImm();
            dst    = EncodeCV(dst, id, code, &cnsVal);
            sz     = id->GetDescSize();
            break;

        default:
            unreached();
    }

#if !FEATURE_FIXED_OUT_ARGS
    if (!ig->IsPrologOrEpilog())
    {
        switch (ins)
        {
            case INS_push_hide:
            case INS_pop_hide:
                break;
            case INS_push:
                StackPush(GetCodeOffset(dst), id->idGCref());
                break;
            case INS_pop:
                StackPop(GetCodeOffset(dst), 1);
                break;
            case INS_add:
            case INS_sub:
                if ((id->idInsFmt() == IF_RRW_CNS) && (id->idReg1() == REG_ESP))
                {
                    size_t imm = static_cast<size_t>(id->GetImm());
                    assert(imm < UINT_MAX);
                    unsigned count    = static_cast<unsigned>(imm) / TARGET_POINTER_SIZE;
                    unsigned codeOffs = GetCodeOffset(dst);

                    if (ins == INS_add)
                    {
                        StackPop(codeOffs, count);
                    }
                    else
                    {
                        StackPushN(codeOffs, count);
                    }
                }
                break;

            default:
                break;
        }
    }

    assert(stackLevel <= INT32_MAX);
#endif // !FEATURE_FIXED_OUT_ARGS

    if (ins == INS_mulEAX || ins == INS_imulEAX)
    {
        assert((gcInfo.GetAllLiveRegs() & (RBM_EAX | RBM_EDX)) == RBM_NONE);
    }
    else if (ins == INS_imuli)
    {
        assert((gcInfo.GetAllLiveRegs() & genRegMask(id->idReg1())) == RBM_NONE);
    }

#ifdef DEBUG
    if ((compiler->opts.disAsm || compiler->verbose) && (*dp != dst))
    {
        PrintInstr(id, *dp, dst - *dp);
    }
#endif

    *dp = dst;

    return sz;
}

#if defined(DEBUG) || defined(LATE_DISASM)

insFormat Encoder::GetPerfScoreMemoryOperation(instrDesc* id)
{
    if (id->idIns() == INS_lea)
    {
        // lea instructions do not actually read memory
        return IF_NONE;
    }

    static const insFormat memOp[]{
#define IF_DEF(name, s, op, mem) IF_##mem,
#include "emitfmtsxarch.h"
    };

    return memOp[id->idInsFmt()];
}

// The instruction latencies and throughput values returned by this function
// are for the Intel Skylake-X processor and are from either:
//   1. Agner.org - https://www.agner.org/optimize/instruction_tables.pdf
//   2. uops.info - https://uops.info/table.html
//
Encoder::InstrPerfScore Encoder::GetInstrPerfScore(instrDesc* id)
{
    instruction ins    = id->idIns();
    insFormat   insFmt = id->idInsFmt();
    insFormat   memFmt = GetPerfScoreMemoryOperation(id);

    InstrPerfScore result;
    result.throughput = PERFSCORE_THROUGHPUT_ILLEGAL;
    result.latency    = PERFSCORE_LATENCY_ILLEGAL;

    PerfScoreMemoryKind memAccessKind;

    // Model the memory latency
    switch (memFmt)
    {
        // Model a read from stack location, possible def to use latency from L0 cache
        case IF_SRD:
            result.latency = PERFSCORE_LATENCY_RD_STACK;
            memAccessKind  = PERFSCORE_MEMORY_READ;
            break;

        case IF_SWR:
            result.latency = PERFSCORE_LATENCY_WR_STACK;
            memAccessKind  = PERFSCORE_MEMORY_WRITE;
            break;

        case IF_SRW:
            result.latency = PERFSCORE_LATENCY_RD_WR_STACK;
            memAccessKind  = PERFSCORE_MEMORY_READ_WRITE;
            break;

        // Model a read from a constant location, possible def to use latency from L0 cache
        case IF_MRD:
            result.latency = PERFSCORE_LATENCY_RD_CONST_ADDR;
            memAccessKind  = PERFSCORE_MEMORY_READ;
            break;

        case IF_MWR:
            result.latency = PERFSCORE_LATENCY_WR_CONST_ADDR;
            memAccessKind  = PERFSCORE_MEMORY_WRITE;
            break;

        case IF_MRW:
            result.latency = PERFSCORE_LATENCY_RD_WR_CONST_ADDR;
            memAccessKind  = PERFSCORE_MEMORY_READ_WRITE;
            break;

        // Model a read from memory location, possible def to use latency from L0 or L1 cache
        case IF_ARD:
            result.latency = PERFSCORE_LATENCY_RD_GENERAL;
            memAccessKind  = PERFSCORE_MEMORY_READ;
            break;

        case IF_AWR:
            result.latency = PERFSCORE_LATENCY_WR_GENERAL;
            memAccessKind  = PERFSCORE_MEMORY_WRITE;
            break;

        case IF_ARW:
            result.latency = PERFSCORE_LATENCY_RD_WR_GENERAL;
            memAccessKind  = PERFSCORE_MEMORY_READ_WRITE;
            break;

        case IF_NONE:
            result.latency = PERFSCORE_LATENCY_ZERO;
            memAccessKind  = PERFSCORE_MEMORY_NONE;
            break;

        default:
            assert(!"Unhandled insFmt for switch (memFmt)");
            result.latency = PERFSCORE_LATENCY_ZERO;
            memAccessKind  = PERFSCORE_MEMORY_NONE;
            break;
    }
    result.memoryAccessKind = memAccessKind;

    switch (ins)
    {
        case INS_align:
            if (id->idCodeSize() == 0)
            {
                // We're not going to generate any instruction, so it doesn't count for PerfScore.
                result.throughput = PERFSCORE_THROUGHPUT_ZERO;
                result.latency    = PERFSCORE_LATENCY_ZERO;
                break;
            }
            FALLTHROUGH;
        case INS_nop:
        case INS_int3:
            assert(memFmt == IF_NONE);
            result.throughput = PERFSCORE_THROUGHPUT_4X;
            result.latency    = PERFSCORE_LATENCY_ZERO;
            break;

        case INS_push:
        case INS_push_hide:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            if (insFmt == IF_RRD) // push  reg
            {
                // For pushes (stack writes) we assume that the full latency will be covered
                result.latency = PERFSCORE_LATENCY_ZERO;
            }
            break;

        case INS_pop:
        case INS_pop_hide:
            if (insFmt == IF_RWR) // pop   reg
            {
                result.throughput = PERFSCORE_THROUGHPUT_2X;
                // For pops (stack reads) we assume that the full latency will be covered
                result.latency = PERFSCORE_LATENCY_ZERO;
            }
            else
            {
                result.throughput = PERFSCORE_THROUGHPUT_1C;
            }
            break;

        case INS_inc:
        case INS_dec:
        case INS_neg:
        case INS_not:
            if (memFmt == IF_NONE)
            {
                // ins   reg
                result.throughput = PERFSCORE_THROUGHPUT_4X;
                result.latency    = PERFSCORE_LATENCY_1C;
            }
            else
            {
                // ins   mem
                result.throughput = PERFSCORE_THROUGHPUT_1C;
            }
            break;

#ifdef TARGET_AMD64
        case INS_movsxd:
#endif
        case INS_mov:
        case INS_movsx:
        case INS_movzx:
        case INS_cmp:
        case INS_test:
            if (memFmt == IF_NONE)
            {
                result.throughput = PERFSCORE_THROUGHPUT_4X;
            }
            else if (memAccessKind == PERFSCORE_MEMORY_READ)
            {
                result.throughput = PERFSCORE_THROUGHPUT_2X;
            }
            else // writes
            {
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
                result.throughput = PERFSCORE_THROUGHPUT_1C;
            }
            break;

        case INS_adc:
        case INS_sbb:
        case INS_add:
        case INS_sub:
        case INS_and:
        case INS_or:
        case INS_xor:
            result.latency = max(PERFSCORE_LATENCY_1C, result.latency);
            if (memFmt == IF_NONE)
            {
                result.throughput = PERFSCORE_THROUGHPUT_4X;
            }
            else if (memAccessKind == PERFSCORE_MEMORY_READ_WRITE)
            {
                result.throughput = PERFSCORE_THROUGHPUT_1C;
            }
            else
            {
                result.throughput = PERFSCORE_THROUGHPUT_2X;
            }
            break;

        case INS_lea:
            // uops.info
            result.throughput = PERFSCORE_THROUGHPUT_2X; // one or two components
            result.latency    = PERFSCORE_LATENCY_1C;

            if (insFmt == IF_RWR_LABEL)
            {
                // RIP relative addressing
                //
                // - throughput is only 1 per cycle
                //
                result.throughput = PERFSCORE_THROUGHPUT_1C;
            }
            else if (insFmt == IF_RWR_ARD)
            {
                if (id->idAddr()->iiaAddrMode.index != REG_NA)
                {
                    RegNum baseReg = id->idAddr()->iiaAddrMode.base;
                    if (baseReg != REG_NA)
                    {
                        ssize_t dsp = id->GetAmDisp();

                        if ((dsp != 0) || BaseRegRequiresDisp(baseReg))
                        {
                            // three components
                            //
                            // - throughput is only 1 per cycle
                            //
                            result.throughput = PERFSCORE_THROUGHPUT_1C;

                            if (BaseRegRequiresDisp(baseReg) || id->idIsDspReloc())
                            {
                                // Increased Latency for these cases
                                //  - see https://reviews.llvm.org/D32277
                                //
                                result.latency = PERFSCORE_LATENCY_3C;
                            }
                        }
                    }
                }
            }

            break;

        case INS_imul:
        case INS_imuli:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency += PERFSCORE_LATENCY_3C;
            break;

        case INS_mulEAX:
        case INS_imulEAX:
            // uops.info: mul/imul rdx:rax,reg latency is 3 only if the low half of the result is needed, but in that
            // case codegen uses imul reg,reg instruction form (except for unsigned overflow checks, which are rare)
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency += PERFSCORE_LATENCY_4C;
            break;

        case INS_div:
            // The integer divide instructions have long latencies
            if ((id->idOpSize() == EA_8BYTE))
            {
                result.throughput = PERFSCORE_THROUGHPUT_52C;
                result.latency    = PERFSCORE_LATENCY_62C;
            }
            else
            {
                assert(id->idOpSize() == EA_4BYTE);
                result.throughput = PERFSCORE_THROUGHPUT_6C;
                result.latency    = PERFSCORE_LATENCY_26C;
            }
            break;

        case INS_idiv:
            // The integer divide instructions have long latenies
            if ((id->idOpSize() == EA_8BYTE))
            {
                result.throughput = PERFSCORE_THROUGHPUT_57C;
                result.latency    = PERFSCORE_LATENCY_69C;
            }
            else
            {
                assert(id->idOpSize() == EA_4BYTE);
                result.throughput = PERFSCORE_THROUGHPUT_6C;
                result.latency    = PERFSCORE_LATENCY_26C;
            }
            break;

        case INS_cdq:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency    = PERFSCORE_LATENCY_1C;
            break;

        case INS_shl:
        case INS_shr:
        case INS_sar:
        case INS_ror:
        case INS_rol:
            switch (insFmt)
            {
                case IF_RRW_CNS:
                    // ins   reg, cns
                    result.throughput = PERFSCORE_THROUGHPUT_2X;
                    result.latency    = PERFSCORE_LATENCY_1C;
                    break;

                case IF_MRW_CNS:
                case IF_SRW_CNS:
                case IF_ARW_CNS:
                    // ins   [mem], cns
                    result.throughput = PERFSCORE_THROUGHPUT_2C;
                    result.latency += PERFSCORE_LATENCY_1C;
                    break;

                case IF_RRW:
                    // ins   reg, cl
                    result.throughput = PERFSCORE_THROUGHPUT_2C;
                    result.latency    = PERFSCORE_LATENCY_2C;
                    break;

                case IF_MRW:
                case IF_SRW:
                case IF_ARW:
                    // ins   [mem], cl
                    result.throughput = PERFSCORE_THROUGHPUT_4C;
                    result.latency += PERFSCORE_LATENCY_2C;
                    break;

                default:
                    // unhandled instruction insFmt combination
                    PerfScoreUnhandledInstr(id, &result);
                    break;
            }
            break;

        case INS_shl_1:
        case INS_shr_1:
        case INS_sar_1:
            result.latency += PERFSCORE_LATENCY_1C;
            switch (insFmt)
            {
                case IF_RRW:
                    // ins   reg, 1
                    result.throughput = PERFSCORE_THROUGHPUT_2X;
                    break;

                case IF_MRW:
                case IF_SRW:
                case IF_ARW:
                    // ins   [mem], 1
                    result.throughput = PERFSCORE_THROUGHPUT_2C;
                    break;

                default:
                    // unhandled instruction insFmt combination
                    PerfScoreUnhandledInstr(id, &result);
                    break;
            }
            break;

        case INS_ror_1:
        case INS_rol_1:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency += PERFSCORE_LATENCY_1C;
            break;

        case INS_shl_N:
        case INS_shr_N:
        case INS_sar_N:
        case INS_ror_N:
        case INS_rol_N:
            result.latency += PERFSCORE_LATENCY_1C;
            switch (insFmt)
            {
                case IF_RRW_CNS:
                    // ins   reg, cns
                    result.throughput = PERFSCORE_THROUGHPUT_2X;
                    break;

                case IF_MRW_CNS:
                case IF_SRW_CNS:
                case IF_ARW_CNS:
                    // ins   [mem], cns
                    result.throughput = PERFSCORE_THROUGHPUT_2C;
                    break;

                default:
                    // unhandled instruction insFmt combination
                    PerfScoreUnhandledInstr(id, &result);
                    break;
            }
            break;

        case INS_rcr:
        case INS_rcl:
            result.throughput = PERFSCORE_THROUGHPUT_6C;
            result.latency += PERFSCORE_LATENCY_6C;
            break;

        case INS_rcr_1:
        case INS_rcl_1:
            // uops.info
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency += PERFSCORE_LATENCY_2C;
            break;

        case INS_shld:
        case INS_shrd:
            if (insFmt == IF_RRW_RRD_CNS)
            {
                // ins   reg, reg, cns
                result.throughput = PERFSCORE_THROUGHPUT_1C;
                result.latency    = PERFSCORE_LATENCY_3C;
            }
            else
            {
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE); // _SHF form never emitted
                result.throughput = PERFSCORE_THROUGHPUT_2C;
            }
            break;

        case INS_bt:
            if ((insFmt == IF_RRD_RRD) || (insFmt == IF_RRD_CNS))
            {
                result.throughput = PERFSCORE_THROUGHPUT_2X;
                result.latency    = PERFSCORE_LATENCY_1C;
            }
            else
            {
                result.throughput = PERFSCORE_THROUGHPUT_1C;
            }
            break;

        case INS_seto:
        case INS_setno:
        case INS_setb:
        case INS_setae:
        case INS_sete:
        case INS_setne:
        case INS_setbe:
        case INS_seta:
        case INS_sets:
        case INS_setns:
        case INS_setp:
        case INS_setnp:
        case INS_setl:
        case INS_setge:
        case INS_setle:
        case INS_setg:
            result.latency = PERFSCORE_LATENCY_1C;
            if (insFmt == IF_RRD)
            {
                result.throughput = PERFSCORE_THROUGHPUT_2X;
            }
            else
            {
                result.throughput = PERFSCORE_THROUGHPUT_1C;
            }
            break;

        case INS_jo:
        case INS_jno:
        case INS_jb:
        case INS_jae:
        case INS_je:
        case INS_jne:
        case INS_jbe:
        case INS_ja:
        case INS_js:
        case INS_jns:
        case INS_jp:
        case INS_jnp:
        case INS_jl:
        case INS_jge:
        case INS_jle:
        case INS_jg:
            // conditional branch
            result.throughput = PERFSCORE_THROUGHPUT_2X;
            result.latency    = PERFSCORE_LATENCY_BRANCH_COND;
            break;

        case INS_jmp:
        case INS_l_jmp:
            // branch to a constant address
            result.throughput = PERFSCORE_THROUGHPUT_2C;
            result.latency    = PERFSCORE_LATENCY_BRANCH_DIRECT;
            break;

#ifdef TARGET_AMD64
        case INS_rex_jmp:
#endif // TARGET_AMD64
        case INS_i_jmp:
            // branch to register
            result.throughput = PERFSCORE_THROUGHPUT_2C;
            result.latency    = PERFSCORE_LATENCY_BRANCH_INDIRECT;
            break;

        case INS_call:
            // uops.info
            result.latency = PERFSCORE_LATENCY_ZERO;
            switch (insFmt)
            {
                case IF_LABEL:
                    result.throughput = PERFSCORE_THROUGHPUT_1C;
                    break;

                case IF_METHOD:
                    result.throughput = PERFSCORE_THROUGHPUT_1C;
                    break;

                case IF_METHPTR:
                    result.throughput = PERFSCORE_THROUGHPUT_3C;
                    break;

                case IF_SRD:
                    result.throughput = PERFSCORE_THROUGHPUT_3C;
                    break;

                case IF_ARD:
                case IF_RRD:
                    result.throughput = PERFSCORE_THROUGHPUT_3C;
                    break;

                default:
                    // unhandled instruction, insFmt combination
                    PerfScoreUnhandledInstr(id, &result);
                    break;
            }
            break;

        case INS_ret:
            if (insFmt == IF_CNS)
            {
                result.throughput = PERFSCORE_THROUGHPUT_2C;
            }
            else
            {
                assert(insFmt == IF_NONE);
                result.throughput = PERFSCORE_THROUGHPUT_1C;
            }
            break;

        case INS_lock:
            result.throughput = PERFSCORE_THROUGHPUT_13C;
            break;

        case INS_xadd:
            // uops.info
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            break;

        case INS_cmpxchg:
            result.throughput = PERFSCORE_THROUGHPUT_5C;
            break;

        case INS_xchg:
            // uops.info
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            if (memFmt == IF_NONE)
            {
                result.latency = PERFSCORE_LATENCY_1C;
            }
            else
            {
                result.latency = PERFSCORE_LATENCY_23C;
            }
            break;

#ifdef TARGET_X86
        case INS_fld:
        case INS_fstp:
            result.throughput = PERFSCORE_THROUGHPUT_2X;
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.latency = PERFSCORE_LATENCY_1C;
            }
            break;
#endif // TARGET_X86

        case INS_movs:
        case INS_stos:
            // uops.info
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            break;

        case INS_rep_movs:
        case INS_rep_stos:
            // Actually variable sized: rep stosd, used to zero frame slots
            // uops.info
            result.throughput = PERFSCORE_THROUGHPUT_25C;
            break;

        case INS_movd:
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                // movd   r32, xmm   or  xmm, r32
                result.throughput = PERFSCORE_THROUGHPUT_1C;
                result.latency    = PERFSCORE_LATENCY_3C;
            }
            else if (memAccessKind == PERFSCORE_MEMORY_READ)
            {
                // movd   xmm, m32
                result.throughput = PERFSCORE_THROUGHPUT_2X;
                // latency is set above (see -  Model the memory latency)
            }
            else
            {
                // movd   m32, xmm
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
                result.throughput = PERFSCORE_THROUGHPUT_1C;
                // latency is set above (see -  Model the memory latency)
            }
            break;

        case INS_movq:
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                // movq   reg, reg
                result.throughput = PERFSCORE_THROUGHPUT_3X;
                result.latency    = PERFSCORE_LATENCY_1C;
            }
            else if (memAccessKind == PERFSCORE_MEMORY_READ)
            {
                // movq   reg, mem
                result.throughput = PERFSCORE_THROUGHPUT_2X;
                // latency is set above (see -  Model the memory latency)
            }
            else
            {
                // movq   mem, reg
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
                result.throughput = PERFSCORE_THROUGHPUT_1C;
                // latency is set above (see -  Model the memory latency)
            }
            break;

        case INS_movdqa:
        case INS_movdqu:
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                // ins   reg, reg
                result.throughput = PERFSCORE_THROUGHPUT_4X;
                result.latency    = PERFSCORE_LATENCY_ZERO;
            }
            else if (memAccessKind == PERFSCORE_MEMORY_READ)
            {
                // ins   reg, mem
                result.throughput = PERFSCORE_THROUGHPUT_2X;
                // latency is set above (see -  Model the memory latency)
            }
            else
            {
                // ins   mem, reg
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
                result.throughput = PERFSCORE_THROUGHPUT_1C;
                // latency is set above (see -  Model the memory latency)
            }
            break;

        case INS_movhps:
        case INS_movhpd:
        case INS_movlps:
        case INS_movlpd:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            if (memAccessKind == PERFSCORE_MEMORY_READ)
            {
                result.latency = max(PERFSCORE_LATENCY_4C, result.latency);
            }
            else
            {
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
                result.latency = max(PERFSCORE_LATENCY_3C, result.latency);
            }
            break;

        case INS_movhlps:
        case INS_movlhps:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency    = PERFSCORE_LATENCY_1C;
            break;

        case INS_movntdq:
        case INS_movnti:
        case INS_movntps:
        case INS_movntpd:
            assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency    = PERFSCORE_LATENCY_400C; // Intel microcode issue with these instuctions
            break;

        case INS_maskmovdqu:
            result.throughput = PERFSCORE_THROUGHPUT_6C;
            result.latency    = PERFSCORE_LATENCY_400C; // Intel microcode issue with these instuctions
            break;

        case INS_movntdqa:
            assert(memAccessKind == PERFSCORE_MEMORY_READ);
            result.throughput = PERFSCORE_THROUGHPUT_2X;
            result.latency    = PERFSCORE_LATENCY_3C;
            break;

        case INS_vzeroupper:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            // latency is zero and is set when we Model the memory latency
            break;

        case INS_movss:
        case INS_movsd:
        case INS_movddup:
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.throughput = PERFSCORE_THROUGHPUT_1C;
                result.latency    = PERFSCORE_LATENCY_1C;
            }
            else
            {
                result.throughput = PERFSCORE_THROUGHPUT_2X;
                result.latency    = max(PERFSCORE_LATENCY_3C, result.latency);
            }
            break;

        case INS_lddqu:
            result.throughput = PERFSCORE_THROUGHPUT_2X;
            result.latency    = max(PERFSCORE_LATENCY_3C, result.latency);
            break;

        case INS_comiss:
        case INS_comisd:
        case INS_ucomiss:
        case INS_ucomisd:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency    = max(PERFSCORE_LATENCY_2C, result.latency);
            break;

        case INS_addsd:
        case INS_addss:
        case INS_addpd:
        case INS_addps:
        case INS_subsd:
        case INS_subss:
        case INS_subpd:
        case INS_subps:
        case INS_cvttps2dq:
        case INS_cvtps2dq:
        case INS_cvtdq2ps:
            result.throughput = PERFSCORE_THROUGHPUT_2X;
            result.latency += PERFSCORE_LATENCY_4C;
            break;

        case INS_haddps:
        case INS_haddpd:
            result.throughput = PERFSCORE_THROUGHPUT_2C;
            result.latency += PERFSCORE_LATENCY_6C;
            break;

        case INS_mulss:
        case INS_mulsd:
        case INS_mulps:
        case INS_mulpd:
            result.throughput = PERFSCORE_THROUGHPUT_2X;
            result.latency += PERFSCORE_LATENCY_4C;
            break;

        case INS_divss:
        case INS_divps:
            result.throughput = PERFSCORE_THROUGHPUT_3C;
            result.latency += PERFSCORE_LATENCY_11C;
            break;

        case INS_divsd:
        case INS_divpd:
            result.throughput = PERFSCORE_THROUGHPUT_4C;
            result.latency += PERFSCORE_LATENCY_13C;
            break;

        case INS_sqrtss:
        case INS_sqrtps:
            result.throughput = PERFSCORE_THROUGHPUT_3C;
            result.latency += PERFSCORE_LATENCY_12C;
            break;

        case INS_sqrtsd:
        case INS_sqrtpd:
            result.throughput = PERFSCORE_THROUGHPUT_4C;
            result.latency += PERFSCORE_LATENCY_13C;
            break;

        case INS_rcpps:
        case INS_rcpss:
        case INS_rsqrtss:
        case INS_rsqrtps:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency += PERFSCORE_LATENCY_4C;
            break;

        case INS_roundpd:
        case INS_roundps:
        case INS_roundsd:
        case INS_roundss:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency += PERFSCORE_LATENCY_8C;
            break;

        case INS_cvttsd2si:
        case INS_cvtsd2si:
        case INS_cvttss2si:
        case INS_cvtss2si:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency += PERFSCORE_LATENCY_6C;
            break;

        case INS_cvtsi2sd:
        case INS_cvtsi2ss:
            result.throughput = PERFSCORE_THROUGHPUT_2C;
            result.latency += PERFSCORE_LATENCY_6C;
            break;

        case INS_cvtss2sd:
            result.throughput = PERFSCORE_THROUGHPUT_2C;
            result.latency += PERFSCORE_LATENCY_5C;
            break;

        case INS_movaps:
        case INS_movups:
        case INS_movapd:
        case INS_movupd:
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.throughput = PERFSCORE_THROUGHPUT_4X;
                result.latency    = PERFSCORE_LATENCY_1C;
            }
            else if (memAccessKind == PERFSCORE_MEMORY_READ)
            {
                result.throughput = PERFSCORE_THROUGHPUT_2X;
                result.latency += PERFSCORE_LATENCY_2C;
            }
            else
            {
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
                result.throughput = PERFSCORE_THROUGHPUT_1C;
                result.latency += PERFSCORE_LATENCY_3C;
            }
            break;

        case INS_paddb:
        case INS_psubb:
        case INS_paddw:
        case INS_psubw:
        case INS_paddd:
        case INS_psubd:
        case INS_paddq:
        case INS_psubq:
        case INS_paddsb:
        case INS_psubsb:
        case INS_paddsw:
        case INS_psubsw:
        case INS_paddusb:
        case INS_psubusb:
        case INS_paddusw:
        case INS_psubusw:
        case INS_pand:
        case INS_pandn:
        case INS_por:
        case INS_pxor:
        case INS_andpd:
        case INS_andps:
        case INS_andnpd:
        case INS_andnps:
        case INS_orpd:
        case INS_orps:
        case INS_xorpd:
        case INS_xorps:
        case INS_blendps:
        case INS_blendpd:
        case INS_vpblendd:
            result.latency += PERFSCORE_LATENCY_1C;
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.throughput = PERFSCORE_THROUGHPUT_3X;
            }
            else
            {
                result.throughput = PERFSCORE_THROUGHPUT_2X;
            }
            break;

        case INS_andn:
        case INS_pcmpeqb:
        case INS_pcmpeqw:
        case INS_pcmpeqd:
        case INS_pcmpeqq:
        case INS_pcmpgtb:
        case INS_pcmpgtw:
        case INS_pcmpgtd:
        case INS_pavgb:
        case INS_pavgw:
        case INS_pminub:
        case INS_pminsb:
        case INS_pminuw:
        case INS_pminsw:
        case INS_pminud:
        case INS_pminsd:
        case INS_pmaxub:
        case INS_pmaxsb:
        case INS_pmaxuw:
        case INS_pmaxsw:
        case INS_pmaxsd:
        case INS_pmaxud:
        case INS_pabsb:
        case INS_pabsw:
        case INS_pabsd:
        case INS_psignb:
        case INS_psignw:
        case INS_psignd:
        case INS_vpsravd:
        case INS_blendvps:
        case INS_blendvpd:
        case INS_pblendvb:
        case INS_vpsllvd:
        case INS_vpsllvq:
        case INS_vpsrlvd:
        case INS_vpsrlvq:
            result.throughput = PERFSCORE_THROUGHPUT_2X;
            result.latency += PERFSCORE_LATENCY_1C;
            break;

        case INS_pslldq:
        case INS_pslld:
        case INS_psllw:
        case INS_psllq:
        case INS_psrlw:
        case INS_psrld:
        case INS_psrlq:
        case INS_psrldq:
        case INS_psrad:
        case INS_psraw:
            result.latency += PERFSCORE_LATENCY_1C;
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.throughput = PERFSCORE_THROUGHPUT_1C;
            }
            else
            {
                result.throughput = PERFSCORE_THROUGHPUT_2X;
            }
            break;

        case INS_blsi:
        case INS_blsmsk:
        case INS_blsr:
        case INS_bextr:
        case INS_bzhi:
            result.latency += PERFSCORE_LATENCY_2C;
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.throughput = PERFSCORE_THROUGHPUT_2X;
            }
            else
            {
                result.throughput = PERFSCORE_THROUGHPUT_1C;
            }
            break;

        case INS_packuswb:
        case INS_packusdw:
        case INS_packsswb:
        case INS_packssdw:
        case INS_unpcklps:
        case INS_unpckhps:
        case INS_unpcklpd:
        case INS_unpckhpd:
        case INS_punpckldq:
        case INS_punpcklwd:
        case INS_punpcklbw:
        case INS_punpckhdq:
        case INS_punpckhwd:
        case INS_punpckhbw:
        case INS_punpcklqdq:
        case INS_punpckhqdq:
        case INS_pshufb:
        case INS_pshufd:
        case INS_pshuflw:
        case INS_pshufhw:
        case INS_shufps:
        case INS_shufpd:
        case INS_pblendw:
        case INS_movsldup:
        case INS_movshdup:
        case INS_insertps:
        case INS_palignr:
        case INS_vpermilps:
        case INS_vpermilpd:
        case INS_vpermilpsvar:
        case INS_vpermilpdvar:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency += PERFSCORE_LATENCY_1C;
            break;

        case INS_vblendvps:
        case INS_vblendvpd:
        case INS_vpblendvb:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.latency = PERFSCORE_LATENCY_2C;
            }
            break;

        case INS_bswap:
            if ((id->idOpSize() == EA_8BYTE))
            {
                result.throughput = PERFSCORE_THROUGHPUT_1C;
                result.latency    = PERFSCORE_LATENCY_2C;
            }
            else
            {
                assert(id->idOpSize() == EA_4BYTE);
                result.throughput = PERFSCORE_THROUGHPUT_2X;
                result.latency    = PERFSCORE_LATENCY_1C;
            }
            break;

        case INS_pmovmskb:
        case INS_movmskpd:
        case INS_movmskps:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency += PERFSCORE_LATENCY_2C;
            break;

        case INS_bsf:
        case INS_bsr:
        case INS_pextrb:
        case INS_pextrd:
        case INS_pextrw:
        case INS_pextrq:
        case INS_pextrw_sse41:
        case INS_lzcnt:
        case INS_tzcnt:
        case INS_popcnt:
        case INS_crc32:
        case INS_rorx:
        case INS_pdep:
        case INS_pext:
        case INS_addsubps:
        case INS_addsubpd:
        case INS_pcmpgtq:
        case INS_psadbw:
        case INS_vpermps:
        case INS_vpermpd:
        case INS_vpermd:
        case INS_vpermq:
        case INS_vperm2i128:
        case INS_vperm2f128:
        case INS_pmovsxbw:
        case INS_pmovsxbd:
        case INS_pmovsxbq:
        case INS_pmovsxwd:
        case INS_pmovsxwq:
        case INS_pmovsxdq:
        case INS_pmovzxbw:
        case INS_pmovzxbd:
        case INS_pmovzxbq:
        case INS_pmovzxwd:
        case INS_pmovzxwq:
        case INS_pmovzxdq:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency += PERFSCORE_LATENCY_3C;
            break;

        case INS_phaddw:
        case INS_phaddd:
        case INS_phaddsw:
        case INS_phsubw:
        case INS_phsubsw:
        case INS_phsubd:
            result.throughput = PERFSCORE_THROUGHPUT_2C;
            result.latency += PERFSCORE_LATENCY_3C;
            break;

        case INS_cmpps:
        case INS_cmppd:
        case INS_cmpss:
        case INS_cmpsd:
            result.throughput = PERFSCORE_THROUGHPUT_2X;
            result.latency    = PERFSCORE_LATENCY_4C;
            break;

        case INS_mulx:
        case INS_maxps:
        case INS_maxpd:
        case INS_maxss:
        case INS_maxsd:
        case INS_minps:
        case INS_minpd:
        case INS_minss:
        case INS_minsd:
        case INS_ptest:
        case INS_phminposuw:
        case INS_extractps:
        case INS_vextractf128:
        case INS_vextracti128:
        case INS_vinsertf128:
        case INS_vinserti128:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency += PERFSCORE_LATENCY_4C;
            break;

        case INS_mpsadbw:
            result.throughput = PERFSCORE_THROUGHPUT_2C;
            result.latency += PERFSCORE_LATENCY_4C;
            break;

        case INS_pmullw:
        case INS_pmulhw:
        case INS_pmulhuw:
        case INS_pmulhrsw:
        case INS_pmuldq:
        case INS_pmuludq:
        case INS_pmaddwd:
        case INS_pmaddubsw:
            result.throughput = PERFSCORE_THROUGHPUT_2X;
            result.latency += PERFSCORE_LATENCY_5C;
            break;

        case INS_cvtsd2ss:
        case INS_cvtps2pd:
        case INS_cvtpd2dq:
        case INS_cvtdq2pd:
        case INS_cvtpd2ps:
        case INS_cvttpd2dq:
        case INS_vtestps:
        case INS_vtestpd:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency += PERFSCORE_LATENCY_5C;
            break;

        case INS_hsubps:
        case INS_hsubpd:
            result.throughput = PERFSCORE_THROUGHPUT_2C;
            result.latency += PERFSCORE_LATENCY_6C;
            break;

        case INS_pclmulqdq:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency += PERFSCORE_LATENCY_7C;
            break;

        case INS_pmulld:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency += PERFSCORE_LATENCY_10C;
            break;

        case INS_vpbroadcastb:
        case INS_vpbroadcastw:
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.throughput = PERFSCORE_THROUGHPUT_1C;
                result.latency    = PERFSCORE_LATENCY_1C;
            }
            else
            {
                result.throughput = PERFSCORE_THROUGHPUT_1C;
                result.latency    = max(PERFSCORE_LATENCY_3C, result.latency);
            }
            break;

        case INS_vpbroadcastd:
        case INS_vpbroadcastq:
        case INS_vbroadcasti128:
        case INS_vbroadcastf128:
        case INS_vbroadcastss:
        case INS_vbroadcastsd:
            result.latency += PERFSCORE_LATENCY_3C;
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.throughput = PERFSCORE_THROUGHPUT_1C;
            }
            else
            {
                result.throughput = PERFSCORE_THROUGHPUT_2X;
            }
            break;

        case INS_pinsrb:
        case INS_pinsrw:
        case INS_pinsrd:
        case INS_pinsrq:
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.throughput = PERFSCORE_THROUGHPUT_2C;
                result.latency    = PERFSCORE_LATENCY_3C;
            }
            else
            {
                result.throughput = PERFSCORE_THROUGHPUT_1C;
            }
            break;

        case INS_dppd:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency    = PERFSCORE_LATENCY_9C;
            break;

        case INS_dpps:
            result.throughput = PERFSCORE_THROUGHPUT_2C;
            result.latency    = PERFSCORE_LATENCY_13C;
            break;

        case INS_vfmadd132pd:
        case INS_vfmadd213pd:
        case INS_vfmadd231pd:
        case INS_vfmadd132ps:
        case INS_vfmadd213ps:
        case INS_vfmadd231ps:
        case INS_vfmadd132sd:
        case INS_vfmadd213sd:
        case INS_vfmadd231sd:
        case INS_vfmadd132ss:
        case INS_vfmadd213ss:
        case INS_vfmadd231ss:
        case INS_vfmaddsub132pd:
        case INS_vfmaddsub213pd:
        case INS_vfmaddsub231pd:
        case INS_vfmaddsub132ps:
        case INS_vfmaddsub213ps:
        case INS_vfmaddsub231ps:
        case INS_vfmsubadd132pd:
        case INS_vfmsubadd213pd:
        case INS_vfmsubadd231pd:
        case INS_vfmsubadd132ps:
        case INS_vfmsubadd213ps:
        case INS_vfmsubadd231ps:
        case INS_vfmsub132pd:
        case INS_vfmsub213pd:
        case INS_vfmsub231pd:
        case INS_vfmsub132ps:
        case INS_vfmsub213ps:
        case INS_vfmsub231ps:
        case INS_vfmsub132sd:
        case INS_vfmsub213sd:
        case INS_vfmsub231sd:
        case INS_vfmsub132ss:
        case INS_vfmsub213ss:
        case INS_vfmsub231ss:
        case INS_vfnmadd132pd:
        case INS_vfnmadd213pd:
        case INS_vfnmadd231pd:
        case INS_vfnmadd132ps:
        case INS_vfnmadd213ps:
        case INS_vfnmadd231ps:
        case INS_vfnmadd132sd:
        case INS_vfnmadd213sd:
        case INS_vfnmadd231sd:
        case INS_vfnmadd132ss:
        case INS_vfnmadd213ss:
        case INS_vfnmadd231ss:
        case INS_vfnmsub132pd:
        case INS_vfnmsub213pd:
        case INS_vfnmsub231pd:
        case INS_vfnmsub132ps:
        case INS_vfnmsub213ps:
        case INS_vfnmsub231ps:
        case INS_vfnmsub132sd:
        case INS_vfnmsub213sd:
        case INS_vfnmsub231sd:
        case INS_vfnmsub132ss:
        case INS_vfnmsub213ss:
        case INS_vfnmsub231ss:
        case INS_vpdpbusd:  // will be populated when the HW becomes publicly available
        case INS_vpdpwssd:  // will be populated when the HW becomes publicly available
        case INS_vpdpbusds: // will be populated when the HW becomes publicly available
        case INS_vpdpwssds: // will be populated when the HW becomes publicly available
            // uops.info
            result.throughput = PERFSCORE_THROUGHPUT_2X;
            result.latency += PERFSCORE_LATENCY_4C;
            break;

        case INS_vmaskmovpd:
        case INS_vmaskmovps:

            if (memAccessKind == PERFSCORE_MEMORY_READ)
            {
                result.throughput = PERFSCORE_THROUGHPUT_1C;
                result.latency += PERFSCORE_LATENCY_1C;
            }
            else
            {
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
                result.throughput = PERFSCORE_THROUGHPUT_1C;
                result.latency    = max(PERFSCORE_LATENCY_10C, result.latency);
            }
            break;

        case INS_vpmaskmovd:
        case INS_vpmaskmovq:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            result.latency    = max(PERFSCORE_LATENCY_4C, result.latency);
            break;

        case INS_vpgatherdd:
        case INS_vpgatherdq:
        case INS_vpgatherqd:
        case INS_vpgatherqq:
        case INS_vgatherdps:
        case INS_vgatherdpd:
        case INS_vgatherqps:
        case INS_vgatherqpd:
            result.throughput = PERFSCORE_THROUGHPUT_4C;
            result.latency    = max(PERFSCORE_LATENCY_4C, result.latency);
            break;

        case INS_aesdec:
        case INS_aesdeclast:
        case INS_aesenc:
        case INS_aesenclast:
            result.throughput = PERFSCORE_THROUGHPUT_1C;
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.latency = PERFSCORE_LATENCY_4C;
            }
            break;

        case INS_aesimc:
            result.throughput = PERFSCORE_THROUGHPUT_2C;
            result.latency += PERFSCORE_LATENCY_8C;
            break;

        case INS_aeskeygenassist:
            result.throughput = PERFSCORE_THROUGHPUT_13C;
            result.latency += PERFSCORE_LATENCY_7C;
            break;

        case INS_lfence:
            result.throughput = PERFSCORE_THROUGHPUT_4C;
            break;

        case INS_sfence:
            result.throughput = PERFSCORE_THROUGHPUT_6C;
            break;

        case INS_mfence:
            result.throughput = PERFSCORE_THROUGHPUT_33C;
            break;

        case INS_prefetcht0:
        case INS_prefetcht1:
        case INS_prefetcht2:
        case INS_prefetchnta:
            result.throughput = PERFSCORE_THROUGHPUT_2X;
            break;

        default:
            PerfScoreUnhandledInstr(id, &result);
            break;
    }

    return result;
}

#endif // defined(DEBUG) || defined(LATE_DISASM)
#endif // TARGET_XARCH
