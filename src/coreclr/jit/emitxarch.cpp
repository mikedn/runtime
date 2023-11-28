// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_XARCH

#include "instr.h"
#include "emit.h"
#include "codegen.h"

constexpr bool IsDisp8(ssize_t disp)
{
    return (-128 <= disp) && (disp <= 127);
}

constexpr bool IsDisp32(ssize_t disp)
{
    return (INT32_MIN <= disp) && (disp <= INT32_MAX);
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

constexpr bool IsImm32(ssize_t imm)
{
    return (INT32_MIN <= imm) && (imm <= INT32_MAX);
}

static bool IsShiftCL(instruction ins)
{
    switch (ins)
    {
        case INS_rcl:
        case INS_rcr:
        case INS_rol:
        case INS_ror:
        case INS_shl:
        case INS_shr:
        case INS_sar:
            return true;
        default:
            return false;
    }
}

static bool IsShiftImm(instruction ins)
{
    switch (ins)
    {
        case INS_rcl_N:
        case INS_rcr_N:
        case INS_rol_N:
        case INS_ror_N:
        case INS_shl_N:
        case INS_shr_N:
        case INS_sar_N:
            return true;
        default:
            return false;
    }
}

static bool insIsCMOV(instruction ins)
{
    return ((ins >= INS_cmovo) && (ins <= INS_cmovg));
}

bool emitter::emitIsCondJump(instrDesc* jmp)
{
    instruction ins = jmp->idIns();

    assert(jmp->idInsFmt() == IF_LABEL);

    return (ins != INS_call && ins != INS_jmp);
}

bool emitter::emitIsUncondJump(instrDesc* jmp)
{
    instruction ins = jmp->idIns();

    assert(jmp->idInsFmt() == IF_LABEL);

    return (ins == INS_jmp);
}

#ifdef DEBUG
static bool instrHasImplicitRegPairDest(instruction ins)
{
    return (ins == INS_mulEAX) || (ins == INS_imulEAX) || (ins == INS_div) || (ins == INS_idiv);
}
#endif

static bool IsSSEOrAVXInstruction(instruction ins)
{
    return (ins >= INS_FIRST_SSE_INSTRUCTION) && (ins <= INS_LAST_AVX_INSTRUCTION);
}

static bool IsFMAInstruction(instruction ins)
{
    return (ins >= INS_FIRST_FMA_INSTRUCTION) && (ins <= INS_LAST_FMA_INSTRUCTION);
}

static bool IsAVXVNNIInstruction(instruction ins)
{
    return (ins >= INS_FIRST_AVXVNNI_INSTRUCTION) && (ins <= INS_LAST_AVXVNNI_INSTRUCTION);
}

static bool IsBMIInstruction(instruction ins)
{
    return (ins >= INS_FIRST_BMI_INSTRUCTION) && (ins <= INS_LAST_BMI_INSTRUCTION);
}

static bool IsBMIRegExtInstruction(instruction ins)
{
    return (ins == INS_blsi) || (ins == INS_blsmsk) || (ins == INS_blsr);
}

static unsigned GetBMIOpcodeRMExt(instruction ins)
{
    switch (ins)
    {
        case INS_blsi:
            return 3;
        case INS_blsmsk:
            return 2;
        default:
            assert(ins == INS_blsr);
            return 1;
    }
}

#ifdef DEBUG
static bool isAvxBlendv(instruction ins)
{
    return ins == INS_vblendvps || ins == INS_vblendvpd || ins == INS_vpblendvb;
}

static bool isSse41Blendv(instruction ins)
{
    return ins == INS_blendvps || ins == INS_blendvpd || ins == INS_pblendvb;
}

#endif

static bool IsPrefetch(instruction ins)
{
    return (ins == INS_prefetcht0) || (ins == INS_prefetcht1) || (ins == INS_prefetcht2) || (ins == INS_prefetchnta);
}

bool emitter::UseVEXEncoding() const
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

    INS_Flags_VexDstDstSrc = 1 << 25,
    INS_Flags_VexDstSrcSrc = 1 << 26
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
bool emitter::IsSseDstSrcImm(instruction ins)
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
bool emitter::IsVexDstDstSrc(instruction ins)
{
    return ((InsFlags(ins) & INS_Flags_VexDstDstSrc) != 0) && UseVEXEncoding();
}

// Returns true if the AVX instruction requires 3 operands that duplicate the source
// register in the vvvv field.
// TODO-XArch-Cleanup: This is a temporary solution for now. Eventually this needs to
// be formalized by adding an additional field to instruction table to
// to indicate whether a 3-operand instruction.
bool emitter::IsVexDstSrcSrc(instruction ins)
{
    return ((InsFlags(ins) & INS_Flags_VexDstSrcSrc) != 0) && UseVEXEncoding();
}

#ifdef DEBUG
bool emitter::IsVexTernary(instruction ins)
{
    return IsVexDstDstSrc(ins) || IsVexDstSrcSrc(ins);
}
#endif

#if defined(TARGET_X86) || defined(DEBUG)
static bool instIsFP(instruction ins)
{
#ifdef TARGET_X86
    return (ins == INS_fld) || (ins == INS_fstp);
#else
    return false;
#endif
}
#endif

#ifdef DEBUG
static bool HasWBit(instruction ins)
{
    // TODO-MIKE-Cleanup: It's probably best to make this a flag,
    // or at least ensure that the instruction order is correct.
    return ((INS_add <= ins) && (ins <= INS_movsx)) || ((INS_rol <= ins) && (ins <= INS_stos));
}
#endif

static bool HasSBit(instruction ins)
{
    return ((INS_add <= ins) && (ins <= INS_cmp)) || (ins == INS_imuli);
}

bool emitter::AreFlagsAlwaysModified(instrDesc* id)
{
    instruction ins = id->idIns();

    return !IsShiftCL(ins) && (!IsShiftImm(ins) || id->idIsLargeCns() || (id->idSmallCns() != 0));
}

// Check if some previously emitted instruction set the upper 32 bits of reg to zero.
// Currently only looks back one instruction.
bool emitter::AreUpper32BitsZero(regNumber reg)
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
bool emitter::AreFlagsSetToZeroCmp(regNumber reg, emitAttr opSize, genTreeOps treeOps)
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

bool emitter::TakesVexPrefix(instruction ins) const
{
    return UseVEXEncoding() && (INS_FIRST_SSE_VEX_INSTRUCTION <= ins) && (ins <= INS_LAST_AVX_INSTRUCTION);
}

bool emitter::hasVexPrefix(code_t code)
{
    return ((code >> 48) & 0xFF) == 0xC4;
}

emitter::code_t emitter::AddVexPrefix(instruction ins, code_t code, emitAttr attr)
{
    assert(TakesVexPrefix(ins));
    assert(!hasVexPrefix(code));
    assert((code >> 32) == 0);

    // We start with a 3-byte VEX by default. Once we gather all its
    // components we can decide to emit a 2-byte VEX prefix instead.

    code |= 0xC4E078ull << 32;

    if (attr == EA_32BYTE)
    {
        code |= 0x04ull << 32;
    }

    return code;
}

emitter::code_t emitter::AddVexPrefixIfNeeded(instruction ins, code_t code, emitAttr size)
{
    if (TakesVexPrefix(ins))
    {
        code = AddVexPrefix(ins, code, size);
    }

    return code;
}

bool emitter::hasRexPrefix(code_t code)
{
#ifdef TARGET_AMD64
    const code_t REX_PREFIX_MASK = 0xFF00000000LL;
    return (code & REX_PREFIX_MASK) != 0;
#else
    return false;
#endif
}

// Returns true if this instruction, for the given EA_SIZE(attr), will require a REX.W prefix
static bool TakesRexWPrefix(instruction ins, emitAttr attr)
{
    // Because the current implementation of AVX does not have a way to distinguish between the register
    // size specification (128 vs. 256 bits) and the operand size specification (32 vs. 64 bits), where both are
    // required, the instruction must be created with the register size attribute (EA_16BYTE or EA_32BYTE),
    // and here we must special case these by the opcode.
    switch (ins)
    {
        case INS_vpermpd:
        case INS_vpermq:
        case INS_vpsrlvq:
        case INS_vpsllvq:
        case INS_pinsrq:
        case INS_pextrq:
        case INS_vfmadd132pd:
        case INS_vfmadd213pd:
        case INS_vfmadd231pd:
        case INS_vfmadd132sd:
        case INS_vfmadd213sd:
        case INS_vfmadd231sd:
        case INS_vfmaddsub132pd:
        case INS_vfmaddsub213pd:
        case INS_vfmaddsub231pd:
        case INS_vfmsubadd132pd:
        case INS_vfmsubadd213pd:
        case INS_vfmsubadd231pd:
        case INS_vfmsub132pd:
        case INS_vfmsub213pd:
        case INS_vfmsub231pd:
        case INS_vfmsub132sd:
        case INS_vfmsub213sd:
        case INS_vfmsub231sd:
        case INS_vfnmadd132pd:
        case INS_vfnmadd213pd:
        case INS_vfnmadd231pd:
        case INS_vfnmadd132sd:
        case INS_vfnmadd213sd:
        case INS_vfnmadd231sd:
        case INS_vfnmsub132pd:
        case INS_vfnmsub213pd:
        case INS_vfnmsub231pd:
        case INS_vfnmsub132sd:
        case INS_vfnmsub213sd:
        case INS_vfnmsub231sd:
        case INS_vpmaskmovq:
        case INS_vpgatherdq:
        case INS_vpgatherqq:
        case INS_vgatherdpd:
        case INS_vgatherqpd:
            return true;
        default:
            break;
    }

#ifdef TARGET_X86
    return false;
#else
    // movsx should always sign extend out to 8 bytes just because we don't track
    // whether the dest should be 4 bytes or 8 bytes (attr indicates the size
    // of the source, not the dest).
    // movsxd should always have a REX.W prefix, otherwise it's equivalent to mov.
    // A 4-byte movzx is equivalent to an 8 byte movzx, so it is not special
    // cased here.

    if (ins == INS_movsx || ins == INS_movsxd || ins == INS_rex_jmp)
    {
        return true;
    }

    if (EA_SIZE(attr) != EA_8BYTE)
    {
        return false;
    }

    if (IsSSEOrAVXInstruction(ins))
    {
        switch (ins)
        {
            case INS_movd: // TODO-Cleanup: replace with movq, https://github.com/dotnet/runtime/issues/47943.
            case INS_andn:
            case INS_bextr:
            case INS_blsi:
            case INS_blsmsk:
            case INS_blsr:
            case INS_bzhi:
            case INS_cvttsd2si:
            case INS_cvttss2si:
            case INS_cvtsd2si:
            case INS_cvtss2si:
            case INS_cvtsi2sd:
            case INS_cvtsi2ss:
            case INS_movnti:
            case INS_mulx:
            case INS_pdep:
            case INS_pext:
            case INS_rorx:
                return true;
            default:
                return false;
        }
    }

    // TODO-XArch-Cleanup: Better way to not emit REX.W when we don't need it, than just testing
    // all these opcodes...
    // These are all the instructions that default to 8-byte operand without the REX.W bit
    // With 1 special case: movzx because the 4 byte version still zeros-out the hi 4 bytes
    // so we never need it

    return (ins != INS_push) && (ins != INS_pop) && (ins != INS_movq) && (ins != INS_movzx) && (ins != INS_push_hide) &&
           (ins != INS_pop_hide) && (ins != INS_ret) && (ins != INS_call) && !((ins >= INS_i_jmp) && (ins <= INS_jg));
#endif // TARGET_AMD64
}

// Returns true if using this register will require a REX.* prefix.
// Since XMM registers overlap with YMM registers, this routine
// can also be used to know whether a YMM register if the
// instruction in question is AVX.
bool IsExtendedReg(regNumber reg)
{
#ifdef TARGET_AMD64
    return ((reg >= REG_R8) && (reg <= REG_R15)) || ((reg >= REG_XMM8) && (reg <= REG_XMM15));
#else
    // X86 JIT operates in 32-bit mode and hence extended reg are not available.
    return false;
#endif
}

// Returns true if using this register, for the given EA_SIZE(attr), will require a REX.* prefix
bool IsExtendedReg(regNumber reg, emitAttr attr)
{
#ifdef TARGET_AMD64
    // Not a register, so doesn't need a prefix
    if (reg > REG_XMM15)
    {
        return false;
    }

    // Opcode field only has 3 bits for the register, these high registers
    // need a 4th bit, that comes from the REX prefix (eiter REX.X, REX.R, or REX.B)
    if (IsExtendedReg(reg))
    {
        return true;
    }

    if (EA_SIZE(attr) != EA_1BYTE)
    {
        return false;
    }

    // There are 12 one byte registers addressible 'below' r8b:
    //     al, cl, dl, bl, ah, ch, dh, bh, spl, bpl, sil, dil.
    // The first 4 are always addressible, the last 8 are divided into 2 sets:
    //     ah,  ch,  dh,  bh
    //          -- or --
    //     spl, bpl, sil, dil
    // Both sets are encoded exactly the same, the difference is the presence
    // of a REX prefix, even a REX prefix with no other bits set (0x40).
    // So in order to get to the second set we need a REX prefix (but no bits).
    //
    // TODO-AMD64-CQ: if we ever want to start using the first set, we'll need a different way of
    // encoding/tracking/encoding registers.
    return (reg >= REG_RSP);
#else
    // X86 JIT operates in 32-bit mode and hence extended reg are not available.
    return false;
#endif
}

static_assert_no_msg((REG_RAX & 0x7) == 0);
static_assert_no_msg((REG_XMM0 & 0x7) == 0);

// Returns bits to be encoded in instruction for the given register.
unsigned RegEncoding(regNumber reg)
{
    return static_cast<unsigned>(reg & 0x7);
}

// Utility routines that abstract the logic of adding REX.W, REX.R, REX.X, REX.B and REX prefixes
// SSE2: separate 1-byte prefix gets added before opcode.
// AVX:  specific bits within VEX prefix need to be set in bit-inverted form.
emitter::code_t emitter::AddRexWPrefix(instruction ins, code_t code)
{
    if (TakesVexPrefix(ins))
    {
        assert(hasVexPrefix(code));

        return code | (0x80ull << 32);
    }

#ifdef TARGET_AMD64
    return code | (0x48ull << 32);
#else
    unreached();
#endif
}

#ifdef TARGET_AMD64

emitter::code_t emitter::AddRexRPrefix(instruction ins, code_t code)
{
    if (TakesVexPrefix(ins))
    {
        assert(hasVexPrefix(code));

        return code & ~(0x8000ull << 32);
    }

    return code | (0x44ull << 32);
}

emitter::code_t emitter::AddRexXPrefix(instruction ins, code_t code)
{
    if (TakesVexPrefix(ins))
    {
        assert(hasVexPrefix(code));

        return code & ~(0x4000ull << 32);
    }

    return code | (0x42ull << 32);
}

emitter::code_t emitter::AddRexBPrefix(instruction ins, code_t code)
{
    if (TakesVexPrefix(ins))
    {
        assert(hasVexPrefix(code));

        return code & ~(0x2000ull << 32);
    }

    return code | (0x41ull << 32);
}

// Adds REX prefix (0x40) without W, R, X or B bits set
emitter::code_t emitter::AddRexPrefix(instruction ins, code_t code)
{
    assert(!TakesVexPrefix(ins));

    return code | (0x40ull << 32);
}

#endif // TARGET_AMD64

static bool isPrefix(uint8_t b)
{
    assert(b != 0);    // Caller should check this
    assert(b != 0x67); // We don't use the address size prefix
    assert(b != 0x65); // The GS segment override prefix is emitted separately
    assert(b != 0x64); // The FS segment override prefix is emitted separately
    assert(b != 0xF0); // The lock prefix is emitted separately
    assert(b != 0x2E); // We don't use the CS segment override prefix
    assert(b != 0x3E); // Or the DS segment override prefix
    assert(b != 0x26); // Or the ES segment override prefix
    assert(b != 0x36); // Or the SS segment override prefix

    // That just leaves the size prefixes used in SSE opcodes:
    //      Scalar Double  Scalar Single  Packed Double
    return (b == 0xF2) || (b == 0xF3) || (b == 0x66);
}

size_t emitter::emitOutputVexPrefix(instruction ins, uint8_t* dst, code_t& code)
{
    assert(TakesVexPrefix(ins) && hasVexPrefix(code));

    uint32_t vexPrefix = (code >> 32) & UINT_MAX;
    code &= UINT_MAX;

    uint16_t leadingBytes = 0;
    uint8_t  check        = (code >> 24) & 0xFF;

    if (check == 0)
    {
        // 2-byte opcode with the bytes ordered as 0x0011RM22
        // the byte in position 11 must be an escape byte.

        leadingBytes = (code >> 16) & 0xFF;
        assert(leadingBytes == 0x0F);
        code &= 0xFFFF;
    }
    else
    {
        // 3-byte opcode: with the bytes ordered as 0x2211RM33 or
        // 4-byte opcode: with the bytes ordered as 0x22114433
        // check for a prefix in the 11 position

        uint8_t sizePrefix = (code >> 16) & 0xFF;

        if ((sizePrefix != 0) && isPrefix(sizePrefix))
        {
            // 'pp' bits in byte2 of VEX prefix allows us to encode SIMD size prefixes as two bits
            //
            //   00  - None   (0F    - packed float)
            //   01  - 66     (66 0F - packed double)
            //   10  - F3     (F3 0F - scalar float
            //   11  - F2     (F2 0F - scalar double)

            switch (sizePrefix)
            {
                case 0x66:
                    if (!IsBMIInstruction(ins))
                    {
                        vexPrefix |= 0x01;
                    }
                    else
                    {
                        switch (ins)
                        {
                            case INS_rorx:
                            case INS_pdep:
                            case INS_mulx:
                                vexPrefix |= 0x03;
                                break;
                            case INS_pext:
                                vexPrefix |= 0x02;
                                break;
                            default:
                                vexPrefix |= 0x00;
                                break;
                        }
                    }
                    break;
                case 0xF3:
                    vexPrefix |= 0x02;
                    break;
                case 0xF2:
                    vexPrefix |= 0x03;
                    break;
                default:
                    unreached();
            }

            // Now the byte in the 22 position must be an escape byte 0F
            leadingBytes = check;
            assert(leadingBytes == 0x0F);

            // Get rid of both sizePrefix and escape byte
            code &= 0xFFFF;

            // Check the byte in the 33 position to see if it is 3A or 38.
            // In such a case escape bytes must be 0x0F3A or 0x0F38
            check = code & 0xFF;

            if (check == 0x3A || check == 0x38)
            {
                leadingBytes = (leadingBytes << 8) | check;

                code &= 0xFF00;
            }
        }
    }

    switch (leadingBytes)
    {
        case 0x0F:
            vexPrefix |= 0x0100;
            break;
        case 0x0F38:
            vexPrefix |= 0x0200;
            break;
        case 0x0F3A:
            vexPrefix |= 0x0300;
            break;
        default:
            unreached();
    }

    if ((vexPrefix & 0xFFFF7F80) == 0x00C46100)
    {
        // Encoding optimization calculation is not done while estimating the instruction
        // size and thus over-predict instruction size by 1 byte.
        // If there are IGs that will be aligned, do not optimize encoding so the
        // estimated alignment sizes are accurate.

        if (emitCurIG->igNum > emitLastAlignedIgNum)
        {
            emitOutputByte(dst, 0xC5);
            emitOutputByte(dst + 1, ((vexPrefix >> 8) & 0x80) | (vexPrefix & 0x7F));

            return 2;
        }
    }

    emitOutputByte(dst, 0xC4);
    emitOutputByte(dst + 1, vexPrefix >> 8);
    emitOutputByte(dst + 2, vexPrefix);

    return 3;
}

#ifdef TARGET_AMD64
size_t emitter::emitOutputRexPrefix(instruction ins, uint8_t* dst, code_t& code)
{
    uint8_t prefix = (code >> 32) & 0xFF;
    noway_assert((prefix >= 0x40) && (prefix <= 0x4F));
    code &= UINT_MAX;

    // TODO-AMD64-Cleanup: when we remove the prefixes (just the SSE opcodes right now)
    // we can remove this code as well

    // The REX prefix is required to come after all other prefixes.
    // Some of our 'opcodes' actually include some prefixes, if that
    // is the case, shift them over and place the REX prefix after
    // the other prefixes, and emit any prefix that got moved out.

    uint8_t check = (code >> 24) & 0xFF;

    if (check == 0)
    {
        // 3-byte opcode: with the bytes ordered as 0x00113322
        // check for a prefix in the 11 position.

        check = (code >> 16) & 0xFF;

        if ((check != 0) && isPrefix(check))
        {
            // Swap the rex prefix and whatever this prefix is
            code   = (static_cast<uint32_t>(prefix) << 16) | (code & 0xFFFF);
            prefix = check;
        }
    }
    else
    {
        // 4-byte opcode with the bytes ordered as 0x22114433
        // first check for a prefix in the 11 position

        uint8_t check2 = (code >> 16) & 0xFF;

        if (isPrefix(check2))
        {
            assert(!isPrefix(check)); // We currently don't use this, so it is untested

            if (isPrefix(check))
            {
                // 3 prefixes were rex = rr, check = c1, check2 = c2 encoded as 0xrrc1c2XXXX
                // Change to c2rrc1XXXX, and emit check2 now

                code = (static_cast<uint32_t>(prefix) << 24) | (static_cast<uint32_t>(check) << 16) | (code & 0xFFFF);
            }
            else
            {
                // 2 prefixes were rex = rr, check2 = c2 encoded as 0xrrXXc2XXXX, (check is part of the opcode)
                // Change to c2XXrrXXXX, and emit check2 now

                code = (static_cast<uint32_t>(check) << 24) | (static_cast<uint32_t>(prefix) << 16) | (code & 0xFFFF);
            }

            prefix = check2;
        }
    }

    return emitOutputByte(dst, prefix);
}
#endif // TARGET_AMD64

size_t emitter::emitOutputRexPrefixIfNeeded(instruction ins, uint8_t* dst, code_t& code)
{
    assert(!hasVexPrefix(code));

#ifdef TARGET_AMD64
    if ((code >> 32) != 0)
    {
        return emitOutputRexPrefix(ins, dst, code);
    }
#endif

    return 0;
}

size_t emitter::emitOutputRexOrVexPrefixIfNeeded(instruction ins, uint8_t* dst, code_t& code)
{
    if (hasVexPrefix(code))
    {
        return emitOutputVexPrefix(ins, dst, code);
    }

#ifdef TARGET_AMD64
    if ((code >> 32) != 0)
    {
        return emitOutputRexPrefix(ins, dst, code);
    }
#endif

    return 0;
}

static bool IsSSE38orSSE3A(uint64_t code);

#ifdef TARGET_X86
void emitter::emitMarkStackLvl(unsigned stackLevel)
{
    assert(int(stackLevel) >= 0);
    assert(emitCurStackLvl == 0);
    assert(emitCurIG->igStkLvl == 0);
    assert(emitCurIGfreeNext == emitCurIGfreeBase);

    assert(stackLevel && stackLevel % REGSIZE_BYTES == 0);

    emitCurStackLvl = emitCurIG->igStkLvl = stackLevel;

    if (emitMaxStackDepth < emitCurStackLvl)
    {
        JITDUMP("Upping emitMaxStackDepth from %d to %d\n", emitMaxStackDepth, emitCurStackLvl);
        emitMaxStackDepth = emitCurStackLvl;
    }
}
#endif

#ifdef WINDOWS_X86_ABI
// Special CORINFO_FIELD_HANDLE that references the FS segment, for x86 TLS access.
static const CORINFO_FIELD_HANDLE FS_SEG_FIELD = reinterpret_cast<CORINFO_FIELD_HANDLE>(-8);
#endif

#ifdef DEBUG
static bool FieldDispRequiresRelocation(CORINFO_FIELD_HANDLE field)
{
#ifdef WINDOWS_X86_ABI
    return field != FS_SEG_FIELD;
#else
    return true;
#endif
}
#endif

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

    assert(ins < _countof(insNames));
    assert(insNames[ins] != nullptr);

    return insNames[ins];
}

enum insUpdateModes
{
    IUM_RD,
    IUM_WR,
    IUM_RW,
};

static insUpdateModes emitInsUpdateMode(instruction ins)
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

    return static_cast<insUpdateModes>(emitInsModeFmtTab[ins]);
}

insFormat emitter::emitInsModeFormat(instruction ins, insFormat base)
{
    assert(IF_RRD + IUM_RD == IF_RRD);
    assert(IF_RRD + IUM_WR == IF_RWR);
    assert(IF_RRD + IUM_RW == IF_RRW);

    return (insFormat)(base + emitInsUpdateMode(ins));
}

// Returns the base encoding of the given CPU instruction.
static size_t insCodeJ(instruction ins)
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
static size_t insCodeACC(instruction ins)
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
static size_t insCodeRR(instruction ins)
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
static size_t insCodeRM(instruction ins)
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
static bool hasCodeMI(instruction ins)
{
    assert(ins < _countof(insCodesMI));

    return insCodesMI[ins] != BAD_CODE;
}

static size_t insCodeMI(instruction ins)
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

static bool hasCodeMR(instruction ins)
{
    assert(ins < _countof(insCodesMR));

    return insCodesMR[ins] != BAD_CODE;
}

// Returns the "RM, REG" or "RM" encoding of the given instruction.
static size_t insCodeMR(instruction ins)
{
    assert(ins < _countof(insCodesMR));
    assert(insCodesMR[ins] != BAD_CODE);

    return insCodesMR[ins];
}

// Return true if the instruction uses the SSE38 or SSE3A macro in instrsXArch.h.
static bool IsSSE38orSSE3A(uint64_t code)
{
    const uint64_t SSE38 = 0x0F660038ull;
    const uint64_t SSE3A = 0x0F66003Aull;
    const uint64_t MASK  = 0xFFFF00FFull;

    code &= MASK;
    return (code == SSE38) || (code == SSE3A);
}

// Returns an encoding for the specified register to be used in the bits 0-2 of an opcode.
unsigned emitter::insEncodeReg012(instruction ins, regNumber reg, emitAttr size, code_t* code)
{
    assert(reg < REG_STK);
    assert(code != nullptr);

#ifdef TARGET_AMD64
    if (IsExtendedReg(reg))
    {
        *code = AddRexBPrefix(ins, *code);
    }
    else if ((size == EA_1BYTE) && (reg > REG_RBX))
    {
        // We are assuming that we only use/encode SPL, BPL, SIL and DIL
        // not the corresponding AH, CH, DH, or BH
        *code = AddRexPrefix(ins, *code);
    }
#endif // TARGET_AMD64

    return RegEncoding(reg);
}

// Returns an encoding for the specified register to be used in the bits 3-5 of an opcode.
unsigned emitter::insEncodeReg345(instruction ins, regNumber reg, emitAttr size, code_t* code)
{
    assert(reg < REG_STK);
    assert(code != nullptr);

#ifdef TARGET_AMD64
    if (IsExtendedReg(reg))
    {
        *code = AddRexRPrefix(ins, *code);
    }
    else if ((size == EA_1BYTE) && (reg > REG_RBX))
    {
        // We are assuming that we only use/encode SPL, BPL, SIL and DIL
        // not the corresponding AH, CH, DH, or BH
        *code = AddRexPrefix(ins, *code);
    }
#endif // TARGET_AMD64

    return RegEncoding(reg) << 3;
}

emitter::code_t emitter::SetRMReg(instruction ins, regNumber reg, emitAttr size, code_t code)
{
    assert(!IsSSE38orSSE3A(ins) && (ins != INS_crc32));
    code |= insEncodeReg345(ins, reg, size, &code) << 8;
    return code;
}

emitter::code_t emitter::SetVexVvvv(instruction ins, regNumber reg, emitAttr size, code_t code)
{
    assert(reg < REG_STK);
    assert(TakesVexPrefix(ins));
    assert(hasVexPrefix(code));

    code_t regBits = RegEncoding(reg);

    if (IsExtendedReg(reg))
    {
        regBits |= 0x08;
    }

    assert(regBits <= 0xF);
    regBits <<= 35;
    return code ^ regBits;
}

// Returns the "[r/m]" opcode with the mod/RM field set to register.
emitter::code_t emitter::insEncodeRMreg(instruction ins, code_t code)
{
    // If Byte 4 (which is 0xFF00) is 0, that's where the RM encoding goes.
    // Otherwise, it will be placed after the 4 byte encoding.
    if ((code & 0xFF00) == 0)
    {
        assert((code & 0xC000) == 0);
        code |= 0xC000;
    }

    return code;
}

// Returns the "byte ptr [r/m]" opcode with the mod/RM field set to the given register.
emitter::code_t emitter::insEncodeRMreg(instruction ins, regNumber reg, emitAttr size, code_t code)
{
    assert((code & 0xC000) == 0);
    code |= 0xC000;
    code |= insEncodeReg012(ins, reg, size, &code) << 8;
    return code;
}

// Returns the "byte ptr [r/m], icon" opcode with the mod/RM field set to the given register.
emitter::code_t emitter::insEncodeMIreg(instruction ins, regNumber reg, emitAttr size, code_t code)
{
    assert((code & 0xC000) == 0);
    code |= 0xC000;
    code |= insEncodeReg012(ins, reg, size, &code) << 8;
    return code;
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

static bool BaseRegRequiresSIB(regNumber base)
{
#ifdef TARGET_AMD64
    return base == REG_ESP || base == REG_R12;
#else
    return base == REG_ESP;
#endif
}

static bool BaseRegRequiresDisp(regNumber base)
{
#ifdef TARGET_AMD64
    return base == REG_EBP || base == REG_R13;
#else
    return base == REG_EBP;
#endif
}

instruction emitter::emitJumpKindToIns(emitJumpKind jumpKind)
{
    static const instruction map[]{INS_nop,
#define JMP_SMALL(en, rev, ins) INS_##ins,
#include "emitjmps.h"
                                   INS_call};

    assert(jumpKind < _countof(map));
    return map[jumpKind];
}

emitJumpKind emitter::emitReverseJumpKind(emitJumpKind jumpKind)
{
    static const emitJumpKind map[]{
        EJ_NONE,
#define JMP_SMALL(en, rev, ins) EJ_##rev,
#include "emitjmps.h"
    };

    assert(jumpKind < EJ_COUNT);
    return map[jumpKind];
}

// When encoding instructions that operate on byte registers on x86
// we have to ensure that we use a low register (EAX, EBX, ECX or EDX).
bool emitter::emitVerifyEncodable(instruction ins, emitAttr size, regNumber reg1, regNumber reg2)
{
#ifdef TARGET_X86
    if (size != EA_1BYTE) // Not operating on a byte register is fine
    {
        return true;
    }

    if ((ins != INS_movsx) && // These three instructions support high register
        (ins != INS_movzx)    // encodings for reg1
        && (ins != INS_crc32))
    {
        // reg1 must be a byte-able register
        if ((genRegMask(reg1) & RBM_BYTE_REGS) == 0)
        {
            return false;
        }
    }
    // if reg2 is not REG_NA then reg2 must be a byte-able register
    if ((reg2 != REG_NA) && ((genRegMask(reg2) & RBM_BYTE_REGS) == 0))
    {
        return false;
    }
#endif

    return true;
}

unsigned emitter::emitGetAdjustedSize(instruction ins, emitAttr attr, code_t code)
{
    assert(ins != INS_invalid);

    unsigned adjustedSize = 0;

    if (TakesVexPrefix(ins))
    {
        // VEX prefix encodes some bytes of the opcode and as a result, overall size of the instruction reduces.
        // Therefore, to estimate the size adding VEX prefix size and size of instruction opcode bytes will always
        // overstimate.
        // Instead this routine will adjust the size of VEX prefix based on the number of bytes of opcode it encodes so
        // that
        // instruction size estimate will be accurate.
        // Basically this  will decrease the vexPrefixSize, so that opcodeSize + vexPrefixAdjustedSize will be the right
        // size.
        //
        // rightOpcodeSize + vexPrefixSize
        //  = (opcodeSize - ExtrabytesSize) + vexPrefixSize
        //  = opcodeSize + (vexPrefixSize - ExtrabytesSize)
        //  = opcodeSize + vexPrefixAdjustedSize

        unsigned vexPrefixAdjustedSize = 3;

        // In this case, opcode will contains escape prefix at least one byte,
        // vexPrefixAdjustedSize should be minus one.
        vexPrefixAdjustedSize -= 1;

        // Get the fourth byte in Opcode.
        // If this byte is non-zero, then we should check whether the opcode contains SIMD prefix or not.
        uint8_t check = (code >> 24) & 0xFF;
        if (check != 0)
        {
            // 3-byte opcode: with the bytes ordered as 0x2211RM33 or
            // 4-byte opcode: with the bytes ordered as 0x22114433
            // Simd prefix is at the first byte.
            uint8_t sizePrefix = (code >> 16) & 0xFF;
            if (sizePrefix != 0 && isPrefix(sizePrefix))
            {
                vexPrefixAdjustedSize -= 1;
            }

            // If the opcode size is 4 bytes, then the second escape prefix is at fourth byte in opcode.
            // But in this case the opcode has not counted R\M part.
            // opcodeSize + VexPrefixAdjustedSize - ExtraEscapePrefixSize + ModR\MSize
            //=opcodeSize + VexPrefixAdjustedSize -1 + 1
            //=opcodeSize + VexPrefixAdjustedSize
            // So although we may have second byte escape prefix, we won't decrease vexPrefixAdjustedSize.
        }

        adjustedSize = vexPrefixAdjustedSize;
    }
    else if (IsSSE38orSSE3A(code))
    {
        // The 4-Byte SSE instructions require one additional byte to hold the ModRM byte
        adjustedSize++;
    }
    else
    {
        if (ins == INS_crc32)
        {
            // Adjust code size for CRC32 that has 4-byte opcode but does not use SSE38 or EES3A encoding.
            adjustedSize++;
        }

        if ((attr == EA_2BYTE) && (ins != INS_movzx) && (ins != INS_movsx))
        {
            // Most 16-bit operand instructions will need a 0x66 prefix.
            adjustedSize++;
        }
    }

    return adjustedSize;
}

unsigned emitter::emitInsSize(code_t code)
{
    assert(!hasRexPrefix(code) && !hasVexPrefix(code));

    return (code & 0xFF000000) ? 4 : (code & 0x00FF0000) ? 3 : 2;
}

unsigned emitter::emitInsSizeRR(instrDesc* id, code_t code)
{
    assert(!hasRexPrefix(code) && !hasVexPrefix(code));

    instruction ins  = id->idIns();
    emitAttr    size = id->idOpSize();

    unsigned sz = emitGetAdjustedSize(ins, size, code);

    if (!TakesVexPrefix(ins) &&
        (TakesRexWPrefix(ins, size) || IsExtendedReg(id->idReg1(), size) || IsExtendedReg(id->idReg2(), size) ||
         (!id->idIsSmallDsc() && (IsExtendedReg(id->idReg3(), size) || IsExtendedReg(id->idReg4(), size)))))
    {
        sz++;
    }

    return sz + emitInsSize(code);
}

unsigned emitter::emitInsSizeRR(instruction ins, regNumber reg1, regNumber reg2, emitAttr attr)
{
    emitAttr size = EA_SIZE(attr);

    code_t code = insCodeRM(ins);
    assert(!hasRexPrefix(code) && !hasVexPrefix(code));

    unsigned sz = emitGetAdjustedSize(ins, size, code);

    if (!TakesVexPrefix(ins) && ((TakesRexWPrefix(ins, size) && ((ins != INS_xor) || (reg1 != reg2))) ||
                                 IsExtendedReg(reg1, attr) || IsExtendedReg(reg2, attr)))
    {
        sz++;
    }

    // TODO-MIKE-Review: This stuff is dubious. The byte claimed to be 0 can contain
    // the RM opcode extension and that doesn't affect the instruction size.
    //
    // If Byte 4 (which is 0xFF00) is zero, that's where the RM encoding goes.
    // Otherwise, it will be placed after the 4 byte encoding, making the total 5 bytes.
    // This would probably be better expressed as a different format or something?

    if (((code & 0xFF00) == 0) || IsSSEOrAVXInstruction(ins))
    {
        sz += emitInsSize(code);
    }
    else
    {
        sz += 5;
    }

    return sz;
}

unsigned emitter::emitInsSizeSV(instrDesc* id, code_t code)
{
    assert(!hasRexPrefix(code) && !hasVexPrefix(code));

    instruction ins  = id->idIns();
    emitAttr    size = id->idOpSize();

    unsigned sz = emitGetAdjustedSize(ins, size, code);

    if (!TakesVexPrefix(ins) &&
        (TakesRexWPrefix(ins, size) || IsExtendedReg(id->idReg1(), size) || IsExtendedReg(id->idReg2(), size)))
    {
        sz++;
    }

#ifdef TARGET_AMD64
    // TODO-MIKE-Cleanup: Old code managed to count the REX prefix twice for movsxd.
    if ((ins == INS_movsxd) && IsExtendedReg(id->idReg1()))
    {
        sz++;
    }
#endif

    sz += emitInsSize(code);

    bool ebpBased = id->idAddr()->isEbpBased;
    int  disp     = id->idAddr()->lclOffset;

    if (!ebpBased)
    {
#if !FEATURE_FIXED_OUT_ARGS
        disp += emitCurStackLvl;
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

unsigned emitter::emitInsSizeAM(instrDesc* id, code_t code)
{
    assert(!hasRexPrefix(code) && !hasVexPrefix(code));

    instruction ins      = id->idIns();
    emitAttr    size     = id->idOpSize();
    ssize_t     disp     = (ins == INS_call) ? emitGetInsCallDisp(id) : emitGetInsAmdDisp(id);
    bool        hasDisp8 = ((int8_t)disp == disp) && !id->idIsDspReloc();
    bool        hasDisp  = (disp != 0) || id->idIsDspReloc();
    regNumber   baseReg;
    regNumber   indexReg;

    // BT supports 16 bit operands and this code doesn't handle the necessary 66 prefix.
    assert(ins != INS_bt);

    switch (id->idInsFmt())
    {
        case IF_RWR_LABEL:
        case IF_MRW_CNS:
        case IF_MRW_RRD:
            baseReg  = REG_NA;
            indexReg = REG_NA;
            break;
        default:
            baseReg  = id->idAddr()->iiaAddrMode.amBaseReg;
            indexReg = id->idAddr()->iiaAddrMode.amIndxReg;
            break;
    }

    unsigned sz = emitInsSize(code);
    sz += emitGetAdjustedSize(ins, size, code);

    if (!TakesVexPrefix(ins) &&
        (TakesRexWPrefix(ins, size) || IsExtendedReg(baseReg, EA_PTRSIZE) || IsExtendedReg(indexReg, EA_PTRSIZE) ||
         ((ins != INS_call) && (IsExtendedReg(id->idReg1(), size) || IsExtendedReg(id->idReg2(), size)))))
    {
        sz++;
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

    if (id->idAddr()->iiaAddrMode.amScale != 0)
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
        id->idAddr()->iiaAddrMode.amBaseReg = baseReg;
        id->idAddr()->iiaAddrMode.amIndxReg = indexReg;
    }

    if (hasDisp || BaseRegRequiresDisp(baseReg))
    {
        sz += hasDisp8 ? 1 : 4;
    }

    return sz;
}

unsigned emitter::emitInsSizeCV(instrDesc* id, code_t code)
{
    assert(!hasRexPrefix(code) && !hasVexPrefix(code));

    instruction ins  = id->idIns();
    emitAttr    size = id->idOpSize();

    unsigned sz = emitGetAdjustedSize(ins, size, code);

    if (!TakesVexPrefix(ins) &&
        (TakesRexWPrefix(ins, size) || IsExtendedReg(id->idReg1(), size) || IsExtendedReg(id->idReg2(), size)))
    {
        sz++;
    }

    return sz + emitInsSize(code) + 4;
}

static unsigned emitInsSizeImm(instruction ins, emitAttr attr, int32_t imm)
{
    unsigned immSize = EA_SIZE_IN_BYTES(attr);
    bool     hasImm8 = ((signed char)imm == imm) && (ins != INS_mov) && (ins != INS_test) && !EA_IS_CNS_RELOC(attr);

    // We should never generate BT mem,reg because it has poor performance. BT mem,imm might be useful
    // but it requires special handling of the immediate value (it is always encoded in a byte).
    // Let's not complicate things until this is needed.
    assert(ins != INS_bt);

    if (hasImm8)
    {
        return 1;
    }

    assert(!IsSSEOrAVXInstruction(ins));

    return Min(immSize, 4u);
}

emitter::instrDescDsp* emitter::emitAllocInstrDsp(emitAttr attr)
{
#if EMITTER_STATS
    emitTotalIDescDspCnt++;
#endif
    return AllocInstr<instrDescDsp>(attr);
}

emitter::instrDescCnsDsp* emitter::emitAllocInstrCnsDsp(emitAttr attr)
{
#if EMITTER_STATS
    emitTotalIDescCnsDspCnt++;
#endif
    return AllocInstr<instrDescCnsDsp>(attr);
}

emitter::instrDescAmd* emitter::emitAllocInstrAmd(emitAttr attr)
{
#if EMITTER_STATS
    emitTotalIDescAmdCnt++;
#endif
    return AllocInstr<instrDescAmd>(attr);
}

emitter::instrDescCnsAmd* emitter::emitAllocInstrCnsAmd(emitAttr attr)
{
#if EMITTER_STATS
    emitTotalIDescCnsAmdCnt++;
#endif
    return AllocInstr<instrDescCnsAmd>(attr);
}

emitter::instrDesc* emitter::emitNewInstrCnsDsp(emitAttr size, target_ssize_t cns, int dsp)
{
    if (dsp == 0)
    {
        if (instrDesc::fitsInSmallCns(cns))
        {
            instrDesc* id = emitAllocInstr(size);
            id->idSmallCns(cns);
#if EMITTER_STATS
            emitSmallCnsCnt++;
            if ((cns - ID_MIN_SMALL_CNS) >= (SMALL_CNS_TSZ - 1))
                emitSmallCns[SMALL_CNS_TSZ - 1]++;
            else
                emitSmallCns[cns - ID_MIN_SMALL_CNS]++;
            emitSmallDspCnt++;
#endif
            return id;
        }

        instrDescCns* id = emitAllocInstrCns(size, cns);
#if EMITTER_STATS
        emitLargeCnsCnt++;
        emitSmallDspCnt++;
#endif
        return id;
    }

    if (instrDesc::fitsInSmallCns(cns))
    {
        instrDescDsp* id = emitAllocInstrDsp(size);
        id->idSetIsLargeDsp();
        id->iddDspVal = dsp;
        id->idSmallCns(cns);
#if EMITTER_STATS
        emitLargeDspCnt++;
        emitSmallCnsCnt++;
        if ((cns - ID_MIN_SMALL_CNS) >= (SMALL_CNS_TSZ - 1))
            emitSmallCns[SMALL_CNS_TSZ - 1]++;
        else
            emitSmallCns[cns - ID_MIN_SMALL_CNS]++;
#endif
        return id;
    }

    instrDescCnsDsp* id = emitAllocInstrCnsDsp(size);
    id->idSetIsLargeCns();
    id->iddcCnsVal = cns;
    id->idSetIsLargeDsp();
    id->iddcDspVal = dsp;
#if EMITTER_STATS
    emitLargeDspCnt++;
    emitLargeCnsCnt++;
#endif
    return id;
}

emitter::instrDesc* emitter::emitNewInstrDsp(emitAttr attr, target_ssize_t dsp)
{
    if (dsp == 0)
    {
        instrDesc* id = emitAllocInstr(attr);
#if EMITTER_STATS
        emitSmallDspCnt++;
#endif
        return id;
    }

    instrDescDsp* id = emitAllocInstrDsp(attr);
    id->idSetIsLargeDsp();
    id->iddDspVal = dsp;
#if EMITTER_STATS
    emitLargeDspCnt++;
#endif
    return id;
}

emitter::instrDesc* emitter::emitNewInstrAmd(emitAttr size, ssize_t dsp)
{
    if ((dsp < AM_DISP_MIN) || (dsp > AM_DISP_MAX))
    {
        instrDescAmd* id = emitAllocInstrAmd(size);
        id->idSetIsLargeDsp();
        INDEBUG(id->idAddr()->iiaAddrMode.amDisp = AM_DISP_BIG_VAL);
        id->idaAmdVal = dsp;
        return id;
    }

    instrDesc* id                    = emitAllocInstr(size);
    id->idAddr()->iiaAddrMode.amDisp = dsp;
    assert(id->idAddr()->iiaAddrMode.amDisp == dsp); // make sure the value fit
    return id;
}

void emitter::emitSetAmdDisp(instrDescAmd* id, ssize_t dsp)
{
    if (dsp < AM_DISP_MIN || dsp > AM_DISP_MAX)
    {
        id->idSetIsLargeDsp();
#ifdef DEBUG
        id->idAddr()->iiaAddrMode.amDisp = AM_DISP_BIG_VAL;
#endif
        id->idaAmdVal = dsp;
    }
    else
    {
        id->idSetIsSmallDsp();
        id->idAddr()->iiaAddrMode.amDisp = dsp;
        assert(id->idAddr()->iiaAddrMode.amDisp == dsp); // make sure the value fit
    }
}

emitter::instrDesc* emitter::emitNewInstrAmdCns(emitAttr size, ssize_t dsp, int32_t cns)
{
    if (dsp >= AM_DISP_MIN && dsp <= AM_DISP_MAX)
    {
        instrDesc* id                    = emitNewInstrCns(size, cns);
        id->idAddr()->iiaAddrMode.amDisp = dsp;
        assert(id->idAddr()->iiaAddrMode.amDisp == dsp); // make sure the value fit

        return id;
    }
    else
    {
        if (instrDesc::fitsInSmallCns(cns))
        {
            instrDescAmd* id = emitAllocInstrAmd(size);

            id->idSetIsLargeDsp();
#ifdef DEBUG
            id->idAddr()->iiaAddrMode.amDisp = AM_DISP_BIG_VAL;
#endif
            id->idaAmdVal = dsp;

            id->idSmallCns(cns);

            return id;
        }
        else
        {
            instrDescCnsAmd* id = emitAllocInstrCnsAmd(size);

            id->idSetIsLargeCns();
            id->idacCnsVal = cns;

            id->idSetIsLargeDsp();
#ifdef DEBUG
            id->idAddr()->iiaAddrMode.amDisp = AM_DISP_BIG_VAL;
#endif
            id->idacAmdVal = dsp;

            return id;
        }
    }
}

void emitter::emitLoopAlign(uint16_t paddingBytes)
{
    assert(paddingBytes <= MAX_ENCODED_SIZE);
    paddingBytes = min(paddingBytes, MAX_ENCODED_SIZE); // We may need to skip up to 15 bytes of code

    instrDescAlign* id = emitNewInstrAlign();
    id->idCodeSize(paddingBytes);
    id->idaIG   = emitCurIG;
    id->idaNext = emitCurIGAlignList;

    emitCurIGsize += paddingBytes;
    emitCurIGAlignList = id;
}

void emitter::emitLongLoopAlign(uint16_t alignmentBoundary)
{
    unsigned short nPaddingBytes    = alignmentBoundary - 1;
    unsigned short nAlignInstr      = (nPaddingBytes + (MAX_ENCODED_SIZE - 1)) / MAX_ENCODED_SIZE;
    unsigned short instrDescSize    = nAlignInstr * sizeof(instrDescAlign);
    unsigned short insAlignCount    = nPaddingBytes / MAX_ENCODED_SIZE;
    unsigned short lastInsAlignSize = nPaddingBytes % MAX_ENCODED_SIZE;

    // Ensure that all align instructions fall in same IG.
    if (emitCurIGfreeNext + instrDescSize >= emitCurIGfreeEndp)
    {
        emitForceNewIG = true;
    }

    while (insAlignCount)
    {
        emitLoopAlign(MAX_ENCODED_SIZE);
        insAlignCount--;
    }
    emitLoopAlign(lastInsAlignSize);
}

void emitter::emitIns_Nop(unsigned size)
{
    assert(size <= MAX_ENCODED_SIZE);

    instrDesc* id = emitNewInstr();
    id->idIns(INS_nop);
    id->idInsFmt(IF_NONE);
    id->idCodeSize(size);

    dispIns(id);
    emitCurIGsize += size;
}

void emitter::emitIns_Lock()
{
    instrDesc* id = emitNewInstr();
    id->idIns(INS_lock);
    id->idInsFmt(IF_NONE);
    id->idCodeSize(1);

    dispIns(id);
    emitCurIGsize++;

#ifdef PSEUDORANDOM_NOP_INSERTION
    if (emitNextNop == 0)
    {
        emitNextNop = 1;
    }
#endif
}

void emitter::emitIns(instruction ins)
{
    assert((ins == INS_int3) || (ins == INS_leave) || (ins == INS_nop) || (ins == INS_ret) || (ins == INS_vzeroupper) ||
           (ins == INS_lfence) || (ins == INS_mfence) || (ins == INS_sfence));

    code_t code = insCodeMR(ins);
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

    // TODO-MIKE-Cleanup: Bozos thought that lfence & co. have VEX.
    if (UseVEXEncoding() && ((ins == INS_lfence) || (ins == INS_mfence) || (ins == INS_sfence)))
    {
        sz++;
    }

    // vzeroupper includes its 2-byte VEX prefix in its MR code.
    assert((ins != INS_vzeroupper) || (sz == 3));

    instrDesc* id = emitNewInstr();
    id->idIns(ins);
    id->idInsFmt(IF_NONE);
    id->idCodeSize(sz);

    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns(instruction ins, emitAttr attr)
{
    assert((ins == INS_cdq) || (ins == INS_movs) || (ins == INS_stos) || (ins == INS_rep_movs) ||
           (ins == INS_rep_stos));
    assert((attr == EA_1BYTE) || (attr == EA_4BYTE)AMD64_ONLY(|| (attr == EA_8BYTE)));

    size_t code = insCodeMR(ins);
    assert((code >> 16) == 0);

    unsigned sz = 1 + ((code & 0xFF00) != 0)AMD64_ONLY(+(attr == EA_8BYTE));

    instrDesc* id = emitNewInstr(attr);
    id->idIns(ins);
    id->idInsFmt(IF_NONE);
    id->idCodeSize(sz);

    dispIns(id);
    emitCurIGsize += sz;
}

// Map the address mode formats ARD, ARW, and AWR to their direct address equivalents.
static insFormat emitMapFmtAtoM(insFormat fmt)
{
    switch (fmt)
    {
        case IF_ARD:
            return IF_MRD;
        case IF_AWR:
            return IF_MWR;
        case IF_ARW:
            return IF_MRW;

        case IF_RRD_ARD:
            return IF_RRD_MRD;
        case IF_RWR_ARD:
            return IF_RWR_MRD;
        case IF_RWR_ARD_CNS:
            return IF_RWR_MRD_CNS;
        case IF_RRW_ARD:
            return IF_RRW_MRD;
        case IF_RRW_ARD_CNS:
            return IF_RRW_MRD_CNS;
        case IF_RWR_RRD_ARD:
            return IF_RWR_RRD_MRD;
        case IF_RWR_RRD_ARD_CNS:
            return IF_RWR_RRD_MRD_CNS;
        case IF_RWR_RRD_ARD_RRD:
            return IF_RWR_RRD_MRD_RRD;

        case IF_ARD_RRD:
            return IF_MRD_RRD;
        case IF_AWR_RRD:
            return IF_MWR_RRD;
        case IF_ARW_RRD:
            return IF_MRW_RRD;

        case IF_ARD_CNS:
            return IF_MRD_CNS;
        case IF_AWR_CNS:
            return IF_MWR_CNS;
        case IF_ARW_CNS:
            return IF_MRW_CNS;

        case IF_AWR_RRD_CNS:
            return IF_MWR_RRD_CNS;

        default:
            unreached();
    }
}

ssize_t emitter::GetAddrModeDisp(GenTree* addr)
{
    if (addr->isContained())
    {
        if (GenTreeAddrMode* addrMode = addr->IsAddrMode())
        {
            return addrMode->GetOffset();
        }

        if (GenTreeIntCon* intConAddr = addr->IsIntCon())
        {
            return intConAddr->GetValue();
        }
    }

    return 0;
}

void emitter::SetInstrLclAddrMode(instrDesc* id, int varNum, int varOffs)
{
    id->SetVarAddr(varNum, varOffs);

    bool ebpBased;
    int  offset = emitComp->lvaFrameAddress(varNum, &ebpBased) + varOffs;

    id->idAddr()->lclOffset  = offset;
    id->idAddr()->isEbpBased = ebpBased;

    if ((id->idGCref() != GCT_NONE) && (id->idInsFmt() == IF_SWR_RRD))
    {
        if (varNum < 0)
        {
            assert(varOffs == 0);
            id->idAddr()->isTrackedGCSlotStore = codeGen->spillTemps.TrackGCSpillTemps();
        }
#if FEATURE_FIXED_OUT_ARGS
        else if (static_cast<unsigned>(varNum) == emitComp->lvaOutgoingArgSpaceVar)
        {
            id->idAddr()->isGCArgStore = true;
        }
#endif
        else if ((varOffs == 0) && (emitComp->lvaGetDesc(static_cast<unsigned>(varNum))->HasGCSlotLiveness()))
        {
            id->idAddr()->isTrackedGCSlotStore = true;
        }
    }
}

void emitter::SetInstrAddrMode(instrDesc* id, insFormat fmt, instruction ins, GenTree* addr)
{
    assert(fmt != IF_NONE);

    if (!addr->isContained())
    {
        id->idInsFmt(fmt);
        id->idAddr()->iiaAddrMode.amBaseReg = addr->GetRegNum();
        id->idAddr()->iiaAddrMode.amIndxReg = REG_NA;
        id->idAddr()->iiaAddrMode.amScale   = 0;
        assert(emitGetInsAmdDisp(id) == 0);

        return;
    }

    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        CORINFO_FIELD_HANDLE field = clsAddr->GetFieldHandle();
        assert(FieldDispRequiresRelocation(field));

        id->idInsFmt(emitMapFmtAtoM(fmt));
        id->idAddr()->iiaFieldHnd = field;
        id->idSetIsDspReloc();

        return;
    }

    if (GenTreeIntCon* intConAddr = addr->IsIntCon())
    {
        // Absolute addresses marked as contained should fit within the base of addr mode.
        assert(intConAddr->FitsInAddrBase(emitComp));

        if (intConAddr->AddrNeedsReloc(emitComp))
        {
            id->idSetIsDspReloc();
        }

        id->idAddr()->iiaAddrMode.amBaseReg = REG_NA;
        id->idAddr()->iiaAddrMode.amIndxReg = REG_NA;
        id->idAddr()->iiaAddrMode.amScale   = 0;

        id->idInsFmt(fmt);

        // Absolute address must have already been set by the caller.
        assert(emitGetInsAmdDisp(id) == intConAddr->GetValue());

        return;
    }

    GenTreeAddrMode* addrMode = addr->AsAddrMode();

    id->idInsFmt(fmt);

    if (GenTree* base = addrMode->GetBase())
    {
        regNumber baseReg = base->GetRegNum();
        assert(baseReg != REG_NA);
        id->idAddr()->iiaAddrMode.amBaseReg = baseReg;
    }
    else
    {
        id->idAddr()->iiaAddrMode.amBaseReg = REG_NA;
    }

    if (GenTree* index = addrMode->GetIndex())
    {
        regNumber indexReg = index->GetRegNum();
        assert(indexReg != REG_NA);
        id->idAddr()->iiaAddrMode.amIndxReg = indexReg;
        id->idAddr()->iiaAddrMode.amScale   = ScaleEncoding(addrMode->GetScale());
    }
    else
    {
        id->idAddr()->iiaAddrMode.amIndxReg = REG_NA;
        id->idAddr()->iiaAddrMode.amScale   = 0;
    }

    // disp must have already been set by the caller.
    assert(emitGetInsAmdDisp(id) == addrMode->GetOffset());
}

// Takes care of storing all incoming register parameters into
// its corresponding shadow space (defined by the win-x64 ABI).
void emitter::PrologSpillParamRegsToShadowSlots()
{
    assert(codeGen->generatingProlog);

    for (unsigned argNum = 0; argNum < MAX_REG_ARG; ++argNum)
    {
        regNumber argReg = intArgRegs[argNum];

        // The offsets for the shadow space start at RSP + 8
        // (right before the caller return address)
        int offset = (argNum + 1) * EA_PTRSIZE;

        instrDesc* id = emitNewInstrAmd(EA_PTRSIZE, offset);
        id->idIns(INS_mov);
        id->idInsFmt(IF_AWR_RRD);
        id->idAddr()->iiaAddrMode.amBaseReg = REG_SPBASE;
        id->idAddr()->iiaAddrMode.amIndxReg = REG_NA;
        id->idAddr()->iiaAddrMode.amScale   = 0;
        id->idReg1(argReg);

        unsigned sz = emitInsSizeAM(id, insCodeMR(INS_mov));
        id->idCodeSize(sz);
        emitCurIGsize += sz;
    }
}

void emitter::emitIns_A(instruction ins, emitAttr attr, GenTree* addr)
{
    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        emitIns_C(ins, attr, clsAddr->GetFieldHandle());
        return;
    }

    // TODO-MIKE-Cleanup: IND with GT_LCL_ADDR address is nonsense.

    if (addr->OperIs(GT_LCL_ADDR))
    {
        GenTreeLclAddr* lclAddr = addr->AsLclAddr();
        assert(emitComp->lvaGetDesc(lclAddr)->IsAddressExposed());

        emitIns_S(ins, attr, lclAddr->GetLclNum(), lclAddr->GetLclOffs());

        return;
    }

    instrDesc* id = emitNewInstrAmd(attr, GetAddrModeDisp(addr));
    id->idIns(ins);
    SetInstrAddrMode(id, emitInsModeFormat(ins, IF_ARD), ins, addr);

    unsigned sz = emitInsSizeAM(id, insCodeMR(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
#if !FEATURE_FIXED_OUT_ARGS
    emitAdjustStackDepthPushPop(ins);
#endif
}

void emitter::emitIns_A_I(instruction ins, emitAttr attr, GenTree* addr, int32_t imm)
{
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));

    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        emitIns_C_I(ins, attr, clsAddr->GetFieldHandle(), imm);
        return;
    }

    if (addr->OperIs(GT_LCL_ADDR))
    {
        GenTreeLclAddr* lclAddr = addr->AsLclAddr();
        assert(emitComp->lvaGetDesc(lclAddr)->IsAddressExposed());
        emitIns_S_I(ins, attr, lclAddr->GetLclNum(), lclAddr->GetLclOffs(), imm);
        return;
    }

    instrDesc* id = emitNewInstrAmdCns(attr, GetAddrModeDisp(addr), imm);
    id->idIns(ins);
    SetInstrAddrMode(id, emitInsModeFormat(ins, IF_ARD_CNS), ins, addr);

    unsigned sz = emitInsSizeAM(id, insCodeMI(ins)) + emitInsSizeImm(ins, attr, imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_A_R(instruction ins, emitAttr attr, GenTree* addr, regNumber reg)
{
    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        emitIns_C_R(ins, attr, clsAddr->GetFieldHandle(), reg);
        return;
    }

    if (addr->OperIs(GT_LCL_ADDR))
    {
        GenTreeLclAddr* lclAddr = addr->AsLclAddr();
        assert(emitComp->lvaGetDesc(lclAddr)->IsAddressExposed());
        emitIns_S_R(ins, attr, reg, lclAddr->GetLclNum(), lclAddr->GetLclOffs());
        return;
    }

    instrDesc* id = emitNewInstrAmd(attr, GetAddrModeDisp(addr));
    id->idIns(ins);
    id->idReg1(reg);
    SetInstrAddrMode(id, emitInsModeFormat(ins, IF_ARD_RRD), ins, addr);

    unsigned sz = emitInsSizeAM(id, insCodeMR(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitInsRMW_A(instruction ins, emitAttr attr, GenTree* addr)
{
    instrDesc* id = emitNewInstrAmd(attr, GetAddrModeDisp(addr));
    SetInstrAddrMode(id, IF_ARW, ins, addr);
    id->idIns(ins);

    unsigned sz = emitInsSizeAM(id, insCodeMR(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitInsRMW_A_I(instruction ins, emitAttr attr, GenTree* addr, int32_t imm)
{
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));

    if (IsShiftImm(ins))
    {
        imm &= 0x7F;
    }

    instrDesc* id = emitNewInstrAmdCns(attr, GetAddrModeDisp(addr), imm);
    SetInstrAddrMode(id, IF_ARW_CNS, ins, addr);
    id->idIns(ins);

    unsigned sz = emitInsSizeAM(id, insCodeMI(ins)) + emitInsSizeImm(ins, attr, imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitInsRMW_A_R(instruction ins, emitAttr attr, GenTree* addr, regNumber reg)
{
    instrDesc* id = emitNewInstrAmd(attr, GetAddrModeDisp(addr));
    SetInstrAddrMode(id, IF_ARW_RRD, ins, addr);
    id->idReg1(reg);
    id->idIns(ins);

    unsigned sz = emitInsSizeAM(id, insCodeMR(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R(instruction ins, emitAttr attr, regNumber reg)
{
    emitAttr size = EA_SIZE(attr);

    assert(size <= EA_PTRSIZE);
    noway_assert(emitVerifyEncodable(ins, size, reg));

    instrDesc* id = emitNewInstrSmall(attr);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_RRD));
    id->idReg1(reg);

    unsigned sz = 2;

    if ((ins == INS_push) || (ins == INS_push_hide) || (ins == INS_pop) || (ins == INS_pop_hide))
    {
        assert(size == EA_PTRSIZE);
        sz = 1;
    }
#ifdef TARGET_X86
    else if (((ins == INS_inc) || (ins == INS_dec)) && (size != EA_1BYTE))
    {
        sz = 1;
    }
#endif
    else if ((INS_seto <= ins) && (ins <= INS_setg))
    {
        static_assert_no_msg(INS_seto + 0xF == INS_setg);
        assert(attr == EA_1BYTE);
        assert(insEncodeRMreg(ins, reg, attr, insCodeMR(ins)) & 0x00FF0000);

        sz = 3;
    }

    sz += emitGetAdjustedSize(ins, attr, insEncodeRMreg(ins, reg, attr, insCodeMR(ins)));

    if (!TakesVexPrefix(ins) && (IsExtendedReg(reg, attr) || TakesRexWPrefix(ins, attr)))
    {
        sz++;
    }

    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
#if !FEATURE_FIXED_OUT_ARGS
    emitAdjustStackDepthPushPop(ins);
#endif
}

void emitter::emitIns_R_H(instruction ins, regNumber reg, void* addr DEBUGARG(HandleKind handleKind))
{
    assert(ins == INS_mov);
    assert(genIsValidIntReg(reg) && (reg != REG_RSP));

#ifdef TARGET_AMD64
    // Because it has to be relocatable this has to be a "mov reg, imm64", we can't narrow it
    // down to imm32. And since it's always a 64 bit operation it always has a REX prefix.
    unsigned size = 10;
#else
    unsigned size = 5;
#endif

    instrDesc* id = emitNewInstrSC(EA_PTR_CNS_RELOC, reinterpret_cast<ssize_t>(addr));
    id->idIns(ins);
    id->idInsFmt(IF_RWR_CNS);
    id->idReg1(reg);
    id->idCodeSize(size);
    INDEBUG(id->idDebugOnlyInfo()->idHandleKind = handleKind);

    dispIns(id);
    emitCurIGsize += size;
}

void emitter::emitIns_R_I(instruction ins, emitAttr attr, regNumber reg, ssize_t imm DEBUGARG(HandleKind handleKind))
{
    // BT reg,imm might be useful but it requires special handling of the immediate value
    // (it is always encoded in a byte). Let's not complicate things until this is needed.
    assert(ins != INS_bt);
    assert(!EA_IS_RELOC(attr));

    emitAttr size = EA_SIZE(attr);
    // Allow emitting SSE2/AVX SIMD instructions of R_I form that can specify EA_16BYTE or EA_32BYTE
    assert((size <= EA_PTRSIZE) || IsSSEOrAVXInstruction(ins));
    noway_assert(emitVerifyEncodable(ins, size, reg));
    // mov reg, imm64 is the only opcode which takes a full 8 byte immediate
    // all other opcodes take a sign-extended 4-byte immediate
    AMD64_ONLY(noway_assert((size < EA_8BYTE) || (ins == INS_mov) || IsImm32(imm)));

    bool     hasImm8 = IsImm8(imm) && (ins != INS_mov) && (ins != INS_test);
    unsigned sz;

    if (IsShiftImm(ins))
    {
        assert(imm != 1);
        sz = 3;
        imm &= 0x7F;
        hasImm8 = true;
    }
    else if (ins == INS_mov)
    {
#ifdef TARGET_AMD64
        // mov reg, imm64 is equivalent to mov reg, imm32 if the high order bits are all 0
        // and this isn't a reloc constant.
        // TODO-MIKE-Review: This doesn't check for relocs as the comment claims, the reloc
        // bit was stripped by EA_SIZE above.
        if ((size == EA_8BYTE) && (0 <= imm) && (imm <= UINT_MAX))
        {
            attr = EA_4BYTE;
            size = EA_4BYTE;
            sz   = 5;
        }
        else if (size > EA_4BYTE)
        {
            sz = 9; // Really it is 10, but we'll add the REX prefix later.
        }
        else
#endif
        {
            sz = 5;
        }
    }
    else if (hasImm8)
    {
        if (IsSSEOrAVXInstruction(ins))
        {
            sz = emitInsSize(insCodeMI(ins)) + 1;
        }
        else
        {
            sz = (reg == REG_EAX) && (size == EA_1BYTE) ? 2 : 3;
        }
    }
    else
    {
        assert(!IsSSEOrAVXInstruction(ins));

        sz = (reg == REG_EAX) ? 1 : 2;

#ifdef TARGET_AMD64
        if (size >= EA_4BYTE)
        {
            sz += 4;
        }
        else
#endif
        {
            sz += EA_SIZE_IN_BYTES(attr);
        }
    }

    sz += emitGetAdjustedSize(ins, attr, insCodeMI(ins));

    if (!TakesVexPrefix(ins) && (IsExtendedReg(reg, attr) || TakesRexWPrefix(ins, size)))
    {
        sz++;
    }

    instrDesc* id = emitNewInstrSC(attr, imm);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_RRD_CNS));
    id->idReg1(reg);
    id->idCodeSize(sz);
    INDEBUG(id->idDebugOnlyInfo()->idHandleKind = handleKind);

    dispIns(id);
    emitCurIGsize += sz;

#if !FEATURE_FIXED_OUT_ARGS
    if (reg == REG_ESP)
    {
        emitAdjustStackDepth(ins, imm);
    }
#endif
}

#ifdef TARGET_X86
void emitter::emitIns_H(instruction ins, void* addr)
{
    assert((ins == INS_push) || (ins == INS_push_hide));

    instrDesc* id = emitNewInstrSC(EA_PTR_CNS_RELOC, reinterpret_cast<ssize_t>(addr));
    id->idIns(ins);
    id->idInsFmt(IF_CNS);
    id->idCodeSize(5);

    dispIns(id);
    emitCurIGsize += 5;
#if !FEATURE_FIXED_OUT_ARGS
    emitAdjustStackDepthPushPop(ins);
#endif
}
#endif

#ifdef WINDOWS_X86_ABI
void emitter::emitInsMov_R_FS(regNumber reg, int offs)
{
    assert(genIsValidIntReg(reg));

    instrDesc* id = emitNewInstrDsp(EA_4BYTE, offs);
    id->idIns(INS_mov);
    id->idInsFmt(emitInsModeFormat(INS_mov, IF_RRD_MRD));
    id->idReg1(reg);
    id->idAddr()->iiaFieldHnd = FS_SEG_FIELD;

    unsigned sz = 1 + (reg == REG_EAX ? 1 : 2) + 4;
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}
#endif // WINDOWS_X86_ABI

void emitter::emitIns_I(instruction ins, emitAttr attr, int32_t imm)
{
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

    instrDesc* id = emitNewInstrSC(attr, imm);
    id->idIns(ins);
    id->idInsFmt(IF_CNS);
    id->idCodeSize(sz);

    dispIns(id);
    emitCurIGsize += sz;
#if !FEATURE_FIXED_OUT_ARGS
    emitAdjustStackDepthPushPop(ins);
#endif
}

void emitter::emitIns_C(instruction ins, emitAttr attr, CORINFO_FIELD_HANDLE field)
{
    assert(FieldDispRequiresRelocation(field));

    instrDesc* id = emitNewInstrDsp(attr, 0);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_MRD));
    id->idAddr()->iiaFieldHnd = field;
    id->idSetIsDspReloc();

    unsigned sz = emitInsSizeCV(id, insCodeMR(ins));

    if (!TakesVexPrefix(ins) && TakesRexWPrefix(ins, attr))
    {
        sz++;
    }

    id->idCodeSize(sz);

    dispIns(id);
    emitCurIGsize += sz;
#if !FEATURE_FIXED_OUT_ARGS
    emitAdjustStackDepthPushPop(ins);
#endif
}

bool emitter::IsMovInstruction(instruction ins)
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
        {
            return true;
        }

#if defined(TARGET_AMD64)
        case INS_movq:
        case INS_movsxd:
        {
            return true;
        }
#endif // TARGET_AMD64

        default:
        {
            return false;
        }
    }
}

bool emitter::IsJccInstruction(instruction ins)
{
    return (ins >= INS_jo) && (ins <= INS_jg);
}

bool emitter::IsJmpInstruction(instruction ins)
{
    return
#ifdef TARGET_AMD64
        (ins == INS_rex_jmp) ||
#endif
        (ins == INS_i_jmp) || (ins == INS_jmp) || (ins == INS_l_jmp);
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
bool emitter::IsRedundantMov(
    instruction ins, insFormat fmt, emitAttr size, regNumber dst, regNumber src, bool canIgnoreSideEffects)
{
    assert(IsMovInstruction(ins));

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

    if (!emitComp->opts.OptimizationEnabled())
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
        {
            // non EA_PTRSIZE moves may zero-extend the source
            hasSideEffect = (size != EA_PTRSIZE);
            break;
        }

        case INS_movapd:
        case INS_movaps:
        case INS_movdqa:
        case INS_movdqu:
        case INS_movupd:
        case INS_movups:
        {
            // non EA_32BYTE moves clear the upper bits under VEX encoding
            hasSideEffect = UseVEXEncoding() && (size != EA_32BYTE);
            break;
        }

        case INS_movd:
        {
            // Clears the upper bits
            hasSideEffect = true;
            break;
        }

        case INS_movsd:
        case INS_movss:
        {
            // Clears the upper bits under VEX encoding
            hasSideEffect = UseVEXEncoding();
            break;
        }

        case INS_movsx:
        case INS_movzx:
        {
            // Sign/Zero-extends the source
            hasSideEffect = true;
            break;
        }

#if defined(TARGET_AMD64)
        case INS_movq:
        {
            // Clears the upper bits
            hasSideEffect = true;
            break;
        }

        case INS_movsxd:
        {
            // Sign-extends the source
            hasSideEffect = true;
            break;
        }
#endif // TARGET_AMD64

        default:
        {
            unreached();
        }
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
        (lastIns->idInsFmt() != fmt))
    {
        return false;
    }

    regNumber lastDst = lastIns->idReg1();
    regNumber lastSrc = lastIns->idReg2();

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

void emitter::emitIns_Mov(instruction ins, emitAttr attr, regNumber dstReg, regNumber srcReg, bool canSkip)
{
    // Only move instructions can use emitIns_Mov
    assert(IsMovInstruction(ins));

    if (EA_IS_GCREF_OR_BYREF(attr) && (ins == INS_mov) && (dstReg == srcReg))
    {
        emitNewInstrGCReg(attr, dstReg);

        return;
    }

#if DEBUG
    switch (ins)
    {
        case INS_mov:
        case INS_movsx:
        case INS_movzx:
            assert(IsGeneralRegister(dstReg) && IsGeneralRegister(srcReg));
            break;
        case INS_movapd:
        case INS_movaps:
        case INS_movdqa:
        case INS_movdqu:
        case INS_movsd:
        case INS_movss:
        case INS_movupd:
        case INS_movups:
            assert(IsFloatReg(dstReg) && IsFloatReg(srcReg));
            break;
        case INS_movd:
            assert(IsFloatReg(dstReg) != IsFloatReg(srcReg));
            break;
#ifdef TARGET_AMD64
        case INS_movq:
            assert(IsFloatReg(dstReg) && IsFloatReg(srcReg));
            break;
        case INS_movsxd:
            assert(IsGeneralRegister(dstReg) && IsGeneralRegister(srcReg));
            break;
#endif
        default:
            unreached();
    }
#endif

    noway_assert(emitVerifyEncodable(ins, EA_SIZE(attr), dstReg, srcReg));

    insFormat fmt = emitInsModeFormat(ins, IF_RRD_RRD);

    if (IsRedundantMov(ins, fmt, attr, dstReg, srcReg, canSkip))
    {
        return;
    }

    instrDesc* id = emitNewInstrSmall(attr);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idReg1(dstReg);
    id->idReg2(srcReg);

    unsigned sz = emitInsSizeRR(ins, dstReg, srcReg, attr);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2)
{
    assert(!instrHasImplicitRegPairDest(ins) && (ins != INS_imuli));

    if (IsMovInstruction(ins))
    {
        assert(!"Please use emitIns_Mov() to correctly handle move elision");
        emitIns_Mov(ins, attr, reg1, reg2, /* canSkip */ false);
    }

    noway_assert(emitVerifyEncodable(ins, EA_SIZE(attr), reg1, reg2));

    instrDesc* id = emitNewInstrSmall(attr);
    id->idIns(ins);
    id->idInsFmt((ins == INS_xchg) ? IF_RRW_RRW : emitInsModeFormat(ins, IF_RRD_RRD));
    id->idReg1(reg1);
    id->idReg2(reg2);

    unsigned sz = emitInsSizeRR(ins, reg1, reg2, attr);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_I(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int32_t imm)
{
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));

    instrDesc* id = emitNewInstrSC(attr, imm);
    id->idIns(ins);
    id->idInsFmt(IF_RRW_RRD_CNS);
    id->idReg1(reg1);
    id->idReg2(reg2);

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
            code = insCodeMR(ins);
            break;
        case INS_psrldq:
        case INS_pslldq:
            code = insCodeMI(ins);
            break;
        default:
            code = insCodeRM(ins);
            break;
    }

    unsigned sz = emitInsSizeRR(id, code) + emitInsSizeImm(ins, attr, imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_AR(instruction ins, emitAttr attr, regNumber base, int32_t disp)
{
    assert(IsPrefetch(ins));

    instrDesc* id = emitNewInstrAmd(attr, disp);
    id->idIns(ins);
    id->idInsFmt(IF_ARD);
    id->idAddr()->iiaAddrMode.amBaseReg = base;
    id->idAddr()->iiaAddrMode.amIndxReg = REG_NA;

    unsigned sz = emitInsSizeAM(id, insCodeMR(ins));

    // TODO-MIKE-Cleanup: Bozos thought that lfence & co. have VEX.
    if (UseVEXEncoding())
    {
        sz += 2;
    }

    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_AR_R_R(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber base, int32_t disp)
{
    assert(IsVexTernary(ins));

    instrDesc* id = emitNewInstrAmd(attr, disp);
    id->idIns(ins);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idInsFmt(IF_AWR_RRD_RRD);
    id->idAddr()->iiaAddrMode.amBaseReg = base;
    id->idAddr()->iiaAddrMode.amIndxReg = REG_NA;

    unsigned sz = emitInsSizeAM(id, insCodeMR(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

// TODO-MIKE-Cleanup: Reconcile this with emitIns_R_A, this one always uses IF_RRW_ARD
// instead of using emitInsModeFormat. It also doesn't handle local addresses but that
// is unlikely to be an issue.
void emitter::emitIns_RRW_A(instruction ins, emitAttr attr, regNumber reg1, GenTree* addr)
{
    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        emitIns_R_C(ins, attr, reg1, clsAddr->GetFieldHandle());
        return;
    }

    instrDesc* id = emitNewInstrAmd(attr, GetAddrModeDisp(addr));
    id->idIns(ins);
    id->idReg1(reg1);
    SetInstrAddrMode(id, IF_RRW_ARD, ins, addr);

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_A(instruction ins, emitAttr attr, regNumber reg, GenTree* addr)
{
    assert(!instrHasImplicitRegPairDest(ins) && (ins != INS_imuli));

    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        emitIns_R_C(ins, attr, reg, clsAddr->GetFieldHandle());
        return;
    }

    if (addr->OperIs(GT_LCL_ADDR))
    {
        GenTreeLclAddr* lclAddr = addr->AsLclAddr();
        assert(emitComp->lvaGetDesc(lclAddr)->IsAddressExposed());
        emitIns_R_S(ins, attr, reg, lclAddr->GetLclNum(), lclAddr->GetLclOffs());
        return;
    }

    instrDesc* id = emitNewInstrAmd(attr, GetAddrModeDisp(addr));
    id->idIns(ins);
    id->idReg1(reg);
    SetInstrAddrMode(id, emitInsModeFormat(ins, IF_RRD_ARD), ins, addr);

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_A_I(instruction ins, emitAttr attr, regNumber reg1, GenTree* addr, int32_t imm)
{
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));

    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        emitIns_R_C_I(ins, attr, reg1, clsAddr->GetFieldHandle(), imm);
        return;
    }

    noway_assert(emitVerifyEncodable(ins, EA_SIZE(attr), reg1));
    assert(IsSSEOrAVXInstruction(ins) || (ins == INS_imuli));

    instrDesc* id = emitNewInstrAmdCns(attr, GetAddrModeDisp(addr), imm);
    id->idIns(ins);
    id->idReg1(reg1);
    SetInstrAddrMode(id, IF_RRW_ARD_CNS, ins, addr);

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins)) + emitInsSizeImm(ins, attr, imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_C_I(instruction ins, emitAttr attr, regNumber reg1, CORINFO_FIELD_HANDLE field, int32_t imm)
{
    assert(IsSSEOrAVXInstruction(ins) || (ins == INS_imuli));
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));
    noway_assert(emitVerifyEncodable(ins, EA_SIZE(attr), reg1));
    assert(FieldDispRequiresRelocation(field));

    instrDesc* id = emitNewInstrCnsDsp(attr, imm, 0);
    id->idIns(ins);
    id->idInsFmt(IF_RRW_MRD_CNS);
    id->idReg1(reg1);
    id->idAddr()->iiaFieldHnd = field;
    id->idSetIsDspReloc();

    unsigned sz = emitInsSizeCV(id, insCodeRM(ins)) + emitInsSizeImm(ins, attr, imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_S_I(instruction ins, emitAttr attr, regNumber reg1, int varx, int offs, int32_t imm)
{
    noway_assert(emitVerifyEncodable(ins, EA_SIZE(attr), reg1));
    assert(IsSSEOrAVXInstruction(ins) || (ins == INS_imuli));
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));

    instrDesc* id = emitNewInstrCns(attr, imm);
    id->idIns(ins);
    id->idInsFmt(IF_RRW_SRD_CNS);
    id->idReg1(reg1);
    SetInstrLclAddrMode(id, varx, offs);

    unsigned sz = emitInsSizeSV(id, insCodeRM(ins)) + emitInsSizeImm(ins, attr, imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_A(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, GenTree* addr)
{
    assert(IsVexTernary(ins));

    instrDesc* id = emitNewInstrAmd(attr, GetAddrModeDisp(addr));
    id->idIns(ins);
    id->idReg1(reg1);
    id->idReg2(reg2);
    SetInstrAddrMode(id, IF_RWR_RRD_ARD, ins, addr);

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

bool IsAVX2GatherInstruction(instruction ins)
{
    switch (ins)
    {
        case INS_vpgatherdd:
        case INS_vpgatherdq:
        case INS_vpgatherqd:
        case INS_vpgatherqq:
        case INS_vgatherdps:
        case INS_vgatherdpd:
        case INS_vgatherqps:
        case INS_vgatherqpd:
            return true;
        default:
            return false;
    }
}

void emitter::emitIns_R_AR_R(instruction ins,
                             emitAttr    attr,
                             regNumber   reg1,
                             regNumber   reg2,
                             regNumber   base,
                             regNumber   index,
                             int         scale,
                             int32_t     disp)
{
    assert(IsAVX2GatherInstruction(ins));

    instrDesc* id = emitNewInstrAmd(attr, disp);
    id->idIns(ins);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idInsFmt(IF_RWR_ARD_RRD);
    id->idAddr()->iiaAddrMode.amBaseReg = base;
    id->idAddr()->iiaAddrMode.amIndxReg = index;
    id->idAddr()->iiaAddrMode.amScale   = ScaleEncoding(scale);

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_C(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, CORINFO_FIELD_HANDLE field)
{
    assert(IsVexTernary(ins));
    assert(FieldDispRequiresRelocation(field));

    instrDesc* id = emitNewInstrDsp(attr, 0);
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_MRD);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idAddr()->iiaFieldHnd = field;
    id->idSetIsDspReloc();

    unsigned sz = emitInsSizeCV(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3)
{
    assert(IsVexTernary(ins));

    instrDesc* id = emitNewInstr(attr);
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_RRD);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idReg3(reg3);

    unsigned sz = emitInsSizeRR(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_S(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int varx, int offs)
{
    assert(IsVexTernary(ins));

    instrDesc* id = emitNewInstr(attr);
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_SRD);
    id->idReg1(reg1);
    id->idReg2(reg2);
    SetInstrLclAddrMode(id, varx, offs);

    unsigned sz = emitInsSizeSV(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_A_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, GenTree* addr, int32_t imm, insFormat fmt)
{
    assert(IsVexTernary(ins));
    assert(!EA_IS_CNS_RELOC(attr));
    assert(IsImm8(imm));

    instrDesc* id = emitNewInstrAmdCns(attr, GetAddrModeDisp(addr), imm);
    id->idIns(ins);
    id->idReg1(reg1);
    id->idReg2(reg2);

    SetInstrAddrMode(id, fmt, ins, addr);

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins)) + 1;
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_C_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, CORINFO_FIELD_HANDLE field, int32_t imm)
{
    assert(IsVexTernary(ins));
    assert(FieldDispRequiresRelocation(field));
    assert(!EA_IS_CNS_RELOC(attr));
    assert(IsImm8(imm));

    instrDesc* id = emitNewInstrCnsDsp(attr, imm, 0);
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_MRD_CNS);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idAddr()->iiaFieldHnd = field;
    id->idSetIsDspReloc();

    unsigned sz = emitInsSizeCV(id, insCodeRM(ins)) + 1;
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_R_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, int32_t imm)
{
    assert(IsVexTernary(ins));
    assert(!EA_IS_CNS_RELOC(attr));
    assert(IsImm8(imm));

    instrDesc* id = emitNewInstrCns(attr, imm);
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_RRD_CNS);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idReg3(reg3);

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
            code = insCodeMR(ins);
            break;
        case INS_psrldq:
        case INS_pslldq:
            code = insCodeMI(ins);
            break;
        default:
            code = insCodeRM(ins);
            break;
    }

    unsigned sz = emitInsSizeRR(id, code) + 1;
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_S_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int varx, int offs, int32_t imm)
{
    assert(IsVexTernary(ins));
    assert(!EA_IS_CNS_RELOC(attr));
    assert(IsImm8(imm));

    instrDesc* id = emitNewInstrCns(attr, imm);
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_SRD_CNS);
    id->idReg1(reg1);
    id->idReg2(reg2);
    SetInstrLclAddrMode(id, varx, offs);

    unsigned sz = emitInsSizeSV(id, insCodeRM(ins)) + 1;
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

// Encodes a XMM register into imm[7:4] for use by a SIMD instruction.
static int8_t EncodeXmmRegAsImm(regNumber reg)
{
    // AVX/AVX2 supports 4-reg format for vblendvps/vblendvpd/vpblendvb,
    // which encodes the fourth register into imm8[7:4]
    assert(reg >= XMMBASE);

    int imm = (reg - XMMBASE) << 4;
    assert((imm >= 0) && (imm <= 255));
    return static_cast<int8_t>(imm);
}

void emitter::emitIns_R_R_A_R(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, GenTree* addr)
{
    assert(isAvxBlendv(ins));
    assert(UseVEXEncoding());

    instrDesc* id = emitNewInstrAmdCns(attr, GetAddrModeDisp(addr), EncodeXmmRegAsImm(reg3));
    id->idIns(ins);
    id->idReg1(reg1);
    id->idReg2(reg2);

    SetInstrAddrMode(id, IF_RWR_RRD_ARD_RRD, ins, addr);

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins)) + 1;
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_C_R(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, CORINFO_FIELD_HANDLE field)
{
    assert(isAvxBlendv(ins));
    assert(UseVEXEncoding());
    assert(FieldDispRequiresRelocation(field));

    instrDesc* id = emitNewInstrCnsDsp(attr, EncodeXmmRegAsImm(reg3), 0);
    id->idIns(ins);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idInsFmt(IF_RWR_RRD_MRD_RRD);
    id->idAddr()->iiaFieldHnd = field;
    id->idSetIsDspReloc();

    unsigned sz = emitInsSizeCV(id, insCodeRM(ins)) + 1;
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_S_R(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, int varx, int offs)
{
    assert(isAvxBlendv(ins));
    assert(UseVEXEncoding());

    instrDesc* id = emitNewInstrCns(attr, EncodeXmmRegAsImm(reg3));
    id->idIns(ins);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idInsFmt(IF_RWR_RRD_SRD_RRD);
    SetInstrLclAddrMode(id, varx, offs);

    unsigned sz = emitInsSizeSV(id, insCodeRM(ins)) + 1;
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_R_R(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, regNumber reg4)
{
    assert(isAvxBlendv(ins));
    assert(UseVEXEncoding());

    instrDesc* id = emitNewInstrCns(attr, EncodeXmmRegAsImm(reg4));
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_RRD_RRD);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idReg3(reg3);
    id->idReg4(reg4);

    unsigned sz = emitInsSizeRR(id, insCodeRM(ins)) + 1;
    id->idCodeSize(sz);

    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_C(instruction ins, emitAttr attr, regNumber reg, CORINFO_FIELD_HANDLE field)
{
    assert(!instrHasImplicitRegPairDest(ins) && (ins != INS_imuli));
    assert(FieldDispRequiresRelocation(field));
    noway_assert(emitVerifyEncodable(ins, EA_SIZE(attr), reg));

    instrDesc* id = emitNewInstrDsp(attr, 0);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_RRD_MRD));
    id->idReg1(reg);
    id->idAddr()->iiaFieldHnd = field;
    id->idSetIsDspReloc();

    unsigned sz;

#ifdef TARGET_X86
    // Special case: "mov eax, [addr]" is smaller.
    // This case is not enabled for amd64 as it always uses RIP relative addressing
    // and it results in smaller instruction size than encoding 64-bit addr in the
    // instruction.
    if ((ins == INS_mov) && (reg == REG_EAX))
    {
        sz = (EA_SIZE(attr) == EA_2BYTE) + 1 + 4;
    }
    else
#endif
    {
        sz = emitInsSizeCV(id, insCodeRM(ins));
    }

    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_C_R(instruction ins, emitAttr attr, CORINFO_FIELD_HANDLE field, regNumber reg)
{
    assert(FieldDispRequiresRelocation(field));

    emitAttr size = EA_SIZE(attr);

#if defined(TARGET_X86)
    // For x86 it is valid to storeind a double sized operand in an xmm reg to memory
    assert(size <= EA_8BYTE);
#else
    assert(size <= EA_PTRSIZE);
#endif

    noway_assert(emitVerifyEncodable(ins, size, reg));

    instrDesc* id  = emitNewInstrDsp(attr, 0);
    insFormat  fmt = emitInsModeFormat(ins, IF_MRD_RRD);

    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idReg1(reg);

    unsigned sz;

#ifdef TARGET_X86
    // Special case: "mov [addr], EAX" is smaller.
    // This case is not enable for amd64 as it always uses RIP relative addressing
    // and it will result in smaller instruction size than encoding 64-bit addr in
    // the instruction.
    if (ins == INS_mov && reg == REG_EAX)
    {
        sz = 1 + TARGET_POINTER_SIZE;

        if (size == EA_2BYTE)
            sz += 1;

        if (!TakesVexPrefix(ins) && (TakesRexWPrefix(ins, attr) || IsExtendedReg(reg, attr)))
        {
            sz++;
        }
    }
    else
#endif // TARGET_X86
    {
        sz = emitInsSizeCV(id, insCodeMR(ins));
    }

    id->idCodeSize(sz);

    id->idAddr()->iiaFieldHnd = field;
    id->idSetIsDspReloc();

    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_C_I(instruction ins, emitAttr attr, CORINFO_FIELD_HANDLE field, int32_t imm)
{
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));
    assert(FieldDispRequiresRelocation(field));

    if (IsShiftImm(ins))
    {
        assert(imm != 1);
        imm &= 0x7F;
    }

    instrDesc* id = emitNewInstrCnsDsp(attr, imm, 0);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_MRD_CNS));
    id->idAddr()->iiaFieldHnd = field;
    id->idSetIsDspReloc();

    unsigned sz = emitInsSizeCV(id, insCodeMI(ins)) + emitInsSizeImm(ins, attr, imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_L(instruction ins, BasicBlock* dst, regNumber reg)
{
    assert(ins == INS_lea);
    assert((dst->bbFlags & BBF_HAS_LABEL) != 0);

    instrDescJmp* id = emitNewInstrJmp();

    id->idIns(INS_lea);
    id->idReg1(reg);
    id->idInsFmt(IF_RWR_LABEL);
    id->idOpSize(EA_PTRSIZE);
    id->idSetIsDspReloc();
    id->idAddr()->iiaBBlabel = dst;
    INDEBUG(id->idDebugOnlyInfo()->idCatchRet = (GetCurrentBlock()->bbJumpKind == BBJ_EHCATCHRET));

    id->idjKeepLong  = true;
    id->idjIG        = emitCurIG;
    id->idjOffs      = emitCurIGsize;
    id->idjNext      = emitCurIGjmpList;
    emitCurIGjmpList = id;

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_AR_I(instruction ins, emitAttr attr, regNumber base, int32_t disp, int32_t imm)
{
    assert(!instIsFP(ins) && (EA_SIZE(attr) <= EA_8BYTE));
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));

    if (IsShiftImm(ins))
    {
        assert(imm != 1);
        imm &= 0x7F;
    }

    instrDesc* id = emitNewInstrAmdCns(attr, disp, imm);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_ARD_CNS));
    id->idAddr()->iiaAddrMode.amBaseReg = base;
    id->idAddr()->iiaAddrMode.amIndxReg = REG_NA;

    unsigned sz = emitInsSizeAM(id, insCodeMI(ins)) + emitInsSizeImm(ins, attr, imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_AR(instruction ins, emitAttr attr, regNumber reg, regNumber base, int32_t disp)
{
    emitIns_R_ARX(ins, attr, reg, base, REG_NA, 1, disp);
}

void emitter::emitIns_R_AH(instruction ins, regNumber reg, void* addr)
{
    assert((ins == INS_mov) || (ins == INS_lea));
    assert(genIsValidIntReg(reg));
    noway_assert(emitVerifyEncodable(ins, EA_PTRSIZE, reg));

    instrDesc* id = emitNewInstrAmd(EA_PTRSIZE, reinterpret_cast<ssize_t>(addr));
    id->idIns(ins);
    id->idInsFmt(IF_RWR_ARD);
    id->idReg1(reg);
    id->idAddr()->iiaAddrMode.amBaseReg = REG_NA;
    id->idAddr()->iiaAddrMode.amIndxReg = REG_NA;
    // On x64 RIP relative addressing is always used and that needs relocs.
    id->idSetIsDspReloc(X86_ONLY(emitComp->opts.compReloc));

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_AR_R(instruction ins, emitAttr attr, regNumber reg, regNumber base, int32_t disp)
{
    emitIns_ARX_R(ins, attr, reg, base, REG_NA, 1, disp);
}

void emitter::emitIns_S_R_I(instruction ins, emitAttr attr, int varNum, int offs, regNumber reg, int32_t imm)
{
    assert(ins == INS_vextracti128 || ins == INS_vextractf128);
    assert((imm == 0) || (imm == 1));

    instrDesc* id = emitNewInstrAmdCns(attr, 0, imm);
    id->idIns(ins);
    id->idInsFmt(IF_SWR_RRD_CNS);
    id->idReg1(reg);
    SetInstrLclAddrMode(id, varNum, offs);

    unsigned sz = emitInsSizeSV(id, insCodeMR(ins)) + 1;
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_A_R_I(instruction ins, emitAttr attr, GenTree* addr, regNumber reg, int32_t imm)
{
    assert((ins == INS_vextracti128) || (ins == INS_vextractf128));
    assert(attr == EA_32BYTE);
    assert(reg != REG_NA);

    instrDesc* id = emitNewInstrAmdCns(attr, GetAddrModeDisp(addr), imm);
    id->idIns(ins);
    id->idReg1(reg);
    SetInstrAddrMode(id, IF_AWR_RRD_CNS, ins, addr);

    unsigned size = emitInsSizeAM(id, insCodeMR(ins)) + 1;
    id->idCodeSize(size);
    dispIns(id);
    emitCurIGsize += size;
}

void emitter::emitIns_R_ARR(
    instruction ins, emitAttr attr, regNumber reg, regNumber base, regNumber index, int32_t disp)
{
    emitIns_R_ARX(ins, attr, reg, base, index, 1, disp);
}

void emitter::emitIns_ARX_I(
    instruction ins, emitAttr attr, regNumber base, regNumber index, unsigned scale, int32_t disp, int32_t imm)
{
    assert(!instIsFP(ins) && (EA_SIZE(attr) <= EA_8BYTE));
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));

    if (IsShiftImm(ins))
    {
        assert(imm != 1);
        imm &= 0x7F;
    }

    instrDesc* id = emitNewInstrAmdCns(attr, disp, imm);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_ARD_CNS));
    id->idAddr()->iiaAddrMode.amBaseReg = base;
    id->idAddr()->iiaAddrMode.amIndxReg = index;
    id->idAddr()->iiaAddrMode.amScale   = ScaleEncoding(scale);

    unsigned sz = emitInsSizeAM(id, insCodeMI(ins)) + emitInsSizeImm(ins, attr, imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_ARX(
    instruction ins, emitAttr attr, regNumber reg, regNumber base, regNumber index, unsigned scale, int32_t disp)
{
    assert(!instIsFP(ins) && (EA_SIZE(attr) <= EA_32BYTE) && (reg != REG_NA));
    noway_assert(emitVerifyEncodable(ins, EA_SIZE(attr), reg));

    if ((ins == INS_lea) && (reg == base) && (index == REG_NA) && (disp == 0))
    {
        // Maybe the emitter is not the common place for this optimization, but it's a better choke point
        // for all the emitIns(ins, tree), we would have to be analyzing at each call site
        //
        return;
    }

    instrDesc* id = emitNewInstrAmd(attr, disp);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_RRD_ARD));
    id->idReg1(reg);
    id->idAddr()->iiaAddrMode.amBaseReg = base;
    id->idAddr()->iiaAddrMode.amIndxReg = index;
    id->idAddr()->iiaAddrMode.amScale   = ScaleEncoding(scale);

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_ARX(instruction ins, emitAttr attr, regNumber base, regNumber index, unsigned scale, int32_t disp)
{
    emitIns_ARX_R(ins, attr, REG_NA, base, index, scale, disp);
}

void emitter::emitIns_ARX_R(
    instruction ins, emitAttr attr, regNumber reg, regNumber base, regNumber index, unsigned scale, int32_t disp)
{
    instrDesc* id = emitNewInstrAmd(attr, disp);
    insFormat  fmt;

    if (reg == REG_NA)
    {
        fmt = emitInsModeFormat(ins, IF_ARD);
    }
    else
    {
        fmt = emitInsModeFormat(ins, IF_ARD_RRD);

        noway_assert(emitVerifyEncodable(ins, EA_SIZE(attr), reg));
        assert(!instIsFP(ins) && (EA_SIZE(attr) <= EA_32BYTE));

        id->idReg1(reg);
    }

    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idAddr()->iiaAddrMode.amBaseReg = base;
    id->idAddr()->iiaAddrMode.amIndxReg = index;
    id->idAddr()->iiaAddrMode.amScale   = ScaleEncoding(scale);

    unsigned sz = emitInsSizeAM(id, insCodeMR(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
#if !FEATURE_FIXED_OUT_ARGS
    emitAdjustStackDepthPushPop(ins);
#endif
}

void emitter::emitIns_SIMD_R_R_I(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int32_t imm)
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

void emitter::emitIns_SIMD_R_R_A(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, GenTree* addr)
{
    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        emitIns_SIMD_R_R_C(ins, attr, reg1, reg2, clsAddr->GetFieldHandle());
    }
    else if (UseVEXEncoding())
    {
        emitIns_R_R_A(ins, attr, reg1, reg2, addr);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_RRW_A(ins, attr, reg1, addr);
    }
}

void emitter::emitIns_SIMD_R_R_C(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, CORINFO_FIELD_HANDLE field)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_C(ins, attr, reg1, reg2, field);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_C(ins, attr, reg1, field);
    }
}

void emitter::emitIns_SIMD_R_R_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3)
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

        if (IsMovInstruction(ins))
        {
            emitIns_Mov(ins, attr, reg1, reg3, /* canSkip */ false);
        }
        else
        {
            emitIns_R_R(ins, attr, reg1, reg3);
        }
    }
}

void emitter::emitIns_SIMD_R_R_S(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int varx, int offs)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_S(ins, attr, reg1, reg2, varx, offs);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_S(ins, attr, reg1, varx, offs);
    }
}

void emitter::emitIns_SIMD_R_R_A_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, GenTree* addr, int32_t imm)
{
    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        emitIns_SIMD_R_R_C_I(ins, attr, reg1, reg2, clsAddr->GetFieldHandle(), imm);
    }
    else if (UseVEXEncoding())
    {
        emitIns_R_R_A_I(ins, attr, reg1, reg2, addr, imm, IF_RWR_RRD_ARD_CNS);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_A_I(ins, attr, reg1, addr, imm);
    }
}

void emitter::emitIns_SIMD_R_R_C_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, CORINFO_FIELD_HANDLE field, int32_t imm)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_C_I(ins, attr, reg1, reg2, field, imm);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_C_I(ins, attr, reg1, field, imm);
    }
}

void emitter::emitIns_SIMD_R_R_R_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, int32_t imm)
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

void emitter::emitIns_SIMD_R_R_S_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int varx, int offs, int32_t imm)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_S_I(ins, attr, reg1, reg2, varx, offs, imm);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_S_I(ins, attr, reg1, varx, offs, imm);
    }
}

void emitter::emitIns_SIMD_R_R_R_A(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, GenTree* addr)
{
    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        emitIns_SIMD_R_R_R_C(ins, attr, reg1, reg2, reg3, clsAddr->GetFieldHandle());
        return;
    }

    assert(IsFMAInstruction(ins) || IsAVXVNNIInstruction(ins));
    assert(UseVEXEncoding());

    // Ensure we aren't overwriting op2
    assert((reg3 != reg1) || (reg2 == reg1));

    emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
    emitIns_R_R_A(ins, attr, reg1, reg3, addr);
}

void emitter::emitIns_SIMD_R_R_R_C(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, CORINFO_FIELD_HANDLE field)
{
    assert(IsFMAInstruction(ins));
    assert(UseVEXEncoding());

    // Ensure we aren't overwriting op2
    assert((reg3 != reg1) || (reg2 == reg1));

    emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
    emitIns_R_R_C(ins, attr, reg1, reg3, field);
}

void emitter::emitIns_SIMD_R_R_R_R(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, regNumber reg4)
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
        assert(isAvxBlendv(ins) || isSse41Blendv(ins));

        switch (ins)
        {
            case INS_blendvps:
                ins = INS_vblendvps;
                break;
            case INS_blendvpd:
                ins = INS_vblendvpd;
                break;
            case INS_pblendvb:
                ins = INS_vpblendvb;
                break;
            default:
                break;
        }
        emitIns_R_R_R_R(ins, attr, reg1, reg2, reg3, reg4);
    }
    else
    {
        assert(isSse41Blendv(ins));

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

void emitter::emitIns_SIMD_R_R_R_S(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, int varx, int offs)
{
    assert(IsFMAInstruction(ins) || IsAVXVNNIInstruction(ins));
    assert(UseVEXEncoding());

    // Ensure we aren't overwriting op2
    assert((reg3 != reg1) || (reg2 == reg1));

    emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
    emitIns_R_R_S(ins, attr, reg1, reg3, varx, offs);
}

void emitter::emitIns_SIMD_R_R_A_R(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, GenTree* addr)
{
    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        emitIns_SIMD_R_R_C_R(ins, attr, reg1, reg2, reg3, clsAddr->GetFieldHandle());
    }
    else if (UseVEXEncoding())
    {
        assert(isAvxBlendv(ins) || isSse41Blendv(ins));

        switch (ins)
        {
            case INS_blendvps:
                ins = INS_vblendvps;
                break;
            case INS_blendvpd:
                ins = INS_vblendvpd;
                break;
            case INS_pblendvb:
                ins = INS_vpblendvb;
                break;
            default:
                break;
        }

        emitIns_R_R_A_R(ins, attr, reg1, reg2, reg3, addr);
    }
    else
    {
        assert(isSse41Blendv(ins));

        // Ensure we aren't overwriting op1
        assert(reg2 != REG_XMM0);

        // SSE4.1 blendv* hardcode the mask vector (op3) in XMM0
        emitIns_Mov(INS_movaps, attr, REG_XMM0, reg3, /* canSkip */ true);

        // Ensure we aren't overwriting op3 (which should be REG_XMM0)
        assert(reg1 != REG_XMM0);

        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_RRW_A(ins, attr, reg1, addr);
    }
}

void emitter::emitIns_SIMD_R_R_C_R(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, CORINFO_FIELD_HANDLE field)
{
    if (UseVEXEncoding())
    {
        assert(isAvxBlendv(ins) || isSse41Blendv(ins));

        switch (ins)
        {
            case INS_blendvps:
                ins = INS_vblendvps;
                break;
            case INS_blendvpd:
                ins = INS_vblendvpd;
                break;
            case INS_pblendvb:
                ins = INS_vpblendvb;
                break;
            default:
                break;
        }

        emitIns_R_R_C_R(ins, attr, reg1, reg2, reg3, field);
    }
    else
    {
        assert(isSse41Blendv(ins));

        // Ensure we aren't overwriting op1
        assert(reg2 != REG_XMM0);

        // SSE4.1 blendv* hardcode the mask vector (op3) in XMM0
        emitIns_Mov(INS_movaps, attr, REG_XMM0, reg3, /* canSkip */ true);

        // Ensure we aren't overwriting op3 (which should be REG_XMM0)
        assert(reg1 != REG_XMM0);

        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_C(ins, attr, reg1, field);
    }
}

void emitter::emitIns_SIMD_R_R_S_R(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, int varx, int offs)
{
    if (UseVEXEncoding())
    {
        assert(isAvxBlendv(ins) || isSse41Blendv(ins));

        // convert SSE encoding of SSE4.1 instructions to VEX encoding
        switch (ins)
        {
            case INS_blendvps:
            {
                ins = INS_vblendvps;
                break;
            }

            case INS_blendvpd:
            {
                ins = INS_vblendvpd;
                break;
            }

            case INS_pblendvb:
            {
                ins = INS_vpblendvb;
                break;
            }

            default:
            {
                break;
            }
        }

        emitIns_R_R_S_R(ins, attr, reg1, reg2, reg3, varx, offs);
    }
    else
    {
        assert(isSse41Blendv(ins));

        // Ensure we aren't overwriting op1
        assert(reg2 != REG_XMM0);

        // SSE4.1 blendv* hardcode the mask vector (op3) in XMM0
        emitIns_Mov(INS_movaps, attr, REG_XMM0, reg3, /* canSkip */ true);

        // Ensure we aren't overwriting op3 (which should be REG_XMM0)
        assert(reg1 != REG_XMM0);

        emitIns_Mov(INS_movaps, attr, reg1, reg2, /* canSkip */ true);
        emitIns_R_S(ins, attr, reg1, varx, offs);
    }
}

void emitter::emitIns_S(instruction ins, emitAttr attr, int varx, int offs)
{
    instrDesc* id = emitNewInstr(attr);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_SRD));
    SetInstrLclAddrMode(id, varx, offs);

    unsigned sz = emitInsSizeSV(id, insCodeMR(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
#if !FEATURE_FIXED_OUT_ARGS
    emitAdjustStackDepthPushPop(ins);
#endif
}

void emitter::emitIns_S_R(instruction ins, emitAttr attr, regNumber reg, int varx, int offs)
{
#ifdef TARGET_X86
    if (attr == EA_1BYTE)
    {
        assert(isByteReg(reg));
    }
#endif

    instrDesc* id = emitNewInstr(attr);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_SRD_RRD));
    id->idReg1(reg);
    SetInstrLclAddrMode(id, varx, offs);

    unsigned sz = emitInsSizeSV(id, insCodeMR(ins));

    // TODO-MIKE-Cleanup: Bozos thought that lfence & co. have VEX.
    if (UseVEXEncoding() && (ins == INS_movnti))
    {
        sz++;
    }

    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_S(instruction ins, emitAttr attr, regNumber reg, int varx, int offs)
{
    assert(!instrHasImplicitRegPairDest(ins) && (ins != INS_imuli));
    noway_assert(emitVerifyEncodable(ins, EA_SIZE(attr), reg));

    instrDesc* id = emitNewInstr(attr);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_RRD_SRD));
    id->idReg1(reg);
    SetInstrLclAddrMode(id, varx, offs);

    unsigned sz = emitInsSizeSV(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_S_I(instruction ins, emitAttr attr, int varx, int offs, int32_t imm)
{
    AMD64_ONLY(assert(!EA_IS_CNS_RELOC(attr)));

    if (IsShiftImm(ins))
    {
        assert(imm != 1);
        imm &= 0x7F;
    }

    instrDesc* id = emitNewInstrCns(attr, imm);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_SRD_CNS));
    SetInstrLclAddrMode(id, varx, offs);

    unsigned sz = emitInsSizeSV(id, insCodeMI(ins)) + emitInsSizeImm(ins, attr, imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitSetShortJump(instrDescJmp* id)
{
    if (!id->idjKeepLong)
    {
        id->idjShort = true;
    }
}

#ifdef TARGET_X86
void emitter::emitIns_L(instruction ins, BasicBlock* dst)
{
    assert(ins == INS_push_hide);
    assert((dst->bbFlags & BBF_HAS_LABEL) != 0);

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(ins);
    id->idInsFmt(IF_LABEL);
    id->idAddr()->iiaBBlabel = dst;
    id->idjKeepLong          = true;
    id->idjIG                = emitCurIG;
    id->idjOffs              = emitCurIGsize;

    // Pushing the address of a basicBlock will need a reloc as the
    // instruction uses the absolute address, not a relative address.
    if (emitComp->opts.compReloc)
    {
        id->idSetIsDspReloc();
    }

    id->idjNext      = emitCurIGjmpList;
    emitCurIGjmpList = id;

    id->idCodeSize(PUSH_INST_SIZE);
    dispIns(id);
    emitCurIGsize += PUSH_INST_SIZE;
}
#endif // TARGET_X86

void emitter::emitIns_J(instruction ins, BasicBlock* dst, int instrCount)
{
    assert((ins == INS_jmp) || IsJccInstruction(ins) AMD64_ONLY(|| ins == INS_call));

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(ins);
    id->idInsFmt(IF_LABEL);

    INDEBUG(id->idDebugOnlyInfo()->idFinallyCall =
                (ins == INS_call) && (GetCurrentBlock()->bbJumpKind == BBJ_CALLFINALLY));

    if (dst != nullptr)
    {
        assert((dst->bbFlags & BBF_HAS_LABEL) != 0);
        assert(instrCount == 0);

        id->idAddr()->iiaBBlabel = dst;
        id->idjKeepLong          = (ins == INS_call) || InDifferentRegions(GetCurrentBlock(), dst);
    }
    else
    {
        // Only allow non-label jmps in prolog.
        assert(emitIGisInProlog(emitCurIG));
        assert(instrCount != 0);

        id->idAddr()->iiaSetInstrCount(instrCount);
        id->idjShort = true;
        id->idSetIsBound();
    }

    id->idjIG        = emitCurIG;
    id->idjOffs      = emitCurIGsize;
    id->idjNext      = emitCurIGjmpList;
    emitCurIGjmpList = id;

    // Figure out the max. size of the jump/call instruction.

    unsigned sz;

    if (ins == INS_call)
    {
        sz = CALL_INST_SIZE;
    }
    else
    {
        insGroup* tgt = nullptr;

        if (dst != nullptr)
        {
            sz  = ins == INS_jmp ? JMP_SIZE_LARGE : JCC_SIZE_LARGE;
            tgt = emitCodeGetCookie(dst);
        }
        else
        {
            sz = JMP_SIZE_SMALL;
        }

        if (tgt != nullptr)
        {
            int      extra;
            unsigned srcOffs;
            int      jmpDist;

            assert(JMP_SIZE_SMALL == JCC_SIZE_SMALL);

            // This is a backward jump - figure out the distance

            srcOffs = emitCurCodeOffset + emitCurIGsize + JMP_SIZE_SMALL;

            jmpDist = srcOffs - tgt->igOffs;
            assert((int)jmpDist > 0);

            extra = jmpDist + JMP_DIST_SMALL_MAX_NEG;

#if DEBUG_EMIT
            if (id->idDebugOnlyInfo()->idNum == (unsigned)INTERESTING_JUMP_NUM || INTERESTING_JUMP_NUM == 0)
            {
                if (INTERESTING_JUMP_NUM == 0)
                {
                    printf("[0] Jump %u:\n", id->idDebugOnlyInfo()->idNum);
                }

                printf("[0] Jump source is at %08X\n", srcOffs);
                printf("[0] Label block is at %08X\n", tgt->igOffs);
                printf("[0] Jump  distance  - %04X\n", jmpDist);

                if (extra > 0)
                {
                    printf("[0] Distance excess = %d  \n", extra);
                }
            }
#endif

            if (extra <= 0 && !id->idjKeepLong)
            {
                emitSetShortJump(id);
                sz = JMP_SIZE_SMALL;
            }
        }
#if DEBUG_EMIT
        else
        {
            if (id->idDebugOnlyInfo()->idNum == (unsigned)INTERESTING_JUMP_NUM || INTERESTING_JUMP_NUM == 0)
            {
                if (INTERESTING_JUMP_NUM == 0)
                {
                    printf("[0] Jump %u:\n", id->idDebugOnlyInfo()->idNum);
                }
                printf("[0] Jump source is at %04X/%08X\n", emitCurIGsize,
                       emitCurCodeOffset + emitCurIGsize + JMP_SIZE_SMALL);
                printf("[0] Label block is unknown\n");
            }
        }
#endif
    }

    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

ssize_t emitter::emitGetInsCns(instrDesc* id)
{
    return id->idIsLargeCns() ? static_cast<instrDescCns*>(id)->idcCnsVal : id->idSmallCns();
}

ssize_t emitter::emitGetInsMemDisp(instrDesc* id)
{
    if (!id->idIsLargeDsp())
    {
        return 0;
    }
    else if (id->idIsLargeCns())
    {
        return static_cast<instrDescCnsDsp*>(id)->iddcDspVal;
    }
    else
    {
        return static_cast<instrDescDsp*>(id)->iddDspVal;
    }
}

ssize_t emitter::emitGetInsMemImm(instrDesc* id)
{
    if (!id->idIsLargeCns())
    {
        return id->idSmallCns();
    }
    else if (id->idIsLargeDsp())
    {
        return static_cast<instrDescCnsDsp*>(id)->iddcCnsVal;
    }
    else
    {
        return static_cast<instrDescCns*>(id)->idcCnsVal;
    }
}

ssize_t emitter::emitGetInsAmdCns(instrDesc* id)
{
    if (!id->idIsLargeCns())
    {
        return id->idSmallCns();
    }
    else if (id->idIsLargeDsp())
    {
        return static_cast<instrDescCnsAmd*>(id)->idacCnsVal;
    }
    else
    {
        return static_cast<instrDescCns*>(id)->idcCnsVal;
    }
}

ssize_t emitter::emitGetInsAmdDisp(instrDesc* id)
{
    if (!id->idIsLargeDsp())
    {
        return id->idAddr()->iiaAddrMode.amDisp;
    }
    else if (id->idIsLargeCns())
    {
        return static_cast<instrDescCnsAmd*>(id)->idacAmdVal;
    }
    else
    {
        return static_cast<instrDescAmd*>(id)->idaAmdVal;
    }
}

ssize_t emitter::emitGetInsCallDisp(instrDesc* id)
{
    if (id->idIsLargeCall())
    {
        return static_cast<instrDescCGCA*>(id)->idcDisp;
    }
    else
    {
        assert(!id->idIsLargeDsp());
        assert(!id->idIsLargeCns());

        return id->idAddr()->iiaAddrMode.amDisp;
    }
}

#if !FEATURE_FIXED_OUT_ARGS
void emitter::emitAdjustStackDepthPushPop(instruction ins)
{
    if (ins == INS_push)
    {
        emitCurStackLvl += emitCntStackDepth;

        if (emitMaxStackDepth < emitCurStackLvl)
        {
            JITDUMP("Upping emitMaxStackDepth from %d to %d\n", emitMaxStackDepth, emitCurStackLvl);
            emitMaxStackDepth = emitCurStackLvl;
        }
    }
    else if (ins == INS_pop)
    {
        emitCurStackLvl -= emitCntStackDepth;
        assert((int)emitCurStackLvl >= 0);
    }
}

void emitter::emitAdjustStackDepth(instruction ins, ssize_t val)
{
    // If we're in the prolog or epilog, or otherwise not tracking the stack depth, just return.
    if (emitCntStackDepth == 0)
    {
        return;
    }

    if (ins == INS_sub)
    {
        S_UINT32 newStackLvl(emitCurStackLvl);
        newStackLvl += S_UINT32(val);
        noway_assert(!newStackLvl.IsOverflow());

        emitCurStackLvl = newStackLvl.Value();

        if (emitMaxStackDepth < emitCurStackLvl)
        {
            JITDUMP("Upping emitMaxStackDepth from %d to %d\n", emitMaxStackDepth, emitCurStackLvl);
            emitMaxStackDepth = emitCurStackLvl;
        }
    }
    else if (ins == INS_add)
    {
        S_UINT32 newStackLvl = S_UINT32(emitCurStackLvl) - S_UINT32(val);
        noway_assert(!newStackLvl.IsOverflow());

        emitCurStackLvl = newStackLvl.Value();
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
void emitter::emitIns_Call(EmitCallType          kind,
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
                           regNumber amBase,
                           regNumber amIndex,
                           unsigned  amScale,
                           int32_t   amDisp,
                           bool      isJump)
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

    instrDesc* id = emitNewInstrCall(methodHandle, retRegAttr,
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

    unsigned insSize;

    if (kind == EC_INDIR_R)
    {
        id->idInsFmt(IF_RRD);
        // Move the GC regs info to the unused address mode bits.
        id->idAddr()->iiaAddrMode.amBaseReg = id->idReg1();
        id->idAddr()->iiaAddrMode.amIndxReg = id->idReg2();
        id->idReg1(amBase);

        insSize = 2 + IsExtendedReg(amBase);
    }
    else if (kind == EC_INDIR_ARD)
    {
        assert(amBase != REG_NA);

        id->idInsFmt(IF_ARD);
        id->idAddr()->iiaAddrMode.amBaseReg = amBase;
        id->idAddr()->iiaAddrMode.amIndxReg = amIndex;
        id->idAddr()->iiaAddrMode.amScale   = ScaleEncoding(amScale);

        insSize = emitInsSizeAM(id, insCodeMR(INS_call));
    }
    else if (kind == EC_FUNC_TOKEN_INDIR)
    {
        assert(addr != nullptr);

        id->idInsFmt(IF_METHPTR);
        id->idAddr()->iiaAddr = addr;

        insSize = 6;

        // Since this is an indirect call through a pointer and we don't
        // currently pass in emitAttr into this function, we query codegen
        // whether addr needs a reloc.
        if (emitComp->opts.compReloc AMD64_ONLY(|| !IsImm32(reinterpret_cast<intptr_t>(addr))))
        {
            id->idSetIsDspReloc();
        }
        else
        {
            // This addr mode requires an extra SIB byte.
            AMD64_ONLY(insSize++);
        }
    }
    else
    {
        assert(kind == EC_FUNC_TOKEN);
        assert(addr != nullptr);

        id->idInsFmt(IF_METHOD);
        id->idAddr()->iiaAddr = addr;

        // Direct call to a method and no addr indirection is needed.
        // On x64 all direct code addresses go through relocation so that VM will
        // setup a jump stub if addr cannot be encoded as RIP relative offset.
        X86_ONLY(if (emitComp->opts.compReloc))
        {
            id->idSetIsDspReloc();
        }

        insSize = 5;
    }

#ifdef DEBUG
    id->idDebugOnlyInfo()->idHandle  = methodHandle;
    id->idDebugOnlyInfo()->idCallSig = sigInfo;
#endif

#ifdef LATE_DISASM
    if (addr != nullptr)
    {
        codeGen->getDisAssembler().disSetMethod(reinterpret_cast<size_t>(addr), methodHandle);
    }
#endif

    id->idCodeSize(insSize);

    dispIns(id);
    emitCurIGsize += insSize;

#if !FEATURE_FIXED_OUT_ARGS
    if (emitCntStackDepth && (argSize > 0))
    {
        noway_assert(argSize <= static_cast<int32_t>(emitCurStackLvl));

        emitCurStackLvl -= argSize;
        assert(emitCurStackLvl <= INT_MAX);
    }
#endif
}

void emitter::EncodeCallGCRegs(regMaskTP regs, instrDesc* id)
{
    static_assert_no_msg((4 <= REGNUM_BITS) && (REGNUM_BITS <= 8));
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

    id->idReg1(static_cast<regNumber>(encoded));

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

    id->idReg2(static_cast<regNumber>(encoded));
#endif
}

unsigned emitter::DecodeCallGCRegs(instrDesc* id)
{
    static_assert_no_msg((4 <= REGNUM_BITS) && (REGNUM_BITS <= 8));

    unsigned encoded;

    if (id->idInsFmt() == IF_RRD)
    {
        encoded = id->idAddr()->iiaAddrMode.amBaseReg | (id->idAddr()->iiaAddrMode.amIndxReg << 8);
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
#define IF_DEF(en, op1, op2) ID_OP_##op2,
#include "emitfmtsxarch.h"
    };

    assert(format < _countof(ops));
    return ops[format];
}

#ifdef DEBUG
void emitter::emitInsSanityCheck(instrDesc* id)
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

// Return the allocated size (in bytes) of the given instruction descriptor.
size_t emitter::emitSizeOfInsDsc(instrDesc* id)
{
    if (id->idIsSmallDsc())
    {
        return sizeof(instrDescSmall);
    }

    if (id->idIsLargeCall())
    {
        return sizeof(instrDescCGCA);
    }

    switch (GetFormatOp(id->idInsFmt()))
    {
        case ID_OP_NONE:
#if FEATURE_LOOP_ALIGN
            if (id->idIns() == INS_align)
            {
                return sizeof(instrDescAlign);
            }
#endif
            break;

        case ID_OP_JMP:
            return sizeof(instrDescJmp);

        case ID_OP_CALL:
        case ID_OP_CNS:
        case ID_OP_DSP:
        case ID_OP_DSP_CNS:
            if (id->idIsLargeCns())
            {
                return id->idIsLargeDsp() ? sizeof(instrDescCnsDsp) : sizeof(instrDescCns);
            }

            if (id->idIsLargeDsp())
            {
                return sizeof(instrDescDsp);
            }
            break;

        case ID_OP_AMD:
        case ID_OP_AMD_CNS:
            if (id->idIsLargeCns())
            {
                return id->idIsLargeDsp() ? sizeof(instrDescCnsAmd) : sizeof(instrDescCns);
            }

            if (id->idIsLargeDsp())
            {
                return sizeof(instrDescAmd);
            }
            break;

        default:
            unreached();
    }

    return sizeof(instrDesc);
}

#ifdef DEBUG

void emitter::PrintClsVar(instrDesc* id)
{
    CORINFO_FIELD_HANDLE field = id->idAddr()->iiaFieldHnd;
    ssize_t              offs  = emitGetInsMemDisp(id);

#ifdef WINDOWS_X86_ABI
    if (field == FS_SEG_FIELD)
    {
        printf("fs:[0x%04X]", offs);
        return;
    }
#endif

    printf("[");

    if (id->idIsDspReloc())
    {
        printf("reloc ");
    }

    if (IsRoDataField(field))
    {
        printf("@RWD%02u", GetRoDataOffset(field));
    }
    else
    {
        printf("classVar[%#x]", emitComp->dspPtr(field));
    }

    if (offs != 0)
    {
        if (emitComp->opts.disDiffable)
        {
            ssize_t top12bits = (offs >> 20);
            if ((top12bits != 0) && (top12bits != -1))
            {
                offs = 0xD1FFAB1E;
            }
        }

        printf("%+Id", offs);
    }

    printf("]");
}

void emitter::PrintFrameRef(instrDesc* id, bool asmfm)
{
    int varNum  = id->idDebugOnlyInfo()->varNum;
    int varOffs = id->idDebugOnlyInfo()->varOffs;

    printf("[");

    if (!asmfm)
    {
        printf("%c%02d", varNum < 0 ? 'T' : 'V', abs(varNum));

        if (varOffs != 0)
        {
            printf("%c0x%X", varOffs < 0 ? '-' : '+', abs(varOffs));
        }

        printf(" ");
    }

    bool ebpBase;
    int  disp = emitComp->lvaFrameAddress(varNum, &ebpBase) + varOffs;

    printf("%s", getRegName(ebpBase ? REG_EBP : REG_ESP));

#if !FEATURE_FIXED_OUT_ARGS
    if (!ebpBase && (emitCurStackLvl != 0))
    {
        printf("+%02XH", emitCurStackLvl);
    }
#endif

    if (disp != 0)
    {
        printf("%c%02XH", disp < 0 ? '-' : '+', abs(disp));
    }

    printf("]");
}

void emitter::PrintImm(instrDesc* id, ssize_t val)
{
    if (id->idIsCnsReloc())
    {
        PrintReloc(val);
        return;
    }

    ssize_t srcVal = val;

    // Munge any pointers if we want diff-able disassembly
    if (emitComp->opts.disDiffable)
    {
        ssize_t top14bits = (val >> 18);
        if ((top14bits != 0) && (top14bits != -1))
        {
            val = 0xD1FFAB1E;
        }
    }

    if ((val > -1000) && (val < 1000))
    {
        printf("%d", val);
    }
    else if ((val > 0) || (val < -0xFFFFFF))
    {
        printf("0x%IX", val);
    }
    else
    {
        printf("-0x%IX", -val);
    }

    if (id->idDebugOnlyInfo()->idHandleKind != HandleKind::None)
    {
        emitDispCommentForHandle(reinterpret_cast<void*>(srcVal), id->idDebugOnlyInfo()->idHandleKind);
    }
}

void emitter::PrintReloc(ssize_t value)
{
    if (emitComp->opts.disAsm && emitComp->opts.disDiffable)
    {
        printf("(reloc)");
    }
    else
    {
        printf("(reloc 0x%Ix)", emitComp->dspPtr(value));
    }
}

void emitter::PrintAddrMode(instrDesc* id)
{
    ssize_t disp     = (id->idIns() == INS_call) ? emitGetInsCallDisp(id) : emitGetInsAmdDisp(id);
    bool    frameRef = false;
    bool    nsep     = false;

    printf("[");

    if (id->idAddr()->iiaAddrMode.amBaseReg != REG_NA)
    {
        printf("%s", RegName(id->idAddr()->iiaAddrMode.amBaseReg, EA_PTRSIZE));
        nsep = true;
        if (id->idAddr()->iiaAddrMode.amBaseReg == REG_ESP)
        {
            frameRef = true;
        }
        else if (codeGen->isFramePointerUsed() && id->idAddr()->iiaAddrMode.amBaseReg == REG_EBP)
        {
            frameRef = true;
        }
    }

    if (id->idAddr()->iiaAddrMode.amIndxReg != REG_NA)
    {
        if (nsep)
        {
            printf("+");
        }

        if (id->idAddr()->iiaAddrMode.amScale != 0)
        {
            printf("%u*", 1u << id->idAddr()->iiaAddrMode.amScale);
        }

        printf("%s", RegName(id->idAddr()->iiaAddrMode.amIndxReg, EA_PTRSIZE));
        nsep = true;
    }

    if (id->idIsDspReloc() && (id->idIns() != INS_i_jmp))
    {
        if (nsep)
        {
            printf("+");
        }
        PrintReloc(disp);
    }
    else
    {
        // Munge any pointers if we want diff-able disassembly
        // It's assumed to be a pointer when disp is outside of the range (-1M, +1M); top bits are not 0 or -1
        if (!frameRef && emitComp->opts.disDiffable && (static_cast<size_t>((disp >> 20) + 1) > 1))
        {
            if (nsep)
            {
                printf("+");
            }
            printf("D1FFAB1EH");
        }
        else if (disp > 0)
        {
            if (nsep)
            {
                printf("+");
            }
            if (frameRef)
            {
                printf("%02XH", disp);
            }
            else if (disp < 1000)
            {
                printf("%d", disp);
            }
            else if (disp <= 0xFFFF)
            {
                printf("%04XH", disp);
            }
            else
            {
                printf("%08XH", disp);
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
                printf("-%04XH", -disp);
            }
            else if (disp < -0xFFFFFF)
            {
                if (nsep)
                {
                    printf("+");
                }
                printf("%08XH", disp);
            }
            else
            {
                printf("-%08XH", -disp);
            }
        }
        else if (!nsep)
        {
            printf("%04XH", disp);
        }
    }

    printf("]");

    // pretty print string if it looks like one
    if ((id->idGCref() == GCT_GCREF) && (id->idIns() == INS_mov) && (id->idAddr()->iiaAddrMode.amBaseReg == REG_NA))
    {
        // TODO-MIKE-Review: This stuff is dubious, probably it only works because strings are the only
        // loading a REF from a memory location. Well, you would expect a static object field load to
        // look identical but on x86 such loads will use CLS_VAR_ADDR as address and this is treated as
        // reloc (even when jitting, why?!?). And on x64 apparently RIP addressing is not used in this
        // case so this is never hit.

        if (const WCHAR* str = emitComp->eeGetCPString(reinterpret_cast<void*>(disp)))
        {
            printf("      '%S'", str);
        }
    }
}

void emitter::PrintShiftCL(instruction ins)
{
    if (IsShiftCL(ins))
    {
        printf(", cl");
    }
}

void emitter::PrintHexCode(instrDesc* id, uint8_t* code, size_t size)
{
    if (emitComp->opts.disDiffable)
    {
        return;
    }

#ifdef TARGET_AMD64
    const size_t digits = 10;
#else
    const size_t digits = 6;
#endif

    printf(" ");

    for (size_t i = 0; i < size; i++)
    {
        printf("%02X", code[i]);
    }

    if (size < digits)
    {
        printf("%.*s", 2 * (digits - size), "                         ");
    }
}

static const char* GetSizeOperator(emitAttr attr)
{
    switch (EA_SIZE(attr))
    {
        case 1:
            return "byte ptr ";
        case 2:
            return "word ptr ";
        case 4:
            return "dword ptr ";
        case 8:
            return "qword ptr ";
        case 16:
            return "xmmword ptr ";
        case 32:
            return "ymmword ptr ";
        default:
            return "??? ";
    }
}

const char* emitter::genInsDisplayName(instrDesc* id)
{
    instruction ins  = id->idIns();
    const char* name = insName(ins);

    static unsigned curBuf = 0;
    static char     buf[4][40];

    // TODO-MIKE-Cleanup: This should check FIRST_SSE_VEX_INSTRUCTION instead of INS_FIRST_SSE_INSTRUCTION,
    // bozos thought that lfence & co. have VEX.
    if (UseVEXEncoding() && (INS_FIRST_SSE_INSTRUCTION <= ins) && (ins <= INS_LAST_SSE_INSTRUCTION))
    {
        auto& retbuf = buf[curBuf++ % _countof(buf)];
        sprintf_s(retbuf, _countof(retbuf), "v%s", name);
        return retbuf;
    }

    if ((ins == INS_stos) || (ins == INS_movs) || (ins == INS_rep_stos) || (ins == INS_rep_movs))
    {
        auto& retbuf = buf[curBuf++ % _countof(buf)];
        sprintf_s(retbuf, _countof(retbuf), "%s%c", name, GetSizeOperator(id->idOpSize())[0]);
        return retbuf;
    }

    return name;
}

void emitter::emitDispIns(
    instrDesc* id, bool isNew, bool doffs, bool asmfm, unsigned offset, uint8_t* code, size_t sz, insGroup* ig)
{
    if (id->idInsFmt() == IF_GC_REG)
    {
        return;
    }

    if (emitComp->verbose)
    {
        printf("IN%04x: ", id->idDebugOnlyInfo()->idNum);
    }

    if (!isNew && !asmfm)
    {
        doffs = true;
    }

    emitDispInsAddr(code);
    emitDispInsOffs(offset, doffs);

    if (code != nullptr)
    {
        assert(((code >= emitCodeBlock) && (code < emitCodeBlock + emitTotalHotCodeSize)) ||
               ((code >= emitColdCodeBlock) && (code < emitColdCodeBlock + emitTotalColdCodeSize)));

        PrintHexCode(id, code + writeableOffset, sz);
    }

    const char* sstr = genInsDisplayName(id);
    printf(" %-9s", sstr);

#ifndef HOST_UNIX
    if (strnlen_s(sstr, 10) >= 9)
#else
    if (strnlen(sstr, 10) >= 9)
#endif
    {
        printf(" ");
    }

    assert((id->idCodeSize() != 0) || InstrHasNoCode(id));

    instruction ins = id->idIns();
    emitAttr    attr;

    if (id->idGCref() == GCT_GCREF)
    {
        attr = EA_GCREF;
        sstr = "gword ptr ";
    }
    else if (id->idGCref() == GCT_BYREF)
    {
        attr = EA_BYREF;
        sstr = "bword ptr ";
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
                sstr = GetSizeOperator(EA_16BYTE);
                break;
            case INS_pextrb:
            case INS_pinsrb:
                sstr = GetSizeOperator(EA_1BYTE);
                break;
            case INS_pextrw:
            case INS_pextrw_sse41:
            case INS_pinsrw:
                sstr = GetSizeOperator(EA_2BYTE);
                break;
            case INS_extractps:
            case INS_insertps:
            case INS_pextrd:
            case INS_pinsrd:
                sstr = GetSizeOperator(EA_4BYTE);
                break;
            case INS_pextrq:
            case INS_pinsrq:
                sstr = GetSizeOperator(EA_8BYTE);
                break;
            case INS_lea:
                sstr = "";
                break;
            default:
                sstr = GetSizeOperator(attr);
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
            PrintImm(id, emitGetInsSC(id));
            break;

        case IF_ARD:
        case IF_AWR:
        case IF_ARW:
            printf("%s", sstr);
            PrintAddrMode(id);

            if ((ins == INS_call) || (ins == INS_i_jmp))
            {
                if (id->idDebugOnlyInfo()->idHandle != nullptr)
                {
                    printf("%s", emitComp->eeGetMethodFullName(
                                     static_cast<CORINFO_METHOD_HANDLE>(id->idDebugOnlyInfo()->idHandle)));
                }
            }
            else
            {
                PrintShiftCL(ins);
            }
            break;

        case IF_RRD_ARD:
        case IF_RWR_ARD:
        case IF_RRW_ARD:
            printf("%s, %s", RegName(id->idReg1(), attr1), sstr);
            PrintAddrMode(id);
            break;

        case IF_RRW_ARD_CNS:
        case IF_RWR_ARD_CNS:
            printf("%s, %s", RegName(id->idReg1(), attr), sstr);
            PrintAddrMode(id);
            printf(", ");
            PrintImm(id, emitGetInsAmdCns(id));
            break;

        case IF_AWR_RRD_CNS:
            printf("%s", GetSizeOperator(EA_16BYTE));
            PrintAddrMode(id);
            printf(", %s, ", RegName(id->idReg1(), attr));
            PrintImm(id, emitGetInsAmdCns(id));
            break;

        case IF_RWR_RRD_ARD:
            printf("%s, %s, %s", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr), sstr);
            PrintAddrMode(id);
            break;

        case IF_RWR_ARD_RRD:
            if ((ins == INS_vpgatherqd) || (ins == INS_vgatherqps))
            {
                attr1 = EA_16BYTE;
                attr2 = EA_16BYTE;
            }

            printf("%s, %s", RegName(id->idReg1(), attr1), GetSizeOperator(EA_4BYTE));
            PrintAddrMode(id);
            printf(", %s", RegName(id->idReg2(), attr2));
            break;

        case IF_RWR_RRD_ARD_CNS:
            printf("%s, %s, %s", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr), sstr);
            PrintAddrMode(id);
            printf(", ");
            PrintImm(id, emitGetInsAmdCns(id));
            break;

        case IF_RWR_RRD_ARD_RRD:
            printf("%s, %s, ", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr));
            PrintAddrMode(id);
            printf(", %s", RegName(static_cast<regNumber>((emitGetInsAmdCns(id) >> 4) + XMMBASE), attr));
            break;

        case IF_ARD_RRD:
        case IF_AWR_RRD:
        case IF_ARW_RRD:
            printf("%s", sstr);
            PrintAddrMode(id);
            printf(", %s", RegName(id->idReg1(), attr));
            break;

        case IF_AWR_RRD_RRD:
            printf("%s", sstr);
            PrintAddrMode(id);
            printf(", %s, %s", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr));
            break;

        case IF_ARD_CNS:
        case IF_AWR_CNS:
        case IF_ARW_CNS:
            printf("%s", sstr);
            PrintAddrMode(id);
            printf(", ");
            PrintImm(id, emitGetInsAmdCns(id));
            break;

        case IF_SRD:
        case IF_SWR:
        case IF_SRW:
            printf("%s", sstr);
#if !FEATURE_FIXED_OUT_ARGS
            if (ins == INS_pop)
            {
                emitCurStackLvl -= REGSIZE_BYTES;
            }
#endif
            PrintFrameRef(id, asmfm);
#if !FEATURE_FIXED_OUT_ARGS
            if (ins == INS_pop)
            {
                emitCurStackLvl += REGSIZE_BYTES;
            }
#endif
            PrintShiftCL(ins);
            break;

        case IF_SRD_RRD:
        case IF_SWR_RRD:
        case IF_SRW_RRD:
            printf("%s", sstr);
            PrintFrameRef(id, asmfm);
            printf(", %s", RegName(id->idReg1(), attr));
            break;

        case IF_SRD_CNS:
        case IF_SWR_CNS:
        case IF_SRW_CNS:
            printf("%s", sstr);
            PrintFrameRef(id, asmfm);
            printf(", ");
            PrintImm(id, emitGetInsCns(id));
            break;

        case IF_SWR_RRD_CNS:
            printf("%s", sstr);
            PrintFrameRef(id, asmfm);
            printf(", %s, ", RegName(id->idReg1(), attr));
            PrintImm(id, emitGetInsAmdCns(id));
            break;

        case IF_RRD_SRD:
        case IF_RWR_SRD:
        case IF_RRW_SRD:
            printf("%s, %s", RegName(id->idReg1(), attr1), sstr);
            PrintFrameRef(id, asmfm);
            break;

        case IF_RRW_SRD_CNS:
        case IF_RWR_SRD_CNS:
            printf("%s, %s", RegName(id->idReg1(), attr), sstr);
            PrintFrameRef(id, asmfm);
            printf(", ");
            PrintImm(id, emitGetInsCns(id));
            break;

        case IF_RWR_RRD_SRD:
            printf("%s, %s, %s", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr), sstr);
            PrintFrameRef(id, asmfm);
            break;

        case IF_RWR_RRD_SRD_CNS:
            printf("%s, %s, %s", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr), sstr);
            PrintFrameRef(id, asmfm);
            printf(", ");
            PrintImm(id, emitGetInsCns(id));
            break;

        case IF_RWR_RRD_SRD_RRD:
            printf("%s, %s, ", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr));
            PrintFrameRef(id, asmfm);
            printf(", %s", RegName(static_cast<regNumber>((emitGetInsCns(id) >> 4) + XMMBASE), attr));
            break;

        case IF_RRD_RRD:
        case IF_RWR_RRD:
        case IF_RRW_RRD:
        case IF_RRW_RRW:
            if ((ins == INS_cvtsi2ss) || (ins == INS_cvtsi2sd) || (ins == INS_cvttsd2si) || (ins == INS_cvtss2si) ||
                (ins == INS_cvtsd2si) || (ins == INS_cvttss2si))
            {
                // TODO-MIKE-Cleanup: Remove stray space.
                printf(" ");
            }

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
            PrintImm(id, emitGetInsSC(id));
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
            PrintImm(id, emitGetInsSC(id));
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
            printf("%s, %s", RegName(id->idReg1(), attr1), sstr);
            PrintClsVar(id);
            break;

        case IF_RRW_MRD_CNS:
        case IF_RWR_MRD_CNS:
            printf("%s, %s", RegName(id->idReg1(), attr), sstr);
            PrintClsVar(id);
            printf(", ");
            PrintImm(id, emitGetInsMemImm(id));
            break;

        case IF_MWR_RRD_CNS:
            printf("%s", GetSizeOperator(EA_16BYTE));
            PrintClsVar(id);
            printf(", %s, ", RegName(id->idReg1(), attr));
            PrintImm(id, emitGetInsMemImm(id));
            break;

        case IF_RWR_RRD_MRD:
            printf("%s, %s, %s", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr), sstr);
            PrintClsVar(id);
            break;

        case IF_RWR_RRD_MRD_CNS:
            printf("%s, %s, %s", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr), sstr);
            PrintClsVar(id);
            printf(", ");
            PrintImm(id, emitGetInsMemImm(id));
            break;

        case IF_RWR_RRD_MRD_RRD:
            printf("%s, %s, ", RegName(id->idReg1(), attr), RegName(id->idReg2(), attr));
            PrintClsVar(id);
            printf(", %s", RegName(static_cast<regNumber>((emitGetInsMemImm(id) >> 4) + XMMBASE), attr));
            break;

        case IF_MRD_RRD:
        case IF_MWR_RRD:
        case IF_MRW_RRD:
            printf("%s", sstr);
            PrintClsVar(id);
            printf(", %s", RegName(id->idReg1(), attr));
            break;

        case IF_MRD_CNS:
        case IF_MWR_CNS:
        case IF_MRW_CNS:
            printf("%s", sstr);
            PrintClsVar(id);
            printf(", ");
            PrintImm(id, emitGetInsMemImm(id));
            break;

        case IF_MRD:
        case IF_MWR:
        case IF_MRW:
            printf("%s", sstr);
            PrintClsVar(id);
            PrintShiftCL(ins);
            break;

        case IF_RRD_CNS:
        case IF_RWR_CNS:
        case IF_RRW_CNS:
            printf("%s, ", RegName(id->idReg1(), attr));
            PrintImm(id, emitGetInsSC(id));
            break;

        case IF_LABEL:
        case IF_RWR_LABEL:
            if (ins == INS_lea)
            {
                printf("%s, ", RegName(id->idReg1(), attr));
            }

            if (static_cast<instrDescJmp*>(id)->idjShort)
            {
                printf("SHORT ");
            }

            if (id->idIsBound())
            {
                if (id->idAddr()->iiaHasInstrCount())
                {
                    printf("%3d instr", id->idAddr()->iiaGetInstrCount());
                }
                else
                {
                    emitPrintLabel(id->idAddr()->iiaIGlabel);
                }
            }
            else
            {
                printf("L_M%03u_" FMT_BB, emitComp->compMethodID, id->idAddr()->iiaBBlabel->bbNum);
            }
            break;

        case IF_METHOD:
        case IF_METHPTR:
            if (id->idInsFmt() == IF_METHPTR)
            {
                printf("[");
            }

            printf("%s",
                   emitComp->eeGetMethodFullName(static_cast<CORINFO_METHOD_HANDLE>(id->idDebugOnlyInfo()->idHandle)));

            if (id->idInsFmt() == IF_METHPTR)
            {
                printf("]");
            }
            break;

        case IF_NONE:
#if FEATURE_LOOP_ALIGN
            if (ins == INS_align)
            {
                printf("[%d bytes]", id->idCodeSize());
            }
#endif
            break;

        default:
            printf("???");
            break;
    }

    if ((sz != 0) && (sz != id->idCodeSize()) && (!asmfm || emitComp->verbose))
    {
        printf(" (ECS:%d, ACS:%d)", id->idCodeSize(), sz);
    }

    printf("\n");
}
#endif // DEBUG

size_t emitter::emitOutputByte(uint8_t* dst, ssize_t val)
{
    // if we're emitting code bytes, ensure that we've already emitted the rex prefix!
    AMD64_ONLY(assert(((val & 0xFF00000000LL) == 0) || ((val & 0xFFFFFFFF00000000LL) == 0xFFFFFFFF00000000LL)));

    *(dst + writeableOffset) = static_cast<uint8_t>(val);
    return 1;
}

size_t emitter::emitOutputWord(uint8_t* dst, ssize_t val)
{
    // if we're emitting code bytes, ensure that we've already emitted the rex prefix!
    AMD64_ONLY(assert(((val & 0xFF00000000LL) == 0) || ((val & 0xFFFFFFFF00000000LL) == 0xFFFFFFFF00000000LL)));

    *reinterpret_cast<int16_t*>(dst + writeableOffset) = static_cast<int16_t>(val);
    return 2;
}

size_t emitter::emitOutputLong(uint8_t* dst, ssize_t val)
{
    // if we're emitting code bytes, ensure that we've already emitted the rex prefix!
    AMD64_ONLY(assert(((val & 0xFF00000000LL) == 0) || ((val & 0xFFFFFFFF00000000LL) == 0xFFFFFFFF00000000LL)));

    *reinterpret_cast<int32_t*>(dst + writeableOffset) = static_cast<int32_t>(val);
    return 4;
}

#ifdef TARGET_AMD64
size_t emitter::emitOutputI64(uint8_t* dst, int64_t val)
{
    *reinterpret_cast<int64_t*>(dst + writeableOffset) = val;
    return 8;
}
#endif

#if defined(TARGET_X86) && !defined(HOST_64BIT)
size_t emitter::emitOutputByte(uint8_t* dst, uint32_t val)
{
    return emitOutputByte(dst, static_cast<int32_t>(val));
}

size_t emitter::emitOutputWord(uint8_t* dst, uint32_t val)
{
    return emitOutputWord(dst, static_cast<int32_t>(val));
}

size_t emitter::emitOutputLong(uint8_t* dst, uint32_t val)
{
    return emitOutputLong(dst, static_cast<int32_t>(val));
}

size_t emitter::emitOutputByte(uint8_t* dst, uint64_t val)
{
    return emitOutputByte(dst, static_cast<int32_t>(val));
}

size_t emitter::emitOutputWord(uint8_t* dst, uint64_t val)
{
    return emitOutputWord(dst, static_cast<int32_t>(val));
}

size_t emitter::emitOutputLong(uint8_t* dst, uint64_t val)
{
    return emitOutputLong(dst, static_cast<int32_t>(val));
}
#endif // defined(TARGET_X86) && !defined(HOST_64BIT)

size_t emitter::emitOutputImm(uint8_t* dst, instrDesc* id, size_t size, ssize_t imm)
{
    switch (size)
    {
        case 1:
            return emitOutputByte(dst, imm);
        case 2:
            return emitOutputWord(dst, imm);
#ifdef TARGET_AMD64
        case 8:
            noway_assert(IsImm32(imm) && !id->idIsCnsReloc());
            return emitOutputLong(dst, imm);
#endif
        default:
            assert(size == 4);
            emitOutputLong(dst, imm);

            if (id->idIsCnsReloc())
            {
                emitRecordRelocation(dst, reinterpret_cast<void*>(imm), IMAGE_REL_BASED_HIGHLOW);
            }

            return 4;
    }
}

static uint8_t* emitOutputNOP(uint8_t* dstRW, size_t nBytes)
{
    assert(nBytes <= 15);

#ifndef TARGET_AMD64
    // TODO-X86-CQ: when VIA C3 CPU's are out of circulation, switch to the
    // more efficient real NOP: 0x0F 0x1F +modR/M
    // Also can't use AMD recommended, multiple size prefixes (i.e. 0x66 0x66 0x90 for 3 byte NOP)
    // because debugger and msdis don't like it, so maybe VIA doesn't either
    // So instead just stick to repeating single byte nops

    switch (nBytes)
    {
        case 15:
            *dstRW++ = 0x90;
            FALLTHROUGH;
        case 14:
            *dstRW++ = 0x90;
            FALLTHROUGH;
        case 13:
            *dstRW++ = 0x90;
            FALLTHROUGH;
        case 12:
            *dstRW++ = 0x90;
            FALLTHROUGH;
        case 11:
            *dstRW++ = 0x90;
            FALLTHROUGH;
        case 10:
            *dstRW++ = 0x90;
            FALLTHROUGH;
        case 9:
            *dstRW++ = 0x90;
            FALLTHROUGH;
        case 8:
            *dstRW++ = 0x90;
            FALLTHROUGH;
        case 7:
            *dstRW++ = 0x90;
            FALLTHROUGH;
        case 6:
            *dstRW++ = 0x90;
            FALLTHROUGH;
        case 5:
            *dstRW++ = 0x90;
            FALLTHROUGH;
        case 4:
            *dstRW++ = 0x90;
            FALLTHROUGH;
        case 3:
            *dstRW++ = 0x90;
            FALLTHROUGH;
        case 2:
            *dstRW++ = 0x90;
            FALLTHROUGH;
        case 1:
            *dstRW++ = 0x90;
            break;
        case 0:
            break;
    }
#else  // TARGET_AMD64
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
            dstRW = emitOutputNOP(emitOutputNOP(dstRW, 7), 8);
            break;
        case 14:
            // More than 3 prefixes is slower than just 2 NOPs
            dstRW = emitOutputNOP(emitOutputNOP(dstRW, 7), 7);
            break;
        case 13:
            // More than 3 prefixes is slower than just 2 NOPs
            dstRW = emitOutputNOP(emitOutputNOP(dstRW, 5), 8);
            break;
        case 12:
            // More than 3 prefixes is slower than just 2 NOPs
            dstRW = emitOutputNOP(emitOutputNOP(dstRW, 4), 8);
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

uint8_t* emitter::emitOutputAlign(insGroup* ig, instrDesc* id, uint8_t* dst)
{
    assert(codeGen->ShouldAlignLoops());

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
    assert((paddingToAdd == 0) || (((size_t)dst & (emitComp->opts.compJitAlignLoopBoundary - 1)) != 0));

    // Padding amount should not exceed the alignment boundary
    assert(0 <= paddingToAdd && paddingToAdd < emitComp->opts.compJitAlignLoopBoundary);

#ifdef DEBUG
    unsigned paddingNeeded = emitCalculatePaddingForLoopAlignment(ig, (size_t)dst, true);

    // For non-adaptive, padding size is spread in multiple instructions, so don't bother checking
    if (emitComp->opts.compJitAlignLoopAdaptive)
    {
        assert(paddingToAdd == paddingNeeded);
    }

    emitComp->loopsAligned++;
#endif

    uint8_t* dstRW = dst + writeableOffset;
    dstRW          = emitOutputNOP(dstRW, paddingToAdd);
    return dstRW - writeableOffset;
}

uint8_t* emitter::emitOutputAM(uint8_t* dst, instrDesc* id, code_t code, ssize_t* imm)
{
    instruction ins      = id->idIns();
    regNumber   baseReg  = id->idAddr()->iiaAddrMode.amBaseReg;
    regNumber   indexReg = id->idAddr()->iiaAddrMode.amIndxReg;
    ssize_t     disp;
    unsigned    immSize;

    // BT/CMOV support 16 bit operands and this code doesn't add the necessary 66 prefix.
    // BT with memory operands is practically useless and CMOV is not currently generated.
    assert((ins != INS_bt) && !insIsCMOV(ins));

    if (ins == INS_call)
    {
#ifdef TARGET_AMD64
        if (IsExtendedReg(baseReg))
        {
            code = AddRexBPrefix(ins, code);
        }

        if (IsExtendedReg(indexReg))
        {
            code = AddRexXPrefix(ins, code);
        }

        dst += emitOutputRexPrefixIfNeeded(ins, dst, code);
#endif // TARGET_AMD64

        // The displacement field is in an unusual place for calls
        disp = emitGetInsCallDisp(id);
        // Calls don't have an immediate operand and the instruction size indicates the size of
        // the return value, the operand size of the call is always 32/64 bit. Just set this to
        // keep the compiler from complaining about it being uninitialized.
        immSize = 0;
    }
    else
    {
        emitAttr size = id->idOpSize();
        immSize       = EA_SIZE_IN_BYTES(size);

        if (imm != nullptr)
        {
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

        // Some callers add the VEX prefix and call this routine, add it only if it's not already present.
        if (TakesVexPrefix(ins) && !hasVexPrefix(code))
        {
            code = AddVexPrefix(ins, code, size);
        }

#ifdef TARGET_AMD64
        if (IsExtendedReg(baseReg))
        {
            code = AddRexBPrefix(ins, code);
        }

        if (IsExtendedReg(indexReg))
        {
            code = AddRexXPrefix(ins, code);
        }
#endif

        if (TakesRexWPrefix(ins, size))
        {
            code = AddRexWPrefix(ins, code);
        }

        if (IsSSE38orSSE3A(code) || (ins == INS_crc32))
        {
            if (ins == INS_crc32)
            {
                if (size == EA_1BYTE)
                {
                    code ^= 0x0100;
                }
                else if (size == EA_2BYTE)
                {
                    dst += emitOutputByte(dst, 0x66);
                }
            }

            regNumber reg345;

            if (IsBMIRegExtInstruction(ins))
            {
                reg345 = static_cast<regNumber>(GetBMIOpcodeRMExt(ins));
            }
            else if (id->idInsFmt() == IF_AWR_RRD_RRD)
            {
                reg345 = id->idReg2();
            }
            else
            {
                reg345 = id->idReg1();
            }

            unsigned regcode = insEncodeReg345(ins, reg345, size, &code);
            dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

            if (UseVEXEncoding() && (ins != INS_crc32))
            {
                // Emit last opcode byte
                // TODO-XArch-CQ: Right now support 4-byte opcode instructions only
                assert((code & 0xFF) == 0);
            }
            else
            {
                dst += emitOutputWord(dst, code >> 16);
                dst += emitOutputByte(dst, code);
            }

            code = ((code >> 8) & 0xFF) | (regcode << 8);
        }
        else if (hasVexPrefix(code))
        {
            dst += emitOutputVexPrefix(ins, dst, code);
        }
#ifdef TARGET_X86
        else if (instIsFP(ins))
        {
            assert(size == EA_4BYTE || size == EA_8BYTE);

            if (size == EA_8BYTE)
            {
                code += 4;
            }
        }
#endif
        else
        {
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
                    assert(!IsSSEOrAVXInstruction(ins));
                    dst += emitOutputByte(dst, 0x66);
                }
            }
            else
            {
                assert((size == EA_4BYTE) || IsSSEOrAVXInstruction(ins) AMD64_ONLY(|| size == EA_8BYTE));
            }

            dst += emitOutputRexPrefixIfNeeded(ins, dst, code);

            if ((code & 0xFF000000) != 0)
            {
                dst += emitOutputWord(dst, code >> 16);
            }
            else if ((code & 0x00FF0000) != 0)
            {
                dst += emitOutputByte(dst, code >> 16);
            }
        }

        disp = emitGetInsAmdDisp(id);
    }

    if ((indexReg == REG_NA) && (baseReg == REG_NA))
    {
        if (!id->idIsDspReloc())
        {
#ifdef TARGET_AMD64
            noway_assert(!emitComp->opts.compReloc);
            noway_assert(!emitComp->eeIsRIPRelativeAddress(reinterpret_cast<void*>(disp)));
            noway_assert(IsDisp32(disp));

            dst += emitOutputWord(dst, code | 0x0400);
            dst += emitOutputByte(dst, 0x25);
#else
            dst += emitOutputWord(dst, code | 0x0500);
#endif

            dst += emitOutputLong(dst, disp);
        }
        else
        {
            dst += emitOutputWord(dst, code | 0x0500);

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
            // We emit zero on x64, to avoid the assert in emitOutputLong
            dst += emitOutputLong(dst, 0);
            emitRecordRelocation(dst - 4, reinterpret_cast<void*>(disp), IMAGE_REL_BASED_REL32, immRelocDelta);
#else
            dst += emitOutputLong(dst, disp);
            emitRecordRelocation(dst - 4, reinterpret_cast<void*>(disp), IMAGE_REL_BASED_HIGHLOW, immRelocDelta);
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

            dst += emitOutputWord(dst, code);

            if (BaseRegRequiresSIB(baseReg))
            {
                dst += emitOutputByte(dst, 0x24);
            }
        }
        else
        {
            unsigned scale = id->idAddr()->iiaAddrMode.amScale;

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

            dst += emitOutputWord(dst, code);
            dst += emitOutputByte(dst, RegEncoding(baseReg) | (RegEncoding(indexReg) << 3) | (scale << 6));
        }

        if (hasDisp)
        {
            if (hasDisp8)
            {
                dst += emitOutputByte(dst, disp);
            }
            else
            {
                dst += emitOutputLong(dst, disp);

                if (id->idIsDspReloc())
                {
                    // We don't expect something like "mov dword ptr [rax*4+reloc], 42"
                    // so this code doesn't attempt to account for the imm reloc delta.
                    assert(imm == nullptr);

                    emitRecordRelocation(dst - 4, reinterpret_cast<void*>(disp), IMAGE_REL_BASED_HIGHLOW);
                }
            }
        }
    }

    if (imm != nullptr)
    {
        dst += emitOutputImm(dst, id, immSize, *imm);
    }

    if (id->idGCref())
    {
        switch (id->idInsFmt())
        {
            case IF_RWR_ARD:
                emitGCregLiveUpd(id->idGCref(), id->idReg1(), dst);
                break;
            case IF_RRW_ARD:
                assert(id->idGCref() == GCT_BYREF && (ins == INS_add || ins == INS_sub));
                emitGCregLiveUpd(GCT_BYREF, id->idReg1(), dst);
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
                INDEBUG(emitDispIns(id));
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
                    emitGCregDeadUpd(id->idReg1(), dst);
                }
                break;
            case IF_ARD:
                if ((ins == INS_mulEAX) || (ins == INS_imulEAX))
                {
                    emitGCregDeadUpd(REG_EAX, dst);
                    emitGCregDeadUpd(REG_EDX, dst);
                }
                break;
            default:
                assert((ins != INS_mulEAX) && (ins != INS_imulEAX));
                break;
        }
    }

    return dst;
}

uint8_t* emitter::emitOutputSV(uint8_t* dst, instrDesc* id, code_t code, ssize_t* imm)
{
    instruction ins     = id->idIns();
    emitAttr    size    = id->idOpSize();
    unsigned    immSize = EA_SIZE_IN_BYTES(size);

    assert(ins != INS_imul || id->idReg1() == REG_EAX || size == EA_4BYTE || size == EA_8BYTE);
    // BT/CMOV support 16 bit operands and this code doesn't add the necessary 66 prefix.
    // BT with memory operands is practically useless and CMOV is not currently generated.
    assert((ins != INS_bt) && !insIsCMOV(ins));

    if (imm != nullptr)
    {
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

    // Some callers add the VEX prefix and call this routine, add it only if it's not already present.
    if (TakesVexPrefix(ins) && !hasVexPrefix(code))
    {
        code = AddVexPrefix(ins, code, size);
    }

    if (TakesRexWPrefix(ins, size))
    {
        code = AddRexWPrefix(ins, code);
    }

    if (IsSSE38orSSE3A(code) || (ins == INS_crc32))
    {
        if (ins == INS_crc32)
        {
            if (size == EA_1BYTE)
            {
                code ^= 0x0100;
            }
            else if (size == EA_2BYTE)
            {
                dst += emitOutputByte(dst, 0x66);
            }
        }

        regNumber reg345;

        if (IsBMIRegExtInstruction(ins))
        {
            reg345 = static_cast<regNumber>(GetBMIOpcodeRMExt(ins));
        }
        // else if (id->idInsFmt() == IF_AWR_RRD_RRD)
        //{
        //    reg345 = id->idReg2();
        //}
        else
        {
            reg345 = id->idReg1();
        }

        unsigned regcode = insEncodeReg345(ins, reg345, size, &code);
        dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

        if (UseVEXEncoding() && (ins != INS_crc32))
        {
            // Emit last opcode byte
            // TODO-XArch-CQ: Right now support 4-byte opcode instructions only
            assert((code & 0xFF) == 0);
        }
        else
        {
            dst += emitOutputWord(dst, code >> 16);
            dst += emitOutputByte(dst, code);
        }

        code = ((code >> 8) & 0xFF) | (regcode << 8);
    }
    else if (hasVexPrefix(code))
    {
        dst += emitOutputVexPrefix(ins, dst, code);
    }
#ifdef TARGET_X86
    else if (instIsFP(ins))
    {
        assert(size == EA_4BYTE || size == EA_8BYTE);

        if (size == EA_8BYTE)
        {
            code += 4;
        }
    }
#endif
    else
    {
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
                assert(!IsSSEOrAVXInstruction(ins));
                dst += emitOutputByte(dst, 0x66);
            }
        }
        else
        {
            assert((size == EA_4BYTE) || IsSSEOrAVXInstruction(ins) AMD64_ONLY(|| size == EA_8BYTE));
        }

        dst += emitOutputRexPrefixIfNeeded(ins, dst, code);

        if ((code & 0xFF000000) != 0)
        {
            dst += emitOutputWord(dst, code >> 16);
        }
        else if ((code & 0x00FF0000) != 0)
        {
            dst += emitOutputByte(dst, code >> 16);
        }
    }

    assert(!id->idIsDspReloc());

    bool ebpBased  = id->idAddr()->isEbpBased;
    int  lclOffset = id->idAddr()->lclOffset;
    int  disp      = lclOffset;

#if !FEATURE_FIXED_OUT_ARGS
    if (!ebpBased)
    {
        disp += emitCurStackLvl;
    }
#endif

    bool hasDisp8 = IsDisp8(disp);
    code |= ebpBased ? 0x0500 : 0x0400;

    if ((disp != 0) || ebpBased)
    {
        code |= hasDisp8 ? 0x4000 : 0x8000;
    }

    dst += emitOutputWord(dst, code);

    if (!ebpBased)
    {
        dst += emitOutputByte(dst, 0x24);
    }

    if ((disp != 0) || ebpBased)
    {
        if (hasDisp8)
        {
            dst += emitOutputByte(dst, disp);
        }
        else
        {
            dst += emitOutputLong(dst, disp);
        }
    }

    if (imm != nullptr)
    {
        dst += emitOutputImm(dst, id, immSize, *imm);
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
            emitGCargLiveUpd(lclOffset, id->idGCref(), dst DEBUGARG(varNum));
        }
        else
#endif
        {
            emitGCvarLiveUpd(lclOffset, id->idGCref(), dst DEBUGARG(varNum));
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
                emitGCregLiveUpd(id->idGCref(), id->idReg1(), dst);
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
                INDEBUG(emitDispIns(id));
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
                    emitGCregDeadUpd(id->idReg1(), dst);
                }
                break;
            case IF_SRD:
                if ((ins == INS_mulEAX) || (ins == INS_imulEAX))
                {
                    emitGCregDeadUpd(REG_EAX, dst);
                    emitGCregDeadUpd(REG_EDX, dst);
                }
                break;
            default:
                assert((ins != INS_mulEAX) && (ins != INS_imulEAX));
                break;
        }
    }

    return dst;
}

uint8_t* emitter::emitOutputCV(uint8_t* dst, instrDesc* id, code_t code, ssize_t* imm)
{
    instruction          ins     = id->idIns();
    emitAttr             size    = id->idOpSize();
    unsigned             immSize = EA_SIZE_IN_BYTES(size);
    CORINFO_FIELD_HANDLE field   = id->idAddr()->iiaFieldHnd;
    ssize_t              disp    = emitGetInsMemDisp(id);

    // BT/CMOV support 16 bit operands and this code doesn't add the necessary 66 prefix.
    // BT with memory operands is practically useless and CMOV is not currently generated.
    assert((ins != INS_bt) && !insIsCMOV(ins));

    if (IsRoDataField(field))
    {
        size_t addr = reinterpret_cast<size_t>(emitConsBlock) + GetRoDataOffset(field);

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
                align = EA_SIZE_IN_BYTES(size);
                break;
        }

        // Check that the offset is properly aligned (i.e. the ddd in [ddd])
        // When SMALL_CODE is set, we only expect 4-byte alignment, otherwise
        // we expect the same alignment as the size of the constant.
        // TODO-MIKE-Review: Figure out why this check is disabled in crossgen.
        assert(((addr & (align - 1)) == 0) || ((emitComp->compCodeOpt() == SMALL_CODE) && ((addr & 3) == 0)) ||
               emitComp->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT));
#endif // DEBUG

        disp += addr;
    }
#ifdef WINDOWS_X86_ABI
    else if (field == FS_SEG_FIELD)
    {
        dst += emitOutputByte(dst, 0x64);
    }
#endif
    else
    {
        void* addr = static_cast<uint8_t*>(emitComp->info.compCompHnd->getFieldAddress(field, nullptr));
        noway_assert(addr != nullptr);
        disp += reinterpret_cast<ssize_t>(addr);
    }

#ifdef TARGET_X86
    // Special case: "mov eax, [addr]" and "mov [addr], eax" have smaller encoding.
    // x64 currently never uses the moffset format, it uses only RIP relative addressing.
    if ((ins == INS_mov) && (id->idReg1() == REG_EAX) && (imm == nullptr))
    {
        assert((id->idInsFmt() == IF_RWR_MRD) || (id->idInsFmt() == IF_MWR_RRD));

        if (size == EA_2BYTE)
        {
            dst += emitOutputByte(dst, 0x66);
        }

        dst += emitOutputByte(dst, 0xA0 | ((id->idInsFmt() == IF_MWR_RRD) << 1) | (size != EA_1BYTE));
    }
    else
#endif // TARGET_X86
    {
        if (imm != nullptr)
        {
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

        // Some callers add the VEX prefix and call this routine, add it only if it's not already present.
        if (TakesVexPrefix(ins) && !hasVexPrefix(code))
        {
            code = AddVexPrefix(ins, code, size);
        }

        if (TakesRexWPrefix(ins, size))
        {
            code = AddRexWPrefix(ins, code);
        }

        if (IsSSE38orSSE3A(code) || (ins == INS_crc32))
        {
            if (ins == INS_crc32)
            {
                if (size == EA_1BYTE)
                {
                    code ^= 0x0100;
                }
                else if (size == EA_2BYTE)
                {
                    dst += emitOutputByte(dst, 0x66);
                }
            }

            regNumber reg345;

            if (IsBMIRegExtInstruction(ins))
            {
                reg345 = static_cast<regNumber>(GetBMIOpcodeRMExt(ins));
            }
            // else if (id->idInsFmt() == IF_AWR_RRD_RRD)
            //{
            //    reg345 = id->idReg2();
            //}
            else
            {
                reg345 = id->idReg1();
            }

            unsigned regcode = insEncodeReg345(ins, reg345, size, &code);
            dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

            if (UseVEXEncoding() && (ins != INS_crc32))
            {
                // Emit last opcode byte
                // TODO-XArch-CQ: Right now support 4-byte opcode instructions only
                assert((code & 0xFF) == 0);
            }
            else
            {
                dst += emitOutputWord(dst, code >> 16);
                dst += emitOutputByte(dst, code);
            }

            code = ((code >> 8) & 0xFF) | (regcode << 8);
        }
        else if (hasVexPrefix(code))
        {
            dst += emitOutputVexPrefix(ins, dst, code);
        }
#ifdef TARGET_X86
        else if (instIsFP(ins))
        {
            assert(size == EA_4BYTE || size == EA_8BYTE);

            if (size == EA_8BYTE)
            {
                code += 4;
            }
        }
#endif
        else
        {
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
                    assert(!IsSSEOrAVXInstruction(ins));
                    dst += emitOutputByte(dst, 0x66);
                }
            }
            else
            {
                assert((size == EA_4BYTE) || IsSSEOrAVXInstruction(ins) AMD64_ONLY(|| size == EA_8BYTE));
            }

            dst += emitOutputRexPrefixIfNeeded(ins, dst, code);

            if ((code & 0xFF000000) != 0)
            {
                dst += emitOutputWord(dst, code >> 16);
            }
            else if ((code & 0x00FF0000) != 0)
            {
                dst += emitOutputByte(dst, code >> 16);
            }
        }

        dst += emitOutputWord(dst, code | 0x0500);
    }

    if (!id->idIsDspReloc())
    {
#ifdef TARGET_AMD64
        // All static field and data section constant accesses should be marked as relocatable
        unreached();
#else
        dst += emitOutputLong(dst, disp);
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
        // We emit zero on x64, to avoid the assert in emitOutputLong
        dst += emitOutputLong(dst, 0);
        emitRecordRelocation(dst - 4, reinterpret_cast<void*>(disp), IMAGE_REL_BASED_REL32, immRelocDelta);
#else
        dst += emitOutputLong(dst, disp);
        emitRecordRelocation(dst - 4, reinterpret_cast<void*>(disp), IMAGE_REL_BASED_HIGHLOW, immRelocDelta);
#endif
    }

    if (imm != nullptr)
    {
        dst += emitOutputImm(dst, id, immSize, *imm);
    }

    if (id->idGCref())
    {
        switch (id->idInsFmt())
        {
            case IF_RWR_MRD:
                emitGCregLiveUpd(id->idGCref(), id->idReg1(), dst);
                break;
            case IF_RRW_MRD:
                assert(id->idGCref() == GCT_BYREF);
                assert((ins == INS_add) || (ins == INS_sub));
                emitGCregLiveUpd(GCT_BYREF, id->idReg1(), dst);
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
                INDEBUG(emitDispIns(id));
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
                    emitGCregDeadUpd(id->idReg1(), dst);
                }
                break;
            case IF_MRD:
                if ((ins == INS_mulEAX) || (ins == INS_imulEAX))
                {
                    emitGCregDeadUpd(REG_EAX, dst);
                    emitGCregDeadUpd(REG_EDX, dst);
                }
                break;
            default:
                assert((ins != INS_mulEAX) && (ins != INS_imulEAX));
                break;
        }
    }

    return dst;
}

uint8_t* emitter::emitOutputR(uint8_t* dst, instrDesc* id)
{
    instruction ins  = id->idIns();
    regNumber   reg  = id->idReg1();
    emitAttr    size = id->idOpSize();

    switch (ins)
    {
        code_t code;

        case INS_call:
            code = insEncodeRMreg(INS_call, reg, EA_PTRSIZE, insCodeMR(INS_call));
            dst += emitOutputRexPrefixIfNeeded(ins, dst, code);
            dst += emitOutputWord(dst, code);
            // Calls use a different mechanism to update GC info so we can skip the normal handling.
            return dst;

        case INS_inc:
        case INS_dec:
            if (size == EA_2BYTE)
            {
                dst += emitOutputByte(dst, 0x66);
            }

#ifdef TARGET_X86
            if (size != EA_1BYTE)
            {
                code = insCodeRR(ins == INS_inc ? INS_inc_s : INS_dec_s);
                code |= RegEncoding(reg);
                dst += emitOutputByte(dst, code);

                break;
            }
#endif

            code = insCodeMR(ins);

            if (size == EA_1BYTE)
            {
                assert(HasWBit(ins) && ((code & 1) != 0));
                code ^= 1;
            }
#ifdef TARGET_AMD64
            else if (size == EA_8BYTE)
            {
                code = AddRexWPrefix(ins, code);
            }
#endif

            code |= (0xC0ull | insEncodeReg012(ins, reg, size, &code)) << 8;
            dst += emitOutputRexPrefixIfNeeded(ins, dst, code);
            dst += emitOutputWord(dst, code);
            break;

        case INS_pop:
        case INS_pop_hide:
        case INS_push:
        case INS_push_hide:
            assert(size == EA_PTRSIZE);
            assert(!TakesVexPrefix(ins));
            assert(!TakesRexWPrefix(ins, size));

            code = insCodeRR(ins);
            code |= insEncodeReg012(ins, reg, size, &code);
            dst += emitOutputRexPrefixIfNeeded(ins, dst, code);
            dst += emitOutputByte(dst, code);
            break;

        case INS_bswap:
            assert(size >= EA_4BYTE && size <= EA_PTRSIZE); // 16-bit BSWAP is undefined

            code = insCodeRR(ins);

            if (size == EA_8BYTE)
            {
                code = AddRexWPrefix(ins, code);
            }

            // The Intel instruction set reference for BSWAP states that extended registers
            // should be enabled via REX.R, but per Vol. 2A, Sec. 2.2.1.2 (see also Figure 2-7),
            // REX.B should instead be used if the register is encoded in the opcode byte itself.
            // Therefore the default logic of insEncodeReg012 is correct for this case.
            code |= insEncodeReg012(ins, reg, size, &code) << 8;
            dst += emitOutputRexPrefixIfNeeded(ins, dst, code);
            dst += emitOutputWord(dst, code);
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

            code = insEncodeRMreg(ins, reg, EA_1BYTE, insCodeMR(ins));
            assert(code & 0x00FF0000);
            dst += emitOutputRexPrefixIfNeeded(ins, dst, code);
            dst += emitOutputByte(dst, code >> 16);
            dst += emitOutputWord(dst, code & 0x0000FFFF);
            break;

        case INS_mulEAX:
        case INS_imulEAX:
            emitGCregDeadUpd(REG_EAX, dst);
            emitGCregDeadUpd(REG_EDX, dst);
            FALLTHROUGH;
        default:
            assert(!IsSSEOrAVXInstruction(ins));
            assert(!TakesVexPrefix(ins));
            assert(id->idGCref() == GCT_NONE);

            code = insEncodeRMreg(ins, reg, size, insCodeMR(ins));

            if (size == EA_1BYTE)
            {
                assert(HasWBit(ins) && ((code & 1) != 0));
                code ^= 1;
            }
            else if (size == EA_2BYTE)
            {
                dst += emitOutputByte(dst, 0x66);
            }

            if (TakesRexWPrefix(ins, size))
            {
                code = AddRexWPrefix(ins, code);
            }

            dst += emitOutputRexPrefixIfNeeded(ins, dst, code);
            dst += emitOutputWord(dst, code);
            break;
    }

    switch (id->idInsFmt())
    {
        case IF_RWR:
            if (id->idGCref())
            {
                emitGCregLiveUpd(id->idGCref(), id->idReg1(), dst);
            }
            else
            {
                emitGCregDeadUpd(id->idReg1(), dst);
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

                emitGCregLiveUpd(GCT_BYREF, id->idReg1(), dst);
            }
            else
            {
                assert((gcInfo.GetLiveRegs(GCT_GCREF) & genRegMask(reg)) == RBM_NONE);
            }
            break;

        case IF_RRD:
            break;
        default:
            INDEBUG(emitDispIns(id));
            assert(!"unexpected instruction format");
            break;
    }

    return dst;
}

uint8_t* emitter::emitOutputRR(uint8_t* dst, instrDesc* id)
{
    instruction ins  = id->idIns();
    regNumber   reg1 = id->idReg1();
    regNumber   reg2 = id->idReg2();
    emitAttr    size = id->idOpSize();
    code_t      code;
    regNumber   reg012 = reg2;
    regNumber   reg345 = reg1;

    if (IsSSEOrAVXInstruction(ins))
    {
        assert((ins != INS_movd) || (IsFloatReg(reg1) != IsFloatReg(reg2)));

        if ((ins != INS_movd) || IsFloatReg(reg1))
        {
            code = insCodeRM(ins);
        }
        else
        {
            code = insCodeMR(ins);
        }

        code = AddVexPrefixIfNeeded(ins, code, size);

        if (TakesRexWPrefix(ins, size))
        {
            code = AddRexWPrefix(ins, code);
        }

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
                code = SetVexVvvv(ins, reg1, size, code);
            }
            else if (IsVexDstSrcSrc(ins))
            {
                code = SetVexVvvv(ins, reg2, size, code);
            }
        }

        if (IsBMIRegExtInstruction(ins))
        {
            reg345 = static_cast<regNumber>(GetBMIOpcodeRMExt(ins));
        }
        else if (ins == INS_movd)
        {
            assert(IsFloatReg(reg1) != IsFloatReg(reg2));

            if (IsFloatReg(reg2))
            {
                std::swap(reg012, reg345);
            }
        }
    }
    else if ((ins == INS_movsx) || (ins == INS_movzx) || insIsCMOV(ins))
    {
        assert(!hasCodeMI(ins) && !hasCodeMR(ins));

        code = insCodeRM(ins);

        if (size == EA_1BYTE)
        {
            assert(HasWBit(ins) && ((code & 1) != 0));
            code ^= 1;
        }

#ifdef TARGET_AMD64
        assert((size < EA_4BYTE) || insIsCMOV(ins));

        if ((size == EA_8BYTE) || (ins == INS_movsx))
        {
            code = AddRexWPrefix(ins, code);
        }
#endif
    }
#ifdef TARGET_AMD64
    else if (ins == INS_movsxd)
    {
        assert(!hasCodeMI(ins) && !hasCodeMR(ins));

        code = insCodeRM(ins);
        code = AddRexWPrefix(ins, code);
    }
#endif
    else if (ins == INS_imul)
    {
        assert(!TakesVexPrefix(ins));
        assert(size != EA_1BYTE);

        code = insCodeRM(ins);

        // TODO-MIKE-Cleanup: There should be no need to generate a 16 bit imul.
        if (size == EA_2BYTE)
        {
            dst += emitOutputByte(dst, 0x66);
        }
#ifdef TARGET_AMD64
        else if (size == EA_8BYTE)
        {
            code = AddRexWPrefix(ins, code);
        }
#endif
    }
    else if (ins == INS_bt)
    {
        assert(!TakesVexPrefix(ins));
        assert(size != EA_1BYTE);

        code = insCodeMR(ins);

        // TODO-MIKE-Cleanup: There should be no need to generate a 16 bit bt.
        if (size == EA_2BYTE)
        {
            dst += emitOutputByte(dst, 0x66);
        }
#ifdef TARGET_AMD64
        else if (size == EA_8BYTE)
        {
            code = AddRexWPrefix(ins, code);
        }
#endif

        std::swap(reg345, reg012);
    }
    else if ((ins == INS_bsf) || (ins == INS_bsr) || (ins == INS_crc32) || (ins == INS_lzcnt) || (ins == INS_popcnt) ||
             (ins == INS_tzcnt))
    {
        assert(!hasCodeMI(ins) && !hasCodeMR(ins) && !TakesVexPrefix(ins));

        code = insCodeRM(ins);

        if (size == EA_1BYTE)
        {
            noway_assert(ins == INS_crc32);
            code ^= 0x0100;
        }
        else if (size == EA_2BYTE)
        {
            dst += emitOutputByte(dst, 0x66);
        }
#ifdef TARGET_AMD64
        else if (size == EA_8BYTE)
        {
            code = AddRexWPrefix(ins, code);
        }
#endif
    }
    else
    {
        assert(!TakesVexPrefix(ins));

        code = insCodeMR(ins);

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
                dst += emitOutputByte(dst, 0x66);
                break;
            case EA_4BYTE:
                break;
#ifdef TARGET_AMD64
            case EA_8BYTE:
                // TODO-AMD64-CQ: Better way to not emit REX.W when we don't need it
                // Don't need to zero out the high bits explicitly
                if ((ins != INS_xor) || (reg1 != reg2))
                {
                    code = AddRexWPrefix(ins, code);
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

    code_t regCode = 0xC0 | insEncodeReg345(ins, reg345, size, &code) | insEncodeReg012(ins, reg012, size, &code);

    if (IsSSE38orSSE3A(code) || (ins == INS_crc32))
    {
        dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

        if (UseVEXEncoding() && (ins != INS_crc32))
        {
            // Emit last opcode byte
            // TODO-XArch-CQ: Right now support 4-byte opcode instructions only
            assert((code & 0xFF) == 0);
        }
        else
        {
            dst += emitOutputWord(dst, code >> 16);
            dst += emitOutputByte(dst, code);
        }

        code = (code >> 8) & 0xFF;
    }
    else if (hasVexPrefix(code))
    {
        dst += emitOutputVexPrefix(ins, dst, code);
    }
    else
    {
        dst += emitOutputRexPrefixIfNeeded(ins, dst, code);

        if ((code & 0xFF000000) != 0)
        {
            dst += emitOutputWord(dst, code >> 16);
        }
        else if ((code & 0x00FF0000) != 0)
        {
            dst += emitOutputByte(dst, code >> 16);
        }
    }

    assert((code & 0xFF00) == 0);

    dst += emitOutputWord(dst, code | (regCode << 8));

    if (id->idGCref())
    {
        switch (id->idInsFmt())
        {
            case IF_RWR_RRD:
                emitGCregLiveUpd(id->idGCref(), reg1, dst);
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
                        emitGCregLiveUpd(id->idGCref(), reg1, dst);
                        break;
                    case INS_or:
                    case INS_and:
                        emitGCregDeadUpd(reg1, dst);
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
                        // generated, and we only (currently) make gcref/byref changes in emitter GC info
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
                        emitGCregLiveUpd(GCT_BYREF, reg1, dst);
                        break;

                    default:
                        INDEBUG(emitDispIns(id));
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
                        emitGCregDeadUpd(reg1, dst);
                    }

                    if (gc2 != GCT_NONE)
                    {
                        emitGCregDeadUpd(reg2, dst);
                    }

                    if (gc1 != GCT_NONE)
                    {
                        emitGCregLiveUpd(gc1, reg2, dst);
                    }

                    if (gc2 != GCT_NONE)
                    {
                        emitGCregLiveUpd(gc2, reg1, dst);
                    }
                }
                break;

            case IF_RRD_RRD:
                break;

            default:
                INDEBUG(emitDispIns(id));
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
                    emitGCregDeadUpd(reg1, dst);
                }
                break;
            default:
                assert((ins != INS_mulEAX) && (ins != INS_imulEAX) && (ins != INS_imuli));
                break;
        }
    }

    return dst;
}

uint8_t* emitter::emitOutputRRR(uint8_t* dst, instrDesc* id)
{
    assert(IsVexTernary(id->idIns()));
    assert((id->idInsFmt() == IF_RWR_RRD_RRD) || (id->idInsFmt() == IF_RWR_RRD_RRD_CNS) ||
           (id->idInsFmt() == IF_RWR_RRD_RRD_RRD));

    instruction ins  = id->idIns();
    regNumber   reg1 = id->idReg1();
    regNumber   reg2 = id->idReg2();
    regNumber   reg3 = id->idReg3();
    emitAttr    size = id->idOpSize();

    code_t code = insCodeRM(ins);

    code = AddVexPrefix(ins, code, size);

    if (TakesRexWPrefix(ins, size))
    {
        code = AddRexWPrefix(ins, code);
    }

    code_t rmCode = 0xC0 | insEncodeReg345(ins, reg1, size, &code) | insEncodeReg012(ins, reg3, size, &code);
    code          = SetVexVvvv(ins, reg2, size, code);

    bool is4ByteOpcode = IsSSE38orSSE3A(code);
    dst += emitOutputVexPrefix(ins, dst, code);

    if (is4ByteOpcode)
    {
        code = (code >> 8) & 0xFF;
    }

    assert((code & 0xFF00) == 0);

    dst += emitOutputWord(dst, code | (rmCode << 8));

    if (IsGeneralRegister(id->idReg1()))
    {
        noway_assert(!id->idGCref());
        emitGCregDeadUpd(id->idReg1(), dst);
    }

    return dst;
}

uint8_t* emitter::emitOutputRRI(uint8_t* dst, instrDesc* id)
{
    instruction ins  = id->idIns();
    emitAttr    size = id->idOpSize();

    // Get the 'base' opcode (it's a big one)
    // Also, determine which operand goes where in the ModRM byte.
    regNumber mReg;
    regNumber rReg;
    code_t    code;

    if (hasCodeMR(ins))
    {
        code = insCodeMR(ins);
        code = AddVexPrefixIfNeeded(ins, code, size);

        mReg = id->idReg1();
        rReg = id->idReg2();
    }
    else if (hasCodeMI(ins))
    {
        assert((INS_psrldq <= ins) && (ins <= INS_psrad) && UseVEXEncoding());

        code = insCodeMI(ins);
        code = AddVexPrefixIfNeeded(ins, code, size);

        mReg = id->idReg2();
        rReg = static_cast<regNumber>((code >> 11) & 7);
    }
    else
    {
        code = insCodeRM(ins);
        code = AddVexPrefixIfNeeded(ins, code, size);

        mReg = id->idReg2();
        rReg = id->idReg1();

        if (ins == INS_imuli)
        {
            if (id->idOpSize() > EA_1BYTE)
            {
                // Don't bother with 16 bit imul, which needs the 66 prefix, we only need 32/64 bit.
                assert(id->idOpSize() != EA_2BYTE);

                code |= 1;

                if (IsImm8(emitGetInsSC(id)))
                {
                    code |= 2;
                }
            }
        }
    }

    assert(((code & 0x00FF0000) != 0) || (id->idIns() == INS_imuli));

    if (TakesRexWPrefix(ins, size))
    {
        code = AddRexWPrefix(ins, code);
    }

    if (TakesVexPrefix(ins))
    {
        if (IsVexDstDstSrc(ins))
        {
            // This code will have to change when we support 3 operands.
            // For now, we always overload this source with the destination (always reg1).
            // (Though we will need to handle the few ops that can have the 'vvvv' bits as destination,
            // e.g. pslldq, when/if we support those instructions with 2 registers.)
            // (see x64 manual Table 2-9. Instructions with a VEX.vvvv destination)
            code = SetVexVvvv(ins, id->idReg1(), size, code);
        }
        else if (IsVexDstSrcSrc(ins))
        {
            // This is a "merge" move instruction.
            code = SetVexVvvv(ins, id->idReg2(), size, code);
        }
    }

    code_t rmCode = 0xC0 | insEncodeReg345(ins, rReg, size, &code) | insEncodeReg012(ins, mReg, size, &code);

    if (IsSSE38orSSE3A(code))
    {
        dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

        if (UseVEXEncoding())
        {
            // Emit last opcode byte
            // TODO-XArch-CQ: Right now support 4-byte opcode instructions only
            assert((code & 0xFF) == 0);
        }
        else
        {
            dst += emitOutputWord(dst, code >> 16);
            dst += emitOutputByte(dst, code);
        }

        code = (code >> 8) & 0xFF;
    }
    else if (hasVexPrefix(code))
    {
        dst += emitOutputVexPrefix(ins, dst, code);
    }
    else
    {
        dst += emitOutputRexPrefixIfNeeded(ins, dst, code);

        if ((code & 0xFF000000) != 0)
        {
            dst += emitOutputWord(dst, code >> 16);
        }
        else if ((code & 0x00FF0000) != 0)
        {
            dst += emitOutputByte(dst, code >> 16);
        }
    }

    assert(((code & 0xFF00) == 0) || ((INS_psrldq <= ins) && (ins <= INS_psrad)));

    dst += emitOutputWord(dst, code | (rmCode << 8));

    if ((ins == INS_imuli) && ((code & 0x02) == 0))
    {
        dst += emitOutputLong(dst, emitGetInsSC(id));
    }
    else
    {
        dst += emitOutputByte(dst, emitGetInsSC(id));
    }

    assert(id->idGCref() == GCT_NONE);

    if (IsGeneralRegister(id->idReg1()))
    {
        emitGCregDeadUpd(id->idReg1(), dst);
    }

    return dst;
}

uint8_t* emitter::emitOutputRI(uint8_t* dst, instrDesc* id)
{
    emitAttr    size = id->idOpSize();
    instruction ins  = id->idIns();
    regNumber   reg  = id->idReg1();
    ssize_t     imm  = emitGetInsSC(id);

    // BT reg,imm might be useful but it requires special handling of the immediate value
    // (it is always encoded in a byte). Let's not complicate things until this is needed.
    assert(ins != INS_bt);

    if (IsSSEOrAVXInstruction(ins))
    {
        assert((INS_psrldq <= ins) && (ins <= INS_psrad));
        assert(id->idGCref() == GCT_NONE);
        assert(IsImm8(imm));

        code_t code = insCodeMI(ins);
        assert(code & 0xFF000000);

        code = AddVexPrefixIfNeeded(ins, code, size);
        code = insEncodeMIreg(ins, reg, size, code);

        if (TakesVexPrefix(ins))
        {
            code = SetVexVvvv(ins, reg, size, code);
        }

        if (hasVexPrefix(code))
        {
            dst += emitOutputVexPrefix(ins, dst, code);
        }
        else
        {
            dst += emitOutputRexPrefixIfNeeded(ins, dst, code);

            if (code & 0xFF000000)
            {
                dst += emitOutputWord(dst, code >> 16);
            }
            else if (code & 0xFF0000)
            {
                dst += emitOutputByte(dst, code >> 16);
            }
        }

        dst += emitOutputWord(dst, code);
        dst += emitOutputByte(dst, imm);

        return dst;
    }

    noway_assert(emitVerifyEncodable(ins, size, reg));

    if (ins == INS_mov)
    {
        assert(id->idInsFmt() == IF_RWR_CNS);

        // TODO-MIKE-Cleanup: 0xB8 could go into the RR encoding table. But there
        // is little point in doing that since we need to special case this anyway.
        // Or perhaps there's a way to avoid this special casing?
        code_t code = 0xB8;
        code |= insEncodeReg012(ins, reg, size, &code);

        assert(!TakesVexPrefix(ins));

        if (TakesRexWPrefix(ins, size))
        {
            code = AddRexWPrefix(ins, code);
        }

        dst += emitOutputRexPrefixIfNeeded(ins, dst, code);
        dst += emitOutputByte(dst, code);

#ifdef TARGET_X86
        assert(size == EA_4BYTE);
        dst += emitOutputLong(dst, imm);

        if (id->idIsCnsReloc())
        {
            emitRecordRelocation(dst - 4, reinterpret_cast<void*>(imm), IMAGE_REL_BASED_HIGHLOW);
        }
#else
        if (size == EA_4BYTE)
        {
            dst += emitOutputLong(dst, imm);
            assert(!id->idIsCnsReloc());
        }
        else
        {
            assert(size == EA_8BYTE);
            dst += emitOutputI64(dst, imm);

            if (id->idIsCnsReloc())
            {
                emitRecordRelocation(dst - 8, reinterpret_cast<void*>(imm), IMAGE_REL_BASED_DIR64);
            }
        }
#endif

        // TODO-MIKE-Cleanup: A constant can't be a GC ref so we should not need to check idGCref.
        // But of course, it's messed up so constants are sometimes byrefs. Probably we could simply
        // ignore that and just call emitGCregDeadUpd, but we may get asserts later on because the
        // GC info is out of sync.

        if (id->idGCref())
        {
            emitGCregLiveUpd(id->idGCref(), reg, dst);
        }
        else
        {
            emitGCregDeadUpd(reg, dst);
        }

        return dst;
    }

    if (IsShiftImm(ins))
    {
        assert(!TakesVexPrefix(ins));

        code_t code = insEncodeRMreg(ins, id->idReg1(), size, insCodeMI(ins));

        if (size == EA_1BYTE)
        {
            assert(HasWBit(ins) && ((code & 1) != 0));
            code ^= 1;
        }
        else if (size == EA_2BYTE)
        {
            dst += emitOutputByte(dst, 0x66);
        }
#ifdef TARGET_AMD64
        else if (size == EA_8BYTE)
        {
            code = AddRexWPrefix(ins, code);
        }
#endif

        dst += emitOutputRexPrefixIfNeeded(ins, dst, code);
        dst += emitOutputWord(dst, code);
        dst += emitOutputByte(dst, emitGetInsSC(id));

        assert(!id->idGCref());
        emitGCregDeadUpd(id->idReg1(), dst);

        return dst;
    }

    // This has to be one of the instructions that have the special EAX,imm encodings.
    assert(insCodeACC(ins) != BAD_CODE);

    size_t immSize = EA_SIZE_IN_BYTES(size);
    bool   useImm8 = IsImm8(imm) && !id->idIsCnsReloc() && (size != EA_1BYTE) && (ins != INS_test);
    bool   useACC  = (reg == REG_EAX) && !useImm8;
    code_t code;

    if (useACC)
    {
        code = insCodeACC(ins);
    }
    else
    {
        code = insCodeMI(ins);
        code = insEncodeMIreg(ins, reg, size, code);

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
        dst += emitOutputByte(dst, 0x66);
    }
#ifdef TARGET_AMD64
    else if (size == EA_8BYTE)
    {
        code = AddRexWPrefix(ins, code);
    }
#endif

    dst += emitOutputRexPrefixIfNeeded(ins, dst, code);

    if (useACC)
    {
        dst += emitOutputByte(dst, code);
    }
    else
    {
        dst += emitOutputWord(dst, code);
    }

    dst += emitOutputImm(dst, id, immSize, imm);

    if (id->idGCref())
    {
        assert(ins != INS_mulEAX && ins != INS_imulEAX && ins != INS_imuli);

        switch (id->idInsFmt())
        {
            case IF_RWR_CNS:
                emitGCregLiveUpd(id->idGCref(), id->idReg1(), dst);
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
                emitGCregLiveUpd(GCT_BYREF, id->idReg1(), dst);
                break;

            case IF_RRD_CNS:
                break;
            default:
                INDEBUG(emitDispIns(id));
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
                emitGCregDeadUpd(id->idReg1(), dst);
                break;
            case IF_RRD_CNS:
                assert(ins != INS_mulEAX && ins != INS_imulEAX && ins != INS_imuli);
                break;
            default:
                INDEBUG(emitDispIns(id));
                assert(!"unexpected GC ref instruction format");
        }
    }

    return dst;
}

uint8_t* emitter::emitOutputIV(uint8_t* dst, instrDesc* id)
{
    instruction ins  = id->idIns();
    emitAttr    size = id->idOpSize();
    ssize_t     val  = emitGetInsSC(id);

#ifdef TARGET_AMD64
    assert((ins == INS_push_hide) && (size == EA_8BYTE) && (val == 0) && !id->idIsCnsReloc());
    dst += emitOutputWord(dst, 0x006A);
#else
    if (ins == INS_ret)
    {
        assert((val != 0) && !id->idIsCnsReloc());

        dst += emitOutputByte(dst, insCodeMI(ins));
        dst += emitOutputWord(dst, val);
    }
    else
    {
        assert((ins == INS_push) || (ins == INS_push_hide));

        code_t code = insCodeMI(ins);

        if (IsImm8(val) && !id->idIsCnsReloc())
        {
            dst += emitOutputByte(dst, code | 2);
            dst += emitOutputByte(dst, val);
        }
        else
        {
            dst += emitOutputByte(dst, code);
            dst += emitOutputLong(dst, val);

            if (id->idIsCnsReloc())
            {
                emitRecordRelocation(dst - 4, reinterpret_cast<void*>(val), IMAGE_REL_BASED_HIGHLOW);
            }
        }
    }
#endif // TARGET_X86

    return dst;
}

uint8_t* emitter::emitOutputRL(uint8_t* dst, instrDescJmp* id, insGroup* ig)
{
    assert(id->idIns() == INS_lea);
    assert(id->idInsFmt() == IF_RWR_LABEL);
    assert(id->idOpSize() == EA_PTRSIZE);
    assert(id->idIsDspReloc());
    assert(id->idGCref() == GCT_NONE);
    assert(!id->idAddr()->iiaHasInstrCount());
    assert(id->idIsBound());
    assert(id->idjKeepLong);
    assert(!id->idjShort);

    unsigned srcOffs = emitCurCodeOffs(dst);
    unsigned dstOffs = id->idAddr()->iiaIGlabel->igOffs;
    uint8_t* dstAddr = emitOffsetToPtr(dstOffs);

    // TODO-MIKE-CQ: For x86 this should really be mov, it can be more efficient.
    code_t code = 0x8D;
    assert(insCodeRM(INS_lea) == code);
    AMD64_ONLY(code = AddRexWPrefix(INS_lea, code));
    code = SetRMReg(INS_lea, id->idReg1(), EA_PTRSIZE, code);
    AMD64_ONLY(dst += emitOutputRexPrefix(INS_lea, dst, code));
    dst += emitOutputWord(dst, code | 0x0500);

    if (dstOffs > srcOffs)
    {
        dstAddr -= RecordForwardJump(id, srcOffs, dstOffs);
        id->idjAddr = dst;
    }

#ifdef TARGET_AMD64
    // TODO-MIKE-Cleanup: This shouldn't call recordRelocation, the target is within
    // this method so it's not like we'll need jump stubs or anything VM related.

    // We emit zero on x64, to avoid the assert in emitOutputLong
    dst += emitOutputLong(dst, 0);
    emitRecordRelocation(dst - 4, dstAddr, IMAGE_REL_BASED_REL32, 0);
#else
    dst += emitOutputLong(dst, reinterpret_cast<ssize_t>(dstAddr));
    emitRecordRelocation(dst - 4, dstAddr, IMAGE_REL_BASED_HIGHLOW, 0);
#endif

    emitGCregDeadUpd(id->idReg1(), dst);

    return dst;
}

#ifdef TARGET_X86
uint8_t* emitter::emitOutputL(uint8_t* dst, instrDescJmp* id, insGroup* ig)
{
    assert(id->idIns() == INS_push_hide);
    assert(id->idInsFmt() == IF_LABEL);
    assert(id->idGCref() == GCT_NONE);
    assert(id->idIsBound());
    assert(!id->idAddr()->iiaHasInstrCount());
    assert(id->idjKeepLong);
    assert(!id->idjShort);

    unsigned srcOffs = emitCurCodeOffs(dst);
    unsigned dstOffs = id->idAddr()->iiaIGlabel->igOffs;
    uint8_t* dstAddr = emitOffsetToPtr(dstOffs);

    assert(insCodeMI(INS_push) == 0x68);
    dst += emitOutputByte(dst, 0x68);

    if (dstOffs > srcOffs)
    {
        dstAddr -= RecordForwardJump(id, srcOffs, dstOffs);
        id->idjAddr = dst;
    }

    dst += emitOutputLong(dst, reinterpret_cast<ssize_t>(dstAddr));

    if (emitComp->opts.compReloc)
    {
        emitRecordRelocation(dst - 4, dstAddr, IMAGE_REL_BASED_HIGHLOW);
    }

    return dst;
}
#endif // TARGET_X86

uint8_t* emitter::emitOutputJ(uint8_t* dst, instrDescJmp* id, insGroup* ig)
{
    assert(id->idGCref() == GCT_NONE);
    assert(id->idIsBound());
    assert(id->idInsFmt() == IF_LABEL);

    instruction ins = id->idIns();
    size_t      ssz;
    size_t      lsz;

    if (ins == INS_jmp)
    {
        ssz = JMP_SIZE_SMALL;
        lsz = JMP_SIZE_LARGE;
    }
#ifdef FEATURE_EH_FUNCLETS
    else if (ins == INS_call)
    {
        ssz = CALL_INST_SIZE;
        lsz = CALL_INST_SIZE;
    }
#endif
    else
    {
        assert((INS_jo <= ins) && (ins <= INS_jg));

        ssz = JCC_SIZE_SMALL;
        lsz = JCC_SIZE_LARGE;
    }

    unsigned srcOffs = emitCurCodeOffs(dst);
    unsigned dstOffs;

    if (id->idAddr()->iiaHasInstrCount())
    {
        assert(ig != nullptr);
        int      instrCount = id->idAddr()->iiaGetInstrCount();
        unsigned insNum     = emitFindInsNum(ig, id);

        if (instrCount < 0)
        {
            // Backward branches using instruction count must be within the same instruction group.
            assert(insNum + 1 >= (unsigned)(-instrCount));
        }

        dstOffs = ig->igOffs + emitFindOffset(ig, (insNum + 1 + instrCount));
    }
    else
    {
        dstOffs = id->idAddr()->iiaIGlabel->igOffs;
    }

    uint8_t* srcAddr = emitOffsetToPtr(srcOffs);
    uint8_t* dstAddr = emitOffsetToPtr(dstOffs);
    ssize_t  distVal = static_cast<ssize_t>(dstAddr - srcAddr);

    if (dstOffs <= srcOffs)
    {
        // This is a backward jump - distance is known at this point
        CLANG_FORMAT_COMMENT_ANCHOR;

#if DEBUG_EMIT
        if (id->idDebugOnlyInfo()->idNum == (unsigned)INTERESTING_JUMP_NUM || INTERESTING_JUMP_NUM == 0)
        {
            size_t blkOffs = id->idjIG->igOffs;

            if (INTERESTING_JUMP_NUM == 0)
            {
                printf("[3] Jump %u:\n", id->idDebugOnlyInfo()->idNum);
            }
            printf("[3] Jump  block is at %08X - %02X = %08X\n", blkOffs, emitOffsAdj, blkOffs - emitOffsAdj);
            printf("[3] Jump        is at %08X - %02X = %08X\n", srcOffs, emitOffsAdj, srcOffs - emitOffsAdj);
            printf("[3] Label block is at %08X - %02X = %08X\n", dstOffs, emitOffsAdj, dstOffs - emitOffsAdj);
        }
#endif

        // Can we use a short jump?
        if ((ins != INS_call) && distVal - ssz >= (size_t)JMP_DIST_SMALL_MAX_NEG)
        {
            emitSetShortJump(id);
        }
    }
    else
    {
        int adjustment = RecordForwardJump(id, srcOffs, dstOffs);
        dstOffs -= adjustment;
        distVal -= adjustment;

#if DEBUG_EMIT
        if (id->idDebugOnlyInfo()->idNum == (unsigned)INTERESTING_JUMP_NUM || INTERESTING_JUMP_NUM == 0)
        {
            size_t blkOffs = id->idjIG->igOffs;

            if (INTERESTING_JUMP_NUM == 0)
            {
                printf("[4] Jump %u:\n", id->idDebugOnlyInfo()->idNum);
            }
            printf("[4] Jump  block is at %08X\n", blkOffs);
            printf("[4] Jump        is at %08X\n", srcOffs);
            printf("[4] Label block is at %08X - %02X = %08X\n", dstOffs + emitOffsAdj, emitOffsAdj, dstOffs);
        }
#endif

        if ((ins != INS_call) && distVal - ssz <= (size_t)JMP_DIST_SMALL_MAX_POS)
        {
            emitSetShortJump(id);
        }
    }

    // Adjust the offset to emit relative to the end of the instruction
    distVal -= id->idjShort ? ssz : lsz;

#ifdef DEBUG
    if (0 && emitComp->verbose)
    {
        size_t sz          = id->idjShort ? ssz : lsz;
        int    distValSize = id->idjShort ? 4 : 8;
        printf("; %s jump [%08X/%03u] from %0*X to %0*X: dist = %08XH\n", (dstOffs <= srcOffs) ? "Fwd" : "Bwd",
               emitComp->dspPtr(id), id->idDebugOnlyInfo()->idNum, distValSize, srcOffs + sz, distValSize, dstOffs,
               distVal);
    }
#endif

    if (id->idjShort)
    {
        assert(!id->idjKeepLong);
        assert(!emitJumpCrossHotColdBoundary(srcOffs, dstOffs));
        assert(JMP_SIZE_SMALL == JCC_SIZE_SMALL);
        assert(JMP_SIZE_SMALL == 2);
        assert(ins != INS_call);

        if (id->idCodeSize() != JMP_SIZE_SMALL)
        {
#if DEBUG_EMIT || defined(DEBUG)
            int offsShrinkage = id->idCodeSize() - JMP_SIZE_SMALL;
            if (INDEBUG(emitComp->verbose ||)(id->idDebugOnlyInfo()->idNum == (unsigned)INTERESTING_JUMP_NUM ||
                                              INTERESTING_JUMP_NUM == 0))
            {
                printf("; NOTE: size of jump [%08p] mis-predicted by %d bytes\n", dspPtr(id), offsShrinkage);
            }
#endif
        }

        dst += emitOutputByte(dst, insCodeJ(ins));

        // For forward jumps, record the address of the distance value
        if (distVal > 0)
        {
            id->idjAddr = dst;
        }

        dst += emitOutputByte(dst, distVal);
    }
    else
    {
#ifdef FEATURE_EH_FUNCLETS
        if (ins == INS_call)
        {
            dst += emitOutputByte(dst, 0xE8);
        }
        else
#endif
            if (ins == INS_jmp)
        {
            dst += emitOutputByte(dst, 0xE9);
        }
        else
        {
            code_t code = insCodeJ(ins);
            assert((0x70 <= code) && (code <= 0x7F));
            dst += emitOutputWord(dst, 0x0F | ((code + 0x10) << 8));
        }

        // For forward jumps, record the address of the distance value
        if (dstOffs > srcOffs)
        {
            id->idjAddr = dst;
        }

        dst += emitOutputLong(dst, distVal);

        // For x64 we always go through recordRelocation since we may need jump stubs.
        // TODO-MIKE-Review: Hmm, jump stubs for jumps within the same method?!?
        if (X86_ONLY(emitComp->opts.compReloc &&) emitJumpCrossHotColdBoundary(srcOffs, dstOffs))
        {
            assert(id->idjKeepLong);
            emitRecordRelocation(dst - 4, dst + distVal, IMAGE_REL_BASED_REL32);
        }
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
    // It certainly doesn't call emitGCregDeadAll but perhaps it achieves the same
    // effect by other means?
    // Anyway, even if this code is useless removing it results in GC info diffs.
    if ((ins == INS_call) && (gcInfo.GetAllLiveRegs() != RBM_NONE))
    {
        emitGCregDeadAll(dst);
    }
#else
    assert(ins != INS_call);
#endif

    return dst;
}

uint8_t* emitter::emitOutputCall(uint8_t* dst, instrDesc* id)
{
    instruction ins = id->idIns();

    void* addr = id->idAddr()->iiaAddr;
    assert(addr != nullptr);

    if (id->idInsFmt() == IF_METHPTR)
    {
        code_t code = insCodeMR(ins);

        if (id->idIsDspReloc())
        {
            dst += emitOutputWord(dst, code | 0x0500);

#ifdef TARGET_AMD64
            dst += emitOutputLong(dst, 0);
            emitRecordRelocation(dst - 4, addr, IMAGE_REL_BASED_REL32);
#else
            dst += emitOutputLong(dst, reinterpret_cast<ssize_t>(addr));
            emitRecordRelocation(dst - 4, addr, IMAGE_REL_BASED_HIGHLOW);
#endif
        }
        else
        {
#ifdef TARGET_X86
            dst += emitOutputWord(dst, code | 0x0500);
#else
            noway_assert(!emitComp->opts.compReloc);
            noway_assert(!emitComp->eeIsRIPRelativeAddress(addr));
            noway_assert(IsDisp32(reinterpret_cast<intptr_t>(addr)));

            dst += emitOutputWord(dst, code | 0x0400);
            dst += emitOutputByte(dst, 0x25);
#endif

            dst += emitOutputLong(dst, reinterpret_cast<intptr_t>(addr));
        }
    }
    else
    {
        assert(id->idInsFmt() == IF_METHOD);

        dst += emitOutputByte(dst, ins == INS_l_jmp ? insCodeJ(ins) : insCodeMI(ins));

#ifdef TARGET_AMD64
        // For x64 we always go through recordRelocation since we may need jump stubs,
        // we'll just use offset 0 since recordRelocation overwrites it anyway.
        noway_assert(id->idIsDspReloc());

        const ssize_t offset = 0;
#else
        const ssize_t offset = reinterpret_cast<uint8_t*>(addr) - dst - 4;
#endif
        dst += emitOutputLong(dst, offset);

        if (id->idIsDspReloc())
        {
            emitRecordRelocation(dst - 4, addr, IMAGE_REL_BASED_REL32);
        }
    }

    return dst;
}

uint8_t* emitter::emitOutputNoOperands(uint8_t* dst, instrDesc* id)
{
    instruction ins = id->idIns();

    if (ins == INS_nop)
    {
        uint8_t* dstRW = dst + writeableOffset;
        dstRW          = emitOutputNOP(dstRW, id->idCodeSize());

        return dstRW - writeableOffset;
    }

    assert(!TakesVexPrefix(ins));
    assert(id->idOpSize() != EA_2BYTE);
    assert(id->idGCref() == GCT_NONE);

    code_t code = insCodeMR(ins);
    assert((code >> 24) == 0);

    if ((ins == INS_rep_stos) || (ins == INS_rep_movs))
    {
        dst += emitOutputByte(dst, code);
        code >>= 8;
        ins = ins == INS_rep_stos ? INS_stos : INS_movs;
    }

    if (ins == INS_cdq)
    {
        emitGCregDeadUpd(REG_EDX, dst);
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

        code = AddRexWPrefix(ins, code);
        dst += emitOutputRexPrefix(ins, dst, code);
    }
#endif

    if (code & 0x00FF0000)
    {
        dst += emitOutputByte(dst, code >> 16);
        dst += emitOutputWord(dst, code);
    }
    else
    {
        assert((code & 0xFF00) == 0);
        dst += emitOutputByte(dst, code);
    }

    return dst;
}

size_t emitter::emitOutputInstr(insGroup* ig, instrDesc* id, uint8_t** dp)
{
    assert(emitIssuing);

    uint8_t*    dst  = *dp;
    size_t      sz   = sizeof(instrDesc);
    instruction ins  = id->idIns();
    emitAttr    size = id->idOpSize();

    assert((ins != INS_imul) || (size >= EA_4BYTE)); // Has no 'w' bit

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
                dst = emitOutputAlign(ig, id, dst);
                sz  = sizeof(instrDescAlign);
                break;
            }

            dst = emitOutputNoOperands(dst, id);
            break;

        /********************************************************************/
        /*                Simple constant, local label, method              */
        /********************************************************************/

        case IF_CNS:
            dst = emitOutputIV(dst, id);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_RWR_LABEL:
            dst = emitOutputRL(dst, static_cast<instrDescJmp*>(id), ig);
            sz  = sizeof(instrDescJmp);
            break;

        case IF_LABEL:
#ifdef TARGET_X86
            if (id->idIns() == INS_push_hide)
            {
                dst = emitOutputL(dst, static_cast<instrDescJmp*>(id), ig);
            }
            else
#endif
            {
                dst = emitOutputJ(dst, static_cast<instrDescJmp*>(id), ig);
            }
            sz = sizeof(instrDescJmp);
            break;

        case IF_METHOD:
        case IF_METHPTR:
            dst = emitOutputCall(dst, id);
            sz  = emitRecordGCCall(id, *dp, dst);
            break;

        /********************************************************************/
        /*                      One register operand                        */
        /********************************************************************/

        case IF_RRD:
        case IF_RWR:
        case IF_RRW:
            dst = emitOutputR(dst, id);

            if (ins == INS_call)
            {
                sz = emitRecordGCCall(id, *dp, dst);
            }
            else
            {
                sz = SMALL_IDSC_SIZE;
            }
            break;

        /********************************************************************/
        /*                 Register and register/constant                   */
        /********************************************************************/

        case IF_RRD_RRD:
        case IF_RWR_RRD:
        case IF_RRW_RRD:
        case IF_RRW_RRW:
            dst = emitOutputRR(dst, id);
            sz  = SMALL_IDSC_SIZE;
            break;

        case IF_RRD_CNS:
        case IF_RWR_CNS:
        case IF_RRW_CNS:
            dst = emitOutputRI(dst, id);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_RWR_RRD_RRD:
            dst = emitOutputRRR(dst, id);
            sz  = emitSizeOfInsDsc(id);
            break;
        case IF_RWR_RRD_RRD_CNS:
        case IF_RWR_RRD_RRD_RRD:
            dst = emitOutputRRR(dst, id);
            dst += emitOutputByte(dst, emitGetInsSC(id));
            sz = emitSizeOfInsDsc(id);
            break;

        case IF_RRW_RRD_CNS:
            dst = emitOutputRRI(dst, id);
            sz  = emitSizeOfInsDsc(id);
            break;

        /********************************************************************/
        /*                      Address mode operand                        */
        /********************************************************************/

        case IF_ARD:
        case IF_AWR:
        case IF_ARW:
            dst = emitOutputAM(dst, id, insCodeMR(ins));

            if (ins == INS_call)
            {
                sz = emitRecordGCCall(id, *dp, dst);
            }
            else
            {
                sz = emitSizeOfInsDsc(id);
            }
            break;

        case IF_ARD_CNS:
        case IF_AWR_CNS:
        case IF_ARW_CNS:
            cnsVal = emitGetInsAmdCns(id);
            dst    = emitOutputAM(dst, id, insCodeMI(ins), &cnsVal);
            sz     = emitSizeOfInsDsc(id);
            break;

        case IF_ARD_RRD:
        case IF_AWR_RRD:
        case IF_ARW_RRD:
            code = insCodeMR(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(ins, id->idReg1(), size, code);
            }

            code = SetRMReg(ins, id->idReg1(), size, code);
            dst  = emitOutputAM(dst, id, code);
            sz   = emitSizeOfInsDsc(id);
            break;

        case IF_AWR_RRD_CNS:
            assert(ins == INS_vextracti128 || ins == INS_vextractf128);
            assert(UseVEXEncoding());
            assert(!IsVexTernary(ins));

            code   = insCodeMR(ins);
            cnsVal = emitGetInsAmdCns(id);
            dst    = emitOutputAM(dst, id, code, &cnsVal);
            sz     = emitSizeOfInsDsc(id);
            break;

        case IF_AWR_RRD_RRD:
            assert(IsVexDstDstSrc(ins));

            code = insCodeMR(ins);
            code = AddVexPrefix(ins, code, size);
            code = SetVexVvvv(ins, id->idReg1(), size, code);
            dst  = emitOutputAM(dst, id, code);
            sz   = emitSizeOfInsDsc(id);
            break;

        case IF_RRD_ARD:
        case IF_RWR_ARD:
        case IF_RRW_ARD:
            code = insCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(ins, id->idReg1(), size, code);
            }

            if (!IsSSE38orSSE3A(code) && (ins != INS_crc32))
            {
                code = SetRMReg(ins, id->idReg1(), size, code);
            }

            dst = emitOutputAM(dst, id, code);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_RRW_ARD_CNS:
        case IF_RWR_ARD_CNS:
            assert(IsSSEOrAVXInstruction(ins) || (ins == INS_imuli));

            code = insCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(ins, id->idReg1(), size, code);
            }

            if (!IsSSE38orSSE3A(code))
            {
                code = SetRMReg(ins, id->idReg1(), size, code);
            }

            cnsVal = emitGetInsAmdCns(id);
            dst    = emitOutputAM(dst, id, code, &cnsVal);
            sz     = emitSizeOfInsDsc(id);
            break;

        case IF_RWR_RRD_ARD:
            assert(IsVexTernary(ins));

            code = insCodeRM(ins);
            code = AddVexPrefix(ins, code, size);
            code = SetVexVvvv(ins, id->idReg2(), size, code);

            if (!IsSSE38orSSE3A(code))
            {
                code = SetRMReg(ins, id->idReg1(), size, code);
            }

            dst = emitOutputAM(dst, id, code);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_RWR_RRD_ARD_CNS:
        case IF_RWR_RRD_ARD_RRD:
            assert(IsVexTernary(ins));

            code = insCodeRM(ins);
            code = AddVexPrefix(ins, code, size);
            code = SetVexVvvv(ins, id->idReg2(), size, code);

            if (!IsSSE38orSSE3A(code))
            {
                code = SetRMReg(ins, id->idReg1(), size, code);
            }

            cnsVal = emitGetInsAmdCns(id);
            dst    = emitOutputAM(dst, id, code, &cnsVal);
            sz     = emitSizeOfInsDsc(id);
            break;

        case IF_RWR_ARD_RRD:
            assert(IsAVX2GatherInstruction(ins));
            assert(IsVexDstDstSrc(ins));

            code = insCodeRM(ins);
            code = AddVexPrefix(ins, code, size);
            code = SetVexVvvv(ins, id->idReg2(), size, code);
            dst  = emitOutputAM(dst, id, code);
            sz   = emitSizeOfInsDsc(id);
            break;

        /********************************************************************/
        /*                      Stack-based operand                         */
        /********************************************************************/

        case IF_SRD:
        case IF_SWR:
        case IF_SRW:
            assert(ins != INS_pop_hide);
#if !FEATURE_FIXED_OUT_ARGS
            if (ins == INS_pop)
            {
                // The offset in "pop [ESP+xxx]" is relative to the new ESP value
                emitCurStackLvl -= REGSIZE_BYTES;
            }
#endif
            dst = emitOutputSV(dst, id, insCodeMR(ins));
#if !FEATURE_FIXED_OUT_ARGS
            if (ins == INS_pop)
            {
                emitCurStackLvl += REGSIZE_BYTES;
            }
#endif
            if (ins == INS_call)
            {
                sz = emitRecordGCCall(id, *dp, dst);
            }
            break;

        case IF_SRD_CNS:
        case IF_SWR_CNS:
        case IF_SRW_CNS:
            cnsVal = emitGetInsCns(id);
            dst    = emitOutputSV(dst, id, insCodeMI(ins), &cnsVal);
            sz     = emitSizeOfInsDsc(id);
            break;

        case IF_SRD_RRD:
        case IF_SWR_RRD:
        case IF_SRW_RRD:
            code = insCodeMR(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(ins, id->idReg1(), size, code);
            }

            code = SetRMReg(ins, id->idReg1(), size, code);
            dst  = emitOutputSV(dst, id, code);
            sz   = emitSizeOfInsDsc(id);
            break;

        case IF_SWR_RRD_CNS:
            assert(ins == INS_vextracti128 || ins == INS_vextractf128);
            assert(UseVEXEncoding());
            assert(!IsVexTernary(ins));

            code   = insCodeMR(ins);
            cnsVal = emitGetInsAmdCns(id);
            dst    = emitOutputSV(dst, id, insCodeMR(ins), &cnsVal);
            sz     = emitSizeOfInsDsc(id);
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

        case IF_RRD_SRD:
        case IF_RWR_SRD:
        case IF_RRW_SRD:
            code = insCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(ins, id->idReg1(), size, code);
            }

            if (!IsSSE38orSSE3A(code) && (ins != INS_crc32))
            {
                code = SetRMReg(ins, id->idReg1(), size, code);
            }

            dst = emitOutputSV(dst, id, code);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_RRW_SRD_CNS:
        case IF_RWR_SRD_CNS:
            assert(IsSSEOrAVXInstruction(ins) || (ins == INS_imuli));

            code = insCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(ins, id->idReg1(), size, code);
            }

            if (!IsSSE38orSSE3A(code))
            {
                code = SetRMReg(ins, id->idReg1(), size, code);
            }

            cnsVal = emitGetInsCns(id);
            dst    = emitOutputSV(dst, id, code, &cnsVal);
            sz     = emitSizeOfInsDsc(id);
            break;

        case IF_RWR_RRD_SRD:
            assert(IsVexTernary(ins));

            code = insCodeRM(ins);
            code = AddVexPrefix(ins, code, size);
            code = SetVexVvvv(ins, id->idReg2(), size, code);

            if (!IsSSE38orSSE3A(code))
            {
                code = SetRMReg(ins, id->idReg1(), size, code);
            }

            dst = emitOutputSV(dst, id, code);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_RWR_RRD_SRD_CNS:
        case IF_RWR_RRD_SRD_RRD:
            assert(IsVexTernary(ins));

            code = insCodeRM(ins);
            code = AddVexPrefix(ins, code, size);
            code = SetVexVvvv(ins, id->idReg2(), size, code);

            if (!IsSSE38orSSE3A(code))
            {
                code = SetRMReg(ins, id->idReg1(), size, code);
            }

            cnsVal = emitGetInsCns(id);
            dst    = emitOutputSV(dst, id, code, &cnsVal);
            sz     = emitSizeOfInsDsc(id);
            break;

        // case IF_RWR_SRD_RRD:
        // This format is used by gather instructions. It's unlikely
        // that such instructions could load from local variables, but
        // perhaps not impossible, e.g. load from a local struct with
        // with a fixed buffer. And 'vgatherdpd xmm0, [rsp+xmm1+32], xmm2'
        // is perfectly valid.

        /********************************************************************/
        /*                    Direct memory address                         */
        /********************************************************************/

        case IF_MRD:
        case IF_MRW:
        case IF_MWR:
            noway_assert(ins != INS_call);
            dst = emitOutputCV(dst, id, insCodeMR(ins));
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_MRD_CNS:
        case IF_MWR_CNS:
        case IF_MRW_CNS:
            cnsVal = emitGetInsMemImm(id);
            dst    = emitOutputCV(dst, id, insCodeMI(ins), &cnsVal);
            sz     = emitSizeOfInsDsc(id);
            break;

        case IF_MRD_RRD:
        case IF_MWR_RRD:
        case IF_MRW_RRD:
            code = insCodeMR(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(ins, id->idReg1(), size, code);
            }

            code = SetRMReg(ins, id->idReg1(), size, code);
            dst  = emitOutputCV(dst, id, code);
            sz   = emitSizeOfInsDsc(id);
            break;

        case IF_MWR_RRD_CNS:
            assert(ins == INS_vextracti128 || ins == INS_vextractf128);
            assert(UseVEXEncoding());
            assert(!IsVexTernary(ins));

            code   = insCodeMR(ins);
            cnsVal = emitGetInsMemImm(id);
            dst    = emitOutputCV(dst, id, code, &cnsVal);
            sz     = emitSizeOfInsDsc(id);
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

        case IF_RRD_MRD:
        case IF_RWR_MRD:
        case IF_RRW_MRD:
            code = insCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(ins, id->idReg1(), size, code);
            }

            if (!IsSSE38orSSE3A(code) && (ins != INS_crc32))
            {
                code = SetRMReg(ins, id->idReg1(), size, code);
            }

            dst = emitOutputCV(dst, id, code);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_RRW_MRD_CNS:
        case IF_RWR_MRD_CNS:
            assert(IsSSEOrAVXInstruction(ins) || (ins == INS_imuli));

            code = insCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            if (IsVexDstDstSrc(ins))
            {
                code = SetVexVvvv(ins, id->idReg1(), size, code);
            }

            if (!IsSSE38orSSE3A(code))
            {
                code = SetRMReg(ins, id->idReg1(), size, code);
            }

            cnsVal = emitGetInsMemImm(id);
            dst    = emitOutputCV(dst, id, code, &cnsVal);
            sz     = emitSizeOfInsDsc(id);
            break;

        case IF_RWR_RRD_MRD:
            assert(IsVexTernary(ins));

            code = insCodeRM(ins);
            code = AddVexPrefix(ins, code, size);
            code = SetVexVvvv(ins, id->idReg2(), size, code);

            if (!IsSSE38orSSE3A(code))
            {
                code = SetRMReg(ins, id->idReg1(), size, code);
            }

            dst = emitOutputCV(dst, id, code);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_RWR_RRD_MRD_CNS:
        case IF_RWR_RRD_MRD_RRD:
            assert(IsVexTernary(ins));

            code = insCodeRM(ins);
            code = AddVexPrefix(ins, code, size);
            code = SetVexVvvv(ins, id->idReg2(), size, code);

            if (!IsSSE38orSSE3A(code))
            {
                code = SetRMReg(ins, id->idReg1(), size, code);
            }

            cnsVal = emitGetInsCns(id);
            dst    = emitOutputCV(dst, id, code, &cnsVal);
            sz     = emitSizeOfInsDsc(id);
            break;

        // case IF_RWR_MRD_RRD:
        // This format is used by gather instructions.
        // It's practically impossible to get this on x64, since RIP relative
        // addressing is used.
        //
        // It may be possible to get this to work on x86 but hey, it's x86.

        default:
            unreached();
    }

    // Make sure we set the instruction descriptor size correctly
    assert(sz == emitSizeOfInsDsc(id));

#if !FEATURE_FIXED_OUT_ARGS
    if (!emitIGisInProlog(ig) && !ig->IsEpilog() && !ig->IsFuncletPrologOrEpilog())
    {
        switch (ins)
        {
            case INS_push_hide:
            case INS_pop_hide:
                break;
            case INS_push:
                emitStackPush(emitCurCodeOffs(dst), id->idGCref());
                break;
            case INS_pop:
                emitStackPop(emitCurCodeOffs(dst), 1);
                break;
            case INS_add:
            case INS_sub:
                if ((id->idInsFmt() == IF_RRW_CNS) && (id->idReg1() == REG_ESP))
                {
                    size_t imm = static_cast<size_t>(emitGetInsSC(id));
                    assert(imm < UINT_MAX);
                    unsigned count    = static_cast<unsigned>(imm) / TARGET_POINTER_SIZE;
                    unsigned codeOffs = emitCurCodeOffs(dst);

                    if (ins == INS_add)
                    {
                        emitStackPop(codeOffs, count);
                    }
                    else
                    {
                        emitStackPushN(codeOffs, count);
                    }
                }
                break;

            default:
                break;
        }
    }

    assert(emitCurStackLvl <= INT32_MAX);
#endif // !FEATURE_FIXED_OUT_ARGS

    assert((*dp != dst) || InstrHasNoCode(id));

#ifdef DEBUG
    if ((emitComp->opts.disAsm || emitComp->verbose) && (*dp != dst))
    {
        emitDispIns(id, false, emitComp->opts.dspGCtbls, true, emitCurCodeOffs(*dp), *dp, (dst - *dp));
    }
#endif

#if FEATURE_LOOP_ALIGN
    // Only compensate over-estimated instructions if emitCurIG is before
    // the last IG that needs alignment.
    if (emitCurIG->igNum <= emitLastAlignedIgNum)
    {
        int diff = id->idCodeSize() - ((unsigned)(dst - *dp));

        assert(diff >= 0);

        if (diff != 0)
        {
#ifdef DEBUG
            // should never over-estimate align instruction
            assert(id->idIns() != INS_align);

            JITDUMP("Added over-estimation compensation: %d\n", diff);

            if (emitComp->opts.disAsm)
            {
                emitDispInsAddr(dst);
                printf("\t\t  ;; NOP compensation instructions of %d bytes.\n", diff);
            }
#endif

            uint8_t* dstRW = dst + writeableOffset;
            dstRW          = emitOutputNOP(dstRW, diff);
            dst            = dstRW - writeableOffset;
        }

        assert((id->idCodeSize() - static_cast<unsigned>(dst - *dp)) == 0);
    }
#endif

#ifdef DEBUG
    if (emitComp->compDebugBreak)
    {
        if (JitConfig.JitEmitPrintRefRegs() != 0)
        {
            printf("Before emitOutputInstr for id->idDebugOnlyInfo()->idNum=0x%02x\n", id->idDebugOnlyInfo()->idNum);
            printf("  REF regs");
            DumpRegSet(gcInfo.GetLiveRegs(GCT_GCREF));
            printf("\n  BYREF regs");
            DumpRegSet(gcInfo.GetLiveRegs(GCT_BYREF));
            printf("\n");
        }

        if (JitConfig.JitBreakEmitOutputInstr() == static_cast<int>(id->idDebugOnlyInfo()->idNum))
        {
            assert(!"JitBreakEmitOutputInstr reached");
        }
    }

    if (ins == INS_mulEAX || ins == INS_imulEAX)
    {
        assert((gcInfo.GetAllLiveRegs() & (RBM_EAX | RBM_EDX)) == RBM_NONE);
    }
    else if (ins == INS_imuli)
    {
        assert((gcInfo.GetAllLiveRegs() & genRegMask(id->idReg1())) == RBM_NONE);
    }
#endif

    *dp = dst;

    return sz;
}

#if defined(DEBUG) || defined(LATE_DISASM)

insFormat emitter::getMemoryOperation(instrDesc* id)
{
    insFormat   result = IF_NONE;
    instruction ins    = id->idIns();
    insFormat   insFmt = id->idInsFmt();

    if (ins == INS_lea)
    {
        // an INS_lea instruction doesn't actually read memory
        insFmt = IF_NONE;
    }

    switch (insFmt)
    {
        case IF_NONE:
        case IF_LABEL:
        case IF_RWR_LABEL:
        case IF_METHOD:
        case IF_CNS:

        case IF_RRD:
        case IF_RWR:
        case IF_RRW:
        case IF_RRD_CNS:
        case IF_RWR_CNS:
        case IF_RRW_CNS:
        case IF_RRD_RRD:
        case IF_RWR_RRD:
        case IF_RRW_RRD:
        case IF_RRW_RRW:
        case IF_RRW_RRD_CNS:
        case IF_RWR_RRD_RRD:
        case IF_RWR_RRD_RRD_CNS:
        case IF_RWR_RRD_RRD_RRD:
            // none, or register only
            result = IF_NONE;
            break;

        case IF_ARD:
        case IF_RRD_ARD:
        case IF_RWR_ARD:
        case IF_RRW_ARD:
        case IF_RWR_ARD_CNS:
        case IF_RWR_RRD_ARD:
        case IF_RRW_ARD_CNS:
        case IF_RWR_ARD_RRD:
        case IF_RWR_RRD_ARD_CNS:
        case IF_RWR_RRD_ARD_RRD:
        case IF_ARD_CNS:
        case IF_ARD_RRD:
            // Address [reg+reg*scale+cns] - read
            result = IF_ARD;
            break;

        case IF_AWR:
        case IF_AWR_RRD:
        case IF_AWR_CNS:
        case IF_AWR_RRD_CNS:
        case IF_AWR_RRD_RRD:
            // Address [reg+reg*scale+cns] - write
            result = IF_AWR;
            break;

        case IF_ARW:
        case IF_ARW_RRD:
        case IF_ARW_CNS:
            // Address [reg+reg*scale+cns] - read and write
            result = IF_ARW;
            break;

        case IF_MRD:
        case IF_MRD_CNS:
        case IF_MRD_RRD:
        case IF_RRD_MRD:
        case IF_RRW_MRD:
        case IF_RWR_MRD:
        case IF_RWR_MRD_CNS:
        case IF_RWR_RRD_MRD:
        case IF_RRW_MRD_CNS:
        case IF_RWR_RRD_MRD_CNS:
        case IF_RWR_RRD_MRD_RRD:
        case IF_METHPTR:
            // Address [cns] - read
            result = IF_MRD;
            break;

        case IF_MWR:
        case IF_MWR_CNS:
        case IF_MWR_RRD:
        case IF_MWR_RRD_CNS:
            // Address [cns] - write
            result = IF_MWR;
            break;

        case IF_MRW:
        case IF_MRW_CNS:
        case IF_MRW_RRD:
            // Address [cns] - read and write
            result = IF_MWR;
            break;

        case IF_SRD:
        case IF_SRD_CNS:
        case IF_SRD_RRD:

        case IF_RRD_SRD:
        case IF_RRW_SRD:
        case IF_RWR_SRD:
        case IF_RWR_SRD_CNS:
        case IF_RWR_RRD_SRD:
        case IF_RRW_SRD_CNS:
        case IF_RWR_RRD_SRD_CNS:
        case IF_RWR_RRD_SRD_RRD:
            // Stack [RSP] - read
            result = IF_SRD;
            break;

        case IF_SWR:
        case IF_SWR_CNS:
        case IF_SWR_RRD:
        case IF_SWR_RRD_CNS:
            // Stack [RSP] - write
            result = IF_SWR;
            break;

        case IF_SRW:
        case IF_SRW_CNS:
        case IF_SRW_RRD:
            // Stack [RSP] - read and write
            result = IF_SWR;
            break;

        default:
            result = IF_NONE;
            break;
    }
    return result;
}

// The instruction latencies and throughput values returned by this function
// are for the Intel Skylake-X processor and are from either:
//   1. Agner.org - https://www.agner.org/optimize/instruction_tables.pdf
//   2. uops.info - https://uops.info/table.html
//
emitter::insExecutionCharacteristics emitter::getInsExecutionCharacteristics(instrDesc* id)
{
    insExecutionCharacteristics result;
    instruction                 ins    = id->idIns();
    insFormat                   insFmt = id->idInsFmt();
    insFormat                   memFmt = getMemoryOperation(id);
    unsigned                    memAccessKind;

    result.insThroughput = PERFSCORE_THROUGHPUT_ILLEGAL;
    result.insLatency    = PERFSCORE_LATENCY_ILLEGAL;

    // Model the memory latency
    switch (memFmt)
    {
        // Model a read from stack location, possible def to use latency from L0 cache
        case IF_SRD:
            result.insLatency = PERFSCORE_LATENCY_RD_STACK;
            memAccessKind     = PERFSCORE_MEMORY_READ;
            break;

        case IF_SWR:
            result.insLatency = PERFSCORE_LATENCY_WR_STACK;
            memAccessKind     = PERFSCORE_MEMORY_WRITE;
            break;

        case IF_SRW:
            result.insLatency = PERFSCORE_LATENCY_RD_WR_STACK;
            memAccessKind     = PERFSCORE_MEMORY_READ_WRITE;
            break;

        // Model a read from a constant location, possible def to use latency from L0 cache
        case IF_MRD:
            result.insLatency = PERFSCORE_LATENCY_RD_CONST_ADDR;
            memAccessKind     = PERFSCORE_MEMORY_READ;
            break;

        case IF_MWR:
            result.insLatency = PERFSCORE_LATENCY_WR_CONST_ADDR;
            memAccessKind     = PERFSCORE_MEMORY_WRITE;
            break;

        case IF_MRW:
            result.insLatency = PERFSCORE_LATENCY_RD_WR_CONST_ADDR;
            memAccessKind     = PERFSCORE_MEMORY_READ_WRITE;
            break;

        // Model a read from memory location, possible def to use latency from L0 or L1 cache
        case IF_ARD:
            result.insLatency = PERFSCORE_LATENCY_RD_GENERAL;
            memAccessKind     = PERFSCORE_MEMORY_READ;
            break;

        case IF_AWR:
            result.insLatency = PERFSCORE_LATENCY_WR_GENERAL;
            memAccessKind     = PERFSCORE_MEMORY_WRITE;
            break;

        case IF_ARW:
            result.insLatency = PERFSCORE_LATENCY_RD_WR_GENERAL;
            memAccessKind     = PERFSCORE_MEMORY_READ_WRITE;
            break;

        case IF_NONE:
            result.insLatency = PERFSCORE_LATENCY_ZERO;
            memAccessKind     = PERFSCORE_MEMORY_NONE;
            break;

        default:
            assert(!"Unhandled insFmt for switch (memFmt)");
            result.insLatency = PERFSCORE_LATENCY_ZERO;
            memAccessKind     = PERFSCORE_MEMORY_NONE;
            break;
    }
    result.insMemoryAccessKind = memAccessKind;

    switch (ins)
    {
        case INS_align:
#if FEATURE_LOOP_ALIGN
            if (id->idCodeSize() == 0)
            {
                // We're not going to generate any instruction, so it doesn't count for PerfScore.
                result.insThroughput = PERFSCORE_THROUGHPUT_ZERO;
                result.insLatency    = PERFSCORE_LATENCY_ZERO;
                break;
            }
#endif
            FALLTHROUGH;

        case INS_nop:
        case INS_int3:
            assert(memFmt == IF_NONE);
            result.insThroughput = PERFSCORE_THROUGHPUT_4X;
            result.insLatency    = PERFSCORE_LATENCY_ZERO;
            break;

        case INS_push:
        case INS_push_hide:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            if (insFmt == IF_RRD) // push  reg
            {
                // For pushes (stack writes) we assume that the full latency will be covered
                result.insLatency = PERFSCORE_LATENCY_ZERO;
            }
            break;

        case INS_pop:
        case INS_pop_hide:
            if (insFmt == IF_RWR) // pop   reg
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                // For pops (stack reads) we assume that the full latency will be covered
                result.insLatency = PERFSCORE_LATENCY_ZERO;
            }
            else
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            }
            break;

        case INS_inc:
        case INS_dec:
        case INS_neg:
        case INS_not:
            if (memFmt == IF_NONE)
            {
                // ins   reg
                result.insThroughput = PERFSCORE_THROUGHPUT_4X;
                result.insLatency    = PERFSCORE_LATENCY_1C;
            }
            else
            {
                // ins   mem
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
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
                result.insThroughput = PERFSCORE_THROUGHPUT_4X;
            }
            else if (memAccessKind == PERFSCORE_MEMORY_READ)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            }
            else // writes
            {
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            }
            break;

        case INS_adc:
        case INS_sbb:
        case INS_add:
        case INS_sub:
        case INS_and:
        case INS_or:
        case INS_xor:
            result.insLatency = max(PERFSCORE_LATENCY_1C, result.insLatency);
            if (memFmt == IF_NONE)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_4X;
            }
            else if (memAccessKind == PERFSCORE_MEMORY_READ_WRITE)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            }
            else
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            }
            break;

        case INS_lea:
            // uops.info
            result.insThroughput = PERFSCORE_THROUGHPUT_2X; // one or two components
            result.insLatency    = PERFSCORE_LATENCY_1C;

            if (insFmt == IF_RWR_LABEL)
            {
                // RIP relative addressing
                //
                // - throughput is only 1 per cycle
                //
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            }
            else if (insFmt != IF_RWR_SRD)
            {
                if (id->idAddr()->iiaAddrMode.amIndxReg != REG_NA)
                {
                    regNumber baseReg = id->idAddr()->iiaAddrMode.amBaseReg;
                    if (baseReg != REG_NA)
                    {
                        ssize_t dsp = emitGetInsAmdDisp(id);

                        if ((dsp != 0) || BaseRegRequiresDisp(baseReg))
                        {
                            // three components
                            //
                            // - throughput is only 1 per cycle
                            //
                            result.insThroughput = PERFSCORE_THROUGHPUT_1C;

                            if (BaseRegRequiresDisp(baseReg) || id->idIsDspReloc())
                            {
                                // Increased Latency for these cases
                                //  - see https://reviews.llvm.org/D32277
                                //
                                result.insLatency = PERFSCORE_LATENCY_3C;
                            }
                        }
                    }
                }
            }

            break;

        case INS_imul:
        case INS_imuli:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency += PERFSCORE_LATENCY_3C;
            break;

        case INS_mulEAX:
        case INS_imulEAX:
            // uops.info: mul/imul rdx:rax,reg latency is 3 only if the low half of the result is needed, but in that
            // case codegen uses imul reg,reg instruction form (except for unsigned overflow checks, which are rare)
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency += PERFSCORE_LATENCY_4C;
            break;

        case INS_div:
            // The integer divide instructions have long latencies
            if ((id->idOpSize() == EA_8BYTE))
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_52C;
                result.insLatency    = PERFSCORE_LATENCY_62C;
            }
            else
            {
                assert(id->idOpSize() == EA_4BYTE);
                result.insThroughput = PERFSCORE_THROUGHPUT_6C;
                result.insLatency    = PERFSCORE_LATENCY_26C;
            }
            break;

        case INS_idiv:
            // The integer divide instructions have long latenies
            if ((id->idOpSize() == EA_8BYTE))
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_57C;
                result.insLatency    = PERFSCORE_LATENCY_69C;
            }
            else
            {
                assert(id->idOpSize() == EA_4BYTE);
                result.insThroughput = PERFSCORE_THROUGHPUT_6C;
                result.insLatency    = PERFSCORE_LATENCY_26C;
            }
            break;

        case INS_cdq:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = PERFSCORE_LATENCY_1C;
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
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    result.insLatency    = PERFSCORE_LATENCY_1C;
                    break;

                case IF_MRW_CNS:
                case IF_SRW_CNS:
                case IF_ARW_CNS:
                    // ins   [mem], cns
                    result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                    result.insLatency += PERFSCORE_LATENCY_1C;
                    break;

                case IF_RRW:
                    // ins   reg, cl
                    result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                    result.insLatency    = PERFSCORE_LATENCY_2C;
                    break;

                case IF_MRW:
                case IF_SRW:
                case IF_ARW:
                    // ins   [mem], cl
                    result.insThroughput = PERFSCORE_THROUGHPUT_4C;
                    result.insLatency += PERFSCORE_LATENCY_2C;
                    break;

                default:
                    // unhandled instruction insFmt combination
                    perfScoreUnhandledInstruction(id, &result);
                    break;
            }
            break;

        case INS_shl_1:
        case INS_shr_1:
        case INS_sar_1:
            result.insLatency += PERFSCORE_LATENCY_1C;
            switch (insFmt)
            {
                case IF_RRW:
                    // ins   reg, 1
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    break;

                case IF_MRW:
                case IF_SRW:
                case IF_ARW:
                    // ins   [mem], 1
                    result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                    break;

                default:
                    // unhandled instruction insFmt combination
                    perfScoreUnhandledInstruction(id, &result);
                    break;
            }
            break;

        case INS_ror_1:
        case INS_rol_1:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency += PERFSCORE_LATENCY_1C;
            break;

        case INS_shl_N:
        case INS_shr_N:
        case INS_sar_N:
        case INS_ror_N:
        case INS_rol_N:
            result.insLatency += PERFSCORE_LATENCY_1C;
            switch (insFmt)
            {
                case IF_RRW_CNS:
                    // ins   reg, cns
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    break;

                case IF_MRW_CNS:
                case IF_SRW_CNS:
                case IF_ARW_CNS:
                    // ins   [mem], cns
                    result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                    break;

                default:
                    // unhandled instruction insFmt combination
                    perfScoreUnhandledInstruction(id, &result);
                    break;
            }
            break;

        case INS_rcr:
        case INS_rcl:
            result.insThroughput = PERFSCORE_THROUGHPUT_6C;
            result.insLatency += PERFSCORE_LATENCY_6C;
            break;

        case INS_rcr_1:
        case INS_rcl_1:
            // uops.info
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency += PERFSCORE_LATENCY_2C;
            break;

        case INS_shld:
        case INS_shrd:
            if (insFmt == IF_RRW_RRD_CNS)
            {
                // ins   reg, reg, cns
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                result.insLatency    = PERFSCORE_LATENCY_3C;
            }
            else
            {
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE); // _SHF form never emitted
                result.insThroughput = PERFSCORE_THROUGHPUT_2C;
            }
            break;

        case INS_bt:
            if ((insFmt == IF_RRD_RRD) || (insFmt == IF_RRD_CNS))
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                result.insLatency    = PERFSCORE_LATENCY_1C;
            }
            else
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
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
            result.insLatency = PERFSCORE_LATENCY_1C;
            if (insFmt == IF_RRD)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            }
            else
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
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
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_BRANCH_COND;
            break;

        case INS_jmp:
        case INS_l_jmp:
            // branch to a constant address
            result.insThroughput = PERFSCORE_THROUGHPUT_2C;
            result.insLatency    = PERFSCORE_LATENCY_BRANCH_DIRECT;
            break;

#ifdef TARGET_AMD64
        case INS_rex_jmp:
#endif // TARGET_AMD64
        case INS_i_jmp:
            // branch to register
            result.insThroughput = PERFSCORE_THROUGHPUT_2C;
            result.insLatency    = PERFSCORE_LATENCY_BRANCH_INDIRECT;
            break;

        case INS_call:
            // uops.info
            result.insLatency = PERFSCORE_LATENCY_ZERO;
            switch (insFmt)
            {
                case IF_LABEL:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    break;

                case IF_METHOD:
                    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                    break;

                case IF_METHPTR:
                    result.insThroughput = PERFSCORE_THROUGHPUT_3C;
                    break;

                case IF_SRD:
                    result.insThroughput = PERFSCORE_THROUGHPUT_3C;
                    break;

                case IF_ARD:
                case IF_RRD:
                    result.insThroughput = PERFSCORE_THROUGHPUT_3C;
                    break;

                default:
                    // unhandled instruction, insFmt combination
                    perfScoreUnhandledInstruction(id, &result);
                    break;
            }
            break;

        case INS_ret:
            if (insFmt == IF_CNS)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_2C;
            }
            else
            {
                assert(insFmt == IF_NONE);
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            }
            break;

        case INS_lock:
            result.insThroughput = PERFSCORE_THROUGHPUT_13C;
            break;

        case INS_xadd:
            // uops.info
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            break;

        case INS_cmpxchg:
            result.insThroughput = PERFSCORE_THROUGHPUT_5C;
            break;

        case INS_xchg:
            // uops.info
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            if (memFmt == IF_NONE)
            {
                result.insLatency = PERFSCORE_LATENCY_1C;
            }
            else
            {
                result.insLatency = PERFSCORE_LATENCY_23C;
            }
            break;

#ifdef TARGET_X86
        case INS_fld:
        case INS_fstp:
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.insLatency = PERFSCORE_LATENCY_1C;
            }
            break;
#endif // TARGET_X86

        case INS_movs:
        case INS_stos:
            // uops.info
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            break;

        case INS_rep_movs:
        case INS_rep_stos:
            // Actually variable sized: rep stosd, used to zero frame slots
            // uops.info
            result.insThroughput = PERFSCORE_THROUGHPUT_25C;
            break;

        case INS_movd:
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                // movd   r32, xmm   or  xmm, r32
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                result.insLatency    = PERFSCORE_LATENCY_3C;
            }
            else if (memAccessKind == PERFSCORE_MEMORY_READ)
            {
                // movd   xmm, m32
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                // insLatency is set above (see -  Model the memory latency)
            }
            else
            {
                // movd   m32, xmm
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                // insLatency is set above (see -  Model the memory latency)
            }
            break;

        case INS_movq:
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                // movq   reg, reg
                result.insThroughput = PERFSCORE_THROUGHPUT_3X;
                result.insLatency    = PERFSCORE_LATENCY_1C;
            }
            else if (memAccessKind == PERFSCORE_MEMORY_READ)
            {
                // movq   reg, mem
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                // insLatency is set above (see -  Model the memory latency)
            }
            else
            {
                // movq   mem, reg
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                // insLatency is set above (see -  Model the memory latency)
            }
            break;

        case INS_movdqa:
        case INS_movdqu:
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                // ins   reg, reg
                result.insThroughput = PERFSCORE_THROUGHPUT_4X;
                result.insLatency    = PERFSCORE_LATENCY_ZERO;
            }
            else if (memAccessKind == PERFSCORE_MEMORY_READ)
            {
                // ins   reg, mem
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                // insLatency is set above (see -  Model the memory latency)
            }
            else
            {
                // ins   mem, reg
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                // insLatency is set above (see -  Model the memory latency)
            }
            break;

        case INS_movhps:
        case INS_movhpd:
        case INS_movlps:
        case INS_movlpd:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            if (memAccessKind == PERFSCORE_MEMORY_READ)
            {
                result.insLatency = max(PERFSCORE_LATENCY_4C, result.insLatency);
            }
            else
            {
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
                result.insLatency = max(PERFSCORE_LATENCY_3C, result.insLatency);
            }
            break;

        case INS_movhlps:
        case INS_movlhps:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = PERFSCORE_LATENCY_1C;
            break;

        case INS_movntdq:
        case INS_movnti:
        case INS_movntps:
        case INS_movntpd:
            assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = PERFSCORE_LATENCY_400C; // Intel microcode issue with these instuctions
            break;

        case INS_maskmovdqu:
            result.insThroughput = PERFSCORE_THROUGHPUT_6C;
            result.insLatency    = PERFSCORE_LATENCY_400C; // Intel microcode issue with these instuctions
            break;

        case INS_movntdqa:
            assert(memAccessKind == PERFSCORE_MEMORY_READ);
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_3C;
            break;

        case INS_vzeroupper:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            // insLatency is zero and is set when we Model the memory latency
            break;

        case INS_movss:
        case INS_movsd:
        case INS_movddup:
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                result.insLatency    = PERFSCORE_LATENCY_1C;
            }
            else
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                result.insLatency    = max(PERFSCORE_LATENCY_3C, result.insLatency);
            }
            break;

        case INS_lddqu:
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = max(PERFSCORE_LATENCY_3C, result.insLatency);
            break;

        case INS_comiss:
        case INS_comisd:
        case INS_ucomiss:
        case INS_ucomisd:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = max(PERFSCORE_LATENCY_2C, result.insLatency);
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
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency += PERFSCORE_LATENCY_4C;
            break;

        case INS_haddps:
        case INS_haddpd:
            result.insThroughput = PERFSCORE_THROUGHPUT_2C;
            result.insLatency += PERFSCORE_LATENCY_6C;
            break;

        case INS_mulss:
        case INS_mulsd:
        case INS_mulps:
        case INS_mulpd:
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency += PERFSCORE_LATENCY_4C;
            break;

        case INS_divss:
        case INS_divps:
            result.insThroughput = PERFSCORE_THROUGHPUT_3C;
            result.insLatency += PERFSCORE_LATENCY_11C;
            break;

        case INS_divsd:
        case INS_divpd:
            result.insThroughput = PERFSCORE_THROUGHPUT_4C;
            result.insLatency += PERFSCORE_LATENCY_13C;
            break;

        case INS_sqrtss:
        case INS_sqrtps:
            result.insThroughput = PERFSCORE_THROUGHPUT_3C;
            result.insLatency += PERFSCORE_LATENCY_12C;
            break;

        case INS_sqrtsd:
        case INS_sqrtpd:
            result.insThroughput = PERFSCORE_THROUGHPUT_4C;
            result.insLatency += PERFSCORE_LATENCY_13C;
            break;

        case INS_rcpps:
        case INS_rcpss:
        case INS_rsqrtss:
        case INS_rsqrtps:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency += PERFSCORE_LATENCY_4C;
            break;

        case INS_roundpd:
        case INS_roundps:
        case INS_roundsd:
        case INS_roundss:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency += PERFSCORE_LATENCY_8C;
            break;

        case INS_cvttsd2si:
        case INS_cvtsd2si:
        case INS_cvttss2si:
        case INS_cvtss2si:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency += PERFSCORE_LATENCY_6C;
            break;

        case INS_cvtsi2sd:
        case INS_cvtsi2ss:
            result.insThroughput = PERFSCORE_THROUGHPUT_2C;
            result.insLatency += PERFSCORE_LATENCY_6C;
            break;

        case INS_cvtss2sd:
            result.insThroughput = PERFSCORE_THROUGHPUT_2C;
            result.insLatency += PERFSCORE_LATENCY_5C;
            break;

        case INS_movaps:
        case INS_movups:
        case INS_movapd:
        case INS_movupd:
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_4X;
                result.insLatency    = PERFSCORE_LATENCY_1C;
            }
            else if (memAccessKind == PERFSCORE_MEMORY_READ)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                result.insLatency += PERFSCORE_LATENCY_2C;
            }
            else
            {
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                result.insLatency += PERFSCORE_LATENCY_3C;
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
            result.insLatency += PERFSCORE_LATENCY_1C;
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_3X;
            }
            else
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
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
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency += PERFSCORE_LATENCY_1C;
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
            result.insLatency += PERFSCORE_LATENCY_1C;
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            }
            else
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            }
            break;

        case INS_blsi:
        case INS_blsmsk:
        case INS_blsr:
        case INS_bextr:
        case INS_bzhi:
            result.insLatency += PERFSCORE_LATENCY_2C;
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            }
            else
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
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
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency += PERFSCORE_LATENCY_1C;
            break;

        case INS_vblendvps:
        case INS_vblendvpd:
        case INS_vpblendvb:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.insLatency = PERFSCORE_LATENCY_2C;
            }
            break;

        case INS_bswap:
            if ((id->idOpSize() == EA_8BYTE))
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                result.insLatency    = PERFSCORE_LATENCY_2C;
            }
            else
            {
                assert(id->idOpSize() == EA_4BYTE);
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                result.insLatency    = PERFSCORE_LATENCY_1C;
            }
            break;

        case INS_pmovmskb:
        case INS_movmskpd:
        case INS_movmskps:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency += PERFSCORE_LATENCY_2C;
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
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency += PERFSCORE_LATENCY_3C;
            break;

        case INS_phaddw:
        case INS_phaddd:
        case INS_phaddsw:
        case INS_phsubw:
        case INS_phsubsw:
        case INS_phsubd:
            result.insThroughput = PERFSCORE_THROUGHPUT_2C;
            result.insLatency += PERFSCORE_LATENCY_3C;
            break;

        case INS_cmpps:
        case INS_cmppd:
        case INS_cmpss:
        case INS_cmpsd:
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency    = PERFSCORE_LATENCY_4C;
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
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency += PERFSCORE_LATENCY_4C;
            break;

        case INS_mpsadbw:
            result.insThroughput = PERFSCORE_THROUGHPUT_2C;
            result.insLatency += PERFSCORE_LATENCY_4C;
            break;

        case INS_pmullw:
        case INS_pmulhw:
        case INS_pmulhuw:
        case INS_pmulhrsw:
        case INS_pmuldq:
        case INS_pmuludq:
        case INS_pmaddwd:
        case INS_pmaddubsw:
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency += PERFSCORE_LATENCY_5C;
            break;

        case INS_cvtsd2ss:
        case INS_cvtps2pd:
        case INS_cvtpd2dq:
        case INS_cvtdq2pd:
        case INS_cvtpd2ps:
        case INS_cvttpd2dq:
        case INS_vtestps:
        case INS_vtestpd:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency += PERFSCORE_LATENCY_5C;
            break;

        case INS_hsubps:
        case INS_hsubpd:
            result.insThroughput = PERFSCORE_THROUGHPUT_2C;
            result.insLatency += PERFSCORE_LATENCY_6C;
            break;

        case INS_pclmulqdq:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency += PERFSCORE_LATENCY_7C;
            break;

        case INS_pmulld:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency += PERFSCORE_LATENCY_10C;
            break;

        case INS_vpbroadcastb:
        case INS_vpbroadcastw:
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                result.insLatency    = PERFSCORE_LATENCY_1C;
            }
            else
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                result.insLatency    = max(PERFSCORE_LATENCY_3C, result.insLatency);
            }
            break;

        case INS_vpbroadcastd:
        case INS_vpbroadcastq:
        case INS_vbroadcasti128:
        case INS_vbroadcastf128:
        case INS_vbroadcastss:
        case INS_vbroadcastsd:
            result.insLatency += PERFSCORE_LATENCY_3C;
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            }
            else
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            }
            break;

        case INS_pinsrb:
        case INS_pinsrw:
        case INS_pinsrd:
        case INS_pinsrq:
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_2C;
                result.insLatency    = PERFSCORE_LATENCY_3C;
            }
            else
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            }
            break;

        case INS_dppd:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = PERFSCORE_LATENCY_9C;
            break;

        case INS_dpps:
            result.insThroughput = PERFSCORE_THROUGHPUT_2C;
            result.insLatency    = PERFSCORE_LATENCY_13C;
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
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            result.insLatency += PERFSCORE_LATENCY_4C;
            break;

        case INS_vmaskmovpd:
        case INS_vmaskmovps:

            if (memAccessKind == PERFSCORE_MEMORY_READ)
            {
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                result.insLatency += PERFSCORE_LATENCY_1C;
            }
            else
            {
                assert(memAccessKind == PERFSCORE_MEMORY_WRITE);
                result.insThroughput = PERFSCORE_THROUGHPUT_1C;
                result.insLatency    = max(PERFSCORE_LATENCY_10C, result.insLatency);
            }
            break;

        case INS_vpmaskmovd:
        case INS_vpmaskmovq:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            result.insLatency    = max(PERFSCORE_LATENCY_4C, result.insLatency);
            break;

        case INS_vpgatherdd:
        case INS_vpgatherdq:
        case INS_vpgatherqd:
        case INS_vpgatherqq:
        case INS_vgatherdps:
        case INS_vgatherdpd:
        case INS_vgatherqps:
        case INS_vgatherqpd:
            result.insThroughput = PERFSCORE_THROUGHPUT_4C;
            result.insLatency    = max(PERFSCORE_LATENCY_4C, result.insLatency);
            break;

        case INS_aesdec:
        case INS_aesdeclast:
        case INS_aesenc:
        case INS_aesenclast:
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            if (memAccessKind == PERFSCORE_MEMORY_NONE)
            {
                result.insLatency = PERFSCORE_LATENCY_4C;
            }
            break;

        case INS_aesimc:
            result.insThroughput = PERFSCORE_THROUGHPUT_2C;
            result.insLatency += PERFSCORE_LATENCY_8C;
            break;

        case INS_aeskeygenassist:
            result.insThroughput = PERFSCORE_THROUGHPUT_13C;
            result.insLatency += PERFSCORE_LATENCY_7C;
            break;

        case INS_lfence:
            result.insThroughput = PERFSCORE_THROUGHPUT_4C;
            break;

        case INS_sfence:
            result.insThroughput = PERFSCORE_THROUGHPUT_6C;
            break;

        case INS_mfence:
            result.insThroughput = PERFSCORE_THROUGHPUT_33C;
            break;

        case INS_prefetcht0:
        case INS_prefetcht1:
        case INS_prefetcht2:
        case INS_prefetchnta:
            result.insThroughput = PERFSCORE_THROUGHPUT_2X;
            break;

        default:
            perfScoreUnhandledInstruction(id, &result);
            break;
    }

    return result;
}

#endif // defined(DEBUG) || defined(LATE_DISASM)
#endif // TARGET_XARCH
