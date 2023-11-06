// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                             emitX86.cpp                                   XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

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

constexpr bool IsImm8(ssize_t imm)
{
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

static_assert_no_msg(INS_imul_AX - INS_imul_AX == REG_EAX);
static_assert_no_msg(INS_imul_BX - INS_imul_AX == REG_EBX);
static_assert_no_msg(INS_imul_CX - INS_imul_AX == REG_ECX);
static_assert_no_msg(INS_imul_DX - INS_imul_AX == REG_EDX);
static_assert_no_msg(INS_imul_BP - INS_imul_AX == REG_EBP);
static_assert_no_msg(INS_imul_SI - INS_imul_AX == REG_ESI);
static_assert_no_msg(INS_imul_DI - INS_imul_AX == REG_EDI);
#ifdef TARGET_AMD64
static_assert_no_msg(INS_imul_08 - INS_imul_AX == REG_R8);
static_assert_no_msg(INS_imul_09 - INS_imul_AX == REG_R9);
static_assert_no_msg(INS_imul_10 - INS_imul_AX == REG_R10);
static_assert_no_msg(INS_imul_11 - INS_imul_AX == REG_R11);
static_assert_no_msg(INS_imul_12 - INS_imul_AX == REG_R12);
static_assert_no_msg(INS_imul_13 - INS_imul_AX == REG_R13);
static_assert_no_msg(INS_imul_14 - INS_imul_AX == REG_R14);
static_assert_no_msg(INS_imul_15 - INS_imul_AX == REG_R15);
#endif

bool emitter::instrIs3opImul(instruction ins)
{
#ifdef TARGET_X86
    return (ins >= INS_imul_AX) && (ins <= INS_imul_DI);
#else
    return (ins >= INS_imul_AX) && (ins <= INS_imul_15);
#endif
}

static bool instrIsExtendedReg3opImul(instruction ins)
{
#ifdef TARGET_X86
    return false;
#else
    return (ins >= INS_imul_08) && (ins <= INS_imul_15);
#endif
}

bool emitter::instrHasImplicitRegPairDest(instruction ins)
{
    return (ins == INS_mulEAX) || (ins == INS_imulEAX) || (ins == INS_div) || (ins == INS_idiv);
}

instruction emitter::inst3opImulForReg(regNumber reg)
{
    assert(genIsValidIntReg(reg));

    instruction ins = instruction(reg + INS_imul_AX);
    assert(instrIs3opImul(ins));
    return ins;
}

static regNumber inst3opImulReg(instruction ins)
{
    regNumber reg = static_cast<regNumber>(ins - INS_imul_AX);
    assert(genIsValidIntReg(reg));
    return reg;
}

bool emitter::IsSSEInstruction(instruction ins)
{
    return (ins >= INS_FIRST_SSE_INSTRUCTION) && (ins <= INS_LAST_SSE_INSTRUCTION);
}

bool emitter::IsSSEOrAVXInstruction(instruction ins)
{
    return (ins >= INS_FIRST_SSE_INSTRUCTION) && (ins <= INS_LAST_AVX_INSTRUCTION);
}

bool emitter::IsAVXOnlyInstruction(instruction ins)
{
    return (ins >= INS_FIRST_AVX_INSTRUCTION) && (ins <= INS_LAST_AVX_INSTRUCTION);
}

bool emitter::IsFMAInstruction(instruction ins)
{
    return (ins >= INS_FIRST_FMA_INSTRUCTION) && (ins <= INS_LAST_FMA_INSTRUCTION);
}

bool emitter::IsAVXVNNIInstruction(instruction ins)
{
    return (ins >= INS_FIRST_AVXVNNI_INSTRUCTION) && (ins <= INS_LAST_AVXVNNI_INSTRUCTION);
}

bool emitter::IsBMIInstruction(instruction ins)
{
    return (ins >= INS_FIRST_BMI_INSTRUCTION) && (ins <= INS_LAST_BMI_INSTRUCTION);
}

regNumber emitter::getBmiRegNumber(instruction ins)
{
    switch (ins)
    {
        case INS_blsi:
        {
            return (regNumber)3;
        }

        case INS_blsmsk:
        {
            return (regNumber)2;
        }

        case INS_blsr:
        {
            return (regNumber)1;
        }

        default:
        {
            assert(IsBMIInstruction(ins));
            return REG_NA;
        }
    }
}

regNumber emitter::getSseShiftRegNumber(instruction ins)
{
    switch (ins)
    {
        case INS_psrldq:
        {
            return (regNumber)3;
        }

        case INS_pslldq:
        {
            return (regNumber)7;
        }

        case INS_psrld:
        case INS_psrlw:
        case INS_psrlq:
        {
            return (regNumber)2;
        }

        case INS_pslld:
        case INS_psllw:
        case INS_psllq:
        {
            return (regNumber)6;
        }

        case INS_psrad:
        case INS_psraw:
        {
            return (regNumber)4;
        }

        default:
        {
            assert(!"Invalid instruction for SSE2 instruction of the form: opcode base, immed8");
            return REG_NA;
        }
    }
}

bool emitter::IsAVXInstruction(instruction ins) const
{
    return UseVEXEncoding() && IsSSEOrAVXInstruction(ins);
}

// Returns true if the AVX instruction is a binary operator that requires 3 operands.
// When we emit an instruction with only two operands, we will duplicate the destination
// as a source.
// TODO-XArch-Cleanup: This is a temporary solution for now. Eventually this needs to
// be formalized by adding an additional field to instruction table to
// to indicate whether a 3-operand instruction.
bool emitter::IsDstDstSrcAVXInstruction(instruction ins)
{
    return ((instInfo[ins] & INS_Flags_IsDstDstSrcAVXInstruction) != 0) && IsAVXInstruction(ins);
}

// Returns true if the AVX instruction requires 3 operands that duplicate the source
// register in the vvvv field.
// TODO-XArch-Cleanup: This is a temporary solution for now. Eventually this needs to
// be formalized by adding an additional field to instruction table to
// to indicate whether a 3-operand instruction.
bool emitter::IsDstSrcSrcAVXInstruction(instruction ins)
{
    return ((instInfo[ins] & INS_Flags_IsDstSrcSrcAVXInstruction) != 0) && IsAVXInstruction(ins);
}

bool emitter::instIsFP(instruction ins)
{
    assert(ins < _countof(instInfo));
#ifdef TARGET_X86
    return (ins == INS_fld) || (ins == INS_fstp);
#else
    return false;
#endif
}

//------------------------------------------------------------------------
// DoesWriteZeroFlag: check if the instruction write the
//     ZF flag.
//
// Arguments:
//    ins - instruction to test
//
// Return Value:
//    true if instruction writes the ZF flag, false otherwise.
//
bool emitter::DoesWriteZeroFlag(instruction ins)
{
    return (emitter::instInfo[ins] & Writes_ZF) != 0;
}

//------------------------------------------------------------------------
// DoesResetOverflowAndCarryFlags: check if the instruction resets the
//     OF and CF flag to 0.
//
// Arguments:
//    ins - instruction to test
//
// Return Value:
//    true if instruction resets the OF and CF flag, false otherwise.
//
bool emitter::DoesResetOverflowAndCarryFlags(instruction ins)
{
    return (emitter::instInfo[ins] & (Resets_OF | Resets_CF)) == (Resets_OF | Resets_CF);
}

//------------------------------------------------------------------------
// IsFlagsAlwaysModified: check if the instruction guarantee to modify any flags.
//
// Arguments:
//    id - instruction to test
//
// Return Value:
//    false, if instruction is guaranteed to not modify any flag.
//    true, if instruction will modify some flag.
//
bool emitter::IsFlagsAlwaysModified(instrDesc* id)
{
    instruction ins = id->idIns();
    insFormat   fmt = id->idInsFmt();

    if (fmt == IF_RRW_SHF)
    {
        if (id->idIsLargeCns())
        {
            return true;
        }
        else if (id->idSmallCns() == 0)
        {
            // If shift-amount is 0, then flags are unaffected.
            return !IsShiftImm(ins);
        }
    }
    else if (fmt == IF_RRW)
    {
        // If shift-amount for is 0, then flags are unaffected.
        // So, to be conservative, do not optimize if the instruction has register
        // as the shift-amount operand.
        return !IsShiftCL(ins);
    }

    return true;
}

//------------------------------------------------------------------------
// AreUpper32BitsZero: check if some previously emitted
//     instruction set the upper 32 bits of reg to zero.
//
// Arguments:
//    reg - register of interest
//
// Return Value:
//    true if previous instruction zeroed reg's upper 32 bits.
//    false if it did not, or if we can't safely determine.
//
// Notes:
//    Currently only looks back one instruction.
//
//    movsx eax, ... might seem viable but we always encode this
//    instruction with a 64 bit destination. See TakesRexWPrefix.

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
        case IF_RRW_SHF:
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

//------------------------------------------------------------------------
// AreFlagsSetToZeroCmp: Checks if the previous instruction set the SZ, and optionally OC, flags to
//                       the same values as if there were a compare to 0
//
// Arguments:
//    reg     - register of interest
//    opSize  - size of register
//    treeOps - type of tree node operation
//
// Return Value:
//    true if the previous instruction set the flags for reg
//    false if not, or if we can't safely determine
//
// Notes:
//    Currently only looks back one instruction.
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
        case IF_RRW_SHF:
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
        if (DoesWriteZeroFlag(id->idIns()) && IsFlagsAlwaysModified(id))
        {
            return id->idOpSize() == opSize;
        }
    }

    return false;
}

//------------------------------------------------------------------------
// IsDstSrcImmAvxInstruction: Checks if the instruction has a "reg, reg/mem, imm" or
//                            "reg/mem, reg, imm" form for the legacy, VEX, and EVEX
//                            encodings.
//
// Arguments:
//    instruction -- processor instruction to check
//
// Return Value:
//    true if instruction has a "reg, reg/mem, imm" or "reg/mem, reg, imm" encoding
//    form for the legacy, VEX, and EVEX encodings.
//
//    That is, the instruction takes two operands, one of which is immediate, and it
//    does not need to encode any data in the VEX.vvvv field.
//
static bool IsDstSrcImmAvxInstruction(instruction ins)
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

// -------------------------------------------------------------------
// Is4ByteSSEInstruction: Returns true if the SSE instruction is a 4-byte opcode.
//
// Arguments:
//    ins  -  instruction
//
// Note that this should be true for any of the instructions in instrsXArch.h
// that use the SSE38 or SSE3A macro but returns false if the VEX encoding is
// in use, since that encoding does not require an additional byte.
bool emitter::Is4ByteSSEInstruction(instruction ins)
{
    return !UseVEXEncoding() && EncodedBySSE38orSSE3A(ins);
}

// Returns true if this instruction requires a VEX prefix
// All AVX instructions require a VEX prefix
bool emitter::TakesVexPrefix(instruction ins) const
{
    // special case vzeroupper as it requires 2-byte VEX prefix
    // special case the fencing, movnti and the prefetch instructions as they never take a VEX prefix
    switch (ins)
    {
        case INS_lfence:
        case INS_mfence:
        case INS_movnti:
        case INS_prefetchnta:
        case INS_prefetcht0:
        case INS_prefetcht1:
        case INS_prefetcht2:
        case INS_sfence:
        case INS_vzeroupper:
            return false;
        default:
            break;
    }

    return IsAVXInstruction(ins);
}

// Add base VEX prefix without setting W, R, X, or B bits
// L bit will be set based on emitter attr.
//
// 2-byte VEX prefix = C5 <R,vvvv,L,pp>
// 3-byte VEX prefix = C4 <R,X,B,m-mmmm> <W,vvvv,L,pp>
//  - R, X, B, W - bits to express corresponding REX prefixes
//  - m-mmmmm (5-bit)
//    0-00001 - implied leading 0F opcode byte
//    0-00010 - implied leading 0F 38 opcode bytes
//    0-00011 - implied leading 0F 3A opcode bytes
//    Rest    - reserved for future use and usage of them will uresult in Undefined instruction exception
//
// - vvvv (4-bits) - register specifier in 1's complement form; must be 1111 if unused
// - L - scalar or AVX-128 bit operations (L=0),  256-bit operations (L=1)
// - pp (2-bits) - opcode extension providing equivalent functionality of a SIMD size prefix
//                 these prefixes are treated mandatory when used with escape opcode 0Fh for
//                 some SIMD instructions
//   00  - None   (0F    - packed float)
//   01  - 66     (66 0F - packed double)
//   10  - F3     (F3 0F - scalar float
//   11  - F2     (F2 0F - scalar double)
#define DEFAULT_3BYTE_VEX_PREFIX 0xC4E07800000000ULL
#define DEFAULT_3BYTE_VEX_PREFIX_MASK 0xFFFFFF00000000ULL
#define LBIT_IN_3BYTE_VEX_PREFIX 0x00000400000000ULL
emitter::code_t emitter::AddVexPrefix(instruction ins, code_t code, emitAttr attr)
{
    // The 2-byte VEX encoding is preferred when possible, but actually emitting
    // it depends on a number of factors that we may not know until much later.
    //
    // In order to handle this "easily", we just carry the 3-byte encoding all
    // the way through and "fix-up" the encoding when the VEX prefix is actually
    // emitted, by simply checking that all the requirements were met.

    // Only AVX instructions require VEX prefix
    assert(IsAVXInstruction(ins));

    // Shouldn't have already added VEX prefix
    assert(!hasVexPrefix(code));

    assert((code & DEFAULT_3BYTE_VEX_PREFIX_MASK) == 0);

    code |= DEFAULT_3BYTE_VEX_PREFIX;

    if (attr == EA_32BYTE)
    {
        // Set L bit to 1 in case of instructions that operate on 256-bits.
        code |= LBIT_IN_3BYTE_VEX_PREFIX;
    }

    return code;
}

// Returns true if this instruction, for the given EA_SIZE(attr), will require a REX.W prefix
bool emitter::TakesRexWPrefix(instruction ins, emitAttr attr)
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

#ifdef TARGET_AMD64
    // movsx should always sign extend out to 8 bytes just because we don't track
    // whether the dest should be 4 bytes or 8 bytes (attr indicates the size
    // of the source, not the dest).
    // A 4-byte movzx is equivalent to an 8 byte movzx, so it is not special
    // cased here.
    //
    // Rex_jmp = jmp with rex prefix always requires rex.w prefix.
    if (ins == INS_movsx || ins == INS_rex_jmp)
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

    // TODO-XArch-Cleanup: Better way to not emit REX.W when we don't need it, than just testing all these
    // opcodes...
    // These are all the instructions that default to 8-byte operand without the REX.W bit
    // With 1 special case: movzx because the 4 byte version still zeros-out the hi 4 bytes
    // so we never need it
    if ((ins != INS_push) && (ins != INS_pop) && (ins != INS_movq) && (ins != INS_movzx) && (ins != INS_push_hide) &&
        (ins != INS_pop_hide) && (ins != INS_ret) && (ins != INS_call) && !((ins >= INS_i_jmp) && (ins <= INS_l_jg)))
    {
        return true;
    }
    else
    {
        return false;
    }
#else  //! TARGET_AMD64 = TARGET_X86
    return false;
#endif //! TARGET_AMD64
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

// Since XMM registers overlap with YMM registers, this routine
// can also used to know whether a YMM register in case of AVX instructions.
bool IsXMMReg(regNumber reg)
{
#ifdef TARGET_AMD64
    return (reg >= REG_XMM0) && (reg <= REG_XMM15);
#else  // !TARGET_AMD64
    return (reg >= REG_XMM0) && (reg <= REG_XMM7);
#endif // !TARGET_AMD64
}

// Returns bits to be encoded in instruction for the given register.
unsigned RegEncoding(regNumber reg)
{
    static_assert((REG_XMM0 & 0x7) == 0, "bad XMMBASE");
    return (unsigned)(reg & 0x7);
}

// Utility routines that abstract the logic of adding REX.W, REX.R, REX.X, REX.B and REX prefixes
// SSE2: separate 1-byte prefix gets added before opcode.
// AVX:  specific bits within VEX prefix need to be set in bit-inverted form.
emitter::code_t emitter::AddRexWPrefix(instruction ins, code_t code)
{
    if (UseVEXEncoding() && IsAVXInstruction(ins))
    {
        if (TakesVexPrefix(ins))
        {
            // W-bit is available only in 3-byte VEX prefix that starts with byte C4.
            assert(hasVexPrefix(code));

            // W-bit is the only bit that is added in non bit-inverted form.
            return emitter::code_t(code | 0x00008000000000ULL);
        }
    }
#ifdef TARGET_AMD64
    return emitter::code_t(code | 0x4800000000ULL);
#else
    assert(!"UNREACHED");
    return code;
#endif
}

#ifdef TARGET_AMD64

emitter::code_t emitter::AddRexRPrefix(instruction ins, code_t code)
{
    if (UseVEXEncoding() && IsAVXInstruction(ins))
    {
        if (TakesVexPrefix(ins))
        {
            // R-bit is supported by both 2-byte and 3-byte VEX prefix
            assert(hasVexPrefix(code));

            // R-bit is added in bit-inverted form.
            return code & 0xFF7FFFFFFFFFFFULL;
        }
    }

    return code | 0x4400000000ULL;
}

emitter::code_t emitter::AddRexXPrefix(instruction ins, code_t code)
{
    if (UseVEXEncoding() && IsAVXInstruction(ins))
    {
        if (TakesVexPrefix(ins))
        {
            // X-bit is available only in 3-byte VEX prefix that starts with byte C4.
            assert(hasVexPrefix(code));

            // X-bit is added in bit-inverted form.
            return code & 0xFFBFFFFFFFFFFFULL;
        }
    }

    return code | 0x4200000000ULL;
}

emitter::code_t emitter::AddRexBPrefix(instruction ins, code_t code)
{
    if (UseVEXEncoding() && IsAVXInstruction(ins))
    {
        if (TakesVexPrefix(ins))
        {
            // B-bit is available only in 3-byte VEX prefix that starts with byte C4.
            assert(hasVexPrefix(code));

            // B-bit is added in bit-inverted form.
            return code & 0xFFDFFFFFFFFFFFULL;
        }
    }

    return code | 0x4100000000ULL;
}

// Adds REX prefix (0x40) without W, R, X or B bits set
emitter::code_t emitter::AddRexPrefix(instruction ins, code_t code)
{
    assert(!UseVEXEncoding() || !IsAVXInstruction(ins));
    return code | 0x4000000000ULL;
}

#endif // TARGET_AMD64

bool isPrefix(BYTE b)
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
    return ((b == 0xF2) || (b == 0xF3) || (b == 0x66));
}

// Outputs VEX prefix (in case of AVX instructions) and REX.R/X/W/B otherwise.
size_t emitter::emitOutputRexOrVexPrefixIfNeeded(instruction ins, BYTE* dst, code_t& code)
{
    if (hasVexPrefix(code))
    {
        // Only AVX instructions should have a VEX prefix
        assert(UseVEXEncoding() && IsAVXInstruction(ins));
        code_t vexPrefix = (code >> 32) & 0x00FFFFFF;
        code &= 0x00000000FFFFFFFFLL;

        WORD leadingBytes = 0;
        BYTE check        = (code >> 24) & 0xFF;
        if (check != 0)
        {
            // 3-byte opcode: with the bytes ordered as 0x2211RM33 or
            // 4-byte opcode: with the bytes ordered as 0x22114433
            // check for a prefix in the 11 position
            BYTE sizePrefix = (code >> 16) & 0xFF;
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
                        if (IsBMIInstruction(ins))
                        {
                            switch (ins)
                            {
                                case INS_rorx:
                                case INS_pdep:
                                case INS_mulx:
                                {
                                    vexPrefix |= 0x03;
                                    break;
                                }

                                case INS_pext:
                                {
                                    vexPrefix |= 0x02;
                                    break;
                                }

                                default:
                                {
                                    vexPrefix |= 0x00;
                                    break;
                                }
                            }
                        }
                        else
                        {
                            vexPrefix |= 0x01;
                        }
                        break;
                    case 0xF3:
                        vexPrefix |= 0x02;
                        break;
                    case 0xF2:
                        vexPrefix |= 0x03;
                        break;
                    default:
                        assert(!"unrecognized SIMD size prefix");
                        unreached();
                }

                // Now the byte in the 22 position must be an escape byte 0F
                leadingBytes = check;
                assert(leadingBytes == 0x0F);

                // Get rid of both sizePrefix and escape byte
                code &= 0x0000FFFFLL;

                // Check the byte in the 33 position to see if it is 3A or 38.
                // In such a case escape bytes must be 0x0F3A or 0x0F38
                check = code & 0xFF;
                if (check == 0x3A || check == 0x38)
                {
                    leadingBytes = (leadingBytes << 8) | check;
                    code &= 0x0000FF00LL;
                }
            }
        }
        else
        {
            // 2-byte opcode with the bytes ordered as 0x0011RM22
            // the byte in position 11 must be an escape byte.
            leadingBytes = (code >> 16) & 0xFF;
            assert(leadingBytes == 0x0F || leadingBytes == 0x00);
            code &= 0xFFFF;
        }

        // If there is an escape byte it must be 0x0F or 0x0F3A or 0x0F38
        // m-mmmmm bits in byte 1 of VEX prefix allows us to encode these
        // implied leading bytes. 0x0F is supported by both the 2-byte and
        // 3-byte encoding. While 0x0F3A and 0x0F38 are only supported by
        // the 3-byte version.

        switch (leadingBytes)
        {
            case 0x00:
                // there is no leading byte
                break;
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
                assert(!"encountered unknown leading bytes");
                unreached();
        }

        // At this point
        //     VEX.2211RM33 got transformed as VEX.0000RM33
        //     VEX.0011RM22 got transformed as VEX.0000RM22
        //
        // Now output VEX prefix leaving the 4-byte opcode

        // The 2-byte VEX encoding, requires that the X and B-bits are set (these
        // bits are inverted from the REX values so set means off), the W-bit is
        // not set (this bit is not inverted), and that the m-mmmm bits are 0-0001
        // (the 2-byte VEX encoding only supports the 0x0F leading byte). When these
        // conditions are met, we can change byte-0 from 0xC4 to 0xC5 and then
        // byte-1 is the logical-or of bit 7 from byte-1 and bits 0-6 from byte 2
        // from the 3-byte VEX encoding.
        //
        // Given the above, the check can be reduced to a simple mask and comparison.
        // * 0xFFFF7F80 is a mask that ignores any bits whose value we don't care about:
        //   * R can be set or unset              (0x7F ignores bit 7)
        //   * vvvv can be any value              (0x80 ignores bits 3-6)
        //   * L can be set or unset              (0x80 ignores bit 2)
        //   * pp can be any value                (0x80 ignores bits 0-1)
        // * 0x00C46100 is a value that signifies the requirements listed above were met:
        //   * We must be a three-byte VEX opcode (0x00C4)
        //   * X and B must be set                (0x61 validates bits 5-6)
        //   * m-mmmm must be 0-00001             (0x61 validates bits 0-4)
        //   * W must be unset                    (0x00 validates bit 7)
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

        emitOutputByte(dst, ((vexPrefix >> 16) & 0xFF));
        emitOutputByte(dst + 1, ((vexPrefix >> 8) & 0xFF));
        emitOutputByte(dst + 2, vexPrefix & 0xFF);
        return 3;
    }

#ifdef TARGET_AMD64
    if (code > 0x00FFFFFFFFLL)
    {
        BYTE prefix = (code >> 32) & 0xFF;
        noway_assert(prefix >= 0x40 && prefix <= 0x4F);
        code &= 0x00000000FFFFFFFFLL;

        // TODO-AMD64-Cleanup: when we remove the prefixes (just the SSE opcodes right now)
        // we can remove this code as well

        // The REX prefix is required to come after all other prefixes.
        // Some of our 'opcodes' actually include some prefixes, if that
        // is the case, shift them over and place the REX prefix after
        // the other prefixes, and emit any prefix that got moved out.
        BYTE check = (code >> 24) & 0xFF;
        if (check == 0)
        {
            // 3-byte opcode: with the bytes ordered as 0x00113322
            // check for a prefix in the 11 position
            check = (code >> 16) & 0xFF;
            if (check != 0 && isPrefix(check))
            {
                // Swap the rex prefix and whatever this prefix is
                code = (((DWORD)prefix << 16) | (code & 0x0000FFFFLL));
                // and then emit the other prefix
                return emitOutputByte(dst, check);
            }
        }
        else
        {
            // 4-byte opcode with the bytes ordered as 0x22114433
            // first check for a prefix in the 11 position
            BYTE check2 = (code >> 16) & 0xFF;
            if (isPrefix(check2))
            {
                assert(!isPrefix(check)); // We currently don't use this, so it is untested
                if (isPrefix(check))
                {
                    // 3 prefixes were rex = rr, check = c1, check2 = c2 encoded as 0xrrc1c2XXXX
                    // Change to c2rrc1XXXX, and emit check2 now
                    code = (((code_t)prefix << 24) | ((code_t)check << 16) | (code & 0x0000FFFFLL));
                }
                else
                {
                    // 2 prefixes were rex = rr, check2 = c2 encoded as 0xrrXXc2XXXX, (check is part of the opcode)
                    // Change to c2XXrrXXXX, and emit check2 now
                    code = (((code_t)check << 24) | ((code_t)prefix << 16) | (code & 0x0000FFFFLL));
                }
                return emitOutputByte(dst, check2);
            }
        }

        return emitOutputByte(dst, prefix);
    }
#endif // TARGET_AMD64

    return 0;
}

// Size of rex prefix in bytes
unsigned emitter::emitGetRexPrefixSize(instruction ins)
{
    // In case of AVX instructions, REX prefixes are part of VEX prefix.
    // And hence requires no additional byte to encode REX prefixes.
    if (IsAVXInstruction(ins))
    {
        return 0;
    }

    // If not AVX, then we would need 1-byte to encode REX prefix.
    return 1;
}

// Size of vex prefix in bytes
unsigned emitter::emitGetVexPrefixSize(instruction ins, emitAttr attr)
{
    if (IsAVXInstruction(ins))
    {
        return 3;
    }

    // If not AVX, then we don't need to encode vex prefix.
    return 0;
}

//------------------------------------------------------------------------
// emitGetAdjustedSize: Determines any size adjustment needed for a given instruction based on the current
// configuration.
//
// Arguments:
//    ins   -- The instruction being emitted
//    attr  -- The emit attribute
//    code  -- The current opcode and any known prefixes
unsigned emitter::emitGetAdjustedSize(instruction ins, emitAttr attr, code_t code)
{
    unsigned adjustedSize = 0;

    if (IsAVXInstruction(ins))
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

        unsigned vexPrefixAdjustedSize = emitGetVexPrefixSize(ins, attr);
        assert(vexPrefixAdjustedSize == 3);

        // In this case, opcode will contains escape prefix at least one byte,
        // vexPrefixAdjustedSize should be minus one.
        vexPrefixAdjustedSize -= 1;

        // Get the fourth byte in Opcode.
        // If this byte is non-zero, then we should check whether the opcode contains SIMD prefix or not.
        BYTE check = (code >> 24) & 0xFF;
        if (check != 0)
        {
            // 3-byte opcode: with the bytes ordered as 0x2211RM33 or
            // 4-byte opcode: with the bytes ordered as 0x22114433
            // Simd prefix is at the first byte.
            BYTE sizePrefix = (code >> 16) & 0xFF;
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
    else if (Is4ByteSSEInstruction(ins))
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

//
//------------------------------------------------------------------------
// emitGetPrefixSize: Get size of rex or vex prefix emitted in code
//
// Arguments:
//    code                  -- The current opcode and any known prefixes
//    includeRexPrefixSize  -- If Rex Prefix size should be included or not
//
unsigned emitter::emitGetPrefixSize(code_t code, bool includeRexPrefixSize)
{
    if (hasVexPrefix(code))
    {
        return 3;
    }

    if (includeRexPrefixSize && hasRexPrefix(code))
    {
        return 1;
    }

    return 0;
}

#ifdef TARGET_X86
/*****************************************************************************
 *
 *  Record a non-empty stack
 */

void emitter::emitMarkStackLvl(unsigned stackLevel)
{
    assert(int(stackLevel) >= 0);
    assert(emitCurStackLvl == 0);
    assert(emitCurIG->igStkLvl == 0);
    assert(emitCurIGfreeNext == emitCurIGfreeBase);

    assert(stackLevel && stackLevel % sizeof(int) == 0);

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
static bool FieldDispRequiresRelocation(CORINFO_FIELD_HANDLE fldHnd)
{
#ifdef WINDOWS_X86_ABI
    return fldHnd != FS_SEG_FIELD;
#else
    return true;
#endif
}
#endif

/*****************************************************************************
 *
 *  Get hold of the address mode displacement value for an indirect call.
 */

inline ssize_t emitter::emitGetInsCIdisp(instrDesc* id)
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

const char* insName(instruction ins)
{
    // clang-format off
    static const char* const insNames[] =
    {
#define INST0(id, nm, um, mr,                 flags) nm,
#define INST1(id, nm, um, mr,                 flags) nm,
#define INST2(id, nm, um, mr, mi,             flags) nm,
#define INST3(id, nm, um, mr, mi, rm,         flags) nm,
#define INST4(id, nm, um, mr, mi, rm, a4,     flags) nm,
#define INST5(id, nm, um, mr, mi, rm, a4, rr, flags) nm,
#include "instrsxarch.h"
    };
    // clang-format on

    assert(ins < _countof(insNames));
    assert(insNames[ins] != nullptr);

    return insNames[ins];
}

// clang-format off
const insFlags emitter::instInfo[]
{
    #define INST0(id, nm, um, mr,                 flags) static_cast<insFlags>(flags),
    #define INST1(id, nm, um, mr,                 flags) static_cast<insFlags>(flags),
    #define INST2(id, nm, um, mr, mi,             flags) static_cast<insFlags>(flags),
    #define INST3(id, nm, um, mr, mi, rm,         flags) static_cast<insFlags>(flags),
    #define INST4(id, nm, um, mr, mi, rm, a4,     flags) static_cast<insFlags>(flags),
    #define INST5(id, nm, um, mr, mi, rm, a4, rr, flags) static_cast<insFlags>(flags),
    #include "instrsxarch.h"
};
// clang-format on

// clang-format off
const BYTE emitter::emitInsModeFmtTab[] =
{
    #define INST0(id, nm, um, mr,                 flags) um,
    #define INST1(id, nm, um, mr,                 flags) um,
    #define INST2(id, nm, um, mr, mi,             flags) um,
    #define INST3(id, nm, um, mr, mi, rm,         flags) um,
    #define INST4(id, nm, um, mr, mi, rm, a4,     flags) um,
    #define INST5(id, nm, um, mr, mi, rm, a4, rr, flags) um,
    #include "instrsxarch.h"
};
// clang-format on

#ifdef DEBUG
unsigned const emitter::emitInsModeFmtCnt = _countof(emitInsModeFmtTab);
#endif

/*****************************************************************************
 *
 *  Combine the given base format with the update mode of the instuction.
 */

emitter::insFormat emitter::emitInsModeFormat(instruction ins, insFormat base)
{
    assert(IF_RRD + IUM_RD == IF_RRD);
    assert(IF_RRD + IUM_WR == IF_RWR);
    assert(IF_RRD + IUM_RW == IF_RRW);

    return (insFormat)(base + emitInsUpdateMode(ins));
}

insUpdateModes emitter::emitInsUpdateMode(instruction ins)
{
    assert((unsigned)ins < emitInsModeFmtCnt);

    return (insUpdateModes)emitInsModeFmtTab[ins];
}

// This is a helper we need due to Vs Whidbey #254016 in order to distinguish
// if we can not possibly be updating an integer register. This is not the best
// solution, but the other ones (see bug) are going to be much more complicated.
bool emitter::emitInsCanOnlyWriteSSE2OrAVXReg(instrDesc* id)
{
    instruction ins = id->idIns();

    if (!IsSSEOrAVXInstruction(ins))
    {
        return false;
    }

    switch (ins)
    {
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
        case INS_extractps:
        case INS_movd:
        case INS_movmskpd:
        case INS_movmskps:
        case INS_mulx:
        case INS_pdep:
        case INS_pext:
        case INS_pmovmskb:
        case INS_pextrb:
        case INS_pextrd:
        case INS_pextrq:
        case INS_pextrw:
        case INS_pextrw_sse41:
        case INS_rorx:
        {
            // These SSE instructions write to a general purpose integer register.
            return false;
        }

        default:
        {
            return true;
        }
    }
}

/*****************************************************************************
 *
 *  Returns the base encoding of the given CPU instruction.
 */

inline size_t insCode(instruction ins)
{
    // clang-format off
    const static
    size_t          insCodes[] =
    {
        #define INST0(id, nm, um, mr,                 flags) mr,
        #define INST1(id, nm, um, mr,                 flags) mr,
        #define INST2(id, nm, um, mr, mi,             flags) mr,
        #define INST3(id, nm, um, mr, mi, rm,         flags) mr,
        #define INST4(id, nm, um, mr, mi, rm, a4,     flags) mr,
        #define INST5(id, nm, um, mr, mi, rm, a4, rr, flags) mr,
        #include "instrsxarch.h"
    };
    // clang-format on

    assert((unsigned)ins < _countof(insCodes));
    assert((insCodes[ins] != BAD_CODE));

    return insCodes[ins];
}

/*****************************************************************************
 *
 *  Returns the "AL/AX/EAX, imm" accumulator encoding of the given instruction.
 */

inline size_t insCodeACC(instruction ins)
{
    // clang-format off
    const static
    size_t          insCodesACC[] =
    {
        #define INST0(id, nm, um, mr,                 flags)
        #define INST1(id, nm, um, mr,                 flags)
        #define INST2(id, nm, um, mr, mi,             flags)
        #define INST3(id, nm, um, mr, mi, rm,         flags)
        #define INST4(id, nm, um, mr, mi, rm, a4,     flags) a4,
        #define INST5(id, nm, um, mr, mi, rm, a4, rr, flags) a4,
        #include "instrsxarch.h"
    };
    // clang-format on

    assert((unsigned)ins < _countof(insCodesACC));
    assert((insCodesACC[ins] != BAD_CODE));

    return insCodesACC[ins];
}

/*****************************************************************************
 *
 *  Returns the "register" encoding of the given CPU instruction.
 */

inline size_t insCodeRR(instruction ins)
{
    // clang-format off
    const static
    size_t          insCodesRR[] =
    {
        #define INST0(id, nm, um, mr,                 flags)
        #define INST1(id, nm, um, mr,                 flags)
        #define INST2(id, nm, um, mr, mi,             flags)
        #define INST3(id, nm, um, mr, mi, rm,         flags)
        #define INST4(id, nm, um, mr, mi, rm, a4,     flags)
        #define INST5(id, nm, um, mr, mi, rm, a4, rr, flags) rr,
        #include "instrsxarch.h"
    };
    // clang-format on

    assert((unsigned)ins < _countof(insCodesRR));
    assert((insCodesRR[ins] != BAD_CODE));

    return insCodesRR[ins];
}

// clang-format off
const static
size_t          insCodesRM[] =
{
    #define INST0(id, nm, um, mr,                 flags)
    #define INST1(id, nm, um, mr,                 flags)
    #define INST2(id, nm, um, mr, mi,             flags)
    #define INST3(id, nm, um, mr, mi, rm,         flags) rm,
    #define INST4(id, nm, um, mr, mi, rm, a4,     flags) rm,
    #define INST5(id, nm, um, mr, mi, rm, a4, rr, flags) rm,
    #include "instrsxarch.h"
};
// clang-format on

// Returns true iff the give CPU instruction has an RM encoding.
inline bool hasCodeRM(instruction ins)
{
    assert((unsigned)ins < _countof(insCodesRM));
    return ((insCodesRM[ins] != BAD_CODE));
}

/*****************************************************************************
 *
 *  Returns the "reg, [r/m]" encoding of the given CPU instruction.
 */

inline size_t insCodeRM(instruction ins)
{
    assert((unsigned)ins < _countof(insCodesRM));
    assert((insCodesRM[ins] != BAD_CODE));

    return insCodesRM[ins];
}

// clang-format off
const static
size_t          insCodesMI[] =
{
    #define INST0(id, nm, um, mr,                 flags)
    #define INST1(id, nm, um, mr,                 flags)
    #define INST2(id, nm, um, mr, mi,             flags) mi,
    #define INST3(id, nm, um, mr, mi, rm,         flags) mi,
    #define INST4(id, nm, um, mr, mi, rm, a4,     flags) mi,
    #define INST5(id, nm, um, mr, mi, rm, a4, rr, flags) mi,
    #include "instrsxarch.h"
};
// clang-format on

// Returns true iff the give CPU instruction has an MI encoding.
inline bool hasCodeMI(instruction ins)
{
    assert((unsigned)ins < _countof(insCodesMI));
    return ((insCodesMI[ins] != BAD_CODE));
}

/*****************************************************************************
 *
 *  Returns the "[r/m], 32-bit icon" encoding of the given CPU instruction.
 */

inline size_t insCodeMI(instruction ins)
{
    assert((unsigned)ins < _countof(insCodesMI));
    assert((insCodesMI[ins] != BAD_CODE));

    return insCodesMI[ins];
}

// clang-format off
const static
size_t          insCodesMR[] =
{
    #define INST0(id, nm, um, mr,                 flags)
    #define INST1(id, nm, um, mr,                 flags) mr,
    #define INST2(id, nm, um, mr, mi,             flags) mr,
    #define INST3(id, nm, um, mr, mi, rm,         flags) mr,
    #define INST4(id, nm, um, mr, mi, rm, a4,     flags) mr,
    #define INST5(id, nm, um, mr, mi, rm, a4, rr, flags) mr,
    #include "instrsxarch.h"
};
// clang-format on

// Returns true iff the give CPU instruction has an MR encoding.
inline bool hasCodeMR(instruction ins)
{
    assert((unsigned)ins < _countof(insCodesMR));
    return ((insCodesMR[ins] != BAD_CODE));
}

/*****************************************************************************
 *
 *  Returns the "[r/m], reg" or "[r/m]" encoding of the given CPU instruction.
 */

inline size_t insCodeMR(instruction ins)
{
    assert((unsigned)ins < _countof(insCodesMR));
    assert((insCodesMR[ins] != BAD_CODE));

    return insCodesMR[ins];
}

// Return true if the instruction uses the SSE38 or SSE3A macro in instrsXArch.h.
bool emitter::EncodedBySSE38orSSE3A(instruction ins)
{
    const size_t SSE38 = 0x0F660038;
    const size_t SSE3A = 0x0F66003A;
    const size_t MASK  = 0xFFFF00FF;

    size_t insCode = 0;

    if (!IsSSEOrAVXInstruction(ins))
    {
        return false;
    }

    if (hasCodeRM(ins))
    {
        insCode = insCodeRM(ins);
    }
    else if (hasCodeMI(ins))
    {
        insCode = insCodeMI(ins);
    }
    else if (hasCodeMR(ins))
    {
        insCode = insCodeMR(ins);
    }

    insCode &= MASK;
    return insCode == SSE38 || insCode == SSE3A;
}

/*****************************************************************************
 *
 *  Returns an encoding for the specified register to be used in the bit0-2
 *  part of an opcode.
 */

inline unsigned emitter::insEncodeReg012(instruction ins, regNumber reg, emitAttr size, code_t* code)
{
    assert(reg < REG_STK);

#ifdef TARGET_AMD64
    // Either code is not NULL or reg is not an extended reg.
    // If reg is an extended reg, instruction needs to be prefixed with 'REX'
    // which would require code != NULL.
    assert(code != nullptr || !IsExtendedReg(reg));

    if (IsExtendedReg(reg))
    {
        *code = AddRexBPrefix(ins, *code); // REX.B
    }
    else if ((EA_SIZE(size) == EA_1BYTE) && (reg > REG_RBX) && (code != nullptr))
    {
        // We are assuming that we only use/encode SPL, BPL, SIL and DIL
        // not the corresponding AH, CH, DH, or BH
        *code = AddRexPrefix(ins, *code); // REX
    }
#endif // TARGET_AMD64

    unsigned regBits = RegEncoding(reg);

    assert(regBits < 8);
    return regBits;
}

/*****************************************************************************
 *
 *  Returns an encoding for the specified register to be used in the bit3-5
 *  part of an opcode.
 */

inline unsigned emitter::insEncodeReg345(instruction ins, regNumber reg, emitAttr size, code_t* code)
{
    assert(reg < REG_STK);

#ifdef TARGET_AMD64
    // Either code is not NULL or reg is not an extended reg.
    // If reg is an extended reg, instruction needs to be prefixed with 'REX'
    // which would require code != NULL.
    assert(code != nullptr || !IsExtendedReg(reg));

    if (IsExtendedReg(reg))
    {
        *code = AddRexRPrefix(ins, *code); // REX.R
    }
    else if ((EA_SIZE(size) == EA_1BYTE) && (reg > REG_RBX) && (code != nullptr))
    {
        // We are assuming that we only use/encode SPL, BPL, SIL and DIL
        // not the corresponding AH, CH, DH, or BH
        *code = AddRexPrefix(ins, *code); // REX
    }
#endif // TARGET_AMD64

    unsigned regBits = RegEncoding(reg);

    assert(regBits < 8);
    return (regBits << 3);
}

/***********************************************************************************
 *
 *  Returns modified AVX opcode with the specified register encoded in bits 3-6 of
 *  byte 2 of VEX prefix.
 */
inline emitter::code_t emitter::insEncodeReg3456(instruction ins, regNumber reg, emitAttr size, code_t code)
{
    assert(reg < REG_STK);
    assert(IsAVXInstruction(ins));
    assert(hasVexPrefix(code));

    // Get 4-bit register encoding
    // RegEncoding() gives lower 3 bits
    // IsExtendedReg() gives MSB.
    code_t regBits = RegEncoding(reg);
    if (IsExtendedReg(reg))
    {
        regBits |= 0x08;
    }

    // VEX prefix encodes register operand in 1's complement form
    // Shift count = 4-bytes of opcode + 0-2 bits
    assert(regBits <= 0xF);
    regBits <<= 35;
    return code ^ regBits;
}

/*****************************************************************************
 *
 *  Returns an encoding for the specified register to be used in the bit3-5
 *  part of an SIB byte (unshifted).
 *  Used exclusively to generate the REX.X bit and truncate the register.
 */

inline unsigned emitter::insEncodeRegSIB(instruction ins, regNumber reg, code_t* code)
{
    assert(reg < REG_STK);

#ifdef TARGET_AMD64
    // Either code is not NULL or reg is not an extended reg.
    // If reg is an extended reg, instruction needs to be prefixed with 'REX'
    // which would require code != NULL.
    assert(code != nullptr || reg < REG_R8 || (reg >= REG_XMM0 && reg < REG_XMM8));

    if (IsExtendedReg(reg))
    {
        *code = AddRexXPrefix(ins, *code); // REX.X
    }
    unsigned regBits = RegEncoding(reg);
#else  // !TARGET_AMD64
    unsigned regBits = reg;
#endif // !TARGET_AMD64

    assert(regBits < 8);
    return regBits;
}

/*****************************************************************************
 *
 *  Returns the "[r/m]" opcode with the mod/RM field set to register.
 */

inline emitter::code_t emitter::insEncodeMRreg(instruction ins, code_t code)
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

/*****************************************************************************
 *
 *  Returns the given "[r/m]" opcode with the mod/RM field set to register.
 */

inline emitter::code_t emitter::insEncodeRMreg(instruction ins, code_t code)
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

/*****************************************************************************
 *
 *  Returns the "byte ptr [r/m]" opcode with the mod/RM field set to
 *  the given register.
 */

inline emitter::code_t emitter::insEncodeMRreg(instruction ins, regNumber reg, emitAttr size, code_t code)
{
    assert((code & 0xC000) == 0);
    code |= 0xC000;
    unsigned regcode = insEncodeReg012(ins, reg, size, &code) << 8;
    code |= regcode;
    return code;
}

/*****************************************************************************
 *
 *  Returns the "byte ptr [r/m], icon" opcode with the mod/RM field set to
 *  the given register.
 */

inline emitter::code_t emitter::insEncodeMIreg(instruction ins, regNumber reg, emitAttr size, code_t code)
{
    assert((code & 0xC000) == 0);
    code |= 0xC000;
    unsigned regcode = insEncodeReg012(ins, reg, size, &code) << 8;
    code |= regcode;
    return code;
}

/*****************************************************************************
 *
 *  Returns true iff the given instruction does not have a "[r/m], icon" form, but *does* have a
 *  "reg,reg,imm8" form.
 */
inline bool insNeedsRRIb(instruction ins)
{
    // If this list gets longer, use a switch or a table.
    return ins == INS_imul;
}

/*****************************************************************************
 *
 *  Returns the "reg,reg,imm8" opcode with both the reg's set to the
 *  the given register.
 */
inline emitter::code_t emitter::insEncodeRRIb(instruction ins, regNumber reg, emitAttr size)
{
    assert(size == EA_4BYTE); // All we handle for now.
    assert(insNeedsRRIb(ins));
    // If this list gets longer, use a switch, or a table lookup.
    code_t   code    = 0x69c0;
    unsigned regcode = insEncodeReg012(ins, reg, size, &code);
    // We use the same register as source and destination.  (Could have another version that does both regs...)
    code |= regcode;
    code |= (regcode << 3);
    return code;
}

/*****************************************************************************
 *
 *  Returns the "+reg" opcode with the the given register set into the low
 *  nibble of the opcode
 */

inline emitter::code_t emitter::insEncodeOpreg(instruction ins, regNumber reg, emitAttr size)
{
    code_t   code    = insCodeRR(ins);
    unsigned regcode = insEncodeReg012(ins, reg, size, &code);
    code |= regcode;
    return code;
}

static unsigned ScaleEncoding(unsigned scale)
{
    assert(scale == 1 || scale == 2 || scale == 4 || scale == 8);

    static constexpr uint8_t scales[]{
        0xFF, // 0
        0x00, // 1
        0x40, // 2
        0xFF, // 3
        0x80, // 4
        0xFF, // 5
        0xFF, // 6
        0xFF, // 7
        0xC0, // 8
    };

    return scales[scale];
}

const instruction emitJumpKindInstructions[] = {INS_nop,

#define JMP_SMALL(en, rev, ins) INS_##ins,
#include "emitjmps.h"

                                                INS_call};

const emitJumpKind emitReverseJumpKinds[] = {
    EJ_NONE,

#define JMP_SMALL(en, rev, ins) EJ_##rev,
#include "emitjmps.h"
};

/*****************************************************************************
 * Look up the instruction for a jump kind
 */

/*static*/ instruction emitter::emitJumpKindToIns(emitJumpKind jumpKind)
{
    assert((unsigned)jumpKind < ArrLen(emitJumpKindInstructions));
    return emitJumpKindInstructions[jumpKind];
}

/*****************************************************************************
 * Reverse the conditional jump
 */

/* static */ emitJumpKind emitter::emitReverseJumpKind(emitJumpKind jumpKind)
{
    assert(jumpKind < EJ_COUNT);
    return emitReverseJumpKinds[jumpKind];
}

/*****************************************************************************
 * When encoding instructions that operate on byte registers
 * we have to ensure that we use a low register (EAX, EBX, ECX or EDX)
 * otherwise we will incorrectly encode the instruction
 */

bool emitter::emitVerifyEncodable(instruction ins, emitAttr size, regNumber reg1, regNumber reg2 /* = REG_NA */)
{
#if CPU_HAS_BYTE_REGS
    if (size != EA_1BYTE) // Not operating on a byte register is fine
    {
        return true;
    }

    if ((ins != INS_movsx) && // These three instructions support high register
        (ins != INS_movzx)    // encodings for reg1
#ifdef FEATURE_HW_INTRINSICS
        && (ins != INS_crc32)
#endif
            )
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
    // The instruction can be encoded
    return true;
}

//------------------------------------------------------------------------
// emitInsSize: Estimate the size (in bytes of generated code) of the given instruction.
//
// Arguments:
//    code  -- The current opcode and any known prefixes
//    includeRexPrefixSize  -- If Rex Prefix size should be included or not
//
inline UNATIVE_OFFSET emitter::emitInsSize(code_t code, bool includeRexPrefixSize)
{
    UNATIVE_OFFSET size = (code & 0xFF000000) ? 4 : (code & 0x00FF0000) ? 3 : 2;
#ifdef TARGET_AMD64
    size += emitGetPrefixSize(code, includeRexPrefixSize);
#endif
    return size;
}

//------------------------------------------------------------------------
// emitInsSizeRR: Determines the code size for an instruction encoding that does not have any addressing modes
//
// Arguments:
//    ins   -- The instruction being emitted
//    code  -- The current opcode and any known prefixes
inline UNATIVE_OFFSET emitter::emitInsSizeRR(instrDesc* id, code_t code)
{
    assert(id->idIns() != INS_invalid);

    instruction ins  = id->idIns();
    emitAttr    attr = id->idOpSize();

    UNATIVE_OFFSET sz = emitGetAdjustedSize(ins, attr, code);

    bool includeRexPrefixSize = true;
    // REX prefix
    if (TakesRexWPrefix(ins, attr) || IsExtendedReg(id->idReg1(), attr) || IsExtendedReg(id->idReg2(), attr) ||
        (!id->idIsSmallDsc() && (IsExtendedReg(id->idReg3(), attr) || IsExtendedReg(id->idReg4(), attr))))
    {
        sz += emitGetRexPrefixSize(ins);
        includeRexPrefixSize = !IsAVXInstruction(ins);
    }

    sz += emitInsSize(code, includeRexPrefixSize);

    return sz;
}

//------------------------------------------------------------------------
// emitInsSizeRR: Determines the code size for an instruction encoding that does not have any addressing modes and
// includes an immediate value
//
// Arguments:
//    ins   -- The instruction being emitted
//    code  -- The current opcode and any known prefixes
//    val   -- The immediate value to encode
inline UNATIVE_OFFSET emitter::emitInsSizeRR(instrDesc* id, code_t code, int val)
{
    instruction    ins       = id->idIns();
    UNATIVE_OFFSET valSize   = EA_SIZE_IN_BYTES(id->idOpSize());
    bool           valInByte = ((signed char)val == val) && (ins != INS_mov) && (ins != INS_test);

#ifdef TARGET_AMD64
    // mov reg, imm64 is the only opcode which takes a full 8 byte immediate
    // all other opcodes take a sign-extended 4-byte immediate
    noway_assert(valSize <= sizeof(INT32) || !id->idIsCnsReloc());
#endif // TARGET_AMD64

    if (valSize > sizeof(INT32))
    {
        valSize = sizeof(INT32);
    }

    if (id->idIsCnsReloc())
    {
        valInByte = false; // relocs can't be placed in a byte
        assert(valSize == sizeof(INT32));
    }

    if (valInByte)
    {
        valSize = sizeof(char);
    }
    else
    {
        assert(!IsSSEOrAVXInstruction(ins));
    }

    return valSize + emitInsSizeRR(id, code);
}

inline UNATIVE_OFFSET emitter::emitInsSizeRR(instruction ins, regNumber reg1, regNumber reg2, emitAttr attr)
{
    emitAttr size = EA_SIZE(attr);

    // If Byte 4 (which is 0xFF00) is zero, that's where the RM encoding goes.
    // Otherwise, it will be placed after the 4 byte encoding, making the total 5 bytes.
    // This would probably be better expressed as a different format or something?
    code_t code = insCodeRM(ins);

    UNATIVE_OFFSET sz = emitGetAdjustedSize(ins, size, insCodeRM(ins));

    bool includeRexPrefixSize = true;
    // REX prefix
    if (!hasRexPrefix(code))
    {
        if ((TakesRexWPrefix(ins, size) && ((ins != INS_xor) || (reg1 != reg2))) || IsExtendedReg(reg1, attr) ||
            IsExtendedReg(reg2, attr))
        {
            sz += emitGetRexPrefixSize(ins);
            includeRexPrefixSize = false;
        }
    }

    if ((code & 0xFF00) != 0)
    {
        sz += IsSSEOrAVXInstruction(ins) ? emitInsSize(code, includeRexPrefixSize) : 5;
    }
    else
    {
        sz += emitInsSize(insEncodeRMreg(ins, code), includeRexPrefixSize);
    }

    return sz;
}

unsigned emitter::emitInsSizeSV_AM(instrDesc* id, code_t code)
{
    unsigned size = emitInsSize(code, /* includeRexPrefixSize */ true);

    bool ebpBased = id->idAddr()->isEbpBased;
    int  disp     = id->idAddr()->lclOffset;

    if (!ebpBased)
    {
#if !FEATURE_FIXED_OUT_ARGS
        disp += emitCurStackLvl;
#endif
        assert(disp >= 0);

        // ESP based addressing modes always require a SIB byte.
        size++;
    }

    // EBP based addressing modes always require displacement.
    if (ebpBased || (disp != 0))
    {
        size += IsDisp8(disp) ? 1 : 4;
    }

    return size;
}

inline UNATIVE_OFFSET emitter::emitInsSizeSV(instrDesc* id, code_t code)
{
    assert(id->idIns() != INS_invalid);
    instruction    ins      = id->idIns();
    emitAttr       attrSize = id->idOpSize();
    UNATIVE_OFFSET prefix   = emitGetAdjustedSize(ins, attrSize, code);

    // REX prefix
    if (TakesRexWPrefix(ins, attrSize) || IsExtendedReg(id->idReg1(), attrSize) ||
        IsExtendedReg(id->idReg2(), attrSize))
    {
        prefix += emitGetRexPrefixSize(ins);
    }

    return prefix + emitInsSizeSV_AM(id, code);
}

inline UNATIVE_OFFSET emitter::emitInsSizeSV(instrDesc* id, code_t code, int val)
{
    assert(id->idIns() != INS_invalid);
    instruction    ins       = id->idIns();
    emitAttr       attrSize  = id->idOpSize();
    UNATIVE_OFFSET valSize   = EA_SIZE_IN_BYTES(attrSize);
    UNATIVE_OFFSET prefix    = emitGetAdjustedSize(ins, attrSize, code);
    bool           valInByte = ((signed char)val == val) && (ins != INS_mov) && (ins != INS_test);

#ifdef TARGET_AMD64
    // mov reg, imm64 is the only opcode which takes a full 8 byte immediate
    // all other opcodes take a sign-extended 4-byte immediate
    noway_assert(valSize <= sizeof(int) || !id->idIsCnsReloc());
#endif // TARGET_AMD64

    if (valSize > sizeof(int))
    {
        valSize = sizeof(int);
    }

    if (id->idIsCnsReloc())
    {
        valInByte = false; // relocs can't be placed in a byte
        assert(valSize == sizeof(int));
    }

    if (valInByte)
    {
        valSize = sizeof(char);
    }
    else
    {
        assert(!IsSSEOrAVXInstruction(ins));
    }

    // 64-bit operand instructions will need a REX.W prefix
    if (TakesRexWPrefix(ins, attrSize) || IsExtendedReg(id->idReg1(), attrSize) ||
        IsExtendedReg(id->idReg2(), attrSize))
    {
        prefix += emitGetRexPrefixSize(ins);
    }

    return prefix + valSize + emitInsSizeSV_AM(id, code);
}

/*****************************************************************************/

static bool baseRegisterRequiresSibByte(regNumber base)
{
#ifdef TARGET_AMD64
    return base == REG_ESP || base == REG_R12;
#else
    return base == REG_ESP;
#endif
}

static bool baseRegisterRequiresDisplacement(regNumber base)
{
#ifdef TARGET_AMD64
    return base == REG_EBP || base == REG_R13;
#else
    return base == REG_EBP;
#endif
}

UNATIVE_OFFSET emitter::emitInsSizeAM(instrDesc* id, code_t code)
{
    assert(id->idIns() != INS_invalid);
    instruction ins      = id->idIns();
    emitAttr    attrSize = id->idOpSize();
    /* The displacement field is in an unusual place for calls */
    ssize_t        dsp       = (ins == INS_call) ? emitGetInsCIdisp(id) : emitGetInsAmdAny(id);
    bool           dspInByte = ((signed char)dsp == (ssize_t)dsp);
    bool           dspIsZero = (dsp == 0);
    UNATIVE_OFFSET size;

    // Note that the values in reg and rgx are used in this method to decide
    // how many bytes will be needed by the address [reg+rgx+cns]
    // this includes the prefix bytes when reg or rgx are registers R8-R15
    regNumber reg;
    regNumber rgx;

    // The idAddr field is a union and only some of the instruction formats use the iiaAddrMode variant
    // these are IF_AWR_*, IF_ARD_*, IF_ARW_* and IF_*_ARD
    // ideally these should really be the only idInsFmts that we see here
    //  but we have some outliers to deal with:
    //     emitIns_R_L adds IF_RWR_LABEL and calls emitInsSizeAM
    //     emitInsRMW adds IF_MRW_CNS, IF_MRW_RRD, IF_MRW_SHF, and calls emitInsSizeAM

    switch (id->idInsFmt())
    {
        case IF_RWR_LABEL:
        case IF_MRW_CNS:
        case IF_MRW_RRD:
        case IF_MRW_SHF:
            reg = REG_NA;
            rgx = REG_NA;
            break;

        default:
            reg = id->idAddr()->iiaAddrMode.amBaseReg;
            rgx = id->idAddr()->iiaAddrMode.amIndxReg;
            break;
    }

    if (id->idIsDspReloc())
    {
        dspInByte = false; // relocs can't be placed in a byte
        dspIsZero = false; // relocs won't always be zero
    }

    if (code & 0xFF000000)
    {
        size = 4;
    }
    else if (code & 0x00FF0000)
    {
        // BT supports 16 bit operands and this code doesn't handle the necessary 66 prefix.
        assert(ins != INS_bt);

        assert((attrSize == EA_4BYTE) || (attrSize == EA_PTRSIZE)    // Only for x64
               || (attrSize == EA_16BYTE) || (attrSize == EA_32BYTE) // only for x64
               || (ins == INS_movzx) || (ins == INS_movsx)
               // The prefetch instructions are always 3 bytes and have part of their modr/m byte hardcoded
               || isPrefetch(ins));
        size = 3;
    }
    else
    {
        size = 2;
    }

    size += emitGetAdjustedSize(ins, attrSize, code);

    if (hasRexPrefix(code))
    {
        // REX prefix
        size += emitGetRexPrefixSize(ins);
    }
    else if (TakesRexWPrefix(ins, attrSize))
    {
        // REX.W prefix
        size += emitGetRexPrefixSize(ins);
    }
    else if (IsExtendedReg(reg, EA_PTRSIZE) || IsExtendedReg(rgx, EA_PTRSIZE) ||
             ((ins != INS_call) && (IsExtendedReg(id->idReg1(), attrSize) || IsExtendedReg(id->idReg2(), attrSize))))
    {
        // Should have a REX byte
        size += emitGetRexPrefixSize(ins);
    }

    if (rgx == REG_NA)
    {
        /* The address is of the form "[reg+disp]" */

        if (reg == REG_NA)
        {
            /* The address is of the form "[disp]" */

            size += sizeof(INT32);

#ifdef TARGET_AMD64
            // If id is not marked for reloc, add 1 additional byte for SIB that follows disp32
            if (!id->idIsDspReloc())
            {
                size++;
            }
#endif
            return size;
        }

        // If the base register is ESP (or R12 on 64-bit systems), a SIB byte must be used.
        if (baseRegisterRequiresSibByte(reg))
        {
            size++;
        }

        // If the base register is EBP (or R13 on 64-bit systems), a displacement is required.
        // Otherwise, the displacement can be elided if it is zero.
        if (dspIsZero && !baseRegisterRequiresDisplacement(reg))
        {
            return size;
        }

        /* Does the offset fit in a byte? */

        if (dspInByte)
        {
            size += sizeof(char);
        }
        else
        {
            size += sizeof(INT32);
        }
    }
    else
    {
        /* An index register is present */

        size++;

        /* Is the index value scaled? */

        if (emitDecodeScale(id->idAddr()->iiaAddrMode.amScale) > 1)
        {
            /* Is there a base register? */

            if (reg != REG_NA)
            {
                /* The address is "[reg + {2/4/8} * rgx + icon]" */

                if (dspIsZero && !baseRegisterRequiresDisplacement(reg))
                {
                    /* The address is "[reg + {2/4/8} * rgx]" */
                }
                else
                {
                    /* The address is "[reg + {2/4/8} * rgx + disp]" */

                    if (dspInByte)
                    {
                        size += sizeof(char);
                    }
                    else
                    {
                        size += sizeof(int);
                    }
                }
            }
            else
            {
                /* The address is "[{2/4/8} * rgx + icon]" */

                size += sizeof(INT32);
            }
        }
        else
        {
            // When we are using the SIB or VSIB format with EBP or R13 as a base, we must emit at least
            // a 1 byte displacement (this is a special case in the encoding to allow for the case of no
            // base register at all). In order to avoid this when we have no scaling, we can reverse the
            // registers so that we don't have to add that extra byte. However, we can't do that if the
            // index register is a vector, such as for a gather instruction.
            //
            if (dspIsZero && baseRegisterRequiresDisplacement(reg) && !baseRegisterRequiresDisplacement(rgx) &&
                !isFloatReg(rgx))
            {
                // Swap reg and rgx, such that reg is not EBP/R13.
                regNumber tmp                       = reg;
                id->idAddr()->iiaAddrMode.amBaseReg = reg = rgx;
                id->idAddr()->iiaAddrMode.amIndxReg = rgx = tmp;
            }

            /* The address is "[reg+rgx+dsp]" */

            if (dspIsZero && !baseRegisterRequiresDisplacement(reg))
            {
                /* This is [reg+rgx]" */
            }
            else
            {
                /* This is [reg+rgx+dsp]" */

                if (dspInByte)
                {
                    size += sizeof(char);
                }
                else
                {
                    size += sizeof(int);
                }
            }
        }
    }

    return size;
}

UNATIVE_OFFSET emitter::emitInsSizeAM(instrDesc* id, code_t code, int val)
{
    assert(id->idIns() != INS_invalid);
    instruction    ins       = id->idIns();
    UNATIVE_OFFSET valSize   = EA_SIZE_IN_BYTES(id->idOpSize());
    bool           valInByte = ((signed char)val == val) && (ins != INS_mov) && (ins != INS_test);

    // We should never generate BT mem,reg because it has poor performance. BT mem,imm might be useful
    // but it requires special handling of the immediate value (it is always encoded in a byte).
    // Let's not complicate things until this is needed.
    assert(ins != INS_bt);

#ifdef TARGET_AMD64
    // mov reg, imm64 is the only opcode which takes a full 8 byte immediate
    // all other opcodes take a sign-extended 4-byte immediate
    noway_assert(valSize <= sizeof(INT32) || !id->idIsCnsReloc());
#endif // TARGET_AMD64

    if (valSize > sizeof(INT32))
    {
        valSize = sizeof(INT32);
    }

    if (id->idIsCnsReloc())
    {
        valInByte = false; // relocs can't be placed in a byte
        assert(valSize == sizeof(INT32));
    }

    if (valInByte)
    {
        valSize = sizeof(char);
    }
    else
    {
        assert(!IsSSEOrAVXInstruction(ins));
    }

    return valSize + emitInsSizeAM(id, code);
}

UNATIVE_OFFSET emitter::emitInsSizeCV(instrDesc* id, code_t code)
{
    assert(id->idIns() != INS_invalid);
    instruction ins      = id->idIns();
    emitAttr    attrSize = id->idOpSize();

    // fgMorph changes any statics that won't fit into 32-bit addresses
    // into constants with an indir, rather than GT_CLS_VAR_ADDR
    // so we should only hit this path for statics that are RIP-relative
    UNATIVE_OFFSET size = sizeof(INT32);

    size += emitGetAdjustedSize(ins, attrSize, code);

    bool includeRexPrefixSize = true;

    // 64-bit operand instructions will need a REX.W prefix
    if (TakesRexWPrefix(ins, attrSize) || IsExtendedReg(id->idReg1(), attrSize) ||
        IsExtendedReg(id->idReg2(), attrSize))
    {
        size += emitGetRexPrefixSize(ins);
        includeRexPrefixSize = false;
    }

    return size + emitInsSize(code, includeRexPrefixSize);
}

UNATIVE_OFFSET emitter::emitInsSizeCV(instrDesc* id, code_t code, int val)
{
    instruction    ins       = id->idIns();
    UNATIVE_OFFSET valSize   = EA_SIZE_IN_BYTES(id->idOpSize());
    bool           valInByte = ((signed char)val == val) && (ins != INS_mov) && (ins != INS_test);

#ifdef TARGET_AMD64
    // 64-bit immediates are only supported on mov r64, imm64
    // As per manual:
    // Support for 64-bit immediate operands is accomplished by expanding
    // the semantics of the existing move (MOV reg, imm16/32) instructions.
    if ((valSize > sizeof(INT32)) && (ins != INS_mov))
        valSize = sizeof(INT32);
#else
    // occasionally longs get here on x86
    if (valSize > sizeof(INT32))
        valSize = sizeof(INT32);
#endif // !TARGET_AMD64

    if (id->idIsCnsReloc())
    {
        valInByte = false; // relocs can't be placed in a byte
        assert(valSize == sizeof(INT32));
    }

    if (valInByte)
    {
        valSize = sizeof(char);
    }
    else
    {
        assert(!IsSSEOrAVXInstruction(ins));
    }

    return valSize + emitInsSizeCV(id, code);
}

emitter::instrDescDsp* emitter::emitAllocInstrDsp(emitAttr attr)
{
#if EMITTER_STATS
    emitTotalIDescDspCnt++;
#endif

    return (instrDescDsp*)emitAllocAnyInstr(sizeof(instrDescDsp), attr);
}

emitter::instrDescCnsDsp* emitter::emitAllocInstrCnsDsp(emitAttr attr)
{
#if EMITTER_STATS
    emitTotalIDescCnsDspCnt++;
#endif

    return (instrDescCnsDsp*)emitAllocAnyInstr(sizeof(instrDescCnsDsp), attr);
}

emitter::instrDescAmd* emitter::emitAllocInstrAmd(emitAttr attr)
{
#if EMITTER_STATS
    emitTotalIDescAmdCnt++;
#endif

    return (instrDescAmd*)emitAllocAnyInstr(sizeof(instrDescAmd), attr);
}

emitter::instrDescCnsAmd* emitter::emitAllocInstrCnsAmd(emitAttr attr)
{
#if EMITTER_STATS
    emitTotalIDescCnsAmdCnt++;
#endif

    return (instrDescCnsAmd*)emitAllocAnyInstr(sizeof(instrDescCnsAmd), attr);
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
        else
        {
            instrDescCns* id = emitAllocInstrCns(size, cns);

#if EMITTER_STATS
            emitLargeCnsCnt++;
            emitSmallDspCnt++;
#endif

            return id;
        }
    }
    else
    {
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
        else
        {
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
    }
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
    else
    {
        instrDescDsp* id = emitAllocInstrDsp(attr);

        id->idSetIsLargeDsp();
        id->iddDspVal = dsp;

#if EMITTER_STATS
        emitLargeDspCnt++;
#endif

        return id;
    }
}

emitter::instrDesc* emitter::emitNewInstrAmd(emitAttr size, ssize_t dsp)
{
    if (dsp < AM_DISP_MIN || dsp > AM_DISP_MAX)
    {
        instrDescAmd* id = emitAllocInstrAmd(size);

        id->idSetIsLargeDsp();
#ifdef DEBUG
        id->idAddr()->iiaAddrMode.amDisp = AM_DISP_BIG_VAL;
#endif
        id->idaAmdVal = dsp;

        return id;
    }
    else
    {
        instrDesc* id = emitAllocInstr(size);

        id->idAddr()->iiaAddrMode.amDisp = dsp;
        assert(id->idAddr()->iiaAddrMode.amDisp == dsp); // make sure the value fit

        return id;
    }
}

/*****************************************************************************
 *
 *  Set the displacement field in an instruction. Only handles instrDescAmd type.
 */

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

/*****************************************************************************
 *
 *  Allocate an instruction descriptor for an instruction that uses both
 *  an address mode displacement and a constant.
 */

emitter::instrDesc* emitter::emitNewInstrAmdCns(emitAttr size, ssize_t dsp, int cns)
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

//-----------------------------------------------------------------------------
//
//  The next instruction will be a loop head entry point
//  So insert an alignment instruction here to ensure that
//  we can properly align the code.
//
void emitter::emitLoopAlign(unsigned short paddingBytes)
{
    /* Insert a pseudo-instruction to ensure that we align
       the next instruction properly */

    assert(paddingBytes <= MAX_ENCODED_SIZE);
    paddingBytes       = min(paddingBytes, MAX_ENCODED_SIZE); // We may need to skip up to 15 bytes of code
    instrDescAlign* id = emitNewInstrAlign();
    id->idCodeSize(paddingBytes);
    id->idaIG = emitCurIG;

    /* Append this instruction to this IG's alignment list */
    id->idaNext = emitCurIGAlignList;

    emitCurIGsize += paddingBytes;
    emitCurIGAlignList = id;
}

//-----------------------------------------------------------------------------
//
//  The next instruction will be a loop head entry point
//  So insert alignment instruction(s) here to ensure that
//  we can properly align the code.
//
//  This emits more than one `INS_align` instruction depending on the
//  alignmentBoundary parameter.
//
void emitter::emitLongLoopAlign(unsigned short alignmentBoundary)
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

    /* Insert a pseudo-instruction to ensure that we align
    the next instruction properly */

    while (insAlignCount)
    {
        emitLoopAlign(MAX_ENCODED_SIZE);
        insAlignCount--;
    }
    emitLoopAlign(lastInsAlignSize);
}

/*****************************************************************************
 *
 *  Add a NOP instruction of the given size.
 */

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

/*****************************************************************************
 *
 *  Add an instruction with no operands.
 */
void emitter::emitIns(instruction ins)
{
    UNATIVE_OFFSET sz;
    instrDesc*     id   = emitNewInstr();
    code_t         code = insCodeMR(ins);

#ifdef DEBUG
    {
        // We cannot have #ifdef inside macro expansion.
        bool assertCond =
            (ins == INS_cdq || ins == INS_int3 || ins == INS_lock || ins == INS_leave || ins == INS_movsb ||
             ins == INS_movsd || ins == INS_movsp || ins == INS_nop || ins == INS_r_movsb || ins == INS_r_movsd ||
             ins == INS_r_movsp || ins == INS_r_stosb || ins == INS_r_stosd || ins == INS_r_stosp || ins == INS_ret ||
             ins == INS_sahf || ins == INS_stosb || ins == INS_stosd || ins == INS_stosp
             // These instructions take zero operands
             || ins == INS_vzeroupper || ins == INS_lfence || ins == INS_mfence || ins == INS_sfence);

        assert(assertCond);
    }
#endif // DEBUG

    assert(!hasRexPrefix(code)); // Can't have a REX bit with no operands, right?

    if (code & 0xFF000000)
    {
        sz = 2; // TODO-XArch-Bug?: Shouldn't this be 4? Or maybe we should assert that we don't see this case.
    }
    else if (code & 0x00FF0000)
    {
        sz = 3;
    }
    else if (code & 0x0000FF00)
    {
        sz = 2;
    }
    else
    {
        sz = 1;
    }

    // vzeroupper includes its 2-byte VEX prefix in its MR code.
    assert((ins != INS_vzeroupper) || (sz == 3));

    insFormat fmt = IF_NONE;

    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idCodeSize(sz);

    dispIns(id);
    emitCurIGsize += sz;
}

// Add an instruction with no operands, but whose encoding depends on the size
// (Only CDQ/CQO currently)
void emitter::emitIns(instruction ins, emitAttr attr)
{
    assert(ins == INS_cdq);

    instrDesc* id   = emitNewInstr(attr);
    code_t     code = insCodeMR(ins);
    assert((code & 0xFFFFFF00) == 0);

    insFormat fmt = IF_NONE;

    unsigned sz = 1 + emitGetAdjustedSize(ins, attr, code);
    if (TakesRexWPrefix(ins, attr))
    {
        sz += emitGetRexPrefixSize(ins);
    }

    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idCodeSize(sz);

    dispIns(id);
    emitCurIGsize += sz;
}

//------------------------------------------------------------------------
// emitMapFmtForIns: map the instruction format based on the instruction.
// Shift-by-a-constant instructions have a special format.
//
// Arguments:
//    fmt - the instruction format to map
//    ins - the instruction
//
// Returns:
//    The mapped instruction format.
//
emitter::insFormat emitter::emitMapFmtForIns(insFormat fmt, instruction ins)
{
    if (IsShiftImm(ins))
    {
        switch (fmt)
        {
            case IF_RRW_CNS:
                return IF_RRW_SHF;
            case IF_MRW_CNS:
                return IF_MRW_SHF;
            case IF_SRW_CNS:
                return IF_SRW_SHF;
            case IF_ARW_CNS:
                return IF_ARW_SHF;
            default:
                unreached();
        }
    }

    return fmt;
}

//------------------------------------------------------------------------
// emitMapFmtAtoM: map the address mode formats ARD, ARW, and AWR to their direct address equivalents.
//
// Arguments:
//    fmt - the instruction format to map
//
// Returns:
//    The mapped instruction format.
//
emitter::insFormat emitter::emitMapFmtAtoM(insFormat fmt)
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

        case IF_ARW_SHF:
            return IF_MRW_SHF;

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

//------------------------------------------------------------------------
// Fill in the address mode fields of the instrDesc.
//
// Assumptions:
//    The correctly sized instrDesc must already be created, e.g., via emitNewInstrAmd() or emitNewInstrAmdCns();
//
// Post-conditions:
//    For base address of int constant:
//        -- the caller must have added the int constant base to the instrDesc when creating it via
//           emitNewInstrAmdCns().
//    For simple address modes (base + scale * index + offset):
//        -- the base register, index register, and scale factor are set.
//        -- the caller must have added the addressing mode offset int constant to the instrDesc when creating it via
//           emitNewInstrAmdCns().
//
//    The instruction format is set.
//
//    idSetIsDspReloc() is called if necessary.
//
void emitter::SetInstrAddrMode(instrDesc* id, insFormat fmt, instruction ins, GenTree* addr)
{
    assert(fmt != IF_NONE);

    if (!addr->isContained())
    {
        id->idInsFmt(emitMapFmtForIns(fmt, ins));
        id->idAddr()->iiaAddrMode.amBaseReg = addr->GetRegNum();
        id->idAddr()->iiaAddrMode.amIndxReg = REG_NA;
        id->idAddr()->iiaAddrMode.amScale   = emitter::OPSZ1;
        assert(emitGetInsAmdAny(id) == 0);

        return;
    }

    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        CORINFO_FIELD_HANDLE fldHnd = clsAddr->GetFieldHandle();
        assert(FieldDispRequiresRelocation(fldHnd));

        id->idInsFmt(emitMapFmtForIns(emitMapFmtAtoM(fmt), ins));
        id->idAddr()->iiaFieldHnd = fldHnd;
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
        id->idAddr()->iiaAddrMode.amScale   = emitter::OPSZ1; // for completeness

        id->idInsFmt(emitMapFmtForIns(fmt, ins));

        // Absolute address must have already been set by the caller.
        assert(emitGetInsAmdAny(id) == intConAddr->GetValue());

        return;
    }

    GenTreeAddrMode* addrMode = addr->AsAddrMode();

    id->idInsFmt(emitMapFmtForIns(fmt, ins));

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
        id->idAddr()->iiaAddrMode.amScale   = emitEncodeScale(addrMode->GetScale());
    }
    else
    {
        id->idAddr()->iiaAddrMode.amIndxReg = REG_NA;
        id->idAddr()->iiaAddrMode.amScale   = OPSZ1;
    }

    // disp must have already been set by the caller.
    assert(emitGetInsAmdAny(id) == addrMode->GetOffset());
}

// Takes care of storing all incoming register parameters
// into its corresponding shadow space (defined by the x64 ABI)
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
        id->idAddr()->iiaAddrMode.amScale   = OPSZ1;
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

    emitAdjustStackDepthPushPop(ins);
}

void emitter::emitIns_A_I(instruction ins, emitAttr attr, GenTree* addr, int imm)
{
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

    unsigned sz = emitInsSizeAM(id, insCodeMI(ins), imm);
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

void emitter::emitInsRMW_A_I(instruction ins, emitAttr attr, GenTree* addr, int imm)
{
    if (IsShiftImm(ins))
    {
        imm &= 0x7F;
    }

    instrDesc* id = emitNewInstrAmdCns(attr, GetAddrModeDisp(addr), imm);
    SetInstrAddrMode(id, IF_ARW_CNS, ins, addr);
    id->idIns(ins);

    unsigned sz = emitInsSizeAM(id, insCodeMI(ins), imm);
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

/*****************************************************************************
 *
 *  Add an instruction referencing a single register.
 */

void emitter::emitIns_R(instruction ins, emitAttr attr, regNumber reg)
{
    emitAttr size = EA_SIZE(attr);

    assert(size <= EA_PTRSIZE);
    noway_assert(emitVerifyEncodable(ins, size, reg));

    unsigned   sz;
    instrDesc* id = emitNewInstrSmall(attr);

    switch (ins)
    {
        case INS_inc:
        case INS_dec:
#ifdef TARGET_AMD64

            sz = 2; // x64 has no 1-byte opcode (it is the same encoding as the REX prefix)

#else // !TARGET_AMD64

            if (size == EA_1BYTE)
                sz = 2; // Use the long form as the small one has no 'w' bit
            else
                sz = 1; // Use short form

#endif // !TARGET_AMD64

            break;

        case INS_pop:
        case INS_pop_hide:
        case INS_push:
        case INS_push_hide:

            /* We don't currently push/pop small values */

            assert(size == EA_PTRSIZE);

            sz = 1;
            break;

        default:

            /* All the sixteen INS_setCCs are contiguous. */

            if (INS_seto <= ins && ins <= INS_setg)
            {
                // Rough check that we used the endpoints for the range check

                assert(INS_seto + 0xF == INS_setg);

                // The caller must specify EA_1BYTE for 'attr'

                assert(attr == EA_1BYTE);

                /* We expect this to always be a 'big' opcode */

                assert(insEncodeMRreg(ins, reg, attr, insCodeMR(ins)) & 0x00FF0000);

                size = attr;

                sz = 3;
                break;
            }
            else
            {
                sz = 2;
                break;
            }
    }
    insFormat fmt = emitInsModeFormat(ins, IF_RRD);

    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idReg1(reg);

    // Vex bytes
    sz += emitGetAdjustedSize(ins, attr, insEncodeMRreg(ins, reg, attr, insCodeMR(ins)));

    // REX byte
    if (IsExtendedReg(reg, attr) || TakesRexWPrefix(ins, attr))
    {
        sz += emitGetRexPrefixSize(ins);
    }

    id->idCodeSize(sz);

    dispIns(id);
    emitCurIGsize += sz;

    emitAdjustStackDepthPushPop(ins);
}

void emitter::emitIns_R_I(instruction ins, emitAttr attr, regNumber reg, ssize_t val DEBUGARG(HandleKind handleKind))
{
    emitAttr size = EA_SIZE(attr);

    // Allow emitting SSE2/AVX SIMD instructions of R_I form that can specify EA_16BYTE or EA_32BYTE
    assert(size <= EA_PTRSIZE || IsSSEOrAVXInstruction(ins));

    noway_assert(emitVerifyEncodable(ins, size, reg));

#ifdef TARGET_AMD64
    // mov reg, imm64 is the only opcode which takes a full 8 byte immediate
    // all other opcodes take a sign-extended 4-byte immediate
    noway_assert(size < EA_8BYTE || ins == INS_mov || ((int)val == val && !EA_IS_CNS_RELOC(attr)));
#endif

    unsigned   sz;
    instrDesc* id;
    insFormat  fmt       = emitInsModeFormat(ins, IF_RRD_CNS);
    bool       valInByte = ((signed char)val == (target_ssize_t)val) && (ins != INS_mov) && (ins != INS_test);

    // BT reg,imm might be useful but it requires special handling of the immediate value
    // (it is always encoded in a byte). Let's not complicate things until this is needed.
    assert(ins != INS_bt);

    // Figure out the size of the instruction
    if (IsShiftImm(ins))
    {
        assert(val != 1);
        fmt = IF_RRW_SHF;
        sz  = 3;
        val &= 0x7F;
        valInByte = true; // shift amount always placed in a byte
    }
    else if (ins == INS_mov)
    {
#ifdef TARGET_AMD64
        // mov reg, imm64 is equivalent to mov reg, imm32 if the high order bits are all 0
        // and this isn't a reloc constant.
        if (((size > EA_4BYTE) && (0 == (val & 0xFFFFFFFF00000000LL))) && !EA_IS_CNS_RELOC(attr))
        {
            attr = size = EA_4BYTE;
        }

        if (size > EA_4BYTE)
        {
            sz = 9; // Really it is 10, but we'll add one more later
        }
        else
#endif // TARGET_AMD64
        {
            sz = 5;
        }
    }
    else
    {
        if (EA_IS_CNS_RELOC(attr))
        {
            valInByte = false; // relocs can't be placed in a byte
        }

        if (valInByte)
        {
            if (IsSSEOrAVXInstruction(ins))
            {
                bool includeRexPrefixSize = true;
                // Do not get the RexSize() but just decide if it will be included down further and if yes,
                // do not include it again.
                if (IsExtendedReg(reg, attr) || TakesRexWPrefix(ins, size) || instrIsExtendedReg3opImul(ins))
                {
                    includeRexPrefixSize = false;
                }

                sz = emitInsSize(insCodeMI(ins), includeRexPrefixSize);
                sz += 1;
            }
            else if (size == EA_1BYTE && reg == REG_EAX && !instrIs3opImul(ins))
            {
                sz = 2;
            }
            else
            {
                sz = 3;
            }
        }
        else
        {
            assert(!IsSSEOrAVXInstruction(ins));

            if (reg == REG_EAX && !instrIs3opImul(ins))
            {
                sz = 1;
            }
            else
            {
                sz = 2;
            }

#ifdef TARGET_AMD64
            if (size > EA_4BYTE)
            {
                // We special-case anything that takes a full 8-byte constant.
                sz += 4;
            }
            else
#endif // TARGET_AMD64
            {
                sz += EA_SIZE_IN_BYTES(attr);
            }
        }
    }

    sz += emitGetAdjustedSize(ins, attr, insCodeMI(ins));

    // Do we need a REX prefix for AMD64? We need one if we are using any extended register (REX.R), or if we have a
    // 64-bit sized operand (REX.W). Note that IMUL in our encoding is special, with a "built-in", implicit, target
    // register. So we also need to check if that built-in register is an extended register.
    if (IsExtendedReg(reg, attr) || TakesRexWPrefix(ins, size) || instrIsExtendedReg3opImul(ins))
    {
        sz += emitGetRexPrefixSize(ins);
    }

    id = emitNewInstrSC(attr, val);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idReg1(reg);
    id->idCodeSize(sz);
    INDEBUG(id->idDebugOnlyInfo()->idHandleKind = handleKind);

    dispIns(id);
    emitCurIGsize += sz;

    if (reg == REG_ESP)
    {
        emitAdjustStackDepth(ins, val);
    }
}

void emitter::emitIns_I(instruction ins, emitAttr attr, ssize_t imm)
{
    unsigned sz;

#ifdef TARGET_AMD64
    // On x64 only LCLHEAP uses push, to allocate and initialize stack memory.
    assert((ins == INS_push_hide) && (attr == EA_8BYTE) && (imm == 0));

    sz = 2;
#else
    if (ins == INS_ret)
    {
        assert((0 <= imm) && (imm <= UINT_MAX));
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
    emitAdjustStackDepthPushPop(ins);
}

void emitter::emitIns_C(instruction ins, emitAttr attr, CORINFO_FIELD_HANDLE fldHnd)
{
    assert(FieldDispRequiresRelocation(fldHnd));

    instrDesc* id = emitNewInstrDsp(attr, 0);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_MRD));
    id->idAddr()->iiaFieldHnd = fldHnd;
    id->idSetIsDspReloc();

    unsigned sz = emitInsSizeCV(id, insCodeMR(ins));

    if (TakesRexWPrefix(ins, attr))
    {
        sz += emitGetRexPrefixSize(ins);
    }

    id->idCodeSize(sz);

    dispIns(id);
    emitCurIGsize += sz;
    emitAdjustStackDepthPushPop(ins);
}

//------------------------------------------------------------------------
// IsMovInstruction: Determines whether a give instruction is a move instruction
//
// Arguments:
//    ins       -- The instruction being checked
//
// Return Value:
//    true if the instruction is a qualifying move instruction; otherwise, false
//
// Remarks:
//    This methods covers most kinds of two operand move instructions that copy a
//    value between two registers. It does not cover all move-like instructions
//    and so doesn't currently cover things like movsb/movsw/movsd/movsq or cmovcc
//    and doesn't currently cover cases where a value is read/written from memory.
//
//    The reason it doesn't cover all instructions was namely to limit the scope
//    of the initial change to that which was impactful to move elision so that
//    it could be centrally managed and optimized. It may be beneficial to support
//    the other move instructions in the future but that may require more extensive
//    changes to ensure relevant codegen/emit paths flow and check things correctly.
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
        case INS_movsdsse2:
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

//------------------------------------------------------------------------
// IsJccInstruction: Determine if an instruction is a conditional jump instruction.
//
// Arguments:
//    ins       -- The instruction being checked
//
// Return Value:
//    true if the instruction qualifies; otherwise, false
//
bool emitter::IsJccInstruction(instruction ins)
{
    return ((ins >= INS_jo) && (ins <= INS_jg)) || ((ins >= INS_l_jo) && (ins <= INS_l_jg));
}

//------------------------------------------------------------------------
// IsJmpInstruction: Determine if an instruction is a jump instruction but NOT a conditional jump instruction.
//
// Arguments:
//    ins       -- The instruction being checked
//
// Return Value:
//    true if the instruction qualifies; otherwise, false
//
bool emitter::IsJmpInstruction(instruction ins)
{
    return
#ifdef TARGET_AMD64
        (ins == INS_rex_jmp) ||
#endif
        (ins == INS_i_jmp) || (ins == INS_jmp) || (ins == INS_l_jmp);
}

//----------------------------------------------------------------------------------------
// IsRedundantMov:
//    Check if the current `mov` instruction is redundant and can be omitted.
//    A `mov` is redundant in following 3 cases:
//
//    1. Move to same register on TARGET_AMD64
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
// Arguments:
//                 ins  - The current instruction
//                 fmt  - The current format
//                 size - Operand size of current instruction
//                 dst  - The current destination
//                 src  - The current source
// canIgnoreSideEffects - The move can be skipped as it doesn't represent special semantics
//
// Return Value:
//    true if the move instruction is redundant; otherwise, false.

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

        case INS_movsdsse2:
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

//------------------------------------------------------------------------
// emitIns_Mov: Emits a move instruction
//
// Arguments:
//    ins       -- The instruction being emitted
//    attr      -- The emit attribute
//    dstReg    -- The destination register
//    srcReg    -- The source register
//    canSkip   -- true if the move can be elided when dstReg == srcReg, otherwise false
//
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
        {
            assert(isGeneralRegister(dstReg) && isGeneralRegister(srcReg));
            break;
        }

        case INS_movapd:
        case INS_movaps:
        case INS_movdqa:
        case INS_movdqu:
        case INS_movsdsse2:
        case INS_movss:
        case INS_movupd:
        case INS_movups:
        {
            assert(isFloatReg(dstReg) && isFloatReg(srcReg));
            break;
        }

        case INS_movd:
        {
            assert(isFloatReg(dstReg) != isFloatReg(srcReg));
            break;
        }

#if defined(TARGET_AMD64)
        case INS_movq:
        {
            assert(isFloatReg(dstReg) && isFloatReg(srcReg));
            break;
        }

        case INS_movsxd:
        {
            assert(isGeneralRegister(dstReg) && isGeneralRegister(srcReg));
            break;
        }
#endif // TARGET_AMD64

        default:
        {
            unreached();
        }
    }
#endif

    emitAttr size = EA_SIZE(attr);

    assert(size <= EA_32BYTE);
    noway_assert(emitVerifyEncodable(ins, size, dstReg, srcReg));

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

/*****************************************************************************
 *
 *  Add an instruction with two register operands.
 */

void emitter::emitIns_R_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2)
{
    if (IsMovInstruction(ins))
    {
        assert(!"Please use emitIns_Mov() to correctly handle move elision");
        emitIns_Mov(ins, attr, reg1, reg2, /* canSkip */ false);
    }

    emitAttr size = EA_SIZE(attr);

    assert(size <= EA_32BYTE);
    noway_assert(emitVerifyEncodable(ins, size, reg1, reg2));

    /* Special case: "XCHG" uses a different format */
    insFormat fmt = (ins == INS_xchg) ? IF_RRW_RRW : emitInsModeFormat(ins, IF_RRD_RRD);

    instrDesc* id = emitNewInstrSmall(attr);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idReg1(reg1);
    id->idReg2(reg2);

    unsigned sz = emitInsSizeRR(ins, reg1, reg2, attr);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

/*****************************************************************************
 *
 *  Add an instruction with two register operands and an integer constant.
 */

void emitter::emitIns_R_R_I(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int ival)
{
#ifdef TARGET_AMD64
    // mov reg, imm64 is the only opcode which takes a full 8 byte immediate
    // all other opcodes take a sign-extended 4-byte immediate
    noway_assert(EA_SIZE(attr) < EA_8BYTE || !EA_IS_CNS_RELOC(attr));
#endif

    instrDesc* id = emitNewInstrSC(attr, ival);

    id->idIns(ins);
    id->idInsFmt(IF_RRW_RRW_CNS);
    id->idReg1(reg1);
    id->idReg2(reg2);

    code_t code = 0;

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
        {
            code = insCodeMR(ins);
            break;
        }

        case INS_psrldq:
        case INS_pslldq:
        {
            code = insCodeMI(ins);
            break;
        }

        default:
        {
            code = insCodeRM(ins);
            break;
        }
    }

    unsigned sz = emitInsSizeRR(id, code, ival);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_AR(instruction ins, emitAttr attr, regNumber base, int disp)
{
    assert(ins == INS_prefetcht0 || ins == INS_prefetcht1 || ins == INS_prefetcht2 || ins == INS_prefetchnta);

    instrDesc* id = emitNewInstrAmd(attr, disp);
    id->idIns(ins);
    id->idInsFmt(IF_ARD);
    id->idAddr()->iiaAddrMode.amBaseReg = base;
    id->idAddr()->iiaAddrMode.amIndxReg = REG_NA;

    unsigned sz = emitInsSizeAM(id, insCodeMR(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

//------------------------------------------------------------------------
// emitIns_AR_R_R: emits the code for an instruction that takes a base memory register, two register operands
//                 and that does not return a value
//
// Arguments:
//    ins       -- The instruction being emitted
//    attr      -- The emit attribute
//    targetReg -- The target register
//    op2Reg    -- The register of the second operand
//    op3Reg    -- The register of the third operand
//    base      -- The base register used for the memory address (first operand)
//    offs      -- The offset from base
//
void emitter::emitIns_AR_R_R(
    instruction ins, emitAttr attr, regNumber op2Reg, regNumber op3Reg, regNumber base, int offs)
{
    assert(IsSSEOrAVXInstruction(ins));
    assert(IsThreeOperandAVXInstruction(ins));

    instrDesc* id = emitNewInstrAmd(attr, offs);
    id->idIns(ins);
    id->idReg1(op2Reg);
    id->idReg2(op3Reg);
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

void emitter::emitIns_R_A_I(instruction ins, emitAttr attr, regNumber reg1, GenTree* addr, int imm)
{
    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        emitIns_R_C_I(ins, attr, reg1, clsAddr->GetFieldHandle(), imm);
        return;
    }

    noway_assert(emitVerifyEncodable(ins, EA_SIZE(attr), reg1));
    assert(IsSSEOrAVXInstruction(ins));

    instrDesc* id = emitNewInstrAmdCns(attr, GetAddrModeDisp(addr), imm);
    id->idIns(ins);
    id->idReg1(reg1);
    SetInstrAddrMode(id, IF_RRW_ARD_CNS, ins, addr);

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins), imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_C_I(instruction ins, emitAttr attr, regNumber reg1, CORINFO_FIELD_HANDLE fldHnd, int imm)
{
    noway_assert(emitVerifyEncodable(ins, EA_SIZE(attr), reg1));
    assert(IsSSEOrAVXInstruction(ins));
    assert(FieldDispRequiresRelocation(fldHnd));

    instrDesc* id = emitNewInstrCnsDsp(attr, imm, 0);
    id->idIns(ins);
    id->idInsFmt(IF_RRW_MRD_CNS);
    id->idReg1(reg1);
    id->idAddr()->iiaFieldHnd = fldHnd;
    id->idSetIsDspReloc();

    unsigned sz = emitInsSizeCV(id, insCodeRM(ins), imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_S_I(instruction ins, emitAttr attr, regNumber reg1, int varx, int offs, int ival)
{
    noway_assert(emitVerifyEncodable(ins, EA_SIZE(attr), reg1));
    assert(IsSSEOrAVXInstruction(ins));

    instrDesc* id = emitNewInstrCns(attr, ival);
    id->idIns(ins);
    id->idInsFmt(IF_RRW_SRD_CNS);
    id->idReg1(reg1);
    SetInstrLclAddrMode(id, varx, offs);

    unsigned sz = emitInsSizeSV(id, insCodeRM(ins), ival);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_A(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, GenTree* addr)
{
    assert(IsSSEOrAVXInstruction(ins));
    assert(IsThreeOperandAVXInstruction(ins));

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

//------------------------------------------------------------------------
// IsAVX2GatherInstruction: return true if the instruction is AVX2 Gather
//
// Arguments:
//    ins - the instruction to check
// Return Value:
//    true if the instruction is AVX2 Gather
//
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

//------------------------------------------------------------------------
// emitIns_R_AR_R: Emits an AVX2 Gather instructions
//
// Arguments:
//    ins - the instruction to emit
//    attr - the instruction operand size
//    reg1 - the destination and first source operand
//    reg2 - the mask operand (encoded in VEX.vvvv)
//    base - the base register of address to load
//    index - the index register of VSIB
//    scale - the scale number of VSIB
//    offs - the offset added to the memory address from base
//
void emitter::emitIns_R_AR_R(instruction ins,
                             emitAttr    attr,
                             regNumber   reg1,
                             regNumber   reg2,
                             regNumber   base,
                             regNumber   index,
                             int         scale,
                             int         offs)
{
    assert(IsAVX2GatherInstruction(ins));

    instrDesc* id = emitNewInstrAmd(attr, offs);
    id->idIns(ins);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idInsFmt(IF_RWR_ARD_RRD);
    id->idAddr()->iiaAddrMode.amBaseReg = base;
    id->idAddr()->iiaAddrMode.amIndxReg = index;
    id->idAddr()->iiaAddrMode.amScale   = emitEncodeSize((emitAttr)scale);

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_C(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, CORINFO_FIELD_HANDLE fldHnd)
{
    assert(IsSSEOrAVXInstruction(ins));
    assert(IsThreeOperandAVXInstruction(ins));
    assert(FieldDispRequiresRelocation(fldHnd));

    instrDesc* id = emitNewInstrDsp(attr, 0);
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_MRD);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idAddr()->iiaFieldHnd = fldHnd;
    id->idSetIsDspReloc();

    unsigned sz = emitInsSizeCV(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

/*****************************************************************************
*
*  Add an instruction with three register operands.
*/

void emitter::emitIns_R_R_R(instruction ins, emitAttr attr, regNumber targetReg, regNumber reg1, regNumber reg2)
{
    assert(IsSSEOrAVXInstruction(ins));
    assert(IsThreeOperandAVXInstruction(ins));

    instrDesc* id = emitNewInstr(attr);
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_RRD);
    id->idReg1(targetReg);
    id->idReg2(reg1);
    id->idReg3(reg2);

    unsigned sz = emitInsSizeRR(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_S(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int varx, int offs)
{
    assert(IsSSEOrAVXInstruction(ins));
    assert(IsThreeOperandAVXInstruction(ins));

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
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, GenTree* addr, int ival, insFormat fmt)
{
    assert(IsSSEOrAVXInstruction(ins));
    assert(IsThreeOperandAVXInstruction(ins));

    instrDesc* id = emitNewInstrAmdCns(attr, GetAddrModeDisp(addr), ival);
    id->idIns(ins);
    id->idReg1(reg1);
    id->idReg2(reg2);

    SetInstrAddrMode(id, fmt, ins, addr);

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins), ival);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_C_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, CORINFO_FIELD_HANDLE fldHnd, int imm)
{
    assert(IsSSEOrAVXInstruction(ins));
    assert(IsThreeOperandAVXInstruction(ins));
    assert(FieldDispRequiresRelocation(fldHnd));

    instrDesc* id = emitNewInstrCnsDsp(attr, imm, 0);
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_MRD_CNS);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idAddr()->iiaFieldHnd = fldHnd;
    id->idSetIsDspReloc();

    unsigned sz = emitInsSizeCV(id, insCodeRM(ins), imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

/**********************************************************************************
* emitIns_R_R_R_I: Add an instruction with three register operands and an immediate.
*
* Arguments:
*    ins       - the instruction to add
*    attr      - the emitter attribute for instruction
*    targetReg - the target (destination) register
*    reg1      - the first source register
*    reg2      - the second source register
*    ival      - the immediate value
*/

void emitter::emitIns_R_R_R_I(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber reg1, regNumber reg2, int ival)
{
    assert(IsSSEOrAVXInstruction(ins));
    assert(IsThreeOperandAVXInstruction(ins));

    instrDesc* id = emitNewInstrCns(attr, ival);
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_RRD_CNS);
    id->idReg1(targetReg);
    id->idReg2(reg1);
    id->idReg3(reg2);

    code_t code = 0;

    switch (ins)
    {
        case INS_pextrb:
        case INS_pextrd:
        case INS_pextrq:
        case INS_pextrw_sse41:
        case INS_extractps:
        case INS_vextractf128:
        case INS_vextracti128:
        {
            code = insCodeMR(ins);
            break;
        }

        case INS_psrldq:
        case INS_pslldq:
        {
            code = insCodeMI(ins);
            break;
        }

        default:
        {
            code = insCodeRM(ins);
            break;
        }
    }

    unsigned sz = emitInsSizeRR(id, code, ival);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_S_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int varx, int offs, int ival)
{
    assert(IsSSEOrAVXInstruction(ins));
    assert(IsThreeOperandAVXInstruction(ins));

    instrDesc* id = emitNewInstrCns(attr, ival);
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_SRD_CNS);
    id->idReg1(reg1);
    id->idReg2(reg2);
    SetInstrLclAddrMode(id, varx, offs);

    unsigned sz = emitInsSizeSV(id, insCodeRM(ins), ival);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

//------------------------------------------------------------------------
// encodeXmmRegAsIval: Encodes a XMM register into imm[7:4] for use by a SIMD instruction
//
// Arguments
//    opReg -- The register being encoded
//
// Returns:
//    opReg encoded in imm[7:4]
static int encodeXmmRegAsIval(regNumber opReg)
{
    // AVX/AVX2 supports 4-reg format for vblendvps/vblendvpd/vpblendvb,
    // which encodes the fourth register into imm8[7:4]
    assert(opReg >= XMMBASE);
    int ival = (opReg - XMMBASE) << 4;

    assert((ival >= 0) && (ival <= 255));
    return (int8_t)ival;
}

void emitter::emitIns_R_R_A_R(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op3Reg, GenTree* addr)
{
    assert(isAvxBlendv(ins));
    assert(UseVEXEncoding());

    int imm = encodeXmmRegAsIval(op3Reg);

    instrDesc* id = emitNewInstrAmdCns(attr, GetAddrModeDisp(addr), imm);
    id->idIns(ins);
    id->idReg1(targetReg);
    id->idReg2(op1Reg);

    SetInstrAddrMode(id, IF_RWR_RRD_ARD_RRD, ins, addr);

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins), imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_C_R(instruction          ins,
                              emitAttr             attr,
                              regNumber            targetReg,
                              regNumber            op1Reg,
                              regNumber            op3Reg,
                              CORINFO_FIELD_HANDLE fldHnd)
{
    assert(isAvxBlendv(ins));
    assert(UseVEXEncoding());
    assert(FieldDispRequiresRelocation(fldHnd));

    int imm = encodeXmmRegAsIval(op3Reg);

    instrDesc* id = emitNewInstrCnsDsp(attr, imm, 0);
    id->idIns(ins);
    id->idReg1(targetReg);
    id->idReg2(op1Reg);
    id->idInsFmt(IF_RWR_RRD_MRD_RRD);
    id->idAddr()->iiaFieldHnd = fldHnd;
    id->idSetIsDspReloc();

    unsigned sz = emitInsSizeCV(id, insCodeRM(ins), imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

//------------------------------------------------------------------------
// emitIns_R_R_R_S: emits the code for a instruction that takes a register operand, a variable index +
//                  offset, another register operand, and that returns a value in register
//
// Arguments:
//    ins       -- The instruction being emitted
//    attr      -- The emit attribute
//    targetReg -- The target register
//    op1Reg    -- The register of the first operand
//    op3Reg    -- The register of the third operand
//    varx      -- The variable index used for the memory address
//    offs      -- The offset added to the memory address from varx
//
// Remarks:
//    op2 is built from varx + offs
//
void emitter::emitIns_R_R_S_R(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op3Reg, int varx, int offs)
{
    assert(isAvxBlendv(ins));
    assert(UseVEXEncoding());

    int imm = encodeXmmRegAsIval(op3Reg);

    instrDesc* id = emitNewInstrCns(attr, imm);
    id->idIns(ins);
    id->idReg1(targetReg);
    id->idReg2(op1Reg);
    id->idInsFmt(IF_RWR_RRD_SRD_RRD);
    SetInstrLclAddrMode(id, varx, offs);

    unsigned sz = emitInsSizeSV(id, insCodeRM(ins), imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_R_R_R(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber reg1, regNumber reg2, regNumber reg3)
{
    assert(isAvxBlendv(ins));
    assert(UseVEXEncoding());

    int imm = encodeXmmRegAsIval(reg3);

    instrDesc* id = emitNewInstrCns(attr, imm);
    id->idIns(ins);
    id->idInsFmt(IF_RWR_RRD_RRD_RRD);
    id->idReg1(targetReg);
    id->idReg2(reg1);
    id->idReg3(reg2);
    id->idReg4(reg3);

    unsigned sz = emitInsSizeRR(id, insCodeRM(ins), imm);
    id->idCodeSize(sz);

    dispIns(id);
    emitCurIGsize += sz;
}

#ifdef WINDOWS_X86_ABI
void emitter::emitInsMov_R_FS(regNumber reg, int offs)
{
    assert(genIsValidIntReg(reg));

    instrDesc* id = emitNewInstrDsp(EA_4BYTE, offs);
    id->idIns(INS_mov);
    id->idInsFmt(emitInsModeFormat(INS_mov, IF_RRD_MRD));
    id->idReg1(reg);
    id->idAddr()->iiaFieldHnd = FS_SEG_FIELD;

    unsigned sz = 1 + ((reg == REG_EAX) ? 4 : emitInsSizeCV(id, insCodeRM(INS_mov)));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}
#endif // WINDOWS_X86_ABI

void emitter::emitIns_R_C(instruction ins, emitAttr attr, regNumber reg, CORINFO_FIELD_HANDLE fldHnd)
{
    assert(FieldDispRequiresRelocation(fldHnd));
    noway_assert(emitVerifyEncodable(ins, EA_SIZE(attr), reg));

    instrDesc* id = emitNewInstrDsp(attr, 0);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_RRD_MRD));
    id->idReg1(reg);
    id->idAddr()->iiaFieldHnd = fldHnd;
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

/*****************************************************************************
 *
 *  Add an instruction with a static member + register operands.
 */

void emitter::emitIns_C_R(instruction ins, emitAttr attr, CORINFO_FIELD_HANDLE fldHnd, regNumber reg)
{
    assert(FieldDispRequiresRelocation(fldHnd));

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

        // REX prefix
        if (TakesRexWPrefix(ins, attr) || IsExtendedReg(reg, attr))
        {
            sz += emitGetRexPrefixSize(ins);
        }
    }
    else
#endif // TARGET_X86
    {
        sz = emitInsSizeCV(id, insCodeMR(ins));
    }

    id->idCodeSize(sz);

    id->idAddr()->iiaFieldHnd = fldHnd;
    id->idSetIsDspReloc();

    dispIns(id);
    emitCurIGsize += sz;
}

/*****************************************************************************
 *
 *  Add an instruction with a static member + constant.
 */

void emitter::emitIns_C_I(instruction ins, emitAttr attr, CORINFO_FIELD_HANDLE fldHnd, int imm)
{
    assert(FieldDispRequiresRelocation(fldHnd));

    insFormat fmt;

    if (IsShiftImm(ins))
    {
        assert(imm != 1);
        fmt = IF_MRW_SHF;
        imm &= 0x7F;
    }
    else
    {
        fmt = emitInsModeFormat(ins, IF_MRD_CNS);
    }

    instrDesc* id = emitNewInstrCnsDsp(attr, imm, 0);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idAddr()->iiaFieldHnd = fldHnd;
    id->idSetIsDspReloc();

    unsigned sz = emitInsSizeCV(id, insCodeMI(ins), imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_L(instruction ins, emitAttr attr, BasicBlock* dst, regNumber reg)
{
    assert(ins == INS_lea);
    assert((dst->bbFlags & BBF_HAS_LABEL) != 0);

    instrDescJmp* id = emitNewInstrJmp();

    id->idIns(INS_lea);
    id->idReg1(reg);
    id->idInsFmt(IF_RWR_LABEL);
    id->idOpSize(EA_SIZE(attr)); // emitNewInstrJmp() sets the size (incorrectly) to EA_1BYTE
    id->idAddr()->iiaBBlabel = dst;
    INDEBUG(id->idDebugOnlyInfo()->idCatchRet = (GetCurrentBlock()->bbJumpKind == BBJ_EHCATCHRET));

    id->idjKeepLong  = true;
    id->idjIG        = emitCurIG;
    id->idjOffs      = emitCurIGsize;
    id->idjNext      = emitCurIGjmpList;
    emitCurIGjmpList = id;

#if EMITTER_STATS
    emitTotalIGjmps++;
#endif

    // Set the relocation flags - these give hint to zap to perform
    // relocation of the specified 32bit address.
    //
    // Note the relocation flags influence the size estimate.
    id->idSetRelocFlags(attr);

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

/*****************************************************************************
 *
 *  The following adds instructions referencing address modes.
 */

void emitter::emitIns_AR_I(instruction ins, emitAttr attr, regNumber base, int disp, int imm)
{
    assert(!instIsFP(ins) && (EA_SIZE(attr) <= EA_8BYTE));

#ifdef TARGET_AMD64
    // mov reg, imm64 is the only opcode which takes a full 8 byte immediate
    // all other opcodes take a sign-extended 4-byte immediate
    noway_assert(EA_SIZE(attr) < EA_8BYTE || !EA_IS_CNS_RELOC(attr));
#endif

    insFormat fmt;

    if (IsShiftImm(ins))
    {
        assert(imm != 1);
        fmt = IF_ARW_SHF;
        imm &= 0x7F;
    }
    else
    {
        fmt = emitInsModeFormat(ins, IF_ARD_CNS);
    }

    instrDesc* id = emitNewInstrAmdCns(attr, disp, imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idAddr()->iiaAddrMode.amBaseReg = base;
    id->idAddr()->iiaAddrMode.amIndxReg = REG_NA;

    unsigned sz = emitInsSizeAM(id, insCodeMI(ins), imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_AR(instruction ins, emitAttr attr, regNumber reg, regNumber base, int disp)
{
    emitIns_R_ARX(ins, attr, reg, base, REG_NA, 1, disp);
}

void emitter::emitIns_R_AI(instruction ins, emitAttr attr, regNumber ireg, ssize_t disp)
{
    assert(!instIsFP(ins) && (EA_SIZE(attr) <= EA_8BYTE) && (ireg != REG_NA));
    noway_assert(emitVerifyEncodable(ins, EA_SIZE(attr), ireg));

    instrDesc* id = emitNewInstrAmd(attr, disp);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_RRD_ARD));
    id->idReg1(ireg);
    id->idAddr()->iiaAddrMode.amBaseReg = REG_NA;
    id->idAddr()->iiaAddrMode.amIndxReg = REG_NA;

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_AR_R(instruction ins, emitAttr attr, regNumber reg, regNumber base, cnsval_ssize_t disp)
{
    emitIns_ARX_R(ins, attr, reg, base, REG_NA, 1, disp);
}

//------------------------------------------------------------------------
// emitIns_S_R_I: emits the code for an instruction that takes a stack operand,
//                a register operand, and an immediate.
//
// Arguments:
//    ins       - The instruction being emitted
//    attr      - The emit attribute
//    varNum    - The varNum of the stack operand
//    offs      - The offset for the stack operand
//    reg       - The register operand
//    ival      - The immediate value
//
void emitter::emitIns_S_R_I(instruction ins, emitAttr attr, int varNum, int offs, regNumber reg, int ival)
{
    // This is only used for INS_vextracti128 and INS_vextractf128, and for these 'ival' must be 0 or 1.
    assert(ins == INS_vextracti128 || ins == INS_vextractf128);
    assert((ival == 0) || (ival == 1));

    instrDesc* id = emitNewInstrAmdCns(attr, 0, ival);
    id->idIns(ins);
    id->idInsFmt(IF_SWR_RRD_CNS);
    id->idReg1(reg);
    SetInstrLclAddrMode(id, varNum, offs);

    unsigned sz = emitInsSizeSV(id, insCodeMR(ins), ival);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_A_R_I(instruction ins, emitAttr attr, GenTree* addr, regNumber reg, int imm)
{
    assert((ins == INS_vextracti128) || (ins == INS_vextractf128));
    assert(attr == EA_32BYTE);
    assert(reg != REG_NA);

    instrDesc* id = emitNewInstrAmdCns(attr, GetAddrModeDisp(addr), imm);
    id->idIns(ins);
    id->idReg1(reg);
    SetInstrAddrMode(id, IF_AWR_RRD_CNS, ins, addr);

    unsigned size = emitInsSizeAM(id, insCodeMR(ins), imm);
    id->idCodeSize(size);
    dispIns(id);
    emitCurIGsize += size;
}

void emitter::emitIns_R_ARR(instruction ins, emitAttr attr, regNumber reg, regNumber base, regNumber index, int disp)
{
    emitIns_R_ARX(ins, attr, reg, base, index, 1, disp);
}

void emitter::emitIns_ARX_I(
    instruction ins, emitAttr attr, regNumber base, regNumber index, unsigned scale, int disp, int imm)
{
    assert(!instIsFP(ins) && (EA_SIZE(attr) <= EA_8BYTE));

#ifdef TARGET_AMD64
    // mov reg, imm64 is the only opcode which takes a full 8 byte immediate
    // all other opcodes take a sign-extended 4-byte immediate
    noway_assert(EA_SIZE(attr) < EA_8BYTE || !EA_IS_CNS_RELOC(attr));
#endif

    insFormat fmt;

    if (IsShiftImm(ins))
    {
        assert(imm != 1);
        fmt = IF_ARW_SHF;
        imm &= 0x7F;
    }
    else
    {
        fmt = emitInsModeFormat(ins, IF_ARD_CNS);
    }

    instrDesc* id = emitNewInstrAmdCns(attr, disp, imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idAddr()->iiaAddrMode.amBaseReg = base;
    id->idAddr()->iiaAddrMode.amIndxReg = index;
    id->idAddr()->iiaAddrMode.amScale   = emitEncodeScale(scale);

    unsigned sz = emitInsSizeAM(id, insCodeMI(ins), imm);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_ARX(
    instruction ins, emitAttr attr, regNumber reg, regNumber base, regNumber index, unsigned scale, int disp)
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
    id->idAddr()->iiaAddrMode.amScale   = emitEncodeScale(scale);

    unsigned sz = emitInsSizeAM(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_ARX(instruction ins, emitAttr attr, regNumber base, regNumber index, unsigned scale, int disp)
{
    emitIns_ARX_R(ins, attr, REG_NA, base, index, scale, disp);
}

void emitter::emitIns_ARX_R(
    instruction ins, emitAttr attr, regNumber reg, regNumber base, regNumber index, unsigned scale, cnsval_ssize_t disp)
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
    id->idAddr()->iiaAddrMode.amScale   = emitEncodeScale(scale);

    unsigned sz = emitInsSizeAM(id, insCodeMR(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;

    emitAdjustStackDepthPushPop(ins);
}

//------------------------------------------------------------------------
// emitIns_SIMD_R_R_I: emits the code for an instruction that takes a register operand, an immediate operand
//                     and that returns a value in register
//
// Arguments:
//    ins       -- The instruction being emitted
//    attr      -- The emit attribute
//    targetReg -- The target register
//    op1Reg    -- The register of the first operand
//    ival      -- The immediate value
//
// Notes:
//    This will handle the required register copy if 'op1Reg' and 'targetReg' are not the same, and
//    the 3-operand format is not available.
//    This is not really SIMD-specific, but is currently only used in that context, as that's
//    where we frequently need to handle the case of generating 3-operand or 2-operand forms
//    depending on what target ISA is supported.
//
void emitter::emitIns_SIMD_R_R_I(instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, int ival)
{
    if (UseVEXEncoding() || IsDstSrcImmAvxInstruction(ins))
    {
        emitIns_R_R_I(ins, attr, targetReg, op1Reg, ival);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
        emitIns_R_I(ins, attr, targetReg, ival);
    }
}

void emitter::emitIns_SIMD_R_R_A(instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, GenTree* addr)
{
    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        emitIns_SIMD_R_R_C(ins, attr, targetReg, op1Reg, clsAddr->GetFieldHandle());
    }
    else if (UseVEXEncoding())
    {
        emitIns_R_R_A(ins, attr, targetReg, op1Reg, addr);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
        emitIns_RRW_A(ins, attr, targetReg, addr);
    }
}

void emitter::emitIns_SIMD_R_R_C(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, CORINFO_FIELD_HANDLE fldHnd)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_C(ins, attr, targetReg, op1Reg, fldHnd);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
        emitIns_R_C(ins, attr, targetReg, fldHnd);
    }
}

//------------------------------------------------------------------------
// emitIns_SIMD_R_R_R: emits the code for a SIMD instruction that takes two register operands, and that returns a
//                     value in register
//
// Arguments:
//    ins       -- The instruction being emitted
//    attr      -- The emit attribute
//    targetReg -- The target register
//    op1Reg    -- The register of the first operand
//    op2Reg    -- The register of the second operand
//
void emitter::emitIns_SIMD_R_R_R(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op2Reg)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_R(ins, attr, targetReg, op1Reg, op2Reg);
    }
    else
    {
        // Ensure we aren't overwriting op2
        assert((op2Reg != targetReg) || (op1Reg == targetReg));

        emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);

        if (IsMovInstruction(ins))
        {
            emitIns_Mov(ins, attr, targetReg, op2Reg, /* canSkip */ false);
        }
        else
        {
            emitIns_R_R(ins, attr, targetReg, op2Reg);
        }
    }
}

//------------------------------------------------------------------------
// emitIns_SIMD_R_R_S: emits the code for a SIMD instruction that takes a register operand, a variable index + offset,
//                     and that returns a value in register
//
// Arguments:
//    ins       -- The instruction being emitted
//    attr      -- The emit attribute
//    targetReg -- The target register
//    op1Reg    -- The register of the first operand
//    varx      -- The variable index used for the memory address
//    offs      -- The offset added to the memory address from varx
//
void emitter::emitIns_SIMD_R_R_S(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, int varx, int offs)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_S(ins, attr, targetReg, op1Reg, varx, offs);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
        emitIns_R_S(ins, attr, targetReg, varx, offs);
    }
}

#ifdef FEATURE_HW_INTRINSICS

void emitter::emitIns_SIMD_R_R_A_I(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, GenTree* addr, int imm)
{
    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        emitIns_SIMD_R_R_C_I(ins, attr, targetReg, op1Reg, clsAddr->GetFieldHandle(), imm);
    }
    else if (UseVEXEncoding())
    {
        emitIns_R_R_A_I(ins, attr, targetReg, op1Reg, addr, imm, IF_RWR_RRD_ARD_CNS);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
        emitIns_R_A_I(ins, attr, targetReg, addr, imm);
    }
}

void emitter::emitIns_SIMD_R_R_C_I(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, CORINFO_FIELD_HANDLE fldHnd, int imm)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_C_I(ins, attr, targetReg, op1Reg, fldHnd, imm);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
        emitIns_R_C_I(ins, attr, targetReg, fldHnd, imm);
    }
}

//------------------------------------------------------------------------
// emitIns_SIMD_R_R_R_I: emits the code for a SIMD instruction that takes two register operands, an immediate operand,
//                       and that returns a value in register
//
// Arguments:
//    ins       -- The instruction being emitted
//    attr      -- The emit attribute
//    targetReg -- The target register
//    op1Reg    -- The register of the first operand
//    op2Reg    -- The register of the second operand
//    ival      -- The immediate value
//
void emitter::emitIns_SIMD_R_R_R_I(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op2Reg, int ival)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_R_I(ins, attr, targetReg, op1Reg, op2Reg, ival);
    }
    else
    {
        // Ensure we aren't overwriting op2
        assert((op2Reg != targetReg) || (op1Reg == targetReg));

        emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
        emitIns_R_R_I(ins, attr, targetReg, op2Reg, ival);
    }
}

//------------------------------------------------------------------------
// emitIns_SIMD_R_R_S_I: emits the code for a SIMD instruction that takes a register operand, a variable index + offset,
//                       an imediate operand, and that returns a value in register
//
// Arguments:
//    ins       -- The instruction being emitted
//    attr      -- The emit attribute
//    targetReg -- The target register
//    op1Reg    -- The register of the first operand
//    varx      -- The variable index used for the memory address
//    offs      -- The offset added to the memory address from varx
//    ival      -- The immediate value
//
void emitter::emitIns_SIMD_R_R_S_I(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, int varx, int offs, int ival)
{
    if (UseVEXEncoding())
    {
        emitIns_R_R_S_I(ins, attr, targetReg, op1Reg, varx, offs, ival);
    }
    else
    {
        emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
        emitIns_R_S_I(ins, attr, targetReg, varx, offs, ival);
    }
}

void emitter::emitIns_SIMD_R_R_R_A(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op2Reg, GenTree* addr)
{
    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        emitIns_SIMD_R_R_R_C(ins, attr, targetReg, op1Reg, op2Reg, clsAddr->GetFieldHandle());
        return;
    }

    assert(IsFMAInstruction(ins) || IsAVXVNNIInstruction(ins));
    assert(UseVEXEncoding());

    // Ensure we aren't overwriting op2
    assert((op2Reg != targetReg) || (op1Reg == targetReg));

    emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
    emitIns_R_R_A(ins, attr, targetReg, op2Reg, addr);
}

void emitter::emitIns_SIMD_R_R_R_C(instruction          ins,
                                   emitAttr             attr,
                                   regNumber            targetReg,
                                   regNumber            op1Reg,
                                   regNumber            op2Reg,
                                   CORINFO_FIELD_HANDLE fldHnd)
{
    assert(IsFMAInstruction(ins));
    assert(UseVEXEncoding());

    // Ensure we aren't overwriting op2
    assert((op2Reg != targetReg) || (op1Reg == targetReg));

    emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
    emitIns_R_R_C(ins, attr, targetReg, op2Reg, fldHnd);
}

//------------------------------------------------------------------------
// emitIns_SIMD_R_R_R_R: emits the code for a SIMD instruction that takes three register operands, and that returns a
//                     value in register
//
// Arguments:
//    ins       -- The instruction being emitted
//    attr      -- The emit attribute
//    targetReg -- The target register
//    op1Reg    -- The register of the first operand
//    op2Reg    -- The register of the second operand
//    op3Reg    -- The register of the second operand
//
void emitter::emitIns_SIMD_R_R_R_R(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op2Reg, regNumber op3Reg)
{
    if (IsFMAInstruction(ins) || IsAVXVNNIInstruction(ins))
    {
        assert(UseVEXEncoding());

        // Ensure we aren't overwriting op2 or op3
        assert((op2Reg != targetReg) || (op1Reg == targetReg));
        assert((op3Reg != targetReg) || (op1Reg == targetReg));

        emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
        emitIns_R_R_R(ins, attr, targetReg, op2Reg, op3Reg);
    }
    else if (UseVEXEncoding())
    {
        assert(isAvxBlendv(ins) || isSse41Blendv(ins));

        // convert SSE encoding of SSE4.1 instructions to VEX encoding
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
        emitIns_R_R_R_R(ins, attr, targetReg, op1Reg, op2Reg, op3Reg);
    }
    else
    {
        assert(isSse41Blendv(ins));

        // Ensure we aren't overwriting op1 or op2
        assert((op1Reg != REG_XMM0) || (op3Reg == REG_XMM0));
        assert((op2Reg != REG_XMM0) || (op3Reg == REG_XMM0));

        // SSE4.1 blendv* hardcode the mask vector (op3) in XMM0
        emitIns_Mov(INS_movaps, attr, REG_XMM0, op3Reg, /* canSkip */ true);

        // Ensure we aren't overwriting op2 or oop3 (which should be REG_XMM0)
        assert((op2Reg != targetReg) || (op1Reg == targetReg));
        assert(targetReg != REG_XMM0);

        emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
        emitIns_R_R(ins, attr, targetReg, op2Reg);
    }
}

//------------------------------------------------------------------------
// emitIns_SIMD_R_R_R_S: emits the code for a SIMD instruction that takes two register operands, a variable index +
//                       offset, and that returns a value in register
//
// Arguments:
//    ins       -- The instruction being emitted
//    attr      -- The emit attribute
//    targetReg -- The target register
//    op1Reg    -- The register of the first operand
//    op2Reg    -- The register of the second operand
//    varx      -- The variable index used for the memory address
//    offs      -- The offset added to the memory address from varx
//
void emitter::emitIns_SIMD_R_R_R_S(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op2Reg, int varx, int offs)
{
    assert(IsFMAInstruction(ins) || IsAVXVNNIInstruction(ins));
    assert(UseVEXEncoding());

    // Ensure we aren't overwriting op2
    assert((op2Reg != targetReg) || (op1Reg == targetReg));

    emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
    emitIns_R_R_S(ins, attr, targetReg, op2Reg, varx, offs);
}

void emitter::emitIns_SIMD_R_R_A_R(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op3Reg, GenTree* addr)
{
    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        emitIns_SIMD_R_R_C_R(ins, attr, targetReg, op1Reg, op3Reg, clsAddr->GetFieldHandle());
    }
    else if (UseVEXEncoding())
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

        emitIns_R_R_A_R(ins, attr, targetReg, op1Reg, op3Reg, addr);
    }
    else
    {
        assert(isSse41Blendv(ins));

        // Ensure we aren't overwriting op1
        assert(op1Reg != REG_XMM0);

        // SSE4.1 blendv* hardcode the mask vector (op3) in XMM0
        emitIns_Mov(INS_movaps, attr, REG_XMM0, op3Reg, /* canSkip */ true);

        // Ensure we aren't overwriting op3 (which should be REG_XMM0)
        assert(targetReg != REG_XMM0);

        emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
        emitIns_RRW_A(ins, attr, targetReg, addr);
    }
}

void emitter::emitIns_SIMD_R_R_C_R(instruction          ins,
                                   emitAttr             attr,
                                   regNumber            targetReg,
                                   regNumber            op1Reg,
                                   regNumber            op3Reg,
                                   CORINFO_FIELD_HANDLE fldHnd)
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

        emitIns_R_R_C_R(ins, attr, targetReg, op1Reg, op3Reg, fldHnd);
    }
    else
    {
        assert(isSse41Blendv(ins));

        // Ensure we aren't overwriting op1
        assert(op1Reg != REG_XMM0);

        // SSE4.1 blendv* hardcode the mask vector (op3) in XMM0
        emitIns_Mov(INS_movaps, attr, REG_XMM0, op3Reg, /* canSkip */ true);

        // Ensure we aren't overwriting op3 (which should be REG_XMM0)
        assert(targetReg != REG_XMM0);

        emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
        emitIns_R_C(ins, attr, targetReg, fldHnd);
    }
}

//------------------------------------------------------------------------
// emitIns_SIMD_R_R_S_R: emits the code for a SIMD instruction that takes a register operand, a variable index +
//                       offset, another register operand, and that returns a value in register
//
// Arguments:
//    ins       -- The instruction being emitted
//    attr      -- The emit attribute
//    targetReg -- The target register
//    op1Reg    -- The register of the first operand
//    op3Reg    -- The register of the third operand
//    varx      -- The variable index used for the memory address
//    offs      -- The offset added to the memory address from varx
//
void emitter::emitIns_SIMD_R_R_S_R(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op3Reg, int varx, int offs)
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

        emitIns_R_R_S_R(ins, attr, targetReg, op1Reg, op3Reg, varx, offs);
    }
    else
    {
        assert(isSse41Blendv(ins));

        // Ensure we aren't overwriting op1
        assert(op1Reg != REG_XMM0);

        // SSE4.1 blendv* hardcode the mask vector (op3) in XMM0
        emitIns_Mov(INS_movaps, attr, REG_XMM0, op3Reg, /* canSkip */ true);

        // Ensure we aren't overwriting op3 (which should be REG_XMM0)
        assert(targetReg != REG_XMM0);

        emitIns_Mov(INS_movaps, attr, targetReg, op1Reg, /* canSkip */ true);
        emitIns_R_S(ins, attr, targetReg, varx, offs);
    }
}
#endif // FEATURE_HW_INTRINSICS

/*****************************************************************************
 *
 *  The following add instructions referencing stack-based local variables.
 */

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

    emitAdjustStackDepthPushPop(ins);
}

void emitter::emitIns_S_R(instruction ins, emitAttr attr, regNumber ireg, int varx, int offs)
{
#ifdef TARGET_X86
    if (attr == EA_1BYTE)
    {
        assert(isByteReg(ireg));
    }
#endif

    instrDesc* id = emitNewInstr(attr);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_SRD_RRD));
    id->idReg1(ireg);
    SetInstrLclAddrMode(id, varx, offs);

    unsigned sz = emitInsSizeSV(id, insCodeMR(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_R_S(instruction ins, emitAttr attr, regNumber ireg, int varx, int offs)
{
    noway_assert(emitVerifyEncodable(ins, EA_SIZE(attr), ireg));

    instrDesc* id = emitNewInstr(attr);
    id->idIns(ins);
    id->idInsFmt(emitInsModeFormat(ins, IF_RRD_SRD));
    id->idReg1(ireg);
    SetInstrLclAddrMode(id, varx, offs);

    unsigned sz = emitInsSizeSV(id, insCodeRM(ins));
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

void emitter::emitIns_S_I(instruction ins, emitAttr attr, int varx, int offs, int val)
{
#ifdef TARGET_AMD64
    // mov reg, imm64 is the only opcode which takes a full 8 byte immediate
    // all other opcodes take a sign-extended 4-byte immediate
    noway_assert(EA_SIZE(attr) < EA_8BYTE || !EA_IS_CNS_RELOC(attr));
#endif

    insFormat fmt;

    if (IsShiftImm(ins))
    {
        assert(val != 1);
        fmt = IF_SRW_SHF;
        val &= 0x7F;
    }
    else
    {
        fmt = emitInsModeFormat(ins, IF_SRD_CNS);
    }

    instrDesc* id = emitNewInstrCns(attr, val);
    id->idIns(ins);
    id->idInsFmt(fmt);
    SetInstrLclAddrMode(id, varx, offs);

    unsigned sz = emitInsSizeSV(id, insCodeMI(ins), val);
    id->idCodeSize(sz);
    dispIns(id);
    emitCurIGsize += sz;
}

/*****************************************************************************
 *
 *  Record that a jump instruction uses the short encoding
 *
 */
void emitter::emitSetShortJump(instrDescJmp* id)
{
    if (id->idjKeepLong)
    {
        return;
    }

    id->idjShort = true;
}

/*****************************************************************************
 *
 *  Add a jmp instruction.
 *  When dst is NULL, instrCount specifies number of instructions
 *       to jump: positive is forward, negative is backward.
 */

void emitter::emitIns_J(instruction ins, BasicBlock* dst, int instrCount /* = 0 */)
{
    instrDescJmp* id = emitNewInstrJmp();

    if (dst != nullptr)
    {
        assert(dst->bbFlags & BBF_HAS_LABEL);
        assert(instrCount == 0);
    }
    else
    {
        // Only allow non-label jmps in prolog.
        assert(emitIGisInProlog(emitCurIG));
        assert(instrCount != 0);
    }

    id->idIns(ins);
    id->idInsFmt(IF_LABEL);

    INDEBUG(id->idDebugOnlyInfo()->idFinallyCall =
                (ins == INS_call) && (GetCurrentBlock()->bbJumpKind == BBJ_CALLFINALLY));

    if (dst != nullptr)
    {
        id->idAddr()->iiaBBlabel = dst;
        id->idjKeepLong          = InDifferentRegions(GetCurrentBlock(), dst);
    }
    else
    {
        id->idAddr()->iiaSetInstrCount(instrCount);
        emitSetShortJump(id);
        id->idSetIsBound();
    }

    id->idjIG        = emitCurIG;
    id->idjOffs      = emitCurIGsize;
    id->idjNext      = emitCurIGjmpList;
    emitCurIGjmpList = id;

#if EMITTER_STATS
    emitTotalIGjmps++;
#endif

    /* Figure out the max. size of the jump/call instruction */

    unsigned sz;

    if (ins == INS_call)
    {
        sz = CALL_INST_SIZE;
    }
    else if (ins == INS_push || ins == INS_push_hide)
    {
        // Pushing the address of a basicBlock will need a reloc
        // as the instruction uses the absolute address,
        // not a relative address
        if (emitComp->opts.compReloc)
        {
            id->idSetIsDspReloc();
        }
        sz = PUSH_INST_SIZE;
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
            int            extra;
            UNATIVE_OFFSET srcOffs;
            int            jmpDist;

            assert(JMP_SIZE_SMALL == JCC_SIZE_SMALL);

            /* This is a backward jump - figure out the distance */

            srcOffs = emitCurCodeOffset + emitCurIGsize + JMP_SIZE_SMALL;

            /* Compute the distance estimate */

            jmpDist = srcOffs - tgt->igOffs;
            assert((int)jmpDist > 0);

            /* How much beyond the max. short distance does the jump go? */

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
                /* Wonderful - this jump surely will be short */

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

    emitAdjustStackDepthPushPop(ins);
}

ssize_t emitter::emitGetInsCns(instrDesc* id)
{
    return id->idIsLargeCns() ? ((instrDescCns*)id)->idcCnsVal : id->idSmallCns();
}

ssize_t emitter::emitGetInsDsp(instrDesc* id)
{
    if (id->idIsLargeDsp())
    {
        if (id->idIsLargeCns())
        {
            return ((instrDescCnsDsp*)id)->iddcDspVal;
        }
        return ((instrDescDsp*)id)->iddDspVal;
    }
    return 0;
}

void emitter::emitGetInsCns(instrDesc* id, CnsVal* cv)
{
    cv->cnsReloc = id->idIsCnsReloc();
    if (id->idIsLargeCns())
    {
        cv->cnsVal = ((instrDescCns*)id)->idcCnsVal;
    }
    else
    {
        cv->cnsVal = id->idSmallCns();
    }
}

ssize_t emitter::emitGetInsAmdCns(instrDesc* id, CnsVal* cv)
{
    cv->cnsReloc = id->idIsCnsReloc();
    if (id->idIsLargeDsp())
    {
        if (id->idIsLargeCns())
        {
            cv->cnsVal = ((instrDescCnsAmd*)id)->idacCnsVal;
            return ((instrDescCnsAmd*)id)->idacAmdVal;
        }
        else
        {
            cv->cnsVal = id->idSmallCns();
            return ((instrDescAmd*)id)->idaAmdVal;
        }
    }
    else
    {
        if (id->idIsLargeCns())
        {
            cv->cnsVal = ((instrDescCns*)id)->idcCnsVal;
        }
        else
        {
            cv->cnsVal = id->idSmallCns();
        }

        return id->idAddr()->iiaAddrMode.amDisp;
    }
}

void emitter::emitGetInsDcmCns(instrDesc* id, CnsVal* cv)
{
    cv->cnsReloc = id->idIsCnsReloc();
    if (id->idIsLargeCns())
    {
        if (id->idIsLargeDsp())
        {
            cv->cnsVal = ((instrDescCnsDsp*)id)->iddcCnsVal;
        }
        else
        {
            cv->cnsVal = ((instrDescCns*)id)->idcCnsVal;
        }
    }
    else
    {
        cv->cnsVal = id->idSmallCns();
    }
}

ssize_t emitter::emitGetInsAmdAny(instrDesc* id)
{
    if (id->idIsLargeDsp())
    {
        if (id->idIsLargeCns())
        {
            return ((instrDescCnsAmd*)id)->idacAmdVal;
        }
        return ((instrDescAmd*)id)->idaAmdVal;
    }

    return id->idAddr()->iiaAddrMode.amDisp;
}

#if !FEATURE_FIXED_OUT_ARGS

//------------------------------------------------------------------------
// emitAdjustStackDepthPushPop: Adjust the current and maximum stack depth.
//
// Arguments:
//    ins - the instruction. Only INS_push and INS_pop adjust the stack depth.
//
// Notes:
//    1. Alters emitCurStackLvl and possibly emitMaxStackDepth.
//    2. emitCntStackDepth must be set (0 in prolog/epilog, one DWORD elsewhere)
//
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

//------------------------------------------------------------------------
// emitAdjustStackDepth: Adjust the current and maximum stack depth.
//
// Arguments:
//    ins - the instruction. Only INS_add and INS_sub adjust the stack depth.
//          It is assumed that the add/sub is on the stack pointer.
//    val - the number of bytes to add to or subtract from the stack pointer.
//
// Notes:
//    1. Alters emitCurStackLvl and possibly emitMaxStackDepth.
//    2. emitCntStackDepth must be set (0 in prolog/epilog, one DWORD elsewhere)
//
void emitter::emitAdjustStackDepth(instruction ins, ssize_t val)
{
    // If we're in the prolog or epilog, or otherwise not tracking the stack depth, just return.
    if (emitCntStackDepth == 0)
        return;

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
                           ssize_t argSize,
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
    assert(static_cast<unsigned>(abs(static_cast<int>(argSize))) <= codeGen->genStackLevel);
    assert(argSize % REGSIZE_BYTES == 0);

    int argSlotCount = static_cast<int>(argSize / REGSIZE_BYTES);
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
        id->idInsFmt(IF_ARD);
        id->idAddr()->iiaAddrMode.amBaseReg = amBase;
        id->idAddr()->iiaAddrMode.amIndxReg = amIndex;
        id->idAddr()->iiaAddrMode.amScale   = amScale ? emitEncodeScale(amScale) : emitter::OPSZ1;

        insSize = emitInsSizeAM(id, insCodeMR(INS_call));

        if ((amBase == REG_NA) && (amIndex == REG_NA))
        {
            if (emitComp->opts.compReloc)
            {
                id->idSetIsDspReloc();
            }
            else
            {
                // This addr mode requires an extra SIB byte.
                AMD64_ONLY(insSize++);
            }
        }
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
        noway_assert(argSize <= static_cast<ssize_t>(emitCurStackLvl));

        emitCurStackLvl -= static_cast<int>(argSize);
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

#ifdef DEBUG
// The following called for each recorded instruction -- use for debugging.
void emitter::emitInsSanityCheck(instrDesc* id)
{
    // make certain you only try to put relocs on things that can have them.

    ID_OPS idOp = GetFormatOp(id->idInsFmt());

    if ((idOp == ID_OP_SCNS) && id->idIsLargeCns())
    {
        idOp = ID_OP_CNS;
    }

    if (id->idIsDspReloc())
    {
        assert(idOp == ID_OP_NONE || idOp == ID_OP_AMD || idOp == ID_OP_DSP || idOp == ID_OP_DSP_CNS ||
               idOp == ID_OP_AMD_CNS || idOp == ID_OP_SPEC || idOp == ID_OP_CALL || idOp == ID_OP_JMP);
    }

    if (id->idIsCnsReloc())
    {
        assert(idOp == ID_OP_CNS || idOp == ID_OP_AMD_CNS || idOp == ID_OP_DSP_CNS || idOp == ID_OP_SPEC ||
               idOp == ID_OP_CALL || idOp == ID_OP_JMP);
    }
}
#endif

// Return the allocated size (in bytes) of the given instruction descriptor.
size_t emitter::emitSizeOfInsDsc(instrDesc* id)
{
    if (emitIsScnsInsDsc(id))
    {
        return SMALL_IDSC_SIZE;
    }

    ID_OPS idOp = GetFormatOp(id->idInsFmt());

    // An INS_call instruction may use a "fat" direct/indirect call descriptor
    // except for a local call to a label (i.e. call to a finally)
    // Only ID_OP_CALL and ID_OP_SPEC check for this, so we enforce that the
    //  INS_call instruction always uses one of these idOps

    if (id->idIns() == INS_call)
    {
        assert(idOp == ID_OP_CALL || // is a direct   call
               idOp == ID_OP_SPEC || // is a indirect call
               idOp == ID_OP_JMP);   // is a local call to finally clause
    }

    switch (idOp)
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
        case ID_OP_SPEC:
            if (id->idIsLargeCall())
            {
                /* Must be a "fat" indirect call descriptor */
                return sizeof(instrDescCGCA);
            }

            FALLTHROUGH;

        case ID_OP_SCNS:
        case ID_OP_CNS:
        case ID_OP_DSP:
        case ID_OP_DSP_CNS:
            if (id->idIsLargeCns())
            {
                if (id->idIsLargeDsp())
                {
                    return sizeof(instrDescCnsDsp);
                }
                else
                {
                    return sizeof(instrDescCns);
                }
            }
            else
            {
                if (id->idIsLargeDsp())
                {
                    return sizeof(instrDescDsp);
                }
                else
                {
                    return sizeof(instrDesc);
                }
            }
        case ID_OP_AMD:
        case ID_OP_AMD_CNS:
            if (id->idIsLargeCns())
            {
                if (id->idIsLargeDsp())
                {
                    return sizeof(instrDescCnsAmd);
                }
                else
                {
                    return sizeof(instrDescCns);
                }
            }
            else
            {
                if (id->idIsLargeDsp())
                {
                    return sizeof(instrDescAmd);
                }
                else
                {
                    return sizeof(instrDesc);
                }
            }

        default:
            NO_WAY("unexpected instruction descriptor format");
            break;
    }

    return sizeof(instrDesc);
}

#ifdef DEBUG

const char* emitter::emitRegName(regNumber reg, emitAttr attr)
{
    if (IsXMMReg(reg))
    {
        return EA_SIZE(attr) == EA_32BYTE ? emitYMMregName(reg) : emitXMMregName(reg);
    }

    static char          rb[2][128];
    static unsigned char rbc = 0;

    const char* rn = getRegName(reg);

#ifdef TARGET_AMD64
    char suffix = '\0';

    switch (EA_SIZE(attr))
    {
        case EA_4BYTE:
            if (reg > REG_R15)
            {
                break;
            }

            if (reg > REG_RDI)
            {
                suffix = 'd';
                goto APPEND_SUFFIX;
            }
            rbc        = (rbc + 1) % 2;
            rb[rbc][0] = 'e';
            rb[rbc][1] = rn[1];
            rb[rbc][2] = rn[2];
            rb[rbc][3] = 0;
            rn         = rb[rbc];
            break;

        case EA_2BYTE:
            if (reg > REG_RDI)
            {
                suffix = 'w';
                goto APPEND_SUFFIX;
            }
            rn++;
            break;

        case EA_1BYTE:
            if (reg > REG_RDI)
            {
                suffix = 'b';
            APPEND_SUFFIX:
                rbc        = (rbc + 1) % 2;
                rb[rbc][0] = rn[0];
                rb[rbc][1] = rn[1];
                if (rn[2])
                {
                    assert(rn[3] == 0);
                    rb[rbc][2] = rn[2];
                    rb[rbc][3] = suffix;
                    rb[rbc][4] = 0;
                }
                else
                {
                    rb[rbc][2] = suffix;
                    rb[rbc][3] = 0;
                }
            }
            else
            {
                rbc        = (rbc + 1) % 2;
                rb[rbc][0] = rn[1];
                if (reg < 4)
                {
                    rb[rbc][1] = 'l';
                    rb[rbc][2] = 0;
                }
                else
                {
                    rb[rbc][1] = rn[2];
                    rb[rbc][2] = 'l';
                    rb[rbc][3] = 0;
                }
            }

            rn = rb[rbc];
            break;

        default:
            break;
    }
#endif // TARGET_AMD64

#ifdef TARGET_X86
    assert(strlen(rn) >= 3);

    switch (EA_SIZE(attr))
    {
        case EA_2BYTE:
            rn++;
            break;

        case EA_1BYTE:
            rbc        = (rbc + 1) % 2;
            rb[rbc][0] = rn[1];
            rb[rbc][1] = 'l';
            strcpy_s(&rb[rbc][2], sizeof(rb[0]) - 2, rn + 3);

            rn = rb[rbc];
            break;

        default:
            break;
    }
#endif // TARGET_X86

#if 0
    // The following is useful if you want register names to be tagged with * or ^ representing gcref or byref, respectively,
    // however it's possibly not interesting most of the time.
    if (EA_IS_GCREF(attr) || EA_IS_BYREF(attr))
    {
        if (rn != rb[rbc])
        {
            rbc = (rbc+1)%2;
            strcpy_s(rb[rbc], sizeof(rb[rbc]), rn);
            rn = rb[rbc];
        }

        if (EA_IS_GCREF(attr))
        {
            strcat_s(rb[rbc], sizeof(rb[rbc]), "*");
        }
        else if (EA_IS_BYREF(attr))
        {
            strcat_s(rb[rbc], sizeof(rb[rbc]), "^");
        }
    }
#endif // 0

    return rn;
}

const char* emitter::emitXMMregName(unsigned reg)
{
    static const char* const regNames[] = {
#define REGDEF(name, rnum, mask, sname) "x" sname,
#include "register.h"
    };

    assert(reg < REG_COUNT);
    assert(reg < _countof(regNames));

    return regNames[reg];
}

/*****************************************************************************
 *
 *  Return a string that represents the given YMM register.
 */

const char* emitter::emitYMMregName(unsigned reg)
{
    static const char* const regNames[] = {
#define REGDEF(name, rnum, mask, sname) "y" sname,
#include "register.h"
    };

    assert(reg < REG_COUNT);
    assert(reg < _countof(regNames));

    return regNames[reg];
}

void emitter::emitDispClsVar(instrDesc* id)
{
    CORINFO_FIELD_HANDLE fldHnd = id->idAddr()->iiaFieldHnd;
    ssize_t              offs   = emitGetInsDsp(id);

#ifdef WINDOWS_X86_ABI
    if (fldHnd == FS_SEG_FIELD)
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

    if (IsRoDataField(fldHnd))
    {
        printf("@RWD%02u", GetRoDataOffset(fldHnd));
    }
    else
    {
        printf("classVar[%#x]", emitComp->dspPtr(fldHnd));
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

void emitter::emitDispFrameRef(instrDesc* id, bool asmfm)
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

void emitter::emitDispImm(instrDesc* id, CnsVal val)
{
    if (val.cnsReloc)
    {
        emitDispReloc(val.cnsVal);
    }
    else
    {
        emitDispImm(id, val.cnsVal);
    }
}

void emitter::emitDispImm(instrDesc* id, ssize_t val)
{
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

/*****************************************************************************
 *
 *  Display a reloc value
 *  If we are formatting for a diffable assembly listing don't print the hex value
 *  since it will prevent us from doing assembly diffs
 */
void emitter::emitDispReloc(ssize_t value)
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

void emitter::emitDispAddrMode(instrDesc* id)
{
    ssize_t disp     = (id->idIns() == INS_call) ? emitGetInsCIdisp(id) : emitGetInsAmdAny(id);
    bool    frameRef = false;
    bool    nsep     = false;

    printf("[");

    if (id->idAddr()->iiaAddrMode.amBaseReg != REG_NA)
    {
        printf("%s", emitRegName(id->idAddr()->iiaAddrMode.amBaseReg));
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
        size_t scale = emitDecodeScale(id->idAddr()->iiaAddrMode.amScale);

        if (nsep)
        {
            printf("+");
        }
        if (scale > 1)
        {
            printf("%u*", scale);
        }
        printf("%s", emitRegName(id->idAddr()->iiaAddrMode.amIndxReg));
        nsep = true;
    }

    if (id->idIsDspReloc() && (id->idIns() != INS_i_jmp))
    {
        if (nsep)
        {
            printf("+");
        }
        emitDispReloc(disp);
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

void emitter::emitDispShiftCL(instruction ins)
{
    if (IsShiftCL(ins))
    {
        printf(", cl");
    }
}

/*****************************************************************************
 *
 *  Display (optionally) the bytes for the instruction encoding in hex
 */

void emitter::emitDispInsHex(instrDesc* id, BYTE* code, size_t sz)
{
    // We do not display the instruction hex if we want diff-able disassembly
    if (!emitComp->opts.disDiffable)
    {
#ifdef TARGET_AMD64
        // how many bytes per instruction we format for
        const size_t digits = 10;
#else // TARGET_X86
        const size_t digits = 6;
#endif
        printf(" ");
        for (unsigned i = 0; i < sz; i++)
        {
            printf("%02X", (*((BYTE*)(code + i))));
        }

        if (sz < digits)
        {
            printf("%.*s", 2 * (digits - sz), "                         ");
        }
    }
}

const char* emitSizeStr(emitAttr attr)
{
    if (attr == EA_GCREF)
    {
        return "gword ptr ";
    }

    if (attr == EA_BYREF)
    {
        return "bword ptr ";
    }

    if (EA_IS_DSP_RELOC(attr))
    {
        return "rword ptr ";
    }

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
            return "unknw ptr ";
    }
}

void emitter::emitDispIns(
    instrDesc* id, bool isNew, bool doffs, bool asmfm, unsigned offset, BYTE* code, size_t sz, insGroup* ig)
{
    if (id->idInsFmt() == IF_GC_REG)
    {
        return;
    }

    emitAttr    attr;
    const char* sstr;
    instruction ins = id->idIns();

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

        emitDispInsHex(id, code + writeableOffset, sz);
    }

    sstr = genInsDisplayName(id);
    printf(" %-9s", sstr);

#ifndef HOST_UNIX
    if (strnlen_s(sstr, 10) >= 9)
#else
    if (strnlen(sstr, 10) >= 9)
#endif
    {
        // Make sure there's at least one space after the instruction name, for very long instruction names.
        printf(" ");
    }

    assert((id->idCodeSize() != 0) || InstrHasNoCode(id));

    /* Figure out the operand size */

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
        emitAttr sizeAttr = id->idOpSize();
        attr              = sizeAttr;

        switch (ins)
        {
            case INS_vextractf128:
            case INS_vextracti128:
            case INS_vinsertf128:
            case INS_vinserti128:
            {
                sizeAttr = EA_16BYTE;
                break;
            }

            case INS_pextrb:
            case INS_pinsrb:
            {
                sizeAttr = EA_1BYTE;
                break;
            }

            case INS_pextrw:
            case INS_pextrw_sse41:
            case INS_pinsrw:
            {
                sizeAttr = EA_2BYTE;
                break;
            }

            case INS_extractps:
            case INS_insertps:
            case INS_pextrd:
            case INS_pinsrd:
            {
                sizeAttr = EA_4BYTE;
                break;
            }

            case INS_pextrq:
            case INS_pinsrq:
            {
                sizeAttr = EA_8BYTE;
                break;
            }

            default:
            {
                break;
            }
        }

        sstr = emitSizeStr(sizeAttr);

        if (ins == INS_lea)
        {
#ifdef TARGET_AMD64
            assert((attr == EA_4BYTE) || (attr == EA_8BYTE));
#else
            assert(attr == EA_4BYTE);
#endif
            sstr = "";
        }
    }

    /* Now see what instruction format we've got */

    // First print the implicit register usage
    if (instrIs3opImul(ins))
    {
        regNumber tgtReg = inst3opImulReg(ins);
        printf("%s, ", emitRegName(tgtReg, id->idOpSize()));
    }

    switch (id->idInsFmt())
    {
        ssize_t val;
        CnsVal  cnsVal;

        case IF_CNS:
#ifdef TARGET_AMD64
            assert((ins == INS_push_hide) && (emitGetInsSC(id) == 0) && !id->idIsReloc());
            printf("0");
#else
            emitDispImm(id, CnsVal{emitGetInsSC(id), id->idIsCnsReloc()});
#endif
            break;

        case IF_ARD:
        case IF_AWR:
        case IF_ARW:
            printf("%s", sstr);
            emitDispAddrMode(id);

            if ((ins == INS_call) || (ins == INS_i_jmp))
            {
                assert(id->idInsFmt() == IF_ARD);

                if (id->idDebugOnlyInfo()->idHandle != nullptr)
                {
                    printf("%s", emitComp->eeGetMethodFullName(
                                     static_cast<CORINFO_METHOD_HANDLE>(id->idDebugOnlyInfo()->idHandle)));
                }
            }
            else
            {
                emitDispShiftCL(ins);
            }
            break;

        case IF_RRD_ARD:
        case IF_RWR_ARD:
        case IF_RRW_ARD:
            if (ins == INS_movsx || ins == INS_movzx AMD64_ONLY(|| ins == INS_movsxd))
            {
                attr = EA_PTRSIZE;
            }
            else if ((ins == INS_crc32) && (attr != EA_8BYTE))
            {
                // The destination reg is always 4 bytes.
                attr = EA_4BYTE;
            }
            printf("%s, %s", emitRegName(id->idReg1(), attr), sstr);
            emitDispAddrMode(id);
            break;

        case IF_RRW_ARD_CNS:
        case IF_RWR_ARD_CNS:
            printf("%s, %s", emitRegName(id->idReg1(), attr), sstr);
            emitDispAddrMode(id);
            emitGetInsAmdCns(id, &cnsVal);
            printf(", ");
            emitDispImm(id, cnsVal);
            break;

        case IF_AWR_RRD_CNS:
            assert(ins == INS_vextracti128 || ins == INS_vextractf128);
            // vextracti/f128 extracts 128-bit data, so we fix sstr as "xmm ptr"
            sstr = emitSizeStr(EA_16BYTE);
            printf(sstr);
            emitDispAddrMode(id);
            printf(", %s", emitRegName(id->idReg1(), attr));
            emitGetInsAmdCns(id, &cnsVal);
            printf(", ");
            emitDispImm(id, cnsVal);
            break;

        case IF_RWR_RRD_ARD:
            printf("%s, %s, %s", emitRegName(id->idReg1(), attr), emitRegName(id->idReg2(), attr), sstr);
            emitDispAddrMode(id);
            break;

        case IF_RWR_ARD_RRD:
            if (ins == INS_vpgatherqd || ins == INS_vgatherqps)
            {
                attr = EA_16BYTE;
            }
            sstr = emitSizeStr(EA_ATTR(4));
            printf("%s, %s", emitRegName(id->idReg1(), attr), sstr);
            emitDispAddrMode(id);
            printf(", %s", emitRegName(id->idReg2(), attr));
            break;

        case IF_RWR_RRD_ARD_CNS:
            printf("%s, %s, %s", emitRegName(id->idReg1(), attr), emitRegName(id->idReg2(), attr), sstr);
            emitDispAddrMode(id);
            emitGetInsAmdCns(id, &cnsVal);
            printf(", ");
            emitDispImm(id, cnsVal);
            break;

        case IF_RWR_RRD_ARD_RRD:
            printf("%s, ", emitRegName(id->idReg1(), attr));
            printf("%s, ", emitRegName(id->idReg2(), attr));
            emitDispAddrMode(id);
            emitGetInsAmdCns(id, &cnsVal);
            val = (cnsVal.cnsVal >> 4) + XMMBASE;
            printf(", %s", emitRegName((regNumber)val, attr));
            break;

        case IF_ARD_RRD:
        case IF_AWR_RRD:
        case IF_ARW_RRD:
            printf("%s", sstr);
            emitDispAddrMode(id);
            printf(", %s", emitRegName(id->idReg1(), attr));
            break;

        case IF_AWR_RRD_RRD:
            printf("%s", sstr);
            emitDispAddrMode(id);
            printf(", %s", emitRegName(id->idReg1(), attr));
            printf(", %s", emitRegName(id->idReg2(), attr));
            break;

        case IF_ARD_CNS:
        case IF_AWR_CNS:
        case IF_ARW_CNS:
        case IF_ARW_SHF:
            printf("%s", sstr);
            emitDispAddrMode(id);
            emitGetInsAmdCns(id, &cnsVal);
            printf(", ");
            emitDispImm(id, cnsVal);
            break;

        case IF_SRD:
        case IF_SWR:
        case IF_SRW:
            printf("%s", sstr);

#if !FEATURE_FIXED_OUT_ARGS
            if (ins == INS_pop)
            {
                emitCurStackLvl -= sizeof(int);
            }
#endif

            emitDispFrameRef(id, asmfm);

#if !FEATURE_FIXED_OUT_ARGS
            if (ins == INS_pop)
            {
                emitCurStackLvl += sizeof(int);
            }
#endif

            emitDispShiftCL(ins);
            break;

        case IF_SRD_RRD:
        case IF_SWR_RRD:
        case IF_SRW_RRD:
            printf("%s", sstr);
            emitDispFrameRef(id, asmfm);
            printf(", %s", emitRegName(id->idReg1(), attr));
            break;

        case IF_SRD_CNS:
        case IF_SWR_CNS:
        case IF_SRW_CNS:
        case IF_SRW_SHF:
            printf("%s", sstr);
            emitDispFrameRef(id, asmfm);
            emitGetInsCns(id, &cnsVal);
            val = cnsVal.cnsVal;
            printf(", ");
            emitDispImm(id, cnsVal);
            break;

        case IF_SWR_RRD_CNS:
            assert(ins == INS_vextracti128 || ins == INS_vextractf128);
            assert(UseVEXEncoding());
            emitGetInsAmdCns(id, &cnsVal);
            printf("%s", sstr);
            emitDispFrameRef(id, asmfm);
            printf(", %s, ", emitRegName(id->idReg1(), attr));
            emitDispImm(id, cnsVal);
            break;

        case IF_RRD_SRD:
        case IF_RWR_SRD:
        case IF_RRW_SRD:
            if (ins == INS_movsx || ins == INS_movzx AMD64_ONLY(|| ins == INS_movsxd))
            {
                attr = EA_PTRSIZE;
            }
            else if ((ins == INS_crc32) && (attr != EA_8BYTE))
            {
                // The destination reg is always 4 bytes.
                attr = EA_4BYTE;
            }

            printf("%s, %s", emitRegName(id->idReg1(), attr), sstr);
            emitDispFrameRef(id, asmfm);

            break;

        case IF_RRW_SRD_CNS:
        case IF_RWR_SRD_CNS:
            printf("%s, %s", emitRegName(id->idReg1(), attr), sstr);
            emitDispFrameRef(id, asmfm);
            emitGetInsCns(id, &cnsVal);
            printf(", ");
            emitDispImm(id, cnsVal);
            break;

        case IF_RWR_RRD_SRD:
            printf("%s, %s, %s", emitRegName(id->idReg1(), attr), emitRegName(id->idReg2(), attr), sstr);
            emitDispFrameRef(id, asmfm);
            break;

        case IF_RWR_RRD_SRD_CNS:
            printf("%s, %s, %s", emitRegName(id->idReg1(), attr), emitRegName(id->idReg2(), attr), sstr);
            emitDispFrameRef(id, asmfm);
            emitGetInsCns(id, &cnsVal);
            printf(", ");
            emitDispImm(id, cnsVal);
            break;

        case IF_RWR_RRD_SRD_RRD:
            printf("%s, ", emitRegName(id->idReg1(), attr));
            printf("%s, ", emitRegName(id->idReg2(), attr));
            emitDispFrameRef(id, asmfm);
            emitGetInsCns(id, &cnsVal);
            val = (cnsVal.cnsVal >> 4) + XMMBASE;
            printf(", %s", emitRegName((regNumber)val, attr));
            break;

        case IF_RRD_RRD:
        case IF_RWR_RRD:
        case IF_RRW_RRD:
            if (ins == INS_pmovmskb)
            {
                printf("%s, %s", emitRegName(id->idReg1(), EA_4BYTE), emitRegName(id->idReg2(), attr));
            }
            else if ((ins == INS_cvtsi2ss) || (ins == INS_cvtsi2sd))
            {
                printf(" %s, %s", emitRegName(id->idReg1(), EA_16BYTE), emitRegName(id->idReg2(), attr));
            }
            else if ((ins == INS_cvttsd2si) || (ins == INS_cvtss2si) || (ins == INS_cvtsd2si) || (ins == INS_cvttss2si))
            {
                printf(" %s, %s", emitRegName(id->idReg1(), attr), emitRegName(id->idReg2(), EA_16BYTE));
            }
            else if (ins == INS_movsx || ins == INS_movzx AMD64_ONLY(|| ins == INS_movsxd))
            {
                printf("%s, %s", emitRegName(id->idReg1(), EA_PTRSIZE), emitRegName(id->idReg2(), attr));
            }
            else if (ins == INS_bt)
            {
                // INS_bt operands are reversed. Display them in the normal order.
                printf("%s, %s", emitRegName(id->idReg2(), attr), emitRegName(id->idReg1(), attr));
            }
            else if (ins == INS_crc32 && attr != EA_8BYTE)
            {
                // The destination reg is always 4 bytes.
                printf("%s, %s", emitRegName(id->idReg1(), EA_4BYTE), emitRegName(id->idReg2(), attr));
            }
            else
            {
                printf("%s, %s", emitRegName(id->idReg1(), attr), emitRegName(id->idReg2(), attr));
            }
            break;

        case IF_RRW_RRW:
            assert(ins == INS_xchg);
            printf("%s,", emitRegName(id->idReg1(), attr));
            printf(" %s", emitRegName(id->idReg2(), attr));
            break;

        case IF_RWR_RRD_RRD:
        {
            assert(IsAVXInstruction(ins));
            assert(IsThreeOperandAVXInstruction(ins));
            regNumber reg2 = id->idReg2();
            regNumber reg3 = id->idReg3();
            if (ins == INS_bextr || ins == INS_bzhi)
            {
                // BMI bextr and bzhi encodes the reg2 in VEX.vvvv and reg3 in modRM,
                // which is different from most of other instructions
                regNumber tmp = reg2;
                reg2          = reg3;
                reg3          = tmp;
            }
            printf("%s, ", emitRegName(id->idReg1(), attr));
            printf("%s, ", emitRegName(reg2, attr));
            printf("%s", emitRegName(reg3, attr));
            break;
        }

        case IF_RWR_RRD_RRD_CNS:
            assert(IsAVXInstruction(ins));
            assert(IsThreeOperandAVXInstruction(ins));
            printf("%s, ", emitRegName(id->idReg1(), attr));
            printf("%s, ", emitRegName(id->idReg2(), attr));

            switch (ins)
            {
                case INS_vinsertf128:
                case INS_vinserti128:
                {
                    attr = EA_16BYTE;
                    break;
                }

                case INS_pinsrb:
                case INS_pinsrw:
                case INS_pinsrd:
                {
                    attr = EA_4BYTE;
                    break;
                }

                case INS_pinsrq:
                {
                    attr = EA_8BYTE;
                    break;
                }

                default:
                {
                    break;
                }
            }

            printf("%s, ", emitRegName(id->idReg3(), attr));
            emitDispImm(id, emitGetInsSC(id));
            break;

        case IF_RWR_RRD_RRD_RRD:
            assert(IsAVXOnlyInstruction(ins));
            assert(UseVEXEncoding());
            printf("%s, ", emitRegName(id->idReg1(), attr));
            printf("%s, ", emitRegName(id->idReg2(), attr));
            printf("%s, ", emitRegName(id->idReg3(), attr));
            printf("%s", emitRegName(id->idReg4(), attr));
            break;

        case IF_RRW_RRW_CNS:
        {
            emitAttr tgtAttr = attr;

            switch (ins)
            {
                case INS_vextractf128:
                case INS_vextracti128:
                {
                    tgtAttr = EA_16BYTE;
                    break;
                }

                case INS_extractps:
                case INS_pextrb:
                case INS_pextrw:
                case INS_pextrw_sse41:
                case INS_pextrd:
                {
                    tgtAttr = EA_4BYTE;
                    break;
                }

                case INS_pextrq:
                {
                    tgtAttr = EA_8BYTE;
                    break;
                }

                case INS_pinsrb:
                case INS_pinsrw:
                case INS_pinsrd:
                {
                    attr = EA_4BYTE;
                    break;
                }

                case INS_pinsrq:
                {
                    attr = EA_8BYTE;
                    break;
                }

                default:
                {
                    break;
                }
            }

            printf("%s,", emitRegName(id->idReg1(), tgtAttr));
            printf(" %s", emitRegName(id->idReg2(), attr));
            printf(", ");
            emitDispImm(id, CnsVal{emitGetInsSC(id), id->idIsCnsReloc()});
            break;
        }

        case IF_RRD:
        case IF_RWR:
        case IF_RRW:
            printf("%s", emitRegName(id->idReg1(), attr));
            emitDispShiftCL(ins);
            break;

        case IF_RRW_SHF:
            printf("%s, %d", emitRegName(id->idReg1(), attr), emitGetInsSC(id));
            break;

        case IF_RRD_MRD:
        case IF_RWR_MRD:
        case IF_RRW_MRD:
            if (ins == INS_movsx || ins == INS_movzx AMD64_ONLY(|| ins == INS_movsxd))
            {
                attr = EA_PTRSIZE;
            }
            else if ((ins == INS_crc32) && (attr != EA_8BYTE))
            {
                // The destination reg is always 4 bytes.
                attr = EA_4BYTE;
            }
            printf("%s, %s", emitRegName(id->idReg1(), attr), sstr);
            emitDispClsVar(id);
            break;

        case IF_RRW_MRD_CNS:
        case IF_RWR_MRD_CNS:
            printf("%s, %s", emitRegName(id->idReg1(), attr), sstr);
            emitDispClsVar(id);
            emitGetInsDcmCns(id, &cnsVal);
            printf(", ");
            emitDispImm(id, cnsVal);
            break;

        case IF_MWR_RRD_CNS:
            assert(ins == INS_vextracti128 || ins == INS_vextractf128);
            // vextracti/f128 extracts 128-bit data, so we fix sstr as "xmm ptr"
            sstr = emitSizeStr(EA_ATTR(16));
            printf(sstr);
            emitDispClsVar(id);
            printf(", %s", emitRegName(id->idReg1(), attr));
            emitGetInsDcmCns(id, &cnsVal);
            printf(", ");
            emitDispImm(id, cnsVal);
            break;

        case IF_RWR_RRD_MRD:
            printf("%s, %s, %s", emitRegName(id->idReg1(), attr), emitRegName(id->idReg2(), attr), sstr);
            emitDispClsVar(id);
            break;

        case IF_RWR_RRD_MRD_CNS:
            printf("%s, %s, %s", emitRegName(id->idReg1(), attr), emitRegName(id->idReg2(), attr), sstr);
            emitDispClsVar(id);
            emitGetInsDcmCns(id, &cnsVal);
            printf(", ");
            emitDispImm(id, cnsVal);
            break;

        case IF_RWR_RRD_MRD_RRD:
            printf("%s, ", emitRegName(id->idReg1(), attr));
            printf("%s, ", emitRegName(id->idReg2(), attr));
            emitDispClsVar(id);
            emitGetInsDcmCns(id, &cnsVal);
            val = (cnsVal.cnsVal >> 4) + XMMBASE;
            printf(", %s", emitRegName((regNumber)val, attr));
            break;

        case IF_MRD_RRD:
        case IF_MWR_RRD:
        case IF_MRW_RRD:
            printf("%s", sstr);
            emitDispClsVar(id);
            printf(", %s", emitRegName(id->idReg1(), attr));
            break;

        case IF_MRD_CNS:
        case IF_MWR_CNS:
        case IF_MRW_CNS:
        case IF_MRW_SHF:
            printf("%s", sstr);
            emitDispClsVar(id);
            emitGetInsDcmCns(id, &cnsVal);
            printf(", ");
            emitDispImm(id, cnsVal);
            break;

        case IF_MRD:
        case IF_MWR:
        case IF_MRW:
            printf("%s", sstr);
            emitDispClsVar(id);
            emitDispShiftCL(ins);
            break;

        case IF_RRD_CNS:
        case IF_RWR_CNS:
        case IF_RRW_CNS:
            printf("%s, ", emitRegName(id->idReg1(), attr));
            emitDispImm(id, CnsVal{emitGetInsSC(id), id->idIsCnsReloc()});
            break;

        case IF_LABEL:
        case IF_RWR_LABEL:
            if (ins == INS_lea)
            {
                printf("%s, ", emitRegName(id->idReg1(), attr));
            }

            if (((instrDescJmp*)id)->idjShort)
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
            printf("unexpected format %s", emitIfName(id->idInsFmt()));
            assert(!"unexpectedFormat");
            break;
    }

    if (sz != 0 && sz != id->idCodeSize() && (!asmfm || emitComp->verbose))
    {
        // Code size in the instrDesc is different from the actual code size we've been given!
        printf(" (ECS:%d, ACS:%d)", id->idCodeSize(), sz);
    }

    printf("\n");
}

/*****************************************************************************/
#endif

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

size_t emitter::emitOutputImm(uint8_t* dst, size_t size, CnsVal imm)
{
    switch (size)
    {
        case 1:
            return emitOutputByte(dst, imm.cnsVal);
        case 2:
            return emitOutputWord(dst, imm.cnsVal);
#ifdef TARGET_AMD64
        case 8:
            noway_assert(IsImm32(imm.cnsVal) && !imm.cnsReloc);
            return emitOutputLong(dst, imm.cnsVal);
#endif
        default:
            assert(size == 4);
            emitOutputLong(dst, imm.cnsVal);

            if (imm.cnsReloc)
            {
                emitRecordRelocation(dst, reinterpret_cast<void*>(imm.cnsVal), IMAGE_REL_BASED_HIGHLOW);
            }

            return 4;
    }
}

/*****************************************************************************
 *
 *  Output nBytes bytes of NOP instructions
 */

static BYTE* emitOutputNOP(BYTE* dstRW, size_t nBytes)
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

//--------------------------------------------------------------------
// emitOutputAlign: Outputs NOP to align the loop
//
// Arguments:
//   ig - Current instruction group
//   id - align instruction that holds amount of padding (NOPs) to add
//   dst - Destination buffer
//
// Return Value:
//   None.
//
// Notes:
//   Amount of padding needed to align the loop is already calculated. This
//   method extracts that information and inserts suitable NOP instructions.
//
BYTE* emitter::emitOutputAlign(insGroup* ig, instrDesc* id, BYTE* dst)
{
    // Candidate for loop alignment
    assert(codeGen->ShouldAlignLoops());
    assert(ig->isLoopAlign());

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

    BYTE* dstRW = dst + writeableOffset;
    dstRW       = emitOutputNOP(dstRW, paddingToAdd);
    return dstRW - writeableOffset;
}

uint8_t* emitter::emitOutputAM(uint8_t* dst, instrDesc* id, code_t code, CnsVal* imm)
{
    instruction ins      = id->idIns();
    emitAttr    size     = id->idOpSize();
    unsigned    immSize  = EA_SIZE_IN_BYTES(size);
    regNumber   baseReg  = id->idAddr()->iiaAddrMode.amBaseReg;
    regNumber   indexReg = id->idAddr()->iiaAddrMode.amIndxReg;
    ssize_t     disp;

    // For INS_call the instruction size is actually the return value size
    if (ins == INS_call)
    {
#ifdef TARGET_AMD64
        // Compute the REX prefix if it exists
        if (IsExtendedReg(baseReg, EA_PTRSIZE))
        {
            insEncodeReg012(ins, baseReg, EA_PTRSIZE, &code);
            // TODO-Cleanup: stop casting RegEncoding() back to a regNumber.
            baseReg = (regNumber)RegEncoding(baseReg);
        }

        if (IsExtendedReg(indexReg, EA_PTRSIZE))
        {
            insEncodeRegSIB(ins, indexReg, &code);
            // TODO-Cleanup: stop casting RegEncoding() back to a regNumber.
            indexReg = (regNumber)RegEncoding(indexReg);
        }

        // And emit the REX prefix
        dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);
#endif // TARGET_AMD64

        // The displacement field is in an unusual place for calls
        disp = emitGetInsCIdisp(id);

        goto GOT_DSP;
    }

    // `imm` is used for two kinds if instructions
    // 1. ins like ADD that can have reg/mem and const versions both and const version needs to modify the opcode for
    // large constant operand (e.g., imm32)
    // 2. certain SSE/AVX ins have const operand as control bits that is always 1-Byte (imm8) even if `size` > 1-Byte
    if ((imm != nullptr) && (size > EA_1BYTE))
    {
        ssize_t cval = imm->cnsVal;

        // Does the constant fit in a byte?
        // SSE/AVX do not need to modify opcode
        if ((signed char)cval == cval && !imm->cnsReloc && ins != INS_mov && ins != INS_test)
        {
            if (id->idInsFmt() != IF_ARW_SHF && !IsSSEOrAVXInstruction(ins))
            {
                code |= 2;
            }

            immSize = 1;
        }
    }

    // Emit VEX prefix if required
    // There are some callers who already add VEX prefix and call this routine.
    // Therefore, add VEX prefix is one is not already present.
    code = AddVexPrefixIfNeededAndNotPresent(ins, code, size);

    // For this format, moves do not support a third operand, so we only need to handle the binary ops.
    if (TakesVexPrefix(ins))
    {
        if (IsDstDstSrcAVXInstruction(ins))
        {
            regNumber src1 = REG_NA;

            switch (id->idInsFmt())
            {
                case IF_RWR_RRD_ARD:
                case IF_RWR_ARD_RRD:
                case IF_RWR_RRD_ARD_CNS:
                case IF_RWR_RRD_ARD_RRD:
                {
                    src1 = id->idReg2();
                    break;
                }

                default:
                {
                    src1 = id->idReg1();
                    break;
                }
            }

            // encode source operand reg in 'vvvv' bits in 1's complement form
            code = insEncodeReg3456(ins, src1, size, code);
        }
        else if (IsDstSrcSrcAVXInstruction(ins))
        {
            code = insEncodeReg3456(ins, id->idReg2(), size, code);
        }
    }

    // Emit the REX prefix if required
    if (TakesRexWPrefix(ins, size))
    {
        code = AddRexWPrefix(ins, code);
    }

    if (IsExtendedReg(baseReg, EA_PTRSIZE))
    {
        insEncodeReg012(ins, baseReg, EA_PTRSIZE, &code);
        // TODO-Cleanup: stop casting RegEncoding() back to a regNumber.
        baseReg = (regNumber)RegEncoding(baseReg);
    }

    if (IsExtendedReg(indexReg, EA_PTRSIZE))
    {
        insEncodeRegSIB(ins, indexReg, &code);
        // TODO-Cleanup: stop casting RegEncoding() back to a regNumber.
        indexReg = (regNumber)RegEncoding(indexReg);
    }

    // Special case emitting AVX instructions
    if (EncodedBySSE38orSSE3A(ins) || (ins == INS_crc32))
    {
        if ((ins == INS_crc32) && (size > EA_1BYTE))
        {
            code |= 0x0100;

            if (size == EA_2BYTE)
            {
                dst += emitOutputByte(dst, 0x66);
            }
        }

        regNumber reg345 = REG_NA;
        if (IsBMIInstruction(ins))
        {
            reg345 = getBmiRegNumber(ins);
        }
        if (reg345 == REG_NA)
        {
            switch (id->idInsFmt())
            {
                case IF_AWR_RRD_RRD:
                {
                    reg345 = id->idReg2();
                    break;
                }

                default:
                {
                    reg345 = id->idReg1();
                    break;
                }
            }
        }
        unsigned regcode = insEncodeReg345(ins, reg345, size, &code);

        dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

        if (UseVEXEncoding() && (ins != INS_crc32))
        {
            // Emit last opcode byte
            // TODO-XArch-CQ: Right now support 4-byte opcode instructions only
            assert((code & 0xFF) == 0);
            dst += emitOutputByte(dst, (code >> 8) & 0xFF);
        }
        else
        {
            dst += emitOutputWord(dst, code >> 16);
            dst += emitOutputWord(dst, code & 0xFFFF);
        }

        code = regcode;
    }
    // Is this a 'big' opcode?
    else if (code & 0xFF000000)
    {
        // Output the REX prefix
        dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

        // Output the highest word of the opcode
        // We need to check again as in case of AVX instructions leading opcode bytes are stripped off
        // and encoded as part of VEX prefix.
        if (code & 0xFF000000)
        {
            dst += emitOutputWord(dst, code >> 16);
            code &= 0x0000FFFF;
        }
    }
    else if (code & 0x00FF0000)
    {
        // BT supports 16 bit operands and this code doesn't handle the necessary 66 prefix.
        assert(ins != INS_bt);

        // Output the REX prefix
        dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

        // Output the highest byte of the opcode
        if (code & 0x00FF0000)
        {
            dst += emitOutputByte(dst, code >> 16);
            code &= 0x0000FFFF;
        }

        // Use the large version if this is not a byte. This trick will not
        // work in case of SSE2 and AVX instructions.
        if ((size != EA_1BYTE) && (ins != INS_imul) && (ins != INS_bsf) && (ins != INS_bsr) && !IsSSEInstruction(ins) &&
            !IsAVXInstruction(ins))
        {
            code++;
        }
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
    else if (!IsSSEInstruction(ins) && !IsAVXInstruction(ins))
    {
        switch (size)
        {
            case EA_1BYTE:
                break;
            case EA_2BYTE:
                dst += emitOutputByte(dst, 0x66);
                FALLTHROUGH;
            case EA_4BYTE:
#ifdef TARGET_AMD64
            case EA_8BYTE:
#endif
                // Set the 'w' size bit to indicate a 16/32/64-bit operation.
                code |= 0x01;
                break;
            default:
                unreached();
        }
    }

    // Output the REX prefix
    dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

    disp = emitGetInsAmdAny(id);
GOT_DSP:
    bool hasDisp8 = IsDisp8(disp) && !id->idIsDspReloc();

    if ((indexReg == REG_NA) && (baseReg == REG_NA))
    {
#ifdef TARGET_X86
        code_t rmCode = 0x05;
#else
        code_t rmCode = id->idIsDspReloc() ? 0x05 : 0x04;
#endif

        if (EncodedBySSE38orSSE3A(ins) || (ins == INS_crc32))
        {
            dst += emitOutputByte(dst, code | rmCode);
        }
        else
        {
            dst += emitOutputWord(dst, code | (rmCode << 8));
        }

        if (id->idIsDspReloc())
        {
            // For emitting relocation, we also need to take into account of the
            // additional bytes of code emitted for imm.
            int32_t immDelta = 0;

            if (imm != nullptr)
            {
#ifdef TARGET_AMD64
                noway_assert((immSize < 8) || (IsImm32(imm->cnsVal) && !imm->cnsReloc));
#else
                noway_assert(immSize <= 4);
#endif
                immDelta = -static_cast<int32_t>(Min(immSize, 4u));
            }

#ifdef TARGET_AMD64
            // We emit zero on x64, to avoid the assert in emitOutputLong
            dst += emitOutputLong(dst, 0);
            emitRecordRelocation(dst - 4, reinterpret_cast<void*>(disp), IMAGE_REL_BASED_REL32, immDelta);
#else
            dst += emitOutputLong(dst, disp);
            emitRecordRelocation(dst - 4, reinterpret_cast<void*>(disp), IMAGE_REL_BASED_HIGHLOW, immDelta);
#endif
        }
        else
        {
#ifdef TARGET_AMD64
            noway_assert(!emitComp->opts.compReloc);
            noway_assert(!emitComp->eeIsRIPRelativeAddress(reinterpret_cast<void*>(disp)));
            noway_assert(IsDisp32(disp));

            dst += emitOutputByte(dst, 0x25);
#endif

            dst += emitOutputLong(dst, disp);
        }
    }
    else
    {
        code_t   rmCode;
        unsigned scale;

        if (indexReg == REG_NA)
        {
            scale  = 0;
            rmCode = RegEncoding(baseReg);

            if ((disp != 0) || (baseReg == REG_EBP))
            {
                rmCode |= hasDisp8 ? 0x40 : 0x80;
            }
        }
        else
        {
            scale  = emitDecodeScale(id->idAddr()->iiaAddrMode.amScale);
            rmCode = 0x04;

            if ((scale > 1) && (baseReg == REG_NA))
            {
                baseReg  = REG_EBP;
                hasDisp8 = false;
            }
            else if ((disp != 0) || (baseReg == REG_EBP))
            {
                rmCode |= hasDisp8 ? 0x40 : 0x80;
            }
        }

        if (EncodedBySSE38orSSE3A(ins) || (ins == INS_crc32))
        {
            dst += emitOutputByte(dst, code | rmCode);
        }
        else
        {
            dst += emitOutputWord(dst, code | (rmCode << 8));
        }

        if (indexReg != REG_NA)
        {
            dst += emitOutputByte(dst, RegEncoding(baseReg) | (RegEncoding(indexReg) << 3) | ScaleEncoding(scale));
        }
        else if (baseReg == REG_ESP)
        {
            dst += emitOutputByte(dst, 0x24);
        }

        if ((disp != 0) || (baseReg == REG_EBP))
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
        dst += emitOutputImm(dst, immSize, *imm);
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
        assert(!instrIs3opImul(ins));
        assert(ins != INS_mulEAX && ins != INS_imulEAX);
    }
    else
    {
        if (!emitInsCanOnlyWriteSSE2OrAVXReg(id))
        {
            switch (id->idInsFmt())
            {
                case IF_RWR_ARD:
                case IF_RRW_ARD:
                case IF_RWR_RRD_ARD:
                    emitGCregDeadUpd(id->idReg1(), dst);
                    break;
                default:
                    break;
            }

            if ((ins == INS_mulEAX) || (ins == INS_imulEAX))
            {
                emitGCregDeadUpd(REG_EAX, dst);
                emitGCregDeadUpd(REG_EDX, dst);
            }
            else if (instrIs3opImul(ins))
            {
                emitGCregDeadUpd(inst3opImulReg(ins), dst);
            }
        }
    }

    return dst;
}

uint8_t* emitter::emitOutputSV(uint8_t* dst, instrDesc* id, code_t code, CnsVal* imm)
{
    instruction ins     = id->idIns();
    emitAttr    size    = id->idOpSize();
    unsigned    immSize = EA_SIZE_IN_BYTES(size);

    assert(ins != INS_imul || id->idReg1() == REG_EAX || size == EA_4BYTE || size == EA_8BYTE);

    // `imm` is used for two kinds if instructions
    // 1. ins like ADD that can have reg/mem and const versions both and const version needs to modify the opcode for
    // large constant operand (e.g., imm32)
    // 2. certain SSE/AVX ins have const operand as control bits that is always 1-Byte (imm8) even if `size` > 1-Byte
    if ((imm != nullptr) && (size > EA_1BYTE))
    {
        // Does the constant fit in a byte?
        // SSE/AVX do not need to modify opcode
        if (IsImm8(imm->cnsVal) && !imm->cnsReloc && (ins != INS_mov) && (ins != INS_test))
        {
            if ((id->idInsFmt() != IF_SRW_SHF) && (id->idInsFmt() != IF_RRW_SRD_CNS) &&
                (id->idInsFmt() != IF_RWR_RRD_SRD_CNS) && !IsSSEOrAVXInstruction(ins))
            {
                code |= 2;
            }

            immSize = 1;
        }
    }

    // Add VEX prefix if required.
    // There are some callers who already add VEX prefix and call this routine.
    // Therefore, add VEX prefix is one is not already present.
    code = AddVexPrefixIfNeededAndNotPresent(ins, code, size);

    // Compute the REX prefix
    if (TakesRexWPrefix(ins, size))
    {
        code = AddRexWPrefix(ins, code);
    }

    // Special case emitting AVX instructions
    if (EncodedBySSE38orSSE3A(ins) || (ins == INS_crc32))
    {
        if ((ins == INS_crc32) && (size > EA_1BYTE))
        {
            code |= 0x0100;

            if (size == EA_2BYTE)
            {
                dst += emitOutputByte(dst, 0x66);
            }
        }

        regNumber reg345 = REG_NA;
        if (IsBMIInstruction(ins))
        {
            reg345 = getBmiRegNumber(ins);
        }
        if (reg345 == REG_NA)
        {
            reg345 = id->idReg1();
        }
        else
        {
            code = insEncodeReg3456(ins, id->idReg1(), size, code);
        }
        unsigned regcode = insEncodeReg345(ins, reg345, size, &code);

        dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

        if (UseVEXEncoding() && (ins != INS_crc32))
        {
            // Emit last opcode byte
            // TODO-XArch-CQ: Right now support 4-byte opcode instructions only
            assert((code & 0xFF) == 0);
            dst += emitOutputByte(dst, (code >> 8) & 0xFF);
        }
        else
        {
            dst += emitOutputWord(dst, code >> 16);
            dst += emitOutputWord(dst, code & 0xFFFF);
        }

        code = regcode;
    }
    // Is this a 'big' opcode?
    else if (code & 0xFF000000)
    {
        // Output the REX prefix
        dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

        // Output the highest word of the opcode
        // We need to check again because in case of AVX instructions the leading
        // escape byte(s) (e.g. 0x0F) will be encoded as part of VEX prefix.
        if (code & 0xFF000000)
        {
            dst += emitOutputWord(dst, code >> 16);
            code &= 0x0000FFFF;
        }
    }
    else if (code & 0x00FF0000)
    {
        // BT supports 16 bit operands and this code doesn't add the necessary 66 prefix.
        assert(ins != INS_bt);

        // Output the REX prefix
        dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

        // Output the highest byte of the opcode.
        // We need to check again because in case of AVX instructions the leading
        // escape byte(s) (e.g. 0x0F) will be encoded as part of VEX prefix.
        if (code & 0x00FF0000)
        {
            dst += emitOutputByte(dst, code >> 16);
            code &= 0x0000FFFF;
        }

        // Use the large version if this is not a byte
        // TODO-XArch-Cleanup Can the need for the 'w' size bit be encoded in the instruction flags?
        if ((size != EA_1BYTE) && (ins != INS_imul) && (ins != INS_bsf) && (ins != INS_bsr) && (!insIsCMOV(ins)) &&
            !IsSSEInstruction(ins) && !IsAVXInstruction(ins))
        {
            code |= 0x1;
        }
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
    else if (!IsSSEInstruction(ins) && !IsAVXInstruction(ins))
    {
        switch (size)
        {
            case EA_1BYTE:
                break;
            case EA_2BYTE:
                dst += emitOutputByte(dst, 0x66);
                FALLTHROUGH;
            case EA_4BYTE:
#ifdef TARGET_AMD64
            case EA_8BYTE:
#endif
                // Set the 'w' size bit to indicate a 16/32/64-bit operation.
                code |= 0x01;
                break;
            default:
                unreached();
        }
    }

    // Output the REX prefix
    dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

    // for stack variables the dsp should never be a reloc
    assert(id->idIsDspReloc() == 0);

    bool ebpBased  = id->idAddr()->isEbpBased;
    int  lclOffset = id->idAddr()->lclOffset;
    int  disp      = lclOffset;

#if !FEATURE_FIXED_OUT_ARGS
    if (!ebpBased)
    {
        disp += emitCurStackLvl;
    }
#endif

    bool   hasDisp8 = IsDisp8(disp);
    code_t rmCode   = ebpBased ? 0x05 : 0x04;

    if ((disp != 0) || ebpBased)
    {
        rmCode |= hasDisp8 ? 0x40 : 0x80;
    }

    if (EncodedBySSE38orSSE3A(ins) || (ins == INS_crc32))
    {
        dst += emitOutputByte(dst, code | rmCode);
    }
    else
    {
        dst += emitOutputWord(dst, code | (rmCode << 8));
    }

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
        dst += emitOutputImm(dst, immSize, *imm);
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
    else if (!emitInsCanOnlyWriteSSE2OrAVXReg(id))
    {
        switch (id->idInsFmt())
        {
            case IF_RWR_SRD:
            case IF_RRW_SRD:
            case IF_RWR_RRD_SRD:
                emitGCregDeadUpd(id->idReg1(), dst);
                break;

            default:
                break;
        }

        if ((ins == INS_mulEAX) || (ins == INS_imulEAX))
        {
            emitGCregDeadUpd(REG_EAX, dst);
            emitGCregDeadUpd(REG_EDX, dst);
        }

        if (instrIs3opImul(ins))
        {
            emitGCregDeadUpd(inst3opImulReg(ins), dst);
        }
    }

    return dst;
}

/*****************************************************************************
 *
 *  Output an instruction with a static data member (class variable).
 */

BYTE* emitter::emitOutputCV(BYTE* dst, instrDesc* id, code_t code, CnsVal* imm)
{
    emitAttr             size    = id->idOpSize();
    instruction          ins     = id->idIns();
    CORINFO_FIELD_HANDLE fldh    = id->idAddr()->iiaFieldHnd;
    ssize_t              offs    = emitGetInsDsp(id);
    unsigned             immSize = EA_SIZE_IN_BYTES(size);

#ifdef WINDOWS_X86_ABI
    if (fldh == FS_SEG_FIELD)
    {
        dst += emitOutputByte(dst, 0x64);
    }
#endif

    // Compute VEX prefix
    // Some of its callers already add VEX prefix and then call this routine.
    // Therefore add VEX prefix is not already present.
    code = AddVexPrefixIfNeededAndNotPresent(ins, code, size);

    // Compute the REX prefix
    if (TakesRexWPrefix(ins, size))
    {
        code = AddRexWPrefix(ins, code);
    }

    // x64 currently never uses the moffset format, it uses only RIP relative addressing.
    X86_ONLY(bool isMoffset = false);

    // `imm` is used for two kinds if instructions
    // 1. ins like ADD that can have reg/mem and const versions both and const version needs to modify the opcode for
    // large constant operand (e.g., imm32)
    // 2. certain SSE/AVX ins have const operand as control bits that is always 1-Byte (imm8) even if `size` > 1-Byte
    if (imm != nullptr)
    {
        if ((size > EA_1BYTE) && IsImm8(imm->cnsVal) && !imm->cnsReloc && (ins != INS_mov) && (ins != INS_test))
        {
            // SSE/AVX do not need to modify opcode
            if (id->idInsFmt() != IF_MRW_SHF && !IsSSEOrAVXInstruction(ins))
            {
                code |= 2;
            }

            immSize = 1;
        }
    }
#ifdef TARGET_X86
    // Special case: "mov eax, [addr]" and "mov [addr], eax" have smaller encoding.
    else if ((ins == INS_mov) && (id->idReg1() == REG_EAX))
    {
        switch (id->idInsFmt())
        {
            case IF_RWR_MRD:

                assert(code == (insCodeRM(ins) | (insEncodeReg345(ins, REG_EAX, EA_PTRSIZE, NULL) << 8) | 0x0500));

                code &= ~((code_t)0xFFFFFFFF);
                code |= 0xA0;
                isMoffset = true;
                break;

            case IF_MWR_RRD:

                assert(code == (insCodeMR(ins) | (insEncodeReg345(ins, REG_EAX, EA_PTRSIZE, NULL) << 8) | 0x0500));

                code &= ~((code_t)0xFFFFFFFF);
                code |= 0xA2;
                isMoffset = true;
                break;

            default:
                break;
        }
    }
#endif // TARGET_X86

    // Special case emitting AVX instructions
    if (EncodedBySSE38orSSE3A(ins) || (ins == INS_crc32))
    {
        if ((ins == INS_crc32) && (size > EA_1BYTE))
        {
            code |= 0x0100;

            if (size == EA_2BYTE)
            {
                dst += emitOutputByte(dst, 0x66);
            }
        }

        regNumber reg345 = REG_NA;
        if (IsBMIInstruction(ins))
        {
            reg345 = getBmiRegNumber(ins);
        }
        if (reg345 == REG_NA)
        {
            reg345 = id->idReg1();
        }
        else
        {
            code = insEncodeReg3456(ins, id->idReg1(), size, code);
        }
        unsigned regcode = insEncodeReg345(ins, reg345, size, &code);

        dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

        if (UseVEXEncoding() && (ins != INS_crc32))
        {
            // Emit last opcode byte
            // TODO-XArch-CQ: Right now support 4-byte opcode instructions only
            assert((code & 0xFF) == 0);
            dst += emitOutputByte(dst, (code >> 8) & 0xFF);
        }
        else
        {
            dst += emitOutputWord(dst, code >> 16);
            dst += emitOutputWord(dst, code & 0xFFFF);
        }

        // Emit Mod,R/M byte
        dst += emitOutputByte(dst, regcode | 0x05);
        code = 0;
    }
    // Is this a 'big' opcode?
    else if (code & 0xFF000000)
    {
        // Output the REX prefix
        dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

        // Output the highest word of the opcode.
        // Check again since AVX instructions encode leading opcode bytes as part of VEX prefix.
        if (code & 0xFF000000)
        {
            dst += emitOutputWord(dst, code >> 16);
        }
        code &= 0x0000FFFF;
    }
    else if (code & 0x00FF0000)
    {
        // Output the REX prefix
        dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

        // Check again as VEX prefix would have encoded leading opcode byte
        if (code & 0x00FF0000)
        {
            dst += emitOutputByte(dst, code >> 16);
            code &= 0x0000FFFF;
        }

        if ((ins == INS_movsx || ins == INS_movzx || ins == INS_cmpxchg || ins == INS_xchg || ins == INS_xadd ||
             insIsCMOV(ins)) &&
            size != EA_1BYTE)
        {
            // movsx and movzx are 'big' opcodes but also have the 'w' bit
            code++;
        }
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
        assert(!IsSSEInstruction(ins) && !IsAVXInstruction(ins));

        switch (size)
        {
            case EA_1BYTE:
                break;
            case EA_2BYTE:
                dst += emitOutputByte(dst, 0x66);
                FALLTHROUGH;
            case EA_4BYTE:
#ifdef TARGET_AMD64
            case EA_8BYTE:
#endif
                // Set the 'w' size bit to indicate a 16/32/64-bit operation.
                code |= 0x01;
                break;
            default:
                unreached();
        }
    }

    // Output the REX prefix
    dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

    if (code != 0)
    {
#ifdef TARGET_X86
        if (isMoffset)
        {
            dst += emitOutputByte(dst, code);
        }
        else
#endif
        {
            dst += emitOutputWord(dst, code);
        }
    }

    BYTE* addr;

    if (IsRoDataField(fldh))
    {
        addr = emitConsBlock + GetRoDataOffset(fldh);

#ifdef DEBUG
        int byteSize = EA_SIZE_IN_BYTES(size);

        if (ins == INS_cvttss2si || ins == INS_cvtss2sd || ins == INS_vbroadcastss || ins == INS_insertps)
        {
            byteSize = 4;
        }
        else if (ins == INS_vbroadcastsd)
        {
            byteSize = 8;
        }
        else if (ins == INS_vinsertf128 || ins == INS_vinserti128)
        {
            byteSize = 16;
        }

        // Check that the offset is properly aligned (i.e. the ddd in [ddd])
        // When SMALL_CODE is set, we only expect 4-byte alignment, otherwise
        // we expect the same alignment as the size of the constant.

        assert((emitChkAlign == false) || (ins == INS_lea) ||
               ((emitComp->compCodeOpt() == SMALL_CODE) && (((size_t)addr & 3) == 0)) ||
               (((size_t)addr & (byteSize - 1)) == 0));
#endif // DEBUG
    }
#ifdef WINDOWS_X86_ABI
    else if (fldh == FS_SEG_FIELD)
    {
        addr = nullptr;
    }
#endif
    else
    {
        addr = static_cast<uint8_t*>(emitComp->info.compCompHnd->getFieldAddress(fldh, nullptr));
        noway_assert(addr != nullptr);
    }

    // For emitting relocation, we also need to take into account of the
    // additional bytes of code emitted for imm.
    unsigned immRelocDelta = 0;
    uint8_t* target        = addr + offs;

#ifdef TARGET_AMD64
    // All static field and data section constant accesses should be marked as relocatable
    noway_assert(id->idIsDspReloc());

    if (imm != nullptr)
    {
        noway_assert((immSize < 8) || (IsImm32(imm->cnsVal) && !imm->cnsReloc));
        immRelocDelta = Min(immSize, 4u);
    }

    dst += emitOutputLong(dst, 0);
    emitRecordRelocation(dst - 4, target, IMAGE_REL_BASED_REL32, -static_cast<int32_t>(immRelocDelta));
#else  // TARGET_X86
    if (imm != nullptr)
    {
        noway_assert(immSize <= 4);
        immRelocDelta = immSize;
    }

    dst += emitOutputLong(dst, reinterpret_cast<intptr_t>(target));

    if (id->idIsDspReloc())
    {
        emitRecordRelocation(dst - 4, target, IMAGE_REL_BASED_HIGHLOW, -static_cast<int32_t>(immRelocDelta));
    }
#endif // TARGET_X86

    if (imm != nullptr)
    {
        dst += emitOutputImm(dst, immSize, *imm);
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
        if (!emitInsCanOnlyWriteSSE2OrAVXReg(id))
        {
            switch (id->idInsFmt())
            {
                case IF_RWR_MRD:
                case IF_RRW_MRD:
                case IF_RWR_RRD_MRD:
                    emitGCregDeadUpd(id->idReg1(), dst);
                    break;
                default:
                    break;
            }

            if (ins == INS_mulEAX || ins == INS_imulEAX)
            {
                emitGCregDeadUpd(REG_EAX, dst);
                emitGCregDeadUpd(REG_EDX, dst);
            }

            // For the three operand imul instruction the target register
            // is encoded in the opcode

            if (instrIs3opImul(ins))
            {
                regNumber tgtReg = inst3opImulReg(ins);
                emitGCregDeadUpd(tgtReg, dst);
            }
        }
    }

    return dst;
}

/*****************************************************************************
 *
 *  Output an instruction with one register operand.
 */

BYTE* emitter::emitOutputR(BYTE* dst, instrDesc* id)
{
    code_t code;

    instruction ins  = id->idIns();
    regNumber   reg  = id->idReg1();
    emitAttr    size = id->idOpSize();

    // We would to update GC info correctly
    assert(!IsSSEInstruction(ins));
    assert(!IsAVXInstruction(ins));

    // Get the 'base' opcode
    switch (ins)
    {
        case INS_call:
            code = insEncodeMRreg(INS_call, reg, EA_PTRSIZE, insCodeMR(INS_call));
            dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);
            dst += emitOutputWord(dst, code);
            // Calls use a different mechanism to update GC info so we can skip the normal handling.
            return dst;

        case INS_inc:
        case INS_dec:

#ifdef TARGET_AMD64
            if (true)
#else
            if (size == EA_1BYTE)
#endif
            {
                assert(INS_inc_l == INS_inc + 1);
                assert(INS_dec_l == INS_dec + 1);

                // Can't use the compact form, use the long form
                ins = (instruction)(ins + 1);
                if (size == EA_2BYTE)
                {
                    // Output a size prefix for a 16-bit operand
                    dst += emitOutputByte(dst, 0x66);
                }

                code = insCodeRR(ins);
                if (size != EA_1BYTE)
                {
                    // Set the 'w' bit to get the large version
                    code |= 0x1;
                }

                if (TakesRexWPrefix(ins, size))
                {
                    code = AddRexWPrefix(ins, code);
                }

                // Register...
                unsigned regcode = insEncodeReg012(ins, reg, size, &code);

                // Output the REX prefix
                dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

                dst += emitOutputWord(dst, code | (regcode << 8));
            }
            else
            {
                if (size == EA_2BYTE)
                {
                    // Output a size prefix for a 16-bit operand
                    dst += emitOutputByte(dst, 0x66);
                }
                dst += emitOutputByte(dst, insCodeRR(ins) | insEncodeReg012(ins, reg, size, nullptr));
            }
            break;

        case INS_pop:
        case INS_pop_hide:
        case INS_push:
        case INS_push_hide:

            assert(size == EA_PTRSIZE);
            code = insEncodeOpreg(ins, reg, size);

            assert(!TakesVexPrefix(ins));
            assert(!TakesRexWPrefix(ins, size));

            // Output the REX prefix
            dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

            dst += emitOutputByte(dst, code);
            break;

        case INS_bswap:
        {
            assert(size >= EA_4BYTE && size <= EA_PTRSIZE); // 16-bit BSWAP is undefined

            // The Intel instruction set reference for BSWAP states that extended registers
            // should be enabled via REX.R, but per Vol. 2A, Sec. 2.2.1.2 (see also Figure 2-7),
            // REX.B should instead be used if the register is encoded in the opcode byte itself.
            // Therefore the default logic of insEncodeReg012 is correct for this case.

            code = insCodeRR(ins);

            if (TakesRexWPrefix(ins, size))
            {
                code = AddRexWPrefix(ins, code);
            }

            // Register...
            unsigned regcode = insEncodeReg012(ins, reg, size, &code);

            // Output the REX prefix
            dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

            dst += emitOutputWord(dst, code | (regcode << 8));
            break;
        }

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

            code = insEncodeMRreg(ins, reg, EA_1BYTE, insCodeMR(ins));

            // Output the REX prefix
            dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

            // We expect this to always be a 'big' opcode
            assert(code & 0x00FF0000);

            dst += emitOutputByte(dst, code >> 16);
            dst += emitOutputWord(dst, code & 0x0000FFFF);

            break;

        case INS_mulEAX:
        case INS_imulEAX:

            // Kill off any GC refs in EAX or EDX
            emitGCregDeadUpd(REG_EAX, dst);
            emitGCregDeadUpd(REG_EDX, dst);

            FALLTHROUGH;

        default:

            assert(id->idGCref() == GCT_NONE);

            code = insEncodeMRreg(ins, reg, size, insCodeMR(ins));

            if (size != EA_1BYTE)
            {
                // Set the 'w' bit to get the large version
                code |= 0x1;

                if (size == EA_2BYTE)
                {
                    // Output a size prefix for a 16-bit operand
                    dst += emitOutputByte(dst, 0x66);
                }
            }

            code = AddVexPrefixIfNeeded(ins, code, size);

            if (TakesRexWPrefix(ins, size))
            {
                code = AddRexWPrefix(ins, code);
            }

            // Output the REX prefix
            dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

            dst += emitOutputWord(dst, code);
            break;
    }

    // Are we writing the register? if so then update the GC information
    switch (id->idInsFmt())
    {
        case IF_RRD:
            break;
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
        {
#ifdef DEBUG
            regMaskTP regMask = genRegMask(reg);
#endif
            if (id->idGCref())
            {
                assert(ins == INS_inc || ins == INS_dec || ins == INS_inc_l || ins == INS_dec_l);
                // We would like to assert that the reg must currently be holding either a gcref or a byref.
                // However, we can see cases where a LCLHEAP generates a non-gcref value into a register,
                // and the first instruction we generate after the LCLHEAP is an `inc` that is typed as
                // byref. We'll properly create the byref gcinfo when this happens.
                //     assert((gcInfo.GetAllLiveRegs() & regMask) != RBM_NONE);
                assert(id->idGCref() == GCT_BYREF);
                // Mark it as holding a GCT_BYREF
                emitGCregLiveUpd(GCT_BYREF, id->idReg1(), dst);
            }
            else
            {
                // Can't use RRW to trash a GC ref.  It's OK for unverifiable code to trash Byrefs.
                assert((gcInfo.GetLiveRegs(GCT_GCREF) & regMask) == RBM_NONE);
            }
        }
        break;
        default:
            INDEBUG(emitDispIns(id));
            assert(!"unexpected instruction format");
            break;
    }

    return dst;
}

/*****************************************************************************
 *
 *  Output an instruction with two register operands.
 */

BYTE* emitter::emitOutputRR(BYTE* dst, instrDesc* id)
{
    code_t code;

    instruction ins  = id->idIns();
    regNumber   reg1 = id->idReg1();
    regNumber   reg2 = id->idReg2();
    emitAttr    size = id->idOpSize();

    if (IsSSEOrAVXInstruction(ins))
    {
        assert((ins != INS_movd) || (isFloatReg(reg1) != isFloatReg(reg2)));

        if ((ins != INS_movd) || isFloatReg(reg1))
        {
            code = insCodeRM(ins);
        }
        else
        {
            code = insCodeMR(ins);
        }
        code = AddVexPrefixIfNeeded(ins, code, size);
        code = insEncodeRMreg(ins, code);

        if (TakesRexWPrefix(ins, size))
        {
            code = AddRexWPrefix(ins, code);
        }
    }
    else if ((ins == INS_movsx) || (ins == INS_movzx) || (insIsCMOV(ins)))
    {
        assert(hasCodeRM(ins) && !hasCodeMI(ins) && !hasCodeMR(ins));
        code = insCodeRM(ins);
        code = AddVexPrefixIfNeeded(ins, code, size);
        code = insEncodeRMreg(ins, code) | (int)(size == EA_2BYTE);
#ifdef TARGET_AMD64

        assert((size < EA_4BYTE) || (insIsCMOV(ins)));
        if ((size == EA_8BYTE) || (ins == INS_movsx))
        {
            code = AddRexWPrefix(ins, code);
        }
    }
    else if (ins == INS_movsxd)
    {
        assert(hasCodeRM(ins) && !hasCodeMI(ins) && !hasCodeMR(ins));
        code = insCodeRM(ins);
        code = AddVexPrefixIfNeeded(ins, code, size);
        code = insEncodeRMreg(ins, code);

#endif // TARGET_AMD64
    }
#ifdef FEATURE_HW_INTRINSICS
    else if ((ins == INS_bsf) || (ins == INS_bsr) || (ins == INS_crc32) || (ins == INS_lzcnt) || (ins == INS_popcnt) ||
             (ins == INS_tzcnt))
    {
        assert(hasCodeRM(ins) && !hasCodeMI(ins) && !hasCodeMR(ins));
        code = insCodeRM(ins);
        code = AddVexPrefixIfNeeded(ins, code, size);
        code = insEncodeRMreg(ins, code);
        if ((ins == INS_crc32) && (size > EA_1BYTE))
        {
            code |= 0x0100;
        }

        if (size == EA_2BYTE)
        {
            assert(ins == INS_crc32);
            dst += emitOutputByte(dst, 0x66);
        }
        else if (size == EA_8BYTE)
        {
            code = AddRexWPrefix(ins, code);
        }
    }
#endif // FEATURE_HW_INTRINSICS
    else
    {
        assert(!TakesVexPrefix(ins));
        code = insCodeMR(ins);
        code = insEncodeMRreg(ins, code);

        if (ins != INS_test)
        {
            code |= 2;
        }

        switch (size)
        {
            case EA_1BYTE:
                noway_assert(RBM_BYTE_REGS & genRegMask(reg1));
                noway_assert(RBM_BYTE_REGS & genRegMask(reg2));
                break;

            case EA_2BYTE:
                // Output a size prefix for a 16-bit operand
                dst += emitOutputByte(dst, 0x66);
                FALLTHROUGH;

            case EA_4BYTE:
                // Set the 'w' bit to get the large version
                code |= 0x1;
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

                // Set the 'w' bit to get the large version
                code |= 0x1;
                break;

#endif // TARGET_AMD64

            default:
                assert(!"unexpected size");
        }
    }

    regNumber regFor012Bits = reg2;
    regNumber regFor345Bits = REG_NA;
    if (IsBMIInstruction(ins))
    {
        regFor345Bits = getBmiRegNumber(ins);
    }
    if (regFor345Bits == REG_NA)
    {
        regFor345Bits = reg1;
    }
    if (ins == INS_movd)
    {
        assert(isFloatReg(reg1) != isFloatReg(reg2));
        if (isFloatReg(reg2))
        {
            std::swap(regFor012Bits, regFor345Bits);
        }
    }

    unsigned regCode = insEncodeReg345(ins, regFor345Bits, size, &code);
    regCode |= insEncodeReg012(ins, regFor012Bits, size, &code);

    if (TakesVexPrefix(ins))
    {
        // In case of AVX instructions that take 3 operands, we generally want to encode reg1
        // as first source.  In this case, reg1 is both a source and a destination.
        // The exception is the "merge" 3-operand case, where we have a move instruction, such
        // as movss, and we want to merge the source with itself.
        //
        // TODO-XArch-CQ: Eventually we need to support 3 operand instruction formats. For
        // now we use the single source as source1 and source2.
        if (IsDstDstSrcAVXInstruction(ins))
        {
            // encode source/dest operand reg in 'vvvv' bits in 1's complement form
            code = insEncodeReg3456(ins, reg1, size, code);
        }
        else if (IsDstSrcSrcAVXInstruction(ins))
        {
            // encode source operand reg in 'vvvv' bits in 1's complement form
            code = insEncodeReg3456(ins, reg2, size, code);
        }
    }

    // Output the REX prefix
    dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

    if (code & 0xFF000000)
    {
        // Output the highest word of the opcode
        dst += emitOutputWord(dst, code >> 16);
        code &= 0x0000FFFF;

        if (Is4ByteSSEInstruction(ins))
        {
            // Output 3rd byte of the opcode
            dst += emitOutputByte(dst, code);
            code &= 0xFF00;
        }
    }
    else if (code & 0x00FF0000)
    {
        dst += emitOutputByte(dst, code >> 16);
        code &= 0x0000FFFF;
    }

    // TODO-XArch-CQ: Right now support 4-byte opcode instructions only
    if ((code & 0xFF00) == 0xC000)
    {
        dst += emitOutputWord(dst, code | (regCode << 8));
    }
    else if ((code & 0xFF) == 0x00)
    {
        // This case happens for some SSE/AVX instructions only
        assert(IsAVXInstruction(ins) || Is4ByteSSEInstruction(ins));

        dst += emitOutputByte(dst, (code >> 8) & 0xFF);
        dst += emitOutputByte(dst, (0xC0 | regCode));
    }
    else
    {
        dst += emitOutputWord(dst, code);
        dst += emitOutputByte(dst, (0xC0 | regCode));
    }

    // Does this instruction operate on a GC ref value?
    if (id->idGCref())
    {
        switch (id->idInsFmt())
        {
            case IF_RRD_RRD:
                break;

            case IF_RWR_RRD:
                emitGCregLiveUpd(id->idGCref(), reg1, dst);
                break;

            case IF_RRW_RRD:

                switch (id->idIns())
                {
                    /*
                        This must be one of the following cases:

                        xor reg, reg        to assign NULL

                        and r1 , r2         if (ptr1 && ptr2) ...
                        or  r1 , r2         if (ptr1 || ptr2) ...

                        add r1 , r2         to compute a normal byref
                        sub r1 , r2         to compute a strange byref (VC only)

                    */
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
#ifdef DEBUG
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
#endif // DEBUG
#endif // 0

                        // Mark r1 as holding a byref
                        emitGCregLiveUpd(GCT_BYREF, reg1, dst);
                        break;

                    default:
                        INDEBUG(emitDispIns(id));
                        assert(!"unexpected GC base update instruction");
                }

                break;

            case IF_RRW_RRW:
                // This must be "xchg reg1, reg2"
                assert(id->idIns() == INS_xchg);

                // If we got here, the GC-ness of the registers doesn't match, so we have to "swap" them in the GC
                // register pointer mask.

                GCtype gc1, gc2;

                gc1 = gcInfo.GetRegType(reg1);
                gc2 = gcInfo.GetRegType(reg2);

                if (gc1 != gc2)
                {
                    // Kill the GC-info about the GC registers

                    if (gc1 != GCT_NONE)
                    {
                        emitGCregDeadUpd(reg1, dst);
                    }

                    if (gc2 != GCT_NONE)
                    {
                        emitGCregDeadUpd(reg2, dst);
                    }

                    // Now, swap the info

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

            default:
                INDEBUG(emitDispIns(id));
                assert(!"unexpected GC ref instruction format");
        }
    }
    else
    {
        if (!emitInsCanOnlyWriteSSE2OrAVXReg(id))
        {
            switch (id->idInsFmt())
            {
                case IF_RRD_CNS:
                    // INS_mulEAX can not be used with any of these formats
                    assert(ins != INS_mulEAX && ins != INS_imulEAX);

                    // For the three operand imul instruction the target
                    // register is encoded in the opcode

                    if (instrIs3opImul(ins))
                    {
                        regNumber tgtReg = inst3opImulReg(ins);
                        emitGCregDeadUpd(tgtReg, dst);
                    }
                    break;

                case IF_RWR_RRD:
                case IF_RRW_RRD:
                case IF_RWR_RRD_RRD:
                    emitGCregDeadUpd(reg1, dst);
                    break;

                default:
                    break;
            }
        }
    }

    return dst;
}

BYTE* emitter::emitOutputRRR(BYTE* dst, instrDesc* id)
{
    code_t code;

    instruction ins = id->idIns();
    assert(IsAVXInstruction(ins));
    assert(IsThreeOperandAVXInstruction(ins) || isAvxBlendv(ins));
    regNumber targetReg = id->idReg1();
    regNumber src1      = id->idReg2();
    regNumber src2      = id->idReg3();
    emitAttr  size      = id->idOpSize();

    code = insCodeRM(ins);
    code = AddVexPrefixIfNeeded(ins, code, size);
    code = insEncodeRMreg(ins, code);

    if (TakesRexWPrefix(ins, size))
    {
        code = AddRexWPrefix(ins, code);
    }

    unsigned regCode = insEncodeReg345(ins, targetReg, size, &code);
    regCode |= insEncodeReg012(ins, src2, size, &code);
    // encode source operand reg in 'vvvv' bits in 1's complement form
    code = insEncodeReg3456(ins, src1, size, code);

    // Output the REX prefix
    dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

    // Is this a 'big' opcode?
    if (code & 0xFF000000)
    {
        // Output the highest word of the opcode
        dst += emitOutputWord(dst, code >> 16);
        code &= 0x0000FFFF;
    }
    else if (code & 0x00FF0000)
    {
        dst += emitOutputByte(dst, code >> 16);
        code &= 0x0000FFFF;
    }

    // TODO-XArch-CQ: Right now support 4-byte opcode instructions only
    if ((code & 0xFF00) == 0xC000)
    {
        dst += emitOutputWord(dst, code | (regCode << 8));
    }
    else if ((code & 0xFF) == 0x00)
    {
        // This case happens for AVX instructions only
        assert(IsAVXInstruction(ins));

        dst += emitOutputByte(dst, (code >> 8) & 0xFF);
        dst += emitOutputByte(dst, (0xC0 | regCode));
    }
    else
    {
        dst += emitOutputWord(dst, code);
        dst += emitOutputByte(dst, (0xC0 | regCode));
    }

    noway_assert(!id->idGCref());

    if (!emitInsCanOnlyWriteSSE2OrAVXReg(id))
    {
        switch (id->idInsFmt())
        {
            case IF_RWR_RRD_RRD:
            case IF_RWR_RRD_RRD_CNS:
            case IF_RWR_RRD_RRD_RRD:
                emitGCregDeadUpd(id->idReg1(), dst);
                break;

            default:
                break;
        }
    }

    return dst;
}

/*****************************************************************************
 *
 *  Output an instruction with a register and constant operands.
 */

BYTE* emitter::emitOutputRI(BYTE* dst, instrDesc* id)
{
    code_t      code;
    emitAttr    size    = id->idOpSize();
    instruction ins     = id->idIns();
    regNumber   reg     = id->idReg1();
    ssize_t     imm     = emitGetInsSC(id);
    bool        hasImm8 = IsImm8(imm) && (ins != INS_mov) && (ins != INS_test) && !id->idIsCnsReloc();

    // BT reg,imm might be useful but it requires special handling of the immediate value
    // (it is always encoded in a byte). Let's not complicate things until this is needed.
    assert(ins != INS_bt);

    noway_assert(emitVerifyEncodable(ins, size, reg));

    if (IsSSEOrAVXInstruction(ins))
    {
        // Handle SSE2 instructions of the form "opcode reg, immed8"

        assert(id->idGCref() == GCT_NONE);
        assert(hasImm8);

        // The left and right shifts use the same encoding, and are distinguished by the Reg/Opcode field.
        regNumber regOpcode = getSseShiftRegNumber(ins);

        // Get the 'base' opcode.
        code = insCodeMI(ins);
        code = AddVexPrefixIfNeeded(ins, code, size);
        code = insEncodeMIreg(ins, reg, size, code);
        assert(code & 0x00FF0000);
        if (TakesVexPrefix(ins))
        {
            // The 'vvvv' bits encode the destination register, which for this case (RI)
            // is the same as the source.
            code = insEncodeReg3456(ins, reg, size, code);
        }

        unsigned regcode = (insEncodeReg345(ins, regOpcode, size, &code) | insEncodeReg012(ins, reg, size, &code)) << 8;

        // Output the REX prefix
        dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

        if (code & 0xFF000000)
        {
            dst += emitOutputWord(dst, code >> 16);
        }
        else if (code & 0xFF0000)
        {
            dst += emitOutputByte(dst, code >> 16);
        }

        dst += emitOutputWord(dst, code | regcode);

        dst += emitOutputByte(dst, imm);

        return dst;
    }

    // The 'mov' opcode is special
    if (ins == INS_mov)
    {
        code = insCodeACC(ins);
        assert(code < 0x100);

        code |= 0x08; // Set the 'w' bit
        unsigned regcode = insEncodeReg012(ins, reg, size, &code);
        code |= regcode;

        // This is INS_mov and will not take VEX prefix
        assert(!TakesVexPrefix(ins));

        if (TakesRexWPrefix(ins, size))
        {
            code = AddRexWPrefix(ins, code);
        }

        dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);
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

        goto DONE;
    }

    // Decide which encoding is the shortest
    bool useSigned, useACC;

    if (reg == REG_EAX && !instrIs3opImul(ins))
    {
        if (size == EA_1BYTE || (ins == INS_test))
        {
            // For al, ACC encoding is always the smallest
            useSigned = false;
            useACC    = true;
        }
        else
        {
            /* For ax/eax, we avoid ACC encoding for small constants as we
             * can emit the small constant and have it sign-extended.
             * For big constants, the ACC encoding is better as we can use
             * the 1 byte opcode
             */

            if (hasImm8)
            {
                // avoid using ACC encoding
                useSigned = true;
                useACC    = false;
            }
            else
            {
                useSigned = false;
                useACC    = true;
            }
        }
    }
    else
    {
        useACC    = false;
        useSigned = hasImm8;
    }

    // "test" has no 's' bit
    if (ins == INS_test)
    {
        useSigned = false;
    }

    // Get the 'base' opcode
    if (useACC)
    {
        assert(!useSigned);
        code = insCodeACC(ins);
    }
    else
    {
        assert(!useSigned || hasImm8);

        // Some instructions (at least 'imul') do not have a
        // r/m, immed form, but do have a dstReg,srcReg,imm8 form.
        if (hasImm8 && useSigned && insNeedsRRIb(ins))
        {
            code = insEncodeRRIb(ins, reg, size);
        }
        else
        {
            code = insCodeMI(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);
            code = insEncodeMIreg(ins, reg, size, code);
        }
    }

    switch (size)
    {
        case EA_1BYTE:
            break;

        case EA_2BYTE:
            // Output a size prefix for a 16-bit operand
            dst += emitOutputByte(dst, 0x66);
            FALLTHROUGH;

        case EA_4BYTE:
            // Set the 'w' bit to get the large version
            code |= 0x1;
            break;

#ifdef TARGET_AMD64
        case EA_8BYTE:
            /* Set the 'w' bit to get the large version */
            /* and the REX.W bit to get the really large version */

            code = AddRexWPrefix(ins, code);
            code |= 0x1;
            break;
#endif

        default:
            assert(!"unexpected size");
    }

    // Output the REX prefix
    dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

    // Does the value fit in a sign-extended byte?
    // Important!  Only set the 's' bit when we have a size larger than EA_1BYTE.
    // Note: A sign-extending immediate when (size == EA_1BYTE) is invalid in 64-bit mode.

    if (useSigned && (size > EA_1BYTE))
    {
        // We can just set the 's' bit, and issue an immediate byte

        code |= 0x2; // Set the 's' bit to use a sign-extended immediate byte.
        dst += emitOutputWord(dst, code);
        dst += emitOutputByte(dst, imm);
    }
    else
    {
        // Can we use an accumulator (EAX) encoding?
        if (useACC)
        {
            dst += emitOutputByte(dst, code);
        }
        else
        {
            dst += emitOutputWord(dst, code);
        }

        dst += emitOutputImm(dst, EA_SIZE_IN_BYTES(size), CnsVal{imm, id->idIsCnsReloc()});
    }

DONE:
    if (id->idGCref())
    {
        switch (id->idInsFmt())
        {
            case IF_RRD_CNS:
                break;

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
                // Mark it as holding a GCT_BYREF
                emitGCregLiveUpd(GCT_BYREF, id->idReg1(), dst);
                break;

            default:
                INDEBUG(emitDispIns(id));
                assert(!"unexpected GC ref instruction format");
        }

        // mul can never produce a GC ref
        assert(!instrIs3opImul(ins));
        assert(ins != INS_mulEAX && ins != INS_imulEAX);
    }
    else
    {
        switch (id->idInsFmt())
        {
            case IF_RRD_CNS:
                // INS_mulEAX can not be used with any of these formats
                assert(ins != INS_mulEAX && ins != INS_imulEAX);

                // For the three operand imul instruction the target
                // register is encoded in the opcode

                if (instrIs3opImul(ins))
                {
                    regNumber tgtReg = inst3opImulReg(ins);
                    emitGCregDeadUpd(tgtReg, dst);
                }
                break;

            case IF_RRW_CNS:
            case IF_RWR_CNS:
                assert(!instrIs3opImul(ins));

                emitGCregDeadUpd(id->idReg1(), dst);
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

/*****************************************************************************
 *
 *  Output a local jump instruction.
 *  This function also handles non-jumps that have jump-like characteristics, like RIP-relative LEA of a label that
 *  needs to get bound to an actual address and processed by branch shortening.
 */

BYTE* emitter::emitOutputLJ(insGroup* ig, BYTE* dst, instrDesc* i)
{
    unsigned srcOffs;
    unsigned dstOffs;
    BYTE*    srcAddr;
    BYTE*    dstAddr;
    ssize_t  distVal;

    instrDescJmp* id  = (instrDescJmp*)i;
    instruction   ins = id->idIns();
    bool          jmp;

    // SSE/AVX doesnt make any sense here
    assert(!IsSSEInstruction(ins));
    assert(!IsAVXInstruction(ins));

    bool   relAddr = true; // does the instruction use relative-addressing?
    size_t ssz;
    size_t lsz;

    switch (ins)
    {
        default:
            ssz = JCC_SIZE_SMALL;
            lsz = JCC_SIZE_LARGE;
            jmp = true;
            break;

        case INS_jmp:
            ssz = JMP_SIZE_SMALL;
            lsz = JMP_SIZE_LARGE;
            jmp = true;
            break;

        case INS_call:
            ssz = lsz = CALL_INST_SIZE;
            jmp       = false;
            break;

        case INS_push_hide:
        case INS_push:
            ssz = lsz = 5;
            jmp       = false;
            relAddr   = false;
            break;

        case INS_mov:
        case INS_lea:
            ssz = lsz = id->idCodeSize();
            jmp       = false;
            relAddr   = false;
            break;
    }

    // Figure out the distance to the target
    srcOffs = emitCurCodeOffs(dst);
    srcAddr = emitOffsetToPtr(srcOffs);

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
        dstAddr = emitOffsetToPtr(dstOffs);
    }
    else
    {
        dstOffs = id->idAddr()->iiaIGlabel->igOffs;
        dstAddr = emitOffsetToPtr(dstOffs);
        if (!relAddr)
        {
            srcAddr = nullptr;
        }
    }

    distVal = (ssize_t)(dstAddr - srcAddr);

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
        if (jmp && distVal - ssz >= (size_t)JMP_DIST_SMALL_MAX_NEG)
        {
            emitSetShortJump(id);
        }
    }
    else
    {
        // This is a  forward jump - distance will be an upper limit
        emitFwdJumps = true;

        // The target offset will be closer by at least 'emitOffsAdj', but only if this
        // jump doesn't cross the hot-cold boundary.
        if (!emitJumpCrossHotColdBoundary(srcOffs, dstOffs))
        {
            dstOffs -= emitOffsAdj;
            distVal -= emitOffsAdj;
        }

        // Record the location of the jump for later patching
        id->idjOffs = dstOffs;

        // Are we overflowing the id->idjOffs bitfield?
        if (id->idjOffs != dstOffs)
        {
            IMPL_LIMITATION("Method is too large");
        }

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

        // Can we use a short jump?
        if (jmp && distVal - ssz <= (size_t)JMP_DIST_SMALL_MAX_POS)
        {
            emitSetShortJump(id);
        }
    }

    // Adjust the offset to emit relative to the end of the instruction
    if (relAddr)
    {
        distVal -= id->idjShort ? ssz : lsz;
    }

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

    // What size jump should we use?
    if (id->idjShort)
    {
        // Short jump
        assert(!id->idjKeepLong);
        assert(emitJumpCrossHotColdBoundary(srcOffs, dstOffs) == false);

        assert(JMP_SIZE_SMALL == JCC_SIZE_SMALL);
        assert(JMP_SIZE_SMALL == 2);

        assert(jmp);

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

        dst += emitOutputByte(dst, insCode(ins));

        // For forward jumps, record the address of the distance value
        id->idjTemp.idjAddr = (distVal > 0) ? dst : nullptr;

        dst += emitOutputByte(dst, distVal);
    }
    else
    {
        code_t code;

        // Long  jump
        if (jmp)
        {
            // clang-format off
            assert(INS_jmp + (INS_l_jmp - INS_jmp) == INS_l_jmp);
            assert(INS_jo  + (INS_l_jmp - INS_jmp) == INS_l_jo);
            assert(INS_jb  + (INS_l_jmp - INS_jmp) == INS_l_jb);
            assert(INS_jae + (INS_l_jmp - INS_jmp) == INS_l_jae);
            assert(INS_je  + (INS_l_jmp - INS_jmp) == INS_l_je);
            assert(INS_jne + (INS_l_jmp - INS_jmp) == INS_l_jne);
            assert(INS_jbe + (INS_l_jmp - INS_jmp) == INS_l_jbe);
            assert(INS_ja  + (INS_l_jmp - INS_jmp) == INS_l_ja);
            assert(INS_js  + (INS_l_jmp - INS_jmp) == INS_l_js);
            assert(INS_jns + (INS_l_jmp - INS_jmp) == INS_l_jns);
            assert(INS_jp  + (INS_l_jmp - INS_jmp) == INS_l_jp);
            assert(INS_jnp + (INS_l_jmp - INS_jmp) == INS_l_jnp);
            assert(INS_jl  + (INS_l_jmp - INS_jmp) == INS_l_jl);
            assert(INS_jge + (INS_l_jmp - INS_jmp) == INS_l_jge);
            assert(INS_jle + (INS_l_jmp - INS_jmp) == INS_l_jle);
            assert(INS_jg  + (INS_l_jmp - INS_jmp) == INS_l_jg);
            // clang-format on

            code = insCode((instruction)(ins + (INS_l_jmp - INS_jmp)));
        }
        else if (ins == INS_push || ins == INS_push_hide)
        {
            assert(insCodeMI(INS_push) == 0x68);
            code = 0x68;
        }
        else if (ins == INS_lea)
        {
            // Make an instrDesc that looks like IF_RWR_ARD so that emitOutputAM emits the r/m32 for us.
            // We basically are doing what emitIns_R_AI does.
            // TODO-XArch-Cleanup: revisit this.
            instrDescAmd  idAmdStackLocal;
            instrDescAmd* idAmd = &idAmdStackLocal;
            *(instrDesc*)idAmd  = *(instrDesc*)id; // copy all the "core" fields
            memset((BYTE*)idAmd + sizeof(instrDesc), 0,
                   sizeof(instrDescAmd) - sizeof(instrDesc)); // zero out the tail that wasn't copied

            idAmd->idInsFmt(IF_RWR_ARD);
            idAmd->idAddr()->iiaAddrMode.amBaseReg = REG_NA;
            idAmd->idAddr()->iiaAddrMode.amIndxReg = REG_NA;
            emitSetAmdDisp(idAmd, distVal);
            idAmd->idSetIsDspReloc(id->idIsDspReloc());

            unsigned sz = emitInsSizeAM(idAmd, insCodeRM(ins));
            idAmd->idCodeSize(sz);

            code = insCodeRM(ins);
            code |= (insEncodeReg345(ins, id->idReg1(), EA_PTRSIZE, &code) << 8);

            dst = emitOutputAM(dst, idAmd, code, nullptr);

            code = 0xCC;

            // For forward jumps, record the address of the distance value
            // Hard-coded 4 here because we already output the displacement, as the last thing.
            id->idjTemp.idjAddr = (dstOffs > srcOffs) ? (dst - 4) : nullptr;

            // We're done
            return dst;
        }
        else
        {
            code = 0xE8;
        }

        if (ins != INS_mov)
        {
            dst += emitOutputByte(dst, code);

            if (code & 0xFF00)
            {
                dst += emitOutputByte(dst, code >> 8);
            }
        }

        // For forward jumps, record the address of the distance value
        id->idjTemp.idjAddr = (dstOffs > srcOffs) ? dst : nullptr;

        dst += emitOutputLong(dst, distVal);

#ifdef TARGET_X86 // For x64 we always go through recordRelocation since we may need jump stubs.
        if (emitComp->opts.compReloc)
#endif
        {
            if (!relAddr)
            {
                emitRecordRelocation(dst - 4, reinterpret_cast<void*>(distVal), IMAGE_REL_BASED_HIGHLOW);
            }
            else if (emitJumpCrossHotColdBoundary(srcOffs, dstOffs))
            {
                assert(id->idjKeepLong);
                emitRecordRelocation(dst - 4, dst + distVal, IMAGE_REL_BASED_REL32);
            }
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

/*****************************************************************************
 *
 *  Append the machine code corresponding to the given instruction descriptor
 *  to the code block at '*dp'; the base of the code block is 'bp', and 'ig'
 *  is the instruction group that contains the instruction. Updates '*dp' to
 *  point past the generated code, and returns the size of the instruction
 *  descriptor in bytes.
 */

#ifdef _PREFAST_
#pragma warning(push)
#pragma warning(disable : 21000) // Suppress PREFast warning about overly large function
#endif
size_t emitter::emitOutputInstr(insGroup* ig, instrDesc* id, BYTE** dp)
{
    assert(emitIssuing);

    BYTE*       dst = *dp;
    size_t      sz  = sizeof(instrDesc);
    instruction ins = id->idIns();

#ifdef DEBUG
    bool dspOffs = emitComp->opts.dspGCtbls;
#endif // DEBUG

    emitAttr size = id->idOpSize();

    assert(REG_NA == (int)REG_NA);

    assert(ins != INS_imul || size >= EA_4BYTE);                  // Has no 'w' bit
    assert(instrIs3opImul(id->idIns()) == 0 || size >= EA_4BYTE); // Has no 'w' bit

    // What instruction format have we got?
    switch (id->idInsFmt())
    {
        code_t   code;
        unsigned regcode;
        CnsVal   cnsVal;
        void*    addr;

        /********************************************************************/
        /*                        No operands                               */
        /********************************************************************/
        case IF_NONE:
            // the loop alignment pseudo instruction
            if (ins == INS_align)
            {
                sz = sizeof(instrDescAlign);
                // IG can be marked as not needing alignment after emitting align instruction
                // In such case, skip outputting alignment.
                if (ig->isLoopAlign())
                {
                    dst = emitOutputAlign(ig, id, dst);
                }
#ifdef DEBUG
                else
                {
                    // If the IG is not marked as need alignment, then the code size
                    // should be zero i.e. no padding needed.
                    assert(id->idCodeSize() == 0);
                }
#endif
                break;
            }

            if (ins == INS_nop)
            {
                BYTE* dstRW = dst + writeableOffset;
                dstRW       = emitOutputNOP(dstRW, id->idCodeSize());
                dst         = dstRW - writeableOffset;
                break;
            }

            // the cdq instruction kills the EDX register implicitly
            if (ins == INS_cdq)
            {
                emitGCregDeadUpd(REG_EDX, dst);
            }

            assert(id->idGCref() == GCT_NONE);

            code = insCodeMR(ins);

#ifdef TARGET_AMD64
            // Support only scalar AVX instructions and hence size is hard coded to 4-byte.
            code = AddVexPrefixIfNeeded(ins, code, EA_4BYTE);

            if (ins == INS_cdq && TakesRexWPrefix(ins, id->idOpSize()))
            {
                code = AddRexWPrefix(ins, code);
            }
            dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);
#endif
            // Is this a 'big' opcode?
            if (code & 0xFF000000)
            {
                // The high word and then the low word
                dst += emitOutputWord(dst, code >> 16);
                code &= 0x0000FFFF;
                dst += emitOutputWord(dst, code);
            }
            else if (code & 0x00FF0000)
            {
                // The high byte and then the low word
                dst += emitOutputByte(dst, code >> 16);
                code &= 0x0000FFFF;
                dst += emitOutputWord(dst, code);
            }
            else if (code & 0xFF00)
            {
                // The 2 byte opcode
                dst += emitOutputWord(dst, code);
            }
            else
            {
                // The 1 byte opcode
                dst += emitOutputByte(dst, code);
            }

            break;

        /********************************************************************/
        /*                Simple constant, local label, method              */
        /********************************************************************/

        case IF_CNS:
            dst = emitOutputIV(dst, id);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_LABEL:
        case IF_RWR_LABEL:
            assert(id->idGCref() == GCT_NONE);
            assert(id->idIsBound());

            // TODO-XArch-Cleanup: handle IF_RWR_LABEL in emitOutputLJ() or change it to emitOutputAM()?
            dst = emitOutputLJ(ig, dst, id);
            sz  = sizeof(instrDescJmp);
            break;

        case IF_METHOD:
        case IF_METHPTR:
            addr = id->idAddr()->iiaAddr;
            assert(addr != nullptr);

            if (id->idInsFmt() == IF_METHPTR)
            {
                // This is call indirect via a method pointer

                code = insCodeMR(ins);
                if (ins == INS_i_jmp)
                {
                    code |= 1;
                }

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
                // This is call direct where we know the target, thus we can
                // use a direct call; the target to jump to is in iiaAddr.
                assert(id->idInsFmt() == IF_METHOD);

                dst += emitOutputByte(dst, ins == INS_l_jmp ? insCode(ins) : insCodeMI(ins));

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

        DONE_CALL:
            sz = emitRecordGCCall(id, *dp, dst);

#ifdef DEBUG
            if (ins == INS_call)
            {
                emitRecordCallSite(emitCurCodeOffs(*dp), id->idDebugOnlyInfo()->idCallSig,
                                   static_cast<CORINFO_METHOD_HANDLE>(id->idDebugOnlyInfo()->idHandle));
            }
#endif

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
                goto DONE_CALL;
            }

            sz = SMALL_IDSC_SIZE;
            break;

        /********************************************************************/
        /*                 Register and register/constant                   */
        /********************************************************************/

        case IF_RRW_SHF:
            code = insCodeMR(ins);
            // Emit the VEX prefix if it exists
            code = AddVexPrefixIfNeeded(ins, code, size);
            code = insEncodeMRreg(ins, id->idReg1(), size, code);

            // set the W bit
            if (size != EA_1BYTE)
            {
                code |= 1;
            }

            // Emit the REX prefix if it exists
            if (TakesRexWPrefix(ins, size))
            {
                code = AddRexWPrefix(ins, code);
            }

            // Output a size prefix for a 16-bit operand
            if (size == EA_2BYTE)
            {
                dst += emitOutputByte(dst, 0x66);
            }

            dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);
            dst += emitOutputWord(dst, code);
            dst += emitOutputByte(dst, emitGetInsSC(id));
            sz = emitSizeOfInsDsc(id);

            // Update GC info.
            assert(!id->idGCref());
            emitGCregDeadUpd(id->idReg1(), dst);
            break;

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
            sz  = emitSizeOfInsDsc(id);
            dst += emitOutputByte(dst, emitGetInsSC(id));
            break;

        case IF_RRW_RRW_CNS:
            assert(id->idGCref() == GCT_NONE);

            // Get the 'base' opcode (it's a big one)
            // Also, determine which operand goes where in the ModRM byte.
            regNumber mReg;
            regNumber rReg;
            if (hasCodeMR(ins))
            {
                code = insCodeMR(ins);
                // Emit the VEX prefix if it exists
                code = AddVexPrefixIfNeeded(ins, code, size);
                code = insEncodeMRreg(ins, code);
                mReg = id->idReg1();
                rReg = id->idReg2();
            }
            else if (hasCodeMI(ins))
            {
                code = insCodeMI(ins);

                // Emit the VEX prefix if it exists
                code = AddVexPrefixIfNeeded(ins, code, size);

                assert((code & 0xC000) == 0);
                code |= 0xC000;

                mReg = id->idReg2();

                // The left and right shifts use the same encoding, and are distinguished by the Reg/Opcode field.
                rReg = getSseShiftRegNumber(ins);
            }
            else
            {
                code = insCodeRM(ins);
                // Emit the VEX prefix if it exists
                code = AddVexPrefixIfNeeded(ins, code, size);
                code = insEncodeRMreg(ins, code);
                mReg = id->idReg2();
                rReg = id->idReg1();
            }
            assert(code & 0x00FF0000);

            if (TakesRexWPrefix(ins, size))
            {
                code = AddRexWPrefix(ins, code);
            }

            if (TakesVexPrefix(ins))
            {
                if (IsDstDstSrcAVXInstruction(ins))
                {
                    // Encode source/dest operand reg in 'vvvv' bits in 1's complement form
                    // This code will have to change when we support 3 operands.
                    // For now, we always overload this source with the destination (always reg1).
                    // (Though we will need to handle the few ops that can have the 'vvvv' bits as destination,
                    // e.g. pslldq, when/if we support those instructions with 2 registers.)
                    // (see x64 manual Table 2-9. Instructions with a VEX.vvvv destination)
                    code = insEncodeReg3456(ins, id->idReg1(), size, code);
                }
                else if (IsDstSrcSrcAVXInstruction(ins))
                {
                    // This is a "merge" move instruction.
                    // Encode source operand reg in 'vvvv' bits in 1's complement form
                    code = insEncodeReg3456(ins, id->idReg2(), size, code);
                }
            }

            regcode = (insEncodeReg345(ins, rReg, size, &code) | insEncodeReg012(ins, mReg, size, &code));

            // Output the REX prefix
            dst += emitOutputRexOrVexPrefixIfNeeded(ins, dst, code);

            if (code & 0xFF000000)
            {
                // Output the highest word of the opcode
                dst += emitOutputWord(dst, code >> 16);
                code &= 0x0000FFFF;

                if (Is4ByteSSEInstruction(ins))
                {
                    // Output 3rd byte of the opcode
                    dst += emitOutputByte(dst, code);
                    code &= 0xFF00;
                }
            }
            else if (code & 0x00FF0000)
            {
                dst += emitOutputByte(dst, code >> 16);
                code &= 0x0000FFFF;
            }

            // TODO-XArch-CQ: Right now support 4-byte opcode instructions only
            if ((code & 0xFF00) == 0xC000)
            {
                dst += emitOutputWord(dst, code | (regcode << 8));
            }
            else if ((code & 0xFF) == 0x00)
            {
                // This case happens for some SSE/AVX instructions only
                assert(IsAVXInstruction(ins) || Is4ByteSSEInstruction(ins));

                dst += emitOutputByte(dst, (code >> 8) & 0xFF);
                dst += emitOutputByte(dst, (0xC0 | regcode));
            }
            else
            {
                dst += emitOutputWord(dst, code);
                dst += emitOutputByte(dst, (0xC0 | regcode));
            }

            dst += emitOutputByte(dst, emitGetInsSC(id));
            sz = emitSizeOfInsDsc(id);

            // Kill any GC ref in the destination register if necessary.
            if (!emitInsCanOnlyWriteSSE2OrAVXReg(id))
            {
                emitGCregDeadUpd(id->idReg1(), dst);
            }
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
                goto DONE_CALL;
            }

            sz = emitSizeOfInsDsc(id);
            break;

        case IF_RRW_ARD_CNS:
        case IF_RWR_ARD_CNS:
            assert(IsSSEOrAVXInstruction(ins));
            emitGetInsAmdCns(id, &cnsVal);
            code = insCodeRM(ins);

            // Special case 4-byte AVX instructions
            if (EncodedBySSE38orSSE3A(ins))
            {
                dst = emitOutputAM(dst, id, code, &cnsVal);
            }
            else
            {
                code    = AddVexPrefixIfNeeded(ins, code, size);
                regcode = (insEncodeReg345(ins, id->idReg1(), size, &code) << 8);
                dst     = emitOutputAM(dst, id, code | regcode, &cnsVal);
            }

            sz = emitSizeOfInsDsc(id);
            break;

        case IF_AWR_RRD_CNS:
            assert(ins == INS_vextracti128 || ins == INS_vextractf128);
            assert(UseVEXEncoding());
            emitGetInsAmdCns(id, &cnsVal);
            code = insCodeMR(ins);
            dst  = emitOutputAM(dst, id, code, &cnsVal);
            sz   = emitSizeOfInsDsc(id);
            break;

        case IF_RRD_ARD:
        case IF_RWR_ARD:
        case IF_RRW_ARD:
        case IF_RWR_RRD_ARD:
        {
            code = insCodeRM(ins);
            if (EncodedBySSE38orSSE3A(ins) || (ins == INS_crc32))
            {
                dst = emitOutputAM(dst, id, code);
            }
            else
            {
                code    = AddVexPrefixIfNeeded(ins, code, size);
                regcode = (insEncodeReg345(ins, id->idReg1(), size, &code) << 8);
                dst     = emitOutputAM(dst, id, code | regcode);
            }
            sz = emitSizeOfInsDsc(id);
            break;
        }

        case IF_RWR_ARD_RRD:
        {
            assert(IsAVX2GatherInstruction(ins));
            code = insCodeRM(ins);
            dst  = emitOutputAM(dst, id, code);
            sz   = emitSizeOfInsDsc(id);
            break;
        }

        case IF_RWR_RRD_ARD_CNS:
        case IF_RWR_RRD_ARD_RRD:
        {
            assert(IsSSEOrAVXInstruction(ins));
            emitGetInsAmdCns(id, &cnsVal);
            code = insCodeRM(ins);
            if (EncodedBySSE38orSSE3A(ins))
            {
                dst = emitOutputAM(dst, id, code, &cnsVal);
            }
            else
            {
                code    = AddVexPrefixIfNeeded(ins, code, size);
                regcode = (insEncodeReg345(ins, id->idReg1(), size, &code) << 8);
                dst     = emitOutputAM(dst, id, code | regcode, &cnsVal);
            }
            sz = emitSizeOfInsDsc(id);
            break;
        }

        case IF_ARD_RRD:
        case IF_AWR_RRD:
        case IF_ARW_RRD:
            code    = insCodeMR(ins);
            code    = AddVexPrefixIfNeeded(ins, code, size);
            regcode = (insEncodeReg345(ins, id->idReg1(), size, &code) << 8);
            dst     = emitOutputAM(dst, id, code | regcode);
            sz      = emitSizeOfInsDsc(id);
            break;

        case IF_AWR_RRD_RRD:
        {
            code = insCodeMR(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);
            dst  = emitOutputAM(dst, id, code);
            sz   = emitSizeOfInsDsc(id);
            break;
        }

        case IF_ARD_CNS:
        case IF_AWR_CNS:
        case IF_ARW_CNS:
            emitGetInsAmdCns(id, &cnsVal);
            dst = emitOutputAM(dst, id, insCodeMI(ins), &cnsVal);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_ARW_SHF:
            emitGetInsAmdCns(id, &cnsVal);
            dst = emitOutputAM(dst, id, insCodeMR(ins), &cnsVal);
            sz  = emitSizeOfInsDsc(id);
            break;

        /********************************************************************/
        /*                      Stack-based operand                         */
        /********************************************************************/

        case IF_SRD:
        case IF_SWR:
        case IF_SRW:

            assert(ins != INS_pop_hide);
            if (ins == INS_pop)
            {
                // The offset in "pop [ESP+xxx]" is relative to the new ESP value
                CLANG_FORMAT_COMMENT_ANCHOR;

#if !FEATURE_FIXED_OUT_ARGS
                emitCurStackLvl -= sizeof(int);
#endif
                dst = emitOutputSV(dst, id, insCodeMR(ins));

#if !FEATURE_FIXED_OUT_ARGS
                emitCurStackLvl += sizeof(int);
#endif
                break;
            }

            dst = emitOutputSV(dst, id, insCodeMR(ins));

            if (ins == INS_call)
            {
                goto DONE_CALL;
            }

            break;

        case IF_SRD_CNS:
        case IF_SWR_CNS:
        case IF_SRW_CNS:
            emitGetInsCns(id, &cnsVal);
            dst = emitOutputSV(dst, id, insCodeMI(ins), &cnsVal);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_SRW_SHF:
            emitGetInsCns(id, &cnsVal);
            dst = emitOutputSV(dst, id, insCodeMR(ins), &cnsVal);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_SWR_RRD_CNS:
            assert(ins == INS_vextracti128 || ins == INS_vextractf128);
            assert(UseVEXEncoding());
            emitGetInsAmdCns(id, &cnsVal);
            code = insCodeMR(ins);
            dst  = emitOutputSV(dst, id, insCodeMR(ins), &cnsVal);
            sz   = emitSizeOfInsDsc(id);
            break;

        case IF_RRW_SRD_CNS:
        case IF_RWR_SRD_CNS:
            assert(IsSSEOrAVXInstruction(ins));
            emitGetInsCns(id, &cnsVal);
            code = insCodeRM(ins);

            // Special case 4-byte AVX instructions
            if (EncodedBySSE38orSSE3A(ins))
            {
                dst = emitOutputSV(dst, id, code, &cnsVal);
            }
            else
            {
                code = AddVexPrefixIfNeeded(ins, code, size);

                // In case of AVX instructions that take 3 operands, encode reg1 as first source.
                // Note that reg1 is both a source and a destination.
                //
                // TODO-XArch-CQ: Eventually we need to support 3 operand instruction formats. For
                // now we use the single source as source1 and source2.
                // For this format, moves do not support a third operand, so we only need to handle the binary ops.
                if (IsDstDstSrcAVXInstruction(ins))
                {
                    // encode source operand reg in 'vvvv' bits in 1's complement form
                    code = insEncodeReg3456(ins, id->idReg1(), size, code);
                }

                regcode = (insEncodeReg345(ins, id->idReg1(), size, &code) << 8);
                dst     = emitOutputSV(dst, id, code | regcode, &cnsVal);
            }

            sz = emitSizeOfInsDsc(id);
            break;

        case IF_RRD_SRD:
        case IF_RWR_SRD:
        case IF_RRW_SRD:
        {
            code = insCodeRM(ins);

            // 4-byte AVX instructions are special cased inside emitOutputSV
            // since they do not have space to encode ModRM byte.
            if (EncodedBySSE38orSSE3A(ins) || (ins == INS_crc32))
            {
                dst = emitOutputSV(dst, id, code);
            }
            else
            {
                code = AddVexPrefixIfNeeded(ins, code, size);

                if (IsDstDstSrcAVXInstruction(ins))
                {
                    // encode source operand reg in 'vvvv' bits in 1's complement form
                    code = insEncodeReg3456(ins, id->idReg1(), size, code);
                }

                regcode = (insEncodeReg345(ins, id->idReg1(), size, &code) << 8);
                dst     = emitOutputSV(dst, id, code | regcode);
            }

            sz = emitSizeOfInsDsc(id);
            break;
        }

        case IF_RWR_RRD_SRD:
        {
            // This should only be called on AVX instructions
            assert(IsAVXInstruction(ins));

            code = insCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);
            code = insEncodeReg3456(ins, id->idReg2(), size,
                                    code); // encode source operand reg in 'vvvv' bits in 1's complement form

            // 4-byte AVX instructions are special cased inside emitOutputSV
            // since they do not have space to encode ModRM byte.
            if (EncodedBySSE38orSSE3A(ins))
            {
                dst = emitOutputSV(dst, id, code);
            }
            else
            {
                regcode = (insEncodeReg345(ins, id->idReg1(), size, &code) << 8);
                dst     = emitOutputSV(dst, id, code | regcode);
            }
            break;
        }

        case IF_RWR_RRD_SRD_CNS:
        case IF_RWR_RRD_SRD_RRD:
        {
            // This should only be called on AVX instructions
            assert(IsAVXInstruction(ins));
            emitGetInsCns(id, &cnsVal);

            code = insCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);
            code = insEncodeReg3456(ins, id->idReg2(), size,
                                    code); // encode source operand reg in 'vvvv' bits in 1's complement form

            // 4-byte AVX instructions are special cased inside emitOutputSV
            // since they do not have space to encode ModRM byte.
            if (EncodedBySSE38orSSE3A(ins))
            {
                dst = emitOutputSV(dst, id, code, &cnsVal);
            }
            else
            {
                regcode = (insEncodeReg345(ins, id->idReg1(), size, &code) << 8);
                dst     = emitOutputSV(dst, id, code | regcode, &cnsVal);
            }

            sz = emitSizeOfInsDsc(id);
            break;
        }

        case IF_SRD_RRD:
        case IF_SWR_RRD:
        case IF_SRW_RRD:
            code = insCodeMR(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            // In case of AVX instructions that take 3 operands, encode reg1 as first source.
            // Note that reg1 is both a source and a destination.
            //
            // TODO-XArch-CQ: Eventually we need to support 3 operand instruction formats. For
            // now we use the single source as source1 and source2.
            // For this format, moves do not support a third operand, so we only need to handle the binary ops.
            if (IsDstDstSrcAVXInstruction(ins))
            {
                // encode source operand reg in 'vvvv' bits in 1's complement form
                code = insEncodeReg3456(ins, id->idReg1(), size, code);
            }

            regcode = (insEncodeReg345(ins, id->idReg1(), size, &code) << 8);
            dst     = emitOutputSV(dst, id, code | regcode);
            break;

        /********************************************************************/
        /*                    Direct memory address                         */
        /********************************************************************/

        case IF_RRW_MRD_CNS:
        case IF_RWR_MRD_CNS:
            assert(IsSSEOrAVXInstruction(ins));
            emitGetInsDcmCns(id, &cnsVal);
            code = insCodeRM(ins);

            // Special case 4-byte AVX instructions
            if (!EncodedBySSE38orSSE3A(ins))
            {
                code = AddVexPrefixIfNeeded(ins, code, size);

                // In case of AVX instructions that take 3 operands, encode reg1 as first source.
                // Note that reg1 is both a source and a destination.
                //
                // TODO-XArch-CQ: Eventually we need to support 3 operand instruction formats. For
                // now we use the single source as source1 and source2.
                // For this format, moves do not support a third operand, so we only need to handle the binary ops.
                if (IsDstDstSrcAVXInstruction(ins))
                {
                    // encode source operand reg in 'vvvv' bits in 1's complement form
                    code = insEncodeReg3456(ins, id->idReg1(), size, code);
                }

                regcode = (insEncodeReg345(ins, id->idReg1(), size, &code) << 8);
                code |= regcode | 0x0500;
            }

            dst = emitOutputCV(dst, id, code, &cnsVal);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_MWR_RRD_CNS:
            assert(ins == INS_vextracti128 || ins == INS_vextractf128);
            assert(UseVEXEncoding());
            emitGetInsDcmCns(id, &cnsVal);
            code = insCodeMR(ins);
            // only AVX2 vextracti128 and AVX vextractf128 can reach this path,
            // they do not need VEX.vvvv to encode the register operand
            dst = emitOutputCV(dst, id, code, &cnsVal);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_RRD_MRD:
        case IF_RWR_MRD:
        case IF_RRW_MRD:
            code = insCodeRM(ins);

            // Special case 4-byte AVX instructions
            if (!EncodedBySSE38orSSE3A(ins) && (ins != INS_crc32))
            {
                code = AddVexPrefixIfNeeded(ins, code, size);

                if (IsDstDstSrcAVXInstruction(ins))
                {
                    // encode source operand reg in 'vvvv' bits in 1's complement form
                    code = insEncodeReg3456(ins, id->idReg1(), size, code);
                }

                regcode = (insEncodeReg345(ins, id->idReg1(), size, &code) << 8);
                code |= regcode | 0x0500;
            }

            dst = emitOutputCV(dst, id, code);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_RWR_RRD_MRD:
            // This should only be called on AVX instructions
            assert(IsAVXInstruction(ins));

            code = insCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);
            // encode source operand reg in 'vvvv' bits in 1's complement form
            code = insEncodeReg3456(ins, id->idReg2(), size, code);

            // Special case 4-byte AVX instructions
            if (!EncodedBySSE38orSSE3A(ins))
            {
                regcode = (insEncodeReg345(ins, id->idReg1(), size, &code) << 8);
                code |= regcode | 0x0500;
            }

            dst = emitOutputCV(dst, id, code);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_RWR_RRD_MRD_CNS:
        case IF_RWR_RRD_MRD_RRD:
            // This should only be called on AVX instructions
            assert(IsAVXInstruction(ins));
            emitGetInsCns(id, &cnsVal);

            code = insCodeRM(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);
            // encode source operand reg in 'vvvv' bits in 1's complement form
            code = insEncodeReg3456(ins, id->idReg2(), size, code);

            // Special case 4-byte AVX instructions
            if (!EncodedBySSE38orSSE3A(ins))
            {
                regcode = (insEncodeReg345(ins, id->idReg1(), size, &code) << 8);
                code |= regcode | 0x0500;
            }

            dst = emitOutputCV(dst, id, code, &cnsVal);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_MRD_RRD:
        case IF_MWR_RRD:
        case IF_MRW_RRD:
            code = insCodeMR(ins);
            code = AddVexPrefixIfNeeded(ins, code, size);

            // In case of AVX instructions that take 3 operands, encode reg1 as first source.
            // Note that reg1 is both a source and a destination.
            //
            // TODO-XArch-CQ: Eventually we need to support 3 operand instruction formats. For
            // now we use the single source as source1 and source2.
            // For this format, moves do not support a third operand, so we only need to handle the binary ops.
            if (IsDstDstSrcAVXInstruction(ins))
            {
                // encode source operand reg in 'vvvv' bits in 1's complement form
                code = insEncodeReg3456(ins, id->idReg1(), size, code);
            }

            regcode = insEncodeReg345(ins, id->idReg1(), size, &code) << 8;

            dst = emitOutputCV(dst, id, code | regcode | 0x0500);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_MRD:
        case IF_MRW:
        case IF_MWR:
            noway_assert(ins != INS_call);
            dst = emitOutputCV(dst, id, insCodeMR(ins) | 0x0500);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_MRD_CNS:
        case IF_MWR_CNS:
        case IF_MRW_CNS:
            emitGetInsDcmCns(id, &cnsVal);
            dst = emitOutputCV(dst, id, insCodeMI(ins) | 0x0500, &cnsVal);
            sz  = emitSizeOfInsDsc(id);
            break;

        case IF_MRW_SHF:
            emitGetInsDcmCns(id, &cnsVal);
            dst = emitOutputCV(dst, id, insCodeMR(ins) | 0x0500, &cnsVal);
            sz  = emitSizeOfInsDsc(id);
            break;

        /********************************************************************/
        /*                            oops                                  */
        /********************************************************************/

        default:

#ifdef DEBUG
            printf("unexpected format %s\n", emitIfName(id->idInsFmt()));
            assert(!"don't know how to encode this instruction");
#endif
            break;
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
        emitDispIns(id, false, dspOffs, true, emitCurCodeOffs(*dp), *dp, (dst - *dp));
    }
#endif

#if FEATURE_LOOP_ALIGN
    // Only compensate over-estimated instructions if emitCurIG is before
    // the last IG that needs alignment.
    if (emitCurIG->igNum <= emitLastAlignedIgNum)
    {
        int diff = id->idCodeSize() - ((UNATIVE_OFFSET)(dst - *dp));
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

            BYTE* dstRW = dst + writeableOffset;
            dstRW       = emitOutputNOP(dstRW, diff);
            dst         = dstRW - writeableOffset;
        }
        assert((id->idCodeSize() - ((UNATIVE_OFFSET)(dst - *dp))) == 0);
    }
#endif

#ifdef DEBUG
    if (emitComp->compDebugBreak)
    {
        if (JitConfig.JitEmitPrintRefRegs() != 0)
        {
            printf("Before emitOutputInstr for id->idDebugOnlyInfo()->idNum=0x%02x\n", id->idDebugOnlyInfo()->idNum);
            printf("  REF regs");
            emitDispRegSet(gcInfo.GetLiveRegs(GCT_GCREF));
            printf("\n  BYREF regs");
            emitDispRegSet(gcInfo.GetLiveRegs(GCT_BYREF));
            printf("\n");
        }

        // For example, set JitBreakEmitOutputInstr=a6 will break when this method is called for
        // emitting instruction a6, (i.e. IN00a6 in jitdump).
        if ((unsigned)JitConfig.JitBreakEmitOutputInstr() == id->idDebugOnlyInfo()->idNum)
        {
            assert(!"JitBreakEmitOutputInstr reached");
        }
    }
#endif

    *dp = dst;

#ifdef DEBUG
    if (ins == INS_mulEAX || ins == INS_imulEAX)
    {
        // INS_mulEAX has implicit target of Edx:Eax. Make sure
        // that we detected this cleared its GC-status.
        assert((gcInfo.GetAllLiveRegs() & (RBM_EAX | RBM_EDX)) == RBM_NONE);
    }

    if (instrIs3opImul(ins))
    {
        // The target of the 3-operand imul is implicitly encoded. Make sure
        // that we detected the implicit register and cleared its GC-status.
        assert((gcInfo.GetAllLiveRegs() & genRegMask(inst3opImulReg(ins))) == RBM_NONE);
    }
#endif

    return sz;
}
#ifdef _PREFAST_
#pragma warning(pop)
#endif

emitter::insFormat emitter::getMemoryOperation(instrDesc* id)
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
        case IF_RRW_SHF:
        case IF_RRD_RRD:
        case IF_RWR_RRD:
        case IF_RRW_RRD:
        case IF_RRW_RRW:
        case IF_RRW_RRW_CNS:
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
        case IF_ARW_SHF:
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
        case IF_MRW_SHF:
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
        case IF_SRW_SHF:
            // Stack [RSP] - read and write
            result = IF_SWR;
            break;

        default:
            result = IF_NONE;
            break;
    }
    return result;
}

#if defined(DEBUG) || defined(LATE_DISASM)

//----------------------------------------------------------------------------------------
// getInsExecutionCharacteristics:
//    Returns the current instruction execution characteristics
//
// Arguments:
//    id  - The current instruction descriptor to be evaluated
//
// Return Value:
//    A struct containing the current instruction execution characteristics
//
// Notes:
//    The instruction latencies and throughput values returned by this function
//    are for the Intel Skylake-X processor and are from either:
//      1.  Agner.org - https://www.agner.org/optimize/instruction_tables.pdf
//      2.  uops.info - https://uops.info/table.html
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
                        ssize_t dsp = emitGetInsAmdAny(id);

                        if ((dsp != 0) || baseRegisterRequiresDisplacement(baseReg))
                        {
                            // three components
                            //
                            // - throughput is only 1 per cycle
                            //
                            result.insThroughput = PERFSCORE_THROUGHPUT_1C;

                            if (baseRegisterRequiresDisplacement(baseReg) || id->idIsDspReloc())
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

        case INS_imul_AX:
        case INS_imul_BX:
        case INS_imul_CX:
        case INS_imul_DX:
        case INS_imul_BP:
        case INS_imul_SI:
        case INS_imul_DI:
#ifdef TARGET_AMD64
        case INS_imul_08:
        case INS_imul_09:
        case INS_imul_10:
        case INS_imul_11:
        case INS_imul_12:
        case INS_imul_13:
        case INS_imul_14:
        case INS_imul_15:
#endif // TARGET_AMD64
        case INS_imul:
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
                case IF_RRW_SHF:
                    // ins   reg, cns
                    result.insThroughput = PERFSCORE_THROUGHPUT_2X;
                    break;

                case IF_MRW_SHF:
                case IF_SRW_SHF:
                case IF_ARW_SHF:
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
            if (insFmt == IF_RRW_RRW_CNS)
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

#ifdef TARGET_AMD64
        case INS_movsq:
        case INS_stosq:
#endif // TARGET_AMD64
        case INS_movsd:
        case INS_stosd:
            // uops.info
            result.insThroughput = PERFSCORE_THROUGHPUT_1C;
            break;

#ifdef TARGET_AMD64
        case INS_r_movsq:
        case INS_r_stosq:
#endif // TARGET_AMD64
        case INS_r_movsd:
        case INS_r_movsb:
        case INS_r_stosd:
        case INS_r_stosb:
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
        case INS_movsdsse2:
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
            // unhandled instruction insFmt combination
            perfScoreUnhandledInstruction(id, &result);
            break;
    }

    return result;
}

#endif // defined(DEBUG) || defined(LATE_DISASM)
#endif // TARGET_XARCH
