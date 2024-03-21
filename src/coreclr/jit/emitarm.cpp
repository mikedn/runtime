// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_ARM

#include "instr.h"
#include "emit.h"
#include "codegen.h"
#include "unwind.h"

// This typedef defines the type that we use to hold encoded instructions.
using code_t = uint32_t;

static bool isModImmConst(int val32);
static int insUnscaleImm(instruction ins, int imm);

static bool insSetsFlags(insFlags flags)
{
    return flags != INS_FLAGS_NOT_SET;
}

static bool insDoesNotSetFlags(insFlags flags)
{
    return flags != INS_FLAGS_SET;
}

static insFlags insMustSetFlags(insFlags flags)
{
    return (flags == INS_FLAGS_SET) ? INS_FLAGS_SET : INS_FLAGS_NOT_SET;
}

static bool insOptsNone(insOpts opt)
{
    return opt == INS_OPTS_NONE;
}

static bool insOptAnyInc(insOpts opt)
{
    return (opt == INS_OPTS_LDST_PRE_DEC) || (opt == INS_OPTS_LDST_POST_INC);
}

static bool insOptsPostInc(insOpts opt)
{
    return opt == INS_OPTS_LDST_POST_INC;
}

static bool insOptsRRX(insOpts opt)
{
    return opt == INS_OPTS_RRX;
}

instruction emitter::emitJumpKindToBranch(emitJumpKind kind)
{
    static const instruction map[]{
        INS_nop, INS_b,
#define CC_DEF(cc, rev, ...) INS_b##cc,
#include "emitjmps.h"
    };

    assert(kind < _countof(map));
    return map[kind];
}

emitJumpKind EmitterBase::emitReverseJumpKind(emitJumpKind kind)
{
    static const emitJumpKind map[]{
        EJ_NONE, EJ_jmp,
#define CC_DEF(cc, rev, ...) EJ_##rev,
#include "emitjmps.h"
    };

    assert(kind < _countof(map));
    return map[kind];
}

static emitJumpKind BranchToJumpKind(instruction ins)
{
    assert((INS_b <= ins) && (ins <= INS_ble));
#define CC_DEF(cc, rev, ...) static_assert_no_msg(INS_b##cc - INS_b == EJ_##cc - EJ_jmp);
#include "emitjmps.h"

    return static_cast<emitJumpKind>(EJ_jmp + (ins - INS_b));
}

enum ID_OPS : uint8_t
{
    ID_OP_NONE, // no additional arguments
    ID_OP_SCNS, // small const  operand (21-bits or less, no reloc)
    ID_OP_JMP,  // local jump
    ID_OP_CALL, // direct method call
    ID_OP_SPEC, // special handling required
};

static ID_OPS GetFormatOp(insFormat format)
{
    static const ID_OPS ops[]{
#define IF_DEF(en, op1, op2) ID_OP_##op2,
#include "emitfmtsarm.h"
    };

    assert(format < _countof(ops));
    return ops[format];
}

// Return the allocated size (in bytes) of the given instruction descriptor.
size_t emitter::instrDescSmall::GetDescSize() const
{
    if (_idSmallDsc)
    {
        return sizeof(instrDescSmall);
    }

    ID_OPS idOp = GetFormatOp(_idInsFmt);

    bool isCallIns    = (_idIns == INS_bl) || (_idIns == INS_blx);
    bool maybeCallIns = (_idIns == INS_b) || (_idIns == INS_bx);

    // An INS_call instruction may use a "fat" direct/indirect call descriptor
    // except for a local call to a label (i.e. call to a finally).
    // Only ID_OP_CALL and ID_OP_SPEC check for this, so we enforce that the
    // INS_call instruction always uses one of these idOps.

    assert(!isCallIns ||         // either not a call or
           idOp == ID_OP_CALL || // is a direct call
           idOp == ID_OP_SPEC || // is an indirect call
           idOp == ID_OP_JMP);   // is a local call to finally clause

    switch (idOp)
    {
        case ID_OP_NONE:
            break;

        case ID_OP_JMP:
            return sizeof(instrDescJmp);

        case ID_OP_CALL:
        case ID_OP_SPEC:
            assert(isCallIns || maybeCallIns);
            if (_idLargeCall)
            {
                // Must be a "fat" indirect call descriptor
                return sizeof(instrDescCGCA);
            }
            else
            {
                assert(!_idLargeCns);
                return sizeof(instrDesc);
            }
            break;

        default:
            NO_WAY("unexpected instruction descriptor format");
            break;
    }

    return _idLargeCns ? sizeof(instrDescCns) : sizeof(instrDesc);
}

size_t emitter::instrDesc::emitGetInstrDescSize() const
{
    if (_idSmallDsc)
    {
        return sizeof(instrDescSmall);
    }

    if (_idLargeCns)
    {
        return sizeof(instrDescCns);
    }

    return sizeof(instrDesc);
}

int32_t emitter::instrDesc::emitGetInsSC() const
{
    return _idLargeCns ? static_cast<const instrDescCns*>(this)->idcCnsVal : _idSmallCns;
}

#ifdef DEBUG
static bool offsetFitsInVectorMem(int disp)
{
    unsigned imm = unsigned_abs(disp);
    return ((imm & 0x03fc) == imm);
}

static bool isGeneralRegister(RegNum reg)
{
    return IsGeneralRegister(reg);
}

static bool isFloatReg(RegNum reg)
{
    return IsFloatReg(reg);
}

static bool isDoubleReg(RegNum reg)
{
    return isFloatReg(reg) && ((reg % 2) == 0);
}

static bool insOptsLSL(insOpts opt)
{
    return opt == INS_OPTS_LSL;
}

static bool insOptAnyShift(insOpts opt)
{
    return (opt >= INS_OPTS_RRX) && (opt <= INS_OPTS_ROR);
}

void EmitterBase::emitInsSanityCheck(instrDesc* id)
{
    switch (id->idInsFmt())
    {
        case IF_T1_A: // T1_A    ................
        case IF_T2_A: // T2_A    ................ ................
            break;

        case IF_T1_B: // T1_B    ........cccc....                                           cond
        case IF_T2_B: // T2_B    ................ ............iiii                          imm4
            assert(id->emitGetInsSC() < 0x10);
            break;

        case IF_T1_C: // T1_C    .....iiiiinnnddd                       R1  R2              imm5
            assert(isLowRegister(id->idReg1()));
            assert(isLowRegister(id->idReg2()));
            assert(insUnscaleImm(id->idIns(), id->emitGetInsSC()) < 0x20);
            break;

        case IF_T1_D0: // T1_D0   ........Dmmmmddd                       R1* R2*
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            break;

        case IF_T1_D1: // T1_D1   .........mmmm...                       R1*
            assert(isGeneralRegister(id->idReg1()));
            break;

        case IF_T1_D2: // T1_D2   .........mmmm...                               R3*
            assert(isGeneralRegister(id->idReg3()));
            break;

        case IF_T1_E: // T1_E    ..........nnnddd                       R1  R2
            assert(isLowRegister(id->idReg1()));
            assert(isLowRegister(id->idReg2()));
            assert(id->idSmallCns() < 0x20);
            break;

        case IF_T1_F: // T1_F    .........iiiiiii                       SP                  imm7
            assert(id->idReg1() == REG_SP);
            assert(id->idOpSize() == EA_4BYTE);
            assert((id->emitGetInsSC() & ~0x1FC) == 0);
            break;

        case IF_T1_G: // T1_G    .......iiinnnddd                       R1  R2              imm3
            assert(isLowRegister(id->idReg1()));
            assert(isLowRegister(id->idReg2()));
            assert(id->idSmallCns() < 0x8);
            break;

        case IF_T1_H: // T1_H    .......mmmnnnddd                       R1  R2  R3
            assert(isLowRegister(id->idReg1()));
            assert(isLowRegister(id->idReg2()));
            assert(isLowRegister(id->idReg3()));
            break;

        case IF_T1_I: // T1_I    ......i.iiiiiddd                       R1                  imm6
            assert(isLowRegister(id->idReg1()));
            break;

        case IF_T1_J0: // T1_J0   .....dddiiiiiiii                       R1                  imm8
            assert(isLowRegister(id->idReg1()));
            assert(id->emitGetInsSC() < 0x100);
            break;

        case IF_T1_J1: // T1_J1   .....dddiiiiiiii                       R1                  <regmask8>
            assert(isLowRegister(id->idReg1()));
            assert(id->emitGetInsSC() < 0x100);
            break;

        case IF_T1_J2: // T1_J2   .....dddiiiiiiii                       R1  SP              imm8
            assert(isLowRegister(id->idReg1()));
            assert(id->idReg2() == REG_SP);
            assert(id->idOpSize() == EA_4BYTE);
            assert((id->emitGetInsSC() & ~0x3FC) == 0);
            break;

        case IF_T1_L0: // T1_L0   ........iiiiiiii                                           imm8
            assert(id->emitGetInsSC() < 0x100);
            break;

        case IF_T1_L1: // T1_L1   .......Rrrrrrrrr                                           <regmask8+2>
            assert(id->emitGetInsSC() < 0x400);
            break;

        case IF_T2_C0: // T2_C0   ...........Snnnn .iiiddddiishmmmm       R1  R2  R3      S, imm5, sh
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(isGeneralRegister(id->idReg3()));
            assert(id->emitGetInsSC() < 0x20);
            break;

        case IF_T2_C4: // T2_C4   ...........Snnnn ....dddd....mmmm       R1  R2  R3      S
        case IF_T2_C5: // T2_C5   ............nnnn ....dddd....mmmm       R1  R2  R3
        case IF_T2_G1: // T2_G1   ............nnnn ttttTTTT........       R1  R2  R3
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(isGeneralRegister(id->idReg3()));
            break;

        case IF_T2_C1: // T2_C1   ...........S.... .iiiddddiishmmmm       R1  R2          S, imm5, sh
        case IF_T2_C2: // T2_C2   ...........S.... .iiiddddii..mmmm       R1  R2          S, imm5
        case IF_T2_C8: // T2_C8   ............nnnn .iii....iishmmmm       R1  R2             imm5, sh
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(id->emitGetInsSC() < 0x20);
            break;

        case IF_T2_C6: // T2_C6   ................ ....dddd..iimmmm       R1  R2                   imm2
        case IF_T2_C7: // T2_C7   ............nnnn ..........shmmmm       R1  R2                   imm2
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(id->emitGetInsSC() < 0x4);
            break;

        case IF_T2_C3:  // T2_C3   ...........S.... ....dddd....mmmm       R1  R2          S
        case IF_T2_C9:  // T2_C9   ............nnnn ............mmmm       R1  R2
        case IF_T2_C10: // T2_C10  ............mmmm ....dddd....mmmm       R1  R2
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            break;

        case IF_T2_D0: // T2_D0   ............nnnn .iiiddddii.wwwww       R1  R2             imm5, imm5
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(id->emitGetInsSC() < 0x400);
            break;

        case IF_T2_D1: // T2_D1   ................ .iiiddddii.wwwww       R1                 imm5, imm5
            assert(isGeneralRegister(id->idReg1()));
            assert(id->emitGetInsSC() < 0x400);
            break;

        case IF_T2_E0: // T2_E0   ............nnnn tttt......shmmmm       R1  R2  R3               imm2
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            if (id->idIsLclVar())
            {
                assert(isGeneralRegister(codeGen->rsGetRsvdReg()));
            }
            else
            {
                assert(isGeneralRegister(id->idReg3()));
                assert(id->emitGetInsSC() < 0x4);
            }
            break;

        case IF_T2_E1: // T2_E1   ............nnnn tttt............       R1  R2
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            break;

        case IF_T2_E2: // T2_E2   ................ tttt............       R1
            assert(isGeneralRegister(id->idReg1()));
            break;

        case IF_T2_F1: // T2_F1    ............nnnn ttttdddd....mmmm       R1  R2  R3  R4
        case IF_T2_F2: // T2_F2    ............nnnn aaaadddd....mmmm       R1  R2  R3  R4
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(isGeneralRegister(id->idReg3()));
            assert(isGeneralRegister(id->idReg4()));
            break;

        case IF_T2_G0: // T2_G0   .......PU.W.nnnn ttttTTTTiiiiiiii       R1  R2  R3         imm8, PUW
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(isGeneralRegister(id->idReg3()));
            assert(unsigned_abs(id->emitGetInsSC()) < 0x100);
            break;

        case IF_T2_H0: // T2_H0   ............nnnn tttt.PUWiiiiiiii       R1  R2             imm8, PUW
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(unsigned_abs(id->emitGetInsSC()) < 0x100);
            break;

        case IF_T2_H1: // T2_H1   ............nnnn tttt....iiiiiiii       R1  R2             imm8
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(id->emitGetInsSC() < 0x100);
            break;

        case IF_T2_H2: // T2_H2   ............nnnn ........iiiiiiii       R1                 imm8
            assert(isGeneralRegister(id->idReg1()));
            assert(id->emitGetInsSC() < 0x100);
            break;

        case IF_T2_I0: // T2_I0   ..........W.nnnn rrrrrrrrrrrrrrrr       R1              W, imm16
            assert(isGeneralRegister(id->idReg1()));
            assert(id->emitGetInsSC() < 0x10000);
            break;

        case IF_T2_N: // T2_N    .....i......iiii .iiiddddiiiiiiii       R1                 imm16
            assert(isGeneralRegister(id->idReg1()));
            assert(!id->idIsCnsReloc());
            break;

        case IF_T2_N2: // T2_N2   .....i......iiii .iiiddddiiiiiiii       R1                 imm16
            assert(isGeneralRegister(id->idReg1()));
            assert((size_t)id->emitGetInsSC() < roData.size);
            break;

        case IF_T2_N3: // T2_N3   .....i......iiii .iiiddddiiiiiiii       R1                 imm16
            assert(isGeneralRegister(id->idReg1()));
            assert(id->idIsCnsReloc());
            break;

        case IF_T2_I1: // T2_I1   ................ rrrrrrrrrrrrrrrr                          imm16
            assert(id->emitGetInsSC() < 0x10000);
            break;

        case IF_T2_K1: // T2_K1   ............nnnn ttttiiiiiiiiiiii       R1  R2             imm12
        case IF_T2_M0: // T2_M0   .....i......nnnn .iiiddddiiiiiiii       R1  R2             imm12
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(id->emitGetInsSC() < 0x1000);
            break;

        case IF_T2_L0: // T2_L0   .....i.....Snnnn .iiiddddiiiiiiii       R1  R2          S, imm8<<imm4
            assert(isGeneralRegister(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(isModImmConst(id->emitGetInsSC()));
            break;

        case IF_T2_K4: // T2_K4   ........U....... ttttiiiiiiiiiiii       R1  PC          U, imm12
        case IF_T2_M1: // T2_M1   .....i.......... .iiiddddiiiiiiii       R1  PC             imm12
            assert(isGeneralRegister(id->idReg1()));
            assert(id->idReg2() == REG_PC);
            assert(id->emitGetInsSC() < 0x1000);
            break;

        case IF_T2_K3: // T2_K3   ........U....... ....iiiiiiiiiiii       PC              U, imm12
            assert(id->idReg1() == REG_PC);
            assert(id->emitGetInsSC() < 0x1000);
            break;

        case IF_T2_K2: // T2_K2   ............nnnn ....iiiiiiiiiiii       R1                 imm12
            assert(isGeneralRegister(id->idReg1()));
            assert(id->emitGetInsSC() < 0x1000);
            break;

        case IF_T2_L1: // T2_L1   .....i.....S.... .iiiddddiiiiiiii       R1              S, imm8<<imm4
        case IF_T2_L2: // T2_L2   .....i......nnnn .iii....iiiiiiii       R1                 imm8<<imm4
            assert(isGeneralRegister(id->idReg1()));
            assert(isModImmConst(id->emitGetInsSC()));
            break;

        case IF_T1_J3: // T1_J3   .....dddiiiiiiii                        R1  PC             imm8
            assert(isGeneralRegister(id->idReg1()));
            assert(id->idReg2() == REG_PC);
            assert(id->emitGetInsSC() < 0x100);
            break;

        case IF_T1_K:  // T1_K    ....cccciiiiiiii                        Branch             imm8, cond4
        case IF_T1_M:  // T1_M    .....iiiiiiiiiii                        Branch             imm11
        case IF_T2_J1: // T2_J1   .....Scccciiiiii ..j.jiiiiiiiiiii       Branch             imm20, cond4
        case IF_T2_J2: // T2_J2   .....Siiiiiiiiii ..j.jiiiiiiiiii.       Branch             imm24
        case IF_T2_N1: // T2_N    .....i......iiii .iiiddddiiiiiiii       R1                 imm16
        case IF_T2_J3: // T2_J3   .....Siiiiiiiiii ..j.jiiiiiiiiii.       Call               imm24
        case IF_LARGEJMP:
            break;

        case IF_T2_VFP3:
            if (id->idOpSize() == EA_8BYTE)
            {
                assert(isDoubleReg(id->idReg1()));
                assert(isDoubleReg(id->idReg2()));
                assert(isDoubleReg(id->idReg3()));
            }
            else
            {
                assert(id->idOpSize() == EA_4BYTE);
                assert(isFloatReg(id->idReg1()));
                assert(isFloatReg(id->idReg2()));
                assert(isFloatReg(id->idReg3()));
            }
            break;

        case IF_T2_VFP2:
            assert(isFloatReg(id->idReg1()));
            assert(isFloatReg(id->idReg2()));
            break;

        case IF_T2_VLDST:
            if (id->idOpSize() == EA_8BYTE)
                assert(isDoubleReg(id->idReg1()));
            else
                assert(isFloatReg(id->idReg1()));
            assert(isGeneralRegister(id->idReg2()));
            assert(offsetFitsInVectorMem(id->emitGetInsSC()));
            break;

        case IF_T2_VMOVD:
            assert(id->idOpSize() == EA_8BYTE);
            if (id->idIns() == INS_vmov_d2i)
            {
                assert(isGeneralRegister(id->idReg1()));
                assert(isGeneralRegister(id->idReg2()));
                assert(isDoubleReg(id->idReg3()));
            }
            else
            {
                assert(id->idIns() == INS_vmov_i2d);
                assert(isDoubleReg(id->idReg1()));
                assert(isGeneralRegister(id->idReg2()));
                assert(isGeneralRegister(id->idReg3()));
            }
            break;

        case IF_T2_VMOVS:
            assert(id->idOpSize() == EA_4BYTE);
            if (id->idIns() == INS_vmov_i2f)
            {
                assert(isFloatReg(id->idReg1()));
                assert(isGeneralRegister(id->idReg2()));
            }
            else
            {
                assert(id->idIns() == INS_vmov_f2i);
                assert(isGeneralRegister(id->idReg1()));
                assert(isFloatReg(id->idReg2()));
            }
            break;

        default:
            printf("unexpected format %s\n", emitIfName(id->idInsFmt()));
            assert(!"Unexpected format");
            break;
    }
}
#endif // DEBUG

class ArmEncoder : public Emitter::Encoder<emitter>
{
public:
    ArmEncoder(Emitter* emit) : Encoder(emit)
    {
    }

    size_t emitOutputInstr(insGroup* ig, instrDesc* id, uint8_t** dp) override;

private:
    static int encodeModImmConst(int imm);
    code_t emitInsCode(instruction ins, insFormat fmt);

#ifdef FEATURE_ITINSTRUCTION
    uint8_t* emitOutputIT(uint8_t* dst, instruction ins, insFormat fmt, code_t condcode);
#endif
    uint8_t* emitOutputLJ(uint8_t* dst, instrDescJmp* id, insGroup* ig);
    uint8_t* emitOutputRL(uint8_t* dst, instrDescJmp* id);
    uint8_t* emitOutputShortBranch(uint8_t* dst, instruction ins, insFormat fmt, ssize_t distVal, instrDesc* id);

    unsigned emitOutput_Thumb1Instr(uint8_t* dst, uint32_t code);
    unsigned emitOutput_Thumb2Instr(uint8_t* dst, uint32_t code);

    void emitHandlePCRelativeMov32(void* location, void* target);

    // Returns true if the instruction may write to more than one register.
    bool emitInsMayWriteMultipleRegs(instrDesc* id);

    // Returns true if instruction "id->idIns()" writes to a register that might be used to contain a GC
    // pointer. This exempts the SP and PC registers, and floating point registers. Memory access
    // instructions that pre- or post-increment their memory address registers are *not* considered to write
    // to GC registers, even if that memory address is a by-ref: such an instruction cannot change the GC
    // status of that register, since it must be a byref before and remains one after.
    //
    // This may return false positives.
    bool emitInsMayWriteToGCReg(instrDesc* id);
};

bool ArmEncoder::emitInsMayWriteToGCReg(instrDesc* id)
{
    instruction ins = id->idIns();
    insFormat   fmt = id->idInsFmt();

    switch (fmt)
    {
        // These are the formats with "destination" or "target" registers:
        case IF_T1_C:
        case IF_T1_D0:
        case IF_T1_E:
        case IF_T1_G:
        case IF_T1_H:
        case IF_T1_J0:
        case IF_T1_J1:
        case IF_T1_J2:
        case IF_T1_J3:
        case IF_T2_C0:
        case IF_T2_C1:
        case IF_T2_C2:
        case IF_T2_C3:
        case IF_T2_C4:
        case IF_T2_C5:
        case IF_T2_C6:
        case IF_T2_C10:
        case IF_T2_D0:
        case IF_T2_D1:
        case IF_T2_F1:
        case IF_T2_F2:
        case IF_T2_L0:
        case IF_T2_L1:
        case IF_T2_M0:
        case IF_T2_M1:
        case IF_T2_N:
        case IF_T2_N1:
        case IF_T2_N2:
        case IF_T2_N3:
        case IF_T2_VFP3:
        case IF_T2_VFP2:
        case IF_T2_VLDST:
        case IF_T2_E0:
        case IF_T2_E1:
        case IF_T2_E2:
        case IF_T2_G0:
        case IF_T2_G1:
        case IF_T2_H0:
        case IF_T2_H1:
        case IF_T2_K1:
        case IF_T2_K4:
            // Some formats with "destination" or "target" registers are actually used for store instructions, for the
            // "source" value written to memory.
            // Similarly, PUSH has a target register, indicating the start of the set of registers to push.  POP
            // *does* write to at least one register, so we do not make that a special case.
            // Various compare/test instructions do not write (except to the flags). Technically "teq" does not need to
            // be in this list because it has no forms matched above, but I'm putting it here for completeness.
            switch (ins)
            {
                case INS_str:
                case INS_strb:
                case INS_strh:
                case INS_strd:
                case INS_strex:
                case INS_strexb:
                case INS_strexd:
                case INS_strexh:
                case INS_push:
                case INS_cmp:
                case INS_cmn:
                case INS_tst:
                case INS_teq:
                    return false;
                default:
                    return true;
            }
        case IF_T2_VMOVS:
            // VMOV.i2f reads from the integer register. Conversely VMOV.f2i writes to GC pointer-sized
            // integer register that might have previously held GC pointers, so they need to be included.
            assert(id->idGCref() == GCT_NONE);
            return (ins == INS_vmov_f2i);

        case IF_T2_VMOVD:
            // VMOV.i2d reads from the integer registers. Conversely VMOV.d2i writes to GC pointer-sized
            // integer registers that might have previously held GC pointers, so they need to be included.
            assert(id->idGCref() == GCT_NONE);
            return (ins == INS_vmov_d2i);

        default:
            assert(fmt != IF_GC_REG);
            return false;
    }
}

bool ArmEncoder::emitInsMayWriteMultipleRegs(instrDesc* id)
{
    instruction ins = id->idIns();
    insFormat   fmt = id->idInsFmt();

    switch (ins)
    {
        case INS_ldm:
        case INS_ldmdb:
        case INS_smlal:
        case INS_smull:
        case INS_umlal:
        case INS_umull:
        case INS_vmov_d2i:
            return true;
        case INS_pop:
            // T2_E2 is pop single register encoding
            return fmt != IF_T2_E2;
        default:
            return false;
    }
}

const char* insName(instruction ins)
{
    // clang-format off
    static const char* const insNames[]
    {
#define INST1(id, nm, fp, ldst, fmt, e1                                 ) nm,
#define INST2(id, nm, fp, ldst, fmt, e1, e2                             ) nm,
#define INST3(id, nm, fp, ldst, fmt, e1, e2, e3                         ) nm,
#define INST4(id, nm, fp, ldst, fmt, e1, e2, e3, e4                     ) nm,
#define INST5(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5                 ) nm,
#define INST6(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6             ) nm,
#define INST8(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8     ) nm,
#define INST9(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9 ) nm,
#include "instrsarm.h"
    };
    // clang-format on

    assert(ins < _countof(insNames));
    assert(insNames[ins] != nullptr);

    return insNames[ins];
}

enum
{
    IF_EN9 = IF_COUNT + 1,
    IF_EN8,
    IF_EN6A,
    IF_EN6B,
    IF_EN5A,
    IF_EN5B,
    IF_EN4A,
    IF_EN4B,
    IF_EN4C,
    IF_EN3A,
    IF_EN3B,
    IF_EN3C,
    IF_EN3D,
    IF_EN3E,
    IF_EN2A,
    IF_EN2B,
    IF_EN2C,
    IF_EN2D,
    IF_EN2E,
    IF_EN2F,
    IF_EN2G,
    IF_ENCOUNT
};

static uint8_t emitInsFormat(instruction ins)
{
    static_assert_no_msg(IF_ENCOUNT <= UINT8_MAX);

    const static uint8_t formats[]{
#define INST1(id, nm, fp, ldst, fmt, ...) fmt,
#define INST2(id, nm, fp, ldst, fmt, ...) fmt,
#define INST3(id, nm, fp, ldst, fmt, ...) fmt,
#define INST4(id, nm, fp, ldst, fmt, ...) fmt,
#define INST5(id, nm, fp, ldst, fmt, ...) fmt,
#define INST6(id, nm, fp, ldst, fmt, ...) fmt,
#define INST8(id, nm, fp, ldst, fmt, ...) fmt,
#define INST9(id, nm, fp, ldst, fmt, ...) fmt,
#include "instrsarm.h"
    };

    assert(ins < _countof(formats));
    assert(formats[ins] != IF_NONE);

    return formats[ins];
}

#define INST_FP 1
#define LD 2
#define ST 4
#define CMP 8

const uint8_t instInfo[]{
#define INST1(id, nm, fp, ldst, ...) ldst | INST_FP *fp,
#define INST2(id, nm, fp, ldst, ...) ldst | INST_FP *fp,
#define INST3(id, nm, fp, ldst, ...) ldst | INST_FP *fp,
#define INST4(id, nm, fp, ldst, ...) ldst | INST_FP *fp,
#define INST5(id, nm, fp, ldst, ...) ldst | INST_FP *fp,
#define INST6(id, nm, fp, ldst, ...) ldst | INST_FP *fp,
#define INST8(id, nm, fp, ldst, ...) ldst | INST_FP *fp,
#define INST9(id, nm, fp, ldst, ...) ldst | INST_FP *fp,
#include "instrsarm.h"
};

static bool instIsFP(instruction ins)
{
    assert(ins < _countof(instInfo));
    return (instInfo[ins] & INST_FP) != 0;
}

bool emitter::emitInsIsLoad(instruction ins)
{
    // We have pseudo ins like lea which are not included in emitInsLdStTab.
    return (ins < _countof(instInfo)) && ((instInfo[ins] & LD) != 0);
}

static bool emitInsIsLoadOrStore(instruction ins)
{
    // We have pseudo ins like lea which are not included in emitInsLdStTab.
    return (ins < _countof(instInfo)) && ((instInfo[ins] & (LD | ST)) != 0);
}

#undef LD
#undef ST
#undef CMP

// Returns the specific encoding of the given CPU instruction and format
code_t ArmEncoder::emitInsCode(instruction ins, insFormat fmt)
{
    // clang-format off
    const static code_t insCodes1[]
    {
        #define INST1(id, nm, fp, ldst, fmt, e1                                ) e1,
        #define INST2(id, nm, fp, ldst, fmt, e1, e2                            ) e1,
        #define INST3(id, nm, fp, ldst, fmt, e1, e2, e3                        ) e1,
        #define INST4(id, nm, fp, ldst, fmt, e1, e2, e3, e4                    ) e1,
        #define INST5(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5                ) e1,
        #define INST6(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6            ) e1,
        #define INST8(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8    ) e1,
        #define INST9(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e1,
        #include "instrsarm.h"
    };
    const static code_t insCodes2[]
    {
        #define INST1(id, nm, fp, ldst, fmt, e1                                )
        #define INST2(id, nm, fp, ldst, fmt, e1, e2                            ) e2,
        #define INST3(id, nm, fp, ldst, fmt, e1, e2, e3                        ) e2,
        #define INST4(id, nm, fp, ldst, fmt, e1, e2, e3, e4                    ) e2,
        #define INST5(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5                ) e2,
        #define INST6(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6            ) e2,
        #define INST8(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8    ) e2,
        #define INST9(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e2,
        #include "instrsarm.h"
    };
    const static code_t insCodes3[]
    {
        #define INST1(id, nm, fp, ldst, fmt, e1                                )
        #define INST2(id, nm, fp, ldst, fmt, e1, e2                            )
        #define INST3(id, nm, fp, ldst, fmt, e1, e2, e3                        ) e3,
        #define INST4(id, nm, fp, ldst, fmt, e1, e2, e3, e4                    ) e3,
        #define INST5(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5                ) e3,
        #define INST6(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6            ) e3,
        #define INST8(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8    ) e3,
        #define INST9(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e3,
        #include "instrsarm.h"
    };
    const static code_t insCodes4[]
    {
        #define INST1(id, nm, fp, ldst, fmt, e1                                )
        #define INST2(id, nm, fp, ldst, fmt, e1, e2                            )
        #define INST3(id, nm, fp, ldst, fmt, e1, e2, e3                        )
        #define INST4(id, nm, fp, ldst, fmt, e1, e2, e3, e4                    ) e4,
        #define INST5(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5                ) e4,
        #define INST6(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6            ) e4,
        #define INST8(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8    ) e4,
        #define INST9(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e4,
        #include "instrsarm.h"
    };
    const static code_t insCodes5[]
    {
        #define INST1(id, nm, fp, ldst, fmt, e1                                )
        #define INST2(id, nm, fp, ldst, fmt, e1, e2                            )
        #define INST3(id, nm, fp, ldst, fmt, e1, e2, e3                        )
        #define INST4(id, nm, fp, ldst, fmt, e1, e2, e3, e4                    )
        #define INST5(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5                ) e5,
        #define INST6(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6            ) e5,
        #define INST8(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8    ) e5,
        #define INST9(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e5,
        #include "instrsarm.h"
    };
    const static code_t insCodes6[]
    {
        #define INST1(id, nm, fp, ldst, fmt, e1                                )
        #define INST2(id, nm, fp, ldst, fmt, e1, e2                            )
        #define INST3(id, nm, fp, ldst, fmt, e1, e2, e3                        )
        #define INST4(id, nm, fp, ldst, fmt, e1, e2, e3, e4                    )
        #define INST5(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5                )
        #define INST6(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6            ) e6,
        #define INST8(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8    ) e6,
        #define INST9(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e6,
        #include "instrsarm.h"
    };
    const static code_t insCodes7[]
    {
        #define INST1(id, nm, fp, ldst, fmt, e1                                )
        #define INST2(id, nm, fp, ldst, fmt, e1, e2                            )
        #define INST3(id, nm, fp, ldst, fmt, e1, e2, e3                        )
        #define INST4(id, nm, fp, ldst, fmt, e1, e2, e3, e4                    )
        #define INST5(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5                )
        #define INST6(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6            )
        #define INST8(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8    ) e7,
        #define INST9(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e7,
        #include "instrsarm.h"
    };
    const static code_t insCodes8[]
    {
        #define INST1(id, nm, fp, ldst, fmt, e1                                )
        #define INST2(id, nm, fp, ldst, fmt, e1, e2                            )
        #define INST3(id, nm, fp, ldst, fmt, e1, e2, e3                        )
        #define INST4(id, nm, fp, ldst, fmt, e1, e2, e3, e4                    )
        #define INST5(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5                )
        #define INST6(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6            )
        #define INST8(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8    ) e8,
        #define INST9(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e8,
        #include "instrsarm.h"
    };
    const static code_t insCodes9[]
    {
        #define INST1(id, nm, fp, ldst, fmt, e1                                )
        #define INST2(id, nm, fp, ldst, fmt, e1, e2                            )
        #define INST3(id, nm, fp, ldst, fmt, e1, e2, e3                        )
        #define INST4(id, nm, fp, ldst, fmt, e1, e2, e3, e4                    )
        #define INST5(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5                )
        #define INST6(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6            )
        #define INST8(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8    )
        #define INST9(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9) e9,
        #include "instrsarm.h"
    };
    const static insFormat formatEncode9[9]  { IF_T1_D0, IF_T1_H,  IF_T1_J0, IF_T1_G,  IF_T2_L0, IF_T2_C0, IF_T1_F,  IF_T1_J2, IF_T1_J3 };
    const static insFormat formatEncode8[8]  { IF_T1_H,  IF_T1_C,  IF_T2_E0, IF_T2_H0, IF_T2_K1, IF_T2_K4, IF_T1_J2, IF_T1_J3 };
    const static insFormat formatEncode6A[6] { IF_T1_H,  IF_T1_C,  IF_T2_E0, IF_T2_H0, IF_T2_K1, IF_T2_K4};
    const static insFormat formatEncode6B[6] { IF_T1_H,  IF_T1_C,  IF_T2_E0, IF_T2_H0, IF_T2_K1, IF_T1_J2 };
    const static insFormat formatEncode5A[5] { IF_T1_E,  IF_T1_D0, IF_T1_J0, IF_T2_L1, IF_T2_C3 };
    const static insFormat formatEncode5B[5] { IF_T1_E,  IF_T1_D0, IF_T1_J0, IF_T2_L2, IF_T2_C8 };
    const static insFormat formatEncode4A[4] { IF_T1_E,  IF_T1_C,  IF_T2_C4, IF_T2_C2 };
    const static insFormat formatEncode4B[4] { IF_T2_K2, IF_T2_H2, IF_T2_C7, IF_T2_K3 };
    const static insFormat formatEncode4C[4] { IF_T2_N,  IF_T2_N1, IF_T2_N2, IF_T2_N3 };
    const static insFormat formatEncode3A[3] { IF_T1_E,  IF_T2_C0, IF_T2_L0 };
    const static insFormat formatEncode3B[3] { IF_T1_E,  IF_T2_C8, IF_T2_L2 };
    const static insFormat formatEncode3C[3] { IF_T1_E,  IF_T2_C1, IF_T2_L1 };
    const static insFormat formatEncode3D[3] { IF_T1_L1, IF_T2_E2, IF_T2_I1 };
    const static insFormat formatEncode3E[3] { IF_T1_M,  IF_T2_J2, IF_T2_J3 };
    const static insFormat formatEncode2A[2] { IF_T1_K,  IF_T2_J1 };
    const static insFormat formatEncode2B[2] { IF_T1_D1, IF_T1_D2 };
    const static insFormat formatEncode2C[2] { IF_T1_D2, IF_T2_J3 };
    const static insFormat formatEncode2D[2] { IF_T1_J1, IF_T2_I0 };
    const static insFormat formatEncode2E[2] { IF_T1_E,  IF_T2_C6 };
    const static insFormat formatEncode2F[2] { IF_T1_E,  IF_T2_C5 };
    const static insFormat formatEncode2G[2] { IF_T1_J3, IF_T2_M1 };
    // clang-format on

    code_t  code   = BAD_CODE;
    uint8_t insFmt = emitInsFormat(ins);
    bool    found  = false;
    int     index  = 0;

    switch (insFmt)
    {
        case IF_EN9:
            for (index = 0; index < 9; index++)
            {
                if (fmt == formatEncode9[index])
                {
                    found = true;
                    break;
                }
            }
            break;

        case IF_EN8:
            for (index = 0; index < 8; index++)
            {
                if (fmt == formatEncode8[index])
                {
                    found = true;
                    break;
                }
            }
            break;

        case IF_EN6A:
            for (index = 0; index < 6; index++)
            {
                if (fmt == formatEncode6A[index])
                {
                    found = true;
                    break;
                }
            }
            break;

        case IF_EN6B:
            for (index = 0; index < 6; index++)
            {
                if (fmt == formatEncode6B[index])
                {
                    found = true;
                    break;
                }
            }
            break;

        case IF_EN5A:
            for (index = 0; index < 5; index++)
            {
                if (fmt == formatEncode5A[index])
                {
                    found = true;
                    break;
                }
            }
            break;

        case IF_EN5B:
            for (index = 0; index < 5; index++)
            {
                if (fmt == formatEncode5B[index])
                {
                    found = true;
                    break;
                }
            }
            break;

        case IF_EN4A:
            for (index = 0; index < 4; index++)
            {
                if (fmt == formatEncode4A[index])
                {
                    found = true;
                    break;
                }
            }
            break;

        case IF_EN4B:
            for (index = 0; index < 4; index++)
            {
                if (fmt == formatEncode4B[index])
                {
                    found = true;
                    break;
                }
            }
            break;

        case IF_EN4C:
            for (index = 0; index < 4; index++)
            {
                if (fmt == formatEncode4C[index])
                {
                    found = true;
                    break;
                }
            }
            break;

        case IF_EN3A:
            for (index = 0; index < 3; index++)
            {
                if (fmt == formatEncode3A[index])
                {
                    found = true;
                    break;
                }
            }
            break;

        case IF_EN3B:
            for (index = 0; index < 3; index++)
            {
                if (fmt == formatEncode3B[index])
                {
                    found = true;
                    break;
                }
            }
            break;
        case IF_EN3C:
            for (index = 0; index < 3; index++)
            {
                if (fmt == formatEncode3C[index])
                {
                    found = true;
                    break;
                }
            }
            break;
        case IF_EN3D:
            for (index = 0; index < 3; index++)
            {
                if (fmt == formatEncode3D[index])
                {
                    found = true;
                    break;
                }
            }
            break;
        case IF_EN3E:
            for (index = 0; index < 3; index++)
            {
                if (fmt == formatEncode3E[index])
                {
                    found = true;
                    break;
                }
            }
            break;

        case IF_EN2A:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2A[index])
                {
                    found = true;
                    break;
                }
            }
            break;
        case IF_EN2B:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2B[index])
                {
                    found = true;
                    break;
                }
            }
            break;
        case IF_EN2C:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2C[index])
                {
                    found = true;
                    break;
                }
            }
            break;
        case IF_EN2D:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2D[index])
                {
                    found = true;
                    break;
                }
            }
            break;
        case IF_EN2E:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2E[index])
                {
                    found = true;
                    break;
                }
            }
            break;
        case IF_EN2F:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2F[index])
                {
                    found = true;
                    break;
                }
            }
            break;

        case IF_EN2G:
            for (index = 0; index < 2; index++)
            {
                if (fmt == formatEncode2G[index])
                {
                    found = true;
                    break;
                }
            }
            break;

        default:
            index = 0;
            found = true;
            break;
    }

    assert(found);

    switch (index)
    {
        case 0:
            assert(ins < _countof(insCodes1));
            code = insCodes1[ins];
            break;
        case 1:
            assert(ins < _countof(insCodes2));
            code = insCodes2[ins];
            break;
        case 2:
            assert(ins < _countof(insCodes3));
            code = insCodes3[ins];
            break;
        case 3:
            assert(ins < _countof(insCodes4));
            code = insCodes4[ins];
            break;
        case 4:
            assert(ins < _countof(insCodes5));
            code = insCodes5[ins];
            break;
        case 5:
            assert(ins < _countof(insCodes6));
            code = insCodes6[ins];
            break;
        case 6:
            assert(ins < _countof(insCodes7));
            code = insCodes7[ins];
            break;
        case 7:
            assert(ins < _countof(insCodes8));
            code = insCodes8[ins];
            break;
        case 8:
            assert(ins < _countof(insCodes9));
            code = insCodes9[ins];
            break;
    }

    assert((code != BAD_CODE));

    return code;
}

static emitter::insSize emitInsSize(insFormat insFmt)
{
    assert(insFmt < IF_COUNT);

    if (insFmt >= IF_T2_A)
    {
        return emitter::ISZ_32BIT;
    }

    if (insFmt >= IF_T1_A)
    {
        return emitter::ISZ_16BIT;
    }

    if (insFmt == IF_LARGEJMP)
    {
        return emitter::ISZ_48BIT;
    }

    assert(insFmt == IF_GC_REG);

    return emitter::ISZ_NONE;
}

bool emitter::IsMovInstruction(instruction ins)
{
    switch (ins)
    {
        case INS_mov:
        case INS_sxtb:
        case INS_sxth:
        case INS_uxtb:
        case INS_uxth:
        case INS_vmov:
        case INS_vmov_i2f:
        case INS_vmov_f2i:
            return true;
        default:
            return false;
    }
}

// Returns true when immediate 'val32' can be encoded using the special modified immediate constant available in Thumb
static bool isModImmConst(int val32)
{
    unsigned uval32 = (unsigned)val32;
    unsigned imm8   = uval32 & 0xff;

    // encode = 0000x
    if (imm8 == uval32)
        return true;

    unsigned imm32a = (imm8 << 16) | imm8;
    // encode = 0001x
    if (imm32a == uval32)
        return true;

    unsigned imm32b = (imm32a << 8);
    // encode = 0010x
    if (imm32b == uval32)
        return true;

    unsigned imm32c = (imm32a | imm32b);
    // encode = 0011x
    if (imm32c == uval32)
        return true;

    unsigned mask32 = 0x00000ff;

    unsigned encode = 31; // 11111
    unsigned temp;

    do
    {
        mask32 <<= 1;
        temp = uval32 & ~mask32;
        if (temp == 0)
            return true;
        encode--;
    } while (encode >= 8);

    return false;
}

// Returns the special ARM 12-bit immediate encoding that is used to encode the immediate. (4-bits, 8-bits)
// If the imm can not be encoded then 0x0BADC0DE is returned.
int ArmEncoder::encodeModImmConst(int val32)
{
    unsigned uval32 = (unsigned)val32;
    unsigned imm8   = uval32 & 0xff;
    unsigned encode = imm8 >> 7;
    unsigned imm32a;
    unsigned imm32b;
    unsigned imm32c;
    unsigned mask32;
    unsigned temp;

    // encode = 0000x
    if (imm8 == uval32)
    {
        goto DONE;
    }

    imm32a = (imm8 << 16) | imm8;
    // encode = 0001x
    if (imm32a == uval32)
    {
        encode += 2;
        goto DONE;
    }

    imm32b = (imm32a << 8);
    // encode = 0010x
    if (imm32b == uval32)
    {
        encode += 4;
        goto DONE;
    }

    imm32c = (imm32a | imm32b);
    // encode = 0011x
    if (imm32c == uval32)
    {
        encode += 6;
        goto DONE;
    }

    mask32 = 0x00000ff;

    encode = 31; // 11111
    do
    {
        mask32 <<= 1;
        temp = uval32 & ~mask32;
        if (temp == 0)
        {
            imm8 = (uval32 & mask32) >> (32 - encode);
            assert((imm8 & 0x80) != 0);
            goto DONE;
        }
        encode--;
    } while (encode >= 8);

    assert(!"encodeModImmConst failed!");
    return BAD_CODE;

DONE:
    unsigned result = (encode << 7) | (imm8 & 0x7f);
    assert(result <= 0x0fff);
    assert(result >= 0);
    return (int)result;
}

static bool validDispForLdSt(int32_t disp, var_types type)
{
    return varTypeIsFloating(type) ? ((disp & 0x3FC) == disp) : ((disp >= -0x00ff) && (disp <= 0x0fff));
}

bool emitter::validImmForInstr(instruction ins, int32_t imm, insFlags flags)
{
    if (emitInsIsLoadOrStore(ins) && !instIsFP(ins))
    {
        return validDispForLdSt(imm, TYP_INT);
    }

    switch (ins)
    {
        case INS_cmp:
        case INS_cmn:
            return emitIns_valid_imm_for_alu(imm) || emitIns_valid_imm_for_alu(-imm);
        case INS_and:
        case INS_bic:
        case INS_orr:
        case INS_orn:
        case INS_mvn:
            return emitIns_valid_imm_for_alu(imm) || emitIns_valid_imm_for_alu(~imm);
        case INS_mov:
            return emitIns_valid_imm_for_mov(imm);
        case INS_addw:
        case INS_subw:
            return (unsigned_abs(imm) <= 0x00000fff) && (flags != INS_FLAGS_SET); // 12-bit immediate
        case INS_add:
        case INS_sub:
            return emitIns_valid_imm_for_add(imm, flags);
        case INS_tst:
        case INS_eor:
        case INS_teq:
        case INS_adc:
        case INS_sbc:
        case INS_rsb:
            return emitIns_valid_imm_for_alu(imm);
        case INS_asr:
        case INS_lsl:
        case INS_lsr:
        case INS_ror:
            return (imm > 0) && (imm <= 32);
        case INS_vstr:
        case INS_vldr:
            return (imm & 0x3FC) == imm;
        default:
            return false;
    }
}

bool emitter::validImmForBL(ssize_t addr, Compiler* compiler)
{
    if (!compiler->info.compMatchedVM)
    {
        // If we are running the altjit for NGEN, then assume we can use the "BL" instruction.
        // This matches the usual behavior for NGEN, since we normally do generate "BL".
        return compiler->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT);
    }

    return compiler->eeIsThumbBranch24TargetAddress(reinterpret_cast<void*>(addr));
}

// Returns true when the immediate 'imm' can be encoded using the 12-bit funky Arm immediate encoding.
bool emitter::emitIns_valid_imm_for_alu(int imm)
{
    return isModImmConst(imm);
}

// Returns true when the immediate 'imm' can be encoded using a single mov or mvn instruction.
bool emitter::emitIns_valid_imm_for_mov(int imm)
{
    if ((imm & 0x0000ffff) == imm) // 16-bit immediate
        return true;
    if (isModImmConst(imm)) // funky arm immediate
        return true;
    if (isModImmConst(~imm)) // funky arm immediate via mvn
        return true;
    return false;
}

// Returns true when the immediate 'imm' can be encoded using a single 2-byte mov instruction.
bool emitter::emitIns_valid_imm_for_small_mov(RegNum reg, int imm, insFlags flags)
{
    return isLowRegister(reg) && insSetsFlags(flags) && ((imm & 0x00ff) == imm);
}

// Returns true when the immediate 'imm' can be encoded using a single add or sub instruction.
bool emitter::emitIns_valid_imm_for_add(int imm, insFlags flags)
{
    if ((unsigned_abs(imm) <= 0x00000fff) && (flags != INS_FLAGS_SET)) // 12-bit immediate via add/sub
        return true;
    if (isModImmConst(imm)) // funky arm immediate
        return true;
    if (isModImmConst(-imm)) // funky arm immediate via sub
        return true;
    return false;
}

// Returns true if this 'imm' can be encoded as a input operand to an cmp instruction.
bool emitter::emitIns_valid_imm_for_cmp(int imm, insFlags flags)
{
    if (isModImmConst(imm)) // funky arm immediate
        return true;
    if (isModImmConst(-imm)) // funky arm immediate via sub
        return true;
    return false;
}

// Returns true when the immediate 'imm' can be encoded in "add Rd,SP,i10".
bool emitter::emitIns_valid_imm_for_add_sp(int imm)
{
    if ((imm & 0x03fc) == imm)
        return true;
    return false;
}

// Returns true when the immediate 'imm' can be encoded as the offset in a ldr/str instruction.
bool emitter::emitIns_valid_imm_for_ldst_offset(int imm, emitAttr size)
{
    if ((imm & 0x0fff) == imm)
        return true; // encodable using IF_T2_K1
    if (unsigned_abs(imm) <= 0x0ff)
        return true; // encodable using IF_T2_H0
    return false;
}

// Returns true when the immediate 'imm' can be encoded as the offset in a vldr/vstr instruction,
// i.e. when it is a non-negative multiple of 4 that is less than 1024.
bool emitter::emitIns_valid_imm_for_vldst_offset(int imm)
{
    if ((imm & 0x3fc) == imm)
        return true;
    return false;
}

template <typename T>
T* emitter::AllocInstr(bool updateLastIns)
{
    instrDescSmall* id = emitAllocAnyInstr(sizeof(T), updateLastIns);
    memset(id, 0, sizeof(T));
    INDEBUG(id->idDebugOnlyInfo(new (emitComp, CMK_DebugOnly) instrDescDebugInfo(++emitInsCount, sizeof(T))));

    return static_cast<T*>(id);
}

emitter::instrDesc* emitter::emitNewInstr()
{
    return AllocInstr<instrDesc>();
}

emitter::instrDesc* emitter::emitNewInstrSmall()
{
    instrDescSmall* id = AllocInstr<instrDescSmall>();
    id->idSetIsSmallDsc();
    return static_cast<instrDesc*>(id);
}

emitter::instrDesc* emitter::emitNewInstrSC(int32_t cns)
{
    if (!instrDesc::fitsInSmallCns(cns))
    {
        instrDescCns* id = AllocInstr<instrDescCns>();
        id->idSetIsLargeCns();
        id->idcCnsVal = cns;
        return id;
    }

    instrDesc* id = emitNewInstrSmall();
    id->idSmallCns(cns);
    return id;
}

emitter::instrDesc* emitter::emitNewInstrCns(int32_t cns)
{
    if (!instrDesc::fitsInSmallCns(cns))
    {
        instrDescCns* id = AllocInstr<instrDescCns>();
        id->idSetIsLargeCns();
        id->idcCnsVal = cns;
        return id;
    }

    instrDesc* id = emitNewInstr();
    id->idSmallCns(cns);
    return id;
}

emitter::instrDescJmp* emitter::emitNewInstrJmp()
{
    instrDescJmp* id = AllocInstr<instrDescJmp>();
    id->idjIG        = emitCurIG;
    id->idjOffs      = emitCurIGsize;
    id->idjNext      = emitCurIGjmpList;
    emitCurIGjmpList = id;
    return id;
}

emitter::instrDescCGCA* emitter::emitAllocInstrCGCA()
{
    return AllocInstr<instrDescCGCA>();
}

emitter::instrDesc* emitter::emitNewInstrGCReg(emitAttr attr, RegNum reg)
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
    id->idOpSize(EA_4BYTE);
    id->idGCref(EA_GC_TYPE(attr));
    id->idReg1(reg);
    id->idReg2(reg);

    return id;
}

void emitter::emitIns(instruction ins)
{
    insFormat fmt = static_cast<insFormat>(emitInsFormat(ins));

    assert((fmt == IF_T1_A) || (fmt == IF_T2_A));

    instrDesc* id = emitNewInstrSmall();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idOpSize(EA_4BYTE);
    id->idInsSize(emitInsSize(fmt));

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_I(instruction ins, emitAttr attr, int32_t imm)
{
    insFormat fmt         = IF_NONE;
    bool      hasLR       = false;
    bool      hasPC       = false;
    bool      useT2       = false;
    bool      isSingleBit = false;

    switch (ins)
    {
#ifdef FEATURE_ITINSTRUCTION
        case INS_it:
        case INS_itt:
        case INS_ite:
        case INS_ittt:
        case INS_itte:
        case INS_itet:
        case INS_itee:
        case INS_itttt:
        case INS_ittte:
        case INS_ittet:
        case INS_ittee:
        case INS_itett:
        case INS_itete:
        case INS_iteet:
        case INS_iteee:
            assert((imm & 0x0F) == imm);
            fmt  = IF_T1_B;
            attr = EA_4BYTE;
            break;
#endif // FEATURE_ITINSTRUCTION

        case INS_push:
            assert((imm & 0xA000) == 0); // Cannot push PC or SP

            if (imm & 0x4000) // Is the LR being pushed?
                hasLR = true;

            goto COMMON_PUSH_POP;

        case INS_pop:
            assert((imm & 0x2000) == 0);      // Cannot pop SP
            assert((imm & 0xC000) != 0xC000); // Cannot pop both PC and LR

            if (imm & 0x8000) // Is the PC being popped?
                hasPC = true;
            if (imm & 0x4000) // Is the LR being popped?
            {
                hasLR = true;
                useT2 = true;
            }

        COMMON_PUSH_POP:

            if (((imm - 1) & imm) == 0) // Is only one or zero bits set in imm?
            {
                if (imm != 0)
                {
                    isSingleBit = true; // only one bits set in imm
                }
            }

            imm &= ~0xE000; // ensure that PC, LR and SP bits are removed from imm

            if (((imm & 0x00ff) == imm) && !useT2)
            {
                // for push {LR,} <reglist8> and pop  {PC,} <regist8> encoding
                fmt = IF_T1_L1;
            }
            else if (!isSingleBit)
            {
                // for other push and pop multiple registers encoding
                fmt = IF_T2_I1;
            }
            else
            {
                // We have to use the Thumb-2 push/pop single register encoding
                if (hasLR)
                {
                    imm |= 0x4000;
                }
                RegNum reg = genRegNumFromMask(imm);
                emitIns_R(ins, attr, reg);
                return;
            }

            //
            // Encode the PC and LR bits as the lowest two bits
            //
            imm <<= 2;
            if (hasPC)
                imm |= 2;
            if (hasLR)
                imm |= 1;

            assert(imm != 0);

            break;

#if 0
    // TODO-ARM-Cleanup: Enable or delete.
    case INS_bkpt:   // Windows uses a different encoding
        if ((imm & 0x0000ffff) == imm)
        {
            fmt = IF_T1_L0;
        }
        else
        {
            assert(!"Instruction cannot be encoded");
        }
        break;
#endif

        case INS_dmb:
        case INS_ism:
            if ((imm & 0x000f) == imm)
            {
                fmt  = IF_T2_B;
                attr = EA_4BYTE;
            }
            else
            {
                assert(!"Instruction cannot be encoded");
            }
            break;

        default:
            unreached();
    }

    assert((fmt == IF_T1_B) || (fmt == IF_T1_L0) || (fmt == IF_T1_L1) || (fmt == IF_T2_I1) || (fmt == IF_T2_B));

    instrDesc* id = emitNewInstrSC(imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idInsSize(emitInsSize(fmt));

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_R(instruction ins, emitAttr attr, RegNum reg)
{
    insFormat fmt;

    switch (ins)
    {
        case INS_pop:
        case INS_push:
            assert(EA_SIZE(attr) == EA_PTRSIZE);

            if (isLowRegister(reg))
            {
                emitIns_I(ins, attr, 1 << static_cast<int>(reg));
                return;
            }

            fmt = IF_T2_E2;
            break;

        case INS_vmrs:
            assert(EA_SIZE(attr) == EA_PTRSIZE);
            fmt = IF_T2_E2;
            break;

        case INS_bx:
            assert(EA_SIZE(attr) == EA_PTRSIZE);
            fmt = IF_T1_D1;
            break;

        default:
            unreached();
    }

    assert((fmt == IF_T1_D1) || (fmt == IF_T2_E2));

    instrDesc* id = emitNewInstrSmall();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idInsSize(emitInsSize(fmt));
    id->idReg1(reg);

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_R_I(instruction ins, emitAttr attr, RegNum reg, int32_t imm, insFlags flags)
{
    insFormat fmt = IF_NONE;
    insFlags  sf  = INS_FLAGS_DONT_CARE;

    switch (ins)
    {
        case INS_add:
        case INS_sub:
            assert(reg != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            if ((reg == REG_SP) && insDoesNotSetFlags(flags) && ((imm & 0x01fc) == imm))
            {
                fmt = IF_T1_F;
                sf  = INS_FLAGS_NOT_SET;
            }
            else if (isLowRegister(reg) && insSetsFlags(flags) && (unsigned_abs(imm) <= 0x00ff))
            {
                if (imm < 0)
                {
                    assert((ins == INS_add) || (ins == INS_sub));
                    if (ins == INS_add)
                        ins = INS_sub;
                    else // ins == INS_sub
                        ins = INS_add;
                    imm     = -imm;
                }
                fmt = IF_T1_J0;
                sf  = INS_FLAGS_SET;
            }
            else
            {
                // otherwise we have to use a Thumb-2 encoding
                emitIns_R_R_I(ins, attr, reg, reg, imm, flags);
                return;
            }
            break;

        case INS_adc:
            assert(reg != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            emitIns_R_R_I(ins, attr, reg, reg, imm, flags);
            return;

        case INS_vpush:
        case INS_vpop:
            assert(imm > 0);
            if (attr == EA_8BYTE)
            {
                assert(isDoubleReg(reg));
                assert(imm <= 16);
                imm *= 2;
            }
            else
            {
                assert(attr == EA_4BYTE);
                assert(isFloatReg(reg));
                assert(imm <= 16);
            }
            assert(((reg - REG_F0) + imm) <= 32);
            imm *= 4;

            if (ins == INS_vpush)
                imm = -imm;

            sf  = INS_FLAGS_NOT_SET;
            fmt = IF_T2_VLDST;
            break;

        case INS_stm:
        {
            sf = INS_FLAGS_NOT_SET;

            bool hasLR  = false;
            bool hasPC  = false;
            bool useT2  = false;
            bool onlyT1 = false;

            assert((imm & 0x2000) == 0);      // Cannot pop SP
            assert((imm & 0xC000) != 0xC000); // Cannot pop both PC and LR
            assert((imm & 0xFFFF0000) == 0);  // Can only contain lower 16 bits

            if (imm & 0x8000) // Is the PC being popped?
                hasPC = true;

            if (imm & 0x4000) // Is the LR being pushed?
            {
                hasLR = true;
                useT2 = true;
            }

            if (!isLowRegister(reg))
                useT2 = true;

            if (((imm - 1) & imm) == 0) // Is only one or zero bits set in imm?
            {
                if (((imm == 0) && !hasLR) || // imm has no bits set, but hasLR is set
                    (!hasPC && !hasLR))       // imm has one bit set, and neither of hasPC/hasLR are set
                {
                    onlyT1 = true; // if only one bit is set we must use the T1 encoding
                }
            }

            imm &= ~0xE000; // ensure that PC, LR and SP bits are removed from imm

            if (((imm & 0x00ff) == imm) && !useT2)
            {
                fmt = IF_T1_J1;
            }
            else if (!onlyT1)
            {
                fmt = IF_T2_I0;
            }
            else
            {
                assert(!"Instruction cannot be encoded");
                // We have to use the Thumb-2 str single register encoding
                // reg = genRegNumFromMask(imm);
                // emitIns_R(ins, attr, reg);
                return;
            }

            //
            // Encode the PC and LR bits as the lowest two bits
            //
            if (fmt == IF_T2_I0)
            {
                imm <<= 2;
                if (hasPC)
                    imm |= 2;
                if (hasLR)
                    imm |= 1;
            }
            assert(imm != 0);
        }
        break;

        case INS_and:
        case INS_bic:
        case INS_eor:
        case INS_orr:
        case INS_orn:
        case INS_rsb:
        case INS_sbc:

        case INS_ror:
        case INS_asr:
        case INS_lsl:
        case INS_lsr:
            // use the Reg, Reg, Imm encoding
            assert(reg != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            emitIns_R_R_I(ins, attr, reg, reg, imm, flags);
            return;

        case INS_mov:
            if (isLowRegister(reg) && insSetsFlags(flags) && ((imm & 0x00ff) == imm))
            {
                fmt = IF_T1_J0;
                sf  = INS_FLAGS_SET;
            }
            else if (isModImmConst(imm))
            {
                fmt = IF_T2_L1;
                sf  = insMustSetFlags(flags);
            }
            else if (isModImmConst(~imm)) // See if we can use move negated instruction instead
            {
                ins = INS_mvn;
                imm = ~imm;
                fmt = IF_T2_L1;
                sf  = insMustSetFlags(flags);
            }
            else if (insDoesNotSetFlags(flags) && ((imm & 0x0000ffff) == imm))
            {
                // mov => movw instruction
                ins = INS_movw;
                fmt = IF_T2_N;
                sf  = INS_FLAGS_NOT_SET;
            }
            else
            {
                assert(!"Instruction cannot be encoded");
            }
            break;

        case INS_movw:
        case INS_movt:
            assert(insDoesNotSetFlags(flags));

            if ((imm & 0x0000ffff) == imm)
            {
                fmt = IF_T2_N;
                sf  = INS_FLAGS_NOT_SET;
            }
            else
            {
                assert(!"Instruction cannot be encoded");
            }
            break;

        case INS_mvn:
            if (isModImmConst(imm))
            {
                fmt = IF_T2_L1;
                sf  = insMustSetFlags(flags);
            }
            else
            {
                assert(!"Instruction cannot be encoded");
            }
            break;

        case INS_cmp:
            assert(reg != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(insSetsFlags(flags));
            sf = INS_FLAGS_SET;
            if (isLowRegister(reg) && ((imm & 0x0ff) == imm))
            {
                fmt = IF_T1_J0;
            }
            else if (isModImmConst(imm))
            {
                fmt = IF_T2_L2;
            }
            else if (isModImmConst(-imm))
            {
                ins = INS_cmn;
                fmt = IF_T2_L2;
                imm = -imm;
            }
            else
            {
                assert(!"emitIns_R_I: immediate doesn't fit into the instruction");
                return;
            }
            break;

        case INS_cmn:
        case INS_tst:
        case INS_teq:
            assert(reg != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(insSetsFlags(flags));
            sf = INS_FLAGS_SET;
            if (isModImmConst(imm))
            {
                fmt = IF_T2_L2;
            }
            else
            {
                assert(!"Instruction cannot be encoded");
            }
            break;

#ifdef FEATURE_PLI_INSTRUCTION
        case INS_pli:
            assert(insDoesNotSetFlags(flags));
            if ((reg == REG_SP) && (unsigned_abs(imm) <= 0x0fff))
            {
                fmt = IF_T2_K3;
                sf  = INS_FLAGS_NOT_SET;
            }
            FALLTHROUGH;
#endif // FEATURE_PLI_INSTRUCTION

        case INS_pld:
        case INS_pldw:
            assert(insDoesNotSetFlags(flags));
            sf = INS_FLAGS_NOT_SET;
            if ((imm >= 0) && (imm <= 0x0fff))
            {
                fmt = IF_T2_K2;
            }
            else if ((imm < 0) && (-imm <= 0x00ff))
            {
                imm = -imm;
                fmt = IF_T2_H2;
            }
            else
            {
                assert(!"Instruction cannot be encoded");
            }
            break;

        default:
            unreached();
    }
    assert((fmt == IF_T1_F) || (fmt == IF_T1_J0) || (fmt == IF_T1_J1) || (fmt == IF_T2_H2) || (fmt == IF_T2_I0) ||
           (fmt == IF_T2_K2) || (fmt == IF_T2_K3) || (fmt == IF_T2_L1) || (fmt == IF_T2_L2) || (fmt == IF_T2_M1) ||
           (fmt == IF_T2_N) || (fmt == IF_T2_VLDST));

    assert(sf != INS_FLAGS_DONT_CARE);

    instrDesc* id = emitNewInstrSC(imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idInsSize(emitInsSize(fmt));
    id->idInsFlags(sf);
    id->idReg1(reg);

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_MovRelocatableImmediate(instruction ins, RegNum reg, void* addr)
{
    assert((ins == INS_movw) || (ins == INS_movt));

    instrDesc* id = emitNewInstr();
    id->idIns(ins);
    id->idInsFmt(IF_T2_N3);
    id->idInsSize(ISZ_32BIT);
    id->idInsFlags(INS_FLAGS_NOT_SET);
    id->idOpSize(EA_4BYTE);
    id->idReg1(reg);
    id->idSetIsCnsReloc(emitComp->opts.compReloc);
    id->SetAddr(addr);

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_Mov(instruction ins, emitAttr attr, RegNum dstReg, RegNum srcReg, bool canSkip, insFlags flags)
{
    assert(IsMovInstruction(ins));

    emitAttr  size = EA_SIZE(attr);
    insFormat fmt  = IF_NONE;
    insFlags  sf   = INS_FLAGS_DONT_CARE;

    switch (ins)
    {
        case INS_mov:
        {
            if (insDoesNotSetFlags(flags))
            {
                if (EA_IS_GCREF_OR_BYREF(attr) && (dstReg == srcReg))
                {
                    emitNewInstrGCReg(attr, dstReg);
                    return;
                }

                if (canSkip && (dstReg == srcReg))
                {
                    // These instructions have no side effect and can be skipped
                    return;
                }
                fmt = IF_T1_D0;
                sf  = INS_FLAGS_NOT_SET;
            }
            else // insSetsFlags(flags)
            {
                sf = INS_FLAGS_SET;
                if (isLowRegister(dstReg) && isLowRegister(srcReg))
                {
                    fmt = IF_T1_E;
                }
                else
                {
                    fmt = IF_T2_C3;
                }
            }
            break;
        }

        case INS_vmov:
        {
            // VM debugging single stepper doesn't support PC register with this instruction.
            assert(dstReg != REG_PC);
            assert(srcReg != REG_PC);

            if (canSkip && (dstReg == srcReg))
            {
                // These instructions have no side effect and can be skipped
                return;
            }

            if (size == EA_8BYTE)
            {
                assert(isDoubleReg(dstReg));
                assert(isDoubleReg(srcReg));
            }
            else
            {
                assert(isFloatReg(dstReg));
                assert(isFloatReg(srcReg));
            }

            fmt = IF_T2_VFP2;
            sf  = INS_FLAGS_NOT_SET;
            break;
        }

        case INS_vmov_i2f:
        {
            // VM debugging single stepper doesn't support PC register with this instruction.
            assert(srcReg != REG_PC);
            assert(isFloatReg(dstReg));
            assert(isGeneralRegister(srcReg));

            fmt = IF_T2_VMOVS;
            sf  = INS_FLAGS_NOT_SET;
            break;
        }

        case INS_vmov_f2i:
        {
            // VM debugging single stepper doesn't support PC register with this instruction.
            assert(dstReg != REG_PC);
            assert(isGeneralRegister(dstReg));
            assert(isFloatReg(srcReg));

            fmt = IF_T2_VMOVS;
            sf  = INS_FLAGS_NOT_SET;
            break;
        }

        case INS_sxtb:
        case INS_uxtb:
        {
            assert(size == EA_4BYTE);
            goto EXTEND_COMMON;
        }

        case INS_sxth:
        case INS_uxth:
        {
            assert(size == EA_4BYTE);

        EXTEND_COMMON:
            if (canSkip && (dstReg == srcReg))
            {
                // There are scenarios such as in genCallInstruction where the sign/zero extension should be elided
                return;
            }

            // VM debugging single stepper doesn't support PC register with this instruction.
            assert(dstReg != REG_PC);
            assert(srcReg != REG_PC);
            assert(insDoesNotSetFlags(flags));

            if (isLowRegister(dstReg) && isLowRegister(srcReg))
            {
                fmt = IF_T1_E;
                sf  = INS_FLAGS_NOT_SET;
            }
            else
            {
                // Use the Thumb-2 reg,reg with rotation encoding
                emitIns_R_R_I(ins, attr, dstReg, srcReg, 0, INS_FLAGS_NOT_SET);
                return;
            }
            break;
        }

        default:
            unreached();
    }

    assert(sf != INS_FLAGS_DONT_CARE);

    instrDesc* id = emitNewInstrSmall();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idInsSize(emitInsSize(fmt));
    id->idInsFlags(sf);
    id->idReg1(dstReg);
    id->idReg2(srcReg);

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, insFlags flags)
{
    if (IsMovInstruction(ins))
    {
        assert(!"Please use emitIns_Mov() to correctly handle move elision");
        emitIns_Mov(ins, attr, reg1, reg2, /* canSkip */ false, flags);
    }

    emitAttr  size = EA_SIZE(attr);
    insFormat fmt  = IF_NONE;
    insFlags  sf   = INS_FLAGS_DONT_CARE;

    switch (ins)
    {
        case INS_add:
            // VM debugging single stepper doesn't support PC register with this instruction.
            // (but reg2 might be PC for ADD Rn, PC instruction)
            assert(reg1 != REG_PC);
            if (insDoesNotSetFlags(flags))
            {
                fmt = IF_T1_D0;
                sf  = INS_FLAGS_NOT_SET;
                break;
            }
            FALLTHROUGH;

        case INS_sub:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            // Use the Thumb-1 reg,reg,reg encoding
            emitIns_R_R_R(ins, attr, reg1, reg1, reg2, flags);
            return;

        case INS_cmp:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(insSetsFlags(flags));
            sf = INS_FLAGS_SET;
            if (isLowRegister(reg1) && isLowRegister(reg2))
            {
                fmt = IF_T1_E; // both are low registers
            }
            else
            {
                fmt = IF_T1_D0; // one or both are high registers
            }
            break;

        case INS_vcvt_d2i:
        case INS_vcvt_d2u:
        case INS_vcvt_d2f:
            assert(isFloatReg(reg1));
            assert(isDoubleReg(reg2));
            goto VCVT_COMMON;

        case INS_vcvt_f2d:
        case INS_vcvt_u2d:
        case INS_vcvt_i2d:
            assert(isDoubleReg(reg1));
            assert(isFloatReg(reg2));
            goto VCVT_COMMON;

        case INS_vcvt_u2f:
        case INS_vcvt_i2f:
        case INS_vcvt_f2i:
        case INS_vcvt_f2u:
            assert(size == EA_4BYTE);
            assert(isFloatReg(reg1));
            assert(isFloatReg(reg2));
            goto VCVT_COMMON;

        case INS_vabs:
        case INS_vsqrt:
        case INS_vcmp:
        case INS_vneg:
            if (size == EA_8BYTE)
            {
                assert(isDoubleReg(reg1));
                assert(isDoubleReg(reg2));
            }
            else
            {
                assert(isFloatReg(reg1));
                assert(isFloatReg(reg2));
            }

        VCVT_COMMON:
            fmt = IF_T2_VFP2;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_vadd:
        case INS_vmul:
        case INS_vsub:
        case INS_vdiv:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            emitIns_R_R_R(ins, attr, reg1, reg1, reg2);
            return;

        case INS_vldr:
        case INS_vstr:
        case INS_ldr:
        case INS_ldrb:
        case INS_ldrsb:
        case INS_ldrh:
        case INS_ldrsh:

        case INS_str:
        case INS_strb:
        case INS_strh:
            emitIns_R_R_I(ins, attr, reg1, reg2, 0);
            return;

        case INS_adc:
        case INS_and:
        case INS_bic:
        case INS_eor:
        case INS_orr:
        case INS_sbc:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            if (insSetsFlags(flags) && isLowRegister(reg1) && isLowRegister(reg2))
            {
                fmt = IF_T1_E;
                sf  = INS_FLAGS_SET;
                break;
            }
            FALLTHROUGH;

        case INS_orn:
            // assert below fired for bug 281892 where the two operands of an OR were
            // the same static field load which got cse'd.
            // there's no reason why this assert would be true in general
            // assert(reg1 != reg2);
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            // Use the Thumb-2 three register encoding
            emitIns_R_R_R_I(ins, attr, reg1, reg1, reg2, 0, flags);
            return;

        case INS_asr:
        case INS_lsl:
        case INS_lsr:
        case INS_ror:
            // assert below fired for bug 296394 where the two operands of an
            // arithmetic right shift were the same local variable
            // there's no reason why this assert would be true in general
            // assert(reg1 != reg2);
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            if (insSetsFlags(flags) && isLowRegister(reg1) && isLowRegister(reg2))
            {
                fmt = IF_T1_E;
                sf  = INS_FLAGS_SET;
            }
            else
            {
                // Use the Thumb-2 three register encoding
                emitIns_R_R_R(ins, attr, reg1, reg1, reg2, flags);
                return;
            }
            break;

        case INS_mul:
            // We will prefer the T2 encoding, unless (flags == INS_FLAGS_SET)
            // The thumb-1 instruction executes much slower as it must always set the flags
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            if (insMustSetFlags(flags) && isLowRegister(reg1) && isLowRegister(reg2))
            {
                fmt = IF_T1_E;
                sf  = INS_FLAGS_SET;
            }
            else
            {
                // Use the Thumb-2 three register encoding
                emitIns_R_R_R(ins, attr, reg1, reg2, reg1, flags);
                return;
            }
            break;

        case INS_mvn:
        case INS_cmn:
        case INS_tst:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            if (insSetsFlags(flags) && isLowRegister(reg1) && isLowRegister(reg2))
            {
                fmt = IF_T1_E;
                sf  = INS_FLAGS_SET;
            }
            else
            {
                // Use the Thumb-2 register with shift encoding
                emitIns_R_R_I(ins, attr, reg1, reg2, 0, flags);
                return;
            }
            break;

        case INS_tbb:
            assert(size == EA_4BYTE);
            assert(insDoesNotSetFlags(flags));
            fmt = IF_T2_C9;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_tbh:
            assert(size == EA_4BYTE);
            assert(insDoesNotSetFlags(flags));
            fmt = IF_T2_C9;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_clz:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(insDoesNotSetFlags(flags));
            fmt = IF_T2_C10;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_ldrexb:
        case INS_strexb:
            assert(size == EA_4BYTE);
            assert(insDoesNotSetFlags(flags));
            fmt = IF_T2_E1;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_ldrexh:
        case INS_strexh:
            assert(size == EA_4BYTE);
            assert(insDoesNotSetFlags(flags));
            fmt = IF_T2_E1;
            sf  = INS_FLAGS_NOT_SET;
            break;
        default:
#ifdef DEBUG
            printf("did not expect instruction %s\n", insName(ins));
#endif
            unreached();
    }

    assert((fmt == IF_T1_D0) || (fmt == IF_T1_E) || (fmt == IF_T2_C3) || (fmt == IF_T2_C9) || (fmt == IF_T2_C10) ||
           (fmt == IF_T2_VFP2) || (fmt == IF_T2_VMOVD) || (fmt == IF_T2_VMOVS) || (fmt == IF_T2_E1));

    assert(sf != INS_FLAGS_DONT_CARE);

    instrDesc* id = emitNewInstrSmall();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idInsSize(emitInsSize(fmt));
    id->idInsFlags(sf);
    id->idReg1(reg1);
    id->idReg2(reg2);

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_R_I_I(instruction ins, emitAttr attr, RegNum reg, int imm1, int imm2, insFlags flags)
{
    int imm = 0; // combined immediates

    assert(ins == INS_bfc);
    assert(reg != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.

    int lsb = imm1;
    int msb = lsb + imm2 - 1;

    assert((lsb >= 0) && (lsb <= 31)); // required for encoding of INS_bfc
    assert((msb >= 0) && (msb <= 31)); // required for encoding of INS_bfc
    assert(msb >= lsb);                // required for encoding of INS_bfc

    imm = (lsb << 5) | msb;

    assert(insDoesNotSetFlags(flags));

    instrDesc* id = emitNewInstrSC(imm);
    id->idIns(ins);
    id->idInsFmt(IF_T2_D1);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idInsSize(ISZ_32BIT);
    id->idInsFlags(INS_FLAGS_NOT_SET);
    id->idReg1(reg);

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_R_R_I(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, int imm, insFlags flags, insOpts opt)
{
    emitAttr  size = EA_SIZE(attr);
    insFormat fmt  = IF_NONE;
    insFlags  sf   = INS_FLAGS_DONT_CARE;

    if (ins == INS_lea)
    {
        ins = INS_add;
    }

    switch (ins)
    {
        case INS_add:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(insOptsNone(opt));

            // Can we possibly encode the immediate 'imm' using a Thumb-1 encoding?
            if ((reg2 == REG_SP) && insDoesNotSetFlags(flags) && ((imm & 0x03fc) == imm))
            {
                if ((reg1 == REG_SP) && ((imm & 0x01fc) == imm))
                {
                    // Use Thumb-1 encoding
                    emitIns_R_I(ins, attr, reg1, imm, flags);
                    return;
                }
                else if (isLowRegister(reg1))
                {
                    fmt = IF_T1_J2;
                    sf  = INS_FLAGS_NOT_SET;
                    break;
                }
            }
            FALLTHROUGH;

        case INS_sub:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(insOptsNone(opt));

            // Is it just a mov?
            if ((imm == 0) && insDoesNotSetFlags(flags))
            {
                // Is the mov even necessary?
                // Fix 383915 ARM ILGEN
                emitIns_Mov(INS_mov, attr, reg1, reg2, /* canSkip */ true, flags);
                return;
            }
            // Can we encode the immediate 'imm' using a Thumb-1 encoding?
            else if (isLowRegister(reg1) && isLowRegister(reg2) && insSetsFlags(flags) && (unsigned_abs(imm) <= 0x0007))
            {
                if (imm < 0)
                {
                    assert((ins == INS_add) || (ins == INS_sub));
                    if (ins == INS_add)
                        ins = INS_sub;
                    else
                        ins = INS_add;
                    imm     = -imm;
                }
                fmt = IF_T1_G;
                sf  = INS_FLAGS_SET;
            }
            else if ((reg1 == reg2) && isLowRegister(reg1) && insSetsFlags(flags) && (unsigned_abs(imm) <= 0x00ff))
            {
                if (imm < 0)
                {
                    assert((ins == INS_add) || (ins == INS_sub));
                    if (ins == INS_add)
                        ins = INS_sub;
                    else
                        ins = INS_add;
                    imm     = -imm;
                }
                // Use Thumb-1 encoding
                emitIns_R_I(ins, attr, reg1, imm, flags);
                return;
            }
            else if (isModImmConst(imm))
            {
                fmt = IF_T2_L0;
                sf  = insMustSetFlags(flags);
            }
            else if (isModImmConst(-imm))
            {
                assert((ins == INS_add) || (ins == INS_sub));
                ins = (ins == INS_add) ? INS_sub : INS_add;
                imm = -imm;
                fmt = IF_T2_L0;
                sf  = insMustSetFlags(flags);
            }
            else if (insDoesNotSetFlags(flags) && (unsigned_abs(imm) <= 0x0fff))
            {
                if (imm < 0)
                {
                    assert((ins == INS_add) || (ins == INS_sub));
                    ins = (ins == INS_add) ? INS_sub : INS_add;
                    imm = -imm;
                }
                // add/sub => addw/subw instruction
                // Note that even when using the w prefix the immediate is still only 12 bits?
                ins = (ins == INS_add) ? INS_addw : INS_subw;
                fmt = IF_T2_M0;
                sf  = INS_FLAGS_NOT_SET;
            }
            else if (insDoesNotSetFlags(flags) && (reg1 != REG_SP) && (reg1 != REG_PC))
            {
                // movw,movt reg1, imm
                codeGen->instGen_Set_Reg_To_Imm(attr, reg1, (ins == INS_sub ? -1 : 1) * imm);

                // ins reg1, reg2
                emitIns_R_R(INS_add, attr, reg1, reg2);

                return;
            }
            else
            {
                assert(!"Instruction cannot be encoded");
            }
            break;

        case INS_and:
        case INS_bic:
        case INS_orr:
        case INS_orn:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(insOptsNone(opt));
            if (isModImmConst(imm))
            {
                fmt = IF_T2_L0;
                sf  = insMustSetFlags(flags);
            }
            else if (isModImmConst(~imm))
            {
                fmt = IF_T2_L0;
                sf  = insMustSetFlags(flags);
                imm = ~imm;

                if (ins == INS_and)
                    ins = INS_bic;
                else if (ins == INS_bic)
                    ins = INS_and;
                else if (ins == INS_orr)
                    ins = INS_orn;
                else if (ins == INS_orn)
                    ins = INS_orr;
                else
                    assert(!"Instruction cannot be encoded");
            }
            else
            {
                assert(!"Instruction cannot be encoded");
            }
            break;

        case INS_rsb:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(insOptsNone(opt));
            if (imm == 0 && isLowRegister(reg1) && isLowRegister(reg2) && insSetsFlags(flags))
            {
                fmt = IF_T1_E;
                sf  = INS_FLAGS_SET;
                break;
            }
            FALLTHROUGH;

        case INS_adc:
        case INS_eor:
        case INS_sbc:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(insOptsNone(opt));
            if (isModImmConst(imm))
            {
                fmt = IF_T2_L0;
                sf  = insMustSetFlags(flags);
            }
            else
            {
                assert(!"Instruction cannot be encoded");
            }
            break;

        case INS_adr:
            // TODO-MIKE-Review: adr isn't used on ARM and this code is likely broken, this would
            // need to create an instrDescJmp instead of instrDesc and it's not even clear why is
            // this in R_R_I instead of R_I or R_L or whatever. Probably not very useful anyway
            // as the imm range is very small.
            assert(insOptsNone(opt));
            assert(insDoesNotSetFlags(flags));
            assert(reg2 == REG_PC);
            sf = INS_FLAGS_NOT_SET;

            if (isLowRegister(reg1) && ((imm & 0x00ff) == imm))
            {
                fmt = IF_T1_J3;
            }
            else if ((imm & 0x0fff) == imm)
            {
                fmt = IF_T2_M1;
            }
            else
            {
                assert(!"Instruction cannot be encoded");
            }
            break;

        case INS_mvn:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert((imm >= 0) && (imm <= 31)); // required for encoding
            assert(!insOptAnyInc(opt));
            if (imm == 0)
            {
                assert(insOptsNone(opt));
                if (isLowRegister(reg1) && isLowRegister(reg2) && insSetsFlags(flags))
                {
                    // Use the Thumb-1 reg,reg encoding
                    emitIns_R_R(ins, attr, reg1, reg2, flags);
                    return;
                }
            }
            else // imm > 0  &&  imm <= 31
            {
                assert(insOptAnyShift(opt));
            }
            fmt = IF_T2_C1;
            sf  = insMustSetFlags(flags);
            break;

        case INS_cmp:
        case INS_cmn:
        case INS_teq:
        case INS_tst:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(insSetsFlags(flags));
            assert((imm >= 0) && (imm <= 31)); // required for encoding
            assert(!insOptAnyInc(opt));
            if (imm == 0)
            {
                assert(insOptsNone(opt));
                if (ins == INS_cmp)
                {
                    // Use the Thumb-1 reg,reg encoding
                    emitIns_R_R(ins, attr, reg1, reg2, flags);
                    return;
                }
                if (((ins == INS_cmn) || (ins == INS_tst)) && isLowRegister(reg1) && isLowRegister(reg2))
                {
                    // Use the Thumb-1 reg,reg encoding
                    emitIns_R_R(ins, attr, reg1, reg2, flags);
                    return;
                }
            }
            else // imm > 0  &&  imm <= 31)
            {
                assert(insOptAnyShift(opt));
                if (insOptsRRX(opt))
                    assert(imm == 1);
            }

            fmt = IF_T2_C8;
            sf  = INS_FLAGS_SET;
            break;

        case INS_ror:
        case INS_asr:
        case INS_lsl:
        case INS_lsr:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(insOptsNone(opt));

            // On ARM, the immediate shift count of LSL and ROR must be between 1 and 31. For LSR and ASR, it is between
            // 1 and 32, though we don't ever use 32. Although x86 allows an immediate shift count of 8-bits in
            // instruction encoding, the CPU looks at only the lower 5 bits. As per ECMA, specifying a shift count to
            // the IL SHR, SHL, or SHL.UN instruction that is greater than or equal to the width of the type will yield
            // an undefined value. We choose that undefined value in this case to match x86 behavior, by only using the
            // lower 5 bits of the constant shift count.
            imm &= 0x1f;

            if (imm == 0)
            {
                // Additional Fix 383915 ARM ILGEN
                emitIns_Mov(INS_mov, attr, reg1, reg2, /* canSkip */ !insMustSetFlags(flags), flags);
                return;
            }

            if (insSetsFlags(flags) && (ins != INS_ror) && isLowRegister(reg1) && isLowRegister(reg2))
            {
                fmt = IF_T1_C;
                sf  = INS_FLAGS_SET;
            }
            else
            {
                fmt = IF_T2_C2;
                sf  = insMustSetFlags(flags);
            }
            break;

        case INS_sxtb:
        case INS_uxtb:
            assert(size == EA_4BYTE);
            goto EXTEND_COMMON;

        case INS_sxth:
        case INS_uxth:
            assert(size == EA_4BYTE);
        EXTEND_COMMON:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(insOptsNone(opt));
            assert(insDoesNotSetFlags(flags));
            assert((imm & 0x018) == imm); // required for encoding

            if ((imm == 0) && isLowRegister(reg1) && isLowRegister(reg2))
            {
                // Use Thumb-1 encoding
                emitIns_R_R(ins, attr, reg1, reg2, INS_FLAGS_NOT_SET);
                return;
            }

            fmt = IF_T2_C6;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_pld:
        case INS_pldw:
#ifdef FEATURE_PLI_INSTRUCTION
        case INS_pli:
#endif // FEATURE_PLI_INSTRUCTION
            assert(insOptsNone(opt));
            assert(insDoesNotSetFlags(flags));
            assert((imm & 0x003) == imm); // required for encoding

            fmt = IF_T2_C7;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_ldrb:
        case INS_strb:
            assert(size == EA_4BYTE);
            assert(insDoesNotSetFlags(flags));

            if (isLowRegister(reg1) && isLowRegister(reg2) && insOptsNone(opt) && ((imm & 0x001f) == imm))
            {
                fmt = IF_T1_C;
                sf  = INS_FLAGS_NOT_SET;
                break;
            }
            goto COMMON_THUMB2_LDST;

        case INS_ldrsb:
            assert(size == EA_4BYTE);
            goto COMMON_THUMB2_LDST;

        case INS_ldrh:
        case INS_strh:
            assert(size == EA_4BYTE);
            assert(insDoesNotSetFlags(flags));

            if (isLowRegister(reg1) && isLowRegister(reg2) && insOptsNone(opt) && ((imm & 0x003e) == imm))
            {
                fmt = IF_T1_C;
                sf  = INS_FLAGS_NOT_SET;
                break;
            }
            goto COMMON_THUMB2_LDST;

        case INS_ldrsh:
            assert(size == EA_4BYTE);
            goto COMMON_THUMB2_LDST;

        case INS_vldr:
        case INS_vstr:
        case INS_vldm:
        case INS_vstm:
            assert(fmt == IF_NONE);
            assert(insDoesNotSetFlags(flags));
            assert(offsetFitsInVectorMem(imm)); // required for encoding
            if (insOptAnyInc(opt))
            {
                if (insOptsPostInc(opt))
                {
                    assert(imm > 0);
                }
                else // insOptsPreDec(opt)
                {
                    assert(imm < 0);
                }
            }
            else
            {
                assert(insOptsNone(opt));
            }

            sf  = INS_FLAGS_NOT_SET;
            fmt = IF_T2_VLDST;
            break;

        case INS_ldr:
        case INS_str:
            assert(size == EA_4BYTE);
            assert(insDoesNotSetFlags(flags));

            // Can we possibly encode the immediate 'imm' using a Thumb-1 encoding?
            if (isLowRegister(reg1) && insOptsNone(opt) && ((imm & 0x03fc) == imm))
            {
                if (reg2 == REG_SP)
                {
                    fmt = IF_T1_J2;
                    sf  = INS_FLAGS_NOT_SET;
                    break;
                }
                else if (reg2 == REG_PC)
                {
                    if (ins == INS_ldr)
                    {
                        fmt = IF_T1_J3;
                        sf  = INS_FLAGS_NOT_SET;
                        break;
                    }
                }
                else if (isLowRegister(reg2))
                {
                    // Only the smaller range 'imm' can be encoded
                    if ((imm & 0x07c) == imm)
                    {
                        fmt = IF_T1_C;
                        sf  = INS_FLAGS_NOT_SET;
                        break;
                    }
                }
            }
        //
        // If we did not find a thumb-1 encoding above
        //

        COMMON_THUMB2_LDST:
            assert(fmt == IF_NONE);
            assert(insDoesNotSetFlags(flags));
            sf = INS_FLAGS_NOT_SET;

            if (insOptAnyInc(opt))
            {
                if (insOptsPostInc(opt))
                    assert(imm > 0);
                else // insOptsPreDec(opt)
                    assert(imm < 0);

                if (unsigned_abs(imm) <= 0x00ff)
                {
                    fmt = IF_T2_H0;
                }
                else
                {
                    assert(!"Instruction cannot be encoded");
                }
            }
            else
            {
                assert(insOptsNone(opt));
                if ((reg2 == REG_PC) && (unsigned_abs(imm) <= 0x0fff))
                {
                    fmt = IF_T2_K4;
                }
                else if ((imm & 0x0fff) == imm)
                {
                    fmt = IF_T2_K1;
                }
                else if (unsigned_abs(imm) <= 0x0ff)
                {
                    fmt = IF_T2_H0;
                }
                else
                {
                    // Load imm into a register
                    RegNum rsvdReg = codeGen->rsGetRsvdReg();
                    codeGen->instGen_Set_Reg_To_Imm(EA_4BYTE, rsvdReg, (ssize_t)imm);
                    emitIns_R_R_R(ins, attr, reg1, reg2, rsvdReg);
                    return;
                }
            }
            break;

        case INS_ldrex:
        case INS_strex:
            assert(insOptsNone(opt));
            assert(insDoesNotSetFlags(flags));
            sf = INS_FLAGS_NOT_SET;

            if ((imm & 0x03fc) == imm)
            {
                fmt = IF_T2_H0;
            }
            else
            {
                assert(!"Instruction cannot be encoded");
            }
            break;

        default:
            assert(!"Unexpected instruction");
    }
    assert((fmt == IF_T1_C) || (fmt == IF_T1_E) || (fmt == IF_T1_G) || (fmt == IF_T1_J2) || (fmt == IF_T1_J3) ||
           (fmt == IF_T2_C1) || (fmt == IF_T2_C2) || (fmt == IF_T2_C6) || (fmt == IF_T2_C7) || (fmt == IF_T2_C8) ||
           (fmt == IF_T2_H0) || (fmt == IF_T2_H1) || (fmt == IF_T2_K1) || (fmt == IF_T2_K4) || (fmt == IF_T2_L0) ||
           (fmt == IF_T2_M0) || (fmt == IF_T2_VLDST) || (fmt == IF_T2_M1));
    assert(sf != INS_FLAGS_DONT_CARE);

    instrDesc* id = emitNewInstrSC(imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idInsSize(emitInsSize(fmt));
    id->idInsFlags(sf);
    id->idInsOpt(opt);
    id->idReg1(reg1);
    id->idReg2(reg2);

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_R_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, insFlags flags)
{
    emitAttr  size = EA_SIZE(attr);
    insFormat fmt  = IF_NONE;
    insFlags  sf   = INS_FLAGS_DONT_CARE;

    switch (ins)
    {
        case INS_add:
            // Encodings do not support SP in the reg3 slot
            if (reg3 == REG_SP)
            {
                // Swap reg2 and reg3
                reg3 = reg2;
                reg2 = REG_SP;
            }
            FALLTHROUGH;

        case INS_sub:
            assert(reg3 != REG_SP);
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(reg3 != REG_PC || ins == INS_add); // allow ADD Rn, PC instruction in T2 encoding

            if (isLowRegister(reg1) && isLowRegister(reg2) && isLowRegister(reg3) && insSetsFlags(flags))
            {
                fmt = IF_T1_H;
                sf  = INS_FLAGS_SET;
                break;
            }

            if ((ins == INS_add) && insDoesNotSetFlags(flags))
            {
                if (reg1 == reg2)
                {
                    // Use the Thumb-1 regdest,reg encoding
                    emitIns_R_R(ins, attr, reg1, reg3, flags);
                    return;
                }
                if (reg1 == reg3)
                {
                    // Use the Thumb-1 regdest,reg encoding
                    emitIns_R_R(ins, attr, reg1, reg2, flags);
                    return;
                }
            }

            // Use the Thumb-2 reg,reg,reg with shift encoding
            emitIns_R_R_R_I(ins, attr, reg1, reg2, reg3, 0, flags);
            return;

        case INS_adc:
        case INS_and:
        case INS_bic:
        case INS_eor:
        case INS_orr:
        case INS_sbc:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(reg3 != REG_PC);
            if (reg1 == reg2)
            {
                // Try to encode as a Thumb-1 instruction
                emitIns_R_R(ins, attr, reg1, reg3, flags);
                return;
            }
            FALLTHROUGH;

        case INS_orn:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(reg3 != REG_PC);
            // Use the Thumb-2 three register encoding, with imm=0
            emitIns_R_R_R_I(ins, attr, reg1, reg2, reg3, 0, flags);
            return;

        case INS_asr:
        case INS_lsl:
        case INS_lsr:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(reg3 != REG_PC);
            if (reg1 == reg2 && insSetsFlags(flags) && isLowRegister(reg1) && isLowRegister(reg3))
            {
                // Use the Thumb-1 regdest,reg encoding
                emitIns_R_R(ins, attr, reg1, reg3, flags);
                return;
            }
            FALLTHROUGH;

        case INS_ror:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(reg3 != REG_PC);
            fmt = IF_T2_C4;
            sf  = insMustSetFlags(flags);
            break;

        case INS_mul:
            if (insMustSetFlags(flags))
            {
                assert(reg1 !=
                       REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
                assert(reg2 != REG_PC);
                assert(reg3 != REG_PC);

                if ((reg1 == reg2) && isLowRegister(reg1))
                {
                    // Use the Thumb-1 regdest,reg encoding
                    emitIns_R_R(ins, attr, reg1, reg3, flags);
                    return;
                }
                if ((reg1 == reg3) && isLowRegister(reg1))
                {
                    // Use the Thumb-1 regdest,reg encoding
                    emitIns_R_R(ins, attr, reg1, reg2, flags);
                    return;
                }
                else
                {
                    assert(!"Instruction cannot be encoded");
                }
            }

#if !defined(USE_HELPERS_FOR_INT_DIV)
            FALLTHROUGH;
        case INS_sdiv:
        case INS_udiv:
#endif // !USE_HELPERS_FOR_INT_DIV

            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(reg3 != REG_PC);
            assert(insDoesNotSetFlags(flags));
            fmt = IF_T2_C5;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_ldrb:
        case INS_strb:
        case INS_ldrsb:
            assert(size == EA_4BYTE);
            goto COMMON_THUMB1_LDST;

        case INS_ldrsh:
        case INS_ldrh:
        case INS_strh:
            assert(size == EA_4BYTE);
            goto COMMON_THUMB1_LDST;

        case INS_ldr:
        case INS_str:
            assert(size == EA_4BYTE);

        COMMON_THUMB1_LDST:
            assert(insDoesNotSetFlags(flags));

            if (isLowRegister(reg1) && isLowRegister(reg2) && isLowRegister(reg3))
            {
                fmt = IF_T1_H;
                sf  = INS_FLAGS_NOT_SET;
            }
            else
            {
                // Use the Thumb-2 reg,reg,reg with shift encoding
                emitIns_R_R_R_I(ins, attr, reg1, reg2, reg3, 0, flags);
                return;
            }
            break;

        case INS_vadd:
        case INS_vmul:
        case INS_vsub:
        case INS_vdiv:
            if (size == EA_8BYTE)
            {
                assert(isDoubleReg(reg1));
                assert(isDoubleReg(reg2));
                assert(isDoubleReg(reg3));
            }
            else
            {
                assert(isFloatReg(reg1));
                assert(isFloatReg(reg2));
                assert(isFloatReg(reg3));
            }
            fmt = IF_T2_VFP3;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_vmov_i2d:
            assert(reg2 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg3 != REG_PC);
            assert(isDoubleReg(reg1));
            assert(isGeneralRegister(reg2));
            assert(isGeneralRegister(reg3));
            fmt = IF_T2_VMOVD;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_vmov_d2i:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(isGeneralRegister(reg1));
            assert(isGeneralRegister(reg2));
            assert(isDoubleReg(reg3));
            fmt = IF_T2_VMOVD;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_ldrexd:
        case INS_strexd:
            assert(insDoesNotSetFlags(flags));
            fmt = IF_T2_G1;
            sf  = INS_FLAGS_NOT_SET;
            break;

        default:
            unreached();
    }
    assert((fmt == IF_T1_H) || (fmt == IF_T2_C4) || (fmt == IF_T2_C5) || (fmt == IF_T2_VFP3) || (fmt == IF_T2_VMOVD) ||
           (fmt == IF_T2_G1));
    assert(sf != INS_FLAGS_DONT_CARE);

    instrDesc* id = emitNewInstr();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idInsSize(emitInsSize(fmt));
    id->idInsFlags(sf);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idReg3(reg3);

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_R_R_I_I(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, int imm1, int imm2, insFlags flags)
{
    insFormat fmt = IF_NONE;
    insFlags  sf  = INS_FLAGS_DONT_CARE;

    int lsb   = imm1;
    int width = imm2;
    int msb   = lsb + width - 1;
    int imm   = 0; // combined immediate

    assert((lsb >= 0) && (lsb <= 31));    // required for encodings
    assert((width > 0) && (width <= 32)); // required for encodings
    assert((msb >= 0) && (msb <= 31));    // required for encodings
    assert(msb >= lsb);                   // required for encodings

    switch (ins)
    {
        case INS_bfi:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);

            assert(insDoesNotSetFlags(flags));
            imm = (lsb << 5) | msb;

            fmt = IF_T2_D0;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_sbfx:
        case INS_ubfx:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);

            assert(insDoesNotSetFlags(flags));
            imm = (lsb << 5) | (width - 1);

            fmt = IF_T2_D0;
            sf  = INS_FLAGS_NOT_SET;
            break;

        default:
            unreached();
    }
    assert((fmt == IF_T2_D0));
    assert(sf != INS_FLAGS_DONT_CARE);

    instrDesc* id = emitNewInstrSC(imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idInsSize(emitInsSize(fmt));
    id->idInsFlags(sf);
    id->idReg1(reg1);
    id->idReg2(reg2);

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_R_R_R_I(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, int32_t imm, insFlags flags, insOpts opt)
{
    emitAttr  size = EA_SIZE(attr);
    insFormat fmt  = IF_NONE;
    insFlags  sf   = INS_FLAGS_DONT_CARE;

    switch (ins)
    {

        case INS_add:
        case INS_sub:
            if (imm == 0)
            {
                if (isLowRegister(reg1) && isLowRegister(reg2) && isLowRegister(reg3) && insSetsFlags(flags))
                {
                    // Use the Thumb-1 reg,reg,reg encoding
                    emitIns_R_R_R(ins, attr, reg1, reg2, reg3, flags);
                    return;
                }
                if ((ins == INS_add) && insDoesNotSetFlags(flags))
                {
                    if (reg1 == reg2)
                    {
                        // Use the Thumb-1 regdest,reg encoding
                        emitIns_R_R(ins, attr, reg1, reg3, flags);
                        return;
                    }
                    if (reg1 == reg3)
                    {
                        // Use the Thumb-1 regdest,reg encoding
                        emitIns_R_R(ins, attr, reg1, reg2, flags);
                        return;
                    }
                }
            }
            FALLTHROUGH;

        case INS_adc:
        case INS_and:
        case INS_bic:
        case INS_eor:
        case INS_orn:
        case INS_orr:
        case INS_sbc:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(reg3 != REG_PC);
            assert((imm >= 0) && (imm <= 31)); // required for encoding
            assert(!insOptAnyInc(opt));
            if (imm == 0)
            {
                if (opt == INS_OPTS_LSL) // left shift of zero
                    opt = INS_OPTS_NONE; //           is a nop

                assert(insOptsNone(opt));
                if (isLowRegister(reg1) && isLowRegister(reg2) && isLowRegister(reg3) && insSetsFlags(flags))
                {
                    if (reg1 == reg2)
                    {
                        // Use the Thumb-1 regdest,reg encoding
                        emitIns_R_R(ins, attr, reg1, reg3, flags);
                        return;
                    }
                    if ((reg1 == reg3) && (ins != INS_bic) && (ins != INS_orn) && (ins != INS_sbc))
                    {
                        // Use the Thumb-1 regdest,reg encoding
                        emitIns_R_R(ins, attr, reg1, reg2, flags);
                        return;
                    }
                }
            }
            else // imm > 0  &&  imm <= 31)
            {
                assert(insOptAnyShift(opt));
                if (insOptsRRX(opt))
                    assert(imm == 1);
            }
            fmt = IF_T2_C0;
            sf  = insMustSetFlags(flags);
            break;

        case INS_ldrb:
        case INS_ldrsb:
        case INS_strb:
            assert(size == EA_4BYTE);
            goto COMMON_THUMB2_LDST;

        case INS_ldrh:
        case INS_ldrsh:
        case INS_strh:
            assert(size == EA_4BYTE);
            goto COMMON_THUMB2_LDST;

        case INS_ldr:
        case INS_str:
            assert(size == EA_4BYTE);

        COMMON_THUMB2_LDST:
            assert(insDoesNotSetFlags(flags));
            assert((imm & 0x0003) == imm); // required for encoding

            if ((imm == 0) && insOptsNone(opt) && isLowRegister(reg1) && isLowRegister(reg2) && isLowRegister(reg3))
            {
                // Use the Thumb-1 reg,reg,reg encoding
                emitIns_R_R_R(ins, attr, reg1, reg2, reg3, flags);
                return;
            }
            assert(insOptsNone(opt) || insOptsLSL(opt));
            fmt = IF_T2_E0;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_ldrd:
        case INS_strd:
            assert(insDoesNotSetFlags(flags));
            assert((imm & 0x03) == 0);
            sf = INS_FLAGS_NOT_SET;

            if (insOptAnyInc(opt))
            {
                if (insOptsPostInc(opt))
                    assert(imm > 0);
                else // insOptsPreDec(opt)
                    assert(imm < 0);
            }
            else
            {
                assert(insOptsNone(opt));
            }

            if (unsigned_abs(imm) <= 0x03fc)
            {
                imm >>= 2;
                fmt = IF_T2_G0;
            }
            else
            {
                assert(!"Instruction cannot be encoded");
            }
            break;

        default:
            unreached();
    }
    assert((fmt == IF_T2_C0) || (fmt == IF_T2_E0) || (fmt == IF_T2_G0));
    assert(sf != INS_FLAGS_DONT_CARE);

    instrDesc* id = emitNewInstrCns(imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idInsSize(emitInsSize(fmt));
    id->idInsFlags(sf);
    id->idInsOpt(opt);
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idReg3(reg3);

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_R_R_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, RegNum reg4)
{
    insFormat fmt = IF_NONE;
    insFlags  sf  = INS_FLAGS_NOT_SET;

    switch (ins)
    {

        case INS_smull:
        case INS_umull:
        case INS_smlal:
        case INS_umlal:
            assert(reg1 != reg2); // Illegal encoding
            fmt = IF_T2_F1;
            break;
        case INS_mla:
        case INS_mls:
            fmt = IF_T2_F2;
            break;
        default:
            unreached();
    }
    assert((fmt == IF_T2_F1) || (fmt == IF_T2_F2));

    assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
    assert(reg2 != REG_PC);
    assert(reg3 != REG_PC);
    assert(reg4 != REG_PC);

    instrDesc* id = emitNewInstr();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idInsSize(emitInsSize(fmt));
    id->idInsFlags(sf);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idReg3(reg3);
    id->idReg4(reg4);

    dispIns(id);
    appendToCurIG(id);
}

void emitter::MovRegStackOffset(RegNum reg, int32_t imm, StackAddrMode s)
{
    auto mov = [&](instruction ins, int32_t imm) {
        instrDesc* id = emitNewInstrCns(imm);
        id->idIns(ins);
        id->idInsFmt(IF_T2_N);
        id->idOpSize(EA_4BYTE);
        id->idInsSize(ISZ_32BIT);
        id->idReg1(reg);
        // TODO-MIKE-Cleanup: Only disassembly uses this...
        id->SetVarAddr(INDEBUG(s));

        dispIns(id);
        appendToCurIG(id);
    };

    mov(INS_movw, imm & 0xFFFF);

    if ((imm >> 16) != 0)
    {
        mov(INS_movt, (imm >> 16) & 0xFFFF);
    }
}

static constexpr bool IsUnsignedImm8(int imm, unsigned shift = 0)
{
    return (imm & ~(255 << shift)) == 0;
}

static constexpr bool IsSignedImm8(int imm, unsigned shift = 0)
{
    return (unsigned_abs(imm) & ~(255 << shift)) == 0;
}

static constexpr bool IsUnsignedImm12(int imm)
{
    return (imm & ~4095) == 0;
}

static constexpr bool IsSignedImm12(int imm)
{
    return (unsigned_abs(imm) & ~4095) == 0;
}

#ifdef DEBUG
static bool IsLoad(instruction ins)
{
    switch (ins)
    {
        case INS_ldr:
        case INS_ldrh:
        case INS_ldrb:
        case INS_ldrsh:
        case INS_ldrsb:
        case INS_vldr:
            return true;
        default:
            return false;
    }
}

static bool IsStore(instruction ins)
{
    switch (ins)
    {
        case INS_str:
        case INS_strh:
        case INS_strb:
        case INS_vstr:
            return true;
        default:
            return false;
    }
}
#endif

void emitter::emitIns_R_S(instruction ins, emitAttr attr, RegNum reg, StackAddrMode s)
{
    assert(IsLoad(ins) || (ins == INS_lea));
    Ins_R_S(ins, attr, reg, s);
}

void emitter::emitIns_S_R(instruction ins, emitAttr attr, RegNum reg, StackAddrMode s)
{
    assert(IsStore(ins));
    Ins_R_S(ins, attr, reg, s);
}

void emitter::Ins_R_S(instruction ins, emitAttr attr, RegNum reg, StackAddrMode s)
{
    assert(IsLoad(ins) || IsStore(ins) || (ins == INS_lea));

    bool    isFloatLoadStore = (ins == INS_vldr) || (ins == INS_vstr);
    bool    fpBased;
    int     baseOffset = emitComp->lvaFrameAddress(s.varNum, &fpBased) + s.varOffs;
    int32_t imm        = baseOffset;
    RegNum  baseReg;

    if (!fpBased)
    {
        baseReg = REG_SP;
    }
    else if (codeGen->funCurrentFunc().kind != FUNC_ROOT)
    {
        baseReg = REG_FPBASE;
    }
    else
    {
        imm = OptimizeFrameAddress(imm, isFloatLoadStore, &baseReg);
    }

    insFormat fmt;

    if (ins == INS_lea)
    {
        if (isLowRegister(reg) && (baseReg == REG_SP) && IsUnsignedImm8(imm, 2))
        {
            ins = INS_add;
            fmt = IF_T1_J2;
        }
        else if (IsSignedImm12(imm))
        {
            if (imm >= 0)
            {
                ins = INS_addw;
            }
            else
            {
                ins = INS_subw;
                imm = -imm;
            }

            fmt = IF_T2_M0;
        }
        else
        {
            RegNum tempReg = codeGen->rsGetRsvdReg();
            MovRegStackOffset(tempReg, imm, s);
            emitIns_R_R_R(INS_add, attr, reg, baseReg, tempReg);

            return;
        }
    }
    else if (isFloatLoadStore)
    {
        if (!IsSignedImm8(imm, 2))
        {
            RegNum tempReg = codeGen->rsGetRsvdReg();
            MovRegStackOffset(tempReg, imm, s);
            emitIns_R_R(INS_add, EA_4BYTE, tempReg, baseReg);
            emitIns_R_R_I(ins, attr, reg, tempReg, 0);

            return;
        }

        fmt = IF_T2_VLDST;
    }
    else if (((ins == INS_ldr) || (ins == INS_str)) && isLowRegister(reg) && (baseReg == REG_SP) &&
             IsUnsignedImm8(imm, 2))
    {
        fmt = IF_T1_J2;
    }
    else if (IsUnsignedImm12(imm))
    {
        fmt = IF_T2_K1;
    }
    else if (IsSignedImm8(imm))
    {
        fmt = IF_T2_H0;
    }
    else
    {
        MovRegStackOffset(codeGen->rsGetRsvdReg(), imm, s);

        fmt = IF_T2_E0;
    }

    instrDesc* id = emitNewInstrCns(imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idInsSize(emitInsSize(fmt));
    id->idReg1(reg);
    id->idReg2(baseReg);
    id->SetVarAddr(INDEBUG(s));

    if ((ins == INS_str) && EA_IS_GCREF_OR_BYREF(attr))
    {
        id->idAddr()->lclOffset = baseOffset;

        if (s.varNum < 0)
        {
            assert(s.varOffs == 0);
            id->idAddr()->isTrackedGCSlotStore = codeGen->spillTemps.TrackGCSpillTemps();
        }
        else if (static_cast<unsigned>(s.varNum) == emitComp->lvaOutgoingArgSpaceVar)
        {
            id->idAddr()->isGCArgStore = true;
        }
        else if ((s.varOffs == 0) && (emitComp->lvaGetDesc(static_cast<unsigned>(s.varNum))->HasGCSlotLiveness()))
        {
            id->idAddr()->isTrackedGCSlotStore = true;
        }
    }

    dispIns(id);
    appendToCurIG(id);
}

// Change frame pointer based addressing to SP-based addressing when possible because it has smaller encoding.
int emitter::OptimizeFrameAddress(int fpOffset, bool isFloatLoadStore, RegNum* baseReg)
{
    int spOffset = fpOffset + codeGen->genSPtoFPdelta();

    int encodingLimitUpper = isFloatLoadStore ? 1020 : 4095;
    int encodingLimitLower = isFloatLoadStore ? -1020 : -255;

    // TODO-MIKE-Review: It's not clear what's the minopts check for.
    // Maybe it was supposed to be !MinOpts()?

    if (emitComp->opts.MinOpts() || (spOffset <= encodingLimitUpper) || (fpOffset < encodingLimitLower) ||
        (fpOffset > encodingLimitUpper))
    {
        *baseReg = emitComp->compLocallocUsed ? REG_SAVED_LOCALLOC_SP : REG_SP;
        return spOffset;
    }
    else
    {
        *baseReg = REG_FPBASE;
        return fpOffset;
    }
}

void emitter::emitSetShortJump(instrDescJmp* id)
{
    assert((id->idInsFmt() == IF_T2_J1) || (id->idInsFmt() == IF_T2_J2) || (id->idInsFmt() == IF_LARGEJMP));
    assert(!id->idIsCnsReloc());

    id->idInsFmt(id->idInsFmt() == IF_T2_J2 ? IF_T1_M : IF_T1_K);
    id->idInsSize(ISZ_16BIT);
}

void emitter::emitSetMediumJump(instrDescJmp* id)
{
    assert((id->idInsFmt() == IF_T2_J1) || (id->idInsFmt() == IF_LARGEJMP));
    assert(!id->idIsCnsReloc());

    id->idInsFmt(IF_T2_J1);
    id->idInsSize(ISZ_32BIT);
}

#define LBL_DIST_SMALL_MAX_NEG (0)
#define LBL_DIST_SMALL_MAX_POS (+1020)
#define LBL_DIST_MED_MAX_NEG (-4095)
#define LBL_DIST_MED_MAX_POS (+4096)

#define JMP_DIST_SMALL_MAX_NEG (-2048)
#define JMP_DIST_SMALL_MAX_POS (+2046)

#define CALL_DIST_MAX_NEG (-16777216)
#define CALL_DIST_MAX_POS (+16777214)

#define JCC_DIST_SMALL_MAX_NEG (-256)
#define JCC_DIST_SMALL_MAX_POS (+254)

#define JCC_DIST_MEDIUM_MAX_NEG (-1048576)
#define JCC_DIST_MEDIUM_MAX_POS (+1048574)

#define JMP_SIZE_SMALL (2)
#define JMP_SIZE_LARGE (4)

static bool IsConditionalBranch(instruction ins)
{
    return (INS_beq <= ins) && (ins <= INS_ble);
}

static bool IsBranch(instruction ins)
{
    return (ins == INS_b) || IsConditionalBranch(ins);
}

void emitter::emitIns_J(instruction ins, int instrCount)
{
    assert(IsMainProlog(emitCurIG));
    assert(IsBranch(ins));
    assert(instrCount < 0);

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(ins);
    id->idInsFmt(ins == INS_b ? IF_T1_M : IF_T1_K);
    id->idInsSize(ISZ_16BIT);
    id->SetInstrCount(instrCount);

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_J(instruction ins, insGroup* label)
{
    assert(IsBranch(ins));
    assert(emitCurIG->GetFuncletIndex() == label->GetFuncletIndex());

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(ins);
    id->idInsFmt(ins == INS_b ? IF_T2_J2 : IF_LARGEJMP);
    id->idInsSize(emitInsSize(id->idInsFmt()));
    id->idSetIsCnsReloc(emitComp->opts.compReloc && InDifferentRegions(emitCurIG, label));
    id->SetLabel(label);

    if (label->IsDefined() && !id->idIsCnsReloc())
    {
        // This is a backward jump, we can determine now if it's going to be short/medium/large.

        uint32_t instrOffs = emitCurCodeOffset + emitCurIGsize;
        int32_t  distance  = instrOffs - label->igOffs;
        assert(distance >= 0);
        distance += 4;

        if (ins == INS_b)
        {
            if (JMP_DIST_SMALL_MAX_NEG <= -distance)
            {
                id->idInsFmt(IF_T1_M);
                id->idInsSize(ISZ_16BIT);
            }
        }
        else
        {
            if (JCC_DIST_SMALL_MAX_NEG <= -distance)
            {
                id->idInsFmt(IF_T1_K);
                id->idInsSize(ISZ_16BIT);
            }
            else if (JCC_DIST_MEDIUM_MAX_NEG <= -distance)
            {
                id->idInsFmt(IF_T2_J1);
                id->idInsSize(ISZ_32BIT);
            }
        }
    }

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_J_R(instruction ins, emitAttr attr, insGroup* label, RegNum reg)
{
    // TODO-MIKE-Review: cbz/cbnz aren't used on ARM. Delete or try to use these instructions?
    // Their limited range might make using them problematic, we might save a cheap 0 compare
    // and end up with an extra unconditional branch.
    assert((ins == INS_cbz) || (ins == INS_cbnz));
    assert(emitCurIG->GetFuncletIndex() == label->GetFuncletIndex());
    assert(isLowRegister(reg));
    assert(!emitComp->opts.compReloc || !InDifferentRegions(emitCurIG, label));

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(ins);
    id->idInsFmt(IF_T1_I);
    id->idInsSize(ISZ_16BIT);
    id->idOpSize(EA_4BYTE);
    id->idReg1(reg);
    id->SetLabel(label);

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_CallFinally(insGroup* label)
{
    INDEBUG(VerifyCallFinally(label));

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(INS_b);
    id->idInsFmt(IF_T2_J2);
    id->idInsSize(ISZ_32BIT);
    id->idSetIsCnsReloc(emitComp->opts.compReloc && InDifferentRegions(emitCurIG, label));
    id->SetLabel(label);

    if (label->IsDefined() && !id->idIsCnsReloc())
    {
        // This is a backward jump, we can determine now if it's going to be short/medium/large.

        uint32_t instrOffs = emitCurCodeOffset + emitCurIGsize;
        int32_t  distance  = instrOffs - label->igOffs;
        assert(distance >= 0);
        distance += 4;

        if (JMP_DIST_SMALL_MAX_NEG <= -distance)
        {
            id->idInsFmt(IF_T1_M);
            id->idInsSize(ISZ_16BIT);
        }
    }

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_R_L(instruction ins, RegNum reg, insGroup* label)
{
    assert((ins == INS_movt) || (ins == INS_movw));
    assert(label != nullptr);

#ifdef DEBUG
    if (codeGen->GetCurrentBlock()->bbJumpKind == BBJ_EHCATCHRET)
    {
        VerifyCatchRet(label);
    }
#endif

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(ins);
    id->idInsFmt(IF_T2_N1);
    id->idInsSize(ISZ_32BIT);
    id->idOpSize(EA_4BYTE);
    id->idReg1(reg);
    id->SetLabel(label);
    id->idSetIsCnsReloc(emitComp->opts.compReloc);

    dispIns(id);
    appendToCurIG(id);
}

void emitter::emitIns_R_D(instruction ins, RegNum reg, ConstData* data)
{
    assert((ins == INS_movw) || (ins == INS_movt));

    instrDesc* id = emitNewInstrSC(data->offset);
    id->idIns(ins);
    id->idReg1(reg);
    id->idInsFmt(IF_T2_N2);
    id->idOpSize(EA_4BYTE);
    id->idInsSize(ISZ_32BIT);
    id->idSetIsCnsReloc(emitComp->opts.compReloc);

    dispIns(id);
    appendToCurIG(id);
}

// Add a call instruction (direct or indirect).
//
// EC_FUNC_TOKEN : addr is the method address
// EC_INDIR_R    : call ireg (addr has to be null)
//
// Please consult the "debugger team notification" comment in genFnProlog().
//
void emitter::emitIns_Call(EmitCallType          kind,
                           CORINFO_METHOD_HANDLE methodHandle DEBUGARG(CORINFO_SIG_INFO* sigInfo),
                           void*    addr,
                           emitAttr retSize,
                           RegNum   reg,
                           bool     isJump)
{
    assert((kind == EC_INDIR_R) || (reg == REG_NA));
    assert((kind != EC_INDIR_R) || (addr == nullptr));
    assert((kind != EC_INDIR_R) || (reg != REG_NA));

    instrDesc* id = emitNewInstrCall(methodHandle, retSize);

    if (kind == EC_INDIR_R)
    {
        id->idIns(isJump ? INS_bx : INS_blx);
        id->idInsFmt(IF_T1_D2);
        id->idInsSize(ISZ_16BIT);
        id->idReg3(reg);
    }
    else
    {
        assert(kind == EC_FUNC_TOKEN);
        // if addr is nullptr then this call is treated as a recursive call.
        assert((addr == nullptr) || validImmForBL(reinterpret_cast<ssize_t>(addr), emitComp));

        id->idIns(isJump ? INS_b : INS_bl);
        id->idInsFmt(IF_T2_J3);
        id->idInsSize(ISZ_32BIT);
        id->idSetIsCnsReloc(emitComp->opts.compReloc);
        id->SetAddr(addr);
    }

#ifdef DEBUG
    id->idDebugOnlyInfo()->idHandle  = methodHandle;
    id->idDebugOnlyInfo()->idCallSig = sigInfo;
#endif

#ifdef LATE_DISASM
    if (addr != nullptr)
    {
        disSetMethod(reinterpret_cast<size_t>(addr), methodHandle);
    }
#endif

    dispIns(id);
    appendToCurIG(id);
}

void EmitterBase::EncodeCallGCRegs(regMaskTP regs, instrDesc* id)
{
    static_assert_no_msg(instrDesc::RegBits >= 4);
    assert((regs & RBM_CALLEE_TRASH) == RBM_NONE);

    unsigned encoded = 0;

    if ((regs & RBM_R4) != RBM_NONE)
        encoded |= 0x01;
    if ((regs & RBM_R5) != RBM_NONE)
        encoded |= 0x02;
    if ((regs & RBM_R6) != RBM_NONE)
        encoded |= 0x04;
    if ((regs & RBM_R7) != RBM_NONE)
        encoded |= 0x08;

    id->idReg1(static_cast<RegNum>(encoded));

    encoded = 0;

    if ((regs & RBM_R8) != RBM_NONE)
        encoded |= 0x01;
    if ((regs & RBM_R9) != RBM_NONE)
        encoded |= 0x02;
    if ((regs & RBM_R10) != RBM_NONE)
        encoded |= 0x04;
    if ((regs & RBM_R11) != RBM_NONE)
        encoded |= 0x08;

    id->idReg2(static_cast<RegNum>(encoded));
}

unsigned EmitterBase::DecodeCallGCRegs(instrDesc* id)
{
    unsigned encoded = id->idReg1() | (id->idReg2() << 8);
    unsigned regs    = 0;

    if ((encoded & 0x01) != 0)
        regs |= RBM_R4;
    if ((encoded & 0x02) != 0)
        regs |= RBM_R5;
    if ((encoded & 0x04) != 0)
        regs |= RBM_R6;
    if ((encoded & 0x08) != 0)
        regs |= RBM_R7;

    if ((encoded & 0x0100) != 0)
        regs |= RBM_R8;
    if ((encoded & 0x0200) != 0)
        regs |= RBM_R9;
    if ((encoded & 0x0400) != 0)
        regs |= RBM_R10;
    if ((encoded & 0x0800) != 0)
        regs |= RBM_R11;

    return regs;
}

void EmitterBase::ShortenBranches()
{
    if (emitJumpList == nullptr)
    {
        return;
    }

#ifdef DEBUG
    if (emitComp->verbose)
    {
        printf("\nInstruction groups before jump shortening:\n\n");
        emitDispIGlist(true);
    }
#endif

AGAIN:
    INDEBUG(emitCheckIGoffsets());

    uint32_t      minDistanceOverflow  = UINT32_MAX;
    uint32_t      totalSizeReduction   = 0;
    uint32_t      instrIGSizeReduction = 0;
    instrDescJmp* previousInstr        = nullptr;
    insGroup*     previousInstrIG      = emitJumpList->idjIG;

    for (instrDescJmp *instr = emitJumpList; instr != nullptr; previousInstr = instr, instr = instr->idjNext)
    {
        assert((instr->idInsFmt() == IF_T2_J1) || (instr->idInsFmt() == IF_T2_J2) || (instr->idInsFmt() == IF_T1_I) ||
               (instr->idInsFmt() == IF_T1_K) || (instr->idInsFmt() == IF_T1_M) || (instr->idInsFmt() == IF_T2_M1) ||
               (instr->idInsFmt() == IF_T2_N1) || (instr->idInsFmt() == IF_T1_J3) ||
               (instr->idInsFmt() == IF_LARGEJMP));

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

        if (!IsBranch(instr->idIns()) || instr->idIsCnsReloc() || !instr->HasLabel())
        {
            continue;
        }

        uint32_t currentSize = instr->idCodeSize();

        if (currentSize == 0)
        {
            continue;
        }

        int32_t smallNegativeDistance;
        int32_t smallPositiveDistance;
        int32_t mediumNegativeDistance;
        int32_t mediumPositiveDistance;

        if (instr->idInsFmt() == IF_T2_J2)
        {
            smallNegativeDistance  = JMP_DIST_SMALL_MAX_NEG;
            smallPositiveDistance  = JMP_DIST_SMALL_MAX_POS;
            mediumNegativeDistance = 0;
            mediumPositiveDistance = 0;
        }
        else
        {
            smallNegativeDistance  = JCC_DIST_SMALL_MAX_NEG;
            smallPositiveDistance  = JCC_DIST_SMALL_MAX_POS;
            mediumNegativeDistance = JCC_DIST_MEDIUM_MAX_NEG;
            mediumPositiveDistance = JCC_DIST_MEDIUM_MAX_POS;
        }

        uint32_t  instrOffs    = instrIG->igOffs + instr->idjOffs;
        uint32_t  instrEndOffs = instrOffs + 4;
        insGroup* label        = instr->GetLabel();
        uint32_t  labelOffs    = label->igOffs;
        int32_t   smallDistanceOverflow;
        int32_t   mediumDistanceOverflow;

        if (label->igNum > instrIG->igNum)
        {
            labelOffs -= totalSizeReduction;

            int32_t distance       = labelOffs - instrEndOffs;
            smallDistanceOverflow  = distance - smallPositiveDistance;
            mediumDistanceOverflow = distance - mediumPositiveDistance;
        }
        else
        {
            int32_t distance       = instrEndOffs - labelOffs;
            smallDistanceOverflow  = distance + smallNegativeDistance;
            mediumDistanceOverflow = distance + mediumNegativeDistance;
        }

        JITDUMP("Jump IN%04X from %04X +%u (" FMT_IG ") to %04X (" FMT_IG
                "), distance %d, overflow %d, medium overflow %d%s\n",
                instr->idDebugOnlyInfo()->idNum, instrOffs, 2, instrIG->GetId(), labelOffs, label->GetId(),
                labelOffs - instrEndOffs, smallDistanceOverflow, mediumDistanceOverflow,
                smallDistanceOverflow <= 0 ? ", short" : (mediumDistanceOverflow <= 0 ? "medium" : ""));

        uint32_t newSize = currentSize;

        if (instrOffs + currentSize == labelOffs)
        {
            // Removing a "jump to next" could produce another "jump to next", we need to force another pass
            // to eliminate that too. Ideally we'd traverse the jump list backwards, but it's a forward only
            // list and given the rarity of such nested jumps it's hard to justify the extra code and memory
            // required to traverse the list both ways.
            minDistanceOverflow = 0;

            instr->idInsSize(ISZ_NONE);
            newSize = 0;
        }
        else if (smallDistanceOverflow <= 0)
        {
            if (currentSize > 2)
            {
                static_cast<emitter*>(this)->emitSetShortJump(instr);
                assert(instr->idInsSize() == ISZ_16BIT);
                newSize = 2;
            }
        }
        else
        {
            minDistanceOverflow = Min(minDistanceOverflow, static_cast<uint32_t>(smallDistanceOverflow));

            if (mediumDistanceOverflow > 0)
            {
                minDistanceOverflow = Min(minDistanceOverflow, static_cast<uint32_t>(mediumDistanceOverflow));
            }
            else if (currentSize > 4)
            {
                static_cast<emitter*>(this)->emitSetMediumJump(instr);
                assert(instr->idInsSize() == ISZ_32BIT);
                newSize = 4;
            }
        }

        if (newSize != currentSize)
        {
            uint32_t sizeReduction = currentSize - newSize;
            instrIG->igSize -= static_cast<uint16_t>(sizeReduction);
            instrIG->igFlags |= IGF_UPD_ISZ;
            instrIGSizeReduction += sizeReduction;
            totalSizeReduction += sizeReduction;
        }
    }

    if (totalSizeReduction != 0)
    {
        for (insGroup* ig = previousInstrIG->igNext; ig != nullptr; ig = ig->igNext)
        {
            JITDUMP(FMT_IG " moved back from %04X", ig->GetId(), ig->igOffs);
            ig->igOffs -= totalSizeReduction;
            JITDUMP(" to % 04X\n", ig->igOffs);
        }

        emitTotalCodeSize -= totalSizeReduction;

        JITDUMP("Total size reduction %u, min distance overflow %u\n", totalSizeReduction, minDistanceOverflow);

        if (minDistanceOverflow <= totalSizeReduction)
        {
            JITDUMP("Iterating branch shortening\n");

            goto AGAIN;
        }

        INDEBUG(emitCheckIGoffsets());
    }

#ifdef DEBUG
    if (emitComp->verbose)
    {
        printf("\nLabels list after the jump shortening:\n\n");
        emitDispIGlist(false);
    }
#endif
}

// Returns an encoding for the specified register (any-reg) to be used in a Thumb-1 encoding in the M4 position.
static unsigned insEncodeRegT1_M4(RegNum reg)
{
    assert(reg < REG_STK);

    return reg << 3;
}

// Returns an encoding for the specified register (any-reg) to be used in a Thumb-1 encoding in the D4 position.
static unsigned insEncodeRegT1_D4(RegNum reg)
{
    assert(reg < REG_STK);

    return (reg & 0x7) | ((reg & 0x8) << 4);
}

// Returns an encoding for the specified register (low-only) to be used in a Thumb-1 encoding in the M3 position.
static unsigned insEncodeRegT1_M3(RegNum reg)
{
    assert(reg < REG_R8);

    return reg << 6;
}

// Returns an encoding for the specified register (low-only) to be used in a Thumb-1 encoding in the N3 position.
static unsigned insEncodeRegT1_N3(RegNum reg)
{
    assert(reg < REG_R8);

    return reg << 3;
}

// Returns an encoding for the specified register (low-only) to be used in a Thumb-1 encoding in the D3 position.
static unsigned insEncodeRegT1_D3(RegNum reg)
{
    assert(reg < REG_R8);

    return reg;
}

// Returns an encoding for the specified register (low-only) to be used in a Thumb-1 encoding in the DI position.
static unsigned insEncodeRegT1_DI(RegNum reg)
{
    assert(reg < REG_R8);

    return reg << 8;
}

// Returns an encoding for the specified register to be used in a Thumb-2 encoding in the N position.
static unsigned insEncodeRegT2_N(RegNum reg)
{
    assert(reg < REG_STK);

    return reg << 16;
}

static unsigned floatRegIndex(RegNum reg, int size)
{
    // theoretically this could support quad floats as well but for now...
    assert(size == EA_8BYTE || size == EA_4BYTE);

    if (size == EA_8BYTE)
        assert(isDoubleReg(reg));
    else
        assert(isFloatReg(reg));

    unsigned result = reg - REG_F0;

    // the assumption here is that the register F8 also refers to D4
    if (size == EA_8BYTE)
    {
        result >>= 1;
    }

    return result;
}

// Some ARM VFP instructions use the convention that for doubles, the split bit holds
// the msb of the register index for singles it holds the lsb
// excerpt : d = if dp_operation then UInt(D:Vd)
// if single  UInt(Vd:D);
static unsigned floatRegEncoding(unsigned index, int size, bool variant = false)
{
    if (!variant || size == EA_8BYTE)
        return index;
    else
    {
        return ((index & 1) << 4) | (index >> 1);
    }
}

// thumb2 VFP M register encoding
static unsigned insEncodeRegT2_VectorM(RegNum reg, int size, bool variant)
{
    unsigned enc = floatRegIndex(reg, size);
    enc          = floatRegEncoding(enc, size, variant);
    return ((enc & 0xf) << 0) | ((enc & 0x10) << 1);
}

// thumb2 VFP N register encoding
static unsigned insEncodeRegT2_VectorN(RegNum reg, int size, bool variant)
{
    unsigned enc = floatRegIndex(reg, size);
    enc          = floatRegEncoding(enc, size, variant);
    return ((enc & 0xf) << 16) | ((enc & 0x10) << 3);
}

// thumb2 VFP D register encoding
static unsigned insEncodeRegT2_VectorD(RegNum reg, int size, bool variant)
{
    unsigned enc = floatRegIndex(reg, size);
    enc          = floatRegEncoding(enc, size, variant);
    return ((enc & 0xf) << 12) | ((enc & 0x10) << 18);
}

// Returns an encoding for the specified register to be used in a Thumb-2 encoding in the T position.
static unsigned insEncodeRegT2_T(RegNum reg)
{
    assert(reg < REG_STK);

    return reg << 12;
}

// Returns an encoding for the specified register to be used in a Thumb-2 encoding in the D position.
static unsigned insEncodeRegT2_D(RegNum reg)
{
    assert(reg < REG_STK);

    return reg << 8;
}

// Returns an encoding for the specified register to be used in a Thumb-2 encoding in the M position.
static unsigned insEncodeRegT2_M(RegNum reg)
{
    assert(reg < REG_STK);

    return reg;
}

// Returns the encoding for the Set Flags bit to be used in a Thumb-2 encoding.
static unsigned insEncodeSetFlags(insFlags sf)
{
    if (sf == INS_FLAGS_SET)
        return (1 << 20);
    else
        return 0;
}

// Returns the encoding for the Shift Type bits to be used in a Thumb-2 encoding.
static unsigned insEncodeShiftOpts(insOpts opt)
{
    if (opt == INS_OPTS_NONE)
        return 0;
    else if (opt == INS_OPTS_LSL)
        return 0x00;
    else if (opt == INS_OPTS_LSR)
        return 0x10;
    else if (opt == INS_OPTS_ASR)
        return 0x20;
    else if (opt == INS_OPTS_ROR)
        return 0x30;
    else if (opt == INS_OPTS_RRX)
        return 0x30;

    assert(!"Invalid insOpts");
    return 0;
}

// Returns the encoding for the PUW bits to be used in a T2_G0 Thumb-2 encoding.
static unsigned insEncodePUW_G0(insOpts opt, int imm)
{
    unsigned result = 0;

    if (opt != INS_OPTS_LDST_POST_INC)
        result |= (1 << 24); // The P bit

    if (imm >= 0)
        result |= (1 << 23); // The U bit

    if (opt != INS_OPTS_NONE)
        result |= (1 << 21); // The W bits
    return result;
}

// Returns the encoding for the PUW bits to be used in a T2_H0 Thumb-2 encoding.
static unsigned insEncodePUW_H0(insOpts opt, int imm)
{
    unsigned result = 0;

    if (opt != INS_OPTS_LDST_POST_INC)
        result |= (1 << 10); // The P bit

    if (imm >= 0)
        result |= (1 << 9); // The U bit

    if (opt != INS_OPTS_NONE)
        result |= (1 << 8); // The W bits

    return result;
}

// Returns the encoding for the Shift Count bits to be used in a Thumb-2 encoding.
static unsigned insEncodeShiftCount(int imm)
{
    unsigned result;

    assert((imm & 0x001F) == imm);
    result = (imm & 0x03) << 6;
    result |= (imm & 0x1C) << 10;

    return result;
}

// Returns the encoding for the immediate use by BFI/BFC Thumb-2 encodings.
static unsigned insEncodeBitFieldImm(int imm)
{
    unsigned result;

    assert((imm & 0x03FF) == imm);
    result = (imm & 0x001f);
    result |= (imm & 0x0060) << 1;
    result |= (imm & 0x0380) << 5;

    return result;
}

// Returns an encoding for the immediate use by MOV/MOVW Thumb-2 encodings.
static unsigned insEncodeImmT2_Mov(int imm)
{
    unsigned result;

    assert((imm & 0x0000ffff) == imm);
    result = (imm & 0x00ff);
    result |= ((imm & 0x0700) << 4);
    result |= ((imm & 0x0800) << 15);
    result |= ((imm & 0xf000) << 4);

    return result;
}

// Unscales the immediate operand of a given IF_T1_C instruction.
static int insUnscaleImm(instruction ins, int imm)
{
    switch (ins)
    {
        case INS_ldr:
        case INS_str:
            assert((imm & 0x0003) == 0);
            imm >>= 2;
            break;
        case INS_ldrh:
        case INS_strh:
            assert((imm & 0x0001) == 0);
            imm >>= 1;
            break;
        case INS_ldrb:
        case INS_strb:
        case INS_lsl:
        case INS_lsr:
        case INS_asr:
            // Do nothing
            break;
        default:
            assert(!"Invalid IF_T1_C instruction");
            break;
    }
    return imm;
}

unsigned ArmEncoder::emitOutput_Thumb1Instr(uint8_t* dst, uint32_t code)
{
    assert((code & 0xFFFF0000) == 0);
    assert((code >> (16 - 5)) < 29);

    *reinterpret_cast<uint16_t*>(dst + writeableOffset) = static_cast<uint16_t>(code & 0xffff);
    return 2;
}

unsigned ArmEncoder::emitOutput_Thumb2Instr(uint8_t* dst, uint32_t code)
{
    assert((code >> (32 - 5)) >= 29);

    *reinterpret_cast<uint16_t*>(dst + writeableOffset) = static_cast<uint16_t>(code >> 16);
    dst += 2;
    *reinterpret_cast<uint16_t*>(dst + writeableOffset) = static_cast<uint16_t>(code & 0xffff);
    return 4;
}

uint8_t* ArmEncoder::emitOutputRL(uint8_t* dst, instrDescJmp* id)
{
    assert(id->idGCref() == GCT_NONE);

    instruction ins = id->idIns();
    insFormat   fmt = id->idInsFmt();

    assert((ins == INS_adr) || (ins == INS_movw) || (ins == INS_movt));
    assert((fmt == IF_T1_J3) || (fmt == IF_T2_M1) || (fmt == IF_T2_N1));

    uint32_t srcOffs = emitCurCodeOffs(dst);
    uint32_t dstOffs = id->GetLabel()->igOffs;
    ssize_t  distance;

    if (ins == INS_adr)
    {
        // For adr, the distance is calculated from 4-byte aligned srcOffs.
        distance = emitOffsetToPtr(dstOffs) -
                   reinterpret_cast<uint8_t*>(reinterpret_cast<size_t>(emitOffsetToPtr(srcOffs)) & ~3) + 1;
    }
    else
    {
        assert((ins == INS_movw) || (ins == INS_movt));

        distance = reinterpret_cast<ssize_t>(emitOffsetToPtr(dstOffs)) + 1; // Or in thumb bit
    }

    // Adjust the offset to emit relative to the end of the instruction.
    if (ins == INS_adr)
    {
        distance -= 4;
    }

    code_t code = emitInsCode(ins, fmt);

    if (fmt == IF_T1_J3)
    {
        assert((dstOffs & 3) == 0);
        assert((0 <= distance) && (distance <= 1022));

        code |= (distance >> 2) & 0xff;

        return dst + emitOutput_Thumb1Instr(dst, code);
    }

    if (fmt == IF_T2_M1)
    {
        assert((-4095 <= distance) && (distance <= 4095));

        if (distance < 0)
        {
            code |= 0x00A0 << 16;
            distance = -distance;
        }

        assert((distance & 0x0fff) == distance);

        code |= distance & 0x00ff;
        code |= (distance & 0x0700) << 4;
        code |= (distance & 0x0800) << 15;
        code |= id->idReg1() << 8;

        return dst + emitOutput_Thumb2Instr(dst, code);
    }

    assert(fmt == IF_T2_N1);
    assert((ins == INS_movt) || (ins == INS_movw));

    code |= insEncodeRegT2_D(id->idReg1());

    if (id->idIsCnsReloc())
    {
        dst += emitOutput_Thumb2Instr(dst, code);

        if ((ins == INS_movt) && emitComp->info.compMatchedVM)
        {
            emitHandlePCRelativeMov32(dst - 8, reinterpret_cast<void*>(distance));
        }

        return dst;
    }

    code |= insEncodeImmT2_Mov((ins == INS_movw ? distance : (distance >> 16)) & 0xffff);

    return dst + emitOutput_Thumb2Instr(dst, code);
}

uint8_t* ArmEncoder::emitOutputLJ(uint8_t* dst, instrDescJmp* id, insGroup* ig)
{
    assert(id->idGCref() == GCT_NONE);

    uint32_t labelOffs;

    if (id->idCodeSize() == 0)
    {
        return dst;
    }

    if (id->HasInstrCount())
    {
        assert(ig != nullptr);

        int      instrCount   = id->GetInstrCount();
        unsigned jumpInstrNum = emitFindInsNum(ig, id);

        assert((instrCount >= 0) || (jumpInstrNum + 1 >= static_cast<unsigned>(-instrCount)));

        labelOffs = ig->igOffs + ig->FindInsOffset(jumpInstrNum + 1 + instrCount);
    }
    else
    {
        labelOffs = id->GetLabel()->igOffs;
    }

    uint32_t    instrOffs = emitCurCodeOffs(dst);
    ssize_t     distance  = emitOffsetToPtr(labelOffs) - emitOffsetToPtr(instrOffs);
    instruction ins       = id->idIns();
    insFormat   fmt       = id->idInsFmt();

    // Adjust the offset to emit relative to the end of the instruction.
    distance -= 4;

    if (id->idInsSize() == Emitter::ISZ_16BIT)
    {
        assert(!id->idIsCnsReloc());
        assert(!emitJumpCrossHotColdBoundary(instrOffs, labelOffs));

        return emitOutputShortBranch(dst, ins, fmt, distance, id);
    }

    if (fmt == IF_T2_J1)
    {
        assert(!id->idIsCnsReloc());
        assert(!emitJumpCrossHotColdBoundary(instrOffs, labelOffs));
        assert((distance & 1) == 0);
        assert((-1048576 <= distance) && (distance <= 1048574));

        code_t code = emitInsCode(ins, fmt);

        if (distance < 0)
        {
            code |= 1 << 26;
        }

        code |= (distance >> 1) & 0x0007ff;
        code |= ((distance >> 1) & 0x01f800) << 5;
        code |= ((distance >> 1) & 0x020000) >> 4;
        code |= ((distance >> 1) & 0x040000) >> 7;

        return dst + emitOutput_Thumb2Instr(dst, code);
    }

    if (fmt == IF_LARGEJMP)
    {
        // This is a pseudo-instruction format representing a large conditional branch, to allow us
        // to get a greater branch target range than we can get by using a straightforward conditional
        // branch. It is encoded as a short conditional branch that branches around a long unconditional
        // branch.
        instruction reverse = Emitter::emitJumpKindToBranch(Emitter::emitReverseJumpKind(BranchToJumpKind(ins)));
        dst                 = emitOutputShortBranch(dst, reverse, IF_T1_K, 2, nullptr);

        // The distance was computed based on the beginning of the pseudo-instruction.
        distance -= 2;
    }
    else
    {
        assert(ins == INS_b);
        assert(fmt == IF_T2_J2);
    }

    assert((distance & 1) == 0);

    code_t code = emitInsCode(INS_b, IF_T2_J2);

    // For relocs we can't compute the offset so we just leave it set to 0,
    // the runtime will patch the instruction later.
    if (!id->idIsCnsReloc())
    {
        assert((CALL_DIST_MAX_NEG <= distance) && (distance <= CALL_DIST_MAX_POS));

        if (distance < 0)
        {
            code |= 1 << 26;
        }

        code |= (distance >> 1) & 0x0007ff;
        code |= ((distance >> 1) & 0x1ff800) << 5;

        bool S  = distance < 0;
        bool I1 = (distance & 0x00800000) == 0;
        bool I2 = (distance & 0x00400000) == 0;

        if (S ^ I1)
        {
            code |= (1 << 13); // J1 bit
        }

        if (S ^ I2)
        {
            code |= (1 << 11); // J2 bit
        }
    }

    emitOutput_Thumb2Instr(dst, code);

    if (id->idIsCnsReloc())
    {
        emitRecordRelocation(dst, emitOffsetToPtr(labelOffs), IMAGE_REL_BASED_THUMB_BRANCH24);
    }

    return dst + 4;
}

uint8_t* ArmEncoder::emitOutputShortBranch(
    uint8_t* dst, instruction ins, insFormat fmt, ssize_t distance, instrDesc* id)
{
    code_t code = emitInsCode(ins, fmt);

    if (fmt == IF_T1_K)
    {
        assert((distance & 1) == 0);
        assert((-256 <= distance) && (distance <= 254));

        if (distance < 0)
        {
            code |= 1 << 7;
        }

        code |= (distance >> 1) & 0x7f;
    }
    else if (fmt == IF_T1_M)
    {
        assert((distance & 1) == 0);
        assert((-2048 <= distance) && (distance <= 2046));

        if (distance < 0)
        {
            code |= 1 << 10;
        }

        code |= (distance >> 1) & 0x3ff;
    }
    else
    {
        assert(fmt == IF_T1_I);
        assert((ins == INS_cbz) || (ins == INS_cbnz));
        assert((distance & 1) == 0);
        assert((0 <= distance) && (distance <= 126));

        code |= (distance << 3) & 0x0200;
        code |= (distance << 2) & 0x00F8;
        code |= id->idReg1() & 0x0007;
    }

    return dst + emitOutput_Thumb1Instr(dst, code);
}

#ifdef FEATURE_ITINSTRUCTION

// The "IT" instruction is deprecated (with a very few exceptions). Don't generate it!
// Don't delete this code, though, in case we ever want to bring it back.
uint8_t* ArmEncoder::emitOutputIT(uint8_t* dst, instruction ins, insFormat fmt, code_t condcode)
{
    code_t imm0;
    code_t code, mask, bit;

    code = emitInsCode(ins, fmt);
    code |= (condcode << 4);        // encode firstcond
    imm0 = condcode & 1;            // this is firstcond[0]
    mask = code & 0x0f;             // initialize mask encoded in opcode
    bit  = 0x08;                    // where in mask we are encoding
    while ((mask & (bit - 1)) != 0) // are the remaining bits all zeros?
    {                               //  then we are done
        // otherwise determine the setting of bit
        if ((imm0 == 1) ^ ((bit & mask) != 0))
        {
            code |= bit; // set the current bit
        }
        else
        {
            code &= ~bit; // clear the current bit
        }
        bit >>= 1;
    }
    dst += emitOutput_Thumb1Instr(dst, code);

    return dst;
}

#endif // FEATURE_ITINSTRUCTION

size_t emitter::instrDesc::emitGetInstrDescSizeSC() const
{
    if (idIsSmallDsc())
    {
        return sizeof(instrDescSmall);
    }
    else if (idIsLargeCns())
    {
        return sizeof(instrDescCns);
    }
    else
    {
        return sizeof(instrDesc);
    }
}

// A helper for handling a Thumb-Mov32 of position-independent (PC-relative) value
//
// This routine either records relocation for the location with the EE,
// or creates a virtual relocation entry to perform offset fixup during
// compilation without recording it with EE - depending on which of
// absolute/relocative relocations mode are used for code section.
void ArmEncoder::emitHandlePCRelativeMov32(void* location, void* target)
{
    if (emitComp->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_RELATIVE_CODE_RELOCS))
    {
        emitRecordRelocation(location, target, IMAGE_REL_BASED_REL_THUMB_MOV32_PCREL);
    }
    else
    {
        emitRecordRelocation(location, target, IMAGE_REL_BASED_THUMB_MOV32);
    }
}

// Return the instruction size in bytes for the instruction at the specified location.
// This is used to assert that the unwind code being generated on ARM has the
// same size as the instruction for which it is being generated (since on ARM
// the unwind codes have a one-to-one relationship with instructions, and the
// unwind codes have an implicit instruction size that must match the instruction size.)
// An instruction must exist at the specified location.
unsigned emitter::emitGetInstructionSize(const emitLocation& emitLoc)
{
    insGroup*  ig;
    instrDesc* id;

    bool anyInstrs = emitGetLocationInfo(emitLoc, &ig, &id);
    // There better be an instruction at this location (otherwise,
    // we're at the end of the instruction list)
    assert(anyInstrs);

    return id->idCodeSize();
}

void EmitterBase::emitEndCodeGen()
{
    ArmEncoder encoder(static_cast<emitter*>(this));
    encoder.emitEndCodeGen();
}

// Append the machine code corresponding to the given instruction descriptor
// to the code block at '*dp'; the base of the code block is 'bp', and 'ig'
// is the instruction group that contains the instruction. Updates '*dp' to
// point past the generated code, and returns the size of the instruction
// descriptor in bytes.
size_t ArmEncoder::emitOutputInstr(insGroup* ig, instrDesc* id, uint8_t** dp)
{
    uint8_t*    dst  = *dp;
    uint8_t*    odst = dst;
    instruction ins  = id->idIns();
    insFormat   fmt  = id->idInsFmt();
    emitAttr    size = id->idOpSize();
    size_t      sz;

    assert(REG_NA == (int)REG_NA);

    switch (fmt)
    {
        code_t code;
        int    imm;
        void*  addr;

        case IF_T1_I:  // ......i.iiiiiddd                       R1                  imm6
        case IF_T1_K:  // ....cccciiiiiiii                       Branch              imm8, cond4
        case IF_T1_M:  // .....iiiiiiiiiii                       Branch              imm11
        case IF_T2_J1: // .....Scccciiiiii ..j.jiiiiiiiiiii      Branch              imm20, cond4
        case IF_T2_J2: // .....Siiiiiiiiii ..j.jiiiiiiiiii.      Branch              imm24
        case IF_LARGEJMP:
            dst = emitOutputLJ(dst, static_cast<instrDescJmp*>(id), ig);
            sz  = sizeof(instrDescJmp);
            break;

        case IF_T1_J3: // .....dddiiiiiiii                        R1  PC             imm8
        case IF_T2_M1: // .....i.......... .iiiddddiiiiiiii       R1  PC             imm12
        case IF_T2_N1: // .....i......iiii .iiiddddiiiiiiii       R1                 imm16
            dst = emitOutputRL(dst, static_cast<instrDescJmp*>(id));
            sz  = sizeof(instrDescJmp);
            break;

        case IF_T1_A: // T1_A    ................
            sz   = sizeof(instrDescSmall);
            code = emitInsCode(ins, fmt);
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

#ifdef FEATURE_ITINSTRUCTION
        case IF_T1_B: // T1_B    ........cccc....                                           cond
        {
            assert(id->idGCref() == GCT_NONE);
            int32_t condcode = id->emitGetInsSC();
            dst              = emitOutputIT(dst, ins, fmt, condcode);
            sz               = sizeof(instrDescSmall);
        }
        break;
#endif // FEATURE_ITINSTRUCTION

        case IF_T1_C: // T1_C    .....iiiiinnnddd                       R1  R2              imm5
            sz   = sizeof(instrDescSmall);
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT1_D3(id->idReg1());
            code |= insEncodeRegT1_N3(id->idReg2());
            imm = insUnscaleImm(ins, imm);
            assert((imm & 0x001f) == imm);
            code |= (imm << 6);
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T1_D0: // T1_D0   ........Dmmmmddd                       R1* R2*
            sz   = sizeof(instrDescSmall);
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT1_D4(id->idReg1());
            code |= insEncodeRegT1_M4(id->idReg2());
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T1_E: // T1_E    ..........nnnddd                       R1  R2
            sz   = sizeof(instrDescSmall);
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT1_D3(id->idReg1());
            code |= insEncodeRegT1_N3(id->idReg2());
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T1_F: // T1_F    .........iiiiiii                       SP                  imm7
            sz   = id->emitGetInstrDescSize();
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            assert((ins == INS_add) || (ins == INS_sub));
            assert((imm & 0x0003) == 0);
            imm >>= 2;
            assert((imm & 0x007F) == imm);
            code |= imm;
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T1_G: // T1_G    .......iiinnnddd                       R1  R2              imm3
            sz   = sizeof(instrDescSmall);
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT1_D3(id->idReg1());
            code |= insEncodeRegT1_N3(id->idReg2());
            assert((imm & 0x0007) == imm);
            code |= (imm << 6);
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T1_H: // T1_H    .......mmmnnnddd                       R1  R2  R3
            sz   = id->emitGetInstrDescSize();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT1_D3(id->idReg1());
            code |= insEncodeRegT1_N3(id->idReg2());
            code |= insEncodeRegT1_M3(id->idReg3());
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T1_J0: // T1_J0   .....dddiiiiiiii                       R1                  imm8
        case IF_T1_J1: // T1_J1   .....dddiiiiiiii                       R1                  <regmask8>
        case IF_T1_J2: // T1_J2   .....dddiiiiiiii                       R1  SP              imm8
            sz   = id->emitGetInstrDescSize();
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT1_DI(id->idReg1());
            if (fmt == IF_T1_J2)
            {
                assert((ins == INS_add) || (ins == INS_ldr) || (ins == INS_str));
                assert((imm & 0x0003) == 0);
                imm >>= 2;
            }
            assert((imm & 0x00ff) == imm);
            code |= imm;
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T1_L0: // T1_L0   ........iiiiiiii                                           imm8
        case IF_T1_L1: // T1_L1   .......Rrrrrrrrr                                           <regmask8>
            sz   = id->emitGetInstrDescSize();
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            if (fmt == IF_T1_L1)
            {
                assert((imm & 0x3) != 0x3);
                if (imm & 0x3)
                    code |= 0x0100; //  R bit
                imm >>= 2;
            }
            assert((imm & 0x00ff) == imm);
            code |= imm;
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T2_A: // T2_A    ................ ................
            sz   = sizeof(instrDescSmall);
            code = emitInsCode(ins, fmt);
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_B: // T2_B    ................ ............iiii                          imm4
            sz   = sizeof(instrDescSmall);
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            assert((imm & 0x000F) == imm);
            code |= imm;
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_C0: // T2_C0   ...........Snnnn .iiiddddiishmmmm       R1  R2  R3      S, imm5, sh
        case IF_T2_C4: // T2_C4   ...........Snnnn ....dddd....mmmm       R1  R2  R3      S
        case IF_T2_C5: // T2_C5   ............nnnn ....dddd....mmmm       R1  R2  R3
            sz   = id->emitGetInstrDescSize();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            code |= insEncodeRegT2_N(id->idReg2());
            code |= insEncodeRegT2_M(id->idReg3());
            if (fmt != IF_T2_C5)
                code |= insEncodeSetFlags(id->idInsFlags());
            if (fmt == IF_T2_C0)
            {
                imm = id->emitGetInsSC();
                code |= insEncodeShiftCount(imm);
                code |= insEncodeShiftOpts(id->idInsOpt());
            }
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_C1: // T2_C1   ...........S.... .iiiddddiishmmmm       R1  R2          S, imm5, sh
        case IF_T2_C2: // T2_C2   ...........S.... .iiiddddii..mmmm       R1  R2          S, imm5
        case IF_T2_C6: // T2_C6   ................ ....dddd..iimmmm       R1  R2                   imm2
            sz   = sizeof(instrDescSmall);
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            code |= insEncodeRegT2_M(id->idReg2());
            if (fmt == IF_T2_C6)
            {
                assert((imm & 0x0018) == imm);
                code |= (imm << 1);
            }
            else
            {
                code |= insEncodeSetFlags(id->idInsFlags());
                code |= insEncodeShiftCount(imm);
                if (fmt == IF_T2_C1)
                    code |= insEncodeShiftOpts(id->idInsOpt());
            }
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_C3: // T2_C3   ...........S.... ....dddd....mmmm       R1  R2          S
            sz   = sizeof(instrDescSmall);
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            code |= insEncodeRegT2_M(id->idReg2());
            code |= insEncodeSetFlags(id->idInsFlags());
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_C7: // T2_C7   ............nnnn ..........shmmmm       R1  R2                   imm2
        case IF_T2_C8: // T2_C8   ............nnnn .iii....iishmmmm       R1  R2             imm5, sh
            sz   = sizeof(instrDescSmall);
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_N(id->idReg1());
            code |= insEncodeRegT2_M(id->idReg2());
            if (fmt == IF_T2_C7)
            {
                assert((imm & 0x0003) == imm);
                code |= (imm << 4);
            }
            else if (fmt == IF_T2_C8)
            {
                code |= insEncodeShiftCount(imm);
                code |= insEncodeShiftOpts(id->idInsOpt());
            }
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_C9: // T2_C9   ............nnnn ............mmmm       R1  R2
            sz   = sizeof(instrDescSmall);
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_N(id->idReg1());
            code |= insEncodeRegT2_M(id->idReg2());
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_C10: // T2_C10  ............mmmm ....dddd....mmmm       R1  R2
            sz   = sizeof(instrDescSmall);
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            code |= insEncodeRegT2_M(id->idReg2());
            code |= insEncodeRegT2_N(id->idReg2());
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_D0: // T2_D0   ............nnnn .iiiddddii.wwwww       R1  R2             imm5, imm5
        case IF_T2_D1: // T2_D1   ................ .iiiddddii.wwwww       R1                 imm5, imm5
            sz   = sizeof(instrDescSmall);
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            if (fmt == IF_T2_D0)
                code |= insEncodeRegT2_N(id->idReg2());
            code |= insEncodeBitFieldImm(imm);
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_E0: // T2_E0   ............nnnn tttt......shmmmm       R1  R2  R3               imm2
        case IF_T2_E1: // T2_E1   ............nnnn tttt............       R1  R2
        case IF_T2_E2: // T2_E2   ................ tttt............       R1
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_T(id->idReg1());
            if (fmt == IF_T2_E0)
            {
                sz = id->emitGetInstrDescSize();
                code |= insEncodeRegT2_N(id->idReg2());
                if (id->idIsLclVar())
                {
                    code |= insEncodeRegT2_M(codeGen->rsGetRsvdReg());
                    imm = 0;
                }
                else
                {
                    code |= insEncodeRegT2_M(id->idReg3());
                    imm = id->emitGetInsSC();
                    assert((imm & 0x0003) == imm);
                    code |= (imm << 4);
                }
            }
            else
            {
                sz = sizeof(instrDescSmall);
                if (fmt != IF_T2_E2)
                {
                    code |= insEncodeRegT2_N(id->idReg2());
                }
            }
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_F1: // T2_F1    ............nnnn ttttdddd....mmmm       R1  R2  R3  R4
            sz = id->emitGetInstrDescSize();
            ;
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_T(id->idReg1());
            code |= insEncodeRegT2_D(id->idReg2());
            code |= insEncodeRegT2_N(id->idReg3());
            code |= insEncodeRegT2_M(id->idReg4());
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_F2: // T2_F2    ............nnnn aaaadddd....mmmm       R1  R2  R3  R4
            sz   = id->emitGetInstrDescSize();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            code |= insEncodeRegT2_N(id->idReg2());
            code |= insEncodeRegT2_M(id->idReg3());
            code |= insEncodeRegT2_T(id->idReg4());
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_G0: // T2_G0   .......PU.W.nnnn ttttTTTTiiiiiiii       R1  R2  R3         imm8, PUW
        case IF_T2_G1: // T2_G1   ............nnnn ttttTTTT........       R1  R2  R3
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_T(id->idReg1());
            code |= insEncodeRegT2_D(id->idReg2());
            code |= insEncodeRegT2_N(id->idReg3());
            if (fmt == IF_T2_G0)
            {
                sz  = id->emitGetInstrDescSizeSC();
                imm = id->emitGetInsSC();
                assert(unsigned_abs(imm) <= 0x00ff);
                code |= abs(imm);
                code |= insEncodePUW_G0(id->idInsOpt(), imm);
            }
            else
            {
                sz = id->emitGetInstrDescSize();
            }
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_H0: // T2_H0   ............nnnn tttt.PUWiiiiiiii       R1  R2             imm8, PUW
        case IF_T2_H1: // T2_H1   ............nnnn tttt....iiiiiiii       R1  R2             imm8
        case IF_T2_H2: // T2_H2   ............nnnn ........iiiiiiii       R1                 imm8
            sz   = id->emitGetInstrDescSizeSC();
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_T(id->idReg1());

            if (fmt != IF_T2_H2)
                code |= insEncodeRegT2_N(id->idReg2());

            if (fmt == IF_T2_H0)
            {
                assert(unsigned_abs(imm) <= 0x00ff);
                code |= insEncodePUW_H0(id->idInsOpt(), imm);
                code |= unsigned_abs(imm);
            }
            else
            {
                assert((imm & 0x00ff) == imm);
                code |= imm;
            }
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_I0: // T2_I0   ..........W.nnnn rrrrrrrrrrrrrrrr       R1              W, imm16
        case IF_T2_I1: // T2_I1   ................ rrrrrrrrrrrrrrrr                          imm16
            sz   = id->emitGetInstrDescSizeSC();
            code = emitInsCode(ins, fmt);
            if (fmt == IF_T2_I0)
            {
                code |= insEncodeRegT2_N(id->idReg1());
                code |= (1 << 21); //  W bit
            }
            imm = id->emitGetInsSC();
            assert((imm & 0x3) != 0x3);
            if (imm & 0x2)
                code |= 0x8000; //  PC bit
            if (imm & 0x1)
                code |= 0x4000; //  LR bit
            imm >>= 2;
            assert(imm <= 0x1fff); //  13 bits
            code |= imm;
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_K1: // T2_K1   ............nnnn ttttiiiiiiiiiiii       R1  R2             imm12
        case IF_T2_K4: // T2_K4   ........U....... ttttiiiiiiiiiiii       R1  PC          U, imm12
        case IF_T2_K3: // T2_K3   ........U....... ....iiiiiiiiiiii       PC              U, imm12
            sz   = id->emitGetInstrDescSize();
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            if (fmt != IF_T2_K3)
            {
                code |= insEncodeRegT2_T(id->idReg1());
            }
            if (fmt == IF_T2_K1)
            {
                code |= insEncodeRegT2_N(id->idReg2());
                assert(imm <= 0xfff); //  12 bits
                code |= imm;
            }
            else
            {
                assert(unsigned_abs(imm) <= 0xfff); //  12 bits (signed)
                code |= abs(imm);
                if (imm >= 0)
                    code |= (1 << 23); //  U bit
            }
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_K2: // T2_K2   ............nnnn ....iiiiiiiiiiii       R1                 imm12
            sz   = id->emitGetInstrDescSizeSC();
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_N(id->idReg1());
            assert(imm <= 0xfff); //  12 bits
            code |= imm;
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_L0: // T2_L0   .....i.....Snnnn .iiiddddiiiiiiii       R1  R2          S, imm8<<imm4
        case IF_T2_L1: // T2_L1   .....i.....S.... .iiiddddiiiiiiii       R1              S, imm8<<imm4
        case IF_T2_L2: // T2_L2   .....i......nnnn .iii....iiiiiiii       R1                 imm8<<imm4
            sz   = id->emitGetInstrDescSize();
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);

            if (fmt == IF_T2_L2)
                code |= insEncodeRegT2_N(id->idReg1());
            else
            {
                code |= insEncodeSetFlags(id->idInsFlags());
                code |= insEncodeRegT2_D(id->idReg1());
                if (fmt == IF_T2_L0)
                    code |= insEncodeRegT2_N(id->idReg2());
            }
            assert(isModImmConst(imm)); // Funky ARM imm encoding
            imm = encodeModImmConst(imm);
            assert(imm <= 0xfff); //  12 bits
            code |= (imm & 0x00ff);
            code |= (imm & 0x0700) << 4;
            code |= (imm & 0x0800) << 15;
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_M0: // T2_M0   .....i......nnnn .iiiddddiiiiiiii       R1  R2             imm12
            sz   = id->emitGetInstrDescSizeSC();
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            if (fmt == IF_T2_M0)
                code |= insEncodeRegT2_N(id->idReg2());
            imm = id->emitGetInsSC();
            assert(imm <= 0xfff); //  12 bits
            code |= (imm & 0x00ff);
            code |= (imm & 0x0700) << 4;
            code |= (imm & 0x0800) << 15;
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_N: // T2_N    .....i......iiii .iiiddddiiiiiiii       R1                 imm16
            assert(!id->idIsCnsReloc());
            sz   = id->emitGetInstrDescSizeSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            code |= insEncodeImmT2_Mov(id->emitGetInsSC());
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_N2: // T2_N2   .....i......iiii .iiiddddiiiiiiii       R1                 imm16
            sz   = id->emitGetInstrDescSizeSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            imm  = id->emitGetInsSC();
            addr = emitConsBlock + imm;
            if (!id->idIsCnsReloc())
            {
                assert(sizeof(size_t) == sizeof(target_size_t));
                imm = (target_size_t)(size_t)addr;
                if (ins == INS_movw)
                {
                    imm &= 0xffff;
                }
                else
                {
                    assert(ins == INS_movt);
                    imm = (imm >> 16) & 0xffff;
                }
                code |= insEncodeImmT2_Mov(imm);
                dst += emitOutput_Thumb2Instr(dst, code);
            }
            else
            {
                assert((ins == INS_movt) || (ins == INS_movw));
                dst += emitOutput_Thumb2Instr(dst, code);
                if ((ins == INS_movt) && emitComp->info.compMatchedVM)
                {
                    emitHandlePCRelativeMov32((void*)(dst - 8), addr);
                }
            }
            break;

        case IF_T2_N3: // T2_N3   .....i......iiii .iiiddddiiiiiiii       R1                 imm16
            assert((ins == INS_movt) || (ins == INS_movw));
            assert(id->idIsCnsReloc());

            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            dst += emitOutput_Thumb2Instr(dst, code);

            if ((ins == INS_movt) && emitComp->info.compMatchedVM)
            {
                emitHandlePCRelativeMov32(dst - 8, id->GetAddr());
            }

            sz = sizeof(instrDesc);
            break;

        case IF_T2_VFP3:
            // these are the binary operators
            // d = n - m
            sz   = id->emitGetInstrDescSize();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_VectorN(id->idReg2(), size, true);
            code |= insEncodeRegT2_VectorM(id->idReg3(), size, true);
            code |= insEncodeRegT2_VectorD(id->idReg1(), size, true);
            if (size == EA_8BYTE)
                code |= 1 << 8;
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_VFP2:
        {
            emitAttr srcSize;
            emitAttr dstSize;
            size_t   szCode = 0;

            switch (ins)
            {
                case INS_vcvt_i2d:
                case INS_vcvt_u2d:
                case INS_vcvt_f2d:
                    srcSize = EA_4BYTE;
                    dstSize = EA_8BYTE;
                    break;

                case INS_vcvt_d2i:
                case INS_vcvt_d2u:
                case INS_vcvt_d2f:
                    srcSize = EA_8BYTE;
                    dstSize = EA_4BYTE;
                    break;

                case INS_vmov:
                case INS_vabs:
                case INS_vsqrt:
                case INS_vcmp:
                case INS_vneg:
                    if (id->idOpSize() == EA_8BYTE)
                        szCode |= (1 << 8);
                    FALLTHROUGH;

                default:
                    srcSize = dstSize = id->idOpSize();
                    break;
            }

            sz   = id->emitGetInstrDescSize();
            code = emitInsCode(ins, fmt);
            code |= szCode;
            code |= insEncodeRegT2_VectorD(id->idReg1(), dstSize, true);
            code |= insEncodeRegT2_VectorM(id->idReg2(), srcSize, true);

            dst += emitOutput_Thumb2Instr(dst, code);
            break;
        }

        case IF_T2_VLDST:
            sz   = id->emitGetInstrDescSizeSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_N(id->idReg2());
            code |= insEncodeRegT2_VectorD(id->idReg1(), size, true);

            imm = id->emitGetInsSC();
            if (imm < 0)
                imm = -imm; // bit 23 at 0 means negate
            else
                code |= 1 << 23; // set the positive bit

            // offset is +/- 1020
            assert(!(imm % 4));
            assert(imm >> 10 == 0);
            code |= imm >> 2;
            // bit 8 is set for doubles
            if (id->idOpSize() == EA_8BYTE)
                code |= (1 << 8);
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_VMOVD:
            // 3op assemble a double from two int regs (or back)
            sz   = id->emitGetInstrDescSize();
            code = emitInsCode(ins, fmt);
            if (ins == INS_vmov_i2d)
            {
                code |= insEncodeRegT2_VectorM(id->idReg1(), size, true);
                code |= id->idReg2() << 12;
                code |= id->idReg3() << 16;
            }
            else
            {
                assert(ins == INS_vmov_d2i);
                code |= id->idReg1() << 12;
                code |= id->idReg2() << 16;
                code |= insEncodeRegT2_VectorM(id->idReg3(), size, true);
            }
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_VMOVS:
            // 2op assemble a float from one int reg (or back)
            sz   = id->emitGetInstrDescSize();
            code = emitInsCode(ins, fmt);
            if (ins == INS_vmov_f2i)
            {
                code |= insEncodeRegT2_VectorN(id->idReg2(), EA_4BYTE, true);
                code |= id->idReg1() << 12;
            }
            else
            {
                assert(ins == INS_vmov_i2f);
                code |= insEncodeRegT2_VectorN(id->idReg1(), EA_4BYTE, true);
                code |= id->idReg2() << 12;
            }

            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T1_D1: // T1_D1   .........mmmm...                       R1*

            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT1_M4(id->idReg1());
            dst += emitOutput_Thumb1Instr(dst, code);
            sz = sizeof(instrDescSmall);
            break;

        case IF_T1_D2: // T1_D2   .........mmmm...                                R3*
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT1_M4(id->idReg3());
            dst += emitOutput_Thumb1Instr(dst, code);
            sz = emitRecordGCCall(id, *dp, dst);
            break;

        case IF_T2_J3:                    // T2_J3   .....Siiiiiiiiii ..j.jiiiiiiiiii.      Call                imm24
            if (id->GetAddr() == nullptr) // a recursive call
            {
                addr = emitCodeBlock;
            }
            else
            {
                addr = id->GetAddr();
            }
            code = emitInsCode(ins, fmt);

            if (id->idIsCnsReloc())
            {
                dst += emitOutput_Thumb2Instr(dst, code);
                emitRecordRelocation((void*)(dst - 4), addr, IMAGE_REL_BASED_THUMB_BRANCH24);
            }
            else
            {
                addr = (uint8_t*)((size_t)addr & ~1); // Clear the lowest bit from target address

                // Calculate PC relative displacement
                ptrdiff_t disp = static_cast<uint8_t*>(addr) - (dst + 4);
                bool      S    = (disp < 0);
                bool      I1   = ((disp & 0x00800000) == 0);
                bool      I2   = ((disp & 0x00400000) == 0);

                if (S)
                    code |= (1 << 26); // S bit
                if (S ^ I1)
                    code |= (1 << 13); // J1 bit
                if (S ^ I2)
                    code |= (1 << 11); // J2 bit

                int immLo = (disp & 0x00000ffe) >> 1;
                int immHi = (disp & 0x003ff000) >> 12;

                code |= (immHi << 16);
                code |= immLo;

                disp = abs(disp);
                assert((disp & 0x00fffffe) == disp);

                dst += emitOutput_Thumb2Instr(dst, code);
            }

            sz = emitRecordGCCall(id, *dp, dst);
            break;

        /********************************************************************/
        /*                            oops                                  */
        /********************************************************************/

        default:
            unreached();
    }

    // Determine if any registers now hold GC refs, or whether a register that was overwritten held a GC ref.
    // We assume here that "id->idGCref()" is not GC_NONE only if the instruction described by "id" writes a
    // GC ref to register "id->idReg1()".  (It may, apparently, also not be GC_NONE in other cases, such as
    // for stores, but we ignore those cases here.)
    if (emitInsMayWriteToGCReg(id)) // True if "id->idIns()" writes to a register than can hold GC ref.
    {
        // If we ever generate instructions that write to multiple registers (LDM, or POP),
        // then we'd need to more work here to ensure that changes in the status of GC refs are
        // tracked properly.
        if (emitInsMayWriteMultipleRegs(id))
        {
            // We explicitly list the multiple-destination-target instruction that we expect to
            // be emitted outside of the prolog and epilog here.
            switch (ins)
            {
                case INS_smull:
                case INS_umull:
                case INS_smlal:
                case INS_umlal:
                case INS_vmov_d2i:
                    // For each of these, idReg1() and idReg2() are the destination registers.
                    emitGCregDeadUpd(id->idReg1(), dst);
                    emitGCregDeadUpd(id->idReg2(), dst);
                    break;
                default:
                    assert(false); // We need to recognize this multi-target instruction...
            }
        }
        else
        {
            if (id->idGCref() != GCT_NONE)
            {
                emitGCregLiveUpd(id->idGCref(), id->idReg1(), dst);
            }
            else
            {
                // I also assume that "idReg1" is the destination register of all instructions that write to registers.
                emitGCregDeadUpd(id->idReg1(), dst);
            }
        }
    }

    // Now we determine if the instruction has written to a (local variable) stack location, and either written a GC
    // ref or overwritten one.
    if (id->idIsLclVar() && (id->idAddr()->isTrackedGCSlotStore || id->idAddr()->isGCArgStore))
    {
        bool isArg = id->idAddr()->isGCArgStore;
        int  adr   = id->idAddr()->lclOffset;
        INDEBUG(unsigned varNum = id->idDebugOnlyInfo()->varNum);

        if (isArg)
        {
            emitGCargLiveUpd(adr, id->idGCref(), dst DEBUGARG(varNum));
        }
        else
        {
            emitGCvarLiveUpd(adr, id->idGCref(), dst DEBUGARG(varNum));
        }
    }

#ifdef DEBUG
    if ((emitComp->opts.disAsm || emitComp->verbose) && (*dp != dst))
    {
        bool dspOffs = emitComp->opts.dspGCtbls || !emitComp->opts.disDiffable;

        emit.emitDispIns(id, false, dspOffs, true, emitCurCodeOffs(*dp), *dp, dst - *dp);
    }
#endif

    *dp = dst;

    return sz;
}

#ifdef DEBUG

static bool insAlwaysSetFlags(instruction ins)
{
    bool result = false;
    switch (ins)
    {
        case INS_cmp:
        case INS_cmn:
        case INS_teq:
        case INS_tst:
            result = true;
            break;

        default:
            break;
    }
    return result;
}

class AsmPrinter
{
    using instrDesc    = Emitter::instrDesc;
    using instrDescJmp = Emitter::instrDescJmp;

    Compiler* compiler;
    Emitter*  emitter;

public:
    AsmPrinter(Emitter* emitter) : compiler(emitter->emitComp), emitter(emitter)
    {
    }

    void Print(instrDesc* id);

private:
    static const char* emitRegName(RegNum reg, emitAttr attr = EA_4BYTE)
    {
        return getRegName(reg);
    }

    void emitDispInst(instruction ins, insFlags flags);
    void emitDispImm(int imm, bool addComma, bool alwaysHex = false);
    void emitDispReloc(void* addr);
    void emitDispFrameRef(instrDesc* id);
    void emitDispCond(int cond);
    void emitDispShiftOpts(insOpts opt);
    void emitDispRegmask(int imm, bool encodedPC_LR);
    void emitDispRegRange(regNumber reg, int len, emitAttr attr);
    void emitDispReg(regNumber reg, emitAttr attr, bool addComma);
    void emitDispLabel(instrDescJmp* id);
    void emitDispAddrR(regNumber reg, emitAttr attr);
    void emitDispAddrRI(regNumber reg, int imm, emitAttr attr);
    void emitDispAddrRR(regNumber reg1, regNumber reg2, emitAttr attr);
    void emitDispAddrRRI(regNumber reg1, regNumber reg2, int imm, emitAttr attr);
    void emitDispAddrPUW(regNumber reg, int imm, insOpts opt, emitAttr attr);
    void emitDispGC(emitAttr attr);
};

// Display the instruction name, optionally the instruction can add the "s" suffix if it must set the flags.
void AsmPrinter::emitDispInst(instruction ins, insFlags flags)
{
    const char* insstr = insName(ins);
    size_t      len    = strlen(insstr);

    printf("%s", insstr);

    if (insSetsFlags(flags) && !insAlwaysSetFlags(ins))
    {
        printf("s");
        len++;
    }

    //
    // Add at least one space after the instruction name
    // and add spaces until we have reach the normal size of 8
    do
    {
        printf(" ");
        len++;
    } while (len < 8);
}

#define STRICT_ARM_ASM 0

void AsmPrinter::emitDispImm(int imm, bool addComma, bool alwaysHex)
{
    if (!alwaysHex && (imm > -1000) && (imm < 1000))
        printf("%d", imm);
    else if ((imm > 0) ||
             (imm == -imm) || // -0x80000000 == 0x80000000. So we don't want to add an extra "-" at the beginning.
             (compiler->opts.disDiffable && (imm == 0xD1FFAB1E))) // Don't display this as negative
        printf("0x%02x", imm);
    else // val <= -1000
        printf("-0x%02x", -imm);

    if (addComma)
        printf(", ");
}

void AsmPrinter::emitDispReloc(void* addr)
{
    printf("0x%p", dspPtr(addr));
}

void AsmPrinter::emitDispCond(int cond)
{
    const static char* armCond[16]{"eq", "ne", "hs", "lo", "mi", "pl", "vs", "vc",
                                   "hi", "ls", "ge", "lt", "gt", "le", "AL", "NV"}; // The last two are invalid
    assert(0 <= cond && (unsigned)cond < _countof(armCond));
    printf(armCond[cond]);
}

void AsmPrinter::emitDispRegRange(RegNum reg, int len, emitAttr attr)
{
    printf("{");
    emitDispReg(reg, attr, false);
    if (len > 1)
    {
        printf("-");
        emitDispReg((RegNum)(reg + len - 1), attr, false);
    }
    printf("}");
}

void AsmPrinter::emitDispRegmask(int imm, bool encodedPC_LR)
{
    bool printedOne = false;
    bool hasPC;
    bool hasLR;

    if (encodedPC_LR)
    {
        hasPC = (imm & 2) != 0;
        hasLR = (imm & 1) != 0;
        imm >>= 2;
    }
    else
    {
        hasPC = (imm & RBM_PC) != 0;
        hasLR = (imm & RBM_LR) != 0;
        imm &= ~(RBM_PC | RBM_LR);
    }

    RegNum   reg = REG_R0;
    unsigned bit = 1;

    printf("{");
    while (imm != 0)
    {
        if (bit & imm)
        {
            if (printedOne)
                printf(",");
            printf("%s", emitRegName(reg));
            printedOne = true;
            imm -= bit;
        }

        reg = RegNum(reg + 1);
        bit <<= 1;
    }

    if (hasLR)
    {
        if (printedOne)
            printf(",");
        printf("%s", emitRegName(REG_LR));
        printedOne = true;
    }

    if (hasPC)
    {
        if (printedOne)
            printf(",");
        printf("%s", emitRegName(REG_PC));
        printedOne = true;
    }
    printf("}");
}

const char* insOptsName(insOpts opt)
{
    switch (opt)
    {
        case INS_OPTS_NONE:
            return "";
        case INS_OPTS_RRX:
            return "RRX";
        case INS_OPTS_LSL:
            return "LSL";
        case INS_OPTS_LSR:
            return "LSR";
        case INS_OPTS_ASR:
            return "ASR";
        case INS_OPTS_ROR:
            return "ROR";
        default:
            return "???";
    }
}

void AsmPrinter::emitDispShiftOpts(insOpts opt)
{
    printf(" %s ", insOptsName(opt));
}

void AsmPrinter::emitDispReg(RegNum reg, emitAttr attr, bool addComma)
{
    if (isFloatReg(reg))
    {
        const char* size = attr == EA_8BYTE ? "d" : "s";
        printf("%s%s", size, emitRegName(reg, attr) + 1);
    }
    else
    {
        printf("%s", emitRegName(reg, attr));
    }

    if (addComma)
        printf(", ");
}

void AsmPrinter::emitDispLabel(instrDescJmp* id)
{
    insFormat fmt = id->idInsFmt();

    if ((fmt == IF_T1_I) || (fmt == IF_T1_J3) || (fmt == IF_T2_M1))
    {
        emitDispReg(id->idReg1(), EA_4BYTE, true);
    }
    else if (fmt == IF_T2_N1)
    {
        emitDispReg(id->idReg1(), EA_4BYTE, true);
        printf("%s ADDRESS ", id->idIns() == INS_movw ? "LOW" : "HIGH");
    }
    else if ((fmt == IF_T1_K) || (fmt == IF_T1_M))
    {
        printf("SHORT ");
    }

    if (id->HasInstrCount())
    {
        if (id->idjIG == nullptr)
        {
            // This is the instruction synthesized by emitDispIns, we can't get
            // its number because it's not part of an actual instruction group.
            printf("pc%s%d instructions", emitter->instrCount >= 0 ? "+" : "", emitter->instrCount);
        }
        else
        {
            unsigned instrNum   = emitter->emitFindInsNum(id->idjIG, id);
            uint32_t instrOffs  = id->idjIG->igOffs + id->idjOffs;
            int      instrCount = id->GetInstrCount();
            uint32_t labelOffs  = id->idjIG->igOffs + id->idjIG->FindInsOffset(instrNum + 1 + instrCount);
            ssize_t  distance   = emitter->emitOffsetToPtr(labelOffs) - emitter->emitOffsetToPtr(instrOffs) - 2;

            printf("pc%s%d (%d instructions)", distance >= 0 ? "+" : "", distance, instrCount);
        }
    }
    else
    {
        emitter->emitPrintLabel(id->GetLabel());
    }
}

void AsmPrinter::emitDispAddrR(RegNum reg, emitAttr attr)
{
    printf("[");
    emitDispReg(reg, attr, false);
    printf("]");
    emitDispGC(attr);
}

void AsmPrinter::emitDispAddrRI(RegNum reg, int imm, emitAttr attr)
{
    bool regIsSPorFP = (reg == REG_SP) || (reg == REG_FP);

    printf("[");
    emitDispReg(reg, attr, false);
    if (imm != 0)
    {
        if (imm >= 0)
        {
#if STRICT_ARM_ASM
            printf(", ");
#else
            printf("+");
#endif
        }
        emitDispImm(imm, false, regIsSPorFP);
    }
    printf("]");
    emitDispGC(attr);
}

void AsmPrinter::emitDispAddrRR(RegNum reg1, RegNum reg2, emitAttr attr)
{
    printf("[");
    emitDispReg(reg1, attr, false);
#if STRICT_ARM_ASM
    printf(", ");
#else
    printf("+");
#endif
    emitDispReg(reg2, attr, false);
    printf("]");
    emitDispGC(attr);
}

void AsmPrinter::emitDispAddrRRI(RegNum reg1, RegNum reg2, int imm, emitAttr attr)
{
    printf("[");
    emitDispReg(reg1, attr, false);
#if STRICT_ARM_ASM
    printf(", ");
    emitDispReg(reg2, attr, false);
    if (imm > 0)
    {
        printf(" LSL ");
        emitDispImm(1 << imm, false);
    }
#else
    printf("+");
    if (imm > 0)
    {
        emitDispImm(1 << imm, false);
        printf("*");
    }
    emitDispReg(reg2, attr, false);
#endif
    printf("]");
    emitDispGC(attr);
}

void AsmPrinter::emitDispAddrPUW(RegNum reg, int imm, insOpts opt, emitAttr attr)
{
    bool regIsSPorFP = (reg == REG_SP) || (reg == REG_FP);

    printf("[");
    emitDispReg(reg, attr, false);
    if (insOptAnyInc(opt))
        printf("!");

    if (imm != 0)
    {
        if (imm >= 0)
        {
#if STRICT_ARM_ASM
            printf(", ");
#else
            printf("+");
#endif
        }
        emitDispImm(imm, false, regIsSPorFP);
    }
    printf("]");

    emitDispGC(attr);
}

void AsmPrinter::emitDispGC(emitAttr attr)
{
#if 0
    // TODO-ARM-Cleanup: Fix or delete.
    if (attr == EA_GCREF)
        printf(" @gc");
    else if (attr == EA_BYREF)
        printf(" @byref");
#endif
}

void emitter::emitDispInsHex(instrDesc* id, uint8_t* code, size_t sz)
{
    // We do not display the instruction hex if we want diff-able disassembly
    if (!emitComp->opts.disDiffable)
    {
        if (sz == 2)
        {
            printf("  %04X     ", (*((unsigned short*)code)));
        }
        else if (sz == 4)
        {
            printf("  %04X %04X", (*((unsigned short*)(code + 0))), (*((unsigned short*)(code + 2))));
        }
        else
        {
            assert(sz == 0);

            // At least display the encoding size of the instruction, even if not displaying its actual encoding.
            insSize isz = emitInsSize(id->idInsFmt());
            switch (isz)
            {
                case ISZ_16BIT:
                    printf("  2B");
                    break;
                case ISZ_32BIT:
                    printf("  4B");
                    break;
                case ISZ_48BIT:
                    printf("  6B");
                    break;
                default:
                    unreached();
            }
        }
    }
}

void emitter::emitDispInsHelp(
    instrDesc* id, bool isNew, bool doffs, bool asmfm, unsigned offset, uint8_t* code, size_t sz)
{
    JITDUMP("IN%04X: ", id->idDebugOnlyInfo()->idNum);

    assert(!isNew || (static_cast<int>(id->GetDescSize()) == emitCurIGfreeNext - reinterpret_cast<uint8_t*>(id)));

    if (code == nullptr)
    {
        sz = 0;
    }

    if (!isNew && !asmfm && sz)
    {
        doffs = true;
    }

    emitDispInsAddr(code);
    emitDispInsOffs(offset, doffs);
    emitDispInsHex(id, code, sz);

    AsmPrinter printer(this);
    printer.Print(id);
}

void AsmPrinter::Print(instrDesc* id)
{
    printf("      ");

    instruction ins = id->idIns();
    insFormat   fmt = id->idInsFmt();

    emitDispInst(ins, id->idInsFlags());

    emitAttr attr;

    if (id->idGCref() == GCT_GCREF)
    {
        attr = EA_GCREF;
    }
    else if (id->idGCref() == GCT_BYREF)
    {
        attr = EA_BYREF;
    }
    else
    {
        attr = id->idOpSize();
    }

    switch (fmt)
    {
        int         imm;
        const char* methodName;

        case IF_T1_A: // None
        case IF_T2_A:
            break;

        case IF_T1_L0: // Imm
        case IF_T2_B:
            emitDispImm(id->emitGetInsSC(), false);
            break;

        case IF_T1_B: // <cond>
            emitDispCond(id->emitGetInsSC());
            break;

        case IF_T1_L1: // <regmask8>
        case IF_T2_I1: // <regmask16>
            emitDispRegmask(id->emitGetInsSC(), true);
            break;

        case IF_T2_E2: // Reg
            if (id->idIns() == INS_vmrs)
            {
                if (id->idReg1() != REG_R15)
                {
                    emitDispReg(id->idReg1(), attr, true);
                    printf("FPSCR");
                }
                else
                {
                    printf("APSR, FPSCR");
                }
            }
            else
            {
                emitDispReg(id->idReg1(), attr, false);
            }
            break;

        case IF_T1_D1:
            emitDispReg(id->idReg1(), attr, false);
            break;

        case IF_T1_D2:
            emitDispReg(id->idReg3(), attr, false);
            if (CORINFO_METHOD_HANDLE handle = static_cast<CORINFO_METHOD_HANDLE>(id->idDebugOnlyInfo()->idHandle))
            {
                methodName = compiler->eeGetMethodFullName(handle);
                printf("\t\t// %s", methodName);
            }
            break;

        case IF_T1_F: // SP, Imm
            emitDispReg(REG_SP, attr, true);
            emitDispImm(id->emitGetInsSC(), false);
            break;

        case IF_T1_J0: // Reg, Imm
        case IF_T2_L1:
        case IF_T2_L2:
            emitDispReg(id->idReg1(), attr, true);
            imm = id->emitGetInsSC();
            emitDispImm(imm, false, false);
            break;

        case IF_T2_N:
            emitDispReg(id->idReg1(), attr, true);
            imm = compiler->opts.disDiffable ? 0xD1FF : id->emitGetInsSC();
            emitDispImm(imm, false, true);
            break;

        case IF_T2_N3:
            emitDispReg(id->idReg1(), attr, true);
            printf("%s RELOC ", (id->idIns() == INS_movw) ? "LOW" : "HIGH");
            emitDispReloc(id->GetAddr());
            break;

        case IF_T2_N2:
            emitDispReg(id->idReg1(), attr, true);
            printf("%s RWD%02u", id->idIns() == INS_movw ? "LOW" : "HIGH", id->emitGetInsSC());
            break;

        case IF_T2_H2: // [Reg+imm]
        case IF_T2_K2:
            emitDispAddrRI(id->idReg1(), id->emitGetInsSC(), attr);
            break;

        case IF_T2_K3: // [PC+imm]
            emitDispAddrRI(REG_PC, id->emitGetInsSC(), attr);
            break;

        case IF_T1_J1: // reg, <regmask8>
        case IF_T2_I0: // reg, <regmask16>
            emitDispReg(id->idReg1(), attr, false);
            printf("!, ");
            emitDispRegmask(id->emitGetInsSC(), false);
            break;

        case IF_T1_D0: // Reg, Reg
        case IF_T1_E:
        case IF_T2_C3:
        case IF_T2_C9:
        case IF_T2_C10:
            emitDispReg(id->idReg1(), attr, true);
            emitDispReg(id->idReg2(), attr, false);
            if (fmt == IF_T1_E && id->idIns() == INS_rsb)
            {
                printf(", 0");
            }
            break;

        case IF_T2_E1: // Reg, [Reg]
            emitDispReg(id->idReg1(), attr, true);
            emitDispAddrR(id->idReg2(), attr);
            break;

        case IF_T2_D1: // Reg, Imm, Imm
            emitDispReg(id->idReg1(), attr, true);
            imm = id->emitGetInsSC();
            {
                int lsb  = (imm >> 5) & 0x1f;
                int msb  = imm & 0x1f;
                int imm1 = lsb;
                int imm2 = msb + 1 - lsb;
                emitDispImm(imm1, true);
                emitDispImm(imm2, false);
            }
            break;

        case IF_T1_C: // Reg, Reg, Imm
        case IF_T1_G:
        case IF_T2_C2:
        case IF_T2_H1:
        case IF_T2_K1:
        case IF_T2_L0:
        case IF_T2_M0:
            emitDispReg(id->idReg1(), attr, true);
            imm = id->emitGetInsSC();
            if (emitInsIsLoadOrStore(ins))
            {
                emitDispAddrRI(id->idReg2(), imm, attr);
            }
            else
            {
                emitDispReg(id->idReg2(), attr, true);
                emitDispImm(imm, false);
            }
            break;

        case IF_T1_J2:
            emitDispReg(id->idReg1(), attr, true);
            imm = id->emitGetInsSC();
            if (emitInsIsLoadOrStore(ins))
            {
                emitDispAddrRI(REG_SP, imm, attr);
            }
            else
            {
                emitDispReg(REG_SP, attr, true);
                emitDispImm(imm, false);
            }
            break;

        case IF_T2_K4:
            emitDispReg(id->idReg1(), attr, true);
            emitDispAddrRI(REG_PC, id->emitGetInsSC(), attr);
            break;

        case IF_T2_C1:
        case IF_T2_C8:
            emitDispReg(id->idReg1(), attr, true);
            emitDispReg(id->idReg2(), attr, false);
            imm = id->emitGetInsSC();
            if (id->idInsOpt() == INS_OPTS_RRX)
            {
                emitDispShiftOpts(id->idInsOpt());
                assert(imm == 1);
            }
            else if (imm > 0)
            {
                emitDispShiftOpts(id->idInsOpt());
                emitDispImm(imm, false);
            }
            break;

        case IF_T2_C6:
            imm = id->emitGetInsSC();
            emitDispReg(id->idReg1(), attr, true);
            emitDispReg(id->idReg2(), attr, (imm != 0));
            if (imm != 0)
            {
                emitDispImm(imm, false);
            }
            break;

        case IF_T2_C7:
            emitDispAddrRRI(id->idReg1(), id->idReg2(), id->emitGetInsSC(), attr);
            break;

        case IF_T2_H0:
            emitDispReg(id->idReg1(), attr, true);
            emitDispAddrPUW(id->idReg2(), id->emitGetInsSC(), id->idInsOpt(), attr);
            break;

        case IF_T1_H: // Reg, Reg, Reg
            emitDispReg(id->idReg1(), attr, true);
            if (emitInsIsLoadOrStore(ins))
            {
                emitDispAddrRR(id->idReg2(), id->idReg3(), attr);
            }
            else
            {
                emitDispReg(id->idReg2(), attr, true);
                emitDispReg(id->idReg3(), attr, false);
            }
            break;

        case IF_T2_C4:
        case IF_T2_C5:
            emitDispReg(id->idReg1(), attr, true);
            emitDispReg(id->idReg2(), attr, true);
            emitDispReg(id->idReg3(), attr, false);
            break;

        case IF_T2_VFP3:
            emitDispReg(id->idReg1(), attr, true);
            emitDispReg(id->idReg2(), attr, true);
            emitDispReg(id->idReg3(), attr, false);
            break;

        case IF_T2_VFP2:
            switch (id->idIns())
            {
                case INS_vcvt_d2i:
                case INS_vcvt_d2u:
                case INS_vcvt_d2f:
                    emitDispReg(id->idReg1(), EA_4BYTE, true);
                    emitDispReg(id->idReg2(), EA_8BYTE, false);
                    break;

                case INS_vcvt_i2d:
                case INS_vcvt_u2d:
                case INS_vcvt_f2d:
                    emitDispReg(id->idReg1(), EA_8BYTE, true);
                    emitDispReg(id->idReg2(), EA_4BYTE, false);
                    break;

                // we just use the type on the instruction
                // unless it is an asymmetrical one like the converts
                default:
                    emitDispReg(id->idReg1(), attr, true);
                    emitDispReg(id->idReg2(), attr, false);
                    break;
            }
            break;

        case IF_T2_VLDST:
            imm = id->emitGetInsSC();
            switch (id->idIns())
            {
                case INS_vldr:
                case INS_vstr:
                    emitDispReg(id->idReg1(), attr, true);
                    emitDispAddrPUW(id->idReg2(), imm, id->idInsOpt(), attr);
                    break;

                case INS_vldm:
                case INS_vstm:
                    emitDispReg(id->idReg2(), attr, false);
                    if (insOptAnyInc(id->idInsOpt()))
                        printf("!");
                    printf(", ");
                    emitDispRegRange(id->idReg1(), abs(imm) >> 2, attr);
                    break;

                case INS_vpush:
                case INS_vpop:
                    emitDispRegRange(id->idReg1(), abs(imm) >> 2, attr);
                    break;

                default:
                    unreached();
            }
            break;

        case IF_T2_VMOVD:
            switch (id->idIns())
            {
                case INS_vmov_i2d:
                    emitDispReg(id->idReg1(), attr, true); // EA_8BYTE
                    emitDispReg(id->idReg2(), EA_4BYTE, true);
                    emitDispReg(id->idReg3(), EA_4BYTE, false);
                    break;
                case INS_vmov_d2i:
                    emitDispReg(id->idReg1(), EA_4BYTE, true);
                    emitDispReg(id->idReg2(), EA_4BYTE, true);
                    emitDispReg(id->idReg3(), attr, false); // EA_8BYTE
                    break;
                default:
                    unreached();
            }
            break;

        case IF_T2_VMOVS:
            emitDispReg(id->idReg1(), attr, true);
            emitDispReg(id->idReg2(), attr, false);
            break;

        case IF_T2_G1:
            emitDispReg(id->idReg1(), attr, true);
            emitDispAddrRR(id->idReg2(), id->idReg3(), attr);
            break;

        case IF_T2_D0: // Reg, Reg, Imm, Imm
            emitDispReg(id->idReg1(), attr, true);
            emitDispReg(id->idReg2(), attr, true);
            imm = id->emitGetInsSC();
            if (ins == INS_bfi)
            {
                int lsb  = (imm >> 5) & 0x1f;
                int msb  = imm & 0x1f;
                int imm1 = lsb;
                int imm2 = msb + 1 - lsb;
                emitDispImm(imm1, true);
                emitDispImm(imm2, false);
            }
            else
            {
                int lsb     = (imm >> 5) & 0x1f;
                int widthm1 = imm & 0x1f;
                int imm1    = lsb;
                int imm2    = widthm1 + 1;
                emitDispImm(imm1, true);
                emitDispImm(imm2, false);
            }
            break;

        case IF_T2_C0: // Reg, Reg, Reg, Imm
            emitDispReg(id->idReg1(), attr, true);
            emitDispReg(id->idReg2(), attr, true);
            emitDispReg(id->idReg3(), attr, false);
            imm = id->emitGetInsSC();
            if (id->idInsOpt() == INS_OPTS_RRX)
            {
                emitDispShiftOpts(id->idInsOpt());
                assert(imm == 1);
            }
            else if (imm > 0)
            {
                emitDispShiftOpts(id->idInsOpt());
                emitDispImm(imm, false);
            }
            break;

        case IF_T2_E0:
            emitDispReg(id->idReg1(), attr, true);
            if (id->idIsLclVar())
            {
                emitDispAddrRRI(id->idReg2(), emitter->codeGen->rsGetRsvdReg(), 0, attr);
            }
            else
            {
                emitDispAddrRRI(id->idReg2(), id->idReg3(), id->emitGetInsSC(), attr);
            }
            break;

        case IF_T2_G0:
            emitDispReg(id->idReg1(), attr, true);
            emitDispReg(id->idReg2(), attr, true);
            emitDispAddrPUW(id->idReg3(), id->emitGetInsSC(), id->idInsOpt(), attr);
            break;

        case IF_T2_F1: // Reg, Reg, Reg, Reg
        case IF_T2_F2:
            emitDispReg(id->idReg1(), attr, true);
            emitDispReg(id->idReg2(), attr, true);
            emitDispReg(id->idReg3(), attr, true);
            emitDispReg(id->idReg4(), attr, false);
            break;

        case IF_T1_I:
        case IF_T1_K:
        case IF_T1_M:
        case IF_T2_N1:
        case IF_T2_J1:
        case IF_T2_J2:
        case IF_T1_J3:
        case IF_T2_M1:
        case IF_LARGEJMP:
            emitDispLabel(static_cast<instrDescJmp*>(id));
            break;

        case IF_T2_J3:
            printf("%s",
                   compiler->eeGetMethodFullName(static_cast<CORINFO_METHOD_HANDLE>(id->idDebugOnlyInfo()->idHandle)));
            break;

        default:
            printf("unexpected format %s", Emitter::emitIfName(id->idInsFmt()));
            assert(!"unexpectedFormat");
            break;
    }

    if (id->idIsLclVar())
    {
        emitDispFrameRef(id);
    }

    printf("\n");
}

void emitter::emitDispIns(instrDesc* id, bool isNew, bool doffs, bool asmfm, unsigned offset, uint8_t* code, size_t sz)
{
    insFormat fmt = id->idInsFmt();

    if (fmt == IF_GC_REG)
    {
        return;
    }

    // Special-case IF_LARGEJMP

    if ((fmt == IF_LARGEJMP) && static_cast<instrDescJmp*>(id)->HasLabel())
    {
        // This is a pseudo-instruction format representing a large conditional branch. See the comment
        // in emitter::emitOutputLJ() for the full description.
        //
        // For this pseudo-instruction, we will actually generate:
        //
        //      b<!cond> L_not  // 2 bytes. Note that we reverse the condition.
        //      b L_target      // 4 bytes
        //   L_not:
        //
        // These instructions don't exist in the actual instruction stream, so we need to fake them
        // up to display them.
        //
        // Note: don't touch the actual instrDesc. If we accidentally messed it up, it would create a very
        // difficult to find bug.

        instrDescJmp* ij = static_cast<instrDescJmp*>(id);
        instrDescJmp  idJmp;

        memset(&idJmp, 0, sizeof(idJmp));
        idJmp.idIns(emitJumpKindToBranch(emitReverseJumpKind(BranchToJumpKind(id->idIns()))));
        idJmp.idInsFmt(IF_T1_K);
        idJmp.idInsSize(ISZ_16BIT);
        idJmp.SetInstrCount(1);
        idJmp.idDebugOnlyInfo(id->idDebugOnlyInfo()); // share the idDebugOnlyInfo() field

        size_t bcondSizeOrZero = (code == NULL) ? 0 : 2; // branch is 2 bytes
        emitDispInsHelp(&idJmp, false, doffs, asmfm, offset, code, bcondSizeOrZero);

        code += bcondSizeOrZero;
        offset += 2;

        // Next, display the unconditional branch

        memset(&idJmp, 0, sizeof(idJmp));
        idJmp.idIns(INS_b);
        idJmp.idInsFmt(IF_T2_J2);
        idJmp.idInsSize(ISZ_32BIT);
        idJmp.SetLabel(ij->GetLabel());
        idJmp.idDebugOnlyInfo(id->idDebugOnlyInfo()); // share the idDebugOnlyInfo() field

        size_t brSizeOrZero = (code == NULL) ? 0 : 4; // unconditional branch is 4 bytes
        emitDispInsHelp(&idJmp, false, doffs, asmfm, offset, code, brSizeOrZero);
    }
    else
    {
        emitDispInsHelp(id, isNew, doffs, asmfm, offset, code, sz);
    }
}

void EmitterBase::PrintAlignmentBoundary(size_t           instrAddr,
                                         size_t           instrEndAddr,
                                         const instrDesc* instr,
                                         const instrDesc*)
{
    const size_t alignment    = emitComp->opts.compJitAlignLoopBoundary;
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

void AsmPrinter::emitDispFrameRef(instrDesc* id)
{
    int varNum  = id->idDebugOnlyInfo()->varNum;
    int varOffs = id->idDebugOnlyInfo()->varOffs;

    printf("\t// [");

    if (varNum < 0)
    {
        printf("T%02d", -varNum);
    }
    else
    {
        compiler->gtDispLclVar(static_cast<unsigned>(varNum), false);
    }

    if (varOffs != 0)
    {
        printf("%c0x%02x", varOffs < 0 ? '-' : '+', abs(varOffs));
    }

    printf("]");
}

#endif // DEBUG

#if defined(DEBUG) || defined(LATE_DISASM)

template <>
emitter::insExecutionCharacteristics emitter::Encoder<emitter>::getInsExecutionCharacteristics(instrDesc* id)
{
    insExecutionCharacteristics result;

    instruction ins    = id->idIns();
    insFormat   insFmt = id->idInsFmt();

    // ToDo: Calculate actual throughput and latency values
    //
    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
    result.insLatency    = PERFSCORE_LATENCY_1C;

    return result;
}

#endif // defined(DEBUG) || defined(LATE_DISASM)

#endif // TARGET_ARM
