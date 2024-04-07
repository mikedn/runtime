// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_ARM

#include "instr.h"
#include "emit.h"
#include "codegen.h"
#include "unwind.h"

enum ID_OPS : uint8_t
{
    ID_OP_NONE,
    ID_OP_JMP,
    ID_OP_CALL
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
size_t instrDescSmall::GetDescSize() const
{
    if (_idSmallDsc)
    {
        return sizeof(instrDescSmall);
    }

    if (_idLargeCns)
    {
        return sizeof(instrDescCns);
    }

    if (_idLargeCall)
    {
        return sizeof(instrDescCGCA);
    }

    if (GetFormatOp(_idInsFmt) == ID_OP_JMP)
    {
        return sizeof(instrDescJmp);
    }

    return sizeof(instrDesc);
}

int32_t instrDesc::emitGetInsSC() const
{
    return _idLargeCns ? static_cast<const instrDescCns*>(this)->idcCnsVal : _idSmallCns;
}

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
    return flags == INS_FLAGS_SET ? INS_FLAGS_SET : INS_FLAGS_NOT_SET;
}

static bool insOptsNone(insOpts opt)
{
    return opt == INS_OPTS_NONE;
}

static bool insOptAnyInc(insOpts opt)
{
    return (opt == INS_OPTS_LDST_PRE_DEC) || (opt == INS_OPTS_LDST_POST_INC);
}

static unsigned unsigned_abs(int x)
{
    return static_cast<unsigned>(x < 0 ? -x : x);
}

#ifdef DEBUG
static bool insOptsPostInc(insOpts opt)
{
    return opt == INS_OPTS_LDST_POST_INC;
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

static bool offsetFitsInVectorMem(int disp)
{
    unsigned imm = unsigned_abs(disp);
    return ((imm & 0x03fc) == imm);
}

static bool IsDoubleReg(RegNum reg)
{
    return IsFloatReg(reg) && ((reg % 2) == 0);
}

static bool insOptAnyShift(insOpts opt)
{
    return (opt >= INS_OPTS_RRX) && (opt <= INS_OPTS_ROR);
}

static bool isModImmConst(int val32);
static int insUnscaleImm(instruction ins, int imm);

void EmitterBase::emitInsSanityCheck(instrDesc* id)
{
    switch (id->idInsFmt())
    {
        case IF_T1_A: // ................
        case IF_T2_A: // ................ ................
            break;

        case IF_T1_K:  // ....cccciiiiiiii                        Branch             imm8, cond4
        case IF_T1_M:  // .....iiiiiiiiiii                        Branch             imm11
        case IF_T2_J1: // .....Scccciiiiii ..j.jiiiiiiiiiii       Branch             imm20, cond4
        case IF_T2_J2: // .....Siiiiiiiiii ..j.jiiiiiiiiii.       Branch             imm24
        case IF_T2_N1: // .....i......iiii .iiiddddiiiiiiii       R1                 imm16
        case IF_T2_J3: // .....Siiiiiiiiii ..j.jiiiiiiiiii.       Call               imm24
        case IF_LARGEJMP:
            break;

        case IF_T1_B: // ........cccc....                                           cond
        case IF_T2_B: // ................ ............iiii                          imm4
            assert(id->emitGetInsSC() < 0x10);
            break;

        case IF_T1_C: // .....iiiiinnnddd                       R1  R2              imm5
            assert(IsLowRegister(id->idReg1()));
            assert(IsLowRegister(id->idReg2()));
            assert(insUnscaleImm(id->idIns(), id->emitGetInsSC()) < 0x20);
            break;

        case IF_T1_D0: // ........Dmmmmddd                       R1* R2*
            assert(IsGeneralRegister(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));
            break;

        case IF_T1_D1: // .........mmmm...                       R1*
            assert(IsGeneralRegister(id->idReg1()));
            break;

        case IF_T1_D2: // .........mmmm...                               R3*
            assert(IsGeneralRegister(id->idReg3()));
            break;

        case IF_T1_E: // ..........nnnddd                       R1  R2
            assert(IsLowRegister(id->idReg1()));
            assert(IsLowRegister(id->idReg2()));
            assert(id->idSmallCns() < 0x20);
            break;

        case IF_T1_F: // .........iiiiiii                       SP                  imm7
            assert(id->idReg1() == REG_SP);
            assert(id->idOpSize() == EA_4BYTE);
            assert((id->emitGetInsSC() & ~0x1FC) == 0);
            break;

        case IF_T1_G: // .......iiinnnddd                       R1  R2              imm3
            assert(IsLowRegister(id->idReg1()));
            assert(IsLowRegister(id->idReg2()));
            assert(id->idSmallCns() < 0x8);
            break;

        case IF_T1_H: // .......mmmnnnddd                       R1  R2  R3
            assert(IsLowRegister(id->idReg1()));
            assert(IsLowRegister(id->idReg2()));
            assert(IsLowRegister(id->idReg3()));
            break;

        case IF_T1_I: // ......i.iiiiiddd                       R1                  imm6
            assert(IsLowRegister(id->idReg1()));
            break;

        case IF_T1_J0: // .....dddiiiiiiii                       R1                  imm8
            assert(IsLowRegister(id->idReg1()));
            assert(id->emitGetInsSC() < 0x100);
            break;

        case IF_T1_J1: // .....dddiiiiiiii                       R1                  <regmask8>
            assert(IsLowRegister(id->idReg1()));
            assert(id->emitGetInsSC() < 0x100);
            break;

        case IF_T1_J2: // .....dddiiiiiiii                       R1  SP              imm8
            assert(IsLowRegister(id->idReg1()));
            assert(id->idReg2() == REG_SP);
            assert(id->idOpSize() == EA_4BYTE);
            assert((id->emitGetInsSC() & ~0x3FC) == 0);
            break;

        case IF_T1_L0: // ........iiiiiiii                                           imm8
            assert(id->emitGetInsSC() < 0x100);
            break;

        case IF_T1_L1: // .......Rrrrrrrrr                                           <regmask8+2>
            assert(id->emitGetInsSC() < 0x400);
            break;

        case IF_T2_C0: // ...........Snnnn .iiiddddiishmmmm       R1  R2  R3      S, imm5, sh
            assert(IsGeneralRegister(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));
            assert(IsGeneralRegister(id->idReg3()));
            assert(id->emitGetInsSC() < 0x20);
            break;

        case IF_T2_C4: // ...........Snnnn ....dddd....mmmm       R1  R2  R3      S
        case IF_T2_C5: // ............nnnn ....dddd....mmmm       R1  R2  R3
        case IF_T2_G1: // ............nnnn ttttTTTT........       R1  R2  R3
            assert(IsGeneralRegister(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));
            assert(IsGeneralRegister(id->idReg3()));
            break;

        case IF_T2_C1: // ...........S.... .iiiddddiishmmmm       R1  R2          S, imm5, sh
        case IF_T2_C2: // ...........S.... .iiiddddii..mmmm       R1  R2          S, imm5
        case IF_T2_C8: // ............nnnn .iii....iishmmmm       R1  R2             imm5, sh
            assert(IsGeneralRegister(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));
            assert(id->emitGetInsSC() < 0x20);
            break;

        case IF_T2_C6: // ................ ....dddd..iimmmm       R1  R2                   imm2
        case IF_T2_C7: // ............nnnn ..........shmmmm       R1  R2                   imm2
            assert(IsGeneralRegister(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));
            assert(id->emitGetInsSC() < 0x4);
            break;

        case IF_T2_C3:  // ...........S.... ....dddd....mmmm       R1  R2          S
        case IF_T2_C9:  // ............nnnn ............mmmm       R1  R2
        case IF_T2_C10: // ............mmmm ....dddd....mmmm       R1  R2
            assert(IsGeneralRegister(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));
            break;

        case IF_T2_D0: // ............nnnn .iiiddddii.wwwww       R1  R2             imm5, imm5
            assert(IsGeneralRegister(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));
            assert(id->emitGetInsSC() < 0x400);
            break;

        case IF_T2_D1: // ................ .iiiddddii.wwwww       R1                 imm5, imm5
            assert(IsGeneralRegister(id->idReg1()));
            assert(id->emitGetInsSC() < 0x400);
            break;

        case IF_T2_E0: // ............nnnn tttt......shmmmm       R1  R2  R3               imm2
            assert(IsGeneralRegister(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));

            if (id->idIsLclVar())
            {
                assert(IsGeneralRegister(codeGen->rsGetRsvdReg()));
            }
            else
            {
                assert(IsGeneralRegister(id->idReg3()));
                assert(id->emitGetInsSC() < 0x4);
            }
            break;

        case IF_T2_E1: // ............nnnn tttt............       R1  R2
            assert(IsGeneralRegister(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));
            break;

        case IF_T2_E2: // ................ tttt............       R1
            assert(IsGeneralRegister(id->idReg1()));
            break;

        case IF_T2_F1: // ............nnnn ttttdddd....mmmm       R1  R2  R3  R4
        case IF_T2_F2: // ............nnnn aaaadddd....mmmm       R1  R2  R3  R4
            assert(IsGeneralRegister(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));
            assert(IsGeneralRegister(id->idReg3()));
            assert(IsGeneralRegister(id->idReg4()));
            break;

        case IF_T2_G0: // .......PU.W.nnnn ttttTTTTiiiiiiii       R1  R2  R3         imm8, PUW
            assert(IsGeneralRegister(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));
            assert(IsGeneralRegister(id->idReg3()));
            assert(unsigned_abs(id->emitGetInsSC()) < 0x100);
            break;

        case IF_T2_H0: // ............nnnn tttt.PUWiiiiiiii       R1  R2             imm8, PUW
            assert(IsGeneralRegister(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));
            assert(unsigned_abs(id->emitGetInsSC()) < 0x100);
            break;

        case IF_T2_H1: // ............nnnn tttt....iiiiiiii       R1  R2             imm8
            assert(IsGeneralRegister(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));
            assert(id->emitGetInsSC() < 0x100);
            break;

        case IF_T2_H2: // ............nnnn ........iiiiiiii       R1                 imm8
            assert(IsGeneralRegister(id->idReg1()));
            assert(id->emitGetInsSC() < 0x100);
            break;

        case IF_T2_I0: // ..........W.nnnn rrrrrrrrrrrrrrrr       R1              W, imm16
            assert(IsGeneralRegister(id->idReg1()));
            assert(id->emitGetInsSC() < 0x10000);
            break;

        case IF_T2_N: // .....i......iiii .iiiddddiiiiiiii       R1                 imm16
            assert(IsGeneralRegister(id->idReg1()));
            assert(!id->idIsCnsReloc());
            break;

        case IF_T2_N2: // .....i......iiii .iiiddddiiiiiiii       R1                 imm16
            assert(IsGeneralRegister(id->idReg1()));
            assert((size_t)id->emitGetInsSC() < roData.size);
            break;

        case IF_T2_N3: // .....i......iiii .iiiddddiiiiiiii       R1                 imm16
            assert(IsGeneralRegister(id->idReg1()));
            assert(id->idIsCnsReloc());
            break;

        case IF_T2_I1: // ................ rrrrrrrrrrrrrrrr                          imm16
            assert(id->emitGetInsSC() < 0x10000);
            break;

        case IF_T2_K1: // ............nnnn ttttiiiiiiiiiiii       R1  R2             imm12
        case IF_T2_M0: // .....i......nnnn .iiiddddiiiiiiii       R1  R2             imm12
            assert(IsGeneralRegister(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));
            assert(id->emitGetInsSC() < 0x1000);
            break;

        case IF_T2_L0: // .....i.....Snnnn .iiiddddiiiiiiii       R1  R2          S, imm8<<imm4
            assert(IsGeneralRegister(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));
            assert(isModImmConst(id->emitGetInsSC()));
            break;

        case IF_T2_K4: // ........U....... ttttiiiiiiiiiiii       R1  PC          U, imm12
        case IF_T2_M1: // .....i.......... .iiiddddiiiiiiii       R1  PC             imm12
            assert(IsGeneralRegister(id->idReg1()));
            assert(id->idReg2() == REG_PC);
            assert(id->emitGetInsSC() < 0x1000);
            break;

        case IF_T2_K3: // ........U....... ....iiiiiiiiiiii       PC              U, imm12
            assert(id->idReg1() == REG_PC);
            assert(id->emitGetInsSC() < 0x1000);
            break;

        case IF_T2_K2: // ............nnnn ....iiiiiiiiiiii       R1                 imm12
            assert(IsGeneralRegister(id->idReg1()));
            assert(id->emitGetInsSC() < 0x1000);
            break;

        case IF_T2_L1: // .....i.....S.... .iiiddddiiiiiiii       R1              S, imm8<<imm4
        case IF_T2_L2: // .....i......nnnn .iii....iiiiiiii       R1                 imm8<<imm4
            assert(IsGeneralRegister(id->idReg1()));
            assert(isModImmConst(id->emitGetInsSC()));
            break;

        case IF_T1_J3: // .....dddiiiiiiii                        R1  PC             imm8
            assert(IsGeneralRegister(id->idReg1()));
            assert(id->idReg2() == REG_PC);
            assert(id->emitGetInsSC() < 0x100);
            break;

        case IF_T2_VFP3:
            if (id->idOpSize() == EA_8BYTE)
            {
                assert(IsDoubleReg(id->idReg1()));
                assert(IsDoubleReg(id->idReg2()));
                assert(IsDoubleReg(id->idReg3()));
            }
            else
            {
                assert(id->idOpSize() == EA_4BYTE);
                assert(IsFloatReg(id->idReg1()));
                assert(IsFloatReg(id->idReg2()));
                assert(IsFloatReg(id->idReg3()));
            }
            break;

        case IF_T2_VFP2:
            assert(IsFloatReg(id->idReg1()));
            assert(IsFloatReg(id->idReg2()));
            break;

        case IF_T2_VLDST:
            if (id->idOpSize() == EA_8BYTE)
                assert(IsDoubleReg(id->idReg1()));
            else
                assert(IsFloatReg(id->idReg1()));
            assert(IsGeneralRegister(id->idReg2()));
            assert(offsetFitsInVectorMem(id->emitGetInsSC()));
            break;

        case IF_T2_VMOVD:
            assert(id->idOpSize() == EA_8BYTE);

            if (id->idIns() == INS_vmov_d2i)
            {
                assert(IsGeneralRegister(id->idReg1()));
                assert(IsGeneralRegister(id->idReg2()));
                assert(IsDoubleReg(id->idReg3()));
            }
            else
            {
                assert(id->idIns() == INS_vmov_i2d);
                assert(IsDoubleReg(id->idReg1()));
                assert(IsGeneralRegister(id->idReg2()));
                assert(IsGeneralRegister(id->idReg3()));
            }
            break;

        case IF_T2_VMOVS:
            assert(id->idOpSize() == EA_4BYTE);

            if (id->idIns() == INS_vmov_i2f)
            {
                assert(IsFloatReg(id->idReg1()));
                assert(IsGeneralRegister(id->idReg2()));
            }
            else
            {
                assert(id->idIns() == INS_vmov_f2i);
                assert(IsGeneralRegister(id->idReg1()));
                assert(IsFloatReg(id->idReg2()));
            }
            break;

        default:
            unreached();
    }
}
#endif // DEBUG

class ArmEncoder final : public Encoder
{
public:
    ArmEncoder(ArmEmitter& emit, GCInfo& gcInfo) : Encoder(emit, gcInfo)
    {
    }

    void EncodeInstr(insGroup* ig, instrDesc* id, uint8_t** dp);

private:
    static int encodeModImmConst(int imm);
    uint32_t emitInsCode(instruction ins, insFormat fmt);

#ifdef FEATURE_ITINSTRUCTION
    uint8_t* emitOutputIT(uint8_t* dst, instruction ins, insFormat fmt, uint32_t condcode);
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

#ifdef DEBUG
    void PrintIns(instrDesc* id, uint8_t* code, size_t sz);
#endif
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
    static const char* const insNames[]{
#define INST1(id, nm, ...) nm,
#define INST2(id, nm, ...) nm,
#define INST3(id, nm, ...) nm,
#define INST4(id, nm, ...) nm,
#define INST5(id, nm, ...) nm,
#define INST6(id, nm, ...) nm,
#define INST8(id, nm, ...) nm,
#define INST9(id, nm, ...) nm,
#include "instrsarm.h"
    };

    return ins < _countof(insNames) ? insNames[ins] : "???";
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

enum InsKind : uint8_t
{
    IK_FP = 1,
    IK_LD = 2,
    IK_ST = 4
};

static uint8_t insInfo(instruction ins)
{
#define LD IK_LD
#define ST IK_ST
    static const uint8_t info[]{
#define INST1(id, nm, fp, ldst, ...) ldst | IK_FP *fp,
#define INST2(id, nm, fp, ldst, ...) ldst | IK_FP *fp,
#define INST3(id, nm, fp, ldst, ...) ldst | IK_FP *fp,
#define INST4(id, nm, fp, ldst, ...) ldst | IK_FP *fp,
#define INST5(id, nm, fp, ldst, ...) ldst | IK_FP *fp,
#define INST6(id, nm, fp, ldst, ...) ldst | IK_FP *fp,
#define INST8(id, nm, fp, ldst, ...) ldst | IK_FP *fp,
#define INST9(id, nm, fp, ldst, ...) ldst | IK_FP *fp,
#include "instrsarm.h"
    };
#undef LD
#undef ST

    // We have pseudo ins like lea which are not included in emitInsLdStTab.
    return ins < _countof(info) ? info[ins] : 0;
}

static bool IsFloatIns(instruction ins)
{
    return (insInfo(ins) & IK_FP) != 0;
}

bool IsLoadIns(instruction ins)
{
    return (insInfo(ins) & IK_LD) != 0;
}

static bool IsLoadStoreIns(instruction ins)
{
    return (insInfo(ins) & (IK_LD | IK_ST)) != 0;
}

bool IsMovIns(instruction ins)
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

// Returns the specific encoding of the given CPU instruction and format
uint32_t ArmEncoder::emitInsCode(instruction ins, insFormat fmt)
{
    // clang-format off
    const static uint32_t insCodes1[]
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
    const static uint32_t insCodes2[]
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
    const static uint32_t insCodes3[]
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
    const static uint32_t insCodes4[]
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
    const static uint32_t insCodes5[]
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
    const static uint32_t insCodes6[]
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
    const static uint32_t insCodes7[]
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
    const static uint32_t insCodes8[]
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
    const static uint32_t insCodes9[]
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

    uint32_t code   = BAD_CODE;
    uint8_t  insFmt = emitInsFormat(ins);
    bool     found  = false;
    int      index  = 0;

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

    assert(code != BAD_CODE);

    return code;
}

static insSize emitInsSize(insFormat insFmt)
{
    assert(insFmt < IF_COUNT);

    if (insFmt >= IF_T2_A)
    {
        return ISZ_32BIT;
    }

    if (insFmt >= IF_T1_A)
    {
        return ISZ_16BIT;
    }

    if (insFmt == IF_LARGEJMP)
    {
        return ISZ_48BIT;
    }

    assert(insFmt == IF_GC_REG);

    return ISZ_NONE;
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

bool ArmImm::IsMvnImm(int value)
{
    return IsAluImm(value) || IsAluImm(~value);
}

bool ArmImm::IsImm(instruction ins, int32_t imm, insFlags flags)
{
    if (IsLoadStoreIns(ins) && !IsFloatIns(ins))
    {
        return validDispForLdSt(imm, TYP_INT);
    }

    switch (ins)
    {
        case INS_tst:
        case INS_eor:
        case INS_teq:
        case INS_adc:
        case INS_sbc:
        case INS_rsb:
            return IsAluImm(imm);
        case INS_cmp:
        case INS_cmn:
            return IsAluImm(imm) || IsAluImm(-imm);
        case INS_and:
        case INS_bic:
        case INS_orr:
        case INS_orn:
        case INS_mvn:
            return IsAluImm(imm) || IsAluImm(~imm);
        case INS_mov:
            return IsMovImm(imm);
        case INS_addw:
        case INS_subw:
            return (unsigned_abs(imm) <= 0x00000fff) && (flags != INS_FLAGS_SET); // 12-bit immediate
        case INS_add:
        case INS_sub:
            return IsAddImm(imm, flags);
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

bool ArmImm::IsBlImm(ssize_t addr, Compiler* compiler)
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
bool ArmImm::IsAluImm(int imm)
{
    return isModImmConst(imm);
}

// Returns true when the immediate 'imm' can be encoded using a single mov or mvn instruction.
bool ArmImm::IsMovImm(int imm)
{
    return ((imm & 0x0000ffff) == imm) || isModImmConst(imm) || isModImmConst(~imm);
}

// Returns true when the immediate 'imm' can be encoded using a single add or sub instruction.
bool ArmImm::IsAddImm(int imm, insFlags flags)
{
    return ((unsigned_abs(imm) <= 0x00000fff) && (flags != INS_FLAGS_SET)) || isModImmConst(imm) || isModImmConst(-imm);
}

// Returns true if this 'imm' can be encoded as a input operand to an cmp instruction.
bool ArmImm::IsCmpImm(int imm, insFlags flags)
{
    return isModImmConst(imm) || isModImmConst(-imm);
}

// Returns true when the immediate 'imm' can be encoded in "add Rd,SP,i10".
bool ArmImm::IsAddSpImm(int imm)
{
    return (imm & 0x03fc) == imm;
}

// Returns true when the immediate 'imm' can be encoded as the offset in a ldr/str instruction.
bool ArmImm::IsLdStImm(int imm, emitAttr size)
{
    return ((imm & 0x0fff) == imm) || (unsigned_abs(imm) <= 0x0ff);
}

// Returns true when the immediate 'imm' can be encoded as the offset in a vldr/vstr instruction,
// i.e. when it is a non-negative multiple of 4 that is less than 1024.
bool ArmImm::IsVLdStImm(int imm)
{
    return (imm & 0x3fc) == imm;
}

template <typename T>
T* ArmEmitter::AllocInstr(bool updateLastIns)
{
    instrDescSmall* id = emitAllocAnyInstr(sizeof(T), updateLastIns);
    memset(id, 0, sizeof(T));
    INDEBUG(id->idDebugOnlyInfo(new (emitComp, CMK_DebugOnly) instrDescDebugInfo(++emitInsCount, sizeof(T))));

    return static_cast<T*>(id);
}

instrDesc* ArmEmitter::emitNewInstr()
{
    return AllocInstr<instrDesc>();
}

instrDesc* ArmEmitter::emitNewInstrSmall()
{
    instrDescSmall* id = AllocInstr<instrDescSmall>();
    id->idSetIsSmallDsc();
    return static_cast<instrDesc*>(id);
}

instrDesc* ArmEmitter::emitNewInstrSC(int32_t cns)
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

instrDesc* ArmEmitter::emitNewInstrCns(int32_t cns)
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

instrDescJmp* ArmEmitter::emitNewInstrJmp()
{
    instrDescJmp* id = AllocInstr<instrDescJmp>();
    id->idjIG        = emitCurIG;
    id->idjOffs      = emitCurIGsize;
    id->idjNext      = emitCurIGjmpList;
    emitCurIGjmpList = id;
    return id;
}

instrDescCGCA* ArmEmitter::emitAllocInstrCGCA()
{
    return AllocInstr<instrDescCGCA>();
}

instrDesc* ArmEmitter::emitNewInstrGCReg(emitAttr attr, RegNum reg)
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

void ArmEmitter::emitIns(instruction ins)
{
    insFormat fmt = static_cast<insFormat>(emitInsFormat(ins));

    assert((fmt == IF_T1_A) || (fmt == IF_T2_A));

    instrDesc* id = emitNewInstrSmall();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idOpSize(EA_4BYTE);
    id->idInsSize(emitInsSize(fmt));

    appendToCurIG(id);
}

void ArmEmitter::emitIns_I(instruction ins, emitAttr attr, int32_t imm)
{
    insFormat fmt;
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
            {
                hasLR = true;
            }

            goto COMMON_PUSH_POP;

        case INS_pop:
            assert((imm & 0x2000) == 0);      // Cannot pop SP
            assert((imm & 0xC000) != 0xC000); // Cannot pop both PC and LR

            if (imm & 0x8000) // Is the PC being popped?
            {
                hasPC = true;
            }

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

                emitIns_R(ins, attr, genRegNumFromMask(imm));
                return;
            }

            // Encode the PC and LR bits as the lowest two bits
            imm <<= 2;

            if (hasPC)
            {
                imm |= 2;
            }

            if (hasLR)
            {
                imm |= 1;
            }

            assert(imm != 0);
            break;

#if 0
        // TODO-ARM-Cleanup: Enable or delete.
        case INS_bkpt:   // Windows uses a different encoding
            assert((imm & 0x0000ffff) == imm);
            fmt = IF_T1_L0;
            break;
#endif

        case INS_dmb:
        case INS_ism:
            assert((imm & 0x000f) == imm);
            fmt  = IF_T2_B;
            attr = EA_4BYTE;
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

    appendToCurIG(id);
}

void ArmEmitter::emitIns_R(instruction ins, emitAttr attr, RegNum reg)
{
    insFormat fmt;

    switch (ins)
    {
        case INS_pop:
        case INS_push:
            assert(EA_SIZE(attr) == EA_PTRSIZE);

            if (IsLowRegister(reg))
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

    appendToCurIG(id);
}

void ArmEmitter::emitIns_R_I(instruction ins, emitAttr attr, RegNum reg, int32_t imm, insFlags flags)
{
    insFormat fmt;
    insFlags  sf;

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
            else if (IsLowRegister(reg) && insSetsFlags(flags) && (unsigned_abs(imm) <= 0x00ff))
            {
                if (imm < 0)
                {
                    assert((ins == INS_add) || (ins == INS_sub));
                    ins = ins == INS_add ? INS_sub : INS_add;
                    imm = -imm;
                }

                fmt = IF_T1_J0;
                sf  = INS_FLAGS_SET;
            }
            else
            {
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
                assert(IsDoubleReg(reg));
                assert(imm <= 16);
                imm *= 2;
            }
            else
            {
                assert(attr == EA_4BYTE);
                assert(IsFloatReg(reg));
                assert(imm <= 16);
            }

            assert(((reg - REG_F0) + imm) <= 32);
            imm *= 4;
            imm = ins == INS_vpush ? -imm : imm;
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
            {
                hasPC = true;
            }

            if (imm & 0x4000) // Is the LR being pushed?
            {
                hasLR = true;
                useT2 = true;
            }

            if (!IsLowRegister(reg))
            {
                useT2 = true;
            }

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
            else
            {
                assert(!onlyT1);
                fmt = IF_T2_I0;
            }

            // Encode the PC and LR bits as the lowest two bits
            if (fmt == IF_T2_I0)
            {
                imm <<= 2;

                if (hasPC)
                {
                    imm |= 2;
                }

                if (hasLR)
                {
                    imm |= 1;
                }
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
            assert(reg != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            emitIns_R_R_I(ins, attr, reg, reg, imm, flags);
            return;

        case INS_mov:
            if (IsLowRegister(reg) && insSetsFlags(flags) && ((imm & 0x00ff) == imm))
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
            else
            {
                assert(insDoesNotSetFlags(flags) && ((imm & 0x0000ffff) == imm));
                ins = INS_movw;
                fmt = IF_T2_N;
                sf  = INS_FLAGS_NOT_SET;
            }
            break;

        case INS_movw:
        case INS_movt:
            assert(insDoesNotSetFlags(flags));
            assert((imm & 0x0000ffff) == imm);
            fmt = IF_T2_N;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_mvn:
            assert(isModImmConst(imm));
            fmt = IF_T2_L1;
            sf  = insMustSetFlags(flags);
            break;

        case INS_cmp:
            assert(reg != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(insSetsFlags(flags));
            sf = INS_FLAGS_SET;

            if (IsLowRegister(reg) && ((imm & 0x0ff) == imm))
            {
                fmt = IF_T1_J0;
            }
            else if (isModImmConst(imm))
            {
                fmt = IF_T2_L2;
            }
            else
            {
                assert(isModImmConst(-imm));
                ins = INS_cmn;
                fmt = IF_T2_L2;
                imm = -imm;
            }
            break;

        case INS_cmn:
        case INS_tst:
        case INS_teq:
            assert(reg != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(insSetsFlags(flags));
            assert(isModImmConst(imm));
            sf  = INS_FLAGS_SET;
            fmt = IF_T2_L2;
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
#endif
        case INS_pld:
        case INS_pldw:
            assert(insDoesNotSetFlags(flags));
            sf = INS_FLAGS_NOT_SET;
            if ((imm >= 0) && (imm <= 0x0fff))
            {
                fmt = IF_T2_K2;
            }
            else
            {
                assert((imm < 0) && (-imm <= 0x00ff));
                imm = -imm;
                fmt = IF_T2_H2;
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

    appendToCurIG(id);
}

void ArmEmitter::emitIns_MovRelocatableImmediate(instruction ins, RegNum reg, void* addr)
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

    appendToCurIG(id);
}

void ArmEmitter::emitIns_Mov(instruction ins, emitAttr attr, RegNum dstReg, RegNum srcReg, bool canSkip, insFlags flags)
{
    assert(IsMovIns(ins));

    emitAttr  size = EA_SIZE(attr);
    insFormat fmt;
    insFlags  sf;

    switch (ins)
    {
        case INS_mov:
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
            else
            {
                assert(insSetsFlags(flags));
                sf  = INS_FLAGS_SET;
                fmt = IsLowRegister(dstReg) && IsLowRegister(srcReg) ? IF_T1_E : IF_T2_C3;
            }
            break;

        case INS_vmov:
            // VM debugging single stepper doesn't support PC register with this instruction.
            assert(dstReg != REG_PC);
            assert(srcReg != REG_PC);

            if (canSkip && (dstReg == srcReg))
            {
                // These instructions have no side effect and can be skipped
                return;
            }

            assert(size == EA_8BYTE ? IsDoubleReg(dstReg) : IsFloatReg(dstReg));
            assert(size == EA_8BYTE ? IsDoubleReg(srcReg) : IsFloatReg(srcReg));

            fmt = IF_T2_VFP2;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_vmov_i2f:
            // VM debugging single stepper doesn't support PC register with this instruction.
            assert(srcReg != REG_PC);
            assert(IsFloatReg(dstReg));
            assert(IsGeneralRegister(srcReg));
            fmt = IF_T2_VMOVS;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_vmov_f2i:
            // VM debugging single stepper doesn't support PC register with this instruction.
            assert(dstReg != REG_PC);
            assert(IsGeneralRegister(dstReg));
            assert(IsFloatReg(srcReg));
            fmt = IF_T2_VMOVS;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_sxtb:
        case INS_uxtb:
            assert(size == EA_4BYTE);
            goto EXTEND_COMMON;
        case INS_sxth:
        case INS_uxth:
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

            if (!IsLowRegister(dstReg) || !IsLowRegister(srcReg))
            {
                emitIns_R_R_I(ins, attr, dstReg, srcReg, 0, INS_FLAGS_NOT_SET);
                return;
            }

            fmt = IF_T1_E;
            sf  = INS_FLAGS_NOT_SET;
            break;

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

    appendToCurIG(id);
}

void ArmEmitter::emitIns_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, insFlags flags)
{
    if (IsMovIns(ins))
    {
        assert(!"Please use emitIns_Mov() to correctly handle move elision");
        emitIns_Mov(ins, attr, reg1, reg2, /* canSkip */ false, flags);

        return;
    }

    emitAttr  size = EA_SIZE(attr);
    insFormat fmt;
    insFlags  sf;

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
            emitIns_R_R_R(ins, attr, reg1, reg1, reg2, flags);
            return;

        case INS_cmp:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(insSetsFlags(flags));
            sf  = INS_FLAGS_SET;
            fmt = IsLowRegister(reg1) && IsLowRegister(reg2) ? IF_T1_E : IF_T1_D0;
            break;

        case INS_vcvt_d2i:
        case INS_vcvt_d2u:
        case INS_vcvt_d2f:
            assert(IsFloatReg(reg1));
            assert(IsDoubleReg(reg2));
            goto VCVT_COMMON;
        case INS_vcvt_f2d:
        case INS_vcvt_u2d:
        case INS_vcvt_i2d:
            assert(IsDoubleReg(reg1));
            assert(IsFloatReg(reg2));
            goto VCVT_COMMON;
        case INS_vcvt_u2f:
        case INS_vcvt_i2f:
        case INS_vcvt_f2i:
        case INS_vcvt_f2u:
            assert(size == EA_4BYTE);
            assert(IsFloatReg(reg1));
            assert(IsFloatReg(reg2));
            goto VCVT_COMMON;
        case INS_vabs:
        case INS_vsqrt:
        case INS_vcmp:
        case INS_vneg:
            assert(size == EA_8BYTE ? IsDoubleReg(reg1) : IsFloatReg(reg1));
            assert(size == EA_8BYTE ? IsDoubleReg(reg2) : IsFloatReg(reg2));
        VCVT_COMMON:
            fmt = IF_T2_VFP2;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_vadd:
        case INS_vmul:
        case INS_vsub:
        case INS_vdiv:
            assert(size == EA_8BYTE ? IsDoubleReg(reg1) : IsFloatReg(reg1));
            assert(size == EA_8BYTE ? IsDoubleReg(reg2) : IsFloatReg(reg2));
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

            if (insSetsFlags(flags) && IsLowRegister(reg1) && IsLowRegister(reg2))
            {
                fmt = IF_T1_E;
                sf  = INS_FLAGS_SET;
                break;
            }
            FALLTHROUGH;
        case INS_orn:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            emitIns_R_R_R_I(ins, attr, reg1, reg1, reg2, 0, flags);
            return;

        case INS_asr:
        case INS_lsl:
        case INS_lsr:
        case INS_ror:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);

            if (!insSetsFlags(flags) || !IsLowRegister(reg1) || !IsLowRegister(reg2))
            {
                emitIns_R_R_R(ins, attr, reg1, reg1, reg2, flags);
                return;
            }

            fmt = IF_T1_E;
            sf  = INS_FLAGS_SET;
            break;

        case INS_mul:
            // We will prefer the T2 encoding, unless (flags == INS_FLAGS_SET)
            // The thumb-1 instruction executes much slower as it must always set the flags
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);

            if (!insMustSetFlags(flags) || !IsLowRegister(reg1) || !IsLowRegister(reg2))
            {
                emitIns_R_R_R(ins, attr, reg1, reg2, reg1, flags);
                return;
            }

            fmt = IF_T1_E;
            sf  = INS_FLAGS_SET;
            break;

        case INS_mvn:
        case INS_cmn:
        case INS_tst:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);

            if (!insSetsFlags(flags) || !IsLowRegister(reg1) || !IsLowRegister(reg2))
            {
                emitIns_R_R_I(ins, attr, reg1, reg2, 0, flags);
                return;
            }

            fmt = IF_T1_E;
            sf  = INS_FLAGS_SET;
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
            unreached();
    }

    assert((fmt == IF_T1_D0) || (fmt == IF_T1_E) || (fmt == IF_T2_C3) || (fmt == IF_T2_C9) || (fmt == IF_T2_C10) ||
           (fmt == IF_T2_VFP2) || (fmt == IF_T2_VMOVD) || (fmt == IF_T2_VMOVS) || (fmt == IF_T2_E1));

    instrDesc* id = emitNewInstrSmall();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idInsSize(emitInsSize(fmt));
    id->idInsFlags(sf);
    id->idReg1(reg1);
    id->idReg2(reg2);

    appendToCurIG(id);
}

void ArmEmitter::emitIns_R_I_I(instruction ins, emitAttr attr, RegNum reg, int imm1, int imm2, insFlags flags)
{
    assert(ins == INS_bfc);
    assert(reg != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
    assert(insDoesNotSetFlags(flags));

    int lsb = imm1;
    int msb = lsb + imm2 - 1;

    assert((lsb >= 0) && (lsb <= 31));
    assert((msb >= 0) && (msb <= 31));
    assert(msb >= lsb);

    instrDesc* id = emitNewInstrSC((lsb << 5) | msb);
    id->idIns(ins);
    id->idInsFmt(IF_T2_D1);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idInsSize(ISZ_32BIT);
    id->idInsFlags(INS_FLAGS_NOT_SET);
    id->idReg1(reg);

    appendToCurIG(id);
}

void ArmEmitter::emitIns_R_R_I(
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
                    emitIns_R_I(ins, attr, reg1, imm, flags);
                    return;
                }

                if (IsLowRegister(reg1))
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

            if ((imm == 0) && insDoesNotSetFlags(flags))
            {
                emitIns_Mov(INS_mov, attr, reg1, reg2, /* canSkip */ true, flags);
                return;
            }

            // Can we encode the immediate 'imm' using a Thumb-1 encoding?
            if (IsLowRegister(reg1) && IsLowRegister(reg2) && insSetsFlags(flags) && (unsigned_abs(imm) <= 0x0007))
            {
                if (imm < 0)
                {
                    assert((ins == INS_add) || (ins == INS_sub));
                    ins = ins == INS_add ? INS_sub : INS_add;
                    imm = -imm;
                }

                fmt = IF_T1_G;
                sf  = INS_FLAGS_SET;
            }
            else if ((reg1 == reg2) && IsLowRegister(reg1) && insSetsFlags(flags) && (unsigned_abs(imm) <= 0x00ff))
            {
                if (imm < 0)
                {
                    assert((ins == INS_add) || (ins == INS_sub));
                    ins = ins == INS_add ? INS_sub : INS_add;
                    imm = -imm;
                }

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
                ins = ins == INS_add ? INS_sub : INS_add;
                imm = -imm;
                fmt = IF_T2_L0;
                sf  = insMustSetFlags(flags);
            }
            else if (insDoesNotSetFlags(flags) && (unsigned_abs(imm) <= 0x0fff))
            {
                if (imm < 0)
                {
                    assert((ins == INS_add) || (ins == INS_sub));
                    ins = ins == INS_add ? INS_sub : INS_add;
                    imm = -imm;
                }

                // add/sub => addw/subw instruction
                // Note that even when using the w prefix the immediate is still only 12 bits?
                ins = ins == INS_add ? INS_addw : INS_subw;
                fmt = IF_T2_M0;
                sf  = INS_FLAGS_NOT_SET;
            }
            else
            {
                assert(insDoesNotSetFlags(flags) && (reg1 != REG_SP) && (reg1 != REG_PC));
                // movw/movt reg1, imm
                codeGen->instGen_Set_Reg_To_Imm(attr, reg1, (ins == INS_sub ? -1 : 1) * imm);
                emitIns_R_R(INS_add, attr, reg1, reg2);

                return;
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
            else
            {
                assert(isModImmConst(~imm));

                fmt = IF_T2_L0;
                sf  = insMustSetFlags(flags);
                imm = ~imm;

                switch (ins)
                {
                    case INS_and:
                        ins = INS_bic;
                        break;
                    case INS_bic:
                        ins = INS_and;
                        break;
                    case INS_orr:
                        ins = INS_orn;
                        break;
                    default:
                        assert(ins == INS_orn);
                        ins = INS_orr;
                        break;
                }
            }
            break;

        case INS_rsb:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(insOptsNone(opt));

            if ((imm == 0) && IsLowRegister(reg1) && IsLowRegister(reg2) && insSetsFlags(flags))
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
            assert(isModImmConst(imm));
            fmt = IF_T2_L0;
            sf  = insMustSetFlags(flags);
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

            if (IsLowRegister(reg1) && ((imm & 0x00ff) == imm))
            {
                fmt = IF_T1_J3;
            }
            else
            {
                assert((imm & 0x0fff) == imm);
                fmt = IF_T2_M1;
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

                if (IsLowRegister(reg1) && IsLowRegister(reg2) && insSetsFlags(flags))
                {
                    emitIns_R_R(ins, attr, reg1, reg2, flags);
                    return;
                }
            }
            else
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
            assert((imm >= 0) && (imm <= 31));
            assert(!insOptAnyInc(opt));

            if (imm == 0)
            {
                assert(insOptsNone(opt));

                if (ins == INS_cmp)
                {
                    emitIns_R_R(ins, attr, reg1, reg2, flags);
                    return;
                }

                if (((ins == INS_cmn) || (ins == INS_tst)) && IsLowRegister(reg1) && IsLowRegister(reg2))
                {
                    emitIns_R_R(ins, attr, reg1, reg2, flags);
                    return;
                }
            }
            else
            {
                assert(insOptAnyShift(opt));
                assert((opt != INS_OPTS_RRX) || (imm == 1));
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
                emitIns_Mov(INS_mov, attr, reg1, reg2, /* canSkip */ !insMustSetFlags(flags), flags);
                return;
            }

            if (insSetsFlags(flags) && (ins != INS_ror) && IsLowRegister(reg1) && IsLowRegister(reg2))
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

            if ((imm == 0) && IsLowRegister(reg1) && IsLowRegister(reg2))
            {
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
#endif
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

            if (IsLowRegister(reg1) && IsLowRegister(reg2) && insOptsNone(opt) && ((imm & 0x001f) == imm))
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

            if (IsLowRegister(reg1) && IsLowRegister(reg2) && insOptsNone(opt) && ((imm & 0x003e) == imm))
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
                assert(insOptsPostInc(opt) ? (imm > 0) : (imm < 0));
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
            if (IsLowRegister(reg1) && insOptsNone(opt) && ((imm & 0x03fc) == imm))
            {
                if (reg2 == REG_SP)
                {
                    fmt = IF_T1_J2;
                    sf  = INS_FLAGS_NOT_SET;
                    break;
                }

                if (reg2 == REG_PC)
                {
                    if (ins == INS_ldr)
                    {
                        fmt = IF_T1_J3;
                        sf  = INS_FLAGS_NOT_SET;
                        break;
                    }
                }
                else if (IsLowRegister(reg2))
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

        COMMON_THUMB2_LDST:
            assert(fmt == IF_NONE);
            assert(insDoesNotSetFlags(flags));
            sf = INS_FLAGS_NOT_SET;

            if (insOptAnyInc(opt))
            {
                assert(insOptsPostInc(opt) ? (imm > 0) : (imm < 0));
                assert(unsigned_abs(imm) <= 0x00ff);

                fmt = IF_T2_H0;
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
            assert((imm & 0x03fc) == imm);
            sf  = INS_FLAGS_NOT_SET;
            fmt = IF_T2_H0;
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

    appendToCurIG(id);
}

void ArmEmitter::emitIns_R_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, insFlags flags)
{
    emitAttr  size = EA_SIZE(attr);
    insFormat fmt;
    insFlags  sf;

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
            assert((reg3 != REG_PC) || (ins == INS_add)); // allow ADD Rn, PC instruction in T2 encoding

            if (!IsLowRegister(reg1) || !IsLowRegister(reg2) || !IsLowRegister(reg3) || !insSetsFlags(flags))
            {
                if ((ins == INS_add) && insDoesNotSetFlags(flags))
                {
                    if (reg1 == reg2)
                    {
                        emitIns_R_R(ins, attr, reg1, reg3, flags);
                        return;
                    }

                    if (reg1 == reg3)
                    {
                        emitIns_R_R(ins, attr, reg1, reg2, flags);
                        return;
                    }
                }

                emitIns_R_R_R_I(ins, attr, reg1, reg2, reg3, 0, flags);
                return;
            }

            fmt = IF_T1_H;
            sf  = INS_FLAGS_SET;
            break;

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
                emitIns_R_R(ins, attr, reg1, reg3, flags);
                return;
            }
            FALLTHROUGH;
        case INS_orn:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(reg3 != REG_PC);
            emitIns_R_R_R_I(ins, attr, reg1, reg2, reg3, 0, flags);
            return;

        case INS_asr:
        case INS_lsl:
        case INS_lsr:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(reg3 != REG_PC);
            if ((reg1 == reg2) && insSetsFlags(flags) && IsLowRegister(reg1) && IsLowRegister(reg3))
            {
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
                // VM debugging single stepper doesn't support PC register with this instruction.
                assert(reg1 != REG_PC);
                assert(reg2 != REG_PC);
                assert(reg3 != REG_PC);

                if ((reg1 == reg2) && IsLowRegister(reg1))
                {
                    emitIns_R_R(ins, attr, reg1, reg3, flags);
                }
                else
                {
                    assert((reg1 == reg3) && IsLowRegister(reg1));
                    emitIns_R_R(ins, attr, reg1, reg2, flags);
                }

                return;
            }
#ifndef USE_HELPERS_FOR_INT_DIV
            FALLTHROUGH;
        case INS_sdiv:
        case INS_udiv:
#endif
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

            if (!IsLowRegister(reg1) || !IsLowRegister(reg2) || !IsLowRegister(reg3))
            {
                emitIns_R_R_R_I(ins, attr, reg1, reg2, reg3, 0, flags);
                return;
            }

            fmt = IF_T1_H;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_vadd:
        case INS_vmul:
        case INS_vsub:
        case INS_vdiv:
            assert(size == EA_8BYTE ? IsDoubleReg(reg1) : IsFloatReg(reg1));
            assert(size == EA_8BYTE ? IsDoubleReg(reg2) : IsFloatReg(reg2));
            assert(size == EA_8BYTE ? IsDoubleReg(reg3) : IsFloatReg(reg3));
            fmt = IF_T2_VFP3;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_vmov_i2d:
            assert(reg2 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg3 != REG_PC);
            assert(IsDoubleReg(reg1));
            assert(IsGeneralRegister(reg2));
            assert(IsGeneralRegister(reg3));
            fmt = IF_T2_VMOVD;
            sf  = INS_FLAGS_NOT_SET;
            break;

        case INS_vmov_d2i:
            assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
            assert(reg2 != REG_PC);
            assert(IsGeneralRegister(reg1));
            assert(IsGeneralRegister(reg2));
            assert(IsDoubleReg(reg3));
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

    appendToCurIG(id);
}

void ArmEmitter::emitIns_R_R_I_I(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, int imm1, int imm2, insFlags flags)
{
    int lsb   = imm1;
    int width = imm2;
    int msb   = lsb + width - 1;

    assert((lsb >= 0) && (lsb <= 31));
    assert((width > 0) && (width <= 32));
    assert((msb >= 0) && (msb <= 31));
    assert(msb >= lsb);

    insFormat fmt;
    insFlags  sf;
    int       imm;

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

    instrDesc* id = emitNewInstrSC(imm);
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idInsSize(emitInsSize(fmt));
    id->idInsFlags(sf);
    id->idReg1(reg1);
    id->idReg2(reg2);

    appendToCurIG(id);
}

void ArmEmitter::emitIns_R_R_R_I(
    instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, int32_t imm, insFlags flags, insOpts opt)
{
    emitAttr  size = EA_SIZE(attr);
    insFormat fmt;
    insFlags  sf;

    switch (ins)
    {
        case INS_add:
        case INS_sub:
            if (imm == 0)
            {
                if (IsLowRegister(reg1) && IsLowRegister(reg2) && IsLowRegister(reg3) && insSetsFlags(flags))
                {
                    emitIns_R_R_R(ins, attr, reg1, reg2, reg3, flags);
                    return;
                }

                if ((ins == INS_add) && insDoesNotSetFlags(flags))
                {
                    if (reg1 == reg2)
                    {
                        emitIns_R_R(ins, attr, reg1, reg3, flags);
                        return;
                    }

                    if (reg1 == reg3)
                    {
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
            assert((imm >= 0) && (imm <= 31));
            assert(!insOptAnyInc(opt));

            if (imm == 0)
            {
                if (opt == INS_OPTS_LSL)
                {
                    opt = INS_OPTS_NONE;
                }

                assert(insOptsNone(opt));

                if (IsLowRegister(reg1) && IsLowRegister(reg2) && IsLowRegister(reg3) && insSetsFlags(flags))
                {
                    if (reg1 == reg2)
                    {
                        emitIns_R_R(ins, attr, reg1, reg3, flags);
                        return;
                    }

                    if ((reg1 == reg3) && (ins != INS_bic) && (ins != INS_orn) && (ins != INS_sbc))
                    {
                        emitIns_R_R(ins, attr, reg1, reg2, flags);
                        return;
                    }
                }
            }
            else
            {
                assert(insOptAnyShift(opt));
                assert((opt != INS_OPTS_RRX) || (imm == 1));
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
            assert((imm & 0x0003) == imm);

            if ((imm == 0) && insOptsNone(opt) && IsLowRegister(reg1) && IsLowRegister(reg2) && IsLowRegister(reg3))
            {
                emitIns_R_R_R(ins, attr, reg1, reg2, reg3, flags);
                return;
            }

            assert(insOptsNone(opt) || (opt == INS_OPTS_LSL));
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
                assert(insOptsPostInc(opt) ? (imm > 0) : (imm < 0));
            }
            else
            {
                assert(insOptsNone(opt));
            }

            assert(unsigned_abs(imm) <= 0x03fc);
            imm >>= 2;
            fmt = IF_T2_G0;
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

    appendToCurIG(id);
}

void ArmEmitter::emitIns_R_R_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, RegNum reg4)
{
    assert(reg1 != REG_PC); // VM debugging single stepper doesn't support PC register with this instruction.
    assert(reg2 != REG_PC);
    assert(reg3 != REG_PC);
    assert(reg4 != REG_PC);

    insFormat fmt;

    switch (ins)
    {
        case INS_smull:
        case INS_umull:
        case INS_smlal:
        case INS_umlal:
            assert(reg1 != reg2);
            fmt = IF_T2_F1;
            break;
        case INS_mla:
        case INS_mls:
            fmt = IF_T2_F2;
            break;
        default:
            unreached();
    }

    instrDesc* id = emitNewInstr();
    id->idIns(ins);
    id->idInsFmt(fmt);
    id->idInsSize(emitInsSize(fmt));
    id->idInsFlags(INS_FLAGS_NOT_SET);
    id->idGCref(EA_GC_TYPE(attr));
    id->idOpSize(EA_SIZE(attr));
    id->idReg1(reg1);
    id->idReg2(reg2);
    id->idReg3(reg3);
    id->idReg4(reg4);

    appendToCurIG(id);
}

void ArmEmitter::MovRegStackOffset(RegNum reg, int32_t imm, StackAddrMode s)
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

        appendToCurIG(id);
    };

    mov(INS_movw, imm & 0xFFFF);

    if ((imm >> 16) != 0)
    {
        mov(INS_movt, (imm >> 16) & 0xFFFF);
    }
}

static bool IsUnsignedImm8(int imm, unsigned shift = 0)
{
    return (imm & ~(255 << shift)) == 0;
}

static bool IsSignedImm8(int imm, unsigned shift = 0)
{
    return (unsigned_abs(imm) & ~(255 << shift)) == 0;
}

static bool IsUnsignedImm12(int imm)
{
    return (imm & ~4095) == 0;
}

static bool IsSignedImm12(int imm)
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

void ArmEmitter::emitIns_R_S(instruction ins, emitAttr attr, RegNum reg, StackAddrMode s)
{
    assert(IsLoad(ins) || (ins == INS_lea));
    Ins_R_S(ins, attr, reg, s);
}

void ArmEmitter::emitIns_S_R(instruction ins, emitAttr attr, RegNum reg, StackAddrMode s)
{
    assert(IsStore(ins));
    Ins_R_S(ins, attr, reg, s);
}

void ArmEmitter::Ins_R_S(instruction ins, emitAttr attr, RegNum reg, StackAddrMode s)
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
        if (IsLowRegister(reg) && (baseReg == REG_SP) && IsUnsignedImm8(imm, 2))
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
    else if (((ins == INS_ldr) || (ins == INS_str)) && IsLowRegister(reg) && (baseReg == REG_SP) &&
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

    appendToCurIG(id);
}

// Change frame pointer based addressing to SP-based addressing when possible because it has smaller encoding.
int ArmEmitter::OptimizeFrameAddress(int fpOffset, bool isFloatLoadStore, RegNum* baseReg)
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

void instrDescJmp::SetShortJump()
{
    assert((idInsFmt() == IF_T2_J1) || (idInsFmt() == IF_T2_J2) || (idInsFmt() == IF_LARGEJMP));
    assert(!idIsCnsReloc());

    idInsFmt(idInsFmt() == IF_T2_J2 ? IF_T1_M : IF_T1_K);
    idInsSize(ISZ_16BIT);
}

void instrDescJmp::SetMediumJump()
{
    assert((idInsFmt() == IF_T2_J1) || (idInsFmt() == IF_LARGEJMP));
    assert(!idIsCnsReloc());

    idInsFmt(IF_T2_J1);
    idInsSize(ISZ_32BIT);
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

instruction JumpKindToJcc(emitJumpKind kind)
{
    static const instruction map[]{
        INS_nop, INS_b,
#define CC_DEF(cc, rev, ...) INS_b##cc,
#include "emitjmps.h"
    };

    assert(kind < _countof(map));
    return map[kind];
}

emitJumpKind ReverseJumpKind(emitJumpKind kind)
{
    static const emitJumpKind map[]{
        EJ_NONE, EJ_jmp,
#define CC_DEF(cc, rev, ...) EJ_##rev,
#include "emitjmps.h"
    };

    assert(kind < _countof(map));
    return map[kind];
}

static emitJumpKind JccToJumpKind(instruction ins)
{
    assert((INS_b <= ins) && (ins <= INS_ble));
#define CC_DEF(cc, rev, ...) static_assert_no_msg(INS_b##cc - INS_b == EJ_##cc - EJ_jmp);
#include "emitjmps.h"

    return static_cast<emitJumpKind>(EJ_jmp + (ins - INS_b));
}

void ArmEmitter::emitIns_J(instruction ins, int instrCount)
{
    assert(IsMainProlog(emitCurIG));
    assert(IsBranch(ins));
    assert(instrCount < 0);

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(ins);
    id->idInsFmt(ins == INS_b ? IF_T1_M : IF_T1_K);
    id->idInsSize(ISZ_16BIT);
    id->SetInstrCount(instrCount);

    appendToCurIG(id);
}

void ArmEmitter::emitIns_J(instruction ins, insGroup* label)
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

    appendToCurIG(id);
}

void ArmEmitter::emitIns_J_R(instruction ins, emitAttr attr, insGroup* label, RegNum reg)
{
    // TODO-MIKE-Review: cbz/cbnz aren't used on ARM. Delete or try to use these instructions?
    // Their limited range might make using them problematic, we might save a cheap 0 compare
    // and end up with an extra unconditional branch.
    assert((ins == INS_cbz) || (ins == INS_cbnz));
    assert(emitCurIG->GetFuncletIndex() == label->GetFuncletIndex());
    assert(IsLowRegister(reg));
    assert(!emitComp->opts.compReloc || !InDifferentRegions(emitCurIG, label));

    instrDescJmp* id = emitNewInstrJmp();
    id->idIns(ins);
    id->idInsFmt(IF_T1_I);
    id->idInsSize(ISZ_16BIT);
    id->idOpSize(EA_4BYTE);
    id->idReg1(reg);
    id->SetLabel(label);

    appendToCurIG(id);
}

void ArmEmitter::emitIns_CallFinally(insGroup* label)
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

    appendToCurIG(id);
}

void ArmEmitter::emitIns_R_L(instruction ins, RegNum reg, insGroup* label)
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

    appendToCurIG(id);
}

void ArmEmitter::emitIns_R_D(instruction ins, RegNum reg, ConstData* data)
{
    assert((ins == INS_movw) || (ins == INS_movt));

    instrDesc* id = emitNewInstrSC(data->offset);
    id->idIns(ins);
    id->idReg1(reg);
    id->idInsFmt(IF_T2_N2);
    id->idOpSize(EA_4BYTE);
    id->idInsSize(ISZ_32BIT);
    id->idSetIsCnsReloc(emitComp->opts.compReloc);

    appendToCurIG(id);
}

// Add a call instruction (direct or indirect).
//
// EC_FUNC_TOKEN : addr is the method address
// EC_INDIR_R    : call ireg (addr has to be null)
//
// Please consult the "debugger team notification" comment in genFnProlog().
//
void ArmEmitter::emitIns_Call(EmitCallType          kind,
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
        assert((addr == nullptr) || ArmImm::IsBlImm(reinterpret_cast<ssize_t>(addr), emitComp));

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
                instr->SetShortJump();
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
                instr->SetMediumJump();
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
        assert(IsDoubleReg(reg));
    else
        assert(IsFloatReg(reg));

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

    uint32_t code = emitInsCode(ins, fmt);

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
        unsigned jumpInstrNum = ig->FindInsNum(id);

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

#ifdef DEBUG
    if (id->HasInstrCount())
    {
        assert(FitsIn<int8_t>(distance));
        // Store the jump distance into the (unused) imm field of the instruction,
        // so the printer doesn't need to recompute it.
        id->idSmallCns(distance + -INT8_MIN);
    }
#endif

    if (id->idInsSize() == ISZ_16BIT)
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

        uint32_t code = emitInsCode(ins, fmt);

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
        instruction reverse = JumpKindToJcc(ReverseJumpKind(JccToJumpKind(ins)));
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

    uint32_t code = emitInsCode(INS_b, IF_T2_J2);

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
    uint32_t code = emitInsCode(ins, fmt);

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
uint8_t* ArmEncoder::emitOutputIT(uint8_t* dst, instruction ins, insFormat fmt, uint32_t condcode)
{
    uint32_t imm0;
    uint32_t code, mask, bit;

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
unsigned ArmEmitter::emitGetInstructionSize(const emitLocation& emitLoc)
{
    insGroup*  ig;
    instrDesc* id;

    bool anyInstrs = emitGetLocationInfo(emitLoc, &ig, &id);
    // There better be an instruction at this location (otherwise,
    // we're at the end of the instruction list)
    assert(anyInstrs);

    return id->idCodeSize();
}

void EmitterBase::emitEndCodeGen(GCInfo& gcInfo)
{
    ArmEmitter& emit = *static_cast<ArmEmitter*>(this);
    ArmEncoder  encoder(emit, gcInfo);
    encoder.emitEndCodeGen(emit);
}

size_t Encoder::emitOutputInstr(insGroup* ig, instrDesc* id, uint8_t** dp)
{
    static_cast<ArmEncoder*>(this)->EncodeInstr(ig, id, dp);
    return id->GetDescSize();
}

void ArmEncoder::EncodeInstr(insGroup* ig, instrDesc* id, uint8_t** dp)
{
    uint8_t*    dst  = *dp;
    instruction ins  = id->idIns();
    insFormat   fmt  = id->idInsFmt();
    emitAttr    size = id->idOpSize();

    switch (fmt)
    {
        uint32_t code;
        int      imm;
        void*    addr;

        case IF_T1_I:  // ......i.iiiiiddd                       R1                  imm6
        case IF_T1_K:  // ....cccciiiiiiii                       Branch              imm8, cond4
        case IF_T1_M:  // .....iiiiiiiiiii                       Branch              imm11
        case IF_T2_J1: // .....Scccciiiiii ..j.jiiiiiiiiiii      Branch              imm20, cond4
        case IF_T2_J2: // .....Siiiiiiiiii ..j.jiiiiiiiiii.      Branch              imm24
        case IF_LARGEJMP:
            dst = emitOutputLJ(dst, static_cast<instrDescJmp*>(id), ig);
            break;

        case IF_T1_J3: // .....dddiiiiiiii                        R1  PC             imm8
        case IF_T2_M1: // .....i.......... .iiiddddiiiiiiii       R1  PC             imm12
        case IF_T2_N1: // .....i......iiii .iiiddddiiiiiiii       R1                 imm16
            dst = emitOutputRL(dst, static_cast<instrDescJmp*>(id));
            break;

        case IF_T1_A: // ................
            code = emitInsCode(ins, fmt);
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

#ifdef FEATURE_ITINSTRUCTION
        case IF_T1_B: // ........cccc....                                           cond
            assert(id->idGCref() == GCT_NONE);
            dst = emitOutputIT(dst, ins, fmt, id->emitGetInsSC());
            break;
#endif

        case IF_T1_C: // .....iiiiinnnddd                       R1  R2              imm5
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT1_D3(id->idReg1());
            code |= insEncodeRegT1_N3(id->idReg2());
            imm = insUnscaleImm(ins, imm);
            assert((imm & 0x001f) == imm);
            code |= (imm << 6);
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T1_D0: // ........Dmmmmddd                       R1* R2*
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT1_D4(id->idReg1());
            code |= insEncodeRegT1_M4(id->idReg2());
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T1_E: // ..........nnnddd                       R1  R2
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT1_D3(id->idReg1());
            code |= insEncodeRegT1_N3(id->idReg2());
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T1_F: // .........iiiiiii                       SP                  imm7
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            assert((ins == INS_add) || (ins == INS_sub));
            assert((imm & 0x0003) == 0);
            imm >>= 2;
            assert((imm & 0x007F) == imm);
            code |= imm;
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T1_G: // .......iiinnnddd                       R1  R2              imm3
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT1_D3(id->idReg1());
            code |= insEncodeRegT1_N3(id->idReg2());
            assert((imm & 0x0007) == imm);
            code |= (imm << 6);
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T1_H: // .......mmmnnnddd                       R1  R2  R3
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT1_D3(id->idReg1());
            code |= insEncodeRegT1_N3(id->idReg2());
            code |= insEncodeRegT1_M3(id->idReg3());
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T1_J0: // .....dddiiiiiiii                       R1                  imm8
        case IF_T1_J1: // .....dddiiiiiiii                       R1                  <regmask8>
        case IF_T1_J2: // .....dddiiiiiiii                       R1  SP              imm8
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

        case IF_T1_L0: // ........iiiiiiii                                           imm8
        case IF_T1_L1: // .......Rrrrrrrrr                                           <regmask8>
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);

            if (fmt == IF_T1_L1)
            {
                assert((imm & 0x3) != 0x3);
                if (imm & 0x3)
                {
                    code |= 0x0100; //  R bit
                }
                imm >>= 2;
            }

            assert((imm & 0x00ff) == imm);
            code |= imm;
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T2_A: // ................ ................
            code = emitInsCode(ins, fmt);
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_B: // ................ ............iiii                          imm4
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            assert((imm & 0x000F) == imm);
            code |= imm;
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_C0: // ...........Snnnn .iiiddddiishmmmm       R1  R2  R3      S, imm5, sh
        case IF_T2_C4: // ...........Snnnn ....dddd....mmmm       R1  R2  R3      S
        case IF_T2_C5: // ............nnnn ....dddd....mmmm       R1  R2  R3
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            code |= insEncodeRegT2_N(id->idReg2());
            code |= insEncodeRegT2_M(id->idReg3());

            if (fmt != IF_T2_C5)
            {
                code |= insEncodeSetFlags(id->idInsFlags());
            }

            if (fmt == IF_T2_C0)
            {
                imm = id->emitGetInsSC();
                code |= insEncodeShiftCount(imm);
                code |= insEncodeShiftOpts(id->idInsOpt());
            }

            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_C1: // ...........S.... .iiiddddiishmmmm       R1  R2          S, imm5, sh
        case IF_T2_C2: // ...........S.... .iiiddddii..mmmm       R1  R2          S, imm5
        case IF_T2_C6: // ................ ....dddd..iimmmm       R1  R2                   imm2
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
                {
                    code |= insEncodeShiftOpts(id->idInsOpt());
                }
            }

            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_C3: // ...........S.... ....dddd....mmmm       R1  R2          S
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            code |= insEncodeRegT2_M(id->idReg2());
            code |= insEncodeSetFlags(id->idInsFlags());
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_C7: // T2_C7   ............nnnn ..........shmmmm       R1  R2                   imm2
        case IF_T2_C8: // T2_C8   ............nnnn .iii....iishmmmm       R1  R2             imm5, sh
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_N(id->idReg1());
            code |= insEncodeRegT2_M(id->idReg2());

            if (fmt == IF_T2_C7)
            {
                assert((imm & 0x0003) == imm);
                code |= (imm << 4);
            }
            else
            {
                code |= insEncodeShiftCount(imm);
                code |= insEncodeShiftOpts(id->idInsOpt());
            }

            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_C9: // ............nnnn ............mmmm       R1  R2
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_N(id->idReg1());
            code |= insEncodeRegT2_M(id->idReg2());
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_C10: // ............mmmm ....dddd....mmmm       R1  R2
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            code |= insEncodeRegT2_M(id->idReg2());
            code |= insEncodeRegT2_N(id->idReg2());
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_D0: // ............nnnn .iiiddddii.wwwww       R1  R2             imm5, imm5
        case IF_T2_D1: // ................ .iiiddddii.wwwww       R1                 imm5, imm5
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());

            if (fmt == IF_T2_D0)
            {
                code |= insEncodeRegT2_N(id->idReg2());
            }

            code |= insEncodeBitFieldImm(imm);
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_E0: // ............nnnn tttt......shmmmm       R1  R2  R3               imm2
        case IF_T2_E1: // ............nnnn tttt............       R1  R2
        case IF_T2_E2: // ................ tttt............       R1
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_T(id->idReg1());

            if (fmt == IF_T2_E0)
            {
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
            else if (fmt == IF_T2_E1)
            {
                code |= insEncodeRegT2_N(id->idReg2());
            }

            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_F1: // ............nnnn ttttdddd....mmmm       R1  R2  R3  R4
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_T(id->idReg1());
            code |= insEncodeRegT2_D(id->idReg2());
            code |= insEncodeRegT2_N(id->idReg3());
            code |= insEncodeRegT2_M(id->idReg4());
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_F2: // ............nnnn aaaadddd....mmmm       R1  R2  R3  R4
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            code |= insEncodeRegT2_N(id->idReg2());
            code |= insEncodeRegT2_M(id->idReg3());
            code |= insEncodeRegT2_T(id->idReg4());
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_G0: // .......PU.W.nnnn ttttTTTTiiiiiiii       R1  R2  R3         imm8, PUW
        case IF_T2_G1: // ............nnnn ttttTTTT........       R1  R2  R3
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_T(id->idReg1());
            code |= insEncodeRegT2_D(id->idReg2());
            code |= insEncodeRegT2_N(id->idReg3());

            if (fmt == IF_T2_G0)
            {
                imm = id->emitGetInsSC();
                assert(unsigned_abs(imm) <= 0x00ff);
                code |= abs(imm);
                code |= insEncodePUW_G0(id->idInsOpt(), imm);
            }

            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_H0: // ............nnnn tttt.PUWiiiiiiii       R1  R2             imm8, PUW
        case IF_T2_H1: // ............nnnn tttt....iiiiiiii       R1  R2             imm8
        case IF_T2_H2: // ............nnnn ........iiiiiiii       R1                 imm8
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_T(id->idReg1());

            if (fmt != IF_T2_H2)
            {
                code |= insEncodeRegT2_N(id->idReg2());
            }

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

        case IF_T2_I0: // ..........W.nnnn rrrrrrrrrrrrrrrr       R1              W, imm16
        case IF_T2_I1: // ................ rrrrrrrrrrrrrrrr                          imm16
            code = emitInsCode(ins, fmt);

            if (fmt == IF_T2_I0)
            {
                code |= insEncodeRegT2_N(id->idReg1());
                code |= (1 << 21); //  W bit
            }

            imm = id->emitGetInsSC();
            assert((imm & 0x3) != 0x3);

            if (imm & 0x2)
            {
                code |= 0x8000; //  PC bit
            }

            if (imm & 0x1)
            {
                code |= 0x4000; //  LR bit
            }

            imm >>= 2;
            assert(imm <= 0x1fff); //  13 bits
            code |= imm;
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_K1: // ............nnnn ttttiiiiiiiiiiii       R1  R2             imm12
        case IF_T2_K4: // ........U....... ttttiiiiiiiiiiii       R1  PC          U, imm12
        case IF_T2_K3: // ........U....... ....iiiiiiiiiiii       PC              U, imm12
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
                {
                    code |= (1 << 23); //  U bit
                }
            }
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_K2: // ............nnnn ....iiiiiiiiiiii       R1                 imm12
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_N(id->idReg1());
            assert(imm <= 0xfff); //  12 bits
            code |= imm;
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_L0: // .....i.....Snnnn .iiiddddiiiiiiii       R1  R2          S, imm8<<imm4
        case IF_T2_L1: // .....i.....S.... .iiiddddiiiiiiii       R1              S, imm8<<imm4
        case IF_T2_L2: // .....i......nnnn .iii....iiiiiiii       R1                 imm8<<imm4
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);

            if (fmt == IF_T2_L2)
            {
                code |= insEncodeRegT2_N(id->idReg1());
            }
            else
            {
                code |= insEncodeSetFlags(id->idInsFlags());
                code |= insEncodeRegT2_D(id->idReg1());
                if (fmt == IF_T2_L0)
                {
                    code |= insEncodeRegT2_N(id->idReg2());
                }
            }

            assert(isModImmConst(imm)); // Funky ARM imm encoding
            imm = encodeModImmConst(imm);
            assert(imm <= 0xfff); //  12 bits
            code |= (imm & 0x00ff);
            code |= (imm & 0x0700) << 4;
            code |= (imm & 0x0800) << 15;
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_M0: // .....i......nnnn .iiiddddiiiiiiii       R1  R2             imm12
            imm  = id->emitGetInsSC();
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            code |= insEncodeRegT2_N(id->idReg2());
            imm = id->emitGetInsSC();
            assert(imm <= 0xfff); //  12 bits
            code |= (imm & 0x00ff);
            code |= (imm & 0x0700) << 4;
            code |= (imm & 0x0800) << 15;
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_N: // .....i......iiii .iiiddddiiiiiiii       R1                 imm16
            assert(!id->idIsCnsReloc());
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            code |= insEncodeImmT2_Mov(id->emitGetInsSC());
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_N2: // .....i......iiii .iiiddddiiiiiiii       R1                 imm16
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            imm  = id->emitGetInsSC();
            addr = roDataBlock + imm;
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

        case IF_T2_N3: // .....i......iiii .iiiddddiiiiiiii       R1                 imm16
            assert((ins == INS_movt) || (ins == INS_movw));
            assert(id->idIsCnsReloc());

            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_D(id->idReg1());
            dst += emitOutput_Thumb2Instr(dst, code);

            if ((ins == INS_movt) && emitComp->info.compMatchedVM)
            {
                emitHandlePCRelativeMov32(dst - 8, id->GetAddr());
            }
            break;

        case IF_T2_VFP3:
            // these are the binary operators
            // d = n - m
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_VectorN(id->idReg2(), size, true);
            code |= insEncodeRegT2_VectorM(id->idReg3(), size, true);
            code |= insEncodeRegT2_VectorD(id->idReg1(), size, true);
            if (size == EA_8BYTE)
            {
                code |= 1 << 8;
            }
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_VFP2:
        {
            emitAttr srcSize;
            emitAttr dstSize;
            uint32_t szCode = 0;

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
                    {
                        szCode |= 1 << 8;
                    }
                    FALLTHROUGH;
                default:
                    srcSize = dstSize = id->idOpSize();
                    break;
            }

            code = emitInsCode(ins, fmt);
            code |= szCode;
            code |= insEncodeRegT2_VectorD(id->idReg1(), dstSize, true);
            code |= insEncodeRegT2_VectorM(id->idReg2(), srcSize, true);

            dst += emitOutput_Thumb2Instr(dst, code);
            break;
        }

        case IF_T2_VLDST:
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT2_N(id->idReg2());
            code |= insEncodeRegT2_VectorD(id->idReg1(), size, true);
            imm = id->emitGetInsSC();

            if (imm < 0)
            {
                imm = -imm; // bit 23 at 0 means negate
            }
            else
            {
                code |= 1 << 23; // set the positive bit
            }

            // offset is +/- 1020
            assert(!(imm % 4));
            assert(imm >> 10 == 0);
            code |= imm >> 2;
            // bit 8 is set for doubles
            if (id->idOpSize() == EA_8BYTE)
            {
                code |= (1 << 8);
            }
            dst += emitOutput_Thumb2Instr(dst, code);
            break;

        case IF_T2_VMOVD:
            // 3op assemble a double from two int regs (or back)
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

        case IF_T1_D1: // .........mmmm...                       R1*
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT1_M4(id->idReg1());
            dst += emitOutput_Thumb1Instr(dst, code);
            break;

        case IF_T1_D2: // .........mmmm...                                R3*
            code = emitInsCode(ins, fmt);
            code |= insEncodeRegT1_M4(id->idReg3());
            dst += emitOutput_Thumb1Instr(dst, code);
            emitRecordGCCall(id, *dp, dst);
            break;

        case IF_T2_J3:                    // .....Siiiiiiiiii ..j.jiiiiiiiiii.      Call                imm24
            if (id->GetAddr() == nullptr) // a recursive call
            {
                addr = hotCodeBlock;
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
                // Clear the lowest bit from target address
                addr = reinterpret_cast<uint8_t*>(reinterpret_cast<size_t>(addr) & ~1);

                // Calculate PC relative displacement
                ptrdiff_t disp = static_cast<uint8_t*>(addr) - (dst + 4);
                bool      S    = disp < 0;
                bool      I1   = (disp & 0x00800000) == 0;
                bool      I2   = (disp & 0x00400000) == 0;

                if (S)
                {
                    code |= (1 << 26); // S bit
                }

                if (S ^ I1)
                {
                    code |= (1 << 13); // J1 bit
                }

                if (S ^ I2)
                {
                    code |= (1 << 11); // J2 bit
                }

                int immLo = (disp & 0x00000ffe) >> 1;
                int immHi = (disp & 0x003ff000) >> 12;

                code |= (immHi << 16);
                code |= immLo;

                disp = abs(disp);
                assert((disp & 0x00fffffe) == disp);

                dst += emitOutput_Thumb2Instr(dst, code);
            }

            emitRecordGCCall(id, *dp, dst);
            break;

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
        PrintIns(id, *dp, dst - *dp);
    }
#endif

    *dp = dst;
}

#ifdef DEBUG

class ArmAsmPrinter final : public AsmPrinter
{
public:
    ArmAsmPrinter(Compiler* compiler, CodeGen* codeGen) : AsmPrinter(compiler, codeGen)
    {
    }

    void Print(instrDesc* id) const;

private:
    void PrintInsName(instruction ins, insFlags flags) const;
    void PrintImm(int imm, bool addComma, bool alwaysHex = false) const;
    void PrintReloc(void* addr) const;
    void PrintFrameRef(instrDesc* id) const;
    void PrintCondition(int cond) const;
    void PrintShiftOpts(insOpts opt) const;
    void PrintRegSet(int imm, bool encodedPC_LR) const;
    void PrintRegRange(RegNum reg, int len, emitAttr attr) const;
    void PrintReg(RegNum reg, emitAttr attr, bool addComma) const;
    void PrintBranchLabel(instrDescJmp* id) const;
    void PrintAddrMode(RegNum reg, emitAttr attr) const;
    void PrintAddrMode(RegNum reg, int imm, emitAttr attr) const;
    void PrintAddrMode(RegNum reg1, RegNum reg2, emitAttr attr) const;
    void PrintAddrMode(RegNum reg1, RegNum reg2, int imm, emitAttr attr) const;
    void PrintAddrMode(RegNum reg, int imm, insOpts opt, emitAttr attr) const;
    void PrintGCType(emitAttr attr) const;
};

static bool insAlwaysSetFlags(instruction ins)
{
    switch (ins)
    {
        case INS_cmp:
        case INS_cmn:
        case INS_teq:
        case INS_tst:
            return true;
        default:
            return false;
    }
}

void ArmAsmPrinter::PrintInsName(instruction ins, insFlags flags) const
{
    static const char pad[8]           = "       ";
    const char*       name             = insName(ins);
    const bool        printFlagsSuffix = insSetsFlags(flags) && !insAlwaysSetFlags(ins);

    printf("%s%s %s", name, printFlagsSuffix ? "s" : "", pad + Min(sizeof(pad) - 1, strlen(name) + printFlagsSuffix));
}

// TODO-MIKE-Cleanup: Get rid of this.
#define STRICT_ARM_ASM 0

void ArmAsmPrinter::PrintImm(int imm, bool addComma, bool alwaysHex) const
{
    if (!alwaysHex && (imm > -1000) && (imm < 1000))
    {
        printf("%d", imm);
    }
    else if ((imm > 0) ||
             (imm == -imm) || // -0x80000000 == 0x80000000. So we don't want to add an extra "-" at the beginning.
             (compiler->opts.disDiffable && (imm == 0xD1FFAB1E))) // Don't display this as negative
    {
        printf("0x%02x", imm);
    }
    else
    {
        printf("-0x%02x", -imm);
    }

    if (addComma)
    {
        printf(", ");
    }
}

void ArmAsmPrinter::PrintReloc(void* addr) const
{
    printf("0x%p", dspPtr(addr));
}

void ArmAsmPrinter::PrintCondition(int cond) const
{
    const static char* armCond[16]{"eq", "ne", "hs", "lo", "mi", "pl", "vs", "vc",
                                   "hi", "ls", "ge", "lt", "gt", "le", "AL", "NV"}; // The last two are invalid
    assert((0 <= cond) && (cond < _countof(armCond)));
    printf(armCond[cond]);
}

void ArmAsmPrinter::PrintRegRange(RegNum reg, int len, emitAttr attr) const
{
    printf("{");

    PrintReg(reg, attr, false);

    if (len > 1)
    {
        printf("-");
        PrintReg(static_cast<RegNum>(reg + len - 1), attr, false);
    }

    printf("}");
}

void ArmAsmPrinter::PrintRegSet(int imm, bool encodedPC_LR) const
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
            printf("%s%s", printedOne ? "," : "", RegName(reg, EA_4BYTE));
            printedOne = true;
            imm -= bit;
        }

        reg = RegNum(reg + 1);
        bit <<= 1;
    }

    if (hasLR)
    {
        printf("%s%s", printedOne ? "," : "", RegName(REG_LR, EA_4BYTE));
        printedOne = true;
    }

    if (hasPC)
    {
        printf("%s%s", printedOne ? "," : "", RegName(REG_PC, EA_4BYTE));
        printedOne = true;
    }

    printf("}");
}

void ArmAsmPrinter::PrintShiftOpts(insOpts opt) const
{
    printf(" %s ", insOptsName(opt));
}

void ArmAsmPrinter::PrintReg(RegNum reg, emitAttr attr, bool addComma) const
{
    if (IsFloatReg(reg))
    {
        printf("%s%s", attr == EA_8BYTE ? "d" : "s", RegName(reg, attr) + 1);
    }
    else
    {
        printf("%s", RegName(reg, EA_4BYTE));
    }

    if (addComma)
    {
        printf(", ");
    }
}

void ArmAsmPrinter::PrintBranchLabel(instrDescJmp* id) const
{
    insFormat fmt = id->idInsFmt();

    if ((fmt == IF_T1_I) || (fmt == IF_T1_J3) || (fmt == IF_T2_M1))
    {
        PrintReg(id->idReg1(), EA_4BYTE, true);
    }
    else if (fmt == IF_T2_N1)
    {
        PrintReg(id->idReg1(), EA_4BYTE, true);
        printf("%s ADDRESS ", id->idIns() == INS_movw ? "LOW" : "HIGH");
    }
    else if ((fmt == IF_T1_K) || (fmt == IF_T1_M))
    {
        printf("SHORT ");
    }

    if (id->HasInstrCount())
    {
        int instrCount = id->GetInstrCount();

        if (id->idjIG == nullptr)
        {
            // This is the instruction synthesized by encoder's PrintIns, we can't
            // get its number because it's not part of an actual instruction group.
            printf("pc%s%d instructions", instrCount >= 0 ? "+" : "", instrCount);
        }
        else
        {
            int distance = static_cast<int>(id->idSmallCns()) - -INT8_MIN + 2;

            // TODO-MIKE-Cleanup: The proper assembly format seems to show the jump
            // distance as an immediate value (e.g. beq #24) not this pc-4 thing.
            // And the instruction count should be displayed as a comment.
            printf("pc%s%d (%d instructions)", distance >= 0 ? "+" : "", distance, instrCount);
        }
    }
    else
    {
        PrintLabel(id->GetLabel());
    }
}

void ArmAsmPrinter::PrintAddrMode(RegNum reg, emitAttr attr) const
{
    printf("[");
    PrintReg(reg, attr, false);
    printf("]");
    PrintGCType(attr);
}

void ArmAsmPrinter::PrintAddrMode(RegNum reg, int imm, emitAttr attr) const
{
    bool regIsSPorFP = (reg == REG_SP) || (reg == REG_FP);

    printf("[");
    PrintReg(reg, attr, false);

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

        PrintImm(imm, false, regIsSPorFP);
    }

    printf("]");
    PrintGCType(attr);
}

void ArmAsmPrinter::PrintAddrMode(RegNum reg1, RegNum reg2, emitAttr attr) const
{
    printf("[");
    PrintReg(reg1, attr, false);
#if STRICT_ARM_ASM
    printf(", ");
#else
    printf("+");
#endif
    PrintReg(reg2, attr, false);
    printf("]");
    PrintGCType(attr);
}

void ArmAsmPrinter::PrintAddrMode(RegNum reg1, RegNum reg2, int imm, emitAttr attr) const
{
    printf("[");
    PrintReg(reg1, attr, false);

#if STRICT_ARM_ASM
    printf(", ");
    PrintReg(reg2, attr, false);

    if (imm > 0)
    {
        printf(" LSL ");
        PrintImm(1 << imm, false);
    }
#else
    printf("+");

    if (imm > 0)
    {
        PrintImm(1 << imm, false);
        printf("*");
    }

    PrintReg(reg2, attr, false);
#endif

    printf("]");
    PrintGCType(attr);
}

void ArmAsmPrinter::PrintAddrMode(RegNum reg, int imm, insOpts opt, emitAttr attr) const
{
    bool regIsSPorFP = (reg == REG_SP) || (reg == REG_FP);

    printf("[");
    PrintReg(reg, attr, false);

    if (insOptAnyInc(opt))
    {
        printf("!");
    }

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

        PrintImm(imm, false, regIsSPorFP);
    }

    printf("]");
    PrintGCType(attr);
}

void ArmAsmPrinter::PrintGCType(emitAttr attr) const
{
#if 0
    // TODO-ARM-Cleanup: Fix or delete.
    if (attr == EA_GCREF)
        printf(" @gc");
    else if (attr == EA_BYREF)
        printf(" @byref");
#endif
}

void ArmAsmPrinter::Print(instrDesc* id) const
{
    printf("      ");

    instruction ins = id->idIns();
    insFormat   fmt = id->idInsFmt();

    if (fmt == IF_GC_REG)
    {
        printf(".gcreg  %s\n", RegName(id->idReg1(), id->idGCref() == GCT_GCREF ? EA_GCREF : EA_BYREF));
        return;
    }

    PrintInsName(ins, id->idInsFlags());

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
        int imm, imm1, imm2;

        case IF_T1_A:
        case IF_T2_A:
            break;

        case IF_T1_L0:
        case IF_T2_B:
            PrintImm(id->emitGetInsSC(), false);
            break;

        case IF_T1_B:
            PrintCondition(id->emitGetInsSC());
            break;

        case IF_T1_L1:
        case IF_T2_I1:
            PrintRegSet(id->emitGetInsSC(), true);
            break;

        case IF_T2_E2:
            if (id->idIns() == INS_vmrs)
            {
                if (id->idReg1() != REG_R15)
                {
                    PrintReg(id->idReg1(), attr, true);
                    printf("FPSCR");
                }
                else
                {
                    printf("APSR, FPSCR");
                }
            }
            else
            {
                PrintReg(id->idReg1(), attr, false);
            }
            break;

        case IF_T1_D1:
            PrintReg(id->idReg1(), attr, false);
            break;

        case IF_T1_D2:
            PrintReg(id->idReg3(), attr, false);

            if (CORINFO_METHOD_HANDLE handle = static_cast<CORINFO_METHOD_HANDLE>(id->idDebugOnlyInfo()->idHandle))
            {
                printf("\t\t// %s", compiler->eeGetMethodFullName(handle));
            }
            break;

        case IF_T1_F:
            PrintReg(REG_SP, attr, true);
            PrintImm(id->emitGetInsSC(), false);
            break;

        case IF_T1_J0:
        case IF_T2_L1:
        case IF_T2_L2:
            PrintReg(id->idReg1(), attr, true);
            imm = id->emitGetInsSC();
            PrintImm(imm, false, false);
            break;

        case IF_T2_N:
            PrintReg(id->idReg1(), attr, true);
            imm = compiler->opts.disDiffable ? 0xD1FF : id->emitGetInsSC();
            PrintImm(imm, false, true);
            break;

        case IF_T2_N3:
            PrintReg(id->idReg1(), attr, true);
            printf("%s RELOC ", ins == INS_movw ? "LOW" : "HIGH");
            PrintReloc(id->GetAddr());
            break;

        case IF_T2_N2:
            PrintReg(id->idReg1(), attr, true);
            printf("%s RWD%02u", ins == INS_movw ? "LOW" : "HIGH", id->emitGetInsSC());
            break;

        case IF_T2_H2:
        case IF_T2_K2:
            PrintAddrMode(id->idReg1(), id->emitGetInsSC(), attr);
            break;

        case IF_T2_K3:
            PrintAddrMode(REG_PC, id->emitGetInsSC(), attr);
            break;

        case IF_T1_J1:
        case IF_T2_I0:
            PrintReg(id->idReg1(), attr, false);
            printf("!, ");
            PrintRegSet(id->emitGetInsSC(), false);
            break;

        case IF_T1_D0:
        case IF_T1_E:
        case IF_T2_C3:
        case IF_T2_C9:
        case IF_T2_C10:
            PrintReg(id->idReg1(), attr, true);
            PrintReg(id->idReg2(), attr, false);

            if ((fmt == IF_T1_E) && (id->idIns() == INS_rsb))
            {
                printf(", 0");
            }
            break;

        case IF_T2_E1:
            PrintReg(id->idReg1(), attr, true);
            PrintAddrMode(id->idReg2(), attr);
            break;

        case IF_T2_D1:
            PrintReg(id->idReg1(), attr, true);
            imm  = id->emitGetInsSC();
            imm1 = (imm >> 5) & 0x1f;
            imm2 = (imm & 0x1f) + 1 - imm1;
            PrintImm(imm1, true);
            PrintImm(imm2, false);
            break;

        case IF_T1_C:
        case IF_T1_G:
        case IF_T2_C2:
        case IF_T2_H1:
        case IF_T2_K1:
        case IF_T2_L0:
        case IF_T2_M0:
            PrintReg(id->idReg1(), attr, true);
            imm = id->emitGetInsSC();

            if (IsLoadStoreIns(ins))
            {
                PrintAddrMode(id->idReg2(), imm, attr);
            }
            else
            {
                PrintReg(id->idReg2(), attr, true);
                PrintImm(imm, false);
            }
            break;

        case IF_T1_J2:
            PrintReg(id->idReg1(), attr, true);
            imm = id->emitGetInsSC();

            if (IsLoadStoreIns(ins))
            {
                PrintAddrMode(REG_SP, imm, attr);
            }
            else
            {
                PrintReg(REG_SP, attr, true);
                PrintImm(imm, false);
            }
            break;

        case IF_T2_K4:
            PrintReg(id->idReg1(), attr, true);
            PrintAddrMode(REG_PC, id->emitGetInsSC(), attr);
            break;

        case IF_T2_C1:
        case IF_T2_C8:
            PrintReg(id->idReg1(), attr, true);
            PrintReg(id->idReg2(), attr, false);
            imm = id->emitGetInsSC();

            if (id->idInsOpt() == INS_OPTS_RRX)
            {
                PrintShiftOpts(id->idInsOpt());
                assert(imm == 1);
            }
            else if (imm > 0)
            {
                PrintShiftOpts(id->idInsOpt());
                PrintImm(imm, false);
            }
            break;

        case IF_T2_C6:
            imm = id->emitGetInsSC();
            PrintReg(id->idReg1(), attr, true);
            PrintReg(id->idReg2(), attr, (imm != 0));

            if (imm != 0)
            {
                PrintImm(imm, false);
            }
            break;

        case IF_T2_C7:
            PrintAddrMode(id->idReg1(), id->idReg2(), id->emitGetInsSC(), attr);
            break;

        case IF_T2_H0:
            PrintReg(id->idReg1(), attr, true);
            PrintAddrMode(id->idReg2(), id->emitGetInsSC(), id->idInsOpt(), attr);
            break;

        case IF_T1_H:
            PrintReg(id->idReg1(), attr, true);

            if (IsLoadStoreIns(ins))
            {
                PrintAddrMode(id->idReg2(), id->idReg3(), attr);
            }
            else
            {
                PrintReg(id->idReg2(), attr, true);
                PrintReg(id->idReg3(), attr, false);
            }
            break;

        case IF_T2_C4:
        case IF_T2_C5:
            PrintReg(id->idReg1(), attr, true);
            PrintReg(id->idReg2(), attr, true);
            PrintReg(id->idReg3(), attr, false);
            break;

        case IF_T2_VFP3:
            PrintReg(id->idReg1(), attr, true);
            PrintReg(id->idReg2(), attr, true);
            PrintReg(id->idReg3(), attr, false);
            break;

        case IF_T2_VFP2:
            switch (id->idIns())
            {
                case INS_vcvt_d2i:
                case INS_vcvt_d2u:
                case INS_vcvt_d2f:
                    PrintReg(id->idReg1(), EA_4BYTE, true);
                    PrintReg(id->idReg2(), EA_8BYTE, false);
                    break;
                case INS_vcvt_i2d:
                case INS_vcvt_u2d:
                case INS_vcvt_f2d:
                    PrintReg(id->idReg1(), EA_8BYTE, true);
                    PrintReg(id->idReg2(), EA_4BYTE, false);
                    break;
                default:
                    // We just use the type on the instruction unless
                    // it is an asymmetrical one like the converts.
                    PrintReg(id->idReg1(), attr, true);
                    PrintReg(id->idReg2(), attr, false);
                    break;
            }
            break;

        case IF_T2_VLDST:
            imm = id->emitGetInsSC();

            switch (id->idIns())
            {
                case INS_vldr:
                case INS_vstr:
                    PrintReg(id->idReg1(), attr, true);
                    PrintAddrMode(id->idReg2(), imm, id->idInsOpt(), attr);
                    break;
                case INS_vldm:
                case INS_vstm:
                    PrintReg(id->idReg2(), attr, false);

                    if (insOptAnyInc(id->idInsOpt()))
                    {
                        printf("!");
                    }

                    printf(", ");
                    FALLTHROUGH;
                default:
                    PrintRegRange(id->idReg1(), abs(imm) >> 2, attr);
                    break;
            }
            break;

        case IF_T2_VMOVD:
            if (ins == INS_vmov_i2d)
            {
                PrintReg(id->idReg1(), attr, true);
                PrintReg(id->idReg2(), EA_4BYTE, true);
                PrintReg(id->idReg3(), EA_4BYTE, false);
            }
            else
            {
                PrintReg(id->idReg1(), EA_4BYTE, true);
                PrintReg(id->idReg2(), EA_4BYTE, true);
                PrintReg(id->idReg3(), attr, false);
            }
            break;

        case IF_T2_VMOVS:
            PrintReg(id->idReg1(), attr, true);
            PrintReg(id->idReg2(), attr, false);
            break;

        case IF_T2_G1:
            PrintReg(id->idReg1(), attr, true);
            PrintAddrMode(id->idReg2(), id->idReg3(), attr);
            break;

        case IF_T2_D0:
            PrintReg(id->idReg1(), attr, true);
            PrintReg(id->idReg2(), attr, true);
            imm  = id->emitGetInsSC();
            imm1 = (imm >> 5) & 0x1f;
            imm2 = (imm & 0x1f) + 1 - (ins == INS_bfi ? imm1 : 0);
            PrintImm(imm1, true);
            PrintImm(imm2, false);
            break;

        case IF_T2_C0:
            PrintReg(id->idReg1(), attr, true);
            PrintReg(id->idReg2(), attr, true);
            PrintReg(id->idReg3(), attr, false);
            imm = id->emitGetInsSC();

            if (id->idInsOpt() == INS_OPTS_RRX)
            {
                PrintShiftOpts(id->idInsOpt());
                assert(imm == 1);
            }
            else if (imm > 0)
            {
                PrintShiftOpts(id->idInsOpt());
                PrintImm(imm, false);
            }
            break;

        case IF_T2_E0:
            PrintReg(id->idReg1(), attr, true);

            if (id->idIsLclVar())
            {
                PrintAddrMode(id->idReg2(), codeGen->rsGetRsvdReg(), 0, attr);
            }
            else
            {
                PrintAddrMode(id->idReg2(), id->idReg3(), id->emitGetInsSC(), attr);
            }
            break;

        case IF_T2_G0:
            PrintReg(id->idReg1(), attr, true);
            PrintReg(id->idReg2(), attr, true);
            PrintAddrMode(id->idReg3(), id->emitGetInsSC(), id->idInsOpt(), attr);
            break;

        case IF_T2_F1:
        case IF_T2_F2:
            PrintReg(id->idReg1(), attr, true);
            PrintReg(id->idReg2(), attr, true);
            PrintReg(id->idReg3(), attr, true);
            PrintReg(id->idReg4(), attr, false);
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
            PrintBranchLabel(static_cast<instrDescJmp*>(id));
            break;

        case IF_T2_J3:
            printf("%s",
                   compiler->eeGetMethodFullName(static_cast<CORINFO_METHOD_HANDLE>(id->idDebugOnlyInfo()->idHandle)));
            break;

        default:
            printf("unexpected format %s", EmitterBase::emitIfName(fmt));
            assert(!"unexpectedFormat");
            break;
    }

    if (id->idIsLclVar())
    {
        PrintFrameRef(id);
    }

    printf("\n");
}

void ArmEmitter::PrintIns(instrDesc* id)
{
    ArmAsmPrinter printer(emitComp, codeGen);
    printer.Print(id);
}

static void PrintHexCode(uint8_t* code, size_t sz)
{
    if (sz == 2)
    {
        printf("  %04X     ", reinterpret_cast<uint16_t*>(code)[0]);
    }
    else if (sz == 4)
    {
        printf("  %04X %04X", reinterpret_cast<uint16_t*>(code)[0], reinterpret_cast<uint16_t*>(code)[1]);
    }
    else
    {
        printf("           ");
    }
}

void ArmEncoder::PrintIns(instrDesc* id, uint8_t* code, size_t sz)
{
    insFormat fmt = id->idInsFmt();

    auto Print = [&](instrDesc* id, uint8_t* code, size_t sz) {
        JITDUMP("IN%04X: ", id->idDebugOnlyInfo()->idNum);

        PrintInsAddr(code);

        if (!emitComp->opts.disDiffable)
        {
            PrintHexCode(code, sz);
        }

        ArmAsmPrinter printer(emitComp, codeGen);
        printer.Print(id);
    };

    if ((fmt == IF_LARGEJMP) && static_cast<instrDescJmp*>(id)->HasLabel())
    {
        instrDescJmp* ij = static_cast<instrDescJmp*>(id);
        instrDescJmp  idJmp;
        memset(&idJmp, 0, sizeof(idJmp));
        idJmp.idIns(JumpKindToJcc(ReverseJumpKind(JccToJumpKind(id->idIns()))));
        idJmp.idInsFmt(IF_T1_K);
        idJmp.idInsSize(ISZ_16BIT);
        idJmp.SetInstrCount(1);
        idJmp.idDebugOnlyInfo(id->idDebugOnlyInfo()); // share the idDebugOnlyInfo() field
        Print(&idJmp, code, 2);
        code += 2;
        memset(&idJmp, 0, sizeof(idJmp));
        idJmp.idIns(INS_b);
        idJmp.idInsFmt(IF_T2_J2);
        idJmp.idInsSize(ISZ_32BIT);
        idJmp.SetLabel(ij->GetLabel());
        idJmp.idDebugOnlyInfo(id->idDebugOnlyInfo()); // share the idDebugOnlyInfo() field
        Print(&idJmp, code, 4);
    }
    else
    {
        Print(id, code, sz);
    }
}

void ArmAsmPrinter::PrintFrameRef(instrDesc* id) const
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

void Encoder::PrintAlignmentBoundary(size_t instrAddr, size_t instrEndAddr, const instrDesc* instr, const instrDesc*)
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

#endif // DEBUG

#if defined(DEBUG) || defined(LATE_DISASM)

Encoder::insExecutionCharacteristics Encoder::getInsExecutionCharacteristics(instrDesc* id)
{
    // ToDo: Calculate actual throughput and latency values
    insExecutionCharacteristics result;
    result.insThroughput = PERFSCORE_THROUGHPUT_1C;
    result.insLatency    = PERFSCORE_LATENCY_1C;
    return result;
}

#endif // defined(DEBUG) || defined(LATE_DISASM)

#endif // TARGET_ARM
