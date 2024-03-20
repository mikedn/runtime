// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_ARM64

friend class Arm64Encoder;

public:
// The ARM64 instructions are all 32 bits in size.
// we use an unsigned int to hold the encoded instructions.
// This typedef defines the type that we use to hold encoded instructions.
using code_t = uint32_t;

private:
static bool strictArmAsm;

/************************************************************************/
/*         Routines that compute the size of / encode instructions      */
/************************************************************************/

static size_t emitGetInstrDescSize(const instrDesc* id);

#ifdef DEBUG

/************************************************************************/
/*             Debug-only routines to display instructions              */
/************************************************************************/

const char* emitVectorRegName(regNumber reg);

void emitDispInsHex(instrDesc* id, BYTE* code, size_t sz);
void emitDispInst(instruction ins);
void emitDispLargeImm(instrDesc* id, insFormat fmt, ssize_t imm);
void emitDispAddrLoadLabel(instrDescJmp* id);
void emitDispJumpLabel(instrDescJmp* id);
void emitDispImm(ssize_t imm, bool addComma, bool alwaysHex = false);
void emitDispFrameRef(instrDesc* id);
void emitDispFloatZero();
void emitDispFloatImm(ssize_t imm8);
void emitDispImmOptsLSL12(ssize_t imm, insOpts opt);
void emitDispCond(insCond cond);
void emitDispFlags(insCflags flags);
void emitDispBarrier(insBarrier barrier);
void emitDispShiftOpts(insOpts opt);
void emitDispExtendOpts(insOpts opt);
void emitDispLSExtendOpts(insOpts opt);
void emitDispReg(regNumber reg, emitAttr attr, bool addComma);
void emitDispVectorReg(regNumber reg, insOpts opt, bool addComma);
void emitDispVectorRegIndex(regNumber reg, emitAttr elemsize, ssize_t index, bool addComma);
void emitDispVectorRegList(regNumber firstReg, unsigned listSize, insOpts opt, bool addComma);
void emitDispVectorElemList(regNumber firstReg, unsigned listSize, emitAttr elemsize, unsigned index, bool addComma);
void emitDispArrangement(insOpts opt);
void emitDispElemsize(emitAttr elemsize);
void emitDispShiftedReg(regNumber reg, insOpts opt, ssize_t imm, emitAttr attr);
void emitDispExtendReg(regNumber reg, insOpts opt, ssize_t imm);
void emitDispAddrRI(regNumber reg, insOpts opt, ssize_t imm);
void emitDispAddrRRExt(regNumber reg1, regNumber reg2, insOpts opt, bool isScaled, emitAttr size);

void emitDispIns(instrDesc* id,
                 bool       isNew = false,
                 bool       doffs = false,
                 bool       asmfm = false,
                 unsigned   offs  = 0,
                 uint8_t*   code  = 0,
                 size_t     sz    = 0);
#endif // DEBUG

/************************************************************************/
/*  Private members that deal with target-dependent instr. descriptors  */
/************************************************************************/

private:
instrDesc* emitNewInstrCall(CORINFO_METHOD_HANDLE methodHandle, emitAttr retSize, emitAttr secondRetSize);

/************************************************************************/
/*               Private helpers for instruction output                 */
/************************************************************************/

private:
static bool emitInsIsCompare(instruction ins);
static bool emitInsIsVectorLong(instruction ins);
static bool emitInsIsVectorNarrow(instruction ins);
static bool emitInsIsVectorWide(instruction ins);
emitAttr emitInsTargetRegSize(instrDesc* id);
emitAttr emitInsLoadStoreSize(instrDesc* id);

// A helper method to return the natural scale for an EA 'size'
static unsigned NaturalScale_helper(emitAttr size);

// A helper method to perform a Rotate-Right shift operation
static UINT64 ROR_helper(UINT64 value, unsigned sh, unsigned width);

// A helper method to perform a 'NOT' bitwise complement operation
static UINT64 NOT_helper(UINT64 value, unsigned width);

// A helper method to perform a bit Replicate operation
static UINT64 Replicate_helper(UINT64 value, unsigned width, emitAttr size);

// Method to do check if mov is redundant with respect to the last instruction.
// If yes, the caller of this method can choose to omit current mov instruction.
bool IsRedundantMov(instruction ins, emitAttr size, regNumber dst, regNumber src, bool canSkip);
bool IsRedundantLdStr(instruction ins, regNumber reg1, regNumber reg2, ssize_t imm, emitAttr size, insFormat fmt);

/************************************************************************
*
* This union is used to to encode/decode the special ARM64 immediate values
* that is listed as imm(N,r,s) and referred to as 'bitmask immediate'
*/

public:
static bool emitInsIsLoad(instruction ins);
static bool emitInsIsStore(instruction ins);
static bool emitInsIsLoadOrStore(instruction ins);
//  For the given 'arrangement' returns the 'elemsize' specified by the vector register arrangement
static emitAttr optGetElemsize(insOpts arrangement);
//    For the given 'datasize', 'elemsize' and 'index' returns true, if it specifies a valid 'index'
//    for an element of size 'elemsize' in a vector register of size 'datasize'
static bool isValidVectorIndex(emitAttr datasize, emitAttr elemsize, ssize_t index);

static bool IsMovInstruction(instruction ins);

union bitMaskImm {
    struct
    {
        unsigned immS : 6; // bits 0..5
        unsigned immR : 6; // bits 6..11
        unsigned immN : 1; // bits 12
    };
    unsigned immNRS; // concat N:R:S forming a 13-bit unsigned immediate
};

/************************************************************************
*
*  Convert between a 64-bit immediate and its 'bitmask immediate'
*   representation imm(i16,hw)
*/

static bitMaskImm emitEncodeBitMaskImm(int64_t imm, emitAttr size);
static int64_t emitDecodeBitMaskImm(const bitMaskImm bmImm, emitAttr size);

private:
/************************************************************************
*
* This union is used to to encode/decode the special ARM64 immediate values
* that is listed as imm(i16,hw) and referred to as 'halfword immediate'
*/

union halfwordImm {
    struct
    {
        unsigned immVal : 16; // bits  0..15
        unsigned immHW : 2;   // bits 16..17
    };
    unsigned immHWVal; // concat HW:Val forming a 18-bit unsigned immediate
};

/************************************************************************
*
*  Convert between a 64-bit immediate and its 'halfword immediate'
*   representation imm(i16,hw)
*/

static halfwordImm emitEncodeHalfwordImm(int64_t imm, emitAttr size);
static int64_t emitDecodeHalfwordImm(const halfwordImm hwImm, emitAttr size);

/************************************************************************
*
* This union is used to encode/decode the special ARM64 immediate values
* that is listed as imm(i16,by) and referred to as 'byteShifted immediate'
*/

union byteShiftedImm {
    struct
    {
        unsigned immVal : 8;  // bits  0..7
        unsigned immBY : 2;   // bits  8..9
        unsigned immOnes : 1; // bit   10
    };
    unsigned immBSVal; // concat Ones:BY:Val forming a 10-bit unsigned immediate
};

static uint32_t emitDecodeByteShiftedImm(const byteShiftedImm bsImm, emitAttr size);

/************************************************************************
*
* This union is used to to encode/decode the special ARM64 immediate values
* that are use for FMOV immediate and referred to as 'float 8-bit immediate'
*/

union floatImm8 {
    struct
    {
        unsigned immMant : 4; // bits 0..3
        unsigned immExp : 3;  // bits 4..6
        unsigned immSign : 1; // bits 7
    };
    unsigned immFPIVal; // concat Sign:Exp:Mant forming an 8-bit unsigned immediate
};

/************************************************************************
*
*  Convert between a double and its 'float 8-bit immediate' representation
*/

static floatImm8 emitEncodeFloatImm8(double immDbl);
static double emitDecodeFloatImm8(const floatImm8 fpImm);

/************************************************************************
*
*  This union is used to to encode/decode the cond, nzcv and imm5 values for
*   instructions that use them in the small constant immediate field
*/

public:
union condFlagsImm {
    struct
    {
        insCond   cond : 4;  // bits  0..3
        insCflags flags : 4; // bits  4..7
        unsigned  imm5 : 5;  // bits  8..12
    };
    unsigned immCFVal; // concat imm5:flags:cond forming an 13-bit unsigned immediate
};

private:
//  For the given 'ins' returns the reverse instruction, if one exists, otherwise returns INS_INVALID
static instruction insReverse(instruction ins);

//  For the given 'datasize' and 'elemsize' returns the insOpts that specifies the vector register arrangement
static insOpts optMakeArrangement(emitAttr datasize, emitAttr elemsize);

//    For the given 'datasize' and 'opt' returns true if it specifies a valid vector register arrangement
static bool isValidArrangement(emitAttr datasize, insOpts opt);

//  For the given 'arrangement' returns the 'datasize' specified by the vector register arrangement
static emitAttr optGetDatasize(insOpts arrangement);

//  For the given 'arrangement' returns the one with the element width that is double that of the 'arrangement' element.
static insOpts optWidenElemsizeArrangement(insOpts arrangement);

//  For the given 'datasize' returns the one that is double that of the 'datasize'.
static emitAttr widenDatasize(emitAttr datasize);

//  For the given 'srcArrangement' returns the "widen" 'dstArrangement' specifying the destination vector register
//  arrangement
//  of Long Pairwise instructions. Note that destination vector elements twice as long as the source vector elements.
static insOpts optWidenDstArrangement(insOpts srcArrangement);

//  For the given 'conversion' returns the 'dstsize' specified by the conversion option
static emitAttr optGetDstsize(insOpts conversion);

//  For the given 'conversion' returns the 'srcsize' specified by the conversion option
static emitAttr optGetSrcsize(insOpts conversion);

// For a given instruction 'ins' which contains a register lists returns a
// number of consecutive SIMD registers the instruction loads to/store from.
static unsigned insGetRegisterListSize(instruction ins);

/************************************************************************/
/*           Public inline informational methods                        */
/************************************************************************/

public:
// true if this 'imm' can be encoded as a input operand to a mov instruction
static bool emitIns_valid_imm_for_mov(INT64 imm, emitAttr size);

struct MoviImm
{
    instruction ins;
    uint8_t     imm;
    uint8_t     shift;
    bool        msl;

    MoviImm() : ins(INS_invalid), imm(0), shift(0), msl(0)
    {
    }

    MoviImm(instruction ins, uint64_t imm, unsigned shift = 0, bool msl = false)
        : ins(ins), imm(static_cast<uint8_t>(imm)), shift(static_cast<uint8_t>(shift)), msl(msl)
    {
        assert(imm <= UINT8_MAX);
        assert(shift <= 24);
    }
};

// true if this 'imm' can be encoded as a input operand to a vector movi instruction
static MoviImm EncodeMoviImm(uint64_t value, insOpts opt);

// true if this 'immDbl' can be encoded as a input operand to a fmov instruction
static bool emitIns_valid_imm_for_fmov(double immDbl);

// true if this 'imm' can be encoded as a input operand to an add instruction
static bool emitIns_valid_imm_for_add(INT64 imm, emitAttr size = EA_8BYTE);

// true if this 'imm' can be encoded as a input operand to a cmp instruction
static bool emitIns_valid_imm_for_cmp(INT64 imm, emitAttr size);

// true if this 'imm' can be encoded as a input operand to an alu instruction
static bool emitIns_valid_imm_for_alu(INT64 imm, emitAttr size);

// true if this 'imm' can be encoded as the offset in a ldr/str instruction
static bool emitIns_valid_imm_for_ldst_offset(INT64 imm, emitAttr size);

static bool validImmForBL(ssize_t addr, Compiler* compiler);

// true if 'imm' can use the left shifted by 12 bits encoding
static bool canEncodeWithShiftImmBy12(INT64 imm);

// Normalize the 'imm' so that the upper bits, as defined by 'size' are zero
static INT64 normalizeImm64(INT64 imm, emitAttr size);

// Normalize the 'imm' so that the upper bits, as defined by 'size' are zero
static INT32 normalizeImm32(INT32 imm, emitAttr size);

// true if 'imm' can be encoded using a 'bitmask immediate', also returns the encoding if wbBMI is non-null
static bool canEncodeBitMaskImm(int64_t imm, emitAttr size, bitMaskImm* wbBMI = nullptr);

// true if 'imm' can be encoded using a 'halfword immediate', also returns the encoding if wbHWI is non-null
static bool canEncodeHalfwordImm(int64_t imm, emitAttr size, halfwordImm* wbHWI = nullptr);

// true if 'imm' can be encoded using a 'byteShifted immediate', also returns the encoding if wbBSI is non-null
static bool canEncodeByteShiftedImm(int64_t imm, emitAttr size, byteShiftedImm* wbBSI = nullptr);

// true if 'immDbl' can be encoded using a 'float immediate', also returns the encoding if wbFPI is non-null
static bool canEncodeFloatImm8(double immDbl, floatImm8* wbFPI = nullptr);

// Returns the number of bits used by the given 'size'.
inline static unsigned getBitWidth(emitAttr size)
{
    assert(size <= EA_8BYTE);
    return (unsigned)size * BITS_PER_BYTE;
}

/************************************************************************/
/*           The public entry points to output instructions             */
/************************************************************************/

public:
static insCond emitJumpKindToCond(emitJumpKind jumpKind);
static instruction emitJumpKindToBranch(emitJumpKind jumpKind);

void emitIns(instruction ins);

void emitIns_J(instruction ins, int instrCount);
void emitIns_J(instruction ins, insGroup* label);
void emitIns_CallFinally(insGroup* label);

void emitIns_BRK(uint16_t imm);

void emitIns_R(instruction ins, emitAttr attr, regNumber reg);

void emitIns_R_I(instruction ins, emitAttr attr, regNumber reg, ssize_t imm, insOpts opt = INS_OPTS_NONE);

void emitIns_R_F(instruction ins, emitAttr attr, regNumber reg, double immDbl, insOpts opt = INS_OPTS_NONE);

void emitIns_Mov(
    instruction ins, emitAttr attr, regNumber dstReg, regNumber srcReg, bool canSkip, insOpts opt = INS_OPTS_NONE);

void emitIns_R_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, insOpts opt = INS_OPTS_NONE);

void emitIns_R_I_I(
    instruction ins, emitAttr attr, regNumber reg1, ssize_t imm1, ssize_t imm2, insOpts opt = INS_OPTS_NONE);

void emitIns_R_R_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, ssize_t imm, insOpts opt = INS_OPTS_NONE);

// Checks for a large immediate that needs a second instruction
void emitIns_R_R_Imm(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, ssize_t imm);

void emitIns_R_R_R(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, insOpts opt = INS_OPTS_NONE);

void emitIns_R_R_R_I(instruction ins,
                     emitAttr    attr,
                     regNumber   reg1,
                     regNumber   reg2,
                     regNumber   reg3,
                     int32_t     imm,
                     insOpts     opt      = INS_OPTS_NONE,
                     emitAttr    attrReg2 = EA_UNKNOWN);

void emitIns_R_R_R_Ext(instruction ins,
                       emitAttr    attr,
                       regNumber   reg1,
                       regNumber   reg2,
                       regNumber   reg3,
                       insOpts     opt         = INS_OPTS_NONE,
                       int         shiftAmount = -1);

void emitIns_R_R_I_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int imm1, int imm2, insOpts opt = INS_OPTS_NONE);

void emitIns_R_R_R_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, regNumber reg4);

void emitIns_R_COND(instruction ins, emitAttr attr, regNumber reg, insCond cond);

void emitIns_R_R_COND(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, insCond cond);

void emitIns_R_R_R_COND(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, insCond cond);

void emitIns_R_R_FLAGS_COND(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, insCflags flags, insCond cond);

void emitIns_R_I_FLAGS_COND(instruction ins, emitAttr attr, regNumber reg1, int imm, insCflags flags, insCond cond);

void emitIns_BARR(instruction ins, insBarrier barrier);

void emitIns_S_R(instruction ins, emitAttr attr, regNumber ireg, StackAddrMode s);

void emitIns_S_S_R_R(instruction ins, emitAttr attr, emitAttr attr2, regNumber ireg, regNumber ireg2, StackAddrMode s);

void emitIns_R_S(instruction ins, emitAttr attr, regNumber ireg, StackAddrMode s);

void emitIns_R_R_S_S(instruction ins, emitAttr attr, emitAttr attr2, regNumber ireg, regNumber ireg2, StackAddrMode s);

void Ins_R_S(instruction ins, emitAttr attr, regNumber reg, StackAddrMode s);
void Ins_R_R_S(instruction ins, emitAttr attr1, emitAttr attr2, regNumber reg1, regNumber reg2, StackAddrMode s);

void emitIns_S_I(instruction ins, emitAttr attr, StackAddrMode s, int val);

void emitIns_R_C(instruction ins, emitAttr attr, regNumber reg, regNumber tmpReg, ConstData* data);

void emitIns_R_L(RegNum reg, insGroup* label);

void emitIns_J_R(instruction ins, emitAttr attr, insGroup* label, regNumber reg);

void emitIns_J_R_I(instruction ins, emitAttr attr, insGroup* label, regNumber reg, int imm);

void emitIns_R_AH(RegNum reg,
                  void* addr DEBUGARG(void* handle = nullptr) DEBUGARG(HandleKind handleKind = HandleKind::None));

enum EmitCallType
{
    EC_FUNC_TOKEN, // Direct call to a helper/static/nonvirtual/global method
    EC_INDIR_R     // Indirect call via register
};

void emitIns_Call(EmitCallType          kind,
                  CORINFO_METHOD_HANDLE methodHandle DEBUGARG(CORINFO_SIG_INFO* sigInfo),
                  void*     addr,
                  emitAttr  retRegAttr,
                  emitAttr  retReg2Attr,
                  regNumber reg    = REG_NA,
                  bool      isJump = false);

private:
void emitSetShortJump(instrDescJmp* id);

/*****************************************************************************
 *
 *  Given an instrDesc, return true if it's a conditional jump.
 */

static bool emitIsCondJump(instrDesc* jmp)
{
    return ((jmp->idInsFmt() == IF_BI_0B) || (jmp->idInsFmt() == IF_BI_1A) || (jmp->idInsFmt() == IF_BI_1B) ||
            (jmp->idInsFmt() == IF_LARGEJMP));
}

/*****************************************************************************
 *
 *  Given a instrDesc, return true if it's an unconditional jump.
 */

static bool emitIsUncondJump(instrDesc* jmp)
{
    return (jmp->idInsFmt() == IF_BI_0A);
}

/*****************************************************************************
 *
 *  Given a instrDesc, return true if it's a direct call.
 */

static bool emitIsDirectCall(instrDesc* call)
{
    return (call->idInsFmt() == IF_BI_0C);
}

/*****************************************************************************
 *
 *  Given a instrDesc, return true if it's a load label instruction.
 */

static bool emitIsLoadLabel(instrDesc* jmp)
{
    return ((jmp->idInsFmt() == IF_DI_1E) || // adr or arp
            (jmp->idInsFmt() == IF_LARGEADR));
}

/*****************************************************************************
*
*  Given a instrDesc, return true if it's a load constant instruction.
*/

static bool emitIsLoadConstant(instrDesc* jmp)
{
    return ((jmp->idInsFmt() == IF_LS_1A) || // ldr
            (jmp->idInsFmt() == IF_LARGELDC));
}

/************************************************************************/
/*                   Interface for generating unwind information        */
/************************************************************************/
public:
void emitUnwindNopPadding(const emitLocation& loc);

private:
template <typename T>
T* AllocInstr(bool updateLastIns = true);

instrDesc* emitNewInstr();
instrDesc* emitNewInstrSmall();
instrDesc* emitNewInstrSC(int64_t imm);
instrDesc* emitNewInstrCns(int32_t imm);
instrDesc* emitNewInstrGCReg(emitAttr attr, regNumber reg);
instrDescJmp*  emitNewInstrJmp();
instrDescCGCA* emitAllocInstrCGCA();

#endif // TARGET_ARM64
