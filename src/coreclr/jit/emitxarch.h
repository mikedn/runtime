// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_XARCH

public:
static bool isGeneralRegister(regNumber reg)
{
    return genIsValidIntReg(reg);
}

static bool isFloatReg(regNumber reg)
{
    return genIsValidFloatReg(reg);
}

static bool isDoubleReg(regNumber reg)
{
    return isFloatReg(reg);
}

// code_t is a type used to accumulate bits of opcode + prefixes. On amd64, it must be 64 bits
// to support the REX prefixes. On both x86 and amd64, it must be 64 bits to support AVX, with
// its 3-byte VEX prefix.
typedef uint64_t code_t;

#ifdef TARGET_X86
void emitMarkStackLvl(unsigned stackLevel);
#endif

instrDescDsp* emitAllocInstrDsp(emitAttr attr);
instrDescCnsDsp* emitAllocInstrCnsDsp(emitAttr attr);
instrDescAmd* emitAllocInstrAmd(emitAttr attr);
instrDescCnsAmd* emitAllocInstrCnsAmd(emitAttr attr);

ssize_t emitGetInsCns(instrDesc* id);
ssize_t emitGetInsMemDisp(instrDesc* id);
ssize_t emitGetInsMemImm(instrDesc* id);
ssize_t emitGetInsAmdCns(instrDesc* id);
ssize_t emitGetInsAmdDisp(instrDesc* id);
ssize_t emitGetInsCallDisp(instrDesc* id);

UNATIVE_OFFSET emitInsSize(code_t code, bool includeRexPrefixSize);
UNATIVE_OFFSET emitInsSizeSV_AM(instrDesc* id, code_t code);
UNATIVE_OFFSET emitInsSizeSV(instrDesc* id, code_t code);
UNATIVE_OFFSET emitInsSizeSV(instrDesc* id, code_t code, int val);
UNATIVE_OFFSET emitInsSizeRR(instrDesc* id, code_t code);
UNATIVE_OFFSET emitInsSizeRR(instrDesc* id, code_t code, int val);
UNATIVE_OFFSET emitInsSizeRR(instruction ins, regNumber reg1, regNumber reg2, emitAttr attr);
UNATIVE_OFFSET emitInsSizeAM(instrDesc* id, code_t code);
UNATIVE_OFFSET emitInsSizeAM(instrDesc* id, code_t code, int val);
UNATIVE_OFFSET emitInsSizeCV(instrDesc* id, code_t code);
UNATIVE_OFFSET emitInsSizeCV(instrDesc* id, code_t code, int val);

size_t emitOutputByte(uint8_t* dst, ssize_t val);
size_t emitOutputWord(uint8_t* dst, ssize_t val);
size_t emitOutputLong(uint8_t* dst, ssize_t val);
AMD64_ONLY(size_t emitOutputI64(uint8_t* dst, int64_t val);)

#if defined(TARGET_X86) && !defined(HOST_64BIT)
size_t emitOutputByte(uint8_t* dst, uint32_t val);
size_t emitOutputWord(uint8_t* dst, uint32_t val);
size_t emitOutputLong(uint8_t* dst, uint32_t val);

size_t emitOutputByte(uint8_t* dst, uint64_t val);
size_t emitOutputWord(uint8_t* dst, uint64_t val);
size_t emitOutputLong(uint8_t* dst, uint64_t val);
#endif // defined(TARGET_X86) && !defined(HOST_64BIT)

size_t emitOutputImm(uint8_t* dst, instrDesc* id, size_t size, ssize_t imm);

uint8_t* emitOutputAlign(insGroup* ig, instrDesc* id, uint8_t* dst);

uint8_t* emitOutputAM(uint8_t* dst, instrDesc* id, code_t code, ssize_t* imm = nullptr);
uint8_t* emitOutputSV(uint8_t* dst, instrDesc* id, code_t code, ssize_t* imm = nullptr);
uint8_t* emitOutputCV(uint8_t* dst, instrDesc* id, code_t code, ssize_t* imm = nullptr);

uint8_t* emitOutputR(uint8_t* dst, instrDesc* id);
uint8_t* emitOutputRI(uint8_t* dst, instrDesc* id);
uint8_t* emitOutputRR(uint8_t* dst, instrDesc* id);
uint8_t* emitOutputIV(uint8_t* dst, instrDesc* id);
uint8_t* emitOutputRRR(uint8_t* dst, instrDesc* id);
uint8_t* emitOutputLJ(insGroup* ig, uint8_t* dst, instrDesc* id);

size_t emitOutputRexOrVexPrefixIfNeeded(instruction ins, uint8_t* dst, code_t& code);
unsigned emitGetRexPrefixSize(instruction ins);
unsigned emitGetVexPrefixSize(instruction ins, emitAttr attr);
unsigned emitGetPrefixSize(code_t code, bool includeRexPrefixSize);
unsigned emitGetAdjustedSize(instruction ins, emitAttr attr, code_t code);

unsigned insEncodeReg012(instruction ins, regNumber reg, emitAttr size, code_t* code);
unsigned insEncodeReg345(instruction ins, regNumber reg, emitAttr size, code_t* code);
code_t insEncodeReg3456(instruction ins, regNumber reg, emitAttr size, code_t code);

code_t insEncodeMRreg(instruction ins, code_t code);
code_t insEncodeRMreg(instruction ins, code_t code);
code_t insEncodeMRreg(instruction ins, regNumber reg, emitAttr size, code_t code);
code_t insEncodeRRIb(instruction ins, regNumber reg, emitAttr size);
code_t insEncodeOpreg(instruction ins, regNumber reg, emitAttr size);

static bool IsSSEInstruction(instruction ins);
static bool IsSSEOrAVXInstruction(instruction ins);
static bool IsAVXOnlyInstruction(instruction ins);
static bool IsFMAInstruction(instruction ins);
static bool IsAVXVNNIInstruction(instruction ins);
static bool IsBMIInstruction(instruction ins);
static regNumber getBmiRegNumber(instruction ins);
static regNumber getSseShiftRegNumber(instruction ins);
bool IsAVXInstruction(instruction ins) const;
code_t insEncodeMIreg(instruction ins, regNumber reg, emitAttr size, code_t code);

code_t AddRexWPrefix(instruction ins, code_t code);
code_t AddRexRPrefix(instruction ins, code_t code);
code_t AddRexXPrefix(instruction ins, code_t code);
code_t AddRexBPrefix(instruction ins, code_t code);
code_t AddRexPrefix(instruction ins, code_t code);

bool EncodedBySSE38orSSE3A(instruction ins);
bool Is4ByteSSEInstruction(instruction ins);
static bool IsMovInstruction(instruction ins);
bool IsRedundantMov(
    instruction ins, insFormat fmt, emitAttr size, regNumber dst, regNumber src, bool canIgnoreSideEffects);

static bool IsJccInstruction(instruction ins);
static bool IsJmpInstruction(instruction ins);

bool AreUpper32BitsZero(regNumber reg);

bool AreFlagsSetToZeroCmp(regNumber reg, emitAttr opSize, genTreeOps treeOps);

bool hasRexPrefix(code_t code)
{
#ifdef TARGET_AMD64
    const code_t REX_PREFIX_MASK = 0xFF00000000LL;
    return (code & REX_PREFIX_MASK) != 0;
#else  // !TARGET_AMD64
    return false;
#endif // !TARGET_AMD64
}

// 3-byte VEX prefix starts with byte 0xC4
#define VEX_PREFIX_MASK_3BYTE 0xFF000000000000ULL
#define VEX_PREFIX_CODE_3BYTE 0xC4000000000000ULL

bool TakesVexPrefix(instruction ins) const;
static bool TakesRexWPrefix(instruction ins, emitAttr attr);

// Returns true if the instruction encoding already contains VEX prefix
bool hasVexPrefix(code_t code)
{
    return (code & VEX_PREFIX_MASK_3BYTE) == VEX_PREFIX_CODE_3BYTE;
}
code_t AddVexPrefix(instruction ins, code_t code, emitAttr attr);
code_t AddVexPrefixIfNeeded(instruction ins, code_t code, emitAttr size)
{
    if (TakesVexPrefix(ins))
    {
        code = AddVexPrefix(ins, code, size);
    }
    return code;
}
code_t AddVexPrefixIfNeededAndNotPresent(instruction ins, code_t code, emitAttr size)
{
    if (TakesVexPrefix(ins) && !hasVexPrefix(code))
    {
        code = AddVexPrefix(ins, code, size);
    }
    return code;
}

bool useVEXEncodings = false;
bool UseVEXEncoding() const
{
    return useVEXEncodings;
}
void SetUseVEXEncoding(bool value)
{
    useVEXEncodings = value;
}

bool containsAVXInstruction = false;
bool ContainsAVX()
{
    return containsAVXInstruction;
}
void SetContainsAVX()
{
    containsAVXInstruction = true;
}

bool contains256bitAVXInstruction = false;
bool Contains256bitAVX()
{
    return contains256bitAVXInstruction;
}
void SetContains256bitAVX()
{
    contains256bitAVXInstruction = true;
}

bool IsDstDstSrcAVXInstruction(instruction ins);
bool IsDstSrcSrcAVXInstruction(instruction ins);
bool DoesWriteZeroFlag(instruction ins);
bool DoesResetOverflowAndCarryFlags(instruction ins);
bool IsFlagsAlwaysModified(instrDesc* id);

bool IsThreeOperandAVXInstruction(instruction ins)
{
    return (IsDstDstSrcAVXInstruction(ins) || IsDstSrcSrcAVXInstruction(ins));
}
bool isAvxBlendv(instruction ins)
{
    return ins == INS_vblendvps || ins == INS_vblendvpd || ins == INS_vpblendvb;
}
bool isSse41Blendv(instruction ins)
{
    return ins == INS_blendvps || ins == INS_blendvpd || ins == INS_pblendvb;
}
bool isPrefetch(instruction ins)
{
    return (ins == INS_prefetcht0) || (ins == INS_prefetcht1) || (ins == INS_prefetcht2) || (ins == INS_prefetchnta);
}

/************************************************************************/
/*             Debug-only routines to display instructions              */
/************************************************************************/

#ifdef DEBUG

void emitDispImm(instrDesc* id, ssize_t val);
void emitDispReloc(ssize_t value);
void emitDispAddrMode(instrDesc* id);
void emitDispClsVar(instrDesc* id);
void emitDispShiftCL(instruction ins);

void emitDispIns(instrDesc* id,
                 bool       isNew = false,
                 bool       doffs = false,
                 bool       asmfm = false,
                 unsigned   offs  = 0,
                 BYTE*      code  = nullptr,
                 size_t     sz    = 0,
                 insGroup*  ig    = nullptr);

static const char* emitXMMregName(unsigned reg);
static const char* emitYMMregName(unsigned reg);

#endif

/************************************************************************/
/*  Private members that deal with target-dependent instr. descriptors  */
/************************************************************************/

private:
void SetInstrLclAddrMode(instrDesc* id, int varNum, int varOffs);
ssize_t GetAddrModeDisp(GenTree* addr);
void SetInstrAddrMode(instrDesc* id, insFormat fmt, instruction ins, GenTree* addr);
void emitSetAmdDisp(instrDescAmd* id, ssize_t dsp);
instrDesc* emitNewInstrDsp(emitAttr attr, target_ssize_t dsp);
instrDesc* emitNewInstrCnsDsp(emitAttr attr, target_ssize_t cns, int dsp);
instrDesc* emitNewInstrAmd(emitAttr attr, ssize_t dsp);
instrDesc* emitNewInstrAmdCns(emitAttr attr, ssize_t dsp, int32_t cns);

instrDesc* emitNewInstrCall(CORINFO_METHOD_HANDLE methodHandle,
                            emitAttr              retRegAttr,
#ifdef UNIX_AMD64_ABI
                            emitAttr retReg2Attr,
#endif
#ifdef TARGET_X86
                            int argSlotCount,
#endif
                            int32_t disp);

/************************************************************************/
/*               Private helpers for instruction output                 */
/************************************************************************/

private:
insFormat emitInsModeFormat(instruction ins, insFormat base, insFormat FPld, insFormat FPst);

bool emitVerifyEncodable(instruction ins, emitAttr size, regNumber reg1, regNumber reg2 = REG_NA);

bool emitInsCanOnlyWriteSSE2OrAVXReg(instrDesc* id);

#if FEATURE_FIXED_OUT_ARGS
void emitAdjustStackDepthPushPop(instruction ins)
{
}
void emitAdjustStackDepth(instruction ins, ssize_t val)
{
}
#else  // !FEATURE_FIXED_OUT_ARGS
void emitAdjustStackDepthPushPop(instruction ins);
void emitAdjustStackDepth(instruction ins, ssize_t val);
#endif // !FEATURE_FIXED_OUT_ARGS

/*****************************************************************************
*
*  Convert between an index scale in bytes to a smaller encoding used for
*  storage in instruction descriptors.
*/

inline emitter::opSize emitEncodeScale(size_t scale)
{
    assert(scale == 1 || scale == 2 || scale == 4 || scale == 8);

    return emitSizeEncode[scale - 1];
}

inline emitAttr emitDecodeScale(unsigned ensz)
{
    assert(ensz < 4);

    return emitter::emitSizeDecode[ensz];
}

/************************************************************************/
/*           The public entry points to output instructions             */
/************************************************************************/

public:
void emitLoopAlign(unsigned short paddingBytes);

void emitLongLoopAlign(unsigned short alignmentBoundary);

void emitIns(instruction ins);

void emitIns(instruction ins, emitAttr attr);

void emitInsRMW_A(instruction ins, emitAttr attr, GenTree* addr);
void emitInsRMW_A_I(instruction ins, emitAttr attr, GenTree* addr, int imm);
void emitInsRMW_A_R(instruction ins, emitAttr attr, GenTree* addr, regNumber reg);

void emitIns_Nop(unsigned size);

#ifdef TARGET_X86
void emitIns_H(instruction ins, void* addr);
#endif
void emitIns_I(instruction ins, emitAttr attr, int32_t val);

void emitIns_R(instruction ins, emitAttr attr, regNumber reg);

void emitIns_C(instruction ins, emitAttr attr, CORINFO_FIELD_HANDLE fdlHnd);

void emitIns_R_H(instruction ins, regNumber reg, void* addr DEBUGARG(HandleKind handleKind = HandleKind::None));

void emitIns_R_I(instruction ins,
                 emitAttr    attr,
                 regNumber   reg,
                 ssize_t val DEBUGARG(HandleKind handleKind = HandleKind::None));

void emitIns_Mov(instruction ins, emitAttr attr, regNumber dstReg, regNumber srgReg, bool canSkip);

void emitIns_R_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2);

void emitIns_R_R_I(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int ival);

void emitIns_A(instruction ins, emitAttr attr, GenTree* addr);
void emitIns_A_I(instruction ins, emitAttr attr, GenTree* addr, int imm);
void emitIns_A_R(instruction ins, emitAttr attr, GenTree* addr, regNumber reg);

void emitIns_RRW_A(instruction ins, emitAttr attr, regNumber reg1, GenTree* addr);
void emitIns_R_A(instruction ins, emitAttr attr, regNumber reg1, GenTree* addr);

void emitIns_R_A_I(instruction ins, emitAttr attr, regNumber reg1, GenTree* addr, int ival);

void emitIns_R_C_I(instruction ins, emitAttr attr, regNumber reg1, CORINFO_FIELD_HANDLE fldHnd, int imm);

void emitIns_R_S_I(instruction ins, emitAttr attr, regNumber reg1, int varx, int offs, int32_t imm);

void emitIns_R_R_A(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, GenTree* addr);

void emitIns_R_R_C(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, CORINFO_FIELD_HANDLE fldHnd);

void emitIns_R_R_S(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int varx, int offs);

void emitIns_R_R_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3);

void emitIns_R_R_A_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, GenTree* addr, int ival, insFormat fmt);
void emitIns_S_R_I(instruction ins, emitAttr attr, int varNum, int offs, regNumber reg, int ival);

void emitIns_A_R_I(instruction ins, emitAttr attr, GenTree* addr, regNumber reg, int imm);

void emitIns_R_R_C_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, CORINFO_FIELD_HANDLE fldHnd, int imm);

void emitIns_R_R_R_I(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, int32_t imm);

void emitIns_R_R_S_I(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int varx, int offs, int32_t imm);

void emitIns_R_R_A_R(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op3Reg, GenTree* addr);

void emitIns_R_R_C_R(instruction          ins,
                     emitAttr             attr,
                     regNumber            targetReg,
                     regNumber            op1Reg,
                     regNumber            op3Reg,
                     CORINFO_FIELD_HANDLE fldHnd);

void emitIns_R_R_S_R(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op3Reg, int varx, int offs);

void emitIns_R_R_R_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, regNumber reg4);

void emitIns_S(instruction ins, emitAttr attr, int varx, int offs);

void emitIns_S_R(instruction ins, emitAttr attr, regNumber ireg, int varx, int offs);

void emitIns_R_S(instruction ins, emitAttr attr, regNumber ireg, int varx, int offs);

void emitIns_S_I(instruction ins, emitAttr attr, int varx, int offs, int32_t imm);

#ifdef WINDOWS_X86_ABI
void emitInsMov_R_FS(regNumber reg, int offs);
#endif
void emitIns_R_C(instruction ins, emitAttr attr, regNumber reg, CORINFO_FIELD_HANDLE fldHnd);

void emitIns_C_R(instruction ins, emitAttr attr, CORINFO_FIELD_HANDLE fldHnd, regNumber reg);

void emitIns_C_I(instruction ins, emitAttr attr, CORINFO_FIELD_HANDLE fdlHnd, int imm);

void emitIns_R_L(instruction ins, BasicBlock* dst, regNumber reg);

void emitIns_R_AH(instruction ins, regNumber ireg, void* addr);

void emitIns_AR(instruction ins, emitAttr attr, regNumber base, int32_t disp);
void emitIns_ARX(instruction ins, emitAttr attr, regNumber base, regNumber index, unsigned scaled, int32_t disp);
void emitIns_AR_I(instruction ins, emitAttr attr, regNumber base, int32_t disp, int imm);
void emitIns_R_AR(instruction ins, emitAttr attr, regNumber reg, regNumber base, int32_t disp);
void emitIns_R_ARR(instruction ins, emitAttr attr, regNumber reg, regNumber base, regNumber index, int32_t disp);
void emitIns_AR_R(instruction ins, emitAttr attr, regNumber reg, regNumber base, int32_t disp);
void emitIns_ARX_I(
    instruction ins, emitAttr attr, regNumber base, regNumber index, unsigned scale, int32_t disp, int imm);
void emitIns_R_ARX(
    instruction ins, emitAttr attr, regNumber reg, regNumber base, regNumber index, unsigned scale, int32_t disp);
void emitIns_ARX_R(
    instruction ins, emitAttr attr, regNumber reg, regNumber base, regNumber index, unsigned scale, int32_t disp);
void emitIns_AR_R_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber base, int32_t disp);
void emitIns_R_AR_R(instruction ins,
                    emitAttr    attr,
                    regNumber   reg1,
                    regNumber   reg2,
                    regNumber   base,
                    regNumber   index,
                    int         scale,
                    int32_t     disp);

void emitIns_SIMD_R_R_I(instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, int ival);

void emitIns_SIMD_R_R_A(instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, GenTree* addr);
void emitIns_SIMD_R_R_C(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, CORINFO_FIELD_HANDLE fldHnd);
void emitIns_SIMD_R_R_R(instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op2Reg);
void emitIns_SIMD_R_R_S(instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, int varx, int offs);

#ifdef FEATURE_HW_INTRINSICS
void emitIns_SIMD_R_R_A_I(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, GenTree* addr, int imm);
void emitIns_SIMD_R_R_C_I(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, CORINFO_FIELD_HANDLE fldHnd, int imm);
void emitIns_SIMD_R_R_R_I(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op2Reg, int ival);
void emitIns_SIMD_R_R_S_I(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, int varx, int offs, int ival);

void emitIns_SIMD_R_R_R_A(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op2Reg, GenTree* addr);
void emitIns_SIMD_R_R_R_C(instruction          ins,
                          emitAttr             attr,
                          regNumber            targetReg,
                          regNumber            op1Reg,
                          regNumber            op2Reg,
                          CORINFO_FIELD_HANDLE fldHnd);
void emitIns_SIMD_R_R_R_R(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op2Reg, regNumber op3Reg);
void emitIns_SIMD_R_R_R_S(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op2Reg, int varx, int offs);

void emitIns_SIMD_R_R_A_R(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op2Reg, GenTree* addr);
void emitIns_SIMD_R_R_C_R(instruction          ins,
                          emitAttr             attr,
                          regNumber            targetReg,
                          regNumber            op1Reg,
                          regNumber            op2Reg,
                          CORINFO_FIELD_HANDLE fldHnd);
void emitIns_SIMD_R_R_S_R(
    instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op2Reg, int varx, int offs);
#endif // FEATURE_HW_INTRINSICS

enum EmitCallType
{
    EC_FUNC_TOKEN,       // Direct call to a helper/static/nonvirtual/global method
    EC_FUNC_TOKEN_INDIR, // Indirect call to a helper/static/nonvirtual/global method
    EC_INDIR_R,          // Indirect call via register
    EC_INDIR_ARD         // Indirect call via an addressing mode
};

void emitIns_Call(EmitCallType          kind,
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
                  regNumber amBase  = REG_NA,
                  regNumber amIndex = REG_NA,
                  unsigned  amScale = 0,
                  int32_t   amDisp  = 0,
                  bool      isJump  = false);

inline bool emitIsCondJump(instrDesc* jmp)
{
    instruction ins = jmp->idIns();

    assert(jmp->idInsFmt() == IF_LABEL);

    return (ins != INS_call && ins != INS_jmp);
}

inline bool emitIsUncondJump(instrDesc* jmp)
{
    instruction ins = jmp->idIns();

    assert(jmp->idInsFmt() == IF_LABEL);

    return (ins == INS_jmp);
}

static bool instrIs3opImul(instruction ins);
static bool instrHasImplicitRegPairDest(instruction ins);
static instruction inst3opImulForReg(regNumber reg);

#endif // TARGET_XARCH
