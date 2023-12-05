// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_XARCH

private:
bool useVEXEncodings = false;

public:
void SetUseVEXEncoding(bool value)
{
    useVEXEncodings = value;
}

static bool IsMovInstruction(instruction ins);

bool AreFlagsSetToZeroCmp(regNumber reg, emitAttr opSize, genTreeOps treeOps);
bool AreUpper32BitsZero(regNumber reg);

#ifdef TARGET_AMD64
bool IsLastInsCall() const
{
    return (emitLastIns != nullptr) && (emitLastIns->idIns() == INS_call);
}
#endif

#ifdef TARGET_X86
void emitMarkStackLvl(unsigned stackLevel);
#endif

/************************************************************************/
/*           The public entry points to output instructions             */
/************************************************************************/

void emitIns(instruction ins);
void emitIns(instruction ins, emitAttr attr);
void emitIns_J(instruction ins, BasicBlock* dst);
void emitIns_J(instruction ins, int instrCount = 0);
void emitInsRMW_A(instruction ins, emitAttr attr, GenTree* addr);
void emitInsRMW_A_I(instruction ins, emitAttr attr, GenTree* addr, int32_t imm);
void emitInsRMW_A_R(instruction ins, emitAttr attr, GenTree* addr, regNumber reg);
void emitInsRMW_C(instruction ins, emitAttr attr, CORINFO_FIELD_HANDLE field);
void emitInsRMW_C_I(instruction ins, emitAttr attr, CORINFO_FIELD_HANDLE field, int32_t imm);
void emitInsRMW_C_R(instruction ins, emitAttr attr, CORINFO_FIELD_HANDLE field, regNumber reg);
void emitIns_Nop(unsigned size);
void emitIns_Lock();
#ifdef TARGET_AMD64
void emitIns_CallFinally(BasicBlock* block);
#endif
#ifdef TARGET_X86
void emitIns_H(instruction ins, void* addr);
void emitIns_L(instruction ins, BasicBlock* dst);
#endif
#ifdef WINDOWS_X86_ABI
void emitInsMov_R_FS(regNumber reg, int32_t offs);
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
void emitIns_R_R_I(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int32_t imm);
void emitIns_A(instruction ins, emitAttr attr, GenTree* addr);
void emitIns_A_I(instruction ins, emitAttr attr, GenTree* addr, int32_t imm);
void emitIns_A_R(instruction ins, emitAttr attr, GenTree* addr, regNumber reg);
void emitIns_R_A(instruction ins, emitAttr attr, regNumber reg1, GenTree* addr);
void emitIns_R_A_I(instruction ins, emitAttr attr, regNumber reg1, GenTree* addr, int32_t imm);
void emitIns_R_C_I(instruction ins, emitAttr attr, regNumber reg1, CORINFO_FIELD_HANDLE field, int32_t imm);
void emitIns_R_S_I(instruction ins, emitAttr attr, regNumber reg1, int varx, int offs, int32_t imm);
void emitIns_R_R_A(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, GenTree* addr);
void emitIns_R_R_C(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, CORINFO_FIELD_HANDLE field);
void emitIns_R_R_S(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int varx, int offs);
void emitIns_R_R_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3);
void emitIns_R_R_A_I(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, GenTree* addr, int32_t imm);
void emitIns_S_R_I(instruction ins, emitAttr attr, int varNum, int offs, regNumber reg, int32_t imm);
void emitIns_A_R_I(instruction ins, emitAttr attr, GenTree* addr, regNumber reg, int32_t imm);
void emitIns_C_R_I(instruction ins, emitAttr attr, CORINFO_FIELD_HANDLE field, regNumber reg, int32_t imm);
void emitIns_R_R_C_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, CORINFO_FIELD_HANDLE field, int32_t imm);
void emitIns_R_R_R_I(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, int32_t imm);
void emitIns_R_R_S_I(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int varx, int offs, int32_t imm);
void emitIns_R_R_A_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, GenTree* addr);
void emitIns_R_R_C_R(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, CORINFO_FIELD_HANDLE field);
void emitIns_R_R_S_R(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, int varx, int offs);
void emitIns_R_R_R_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, regNumber reg4);
void emitIns_S(instruction ins, emitAttr attr, int varx, int offs);
void emitIns_S_R(instruction ins, emitAttr attr, regNumber reg, int varx, int offs);
void emitIns_R_S(instruction ins, emitAttr attr, regNumber reg, int varx, int offs);
void emitIns_S_I(instruction ins, emitAttr attr, int varx, int offs, int32_t imm);
void emitIns_R_C(instruction ins, emitAttr attr, regNumber reg, CORINFO_FIELD_HANDLE field);
void emitIns_C_R(instruction ins, emitAttr attr, CORINFO_FIELD_HANDLE field, regNumber reg);
void emitIns_C_I(instruction ins, emitAttr attr, CORINFO_FIELD_HANDLE field, int32_t imm);
void emitIns_R_L(instruction ins, BasicBlock* dst, regNumber reg);
void emitIns_R_AH(instruction ins, regNumber ireg, void* addr);
void emitIns_AR(instruction ins, emitAttr attr, regNumber base, int32_t disp);
void emitIns_ARX(instruction ins, emitAttr attr, regNumber base, regNumber index, unsigned scaled, int32_t disp);
void emitIns_AR_I(instruction ins, emitAttr attr, regNumber base, int32_t disp, int32_t imm);
void emitIns_R_AR(instruction ins, emitAttr attr, regNumber reg, regNumber base, int32_t disp);
void emitIns_R_ARR(instruction ins, emitAttr attr, regNumber reg, regNumber base, regNumber index, int32_t disp);
void emitIns_AR_R(instruction ins, emitAttr attr, regNumber reg, regNumber base, int32_t disp);
void emitIns_ARX_I(
    instruction ins, emitAttr attr, regNumber base, regNumber index, unsigned scale, int32_t disp, int32_t imm);
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
void emitIns_SIMD_R_R_I(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int32_t imm);
void emitIns_SIMD_R_R_A(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, GenTree* addr);
void emitIns_SIMD_R_R_C(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, CORINFO_FIELD_HANDLE field);
void emitIns_SIMD_R_R_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber op2Reg);
void emitIns_SIMD_R_R_S(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int varx, int offs);
void emitIns_SIMD_R_R_A_I(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, GenTree* addr, int32_t imm);
void emitIns_SIMD_R_R_C_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber op1Reg, CORINFO_FIELD_HANDLE field, int32_t imm);
void emitIns_SIMD_R_R_R_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber op1Reg, regNumber op2Reg, int32_t imm);
void emitIns_SIMD_R_R_S_I(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, int varx, int offs, int32_t imm);
void emitIns_SIMD_R_R_R_A(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, GenTree* addr);
void emitIns_SIMD_R_R_R_C(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, CORINFO_FIELD_HANDLE field);
void emitIns_SIMD_R_R_R_R(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, regNumber reg4);
void emitIns_SIMD_R_R_R_S(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, int varx, int offs);
void emitIns_SIMD_R_R_A_R(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, GenTree* addr);
void emitIns_SIMD_R_R_S_R(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, int varx, int offs);

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

private:
bool UseVEXEncoding() const;
X86_ONLY(static bool emitVerifyEncodable(instruction ins, emitAttr size, regNumber reg1, regNumber reg2 = REG_NA));

// code_t is a type used to accumulate bits of opcode + prefixes. On amd64, it must be 64 bits
// to support the REX prefixes. On both x86 and amd64, it must be 64 bits to support AVX, with
// its 3-byte VEX prefix.
typedef uint64_t code_t;

unsigned emitGetVexAdjustedSize(instruction ins);
unsigned emitGetAdjustedSize(instruction ins, emitAttr attr, code_t code, bool isRR = false);
unsigned emitInsSize(code_t code);
unsigned emitInsSizeR(instruction ins, emitAttr size, regNumber reg);
unsigned emitInsSizeRI(instruction ins, emitAttr size, regNumber reg, ssize_t imm);
unsigned emitInsSizeRR(instruction ins, emitAttr size, regNumber reg1, regNumber reg2);
unsigned emitInsSizeRRI(instruction ins, emitAttr size, regNumber reg1, regNumber reg2);
unsigned emitInsSizeRRR(instruction ins);
unsigned emitInsSizeSV(instrDesc* id, code_t code);
unsigned emitInsSizeAM(instrDesc* id, code_t code);
unsigned emitInsSizeCV(instrDesc* id, code_t code);

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
uint8_t* emitOutputRRI(uint8_t* dst, instrDesc* id);
uint8_t* emitOutputRL(uint8_t* dst, instrDescJmp* id, insGroup* ig);
#ifdef TARGET_X86
uint8_t* emitOutputL(uint8_t* dst, instrDescJmp* id, insGroup* ig);
#endif
uint8_t* emitOutputJ(uint8_t* dst, instrDescJmp* id, insGroup* ig);
uint8_t* emitOutputCall(uint8_t* dst, instrDesc* id);
uint8_t* emitOutputNoOperands(uint8_t* dst, instrDesc* id);

size_t emitOutputVexPrefix(instruction ins, uint8_t* dst, code_t& code);
#ifdef TARGET_AMD64
size_t emitOutputRexPrefix(instruction ins, uint8_t* dst, code_t& code);
#endif
size_t emitOutputRexPrefixIfNeeded(instruction ins, uint8_t* dst, code_t& code);
size_t emitOutputRexOrVexPrefixIfNeeded(instruction ins, uint8_t* dst, code_t& code);

unsigned insEncodeReg012(instruction ins, regNumber reg, emitAttr size, code_t* code);
unsigned insEncodeReg345(instruction ins, regNumber reg, emitAttr size, code_t* code);
code_t SetRMReg(instruction ins, regNumber reg, emitAttr size, code_t code);
code_t SetVexVvvv(instruction ins, regNumber reg, emitAttr size, code_t code);

code_t insEncodeRMreg(instruction ins, regNumber reg, emitAttr size, code_t code);

code_t AddRexWPrefix(instruction ins, code_t code);
code_t AddRexRPrefix(instruction ins, code_t code);
code_t AddRexXPrefix(instruction ins, code_t code);
code_t AddRexBPrefix(instruction ins, code_t code);
code_t AddRexPrefix(instruction ins, code_t code);

bool IsRedundantMov(instruction ins, emitAttr size, regNumber dst, regNumber src, bool canIgnoreSideEffects);

static bool IsJccInstruction(instruction ins);
static bool IsJmpInstruction(instruction ins);

static bool hasRexPrefix(code_t code);
bool TakesVexPrefix(instruction ins) const;
bool hasVexPrefix(code_t code);
code_t AddVexPrefix(instruction ins, code_t code, emitAttr attr);
code_t AddVexPrefixIfNeeded(instruction ins, code_t code, emitAttr size);

bool IsSseDstSrcImm(instruction ins);
bool IsVexDstDstSrc(instruction ins);
bool IsVexDstSrcSrc(instruction ins);
INDEBUG(bool IsVexTernary(instruction ins);)

bool AreFlagsAlwaysModified(instrDesc* id);

/************************************************************************/
/*             Debug-only routines to display instructions              */
/************************************************************************/

#ifdef DEBUG
void PrintHexCode(instrDesc* id, const uint8_t* code, size_t sz);
void emitDispIns(instrDesc* id,
                 bool       isNew = false,
                 bool       doffs = false,
                 bool       asmfm = false,
                 unsigned   offs  = 0,
                 uint8_t*   code  = nullptr,
                 size_t     sz    = 0,
                 insGroup*  ig    = nullptr);
void PrintIns(instrDesc* id, bool asmfm = false);
void PrintImm(instrDesc* id, ssize_t val);
void PrintReloc(ssize_t value);
void PrintAddrMode(instrDesc* id, const char* sizeOper);
void PrintClsVar(instrDesc* id, const char* sizeOper);
void PrintShiftCL(instruction ins);
void PrintFrameRef(instrDesc* id, bool asmfm, const char* sizeOper);
#endif

/************************************************************************/
/*  Private members that deal with target-dependent instr. descriptors  */
/************************************************************************/

void SetInstrLclAddrMode(instrDesc* id, int varNum, int varOffs);
ssize_t GetAddrModeDisp(GenTree* addr);
void SetInstrAddrMode(instrDesc* id, GenTree* addr);
bool IntConNeedsReloc(GenTreeIntCon* con);

template <typename T>
T* AllocInstr(bool updateLastIns = true);

instrDesc*     emitNewInstr();
instrDesc*     emitNewInstrSmall();
instrDescJmp*  emitNewInstrJmp();
instrDescCGCA* emitNewInstrCGCA();
instrDesc* emitNewInstrSC(ssize_t imm);
instrDesc* emitNewInstrCns(int32_t imm);
#ifdef TARGET_X86
instrDesc* emitNewInstrDsp(int32_t disp);
#endif
instrDesc* emitNewInstrAmd(ssize_t disp);
instrDesc* emitNewInstrAmdCns(ssize_t disp, int32_t imm);
instrDesc* emitNewInstrGCReg(emitAttr attr, regNumber reg);
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

#if !FEATURE_FIXED_OUT_ARGS
void emitAdjustStackDepthPushPop(instruction ins);
void emitAdjustStackDepth(instruction ins, ssize_t val);
#endif

void emitLoopAlign(uint16_t paddingBytes);
void emitLongLoopAlign(uint16_t alignmentBoundary);

static insFormat emitInsModeFormat(instruction ins, insFormat base);

#endif // TARGET_XARCH
