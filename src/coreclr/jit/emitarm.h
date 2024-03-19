// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_ARM

friend class ArmEncoder;

public:
// This typedef defines the type that we use to hold encoded instructions.
using code_t = uint32_t;

/************************************************************************/
/*         Routines that compute the size of / encode instructions      */
/************************************************************************/

static insSize emitInsSize(insFormat insFmt);
static size_t emitGetInstrDescSize(const instrDesc* id);

/************************************************************************/
/*             Debug-only routines to display instructions              */
/************************************************************************/

#ifdef DEBUG
void emitDispInsHex(instrDesc* id, BYTE* code, size_t sz);
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

void emitDispInsHelp(
    instrDesc* id, bool isNew, bool doffs, bool asmfm, unsigned offs = 0, BYTE* code = 0, size_t sz = 0);
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

instrDesc* emitNewInstrCall(CORINFO_METHOD_HANDLE methodHandle, emitAttr retSize);

/************************************************************************/
/*               Private helpers for instruction output                 */
/************************************************************************/

public:
static bool validImmForInstr(instruction ins, int32_t imm, insFlags flags = INS_FLAGS_DONT_CARE);
static bool validDispForLdSt(int32_t disp, var_types type);
static bool validImmForBL(ssize_t addr, Compiler* compiler);
static bool emitInsIsCompare(instruction ins);
static bool emitInsIsLoad(instruction ins);
static bool emitInsIsStore(instruction ins);
static bool emitInsIsLoadOrStore(instruction ins);

static bool IsMovInstruction(instruction ins);

void MovRegStackOffset(regNumber reg, int32_t imm, StackAddrMode s);
int OptimizeFrameAddress(int fpOffset, bool isFloatLoadStore, regNumber* baseReg);
void Ins_R_S(instruction ins, emitAttr attr, regNumber reg, StackAddrMode s);

/************************************************************************/
/*           Public inline informational methods                        */
/************************************************************************/

static bool isLowRegister(regNumber reg)
{
    return (reg <= REG_R7);
}

// Returns the number of bits used by the given 'size'.
static unsigned getBitWidth(emitAttr size)
{
    assert(size <= EA_8BYTE);
    return (unsigned)size * BITS_PER_BYTE;
}

/************************************************************************/
/*           The public entry points to output instructions             */
/************************************************************************/

public:
static instruction emitJumpKindToBranch(emitJumpKind jumpKind);

static bool emitIns_valid_imm_for_alu(int imm);
static bool emitIns_valid_imm_for_mov(int imm);
static bool emitIns_valid_imm_for_small_mov(regNumber reg, int imm, insFlags flags);
static bool emitIns_valid_imm_for_add(int imm, insFlags flags = INS_FLAGS_DONT_CARE);
static bool emitIns_valid_imm_for_cmp(int imm, insFlags flags);
static bool emitIns_valid_imm_for_add_sp(int imm);
static bool emitIns_valid_imm_for_ldst_offset(int imm, emitAttr size);
static bool emitIns_valid_imm_for_vldst_offset(int imm);

void emitIns(instruction ins);

void emitIns_J(instruction ins, int instrCount);
void emitIns_J(instruction ins, insGroup* label);
void emitIns_CallFinally(insGroup* label);

void emitIns_I(instruction ins, emitAttr attr, int32_t imm);

void emitIns_R(instruction ins, emitAttr attr, regNumber reg);

void emitIns_R_I(instruction ins, emitAttr attr, regNumber reg, int32_t imm, insFlags flags = INS_FLAGS_DONT_CARE);
void emitIns_MovRelocatableImmediate(instruction ins, regNumber reg, void* addr);

void emitIns_Mov(instruction ins,
                 emitAttr    attr,
                 regNumber   dstReg,
                 regNumber   srgReg,
                 bool        canSkip,
                 insFlags    flags = INS_FLAGS_DONT_CARE);

void emitIns_R_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, insFlags flags = INS_FLAGS_DONT_CARE);

void emitIns_R_I_I(
    instruction ins, emitAttr attr, regNumber reg1, int imm1, int imm2, insFlags flags = INS_FLAGS_DONT_CARE);

void emitIns_R_R_I(instruction ins,
                   emitAttr    attr,
                   regNumber   reg1,
                   regNumber   reg2,
                   int         imm,
                   insFlags    flags = INS_FLAGS_DONT_CARE,
                   insOpts     opt   = INS_OPTS_NONE);

void emitIns_R_R_R(instruction ins,
                   emitAttr    attr,
                   regNumber   reg1,
                   regNumber   reg2,
                   regNumber   reg3,
                   insFlags    flags = INS_FLAGS_DONT_CARE);

void emitIns_R_R_I_I(instruction ins,
                     emitAttr    attr,
                     regNumber   reg1,
                     regNumber   reg2,
                     int         imm1,
                     int         imm2,
                     insFlags    flags = INS_FLAGS_DONT_CARE);

void emitIns_R_R_R_I(instruction ins,
                     emitAttr    attr,
                     regNumber   reg1,
                     regNumber   reg2,
                     regNumber   reg3,
                     int32_t     imm,
                     insFlags    flags = INS_FLAGS_DONT_CARE,
                     insOpts     opt   = INS_OPTS_NONE);

void emitIns_R_R_R_R(instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, regNumber reg3, regNumber reg4);

void emitIns_S_R(instruction ins, emitAttr attr, regNumber ireg, StackAddrMode s);
void emitIns_R_S(instruction ins, emitAttr attr, regNumber ireg, StackAddrMode s);

void emitIns_R_L(instruction ins, RegNum reg, insGroup* label);

void emitIns_R_D(instruction ins, RegNum reg, ConstData* data);

void emitIns_J_R(instruction ins, emitAttr attr, insGroup* label, regNumber reg);

enum EmitCallType
{
    EC_FUNC_TOKEN, // Direct call to a helper/static/nonvirtual/global method
    EC_INDIR_R     // Indirect call via register
};

void emitIns_Call(EmitCallType          kind,
                  CORINFO_METHOD_HANDLE methodHandle DEBUGARG(CORINFO_SIG_INFO* sigInfo),
                  void*     addr,
                  emitAttr  retRegAttr,
                  regNumber reg    = REG_NA,
                  bool      isJump = false);

private:
/*****************************************************************************
 *
 *  Given an instrDesc, return true if it's a conditional jump.
 */

inline bool emitIsCondJump(instrDesc* jmp)
{
    return (jmp->idInsFmt() == IF_T2_J1) || (jmp->idInsFmt() == IF_T1_K) || (jmp->idInsFmt() == IF_LARGEJMP);
}

/*****************************************************************************
 *
 *  Given an instrDesc, return true if it's a comapre and jump.
 */

inline bool emitIsCmpJump(instrDesc* jmp)
{
    return (jmp->idInsFmt() == IF_T1_I);
}

/*****************************************************************************
 *
 *  Given a instrDesc, return true if it's an unconditional jump.
 */

inline bool emitIsUncondJump(instrDesc* jmp)
{
    return (jmp->idInsFmt() == IF_T2_J2) || (jmp->idInsFmt() == IF_T1_M);
}

/*****************************************************************************
 *
 *  Given a instrDesc, return true if it's a load label instruction.
 */

inline bool emitIsLoadLabel(instrDesc* jmp)
{
    return (jmp->idInsFmt() == IF_T2_M1) || (jmp->idInsFmt() == IF_T1_J3) || (jmp->idInsFmt() == IF_T2_N1);
}

/************************************************************************/
/*                   Interface for generating unwind information        */
/************************************************************************/
public:
void emitUnwindNopPadding(const emitLocation& loc);
unsigned emitGetInstructionSize(const emitLocation& emit);

private:
void emitSetShortJump(instrDescJmp* id);
void emitSetMediumJump(instrDescJmp* id);

template <typename T>
T* AllocInstr(bool updateLastIns = true);

instrDesc* emitNewInstr();
instrDesc* emitNewInstrSmall();
instrDesc* emitNewInstrSC(int32_t cns);
instrDesc* emitNewInstrCns(int32_t cns);
instrDesc* emitNewInstrGCReg(emitAttr attr, regNumber reg);
instrDescJmp*  emitNewInstrJmp();
instrDescCGCA* emitAllocInstrCGCA();

static int32_t emitGetInsSC(instrDesc* id);

#endif // TARGET_ARM
