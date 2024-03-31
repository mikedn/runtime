// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_ARM64

class Arm64Emitter final : public EmitterBase
{
    friend class Arm64Encoder;
    friend class Arm64AsmPrinter;
    friend class EmitterBase;

public:
    Arm64Emitter(Compiler* compiler, CodeGen* codeGen, ICorJitInfo* jitInfo) : EmitterBase(compiler, codeGen, jitInfo)
    {
    }

    void emitIns(instruction ins);
    void emitIns_J(instruction ins, int instrCount);
    void emitIns_J(instruction ins, insGroup* label);
    void emitIns_CallFinally(insGroup* label);
    void emitIns_BRK(uint16_t imm);
    void emitIns_R(instruction ins, emitAttr attr, RegNum reg);
    void emitIns_R_I(instruction ins, emitAttr attr, RegNum reg, int64_t imm, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_F(instruction ins, emitAttr attr, RegNum reg, double immDbl, insOpts opt = INS_OPTS_NONE);
    void emitIns_Mov(
        instruction ins, emitAttr attr, RegNum dstReg, RegNum srcReg, bool canSkip, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_I_I(
        instruction ins, emitAttr attr, RegNum reg1, int64_t imm1, int64_t imm2, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_R_I(
        instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, int64_t imm, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_R_Imm(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, int64_t imm);
    void emitIns_R_R_R(
        instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_R_R_I(instruction ins,
                         emitAttr    attr,
                         RegNum      reg1,
                         RegNum      reg2,
                         RegNum      reg3,
                         int32_t     imm,
                         insOpts     opt      = INS_OPTS_NONE,
                         emitAttr    attrReg2 = EA_UNKNOWN);
    void emitIns_R_R_R_Ext(instruction ins,
                           emitAttr    attr,
                           RegNum      reg1,
                           RegNum      reg2,
                           RegNum      reg3,
                           insOpts     opt         = INS_OPTS_NONE,
                           int         shiftAmount = -1);
    void emitIns_R_R_I_I(
        instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, int imm1, int imm2, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_R_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, RegNum reg4);
    void emitIns_R_COND(instruction ins, emitAttr attr, RegNum reg, insCond cond);
    void emitIns_R_R_COND(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, insCond cond);
    void emitIns_R_R_R_COND(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, insCond cond);
    void emitIns_R_R_FLAGS_COND(
        instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, insCflags flags, insCond cond);
    void emitIns_R_I_FLAGS_COND(instruction ins, emitAttr attr, RegNum reg1, int imm, insCflags flags, insCond cond);
    void emitIns_BARR(instruction ins, insBarrier barrier);
    void emitIns_S_R(instruction ins, emitAttr attr, RegNum ireg, StackAddrMode s);
    void emitIns_S_S_R_R(instruction ins, emitAttr attr, emitAttr attr2, RegNum ireg, RegNum ireg2, StackAddrMode s);
    void emitIns_R_S(instruction ins, emitAttr attr, RegNum ireg, StackAddrMode s);
    void emitIns_R_R_S_S(instruction ins, emitAttr attr, emitAttr attr2, RegNum ireg, RegNum ireg2, StackAddrMode s);
    void Ins_R_S(instruction ins, emitAttr attr, RegNum reg, StackAddrMode s);
    void Ins_R_R_S(instruction ins, emitAttr attr1, emitAttr attr2, RegNum reg1, RegNum reg2, StackAddrMode s);
    void emitIns_S_I(instruction ins, emitAttr attr, StackAddrMode s, int val);
    void emitIns_R_C(instruction ins, emitAttr attr, RegNum reg, RegNum tmpReg, ConstData* data);
    void emitIns_R_L(RegNum reg, insGroup* label);
    void emitIns_J_R(instruction ins, emitAttr attr, insGroup* label, RegNum reg);
    void emitIns_J_R_I(instruction ins, emitAttr attr, insGroup* label, RegNum reg, int imm);
    void emitIns_R_AH(RegNum reg,
                      void* addr DEBUGARG(void* handle = nullptr) DEBUGARG(HandleKind handleKind = HandleKind::None));

    enum EmitCallType
    {
        EC_FUNC_TOKEN, // Direct call to a helper/static/nonvirtual/global method
        EC_INDIR_R     // Indirect call via register
    };

    void emitIns_Call(EmitCallType          kind,
                      CORINFO_METHOD_HANDLE methodHandle DEBUGARG(CORINFO_SIG_INFO* sigInfo),
                      void*    addr,
                      emitAttr retRegAttr,
                      emitAttr retReg2Attr,
                      RegNum   reg    = REG_NA,
                      bool     isJump = false);

private:
    template <typename T>
    T* AllocInstr(bool updateLastIns = true);

    instrDesc* emitNewInstr();
    instrDesc* emitNewInstrSmall();
    instrDesc* emitNewInstrSC(int64_t imm);
    instrDesc* emitNewInstrCns(int32_t imm);
    instrDesc* emitNewInstrGCReg(emitAttr attr, RegNum reg);
    instrDescJmp* emitNewInstrJmp();
    instrDesc* emitNewInstrCall(CORINFO_METHOD_HANDLE methodHandle, emitAttr retSize, emitAttr secondRetSize);
    instrDescCGCA* emitAllocInstrCGCA();

    // Method to do check if mov is redundant with respect to the last instruction.
    // If yes, the caller of this method can choose to omit current mov instruction.
    bool IsRedundantMov(instruction ins, emitAttr size, RegNum dst, RegNum src, bool canSkip);
    bool IsRedundantLdStr(instruction ins, RegNum reg1, RegNum reg2, int64_t imm, emitAttr size, insFormat fmt);

#ifdef DEBUG
    void PrintIns(instrDesc* id);
#endif
};

using ArchEmitter = Arm64Emitter;

#endif // TARGET_ARM64
