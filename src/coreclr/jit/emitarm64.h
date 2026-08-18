// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_ARM64

class Arm64Emitter final : public EmitterBase
{
    friend class Arm64Encoder;
    friend class Arm64AsmPrinter;
    friend class EmitterBase;

    using Ins     = instruction;
    using InsAttr = emitAttr;

public:
    Arm64Emitter(Compiler* compiler, CodeGen* codeGen, ICorJitInfo* jitInfo) : EmitterBase(compiler, codeGen, jitInfo)
    {
    }

    void emitIns(Ins ins);
    void emitIns_J(Ins ins, int instrCount);
    void emitIns_J(Ins ins, insGroup* label);
    void Ins_CallFinally(insGroup* label);
    void emitIns_BRK(uint16_t imm);
    void emitIns_R(Ins ins, InsAttr attr, RegNum reg);
    void emitIns_R_I(Ins ins, InsAttr attr, RegNum reg, int64_t imm, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_F(Ins ins, InsAttr attr, RegNum reg, double immDbl, insOpts opt = INS_OPTS_NONE);
    void emitIns_Mov(Ins ins, InsAttr attr, RegNum dstReg, RegNum srcReg, bool canSkip, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_I_I(Ins ins, InsAttr attr, RegNum reg1, int64_t imm1, int64_t imm2, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_R_I(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, int64_t imm, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_R_Imm(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, int64_t imm);
    void emitIns_R_R_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_R_R_I(Ins     ins,
                         InsAttr attr,
                         RegNum  reg1,
                         RegNum  reg2,
                         RegNum  reg3,
                         int32_t imm,
                         insOpts opt      = INS_OPTS_NONE,
                         InsAttr attrReg2 = EA_UNKNOWN);
    void emitIns_R_R_R_Ext(Ins     ins,
                           InsAttr attr,
                           RegNum  reg1,
                           RegNum  reg2,
                           RegNum  reg3,
                           insOpts opt         = INS_OPTS_NONE,
                           int     shiftAmount = -1);
    void emitIns_R_R_I_I(
        Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, int imm1, int imm2, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_R_R_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, RegNum reg4);
    void emitIns_R_COND(Ins ins, InsAttr attr, RegNum reg, insCond cond);
    void emitIns_R_R_COND(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, insCond cond);
    void emitIns_R_R_R_COND(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, insCond cond);
    void emitIns_R_R_FLAGS_COND(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, insCflags flags, insCond cond);
    void emitIns_R_I_FLAGS_COND(Ins ins, InsAttr attr, RegNum reg1, int imm, insCflags flags, insCond cond);
    void emitIns_BARR(Ins ins, insBarrier barrier);
    void emitIns_S_R(Ins ins, InsAttr attr, RegNum ireg, StackAddrMode s);
    void emitIns_S_S_R_R(Ins ins, InsAttr attr, InsAttr attr2, RegNum ireg, RegNum ireg2, StackAddrMode s);
    void emitIns_R_S(Ins ins, InsAttr attr, RegNum ireg, StackAddrMode s);
    void emitIns_R_R_S_S(Ins ins, InsAttr attr, InsAttr attr2, RegNum ireg, RegNum ireg2, StackAddrMode s);
    void Ins_R_S(Ins ins, InsAttr attr, RegNum reg, StackAddrMode s);
    void Ins_R_R_S(Ins ins, InsAttr attr1, InsAttr attr2, RegNum reg1, RegNum reg2, StackAddrMode s);
    void emitIns_S_I(Ins ins, InsAttr attr, StackAddrMode s, int val);
    void emitIns_R_C(Ins ins, InsAttr attr, RegNum reg, RegNum tmpReg, ConstData* data);
    void emitIns_R_L(RegNum reg, insGroup* label);
    void emitIns_J_R(Ins ins, InsAttr attr, insGroup* label, RegNum reg);
    void emitIns_J_R_I(Ins ins, InsAttr attr, insGroup* label, RegNum reg, int imm);
    void emitIns_R_AH(RegNum reg,
                      void* addr DEBUGARG(void* handle = nullptr) DEBUGARG(HandleKind handleKind = HandleKind::None));

    void Ins_Call(RegNum reg,
                  void*  addr,
                  jitstd::pair<InsAttr, InsAttr> retRegAttr,
                  bool                  isJump,
                  CORINFO_METHOD_HANDLE methodHandle DEBUGARG(CORINFO_SIG_INFO* sigInfo = nullptr));

private:
    template <typename T>
    T* AllocInstr(bool updateLastIns = true);

    instrDesc* NewInstr();
    instrDesc* NewInstrSmall();
    instrDesc* NewInstrSmall(int64_t imm);
    instrDesc* NewInstrCns(int32_t imm);
    instrDesc* NewInstrGCReg(InsAttr attr, RegNum reg);
    instrDescJmp* NewInstrJmp();
    instrDesc* NewInstrCall(CORINFO_METHOD_HANDLE methodHandle, InsAttr regReg0Attr, InsAttr retReg1Attr);
    instrDescCGCA* AllocInstrCGCA();

    // Method to do check if mov is redundant with respect to the last Ins.
    // If yes, the caller of this method can choose to omit current mov Ins.
    bool IsRedundantMov(Ins ins, InsAttr size, RegNum dst, RegNum src, bool canSkip);
    bool IsRedundantLdStr(Ins ins, RegNum reg1, RegNum reg2, int64_t imm, InsAttr size, insFormat fmt);

#ifdef DEBUG
    void PrintInstr(instrDesc* id);
#endif
};

using ArchEmitter = Arm64Emitter;

#endif // TARGET_ARM64
