// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_ARM

class ArmEmitter final : public EmitterBase
{
    friend class ArmEncoder;
    friend class ArmAsmPrinter;
    friend class EmitterBase;

    using Ins     = instruction;
    using InsAttr = emitAttr;

public:
    ArmEmitter(Compiler* compiler, CodeGen* codeGen, ICorJitInfo* jitInfo) : EmitterBase(compiler, codeGen, jitInfo)
    {
    }

    void Ins_R_S(Ins ins, InsAttr attr, RegNum reg, StackAddrMode s);

    void emitIns(Ins ins);
    void emitIns_J(Ins ins, int instrCount);
    void emitIns_J(Ins ins, insGroup* label);
    void Ins_CallFinally(insGroup* label);
    void emitIns_I(Ins ins, InsAttr attr, int32_t imm);
    void emitIns_R(Ins ins, InsAttr attr, RegNum reg);
    void emitIns_R_I(Ins ins, InsAttr attr, RegNum reg, int32_t imm, insFlags flags = INS_FLAGS_DONT_CARE);
    void emitIns_MovRelocatableImmediate(Ins ins, RegNum reg, void* addr);
    void emitIns_Mov(
        Ins ins, InsAttr attr, RegNum dstReg, RegNum srgReg, bool canSkip, insFlags flags = INS_FLAGS_DONT_CARE);
    void emitIns_R_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, insFlags flags = INS_FLAGS_DONT_CARE);
    void emitIns_R_I_I(Ins ins, InsAttr attr, RegNum reg1, int imm1, int imm2, insFlags flags = INS_FLAGS_DONT_CARE);
    void emitIns_R_R_I(Ins      ins,
                       InsAttr  attr,
                       RegNum   reg1,
                       RegNum   reg2,
                       int      imm,
                       insFlags flags = INS_FLAGS_DONT_CARE,
                       insOpts  opt   = INS_OPTS_NONE);
    void emitIns_R_R_R(
        Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, insFlags flags = INS_FLAGS_DONT_CARE);
    void emitIns_R_R_I_I(
        Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, int imm1, int imm2, insFlags flags = INS_FLAGS_DONT_CARE);
    void emitIns_R_R_R_I(Ins      ins,
                         InsAttr  attr,
                         RegNum   reg1,
                         RegNum   reg2,
                         RegNum   reg3,
                         int32_t  imm,
                         insFlags flags = INS_FLAGS_DONT_CARE,
                         insOpts  opt   = INS_OPTS_NONE);
    void emitIns_R_R_R_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, RegNum reg4);
    void emitIns_S_R(Ins ins, InsAttr attr, RegNum ireg, StackAddrMode s);
    void emitIns_R_S(Ins ins, InsAttr attr, RegNum ireg, StackAddrMode s);
    void emitIns_R_L(Ins ins, RegNum reg, insGroup* label);
    void emitIns_R_D(Ins ins, RegNum reg, ConstData* data);
    void emitIns_J_R(Ins ins, InsAttr attr, insGroup* label, RegNum reg);

    void Ins_Call(RegNum                reg,
                  void*                 addr,
                  InsAttr               retRegAttr,
                  bool                  isJump,
                  CORINFO_METHOD_HANDLE methodHandle DEBUGARG(CORINFO_SIG_INFO* sigInfo = nullptr));

    unsigned GetInstructionSize(const emitLocation& loc);

private:
    template <typename T>
    T* AllocInstr(bool updateLastIns = true);

    instrDesc* NewInstr();
    instrDesc* NewInstrSmall();
    instrDesc* NewInstrSmall(int32_t cns);
    instrDesc* NewInstrCns(int32_t cns);
    instrDesc* NewInstrGCReg(InsAttr attr, RegNum reg);
    instrDescJmp* NewInstrJmp();
    instrDesc* NewInstrCall(CORINFO_METHOD_HANDLE methodHandle, InsAttr retSize);
    instrDescCGCA* AllocInstrCGCA();

    void MovRegStackOffset(RegNum reg, int32_t imm, StackAddrMode s);
    int OptimizeFrameAddress(int fpOffset, bool isFloatLoadStore, RegNum* baseReg);

#ifdef DEBUG
    void PrintInstr(instrDesc* id);
#endif
};

using ArchEmitter = ArmEmitter;

#endif // TARGET_ARM
