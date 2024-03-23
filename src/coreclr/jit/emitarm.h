// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_ARM

class ArmEmitter final : public EmitterBase
{
    friend class ArmEncoder;
    friend class ArmAsmPrinter;
    friend class EmitterBase;

public:
    ArmEmitter(Compiler* compiler, CodeGen* codeGen, ICorJitInfo* jitInfo) : EmitterBase(compiler, codeGen, jitInfo)
    {
    }

    static bool emitIns_valid_imm_for_alu(int imm);
    static bool emitIns_valid_imm_for_mov(int imm);
    static bool emitIns_valid_imm_for_small_mov(RegNum reg, int imm, insFlags flags);
    static bool emitIns_valid_imm_for_add(int imm, insFlags flags = INS_FLAGS_DONT_CARE);
    static bool emitIns_valid_imm_for_cmp(int imm, insFlags flags);
    static bool emitIns_valid_imm_for_add_sp(int imm);
    static bool emitIns_valid_imm_for_ldst_offset(int imm, emitAttr size);
    static bool emitIns_valid_imm_for_vldst_offset(int imm);
    static bool validImmForInstr(instruction ins, int32_t imm, insFlags flags = INS_FLAGS_DONT_CARE);
    static bool validImmForBL(ssize_t addr, Compiler* compiler);

    static bool emitInsIsLoad(instruction ins);
    static bool IsMovInstruction(instruction ins);

    void Ins_R_S(instruction ins, emitAttr attr, RegNum reg, StackAddrMode s);

    /************************************************************************/
    /*           The public entry points to output instructions             */
    /************************************************************************/

    static instruction emitJumpKindToBranch(emitJumpKind jumpKind);

    void emitIns(instruction ins);
    void emitIns_J(instruction ins, int instrCount);
    void emitIns_J(instruction ins, insGroup* label);
    void emitIns_CallFinally(insGroup* label);
    void emitIns_I(instruction ins, emitAttr attr, int32_t imm);
    void emitIns_R(instruction ins, emitAttr attr, RegNum reg);
    void emitIns_R_I(instruction ins, emitAttr attr, RegNum reg, int32_t imm, insFlags flags = INS_FLAGS_DONT_CARE);
    void emitIns_MovRelocatableImmediate(instruction ins, RegNum reg, void* addr);
    void emitIns_Mov(instruction ins,
                     emitAttr    attr,
                     RegNum      dstReg,
                     RegNum      srgReg,
                     bool        canSkip,
                     insFlags    flags = INS_FLAGS_DONT_CARE);
    void emitIns_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, insFlags flags = INS_FLAGS_DONT_CARE);
    void emitIns_R_I_I(
        instruction ins, emitAttr attr, RegNum reg1, int imm1, int imm2, insFlags flags = INS_FLAGS_DONT_CARE);
    void emitIns_R_R_I(instruction ins,
                       emitAttr    attr,
                       RegNum      reg1,
                       RegNum      reg2,
                       int         imm,
                       insFlags    flags = INS_FLAGS_DONT_CARE,
                       insOpts     opt   = INS_OPTS_NONE);
    void emitIns_R_R_R(
        instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, insFlags flags = INS_FLAGS_DONT_CARE);
    void emitIns_R_R_I_I(instruction ins,
                         emitAttr    attr,
                         RegNum      reg1,
                         RegNum      reg2,
                         int         imm1,
                         int         imm2,
                         insFlags    flags = INS_FLAGS_DONT_CARE);
    void emitIns_R_R_R_I(instruction ins,
                         emitAttr    attr,
                         RegNum      reg1,
                         RegNum      reg2,
                         RegNum      reg3,
                         int32_t     imm,
                         insFlags    flags = INS_FLAGS_DONT_CARE,
                         insOpts     opt   = INS_OPTS_NONE);
    void emitIns_R_R_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, RegNum reg4);
    void emitIns_S_R(instruction ins, emitAttr attr, RegNum ireg, StackAddrMode s);
    void emitIns_R_S(instruction ins, emitAttr attr, RegNum ireg, StackAddrMode s);
    void emitIns_R_L(instruction ins, RegNum reg, insGroup* label);
    void emitIns_R_D(instruction ins, RegNum reg, ConstData* data);
    void emitIns_J_R(instruction ins, emitAttr attr, insGroup* label, RegNum reg);

    enum EmitCallType
    {
        EC_FUNC_TOKEN, // Direct call to a helper/static/nonvirtual/global method
        EC_INDIR_R     // Indirect call via register
    };

    void emitIns_Call(EmitCallType          kind,
                      CORINFO_METHOD_HANDLE methodHandle DEBUGARG(CORINFO_SIG_INFO* sigInfo),
                      void*    addr,
                      emitAttr retRegAttr,
                      RegNum   reg    = REG_NA,
                      bool     isJump = false);

    /************************************************************************/
    /*                   Interface for generating unwind information        */
    /************************************************************************/
    unsigned emitGetInstructionSize(const emitLocation& emit);

private:
    template <typename T>
    T* AllocInstr(bool updateLastIns = true);

    instrDesc* emitNewInstr();
    instrDesc* emitNewInstrSmall();
    instrDesc* emitNewInstrSC(int32_t cns);
    instrDesc* emitNewInstrCns(int32_t cns);
    instrDesc* emitNewInstrGCReg(emitAttr attr, RegNum reg);
    instrDescJmp* emitNewInstrJmp();
    instrDesc* emitNewInstrCall(CORINFO_METHOD_HANDLE methodHandle, emitAttr retSize);
    instrDescCGCA* emitAllocInstrCGCA();

    void MovRegStackOffset(RegNum reg, int32_t imm, StackAddrMode s);
    int OptimizeFrameAddress(int fpOffset, bool isFloatLoadStore, RegNum* baseReg);

#ifdef DEBUG
    /************************************************************************/
    /*             Debug-only routines to display instructions              */
    /************************************************************************/

    void emitDispInsHex(instrDesc* id, uint8_t* code, size_t sz);
    void emitDispInsHelp(
        instrDesc* id, bool isNew, bool doffs, bool asmfm, unsigned offs = 0, uint8_t* code = 0, size_t sz = 0);
    void emitDispIns(instrDesc* id, bool isNew, bool doffs, unsigned offs);
#endif // DEBUG
};

using ArchEmitter = ArmEmitter;

#endif // TARGET_ARM
