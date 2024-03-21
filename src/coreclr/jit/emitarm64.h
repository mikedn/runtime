// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_ARM64

class Arm64Emitter : public EmitterBase
{
    friend class Arm64Encoder;
    friend class AsmPrinter;
    friend class EmitterBase;

public:
    Arm64Emitter(Compiler* compiler, CodeGen* codeGen, ICorJitInfo* jitInfo) : EmitterBase(compiler, codeGen, jitInfo)
    {
    }

    static bool emitInsIsLoad(instruction ins);
    // For the given 'arrangement' returns the 'elemsize' specified by the vector register arrangement
    static emitAttr optGetElemsize(insOpts arrangement);
    // For the given 'datasize', 'elemsize' and 'index' returns true, if it specifies a valid 'index'
    // for an element of size 'elemsize' in a vector register of size 'datasize'
    static bool isValidVectorIndex(emitAttr datasize, emitAttr elemsize, ssize_t index);

    static bool IsMovInstruction(instruction ins);

    // This union is used to to encode/decode the special ARM64 immediate values
    // that is listed as imm(N,r,s) and referred to as 'bitmask immediate'
    union bitMaskImm {
        struct
        {
            unsigned immS : 6; // bits 0..5
            unsigned immR : 6; // bits 6..11
            unsigned immN : 1; // bits 12
        };
        unsigned immNRS; // concat N:R:S forming a 13-bit unsigned immediate
    };

    // Convert between a 64-bit immediate and its 'bitmask immediate' representation imm(i16,hw)
    static bitMaskImm emitEncodeBitMaskImm(int64_t imm, emitAttr size);
    static int64_t emitDecodeBitMaskImm(const bitMaskImm bmImm, emitAttr size);

    /************************************************************************/
    /*           Public inline informational methods                        */
    /************************************************************************/

    // true if this 'imm' can be encoded as a input operand to a mov instruction
    static bool emitIns_valid_imm_for_mov(int64_t imm, emitAttr size);

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
    static bool emitIns_valid_imm_for_add(int64_t imm, emitAttr size = EA_8BYTE);

    // true if this 'imm' can be encoded as a input operand to a cmp instruction
    static bool emitIns_valid_imm_for_cmp(int64_t imm, emitAttr size);

    // true if this 'imm' can be encoded as a input operand to an alu instruction
    static bool emitIns_valid_imm_for_alu(int64_t imm, emitAttr size);

    // true if this 'imm' can be encoded as the offset in a ldr/str instruction
    static bool emitIns_valid_imm_for_ldst_offset(int64_t imm, emitAttr size);

    static bool validImmForBL(ssize_t addr, Compiler* compiler);

    // true if 'imm' can be encoded using a 'bitmask immediate', also returns the encoding if wbBMI is non-null
    static bool canEncodeBitMaskImm(int64_t imm, emitAttr size, bitMaskImm* wbBMI = nullptr);

    /************************************************************************/
    /*           The public entry points to output instructions             */
    /************************************************************************/

    static insCond emitJumpKindToCond(emitJumpKind jumpKind);
    static instruction emitJumpKindToBranch(emitJumpKind jumpKind);

    void emitIns(instruction ins);
    void emitIns_J(instruction ins, int instrCount);
    void emitIns_J(instruction ins, insGroup* label);
    void emitIns_CallFinally(insGroup* label);
    void emitIns_BRK(uint16_t imm);
    void emitIns_R(instruction ins, emitAttr attr, RegNum reg);
    void emitIns_R_I(instruction ins, emitAttr attr, RegNum reg, ssize_t imm, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_F(instruction ins, emitAttr attr, RegNum reg, double immDbl, insOpts opt = INS_OPTS_NONE);
    void emitIns_Mov(
        instruction ins, emitAttr attr, RegNum dstReg, RegNum srcReg, bool canSkip, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_I_I(
        instruction ins, emitAttr attr, RegNum reg1, ssize_t imm1, ssize_t imm2, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_R_I(
        instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, ssize_t imm, insOpts opt = INS_OPTS_NONE);
    void emitIns_R_R_Imm(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, ssize_t imm);
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
    bool IsRedundantLdStr(instruction ins, RegNum reg1, RegNum reg2, ssize_t imm, emitAttr size, insFormat fmt);

#ifdef DEBUG
    /************************************************************************/
    /*             Debug-only routines to display instructions              */
    /************************************************************************/

    void emitDispInsHex(instrDesc* id, uint8_t* code, size_t sz);
    void emitDispIns(instrDesc* id,
                     bool       isNew = false,
                     bool       doffs = false,
                     bool       asmfm = false,
                     unsigned   offs  = 0,
                     uint8_t*   code  = 0,
                     size_t     sz    = 0);
    uint8_t* emitOffsetToPtr(unsigned offset) const;
#endif // DEBUG
};

using ArchEmitter = Arm64Emitter;

#endif // TARGET_ARM64
