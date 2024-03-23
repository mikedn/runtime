// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_XARCH

class X86Emitter final : public EmitterBase
{
    friend class X86Encoder;
    friend class X86AsmPrinter;
    friend class EmitterBase;

private:
    bool useVEXEncodings = false;

public:
    X86Emitter(Compiler* compiler, CodeGen* codeGen, ICorJitInfo* jitInfo) : EmitterBase(compiler, codeGen, jitInfo)
    {
    }

    void SetUseVEXEncoding(bool value)
    {
        useVEXEncodings = value;
    }

    // code_t is a type used to accumulate bits of opcode + prefixes. On amd64, it must be 64 bits
    // to support the REX prefixes. On both x86 and amd64, it must be 64 bits to support AVX, with
    // its 3-byte VEX prefix.
    using code_t = uint64_t;

    static bool IsMovInstruction(instruction ins);

    bool AreFlagsSetToZeroCmp(RegNum reg, emitAttr opSize, genTreeOps treeOps);
    bool AreUpper32BitsZero(RegNum reg);

#ifdef TARGET_AMD64
    bool IsLastInsCall() const
    {
        return (emitLastIns != nullptr) && (emitLastIns->idIns() == INS_call);
    }
#endif

    void PrologSpillParamRegsToShadowSlots();

#ifdef TARGET_X86
    void emitMarkStackLvl(unsigned stackLevel);
#endif

    static instruction emitJumpKindToSetcc(emitJumpKind jumpKind);
    static instruction emitJumpKindToBranch(emitJumpKind jumpKind);

    /************************************************************************/
    /*           The public entry points to output instructions             */
    /************************************************************************/

    void emitIns(instruction ins);
    void emitIns(instruction ins, emitAttr attr);
    void emitIns_J(instruction ins, insGroup* label);
    void emitIns_J(instruction ins, int instrCount = 0);
    void emitInsRMW_A(instruction ins, emitAttr attr, GenTree* addr);
    void emitInsRMW_A_I(instruction ins, emitAttr attr, GenTree* addr, int32_t imm);
    void emitInsRMW_A_R(instruction ins, emitAttr attr, GenTree* addr, RegNum reg);
    void emitIns_Nop(unsigned size);
    void emitIns_Lock();
#ifdef TARGET_AMD64
    void emitIns_CallFinally(insGroup* label);
#endif
#ifdef TARGET_X86
    void emitIns_H(instruction ins, void* addr);
    void emitIns_L(instruction ins, insGroup* label);
#endif
#ifdef WINDOWS_X86_ABI
    void emitInsMov_R_FS(RegNum reg, int32_t disp);
#endif
    void emitIns_I(instruction ins, emitAttr attr, int32_t val);
    void emitIns_R(instruction ins, emitAttr attr, RegNum reg);
    void emitIns_C(instruction ins, emitAttr attr, ConstData* data);
    void emitIns_R_H(instruction ins, RegNum reg, void* addr DEBUGARG(HandleKind handleKind = HandleKind::None));
    void emitIns_R_I(instruction ins,
                     emitAttr    attr,
                     RegNum      reg,
                     ssize_t val DEBUGARG(HandleKind handleKind = HandleKind::None));
    void emitIns_Mov(instruction ins, emitAttr attr, RegNum dstReg, RegNum srgReg, bool canSkip);
    void emitIns_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2);
    void emitIns_R_R_I(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, int32_t imm);
    void emitIns_A(instruction ins, emitAttr attr, GenTree* addr);
    void emitIns_A_I(instruction ins, emitAttr attr, GenTree* addr, int32_t imm);
    void emitIns_A_R(instruction ins, emitAttr attr, GenTree* addr, RegNum reg);
    void emitIns_R_A(instruction ins, emitAttr attr, RegNum reg1, GenTree* addr);
    void emitIns_R_A_I(instruction ins, emitAttr attr, RegNum reg1, GenTree* addr, int32_t imm);
    void emitIns_R_C_I(instruction ins, emitAttr attr, RegNum reg1, ConstData* data, int32_t imm);
    void emitIns_R_S_I(instruction ins, emitAttr attr, RegNum reg1, StackAddrMode s, int32_t imm);
    void emitIns_R_R_A(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, GenTree* addr);
    void emitIns_R_R_C(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, ConstData* data);
    void emitIns_R_R_S(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, StackAddrMode s);
    void emitIns_R_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3);
    void emitIns_R_R_A_I(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, GenTree* addr, int32_t imm);
    void emitIns_S_R_I(instruction ins, emitAttr attr, StackAddrMode s, RegNum reg, int32_t imm);
    void emitIns_A_R_I(instruction ins, emitAttr attr, GenTree* addr, RegNum reg, int32_t imm);
    void emitIns_C_R_I(instruction ins, emitAttr attr, ConstData* data, RegNum reg, int32_t imm);
    void emitIns_R_R_C_I(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, ConstData* data, int32_t imm);
    void emitIns_R_R_R_I(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, int32_t imm);
    void emitIns_R_R_S_I(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, StackAddrMode s, int32_t imm);
    void emitIns_R_R_A_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, GenTree* addr);
    void emitIns_R_R_C_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, ConstData* data);
    void emitIns_R_R_S_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, StackAddrMode s);
    void emitIns_R_R_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, RegNum reg4);
    void emitIns_S(instruction ins, emitAttr attr, StackAddrMode s);
    void emitIns_S_R(instruction ins, emitAttr attr, RegNum reg, StackAddrMode s);
    void emitIns_R_S(instruction ins, emitAttr attr, RegNum reg, StackAddrMode s);
    void emitIns_S_I(instruction ins, emitAttr attr, StackAddrMode s, int32_t imm);
    void emitIns_R_C(instruction ins, emitAttr attr, RegNum reg, ConstData* data);
    void emitIns_C_R(instruction ins, emitAttr attr, ConstData* data, RegNum reg);
    void emitIns_C_I(instruction ins, emitAttr attr, ConstData* data, int32_t imm);
    void emitIns_R_L(RegNum reg, insGroup* label);
#ifdef TARGET_X86
    void emitIns_R_L(RegNum reg, ConstData* data);
#endif
    void emitIns_R_AH(instruction ins, RegNum ireg, void* addr);
    void emitIns_AR(instruction ins, emitAttr attr, RegNum base, int32_t disp);
    void emitIns_ARX(instruction ins, emitAttr attr, RegNum base, RegNum index, unsigned scaled, int32_t disp);
    void emitIns_R_AR(instruction ins, emitAttr attr, RegNum reg, RegNum base, int32_t disp);
    void emitIns_AR_R(instruction ins, emitAttr attr, RegNum reg, RegNum base, int32_t disp);
    void emitIns_ARX_I(
        instruction ins, emitAttr attr, RegNum base, RegNum index, unsigned scale, int32_t disp, int32_t imm);
    void emitIns_R_ARX(
        instruction ins, emitAttr attr, RegNum reg, RegNum base, RegNum index, unsigned scale, int32_t disp);
    void emitIns_ARX_R(
        instruction ins, emitAttr attr, RegNum reg, RegNum base, RegNum index, unsigned scale, int32_t disp);
    void emitIns_AR_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum base, int32_t disp);
    void emitIns_R_AR_R(
        instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum base, RegNum index, int scale, int32_t disp);
    void emitIns_SIMD_R_R_I(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, int32_t imm);
    void emitIns_SIMD_R_R_A(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, GenTree* addr);
    void emitIns_SIMD_R_R_C(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, ConstData* data);
    void emitIns_SIMD_R_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum op2Reg);
    void emitIns_SIMD_R_R_S(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, StackAddrMode s);
    void emitIns_SIMD_R_R_A_I(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, GenTree* addr, int32_t imm);
    void emitIns_SIMD_R_R_C_I(instruction ins, emitAttr attr, RegNum reg1, RegNum op1Reg, ConstData* data, int32_t imm);
    void emitIns_SIMD_R_R_R_I(instruction ins, emitAttr attr, RegNum reg1, RegNum op1Reg, RegNum op2Reg, int32_t imm);
    void emitIns_SIMD_R_R_S_I(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, StackAddrMode s, int32_t imm);
    void emitIns_SIMD_R_R_R_A(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, GenTree* addr);
    void emitIns_SIMD_R_R_R_C(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, ConstData* data);
    void emitIns_SIMD_R_R_R_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, RegNum reg4);
    void emitIns_SIMD_R_R_R_S(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, StackAddrMode s);
    void emitIns_SIMD_R_R_A_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, GenTree* addr);
    void emitIns_SIMD_R_R_S_R(instruction ins, emitAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, StackAddrMode s);

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
                      RegNum   amBase  = REG_NA,
                      RegNum   amIndex = REG_NA,
                      unsigned amScale = 0,
                      int32_t  amDisp  = 0,
                      bool     isJump  = false);

private:
    bool UseVEXEncoding() const;

    unsigned emitGetAdjustedSize(instruction ins, emitAttr attr, code_t code, bool isRR = false);
    unsigned emitInsSizeR(instruction ins, emitAttr size, RegNum reg);
    unsigned emitInsSizeRI(instruction ins, emitAttr size, RegNum reg, ssize_t imm);
    unsigned emitInsSizeRR(instruction ins, emitAttr size, RegNum reg1, RegNum reg2);
    unsigned emitInsSizeRRI(instruction ins, emitAttr size, RegNum reg1, RegNum reg2);
    unsigned emitInsSizeRRR(instruction ins, emitAttr size, RegNum reg3);
    unsigned emitInsSizeSV(instrDesc* id, code_t code);
    unsigned emitInsSizeAM(instrDesc* id, code_t code);
    unsigned emitInsSizeCV(instrDesc* id, code_t code);

    bool IsRedundantMov(instruction ins, emitAttr size, RegNum dst, RegNum src, bool canIgnoreSideEffects);

    bool TakesVexPrefix(instruction ins) const;

    bool IsVexDstDstSrc(instruction ins) const;
    bool IsVexDstSrcSrc(instruction ins) const;
    INDEBUG(bool IsVexTernary(instruction ins) const;)
    INDEBUG(bool IsReallyVexTernary(instruction ins) const;)

    bool AreFlagsAlwaysModified(instrDesc* id);

    /************************************************************************/
    /*  Private members that deal with target-dependent instr. descriptors  */
    /************************************************************************/

    void SetInstrLclAddrMode(instrDesc* id, StackAddrMode s);
    ssize_t GetAddrModeDisp(GenTree* addr);
    void SetInstrAddrMode(instrDesc* id, GenTree* addr);
    bool IntConNeedsReloc(GenTreeIntCon* con);

    template <typename T>
    T* AllocInstr(bool updateLastIns = true);

    instrDesc*     emitNewInstr();
    instrDesc*     emitNewInstrSmall();
    instrDescJmp*  emitNewInstrJmp();
    instrDescCGCA* emitAllocInstrCGCA();
    instrDesc* emitNewInstrSC(ssize_t imm);
    instrDesc* emitNewInstrCns(int32_t imm);
#ifdef TARGET_X86
    instrDesc* emitNewInstrDsp(int32_t disp);
#endif
    instrDesc* emitNewInstrAmd(ssize_t disp);
    instrDesc* emitNewInstrAmdCns(ssize_t disp, int32_t imm);
    instrDesc* emitNewInstrGCReg(emitAttr attr, RegNum reg);
    instrDesc* emitNewInstrCall(CORINFO_METHOD_HANDLE methodHandle,
                                emitAttr              retRegAttr,
#ifdef UNIX_AMD64_ABI
                                emitAttr retReg2Attr,
#endif
#ifdef TARGET_X86
                                int argSlotCount,
#endif
                                int32_t disp);

#if !FEATURE_FIXED_OUT_ARGS
    void emitAdjustStackDepthPushPop(instruction ins);
    void emitAdjustStackDepth(instruction ins, ssize_t val);
#endif

    void emitLoopAlign(uint16_t paddingBytes);
    void emitLongLoopAlign(uint16_t alignmentBoundary);

#ifdef DEBUG

    /************************************************************************/
    /*             Debug-only routines to display instructions              */
    /************************************************************************/

    void PrintHexCode(instrDesc* id, const uint8_t* code, size_t sz);
    void emitDispIns(instrDesc* id, bool isNew, bool doffs, unsigned offs);

#endif // DEBUG
};

using ArchEmitter = X86Emitter;

#endif // TARGET_XARCH
