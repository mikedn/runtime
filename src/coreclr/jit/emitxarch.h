// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifdef TARGET_XARCH

enum CallInsKind
{
    CK_FUNC_TOKEN       = IF_METHOD,
    CK_FUNC_TOKEN_INDIR = IF_METHPTR,
    CK_INDIR_R          = IF_RRD,
    CK_INDIR_ARD        = IF_ARD
};

class X86Emitter final : public EmitterBase
{
    friend class X86Encoder;
    friend class X86AsmPrinter;
    friend class EmitterBase;

    using Ins     = instruction;
    using InsAttr = emitAttr;
    using InsFmt  = insFormat;

private:
    bool useVexEncoding = false;

public:
    X86Emitter(Compiler* compiler, CodeGen* codeGen, ICorJitInfo* jitInfo) : EmitterBase(compiler, codeGen, jitInfo)
    {
    }

    void SetUseVexEncoding(bool value)
    {
        useVexEncoding = value;
    }

    bool UseVexEncoding() const
    {
        return useVexEncoding;
    }

    // code_t is a type used to accumulate bits of opcode + prefixes. On amd64, it must be 64 bits
    // to support the REX prefixes. On both x86 and amd64, it must be 64 bits to support AVX, with
    // its 3-byte VEX prefix.
    using code_t = uint64_t;

    bool AreFlagsSetToZeroCmp(RegNum reg, emitAttr opSize, genTreeOps treeOps);
    bool AreUpper32BitsZero(RegNum reg);

#ifdef TARGET_AMD64
    bool IsLastInsCall() const
    {
        return (lastInstr != nullptr) && (lastInstr->idIns() == INS_call);
    }
#endif

    void PrologSpillParamRegsToShadowSlots();

#ifdef TARGET_X86
    void SetStackLevel(unsigned stackLevel);
#endif

    /************************************************************************/
    /*           The public entry points to output instructions             */
    /************************************************************************/

    void emitIns(Ins ins);
    void emitIns(Ins ins, InsAttr attr);
    void emitIns_J(Ins ins, insGroup* label);
    void emitIns_J(Ins ins, int instrCount = 0);
    void InsRMW_A(Ins ins, InsAttr attr, GenTree* addr);
    void InsRMW_A_I(Ins ins, InsAttr attr, GenTree* addr, int32_t imm);
    void InsRMW_A_R(Ins ins, InsAttr attr, GenTree* addr, RegNum reg);
    void Ins_Nop(unsigned size);
    void Ins_Lock();
#ifdef TARGET_AMD64
    void emitIns_CallFinally(insGroup* label);
#endif
#ifdef TARGET_X86
    void Ins_H(Ins ins, void* addr);
    void Ins_L(Ins ins, insGroup* label);
#endif
#ifdef WINDOWS_X86_ABI
    void InsMov_R_FS(RegNum reg, int32_t disp);
#endif
    void Ins_I(Ins ins, InsAttr attr, int32_t val);
    void Ins_R(Ins ins, InsAttr attr, RegNum reg);
    void Ins_C(Ins ins, InsAttr attr, ConstData* data);
    void Ins_R_H(Ins ins, RegNum reg, void* addr DEBUGARG(HandleKind handleKind = HandleKind::None));
    void Ins_R_I(Ins ins, InsAttr attr, RegNum reg, ssize_t val DEBUGARG(HandleKind handleKind = HandleKind::None));
    void emitIns_Mov(Ins ins, InsAttr attr, RegNum dstReg, RegNum srgReg, bool canSkip);
    void Ins_R_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2);
    void Ins_R_R_I(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, int32_t imm);
    void Ins_A(Ins ins, InsAttr attr, GenTree* addr);
    void Ins_A_I(Ins ins, InsAttr attr, GenTree* addr, int32_t imm);
    void Ins_A_R(Ins ins, InsAttr attr, GenTree* addr, RegNum reg);
    void Ins_R_A(Ins ins, InsAttr attr, RegNum reg1, GenTree* addr);
    void Ins_R_A_I(Ins ins, InsAttr attr, RegNum reg1, GenTree* addr, int32_t imm);
    void Ins_R_C_I(Ins ins, InsAttr attr, RegNum reg1, ConstData* data, int32_t imm);
    void Ins_R_R_A(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, GenTree* addr);
    void Ins_R_R_C(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, ConstData* data);
    void Ins_R_R_S(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, StackAddrMode s);
    void Ins_R_R_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3);
    void Ins_R_R_A_I(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, GenTree* addr, int32_t imm);
    void Ins_S_R_I(Ins ins, InsAttr attr, StackAddrMode s, RegNum reg, int32_t imm);
    void Ins_R_S_I(Ins ins, InsAttr attr, RegNum reg, StackAddrMode s, int32_t imm);
    void emitIns_R_S_I(Ins ins, InsAttr attr, RegNum reg, StackAddrMode s, int32_t imm);
    void Ins_A_R_I(Ins ins, InsAttr attr, GenTree* addr, RegNum reg, int32_t imm);
    void Ins_C_R_I(Ins ins, InsAttr attr, ConstData* data, RegNum reg, int32_t imm);
    void Ins_R_R_C_I(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, ConstData* data, int32_t imm);
    void Ins_R_R_R_I(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, int32_t imm);
    void Ins_R_R_S_I(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, StackAddrMode s, int32_t imm);
    void Ins_R_R_A_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, GenTree* addr);
    void Ins_R_R_C_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, ConstData* data);
    void Ins_R_R_S_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, StackAddrMode s);
    void Ins_R_R_R_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, RegNum reg4);
    void Ins_S(Ins ins, InsAttr attr, StackAddrMode s);
    void emitIns_S_R(Ins ins, InsAttr attr, RegNum reg, StackAddrMode s);
    void emitIns_R_S(Ins ins, InsAttr attr, RegNum reg, StackAddrMode s);
    void Ins_S_I(Ins ins, InsAttr attr, StackAddrMode s, int32_t imm);
    void Ins_R_C(Ins ins, InsAttr attr, RegNum reg, ConstData* data);
    void Ins_C_R(Ins ins, InsAttr attr, ConstData* data, RegNum reg);
    void Ins_C_I(Ins ins, InsAttr attr, ConstData* data, int32_t imm);
    void Ins_R_L(RegNum reg, insGroup* label);
#ifdef TARGET_X86
    void Ins_R_L(RegNum reg, ConstData* data);
#endif
    void Ins_R_AH(Ins ins, RegNum ireg, void* addr);
    void Ins_AR(Ins ins, InsAttr attr, RegNum base, int32_t disp);
    void Ins_ARX(Ins ins, InsAttr attr, RegNum base, RegNum index, unsigned scaled, int32_t disp);
    void Ins_R_AR(Ins ins, InsAttr attr, RegNum reg, RegNum base, int32_t disp);
    void Ins_AR_R(Ins ins, InsAttr attr, RegNum reg, RegNum base, int32_t disp);
    void Ins_ARX_I(Ins ins, InsAttr attr, RegNum base, RegNum index, unsigned scale, int32_t disp, int32_t imm);
    void Ins_R_ARX(Ins ins, InsAttr attr, RegNum reg, RegNum base, RegNum index, unsigned scale, int32_t disp);
    void Ins_ARX_R(Ins ins, InsAttr attr, RegNum reg, RegNum base, RegNum index, unsigned scale, int32_t disp);
    void Ins_ARX_R_I(
        Ins ins, InsAttr attr, RegNum base, RegNum index, unsigned scale, int32_t disp, RegNum reg, int32_t imm);
    void Ins_R_ARX_I(
        Ins ins, InsAttr attr, RegNum reg, RegNum base, RegNum index, unsigned scale, int32_t disp, int32_t imm);
    void Ins_AR_R_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum base, int32_t disp);
    void Ins_R_AR_R(
        Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum base, RegNum index, int scale, int32_t disp);

    void VexIns_R_R_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3);
    void VexIns_R_R_I(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, int32_t imm);
    void VexIns_R_R_A(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, GenTree* addr);
    void VexIns_R_R_C(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, ConstData* data);
    void VexIns_R_R_S(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, StackAddrMode s);

    void VexIns_R_R_R_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, RegNum reg4);
    void VexIns_R_R_R_I(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, int32_t imm);
    void VexIns_R_R_R_A(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, GenTree* addr);
    void VexIns_R_R_R_C(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, ConstData* data);
    void VexIns_R_R_R_S(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, StackAddrMode s);

    void VexIns_R_R_A_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, GenTree* addr);
    void VexIns_R_R_A_I(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, GenTree* addr, int32_t imm);
    void VexIns_R_R_C_I(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, ConstData* data, int32_t imm);
    void VexIns_R_R_S_R(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, RegNum reg3, StackAddrMode s);
    void VexIns_R_R_S_I(Ins ins, InsAttr attr, RegNum reg1, RegNum reg2, StackAddrMode s, int32_t imm);

    void emitIns_Call(InsFmt   format,
                      void*    addr,
                      RegNum   amBase,
                      RegNum   amIndex,
                      unsigned amScale,
                      int32_t  amDisp,
                      bool     isJump,
                      InsAttr  retRegAttr,
#ifdef UNIX_AMD64_ABI
                      InsAttr retReg2Attr,
#endif
#ifdef TARGET_X86
                      int32_t argSize,
#endif
                      CORINFO_METHOD_HANDLE methodHandle
#ifdef DEBUG
                      ,
                      CORINFO_SIG_INFO* sigInfo = nullptr
#endif
                      );

private:
    unsigned EncodingSize(instruction ins, emitAttr attr, code_t code, bool isRR = false);
    unsigned EncodingSizeR(instruction ins, emitAttr size, RegNum reg);
    unsigned EncodingSizeRI(instruction ins, emitAttr size, RegNum reg, ssize_t imm);
    unsigned EncodingSizeRR(instruction ins, emitAttr size, RegNum reg1, RegNum reg2);
    unsigned EncodingSizeRRI(instruction ins, emitAttr size, RegNum reg1, RegNum reg2);
    unsigned EncodingSizeRRR(instruction ins, emitAttr size, RegNum reg3);
    unsigned EncodingSizeSV(instrDesc* id, code_t code);
    unsigned EncodingSizeAM(instrDesc* id, code_t code);
    unsigned EncodingSizeCV(instrDesc* id, code_t code);

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

    instrDesc*     NewInstr();
    instrDesc*     NewInstrSmall();
    instrDescJmp*  NewInstrJmp();
    instrDescCGCA* AllocInstrCGCA();
    instrDesc* NewInstrSmall(ssize_t imm);
    instrDesc* NewInstrCns(int32_t imm);
#ifdef TARGET_X86
    instrDesc* NewInstrDsp(int32_t disp);
#endif
    instrDesc* NewInstrAMDisp(ssize_t disp);
    instrDesc* NewInstrAMDispImm(ssize_t disp, int32_t imm);
    instrDesc* NewInstrGCReg(emitAttr attr, RegNum reg);
    instrDesc* NewInstrCall(CORINFO_METHOD_HANDLE methodHandle,
                            emitAttr              retRegAttr,
#ifdef UNIX_AMD64_ABI
                            emitAttr retReg2Attr,
#endif
#ifdef TARGET_X86
                            int argSlotCount,
#endif
                            int32_t disp);

#if !FEATURE_FIXED_OUT_ARGS
    void UpdateStackLevel(instruction ins);
    void UpdateStackLevel(instruction ins, ssize_t val);
#endif

    void emitLoopAlign(uint16_t paddingBytes);
    void emitLongLoopAlign(uint16_t alignmentBoundary);

#ifdef DEBUG
    void PrintInstr(instrDesc* id);
#endif
};

using ArchEmitter = X86Emitter;

#endif // TARGET_XARCH
