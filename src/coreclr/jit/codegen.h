// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifndef CODEGEN_H
#define CODEGEN_H

#include "codegeninterface.h"
#include "compiler.h"
#include "regset.h"
#include "treelifeupdater.h"
#include "emit.h"

#ifdef FEATURE_EH_FUNCLETS
enum FuncKind : uint8_t;
struct FuncInfoDsc;
#endif

class CodeGen final : public CodeGenInterface
{
    friend class emitter;
    friend class DisAssembler;
    friend class CodeGenLivenessUpdater;
    friend class CodeGenInterface;

    //  The following holds information about instr offsets in terms of generated code.
    struct IPmappingDsc
    {
        IPmappingDsc* ipmdNext;      // next line# record
        emitLocation  ipmdNativeLoc; // the emitter location of the native code corresponding to the IL offset
        IL_OFFSETX    ipmdILoffsx;   // the instr offset
        bool          ipmdIsLabel;   // Can this code be a branch label?
    };

    class LinearScan* m_lsra           = nullptr;
    IPmappingDsc*     genIPmappingList = nullptr;
    IPmappingDsc*     genIPmappingLast = nullptr;
    BasicBlock*       m_currentBlock   = nullptr;
    GSCookie*         m_gsCookieAddr   = nullptr;
    GSCookie          m_gsCookieVal    = 0;

    CodeGenLivenessUpdater liveness;

#ifdef TARGET_ARMARCH
    // Registers reserved for special purposes, that cannot be allocated by LSRA.
    regMaskTP reservedRegs = RBM_NONE;
#endif

public:
    CodeGen(Compiler* compiler);

    void genGenerateCode(void** nativeCode, uint32_t* nativeCodeSize);
#ifdef LATE_DISASM
    const char* siRegVarName(size_t offs, size_t size, unsigned reg);
    const char* siStackVarName(size_t offs, size_t size, unsigned reg, unsigned stkOffs);
#endif

    BasicBlock* GetCurrentBlock() const
    {
        return m_currentBlock;
    }

    void genCreateFunclets();
    void genAllocateRegisters();
    void genGenerateMachineCode();
    void genEmitMachineCode();
    void genEmitUnwindDebugGCandEH();

    VARSET_VALARG_TP GetLiveSet() const
    {
        return liveness.GetLiveSet();
    }

private:
#if defined(TARGET_XARCH)
    // Bit masks used in negating a float or double number.
    // This is to avoid creating more than one data constant for these bitmasks when a
    // method has more than one GT_FNEG operation on floating point values.
    CORINFO_FIELD_HANDLE negBitmaskFlt = nullptr;
    CORINFO_FIELD_HANDLE negBitmaskDbl = nullptr;

    // Bit masks used in computing Math.Abs() of a float or double number.
    CORINFO_FIELD_HANDLE absBitmaskFlt = nullptr;
    CORINFO_FIELD_HANDLE absBitmaskDbl = nullptr;

    // Bit mask used in U8 -> double conversion to adjust the result.
    CORINFO_FIELD_HANDLE u8ToDblBitmask = nullptr;
    CORINFO_FIELD_HANDLE u8ToFltBitmask = nullptr;

    void GenFloatAbs(GenTreeIntrinsic* node);

    void genSSE41RoundOp(GenTreeIntrinsic* node);

    instruction simdAlignedMovIns()
    {
        // We use movaps when non-VEX because it is a smaller instruction;
        // however the VEX version vmovaps would be used which is the same size as vmovdqa;
        // also vmovdqa has more available CPU ports on older processors so we switch to that
        return compiler->canUseVexEncoding() ? INS_movdqa : INS_movaps;
    }
    instruction simdUnalignedMovIns()
    {
        // We use movups when non-VEX because it is a smaller instruction;
        // however the VEX version vmovups would be used which is the same size as vmovdqu;
        // but vmovdqu has more available CPU ports on older processors so we switch to that
        return compiler->canUseVexEncoding() ? INS_movdqu : INS_movups;
    }
#endif // defined(TARGET_XARCH)

    void genMarkLabelsForCodegen();

    regNumber genFramePointerReg()
    {
        if (isFramePointerUsed())
        {
            return REG_FPBASE;
        }
        else
        {
            return REG_SPBASE;
        }
    }

    void genRangeCheck(GenTreeBoundsChk* bndsChk);

    void genLockedInstructions(GenTreeOp* node);
#ifdef TARGET_XARCH
    void genCodeForLockAdd(GenTreeOp* node);
#endif

#ifdef TARGET_ARMARCH
    // On some targets such as the ARM we may need to have an extra reserved register
    // that is used when addressing stack based locals and stack based temps.
    // This method returns the regNumber that should be used when an extra register
    // is needed to access the stack based locals and stack based temps.
    regNumber rsGetRsvdReg()
    {
        // We should have already added this register to the mask
        // of reserved registers in regSet.rdMaskResvd
        noway_assert((reservedRegs & RBM_OPT_RSVD) != 0);

        return REG_OPT_RSVD;
    }
#endif // TARGET_ARMARCH

    bool generatingProlog = false;
    bool generatingEpilog = false;

    bool     genUseBlockInit;  // true if we plan to block-initialize the local stack frame
    unsigned genInitStkLclCnt; // The count of local variables that we need to zero init

#if !FEATURE_FIXED_OUT_ARGS
    void SubtractStackLevel(unsigned adjustment);
    void AddStackLevel(unsigned adjustment);
    void SetStackLevel(unsigned newStackLevel);
#endif

    void genReportEH();
    void eeSetEHcount(unsigned cEH);
    void eeSetEHinfo(unsigned EHnumber, const CORINFO_EH_CLAUSE* clause);
#ifdef DEBUG
    void dispOutgoingEHClause(unsigned num, const CORINFO_EH_CLAUSE& clause);
#endif

private:
    // the current (pending) label ref, a label which has been referenced but not yet seen
    insGroup* genPendingCallLabel = nullptr;

#ifdef JIT32_GCENCODER
    unsigned gcInfoSize = 0;
#endif
#if defined(DEBUG) || defined(LATE_DISASM) || DUMP_FLOWGRAPHS
    double perfScore = 0.0;
#endif

    // JIT-time constants for use in multi-dimensional array code generation.
    unsigned genOffsetOfMDArrayLowerBound(var_types elemType, unsigned rank, unsigned dimension);
    unsigned genOffsetOfMDArrayDimensionSize(var_types elemType, unsigned rank, unsigned dimension);

    void InitLclBlockLiveInRegs();
    void genCodeForBBlist();

public:
#ifdef JIT32_GCENCODER
    void SetGCInfoSize(unsigned size)
    {
        gcInfoSize = size;
    }
#endif

    bool SpillRegCandidateLclVar(GenTreeLclVar* node);

#ifdef TARGET_X86
    void genEmitHelperCall(CorInfoHelpFunc helper, emitAttr retSize = EA_UNKNOWN, regNumber callTarget = REG_NA)
    {
        genEmitHelperCall(helper, 0, retSize, callTarget);
    }

    void genEmitHelperCall(CorInfoHelpFunc helper, int argSize, emitAttr retSize, regNumber callTarget = REG_NA);
#else
    void genEmitHelperCall(CorInfoHelpFunc helper, emitAttr retSize = EA_UNKNOWN, regNumber callTarget = REG_NA);
#endif

    void genGCWriteBarrier(GenTreeStoreInd* store, GCInfo::WriteBarrierForm wbf);

#if !FEATURE_FIXED_OUT_ARGS
    void SetThrowHelperBlockStackLevel(BasicBlock* block);
#endif

    void genExitCode(BasicBlock* block);

    void genJumpToThrowHlpBlk(emitJumpKind condition, ThrowHelperKind throwKind, BasicBlock* throwBlock = nullptr);

#ifdef TARGET_ARM64
    void genCheckOverflow(GenTree* tree);
#endif

    unsigned prologSize;
#ifdef JIT32_GCENCODER
    unsigned epilogSize;
#endif

    void PrologEstablishFramePointer(int delta, bool reportUnwindData);

    struct ParamRegInfo
    {
        unsigned lclNum;
        // index into the param registers table of the register that will be copied to this register.
        // That is, for paramRegs[x].trashBy = y, argument register number 'y' will be copied to
        // argument register number 'x'. Only used when circular = true.
        unsigned trashBy;

        uint8_t   regIndex;
        bool      stackArg;  // true if the argument gets homed to the stack
        bool      writeThru; // true if the argument gets homed to both stack and register
        bool      processed; // true after we've processed the argument (and it is in its final location)
        bool      circular;  // true if this register participates in a circular dependency loop.
        var_types type;
    };

    void genPrologMoveParamRegs(
        unsigned regCount, regMaskTP regLiveIn, bool isFloat, regNumber tempReg, bool* tempRegClobbered);
    regMaskTP genPrologBuildParamRegsTable(
        ParamRegInfo* paramRegs, unsigned paramRegCount, regMaskTP liveParamRegs, bool isFloat, regNumber tempReg);
    void genPrologMarkParamRegsCircularDependencies(ParamRegInfo* paramRegs,
                                                    unsigned      paramRegCount,
                                                    regMaskTP     liveParamRegs);
    regMaskTP genPrologSpillParamRegs(ParamRegInfo* paramRegs, unsigned paramRegCount, regMaskTP liveParamRegs);
    void genPrologMoveParamRegs(ParamRegInfo* paramRegs,
                                unsigned      paramRegCount,
                                regMaskTP     liveParamRegs,
                                bool          isFloat,
                                regNumber     tempReg,
                                bool*         tempRegClobbered);
    void genPrologEnregisterIncomingStackParams();
    void MarkStackLocals();
    void CheckUseBlockInit();
    void MarkGCTrackedSlots(int&       minBlockInitOffset,
                            int&       maxBlockInitOffset,
                            regMaskTP& initRegs ARM_ARG(regMaskTP& initDblRegs));
#ifdef UNIX_AMD64_ABI
    void PrologClearVector3StackParamUpperBits();
#endif

#if defined(TARGET_ARM64)
    bool genInstrWithConstant(instruction ins,
                              emitAttr    attr,
                              regNumber   reg1,
                              regNumber   reg2,
                              ssize_t     imm,
                              regNumber   tmpReg,
                              bool        inUnwindRegion = false);

    void genStackPointerAdjustment(ssize_t spAdjustment, regNumber tmpReg, bool* pTmpRegIsZero, bool reportUnwindData);

    void genPrologSaveRegPair(regNumber reg1,
                              regNumber reg2,
                              int       spOffset,
                              int       spDelta,
                              bool      useSaveNextPair,
                              regNumber tmpReg,
                              bool*     pTmpRegIsZero);

    void genPrologSaveReg(regNumber reg1, int spOffset, int spDelta, regNumber tmpReg, bool* pTmpRegIsZero);

    void genEpilogRestoreRegPair(regNumber reg1,
                                 regNumber reg2,
                                 int       spOffset,
                                 int       spDelta,
                                 bool      useSaveNextPair,
                                 regNumber tmpReg,
                                 bool*     pTmpRegIsZero);

    void genEpilogRestoreReg(regNumber reg1, int spOffset, int spDelta, regNumber tmpReg, bool* pTmpRegIsZero);

    // A simple struct to keep register pairs for prolog and epilog.
    struct RegPair
    {
        regNumber reg1;
        regNumber reg2;
        bool      useSaveNextPair;

        RegPair(regNumber reg1) : reg1(reg1), reg2(REG_NA), useSaveNextPair(false)
        {
        }

        RegPair(regNumber reg1, regNumber reg2) : reg1(reg1), reg2(reg2), useSaveNextPair(false)
        {
            assert(reg2 == REG_NEXT(reg1));
        }
    };

    static void genBuildRegPairsStack(regMaskTP regsMask, ArrayStack<RegPair>* regStack);
    static void genSetUseSaveNextPairs(ArrayStack<RegPair>* regStack);

    static int genGetSlotSizeForRegsInMask(regMaskTP regsMask);

    void genSaveCalleeSavedRegisterGroup(regMaskTP regsMask, int spDelta, int spOffset);
    void genRestoreCalleeSavedRegisterGroup(regMaskTP regsMask, int spDelta, int spOffset);

    void genSaveCalleeSavedRegistersHelp(regMaskTP regsToSaveMask, int lowestCalleeSavedOffset, int spDelta);
    void genRestoreCalleeSavedRegistersHelp(regMaskTP regsToRestoreMask, int lowestCalleeSavedOffset, int spDelta);

    void PrologPushCalleeSavedRegisters(regNumber initReg, bool* pInitRegZeroed);
#else
    void PrologPushCalleeSavedRegisters();
#endif

    void PrologAllocLclFrame(unsigned frameSize, regNumber initReg, bool* pInitRegZeroed, regMaskTP maskArgRegsLiveIn);

    void genPoisonFrame(regMaskTP bbRegLiveIn);

#if defined(TARGET_ARM)

    void genInstrWithConstant(instruction ins, regNumber reg1, regNumber reg2, int32_t imm, regNumber tmpReg);

    void genStackPointerAdjustment(int32_t spAdjustment, regNumber tmpReg);

    void genPushFltRegs(regMaskTP regMask);
    void genPopFltRegs(regMaskTP regMask);
    regMaskTP genStackAllocRegisterMask(unsigned frameSize, regMaskTP maskCalleeSavedFloat);

    void genFreeLclFrame(unsigned frameSize, bool* pUnwindStarted);

    void genMov32RelocatableDisplacement(BasicBlock* block, regNumber reg);
    void genMov32RelocatableDisplacement(insGroup* block, regNumber reg);
    void genMov32RelocatableDataLabel(unsigned value, regNumber reg);

    bool genUsedPopToReturn; // True if we use the pop into PC to return,
                             // False if we didn't and must branch to LR to return.

    // A set of information that is used by funclet prolog and epilog generation. It is collected once, before
    // funclet prologs and epilogs are generated, and used by all funclet prologs and epilogs, which must all be the
    // same.
    struct FuncletFrameInfoDsc
    {
        regMaskTP fiSaveRegs;                  // Set of registers saved in the funclet prolog (includes LR)
        unsigned  fiFunctionCallerSPtoFPdelta; // Delta between caller SP and the frame pointer
        unsigned  fiSpDelta;                   // Stack pointer delta
        unsigned  fiPSP_slot_SP_offset;        // PSP slot offset from SP
        int       fiPSP_slot_CallerSP_offset;  // PSP slot offset from Caller SP
    };

    FuncletFrameInfoDsc genFuncletInfo;

#elif defined(TARGET_ARM64)

    // A set of information that is used by funclet prolog and epilog generation. It is collected once, before
    // funclet prologs and epilogs are generated, and used by all funclet prologs and epilogs, which must all be the
    // same.
    struct FuncletFrameInfoDsc
    {
        regMaskTP fiSaveRegs;                // Set of callee-saved registers saved in the funclet prolog (includes LR)
        int fiFunction_CallerSP_to_FP_delta; // Delta between caller SP and the frame pointer in the parent function
                                             // (negative)
        int fiSP_to_FPLR_save_delta;         // FP/LR register save offset from SP (positive)
        int fiSP_to_PSP_slot_delta;          // PSP slot offset from SP (positive)
        int fiSP_to_CalleeSave_delta;        // First callee-saved register slot offset from SP (positive)
        int fiCallerSP_to_PSP_slot_delta;    // PSP slot offset from Caller SP (negative)
        int fiFrameType;                     // Funclet frame types are numbered. See genFuncletProlog() for details.
        int fiSpDelta1;                      // Stack pointer delta 1 (negative)
        int fiSpDelta2;                      // Stack pointer delta 2 (negative)
    };

    FuncletFrameInfoDsc genFuncletInfo;

#elif defined(TARGET_AMD64)

    // A set of information that is used by funclet prolog and epilog generation. It is collected once, before
    // funclet prologs and epilogs are generated, and used by all funclet prologs and epilogs, which must all be the
    // same.
    struct FuncletFrameInfoDsc
    {
        unsigned fiFunction_InitialSP_to_FP_delta; // Delta between Initial-SP and the frame pointer
        unsigned fiSpDelta;                        // Stack pointer delta
        int      fiPSP_slot_InitialSP_offset;      // PSP slot offset from Initial-SP
    };

    FuncletFrameInfoDsc genFuncletInfo;

#endif // TARGET_AMD64

#ifdef FEATURE_EH_FUNCLETS
    uint16_t     compFuncInfoCount   = 0;
    uint16_t     currentFuncletIndex = 0;
    FuncInfoDsc* compFuncInfos       = nullptr;

    uint16_t compFuncCount() const
    {
        assert(compFuncInfos != nullptr);
        return compFuncInfoCount;
    }

    unsigned funGetFuncIdx(BasicBlock* block);

    FuncInfoDsc& funGetFunc(unsigned index);

    FuncInfoDsc& funCurrentFunc()
    {
        return funGetFunc(currentFuncletIndex);
    }

    FuncInfoDsc& funSetCurrentFunc(unsigned index)
    {
        assert(FitsIn<uint16_t>(index));
        noway_assert(index < compFuncInfoCount);
        currentFuncletIndex = static_cast<int16_t>(index);
        return funGetFunc(index);
    }
#else
    void funSetCurrentFunc(unsigned index)
    {
        assert(index == 0);
    }
#endif

#ifdef TARGET_XARCH
    void PrologPreserveCalleeSavedFloatRegs(unsigned lclFrameSize);
    void genRestoreCalleeSavedFltRegs(unsigned lclFrameSize);
    void genVzeroupperIfNeeded(bool check256bitOnly = true);
#endif

    regNumber PrologChooseInitReg(regMaskTP initRegs);
    void PrologBlockInitLocals(int untrackedLo, int untrackedHi, regNumber initReg, bool* initRegZeroed);
    void PrologZeroInitUntrackedLocals(regNumber initReg, bool* initRegZeroed);
    void PrologInitOsrLocals();
    void PrologZeroRegs(regMaskTP initRegs, regNumber initReg ARM_ARG(regMaskTP doubleRegs));

    void PrologReportGenericContextArg(regNumber initReg, bool* pInitRegZeroed);

    void PrologSetGSSecurityCookie(regNumber initReg, bool* pInitRegZeroed);

    void genFinalizeFrame();

#ifdef PROFILING_SUPPORTED
    void PrologProfilingEnterCallback(regNumber initReg, bool* pInitRegZeroed);
    void genProfilingLeaveCallback(CorInfoHelpFunc helper);
#endif

#if defined(TARGET_ARM)
    bool genCanUsePopToReturn(regMaskTP maskPopRegsInt, bool jmpEpilog);
#endif

#ifdef TARGET_ARM64
    void genPopCalleeSavedRegistersAndFreeLclFrame(bool jmpEpilog);
#else
    void genPopCalleeSavedRegisters(bool jmpEpilog = false);
#endif

    void genFnProlog();
    void genFnEpilog(BasicBlock* block);
    void UpdateParamsWithInitialReg();
    void PrologMoveParams(regNumber initReg, bool* initRegZeroed);
#ifdef TARGET_X86
    void PrologInitVarargsStackParamsBaseOffset();
#endif

#ifdef FEATURE_EH_FUNCLETS
    void genFuncletProlog(BasicBlock* block);
    void genFuncletEpilog();
    void genCaptureFuncletPrologEpilogInfo();

    void PrologSetPSPSym(regNumber initReg, bool* pInitRegZeroed);

    void genUpdateCurrentFunclet(BasicBlock* block);
#ifdef TARGET_ARM
    void genInsertNopForUnwinder(BasicBlock* block);
#endif

#else  // !FEATURE_EH_FUNCLETS
    void genUpdateCurrentFunclet(BasicBlock* block)
    {
    }
#endif // !FEATURE_EH_FUNCLETS

    void genGeneratePrologsAndEpilogs();

#if defined(DEBUG) && defined(TARGET_ARM64)
    void genArm64EmitterUnitTests();
#endif

#if defined(DEBUG) && defined(LATE_DISASM) && defined(TARGET_AMD64)
    void genAmd64EmitterUnitTests();
#endif

#ifdef TARGET_X86
    var_types PushTempReg(regNumber reg);
    void PopTempReg(regNumber reg, var_types type);
#endif

    INDEBUG(void genIPmappingDisp(IPmappingDsc* ipMapping);)

    void genIPmappingAdd(IL_OFFSETX offset, bool isLabel);
    void genIPmappingAddToFront(IL_OFFSETX offset);
    void genIPmappingGen();

    unsigned eeBoundariesCount;

    struct boundariesDsc
    {
        UNATIVE_OFFSET nativeIP;
        IL_OFFSET      ilOffset;
        unsigned       sourceReason;
    } * eeBoundaries; // Boundaries to report to EE

    void eeSetLIcount(unsigned count);
    void eeSetLIinfo(unsigned which, UNATIVE_OFFSET offs, unsigned srcIP, bool stkEmpty, bool callInstruction);
    void eeSetLIdone();

#ifdef DEBUG
    static void eeDispILOffs(IL_OFFSET offs);
    static void eeDispLineInfo(const boundariesDsc* line);
    void eeDispLineInfos();
#endif

    struct VarResultInfo
    {
        uint32_t      startOffset;
        uint32_t      endOffset;
        uint32_t      varNumber;
        DbgInfoVarLoc loc;
    };

    VarResultInfo* eeSetLVcount(unsigned count);
    void eeSetLVdone(VarResultInfo* vars, unsigned count);

#ifdef DEBUG
    void eeDispVar(ICorDebugInfo::NativeVarInfo* var);
    void eeDispVars(CORINFO_METHOD_HANDLE ftn, ULONG32 cVars, ICorDebugInfo::NativeVarInfo* vars);
#endif

    void genEnsureCodeEmitted(IL_OFFSETX offsx);

    void genSetScopeInfo();
    void genSetScopeInfoUsingVariableRanges(VarResultInfo* vars);
    void genSetScopeInfo(VarResultInfo* vars,
                         unsigned       index,
                         uint32_t       startOffs,
                         uint32_t       endOffs,
                         unsigned       lclNum,
                         unsigned       ilVarNum,
                         DbgInfoVarLoc* varLoc);

protected:
#ifdef LATE_DISASM
    struct TrnslLocalVarInfo
    {
        const char*   tlviName;
        uint32_t      tlviStartPC;
        uint32_t      tlviEndPC;
        DbgInfoVarLoc tlviVarLoc;
    };

    TrnslLocalVarInfo* genTrnslLocalVarInfo  = nullptr;
    unsigned           genTrnslLocalVarCount = 0;
#endif

#ifdef TARGET_XARCH
    void GenIntCon(GenTreeIntCon* node, regNumber reg, var_types type);
    void GenDblCon(GenTreeDblCon* node, regNumber reg, var_types type);
#else
    void GenIntCon(GenTreeIntCon* node);
    void GenDblCon(GenTreeDblCon* node);
#endif
    void GenNode(GenTree* node, BasicBlock* block);
    void genCodeForBinary(GenTreeOp* treeNode);
    void GenFloatNegate(GenTreeUnOp* node);
    void GenFloatBinaryOp(GenTreeOp* node);
#ifdef TARGET_XARCH
    void GenFloatCompare(GenTreeOp* cmp);
    void GenIntCompare(GenTreeOp* cmp);
#endif

#ifdef TARGET_X86
    void GenLongUMod(GenTreeOp* node);
#endif

#if defined(TARGET_ARM64) || defined(TARGET_XARCH)
    void GenDivMod(GenTreeOp* treeNode);
#endif

    void GenMul(GenTreeOp* mul);
    void GenMulLong(GenTreeOp* mul);
    void genCodeForIncSaturate(GenTree* treeNode);
    void genLeaInstruction(GenTreeAddrMode* lea);

#ifdef TARGET_ARMARCH
    void genScaledAdd(emitAttr attr, regNumber targetReg, regNumber baseReg, regNumber indexReg, int scale);
    enum BarrierKind{BARRIER_FULL, BARRIER_LOAD_ONLY};
    void instGen_MemoryBarrier(BarrierKind barrierKind = BARRIER_FULL);
#endif

#ifdef TARGET_ARM
    void genCodeForMulLong(GenTreeOp* mul);
#endif

#ifndef TARGET_64BIT
    void genLongToIntCast(GenTreeCast* cast);
#endif

    void genCodeForBitCast(GenTreeUnOp* bitcast);
    void inst_BitCast(var_types dstType, regNumber dstReg, var_types srcType, regNumber srcReg);

    struct GenIntCastDesc
    {
        enum LoadKind : unsigned
        {
            LOAD,
            LOAD_ZERO_EXTEND_SMALL_INT,
            LOAD_SIGN_EXTEND_SMALL_INT,
#ifdef TARGET_64BIT
            LOAD_SIGN_EXTEND_INT
#endif
        };

        enum CheckKind : unsigned
        {
            CHECK_NONE,
            CHECK_SMALL_INT_RANGE,
            CHECK_POSITIVE,
#ifdef TARGET_64BIT
            CHECK_UINT_RANGE,
            CHECK_POSITIVE_INT_RANGE,
            CHECK_INT_RANGE,
#endif
        };

        enum ExtendKind : unsigned
        {
            COPY,
            ZERO_EXTEND_SMALL_INT,
            SIGN_EXTEND_SMALL_INT,
#ifdef TARGET_64BIT
            ZERO_EXTEND_INT,
            SIGN_EXTEND_INT,
#endif
        };

    private:
        LoadKind   m_loadKind;
        unsigned   m_loadSrcSize;
        CheckKind  m_checkKind;
        unsigned   m_checkSrcSize;
        int        m_checkSmallIntMin;
        int        m_checkSmallIntMax;
        ExtendKind m_extendKind;
        unsigned   m_extendSrcSize;

    public:
        GenIntCastDesc(GenTreeCast* cast);

        LoadKind LoadKind() const
        {
            return m_loadKind;
        }

        unsigned LoadSrcSize() const
        {
            return m_loadSrcSize;
        }

        CheckKind CheckKind() const
        {
            return m_checkKind;
        }

        unsigned CheckSrcSize() const
        {
            assert(m_checkKind != CHECK_NONE);
            return m_checkSrcSize;
        }

        int CheckSmallIntMin() const
        {
            assert(m_checkKind == CHECK_SMALL_INT_RANGE);
            return m_checkSmallIntMin;
        }

        int CheckSmallIntMax() const
        {
            assert(m_checkKind == CHECK_SMALL_INT_RANGE);
            return m_checkSmallIntMax;
        }

        ExtendKind ExtendKind() const
        {
            return m_extendKind;
        }

        unsigned ExtendSrcSize() const
        {
            return m_extendSrcSize;
        }
    };

    void genIntCastOverflowCheck(GenTreeCast* cast, const GenIntCastDesc& desc, regNumber reg);
    void genIntToIntCast(GenTreeCast* cast);
    void genFloatToFloatCast(GenTreeCast* cast);
    void genFloatToIntCast(GenTreeCast* cast);
    void genIntToFloatCast(GenTreeCast* cast);

    void genCkfinite(GenTree* treeNode);
    void GenCompare(GenTreeOp* tree);
    void genIntrinsic(GenTreeIntrinsic* node);
    void genPutArgReg(GenTreeUnOp* putArg);
    void genPutArgStk(GenTreePutArgStk* treeNode);
#if FEATURE_FASTTAILCALL
    unsigned GetFirstStackParamLclNum();
#endif
#if FEATURE_ARG_SPLIT
    void genPutArgSplit(GenTreePutArgSplit* treeNode);
#endif

#ifdef FEATURE_SIMD
    void genSIMDUpperSpill(GenTreeUnOp* node);
    void genSIMDUpperUnspill(GenTreeUnOp* node);
    void LoadSIMD12(GenTree* load);
#ifdef TARGET_X86
    void genStoreSIMD12ToStack(regNumber operandReg, regNumber tmpReg);
#endif
#endif

#ifdef FEATURE_HW_INTRINSICS
    void genHWIntrinsic(GenTreeHWIntrinsic* node);
    void genVectorGetElement(GenTreeHWIntrinsic* node);

#ifdef TARGET_XARCH
    void genHWIntrinsic_R_RM(GenTreeHWIntrinsic* node, instruction ins, emitAttr attr, regNumber reg, GenTree* rmOp);
    void genHWIntrinsic_R_RM_I(GenTreeHWIntrinsic* node, instruction ins, int8_t ival);
    void genHWIntrinsic_R_R_RM(GenTreeHWIntrinsic* node, instruction ins, emitAttr attr);
    void genHWIntrinsic_R_R_RM(
        GenTreeHWIntrinsic* node, instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, GenTree* op2);
    void genHWIntrinsic_R_R_RM_I(GenTreeHWIntrinsic* node, instruction ins, int8_t ival);
    void genHWIntrinsic_R_R_RM_R(GenTreeHWIntrinsic* node, instruction ins);
    void genHWIntrinsic_R_R_R_RM(
        instruction ins, emitAttr attr, regNumber targetReg, regNumber op1Reg, regNumber op2Reg, GenTree* op3);
    void genBaseIntrinsic(GenTreeHWIntrinsic* node);
    void genX86BaseIntrinsic(GenTreeHWIntrinsic* node);
    void genSSEIntrinsic(GenTreeHWIntrinsic* node);
    void genSSE2Intrinsic(GenTreeHWIntrinsic* node);
    void genSSE41Intrinsic(GenTreeHWIntrinsic* node);
    void genSSE42Intrinsic(GenTreeHWIntrinsic* node);
    void genAvxOrAvx2Intrinsic(GenTreeHWIntrinsic* node);
    void genAESIntrinsic(GenTreeHWIntrinsic* node);
    void genBMI1OrBMI2Intrinsic(GenTreeHWIntrinsic* node);
    void genFMAIntrinsic(GenTreeHWIntrinsic* node);
    void genLZCNTIntrinsic(GenTreeHWIntrinsic* node);
    void genPCLMULQDQIntrinsic(GenTreeHWIntrinsic* node);
    void genPOPCNTIntrinsic(GenTreeHWIntrinsic* node);
    void genXCNTIntrinsic(GenTreeHWIntrinsic* node, instruction ins);
    template <typename HWIntrinsicSwitchCaseBody>
    void genHWIntrinsicJumpTableFallback(NamedIntrinsic            intrinsic,
                                         regNumber                 nonConstImmReg,
                                         regNumber                 baseReg,
                                         regNumber                 offsReg,
                                         HWIntrinsicSwitchCaseBody emitSwCase);
#endif // TARGET_XARCH
#endif // FEATURE_HW_INTRINSICS

    void SpillNodeReg(GenTree* node, var_types regType, unsigned regIndex);
    X86_ONLY(void SpillST0(GenTree* node);)
    void UnspillNodeReg(GenTree* node, regNumber reg, unsigned regIndex);
    X86_ONLY(void UnspillST0(GenTree* node);)
    void genProduceReg(GenTree* node);
    void DefReg(GenTree* node);
    void DefLclVarReg(GenTreeLclVar* lclVar);
#if FEATURE_ARG_SPLIT
    void DefPutArgSplitRegs(GenTreePutArgSplit* arg);
#endif
    void DefCallRegs(GenTreeCall* call);
#ifndef TARGET_64BIT
    void DefLongRegs(GenTree* node);
#endif
    void SpillLclVarReg(unsigned varNum, var_types type, GenTreeLclVar* lclNode, regNumber regNum);
    void UnspillRegIfNeeded(GenTree* node);
    void UnspillRegCandidateLclVar(GenTreeLclVar* node);
    void UnspillRegIfNeeded(GenTree* node, unsigned regIndex);
    void UnspillRegsIfNeeded(GenTree* node);
    regNumber UseReg(GenTree* node);
    regNumber UseRegCandidateLclVar(GenTreeLclVar* node);
    void UseRegs(GenTree* node);
    regNumber genConsumeReg(GenTree* node);
    regNumber UseReg(GenTree* node, unsigned regIndex);
    void genCopyRegIfNeeded(GenTree* tree, regNumber needReg);

    void genConsumeIfReg(GenTree* tree)
    {
        if (!tree->isContained())
        {
            (void)genConsumeReg(tree);
        }
    }

    void CopyReg(GenTreeCopyOrReload* copy);
    void CopyRegs(GenTreeCopyOrReload* copy);
    regNumber CopyReg(GenTreeCopyOrReload* copy, unsigned regIndex);
    void genConsumeAddress(GenTree* addr);
    void ConsumeStructStore(GenTree* store, ClassLayout* layout, regNumber dstReg, regNumber srcReg, regNumber sizeReg);
    void ConsumeDynBlk(GenTreeDynBlk* store, regNumber dstReg, regNumber srcReg, regNumber sizeReg);
    INDEBUG(bool IsValidContainedLcl(GenTreeLclVarCommon* node);)
    void genConsumeRegs(GenTree* tree);
    void genCodeForShift(GenTreeOp* shift);

#if defined(TARGET_X86) || defined(TARGET_ARM)
    void genCodeForShiftLong(GenTree* tree);
#endif

#ifdef TARGET_XARCH
#ifdef FEATURE_HW_INTRINSICS
    void genConsumeHWIntrinsicOperands(GenTreeHWIntrinsic* tree);
#endif
    void GenIndStoreRMWShift(GenTree* addr, GenTreeOp* shift, GenTree* shiftBy);
    void genCodeForBT(GenTreeOp* bt);
    void EpilogGSCookieCheck(bool tailCallEpilog);
#else
    void EpilogGSCookieCheck();
#endif

    void GenCast(GenTreeCast* cast);
    void GenLclAddr(GenTreeLclAddr* addr);
    void genCodeForIndexAddr(GenTreeIndexAddr* tree);
    void GenIndLoad(GenTreeIndir* load);
    void genCodeForNegNot(GenTreeUnOp* tree);
    void genCodeForBswap(GenTree* tree);
    void GenLoadLclVar(GenTreeLclVar* load);
    void genCodeForLclFld(GenTreeLclFld* tree);
    void GenStoreLclFld(GenTreeLclFld* store);
    void GenStoreLclVar(GenTreeLclVar* store);
    void GenStoreLclRMW(var_types type, unsigned lclNum, unsigned lclOffs, GenTree* src);
#ifndef TARGET_64BIT
    void GenStoreLclVarLong(GenTreeLclVar* store);
#endif
    void GenStoreLclVarMultiReg(GenTreeLclVar* store);
    void GenStoreLclVarMultiRegSIMDMem(GenTreeLclVar* store);
    void GenStoreLclVarMultiRegSIMDReg(GenTreeLclVar* store);
    void genCodeForReturnTrap(GenTreeOp* tree);
    void GenSetCC(GenTreeCC* setcc);
    void GenIndStore(GenTreeStoreInd* tree);
#ifdef TARGET_XARCH
    void genCodeForSwap(GenTreeOp* tree);
#endif
    void genCodeForPhysReg(GenTreePhysReg* tree);
    void genCodeForNullCheck(GenTreeIndir* tree);
    void genCodeForCmpXchg(GenTreeCmpXchg* tree);
    void GenMemoryBarrier(GenTree* barrier);
    void genCodeForInstr(GenTreeInstr* instr);

    void genAlignStackBeforeCall(GenTreePutArgStk* putArgStk);
    void genAlignStackBeforeCall(GenTreeCall* call);
#ifdef TARGET_X86
    void genRemoveAlignmentAfterCall(GenTreeCall* call, unsigned bias);
#endif

#if defined(UNIX_X86_ABI)

    unsigned curNestedAlignment = 0; // Keep track of alignment adjustment required during codegen.
    unsigned maxNestedAlignment = 0; // The maximum amount of alignment adjustment required.

    void SubtractNestedAlignment(unsigned adjustment)
    {
        assert(curNestedAlignment >= adjustment);
        unsigned newNestedAlignment = curNestedAlignment - adjustment;
        if (curNestedAlignment != newNestedAlignment)
        {
            JITDUMP("Adjusting stack nested alignment from %d to %d\n", curNestedAlignment, newNestedAlignment);
        }
        curNestedAlignment = newNestedAlignment;
    }

    void AddNestedAlignment(unsigned adjustment)
    {
        unsigned newNestedAlignment = curNestedAlignment + adjustment;
        if (curNestedAlignment != newNestedAlignment)
        {
            JITDUMP("Adjusting stack nested alignment from %d to %d\n", curNestedAlignment, newNestedAlignment);
        }
        curNestedAlignment = newNestedAlignment;

        if (curNestedAlignment > maxNestedAlignment)
        {
            JITDUMP("Max stack nested alignment changed from %d to %d\n", maxNestedAlignment, curNestedAlignment);
            maxNestedAlignment = curNestedAlignment;
        }
    }

#endif

#ifndef TARGET_X86
    void genPutArgStkFieldList(GenTreePutArgStk* putArg,
                               unsigned          outArgLclNum,
                               unsigned outArgLclOffs DEBUGARG(unsigned outArgLclSize));
#endif

#ifdef TARGET_X86
    void genPreAdjustStackForPutArgStk(unsigned argSize);
    void genPushReg(var_types type, regNumber srcReg);
    void genPutArgStkFieldList(GenTreePutArgStk* putArgStk);
    void genPutStructArgStk(GenTreePutArgStk* putArgStk);
#else
    void genPutStructArgStk(GenTreePutArgStk* putArgStk,
                            unsigned          outArgLclNum,
                            unsigned outArgLclOffs DEBUGARG(unsigned outArgLclSize));
#endif

    void GenDynBlk(GenTreeDynBlk* store);
    void GenStructStore(GenTree* store, StructStoreKind kind, ClassLayout* layout);
    void GenStructStoreUnrollCopyWB(GenTree* store, ClassLayout* layout);
#ifndef TARGET_X86
    void GenStructStoreMemSet(GenTree* store, ClassLayout* layout);
    void GenStructStoreMemCpy(GenTree* store, ClassLayout* layout);
#endif
#ifdef TARGET_XARCH
    void GenStructStoreRepStos(GenTree* store, ClassLayout* layout);
    void GenStructStoreRepMovs(GenTree* store, ClassLayout* layout);
#endif
    void GenStructStoreUnrollInit(GenTree* store, ClassLayout* layout);
    void GenStructStoreUnrollCopy(GenTree* store, ClassLayout* layout);
    void GenStructStoreUnrollRegs(GenTree* store, ClassLayout* layout);
#if defined(UNIX_AMD64_ABI) || defined(TARGET_ARM64)
    void GenStructStoreUnrollRegsWB(GenTreeObj* store);
#endif
    void GenJmpTable(GenTree* node, BasicBlock* switchBlock);
    void genTableBasedSwitch(GenTreeOp* tree);
    void genCodeForArrIndex(GenTreeArrIndex* treeNode);
    void genCodeForArrOffset(GenTreeArrOffs* treeNode);
    instruction genGetInsForOper(genTreeOps oper);
    bool genEmitOptimizedGCWriteBarrier(GCInfo::WriteBarrierForm writeBarrierForm, GenTree* addr, GenTree* data);
    void genCallInstruction(GenTreeCall* call);
    void GenJmp(GenTree* jmp);
    void GenJmpEpilog(BasicBlock* block
#ifdef TARGET_ARMARCH
                      ,
                      CORINFO_METHOD_HANDLE       methHnd,
                      const CORINFO_CONST_LOOKUP& addrInfo
#endif
                      );
    void GenCallFinally(BasicBlock* block);
    void GenJTrue(GenTreeUnOp* jtrue, BasicBlock* block);
    void GenJCC(GenTreeCC* jcc, BasicBlock* block);
#ifdef TARGET_ARM64
    void GenJCmp(GenTreeOp* jcmp, BasicBlock* block);
#endif

#ifdef FEATURE_EH_FUNCLETS
    void genEHCatchRet(BasicBlock* block);
#else
    void genEHFinallyOrFilterRet(BasicBlock* block);
#endif

#ifndef WINDOWS_AMD64_ABI
    void genMultiRegStructReturn(GenTree* src);
#endif

#ifndef TARGET_64BIT
    void genLongReturn(GenTree* src);
#endif

#if defined(TARGET_X86) || defined(TARGET_ARM)
    void genFloatReturn(GenTree* src);
#endif

    void GenRetFilt(GenTree* retfilt, BasicBlock* block);
    void GenReturn(GenTree* ret, BasicBlock* block);

#ifdef TARGET_ARM
    void genStackPointerConstantAdjustment(int32_t spDelta, regNumber regTmp);
    void genStackPointerConstantAdjustmentWithProbe(int32_t spDelta, regNumber regTmp);
    int32_t genStackPointerConstantAdjustmentLoopWithProbe(int32_t spDelta, regNumber regTmp);
#else
    void genStackPointerConstantAdjustment(ssize_t spDelta, regNumber regTmp);
    void genStackPointerConstantAdjustmentWithProbe(ssize_t spDelta, regNumber regTmp);
    target_ssize_t genStackPointerConstantAdjustmentLoopWithProbe(ssize_t spDelta, regNumber regTmp);
#endif

#ifdef TARGET_XARCH
    void genStackPointerDynamicAdjustmentWithProbe(regNumber regSpDelta, regNumber regTmp);
#endif

    void genLclHeap(GenTree* tree);

    GenTreeLclVar* IsRegCandidateLclVar(GenTree* node)
    {
        return node->OperIs(GT_LCL_VAR, GT_STORE_LCL_VAR) && compiler->lvaGetDesc(node->AsLclVar())->IsRegCandidate()
                   ? node->AsLclVar()
                   : nullptr;
    }

#if defined(DEBUG) && defined(TARGET_XARCH)
    void genStackPointerCheck();
#endif

#ifdef DEBUG
    GenTree* lastConsumedNode;
    void AssignUseOrder(BasicBlock* block);
    void AssignUseOrder(GenTree* const operand, int& useNum) const;
    void VerifyUseOrder(GenTree* const node);
#endif

public:
    void inst_Mov(var_types dstType, regNumber dstReg, regNumber srcReg, bool canSkip);

    bool IsLocalMemoryOperand(GenTree* op, unsigned* lclNum, unsigned* lclOffs);

#ifdef TARGET_XARCH
    void inst_RV_SH(instruction ins, emitAttr size, regNumber reg, unsigned val);
    bool IsMemoryOperand(GenTree* op, unsigned* lclNum, unsigned* lclOffs, GenTree** addr, CORINFO_FIELD_HANDLE* field);
    void emitInsRM(instruction ins, emitAttr attr, GenTree* src);
    void emitInsRegRM(instruction ins, emitAttr attr, regNumber reg, GenTree* mem);
    void emitInsCmp(instruction ins, emitAttr attr, GenTree* op1, GenTree* op2);
    void inst_RV_TT_IV(instruction ins, emitAttr attr, regNumber reg1, GenTree* rmOp, int ival);
    void inst_RV_RV_TT(instruction ins, emitAttr size, regNumber targetReg, regNumber op1Reg, GenTree* op2, bool isRMW);
#endif

#ifdef TARGET_ARM
    void inst_RV_IV(instruction ins, regNumber reg, target_ssize_t val, emitAttr size);
    void emitInsLoad(instruction ins, emitAttr attr, regNumber reg, GenTreeIndir* load);
    void emitInsStore(instruction ins, emitAttr attr, regNumber reg, GenTreeStoreInd* store);
    void emitInsIndir(instruction ins, emitAttr attr, regNumber dataReg, GenTreeIndir* indir, int offset);
    regNumber emitInsTernary(instruction ins, emitAttr attr, GenTree* dst, GenTree* src1, GenTree* src2);
#endif

#ifdef TARGET_ARM64
    void inst_RV_IV(instruction ins, regNumber reg, target_ssize_t val, emitAttr size);
    void emitInsLoad(instruction ins, emitAttr attr, regNumber reg, GenTreeIndir* load);
    void emitInsStore(instruction ins, emitAttr attr, regNumber reg, GenTreeStoreInd* store);
    void emitInsIndir(instruction ins, emitAttr attr, regNumber dataReg, GenTreeIndir* indir);
    regNumber emitInsTernary(instruction ins, emitAttr attr, GenTree* dst, GenTree* src1, GenTree* src2);
#endif

    class GenAddrMode
    {
        regNumber m_base;
        regNumber m_index;
        unsigned  m_scale;
        int       m_disp;
        unsigned  m_lclNum;

    public:
        GenAddrMode(unsigned lclNum, unsigned lclOffs)
            : m_base(REG_NA), m_index(REG_NA), m_scale(1), m_disp(lclOffs), m_lclNum(lclNum)
        {
        }

        GenAddrMode(GenTree* tree, CodeGen* codeGen);

        regNumber Base() const
        {
            return m_base;
        }

        regNumber Index() const
        {
            return m_index;
        }

        int Scale() const
        {
            return m_scale;
        }

        int Disp() const
        {
            return m_disp;
        }

        int Disp(unsigned offset) const
        {
            assert(offset <= INT32_MAX);
            assert(m_disp <= INT32_MAX - static_cast<int>(offset));

            return m_disp + offset;
        }

        bool IsLcl() const
        {
            return m_lclNum != BAD_VAR_NUM;
        }

        unsigned LclNum() const
        {
            return m_lclNum;
        }
    };

#ifdef FEATURE_SIMD
    void genStoreSIMD12(GenTree* store, GenTree* value)
    {
        genStoreSIMD12(GenAddrMode(store, this), value,
                       store->AvailableTempRegCount() ? store->GetSingleTempReg() : REG_NA);
    }

    void genStoreSIMD12(const GenAddrMode& dst, GenTree* value, regNumber tmpReg);
#endif

    void inst_R_AM(instruction ins, emitAttr size, regNumber reg, const GenAddrMode& addrMode, unsigned offset = 0);
    void inst_AM_R(instruction ins, emitAttr size, regNumber reg, const GenAddrMode& addrMode, unsigned offset = 0);

    bool isMoveIns(instruction ins);

    instruction ins_Load(var_types srcType, bool aligned = false);
    instruction ins_Store(var_types dstType, bool aligned = false);
    instruction ins_StoreFromSrc(regNumber srcReg, var_types dstType, bool aligned = false);
    instruction ins_Copy(var_types type);
    instruction ins_Copy(regNumber srcReg, var_types dstType);
#ifdef TARGET_XARCH
    instruction ins_Move_Extend(var_types type);
    instruction ins_FloatCompare(var_types type);
#endif

    void instGen_Set_Reg_To_Zero(emitAttr size, regNumber reg);
    void instGen_Set_Reg_To_Addr(regNumber reg,
                                 void* addr DEBUGARG(void* handle = nullptr)
                                     DEBUGARG(HandleKind handleKind = HandleKind::None));
    void instGen_Set_Reg_To_Reloc(regNumber reg,
                                  void* addr DEBUGARG(void* handle = nullptr)
                                      DEBUGARG(HandleKind handleKind = HandleKind::None));
#ifdef TARGET_ARMARCH
#ifdef TARGET_ARM
    void instGen_Set_Reg_To_Imm(regNumber reg, int32_t imm);
#endif
    void instGen_Set_Reg_To_Imm(emitAttr  size,
                                regNumber reg,
                                ssize_t imm DEBUGARG(void* handle = nullptr)
                                    DEBUGARG(HandleKind handleKind = HandleKind::None));
#endif

#ifdef TARGET_XARCH
    instruction MapShiftInsToShiftBy1Ins(instruction ins);
    instruction MapShiftInsToShiftByImmIns(instruction ins);
#endif

    // Maps a GenCondition code to a sequence of conditional jumps or other conditional instructions
    // such as X86's SETcc. A sequence of instructions rather than just a single one is required for
    // certain floating point conditions.
    // For example, X86's UCOMISS sets ZF to indicate equality but it also sets it, together with PF,
    // to indicate an unordered result. So for GenCondition::FEQ we first need to check if PF is 0
    // and then jump if ZF is 1:
    //       JP fallThroughBlock
    //       JE jumpDestBlock
    //   fallThroughBlock:
    //       ...
    //   jumpDestBlock:
    //
    // This is very similar to the way shortcircuit evaluation of bool AND and OR operators works so
    // in order to make the GenConditionDesc mapping tables easier to read, a bool expression-like
    // pattern is used to encode the above:
    //     { EJ_jnp, GT_AND, EJ_je  }
    //     { EJ_jp,  GT_OR,  EJ_jne }
    //
    // For more details check inst_JCC and inst_SETCC functions.
    //
    struct GenConditionDesc
    {
        emitJumpKind jumpKind1;
        genTreeOps   oper;
        emitJumpKind jumpKind2;
        char         padTo4Bytes;

        static const GenConditionDesc& Get(GenCondition condition)
        {
            assert(condition.GetCode() < _countof(map));
            const GenConditionDesc& desc = map[condition.GetCode()];
            assert(desc.jumpKind1 != EJ_NONE);
            assert((desc.oper == GT_NONE) || (desc.oper == GT_AND) || (desc.oper == GT_OR));
            assert((desc.oper == GT_NONE) == (desc.jumpKind2 == EJ_NONE));
            return desc;
        }

    private:
        static const GenConditionDesc map[32];
    };

    void inst_JCC(GenCondition condition, BasicBlock* target);
    void inst_SETCC(GenCondition condition, var_types type, regNumber dstReg);

    INDEBUG(bool IsValidSourceType(var_types instrType, var_types sourceType);)

    bool IsSimdLocalAligned(unsigned lclNum);

    INDEBUG(void DumpDisasmHeader() const;)

#ifdef TARGET_X86
    //  Tracking of region covered by the monitor in synchronized methods
    insGroup* syncStartEmitCookie = nullptr;
    insGroup* syncEndEmitCookie   = nullptr;
#endif

#if !FEATURE_FIXED_OUT_ARGS
    //  Keeps track of how many bytes we've pushed on the processor's stack.
    unsigned genStackLevel = 0;

    unsigned GetCurrentStackLevel() const
    {
        return genStackLevel;
    }
#endif

    insGroup* ehEmitLabel(BasicBlock* block);
    uint32_t ehCodeOffset(BasicBlock* block);

#ifdef TARGET_ARMARCH
    emitLocation unwindLoc;

    void unwindCaptureLocation()
    {
        unwindLoc.CaptureLocation(GetEmitter());
    }

    const emitLocation& unwidGetLocation() const
    {
        return unwindLoc;
    }
#endif

    void unwindBegProlog();
    void unwindEndProlog();
    void unwindBegEpilog();
    void unwindEndEpilog();
#ifdef FEATURE_EH_FUNCLETS
    void unwindReserve();
    void unwindEmit();
#endif

    void unwindAllocStack(unsigned size);

#ifdef TARGET_ARM
    void unwindSetFrameReg(RegNum reg);
    void unwindPushMaskInt(regMaskTP mask);
    void unwindPushMaskFloat(regMaskTP mask);
    void unwindPopMaskInt(regMaskTP mask);
    void unwindPopMaskFloat(regMaskTP mask);
    void unwindBranch16();                    // The epilog terminates with a 16-bit branch (e.g., "bx lr")
    void unwindNop(unsigned codeSizeInBytes); // Generate unwind NOP code
    void unwindPadding(); // Generate a sequence of unwind NOP codes representing instructions between the last
                          // instruction and the current location.
    void unwindPushPopMaskInt(regMaskTP mask, bool useOpsize16);
    void unwindPushPopMaskFloat(regMaskTP mask);
    unsigned unwindGetInstructionSize() const;
#ifdef DEBUG
    void DumpUnwindInfo(bool isHotCode, CodeRange range, const uint8_t* header, uint32_t unwindSize) const;
#endif
#endif

#ifdef TARGET_ARM64
    void unwindNop();
    void unwindPadding(); // Generate a sequence of unwind NOP codes representing instructions between the last
    // instruction and the current location.
    void unwindSetFrameReg(RegNum reg, unsigned offset);
    void unwindSaveReg(RegNum reg, int offset);                             // str reg, [sp, #offset]
    void unwindSaveRegPreindexed(RegNum reg, int offset);                   // str reg, [sp, #offset]!
    void unwindSaveRegPair(RegNum reg1, RegNum reg2, int offset);           // stp reg1, reg2, [sp, #offset]
    void unwindSaveRegPairPreindexed(RegNum reg1, RegNum reg2, int offset); // stp reg1, reg2, [sp, #offset]!
    void unwindSaveNext();                                                  // unwind code: save_next
    void unwindReturn(RegNum reg);                                          // ret lr
#ifdef DEBUG
    void DumpUnwindInfo(bool isHotCode, CodeRange range, const uint8_t* header, uint32_t unwindSize) const;
#endif
#endif

#ifdef TARGET_AMD64
    void unwindBegPrologWindows();
    void unwindSetFrameReg(RegNum reg, unsigned offset);
    void unwindSaveReg(RegNum reg, unsigned offset);
    void unwindPush(RegNum reg);
    void unwindPushWindows(RegNum reg);
    void unwindAllocStackWindows(unsigned size);
    void unwindSetFrameRegWindows(RegNum reg, unsigned offset);
    void unwindSaveRegWindows(RegNum reg, unsigned offset);
#ifdef TARGET_UNIX
    void unwindSaveRegCFI(RegNum reg, unsigned offset);
#endif
#ifdef DEBUG
    void DumpUnwindInfo(bool isHotCode, CodeRange range, const UNWIND_INFO* header) const;
#endif
#endif

#ifdef TARGET_X86
    void unwindSetFrameReg(RegNum reg, unsigned offset);
    void unwindPush(RegNum reg);
    void unwindSaveReg(RegNum reg, unsigned offset);
#endif

#ifdef TARGET_UNIX
    static int16_t mapRegNumToDwarfReg(RegNum reg);

    void unwindPushPopCFI(RegNum reg);
    void unwindBegPrologCFI();
    void unwindPushPopMaskCFI(regMaskTP regMask, bool isFloat);
    void unwindAllocStackCFI(unsigned size);
    void unwindSetFrameRegCFI(RegNum reg, unsigned offset);
    void unwindEmitFuncCFI(FuncInfoDsc* func);

    bool generateCFIUnwindCodes() const
    {
        return compiler->IsTargetAbi(CORINFO_CORERT_ABI);
    }

#ifdef DEBUG
    void DumpCfiInfo(bool isHotCode, CodeRange range, uint32_t count, const CFI_CODE* codes) const;
#endif
#endif

#if defined(TARGET_AMD64) || defined(TARGET_UNIX)
    uint32_t unwindGetCurrentOffset();
#endif

#ifdef FEATURE_EH_FUNCLETS
    void unwindGetFuncHotRange(FuncInfoDsc* func, insGroup** start, insGroup** end);
    CodeRange unwindGetFuncHotRange(FuncInfoDsc* func);
    void unwindGetFuncColdRange(FuncInfoDsc* func, insGroup** start, insGroup** end);
    CodeRange unwindGetFuncColdRange(FuncInfoDsc* func);
    void unwindReserveFunc(FuncInfoDsc* func);
    void unwindEmitFunc(FuncInfoDsc* func);
#endif
#if defined(TARGET_AMD64) || (defined(TARGET_X86) && defined(FEATURE_EH_FUNCLETS))
    void unwindReserveFuncRegion(FuncInfoDsc* func, bool isHotCode);
    void unwindEmitFuncRegion(FuncInfoDsc* func, bool isHotCode);
#endif

#ifdef FEATURE_EH_FUNCLETS
    void eeReserveUnwindInfo(bool isFunclet, bool isHotCode, uint32_t unwindSize);
    void eeAllocUnwindInfo(FuncKind kind, bool isHotCode, CodeRange range, uint32_t unwindSize, void* unwindBlock);
#endif
};

#endif // CODEGEN_H
