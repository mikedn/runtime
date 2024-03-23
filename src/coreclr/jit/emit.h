// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "instr.h"
#include "jitgcinfo.h"

#ifdef TARGET_ARM64
insOpts emitSimdArrangementOpt(emitAttr size, var_types elementType);
#endif

#define FMT_IG "IG%02u"

class CodeGen;
class EmitterBase;
class Encoder;
struct insGroup;

struct CodeRange
{
    uint32_t start;
    uint32_t end;
};

// emitCurCodePos returns a cookie that identifies the current position in the instruction
// group. Due to things like branch shortening, the final size of some instructions is not
// known until instruction encoding, so we return a value containing both the instruction
// index and its estimated offset within the instruction group, allowing us to skip
// recomputing the offset if instruction sizes within the group have not changed.
enum class CodePos : uint32_t
{
    First   = 0,
    Invalid = UINT32_MAX
};

class emitLocation
{
    insGroup* ig;
    CodePos   codePos = CodePos::First;

public:
    emitLocation(insGroup* ig = nullptr) : ig(ig)
    {
    }

    void SetLocation(insGroup* label)
    {
        assert(label != nullptr);

        ig      = label;
        codePos = CodePos::First;
    }

    void CaptureLocation(const EmitterBase* emit);

    insGroup* GetIG() const
    {
        return ig;
    }

    CodePos GetCodePos() const
    {
        return codePos;
    }

    bool Valid() const
    {
        return ig != nullptr;
    }

    unsigned GetInsNum() const;
    uint32_t GetCodeOffset() const;

    INDEBUG(void Print(const char* suffix = nullptr) const;)
};

#define IGF_BASIC_BLOCK 0x0001
#define IGF_PROLOG 0x0002        // this group belongs to a prolog
#define IGF_EPILOG 0x0004        // this group belongs to a epilog
#define IGF_NOGCINTERRUPT 0x0008 // this IG is is a no-interrupt region (prolog, epilog, etc.)
#define IGF_UPD_ISZ 0x0010       // some instruction sizes updated
#define IGF_PLACEHOLDER 0x0020   // this is a placeholder group, to be filled in later
#define IGF_EXTEND 0x0040        // this block is conceptually an extension of the previous block and the
                                 // emitter should continue to track GC info as if there was no new block.
#define IGF_LOOP_ALIGN 0x0080    // this group contains alignment instruction(s) at the end; the next IG
                                 // is the head of a loop that needs alignment.
#define IGF_COLD 0x0100

struct insGroup
{
    insGroup* igNext;
#if FEATURE_LOOP_ALIGN
    insGroup* igLoopBackEdge; // "last" back-edge that branches back to an aligned loop head.
#endif
    union {
        uint8_t*    igData;   // addr of instruction descriptors
        BasicBlock* igPhData; // when igFlags & IGF_PLACEHOLDER
    };

    unsigned igNum;  // for ordering (and display) purposes
    unsigned igOffs; // offset of this group within method
#if !FEATURE_FIXED_OUT_ARGS
    unsigned igStkLvl; // stack level on entry
#endif
#ifdef FEATURE_EH_FUNCLETS
    uint16_t igFuncIdx; // Which function/funclet does this belong to? (Index into CodeGen::compFuncInfos array.)
#else
    static constexpr uint16_t igFuncIdx = 0;
#endif
    uint16_t igSize;   // # of bytes of code in this group
    uint16_t igFlags;  // see IGF_xxx below
    uint8_t  igInsCnt; // # of instructions  in this group

    static_assert_no_msg(REG_INT_COUNT <= 32);

    VARSET_TP gcLcls;
    uint32_t  refRegs;
    uint32_t  byrefRegs;

#if defined(DEBUG) || defined(LATE_DISASM)
    BasicBlock::weight_t igWeight;    // the block weight used for this insGroup
    double               igPerfScore; // The PerfScore for this insGroup
#endif
#ifdef DEBUG
    uint16_t                  tryIndex;
    jitstd::list<BasicBlock*> igBlocks; // All the blocks that generated code into this insGroup.
#endif

#ifdef DEBUG
    unsigned GetId() const
    {
        return igNum;
    }
#endif

    uint32_t GetCodeOffset() const
    {
        assert(igNum != 0);
        return igOffs;
    }

    uint32_t GetCodeOffset(CodePos codePos) const;
    uint32_t FindInsOffset(unsigned insNum) const;

    bool IsDefined() const
    {
        return igNum != 0;
    }

    uint16_t GetFuncletIndex() const
    {
        return igFuncIdx;
    }

    VARSET_TP GetGCLcls() const
    {
        assert((igFlags & IGF_EXTEND) == 0);

        return gcLcls;
    }

    regMaskTP GetRefRegs() const
    {
        assert((igFlags & IGF_EXTEND) == 0);

        return refRegs;
    }

    regMaskTP GetByrefRegs() const
    {
        assert((igFlags & IGF_EXTEND) == 0);

        return byrefRegs;
    }

    bool isLoopAlign() const
    {
        return (igFlags & IGF_LOOP_ALIGN) != 0;
    }

    bool IsMainProlog() const
    {
        return ((igFlags & IGF_PROLOG) != 0) && (igFuncIdx == 0);
    }

    bool IsMainEpilog() const
    {
        return ((igFlags & IGF_EPILOG) != 0) && (igFuncIdx == 0);
    }

    bool IsFuncletProlog() const
    {
        return ((igFlags & IGF_PROLOG) != 0) && (igFuncIdx != 0);
    }

    bool IsFuncletEpilog() const
    {
        return ((igFlags & IGF_EPILOG) != 0) && (igFuncIdx != 0);
    }

    bool IsFuncletPrologOrEpilog() const
    {
        return ((igFlags & (IGF_PROLOG | IGF_EPILOG)) != 0) && (igFuncIdx != 0);
    }

    bool IsProlog() const
    {
        return (igFlags & IGF_PROLOG) != 0;
    }

    bool IsEpilog() const
    {
        return (igFlags & IGF_EPILOG) != 0;
    }

    bool IsPrologOrEpilog() const
    {
        return (igFlags & (IGF_PROLOG | IGF_EPILOG)) != 0;
    }

    bool IsBasicBlock() const
    {
        return (igFlags & IGF_BASIC_BLOCK) != 0;
    }

    bool IsNoGC() const
    {
        return (igFlags & IGF_NOGCINTERRUPT) != 0;
    }

    bool IsExtension() const
    {
        return (igFlags & IGF_EXTEND) != 0;
    }

    bool IsCold() const
    {
        return (igFlags & IGF_COLD) != 0;
    }
};

enum insFormat : unsigned
{
#define IF_DEF(en, ...) IF_##en,
#include "emitfmts.h"
    IF_COUNT
};

struct ConstData
{
    // Alignments greater than 32 requires VM changes (see ICorJitInfo::allocMem)
    const static uint32_t MinAlign = 4;
    const static uint32_t MaxAlign = 32;

    enum Kind : uint8_t
    {
        Const,
        LabelAddr,
        LabelRel32
    };

    uint32_t offset;
    uint32_t size;
    Kind     kind;
    INDEBUG(var_types type;)

    void* GetData() const
    {
        assert(size != 0);
        return const_cast<uint8_t*>(reinterpret_cast<const uint8_t*>(this)) + sizeof(ConstData);
    }
};

struct StackAddrMode
{
    int varNum;
    int varOffs;
};

#if defined(TARGET_XARCH)
class X86Encoder;
#elif defined(TARGET_ARM)
class ArmEncoder;
#elif defined(TARGET_ARM64)
class Arm64Encoder;
#else
#error Unsupported or unset target architecture
#endif

class AsmPrinter;

class EmitterBase
{
    friend class emitLocation;
    friend class GCInfo;
    friend class AsmPrinter;
    friend class Encoder;
    friend struct insGroup;

protected:
    Compiler* emitComp;
    GCInfo    gcInfo;
    CodeGen*  codeGen;
#ifdef LATE_DISASM
    class DisAssembler* disasm;
#endif

public:
    EmitterBase(Compiler* compiler, CodeGen* codeGen, ICorJitInfo* jitInfo);

    GCInfo& GetGCInfo()
    {
        return gcInfo;
    }

    insGroup* GetProlog() const
    {
        return emitIGfirst;
    }

protected:
    static bool InDifferentRegions(insGroup* ig1, insGroup* ig2);

#ifdef LATE_DISASM
    void disSetMethod(size_t addr, CORINFO_METHOD_HANDLE methHnd);
#endif

public:
    void emitBegFN();
    void emitComputeCodeSizes();
    void emitEndCodeGen();

#ifdef LATE_DISASM
    void Disassemble();
#endif

    void     emitBegProlog();
    unsigned emitGetCurrentPrologCodeSize();
    void     MarkMainPrologNoGCEnd();
    void     EndMainProlog();

#ifdef FEATURE_EH_FUNCLETS
    void ReserveFuncletProlog(BasicBlock* block);
#endif
    void ReserveEpilog(BasicBlock* block);
    void emitGeneratePrologEpilog();

#ifndef JIT32_GCENCODER
    template <typename Callback>
    void EnumerateNoGCInsGroups(Callback callback) const
    {
        for (insGroup* ig = emitIGfirst; ig != nullptr; ig = ig->igNext)
        {
            if (ig->IsNoGC())
            {
                callback(ig->igOffs, ig->igSize DEBUGARG(ig->GetFuncletIndex()));
            }
        }
    }
#else
    unsigned GetMaxStackDepth()
    {
        return emitMaxStackDepth;
    }
#endif // JIT32_GCENCODER

    CodePos emitCurCodePos() const;
    bool IsCurrentLocation(const emitLocation& loc) const;
    bool IsPreviousLocation(const emitLocation& loc) const;
    INDEBUG(const char* emitOffsetToLabel(unsigned offs);)

public:
    ConstData* CreateBlockLabelTable(BasicBlock** blocks, unsigned count, bool relative);
    ConstData* CreateTempLabelTable(insGroup*** labels, unsigned count, bool relative);

    ConstData* GetFloatConst(double value, var_types type);
    ConstData* GetConst(const void* data, uint32_t size, uint32_t align DEBUGARG(var_types type));

#if DISPLAY_SIZES
    uint32_t GetRoDataSize() const
    {
        return roData.size;
    }
#endif

protected:
    struct DataSection
    {
        DataSection* next;
        ConstData    data;
    };

    DataSection* CreateLabelTable(unsigned count, bool relative);
    DataSection* CreateConst(const void* data, uint32_t size, uint32_t align DEBUGARG(var_types type));
    DataSection* CreateConstSection(const void* data, uint32_t size, uint32_t align DEBUGARG(var_types type));

    void* emitGetMem(size_t sz);

    enum opSize : unsigned
    {
        OPSZ1  = 0,
        OPSZ2  = 1,
        OPSZ4  = 2,
        OPSZ8  = 3,
        OPSZ16 = 4,
        OPSZ32 = 5
    };

    bool IsMainProlog(const insGroup* ig) const
    {
        // Currently, we only allow one IG for the prolog
        return ig == emitIGfirst;
    }

    void emitRecomputeIGoffsets();
    void emitDispCommentForHandle(void* handle, HandleKind kind);

#ifdef TARGET_XARCH
    struct emitAddrMode
    {
#ifdef TARGET_AMD64
        // x64 has 32 registers currently but APX has 64.
        // We need to be able to store REG_NA too so we'll need an extra bit.
        static_assert_no_msg(REG_NA <= 127);
        static constexpr unsigned RegBits  = 7;
        static constexpr unsigned DispBits = 16;
        static constexpr unsigned DispMin  = -32767;
        static constexpr unsigned DispMax  = 32767;
        INDEBUG(static constexpr int32_t LargeDispMarker = -32768;)
#else
        static_assert_no_msg(REG_NA <= 31);
        static constexpr unsigned RegBits  = 5;
        static constexpr unsigned DispBits = 16;
        static constexpr unsigned DispMin  = -32767;
        static constexpr unsigned DispMax  = 32767;
        INDEBUG(static constexpr int32_t LargeDispMarker = -32768;)
#endif

        RegNum   base : RegBits;
        RegNum   index : RegBits;
        uint32_t scale : 2;
#ifdef TARGET_X86
        unsigned spare : 4; // Give these to disp? Though 16 bits should be enough for everybody...
#endif
        int32_t disp : DispBits;

        static bool IsLargeDisp(ssize_t disp)
        {
            return (disp < DispMin) || (DispMax < disp);
        }
    };
#endif // TARGET_XARCH

#ifdef DEBUG // This information is used in DEBUG builds to display the method name for call instructions
    struct instrDescDebugInfo
    {
        unsigned idNum;
        uint16_t idSize; // size of the instruction descriptor
        int      varNum  = INT_MIN;
        int      varOffs = 0;
#ifdef TARGET_XARCH
        HandleKind dispHandleKind = HandleKind::None;
#endif
        HandleKind        idHandleKind = HandleKind::None;
        void*             idHandle     = nullptr;
        CORINFO_SIG_INFO* idCallSig    = nullptr; // Used to report native call site signatures to the EE

        instrDescDebugInfo(unsigned num, unsigned size) : idNum(num), idSize(static_cast<uint16_t>(size))
        {
            assert(size <= UINT16_MAX);
        }
    };
#endif // DEBUG

public:
#ifdef TARGET_ARM
    enum insSize : unsigned
    {
        ISZ_NONE,
        ISZ_16BIT,
        ISZ_32BIT,
        ISZ_48BIT // pseudo-instruction for conditional branch with imm24 range,
                  // encoded as IT of condition followed by an unconditional branch
    };
#endif

#ifdef TARGET_XARCH
#define MAX_ENCODED_SIZE 15
#endif

    struct instrDesc;

    struct instrDescSmall
    {
        friend struct instrDesc;

#ifdef TARGET_AMD64
        static_assert_no_msg(INS_COUNT <= 1024);
        static_assert_no_msg(IF_COUNT <= 128);
        static_assert_no_msg(ACTUAL_REG_COUNT <= 64); // 32 currently but APX has 64
        static constexpr unsigned RegBits      = 6;
        static constexpr unsigned SmallImmBits = 16;

    private:
        instruction _idIns : 10;       // Instruction opcode
        insFormat   _idInsFmt : 7;     // Instruction format
        unsigned    _idOpSize : 3;     // Operation size (log 2)
        GCtype      _idGCref : 2;      // GC type of the first destination register
        unsigned    _idCnsReloc : 1;   // Immediate is relocatable
        unsigned    _idDspReloc : 1;   // Address mode displacement is relocatable
        unsigned    _idSmallDsc : 1;   // this is instrDescSmall
        unsigned    _idLargeCall : 1;  // this is instrDescCGCA
        unsigned    _idLargeCns : 1;   // this is instrDescCns/instrDescCnsAmd
        unsigned    _idLargeDsp : 1;   // this is instrDescAmd
        unsigned    _idCodeSize : 4;   // Encoded instruction size
        RegNum      _idReg1 : RegBits; // First register, also holds the GC ref reg mask for calls
        RegNum      _idReg2 : RegBits; // Second register, also holds the GC byref reg mask for calls
        unsigned    _idNoGC : 1;       // Helper call that does not need GC information
        unsigned    _idSpare : 3;      // Reserved for EVEX stuff?
        unsigned    _idSmallCns : SmallImmBits;
#endif // TARGET_XARCH

#ifdef TARGET_X86
        static_assert_no_msg(INS_COUNT <= 1024);
        static_assert_no_msg(IF_COUNT <= 128);
        static_assert_no_msg(ACTUAL_REG_COUNT == 16);
        static constexpr unsigned RegBits      = 4;
        static constexpr unsigned SmallImmBits = 16;

    private:
        instruction _idIns : 10;       // Instruction opcode
        insFormat   _idInsFmt : 7;     // Instruction format
        unsigned    _idOpSize : 3;     // Operation size (log 2)
        GCtype      _idGCref : 2;      // GC type of the first destination register
        unsigned    _idCnsReloc : 1;   // Immediate is relocatable
        unsigned    _idDspReloc : 1;   // Address mode displacement is relocatable
        unsigned    _idSmallDsc : 1;   // this is instrDescSmall
        unsigned    _idLargeCall : 1;  // this is instrDescCGCA
        unsigned    _idLargeCns : 1;   // this is instrDescCns/instrDescCnsAmd
        unsigned    _idLargeDsp : 1;   // this is instrDescAmd
        unsigned    _idCodeSize : 4;   // Encoded instruction size
        RegNum      _idReg1 : RegBits; // First register, also holds the GC ref reg mask for calls
        RegNum      _idReg2 : RegBits; // Second register, also holds the GC byref reg mask for calls
        unsigned    _idNoGC : 1;       // Helper call that does not need GC information
        unsigned    _idFSPrefix : 1;   // FS segment prefix
        unsigned    _idSpare : 6;      // EVEX stuff?
        unsigned    _idSmallCns : SmallImmBits;
#endif // TARGET_XARCH

#ifdef TARGET_ARM64
        static_assert_no_msg(INS_COUNT <= 512);
        static_assert_no_msg(IF_COUNT <= 128);
        // REG_SP is included the in actual reg count but we don't need that here.
        static_assert_no_msg(ACTUAL_REG_COUNT - 1 <= 64);
        static constexpr unsigned RegBits      = 6;
        static constexpr unsigned SmallImmBits = 16;

    private:
        instruction _idIns : 9;        // Instruction opcode
        insFormat   _idInsFmt : 7;     // Instruction format
        insOpts     _idInsOpt : 6;     // Instruction options
        unsigned    _idOpSize : 3;     // Operation size (log 2)
        GCtype      _idGCref : 2;      // GC type of the first destination register
        unsigned    _idSmallDsc : 1;   // this is instrDescSmall
        unsigned    _idLargeCall : 1;  // this is instrDescCGCA
        unsigned    _idLargeCns : 1;   // this is instrDescCns
        unsigned    _idCnsReloc : 1;   // Immediate is relocatable
        unsigned    _idLclVar : 1;     // Local load/store
        RegNum      _idReg1 : RegBits; // First register, also holds the GC ref reg mask for calls
        RegNum      _idReg2 : RegBits; // Second register, also holds the GC byref reg mask for calls
        unsigned    _idNoGC : 1;       // Helper call that does not need GC information
        unsigned    _idSpare : 3;      // Give these to small imm?
        unsigned    _idSmallCns : SmallImmBits;
#endif // TARGET_ARM64

#ifdef TARGET_ARM
        static_assert_no_msg(INS_COUNT <= 256);
        static_assert_no_msg(IF_COUNT <= 128);
        static_assert_no_msg(ACTUAL_REG_COUNT <= 64);
        static constexpr unsigned RegBits      = 6;
        static constexpr unsigned SmallImmBits = 16;

    private:
        instruction _idIns : 8;        // Instruction opcode
        insFormat   _idInsFmt : 7;     // Instruction format
        insOpts     _idInsOpt : 3;     // Instruction options
        insFlags    _idInsFlags : 1;   // Instruction sets flags
        unsigned    _idOpSize : 2;     // Operation size (log 2)
        GCtype      _idGCref : 2;      // GC type of the first destination register
        unsigned    _idSmallDsc : 1;   // this is instrDescSmall
        unsigned    _idLargeCall : 1;  // this is instrDescCGCA
        unsigned    _idLargeCns : 1;   // this is instrDescCns
        unsigned    _idSpare1 : 1;     // Give this to small imm?
        unsigned    _idNoGC : 1;       // Helper call that does not need GC information
        insSize     _idInsSize : 2;    // Encoded instruction size: 16, 32 or 48 bits
        unsigned    _idLclVar : 1;     // Local load/store
        unsigned    _idCnsReloc : 1;   // Immediate is relocatable
        RegNum      _idReg1 : RegBits; // First register, also holds the GC ref reg mask for calls
        RegNum      _idReg2 : RegBits; // Second register, also holds the GC byref reg mask for calls
        unsigned    _idSpare : 4;      // Give these to small imm?
        unsigned    _idSmallCns : SmallImmBits;
#endif // TARGET_ARM

        INDEBUG(instrDescDebugInfo* _idDebugOnlyInfo;)

    public:
        instruction idIns() const
        {
            return _idIns;
        }

        void idIns(instruction ins)
        {
            assert((ins != INS_invalid) && (ins < INS_COUNT));
            _idIns = ins;
        }

        insFormat idInsFmt() const
        {
            return _idInsFmt;
        }

        void idInsFmt(insFormat insFmt)
        {
#ifdef TARGET_ARM64
            noway_assert(insFmt != IF_NONE); // Only the x86 emitter uses IF_NONE, it is invalid for ARM64 (and ARM32)
#endif
            assert(insFmt < IF_COUNT);
            _idInsFmt = insFmt;
        }

        bool idIsSmallDsc() const
        {
            return _idSmallDsc;
        }

        void idSetIsSmallDsc()
        {
            _idSmallDsc = true;
        }

#ifdef TARGET_XARCH
        unsigned idCodeSize() const
        {
            return _idCodeSize;
        }

        void idCodeSize(unsigned sz)
        {
            assert(sz <= 15); // Intel decoder limit.
            _idCodeSize = sz;
            assert(sz == _idCodeSize);
        }

        bool idIsDspReloc() const
        {
            return _idDspReloc;
        }

        void idSetIsDspReloc(bool val = true)
        {
            _idDspReloc = val;
        }

        bool idIsReloc()
        {
            return idIsDspReloc() || idIsCnsReloc();
        }

        bool idIsLargeDsp() const
        {
            return _idLargeDsp;
        }

        void idSetIsLargeDsp()
        {
            _idLargeDsp = true;
        }

        void idSetIsSmallDsp()
        {
            _idLargeDsp = false;
        }

#ifdef WINDOWS_X86_ABI
        void SetHasFSPrefix()
        {
            _idFSPrefix = true;
        }
#endif

        bool HasFSPrefix() const
        {
#ifdef WINDOWS_X86_ABI
            return _idFSPrefix;
#else
            return false;
#endif
        }
#endif // TARGET_XARCH

#ifdef TARGET_ARM64
        unsigned idCodeSize() const
        {
            switch (_idInsFmt)
            {
                case IF_LARGEADR: // adrp + add
                case IF_LARGELDC: // adrp + ldr
                case IF_LARGEJMP: // b<cond> + b<uncond>
                    return 8;
                case IF_GC_REG:
                case IF_NOP_JMP:
                    return 0;
                default:
                    return 4;
            }
        }
#endif // TARGET_ARM64

#ifdef TARGET_ARM
        unsigned idCodeSize() const
        {
            switch (_idInsSize)
            {
                case ISZ_NONE:
                    return 0;
                case ISZ_16BIT:
                    return 2;
                case ISZ_32BIT:
                    return 4;
                default:
                    assert(_idInsSize == ISZ_48BIT);
                    return 6;
            }
        }

        insSize idInsSize() const
        {
            return _idInsSize;
        }

        void idInsSize(insSize isz)
        {
            _idInsSize = isz;
            assert(isz == _idInsSize);
        }

        insFlags idInsFlags() const
        {
            return _idInsFlags;
        }

        void idInsFlags(insFlags sf)
        {
            _idInsFlags = sf;
            assert(sf == _idInsFlags);
        }
#endif // TARGET_ARM

#ifdef TARGET_ARMARCH
        insOpts idInsOpt() const
        {
            return static_cast<insOpts>(_idInsOpt);
        }

        void idInsOpt(insOpts opt)
        {
            _idInsOpt = opt;
            assert(opt == _idInsOpt);
        }

        bool idIsLclVar() const
        {
            return _idLclVar;
        }
#endif // TARGET_ARMARCH

        emitAttr idOpSize() const
        {
            return static_cast<emitAttr>(1 << _idOpSize);
        }

        void idOpSize(emitAttr size)
        {
            assert(size == 1 || size == 2 || size == 4 || size == 8 || size == 16 || size == 32);
            _idOpSize = BitPosition(size);
        }

        GCtype idGCref() const
        {
            return _idGCref;
        }

        void idGCref(GCtype gctype)
        {
            _idGCref = gctype;
        }

        regNumber idReg1() const
        {
            return _idReg1;
        }

        void idReg1(regNumber reg)
        {
            _idReg1 = reg;
            assert(reg == _idReg1);
        }

        regNumber idReg2() const
        {
            return _idReg2;
        }

        void idReg2(regNumber reg)
        {
            _idReg2 = reg;
            assert(reg == _idReg2);
        }

        static bool fitsInSmallCns(ssize_t val)
        {
            return (val >= 0) && (val < (static_cast<ssize_t>(1) << SmallImmBits));
        }

        bool idIsLargeCns() const
        {
            return _idLargeCns;
        }

        void idSetIsLargeCns()
        {
            _idLargeCns = true;
        }

        bool idIsLargeCall() const
        {
            return _idLargeCall;
        }

        void idSetIsLargeCall()
        {
            _idLargeCall = true;
        }

        // Only call instructions that call helper functions may be marked as "IsNoGC", indicating
        // that a thread executing such a call cannot be stopped for GC. Thus, in partially-interruptible
        // code, it is not necessary to generate GC info for a call so labeled.
        bool idIsNoGC() const
        {
            return _idNoGC;
        }

        void idSetIsNoGC(bool val)
        {
            _idNoGC = val;
        }

        bool idIsCnsReloc() const
        {
            return _idCnsReloc;
        }

        void idSetIsCnsReloc(bool val = true)
        {
            _idCnsReloc = val;
        }

        size_t GetDescSize() const;

        unsigned idSmallCns() const
        {
            return _idSmallCns;
        }

        void idSmallCns(size_t value)
        {
            assert(fitsInSmallCns(value));
            _idSmallCns = value;
        }

        void SetVarAddr(INDEBUG(StackAddrMode s))
        {
#ifdef TARGET_ARMARCH
            _idLclVar = true;
#endif
#ifdef DEBUG
            _idDebugOnlyInfo->varNum  = s.varNum;
            _idDebugOnlyInfo->varOffs = s.varOffs;
#endif
        }

#ifdef DEBUG
        bool InstrHasNoCode() const
        {
            return (_idInsFmt == IF_GC_REG)
#ifdef TARGET_XARCH
                   || (_idIns == INS_align) || (_idCodeSize == 0)
#endif
#ifdef TARGET_ARM
                   || (_idInsSize == ISZ_NONE)
#endif
#ifdef TARGET_ARM64
                   || (_idInsFmt == IF_NOP_JMP)
#endif
                ;
        }

        instrDescDebugInfo* idDebugOnlyInfo() const
        {
            return _idDebugOnlyInfo;
        }

        void idDebugOnlyInfo(instrDescDebugInfo* info)
        {
            _idDebugOnlyInfo = info;
        }
#endif // DEBUG
    };

    static_assert(sizeof(instrDescSmall) == 8 INDEBUG(+sizeof(void*)), "Bad instrDescSmall size");

    struct instrDescJmp;

    struct instrDesc : public instrDescSmall
    {
        friend struct instrDescJmp;

    private:
        union idAddrUnion {
            // TODO-Cleanup: We should really add a DEBUG-only tag to this union so we can add asserts
            // about reading what we think is here, to avoid unexpected corruption issues.

            uintptr_t label;
            uintptr_t addr;
#ifdef TARGET_XARCH
            ConstData* data;
#endif

#ifdef TARGET_ARM
            struct
            {
                unsigned isTrackedGCSlotStore : 1;
                unsigned isGCArgStore : 1;
                int      lclOffset : 30;
            };

            struct
            {
                regNumber _idReg3 : RegBits;
                regNumber _idReg4 : RegBits;
            };
#endif

#ifdef TARGET_X86
            emitAddrMode iiaAddrMode;
            regNumber    _idReg3 : RegBits;

            struct
            {
                unsigned isTrackedGCSlotStore : 1;
                unsigned isEbpBased : 1;
                int      lclOffset : 30;
            };
#endif

#ifdef TARGET_AMD64
            emitAddrMode iiaAddrMode;
            regNumber    _idReg3 : RegBits;

            struct
            {
                unsigned isTrackedGCSlotStore : 1;
                unsigned isGCArgStore : 1;
                unsigned isEbpBased : 1;
                int      lclOffset;
            };
#endif

#ifdef TARGET_ARM64
            struct
            {
                regNumber _idReg3 : RegBits;
                regNumber _idReg4 : RegBits;
                unsigned  _idReg3Scaled : 1;
                GCtype    _idGCref2 : 2;
                unsigned  isTrackedGCSlotStore : 1;
                unsigned  isGCArgStore : 1;
                int       lclOffset;
            };
#endif
        } _idAddrUnion;

        static_assert_no_msg(sizeof(idAddrUnion) == sizeof(void*));

#ifdef DEBUG
        bool HasAddr() const
        {
#ifdef TARGET_ARM
            return ((_idInsFmt == IF_T2_N3) && ((_idIns == INS_movt) || (_idIns == INS_movw))) ||
                   ((_idInsFmt == IF_T2_J3) && ((_idIns == INS_b) || (_idIns == INS_bl)));
#endif
#ifdef TARGET_ARM64
            return ((_idInsFmt == IF_BI_0C) && ((_idIns == INS_bl) || (_idIns == INS_b_tail))) ||
                   ((_idInsFmt == IF_DI_1E) && (_idIns == INS_adrp)) ||
                   ((_idInsFmt == IF_DI_2A) && (_idIns == INS_add));
#endif
#ifdef TARGET_XARCH
            return (_idInsFmt == IF_METHOD) || (_idInsFmt == IF_METHPTR);
#endif
        }
#endif // DEBUG

    public:
        void* GetAddr() const
        {
            assert(HasAddr());
            return reinterpret_cast<void*>(idAddr()->addr);
        }

        void SetAddr(void* addr)
        {
#ifndef TARGET_ARM
            assert(addr != nullptr);
#endif
            assert(HasAddr());
            idAddr()->addr = reinterpret_cast<uintptr_t>(addr);
        }

        const idAddrUnion* idAddr() const
        {
            assert(!idIsSmallDsc());
            return &_idAddrUnion;
        }

        idAddrUnion* idAddr()
        {
            assert(!idIsSmallDsc());
            return &_idAddrUnion;
        }

#ifdef TARGET_XARCH
        ssize_t GetImm() const;
        ssize_t GetMemDisp() const;
        ssize_t GetAmDisp() const;
        ssize_t GetCallDisp() const;

        ConstData* GetConstData() const
        {
            assert((IF_MRD <= _idInsFmt) && (_idInsFmt <= IF_MWR_RRD_CNS));
            return idAddr()->data;
        }

        void SetConstData(ConstData* data)
        {
            assert((IF_MRD <= _idInsFmt) && (_idInsFmt <= IF_MWR_RRD_CNS));
            idAddr()->data = data;
        }

        regNumber idReg3() const
        {
            return idAddr()->_idReg3;
        }

        void idReg3(regNumber reg)
        {
            idAddr()->_idReg3 = reg;
            assert(reg == idAddr()->_idReg3);
        }

        regNumber idReg4() const
        {
            assert(_idInsFmt == IF_RWR_RRD_ARD_RRD || _idInsFmt == IF_RWR_RRD_SRD_RRD ||
                   _idInsFmt == IF_RWR_RRD_MRD_RRD || _idInsFmt == IF_RWR_RRD_RRD_RRD);
            return static_cast<regNumber>(REG_XMM0 + (_idSmallCns >> 4));
        }
#endif // TARGET_XARCH

#ifdef TARGET_ARMARCH
        regNumber idReg3() const
        {
            return idAddr()->_idReg3;
        }

        void idReg3(regNumber reg)
        {
            idAddr()->_idReg3 = reg;
            assert(reg == idAddr()->_idReg3);
        }

        regNumber idReg4() const
        {
            return idAddr()->_idReg4;
        }

        void idReg4(regNumber reg)
        {
            idAddr()->_idReg4 = reg;
            assert(reg == idAddr()->_idReg4);
        }
#endif // TARGET_ARMARCH

#ifdef TARGET_ARM
        size_t  emitGetInstrDescSize() const;
        size_t  emitGetInstrDescSizeSC() const;
        int32_t emitGetInsSC() const;
#endif

#ifdef TARGET_ARM64
        GCtype idGCrefReg2() const
        {
            return static_cast<GCtype>(idAddr()->_idGCref2);
        }

        void idGCrefReg2(GCtype gctype)
        {
            idAddr()->_idGCref2 = gctype;
        }

        bool idReg3Scaled() const
        {
            return idAddr()->_idReg3Scaled;
        }

        void idReg3Scaled(bool val)
        {
            idAddr()->_idReg3Scaled = val;
        }

        int64_t emitGetInsSC() const;
        size_t  emitGetInstrDescSize() const;
#endif // TARGET_ARM64
    };

#ifdef TARGET_ARM
    struct instrDescCns : instrDesc
    {
        int32_t idcCnsVal;
    };
#endif

#ifdef TARGET_ARM64
    struct instrDescCns : instrDesc
    {
        int64_t idcCnsVal;
    };
#endif

#ifdef TARGET_XARCH
    struct instrDescCns : instrDesc
    {
        // Normally immediate values should be target_ssize_t but for relocatable immediates
        // we need to store a host pointer here, that will get converted to the actual 32 bit
        // immediate during/after encoding.
        ssize_t idcCnsVal;
    };

    struct instrDescAmd : instrDesc
    {
        ssize_t idaAmdVal;
    };

    struct instrDescCnsAmd : instrDescCns
    {
        ssize_t idacAmdVal;
    };
#endif // TARGET_XARCH

#if FEATURE_LOOP_ALIGN
    struct instrDescAlign : instrDesc
    {
        instrDescAlign* idaNext; // next align in the group/method
        insGroup*       idaIG;   // containing group
    };
#endif

    struct instrDescJmp : instrDesc
    {
    private:
        enum : uintptr_t
        {
            LabelTag = 0,
#if defined(TARGET_ARM64) || defined(TARGET_X86)
            ConstDataTag = 1,
#endif
            InstrCountTag = 2,
            TagMask       = 3,
            TagSize       = 2
        };

    public:
        instrDescJmp* idjNext; // next jump in the group/method
        insGroup*     idjIG;   // containing group
        unsigned      idjOffs; // The byte offset within IG of the jump instruction.

        const idAddrUnion* idAddr() const = delete;
        idAddrUnion*       idAddr()       = delete;

        bool HasLabel() const
        {
            return (_idAddrUnion.label & TagMask) == LabelTag;
        }

        insGroup* GetLabel() const
        {
            assert(HasLabel());
            return reinterpret_cast<insGroup*>(_idAddrUnion.label);
        }

        void SetLabel(insGroup* label)
        {
            _idAddrUnion.label = reinterpret_cast<uintptr_t>(label);
        }

#if defined(TARGET_ARM64) || defined(TARGET_X86)
        bool HasConstData() const
        {
            return (_idAddrUnion.label & TagMask) == ConstDataTag;
        }

        ConstData* GetConstData() const
        {
            assert(HasConstData());
            return reinterpret_cast<ConstData*>(_idAddrUnion.label & ~TagMask);
        }

        void SetConstData(ConstData* data)
        {
            _idAddrUnion.label = reinterpret_cast<uintptr_t>(data) | ConstDataTag;
        }
#endif

        bool HasInstrCount() const
        {
            return (_idAddrUnion.label & TagMask) == InstrCountTag;
        }

        int GetInstrCount() const
        {
            assert(HasInstrCount());
            return static_cast<int>(static_cast<intptr_t>(_idAddrUnion.label) >> TagSize);
        }

        void SetInstrCount(int count)
        {
            assert(abs(count) < 10);
            _idAddrUnion.label = (static_cast<intptr_t>(count) << TagSize) | InstrCountTag;
        }

#ifdef TARGET_ARM
        void SetShortJump();
        void SetMediumJump();
#endif

#ifdef TARGET_ARM64
        void SetShortJump();
#endif
    };

    struct instrDescCGCA : instrDesc
    {
        VARSET_TP idcGCvars;
#ifdef TARGET_XARCH
        int32_t idcDisp;
#endif
        regMaskTP idcGcrefRegs;
        regMaskTP idcByrefRegs;
#ifdef TARGET_X86
        int idcArgCnt;
#endif
    };

#if defined(DEBUG) || defined(LATE_DISASM)
#define PERFSCORE_THROUGHPUT_ILLEGAL -1024.0f

#define PERFSCORE_THROUGHPUT_ZERO 0.0f // Only used for pseudo-instructions that don't generate code

#define PERFSCORE_THROUGHPUT_6X (1.0f / 6.0f)
#define PERFSCORE_THROUGHPUT_5X 0.20f
#define PERFSCORE_THROUGHPUT_4X 0.25f
#define PERFSCORE_THROUGHPUT_3X (1.0f / 3.0f)
#define PERFSCORE_THROUGHPUT_2X 0.5f

#define PERFSCORE_THROUGHPUT_1C 1.0f

#define PERFSCORE_THROUGHPUT_2C 2.0f
#define PERFSCORE_THROUGHPUT_3C 3.0f
#define PERFSCORE_THROUGHPUT_4C 4.0f
#define PERFSCORE_THROUGHPUT_5C 5.0f
#define PERFSCORE_THROUGHPUT_6C 6.0f
#define PERFSCORE_THROUGHPUT_7C 7.0f
#define PERFSCORE_THROUGHPUT_8C 8.0f
#define PERFSCORE_THROUGHPUT_9C 9.0f
#define PERFSCORE_THROUGHPUT_10C 10.0f
#define PERFSCORE_THROUGHPUT_13C 13.0f
#define PERFSCORE_THROUGHPUT_19C 19.0f
#define PERFSCORE_THROUGHPUT_25C 25.0f
#define PERFSCORE_THROUGHPUT_33C 33.0f
#define PERFSCORE_THROUGHPUT_52C 52.0f
#define PERFSCORE_THROUGHPUT_57C 57.0f

#define PERFSCORE_LATENCY_ILLEGAL -1024.0f

#define PERFSCORE_LATENCY_ZERO 0.0f
#define PERFSCORE_LATENCY_1C 1.0f
#define PERFSCORE_LATENCY_2C 2.0f
#define PERFSCORE_LATENCY_3C 3.0f
#define PERFSCORE_LATENCY_4C 4.0f
#define PERFSCORE_LATENCY_5C 5.0f
#define PERFSCORE_LATENCY_6C 6.0f
#define PERFSCORE_LATENCY_7C 7.0f
#define PERFSCORE_LATENCY_8C 8.0f
#define PERFSCORE_LATENCY_9C 9.0f
#define PERFSCORE_LATENCY_10C 10.0f
#define PERFSCORE_LATENCY_11C 11.0f
#define PERFSCORE_LATENCY_12C 12.0f
#define PERFSCORE_LATENCY_13C 13.0f
#define PERFSCORE_LATENCY_15C 15.0f
#define PERFSCORE_LATENCY_16C 16.0f
#define PERFSCORE_LATENCY_18C 18.0f
#define PERFSCORE_LATENCY_20C 20.0f
#define PERFSCORE_LATENCY_22C 22.0f
#define PERFSCORE_LATENCY_23C 23.0f
#define PERFSCORE_LATENCY_26C 26.0f
#define PERFSCORE_LATENCY_62C 62.0f
#define PERFSCORE_LATENCY_69C 69.0f
#define PERFSCORE_LATENCY_400C 400.0f // Intel microcode issue with these instructions

#define PERFSCORE_LATENCY_BRANCH_DIRECT 1.0f   // cost of an unconditional branch
#define PERFSCORE_LATENCY_BRANCH_COND 2.0f     // includes cost of a possible misprediction
#define PERFSCORE_LATENCY_BRANCH_INDIRECT 2.0f // includes cost of a possible misprediction

#if defined(TARGET_XARCH)

// a read,write or modify from stack location, possible def to use latency from L0 cache
#define PERFSCORE_LATENCY_RD_STACK PERFSCORE_LATENCY_2C
#define PERFSCORE_LATENCY_WR_STACK PERFSCORE_LATENCY_2C
#define PERFSCORE_LATENCY_RD_WR_STACK PERFSCORE_LATENCY_5C

// a read, write or modify from constant location, possible def to use latency from L0 cache
#define PERFSCORE_LATENCY_RD_CONST_ADDR PERFSCORE_LATENCY_2C
#define PERFSCORE_LATENCY_WR_CONST_ADDR PERFSCORE_LATENCY_2C
#define PERFSCORE_LATENCY_RD_WR_CONST_ADDR PERFSCORE_LATENCY_5C

// a read, write or modify from memory location, possible def to use latency from L0 or L1 cache
// plus an extra cost  (of 1.0) for a increased chance  of a cache miss
#define PERFSCORE_LATENCY_RD_GENERAL PERFSCORE_LATENCY_3C
#define PERFSCORE_LATENCY_WR_GENERAL PERFSCORE_LATENCY_3C
#define PERFSCORE_LATENCY_RD_WR_GENERAL PERFSCORE_LATENCY_6C

#elif defined(TARGET_ARM64) || defined(TARGET_ARM)

// a read,write or modify from stack location, possible def to use latency from L0 cache
#define PERFSCORE_LATENCY_RD_STACK PERFSCORE_LATENCY_3C
#define PERFSCORE_LATENCY_WR_STACK PERFSCORE_LATENCY_1C
#define PERFSCORE_LATENCY_RD_WR_STACK PERFSCORE_LATENCY_3C

// a read, write or modify from constant location, possible def to use latency from L0 cache
#define PERFSCORE_LATENCY_RD_CONST_ADDR PERFSCORE_LATENCY_3C
#define PERFSCORE_LATENCY_WR_CONST_ADDR PERFSCORE_LATENCY_1C
#define PERFSCORE_LATENCY_RD_WR_CONST_ADDR PERFSCORE_LATENCY_3C

// a read, write or modify from memory location, possible def to use latency from L0 or L1 cache
// plus an extra cost  (of 1.0) for a increased chance  of a cache miss
#define PERFSCORE_LATENCY_RD_GENERAL PERFSCORE_LATENCY_4C
#define PERFSCORE_LATENCY_WR_GENERAL PERFSCORE_LATENCY_1C
#define PERFSCORE_LATENCY_RD_WR_GENERAL PERFSCORE_LATENCY_4C

#endif // TARGET_XXX

#define PERFSCORE_MEMORY_NONE 0
#define PERFSCORE_MEMORY_READ 1
#define PERFSCORE_MEMORY_WRITE 2
#define PERFSCORE_MEMORY_READ_WRITE 3

protected:
    BasicBlock::weight_t getCurrentBlockWeight();
#endif // defined(DEBUG) || defined(LATE_DISASM)

    void dispIns(instrDesc* id);

    void appendToCurIG(instrDesc* id);

    static size_t emitGetInstrDescSizeSC(const instrDesc* id);

#ifdef DEBUG
    static const char* emitIfName(unsigned f);

    unsigned emitInsCount = 0;

    void emitDispIG(insGroup* ig, bool dispInstr);
    void emitDispIGlist(bool dispInstr);
    void emitDispInsAddr(BYTE* code);
    void emitDispInsOffs(unsigned offs, bool doffs);
#endif // !DEBUG

    struct Placeholder
    {
        Placeholder* next = nullptr;
        insGroup*    ig;

        Placeholder(insGroup* ig) : ig(ig)
        {
        }
    };

    Placeholder* firstPlaceholder = nullptr;
    Placeholder* lastPlaceholder  = nullptr;

    BasicBlock* BeginPrologEpilog(insGroup* ig);
    void EndPrologEpilog();

#ifdef JIT32_GCENCODER
    struct Epilog
    {
        Epilog*      next = nullptr;
        emitLocation startLoc;
    };

    Epilog*      firstEpilog = nullptr;
    Epilog*      lastEpilog  = nullptr;
    emitLocation epilogExitLoc;
    unsigned     epilogCount      = 0;
    unsigned     epilogCommonSize = 0;
    unsigned     epilogExitSize   = 0;

    void BeginGCEpilog();
    void EndGCEpilog();

public:
    void MarkGCEpilogStart() const;
    void MarkGCEpilogExit();

    bool HasSingleEpilogAtEnd();

    unsigned GetEpilogCount() const
    {
        return epilogCount;
    }

    template <typename Callback>
    void EnumerateEpilogs(Callback callback)
    {
        for (Epilog* e = firstEpilog; e != nullptr; e = e->next)
        {
            assert(e->startLoc.GetIG()->IsMainEpilog());

            callback(e->startLoc.GetCodeOffset());
        }
    }
#endif // JIT32_GCENCODER

    static unsigned emitFindInsNum(const insGroup* ig, const instrDesc* instr);

    // If we have started issuing instructions from the list of instrDesc, this is set
    INDEBUG(bool emitIssuing = false;)

    uint8_t* emitCodeBlock     = nullptr; // Hot code block
    uint8_t* emitColdCodeBlock = nullptr; // Cold code block

#ifdef PSEUDORANDOM_NOP_INSERTION
    bool emitInInstrumentation = false;
#endif

    insGroup* emitCurIG;
    insGroup* emitCurLabel = nullptr;

protected:
    insGroup* emitIGfirst = nullptr;
    insGroup* emitIGlast  = nullptr;

    instrDescJmp* emitJumpList = nullptr; // list of local jumps in method
    instrDescJmp* emitJumpLast = nullptr; // last of local jumps in method

public:
    insGroup* GetCurrentInsGroup() const
    {
        return emitCurIG;
    }

    INDEBUG(static bool IsCodeAligned(unsigned offset);)

    void ShortenBranches();

#if FEATURE_LOOP_ALIGN
    void emitLoopAlignment();
    bool emitEndsWithAlignInstr(); // Validate if newLabel is appropriate
    void emitSetLoopBackEdge(insGroup* dstIG);
    void emitLoopAlignAdjustments(); // Predict if loop alignment is needed and make appropriate adjustments

protected:
    instrDescAlign* emitCurIGAlignList = nullptr; // list of align instructions in current IG
    unsigned        emitLastLoopStart  = 0;       // Start IG of last inner loop
    unsigned        emitLastLoopEnd    = 0;       // End IG of last inner loop
    instrDescAlign* emitAlignList      = nullptr; // list of local align instructions in method
    instrDescAlign* emitAlignLast      = nullptr; // last align instruction in method
    unsigned getLoopSize(insGroup* igLoopHeader,
                         unsigned maxLoopSize DEBUG_ARG(bool isAlignAdjusted)); // Get the smallest loop size
    unsigned emitCalculatePaddingForLoopAlignment(insGroup* ig, size_t offset DEBUG_ARG(bool isAlignAdjusted));
#endif

protected:
#ifdef DEBUG
    void VerifyCallFinally(insGroup* label) const;
    void VerifyCatchRet(insGroup* label) const;
#endif

    // Are we generating IGF_NOGCINTERRUPT insGroups (for prologs, epilogs, etc.)
    bool emitNoGCIG = false;
    // If we generate an instruction, and not another instruction group, force create a new emitAdd
    // instruction group.
    bool emitForceNewIG = false;

    BYTE* emitCurIGfreeNext;           // next available byte in buffer
    BYTE* emitCurIGfreeEndp;           // one byte past the last available byte in buffer
    BYTE* emitCurIGfreeBase = nullptr; // first byte address

    instrDescJmp* emitCurIGjmpList = nullptr; // list of jumps   in current IG

    unsigned emitCurIGinsCnt;       // # of collected instr's in buffer
    unsigned emitCurIGsize;         // estimated code size of current group in bytes
    unsigned emitCurCodeOffset = 0; // current code offset within group

    CodePos mainPrologNoGCEndCodePos;

    unsigned emitTotalCodeSize    = 0; // bytes of code in entire method
    unsigned emitTotalHotCodeSize = 0;

    insGroup* emitFirstColdIG = nullptr; // first cold instruction group

#ifdef DEBUG
    unsigned instrCount      = 0;
    bool     compCodeGenDone = false;
#endif
#if defined(DEBUG) || defined(LATE_DISASM)
    double perfScore = 0.0;
#endif

public:
    uint8_t* GetHotCodeAddr() const
    {
        return emitCodeBlock;
    }

    unsigned GetHotCodeSize() const
    {
        assert(emitTotalHotCodeSize != 0);
        return emitTotalHotCodeSize;
    }

    uint8_t* GetColdCodeAddr() const
    {
        return emitColdCodeBlock;
    }

    unsigned GetColdCodeSize() const
    {
        assert(emitTotalCodeSize != 0);
        return emitTotalCodeSize - emitTotalHotCodeSize;
    }

    unsigned GetCodeSize() const
    {
        assert(emitTotalCodeSize != 0);
        return emitTotalCodeSize;
    }

    unsigned GetMainPrologNoGCSize() const
    {
        return GetProlog()->GetCodeOffset(mainPrologNoGCEndCodePos);
    }

#ifdef JIT32_GCENCODER
    unsigned GetEpilogSize() const
    {
        // Currently, in methods with multiple epilogs, all epilogs must have the same
        // size. epilogSize is the size of just one of these epilogs, not the cumulative
        // size of all of the method's epilogs.
        return epilogCommonSize + epilogExitSize;
    }
#endif

#ifdef DEBUG
    unsigned GetInstrCount() const
    {
        return instrCount;
    }
#endif

#if defined(DEBUG) || defined(LATE_DISASM)
    double GetPerfScore() const
    {
        return perfScore;
    }
#endif

protected:
    VARSET_TP emitEmptyGCrefVars = VarSetOps::UninitVal();

    static void EncodeCallGCRegs(regMaskTP regs, instrDesc* id);
    static unsigned DecodeCallGCRegs(instrDesc* id);

#ifdef PSEUDORANDOM_NOP_INSERTION
    // random nop insertion to break up nop sleds
    unsigned emitNextNop;
    bool     emitRandomNops;

    void emitEnableRandomNops()
    {
        emitRandomNops = true;
    }

    void emitDisableRandomNops()
    {
        emitRandomNops = false;
    }
#endif // PSEUDORANDOM_NOP_INSERTION

    insGroup* emitAllocIG(unsigned num);

    void emitNewIG();
    void emitAppendIG(insGroup* ig);
    void emitGenIG(insGroup* ig);
    void emitExtendIG();
    void emitFinishIG(bool extend = false);
    void MoveJumpInstrList(insGroup* ig);
#if FEATURE_LOOP_ALIGN
    void MoveAlignInstrList(insGroup* ig);
#endif

#ifndef JIT32_GCENCODER
public:
    void emitDisableGC();
    void emitEnableGC();

protected:
#endif

    bool emitCurIGnonEmpty() const
    {
        return (emitCurIG != nullptr) && (emitCurIGfreeNext > emitCurIGfreeBase);
    }

    instrDesc* emitLastIns      = nullptr;
    insGroup*  emitLastInsLabel = nullptr;

    instrDesc* GetLastInsInCurrentBlock() const
    {
        return (emitLastIns != nullptr) && (emitLastInsLabel == emitCurLabel) ? emitLastIns : nullptr;
    }

#ifdef DEBUG
    void emitCheckIGoffsets();
    void emitPrintLabel(insGroup* ig);
    const char* emitLabelString(insGroup* ig) const;
    void PrintAlignmentBoundary(size_t           instrAddr,
                                size_t           instrEndAddr,
                                const instrDesc* instr,
                                const instrDesc* nextInstr);
#endif

public:
    insGroup* CreateBlockLabel(BasicBlock* block, unsigned funcletIndex);
    insGroup* CreateTempLabel();
    insGroup* DefineTempLabel();
    void DefineTempLabel(insGroup* label);
    void DefineInlineTempLabel(insGroup* label);
    void DefineBlockLabel(insGroup* label);
    void SetLabelGCLiveness(insGroup* label);
    insGroup* DefineInlineTempLabel();

#ifdef TARGET_ARMARCH
protected:
    void emitGetInstrDescs(insGroup* ig, instrDesc** id, int* insCnt);
    bool emitGetLocationInfo(const emitLocation& emitLoc, insGroup** pig, instrDesc** pid, int* pinsRemaining = NULL);
    bool GetNextInstr(insGroup*& ig, instrDesc*& id, int& insRemaining);
    typedef void (*WalkInstrCallback)(instrDesc* id, void* context);
    void WalkInstr(const emitLocation& locFrom, WalkInstrCallback callback, void* context);

public:
    void emitUnwindNopPadding(const emitLocation& loc);
#endif // TARGET_ARMARCH

protected:
    int emitNextRandomNop();

    instrDescSmall* emitAllocAnyInstr(unsigned sz, bool updateLastIns);

public:
    static emitJumpKind emitReverseJumpKind(emitJumpKind jumpKind);

protected:
#ifdef DEBUG
    void emitInsSanityCheck(instrDesc* id);
#endif

#ifdef JIT32_GCENCODER
    unsigned emitCntStackDepth;     // 0 in prolog/epilog, One DWORD elsewhere
    unsigned emitMaxStackDepth = 0; // actual computed max. stack depth
    unsigned emitCurStackLvl   = 0; // amount of bytes pushed on stack
#endif

#ifdef DEBUG
public:
    const char* emitGetFrameReg();

protected:
#endif

    struct RoData
    {
        DataSection* first = nullptr;
        DataSection* last  = nullptr;
        uint32_t     size  = 0;
        uint32_t     align = ConstData::MinAlign;

        DataSection* Find(const void* data, uint32_t size, uint32_t align) const;
    };

    RoData roData;
};

#if defined(TARGET_XARCH)
#include "emitxarch.h"
#elif defined(TARGET_ARM)
#include "emitarm.h"
#elif defined(TARGET_ARM64)
#include "emitarm64.h"
#else
#error Unsupported or unset target architecture
#endif

using Emitter = ArchEmitter;
using emitter = ArchEmitter;

class Encoder
{
protected:
    using instrDescSmall = Emitter::instrDescSmall;
    using instrDesc      = Emitter::instrDesc;
    using instrDescJmp   = Emitter::instrDescJmp;
    using instrDescCGCA  = Emitter::instrDescCGCA;
#ifdef TARGET_ARMARCH
    using instrDescCns = Emitter::instrDescCns;
#endif
#if FEATURE_LOOP_ALIGN
    using instrDescAlign = Emitter::instrDescAlign;
#endif
    using RoData      = EmitterBase::RoData;
    using DataSection = EmitterBase::DataSection;

    Compiler*    emitComp;
    ICorJitInfo* jitInfo;
    CodeGen*     codeGen;
    ArchEmitter& emit;
    GCInfo&      gcInfo;
    RoData&      roData;
    unsigned     totalCodeSize;
    unsigned     hotCodeSize;
    insGroup*    firstColdIG;
    uint8_t*     emitCodeBlock;
    uint8_t*     emitColdCodeBlock;
    uint8_t*     emitConsBlock;
    size_t       writeableOffset = 0;
    insGroup*&   currentIG;
#ifdef JIT32_GCENCODER
    unsigned& stackLevel;
#endif

    Encoder(ArchEmitter* emit)
        : emitComp(emit->emitComp)
        , jitInfo(emit->emitComp->info.compCompHnd)
        , codeGen(emit->codeGen)
        , emit(*emit)
        , gcInfo(emit->gcInfo)
        , roData(emit->roData)
        , totalCodeSize(emit->emitTotalCodeSize)
        , hotCodeSize(emit->emitTotalHotCodeSize)
        , firstColdIG(emit->emitFirstColdIG)
        , currentIG(emit->emitCurIG)
#ifdef JIT32_GCENCODER
        , stackLevel(emit->emitCurStackLvl)
#endif
    {
        assert(currentIG == nullptr);
    }

public:
    void emitEndCodeGen();

protected:
    unsigned GetColdCodeSize() const
    {
        return totalCodeSize - hotCodeSize;
    }

    size_t emitIssue1Instr(insGroup* ig, instrDesc* id, uint8_t** dp);
    virtual size_t emitOutputInstr(insGroup* ig, instrDesc* id, uint8_t** dp) = 0;
    void OutputRoData(uint8_t* dst);

    void emitGCregLiveUpd(GCtype gcType, RegNum reg, uint8_t* addr);
    void emitGCregDeadUpd(RegNum reg, uint8_t* addr);
#ifdef FEATURE_EH_FUNCLETS
    void emitGCregDeadAll(uint8_t* addr);
#endif
    void emitGCvarLiveUpd(int slotOffs, GCtype gcType, uint8_t* addr DEBUGARG(int varNum));
#if FEATURE_FIXED_OUT_ARGS
    void emitGCargLiveUpd(int offs, GCtype gcType, uint8_t* addr DEBUGARG(int varNum));
#endif
    size_t emitRecordGCCall(instrDesc* id, uint8_t* callAddr, uint8_t* callEndAddr);
    void emitRecordCallSite(unsigned instrOffset, CORINFO_SIG_INFO* callSig, CORINFO_METHOD_HANDLE methodHandle);
    void emitRecordRelocation(void* location, void* target, uint16_t relocType, int32_t addlDelta = 0);

    unsigned emitCurCodeOffs(uint8_t* dst) const;
    uint8_t* emitOffsetToPtr(unsigned offset) const;
    uint8_t* emitDataOffsetToPtr(unsigned offset) const;

#ifdef JIT32_GCENCODER
    void emitStackPush(unsigned codeOffs, GCtype type);
    void emitStackPushN(unsigned codeOffs, unsigned count);
    void emitStackPop(unsigned codeOffs, unsigned count);
    void emitStackPopArgs(unsigned codeOffs, unsigned count);
    void emitStackKillArgs(unsigned codeOffs, unsigned count);
#endif

#ifdef DEBUG
    void PrintRoData() const;
    void GetGCDeltaDumpHeader(char* buffer, size_t count);
    bool emitJumpCrossHotColdBoundary(size_t srcOffset, size_t dstOffset) const;
#endif

#if defined(DEBUG) || defined(LATE_DISASM)
    // TODO-MIKE-Cleanup: These should be double. Bozos defined them as float, even if they wanted
    // double precision computations. Now of course that changing these now to double resuls in
    // perf scores diffs, because 0.10f isn't the same as 0.10.
    static constexpr float PERFSCORE_CODESIZE_COST_HOT  = 0.10f;
    static constexpr float PERFSCORE_CODESIZE_COST_COLD = 0.01f;

    struct insExecutionCharacteristics
    {
        float    insThroughput;
        float    insLatency;
        unsigned insMemoryAccessKind;
    };

#if defined(TARGET_XARCH)
    static insFormat getMemoryOperation(instrDesc* id);
#elif defined(TARGET_ARM64)
    void getMemoryOperation(instrDesc* id, unsigned* pMemAccessKind, bool* pIsLocalAccess);
#endif
    float insEvaluateExecutionCost(instrDesc* id);
    insExecutionCharacteristics getInsExecutionCharacteristics(instrDesc* id);
    void perfScoreUnhandledInstruction(instrDesc* id, insExecutionCharacteristics* result);
#endif
};

#ifdef DEBUG
class AsmPrinter
{
protected:
    using instrDescSmall = Emitter::instrDescSmall;
    using instrDesc      = Emitter::instrDesc;
    using instrDescJmp   = Emitter::instrDescJmp;
    using instrDescCGCA  = Emitter::instrDescCGCA;
#ifdef TARGET_ARMARCH
    using instrDescCns = Emitter::instrDescCns;
#endif
#if FEATURE_LOOP_ALIGN
    using instrDescAlign = Emitter::instrDescAlign;
#endif

    Compiler*    compiler;
    CodeGen*     codeGen;
    ArchEmitter& emit;

    AsmPrinter(ArchEmitter& emit) : compiler(emit.emitComp), codeGen(emit.codeGen), emit(emit)
    {
    }
};

#if defined(TARGET_XARCH)
class X86AsmPrinter;
#elif defined(TARGET_ARM)
class ArmAsmPrinter;
#elif defined(TARGET_ARM64)
class Arm64AsmPrinter;
#else
#error Unsupported or unset target architecture
#endif
#endif // DEBUG
