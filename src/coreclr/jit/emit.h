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
class emitter;
struct insGroup;

class emitLocation
{
public:
    emitLocation() : ig(nullptr), codePos(0)
    {
    }

    emitLocation(insGroup* _ig) : ig(_ig), codePos(0)
    {
    }

    emitLocation(void* emitCookie) : ig((insGroup*)emitCookie), codePos(0)
    {
    }

    // A constructor for code that needs to call it explicitly.
    void Init()
    {
        *this = emitLocation();
    }

    void CaptureLocation(emitter* emit);

    bool IsCurrentLocation(emitter* emit) const;

    // This function is highly suspect, since it presumes knowledge of the codePos "cookie",
    // and doesn't look at the 'ig' pointer.
    bool IsOffsetZero() const
    {
        return (codePos == 0);
    }

    UNATIVE_OFFSET CodeOffset(emitter* emit) const;

    insGroup* GetIG() const
    {
        return ig;
    }

    int GetInsNum() const;

    bool operator!=(const emitLocation& other) const
    {
        return (ig != other.ig) || (codePos != other.codePos);
    }

    bool operator==(const emitLocation& other) const
    {
        return !(*this != other);
    }

    bool Valid() const
    {
        // Things we could validate:
        //   1. the instruction group pointer is non-nullptr.
        //   2. 'ig' is a legal pointer to an instruction group.
        //   3. 'codePos' is a legal offset into 'ig'.
        // Currently, we just do #1.
        // #2 and #3 should only be done in DEBUG, if they are implemented.

        if (ig == nullptr)
        {
            return false;
        }

        return true;
    }

    UNATIVE_OFFSET GetFuncletPrologOffset(emitter* emit) const;

    bool IsPreviousInsNum(emitter* emit) const;

#ifdef DEBUG
    void Print(LONG compMethodID) const;
#endif // DEBUG

private:
    insGroup* ig;      // the instruction group
    unsigned  codePos; // the code position within the IG (see emitCurOffset())
};

typedef void (*emitSplitCallbackType)(void* context, emitLocation* emitLoc);

enum insGroupPlaceholderType
{
    IGPT_EPILOG,
#ifdef FEATURE_EH_FUNCLETS
    IGPT_FUNCLET_PROLOG,
    IGPT_FUNCLET_EPILOG,
#endif
};

struct insPlaceholderGroupData
{
    insGroup*   igPhNext = nullptr;
    BasicBlock* igPhBB;

    insPlaceholderGroupData(BasicBlock* block) : igPhBB(block)
    {
    }
};

#define IGF_BASIC_BLOCK 0x0001
#define IGF_FUNCLET_PROLOG 0x0002 // this group belongs to a funclet prolog
#define IGF_FUNCLET_EPILOG 0x0004 // this group belongs to a funclet epilog.
#define IGF_EPILOG 0x0008         // this group belongs to a main function epilog
#define IGF_NOGCINTERRUPT 0x0010  // this IG is is a no-interrupt region (prolog, epilog, etc.)
#define IGF_UPD_ISZ 0x0020        // some instruction sizes updated
#define IGF_PLACEHOLDER 0x0040    // this is a placeholder group, to be filled in later
#define IGF_EXTEND 0x0080         // this block is conceptually an extension of the previous block and the
                                  // emitter should continue to track GC info as if there was no new block.
#define IGF_LOOP_ALIGN 0x0100     // this group contains alignment instruction(s) at the end; the next IG
                                  // is the head of a loop that needs alignment.

// Mask of IGF_* flags that should be propagated to new blocks when they are created.
// This allows prologs and epilogs to be any number of IGs, but still be
// automatically marked properly.
#ifndef FEATURE_EH_FUNCLETS
#define IGF_PROPAGATE_MASK IGF_EPILOG
#elif defined(DEBUG)
#define IGF_PROPAGATE_MASK (IGF_EPILOG | IGF_FUNCLET_PROLOG | IGF_FUNCLET_EPILOG)
#else
#define IGF_PROPAGATE_MASK (IGF_EPILOG | IGF_FUNCLET_PROLOG)
#endif

// For AMD64 the maximum prolog/epilog size supported on the OS is 256 bytes
// Since it is incorrect for us to be jumping across funclet prolog/epilogs
// we will use the following estimate as the maximum placeholder size.
#define MAX_PLACEHOLDER_IG_SIZE 256

struct insGroup
{
    insGroup* igNext;
#if FEATURE_LOOP_ALIGN
    insGroup* igLoopBackEdge; // "last" back-edge that branches back to an aligned loop head.
#endif
    union {
        uint8_t*                 igData;   // addr of instruction descriptors
        insPlaceholderGroupData* igPhData; // when igFlags & IGF_PLACEHOLDER
    };

    unsigned igNum;     // for ordering (and display) purposes
    unsigned igOffs;    // offset of this group within method
    unsigned igFuncIdx; // Which function/funclet does this belong to? (Index into Compiler::compFuncInfos array.)
#if !FEATURE_FIXED_OUT_ARGS
    unsigned igStkLvl; // stack level on entry
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
    BasicBlock*               lastGeneratedBlock; // The last block that generated code into this insGroup.
    jitstd::list<BasicBlock*> igBlocks;           // All the blocks that generated code into this insGroup.
#endif

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

    bool IsEpilog() const
    {
        return (igFlags & IGF_EPILOG) != 0;
    }

    bool IsFuncletPrologOrEpilog() const
    {
#ifdef FEATURE_EH_FUNCLETS
        return (igFlags & (IGF_FUNCLET_PROLOG | IGF_FUNCLET_EPILOG)) != 0;
#else
        return false;
#endif
    }

    bool IsBasicBlock() const
    {
        return (igFlags & IGF_BASIC_BLOCK) != 0;
    }

    bool IsNoGC() const
    {
        return (igFlags & IGF_NOGCINTERRUPT) != 0;
    }

#ifdef DEBUG
    bool IsExtension() const
    {
        return (igFlags & IGF_EXTEND) != 0;
    }
#endif
};

enum insFormat : unsigned
{
#define IF_DEF(en, ...) IF_##en,
#include "emitfmts.h"
    IF_COUNT
};

class AsmPrinter;

class emitter
{
    friend class emitLocation;
    friend class GCInfo;
    friend class AsmPrinter;

    Compiler*    emitComp;
    GCInfo       gcInfo;
    CodeGen*     codeGen;
    ICorJitInfo* emitCmpHandle;

public:
    emitter(Compiler* compiler, CodeGen* codeGen, ICorJitInfo* jitInfo);

    GCInfo& GetGCInfo()
    {
        return gcInfo;
    }

    BasicBlock* GetCurrentBlock() const;

private:
    bool InDifferentRegions(BasicBlock* block1, BasicBlock* block2) const;
    bool IsColdBlock(BasicBlock* block) const;

    /************************************************************************/
    /*       Overall emitter control (including startup and shutdown)       */
    /************************************************************************/

public:
    void     emitBegFN();
    void     emitComputeCodeSizes();
    unsigned emitEndCodeGen(unsigned* prologSize,
#ifdef JIT32_GCENCODER
                            unsigned* epilogSize,
#endif
                            void** codeAddr,
                            void** coldCodeAddr,
                            void** consAddr DEBUGARG(unsigned* instrCount));

    /************************************************************************/
    /*                      Method prolog and epilog                        */
    /************************************************************************/

    void     emitBegProlog();
    unsigned emitGetPrologOffsetEstimate();
    void     emitMarkPrologEnd();
    void     emitEndProlog();
    void emitCreatePlaceholderIG(insGroupPlaceholderType kind, BasicBlock* block);
    void emitGeneratePrologEpilog();

#ifndef JIT32_GCENCODER
    template <typename Callback>
    void EnumerateNoGCInsGroups(Callback callback)
    {
        for (insGroup* ig = emitIGfirst; ig != nullptr; ig = ig->igNext)
        {
            if (ig->IsNoGC())
            {
                callback(ig->igFuncIdx, ig->igOffs, ig->igSize);
            }
        }
    }
#else
    unsigned GetMaxStackDepth()
    {
        return emitMaxStackDepth;
    }

    unsigned emitGetEpilogCnt()
    {
        return emitEpilogCnt;
    }

    template <typename Callback>
    void EnumerateEpilogs(Callback callback)
    {
        for (EpilogList* el = emitEpilogList; el != nullptr; el = el->elNext)
        {
            assert((el->elLoc.GetIG()->igFlags & IGF_EPILOG) != 0);

            callback(el->elLoc.CodeOffset(this));
        }
    }
#endif // JIT32_GCENCODER

    /************************************************************************/
    /*           Record a code position and later convert it to offset      */
    /************************************************************************/

    unsigned emitCurOffset();
    uint32_t emitCodeOffset(insGroup* ig);
    uint32_t emitCodeOffset(insGroup* ig, unsigned codeOffs);
    INDEBUG(const char* emitOffsetToLabel(unsigned offs);)

    /************************************************************************/
    /*                   Emit initialized data sections                     */
    /************************************************************************/

private:
    enum idAddrUnionTag
    {
        iaut_ALIGNED_POINTER = 0x0,
        iaut_DATA_OFFSET     = 0x1,
        iaut_INST_COUNT      = 0x2,
        iaut_UNUSED_TAG      = 0x3,

        iaut_MASK  = 0x3,
        iaut_SHIFT = 2
    };

public:
    static CORINFO_FIELD_HANDLE MakeRoDataField(unsigned offset)
    {
        assert(offset < 0x40000000);
        uintptr_t bits = static_cast<uintptr_t>((offset << iaut_SHIFT) | iaut_DATA_OFFSET);
        return reinterpret_cast<CORINFO_FIELD_HANDLE>(bits);
    }

    static bool IsRoDataField(CORINFO_FIELD_HANDLE field)
    {
        uintptr_t bits = reinterpret_cast<uintptr_t>(field);
        return (bits <= UINT_MAX) && ((bits & iaut_MASK) == iaut_DATA_OFFSET);
    }

    static int GetRoDataOffset(CORINFO_FIELD_HANDLE field)
    {
        if (IsRoDataField(field))
        {
            uintptr_t bits = reinterpret_cast<uintptr_t>(field);
            return static_cast<int>(bits >> iaut_SHIFT);
        }
        else
        {
            return -1;
        }
    }

    UNATIVE_OFFSET emitDataGenBeg(unsigned size, unsigned alignment, var_types dataType);
    UNATIVE_OFFSET emitBBTableDataGenBeg(unsigned numEntries, bool relativeAddr);
    void emitDataGenData(unsigned offs, const void* data, UNATIVE_OFFSET size);
    void emitDataGenData(unsigned offs, BasicBlock* label);
    void           emitDataGenEnd();
    UNATIVE_OFFSET emitDataGenFind(const void* cnsAddr, unsigned size, unsigned alignment, var_types dataType);
    UNATIVE_OFFSET emitDataConst(const void* cnsAddr, unsigned cnsSize, unsigned cnsAlign, var_types dataType);

    /*****************************************************************************
     *
     *  Return the current size of the specified data section.
     */

    UNATIVE_OFFSET emitDataSize()
    {
        return emitConsDsc.dsdOffs;
    }

private:
    static const UNATIVE_OFFSET INVALID_UNATIVE_OFFSET = (UNATIVE_OFFSET)-1;

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

    insGroup* GetProlog() const
    {
        return emitIGfirst;
    }

    bool emitIGisInProlog(const insGroup* ig) const
    {
        // Currently, we only allow one IG for the prolog
        return ig == emitIGfirst;
    }

    void emitRecomputeIGoffsets();

    void emitDispCommentForHandle(void* handle, HandleKind kind);

/************************************************************************/
/*          The following describes a single instruction                */
/************************************************************************/

#ifdef TARGET_XARCH
#define AM_DISP_BITS ((sizeof(unsigned) * 8) - 2 * (REGNUM_BITS + 1) - 2)
#define AM_DISP_BIG_VAL (-(1 << (AM_DISP_BITS - 1)))
#define AM_DISP_MIN (-((1 << (AM_DISP_BITS - 1)) - 1))
#define AM_DISP_MAX (+((1 << (AM_DISP_BITS - 1)) - 1))

    struct emitAddrMode
    {
        RegNum   base : REGNUM_BITS + 1;
        RegNum   index : REGNUM_BITS + 1;
        uint32_t scale : 2;
        int32_t  disp : AM_DISP_BITS;
    };
#endif // TARGET_XARCH

#ifdef DEBUG // This information is used in DEBUG builds to display the method name for call instructions
    struct instrDescDebugInfo
    {
        unsigned          idNum;
        uint16_t          idSize;                // size of the instruction descriptor
        bool              idFinallyCall = false; // Branch instruction is a call to finally
        bool              idCatchRet    = false; // Instruction is for a catch 'return'
        int               varNum        = INT_MIN;
        int               varOffs       = 0;
        HandleKind        idHandleKind  = HandleKind::None;
        void*             idHandle      = nullptr;
        CORINFO_SIG_INFO* idCallSig     = nullptr; // Used to report native call site signatures to the EE

        instrDescDebugInfo(unsigned num, unsigned size) : idNum(num), idSize(static_cast<uint16_t>(size))
        {
            assert(size <= UINT16_MAX);
        }
    };
#endif // DEBUG

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

    private:
#ifdef TARGET_XARCH
        static_assert_no_msg(INS_COUNT <= 1024);
        static_assert_no_msg(IF_COUNT <= 128);
        static_assert_no_msg(ACTUAL_REG_COUNT <= 64);
        static constexpr unsigned SmallImmBits = 16;

        instruction _idIns : 10;      // Instruction opcode
        insFormat   _idInsFmt : 7;    // Instruction format
        unsigned    _idOpSize : 3;    // Operation size (log 2)
        GCtype      _idGCref : 2;     // GC type of the first destination register
        unsigned    _idCnsReloc : 1;  // Immediate is relocatable
        unsigned    _idDspReloc : 1;  // Address mode displacement is relocatable
        unsigned    _idSmallDsc : 1;  // this is instrDescSmall
        unsigned    _idLargeCall : 1; // this is instrDescCGCA
        unsigned    _idLargeCns : 1;  // this is instrDescCns/instrDescCnsAmd
        unsigned    _idLargeDsp : 1;  // this is instrDescAmd
        unsigned    _idBound : 1;     // Jump target / frame offset bound
        unsigned    _idNoGC : 1;      // Helper call that does not need GC information
        unsigned    _idSpare : 2;     // Reserved for EVEX/APX registers?
        unsigned    _idCodeSize : 4;  // Encoded instruction size
        RegNum      _idReg1 : 6;      // First register, also holds the GC ref reg mask for calls
        RegNum      _idReg2 : 6;      // Second register, also holds the GC byref reg mask for calls
        unsigned    _idSmallCns : SmallImmBits;
#endif // TARGET_XARCH

#ifdef TARGET_ARM64
        static_assert_no_msg(INS_COUNT <= 512);
        static_assert_no_msg(IF_COUNT <= 128);
        // REG_SP is included the in actual reg count but we don't need that here.
        static_assert_no_msg(ACTUAL_REG_COUNT - 1 <= 64);
        static constexpr unsigned SmallImmBits = 16;

        instruction _idIns : 9;       // Instruction opcode
        insFormat   _idInsFmt : 7;    // Instruction format
        GCtype      _idGCref : 2;     // GC type of the first destination register
        RegNum      _idReg1 : 6;      // First register, also holds the GC ref reg mask for calls
        RegNum      _idReg2 : 6;      // Second register, also holds the GC byref reg mask for calls
        unsigned    _idSmallDsc : 1;  // this is instrDescSmall
        unsigned    _idLargeCall : 1; // this is instrDescCGCA
        unsigned    _idLargeCns : 1;  // this is instrDescCns
        unsigned    _idBound : 1;     // Jump target / frame offset bound
        unsigned    _idNoGC : 1;      // Helper call that does not need GC information
        unsigned    _idOpSize : 3;    // Operation size (log 2)
        insOpts     _idInsOpt : 6;    // Instruction options
        unsigned    _idLclVar : 1;    // Local load/store
        unsigned    _idCnsReloc : 1;  // Immediate is relocatable
        unsigned    _idSpare : 2;     // Give these to small imm?
        unsigned    _idSmallCns : SmallImmBits;
#endif // TARGET_ARM64

#ifdef TARGET_ARM
        static_assert_no_msg(INS_COUNT <= 256);
        static_assert_no_msg(IF_COUNT <= 128);
        static_assert_no_msg(ACTUAL_REG_COUNT <= 64);
        static constexpr unsigned SmallImmBits = 16;

        instruction _idIns : 8;       // Instruction opcode
        insFormat   _idInsFmt : 7;    // Instruction format
        unsigned    _idOpSize : 2;    // Operation size (log 2)
        GCtype      _idGCref : 2;     // GC type of the first destination register
        RegNum      _idReg1 : 6;      // First register, also holds the GC ref reg mask for calls
        RegNum      _idReg2 : 6;      // Second register, also holds the GC byref reg mask for calls
        unsigned    _idSmallDsc : 1;  // this is instrDescSmall
        unsigned    _idLargeCall : 1; // this is instrDescCGCA
        unsigned    _idLargeCns : 1;  // this is instrDescCns
        unsigned    _idBound : 1;     // Jump target / frame offset bound
        unsigned    _idNoGC : 1;      // Helper call that does not need GC information
        insSize     _idInsSize : 2;   // Encoded instruction size: 16, 32 or 48 bits
        insFlags    _idInsFlags : 1;  // Instruction sets flags
        unsigned    _idLclVar : 1;    // Local load/store
        insOpts     _idInsOpt : 3;    // Instruction options
        unsigned    _idCnsReloc : 1;  // Immediate is relocatable
        unsigned    _idSpare : 4;     // Give these to small imm?
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

        bool idIsBound() const
        {
            return _idBound;
        }

        void idSetIsBound()
        {
            _idBound = true;
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

        void SetVarAddr(int varNum, int varOffs)
        {
#ifdef TARGET_ARMARCH
            _idLclVar = true;
#endif
#ifdef DEBUG
            _idDebugOnlyInfo->varNum  = varNum;
            _idDebugOnlyInfo->varOffs = varOffs;
#endif
        }

#ifdef DEBUG
        bool InstrHasNoCode() const
        {
            return (_idInsFmt == IF_GC_REG)
#ifdef TARGET_XARCH
                   || (_idIns == INS_align)
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

    struct instrDesc : public instrDescSmall
    {
    private:
        union idAddrUnion {
            // TODO-Cleanup: We should really add a DEBUG-only tag to this union so we can add asserts
            // about reading what we think is here, to avoid unexpected corruption issues.

            BasicBlock* iiaBBlabel;
            insGroup*   iiaIGlabel;
            void*       iiaAddr;

#ifdef TARGET_XARCH
            CORINFO_FIELD_HANDLE iiaFieldHnd;
#endif
#ifdef TARGET_ARM64
            unsigned roDataOffset;
#endif

            // Used to specify an instruction count for jumps, instead of using
            // a label and multiple blocks. This is used in the prolog as well
            // as for IF_LARGEJMP pseudo-branch instructions.
            int iiaEncodedInstrCount;

#ifdef TARGET_ARM
            struct
            {
                unsigned isTrackedGCSlotStore : 1;
                unsigned isGCArgStore : 1;
                int      lclOffset : 30;
            };

            struct
            {
                regNumber _idReg3 : REGNUM_BITS;
                regNumber _idReg4 : REGNUM_BITS;
            };
#endif

#ifdef TARGET_X86
            emitAddrMode iiaAddrMode;
            regNumber    _idReg3 : REGNUM_BITS;

            struct
            {
                unsigned isTrackedGCSlotStore : 1;
                unsigned isEbpBased : 1;
                int      lclOffset : 30;
            };
#endif

#ifdef TARGET_AMD64
            emitAddrMode iiaAddrMode;
            regNumber    _idReg3 : REGNUM_BITS;

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
                regNumber _idReg3 : REGNUM_BITS;
                regNumber _idReg4 : REGNUM_BITS;
                unsigned  _idReg3Scaled : 1;
                GCtype    _idGCref2 : 2;
                unsigned  isTrackedGCSlotStore : 1;
                unsigned  isGCArgStore : 1;
                int       lclOffset;
            };
#endif

#ifdef TARGET_ARM64
            bool iiaIsJitDataOffset() const
            {
                return (roDataOffset & iaut_MASK) == iaut_DATA_OFFSET;
            }

            unsigned iiaGetJitDataOffset() const
            {
                assert(iiaIsJitDataOffset());
                return roDataOffset >> iaut_SHIFT;
            }

            void SetRoDataOffset(unsigned offset)
            {
                roDataOffset = (offset << iaut_SHIFT) | iaut_DATA_OFFSET;
            }
#endif

            bool iiaHasInstrCount() const
            {
                return (iiaEncodedInstrCount & iaut_MASK) == iaut_INST_COUNT;
            }

            int iiaGetInstrCount() const
            {
                assert(iiaHasInstrCount());
                return (iiaEncodedInstrCount >> iaut_SHIFT);
            }

            void iiaSetInstrCount(int count)
            {
                assert(abs(count) < 10);
                iiaEncodedInstrCount = (count << iaut_SHIFT) | iaut_INST_COUNT;
            }
        } _idAddrUnion;

        static_assert_no_msg(sizeof(idAddrUnion) == sizeof(void*));

    public:
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
        instrDescJmp* idjNext; // next jump in the group/method
        insGroup*     idjIG;   // containing group
        unsigned      idjOffs; // The byte offset within IG of the jump instruction.

        void SetInstrCount(int count)
        {
            idSetIsBound();
            idAddr()->iiaSetInstrCount(count);
        }
    };

    struct instrDescCGCA : instrDesc // call with ...
    {
        VARSET_TP idcGCvars; // ... updated GC vars or
#ifdef TARGET_XARCH
        int32_t idcDisp; // ... big addrmode disp
#endif
        regMaskTP idcGcrefRegs; // ... gcref registers
        regMaskTP idcByrefRegs; // ... byref registers
#ifdef TARGET_X86
        int idcArgCnt; // ... lots of args or (<0 ==> caller pops args)
#endif
    };

#if defined(DEBUG) || defined(LATE_DISASM)
#if defined(TARGET_XARCH)
    static insFormat getMemoryOperation(instrDesc* id);
#elif defined(TARGET_ARM64)
    void getMemoryOperation(instrDesc* id, unsigned* pMemAccessKind, bool* pIsLocalAccess);
#endif

#define PERFSCORE_THROUGHPUT_ILLEGAL -1024.0f

#define PERFSCORE_THROUGHPUT_ZERO 0.0f // Only used for pseudo-instructions that don't generate code

#define PERFSCORE_THROUGHPUT_6X (1.0f / 6.0f) // Hextuple issue
#define PERFSCORE_THROUGHPUT_5X 0.20f         // Pentuple issue
#define PERFSCORE_THROUGHPUT_4X 0.25f         // Quad issue
#define PERFSCORE_THROUGHPUT_3X (1.0f / 3.0f) // Three issue
#define PERFSCORE_THROUGHPUT_2X 0.5f          // Dual issue

#define PERFSCORE_THROUGHPUT_1C 1.0f // Single Issue

#define PERFSCORE_THROUGHPUT_2C 2.0f   // slower - 2 cycles
#define PERFSCORE_THROUGHPUT_3C 3.0f   // slower - 3 cycles
#define PERFSCORE_THROUGHPUT_4C 4.0f   // slower - 4 cycles
#define PERFSCORE_THROUGHPUT_5C 5.0f   // slower - 5 cycles
#define PERFSCORE_THROUGHPUT_6C 6.0f   // slower - 6 cycles
#define PERFSCORE_THROUGHPUT_7C 7.0f   // slower - 7 cycles
#define PERFSCORE_THROUGHPUT_8C 8.0f   // slower - 8 cycles
#define PERFSCORE_THROUGHPUT_9C 9.0f   // slower - 9 cycles
#define PERFSCORE_THROUGHPUT_10C 10.0f // slower - 10 cycles
#define PERFSCORE_THROUGHPUT_13C 13.0f // slower - 13 cycles
#define PERFSCORE_THROUGHPUT_19C 19.0f // slower - 19 cycles
#define PERFSCORE_THROUGHPUT_25C 25.0f // slower - 25 cycles
#define PERFSCORE_THROUGHPUT_33C 33.0f // slower - 33 cycles
#define PERFSCORE_THROUGHPUT_52C 52.0f // slower - 52 cycles
#define PERFSCORE_THROUGHPUT_57C 57.0f // slower - 57 cycles

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
#define PERFSCORE_LATENCY_400C 400.0f // Intel microcode issue with these instuctions

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

// Make this an enum:
//
#define PERFSCORE_MEMORY_NONE 0
#define PERFSCORE_MEMORY_READ 1
#define PERFSCORE_MEMORY_WRITE 2
#define PERFSCORE_MEMORY_READ_WRITE 3

#define PERFSCORE_CODESIZE_COST_HOT 0.10f
#define PERFSCORE_CODESIZE_COST_COLD 0.01f

#define PERFSCORE_CALLEE_SPILL_COST 0.75f

    struct insExecutionCharacteristics
    {
        float    insThroughput;
        float    insLatency;
        unsigned insMemoryAccessKind;
    };

    float insEvaluateExecutionCost(instrDesc* id);

    insExecutionCharacteristics getInsExecutionCharacteristics(instrDesc* id);

    void perfScoreUnhandledInstruction(instrDesc* id, insExecutionCharacteristics* result);

    BasicBlock::weight_t getCurrentBlockWeight();
#endif // defined(DEBUG) || defined(LATE_DISASM)

    void dispIns(instrDesc* id);

    void appendToCurIG(instrDesc* id);

    size_t emitGetInstrDescSizeSC(const instrDesc* id);

#ifndef TARGET_XARCH
    target_ssize_t emitGetInsSC(instrDesc* id);
#endif

#ifdef DEBUG
    static const char* emitIfName(unsigned f);

    unsigned emitInsCount = 0;

    static const char* emitRegName(regNumber reg, emitAttr size = EA_PTRSIZE);

    void GetGCDeltaDumpHeader(char* buffer, size_t count);
    void emitDispIG(insGroup* ig, insGroup* igPrev, bool verbose);

public:
    unsigned GetCodeSize() const
    {
        return emitTotalHotCodeSize + emitTotalColdCodeSize;
    }

    void emitDispIGlist(bool verbose = false);

private:
    void emitDispInsAddr(BYTE* code);
    void emitDispInsOffs(unsigned offs, bool doffs);
#endif // !DEBUG

    /************************************************************************/
    /*                      Method prolog and epilog                        */
    /************************************************************************/

    unsigned emitPrologEndPos;

    insGroup* emitPlaceholderList = nullptr; // per method placeholder list - head
    insGroup* emitPlaceholderLast = nullptr; // per method placeholder list - tail

    void emitBegPrologEpilog(insGroup* igPh);
    void emitEndPrologEpilog();

#ifdef JIT32_GCENCODER
    // The x86 GC encoder needs to iterate over a list of epilogs to generate a table of
    // epilog offsets. Epilogs always start at the beginning of an IG, so save the first
    // IG of the epilog, and use it to find the epilog offset at the end of code generation.
    struct EpilogList
    {
        EpilogList*  elNext;
        emitLocation elLoc;

        EpilogList() : elNext(nullptr), elLoc()
        {
        }
    };

    EpilogList*  emitEpilogList = nullptr; // per method epilog list - head
    EpilogList*  emitEpilogLast = nullptr; // per method epilog list - tail
    emitLocation emitExitSeqBegLoc;
    unsigned     emitExitSeqSize = INT_MAX; // minimum size of any return sequence - the 'ret' after the epilog
    unsigned     emitEpilogCnt   = 0;
    unsigned     emitEpilogSize  = 0;

    void emitBegFnEpilog(insGroup* igPh);
    void emitEndFnEpilog();

public:
    void emitStartExitSeq(); // Mark the start of the "return" sequence
    void emitStartEpilog();
    bool emitHasEpilogEnd();
#endif // JIT32_GCENCODER

    /************************************************************************/
    /*    Methods to record a code position and later convert to offset     */
    /************************************************************************/

    unsigned emitFindInsNum(insGroup* ig, instrDesc* id);
    uint32_t emitFindOffset(insGroup* ig, unsigned insNum);

    /************************************************************************/
    /*        Members and methods used to issue (encode) instructions.      */
    /************************************************************************/

    // If we have started issuing instructions from the list of instrDesc, this is set
    INDEBUG(bool emitIssuing = false;)

    BYTE*  emitCodeBlock;     // Hot code block
    BYTE*  emitColdCodeBlock; // Cold code block
    BYTE*  emitConsBlock;     // Read-only (constant) data block
    size_t writeableOffset;   // Offset applied to a code address to get memory location that can be written

    UNATIVE_OFFSET emitTotalHotCodeSize;
    UNATIVE_OFFSET emitTotalColdCodeSize;

    UNATIVE_OFFSET emitCurCodeOffs(BYTE* dst);
    BYTE* emitOffsetToPtr(UNATIVE_OFFSET offset);
    BYTE* emitDataOffsetToPtr(UNATIVE_OFFSET offset);
    bool emitJumpCrossHotColdBoundary(size_t srcOffset, size_t dstOffset);

    size_t emitIssue1Instr(insGroup* ig, instrDesc* id, uint8_t** dp);
    size_t emitOutputInstr(insGroup* ig, instrDesc* id, uint8_t** dp);

#ifdef PSEUDORANDOM_NOP_INSERTION
    bool emitInInstrumentation = false;
#endif

    insGroup* emitCurIG;
    insGroup* emitCurLabel = nullptr;

private:
    /************************************************************************/
    /*      The logic that creates and keeps track of instruction groups    */
    /************************************************************************/

    insGroup* emitIGfirst = nullptr;
    insGroup* emitIGlast  = nullptr;

    instrDescJmp* emitJumpList = nullptr; // list of local jumps in method
    instrDescJmp* emitJumpLast = nullptr; // last of local jumps in method

public:
    insGroup* GetCurrentInsGroup() const
    {
        return emitCurIG;
    }

    void PrologSpillParamRegsToShadowSlots();

    CORINFO_FIELD_HANDLE emitBlkConst(const void* cnsAddr, unsigned cnsSize, unsigned cnsAlign, var_types elemType);
    CORINFO_FIELD_HANDLE emitFltOrDblConst(double constValue, emitAttr attr);

    INDEBUG(static bool IsCodeAligned(UNATIVE_OFFSET offset);)
    void emitJumpDistBind(); // Bind all the local jumps in method

#if FEATURE_LOOP_ALIGN
    void emitLoopAlignment();
    bool emitEndsWithAlignInstr(); // Validate if newLabel is appropriate
    void emitSetLoopBackEdge(BasicBlock* loopTopBlock);
    void emitLoopAlignAdjustments(); // Predict if loop alignment is needed and make appropriate adjustments

private:
    instrDescAlign* emitCurIGAlignList   = nullptr; // list of align instructions in current IG
    unsigned        emitLastLoopStart    = 0;       // Start IG of last inner loop
    unsigned        emitLastLoopEnd      = 0;       // End IG of last inner loop
    unsigned        emitLastAlignedIgNum = 0;       // last IG that has align instruction
    instrDescAlign* emitAlignList        = nullptr; // list of local align instructions in method
    instrDescAlign* emitAlignLast        = nullptr; // last align instruction in method
    unsigned getLoopSize(insGroup* igLoopHeader,
                         unsigned maxLoopSize DEBUG_ARG(bool isAlignAdjusted)); // Get the smallest loop size
    unsigned emitCalculatePaddingForLoopAlignment(insGroup* ig, size_t offset DEBUG_ARG(bool isAlignAdjusted));
#endif

private:
    INDEBUG(void emitCheckFuncletBranch(instrDescJmp* jmp);)

    // Are we generating IGF_NOGCINTERRUPT insGroups (for prologs, epilogs, etc.)
    bool emitNoGCIG = false;
    // If we generate an instruction, and not another instruction group, force create a new emitAdd
    // instruction group.
    bool emitForceNewIG = false;

    BYTE* emitCurIGfreeNext;           // next available byte in buffer
    BYTE* emitCurIGfreeEndp;           // one byte past the last available byte in buffer
    BYTE* emitCurIGfreeBase = nullptr; // first byte address

    unsigned       emitCurIGinsCnt;       // # of collected instr's in buffer
    unsigned       emitCurIGsize;         // estimated code size of current group in bytes
    UNATIVE_OFFSET emitCurCodeOffset = 0; // current code offset within group
    UNATIVE_OFFSET emitTotalCodeSize = 0; // bytes of code in entire method

    insGroup* emitFirstColdIG = nullptr; // first cold instruction group

public:
    void emitSetFirstColdIGCookie(insGroup* ig)
    {
        emitFirstColdIG = ig;
    }

private:
    instrDescJmp* emitCurIGjmpList = nullptr; // list of jumps   in current IG

    VARSET_TP emitEmptyGCrefVars = VarSetOps::UninitVal();

    static void EncodeCallGCRegs(regMaskTP regs, instrDesc* id);
    static unsigned DecodeCallGCRegs(instrDesc* id);

    unsigned emitNxtIGnum = 0;

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

    insGroup* emitAllocIG();

    void emitNewIG();
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

private:
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
    const char* emitLabelString(insGroup* ig);
#endif

public:
    // Terminates any in-progress instruction group, making the current IG a new empty one.
    // Mark this instruction group as having a label; return the the new instruction group.
    // Sets the emitter's record of the currently live GC variables and registers.
    insGroup* emitAddLabel();

    // Same as above, except the label is added and is conceptually "inline" in
    // the current block. Thus it extends the previous block and the emitter
    // continues to track GC info as if there was no label.
    insGroup* emitAddInlineLabel();

private:
    inline insGroup* emitCodeGetCookie(BasicBlock* block)
    {
        return static_cast<insGroup*>(block->bbEmitCookie);
    }

#ifdef TARGET_ARMARCH
    void emitGetInstrDescs(insGroup* ig, instrDesc** id, int* insCnt);
    bool emitGetLocationInfo(emitLocation* emitLoc, insGroup** pig, instrDesc** pid, int* pinsRemaining = NULL);
    bool emitNextID(insGroup*& ig, instrDesc*& id, int& insRemaining);
    typedef void (*emitProcessInstrFunc_t)(instrDesc* id, void* context);
    void emitWalkIDs(emitLocation* locFrom, emitProcessInstrFunc_t processFunc, void* context);
    static void emitGenerateUnwindNop(instrDesc* id, void* context);
#endif // TARGET_ARMARCH

    int emitNextRandomNop();

    instrDescSmall* emitAllocAnyInstr(unsigned sz, bool updateLastIns);

    static emitJumpKind emitInsToJumpKind(instruction ins);

public:
    static instruction emitJumpKindToIns(emitJumpKind jumpKind);
    static emitJumpKind emitReverseJumpKind(emitJumpKind jumpKind);

private:
#ifdef DEBUG
    void emitInsSanityCheck(instrDesc* id);
#endif

/************************************************************************/
/*    The following is used to distinguish helper vs non-helper calls   */
/************************************************************************/

#ifdef JIT32_GCENCODER
    unsigned emitCntStackDepth;     // 0 in prolog/epilog, One DWORD elsewhere
    unsigned emitMaxStackDepth = 0; // actual computed max. stack depth
    unsigned emitCurStackLvl   = 0; // amount of bytes pushed on stack

    void emitStackPush(unsigned codeOffs, GCtype type);
    void emitStackPushN(unsigned codeOffs, unsigned count);
    void emitStackPop(unsigned codeOffs, unsigned count);
    void emitStackPopArgs(unsigned codeOffs, unsigned count);
    void emitStackKillArgs(unsigned codeOffs, unsigned count);
#endif
    size_t emitRecordGCCall(instrDesc* id, uint8_t* callAddr, uint8_t* callEndAddr);

#ifdef DEBUG
public:
    const char* emitGetFrameReg();

private:
#endif

    void emitGCregLiveUpd(GCtype gcType, regNumber reg, BYTE* addr);
    void emitGCregDeadUpd(regNumber reg, BYTE* addr);
#ifdef FEATURE_EH_FUNCLETS
    void emitGCregDeadAll(BYTE* addr);
#endif

#if FEATURE_FIXED_OUT_ARGS
    void emitGCargLiveUpd(int offs, GCtype gcType, BYTE* addr DEBUGARG(int varNum));
#endif
    void emitGCvarLiveUpd(int slotOffs, GCtype gcType, BYTE* addr DEBUGARG(int varNum));

    /************************************************************************/
    /*      The following logic keeps track of initialized data sections    */
    /************************************************************************/

    /* One of these is allocated for every blob of initialized data */

public:
    // Note to use alignments greater than 32 requires modification in the VM
    // to support larger alignments (see ICorJitInfo::allocMem)
    //
    const static unsigned MIN_DATA_ALIGN = 4;
    const static unsigned MAX_DATA_ALIGN = 32;

private:
    struct dataSection
    {
        enum sectionType
        {
            data,
            blockAbsoluteAddr,
            blockRelative32
        };

        dataSection*   dsNext;
        UNATIVE_OFFSET dsSize;
        sectionType    dsType;
        var_types      dsDataType;

        // variable-sized array used to store the constant data
        // or BasicBlock* array in the block cases.
        BYTE dsCont[0];
    };

    /* These describe the entire initialized/uninitialized data sections */

    struct dataSecDsc
    {
        dataSection*   dsdList;
        dataSection*   dsdLast;
        UNATIVE_OFFSET dsdOffs;
        UNATIVE_OFFSET alignment; // in bytes, defaults to 4

        dataSecDsc() : dsdList(nullptr), dsdLast(nullptr), dsdOffs(0), alignment(4)
        {
        }
    };

    dataSecDsc emitConsDsc;

    dataSection* emitDataSecCur = nullptr;

    void emitOutputDataSec(dataSecDsc* sec, BYTE* dst);
    INDEBUG(void emitDispDataSec(dataSecDsc* section);)

    void emitRecordRelocation(void* location, void* target, uint16_t fRelocType, int32_t addlDelta = 0);

    void emitRecordCallSite(ULONG                 instrOffset,   /* IN */
                            CORINFO_SIG_INFO*     callSig,       /* IN */
                            CORINFO_METHOD_HANDLE methodHandle); /* IN */

#if defined(TARGET_XARCH)
#include "emitxarch.h"
#elif defined(TARGET_ARM)
#include "emitarm.h"
#elif defined(TARGET_ARM64)
#include "emitarm64.h"
#else
#error Unsupported or unset target architecture
#endif
};

using Emitter = emitter;
