// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
/*****************************************************************************/

#ifndef _EMIT_H_
#define _EMIT_H_

#include "instr.h"

#ifndef _GCINFO_H_
#include "gcinfo.h"
#endif

#include "jitgcinfo.h"

#if 0
#define EMITVERBOSE 1
#else
#define EMITVERBOSE (emitComp->verbose)
#endif

#if 0
#define EMIT_GC_VERBOSE 0
#else
#define EMIT_GC_VERBOSE (emitComp->verbose)
#endif

#if 1
#define EMIT_INSTLIST_VERBOSE 0
#else
#define EMIT_INSTLIST_VERBOSE (emitComp->verbose)
#endif

/*****************************************************************************/

#ifdef DEBUG
#define DEBUG_EMIT 1
#else
#define DEBUG_EMIT 0
#endif

#if EMITTER_STATS
void emitterStats(FILE* fout);
#endif

class emitLocation;
class emitter;
struct insGroup;

typedef void (*emitSplitCallbackType)(void* context, emitLocation* emitLoc);

/*****************************************************************************/

//-----------------------------------------------------------------------------

inline bool needsGC(GCtype gcType)
{
    if (gcType == GCT_NONE)
    {
        return false;
    }
    else
    {
        assert(gcType == GCT_GCREF || gcType == GCT_BYREF);
        return true;
    }
}

//-----------------------------------------------------------------------------

#ifdef DEBUG

inline bool IsValidGCtype(GCtype gcType)
{
    return (gcType == GCT_NONE || gcType == GCT_GCREF || gcType == GCT_BYREF);
}

// Get a string name to represent the GC type

inline const char* GCtypeStr(GCtype gcType)
{
    switch (gcType)
    {
        case GCT_NONE:
            return "npt";
        case GCT_GCREF:
            return "gcr";
        case GCT_BYREF:
            return "byr";
        default:
            assert(!"Invalid GCtype");
            return "err";
    }
}

#endif // DEBUG

/*****************************************************************************/

#if DEBUG_EMIT
#define INTERESTING_JUMP_NUM -1 // set to 0 to see all jump info
//#define INTERESTING_JUMP_NUM    0
#endif

/*****************************************************************************
 *
 *  Represent an emitter location.
 */

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

/************************************************************************/
/*          The following describes an instruction group                */
/************************************************************************/

enum insGroupPlaceholderType : uint8_t
{
    IGPT_EPILOG,
#ifdef FEATURE_EH_FUNCLETS
    IGPT_FUNCLET_PROLOG,
    IGPT_FUNCLET_EPILOG,
#endif
};

#if defined(_MSC_VER) && defined(TARGET_ARM)
// ARM aligns structures that contain 64-bit ints or doubles on 64-bit boundaries. This causes unwanted
// padding to be added to the end, so sizeof() is unnecessarily big.
#pragma pack(push)
#pragma pack(4)
#endif // defined(_MSC_VER) && defined(TARGET_ARM)

struct insPlaceholderGroupData
{
    insGroup*               igPhNext;
    BasicBlock*             igPhBB;
    VARSET_TP               igPhInitGCrefVars;
    regMaskTP               igPhInitGCrefRegs;
    regMaskTP               igPhInitByrefRegs;
    VARSET_TP               igPhPrevGCrefVars;
    regMaskTP               igPhPrevGCrefRegs;
    regMaskTP               igPhPrevByrefRegs;
    insGroupPlaceholderType igPhType;
}; // end of struct insPlaceholderGroupData

struct insGroup
{
    insGroup* igNext;

#ifdef DEBUG
    insGroup* igSelf; // for consistency checking
#endif
#if defined(DEBUG) || defined(LATE_DISASM)
    BasicBlock::weight_t igWeight;    // the block weight used for this insGroup
    double               igPerfScore; // The PerfScore for this insGroup
#endif

#ifdef DEBUG
    BasicBlock*               lastGeneratedBlock; // The last block that generated code into this insGroup.
    jitstd::list<BasicBlock*> igBlocks;           // All the blocks that generated code into this insGroup.
#endif

    UNATIVE_OFFSET igNum;     // for ordering (and display) purposes
    UNATIVE_OFFSET igOffs;    // offset of this group within method
    unsigned int   igFuncIdx; // Which function/funclet does this belong to? (Index into Compiler::compFuncInfos array.)
    unsigned short igFlags;   // see IGF_xxx below
    unsigned short igSize;    // # of bytes of code in this group

#if FEATURE_LOOP_ALIGN
    insGroup* igLoopBackEdge; // "last" back-edge that branches back to an aligned loop head.
#endif

#define IGF_GC_VARS 0x0001    // new set of live GC ref variables
#define IGF_BYREF_REGS 0x0002 // new set of live by-ref registers
#if defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
#define IGF_FINALLY_TARGET 0x0004 // this group is the start of a basic block that is returned to after a finally.
#endif                            // defined(FEATURE_EH_FUNCLETS) && defined(TARGET_ARM)
#define IGF_FUNCLET_PROLOG 0x0008 // this group belongs to a funclet prolog
#define IGF_FUNCLET_EPILOG 0x0010 // this group belongs to a funclet epilog.
#define IGF_EPILOG 0x0020         // this group belongs to a main function epilog
#define IGF_NOGCINTERRUPT 0x0040  // this IG is is a no-interrupt region (prolog, epilog, etc.)
#define IGF_UPD_ISZ 0x0080        // some instruction sizes updated
#define IGF_PLACEHOLDER 0x0100    // this is a placeholder group, to be filled in later
#define IGF_EXTEND 0x0200         // this block is conceptually an extension of the previous block
                                  // and the emitter should continue to track GC info as if there was no new block.
#define IGF_LOOP_ALIGN 0x0400     // this group contains alignment instruction(s) at the end; the next IG is the
                                  // head of a loop that needs alignment.

// Mask of IGF_* flags that should be propagated to new blocks when they are created.
// This allows prologs and epilogs to be any number of IGs, but still be
// automatically marked properly.
#if defined(FEATURE_EH_FUNCLETS)
#ifdef DEBUG
#define IGF_PROPAGATE_MASK (IGF_EPILOG | IGF_FUNCLET_PROLOG | IGF_FUNCLET_EPILOG)
#else // DEBUG
#define IGF_PROPAGATE_MASK (IGF_EPILOG | IGF_FUNCLET_PROLOG)
#endif // DEBUG
#else  // !FEATURE_EH_FUNCLETS
#define IGF_PROPAGATE_MASK (IGF_EPILOG)
#endif // !FEATURE_EH_FUNCLETS

    // Try to do better packing based on how large regMaskSmall is (8, 16, or 64 bits).
    CLANG_FORMAT_COMMENT_ANCHOR;
#if REGMASK_BITS <= 32

    union {
        BYTE*                    igData;   // addr of instruction descriptors
        insPlaceholderGroupData* igPhData; // when igFlags & IGF_PLACEHOLDER
    };

    unsigned igStkLvl; // stack level on entry

    regMaskSmall  igGCregs; // set of registers with live GC refs
    unsigned char igInsCnt; // # of instructions  in this group

#else // REGMASK_BITS

    regMaskSmall igGCregs; // set of registers with live GC refs

    union {
        BYTE*                    igData;   // addr of instruction descriptors
        insPlaceholderGroupData* igPhData; // when igFlags & IGF_PLACEHOLDER
    };

#if !FEATURE_FIXED_OUT_ARGS
    unsigned igStkLvl; // stack level on entry
#endif

    unsigned char igInsCnt; // # of instructions  in this group

#endif // REGMASK_BITS

    VARSET_VALRET_TP igGCvars() const
    {
        assert(igFlags & IGF_GC_VARS);

        BYTE* ptr = (BYTE*)igData;
        ptr -= sizeof(VARSET_TP);

        return *(VARSET_TP*)ptr;
    }

    unsigned igByrefRegs() const
    {
        assert(igFlags & IGF_BYREF_REGS);

        BYTE* ptr = (BYTE*)igData;

        if (igFlags & IGF_GC_VARS)
        {
            ptr -= sizeof(VARSET_TP);
        }

        ptr -= sizeof(unsigned);

        return *(unsigned*)ptr;
    }

    bool isLoopAlign()
    {
        return (igFlags & IGF_LOOP_ALIGN) != 0;
    }

}; // end of struct insGroup

//  For AMD64 the maximum prolog/epilog size supported on the OS is 256 bytes
//  Since it is incorrect for us to be jumping across funclet prolog/epilogs
//  we will use the following estimate as the maximum placeholder size.
//
#define MAX_PLACEHOLDER_IG_SIZE 256

#if defined(_MSC_VER) && defined(TARGET_ARM)
#pragma pack(pop)
#endif // defined(_MSC_VER) && defined(TARGET_ARM)

/*****************************************************************************/

#define DEFINE_ID_OPS
#include "emitfmts.h"
#undef DEFINE_ID_OPS

enum idAddrUnionTag
{
    iaut_ALIGNED_POINTER = 0x0,
    iaut_DATA_OFFSET     = 0x1,
    iaut_INST_COUNT      = 0x2,
    iaut_UNUSED_TAG      = 0x3,

    iaut_MASK  = 0x3,
    iaut_SHIFT = 2
};

class CodeGen;

class emitter
{
    friend class emitLocation;
    friend class Compiler;
    friend class CodeGen;
    friend class CodeGenInterface;
    friend class GCInfo;

    Compiler*    emitComp;
    GCInfo*      gcInfo;
    CodeGen*     codeGen;
    ICorJitInfo* emitCmpHandle;

public:
    emitter(Compiler* compiler, CodeGen* codeGen, GCInfo& gcInfo, ICorJitInfo* jitInfo);

#include "emitpub.h"

protected:
    /************************************************************************/
    /*                        Miscellaneous stuff                           */
    /************************************************************************/

    typedef GCInfo::FrameLifetime GCFrameLifetime;
    typedef GCInfo::regPtrDsc     regPtrDsc;
    typedef GCInfo::CallDsc       callDsc;

    void* emitGetMem(size_t sz);

    enum opSize : unsigned
    {
        OPSZ1      = 0,
        OPSZ2      = 1,
        OPSZ4      = 2,
        OPSZ8      = 3,
        OPSZ16     = 4,
        OPSZ32     = 5,
        OPSZ_COUNT = 6,
#ifdef TARGET_AMD64
        OPSZP = OPSZ8,
#else
        OPSZP = OPSZ4,
#endif
    };

#define OPSIZE_INVALID ((opSize)0xffff)

    static const emitter::opSize emitSizeEncode[];
    static const emitAttr        emitSizeDecode[];

    static emitter::opSize emitEncodeSize(emitAttr size);
    static emitAttr emitDecodeSize(emitter::opSize ensz);

    // Currently, we only allow one IG for the prolog
    bool emitIGisInProlog(const insGroup* ig)
    {
        return ig == emitPrologIG;
    }

    bool emitIGisInEpilog(const insGroup* ig)
    {
        return (ig != nullptr) && ((ig->igFlags & IGF_EPILOG) != 0);
    }

#if defined(FEATURE_EH_FUNCLETS)

    bool emitIGisInFuncletProlog(const insGroup* ig)
    {
        return (ig != nullptr) && ((ig->igFlags & IGF_FUNCLET_PROLOG) != 0);
    }

    bool emitIGisInFuncletEpilog(const insGroup* ig)
    {
        return (ig != nullptr) && ((ig->igFlags & IGF_FUNCLET_EPILOG) != 0);
    }

#endif // FEATURE_EH_FUNCLETS

    // If "ig" corresponds to the start of a basic block that is the
    // target of a funclet return, generate GC information for it's start
    // address "cp", as if it were the return address of a call.
    void emitGenGCInfoIfFuncletRetTarget(insGroup* ig, BYTE* cp);

    void emitRecomputeIGoffsets();

    void emitDispCommentForHandle(size_t handle, GenTreeFlags flags);

    /************************************************************************/
    /*          The following describes a single instruction                */
    /************************************************************************/

    enum insFormat : unsigned
    {
#define IF_DEF(en, op1, op2) IF_##en,
#include "emitfmts.h"

        IF_COUNT
    };

#ifdef TARGET_XARCH
#define AM_DISP_BITS ((sizeof(unsigned) * 8) - 2 * (REGNUM_BITS + 1) - 2)
#define AM_DISP_BIG_VAL (-(1 << (AM_DISP_BITS - 1)))
#define AM_DISP_MIN (-((1 << (AM_DISP_BITS - 1)) - 1))
#define AM_DISP_MAX (+((1 << (AM_DISP_BITS - 1)) - 1))

    struct emitAddrMode
    {
        regNumber       amBaseReg : REGNUM_BITS + 1;
        regNumber       amIndxReg : REGNUM_BITS + 1;
        emitter::opSize amScale : 2;
        int32_t         amDisp : AM_DISP_BITS;
    };
#endif // TARGET_XARCH

#ifdef DEBUG // This information is used in DEBUG builds to display the method name for call instructions
    struct instrDescDebugInfo
    {
        unsigned          idNum;
        uint16_t          idSize;                    // size of the instruction descriptor
        bool              idFinallyCall = false;     // Branch instruction is a call to finally
        bool              idCatchRet    = false;     // Instruction is for a catch 'return'
        GenTreeFlags      idFlags       = GTF_EMPTY; // for determining type of handle in idMemCookie
        int               varNum        = INT_MIN;
        int               varOffs       = 0;
        CORINFO_SIG_INFO* idCallSig     = nullptr; // Used to report native call site signatures to the EE
        size_t            idMemCookie   = 0;       // for display of method name (also used by switch table)

        instrDescDebugInfo(unsigned num, unsigned size) : idNum(num), idSize(static_cast<uint16_t>(size))
        {
            assert(size <= UINT16_MAX);
        }
    };

#endif // DEBUG

#ifdef TARGET_ARM
    unsigned insEncodeSetFlags(insFlags sf);

    enum insSize : unsigned
    {
        ISZ_16BIT,
        ISZ_32BIT,
        ISZ_48BIT // pseudo-instruction for conditional branch with imm24 range,
                  // encoded as IT of condition followed by an unconditional branch
    };

    unsigned insEncodeShiftOpts(insOpts opt);
    unsigned insEncodePUW_G0(insOpts opt, int imm);
    unsigned insEncodePUW_H0(insOpts opt, int imm);
#endif // TARGET_ARM

    struct instrDesc
    {
    private:
// The assembly instruction
#if defined(TARGET_XARCH)
        static_assert_no_msg(INS_count <= 1024);
        instruction _idIns : 10;
#define MAX_ENCODED_SIZE 15
#elif defined(TARGET_ARM64)
        static_assert_no_msg(INS_count <= 512);
        instruction _idIns : 9;
#else  // !(defined(TARGET_XARCH) || defined(TARGET_ARM64))
        static_assert_no_msg(INS_count <= 256);
        instruction _idIns : 8;
#endif // !(defined(TARGET_XARCH) || defined(TARGET_ARM64))

// The format for the instruction
#if defined(TARGET_XARCH)
        static_assert_no_msg(IF_COUNT <= 128);
        insFormat _idInsFmt : 7;
#else
        static_assert_no_msg(IF_COUNT <= 256);
        insFormat _idInsFmt : 8;
#endif

    public:
        instruction idIns() const
        {
            return _idIns;
        }
        void idIns(instruction ins)
        {
            assert((ins != INS_invalid) && (ins < INS_count));
            _idIns = ins;
        }

        insFormat idInsFmt() const
        {
            return _idInsFmt;
        }
        void idInsFmt(insFormat insFmt)
        {
#if defined(TARGET_ARM64)
            noway_assert(insFmt != IF_NONE); // Only the x86 emitter uses IF_NONE, it is invalid for ARM64 (and ARM32)
#endif
            assert(insFmt < IF_COUNT);
            _idInsFmt = insFmt;
        }

        void idSetRelocFlags(emitAttr attr)
        {
            _idCnsReloc = (EA_IS_CNS_RELOC(attr) ? 1 : 0);
            _idDspReloc = (EA_IS_DSP_RELOC(attr) ? 1 : 0);
        }

        ////////////////////////////////////////////////////////////////////////
        // Space taken up to here:
        // x86:   17 bits
        // amd64: 17 bits
        // arm:   16 bits
        // arm64: 17 bits

    private:
#if defined(TARGET_XARCH)
        unsigned _idCodeSize : 4; // size of instruction in bytes. Max size of an Intel instruction is 15 bytes.
        opSize   _idOpSize : 3;   // operand size: 0=1 , 1=2 , 2=4 , 3=8, 4=16, 5=32
                                  // At this point we have fully consumed first DWORD so that next field
                                  // doesn't cross a byte boundary.
#elif defined(TARGET_ARM64)
// Moved the definition of '_idOpSize' later so that we don't cross a 32-bit boundary when laying out bitfields
#else  // ARM
        opSize      _idOpSize : 2; // operand size: 0=1 , 1=2 , 2=4 , 3=8
#endif // ARM

        // On Amd64, this is where the second DWORD begins
        // On System V a call could return a struct in 2 registers. The instrDescCGCA struct below has  member that
        // stores the GC-ness of the second register.
        // It is added to the instrDescCGCA and not here (the base struct) since it is not needed by all the
        // instructions. This struct (instrDesc) is very carefully kept to be no more than 128 bytes. There is no more
        // space to add members for keeping GC-ness of the second return registers. It will also bloat the base struct
        // unnecessarily since the GC-ness of the second register is only needed for call instructions.
        // The instrDescCGCA struct's member keeping the GC-ness of the first return register is _idcSecondRetRegGCType.
        GCtype _idGCref : 2; // GCref operand? (value is a "GCtype")

        // The idReg1 and idReg2 fields hold the first and second register
        // operand(s), whenever these are present. Note that currently the
        // size of these fields is 6 bits on all targets, and care needs to
        // be taken to make sure all of these fields stay reasonably packed.

        // Note that we use the _idReg1 and _idReg2 fields to hold
        // the live gcrefReg mask for the call instructions on x86/x64
        //
        regNumber _idReg1 : REGNUM_BITS; // register num

        regNumber _idReg2 : REGNUM_BITS;

        ////////////////////////////////////////////////////////////////////////
        // Space taken up to here:
        // x86:   38 bits
        // amd64: 38 bits
        // arm:   32 bits
        // arm64: 31 bits

        unsigned _idSmallDsc : 1;  // is this a "small" descriptor?
        unsigned _idLargeCns : 1;  // does a large constant     follow?
        unsigned _idLargeCall : 1; // large call descriptor used
        unsigned _idBound : 1;     // jump target / frame offset bound
#ifdef TARGET_XARCH
        unsigned _idLargeDsp : 1;   // does a large displacement follow?
        unsigned _idCallRegPtr : 1; // IL indirect calls: addr in reg
#endif
        INDEBUG(unsigned _idCallAddr : 1;) // IL indirect calls: can make a direct call to iiaAddr
        unsigned _idNoGC : 1;              // Some helpers don't get recorded in GC tables

#ifdef TARGET_ARM64
        opSize   _idOpSize : 3; // operand size: 0=1 , 1=2 , 2=4 , 3=8, 4=16
        insOpts  _idInsOpt : 6; // options for instructions
        unsigned _idLclVar : 1; // access a local on stack
#endif

#ifdef TARGET_ARM
        insSize  _idInsSize : 2;  // size of instruction: 16, 32 or 48 bits
        insFlags _idInsFlags : 1; // will this instruction set the flags
        unsigned _idLclVar : 1;   // access a local on stack
        insOpts  _idInsOpt : 3;   // options for Load/Store instructions

// For arm we have used 16 bits
#define ID_EXTRA_BITFIELD_BITS (16)

#elif defined(TARGET_ARM64)
// For Arm64, we have used 16 bits from the second DWORD.
#define ID_EXTRA_BITFIELD_BITS (16)
#elif defined(TARGET_XARCH)
                                   // For xarch, we have used 14 bits from the second DWORD.
#define ID_EXTRA_BITFIELD_BITS (14)
#else
#error Unsupported or unset target architecture
#endif

        ////////////////////////////////////////////////////////////////////////
        // Space taken up to here:
        // x86:   46 bits
        // amd64: 46 bits
        // arm:   48 bits
        // arm64: 49 bits

        unsigned _idCnsReloc : 1; // LargeCns is an RVA and needs reloc tag
        unsigned _idDspReloc : 1; // LargeDsp is an RVA and needs reloc tag

#define ID_EXTRA_RELOC_BITS (2)

        ////////////////////////////////////////////////////////////////////////
        // Space taken up to here:
        // x86:   48 bits
        // amd64: 48 bits
        // arm:   50 bits
        // arm64: 51 bits
        CLANG_FORMAT_COMMENT_ANCHOR;

#define ID_EXTRA_BITS (ID_EXTRA_RELOC_BITS + ID_EXTRA_BITFIELD_BITS)

/* Use whatever bits are left over for small constants */

#define ID_BIT_SMALL_CNS (32 - ID_EXTRA_BITS)
#define ID_MIN_SMALL_CNS 0
#define ID_MAX_SMALL_CNS (int)((1 << ID_BIT_SMALL_CNS) - 1U)

        ////////////////////////////////////////////////////////////////////////
        // Small constant size:
        // x86:   16 bits
        // amd64: 16 bits
        // arm:   14 bits
        // arm64: 13 bits

        unsigned _idSmallCns : ID_BIT_SMALL_CNS;

        ////////////////////////////////////////////////////////////////////////
        // Space taken up to here: 64 bits, all architectures, by design.
        ////////////////////////////////////////////////////////////////////////
        CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef DEBUG

        instrDescDebugInfo* _idDebugOnlyInfo;

    public:
        instrDescDebugInfo* idDebugOnlyInfo() const
        {
            return _idDebugOnlyInfo;
        }
        void idDebugOnlyInfo(instrDescDebugInfo* info)
        {
            _idDebugOnlyInfo = info;
        }

    private:
#endif // DEBUG

        CLANG_FORMAT_COMMENT_ANCHOR;

//
// This is the end of the 'small' instrDesc which is the same on all
//   platforms (except 64-bit DEBUG which is a little bigger).
// Non-DEBUG sizes:
//   x86/amd64/arm/arm64: 64 bits
// DEBUG sizes (includes one pointer):
//   x86:   2 DWORDs, 96 bits
//   amd64: 4 DWORDs, 128 bits
//   arm:   3 DWORDs, 96 bits
//   arm64: 4 DWORDs, 128 bits
// There should no padding or alignment issues on any platform or
//   configuration (including DEBUG which has 1 extra pointer).
//

/*
    If you add lots more fields that need to be cleared (such
    as various flags), you might need to update the body of
    emitter::emitAllocInstr() to clear them.
 */

#if DEBUG
#define SMALL_IDSC_DEBUG_EXTRA (sizeof(void*))
#else
#define SMALL_IDSC_DEBUG_EXTRA (0)
#endif

#define SMALL_IDSC_SIZE (8 + SMALL_IDSC_DEBUG_EXTRA)

        void checkSizes();

        union idAddrUnion {
            // TODO-Cleanup: We should really add a DEBUG-only tag to this union so we can add asserts
            // about reading what we think is here, to avoid unexpected corruption issues.

            BasicBlock* iiaBBlabel;
            insGroup*   iiaIGlabel;
            BYTE*       iiaAddr;

            // Used to encode an offset into the JIT data constant area
            CORINFO_FIELD_HANDLE iiaFieldHnd;

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

            struct
            {
                unsigned isTrackedGCSlotStore : 1;
                unsigned isEbpBased : 1;
                int      lclOffset : 30;
            };

            struct
            {
                regNumber _idReg3 : REGNUM_BITS;
                regNumber _idReg4 : REGNUM_BITS;
            };
#endif

#ifdef TARGET_AMD64
            emitAddrMode iiaAddrMode;

            struct
            {
                regNumber _idReg3 : REGNUM_BITS;
                regNumber _idReg4 : REGNUM_BITS;
                unsigned  isTrackedGCSlotStore : 1;
                unsigned  isGCArgStore : 1;
                unsigned  isEbpBased : 1;
                int       lclOffset;
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

            bool iiaIsJitDataOffset() const;
            int  iiaGetJitDataOffset() const;

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

        static_assert_no_msg(sizeof(idAddrUnion) <= sizeof(void*));

    public:
        bool idIsSmallDsc() const
        {
            return (_idSmallDsc != 0);
        }
        void idSetIsSmallDsc()
        {
            _idSmallDsc = 1;
        }

#if defined(TARGET_XARCH)

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

#elif defined(TARGET_ARM64)
        unsigned idCodeSize() const
        {
            int size = 4;
            switch (idInsFmt())
            {
                case IF_LARGEADR:
                // adrp + add
                case IF_LARGEJMP:
                    // b<cond> + b<uncond>
                    size = 8;
                    break;
                case IF_LARGELDC:
                    if (isVectorRegister(idReg1()))
                    {
                        // adrp + ldr + fmov
                        size = 12;
                    }
                    else
                    {
                        // adrp + ldr
                        size = 8;
                    }
                    break;
                default:
                    break;
            }

            return size;
        }

#elif defined(TARGET_ARM)

        bool idInstrIsT1() const
        {
            return (_idInsSize == ISZ_16BIT);
        }
        unsigned idCodeSize() const
        {
            unsigned result = (_idInsSize == ISZ_16BIT) ? 2 : (_idInsSize == ISZ_32BIT) ? 4 : 6;
            return result;
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

        emitAttr idOpSize()
        {
            return emitDecodeSize(_idOpSize);
        }
        void idOpSize(emitAttr opsz)
        {
            _idOpSize = emitEncodeSize(opsz);
        }

        GCtype idGCref() const
        {
            return (GCtype)_idGCref;
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

#ifdef TARGET_ARM64
        GCtype idGCrefReg2() const
        {
            assert(!idIsSmallDsc());
            return (GCtype)idAddr()->_idGCref2;
        }
        void idGCrefReg2(GCtype gctype)
        {
            assert(!idIsSmallDsc());
            idAddr()->_idGCref2 = gctype;
        }
#endif // TARGET_ARM64

        regNumber idReg2() const
        {
            return _idReg2;
        }
        void idReg2(regNumber reg)
        {
            _idReg2 = reg;
            assert(reg == _idReg2);
        }

#if defined(TARGET_XARCH)
        regNumber idReg3() const
        {
            assert(!idIsSmallDsc());
            return idAddr()->_idReg3;
        }
        void idReg3(regNumber reg)
        {
            assert(!idIsSmallDsc());
            idAddr()->_idReg3 = reg;
            assert(reg == idAddr()->_idReg3);
        }
        regNumber idReg4() const
        {
            assert(!idIsSmallDsc());
            return idAddr()->_idReg4;
        }
        void idReg4(regNumber reg)
        {
            assert(!idIsSmallDsc());
            idAddr()->_idReg4 = reg;
            assert(reg == idAddr()->_idReg4);
        }
#endif // defined(TARGET_XARCH)
#ifdef TARGET_ARMARCH
        insOpts idInsOpt() const
        {
            return (insOpts)_idInsOpt;
        }
        void idInsOpt(insOpts opt)
        {
            _idInsOpt = opt;
            assert(opt == _idInsOpt);
        }

        regNumber idReg3() const
        {
            assert(!idIsSmallDsc());
            return idAddr()->_idReg3;
        }
        void idReg3(regNumber reg)
        {
            assert(!idIsSmallDsc());
            idAddr()->_idReg3 = reg;
            assert(reg == idAddr()->_idReg3);
        }
        regNumber idReg4() const
        {
            assert(!idIsSmallDsc());
            return idAddr()->_idReg4;
        }
        void idReg4(regNumber reg)
        {
            assert(!idIsSmallDsc());
            idAddr()->_idReg4 = reg;
            assert(reg == idAddr()->_idReg4);
        }
#ifdef TARGET_ARM64
        bool idReg3Scaled() const
        {
            assert(!idIsSmallDsc());
            return (idAddr()->_idReg3Scaled == 1);
        }
        void idReg3Scaled(bool val)
        {
            assert(!idIsSmallDsc());
            idAddr()->_idReg3Scaled = val ? 1 : 0;
        }
#endif // TARGET_ARM64

#endif // TARGET_ARMARCH

        inline static bool fitsInSmallCns(ssize_t val)
        {
            return ((val >= ID_MIN_SMALL_CNS) && (val <= ID_MAX_SMALL_CNS));
        }

        bool idIsLargeCns() const
        {
            return _idLargeCns != 0;
        }
        void idSetIsLargeCns()
        {
            _idLargeCns = 1;
        }

#ifdef TARGET_XARCH
        bool idIsLargeDsp() const
        {
            return _idLargeDsp != 0;
        }
        void idSetIsLargeDsp()
        {
            _idLargeDsp = 1;
        }
        void idSetIsSmallDsp()
        {
            _idLargeDsp = 0;
        }
#endif // TARGET_XARCH

        bool idIsLargeCall() const
        {
            return _idLargeCall != 0;
        }
        void idSetIsLargeCall()
        {
            _idLargeCall = 1;
        }

        bool idIsBound() const
        {
            return _idBound != 0;
        }
        void idSetIsBound()
        {
            _idBound = 1;
        }

#ifdef TARGET_XARCH
        bool idIsCallRegPtr() const
        {
            return _idCallRegPtr != 0;
        }
        void idSetIsCallRegPtr()
        {
            _idCallRegPtr = 1;
        }
#endif

#ifdef DEBUG
        bool idIsCallAddr() const
        {
            return _idCallAddr != 0;
        }
        void idSetIsCallAddr()
        {
            _idCallAddr = 1;
        }
#endif

        // Only call instructions that call helper functions may be marked as "IsNoGC", indicating
        // that a thread executing such a call cannot be stopped for GC.  Thus, in partially-interruptible
        // code, it is not necessary to generate GC info for a call so labeled.
        bool idIsNoGC() const
        {
            return _idNoGC != 0;
        }
        void idSetIsNoGC(bool val)
        {
            _idNoGC = val;
        }

#ifdef TARGET_ARMARCH
        bool idIsLclVar() const
        {
            return _idLclVar != 0;
        }
#endif

        bool idIsCnsReloc() const
        {
            return _idCnsReloc != 0;
        }
        void idSetIsCnsReloc()
        {
            _idCnsReloc = 1;
        }

        bool idIsDspReloc() const
        {
            return _idDspReloc != 0;
        }
        void idSetIsDspReloc(bool val = true)
        {
            _idDspReloc = val;
        }
        bool idIsReloc()
        {
            return idIsDspReloc() || idIsCnsReloc();
        }

        unsigned idSmallCns() const
        {
            return _idSmallCns;
        }
        void idSmallCns(size_t value)
        {
            assert(fitsInSmallCns(value));
            _idSmallCns = value;
        }

        inline const idAddrUnion* idAddr() const
        {
            assert(!idIsSmallDsc());
            return &_idAddrUnion;
        }

        inline idAddrUnion* idAddr()
        {
            assert(!idIsSmallDsc());
            return &_idAddrUnion;
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
    }; // End of  struct instrDesc

#if defined(TARGET_XARCH)
    insFormat getMemoryOperation(instrDesc* id);
#elif defined(TARGET_ARM64)
    void getMemoryOperation(instrDesc* id, unsigned* pMemAccessKind, bool* pIsLocalAccess);
#endif

#if defined(DEBUG) || defined(LATE_DISASM)

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

#endif // defined(DEBUG) || defined(LATE_DISASM)

    BasicBlock::weight_t getCurrentBlockWeight();

    void dispIns(instrDesc* id);

    void appendToCurIG(instrDesc* id);

    /********************************************************************************************/

    struct instrDescJmp : instrDesc
    {
        instrDescJmp* idjNext; // next jump in the group/method
        insGroup*     idjIG;   // containing group

        union {
            BYTE* idjAddr; // address of jump ins (for patching)
        } idjTemp;

        unsigned idjOffs : 30;    // Before jump emission, this is the byte offset within IG of the jump instruction.
                                  // After emission, for forward jumps, this is the target offset -- in bytes from the
                                  // beginning of the function -- of the target instruction of the jump, used to
                                  // determine if this jump needs to be patched.
        unsigned idjShort : 1;    // is the jump known to be a short  one?
        unsigned idjKeepLong : 1; // should the jump be kept long? (used for
                                  // hot to cold and cold to hot jumps)
    };

#if FEATURE_LOOP_ALIGN
    struct instrDescAlign : instrDesc
    {
        instrDescAlign* idaNext; // next align in the group/method
        insGroup*       idaIG;   // containing group
    };
#endif

    struct instrDescCns : instrDesc // large const
    {
        cnsval_ssize_t idcCnsVal;
    };

#ifdef TARGET_XARCH
    struct instrDescDsp : instrDesc // large displacement
    {
        target_ssize_t iddDspVal;
    };

    struct instrDescCnsDsp : instrDesc // large cons + disp
    {
        target_ssize_t iddcCnsVal;
        int            iddcDspVal;
    };

    struct instrDescAmd : instrDesc // large addrmode disp
    {
        ssize_t idaAmdVal;
    };

    struct instrDescCnsAmd : instrDesc // large cons + addrmode disp
    {
        ssize_t idacCnsVal;
        ssize_t idacAmdVal;
    };
#endif // TARGET_XARCH

    struct instrDescCGCA : instrDesc // call with ...
    {
        VARSET_TP idcGCvars; // ... updated GC vars or
#ifdef TARGET_XARCH
        int32_t idcDisp; // ... big addrmode disp
#endif
        regMaskTP idcGcrefRegs; // ... gcref registers
        regMaskTP idcByrefRegs; // ... byref registers
#ifdef TARGET_X86
        unsigned idcArgCnt; // ... lots of args or (<0 ==> caller pops args)
#endif

#if MULTIREG_HAS_SECOND_GC_RET
        // This method handle the GC-ness of the second register in a 2 register returned struct on System V.
        GCtype idSecondGCref() const
        {
            return (GCtype)_idcSecondRetRegGCType;
        }
        void idSecondGCref(GCtype gctype)
        {
            _idcSecondRetRegGCType = gctype;
        }

    private:
        // This member stores the GC-ness of the second register in a 2 register returned struct on System V.
        // It is added to the call struct since it is not needed by the base instrDesc struct, which keeps GC-ness
        // of the first register for the instCall nodes.
        // The base instrDesc is very carefully kept to be no more than 128 bytes. There is no more space to add members
        // for keeping GC-ness of the second return registers. It will also bloat the base struct unnecessarily
        // since the GC-ness of the second register is only needed for call instructions.
        // The base struct's member keeping the GC-ness of the first return register is _idGCref.
        GCtype _idcSecondRetRegGCType : 2; // ... GC type for the second return register.
#endif                                     // MULTIREG_HAS_SECOND_GC_RET
    };

#ifdef TARGET_ARM

    struct instrDescReloc : instrDesc
    {
        BYTE* idrRelocVal;
    };

    BYTE* emitGetInsRelocValue(instrDesc* id);

#endif // TARGET_ARM

    static insUpdateModes emitInsUpdateMode(instruction ins);
    static insFormat emitInsModeFormat(instruction ins, insFormat base);

    static const BYTE emitInsModeFmtTab[];
#ifdef DEBUG
    static const unsigned emitInsModeFmtCnt;
#endif

#if defined(TARGET_XARCH)
    static const insFlags instInfo[INS_count];
#elif defined(TARGET_ARMARCH)
    static const uint8_t instInfo[INS_count];
#else
#error Unsupported target architecture
#endif

    static bool instIsFP(instruction ins);

    size_t emitGetInstrDescSize(const instrDesc* id);
    size_t emitGetInstrDescSizeSC(const instrDesc* id);

#ifdef TARGET_XARCH
    ssize_t emitGetInsCns(instrDesc* id);
    ssize_t emitGetInsDsp(instrDesc* id);
    ssize_t emitGetInsAmd(instrDesc* id);
    ssize_t emitGetInsCIdisp(instrDesc* id);
#endif

#ifdef TARGET_X86
    unsigned emitGetInsCIargs(instrDesc* id);
    int emitGetInsCDinfo(instrDesc* id);
#endif

    cnsval_ssize_t emitGetInsSC(instrDesc* id);
    unsigned emitInsCount = 0;

    /************************************************************************/
    /*           A few routines used for debug display purposes             */
    /************************************************************************/

    INDEBUG(const char* genInsDisplayName(instrDesc* id);)

#if defined(DEBUG) || EMITTER_STATS

    static const char* emitIfName(unsigned f);

#endif // defined(DEBUG) || EMITTER_STATS

#ifdef DEBUG
    static const char* emitRegName(regNumber reg, emitAttr size = EA_PTRSIZE);

    // GC Info changes are not readily available at each instruction.
    // We use debug-only sets to track the per-instruction state, and to remember
    // what the state was at the last time it was output (instruction or label).
    ArrayStack<GCFrameLifetime*> debugGCSlotChanges;

    regPtrDsc* debugPrevRegPtrDsc = nullptr;
    regMaskTP  debugPrevGCrefRegs = RBM_NONE;
    regMaskTP  debugPrevByrefRegs = RBM_NONE;
    void emitDispGCDeltaTitle(const char* title);
    void emitDispGCRegDelta(const char* title, regMaskTP prevRegs, regMaskTP curRegs);
    void emitDispGCVarDelta();
    void emitDispRegPtrListDelta();
    void emitDispGCInfoDelta();

    void emitDispIGflags(unsigned flags);
    void emitDispIG(insGroup* ig, insGroup* igPrev = nullptr, bool verbose = false);
    void emitDispIGlist(bool verbose = false);
    void emitDispGCinfo();
    void emitDispClsVar(CORINFO_FIELD_HANDLE fldHnd, ssize_t offs, bool reloc = false);
#ifdef TARGET_XARCH
    void emitDispFrameRef(instrDesc* id, bool asmfm);
#else
    void emitDispFrameRef(instrDesc* id);
#endif
    void emitDispInsAddr(BYTE* code);
    void emitDispInsOffs(unsigned offs, bool doffs);
    void emitDispInsHex(instrDesc* id, BYTE* code, size_t sz);

#else // !DEBUG
#define emitVarRefOffs 0
#endif // !DEBUG

    /************************************************************************/
    /*                      Method prolog and epilog                        */
    /************************************************************************/

    unsigned emitPrologEndPos;

    unsigned       emitEpilogCnt  = 0;
    UNATIVE_OFFSET emitEpilogSize = 0;

#ifdef TARGET_XARCH

    void           emitStartExitSeq(); // Mark the start of the "return" sequence
    emitLocation   emitExitSeqBegLoc;
    UNATIVE_OFFSET emitExitSeqSize = INT_MAX; // minimum size of any return sequence - the 'ret' after the epilog

#endif // TARGET_XARCH

    insGroup* emitPlaceholderList = nullptr; // per method placeholder list - head
    insGroup* emitPlaceholderLast = nullptr; // per method placeholder list - tail

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

    EpilogList* emitEpilogList = nullptr; // per method epilog list - head
    EpilogList* emitEpilogLast = nullptr; // per method epilog list - tail

public:
    void emitStartEpilog();

    bool emitHasEpilogEnd();

    size_t emitGenEpilogLst(size_t (*fp)(void*, unsigned), void* cp);

#endif // JIT32_GCENCODER

    void emitBegPrologEpilog(insGroup* igPh);
    void emitEndPrologEpilog();

    void emitBegFnEpilog(insGroup* igPh);
    void emitEndFnEpilog();

#if defined(FEATURE_EH_FUNCLETS)

    void emitBegFuncletProlog(insGroup* igPh);
    void emitEndFuncletProlog();

    void emitBegFuncletEpilog(insGroup* igPh);
    void emitEndFuncletEpilog();

#endif // FEATURE_EH_FUNCLETS

    /************************************************************************/
    /*    Methods to record a code position and later convert to offset     */
    /************************************************************************/

    unsigned emitFindInsNum(insGroup* ig, instrDesc* id);
    UNATIVE_OFFSET emitFindOffset(insGroup* ig, unsigned insNum);

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

    UNATIVE_OFFSET emitCurCodeOffs(BYTE* dst)
    {
        size_t distance;
        if ((dst >= emitCodeBlock) && (dst <= (emitCodeBlock + emitTotalHotCodeSize)))
        {
            distance = (dst - emitCodeBlock);
        }
        else
        {
            assert(emitFirstColdIG);
            assert(emitColdCodeBlock);
            assert((dst >= emitColdCodeBlock) && (dst <= (emitColdCodeBlock + emitTotalColdCodeSize)));

            distance = (dst - emitColdCodeBlock + emitTotalHotCodeSize);
        }
        noway_assert((UNATIVE_OFFSET)distance == distance);
        return (UNATIVE_OFFSET)distance;
    }

    BYTE* emitOffsetToPtr(UNATIVE_OFFSET offset)
    {
        if (offset < emitTotalHotCodeSize)
        {
            return emitCodeBlock + offset;
        }
        else
        {
            assert(offset < (emitTotalHotCodeSize + emitTotalColdCodeSize));

            return emitColdCodeBlock + (offset - emitTotalHotCodeSize);
        }
    }

    BYTE* emitDataOffsetToPtr(UNATIVE_OFFSET offset)
    {
        assert(offset < emitDataSize());
        return emitConsBlock + offset;
    }

    bool emitJumpCrossHotColdBoundary(size_t srcOffset, size_t dstOffset)
    {
        if (emitTotalColdCodeSize == 0)
        {
            return false;
        }

        assert(srcOffset < (emitTotalHotCodeSize + emitTotalColdCodeSize));
        assert(dstOffset < (emitTotalHotCodeSize + emitTotalColdCodeSize));

        return ((srcOffset < emitTotalHotCodeSize) != (dstOffset < emitTotalHotCodeSize));
    }

    unsigned char emitOutputByte(BYTE* dst, ssize_t val);
    unsigned char emitOutputWord(BYTE* dst, ssize_t val);
    unsigned char emitOutputLong(BYTE* dst, ssize_t val);
    unsigned char emitOutputSizeT(BYTE* dst, ssize_t val);

#if !defined(HOST_64BIT)
#if defined(TARGET_X86)
    unsigned char emitOutputByte(BYTE* dst, size_t val);
    unsigned char emitOutputWord(BYTE* dst, size_t val);
    unsigned char emitOutputLong(BYTE* dst, size_t val);
    unsigned char emitOutputSizeT(BYTE* dst, size_t val);

    unsigned char emitOutputByte(BYTE* dst, unsigned __int64 val);
    unsigned char emitOutputWord(BYTE* dst, unsigned __int64 val);
    unsigned char emitOutputLong(BYTE* dst, unsigned __int64 val);
    unsigned char emitOutputSizeT(BYTE* dst, unsigned __int64 val);
#endif // defined(TARGET_X86)
#endif // !defined(HOST_64BIT)

    size_t emitIssue1Instr(insGroup* ig, instrDesc* id, BYTE** dp);
    size_t emitOutputInstr(insGroup* ig, instrDesc* id, BYTE** dp);

#ifdef PSEUDORANDOM_NOP_INSERTION
    bool emitInInstrumentation = false;
#endif

#ifdef DEBUG
    bool emitChkAlign; // perform some alignment checks
#endif

    insGroup* emitCurIG;

    void emitSetShortJump(instrDescJmp* id);
    void emitSetMediumJump(instrDescJmp* id);

public:
    CORINFO_FIELD_HANDLE emitBlkConst(const void* cnsAddr, unsigned cnsSize, unsigned cnsAlign, var_types elemType);

private:
    CORINFO_FIELD_HANDLE emitFltOrDblConst(double constValue, emitAttr attr);
    insFormat emitMapFmtForIns(insFormat fmt, instruction ins);
    insFormat emitMapFmtAtoM(insFormat fmt);
    void PrologSpillParamRegsToShadowSlots();

/************************************************************************/
/*      The logic that creates and keeps track of instruction groups    */
/************************************************************************/

#ifdef TARGET_ARMARCH
// The only place where this limited instruction group size is a problem is
// in the prolog, where we only support a single instruction group. We should really fix that.
// ARM32 and ARM64 both can require a bigger prolog instruction group. One scenario is where
// a function uses all the incoming integer and single-precision floating-point arguments,
// and must store them all to the frame on entry. If the frame is very large, we generate
// ugly code like "movw r10, 0x488; add r10, sp; vstr s0, [r10]" for each store, which
// eats up our insGroup buffer.
#define SC_IG_BUFFER_SIZE (100 * sizeof(emitter::instrDesc) + 14 * SMALL_IDSC_SIZE)
#else // !TARGET_ARMARCH
#define SC_IG_BUFFER_SIZE (50 * sizeof(emitter::instrDesc) + 14 * SMALL_IDSC_SIZE)
#endif // !TARGET_ARMARCH

    size_t emitIGbuffSize = 0;

    insGroup* emitIGlist = nullptr; // first  instruction group
    insGroup* emitIGlast = nullptr; // last   instruction group
    insGroup* emitIGthis;           // issued instruction group

    insGroup* emitPrologIG; // prolog instruction group

    instrDescJmp* emitJumpList = nullptr; // list of local jumps in method
    instrDescJmp* emitJumpLast = nullptr; // last of local jumps in method
    void          emitJumpDistBind();     // Bind all the local jumps in method

#if FEATURE_LOOP_ALIGN
    instrDescAlign* emitCurIGAlignList   = nullptr; // list of align instructions in current IG
    unsigned        emitLastLoopStart    = 0;       // Start IG of last inner loop
    unsigned        emitLastLoopEnd      = 0;       // End IG of last inner loop
    unsigned        emitLastAlignedIgNum = 0;       // last IG that has align instruction
    instrDescAlign* emitAlignList        = nullptr; // list of local align instructions in method
    instrDescAlign* emitAlignLast        = nullptr; // last align instruction in method
    unsigned getLoopSize(insGroup* igLoopHeader,
                         unsigned maxLoopSize DEBUG_ARG(bool isAlignAdjusted)); // Get the smallest loop size
    void emitLoopAlignment();
    bool emitEndsWithAlignInstr(); // Validate if newLabel is appropriate
    void emitSetLoopBackEdge(BasicBlock* loopTopBlock);
    void     emitLoopAlignAdjustments(); // Predict if loop alignment is needed and make appropriate adjustments
    unsigned emitCalculatePaddingForLoopAlignment(insGroup* ig, size_t offset DEBUG_ARG(bool isAlignAdjusted));
#endif

    void emitCheckFuncletBranch(instrDesc* jmp, insGroup* jmpIG); // Check for illegal branches between funclets

    // Are forward jumps present?
    bool emitFwdJumps = false;
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

    void emitSetFirstColdIGCookie(void* bbEmitCookie)
    {
        emitFirstColdIG = (insGroup*)bbEmitCookie;
    }

    int emitOffsAdj; // current code offset adjustment

    instrDescJmp* emitCurIGjmpList = nullptr; // list of jumps   in current IG

    // emitPrev* and emitInit* are only used during code generation, not during
    // emission (issuing), to determine what GC values to store into an IG.
    // Note that only the Vars ones are actually used, apparently due to bugs
    // in that tracking. See emitSavIG(): the important use of ByrefRegs is commented
    // out, and GCrefRegs is always saved.

    VARSET_TP emitPrevGCrefVars;
    regMaskTP emitPrevGCrefRegs = RBM_NONE;
    regMaskTP emitPrevByrefRegs = RBM_NONE;

    VARSET_TP emitInitGCrefVars;
    regMaskTP emitInitGCrefRegs = RBM_NONE;
    regMaskTP emitInitByrefRegs = RBM_NONE;

    // If this is set, we ignore comparing emitPrev* and emitInit* to determine
    // whether to save GC state (to save space in the IG), and always save it.

    bool emitForceStoreGCState = false;

    // emitThis* variables are used during emission, to track GC updates
    // on a per-instruction basis. During code generation, per-instruction
    // tracking is done with variables gcVarPtrSetCur, gcRegGCrefSetCur,
    // and gcRegByrefSetCur. However, these are also used for a slightly
    // different purpose during code generation: to try to minimize the
    // amount of GC data stored to an IG, by only storing deltas from what
    // we expect to see at an IG boundary. Also, only emitThisGCrefVars is
    // really the only one used; the others seem to be calculated, but not
    // used due to bugs.

    VARSET_TP emitThisGCrefVars;
    regMaskTP emitThisGCrefRegs = RBM_NONE; // Current set of registers holding GC references
    regMaskTP emitThisByrefRegs = RBM_NONE; // Current set of registers holding BYREF references

    bool emitThisGCrefVset; // Is "emitThisGCrefVars" up to date?

    VARSET_TP emitEmptyGCrefVars = VarSetOps::UninitVal();

    regNumber emitSyncThisObjReg = REG_NA; // where is "this" enregistered for synchronized methods?

#if MULTIREG_HAS_SECOND_GC_RET
    void emitSetSecondRetRegGCType(instrDescCGCA* id, emitAttr secondRetSize);
#endif // MULTIREG_HAS_SECOND_GC_RET

    static void emitEncodeCallGCregs(regMaskTP regs, instrDesc* id);
    static unsigned emitDecodeCallGCregs(instrDesc* id);

    unsigned emitNxtIGnum;

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

    insGroup* emitAllocAndLinkIG();
    insGroup* emitAllocIG();
    void emitInitIG(insGroup* ig);
    void emitInsertIGAfter(insGroup* insertAfterIG, insGroup* ig);

    void emitNewIG();

#if !defined(JIT32_GCENCODER)
    void emitDisableGC();
    void emitEnableGC();
#endif // !defined(JIT32_GCENCODER)

    void emitGenIG(insGroup* ig);
    insGroup* emitSavIG(bool emitAdd = false);
    void emitNxtIG(bool extend);

    bool emitCurIGnonEmpty()
    {
        return (emitCurIG && emitCurIGfreeNext > emitCurIGfreeBase);
    }

    instrDesc* emitLastIns = nullptr;

#ifdef DEBUG
    void emitCheckIGoffsets();
#endif

    // Terminates any in-progress instruction group, making the current IG a new empty one.
    // Mark this instruction group as having a label; return the the new instruction group.
    // Sets the emitter's record of the currently live GC variables
    // and registers.  The "isFinallyTarget" parameter indicates that the current location is
    // the start of a basic block that is returned to after a finally clause in non-exceptional execution.
    void* emitAddLabel(bool isFinallyTarget = false DEBUG_ARG(BasicBlock* block = nullptr));

    // Same as above, except the label is added and is conceptually "inline" in
    // the current block. Thus it extends the previous block and the emitter
    // continues to track GC info as if there was no label.
    void* emitAddInlineLabel();

#ifdef DEBUG
    void emitPrintLabel(insGroup* ig);
    const char* emitLabelString(insGroup* ig);
#endif

#ifdef TARGET_ARMARCH

    void emitGetInstrDescs(insGroup* ig, instrDesc** id, int* insCnt);

    bool emitGetLocationInfo(emitLocation* emitLoc, insGroup** pig, instrDesc** pid, int* pinsRemaining = NULL);

    bool emitNextID(insGroup*& ig, instrDesc*& id, int& insRemaining);

    typedef void (*emitProcessInstrFunc_t)(instrDesc* id, void* context);

    void emitWalkIDs(emitLocation* locFrom, emitProcessInstrFunc_t processFunc, void* context);

    static void emitGenerateUnwindNop(instrDesc* id, void* context);

#endif // TARGET_ARMARCH

#ifdef TARGET_X86
    void emitMarkStackLvl(unsigned stackLevel);
#endif

    int emitNextRandomNop();

    //
    // Functions for allocating instrDescs.
    //
    // The emitAllocXXX functions are the base level that allocate memory, and do little else.
    // The emitters themselves use emitNewXXX, which might be thin wrappers over the emitAllocXXX functions.
    //

    void* emitAllocAnyInstr(unsigned sz, emitAttr attr);

    instrDesc* emitAllocInstr(emitAttr attr)
    {
#if EMITTER_STATS
        emitTotalIDescCnt++;
#endif // EMITTER_STATS
        return (instrDesc*)emitAllocAnyInstr(sizeof(instrDesc), attr);
    }

    instrDescJmp* emitAllocInstrJmp()
    {
#if EMITTER_STATS
        emitTotalIDescJmpCnt++;
#endif // EMITTER_STATS
        return (instrDescJmp*)emitAllocAnyInstr(sizeof(instrDescJmp), EA_1BYTE);
    }

    instrDescCns* emitAllocInstrCns(emitAttr attr)
    {
#if EMITTER_STATS
        emitTotalIDescCnsCnt++;
#endif // EMITTER_STATS
        return (instrDescCns*)emitAllocAnyInstr(sizeof(instrDescCns), attr);
    }

    instrDescCns* emitAllocInstrCns(emitAttr attr, cnsval_size_t cns)
    {
        instrDescCns* result = emitAllocInstrCns(attr);
        result->idSetIsLargeCns();
        result->idcCnsVal = cns;
        return result;
    }

#ifdef TARGET_XARCH
    instrDescDsp* emitAllocInstrDsp(emitAttr attr);
    instrDescCnsDsp* emitAllocInstrCnsDsp(emitAttr attr);
    instrDescAmd* emitAllocInstrAmd(emitAttr attr);
    instrDescCnsAmd* emitAllocInstrCnsAmd(emitAttr attr);
#endif // TARGET_XARCH

    instrDescCGCA* emitAllocInstrCGCA(emitAttr attr)
    {
#if EMITTER_STATS
        emitTotalIDescCGCACnt++;
#endif // EMITTER_STATS
        return (instrDescCGCA*)emitAllocAnyInstr(sizeof(instrDescCGCA), attr);
    }

#if FEATURE_LOOP_ALIGN
    instrDescAlign* emitAllocInstrAlign()
    {
#if EMITTER_STATS
        emitTotalDescAlignCnt++;
#endif // EMITTER_STATS
        return (instrDescAlign*)emitAllocAnyInstr(sizeof(instrDescAlign), EA_1BYTE);
    }
    instrDescAlign* emitNewInstrAlign();
#endif

    instrDesc* emitNewInstrSmall(emitAttr attr);
    instrDesc* emitNewInstr(emitAttr attr = EA_4BYTE);
    instrDesc* emitNewInstrSC(emitAttr attr, cnsval_ssize_t cns);
    instrDesc* emitNewInstrCns(emitAttr attr, cnsval_ssize_t cns);
    instrDesc* emitNewInstrDsp(emitAttr attr, target_ssize_t dsp);
    instrDesc* emitNewInstrCnsDsp(emitAttr attr, target_ssize_t cns, int dsp);
#ifdef TARGET_ARM
    instrDesc* emitNewInstrReloc(emitAttr attr, BYTE* addr);
#endif
    instrDescJmp* emitNewInstrJmp();

    static ID_OPS GetFormatOp(insFormat format);

    bool emitIsScnsInsDsc(instrDesc* id);

    size_t emitSizeOfInsDsc(instrDesc* id);

    /************************************************************************/
    /*        The following keeps track of stack-based GC values            */
    /************************************************************************/

    unsigned          emitGCrFrameOffsCnt = 0;       // Number of       tracked stack ptr vars
    int               emitGCrFrameOffsMin = 0;       // Min offset of a tracked stack ptr var
    int               emitGCrFrameOffsMax = 0;       // Max offset of a tracked stack ptr var
    GCFrameLifetime** emitGCrFrameLiveTab = nullptr; // Cache of currently live varPtrs (stkOffs -> varPtrDsc)

#if defined(JIT32_GCENCODER) && !defined(FEATURE_EH_FUNCLETS)
    int emitSyncThisObjOffs = INT_MIN; // what is the offset of "this" for synchronized methods?
#endif

public:
    void emitSetFrameRangeGCRs(int offsLo, int offsHi);

    static instruction emitJumpKindToIns(emitJumpKind jumpKind);
    static emitJumpKind emitInsToJumpKind(instruction ins);
    static emitJumpKind emitReverseJumpKind(emitJumpKind jumpKind);

#ifdef DEBUG
    void emitInsSanityCheck(instrDesc* id);
#endif

#ifdef TARGET_ARMARCH
    // Returns true if instruction "id->idIns()" writes to a register that might be used to contain a GC
    // pointer. This exempts the SP and PC registers, and floating point registers. Memory access
    // instructions that pre- or post-increment their memory address registers are *not* considered to write
    // to GC registers, even if that memory address is a by-ref: such an instruction cannot change the GC
    // status of that register, since it must be a byref before and remains one after.
    //
    // This may return false positives.
    bool emitInsMayWriteToGCReg(instrDesc* id);

    // Returns true if the instruction may write to more than one register.
    bool emitInsMayWriteMultipleRegs(instrDesc* id);
#endif // TARGET_ARMARCH

    /************************************************************************/
    /*    The following is used to distinguish helper vs non-helper calls   */
    /************************************************************************/

    static bool emitNoGChelper(CorInfoHelpFunc helpFunc);
    static bool emitNoGChelper(CORINFO_METHOD_HANDLE methHnd);

    /************************************************************************/
    /*         The following logic keeps track of live GC ref values        */
    /************************************************************************/

    bool emitFullArgInfo; // full arg info (including non-ptr arg)?
    bool emitFullGCinfo;  // full GC pointer maps?
    bool emitFullyInt;    // fully interruptible code?

    regMaskTP emitGetGCRegsSavedOrModified(CORINFO_METHOD_HANDLE methHnd);

    // Gets a register mask that represent the kill set for a NoGC helper call.
    regMaskTP emitGetGCRegsKilledByNoGCCall(CorInfoHelpFunc helper);

#if !FEATURE_FIXED_OUT_ARGS
    bool emitSimpleStkUsed; // using the "simple" stack table?

    union {
        struct // if emitSimpleStkUsed==true
        {
#define BITS_IN_BYTE (8)
#define MAX_SIMPLE_STK_DEPTH (BITS_IN_BYTE * sizeof(unsigned))

            unsigned emitSimpleStkMask;      // bit per pushed dword (if it fits. Lowest bit <==> last pushed arg)
            unsigned emitSimpleByrefStkMask; // byref qualifier for emitSimpleStkMask
        } u1;

        struct // if emitSimpleStkUsed==false
        {
            BYTE   emitArgTrackLcl[16]; // small local table to avoid malloc
            BYTE*  emitArgTrackTab;     // base of the argument tracking stack
            BYTE*  emitArgTrackTop;     // top  of the argument tracking stack
            USHORT emitGcArgTrackCnt;   // count of pending arg records (stk-depth for frameless methods, gc ptrs on stk
                                        // for framed methods)
        } u2;
    };

    unsigned emitCntStackDepth;     // 0 in prolog/epilog, One DWORD elsewhere
    unsigned emitMaxStackDepth = 0; // actual computed max. stack depth
    unsigned emitCurStackLvl   = 0; // amount of bytes pushed on stack

    void emitStackPush(BYTE* addr, GCtype gcType);
    void emitStackPushN(BYTE* addr, unsigned count);
    void emitStackPushLargeStk(BYTE* addr, GCtype gcType, unsigned count);
    void emitStackKillArgs(BYTE* addr, unsigned count, unsigned callInstrSize);
    void emitStackPop(BYTE* addr, bool isCall, unsigned callInstrSize, unsigned count);
    void emitStackPopLargeStk(BYTE* addr, bool isCall, unsigned callInstrSize, unsigned count);
#endif // !FEATURE_FIXED_OUT_ARGS
    void emitRecordGCCallPop(BYTE* addr, unsigned callInstrSize);
    void emitRecordGCcall(BYTE* codePos, unsigned callInstrSize);

    /* Liveness of stack variables, and registers */

    void emitUpdateLiveGCvars(VARSET_VALARG_TP vars, BYTE* addr);
    void emitUpdateLiveGCregs(GCtype gcType, regMaskTP regs, BYTE* addr);

#ifdef DEBUG
    const char* emitGetFrameReg();
    void        emitDispVarSet();
    static void emitDispRegSet(regMaskTP regs);
    static void emitDispRegSetDiff(const char* name, regMaskTP from, regMaskTP to);
#endif

    void emitGCregLiveUpd(GCtype gcType, regNumber reg, BYTE* addr);
    void emitGCregLiveSet(GCtype gcType, regMaskTP mask, BYTE* addr, bool isThis);
    void emitGCregDeadUpdMask(regMaskTP, BYTE* addr);
    void emitGCregDeadUpd(regNumber reg, BYTE* addr);
    void emitGCregDeadSet(GCtype gcType, regMaskTP mask, BYTE* addr);

#if FEATURE_FIXED_OUT_ARGS
    void emitGCargLiveUpd(int offs, GCtype gcType, BYTE* addr DEBUGARG(unsigned lclNum));
#endif
    void emitGCvarLiveUpd(int slotOffs, GCtype gcType, BYTE* addr DEBUGARG(unsigned lclNum));
    void emitGCvarLiveSet(int slotOffs, GCtype gcType, unsigned codeOffs, unsigned index);
    void emitGCvarDeadSet(int slotOffs, unsigned codeOffs, unsigned index);

    GCtype emitRegGCtype(regNumber reg);

    // We have a mixture of code emission methods, some of which return the size of the emitted instruction,
    // requiring the caller to add this to the current code pointer (dst += <call to emit code>), others of which
    // return the updated code pointer (dst = <call to emit code>).  Sometimes we'd like to get the size of
    // the generated instruction for the latter style.  This method accomplishes that --
    // "emitCodeWithInstructionSize(dst, <call to emitCode>, &instrSize)" will do the call, and set
    // "*instrSize" to the after-before code pointer difference.  Returns the result of the call.  (And
    // asserts that the instruction size fits in an unsigned char.)
    static BYTE* emitCodeWithInstructionSize(BYTE* codePtrBefore, BYTE* newCodePointer, unsigned char* instrSize);

    /************************************************************************/
    /*      The following logic keeps track of initialized data sections    */
    /************************************************************************/

    /* One of these is allocated for every blob of initialized data */

    struct dataSection
    {
        // Note to use alignments greater than 32 requires modification in the VM
        // to support larger alignments (see ICorJitInfo::allocMem)
        //
        const static unsigned MIN_DATA_ALIGN = 4;
        const static unsigned MAX_DATA_ALIGN = 32;

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
#ifdef DEBUG
    void emitDispDataSec(dataSecDsc* section);
#endif

    void emitRecordRelocation(void* location,       /* IN */
                              void* target,         /* IN */
                              WORD  fRelocType,     /* IN */
                              WORD  slotNum   = 0,  /* IN */
                              INT32 addlDelta = 0); /* IN */

#ifdef TARGET_ARM
    void emitHandlePCRelativeMov32(void* location, /* IN */
                                   void* target);  /* IN */
#endif

    void emitRecordCallSite(ULONG                 instrOffset,   /* IN */
                            CORINFO_SIG_INFO*     callSig,       /* IN */
                            CORINFO_METHOD_HANDLE methodHandle); /* IN */

    // This is a scratch buffer used to minimize the number of sig info structs
    // we have to allocate for recordCallSite.
    INDEBUG(CORINFO_SIG_INFO* emitScratchSigInfo = nullptr;)

/************************************************************************/
/*               Logic to collect and display statistics                */
/************************************************************************/

#if EMITTER_STATS

    friend void emitterStats(FILE* fout);

    static size_t emitSizeMethod;

    static unsigned emitTotalInsCnt;

    static unsigned emitCurPrologInsCnt; // current number of prolog instrDescs
    static size_t   emitCurPrologIGSize; // current size of prolog instrDescs
    static unsigned emitMaxPrologInsCnt; // maximum number of prolog instrDescs
    static size_t   emitMaxPrologIGSize; // maximum size of prolog instrDescs

    static unsigned emitTotalIGcnt;   // total number of insGroup allocated
    static unsigned emitTotalPhIGcnt; // total number of insPlaceholderGroupData allocated
    static unsigned emitTotalIGicnt;
    static size_t   emitTotalIGsize;
    static unsigned emitTotalIGmcnt;   // total method count
    static unsigned emitTotalIGExtend; // total number of 'emitExtend' (typically overflow) groups
    static unsigned emitTotalIGjmps;
    static unsigned emitTotalIGptrs;

    static unsigned emitTotalIDescSmallCnt;
    static unsigned emitTotalIDescCnt;
    static unsigned emitTotalIDescJmpCnt;
#if !defined(TARGET_ARM64)
    static unsigned emitTotalIDescLblCnt;
#endif // !defined(TARGET_ARM64)
    static unsigned emitTotalIDescCnsCnt;
    static unsigned emitTotalIDescDspCnt;
    static unsigned emitTotalIDescCnsDspCnt;
#ifdef TARGET_XARCH
    static unsigned emitTotalIDescAmdCnt;
    static unsigned emitTotalIDescCnsAmdCnt;
#endif // TARGET_XARCH
    static unsigned emitTotalIDescCGCACnt;
#ifdef TARGET_ARM
    static unsigned emitTotalIDescRelocCnt;
#endif // TARGET_ARM

    static size_t emitTotMemAlloc;

    static unsigned emitSmallDspCnt;
    static unsigned emitLargeDspCnt;

    static unsigned emitSmallCnsCnt;
#define SMALL_CNS_TSZ 256
    static unsigned emitSmallCns[SMALL_CNS_TSZ];
    static unsigned emitLargeCnsCnt;
    static unsigned emitTotalDescAlignCnt;

    static unsigned emitIFcounts[IF_COUNT];

#endif // EMITTER_STATS

/*************************************************************************
 *
 *  Define any target-dependent emitter members.
 */

#include "emitdef.h"
};

/*****************************************************************************
 *
 *  Define any target-dependent inlines.
 */

#include "emitinl.h"

inline void emitter::instrDesc::checkSizes()
{
#ifdef DEBUG
    C_ASSERT(SMALL_IDSC_SIZE == (offsetof(instrDesc, _idDebugOnlyInfo) + sizeof(instrDescDebugInfo*)));
#endif
    C_ASSERT(SMALL_IDSC_SIZE == offsetof(instrDesc, _idAddrUnion));
}

/*****************************************************************************
 *
 *  Returns true if the given instruction descriptor is a "small
 *  constant" one (i.e. one of the descriptors that don't have all instrDesc
 *  fields allocated).
 */

inline bool emitter::emitIsScnsInsDsc(instrDesc* id)
{
    return id->idIsSmallDsc();
}

/*****************************************************************************
 *
 *  Given an instruction, return its "update mode" (RD/WR/RW).
 */

/*****************************************************************************
 *
 *  Return the number of epilog blocks generated so far.
 */

inline unsigned emitter::emitGetEpilogCnt()
{
    return emitEpilogCnt;
}

/*****************************************************************************
 *
 *  Return the current size of the specified data section.
 */

inline UNATIVE_OFFSET emitter::emitDataSize()
{
    return emitConsDsc.dsdOffs;
}

/*****************************************************************************
 *
 *  Return a handle to the current position in the output stream. This can
 *  be later converted to an actual code offset in bytes.
 */

inline void* emitter::emitCurBlock()
{
    return emitCurIG;
}

/*****************************************************************************
 *
 *  The emitCurOffset() method returns a cookie that identifies the current
 *  position in the instruction stream. Due to things like scheduling (and
 *  the fact that the final size of some instructions cannot be known until
 *  the end of code generation), we return a value with the instruction number
 *  and its estimated offset to the caller.
 */

inline unsigned emitGetInsNumFromCodePos(unsigned codePos)
{
    return (codePos & 0xFFFF);
}

inline unsigned emitGetInsOfsFromCodePos(unsigned codePos)
{
    return (codePos >> 16);
}

inline unsigned emitter::emitCurOffset()
{
    unsigned codePos = emitCurIGinsCnt + (emitCurIGsize << 16);

    assert(emitGetInsOfsFromCodePos(codePos) == emitCurIGsize);
    assert(emitGetInsNumFromCodePos(codePos) == emitCurIGinsCnt);

    // printf("[IG=%02u;ID=%03u;OF=%04X] => %08X\n", emitCurIG->igNum, emitCurIGinsCnt, emitCurIGsize, codePos);

    return codePos;
}

extern const unsigned short emitTypeSizes[TYP_COUNT];

template <class T>
inline emitAttr emitTypeSize(T type)
{
    assert(TypeGet(type) < TYP_COUNT);
    assert(emitTypeSizes[TypeGet(type)] > 0);
    return (emitAttr)emitTypeSizes[TypeGet(type)];
}

extern const unsigned short emitTypeActSz[TYP_COUNT];

template <class T>
inline emitAttr emitActualTypeSize(T type)
{
    assert(TypeGet(type) < TYP_COUNT);
    assert(emitTypeActSz[TypeGet(type)] > 0);
    return (emitAttr)emitTypeActSz[TypeGet(type)];
}

#ifdef TARGET_ARM64
insOpts emitSimdArrangementOpt(emitAttr size, var_types elementType);
#endif

/*****************************************************************************
 *
 *  Convert between an operand size in bytes and a smaller encoding used for
 *  storage in instruction descriptors.
 */

/* static */ inline emitter::opSize emitter::emitEncodeSize(emitAttr size)
{
    assert(size == EA_1BYTE || size == EA_2BYTE || size == EA_4BYTE || size == EA_8BYTE || size == EA_16BYTE ||
           size == EA_32BYTE);

    return emitSizeEncode[((int)size) - 1];
}

/* static */ inline emitAttr emitter::emitDecodeSize(emitter::opSize ensz)
{
    assert(((unsigned)ensz) < OPSZ_COUNT);

    return emitSizeDecode[ensz];
}

/*****************************************************************************
 *
 *  Little helpers to allocate various flavors of instructions.
 */

inline emitter::instrDesc* emitter::emitNewInstrSmall(emitAttr attr)
{
    instrDesc* id;

    id = (instrDesc*)emitAllocAnyInstr(SMALL_IDSC_SIZE, attr);
    id->idSetIsSmallDsc();

#if EMITTER_STATS
    emitTotalIDescSmallCnt++;
#endif // EMITTER_STATS

    return id;
}

inline emitter::instrDesc* emitter::emitNewInstr(emitAttr attr)
{
    // This is larger than the Small Descr
    return emitAllocInstr(attr);
}

inline emitter::instrDescJmp* emitter::emitNewInstrJmp()
{
    return emitAllocInstrJmp();
}

#if FEATURE_LOOP_ALIGN
inline emitter::instrDescAlign* emitter::emitNewInstrAlign()
{
    instrDescAlign* newInstr = emitAllocInstrAlign();
    newInstr->idIns(INS_align);
    return newInstr;
}
#endif

/*****************************************************************************
 *
 *  Allocate an instruction descriptor for an instruction with a constant operand.
 *  The instruction descriptor uses the idAddrUnion to save additional info
 *  so the smallest size that this can be is sizeof(instrDesc).
 *  Note that this very similar to emitter::emitNewInstrSC(), except it never
 *  allocates a small descriptor.
 */
inline emitter::instrDesc* emitter::emitNewInstrCns(emitAttr attr, cnsval_ssize_t cns)
{
    if (instrDesc::fitsInSmallCns(cns))
    {
        instrDesc* id = emitAllocInstr(attr);
        id->idSmallCns(cns);

#if EMITTER_STATS
        emitSmallCnsCnt++;
        if ((cns - ID_MIN_SMALL_CNS) >= (SMALL_CNS_TSZ - 1))
            emitSmallCns[SMALL_CNS_TSZ - 1]++;
        else
            emitSmallCns[cns - ID_MIN_SMALL_CNS]++;
#endif

        return id;
    }
    else
    {
        instrDescCns* id = emitAllocInstrCns(attr, cns);

#if EMITTER_STATS
        emitLargeCnsCnt++;
#endif

        return id;
    }
}

/*****************************************************************************
 *
 *  Get the instrDesc size, general purpose version
 *
 */

inline size_t emitter::emitGetInstrDescSize(const instrDesc* id)
{
    if (id->idIsSmallDsc())
    {
        return SMALL_IDSC_SIZE;
    }

    if (id->idIsLargeCns())
    {
        return sizeof(instrDescCns);
    }

    return sizeof(instrDesc);
}

/*****************************************************************************
 *
 *  Allocate an instruction descriptor for an instruction with a small integer
 *  constant operand. This is the same as emitNewInstrCns() except that here
 *  any constant that is small enough for instrDesc::fitsInSmallCns() only gets
 *  allocated SMALL_IDSC_SIZE bytes (and is thus a small descriptor, whereas
 *  emitNewInstrCns() always allocates at least sizeof(instrDesc)).
 */

inline emitter::instrDesc* emitter::emitNewInstrSC(emitAttr attr, cnsval_ssize_t cns)
{
    if (instrDesc::fitsInSmallCns(cns))
    {
        instrDesc* id = emitNewInstrSmall(attr);
        id->idSmallCns(cns);

#if EMITTER_STATS
        emitSmallCnsCnt++;
        if ((cns - ID_MIN_SMALL_CNS) >= (SMALL_CNS_TSZ - 1))
            emitSmallCns[SMALL_CNS_TSZ - 1]++;
        else
            emitSmallCns[cns - ID_MIN_SMALL_CNS]++;
#endif

        return id;
    }
    else
    {
        instrDescCns* id = emitAllocInstrCns(attr, cns);

#if EMITTER_STATS
        emitLargeCnsCnt++;
#endif

        return id;
    }
}

inline bool IsCodeAligned(UNATIVE_OFFSET offset)
{
    return ((offset & (CODE_ALIGN - 1)) == 0);
}

// Static:
inline BYTE* emitter::emitCodeWithInstructionSize(BYTE* codePtrBefore, BYTE* newCodePointer, unsigned char* instrSize)
{
    // DLD: Perhaps this method should return the instruction size, and we should do dst += <that size>
    // as is done in other cases?
    assert(newCodePointer >= codePtrBefore);
    ClrSafeInt<unsigned char> callInstrSizeSafe = ClrSafeInt<unsigned char>(newCodePointer - codePtrBefore);
    assert(!callInstrSizeSafe.IsOverflow());
    *instrSize = callInstrSizeSafe.Value();
    return newCodePointer;
}

inline void emitter::emitNewIG()
{
    emitGenIG(emitAllocAndLinkIG());
}

#if !defined(JIT32_GCENCODER)
// Start a new instruction group that is not interruptable
inline void emitter::emitDisableGC()
{
    emitNoGCIG = true;

    if (emitCurIGnonEmpty())
    {
        emitNxtIG(true);
    }
    else
    {
        emitCurIG->igFlags |= IGF_NOGCINTERRUPT;
    }
}

// Start a new instruction group that is interruptable
inline void emitter::emitEnableGC()
{
    emitNoGCIG = false;

    // The next time an instruction needs to be generated, force a new instruction group.
    // It will be an emitAdd group in that case. Note that the next thing we see might be
    // a label, which will force a non-emitAdd group.
    //
    // Note that we can't just create a new instruction group here, because we don't know
    // if there are going to be any instructions added to it, and we don't support empty
    // instruction groups.
    emitForceNewIG = true;
}
#endif // !defined(JIT32_GCENCODER)

/*****************************************************************************/
#endif // _EMIT_H_
/*****************************************************************************/
