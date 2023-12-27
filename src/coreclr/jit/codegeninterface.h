// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#ifndef _CODEGEN_INTERFACE_H_
#define _CODEGEN_INTERFACE_H_

#include "regset.h"
#include "instr.h"
#include "jitgcinfo.h"

class LclVarDsc;
class emitter;

struct ParamRegState
{
    unsigned  intRegCount    = 0;
    unsigned  floatRegCount  = 0;
    regMaskTP intRegLiveIn   = RBM_NONE;
    regMaskTP floatRegLiveIn = RBM_NONE;
};

class CodeGenInterface
{
    friend class emitter;

public:
    CodeGenInterface(Compiler* compiler);
    virtual void genGenerateCode(void** codePtr, uint32_t* nativeSizeOfCode) = 0;

    Compiler* GetCompiler() const
    {
        return compiler;
    }

    bool ShouldAlignLoops()
    {
        return m_genAlignLoops;
    }

    void SetAlignLoops(bool value)
    {
        m_genAlignLoops = value;
    }

    Compiler* compiler;

#ifdef JIT32_GCENCODER
    size_t compInfoBlkSize;
#endif

    ParamRegState paramRegState;
    SpillTempSet  spillTemps;

#ifdef TARGET_ARM
    // Registers that are spilled at the start of the prolog, right below stack params,
    // such that they form a contiguous area useful to handle varargs and split params
    // but also to take advantage of ARM's multi reg push instruction. Normally these
    // are registers allocated to parameters but some unused registers might have to be
    // spilled to maintain alignment.
    regMaskTP preSpillParamRegs = RBM_NONE;
    regMaskTP preSpillAlignRegs = RBM_NONE;

    regMaskTP GetPreSpillRegs() const
    {
        return preSpillParamRegs | preSpillAlignRegs;
    }

    unsigned GetPreSpillRegCount() const
    {
        return genCountBits(preSpillParamRegs | preSpillAlignRegs);
    }

    unsigned GetPreSpillSize() const
    {
        return GetPreSpillRegCount() * REGSIZE_BYTES;
    }
#endif

    //  The following keeps track of how many bytes of local frame space we've
    //  grabbed so far in the current function, and how many argument bytes we
    //  need to pop when we return.
    unsigned lclFrameSize; // secObject + lclBlk + locals + temps

    unsigned paramsStackSize; // total size of parameters passed in stack

#if FEATURE_FIXED_OUT_ARGS
    PhasedVar<unsigned> outgoingArgSpaceSize; // size of fixed outgoing argument space
#endif

    // For CORINFO_CALLCONV_PARAMTYPE and if generic context is passed as THIS pointer
    int cachedGenericContextArgOffset;

    // Count of callee-saved regs we pushed in the prolog.
    // Does not include EBP for isFramePointerUsed() and double-aligned frames.
    // In case of Amd64 this doesn't include float regs saved on stack.
    unsigned calleeRegsPushed = UINT_MAX;

    // Callee saved registers that are modified by the compiled method, and thus
    // need to be saved in prolog and restored in epilog.
    // These are registers that have been allocated by LSRA and registers used in
    // prolog for various purposes (e.g. block initialization), without registers
    // having special uses (frame/stack pointer, link register on ARM64) that too
    // have to be saved and restored.
    regMaskTP calleeSavedModifiedRegs = RBM_NONE;

#ifdef UNIX_AMD64_ABI
    // This flag  is indicating if there is a need to align the frame.
    // On AMD64-Windows, if there are calls, 4 slots for the outgoing ars are allocated, except for
    // FastTailCall. This slots makes the frame size non-zero, so alignment logic will be called.
    // On AMD64-Unix, there are no such slots. There is a possibility to have calls in the method with frame size of
    // 0. The frame alignment logic won't kick in. This flags takes care of the AMD64-Unix case by remembering that
    // there are calls and making sure the frame alignment logic is executed.
    bool needToAlignFrame = false;
#endif

    bool generatingProlog = false;
    bool generatingEpilog = false;

protected:
#ifdef TARGET_XARCH
    bool contains256bitAVXInstructions = false;
    bool containsAVXInstructions       = false;
#endif
    bool m_genAlignLoops;

public:
    static bool            UseOptimizedWriteBarriers();
    static CorInfoHelpFunc GetWriteBarrierHelperCall(GCInfo::WriteBarrierForm wbf);

private:
    bool m_cgFramePointerUsed = false;

public:
    bool isFramePointerUsed() const
    {
        return m_cgFramePointerUsed;
    }

    void setFramePointerUsed()
    {
        m_cgFramePointerUsed = true;
    }

#ifdef TARGET_XARCH
    void SetContainsAVX()
    {
        containsAVXInstructions = true;
    }

    void SetContains256bitAVX()
    {
        contains256bitAVXInstructions = true;
    }
#endif

public:
    int genCallerSPtoFPdelta() const;
    int genCallerSPtoInitialSPdelta() const;
    int genSPtoFPdelta() const;
    int genTotalFrameSize() const;

#ifdef TARGET_ARM64
    void SetSaveFpLrWithAllCalleeSavedRegisters(bool value);
    bool IsSaveFpLrWithAllCalleeSavedRegisters() const;
    bool genSaveFpLrWithAllCalleeSavedRegisters = false;
#endif

#if DOUBLE_ALIGN
    // The following property indicates whether we going to double-align the frame.
    // Arguments are accessed relative to the Frame Pointer (EBP), and locals are
    // accessed relative to the Stack Pointer (ESP).
private:
    bool m_cgDoubleAlign = false;

public:
    bool doDoubleAlign() const
    {
        return m_cgDoubleAlign;
    }

    void setDoubleAlign()
    {
        m_cgDoubleAlign = true;
    }
#endif

    bool IsFramePointerRequired() const
    {
        return isFramePointerUsed()
#if DOUBLE_ALIGN
               || doDoubleAlign()
#endif
            ;
    }

    emitter* GetEmitter() const
    {
        return m_cgEmitter;
    }

#ifdef TARGET_XARCH
    void SetUseVEXEncoding(bool value);
#endif

protected:
    emitter* m_cgEmitter;

#ifdef LATE_DISASM
public:
    DisAssembler& getDisAssembler()
    {
        return m_cgDisAsm;
    }

protected:
    DisAssembler m_cgDisAsm;
#endif // LATE_DISASM

public:
#ifdef DEBUG
    void setVerbose()
    {
        verbose = true;
    }

    bool verbose = false;
#endif // DEBUG

public:
    bool GetInterruptible() const
    {
        return m_cgInterruptible;
    }

    void SetInterruptible(bool value)
    {
        m_cgInterruptible = value;
    }

#ifdef TARGET_ARMARCH
    bool GetHasTailCalls() const
    {
        return m_cgHasTailCalls;
    }

    void SetHasTailCalls(bool value)
    {
        m_cgHasTailCalls = value;
    }
#endif // TARGET_ARMARCH

private:
    bool m_cgInterruptible = false;
#ifdef TARGET_ARMARCH
    bool m_cgHasTailCalls = false;
#endif

public:
    // These are the different addressing modes used to access a local var.
    // The JIT has to report the location of the locals back to the EE
    // for debugging purposes.
    enum siVarLocType
    {
        VLT_REG,
        VLT_REG_BYREF, // this type is currently only used for value types on X64
        VLT_REG_FP,
        VLT_STK,
        VLT_STK_BYREF, // this type is currently only used for value types on X64
        VLT_REG_REG,
        VLT_REG_STK,
        VLT_STK_REG,
        VLT_STK2,
        VLT_FPSTK,
        VLT_FIXED_VA,

        VLT_COUNT,
        VLT_INVALID
    };

    struct siVarLoc
    {
        siVarLocType vlType;

        union {
            // VLT_REG/VLT_REG_FP -- Any pointer-sized enregistered value (TYP_INT, TYP_REF, etc)
            // eg. EAX
            // VLT_REG_BYREF -- the specified register contains the address of the variable
            // eg. [EAX]

            struct
            {
                regNumber vlrReg;
            } vlReg;

            // VLT_STK       -- Any 32 bit value which is on the stack
            // eg. [ESP+0x20], or [EBP-0x28]
            // VLT_STK_BYREF -- the specified stack location contains the address of the variable
            // eg. mov EAX, [ESP+0x20]; [EAX]

            struct
            {
                regNumber vlsBaseReg;
                int32_t   vlsOffset;
            } vlStk;

            // VLT_REG_REG -- TYP_LONG/TYP_DOUBLE with both DWords enregistered
            // eg. RBM_EAXEDX

            struct
            {
                regNumber vlrrReg1;
                regNumber vlrrReg2;
            } vlRegReg;

            // VLT_REG_STK -- Partly enregistered TYP_LONG/TYP_DOUBLE
            // eg { LowerDWord=EAX UpperDWord=[ESP+0x8] }

            struct
            {
                regNumber vlrsReg;

                struct
                {
                    regNumber vlrssBaseReg;
                    int32_t   vlrssOffset;
                } vlrsStk;
            } vlRegStk;

            // VLT_STK_REG -- Partly enregistered TYP_LONG/TYP_DOUBLE
            // eg { LowerDWord=[ESP+0x8] UpperDWord=EAX }

            struct
            {
                struct
                {
                    regNumber vlsrsBaseReg;
                    int32_t   vlsrsOffset;
                } vlsrStk;

                regNumber vlsrReg;
            } vlStkReg;

            // VLT_STK2 -- Any 64 bit value which is on the stack, in 2 successive DWords
            // eg 2 DWords at [ESP+0x10]

            struct
            {
                regNumber vls2BaseReg;
                int32_t   vls2Offset;
            } vlStk2;

            // VLT_FPSTK -- enregisterd TYP_DOUBLE (on the FP stack)
            // eg. ST(3). Actually it is ST("FPstkHeight - vpFpStk")

            struct
            {
                unsigned vlfReg;
            } vlFPstk;

            // VLT_FIXED_VA -- fixed argument of a varargs function.
            // The argument location depends on the size of the variable
            // arguments (...). Inspecting the VARARGS_HANDLE indicates the
            // location of the first arg. This argument can then be accessed
            // relative to the position of the first arg

            struct
            {
                unsigned vlfvOffset;
            } vlFixedVarArg;

            // VLT_MEMORY

            struct
            {
                void* rpValue; // pointer to the in-process location of the value.
            } vlMemory;
        };

#ifdef LATE_DISASM
        bool vlIsInReg(regNumber reg) const;
        bool vlIsOnStack(regNumber reg, int32_t offset) const;
#endif

        void storeVariableInRegisters(regNumber reg1, regNumber reg2);
        void storeVariableOnStack(regNumber stackBaseReg, int32_t stackOffset);

        siVarLoc(const LclVarDsc* lcl, regNumber baseReg, int offset, bool isFramePointerUsed);
        siVarLoc(){};

        static bool Equals(const siVarLoc& x, const siVarLoc& y);

        INDEBUG(void Dump() const;)

    private:
        void siFillRegisterVarLoc(
            const LclVarDsc* lcl, var_types type, regNumber baseReg, int offset, bool isFramePointerUsed);

        void siFillStackVarLoc(
            const LclVarDsc* lcl, var_types type, regNumber baseReg, int offset, bool isFramePointerUsed);
    };

public:
    siVarLoc getSiVarLoc(const LclVarDsc* lcl) const;

#if !FEATURE_FIXED_OUT_ARGS
    unsigned getCurrentStackLevel() const;

protected:
    //  Keeps track of how many bytes we've pushed on the processor's stack.
    unsigned genStackLevel = 0;
#endif

#ifdef LATE_DISASM
public:
    virtual const char* siRegVarName(size_t offs, size_t size, unsigned reg) = 0;
    virtual const char* siStackVarName(size_t offs, size_t size, unsigned reg, unsigned stkOffs) = 0;
#endif
};

StructStoreKind GetStructStoreKind(bool isLocalStore, ClassLayout* layout, GenTree* src);

#endif // _CODEGEN_INTERFACE_H_
