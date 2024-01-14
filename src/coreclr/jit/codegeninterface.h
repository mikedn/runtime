// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

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

protected:
    emitter* m_cgEmitter;

public:
    Compiler*     compiler;
    ParamRegState paramRegState;
    SpillTempSet  spillTemps;
    // Callee saved registers that are modified by the compiled method, and thus
    // need to be saved in prolog and restored in epilog.
    // These are registers that have been allocated by LSRA and registers used in
    // prolog for various purposes (e.g. block initialization), without registers
    // having special uses (frame/stack pointer, link register on ARM64) that too
    // have to be saved and restored.
    regMaskTP calleeSavedModifiedRegs = RBM_NONE;
#ifdef TARGET_ARM
    // Registers that are spilled at the start of the prolog, right below stack params,
    // such that they form a contiguous area useful to handle varargs and split params
    // but also to take advantage of ARM's multi reg push instruction. Normally these
    // are registers allocated to parameters but some unused registers might have to be
    // spilled to maintain alignment.
    regMaskTP preSpillParamRegs = RBM_NONE;
    regMaskTP preSpillAlignRegs = RBM_NONE;
#endif
    // The following keeps track of how many bytes of local frame space we've
    // grabbed so far in the current function, and how many argument bytes we
    // need to pop when we return.
    unsigned lclFrameSize;    // secObject + lclBlk + locals + temps
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
#ifdef UNIX_AMD64_ABI
    // This flag  is indicating if there is a need to align the frame.
    // On AMD64-Windows, if there are calls, 4 slots for the outgoing ars are allocated,
    // except for FastTailCall. This slots makes the frame size non-zero, so alignment
    // logic will be called. On unix-x64, there are no such slots. There is a possibility
    // to have calls in the method with frame size of 0. The frame alignment logic won't
    // kick in. This flags takes care of the unix-x64 case by remembering that there are
    // calls and making sure the frame alignment logic is executed.
    bool needToAlignFrame = false;
#endif

protected:
#ifdef TARGET_XARCH
    bool contains256bitAVXInstructions = false;
    bool containsAVXInstructions       = false;
#endif
#ifdef TARGET_ARM64
    bool genSaveFpLrWithAllCalleeSavedRegisters = false;
#endif

private:
    bool m_cgFramePointerUsed = false;
    bool m_cgInterruptible    = false;
#if DOUBLE_ALIGN
    // The following property indicates whether we going to double-align the frame.
    // Arguments are accessed relative to the Frame Pointer (EBP), and locals are
    // accessed relative to the Stack Pointer (ESP).
    bool m_cgDoubleAlign = false;
#endif
#ifdef TARGET_ARMARCH
    bool m_cgHasTailCalls = false;
#endif
#ifdef LATE_DISASM
    DisAssembler m_cgDisAsm;
#endif

protected:
    CodeGenInterface(Compiler* compiler);

public:
    void genGenerateCode(void** nativeCode, uint32_t* nativeCodeSize);

    Compiler* GetCompiler() const
    {
        return compiler;
    }

#ifdef TARGET_ARM
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

    bool isFramePointerUsed() const
    {
        return m_cgFramePointerUsed;
    }

    void setFramePointerUsed()
    {
        m_cgFramePointerUsed = true;
    }

    bool IsFramePointerRequired() const
    {
        return isFramePointerUsed()
#if DOUBLE_ALIGN
               || doDoubleAlign()
#endif
            ;
    }

    bool GetInterruptible() const
    {
        return m_cgInterruptible;
    }

    void SetInterruptible(bool value)
    {
        m_cgInterruptible = value;
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

    void SetUseVEXEncoding(bool value);
#endif

    int genCallerSPtoFPdelta() const;
    int genCallerSPtoInitialSPdelta() const;
    int genSPtoFPdelta() const;
    int genTotalFrameSize() const;

#ifdef TARGET_ARM64
    void SetSaveFpLrWithAllCalleeSavedRegisters(bool value);
    bool IsSaveFpLrWithAllCalleeSavedRegisters() const;
#endif

#ifdef TARGET_ARMARCH
    bool GetHasTailCalls() const
    {
        return m_cgHasTailCalls;
    }

    void SetHasTailCalls(bool value)
    {
        m_cgHasTailCalls = value;
    }
#endif

#if DOUBLE_ALIGN
    bool doDoubleAlign() const
    {
        return m_cgDoubleAlign;
    }

    void setDoubleAlign()
    {
        m_cgDoubleAlign = true;
    }
#endif

    emitter* GetEmitter() const
    {
        return m_cgEmitter;
    }

    unsigned GetHotCodeSize() const;
    unsigned GetColdCodeSize() const;
    unsigned GetCodeSize() const;
#ifdef JIT32_GCENCODER
    unsigned GetGCInfoSize() const;
#endif

#if defined(DEBUG) || defined(LATE_DISASM)
    double GetPerfScore() const;
#endif

#ifdef LATE_DISASM
    const char* siRegVarName(size_t offs, size_t size, unsigned reg);
    const char* siStackVarName(size_t offs, size_t size, unsigned reg, unsigned stkOffs);

    DisAssembler& getDisAssembler()
    {
        return m_cgDisAsm;
    }
#endif

    static bool            UseOptimizedWriteBarriers();
    static CorInfoHelpFunc GetWriteBarrierHelperCall(GCInfo::WriteBarrierForm wbf);
};

StructStoreKind GetStructStoreKind(bool isLocalStore, ClassLayout* layout, GenTree* src);
