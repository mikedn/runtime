// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

//
// This file declares the types that constitute the interface between the
// code generator (CodeGen class) and the rest of the JIT.
//
// RegState
//
// CodeGenInterface includes only the public methods that are called by
// the Compiler.
//
// CodeGenContext contains the shared context between the code generator
// and other phases of the JIT, especially the register allocator and
// GC encoder.  It is distinct from CodeGenInterface so that it can be
// included in the Compiler object, and avoid an extra indirection when
// accessed from members of Compiler.
//

#ifndef _CODEGEN_INTERFACE_H_
#define _CODEGEN_INTERFACE_H_

#include "regset.h"
#include "jitgcinfo.h"
#include "emit.h"

class LclVarDsc;

#if 0
// Enable USING_SCOPE_INFO flag to use psiScope/siScope info to report variables' locations.
#define USING_SCOPE_INFO
#endif
#if 1
// Enable USING_VARIABLE_LIVE_RANGE flag to use VariableLiveRange info to report variables' locations.
// Note: if both USING_SCOPE_INFO and USING_VARIABLE_LIVE_RANGE are defined, then USING_SCOPE_INFO
// information is reported to the debugger.
#define USING_VARIABLE_LIVE_RANGE
#endif

class emitter;

struct ParamRegState
{
    unsigned  intRegCount    = 0;
    unsigned  floatRegCount  = 0;
    regMaskTP intRegLiveIn   = RBM_NONE;
    regMaskTP floatRegLiveIn = RBM_NONE;
};

//-------------------- CodeGenInterface ---------------------------------
// interface to hide the full CodeGen implementation from rest of Compiler

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

    //-------------------------------------------------------------------------
    //  The following property indicates whether to align loops.
    //  (Used to avoid effects of loop alignment when diagnosing perf issues.)

    bool ShouldAlignLoops()
    {
        return m_genAlignLoops;
    }
    void SetAlignLoops(bool value)
    {
        m_genAlignLoops = value;
    }

    Compiler* compiler;

    //  The following is used to create the 'method JIT info' block.
    size_t compInfoBlkSize;
    BYTE*  compInfoBlkAddr;

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

    //-------------------------------------------------------------------------
    //  The following keeps track of how many bytes of local frame space we've
    //  grabbed so far in the current function, and how many argument bytes we
    //  need to pop when we return.
    //

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
    unsigned calleeRegsPushed;

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

    FrameType rpFrameType;

    bool generatingProlog = false;
    bool generatingEpilog = false;

protected:
    bool m_genAlignLoops;

public:
    static bool            UseOptimizedWriteBarriers();
    static CorInfoHelpFunc GetWriteBarrierHelperCall(GCInfo::WriteBarrierForm wbf);

    // The following property indicates whether the current method sets up
    // an explicit stack frame or not.
private:
    PhasedVar<bool> m_cgFramePointerUsed;

public:
    bool isFramePointerUsed() const
    {
        return m_cgFramePointerUsed;
    }
    void setFramePointerUsed(bool value)
    {
        m_cgFramePointerUsed = value;
    }

    // The following property indicates whether the current method requires
    // an explicit frame. Does not prohibit double alignment of the stack.
private:
    PhasedVar<bool> m_cgFrameRequired{false};

public:
    bool isFrameRequired() const
    {
        return m_cgFrameRequired;
    }
    void setFrameRequired(bool value)
    {
        m_cgFrameRequired = value;
    }

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

    // If both isFramePointerRequired() and isFrameRequired() are false, the method is eligible
    // for Frame-Pointer-Omission (FPO).

    // The following property indicates whether the current method requires
    // an explicit stack frame, and all arguments and locals to be
    // accessible relative to the Frame Pointer. Prohibits double alignment
    // of the stack.
private:
    PhasedVar<bool> m_cgFramePointerRequired{false};

public:
    bool isFramePointerRequired() const
    {
        return m_cgFramePointerRequired;
    }

    void setFramePointerRequired(bool value)
    {
        m_cgFramePointerRequired = value;
    }

    //------------------------------------------------------------------------
    // resetWritePhaseForFramePointerRequired: Return m_cgFramePointerRequired into the write phase.
    // It is used only before the first phase, that locks this value, currently it is LSRA.
    // Use it if you want to skip checks that set this value to true if the value is already true.
    void resetWritePhaseForFramePointerRequired()
    {
        m_cgFramePointerRequired.ResetWritePhase();
    }

#if DOUBLE_ALIGN
    // The following property indicates whether we going to double-align the frame.
    // Arguments are accessed relative to the Frame Pointer (EBP), and
    // locals are accessed relative to the Stack Pointer (ESP).
public:
    bool doDoubleAlign() const
    {
        return m_cgDoubleAlign;
    }
    void setDoubleAlign(bool value)
    {
        m_cgDoubleAlign = value;
    }
    bool doubleAlignOrFramePointerUsed() const
    {
        return isFramePointerUsed() || doDoubleAlign();
    }

private:
    bool m_cgDoubleAlign;
#else  // !DOUBLE_ALIGN

public:
    bool doubleAlignOrFramePointerUsed() const
    {
        return isFramePointerUsed();
    }
#endif // !DOUBLE_ALIGN

public:
    bool validImmForBL(ssize_t addr);

public:
    emitter* GetEmitter() const
    {
        return m_cgEmitter;
    }

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

    // The following is set to true if we've determined that the current method
    // is to be fully interruptible.
    //
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
#endif // TARGET_ARMARCH

    //  The following will be set to true if we've determined that we need to
    //  generate a full-blown pointer register map for the current method.
    //  Currently it is equal to (GetInterruptible() || !isFramePointerUsed())
    //  (i.e. We generate the full-blown map for EBP-less methods and
    //        for fully interruptible methods)
    //
public:
    bool IsFullPtrRegMapRequired() const
    {
        return m_cgFullPtrRegMap;
    }

protected:
    bool m_cgFullPtrRegMap;

public:
    /* These are the different addressing modes used to access a local var.
     * The JIT has to report the location of the locals back to the EE
     * for debugging purposes.
     */

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
                regNumber     vlsBaseReg;
                NATIVE_OFFSET vlsOffset;
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
                    regNumber     vlrssBaseReg;
                    NATIVE_OFFSET vlrssOffset;
                } vlrsStk;
            } vlRegStk;

            // VLT_STK_REG -- Partly enregistered TYP_LONG/TYP_DOUBLE
            // eg { LowerDWord=[ESP+0x8] UpperDWord=EAX }

            struct
            {
                struct
                {
                    regNumber     vlsrsBaseReg;
                    NATIVE_OFFSET vlsrsOffset;
                } vlsrStk;

                regNumber vlsrReg;
            } vlStkReg;

            // VLT_STK2 -- Any 64 bit value which is on the stack, in 2 successive DWords
            // eg 2 DWords at [ESP+0x10]

            struct
            {
                regNumber     vls2BaseReg;
                NATIVE_OFFSET vls2Offset;
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
                void* rpValue; // pointer to the in-process
                               // location of the value.
            } vlMemory;
        };

        // Helper functions

        bool vlIsInReg(regNumber reg) const;
        bool vlIsOnStack(regNumber reg, signed offset) const;
        bool vlIsOnStack() const;

        void storeVariableInRegisters(regNumber reg, regNumber otherReg);
        void storeVariableOnStack(regNumber stackBaseReg, NATIVE_OFFSET variableStackOffset);

        siVarLoc(const LclVarDsc* varDsc, regNumber baseReg, int offset, bool isFramePointerUsed);
        siVarLoc(){};

        // An overload for the equality comparator
        static bool Equals(const siVarLoc* lhs, const siVarLoc* rhs);

    private:
        // Fill "siVarLoc" properties indicating the register position of the variable
        // using "LclVarDsc" and "baseReg"/"offset" if it has a part in the stack (x64 bit float or long).
        void siFillRegisterVarLoc(
            const LclVarDsc* varDsc, var_types type, regNumber baseReg, int offset, bool isFramePointerUsed);

        // Fill "siVarLoc" properties indicating the register position of the variable
        // using "LclVarDsc" and "baseReg"/"offset" if it is a variable with part in a register and
        // part in thestack
        void siFillStackVarLoc(
            const LclVarDsc* varDsc, var_types type, regNumber baseReg, int offset, bool isFramePointerUsed);
    };

public:
    siVarLoc getSiVarLoc(const LclVarDsc* varDsc
#if !FEATURE_FIXED_OUT_ARGS
                         ,
                         unsigned stackLevel
#endif
                         ) const;

#ifdef DEBUG
    void dumpSiVarLoc(const siVarLoc* varLoc) const;
#endif

#if !FEATURE_FIXED_OUT_ARGS
    unsigned getCurrentStackLevel() const;

protected:
    //  Keeps track of how many bytes we've pushed on the processor's stack.
    unsigned genStackLevel = 0;
#endif

public:
#ifdef USING_VARIABLE_LIVE_RANGE
    //--------------------------------------------
    //
    // VariableLiveKeeper: Holds an array of "VariableLiveDescriptor", one for each variable
    //  whose location we track. It provides start/end/update/count operations over the
    //  "LiveRangeList" of any variable.
    //
    // Notes:
    //  This method could be implemented on Compiler class too, but the intention is to move code
    //  out of that class, which is huge. With this solution the only code needed in Compiler is
    //  a getter and an initializer of this class.
    //  The index of each variable in this array corresponds to the one in "compiler->lvaTable".
    //  We care about tracking the variable locations of arguments, special arguments, and local IL
    //  variables, and we ignore any other variable (like JIT temporary variables).
    //
    class VariableLiveKeeper
    {
    public:
        //--------------------------------------------
        //
        // VariableLiveRange: Represent part of the life of a variable. A
        //      variable lives in a location (represented with struct "siVarLoc")
        //      between two native offsets.
        //
        // Notes:
        //    We use emitLocation and not NATTIVE_OFFSET because location
        //    is captured when code is being generated (genCodeForBBList
        //    and genGeneratePrologsAndEpilogs) but only after the whole
        //    method's code is generated can we obtain a final, fixed
        //    NATIVE_OFFSET representing the actual generated code offset.
        //    There is also a IL_OFFSET, but this is more accurate and the
        //    debugger is expecting assembly offsets.
        //    This class doesn't have behaviour attached to itself, it is
        //    just putting a name to a representation. It is used to build
        //    typedefs LiveRangeList and LiveRangeListIterator, which are
        //    basically a list of this class and a const_iterator of that
        //    list.
        //
        class VariableLiveRange
        {
        public:
            emitLocation               m_StartEmitLocation; // first position from where "m_VarLocation" becomes valid
            emitLocation               m_EndEmitLocation;   // last position where "m_VarLocation" is valid
            CodeGenInterface::siVarLoc m_VarLocation;       // variable location

            VariableLiveRange(CodeGenInterface::siVarLoc varLocation,
                              emitLocation               startEmitLocation,
                              emitLocation               endEmitLocation)
                : m_StartEmitLocation(startEmitLocation), m_EndEmitLocation(endEmitLocation), m_VarLocation(varLocation)
            {
            }

#ifdef DEBUG
            // Dump "VariableLiveRange" when code has not been generated. We don't have the native code offset,
            // but we do have "emitLocation"s and "siVarLoc".
            void dumpVariableLiveRange(const CodeGenInterface* codeGen) const;

            // Dump "VariableLiveRange" when code has been generated and we have the native code offset of each
            // "emitLocation"
            void dumpVariableLiveRange(emitter* emit, const CodeGenInterface* codeGen) const;
#endif // DEBUG
        };

        typedef jitstd::list<VariableLiveRange> LiveRangeList;
        typedef LiveRangeList::const_iterator   LiveRangeListIterator;

    private:
#ifdef DEBUG
        //--------------------------------------------
        //
        // LiveRangeDumper: Used for debugging purposes during code
        //  generation on genCodeForBBList. Keeps an iterator to the first
        //  edited/added "VariableLiveRange" of a variable during the
        //  generation of code of one block.
        //
        // Notes:
        //  The first "VariableLiveRange" reported for a variable during
        //  a BasicBlock is sent to "setDumperStartAt" so we can dump all
        //  the "VariableLiveRange"s from that one.
        //  After we dump all the "VariableLiveRange"s we call "reset" with
        //  the "liveRangeList" to set the barrier to nullptr or the last
        //  "VariableLiveRange" if it is opened.
        //  If no "VariableLiveRange" was edited/added during block,
        //  the iterator points to the end of variable's LiveRangeList.
        //
        class LiveRangeDumper
        {
            // Iterator to the first edited/added position during actual block code generation. If last
            // block had a closed "VariableLiveRange" (with a valid "m_EndEmitLocation") and not changes
            // were applied to variable liveness, it points to the end of variable's LiveRangeList.
            LiveRangeListIterator m_StartingLiveRange;
            bool                  m_hasLiveRangestoDump; // True if a live range for this variable has been
                                                         // reported from last call to EndBlock

        public:
            LiveRangeDumper(const LiveRangeList* liveRanges)
                : m_StartingLiveRange(liveRanges->end()), m_hasLiveRangestoDump(false){};

            // Make the dumper point to the last "VariableLiveRange" opened or nullptr if all are closed
            void resetDumper(const LiveRangeList* list);

            // Make "LiveRangeDumper" instance points the last "VariableLiveRange" added so we can
            // start dumping from there after the actual "BasicBlock"s code is generated.
            void setDumperStartAt(const LiveRangeListIterator liveRangeIt);

            // Return an iterator to the first "VariableLiveRange" edited/added during the current
            // "BasicBlock"
            LiveRangeListIterator getStartForDump() const;

            // Return whether at least a "VariableLiveRange" was alive during the current "BasicBlock"'s
            // code generation
            bool hasLiveRangesToDump() const;
        };
#endif // DEBUG

        //--------------------------------------------
        //
        // VariableLiveDescriptor: This class persist and update all the changes
        //  to the home of a variable. It has an instance of "LiveRangeList"
        //  and methods to report the start/end of a VariableLiveRange.
        //
        class VariableLiveDescriptor
        {
            LiveRangeList* m_VariableLiveRanges; // the variable locations of this variable
            INDEBUG(LiveRangeDumper* m_VariableLifeBarrier);

        public:
            VariableLiveDescriptor(CompAllocator allocator);

            bool           hasVariableLiveRangeOpen() const;
            LiveRangeList* getLiveRanges() const;

            void startLiveRangeFromEmitter(CodeGenInterface::siVarLoc varLocation, emitter* emit) const;
            void endLiveRangeAtEmitter(emitter* emit) const;
            void updateLiveRangeAtEmitter(CodeGenInterface::siVarLoc varLocation, emitter* emit) const;

#ifdef DEBUG
            void dumpAllRegisterLiveRangesForBlock(emitter* emit, const CodeGenInterface* codeGen) const;
            void dumpRegisterLiveRangesForBlockBeforeCodeGenerated(const CodeGenInterface* codeGen) const;
            bool hasVarLiveRangesToDump() const;
            bool hasVarLiveRangesFromLastBlockToDump() const;
            void endBlockLiveRanges();
#endif // DEBUG
        };

        unsigned int m_LiveDscCount;  // count of args, special args, and IL local variables to report home
        unsigned int m_LiveArgsCount; // count of arguments to report home

        Compiler* m_Compiler;

        VariableLiveDescriptor* m_vlrLiveDsc; // Array of descriptors that manage VariableLiveRanges.
                                              // Its indices correspond to lvaTable indexes.

        VariableLiveDescriptor* m_vlrLiveDscForProlog; // Array of descriptors that manage VariableLiveRanges.
                                                       // Its indices correspond to lvaTable indexes.

        bool m_LastBasicBlockHasBeenEmited; // When true no more siEndVariableLiveRange is considered.
                                            // No update/start happens when code has been generated.

    public:
        VariableLiveKeeper(unsigned int  totalLocalCount,
                           unsigned int  argsCount,
                           Compiler*     compiler,
                           CompAllocator allocator);

        // For tracking locations during code generation
        void siStartOrCloseVariableLiveRange(const LclVarDsc* varDsc, unsigned int varNum, bool isBorn, bool isDying);
        void siStartVariableLiveRange(const LclVarDsc* varDsc, unsigned int varNum);
        void siEndVariableLiveRange(unsigned int varNum);
        void siUpdateVariableLiveRange(const LclVarDsc* varDsc, unsigned int varNum);
        void siEndAllVariableLiveRange(VARSET_VALARG_TP varsToClose);
        void siEndAllVariableLiveRange();

        LiveRangeList* getLiveRangesForVarForBody(unsigned int varNum) const;
        LiveRangeList* getLiveRangesForVarForProlog(unsigned int varNum) const;
        size_t getLiveRangesCount() const;

        // For parameters locations on prolog
        void psiStartVariableLiveRange(CodeGenInterface::siVarLoc varLocation, unsigned int varNum);
        void psiClosePrologVariableRanges();

#ifdef DEBUG
        void dumpBlockVariableLiveRanges(const BasicBlock* block);
        void dumpLvaVariableLiveRanges() const;
#endif // DEBUG
    };

    void initializeVariableLiveKeeper();

    VariableLiveKeeper* getVariableLiveKeeper() const;

protected:
    VariableLiveKeeper* varLiveKeeper; // Used to manage VariableLiveRanges of variables
#endif                                 // USING_VARIABLE_LIVE_RANGE

#ifdef LATE_DISASM
public:
    virtual const char* siRegVarName(size_t offs, size_t size, unsigned reg) = 0;

    virtual const char* siStackVarName(size_t offs, size_t size, unsigned reg, unsigned stkOffs) = 0;
#endif // LATE_DISASM
};

StructStoreKind GetStructStoreKind(bool isLocalStore, ClassLayout* layout, GenTree* src);

#endif // _CODEGEN_INTERFACE_H_
