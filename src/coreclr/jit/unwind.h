// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#ifdef FEATURE_EH_FUNCLETS
enum FuncKind : uint8_t
{
    FUNC_ROOT,    // The main/root function (always id==0)
    FUNC_HANDLER, // a funclet associated with an EH handler (finally, fault, catch, filter handler)
    FUNC_FILTER   // a funclet associated with an EH filter
};
#endif

#ifdef TARGET_AMD64
#include <win64unwind.h>
#endif

#ifdef TARGET_ARMARCH

#include "emit.h"

class UnwindCodesBase;
class UnwindPrologCodes;
class UnwindEpilogCodes;
class UnwindEpilogInfo;
class UnwindFragmentInfo;
class UnwindInfo;

// UnwindBase: A base class shared by the the unwind classes that require
// a Compiler* for memory allocation.

class UnwindBase
{
protected:
    Compiler* uwiComp;

    UnwindBase()
    {
    }

    UnwindBase(Compiler* comp) : uwiComp(comp)
    {
    }
};

// UnwindCodesBase: A base class shared by the the classes used to represent the prolog
// and epilog unwind codes.

class UnwindCodesBase : public UnwindBase
{
public:
    UnwindCodesBase()
    {
    }

    UnwindCodesBase(Compiler* comp) : UnwindBase(comp)
    {
    }

    // Add a single unwind code.

    virtual void AddCode(uint8_t b1) = 0;
    virtual void AddCode(uint8_t b1, uint8_t b2) = 0;
    virtual void AddCode(uint8_t b1, uint8_t b2, uint8_t b3) = 0;
    virtual void AddCode(uint8_t b1, uint8_t b2, uint8_t b3, uint8_t b4) = 0;

    // Get access to the unwind codes

    virtual uint8_t* GetCodes() const = 0;

    bool IsEndCode(uint8_t b) const;

    INDEBUG(unsigned GetCodeSizeFromUnwindCodes(bool isProlog);)
};

// UnwindPrologCodes: represents the unwind codes for a prolog sequence.
// Prolog unwind codes arrive in reverse order from how they will be emitted.
// Store them as a stack, storing from the end of an array towards the beginning.
// This class is also re-used as the final location of the consolidated unwind
// information for a function, including unwind info header, the prolog codes,
// and any epilog codes.

class UnwindPrologCodes : public UnwindCodesBase
{
    // UPC_LOCAL_COUNT is the amount of memory local to this class. For ARM CoreLib, the maximum size is 34.
    // Here is a histogram of other interesting sizes:
    //     <=16  79%
    //     <=24  96%
    //     <=32  99%
    // From this data, we choose to use 24.
    static const int UPC_LOCAL_COUNT = 24;

    // To store the unwind codes, we first use a local array that should satisfy almost all cases.
    // If there are more unwind codes, we dynamically allocate memory.
    uint8_t  upcMemLocal[UPC_LOCAL_COUNT];
    uint8_t* upcMem = upcMemLocal;
    // upcMemSize is the number of bytes in upcMem. This is equal to UPC_LOCAL_COUNT unless
    // we've dynamically allocated memory to store the codes.
    int upcMemSize = _countof(upcMemLocal);
    // upcCodeSlot points to the last unwind code added to the array. The array is filled in from
    // the end, so it starts pointing one beyond the array end.
    int upcCodeSlot = _countof(upcMemLocal);
    // upcHeaderSlot points to the last header byte prepended to the array. Headers bytes are
    // filled in from the beginning, and only after SetFinalSize() is called.
    int upcHeaderSlot = -1;
    // upcEpilogSlot points to the next epilog location to fill
    int upcEpilogSlot = -1;
    // upcUnwindBlockSlot is only set after SetFinalSize() is called. It is the index of the first
    // byte of the final unwind data, namely the first byte of the header.
    int upcUnwindBlockSlot;

public:
    UnwindPrologCodes()
    {
    }

    UnwindPrologCodes(Compiler* comp);

    UnwindPrologCodes(const UnwindPrologCodes& info) = delete;
    UnwindPrologCodes& operator=(const UnwindPrologCodes&) = delete;

    //
    // Implementation of UnwindCodesBase
    //

    virtual void AddCode(uint8_t b1)
    {
        PushByte(b1);
    }

    virtual void AddCode(uint8_t b1, uint8_t b2)
    {
        PushByte(b2);
        PushByte(b1);
    }

    virtual void AddCode(uint8_t b1, uint8_t b2, uint8_t b3)
    {
        PushByte(b3);
        PushByte(b2);
        PushByte(b1);
    }

    virtual void AddCode(uint8_t b1, uint8_t b2, uint8_t b3, uint8_t b4)
    {
        PushByte(b4);
        PushByte(b3);
        PushByte(b2);
        PushByte(b1);
    }

    // Return a pointer to the first unwind code byte
    virtual uint8_t* GetCodes() const
    {
        assert(upcCodeSlot < upcMemSize); // There better be at least one code!
        return &upcMem[upcCodeSlot];
    }

    ///////////////////////////////////////////////////////////////////////////

    uint8_t GetByte(int index)
    {
        assert(upcCodeSlot <= index && index < upcMemSize);
        return upcMem[index];
    }

    // Push a single byte on the unwind code stack
    void PushByte(uint8_t b)
    {
        if (upcCodeSlot == 0)
        {
            // We've run out of space! Reallocate, and copy everything to a new array.
            EnsureSize(upcMemSize + 1);
        }

        --upcCodeSlot;
        noway_assert(0 <= upcCodeSlot && upcCodeSlot < upcMemSize);

        upcMem[upcCodeSlot] = b;
    }

    // Return the size of the unwind codes, in bytes. The size is the exact size, not an aligned size.
    // The size includes exactly one "end" code.
    int Size() const
    {
        // -3 because we put 4 "end" codes at the end in the constructor, and we shouldn't count that here
        return upcMemSize - upcCodeSlot - 3;
    }

    void SetFinalSize(int headerBytes, int epilogBytes);
    void AddHeaderWord(uint32_t d);
    void GetFinalInfo(uint8_t** unwindBlock, uint32_t* unwindBlockSize);
    // Copy the epilog bytes to the next epilog bytes slot
    void AppendEpilog(UnwindEpilogInfo* pEpi);
    // Match the prolog codes to a set of epilog codes
    int Match(UnwindEpilogInfo* pEpi);
    // Copy the prolog codes from another prolog
    void CopyFrom(UnwindPrologCodes* pCopyFrom);

    INDEBUG(void Dump(int indent = 0);)

private:
    void EnsureSize(int requiredSize);
};

// UnwindEpilogCodes: represents the unwind codes for a single epilog sequence.
// Epilog unwind codes arrive in the order they will be emitted. Store them as an array,
// adding new ones to the end of the array.

class UnwindEpilogCodes : public UnwindCodesBase
{
    // UEC_LOCAL_COUNT is the amount of memory local to this class. For ARM CoreLib, the maximum size is 6,
    // while 89% of epilogs fit in 4. So, set it to 4 to maintain array alignment and hit most cases.
    static const int UEC_LOCAL_COUNT = 4;

    // To store the unwind codes, we first use a local array that should satisfy almost all cases.
    // If there are more unwind codes, we dynamically allocate memory.
    uint8_t  uecMemLocal[UEC_LOCAL_COUNT];
    uint8_t* uecMem              = uecMemLocal;
    uint8_t  firstByteOfLastCode = 0;

    // uecMemSize is the number of bytes/slots in uecMem. This is equal to UEC_LOCAL_COUNT unless
    // we've dynamically allocated memory to store the codes.
    int uecMemSize = _countof(uecMemLocal);

    // uecCodeSlot points to the last unwind code added to the array. The array is filled in from
    // the beginning, so it starts at -1.
    int uecCodeSlot = -1;

    // Is the unwind information finalized? Finalized info has an end code appended.
    bool uecFinalized = false;

public:
    UnwindEpilogCodes()
    {
    }

    UnwindEpilogCodes(Compiler* comp) : UnwindCodesBase(comp)
    {
    }

    UnwindEpilogCodes(const UnwindEpilogCodes& info) = delete;
    UnwindEpilogCodes& operator=(const UnwindEpilogCodes&) = delete;

    //
    // Implementation of UnwindCodesBase
    //

    virtual void AddCode(uint8_t b1)
    {
        AppendByte(b1);

        firstByteOfLastCode = b1;
    }

    virtual void AddCode(uint8_t b1, uint8_t b2)
    {
        AppendByte(b1);
        AppendByte(b2);

        firstByteOfLastCode = b1;
    }

    virtual void AddCode(uint8_t b1, uint8_t b2, uint8_t b3)
    {
        AppendByte(b1);
        AppendByte(b2);
        AppendByte(b3);

        firstByteOfLastCode = b1;
    }

    virtual void AddCode(uint8_t b1, uint8_t b2, uint8_t b3, uint8_t b4)
    {
        AppendByte(b1);
        AppendByte(b2);
        AppendByte(b3);
        AppendByte(b4);

        firstByteOfLastCode = b1;
    }

    // Return a pointer to the first unwind code byte
    virtual uint8_t* GetCodes() const
    {
        assert(uecFinalized);

        // Codes start at the beginning
        return uecMem;
    }

    ///////////////////////////////////////////////////////////////////////////

    uint8_t GetByte(int index) const
    {
        assert(0 <= index && index <= uecCodeSlot);
        return uecMem[index];
    }

    void AppendByte(uint8_t b);
    int  Size() const;
    void FinalizeCodes();

    INDEBUG(void Dump(int indent = 0);)

private:
    void EnsureSize(int requiredSize);
};

// UnwindEpilogInfo: represents the unwind information for a single epilog sequence. Epilogs for a
// single function/funclet are in a linked list.

class UnwindEpilogInfo : public UnwindBase
{
    friend class UnwindFragmentInfo;

    UnwindEpilogInfo* epiNext = nullptr;
    // The emitter location of the beginning of the epilog
    emitLocation      epiStartLoc;
    UnwindEpilogCodes epiCodes;
    // Do the epilog unwind codes match some other set of codes? If so, we don't copy these to the
    // final set; we just point to another set.
    bool epiMatches = false;
    // The final "Epilog Start Index" of this epilog's unwind codes
    int epiStartIndex = -1;

public:
    UnwindEpilogInfo()
    {
    }

    UnwindEpilogInfo(Compiler* comp) : UnwindBase(comp), epiCodes(comp)
    {
    }

    UnwindEpilogInfo(const UnwindEpilogInfo& info) = delete;
    UnwindEpilogInfo& operator=(const UnwindEpilogInfo&) = delete;

    void CaptureEmitLocation(class emitter* emitter);

    void FinalizeCodes()
    {
        epiCodes.FinalizeCodes();
    }

    const emitLocation& GetStartLocation() const
    {
        return epiStartLoc;
    }

    int GetStartIndex()
    {
        assert(epiStartIndex != -1);
        return epiStartIndex; // The final "Epilog Start Index" of this epilog's unwind codes
    }

    void SetStartIndex(int index)
    {
        assert(epiStartIndex == -1);
        epiStartIndex = (int)index;
    }

    void SetMatches()
    {
        epiMatches = true;
    }

    bool Matches() const
    {
        return epiMatches;
    }

    // Size of epilog unwind codes in bytes
    int Size() const
    {
        return epiCodes.Size();
    }

    // Return a pointer to the first unwind code byte
    uint8_t* GetCodes() const
    {
        return epiCodes.GetCodes();
    }

    // Match the codes to a set of epilog codes
    int Match(UnwindEpilogInfo* pEpi);

    INDEBUG(void Dump(int indent = 0);)
};

// UnwindFragmentInfo: represents all the unwind information for a single fragment of a function or funclet.
// A fragment is a section with a code size less than the maximum unwind code size: either 512K bytes, or
// that specified by COMPlus_JitSplitFunctionSize. In most cases, there will be exactly one fragment.

class UnwindFragmentInfo : public UnwindBase
{
    friend class UnwindInfo;

    // The next fragment
    UnwindFragmentInfo* ufiNext = nullptr;
    // Emitter location for start of fragment
    insGroup* ufiStartLoc = nullptr;
    // Are the prolog codes for a phantom prolog, or a real prolog?
    // (For a phantom prolog, this code fragment represents a fragment in
    // the sense of the unwind info spec; something without a real prolog.)
    bool ufiHasPhantomProlog;
    // The unwind codes for the prolog
    UnwindPrologCodes ufiPrologCodes;
    // In-line the first epilog to avoid separate memory allocation, since
    // almost all functions will have at least one epilog. It is pointed
    // to by ufiEpilogList when the first epilog is added.
    UnwindEpilogInfo ufiEpilogFirst;
    // The head of the epilog list
    UnwindEpilogInfo* ufiEpilogList = nullptr;
    // The last entry in the epilog list (the last epilog added)
    UnwindEpilogInfo* ufiEpilogLast = nullptr;
    // Pointer to current unwind codes, either prolog or epilog
    UnwindCodesBase* ufiCurCodes = &ufiPrologCodes;

    // Some data computed when merging the unwind codes, and used when finalizing the
    // unwind block for emission.

    // The size of the unwind data for this fragment, in bytes
    unsigned ufiSize = 0;
    bool     ufiSetEBit;
    bool     ufiNeedExtendedCodeWordsEpilogCount;
    unsigned ufiCodeWords;
    unsigned ufiEpilogScopes;

#ifdef DEBUG
    unsigned ufiNum = 1;
    // Are we processing the prolog? The prolog must come first, followed by a (possibly empty)
    // set of epilogs, for this function/funclet.
    bool ufiInProlog    = true;
    bool ufiInitialized = false;
#endif

public:
    UnwindFragmentInfo()
    {
    }

    UnwindFragmentInfo(Compiler* comp, insGroup* start, bool hasPhantomProlog)
        : UnwindBase(comp)
        , ufiStartLoc(start)
        , ufiHasPhantomProlog(hasPhantomProlog)
        , ufiPrologCodes(comp)
        , ufiEpilogFirst(comp)
#ifdef DEBUG
        , ufiInitialized(true)
#endif
    {
    }

    UnwindFragmentInfo(const UnwindFragmentInfo& info) = delete;
    UnwindFragmentInfo& operator=(const UnwindFragmentInfo&) = delete;

    const insGroup* GetStartLoc() const
    {
        return ufiStartLoc;
    }

    // Add an unwind code. It could be for a prolog, or for the current epilog.
    // A single unwind code can be from 1 to 4 bytes.

    void AddCode(uint8_t b1)
    {
        assert(ufiInitialized);
        ufiCurCodes->AddCode(b1);
    }

    void AddCode(uint8_t b1, uint8_t b2)
    {
        assert(ufiInitialized);
        ufiCurCodes->AddCode(b1, b2);
    }

    void AddCode(uint8_t b1, uint8_t b2, uint8_t b3)
    {
        assert(ufiInitialized);
        ufiCurCodes->AddCode(b1, b2, b3);
    }

    void AddCode(uint8_t b1, uint8_t b2, uint8_t b3, uint8_t b4)
    {
        assert(ufiInitialized);
        ufiCurCodes->AddCode(b1, b2, b3, b4);
    }

    unsigned EpilogCount()
    {
        unsigned count = 0;
        for (UnwindEpilogInfo* pEpi = ufiEpilogList; pEpi != nullptr; pEpi = pEpi->epiNext)
        {
            ++count;
        }
        return count;
    }

    UnwindEpilogInfo* AddEpilog();
    void              MergeCodes();
    void CopyPrologCodes(UnwindFragmentInfo* copyFrom);
    void SplitEpilogCodes(const insGroup* splitLoc, UnwindFragmentInfo* splitFrom);
    bool IsAtFragmentEnd(UnwindEpilogInfo* epilog);

    // Return the full, final size of unwind block. This will be used to allocate memory for
    // the unwind block. This is called before the code offsets are finalized.
    // Size is in bytes.
    uint32_t Size() const
    {
        assert(ufiSize != 0);
        return ufiSize;
    }

    void Finalize(uint32_t startOffset, uint32_t functionLength);

    // GetFinalInfo: return a pointer to the final unwind info to hand to the VM, and the size of this info in bytes
    void GetFinalInfo(uint8_t** unwindBlock, uint32_t* unwindBlockSize)
    {
        ufiPrologCodes.GetFinalInfo(unwindBlock, unwindBlockSize);
    }

    void Reserve(FuncKind kind, bool isHotCode);
    void Allocate(
        FuncKind kind, void* hotCode, void* coldCode, uint32_t startOffset, uint32_t endOffset, bool isHotCode);

    INDEBUG(void Dump(int indent = 0);)
};

// UnwindInfo: represents all the unwind information for a single function or funclet

class UnwindInfo : public UnwindBase
{
    void Split(insGroup* start, insGroup* end, uint32_t maxCodeSize);

    // The first fragment is directly here, so it doesn't need to be separately allocated.
    UnwindFragmentInfo uwiFragmentFirst;
    // End emitter location of this function/funclet (nullptr == end of all code)
    insGroup* uwiEndLoc = nullptr;
    // The current emitter location (updated after an unwind code is added), used for NOP
    // padding, and asserts.
    emitLocation uwiCurLoc;
    INDEBUG(bool uwiInitialized = false;)

public:
    INDEBUG(bool uwiAddingNOP = false;)

    UnwindInfo()
    {
    }

    UnwindInfo(Compiler* comp, insGroup* start, insGroup* end);

    UnwindInfo(const UnwindInfo& info) = delete;
    UnwindInfo& operator=(const UnwindInfo&) = delete;

    void SplitColdCodes(UnwindInfo* hotInfo);
    void SplitLargeFragment();
    void Reserve(FuncKind kind, bool isHotCode);
    void Allocate(FuncKind kind, void* hotCode, void* coldCode, bool isHotCode);

    // Add an unwind code. It could be for a prolog, or for the current epilog.
    // A single unwind code can be from 1 to 4 bytes.

    void AddCode(uint8_t b1)
    {
        assert(uwiInitialized);
        INDEBUG(CheckOpsize(b1));

        uwiFragmentFirst.AddCode(b1);
    }

    void AddCode(uint8_t b1, uint8_t b2)
    {
        assert(uwiInitialized);
        INDEBUG(CheckOpsize(b1));

        uwiFragmentFirst.AddCode(b1, b2);
    }

    void AddCode(uint8_t b1, uint8_t b2, uint8_t b3)
    {
        assert(uwiInitialized);
        INDEBUG(CheckOpsize(b1));

        uwiFragmentFirst.AddCode(b1, b2, b3);
    }

    void AddCode(uint8_t b1, uint8_t b2, uint8_t b3, uint8_t b4)
    {
        assert(uwiInitialized);
        INDEBUG(CheckOpsize(b1));

        uwiFragmentFirst.AddCode(b1, b2, b3, b4);
    }

    UnwindEpilogInfo* AddEpilog();

    void CaptureLocation(class emitter* emitter);

    const emitLocation& GetCurrentEmitterLocation()
    {
        return uwiCurLoc;
    }

#ifdef TARGET_ARM
    unsigned GetInstructionSize();
#endif

#ifdef DEBUG
    // Given the first byte of the unwind code, check that its opsize matches
    // the last instruction added in the emitter.
    void CheckOpsize(uint8_t b1);
    void Dump(bool isHotCode, int indent = 0);
#endif

private:
    UnwindFragmentInfo* AddFragment(UnwindFragmentInfo* last, insGroup* ig);
};
#endif // TARGET_ARMARCH

#ifdef FEATURE_EH_FUNCLETS
struct FuncInfoDsc
{
    FuncKind       funKind;
    unsigned short funEHIndex; // index, into the ebd table, of innermost EH clause corresponding to this
// funclet. It is only valid if funKind field indicates this is a
// EH-related funclet: FUNC_HANDLER or FUNC_FILTER

#ifdef TARGET_AMD64
    UNWIND_INFO unwindHeader;
    // Maximum of 255 UNWIND_CODE 'nodes' and then the unwind header. If there are an odd
    // number of codes, the VM or Zapper will 4-byte align the whole thing.
    // TODO-AMD64-Throughput: make the AMD64 info more like the ARM info to avoid having this large static array.
    uint8_t  unwindCodes[offsetof(UNWIND_INFO, UnwindCode) + (0xFF * sizeof(UNWIND_CODE))];
    unsigned unwindCodeSlot;
#endif

#ifdef TARGET_ARMARCH
    UnwindInfo  uwi;     // Unwind information for this function/funclet's hot  section
    UnwindInfo* uwiCold; // Unwind information for this function/funclet's cold section
                         //   Note: we only have a pointer here instead of the actual object,
                         //   to save memory in the JIT case (compared to the NGEN case),
                         //   where we don't have any cold section.
                         //   Note 2: we currently don't support hot/cold splitting in functions
                         //   with EH, so uwiCold will be nullptr for all funclets.
#endif

#ifdef TARGET_UNIX
    jitstd::vector<CFI_CODE>* cfiCodes;
#endif

    // Eventually we may want to move rsModifiedRegsMask, lvaOutgoingArgSize, and anything else
    // that isn't shared between the main function body and funclets.
};
#endif // FEATURE_EH_FUNCLETS
