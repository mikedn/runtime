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

struct Win64UnwindInfo
{
    // The header allows for maximum 255 codes but it has space only for 1,
    // put it in a byte array of sufficient size to actually store 255 codes.
    // TODO-MIKE-Throughput: This array is huge, especially for funclets that
    // have very simple prologs.
    uint8_t  block[sizeof(UNWIND_INFO) + 254 * sizeof(UNWIND_CODE)];
    unsigned codesIndex = 0;

    static constexpr unsigned headerSize   = offsetof(UNWIND_INFO, UnwindCode);
    static constexpr unsigned endCodeIndex = sizeof(block);

    Win64UnwindInfo()
    {
        memset(block, 0, headerSize);
    }

    UNWIND_INFO& GetHeader()
    {
        return *reinterpret_cast<UNWIND_INFO*>(block);
    }

    UNWIND_CODE* AddCode(uint32_t codeOffset, UNWIND_OP_CODES op, uint8_t info);
    uint16_t* AddUInt16(uint16_t value);
    uint32_t* AddUInt32(uint32_t value);
};
#endif // TARGET_AMD64

#ifdef TARGET_ARMARCH

#include "emit.h"

// A base class shared by the the classes used to represent the prolog
// and epilog unwind codes.
class UnwindCodes
{
protected:
    Compiler* uwiComp;

    UnwindCodes(Compiler* comp = nullptr) : uwiComp(comp)
    {
    }

    static bool IsEndCode(uint8_t b);

    INDEBUG(static unsigned GetCodeSizeFromUnwindCodes(bool isProlog, const uint8_t* codes);)

public:
    Compiler* GetCompiler() const
    {
        return uwiComp;
    }
};

class UnwindEpilogInfo;
class UnwindEpilogCodes;

// Represents the unwind codes for a prolog sequence.
// Prolog unwind codes arrive in reverse order from how they will be emitted.
// Store them as a stack, storing from the end of an array towards the beginning.
// This class is also re-used as the final location of the consolidated unwind
// information for a function, including unwind info header, the prolog codes,
// and any epilog codes.
class UnwindPrologCodes : public UnwindCodes
{
    // To store the unwind codes, we first use a local array that should satisfy almost all cases.
    // If there are more unwind codes, we dynamically allocate memory. For ARM CoreLib, the maximum
    // size is 34. Here is a histogram of other interesting sizes:
    //     <=16  79%
    //     <=24  96%
    //     <=32  99%
    // From this data, we choose to use 24.
    uint8_t upcMemLocal[24]{};

    uint8_t* upcMem = upcMemLocal;
    // upcMemSize is the number of bytes in upcMem.
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
    int upcUnwindBlockSlot = 0;

public:
    UnwindPrologCodes()
    {
    }

    UnwindPrologCodes(Compiler* comp);

    UnwindPrologCodes(const UnwindPrologCodes& info) = delete;
    UnwindPrologCodes& operator=(const UnwindPrologCodes&) = delete;

    uint8_t* AllocCode(int size);

    uint8_t* GetCodes() const
    {
        assert(upcCodeSlot < upcMemSize);
        return &upcMem[upcCodeSlot];
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
    void AppendEpilog(UnwindEpilogInfo* epilog);
    // Copy the prolog codes from another prolog
    void CopyFrom(const UnwindPrologCodes& pCopyFrom);

    INDEBUG(void Dump(int indent = 0);)

private:
    void EnsureSize(int requiredSize);
};

// Represents the unwind codes for a single epilog sequence.
// Epilog unwind codes arrive in the order they will be emitted.
// Store them as an array, adding new ones to the end of the array.
class UnwindEpilogCodes : public UnwindCodes
{
    // To store the unwind codes, we first use a local array that should satisfy almost all cases.
    // If there are more unwind codes, we dynamically allocate memory. For ARM CoreLib, the maximum
    // size is 6, while 89% of epilogs fit in 4. So, set it to 4 to maintain array alignment and hit
    // most cases.
    uint8_t uecMemLocal[4]{};

    uint8_t* uecMem = uecMemLocal;
    // uecMemSize is the number of bytes/slots in uecMem. This is equal to UEC_LOCAL_COUNT unless
    // we've dynamically allocated memory to store the codes.
    int uecMemSize = _countof(uecMemLocal);
    // uecCodeSlot points to the last unwind code added to the array. The array is filled in from
    // the beginning, so it starts at -1.
    int uecCodeSlot = -1;

    int lastCodeSize = 0;

    // Is the unwind information finalized? Finalized info has an end code appended.
    bool uecFinalized = false;

public:
    UnwindEpilogCodes()
    {
    }

    UnwindEpilogCodes(Compiler* comp) : UnwindCodes(comp)
    {
    }

    UnwindEpilogCodes(const UnwindEpilogCodes& info) = delete;
    UnwindEpilogCodes& operator=(const UnwindEpilogCodes&) = delete;

    uint8_t* AllocCode(int size);

    uint8_t* GetCodes() const
    {
        assert(uecFinalized);
        return uecMem;
    }

    int  Size() const;
    void FinalizeCodes();

    INDEBUG(void Dump(int indent = 0);)

private:
    void EnsureSize(int requiredSize);
};

// Represents the unwind information for a single epilog sequence.
// Epilogs for a single function/funclet are in a linked list.
class UnwindEpilogInfo
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
    UnwindEpilogInfo(CodeGen* codeGen);

    UnwindEpilogInfo(const UnwindEpilogInfo& info) = delete;
    UnwindEpilogInfo& operator=(const UnwindEpilogInfo&) = delete;

    const emitLocation& GetStartLocation() const
    {
        return epiStartLoc;
    }

    int GetStartIndex() const
    {
        assert(epiStartIndex != -1);
        return epiStartIndex; // The final "Epilog Start Index" of this epilog's unwind codes
    }

    void SetStartIndex(int index)
    {
        assert(epiStartIndex == -1);
        epiStartIndex = index;
    }

    void SetMatches()
    {
        epiMatches = true;
    }

    bool HasMatch() const
    {
        return epiMatches;
    }

    const UnwindEpilogCodes& Codes() const
    {
        return epiCodes;
    }

    int Size() const
    {
        return epiCodes.Size();
    }

    uint8_t* GetCodes() const
    {
        return epiCodes.GetCodes();
    }

    INDEBUG(void Dump(int indent = 0);)
};

// Represents all the unwind information for a single fragment of a function or funclet.
// A fragment is a section with a code size less than the maximum unwind code size: either 512K bytes,
// or that specified by COMPlus_JitSplitFunctionSize. In most cases, there will be exactly one fragment.
class UnwindFragmentInfo
{
    friend class UnwindInfo;

    UnwindFragmentInfo* ufiNext       = nullptr;
    insGroup*           ufiStartLoc   = nullptr;
    UnwindEpilogInfo*   ufiEpilogList = nullptr;
    UnwindEpilogInfo*   ufiEpilogLast = nullptr;

    // The unwind codes for the prolog
    UnwindPrologCodes ufiPrologCodes;

    // Some data computed when merging the unwind codes, and used when finalizing the
    // unwind block for emission.

    // The size of the unwind data for this fragment, in bytes
    unsigned ufiSize         = 0;
    unsigned ufiCodeWords    = 0;
    unsigned ufiEpilogScopes = 0;
    // Are the prolog codes for a phantom prolog, or a real prolog?
    // (For a phantom prolog, this code fragment represents a fragment in
    // the sense of the unwind info spec; something without a real prolog.)
    bool ufiHasPhantomProlog                 = false;
    bool ufiSetEBit                          = false;
    bool ufiNeedExtendedCodeWordsEpilogCount = false;

#ifdef DEBUG
    unsigned ufiNum         = 1;
    bool     ufiInitialized = false;
#endif

public:
    UnwindFragmentInfo()
    {
    }

    UnwindFragmentInfo(Compiler* comp, insGroup* start, bool hasPhantomProlog)
        : ufiStartLoc(start)
        , ufiPrologCodes(comp)
        , ufiHasPhantomProlog(hasPhantomProlog)
#ifdef DEBUG
        , ufiInitialized(true)
#endif
    {
    }

    UnwindFragmentInfo(const UnwindFragmentInfo& info) = delete;
    UnwindFragmentInfo& operator=(const UnwindFragmentInfo&) = delete;

    Compiler* GetCompiler() const
    {
        return ufiPrologCodes.GetCompiler();
    }

    const insGroup* GetStartLoc() const
    {
        return ufiStartLoc;
    }

    uint8_t* AllocCode(int size)
    {
        assert(ufiInitialized);

        return ufiEpilogLast == nullptr ? ufiPrologCodes.AllocCode(size) : ufiEpilogLast->epiCodes.AllocCode(size);
    }

    UnwindEpilogInfo* AddEpilog(CodeGen* codeGen);

    void MergeCodes();
    void CopyPrologCodes(const UnwindFragmentInfo& copyFrom);
    void SplitEpilogs(UnwindFragmentInfo* fromFragment);
    bool IsAtFragmentEnd(UnwindEpilogInfo* epilog);

    // Return the full, final size of unwind block. This will be used to allocate memory
    // for the unwind block. This is called before the code offsets are finalized.
    uint32_t Size() const
    {
        assert(ufiSize != 0);
        return ufiSize;
    }

    void Finalize(uint32_t startOffset, uint32_t functionLength);
    void Reserve(CodeGen* codeGen, FuncKind kind, bool isHotCode);
    void Allocate(CodeGen* codeGen, FuncKind kind, CodeRange range, bool isHotCode);

    INDEBUG(void Dump(int indent = 0);)
};

// Represents all the unwind information for a single function or funclet
class UnwindInfo
{
    // The first fragment is directly here, so it doesn't need to be separately allocated.
    UnwindFragmentInfo uwiFragmentFirst;
    // End emitter location of this function/funclet (nullptr == end of all code)
    insGroup* uwiEndLoc = nullptr;

    INDEBUG(bool uwiInitialized = false;)

public:
#ifdef TARGET_ARM
    INDEBUG(bool uwiAddingNOP = false;)
#endif

    UnwindInfo()
    {
    }

    UnwindInfo(Compiler* comp, insGroup* start, insGroup* end);

    UnwindInfo(const UnwindInfo& info) = delete;
    UnwindInfo& operator=(const UnwindInfo&) = delete;

    void SplitColdCodes(UnwindInfo* hotInfo);
    void SplitLargeFragment(CodeGen* codeGen);
    void Reserve(CodeGen* codeGen, FuncKind kind, bool isHotCode);
    void Allocate(CodeGen* codeGen, FuncKind kind, bool isHotCode);

    // Add an unwind code. It could be for a prolog, or for the current epilog.
    // A single unwind code can be from 1 to 4 bytes.

    void AddCode(uint8_t b1)
    {
        assert(uwiInitialized);
        INDEBUG(CheckOpsize(b1));

        uint8_t* code = uwiFragmentFirst.AllocCode(1);

        code[0] = b1;
    }

    void AddCode(uint8_t b1, uint8_t b2)
    {
        assert(uwiInitialized);
        INDEBUG(CheckOpsize(b1));

        uint8_t* code = uwiFragmentFirst.AllocCode(2);

        code[0] = b1;
        code[1] = b2;
    }

    void AddCode(uint8_t b1, uint8_t b2, uint8_t b3)
    {
        assert(uwiInitialized);
        INDEBUG(CheckOpsize(b1));

        uint8_t* code = uwiFragmentFirst.AllocCode(3);

        code[0] = b1;
        code[1] = b2;
        code[2] = b3;
    }

    void AddCode(uint8_t b1, uint8_t b2, uint8_t b3, uint8_t b4)
    {
        assert(uwiInitialized);
        INDEBUG(CheckOpsize(b1));

        uint8_t* code = uwiFragmentFirst.AllocCode(4);

        code[0] = b1;
        code[1] = b2;
        code[2] = b3;
        code[3] = b4;
    }

    UnwindEpilogInfo* AddEpilog(CodeGen* codeGen);

private:
    UnwindFragmentInfo* AddFragment(UnwindFragmentInfo* last, insGroup* ig);
    void Split(insGroup* start, insGroup* end, uint32_t maxCodeSize);

#ifdef DEBUG
    // Given the first byte of the unwind code, check that its opsize matches
    // the last instruction added in the emitter.
    void CheckOpsize(uint8_t b1);
    void Dump(bool isHotCode, int indent = 0);
#endif
};
#endif // TARGET_ARMARCH

#ifdef TARGET_UNIX
struct CfiUnwindInfo
{
    jitstd::vector<CFI_CODE>* codes = nullptr;

    void AddCode(uint32_t codeOffset, uint8_t opcode, int16_t dwarfReg, int32_t offset = 0);
};
#endif // TARGET_UNIX

#ifdef FEATURE_EH_FUNCLETS
struct FuncInfoDsc
{
    FuncKind kind;
    uint16_t ehIndex;

#ifdef TARGET_AMD64
    Win64UnwindInfo win;
#endif

#ifdef TARGET_ARMARCH
    // Unwind information for this function/funclet's hot  section
    UnwindInfo uwi;
    // Unwind information for this function/funclet's cold section
    // Note: we only have a pointer here instead of the actual object,
    // to save memory in the JIT case (compared to the NGEN case),
    // where we don't have any cold section.
    // And we currently don't support hot/cold splitting in functions
    // with EH, so uwiCold will be nullptr for all funclets.
    UnwindInfo* uwiCold = nullptr;
#endif

#ifdef TARGET_UNIX
    CfiUnwindInfo cfi;
#endif

    FuncInfoDsc(FuncKind kind, uint32_t ehIndex) : kind(kind), ehIndex(static_cast<uint16_t>(ehIndex))
    {
        assert(ehIndex <= UINT16_MAX);
    }

    // Eventually we may want to move rsModifiedRegsMask, lvaOutgoingArgSize, and anything else
    // that isn't shared between the main function body and funclets.
};
#endif // FEATURE_EH_FUNCLETS
