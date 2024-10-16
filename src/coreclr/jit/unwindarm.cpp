// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "unwind.h"
#include "codegen.h"

#ifdef TARGET_ARMARCH

void CodeGen::unwindBegProlog()
{
    assert(generatingProlog);

#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        unwindBegPrologCFI();

        return;
    }
#endif

    FuncInfoDsc& func = funCurrentFunc();
    assert(func.uwiCold == nullptr);

    insGroup* startLoc;
    insGroup* endLoc;
    unwindGetFuncHotRange(&func, &startLoc, &endLoc);

    new (&func.uwi) UnwindInfo(compiler, startLoc, endLoc);
    unwindCaptureLocation();
}

void CodeGen::unwindBegEpilog()
{
    assert(generatingEpilog);

#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        return;
    }
#endif

    unwindCaptureLocation();
    funCurrentFunc().uwi.AddEpilog(this);
}

#ifdef TARGET_ARM

unsigned CodeGen::unwindGetInstructionSize() const
{
    return GetEmitter()->GetInstructionSize(unwindLoc);
}

void CodeGen::unwindPushPopMaskInt(regMaskTP maskInt, bool useOpsize16)
{
    assert((maskInt & RBM_ALLFLOAT) == RBM_NONE);

    UnwindInfo& info = funCurrentFunc().uwi;

    if (useOpsize16)
    {
        assert((maskInt & ~(RBM_R0 | RBM_R1 | RBM_R2 | RBM_R3 | RBM_R4 | RBM_R5 | RBM_R6 | RBM_R7 | RBM_LR)) ==
               RBM_NONE);

        bool    shortFormat = false;
        uint8_t val         = 0;

        if ((maskInt & (RBM_R0 | RBM_R1 | RBM_R2 | RBM_R3)) == 0)
        {
            regMaskTP matchMask = maskInt & (RBM_R4 | RBM_R5 | RBM_R6 | RBM_R7);
            regMaskTP valMask   = RBM_R4;

            while (val < 4)
            {
                if (matchMask == valMask)
                {
                    shortFormat = true;
                    break;
                }

                valMask <<= 1;
                valMask |= RBM_R4;

                val++;
            }
        }

        if (shortFormat)
        {
            // D0-D7 : pop {r4-rX,lr} (X=4-7) (opsize 16)
            info.AddCode(0xD0 | ((maskInt >> 12) & 0x4) | val);
        }
        else
        {
            // EC-ED : pop {r0-r7,lr} (opsize 16)
            info.AddCode(0xEC | ((maskInt >> 14) & 0x1), (uint8_t)maskInt);
        }
    }
    else
    {
        assert((maskInt &
                ~(RBM_R0 | RBM_R1 | RBM_R2 | RBM_R3 | RBM_R4 | RBM_R5 | RBM_R6 | RBM_R7 | RBM_R8 | RBM_R9 | RBM_R10 |
                  RBM_R11 | RBM_R12 | RBM_LR)) == 0);

        bool    shortFormat = false;
        uint8_t val         = 0;

        if (((maskInt & (RBM_R0 | RBM_R1 | RBM_R2 | RBM_R3)) == 0) &&
            ((maskInt & (RBM_R4 | RBM_R5 | RBM_R6 | RBM_R7 | RBM_R8)) == (RBM_R4 | RBM_R5 | RBM_R6 | RBM_R7 | RBM_R8)))
        {
            regMaskTP matchMask = maskInt & (RBM_R4 | RBM_R5 | RBM_R6 | RBM_R7 | RBM_R8 | RBM_R9 | RBM_R10 | RBM_R11);
            regMaskTP valMask   = RBM_R4 | RBM_R5 | RBM_R6 | RBM_R7 | RBM_R8;

            while (val < 4)
            {
                if (matchMask == valMask)
                {
                    shortFormat = true;
                    break;
                }

                valMask <<= 1;
                valMask |= RBM_R4;

                val++;
            }
        }

        if (shortFormat)
        {
            // D8-DF : pop {r4-rX,lr} (X=8-11) (opsize 32)
            info.AddCode(0xD8 | ((maskInt >> 12) & 0x4) | val);
        }
        else
        {
            // 80-BF : pop {r0-r12,lr} (opsize 32)
            info.AddCode(0x80 | ((maskInt >> 8) & 0x1F) | ((maskInt >> 9) & 0x20), (uint8_t)maskInt);
        }
    }

    unwindCaptureLocation();
}

void CodeGen::unwindPushPopMaskFloat(regMaskTP maskFloat)
{
    assert((maskFloat & ~RBM_ALLFLOAT) == RBM_NONE);

    if (maskFloat == RBM_NONE)
    {
        return;
    }

    UnwindInfo& info = funCurrentFunc().uwi;

    uint8_t   val     = 0;
    regMaskTP valMask = RBM_F16 | RBM_F17;

    while (maskFloat != valMask)
    {
        valMask <<= 2;
        valMask |= RBM_F16 | RBM_F17;

        val++;

        if (val == 8)
        {
            noway_assert(!"Illegal maskFloat");
        }
    }

    // E0-E7 : vpop {d8-dX} (X=8-15) (opsize 32)
    assert(0 <= val && val <= 7);
    info.AddCode(0xE0 | val);
    unwindCaptureLocation();
}

void CodeGen::unwindPushMaskInt(regMaskTP maskInt)
{
    assert((maskInt &
            ~(RBM_R0 | RBM_R1 | RBM_R2 | RBM_R3 | RBM_R4 | RBM_R5 | RBM_R6 | RBM_R7 | RBM_R8 | RBM_R9 | RBM_R10 |
              RBM_R11 | RBM_R12 | RBM_LR)) == RBM_NONE);

#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        // If we are pushing LR, we should give unwind codes in terms of caller's PC
        if (maskInt & RBM_LR)
        {
            maskInt = (maskInt & ~RBM_LR) | RBM_PC;
        }

        unwindPushPopMaskCFI(maskInt, false);
        return;
    }
#endif

    bool useOpsize16 = (maskInt & (RBM_LOW_REGS | RBM_LR)) == maskInt;
    unwindPushPopMaskInt(maskInt, useOpsize16);
}

void CodeGen::unwindPushMaskFloat(regMaskTP maskFloat)
{
    assert((maskFloat & RBM_ALLFLOAT) == maskFloat);

#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        unwindPushPopMaskCFI(maskFloat, true);

        return;
    }
#endif

    unwindPushPopMaskFloat(maskFloat);
}

void CodeGen::unwindPopMaskInt(regMaskTP maskInt)
{
    // Only r0-r12 and lr and pc are supported (pc is mapped to lr when encoding)
    assert((maskInt &
            ~(RBM_R0 | RBM_R1 | RBM_R2 | RBM_R3 | RBM_R4 | RBM_R5 | RBM_R6 | RBM_R7 | RBM_R8 | RBM_R9 | RBM_R10 |
              RBM_R11 | RBM_R12 | RBM_LR | RBM_PC)) == RBM_NONE);

#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        return;
    }
#endif

    bool useOpsize16 = (maskInt & (RBM_LOW_REGS | RBM_PC)) == maskInt;

    // If we are popping PC, then we'll return from the function. In this case, we assume
    // the first thing the prolog did was push LR, so give the unwind codes in terms of
    // the LR that was pushed. Note that the epilog unwind codes are meant to reverse
    // the effect of the prolog. For "pop {pc}", the prolog had "push {lr}", so we need
    // an epilog code to model the reverse of that.
    if ((maskInt & RBM_PC) != RBM_NONE)
    {
        maskInt = (maskInt & ~RBM_PC) | RBM_LR;
    }

    unwindPushPopMaskInt(maskInt, useOpsize16);
}

void CodeGen::unwindPopMaskFloat(regMaskTP maskFloat)
{
    assert((maskFloat & RBM_ALLFLOAT) == maskFloat);

#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        return;
    }
#endif

    unwindPushPopMaskFloat(maskFloat);
}

void CodeGen::unwindAllocStack(unsigned size)
{
    assert(size % 4 == 0);

#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        if (generatingProlog)
        {
            unwindAllocStackCFI(size);
        }

        return;
    }
#endif

    UnwindInfo& info = funCurrentFunc().uwi;

    size /= 4;

    if (size <= 0x7F)
    {
        // 00-7F : add sp, sp, #X*4 (opsize 16)
        info.AddCode((uint8_t)size);
    }
    else if (size <= 0x3FF)
    {
        // E8-EB : addw sp, sp, #X*4 (opsize 32)
        info.AddCode(0xE8 | (uint8_t)(size >> 8), (uint8_t)size);
    }
    else if (size <= 0xFFFF)
    {
        // F7 : add sp, sp, #X*4 (opsize 16)
        // F9 : add sp, sp, #X*4 (opsize 32)
        uint8_t b1 = unwindGetInstructionSize() == 2 ? 0xF7 : 0xF9;
        info.AddCode(b1, (uint8_t)(size >> 8), (uint8_t)size);
    }
    else
    {
        // F8 : add sp, sp, #X*4 (opsize 16)
        // FA : add sp, sp, #X*4 (opsize 32)
        uint8_t b1 = unwindGetInstructionSize() == 2 ? 0xF8 : 0xFA;
        info.AddCode(b1, (uint8_t)(size >> 16), (uint8_t)(size >> 8), (uint8_t)size);
    }

    unwindCaptureLocation();
}

void CodeGen::unwindSetFrameReg(RegNum reg)
{
    assert(0 <= reg && reg <= 15);

#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        if (generatingProlog)
        {
            unwindSetFrameRegCFI(reg, 0);
        }

        return;
    }
#endif

    UnwindInfo& info = funCurrentFunc().uwi;

    // C0-CF : mov sp, rX (opsize 16)
    info.AddCode((uint8_t)(0xC0 + reg));
    unwindCaptureLocation();
}

void CodeGen::unwindBranch16()
{
#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        return;
    }
#endif

    UnwindInfo& info = funCurrentFunc().uwi;

    // TODO-CQ: need to handle changing the exit code from 0xFF to 0xFD.
    // Currently, this will waste an extra 0xFF at the end, automatically added.
    info.AddCode(0xFD);
    unwindCaptureLocation();
}

void CodeGen::unwindNop(unsigned codeSizeInBytes) // codeSizeInBytes is 2 or 4 bytes for Thumb2 instruction
{
#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        return;
    }
#endif

    JITDUMP("unwindNop: adding NOP for %d byte instruction\n", codeSizeInBytes);

    UnwindInfo& info = funCurrentFunc().uwi;

    INDEBUG(info.uwiAddingNOP = true);

    if (codeSizeInBytes == 2)
    {
        info.AddCode(0xFB); // nop (opsize 16)
    }
    else
    {
        noway_assert(codeSizeInBytes == 4);

        info.AddCode(0xFC); // nop (opsize 32)
    }

    unwindCaptureLocation();

    INDEBUG(info.uwiAddingNOP = false);
}

#endif // TARGET_ARM

// The instructions between the last captured "current state" and the current
// instruction are in the prolog but have no effect for unwinding.
// Emit the appropriate NOP unwind codes for them.
void CodeGen::unwindPadding()
{
#if TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        return;
    }
#endif

    GetEmitter()->GenerateUnwindNopPadding(unwindLoc);
}

void CodeGen::unwindReserve()
{
    assert(!generatingProlog);
    assert(!generatingEpilog);
    assert(compFuncInfoCount > 0);

    for (unsigned i = 0; i < compFuncInfoCount; i++)
    {
        unwindReserveFunc(&funGetFunc(i));
    }
}

void CodeGen::unwindReserveFunc(FuncInfoDsc* func)
{
#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        if (compiler->fgFirstColdBlock != nullptr)
        {
            eeReserveUnwindInfo(func->kind != FUNC_ROOT, false, 0);
        }

        uint32_t unwindSize = static_cast<uint32_t>(func->cfi.codes->size() * sizeof(CFI_CODE));
        eeReserveUnwindInfo(func->kind != FUNC_ROOT, true, unwindSize);

        return;
    }
#endif

    // If there is cold code, split the unwind data between the hot section and the
    // cold section. This needs to be done before we split into fragments, as each
    // of the hot and cold sections can have multiple fragments.

    if (compiler->fgFirstColdBlock != nullptr)
    {
        insGroup* startLoc;
        insGroup* endLoc;
        unwindGetFuncColdRange(func, &startLoc, &endLoc);

        func->uwiCold = new (compiler, CMK_UnwindInfo) UnwindInfo(compiler, startLoc, endLoc);
        func->uwiCold->SplitColdCodes(&func->uwi);
    }

    func->uwi.Reserve(this, func->kind, true);

    if (func->uwiCold != nullptr)
    {
        func->uwiCold->Reserve(this, func->kind, false);
    }
}

void CodeGen::unwindEmit()
{
    assert(compFuncInfoCount > 0);

    for (unsigned i = 0; i < compFuncInfoCount; i++)
    {
        unwindEmitFunc(&funGetFunc(i));
    }
}

void CodeGen::unwindEmitFunc(FuncInfoDsc* func)
{
#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        unwindEmitFuncCFI(func);
        return;
    }
#endif

    func->uwi.Allocate(this, func->kind, true);

    if (func->uwiCold != nullptr)
    {
        func->uwiCold->Allocate(this, func->kind, false);
    }
}

#ifdef TARGET_ARM
constexpr unsigned MAX_PROLOG_SIZE_BYTES      = 44;
constexpr unsigned MAX_EPILOG_SIZE_BYTES      = 44;
constexpr uint8_t  UWC_END                    = 0xFF;
constexpr uint32_t UW_MAX_FRAGMENT_SIZE_BYTES = 1U << 19;
// Max number that can be encoded in the "Code Words" field of the .pdata record
constexpr uint32_t UW_MAX_CODE_WORDS_COUNT = 15;
// Max number that can be encoded in the "Epilog Start Index" field  of the .pdata record
constexpr uint32_t UW_MAX_EPILOG_START_INDEX = 0xFFU;
#else
constexpr unsigned MAX_PROLOG_SIZE_BYTES      = 100;
constexpr unsigned MAX_EPILOG_SIZE_BYTES      = 100;
constexpr uint8_t  UWC_END                    = 0xE4;
constexpr uint8_t  UWC_END_C                  = 0xE5;
constexpr uint32_t UW_MAX_FRAGMENT_SIZE_BYTES = 1U << 20;
constexpr uint32_t UW_MAX_CODE_WORDS_COUNT    = 31;
constexpr uint32_t UW_MAX_EPILOG_START_INDEX  = 0x3FFU;
#endif

// Max number that can be encoded in the "Epilog count" field  of the .pdata record
constexpr unsigned UW_MAX_EPILOG_COUNT = 31;
// Max number that can be encoded in the "Extended Code Words" field of the .pdata record
constexpr uint32_t UW_MAX_EXTENDED_CODE_WORDS_COUNT = 0xFFU;
// Max number that can be encoded in the "Extended Epilog Count" field of the .pdata record
constexpr uint32_t UW_MAX_EXTENDED_EPILOG_COUNT = 0xFFFFU;
// Max number that can be encoded in the "Epilog Start Offset" field of the .pdata record
constexpr uint32_t UW_MAX_EPILOG_START_OFFSET = 0x3FFFFU;

bool UnwindCodes::IsEndCode(uint8_t b)
{
#ifdef TARGET_ARM
    return b >= 0xFD;
#else
    return b == UWC_END; // TODO-ARM64-Bug?: what about the "end_c" code?
#endif
}

UnwindPrologCodes::UnwindPrologCodes(Compiler* comp) : UnwindCodes(comp)
{
    // We fill the codes array backwards and we may need to add padding at the end,
    // to make the size a multiple of 4. Add sufficient padding now, in the form of
    // "end" codes, to avoid having to move the codes later to make room for padding.
    upcMemLocal[--upcCodeSlot] = UWC_END;
    upcMemLocal[--upcCodeSlot] = UWC_END;
    upcMemLocal[--upcCodeSlot] = UWC_END;
    upcMemLocal[--upcCodeSlot] = UWC_END;
}

// We're going to use the prolog codes memory to store the final unwind data.
// Ensure we have enough memory to store everything. If 'epilogBytes' > 0, then
// move the prolog codes so there are 'epilogBytes' bytes after the prolog codes.
// Set the header pointer for future use, adding the header bytes (this pointer
// is updated when a header byte is added), and remember the index that points
// to the beginning of the header.
void UnwindPrologCodes::SetFinalSize(int headerBytes, int epilogBytes)
{
    // We're done adding codes. Check that we didn't accidentally create a bigger prolog.
    assert(GetCodeSizeFromUnwindCodes(true, GetCodes()) <= MAX_PROLOG_SIZE_BYTES);

    int prologBytes = Size();

    EnsureSize(headerBytes + prologBytes + epilogBytes + 3); // 3 = padding bytes for alignment

    upcUnwindBlockSlot = upcCodeSlot - headerBytes - epilogBytes; // Index of the first byte of the unwind header

    assert(upcMemSize == upcUnwindBlockSlot + headerBytes + prologBytes + epilogBytes + 3);

    upcHeaderSlot = upcUnwindBlockSlot - 1; // upcHeaderSlot is always incremented before storing
    assert(upcHeaderSlot >= -1);

    if (epilogBytes > 0)
    {
        // The prolog codes that are already at the end of the array need to get moved to the middle,
        // with space for the non-matching epilog codes to follow.

        memmove_s(&upcMem[upcUnwindBlockSlot + headerBytes], upcMemSize - (upcUnwindBlockSlot + headerBytes),
                  &upcMem[upcCodeSlot], prologBytes);

        // Note that the three UWC_END padding bytes still exist at the end of the array.

        // Zero out the epilog codes memory, to ensure we've copied the right bytes. Don't zero the padding bytes.
        INDEBUG(memset(&upcMem[upcUnwindBlockSlot + headerBytes + prologBytes], 0, epilogBytes));

        // upcEpilogSlot points to the next epilog location to fill
        upcEpilogSlot = upcUnwindBlockSlot + headerBytes + prologBytes;

        // Update upcCodeSlot to point at the new beginning of the prolog codes
        upcCodeSlot = upcUnwindBlockSlot + headerBytes;
    }
}

// Add a header word. Header words are added starting at the beginning, in order: first to last.
// This is in contrast to the prolog unwind codes, which are added in reverse order.
void UnwindPrologCodes::AddHeaderWord(uint32_t d)
{
    assert(-1 <= upcHeaderSlot);
    assert(upcHeaderSlot + 4 < upcCodeSlot); // Don't collide with the unwind codes that are already there!

    // Store it byte-by-byte in little-endian format. We've already ensured there is enough space
    // in SetFinalSize().
    upcMem[++upcHeaderSlot] = (uint8_t)d;
    upcMem[++upcHeaderSlot] = (uint8_t)(d >> 8);
    upcMem[++upcHeaderSlot] = (uint8_t)(d >> 16);
    upcMem[++upcHeaderSlot] = (uint8_t)(d >> 24);
}

// Copy the epilog bytes to the next epilog bytes slot
void UnwindPrologCodes::AppendEpilog(UnwindEpilogInfo* epilog)
{
    assert(upcEpilogSlot != -1);

    int epiSize = epilog->Size();
    // -3 to avoid writing to the alignment padding
    memcpy_s(&upcMem[upcEpilogSlot], upcMemSize - upcEpilogSlot - 3, epilog->GetCodes(), epiSize);
    // Make sure we copied it where we expected to copy it.
    assert(epilog->GetStartIndex() == upcEpilogSlot - upcCodeSlot);

    upcEpilogSlot += epiSize;
    assert(upcEpilogSlot <= upcMemSize - 3);
}

// Return a pointer to the final unwind info to hand to the VM, and the size of this info in bytes
void UnwindPrologCodes::GetFinalInfo(uint8_t** unwindBlock, uint32_t* unwindBlockSize)
{
    assert(upcHeaderSlot + 1 == upcCodeSlot); // We better have filled in the header before asking for the final data!

    *unwindBlock = &upcMem[upcUnwindBlockSlot];

    // We put 4 'end' codes at the end for padding, so we can ensure we have an
    // unwind block that is a multiple of 4 bytes in size. Subtract off three 'end'
    // codes (leave one), and then align the size up to a multiple of 4.
    *unwindBlockSize = AlignUp(static_cast<uint32_t>(upcMemSize - upcUnwindBlockSlot - 3), 4);
}

template <class T, class U>
static int MatchCodes(const T& x, const U& y)
{
    if (x.Size() < y.Size())
    {
        return -1;
    }

    int matchIndex = x.Size() - y.Size();

    return memcmp(x.GetCodes() + matchIndex, y.GetCodes(), y.Size()) == 0 ? matchIndex : -1;
}

// Copy the prolog codes from another prolog. The only time this is legal
// is if we are at the initial state and no prolog codes have been added.
// This is used to create the 'phantom' prolog for non-first fragments.
void UnwindPrologCodes::CopyFrom(const UnwindPrologCodes& copyFrom)
{
    assert(upcMem == upcMemLocal);
    assert(upcMemSize == _countof(upcMemLocal));
    assert(upcHeaderSlot == -1);
    assert(upcEpilogSlot == -1);

    // Copy the codes
    EnsureSize(copyFrom.upcMemSize);
    assert(upcMemSize == copyFrom.upcMemSize);
    memcpy_s(upcMem, upcMemSize, copyFrom.upcMem, copyFrom.upcMemSize);

    // Copy the other data
    upcCodeSlot        = copyFrom.upcCodeSlot;
    upcHeaderSlot      = copyFrom.upcHeaderSlot;
    upcEpilogSlot      = copyFrom.upcEpilogSlot;
    upcUnwindBlockSlot = copyFrom.upcUnwindBlockSlot;
}

uint8_t* UnwindPrologCodes::AllocCode(int size)
{
    assert(1 <= size && size <= 4);

    if (upcCodeSlot < size)
    {
        EnsureSize(GrowSize(upcMemSize, upcMemSize + size));
    }

    upcCodeSlot -= size;
    noway_assert(0 <= upcCodeSlot && upcCodeSlot < upcMemSize);

    return &upcMem[upcCodeSlot];
}

int UnwindCodes::GrowSize(int current, int min)
{
    int next = current * 2;
    // We need to increase the size by at most 4 bytes at a time, and the initial
    // size is at least 4 too. So every time we double the size we should have
    // enough to accomodate the required growth.
    noway_assert(next >= min);
    return next;
}

void UnwindPrologCodes::EnsureSize(int newSize)
{
    if (newSize <= upcMemSize)
    {
        return;
    }

    uint8_t* newMem = new (uwiComp, CMK_UnwindInfo) uint8_t[newSize];
    memcpy(newMem + newSize - upcMemSize, upcMem, upcMemSize);
    INDEBUG(memset(upcMem, 0xFF, upcMemSize));

    upcMem = newMem;
    upcCodeSlot += newSize - upcMemSize;
    upcMemSize = newSize;
}

uint8_t* UnwindEpilogCodes::AllocCode(int size)
{
    if (uecCodeSlot >= uecMemSize - size)
    {
        EnsureSize(GrowSize(uecMemSize, uecMemSize + size));
    }

    uecCodeSlot += size;
    noway_assert(0 <= uecCodeSlot && uecCodeSlot < uecMemSize);
    lastCodeSize = size;
    return &uecMem[uecCodeSlot - size + 1];
}

// Return the size of the unwind codes, in bytes. The size is the exact size, not an aligned size.
int UnwindEpilogCodes::Size() const
{
    if (uecFinalized)
    {
        // Add one because uecCodeSlot is 0-based
        return uecCodeSlot + 1;
    }
    else
    {
        // Add one because uecCodeSlot is 0-based, and one for an "end" code that isn't stored (yet).
        return uecCodeSlot + 2;
    }
}

void UnwindEpilogCodes::FinalizeCodes()
{
    assert(!uecFinalized);
    noway_assert(0 <= uecCodeSlot && uecCodeSlot < uecMemSize); // There better be at least one code!

    // If the last code is an end code, we don't need to append another one.
    if (!IsEndCode(uecMem[uecCodeSlot - lastCodeSize + 1]))
    {
        AllocCode(1)[0] = UWC_END;
    }

    uecFinalized = true; // With the "end" code in place, now we're done

    assert(GetCodeSizeFromUnwindCodes(false, GetCodes()) <= MAX_EPILOG_SIZE_BYTES);
}

void UnwindEpilogCodes::EnsureSize(int newSize)
{
    if (newSize <= uecMemSize)
    {
        return;
    }

    uint8_t* newMem = new (uwiComp, CMK_UnwindInfo) uint8_t[newSize];
    memcpy(newMem, uecMem, uecMemSize);
    INDEBUG(memset(uecMem, 0xFF, uecMemSize));

    uecMem     = newMem;
    uecMemSize = newSize;
}

UnwindEpilogInfo::UnwindEpilogInfo(CodeGen* codeGen)
    : epiStartLoc(codeGen->unwidGetLocation()), epiCodes(codeGen->GetCompiler())
{
}

UnwindEpilogInfo* UnwindFragmentInfo::AddEpilog(CodeGen* codeGen)
{
    assert(ufiInitialized);

    UnwindEpilogInfo* epilog = new (codeGen->GetCompiler(), CMK_UnwindInfo) UnwindEpilogInfo(codeGen);

    if (ufiEpilogList == nullptr)
    {
        ufiEpilogList = epilog;
    }
    else
    {
        ufiEpilogLast->epiNext = epilog;
    }

    ufiEpilogLast = epilog;

    return epilog;
}

// Copy the prolog codes from the 'pCopyFrom' fragment. These prolog codes will
// become 'phantom' prolog codes in this fragment. Note that this fragment should
// not have any prolog codes currently; it is at the initial state.
void UnwindFragmentInfo::CopyPrologCodes(const UnwindFragmentInfo& copyFrom)
{
    ufiPrologCodes.CopyFrom(copyFrom.ufiPrologCodes);
#ifdef TARGET_ARM64
    ufiPrologCodes.AllocCode(1)[0] = UWC_END_C;
#endif
}

// Split the epilogs that currently exist in 'fromFragment'. The ones that represent
// epilogs that start at or after this fragment's start location are removed from
// 'fromFragment' and moved to this fragment.
void UnwindFragmentInfo::SplitEpilogs(UnwindFragmentInfo* fromFragment)
{
    assert(ufiEpilogList == nullptr);

    uint32_t splitOffset = ufiStartLoc->GetCodeOffset();

    UnwindEpilogInfo* prevEpilog = nullptr;
    UnwindEpilogInfo* fromEpilog = nullptr;

    for (UnwindEpilogInfo *epilog = fromFragment->ufiEpilogList, *prev = nullptr; epilog != nullptr;
         prev = epilog, epilog = epilog->epiNext)
    {
        uint32_t epilogStartOffset = epilog->GetStartLocation().GetCodeOffset();

        if (epilogStartOffset >= splitOffset)
        {
            prevEpilog = prev;
            fromEpilog = epilog;

            break;
        }
    }

    if (fromEpilog != nullptr)
    {
        ufiEpilogList = fromEpilog;
        ufiEpilogLast = fromFragment->ufiEpilogLast;

        fromFragment->ufiEpilogLast = prevEpilog;

        if (fromFragment->ufiEpilogLast == nullptr)
        {
            fromFragment->ufiEpilogList = nullptr;
        }
        else
        {
            fromFragment->ufiEpilogLast->epiNext = nullptr;
        }
    }
}

bool UnwindFragmentInfo::IsAtFragmentEnd(UnwindEpilogInfo* epilog)
{
    // We're assuming that the epilog doesn't span multiple instruction groups.
    return epilog->epiStartLoc.GetIG()->igNext == ufiEndLoc;
}

// Merge the unwind codes as much as possible.
// This function is called before all offsets are final.
// Also, compute the size of the final unwind block. Store this
// and some other data for later, when we actually emit the
// unwind block.
void UnwindFragmentInfo::MergeCodes()
{
    assert(ufiInitialized);

    unsigned epilogCount = 0;
    // The total number of unwind code bytes used by epilogs that don't match the prolog codes
    unsigned epilogCodeBytes = 0;
    // The "Epilog Start Index" for the next non-matching epilog codes
    unsigned epilogIndex = ufiPrologCodes.Size();

    for (UnwindEpilogInfo* epilog = ufiEpilogList; epilog != nullptr; epilog = epilog->epiNext)
    {
        ++epilogCount;

        epilog->epiCodes.FinalizeCodes();

        // Does this epilog match the prolog?
        // NOTE: for the purpose of matching, we don't handle the 0xFD and 0xFE end codes that allow slightly unequal
        // prolog and epilog codes.

        int matchIndex = MatchCodes(ufiPrologCodes, epilog->Codes());

        if (matchIndex != -1)
        {
            epilog->SetMatches();
            epilog->SetStartIndex(matchIndex); // Prolog codes start at zero, so matchIndex is exactly the start index
        }
        else
        {
            // The epilog codes don't match the prolog codes. Do they match any of the epilogs
            // we've seen so far?

            for (UnwindEpilogInfo* epilog2 = ufiEpilogList; epilog2 != epilog; epilog2 = epilog2->epiNext)
            {
                if (epilog2->HasMatch())
                {
                    continue;
                }

                matchIndex = MatchCodes(epilog2->Codes(), epilog->Codes());

                if (matchIndex != -1)
                {
                    // Use the same epilog index as the one we matched, as it has already been set.
                    epilog->SetMatches();
                    // We might match somewhere inside epilog2's codes, in which case matchIndex > 0
                    epilog->SetStartIndex(epilog2->GetStartIndex() + matchIndex);

                    break;
                }
            }

            if (matchIndex == -1)
            {
                epilog->SetStartIndex(epilogIndex); // We'll copy these codes to the next available location
                epilogCodeBytes += epilog->Size();
                epilogIndex += epilog->Size();
            }
        }
    }

    uint32_t codeBytes = ufiPrologCodes.Size() + epilogCodeBytes;
    codeBytes          = AlignUp(codeBytes, sizeof(uint32_t));

    uint32_t codeWords = codeBytes / sizeof(uint32_t); // This is how many words we need to store all the unwind codes
                                                       // in the unwind block

    // Do we need the 2nd header word for "Extended Code Words" or "Extended Epilog Count"?

    bool needExtendedCodeWordsEpilogCount =
        (codeWords > UW_MAX_CODE_WORDS_COUNT) || (epilogCount > UW_MAX_EPILOG_COUNT);

    // How many epilog scope words do we need?

    bool     setEBit      = false;       // do we need to set the E bit?
    unsigned epilogScopes = epilogCount; // Note that this could be zero if we have no epilogs!

    if (epilogCount == 1)
    {
        assert(ufiEpilogList != nullptr);
        assert(ufiEpilogList->epiNext == nullptr);

        if (ufiEpilogList->HasMatch() && (ufiEpilogList->GetStartIndex() == 0) && // The match is with the prolog
            !needExtendedCodeWordsEpilogCount && IsAtFragmentEnd(ufiEpilogList))
        {
            epilogScopes = 0; // Don't need any epilog scope words
            setEBit      = true;
        }
    }

    uint32_t headerBytes =
        (1                                            // Always need first header uint32_t
         + (needExtendedCodeWordsEpilogCount ? 1 : 0) // Do we need the 2nd uint32_t for Extended Code
                                                      // Words or Extended Epilog Count?
         + epilogScopes                               // One uint32_t per epilog scope, for EBit = 0
         ) *
        sizeof(uint32_t); // convert it to bytes

    uint32_t finalSize = headerBytes + codeBytes; // Size of actual unwind codes, aligned up to 4-byte words,
                                                  // including end padding if necessary

    // Construct the final unwind information.

    // We re-use the memory for the prolog unwind codes to construct the full unwind data. If all the epilogs
    // match the prolog, this is easy: we just prepend the header. If there are epilog codes that don't match
    // the prolog, we still use the prolog codes memory, but it's a little more complicated, since the
    // unwind info is ordered as: (a) header, (b) prolog codes, (c) non-matching epilog codes. And, the prolog
    // codes array is filled in from end-to-beginning. So, we compute the size of memory we need, ensure we
    // have that much memory, and then copy the prolog codes to the right place, appending the non-matching
    // epilog codes and prepending the header.

    ufiPrologCodes.SetFinalSize(headerBytes, epilogCodeBytes);

    if (epilogCodeBytes != 0)
    {
        // We need to copy the epilog code bytes to their final memory location

        for (UnwindEpilogInfo* e = ufiEpilogList; e != nullptr; e = e->epiNext)
        {
            if (!e->HasMatch())
            {
                ufiPrologCodes.AppendEpilog(e);
            }
        }
    }

    // Save some data for later

    ufiSize                             = finalSize;
    ufiSetEBit                          = setEBit;
    ufiNeedExtendedCodeWordsEpilogCount = needExtendedCodeWordsEpilogCount;
    ufiCodeWords                        = codeWords;
    ufiEpilogScopes                     = epilogScopes;
}

// Prepare the unwind information for the VM. Compute and prepend the unwind header.
void UnwindFragmentInfo::Finalize(uint32_t startOffset, uint32_t functionLength)
{
    assert(ufiInitialized);

#ifdef DEBUG
    if (0 && ufiPrologCodes.GetCompiler()->verbose)
    {
        printf("*************** Before fragment #%u finalize\n", ufiNum);
        Dump();
    }
#endif

#ifdef TARGET_ARM
    noway_assert((functionLength & 1) == 0);
    uint32_t headerFunctionLength = functionLength / 2;
#else
    noway_assert((functionLength & 3) == 0);
    uint32_t headerFunctionLength = functionLength / 4;
#endif

    // Version of the unwind info is zero. No other version number is currently defined.
    uint32_t headerVers = 0;
    // We never generate "exception data", but the VM might add some.
    uint32_t headerXBit = 0;
    uint32_t headerEBit;
#ifdef TARGET_ARM
    // Is this data a fragment in the sense of the unwind data specification?
    // That is, do the prolog codes represent a real prolog or not?
    uint32_t headerFBit = ufiHasPhantomProlog ? 1 : 0;
#endif
    uint32_t headerEpilogCount; // This depends on how we set headerEBit.
    uint32_t headerCodeWords;
    uint32_t headerExtendedEpilogCount = 0; // This depends on how we set headerEBit.
    uint32_t headerExtendedCodeWords   = 0;

    if (ufiSetEBit)
    {
        headerEBit        = 1;
        headerEpilogCount = ufiEpilogList->GetStartIndex(); // probably zero -- the start of the prolog codes!
        headerCodeWords   = ufiCodeWords;
    }
    else
    {
        headerEBit = 0;

        if (ufiNeedExtendedCodeWordsEpilogCount)
        {
            headerEpilogCount         = 0;
            headerCodeWords           = 0;
            headerExtendedEpilogCount = ufiEpilogScopes;
            headerExtendedCodeWords   = ufiCodeWords;
        }
        else
        {
            headerEpilogCount = ufiEpilogScopes;
            headerCodeWords   = ufiCodeWords;
        }
    }

    // Start writing the header

    noway_assert(headerFunctionLength <=
                 0x3FFFFU); // We create fragments to prevent this from firing, so if it hits, we have an internal error

    if ((headerEpilogCount > UW_MAX_EPILOG_COUNT) || (headerCodeWords > UW_MAX_CODE_WORDS_COUNT))
    {
        IMPL_LIMITATION("unwind data too large");
    }

#ifdef TARGET_ARM
    uint32_t header = headerFunctionLength | (headerVers << 18) | (headerXBit << 20) | (headerEBit << 21) |
                      (headerFBit << 22) | (headerEpilogCount << 23) | (headerCodeWords << 28);
#else
    uint32_t header = headerFunctionLength | (headerVers << 18) | (headerXBit << 20) | (headerEBit << 21) |
                      (headerEpilogCount << 22) | (headerCodeWords << 27);
#endif

    ufiPrologCodes.AddHeaderWord(header);

    // Construct the second header word, if needed

    if (ufiNeedExtendedCodeWordsEpilogCount)
    {
        noway_assert(headerEBit == 0);
        noway_assert(headerEpilogCount == 0);
        noway_assert(headerCodeWords == 0);
        noway_assert((headerExtendedEpilogCount > UW_MAX_EPILOG_COUNT) ||
                     (headerExtendedCodeWords > UW_MAX_CODE_WORDS_COUNT));

        if ((headerExtendedEpilogCount > UW_MAX_EXTENDED_EPILOG_COUNT) ||
            (headerExtendedCodeWords > UW_MAX_EXTENDED_CODE_WORDS_COUNT))
        {
            IMPL_LIMITATION("unwind data too large");
        }

        uint32_t header2 = headerExtendedEpilogCount | (headerExtendedCodeWords << 16);

        ufiPrologCodes.AddHeaderWord(header2);
    }

    // Construct the epilog scope words, if needed

    if (!ufiSetEBit)
    {
        for (UnwindEpilogInfo* epilog = ufiEpilogList; epilog != nullptr; epilog = epilog->epiNext)
        {
            uint32_t epilogStartOffset = epilog->GetStartLocation().GetCodeOffset();

            // The epilog must strictly follow the prolog. The prolog is in the first fragment of
            // the hot section. If this epilog is at the start of a fragment, it can't be the
            // first fragment in the hot section. We actually don't know if we're processing
            // the hot or cold section (or a funclet), so we can't distinguish these cases. Thus,
            // we just assert that the epilog starts within the fragment.
            assert(epilogStartOffset >= startOffset);

            // We report the offset of an epilog as the offset from the beginning of the function/funclet fragment,
            // NOT the offset from the beginning of the main function.
            uint32_t headerEpilogStartOffset = epilogStartOffset - startOffset;

#ifdef TARGET_ARM
            noway_assert((headerEpilogStartOffset & 1) == 0);
            headerEpilogStartOffset /= 2; // The unwind data stores the actual offset divided by 2 (since the low bit of
                                          // the actual offset is always zero)
#else
            noway_assert((headerEpilogStartOffset & 3) == 0);
            headerEpilogStartOffset /= 4; // The unwind data stores the actual offset divided by 4 (since the low 2 bits
                                          // of the actual offset is always zero)
#endif

            uint32_t headerEpilogStartIndex = epilog->GetStartIndex();

            if ((headerEpilogStartOffset > UW_MAX_EPILOG_START_OFFSET) ||
                (headerEpilogStartIndex > UW_MAX_EPILOG_START_INDEX))
            {
                IMPL_LIMITATION("unwind data too large");
            }

#ifdef TARGET_ARM
            // The epilog is unconditional. We don't have epilogs under the IT instruction.
            const uint32_t headerCondition = 0xE;
            uint32_t       epilogScopeWord =
                headerEpilogStartOffset | (headerCondition << 20) | (headerEpilogStartIndex << 24);
#else
            uint32_t epilogScopeWord = headerEpilogStartOffset | (headerEpilogStartIndex << 22);
#endif

            ufiPrologCodes.AddHeaderWord(epilogScopeWord);
        }
    }

    // The unwind code words are already here, following the header, so we're done!
}

void UnwindFragmentInfo::Reserve(CodeGen* codeGen, FuncKind kind, bool isHotCode)
{
    assert(isHotCode || (kind == FUNC_ROOT)); // TODO-CQ: support hot/cold splitting in functions with EH

    MergeCodes();

    DBEXEC(ufiNum != 1, JITDUMP("reserveUnwindInfo: fragment #%d:\n", ufiNum));

    codeGen->eeReserveUnwindInfo(kind != FUNC_ROOT, isHotCode, Size());
}

void UnwindFragmentInfo::Allocate(CodeGen* codeGen, FuncKind kind, CodeRange range, bool isHotCode)
{
    noway_assert(isHotCode || (kind == FUNC_ROOT)); // TODO-CQ: support funclets in cold code
    assert(range.end > range.start);

    uint32_t codeSize = range.end - range.start;

    Finalize(range.start, codeSize);

    uint8_t* unwindBlock;
    uint32_t unwindSize;
    ufiPrologCodes.GetFinalInfo(&unwindBlock, &unwindSize);

    DBEXEC(ufiNum != 1, JITDUMP("unwindEmit: fragment #%d:\n", ufiNum));

    codeGen->eeAllocUnwindInfo(kind, isHotCode, range, unwindSize, unwindBlock);
}

UnwindInfo::UnwindInfo(Compiler* comp, insGroup* start, insGroup* end)
    : uwiFragmentFirst(comp, start, end, false)
#ifdef DEBUG
    , uwiInitialized(true)
#endif
{
}

void UnwindInfo::SplitColdCodes(UnwindInfo* hotInfo)
{
    assert(uwiFragmentFirst.ufiNext == nullptr);
    assert(hotInfo->uwiFragmentFirst.ufiNext == nullptr);

    uwiFragmentFirst.ufiHasPhantomProlog = true;
    uwiFragmentFirst.CopyPrologCodes(hotInfo->uwiFragmentFirst);
    uwiFragmentFirst.SplitEpilogs(&hotInfo->uwiFragmentFirst);
}

// Split the function or funclet into fragments that are no larger than 512K,
// so the fragment size will fit in the unwind data "Function Length" field.
// The ARM Exception Data specification "Function Fragments" section describes this.
// We split the function so that it is no larger than 512K bytes, or the value of
// the COMPlus_JitSplitFunctionSize value, if defined (and smaller). We must determine
// how to split the function/funclet before we issue the instructions, so we can
// reserve the unwind space with the VM. The instructions issued may shrink (but not
// expand!) during issuing (although this is extremely rare in any case, and may not
// actually occur on ARM), so we don't finalize actual sizes or offsets.
//
// ARM64 has very similar limitations, except functions can be up to 1MB.
// TODO-ARM64-Bug?: make sure this works!
//
// We don't split any prolog or epilog. Ideally, we might not split an instruction,
// although that doesn't matter because the unwind at any point would still be
// well-defined.
void UnwindInfo::SplitLargeFragment(CodeGen* codeGen)
{
    uint32_t maxFragmentSize = UW_MAX_FRAGMENT_SIZE_BYTES;

#ifdef DEBUG
    if (unsigned splitFunctionSize = static_cast<unsigned>(JitConfig.JitSplitFunctionSize()))
    {
        maxFragmentSize = Min(maxFragmentSize, splitFunctionSize);
    }
#endif

    // Now, there should be exactly one fragment.
    assert(uwiFragmentFirst.ufiNext == nullptr);

    // Find the code size of this function/funclet.
    insGroup* startLoc = uwiFragmentFirst.ufiStartLoc;
    insGroup* endLoc   = uwiFragmentFirst.ufiEndLoc;

    uint32_t startOffset = startLoc->GetCodeOffset();
    uint32_t endOffset   = endLoc == nullptr ? codeGen->GetCodeSize() : endLoc->GetCodeOffset();

    assert(endOffset > startOffset);

    uint32_t codeSize      = endOffset - startOffset;
    uint32_t fragmentCount = (codeSize + maxFragmentSize - 1) / maxFragmentSize; // round up

    if (fragmentCount == 1)
    {
        return;
    }

    JITDUMP("Split unwind info into %d fragments (function/funclet size: %d, maximum fragment size: %d)\n",
            fragmentCount, codeSize, maxFragmentSize);

    Split(startLoc, endLoc, maxFragmentSize);

#ifdef DEBUG
    unsigned actualFragmentCount = 0;

    for (UnwindFragmentInfo* f = &uwiFragmentFirst; f != nullptr; f = f->ufiNext)
    {
        ++actualFragmentCount;
    }

    if (actualFragmentCount < fragmentCount)
    {
        JITDUMP("WARNING: asked the emitter for %u fragments, but only got %u\n", fragmentCount, actualFragmentCount);

        // We might not be able to split into as many fragments as asked for, because we
        // only split along instruction group boundaries. The maximum instruction group
        // code size is very small compared to UW_MAX_FRAGMENT_SIZE_BYTES so this is only
        // an issue if JitSplitFunctionSize was used and set to a low value, in which case
        // we don't need to assert.
        assert(maxFragmentSize != UW_MAX_FRAGMENT_SIZE_BYTES);
    }
#endif // DEBUG
}

void UnwindInfo::Split(insGroup* start, insGroup* end, uint32_t maxCodeSize)
{
    UnwindFragmentInfo* lastFragment = &uwiFragmentFirst;

    insGroup* prevFragmentStart = start;
    insGroup* prevCandidate     = nullptr;
    insGroup* prev              = nullptr;
    uint32_t  currentSize       = 0;
    uint32_t  prevSize          = 0;

    for (insGroup* ig = start; ig != end && ig != nullptr; ig = ig->igNext)
    {
        if (currentSize >= maxCodeSize)
        {
            bool useCandidate = true;

            if (prevCandidate == nullptr)
            {
                JITDUMP("UnwindInfoSplit: can't split at " FMT_IG "; we don't have a candidate to report\n",
                        ig->GetId());
                useCandidate = false;
            }
            else if (prevCandidate == prevFragmentStart)
            {
                JITDUMP("UnwindInfoSplit: can't split at " FMT_IG "; we already reported it\n", prevCandidate->GetId());
                useCandidate = false;
            }

            if (useCandidate)
            {
                if (prevSize >= maxCodeSize)
                {
                    JITDUMP("UnwindInfoSplit: split at " FMT_IG
                            " is size %u, larger than requested maximum size of %u\n",
                            prevCandidate->GetId(), prevSize, maxCodeSize);
                }

                lastFragment = SplitFragment(lastFragment, prevCandidate);

                prevFragmentStart = prevCandidate;
                prevCandidate     = nullptr;
                currentSize -= prevSize;
            }
        }

        // Update the current prev candidate to be this block, if it isn't in the middle of
        // a prolog or epilog, which we can't split. All we know is that certain IGs are
        // marked as prolog or epilog. We don't actually know if two adjacent IGs are part
        // of the *same* prolog or epilog, so we have to assume they are.

        // TODO-MIKE-Review: Should this check for funclet epilogs as well? But then the
        // thing is suspect, as this code doesn't really split any instruction groups
        // and prologs/epilogs should have only one instruction group, and then this code
        // would be pointless. Though some other parts of the code seem to think that only
        // the main prolog is limited to only one instruction group, but it's unlikely
        // that funclet prologs or any epilogs can have more than one instruction group.

        if ((prev != nullptr) &&
            ((prev->IsFuncletProlog() && ig->IsFuncletProlog()) || (prev->IsMainEpilog() && ig->IsMainEpilog())))
        {
            // We can't update the prev candidate.
        }
        else
        {
            prevCandidate = ig;
            prevSize      = currentSize;
        }

        prev = ig;
        currentSize += ig->igSize;
    }
}

// Reserve space for the unwind info for all fragments
void UnwindInfo::Reserve(CodeGen* codeGen, FuncKind kind, bool isHotCode)
{
    assert(uwiInitialized);
    assert(isHotCode || (kind == FUNC_ROOT));

    // First we need to split the function or funclet into fragments that are no larger
    // than 512K, so the fragment size will fit in the unwind data "Function Length" field.
    // The ARM Exception Data specification "Function Fragments" section describes this.
    SplitLargeFragment(codeGen);

    for (UnwindFragmentInfo* f = &uwiFragmentFirst; f != nullptr; f = f->ufiNext)
    {
        f->Reserve(codeGen, kind, isHotCode);
    }
}

// Allocate and populate VM unwind info for all fragments
void UnwindInfo::Allocate(CodeGen* codeGen, FuncKind kind, bool isHotCode)
{
    assert(uwiInitialized);
    DBEXEC(codeGen->compiler->verbose, Dump(isHotCode, 0));

    for (UnwindFragmentInfo* f = &uwiFragmentFirst; f != nullptr; f = f->ufiNext)
    {
        uint32_t startOffset = f->GetStartLoc()->GetCodeOffset();
        uint32_t endOffset   = f->ufiEndLoc == nullptr ? codeGen->GetCodeSize() : f->ufiEndLoc->GetCodeOffset();

        f->Allocate(codeGen, kind, {startOffset, endOffset}, isHotCode);
        startOffset = endOffset;
    }
}

UnwindEpilogInfo* UnwindInfo::AddEpilog(CodeGen* codeGen)
{
    assert(uwiInitialized);
    return uwiFragmentFirst.AddEpilog(codeGen);
}

UnwindFragmentInfo* UnwindInfo::SplitFragment(UnwindFragmentInfo* frag, insGroup* newStart)
{
    assert(uwiInitialized);

    Compiler* compiler = uwiFragmentFirst.GetCompiler();
    insGroup* oldEnd   = frag->ufiEndLoc;

    UnwindFragmentInfo* newFrag = new (compiler, CMK_UnwindInfo) UnwindFragmentInfo(compiler, newStart, oldEnd, true);
    INDEBUG(newFrag->ufiNum = frag->ufiNum + 1);
    newFrag->CopyPrologCodes(uwiFragmentFirst);
    newFrag->SplitEpilogs(frag);

    frag->ufiNext   = newFrag;
    frag->ufiEndLoc = newStart;

    return newFrag;
}

#ifdef DEBUG

void UnwindPrologCodes::Dump(int indent)
{
    printf("%*sUnwindPrologCodes:\n", indent, "");
    printf("%*s  upcMemSize: %d\n", indent, "", upcMemSize);
    printf("%*s  upcCodeSlot: %d\n", indent, "", upcCodeSlot);
    printf("%*s  upcHeaderSlot: %d\n", indent, "", upcHeaderSlot);
    printf("%*s  upcEpilogSlot: %d\n", indent, "", upcEpilogSlot);
    printf("%*s  upcUnwindBlockSlot: %d\n", indent, "", upcUnwindBlockSlot);

    if (upcMemSize > 0)
    {
        printf("%*s  codes:", indent, "");
        for (int i = 0; i < upcMemSize; i++)
        {
            printf(" %02x", upcMem[i]);
            if (i == upcCodeSlot)
                printf(" <-C");
            else if (i == upcHeaderSlot)
                printf(" <-H");
            else if (i == upcEpilogSlot)
                printf(" <-E");
            else if (i == upcUnwindBlockSlot)
                printf(" <-U");
        }
        printf("\n");
    }
}

void UnwindEpilogCodes::Dump(int indent)
{
    printf("%*sUnwindEpilogCodes:\n", indent, "");
    printf("%*s  uecMemSize: %d\n", indent, "", uecMemSize);
    printf("%*s  uecCodeSlot: %d\n", indent, "", uecCodeSlot);
    printf("%*s  uecFinalized: %d\n", indent, "", uecFinalized);

    if (uecMemSize > 0)
    {
        printf("%*s  codes:", indent, "");
        for (int i = 0; i < uecMemSize; i++)
        {
            printf(" %02x", uecMem[i]);
            if (i == uecCodeSlot)
                printf(" <-C"); // Indicate the current pointer
        }
        printf("\n");
    }
}

void UnwindEpilogInfo::Dump(int indent)
{
    printf("%*sUnwindEpilogInfo:\n", indent, "");
    printf("%*s  epiStartLoc: ", indent, "");
    epiStartLoc.Print();
    printf("%*s  epiMatches: %d\n", indent, "", epiMatches);
    printf("%*s  epiStartIndex: %d\n", indent, "", epiStartIndex);

    epiCodes.Dump(indent + 2);
}

void UnwindFragmentInfo::Dump(int indent)
{
    unsigned count = 0;

    for (UnwindEpilogInfo* e = ufiEpilogList; e != nullptr; e = e->epiNext)
    {
        ++count;
    }

    printf("%*sUnwindFragmentInfo #%d\n", indent, "", ufiNum);
    printf("%*s  ufiHasPhantomProlog: %d\n", indent, "", ufiHasPhantomProlog);
    printf("%*s  %d epilog%s\n", indent, "", count, (count != 1) ? "s" : "");
    printf("%*s  ufiSize: %u\n", indent, "", ufiSize);
    printf("%*s  ufiSetEBit: %d\n", indent, "", ufiSetEBit);
    printf("%*s  ufiNeedExtendedCodeWordsEpilogCount: %d\n", indent, "", ufiNeedExtendedCodeWordsEpilogCount);
    printf("%*s  ufiCodeWords: %u\n", indent, "", ufiCodeWords);
    printf("%*s  ufiEpilogScopes: %u\n", indent, "", ufiEpilogScopes);
    printf("%*s  ufiInitialized: %d\n", indent, "", ufiInitialized);

    ufiPrologCodes.Dump(indent + 2);

    for (UnwindEpilogInfo* e = ufiEpilogList; e != nullptr; e = e->epiNext)
    {
        e->Dump(indent + 2);
    }
}

void UnwindInfo::Dump(bool isHotCode, int indent)
{
    unsigned count = 0;

    for (UnwindFragmentInfo* f = &uwiFragmentFirst; f != nullptr; f = f->ufiNext)
    {
        ++count;
    }

    printf("%*sUnwindInfo%s:\n", indent, "", isHotCode ? "" : " COLD");
    printf("%*s  %d fragment%s\n", indent, "", count, (count != 1) ? "s" : "");
    printf("\n%*s  uwiInitialized: 0x%08x\n", indent, "", uwiInitialized);

    for (UnwindFragmentInfo* f = &uwiFragmentFirst; f != nullptr; f = f->ufiNext)
    {
        f->Dump(indent + 2);
    }
}

#endif // DEBUG

#ifdef TARGET_ARM

#ifdef TARGET_UNIX
int16_t CodeGen::mapRegNumToDwarfReg(RegNum reg)
{
    switch (reg)
    {
        case REG_R0:
            return 0;
        case REG_R1:
            return 1;
        case REG_R2:
            return 2;
        case REG_R3:
            return 3;
        case REG_R4:
            return 4;
        case REG_R5:
            return 5;
        case REG_R6:
            return 6;
        case REG_R7:
            return 7;
        case REG_R8:
            return 8;
        case REG_R9:
            return 9;
        case REG_R10:
            return 10;
        case REG_R11:
            return 11;
        case REG_R12:
            return 12;
        case REG_R13:
            return 13;
        case REG_R14:
            return 14;
        case REG_R15:
            return 15;
        case REG_F0:
            return 256;
        case REG_F2:
            return 257;
        case REG_F4:
            return 258;
        case REG_F6:
            return 259;
        case REG_F8:
            return 260;
        case REG_F10:
            return 261;
        case REG_F12:
            return 262;
        case REG_F14:
            return 263;
        case REG_F16:
            return 264;
        case REG_F18:
            return 265;
        case REG_F20:
            return 266;
        case REG_F22:
            return 267;
        case REG_F24:
            return 268;
        case REG_F26:
            return 269;
        case REG_F28:
            return 270;
        case REG_F30:
            return 271;
        default:
            unreached();
    }
}
#endif // TARGET_UNIX

#ifdef DEBUG

// Return the opcode size of an instruction, in bytes, given the first byte of its corresponding unwind code.
static unsigned GetOpcodeSizeFromUnwindHeader(uint8_t b1)
{
    static const uint8_t s_UnwindOpsize[256]{
        // array of opsizes, in bytes (as specified in the ARM unwind specification)
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 00-0F
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 10-1F
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 20-2F
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 30-3F
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 40-4F
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 50-5F
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 60-6F
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 70-7F
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, // 80-8F
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, // 90-9F
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, // A0-AF
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, // B0-BF
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // C0-CF
        2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, // D0-DF
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 2, 2, 4, // E0-EF
        0, 0, 0, 0, 0, 4, 4, 2, 2, 4, 4, 2, 4, 2, 4, 0  // F0-FF
    };

    unsigned opsize = s_UnwindOpsize[b1];
    // We shouldn't get a code with no opsize (the 0xFF end code is handled specially)
    assert(opsize == 2 || opsize == 4);
    return opsize;
}

// Return the size of the unwind code (from 1 to 4 bytes), given the first byte of the unwind bytes
static unsigned GetUnwindSizeFromUnwindHeader(uint8_t b1)
{
    static const uint8_t s_UnwindSize[256]{
        // array of unwind sizes, in bytes (as specified in the ARM unwind specification)
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 00-0F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 10-1F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 20-2F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 30-3F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 40-4F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 50-5F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 60-6F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 70-7F
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 80-8F
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 90-9F
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // A0-AF
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // B0-BF
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // C0-CF
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // D0-DF
        1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, // E0-EF
        1, 1, 1, 1, 1, 2, 2, 3, 4, 3, 4, 1, 1, 1, 1, 1  // F0-FF
    };

    unsigned size = s_UnwindSize[b1];
    assert(1 <= size && size <= 4);
    return size;
}

// Walk the prolog codes and calculate the size of the prolog or epilog, in bytes.
// The 0xFD and 0xFE "end + NOP" codes need to be handled differently between
// the prolog and epilog. They count as pure "end" codes in a prolog, but they
// count as 16 and 32 bit NOPs (respectively), as well as an "end", in an epilog.
unsigned UnwindCodes::GetCodeSizeFromUnwindCodes(bool isProlog, const uint8_t* codes)
{
    unsigned size = 0;

    for (const uint8_t* c = codes;;)
    {
        uint8_t b1 = *c;

        if (b1 >= 0xFD)
        {
            // 0xFD, 0xFE, 0xFF are "end" codes

            if (!isProlog && (b1 == 0xFD || b1 == 0xFE))
            {
                // Count the special "end + NOP" code size in the epilog
                size += GetOpcodeSizeFromUnwindHeader(b1);
            }

            break; // We hit an "end" code; we're done
        }

        size += GetOpcodeSizeFromUnwindHeader(b1);
        c += GetUnwindSizeFromUnwindHeader(b1);

        assert(c - codes < 256); // 255 is the absolute maximum number of code bytes allowed
    }

    return size;
}

// Given the first byte of the unwind code, check that its opsize matches
// the last instruction added in the emitter.
void UnwindInfo::CheckOpsize(uint8_t b1)
{
    // Adding NOP padding goes through the same path, but doesn't update the location to indicate
    // the correct location of the instruction for which we are adding a NOP, so just skip the
    // assert. Should be ok, because the emitter is telling us the size of the instruction for
    // which we are adding the NOP.
    if (!uwiAddingNOP)
    {
        unsigned opsizeInBytes = GetOpcodeSizeFromUnwindHeader(b1);
        unsigned instrSizeInBytes =
            static_cast<CodeGen*>(uwiFragmentFirst.GetCompiler()->codeGen)->unwindGetInstructionSize();

        assert(opsizeInBytes == instrSizeInBytes);
    }
}

// start is 0-based index from LSB, length is number of bits
static uint32_t ExtractBits(uint32_t dw, uint32_t start, uint32_t length)
{
    return (dw >> start) & ((1 << length) - 1);
}

// Dump an integer register set. 'x' is an array of bits where bit 0 = r0, bit 1 = r1, etc.
// The highest register considered is r12.
// If 'lr' is non-zero, the "lr" register is emitted last.
// Returns the number of characters printed.
static uint32_t DumpIntRegSet(uint32_t x, uint32_t lr)
{
    assert(x != 0 || lr != 0); // we must have one
    assert((x & 0xE000) == 0); // don't handle r13 (sp), r14 (lr), r15 (pc) in 'x'
    uint32_t printed = 0;

    printf("{");
    ++printed;
    bool     first   = true;
    uint32_t bitMask = 1;
    for (uint32_t bitNum = 0; bitNum < 12; bitNum++)
    {
        if (x & bitMask)
        {
            if (!first)
            {
                printf(",");
                ++printed;
            }
            printf("r%u", bitNum);
            printed += (bitNum < 10) ? 2 : 3;
            first = false;
        }
        bitMask <<= 1;
    }
    if (lr)
    {
        if (!first)
        {
            printf(",");
            ++printed;
        }
        printf("lr");
        printed += 2;
    }
    printf("}");
    ++printed;

    return printed;
}

// Dump a register set range from register 'start' to register 'end'.
// rtype should be "r" or "d" to indicate register type.
// If 'lr' is non-zero, the "lr" register is emitted last. (Note that
// 'lr' should be zero for rtype == "d".)
// Returns the number of characters printed.
static uint32_t DumpRegSetRange(const char* const rtype, uint32_t start, uint32_t end, uint32_t lr)
{
    assert(start <= end);
    uint32_t printed  = 0;
    uint32_t rtypeLen = (uint32_t)strlen(rtype);

    printf("{");
    ++printed;
    bool first = true;
    for (uint32_t reg = start; reg <= end; reg++)
    {
        if (!first)
        {
            printf(",");
            ++printed;
        }
        printf("%s%u", rtype, reg);
        printed += rtypeLen + ((reg < 10) ? 1 : 2);
        first = false;
    }
    if (lr)
    {
        assert(!first); // If 'lr' is set, it can't be first, since we require a non-empty range
        printf(",lr");
        printed += 3;
    }
    printf("}");
    ++printed;

    return printed;
}

// Dump the opsize.
// Returns the number of characters printed.
static uint32_t DumpOpsize(uint32_t padding, uint32_t opsize)
{
    if (padding > 100) // underflow?
        padding      = 4;
    uint32_t printed = padding;
    for (; padding > 0; padding--)
        printf(" ");
    printf("; opsize %d\n", opsize);
    return printed + 11; // assumes opsize is always 2 digits
}

void CodeGen::DumpUnwindInfo(bool isHotCode, CodeRange range, const uint8_t* header, uint32_t unwindSize) const
{
    printf("Unwind Info%s:\n", isHotCode ? "" : " COLD");

    // header is not guaranteed to be aligned. We put four 0xFF end codes at the end
    // to provide padding, and round down to get a multiple of 4 bytes in size.
    uint32_t UNALIGNED* pdw = (uint32_t UNALIGNED*)header;
    uint32_t dw;

    dw = *pdw++;

    uint32_t codeWords      = ExtractBits(dw, 28, 4);
    uint32_t epilogCount    = ExtractBits(dw, 23, 5);
    uint32_t FBit           = ExtractBits(dw, 22, 1);
    uint32_t EBit           = ExtractBits(dw, 21, 1);
    uint32_t XBit           = ExtractBits(dw, 20, 1);
    uint32_t Vers           = ExtractBits(dw, 18, 2);
    uint32_t functionLength = ExtractBits(dw, 0, 18);

    printf("  >> Start offset   : 0x%06x (not in unwind data)\n", compiler->dspOffset(range.start));
    printf("  >>   End offset   : 0x%06x (not in unwind data)\n", compiler->dspOffset(range.end));
    printf("  Code Words        : %u\n", codeWords);
    printf("  Epilog Count      : %u\n", epilogCount);
    printf("  F bit             : %u\n", FBit);
    printf("  E bit             : %u\n", EBit);
    printf("  X bit             : %u\n", XBit);
    printf("  Vers              : %u\n", Vers);
    printf("  Function Length   : %u (0x%05x) Actual length = %u (0x%06x)\n", functionLength, functionLength,
           functionLength * 2, functionLength * 2);

    assert(functionLength * 2 == range.end - range.start);

    if (codeWords == 0 && epilogCount == 0)
    {
        // We have an extension word specifying a larger number of Code Words or Epilog Counts
        // than can be specified in the header word.

        dw = *pdw++;

        codeWords   = ExtractBits(dw, 16, 8);
        epilogCount = ExtractBits(dw, 0, 16);
        assert((dw & 0xF0000000) == 0); // reserved field should be zero

        printf("  ---- Extension word ----\n");
        printf("  Extended Code Words        : %u\n", codeWords);
        printf("  Extended Epilog Count      : %u\n", epilogCount);
    }

    bool epilogStartAt[256] = {}; // One byte per possible epilog start index; initialized to false

    if (EBit == 0)
    {
        // We have an array of epilog scopes

        printf("  ---- Epilog scopes ----\n");
        if (epilogCount == 0)
        {
            printf("  No epilogs\n");
        }
        else
        {
            for (uint32_t scope = 0; scope < epilogCount; scope++)
            {
                dw = *pdw++;

                uint32_t epilogStartOffset = ExtractBits(dw, 0, 18);
                uint32_t res               = ExtractBits(dw, 18, 2);
                uint32_t condition         = ExtractBits(dw, 20, 4);
                uint32_t epilogStartIndex  = ExtractBits(dw, 24, 8);

                // Note that epilogStartOffset for a funclet is the offset from the beginning
                // of the current funclet, not the offset from the beginning of the main function.
                // To help find it when looking through JitDump output, also show the offset from
                // the beginning of the main function.
                uint32_t epilogStartOffsetFromMainFunctionBegin = epilogStartOffset * 2 + range.start;

                assert(res == 0);

                printf("  ---- Scope %d\n", scope);
                printf("  Epilog Start Offset        : %u (0x%05x) Actual offset = %u (0x%06x) Offset from main "
                       "function begin = %u (0x%06x)\n",
                       compiler->dspOffset(epilogStartOffset), compiler->dspOffset(epilogStartOffset),
                       compiler->dspOffset(epilogStartOffset * 2), compiler->dspOffset(epilogStartOffset * 2),
                       compiler->dspOffset(epilogStartOffsetFromMainFunctionBegin),
                       compiler->dspOffset(epilogStartOffsetFromMainFunctionBegin));
                printf("  Condition                  : %u (0x%x)%s\n", condition, condition,
                       (condition == 0xE) ? " (always)" : "");
                printf("  Epilog Start Index         : %u (0x%02x)\n", epilogStartIndex, epilogStartIndex);

                epilogStartAt[epilogStartIndex] = true; // an epilog starts at this offset in the unwind codes
            }
        }
    }
    else
    {
        printf("  --- One epilog, unwind codes at %u\n", epilogCount);
        assert(epilogCount < _countof(epilogStartAt));
        epilogStartAt[epilogCount] = true; // the one and only epilog starts its unwind codes at this offset
    }

    if (FBit)
    {
        printf("  ---- Note: 'F' bit is set. Prolog codes are for a 'phantom' prolog.\n");
    }

    // Dump the unwind codes

    printf("  ---- Unwind codes ----\n");

    uint32_t countOfUnwindCodes = codeWords * 4;
    PBYTE    pUnwindCode        = (PBYTE)pdw;
    uint8_t  b1, b2, b3, b4;
    uint32_t x, y;
    uint32_t opsize;
    uint32_t opCol = 52;
    uint32_t printed;
    for (uint32_t i = 0; i < countOfUnwindCodes; i++)
    {
        // Does this byte start an epilog sequence? If so, note that fact.
        if (epilogStartAt[i])
        {
            printf("    ---- Epilog start at index %u ----\n", i);
        }

        b1 = *pUnwindCode++;

        if ((b1 & 0x80) == 0)
        {
            // 00-7F : add sp, sp, #X*4 (opsize 16)
            x = b1 & 0x7F;
            printf("    %02X          add sp, sp, #%-8d", b1, x * 4);
            DumpOpsize(opCol - 37, 16);
        }
        else if ((b1 & 0xC0) == 0x80)
        {
            // 80-BF : pop {r0-r12,lr} (X = bitmask) (opsize 32)
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            uint32_t LBit = ExtractBits(b1, 5, 1);
            x             = ((uint32_t)(b1 & 0x1F) << 8) | (uint32_t)b2;

            printf("    %02X %02X       pop ", b1, b2);
            printed = 20;
            printed += DumpIntRegSet(x, LBit);
            DumpOpsize(opCol - printed, 32);
        }
        else if ((b1 & 0xF0) == 0xC0)
        {
            // C0-CF : mov sp, rX (X=0-15) (opsize 16)
            x = b1 & 0xF;
            printf("    %02X          mov sp, r%u", b1, x);
            printed = 25 + ((x > 10) ? 2 : 1);
            DumpOpsize(opCol - printed, 16);
        }
        else if ((b1 & 0xF8) == 0xD0)
        {
            // D0-D7 : pop {r4-rX,lr} (X=4-7) (opsize 16)
            x             = b1 & 0x3;
            uint32_t LBit = b1 & 0x4;
            printf("    %02X          pop ", b1);
            printed = 20;
            printed += DumpRegSetRange("r", 4, x + 4, LBit);
            DumpOpsize(opCol - printed, 16);
        }
        else if ((b1 & 0xF8) == 0xD8)
        {
            // D8-DF : pop {r4-rX,lr} (X=8-11) (opsize 32)
            x             = b1 & 0x3;
            uint32_t LBit = b1 & 0x4;
            printf("    %02X          pop ", b1);
            printed = 20;
            printed += DumpRegSetRange("r", 4, x + 8, LBit);
            DumpOpsize(opCol - printed, 32);
        }
        else if ((b1 & 0xF8) == 0xE0)
        {
            // E0-E7 : vpop {d8-dX} (X=8-15) (opsize 32)
            x = b1 & 0x7;
            printf("    %02X          vpop ", b1);
            printed = 21;
            printed += DumpRegSetRange("d", 8, x + 8, 0);
            DumpOpsize(opCol - printed, 32);
        }
        else if ((b1 & 0xFC) == 0xE8)
        {
            // E8-EB : addw sp, sp, #X*4 (opsize 32)
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            x = ((uint32_t)(b1 & 0x3) << 8) | (uint32_t)b2;

            printf("    %02X %02X       addw sp, sp, #%-8u", b1, b2, x * 4);
            DumpOpsize(opCol - 38, 32);
        }
        else if ((b1 & 0xFE) == 0xEC)
        {
            // EC-ED : pop {r0-r7,lr} (X = bitmask) (opsize 16)
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            uint32_t LBit = ExtractBits(b1, 0, 1);
            x             = (uint32_t)b2;

            printf("    %02X %02X       pop ", b1, b2);
            printed = 20;
            printed += DumpIntRegSet(x, LBit);
            DumpOpsize(opCol - printed, 16);
        }
        else if (b1 == 0xEE)
        {
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            if ((b2 & 0xF0) == 0)
            {
                // EE/0x (opsize 16)
                x = b2 & 0xF;
                printf("    %02X %02X       Microsoft-specific (x = %02X)", b1, b2, x);
                DumpOpsize(4, 16);
            }
            else
            {
                // EE/xy (opsize 16)
                x = ExtractBits(b2, 4, 4);
                y = ExtractBits(b2, 0, 4);
                printf("    %02X %02X       Available (x = %02X, y = %02X)", b1, b2, x, y);
                DumpOpsize(4, 16);
            }
        }
        else if (b1 == 0xEF)
        {
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            if ((b2 & 0xF0) == 0)
            {
                // EF/0x : ldr lr, [sp], #X*4 (opsize 32)
                x = b2 & 0xF;
                printf("    %02X %02X       ldr lr, [sp], #%-8u", b1, b2, x * 4);
                DumpOpsize(opCol - 39, 32);
            }
            else
            {
                // EF/xy (opsize 32)
                x = ExtractBits(b2, 4, 4);
                y = ExtractBits(b2, 0, 4);
                printf("    %02X %02X       Available (x = %02X, y = %02X)", b1, b2, x, y);
                DumpOpsize(4, 32);
            }
        }
        else if ((b1 >= 0xF0) && (b1 <= 0xF4))
        {
            // F0-F4
            x = b1 & 0x7;
            printf("    %02X          Available (x = %02X)\n", b1, x);
        }
        else if (b1 == 0xF5)
        {
            // F5 : vpop {dS-dE} (opsize 32)

            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            uint32_t s = ExtractBits(b2, 4, 4);
            uint32_t e = ExtractBits(b2, 0, 4);

            printf("    %02X %02X       vpop ", b1, b2);
            printed = 21;
            printed += DumpRegSetRange("d", s, e, 0);
            DumpOpsize(opCol - printed, 32);
        }
        else if (b1 == 0xF6)
        {
            // F6 : vpop {d(S+16)-d(E+16)} (opsize 32)

            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            uint32_t s = ExtractBits(b2, 4, 4);
            uint32_t e = ExtractBits(b2, 0, 4);

            printf("    %02X %02X       vpop ", b1, b2);
            printed = 21;
            printed += DumpRegSetRange("d", s + 16, e + 16, 0);
            DumpOpsize(opCol - printed, 32);
        }
        else if (b1 == 0xF7 || b1 == 0xF9)
        {
            // F7, F9 : add sp, sp, #X*4
            // 0xF7 has opsize 16, 0xF9 has opsize 32

            assert(i + 2 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            b3 = *pUnwindCode++;
            i += 2;

            x = ((uint32_t)b2 << 8) | (uint32_t)b3;

            opsize = (b1 == 0xF7) ? 16 : 32;

            printf("    %02X %02X %02X    add sp, sp, #%-8u", b1, b2, b3, x * 4, opsize);
            DumpOpsize(opCol - 37, opsize);
        }
        else if (b1 == 0xF8 || b1 == 0xFA)
        {
            // F8, FA : add sp, sp, #X*4
            // 0xF8 has opsize 16, 0xFA has opsize 32

            assert(i + 3 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            b3 = *pUnwindCode++;
            b4 = *pUnwindCode++;
            i += 3;

            x = ((uint32_t)b2 << 16) | ((uint32_t)b3 << 8) | (uint32_t)b4;

            opsize = (b1 == 0xF8) ? 16 : 32;

            printf("    %02X %02X %02X %02X add sp, sp, #%-8u", b1, b2, b3, b4, x * 4, opsize);
            DumpOpsize(opCol - 37, opsize);
        }
        else if (b1 == 0xFB || b1 == 0xFC)
        {
            // FB, FC : nop
            // 0xFB has opsize 16, 0xFC has opsize 32

            opsize = (b1 == 0xFB) ? 16 : 32;

            printf("    %02X          nop", b1, opsize);
            DumpOpsize(opCol - 19, opsize);
        }
        else if (b1 == 0xFD || b1 == 0xFE)
        {
            // FD, FE : end + nop
            // 0xFD has opsize 16, 0xFE has opsize 32

            opsize = (b1 == 0xFD) ? 16 : 32;

            printf("    %02X          end + nop", b1, opsize);
            DumpOpsize(opCol - 25, opsize);
        }
        else if (b1 == 0xFF)
        {
            // FF : end

            printf("    %02X          end\n", b1);
        }
        else
        {
            assert(!"Internal error decoding unwind codes");
        }
    }

    pdw += codeWords;
    assert((PBYTE)pdw == pUnwindCode);
    assert((PBYTE)pdw == header + unwindSize);

    assert(XBit == 0); // We don't handle the case where exception data is present, such as the Exception Handler RVA

    printf("\n");
}

#endif // DEBUG
#endif // TARGET_ARM
#endif // TARGET_ARMARCH
