// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "unwind.h"
#include "codegen.h"

#ifdef TARGET_ARM64

void CodeGen::unwindPush(RegNum reg)
{
    unreached(); // use one of the unwindSaveReg* functions instead.
}

void CodeGen::unwindAllocStack(unsigned size)
{
#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        if (generatingProlog)
        {
            unwindAllocStackCFI(size);
        }

        return;
    }
#endif // TARGET_UNIX

    assert(size % 16 == 0);
    unsigned x = size / 16;

    UnwindInfo& info = funCurrentFunc().uwi;

    if (x <= 0x1F)
    {
        // alloc_s: 000xxxxx: allocate small stack with size < 128 (2^5 * 16)
        // TODO-Review: should say size < 512

        info.AddCode((uint8_t)x);
    }
    else if (x <= 0x7FF)
    {
        // alloc_m: 11000xxx | xxxxxxxx: allocate large stack with size < 16k (2^11 * 16)
        // TODO-Review: should say size < 32K

        info.AddCode(0xC0 | (uint8_t)(x >> 8), (uint8_t)x);
    }
    else
    {
        // alloc_l: 11100000 | xxxxxxxx | xxxxxxxx | xxxxxxxx : allocate large stack with size < 256M (2^24 * 16)
        //
        // For large stack size, the most significant bits
        // are stored first (and next to the opCode) per the unwind spec.

        info.AddCode(0xE0, (uint8_t)(x >> 16), (uint8_t)(x >> 8), (uint8_t)x);
    }

    info.CaptureLocation(GetEmitter());
}

void CodeGen::unwindSetFrameReg(RegNum reg, unsigned offset)
{
#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        if (generatingProlog)
        {
            unwindSetFrameRegCFI(reg, offset);
        }

        return;
    }
#endif // TARGET_UNIX

    UnwindInfo& info = funCurrentFunc().uwi;

    if (offset == 0)
    {
        assert(reg == REG_FP);

        // set_fp: 11100001 : set up r29 : with : mov r29, sp
        info.AddCode(0xE1);
    }
    else
    {
        // add_fp: 11100010 | xxxxxxxx : set up r29 with : add r29, sp, #x * 8

        assert(reg == REG_FP);
        assert((offset % 8) == 0);

        unsigned x = offset / 8;
        assert(x <= 0xFF);

        info.AddCode(0xE2, (uint8_t)x);
    }

    info.CaptureLocation(GetEmitter());
}

void CodeGen::unwindSaveReg(RegNum reg, unsigned offset)
{
    unreached();
}

void CodeGen::unwindNop()
{
    JITDUMP("unwindNop: adding NOP\n");

    UnwindInfo& info = funCurrentFunc().uwi;

    INDEBUG(info.uwiAddingNOP = true);

    // nop: 11100011: no unwind operation is required.
    info.AddCode(0xE3);
    info.CaptureLocation(GetEmitter());

    INDEBUG(info.uwiAddingNOP = false);
}

// Save a register pair to the stack at the specified byte offset (which must be positive,
// a multiple of 8 from 0 to 504). Note that for ARM64 unwind codes, reg2 must be exactly
// one register higher than reg1, except for the case of a pair including LR, in which case
// reg1 must be either FP or R19/R21/R23/R25/R27 (note that it can't be even, such as R20,
// because that would mean R19 was saved separately, instead of saving <R19,R20> as a pair,
// which we should do instead).
void CodeGen::unwindSaveRegPair(RegNum reg1, RegNum reg2, int offset)
{
    // stp reg1, reg2, [sp, #offset]

    // offset for store pair in prolog must be positive and a multiple of 8.
    assert(0 <= offset && offset <= 504);
    assert((offset % 8) == 0);

#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        if (generatingProlog)
        {
            FuncInfoDsc& func     = funCurrentFunc();
            uint32_t     cbProlog = unwindGetCurrentOffset();

            func.cfi.AddCode(cbProlog, CFI_REL_OFFSET, mapRegNumToDwarfReg(reg1), offset);
            func.cfi.AddCode(cbProlog, CFI_REL_OFFSET, mapRegNumToDwarfReg(reg2), offset + 8);
        }

        return;
    }
#endif // TARGET_UNIX

    UnwindInfo& info = funCurrentFunc().uwi;

    int z = offset / 8;
    assert(0 <= z && z <= 0x3F);

    if (reg1 == REG_FP)
    {
        // save_fplr: 01zzzzzz: save <r29,lr> pair at [sp+#Z*8], offset <= 504

        assert(reg2 == REG_LR);

        info.AddCode(0x40 | (uint8_t)z);
    }
    else if (reg2 == REG_LR)
    {
        // save_lrpair: 1101011x | xxzzzzzz: save pair <r19 + 2 * #X, lr> at [sp + #Z * 8], offset <= 504

        assert(REG_R19 <= reg1 && // first legal pair: R19, LR
               reg1 <= REG_R27);  // last legal pair: R27, LR

        uint8_t x = (uint8_t)(reg1 - REG_R19);
        assert((x % 2) == 0); // only legal reg1: R19, R21, R23, R25, R27
        x /= 2;
        assert(0 <= x && x <= 0x7);

        info.AddCode(0xD6 | (uint8_t)(x >> 2), (uint8_t)(x << 6) | (uint8_t)z);
    }
    else if (IsGeneralRegister(reg1))
    {
        // save_regp: 110010xx | xxzzzzzz: save r(19 + #X) pair at [sp + #Z * 8], offset <= 504

        assert(REG_NEXT(reg1) == reg2);
        assert(REG_R19 <= reg1 && // first legal pair: R19, R20
               reg1 <= REG_R27);  // last legal pair: R27, R28 (FP is never saved without LR)

        uint8_t x = (uint8_t)(reg1 - REG_R19);
        assert(0 <= x && x <= 0xF);

        info.AddCode(0xC8 | (uint8_t)(x >> 2), (uint8_t)(x << 6) | (uint8_t)z);
    }
    else
    {
        // save_fregp: 1101100x | xxzzzzzz : save pair d(8 + #X) at [sp + #Z * 8], offset <= 504

        assert(REG_NEXT(reg1) == reg2);
        assert(REG_V8 <= reg1 && // first legal pair: V8, V9
               reg1 <= REG_V14); // last legal pair: V14, V15

        uint8_t x = (uint8_t)(reg1 - REG_V8);
        assert(0 <= x && x <= 0x7);

        info.AddCode(0xD8 | (uint8_t)(x >> 2), (uint8_t)(x << 6) | (uint8_t)z);
    }

    info.CaptureLocation(GetEmitter());
}

// Save a register pair to the stack at the specified byte offset (which must be negative,
// a multiple of 8 from -512 to -8). Note that for ARM64 unwind codes, reg2 must be exactly
// one register higher than reg1.
void CodeGen::unwindSaveRegPairPreindexed(RegNum reg1, RegNum reg2, int offset)
{
    // stp reg1, reg2, [sp, #offset]!

    // pre-indexed offset in prolog must be negative and a multiple of 8.
    assert(offset < 0);
    assert((offset % 8) == 0);

#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        if (generatingProlog)
        {
            FuncInfoDsc& func     = funCurrentFunc();
            uint32_t     cbProlog = unwindGetCurrentOffset();

            func.cfi.AddCode(cbProlog, CFI_ADJUST_CFA_OFFSET, DWARF_REG_ILLEGAL, -offset);
            func.cfi.AddCode(cbProlog, CFI_REL_OFFSET, mapRegNumToDwarfReg(reg1), 0);
            func.cfi.AddCode(cbProlog, CFI_REL_OFFSET, mapRegNumToDwarfReg(reg2), 8);
        }

        return;
    }
#endif // TARGET_UNIX

    UnwindInfo& info = funCurrentFunc().uwi;

    if (reg1 == REG_FP)
    {
        // save_fplr_x: 10zzzzzz: save <r29,lr> pair at [sp-(#Z+1)*8]!, pre-indexed offset >= -512

        assert(-512 <= offset);
        int z = (-offset) / 8 - 1;
        assert(0 <= z && z <= 0x3F);

        assert(reg2 == REG_LR);

        info.AddCode(0x80 | (uint8_t)z);
    }
    else if ((reg1 == REG_R19) &&
             (-256 <= offset)) // If the offset is between -512 and -256, we use the save_regp_x unwind code.
    {
        // save_r19r20_x: 001zzzzz: save <r19,r20> pair at [sp-#Z*8]!, pre-indexed offset >= -248
        // NOTE: I'm not sure why we allow Z==0 here; seems useless, and the calculation of offset is different from the
        // other cases.

        int z = (-offset) / 8;
        assert(0 <= z && z <= 0x1F);

        assert(reg2 == REG_R20);

        info.AddCode(0x20 | (uint8_t)z);
    }
    else if (IsGeneralRegister(reg1))
    {
        // save_regp_x: 110011xx | xxzzzzzz: save pair r(19 + #X) at [sp - (#Z + 1) * 8]!, pre-indexed offset >= -512

        assert(-512 <= offset);
        int z = (-offset) / 8 - 1;
        assert(0 <= z && z <= 0x3F);

        assert(REG_NEXT(reg1) == reg2);
        assert(REG_R19 <= reg1 && // first legal pair: R19, R20
               reg1 <= REG_R27);  // last legal pair: R27, R28 (FP is never saved without LR)

        uint8_t x = (uint8_t)(reg1 - REG_R19);
        assert(0 <= x && x <= 0xF);

        info.AddCode(0xCC | (uint8_t)(x >> 2), (uint8_t)(x << 6) | (uint8_t)z);
    }
    else
    {
        // save_fregp_x: 1101101x | xxzzzzzz : save pair d(8 + #X), at [sp - (#Z + 1) * 8]!, pre-indexed offset >= -512

        assert(-512 <= offset);
        int z = (-offset) / 8 - 1;
        assert(0 <= z && z <= 0x3F);

        assert(REG_NEXT(reg1) == reg2);
        assert(REG_V8 <= reg1 && // first legal pair: V8, V9
               reg1 <= REG_V14); // last legal pair: V14, V15

        uint8_t x = (uint8_t)(reg1 - REG_V8);
        assert(0 <= x && x <= 0x7);

        info.AddCode(0xDA | (uint8_t)(x >> 2), (uint8_t)(x << 6) | (uint8_t)z);
    }

    info.CaptureLocation(GetEmitter());
}

void CodeGen::unwindSaveReg(RegNum reg, int offset)
{
    // str reg, [sp, #offset]

    // offset for store in prolog must be positive and a multiple of 8.
    assert(0 <= offset && offset <= 504);
    assert((offset % 8) == 0);

#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        if (generatingProlog)
        {
            FuncInfoDsc& func     = funCurrentFunc();
            uint32_t     cbProlog = unwindGetCurrentOffset();

            func.cfi.AddCode(cbProlog, CFI_REL_OFFSET, mapRegNumToDwarfReg(reg), offset);
        }

        return;
    }
#endif // TARGET_UNIX

    int z = offset / 8;
    assert(0 <= z && z <= 0x3F);

    UnwindInfo& info = funCurrentFunc().uwi;

    if (IsGeneralRegister(reg))
    {
        // save_reg: 110100xx | xxzzzzzz: save reg r(19 + #X) at [sp + #Z * 8], offset <= 504

        assert(REG_R19 <= reg && // first legal register: R19
               reg <= REG_LR);   // last legal register: LR

        uint8_t x = (uint8_t)(reg - REG_R19);
        assert(0 <= x && x <= 0xF);

        info.AddCode(0xD0 | (uint8_t)(x >> 2), (uint8_t)(x << 6) | (uint8_t)z);
    }
    else
    {
        // save_freg: 1101110x | xxzzzzzz : save reg d(8 + #X) at [sp + #Z * 8], offset <= 504

        assert(REG_V8 <= reg && // first legal register: V8
               reg <= REG_V15); // last legal register: V15

        uint8_t x = (uint8_t)(reg - REG_V8);
        assert(0 <= x && x <= 0x7);

        info.AddCode(0xDC | (uint8_t)(x >> 2), (uint8_t)(x << 6) | (uint8_t)z);
    }

    info.CaptureLocation(GetEmitter());
}

void CodeGen::unwindSaveRegPreindexed(RegNum reg, int offset)
{
    // str reg, [sp, #offset]!

    // pre-indexed offset in prolog must be negative and a multiple of 8.
    assert(-256 <= offset && offset < 0);
    assert((offset % 8) == 0);

#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        if (generatingProlog)
        {
            FuncInfoDsc& func     = funCurrentFunc();
            uint32_t     cbProlog = unwindGetCurrentOffset();

            func.cfi.AddCode(cbProlog, CFI_ADJUST_CFA_OFFSET, DWARF_REG_ILLEGAL, -offset);
            func.cfi.AddCode(cbProlog, CFI_REL_OFFSET, mapRegNumToDwarfReg(reg), 0);
        }

        return;
    }
#endif // TARGET_UNIX

    UnwindInfo& info = funCurrentFunc().uwi;

    int z = (-offset) / 8 - 1;
    assert(0 <= z && z <= 0x1F);

    if (IsGeneralRegister(reg))
    {
        // save_reg_x: 1101010x | xxxzzzzz: save reg r(19 + #X) at [sp - (#Z + 1) * 8]!, pre-indexed offset >= -256

        assert(REG_R19 <= reg && // first legal register: R19
               reg <= REG_LR);   // last legal register: LR

        uint8_t x = (uint8_t)(reg - REG_R19);
        assert(0 <= x && x <= 0xF);

        info.AddCode(0xD4 | (uint8_t)(x >> 3), (uint8_t)(x << 5) | (uint8_t)z);
    }
    else
    {
        // save_freg_x: 11011110 | xxxzzzzz : save reg d(8 + #X) at [sp - (#Z + 1) * 8]!, pre - indexed offset >= -256

        assert(REG_V8 <= reg && // first legal register: V8
               reg <= REG_V15); // last legal register: V15

        uint8_t x = (uint8_t)(reg - REG_V8);
        assert(0 <= x && x <= 0x7);

        info.AddCode(0xDE, (uint8_t)(x << 5) | (uint8_t)z);
    }

    info.CaptureLocation(GetEmitter());
}

void CodeGen::unwindSaveNext()
{
#ifdef TARGET_UNIX
    // Do not use unwindSaveNext when generating CFI codes as there is no code for this.
    assert(!generateCFIUnwindCodes());
#endif

    UnwindInfo& info = funCurrentFunc().uwi;

    // We're saving the next register pair. The caller is responsible for ensuring this is correct!

    // save_next: 11100110 : save next non - volatile Int or FP register pair.
    info.AddCode(0xE6);
    info.CaptureLocation(GetEmitter());
}

void CodeGen::unwindReturn(RegNum reg)
{
    // Nothing to do; we will always have at least one trailing "end" opcode in our padding.
}

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
        case REG_R16:
            return 16;
        case REG_R17:
            return 17;
        case REG_R18:
            return 18;
        case REG_R19:
            return 19;
        case REG_R20:
            return 20;
        case REG_R21:
            return 21;
        case REG_R22:
            return 22;
        case REG_R23:
            return 23;
        case REG_R24:
            return 24;
        case REG_R25:
            return 25;
        case REG_R26:
            return 26;
        case REG_R27:
            return 27;
        case REG_R28:
            return 28;
        case REG_R29:
            return 29;
        case REG_R30:
            return 30;
        case REG_SP:
            return 31;
        case REG_V0:
            return 64;
        case REG_V1:
            return 65;
        case REG_V2:
            return 66;
        case REG_V3:
            return 67;
        case REG_V4:
            return 68;
        case REG_V5:
            return 69;
        case REG_V6:
            return 70;
        case REG_V7:
            return 71;
        case REG_V8:
            return 72;
        case REG_V9:
            return 73;
        case REG_V10:
            return 74;
        case REG_V11:
            return 75;
        case REG_V12:
            return 76;
        case REG_V13:
            return 77;
        case REG_V14:
            return 78;
        case REG_V15:
            return 79;
        case REG_V16:
            return 80;
        case REG_V17:
            return 81;
        case REG_V18:
            return 82;
        case REG_V19:
            return 83;
        case REG_V20:
            return 84;
        case REG_V21:
            return 85;
        case REG_V22:
            return 86;
        case REG_V23:
            return 87;
        case REG_V24:
            return 88;
        case REG_V25:
            return 89;
        case REG_V26:
            return 90;
        case REG_V27:
            return 91;
        case REG_V28:
            return 92;
        case REG_V29:
            return 93;
        case REG_V30:
            return 94;
        case REG_V31:
            return 95;
        default:
            unreached();
    }
}
#endif // TARGET_UNIX

#ifdef DEBUG

void UnwindInfo::CheckOpsize(uint8_t b1)
{
    // nothing to do; all instructions are 4 bytes
}

// Return the size of the unwind code (from 1 to 4 bytes), given the first byte of the unwind bytes
static unsigned GetUnwindSizeFromUnwindHeader(uint8_t b1)
{
    static uint8_t s_UnwindSize[256] = {
        // array of unwind sizes, in bytes (as specified in the ARM unwind specification)
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 00-0F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 10-1F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 20-2F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 30-3F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 40-4F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 50-5F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 60-6F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 70-7F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 80-8F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 90-9F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // A0-AF
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // B0-BF
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // C0-CF
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, // D0-DF
        4, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // E0-EF
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1  // F0-FF
    };

    unsigned size = s_UnwindSize[b1];
    assert(1 <= size && size <= 4);
    return size;
}

// Walk the prolog codes and calculate the size of the prolog or epilog, in bytes.
unsigned UnwindCodesBase::GetCodeSizeFromUnwindCodes(bool isProlog) const
{
    uint8_t* codesStart = GetCodes();
    uint8_t* codes      = codesStart;
    unsigned size       = 0;

    for (;;)
    {
        uint8_t b1 = *codes;

        if (IsEndCode(b1))
        {
            break; // We hit an "end" code; we're done
        }

        size += 4; // All codes represent 4 byte instructions.
        codes += GetUnwindSizeFromUnwindHeader(b1);

        assert(codes - codesStart < 256); // 255 is the absolute maximum number of code bytes allowed
    }

    return size;
}

// start is 0-based index from LSB, length is number of bits
static uint32_t ExtractBits(uint32_t dw, uint32_t start, uint32_t length)
{
    return (dw >> start) & ((1 << length) - 1);
}

void CodeGen::DumpUnwindInfo(
    bool isHotCode, uint32_t startOffset, uint32_t endOffset, const uint8_t* header, uint32_t unwindSize) const
{
    printf("Unwind Info%s:\n", isHotCode ? "" : " COLD");

    // header is not guaranteed to be aligned. We put four 0xFF end codes at the end
    // to provide padding, and round down to get a multiple of 4 bytes in size.
    uint32_t UNALIGNED* pdw = (uint32_t UNALIGNED*)header;
    uint32_t dw;

    dw = *pdw++;

    uint32_t codeWords      = ExtractBits(dw, 27, 5);
    uint32_t epilogCount    = ExtractBits(dw, 22, 5);
    uint32_t EBit           = ExtractBits(dw, 21, 1);
    uint32_t XBit           = ExtractBits(dw, 20, 1);
    uint32_t Vers           = ExtractBits(dw, 18, 2);
    uint32_t functionLength = ExtractBits(dw, 0, 18);

    printf("  >> Start offset   : 0x%06x (not in unwind data)\n", compiler->dspOffset(startOffset));
    printf("  >>   End offset   : 0x%06x (not in unwind data)\n", compiler->dspOffset(endOffset));
    printf("  Code Words        : %u\n", codeWords);
    printf("  Epilog Count      : %u\n", epilogCount);
    printf("  E bit             : %u\n", EBit);
    printf("  X bit             : %u\n", XBit);
    printf("  Vers              : %u\n", Vers);
    printf("  Function Length   : %u (0x%05x) Actual length = %u (0x%06x)\n", functionLength, functionLength,
           functionLength * 4, functionLength * 4);

    assert(functionLength * 4 == endOffset - startOffset);

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

    bool epilogStartAt[1024] = {}; // One byte per possible epilog start index; initialized to false

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
                uint32_t res               = ExtractBits(dw, 18, 4);
                uint32_t epilogStartIndex  = ExtractBits(dw, 22, 10);

                // Note that epilogStartOffset for a funclet is the offset from the beginning
                // of the current funclet, not the offset from the beginning of the main function.
                // To help find it when looking through JitDump output, also show the offset from
                // the beginning of the main function.
                uint32_t epilogStartOffsetFromMainFunctionBegin = epilogStartOffset * 4 + startOffset;

                assert(res == 0);

                printf("  ---- Scope %d\n", scope);
                printf("  Epilog Start Offset        : %u (0x%05x) Actual offset = %u (0x%06x) Offset from main "
                       "function begin = %u (0x%06x)\n",
                       compiler->dspOffset(epilogStartOffset), compiler->dspOffset(epilogStartOffset),
                       compiler->dspOffset(epilogStartOffset * 4), compiler->dspOffset(epilogStartOffset * 4),
                       compiler->dspOffset(epilogStartOffsetFromMainFunctionBegin),
                       compiler->dspOffset(epilogStartOffsetFromMainFunctionBegin));
                printf("  Epilog Start Index         : %u (0x%02x)\n", epilogStartIndex, epilogStartIndex);

                epilogStartAt[epilogStartIndex] = true; // an epilog starts at this offset in the unwind codes
            }
        }
    }
    else
    {
        printf("  --- One epilog, unwind codes at %u\n", epilogCount);
        assert(epilogCount < ArrLen(epilogStartAt));
        epilogStartAt[epilogCount] = true; // the one and only epilog starts its unwind codes at this offset
    }

    // Dump the unwind codes

    printf("  ---- Unwind codes ----\n");

    uint32_t countOfUnwindCodes = codeWords * 4;
    uint8_t* pUnwindCode        = (uint8_t*)pdw;
    uint8_t  b1, b2, b3, b4;
    uint32_t x, z;
    for (uint32_t i = 0; i < countOfUnwindCodes; i++)
    {
        // Does this byte start an epilog sequence? If so, note that fact.
        if (epilogStartAt[i])
        {
            printf("    ---- Epilog start at index %u ----\n", i);
        }

        b1 = *pUnwindCode++;

        if ((b1 & 0xE0) == 0)
        {
            // alloc_s: 000xxxxx: allocate small stack with size < 128 (2^5 * 16)
            // TODO-Review:should say size < 512
            x = b1 & 0x1F;
            printf("    %02X          alloc_s #%u (0x%02X); sub sp, sp, #%u (0x%03X)\n", b1, x, x, x * 16, x * 16);
        }
        else if ((b1 & 0xE0) == 0x20)
        {
            // save_r19r20_x: 001zzzzz: save <r19,r20> pair at [sp-#Z*8]!, pre-indexed offset >= -248
            z = b1 & 0x1F;
            printf("    %02X          save_r19r20_x #%u (0x%02X); stp %s, %s, [sp, #-%u]!\n", b1, z, z,
                   getRegName(REG_R19), getRegName(REG_R20), z * 8);
        }
        else if ((b1 & 0xC0) == 0x40)
        {
            // save_fplr: 01zzzzzz: save <r29,lr> pair at [sp+#Z*8], offset <= 504
            z = b1 & 0x3F;
            printf("    %02X          save_fplr #%u (0x%02X); stp %s, %s, [sp, #%u]\n", b1, z, z, getRegName(REG_FP),
                   getRegName(REG_LR), z * 8);
        }
        else if ((b1 & 0xC0) == 0x80)
        {
            // save_fplr_x: 10zzzzzz: save <r29,lr> pair at [sp-(#Z+1)*8]!, pre-indexed offset >= -512
            z = b1 & 0x3F;
            printf("    %02X          save_fplr_x #%u (0x%02X); stp %s, %s, [sp, #-%u]!\n", b1, z, z,
                   getRegName(REG_FP), getRegName(REG_LR), (z + 1) * 8);
        }
        else if ((b1 & 0xF8) == 0xC0)
        {
            // alloc_m: 11000xxx | xxxxxxxx: allocate large stack with size < 16k (2^11 * 16)
            // TODO-Review: should save size < 32K
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            x = ((uint32_t)(b1 & 0x7) << 8) | (uint32_t)b2;

            printf("    %02X %02X       alloc_m #%u (0x%03X); sub sp, sp, #%u (0x%04X)\n", b1, b2, x, x, x * 16,
                   x * 16);
        }
        else if ((b1 & 0xFC) == 0xC8)
        {
            // save_regp: 110010xx | xxzzzzzz: save r(19 + #X) pair at [sp + #Z * 8], offset <= 504
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            x = ((uint32_t)(b1 & 0x3) << 2) | (uint32_t)(b2 >> 6);
            z = (uint32_t)(b2 & 0x3F);

            printf("    %02X %02X       save_regp X#%u Z#%u (0x%02X); stp %s, %s, [sp, #%u]\n", b1, b2, x, z, z,
                   getRegName(REG_R19 + x), getRegName(REG_R19 + x + 1), z * 8);
        }
        else if ((b1 & 0xFC) == 0xCC)
        {
            // save_regp_x: 110011xx | xxzzzzzz: save pair r(19 + #X) at [sp - (#Z + 1) * 8]!, pre-indexed offset >=
            // -512
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            x = ((uint32_t)(b1 & 0x3) << 2) | (uint32_t)(b2 >> 6);
            z = (uint32_t)(b2 & 0x3F);

            printf("    %02X %02X       save_regp_x X#%u Z#%u (0x%02X); stp %s, %s, [sp, #-%u]!\n", b1, b2, x, z, z,
                   getRegName(REG_R19 + x), getRegName(REG_R19 + x + 1), (z + 1) * 8);
        }
        else if ((b1 & 0xFC) == 0xD0)
        {
            // save_reg: 110100xx | xxzzzzzz: save reg r(19 + #X) at [sp + #Z * 8], offset <= 504
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            x = ((uint32_t)(b1 & 0x3) << 2) | (uint32_t)(b2 >> 6);
            z = (uint32_t)(b2 & 0x3F);

            printf("    %02X %02X       save_reg X#%u Z#%u (0x%02X); str %s, [sp, #%u]\n", b1, b2, x, z, z,
                   getRegName(REG_R19 + x), z * 8);
        }
        else if ((b1 & 0xFE) == 0xD4)
        {
            // save_reg_x: 1101010x | xxxzzzzz: save reg r(19 + #X) at [sp - (#Z + 1) * 8]!, pre-indexed offset >= -256
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            x = ((uint32_t)(b1 & 0x1) << 3) | (uint32_t)(b2 >> 5);
            z = (uint32_t)(b2 & 0x1F);

            printf("    %02X %02X       save_reg_x X#%u Z#%u (0x%02X); str %s, [sp, #-%u]!\n", b1, b2, x, z, z,
                   getRegName(REG_R19 + x), (z + 1) * 8);
        }
        else if ((b1 & 0xFE) == 0xD6)
        {
            // save_lrpair: 1101011x | xxzzzzzz: save pair <r19 + 2 * #X, lr> at [sp + #Z * 8], offset <= 504
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            x = ((uint32_t)(b1 & 0x1) << 2) | (uint32_t)(b2 >> 6);
            z = (uint32_t)(b2 & 0x3F);

            printf("    %02X %02X       save_lrpair X#%u Z#%u (0x%02X); stp %s, %s, [sp, #%u]\n", b1, b2, x, z, z,
                   getRegName(REG_R19 + 2 * x), getRegName(REG_LR), z * 8);
        }
        else if ((b1 & 0xFE) == 0xD8)
        {
            // save_fregp: 1101100x | xxzzzzzz : save pair d(8 + #X) at [sp + #Z * 8], offset <= 504
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            x = ((uint32_t)(b1 & 0x1) << 2) | (uint32_t)(b2 >> 6);
            z = (uint32_t)(b2 & 0x3F);

            printf("    %02X %02X       save_fregp X#%u Z#%u (0x%02X); stp %s, %s, [sp, #%u]\n", b1, b2, x, z, z,
                   getRegName(REG_V8 + x), getRegName(REG_V8 + x + 1), z * 8);
        }
        else if ((b1 & 0xFE) == 0xDA)
        {
            // save_fregp_x: 1101101x | xxzzzzzz : save pair d(8 + #X), at [sp - (#Z + 1) * 8]!, pre-indexed offset >=
            // -512
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            x = ((uint32_t)(b1 & 0x1) << 2) | (uint32_t)(b2 >> 6);
            z = (uint32_t)(b2 & 0x3F);

            printf("    %02X %02X       save_fregp_x X#%u Z#%u (0x%02X); stp %s, %s, [sp, #-%u]!\n", b1, b2, x, z, z,
                   getRegName(REG_V8 + x), getRegName(REG_V8 + x + 1), (z + 1) * 8);
        }
        else if ((b1 & 0xFE) == 0xDC)
        {
            // save_freg: 1101110x | xxzzzzzz : save reg d(8 + #X) at [sp + #Z * 8], offset <= 504
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            x = ((uint32_t)(b1 & 0x1) << 2) | (uint32_t)(b2 >> 6);
            z = (uint32_t)(b2 & 0x3F);

            printf("    %02X %02X       save_freg X#%u Z#%u (0x%02X); str %s, [sp, #%u]\n", b1, b2, x, z, z,
                   getRegName(REG_V8 + x), z * 8);
        }
        else if (b1 == 0xDE)
        {
            // save_freg_x: 11011110 | xxxzzzzz : save reg d(8 + #X) at [sp - (#Z + 1) * 8]!, pre - indexed offset >=
            // -256
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            x = (uint32_t)(b2 >> 5);
            z = (uint32_t)(b2 & 0x1F);

            printf("    %02X %02X       save_freg_x X#%u Z#%u (0x%02X); str %s, [sp, #-%u]!\n", b1, b2, x, z, z,
                   getRegName(REG_V8 + x), (z + 1) * 8);
        }
        else if (b1 == 0xE0)
        {
            // alloc_l: 11100000 | xxxxxxxx | xxxxxxxx | xxxxxxxx : allocate large stack with size < 256M (2^24 * 16)
            assert(i + 3 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            b3 = *pUnwindCode++;
            b4 = *pUnwindCode++;
            i += 3;

            x = ((uint32_t)b2 << 16) | ((uint32_t)b3 << 8) | (uint32_t)b4;

            printf("    %02X %02X %02X %02X alloc_l %u (0x%06X); sub sp, sp, #%u (%06X)\n", b1, b2, b3, b4, x, x,
                   x * 16, x * 16);
        }
        else if (b1 == 0xE1)
        {
            // set_fp: 11100001 : set up r29 : with : mov r29, sp

            printf("    %02X          set_fp; mov %s, sp\n", b1, getRegName(REG_FP));
        }
        else if (b1 == 0xE2)
        {
            // add_fp: 11100010 | xxxxxxxx : set up r29 with : add r29, sp, #x * 8
            assert(i + 1 < countOfUnwindCodes);
            b2 = *pUnwindCode++;
            i++;

            x = (uint32_t)b2;

            printf("    %02X %02X       add_fp %u (0x%02X); add %s, sp, #%u\n", b1, b2, x, x, getRegName(REG_FP),
                   x * 8);
        }
        else if (b1 == 0xE3)
        {
            // nop: 11100011: no unwind operation is required.

            printf("    %02X          nop\n", b1);
        }
        else if (b1 == 0xE4)
        {
            // end: 11100100 : end of unwind code

            printf("    %02X          end\n", b1);
        }
        else if (b1 == 0xE5)
        {
            // end_c: 11100101 : end of unwind code in current chained scope.

            printf("    %02X          end_c\n", b1);
        }
        else if (b1 == 0xE6)
        {
            // save_next: 11100110 : save next non - volatile Int or FP register pair.

            printf("    %02X          save_next\n", b1);
        }
        else
        {
            // Unknown / reserved unwind code
            assert(!"Internal error decoding unwind codes");
        }
    }

    pdw += codeWords;
    assert((uint8_t*)pdw == pUnwindCode);
    assert((uint8_t*)pdw == header + unwindSize);

    assert(XBit == 0); // We don't handle the case where exception data is present, such as the Exception Handler RVA

    printf("\n");
}

#endif // DEBUG
#endif // TARGET_ARM64
