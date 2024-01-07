// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "unwind.h"
#include "codegen.h"

#ifdef TARGET_AMD64

void CodeGen::unwindBegProlog()
{
#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        unwindBegPrologCFI();
        return;
    }
#endif

    unwindBegPrologWindows();
}

void CodeGen::unwindEndProlog()
{
    assert(generatingProlog);
}

void CodeGen::unwindBegEpilog()
{
    assert(generatingEpilog);
}

void CodeGen::unwindEndEpilog()
{
    assert(generatingEpilog);
}

void CodeGen::unwindPush(RegNum reg)
{
#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        unwindPushPopCFI(reg);
        return;
    }
#endif

    unwindPushWindows(reg);
}

void CodeGen::unwindAllocStack(unsigned size)
{
#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        unwindAllocStackCFI(size);
        return;
    }
#endif

    unwindAllocStackWindows(size);
}

void CodeGen::unwindSetFrameReg(RegNum reg, unsigned offset)
{
#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        unwindSetFrameRegCFI(reg, offset);
        return;
    }
#endif

    unwindSetFrameRegWindows(reg, offset);
}

void CodeGen::unwindSaveReg(RegNum reg, unsigned offset)
{
#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        unwindSaveRegCFI(reg, offset);
        return;
    }
#endif

    unwindSaveRegWindows(reg, offset);
}

void CodeGen::unwindBegPrologWindows()
{
    assert(generatingProlog);

    FuncInfoDsc& func = funCurrentFunc();

    func.win.codesIndex                = sizeof(func.win.codes);
    func.win.header.Version            = 1;
    func.win.header.Flags              = 0;
    func.win.header.CountOfUnwindCodes = 0;
    func.win.header.FrameRegister      = 0;
    func.win.header.FrameOffset        = 0;
}

UNWIND_CODE* Win64UnwindInfo::AllocCode()
{
    assert(header.Version == 1);
    assert(header.CountOfUnwindCodes == 0);
    assert(codesIndex > sizeof(UNWIND_CODE));

    codesIndex -= sizeof(UNWIND_CODE);
    return reinterpret_cast<UNWIND_CODE*>(&codes[codesIndex]);
}

uint16_t* Win64UnwindInfo::AllocUInt16(uint16_t value)
{
    assert(header.Version == 1);
    assert(header.CountOfUnwindCodes == 0);
    assert(codesIndex > sizeof(uint16_t));

    codesIndex -= sizeof(uint16_t);
    uint16_t* p = reinterpret_cast<uint16_t*>(&codes[codesIndex]);
    *p          = value;
    return p;
}

uint32_t* Win64UnwindInfo::AllocUInt32(uint32_t value)
{
    assert(header.Version == 1);
    assert(header.CountOfUnwindCodes == 0);
    assert(codesIndex > sizeof(uint32_t));

    codesIndex -= sizeof(uint32_t);
    uint32_t* p = reinterpret_cast<uint32_t*>(&codes[codesIndex]);
    *p          = value;
    return p;
}

void CodeGen::unwindPushWindows(RegNum reg)
{
    assert(generatingProlog);

    FuncInfoDsc& func     = funCurrentFunc();
    uint32_t     cbProlog = unwindGetCurrentOffset();

    UNWIND_CODE* code = func.win.AllocCode();
    noway_assert((uint8_t)cbProlog == cbProlog);
    code->CodeOffset = (uint8_t)cbProlog;

    if (((RBM_CALLEE_SAVED & genRegMask(reg)) != RBM_NONE)
#if ETW_EBP_FRAMED
        // In case of ETW_EBP_FRAMED defined the REG_FPBASE (RBP)
        // is excluded from the callee-save register list.
        // Make sure the register gets PUSH unwind info in this case,
        // since it is pushed as a frame register.
        || (reg == REG_FPBASE)
#endif // ETW_EBP_FRAMED
            )
    {
        code->UnwindOp = UWOP_PUSH_NONVOL;
        code->OpInfo   = static_cast<uint8_t>(reg);
    }
    else
    {
        // Push of a volatile register is just a small stack allocation
        code->UnwindOp = UWOP_ALLOC_SMALL;
        code->OpInfo   = 0;
    }
}

void CodeGen::unwindAllocStackWindows(unsigned size)
{
    assert(generatingProlog);
    assert(size % 8 == 0);

    FuncInfoDsc& func     = funCurrentFunc();
    uint32_t     cbProlog = unwindGetCurrentOffset();

    UNWIND_CODE* code;

    if (size <= 128)
    {
        code           = func.win.AllocCode();
        code->UnwindOp = UWOP_ALLOC_SMALL;
        code->OpInfo   = (size - 8) / 8;
    }
    else if (size <= 0x7FFF8)
    {
        func.win.AllocUInt16(static_cast<uint16_t>(size / 8));

        code           = func.win.AllocCode();
        code->UnwindOp = UWOP_ALLOC_LARGE;
        code->OpInfo   = 0;
    }
    else
    {
        func.win.AllocUInt32(size);

        code           = func.win.AllocCode();
        code->UnwindOp = UWOP_ALLOC_LARGE;
        code->OpInfo   = 1;
    }

    noway_assert((uint8_t)cbProlog == cbProlog);
    code->CodeOffset = (uint8_t)cbProlog;
}

void CodeGen::unwindSetFrameRegWindows(RegNum reg, unsigned offset)
{
    assert(generatingProlog);

    FuncInfoDsc& func     = funCurrentFunc();
    uint32_t     cbProlog = unwindGetCurrentOffset();

    func.win.header.FrameRegister = static_cast<uint8_t>(reg);

#ifdef TARGET_UNIX
    if (offset > 240)
    {
        // On Unix only, we have a CLR-only extension to the AMD64 unwind codes: UWOP_SET_FPREG_LARGE.
        // It has a 32-bit offset (scaled). You must set UNWIND_INFO.FrameOffset to 15. The 32-bit
        // offset follows in 2 UNWIND_CODE fields.

        assert(offset % 16 == 0);

        func.win.AllocUInt32(offset / 16);

        UNWIND_CODE* code = func.win.AllocCode();
        noway_assert((uint8_t)cbProlog == cbProlog);
        code->CodeOffset = (uint8_t)cbProlog;
        code->OpInfo     = 0;
        code->UnwindOp   = UWOP_SET_FPREG_LARGE;

        func.win.header.FrameOffset = 15;
    }
    else
#endif // TARGET_UNIX
    {
        assert(offset <= 240);
        assert(offset % 16 == 0);

        UNWIND_CODE* code = func.win.AllocCode();
        noway_assert((uint8_t)cbProlog == cbProlog);
        code->CodeOffset = (uint8_t)cbProlog;
        code->OpInfo     = 0;
        code->UnwindOp   = UWOP_SET_FPREG;

        func.win.header.FrameOffset = offset / 16;
    }
}

void CodeGen::unwindSaveRegWindows(RegNum reg, unsigned offset)
{
    assert(generatingProlog);

    FuncInfoDsc& func = funCurrentFunc();

    if ((genRegMask(reg) & RBM_CALLEE_SAVED) == RBM_NONE)
    {
        return;
    }

    uint32_t     cbProlog = unwindGetCurrentOffset();
    UNWIND_CODE* code;

    if (offset < 0x80000)
    {
        uint16_t* codedSize = func.win.AllocUInt16(0);
        code                = func.win.AllocCode();

        if (genIsValidFloatReg(reg))
        {
            *codedSize     = static_cast<uint16_t>(offset / 16);
            code->UnwindOp = UWOP_SAVE_XMM128;
        }
        else
        {
            *codedSize     = static_cast<uint16_t>(offset / 8);
            code->UnwindOp = UWOP_SAVE_NONVOL;
        }
    }
    else
    {
        func.win.AllocUInt32(offset);

        code           = func.win.AllocCode();
        code->UnwindOp = genIsValidFloatReg(reg) ? UWOP_SAVE_XMM128_FAR : UWOP_SAVE_NONVOL_FAR;
    }

    code->OpInfo = (uint8_t)reg;
    noway_assert((uint8_t)cbProlog == cbProlog);
    code->CodeOffset = (uint8_t)cbProlog;
}

#ifdef TARGET_UNIX
int16_t CodeGen::mapRegNumToDwarfReg(RegNum reg)
{
    switch (reg)
    {
        case REG_RAX:
            return 0;
        case REG_RCX:
            return 2;
        case REG_RDX:
            return 1;
        case REG_RBX:
            return 3;
        case REG_RSP:
            return 7;
        case REG_RBP:
            return 6;
        case REG_RSI:
            return 4;
        case REG_RDI:
            return 5;
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
        case REG_XMM0:
            return 17;
        case REG_XMM1:
            return 18;
        case REG_XMM2:
            return 19;
        case REG_XMM3:
            return 20;
        case REG_XMM4:
            return 21;
        case REG_XMM5:
            return 22;
        case REG_XMM6:
            return 23;
        case REG_XMM7:
            return 24;
        case REG_XMM8:
            return 25;
        case REG_XMM9:
            return 26;
        case REG_XMM10:
            return 27;
        case REG_XMM11:
            return 28;
        case REG_XMM12:
            return 29;
        case REG_XMM13:
            return 30;
        case REG_XMM14:
            return 31;
        case REG_XMM15:
            return 32;
        default:
            unreached();
    }
}

void CodeGen::unwindSaveRegCFI(RegNum reg, unsigned offset)
{
    assert(generatingProlog);

    if ((genRegMask(reg) & RBM_CALLEE_SAVED) == RBM_NONE)
    {
        return;
    }

    FuncInfoDsc& func     = funCurrentFunc();
    uint32_t     cbProlog = unwindGetCurrentOffset();

    func.cfi.AddCode(cbProlog, CFI_REL_OFFSET, mapRegNumToDwarfReg(reg), offset);
}
#endif // TARGET_UNIX

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
    unwindReserveFuncRegion(func, true);

    if (compiler->fgFirstColdBlock != nullptr)
    {
        unwindReserveFuncRegion(func, false);
    }
}

void CodeGen::unwindReserveFuncRegion(FuncInfoDsc* func, bool isHotCode)
{
    uint32_t unwindSize = 0;

    if (isHotCode)
    {
#ifdef TARGET_UNIX
        if (generateCFIUnwindCodes())
        {
            unwindSize = static_cast<uint32_t>(func->cfi.codes->size() * sizeof(CFI_CODE));
        }
        else
#endif
        {
            assert(func->win.header.Version == 1);
            assert(func->win.header.CountOfUnwindCodes == 0);

            // Set the size of the prolog to be the last encoded action
            if (func->win.codesIndex < sizeof(func->win.codes))
            {
                UNWIND_CODE* code             = reinterpret_cast<UNWIND_CODE*>(&func->win.codes[func->win.codesIndex]);
                func->win.header.SizeOfProlog = code->CodeOffset;
            }
            else
            {
                func->win.header.SizeOfProlog = 0;
            }

            func->win.header.CountOfUnwindCodes =
                static_cast<uint8_t>((sizeof(func->win.codes) - func->win.codesIndex) / sizeof(UNWIND_CODE));

            // Prepend the unwindHeader onto the unwind codes
            assert(func->win.codesIndex >= offsetof(UNWIND_INFO, UnwindCode));

            func->win.codesIndex -= offsetof(UNWIND_INFO, UnwindCode);
            UNWIND_INFO* header = reinterpret_cast<UNWIND_INFO*>(&func->win.codes[func->win.codesIndex]);
            memcpy(header, &func->win.header, offsetof(UNWIND_INFO, UnwindCode));

            unwindSize = sizeof(func->win.codes) - func->win.codesIndex;
        }
    }

    eeReserveUnwindInfo(func->kind != FUNC_ROOT, !isHotCode, unwindSize);
}

void CodeGen::unwindEmit(void* hotCode, void* coldCode)
{
    assert(!generatingProlog);
    assert(!generatingEpilog);
    assert(compFuncInfoCount > 0);

    for (unsigned i = 0; i < compFuncInfoCount; i++)
    {
        unwindEmitFunc(&funGetFunc(i), hotCode, coldCode);
    }
}

void CodeGen::unwindEmitFunc(FuncInfoDsc* func, void* hotCode, void* coldCode)
{
    unwindEmitFuncRegion(func, hotCode, coldCode, true);

    if (coldCode != nullptr)
    {
        unwindEmitFuncRegion(func, hotCode, coldCode, false);
    }
}

static void DumpUnwindInfo(bool isHotCode, uint32_t startOffset, uint32_t endOffset, const UNWIND_INFO* header);

void CodeGen::unwindEmitFuncRegion(FuncInfoDsc* func, void* hotCode, void* coldCode, bool isHotCode)
{
    uint32_t startOffset;
    uint32_t endOffset;
    uint32_t unwindSize  = 0;
    uint8_t* unwindBlock = nullptr;

    if (isHotCode)
    {
        unwindGetFuncHotRange(func, &startOffset, &endOffset);

#ifdef TARGET_UNIX
        if (generateCFIUnwindCodes())
        {
            uint32_t size = static_cast<uint32_t>(func->cfi.codes->size());

            if (size > 0)
            {
                unwindSize  = size * sizeof(CFI_CODE);
                unwindBlock = reinterpret_cast<uint8_t*>(func->cfi.codes->data());
            }
        }
        else
#endif
        {
            unwindSize  = sizeof(func->win.codes) - func->win.codesIndex;
            unwindBlock = &func->win.codes[func->win.codesIndex];

            UNWIND_INFO* unwindInfo = reinterpret_cast<UNWIND_INFO*>(&func->win.codes[func->win.codesIndex]);
            assert(unwindSize ==
                   offsetof(UNWIND_INFO, UnwindCode) + unwindInfo->CountOfUnwindCodes * sizeof(UNWIND_CODE));
        }
    }
    else
    {
        unwindGetFuncColdRange(func, &startOffset, &endOffset);
    }

#ifdef DEBUG
    if (compiler->opts.dspUnwind)
    {
#ifdef TARGET_UNIX
        if (generateCFIUnwindCodes())
        {
            DumpCfiInfo(isHotCode, startOffset, endOffset, unwindSize / sizeof(CFI_CODE),
                        reinterpret_cast<CFI_CODE*>(unwindBlock));
        }
        else
#endif
        {
            DumpUnwindInfo(isHotCode, startOffset, endOffset, reinterpret_cast<UNWIND_INFO*>(unwindBlock));
        }
    }
#endif // DEBUG

    // Adjust for cold or hot code:
    // 1. The VM doesn't want the cold code pointer unless this is cold code.
    // 2. The startOffset and endOffset need to be from the base of the hot section for hot code
    //    and from the base of the cold section for cold code

    if (isHotCode)
    {
        assert(endOffset <= compTotalHotCodeSize);

        coldCode = nullptr;
    }
    else
    {
        assert(startOffset >= compTotalHotCodeSize);

        startOffset -= compTotalHotCodeSize;
        endOffset -= compTotalHotCodeSize;
    }

    eeAllocUnwindInfo(func->kind, hotCode, coldCode, startOffset, endOffset, unwindSize, unwindBlock);
}

#ifdef DEBUG

static void DumpUnwindInfo(bool isHotCode, uint32_t startOffset, uint32_t endOffset, const UNWIND_INFO* header)
{
    printf("Unwind Info%s:\n", isHotCode ? "" : " COLD");
    printf("  >> Start offset   : 0x%06x (not in unwind data)\n", dspOffset(startOffset));
    printf("  >>   End offset   : 0x%06x (not in unwind data)\n", dspOffset(endOffset));

    if (header == nullptr)
    {
        // Cold AMD64 code doesn't have unwind info; the VM creates chained unwind info.
        assert(!isHotCode);
        return;
    }

    printf("  Version           : %u\n", header->Version);
    printf("  Flags             : 0x%02x", header->Flags);

    if (header->Flags != 0)
    {
        const uint8_t flags = header->Flags;

        printf(" (");
        if (flags & UNW_FLAG_EHANDLER)
        {
            printf(" UNW_FLAG_EHANDLER");
        }
        if (flags & UNW_FLAG_UHANDLER)
        {
            printf(" UNW_FLAG_UHANDLER");
        }
        if (flags & UNW_FLAG_CHAININFO)
        {
            printf(" UNW_FLAG_CHAININFO");
        }
        printf(")");
    }

    printf("\n");
    printf("  SizeOfProlog      : 0x%02X\n", header->SizeOfProlog);
    printf("  CountOfUnwindCodes: %u\n", header->CountOfUnwindCodes);
    printf("  FrameRegister     : %s (%u)\n", (header->FrameRegister == 0) ? "none" : getRegName(header->FrameRegister),
           header->FrameRegister); // RAX (0) is not allowed as a frame register

    if (header->FrameRegister == 0)
    {
        printf("  FrameOffset       : N/A (no FrameRegister) (Value=%u)\n", header->FrameOffset);
    }
    else
    {
        printf("  FrameOffset       : %u * 16 = 0x%02X\n", header->FrameOffset, header->FrameOffset * 16);
    }

    printf("  UnwindCodes       :\n");

    for (unsigned i = 0; i < header->CountOfUnwindCodes; i++)
    {
        const UNWIND_CODE& code = header->UnwindCode[i];

        switch (code.UnwindOp)
        {
            case UWOP_PUSH_NONVOL:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_PUSH_NONVOL (%u)     OpInfo: %s (%u)\n", code.CodeOffset,
                       code.UnwindOp, getRegName(code.OpInfo), code.OpInfo);
                break;

            case UWOP_ALLOC_LARGE:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_ALLOC_LARGE (%u)     OpInfo: %u - ", code.CodeOffset,
                       code.UnwindOp, code.OpInfo);
                if (code.OpInfo == 0)
                {
                    i++;
                    printf("Scaled small  \n      Size: %u * 8 = %u = 0x%05X\n", header->UnwindCode[i].FrameOffset,
                           header->UnwindCode[i].FrameOffset * 8, header->UnwindCode[i].FrameOffset * 8);
                }
                else if (code.OpInfo == 1)
                {
                    i++;
                    printf("Unscaled large\n      Size: %u = 0x%08X\n\n", *(uint32_t*)&(header->UnwindCode[i]),
                           *(uint32_t*)&(header->UnwindCode[i]));
                    i++;
                }
                else
                {
                    printf("Unknown\n");
                }
                break;

            case UWOP_ALLOC_SMALL:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_ALLOC_SMALL (%u)     OpInfo: %u * 8 + 8 = %u = 0x%02X\n",
                       code.CodeOffset, code.UnwindOp, code.OpInfo, code.OpInfo * 8 + 8, code.OpInfo * 8 + 8);
                break;

            case UWOP_SET_FPREG:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_SET_FPREG (%u)       OpInfo: Unused (%u)\n",
                       code.CodeOffset, code.UnwindOp, code.OpInfo); // This should be zero
                break;

#ifdef TARGET_UNIX
            case UWOP_SET_FPREG_LARGE:
            {
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_SET_FPREG_LARGE (%u) OpInfo: Unused (%u)\n",
                       code.CodeOffset, code.UnwindOp, code.OpInfo); // This should be zero
                i++;
                unsigned offset = *(uint32_t*)&(header->UnwindCode[i]);
                i++;
                printf("      Scaled Offset: %u * 16 = %u = 0x%08X\n", offset, offset * 16, offset * 16);
                if ((offset & 0xF0000000) != 0)
                {
                    printf("      Illegal unscaled offset: too large\n");
                }
            }
            break;
#endif // TARGET_UNIX

            case UWOP_SAVE_NONVOL:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_SAVE_NONVOL (%u)     OpInfo: %s (%u)\n", code.CodeOffset,
                       code.UnwindOp, getRegName(code.OpInfo), code.OpInfo);
                i++;
                printf("      Scaled Small Offset: %u * 8 = %u = 0x%05X\n", header->UnwindCode[i].FrameOffset,
                       header->UnwindCode[i].FrameOffset * 8, header->UnwindCode[i].FrameOffset * 8);
                break;

            case UWOP_SAVE_NONVOL_FAR:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_SAVE_NONVOL_FAR (%u) OpInfo: %s (%u)\n", code.CodeOffset,
                       code.UnwindOp, getRegName(code.OpInfo), code.OpInfo);
                i++;
                printf("      Unscaled Large Offset: 0x%08X\n\n", *(uint32_t*)&(header->UnwindCode[i]));
                i++;
                break;

            case UWOP_SAVE_XMM128:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_SAVE_XMM128 (%u)     OpInfo: XMM%u (%u)\n",
                       code.CodeOffset, code.UnwindOp, code.OpInfo, code.OpInfo);
                i++;
                printf("      Scaled Small Offset: %u * 16 = %u = 0x%05X\n", header->UnwindCode[i].FrameOffset,
                       header->UnwindCode[i].FrameOffset * 16, header->UnwindCode[i].FrameOffset * 16);
                break;

            case UWOP_SAVE_XMM128_FAR:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_SAVE_XMM128_FAR (%u) OpInfo: XMM%u (%u)\n",
                       code.CodeOffset, code.UnwindOp, code.OpInfo, code.OpInfo);
                i++;
                printf("      Unscaled Large Offset: 0x%08X\n\n", *(uint32_t*)&(header->UnwindCode[i]));
                i++;
                break;

            case UWOP_EPILOG:
            case UWOP_SPARE_CODE:
            case UWOP_PUSH_MACHFRAME:
            default:
                printf("    Unrecognized UNWIND_CODE: 0x%04X\n", reinterpret_cast<const uint16_t*>(&code));
                break;
        }
    }
}

#endif // DEBUG
#endif // TARGET_AMD64
