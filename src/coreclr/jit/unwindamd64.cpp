// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                              UnwindInfo                                   XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#include "unwind.h"
#include "codegen.h"

#ifdef TARGET_AMD64
#ifdef UNIX_AMD64_ABI
short CodeGen::mapRegNumToDwarfReg(regNumber reg)
{
    short dwarfReg = DWARF_REG_ILLEGAL;

    switch (reg)
    {
        case REG_RAX:
            dwarfReg = 0;
            break;
        case REG_RCX:
            dwarfReg = 2;
            break;
        case REG_RDX:
            dwarfReg = 1;
            break;
        case REG_RBX:
            dwarfReg = 3;
            break;
        case REG_RSP:
            dwarfReg = 7;
            break;
        case REG_RBP:
            dwarfReg = 6;
            break;
        case REG_RSI:
            dwarfReg = 4;
            break;
        case REG_RDI:
            dwarfReg = 5;
            break;
        case REG_R8:
            dwarfReg = 8;
            break;
        case REG_R9:
            dwarfReg = 9;
            break;
        case REG_R10:
            dwarfReg = 10;
            break;
        case REG_R11:
            dwarfReg = 11;
            break;
        case REG_R12:
            dwarfReg = 12;
            break;
        case REG_R13:
            dwarfReg = 13;
            break;
        case REG_R14:
            dwarfReg = 14;
            break;
        case REG_R15:
            dwarfReg = 15;
            break;
        case REG_XMM0:
            dwarfReg = 17;
            break;
        case REG_XMM1:
            dwarfReg = 18;
            break;
        case REG_XMM2:
            dwarfReg = 19;
            break;
        case REG_XMM3:
            dwarfReg = 20;
            break;
        case REG_XMM4:
            dwarfReg = 21;
            break;
        case REG_XMM5:
            dwarfReg = 22;
            break;
        case REG_XMM6:
            dwarfReg = 23;
            break;
        case REG_XMM7:
            dwarfReg = 24;
            break;
        case REG_XMM8:
            dwarfReg = 25;
            break;
        case REG_XMM9:
            dwarfReg = 26;
            break;
        case REG_XMM10:
            dwarfReg = 27;
            break;
        case REG_XMM11:
            dwarfReg = 28;
            break;
        case REG_XMM12:
            dwarfReg = 29;
            break;
        case REG_XMM13:
            dwarfReg = 30;
            break;
        case REG_XMM14:
            dwarfReg = 31;
            break;
        case REG_XMM15:
            dwarfReg = 32;
            break;
        default:
            noway_assert(!"unexpected REG_NUM");
    }

    return dwarfReg;
}

#endif // UNIX_AMD64_ABI

// Initialize the unwind info data structures.
// Called at the beginning of main function or funclet prolog generation.
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

void CodeGen::unwindBegPrologWindows()
{
    assert(generatingProlog);

    FuncInfoDsc* func = funCurrentFunc();

    func->unwindCodeSlot                  = sizeof(func->unwindCodes);
    func->unwindHeader.Version            = 1;
    func->unwindHeader.Flags              = 0;
    func->unwindHeader.CountOfUnwindCodes = 0;
    func->unwindHeader.FrameRegister      = 0;
    func->unwindHeader.FrameOffset        = 0;
}

// Called at the end of main function or funclet
// prolog generation to indicate there is no more unwind information for this prolog.
void CodeGen::unwindEndProlog()
{
    assert(generatingProlog);
}

// Called at the beginning of main function or funclet
// epilog generation.
void CodeGen::unwindBegEpilog()
{
    assert(generatingEpilog);
}

// Called at the end of main function or funclet
// epilog generation.
void CodeGen::unwindEndEpilog()
{
    assert(generatingEpilog);
}

// Record a push/save of a register.
//
// Arguments:
//    reg - The register being pushed/saved.
//
void CodeGen::unwindPush(regNumber reg)
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

void CodeGen::unwindPushWindows(regNumber reg)
{
    assert(generatingProlog);

    FuncInfoDsc* func = funCurrentFunc();

    assert(func->unwindHeader.Version == 1);            // Can't call this before unwindBegProlog
    assert(func->unwindHeader.CountOfUnwindCodes == 0); // Can't call this after unwindReserve
    assert(func->unwindCodeSlot > sizeof(UNWIND_CODE));
    UNWIND_CODE* code     = (UNWIND_CODE*)&func->unwindCodes[func->unwindCodeSlot -= sizeof(UNWIND_CODE)];
    uint32_t     cbProlog = unwindGetCurrentOffset();
    noway_assert((uint8_t)cbProlog == cbProlog);
    code->CodeOffset = (uint8_t)cbProlog;

    if ((RBM_CALLEE_SAVED & genRegMask(reg))
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
        code->OpInfo   = (uint8_t)reg;
    }
    else
    {
        // Push of a volatile register is just a small stack allocation
        code->UnwindOp = UWOP_ALLOC_SMALL;
        code->OpInfo   = 0;
    }
}

#ifdef UNIX_AMD64_ABI
#endif // UNIX_AMD64_ABI

// Record a stack frame allocation (sub sp, X).
//
// Arguments:
//    size - The size of the stack frame allocation (the amount subtracted from the stack pointer).
//
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

void CodeGen::unwindAllocStackWindows(unsigned size)
{
    assert(generatingProlog);

    FuncInfoDsc* func = funCurrentFunc();

    assert(func->unwindHeader.Version == 1);            // Can't call this before unwindBegProlog
    assert(func->unwindHeader.CountOfUnwindCodes == 0); // Can't call this after unwindReserve
    assert(size % 8 == 0);                              // Stack size is *always* 8 byte aligned
    UNWIND_CODE* code;
    if (size <= 128)
    {
        assert(func->unwindCodeSlot > sizeof(UNWIND_CODE));
        code           = (UNWIND_CODE*)&func->unwindCodes[func->unwindCodeSlot -= sizeof(UNWIND_CODE)];
        code->UnwindOp = UWOP_ALLOC_SMALL;
        code->OpInfo   = (size - 8) / 8;
    }
    else if (size <= 0x7FFF8)
    {
        assert(func->unwindCodeSlot > (sizeof(UNWIND_CODE) + sizeof(uint16_t)));
        uint16_t* codedSize = (uint16_t*)&func->unwindCodes[func->unwindCodeSlot -= sizeof(uint16_t)];
        *codedSize          = (uint16_t)(size / 8);
        code                = (UNWIND_CODE*)&func->unwindCodes[func->unwindCodeSlot -= sizeof(UNWIND_CODE)];
        code->UnwindOp      = UWOP_ALLOC_LARGE;
        code->OpInfo        = 0;
    }
    else
    {
        assert(func->unwindCodeSlot > (sizeof(UNWIND_CODE) + sizeof(uint32_t)));
        uint32_t* codedSize = (uint32_t*)&func->unwindCodes[func->unwindCodeSlot -= sizeof(uint32_t)];
        *codedSize          = size;
        code                = (UNWIND_CODE*)&func->unwindCodes[func->unwindCodeSlot -= sizeof(UNWIND_CODE)];
        code->UnwindOp      = UWOP_ALLOC_LARGE;
        code->OpInfo        = 1;
    }
    uint32_t cbProlog = unwindGetCurrentOffset();
    noway_assert((uint8_t)cbProlog == cbProlog);
    code->CodeOffset = (uint8_t)cbProlog;
}

// Record a frame register.
//
// Arguments:
//    reg    - The register being set as the frame register.
//    offset - The offset from the current stack pointer that the frame pointer will point at.
//
void CodeGen::unwindSetFrameReg(regNumber reg, unsigned offset)
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

void CodeGen::unwindSetFrameRegWindows(regNumber reg, unsigned offset)
{
    assert(generatingProlog);

    FuncInfoDsc* func = funCurrentFunc();

    assert(func->unwindHeader.Version == 1);            // Can't call this before unwindBegProlog
    assert(func->unwindHeader.CountOfUnwindCodes == 0); // Can't call this after unwindReserve
    uint32_t cbProlog = unwindGetCurrentOffset();
    noway_assert((uint8_t)cbProlog == cbProlog);

    func->unwindHeader.FrameRegister = static_cast<uint8_t>(reg);

#ifdef UNIX_AMD64_ABI
    if (offset > 240)
    {
        // On Unix only, we have a CLR-only extension to the AMD64 unwind codes: UWOP_SET_FPREG_LARGE.
        // It has a 32-bit offset (scaled). You must set UNWIND_INFO.FrameOffset to 15. The 32-bit
        // offset follows in 2 UNWIND_CODE fields.

        assert(func->unwindCodeSlot > (sizeof(UNWIND_CODE) + sizeof(uint32_t)));
        uint32_t* codedSize = (uint32_t*)&func->unwindCodes[func->unwindCodeSlot -= sizeof(uint32_t)];
        assert(offset % 16 == 0);
        *codedSize = offset / 16;

        UNWIND_CODE* code              = (UNWIND_CODE*)&func->unwindCodes[func->unwindCodeSlot -= sizeof(UNWIND_CODE)];
        code->CodeOffset               = (uint8_t)cbProlog;
        code->OpInfo                   = 0;
        code->UnwindOp                 = UWOP_SET_FPREG_LARGE;
        func->unwindHeader.FrameOffset = 15;
    }
    else
#endif // UNIX_AMD64_ABI
    {
        assert(func->unwindCodeSlot > sizeof(UNWIND_CODE));
        UNWIND_CODE* code = (UNWIND_CODE*)&func->unwindCodes[func->unwindCodeSlot -= sizeof(UNWIND_CODE)];
        code->CodeOffset  = (uint8_t)cbProlog;
        code->OpInfo      = 0;
        code->UnwindOp    = UWOP_SET_FPREG;
        assert(offset <= 240);
        assert(offset % 16 == 0);
        func->unwindHeader.FrameOffset = offset / 16;
    }
}

// Record a register save.
//
// Arguments:
//    reg    - The register being saved.
//    offset - The offset from the current stack pointer where the register is being saved.
//
void CodeGen::unwindSaveReg(regNumber reg, unsigned offset)
{
#ifdef TARGET_UNIX
    if (generateCFIUnwindCodes())
    {
        unwindSaveRegCFI(reg, offset);
    }
#endif

    unwindSaveRegWindows(reg, offset);
}

void CodeGen::unwindSaveRegWindows(regNumber reg, unsigned offset)
{
    assert(generatingProlog);

    FuncInfoDsc* func = funCurrentFunc();

    assert(func->unwindHeader.Version == 1);            // Can't call this before unwindBegProlog
    assert(func->unwindHeader.CountOfUnwindCodes == 0); // Can't call this after unwindReserve
    if (RBM_CALLEE_SAVED & genRegMask(reg))
    {
        UNWIND_CODE* code;
        if (offset < 0x80000)
        {
            assert(func->unwindCodeSlot > (sizeof(UNWIND_CODE) + sizeof(uint16_t)));
            uint16_t* codedSize = (uint16_t*)&func->unwindCodes[func->unwindCodeSlot -= sizeof(uint16_t)];
            code                = (UNWIND_CODE*)&func->unwindCodes[func->unwindCodeSlot -= sizeof(UNWIND_CODE)];

            // As per AMD64 ABI, if saving entire xmm reg, then offset need to be scaled by 16.
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
            assert(func->unwindCodeSlot > (sizeof(UNWIND_CODE) + sizeof(uint32_t)));
            uint32_t* codedSize = (uint32_t*)&func->unwindCodes[func->unwindCodeSlot -= sizeof(uint32_t)];
            *codedSize          = offset;
            code                = (UNWIND_CODE*)&func->unwindCodes[func->unwindCodeSlot -= sizeof(UNWIND_CODE)];
            code->UnwindOp      = (genIsValidFloatReg(reg)) ? UWOP_SAVE_XMM128_FAR : UWOP_SAVE_NONVOL_FAR;
        }
        code->OpInfo      = (uint8_t)reg;
        uint32_t cbProlog = unwindGetCurrentOffset();
        noway_assert((uint8_t)cbProlog == cbProlog);
        code->CodeOffset = (uint8_t)cbProlog;
    }
}

#ifdef UNIX_AMD64_ABI
void CodeGen::unwindSaveRegCFI(regNumber reg, unsigned offset)
{
    assert(generatingProlog);

    if (RBM_CALLEE_SAVED & genRegMask(reg))
    {
        FuncInfoDsc* func = funCurrentFunc();

        uint32_t cbProlog = unwindGetCurrentOffset();
        createCfiCode(func, cbProlog, CFI_REL_OFFSET, mapRegNumToDwarfReg(reg), offset);
    }
}
#endif // UNIX_AMD64_ABI

#ifdef DEBUG

//------------------------------------------------------------------------
// DumpUnwindInfo: Dump the unwind data.
//
// Arguments:
//    isHotCode   - true if this unwind data is for the hot section, false otherwise.
//    startOffset - byte offset of the code start that this unwind data represents.
//    endOffset   - byte offset of the code end   that this unwind data represents.
//    pHeader     - pointer to the unwind data blob.
//
void DumpUnwindInfo(bool isHotCode, uint32_t startOffset, uint32_t endOffset, const UNWIND_INFO* const pHeader)
{
    printf("Unwind Info%s:\n", isHotCode ? "" : " COLD");
    printf("  >> Start offset   : 0x%06x (not in unwind data)\n", dspOffset(startOffset));
    printf("  >>   End offset   : 0x%06x (not in unwind data)\n", dspOffset(endOffset));

    if (pHeader == nullptr)
    {
        // Cold AMD64 code doesn't have unwind info; the VM creates chained unwind info.
        assert(!isHotCode);
        return;
    }

    printf("  Version           : %u\n", pHeader->Version);
    printf("  Flags             : 0x%02x", pHeader->Flags);
    if (pHeader->Flags)
    {
        const UCHAR flags = pHeader->Flags;
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
    printf("  SizeOfProlog      : 0x%02X\n", pHeader->SizeOfProlog);
    printf("  CountOfUnwindCodes: %u\n", pHeader->CountOfUnwindCodes);
    printf("  FrameRegister     : %s (%u)\n",
           (pHeader->FrameRegister == 0) ? "none" : getRegName(pHeader->FrameRegister),
           pHeader->FrameRegister); // RAX (0) is not allowed as a frame register
    if (pHeader->FrameRegister == 0)
    {
        printf("  FrameOffset       : N/A (no FrameRegister) (Value=%u)\n", pHeader->FrameOffset);
    }
    else
    {
        printf("  FrameOffset       : %u * 16 = 0x%02X\n", pHeader->FrameOffset, pHeader->FrameOffset * 16);
    }
    printf("  UnwindCodes       :\n");

    for (unsigned i = 0; i < pHeader->CountOfUnwindCodes; i++)
    {
        const UNWIND_CODE* const pCode = &(pHeader->UnwindCode[i]);
        switch (pCode->UnwindOp)
        {
            case UWOP_PUSH_NONVOL:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_PUSH_NONVOL (%u)     OpInfo: %s (%u)\n",
                       pCode->CodeOffset, pCode->UnwindOp, getRegName(pCode->OpInfo), pCode->OpInfo);
                break;

            case UWOP_ALLOC_LARGE:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_ALLOC_LARGE (%u)     OpInfo: %u - ", pCode->CodeOffset,
                       pCode->UnwindOp, pCode->OpInfo);
                if (pCode->OpInfo == 0)
                {
                    i++;
                    printf("Scaled small  \n      Size: %u * 8 = %u = 0x%05X\n", pHeader->UnwindCode[i].FrameOffset,
                           pHeader->UnwindCode[i].FrameOffset * 8, pHeader->UnwindCode[i].FrameOffset * 8);
                }
                else if (pCode->OpInfo == 1)
                {
                    i++;
                    printf("Unscaled large\n      Size: %u = 0x%08X\n\n", *(uint32_t*)&(pHeader->UnwindCode[i]),
                           *(uint32_t*)&(pHeader->UnwindCode[i]));
                    i++;
                }
                else
                {
                    printf("Unknown\n");
                }
                break;

            case UWOP_ALLOC_SMALL:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_ALLOC_SMALL (%u)     OpInfo: %u * 8 + 8 = %u = 0x%02X\n",
                       pCode->CodeOffset, pCode->UnwindOp, pCode->OpInfo, pCode->OpInfo * 8 + 8, pCode->OpInfo * 8 + 8);
                break;

            case UWOP_SET_FPREG:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_SET_FPREG (%u)       OpInfo: Unused (%u)\n",
                       pCode->CodeOffset, pCode->UnwindOp, pCode->OpInfo); // This should be zero
                break;

#ifdef UNIX_AMD64_ABI

            case UWOP_SET_FPREG_LARGE:
            {
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_SET_FPREG_LARGE (%u) OpInfo: Unused (%u)\n",
                       pCode->CodeOffset, pCode->UnwindOp, pCode->OpInfo); // This should be zero
                i++;
                unsigned offset = *(uint32_t*)&(pHeader->UnwindCode[i]);
                i++;
                printf("      Scaled Offset: %u * 16 = %u = 0x%08X\n", offset, offset * 16, offset * 16);
                if ((offset & 0xF0000000) != 0)
                {
                    printf("      Illegal unscaled offset: too large\n");
                }
            }
            break;

#endif // UNIX_AMD64_ABI

            case UWOP_SAVE_NONVOL:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_SAVE_NONVOL (%u)     OpInfo: %s (%u)\n",
                       pCode->CodeOffset, pCode->UnwindOp, getRegName(pCode->OpInfo), pCode->OpInfo);
                i++;
                printf("      Scaled Small Offset: %u * 8 = %u = 0x%05X\n", pHeader->UnwindCode[i].FrameOffset,
                       pHeader->UnwindCode[i].FrameOffset * 8, pHeader->UnwindCode[i].FrameOffset * 8);
                break;

            case UWOP_SAVE_NONVOL_FAR:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_SAVE_NONVOL_FAR (%u) OpInfo: %s (%u)\n",
                       pCode->CodeOffset, pCode->UnwindOp, getRegName(pCode->OpInfo), pCode->OpInfo);
                i++;
                printf("      Unscaled Large Offset: 0x%08X\n\n", *(uint32_t*)&(pHeader->UnwindCode[i]));
                i++;
                break;

            case UWOP_SAVE_XMM128:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_SAVE_XMM128 (%u)     OpInfo: XMM%u (%u)\n",
                       pCode->CodeOffset, pCode->UnwindOp, pCode->OpInfo, pCode->OpInfo);
                i++;
                printf("      Scaled Small Offset: %u * 16 = %u = 0x%05X\n", pHeader->UnwindCode[i].FrameOffset,
                       pHeader->UnwindCode[i].FrameOffset * 16, pHeader->UnwindCode[i].FrameOffset * 16);
                break;

            case UWOP_SAVE_XMM128_FAR:
                printf("    CodeOffset: 0x%02X UnwindOp: UWOP_SAVE_XMM128_FAR (%u) OpInfo: XMM%u (%u)\n",
                       pCode->CodeOffset, pCode->UnwindOp, pCode->OpInfo, pCode->OpInfo);
                i++;
                printf("      Unscaled Large Offset: 0x%08X\n\n", *(uint32_t*)&(pHeader->UnwindCode[i]));
                i++;
                break;

            case UWOP_EPILOG:
            case UWOP_SPARE_CODE:
            case UWOP_PUSH_MACHFRAME:
            default:
                printf("    Unrecognized UNWIND_CODE: 0x%04X\n", *(uint16_t*)pCode);
                break;
        }
    }
}

#endif // DEBUG

// Ask the VM to reserve space for the unwind information
// for the function and all its funclets. Called once, just before asking the VM
// for memory and emitting the generated code. Calls unwindReserveFunc() to handle
// the main function and each of the funclets, in turn.
//
void CodeGen::unwindReserve()
{
    assert(!generatingProlog);
    assert(!generatingEpilog);

    assert(compFuncInfoCount > 0);
    for (unsigned funcIdx = 0; funcIdx < compFuncInfoCount; funcIdx++)
    {
        unwindReserveFunc(funGetFunc(funcIdx));
    }
}

// Reserve the unwind information from the VM for a
// given main function or funclet.
//
// Arguments:
//    func - The main function or funclet to reserve unwind info for.
//
void CodeGen::unwindReserveFunc(FuncInfoDsc* func)
{
    unwindReserveFuncHelper(func, true);

    if (compiler->fgFirstColdBlock != nullptr)
    {
        unwindReserveFuncHelper(func, false);
    }
}

// Reserve the unwind information from the VM for a
// given main function or funclet, for either the hot or the cold section.
//
// Arguments:
//    func      - The main function or funclet to reserve unwind info for.
//    isHotCode - 'true' to reserve the hot section, 'false' to reserve the cold section.
//
void CodeGen::unwindReserveFuncHelper(FuncInfoDsc* func, bool isHotCode)
{
    uint32_t unwindCodeBytes = 0;

    if (isHotCode)
    {
#ifdef TARGET_UNIX
        if (generateCFIUnwindCodes())
        {
            unwindCodeBytes = static_cast<uint32_t>(func->cfiCodes->size() * sizeof(CFI_CODE));
        }
        else
#endif
        {
            assert(func->unwindHeader.Version == 1);            // Can't call this before unwindBegProlog
            assert(func->unwindHeader.CountOfUnwindCodes == 0); // Only call this once per prolog

            // Set the size of the prolog to be the last encoded action
            if (func->unwindCodeSlot < sizeof(func->unwindCodes))
            {
                UNWIND_CODE* code = reinterpret_cast<UNWIND_CODE*>(&func->unwindCodes[func->unwindCodeSlot]);
                func->unwindHeader.SizeOfProlog = code->CodeOffset;
            }
            else
            {
                func->unwindHeader.SizeOfProlog = 0;
            }

            func->unwindHeader.CountOfUnwindCodes =
                static_cast<uint8_t>((sizeof(func->unwindCodes) - func->unwindCodeSlot) / sizeof(UNWIND_CODE));

            // Prepend the unwindHeader onto the unwind codes
            assert(func->unwindCodeSlot >= offsetof(UNWIND_INFO, UnwindCode));

            func->unwindCodeSlot -= offsetof(UNWIND_INFO, UnwindCode);
            UNWIND_INFO* pHeader = reinterpret_cast<UNWIND_INFO*>(&func->unwindCodes[func->unwindCodeSlot]);
            memcpy(pHeader, &func->unwindHeader, offsetof(UNWIND_INFO, UnwindCode));

            unwindCodeBytes = sizeof(func->unwindCodes) - func->unwindCodeSlot;
        }
    }

    compiler->eeReserveUnwindInfo(func->funKind != FUNC_ROOT, !isHotCode, unwindCodeBytes);
}

void CodeGen::unwindEmit(void* hotCode, void* coldCode)
{
    assert(!generatingProlog);
    assert(!generatingEpilog);
    assert(compFuncInfoCount > 0);

    for (unsigned i = 0; i < compFuncInfoCount; i++)
    {
        unwindEmitFunc(funGetFunc(i), hotCode, coldCode);
    }
}

void CodeGen::unwindEmitFunc(FuncInfoDsc* func, void* hotCode, void* coldCode)
{
    unwindEmitFuncHelper(func, hotCode, coldCode, true);

    if (coldCode != nullptr)
    {
        unwindEmitFuncHelper(func, hotCode, coldCode, false);
    }
}

void CodeGen::unwindEmitFuncHelper(FuncInfoDsc* func, void* hotCode, void* coldCode, bool isHotCode)
{
    uint32_t startOffset;
    uint32_t endOffset;
    uint32_t unwindCodeBytes = 0;
    uint8_t* unwindBlock     = nullptr;

    if (isHotCode)
    {
        insGroup* startLoc = nullptr;
        insGroup* endLoc   = nullptr;
        unwindGetFuncRange(func, true, &startLoc, &endLoc);

        startOffset = GetEmitter()->GetCodeOffset(startLoc);
        endOffset   = endLoc == nullptr ? compNativeCodeSize : GetEmitter()->GetCodeOffset(endLoc);

#ifdef TARGET_UNIX
        if (generateCFIUnwindCodes())
        {
            uint32_t size = static_cast<uint32_t>(func->cfiCodes->size());

            if (size > 0)
            {
                unwindCodeBytes = size * sizeof(CFI_CODE);
                unwindBlock     = reinterpret_cast<uint8_t*>(func->cfiCodes->data());
            }
        }
        else
#endif
        {
            unwindCodeBytes = sizeof(func->unwindCodes) - func->unwindCodeSlot;
            unwindBlock     = &func->unwindCodes[func->unwindCodeSlot];

            UNWIND_INFO* unwindInfo = reinterpret_cast<UNWIND_INFO*>(&func->unwindCodes[func->unwindCodeSlot]);
            assert(unwindCodeBytes ==
                   offsetof(UNWIND_INFO, UnwindCode) + unwindInfo->CountOfUnwindCodes * sizeof(UNWIND_CODE));
        }
    }
    else
    {
        insGroup* startLoc = nullptr;
        insGroup* endLoc   = nullptr;
        unwindGetFuncRange(func, false, &startLoc, &endLoc);

        startOffset = GetEmitter()->GetCodeOffset(startLoc);
        endOffset   = endLoc == nullptr ? compNativeCodeSize : GetEmitter()->GetCodeOffset(endLoc);
    }

#ifdef DEBUG
    if (compiler->opts.dspUnwind)
    {
#ifdef TARGET_UNIX
        if (generateCFIUnwindCodes())
        {
            DumpCfiInfo(isHotCode, startOffset, endOffset, unwindCodeBytes / sizeof(CFI_CODE),
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

    // Verify that the JIT enum is in sync with the JIT-EE interface enum
    static_assert_no_msg(FUNC_ROOT == (FuncKind)CORJIT_FUNC_ROOT);
    static_assert_no_msg(FUNC_HANDLER == (FuncKind)CORJIT_FUNC_HANDLER);
    static_assert_no_msg(FUNC_FILTER == (FuncKind)CORJIT_FUNC_FILTER);

    compiler->eeAllocUnwindInfo(hotCode, coldCode, startOffset, endOffset, unwindCodeBytes, unwindBlock,
                                static_cast<CorJitFuncKind>(func->funKind));
}

#endif // TARGET_AMD64
