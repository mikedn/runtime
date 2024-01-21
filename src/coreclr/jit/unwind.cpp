// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "unwind.h"
#include "codegen.h"

#if defined(TARGET_AMD64) || defined(TARGET_UNIX)
uint32_t CodeGen::unwindGetCurrentOffset()
{
    return GetEmitter()->emitGetCurrentPrologCodeSize();
}
#endif

#ifdef FEATURE_EH_FUNCLETS

// Get the start/end emitter locations for this function or funclet.
// There is no instruction group beyond the end of the end of the function,
// so null is used to indicate that.
void CodeGen::unwindGetFuncHotRange(FuncInfoDsc* func, insGroup** start, insGroup** end)
{
    if (func->kind == FUNC_ROOT)
    {
        // Since all funclets are pulled out of line, the main code size is everything
        // up to the first handler. If the function is hot/cold split, we need to get
        // the appropriate sub-range.

        *start = GetEmitter()->GetProlog();

        if (compiler->fgFirstColdBlock != nullptr)
        {
            // The hot section only goes up to the cold section
            assert(compiler->fgFirstFuncletBB == nullptr);

            *end = ehEmitLabel(compiler->fgFirstColdBlock);
        }
        else if (compiler->fgFirstFuncletBB != nullptr)
        {
            *end = ehEmitLabel(compiler->fgFirstFuncletBB);
        }
        else
        {
            *end = nullptr;
        }
    }
    else
    {
        EHblkDsc* HBtab = compiler->ehGetDsc(func->ehIndex);

        if (func->kind == FUNC_FILTER)
        {
            assert(HBtab->HasFilter());

            *start = ehEmitLabel(HBtab->ebdFilter);
            *end   = ehEmitLabel(HBtab->ebdHndBeg);
        }
        else
        {
            assert(func->kind == FUNC_HANDLER);

            *start = ehEmitLabel(HBtab->ebdHndBeg);
            *end   = HBtab->ebdHndLast->bbNext == nullptr ? nullptr : ehEmitLabel(HBtab->ebdHndLast->bbNext);
        }
    }
}

CodeRange CodeGen::unwindGetFuncHotRange(FuncInfoDsc* func)
{
    insGroup* startLoc = nullptr;
    insGroup* endLoc   = nullptr;
    unwindGetFuncHotRange(func, &startLoc, &endLoc);

    return {startLoc->GetCodeOffset(), endLoc == nullptr ? GetEmitter()->GetCodeSize() : endLoc->GetCodeOffset()};
}

void CodeGen::unwindGetFuncColdRange(FuncInfoDsc* func, insGroup** start, insGroup** end)
{
    assert(func->kind == FUNC_ROOT); // TODO-CQ: support funclets in cold section

    // Since all funclets are pulled out of line, the main code size is everything
    // up to the first handler. If the function is hot/cold split, we need to get the
    // appropriate sub-range.

    assert(compiler->fgFirstFuncletBB == nullptr); // TODO-CQ: support hot/cold splitting in functions with EH
    assert(compiler->fgFirstColdBlock != nullptr); // There better be a cold section!

    *start = ehEmitLabel(compiler->fgFirstColdBlock);
    *end   = nullptr;
}

CodeRange CodeGen::unwindGetFuncColdRange(FuncInfoDsc* func)
{
    insGroup* startLoc = nullptr;
    insGroup* endLoc   = nullptr;
    unwindGetFuncColdRange(func, &startLoc, &endLoc);

    return {startLoc->GetCodeOffset(), endLoc == nullptr ? GetEmitter()->GetCodeSize() : endLoc->GetCodeOffset()};
}

#ifdef TARGET_UNIX

void CfiUnwindInfo::AddCode(uint32_t codeOffset, uint8_t cfiOpcode, int16_t dwarfReg, int32_t offset)
{
    noway_assert(FitsIn<uint8_t>(codeOffset));

    codes->emplace_back(static_cast<uint8_t>(codeOffset), cfiOpcode, dwarfReg, offset);
}

void CodeGen::unwindPushPopCFI(RegNum reg)
{
    assert(generatingProlog);

    CfiUnwindInfo& cfi        = funCurrentFunc().cfi;
    uint32_t       codeOffset = unwindGetCurrentOffset();

    regMaskTP relOffsetMask = RBM_CALLEE_SAVED

#if defined(UNIX_AMD64_ABI) && ETW_EBP_FRAMED
                              // In case of ETW_EBP_FRAMED defined the REG_FPBASE (RBP)
                              // is excluded from the callee-save register list.
                              // Make sure the register gets PUSH unwind info in this case,
                              // since it is pushed as a frame register.
                              | RBM_FPBASE
#endif
#ifdef TARGET_ARM
                              | RBM_R11 | RBM_LR | RBM_PC
#endif
        ;

    if (relOffsetMask & genRegMask(reg))
    {
#ifndef TARGET_ARM
        cfi.AddCode(codeOffset, CFI_ADJUST_CFA_OFFSET, DWARF_REG_ILLEGAL, REGSIZE_BYTES);
#endif
        cfi.AddCode(codeOffset, CFI_REL_OFFSET, mapRegNumToDwarfReg(reg));
    }
    else
    {
        cfi.AddCode(codeOffset, CFI_ADJUST_CFA_OFFSET, DWARF_REG_ILLEGAL, REGSIZE_BYTES);
    }
}

void CodeGen::unwindBegPrologCFI()
{
    assert(generatingProlog);

#ifdef FEATURE_EH_FUNCLETS
    CfiUnwindInfo& cfi = funCurrentFunc().cfi;

    cfi.codes = new (compiler, CMK_UnwindInfo) jitstd::vector<CFI_CODE>(compiler->getAllocator(CMK_UnwindInfo));
#endif
}

void CodeGen::unwindPushPopMaskCFI(regMaskTP regMask, bool isFloat)
{
    regMaskTP regBit = isFloat ? genRegMask(REG_FP_FIRST) : 1;

    for (RegNum regNum = isFloat ? REG_FP_FIRST : REG_FIRST; regNum < REG_COUNT;)
    {
        if (regBit > regMask)
        {
            break;
        }

        if (regBit & regMask)
        {
            unwindPushPopCFI(regNum);
        }

#if TARGET_ARM
        // JIT for ARM emit local variables in S0-S31 registers,
        // which cannot be emitted to DWARF when using LLVM,
        // because LLVM only know about D0-D31.
        // As such pairs Sx,Sx+1 are referenced as D0-D15 registers in DWARF
        // For that we process registers in pairs.
        regNum = isFloat ? REG_NEXT(REG_NEXT(regNum)) : REG_NEXT(regNum);
        regBit <<= isFloat ? 2 : 1;
#else
        regNum = REG_NEXT(regNum);
        regBit <<= 1;
#endif
    }
}

void CodeGen::unwindAllocStackCFI(unsigned size)
{
    assert(generatingProlog);

    CfiUnwindInfo& cfi        = funCurrentFunc().cfi;
    uint32_t       codeOffset = unwindGetCurrentOffset();

    cfi.AddCode(codeOffset, CFI_ADJUST_CFA_OFFSET, DWARF_REG_ILLEGAL, size);
}

void CodeGen::unwindSetFrameRegCFI(RegNum reg, unsigned offset)
{
    assert(generatingProlog);

    CfiUnwindInfo& cfi        = funCurrentFunc().cfi;
    uint32_t       codeOffset = unwindGetCurrentOffset();

    cfi.AddCode(codeOffset, CFI_DEF_CFA_REGISTER, mapRegNumToDwarfReg(reg));

    if (offset != 0)
    {
        // before: cfa = rsp + old_cfa_offset;
        //         rbp = rsp + offset;
        // after: cfa should be based on rbp, but points to the old address:
        //         rsp + old_cfa_offset == rbp + old_cfa_offset + adjust;
        // adjust = -offset;
        int adjust = -(int)offset;
        cfi.AddCode(codeOffset, CFI_ADJUST_CFA_OFFSET, DWARF_REG_ILLEGAL, adjust);
    }
}

void CodeGen::unwindEmitFuncCFI(FuncInfoDsc* func)
{
    uint32_t  codeCount = static_cast<uint32_t>(func->cfi.codes->size());
    CFI_CODE* codes     = codeCount == 0 ? nullptr : func->cfi.codes->data();

    eeAllocUnwindInfo(func->kind, true, unwindGetFuncHotRange(func), codeCount * sizeof(CFI_CODE), codes);

    if (GetEmitter()->GetColdCodeAddr() != nullptr)
    {
        eeAllocUnwindInfo(func->kind, false, unwindGetFuncColdRange(func), 0, nullptr);
    }
}

#ifdef DEBUG
void CodeGen::DumpCfiInfo(bool isHotCode, CodeRange range, uint32_t count, const CFI_CODE* codes) const
{
    printf("Cfi Info%s:\n", isHotCode ? "" : " COLD");
    printf("  >> Start offset   : 0x%06x \n", compiler->dspOffset(range.start));
    printf("  >>   End offset   : 0x%06x \n", compiler->dspOffset(range.end));

    for (unsigned i = 0; i < count; i++)
    {
        const CFI_CODE& code = codes[i];

        uint8_t codeOffset = code.CodeOffset;
        int16_t dwarfReg   = code.DwarfReg;
        int32_t offset     = code.Offset;

        switch (code.CfiOpCode)
        {
            case CFI_REL_OFFSET:
                printf("    CodeOffset: 0x%02X Op: RelOffset DwarfReg:0x%x Offset:0x%X\n", codeOffset, dwarfReg,
                       offset);
                break;
            case CFI_DEF_CFA_REGISTER:
                assert(offset == 0);
                printf("    CodeOffset: 0x%02X Op: DefCfaRegister DwarfReg:0x%X\n", codeOffset, dwarfReg);
                break;
            case CFI_ADJUST_CFA_OFFSET:
                assert(dwarfReg == DWARF_REG_ILLEGAL);
                printf("    CodeOffset: 0x%02X Op: AdjustCfaOffset Offset:0x%X\n", codeOffset, offset);
                break;
            default:
                printf("    Unrecognized CFI_CODE: 0x%IX\n", *reinterpret_cast<const uint64_t*>(&code));
                break;
        }
    }
}
#endif // DEBUG

#endif // TARGET_UNIX

void CodeGen::eeReserveUnwindInfo(bool isFunclet, bool isHotCode, uint32_t unwindSize)
{
    JITDUMP("reserveUnwindInfo(isFunclet=%s, isColdCode=%s, unwindSize=0x%x)\n", isFunclet ? "true" : "false",
            isHotCode ? "false" : "true", unwindSize);

    if (compiler->info.compMatchedVM)
    {
        compiler->info.compCompHnd->reserveUnwindInfo(isFunclet, !isHotCode, unwindSize);
    }
}

#ifdef DEBUG
static const char* GetFuncKindName(FuncKind kind)
{
    switch (kind)
    {
        case FUNC_ROOT:
            return "main function";
        case FUNC_HANDLER:
            return "handler";
        case FUNC_FILTER:
            return "filter";
        default:
            return "???";
    }
}
#endif

void CodeGen::eeAllocUnwindInfo(FuncKind kind, bool isHotCode, CodeRange range, uint32_t unwindSize, void* unwindBlock)
{
#ifdef DEBUG
    if (compiler->opts.dspUnwind)
    {
#ifdef TARGET_UNIX
        if (generateCFIUnwindCodes())
        {
            DumpCfiInfo(isHotCode, range, unwindSize / sizeof(CFI_CODE), static_cast<CFI_CODE*>(unwindBlock));
        }
        else
#endif
        {
#ifdef TARGET_AMD64
            DumpUnwindInfo(isHotCode, range, reinterpret_cast<UNWIND_INFO*>(unwindBlock));
#endif
#ifdef TARGET_ARMARCH
            DumpUnwindInfo(isHotCode, range, reinterpret_cast<uint8_t*>(unwindBlock), unwindSize);
#endif
        }
    }
#endif

    Emitter& emit         = *GetEmitter();
    uint8_t* hotCodeAddr  = emit.GetHotCodeAddr();
    uint32_t hotCodeSize  = emit.GetHotCodeSize();
    uint8_t* coldCodeAddr = emit.GetColdCodeAddr();

    if (isHotCode)
    {
        assert(range.end <= hotCodeSize);

        coldCodeAddr = nullptr;
    }
    else
    {
        assert(range.start >= hotCodeSize);

        range.start -= hotCodeSize;
        range.end -= hotCodeSize;
    }

    JITDUMP("allocUnwindInfo(hotCode=0x%p, coldCode=0x%p, startOffset=0x%x, endOffset=0x%x, unwindSize=0x%x, "
            "unwindBlock=0x%p, funKind=%s)\n",
            dspPtr(hotCodeAddr), dspPtr(coldCodeAddr), range.start, range.end, unwindSize, dspPtr(unwindBlock),
            GetFuncKindName(kind));

    // Verify that the JIT enum is in sync with the JIT-EE interface enum
    static_assert_no_msg(FUNC_ROOT == (FuncKind)CORJIT_FUNC_ROOT);
    static_assert_no_msg(FUNC_HANDLER == (FuncKind)CORJIT_FUNC_HANDLER);
    static_assert_no_msg(FUNC_FILTER == (FuncKind)CORJIT_FUNC_FILTER);

    if (compiler->info.compMatchedVM)
    {
        compiler->info.compCompHnd->allocUnwindInfo(hotCodeAddr, coldCodeAddr, range.start, range.end, unwindSize,
                                                    static_cast<uint8_t*>(unwindBlock),
                                                    static_cast<CorJitFuncKind>(kind));
    }
}
#endif // FEATURE_EH_FUNCLETS
