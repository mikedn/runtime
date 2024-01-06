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

#if defined(TARGET_AMD64) || defined(TARGET_UNIX)
uint32_t CodeGen::unwindGetCurrentOffset()
{
    return GetEmitter()->emitGetCurrentPrologCodeSize();
}
#endif

#ifdef FEATURE_EH_FUNCLETS

// Get the start/end emitter locations for this
// function or funclet. If 'getHotSectionData' is true, get the start/end locations
// for the hot section. Otherwise, get the data for the cold section.
//
// Note that we grab these locations before the prolog and epilogs are generated, so the
// locations must remain correct after the prolog and epilogs are generated.
//
// For the prolog, instructions are put in the special, preallocated, prolog instruction group.
// We don't want to expose the emitPrologIG unnecessarily (locations are actually pointers to
// emitter instruction groups). Since we know the offset of the start of the function/funclet,
// where the prolog is, will be zero, we use a nullptr start location to indicate that.
//
// There is no instruction group beyond the end of the end of the function, so there is no
// location to indicate that. Once again, use nullptr for that.
//
// Intermediate locations point at the first instruction group of a funclet, which is a
// placeholder IG. These are converted to real IGs, not deleted and replaced, so the location
// remains valid.
//
// Arguments:
//    func              - main function or funclet to get locations for.
//    getHotSectionData - 'true' to get the hot section data, 'false' to get the cold section data.
//    ppStartLoc        - OUT parameter. Set to the start emitter location.
//    ppEndLoc          - OUT parameter. Set to the end   emitter location (the location immediately
//                        the range; the 'end' location is not inclusive).
//
// Notes:
//    A start location of nullptr means the beginning of the code.
//    An end location of nullptr means the end of the code.
//
void CodeGen::unwindGetFuncLocations(FuncInfoDsc*   func,
                                     bool           getHotSectionData,
                                     emitLocation** ppStartLoc,
                                     emitLocation** ppEndLoc)
{
    if (func->funKind == FUNC_ROOT)
    {
        // Since all funclets are pulled out of line, the main code size is everything
        // up to the first handler. If the function is hot/cold split, we need to get the
        // appropriate sub-range.

        if (getHotSectionData)
        {
            *ppStartLoc = nullptr; // nullptr emit location means the beginning of the code. This is to handle the first
                                   // fragment prolog.

            if (compiler->fgFirstColdBlock != nullptr)
            {
                // The hot section only goes up to the cold section
                assert(compiler->fgFirstFuncletBB == nullptr);

                *ppEndLoc = new (compiler, CMK_UnwindInfo) emitLocation(ehEmitLabel(compiler->fgFirstColdBlock));
            }
            else
            {
                if (compiler->fgFirstFuncletBB != nullptr)
                {
                    *ppEndLoc = new (compiler, CMK_UnwindInfo) emitLocation(ehEmitLabel(compiler->fgFirstFuncletBB));
                }
                else
                {
                    *ppEndLoc = nullptr; // nullptr end location means the end of the code
                }
            }
        }
        else
        {
            assert(compiler->fgFirstFuncletBB == nullptr); // TODO-CQ: support hot/cold splitting in functions with EH
            assert(compiler->fgFirstColdBlock != nullptr); // There better be a cold section!

            *ppStartLoc = new (compiler, CMK_UnwindInfo) emitLocation(ehEmitLabel(compiler->fgFirstColdBlock));
            *ppEndLoc   = nullptr; // nullptr end location means the end of the code
        }
    }
    else
    {
        assert(getHotSectionData); // TODO-CQ: support funclets in cold section

        EHblkDsc* HBtab = compiler->ehGetDsc(func->funEHIndex);

        if (func->funKind == FUNC_FILTER)
        {
            assert(HBtab->HasFilter());
            *ppStartLoc = new (compiler, CMK_UnwindInfo) emitLocation(ehEmitLabel(HBtab->ebdFilter));
            *ppEndLoc   = new (compiler, CMK_UnwindInfo) emitLocation(ehEmitLabel(HBtab->ebdHndBeg));
        }
        else
        {
            assert(func->funKind == FUNC_HANDLER);
            *ppStartLoc = new (compiler, CMK_UnwindInfo) emitLocation(ehEmitLabel(HBtab->ebdHndBeg));
            *ppEndLoc   = (HBtab->ebdHndLast->bbNext == nullptr)
                            ? nullptr
                            : new (compiler, CMK_UnwindInfo) emitLocation(ehEmitLabel(HBtab->ebdHndLast->bbNext));
        }
    }
}

#endif // FEATURE_EH_FUNCLETS

#ifdef TARGET_UNIX

void CodeGen::createCfiCode(FuncInfoDsc* func, uint32_t codeOffset, uint8_t cfiOpcode, int16_t dwarfReg, int32_t offset)
{
    noway_assert(FitsIn<uint8_t>(codeOffset));

    func->cfiCodes->emplace_back(static_cast<uint8_t>(codeOffset), cfiOpcode, dwarfReg, offset);
}

void CodeGen::unwindPushPopCFI(regNumber reg)
{
    assert(generatingProlog);

    FuncInfoDsc* func     = funCurrentFunc();
    uint32_t     cbProlog = unwindGetCurrentOffset();

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
        createCfiCode(func, cbProlog, CFI_ADJUST_CFA_OFFSET, DWARF_REG_ILLEGAL, REGSIZE_BYTES);
#endif
        createCfiCode(func, cbProlog, CFI_REL_OFFSET, mapRegNumToDwarfReg(reg));
    }
    else
    {
        createCfiCode(func, cbProlog, CFI_ADJUST_CFA_OFFSET, DWARF_REG_ILLEGAL, REGSIZE_BYTES);
    }
}

void CodeGen::unwindBegPrologCFI()
{
    assert(generatingProlog);

#ifdef FEATURE_EH_FUNCLETS
    FuncInfoDsc* func = funCurrentFunc();

    func->cfiCodes = new (compiler, CMK_UnwindInfo) jitstd::vector<CFI_CODE>(compiler->getAllocator(CMK_UnwindInfo));
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

    FuncInfoDsc* func     = funCurrentFunc();
    uint32_t     cbProlog = unwindGetCurrentOffset();

    createCfiCode(func, cbProlog, CFI_ADJUST_CFA_OFFSET, DWARF_REG_ILLEGAL, size);
}

// Record a cfi info for a frame register set.
//
// Arguments:
//    reg    - The register being set as the frame register.
//    offset - The offset from the current stack pointer that the frame pointer will point at.
//
void CodeGen::unwindSetFrameRegCFI(regNumber reg, unsigned offset)
{
    assert(generatingProlog);

    FuncInfoDsc* func     = funCurrentFunc();
    uint32_t     cbProlog = unwindGetCurrentOffset();

    createCfiCode(func, cbProlog, CFI_DEF_CFA_REGISTER, mapRegNumToDwarfReg(reg));

    if (offset != 0)
    {
        // before: cfa = rsp + old_cfa_offset;
        //         rbp = rsp + offset;
        // after: cfa should be based on rbp, but points to the old address:
        //         rsp + old_cfa_offset == rbp + old_cfa_offset + adjust;
        // adjust = -offset;
        int adjust = -(int)offset;
        createCfiCode(func, cbProlog, CFI_ADJUST_CFA_OFFSET, DWARF_REG_ILLEGAL, adjust);
    }
}

void CodeGen::unwindEmitFuncCFI(FuncInfoDsc* func, void* hotCode, void* coldCode)
{
    emitLocation* startLoc     = nullptr;
    emitLocation* endLoc       = nullptr;
    emitLocation* coldStartLoc = nullptr;
    emitLocation* coldEndLoc   = nullptr;
    unwindGetFuncLocations(func, true, &startLoc, &endLoc);

    if (compiler->fgFirstColdBlock != nullptr)
    {
        unwindGetFuncLocations(func, false, &coldStartLoc, &coldEndLoc);
    }

    uint32_t startOffset = startLoc == nullptr ? 0 : GetEmitter()->GetCodeOffset(startLoc);
    uint32_t endOffset   = endLoc == nullptr ? compNativeCodeSize : GetEmitter()->GetCodeOffset(endLoc);

    assert(endOffset <= compTotalHotCodeSize);

    uint32_t  codeCount = static_cast<uint32_t>(func->cfiCodes->size());
    CFI_CODE* codes     = codeCount == 0 ? nullptr : func->cfiCodes->data();

    DBEXEC(compiler->opts.dspUnwind, DumpCfiInfo(/* isHotCode */ true, startOffset, endOffset, codeCount, codes));

    compiler->eeAllocUnwindInfo(hotCode, nullptr, startOffset, endOffset, codeCount * sizeof(CFI_CODE), codes,
                                static_cast<CorJitFuncKind>(func->funKind));

    if (coldCode != nullptr)
    {
        assert(compiler->fgFirstColdBlock != nullptr);
        assert(func->funKind == FUNC_ROOT); // No splitting of funclets.

        startOffset = coldStartLoc == nullptr ? 0 : GetEmitter()->GetCodeOffset(coldStartLoc);
        endOffset   = coldEndLoc == nullptr ? compNativeCodeSize : GetEmitter()->GetCodeOffset(coldEndLoc);

        assert(startOffset >= compTotalHotCodeSize);

        DBEXEC(compiler->opts.dspUnwind, DumpCfiInfo(/* isHotCode */ false, startOffset, endOffset, 0, nullptr));

        startOffset -= compTotalHotCodeSize;
        endOffset -= compTotalHotCodeSize;

        compiler->eeAllocUnwindInfo(hotCode, coldCode, startOffset, endOffset, 0, nullptr,
                                    static_cast<CorJitFuncKind>(func->funKind));
    }
}

#ifdef DEBUG
void CodeGen::DumpCfiInfo(
    bool isHotCode, uint32_t startOffset, uint32_t endOffset, uint32_t count, const CFI_CODE* codes)
{
    printf("Cfi Info%s:\n", isHotCode ? "" : " COLD");
    printf("  >> Start offset   : 0x%06x \n", dspOffset(startOffset));
    printf("  >>   End offset   : 0x%06x \n", dspOffset(endOffset));

    for (unsigned i = 0; i < count; i++)
    {
        const CFI_CODE& code = codes[i];

        UCHAR codeOffset = code.CodeOffset;
        SHORT dwarfReg   = code.DwarfReg;
        INT   offset     = code.Offset;

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
