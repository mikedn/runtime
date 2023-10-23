// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_ARM64

#include "emit.h"
#include "codegen.h"
#include "lower.h"

// Generate an instruction with an immediate operand. Usually that's simply:
//
//    ins  reg1, reg2, imm
//
// However, the imm might not fit as a directly encodable immediate. When it
// doesn't fit we generate extra instruction(s) that sets up the 'regTmp'
// with the proper immediate value.
//
//     mov  regTmp, imm
//     ins  reg1, reg2, regTmp
//
// Returns true if the immediate was small enough to be encoded inside instruction.
// If not, returns false meaning the immediate was too large and tmpReg was used
// and modified.
//
bool CodeGen::genInstrWithConstant(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, ssize_t imm, regNumber tmpReg, bool inUnwindRegion)
{
    bool     immFitsInIns = false;
    emitAttr size         = EA_SIZE(attr);

    // reg1 is usually a dest register
    // reg2 is always source register
    assert(tmpReg != reg2); // regTmp can not match any source register

    switch (ins)
    {
        case INS_add:
        case INS_sub:
            if (imm < 0)
            {
                imm = -imm;
                ins = (ins == INS_add) ? INS_sub : INS_add;
            }
            immFitsInIns = emitter::emitIns_valid_imm_for_add(imm, size);
            break;

        case INS_strb:
        case INS_strh:
        case INS_str:
            // reg1 is a source register for store instructions
            assert(tmpReg != reg1); // regTmp can not match any source register
            immFitsInIns = emitter::emitIns_valid_imm_for_ldst_offset(imm, size);
            break;

        case INS_ldrsb:
        case INS_ldrsh:
        case INS_ldrsw:
        case INS_ldrb:
        case INS_ldrh:
        case INS_ldr:
            immFitsInIns = emitter::emitIns_valid_imm_for_ldst_offset(imm, size);
            break;

        default:
            assert(!"Unexpected instruction in genInstrWithConstant");
            break;
    }

    if (immFitsInIns)
    {
        // generate a single instruction that encodes the immediate directly
        GetEmitter()->emitIns_R_R_I(ins, attr, reg1, reg2, imm);
    }
    else
    {
        // caller can specify REG_NA  for tmpReg, when it "knows" that the immediate will always fit
        assert(tmpReg != REG_NA);

        // generate two or more instructions

        // first we load the immediate into tmpReg
        instGen_Set_Reg_To_Imm(size, tmpReg, imm);

        // when we are in an unwind code region
        // we record the extra instructions using unwindPadding()
        if (inUnwindRegion)
        {
            compiler->unwindPadding();
        }

        // generate the instruction using a three register encoding with the immediate in tmpReg
        GetEmitter()->emitIns_R_R_R(ins, attr, reg1, reg2, tmpReg);
    }
    return immFitsInIns;
}

// Add a specified constant value to the stack pointer in either the prolog or the epilog.
// The unwind codes for the generated instructions are produced. An available temporary
// register is required to be specified, in case the constant is too large to encode in
// an "add" instruction (or "sub" instruction if we choose to use one), such that we need
// to load the constant into a register first, before using it.
//
// spDelta          - the value to add to SP (can be negative)
// tmpReg           - an available temporary register
// pTmpRegIsZero    - If we use tmpReg, and pTmpRegIsZero is non-null, we set *pTmpRegIsZero to 'false'.
//                    Otherwise, we don't touch it.
// reportUnwindData - If true, report the change in unwind data. Otherwise, do not report it.
//
void CodeGen::genStackPointerAdjustment(ssize_t spDelta, regNumber tmpReg, bool* pTmpRegIsZero, bool reportUnwindData)
{
    // Even though INS_add is specified here, the encoder will choose either
    // an INS_add or an INS_sub and encode the immediate as a positive value
    //
    bool wasTempRegisterUsedForImm =
        !genInstrWithConstant(INS_add, EA_PTRSIZE, REG_SPBASE, REG_SPBASE, spDelta, tmpReg, true);
    if (wasTempRegisterUsedForImm)
    {
        if (pTmpRegIsZero != nullptr)
        {
            *pTmpRegIsZero = false;
        }
    }

    if (reportUnwindData)
    {
        // spDelta is negative in the prolog, positive in the epilog, but we always tell the unwind codes the positive
        // value.
        ssize_t  spDeltaAbs    = abs(spDelta);
        unsigned unwindSpDelta = (unsigned)spDeltaAbs;
        assert((ssize_t)unwindSpDelta == spDeltaAbs); // make sure that it fits in a unsigned

        compiler->unwindAllocStack(unwindSpDelta);
    }
}

// Save a pair of general-purpose or floating-point/SIMD registers in a function or funclet
// prolog. If possible, we use pre-indexed addressing to adjust SP and store the registers
// with a single instruction. The caller must ensure that we can use the STP instruction,
// and that spOffset will be in the legal range for that instruction.
//
// reg1            - First register of pair to save.
// reg2            - Second register of pair to save.
// spOffset        - The offset from SP to store reg1 (must be positive or zero).
// spDelta         - If non-zero, the amount to add to SP before the register saves (must be negative or zero).
// useSaveNextPair - True if the last prolog instruction was to save the previous register pair. This
//                   allows us to emit the "save_next" unwind code.
// tmpReg          - An available temporary register. Needed for the case of large frames.
// pTmpRegIsZero   - If we use tmpReg, and pTmpRegIsZero is non-null, we set *pTmpRegIsZero to 'false'.
//                   Otherwise, we don't touch it.
//
void CodeGen::genPrologSaveRegPair(regNumber reg1,
                                   regNumber reg2,
                                   int       spOffset,
                                   int       spDelta,
                                   bool      useSaveNextPair,
                                   regNumber tmpReg,
                                   bool*     pTmpRegIsZero)
{
    assert(spOffset >= 0);
    assert(spDelta <= 0);
    assert((spDelta % 16) == 0);                                  // SP changes must be 16-byte aligned
    assert(genIsValidFloatReg(reg1) == genIsValidFloatReg(reg2)); // registers must be both general-purpose, or both
                                                                  // FP/SIMD

    bool needToSaveRegs = true;
    if (spDelta != 0)
    {
        assert(!useSaveNextPair);
        if ((spOffset == 0) && (spDelta >= -512))
        {
            // We can use pre-indexed addressing.
            // stp REG, REG + 1, [SP, #spDelta]!
            // 64-bit STP offset range: -512 to 504, multiple of 8.
            GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_PTRSIZE, reg1, reg2, REG_SPBASE, spDelta, INS_OPTS_PRE_INDEX);
            compiler->unwindSaveRegPairPreindexed(reg1, reg2, spDelta);

            needToSaveRegs = false;
        }
        else // (spOffset != 0) || (spDelta < -512)
        {
            // We need to do SP adjustment separately from the store; we can't fold in a pre-indexed addressing and the
            // non-zero offset.

            // generate sub SP,SP,imm
            genStackPointerAdjustment(spDelta, tmpReg, pTmpRegIsZero, /* reportUnwindData */ true);
        }
    }

    if (needToSaveRegs)
    {
        // stp REG, REG + 1, [SP, #offset]
        // 64-bit STP offset range: -512 to 504, multiple of 8.
        assert(spOffset <= 504);
        assert((spOffset % 8) == 0);
        GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_PTRSIZE, reg1, reg2, REG_SPBASE, spOffset);

#ifdef TARGET_UNIX
        if (compiler->generateCFIUnwindCodes())
        {
            useSaveNextPair = false;
        }
#endif

        if (useSaveNextPair)
        {
            // This works as long as we've only been saving pairs, in order, and we've saved the previous one just
            // before this one.
            compiler->unwindSaveNext();
        }
        else
        {
            compiler->unwindSaveRegPair(reg1, reg2, spOffset);
        }
    }
}

// Like genPrologSaveRegPair, but for a single register. Save a single general-purpose or
// floating-point/SIMD register in a function or funclet prolog. Note that if we wish to
// change SP (i.e., spDelta != 0), then spOffset must be 8. This is because otherwise we
// would create an alignment hole above the saved register, not below it, which we currently
// don't support. This restriction could be loosened if the callers change to handle it
// (and this function changes to support using pre-indexed STR addressing). The caller must
// ensure that we can use the STR instruction, and that spOffset will be in the legal range
// for that instruction.
//
// reg1          - Register to save.
// spOffset      - The offset from SP to store reg1 (must be positive or zero).
// spDelta       - If non-zero, the amount to add to SP before the register saves (must be negative or
//                 zero).
// tmpReg        - An available temporary register. Needed for the case of large frames.
// pTmpRegIsZero - If we use tmpReg, and pTmpRegIsZero is non-null, we set *pTmpRegIsZero to 'false'.
//                 Otherwise, we don't touch it.
//
void CodeGen::genPrologSaveReg(regNumber reg1, int spOffset, int spDelta, regNumber tmpReg, bool* pTmpRegIsZero)
{
    assert(spOffset >= 0);
    assert(spDelta <= 0);
    assert((spDelta % 16) == 0); // SP changes must be 16-byte aligned

    bool needToSaveRegs = true;
    if (spDelta != 0)
    {
        if ((spOffset == 0) && (spDelta >= -256))
        {
            // We can use pre-index addressing.
            // str REG, [SP, #spDelta]!
            GetEmitter()->emitIns_R_R_I(INS_str, EA_PTRSIZE, reg1, REG_SPBASE, spDelta, INS_OPTS_PRE_INDEX);
            compiler->unwindSaveRegPreindexed(reg1, spDelta);

            needToSaveRegs = false;
        }
        else // (spOffset != 0) || (spDelta < -256)
        {
            // generate sub SP,SP,imm
            genStackPointerAdjustment(spDelta, tmpReg, pTmpRegIsZero, /* reportUnwindData */ true);
        }
    }

    if (needToSaveRegs)
    {
        // str REG, [SP, #offset]
        // 64-bit STR offset range: 0 to 32760, multiple of 8.
        GetEmitter()->emitIns_R_R_I(INS_str, EA_PTRSIZE, reg1, REG_SPBASE, spOffset);
        compiler->unwindSaveReg(reg1, spOffset);
    }
}

// This is the opposite of genPrologSaveRegPair(), run in the epilog instead of the prolog.
// The stack pointer adjustment, if requested, is done after the register restore, using
// post-index addressing. The caller must ensure that we can use the LDP instruction, and
// that spOffset will be in the legal range for that instruction.
//
// reg1            - First register of pair to restore.
// reg2            - Second register of pair to restore.
// spOffset        - The offset from SP to load reg1 (must be positive or zero).
// spDelta         - If non-zero, the amount to add to SP after the register restores (must be positive or
//                   zero).
// useSaveNextPair - True if the last prolog instruction was to save the previous register pair. This
//                   allows us to emit the "save_next" unwind code.
// tmpReg          - An available temporary register. Needed for the case of large frames.
// pTmpRegIsZero   - If we use tmpReg, and pTmpRegIsZero is non-null, we set *pTmpRegIsZero to 'false'.
//                   Otherwise, we don't touch it.
//
void CodeGen::genEpilogRestoreRegPair(regNumber reg1,
                                      regNumber reg2,
                                      int       spOffset,
                                      int       spDelta,
                                      bool      useSaveNextPair,
                                      regNumber tmpReg,
                                      bool*     pTmpRegIsZero)
{
    assert(spOffset >= 0);
    assert(spDelta >= 0);
    assert((spDelta % 16) == 0);                                  // SP changes must be 16-byte aligned
    assert(genIsValidFloatReg(reg1) == genIsValidFloatReg(reg2)); // registers must be both general-purpose, or both
                                                                  // FP/SIMD

    if (spDelta != 0)
    {
        assert(!useSaveNextPair);
        if ((spOffset == 0) && (spDelta <= 504))
        {
            // Fold the SP change into this instruction.
            // ldp reg1, reg2, [SP], #spDelta
            GetEmitter()->emitIns_R_R_R_I(INS_ldp, EA_PTRSIZE, reg1, reg2, REG_SPBASE, spDelta, INS_OPTS_POST_INDEX);
            compiler->unwindSaveRegPairPreindexed(reg1, reg2, -spDelta);
        }
        else // (spOffset != 0) || (spDelta > 504)
        {
            // Can't fold in the SP change; need to use a separate ADD instruction.

            // ldp reg1, reg2, [SP, #offset]
            GetEmitter()->emitIns_R_R_R_I(INS_ldp, EA_PTRSIZE, reg1, reg2, REG_SPBASE, spOffset);
            compiler->unwindSaveRegPair(reg1, reg2, spOffset);

            // generate add SP,SP,imm
            genStackPointerAdjustment(spDelta, tmpReg, pTmpRegIsZero, /* reportUnwindData */ true);
        }
    }
    else
    {
        GetEmitter()->emitIns_R_R_R_I(INS_ldp, EA_PTRSIZE, reg1, reg2, REG_SPBASE, spOffset);

#ifdef TARGET_UNIX
        if (compiler->generateCFIUnwindCodes())
        {
            useSaveNextPair = false;
        }
#endif

        if (useSaveNextPair)
        {
            compiler->unwindSaveNext();
        }
        else
        {
            compiler->unwindSaveRegPair(reg1, reg2, spOffset);
        }
    }
}

// The opposite of genPrologSaveReg(), run in the epilog instead of the prolog.
//
// reg1          - Register to restore.
// spOffset      - The offset from SP to restore reg1 (must be positive or zero).
// spDelta       - If non-zero, the amount to add to SP after the register restores (must be positive or
//                 zero).
// tmpReg        - An available temporary register. Needed for the case of large frames.
// pTmpRegIsZero - If we use tmpReg, and pTmpRegIsZero is non-null, we set *pTmpRegIsZero to 'false'.
//                 Otherwise, we don't touch it.
//
void CodeGen::genEpilogRestoreReg(regNumber reg1, int spOffset, int spDelta, regNumber tmpReg, bool* pTmpRegIsZero)
{
    assert(spOffset >= 0);
    assert(spDelta >= 0);
    assert((spDelta % 16) == 0); // SP changes must be 16-byte aligned

    if (spDelta != 0)
    {
        if ((spOffset == 0) && (spDelta <= 255))
        {
            // We can use post-index addressing.
            // ldr REG, [SP], #spDelta
            GetEmitter()->emitIns_R_R_I(INS_ldr, EA_PTRSIZE, reg1, REG_SPBASE, spDelta, INS_OPTS_POST_INDEX);
            compiler->unwindSaveRegPreindexed(reg1, -spDelta);
        }
        else // (spOffset != 0) || (spDelta > 255)
        {
            // ldr reg1, [SP, #offset]
            GetEmitter()->emitIns_R_R_I(INS_ldr, EA_PTRSIZE, reg1, REG_SPBASE, spOffset);
            compiler->unwindSaveReg(reg1, spOffset);

            // generate add SP,SP,imm
            genStackPointerAdjustment(spDelta, tmpReg, pTmpRegIsZero, /* reportUnwindData */ true);
        }
    }
    else
    {
        // ldr reg1, [SP, #offset]
        GetEmitter()->emitIns_R_R_I(INS_ldr, EA_PTRSIZE, reg1, REG_SPBASE, spOffset);
        compiler->unwindSaveReg(reg1, spOffset);
    }
}

// Build a stack of register pairs for prolog/epilog save/restore for the given mask.
// The first register pair will contain the lowest register. Register pairs will combine neighbor
// registers in pairs. If it can't be done (for example if we have a hole or this is the last reg
// in a mask with odd number of regs) then the second element of that RegPair will be REG_NA.
//
// regsMask - a mask of registers for prolog/epilog generation;
// regStack - a regStack instance to build the stack in, used to save temp copyings.
//
void CodeGen::genBuildRegPairsStack(regMaskTP regsMask, ArrayStack<RegPair>* regStack)
{
    assert(regStack->Empty());

    unsigned regsCount = genCountBits(regsMask);

    while (regsMask != RBM_NONE)
    {
        regMaskTP reg1Mask = genFindLowestBit(regsMask);
        regNumber reg1     = genRegNumFromMask(reg1Mask);
        regsMask &= ~reg1Mask;
        regsCount -= 1;

        bool isPairSave = false;
        if (regsCount > 0)
        {
            regMaskTP reg2Mask = genFindLowestBit(regsMask);
            regNumber reg2     = genRegNumFromMask(reg2Mask);
            if (reg2 == REG_NEXT(reg1))
            {
                // The JIT doesn't allow saving pair (R28,FP), even though the
                // save_regp register pair unwind code specification allows it.
                // The JIT always saves (FP,LR) as a pair, and uses the save_fplr
                // unwind code. This only comes up in stress mode scenarios
                // where callee-saved registers are not allocated completely
                // from lowest-to-highest, without gaps.
                if (reg1 != REG_R28)
                {
                    // Both registers must have the same type to be saved as pair.
                    if (genIsValidFloatReg(reg1) == genIsValidFloatReg(reg2))
                    {
                        isPairSave = true;

                        regsMask &= ~reg2Mask;
                        regsCount -= 1;

                        regStack->Push(RegPair(reg1, reg2));
                    }
                }
            }
        }
        if (!isPairSave)
        {
            regStack->Push(RegPair(reg1));
        }
    }
    assert(regsCount == 0 && regsMask == RBM_NONE);

    genSetUseSaveNextPairs(regStack);
}

// Set useSaveNextPair for each RegPair on the stack which unwind info can be encoded as
// save_next code.
//
// We can use save_next for RegPair(N, N+1) only when we have sequence like (N-2, N-1), (N, N+1).
// In this case in the prolog save_next for (N, N+1) refers to save_pair(N-2, N-1);
// in the epilog the unwinder will search for the first save_pair (N-2, N-1)
// and then go back to the first save_next (N, N+1) to restore it first.
//
// regStack - a regStack instance to set useSaveNextPair.
//
void CodeGen::genSetUseSaveNextPairs(ArrayStack<RegPair>* regStack)
{
    for (unsigned i = 1; i < regStack->Size(); ++i)
    {
        RegPair& curr = regStack->BottomRef(i);
        RegPair  prev = regStack->Bottom(i - 1);

        if (prev.reg2 == REG_NA || curr.reg2 == REG_NA)
        {
            continue;
        }

        if (REG_NEXT(prev.reg2) != curr.reg1)
        {
            continue;
        }

        if (genIsValidFloatReg(prev.reg2) != genIsValidFloatReg(curr.reg1))
        {
            // It is possible to support changing of the last int pair with the first float pair,
            // but it is very rare case and it would require superfluous changes in the unwinder.
            continue;
        }
        curr.useSaveNextPair = true;
    }
}

// Get the stack slot size appropriate for the register type from the mask.
// Note: Because int and float register type sizes match we can call this
// function with a mask that includes both.
//
// regsMask - a mask of registers for prolog/epilog generation.
//
// Returns the stack slot size in bytes.
//
int CodeGen::genGetSlotSizeForRegsInMask(regMaskTP regsMask)
{
    assert((regsMask & (RBM_CALLEE_SAVED | RBM_FP | RBM_LR)) == regsMask); // Do not expect anything else.

    static_assert_no_msg(REGSIZE_BYTES == FPSAVE_REGSIZE_BYTES);
    return REGSIZE_BYTES;
}

// Saves the group of registers described by the mask.
//
// regsMask - a mask of registers for prolog generation;
// spDelta  - if non-zero, the amount to add to SP before the first register save (or together with it);
// spOffset - the offset from SP that is the beginning of the callee-saved register area;
//
void CodeGen::genSaveCalleeSavedRegisterGroup(regMaskTP regsMask, int spDelta, int spOffset)
{
    const int slotSize = genGetSlotSizeForRegsInMask(regsMask);

    ArrayStack<RegPair> regStack(compiler->getAllocator(CMK_Codegen));
    genBuildRegPairsStack(regsMask, &regStack);

    for (unsigned i = 0; i < regStack.Size(); ++i)
    {
        RegPair regPair = regStack.Bottom(i);
        if (regPair.reg2 != REG_NA)
        {
            // We can use a STP instruction.
            genPrologSaveRegPair(regPair.reg1, regPair.reg2, spOffset, spDelta, regPair.useSaveNextPair, REG_IP0,
                                 nullptr);

            spOffset += 2 * slotSize;
        }
        else
        {
            // No register pair; we use a STR instruction.
            genPrologSaveReg(regPair.reg1, spOffset, spDelta, REG_IP0, nullptr);
            spOffset += slotSize;
        }

        spDelta = 0; // We've now changed SP already, if necessary; don't do it again.
    }
}

// Save the callee-saved registers in 'regsToSaveMask' to the stack frame in the function or funclet prolog.
// Registers are saved in register number order from low addresses to high addresses.
// This means that integer registers are saved at lower addresses than floating-point/SIMD registers.
// However, when genSaveFpLrWithAllCalleeSavedRegisters is true, the integer registers are stored
// at higher addresses than floating-point/SIMD registers, that is, the relative order of these two
// classes is reversed. This is done to put the saved frame pointer very high in the frame, for simplicity.
//
// TODO: We could always put integer registers at the higher addresses, if desired, to remove this special
// case. It would cause many asm diffs when first implemented.
//
// If establishing frame pointer chaining, it must be done after saving the callee-saved registers.
//
// We can only use the instructions that are allowed by the unwind codes. The caller ensures that
// there is enough space on the frame to store these registers, and that the store instructions
// we need to use (STR or STP) are encodable with the stack-pointer immediate offsets we need to use.
//
// The caller can tell us to fold in a stack pointer adjustment, which we will do with the first instruction.
// Note that the stack pointer adjustment must be by a multiple of 16 to preserve the invariant that the
// stack pointer is always 16 byte aligned. If we are saving an odd number of callee-saved
// registers, though, we will have an empty alignment slot somewhere. It turns out we will put
// it below (at a lower address) the callee-saved registers, as that is currently how we
// do frame layout. This means that the first stack offset will be 8 and the stack pointer
// adjustment must be done by a SUB, and not folded in to a pre-indexed store.
//
// regsToSaveMask          - The mask of callee-saved registers to save. If empty, this function does nothing.
// lowestCalleeSavedOffset - The offset from SP that is the beginning of the callee-saved register area. Note that
//                           if non-zero spDelta, then this is the offset of the first save *after* that
//                           SP adjustment.
// spDelta                 - If non-zero, the amount to add to SP before the register saves (must be negative or
//                           zero).
// The save set can contain LR in which case LR is saved along with the other callee-saved registers.
// But currently Jit doesn't use frames without frame pointer on arm64.
//
void CodeGen::genSaveCalleeSavedRegistersHelp(regMaskTP regsToSaveMask, int lowestCalleeSavedOffset, int spDelta)
{
    assert(spDelta <= 0);
    assert(-spDelta <= STACK_PROBE_BOUNDARY_THRESHOLD_BYTES);

    unsigned regsToSaveCount = genCountBits(regsToSaveMask);
    if (regsToSaveCount == 0)
    {
        if (spDelta != 0)
        {
            // Currently this is the case for varargs only
            // whose size is MAX_REG_ARG * REGSIZE_BYTES = 64 bytes.
            genStackPointerAdjustment(spDelta, REG_NA, nullptr, /* reportUnwindData */ true);
        }
        return;
    }

    assert((spDelta % 16) == 0);

    // We also can save FP and LR, even though they are not in RBM_CALLEE_SAVED.
    assert(regsToSaveCount <= genCountBits(RBM_CALLEE_SAVED | RBM_FP | RBM_LR));

    // Save integer registers at higher addresses than floating-point registers.

    regMaskTP maskSaveRegsFloat = regsToSaveMask & RBM_ALLFLOAT;
    regMaskTP maskSaveRegsInt   = regsToSaveMask & ~maskSaveRegsFloat;

    if (maskSaveRegsFloat != RBM_NONE)
    {
        genSaveCalleeSavedRegisterGroup(maskSaveRegsFloat, spDelta, lowestCalleeSavedOffset);
        spDelta = 0;
        lowestCalleeSavedOffset += genCountBits(maskSaveRegsFloat) * FPSAVE_REGSIZE_BYTES;
    }

    if (maskSaveRegsInt != RBM_NONE)
    {
        genSaveCalleeSavedRegisterGroup(maskSaveRegsInt, spDelta, lowestCalleeSavedOffset);
        // No need to update spDelta, lowestCalleeSavedOffset since they're not used after this.
    }
}

// Restores the group of registers described by the mask.
//
// regsMask - a mask of registers for epilog generation;
// spDelta  - if non-zero, the amount to add to SP after the last register restore (or together with it);
// spOffset - the offset from SP that is the beginning of the callee-saved register area;
//
void CodeGen::genRestoreCalleeSavedRegisterGroup(regMaskTP regsMask, int spDelta, int spOffset)
{
    const int slotSize = genGetSlotSizeForRegsInMask(regsMask);

    ArrayStack<RegPair> regStack(compiler->getAllocator(CMK_Codegen));
    genBuildRegPairsStack(regsMask, &regStack);

    int stackDelta = 0;
    for (unsigned i = 0; i < regStack.Size(); ++i)
    {
        bool lastRestoreInTheGroup = (i == regStack.Size() - 1);
        bool updateStackDelta      = lastRestoreInTheGroup && (spDelta != 0);
        if (updateStackDelta)
        {
            // Update stack delta only if it is the last restore (the first save).
            assert(stackDelta == 0);
            stackDelta = spDelta;
        }

        RegPair regPair = regStack.Top(i);
        if (regPair.reg2 != REG_NA)
        {
            spOffset -= 2 * slotSize;

            genEpilogRestoreRegPair(regPair.reg1, regPair.reg2, spOffset, stackDelta, regPair.useSaveNextPair, REG_IP1,
                                    nullptr);
        }
        else
        {
            spOffset -= slotSize;
            genEpilogRestoreReg(regPair.reg1, spOffset, stackDelta, REG_IP1, nullptr);
        }
    }
}

// Restore the callee-saved registers in 'regsToRestoreMask' from the stack frame in the function
// or funclet epilog. This exactly reverses the actions of genSaveCalleeSavedRegistersHelp().
//
// regsToRestoreMask       - The mask of callee-saved registers to restore. If empty, this function does nothing.
// lowestCalleeSavedOffset - The offset from SP that is the beginning of the callee-saved register area.
// spDelta                 - If non-zero, the amount to add to SP after the register restores (must be positive or
//                           zero).
//
// Here's an example restore sequence:
//      ldp     x27, x28, [sp,#96]
//      ldp     x25, x26, [sp,#80]
//      ldp     x23, x24, [sp,#64]
//      ldp     x21, x22, [sp,#48]
//      ldp     x19, x20, [sp,#32]
//
// For the case of non-zero spDelta, we assume the base of the callee-save registers to restore is at SP, and
// the last restore adjusts SP by the specified amount. For example:
//      ldp     x27, x28, [sp,#64]
//      ldp     x25, x26, [sp,#48]
//      ldp     x23, x24, [sp,#32]
//      ldp     x21, x22, [sp,#16]
//      ldp     x19, x20, [sp], #80
//
// Note you call the unwind functions specifying the prolog operation that is being un-done. So, for example, when
// generating a post-indexed load, you call the unwind function for specifying the corresponding preindexed store.
//
void CodeGen::genRestoreCalleeSavedRegistersHelp(regMaskTP regsToRestoreMask, int lowestCalleeSavedOffset, int spDelta)
{
    assert(spDelta >= 0);
    unsigned regsToRestoreCount = genCountBits(regsToRestoreMask);
    if (regsToRestoreCount == 0)
    {
        if (spDelta != 0)
        {
            // Currently this is the case for varargs only
            // whose size is MAX_REG_ARG * REGSIZE_BYTES = 64 bytes.
            genStackPointerAdjustment(spDelta, REG_NA, nullptr, /* reportUnwindData */ true);
        }
        return;
    }

    assert((spDelta % 16) == 0);

    // We also can restore FP and LR, even though they are not in RBM_CALLEE_SAVED.
    assert(regsToRestoreCount <= genCountBits(RBM_CALLEE_SAVED | RBM_FP | RBM_LR));

    // Point past the end, to start. We predecrement to find the offset to load from.
    static_assert_no_msg(REGSIZE_BYTES == FPSAVE_REGSIZE_BYTES);
    int spOffset = lowestCalleeSavedOffset + regsToRestoreCount * REGSIZE_BYTES;

    // Save integer registers at higher addresses than floating-point registers.

    regMaskTP maskRestoreRegsFloat = regsToRestoreMask & RBM_ALLFLOAT;
    regMaskTP maskRestoreRegsInt   = regsToRestoreMask & ~maskRestoreRegsFloat;

    // Restore in the opposite order of saving.

    if (maskRestoreRegsInt != RBM_NONE)
    {
        int spIntDelta = (maskRestoreRegsFloat != RBM_NONE) ? 0 : spDelta; // should we delay the SP adjustment?
        genRestoreCalleeSavedRegisterGroup(maskRestoreRegsInt, spIntDelta, spOffset);
        spOffset -= genCountBits(maskRestoreRegsInt) * REGSIZE_BYTES;
    }

    if (maskRestoreRegsFloat != RBM_NONE)
    {
        // If there is any spDelta, it must be used here.
        genRestoreCalleeSavedRegisterGroup(maskRestoreRegsFloat, spDelta, spOffset);
        // No need to update spOffset since it's not used after this.
    }
}

// clang-format off
//
//  Generates code for an EH funclet prolog.
//
//  Funclets have the following incoming arguments:
//
//      catch:          x0 = the exception object that was caught (see GT_CATCH_ARG)
//      filter:         x0 = the exception object to filter (see GT_CATCH_ARG), x1 = CallerSP of the containing function
//      finally/fault:  none
//
//  Funclets set the following registers on exit:
//
//      catch:          x0 = the address at which execution should resume (see BBJ_EHCATCHRET)
//      filter:         x0 = non-zero if the handler should handle the exception, zero otherwise (see GT_RETFILT)
//      finally/fault:  none
//
//  The ARM64 funclet prolog sequence is one of the following (Note: #framesz is total funclet frame size,
//  including everything; #outsz is outgoing argument space. #framesz must be a multiple of 16):
//
//  Frame type 1:
//     For #outsz == 0 and #framesz <= 512:
//     stp fp,lr,[sp,-#framesz]!    ; establish the frame (predecrement by #framesz), save FP/LR
//     stp x19,x20,[sp,#xxx]        ; save callee-saved registers, as necessary
//
//  The funclet frame is thus:
//
//      |                       |
//      |-----------------------|
//      |  incoming arguments   |
//      +=======================+ <---- Caller's SP
//      |  Varargs regs space   | // Only for varargs main functions; 64 bytes
//      |-----------------------|
//      |Callee saved registers | // multiple of 8 bytes
//      |-----------------------|
//      |        PSP slot       | // 8 bytes (omitted in CoreRT ABI)
//      |-----------------------|
//      ~  alignment padding    ~ // To make the whole frame 16 byte aligned.
//      |-----------------------|
//      |      Saved FP, LR     | // 16 bytes
//      |-----------------------| <---- Ambient SP
//      |       |               |
//      ~       | Stack grows   ~
//      |       | downward      |
//              V
//
//  Frame type 2:
//     For #outsz != 0 and #framesz <= 512:
//     sub sp,sp,#framesz           ; establish the frame
//     stp fp,lr,[sp,#outsz]        ; save FP/LR.
//     stp x19,x20,[sp,#xxx]        ; save callee-saved registers, as necessary
//
//  The funclet frame is thus:
//
//      |                       |
//      |-----------------------|
//      |  incoming arguments   |
//      +=======================+ <---- Caller's SP
//      |  Varargs regs space   | // Only for varargs main functions; 64 bytes
//      |-----------------------|
//      |Callee saved registers | // multiple of 8 bytes
//      |-----------------------|
//      |        PSP slot       | // 8 bytes (omitted in CoreRT ABI)
//      |-----------------------|
//      ~  alignment padding    ~ // To make the whole frame 16 byte aligned.
//      |-----------------------|
//      |      Saved FP, LR     | // 16 bytes
//      |-----------------------|
//      |   Outgoing arg space  | // multiple of 8 bytes
//      |-----------------------| <---- Ambient SP
//      |       |               |
//      ~       | Stack grows   ~
//      |       | downward      |
//              V
//
//  Frame type 3:
//     For #framesz > 512:
//     stp fp,lr,[sp,- (#framesz - #outsz)]!    ; establish the frame, save FP/LR
//                                              ; note that it is guaranteed here that (#framesz - #outsz) <= 240
//     stp x19,x20,[sp,#xxx]                    ; save callee-saved registers, as necessary
//     sub sp,sp,#outsz                         ; create space for outgoing argument space
//
//  The funclet frame is thus:
//
//      |                       |
//      |-----------------------|
//      |  incoming arguments   |
//      +=======================+ <---- Caller's SP
//      |  Varargs regs space   | // Only for varargs main functions; 64 bytes
//      |-----------------------|
//      |Callee saved registers | // multiple of 8 bytes
//      |-----------------------|
//      |        PSP slot       | // 8 bytes (omitted in CoreRT ABI)
//      |-----------------------|
//      ~  alignment padding    ~ // To make the first SP subtraction 16 byte aligned
//      |-----------------------|
//      |      Saved FP, LR     | // 16 bytes
//      |-----------------------|
//      ~  alignment padding    ~ // To make the whole frame 16 byte aligned (specifically, to 16-byte align the outgoing argument space).
//      |-----------------------|
//      |   Outgoing arg space  | // multiple of 8 bytes
//      |-----------------------| <---- Ambient SP
//      |       |               |
//      ~       | Stack grows   ~
//      |       | downward      |
//              V
//
// Both #1 and #2 only change SP once. That means that there will be a maximum of one alignment slot needed. For the general case, #3,
// it is possible that we will need to add alignment to both changes to SP, leading to 16 bytes of alignment. Remember that the stack
// pointer needs to be 16 byte aligned at all times. The size of the PSP slot plus callee-saved registers space is a maximum of 240 bytes:
//
//     FP,LR registers
//     10 int callee-saved register x19-x28
//     8 float callee-saved registers v8-v15
//     8 saved integer argument registers x0-x7, if varargs function
//     1 PSP slot
//     1 alignment slot
//     == 30 slots * 8 bytes = 240 bytes.
//
// The outgoing argument size, however, can be very large, if we call a function that takes a large number of
// arguments (note that we currently use the same outgoing argument space size in the funclet as for the main
// function, even if the funclet doesn't have any calls, or has a much smaller, or larger, maximum number of
// outgoing arguments for any call). In that case, we need to 16-byte align the initial change to SP, before
// saving off the callee-saved registers and establishing the PSPsym, so we can use the limited immediate offset
// encodings we have available, before doing another 16-byte aligned SP adjustment to create the outgoing argument
// space. Both changes to SP might need to add alignment padding.
//
// In addition to the above "standard" frames, we also need to support a frame where the saved FP/LR are at the
// highest addresses. This is to match the frame layout (specifically, callee-saved registers including FP/LR
// and the PSPSym) that is used in the main function when a GS cookie is required due to the use of localloc.
// (Note that localloc cannot be used in a funclet.) In these variants, not only has the position of FP/LR
// changed, but where the alignment padding is placed has also changed.
//
//  Frame type 4 (variant of frame types 1 and 2):
//     For #framesz <= 512:
//     sub sp,sp,#framesz           ; establish the frame
//     stp x19,x20,[sp,#xxx]        ; save callee-saved registers, as necessary
//     stp fp,lr,[sp,#yyy]          ; save FP/LR.
//     ; write PSPSym
//
//  The "#framesz <= 512" condition ensures that after we've established the frame, we can use "stp" with its
//  maximum allowed offset (504) to save the callee-saved register at the highest address.
//
//  We use "sub" instead of folding it into the next instruction as a predecrement, as we need to write PSPSym
//  at the bottom of the stack, and there might also be an alignment padding slot.
//
//  The funclet frame is thus:
//
//      |                       |
//      |-----------------------|
//      |  incoming arguments   |
//      +=======================+ <---- Caller's SP
//      |  Varargs regs space   | // Only for varargs main functions; 64 bytes
//      |-----------------------|
//      |      Saved LR         | // 8 bytes
//      |-----------------------|
//      |      Saved FP         | // 8 bytes
//      |-----------------------|
//      |Callee saved registers | // multiple of 8 bytes
//      |-----------------------|
//      |        PSP slot       | // 8 bytes (omitted in CoreRT ABI)
//      |-----------------------|
//      ~  alignment padding    ~ // To make the whole frame 16 byte aligned.
//      |-----------------------|
//      |   Outgoing arg space  | // multiple of 8 bytes (optional; if #outsz > 0)
//      |-----------------------| <---- Ambient SP
//      |       |               |
//      ~       | Stack grows   ~
//      |       | downward      |
//              V
//
//  Frame type 5 (variant of frame type 3):
//     For #framesz > 512:
//     sub sp,sp,(#framesz - #outsz) ; establish part of the frame. Note that it is guaranteed here that (#framesz - #outsz) <= 240
//     stp x19,x20,[sp,#xxx]        ; save callee-saved registers, as necessary
//     stp fp,lr,[sp,#yyy]          ; save FP/LR.
//     sub sp,sp,#outsz             ; create space for outgoing argument space
//     ; write PSPSym
//
//  For large frames with "#framesz > 512", we must do one SP adjustment first, after which we can save callee-saved
//  registers with up to the maximum "stp" offset of 504. Then, we can establish the rest of the frame (namely, the
//  space for the outgoing argument space).
//
//  The funclet frame is thus:
//
//      |                       |
//      |-----------------------|
//      |  incoming arguments   |
//      +=======================+ <---- Caller's SP
//      |  Varargs regs space   | // Only for varargs main functions; 64 bytes
//      |-----------------------|
//      |      Saved LR         | // 8 bytes
//      |-----------------------|
//      |      Saved FP         | // 8 bytes
//      |-----------------------|
//      |Callee saved registers | // multiple of 8 bytes
//      |-----------------------|
//      |        PSP slot       | // 8 bytes (omitted in CoreRT ABI)
//      |-----------------------|
//      ~  alignment padding    ~ // To make the first SP subtraction 16 byte aligned
//      |-----------------------|
//      ~  alignment padding    ~ // To make the whole frame 16 byte aligned (specifically, to 16-byte align the outgoing argument space).
//      |-----------------------|
//      |   Outgoing arg space  | // multiple of 8 bytes
//      |-----------------------| <---- Ambient SP
//      |       |               |
//      ~       | Stack grows   ~
//      |       | downward      |
//              V
//
// Note that in this case we might have 16 bytes of alignment that is adjacent. This is because we are doing 2 SP
// subtractions, and each one must be aligned up to 16 bytes.
//
// Note that in all cases, the PSPSym is in exactly the same position with respect to Caller-SP, and that location is the same relative to Caller-SP
// as in the main function.
//
// Funclets do not have varargs arguments. However, because the PSPSym must exist at the same offset from Caller-SP as in the main function, we
// must add buffer space for the saved varargs argument registers here, if the main function did the same.
//
//     ; After this header, fill the PSP slot, for use by the VM (it gets reported with the GC info), or by code generation of nested filters.
//     ; This is not part of the "OS prolog"; it has no associated unwind data, and is not reversed in the funclet epilog.
//
//     if (this is a filter funclet)
//     {
//          // x1 on entry to a filter funclet is CallerSP of the containing function:
//          // either the main function, or the funclet for a handler that this filter is dynamically nested within.
//          // Note that a filter can be dynamically nested within a funclet even if it is not statically within
//          // a funclet. Consider:
//          //
//          //    try {
//          //        try {
//          //            throw new Exception();
//          //        } catch(Exception) {
//          //            throw new Exception();     // The exception thrown here ...
//          //        }
//          //    } filter {                         // ... will be processed here, while the "catch" funclet frame is still on the stack
//          //    } filter-handler {
//          //    }
//          //
//          // Because of this, we need a PSP in the main function anytime a filter funclet doesn't know whether the enclosing frame will
//          // be a funclet or main function. We won't know any time there is a filter protecting nested EH. To simplify, we just always
//          // create a main function PSP for any function with a filter.
//
//          ldr x1, [x1, #CallerSP_to_PSP_slot_delta]  ; Load the CallerSP of the main function (stored in the PSP of the dynamically containing funclet or function)
//          str x1, [sp, #SP_to_PSP_slot_delta]        ; store the PSP
//          add fp, x1, #Function_CallerSP_to_FP_delta ; re-establish the frame pointer
//     }
//     else
//     {
//          // This is NOT a filter funclet. The VM re-establishes the frame pointer on entry.
//          // TODO-ARM64-CQ: if VM set x1 to CallerSP on entry, like for filters, we could save an instruction.
//
//          add x3, fp, #Function_FP_to_CallerSP_delta  ; compute the CallerSP, given the frame pointer. x3 is scratch.
//          str x3, [sp, #SP_to_PSP_slot_delta]         ; store the PSP
//     }
//
//  An example epilog sequence is then:
//
//     add sp,sp,#outsz             ; if any outgoing argument space
//     ...                          ; restore callee-saved registers
//     ldp x19,x20,[sp,#xxx]
//     ldp fp,lr,[sp],#framesz
//     ret lr
//
// clang-format on
void CodeGen::genFuncletProlog(BasicBlock* block)
{
    JITDUMP("*************** In genFuncletProlog()\n");

    assert(block != NULL);
    assert(block->bbFlags & BBF_FUNCLET_BEG);

    ScopedSetVariable<bool> _setGeneratingProlog(&generatingProlog, true);

    compiler->unwindBegProlog();

    regMaskTP maskSaveRegsFloat = genFuncletInfo.fiSaveRegs & RBM_ALLFLOAT;
    regMaskTP maskSaveRegsInt   = genFuncletInfo.fiSaveRegs & ~maskSaveRegsFloat;

    // Funclets must always save LR and FP, since when we have funclets we must have an FP frame.
    assert((maskSaveRegsInt & RBM_LR) != 0);
    assert((maskSaveRegsInt & RBM_FP) != 0);

    bool isFilter = (block->bbCatchTyp == BBCT_FILTER);

    regMaskTP maskArgRegsLiveIn;
    if (isFilter)
    {
        maskArgRegsLiveIn = RBM_R0 | RBM_R1;
    }
    else if ((block->bbCatchTyp == BBCT_FINALLY) || (block->bbCatchTyp == BBCT_FAULT))
    {
        maskArgRegsLiveIn = RBM_NONE;
    }
    else
    {
        maskArgRegsLiveIn = RBM_R0;
    }

    if (genFuncletInfo.fiFrameType == 1)
    {
        GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_PTRSIZE, REG_FP, REG_LR, REG_SPBASE, genFuncletInfo.fiSpDelta1,
                                      INS_OPTS_PRE_INDEX);
        compiler->unwindSaveRegPairPreindexed(REG_FP, REG_LR, genFuncletInfo.fiSpDelta1);

        maskSaveRegsInt &= ~(RBM_LR | RBM_FP); // We've saved these now

        assert(genFuncletInfo.fiSpDelta2 == 0);
        assert(genFuncletInfo.fiSP_to_FPLR_save_delta == 0);
    }
    else if (genFuncletInfo.fiFrameType == 2)
    {
        // fiFrameType==2 constraints:
        assert(genFuncletInfo.fiSpDelta1 < 0);
        assert(genFuncletInfo.fiSpDelta1 >= -512);

        // generate sub SP,SP,imm
        genStackPointerAdjustment(genFuncletInfo.fiSpDelta1, REG_NA, nullptr, /* reportUnwindData */ true);

        assert(genFuncletInfo.fiSpDelta2 == 0);

        GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_PTRSIZE, REG_FP, REG_LR, REG_SPBASE,
                                      genFuncletInfo.fiSP_to_FPLR_save_delta);
        compiler->unwindSaveRegPair(REG_FP, REG_LR, genFuncletInfo.fiSP_to_FPLR_save_delta);

        maskSaveRegsInt &= ~(RBM_LR | RBM_FP); // We've saved these now
    }
    else if (genFuncletInfo.fiFrameType == 3)
    {
        GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_PTRSIZE, REG_FP, REG_LR, REG_SPBASE, genFuncletInfo.fiSpDelta1,
                                      INS_OPTS_PRE_INDEX);
        compiler->unwindSaveRegPairPreindexed(REG_FP, REG_LR, genFuncletInfo.fiSpDelta1);

        maskSaveRegsInt &= ~(RBM_LR | RBM_FP); // We've saved these now
    }
    else if (genFuncletInfo.fiFrameType == 4)
    {
        // fiFrameType==4 constraints:
        assert(genFuncletInfo.fiSpDelta1 < 0);
        assert(genFuncletInfo.fiSpDelta1 >= -512);

        // generate sub SP,SP,imm
        genStackPointerAdjustment(genFuncletInfo.fiSpDelta1, REG_NA, nullptr, /* reportUnwindData */ true);

        assert(genFuncletInfo.fiSpDelta2 == 0);
    }
    else
    {
        assert(genFuncletInfo.fiFrameType == 5);

        // Nothing to do here; the first SP adjustment will be done by saving the callee-saved registers.
    }

    int lowestCalleeSavedOffset = genFuncletInfo.fiSP_to_CalleeSave_delta +
                                  genFuncletInfo.fiSpDelta2; // We haven't done the second adjustment of SP yet (if any)
    genSaveCalleeSavedRegistersHelp(maskSaveRegsInt | maskSaveRegsFloat, lowestCalleeSavedOffset, 0);

    if ((genFuncletInfo.fiFrameType == 3) || (genFuncletInfo.fiFrameType == 5))
    {
        // Note that genFuncletInfo.fiSpDelta2 is always a negative value
        assert(genFuncletInfo.fiSpDelta2 < 0);

        // generate sub SP,SP,imm
        genStackPointerAdjustment(genFuncletInfo.fiSpDelta2, REG_R2, nullptr, /* reportUnwindData */ true);
    }

    // This is the end of the OS-reported prolog for purposes of unwinding
    compiler->unwindEndProlog();

    // If there is no PSPSym (CoreRT ABI), we are done. Otherwise, we need to set up the PSPSym in the functlet frame.
    if (compiler->lvaPSPSym != BAD_VAR_NUM)
    {
        if (isFilter)
        {
            // This is the first block of a filter
            // Note that register x1 = CallerSP of the containing function
            // X1 is overwritten by the first Load (new callerSP)
            // X2 is scratch when we have a large constant offset

            // Load the CallerSP of the main function (stored in the PSP of the dynamically containing funclet or
            // function)
            genInstrWithConstant(INS_ldr, EA_PTRSIZE, REG_R1, REG_R1, genFuncletInfo.fiCallerSP_to_PSP_slot_delta,
                                 REG_R2, false);

            // Store the PSP value (aka CallerSP)
            genInstrWithConstant(INS_str, EA_PTRSIZE, REG_R1, REG_SPBASE, genFuncletInfo.fiSP_to_PSP_slot_delta, REG_R2,
                                 false);

            // re-establish the frame pointer
            genInstrWithConstant(INS_add, EA_PTRSIZE, REG_FPBASE, REG_R1,
                                 genFuncletInfo.fiFunction_CallerSP_to_FP_delta, REG_R2, false);
        }
        else // This is a non-filter funclet
        {
            // X3 is scratch, X2 can also become scratch

            // compute the CallerSP, given the frame pointer. x3 is scratch.
            genInstrWithConstant(INS_add, EA_PTRSIZE, REG_R3, REG_FPBASE,
                                 -genFuncletInfo.fiFunction_CallerSP_to_FP_delta, REG_R2, false);

            genInstrWithConstant(INS_str, EA_PTRSIZE, REG_R3, REG_SPBASE, genFuncletInfo.fiSP_to_PSP_slot_delta, REG_R2,
                                 false);
        }
    }
}

void CodeGen::genFuncletEpilog()
{
    JITDUMP("*************** In genFuncletEpilog()\n");

    ScopedSetVariable<bool> _setGeneratingEpilog(&generatingEpilog, true);

    bool unwindStarted = false;

    if (!unwindStarted)
    {
        // We can delay this until we know we'll generate an unwindable instruction, if necessary.
        compiler->unwindBegEpilog();
        unwindStarted = true;
    }

    regMaskTP maskRestoreRegsFloat = genFuncletInfo.fiSaveRegs & RBM_ALLFLOAT;
    regMaskTP maskRestoreRegsInt   = genFuncletInfo.fiSaveRegs & ~maskRestoreRegsFloat;

    // Funclets must always save LR and FP, since when we have funclets we must have an FP frame.
    assert((maskRestoreRegsInt & RBM_LR) != 0);
    assert((maskRestoreRegsInt & RBM_FP) != 0);

    if ((genFuncletInfo.fiFrameType == 3) || (genFuncletInfo.fiFrameType == 5))
    {
        // Note that genFuncletInfo.fiSpDelta2 is always a negative value
        assert(genFuncletInfo.fiSpDelta2 < 0);

        // generate add SP,SP,imm
        genStackPointerAdjustment(-genFuncletInfo.fiSpDelta2, REG_R2, nullptr, /* reportUnwindData */ true);
    }

    regMaskTP regsToRestoreMask = maskRestoreRegsInt | maskRestoreRegsFloat;
    if ((genFuncletInfo.fiFrameType == 1) || (genFuncletInfo.fiFrameType == 2) || (genFuncletInfo.fiFrameType == 3))
    {
        regsToRestoreMask &= ~(RBM_LR | RBM_FP); // We restore FP/LR at the end
    }
    int lowestCalleeSavedOffset = genFuncletInfo.fiSP_to_CalleeSave_delta + genFuncletInfo.fiSpDelta2;
    genRestoreCalleeSavedRegistersHelp(regsToRestoreMask, lowestCalleeSavedOffset, 0);

    if (genFuncletInfo.fiFrameType == 1)
    {
        GetEmitter()->emitIns_R_R_R_I(INS_ldp, EA_PTRSIZE, REG_FP, REG_LR, REG_SPBASE, -genFuncletInfo.fiSpDelta1,
                                      INS_OPTS_POST_INDEX);
        compiler->unwindSaveRegPairPreindexed(REG_FP, REG_LR, genFuncletInfo.fiSpDelta1);

        assert(genFuncletInfo.fiSpDelta2 == 0);
        assert(genFuncletInfo.fiSP_to_FPLR_save_delta == 0);
    }
    else if (genFuncletInfo.fiFrameType == 2)
    {
        GetEmitter()->emitIns_R_R_R_I(INS_ldp, EA_PTRSIZE, REG_FP, REG_LR, REG_SPBASE,
                                      genFuncletInfo.fiSP_to_FPLR_save_delta);
        compiler->unwindSaveRegPair(REG_FP, REG_LR, genFuncletInfo.fiSP_to_FPLR_save_delta);

        // fiFrameType==2 constraints:
        assert(genFuncletInfo.fiSpDelta1 < 0);
        assert(genFuncletInfo.fiSpDelta1 >= -512);

        // generate add SP,SP,imm
        genStackPointerAdjustment(-genFuncletInfo.fiSpDelta1, REG_NA, nullptr, /* reportUnwindData */ true);

        assert(genFuncletInfo.fiSpDelta2 == 0);
    }
    else if (genFuncletInfo.fiFrameType == 3)
    {
        GetEmitter()->emitIns_R_R_R_I(INS_ldp, EA_PTRSIZE, REG_FP, REG_LR, REG_SPBASE, -genFuncletInfo.fiSpDelta1,
                                      INS_OPTS_POST_INDEX);
        compiler->unwindSaveRegPairPreindexed(REG_FP, REG_LR, genFuncletInfo.fiSpDelta1);
    }
    else if (genFuncletInfo.fiFrameType == 4)
    {
        // fiFrameType==4 constraints:
        assert(genFuncletInfo.fiSpDelta1 < 0);
        assert(genFuncletInfo.fiSpDelta1 >= -512);

        // generate add SP,SP,imm
        genStackPointerAdjustment(-genFuncletInfo.fiSpDelta1, REG_NA, nullptr, /* reportUnwindData */ true);

        assert(genFuncletInfo.fiSpDelta2 == 0);
    }
    else
    {
        assert(genFuncletInfo.fiFrameType == 5);
        // Same work as fiFrameType==4, but different asserts.

        assert(genFuncletInfo.fiSpDelta1 < 0);
        assert(genFuncletInfo.fiSpDelta1 >= -240);

        // generate add SP,SP,imm
        genStackPointerAdjustment(-genFuncletInfo.fiSpDelta1, REG_NA, nullptr, /* reportUnwindData */ true);
    }

    inst_RV(INS_ret, REG_LR, TYP_I_IMPL);
    compiler->unwindReturn(REG_LR);

    compiler->unwindEndEpilog();
}

void CodeGen::genCaptureFuncletPrologEpilogInfo()
{
    if (!compiler->ehAnyFunclets())
        return;

    assert(isFramePointerUsed());

    // The frame size and offsets must be finalized
    assert(compiler->lvaDoneFrameLayout == Compiler::FINAL_FRAME_LAYOUT);

    genFuncletInfo.fiFunction_CallerSP_to_FP_delta = genCallerSPtoFPdelta();

    regMaskTP rsMaskSaveRegs = calleeSavedModifiedRegs;

    if (isFramePointerUsed())
    {
        rsMaskSaveRegs |= RBM_FP;
    }

    rsMaskSaveRegs |= RBM_LR;

    unsigned PSPSize = (compiler->lvaPSPSym != BAD_VAR_NUM) ? REGSIZE_BYTES : 0;

    unsigned saveRegsCount       = genCountBits(rsMaskSaveRegs);
    unsigned saveRegsPlusPSPSize = saveRegsCount * REGSIZE_BYTES + PSPSize;
    if (compiler->info.compIsVarArgs)
    {
        // For varargs we always save all of the integer register arguments
        // so that they are contiguous with the incoming stack arguments.
        saveRegsPlusPSPSize += MAX_REG_ARG * REGSIZE_BYTES;
    }
    unsigned saveRegsPlusPSPSizeAligned = roundUp(saveRegsPlusPSPSize, STACK_ALIGN);

    assert(outgoingArgSpaceSize % REGSIZE_BYTES == 0);
    unsigned outgoingArgSpaceAligned = roundUp(outgoingArgSpaceSize, STACK_ALIGN);

    unsigned maxFuncletFrameSizeAligned = saveRegsPlusPSPSizeAligned + outgoingArgSpaceAligned;
    assert((maxFuncletFrameSizeAligned % STACK_ALIGN) == 0);

    int SP_to_FPLR_save_delta;
    int SP_to_PSP_slot_delta;
    int CallerSP_to_PSP_slot_delta;

    unsigned funcletFrameSize        = saveRegsPlusPSPSize + outgoingArgSpaceSize;
    unsigned funcletFrameSizeAligned = roundUp(funcletFrameSize, STACK_ALIGN);
    assert(funcletFrameSizeAligned <= maxFuncletFrameSizeAligned);

    unsigned funcletFrameAlignmentPad = funcletFrameSizeAligned - funcletFrameSize;
    assert((funcletFrameAlignmentPad == 0) || (funcletFrameAlignmentPad == REGSIZE_BYTES));

    if (maxFuncletFrameSizeAligned <= 512)
    {
        if (genSaveFpLrWithAllCalleeSavedRegisters)
        {
            SP_to_FPLR_save_delta = funcletFrameSizeAligned - (2 /* FP, LR */ * REGSIZE_BYTES);
            if (compiler->info.compIsVarArgs)
            {
                SP_to_FPLR_save_delta -= MAX_REG_ARG * REGSIZE_BYTES;
            }

            SP_to_PSP_slot_delta       = outgoingArgSpaceSize + funcletFrameAlignmentPad;
            CallerSP_to_PSP_slot_delta = -(int)saveRegsPlusPSPSize;

            genFuncletInfo.fiFrameType = 4;
        }
        else
        {
            SP_to_FPLR_save_delta = outgoingArgSpaceSize;
            SP_to_PSP_slot_delta  = SP_to_FPLR_save_delta + 2 /* FP, LR */ * REGSIZE_BYTES + funcletFrameAlignmentPad;
            CallerSP_to_PSP_slot_delta = -(int)(saveRegsPlusPSPSize - 2 /* FP, LR */ * REGSIZE_BYTES);

            if (outgoingArgSpaceSize == 0)
            {
                genFuncletInfo.fiFrameType = 1;
            }
            else
            {
                genFuncletInfo.fiFrameType = 2;
            }
        }

        genFuncletInfo.fiSpDelta1 = -(int)funcletFrameSizeAligned;
        genFuncletInfo.fiSpDelta2 = 0;

        assert(genFuncletInfo.fiSpDelta1 + genFuncletInfo.fiSpDelta2 == -(int)funcletFrameSizeAligned);
    }
    else
    {
        unsigned saveRegsPlusPSPAlignmentPad = saveRegsPlusPSPSizeAligned - saveRegsPlusPSPSize;
        assert((saveRegsPlusPSPAlignmentPad == 0) || (saveRegsPlusPSPAlignmentPad == REGSIZE_BYTES));

        if (genSaveFpLrWithAllCalleeSavedRegisters)
        {
            SP_to_FPLR_save_delta = funcletFrameSizeAligned - (2 /* FP, LR */ * REGSIZE_BYTES);
            if (compiler->info.compIsVarArgs)
            {
                SP_to_FPLR_save_delta -= MAX_REG_ARG * REGSIZE_BYTES;
            }

            SP_to_PSP_slot_delta       = outgoingArgSpaceSize + funcletFrameAlignmentPad + saveRegsPlusPSPAlignmentPad;
            CallerSP_to_PSP_slot_delta = -(int)saveRegsPlusPSPSize;

            genFuncletInfo.fiFrameType = 5;
        }
        else
        {
            SP_to_FPLR_save_delta = outgoingArgSpaceAligned;
            SP_to_PSP_slot_delta = SP_to_FPLR_save_delta + 2 /* FP, LR */ * REGSIZE_BYTES + saveRegsPlusPSPAlignmentPad;
            CallerSP_to_PSP_slot_delta =
                -(int)(saveRegsPlusPSPSizeAligned - 2 /* FP, LR */ * REGSIZE_BYTES - saveRegsPlusPSPAlignmentPad);

            genFuncletInfo.fiFrameType = 3;
        }

        genFuncletInfo.fiSpDelta1 = -(int)saveRegsPlusPSPSizeAligned;
        genFuncletInfo.fiSpDelta2 = -(int)outgoingArgSpaceAligned;

        assert(genFuncletInfo.fiSpDelta1 + genFuncletInfo.fiSpDelta2 == -(int)maxFuncletFrameSizeAligned);
    }

    /* Now save it for future use */

    genFuncletInfo.fiSaveRegs                   = rsMaskSaveRegs;
    genFuncletInfo.fiSP_to_FPLR_save_delta      = SP_to_FPLR_save_delta;
    genFuncletInfo.fiSP_to_PSP_slot_delta       = SP_to_PSP_slot_delta;
    genFuncletInfo.fiSP_to_CalleeSave_delta     = SP_to_PSP_slot_delta + PSPSize;
    genFuncletInfo.fiCallerSP_to_PSP_slot_delta = CallerSP_to_PSP_slot_delta;

#ifdef DEBUG
    if (verbose)
    {
        printf("\n");
        printf("Funclet prolog / epilog info\n");
        printf("                        Save regs: ");
        dspRegMask(genFuncletInfo.fiSaveRegs);
        printf("\n");
        printf("    Function CallerSP-to-FP delta: %d\n", genFuncletInfo.fiFunction_CallerSP_to_FP_delta);
        printf("  SP to FP/LR save location delta: %d\n", genFuncletInfo.fiSP_to_FPLR_save_delta);
        printf("             SP to PSP slot delta: %d\n", genFuncletInfo.fiSP_to_PSP_slot_delta);
        printf("    SP to callee-saved area delta: %d\n", genFuncletInfo.fiSP_to_CalleeSave_delta);
        printf("      Caller SP to PSP slot delta: %d\n", genFuncletInfo.fiCallerSP_to_PSP_slot_delta);
        printf("                       Frame type: %d\n", genFuncletInfo.fiFrameType);
        printf("                       SP delta 1: %d\n", genFuncletInfo.fiSpDelta1);
        printf("                       SP delta 2: %d\n", genFuncletInfo.fiSpDelta2);

        if (compiler->lvaPSPSym != BAD_VAR_NUM)
        {
            if (CallerSP_to_PSP_slot_delta !=
                compiler->lvaGetCallerSPRelativeOffset(compiler->lvaPSPSym)) // for debugging
            {
                printf("lvaGetCallerSPRelativeOffset(lvaPSPSym): %d\n",
                       compiler->lvaGetCallerSPRelativeOffset(compiler->lvaPSPSym));
            }
        }
    }

    assert(genFuncletInfo.fiSP_to_FPLR_save_delta >= 0);
    assert(genFuncletInfo.fiSP_to_PSP_slot_delta >= 0);
    assert(genFuncletInfo.fiSP_to_CalleeSave_delta >= 0);
    assert(genFuncletInfo.fiCallerSP_to_PSP_slot_delta <= 0);

    if (compiler->lvaPSPSym != BAD_VAR_NUM)
    {
        assert(genFuncletInfo.fiCallerSP_to_PSP_slot_delta ==
               compiler->lvaGetCallerSPRelativeOffset(compiler->lvaPSPSym)); // same offset used in main function and
                                                                             // funclet!
    }
#endif // DEBUG
}

BasicBlock* CodeGen::genCallFinally(BasicBlock* block)
{
    // Generate a call to the finally, like this:
    //      mov         x0,qword ptr [fp + 10H] / sp    // Load x0 with PSPSym, or sp if PSPSym is not used
    //      bl          finally-funclet
    //      b           finally-return                  // Only for non-retless finally calls
    // The 'b' can be a NOP if we're going to the next block.

    if (compiler->lvaPSPSym != BAD_VAR_NUM)
    {
        GetEmitter()->emitIns_R_S(INS_ldr, EA_PTRSIZE, REG_R0, compiler->lvaPSPSym, 0);
    }
    else
    {
        GetEmitter()->emitIns_Mov(INS_mov, EA_PTRSIZE, REG_R0, REG_SPBASE, /* canSkip */ false);
    }
    GetEmitter()->emitIns_J(INS_bl_local, block->bbJumpDest);

    if (block->bbFlags & BBF_RETLESS_CALL)
    {
        // We have a retless call, and the last instruction generated was a call.
        // If the next block is in a different EH region (or is the end of the code
        // block), then we need to generate a breakpoint here (since it will never
        // get executed) to get proper unwind behavior.

        if ((block->bbNext == nullptr) || !BasicBlock::sameEHRegion(block, block->bbNext))
        {
            instGen(INS_BREAKPOINT); // This should never get executed
        }
    }
    else
    {
        // Because of the way the flowgraph is connected, the liveness info for this one instruction
        // after the call is not (can not be) correct in cases where a variable has a last use in the
        // handler.  So turn off GC reporting for this single instruction.
        GetEmitter()->emitDisableGC();

        // Now go to where the finally funclet needs to return to.
        if (block->bbNext->bbJumpDest == block->bbNext->bbNext)
        {
            // Fall-through.
            // TODO-ARM64-CQ: Can we get rid of this instruction, and just have the call return directly
            // to the next instruction? This would depend on stack walking from within the finally
            // handler working without this instruction being in this special EH region.
            instGen(INS_nop);
        }
        else
        {
            inst_JMP(EJ_jmp, block->bbNext->bbJumpDest);
        }

        GetEmitter()->emitEnableGC();
    }

    // The BBJ_ALWAYS is used because the BBJ_CALLFINALLY can't point to the
    // jump target using bbJumpDest - that is already used to point
    // to the finally block. So just skip past the BBJ_ALWAYS unless the
    // block is RETLESS.
    if (!(block->bbFlags & BBF_RETLESS_CALL))
    {
        assert(block->isBBCallAlwaysPair());
        block = block->bbNext;
    }
    return block;
}

void CodeGen::genEHCatchRet(BasicBlock* block)
{
    // For long address (default): `adrp + add` will be emitted.
    // For short address (proven later): `adr` will be emitted.
    GetEmitter()->emitIns_R_L(INS_adr, EA_PTRSIZE, block->bbJumpDest, REG_INTRET);
}

void CodeGen::instGen_Set_Reg_To_Zero(emitAttr size, regNumber reg)
{
    GetEmitter()->emitIns_R_I(INS_mov, size, reg, 0);
}

void CodeGen::instGen_Set_Reg_To_Imm(emitAttr  size,
                                     regNumber reg,
                                     ssize_t imm DEBUGARG(size_t targetHandle) DEBUGARG(GenTreeFlags gtFlags))
{
    // reg cannot be a FP register
    assert(!genIsValidFloatReg(reg));
    if (!compiler->opts.compReloc)
    {
        size = EA_SIZE(size); // Strip any Reloc flags from size if we aren't doing relocs
    }

    if (EA_IS_RELOC(size))
    {
        // This emits a pair of adrp/add (two instructions) with fix-ups.
        GetEmitter()->emitIns_R_AI(INS_adrp, size, reg, imm DEBUGARG(targetHandle) DEBUGARG(gtFlags));
    }
    else if ((imm == 0) || emitter::emitIns_valid_imm_for_mov(imm, size))
    {
        GetEmitter()->emitIns_R_I(INS_mov, size, reg, imm);
    }
    else
    {
        // Arm64 allows any arbitrary 16-bit constant to be loaded into a register halfword
        // There are three forms
        //    movk which loads into any halfword preserving the remaining halfwords
        //    movz which loads into any halfword zeroing the remaining halfwords
        //    movn which loads into any halfword zeroing the remaining halfwords then bitwise inverting the register
        // In some cases it is preferable to use movn, because it has the side effect of filling the other halfwords
        // with ones

        // Determine whether movn or movz will require the fewest instructions to populate the immediate
        int preferMovn = 0;

        for (int i = (size == EA_8BYTE) ? 48 : 16; i >= 0; i -= 16)
        {
            if (uint16_t(imm >> i) == 0xffff)
                ++preferMovn; // a single movk 0xffff could be skipped if movn was used
            else if (uint16_t(imm >> i) == 0x0000)
                --preferMovn; // a single movk 0 could be skipped if movz was used
        }

        // Select the first instruction.  Any additional instruction will use movk
        instruction ins = (preferMovn > 0) ? INS_movn : INS_movz;

        // Initial movz or movn will fill the remaining bytes with the skipVal
        // This can allow skipping filling a halfword
        uint16_t skipVal = (preferMovn > 0) ? 0xffff : 0;

        unsigned bits = (size == EA_8BYTE) ? 64 : 32;

        // Iterate over imm examining 16 bits at a time
        for (unsigned i = 0; i < bits; i += 16)
        {
            uint16_t imm16 = uint16_t(imm >> i);

            if (imm16 != skipVal)
            {
                if (ins == INS_movn)
                {
                    // For the movn case, we need to bitwise invert the immediate.  This is because
                    //   (movn x0, ~imm16) === (movz x0, imm16; or x0, x0, #0xffff`ffff`ffff`0000)
                    imm16 = ~imm16;
                }

                GetEmitter()->emitIns_R_I_I(ins, size, reg, imm16, i, INS_OPTS_LSL);

                // Once the initial movz/movn is emitted the remaining instructions will all use movk
                ins = INS_movk;
            }
        }

        // We must emit a movn or movz or we have not done anything
        // The cases which hit this assert should be (emitIns_valid_imm_for_mov() == true) and
        // should not be in this else condition
        assert(ins == INS_movk);
    }
}

void CodeGen::GenIntCon(GenTreeIntCon* node)
{
    if (node->ImmedValNeedsReloc(compiler))
    {
        instGen_Set_Reg_To_Imm(EA_HANDLE_CNS_RELOC, node->GetRegNum(),
                               node->GetValue() DEBUGARG(node->gtTargetHandle) DEBUGARG(node->gtFlags));
    }
    else
    {
        // The only REF constant that can come this path is a 'null' since it is not
        // relocatable. Other REF type constants (e.g. string objects) go through a
        // different code path.
        noway_assert(!node->TypeIs(TYP_REF) || (node->GetValue() == 0));

        instGen_Set_Reg_To_Imm(emitActualTypeSize(node->GetType()), node->GetRegNum(), node->GetValue());
    }

    DefReg(node);
}

void CodeGen::GenDblCon(GenTreeDblCon* node)
{
    if (node->IsPositiveZero())
    {
        GetEmitter()->emitIns_R_I(INS_movi, EA_16BYTE, node->GetRegNum(), 0x00, INS_OPTS_16B);
    }
    else if (emitter::emitIns_valid_imm_for_fmov(node->GetValue()))
    {
        GetEmitter()->emitIns_R_F(INS_fmov, emitTypeSize(node->GetType()), node->GetRegNum(), node->GetValue());
    }
    else
    {
        emitAttr             size = emitTypeSize(node->GetType());
        regNumber            temp = node->GetSingleTempReg();
        CORINFO_FIELD_HANDLE data = GetEmitter()->emitFltOrDblConst(node->GetValue(), size);

        GetEmitter()->emitIns_R_C(INS_ldr, size, node->GetRegNum(), temp, data);
    }

    DefReg(node);
}

void CodeGen::genCodeForIncSaturate(GenTree* tree)
{
    regNumber targetReg  = tree->GetRegNum();
    regNumber operandReg = UseReg(tree->AsUnOp()->GetOp(0));

    GetEmitter()->emitIns_R_R_I(INS_adds, emitActualTypeSize(tree), targetReg, operandReg, 1);
    GetEmitter()->emitIns_R_R_COND(INS_cinv, emitActualTypeSize(tree), targetReg, targetReg, INS_COND_HS);

    DefReg(tree);
}

void CodeGen::GenMulLong(GenTreeOp* mul)
{
    assert(mul->OperIs(GT_MULHI) && !mul->gtOverflowEx() && varTypeIsIntegral(mul->GetType()));

    emitAttr  size    = emitActualTypeSize(mul->GetType());
    regNumber srcReg1 = UseReg(mul->GetOp(0));
    regNumber srcReg2 = UseReg(mul->GetOp(1));
    regNumber dstReg  = mul->GetRegNum();

    if (EA_SIZE(size) == EA_8BYTE)
    {
        instruction ins = mul->IsUnsigned() ? INS_umulh : INS_smulh;

        GetEmitter()->emitIns_R_R_R(ins, size, dstReg, srcReg1, srcReg2);
    }
    else
    {
        assert(EA_SIZE(size) == EA_4BYTE);

        instruction ins = mul->IsUnsigned() ? INS_umull : INS_smull;

        GetEmitter()->emitIns_R_R_R(ins, EA_4BYTE, dstReg, srcReg1, srcReg2);
        GetEmitter()->emitIns_R_R_I(mul->IsUnsigned() ? INS_lsr : INS_asr, EA_8BYTE, dstReg, dstReg, 32);
    }

    DefReg(mul);
}

void CodeGen::genCodeForBinary(GenTreeOp* treeNode)
{
    assert(varTypeIsIntegralOrI(treeNode->GetType()));

    const genTreeOps oper       = treeNode->OperGet();
    regNumber        targetReg  = treeNode->GetRegNum();
    var_types        targetType = treeNode->TypeGet();
    emitter*         emit       = GetEmitter();

    assert(oper == GT_ADD || oper == GT_SUB || oper == GT_MUL || oper == GT_DIV || oper == GT_UDIV || oper == GT_AND ||
           oper == GT_OR || oper == GT_XOR);

    GenTree*    op1 = treeNode->gtGetOp1();
    GenTree*    op2 = treeNode->gtGetOp2();
    instruction ins = genGetInsForOper(treeNode->OperGet());

    if (op1->isUsedFromReg())
    {
        UseReg(op1);
    }

    if (op2->isUsedFromReg())
    {
        UseReg(op2);
    }

    if ((treeNode->gtFlags & GTF_SET_FLAGS) != 0)
    {
        switch (oper)
        {
            case GT_ADD:
                ins = INS_adds;
                break;
            case GT_SUB:
                ins = INS_subs;
                break;
            case GT_AND:
                ins = INS_ands;
                break;
            default:
                noway_assert(!"Unexpected BinaryOp with GTF_SET_FLAGS set");
        }
    }

    // The arithmetic node must be sitting in a register (since it's not contained)
    assert(targetReg != REG_NA);
    emitAttr attr = emitActualTypeSize(treeNode);

    // UMULL/SMULL is twice as fast for 32*32->64bit MUL
    if ((oper == GT_MUL) && (targetType == TYP_LONG) && varActualTypeIsInt(op1) && varActualTypeIsInt(op2))
    {
        ins  = treeNode->IsUnsigned() ? INS_umull : INS_smull;
        attr = EA_4BYTE;
    }
    else
    {
        assert(IsValidSourceType(targetType, op1->GetType()));
        assert(IsValidSourceType(targetType, op2->GetType()));
    }

    regNumber r = emitInsTernary(ins, attr, treeNode, op1, op2);
    assert(r == targetReg);

    DefReg(treeNode);
}

void CodeGen::GenLoadLclVar(GenTreeLclVar* load)
{
    assert(load->OperIs(GT_LCL_VAR));

    LclVarDsc* lcl = compiler->lvaGetDesc(load);

    assert(!lcl->IsIndependentPromoted());

    if (lcl->IsRegCandidate() || load->IsRegSpilled(0))
    {
        return;
    }

    // TODO-MIKE-Review: Does this need special TYP_SIMD12 handling of params on OSX?

    var_types   type = lcl->GetRegisterType(load);
    instruction ins  = ins_Load(type);
    emitAttr    attr = emitActualTypeSize(type);

    GetEmitter()->emitIns_R_S(ins, attr, load->GetRegNum(), load->GetLclNum(), 0);

    DefLclVarReg(load);
}

void CodeGen::GenStoreLclFld(GenTreeLclFld* store)
{
    assert(store->OperIs(GT_STORE_LCL_FLD));

    var_types type = store->GetType();
    GenTree*  src  = store->GetOp(0);

    if (type == TYP_STRUCT)
    {
        ClassLayout*    layout = store->GetLayout(compiler);
        StructStoreKind kind   = GetStructStoreKind(true, layout, src);
        GenStructStore(store, kind, layout);
    }
    else if (type == TYP_SIMD12)
    {
        genStoreSIMD12(store, src);
    }
    else
    {
        assert(IsValidSourceType(type, src->GetType()));

        regNumber srcReg;

        if (src->isContained())
        {
            assert(src->IsIntegralConst(0) || src->IsDblConPositiveZero() || src->IsHWIntrinsicZero());

            srcReg = REG_ZR;
        }
        else
        {
            srcReg = genConsumeReg(src);
        }

        unsigned lclNum  = store->GetLclNum();
        unsigned lclOffs = store->GetLclOffs();

        if ((srcReg == REG_ZR) && (type == TYP_SIMD16))
        {
            GetEmitter()->emitIns_S_S_R_R(INS_stp, EA_8BYTE, EA_8BYTE, srcReg, srcReg, lclNum, lclOffs);
        }
        else
        {
            assert((srcReg != REG_ZR) || (varTypeSize(type) <= REGSIZE_BYTES));

            GetEmitter()->emitIns_S_R(ins_Store(type), emitActualTypeSize(type), srcReg, lclNum, lclOffs);
        }
    }

    genUpdateLife(store);
}

void CodeGen::GenStoreLclVar(GenTreeLclVar* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR));

    LclVarDsc* lcl = compiler->lvaGetDesc(store);

    if (lcl->IsIndependentPromoted())
    {
        GenStoreLclVarMultiReg(store);
        return;
    }

    GenTree* src = store->GetOp(0);

    if (store->TypeIs(TYP_STRUCT))
    {
        ClassLayout*    layout = lcl->GetLayout();
        StructStoreKind kind   = GetStructStoreKind(true, layout, src);
        GenStructStore(store, kind, layout);
        genUpdateLife(store);
        return;
    }

    if (src->IsMultiRegNode())
    {
        assert(varTypeIsSIMD(store->GetType()));

        if (lcl->IsRegCandidate() && (store->GetRegNum() != REG_NA))
        {
            GenStoreLclVarMultiRegSIMDReg(store);
        }
        else
        {
            GenStoreLclVarMultiRegSIMDMem(store);
        }

        return;
    }

    var_types lclRegType = lcl->GetRegisterType(store);

    if (lclRegType == TYP_SIMD12)
    {
        genStoreSIMD12(store, src);
        // TODO-MIKE-Review: Doesn't this need a DefLclVarReg call?
        return;
    }

    regNumber srcReg;

    if (src->isContained())
    {
        assert(src->IsIntegralConst(0) || src->IsDblConPositiveZero() || src->IsHWIntrinsicZero());

        srcReg = REG_ZR;
    }
    else
    {
        srcReg = UseReg(src);
    }

    regNumber dstReg = store->GetRegNum();

    if (dstReg == REG_NA)
    {
        unsigned lclNum = store->GetLclNum();

        if ((srcReg == REG_ZR) && (lclRegType == TYP_SIMD16))
        {
            GetEmitter()->emitIns_S_S_R_R(INS_stp, EA_8BYTE, EA_8BYTE, srcReg, srcReg, lclNum, 0);
        }
        else
        {
            assert((srcReg != REG_ZR) || (varTypeSize(lclRegType) <= REGSIZE_BYTES));

            GetEmitter()->emitIns_S_R(ins_Store(lclRegType), emitActualTypeSize(lclRegType), srcReg, lclNum, 0);
        }

        genUpdateLife(store);
        lcl->SetRegNum(REG_STK);

        return;
    }

    if ((srcReg == REG_ZR) && genIsValidFloatReg(dstReg))
    {
        GetEmitter()->emitIns_R_I(INS_movi, EA_16BYTE, dstReg, 0x00, INS_OPTS_16B);
    }
    else if ((dstReg != srcReg) || (varActualType(lclRegType) != varActualType(src->GetType())))
    {
        GetEmitter()->emitIns_Mov(ins_Copy(lclRegType), emitActualTypeSize(lclRegType), dstReg, srcReg,
                                  /* canSkip */ true);
    }

    DefLclVarReg(store);
}

void CodeGen::GenStoreLclVarMultiRegSIMDReg(GenTreeLclVar* store)
{
    GenTree* src = store->GetOp(0);
    assert(src->IsMultiRegNode());

    UseRegs(src);

    GenTreeCall* call     = src->gtSkipReloadOrCopy()->AsCall();
    unsigned     regCount = call->GetRegCount();
    regNumber    dstReg   = store->GetRegNum();

    for (unsigned i = 0; i < regCount; i++)
    {
        // Vector2/3/4 are returned only in FLOAT regs.
        assert(call->GetRegType(i) == TYP_FLOAT);

        // Insert elements in reverse order, so that the first element in the destination
        // register is last, in case the destination register is also a source register.
        int regIndex = regCount - 1 - i;
        GetEmitter()->emitIns_R_R_I_I(INS_mov, EA_4BYTE, dstReg, call->GetRegNum(regIndex), regIndex, 0);
    }

    DefLclVarReg(store);
}

void CodeGen::GenStoreLclVarMultiRegSIMDMem(GenTreeLclVar* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR) && varTypeIsSIMD(store->GetType()) && !store->IsMultiReg());

    GenTree*     src      = store->GetOp(0);
    GenTreeCall* call     = src->gtSkipReloadOrCopy()->AsCall();
    unsigned     regCount = call->GetRegCount();
    unsigned     lclNum   = store->GetLclNum();
    LclVarDsc*   lcl      = compiler->lvaGetDesc(lclNum);

    assert((regCount >= 2) && (regCount <= 4));
    assert(!lcl->IsRegCandidate() || (store->GetRegNum() == REG_NA));

    regNumber regs[4];

    for (unsigned i = 0; i < regCount; ++i)
    {
        // Vector2/3/4 are returned only in FLOAT regs.
        assert(call->GetMultiRegType(compiler, i) == TYP_FLOAT);

        regs[i] = UseReg(src, i);
    }

    GetEmitter()->emitIns_S_S_R_R(INS_stp, EA_4BYTE, EA_4BYTE, regs[0], regs[1], lclNum, 0);

    if (regCount == 4)
    {
        GetEmitter()->emitIns_S_S_R_R(INS_stp, EA_4BYTE, EA_4BYTE, regs[2], regs[3], lclNum, 8);
    }
    else if (regCount == 3)
    {
        // TODO-MIKE-Review: Do we need to store a 0 for the 4th element of Vector3? Old code did not.
        GetEmitter()->emitIns_S_R(INS_str, EA_4BYTE, regs[2], lclNum, 8);
    }

    genUpdateLife(store);
    lcl->SetRegNum(REG_STK);
}

void CodeGen::genLclHeap(GenTree* tree)
{
    assert(tree->OperGet() == GT_LCLHEAP);
    assert(compiler->compLocallocUsed);

    GenTree* size = tree->AsOp()->gtOp1;
    noway_assert((genActualType(size->gtType) == TYP_INT) || (genActualType(size->gtType) == TYP_I_IMPL));

    regNumber            targetReg                = tree->GetRegNum();
    regNumber            regCnt                   = REG_NA;
    regNumber            pspSymReg                = REG_NA;
    var_types            type                     = genActualType(size->gtType);
    emitAttr             easz                     = emitTypeSize(type);
    BasicBlock*          endLabel                 = nullptr;
    BasicBlock*          loop                     = nullptr;
    unsigned             stackAdjustment          = 0;
    const target_ssize_t ILLEGAL_LAST_TOUCH_DELTA = (target_ssize_t)-1;
    target_ssize_t       lastTouchDelta =
        ILLEGAL_LAST_TOUCH_DELTA; // The number of bytes from SP to the last stack address probed.

    noway_assert(isFramePointerUsed()); // localloc requires Frame Pointer to be established since SP changes
#if !FEATURE_FIXED_OUT_ARGS
    noway_assert(genStackLevel == 0); // Can't have anything on the stack
#endif

    // compute the amount of memory to allocate to properly STACK_ALIGN.
    size_t amount = 0;

    if (GenTreeIntCon* intCon = size->IsIntCon())
    {
        assert(intCon->isContained());

        amount = intCon->GetValue();
        assert(amount != 0);

        // 'amount' is the total number of bytes to localloc to properly STACK_ALIGN
        amount = AlignUp(amount, STACK_ALIGN);
    }
    else
    {
        // If 0 bail out by returning null in targetReg
        genConsumeReg(size);
        genCopyRegIfNeeded(size, targetReg);
        endLabel = genCreateTempLabel();
        GetEmitter()->emitIns_R_R(INS_tst, easz, targetReg, targetReg);
        inst_JMP(EJ_eq, endLabel);

        // Compute the size of the block to allocate and perform alignment.
        // If compInitMem=true, we can reuse targetReg as regcnt,
        // since we don't need any internal registers.
        if (compiler->info.compInitMem)
        {
            assert(tree->AvailableTempRegCount() == 0);
            regCnt = targetReg;
        }
        else
        {
            regCnt = tree->ExtractTempReg();
            inst_Mov(size->TypeGet(), regCnt, targetReg, /* canSkip */ true);
        }

        // Align to STACK_ALIGN
        // regCnt will be the total number of bytes to localloc
        inst_RV_IV(INS_add, regCnt, (STACK_ALIGN - 1), emitActualTypeSize(type));
        inst_RV_IV(INS_and, regCnt, ~(STACK_ALIGN - 1), emitActualTypeSize(type));
    }

    // If we have an outgoing arg area then we must adjust the SP by popping off the
    // outgoing arg area. We will restore it right before we return from this method.
    //
    // Localloc returns stack space that aligned to STACK_ALIGN bytes. The following
    // are the cases that need to be handled:
    //   i) Method has out-going arg area.
    //      It is guaranteed that size of out-going arg area is STACK_ALIGN'ed (see fgMorphArgs).
    //      Therefore, we will pop off the out-going arg area from the stack pointer before allocating the localloc
    //      space.
    //  ii) Method has no out-going arg area.
    //      Nothing to pop off from the stack.
    if (outgoingArgSpaceSize > 0)
    {
        // This must be true for the stack to remain aligned
        assert(outgoingArgSpaceSize % STACK_ALIGN == 0);

        genInstrWithConstant(INS_add, EA_PTRSIZE, REG_SPBASE, REG_SPBASE, outgoingArgSpaceSize, rsGetRsvdReg());
        stackAdjustment += outgoingArgSpaceSize;
    }

    if (size->IsCnsIntOrI())
    {
        // We should reach here only for non-zero, constant size allocations.
        assert(amount > 0);

        // For small allocations we will generate up to four stp instructions, to zero 16 to 64 bytes.
        static_assert_no_msg(STACK_ALIGN == (REGSIZE_BYTES * 2));
        assert(amount % (REGSIZE_BYTES * 2) == 0); // stp stores two registers at a time
        size_t stpCount = amount / (REGSIZE_BYTES * 2);
        if (stpCount <= 4)
        {
            while (stpCount != 0)
            {
                // We can use pre-indexed addressing.
                // stp ZR, ZR, [SP, #-16]!   // STACK_ALIGN is 16
                GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_PTRSIZE, REG_ZR, REG_ZR, REG_SPBASE, -16, INS_OPTS_PRE_INDEX);
                stpCount -= 1;
            }

            lastTouchDelta = 0;

            goto ALLOC_DONE;
        }
        else if (!compiler->info.compInitMem && (amount < compiler->eeGetPageSize())) // must be < not <=
        {
            // Since the size is less than a page, simply adjust the SP value.
            // The SP might already be in the guard page, so we must touch it BEFORE
            // the alloc, not after.

            // ldr wz, [SP, #0]
            GetEmitter()->emitIns_R_R_I(INS_ldr, EA_4BYTE, REG_ZR, REG_SP, 0);

            genInstrWithConstant(INS_sub, EA_PTRSIZE, REG_SPBASE, REG_SPBASE, amount, rsGetRsvdReg());

            lastTouchDelta = amount;

            goto ALLOC_DONE;
        }

        // else, "mov regCnt, amount"
        // If compInitMem=true, we can reuse targetReg as regcnt.
        // Since size is a constant, regCnt is not yet initialized.
        assert(regCnt == REG_NA);
        if (compiler->info.compInitMem)
        {
            assert(tree->AvailableTempRegCount() == 0);
            regCnt = targetReg;
        }
        else
        {
            regCnt = tree->ExtractTempReg();
        }

        instGen_Set_Reg_To_Imm(amount > UINT32_MAX ? EA_8BYTE : EA_4BYTE, regCnt, amount);
    }

    if (compiler->info.compInitMem)
    {
        BasicBlock* loop = genCreateTempLabel();

        // At this point 'regCnt' is set to the total number of bytes to locAlloc.
        // Since we have to zero out the allocated memory AND ensure that the stack pointer is always valid
        // by tickling the pages, we will just push 0's on the stack.
        //
        // Note: regCnt is guaranteed to be even on Amd64 since STACK_ALIGN/TARGET_POINTER_SIZE = 2
        // and localloc size is a multiple of STACK_ALIGN.

        // Loop:
        genDefineTempLabel(loop);

        // We can use pre-indexed addressing.
        // stp ZR, ZR, [SP, #-16]!
        GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_PTRSIZE, REG_ZR, REG_ZR, REG_SPBASE, -16, INS_OPTS_PRE_INDEX);

        // If not done, loop
        // Note that regCnt is the number of bytes to stack allocate.
        // Therefore we need to subtract 16 from regcnt here.
        assert(genIsValidIntReg(regCnt));
        inst_RV_IV(INS_subs, regCnt, 16, emitActualTypeSize(type));
        inst_JMP(EJ_ne, loop);

        lastTouchDelta = 0;
    }
    else
    {
        // At this point 'regCnt' is set to the total number of bytes to localloc.
        //
        // We don't need to zero out the allocated memory. However, we do have
        // to tickle the pages to ensure that SP is always valid and is
        // in sync with the "stack guard page".  Note that in the worst
        // case SP is on the last byte of the guard page.  Thus you must
        // touch SP-0 first not SP-0x1000.
        //
        // This is similar to the prolog code in CodeGen::PrologAllocLclFrame().
        //
        // Note that we go through a few hoops so that SP never points to
        // illegal pages at any time during the tickling process.
        //
        //       subs  regCnt, SP, regCnt      // regCnt now holds ultimate SP
        //       bvc   Loop                    // result is smaller than original SP (no wrap around)
        //       mov   regCnt, #0              // Overflow, pick lowest possible value
        //
        //  Loop:
        //       ldr   wzr, [SP + 0]           // tickle the page - read from the page
        //       sub   regTmp, SP, PAGE_SIZE   // decrement SP by eeGetPageSize()
        //       cmp   regTmp, regCnt
        //       jb    Done
        //       mov   SP, regTmp
        //       j     Loop
        //
        //  Done:
        //       mov   SP, regCnt
        //

        // Setup the regTmp
        regNumber regTmp = tree->GetSingleTempReg();

        BasicBlock* loop = genCreateTempLabel();
        BasicBlock* done = genCreateTempLabel();

        //       subs  regCnt, SP, regCnt      // regCnt now holds ultimate SP
        GetEmitter()->emitIns_R_R_R(INS_subs, EA_8BYTE, regCnt, REG_SP, regCnt);

        inst_JMP(EJ_vc, loop); // branch if the V flag is not set

        // Overflow, set regCnt to lowest possible value
        GetEmitter()->emitIns_R_I(INS_mov, EA_8BYTE, regCnt, 0);

        genDefineTempLabel(loop);

        // tickle the page - Read from the updated SP - this triggers a page fault when on the guard page
        GetEmitter()->emitIns_R_R_I(INS_ldr, EA_4BYTE, REG_ZR, REG_SP, 0);

        // decrement SP by eeGetPageSize()
        GetEmitter()->emitIns_R_R_I(INS_sub, EA_8BYTE, regTmp, REG_SP, compiler->eeGetPageSize());

        GetEmitter()->emitIns_R_R(INS_cmp, EA_8BYTE, regTmp, regCnt);
        inst_JMP(EJ_lo, done);

        // Update SP to be at the next page of stack that we will tickle
        GetEmitter()->emitIns_Mov(INS_mov, EA_8BYTE, REG_SP, regTmp, /* canSkip */ false);

        // Jump to loop and tickle new stack address
        inst_JMP(EJ_jmp, loop);

        // Done with stack tickle loop
        genDefineTempLabel(done);

        // Now just move the final value to SP
        GetEmitter()->emitIns_Mov(INS_mov, EA_8BYTE, REG_SP, regCnt, /* canSkip */ false);

        // lastTouchDelta is dynamic, and can be up to a page. So if we have outgoing arg space,
        // we're going to assume the worst and probe.
    }

ALLOC_DONE:
    // Re-adjust SP to allocate outgoing arg area. We must probe this adjustment.
    if (stackAdjustment != 0)
    {
        assert((stackAdjustment % STACK_ALIGN) == 0); // This must be true for the stack to remain aligned
        assert((lastTouchDelta == ILLEGAL_LAST_TOUCH_DELTA) || (lastTouchDelta >= 0));

        const regNumber tmpReg = rsGetRsvdReg();

        if ((lastTouchDelta == ILLEGAL_LAST_TOUCH_DELTA) ||
            (stackAdjustment + (unsigned)lastTouchDelta + STACK_PROBE_BOUNDARY_THRESHOLD_BYTES >
             compiler->eeGetPageSize()))
        {
            genStackPointerConstantAdjustmentLoopWithProbe(-(ssize_t)stackAdjustment, tmpReg);
        }
        else
        {
            genStackPointerConstantAdjustment(-(ssize_t)stackAdjustment, tmpReg);
        }

        // Return the stackalloc'ed address in result register.
        // TargetReg = SP + stackAdjustment.
        //
        genInstrWithConstant(INS_add, EA_PTRSIZE, targetReg, REG_SPBASE, (ssize_t)stackAdjustment, tmpReg);
    }
    else // stackAdjustment == 0
    {
        // Move the final value of SP to targetReg
        inst_Mov(TYP_I_IMPL, targetReg, REG_SPBASE, /* canSkip */ false);
    }

    if (endLabel != nullptr)
    {
        genDefineTempLabel(endLabel);
    }

    genProduceReg(tree);
}

void CodeGen::genCodeForBswap(GenTree* tree)
{
    assert(tree->OperIs(GT_BSWAP, GT_BSWAP16));

    regNumber targetReg  = tree->GetRegNum();
    var_types targetType = tree->TypeGet();

    GenTree* operand = tree->gtGetOp1();
    assert(operand->isUsedFromReg());
    regNumber operandReg = genConsumeReg(operand);

    if (tree->OperIs(GT_BSWAP16))
    {
        inst_RV_RV(INS_rev16, targetReg, operandReg, targetType);
    }
    else
    {
        inst_RV_RV(INS_rev, targetReg, operandReg, targetType);
    }

    genProduceReg(tree);
}

void CodeGen::GenDivMod(GenTreeOp* div)
{
    assert(div->OperIs(GT_DIV, GT_UDIV) && varTypeIsIntegral(div->GetType()));

    GenTree* dividend = div->GetOp(0);
    GenTree* divisor  = div->GetOp(1);

    regNumber dividendReg = UseReg(dividend);
    regNumber divisorReg  = UseReg(divisor);
    regNumber dstReg      = div->GetRegNum();

    emitAttr attr = emitActualTypeSize(div->GetType());
    emitter* emit = GetEmitter();

    if (divisor->IsIntegralConst(0))
    {
        genJumpToThrowHlpBlk(EJ_jmp, ThrowHelperKind::DivideByZero);
    }
    else
    {
        // (U)DIV(AnyVal, 0) => DivideByZeroException
        // DIV(MinInt, -1) => ArithmeticException

        bool checkDividend = true;

        if (!divisor->IsIntCon())
        {
            emit->emitIns_R_I(INS_cmp, attr, divisorReg, 0);
            genJumpToThrowHlpBlk(EJ_eq, ThrowHelperKind::DivideByZero);
        }
        else
        {
            checkDividend = divisor->AsIntCon()->GetValue() == -1;
        }

        if (div->OperIs(GT_DIV) && checkDividend)
        {
            BasicBlock* sdivLabel = genCreateTempLabel();
            emit->emitIns_R_I(INS_cmp, attr, divisorReg, -1);
            inst_JMP(EJ_ne, sdivLabel);
            emit->emitIns_R_R_R(INS_adds, attr, REG_ZR, dividendReg, dividendReg);
            inst_JMP(EJ_ne, sdivLabel);
            genJumpToThrowHlpBlk(EJ_vs, ThrowHelperKind::Arithmetic);
            genDefineTempLabel(sdivLabel);
        }

        emit->emitIns_R_R_R(div->OperIs(GT_DIV) ? INS_sdiv : INS_udiv, attr, dstReg, dividendReg, divisorReg);
    }

    DefReg(div);
}

void CodeGen::genTableBasedSwitch(GenTreeOp* treeNode)
{
    regNumber idxReg  = UseReg(treeNode->GetOp(0));
    regNumber baseReg = UseReg(treeNode->GetOp(1));

    regNumber tmpReg = treeNode->GetSingleTempReg();

    // load the ip-relative offset (which is relative to start of fgFirstBB)
    GetEmitter()->emitIns_R_R_R(INS_ldr, EA_4BYTE, baseReg, baseReg, idxReg, INS_OPTS_LSL);

    // add it to the absolute address of fgFirstBB
    GetEmitter()->emitIns_R_L(INS_adr, EA_PTRSIZE, compiler->fgFirstBB, tmpReg);
    GetEmitter()->emitIns_R_R_R(INS_add, EA_PTRSIZE, baseReg, baseReg, tmpReg);

    // br baseReg
    GetEmitter()->emitIns_R(INS_br, emitActualTypeSize(TYP_I_IMPL), baseReg);
}

void CodeGen::GenJmpTable(GenTree* node, BasicBlock* switchBlock)
{
    assert(switchBlock->bbJumpKind == BBJ_SWITCH);
    assert(node->OperIs(GT_JMPTABLE));

    unsigned     jumpCount  = switchBlock->bbJumpSwt->bbsCount;
    BasicBlock** jumpTable  = switchBlock->bbJumpSwt->bbsDstTab;
    unsigned     jmpTabBase = GetEmitter()->emitBBTableDataGenBeg(jumpCount, true);

    JITDUMP("\n      J_M%03u_DS%02u LABEL   DWORD\n", compiler->compMethodID, jmpTabBase);

    for (unsigned i = 0; i < jumpCount; i++)
    {
        BasicBlock* target = *jumpTable++;
        noway_assert(target->bbFlags & BBF_HAS_LABEL);

        JITDUMP("            DD      L_M%03u_" FMT_BB "\n", compiler->compMethodID, target->bbNum);

        GetEmitter()->emitDataGenData(i, target);
    };

    GetEmitter()->emitDataGenEnd();

    // Access to inline data is 'abstracted' by a special type of static member
    // (produced by eeFindJitDataOffs) which the emitter recognizes as being a reference
    // to constant data, not a real static field.
    GetEmitter()->emitIns_R_C(INS_adr, EA_PTRSIZE, node->GetRegNum(), REG_NA, compiler->eeFindJitDataOffs(jmpTabBase));
    DefReg(node);
}

void CodeGen::genLockedInstructions(GenTreeOp* treeNode)
{
    GenTree* addr = treeNode->GetOp(0);
    GenTree* data = treeNode->GetOp(1);

    regNumber addrReg   = UseReg(addr);
    regNumber dataReg   = data->isUsedFromReg() ? UseReg(data) : REG_NA;
    regNumber targetReg = treeNode->GetRegNum();

    emitAttr dataSize = emitActualTypeSize(data);

    if (compiler->compOpportunisticallyDependsOn(InstructionSet_Atomics))
    {
        assert(!data->isContainedIntOrIImmed());

        switch (treeNode->gtOper)
        {
            case GT_XORR:
                GetEmitter()->emitIns_R_R_R(INS_ldsetal, dataSize, dataReg, (targetReg == REG_NA) ? REG_ZR : targetReg,
                                            addrReg);
                break;
            case GT_XAND:
            {
                // Grab a temp reg to perform `MVN` for dataReg first.
                regNumber tempReg = treeNode->GetSingleTempReg();
                GetEmitter()->emitIns_R_R(INS_mvn, dataSize, tempReg, dataReg);
                GetEmitter()->emitIns_R_R_R(INS_ldclral, dataSize, tempReg, (targetReg == REG_NA) ? REG_ZR : targetReg,
                                            addrReg);
                break;
            }
            case GT_XCHG:
                GetEmitter()->emitIns_R_R_R(INS_swpal, dataSize, dataReg, targetReg, addrReg);
                break;
            case GT_XADD:
                GetEmitter()->emitIns_R_R_R(INS_ldaddal, dataSize, dataReg, (targetReg == REG_NA) ? REG_ZR : targetReg,
                                            addrReg);
                break;
            default:
                assert(!"Unexpected treeNode->gtOper");
        }
    }
    else
    {
        // These are imported normally if Atomics aren't supported.
        assert(!treeNode->OperIs(GT_XORR, GT_XAND));

        regNumber exResultReg  = treeNode->ExtractTempReg(RBM_ALLINT);
        regNumber storeDataReg = (treeNode->OperGet() == GT_XCHG) ? dataReg : treeNode->ExtractTempReg(RBM_ALLINT);
        regNumber loadReg      = (targetReg != REG_NA) ? targetReg : storeDataReg;

        // Check allocator assumptions
        //
        // The register allocator should have extended the lifetimes of all input and internal registers so that
        // none interfere with the target.
        noway_assert(addrReg != targetReg);

        noway_assert(addrReg != loadReg);
        noway_assert(dataReg != loadReg);

        noway_assert(addrReg != storeDataReg);
        noway_assert((treeNode->OperGet() == GT_XCHG) || (addrReg != dataReg));

        assert(addr->isUsedFromReg());
        noway_assert(exResultReg != REG_NA);
        noway_assert(exResultReg != targetReg);
        noway_assert((targetReg != REG_NA) || (treeNode->OperGet() != GT_XCHG));

        // Store exclusive unpredictable cases must be avoided
        noway_assert(exResultReg != storeDataReg);
        noway_assert(exResultReg != addrReg);

        // TODO-MIKE-Review: This is dubious, GC liveness doesn't really matter until we reach a call...

        // NOTE: `genConsumeAddress` marks the consumed register as not a GC pointer, as it assumes that the input
        // registers
        // die at the first instruction generated by the node. This is not the case for these atomics as the  input
        // registers are multiply-used. As such, we need to mark the addr register as containing a GC pointer until
        // we are finished generating the code for this node.

        liveness.SetGCRegType(addrReg, addr->GetType());

        // Emit code like this:
        //   retry:
        //     ldxr loadReg, [addrReg]
        //     add storeDataReg, loadReg, dataReg         # Only for GT_XADD
        //                                                # GT_XCHG storeDataReg === dataReg
        //     stxr exResult, storeDataReg, [addrReg]
        //     cbnz exResult, retry
        //     dmb ish

        BasicBlock* labelRetry = genCreateTempLabel();
        genDefineTempLabel(labelRetry);

        // The following instruction includes a acquire half barrier
        GetEmitter()->emitIns_R_R(INS_ldaxr, dataSize, loadReg, addrReg);

        switch (treeNode->OperGet())
        {
            case GT_XADD:
                if (data->isContainedIntOrIImmed())
                {
                    // Even though INS_add is specified here, the encoder will choose either
                    // an INS_add or an INS_sub and encode the immediate as a positive value
                    genInstrWithConstant(INS_add, dataSize, storeDataReg, loadReg, data->AsIntConCommon()->IconValue(),
                                         REG_NA);
                }
                else
                {
                    GetEmitter()->emitIns_R_R_R(INS_add, dataSize, storeDataReg, loadReg, dataReg);
                }
                break;
            case GT_XCHG:
                assert(!data->isContained());
                storeDataReg = dataReg;
                break;
            default:
                unreached();
        }

        // The following instruction includes a release half barrier
        GetEmitter()->emitIns_R_R_R(INS_stlxr, dataSize, exResultReg, storeDataReg, addrReg);

        GetEmitter()->emitIns_J_R(INS_cbnz, EA_4BYTE, labelRetry, exResultReg);

        instGen_MemoryBarrier();

        liveness.RemoveGCRegs(genRegMask(addrReg));
    }

    if (treeNode->GetRegNum() != REG_NA)
    {
        genProduceReg(treeNode);
    }
}

void CodeGen::genCodeForCmpXchg(GenTreeCmpXchg* treeNode)
{
    assert(treeNode->OperIs(GT_CMPXCHG));

    GenTree* addr      = treeNode->GetAddr();
    GenTree* data      = treeNode->GetValue();
    GenTree* comparand = treeNode->GetCompareValue();

    regNumber addrReg      = UseReg(addr);
    regNumber dataReg      = UseReg(data);
    regNumber comparandReg = comparand->isUsedFromReg() ? UseReg(comparand) : REG_NA;
    regNumber targetReg    = treeNode->GetRegNum();

    if (compiler->compOpportunisticallyDependsOn(InstructionSet_Atomics))
    {
        emitAttr dataSize = emitActualTypeSize(data);

        // casal use the comparand as the target reg
        GetEmitter()->emitIns_Mov(INS_mov, dataSize, targetReg, comparandReg, /* canSkip */ true);

        // Catch case we destroyed data or address before use
        noway_assert((addrReg != targetReg) || (targetReg == comparandReg));
        noway_assert((dataReg != targetReg) || (targetReg == comparandReg));

        GetEmitter()->emitIns_R_R_R(INS_casal, dataSize, targetReg, dataReg, addrReg);
    }
    else
    {
        regNumber exResultReg = treeNode->ExtractTempReg(RBM_ALLINT);

        // Check allocator assumptions
        //
        // The register allocator should have extended the lifetimes of all input and internal registers so that
        // none interfere with the target.
        noway_assert(addrReg != targetReg);
        noway_assert(dataReg != targetReg);
        noway_assert(comparandReg != targetReg);
        noway_assert(addrReg != dataReg);
        noway_assert(targetReg != REG_NA);
        noway_assert(exResultReg != REG_NA);
        noway_assert(exResultReg != targetReg);

        // Store exclusive unpredictable cases must be avoided
        noway_assert(exResultReg != dataReg);
        noway_assert(exResultReg != addrReg);

        // TODO-MIKE-Review: This is dubious, GC liveness stuff doesn't really matter until we reach a call...

        // NOTE: `genConsumeAddress` marks the consumed register as not a GC pointer, as it assumes that the input
        // registers
        // die at the first instruction generated by the node. This is not the case for these atomics as the  input
        // registers are multiply-used. As such, we need to mark the addr register as containing a GC pointer until
        // we are finished generating the code for this node.

        liveness.SetGCRegType(addrReg, addr->TypeGet());

        // TODO-ARM64-CQ Use ARMv8.1 atomics if available
        // https://github.com/dotnet/runtime/issues/8225

        // Emit code like this:
        //   retry:
        //     ldxr targetReg, [addrReg]
        //     cmp targetReg, comparandReg
        //     bne compareFail
        //     stxr exResult, dataReg, [addrReg]
        //     cbnz exResult, retry
        //   compareFail:
        //     dmb ish

        BasicBlock* labelRetry       = genCreateTempLabel();
        BasicBlock* labelCompareFail = genCreateTempLabel();
        genDefineTempLabel(labelRetry);

        // The following instruction includes a acquire half barrier
        GetEmitter()->emitIns_R_R(INS_ldaxr, emitTypeSize(treeNode), targetReg, addrReg);

        if (GenTreeIntCon* con = comparand->IsContainedIntCon())
        {
            if (con->GetValue() == 0)
            {
                GetEmitter()->emitIns_J_R(INS_cbnz, emitActualTypeSize(treeNode), labelCompareFail, targetReg);
            }
            else
            {
                GetEmitter()->emitIns_R_I(INS_cmp, emitActualTypeSize(treeNode), targetReg, con->GetValue());
                GetEmitter()->emitIns_J(INS_bne, labelCompareFail);
            }
        }
        else
        {
            GetEmitter()->emitIns_R_R(INS_cmp, emitActualTypeSize(treeNode), targetReg, comparandReg);
            GetEmitter()->emitIns_J(INS_bne, labelCompareFail);
        }

        // The following instruction includes a release half barrier
        GetEmitter()->emitIns_R_R_R(INS_stlxr, emitTypeSize(treeNode), exResultReg, dataReg, addrReg);

        GetEmitter()->emitIns_J_R(INS_cbnz, EA_4BYTE, labelRetry, exResultReg);

        genDefineTempLabel(labelCompareFail);

        instGen_MemoryBarrier();

        liveness.RemoveGCRegs(genRegMask(addrReg));
    }

    genProduceReg(treeNode);
}

instruction CodeGen::genGetInsForOper(genTreeOps oper)
{
    switch (oper)
    {
        case GT_ADD:
            return INS_add;
        case GT_AND:
            return INS_and;
        case GT_DIV:
            return INS_sdiv;
        case GT_UDIV:
            return INS_udiv;
        case GT_MUL:
            return INS_mul;
        case GT_LSH:
            return INS_lsl;
        case GT_NEG:
            return INS_neg;
        case GT_NOT:
            return INS_mvn;
        case GT_OR:
            return INS_orr;
        case GT_ROR:
            return INS_ror;
        case GT_RSH:
            return INS_asr;
        case GT_RSZ:
            return INS_lsr;
        case GT_SUB:
            return INS_sub;
        case GT_XOR:
            return INS_eor;
        default:
            unreached();
    }
}

void CodeGen::genCodeForReturnTrap(GenTreeOp* tree)
{
    assert(tree->OperGet() == GT_RETURNTRAP);

    // this is nothing but a conditional call to CORINFO_HELP_STOP_FOR_GC
    // based on the contents of 'data'

    regNumber reg = UseReg(tree->GetOp(0));

    GetEmitter()->emitIns_R_I(INS_cmp, EA_4BYTE, reg, 0);

    BasicBlock* skipLabel = genCreateTempLabel();

    inst_JMP(EJ_eq, skipLabel);
    // emit the call to the EE-helper that stops for GC (or other reasons)

    genEmitHelperCall(CORINFO_HELP_STOP_FOR_GC);
    genDefineTempLabel(skipLabel);
}

void CodeGen::genCodeForNullCheck(GenTreeIndir* tree)
{
    assert(tree->OperIs(GT_NULLCHECK));

    genConsumeAddress(tree->GetAddr());
    emitInsLoad(INS_ldr, EA_4BYTE, REG_ZR, tree);
}

void CodeGen::genCodeForIndir(GenTreeIndir* load)
{
    assert(load->OperIs(GT_IND));

    if (load->TypeIs(TYP_SIMD12))
    {
        LoadSIMD12(load);
        genProduceReg(load);
        return;
    }

    genConsumeAddress(load->Addr());

    var_types   type        = load->GetType();
    instruction ins         = ins_Load(type);
    regNumber   dstReg      = load->GetRegNum();
    bool        emitBarrier = false;

    if (load->IsVolatile())
    {
        bool addrIsInReg   = load->Addr()->isUsedFromReg();
        bool addrIsAligned = !load->IsUnaligned();

        if ((ins == INS_ldrb) && addrIsInReg)
        {
            ins = INS_ldarb;
        }
        else if ((ins == INS_ldrh) && addrIsInReg && addrIsAligned)
        {
            ins = INS_ldarh;
        }
        else if ((ins == INS_ldr) && addrIsInReg && addrIsAligned && genIsValidIntReg(dstReg))
        {
            ins = INS_ldar;
        }
        else
        {
            emitBarrier = true;
        }
    }

    emitInsLoad(ins, emitActualTypeSize(type), dstReg, load);

    if (emitBarrier)
    {
        // when INS_ldar* could not be used for a volatile load,
        // we use an ordinary load followed by a load barrier.
        instGen_MemoryBarrier(BARRIER_LOAD_ONLY);
    }

    DefReg(load);
}

void CodeGen::genCodeForStoreInd(GenTreeStoreInd* tree)
{
    if (tree->TypeIs(TYP_SIMD12))
    {
        genStoreSIMD12(tree, tree->GetValue());
        return;
    }

    GenTree* addr = tree->GetAddr();
    GenTree* data = tree->GetValue();

    assert(IsValidSourceType(tree->GetType(), data->GetType()));

    GCInfo::WriteBarrierForm writeBarrierForm = GCInfo::GetWriteBarrierForm(tree);
    if (writeBarrierForm != GCInfo::WBF_NoBarrier)
    {
        regNumber addrReg = UseReg(addr);
        regNumber dataReg = UseReg(data);

        // At this point, we should not have any interference.
        // That is, 'data' must not be in REG_WRITE_BARRIER_DST_BYREF,
        // as that is where 'addr' must go.
        noway_assert(dataReg != REG_WRITE_BARRIER_DST_BYREF);

        inst_Mov(addr->GetType(), REG_WRITE_BARRIER_DST, addrReg, /* canSkip */ true);
        inst_Mov(data->GetType(), REG_WRITE_BARRIER_SRC, dataReg, /* canSkip */ true);
        genGCWriteBarrier(tree, writeBarrierForm);

        return;
    }

    // We must consume the operands in the proper execution order,
    // so that liveness is updated appropriately.
    genConsumeAddress(addr);
    var_types type = tree->GetType();
    regNumber dataReg;

    if (data->isContained())
    {
        assert(data->IsIntegralConst(0) || data->IsDblConPositiveZero() || data->IsHWIntrinsicZero());
        assert(varTypeSize(type) <= REGSIZE_BYTES);

        dataReg = REG_ZR;
    }
    else
    {
        dataReg = UseReg(data);
    }

    instruction ins = ins_Store(type);

    if (tree->IsVolatile())
    {
        bool addrIsInReg   = addr->isUsedFromReg();
        bool addrIsAligned = ((tree->gtFlags & GTF_IND_UNALIGNED) == 0);

        if ((ins == INS_strb) && addrIsInReg)
        {
            ins = INS_stlrb;
        }
        else if ((ins == INS_strh) && addrIsInReg && addrIsAligned)
        {
            ins = INS_stlrh;
        }
        else if ((ins == INS_str) && genIsValidIntReg(dataReg) && addrIsInReg && addrIsAligned)
        {
            ins = INS_stlr;
        }
        else
        {
            // issue a full memory barrier before a volatile StInd
            instGen_MemoryBarrier();
        }
    }

    emitInsStore(ins, emitActualTypeSize(type), dataReg, tree);
}

void CodeGen::genIntToFloatCast(GenTreeCast* cast)
{
    assert(cast->GetType() == cast->GetCastType());
    assert(!cast->gtOverflow());

    GenTree*  src     = cast->GetOp(0);
    var_types srcType = varActualType(src->GetType());
    var_types dstType = cast->GetType();

    noway_assert((srcType == TYP_INT) || (srcType == TYP_LONG));
    assert((dstType == TYP_FLOAT) || (dstType == TYP_DOUBLE));

    regNumber srcReg = UseReg(src);
    regNumber dstReg = cast->GetRegNum();

    assert(genIsValidIntReg(srcReg) && genIsValidFloatReg(dstReg));

    instruction ins     = cast->IsUnsigned() ? INS_ucvtf : INS_scvtf;
    emitAttr    insSize = emitTypeSize(dstType);
    insOpts     opts;

    if (dstType == TYP_DOUBLE)
    {
        opts = (srcType == TYP_INT) ? INS_OPTS_4BYTE_TO_D : INS_OPTS_8BYTE_TO_D;
    }
    else
    {
        opts = (srcType == TYP_INT) ? INS_OPTS_4BYTE_TO_S : INS_OPTS_8BYTE_TO_S;
    }

    GetEmitter()->emitIns_R_R(ins, insSize, dstReg, srcReg, opts);

    genProduceReg(cast);
}

void CodeGen::genFloatToIntCast(GenTreeCast* cast)
{
    assert(!cast->gtOverflow());

    GenTree*  src     = cast->GetOp(0);
    var_types srcType = src->GetType();
    var_types dstType = cast->GetCastType();

    assert((srcType == TYP_FLOAT) || (srcType == TYP_DOUBLE));
    noway_assert((dstType == TYP_INT) || (dstType == TYP_UINT) || (dstType == TYP_LONG) || (dstType == TYP_ULONG));
    assert(cast->GetType() == varActualType(dstType));

    regNumber srcReg = genConsumeReg(src);
    regNumber dstReg = cast->GetRegNum();

    assert(genIsValidFloatReg(srcReg) && genIsValidIntReg(dstReg));

    instruction ins     = varTypeIsUnsigned(dstType) ? INS_fcvtzu : INS_fcvtzs;
    emitAttr    insSize = emitTypeSize(dstType);
    insOpts     opts;

    if (srcType == TYP_DOUBLE)
    {
        opts = (insSize == EA_4BYTE) ? INS_OPTS_D_TO_4BYTE : INS_OPTS_D_TO_8BYTE;
    }
    else
    {
        opts = (insSize == EA_4BYTE) ? INS_OPTS_S_TO_4BYTE : INS_OPTS_S_TO_8BYTE;
    }

    GetEmitter()->emitIns_R_R(ins, insSize, dstReg, srcReg, opts);

    genProduceReg(cast);
}

void CodeGen::genCkfinite(GenTree* treeNode)
{
    assert(treeNode->OperIs(GT_CKFINITE));

    GenTree*  op1         = treeNode->AsUnOp()->GetOp(0);
    var_types targetType  = treeNode->GetType();
    int       expMask     = targetType == TYP_FLOAT ? 0x7F8 : 0x7FF; // Bit mask to extract exponent.
    int       shiftAmount = targetType == TYP_FLOAT ? 20 : 52;

    emitter* emit = GetEmitter();

    // Extract exponent into a register.
    regNumber intReg = treeNode->GetSingleTempReg();
    regNumber fpReg  = genConsumeReg(op1);

    inst_Mov(targetType, intReg, fpReg, /* canSkip */ false);
    emit->emitIns_R_R_I(INS_lsr, emitActualTypeSize(targetType), intReg, intReg, shiftAmount);

    // Mask of exponent with all 1's and check if the exponent is all 1's
    emit->emitIns_R_R_I(INS_and, EA_4BYTE, intReg, intReg, expMask);
    emit->emitIns_R_I(INS_cmp, EA_4BYTE, intReg, expMask);

    // If exponent is all 1's, throw ArithmeticException
    genJumpToThrowHlpBlk(EJ_eq, ThrowHelperKind::Arithmetic);

    // if it is a finite value copy it to targetReg
    inst_Mov(targetType, treeNode->GetRegNum(), fpReg, /* canSkip */ true);

    genProduceReg(treeNode);
}

void CodeGen::GenCompare(GenTreeOp* cmp)
{
    GenTree*  op1   = cmp->GetOp(0);
    GenTree*  op2   = cmp->GetOp(1);
    var_types type1 = varActualType(op1->GetType());
    var_types type2 = varActualType(op2->GetType());
    regNumber reg1  = UseReg(op1);
    regNumber reg2  = op2->isContained() ? REG_NA : UseReg(op2);
    emitAttr  attr  = EA_ATTR(varTypeSize(type1));

    assert(varTypeSize(type1) == varTypeSize(type2));

    emitter* emit = GetEmitter();

    if (varTypeIsFloating(type1))
    {
        assert(type1 == type2);

        // TODO-MIKE-Review: This is nonsense...
        if (op2->IsIntegralConst(0))
        {
            assert(op2->isContained());
            emit->emitIns_R_F(INS_fcmp, attr, reg1, 0.0);
        }
        else
        {
            assert(!op2->isContained());
            emit->emitIns_R_R(INS_fcmp, attr, reg1, reg2);
        }
    }
    else
    {
        assert(!varTypeIsFloating(type2));
        // We don't support swapping op1 and op2 to generate cmp reg, imm
        assert(!op1->IsContainedIntCon());

        instruction ins = cmp->OperIs(GT_TEST_EQ, GT_TEST_NE) ? INS_tst : INS_cmp;

        if (op2->IsContainedIntCon())
        {
            emit->emitIns_R_I(ins, attr, reg1, op2->AsIntCon()->GetValue());
        }
        else
        {
            emit->emitIns_R_R(ins, attr, reg1, reg2);
        }
    }

    if (cmp->GetRegNum() == REG_NA)
    {
        return;
    }

    inst_SETCC(GenCondition::FromRelop(cmp), cmp->GetType(), cmp->GetRegNum());
    DefReg(cmp);
}

// Generates code for JCMP node.
//
// A GT_JCMP node is created when a comparison and conditional branch
// can be executed in a single instruction.
//
// Arm64 has a few instructions with this behavior.
//   - cbz/cbnz -- Compare and branch register zero/not zero
//   - tbz/tbnz -- Test and branch register bit zero/not zero
//
// The cbz/cbnz supports the normal +/- 1MB branch range for conditional branches
// The tbz/tbnz supports a  smaller +/- 32KB branch range
//
// A GT_JCMP cbz/cbnz node is created when there is a GT_EQ or GT_NE
// integer/unsigned comparison against #0 which is used by a GT_JTRUE
// condition jump node.
//
// A GT_JCMP tbz/tbnz node is created when there is a GT_TEST_EQ or GT_TEST_NE
// integer/unsigned comparison against against a mask with a single bit set
// which is used by a GT_JTRUE condition jump node.
//
// This node is repsonsible for consuming the register, and emitting the
// appropriate fused compare/test and branch instruction
//
// Two flags guide code generation
//    GTF_JCMP_TST -- Set if this is a tbz/tbnz rather than cbz/cbnz
//    GTF_JCMP_EQ  -- Set if this is cbz/tbz rather than cbnz/tbnz
//
void CodeGen::GenJCmp(GenTreeOp* tree, BasicBlock* block)
{
    assert(tree->OperIs(GT_JCMP));
    assert(block->bbJumpKind == BBJ_COND);

    GenTree* op1 = tree->GetOp(0);
    GenTree* op2 = tree->GetOp(1);

    assert(!varTypeIsFloating(tree));
    assert(op2->IsIntCon());
    assert(op2->isContained());

    regNumber reg = UseReg(op1);

    emitAttr attr = emitActualTypeSize(op1->TypeGet());

    if (tree->gtFlags & GTF_JCMP_TST)
    {
        size_t imm = static_cast<size_t>(op2->AsIntCon()->GetValue());

        assert(imm < EA_SIZE(attr) * 8);

        instruction ins = (tree->gtFlags & GTF_JCMP_EQ) ? INS_tbz : INS_tbnz;

        GetEmitter()->emitIns_J_R_I(ins, attr, block->bbJumpDest, reg, static_cast<int>(imm));
    }
    else
    {
        assert(op2->IsIntegralConst(0));

        instruction ins = (tree->gtFlags & GTF_JCMP_EQ) ? INS_cbz : INS_cbnz;

        GetEmitter()->emitIns_J_R(ins, attr, block->bbJumpDest, reg);
    }
}

// Return offset from the stack pointer (Initial-SP) to the frame pointer. The frame pointer
// will point to the saved frame pointer slot (i.e., there will be frame pointer chaining).
int CodeGenInterface::genSPtoFPdelta() const
{
    assert(isFramePointerUsed());
    int delta = -1; // initialization to illegal value

    if (IsSaveFpLrWithAllCalleeSavedRegisters())
    {
        // The saved frame pointer is at the top of the frame, just beneath the saved varargs register space and the
        // saved LR.
        delta = genTotalFrameSize() - (compiler->info.compIsVarArgs ? MAX_REG_ARG * REGSIZE_BYTES : 0) -
                2 /* FP, LR */ * REGSIZE_BYTES;
    }
    else
    {
        // We place the saved frame pointer immediately above the outgoing argument space.
        delta = static_cast<int>(outgoingArgSpaceSize);
    }

    assert(delta >= 0);
    return delta;
}

// Return the total size of the stack frame, including local size,
// callee-saved register size, etc.
int CodeGenInterface::genTotalFrameSize() const
{
    assert(calleeRegsPushed != UINT_MAX);

    // For varargs functions, we home all the incoming register arguments. They are not
    // included in the compCalleeRegsPushed count. This is like prespill on ARM32, but
    // since we don't use "push" instructions to save them, we don't have to do the
    // save of these varargs register arguments as the first thing in the prolog.

    int totalFrameSize = (compiler->info.compIsVarArgs ? MAX_REG_ARG * REGSIZE_BYTES : 0) +
                         calleeRegsPushed * REGSIZE_BYTES + lclFrameSize;

    assert(totalFrameSize >= 0);
    return totalFrameSize;
}

// Return the offset from Caller-SP to the frame pointer.
// This number is going to be negative, since the Caller-SP is at a higher
// address than the frame pointer.
// There must be a frame pointer to call this function!
int CodeGenInterface::genCallerSPtoFPdelta() const
{
    assert(isFramePointerUsed());
    int callerSPtoFPdelta;

    callerSPtoFPdelta = genCallerSPtoInitialSPdelta() + genSPtoFPdelta();

    assert(callerSPtoFPdelta <= 0);
    return callerSPtoFPdelta;
}

// Returns the offset from Caller-SP to Initial SP.
// This number will be negative.
int CodeGenInterface::genCallerSPtoInitialSPdelta() const
{
    int callerSPtoSPdelta = 0;

    callerSPtoSPdelta -= genTotalFrameSize();

    assert(callerSPtoSPdelta <= 0);
    return callerSPtoSPdelta;
}

void CodeGenInterface::SetSaveFpLrWithAllCalleeSavedRegisters(bool value)
{
    JITDUMP("Setting genSaveFpLrWithAllCalleeSavedRegisters to %s\n", dspBool(value));
    genSaveFpLrWithAllCalleeSavedRegisters = value;
}

bool CodeGenInterface::IsSaveFpLrWithAllCalleeSavedRegisters() const
{
    return genSaveFpLrWithAllCalleeSavedRegisters;
}

void CodeGen::genEmitHelperCall(CorInfoHelpFunc helper, emitAttr retSize, regNumber callTargetReg /*= REG_NA */)
{
    void* addr  = nullptr;
    void* pAddr = nullptr;

    emitter::EmitCallType callType = emitter::EC_FUNC_TOKEN;
    addr                           = compiler->compGetHelperFtn(helper, &pAddr);
    regNumber callTarget           = REG_NA;

    if (addr == nullptr)
    {
        // This is call to a runtime helper.
        // adrp x, [reloc:rel page addr]
        // add x, x, [reloc:page offset]
        // ldr x, [x]
        // br x

        if (callTargetReg == REG_NA)
        {
            // If a callTargetReg has not been explicitly provided, we will use REG_DEFAULT_HELPER_CALL_TARGET, but
            // this is only a valid assumption if the helper call is known to kill REG_DEFAULT_HELPER_CALL_TARGET.
            callTargetReg = REG_DEFAULT_HELPER_CALL_TARGET;
        }

        regMaskTP callTargetMask = genRegMask(callTargetReg);
        regMaskTP callKillSet    = compiler->compHelperCallKillSet(helper);

        // assert that all registers in callTargetMask are in the callKillSet
        noway_assert((callTargetMask & callKillSet) == callTargetMask);

        callTarget = callTargetReg;

        // adrp + add with relocations will be emitted
        GetEmitter()->emitIns_R_AI(INS_adrp, EA_PTR_DSP_RELOC, callTarget,
                                   (ssize_t)pAddr DEBUGARG((size_t)Compiler::eeFindHelper(helper))
                                       DEBUGARG(GTF_ICON_METHOD_HDL));
        GetEmitter()->emitIns_R_R(INS_ldr, EA_PTRSIZE, callTarget, callTarget);
        callType = emitter::EC_INDIR_R;
    }

    // clang-format off
    GetEmitter()->emitIns_Call(
        callType,
        Compiler::eeFindHelper(helper)
        DEBUGARG(nullptr),
        addr,
        retSize, EA_UNKNOWN,
        callTarget,
        false);
    // clang-format on
}

// Save the upper half of a TYP_SIMD16 vector to the given register, if any, or to memory.
// The upper half of all SIMD registers are volatile, even the callee-save registers.
// When a 16-byte SIMD value is live across a call, the register allocator will use this intrinsic
// to cause the upper half to be saved.  It will first attempt to find another, unused, callee-save
// register. If such a register cannot be found, it will save it to an available caller-save register.
// In that case, this node's register will be marked SPILL, which will cause this method to save
// the upper half to the lclVar's home location.
void CodeGen::genSIMDUpperSpill(GenTreeUnOp* node)
{
    GenTree* op1 = node->GetOp(0);
    assert(op1->OperIs(GT_LCL_VAR) && op1->TypeIs(TYP_SIMD12, TYP_SIMD16));

    regNumber srcReg = genConsumeReg(op1);
    assert(srcReg != REG_NA);
    regNumber dstReg = node->GetRegNum();
    assert(dstReg != REG_NA);

    GetEmitter()->emitIns_R_R_I_I(INS_mov, EA_8BYTE, dstReg, srcReg, 0, 1);

    if (node->IsRegSpill(0))
    {
        unsigned lclNum = op1->AsLclVar()->GetLclNum();
        assert(compiler->lvaGetDesc(lclNum)->lvOnFrame);

        GetEmitter()->emitIns_S_R(INS_str, EA_8BYTE, dstReg, lclNum, 8);
    }
    else
    {
        genProduceReg(node);
    }
}

// Restore the upper half of a TYP_SIMD16 vector to the given register, if any, or to memory.
// For consistency with genSIMDIntrinsicUpperSave, and to ensure that lclVar nodes always
// have their home register, this node has its targetReg on the lclVar child, and its source
// on the simdNode.
// Regarding spill, please see the note above on genSIMDIntrinsicUpperSave.  If we have spilled
// an upper-half to the lclVar's home location, this node's register will be marked SPILLED.
void CodeGen::genSIMDUpperUnspill(GenTreeUnOp* node)
{
    GenTree* op1 = node->GetOp(0);
    assert(op1->OperIs(GT_LCL_VAR) && op1->TypeIs(TYP_SIMD12, TYP_SIMD16));

    regNumber srcReg = node->GetRegNum();
    assert(srcReg != REG_NA);
    regNumber dstReg = genConsumeReg(op1);
    assert(dstReg != REG_NA);

    if (node->IsRegSpilled(0))
    {
        unsigned lclNum = op1->AsLclVar()->GetLclNum();
        assert(compiler->lvaGetDesc(lclNum)->lvOnFrame);

        GetEmitter()->emitIns_R_S(INS_ldr, EA_8BYTE, srcReg, lclNum, 8);
    }

    GetEmitter()->emitIns_R_R_I_I(INS_mov, EA_8BYTE, dstReg, srcReg, 1, 0);
}

void CodeGen::genStoreSIMD12(const GenAddrMode& dst, GenTree* value, regNumber tmpReg)
{
    if (value->IsHWIntrinsicZero())
    {
        inst_AM_R(INS_str, EA_8BYTE, REG_ZR, dst, 0);
        inst_AM_R(INS_str, EA_4BYTE, REG_ZR, dst, 8);
        return;
    }

    if (value->isContained())
    {
        GenAddrMode src(value, this);
        inst_R_AM(INS_ldr, EA_8BYTE, tmpReg, src, 0);
        inst_AM_R(INS_str, EA_8BYTE, tmpReg, dst, 0);
        inst_R_AM(INS_ldr, EA_4BYTE, tmpReg, src, 8);
        inst_AM_R(INS_str, EA_4BYTE, tmpReg, dst, 8);
        return;
    }

    regNumber valueReg = genConsumeReg(value);

    inst_AM_R(INS_str, EA_8BYTE, valueReg, dst, 0);
    GetEmitter()->emitIns_R_R_I(INS_mov, EA_4BYTE, tmpReg, valueReg, 2);
    inst_AM_R(INS_str, EA_4BYTE, tmpReg, dst, 8);
}

void CodeGen::LoadSIMD12(GenTree* load)
{
    GenAddrMode src(load, this);

    regNumber tmpReg = load->GetSingleTempReg();
    regNumber dstReg = load->GetRegNum();

    assert(tmpReg != dstReg);

    inst_R_AM(INS_ldr, EA_8BYTE, dstReg, src, 0);
    inst_R_AM(INS_ldr, EA_4BYTE, tmpReg, src, 8);
    GetEmitter()->emitIns_R_R_I(INS_mov, EA_4BYTE, dstReg, tmpReg, 2);
}

#ifdef PROFILING_SUPPORTED

void CodeGen::PrologProfilingEnterCallback(regNumber initReg, bool* pInitRegZeroed)
{
    assert(generatingProlog);

    if (!compiler->compIsProfilerHookNeeded())
    {
        return;
    }

    if (compiler->compProfilerMethHndIndirected)
    {
        instGen_Set_Reg_To_Imm(EA_PTR_DSP_RELOC, REG_PROFILER_ENTER_ARG_FUNC_ID,
                               reinterpret_cast<ssize_t>(compiler->compProfilerMethHnd));
        GetEmitter()->emitIns_R_R(INS_ldr, EA_PTRSIZE, REG_PROFILER_ENTER_ARG_FUNC_ID, REG_PROFILER_ENTER_ARG_FUNC_ID);
    }
    else
    {
        instGen_Set_Reg_To_Imm(EA_8BYTE, REG_PROFILER_ENTER_ARG_FUNC_ID,
                               reinterpret_cast<ssize_t>(compiler->compProfilerMethHnd));
    }

    int callerSPOffset = compiler->lvaToCallerSPRelativeOffset(0, isFramePointerUsed());
    genInstrWithConstant(INS_add, EA_PTRSIZE, REG_PROFILER_ENTER_ARG_CALLER_SP, genFramePointerReg(),
                         (ssize_t)(-callerSPOffset), REG_PROFILER_ENTER_ARG_CALLER_SP);

    genEmitHelperCall(CORINFO_HELP_PROF_FCN_ENTER);

    if ((genRegMask(initReg) & RBM_PROFILER_ENTER_TRASH) != RBM_NONE)
    {
        *pInitRegZeroed = false;
    }
}

void CodeGen::genProfilingLeaveCallback(CorInfoHelpFunc helper)
{
    assert((helper == CORINFO_HELP_PROF_FCN_LEAVE) || (helper == CORINFO_HELP_PROF_FCN_TAILCALL));

    if (!compiler->compIsProfilerHookNeeded())
    {
        return;
    }

    compiler->info.compProfilerCallback = true;

    if (compiler->compProfilerMethHndIndirected)
    {
        instGen_Set_Reg_To_Imm(EA_PTR_DSP_RELOC, REG_PROFILER_LEAVE_ARG_FUNC_ID,
                               reinterpret_cast<ssize_t>(compiler->compProfilerMethHnd));
        GetEmitter()->emitIns_R_R(INS_ldr, EA_PTRSIZE, REG_PROFILER_LEAVE_ARG_FUNC_ID, REG_PROFILER_LEAVE_ARG_FUNC_ID);
    }
    else
    {
        instGen_Set_Reg_To_Imm(EA_8BYTE, REG_PROFILER_LEAVE_ARG_FUNC_ID,
                               reinterpret_cast<ssize_t>(compiler->compProfilerMethHnd));
    }

    liveness.RemoveGCRegs(RBM_PROFILER_LEAVE_ARG_FUNC_ID);

    int callerSPOffset = compiler->lvaToCallerSPRelativeOffset(0, isFramePointerUsed());
    genInstrWithConstant(INS_add, EA_PTRSIZE, REG_PROFILER_LEAVE_ARG_CALLER_SP, genFramePointerReg(),
                         (ssize_t)(-callerSPOffset), REG_PROFILER_LEAVE_ARG_CALLER_SP);

    liveness.RemoveGCRegs(RBM_PROFILER_LEAVE_ARG_CALLER_SP);

    genEmitHelperCall(helper);
}

#endif // PROFILING_SUPPORTED

// Uncomment "#define ALL_ARM64_EMITTER_UNIT_TESTS" to run all the unit tests here.
// After adding a unit test, and verifying it works, put it under this #ifdef, so we
// don't see it run every time.
//#define ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef DEBUG
void CodeGen::genArm64EmitterUnitTests()
{
    if (!verbose)
    {
        return;
    }

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    // Mark the "fake" instructions in the output.
    printf("*************** In genArm64EmitterUnitTests()\n");

    emitter* theEmitter = GetEmitter();
#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    // We use this:
    //      genDefineTempLabel(genCreateTempLabel());
    // to create artificial labels to help separate groups of tests.

    //
    // Loads/Stores basic general register
    //

    genDefineTempLabel(genCreateTempLabel());

    // ldr/str Xt, [reg]
    theEmitter->emitIns_R_R(INS_ldr, EA_8BYTE, REG_R8, REG_R9);
    theEmitter->emitIns_R_R(INS_ldrb, EA_1BYTE, REG_R8, REG_R9);
    theEmitter->emitIns_R_R(INS_ldrh, EA_2BYTE, REG_R8, REG_R9);
    theEmitter->emitIns_R_R(INS_str, EA_8BYTE, REG_R8, REG_R9);
    theEmitter->emitIns_R_R(INS_strb, EA_1BYTE, REG_R8, REG_R9);
    theEmitter->emitIns_R_R(INS_strh, EA_2BYTE, REG_R8, REG_R9);

    // ldr/str Wt, [reg]
    theEmitter->emitIns_R_R(INS_ldr, EA_4BYTE, REG_R8, REG_R9);
    theEmitter->emitIns_R_R(INS_ldrb, EA_1BYTE, REG_R8, REG_R9);
    theEmitter->emitIns_R_R(INS_ldrh, EA_2BYTE, REG_R8, REG_R9);
    theEmitter->emitIns_R_R(INS_str, EA_4BYTE, REG_R8, REG_R9);
    theEmitter->emitIns_R_R(INS_strb, EA_1BYTE, REG_R8, REG_R9);
    theEmitter->emitIns_R_R(INS_strh, EA_2BYTE, REG_R8, REG_R9);

    theEmitter->emitIns_R_R(INS_ldrsb, EA_4BYTE, REG_R8, REG_R9); // target Wt
    theEmitter->emitIns_R_R(INS_ldrsh, EA_4BYTE, REG_R8, REG_R9); // target Wt
    theEmitter->emitIns_R_R(INS_ldrsb, EA_8BYTE, REG_R8, REG_R9); // target Xt
    theEmitter->emitIns_R_R(INS_ldrsh, EA_8BYTE, REG_R8, REG_R9); // target Xt
    theEmitter->emitIns_R_R(INS_ldrsw, EA_8BYTE, REG_R8, REG_R9); // target Xt

    theEmitter->emitIns_R_R_I(INS_ldurb, EA_4BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_ldurh, EA_4BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_sturb, EA_4BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_sturh, EA_4BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_ldursb, EA_4BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_ldursb, EA_8BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_ldursh, EA_4BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_ldursh, EA_8BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_ldur, EA_8BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_ldur, EA_4BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_stur, EA_4BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_stur, EA_8BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_ldursw, EA_8BYTE, REG_R8, REG_R9, 1);

    // SP and ZR tests
    theEmitter->emitIns_R_R_I(INS_ldur, EA_8BYTE, REG_R8, REG_SP, 1);
    theEmitter->emitIns_R_R_I(INS_ldurb, EA_8BYTE, REG_ZR, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_ldurh, EA_8BYTE, REG_ZR, REG_SP, 1);

    // scaled
    theEmitter->emitIns_R_R_I(INS_ldrb, EA_1BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_ldrh, EA_2BYTE, REG_R8, REG_R9, 2);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_4BYTE, REG_R8, REG_R9, 4);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_8BYTE, REG_R8, REG_R9, 8);

    // pre-/post-indexed (unscaled)
    theEmitter->emitIns_R_R_I(INS_ldr, EA_4BYTE, REG_R8, REG_R9, 1, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_4BYTE, REG_R8, REG_R9, 1, INS_OPTS_PRE_INDEX);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_8BYTE, REG_R8, REG_R9, 1, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_8BYTE, REG_R8, REG_R9, 1, INS_OPTS_PRE_INDEX);

    // ldar/stlr Rt, [reg]
    theEmitter->emitIns_R_R(INS_ldar, EA_8BYTE, REG_R9, REG_R8);
    theEmitter->emitIns_R_R(INS_ldar, EA_4BYTE, REG_R7, REG_R10);
    theEmitter->emitIns_R_R(INS_ldarb, EA_4BYTE, REG_R5, REG_R11);
    theEmitter->emitIns_R_R(INS_ldarh, EA_4BYTE, REG_R5, REG_R12);

    theEmitter->emitIns_R_R(INS_stlr, EA_8BYTE, REG_R9, REG_R8);
    theEmitter->emitIns_R_R(INS_stlr, EA_4BYTE, REG_R7, REG_R13);
    theEmitter->emitIns_R_R(INS_stlrb, EA_4BYTE, REG_R5, REG_R14);
    theEmitter->emitIns_R_R(INS_stlrh, EA_4BYTE, REG_R3, REG_R15);

    // ldaxr Rt, [reg]
    theEmitter->emitIns_R_R(INS_ldaxr, EA_8BYTE, REG_R9, REG_R8);
    theEmitter->emitIns_R_R(INS_ldaxr, EA_4BYTE, REG_R7, REG_R10);
    theEmitter->emitIns_R_R(INS_ldaxrb, EA_4BYTE, REG_R5, REG_R11);
    theEmitter->emitIns_R_R(INS_ldaxrh, EA_4BYTE, REG_R5, REG_R12);

    // ldxr Rt, [reg]
    theEmitter->emitIns_R_R(INS_ldxr, EA_8BYTE, REG_R9, REG_R8);
    theEmitter->emitIns_R_R(INS_ldxr, EA_4BYTE, REG_R7, REG_R10);
    theEmitter->emitIns_R_R(INS_ldxrb, EA_4BYTE, REG_R5, REG_R11);
    theEmitter->emitIns_R_R(INS_ldxrh, EA_4BYTE, REG_R5, REG_R12);

    // stxr Ws, Rt, [reg]
    theEmitter->emitIns_R_R_R(INS_stxr, EA_8BYTE, REG_R1, REG_R9, REG_R8);
    theEmitter->emitIns_R_R_R(INS_stxr, EA_4BYTE, REG_R3, REG_R7, REG_R13);
    theEmitter->emitIns_R_R_R(INS_stxrb, EA_4BYTE, REG_R8, REG_R5, REG_R14);
    theEmitter->emitIns_R_R_R(INS_stxrh, EA_4BYTE, REG_R12, REG_R3, REG_R15);

    // stlxr Ws, Rt, [reg]
    theEmitter->emitIns_R_R_R(INS_stlxr, EA_8BYTE, REG_R1, REG_R9, REG_R8);
    theEmitter->emitIns_R_R_R(INS_stlxr, EA_4BYTE, REG_R3, REG_R7, REG_R13);
    theEmitter->emitIns_R_R_R(INS_stlxrb, EA_4BYTE, REG_R8, REG_R5, REG_R14);
    theEmitter->emitIns_R_R_R(INS_stlxrh, EA_4BYTE, REG_R12, REG_R3, REG_R15);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // Loads to and Stores from one, two, three, or four SIMD&FP registers
    //

    genDefineTempLabel(genCreateTempLabel());

    // ld1 {Vt}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_ld1, EA_8BYTE, REG_V0, REG_R1, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_ld1, EA_16BYTE, REG_V2, REG_R3, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_ld1, EA_8BYTE, REG_V4, REG_R5, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_ld1, EA_16BYTE, REG_V6, REG_R7, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_ld1, EA_8BYTE, REG_V8, REG_R9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_ld1, EA_16BYTE, REG_V10, REG_R11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_ld1, EA_8BYTE, REG_V12, REG_R13, INS_OPTS_1D);
    theEmitter->emitIns_R_R(INS_ld1, EA_16BYTE, REG_V14, REG_R15, INS_OPTS_2D);

    // ld1 {Vt, Vt2}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_ld1_2regs, EA_8BYTE, REG_V0, REG_R2, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_ld1_2regs, EA_16BYTE, REG_V3, REG_R5, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_ld1_2regs, EA_8BYTE, REG_V6, REG_R8, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_ld1_2regs, EA_16BYTE, REG_V9, REG_R11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_ld1_2regs, EA_8BYTE, REG_V12, REG_R14, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_ld1_2regs, EA_16BYTE, REG_V15, REG_R17, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_ld1_2regs, EA_8BYTE, REG_V18, REG_R20, INS_OPTS_1D);
    theEmitter->emitIns_R_R(INS_ld1_2regs, EA_16BYTE, REG_V21, REG_R23, INS_OPTS_2D);

    // ld1 {Vt, Vt2, Vt3}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_ld1_3regs, EA_8BYTE, REG_V0, REG_R3, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_ld1_3regs, EA_16BYTE, REG_V4, REG_R7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_ld1_3regs, EA_8BYTE, REG_V8, REG_R11, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_ld1_3regs, EA_16BYTE, REG_V12, REG_R15, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_ld1_3regs, EA_8BYTE, REG_V16, REG_R19, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_ld1_3regs, EA_16BYTE, REG_V20, REG_R23, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_ld1_3regs, EA_8BYTE, REG_V24, REG_R27, INS_OPTS_1D);
    theEmitter->emitIns_R_R(INS_ld1_3regs, EA_16BYTE, REG_V28, REG_SP, INS_OPTS_2D);

    // ld1 {Vt, Vt2, Vt3, Vt4}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_ld1_4regs, EA_8BYTE, REG_V0, REG_R4, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_ld1_4regs, EA_16BYTE, REG_V5, REG_R9, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_ld1_4regs, EA_8BYTE, REG_V10, REG_R14, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_ld1_4regs, EA_16BYTE, REG_V15, REG_R19, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_ld1_4regs, EA_8BYTE, REG_V20, REG_R24, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_ld1_4regs, EA_16BYTE, REG_V25, REG_R29, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_ld1_4regs, EA_8BYTE, REG_V30, REG_R2, INS_OPTS_1D);
    theEmitter->emitIns_R_R(INS_ld1_4regs, EA_16BYTE, REG_V3, REG_R7, INS_OPTS_2D);

    // ld2 {Vt, Vt2}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_ld2, EA_8BYTE, REG_V0, REG_R2, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_ld2, EA_16BYTE, REG_V3, REG_R5, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_ld2, EA_8BYTE, REG_V6, REG_R8, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_ld2, EA_16BYTE, REG_V9, REG_R11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_ld2, EA_8BYTE, REG_V12, REG_R14, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_ld2, EA_16BYTE, REG_V15, REG_R17, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_ld2, EA_16BYTE, REG_V18, REG_R20, INS_OPTS_2D);

    // ld3 {Vt, Vt2, Vt3}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_ld3, EA_8BYTE, REG_V0, REG_R3, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_ld3, EA_16BYTE, REG_V4, REG_R7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_ld3, EA_8BYTE, REG_V8, REG_R11, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_ld3, EA_16BYTE, REG_V12, REG_R15, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_ld3, EA_8BYTE, REG_V16, REG_R19, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_ld3, EA_16BYTE, REG_V20, REG_R23, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_ld3, EA_16BYTE, REG_V24, REG_R27, INS_OPTS_2D);

    // ld4 {Vt, Vt2, Vt3, Vt4}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_ld4, EA_8BYTE, REG_V0, REG_R4, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_ld4, EA_16BYTE, REG_V5, REG_R9, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_ld4, EA_8BYTE, REG_V10, REG_R14, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_ld4, EA_16BYTE, REG_V15, REG_R19, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_ld4, EA_8BYTE, REG_V20, REG_R24, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_ld4, EA_16BYTE, REG_V25, REG_R29, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_ld4, EA_16BYTE, REG_V30, REG_R2, INS_OPTS_2D);

    // st1 {Vt}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_st1, EA_8BYTE, REG_V0, REG_R1, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_st1, EA_16BYTE, REG_V2, REG_R3, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_st1, EA_8BYTE, REG_V4, REG_R5, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_st1, EA_16BYTE, REG_V6, REG_R7, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_st1, EA_8BYTE, REG_V8, REG_R9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_st1, EA_16BYTE, REG_V10, REG_R11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_st1, EA_8BYTE, REG_V12, REG_R13, INS_OPTS_1D);
    theEmitter->emitIns_R_R(INS_st1, EA_16BYTE, REG_V14, REG_R15, INS_OPTS_2D);

    // st1 {Vt, Vt2}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_st1_2regs, EA_8BYTE, REG_V0, REG_R2, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_st1_2regs, EA_16BYTE, REG_V3, REG_R5, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_st1_2regs, EA_8BYTE, REG_V6, REG_R8, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_st1_2regs, EA_16BYTE, REG_V9, REG_R11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_st1_2regs, EA_8BYTE, REG_V12, REG_R14, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_st1_2regs, EA_16BYTE, REG_V15, REG_R17, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_st1_2regs, EA_8BYTE, REG_V18, REG_R20, INS_OPTS_1D);
    theEmitter->emitIns_R_R(INS_st1_2regs, EA_16BYTE, REG_V21, REG_R23, INS_OPTS_2D);

    // st1 {Vt, Vt2, Vt3}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_st1_3regs, EA_8BYTE, REG_V0, REG_R3, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_st1_3regs, EA_16BYTE, REG_V4, REG_R7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_st1_3regs, EA_8BYTE, REG_V8, REG_R11, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_st1_3regs, EA_16BYTE, REG_V12, REG_R15, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_st1_3regs, EA_8BYTE, REG_V16, REG_R19, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_st1_3regs, EA_16BYTE, REG_V20, REG_R23, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_st1_3regs, EA_8BYTE, REG_V24, REG_R27, INS_OPTS_1D);
    theEmitter->emitIns_R_R(INS_st1_3regs, EA_16BYTE, REG_V28, REG_SP, INS_OPTS_2D);

    // st1 {Vt, Vt2, Vt3, Vt4}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_st1_4regs, EA_8BYTE, REG_V0, REG_R4, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_st1_4regs, EA_16BYTE, REG_V5, REG_R9, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_st1_4regs, EA_8BYTE, REG_V10, REG_R14, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_st1_4regs, EA_16BYTE, REG_V15, REG_R19, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_st1_4regs, EA_8BYTE, REG_V20, REG_R24, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_st1_4regs, EA_16BYTE, REG_V25, REG_R29, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_st1_4regs, EA_8BYTE, REG_V30, REG_R2, INS_OPTS_1D);
    theEmitter->emitIns_R_R(INS_st1_4regs, EA_16BYTE, REG_V3, REG_R7, INS_OPTS_2D);

    // st2 {Vt, Vt2}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_st2, EA_8BYTE, REG_V0, REG_R2, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_st2, EA_16BYTE, REG_V3, REG_R5, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_st2, EA_8BYTE, REG_V6, REG_R8, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_st2, EA_16BYTE, REG_V9, REG_R11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_st2, EA_8BYTE, REG_V12, REG_R14, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_st2, EA_16BYTE, REG_V15, REG_R17, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_st2, EA_16BYTE, REG_V18, REG_R20, INS_OPTS_2D);

    // st3 {Vt, Vt2, Vt3}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_st3, EA_8BYTE, REG_V0, REG_R3, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_st3, EA_16BYTE, REG_V4, REG_R7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_st3, EA_8BYTE, REG_V8, REG_R11, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_st3, EA_16BYTE, REG_V12, REG_R15, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_st3, EA_8BYTE, REG_V16, REG_R19, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_st3, EA_16BYTE, REG_V20, REG_R23, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_st3, EA_16BYTE, REG_V24, REG_R27, INS_OPTS_2D);

    // st4 {Vt, Vt2, Vt3, Vt4}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_st4, EA_8BYTE, REG_V0, REG_R4, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_st4, EA_16BYTE, REG_V5, REG_R9, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_st4, EA_8BYTE, REG_V10, REG_R14, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_st4, EA_16BYTE, REG_V15, REG_R19, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_st4, EA_8BYTE, REG_V20, REG_R24, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_st4, EA_16BYTE, REG_V25, REG_R29, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_st4, EA_16BYTE, REG_V30, REG_R2, INS_OPTS_2D);

    // ld1r {Vt}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_ld1r, EA_8BYTE, REG_V0, REG_R1, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_ld1r, EA_16BYTE, REG_V2, REG_R3, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_ld1r, EA_8BYTE, REG_V4, REG_R5, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_ld1r, EA_16BYTE, REG_V6, REG_R7, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_ld1r, EA_8BYTE, REG_V8, REG_R9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_ld1r, EA_16BYTE, REG_V10, REG_R11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_ld1r, EA_8BYTE, REG_V12, REG_R13, INS_OPTS_1D);
    theEmitter->emitIns_R_R(INS_ld1r, EA_16BYTE, REG_V14, REG_R15, INS_OPTS_2D);

    // ld2r {Vt, Vt2}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_ld2r, EA_8BYTE, REG_V0, REG_R2, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_ld2r, EA_16BYTE, REG_V3, REG_R5, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_ld2r, EA_8BYTE, REG_V6, REG_R8, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_ld2r, EA_16BYTE, REG_V9, REG_R11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_ld2r, EA_8BYTE, REG_V12, REG_R14, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_ld2r, EA_16BYTE, REG_V15, REG_R17, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_ld2r, EA_8BYTE, REG_V18, REG_R20, INS_OPTS_1D);
    theEmitter->emitIns_R_R(INS_ld2r, EA_16BYTE, REG_V21, REG_R23, INS_OPTS_2D);

    // ld3r {Vt, Vt2, Vt3}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_ld3r, EA_8BYTE, REG_V0, REG_R3, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_ld3r, EA_16BYTE, REG_V4, REG_R7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_ld3r, EA_8BYTE, REG_V8, REG_R11, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_ld3r, EA_16BYTE, REG_V12, REG_R15, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_ld3r, EA_8BYTE, REG_V16, REG_R19, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_ld3r, EA_16BYTE, REG_V20, REG_R23, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_ld3r, EA_8BYTE, REG_V24, REG_R27, INS_OPTS_1D);
    theEmitter->emitIns_R_R(INS_ld3r, EA_16BYTE, REG_V28, REG_SP, INS_OPTS_2D);

    // ld4r {Vt, Vt2, Vt3, Vt4}, [Xn|SP]
    theEmitter->emitIns_R_R(INS_ld4r, EA_8BYTE, REG_V0, REG_R4, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_ld4r, EA_16BYTE, REG_V5, REG_R9, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_ld4r, EA_8BYTE, REG_V10, REG_R14, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_ld4r, EA_16BYTE, REG_V15, REG_R19, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_ld4r, EA_8BYTE, REG_V20, REG_R24, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_ld4r, EA_16BYTE, REG_V25, REG_R29, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_ld4r, EA_8BYTE, REG_V30, REG_R2, INS_OPTS_1D);
    theEmitter->emitIns_R_R(INS_ld4r, EA_16BYTE, REG_V3, REG_R7, INS_OPTS_2D);

    // tbl Vd, {Vt}, Vm
    theEmitter->emitIns_R_R_R(INS_tbl, EA_8BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_tbl, EA_16BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_16B);

    // tbx Vd, {Vt}, Vm
    theEmitter->emitIns_R_R_R(INS_tbx, EA_8BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_tbx, EA_16BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_16B);

    // tbl Vd, {Vt, Vt2}, Vm
    theEmitter->emitIns_R_R_R(INS_tbl_2regs, EA_8BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_tbl_2regs, EA_16BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_16B);

    // tbx Vd, {Vt, Vt2}, Vm
    theEmitter->emitIns_R_R_R(INS_tbx_2regs, EA_8BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_tbx_2regs, EA_16BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_16B);

    // tbl Vd, {Vt, Vt2, Vt3}, Vm
    theEmitter->emitIns_R_R_R(INS_tbl_3regs, EA_8BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_tbl_3regs, EA_16BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_16B);

    // tbx Vd, {Vt, Vt2, Vt3}, Vm
    theEmitter->emitIns_R_R_R(INS_tbx_3regs, EA_8BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_tbx_3regs, EA_16BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_16B);

    // tbl Vd, {Vt, Vt2, Vt3, Vt4}, Vm
    theEmitter->emitIns_R_R_R(INS_tbl_4regs, EA_8BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_tbl_4regs, EA_16BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_16B);

    // tbx Vd, {Vt, Vt2, Vt3, Vt4}, Vm
    theEmitter->emitIns_R_R_R(INS_tbx_4regs, EA_8BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_tbx_4regs, EA_16BYTE, REG_V0, REG_V1, REG_V6, INS_OPTS_16B);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // Loads to and Stores from one, two, three, or four SIMD&FP registers
    //

    genDefineTempLabel(genCreateTempLabel());

    // ld1 {Vt}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_ld1, EA_8BYTE, REG_V0, REG_R1, REG_R2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_ld1, EA_16BYTE, REG_V3, REG_R4, REG_R5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_ld1, EA_8BYTE, REG_V6, REG_R7, REG_R8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_ld1, EA_16BYTE, REG_V9, REG_R10, REG_R11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_ld1, EA_8BYTE, REG_V12, REG_R13, REG_R14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_ld1, EA_16BYTE, REG_V15, REG_R16, REG_R17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_ld1, EA_8BYTE, REG_V18, REG_R19, REG_R20, INS_OPTS_1D);
    theEmitter->emitIns_R_R_R(INS_ld1, EA_16BYTE, REG_V21, REG_R22, REG_R23, INS_OPTS_2D);

    // ld1 {Vt, Vt2}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_ld1_2regs, EA_8BYTE, REG_V0, REG_R2, REG_R3, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_ld1_2regs, EA_16BYTE, REG_V4, REG_R6, REG_R7, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_ld1_2regs, EA_8BYTE, REG_V8, REG_R10, REG_R11, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_ld1_2regs, EA_16BYTE, REG_V12, REG_R14, REG_R15, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_ld1_2regs, EA_8BYTE, REG_V16, REG_R18, REG_R19, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_ld1_2regs, EA_16BYTE, REG_V20, REG_R22, REG_R23, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_ld1_2regs, EA_8BYTE, REG_V24, REG_R26, REG_R27, INS_OPTS_1D);
    theEmitter->emitIns_R_R_R(INS_ld1_2regs, EA_16BYTE, REG_V28, REG_SP, REG_R30, INS_OPTS_2D);

    // ld1 {Vt, Vt2, Vt3}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_ld1_3regs, EA_8BYTE, REG_V0, REG_R3, REG_R4, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_ld1_3regs, EA_16BYTE, REG_V5, REG_R8, REG_R9, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_ld1_3regs, EA_8BYTE, REG_V10, REG_R13, REG_R14, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_ld1_3regs, EA_16BYTE, REG_V15, REG_R18, REG_R19, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_ld1_3regs, EA_8BYTE, REG_V20, REG_R23, REG_R24, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_ld1_3regs, EA_16BYTE, REG_V25, REG_R28, REG_R29, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_ld1_3regs, EA_8BYTE, REG_V30, REG_R0, REG_R1, INS_OPTS_1D);
    theEmitter->emitIns_R_R_R(INS_ld1_3regs, EA_16BYTE, REG_V2, REG_R5, REG_R6, INS_OPTS_2D);

    // ld1 {Vt, Vt2, Vt3, Vt4}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_ld1_4regs, EA_8BYTE, REG_V0, REG_R4, REG_R5, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_ld1_4regs, EA_16BYTE, REG_V6, REG_R10, REG_R11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_ld1_4regs, EA_8BYTE, REG_V12, REG_R16, REG_R17, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_ld1_4regs, EA_16BYTE, REG_V18, REG_R22, REG_R23, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_ld1_4regs, EA_8BYTE, REG_V24, REG_R28, REG_R29, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_ld1_4regs, EA_16BYTE, REG_V30, REG_R2, REG_R3, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_ld1_4regs, EA_8BYTE, REG_V4, REG_R8, REG_R9, INS_OPTS_1D);
    theEmitter->emitIns_R_R_R(INS_ld1_4regs, EA_16BYTE, REG_V10, REG_R14, REG_R15, INS_OPTS_2D);

    // ld2 {Vt, Vt2}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_ld2, EA_8BYTE, REG_V0, REG_R2, REG_R3, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_ld2, EA_16BYTE, REG_V4, REG_R6, REG_R7, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_ld2, EA_8BYTE, REG_V8, REG_R10, REG_R11, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_ld2, EA_16BYTE, REG_V12, REG_R14, REG_R15, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_ld2, EA_8BYTE, REG_V16, REG_R18, REG_R19, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_ld2, EA_16BYTE, REG_V20, REG_R22, REG_R23, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_ld2, EA_16BYTE, REG_V24, REG_R26, REG_R27, INS_OPTS_2D);

    // ld3 {Vt, Vt2, Vt3}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_ld3, EA_8BYTE, REG_V0, REG_R3, REG_R4, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_ld3, EA_16BYTE, REG_V5, REG_R8, REG_R9, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_ld3, EA_8BYTE, REG_V10, REG_R13, REG_R14, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_ld3, EA_16BYTE, REG_V15, REG_R18, REG_R19, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_ld3, EA_8BYTE, REG_V20, REG_R23, REG_R24, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_ld3, EA_16BYTE, REG_V25, REG_R28, REG_R29, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_ld3, EA_16BYTE, REG_V30, REG_R0, REG_R1, INS_OPTS_2D);

    // ld4 {Vt, Vt2, Vt3, Vt4}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_ld4, EA_8BYTE, REG_V0, REG_R4, REG_R5, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_ld4, EA_16BYTE, REG_V6, REG_R10, REG_R11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_ld4, EA_8BYTE, REG_V12, REG_R16, REG_R17, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_ld4, EA_16BYTE, REG_V18, REG_R22, REG_R23, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_ld4, EA_8BYTE, REG_V24, REG_R28, REG_R29, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_ld4, EA_16BYTE, REG_V30, REG_R2, REG_R3, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_ld4, EA_16BYTE, REG_V4, REG_R8, REG_R9, INS_OPTS_2D);

    // st1 {Vt}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_st1, EA_8BYTE, REG_V0, REG_R1, REG_R2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_st1, EA_16BYTE, REG_V3, REG_R4, REG_R5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_st1, EA_8BYTE, REG_V6, REG_R7, REG_R8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_st1, EA_16BYTE, REG_V9, REG_R10, REG_R11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_st1, EA_8BYTE, REG_V12, REG_R13, REG_R14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_st1, EA_16BYTE, REG_V15, REG_R16, REG_R17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_st1, EA_8BYTE, REG_V18, REG_R19, REG_R20, INS_OPTS_1D);
    theEmitter->emitIns_R_R_R(INS_st1, EA_16BYTE, REG_V21, REG_R22, REG_R23, INS_OPTS_2D);

    // st1 {Vt, Vt2}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_st1_2regs, EA_8BYTE, REG_V0, REG_R2, REG_R3, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_st1_2regs, EA_16BYTE, REG_V4, REG_R6, REG_R7, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_st1_2regs, EA_8BYTE, REG_V8, REG_R10, REG_R11, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_st1_2regs, EA_16BYTE, REG_V12, REG_R14, REG_R15, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_st1_2regs, EA_8BYTE, REG_V16, REG_R18, REG_R19, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_st1_2regs, EA_16BYTE, REG_V20, REG_R22, REG_R23, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_st1_2regs, EA_8BYTE, REG_V24, REG_R26, REG_R27, INS_OPTS_1D);
    theEmitter->emitIns_R_R_R(INS_st1_2regs, EA_16BYTE, REG_V28, REG_SP, REG_R30, INS_OPTS_2D);

    // st1 {Vt, Vt2, Vt3}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_st1_3regs, EA_8BYTE, REG_V0, REG_R3, REG_R4, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_st1_3regs, EA_16BYTE, REG_V5, REG_R8, REG_R9, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_st1_3regs, EA_8BYTE, REG_V10, REG_R13, REG_R14, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_st1_3regs, EA_16BYTE, REG_V15, REG_R18, REG_R19, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_st1_3regs, EA_8BYTE, REG_V20, REG_R23, REG_R24, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_st1_3regs, EA_16BYTE, REG_V25, REG_R28, REG_R29, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_st1_3regs, EA_8BYTE, REG_V30, REG_R0, REG_R1, INS_OPTS_1D);
    theEmitter->emitIns_R_R_R(INS_st1_3regs, EA_16BYTE, REG_V2, REG_R5, REG_R6, INS_OPTS_2D);

    // st1 {Vt, Vt2, Vt3, Vt4}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_st1_4regs, EA_8BYTE, REG_V0, REG_R4, REG_R5, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_st1_4regs, EA_16BYTE, REG_V6, REG_R10, REG_R11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_st1_4regs, EA_8BYTE, REG_V12, REG_R16, REG_R17, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_st1_4regs, EA_16BYTE, REG_V18, REG_R22, REG_R23, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_st1_4regs, EA_8BYTE, REG_V24, REG_R28, REG_R29, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_st1_4regs, EA_16BYTE, REG_V30, REG_R2, REG_R3, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_st1_4regs, EA_8BYTE, REG_V4, REG_R8, REG_R9, INS_OPTS_1D);
    theEmitter->emitIns_R_R_R(INS_st1_4regs, EA_16BYTE, REG_V10, REG_R14, REG_R15, INS_OPTS_2D);

    // st2 {Vt, Vt2}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_st2, EA_8BYTE, REG_V0, REG_R2, REG_R3, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_st2, EA_16BYTE, REG_V4, REG_R6, REG_R7, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_st2, EA_8BYTE, REG_V8, REG_R10, REG_R11, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_st2, EA_16BYTE, REG_V12, REG_R14, REG_R15, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_st2, EA_8BYTE, REG_V16, REG_R18, REG_R19, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_st2, EA_16BYTE, REG_V20, REG_R22, REG_R23, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_st2, EA_16BYTE, REG_V24, REG_R26, REG_R27, INS_OPTS_2D);

    // st3 {Vt, Vt2, Vt3}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_st3, EA_8BYTE, REG_V0, REG_R3, REG_R4, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_st3, EA_16BYTE, REG_V5, REG_R8, REG_R9, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_st3, EA_8BYTE, REG_V10, REG_R13, REG_R14, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_st3, EA_16BYTE, REG_V15, REG_R18, REG_R19, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_st3, EA_8BYTE, REG_V20, REG_R23, REG_R24, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_st3, EA_16BYTE, REG_V25, REG_R28, REG_R29, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_st3, EA_16BYTE, REG_V30, REG_R0, REG_R1, INS_OPTS_2D);

    // st4 {Vt, Vt2, Vt3, Vt4}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_st4, EA_8BYTE, REG_V0, REG_R4, REG_R5, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_st4, EA_16BYTE, REG_V6, REG_R10, REG_R11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_st4, EA_8BYTE, REG_V12, REG_R16, REG_R17, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_st4, EA_16BYTE, REG_V18, REG_R22, REG_R23, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_st4, EA_8BYTE, REG_V24, REG_R28, REG_R29, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_st4, EA_16BYTE, REG_V30, REG_R2, REG_R3, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_st4, EA_16BYTE, REG_V4, REG_R8, REG_R9, INS_OPTS_2D);

    // ld1r {Vt}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_ld1r, EA_8BYTE, REG_V0, REG_R1, REG_R2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_ld1r, EA_16BYTE, REG_V3, REG_R4, REG_R5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_ld1r, EA_8BYTE, REG_V6, REG_R7, REG_R8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_ld1r, EA_16BYTE, REG_V9, REG_R10, REG_R11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_ld1r, EA_8BYTE, REG_V12, REG_R13, REG_R14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_ld1r, EA_16BYTE, REG_V15, REG_R16, REG_R17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_ld1r, EA_8BYTE, REG_V18, REG_R19, REG_R20, INS_OPTS_1D);
    theEmitter->emitIns_R_R_R(INS_ld1r, EA_16BYTE, REG_V21, REG_R22, REG_R23, INS_OPTS_2D);

    // ld2r {Vt, Vt2}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_ld2r, EA_8BYTE, REG_V0, REG_R2, REG_R3, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_ld2r, EA_16BYTE, REG_V4, REG_R6, REG_R7, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_ld2r, EA_8BYTE, REG_V8, REG_R10, REG_R11, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_ld2r, EA_16BYTE, REG_V12, REG_R14, REG_R15, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_ld2r, EA_8BYTE, REG_V16, REG_R18, REG_R19, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_ld2r, EA_16BYTE, REG_V20, REG_R22, REG_R23, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_ld2r, EA_8BYTE, REG_V24, REG_R26, REG_R27, INS_OPTS_1D);
    theEmitter->emitIns_R_R_R(INS_ld2r, EA_16BYTE, REG_V28, REG_SP, REG_R30, INS_OPTS_2D);

    // ld3r {Vt, Vt2, Vt3}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_ld3r, EA_8BYTE, REG_V0, REG_R3, REG_R4, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_ld3r, EA_16BYTE, REG_V5, REG_R8, REG_R9, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_ld3r, EA_8BYTE, REG_V10, REG_R13, REG_R14, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_ld3r, EA_16BYTE, REG_V15, REG_R18, REG_R19, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_ld3r, EA_8BYTE, REG_V20, REG_R23, REG_R24, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_ld3r, EA_16BYTE, REG_V25, REG_R28, REG_R29, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_ld3r, EA_8BYTE, REG_V30, REG_R0, REG_R1, INS_OPTS_1D);
    theEmitter->emitIns_R_R_R(INS_ld3r, EA_16BYTE, REG_V2, REG_R5, REG_R6, INS_OPTS_2D);

    // ld4r {Vt, Vt2, Vt3, Vt4}, [Xn|SP], Xm
    theEmitter->emitIns_R_R_R(INS_ld4r, EA_8BYTE, REG_V0, REG_R4, REG_R5, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_ld4r, EA_16BYTE, REG_V6, REG_R10, REG_R11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_ld4r, EA_8BYTE, REG_V12, REG_R16, REG_R17, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_ld4r, EA_16BYTE, REG_V18, REG_R22, REG_R23, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_ld4r, EA_8BYTE, REG_V24, REG_R28, REG_R29, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_ld4r, EA_16BYTE, REG_V30, REG_R2, REG_R3, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_ld4r, EA_8BYTE, REG_V4, REG_R8, REG_R9, INS_OPTS_1D);
    theEmitter->emitIns_R_R_R(INS_ld4r, EA_16BYTE, REG_V10, REG_R14, REG_R15, INS_OPTS_2D);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // Loads to and Stores from one, two, three, or four SIMD&FP registers
    //

    genDefineTempLabel(genCreateTempLabel());

    // ld1 {Vt}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_ld1, EA_8BYTE, REG_V0, REG_R1, 8, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_ld1, EA_16BYTE, REG_V2, REG_R3, 16, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_ld1, EA_8BYTE, REG_V4, REG_R5, 8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_ld1, EA_16BYTE, REG_V6, REG_R7, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_ld1, EA_8BYTE, REG_V8, REG_R9, 8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_ld1, EA_16BYTE, REG_V10, REG_R11, 16, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_ld1, EA_8BYTE, REG_V12, REG_R13, 8, INS_OPTS_1D);
    theEmitter->emitIns_R_R_I(INS_ld1, EA_16BYTE, REG_V14, REG_R15, 16, INS_OPTS_2D);

    // ld1 {Vt, Vt2}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_ld1_2regs, EA_8BYTE, REG_V0, REG_R2, 16, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_ld1_2regs, EA_16BYTE, REG_V3, REG_R5, 32, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_ld1_2regs, EA_8BYTE, REG_V6, REG_R8, 16, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_ld1_2regs, EA_16BYTE, REG_V9, REG_R11, 32, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_ld1_2regs, EA_8BYTE, REG_V12, REG_R14, 16, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_ld1_2regs, EA_16BYTE, REG_V15, REG_R17, 32, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_ld1_2regs, EA_8BYTE, REG_V18, REG_R20, 16, INS_OPTS_1D);
    theEmitter->emitIns_R_R_I(INS_ld1_2regs, EA_16BYTE, REG_V21, REG_R23, 32, INS_OPTS_2D);

    // ld1 {Vt, Vt2, Vt3}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_ld1_3regs, EA_8BYTE, REG_V0, REG_R3, 24, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_ld1_3regs, EA_16BYTE, REG_V4, REG_R7, 48, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_ld1_3regs, EA_8BYTE, REG_V8, REG_R11, 24, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_ld1_3regs, EA_16BYTE, REG_V12, REG_R15, 48, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_ld1_3regs, EA_8BYTE, REG_V16, REG_R19, 24, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_ld1_3regs, EA_16BYTE, REG_V20, REG_R23, 48, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_ld1_3regs, EA_8BYTE, REG_V24, REG_R27, 24, INS_OPTS_1D);
    theEmitter->emitIns_R_R_I(INS_ld1_3regs, EA_16BYTE, REG_V28, REG_SP, 48, INS_OPTS_2D);

    // ld1 {Vt, Vt2, Vt3, Vt4}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_ld1_4regs, EA_8BYTE, REG_V0, REG_R4, 32, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_ld1_4regs, EA_16BYTE, REG_V5, REG_R9, 64, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_ld1_4regs, EA_8BYTE, REG_V10, REG_R14, 32, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_ld1_4regs, EA_16BYTE, REG_V15, REG_R19, 64, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_ld1_4regs, EA_8BYTE, REG_V20, REG_R24, 32, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_ld1_4regs, EA_16BYTE, REG_V25, REG_R29, 64, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_ld1_4regs, EA_8BYTE, REG_V30, REG_R2, 32, INS_OPTS_1D);
    theEmitter->emitIns_R_R_I(INS_ld1_4regs, EA_16BYTE, REG_V3, REG_R7, 64, INS_OPTS_2D);

    // ld2 {Vt, Vt2}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_ld2, EA_8BYTE, REG_V0, REG_R2, 16, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_ld2, EA_16BYTE, REG_V3, REG_R5, 32, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_ld2, EA_8BYTE, REG_V6, REG_R8, 16, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_ld2, EA_16BYTE, REG_V9, REG_R11, 32, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_ld2, EA_8BYTE, REG_V12, REG_R14, 16, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_ld2, EA_16BYTE, REG_V15, REG_R17, 32, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_ld2, EA_16BYTE, REG_V18, REG_R20, 32, INS_OPTS_2D);

    // ld3 {Vt, Vt2, Vt3}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_ld3, EA_8BYTE, REG_V0, REG_R3, 24, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_ld3, EA_16BYTE, REG_V4, REG_R7, 48, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_ld3, EA_8BYTE, REG_V8, REG_R11, 24, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_ld3, EA_16BYTE, REG_V12, REG_R15, 48, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_ld3, EA_8BYTE, REG_V16, REG_R19, 24, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_ld3, EA_16BYTE, REG_V20, REG_R23, 48, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_ld3, EA_16BYTE, REG_V24, REG_R27, 48, INS_OPTS_2D);

    // ld4 {Vt, Vt2, Vt3, Vt4}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_ld4, EA_8BYTE, REG_V0, REG_R4, 32, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_ld4, EA_16BYTE, REG_V5, REG_R9, 64, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_ld4, EA_8BYTE, REG_V10, REG_R14, 32, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_ld4, EA_16BYTE, REG_V15, REG_R19, 64, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_ld4, EA_8BYTE, REG_V20, REG_R24, 32, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_ld4, EA_16BYTE, REG_V25, REG_R29, 64, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_ld4, EA_16BYTE, REG_V30, REG_R2, 64, INS_OPTS_2D);

    // st1 {Vt}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_st1, EA_8BYTE, REG_V0, REG_R1, 8, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_st1, EA_16BYTE, REG_V2, REG_R3, 16, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_st1, EA_8BYTE, REG_V4, REG_R5, 8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_st1, EA_16BYTE, REG_V6, REG_R7, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_st1, EA_8BYTE, REG_V8, REG_R9, 8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_st1, EA_16BYTE, REG_V10, REG_R11, 16, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_st1, EA_8BYTE, REG_V12, REG_R13, 8, INS_OPTS_1D);
    theEmitter->emitIns_R_R_I(INS_st1, EA_16BYTE, REG_V14, REG_R15, 16, INS_OPTS_2D);

    // st1 {Vt, Vt2}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_st1_2regs, EA_8BYTE, REG_V0, REG_R2, 16, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_st1_2regs, EA_16BYTE, REG_V3, REG_R5, 32, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_st1_2regs, EA_8BYTE, REG_V6, REG_R8, 16, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_st1_2regs, EA_16BYTE, REG_V9, REG_R11, 32, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_st1_2regs, EA_8BYTE, REG_V12, REG_R14, 16, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_st1_2regs, EA_16BYTE, REG_V15, REG_R17, 32, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_st1_2regs, EA_8BYTE, REG_V18, REG_R20, 16, INS_OPTS_1D);
    theEmitter->emitIns_R_R_I(INS_st1_2regs, EA_16BYTE, REG_V21, REG_R23, 32, INS_OPTS_2D);

    // st1 {Vt, Vt2, Vt3}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_st1_3regs, EA_8BYTE, REG_V0, REG_R3, 24, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_st1_3regs, EA_16BYTE, REG_V4, REG_R7, 48, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_st1_3regs, EA_8BYTE, REG_V8, REG_R11, 24, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_st1_3regs, EA_16BYTE, REG_V12, REG_R15, 48, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_st1_3regs, EA_8BYTE, REG_V16, REG_R19, 24, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_st1_3regs, EA_16BYTE, REG_V20, REG_R23, 48, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_st1_3regs, EA_8BYTE, REG_V24, REG_R27, 24, INS_OPTS_1D);
    theEmitter->emitIns_R_R_I(INS_st1_3regs, EA_16BYTE, REG_V28, REG_SP, 48, INS_OPTS_2D);

    // st1 {Vt, Vt2, Vt3, Vt4}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_st1_4regs, EA_8BYTE, REG_V0, REG_R4, 32, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_st1_4regs, EA_16BYTE, REG_V5, REG_R9, 64, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_st1_4regs, EA_8BYTE, REG_V10, REG_R14, 32, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_st1_4regs, EA_16BYTE, REG_V15, REG_R19, 64, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_st1_4regs, EA_8BYTE, REG_V20, REG_R24, 32, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_st1_4regs, EA_16BYTE, REG_V25, REG_R29, 64, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_st1_4regs, EA_8BYTE, REG_V30, REG_R2, 32, INS_OPTS_1D);
    theEmitter->emitIns_R_R_I(INS_st1_4regs, EA_16BYTE, REG_V3, REG_R7, 64, INS_OPTS_2D);

    // st2 {Vt, Vt2}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_st2, EA_8BYTE, REG_V0, REG_R2, 16, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_st2, EA_16BYTE, REG_V3, REG_R5, 32, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_st2, EA_8BYTE, REG_V6, REG_R8, 16, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_st2, EA_16BYTE, REG_V9, REG_R11, 32, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_st2, EA_8BYTE, REG_V12, REG_R14, 16, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_st2, EA_16BYTE, REG_V15, REG_R17, 32, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_st2, EA_16BYTE, REG_V18, REG_R20, 32, INS_OPTS_2D);

    // st3 {Vt, Vt2, Vt3}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_st3, EA_8BYTE, REG_V0, REG_R3, 24, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_st3, EA_16BYTE, REG_V4, REG_R7, 48, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_st3, EA_8BYTE, REG_V8, REG_R11, 24, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_st3, EA_16BYTE, REG_V12, REG_R15, 48, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_st3, EA_8BYTE, REG_V16, REG_R19, 24, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_st3, EA_16BYTE, REG_V20, REG_R23, 48, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_st3, EA_16BYTE, REG_V24, REG_R27, 48, INS_OPTS_2D);

    // st4 {Vt, Vt2, Vt3, Vt4}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_st4, EA_8BYTE, REG_V0, REG_R4, 32, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_st4, EA_16BYTE, REG_V5, REG_R9, 64, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_st4, EA_8BYTE, REG_V10, REG_R14, 32, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_st4, EA_16BYTE, REG_V15, REG_R19, 64, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_st4, EA_8BYTE, REG_V20, REG_R24, 32, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_st4, EA_16BYTE, REG_V25, REG_R29, 64, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_st4, EA_16BYTE, REG_V30, REG_R2, 64, INS_OPTS_2D);

    // ld1r {Vt}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_ld1r, EA_8BYTE, REG_V0, REG_R1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_ld1r, EA_16BYTE, REG_V2, REG_R3, 1, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_ld1r, EA_8BYTE, REG_V4, REG_R5, 2, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_ld1r, EA_16BYTE, REG_V6, REG_R7, 2, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_ld1r, EA_8BYTE, REG_V8, REG_R9, 4, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_ld1r, EA_16BYTE, REG_V10, REG_R11, 4, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_ld1r, EA_8BYTE, REG_V12, REG_R13, 8, INS_OPTS_1D);
    theEmitter->emitIns_R_R_I(INS_ld1r, EA_16BYTE, REG_V14, REG_R15, 8, INS_OPTS_2D);

    // ld2r {Vt, Vt2}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_ld2r, EA_8BYTE, REG_V0, REG_R2, 2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_ld2r, EA_16BYTE, REG_V3, REG_R5, 2, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_ld2r, EA_8BYTE, REG_V6, REG_R8, 4, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_ld2r, EA_16BYTE, REG_V9, REG_R11, 4, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_ld2r, EA_8BYTE, REG_V12, REG_R14, 8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_ld2r, EA_16BYTE, REG_V15, REG_R17, 8, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_ld2r, EA_8BYTE, REG_V18, REG_R20, 16, INS_OPTS_1D);
    theEmitter->emitIns_R_R_I(INS_ld2r, EA_16BYTE, REG_V21, REG_R23, 16, INS_OPTS_2D);

    // ld3r {Vt, Vt2, Vt3}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_ld3r, EA_8BYTE, REG_V0, REG_R3, 3, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_ld3r, EA_16BYTE, REG_V4, REG_R7, 3, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_ld3r, EA_8BYTE, REG_V8, REG_R11, 6, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_ld3r, EA_16BYTE, REG_V12, REG_R15, 6, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_ld3r, EA_8BYTE, REG_V16, REG_R19, 12, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_ld3r, EA_16BYTE, REG_V20, REG_R23, 12, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_ld3r, EA_8BYTE, REG_V24, REG_R27, 24, INS_OPTS_1D);
    theEmitter->emitIns_R_R_I(INS_ld3r, EA_16BYTE, REG_V28, REG_SP, 24, INS_OPTS_2D);

    // ld4r {Vt, Vt2, Vt3, Vt4}, [Xn|SP], #imm
    theEmitter->emitIns_R_R_I(INS_ld4r, EA_8BYTE, REG_V0, REG_R4, 4, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_ld4r, EA_16BYTE, REG_V5, REG_R9, 4, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_ld4r, EA_8BYTE, REG_V10, REG_R14, 8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_ld4r, EA_16BYTE, REG_V15, REG_R19, 8, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_ld4r, EA_8BYTE, REG_V20, REG_R24, 16, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_ld4r, EA_16BYTE, REG_V25, REG_R29, 16, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_ld4r, EA_8BYTE, REG_V30, REG_R2, 32, INS_OPTS_1D);
    theEmitter->emitIns_R_R_I(INS_ld4r, EA_16BYTE, REG_V3, REG_R7, 32, INS_OPTS_2D);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // Loads to and Stores from one, two, three, or four SIMD&FP registers
    //

    genDefineTempLabel(genCreateTempLabel());

    // ld1 {Vt}[#index], [Xn|SP]
    theEmitter->emitIns_R_R_I(INS_ld1, EA_1BYTE, REG_V0, REG_R1, 3);
    theEmitter->emitIns_R_R_I(INS_ld1, EA_2BYTE, REG_V2, REG_R3, 2);
    theEmitter->emitIns_R_R_I(INS_ld1, EA_4BYTE, REG_V4, REG_R5, 1);
    theEmitter->emitIns_R_R_I(INS_ld1, EA_8BYTE, REG_V6, REG_R7, 0);

    // ld2 {Vt, Vt2}[#index], [Xn|SP]
    theEmitter->emitIns_R_R_I(INS_ld2, EA_1BYTE, REG_V0, REG_R2, 4);
    theEmitter->emitIns_R_R_I(INS_ld2, EA_2BYTE, REG_V3, REG_R5, 3);
    theEmitter->emitIns_R_R_I(INS_ld2, EA_4BYTE, REG_V6, REG_R8, 2);
    theEmitter->emitIns_R_R_I(INS_ld2, EA_8BYTE, REG_V9, REG_R11, 1);

    // ld3 {Vt, Vt2, Vt3}[#index], [Xn|SP]
    theEmitter->emitIns_R_R_I(INS_ld3, EA_1BYTE, REG_V0, REG_R3, 5);
    theEmitter->emitIns_R_R_I(INS_ld3, EA_2BYTE, REG_V4, REG_R7, 4);
    theEmitter->emitIns_R_R_I(INS_ld3, EA_4BYTE, REG_V8, REG_R11, 3);
    theEmitter->emitIns_R_R_I(INS_ld3, EA_8BYTE, REG_V12, REG_R15, 0);

    // ld4 {Vt, Vt2, Vt3, Vt4}[#index], [Xn|SP]
    theEmitter->emitIns_R_R_I(INS_ld4, EA_1BYTE, REG_V0, REG_R4, 6);
    theEmitter->emitIns_R_R_I(INS_ld4, EA_2BYTE, REG_V5, REG_R9, 5);
    theEmitter->emitIns_R_R_I(INS_ld4, EA_4BYTE, REG_V10, REG_R14, 0);
    theEmitter->emitIns_R_R_I(INS_ld4, EA_8BYTE, REG_V15, REG_R19, 1);

    // st1 {Vt}[#index], [Xn|SP]
    theEmitter->emitIns_R_R_I(INS_st1, EA_1BYTE, REG_V0, REG_R1, 7);
    theEmitter->emitIns_R_R_I(INS_st1, EA_2BYTE, REG_V2, REG_R3, 6);
    theEmitter->emitIns_R_R_I(INS_st1, EA_4BYTE, REG_V4, REG_R5, 1);
    theEmitter->emitIns_R_R_I(INS_st1, EA_8BYTE, REG_V6, REG_R7, 0);

    // st2 {Vt, Vt2}[#index], [Xn|SP]
    theEmitter->emitIns_R_R_I(INS_st2, EA_1BYTE, REG_V0, REG_R2, 8);
    theEmitter->emitIns_R_R_I(INS_st2, EA_2BYTE, REG_V3, REG_R5, 7);
    theEmitter->emitIns_R_R_I(INS_st2, EA_4BYTE, REG_V6, REG_R8, 2);
    theEmitter->emitIns_R_R_I(INS_st2, EA_8BYTE, REG_V9, REG_R11, 1);

    // st3 {Vt, Vt2, Vt3}[#index], [Xn|SP]
    theEmitter->emitIns_R_R_I(INS_st3, EA_1BYTE, REG_V0, REG_R3, 9);
    theEmitter->emitIns_R_R_I(INS_st3, EA_2BYTE, REG_V4, REG_R7, 0);
    theEmitter->emitIns_R_R_I(INS_st3, EA_4BYTE, REG_V8, REG_R11, 3);
    theEmitter->emitIns_R_R_I(INS_st3, EA_8BYTE, REG_V12, REG_R15, 0);

    // st4 {Vt, Vt2, Vt3, Vt4}[#index], [Xn|SP]
    theEmitter->emitIns_R_R_I(INS_st4, EA_1BYTE, REG_V0, REG_R4, 10);
    theEmitter->emitIns_R_R_I(INS_st4, EA_2BYTE, REG_V5, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_st4, EA_4BYTE, REG_V10, REG_R14, 0);
    theEmitter->emitIns_R_R_I(INS_st4, EA_8BYTE, REG_V15, REG_R19, 1);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // Loads to and Stores from one, two, three, or four SIMD&FP registers
    //

    genDefineTempLabel(genCreateTempLabel());

    // ld1 {Vt}[#index], [Xn|SP], Xm
    theEmitter->emitIns_R_R_R_I(INS_ld1, EA_1BYTE, REG_V0, REG_R1, REG_R2, 3, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ld1, EA_2BYTE, REG_V3, REG_R4, REG_R5, 2, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ld1, EA_4BYTE, REG_V6, REG_R7, REG_R8, 1, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ld1, EA_8BYTE, REG_V9, REG_R10, REG_R11, 0, INS_OPTS_POST_INDEX);

    // ld2 {Vt, Vt2}[#index], [Xn|SP], Xm
    theEmitter->emitIns_R_R_R_I(INS_ld2, EA_1BYTE, REG_V0, REG_R2, REG_R3, 4, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ld2, EA_2BYTE, REG_V4, REG_R6, REG_R7, 3, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ld2, EA_4BYTE, REG_V8, REG_R10, REG_R11, 2, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ld2, EA_8BYTE, REG_V12, REG_R14, REG_R15, 1, INS_OPTS_POST_INDEX);

    // ld3 {Vt, Vt2, Vt3}[#index], [Xn|SP], Xm
    theEmitter->emitIns_R_R_R_I(INS_ld3, EA_1BYTE, REG_V0, REG_R3, REG_R4, 5, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ld3, EA_2BYTE, REG_V5, REG_R8, REG_R9, 4, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ld3, EA_4BYTE, REG_V10, REG_R13, REG_R14, 3, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ld3, EA_8BYTE, REG_V15, REG_R18, REG_R19, 0, INS_OPTS_POST_INDEX);

    // ld4 {Vt, Vt2, Vt3, Vt4}[#index], [Xn|SP], Xm
    theEmitter->emitIns_R_R_R_I(INS_ld4, EA_1BYTE, REG_V0, REG_R4, REG_R5, 6, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ld4, EA_2BYTE, REG_V6, REG_R10, REG_R11, 5, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ld4, EA_4BYTE, REG_V12, REG_R16, REG_R17, 0, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ld4, EA_8BYTE, REG_V18, REG_R22, REG_R23, 1, INS_OPTS_POST_INDEX);

    // st1 {Vt}[#index], [Xn|SP], Xm
    theEmitter->emitIns_R_R_R_I(INS_st1, EA_1BYTE, REG_V0, REG_R1, REG_R2, 7, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_st1, EA_2BYTE, REG_V3, REG_R4, REG_R5, 6, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_st1, EA_4BYTE, REG_V6, REG_R7, REG_R8, 1, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_st1, EA_8BYTE, REG_V9, REG_R10, REG_R11, 0, INS_OPTS_POST_INDEX);

    // st2 {Vt, Vt2}[#index], [Xn|SP], Xm
    theEmitter->emitIns_R_R_R_I(INS_st2, EA_1BYTE, REG_V0, REG_R2, REG_R3, 8, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_st2, EA_2BYTE, REG_V4, REG_R6, REG_R7, 7, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_st2, EA_4BYTE, REG_V8, REG_R10, REG_R11, 2, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_st2, EA_8BYTE, REG_V12, REG_R14, REG_R15, 1, INS_OPTS_POST_INDEX);

    // st3 {Vt, Vt2, Vt3}[#index], [Xn|SP], Xm
    theEmitter->emitIns_R_R_R_I(INS_st3, EA_1BYTE, REG_V0, REG_R3, REG_R4, 9, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_st3, EA_2BYTE, REG_V5, REG_R8, REG_R9, 0, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_st3, EA_4BYTE, REG_V10, REG_R13, REG_R14, 3, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_st3, EA_8BYTE, REG_V15, REG_R18, REG_R19, 0, INS_OPTS_POST_INDEX);

    // st4 {Vt, Vt2, Vt3, Vt4}[#index], [Xn|SP], Xm
    theEmitter->emitIns_R_R_R_I(INS_st4, EA_1BYTE, REG_V0, REG_R4, REG_R5, 10, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_st4, EA_2BYTE, REG_V6, REG_R10, REG_R11, 1, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_st4, EA_4BYTE, REG_V12, REG_R16, REG_R17, 0, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_st4, EA_8BYTE, REG_V18, REG_R22, REG_R23, 1, INS_OPTS_POST_INDEX);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // Loads to and Stores from one, two, three, or four SIMD&FP registers
    //

    genDefineTempLabel(genCreateTempLabel());

    // ld1 {Vt}[#index], [Xn|SP], #imm
    theEmitter->emitIns_R_R_I_I(INS_ld1, EA_1BYTE, REG_V0, REG_R1, 3, 1, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_ld1, EA_2BYTE, REG_V2, REG_R3, 2, 2, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_ld1, EA_4BYTE, REG_V4, REG_R5, 1, 4, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_ld1, EA_8BYTE, REG_V6, REG_R7, 0, 8, INS_OPTS_POST_INDEX);

    // ld2 {Vt, Vt2}[#index], [Xn|SP], #imm
    theEmitter->emitIns_R_R_I_I(INS_ld2, EA_1BYTE, REG_V0, REG_R2, 4, 2, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_ld2, EA_2BYTE, REG_V3, REG_R5, 3, 4, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_ld2, EA_4BYTE, REG_V6, REG_R8, 2, 8, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_ld2, EA_8BYTE, REG_V9, REG_R11, 1, 16, INS_OPTS_POST_INDEX);

    // ld3 {Vt, Vt2, Vt3}[#index], [Xn|SP], #imm
    theEmitter->emitIns_R_R_I_I(INS_ld3, EA_1BYTE, REG_V0, REG_R3, 5, 3, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_ld3, EA_2BYTE, REG_V4, REG_R7, 4, 6, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_ld3, EA_4BYTE, REG_V8, REG_R11, 3, 12, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_ld3, EA_8BYTE, REG_V12, REG_R15, 0, 24, INS_OPTS_POST_INDEX);

    // ld4 {Vt, Vt2, Vt3, Vt4}[#index], [Xn|SP], #imm
    theEmitter->emitIns_R_R_I_I(INS_ld4, EA_1BYTE, REG_V0, REG_R4, 6, 4, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_ld4, EA_2BYTE, REG_V5, REG_R9, 5, 8, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_ld4, EA_4BYTE, REG_V10, REG_R14, 0, 16, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_ld4, EA_8BYTE, REG_V15, REG_R19, 1, 32, INS_OPTS_POST_INDEX);

    // st1 {Vt}[#index], [Xn|SP], #imm
    theEmitter->emitIns_R_R_I_I(INS_st1, EA_1BYTE, REG_V0, REG_R1, 3, 1, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_st1, EA_2BYTE, REG_V2, REG_R3, 2, 2, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_st1, EA_4BYTE, REG_V4, REG_R5, 1, 4, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_st1, EA_8BYTE, REG_V6, REG_R7, 0, 8, INS_OPTS_POST_INDEX);

    // st2 {Vt, Vt2}[#index], [Xn|SP], #imm
    theEmitter->emitIns_R_R_I_I(INS_st2, EA_1BYTE, REG_V0, REG_R2, 4, 2, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_st2, EA_2BYTE, REG_V3, REG_R5, 3, 4, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_st2, EA_4BYTE, REG_V6, REG_R8, 2, 8, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_st2, EA_8BYTE, REG_V9, REG_R11, 1, 16, INS_OPTS_POST_INDEX);

    // st3 {Vt, Vt2, Vt3}[#index], [Xn|SP], #imm
    theEmitter->emitIns_R_R_I_I(INS_st3, EA_1BYTE, REG_V0, REG_R3, 5, 3, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_st3, EA_2BYTE, REG_V4, REG_R7, 4, 6, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_st3, EA_4BYTE, REG_V8, REG_R11, 3, 12, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_st3, EA_8BYTE, REG_V12, REG_R15, 0, 24, INS_OPTS_POST_INDEX);

    // st4 {Vt, Vt2, Vt3, Vt4}[#index], [Xn|SP], #imm
    theEmitter->emitIns_R_R_I_I(INS_st4, EA_1BYTE, REG_V0, REG_R4, 6, 4, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_st4, EA_2BYTE, REG_V5, REG_R9, 5, 8, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_st4, EA_4BYTE, REG_V10, REG_R14, 0, 16, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I_I(INS_st4, EA_8BYTE, REG_V15, REG_R19, 1, 32, INS_OPTS_POST_INDEX);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // Compares
    //

    genDefineTempLabel(genCreateTempLabel());

    // cmp reg, reg
    theEmitter->emitIns_R_R(INS_cmp, EA_8BYTE, REG_R8, REG_R9);
    theEmitter->emitIns_R_R(INS_cmn, EA_8BYTE, REG_R8, REG_R9);

    // cmp reg, imm
    theEmitter->emitIns_R_I(INS_cmp, EA_8BYTE, REG_R8, 0);
    theEmitter->emitIns_R_I(INS_cmp, EA_8BYTE, REG_R8, 4095);
    theEmitter->emitIns_R_I(INS_cmp, EA_8BYTE, REG_R8, 1 << 12);
    theEmitter->emitIns_R_I(INS_cmp, EA_8BYTE, REG_R8, 4095 << 12);

    theEmitter->emitIns_R_I(INS_cmn, EA_8BYTE, REG_R8, 0);
    theEmitter->emitIns_R_I(INS_cmn, EA_8BYTE, REG_R8, 4095);
    theEmitter->emitIns_R_I(INS_cmn, EA_8BYTE, REG_R8, 1 << 12);
    theEmitter->emitIns_R_I(INS_cmn, EA_8BYTE, REG_R8, 4095 << 12);

    theEmitter->emitIns_R_I(INS_cmp, EA_8BYTE, REG_R8, -1);
    theEmitter->emitIns_R_I(INS_cmp, EA_8BYTE, REG_R8, -0xfff);
    theEmitter->emitIns_R_I(INS_cmp, EA_8BYTE, REG_R8, 0xfffffffffffff000LL);
    theEmitter->emitIns_R_I(INS_cmp, EA_8BYTE, REG_R8, 0xffffffffff800000LL);

    theEmitter->emitIns_R_I(INS_cmn, EA_8BYTE, REG_R8, -1);
    theEmitter->emitIns_R_I(INS_cmn, EA_8BYTE, REG_R8, -0xfff);
    theEmitter->emitIns_R_I(INS_cmn, EA_8BYTE, REG_R8, 0xfffffffffffff000LL);
    theEmitter->emitIns_R_I(INS_cmn, EA_8BYTE, REG_R8, 0xffffffffff800000LL);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    // R_R
    //

    genDefineTempLabel(genCreateTempLabel());

    theEmitter->emitIns_R_R(INS_cls, EA_8BYTE, REG_R1, REG_R12);
    theEmitter->emitIns_R_R(INS_clz, EA_8BYTE, REG_R2, REG_R13);
    theEmitter->emitIns_R_R(INS_rbit, EA_8BYTE, REG_R3, REG_R14);
    theEmitter->emitIns_R_R(INS_rev, EA_8BYTE, REG_R4, REG_R15);
    theEmitter->emitIns_R_R(INS_rev16, EA_8BYTE, REG_R5, REG_R0);
    theEmitter->emitIns_R_R(INS_rev32, EA_8BYTE, REG_R6, REG_R1);

    theEmitter->emitIns_R_R(INS_cls, EA_4BYTE, REG_R7, REG_R2);
    theEmitter->emitIns_R_R(INS_clz, EA_4BYTE, REG_R8, REG_R3);
    theEmitter->emitIns_R_R(INS_rbit, EA_4BYTE, REG_R9, REG_R4);
    theEmitter->emitIns_R_R(INS_rev, EA_4BYTE, REG_R10, REG_R5);
    theEmitter->emitIns_R_R(INS_rev16, EA_4BYTE, REG_R11, REG_R6);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_I
    //

    genDefineTempLabel(genCreateTempLabel());

    // mov reg, imm(i16,hw)
    theEmitter->emitIns_R_I(INS_mov, EA_8BYTE, REG_R8, 0x0000000000001234);
    theEmitter->emitIns_R_I(INS_mov, EA_8BYTE, REG_R8, 0x0000000043210000);
    theEmitter->emitIns_R_I(INS_mov, EA_8BYTE, REG_R8, 0x0000567800000000);
    theEmitter->emitIns_R_I(INS_mov, EA_8BYTE, REG_R8, 0x8765000000000000);
    theEmitter->emitIns_R_I(INS_mov, EA_8BYTE, REG_R8, 0xFFFFFFFFFFFF1234);
    theEmitter->emitIns_R_I(INS_mov, EA_8BYTE, REG_R8, 0xFFFFFFFF4321FFFF);
    theEmitter->emitIns_R_I(INS_mov, EA_8BYTE, REG_R8, 0xFFFF5678FFFFFFFF);
    theEmitter->emitIns_R_I(INS_mov, EA_8BYTE, REG_R8, 0x8765FFFFFFFFFFFF);

    theEmitter->emitIns_R_I(INS_mov, EA_4BYTE, REG_R8, 0x00001234);
    theEmitter->emitIns_R_I(INS_mov, EA_4BYTE, REG_R8, 0x87650000);
    theEmitter->emitIns_R_I(INS_mov, EA_4BYTE, REG_R8, 0xFFFF1234);
    theEmitter->emitIns_R_I(INS_mov, EA_4BYTE, REG_R8, 0x4567FFFF);

    // mov reg, imm(N,r,s)
    theEmitter->emitIns_R_I(INS_mov, EA_8BYTE, REG_R8, 0x00FFFFF000000000);
    theEmitter->emitIns_R_I(INS_mov, EA_8BYTE, REG_R8, 0x6666666666666666);
    theEmitter->emitIns_R_I(INS_mov, EA_8BYTE, REG_SP, 0x7FFF00007FFF0000);
    theEmitter->emitIns_R_I(INS_mov, EA_8BYTE, REG_R8, 0x5555555555555555);
    theEmitter->emitIns_R_I(INS_mov, EA_8BYTE, REG_R8, 0xE003E003E003E003);
    theEmitter->emitIns_R_I(INS_mov, EA_8BYTE, REG_R8, 0x0707070707070707);

    theEmitter->emitIns_R_I(INS_mov, EA_4BYTE, REG_R8, 0x00FFFFF0);
    theEmitter->emitIns_R_I(INS_mov, EA_4BYTE, REG_R8, 0x66666666);
    theEmitter->emitIns_R_I(INS_mov, EA_4BYTE, REG_R8, 0x03FFC000);
    theEmitter->emitIns_R_I(INS_mov, EA_4BYTE, REG_R8, 0x55555555);
    theEmitter->emitIns_R_I(INS_mov, EA_4BYTE, REG_R8, 0xE003E003);
    theEmitter->emitIns_R_I(INS_mov, EA_4BYTE, REG_R8, 0x07070707);

    theEmitter->emitIns_R_I(INS_tst, EA_8BYTE, REG_R8, 0xE003E003E003E003);
    theEmitter->emitIns_R_I(INS_tst, EA_8BYTE, REG_R8, 0x00FFFFF000000000);
    theEmitter->emitIns_R_I(INS_tst, EA_8BYTE, REG_R8, 0x6666666666666666);
    theEmitter->emitIns_R_I(INS_tst, EA_8BYTE, REG_R8, 0x0707070707070707);
    theEmitter->emitIns_R_I(INS_tst, EA_8BYTE, REG_R8, 0x7FFF00007FFF0000);
    theEmitter->emitIns_R_I(INS_tst, EA_8BYTE, REG_R8, 0x5555555555555555);

    theEmitter->emitIns_R_I(INS_tst, EA_4BYTE, REG_R8, 0xE003E003);
    theEmitter->emitIns_R_I(INS_tst, EA_4BYTE, REG_R8, 0x00FFFFF0);
    theEmitter->emitIns_R_I(INS_tst, EA_4BYTE, REG_R8, 0x66666666);
    theEmitter->emitIns_R_I(INS_tst, EA_4BYTE, REG_R8, 0x07070707);
    theEmitter->emitIns_R_I(INS_tst, EA_4BYTE, REG_R8, 0xFFF00000);
    theEmitter->emitIns_R_I(INS_tst, EA_4BYTE, REG_R8, 0x55555555);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R
    //

    genDefineTempLabel(genCreateTempLabel());

    // tst reg, reg
    theEmitter->emitIns_R_R(INS_tst, EA_8BYTE, REG_R7, REG_R10);

    // mov reg, reg
    theEmitter->emitIns_Mov(INS_mov, EA_8BYTE, REG_R7, REG_R10, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_mov, EA_8BYTE, REG_R8, REG_SP, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_mov, EA_8BYTE, REG_SP, REG_R9, /* canSkip */ false);

    theEmitter->emitIns_R_R(INS_mvn, EA_8BYTE, REG_R5, REG_R11);
    theEmitter->emitIns_R_R(INS_neg, EA_8BYTE, REG_R4, REG_R12);
    theEmitter->emitIns_R_R(INS_negs, EA_8BYTE, REG_R3, REG_R13);

    theEmitter->emitIns_Mov(INS_mov, EA_4BYTE, REG_R7, REG_R10, /* canSkip */ false);
    theEmitter->emitIns_R_R(INS_mvn, EA_4BYTE, REG_R5, REG_R11);
    theEmitter->emitIns_R_R(INS_neg, EA_4BYTE, REG_R4, REG_R12);
    theEmitter->emitIns_R_R(INS_negs, EA_4BYTE, REG_R3, REG_R13);

    theEmitter->emitIns_Mov(INS_sxtb, EA_8BYTE, REG_R7, REG_R10, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_sxth, EA_8BYTE, REG_R5, REG_R11, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_sxtw, EA_8BYTE, REG_R4, REG_R12, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_uxtb, EA_8BYTE, REG_R3, REG_R13, /* canSkip */ false); // map to Wt
    theEmitter->emitIns_Mov(INS_uxth, EA_8BYTE, REG_R2, REG_R14, /* canSkip */ false); // map to Wt

    theEmitter->emitIns_Mov(INS_sxtb, EA_4BYTE, REG_R7, REG_R10, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_sxth, EA_4BYTE, REG_R5, REG_R11, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_uxtb, EA_4BYTE, REG_R3, REG_R13, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_uxth, EA_4BYTE, REG_R2, REG_R14, /* canSkip */ false);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_I_I
    //

    genDefineTempLabel(genCreateTempLabel());

    // mov reg, imm(i16,hw)
    theEmitter->emitIns_R_I_I(INS_mov, EA_8BYTE, REG_R8, 0x1234, 0, INS_OPTS_LSL);
    theEmitter->emitIns_R_I_I(INS_mov, EA_8BYTE, REG_R8, 0x4321, 16, INS_OPTS_LSL);

    theEmitter->emitIns_R_I_I(INS_movk, EA_8BYTE, REG_R8, 0x4321, 16, INS_OPTS_LSL);
    theEmitter->emitIns_R_I_I(INS_movn, EA_8BYTE, REG_R8, 0x5678, 32, INS_OPTS_LSL);
    theEmitter->emitIns_R_I_I(INS_movz, EA_8BYTE, REG_R8, 0x8765, 48, INS_OPTS_LSL);

    theEmitter->emitIns_R_I_I(INS_movk, EA_4BYTE, REG_R8, 0x4321, 16, INS_OPTS_LSL);
    theEmitter->emitIns_R_I_I(INS_movn, EA_4BYTE, REG_R8, 0x5678, 16, INS_OPTS_LSL);
    theEmitter->emitIns_R_I_I(INS_movz, EA_4BYTE, REG_R8, 0x8765, 16, INS_OPTS_LSL);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R_I
    //

    genDefineTempLabel(genCreateTempLabel());

    theEmitter->emitIns_R_R_I(INS_lsl, EA_8BYTE, REG_R0, REG_R0, 1);
    theEmitter->emitIns_R_R_I(INS_lsl, EA_4BYTE, REG_R9, REG_R3, 18);
    theEmitter->emitIns_R_R_I(INS_lsr, EA_8BYTE, REG_R7, REG_R0, 37);
    theEmitter->emitIns_R_R_I(INS_lsr, EA_4BYTE, REG_R0, REG_R1, 2);
    theEmitter->emitIns_R_R_I(INS_asr, EA_8BYTE, REG_R2, REG_R3, 53);
    theEmitter->emitIns_R_R_I(INS_asr, EA_4BYTE, REG_R9, REG_R3, 18);

    theEmitter->emitIns_R_R_I(INS_and, EA_8BYTE, REG_R2, REG_R3, 0x5555555555555555);
    theEmitter->emitIns_R_R_I(INS_ands, EA_8BYTE, REG_R1, REG_R5, 0x6666666666666666);
    theEmitter->emitIns_R_R_I(INS_eor, EA_8BYTE, REG_R8, REG_R9, 0x0707070707070707);
    theEmitter->emitIns_R_R_I(INS_orr, EA_8BYTE, REG_SP, REG_R3, 0xFFFC000000000000);
    theEmitter->emitIns_R_R_I(INS_ands, EA_4BYTE, REG_R8, REG_R9, 0xE003E003);

    theEmitter->emitIns_R_R_I(INS_ror, EA_8BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_ror, EA_8BYTE, REG_R8, REG_R9, 31);
    theEmitter->emitIns_R_R_I(INS_ror, EA_8BYTE, REG_R8, REG_R9, 32);
    theEmitter->emitIns_R_R_I(INS_ror, EA_8BYTE, REG_R8, REG_R9, 63);

    theEmitter->emitIns_R_R_I(INS_ror, EA_4BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_ror, EA_4BYTE, REG_R8, REG_R9, 31);

    theEmitter->emitIns_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, 0); // == mov
    theEmitter->emitIns_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, -1);
    theEmitter->emitIns_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, 0xfff);
    theEmitter->emitIns_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, -0xfff);
    theEmitter->emitIns_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, 0x1000);
    theEmitter->emitIns_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, 0xfff000);
    theEmitter->emitIns_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, 0xfffffffffffff000LL);
    theEmitter->emitIns_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, 0xffffffffff800000LL);

    theEmitter->emitIns_R_R_I(INS_add, EA_4BYTE, REG_R8, REG_R9, 0); // == mov
    theEmitter->emitIns_R_R_I(INS_add, EA_4BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_add, EA_4BYTE, REG_R8, REG_R9, -1);
    theEmitter->emitIns_R_R_I(INS_add, EA_4BYTE, REG_R8, REG_R9, 0xfff);
    theEmitter->emitIns_R_R_I(INS_add, EA_4BYTE, REG_R8, REG_R9, -0xfff);
    theEmitter->emitIns_R_R_I(INS_add, EA_4BYTE, REG_R8, REG_R9, 0x1000);
    theEmitter->emitIns_R_R_I(INS_add, EA_4BYTE, REG_R8, REG_R9, 0xfff000);
    theEmitter->emitIns_R_R_I(INS_add, EA_4BYTE, REG_R8, REG_R9, 0xfffffffffffff000LL);
    theEmitter->emitIns_R_R_I(INS_add, EA_4BYTE, REG_R8, REG_R9, 0xffffffffff800000LL);

    theEmitter->emitIns_R_R_I(INS_sub, EA_8BYTE, REG_R8, REG_R9, 0); // == mov
    theEmitter->emitIns_R_R_I(INS_sub, EA_8BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_sub, EA_8BYTE, REG_R8, REG_R9, -1);
    theEmitter->emitIns_R_R_I(INS_sub, EA_8BYTE, REG_R8, REG_R9, 0xfff);
    theEmitter->emitIns_R_R_I(INS_sub, EA_8BYTE, REG_R8, REG_R9, -0xfff);
    theEmitter->emitIns_R_R_I(INS_sub, EA_8BYTE, REG_R8, REG_R9, 0x1000);
    theEmitter->emitIns_R_R_I(INS_sub, EA_8BYTE, REG_R8, REG_R9, 0xfff000);
    theEmitter->emitIns_R_R_I(INS_sub, EA_8BYTE, REG_R8, REG_R9, 0xfffffffffffff000LL);
    theEmitter->emitIns_R_R_I(INS_sub, EA_8BYTE, REG_R8, REG_R9, 0xffffffffff800000LL);

    theEmitter->emitIns_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, 0); // == mov
    theEmitter->emitIns_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, -1);
    theEmitter->emitIns_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, 0xfff);
    theEmitter->emitIns_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, -0xfff);
    theEmitter->emitIns_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, 0x1000);
    theEmitter->emitIns_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, 0xfff000);
    theEmitter->emitIns_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, 0xfffffffffffff000LL);
    theEmitter->emitIns_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, 0xffffffffff800000LL);

    theEmitter->emitIns_R_R_I(INS_adds, EA_8BYTE, REG_R8, REG_R9, 0); // == mov
    theEmitter->emitIns_R_R_I(INS_adds, EA_8BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_adds, EA_8BYTE, REG_R8, REG_R9, -1);
    theEmitter->emitIns_R_R_I(INS_adds, EA_8BYTE, REG_R8, REG_R9, 0xfff);
    theEmitter->emitIns_R_R_I(INS_adds, EA_8BYTE, REG_R8, REG_R9, -0xfff);
    theEmitter->emitIns_R_R_I(INS_adds, EA_8BYTE, REG_R8, REG_R9, 0x1000);
    theEmitter->emitIns_R_R_I(INS_adds, EA_8BYTE, REG_R8, REG_R9, 0xfff000);
    theEmitter->emitIns_R_R_I(INS_adds, EA_8BYTE, REG_R8, REG_R9, 0xfffffffffffff000LL);
    theEmitter->emitIns_R_R_I(INS_adds, EA_8BYTE, REG_R8, REG_R9, 0xffffffffff800000LL);

    theEmitter->emitIns_R_R_I(INS_adds, EA_4BYTE, REG_R8, REG_R9, 0); // == mov
    theEmitter->emitIns_R_R_I(INS_adds, EA_4BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_adds, EA_4BYTE, REG_R8, REG_R9, -1);
    theEmitter->emitIns_R_R_I(INS_adds, EA_4BYTE, REG_R8, REG_R9, 0xfff);
    theEmitter->emitIns_R_R_I(INS_adds, EA_4BYTE, REG_R8, REG_R9, -0xfff);
    theEmitter->emitIns_R_R_I(INS_adds, EA_4BYTE, REG_R8, REG_R9, 0x1000);
    theEmitter->emitIns_R_R_I(INS_adds, EA_4BYTE, REG_R8, REG_R9, 0xfff000);
    theEmitter->emitIns_R_R_I(INS_adds, EA_4BYTE, REG_R8, REG_R9, 0xfffffffffffff000LL);
    theEmitter->emitIns_R_R_I(INS_adds, EA_4BYTE, REG_R8, REG_R9, 0xffffffffff800000LL);

    theEmitter->emitIns_R_R_I(INS_subs, EA_8BYTE, REG_R8, REG_R9, 0); // == mov
    theEmitter->emitIns_R_R_I(INS_subs, EA_8BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_subs, EA_8BYTE, REG_R8, REG_R9, -1);
    theEmitter->emitIns_R_R_I(INS_subs, EA_8BYTE, REG_R8, REG_R9, 0xfff);
    theEmitter->emitIns_R_R_I(INS_subs, EA_8BYTE, REG_R8, REG_R9, -0xfff);
    theEmitter->emitIns_R_R_I(INS_subs, EA_8BYTE, REG_R8, REG_R9, 0x1000);
    theEmitter->emitIns_R_R_I(INS_subs, EA_8BYTE, REG_R8, REG_R9, 0xfff000);
    theEmitter->emitIns_R_R_I(INS_subs, EA_8BYTE, REG_R8, REG_R9, 0xfffffffffffff000LL);
    theEmitter->emitIns_R_R_I(INS_subs, EA_8BYTE, REG_R8, REG_R9, 0xffffffffff800000LL);

    theEmitter->emitIns_R_R_I(INS_subs, EA_4BYTE, REG_R8, REG_R9, 0); // == mov
    theEmitter->emitIns_R_R_I(INS_subs, EA_4BYTE, REG_R8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_subs, EA_4BYTE, REG_R8, REG_R9, -1);
    theEmitter->emitIns_R_R_I(INS_subs, EA_4BYTE, REG_R8, REG_R9, 0xfff);
    theEmitter->emitIns_R_R_I(INS_subs, EA_4BYTE, REG_R8, REG_R9, -0xfff);
    theEmitter->emitIns_R_R_I(INS_subs, EA_4BYTE, REG_R8, REG_R9, 0x1000);
    theEmitter->emitIns_R_R_I(INS_subs, EA_4BYTE, REG_R8, REG_R9, 0xfff000);
    theEmitter->emitIns_R_R_I(INS_subs, EA_4BYTE, REG_R8, REG_R9, 0xfffffffffffff000LL);
    theEmitter->emitIns_R_R_I(INS_subs, EA_4BYTE, REG_R8, REG_R9, 0xffffffffff800000LL);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R_I cmp/txt
    //

    // cmp
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 0);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_4BYTE, REG_R8, REG_R9, 0);

    // CMP (shifted register)
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 31, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 32, INS_OPTS_LSR);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 33, INS_OPTS_ASR);

    theEmitter->emitIns_R_R_I(INS_cmp, EA_4BYTE, REG_R8, REG_R9, 21, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_4BYTE, REG_R8, REG_R9, 22, INS_OPTS_LSR);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_4BYTE, REG_R8, REG_R9, 23, INS_OPTS_ASR);

    // TST (shifted register)
    theEmitter->emitIns_R_R_I(INS_tst, EA_8BYTE, REG_R8, REG_R9, 31, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_I(INS_tst, EA_8BYTE, REG_R8, REG_R9, 32, INS_OPTS_LSR);
    theEmitter->emitIns_R_R_I(INS_tst, EA_8BYTE, REG_R8, REG_R9, 33, INS_OPTS_ASR);
    theEmitter->emitIns_R_R_I(INS_tst, EA_8BYTE, REG_R8, REG_R9, 34, INS_OPTS_ROR);

    theEmitter->emitIns_R_R_I(INS_tst, EA_4BYTE, REG_R8, REG_R9, 21, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_I(INS_tst, EA_4BYTE, REG_R8, REG_R9, 22, INS_OPTS_LSR);
    theEmitter->emitIns_R_R_I(INS_tst, EA_4BYTE, REG_R8, REG_R9, 23, INS_OPTS_ASR);
    theEmitter->emitIns_R_R_I(INS_tst, EA_4BYTE, REG_R8, REG_R9, 24, INS_OPTS_ROR);

    // CMP (extended register)
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 0, INS_OPTS_UXTB);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 0, INS_OPTS_UXTH);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 0, INS_OPTS_UXTW); // "cmp x8, x9, UXTW"; msdis
                                                                                    // disassembles this "cmp x8,x9",
                                                                                    // which looks like an msdis issue.
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 0, INS_OPTS_UXTX);

    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 0, INS_OPTS_SXTB);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 0, INS_OPTS_SXTH);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 0, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 0, INS_OPTS_SXTX);

    // CMP 64-bit (extended register) and left shift
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 1, INS_OPTS_UXTB);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 2, INS_OPTS_UXTH);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 3, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 4, INS_OPTS_UXTX);

    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 1, INS_OPTS_SXTB);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 2, INS_OPTS_SXTH);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 3, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_8BYTE, REG_R8, REG_R9, 4, INS_OPTS_SXTX);

    // CMP 32-bit (extended register) and left shift
    theEmitter->emitIns_R_R_I(INS_cmp, EA_4BYTE, REG_R8, REG_R9, 0, INS_OPTS_UXTB);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_4BYTE, REG_R8, REG_R9, 2, INS_OPTS_UXTH);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_4BYTE, REG_R8, REG_R9, 4, INS_OPTS_UXTW);

    theEmitter->emitIns_R_R_I(INS_cmp, EA_4BYTE, REG_R8, REG_R9, 0, INS_OPTS_SXTB);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_4BYTE, REG_R8, REG_R9, 2, INS_OPTS_SXTH);
    theEmitter->emitIns_R_R_I(INS_cmp, EA_4BYTE, REG_R8, REG_R9, 4, INS_OPTS_SXTW);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R_R
    //

    genDefineTempLabel(genCreateTempLabel());

    theEmitter->emitIns_R_R_R(INS_lsl, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_lsr, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_asr, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ror, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_adc, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_adcs, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_sbc, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_sbcs, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_udiv, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_sdiv, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_mul, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_mneg, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_smull, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_smnegl, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_smulh, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_umull, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_umnegl, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_umulh, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_lslv, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_lsrv, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_asrv, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_rorv, EA_8BYTE, REG_R8, REG_R9, REG_R10);

    theEmitter->emitIns_R_R_R(INS_lsl, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_lsr, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_asr, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ror, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_adc, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_adcs, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_sbc, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_sbcs, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_udiv, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_sdiv, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_mul, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_mneg, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_smull, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_smnegl, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_smulh, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_umull, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_umnegl, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_umulh, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_lslv, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_lsrv, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_asrv, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_rorv, EA_4BYTE, REG_R8, REG_R9, REG_R10);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // ARMv8.1 LSE Atomics
    //
    genDefineTempLabel(genCreateTempLabel());

    theEmitter->emitIns_R_R_R(INS_casb, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_casab, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_casalb, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_caslb, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_cash, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_casah, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_casalh, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_caslh, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_cas, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_casa, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_casal, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_casl, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_cas, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_casa, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_casal, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_casl, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldaddb, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldaddab, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldaddalb, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldaddlb, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldaddh, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldaddah, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldaddalh, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldaddlh, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldadd, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldadda, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldaddal, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldaddl, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldadd, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldadda, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldaddal, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldclral, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldclral, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldsetal, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldsetal, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_ldaddl, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swpb, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swpab, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swpalb, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swplb, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swph, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swpah, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swpalh, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swplh, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swp, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swpa, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swpal, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swpl, EA_4BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swp, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swpa, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swpal, EA_8BYTE, REG_R8, REG_R9, REG_R10);
    theEmitter->emitIns_R_R_R(INS_swpl, EA_8BYTE, REG_R8, REG_R9, REG_R10);

    theEmitter->emitIns_R_R(INS_staddb, EA_4BYTE, REG_R8, REG_R10);
    theEmitter->emitIns_R_R(INS_staddlb, EA_4BYTE, REG_R8, REG_R10);
    theEmitter->emitIns_R_R(INS_staddh, EA_4BYTE, REG_R8, REG_R10);
    theEmitter->emitIns_R_R(INS_staddlh, EA_4BYTE, REG_R8, REG_R10);
    theEmitter->emitIns_R_R(INS_stadd, EA_4BYTE, REG_R8, REG_R10);
    theEmitter->emitIns_R_R(INS_staddl, EA_4BYTE, REG_R8, REG_R10);
    theEmitter->emitIns_R_R(INS_stadd, EA_8BYTE, REG_R8, REG_R10);
    theEmitter->emitIns_R_R(INS_staddl, EA_8BYTE, REG_R8, REG_R10);
#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R_I_I
    //

    genDefineTempLabel(genCreateTempLabel());

    theEmitter->emitIns_R_R_I_I(INS_sbfm, EA_8BYTE, REG_R2, REG_R3, 4, 39);
    theEmitter->emitIns_R_R_I_I(INS_bfm, EA_8BYTE, REG_R1, REG_R5, 20, 23);
    theEmitter->emitIns_R_R_I_I(INS_ubfm, EA_8BYTE, REG_R8, REG_R9, 36, 7);

    theEmitter->emitIns_R_R_I_I(INS_sbfiz, EA_8BYTE, REG_R2, REG_R3, 7, 37);
    theEmitter->emitIns_R_R_I_I(INS_bfi, EA_8BYTE, REG_R1, REG_R5, 23, 21);
    theEmitter->emitIns_R_R_I_I(INS_ubfiz, EA_8BYTE, REG_R8, REG_R9, 39, 5);

    theEmitter->emitIns_R_R_I_I(INS_sbfx, EA_8BYTE, REG_R2, REG_R3, 10, 24);
    theEmitter->emitIns_R_R_I_I(INS_bfxil, EA_8BYTE, REG_R1, REG_R5, 26, 16);
    theEmitter->emitIns_R_R_I_I(INS_ubfx, EA_8BYTE, REG_R8, REG_R9, 42, 8);

    theEmitter->emitIns_R_R_I_I(INS_sbfm, EA_4BYTE, REG_R2, REG_R3, 4, 19);
    theEmitter->emitIns_R_R_I_I(INS_bfm, EA_4BYTE, REG_R1, REG_R5, 10, 13);
    theEmitter->emitIns_R_R_I_I(INS_ubfm, EA_4BYTE, REG_R8, REG_R9, 16, 7);

    theEmitter->emitIns_R_R_I_I(INS_sbfiz, EA_4BYTE, REG_R2, REG_R3, 5, 17);
    theEmitter->emitIns_R_R_I_I(INS_bfi, EA_4BYTE, REG_R1, REG_R5, 13, 11);
    theEmitter->emitIns_R_R_I_I(INS_ubfiz, EA_4BYTE, REG_R8, REG_R9, 19, 5);

    theEmitter->emitIns_R_R_I_I(INS_sbfx, EA_4BYTE, REG_R2, REG_R3, 3, 14);
    theEmitter->emitIns_R_R_I_I(INS_bfxil, EA_4BYTE, REG_R1, REG_R5, 11, 9);
    theEmitter->emitIns_R_R_I_I(INS_ubfx, EA_4BYTE, REG_R8, REG_R9, 22, 8);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R_R_I
    //

    genDefineTempLabel(genCreateTempLabel());

    // ADD (extended register)
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_UXTB);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_UXTH);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_SXTB);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_SXTH);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_SXTX);

    // ADD (extended register) and left shift
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_UXTB);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_UXTH);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_SXTB);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_SXTH);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_SXTX);

    // ADD (shifted register)
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 31, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 32, INS_OPTS_LSR);
    theEmitter->emitIns_R_R_R_I(INS_add, EA_8BYTE, REG_R8, REG_R9, REG_R10, 33, INS_OPTS_ASR);

    // EXTR (extract field from register pair)
    theEmitter->emitIns_R_R_R_I(INS_extr, EA_8BYTE, REG_R8, REG_R9, REG_R10, 1);
    theEmitter->emitIns_R_R_R_I(INS_extr, EA_8BYTE, REG_R8, REG_R9, REG_R10, 31);
    theEmitter->emitIns_R_R_R_I(INS_extr, EA_8BYTE, REG_R8, REG_R9, REG_R10, 32);
    theEmitter->emitIns_R_R_R_I(INS_extr, EA_8BYTE, REG_R8, REG_R9, REG_R10, 63);

    theEmitter->emitIns_R_R_R_I(INS_extr, EA_4BYTE, REG_R8, REG_R9, REG_R10, 1);
    theEmitter->emitIns_R_R_R_I(INS_extr, EA_4BYTE, REG_R8, REG_R9, REG_R10, 31);

    // SUB (extended register)
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_UXTB);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_UXTH);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_SXTB);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_SXTH);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0, INS_OPTS_SXTX);

    // SUB (extended register) and left shift
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_UXTB);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_UXTH);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_SXTB);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_SXTH);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_SXTX);

    // SUB (shifted register)
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 27, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 28, INS_OPTS_LSR);
    theEmitter->emitIns_R_R_R_I(INS_sub, EA_4BYTE, REG_R8, REG_R9, REG_R10, 29, INS_OPTS_ASR);

    // bit operations
    theEmitter->emitIns_R_R_R_I(INS_and, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_ands, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_eor, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_orr, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_bic, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_bics, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_eon, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_orn, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0);

    theEmitter->emitIns_R_R_R_I(INS_and, EA_8BYTE, REG_R8, REG_R9, REG_R10, 1, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_I(INS_ands, EA_8BYTE, REG_R8, REG_R9, REG_R10, 2, INS_OPTS_LSR);
    theEmitter->emitIns_R_R_R_I(INS_eor, EA_8BYTE, REG_R8, REG_R9, REG_R10, 3, INS_OPTS_ASR);
    theEmitter->emitIns_R_R_R_I(INS_orr, EA_8BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_ROR);
    theEmitter->emitIns_R_R_R_I(INS_bic, EA_8BYTE, REG_R8, REG_R9, REG_R10, 5, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_I(INS_bics, EA_8BYTE, REG_R8, REG_R9, REG_R10, 6, INS_OPTS_LSR);
    theEmitter->emitIns_R_R_R_I(INS_eon, EA_8BYTE, REG_R8, REG_R9, REG_R10, 7, INS_OPTS_ASR);
    theEmitter->emitIns_R_R_R_I(INS_orn, EA_8BYTE, REG_R8, REG_R9, REG_R10, 8, INS_OPTS_ROR);

    theEmitter->emitIns_R_R_R_I(INS_and, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_ands, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_eor, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_orr, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_bic, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_bics, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_eon, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_orn, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0);

    theEmitter->emitIns_R_R_R_I(INS_and, EA_4BYTE, REG_R8, REG_R9, REG_R10, 1, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_I(INS_ands, EA_4BYTE, REG_R8, REG_R9, REG_R10, 2, INS_OPTS_LSR);
    theEmitter->emitIns_R_R_R_I(INS_eor, EA_4BYTE, REG_R8, REG_R9, REG_R10, 3, INS_OPTS_ASR);
    theEmitter->emitIns_R_R_R_I(INS_orr, EA_4BYTE, REG_R8, REG_R9, REG_R10, 4, INS_OPTS_ROR);
    theEmitter->emitIns_R_R_R_I(INS_bic, EA_4BYTE, REG_R8, REG_R9, REG_R10, 5, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_I(INS_bics, EA_4BYTE, REG_R8, REG_R9, REG_R10, 6, INS_OPTS_LSR);
    theEmitter->emitIns_R_R_R_I(INS_eon, EA_4BYTE, REG_R8, REG_R9, REG_R10, 7, INS_OPTS_ASR);
    theEmitter->emitIns_R_R_R_I(INS_orn, EA_4BYTE, REG_R8, REG_R9, REG_R10, 8, INS_OPTS_ROR);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R_R_I  -- load/store pair
    //

    theEmitter->emitIns_R_R_R_I(INS_ldnp, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_stnp, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_ldnp, EA_8BYTE, REG_R8, REG_R9, REG_R10, 8);
    theEmitter->emitIns_R_R_R_I(INS_stnp, EA_8BYTE, REG_R8, REG_R9, REG_R10, 8);

    theEmitter->emitIns_R_R_R_I(INS_ldnp, EA_4BYTE, REG_R8, REG_R9, REG_SP, 0);
    theEmitter->emitIns_R_R_R_I(INS_stnp, EA_4BYTE, REG_R8, REG_R9, REG_SP, 0);
    theEmitter->emitIns_R_R_R_I(INS_ldnp, EA_4BYTE, REG_R8, REG_R9, REG_SP, 8);
    theEmitter->emitIns_R_R_R_I(INS_stnp, EA_4BYTE, REG_R8, REG_R9, REG_SP, 8);

    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_8BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_8BYTE, REG_R8, REG_R9, REG_R10, 16);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_8BYTE, REG_R8, REG_R9, REG_R10, 16);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_8BYTE, REG_R8, REG_R9, REG_R10, 16, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_8BYTE, REG_R8, REG_R9, REG_R10, 16, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_8BYTE, REG_R8, REG_R9, REG_R10, 16, INS_OPTS_PRE_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_8BYTE, REG_R8, REG_R9, REG_R10, 16, INS_OPTS_PRE_INDEX);

    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_4BYTE, REG_R8, REG_R9, REG_SP, 0);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_4BYTE, REG_R8, REG_R9, REG_SP, 0);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_4BYTE, REG_R8, REG_R9, REG_SP, 16);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_4BYTE, REG_R8, REG_R9, REG_SP, 16);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_4BYTE, REG_R8, REG_R9, REG_R10, 16, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_4BYTE, REG_R8, REG_R9, REG_R10, 16, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_4BYTE, REG_R8, REG_R9, REG_R10, 16, INS_OPTS_PRE_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_4BYTE, REG_R8, REG_R9, REG_R10, 16, INS_OPTS_PRE_INDEX);

    theEmitter->emitIns_R_R_R_I(INS_ldpsw, EA_4BYTE, REG_R8, REG_R9, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_ldpsw, EA_4BYTE, REG_R8, REG_R9, REG_R10, 16);
    theEmitter->emitIns_R_R_R_I(INS_ldpsw, EA_4BYTE, REG_R8, REG_R9, REG_R10, 16, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ldpsw, EA_4BYTE, REG_R8, REG_R9, REG_R10, 16, INS_OPTS_PRE_INDEX);

    // SP and ZR tests
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_8BYTE, REG_ZR, REG_R1, REG_SP, 0);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_8BYTE, REG_R0, REG_ZR, REG_SP, 16);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_8BYTE, REG_ZR, REG_R1, REG_SP, 0);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_8BYTE, REG_R0, REG_ZR, REG_SP, 16);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_8BYTE, REG_ZR, REG_ZR, REG_SP, 16, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_8BYTE, REG_ZR, REG_ZR, REG_R8, 16, INS_OPTS_PRE_INDEX);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R_R_Ext    -- load/store shifted/extend
    //

    genDefineTempLabel(genCreateTempLabel());

    // LDR (register)
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_R8, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL, 3);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW, 3);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW, 3);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX, 3);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX, 3);

    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_R8, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX, 2);

    theEmitter->emitIns_R_R_R_Ext(INS_ldrh, EA_2BYTE, REG_R8, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX, 1);

    theEmitter->emitIns_R_R_R_Ext(INS_ldrb, EA_1BYTE, REG_R8, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrb, EA_1BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrb, EA_1BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrb, EA_1BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrb, EA_1BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX);

    theEmitter->emitIns_R_R_R_Ext(INS_ldrsw, EA_4BYTE, REG_R8, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsw, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsw, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsw, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsw, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsw, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsw, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsw, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsw, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsw, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsw, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX, 2);

    theEmitter->emitIns_R_R_R_Ext(INS_ldrsh, EA_4BYTE, REG_R8, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsh, EA_8BYTE, REG_R8, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsh, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsh, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsh, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsh, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsh, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsh, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsh, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsh, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsh, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsh, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX, 1);

    theEmitter->emitIns_R_R_R_Ext(INS_ldrsb, EA_4BYTE, REG_R8, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsb, EA_8BYTE, REG_R8, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsb, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsb, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsb, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldrsb, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX);

    // STR (register)
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_8BYTE, REG_R8, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL, 3);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW, 3);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW, 3);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX, 3);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_8BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX, 3);

    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_4BYTE, REG_R8, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_str, EA_4BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX, 2);

    theEmitter->emitIns_R_R_R_Ext(INS_strh, EA_2BYTE, REG_R8, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_strh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_Ext(INS_strh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_LSL, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_strh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_strh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_strh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_strh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_strh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_strh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_strh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_strh, EA_2BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX, 1);

    theEmitter->emitIns_R_R_R_Ext(INS_strb, EA_1BYTE, REG_R8, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_strb, EA_1BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_strb, EA_1BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_strb, EA_1BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_strb, EA_1BYTE, REG_R8, REG_SP, REG_R9, INS_OPTS_UXTX);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R_R_R
    //

    genDefineTempLabel(genCreateTempLabel());

    theEmitter->emitIns_R_R_R_R(INS_madd, EA_4BYTE, REG_R0, REG_R12, REG_R27, REG_R10);
    theEmitter->emitIns_R_R_R_R(INS_msub, EA_4BYTE, REG_R1, REG_R13, REG_R28, REG_R11);
    theEmitter->emitIns_R_R_R_R(INS_smaddl, EA_4BYTE, REG_R2, REG_R14, REG_R0, REG_R12);
    theEmitter->emitIns_R_R_R_R(INS_smsubl, EA_4BYTE, REG_R3, REG_R15, REG_R1, REG_R13);
    theEmitter->emitIns_R_R_R_R(INS_umaddl, EA_4BYTE, REG_R4, REG_R19, REG_R2, REG_R14);
    theEmitter->emitIns_R_R_R_R(INS_umsubl, EA_4BYTE, REG_R5, REG_R20, REG_R3, REG_R15);

    theEmitter->emitIns_R_R_R_R(INS_madd, EA_8BYTE, REG_R6, REG_R21, REG_R4, REG_R19);
    theEmitter->emitIns_R_R_R_R(INS_msub, EA_8BYTE, REG_R7, REG_R22, REG_R5, REG_R20);
    theEmitter->emitIns_R_R_R_R(INS_smaddl, EA_8BYTE, REG_R8, REG_R23, REG_R6, REG_R21);
    theEmitter->emitIns_R_R_R_R(INS_smsubl, EA_8BYTE, REG_R9, REG_R24, REG_R7, REG_R22);
    theEmitter->emitIns_R_R_R_R(INS_umaddl, EA_8BYTE, REG_R10, REG_R25, REG_R8, REG_R23);
    theEmitter->emitIns_R_R_R_R(INS_umsubl, EA_8BYTE, REG_R11, REG_R26, REG_R9, REG_R24);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    // R_COND
    //

    // cset reg, cond
    theEmitter->emitIns_R_COND(INS_cset, EA_8BYTE, REG_R9, INS_COND_EQ); // eq
    theEmitter->emitIns_R_COND(INS_cset, EA_4BYTE, REG_R8, INS_COND_NE); // ne
    theEmitter->emitIns_R_COND(INS_cset, EA_4BYTE, REG_R7, INS_COND_HS); // hs
    theEmitter->emitIns_R_COND(INS_cset, EA_8BYTE, REG_R6, INS_COND_LO); // lo
    theEmitter->emitIns_R_COND(INS_cset, EA_8BYTE, REG_R5, INS_COND_MI); // mi
    theEmitter->emitIns_R_COND(INS_cset, EA_4BYTE, REG_R4, INS_COND_PL); // pl
    theEmitter->emitIns_R_COND(INS_cset, EA_4BYTE, REG_R3, INS_COND_VS); // vs
    theEmitter->emitIns_R_COND(INS_cset, EA_8BYTE, REG_R2, INS_COND_VC); // vc
    theEmitter->emitIns_R_COND(INS_cset, EA_8BYTE, REG_R1, INS_COND_HI); // hi
    theEmitter->emitIns_R_COND(INS_cset, EA_4BYTE, REG_R0, INS_COND_LS); // ls
    theEmitter->emitIns_R_COND(INS_cset, EA_4BYTE, REG_R9, INS_COND_GE); // ge
    theEmitter->emitIns_R_COND(INS_cset, EA_8BYTE, REG_R8, INS_COND_LT); // lt
    theEmitter->emitIns_R_COND(INS_cset, EA_8BYTE, REG_R7, INS_COND_GT); // gt
    theEmitter->emitIns_R_COND(INS_cset, EA_4BYTE, REG_R6, INS_COND_LE); // le

    // csetm reg, cond
    theEmitter->emitIns_R_COND(INS_csetm, EA_4BYTE, REG_R9, INS_COND_EQ); // eq
    theEmitter->emitIns_R_COND(INS_csetm, EA_8BYTE, REG_R8, INS_COND_NE); // ne
    theEmitter->emitIns_R_COND(INS_csetm, EA_8BYTE, REG_R7, INS_COND_HS); // hs
    theEmitter->emitIns_R_COND(INS_csetm, EA_4BYTE, REG_R6, INS_COND_LO); // lo
    theEmitter->emitIns_R_COND(INS_csetm, EA_4BYTE, REG_R5, INS_COND_MI); // mi
    theEmitter->emitIns_R_COND(INS_csetm, EA_8BYTE, REG_R4, INS_COND_PL); // pl
    theEmitter->emitIns_R_COND(INS_csetm, EA_8BYTE, REG_R3, INS_COND_VS); // vs
    theEmitter->emitIns_R_COND(INS_csetm, EA_4BYTE, REG_R2, INS_COND_VC); // vc
    theEmitter->emitIns_R_COND(INS_csetm, EA_4BYTE, REG_R1, INS_COND_HI); // hi
    theEmitter->emitIns_R_COND(INS_csetm, EA_8BYTE, REG_R0, INS_COND_LS); // ls
    theEmitter->emitIns_R_COND(INS_csetm, EA_8BYTE, REG_R9, INS_COND_GE); // ge
    theEmitter->emitIns_R_COND(INS_csetm, EA_4BYTE, REG_R8, INS_COND_LT); // lt
    theEmitter->emitIns_R_COND(INS_csetm, EA_4BYTE, REG_R7, INS_COND_GT); // gt
    theEmitter->emitIns_R_COND(INS_csetm, EA_8BYTE, REG_R6, INS_COND_LE); // le

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    // R_R_COND
    //

    // cinc reg, reg, cond
    // cinv reg, reg, cond
    // cneg reg, reg, cond
    theEmitter->emitIns_R_R_COND(INS_cinc, EA_8BYTE, REG_R0, REG_R4, INS_COND_EQ); // eq
    theEmitter->emitIns_R_R_COND(INS_cinv, EA_4BYTE, REG_R1, REG_R5, INS_COND_NE); // ne
    theEmitter->emitIns_R_R_COND(INS_cneg, EA_4BYTE, REG_R2, REG_R6, INS_COND_HS); // hs
    theEmitter->emitIns_R_R_COND(INS_cinc, EA_8BYTE, REG_R3, REG_R7, INS_COND_LO); // lo
    theEmitter->emitIns_R_R_COND(INS_cinv, EA_4BYTE, REG_R4, REG_R8, INS_COND_MI); // mi
    theEmitter->emitIns_R_R_COND(INS_cneg, EA_8BYTE, REG_R5, REG_R9, INS_COND_PL); // pl
    theEmitter->emitIns_R_R_COND(INS_cinc, EA_8BYTE, REG_R6, REG_R0, INS_COND_VS); // vs
    theEmitter->emitIns_R_R_COND(INS_cinv, EA_4BYTE, REG_R7, REG_R1, INS_COND_VC); // vc
    theEmitter->emitIns_R_R_COND(INS_cneg, EA_8BYTE, REG_R8, REG_R2, INS_COND_HI); // hi
    theEmitter->emitIns_R_R_COND(INS_cinc, EA_4BYTE, REG_R9, REG_R3, INS_COND_LS); // ls
    theEmitter->emitIns_R_R_COND(INS_cinv, EA_4BYTE, REG_R0, REG_R4, INS_COND_GE); // ge
    theEmitter->emitIns_R_R_COND(INS_cneg, EA_8BYTE, REG_R2, REG_R5, INS_COND_LT); // lt
    theEmitter->emitIns_R_R_COND(INS_cinc, EA_4BYTE, REG_R2, REG_R6, INS_COND_GT); // gt
    theEmitter->emitIns_R_R_COND(INS_cinv, EA_8BYTE, REG_R3, REG_R7, INS_COND_LE); // le

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    // R_R_R_COND
    //

    // csel  reg, reg, reg, cond
    // csinc reg, reg, reg, cond
    // csinv reg, reg, reg, cond
    // csneg reg, reg, reg, cond
    theEmitter->emitIns_R_R_R_COND(INS_csel, EA_8BYTE, REG_R0, REG_R4, REG_R8, INS_COND_EQ);  // eq
    theEmitter->emitIns_R_R_R_COND(INS_csinc, EA_4BYTE, REG_R1, REG_R5, REG_R9, INS_COND_NE); // ne
    theEmitter->emitIns_R_R_R_COND(INS_csinv, EA_4BYTE, REG_R2, REG_R6, REG_R0, INS_COND_HS); // hs
    theEmitter->emitIns_R_R_R_COND(INS_csneg, EA_8BYTE, REG_R3, REG_R7, REG_R1, INS_COND_LO); // lo
    theEmitter->emitIns_R_R_R_COND(INS_csel, EA_4BYTE, REG_R4, REG_R8, REG_R2, INS_COND_MI);  // mi
    theEmitter->emitIns_R_R_R_COND(INS_csinc, EA_8BYTE, REG_R5, REG_R9, REG_R3, INS_COND_PL); // pl
    theEmitter->emitIns_R_R_R_COND(INS_csinv, EA_8BYTE, REG_R6, REG_R0, REG_R4, INS_COND_VS); // vs
    theEmitter->emitIns_R_R_R_COND(INS_csneg, EA_4BYTE, REG_R7, REG_R1, REG_R5, INS_COND_VC); // vc
    theEmitter->emitIns_R_R_R_COND(INS_csel, EA_8BYTE, REG_R8, REG_R2, REG_R6, INS_COND_HI);  // hi
    theEmitter->emitIns_R_R_R_COND(INS_csinc, EA_4BYTE, REG_R9, REG_R3, REG_R7, INS_COND_LS); // ls
    theEmitter->emitIns_R_R_R_COND(INS_csinv, EA_4BYTE, REG_R0, REG_R4, REG_R8, INS_COND_GE); // ge
    theEmitter->emitIns_R_R_R_COND(INS_csneg, EA_8BYTE, REG_R2, REG_R5, REG_R9, INS_COND_LT); // lt
    theEmitter->emitIns_R_R_R_COND(INS_csel, EA_4BYTE, REG_R2, REG_R6, REG_R0, INS_COND_GT);  // gt
    theEmitter->emitIns_R_R_R_COND(INS_csinc, EA_8BYTE, REG_R3, REG_R7, REG_R1, INS_COND_LE); // le

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    // R_R_FLAGS_COND
    //

    // ccmp reg1, reg2, nzcv, cond
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R9, REG_R3, INS_FLAGS_V, INS_COND_EQ);    // eq
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R8, REG_R2, INS_FLAGS_C, INS_COND_NE);    // ne
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R7, REG_R1, INS_FLAGS_Z, INS_COND_HS);    // hs
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R6, REG_R0, INS_FLAGS_N, INS_COND_LO);    // lo
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R5, REG_R3, INS_FLAGS_CV, INS_COND_MI);   // mi
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R4, REG_R2, INS_FLAGS_ZV, INS_COND_PL);   // pl
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R3, REG_R1, INS_FLAGS_ZC, INS_COND_VS);   // vs
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R2, REG_R0, INS_FLAGS_NV, INS_COND_VC);   // vc
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R1, REG_R3, INS_FLAGS_NC, INS_COND_HI);   // hi
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R0, REG_R2, INS_FLAGS_NZ, INS_COND_LS);   // ls
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R9, REG_R1, INS_FLAGS_NONE, INS_COND_GE); // ge
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R8, REG_R0, INS_FLAGS_NZV, INS_COND_LT);  // lt
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R7, REG_R3, INS_FLAGS_NZC, INS_COND_GT);  // gt
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R6, REG_R2, INS_FLAGS_NZCV, INS_COND_LE); // le

    // ccmp reg1, imm, nzcv, cond
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R9, 3, INS_FLAGS_V, INS_COND_EQ);     // eq
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R8, 2, INS_FLAGS_C, INS_COND_NE);     // ne
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R7, 1, INS_FLAGS_Z, INS_COND_HS);     // hs
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R6, 0, INS_FLAGS_N, INS_COND_LO);     // lo
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R5, 31, INS_FLAGS_CV, INS_COND_MI);   // mi
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R4, 28, INS_FLAGS_ZV, INS_COND_PL);   // pl
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R3, 25, INS_FLAGS_ZC, INS_COND_VS);   // vs
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R2, 22, INS_FLAGS_NV, INS_COND_VC);   // vc
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R1, 19, INS_FLAGS_NC, INS_COND_HI);   // hi
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R0, 16, INS_FLAGS_NZ, INS_COND_LS);   // ls
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R9, 13, INS_FLAGS_NONE, INS_COND_GE); // ge
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R8, 10, INS_FLAGS_NZV, INS_COND_LT);  // lt
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R7, 7, INS_FLAGS_NZC, INS_COND_GT);   // gt
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R6, 4, INS_FLAGS_NZCV, INS_COND_LE);  // le

    // ccmp reg1, imm, nzcv, cond  -- encoded as ccmn
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R9, -3, INS_FLAGS_V, INS_COND_EQ);     // eq
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R8, -2, INS_FLAGS_C, INS_COND_NE);     // ne
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R7, -1, INS_FLAGS_Z, INS_COND_HS);     // hs
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R6, -5, INS_FLAGS_N, INS_COND_LO);     // lo
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R5, -31, INS_FLAGS_CV, INS_COND_MI);   // mi
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R4, -28, INS_FLAGS_ZV, INS_COND_PL);   // pl
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R3, -25, INS_FLAGS_ZC, INS_COND_VS);   // vs
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R2, -22, INS_FLAGS_NV, INS_COND_VC);   // vc
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R1, -19, INS_FLAGS_NC, INS_COND_HI);   // hi
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R0, -16, INS_FLAGS_NZ, INS_COND_LS);   // ls
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R9, -13, INS_FLAGS_NONE, INS_COND_GE); // ge
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R8, -10, INS_FLAGS_NZV, INS_COND_LT);  // lt
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_8BYTE, REG_R7, -7, INS_FLAGS_NZC, INS_COND_GT);   // gt
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmp, EA_4BYTE, REG_R6, -4, INS_FLAGS_NZCV, INS_COND_LE);  // le

    // ccmn reg1, reg2, nzcv, cond
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmn, EA_8BYTE, REG_R9, REG_R3, INS_FLAGS_V, INS_COND_EQ);    // eq
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmn, EA_4BYTE, REG_R8, REG_R2, INS_FLAGS_C, INS_COND_NE);    // ne
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmn, EA_4BYTE, REG_R7, REG_R1, INS_FLAGS_Z, INS_COND_HS);    // hs
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmn, EA_8BYTE, REG_R6, REG_R0, INS_FLAGS_N, INS_COND_LO);    // lo
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmn, EA_8BYTE, REG_R5, REG_R3, INS_FLAGS_CV, INS_COND_MI);   // mi
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmn, EA_4BYTE, REG_R4, REG_R2, INS_FLAGS_ZV, INS_COND_PL);   // pl
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmn, EA_4BYTE, REG_R3, REG_R1, INS_FLAGS_ZC, INS_COND_VS);   // vs
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmn, EA_8BYTE, REG_R2, REG_R0, INS_FLAGS_NV, INS_COND_VC);   // vc
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmn, EA_8BYTE, REG_R1, REG_R3, INS_FLAGS_NC, INS_COND_HI);   // hi
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmn, EA_4BYTE, REG_R0, REG_R2, INS_FLAGS_NZ, INS_COND_LS);   // ls
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmn, EA_4BYTE, REG_R9, REG_R1, INS_FLAGS_NONE, INS_COND_GE); // ge
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmn, EA_8BYTE, REG_R8, REG_R0, INS_FLAGS_NZV, INS_COND_LT);  // lt
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmn, EA_8BYTE, REG_R7, REG_R3, INS_FLAGS_NZC, INS_COND_GT);  // gt
    theEmitter->emitIns_R_R_FLAGS_COND(INS_ccmn, EA_4BYTE, REG_R6, REG_R2, INS_FLAGS_NZCV, INS_COND_LE); // le

    // ccmn reg1, imm, nzcv, cond
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmn, EA_8BYTE, REG_R9, 3, INS_FLAGS_V, INS_COND_EQ);     // eq
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmn, EA_4BYTE, REG_R8, 2, INS_FLAGS_C, INS_COND_NE);     // ne
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmn, EA_4BYTE, REG_R7, 1, INS_FLAGS_Z, INS_COND_HS);     // hs
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmn, EA_8BYTE, REG_R6, 0, INS_FLAGS_N, INS_COND_LO);     // lo
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmn, EA_8BYTE, REG_R5, 31, INS_FLAGS_CV, INS_COND_MI);   // mi
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmn, EA_4BYTE, REG_R4, 28, INS_FLAGS_ZV, INS_COND_PL);   // pl
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmn, EA_4BYTE, REG_R3, 25, INS_FLAGS_ZC, INS_COND_VS);   // vs
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmn, EA_8BYTE, REG_R2, 22, INS_FLAGS_NV, INS_COND_VC);   // vc
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmn, EA_8BYTE, REG_R1, 19, INS_FLAGS_NC, INS_COND_HI);   // hi
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmn, EA_4BYTE, REG_R0, 16, INS_FLAGS_NZ, INS_COND_LS);   // ls
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmn, EA_4BYTE, REG_R9, 13, INS_FLAGS_NONE, INS_COND_GE); // ge
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmn, EA_8BYTE, REG_R8, 10, INS_FLAGS_NZV, INS_COND_LT);  // lt
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmn, EA_8BYTE, REG_R7, 7, INS_FLAGS_NZC, INS_COND_GT);   // gt
    theEmitter->emitIns_R_I_FLAGS_COND(INS_ccmn, EA_4BYTE, REG_R6, 4, INS_FLAGS_NZCV, INS_COND_LE);  // le

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // Branch to register
    //

    genDefineTempLabel(genCreateTempLabel());

    theEmitter->emitIns_R(INS_br, EA_PTRSIZE, REG_R8);
    theEmitter->emitIns_R(INS_ret, EA_PTRSIZE, REG_R8);
    theEmitter->emitIns_R(INS_ret, EA_PTRSIZE, REG_LR);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // Misc
    //

    genDefineTempLabel(genCreateTempLabel());

    theEmitter->emitIns_I(INS_brk, EA_PTRSIZE, 0);
    theEmitter->emitIns_I(INS_brk, EA_PTRSIZE, 65535);

    theEmitter->emitIns_BARR(INS_dsb, INS_BARRIER_OSHLD);
    theEmitter->emitIns_BARR(INS_dmb, INS_BARRIER_OSHST);
    theEmitter->emitIns_BARR(INS_isb, INS_BARRIER_OSH);

    theEmitter->emitIns_BARR(INS_dmb, INS_BARRIER_NSHLD);
    theEmitter->emitIns_BARR(INS_isb, INS_BARRIER_NSHST);
    theEmitter->emitIns_BARR(INS_dsb, INS_BARRIER_NSH);

    theEmitter->emitIns_BARR(INS_isb, INS_BARRIER_ISHLD);
    theEmitter->emitIns_BARR(INS_dsb, INS_BARRIER_ISHST);
    theEmitter->emitIns_BARR(INS_dmb, INS_BARRIER_ISH);

    theEmitter->emitIns_BARR(INS_dsb, INS_BARRIER_LD);
    theEmitter->emitIns_BARR(INS_dmb, INS_BARRIER_ST);
    theEmitter->emitIns_BARR(INS_isb, INS_BARRIER_SY);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    ////////////////////////////////////////////////////////////////////////////////
    //
    // SIMD and Floating point
    //
    ////////////////////////////////////////////////////////////////////////////////

    //
    // Load/Stores vector register
    //

    genDefineTempLabel(genCreateTempLabel());

    // ldr/str Vt, [reg]
    theEmitter->emitIns_R_R(INS_ldr, EA_8BYTE, REG_V1, REG_R9);
    theEmitter->emitIns_R_R(INS_str, EA_8BYTE, REG_V2, REG_R8);
    theEmitter->emitIns_R_R(INS_ldr, EA_4BYTE, REG_V3, REG_R7);
    theEmitter->emitIns_R_R(INS_str, EA_4BYTE, REG_V4, REG_R6);
    theEmitter->emitIns_R_R(INS_ldr, EA_2BYTE, REG_V5, REG_R5);
    theEmitter->emitIns_R_R(INS_str, EA_2BYTE, REG_V6, REG_R4);
    theEmitter->emitIns_R_R(INS_ldr, EA_1BYTE, REG_V7, REG_R3);
    theEmitter->emitIns_R_R(INS_str, EA_1BYTE, REG_V8, REG_R2);
    theEmitter->emitIns_R_R(INS_ldr, EA_16BYTE, REG_V9, REG_R1);
    theEmitter->emitIns_R_R(INS_str, EA_16BYTE, REG_V10, REG_R0);

    // ldr/str Vt, [reg+cns]        -- scaled
    theEmitter->emitIns_R_R_I(INS_ldr, EA_1BYTE, REG_V8, REG_R9, 1);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_2BYTE, REG_V8, REG_R9, 2);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_4BYTE, REG_V8, REG_R9, 4);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_8BYTE, REG_V8, REG_R9, 8);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_16BYTE, REG_V8, REG_R9, 16);

    theEmitter->emitIns_R_R_I(INS_ldr, EA_1BYTE, REG_V7, REG_R10, 1);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_2BYTE, REG_V7, REG_R10, 2);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_4BYTE, REG_V7, REG_R10, 4);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_8BYTE, REG_V7, REG_R10, 8);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_16BYTE, REG_V7, REG_R10, 16);

    // ldr/str Vt, [reg],cns        -- post-indexed (unscaled)
    // ldr/str Vt, [reg+cns]!       -- post-indexed (unscaled)
    theEmitter->emitIns_R_R_I(INS_ldr, EA_1BYTE, REG_V8, REG_R9, 1, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_2BYTE, REG_V8, REG_R9, 1, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_4BYTE, REG_V8, REG_R9, 1, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_8BYTE, REG_V8, REG_R9, 1, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_16BYTE, REG_V8, REG_R9, 1, INS_OPTS_POST_INDEX);

    theEmitter->emitIns_R_R_I(INS_ldr, EA_1BYTE, REG_V8, REG_R9, 1, INS_OPTS_PRE_INDEX);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_2BYTE, REG_V8, REG_R9, 1, INS_OPTS_PRE_INDEX);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_4BYTE, REG_V8, REG_R9, 1, INS_OPTS_PRE_INDEX);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_8BYTE, REG_V8, REG_R9, 1, INS_OPTS_PRE_INDEX);
    theEmitter->emitIns_R_R_I(INS_ldr, EA_16BYTE, REG_V8, REG_R9, 1, INS_OPTS_PRE_INDEX);

    theEmitter->emitIns_R_R_I(INS_str, EA_1BYTE, REG_V8, REG_R9, 1, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I(INS_str, EA_2BYTE, REG_V8, REG_R9, 1, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I(INS_str, EA_4BYTE, REG_V8, REG_R9, 1, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I(INS_str, EA_8BYTE, REG_V8, REG_R9, 1, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_I(INS_str, EA_16BYTE, REG_V8, REG_R9, 1, INS_OPTS_POST_INDEX);

    theEmitter->emitIns_R_R_I(INS_str, EA_1BYTE, REG_V8, REG_R9, 1, INS_OPTS_PRE_INDEX);
    theEmitter->emitIns_R_R_I(INS_str, EA_2BYTE, REG_V8, REG_R9, 1, INS_OPTS_PRE_INDEX);
    theEmitter->emitIns_R_R_I(INS_str, EA_4BYTE, REG_V8, REG_R9, 1, INS_OPTS_PRE_INDEX);
    theEmitter->emitIns_R_R_I(INS_str, EA_8BYTE, REG_V8, REG_R9, 1, INS_OPTS_PRE_INDEX);
    theEmitter->emitIns_R_R_I(INS_str, EA_16BYTE, REG_V8, REG_R9, 1, INS_OPTS_PRE_INDEX);

    theEmitter->emitIns_R_R_I(INS_ldur, EA_1BYTE, REG_V8, REG_R9, 2);
    theEmitter->emitIns_R_R_I(INS_ldur, EA_2BYTE, REG_V8, REG_R9, 3);
    theEmitter->emitIns_R_R_I(INS_ldur, EA_4BYTE, REG_V8, REG_R9, 5);
    theEmitter->emitIns_R_R_I(INS_ldur, EA_8BYTE, REG_V8, REG_R9, 9);
    theEmitter->emitIns_R_R_I(INS_ldur, EA_16BYTE, REG_V8, REG_R9, 17);

    theEmitter->emitIns_R_R_I(INS_stur, EA_1BYTE, REG_V7, REG_R10, 2);
    theEmitter->emitIns_R_R_I(INS_stur, EA_2BYTE, REG_V7, REG_R10, 3);
    theEmitter->emitIns_R_R_I(INS_stur, EA_4BYTE, REG_V7, REG_R10, 5);
    theEmitter->emitIns_R_R_I(INS_stur, EA_8BYTE, REG_V7, REG_R10, 9);
    theEmitter->emitIns_R_R_I(INS_stur, EA_16BYTE, REG_V7, REG_R10, 17);

    // load/store pair
    theEmitter->emitIns_R_R_R(INS_ldnp, EA_8BYTE, REG_V0, REG_V1, REG_R10);
    theEmitter->emitIns_R_R_R_I(INS_stnp, EA_8BYTE, REG_V1, REG_V2, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_ldnp, EA_8BYTE, REG_V2, REG_V3, REG_R10, 8);
    theEmitter->emitIns_R_R_R_I(INS_stnp, EA_8BYTE, REG_V3, REG_V4, REG_R10, 24);

    theEmitter->emitIns_R_R_R(INS_ldnp, EA_4BYTE, REG_V4, REG_V5, REG_SP);
    theEmitter->emitIns_R_R_R_I(INS_stnp, EA_4BYTE, REG_V5, REG_V6, REG_SP, 0);
    theEmitter->emitIns_R_R_R_I(INS_ldnp, EA_4BYTE, REG_V6, REG_V7, REG_SP, 4);
    theEmitter->emitIns_R_R_R_I(INS_stnp, EA_4BYTE, REG_V7, REG_V8, REG_SP, 12);

    theEmitter->emitIns_R_R_R(INS_ldnp, EA_16BYTE, REG_V8, REG_V9, REG_R10);
    theEmitter->emitIns_R_R_R_I(INS_stnp, EA_16BYTE, REG_V9, REG_V10, REG_R10, 0);
    theEmitter->emitIns_R_R_R_I(INS_ldnp, EA_16BYTE, REG_V10, REG_V11, REG_R10, 16);
    theEmitter->emitIns_R_R_R_I(INS_stnp, EA_16BYTE, REG_V11, REG_V12, REG_R10, 48);

    theEmitter->emitIns_R_R_R(INS_ldp, EA_8BYTE, REG_V0, REG_V1, REG_R10);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_8BYTE, REG_V1, REG_V2, REG_SP, 0);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_8BYTE, REG_V2, REG_V3, REG_SP, 8);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_8BYTE, REG_V3, REG_V4, REG_R10, 16);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_8BYTE, REG_V4, REG_V5, REG_R10, 24, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_8BYTE, REG_V5, REG_V6, REG_SP, 32, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_8BYTE, REG_V6, REG_V7, REG_SP, 40, INS_OPTS_PRE_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_8BYTE, REG_V7, REG_V8, REG_R10, 48, INS_OPTS_PRE_INDEX);

    theEmitter->emitIns_R_R_R(INS_ldp, EA_4BYTE, REG_V0, REG_V1, REG_R10);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_4BYTE, REG_V1, REG_V2, REG_SP, 0);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_4BYTE, REG_V2, REG_V3, REG_SP, 4);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_4BYTE, REG_V3, REG_V4, REG_R10, 8);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_4BYTE, REG_V4, REG_V5, REG_R10, 12, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_4BYTE, REG_V5, REG_V6, REG_SP, 16, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_4BYTE, REG_V6, REG_V7, REG_SP, 20, INS_OPTS_PRE_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_4BYTE, REG_V7, REG_V8, REG_R10, 24, INS_OPTS_PRE_INDEX);

    theEmitter->emitIns_R_R_R(INS_ldp, EA_16BYTE, REG_V0, REG_V1, REG_R10);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_16BYTE, REG_V1, REG_V2, REG_SP, 0);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_16BYTE, REG_V2, REG_V3, REG_SP, 16);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_16BYTE, REG_V3, REG_V4, REG_R10, 32);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_16BYTE, REG_V4, REG_V5, REG_R10, 48, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_16BYTE, REG_V5, REG_V6, REG_SP, 64, INS_OPTS_POST_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_ldp, EA_16BYTE, REG_V6, REG_V7, REG_SP, 80, INS_OPTS_PRE_INDEX);
    theEmitter->emitIns_R_R_R_I(INS_stp, EA_16BYTE, REG_V7, REG_V8, REG_R10, 96, INS_OPTS_PRE_INDEX);

    // LDR (register)
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_V1, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_V2, REG_R7, REG_R9, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_V3, REG_R7, REG_R9, INS_OPTS_LSL, 3);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_V4, REG_R7, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_V5, REG_R7, REG_R9, INS_OPTS_SXTW, 3);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_V6, REG_SP, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_V7, REG_R7, REG_R9, INS_OPTS_UXTW, 3);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_V8, REG_R7, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_V9, REG_R7, REG_R9, INS_OPTS_SXTX, 3);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_V10, REG_R7, REG_R9, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_8BYTE, REG_V11, REG_SP, REG_R9, INS_OPTS_UXTX, 3);

    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_V1, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_V2, REG_R7, REG_R9, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_V3, REG_R7, REG_R9, INS_OPTS_LSL, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_V4, REG_R7, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_V5, REG_R7, REG_R9, INS_OPTS_SXTW, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_V6, REG_SP, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_V7, REG_R7, REG_R9, INS_OPTS_UXTW, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_V8, REG_R7, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_V9, REG_R7, REG_R9, INS_OPTS_SXTX, 2);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_V10, REG_R7, REG_R9, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_4BYTE, REG_V11, REG_SP, REG_R9, INS_OPTS_UXTX, 2);

    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_16BYTE, REG_V1, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_16BYTE, REG_V2, REG_R7, REG_R9, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_16BYTE, REG_V3, REG_R7, REG_R9, INS_OPTS_LSL, 4);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_16BYTE, REG_V4, REG_R7, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_16BYTE, REG_V5, REG_R7, REG_R9, INS_OPTS_SXTW, 4);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_16BYTE, REG_V6, REG_SP, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_16BYTE, REG_V7, REG_R7, REG_R9, INS_OPTS_UXTW, 4);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_16BYTE, REG_V8, REG_R7, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_16BYTE, REG_V9, REG_R7, REG_R9, INS_OPTS_SXTX, 4);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_16BYTE, REG_V10, REG_R7, REG_R9, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_16BYTE, REG_V11, REG_SP, REG_R9, INS_OPTS_UXTX, 4);

    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_2BYTE, REG_V1, REG_SP, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_2BYTE, REG_V2, REG_R7, REG_R9, INS_OPTS_LSL);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_2BYTE, REG_V3, REG_R7, REG_R9, INS_OPTS_LSL, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_2BYTE, REG_V4, REG_R7, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_2BYTE, REG_V5, REG_R7, REG_R9, INS_OPTS_SXTW, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_2BYTE, REG_V6, REG_SP, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_2BYTE, REG_V7, REG_R7, REG_R9, INS_OPTS_UXTW, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_2BYTE, REG_V8, REG_R7, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_2BYTE, REG_V9, REG_R7, REG_R9, INS_OPTS_SXTX, 1);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_2BYTE, REG_V10, REG_R7, REG_R9, INS_OPTS_UXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_2BYTE, REG_V11, REG_SP, REG_R9, INS_OPTS_UXTX, 1);

    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_1BYTE, REG_V1, REG_R7, REG_R9);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_1BYTE, REG_V2, REG_SP, REG_R9, INS_OPTS_SXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_1BYTE, REG_V3, REG_R7, REG_R9, INS_OPTS_UXTW);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_1BYTE, REG_V4, REG_SP, REG_R9, INS_OPTS_SXTX);
    theEmitter->emitIns_R_R_R_Ext(INS_ldr, EA_1BYTE, REG_V5, REG_R7, REG_R9, INS_OPTS_UXTX);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R   mov and aliases for mov
    //

    // mov vector to vector
    theEmitter->emitIns_Mov(INS_mov, EA_8BYTE, REG_V0, REG_V1, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_mov, EA_16BYTE, REG_V2, REG_V3, /* canSkip */ false);

    theEmitter->emitIns_Mov(INS_mov, EA_4BYTE, REG_V12, REG_V13, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_mov, EA_2BYTE, REG_V14, REG_V15, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_mov, EA_1BYTE, REG_V16, REG_V17, /* canSkip */ false);

    // mov vector to general
    theEmitter->emitIns_Mov(INS_mov, EA_8BYTE, REG_R0, REG_V4, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_mov, EA_4BYTE, REG_R1, REG_V5, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_mov, EA_2BYTE, REG_R2, REG_V6, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_mov, EA_1BYTE, REG_R3, REG_V7, /* canSkip */ false);

    // mov general to vector
    theEmitter->emitIns_Mov(INS_mov, EA_8BYTE, REG_V8, REG_R4, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_mov, EA_4BYTE, REG_V9, REG_R5, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_mov, EA_2BYTE, REG_V10, REG_R6, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_mov, EA_1BYTE, REG_V11, REG_R7, /* canSkip */ false);

    // mov vector[index] to vector
    theEmitter->emitIns_R_R_I(INS_mov, EA_8BYTE, REG_V0, REG_V1, 1);
    theEmitter->emitIns_R_R_I(INS_mov, EA_4BYTE, REG_V2, REG_V3, 3);
    theEmitter->emitIns_R_R_I(INS_mov, EA_2BYTE, REG_V4, REG_V5, 7);
    theEmitter->emitIns_R_R_I(INS_mov, EA_1BYTE, REG_V6, REG_V7, 15);

    // mov to general from vector[index]
    theEmitter->emitIns_R_R_I(INS_mov, EA_8BYTE, REG_R8, REG_V16, 1);
    theEmitter->emitIns_R_R_I(INS_mov, EA_4BYTE, REG_R9, REG_V17, 2);
    theEmitter->emitIns_R_R_I(INS_mov, EA_2BYTE, REG_R10, REG_V18, 3);
    theEmitter->emitIns_R_R_I(INS_mov, EA_1BYTE, REG_R11, REG_V19, 4);

    // mov to vector[index] from general
    theEmitter->emitIns_R_R_I(INS_mov, EA_8BYTE, REG_V20, REG_R12, 1);
    theEmitter->emitIns_R_R_I(INS_mov, EA_4BYTE, REG_V21, REG_R13, 2);
    theEmitter->emitIns_R_R_I(INS_mov, EA_2BYTE, REG_V22, REG_R14, 6);
    theEmitter->emitIns_R_R_I(INS_mov, EA_1BYTE, REG_V23, REG_R15, 8);

    // mov vector[index] to vector[index2]
    theEmitter->emitIns_R_R_I_I(INS_mov, EA_8BYTE, REG_V8, REG_V9, 1, 0);
    theEmitter->emitIns_R_R_I_I(INS_mov, EA_4BYTE, REG_V10, REG_V11, 2, 1);
    theEmitter->emitIns_R_R_I_I(INS_mov, EA_2BYTE, REG_V12, REG_V13, 5, 2);
    theEmitter->emitIns_R_R_I_I(INS_mov, EA_1BYTE, REG_V14, REG_V15, 12, 3);

    //////////////////////////////////////////////////////////////////////////////////

    // mov/dup scalar
    theEmitter->emitIns_R_R_I(INS_dup, EA_8BYTE, REG_V24, REG_V25, 1);
    theEmitter->emitIns_R_R_I(INS_dup, EA_4BYTE, REG_V26, REG_V27, 3);
    theEmitter->emitIns_R_R_I(INS_dup, EA_2BYTE, REG_V28, REG_V29, 7);
    theEmitter->emitIns_R_R_I(INS_dup, EA_1BYTE, REG_V30, REG_V31, 15);

    // mov/ins vector element
    theEmitter->emitIns_R_R_I_I(INS_ins, EA_8BYTE, REG_V0, REG_V1, 0, 1);
    theEmitter->emitIns_R_R_I_I(INS_ins, EA_4BYTE, REG_V2, REG_V3, 2, 2);
    theEmitter->emitIns_R_R_I_I(INS_ins, EA_2BYTE, REG_V4, REG_V5, 4, 3);
    theEmitter->emitIns_R_R_I_I(INS_ins, EA_1BYTE, REG_V6, REG_V7, 8, 4);

    // umov to general from vector element
    theEmitter->emitIns_R_R_I(INS_umov, EA_8BYTE, REG_R0, REG_V8, 1);
    theEmitter->emitIns_R_R_I(INS_umov, EA_4BYTE, REG_R1, REG_V9, 2);
    theEmitter->emitIns_R_R_I(INS_umov, EA_2BYTE, REG_R2, REG_V10, 4);
    theEmitter->emitIns_R_R_I(INS_umov, EA_1BYTE, REG_R3, REG_V11, 8);

    // ins to vector element from general
    theEmitter->emitIns_R_R_I(INS_ins, EA_8BYTE, REG_V12, REG_R4, 1);
    theEmitter->emitIns_R_R_I(INS_ins, EA_4BYTE, REG_V13, REG_R5, 3);
    theEmitter->emitIns_R_R_I(INS_ins, EA_2BYTE, REG_V14, REG_R6, 7);
    theEmitter->emitIns_R_R_I(INS_ins, EA_1BYTE, REG_V15, REG_R7, 15);

    // smov to general from vector element
    theEmitter->emitIns_R_R_I(INS_smov, EA_4BYTE, REG_R5, REG_V17, 2);
    theEmitter->emitIns_R_R_I(INS_smov, EA_2BYTE, REG_R6, REG_V18, 4);
    theEmitter->emitIns_R_R_I(INS_smov, EA_1BYTE, REG_R7, REG_V19, 8);

    // ext extract vector from pair of vectors
    theEmitter->emitIns_R_R_R_I(INS_ext, EA_8BYTE, REG_V0, REG_V1, REG_V2, 3, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R_I(INS_ext, EA_8BYTE, REG_V4, REG_V5, REG_V6, 7, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R_I(INS_ext, EA_16BYTE, REG_V8, REG_V9, REG_V10, 11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R_I(INS_ext, EA_16BYTE, REG_V12, REG_V13, REG_V14, 15, INS_OPTS_16B);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_I   movi and mvni
    //

    // movi  imm8  (vector)
    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V0, 0x00, INS_OPTS_8B);
    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V1, 0xFF, INS_OPTS_8B);
    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V2, 0x00, INS_OPTS_16B);
    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V3, 0xFF, INS_OPTS_16B);

    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V4, 0x007F, INS_OPTS_4H);
    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V5, 0x7F00, INS_OPTS_4H); // LSL  8
    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V6, 0x003F, INS_OPTS_8H);
    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V7, 0x3F00, INS_OPTS_8H); // LSL  8

    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V8, 0x1F, INS_OPTS_2S);
    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V9, 0x1F00, INS_OPTS_2S);      // LSL  8
    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V10, 0x1F0000, INS_OPTS_2S);   // LSL 16
    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V11, 0x1F000000, INS_OPTS_2S); // LSL 24

    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V12, 0x1FFF, INS_OPTS_2S);   // MSL  8
    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V13, 0x1FFFFF, INS_OPTS_2S); // MSL 16

    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V14, 0x37, INS_OPTS_4S);
    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V15, 0x3700, INS_OPTS_4S);     // LSL  8
    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V16, 0x370000, INS_OPTS_4S);   // LSL 16
    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V17, 0x37000000, INS_OPTS_4S); // LSL 24

    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V18, 0x37FF, INS_OPTS_4S);   // MSL  8
    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V19, 0x37FFFF, INS_OPTS_4S); // MSL 16

    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V20, 0xFF80, INS_OPTS_4H);  // mvni
    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V21, 0xFFC0, INS_OPTS_8H); // mvni

    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V22, 0xFFFFFFE0, INS_OPTS_2S);  // mvni
    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V23, 0xFFFFF0FF, INS_OPTS_4S); // mvni LSL  8
    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V24, 0xFFF8FFFF, INS_OPTS_2S);  // mvni LSL 16
    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V25, 0xFCFFFFFF, INS_OPTS_4S); // mvni LSL 24

    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V26, 0xFFFFFE00, INS_OPTS_2S);  // mvni MSL  8
    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V27, 0xFFFC0000, INS_OPTS_4S); // mvni MSL 16

    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V28, 0x00FF00FF00FF00FF, INS_OPTS_1D);
    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V29, 0x00FFFF0000FFFF00, INS_OPTS_2D);
    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V30, 0xFF000000FF000000);
    theEmitter->emitIns_R_I(INS_movi, EA_16BYTE, REG_V31, 0x0, INS_OPTS_2D);

    // We were not encoding immediate of movi that was int.MaxValue or int.MaxValue / 2.
    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V16, 0x7fffffff, INS_OPTS_2S);
    theEmitter->emitIns_R_I(INS_movi, EA_8BYTE, REG_V16, 0x3fffffff, INS_OPTS_2S);
#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_I   orr/bic vector immediate
    //

    theEmitter->emitIns_R_I(INS_orr, EA_8BYTE, REG_V0, 0x0022, INS_OPTS_4H);
    theEmitter->emitIns_R_I(INS_orr, EA_8BYTE, REG_V1, 0x2200, INS_OPTS_4H); // LSL  8
    theEmitter->emitIns_R_I(INS_orr, EA_16BYTE, REG_V2, 0x0033, INS_OPTS_8H);
    theEmitter->emitIns_R_I(INS_orr, EA_16BYTE, REG_V3, 0x3300, INS_OPTS_8H); // LSL  8

    theEmitter->emitIns_R_I(INS_orr, EA_8BYTE, REG_V4, 0x42, INS_OPTS_2S);
    theEmitter->emitIns_R_I(INS_orr, EA_8BYTE, REG_V5, 0x4200, INS_OPTS_2S);     // LSL  8
    theEmitter->emitIns_R_I(INS_orr, EA_8BYTE, REG_V6, 0x420000, INS_OPTS_2S);   // LSL 16
    theEmitter->emitIns_R_I(INS_orr, EA_8BYTE, REG_V7, 0x42000000, INS_OPTS_2S); // LSL 24

    theEmitter->emitIns_R_I(INS_orr, EA_16BYTE, REG_V10, 0x5D, INS_OPTS_4S);
    theEmitter->emitIns_R_I(INS_orr, EA_16BYTE, REG_V11, 0x5D00, INS_OPTS_4S);     // LSL  8
    theEmitter->emitIns_R_I(INS_orr, EA_16BYTE, REG_V12, 0x5D0000, INS_OPTS_4S);   // LSL 16
    theEmitter->emitIns_R_I(INS_orr, EA_16BYTE, REG_V13, 0x5D000000, INS_OPTS_4S); // LSL 24

    theEmitter->emitIns_R_I(INS_bic, EA_8BYTE, REG_V0, 0x0022, INS_OPTS_4H);
    theEmitter->emitIns_R_I(INS_bic, EA_8BYTE, REG_V1, 0x2200, INS_OPTS_4H); // LSL  8
    theEmitter->emitIns_R_I(INS_bic, EA_16BYTE, REG_V2, 0x0033, INS_OPTS_8H);
    theEmitter->emitIns_R_I(INS_bic, EA_16BYTE, REG_V3, 0x3300, INS_OPTS_8H); // LSL  8

    theEmitter->emitIns_R_I(INS_bic, EA_8BYTE, REG_V4, 0x42, INS_OPTS_2S);
    theEmitter->emitIns_R_I(INS_bic, EA_8BYTE, REG_V5, 0x4200, INS_OPTS_2S);     // LSL  8
    theEmitter->emitIns_R_I(INS_bic, EA_8BYTE, REG_V6, 0x420000, INS_OPTS_2S);   // LSL 16
    theEmitter->emitIns_R_I(INS_bic, EA_8BYTE, REG_V7, 0x42000000, INS_OPTS_2S); // LSL 24

    theEmitter->emitIns_R_I(INS_bic, EA_16BYTE, REG_V10, 0x5D, INS_OPTS_4S);
    theEmitter->emitIns_R_I(INS_bic, EA_16BYTE, REG_V11, 0x5D00, INS_OPTS_4S);     // LSL  8
    theEmitter->emitIns_R_I(INS_bic, EA_16BYTE, REG_V12, 0x5D0000, INS_OPTS_4S);   // LSL 16
    theEmitter->emitIns_R_I(INS_bic, EA_16BYTE, REG_V13, 0x5D000000, INS_OPTS_4S); // LSL 24

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_F   cmp/fmov immediate
    //

    // fmov  imm8  (scalar)
    theEmitter->emitIns_R_F(INS_fmov, EA_8BYTE, REG_V14, 1.0);
    theEmitter->emitIns_R_F(INS_fmov, EA_4BYTE, REG_V15, -1.0);
    theEmitter->emitIns_R_F(INS_fmov, EA_4BYTE, REG_V0, 2.0); // encodes imm8 == 0
    theEmitter->emitIns_R_F(INS_fmov, EA_4BYTE, REG_V16, 10.0);
    theEmitter->emitIns_R_F(INS_fmov, EA_8BYTE, REG_V17, -10.0);
    theEmitter->emitIns_R_F(INS_fmov, EA_8BYTE, REG_V18, 31); // Largest encodable value
    theEmitter->emitIns_R_F(INS_fmov, EA_4BYTE, REG_V19, -31);
    theEmitter->emitIns_R_F(INS_fmov, EA_4BYTE, REG_V20, 1.25);
    theEmitter->emitIns_R_F(INS_fmov, EA_8BYTE, REG_V21, -1.25);
    theEmitter->emitIns_R_F(INS_fmov, EA_8BYTE, REG_V22, 0.125); // Smallest encodable value
    theEmitter->emitIns_R_F(INS_fmov, EA_4BYTE, REG_V23, -0.125);

    // fmov  imm8  (vector)
    theEmitter->emitIns_R_F(INS_fmov, EA_8BYTE, REG_V0, 2.0, INS_OPTS_2S);
    theEmitter->emitIns_R_F(INS_fmov, EA_8BYTE, REG_V24, 1.0, INS_OPTS_2S);
    theEmitter->emitIns_R_F(INS_fmov, EA_16BYTE, REG_V25, 1.0, INS_OPTS_4S);
    theEmitter->emitIns_R_F(INS_fmov, EA_16BYTE, REG_V26, 1.0, INS_OPTS_2D);
    theEmitter->emitIns_R_F(INS_fmov, EA_8BYTE, REG_V27, -10.0, INS_OPTS_2S);
    theEmitter->emitIns_R_F(INS_fmov, EA_16BYTE, REG_V28, -10.0, INS_OPTS_4S);
    theEmitter->emitIns_R_F(INS_fmov, EA_16BYTE, REG_V29, -10.0, INS_OPTS_2D);
    theEmitter->emitIns_R_F(INS_fmov, EA_8BYTE, REG_V30, 31.0, INS_OPTS_2S);
    theEmitter->emitIns_R_F(INS_fmov, EA_16BYTE, REG_V31, 31.0, INS_OPTS_4S);
    theEmitter->emitIns_R_F(INS_fmov, EA_16BYTE, REG_V0, 31.0, INS_OPTS_2D);
    theEmitter->emitIns_R_F(INS_fmov, EA_8BYTE, REG_V1, -0.125, INS_OPTS_2S);
    theEmitter->emitIns_R_F(INS_fmov, EA_16BYTE, REG_V2, -0.125, INS_OPTS_4S);
    theEmitter->emitIns_R_F(INS_fmov, EA_16BYTE, REG_V3, -0.125, INS_OPTS_2D);

    // fcmp with 0.0
    theEmitter->emitIns_R_F(INS_fcmp, EA_8BYTE, REG_V12, 0.0);
    theEmitter->emitIns_R_F(INS_fcmp, EA_4BYTE, REG_V13, 0.0);
    theEmitter->emitIns_R_F(INS_fcmpe, EA_8BYTE, REG_V14, 0.0);
    theEmitter->emitIns_R_F(INS_fcmpe, EA_4BYTE, REG_V15, 0.0);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R   fmov/fcmp/fcvt
    //

    // fmov to vector to vector
    theEmitter->emitIns_Mov(INS_fmov, EA_8BYTE, REG_V0, REG_V2, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_fmov, EA_4BYTE, REG_V1, REG_V3, /* canSkip */ false);

    // fmov to vector to general
    theEmitter->emitIns_Mov(INS_fmov, EA_8BYTE, REG_R0, REG_V4, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_fmov, EA_4BYTE, REG_R1, REG_V5, /* canSkip */ false);
    //    using the optional conversion specifier
    theEmitter->emitIns_Mov(INS_fmov, EA_8BYTE, REG_R2, REG_V6, /* canSkip */ false, INS_OPTS_D_TO_8BYTE);
    theEmitter->emitIns_Mov(INS_fmov, EA_4BYTE, REG_R3, REG_V7, /* canSkip */ false, INS_OPTS_S_TO_4BYTE);

    // fmov to general to vector
    theEmitter->emitIns_Mov(INS_fmov, EA_8BYTE, REG_V8, REG_R4, /* canSkip */ false);
    theEmitter->emitIns_Mov(INS_fmov, EA_4BYTE, REG_V9, REG_R5, /* canSkip */ false);
    //   using the optional conversion specifier
    theEmitter->emitIns_Mov(INS_fmov, EA_4BYTE, REG_V11, REG_R7, /* canSkip */ false, INS_OPTS_4BYTE_TO_S);
    theEmitter->emitIns_Mov(INS_fmov, EA_8BYTE, REG_V10, REG_R6, /* canSkip */ false, INS_OPTS_8BYTE_TO_D);

    // fcmp/fcmpe
    theEmitter->emitIns_R_R(INS_fcmp, EA_8BYTE, REG_V8, REG_V16);
    theEmitter->emitIns_R_R(INS_fcmp, EA_4BYTE, REG_V9, REG_V17);
    theEmitter->emitIns_R_R(INS_fcmpe, EA_8BYTE, REG_V10, REG_V18);
    theEmitter->emitIns_R_R(INS_fcmpe, EA_4BYTE, REG_V11, REG_V19);

    // fcvt
    theEmitter->emitIns_R_R(INS_fcvt, EA_8BYTE, REG_V24, REG_V25, INS_OPTS_S_TO_D); // Single to Double
    theEmitter->emitIns_R_R(INS_fcvt, EA_4BYTE, REG_V26, REG_V27, INS_OPTS_D_TO_S); // Double to Single

    theEmitter->emitIns_R_R(INS_fcvt, EA_4BYTE, REG_V1, REG_V2, INS_OPTS_H_TO_S);
    theEmitter->emitIns_R_R(INS_fcvt, EA_8BYTE, REG_V3, REG_V4, INS_OPTS_H_TO_D);

    theEmitter->emitIns_R_R(INS_fcvt, EA_2BYTE, REG_V5, REG_V6, INS_OPTS_S_TO_H);
    theEmitter->emitIns_R_R(INS_fcvt, EA_2BYTE, REG_V7, REG_V8, INS_OPTS_D_TO_H);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R   floating point conversions
    //

    // fcvtas scalar
    theEmitter->emitIns_R_R(INS_fcvtas, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_fcvtas, EA_8BYTE, REG_V2, REG_V3);

    // fcvtas scalar to general
    theEmitter->emitIns_R_R(INS_fcvtas, EA_4BYTE, REG_R0, REG_V4, INS_OPTS_S_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtas, EA_4BYTE, REG_R1, REG_V5, INS_OPTS_D_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtas, EA_8BYTE, REG_R2, REG_V6, INS_OPTS_S_TO_8BYTE);
    theEmitter->emitIns_R_R(INS_fcvtas, EA_8BYTE, REG_R3, REG_V7, INS_OPTS_D_TO_8BYTE);

    // fcvtas vector
    theEmitter->emitIns_R_R(INS_fcvtas, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fcvtas, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_fcvtas, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

    // fcvtau scalar
    theEmitter->emitIns_R_R(INS_fcvtau, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_fcvtau, EA_8BYTE, REG_V2, REG_V3);

    // fcvtau scalar to general
    theEmitter->emitIns_R_R(INS_fcvtau, EA_4BYTE, REG_R0, REG_V4, INS_OPTS_S_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtau, EA_4BYTE, REG_R1, REG_V5, INS_OPTS_D_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtau, EA_8BYTE, REG_R2, REG_V6, INS_OPTS_S_TO_8BYTE);
    theEmitter->emitIns_R_R(INS_fcvtau, EA_8BYTE, REG_R3, REG_V7, INS_OPTS_D_TO_8BYTE);

    // fcvtau vector
    theEmitter->emitIns_R_R(INS_fcvtau, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fcvtau, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_fcvtau, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

    ////////////////////////////////////////////////////////////////////////////////

    // fcvtms scalar
    theEmitter->emitIns_R_R(INS_fcvtms, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_fcvtms, EA_8BYTE, REG_V2, REG_V3);

    // fcvtms scalar to general
    theEmitter->emitIns_R_R(INS_fcvtms, EA_4BYTE, REG_R0, REG_V4, INS_OPTS_S_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtms, EA_4BYTE, REG_R1, REG_V5, INS_OPTS_D_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtms, EA_8BYTE, REG_R2, REG_V6, INS_OPTS_S_TO_8BYTE);
    theEmitter->emitIns_R_R(INS_fcvtms, EA_8BYTE, REG_R3, REG_V7, INS_OPTS_D_TO_8BYTE);

    // fcvtms vector
    theEmitter->emitIns_R_R(INS_fcvtms, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fcvtms, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_fcvtms, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

    // fcvtmu scalar
    theEmitter->emitIns_R_R(INS_fcvtmu, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_fcvtmu, EA_8BYTE, REG_V2, REG_V3);

    // fcvtmu scalar to general
    theEmitter->emitIns_R_R(INS_fcvtmu, EA_4BYTE, REG_R0, REG_V4, INS_OPTS_S_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtmu, EA_4BYTE, REG_R1, REG_V5, INS_OPTS_D_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtmu, EA_8BYTE, REG_R2, REG_V6, INS_OPTS_S_TO_8BYTE);
    theEmitter->emitIns_R_R(INS_fcvtmu, EA_8BYTE, REG_R3, REG_V7, INS_OPTS_D_TO_8BYTE);

    // fcvtmu vector
    theEmitter->emitIns_R_R(INS_fcvtmu, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fcvtmu, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_fcvtmu, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

    ////////////////////////////////////////////////////////////////////////////////

    // fcvtns scalar
    theEmitter->emitIns_R_R(INS_fcvtns, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_fcvtns, EA_8BYTE, REG_V2, REG_V3);

    // fcvtns scalar to general
    theEmitter->emitIns_R_R(INS_fcvtns, EA_4BYTE, REG_R0, REG_V4, INS_OPTS_S_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtns, EA_4BYTE, REG_R1, REG_V5, INS_OPTS_D_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtns, EA_8BYTE, REG_R2, REG_V6, INS_OPTS_S_TO_8BYTE);
    theEmitter->emitIns_R_R(INS_fcvtns, EA_8BYTE, REG_R3, REG_V7, INS_OPTS_D_TO_8BYTE);

    // fcvtns vector
    theEmitter->emitIns_R_R(INS_fcvtns, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fcvtns, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_fcvtns, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

    // fcvtnu scalar
    theEmitter->emitIns_R_R(INS_fcvtnu, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_fcvtnu, EA_8BYTE, REG_V2, REG_V3);

    // fcvtnu scalar to general
    theEmitter->emitIns_R_R(INS_fcvtnu, EA_4BYTE, REG_R0, REG_V4, INS_OPTS_S_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtnu, EA_4BYTE, REG_R1, REG_V5, INS_OPTS_D_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtnu, EA_8BYTE, REG_R2, REG_V6, INS_OPTS_S_TO_8BYTE);
    theEmitter->emitIns_R_R(INS_fcvtnu, EA_8BYTE, REG_R3, REG_V7, INS_OPTS_D_TO_8BYTE);

    // fcvtnu vector
    theEmitter->emitIns_R_R(INS_fcvtnu, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fcvtnu, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_fcvtnu, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

    ////////////////////////////////////////////////////////////////////////////////

    // fcvtps scalar
    theEmitter->emitIns_R_R(INS_fcvtps, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_fcvtps, EA_8BYTE, REG_V2, REG_V3);

    // fcvtps scalar to general
    theEmitter->emitIns_R_R(INS_fcvtps, EA_4BYTE, REG_R0, REG_V4, INS_OPTS_S_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtps, EA_4BYTE, REG_R1, REG_V5, INS_OPTS_D_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtps, EA_8BYTE, REG_R2, REG_V6, INS_OPTS_S_TO_8BYTE);
    theEmitter->emitIns_R_R(INS_fcvtps, EA_8BYTE, REG_R3, REG_V7, INS_OPTS_D_TO_8BYTE);

    // fcvtps vector
    theEmitter->emitIns_R_R(INS_fcvtps, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fcvtps, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_fcvtps, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

    // fcvtpu scalar
    theEmitter->emitIns_R_R(INS_fcvtpu, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_fcvtpu, EA_8BYTE, REG_V2, REG_V3);

    // fcvtpu scalar to general
    theEmitter->emitIns_R_R(INS_fcvtpu, EA_4BYTE, REG_R0, REG_V4, INS_OPTS_S_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtpu, EA_4BYTE, REG_R1, REG_V5, INS_OPTS_D_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtpu, EA_8BYTE, REG_R2, REG_V6, INS_OPTS_S_TO_8BYTE);
    theEmitter->emitIns_R_R(INS_fcvtpu, EA_8BYTE, REG_R3, REG_V7, INS_OPTS_D_TO_8BYTE);

    // fcvtpu vector
    theEmitter->emitIns_R_R(INS_fcvtpu, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fcvtpu, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_fcvtpu, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

    ////////////////////////////////////////////////////////////////////////////////

    // fcvtzs scalar
    theEmitter->emitIns_R_R(INS_fcvtzs, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_fcvtzs, EA_8BYTE, REG_V2, REG_V3);

    // fcvtzs scalar to general
    theEmitter->emitIns_R_R(INS_fcvtzs, EA_4BYTE, REG_R0, REG_V4, INS_OPTS_S_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtzs, EA_4BYTE, REG_R1, REG_V5, INS_OPTS_D_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtzs, EA_8BYTE, REG_R2, REG_V6, INS_OPTS_S_TO_8BYTE);
    theEmitter->emitIns_R_R(INS_fcvtzs, EA_8BYTE, REG_R3, REG_V7, INS_OPTS_D_TO_8BYTE);

    // fcvtzs vector
    theEmitter->emitIns_R_R(INS_fcvtzs, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fcvtzs, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_fcvtzs, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

    // fcvtzu scalar
    theEmitter->emitIns_R_R(INS_fcvtzu, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_fcvtzu, EA_8BYTE, REG_V2, REG_V3);

    // fcvtzu scalar to general
    theEmitter->emitIns_R_R(INS_fcvtzu, EA_4BYTE, REG_R0, REG_V4, INS_OPTS_S_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtzu, EA_4BYTE, REG_R1, REG_V5, INS_OPTS_D_TO_4BYTE);
    theEmitter->emitIns_R_R(INS_fcvtzu, EA_8BYTE, REG_R2, REG_V6, INS_OPTS_S_TO_8BYTE);
    theEmitter->emitIns_R_R(INS_fcvtzu, EA_8BYTE, REG_R3, REG_V7, INS_OPTS_D_TO_8BYTE);

    // fcvtzu vector
    theEmitter->emitIns_R_R(INS_fcvtzu, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fcvtzu, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_fcvtzu, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

    ////////////////////////////////////////////////////////////////////////////////

    // scvtf scalar
    theEmitter->emitIns_R_R(INS_scvtf, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_scvtf, EA_8BYTE, REG_V2, REG_V3);

    // scvtf scalar from general
    theEmitter->emitIns_R_R(INS_scvtf, EA_4BYTE, REG_V4, REG_R0, INS_OPTS_4BYTE_TO_S);
    theEmitter->emitIns_R_R(INS_scvtf, EA_4BYTE, REG_V5, REG_R1, INS_OPTS_8BYTE_TO_S);
    theEmitter->emitIns_R_R(INS_scvtf, EA_8BYTE, REG_V6, REG_R2, INS_OPTS_4BYTE_TO_D);
    theEmitter->emitIns_R_R(INS_scvtf, EA_8BYTE, REG_V7, REG_R3, INS_OPTS_8BYTE_TO_D);

    // scvtf vector
    theEmitter->emitIns_R_R(INS_scvtf, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_scvtf, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_scvtf, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

    // ucvtf scalar
    theEmitter->emitIns_R_R(INS_ucvtf, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_ucvtf, EA_8BYTE, REG_V2, REG_V3);

    // ucvtf scalar from general
    theEmitter->emitIns_R_R(INS_ucvtf, EA_4BYTE, REG_V4, REG_R0, INS_OPTS_4BYTE_TO_S);
    theEmitter->emitIns_R_R(INS_ucvtf, EA_4BYTE, REG_V5, REG_R1, INS_OPTS_8BYTE_TO_S);
    theEmitter->emitIns_R_R(INS_ucvtf, EA_8BYTE, REG_V6, REG_R2, INS_OPTS_4BYTE_TO_D);
    theEmitter->emitIns_R_R(INS_ucvtf, EA_8BYTE, REG_V7, REG_R3, INS_OPTS_8BYTE_TO_D);

    // ucvtf vector
    theEmitter->emitIns_R_R(INS_ucvtf, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_ucvtf, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_ucvtf, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R   floating point operations, one dest, one source
    //

    // fabs scalar
    theEmitter->emitIns_R_R(INS_fabs, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_fabs, EA_8BYTE, REG_V2, REG_V3);

    // fabs vector
    theEmitter->emitIns_R_R(INS_fabs, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fabs, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_fabs, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_2D);

    // fmaxp scalar
    theEmitter->emitIns_R_R(INS_fmaxp, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fmaxp, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_2D);

    // fmaxnmp scalar
    theEmitter->emitIns_R_R(INS_fmaxnmp, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fmaxnmp, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_2D);

    // fmaxnmv vector
    theEmitter->emitIns_R_R(INS_fmaxnmv, EA_16BYTE, REG_V0, REG_V1, INS_OPTS_4S);

    // fmaxv vector
    theEmitter->emitIns_R_R(INS_fmaxv, EA_16BYTE, REG_V0, REG_V1, INS_OPTS_4S);

    // fminp scalar
    theEmitter->emitIns_R_R(INS_fminp, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fminp, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_2D);

    // fminnmp scalar
    theEmitter->emitIns_R_R(INS_fminnmp, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fminnmp, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_2D);

    // fminnmv vector
    theEmitter->emitIns_R_R(INS_fminnmv, EA_16BYTE, REG_V0, REG_V1, INS_OPTS_4S);

    // fminv vector
    theEmitter->emitIns_R_R(INS_fminv, EA_16BYTE, REG_V0, REG_V1, INS_OPTS_4S);

    // fneg scalar
    theEmitter->emitIns_R_R(INS_fneg, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_fneg, EA_8BYTE, REG_V2, REG_V3);

    // fneg vector
    theEmitter->emitIns_R_R(INS_fneg, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fneg, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_fneg, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_2D);

    // fsqrt scalar
    theEmitter->emitIns_R_R(INS_fsqrt, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_fsqrt, EA_8BYTE, REG_V2, REG_V3);

    // fsqrt vector
    theEmitter->emitIns_R_R(INS_fsqrt, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fsqrt, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_fsqrt, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_2D);

    // faddp scalar
    theEmitter->emitIns_R_R(INS_faddp, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_faddp, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_2D);

    // fcmeq Vd, Vn, #0.0
    theEmitter->emitIns_R_R(INS_fcmeq, EA_4BYTE, REG_V0, REG_V1); // scalar 4BYTE
    theEmitter->emitIns_R_R(INS_fcmeq, EA_8BYTE, REG_V2, REG_V3); // scalar 8BYTE

    // fcmge Vd, Vn, #0.0
    theEmitter->emitIns_R_R(INS_fcmge, EA_4BYTE, REG_V0, REG_V1); // scalar 4BYTE
    theEmitter->emitIns_R_R(INS_fcmge, EA_8BYTE, REG_V2, REG_V3); // scalar 8BYTE

    // fcmgt Vd, Vn, #0.0
    theEmitter->emitIns_R_R(INS_fcmgt, EA_4BYTE, REG_V0, REG_V1); // scalar 4BYTE
    theEmitter->emitIns_R_R(INS_fcmgt, EA_8BYTE, REG_V2, REG_V3); // scalar 8BYTE

    // fcmle Vd, Vn, #0.0
    theEmitter->emitIns_R_R(INS_fcmle, EA_4BYTE, REG_V0, REG_V1); // scalar 4BYTE
    theEmitter->emitIns_R_R(INS_fcmle, EA_8BYTE, REG_V2, REG_V3); // scalar 8BYTE

    // fcmlt Vd, Vn, #0.0
    theEmitter->emitIns_R_R(INS_fcmlt, EA_4BYTE, REG_V0, REG_V1); // scalar 4BYTE
    theEmitter->emitIns_R_R(INS_fcmlt, EA_8BYTE, REG_V2, REG_V3); // scalar 8BYTE

    // frecpe scalar
    theEmitter->emitIns_R_R(INS_frecpe, EA_4BYTE, REG_V0, REG_V1); // scalar 4BYTE
    theEmitter->emitIns_R_R(INS_frecpe, EA_8BYTE, REG_V2, REG_V3); // scalar 8BYTE
    theEmitter->emitIns_R_R(INS_frecpe, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_frecpe, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_frecpe, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_2D);

    // frecpx scalar
    theEmitter->emitIns_R_R(INS_frecpx, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_frecpx, EA_8BYTE, REG_V2, REG_V3);

    // frsqrte
    theEmitter->emitIns_R_R(INS_frsqrte, EA_4BYTE, REG_V0, REG_V1); // scalar 4BYTE
    theEmitter->emitIns_R_R(INS_frsqrte, EA_8BYTE, REG_V2, REG_V3); // scalar 8BYTE
    theEmitter->emitIns_R_R(INS_frsqrte, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_frsqrte, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_frsqrte, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_2D);

    // fcvtl{2} vector
    theEmitter->emitIns_R_R(INS_fcvtl, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_fcvtl2, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_fcvtl, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fcvtl2, EA_16BYTE, REG_V5, REG_V6, INS_OPTS_4S);

    // fcvtn{2} vector
    theEmitter->emitIns_R_R(INS_fcvtn, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_fcvtn2, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_fcvtn, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fcvtn2, EA_16BYTE, REG_V5, REG_V6, INS_OPTS_4S);

    // fcvtxn scalar
    theEmitter->emitIns_R_R(INS_fcvtxn, EA_4BYTE, REG_V0, REG_V1);

    // fcvtxn{2} vector
    theEmitter->emitIns_R_R(INS_fcvtxn, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_fcvtxn2, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_4S);

#endif

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    genDefineTempLabel(genCreateTempLabel());

    // abs scalar
    theEmitter->emitIns_R_R(INS_abs, EA_8BYTE, REG_V2, REG_V3);

    // abs vector
    theEmitter->emitIns_R_R(INS_abs, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_abs, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_abs, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_abs, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_abs, EA_8BYTE, REG_V12, REG_V13, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_abs, EA_16BYTE, REG_V14, REG_V15, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_abs, EA_16BYTE, REG_V16, REG_V17, INS_OPTS_2D);

    // addv vector
    theEmitter->emitIns_R_R(INS_addv, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_addv, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_addv, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_addv, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_addv, EA_16BYTE, REG_V14, REG_V15, INS_OPTS_4S);

    // cnt vector
    theEmitter->emitIns_R_R(INS_cnt, EA_8BYTE, REG_V22, REG_V23, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_cnt, EA_16BYTE, REG_V24, REG_V25, INS_OPTS_16B);

    // cls vector
    theEmitter->emitIns_R_R(INS_cls, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_cls, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_cls, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_cls, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_cls, EA_8BYTE, REG_V12, REG_V13, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_cls, EA_16BYTE, REG_V14, REG_V15, INS_OPTS_4S);

    // clz vector
    theEmitter->emitIns_R_R(INS_clz, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_clz, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_clz, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_clz, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_clz, EA_8BYTE, REG_V12, REG_V13, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_clz, EA_16BYTE, REG_V14, REG_V15, INS_OPTS_4S);

    // mvn vector
    theEmitter->emitIns_R_R(INS_mvn, EA_8BYTE, REG_V4, REG_V5);
    theEmitter->emitIns_R_R(INS_mvn, EA_8BYTE, REG_V6, REG_V7, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_mvn, EA_16BYTE, REG_V8, REG_V9);
    theEmitter->emitIns_R_R(INS_mvn, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_16B);

    // neg scalar
    theEmitter->emitIns_R_R(INS_neg, EA_8BYTE, REG_V2, REG_V3);

    // neg vector
    theEmitter->emitIns_R_R(INS_neg, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_neg, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_neg, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_neg, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_neg, EA_8BYTE, REG_V12, REG_V13, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_neg, EA_16BYTE, REG_V14, REG_V15, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_neg, EA_16BYTE, REG_V16, REG_V17, INS_OPTS_2D);

    // not vector (the same encoding as mvn)
    theEmitter->emitIns_R_R(INS_not, EA_8BYTE, REG_V12, REG_V13);
    theEmitter->emitIns_R_R(INS_not, EA_8BYTE, REG_V14, REG_V15, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_not, EA_16BYTE, REG_V16, REG_V17);
    theEmitter->emitIns_R_R(INS_not, EA_16BYTE, REG_V18, REG_V19, INS_OPTS_16B);

    // rbit vector
    theEmitter->emitIns_R_R(INS_rbit, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_rbit, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_16B);

    // rev16 vector
    theEmitter->emitIns_R_R(INS_rev16, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_rev16, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_16B);

    // rev32 vector
    theEmitter->emitIns_R_R(INS_rev32, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_rev32, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_rev32, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_rev32, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_8H);

    // rev64 vector
    theEmitter->emitIns_R_R(INS_rev64, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_rev64, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_rev64, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_rev64, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_rev64, EA_8BYTE, REG_V12, REG_V13, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_rev64, EA_16BYTE, REG_V14, REG_V15, INS_OPTS_4S);

    // sadalp vector
    theEmitter->emitIns_R_R(INS_sadalp, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_sadalp, EA_8BYTE, REG_V2, REG_V3, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_sadalp, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_sadalp, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_sadalp, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_sadalp, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);

    // saddlp vector
    theEmitter->emitIns_R_R(INS_saddlp, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_saddlp, EA_8BYTE, REG_V2, REG_V3, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_saddlp, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_saddlp, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_saddlp, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_saddlp, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);

    // saddlv vector
    theEmitter->emitIns_R_R(INS_saddlv, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_saddlv, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_saddlv, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_saddlv, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_saddlv, EA_16BYTE, REG_V14, REG_V15, INS_OPTS_4S);

    // smaxv vector
    theEmitter->emitIns_R_R(INS_smaxv, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_smaxv, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_smaxv, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_smaxv, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_smaxv, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_4S);

    // sminv vector
    theEmitter->emitIns_R_R(INS_sminv, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_sminv, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_sminv, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_sminv, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_sminv, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_4S);

    // sqabs scalar
    theEmitter->emitIns_R_R(INS_sqabs, EA_1BYTE, REG_V0, REG_V1, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_sqabs, EA_2BYTE, REG_V2, REG_V3, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_sqabs, EA_4BYTE, REG_V4, REG_V5, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_sqabs, EA_8BYTE, REG_V6, REG_V7, INS_OPTS_NONE);

    // sqabs vector
    theEmitter->emitIns_R_R(INS_sqabs, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_sqabs, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_sqabs, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_sqabs, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_sqabs, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_sqabs, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_sqabs, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

    // sqneg scalar
    theEmitter->emitIns_R_R(INS_sqneg, EA_1BYTE, REG_V0, REG_V1, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_sqneg, EA_2BYTE, REG_V2, REG_V3, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_sqneg, EA_4BYTE, REG_V4, REG_V5, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_sqneg, EA_8BYTE, REG_V6, REG_V7, INS_OPTS_NONE);

    // sqneg vector
    theEmitter->emitIns_R_R(INS_sqneg, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_sqneg, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_sqneg, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_sqneg, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_sqneg, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_sqneg, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_sqneg, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

    // sqxtn scalar
    theEmitter->emitIns_R_R(INS_sqxtn, EA_1BYTE, REG_V0, REG_V1, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_sqxtn, EA_2BYTE, REG_V2, REG_V3, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_sqxtn, EA_4BYTE, REG_V4, REG_V5, INS_OPTS_NONE);

    // sqxtn vector
    theEmitter->emitIns_R_R(INS_sqxtn, EA_8BYTE, REG_V0, REG_V6, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_sqxtn, EA_8BYTE, REG_V1, REG_V7, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_sqxtn, EA_8BYTE, REG_V2, REG_V8, INS_OPTS_2S);

    // sqxtn2 vector
    theEmitter->emitIns_R_R(INS_sqxtn2, EA_16BYTE, REG_V3, REG_V9, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_sqxtn2, EA_16BYTE, REG_V4, REG_V10, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_sqxtn2, EA_16BYTE, REG_V5, REG_V11, INS_OPTS_4S);

    // sqxtun scalar
    theEmitter->emitIns_R_R(INS_sqxtun, EA_1BYTE, REG_V0, REG_V1, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_sqxtun, EA_2BYTE, REG_V2, REG_V3, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_sqxtun, EA_4BYTE, REG_V4, REG_V5, INS_OPTS_NONE);

    // sqxtun vector
    theEmitter->emitIns_R_R(INS_sqxtun, EA_8BYTE, REG_V0, REG_V6, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_sqxtun, EA_8BYTE, REG_V1, REG_V7, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_sqxtun, EA_8BYTE, REG_V2, REG_V8, INS_OPTS_2S);

    // sqxtun2 vector
    theEmitter->emitIns_R_R(INS_sqxtun2, EA_16BYTE, REG_V3, REG_V9, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_sqxtun2, EA_16BYTE, REG_V4, REG_V10, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_sqxtun2, EA_16BYTE, REG_V5, REG_V11, INS_OPTS_4S);

    // suqadd scalar
    theEmitter->emitIns_R_R(INS_suqadd, EA_1BYTE, REG_V0, REG_V1, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_suqadd, EA_2BYTE, REG_V2, REG_V3, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_suqadd, EA_4BYTE, REG_V4, REG_V5, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_suqadd, EA_8BYTE, REG_V6, REG_V7, INS_OPTS_NONE);

    // suqadd vector
    theEmitter->emitIns_R_R(INS_suqadd, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_suqadd, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_suqadd, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_suqadd, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_suqadd, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_suqadd, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_suqadd, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

    // uadalp vector
    theEmitter->emitIns_R_R(INS_uadalp, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_uadalp, EA_8BYTE, REG_V2, REG_V3, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_uadalp, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_uadalp, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_uadalp, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_uadalp, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);

    // uaddlp vector
    theEmitter->emitIns_R_R(INS_uaddlp, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_uaddlp, EA_8BYTE, REG_V2, REG_V3, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_uaddlp, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_uaddlp, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_uaddlp, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_uaddlp, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);

    // uaddlv vector
    theEmitter->emitIns_R_R(INS_uaddlv, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_uaddlv, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_uaddlv, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_uaddlv, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_uaddlv, EA_16BYTE, REG_V14, REG_V15, INS_OPTS_4S);

    // umaxv vector
    theEmitter->emitIns_R_R(INS_umaxv, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_umaxv, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_umaxv, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_umaxv, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_umaxv, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_4S);

    // uminv vector
    theEmitter->emitIns_R_R(INS_uminv, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_uminv, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_uminv, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_uminv, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_uminv, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_4S);

    // uqxtn scalar
    theEmitter->emitIns_R_R(INS_uqxtn, EA_1BYTE, REG_V0, REG_V1, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_uqxtn, EA_2BYTE, REG_V2, REG_V3, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_uqxtn, EA_4BYTE, REG_V4, REG_V5, INS_OPTS_NONE);

    // uqxtn vector
    theEmitter->emitIns_R_R(INS_uqxtn, EA_8BYTE, REG_V0, REG_V6, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_uqxtn, EA_8BYTE, REG_V1, REG_V7, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_uqxtn, EA_8BYTE, REG_V2, REG_V8, INS_OPTS_2S);

    // uqxtn2 vector
    theEmitter->emitIns_R_R(INS_uqxtn2, EA_16BYTE, REG_V3, REG_V9, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_uqxtn2, EA_16BYTE, REG_V4, REG_V10, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_uqxtn2, EA_16BYTE, REG_V5, REG_V11, INS_OPTS_4S);

    // urecpe vector
    theEmitter->emitIns_R_R(INS_urecpe, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_urecpe, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_4S);

    // ursqrte vector
    theEmitter->emitIns_R_R(INS_ursqrte, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_ursqrte, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_4S);

    // usqadd scalar
    theEmitter->emitIns_R_R(INS_usqadd, EA_1BYTE, REG_V0, REG_V1, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_usqadd, EA_2BYTE, REG_V2, REG_V3, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_usqadd, EA_4BYTE, REG_V4, REG_V5, INS_OPTS_NONE);
    theEmitter->emitIns_R_R(INS_usqadd, EA_8BYTE, REG_V6, REG_V7, INS_OPTS_NONE);

    // usqadd vector
    theEmitter->emitIns_R_R(INS_usqadd, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_usqadd, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_usqadd, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_usqadd, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_usqadd, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_usqadd, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_usqadd, EA_16BYTE, REG_V12, REG_V13, INS_OPTS_2D);

    // xtn vector
    theEmitter->emitIns_R_R(INS_xtn, EA_8BYTE, REG_V0, REG_V6, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_xtn, EA_8BYTE, REG_V1, REG_V7, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_xtn, EA_8BYTE, REG_V2, REG_V8, INS_OPTS_2S);

    // xtn2 vector
    theEmitter->emitIns_R_R(INS_xtn2, EA_16BYTE, REG_V3, REG_V9, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_xtn2, EA_16BYTE, REG_V4, REG_V10, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_xtn2, EA_16BYTE, REG_V5, REG_V11, INS_OPTS_4S);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R   floating point round to int, one dest, one source
    //

    // frinta scalar
    theEmitter->emitIns_R_R(INS_frinta, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_frinta, EA_8BYTE, REG_V2, REG_V3);

    // frinta vector
    theEmitter->emitIns_R_R(INS_frinta, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_frinta, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_frinta, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_2D);

    // frinti scalar
    theEmitter->emitIns_R_R(INS_frinti, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_frinti, EA_8BYTE, REG_V2, REG_V3);

    // frinti vector
    theEmitter->emitIns_R_R(INS_frinti, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_frinti, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_frinti, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_2D);

    // frintm scalar
    theEmitter->emitIns_R_R(INS_frintm, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_frintm, EA_8BYTE, REG_V2, REG_V3);

    // frintm vector
    theEmitter->emitIns_R_R(INS_frintm, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_frintm, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_frintm, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_2D);

    // frintn scalar
    theEmitter->emitIns_R_R(INS_frintn, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_frintn, EA_8BYTE, REG_V2, REG_V3);

    // frintn vector
    theEmitter->emitIns_R_R(INS_frintn, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_frintn, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_frintn, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_2D);

    // frintp scalar
    theEmitter->emitIns_R_R(INS_frintp, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_frintp, EA_8BYTE, REG_V2, REG_V3);

    // frintp vector
    theEmitter->emitIns_R_R(INS_frintp, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_frintp, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_frintp, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_2D);

    // frintx scalar
    theEmitter->emitIns_R_R(INS_frintx, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_frintx, EA_8BYTE, REG_V2, REG_V3);

    // frintx vector
    theEmitter->emitIns_R_R(INS_frintx, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_frintx, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_frintx, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_2D);

    // frintz scalar
    theEmitter->emitIns_R_R(INS_frintz, EA_4BYTE, REG_V0, REG_V1);
    theEmitter->emitIns_R_R(INS_frintz, EA_8BYTE, REG_V2, REG_V3);

    // frintz vector
    theEmitter->emitIns_R_R(INS_frintz, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_frintz, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_4S);
    theEmitter->emitIns_R_R(INS_frintz, EA_16BYTE, REG_V8, REG_V9, INS_OPTS_2D);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R_R   floating point operations, one dest, two source
    //

    genDefineTempLabel(genCreateTempLabel());

    // fadd
    theEmitter->emitIns_R_R_R(INS_fadd, EA_4BYTE, REG_V0, REG_V1, REG_V2); // scalar 4BYTE
    theEmitter->emitIns_R_R_R(INS_fadd, EA_8BYTE, REG_V3, REG_V4, REG_V5); // scalar 8BYTE
    theEmitter->emitIns_R_R_R(INS_fadd, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fadd, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fadd, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2D);

    // fsub
    theEmitter->emitIns_R_R_R(INS_fsub, EA_4BYTE, REG_V0, REG_V1, REG_V2); // scalar 4BYTE
    theEmitter->emitIns_R_R_R(INS_fsub, EA_8BYTE, REG_V3, REG_V4, REG_V5); // scalar 8BYTE
    theEmitter->emitIns_R_R_R(INS_fsub, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fsub, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fsub, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2D);

    // fdiv
    theEmitter->emitIns_R_R_R(INS_fdiv, EA_4BYTE, REG_V0, REG_V1, REG_V2); // scalar 4BYTE
    theEmitter->emitIns_R_R_R(INS_fdiv, EA_8BYTE, REG_V3, REG_V4, REG_V5); // scalar 8BYTE
    theEmitter->emitIns_R_R_R(INS_fdiv, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fdiv, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fdiv, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2D);

    // fmax
    theEmitter->emitIns_R_R_R(INS_fmax, EA_4BYTE, REG_V0, REG_V1, REG_V2); // scalar 4BYTE
    theEmitter->emitIns_R_R_R(INS_fmax, EA_8BYTE, REG_V3, REG_V4, REG_V5); // scalar 8BYTE
    theEmitter->emitIns_R_R_R(INS_fmax, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fmax, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fmax, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2D);

    // fmaxp
    theEmitter->emitIns_R_R_R(INS_fmaxp, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fmaxp, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fmaxp, EA_16BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2D);

    // fmaxnm
    theEmitter->emitIns_R_R_R(INS_fmaxnm, EA_4BYTE, REG_V0, REG_V1, REG_V2); // scalar 4BYTE
    theEmitter->emitIns_R_R_R(INS_fmaxnm, EA_8BYTE, REG_V3, REG_V4, REG_V5); // scalar 8BYTE
    theEmitter->emitIns_R_R_R(INS_fmaxnm, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fmaxnm, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fmaxnm, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2D);

    // fmaxnmp vector
    theEmitter->emitIns_R_R_R(INS_fmaxnmp, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fmaxnmp, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fmaxnmp, EA_16BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2D);

    // fmin
    theEmitter->emitIns_R_R_R(INS_fmin, EA_4BYTE, REG_V0, REG_V1, REG_V2); // scalar 4BYTE
    theEmitter->emitIns_R_R_R(INS_fmin, EA_8BYTE, REG_V3, REG_V4, REG_V5); // scalar 8BYTE
    theEmitter->emitIns_R_R_R(INS_fmin, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fmin, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fmin, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2D);

    // fminp
    theEmitter->emitIns_R_R_R(INS_fminp, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fminp, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fminp, EA_16BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2D);

    // fminnm
    theEmitter->emitIns_R_R_R(INS_fminnm, EA_4BYTE, REG_V0, REG_V1, REG_V2); // scalar 4BYTE
    theEmitter->emitIns_R_R_R(INS_fminnm, EA_8BYTE, REG_V3, REG_V4, REG_V5); // scalar 8BYTE
    theEmitter->emitIns_R_R_R(INS_fminnm, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fminnm, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fminnm, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2D);

    // fminnmp vector
    theEmitter->emitIns_R_R_R(INS_fminnmp, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fminnmp, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fminnmp, EA_16BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2D);

    // fabd
    theEmitter->emitIns_R_R_R(INS_fabd, EA_4BYTE, REG_V0, REG_V1, REG_V2); // scalar 4BYTE
    theEmitter->emitIns_R_R_R(INS_fabd, EA_8BYTE, REG_V3, REG_V4, REG_V5); // scalar 8BYTE
    theEmitter->emitIns_R_R_R(INS_fabd, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fabd, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fabd, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2D);

    // frecps
    theEmitter->emitIns_R_R_R(INS_frecps, EA_4BYTE, REG_V0, REG_V1, REG_V2); // scalar 4BYTE
    theEmitter->emitIns_R_R_R(INS_frecps, EA_8BYTE, REG_V3, REG_V4, REG_V5); // scalar 8BYTE
    theEmitter->emitIns_R_R_R(INS_frecps, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_frecps, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_frecps, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2D);

    // frsqrts
    theEmitter->emitIns_R_R_R(INS_frsqrts, EA_4BYTE, REG_V0, REG_V1, REG_V2); // scalar 4BYTE
    theEmitter->emitIns_R_R_R(INS_frsqrts, EA_8BYTE, REG_V3, REG_V4, REG_V5); // scalar 8BYTE
    theEmitter->emitIns_R_R_R(INS_frsqrts, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_frsqrts, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_frsqrts, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2D);

    genDefineTempLabel(genCreateTempLabel());

    theEmitter->emitIns_R_R_R(INS_fmul, EA_4BYTE, REG_V0, REG_V1, REG_V2); // scalar 4BYTE
    theEmitter->emitIns_R_R_R(INS_fmul, EA_8BYTE, REG_V3, REG_V4, REG_V5); // scalar 8BYTE
    theEmitter->emitIns_R_R_R(INS_fmul, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fmul, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fmul, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2D);

    theEmitter->emitIns_R_R_R_I(INS_fmul, EA_4BYTE, REG_V15, REG_V16, REG_V17, 3); // scalar by element 4BYTE
    theEmitter->emitIns_R_R_R_I(INS_fmul, EA_8BYTE, REG_V18, REG_V19, REG_V20, 1); // scalar by element 8BYTE
    theEmitter->emitIns_R_R_R_I(INS_fmul, EA_8BYTE, REG_V21, REG_V22, REG_V23, 0, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_fmul, EA_16BYTE, REG_V24, REG_V25, REG_V26, 2, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R_I(INS_fmul, EA_16BYTE, REG_V27, REG_V28, REG_V29, 0, INS_OPTS_2D);

    theEmitter->emitIns_R_R_R(INS_fmulx, EA_4BYTE, REG_V0, REG_V1, REG_V2); // scalar 4BYTE
    theEmitter->emitIns_R_R_R(INS_fmulx, EA_8BYTE, REG_V3, REG_V4, REG_V5); // scalar 8BYTE
    theEmitter->emitIns_R_R_R(INS_fmulx, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fmulx, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fmulx, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2D);

    theEmitter->emitIns_R_R_R_I(INS_fmulx, EA_4BYTE, REG_V15, REG_V16, REG_V17, 3); // scalar by element 4BYTE
    theEmitter->emitIns_R_R_R_I(INS_fmulx, EA_8BYTE, REG_V18, REG_V19, REG_V20, 1); // scalar by element 8BYTE
    theEmitter->emitIns_R_R_R_I(INS_fmulx, EA_8BYTE, REG_V21, REG_V22, REG_V23, 0, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_fmulx, EA_16BYTE, REG_V24, REG_V25, REG_V26, 2, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R_I(INS_fmulx, EA_16BYTE, REG_V27, REG_V28, REG_V29, 0, INS_OPTS_2D);

    theEmitter->emitIns_R_R_R(INS_fnmul, EA_4BYTE, REG_V0, REG_V1, REG_V2); // scalar 4BYTE
    theEmitter->emitIns_R_R_R(INS_fnmul, EA_8BYTE, REG_V3, REG_V4, REG_V5); // scalar 8BYTE

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R_I  vector operations, one dest, one source reg, one immed
    //

    // Some of the tests cases below might appear redundant since they emit same combinations of instruction x size x
    // vector arrangements. However, these are added to verify that the split constant encoding works with both - small
    // and large constants.

    genDefineTempLabel(genCreateTempLabel());

    // sshr scalar
    theEmitter->emitIns_R_R_I(INS_sshr, EA_8BYTE, REG_V0, REG_V1, 1);
    theEmitter->emitIns_R_R_I(INS_sshr, EA_8BYTE, REG_V2, REG_V3, 14);
    theEmitter->emitIns_R_R_I(INS_sshr, EA_8BYTE, REG_V4, REG_V5, 27);
    theEmitter->emitIns_R_R_I(INS_sshr, EA_8BYTE, REG_V6, REG_V7, 40);
    theEmitter->emitIns_R_R_I(INS_sshr, EA_8BYTE, REG_V8, REG_V9, 64);

    // sshr vector
    theEmitter->emitIns_R_R_I(INS_sshr, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_sshr, EA_16BYTE, REG_V2, REG_V3, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_sshr, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_sshr, EA_16BYTE, REG_V6, REG_V7, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_sshr, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_sshr, EA_16BYTE, REG_V10, REG_V11, 32, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_sshr, EA_16BYTE, REG_V12, REG_V13, 33, INS_OPTS_2D);
    theEmitter->emitIns_R_R_I(INS_sshr, EA_16BYTE, REG_V14, REG_V15, 64, INS_OPTS_2D);

    // ssra scalar
    theEmitter->emitIns_R_R_I(INS_ssra, EA_8BYTE, REG_V0, REG_V1, 1);
    theEmitter->emitIns_R_R_I(INS_ssra, EA_8BYTE, REG_V2, REG_V3, 14);
    theEmitter->emitIns_R_R_I(INS_ssra, EA_8BYTE, REG_V4, REG_V5, 27);
    theEmitter->emitIns_R_R_I(INS_ssra, EA_8BYTE, REG_V6, REG_V7, 40);
    theEmitter->emitIns_R_R_I(INS_ssra, EA_8BYTE, REG_V8, REG_V9, 64);

    // ssra vector
    theEmitter->emitIns_R_R_I(INS_ssra, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_ssra, EA_16BYTE, REG_V2, REG_V3, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_ssra, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_ssra, EA_16BYTE, REG_V6, REG_V7, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_ssra, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_ssra, EA_16BYTE, REG_V10, REG_V11, 32, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_ssra, EA_16BYTE, REG_V12, REG_V13, 33, INS_OPTS_2D);
    theEmitter->emitIns_R_R_I(INS_ssra, EA_16BYTE, REG_V14, REG_V15, 64, INS_OPTS_2D);

    // srshr scalar
    theEmitter->emitIns_R_R_I(INS_srshr, EA_8BYTE, REG_V0, REG_V1, 1);
    theEmitter->emitIns_R_R_I(INS_srshr, EA_8BYTE, REG_V2, REG_V3, 14);
    theEmitter->emitIns_R_R_I(INS_srshr, EA_8BYTE, REG_V4, REG_V5, 27);
    theEmitter->emitIns_R_R_I(INS_srshr, EA_8BYTE, REG_V6, REG_V7, 40);
    theEmitter->emitIns_R_R_I(INS_srshr, EA_8BYTE, REG_V8, REG_V9, 64);

    // srshr vector
    theEmitter->emitIns_R_R_I(INS_srshr, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_srshr, EA_16BYTE, REG_V2, REG_V3, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_srshr, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_srshr, EA_16BYTE, REG_V6, REG_V7, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_srshr, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_srshr, EA_16BYTE, REG_V10, REG_V11, 32, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_srshr, EA_16BYTE, REG_V12, REG_V13, 33, INS_OPTS_2D);
    theEmitter->emitIns_R_R_I(INS_srshr, EA_16BYTE, REG_V14, REG_V15, 64, INS_OPTS_2D);

    // srsra scalar
    theEmitter->emitIns_R_R_I(INS_srsra, EA_8BYTE, REG_V0, REG_V1, 1);
    theEmitter->emitIns_R_R_I(INS_srsra, EA_8BYTE, REG_V2, REG_V3, 14);
    theEmitter->emitIns_R_R_I(INS_srsra, EA_8BYTE, REG_V4, REG_V5, 27);
    theEmitter->emitIns_R_R_I(INS_srsra, EA_8BYTE, REG_V6, REG_V7, 40);
    theEmitter->emitIns_R_R_I(INS_srsra, EA_8BYTE, REG_V8, REG_V9, 64);

    // srsra vector
    theEmitter->emitIns_R_R_I(INS_srsra, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_srsra, EA_16BYTE, REG_V2, REG_V3, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_srsra, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_srsra, EA_16BYTE, REG_V6, REG_V7, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_srsra, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_srsra, EA_16BYTE, REG_V10, REG_V11, 32, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_srsra, EA_16BYTE, REG_V12, REG_V13, 33, INS_OPTS_2D);
    theEmitter->emitIns_R_R_I(INS_srsra, EA_16BYTE, REG_V14, REG_V15, 64, INS_OPTS_2D);

    // shl scalar
    theEmitter->emitIns_R_R_I(INS_shl, EA_8BYTE, REG_V0, REG_V1, 0);
    theEmitter->emitIns_R_R_I(INS_shl, EA_8BYTE, REG_V2, REG_V3, 14);
    theEmitter->emitIns_R_R_I(INS_shl, EA_8BYTE, REG_V4, REG_V5, 27);
    theEmitter->emitIns_R_R_I(INS_shl, EA_8BYTE, REG_V6, REG_V7, 40);
    theEmitter->emitIns_R_R_I(INS_shl, EA_8BYTE, REG_V8, REG_V9, 63);

    // shl vector
    theEmitter->emitIns_R_R_I(INS_shl, EA_8BYTE, REG_V0, REG_V1, 0, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_shl, EA_16BYTE, REG_V2, REG_V3, 7, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_shl, EA_8BYTE, REG_V4, REG_V5, 8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_shl, EA_16BYTE, REG_V6, REG_V7, 15, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_shl, EA_8BYTE, REG_V8, REG_V9, 16, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_shl, EA_16BYTE, REG_V10, REG_V11, 31, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_shl, EA_16BYTE, REG_V12, REG_V13, 32, INS_OPTS_2D);
    theEmitter->emitIns_R_R_I(INS_shl, EA_16BYTE, REG_V14, REG_V15, 63, INS_OPTS_2D);

    // ushr scalar
    theEmitter->emitIns_R_R_I(INS_ushr, EA_8BYTE, REG_V0, REG_V1, 1);
    theEmitter->emitIns_R_R_I(INS_ushr, EA_8BYTE, REG_V2, REG_V3, 14);
    theEmitter->emitIns_R_R_I(INS_ushr, EA_8BYTE, REG_V4, REG_V5, 27);
    theEmitter->emitIns_R_R_I(INS_ushr, EA_8BYTE, REG_V6, REG_V7, 40);
    theEmitter->emitIns_R_R_I(INS_ushr, EA_8BYTE, REG_V8, REG_V9, 64);

    // ushr vector
    theEmitter->emitIns_R_R_I(INS_ushr, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_ushr, EA_16BYTE, REG_V2, REG_V3, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_ushr, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_ushr, EA_16BYTE, REG_V6, REG_V7, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_ushr, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_ushr, EA_16BYTE, REG_V10, REG_V11, 32, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_ushr, EA_16BYTE, REG_V12, REG_V13, 33, INS_OPTS_2D);
    theEmitter->emitIns_R_R_I(INS_ushr, EA_16BYTE, REG_V14, REG_V15, 64, INS_OPTS_2D);

    // usra scalar
    theEmitter->emitIns_R_R_I(INS_usra, EA_8BYTE, REG_V0, REG_V1, 1);
    theEmitter->emitIns_R_R_I(INS_usra, EA_8BYTE, REG_V2, REG_V3, 14);
    theEmitter->emitIns_R_R_I(INS_usra, EA_8BYTE, REG_V4, REG_V5, 27);
    theEmitter->emitIns_R_R_I(INS_usra, EA_8BYTE, REG_V6, REG_V7, 40);
    theEmitter->emitIns_R_R_I(INS_usra, EA_8BYTE, REG_V8, REG_V9, 64);

    // usra vector
    theEmitter->emitIns_R_R_I(INS_usra, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_usra, EA_16BYTE, REG_V2, REG_V3, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_usra, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_usra, EA_16BYTE, REG_V6, REG_V7, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_usra, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_usra, EA_16BYTE, REG_V10, REG_V11, 32, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_usra, EA_16BYTE, REG_V12, REG_V13, 33, INS_OPTS_2D);
    theEmitter->emitIns_R_R_I(INS_usra, EA_16BYTE, REG_V14, REG_V15, 64, INS_OPTS_2D);

    // urshr scalar
    theEmitter->emitIns_R_R_I(INS_urshr, EA_8BYTE, REG_V0, REG_V1, 1);
    theEmitter->emitIns_R_R_I(INS_urshr, EA_8BYTE, REG_V2, REG_V3, 14);
    theEmitter->emitIns_R_R_I(INS_urshr, EA_8BYTE, REG_V4, REG_V5, 27);
    theEmitter->emitIns_R_R_I(INS_urshr, EA_8BYTE, REG_V6, REG_V7, 40);
    theEmitter->emitIns_R_R_I(INS_urshr, EA_8BYTE, REG_V8, REG_V9, 64);

    // urshr vector
    theEmitter->emitIns_R_R_I(INS_urshr, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_urshr, EA_16BYTE, REG_V2, REG_V3, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_urshr, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_urshr, EA_16BYTE, REG_V6, REG_V7, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_urshr, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_urshr, EA_16BYTE, REG_V10, REG_V11, 32, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_urshr, EA_16BYTE, REG_V12, REG_V13, 33, INS_OPTS_2D);
    theEmitter->emitIns_R_R_I(INS_urshr, EA_16BYTE, REG_V14, REG_V15, 64, INS_OPTS_2D);

    // ursra scalar
    theEmitter->emitIns_R_R_I(INS_ursra, EA_8BYTE, REG_V0, REG_V1, 1);
    theEmitter->emitIns_R_R_I(INS_ursra, EA_8BYTE, REG_V2, REG_V3, 14);
    theEmitter->emitIns_R_R_I(INS_ursra, EA_8BYTE, REG_V4, REG_V5, 27);
    theEmitter->emitIns_R_R_I(INS_ursra, EA_8BYTE, REG_V6, REG_V7, 40);
    theEmitter->emitIns_R_R_I(INS_ursra, EA_8BYTE, REG_V8, REG_V9, 64);

    // ursra vector
    theEmitter->emitIns_R_R_I(INS_ursra, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_ursra, EA_16BYTE, REG_V2, REG_V3, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_ursra, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_ursra, EA_16BYTE, REG_V6, REG_V7, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_ursra, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_ursra, EA_16BYTE, REG_V10, REG_V11, 32, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_ursra, EA_16BYTE, REG_V12, REG_V13, 33, INS_OPTS_2D);
    theEmitter->emitIns_R_R_I(INS_ursra, EA_16BYTE, REG_V14, REG_V15, 64, INS_OPTS_2D);

    // sri scalar
    theEmitter->emitIns_R_R_I(INS_sri, EA_8BYTE, REG_V0, REG_V1, 1);
    theEmitter->emitIns_R_R_I(INS_sri, EA_8BYTE, REG_V2, REG_V3, 14);
    theEmitter->emitIns_R_R_I(INS_sri, EA_8BYTE, REG_V4, REG_V5, 27);
    theEmitter->emitIns_R_R_I(INS_sri, EA_8BYTE, REG_V6, REG_V7, 40);
    theEmitter->emitIns_R_R_I(INS_sri, EA_8BYTE, REG_V8, REG_V9, 64);

    // sri vector
    theEmitter->emitIns_R_R_I(INS_sri, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_sri, EA_16BYTE, REG_V2, REG_V3, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_sri, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_sri, EA_16BYTE, REG_V6, REG_V7, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_sri, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_sri, EA_16BYTE, REG_V10, REG_V11, 32, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_sri, EA_16BYTE, REG_V12, REG_V13, 33, INS_OPTS_2D);
    theEmitter->emitIns_R_R_I(INS_sri, EA_16BYTE, REG_V14, REG_V15, 64, INS_OPTS_2D);

    // sli scalar
    theEmitter->emitIns_R_R_I(INS_sli, EA_8BYTE, REG_V0, REG_V1, 0);
    theEmitter->emitIns_R_R_I(INS_sli, EA_8BYTE, REG_V2, REG_V3, 14);
    theEmitter->emitIns_R_R_I(INS_sli, EA_8BYTE, REG_V4, REG_V5, 27);
    theEmitter->emitIns_R_R_I(INS_sli, EA_8BYTE, REG_V6, REG_V7, 40);
    theEmitter->emitIns_R_R_I(INS_sli, EA_8BYTE, REG_V8, REG_V9, 63);

    // sli vector
    theEmitter->emitIns_R_R_I(INS_sli, EA_8BYTE, REG_V0, REG_V1, 0, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_sli, EA_16BYTE, REG_V2, REG_V3, 7, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_sli, EA_8BYTE, REG_V4, REG_V5, 8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_sli, EA_16BYTE, REG_V6, REG_V7, 15, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_sli, EA_8BYTE, REG_V8, REG_V9, 16, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_sli, EA_16BYTE, REG_V10, REG_V11, 31, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_sli, EA_16BYTE, REG_V12, REG_V13, 32, INS_OPTS_2D);
    theEmitter->emitIns_R_R_I(INS_sli, EA_16BYTE, REG_V14, REG_V15, 63, INS_OPTS_2D);

    // sshll{2} vector
    theEmitter->emitIns_R_R_I(INS_sshll, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_sshll2, EA_16BYTE, REG_V2, REG_V3, 7, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_sshll, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_sshll2, EA_16BYTE, REG_V6, REG_V7, 15, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_sshll, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_sshll2, EA_16BYTE, REG_V10, REG_V11, 31, INS_OPTS_4S);

    // ushll{2} vector
    theEmitter->emitIns_R_R_I(INS_ushll, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_ushll2, EA_16BYTE, REG_V2, REG_V3, 7, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_ushll, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_ushll2, EA_16BYTE, REG_V6, REG_V7, 15, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_ushll, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_ushll2, EA_16BYTE, REG_V10, REG_V11, 31, INS_OPTS_4S);

    // shrn{2} vector
    theEmitter->emitIns_R_R_I(INS_shrn, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_shrn2, EA_16BYTE, REG_V2, REG_V3, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_shrn, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_shrn2, EA_16BYTE, REG_V6, REG_V7, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_shrn, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_shrn2, EA_16BYTE, REG_V10, REG_V11, 32, INS_OPTS_4S);

    // rshrn{2} vector
    theEmitter->emitIns_R_R_I(INS_rshrn, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_rshrn2, EA_16BYTE, REG_V2, REG_V3, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_rshrn, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_rshrn2, EA_16BYTE, REG_V6, REG_V7, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_rshrn, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_rshrn2, EA_16BYTE, REG_V10, REG_V11, 32, INS_OPTS_4S);

    // sxtl{2} vector
    theEmitter->emitIns_R_R(INS_sxtl, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_sxtl2, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_sxtl, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_sxtl2, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_sxtl, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_sxtl2, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);

    // uxtl{2} vector
    theEmitter->emitIns_R_R(INS_uxtl, EA_8BYTE, REG_V0, REG_V1, INS_OPTS_8B);
    theEmitter->emitIns_R_R(INS_uxtl2, EA_16BYTE, REG_V2, REG_V3, INS_OPTS_16B);
    theEmitter->emitIns_R_R(INS_uxtl, EA_8BYTE, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R(INS_uxtl2, EA_16BYTE, REG_V6, REG_V7, INS_OPTS_8H);
    theEmitter->emitIns_R_R(INS_uxtl, EA_8BYTE, REG_V8, REG_V9, INS_OPTS_2S);
    theEmitter->emitIns_R_R(INS_uxtl2, EA_16BYTE, REG_V10, REG_V11, INS_OPTS_4S);

    // sqrshrn scalar
    theEmitter->emitIns_R_R_I(INS_sqrshrn, EA_1BYTE, REG_V0, REG_V1, 1, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqrshrn, EA_1BYTE, REG_V2, REG_V3, 8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqrshrn, EA_2BYTE, REG_V4, REG_V5, 9, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqrshrn, EA_2BYTE, REG_V6, REG_V7, 16, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqrshrn, EA_4BYTE, REG_V8, REG_V9, 17, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqrshrn, EA_4BYTE, REG_V10, REG_V11, 32, INS_OPTS_NONE);

    // sqrshrn{2} vector
    theEmitter->emitIns_R_R_I(INS_sqrshrn, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_sqrshrn, EA_8BYTE, REG_V2, REG_V3, 8, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_sqrshrn2, EA_16BYTE, REG_V4, REG_V5, 1, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_sqrshrn2, EA_16BYTE, REG_V6, REG_V7, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_sqrshrn, EA_8BYTE, REG_V8, REG_V9, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_sqrshrn, EA_8BYTE, REG_V10, REG_V11, 16, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_sqrshrn2, EA_16BYTE, REG_V12, REG_V13, 9, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_sqrshrn2, EA_16BYTE, REG_V14, REG_V15, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_sqrshrn, EA_8BYTE, REG_V16, REG_V17, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_sqrshrn, EA_8BYTE, REG_V18, REG_V18, 32, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_sqrshrn2, EA_16BYTE, REG_V20, REG_V21, 17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_sqrshrn2, EA_16BYTE, REG_V22, REG_V23, 32, INS_OPTS_4S);

    // sqrshrun scalar
    theEmitter->emitIns_R_R_I(INS_sqrshrun, EA_1BYTE, REG_V0, REG_V1, 1, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqrshrun, EA_1BYTE, REG_V0, REG_V1, 8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqrshrun, EA_2BYTE, REG_V2, REG_V3, 9, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqrshrun, EA_2BYTE, REG_V2, REG_V3, 16, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqrshrun, EA_4BYTE, REG_V4, REG_V5, 17, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqrshrun, EA_4BYTE, REG_V4, REG_V5, 32, INS_OPTS_NONE);

    // sqrshrun{2} vector
    theEmitter->emitIns_R_R_I(INS_sqrshrun, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_sqrshrun, EA_8BYTE, REG_V2, REG_V3, 8, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_sqrshrun2, EA_16BYTE, REG_V4, REG_V5, 1, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_sqrshrun2, EA_16BYTE, REG_V6, REG_V7, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_sqrshrun, EA_8BYTE, REG_V8, REG_V9, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_sqrshrun, EA_8BYTE, REG_V10, REG_V11, 16, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_sqrshrun2, EA_16BYTE, REG_V12, REG_V13, 9, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_sqrshrun2, EA_16BYTE, REG_V14, REG_V15, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_sqrshrun, EA_8BYTE, REG_V16, REG_V17, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_sqrshrun, EA_8BYTE, REG_V18, REG_V18, 32, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_sqrshrun2, EA_16BYTE, REG_V20, REG_V21, 17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_sqrshrun2, EA_16BYTE, REG_V22, REG_V23, 32, INS_OPTS_4S);

    // sqshl scalar
    theEmitter->emitIns_R_R_I(INS_sqshl, EA_1BYTE, REG_V0, REG_V1, 0, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshl, EA_1BYTE, REG_V2, REG_V3, 7, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshl, EA_2BYTE, REG_V4, REG_V5, 8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshl, EA_2BYTE, REG_V6, REG_V7, 15, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshl, EA_4BYTE, REG_V8, REG_V9, 16, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshl, EA_4BYTE, REG_V10, REG_V11, 31, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshl, EA_8BYTE, REG_V12, REG_V13, 32, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshl, EA_8BYTE, REG_V14, REG_V15, 63, INS_OPTS_NONE);

    // sqshl vector
    theEmitter->emitIns_R_R_I(INS_sqshl, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_sqshl, EA_16BYTE, REG_V2, REG_V3, 7, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_sqshl, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_sqshl, EA_16BYTE, REG_V6, REG_V7, 15, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_sqshl, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_sqshl, EA_16BYTE, REG_V10, REG_V11, 31, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_sqshl, EA_16BYTE, REG_V12, REG_V13, 63, INS_OPTS_2D);

    // sqshlu scalar
    theEmitter->emitIns_R_R_I(INS_sqshlu, EA_1BYTE, REG_V0, REG_V1, 0, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshlu, EA_1BYTE, REG_V2, REG_V3, 7, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshlu, EA_2BYTE, REG_V4, REG_V5, 8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshlu, EA_2BYTE, REG_V6, REG_V7, 15, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshlu, EA_4BYTE, REG_V8, REG_V9, 16, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshlu, EA_4BYTE, REG_V10, REG_V11, 31, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshlu, EA_8BYTE, REG_V12, REG_V13, 32, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshlu, EA_8BYTE, REG_V14, REG_V15, 63, INS_OPTS_NONE);

    // sqshlu vector
    theEmitter->emitIns_R_R_I(INS_sqshlu, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_sqshlu, EA_16BYTE, REG_V2, REG_V3, 7, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_sqshlu, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_sqshlu, EA_16BYTE, REG_V6, REG_V7, 15, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_sqshlu, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_sqshlu, EA_16BYTE, REG_V10, REG_V11, 31, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_sqshlu, EA_16BYTE, REG_V12, REG_V13, 63, INS_OPTS_2D);

    // sqshrn scalar
    theEmitter->emitIns_R_R_I(INS_sqshrn, EA_1BYTE, REG_V0, REG_V1, 1, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshrn, EA_1BYTE, REG_V2, REG_V3, 8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshrn, EA_2BYTE, REG_V4, REG_V5, 9, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshrn, EA_2BYTE, REG_V6, REG_V7, 16, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshrn, EA_4BYTE, REG_V8, REG_V9, 17, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshrn, EA_4BYTE, REG_V10, REG_V11, 32, INS_OPTS_NONE);

    // sqshrn{2} vector
    theEmitter->emitIns_R_R_I(INS_sqshrn, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_sqshrn, EA_8BYTE, REG_V2, REG_V3, 8, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_sqshrn2, EA_16BYTE, REG_V4, REG_V5, 1, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_sqshrn2, EA_16BYTE, REG_V6, REG_V7, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_sqshrn, EA_8BYTE, REG_V8, REG_V9, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_sqshrn, EA_8BYTE, REG_V10, REG_V11, 16, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_sqshrn2, EA_16BYTE, REG_V12, REG_V13, 9, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_sqshrn2, EA_16BYTE, REG_V14, REG_V15, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_sqshrn, EA_8BYTE, REG_V16, REG_V17, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_sqshrn, EA_8BYTE, REG_V18, REG_V18, 32, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_sqshrn2, EA_16BYTE, REG_V20, REG_V21, 17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_sqshrn2, EA_16BYTE, REG_V22, REG_V23, 32, INS_OPTS_4S);

    // sqshrun scalar
    theEmitter->emitIns_R_R_I(INS_sqshrun, EA_1BYTE, REG_V0, REG_V1, 1, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshrun, EA_1BYTE, REG_V2, REG_V3, 8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshrun, EA_2BYTE, REG_V4, REG_V5, 9, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshrun, EA_2BYTE, REG_V6, REG_V7, 16, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshrun, EA_4BYTE, REG_V8, REG_V9, 17, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_sqshrun, EA_4BYTE, REG_V10, REG_V11, 32, INS_OPTS_NONE);

    // sqshrun{2} vector
    theEmitter->emitIns_R_R_I(INS_sqshrun, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_sqshrun, EA_8BYTE, REG_V2, REG_V3, 8, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_sqshrun2, EA_16BYTE, REG_V4, REG_V5, 1, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_sqshrun2, EA_16BYTE, REG_V6, REG_V7, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_sqshrun, EA_8BYTE, REG_V8, REG_V9, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_sqshrun, EA_8BYTE, REG_V10, REG_V11, 16, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_sqshrun2, EA_16BYTE, REG_V12, REG_V13, 9, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_sqshrun2, EA_16BYTE, REG_V14, REG_V15, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_sqshrun, EA_8BYTE, REG_V16, REG_V17, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_sqshrun, EA_8BYTE, REG_V18, REG_V18, 32, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_sqshrun2, EA_16BYTE, REG_V20, REG_V21, 17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_sqshrun2, EA_16BYTE, REG_V22, REG_V23, 32, INS_OPTS_4S);

    // uqrshrn scalar
    theEmitter->emitIns_R_R_I(INS_uqrshrn, EA_1BYTE, REG_V0, REG_V1, 1, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqrshrn, EA_1BYTE, REG_V2, REG_V3, 8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqrshrn, EA_2BYTE, REG_V4, REG_V5, 9, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqrshrn, EA_2BYTE, REG_V6, REG_V7, 16, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqrshrn, EA_4BYTE, REG_V8, REG_V9, 17, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqrshrn, EA_4BYTE, REG_V10, REG_V11, 32, INS_OPTS_NONE);

    // uqrshrn{2} vector
    theEmitter->emitIns_R_R_I(INS_uqrshrn, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_uqrshrn, EA_8BYTE, REG_V2, REG_V3, 8, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_uqrshrn2, EA_16BYTE, REG_V4, REG_V5, 1, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_uqrshrn2, EA_16BYTE, REG_V6, REG_V7, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_uqrshrn, EA_8BYTE, REG_V8, REG_V9, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_uqrshrn, EA_8BYTE, REG_V10, REG_V11, 16, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_uqrshrn2, EA_16BYTE, REG_V12, REG_V13, 9, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_uqrshrn2, EA_16BYTE, REG_V14, REG_V15, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_uqrshrn, EA_8BYTE, REG_V16, REG_V17, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_uqrshrn, EA_8BYTE, REG_V18, REG_V18, 32, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_uqrshrn2, EA_16BYTE, REG_V20, REG_V21, 17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_uqrshrn2, EA_16BYTE, REG_V22, REG_V23, 32, INS_OPTS_4S);

    // uqshl scalar
    theEmitter->emitIns_R_R_I(INS_uqshl, EA_1BYTE, REG_V0, REG_V1, 0, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqshl, EA_1BYTE, REG_V2, REG_V3, 7, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqshl, EA_2BYTE, REG_V4, REG_V5, 8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqshl, EA_2BYTE, REG_V6, REG_V7, 15, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqshl, EA_4BYTE, REG_V8, REG_V9, 16, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqshl, EA_4BYTE, REG_V10, REG_V11, 31, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqshl, EA_8BYTE, REG_V12, REG_V13, 32, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqshl, EA_8BYTE, REG_V14, REG_V15, 63, INS_OPTS_NONE);

    // uqshl vector
    theEmitter->emitIns_R_R_I(INS_uqshl, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_uqshl, EA_16BYTE, REG_V2, REG_V3, 7, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_uqshl, EA_8BYTE, REG_V4, REG_V5, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_uqshl, EA_16BYTE, REG_V6, REG_V7, 15, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_uqshl, EA_8BYTE, REG_V8, REG_V9, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_uqshl, EA_16BYTE, REG_V10, REG_V11, 31, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_uqshl, EA_16BYTE, REG_V12, REG_V13, 63, INS_OPTS_2D);

    // uqshrn scalar
    theEmitter->emitIns_R_R_I(INS_uqshrn, EA_1BYTE, REG_V0, REG_V1, 1, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqshrn, EA_1BYTE, REG_V2, REG_V3, 8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqshrn, EA_2BYTE, REG_V4, REG_V5, 9, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqshrn, EA_2BYTE, REG_V6, REG_V7, 16, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqshrn, EA_4BYTE, REG_V8, REG_V9, 17, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_I(INS_uqshrn, EA_4BYTE, REG_V10, REG_V11, 32, INS_OPTS_NONE);

    // uqshrn{2} vector
    theEmitter->emitIns_R_R_I(INS_uqshrn, EA_8BYTE, REG_V0, REG_V1, 1, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_uqshrn, EA_8BYTE, REG_V2, REG_V3, 8, INS_OPTS_8B);
    theEmitter->emitIns_R_R_I(INS_uqshrn2, EA_16BYTE, REG_V4, REG_V5, 1, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_uqshrn2, EA_16BYTE, REG_V6, REG_V7, 8, INS_OPTS_16B);
    theEmitter->emitIns_R_R_I(INS_uqshrn, EA_8BYTE, REG_V8, REG_V9, 9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_uqshrn, EA_8BYTE, REG_V10, REG_V11, 16, INS_OPTS_4H);
    theEmitter->emitIns_R_R_I(INS_uqshrn2, EA_16BYTE, REG_V12, REG_V13, 9, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_uqshrn2, EA_16BYTE, REG_V14, REG_V15, 16, INS_OPTS_8H);
    theEmitter->emitIns_R_R_I(INS_uqshrn, EA_8BYTE, REG_V16, REG_V17, 17, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_uqshrn, EA_8BYTE, REG_V18, REG_V18, 32, INS_OPTS_2S);
    theEmitter->emitIns_R_R_I(INS_uqshrn2, EA_16BYTE, REG_V20, REG_V21, 17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_I(INS_uqshrn2, EA_16BYTE, REG_V22, REG_V23, 32, INS_OPTS_4S);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R_R   vector operations, one dest, two source
    //

    genDefineTempLabel(genCreateTempLabel());

    // Specifying an Arrangement is optional
    //
    theEmitter->emitIns_R_R_R(INS_and, EA_8BYTE, REG_V6, REG_V7, REG_V8);
    theEmitter->emitIns_R_R_R(INS_bic, EA_8BYTE, REG_V9, REG_V10, REG_V11);
    theEmitter->emitIns_R_R_R(INS_eor, EA_8BYTE, REG_V12, REG_V13, REG_V14);
    theEmitter->emitIns_R_R_R(INS_orr, EA_8BYTE, REG_V15, REG_V16, REG_V17);
    theEmitter->emitIns_R_R_R(INS_orn, EA_8BYTE, REG_V18, REG_V19, REG_V20);
    theEmitter->emitIns_R_R_R(INS_and, EA_16BYTE, REG_V21, REG_V22, REG_V23);
    theEmitter->emitIns_R_R_R(INS_bic, EA_16BYTE, REG_V24, REG_V25, REG_V26);
    theEmitter->emitIns_R_R_R(INS_eor, EA_16BYTE, REG_V27, REG_V28, REG_V29);
    theEmitter->emitIns_R_R_R(INS_orr, EA_16BYTE, REG_V30, REG_V31, REG_V0);
    theEmitter->emitIns_R_R_R(INS_orn, EA_16BYTE, REG_V1, REG_V2, REG_V3);

    theEmitter->emitIns_R_R_R(INS_bsl, EA_8BYTE, REG_V4, REG_V5, REG_V6);
    theEmitter->emitIns_R_R_R(INS_bit, EA_8BYTE, REG_V7, REG_V8, REG_V9);
    theEmitter->emitIns_R_R_R(INS_bif, EA_8BYTE, REG_V10, REG_V11, REG_V12);
    theEmitter->emitIns_R_R_R(INS_bsl, EA_16BYTE, REG_V13, REG_V14, REG_V15);
    theEmitter->emitIns_R_R_R(INS_bit, EA_16BYTE, REG_V16, REG_V17, REG_V18);
    theEmitter->emitIns_R_R_R(INS_bif, EA_16BYTE, REG_V19, REG_V20, REG_V21);

    // Default Arrangement as per the ARM64 manual
    //
    theEmitter->emitIns_R_R_R(INS_and, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_bic, EA_8BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_eor, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_orr, EA_8BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_orn, EA_8BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_and, EA_16BYTE, REG_V21, REG_V22, REG_V23, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_bic, EA_16BYTE, REG_V24, REG_V25, REG_V26, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_eor, EA_16BYTE, REG_V27, REG_V28, REG_V29, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_orr, EA_16BYTE, REG_V30, REG_V31, REG_V0, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_orn, EA_16BYTE, REG_V1, REG_V2, REG_V3, INS_OPTS_16B);

    theEmitter->emitIns_R_R_R(INS_bsl, EA_8BYTE, REG_V4, REG_V5, REG_V6, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_bit, EA_8BYTE, REG_V7, REG_V8, REG_V9, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_bif, EA_8BYTE, REG_V10, REG_V11, REG_V12, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_bsl, EA_16BYTE, REG_V13, REG_V14, REG_V15, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_bit, EA_16BYTE, REG_V16, REG_V17, REG_V18, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_bif, EA_16BYTE, REG_V19, REG_V20, REG_V21, INS_OPTS_16B);

    genDefineTempLabel(genCreateTempLabel());

    // add
    theEmitter->emitIns_R_R_R(INS_add, EA_8BYTE, REG_V0, REG_V1, REG_V2); // scalar 8BYTE
    theEmitter->emitIns_R_R_R(INS_add, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_add, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_add, EA_8BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_add, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_add, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_add, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_add, EA_16BYTE, REG_V21, REG_V22, REG_V23, INS_OPTS_2D);

    // addp
    theEmitter->emitIns_R_R(INS_addp, EA_16BYTE, REG_V0, REG_V1, INS_OPTS_2D); // scalar 16BYTE
    theEmitter->emitIns_R_R_R(INS_addp, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_addp, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_addp, EA_8BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_addp, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_addp, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_addp, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_addp, EA_16BYTE, REG_V21, REG_V22, REG_V23, INS_OPTS_2D);

    // sub
    theEmitter->emitIns_R_R_R(INS_sub, EA_8BYTE, REG_V1, REG_V2, REG_V3); // scalar 8BYTE
    theEmitter->emitIns_R_R_R(INS_sub, EA_8BYTE, REG_V4, REG_V5, REG_V6, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_sub, EA_8BYTE, REG_V7, REG_V8, REG_V9, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sub, EA_8BYTE, REG_V10, REG_V11, REG_V12, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_sub, EA_16BYTE, REG_V13, REG_V14, REG_V15, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_sub, EA_16BYTE, REG_V16, REG_V17, REG_V18, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sub, EA_16BYTE, REG_V19, REG_V20, REG_V21, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_sub, EA_16BYTE, REG_V22, REG_V23, REG_V24, INS_OPTS_2D);

    genDefineTempLabel(genCreateTempLabel());

    // saba vector
    theEmitter->emitIns_R_R_R(INS_saba, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_saba, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_saba, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_saba, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_saba, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_saba, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // sabd vector
    theEmitter->emitIns_R_R_R(INS_sabd, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_sabd, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_sabd, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sabd, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sabd, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_sabd, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // uaba vector
    theEmitter->emitIns_R_R_R(INS_uaba, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_uaba, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_uaba, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_uaba, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_uaba, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_uaba, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // uabd vector
    theEmitter->emitIns_R_R_R(INS_uabd, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_uabd, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_uabd, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_uabd, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_uabd, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_uabd, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    // sdot vector
    theEmitter->emitIns_R_R_R(INS_sdot, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_sdot, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4S);

    // smax vector
    theEmitter->emitIns_R_R_R(INS_smax, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_smax, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_smax, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_smax, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_smax, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_smax, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // smaxp vector
    theEmitter->emitIns_R_R_R(INS_smaxp, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_smaxp, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_smaxp, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_smaxp, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_smaxp, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_smaxp, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // smin vector
    theEmitter->emitIns_R_R_R(INS_smin, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_smin, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_smin, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_smin, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_smin, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_smin, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // sminp vector
    theEmitter->emitIns_R_R_R(INS_sminp, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_sminp, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_sminp, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sminp, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sminp, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_sminp, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // udot vector
    theEmitter->emitIns_R_R_R(INS_udot, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_udot, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4S);

    // umax vector
    theEmitter->emitIns_R_R_R(INS_umax, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_umax, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_umax, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_umax, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_umax, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_umax, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // umaxp vector
    theEmitter->emitIns_R_R_R(INS_umaxp, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_umaxp, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_umaxp, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_umaxp, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_umaxp, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_umaxp, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // umin vector
    theEmitter->emitIns_R_R_R(INS_umin, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_umin, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_umin, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_umin, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_umin, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_umin, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // uminp vector
    theEmitter->emitIns_R_R_R(INS_uminp, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_uminp, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_uminp, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_uminp, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_uminp, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_uminp, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // cmeq vector
    theEmitter->emitIns_R_R_R(INS_cmeq, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_cmeq, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_cmeq, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_cmeq, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_cmeq, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_cmeq, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_cmeq, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // cmge vector
    theEmitter->emitIns_R_R_R(INS_cmge, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_cmge, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_cmge, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_cmge, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_cmge, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_cmge, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_cmge, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // cmgt vector
    theEmitter->emitIns_R_R_R(INS_cmgt, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_cmgt, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_cmgt, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_cmgt, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_cmgt, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_cmgt, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_cmgt, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // cmhi vector
    theEmitter->emitIns_R_R_R(INS_cmhi, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_cmhi, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_cmhi, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_cmhi, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_cmhi, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_cmhi, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_cmhi, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // cmhs vector
    theEmitter->emitIns_R_R_R(INS_cmhs, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_cmhs, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_cmhs, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_cmhs, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_cmhs, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_cmhs, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_cmhs, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // cmtst vector
    theEmitter->emitIns_R_R_R(INS_cmtst, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_cmtst, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_cmtst, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_cmtst, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_cmtst, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_cmtst, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_cmtst, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // faddp vector
    theEmitter->emitIns_R_R_R(INS_faddp, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_faddp, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_faddp, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_2D);

    // fcmeq vector
    theEmitter->emitIns_R_R_R(INS_fcmeq, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fcmeq, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fcmeq, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_2D);

    // fcmge vector
    theEmitter->emitIns_R_R_R(INS_fcmge, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fcmge, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fcmge, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_2D);

    // fcmgt vector
    theEmitter->emitIns_R_R_R(INS_fcmgt, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fcmgt, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fcmgt, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_2D);
#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    // trn1 vector
    theEmitter->emitIns_R_R_R(INS_trn1, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_trn1, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_trn1, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_trn1, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_trn1, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_trn1, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_trn1, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // trn2 vector
    theEmitter->emitIns_R_R_R(INS_trn2, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_trn2, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_trn2, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_trn2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_trn2, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_trn2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_trn2, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // uzp1 vector
    theEmitter->emitIns_R_R_R(INS_uzp1, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_uzp1, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_uzp1, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_uzp1, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_uzp1, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_uzp1, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_uzp1, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // uzp2 vector
    theEmitter->emitIns_R_R_R(INS_uzp2, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_uzp2, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_uzp2, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_uzp2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_uzp2, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_uzp2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_uzp2, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // zip1 vector
    theEmitter->emitIns_R_R_R(INS_zip1, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_zip1, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_zip1, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_zip1, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_zip1, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_zip1, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_zip1, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // zip2 vector
    theEmitter->emitIns_R_R_R(INS_zip2, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_zip2, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_zip2, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_zip2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_zip2, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_zip2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_zip2, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);
#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    // srshl scalar
    theEmitter->emitIns_R_R_R(INS_srshl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);

    // srshl vector
    theEmitter->emitIns_R_R_R(INS_srshl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_srshl, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_srshl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_srshl, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_srshl, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_srshl, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_srshl, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // sshl scalar
    theEmitter->emitIns_R_R_R(INS_sshl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);

    // sshl vector
    theEmitter->emitIns_R_R_R(INS_sshl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_sshl, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_sshl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sshl, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sshl, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_sshl, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_sshl, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // urshl scalar
    theEmitter->emitIns_R_R_R(INS_urshl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);

    // urshl vector
    theEmitter->emitIns_R_R_R(INS_urshl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_urshl, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_urshl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_urshl, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_urshl, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_urshl, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_urshl, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // ushl scalar
    theEmitter->emitIns_R_R_R(INS_ushl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);

    // ushl vector
    theEmitter->emitIns_R_R_R(INS_ushl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_ushl, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_ushl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_ushl, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_ushl, EA_8BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_ushl, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_ushl, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // addhn vector
    theEmitter->emitIns_R_R_R(INS_addhn, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_addhn, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_addhn, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // addhn2 vector
    theEmitter->emitIns_R_R_R(INS_addhn2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_addhn2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_addhn2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // raddhn vector
    theEmitter->emitIns_R_R_R(INS_raddhn, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_raddhn, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_raddhn, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // raddhn2 vector
    theEmitter->emitIns_R_R_R(INS_raddhn2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_raddhn2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_raddhn2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // rsubhn vector
    theEmitter->emitIns_R_R_R(INS_rsubhn, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_rsubhn, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_rsubhn, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // rsubhn2 vector
    theEmitter->emitIns_R_R_R(INS_rsubhn2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_rsubhn2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_rsubhn2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // sabal vector
    theEmitter->emitIns_R_R_R(INS_sabal, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_sabal, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sabal, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // sabal2 vector
    theEmitter->emitIns_R_R_R(INS_sabal2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_sabal2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sabal2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // sabdl vector
    theEmitter->emitIns_R_R_R(INS_sabdl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_sabdl, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sabdl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // sabdl2 vector
    theEmitter->emitIns_R_R_R(INS_sabdl2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_sabdl2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sabdl2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // saddl vector
    theEmitter->emitIns_R_R_R(INS_saddl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_saddl, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_saddl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // saddl2 vector
    theEmitter->emitIns_R_R_R(INS_saddl2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_saddl2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_saddl2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // saddw vector
    theEmitter->emitIns_R_R_R(INS_saddw, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_saddw, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_saddw, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // saddw2 vector
    theEmitter->emitIns_R_R_R(INS_saddw2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_saddw2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_saddw2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // shadd vector
    theEmitter->emitIns_R_R_R(INS_shadd, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_shadd, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_shadd, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_shadd, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_shadd, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_shadd, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // shsub vector
    theEmitter->emitIns_R_R_R(INS_shsub, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_shsub, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_shsub, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_shsub, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_shsub, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_shsub, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // sqadd scalar
    theEmitter->emitIns_R_R_R(INS_sqadd, EA_1BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqadd, EA_2BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqadd, EA_4BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqadd, EA_8BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_NONE);

    // sqadd vector
    theEmitter->emitIns_R_R_R(INS_sqadd, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_sqadd, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sqadd, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_sqadd, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_sqadd, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sqadd, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // sqrshl scalar
    theEmitter->emitIns_R_R_R(INS_sqrshl, EA_1BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqrshl, EA_2BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqrshl, EA_4BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqrshl, EA_8BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_NONE);

    // sqrshl vector
    theEmitter->emitIns_R_R_R(INS_sqrshl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_sqrshl, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sqrshl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_sqrshl, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_sqrshl, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sqrshl, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_sqrshl, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // sqshl scalar
    theEmitter->emitIns_R_R_R(INS_sqshl, EA_1BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqshl, EA_2BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqshl, EA_4BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqshl, EA_8BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_NONE);

    // sqshl vector
    theEmitter->emitIns_R_R_R(INS_sqshl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_sqshl, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sqshl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_sqshl, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_sqshl, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sqshl, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_sqshl, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // sqsub scalar
    theEmitter->emitIns_R_R_R(INS_sqsub, EA_1BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqsub, EA_2BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqsub, EA_4BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqsub, EA_8BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_NONE);

    // sqsub vector
    theEmitter->emitIns_R_R_R(INS_sqsub, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_sqsub, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sqsub, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_sqsub, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_sqsub, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sqsub, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // srhadd vector
    theEmitter->emitIns_R_R_R(INS_srhadd, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_srhadd, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_srhadd, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_srhadd, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_srhadd, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_srhadd, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // ssubl vector
    theEmitter->emitIns_R_R_R(INS_ssubl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_ssubl, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_ssubl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // ssubl2 vector
    theEmitter->emitIns_R_R_R(INS_ssubl2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_ssubl2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_ssubl2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // ssubw vector
    theEmitter->emitIns_R_R_R(INS_ssubw, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_ssubw, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_ssubw, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // ssubw2 vector
    theEmitter->emitIns_R_R_R(INS_ssubw2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_ssubw2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_ssubw2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // subhn vector
    theEmitter->emitIns_R_R_R(INS_subhn, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_subhn, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_subhn, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // sqdmlal scalar
    theEmitter->emitIns_R_R_R(INS_sqdmlal, EA_2BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqdmlal, EA_4BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_NONE);

    // sqdmlal vector
    theEmitter->emitIns_R_R_R(INS_sqdmlal, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sqdmlal, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_2S);

    // sqdmlal2 vector
    theEmitter->emitIns_R_R_R(INS_sqdmlal2, EA_16BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sqdmlal2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);

    // sqdmlsl scalar
    theEmitter->emitIns_R_R_R(INS_sqdmlsl, EA_2BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqdmlsl, EA_4BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_NONE);

    // sqdmlsl vector
    theEmitter->emitIns_R_R_R(INS_sqdmlsl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sqdmlsl, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_2S);

    // sqdmlsl2 vector
    theEmitter->emitIns_R_R_R(INS_sqdmlsl2, EA_16BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sqdmlsl2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);

    // sqdmulh scalar
    theEmitter->emitIns_R_R_R(INS_sqdmulh, EA_2BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqdmulh, EA_4BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_NONE);

    // sqdmulh vector
    theEmitter->emitIns_R_R_R(INS_sqdmulh, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sqdmulh, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_sqdmulh, EA_16BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sqdmulh, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);

    // sqdmull scalar
    theEmitter->emitIns_R_R_R(INS_sqdmull, EA_2BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqdmull, EA_4BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_NONE);

    // sqdmull vector
    theEmitter->emitIns_R_R_R(INS_sqdmull, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sqdmull, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_2S);

    // sqdmull2 vector
    theEmitter->emitIns_R_R_R(INS_sqdmull2, EA_16BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sqdmull2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);

    // sqrdmlah scalar
    theEmitter->emitIns_R_R_R(INS_sqrdmlah, EA_2BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqrdmlah, EA_4BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_NONE);

    // sqdrmlah vector
    theEmitter->emitIns_R_R_R(INS_sqrdmlah, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sqrdmlah, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_sqrdmlah, EA_16BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sqrdmlah, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);

    // sqrdmlsh scalar
    theEmitter->emitIns_R_R_R(INS_sqrdmlsh, EA_2BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqrdmlsh, EA_4BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_NONE);

    // sqdrmlsh vector
    theEmitter->emitIns_R_R_R(INS_sqrdmlsh, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sqrdmlsh, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_sqrdmlsh, EA_16BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sqrdmlsh, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);

    // sqrdmulh scalar
    theEmitter->emitIns_R_R_R(INS_sqrdmulh, EA_2BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_sqrdmulh, EA_4BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_NONE);

    // sqdrmulh vector
    theEmitter->emitIns_R_R_R(INS_sqrdmulh, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_sqrdmulh, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_sqrdmulh, EA_16BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_sqrdmulh, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);

    // subhn2 vector
    theEmitter->emitIns_R_R_R(INS_subhn2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_subhn2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_subhn2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // uabal vector
    theEmitter->emitIns_R_R_R(INS_uabal, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_uabal, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_uabal, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // uabal2 vector
    theEmitter->emitIns_R_R_R(INS_uabal2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_uabal2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_uabal2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // uabdl vector
    theEmitter->emitIns_R_R_R(INS_uabdl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_uabdl, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_uabdl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // uabdl2 vector
    theEmitter->emitIns_R_R_R(INS_uabdl2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_uabdl2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_uabdl2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // uaddl vector
    theEmitter->emitIns_R_R_R(INS_uaddl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_uaddl, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_uaddl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // uaddl2 vector
    theEmitter->emitIns_R_R_R(INS_uaddl2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_uaddl2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_uaddl2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // uaddw vector
    theEmitter->emitIns_R_R_R(INS_uaddw, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_uaddw, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_uaddw, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // uaddw2 vector
    theEmitter->emitIns_R_R_R(INS_uaddw2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_uaddw2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_uaddw2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // uhadd vector
    theEmitter->emitIns_R_R_R(INS_uhadd, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_uhadd, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_uhadd, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_uhadd, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_uhadd, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_uhadd, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // uhsub vector
    theEmitter->emitIns_R_R_R(INS_uhsub, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_uhsub, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_uhsub, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_uhsub, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_uhsub, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_uhsub, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // uqadd scalar
    theEmitter->emitIns_R_R_R(INS_uqadd, EA_1BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_uqadd, EA_2BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_uqadd, EA_4BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_uqadd, EA_8BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_NONE);

    // uqadd vector
    theEmitter->emitIns_R_R_R(INS_uqadd, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_uqadd, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_uqadd, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_uqadd, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_uqadd, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_uqadd, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // uqrshl scalar
    theEmitter->emitIns_R_R_R(INS_uqrshl, EA_1BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_uqrshl, EA_2BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_uqrshl, EA_4BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_uqrshl, EA_8BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_NONE);

    // uqrshl vector
    theEmitter->emitIns_R_R_R(INS_uqrshl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_uqrshl, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_uqrshl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_uqrshl, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_uqrshl, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_uqrshl, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_uqrshl, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // uqshl scalar
    theEmitter->emitIns_R_R_R(INS_uqshl, EA_1BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_uqshl, EA_2BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_uqshl, EA_4BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_uqshl, EA_8BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_NONE);

    // uqshl vector
    theEmitter->emitIns_R_R_R(INS_uqshl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_uqshl, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_uqshl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_uqshl, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_uqshl, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_uqshl, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_uqshl, EA_16BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_2D);

    // uqsub scalar
    theEmitter->emitIns_R_R_R(INS_uqsub, EA_1BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_uqsub, EA_2BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_uqsub, EA_4BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R(INS_uqsub, EA_8BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_NONE);

    // uqsub vector
    theEmitter->emitIns_R_R_R(INS_uqsub, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_uqsub, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_uqsub, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_uqsub, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_uqsub, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_uqsub, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // urhadd vector
    theEmitter->emitIns_R_R_R(INS_urhadd, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_urhadd, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_urhadd, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_urhadd, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_urhadd, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_urhadd, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // usubl vector
    theEmitter->emitIns_R_R_R(INS_usubl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_usubl, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_usubl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // usubl2 vector
    theEmitter->emitIns_R_R_R(INS_usubl2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_usubl2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_usubl2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // usubw vector
    theEmitter->emitIns_R_R_R(INS_usubw, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_usubw, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_usubw, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // usubw2 vector
    theEmitter->emitIns_R_R_R(INS_usubw2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_usubw2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_usubw2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);
#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R_R  vector multiply
    //

    genDefineTempLabel(genCreateTempLabel());

    theEmitter->emitIns_R_R_R(INS_mul, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_mul, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_mul, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_mul, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_mul, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_mul, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    theEmitter->emitIns_R_R_R(INS_pmul, EA_8BYTE, REG_V18, REG_V19, REG_V20, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_pmul, EA_16BYTE, REG_V21, REG_V22, REG_V23, INS_OPTS_16B);

    // 'mul' vector by element
    theEmitter->emitIns_R_R_R_I(INS_mul, EA_8BYTE, REG_V0, REG_V1, REG_V16, 0, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_mul, EA_8BYTE, REG_V2, REG_V3, REG_V15, 1, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_mul, EA_8BYTE, REG_V4, REG_V5, REG_V17, 3, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_mul, EA_8BYTE, REG_V6, REG_V7, REG_V0, 0, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_mul, EA_8BYTE, REG_V8, REG_V9, REG_V1, 3, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_mul, EA_8BYTE, REG_V10, REG_V11, REG_V2, 7, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_mul, EA_16BYTE, REG_V12, REG_V13, REG_V14, 0, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R_I(INS_mul, EA_16BYTE, REG_V14, REG_V15, REG_V18, 1, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R_I(INS_mul, EA_16BYTE, REG_V16, REG_V17, REG_V13, 3, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R_I(INS_mul, EA_16BYTE, REG_V18, REG_V19, REG_V3, 0, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_mul, EA_16BYTE, REG_V20, REG_V21, REG_V4, 3, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_mul, EA_16BYTE, REG_V22, REG_V23, REG_V5, 7, INS_OPTS_8H);

    // 'mla' vector by element
    theEmitter->emitIns_R_R_R_I(INS_mla, EA_8BYTE, REG_V0, REG_V1, REG_V16, 0, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_mla, EA_8BYTE, REG_V2, REG_V3, REG_V15, 1, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_mla, EA_8BYTE, REG_V4, REG_V5, REG_V17, 3, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_mla, EA_8BYTE, REG_V6, REG_V7, REG_V0, 0, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_mla, EA_8BYTE, REG_V8, REG_V9, REG_V1, 3, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_mla, EA_8BYTE, REG_V10, REG_V11, REG_V2, 7, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_mla, EA_16BYTE, REG_V12, REG_V13, REG_V14, 0, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R_I(INS_mla, EA_16BYTE, REG_V14, REG_V15, REG_V18, 1, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R_I(INS_mla, EA_16BYTE, REG_V16, REG_V17, REG_V13, 3, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R_I(INS_mla, EA_16BYTE, REG_V18, REG_V19, REG_V3, 0, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_mla, EA_16BYTE, REG_V20, REG_V21, REG_V4, 3, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_mla, EA_16BYTE, REG_V22, REG_V23, REG_V5, 7, INS_OPTS_8H);

    // 'mls' vector by element
    theEmitter->emitIns_R_R_R_I(INS_mls, EA_8BYTE, REG_V0, REG_V1, REG_V16, 0, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_mls, EA_8BYTE, REG_V2, REG_V3, REG_V15, 1, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_mls, EA_8BYTE, REG_V4, REG_V5, REG_V17, 3, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_mls, EA_8BYTE, REG_V6, REG_V7, REG_V0, 0, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_mls, EA_8BYTE, REG_V8, REG_V9, REG_V1, 3, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_mls, EA_8BYTE, REG_V10, REG_V11, REG_V2, 7, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_mls, EA_16BYTE, REG_V12, REG_V13, REG_V14, 0, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R_I(INS_mls, EA_16BYTE, REG_V14, REG_V15, REG_V18, 1, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R_I(INS_mls, EA_16BYTE, REG_V16, REG_V17, REG_V13, 3, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R_I(INS_mls, EA_16BYTE, REG_V18, REG_V19, REG_V3, 0, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_mls, EA_16BYTE, REG_V20, REG_V21, REG_V4, 3, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_mls, EA_16BYTE, REG_V22, REG_V23, REG_V5, 7, INS_OPTS_8H);
#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    // pmull vector
    theEmitter->emitIns_R_R_R(INS_pmull, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_pmull, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_1D);

    // pmull2 vector
    theEmitter->emitIns_R_R_R(INS_pmull2, EA_16BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_pmull2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_2D);

    // sdot vector
    theEmitter->emitIns_R_R_R_I(INS_sdot, EA_8BYTE, REG_V0, REG_V1, REG_V16, 3, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_sdot, EA_16BYTE, REG_V3, REG_V4, REG_V31, 1, INS_OPTS_4S);

    // smlal vector
    theEmitter->emitIns_R_R_R(INS_smlal, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_smlal, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_smlal, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // smlal2 vector
    theEmitter->emitIns_R_R_R(INS_smlal2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_smlal2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_smlal2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // smlsl vector
    theEmitter->emitIns_R_R_R(INS_smlsl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_smlsl, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_smlsl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // smlsl2 vector
    theEmitter->emitIns_R_R_R(INS_smlsl2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_smlsl2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_smlsl2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // smull vector
    theEmitter->emitIns_R_R_R(INS_smull, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_smull, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_smull, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // smull2 vector
    theEmitter->emitIns_R_R_R(INS_smull2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_smull2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_smull2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // udot vector
    theEmitter->emitIns_R_R_R_I(INS_udot, EA_8BYTE, REG_V0, REG_V1, REG_V16, 3, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_udot, EA_16BYTE, REG_V3, REG_V4, REG_V31, 1, INS_OPTS_4S);

    // umlal vector
    theEmitter->emitIns_R_R_R(INS_umlal, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_umlal, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_umlal, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // umlal2 vector
    theEmitter->emitIns_R_R_R(INS_umlal2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_umlal2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_umlal2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // umlsl vector
    theEmitter->emitIns_R_R_R(INS_umlsl, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_umlsl, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_umlsl, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // umlsl2 vector
    theEmitter->emitIns_R_R_R(INS_umlsl2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_umlsl2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_umlsl2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // umull vector
    theEmitter->emitIns_R_R_R(INS_umull, EA_8BYTE, REG_V0, REG_V1, REG_V2, INS_OPTS_8B);
    theEmitter->emitIns_R_R_R(INS_umull, EA_8BYTE, REG_V3, REG_V4, REG_V5, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R(INS_umull, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);

    // umull2 vector
    theEmitter->emitIns_R_R_R(INS_umull2, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_16B);
    theEmitter->emitIns_R_R_R(INS_umull2, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R(INS_umull2, EA_16BYTE, REG_V15, REG_V16, REG_V17, INS_OPTS_4S);

    // smlal vector, by element
    theEmitter->emitIns_R_R_R_I(INS_smlal, EA_8BYTE, REG_V0, REG_V1, REG_V2, 3, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_smlal, EA_8BYTE, REG_V3, REG_V4, REG_V5, 1, INS_OPTS_2S);

    // smlal2 vector, by element
    theEmitter->emitIns_R_R_R_I(INS_smlal2, EA_16BYTE, REG_V6, REG_V7, REG_V8, 7, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_smlal2, EA_16BYTE, REG_V9, REG_V10, REG_V11, 3, INS_OPTS_4S);

    // smlsl vector, by element
    theEmitter->emitIns_R_R_R_I(INS_smlsl, EA_8BYTE, REG_V0, REG_V1, REG_V2, 3, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_smlsl, EA_8BYTE, REG_V3, REG_V4, REG_V5, 1, INS_OPTS_2S);

    // smlsl2 vector, by element
    theEmitter->emitIns_R_R_R_I(INS_smlsl2, EA_16BYTE, REG_V6, REG_V7, REG_V8, 7, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_smlsl2, EA_16BYTE, REG_V9, REG_V10, REG_V11, 3, INS_OPTS_4S);

    // smull vector, by element
    theEmitter->emitIns_R_R_R_I(INS_smull, EA_8BYTE, REG_V0, REG_V1, REG_V2, 3, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_smull, EA_8BYTE, REG_V3, REG_V4, REG_V5, 1, INS_OPTS_2S);

    // smull2 vector, by element
    theEmitter->emitIns_R_R_R_I(INS_smull2, EA_16BYTE, REG_V6, REG_V7, REG_V8, 7, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_smull2, EA_16BYTE, REG_V9, REG_V10, REG_V11, 3, INS_OPTS_4S);

    // sqdmlal scalar, by element
    theEmitter->emitIns_R_R_R_I(INS_sqdmlal, EA_2BYTE, REG_V0, REG_V1, REG_V2, 7, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R_I(INS_sqdmlal, EA_4BYTE, REG_V3, REG_V4, REG_V5, 3, INS_OPTS_NONE);

    // sqdmlal vector, by element
    theEmitter->emitIns_R_R_R_I(INS_sqdmlal, EA_8BYTE, REG_V0, REG_V1, REG_V2, 7, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_sqdmlal, EA_8BYTE, REG_V3, REG_V4, REG_V5, 3, INS_OPTS_2S);

    // sqdmlal2 vector, by element
    theEmitter->emitIns_R_R_R_I(INS_sqdmlal2, EA_16BYTE, REG_V6, REG_V7, REG_V8, 7, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_sqdmlal2, EA_16BYTE, REG_V9, REG_V10, REG_V11, 3, INS_OPTS_4S);

    // sqdmlsl scalar, by element
    theEmitter->emitIns_R_R_R_I(INS_sqdmlsl, EA_2BYTE, REG_V0, REG_V1, REG_V2, 7, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R_I(INS_sqdmlsl, EA_4BYTE, REG_V3, REG_V4, REG_V5, 3, INS_OPTS_NONE);

    // sqdmlsl vector, by element
    theEmitter->emitIns_R_R_R_I(INS_sqdmlsl, EA_8BYTE, REG_V0, REG_V1, REG_V2, 7, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_sqdmlsl, EA_8BYTE, REG_V3, REG_V4, REG_V5, 3, INS_OPTS_2S);

    // sqdmlsl2 vector, by element
    theEmitter->emitIns_R_R_R_I(INS_sqdmlsl2, EA_16BYTE, REG_V6, REG_V7, REG_V8, 7, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_sqdmlsl2, EA_16BYTE, REG_V9, REG_V10, REG_V11, 3, INS_OPTS_4S);

    // sqdmulh scalar
    theEmitter->emitIns_R_R_R_I(INS_sqdmulh, EA_2BYTE, REG_V0, REG_V1, REG_V2, 7, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R_I(INS_sqdmulh, EA_4BYTE, REG_V3, REG_V4, REG_V5, 3, INS_OPTS_NONE);

    // sqdmulh vector
    theEmitter->emitIns_R_R_R_I(INS_sqdmulh, EA_8BYTE, REG_V0, REG_V1, REG_V2, 7, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_sqdmulh, EA_8BYTE, REG_V3, REG_V4, REG_V5, 3, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_sqdmulh, EA_16BYTE, REG_V6, REG_V7, REG_V8, 7, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_sqdmulh, EA_16BYTE, REG_V9, REG_V10, REG_V11, 3, INS_OPTS_4S);

    // sqdmull scalar, by element
    theEmitter->emitIns_R_R_R_I(INS_sqdmull, EA_2BYTE, REG_V0, REG_V1, REG_V2, 7, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R_I(INS_sqdmull, EA_4BYTE, REG_V3, REG_V4, REG_V5, 3, INS_OPTS_NONE);

    // sqdmull vector, by element
    theEmitter->emitIns_R_R_R_I(INS_sqdmull, EA_8BYTE, REG_V0, REG_V1, REG_V2, 7, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_sqdmull, EA_8BYTE, REG_V3, REG_V4, REG_V5, 3, INS_OPTS_2S);

    // sqdmull2 vector, by element
    theEmitter->emitIns_R_R_R_I(INS_sqdmull2, EA_16BYTE, REG_V6, REG_V7, REG_V8, 7, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_sqdmull2, EA_16BYTE, REG_V9, REG_V10, REG_V11, 3, INS_OPTS_4S);

    // sqrdmlah scalar
    theEmitter->emitIns_R_R_R_I(INS_sqrdmlah, EA_2BYTE, REG_V0, REG_V1, REG_V2, 7, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R_I(INS_sqrdmlah, EA_4BYTE, REG_V3, REG_V4, REG_V5, 3, INS_OPTS_NONE);

    // sqdrmlah vector
    theEmitter->emitIns_R_R_R_I(INS_sqrdmlah, EA_8BYTE, REG_V0, REG_V1, REG_V2, 7, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_sqrdmlah, EA_8BYTE, REG_V3, REG_V4, REG_V5, 3, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_sqrdmlah, EA_16BYTE, REG_V6, REG_V7, REG_V8, 7, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_sqrdmlah, EA_16BYTE, REG_V9, REG_V10, REG_V11, 3, INS_OPTS_4S);

    // sqrdmlsh scalar
    theEmitter->emitIns_R_R_R_I(INS_sqrdmlsh, EA_2BYTE, REG_V0, REG_V1, REG_V2, 7, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R_I(INS_sqrdmlsh, EA_4BYTE, REG_V3, REG_V4, REG_V5, 3, INS_OPTS_NONE);

    // sqdrmlsh vector
    theEmitter->emitIns_R_R_R_I(INS_sqrdmlsh, EA_8BYTE, REG_V0, REG_V1, REG_V2, 7, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_sqrdmlsh, EA_8BYTE, REG_V3, REG_V4, REG_V5, 3, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_sqrdmlsh, EA_16BYTE, REG_V6, REG_V7, REG_V8, 7, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_sqrdmlsh, EA_16BYTE, REG_V9, REG_V10, REG_V11, 3, INS_OPTS_4S);

    // sqrdmulh scalar
    theEmitter->emitIns_R_R_R_I(INS_sqrdmulh, EA_2BYTE, REG_V0, REG_V1, REG_V2, 7, INS_OPTS_NONE);
    theEmitter->emitIns_R_R_R_I(INS_sqrdmulh, EA_4BYTE, REG_V3, REG_V4, REG_V5, 3, INS_OPTS_NONE);

    // sqdrmulh vector
    theEmitter->emitIns_R_R_R_I(INS_sqrdmulh, EA_8BYTE, REG_V0, REG_V1, REG_V2, 7, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_sqrdmulh, EA_8BYTE, REG_V3, REG_V4, REG_V5, 3, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_sqrdmulh, EA_16BYTE, REG_V6, REG_V7, REG_V8, 7, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_sqrdmulh, EA_16BYTE, REG_V9, REG_V10, REG_V11, 3, INS_OPTS_4S);

    // umlal vector, by element
    theEmitter->emitIns_R_R_R_I(INS_umlal, EA_8BYTE, REG_V0, REG_V1, REG_V2, 3, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_umlal, EA_8BYTE, REG_V3, REG_V4, REG_V5, 1, INS_OPTS_2S);

    // umlal2 vector, by element
    theEmitter->emitIns_R_R_R_I(INS_umlal2, EA_16BYTE, REG_V6, REG_V7, REG_V8, 7, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_umlal2, EA_16BYTE, REG_V9, REG_V10, REG_V11, 3, INS_OPTS_4S);

    // umlsl vector, by element
    theEmitter->emitIns_R_R_R_I(INS_umlsl, EA_8BYTE, REG_V0, REG_V1, REG_V2, 3, INS_OPTS_4H);

    // umlsl2 vector, by element
    theEmitter->emitIns_R_R_R_I(INS_umlsl2, EA_16BYTE, REG_V6, REG_V7, REG_V8, 7, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_umlsl2, EA_16BYTE, REG_V9, REG_V10, REG_V11, 3, INS_OPTS_4S);

    // umull vector, by element
    theEmitter->emitIns_R_R_R_I(INS_umull, EA_8BYTE, REG_V0, REG_V1, REG_V2, 3, INS_OPTS_4H);
    theEmitter->emitIns_R_R_R_I(INS_umull, EA_8BYTE, REG_V3, REG_V4, REG_V5, 1, INS_OPTS_2S);

    // umull2 vector, by element
    theEmitter->emitIns_R_R_R_I(INS_umull2, EA_16BYTE, REG_V6, REG_V7, REG_V8, 7, INS_OPTS_8H);
    theEmitter->emitIns_R_R_R_I(INS_umull2, EA_16BYTE, REG_V9, REG_V10, REG_V11, 3, INS_OPTS_4S);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R_R   floating point operations, one source/dest, and two source
    //

    genDefineTempLabel(genCreateTempLabel());

    theEmitter->emitIns_R_R_R(INS_fmla, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fmla, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fmla, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2D);

    theEmitter->emitIns_R_R_R_I(INS_fmla, EA_4BYTE, REG_V15, REG_V16, REG_V17, 3); // scalar by element 4BYTE
    theEmitter->emitIns_R_R_R_I(INS_fmla, EA_8BYTE, REG_V18, REG_V19, REG_V20, 1); // scalar by element 8BYTE
    theEmitter->emitIns_R_R_R_I(INS_fmla, EA_8BYTE, REG_V21, REG_V22, REG_V23, 0, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_fmla, EA_16BYTE, REG_V24, REG_V25, REG_V26, 2, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R_I(INS_fmla, EA_16BYTE, REG_V27, REG_V28, REG_V29, 0, INS_OPTS_2D);

    theEmitter->emitIns_R_R_R(INS_fmls, EA_8BYTE, REG_V6, REG_V7, REG_V8, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R(INS_fmls, EA_16BYTE, REG_V9, REG_V10, REG_V11, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R(INS_fmls, EA_16BYTE, REG_V12, REG_V13, REG_V14, INS_OPTS_2D);

    theEmitter->emitIns_R_R_R_I(INS_fmls, EA_4BYTE, REG_V15, REG_V16, REG_V17, 3); // scalar by element 4BYTE
    theEmitter->emitIns_R_R_R_I(INS_fmls, EA_8BYTE, REG_V18, REG_V19, REG_V20, 1); // scalar by element 8BYTE
    theEmitter->emitIns_R_R_R_I(INS_fmls, EA_8BYTE, REG_V21, REG_V22, REG_V23, 0, INS_OPTS_2S);
    theEmitter->emitIns_R_R_R_I(INS_fmls, EA_16BYTE, REG_V24, REG_V25, REG_V26, 2, INS_OPTS_4S);
    theEmitter->emitIns_R_R_R_I(INS_fmls, EA_16BYTE, REG_V27, REG_V28, REG_V29, 0, INS_OPTS_2D);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    //
    // R_R_R_R   floating point operations, one dest, and three source
    //

    theEmitter->emitIns_R_R_R_R(INS_fmadd, EA_4BYTE, REG_V0, REG_V8, REG_V16, REG_V24);
    theEmitter->emitIns_R_R_R_R(INS_fmsub, EA_4BYTE, REG_V1, REG_V9, REG_V17, REG_V25);
    theEmitter->emitIns_R_R_R_R(INS_fnmadd, EA_4BYTE, REG_V2, REG_V10, REG_V18, REG_V26);
    theEmitter->emitIns_R_R_R_R(INS_fnmsub, EA_4BYTE, REG_V3, REG_V11, REG_V19, REG_V27);

    theEmitter->emitIns_R_R_R_R(INS_fmadd, EA_8BYTE, REG_V4, REG_V12, REG_V20, REG_V28);
    theEmitter->emitIns_R_R_R_R(INS_fmsub, EA_8BYTE, REG_V5, REG_V13, REG_V21, REG_V29);
    theEmitter->emitIns_R_R_R_R(INS_fnmadd, EA_8BYTE, REG_V6, REG_V14, REG_V22, REG_V30);
    theEmitter->emitIns_R_R_R_R(INS_fnmsub, EA_8BYTE, REG_V7, REG_V15, REG_V23, REG_V31);

#endif

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS

    BasicBlock* label = genCreateTempLabel();
    genDefineTempLabel(label);
    instGen(INS_nop);
    instGen(INS_nop);
    instGen(INS_nop);
    instGen(INS_nop);
    theEmitter->emitIns_R_L(INS_adr, EA_4BYTE_DSP_RELOC, label, REG_R0);

#endif // ALL_ARM64_EMITTER_UNIT_TESTS

#ifdef ALL_ARM64_EMITTER_UNIT_TESTS
    printf("*************** End of genArm64EmitterUnitTests()\n");
#endif // ALL_ARM64_EMITTER_UNIT_TESTS
}
#endif // DEBUG

// Probe the stack.
//
// This only does the probing; allocating the frame is done when callee-saved registers are saved.
// This is done before anything has been pushed. The previous frame might have a large outgoing argument
// space that has been allocated, but the lowest addresses have not been touched. Our frame setup might
// not touch up to the first 504 bytes. This means we could miss a guard page. On Windows, however,
// there are always three guard pages, so we will not miss them all. On Linux, there is only one guard
// page by default, so we need to be more careful. We do an extra probe if we might not have probed
// recently enough. That is, if a call and prolog establishment might lead to missing a page. We do this
// on Windows as well just to be consistent, even though it should not be necessary.
//
// frameSize         - the size of the stack frame being allocated.
// initReg           - register to use as a scratch register.
// pInitRegZeroed    - OUT parameter. *pInitRegZeroed is set to 'false' if and only if
//                     this call sets 'initReg' to a non-zero value. Otherwise, it is unchanged.
// maskArgRegsLiveIn - incoming argument registers that are currently live.
//
void CodeGen::PrologAllocLclFrame(unsigned  frameSize,
                                  regNumber initReg,
                                  bool*     pInitRegZeroed,
                                  regMaskTP maskArgRegsLiveIn)
{
    assert(generatingProlog);

    if (frameSize == 0)
    {
        return;
    }

    const target_size_t pageSize = compiler->eeGetPageSize();

    // What offset from the final SP was the last probe? If we haven't probed almost a complete page, and
    // if the next action on the stack might subtract from SP first, before touching the current SP, then
    // we do one more probe at the very bottom. This can happen if we call a function on arm64 that does
    // a "STP fp, lr, [sp-504]!", that is, pre-decrement SP then store. Note that we probe here for arm64,
    // but we don't alter SP.
    target_size_t lastTouchDelta = 0;

    assert(!compiler->info.compPublishStubParam || (REG_SECRET_STUB_PARAM != initReg));

    if (frameSize < pageSize)
    {
        lastTouchDelta = frameSize;
    }
    else if (frameSize < 3 * pageSize)
    {
        // The probing loop in "else"-case below would require at least 6 instructions (and more if
        // 'frameSize' or 'pageSize' can not be encoded with mov-instruction immediate).
        // Hence for frames that are smaller than 3 * PAGE_SIZE the JIT inlines the following probing code
        // to decrease code size.
        // TODO-ARM64: The probing mechanisms should be replaced by a call to stack probe helper
        // as it is done on other platforms.

        lastTouchDelta = frameSize;

        for (target_size_t probeOffset = pageSize; probeOffset <= frameSize; probeOffset += pageSize)
        {
            // Generate:
            //    movw initReg, -probeOffset
            //    ldr wzr, [sp + initReg]

            instGen_Set_Reg_To_Imm(EA_PTRSIZE, initReg, -(ssize_t)probeOffset);
            GetEmitter()->emitIns_R_R_R(INS_ldr, EA_4BYTE, REG_ZR, REG_SPBASE, initReg);
            *pInitRegZeroed = false; // The initReg does not contain zero

            lastTouchDelta -= pageSize;
        }

        assert(lastTouchDelta == frameSize % pageSize);
        compiler->unwindPadding();
    }
    else
    {
        // Emit the following sequence to 'tickle' the pages. Note it is important that stack pointer not change
        // until this is complete since the tickles could cause a stack overflow, and we need to be able to crawl
        // the stack afterward (which means the stack pointer needs to be known).

        regMaskTP availMask = RBM_INT_CALLEE_TRASH;
        availMask &= ~maskArgRegsLiveIn;   // Remove all of the incoming argument registers as they are currently live
        availMask &= ~genRegMask(initReg); // Remove the pre-calculated initReg

        regNumber rOffset = initReg;
        regNumber rLimit;
        regMaskTP tempMask;

        // We pick the next lowest register number for rLimit
        noway_assert(availMask != RBM_NONE);
        tempMask = genFindLowestBit(availMask);
        rLimit   = genRegNumFromMask(tempMask);

        // Generate:
        //
        //      mov rOffset, -pageSize    // On arm, this turns out to be "movw r1, 0xf000; sxth r1, r1".
        //                                // We could save 4 bytes in the prolog by using "movs r1, 0" at the
        //                                // runtime expense of running a useless first loop iteration.
        //      mov rLimit, -frameSize
        // loop:
        //      ldr wzr, [sp + rOffset]
        //      sub rOffset, pageSize
        //      cmp rLimit, rOffset
        //      b.ls loop                 // If rLimit is lower or same, we need to probe this rOffset. Note
        //                                // especially that if it is the same, we haven't probed this page.

        noway_assert((ssize_t)(int)frameSize == (ssize_t)frameSize); // make sure framesize safely fits within an int

        instGen_Set_Reg_To_Imm(EA_PTRSIZE, rOffset, -(ssize_t)pageSize);
        instGen_Set_Reg_To_Imm(EA_PTRSIZE, rLimit, -(ssize_t)frameSize);

        // There's a "virtual" label here. But we can't create a label in the prolog, so we use the magic
        // `emitIns_J` with a negative `instrCount` to branch back a specific number of instructions.

        GetEmitter()->emitIns_R_R_R(INS_ldr, EA_4BYTE, REG_ZR, REG_SPBASE, rOffset);
        GetEmitter()->emitIns_R_R_I(INS_sub, EA_PTRSIZE, rOffset, rOffset, pageSize);
        GetEmitter()->emitIns_R_R(INS_cmp, EA_PTRSIZE, rLimit, rOffset); // If equal, we need to probe again
        GetEmitter()->emitIns_J(INS_bls, NULL, -4);

        *pInitRegZeroed = false; // The initReg does not contain zero

        compiler->unwindPadding();

        lastTouchDelta = frameSize % pageSize;
    }

    if (lastTouchDelta + STACK_PROBE_BOUNDARY_THRESHOLD_BYTES > pageSize)
    {
        assert(lastTouchDelta + STACK_PROBE_BOUNDARY_THRESHOLD_BYTES < 2 * pageSize);
        instGen_Set_Reg_To_Imm(EA_PTRSIZE, initReg, -(ssize_t)frameSize);
        GetEmitter()->emitIns_R_R_R(INS_ldr, EA_4BYTE, REG_ZR, REG_SPBASE, initReg);
        compiler->unwindPadding();

        *pInitRegZeroed = false; // The initReg does not contain zero
    }
}

void CodeGen::PrologEstablishFramePointer(int delta, bool reportUnwindData)
{
    if (delta == 0)
    {
        GetEmitter()->emitIns_Mov(INS_mov, EA_8BYTE, REG_FP, REG_SP, /* canSkip */ false);
    }
    else
    {
        GetEmitter()->emitIns_R_R_I(INS_add, EA_8BYTE, REG_FP, REG_SP, delta);
    }

    if (reportUnwindData)
    {
        compiler->unwindSetFrameReg(REG_FP, delta);
    }
}

void CodeGen::genCodeForInstr(GenTreeInstr* instr)
{
    instruction ins  = instr->GetIns();
    emitAttr    attr = instr->GetSize();
    insOpts     opt  = instr->GetOption();
    unsigned    imm  = instr->GetImmediate();

    assert(!varTypeIsGC(instr->GetType()) || (emitActualTypeSize(instr->GetType()) == attr));

    regNumber dstReg = instr->TypeIs(TYP_VOID) ? REG_NA : instr->GetRegNum();

    // TODO-MIKE-Cleanup: It would be better to add some kind of "format" to GenTreeInstr
    // in order to be able to simplify all this to a simple switch statement.

    if (instr->GetNumOps() == 1)
    {
        regNumber srcReg1 = genConsumeReg(instr->GetOp(0));

        switch (ins)
        {
            case INS_mul:
                // Special case - INS_mul with a single operand is treated as ADD ..., x1, x1, LSL #imm
                GetEmitter()->emitIns_R_R_R_I(INS_add, attr, dstReg, srcReg1, srcReg1, imm, INS_OPTS_LSL);
                break;

            case INS_add:
            case INS_sub:
            case INS_asr:
            case INS_lsr:
            case INS_lsl:
            case INS_ror:
                GetEmitter()->emitIns_R_R_I(ins, attr, dstReg, srcReg1, imm);
                break;

            case INS_cmp:
            case INS_cmn:
                GetEmitter()->emitIns_R_I(ins, attr, srcReg1, imm);
                break;

            case INS_and:
            case INS_orr:
            case INS_eor:
                GetEmitter()->emitIns_R_R_I(ins, attr, dstReg, srcReg1, DecodeBitmaskImm(imm, attr));
                break;

            case INS_tst:
                GetEmitter()->emitIns_R_I(ins, attr, srcReg1, DecodeBitmaskImm(imm, attr));
                break;

            case INS_mvn:
            case INS_neg:
                if (opt != INS_OPTS_NONE)
                {
                    GetEmitter()->emitIns_R_R_I(ins, attr, dstReg, srcReg1, imm, opt);
                }
                else
                {
                    GetEmitter()->emitIns_R_R(ins, attr, dstReg, srcReg1);
                }
                break;

            case INS_sbfiz:
            case INS_ubfiz:
            case INS_sbfx:
            case INS_ubfx:
                GetEmitter()->emitIns_R_R_I_I(ins, attr, dstReg, srcReg1, imm >> 6, imm & 63);
                break;

            default:
                GetEmitter()->emitIns_R_R(ins, attr, dstReg, srcReg1);
                break;
        }
    }
    else if (instr->GetNumOps() == 2)
    {
        regNumber srcReg1 = genConsumeReg(instr->GetOp(0));
        regNumber srcReg2 = genConsumeReg(instr->GetOp(1));

        switch (ins)
        {
            case INS_add:
            case INS_sub:
            case INS_and:
            case INS_bic:
            case INS_orr:
            case INS_orn:
            case INS_eor:
            case INS_eon:
                if (opt != INS_OPTS_NONE)
                {
                    GetEmitter()->emitIns_R_R_R_I(ins, attr, dstReg, srcReg1, srcReg2, imm, opt);
                }
                else
                {
                    GetEmitter()->emitIns_R_R_R(ins, attr, dstReg, srcReg1, srcReg2);
                }
                break;

            case INS_cmp:
            case INS_cmn:
            case INS_tst:
                if (opt != INS_OPTS_NONE)
                {
                    GetEmitter()->emitIns_R_R_I(ins, attr, srcReg1, srcReg2, imm, opt);
                }
                else
                {
                    GetEmitter()->emitIns_R_R(ins, attr, srcReg1, srcReg2);
                }
                break;

            default:
                GetEmitter()->emitIns_R_R_R(ins, attr, dstReg, srcReg1, srcReg2);
                break;
        }
    }
    else
    {
        assert(instr->GetNumOps() == 3);

        regNumber srcReg1 = genConsumeReg(instr->GetOp(0));
        regNumber srcReg2 = genConsumeReg(instr->GetOp(1));
        regNumber srcReg3 = genConsumeReg(instr->GetOp(2));

        GetEmitter()->emitIns_R_R_R_R(ins, attr, dstReg, srcReg1, srcReg2, srcReg3);
    }

    if (!instr->TypeIs(TYP_VOID))
    {
        genProduceReg(instr);
    }
}

CodeGen::GenAddrMode::GenAddrMode(GenTree* tree, CodeGen* codeGen)
    : m_base(REG_NA), m_index(REG_NA), m_scale(1), m_disp(0), m_lclNum(BAD_VAR_NUM)
{
    if (GenTreeIndir* indir = tree->IsIndir())
    {
        GenTree* addr = indir->GetAddr();

        if (addr->isUsedFromReg())
        {
            m_base = codeGen->genConsumeReg(addr);
        }
        else if (GenTreeAddrMode* addrMode = addr->IsAddrMode())
        {
            if (GenTree* base = addrMode->GetBase())
            {
                m_base = codeGen->genConsumeReg(base);
            }

            // ARM does have indexed address modes but this code is used currently
            // only in cases where an immediate offset is also needed (e.g. SIMD12
            // load/store) so we can't really have an index.
            assert(!addrMode->HasIndex());

            m_disp = addrMode->GetOffset();
        }
    }
    else
    {
        m_lclNum = tree->AsLclVarCommon()->GetLclNum();

        if (tree->OperIs(GT_LCL_FLD, GT_STORE_LCL_FLD))
        {
            m_disp = tree->AsLclFld()->GetLclOffs();
        }
    }
}

void CodeGen::inst_R_AM(instruction ins, emitAttr attr, regNumber reg, const GenAddrMode& addrMode, unsigned offset)
{
    if (addrMode.IsLcl())
    {
        GetEmitter()->emitIns_R_S(ins, attr, reg, addrMode.LclNum(), addrMode.Disp(offset));
    }
    else
    {
        GetEmitter()->emitIns_R_R_I(ins, attr, reg, addrMode.Base(), addrMode.Disp(offset));
    }
}

void CodeGen::inst_AM_R(instruction ins, emitAttr attr, regNumber reg, const GenAddrMode& addrMode, unsigned offset)
{
    if (addrMode.IsLcl())
    {
        GetEmitter()->emitIns_S_R(ins, attr, reg, addrMode.LclNum(), addrMode.Disp(offset));
    }
    else
    {
        GetEmitter()->emitIns_R_R_I(ins, attr, reg, addrMode.Base(), addrMode.Disp(offset));
    }
}

void CodeGen::emitInsLoad(instruction ins, emitAttr attr, regNumber dataReg, GenTreeIndir* load)
{
    assert(load->OperIs(GT_IND, GT_NULLCHECK));

    emitInsIndir(ins, attr, dataReg, load);
}

void CodeGen::emitInsStore(instruction ins, emitAttr attr, regNumber dataReg, GenTreeStoreInd* store)
{
    assert(store->OperIs(GT_STOREIND));

    emitInsIndir(ins, attr, dataReg, store);
}

void CodeGen::emitInsIndir(instruction ins, emitAttr attr, regNumber valueReg, GenTreeIndir* indir)
{
    emitter* emit = GetEmitter();
    GenTree* addr = indir->GetAddr();

    if (!addr->isContained())
    {
        emit->emitIns_R_R(ins, attr, valueReg, addr->GetRegNum());

        return;
    }

    if (GenTreeClsVar* clsAddr = addr->IsClsVar())
    {
        regNumber tmpReg = indir->GetSingleTempReg();
        emit->emitIns_R_C(ins, attr, valueReg, tmpReg, addr->AsClsVar()->GetFieldHandle());

        return;
    }

    if (addr->OperIs(GT_LCL_ADDR))
    {
        GenTreeLclAddr* lclAddr = addr->AsLclAddr();
        unsigned        lclNum  = lclAddr->GetLclNum();
        unsigned        offset  = lclAddr->GetLclOffs();

        if (emitter::emitInsIsStore(ins))
        {
            emit->emitIns_S_R(ins, attr, valueReg, lclNum, offset);
        }
        else
        {
            emit->emitIns_R_S(ins, attr, valueReg, lclNum, offset);
        }

        return;
    }

    GenTreeAddrMode* addrMode = addr->AsAddrMode();
    GenTree*         base     = addrMode->GetBase();
    GenTree*         index    = addrMode->GetIndex();
    int              offset   = addrMode->GetOffset();

    if (index == nullptr)
    {
        if (emitter::emitIns_valid_imm_for_ldst_offset(offset, emitTypeSize(indir->GetType())))
        {
            emit->emitIns_R_R_I(ins, attr, valueReg, base->GetRegNum(), offset);
        }
        else
        {
            regNumber offsetReg = indir->GetSingleTempReg();
            instGen_Set_Reg_To_Imm(EA_8BYTE, offsetReg, offset);
            emit->emitIns_R_R_R(ins, attr, valueReg, base->GetRegNum(), offsetReg);
        }

        return;
    }

    assert(isPow2(addrMode->GetScale()));

    regNumber baseReg  = base->GetRegNum();
    regNumber indexReg = index->GetRegNum();
    unsigned  lsl      = genLog2(addrMode->GetScale());

    if (offset == 0)
    {
        if (lsl > 0)
        {
            emit->emitIns_R_R_R_I(ins, attr, valueReg, baseReg, indexReg, lsl, INS_OPTS_LSL);
        }
        else
        {
            emit->emitIns_R_R_R(ins, attr, valueReg, baseReg, indexReg);
        }

        return;
    }

    // TODO-MIKE-Cleanup: Remove all this idiocy.

    regNumber tmpReg  = indir->GetSingleTempReg();
    emitAttr  tmpAttr = varTypeIsGC(base->GetType()) ? EA_BYREF : EA_8BYTE;

    noway_assert(emitter::emitInsIsLoad(ins) || (tmpReg != valueReg));

    if (!emitter::emitIns_valid_imm_for_add(offset, EA_8BYTE))
    {
        noway_assert(tmpReg != indexReg);

        instGen_Set_Reg_To_Imm(EA_8BYTE, tmpReg, offset);
        emit->emitIns_R_R_R(INS_add, tmpAttr, tmpReg, tmpReg, baseReg);
        emit->emitIns_R_R_R_I(ins, attr, valueReg, tmpReg, indexReg, lsl, INS_OPTS_LSL);

        return;
    }

    if (lsl > 0)
    {
        emit->emitIns_R_R_R_I(INS_add, tmpAttr, tmpReg, baseReg, indexReg, lsl, INS_OPTS_LSL);
    }
    else
    {
        emit->emitIns_R_R_R(INS_add, tmpAttr, tmpReg, baseReg, indexReg);
    }

    emit->emitIns_R_R_I(ins, attr, valueReg, tmpReg, offset);
}

regNumber CodeGen::emitInsTernary(instruction ins, emitAttr attr, GenTree* dst, GenTree* src1, GenTree* src2)
{
    // dst can only be a reg
    assert(!dst->isContained());
    assert(varTypeIsIntegralOrI(dst->GetType()));

    // find immed (if any) - it cannot be a dst
    // Only one src can be an int.
    GenTreeIntConCommon* intConst  = nullptr;
    GenTree*             nonIntReg = nullptr;

    // src2 can be immed or reg
    assert(!src2->isContained() || src2->isContainedIntOrIImmed());

    // Check src2 first as we can always allow it to be a contained immediate
    if (src2->isContainedIntOrIImmed())
    {
        intConst  = src2->AsIntConCommon();
        nonIntReg = src1;
    }
    // Only for commutative operations do we check src1 and allow it to be a contained immediate
    else if (dst->OperIsCommutative())
    {
        // src1 can be immed or reg
        assert(!src1->isContained() || src1->isContainedIntOrIImmed());

        // Check src1 and allow it to be a contained immediate
        if (src1->isContainedIntOrIImmed())
        {
            assert(!src2->isContainedIntOrIImmed());
            intConst  = src1->AsIntConCommon();
            nonIntReg = src2;
        }
    }
    else
    {
        // src1 can only be a reg
        assert(!src1->isContained());
    }

    bool isMulOverflow = false;
    if (dst->gtOverflowEx())
    {
        if ((ins == INS_add) || (ins == INS_adds))
        {
            ins = INS_adds;
        }
        else if ((ins == INS_sub) || (ins == INS_subs))
        {
            ins = INS_subs;
        }
        else if (ins == INS_mul)
        {
            isMulOverflow = true;
            assert(intConst == nullptr); // overflow format doesn't support an int constant operand
        }
        else
        {
            assert(!"Invalid ins for overflow check");
        }
    }

    emitter* emit = GetEmitter();

    if (intConst != nullptr)
    {
        emit->emitIns_R_R_I(ins, attr, dst->GetRegNum(), nonIntReg->GetRegNum(), intConst->IconValue());
    }
    else
    {
        if (isMulOverflow)
        {
            regNumber extraReg = dst->GetSingleTempReg();
            assert(extraReg != dst->GetRegNum());

            if ((dst->gtFlags & GTF_UNSIGNED) != 0)
            {
                if (attr == EA_4BYTE)
                {
                    // Compute 8 byte results from 4 byte by 4 byte multiplication.
                    emit->emitIns_R_R_R(INS_umull, EA_8BYTE, dst->GetRegNum(), src1->GetRegNum(), src2->GetRegNum());

                    // Get the high result by shifting dst.
                    emit->emitIns_R_R_I(INS_lsr, EA_8BYTE, extraReg, dst->GetRegNum(), 32);
                }
                else
                {
                    assert(attr == EA_8BYTE);
                    // Compute the high result.
                    emit->emitIns_R_R_R(INS_umulh, attr, extraReg, src1->GetRegNum(), src2->GetRegNum());

                    // Now multiply without skewing the high result.
                    emit->emitIns_R_R_R(ins, attr, dst->GetRegNum(), src1->GetRegNum(), src2->GetRegNum());
                }

                // zero-sign bit comparison to detect overflow.
                emit->emitIns_R_I(INS_cmp, attr, extraReg, 0);
            }
            else
            {
                int bitShift = 0;
                if (attr == EA_4BYTE)
                {
                    // Compute 8 byte results from 4 byte by 4 byte multiplication.
                    emit->emitIns_R_R_R(INS_smull, EA_8BYTE, dst->GetRegNum(), src1->GetRegNum(), src2->GetRegNum());

                    // Get the high result by shifting dst.
                    emit->emitIns_R_R_I(INS_lsr, EA_8BYTE, extraReg, dst->GetRegNum(), 32);

                    bitShift = 31;
                }
                else
                {
                    assert(attr == EA_8BYTE);
                    // Save the high result in a temporary register.
                    emit->emitIns_R_R_R(INS_smulh, attr, extraReg, src1->GetRegNum(), src2->GetRegNum());

                    // Now multiply without skewing the high result.
                    emit->emitIns_R_R_R(ins, attr, dst->GetRegNum(), src1->GetRegNum(), src2->GetRegNum());

                    bitShift = 63;
                }

                // Sign bit comparison to detect overflow.
                emit->emitIns_R_R_I(INS_cmp, attr, extraReg, dst->GetRegNum(), bitShift, INS_OPTS_ASR);
            }
        }
        else
        {
            // We can just multiply.
            emit->emitIns_R_R_R(ins, attr, dst->GetRegNum(), src1->GetRegNum(), src2->GetRegNum());
        }
    }

    if (dst->gtOverflowEx())
    {
        genCheckOverflow(dst);
    }

    return dst->GetRegNum();
}

void CodeGen::inst_RV_IV(instruction ins, regNumber reg, target_ssize_t val, emitAttr size)
{
    assert(ins != INS_mov);
    assert(ins != INS_cmp);
    assert(ins != INS_tst);

    // TODO-Arm64-Bug: handle large constants!
    // Probably need something like the ARM case above: if (validImmForInstr(ins, val)) ...

    GetEmitter()->emitIns_R_R_I(ins, size, reg, reg, val);
}

void CodeGen::genCheckOverflow(GenTree* node)
{
    assert(node->gtOverflow());
    assert(!varTypeIsSmall(node->GetType()));

    emitJumpKind jumpKind;

    if (node->OperIs(GT_MUL))
    {
        jumpKind = EJ_ne;
    }
    else if (!node->IsUnsigned())
    {
        jumpKind = EJ_vs;
    }
    else if (node->OperIs(GT_SUB))
    {
        jumpKind = EJ_lo;
    }
    else
    {
        jumpKind = EJ_hs;
    }

    genJumpToThrowHlpBlk(jumpKind, ThrowHelperKind::Overflow);
}

void CodeGen::PrologPushCalleeSavedRegisters(regNumber initReg, bool* pInitRegZeroed)
{
    assert(generatingProlog);

    // Probe large frames now, if necessary, since PrologPushCalleeSavedRegisters() will allocate the frame. Note that
    // for arm64, PrologAllocLclFrame only probes the frame; it does not actually allocate it (it does not change SP).
    // For arm64, we are probing the frame before the callee-saved registers are saved. The 'initReg' might have
    // been calculated to be one of the callee-saved registers (say, if all the integer argument registers are
    // in use, and perhaps with other conditions being satisfied). This is ok in other cases, after the callee-saved
    // registers have been saved. So instead of letting PrologAllocLclFrame use initReg as a temporary register,
    // always use REG_SCRATCH. We don't care if it trashes it, so ignore the initRegZeroed output argument.
    bool ignoreInitRegZeroed = false;
    PrologAllocLclFrame(lclFrameSize, REG_SCRATCH, &ignoreInitRegZeroed, paramRegState.intRegLiveIn);

    regMaskTP rsPushRegs = calleeSavedModifiedRegs;

    // On ARM we push the FP (frame-pointer) here along with all other callee saved registers
    if (isFramePointerUsed())
    {
        rsPushRegs |= RBM_FPBASE;
    }

    //
    // It may be possible to skip pushing/popping lr for leaf methods. However, such optimization would require
    // changes in GC suspension architecture.
    //
    // We would need to guarantee that a tight loop calling a virtual leaf method can be suspended for GC. Today, we
    // generate partially interruptible code for both the method that contains the tight loop with the call and the
    // leaf
    // method. GC suspension depends on return address hijacking in this case. Return address hijacking depends
    // on the return address to be saved on the stack. If we skipped pushing/popping lr, the return address would
    // never
    // be saved on the stack and the GC suspension would time out.
    //
    // So if we wanted to skip pushing pushing/popping lr for leaf frames, we would also need to do one of
    // the following to make GC suspension work in the above scenario:
    // - Make return address hijacking work even when lr is not saved on the stack.
    // - Generate fully interruptible code for loops that contains calls
    // - Generate fully interruptible code for leaf methods
    //
    // Given the limited benefit from this optimization (<10k for CoreLib NGen image), the extra complexity
    // is not worth it.
    //
    rsPushRegs |= RBM_LR; // We must save the return address (in the LR register)

#ifdef DEBUG
    if (calleeRegsPushed != genCountBits(rsPushRegs))
    {
        printf("Error: unexpected number of callee-saved registers to push. Expected: %d. Got: %d ", calleeRegsPushed,
               genCountBits(rsPushRegs));
        dspRegMask(rsPushRegs);
        printf("\n");
        assert(calleeRegsPushed == genCountBits(rsPushRegs));
    }
#endif // DEBUG

    // See the document "ARM64 JIT Frame Layout" and/or "ARM64 Exception Data" for more details or requirements and
    // options. Case numbers in comments here refer to this document. See also Compiler::lvaAssignFrameOffsets()
    // for pictures of the general frame layouts, and CodeGen::genFuncletProlog() implementations (per architecture)
    // for pictures of the funclet frame layouts.
    //
    // For most frames, generate, e.g.:
    //      stp fp,  lr,  [sp,-0x80]!   // predecrement SP with full frame attr, and store FP/LR pair.
    //      stp r19, r20, [sp, 0x60]    // store at positive offset from SP established above, into callee-saved
    //      area
    //                                  // at top of frame (highest addresses).
    //      stp r21, r22, [sp, 0x70]
    //
    // Notes:
    // 1. We don't always need to save FP. If FP isn't saved, then LR is saved with the other callee-saved registers
    //    at the top of the frame.
    // 2. If we save FP, then the first store is FP, LR.
    // 3. General-purpose registers are 8 bytes, floating-point registers are 16 bytes, but FP/SIMD registers only
    //    preserve their lower 8 bytes, by calling convention.
    // 4. For frames with varargs, we spill the integer register arguments to the stack, so all the arguments are
    //    consecutive, and at the top of the frame.
    // 5. We allocate the frame here; no further changes to SP are allowed (except in the body, for localloc).
    //
    // For functions with GS and localloc, we change the frame so the frame pointer and LR are saved at the top
    // of the frame, just under the varargs registers (if any). Note that the funclet frames must follow the same
    // rule, and both main frame and funclet frames (if any) must put PSPSym in the same offset from Caller-SP.
    // Since this frame type is relatively rare, we force using it via stress modes, for additional coverage.
    //
    // The frames look like the following (simplified to only include components that matter for establishing the
    // frames). See also Compiler::lvaAssignFrameOffsets().
    //
    // Frames with FP, LR saved at bottom of frame (above outgoing argument space):
    //
    //      |                       |
    //      |-----------------------|
    //      |  incoming arguments   |
    //      +=======================+ <---- Caller's SP
    //      |  Varargs regs space   | // Only for varargs functions; 64 bytes
    //      |-----------------------|
    //      |Callee saved registers | // not including FP/LR; multiple of 8 bytes
    //      |-----------------------|
    //      |        PSP slot       | // 8 bytes (omitted in CoreRT ABI)
    //      |-----------------------|
    //      | locals, temps, etc.   |
    //      |-----------------------|
    //      |  possible GS cookie   |
    //      |-----------------------|
    //      |      Saved LR         | // 8 bytes
    //      |-----------------------|
    //      |      Saved FP         | // 8 bytes
    //      |-----------------------|
    //      |   Outgoing arg space  | // multiple of 8 bytes; if required (i.e., #outsz != 0)
    //      |-----------------------| <---- Ambient SP
    //      |       |               |
    //      ~       | Stack grows   ~
    //      |       | downward      |
    //              V
    //
    // Frames with FP, LR saved at top of frame (below saved varargs incoming arguments):
    //
    //      |                       |
    //      |-----------------------|
    //      |  incoming arguments   |
    //      +=======================+ <---- Caller's SP
    //      |  Varargs regs space   | // Only for varargs functions; 64 bytes
    //      |-----------------------|
    //      |      Saved LR         | // 8 bytes
    //      |-----------------------|
    //      |      Saved FP         | // 8 bytes
    //      |-----------------------|
    //      |Callee saved registers | // not including FP/LR; multiple of 8 bytes
    //      |-----------------------|
    //      |        PSP slot       | // 8 bytes (omitted in CoreRT ABI)
    //      |-----------------------|
    //      | locals, temps, etc.   |
    //      |-----------------------|
    //      |  possible GS cookie   |
    //      |-----------------------|
    //      |   Outgoing arg space  | // multiple of 8 bytes; if required (i.e., #outsz != 0)
    //      |-----------------------| <---- Ambient SP
    //      |       |               |
    //      ~       | Stack grows   ~
    //      |       | downward      |
    //              V
    //

    int totalFrameSize = genTotalFrameSize();

    int offset; // This will be the starting place for saving the callee-saved registers, in increasing order.

    regMaskTP maskSaveRegsFloat = rsPushRegs & RBM_ALLFLOAT;
    regMaskTP maskSaveRegsInt   = rsPushRegs & ~maskSaveRegsFloat;

#ifdef DEBUG
    if (verbose)
    {
        printf("Save float regs: ");
        dspRegMask(maskSaveRegsFloat);
        printf("\n");
        printf("Save int   regs: ");
        dspRegMask(maskSaveRegsInt);
        printf("\n");
    }
#endif // DEBUG

    // The frameType number is arbitrary, is defined below, and corresponds to one of the frame styles we
    // generate based on various sizes.
    int frameType = 0;

    // The amount to subtract from SP before starting to store the callee-saved registers. It might be folded into
    // the
    // first save instruction as a "predecrement" amount, if possible.
    int calleeSaveSPDelta = 0;

    if (isFramePointerUsed())
    {
        // We need to save both FP and LR.

        assert((maskSaveRegsInt & RBM_FP) != 0);
        assert((maskSaveRegsInt & RBM_LR) != 0);

        // If we need to generate a GS cookie, we need to make sure the saved frame pointer and return address
        // (FP and LR) are protected from buffer overrun by the GS cookie. If FP/LR are at the lowest addresses,
        // then they are safe, since they are lower than any unsafe buffers. And the GS cookie we add will
        // protect our caller's frame. If we have a localloc, however, that is dynamically placed lower than our
        // saved FP/LR. In that case, we save FP/LR along with the rest of the callee-saved registers, above
        // the GS cookie.
        //
        // After the frame is allocated, the frame pointer is established, pointing at the saved frame pointer to
        // create a frame pointer chain.
        //
        // Do we need another frame pointer register to get good code quality in the case of having the frame
        // pointer
        // point high in the frame, so we can take advantage of arm64's preference for positive offsets? C++ native
        // code dedicates callee-saved x19 to this, so generates:
        //      mov x19, sp
        // in the prolog, then uses x19 for local var accesses. Given that this case is so rare, we currently do
        // not do this. That means that negative offsets from FP might need to use the reserved register to form
        // the local variable offset for an addressing mode.

        if (((outgoingArgSpaceSize == 0) && (totalFrameSize <= 504)) && !genSaveFpLrWithAllCalleeSavedRegisters)
        {
            // Case #1.
            //
            // Generate:
            //      stp fp,lr,[sp,#-framesz]!
            //
            // The (totalFrameSize <= 504) condition ensures that both the pre-index STP instruction
            // used in the prolog, and the post-index LDP instruction used in the epilog, can be generated.
            // Note that STP and the unwind codes can handle -512, but LDP with a positive post-index value
            // can only handle up to 504, and we want our prolog and epilog to match.
            //
            // After saving callee-saved registers, we establish the frame pointer with:
            //      mov fp,sp
            // We do this *after* saving callee-saved registers, so the prolog/epilog unwind codes mostly match.

            JITDUMP("Frame type 1. #outsz=0; #framesz=%d; LclFrameSize=%d\n", totalFrameSize, lclFrameSize);

            frameType = 1;

            assert(totalFrameSize <= STACK_PROBE_BOUNDARY_THRESHOLD_BYTES);

            GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_PTRSIZE, REG_FP, REG_LR, REG_SPBASE, -totalFrameSize,
                                          INS_OPTS_PRE_INDEX);
            compiler->unwindSaveRegPairPreindexed(REG_FP, REG_LR, -totalFrameSize);

            maskSaveRegsInt &= ~(RBM_FP | RBM_LR);          // We've already saved FP/LR
            offset = (int)lclFrameSize + 2 * REGSIZE_BYTES; // 2 for FP/LR
        }
        else if (totalFrameSize <= 512)
        {
            // Case #2.
            //
            // The (totalFrameSize <= 512) condition ensures the callee-saved registers can all be saved using STP
            // with signed offset encoding. The maximum positive STP offset is 504, but when storing a pair of
            // 8 byte registers, the largest actual offset we use would be 512 - 8 * 2 = 496. And STR with positive
            // offset has a range 0 to 32760.
            //
            // After saving callee-saved registers, we establish the frame pointer with:
            //      add fp,sp,#outsz
            // We do this *after* saving callee-saved registers, so the prolog/epilog unwind codes mostly match.

            if (genSaveFpLrWithAllCalleeSavedRegisters)
            {
                JITDUMP("Frame type 4 (save FP/LR at top). #outsz=%d; #framesz=%d; LclFrameSize=%d\n",
                        static_cast<unsigned>(outgoingArgSpaceSize), totalFrameSize, lclFrameSize);

                frameType = 4;

                // The frame will be allocated below, when the callee-saved registers are saved. This might mean a
                // separate SUB instruction or the SP adjustment might be folded in to the first STP if there is
                // no outgoing argument space AND no local frame space, that is, if the only thing the frame does
                // is save callee-saved registers (and possibly varargs argument registers).
                calleeSaveSPDelta = totalFrameSize;

                offset = (int)lclFrameSize;
            }
            else
            {
                JITDUMP("Frame type 2 (save FP/LR at bottom). #outsz=%d; #framesz=%d; LclFrameSize=%d\n",
                        static_cast<unsigned>(outgoingArgSpaceSize), totalFrameSize, lclFrameSize);

                frameType = 2;

                // Generate:
                //      sub sp,sp,#framesz
                //      stp fp,lr,[sp,#outsz]   // note that by necessity, #outsz <= #framesz - 16, so #outsz <=
                //      496.

                assert(totalFrameSize - outgoingArgSpaceSize <= STACK_PROBE_BOUNDARY_THRESHOLD_BYTES);

                GetEmitter()->emitIns_R_R_I(INS_sub, EA_PTRSIZE, REG_SPBASE, REG_SPBASE, totalFrameSize);
                compiler->unwindAllocStack(totalFrameSize);

                assert(outgoingArgSpaceSize + 2 * REGSIZE_BYTES <= (unsigned)totalFrameSize);

                GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_PTRSIZE, REG_FP, REG_LR, REG_SPBASE, outgoingArgSpaceSize);
                compiler->unwindSaveRegPair(REG_FP, REG_LR, outgoingArgSpaceSize);

                maskSaveRegsInt &= ~(RBM_FP | RBM_LR);          // We've already saved FP/LR
                offset = (int)lclFrameSize + 2 * REGSIZE_BYTES; // 2 for FP/LR
            }
        }
        else
        {
            // Case 5 or 6.
            //
            // First, the callee-saved registers will be saved, and the callee-saved register code must use
            // pre-index to subtract from SP as the first instruction. It must also leave space for varargs
            // registers to be stored. For example:
            //      stp r19,r20,[sp,#-96]!
            //      stp d8,d9,[sp,#16]
            //      ... save varargs incoming integer registers ...
            // Note that all SP alterations must be 16-byte aligned. We have already calculated any alignment to be
            // lower on the stack than the callee-saved registers (see lvaAlignFrame() for how we calculate
            // alignment). So, if there is an odd number of callee-saved registers, we use (for example, with just
            // one saved register):
            //      sub sp,sp,#16
            //      str r19,[sp,#8]
            // This is one additional instruction, but it centralizes the aligned space. Otherwise, it might be
            // possible to have two 8-byte alignment padding words, one below the callee-saved registers, and one
            // above them. If that is preferable, we could implement it.
            //
            // Note that any varargs saved space will always be 16-byte aligned, since there are 8 argument
            // registers.
            //
            // Then, define #remainingFrameSz = #framesz - (callee-saved attr + varargs space + possible alignment
            // padding from above). Note that #remainingFrameSz must not be zero, since we still need to save FP,SP.
            //
            // Generate:
            //      sub sp,sp,#remainingFrameSz
            // or, for large frames:
            //      mov rX, #remainingFrameSz // maybe multiple instructions
            //      sub sp,sp,rX
            //
            // followed by:
            //      stp fp,lr,[sp,#outsz]
            //      add fp,sp,#outsz
            //
            // However, we need to handle the case where #outsz is larger than the constant signed offset encoding
            // can handle. And, once again, we might need to deal with #outsz that is not aligned to 16-bytes (i.e.,
            // STACK_ALIGN). So, in the case of large #outsz we will have an additional SP adjustment, using one of
            // the following sequences:
            //
            // Define #remainingFrameSz2 = #remainingFrameSz - #outsz.
            //
            //      sub sp,sp,#remainingFrameSz2  // if #remainingFrameSz2 is 16-byte aligned
            //      stp fp,lr,[sp]
            //      mov fp,sp
            //      sub sp,sp,#outsz    // in this case, #outsz must also be 16-byte aligned
            //
            // Or:
            //
            //      sub sp,sp,roundUp(#remainingFrameSz2,16) // if #remainingFrameSz2 is not 16-byte aligned (it is
            //                                               // always guaranteed to be 8 byte aligned).
            //      stp fp,lr,[sp,#8]                        // it will always be #8 in the unaligned case
            //      add fp,sp,#8
            //      sub sp,sp,#outsz - #8
            //
            // (As usual, for a large constant "#outsz - #8", we might need multiple instructions:
            //      mov rX, #outsz - #8 // maybe multiple instructions
            //      sub sp,sp,rX
            // )
            //
            // Note that even if we align the SP alterations, that does not imply that we are creating empty
            // alignment
            // slots. In fact, we are not; any empty alignment slots were calculated in
            // Compiler::lvaAssignFrameOffsets() and its callees.

            int calleeSaveSPDeltaUnaligned = totalFrameSize - lclFrameSize;
            if (genSaveFpLrWithAllCalleeSavedRegisters)
            {
                JITDUMP("Frame type 5 (save FP/LR at top). #outsz=%d; #framesz=%d; LclFrameSize=%d\n",
                        static_cast<unsigned>(outgoingArgSpaceSize), totalFrameSize, lclFrameSize);

                // This case is much simpler, because we allocate space for the callee-saved register area,
                // including
                // FP/LR. Note the SP adjustment might be SUB or be folded into the first store as a predecrement.
                // Then, we use a single SUB to establish the rest of the frame. We need to be careful about where
                // to establish the frame pointer, as there is a limit of 2040 bytes offset from SP to FP in the
                // unwind codes when FP is established.
                frameType = 5;
            }
            else
            {
                JITDUMP("Frame type 3 (save FP/LR at bottom). #outsz=%d; #framesz=%d; LclFrameSize=%d\n",
                        static_cast<unsigned>(outgoingArgSpaceSize), totalFrameSize, lclFrameSize);

                frameType = 3;

                calleeSaveSPDeltaUnaligned -= 2 * REGSIZE_BYTES; // 2 for FP, LR which we'll save later.

                // We'll take care of these later, but callee-saved regs code shouldn't see them.
                maskSaveRegsInt &= ~(RBM_FP | RBM_LR);
            }

            assert(calleeSaveSPDeltaUnaligned >= 0);
            assert((calleeSaveSPDeltaUnaligned % 8) == 0); // It better at least be 8 byte aligned.
            calleeSaveSPDelta = AlignUp((UINT)calleeSaveSPDeltaUnaligned, STACK_ALIGN);

            offset = calleeSaveSPDelta - calleeSaveSPDeltaUnaligned;

            JITDUMP("    calleeSaveSPDelta=%d, offset=%d\n", calleeSaveSPDelta, offset);

            // At most one alignment slot between SP and where we store the callee-saved registers.
            assert((offset == 0) || (offset == REGSIZE_BYTES));
        }
    }
    else
    {
        // No frame pointer (no chaining).
        assert((maskSaveRegsInt & RBM_FP) == 0);
        assert((maskSaveRegsInt & RBM_LR) != 0);

        // Note that there is no pre-indexed save_lrpair unwind code variant, so we can't allocate the frame using
        // 'stp' if we only have one callee-saved register plus LR to save.

        NYI("Frame without frame pointer");
        offset = 0;
    }

    assert(frameType != 0);

    JITDUMP("    offset=%d, calleeSaveSPDelta=%d\n", offset, calleeSaveSPDelta);
    genSaveCalleeSavedRegistersHelp(maskSaveRegsInt | maskSaveRegsFloat, offset, -calleeSaveSPDelta);

    offset += genCountBits(maskSaveRegsInt | maskSaveRegsFloat) * REGSIZE_BYTES;

    // For varargs, home the incoming arg registers last. Note that there is nothing to unwind here,
    // so we just report "NOP" unwind codes. If there's no more frame setup after this, we don't
    // need to add codes at all.

    if (compiler->info.compIsVarArgs)
    {
        JITDUMP("    compIsVarArgs=true\n");

        // There are 8 general-purpose registers to home, thus 'offset' must be 16-byte aligned here.
        assert((offset % 16) == 0);
        for (regNumber reg1 = REG_ARG_FIRST; reg1 < REG_ARG_LAST; reg1 = REG_NEXT(REG_NEXT(reg1)))
        {
            regNumber reg2 = REG_NEXT(reg1);
            // stp REG, REG + 1, [SP, #offset]
            GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_PTRSIZE, reg1, reg2, REG_SPBASE, offset);
            compiler->unwindNop();
            offset += 2 * REGSIZE_BYTES;
        }
    }

    // By default, we'll establish the frame pointer chain. (Note that currently frames without FP are NYI.)
    bool establishFramePointer = true;

    // If we do establish the frame pointer, what is the amount we add to SP to do so?
    unsigned offsetSpToSavedFp = 0;

    if (frameType == 1)
    {
        assert(!genSaveFpLrWithAllCalleeSavedRegisters);
        assert(offsetSpToSavedFp == 0);
    }
    else if (frameType == 2)
    {
        assert(!genSaveFpLrWithAllCalleeSavedRegisters);

        offsetSpToSavedFp = outgoingArgSpaceSize;
    }
    else if (frameType == 3)
    {
        assert(!genSaveFpLrWithAllCalleeSavedRegisters);

        int remainingFrameSz = totalFrameSize - calleeSaveSPDelta;
        assert(remainingFrameSz > 0);
        assert((remainingFrameSz % 16) == 0); // this is guaranteed to be 16-byte aligned because each component --
        // totalFrameSize and calleeSaveSPDelta -- is 16-byte aligned.

        if (outgoingArgSpaceSize > 504)
        {
            // We can't do "stp fp,lr,[sp,#outsz]" because #outsz is too big.
            // If compiler->lvaOutgoingArgSpaceSize is not aligned, we need to align the SP adjustment.
            assert(remainingFrameSz > static_cast<int>(outgoingArgSpaceSize));
            int spAdjustment2Unaligned = remainingFrameSz - outgoingArgSpaceSize;
            int spAdjustment2 = static_cast<int>(roundUp(static_cast<unsigned>(spAdjustment2Unaligned), STACK_ALIGN));
            int alignmentAdjustment2 = spAdjustment2 - spAdjustment2Unaligned;
            assert((alignmentAdjustment2 == 0) || (alignmentAdjustment2 == 8));

            JITDUMP("    spAdjustment2=%d\n", spAdjustment2);

            genPrologSaveRegPair(REG_FP, REG_LR, alignmentAdjustment2, -spAdjustment2, false, initReg, pInitRegZeroed);
            offset += spAdjustment2;

            // Now subtract off the #outsz (or the rest of the #outsz if it was unaligned, and the above "sub"
            // included some of it)

            int spAdjustment3 = outgoingArgSpaceSize - alignmentAdjustment2;
            assert(spAdjustment3 > 0);
            assert((spAdjustment3 % 16) == 0);

            JITDUMP("    alignmentAdjustment2=%d\n", alignmentAdjustment2);
            PrologEstablishFramePointer(alignmentAdjustment2, /* reportUnwindData */ true);

            // We just established the frame pointer chain; don't do it again.
            establishFramePointer = false;

            JITDUMP("    spAdjustment3=%d\n", spAdjustment3);

            // We've already established the frame pointer, so no need to report the stack pointer change to unwind
            // info.
            genStackPointerAdjustment(-spAdjustment3, initReg, pInitRegZeroed, /* reportUnwindData */ false);
            offset += spAdjustment3;
        }
        else
        {
            genPrologSaveRegPair(REG_FP, REG_LR, outgoingArgSpaceSize, -remainingFrameSz, false, initReg,
                                 pInitRegZeroed);
            offset += remainingFrameSz;

            offsetSpToSavedFp = outgoingArgSpaceSize;
        }
    }
    else if (frameType == 4)
    {
        assert(genSaveFpLrWithAllCalleeSavedRegisters);
        offsetSpToSavedFp = calleeSaveSPDelta - (compiler->info.compIsVarArgs ? MAX_REG_ARG * REGSIZE_BYTES : 0) -
                            2 * REGSIZE_BYTES; // -2 for FP, LR
    }
    else if (frameType == 5)
    {
        assert(genSaveFpLrWithAllCalleeSavedRegisters);

        offsetSpToSavedFp = calleeSaveSPDelta - (compiler->info.compIsVarArgs ? MAX_REG_ARG * REGSIZE_BYTES : 0) -
                            2 * REGSIZE_BYTES; // -2 for FP, LR
        JITDUMP("    offsetSpToSavedFp=%d\n", offsetSpToSavedFp);
        PrologEstablishFramePointer(offsetSpToSavedFp, /* reportUnwindData */ true);

        // We just established the frame pointer chain; don't do it again.
        establishFramePointer = false;

        int remainingFrameSz = totalFrameSize - calleeSaveSPDelta;
        assert(remainingFrameSz > 0);
        assert((remainingFrameSz % 16) == 0); // this is guaranteed to be 16-byte aligned because each component --
        // totalFrameSize and calleeSaveSPDelta -- is 16-byte aligned.

        JITDUMP("    remainingFrameSz=%d\n", remainingFrameSz);

        // We've already established the frame pointer, so no need to report the stack pointer change to unwind
        // info.
        genStackPointerAdjustment(-remainingFrameSz, initReg, pInitRegZeroed, /* reportUnwindData */ false);
        offset += remainingFrameSz;
    }
    else
    {
        unreached();
    }

    if (establishFramePointer)
    {
        JITDUMP("    offsetSpToSavedFp=%d\n", offsetSpToSavedFp);
        PrologEstablishFramePointer(offsetSpToSavedFp, /* reportUnwindData */ true);
    }

    assert(offset == totalFrameSize);
}

void CodeGen::PrologBlockInitLocals(int untrLclLo, int untrLclHi, regNumber initReg, bool* pInitRegZeroed)
{
    assert(generatingProlog && genUseBlockInit);
    assert(untrLclHi > untrLclLo);

    int bytesToWrite = untrLclHi - untrLclLo;

    const regNumber zeroSimdReg          = REG_ZERO_INIT_FRAME_SIMD;
    bool            simdRegZeroed        = false;
    const int       simdRegPairSizeBytes = 2 * FP_REGSIZE_BYTES;

    regNumber addrReg = REG_ZERO_INIT_FRAME_REG1;

    if (addrReg == initReg)
    {
        *pInitRegZeroed = false;
    }

    int addrOffset = 0;

    // The following invariants are held below:
    //
    //   1) [addrReg, #addrOffset] points at a location where next chunk of zero bytes will be written;
    //   2) bytesToWrite specifies the number of bytes on the frame to initialize;
    //   3) if simdRegZeroed is true then 128-bit wide zeroSimdReg contains zeroes.

    const int bytesUseZeroingLoop = 192;

    if (bytesToWrite >= bytesUseZeroingLoop)
    {
        // Generates the following code:
        //
        // When the size of the region is greater than or equal to 256 bytes
        // **and** DC ZVA instruction use is permitted
        // **and** the instruction block size is configured to 64 bytes:
        //
        //    movi    v16.16b, #0
        //    add     x9, fp, #(untrLclLo+64)
        //    add     x10, fp, #(untrLclHi-64)
        //    stp     q16, q16, [x9, #-64]
        //    stp     q16, q16, [x9, #-32]
        //    bfm     x9, xzr, #0, #5
        //
        // loop:
        //    dc      zva, x9
        //    add     x9, x9, #64
        //    cmp     x9, x10
        //    blo     loop
        //
        //    stp     q16, q16, [x10]
        //    stp     q16, q16, [x10, #32]
        //
        // Otherwise:
        //
        //     movi    v16.16b, #0
        //     add     x9, fp, #(untrLclLo-32)
        //     mov     x10, #(bytesToWrite-64)
        //
        // loop:
        //     stp     q16, q16, [x9, #32]
        //     stp     q16, q16, [x9, #64]!
        //     subs    x10, x10, #64
        //     bge     loop

        const int bytesUseDataCacheZeroInstruction = 256;

        GetEmitter()->emitIns_R_I(INS_movi, EA_16BYTE, zeroSimdReg, 0, INS_OPTS_16B);
        simdRegZeroed = true;

        if ((bytesToWrite >= bytesUseDataCacheZeroInstruction) &&
            compiler->compOpportunisticallyDependsOn(InstructionSet_Dczva))
        {
            // The first and the last 64 bytes should be written with two stp q-reg instructions.
            // This is in order to avoid **unintended** zeroing of the data by dc zva
            // outside of [fp+untrLclLo, fp+untrLclHi) memory region.

            genInstrWithConstant(INS_add, EA_PTRSIZE, addrReg, genFramePointerReg(), untrLclLo + 64, addrReg);
            addrOffset = -64;

            const regNumber endAddrReg = REG_ZERO_INIT_FRAME_REG2;

            if (endAddrReg == initReg)
            {
                *pInitRegZeroed = false;
            }

            genInstrWithConstant(INS_add, EA_PTRSIZE, endAddrReg, genFramePointerReg(), untrLclHi - 64, endAddrReg);

            GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_16BYTE, zeroSimdReg, zeroSimdReg, addrReg, addrOffset);
            addrOffset += simdRegPairSizeBytes;

            GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_16BYTE, zeroSimdReg, zeroSimdReg, addrReg, addrOffset);
            addrOffset += simdRegPairSizeBytes;

            assert(addrOffset == 0);

            GetEmitter()->emitIns_R_R_I_I(INS_bfm, EA_PTRSIZE, addrReg, REG_ZR, 0, 5);
            // addrReg points at the beginning of a cache line.

            GetEmitter()->emitIns_R(INS_dczva, EA_PTRSIZE, addrReg);
            GetEmitter()->emitIns_R_R_I(INS_add, EA_PTRSIZE, addrReg, addrReg, 64);
            GetEmitter()->emitIns_R_R(INS_cmp, EA_PTRSIZE, addrReg, endAddrReg);
            GetEmitter()->emitIns_J(INS_blo, NULL, -4);

            addrReg      = endAddrReg;
            bytesToWrite = 64;
        }
        else
        {
            genInstrWithConstant(INS_add, EA_PTRSIZE, addrReg, genFramePointerReg(), untrLclLo - 32, addrReg);
            addrOffset = 32;

            const regNumber countReg = REG_ZERO_INIT_FRAME_REG2;

            if (countReg == initReg)
            {
                *pInitRegZeroed = false;
            }

            instGen_Set_Reg_To_Imm(EA_PTRSIZE, countReg, bytesToWrite - 64);

            GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_16BYTE, zeroSimdReg, zeroSimdReg, addrReg, 32);
            GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_16BYTE, zeroSimdReg, zeroSimdReg, addrReg, 64,
                                          INS_OPTS_PRE_INDEX);

            GetEmitter()->emitIns_R_R_I(INS_subs, EA_PTRSIZE, countReg, countReg, 64);
            GetEmitter()->emitIns_J(INS_bge, NULL, -4);

            bytesToWrite %= 64;
        }
    }
    else
    {
        genInstrWithConstant(INS_add, EA_PTRSIZE, addrReg, genFramePointerReg(), untrLclLo, addrReg);
    }

    if (bytesToWrite >= simdRegPairSizeBytes)
    {
        // Generates the following code:
        //
        //     movi    v16.16b, #0
        //     stp     q16, q16, [x9, #addrOffset]
        //     stp     q16, q16, [x9, #(addrOffset+32)]
        // ...
        //     stp     q16, q16, [x9, #(addrOffset+roundDown(bytesToWrite, 32))]

        if (!simdRegZeroed)
        {
            GetEmitter()->emitIns_R_I(INS_movi, EA_16BYTE, zeroSimdReg, 0, INS_OPTS_16B);
            simdRegZeroed = true;
        }

        for (; bytesToWrite >= simdRegPairSizeBytes; bytesToWrite -= simdRegPairSizeBytes)
        {
            GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_16BYTE, zeroSimdReg, zeroSimdReg, addrReg, addrOffset);
            addrOffset += simdRegPairSizeBytes;
        }
    }

    const int regPairSizeBytes = 2 * REGSIZE_BYTES;

    if (bytesToWrite >= regPairSizeBytes)
    {
        GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_PTRSIZE, REG_ZR, REG_ZR, addrReg, addrOffset);
        addrOffset += regPairSizeBytes;
        bytesToWrite -= regPairSizeBytes;
    }

    if (bytesToWrite >= REGSIZE_BYTES)
    {
        GetEmitter()->emitIns_R_R_I(INS_str, EA_PTRSIZE, REG_ZR, addrReg, addrOffset);
        addrOffset += REGSIZE_BYTES;
        bytesToWrite -= REGSIZE_BYTES;
    }

    if (bytesToWrite == sizeof(int))
    {
        GetEmitter()->emitIns_R_R_I(INS_str, EA_4BYTE, REG_ZR, addrReg, addrOffset);
        bytesToWrite = 0;
    }

    assert(bytesToWrite == 0);
}

void CodeGen::PrologZeroRegs(regMaskTP initRegs, regNumber initReg)
{
    for (regNumber reg = REG_INT_FIRST; reg <= REG_INT_LAST; reg = REG_NEXT(reg))
    {
        if (((initRegs & genRegMask(reg)) == RBM_NONE) || (reg == initReg))
        {
            continue;
        }

        instGen_Set_Reg_To_Zero(EA_8BYTE, reg);
    }

    // TODO-MIKE-CQ: Copying from another reg instead of just zeroing with movi is dubious...
    regNumber zeroReg = REG_NA;

    for (regNumber reg = REG_FP_FIRST; reg <= REG_FP_LAST; reg = REG_NEXT(reg))
    {
        if ((initRegs & genRegMask(reg)) == RBM_NONE)
        {
            continue;
        }

        if (zeroReg == REG_NA)
        {
            GetEmitter()->emitIns_R_I(INS_movi, EA_16BYTE, reg, 0, INS_OPTS_16B);
            zeroReg = reg;
            continue;
        }

        GetEmitter()->emitIns_Mov(INS_fmov, EA_8BYTE, reg, zeroReg, /* canSkip */ false);
    }
}

void CodeGen::genPopCalleeSavedRegistersAndFreeLclFrame(bool jmpEpilog)
{
    assert(generatingEpilog);

    regMaskTP rsRestoreRegs = calleeSavedModifiedRegs;

    if (isFramePointerUsed())
    {
        rsRestoreRegs |= RBM_FP;
    }

    rsRestoreRegs |= RBM_LR; // We must save/restore the return address (in the LR register)

    regMaskTP regsToRestoreMask = rsRestoreRegs;

    int totalFrameSize = genTotalFrameSize();

    int calleeSaveSPOffset = 0; // This will be the starting place for restoring the callee-saved registers, in
    // decreasing order.
    int frameType         = 0; // An indicator of what type of frame we are popping.
    int calleeSaveSPDelta = 0; // Amount to add to SP after callee-saved registers have been restored.

    if (isFramePointerUsed())
    {
        if ((outgoingArgSpaceSize == 0) && (totalFrameSize <= 504) && !genSaveFpLrWithAllCalleeSavedRegisters)
        {
            JITDUMP("Frame type 1. #outsz=0; #framesz=%d; localloc? %s\n", totalFrameSize,
                    dspBool(compiler->compLocallocUsed));

            frameType = 1;
            if (compiler->compLocallocUsed)
            {
                // Restore sp from fp
                //      mov sp, fp
                inst_Mov(TYP_I_IMPL, REG_SPBASE, REG_FPBASE, /* canSkip */ false);
                compiler->unwindSetFrameReg(REG_FPBASE, 0);
            }

            regsToRestoreMask &= ~(RBM_FP | RBM_LR); // We'll restore FP/LR at the end, and post-index SP.

            // Compute callee save SP offset which is at the top of local frame while the FP/LR is saved at the
            // bottom of stack.
            calleeSaveSPOffset = lclFrameSize + 2 * REGSIZE_BYTES;
        }
        else if (totalFrameSize <= 512)
        {
            if (compiler->compLocallocUsed)
            {
                // Restore sp from fp
                //      sub sp, fp, #outsz // Uses #outsz if FP/LR stored at bottom
                int SPtoFPdelta = genSPtoFPdelta();
                GetEmitter()->emitIns_R_R_I(INS_sub, EA_PTRSIZE, REG_SPBASE, REG_FPBASE, SPtoFPdelta);
                compiler->unwindSetFrameReg(REG_FPBASE, SPtoFPdelta);
            }

            if (genSaveFpLrWithAllCalleeSavedRegisters)
            {
                JITDUMP("Frame type 4 (save FP/LR at top). #outsz=%d; #framesz=%d; localloc? %s\n",
                        static_cast<unsigned>(outgoingArgSpaceSize), totalFrameSize,
                        dspBool(compiler->compLocallocUsed));

                frameType = 4;

                calleeSaveSPOffset = lclFrameSize;

                // Remove the frame after we're done restoring the callee-saved registers.
                calleeSaveSPDelta = totalFrameSize;
            }
            else
            {
                JITDUMP("Frame type 2 (save FP/LR at bottom). #outsz=%d; #framesz=%d; localloc? %s\n",
                        static_cast<unsigned>(outgoingArgSpaceSize), totalFrameSize,
                        dspBool(compiler->compLocallocUsed));

                frameType = 2;

                regsToRestoreMask &= ~(RBM_FP | RBM_LR); // We'll restore FP/LR at the end, and post-index SP.

                // Compute callee save SP offset which is at the top of local frame while the FP/LR is saved at the
                // bottom of stack.
                calleeSaveSPOffset = lclFrameSize + 2 * REGSIZE_BYTES;
            }
        }
        else if (!genSaveFpLrWithAllCalleeSavedRegisters)
        {
            JITDUMP("Frame type 3 (save FP/LR at bottom). #outsz=%d; #framesz=%d; localloc? %s\n",
                    static_cast<unsigned>(outgoingArgSpaceSize), totalFrameSize, dspBool(compiler->compLocallocUsed));

            frameType = 3;

            int calleeSaveSPDeltaUnaligned =
                totalFrameSize - lclFrameSize - 2 * REGSIZE_BYTES; // 2 for FP, LR which we'll restore later.
            assert(calleeSaveSPDeltaUnaligned >= 0);
            assert((calleeSaveSPDeltaUnaligned % 8) == 0); // It better at least be 8 byte aligned.
            calleeSaveSPDelta = AlignUp((UINT)calleeSaveSPDeltaUnaligned, STACK_ALIGN);

            JITDUMP("    calleeSaveSPDelta=%d\n", calleeSaveSPDelta);

            regsToRestoreMask &= ~(RBM_FP | RBM_LR); // We'll restore FP/LR at the end, and (hopefully) post-index SP.

            int remainingFrameSz = totalFrameSize - calleeSaveSPDelta;
            assert(remainingFrameSz > 0);

            if (outgoingArgSpaceSize > 504)
            {
                // We can't do "ldp fp,lr,[sp,#outsz]" because #outsz is too big.
                // If compiler->lvaOutgoingArgSpaceSize is not aligned, we need to align the SP adjustment.
                assert(remainingFrameSz > static_cast<int>(outgoingArgSpaceSize));
                int spAdjustment2Unaligned = remainingFrameSz - static_cast<int>(outgoingArgSpaceSize);
                int spAdjustment2 =
                    static_cast<int>(roundUp(static_cast<unsigned>(spAdjustment2Unaligned), STACK_ALIGN));
                int alignmentAdjustment2 = spAdjustment2 - spAdjustment2Unaligned;
                assert((alignmentAdjustment2 == 0) || (alignmentAdjustment2 == REGSIZE_BYTES));

                // Restore sp from fp. No need to update sp after this since we've set up fp before adjusting sp
                // in prolog.
                //      sub sp, fp, #alignmentAdjustment2
                GetEmitter()->emitIns_R_R_I(INS_sub, EA_PTRSIZE, REG_SPBASE, REG_FPBASE, alignmentAdjustment2);
                compiler->unwindSetFrameReg(REG_FPBASE, alignmentAdjustment2);

                // Generate:
                //      ldp fp,lr,[sp]
                //      add sp,sp,#remainingFrameSz

                JITDUMP("    alignmentAdjustment2=%d\n", alignmentAdjustment2);
                genEpilogRestoreRegPair(REG_FP, REG_LR, alignmentAdjustment2, spAdjustment2, false, REG_IP1, nullptr);
            }
            else
            {
                if (compiler->compLocallocUsed)
                {
                    // Restore sp from fp; here that's #outsz from SP
                    //      sub sp, fp, #outsz
                    int SPtoFPdelta = genSPtoFPdelta();
                    assert(SPtoFPdelta == static_cast<int>(outgoingArgSpaceSize));
                    GetEmitter()->emitIns_R_R_I(INS_sub, EA_PTRSIZE, REG_SPBASE, REG_FPBASE, SPtoFPdelta);
                    compiler->unwindSetFrameReg(REG_FPBASE, SPtoFPdelta);
                }

                // Generate:
                //      ldp fp,lr,[sp,#outsz]
                //      add sp,sp,#remainingFrameSz     ; might need to load this constant in a scratch register if
                //                                      ; it's large

                JITDUMP("    remainingFrameSz=%d\n", remainingFrameSz);

                genEpilogRestoreRegPair(REG_FP, REG_LR, outgoingArgSpaceSize, remainingFrameSz, false, REG_IP1,
                                        nullptr);
            }

            // Unlike frameType=1 or frameType=2 that restore SP at the end,
            // frameType=3 already adjusted SP above to delete local frame.
            // There is at most one alignment slot between SP and where we store the callee-saved registers.
            calleeSaveSPOffset = calleeSaveSPDelta - calleeSaveSPDeltaUnaligned;
            assert((calleeSaveSPOffset == 0) || (calleeSaveSPOffset == REGSIZE_BYTES));
        }
        else
        {
            JITDUMP("Frame type 5 (save FP/LR at top). #outsz=%d; #framesz=%d; localloc? %s\n",
                    static_cast<unsigned>(outgoingArgSpaceSize), totalFrameSize, dspBool(compiler->compLocallocUsed));

            frameType = 5;

            int calleeSaveSPDeltaUnaligned = totalFrameSize - lclFrameSize;
            assert(calleeSaveSPDeltaUnaligned >= 0);
            assert((calleeSaveSPDeltaUnaligned % 8) == 0); // It better at least be 8 byte aligned.
            calleeSaveSPDelta = AlignUp((UINT)calleeSaveSPDeltaUnaligned, STACK_ALIGN);

            calleeSaveSPOffset = calleeSaveSPDelta - calleeSaveSPDeltaUnaligned;
            assert((calleeSaveSPOffset == 0) || (calleeSaveSPOffset == REGSIZE_BYTES));

            // Restore sp from fp:
            //      sub sp, fp, #sp-to-fp-delta
            // This is the same whether there is localloc or not. Note that we don't need to do anything to remove the
            // "remainingFrameSz" to reverse the SUB of that amount in the prolog.

            int offsetSpToSavedFp = calleeSaveSPDelta -
                                    (compiler->info.compIsVarArgs ? MAX_REG_ARG * REGSIZE_BYTES : 0) -
                                    2 * REGSIZE_BYTES; // -2 for FP, LR
            GetEmitter()->emitIns_R_R_I(INS_sub, EA_PTRSIZE, REG_SPBASE, REG_FPBASE, offsetSpToSavedFp);
            compiler->unwindSetFrameReg(REG_FPBASE, offsetSpToSavedFp);
        }
    }
    else
    {
        // No frame pointer (no chaining).
        NYI("Frame without frame pointer");
        calleeSaveSPOffset = 0;
    }

    JITDUMP("    calleeSaveSPOffset=%d, calleeSaveSPDelta=%d\n", calleeSaveSPOffset, calleeSaveSPDelta);
    genRestoreCalleeSavedRegistersHelp(regsToRestoreMask, calleeSaveSPOffset, calleeSaveSPDelta);

    if (frameType == 1)
    {
        // Generate:
        //      ldp fp,lr,[sp],#framesz

        GetEmitter()->emitIns_R_R_R_I(INS_ldp, EA_PTRSIZE, REG_FP, REG_LR, REG_SPBASE, totalFrameSize,
                                      INS_OPTS_POST_INDEX);
        compiler->unwindSaveRegPairPreindexed(REG_FP, REG_LR, -totalFrameSize);
    }
    else if (frameType == 2)
    {
        // Generate:
        //      ldr fp,lr,[sp,#outsz]
        //      add sp,sp,#framesz

        GetEmitter()->emitIns_R_R_R_I(INS_ldp, EA_PTRSIZE, REG_FP, REG_LR, REG_SPBASE, outgoingArgSpaceSize);
        compiler->unwindSaveRegPair(REG_FP, REG_LR, outgoingArgSpaceSize);

        GetEmitter()->emitIns_R_R_I(INS_add, EA_PTRSIZE, REG_SPBASE, REG_SPBASE, totalFrameSize);
        compiler->unwindAllocStack(totalFrameSize);
    }
    else if (frameType == 3)
    {
        // Nothing to do after restoring callee-saved registers.
    }
    else if (frameType == 4)
    {
        // Nothing to do after restoring callee-saved registers.
    }
    else if (frameType == 5)
    {
        // Nothing to do after restoring callee-saved registers.
    }
    else
    {
        unreached();
    }
}

void CodeGen::genFnEpilog(BasicBlock* block)
{
    JITDUMP("*************** In genFnEpilog()\n");
    DBEXEC(compiler->opts.dspCode, printf("\n__epilog:\n"))

    ScopedSetVariable<bool> _setGeneratingEpilog(&generatingEpilog, true);

    bool     jmpEpilog = ((block->bbFlags & BBF_HAS_JMP) != 0);
    GenTree* lastNode  = block->lastNode();

    // Method handle and address info used in case of jump epilog
    CORINFO_METHOD_HANDLE methHnd = nullptr;
    CORINFO_CONST_LOOKUP  addrInfo;
    addrInfo.addr       = nullptr;
    addrInfo.accessType = IAT_VALUE;

    if (jmpEpilog && lastNode->gtOper == GT_JMP)
    {
        methHnd = (CORINFO_METHOD_HANDLE)lastNode->AsVal()->gtVal1;
        compiler->info.compCompHnd->getFunctionEntryPoint(methHnd, &addrInfo);
    }

    compiler->unwindBegEpilog();

    genPopCalleeSavedRegistersAndFreeLclFrame(jmpEpilog);

    if (jmpEpilog)
    {
        GenJmpEpilog(block, methHnd, addrInfo);
    }
    else
    {
        inst_RV(INS_ret, REG_LR, TYP_I_IMPL);
        compiler->unwindReturn(REG_LR);
    }

    compiler->unwindEndEpilog();
}

#endif // TARGET_ARM64
