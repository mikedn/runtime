// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                        ARM Code Generator                                 XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/
#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#ifdef TARGET_ARM
#include "codegen.h"
#include "lower.h"
#include "emit.h"

//------------------------------------------------------------------------
// genInstrWithConstant: We will typically generate one instruction
//
//    ins  reg1, reg2, imm
//
// However the imm might not fit as a directly encodable immediate.
// When it doesn't fit we generate extra instruction(s) that sets up
// the 'regTmp' with the proper immediate value.
//
//     mov  regTmp, imm
//     ins  reg1, reg2, regTmp
//
// Generally, codegen constants are marked non-containable if they don't fit. This function
// is used for cases that aren't mirrored in the IR, such as in the prolog.
//
// Arguments:
//    ins                 - instruction
//    attr                - operation size and GC attribute
//    reg1, reg2          - first and second register operands
//    imm                 - immediate value (third operand when it fits)
//    tmpReg              - temp register to use when the 'imm' doesn't fit. Can be REG_NA
//                          if caller knows for certain the constant will fit.
//
// Return Value:
//    returns true if the immediate was small enough to be encoded inside instruction. If not,
//    returns false meaning the immediate was too large and tmpReg was used and modified.
//
bool CodeGen::genInstrWithConstant(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, ssize_t imm, regNumber tmpReg)
{
    bool immFitsInIns = false;

    // reg1 is usually a dest register
    // reg2 is always source register
    assert(tmpReg != reg2); // regTmp cannot match any source register

    switch (ins)
    {
        case INS_add:
        case INS_sub:
            immFitsInIns = emitter::validImmForInstr(ins, (target_ssize_t)imm);
            break;

        default:
            assert(!"Unexpected instruction in genInstrWithConstant");
            break;
    }

    if (immFitsInIns)
    {
        // generate a single instruction that encodes the immediate directly
        GetEmitter()->emitIns_R_R_I(ins, attr, reg1, reg2, (target_ssize_t)imm);
    }
    else
    {
        // caller can specify REG_NA  for tmpReg, when it "knows" that the immediate will always fit
        assert(tmpReg != REG_NA);

        // generate two or more instructions

        // first we load the immediate into tmpReg
        instGen_Set_Reg_To_Imm(attr, tmpReg, imm);

        // generate the instruction using a three register encoding with the immediate in tmpReg
        GetEmitter()->emitIns_R_R_R(ins, attr, reg1, reg2, tmpReg);
    }
    return immFitsInIns;
}

//------------------------------------------------------------------------
// genStackPointerAdjustment: add a specified constant value to the stack pointer.
// An available temporary register is required to be specified, in case the constant
// is too large to encode in an "add" instruction (or "sub" instruction if we choose
// to use one), such that we need to load the constant into a register first, before using it.
//
// Arguments:
//    spDelta                 - the value to add to SP (can be negative)
//    tmpReg                  - an available temporary register
//
// Return Value:
//    returns true if the immediate was small enough to be encoded inside instruction. If not,
//    returns false meaning the immediate was too large and tmpReg was used and modified.
//
bool CodeGen::genStackPointerAdjustment(ssize_t spDelta, regNumber tmpReg)
{
    // Even though INS_add is specified here, the encoder will choose either
    // an INS_add or an INS_sub and encode the immediate as a positive value
    return genInstrWithConstant(INS_add, EA_PTRSIZE, REG_SPBASE, REG_SPBASE, spDelta, tmpReg);
}

//------------------------------------------------------------------------
// genCallFinally: Generate a call to the finally block.
//
BasicBlock* CodeGen::genCallFinally(BasicBlock* block)
{
    BasicBlock* bbFinallyRet = nullptr;

    // We don't have retless calls, since we use the BBJ_ALWAYS to point at a NOP pad where
    // we would have otherwise created retless calls.
    assert(block->isBBCallAlwaysPair());

    assert(block->bbNext != NULL);
    assert(block->bbNext->bbJumpKind == BBJ_ALWAYS);
    assert(block->bbNext->bbJumpDest != NULL);
    assert(block->bbNext->bbJumpDest->bbFlags & BBF_FINALLY_TARGET);

    bbFinallyRet = block->bbNext->bbJumpDest;

    // Load the address where the finally funclet should return into LR.
    // The funclet prolog/epilog will do "push {lr}" / "pop {pc}" to do the return.
    genMov32RelocatableDisplacement(bbFinallyRet, REG_LR);

    // Jump to the finally BB
    inst_JMP(EJ_jmp, block->bbJumpDest);

    // The BBJ_ALWAYS is used because the BBJ_CALLFINALLY can't point to the
    // jump target using bbJumpDest - that is already used to point
    // to the finally block. So just skip past the BBJ_ALWAYS unless the
    // block is RETLESS.
    assert(!(block->bbFlags & BBF_RETLESS_CALL));
    assert(block->isBBCallAlwaysPair());
    return block->bbNext;
}

void CodeGen::genEHCatchRet(BasicBlock* block)
{
    genMov32RelocatableDisplacement(block->bbJumpDest, REG_INTRET);
}

// Move of relocatable displacement value to register
void CodeGen::genMov32RelocatableDisplacement(BasicBlock* block, regNumber reg)
{
    GetEmitter()->emitIns_R_L(INS_movw, EA_4BYTE_DSP_RELOC, block, reg);
    GetEmitter()->emitIns_R_L(INS_movt, EA_4BYTE_DSP_RELOC, block, reg);

    if (compiler->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_RELATIVE_CODE_RELOCS))
    {
        GetEmitter()->emitIns_R_R_R(INS_add, EA_4BYTE_DSP_RELOC, reg, reg, REG_PC);
    }
}

// Move of relocatable data-label to register
void CodeGen::genMov32RelocatableDataLabel(unsigned value, regNumber reg)
{
    GetEmitter()->emitIns_R_D(INS_movw, EA_HANDLE_CNS_RELOC, value, reg);
    GetEmitter()->emitIns_R_D(INS_movt, EA_HANDLE_CNS_RELOC, value, reg);

    if (compiler->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_RELATIVE_CODE_RELOCS))
    {
        GetEmitter()->emitIns_R_R_R(INS_add, EA_HANDLE_CNS_RELOC, reg, reg, REG_PC);
    }
}

// Move of relocatable immediate to register
void CodeGen::genMov32RelocatableImmediate(emitAttr size, BYTE* addr, regNumber reg)
{
    _ASSERTE(EA_IS_RELOC(size));

    GetEmitter()->emitIns_MovRelocatableImmediate(INS_movw, size, reg, addr);
    GetEmitter()->emitIns_MovRelocatableImmediate(INS_movt, size, reg, addr);

    if (compiler->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_RELATIVE_CODE_RELOCS))
    {
        GetEmitter()->emitIns_R_R_R(INS_add, size, reg, reg, REG_PC);
    }
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
        // TODO-CrossBitness: we wouldn't need the cast below if we had CodeGen::instGen_Set_Reg_To_Reloc_Imm.
        genMov32RelocatableImmediate(size, (BYTE*)imm, reg);
    }
    else if (imm == 0)
    {
        GetEmitter()->emitIns_R_I(INS_mov, size, reg, 0);
    }
    else
    {
        // TODO-CrossBitness: we wouldn't need the cast below if we had CodeGen::instGen_Set_Reg_To_Reloc_Imm.
        // TODO-MIKE-Review: Why the crap does ARM use ssize_t for imm?!?
        const int val32 = (int)imm;
        if (emitter::emitIns_valid_imm_for_mov(val32))
        {
            GetEmitter()->emitIns_R_I(INS_mov, size, reg, val32);
        }
        else // We have to use a movw/movt pair of instructions
        {
            const int imm_lo16 = val32 & 0xffff;
            const int imm_hi16 = (val32 >> 16) & 0xffff;

            assert(emitter::emitIns_valid_imm_for_mov(imm_lo16));
            assert(imm_hi16 != 0);

            GetEmitter()->emitIns_R_I(INS_movw, size, reg, imm_lo16);

            // If we've got a low register, the high word is all bits set,
            // and the high bit of the low word is set, we can sign extend
            // halfword and save two bytes of encoding. This can happen for
            // small magnitude negative numbers 'n' for -32768 <= n <= -1.

            if (GetEmitter()->isLowRegister(reg) && (imm_hi16 == 0xffff) && ((imm_lo16 & 0x8000) == 0x8000))
            {
                GetEmitter()->emitIns_Mov(INS_sxth, EA_4BYTE, reg, reg, /* canSkip */ false);
            }
            else
            {
                GetEmitter()->emitIns_R_I(INS_movt, size, reg, imm_hi16);
            }
        }
    }
}

void CodeGen::GenIntCon(GenTreeIntCon* node)
{
    if (node->ImmedValNeedsReloc(compiler))
    {
        instGen_Set_Reg_To_Imm(EA_HANDLE_CNS_RELOC, node->GetRegNum(), node->GetValue());
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
    // TODO-ARM-CQ: Do we have a faster/smaller way to generate 0.0 in thumb2 ISA ?

    if (node->TypeIs(TYP_FLOAT))
    {
        uint32_t bits = node->GetFloatBits();

        regNumber temp = node->GetSingleTempReg();
        instGen_Set_Reg_To_Imm(EA_4BYTE, temp, static_cast<int32_t>(bits));

        GetEmitter()->emitIns_Mov(INS_vmov_i2f, EA_4BYTE, node->GetRegNum(), temp, /* canSkip */ false);
    }
    else
    {
        assert(node->TypeIs(TYP_DOUBLE));

        uint64_t bits = node->GetDoubleBits();

        regNumber temp1 = node->ExtractTempReg();
        regNumber temp2 = node->GetSingleTempReg();
        instGen_Set_Reg_To_Imm(EA_4BYTE, temp1, static_cast<int32_t>(bits & UINT32_MAX));
        instGen_Set_Reg_To_Imm(EA_4BYTE, temp2, static_cast<int32_t>(bits >> 32));

        GetEmitter()->emitIns_R_R_R(INS_vmov_i2d, EA_8BYTE, node->GetRegNum(), temp1, temp2);
    }

    DefReg(node);
}

void CodeGen::genCodeForBinary(GenTreeOp* treeNode)
{
    assert(varTypeIsIntegralOrI(treeNode->GetType()));

    const genTreeOps oper       = treeNode->OperGet();
    regNumber        targetReg  = treeNode->GetRegNum();
    var_types        targetType = treeNode->TypeGet();
    emitter*         emit       = GetEmitter();

    assert(oper == GT_ADD || oper == GT_SUB || oper == GT_MUL || oper == GT_ADD_LO || oper == GT_ADD_HI ||
           oper == GT_SUB_LO || oper == GT_SUB_HI || oper == GT_OR || oper == GT_XOR || oper == GT_AND);

    GenTree* op1 = treeNode->gtGetOp1();
    GenTree* op2 = treeNode->gtGetOp2();

    if (op1->isUsedFromReg())
    {
        UseReg(op1);
    }

    if (op2->isUsedFromReg())
    {
        UseReg(op2);
    }

    assert(IsValidSourceType(targetType, op1->GetType()));
    assert(IsValidSourceType(targetType, op2->GetType()));

    instruction ins = genGetInsForOper(oper);

    // The arithmetic node must be sitting in a register (since it's not contained)
    noway_assert(targetReg != REG_NA);

    if ((oper == GT_ADD_LO || oper == GT_SUB_LO))
    {
        // During decomposition, all operands become reg
        assert(!op1->isContained() && !op2->isContained());
        emit->emitIns_R_R_R(ins, emitTypeSize(treeNode), treeNode->GetRegNum(), op1->GetRegNum(), op2->GetRegNum(),
                            INS_FLAGS_SET);
    }
    else
    {
        regNumber r = emitInsTernary(ins, emitTypeSize(treeNode), treeNode, op1, op2);
        assert(r == targetReg);
    }

    DefReg(treeNode);
}

//--------------------------------------------------------------------------------------
// genLclHeap: Generate code for localloc
//
// Description:
//      There are 2 ways depending from build version to generate code for localloc:
//          1) For debug build where memory should be initialized we generate loop
//             which invoke push {tmpReg} N times.
//          2) For non-debug build, we tickle the pages to ensure that SP is always
//             valid and is in sync with the "stack guard page". Amount of iteration
//             is N/eeGetPageSize().
//
// Comments:
//      There can be some optimization:
//          1) It's not needed to generate loop for zero size allocation
//          2) For small allocation (less than 4 store) we unroll loop
//          3) For allocation less than eeGetPageSize() and when it's not needed to initialize
//             memory to zero, we can just decrement SP.
//
// Notes: Size N should be aligned to STACK_ALIGN before any allocation
//
void CodeGen::genLclHeap(GenTree* tree)
{
    assert(tree->OperGet() == GT_LCLHEAP);
    assert(compiler->compLocallocUsed);

    GenTree* size = tree->AsOp()->gtOp1;
    noway_assert((genActualType(size->gtType) == TYP_INT) || (genActualType(size->gtType) == TYP_I_IMPL));

    // Result of localloc will be returned in regCnt.
    // Also it used as temporary register in code generation
    // for storing allocation size
    regNumber            regCnt                   = tree->GetRegNum();
    var_types            type                     = genActualType(size->gtType);
    emitAttr             easz                     = emitTypeSize(type);
    BasicBlock*          endLabel                 = nullptr;
    unsigned             stackAdjustment          = 0;
    regNumber            regTmp                   = REG_NA;
    const target_ssize_t ILLEGAL_LAST_TOUCH_DELTA = (target_ssize_t)-1;
    target_ssize_t       lastTouchDelta =
        ILLEGAL_LAST_TOUCH_DELTA; // The number of bytes from SP to the last stack address probed.

    noway_assert(isFramePointerUsed()); // localloc requires Frame Pointer to be established since SP changes
#if !FEATURE_FIXED_OUT_ARGS
    noway_assert(genStackLevel == 0); // Can't have anything on the stack
#endif

    if (GenTreeIntCon* intCon = size->IsIntCon())
    {
        assert(intCon->isContained());
        assert(intCon->GetValue() != 0);
    }
    else
    {
        // If 0 bail out by returning null in regCnt
        genConsumeReg(size);
        genCopyRegIfNeeded(size, regCnt);
        endLabel = genCreateTempLabel();
        GetEmitter()->emitIns_R_R(INS_tst, easz, regCnt, regCnt);
        inst_JMP(EJ_eq, endLabel);
    }

    // Setup the regTmp, if there is one.
    if (tree->AvailableTempRegCount() > 0)
    {
        regTmp = tree->ExtractTempReg();
    }

    // If we have an outgoing arg area then we must adjust the SP by popping off the
    // outgoing arg area. We will restore it right before we return from this method.
    if (outgoingArgSpaceSize > 0)
    {
        // This must be true for the stack to remain aligned
        assert(outgoingArgSpaceSize % STACK_ALIGN == 0);

        // We're guaranteed (by LinearScan::BuildLclHeap()) to have a legal regTmp if we need one.
        genStackPointerAdjustment(outgoingArgSpaceSize, regTmp);

        stackAdjustment += outgoingArgSpaceSize;
    }

    // Put aligned allocation size to regCnt
    if (size->IsCnsIntOrI())
    {
        // 'amount' is the total number of bytes to localloc to properly STACK_ALIGN
        target_size_t amount = (target_size_t)size->AsIntCon()->gtIconVal;
        amount               = AlignUp(amount, STACK_ALIGN);

        // For small allocations we will generate up to four push instructions (either 2 or 4, exactly,
        // since STACK_ALIGN is 8, and REGSIZE_BYTES is 4).
        static_assert_no_msg(STACK_ALIGN == (REGSIZE_BYTES * 2));
        assert(amount % REGSIZE_BYTES == 0);
        target_size_t pushCount = amount / REGSIZE_BYTES;
        if (pushCount <= 4)
        {
            GetEmitter()->emitIns_R_I(INS_mov, EA_4BYTE, regCnt, 0);

            while (pushCount != 0)
            {
                inst_IV(INS_push, (unsigned)genRegMask(regCnt));
                pushCount -= 1;
            }

            lastTouchDelta = 0;

            goto ALLOC_DONE;
        }
        else if (!compiler->info.compInitMem && (amount < compiler->eeGetPageSize())) // must be < not <=
        {
            // Since the size is less than a page, simply adjust the SP value.
            // The SP might already be in the guard page, must touch it BEFORE
            // the alloc, not after.
            GetEmitter()->emitIns_R_R_I(INS_ldr, EA_4BYTE, regCnt, REG_SP, 0);
            inst_RV_IV(INS_sub, REG_SP, amount, EA_PTRSIZE);

            lastTouchDelta = amount;

            goto ALLOC_DONE;
        }

        // regCnt will be the total number of bytes to locAlloc
        instGen_Set_Reg_To_Imm(EA_4BYTE, regCnt, amount);
    }
    else
    {
        // Round up the number of bytes to allocate to a STACK_ALIGN boundary.
        inst_RV_IV(INS_add, regCnt, (STACK_ALIGN - 1), emitActualTypeSize(type));
        inst_RV_IV(INS_and, regCnt, ~(STACK_ALIGN - 1), emitActualTypeSize(type));
    }

    // Allocation
    if (compiler->info.compInitMem)
    {
        // At this point 'regCnt' is set to the total number of bytes to localloc.
        // Since we have to zero out the allocated memory AND ensure that the stack pointer is always valid
        // by tickling the pages, we will just push 0's on the stack.

        GetEmitter()->emitIns_R_I(INS_mov, EA_4BYTE, regTmp, 0);

        // Loop:
        BasicBlock* loop = genCreateTempLabel();
        genDefineTempLabel(loop);

        noway_assert(STACK_ALIGN == 8);
        inst_IV(INS_push, (unsigned)genRegMask(regTmp));
        inst_IV(INS_push, (unsigned)genRegMask(regTmp));

        // If not done, loop
        // Note that regCnt is the number of bytes to stack allocate.
        assert(genIsValidIntReg(regCnt));
        GetEmitter()->emitIns_R_I(INS_sub, EA_PTRSIZE, regCnt, STACK_ALIGN, INS_FLAGS_SET);
        inst_JMP(EJ_ne, loop);

        lastTouchDelta = 0;
    }
    else
    {
        // At this point 'regCnt' is set to the total number of bytes to locAlloc.
        //
        // We don't need to zero out the allocated memory. However, we do have
        // to tickle the pages to ensure that SP is always valid and is
        // in sync with the "stack guard page".  Note that in the worst
        // case SP is on the last byte of the guard page.  Thus you must
        // touch SP-0 first not SP-0x1000.
        //
        // Another subtlety is that you don't want SP to be exactly on the
        // boundary of the guard page because PUSH is predecrement, thus
        // call setup would not touch the guard page but just beyond it
        //
        // Note that we go through a few hoops so that SP never points to
        // illegal pages at any time during the tickling process
        //
        //       subs  regCnt, SP, regCnt      // regCnt now holds ultimate SP
        //       bvc   Loop                    // result is smaller than original SP (no wrap around)
        //       mov   regCnt, #0              // Overflow, pick lowest possible value
        //
        //  Loop:
        //       ldr   regTmp, [SP + 0]        // tickle the page - read from the page
        //       sub   regTmp, SP, PAGE_SIZE   // decrement SP by eeGetPageSize()
        //       cmp   regTmp, regCnt
        //       jb    Done
        //       mov   SP, regTmp
        //       j     Loop
        //
        //  Done:
        //       mov   SP, regCnt
        //

        BasicBlock* loop = genCreateTempLabel();
        BasicBlock* done = genCreateTempLabel();

        //       subs  regCnt, SP, regCnt      // regCnt now holds ultimate SP
        GetEmitter()->emitIns_R_R_R(INS_sub, EA_PTRSIZE, regCnt, REG_SPBASE, regCnt, INS_FLAGS_SET);

        inst_JMP(EJ_vc, loop); // branch if the V flag is not set

        // Overflow, set regCnt to lowest possible value
        GetEmitter()->emitIns_R_I(INS_mov, EA_4BYTE, regCnt, 0);

        genDefineTempLabel(loop);

        // tickle the page - Read from the updated SP - this triggers a page fault when on the guard page
        GetEmitter()->emitIns_R_R_I(INS_ldr, EA_4BYTE, regTmp, REG_SPBASE, 0);

        // decrement SP by eeGetPageSize()
        GetEmitter()->emitIns_R_R_I(INS_sub, EA_PTRSIZE, regTmp, REG_SPBASE, compiler->eeGetPageSize());

        GetEmitter()->emitIns_R_R(INS_cmp, EA_PTRSIZE, regTmp, regCnt);
        inst_JMP(EJ_lo, done);

        // Update SP to be at the next page of stack that we will tickle
        GetEmitter()->emitIns_Mov(INS_mov, EA_PTRSIZE, REG_SPBASE, regTmp, /* canSkip */ false);

        // Jump to loop and tickle new stack address
        inst_JMP(EJ_jmp, loop);

        // Done with stack tickle loop
        genDefineTempLabel(done);

        // Now just move the final value to SP
        GetEmitter()->emitIns_Mov(INS_mov, EA_PTRSIZE, REG_SPBASE, regCnt, /* canSkip */ false);

        // lastTouchDelta is dynamic, and can be up to a page. So if we have outgoing arg space,
        // we're going to assume the worst and probe.
    }

ALLOC_DONE:
    // Re-adjust SP to allocate outgoing arg area. We must probe this adjustment.
    if (stackAdjustment != 0)
    {
        assert((stackAdjustment % STACK_ALIGN) == 0); // This must be true for the stack to remain aligned
        assert((lastTouchDelta == ILLEGAL_LAST_TOUCH_DELTA) || (lastTouchDelta >= 0));

        if ((lastTouchDelta == ILLEGAL_LAST_TOUCH_DELTA) ||
            (stackAdjustment + (unsigned)lastTouchDelta + STACK_PROBE_BOUNDARY_THRESHOLD_BYTES >
             compiler->eeGetPageSize()))
        {
            genStackPointerConstantAdjustmentLoopWithProbe(-(ssize_t)stackAdjustment, regTmp);
        }
        else
        {
            genStackPointerConstantAdjustment(-(ssize_t)stackAdjustment, regTmp);
        }

        // Return the stackalloc'ed address in result register.
        // regCnt = SP + stackAdjustment.
        genInstrWithConstant(INS_add, EA_PTRSIZE, regCnt, REG_SPBASE, (ssize_t)stackAdjustment, regTmp);
    }
    else // stackAdjustment == 0
    {
        // Move the final value of SP to regCnt
        inst_Mov(TYP_I_IMPL, regCnt, REG_SPBASE, /* canSkip */ false);
    }

    if (endLabel != nullptr)
    {
        genDefineTempLabel(endLabel);
    }

    genProduceReg(tree);
}

//------------------------------------------------------------------------
// genTableBasedSwitch: generate code for a switch statement based on a table of ip-relative offsets
//
void CodeGen::genTableBasedSwitch(GenTreeOp* treeNode)
{
    regNumber idxReg  = UseReg(treeNode->GetOp(0));
    regNumber baseReg = UseReg(treeNode->GetOp(1));

    GetEmitter()->emitIns_R_ARX(INS_ldr, EA_4BYTE, REG_PC, baseReg, idxReg, TARGET_POINTER_SIZE, 0);
}

void CodeGen::GenJmpTable(GenTree* node, BasicBlock* switchBlock)
{
    assert(switchBlock->bbJumpKind == BBJ_SWITCH);
    assert(node->OperIs(GT_JMPTABLE));

    unsigned     jumpCount  = switchBlock->bbJumpSwt->bbsCount;
    BasicBlock** jumpTable  = switchBlock->bbJumpSwt->bbsDstTab;
    unsigned     jmpTabBase = GetEmitter()->emitBBTableDataGenBeg(jumpCount, false);

    JITDUMP("\n      J_M%03u_DS%02u LABEL   DWORD\n", compiler->compMethodID, jmpTabBase);

    for (unsigned i = 0; i < jumpCount; i++)
    {
        BasicBlock* target = *jumpTable++;
        noway_assert(target->bbFlags & BBF_HAS_LABEL);

        JITDUMP("            DD      L_M%03u_" FMT_BB "\n", compiler->compMethodID, target->bbNum);

        GetEmitter()->emitDataGenData(i, target);
    }

    GetEmitter()->emitDataGenEnd();

    genMov32RelocatableDataLabel(jmpTabBase, node->GetRegNum());
    DefReg(node);
}

instruction CodeGen::genGetInsForOper(genTreeOps oper)
{
    switch (oper)
    {
        case GT_ADD:
            return INS_add;
        case GT_AND:
            return INS_and;
        case GT_MUL:
            return INS_mul;
#ifndef USE_HELPERS_FOR_INT_DIV
        case GT_DIV:
            return INS_sdiv;
#endif
        case GT_LSH:
            return INS_lsl;
        case GT_NEG:
            return INS_rsb;
        case GT_NOT:
            return INS_mvn;
        case GT_OR:
            return INS_orr;
        case GT_RSH:
            return INS_asr;
        case GT_RSZ:
            return INS_lsr;
        case GT_SUB:
            return INS_sub;
        case GT_XOR:
            return INS_eor;
        case GT_ROR:
            return INS_ror;
        case GT_ADD_LO:
            return INS_add;
        case GT_ADD_HI:
            return INS_adc;
        case GT_SUB_LO:
            return INS_sub;
        case GT_SUB_HI:
            return INS_sbc;
        case GT_LSH_HI:
            return INS_lsl;
        case GT_RSH_LO:
            return INS_lsr;
        default:
            unreached();
    }
}

//------------------------------------------------------------------------
// genCodeForShiftLong: Generates the code sequence for a GenTree node that
// represents a three operand bit shift or rotate operation (<<Hi, >>Lo).
//
// Arguments:
//    tree - the bit shift node (that specifies the type of bit shift to perform).
//
// Assumptions:
//    a) All GenTrees are register allocated.
//    b) The shift-by-amount in tree->AsOp()->gtOp2 is a contained constant
//
void CodeGen::genCodeForShiftLong(GenTree* tree)
{
    // Only the non-RMW case here.
    genTreeOps oper = tree->OperGet();
    assert(oper == GT_LSH_HI || oper == GT_RSH_LO);

    GenTree* operand = tree->AsOp()->gtOp1;
    assert(operand->OperGet() == GT_LONG);

    GenTree* operandLo = operand->gtGetOp1();
    GenTree* operandHi = operand->gtGetOp2();

    regNumber regLo  = UseReg(operandLo);
    regNumber regHi  = UseReg(operandHi);
    regNumber dstReg = tree->GetRegNum();

    var_types   targetType = tree->TypeGet();
    instruction ins        = genGetInsForOper(oper);

    GenTree* shiftBy = tree->gtGetOp2();

    assert(shiftBy->isContainedIntOrIImmed());

    unsigned count = (unsigned)shiftBy->AsIntConCommon()->IconValue();

    regNumber regResult = (oper == GT_LSH_HI) ? regHi : regLo;

    inst_Mov(targetType, dstReg, regResult, /* canSkip */ true);

    if (oper == GT_LSH_HI)
    {
        GetEmitter()->emitIns_R_I(ins, EA_4BYTE, dstReg, count & 31);
        GetEmitter()->emitIns_R_R_R_I(INS_orr, EA_4BYTE, dstReg, dstReg, regLo, 32 - count, INS_FLAGS_DONT_CARE,
                                      INS_OPTS_LSR);
    }
    else
    {
        assert(oper == GT_RSH_LO);
        GetEmitter()->emitIns_R_I(ins, EA_4BYTE, dstReg, count & 31);
        GetEmitter()->emitIns_R_R_R_I(INS_orr, EA_4BYTE, dstReg, dstReg, regHi, 32 - count, INS_FLAGS_DONT_CARE,
                                      INS_OPTS_LSL);
    }

    DefReg(tree);
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

    var_types   type = lcl->GetRegisterType(load);
    instruction ins  = ins_Load(type);
    emitAttr    attr = emitTypeSize(type);

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
    else
    {
        assert(IsValidSourceType(type, src->GetType()));

        regNumber srcReg  = genConsumeReg(src);
        unsigned  lclOffs = store->GetLclOffs();
        unsigned  lclNum  = store->GetLclNum();
        emitter*  emit    = GetEmitter();

        if (store->IsOffsetMisaligned())
        {
            // ARM supports unaligned access only for integer types,
            // use integer stores if the field is not aligned.

            regNumber addrReg = store->ExtractTempReg();
            emit->emitIns_R_S(INS_lea, EA_PTRSIZE, addrReg, lclNum, lclOffs);

            if (type == TYP_FLOAT)
            {
                regNumber tempReg = store->GetSingleTempReg();

                emit->emitIns_R_R(INS_vmov_f2i, EA_4BYTE, tempReg, srcReg);
                emit->emitIns_R_R(INS_str, EA_4BYTE, tempReg, addrReg);
            }
            else
            {
                regNumber tempRegLo = store->ExtractTempReg();
                regNumber tempRegHi = store->GetSingleTempReg();

                emit->emitIns_R_R_R(INS_vmov_d2i, EA_8BYTE, tempRegLo, tempRegHi, srcReg);
                emit->emitIns_R_R_I(INS_str, EA_4BYTE, tempRegLo, addrReg, 0);
                emit->emitIns_R_R_I(INS_str, EA_4BYTE, tempRegHi, addrReg, 4);
            }
        }
        else
        {
            emit->emitIns_S_R(ins_Store(type), emitTypeSize(type), srcReg, lclNum, lclOffs);
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

    if (store->TypeIs(TYP_LONG))
    {
        GenStoreLclVarLong(store);
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

    var_types lclRegType = lcl->GetRegisterType(store);

    regNumber srcReg = UseReg(src);
    regNumber dstReg = store->GetRegNum();

    if (dstReg == REG_NA)
    {
        unsigned lclNum = store->GetLclNum();
        GetEmitter()->emitIns_S_R(ins_Store(lclRegType), emitTypeSize(lclRegType), srcReg, lclNum, 0);
        genUpdateLife(store);
        lcl->SetRegNum(REG_STK);

        return;
    }

    if ((dstReg != srcReg) || (varActualType(lclRegType) != varActualType(src->GetType())))
    {
        GetEmitter()->emitIns_Mov(ins_Copy(lclRegType), emitActualTypeSize(lclRegType), dstReg, srcReg,
                                  /*canSkip*/ true);
    }

    DefLclVarReg(store);
}

//------------------------------------------------------------------------
// genCkfinite: Generate code for ckfinite opcode.
//
// Arguments:
//    treeNode - The GT_CKFINITE node
//
// Return Value:
//    None.
//
// Assumptions:
//    GT_CKFINITE node has reserved an internal register.
//
void CodeGen::genCkfinite(GenTree* treeNode)
{
    assert(treeNode->OperIs(GT_CKFINITE));

    emitter*  emit       = GetEmitter();
    var_types targetType = treeNode->GetType();
    regNumber intReg     = treeNode->GetSingleTempReg();
    regNumber fpReg      = genConsumeReg(treeNode->AsUnOp()->GetOp(0));
    regNumber targetReg  = treeNode->GetRegNum();

    // Extract and sign-extend the exponent into an integer register
    if (targetType == TYP_FLOAT)
    {
        emit->emitIns_Mov(INS_vmov_f2i, EA_4BYTE, intReg, fpReg, /* canSkip */ false);
        emit->emitIns_R_R_I_I(INS_sbfx, EA_4BYTE, intReg, intReg, 23, 8);
    }
    else
    {
        assert(targetType == TYP_DOUBLE);
        emit->emitIns_Mov(INS_vmov_f2i, EA_4BYTE, intReg, REG_NEXT(fpReg), /* canSkip */ false);
        emit->emitIns_R_R_I_I(INS_sbfx, EA_4BYTE, intReg, intReg, 20, 11);
    }

    // If exponent is all 1's, throw ArithmeticException
    emit->emitIns_R_I(INS_add, EA_4BYTE, intReg, 1, INS_FLAGS_SET);
    genJumpToThrowHlpBlk(EJ_eq, ThrowHelperKind::Arithmetic);

    // If it's a finite value, copy it to targetReg
    inst_Mov(targetType, targetReg, fpReg, /* canSkip */ true);

    genProduceReg(treeNode);
}

void CodeGen::GenCompare(GenTreeOp* cmp)
{
    // TODO-ARM-CQ: Check if we can use the currently set flags.
    // TODO-ARM-CQ: Check for the case where we can simply transfer the carry bit to a register
    //         (signed < or >= where targetReg != REG_NA)

    GenTree*  op1   = cmp->GetOp(0);
    GenTree*  op2   = cmp->GetOp(1);
    var_types type1 = op1->GetType();
    var_types type2 = op2->GetType();
    regNumber reg1  = UseReg(op1);
    regNumber reg2  = op2->isContained() ? REG_NA : UseReg(op2);

    assert(!varTypeIsLong(type1));
    assert(!varTypeIsLong(type2));

    emitter* emit = GetEmitter();

    if (varTypeIsFloating(type1))
    {
        assert(type1 == type2);
        assert(!cmp->OperIs(GT_CMP));

        emit->emitIns_R_R(INS_vcmp, emitTypeSize(type1), op1->GetRegNum(), op2->GetRegNum());
        // vmrs with register 0xf has special meaning of transferring flags
        emit->emitIns_R(INS_vmrs, EA_4BYTE, REG_R15);
    }
    else if (GenTreeIntCon* imm = op2->IsContainedIntCon())
    {
        emit->emitIns_R_I(INS_cmp, EA_4BYTE, op1->GetRegNum(), imm->GetInt32Value());
    }
    else
    {
        emit->emitIns_R_R(INS_cmp, EA_4BYTE, op1->GetRegNum(), op2->GetRegNum());
    }

    if (cmp->GetRegNum() == REG_NA)
    {
        return;
    }

    inst_SETCC(GenCondition::FromRelop(cmp), cmp->GetType(), cmp->GetRegNum());
    DefReg(cmp);
}

//------------------------------------------------------------------------
// genCodeForReturnTrap: Produce code for a GT_RETURNTRAP node.
//
// Arguments:
//    tree - the GT_RETURNTRAP node
//
void CodeGen::genCodeForReturnTrap(GenTreeOp* tree)
{
    assert(tree->OperGet() == GT_RETURNTRAP);

    // this is nothing but a conditional call to CORINFO_HELP_STOP_FOR_GC
    // based on the contents of 'data'

    GenTree* data = tree->GetOp(0);
    GetEmitter()->emitIns_R_I(INS_cmp, EA_4BYTE, UseReg(data), 0);

    BasicBlock* skipLabel = genCreateTempLabel();

    inst_JMP(EJ_eq, skipLabel);

    // emit the call to the EE-helper that stops for GC (or other reasons)

    genEmitHelperCall(CORINFO_HELP_STOP_FOR_GC);
    genDefineTempLabel(skipLabel);
}

void CodeGen::genCodeForNullCheck(GenTreeIndir* tree)
{
    assert(tree->OperIs(GT_NULLCHECK));
    assert(!"GT_NULLCHECK isn't supported for Arm32; use GT_IND.");
}

void CodeGen::genCodeForIndir(GenTreeIndir* load)
{
    assert(load->OperIs(GT_IND));

    genConsumeAddress(load->GetAddr());
    emitInsLoad(ins_Load(load->GetType()), emitActualTypeSize(load->GetType()), load->GetRegNum(), load);

    if (load->IsVolatile())
    {
        instGen_MemoryBarrier(BARRIER_LOAD_ONLY);
    }

    DefReg(load);
}

void CodeGen::genCodeForStoreInd(GenTreeStoreInd* tree)
{
    GenTree*  addr = tree->GetAddr();
    GenTree*  data = tree->GetValue();
    var_types type = tree->GetType();

    assert(IsValidSourceType(type, data->GetType()));

    GCInfo::WriteBarrierForm writeBarrierForm = GCInfo::GetWriteBarrierForm(tree);
    if (writeBarrierForm != GCInfo::WBF_NoBarrier)
    {
        regNumber addrReg = UseReg(addr);
        regNumber dataReg = UseReg(data);

        // At this point, we should not have any interference.
        // That is, 'data' must not be in REG_ARG_0,
        // as that is where 'addr' must go.
        noway_assert(dataReg != REG_ARG_0);

        inst_Mov(addr->GetType(), REG_ARG_0, addrReg, /* canSkip */ true);
        inst_Mov(data->GetType(), REG_ARG_1, dataReg, /* canSkip */ true);
        genGCWriteBarrier(tree, writeBarrierForm);

        return;
    }

    // We must consume the operands in the proper execution order,
    // so that liveness is updated appropriately.
    genConsumeAddress(addr);
    regNumber dataReg = UseReg(data);

    if ((tree->gtFlags & GTF_IND_VOLATILE) != 0)
    {
        // issue a full memory barrier a before volatile StInd
        instGen_MemoryBarrier();
    }

    emitInsStore(ins_Store(type), emitActualTypeSize(type), dataReg, tree);
}

// genLongToIntCast: Generate code for long to int casts.
//
// Arguments:
//    cast - The GT_CAST node
//
// Return Value:
//    None.
//
// Assumptions:
//    The cast node and its sources (via GT_LONG) must have been assigned registers.
//    The destination cannot be a floating point type or a small integer type.
//
void CodeGen::genLongToIntCast(GenTreeCast* cast)
{
    GenTreeOp* src = cast->GetOp(0)->AsOp();
    noway_assert(src->OperIs(GT_LONG));

    var_types srcType  = cast->IsUnsigned() ? TYP_ULONG : TYP_LONG;
    var_types dstType  = cast->GetCastType();
    regNumber loSrcReg = UseReg(src->GetOp(0));
    regNumber hiSrcReg = UseReg(src->GetOp(1));
    regNumber dstReg   = cast->GetRegNum();

    assert((dstType == TYP_INT) || (dstType == TYP_UINT));
    assert(genIsValidIntReg(loSrcReg));
    assert(genIsValidIntReg(hiSrcReg));
    assert(genIsValidIntReg(dstReg));

    if (cast->gtOverflow())
    {
        //
        // Generate an overflow check for [u]long to [u]int casts:
        //
        // long  -> int  - check if the upper 33 bits are all 0 or all 1
        //
        // ulong -> int  - check if the upper 33 bits are all 0
        //
        // long  -> uint - check if the upper 32 bits are all 0
        // ulong -> uint - check if the upper 32 bits are all 0
        //

        if ((srcType == TYP_LONG) && (dstType == TYP_INT))
        {
            BasicBlock* allOne  = genCreateTempLabel();
            BasicBlock* success = genCreateTempLabel();

            inst_RV_RV(INS_tst, loSrcReg, loSrcReg, TYP_INT);
            inst_JMP(EJ_mi, allOne);
            inst_RV_RV(INS_tst, hiSrcReg, hiSrcReg, TYP_INT);
            genJumpToThrowHlpBlk(EJ_ne, ThrowHelperKind::Overflow);
            inst_JMP(EJ_jmp, success);

            genDefineTempLabel(allOne);
            inst_RV_IV(INS_cmp, hiSrcReg, -1, EA_4BYTE);
            genJumpToThrowHlpBlk(EJ_ne, ThrowHelperKind::Overflow);

            genDefineTempLabel(success);
        }
        else
        {
            if ((srcType == TYP_ULONG) && (dstType == TYP_INT))
            {
                inst_RV_RV(INS_tst, loSrcReg, loSrcReg, TYP_INT);
                genJumpToThrowHlpBlk(EJ_mi, ThrowHelperKind::Overflow);
            }

            inst_RV_RV(INS_tst, hiSrcReg, hiSrcReg, TYP_INT);
            genJumpToThrowHlpBlk(EJ_ne, ThrowHelperKind::Overflow);
        }
    }

    inst_Mov(TYP_INT, dstReg, loSrcReg, /* canSkip */ true);

    DefReg(cast);
}

//------------------------------------------------------------------------
// genIntToFloatCast: Generate code to cast an int to float/double
//
// Arguments:
//    cast - The GT_CAST node
//
void CodeGen::genIntToFloatCast(GenTreeCast* cast)
{
    assert(!cast->gtOverflow());

    GenTree*  src     = cast->GetOp(0);
    var_types srcType = varActualType(src->GetType());
    var_types dstType = cast->GetCastType();

    noway_assert(srcType == TYP_INT);
    assert((dstType == TYP_FLOAT) || (dstType == TYP_DOUBLE));
    assert(cast->GetType() == dstType);

    regNumber srcReg = genConsumeReg(src);
    regNumber dstReg = cast->GetRegNum();

    assert(genIsValidIntReg(srcReg) && genIsValidFloatReg(dstReg));

    instruction ins;

    if (dstType == TYP_DOUBLE)
    {
        ins = cast->IsUnsigned() ? INS_vcvt_u2d : INS_vcvt_i2d;
    }
    else
    {
        ins = cast->IsUnsigned() ? INS_vcvt_u2f : INS_vcvt_i2f;
    }

    GetEmitter()->emitIns_Mov(INS_vmov_i2f, EA_4BYTE, dstReg, srcReg, /* canSkip */ false);
    GetEmitter()->emitIns_R_R(ins, EA_4BYTE, dstReg, dstReg);

    genProduceReg(cast);
}

//------------------------------------------------------------------------
// genFloatToIntCast: Generate code to cast float/double to int
//
// Arguments:
//    cast - The GT_CAST node
//
void CodeGen::genFloatToIntCast(GenTreeCast* cast)
{
    assert(!cast->gtOverflow());

    GenTree*  src     = cast->GetOp(0);
    var_types srcType = src->GetType();
    var_types dstType = cast->GetCastType();

    assert((srcType == TYP_FLOAT) || (srcType == TYP_DOUBLE));
    noway_assert((dstType == TYP_INT) || (dstType == TYP_UINT));
    assert(cast->GetType() == TYP_INT);

    regNumber srcReg = genConsumeReg(src);
    regNumber dstReg = cast->GetRegNum();
    regNumber tmpReg = cast->GetSingleTempReg();

    assert(genIsValidFloatReg(srcReg) && genIsValidFloatReg(tmpReg) && genIsValidIntReg(dstReg));

    instruction ins;

    if (srcType == TYP_DOUBLE)
    {
        ins = varTypeIsUnsigned(dstType) ? INS_vcvt_d2u : INS_vcvt_d2i;
    }
    else
    {
        ins = varTypeIsUnsigned(dstType) ? INS_vcvt_f2u : INS_vcvt_f2i;
    }

    GetEmitter()->emitIns_R_R(ins, EA_4BYTE, tmpReg, srcReg);
    GetEmitter()->emitIns_Mov(INS_vmov_f2i, EA_4BYTE, dstReg, tmpReg, false);

    genProduceReg(cast);
}

void CodeGen::genEmitHelperCall(CorInfoHelpFunc helper, emitAttr retSize, regNumber callTargetReg /*= REG_NA */)
{
    // Can we call the helper function directly

    void *addr = NULL, **pAddr = NULL;

#if defined(DEBUG) && defined(PROFILING_SUPPORTED)
    // Don't ask VM if it hasn't requested ELT hooks
    if (!compiler->compProfilerHookNeeded && compiler->opts.compJitELTHookEnabled &&
        (helper == CORINFO_HELP_PROF_FCN_ENTER || helper == CORINFO_HELP_PROF_FCN_LEAVE ||
         helper == CORINFO_HELP_PROF_FCN_TAILCALL))
    {
        addr = compiler->compProfilerMethHnd;
    }
    else
#endif
    {
        addr = compiler->compGetHelperFtn(helper, (void**)&pAddr);
    }

    emitter::EmitCallType callKind;
    void*                 callAddr;

    if ((addr == nullptr) || !emitter::validImmForBL(reinterpret_cast<ssize_t>(addr), compiler))
    {
        if (callTargetReg == REG_NA)
        {
            // If a callTargetReg has not been explicitly provided, we will use REG_DEFAULT_HELPER_CALL_TARGET, but
            // this is only a valid assumption if the helper call is known to kill REG_DEFAULT_HELPER_CALL_TARGET.
            callTargetReg = REG_DEFAULT_HELPER_CALL_TARGET;
        }

        // Load the address into a register and call through a register
        if (addr != nullptr)
        {
            instGen_Set_Reg_To_Imm(EA_HANDLE_CNS_RELOC, callTargetReg, reinterpret_cast<ssize_t>(addr));
        }
        else
        {
            GetEmitter()->emitIns_R_AI(INS_ldr, EA_PTR_DSP_RELOC, callTargetReg, reinterpret_cast<ssize_t>(pAddr));
        }

        callKind = emitter::EC_INDIR_R;
        callAddr = nullptr;
    }
    else
    {
        callKind      = emitter::EC_FUNC_TOKEN;
        callAddr      = addr;
        callTargetReg = REG_NA;
    }

    // clang-format off
    GetEmitter()->emitIns_Call(
        callKind,
        Compiler::eeFindHelper(helper)
        DEBUGARG(nullptr),
        callAddr,
        retSize,
        callTargetReg,
        false);
    // clang-format on
}

//------------------------------------------------------------------------
// genCodeForMulLong: Generates code for int*int->long multiplication
//
// Arguments:
//    node - the GT_MUL_LONG node
//
// Return Value:
//    None.
//
void CodeGen::genCodeForMulLong(GenTreeOp* node)
{
    assert(node->OperGet() == GT_MUL_LONG);

    regNumber srcReg1 = UseReg(node->GetOp(0));
    regNumber srcReg2 = UseReg(node->GetOp(1));
    regNumber dstReg1 = node->GetRegNum(0);
    regNumber dstReg2 = node->GetRegNum(1);

    instruction ins = node->IsUnsigned() ? INS_umull : INS_smull;
    GetEmitter()->emitIns_R_R_R_R(ins, EA_4BYTE, dstReg1, dstReg2, srcReg1, srcReg2);

    DefLongRegs(node);
}

void CodeGen::genFloatReturn(GenTree* src)
{
    assert(compiler->opts.compUseSoftFP || compiler->info.compIsVarArgs);

    regNumber srcReg = genConsumeReg(src);

    if (src->TypeIs(TYP_FLOAT))
    {
        GetEmitter()->emitIns_Mov(INS_vmov_f2i, EA_4BYTE, REG_R0, srcReg, /* canSkip */ false);
    }
    else
    {
        assert(src->TypeIs(TYP_DOUBLE));
        GetEmitter()->emitIns_R_R_R(INS_vmov_d2i, EA_8BYTE, REG_R0, REG_R1, srcReg);
    }
}

#ifdef PROFILING_SUPPORTED

//-----------------------------------------------------------------------------------
// PrologProfilingEnterCallback: Generate the profiling function enter callback.
//
// Arguments:
//     initReg        - register to use as scratch register
//     pInitRegZeroed - OUT parameter. *pInitRegZeroed set to 'false' if 'initReg' is
//                      not zero after this call.
//
// Return Value:
//     None
//
void CodeGen::PrologProfilingEnterCallback(regNumber initReg, bool* pInitRegZeroed)
{
    assert(generatingProlog);

    // Give profiler a chance to back out of hooking this method
    if (!compiler->compIsProfilerHookNeeded())
    {
        return;
    }

    // On Arm arguments are prespilled on stack, which frees r0-r3.
    // For generating Enter callout we would need two registers and one of them has to be r0 to pass profiler handle.
    // The call target register could be any free register.
    regNumber argReg = REG_PROFILER_ENTER_ARG;

    assert((preSpillParamRegs & genRegMask(argReg)) != RBM_NONE);

    if (compiler->compProfilerMethHndIndirected)
    {
        GetEmitter()->emitIns_R_AI(INS_ldr, EA_PTR_DSP_RELOC, argReg, (ssize_t)compiler->compProfilerMethHnd);
    }
    else
    {
        instGen_Set_Reg_To_Imm(EA_4BYTE, argReg, (ssize_t)compiler->compProfilerMethHnd);
    }

    genEmitHelperCall(CORINFO_HELP_PROF_FCN_ENTER);

    if (initReg == argReg)
    {
        *pInitRegZeroed = false;
    }
}

// Generate the profiling function leave or tailcall callback.
// Technically, this is not part of the epilog; it is called when we are generating code for a GT_RETURN node.
void CodeGen::genProfilingLeaveCallback(CorInfoHelpFunc helper)
{
    assert((helper == CORINFO_HELP_PROF_FCN_LEAVE) || (helper == CORINFO_HELP_PROF_FCN_TAILCALL));

    // Only hook if profiler says it's okay.
    if (!compiler->compIsProfilerHookNeeded())
    {
        return;
    }

    compiler->info.compProfilerCallback = true;

    //
    // Push the profilerHandle
    //

    // Contract between JIT and Profiler Leave callout on arm:
    // Return size <= 4 bytes: REG_PROFILER_RET_SCRATCH will contain return value
    // Return size > 4 and <= 8: <REG_PROFILER_RET_SCRATCH,r1> will contain return value.
    // Floating point or double or HFA return values will be in s0-s15 in case of non-vararg methods.
    // It is assumed that profiler Leave callback doesn't trash registers r1,REG_PROFILER_RET_SCRATCH and s0-s15.
    //
    // In the following cases r0 doesn't contain a return value and hence need not be preserved before emitting Leave
    // callback.
    bool     r0InUse;
    emitAttr attr = EA_UNKNOWN;

    if (helper == CORINFO_HELP_PROF_FCN_TAILCALL)
    {
        // For the tail call case, the helper call is introduced during lower,
        // so the allocator will arrange things so R0 is not in use here.
        //
        // For the tail jump case we expect to generate the profiler call
        // before register args are loaded so R0 should not be in use.
        r0InUse = false;
    }
    else
    {
        r0InUse =
            (compiler->info.retDesc.GetRegCount() > 0) && !varTypeUsesFloatReg(compiler->info.retDesc.GetRegType(0));
    }

    if (r0InUse)
    {
        if (varTypeIsGC(compiler->info.retDesc.GetRegType(0)))
        {
            attr = emitActualTypeSize(compiler->info.retDesc.GetRegType(0));
        }
        else if ((compiler->info.compRetBuffArg != BAD_VAR_NUM) && (compiler->info.retDesc.GetRegCount() != 0))
        {
            attr = EA_BYREF;
        }
        else
        {
            attr = EA_PTRSIZE;
        }
    }

    if (r0InUse)
    {
        // Has a return value and r0 is in use. For emitting Leave profiler callout we would need r0 for passing
        // profiler handle. Therefore, r0 is moved to REG_PROFILER_RETURN_SCRATCH as per contract.
        GetEmitter()->emitIns_Mov(INS_mov, attr, REG_PROFILER_RET_SCRATCH, REG_R0, /* canSkip */ false);
        liveness.TransferGCRegType(REG_PROFILER_RET_SCRATCH, REG_R0);
    }

    if (compiler->compProfilerMethHndIndirected)
    {
        GetEmitter()->emitIns_R_AI(INS_ldr, EA_PTR_DSP_RELOC, REG_R0, (ssize_t)compiler->compProfilerMethHnd);
    }
    else
    {
        instGen_Set_Reg_To_Imm(EA_PTRSIZE, REG_R0, (ssize_t)compiler->compProfilerMethHnd);
    }

    liveness.RemoveGCRegs(RBM_R0);

    genEmitHelperCall(helper);

    // Restore state that existed before profiler callback
    if (r0InUse)
    {
        GetEmitter()->emitIns_Mov(INS_mov, attr, REG_R0, REG_PROFILER_RET_SCRATCH, /* canSkip */ false);
        liveness.TransferGCRegType(REG_R0, REG_PROFILER_RET_SCRATCH);
        liveness.RemoveGCRegs(RBM_PROFILER_RET_SCRATCH);
    }
}

#endif // PROFILING_SUPPORTED

//------------------------------------------------------------------------
// PrologAllocLclFrame: Probe the stack and allocate the local stack frame - subtract from SP.
//
// Notes:
//      The first instruction of the prolog is always a push (which touches the lowest address
//      of the stack), either of the LR register or of some argument registers, e.g., in the case of
//      pre-spilling. The LR register is always pushed because we require it to allow for GC return
//      address hijacking (see the comment in CodeGen::PrologPushCalleeSavedRegisters()). These pushes
//      happen immediately before calling this function, so the SP at the current location has already
//      been touched.
//
// Arguments:
//      frameSize         - the size of the stack frame being allocated.
//      initReg           - register to use as a scratch register.
//      pInitRegZeroed    - OUT parameter. *pInitRegZeroed is set to 'false' if and only if
//                          this call sets 'initReg' to a non-zero value.
//      maskArgRegsLiveIn - incoming argument registers that are currently live.
//
// Return value:
//      None
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

    assert(!compiler->info.compPublishStubParam || (REG_SECRET_STUB_PARAM != initReg));

    if (frameSize < pageSize)
    {
        GetEmitter()->emitIns_R_I(INS_sub, EA_PTRSIZE, REG_SPBASE, frameSize);
    }
    else
    {
        genInstrWithConstant(INS_sub, EA_PTRSIZE, REG_STACK_PROBE_HELPER_ARG, REG_SPBASE, frameSize,
                             REG_STACK_PROBE_HELPER_ARG);
        genEmitHelperCall(CORINFO_HELP_STACK_PROBE, EA_UNKNOWN, REG_STACK_PROBE_HELPER_CALL_TARGET);
        compiler->unwindPadding();
        GetEmitter()->emitIns_Mov(INS_mov, EA_PTRSIZE, REG_SPBASE, REG_STACK_PROBE_HELPER_ARG, /* canSkip */ false);

        if ((genRegMask(initReg) & (RBM_STACK_PROBE_HELPER_ARG | RBM_STACK_PROBE_HELPER_CALL_TARGET |
                                    RBM_STACK_PROBE_HELPER_TRASH)) != RBM_NONE)
        {
            *pInitRegZeroed = false;
        }
    }

    compiler->unwindAllocStack(frameSize);
#ifdef USING_SCOPE_INFO
    if (!IsFramePointerRequired())
    {
        psiAdjustStackLevel(frameSize);
    }
#endif // USING_SCOPE_INFO
}

void CodeGen::PrologEstablishFramePointer(int delta, bool reportUnwindData)
{
    assert(emitter::emitIns_valid_imm_for_add_sp(delta));

    GetEmitter()->emitIns_R_R_I(INS_add, EA_4BYTE, REG_FP, REG_SP, delta);

    if (reportUnwindData)
    {
        compiler->unwindPadding();
    }
}

void CodeGen::genCodeForInstr(GenTreeInstr* instr)
{
    unreached();
}

void CodeGen::emitInsLoad(instruction ins, emitAttr attr, regNumber dataReg, GenTreeIndir* load)
{
    assert(load->OperIs(GT_IND));

    if (load->IsUnaligned() && varTypeIsFloating(load->GetType()))
    {
        emitter* emit = GetEmitter();

        if (load->TypeIs(TYP_FLOAT))
        {
            regNumber tmpReg = load->GetSingleTempReg();
            emitInsIndir(INS_ldr, EA_4BYTE, tmpReg, load, 0);
            emit->emitIns_Mov(INS_vmov_i2f, EA_4BYTE, dataReg, tmpReg, /* canSkip */ false);
        }
        else
        {
            assert(load->TypeIs(TYP_DOUBLE));

            regNumber tmpReg1 = load->ExtractTempReg();
            regNumber tmpReg2 = load->GetSingleTempReg();
            emitInsIndir(INS_ldr, EA_4BYTE, tmpReg1, load, 0);
            emitInsIndir(INS_ldr, EA_4BYTE, tmpReg2, load, 4);
            emit->emitIns_R_R_R(INS_vmov_i2d, EA_8BYTE, dataReg, tmpReg1, tmpReg2);
        }

        return;
    }

    emitInsIndir(ins, attr, dataReg, load, 0);
}

void CodeGen::emitInsStore(instruction ins, emitAttr attr, regNumber dataReg, GenTreeStoreInd* store)
{
    assert(store->OperIs(GT_STOREIND));

    if (store->IsUnaligned() && varTypeIsFloating(store->GetType()))
    {
        emitter* emit = GetEmitter();

        if (store->TypeIs(TYP_FLOAT))
        {
            regNumber tmpReg = store->GetSingleTempReg();
            emit->emitIns_Mov(INS_vmov_f2i, EA_4BYTE, tmpReg, dataReg, /* canSkip */ false);
            emitInsIndir(INS_str, EA_4BYTE, tmpReg, store, 0);
        }
        else
        {
            assert(store->TypeIs(TYP_DOUBLE));

            regNumber tmpReg1 = store->ExtractTempReg();
            regNumber tmpReg2 = store->GetSingleTempReg();
            emit->emitIns_R_R_R(INS_vmov_d2i, EA_8BYTE, tmpReg1, tmpReg2, dataReg);
            emitInsIndir(INS_str, EA_4BYTE, tmpReg1, store, 0);
            emitInsIndir(INS_str, EA_4BYTE, tmpReg2, store, 4);
        }

        return;
    }

    emitInsIndir(ins, attr, dataReg, store, 0);
}

void CodeGen::emitInsIndir(instruction ins, emitAttr attr, regNumber valueReg, GenTreeIndir* indir, int offset)
{
    emitter* emit = GetEmitter();
    GenTree* addr = indir->GetAddr();

    if (!addr->isContained())
    {
        if (offset != 0)
        {
            assert(emitter::emitIns_valid_imm_for_add(offset, INS_FLAGS_DONT_CARE));

            emit->emitIns_R_R_I(ins, attr, valueReg, addr->GetRegNum(), offset);
        }
        else
        {
            emit->emitIns_R_R(ins, attr, valueReg, addr->GetRegNum());
        }

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

    offset += addrMode->GetOffset();

    if (index == nullptr)
    {
        if (emitter::emitIns_valid_imm_for_ldst_offset(offset, attr))
        {
            emit->emitIns_R_R_I(ins, attr, valueReg, base->GetRegNum(), offset);
        }
        else
        {
            regNumber offsetReg = indir->GetSingleTempReg();
            instGen_Set_Reg_To_Imm(EA_4BYTE, offsetReg, offset);
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
            emit->emitIns_R_R_R_I(ins, attr, valueReg, baseReg, indexReg, lsl, INS_FLAGS_DONT_CARE, INS_OPTS_LSL);
        }
        else
        {
            emit->emitIns_R_R_R(ins, attr, valueReg, baseReg, indexReg);
        }

        return;
    }

    // TODO-MIKE-Cleanup: Remove all this idiocy.

    regNumber tmpReg  = indir->GetSingleTempReg();
    emitAttr  tmpAttr = varTypeIsGC(base->GetType()) ? EA_BYREF : EA_4BYTE;

    noway_assert(emitter::emitInsIsLoad(ins) || (tmpReg != valueReg));

    if (!emitter::emitIns_valid_imm_for_add(offset, INS_FLAGS_DONT_CARE))
    {
        noway_assert(tmpReg != indexReg);

        instGen_Set_Reg_To_Imm(EA_4BYTE, tmpReg, offset);
        emit->emitIns_R_R_R(INS_add, tmpAttr, tmpReg, tmpReg, baseReg);
        emit->emitIns_R_R_R_I(ins, attr, valueReg, tmpReg, indexReg, lsl, INS_FLAGS_DONT_CARE, INS_OPTS_LSL);

        return;
    }

    if (lsl > 0)
    {
        emit->emitIns_R_R_R_I(INS_add, tmpAttr, tmpReg, baseReg, indexReg, lsl, INS_FLAGS_DONT_CARE, INS_OPTS_LSL);
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

    insFlags flags         = INS_FLAGS_DONT_CARE;
    bool     isMulOverflow = false;
    if (dst->gtOverflowEx())
    {
        if ((ins == INS_add) || (ins == INS_adc) || (ins == INS_sub) || (ins == INS_sbc))
        {
            flags = INS_FLAGS_SET;
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

    if ((dst->gtFlags & GTF_SET_FLAGS) != 0)
    {
        assert((ins == INS_add) || (ins == INS_adc) || (ins == INS_sub) || (ins == INS_sbc) || (ins == INS_and) ||
               (ins == INS_orr) || (ins == INS_eor) || (ins == INS_orn));
        flags = INS_FLAGS_SET;
    }

    if (intConst != nullptr)
    {
        emit->emitIns_R_R_I(ins, attr, dst->GetRegNum(), nonIntReg->GetRegNum(), (target_ssize_t)intConst->IconValue(),
                            flags);
    }
    else
    {
        if (isMulOverflow)
        {
            regNumber extraReg = dst->GetSingleTempReg();
            assert(extraReg != dst->GetRegNum());

            if ((dst->gtFlags & GTF_UNSIGNED) != 0)
            {
                // Compute 8 byte result from 4 byte by 4 byte multiplication.
                emit->emitIns_R_R_R_R(INS_umull, EA_4BYTE, dst->GetRegNum(), extraReg, src1->GetRegNum(),
                                      src2->GetRegNum());

                // Overflow exists if the result's high word is non-zero.
                emit->emitIns_R_I(INS_cmp, attr, extraReg, 0);
            }
            else
            {
                // Compute 8 byte result from 4 byte by 4 byte multiplication.
                emit->emitIns_R_R_R_R(INS_smull, EA_4BYTE, dst->GetRegNum(), extraReg, src1->GetRegNum(),
                                      src2->GetRegNum());

                // Overflow exists if the result's high word is not merely a sign bit.
                emit->emitIns_R_R_I(INS_cmp, attr, extraReg, dst->GetRegNum(), 31, INS_FLAGS_DONT_CARE, INS_OPTS_ASR);
            }
        }
        else
        {
            // We can just do the arithmetic, setting the flags if needed.
            emit->emitIns_R_R_R(ins, attr, dst->GetRegNum(), src1->GetRegNum(), src2->GetRegNum(), flags);
        }
    }

    if (dst->gtOverflowEx())
    {
        assert(!varTypeIsFloating(dst));

        emitJumpKind jumpKind;

        if (dst->OperGet() == GT_MUL)
        {
            jumpKind = EJ_ne;
        }
        else
        {
            bool isUnsignedOverflow = ((dst->gtFlags & GTF_UNSIGNED) != 0);
            jumpKind                = isUnsignedOverflow ? EJ_lo : EJ_vs;
            if (jumpKind == EJ_lo)
            {
                if ((dst->OperGet() != GT_SUB) && (dst->OperGet() != GT_SUB_HI))
                {
                    jumpKind = EJ_hs;
                }
            }
        }

        genJumpToThrowHlpBlk(jumpKind, ThrowHelperKind::Overflow);
    }

    return dst->GetRegNum();
}

//---------------------------------------------------------------------
// genTotalFrameSize - return the "total" size of the stack frame, including local size
// and callee-saved register size. There are a few things "missing" depending on the
// platform. The function genCallerSPtoInitialSPdelta() includes those things.
//
// For ARM, this doesn't include the prespilled registers.
//
// For x86, this doesn't include the frame pointer if isFramePointerUsed() is true.
// It also doesn't include the pushed return address.
//
// Return value:
//    Frame size

int CodeGenInterface::genTotalFrameSize() const
{
    assert(calleeRegsPushed != UINT_MAX);

    int totalFrameSize = calleeRegsPushed * REGSIZE_BYTES + lclFrameSize;

    assert(totalFrameSize >= 0);
    return totalFrameSize;
}

//---------------------------------------------------------------------
// genSPtoFPdelta - return the offset from SP to the frame pointer.
// This number is going to be positive, since SP must be at the lowest
// address.
//
// There must be a frame pointer to call this function!

int CodeGenInterface::genSPtoFPdelta() const
{
    assert(isFramePointerUsed());

    int delta = -genCallerSPtoInitialSPdelta() + genCallerSPtoFPdelta();

    assert(delta >= 0);
    return delta;
}

//---------------------------------------------------------------------
// genCallerSPtoFPdelta - return the offset from Caller-SP to the frame pointer.
// This number is going to be negative, since the Caller-SP is at a higher
// address than the frame pointer.
//
// There must be a frame pointer to call this function!

int CodeGenInterface::genCallerSPtoFPdelta() const
{
    assert(isFramePointerUsed());
    int callerSPtoFPdelta = 0;

    // On ARM, we first push the prespill registers, then store LR, then R11 (FP), and point R11 at the saved R11.
    callerSPtoFPdelta -= GetPreSpillSize();
    callerSPtoFPdelta -= 2 * REGSIZE_BYTES;

    assert(callerSPtoFPdelta <= 0);
    return callerSPtoFPdelta;
}

//---------------------------------------------------------------------
// genCallerSPtoInitialSPdelta - return the offset from Caller-SP to Initial SP.
//
// This number will be negative.

int CodeGenInterface::genCallerSPtoInitialSPdelta() const
{
    int callerSPtoSPdelta = 0;

    callerSPtoSPdelta -= GetPreSpillSize();
    callerSPtoSPdelta -= genTotalFrameSize();

    assert(callerSPtoSPdelta <= 0);
    return callerSPtoSPdelta;
}

void CodeGen::genPushFltRegs(regMaskTP regMask)
{
    assert(regMask != 0);                        // Don't call uness we have some registers to push
    assert((regMask & RBM_ALLFLOAT) == regMask); // Only floasting point registers should be in regMask

    regNumber lowReg = genRegNumFromMask(genFindLowestBit(regMask));
    int       slots  = genCountBits(regMask);
    // regMask should be contiguously set
    regMaskTP tmpMask = ((regMask >> lowReg) + 1); // tmpMask should have a single bit set
    assert((tmpMask & (tmpMask - 1)) == 0);
    assert(lowReg == REG_F16); // Currently we expect to start at F16 in the unwind codes

    // Our calling convention requires that we only use vpush for TYP_DOUBLE registers
    noway_assert(floatRegCanHoldType(lowReg, TYP_DOUBLE));
    noway_assert((slots % 2) == 0);

    GetEmitter()->emitIns_R_I(INS_vpush, EA_8BYTE, lowReg, slots / 2);
}

void CodeGen::genPopFltRegs(regMaskTP regMask)
{
    assert(regMask != 0);                        // Don't call uness we have some registers to pop
    assert((regMask & RBM_ALLFLOAT) == regMask); // Only floasting point registers should be in regMask

    regNumber lowReg = genRegNumFromMask(genFindLowestBit(regMask));
    int       slots  = genCountBits(regMask);
    // regMask should be contiguously set
    regMaskTP tmpMask = ((regMask >> lowReg) + 1); // tmpMask should have a single bit set
    assert((tmpMask & (tmpMask - 1)) == 0);

    // Our calling convention requires that we only use vpop for TYP_DOUBLE registers
    noway_assert(floatRegCanHoldType(lowReg, TYP_DOUBLE));
    noway_assert((slots % 2) == 0);

    GetEmitter()->emitIns_R_I(INS_vpop, EA_8BYTE, lowReg, slots / 2);
}

void CodeGen::PrologBlockInitLocals(int untrLclLo, int untrLclHi, regNumber initReg, bool* pInitRegZeroed)
{
    assert(generatingProlog && genUseBlockInit);
    assert(untrLclHi > untrLclLo);

    // Generate the following code:
    //
    // For cnt less than 10
    //
    //            mov     rZero1, 0
    //            mov     rZero2, 0
    //            mov     rCnt,  <cnt>
    //            stm     <rZero1,rZero2>,[rAddr!]
    // <optional> stm     <rZero1,rZero2>,[rAddr!]
    // <optional> stm     <rZero1,rZero2>,[rAddr!]
    // <optional> stm     <rZero1,rZero2>,[rAddr!]
    // <optional> str     rZero1,[rAddr]
    //
    // For rCnt greater than or equal to 10
    //
    //            mov     rZero1, 0
    //            mov     rZero2, 0
    //            mov     rCnt,  <cnt/2>
    //            sub     rAddr, sp, OFFS
    //
    //        loop:
    //            stm     <rZero1,rZero2>,[rAddr!]
    //            sub     rCnt,rCnt,1
    //            jnz     loop
    //
    // <optional> str     rZero1,[rAddr]   // When cnt is odd

    regNumber rAddr;
    regNumber rCnt = REG_NA; // Invalid
    regMaskTP regMask;

    // Set of available registers
    regMaskTP availMask = (calleeSavedModifiedRegs & RBM_ALLINT) | RBM_INT_CALLEE_TRASH;
    // Remove all of the incoming argument registers as they are currently live
    availMask &= ~paramRegState.intRegLiveIn;
    // Remove the pre-calculated initReg as we will zero it and maybe use it for a large constant.
    availMask &= ~genRegMask(initReg);

    if (compiler->compLocallocUsed)
    {
        availMask &= ~RBM_SAVED_LOCALLOC_SP; // Remove the register reserved when we have a localloc frame
    }

    regNumber rZero1; // We're going to use initReg for rZero1
    regNumber rZero2;

    // We pick the next lowest register number for rZero2
    noway_assert(availMask != RBM_NONE);
    regMask = genFindLowestBit(availMask);
    rZero2  = genRegNumFromMask(regMask);
    availMask &= ~regMask;

    // rZero2 is not a live incoming argument reg
    assert((genRegMask(rZero2) & paramRegState.intRegLiveIn) == RBM_NONE);

    // There should always be some available registers, genFinalizeFrame
    // adds callee saved registers if needed.
    noway_assert(availMask != RBM_NONE);

    regMask = genFindLowestBit(availMask);
    rAddr   = genRegNumFromMask(regMask);
    availMask &= ~regMask;

    bool     useLoop   = false;
    unsigned uCntBytes = untrLclHi - untrLclLo;
    assert((uCntBytes % sizeof(int)) == 0);         // The smallest stack slot is always 4 bytes.
    unsigned uCntSlots = uCntBytes / REGSIZE_BYTES; // How many register sized stack slots we're going to use.

    // When uCntSlots is 9 or less, we will emit a sequence of stm/stp instructions inline.
    // When it is 10 or greater, we will emit a loop containing a stm/stp instruction.
    // In both of these cases the stm/stp instruction will write two zeros to memory
    // and we will use a single str instruction at the end whenever we have an odd count.
    if (uCntSlots >= 10)
        useLoop = true;

    if (useLoop)
    {
        // We pick the next lowest register number for rCnt
        noway_assert(availMask != RBM_NONE);
        regMask = genFindLowestBit(availMask);
        rCnt    = genRegNumFromMask(regMask);
        availMask &= ~regMask;
    }

    // rAddr is not a live incoming argument reg
    assert((genRegMask(rAddr) & paramRegState.intRegLiveIn) == RBM_NONE);

    if (emitter::emitIns_valid_imm_for_add(untrLclLo))
    {
        GetEmitter()->emitIns_R_R_I(INS_add, EA_PTRSIZE, rAddr, genFramePointerReg(), untrLclLo);
    }
    else
    {
        // Load immediate into the InitReg register
        instGen_Set_Reg_To_Imm(EA_PTRSIZE, initReg, (ssize_t)untrLclLo);
        GetEmitter()->emitIns_R_R_R(INS_add, EA_PTRSIZE, rAddr, genFramePointerReg(), initReg);
        *pInitRegZeroed = false;
    }

    if (useLoop)
    {
        noway_assert(uCntSlots >= 2);
        // rCnt is not a live incoming param reg
        assert((genRegMask(rCnt) & paramRegState.intRegLiveIn) == RBM_NONE);

        instGen_Set_Reg_To_Imm(EA_PTRSIZE, rCnt, (ssize_t)uCntSlots / 2);
    }

    if (!*pInitRegZeroed)
    {
        instGen_Set_Reg_To_Zero(EA_PTRSIZE, initReg);
        *pInitRegZeroed = true;
    }

    rZero1 = initReg;

    instGen_Set_Reg_To_Zero(EA_PTRSIZE, rZero2);
    target_ssize_t stmImm = (target_ssize_t)(genRegMask(rZero1) | genRegMask(rZero2));

    if (!useLoop)
    {
        while (uCntBytes >= REGSIZE_BYTES * 2)
        {
            GetEmitter()->emitIns_R_I(INS_stm, EA_PTRSIZE, rAddr, stmImm);
            uCntBytes -= REGSIZE_BYTES * 2;
        }
    }
    else
    {
        GetEmitter()->emitIns_R_I(INS_stm, EA_PTRSIZE, rAddr, stmImm); // zero stack slots
        GetEmitter()->emitIns_R_I(INS_sub, EA_PTRSIZE, rCnt, 1, INS_FLAGS_SET);
        GetEmitter()->emitIns_J(INS_bhi, NULL, -3);
        uCntBytes %= REGSIZE_BYTES * 2;
    }

    if (uCntBytes >= REGSIZE_BYTES) // check and zero the last register-sized stack slot (odd number)
    {
        GetEmitter()->emitIns_R_R_I(INS_str, EA_PTRSIZE, rZero1, rAddr, 0);
        uCntBytes -= REGSIZE_BYTES;
    }

    noway_assert(uCntBytes == 0);
}

void CodeGen::PrologZeroRegs(regMaskTP initRegs, regNumber initReg, regMaskTP doubleRegs)
{
    for (regNumber reg = REG_INT_FIRST; reg <= REG_INT_LAST; reg = REG_NEXT(reg))
    {
        if (((initRegs & genRegMask(reg)) == RBM_NONE) || (reg == initReg))
        {
            continue;
        }

        instGen_Set_Reg_To_Zero(EA_PTRSIZE, reg);
        initReg = reg;
    }

    if (((initRegs & RBM_ALLFLOAT) | doubleRegs) == RBM_NONE)
    {
        return;
    }

    if (initReg == REG_NA)
    {
        initReg = REG_SCRATCH;
        instGen_Set_Reg_To_Zero(EA_PTRSIZE, initReg);
    }

    regNumber fltInitReg = REG_NA;
    regNumber dblInitReg = REG_NA;
    regMaskTP regMask    = genRegMask(REG_FP_FIRST);

    for (regNumber reg = REG_FP_FIRST; reg <= REG_FP_LAST; reg = REG_NEXT(reg), regMask <<= 1)
    {
        if ((initRegs & regMask) != RBM_NONE)
        {
            if (fltInitReg == REG_NA)
            {
                if (dblInitReg != REG_NA)
                {
                    GetEmitter()->emitIns_R_R(INS_vcvt_d2f, EA_4BYTE, reg, dblInitReg);
                }
                else
                {
                    GetEmitter()->emitIns_Mov(INS_vmov_i2f, EA_4BYTE, reg, initReg, /* canSkip */ false);
                }

                fltInitReg = reg;
                continue;
            }

            GetEmitter()->emitIns_Mov(INS_vmov, EA_4BYTE, reg, fltInitReg, /* canSkip */ false);
        }
        else if ((regMask & doubleRegs) != RBM_NONE)
        {
            if (dblInitReg == REG_NA)
            {
                if (fltInitReg != REG_NA)
                {
                    GetEmitter()->emitIns_R_R(INS_vcvt_f2d, EA_8BYTE, reg, fltInitReg);
                }
                else
                {
                    GetEmitter()->emitIns_R_R_R(INS_vmov_i2d, EA_8BYTE, reg, initReg, initReg);
                }

                dblInitReg = reg;
            }
            else
            {
                GetEmitter()->emitIns_Mov(INS_vmov, EA_8BYTE, reg, dblInitReg, /* canSkip */ false);
            }

            reg = REG_NEXT(reg);
            regMask <<= 1;
        }
    }
}

//------------------------------------------------------------------------
// genFreeLclFrame: free the local stack frame by adding `frameSize` to SP.
//
// Arguments:
//   frameSize - the frame size to free;
//   pUnwindStarted - was epilog unwind started or not.
//
// Notes:
//   If epilog unwind hasn't been started, and we generate code, we start unwind
//    and set* pUnwindStarted = true.
//
void CodeGen::genFreeLclFrame(unsigned frameSize, /* IN OUT */ bool* pUnwindStarted)
{
    assert(generatingEpilog);

    if (frameSize == 0)
        return;

    // Add 'frameSize' to SP.
    //
    // Unfortunately, we can't just use:
    //
    //      inst_RV_IV(INS_add, REG_SPBASE, frameSize, EA_PTRSIZE);
    //
    // because we need to generate proper unwind codes for each instruction generated,
    // and large frame sizes might generate a temp register load which might
    // need an unwind code. We don't want to generate a "NOP" code for this
    // temp register load; we want the unwind codes to start after that.

    if (emitter::validImmForInstr(INS_add, frameSize))
    {
        if (!*pUnwindStarted)
        {
            compiler->unwindBegEpilog();
            *pUnwindStarted = true;
        }

        GetEmitter()->emitIns_R_I(INS_add, EA_PTRSIZE, REG_SPBASE, frameSize);
    }
    else
    {
        // R12 doesn't hold arguments or return values, so can be used as temp.
        regNumber tmpReg = REG_R12;
        instGen_Set_Reg_To_Imm(EA_PTRSIZE, tmpReg, frameSize);
        if (*pUnwindStarted)
        {
            compiler->unwindPadding();
        }

        // We're going to generate an unwindable instruction, so check again if
        // we need to start the unwind codes.

        if (!*pUnwindStarted)
        {
            compiler->unwindBegEpilog();
            *pUnwindStarted = true;
        }

        GetEmitter()->emitIns_R_R(INS_add, EA_PTRSIZE, REG_SPBASE, tmpReg);
    }

    compiler->unwindAllocStack(frameSize);
}

/*-----------------------------------------------------------------------------
 *
 *  Returns register mask to push/pop to allocate a small stack frame,
 *  instead of using "sub sp" / "add sp". Returns RBM_NONE if either frame size
 *  is zero, or if we should use "sub sp" / "add sp" instead of push/pop.
 */
regMaskTP CodeGen::genStackAllocRegisterMask(unsigned frameSize, regMaskTP modifiedRegs)
{
    assert(generatingProlog || generatingEpilog);

    // We can't do this optimization with callee saved floating point registers because
    // the stack would be allocated in a wrong spot.
    if ((modifiedRegs & RBM_FLT_CALLEE_SAVED) != RBM_NONE)
    {
        return RBM_NONE;
    }

    // Allocate space for small frames by pushing extra registers. It generates smaller and faster code
    // that extra sub sp,XXX/add sp,XXX.
    // R0 and R1 may be used by return value. Keep things simple and just skip the optimization
    // for the 3*REGSIZE_BYTES and 4*REGSIZE_BYTES cases. They are less common and they have more
    // significant negative side-effects (more memory bus traffic).
    switch (frameSize)
    {
        case REGSIZE_BYTES:
            return RBM_R3;
        case 2 * REGSIZE_BYTES:
            return RBM_R2 | RBM_R3;
        default:
            return RBM_NONE;
    }
}

bool CodeGen::genCanUsePopToReturn(regMaskTP maskPopRegsInt, bool jmpEpilog)
{
    assert(generatingEpilog);

    return !jmpEpilog && (GetPreSpillRegs() == RBM_NONE);
}

void CodeGen::PrologPushCalleeSavedRegisters()
{
    assert(generatingProlog);

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

    regMaskTP maskPushRegsFloat = rsPushRegs & RBM_ALLFLOAT;
    regMaskTP maskPushRegsInt   = rsPushRegs & ~maskPushRegsFloat;

    maskPushRegsInt |= genStackAllocRegisterMask(lclFrameSize, maskPushRegsFloat);

    assert(FitsIn<int>(maskPushRegsInt));
    inst_IV(INS_push, (int)maskPushRegsInt);
    compiler->unwindPushMaskInt(maskPushRegsInt);

    if (maskPushRegsFloat != 0)
    {
        genPushFltRegs(maskPushRegsFloat);
        compiler->unwindPushMaskFloat(maskPushRegsFloat);
    }
}

void CodeGen::genPopCalleeSavedRegisters(bool jmpEpilog)
{
    assert(generatingEpilog);

    regMaskTP maskPopRegs      = calleeSavedModifiedRegs;
    regMaskTP maskPopRegsFloat = maskPopRegs & RBM_ALLFLOAT;
    regMaskTP maskPopRegsInt   = maskPopRegs & ~maskPopRegsFloat;

    // First, pop float registers

    if (maskPopRegsFloat != RBM_NONE)
    {
        genPopFltRegs(maskPopRegsFloat);
        compiler->unwindPopMaskFloat(maskPopRegsFloat);
    }

    // Next, pop integer registers

    if (!jmpEpilog)
    {
        regMaskTP maskStackAlloc = genStackAllocRegisterMask(lclFrameSize, maskPopRegsFloat);
        maskPopRegsInt |= maskStackAlloc;
    }

    if (isFramePointerUsed())
    {
        maskPopRegsInt |= RBM_FPBASE;
    }

    if (genCanUsePopToReturn(maskPopRegsInt, jmpEpilog))
    {
        maskPopRegsInt |= RBM_PC;
        // Record the fact that we use a pop to the PC to perform the return
        genUsedPopToReturn = true;
    }
    else
    {
        maskPopRegsInt |= RBM_LR;
        // Record the fact that we did not use a pop to the PC to perform the return
        genUsedPopToReturn = false;
    }

    assert(FitsIn<int>(maskPopRegsInt));
    inst_IV(INS_pop, (int)maskPopRegsInt);
    compiler->unwindPopMaskInt(maskPopRegsInt);
}

/*****************************************************************************
 *
 *  Generates code for an EH funclet prolog.
 *
 *  Funclets have the following incoming arguments:
 *
 *      catch:          r0 = the exception object that was caught (see GT_CATCH_ARG)
 *      filter:         r0 = the exception object to filter (see GT_CATCH_ARG), r1 = CallerSP of the containing function
 *      finally/fault:  none
 *
 *  Funclets set the following registers on exit:
 *
 *      catch:          r0 = the address at which execution should resume (see BBJ_EHCATCHRET)
 *      filter:         r0 = non-zero if the handler should handle the exception, zero otherwise (see GT_RETFILT)
 *      finally/fault:  none
 *
 *  The ARM funclet prolog sequence is:
 *
 *     push {regs,lr}   ; We push the callee-saved regs and 'lr'.
 *                      ;   TODO-ARM-CQ: We probably only need to save lr, plus any callee-save registers that we
 *                      ;         actually use in the funclet. Currently, we save the same set of callee-saved regs
 *                      ;         calculated for the entire function.
 *     sub sp, XXX      ; Establish the rest of the frame.
 *                      ;   XXX is determined by lvaOutgoingArgSpaceSize plus space for the PSP slot, aligned
 *                      ;   up to preserve stack alignment. If we push an odd number of registers, we also
 *                      ;   generate this, to keep the stack aligned.
 *
 *     ; Fill the PSP slot, for use by the VM (it gets reported with the GC info), or by code generation of nested
 *     ;     filters.
 *     ; This is not part of the "OS prolog"; it has no associated unwind data, and is not reversed in the funclet
 *     ;     epilog.
 *
 *     if (this is a filter funclet)
 *     {
 *          // r1 on entry to a filter funclet is CallerSP of the containing function:
 *          // either the main function, or the funclet for a handler that this filter is dynamically nested within.
 *          // Note that a filter can be dynamically nested within a funclet even if it is not statically within
 *          // a funclet. Consider:
 *          //
 *          //    try {
 *          //        try {
 *          //            throw new Exception();
 *          //        } catch(Exception) {
 *          //            throw new Exception();     // The exception thrown here ...
 *          //        }
 *          //    } filter {                         // ... will be processed here, while the "catch" funclet frame is
 *          //                                       // still on the stack
 *          //    } filter-handler {
 *          //    }
 *          //
 *          // Because of this, we need a PSP in the main function anytime a filter funclet doesn't know whether the
 *          // enclosing frame will be a funclet or main function. We won't know any time there is a filter protecting
 *          // nested EH. To simplify, we just always create a main function PSP for any function with a filter.
 *
 *          ldr r1, [r1 - PSP_slot_CallerSP_offset]     ; Load the CallerSP of the main function (stored in the PSP of
 *                                                      ; the dynamically containing funclet or function)
 *          str r1, [sp + PSP_slot_SP_offset]           ; store the PSP
 *          sub r11, r1, Function_CallerSP_to_FP_delta  ; re-establish the frame pointer
 *     }
 *     else
 *     {
 *          // This is NOT a filter funclet. The VM re-establishes the frame pointer on entry.
 *          // TODO-ARM-CQ: if VM set r1 to CallerSP on entry, like for filters, we could save an instruction.
 *
 *          add r3, r11, Function_CallerSP_to_FP_delta  ; compute the CallerSP, given the frame pointer. r3 is scratch.
 *          str r3, [sp + PSP_slot_SP_offset]           ; store the PSP
 *     }
 *
 *  The epilog sequence is then:
 *
 *     add sp, XXX      ; if necessary
 *     pop {regs,pc}
 *
 *  If it is worth it, we could push r0, r1, r2, r3 instead of using an additional add/sub instruction.
 *  Code size would be smaller, but we would be writing to / reading from the stack, which might be slow.
 *
 *  The funclet frame is thus:
 *
 *      |                       |
 *      |-----------------------|
 *      |       incoming        |
 *      |       arguments       |
 *      +=======================+ <---- Caller's SP
 *      |Callee saved registers |
 *      |-----------------------|
 *      |Pre-spill regs space   |   // This is only necessary to keep the PSP slot at the same offset
 *      |                       |   // in function and funclet
 *      |-----------------------|
 *      |        PSP slot       |   // Omitted in CoreRT ABI
 *      |-----------------------|
 *      ~  possible 4 byte pad  ~
 *      ~     for alignment     ~
 *      |-----------------------|
 *      |   Outgoing arg space  |
 *      |-----------------------| <---- Ambient SP
 *      |       |               |
 *      ~       | Stack grows   ~
 *      |       | downward      |
 *              V
 */

void CodeGen::genFuncletProlog(BasicBlock* block)
{
#ifdef DEBUG
    if (verbose)
        printf("*************** In genFuncletProlog()\n");
#endif

    assert(block != NULL);
    assert(block->bbFlags & BBF_FUNCLET_BEG);

    ScopedSetVariable<bool> _setGeneratingProlog(&generatingProlog, true);

    compiler->unwindBegProlog();

    regMaskTP maskPushRegsFloat = genFuncletInfo.fiSaveRegs & RBM_ALLFLOAT;
    regMaskTP maskPushRegsInt   = genFuncletInfo.fiSaveRegs & ~maskPushRegsFloat;

    regMaskTP maskStackAlloc = genStackAllocRegisterMask(genFuncletInfo.fiSpDelta, maskPushRegsFloat);
    maskPushRegsInt |= maskStackAlloc;

    assert(FitsIn<int>(maskPushRegsInt));
    inst_IV(INS_push, (int)maskPushRegsInt);
    compiler->unwindPushMaskInt(maskPushRegsInt);

    if (maskPushRegsFloat != RBM_NONE)
    {
        genPushFltRegs(maskPushRegsFloat);
        compiler->unwindPushMaskFloat(maskPushRegsFloat);
    }

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

    regNumber initReg       = REG_R3; // R3 is never live on entry to a funclet, so it can be trashed
    bool      initRegZeroed = false;

    if (maskStackAlloc == RBM_NONE)
    {
        PrologAllocLclFrame(genFuncletInfo.fiSpDelta, initReg, &initRegZeroed, maskArgRegsLiveIn);
    }

    // This is the end of the OS-reported prolog for purposes of unwinding
    compiler->unwindEndProlog();

    // If there is no PSPSym (CoreRT ABI), we are done.
    if (compiler->lvaPSPSym == BAD_VAR_NUM)
    {
        return;
    }

    if (isFilter)
    {
        // This is the first block of a filter

        GetEmitter()->emitIns_R_R_I(INS_ldr, EA_PTRSIZE, REG_R1, REG_R1, genFuncletInfo.fiPSP_slot_CallerSP_offset);
        GetEmitter()->emitIns_R_R_I(INS_str, EA_PTRSIZE, REG_R1, REG_SPBASE, genFuncletInfo.fiPSP_slot_SP_offset);
        GetEmitter()->emitIns_R_R_I(INS_sub, EA_PTRSIZE, REG_FPBASE, REG_R1,
                                    genFuncletInfo.fiFunctionCallerSPtoFPdelta);
    }
    else
    {
        // This is a non-filter funclet
        GetEmitter()->emitIns_R_R_I(INS_add, EA_PTRSIZE, REG_R3, REG_FPBASE,
                                    genFuncletInfo.fiFunctionCallerSPtoFPdelta);
        GetEmitter()->emitIns_R_R_I(INS_str, EA_PTRSIZE, REG_R3, REG_SPBASE, genFuncletInfo.fiPSP_slot_SP_offset);
    }
}

/*****************************************************************************
 *
 *  Generates code for an EH funclet epilog.
 */

void CodeGen::genFuncletEpilog()
{
#ifdef DEBUG
    if (verbose)
        printf("*************** In genFuncletEpilog()\n");
#endif

    ScopedSetVariable<bool> _setGeneratingEpilog(&generatingEpilog, true);

    // Just as for the main function, we delay starting the unwind codes until we have
    // an instruction which we know needs an unwind code. This is to support code like
    // this:
    //      movw    r3, 0x38e0
    //      add     sp, r3
    //      pop     {r4,r5,r6,r10,r11,pc}
    // where the "movw" shouldn't be part of the unwind codes. See genFnEpilog() for more details.

    bool unwindStarted = false;

    /* The saved regs info saves the LR register. We need to pop the PC register to return */
    assert(genFuncletInfo.fiSaveRegs & RBM_LR);

    regMaskTP maskPopRegsFloat = genFuncletInfo.fiSaveRegs & RBM_ALLFLOAT;
    regMaskTP maskPopRegsInt   = genFuncletInfo.fiSaveRegs & ~maskPopRegsFloat;

    regMaskTP maskStackAlloc = genStackAllocRegisterMask(genFuncletInfo.fiSpDelta, maskPopRegsFloat);
    maskPopRegsInt |= maskStackAlloc;

    if (maskStackAlloc == RBM_NONE)
    {
        genFreeLclFrame(genFuncletInfo.fiSpDelta, &unwindStarted);
    }

    if (!unwindStarted)
    {
        // We'll definitely generate an unwindable instruction next
        compiler->unwindBegEpilog();
        unwindStarted = true;
    }

    maskPopRegsInt &= ~RBM_LR;
    maskPopRegsInt |= RBM_PC;

    if (maskPopRegsFloat != RBM_NONE)
    {
        genPopFltRegs(maskPopRegsFloat);
        compiler->unwindPopMaskFloat(maskPopRegsFloat);
    }

    assert(FitsIn<int>(maskPopRegsInt));
    inst_IV(INS_pop, (int)maskPopRegsInt);
    compiler->unwindPopMaskInt(maskPopRegsInt);

    compiler->unwindEndEpilog();
}

/*****************************************************************************
 *
 *  Capture the information used to generate the funclet prologs and epilogs.
 *  Note that all funclet prologs are identical, and all funclet epilogs are
 *  identical (per type: filters are identical, and non-filters are identical).
 *  Thus, we compute the data used for these just once.
 *
 *  See genFuncletProlog() for more information about the prolog/epilog sequences.
 */

void CodeGen::genCaptureFuncletPrologEpilogInfo()
{
    if (compiler->ehAnyFunclets())
    {
        assert(isFramePointerUsed());
        assert(compiler->lvaDoneFrameLayout == Compiler::FINAL_FRAME_LAYOUT); // The frame size and offsets must be
        // finalized

        // Frame pointer doesn't point at the end, it points at the pushed r11. So, instead
        // of adding the number of callee-saved regs to CallerSP, we add 1 for lr and 1 for r11
        // (plus the "pre spill regs").

        unsigned preSpillRegArgSize                = GetPreSpillSize();
        genFuncletInfo.fiFunctionCallerSPtoFPdelta = preSpillRegArgSize + 2 * REGSIZE_BYTES;

        regMaskTP rsMaskSaveRegs = calleeSavedModifiedRegs;

        if (isFramePointerUsed())
        {
            rsMaskSaveRegs |= RBM_FP;
        }

        rsMaskSaveRegs |= RBM_LR;

        unsigned saveRegsCount = genCountBits(rsMaskSaveRegs);
        unsigned saveRegsSize  = saveRegsCount * REGSIZE_BYTES; // bytes of regs we're saving
        assert(outgoingArgSpaceSize % REGSIZE_BYTES == 0);
        unsigned funcletFrameSize =
            preSpillRegArgSize + saveRegsSize + REGSIZE_BYTES /* PSP slot */ + outgoingArgSpaceSize;

        unsigned funcletFrameSizeAligned  = roundUp(funcletFrameSize, STACK_ALIGN);
        unsigned funcletFrameAlignmentPad = funcletFrameSizeAligned - funcletFrameSize;
        unsigned spDelta                  = funcletFrameSizeAligned - saveRegsSize;

        unsigned PSP_slot_SP_offset       = outgoingArgSpaceSize + funcletFrameAlignmentPad;
        int      PSP_slot_CallerSP_offset = -(int)(funcletFrameSize - outgoingArgSpaceSize); // NOTE: it's negative!

        /* Now save it for future use */

        genFuncletInfo.fiSaveRegs                 = rsMaskSaveRegs;
        genFuncletInfo.fiSpDelta                  = spDelta;
        genFuncletInfo.fiPSP_slot_SP_offset       = PSP_slot_SP_offset;
        genFuncletInfo.fiPSP_slot_CallerSP_offset = PSP_slot_CallerSP_offset;

#ifdef DEBUG
        if (verbose)
        {
            printf("\n");
            printf("Funclet prolog / epilog info\n");
            printf("    Function CallerSP-to-FP delta: %d\n", genFuncletInfo.fiFunctionCallerSPtoFPdelta);
            printf("                        Save regs: ");
            dspRegMask(rsMaskSaveRegs);
            printf("\n");
            printf("                         SP delta: %d\n", genFuncletInfo.fiSpDelta);
            printf("               PSP slot SP offset: %d\n", genFuncletInfo.fiPSP_slot_SP_offset);
            printf("        PSP slot Caller SP offset: %d\n", genFuncletInfo.fiPSP_slot_CallerSP_offset);

            if (PSP_slot_CallerSP_offset != compiler->lvaGetCallerSPRelativeOffset(compiler->lvaPSPSym))
            {
                printf("lvaGetCallerSPRelativeOffset(lvaPSPSym): %d\n",
                       compiler->lvaGetCallerSPRelativeOffset(compiler->lvaPSPSym));
            }
        }
#endif // DEBUG

        assert(PSP_slot_CallerSP_offset < 0);
        if (compiler->lvaPSPSym != BAD_VAR_NUM)
        {
            assert(PSP_slot_CallerSP_offset ==
                   compiler->lvaGetCallerSPRelativeOffset(compiler->lvaPSPSym)); // same offset used in main
            // function and funclet!
        }
    }
}

void CodeGen::genFnEpilog(BasicBlock* block)
{
    JITDUMP("*************** In genFnEpilog()\n");
#ifdef DEBUG
    if (compiler->opts.dspCode)
    {
        printf("\n__epilog:\n");
    }
#endif

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

    // We delay starting the unwind codes until we have an instruction which we know
    // needs an unwind code. In particular, for large stack frames in methods without
    // localloc, the sequence might look something like this:
    //      movw    r3, 0x38e0
    //      add     sp, r3
    //      pop     {r4,r5,r6,r10,r11,pc}
    // In this case, the "movw" should not be part of the unwind codes, since it will
    // be a NOP, and it is a waste to start with a NOP. Note that calling unwindBegEpilog()
    // also sets the current location as the beginning offset of the epilog, so every
    // instruction afterwards needs an unwind code. In the case above, if you call
    // unwindBegEpilog() before the "movw", then you must generate a NOP for the "movw".

    bool unwindStarted = false;

    // Tear down the stack frame

    if (compiler->compLocallocUsed)
    {
        if (!unwindStarted)
        {
            compiler->unwindBegEpilog();
            unwindStarted = true;
        }

        // mov R9 into SP
        inst_Mov(TYP_I_IMPL, REG_SP, REG_SAVED_LOCALLOC_SP, /* canSkip */ false);
        compiler->unwindSetFrameReg(REG_SAVED_LOCALLOC_SP, 0);
    }

    if (jmpEpilog || genStackAllocRegisterMask(lclFrameSize, calleeSavedModifiedRegs) == RBM_NONE)
    {
        genFreeLclFrame(lclFrameSize, &unwindStarted);
    }

    if (!unwindStarted)
    {
        // If we haven't generated anything yet, we're certainly going to generate a "pop" next.
        compiler->unwindBegEpilog();
        unwindStarted = true;
    }

    if (jmpEpilog && lastNode->gtOper == GT_JMP && addrInfo.accessType == IAT_RELPVALUE)
    {
        // IAT_RELPVALUE jump at the end is done using relative indirection, so,
        // additional helper register is required.
        // We use LR just before it is going to be restored from stack, i.e.
        //
        //     movw r12, laddr
        //     movt r12, haddr
        //     mov lr, r12
        //     ldr r12, [r12]
        //     add r12, r12, lr
        //     pop {lr}
        //     ...
        //     bx r12

        regNumber indCallReg = REG_R12;
        regNumber vptrReg1   = REG_LR;

        instGen_Set_Reg_To_Imm(EA_HANDLE_CNS_RELOC, indCallReg, (ssize_t)addrInfo.addr);
        GetEmitter()->emitIns_Mov(INS_mov, EA_PTRSIZE, vptrReg1, indCallReg, /* canSkip */ false);
        GetEmitter()->emitIns_R_R_I(INS_ldr, EA_PTRSIZE, indCallReg, indCallReg, 0);
        GetEmitter()->emitIns_R_R(INS_add, EA_PTRSIZE, indCallReg, vptrReg1);
    }

    genPopCalleeSavedRegisters(jmpEpilog);

    if (unsigned preSpillRegArgSize = GetPreSpillSize())
    {
        // We better not have used a pop PC to return otherwise this will be unreachable code
        noway_assert(!genUsedPopToReturn);

        inst_RV_IV(INS_add, REG_SPBASE, preSpillRegArgSize, EA_PTRSIZE);
        compiler->unwindAllocStack(preSpillRegArgSize);
    }

    if (jmpEpilog)
    {
        // We better not have used a pop PC to return otherwise this will be unreachable code
        noway_assert(!genUsedPopToReturn);
    }

    if (jmpEpilog)
    {
        GenJmpEpilog(block, methHnd, addrInfo);
    }
    else if (!genUsedPopToReturn)
    {
        // If we did not use a pop to return, then we did a "pop {..., lr}" instead of "pop {..., pc}",
        // so we need a "bx lr" instruction to return from the function.
        inst_RV(INS_bx, REG_LR, TYP_I_IMPL);
        compiler->unwindBranch16();
    }

    compiler->unwindEndEpilog();
}

#endif // TARGET_ARM
