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
#include "gcinfo.h"
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
//    flags               - whether flags are set
//    tmpReg              - temp register to use when the 'imm' doesn't fit. Can be REG_NA
//                          if caller knows for certain the constant will fit.
//
// Return Value:
//    returns true if the immediate was small enough to be encoded inside instruction. If not,
//    returns false meaning the immediate was too large and tmpReg was used and modified.
//
bool CodeGen::genInstrWithConstant(
    instruction ins, emitAttr attr, regNumber reg1, regNumber reg2, ssize_t imm, insFlags flags, regNumber tmpReg)
{
    bool immFitsInIns = false;

    // reg1 is usually a dest register
    // reg2 is always source register
    assert(tmpReg != reg2); // regTmp cannot match any source register

    switch (ins)
    {
        case INS_add:
        case INS_sub:
            immFitsInIns = emitter::validImmForInstr(ins, (target_ssize_t)imm, flags);
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
    //
    return genInstrWithConstant(INS_add, EA_PTRSIZE, REG_SPBASE, REG_SPBASE, spDelta, INS_FLAGS_DONT_CARE, tmpReg);
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

// Move an immediate value into an integer register.
void CodeGen::instGen_Set_Reg_To_Imm(emitAttr  size,
                                     regNumber reg,
                                     ssize_t   imm,
                                     insFlags flags DEBUGARG(size_t targetHandle) DEBUGARG(GenTreeFlags gtFlags))
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
        instGen_Set_Reg_To_Zero(size, reg, flags);
    }
    else
    {
        // TODO-CrossBitness: we wouldn't need the cast below if we had CodeGen::instGen_Set_Reg_To_Reloc_Imm.
        const int val32 = (int)imm;
        if (emitter::emitIns_valid_imm_for_mov(val32))
        {
            GetEmitter()->emitIns_R_I(INS_mov, size, reg, val32, flags);
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

            if (flags == INS_FLAGS_SET)
                GetEmitter()->emitIns_Mov(INS_mov, size, reg, reg, /* canSkip */ false, INS_FLAGS_SET);
        }
    }

    regSet.verifyRegUsed(reg);
}

//------------------------------------------------------------------------
// genSetRegToConst: Generate code to set a register 'targetReg' of type 'targetType'
//    to the constant specified by the constant (GT_CNS_INT or GT_CNS_DBL) in 'tree'.
//
// Notes:
//    This does not call genProduceReg() on the target register.
//
void CodeGen::genSetRegToConst(regNumber targetReg, var_types targetType, GenTree* tree)
{
    switch (tree->gtOper)
    {
        case GT_CNS_INT:
        {
            // relocatable values tend to come down as a CNS_INT of native int type
            // so the line between these two opcodes is kind of blurry
            GenTreeIntCon* con    = tree->AsIntCon();
            ssize_t        cnsVal = con->GetValue();

            if (con->ImmedValNeedsReloc(compiler))
            {
                instGen_Set_Reg_To_Imm(EA_HANDLE_CNS_RELOC, targetReg, cnsVal);
            }
            else
            {
                genSetRegToIcon(targetReg, cnsVal, targetType);
            }
        }
        break;

        case GT_CNS_DBL:
        {
            GenTreeDblCon* dblConst   = tree->AsDblCon();
            double         constValue = dblConst->AsDblCon()->gtDconVal;
            // TODO-ARM-CQ: Do we have a faster/smaller way to generate 0.0 in thumb2 ISA ?
            if (targetType == TYP_FLOAT)
            {
                // Get a temp integer register
                regNumber tmpReg = tree->GetSingleTempReg();

                float f = forceCastToFloat(constValue);
                genSetRegToIcon(tmpReg, *((int*)(&f)));
                GetEmitter()->emitIns_Mov(INS_vmov_i2f, EA_4BYTE, targetReg, tmpReg, /* canSkip */ false);
            }
            else
            {
                assert(targetType == TYP_DOUBLE);

                unsigned* cv = (unsigned*)&constValue;

                // Get two temp integer registers
                regNumber tmpReg1 = tree->ExtractTempReg();
                regNumber tmpReg2 = tree->GetSingleTempReg();

                genSetRegToIcon(tmpReg1, cv[0]);
                genSetRegToIcon(tmpReg2, cv[1]);

                GetEmitter()->emitIns_R_R_R(INS_vmov_i2d, EA_8BYTE, targetReg, tmpReg1, tmpReg2);
            }
        }
        break;

        default:
            unreached();
    }
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

    // Check to 0 size allocations
    // size_t amount = 0;
    if (size->IsCnsIntOrI())
    {
        // If size is a constant, then it must be contained.
        assert(size->isContained());

        // If amount is zero then return null in regCnt
        size_t amount = size->AsIntCon()->gtIconVal;
        if (amount == 0)
        {
            instGen_Set_Reg_To_Zero(EA_PTRSIZE, regCnt);
            goto BAILOUT;
        }
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
            instGen_Set_Reg_To_Zero(EA_PTRSIZE, regCnt);

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
        genSetRegToIcon(regCnt, amount, TYP_INT);
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

        instGen_Set_Reg_To_Zero(EA_PTRSIZE, regTmp);

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
        instGen_Set_Reg_To_Zero(EA_PTRSIZE, regCnt);

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
        genInstrWithConstant(INS_add, EA_PTRSIZE, regCnt, REG_SPBASE, (ssize_t)stackAdjustment, INS_FLAGS_DONT_CARE,
                             regTmp);
    }
    else // stackAdjustment == 0
    {
        // Move the final value of SP to regCnt
        inst_Mov(TYP_I_IMPL, regCnt, REG_SPBASE, /* canSkip */ false);
    }

BAILOUT:
    if (endLabel != nullptr)
        genDefineTempLabel(endLabel);

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

//------------------------------------------------------------------------
// genJumpTable: emits the table and an instruction to get the address of the first element
//
void CodeGen::genJumpTable(GenTree* treeNode)
{
    noway_assert(compiler->compCurBB->bbJumpKind == BBJ_SWITCH);
    assert(treeNode->OperGet() == GT_JMPTABLE);

    unsigned     jumpCount = compiler->compCurBB->bbJumpSwt->bbsCount;
    BasicBlock** jumpTable = compiler->compCurBB->bbJumpSwt->bbsDstTab;
    unsigned     jmpTabBase;

    jmpTabBase = GetEmitter()->emitBBTableDataGenBeg(jumpCount, false);

    JITDUMP("\n      J_M%03u_DS%02u LABEL   DWORD\n", compiler->compMethodID, jmpTabBase);

    for (unsigned i = 0; i < jumpCount; i++)
    {
        BasicBlock* target = *jumpTable++;
        noway_assert(target->bbFlags & BBF_HAS_LABEL);

        JITDUMP("            DD      L_M%03u_" FMT_BB "\n", compiler->compMethodID, target->bbNum);

        GetEmitter()->emitDataGenData(i, target);
    }

    GetEmitter()->emitDataGenEnd();

    genMov32RelocatableDataLabel(jmpTabBase, treeNode->GetRegNum());

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

    regNumber srcReg = genConsumeReg(src);
    regNumber dstReg = store->GetRegNum();

    if (dstReg == REG_NA)
    {
        unsigned lclNum = store->GetLclNum();
        GetEmitter()->emitIns_S_R(ins_Store(lclRegType), emitTypeSize(lclRegType), srcReg, lclNum, 0);
        genUpdateLife(store);
        lcl->SetRegNum(REG_STK);

        return;
    }

    GetEmitter()->emitIns_Mov(ins_Copy(lclRegType), emitActualTypeSize(lclRegType), dstReg, srcReg, /*canSkip*/ true);

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
    assert(treeNode->OperGet() == GT_CKFINITE);

    emitter*  emit       = GetEmitter();
    var_types targetType = treeNode->TypeGet();
    regNumber intReg     = treeNode->GetSingleTempReg();
    regNumber fpReg      = genConsumeReg(treeNode->AsOp()->gtOp1);
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
    genJumpToThrowHlpBlk(EJ_eq, SCK_ARITH_EXCPN);

    // If it's a finite value, copy it to targetReg
    inst_Mov(targetType, targetReg, fpReg, /* canSkip */ true, emitTypeSize(treeNode));

    genProduceReg(treeNode);
}

//------------------------------------------------------------------------
// genCodeForCompare: Produce code for a GT_EQ/GT_NE/GT_LT/GT_LE/GT_GE/GT_GT/GT_CMP node.
//
// Arguments:
//    tree - the node
//
void CodeGen::genCodeForCompare(GenTreeOp* tree)
{
    // TODO-ARM-CQ: Check if we can use the currently set flags.
    // TODO-ARM-CQ: Check for the case where we can simply transfer the carry bit to a register
    //         (signed < or >= where targetReg != REG_NA)

    GenTree*  op1     = tree->gtOp1;
    GenTree*  op2     = tree->gtOp2;
    var_types op1Type = op1->TypeGet();
    var_types op2Type = op2->TypeGet();

    assert(!varTypeIsLong(op1Type));
    assert(!varTypeIsLong(op2Type));

    regNumber targetReg = tree->GetRegNum();
    emitter*  emit      = GetEmitter();

    genConsumeIfReg(op1);
    genConsumeIfReg(op2);

    if (varTypeIsFloating(op1Type))
    {
        assert(op1Type == op2Type);
        assert(!tree->OperIs(GT_CMP));
        emit->emitIns_R_R(INS_vcmp, emitTypeSize(op1Type), op1->GetRegNum(), op2->GetRegNum());
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

    // Are we evaluating this into a register?
    if (targetReg != REG_NA)
    {
        inst_SETCC(GenCondition::FromRelop(tree), tree->TypeGet(), targetReg);
        genProduceReg(tree);
    }
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
void CodeGen::genLongToIntCast(GenTree* cast)
{
    assert(cast->OperGet() == GT_CAST);

    GenTree* src = cast->gtGetOp1();
    noway_assert(src->OperGet() == GT_LONG);

    var_types srcType  = ((cast->gtFlags & GTF_UNSIGNED) != 0) ? TYP_ULONG : TYP_LONG;
    var_types dstType  = cast->CastToType();
    regNumber loSrcReg = UseReg(src->gtGetOp1());
    regNumber hiSrcReg = UseReg(src->gtGetOp2());
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

            inst_RV_RV(INS_tst, loSrcReg, loSrcReg, TYP_INT, EA_4BYTE);
            inst_JMP(EJ_mi, allOne);
            inst_RV_RV(INS_tst, hiSrcReg, hiSrcReg, TYP_INT, EA_4BYTE);
            genJumpToThrowHlpBlk(EJ_ne, SCK_OVERFLOW);
            inst_JMP(EJ_jmp, success);

            genDefineTempLabel(allOne);
            inst_RV_IV(INS_cmp, hiSrcReg, -1, EA_4BYTE);
            genJumpToThrowHlpBlk(EJ_ne, SCK_OVERFLOW);

            genDefineTempLabel(success);
        }
        else
        {
            if ((srcType == TYP_ULONG) && (dstType == TYP_INT))
            {
                inst_RV_RV(INS_tst, loSrcReg, loSrcReg, TYP_INT, EA_4BYTE);
                genJumpToThrowHlpBlk(EJ_mi, SCK_OVERFLOW);
            }

            inst_RV_RV(INS_tst, hiSrcReg, hiSrcReg, TYP_INT, EA_4BYTE);
            genJumpToThrowHlpBlk(EJ_ne, SCK_OVERFLOW);
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

    if (!addr || !validImmForBL((ssize_t)addr))
    {
        if (callTargetReg == REG_NA)
        {
            // If a callTargetReg has not been explicitly provided, we will use REG_DEFAULT_HELPER_CALL_TARGET, but
            // this is only a valid assumption if the helper call is known to kill REG_DEFAULT_HELPER_CALL_TARGET.
            callTargetReg = REG_DEFAULT_HELPER_CALL_TARGET;
        }

        // Load the address into a register and call through a register
        if (addr)
        {
            instGen_Set_Reg_To_Imm(EA_HANDLE_CNS_RELOC, callTargetReg, (ssize_t)addr);
        }
        else
        {
            GetEmitter()->emitIns_R_AI(INS_ldr, EA_PTR_DSP_RELOC, callTargetReg, (ssize_t)pAddr);
            regSet.verifyRegUsed(callTargetReg);
        }

        GetEmitter()->emitIns_Call(emitter::EC_INDIR_R, Compiler::eeFindHelper(helper) DEBUGARG(nullptr), nullptr,
                                   retSize, gcInfo.gcVarPtrSetCur, gcInfo.gcRegGCrefSetCur, gcInfo.gcRegByrefSetCur,
                                   BAD_IL_OFFSET, callTargetReg, false);
    }
    else
    {
        GetEmitter()->emitIns_Call(emitter::EC_FUNC_TOKEN, Compiler::eeFindHelper(helper) DEBUGARG(nullptr), addr,
                                   retSize, gcInfo.gcVarPtrSetCur, gcInfo.gcRegGCrefSetCur, gcInfo.gcRegByrefSetCur,
                                   BAD_IL_OFFSET, REG_NA, false);
    }

    regSet.verifyRegistersUsed(RBM_CALLEE_TRASH);
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
// genProfilingEnterCallback: Generate the profiling function enter callback.
//
// Arguments:
//     initReg        - register to use as scratch register
//     pInitRegZeroed - OUT parameter. *pInitRegZeroed set to 'false' if 'initReg' is
//                      not zero after this call.
//
// Return Value:
//     None
//
void CodeGen::genProfilingEnterCallback(regNumber initReg, bool* pInitRegZeroed)
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
    regNumber argReg     = REG_PROFILER_ENTER_ARG;
    regMaskTP argRegMask = genRegMask(argReg);
    assert((preSpillParamRegs & argRegMask) != 0);

    if (compiler->compProfilerMethHndIndirected)
    {
        GetEmitter()->emitIns_R_AI(INS_ldr, EA_PTR_DSP_RELOC, argReg, (ssize_t)compiler->compProfilerMethHnd);
        regSet.verifyRegUsed(argReg);
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
        genTransferRegGCState(REG_PROFILER_RET_SCRATCH, REG_R0);
        regSet.verifyRegUsed(REG_PROFILER_RET_SCRATCH);
    }

    if (compiler->compProfilerMethHndIndirected)
    {
        GetEmitter()->emitIns_R_AI(INS_ldr, EA_PTR_DSP_RELOC, REG_R0, (ssize_t)compiler->compProfilerMethHnd);
        regSet.verifyRegUsed(REG_R0);
    }
    else
    {
        instGen_Set_Reg_To_Imm(EA_PTRSIZE, REG_R0, (ssize_t)compiler->compProfilerMethHnd);
    }

    gcInfo.gcMarkRegSetNpt(RBM_R0);

    genEmitHelperCall(helper);

    // Restore state that existed before profiler callback
    if (r0InUse)
    {
        GetEmitter()->emitIns_Mov(INS_mov, attr, REG_R0, REG_PROFILER_RET_SCRATCH, /* canSkip */ false);
        genTransferRegGCState(REG_R0, REG_PROFILER_RET_SCRATCH);
        gcInfo.gcMarkRegSetNpt(RBM_PROFILER_RET_SCRATCH);
    }
}

#endif // PROFILING_SUPPORTED

//------------------------------------------------------------------------
// genAllocLclFrame: Probe the stack and allocate the local stack frame - subtract from SP.
//
// Notes:
//      The first instruction of the prolog is always a push (which touches the lowest address
//      of the stack), either of the LR register or of some argument registers, e.g., in the case of
//      pre-spilling. The LR register is always pushed because we require it to allow for GC return
//      address hijacking (see the comment in CodeGen::genPushCalleeSavedRegisters()). These pushes
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
void CodeGen::genAllocLclFrame(unsigned frameSize, regNumber initReg, bool* pInitRegZeroed, regMaskTP maskArgRegsLiveIn)
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
                             INS_FLAGS_DONT_CARE, REG_STACK_PROBE_HELPER_ARG);
        regSet.verifyRegUsed(REG_STACK_PROBE_HELPER_ARG);
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
    if (!doubleAlignOrFramePointerUsed())
    {
        psiAdjustStackLevel(frameSize);
    }
#endif // USING_SCOPE_INFO
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

    if (addr->OperIs(GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR))
    {
        GenTreeLclVarCommon* lclNode = addr->AsLclVarCommon();
        unsigned             lclNum  = lclNode->GetLclNum();
        unsigned             offset  = lclNode->GetLclOffs();

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

        // Jump to the block which will throw the exception.
        genJumpToThrowHlpBlk(jumpKind, SCK_OVERFLOW);
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
    assert(!IsUninitialized(calleeRegsPushed));

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

#endif // TARGET_ARM
