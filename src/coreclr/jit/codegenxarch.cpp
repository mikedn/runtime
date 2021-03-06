// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                        Amd64/x86 Code Generator                           XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/
#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#pragma warning(disable : 4310) // cast truncates constant value - happens for (int8_t)0xb1
#endif

#ifdef TARGET_XARCH
#include "emit.h"
#include "codegen.h"
#include "lower.h"
#include "gcinfo.h"
#include "gcinfoencoder.h"
#include "patchpointinfo.h"

/*****************************************************************************
 *
 *  Generate code that will set the given register to the integer constant.
 */

void CodeGen::genSetRegToIcon(regNumber reg, ssize_t val, var_types type, insFlags flags)
{
    // Reg cannot be a FP reg
    assert(!genIsValidFloatReg(reg));

    // The only TYP_REF constant that can come this path is a managed 'null' since it is not
    // relocatable.  Other ref type constants (e.g. string objects) go through a different
    // code path.
    noway_assert(type != TYP_REF || val == 0);

    if (val == 0)
    {
        instGen_Set_Reg_To_Zero(emitActualTypeSize(type), reg, flags);
    }
    else
    {
        // TODO-XArch-CQ: needs all the optimized cases
        GetEmitter()->emitIns_R_I(INS_mov, emitActualTypeSize(type), reg, val);
    }
}

//---------------------------------------------------------------------
// genSetGSSecurityCookie: Set the "GS" security cookie in the prolog.
//
// Arguments:
//     initReg        - register to use as a scratch register
//     pInitRegZeroed - OUT parameter. *pInitRegZeroed is set to 'false' if and only if
//                      this call sets 'initReg' to a non-zero value.
//
// Return Value:
//     None
//
void CodeGen::genSetGSSecurityCookie(regNumber initReg, bool* pInitRegZeroed)
{
    assert(compiler->compGeneratingProlog);

    if (!compiler->getNeedsGSSecurityCookie())
    {
        return;
    }

    if (compiler->opts.IsOSR() && compiler->info.compPatchpointInfo->HasSecurityCookie())
    {
        // Security cookie is on original frame and was initialized there.
        return;
    }

    if (compiler->gsGlobalSecurityCookieAddr == nullptr)
    {
        noway_assert(compiler->gsGlobalSecurityCookieVal != 0);
#ifdef TARGET_AMD64
        if ((int)compiler->gsGlobalSecurityCookieVal != compiler->gsGlobalSecurityCookieVal)
        {
            // initReg = #GlobalSecurityCookieVal64; [frame.GSSecurityCookie] = initReg
            genSetRegToIcon(initReg, compiler->gsGlobalSecurityCookieVal, TYP_I_IMPL);
            GetEmitter()->emitIns_S_R(INS_mov, EA_PTRSIZE, initReg, compiler->lvaGSSecurityCookie, 0);
            *pInitRegZeroed = false;
        }
        else
#endif
        {
            // mov   dword ptr [frame.GSSecurityCookie], #GlobalSecurityCookieVal
            GetEmitter()->emitIns_S_I(INS_mov, EA_PTRSIZE, compiler->lvaGSSecurityCookie, 0,
                                      (int)compiler->gsGlobalSecurityCookieVal);
        }
    }
    else
    {
        // Always use EAX on x86 and x64
        // On x64, if we're not moving into RAX, and the address isn't RIP relative, we can't encode it.
        //  mov   eax, dword ptr [compiler->gsGlobalSecurityCookieAddr]
        //  mov   dword ptr [frame.GSSecurityCookie], eax
        GetEmitter()->emitIns_R_AI(INS_mov, EA_PTR_DSP_RELOC, REG_EAX, (ssize_t)compiler->gsGlobalSecurityCookieAddr);
        regSet.verifyRegUsed(REG_EAX);
        GetEmitter()->emitIns_S_R(INS_mov, EA_PTRSIZE, REG_EAX, compiler->lvaGSSecurityCookie, 0);
        if (initReg == REG_EAX)
        {
            *pInitRegZeroed = false;
        }
    }
}

/*****************************************************************************
 *
 *   Generate code to check that the GS cookie wasn't thrashed by a buffer
 *   overrun.  If pushReg is true, preserve all registers around code sequence.
 *   Otherwise ECX could be modified.
 *
 *   Implementation Note: pushReg = true, in case of tail calls.
 */
void CodeGen::genEmitGSCookieCheck(bool pushReg)
{
    noway_assert(compiler->gsGlobalSecurityCookieAddr || compiler->gsGlobalSecurityCookieVal);

    if (!pushReg && !varTypeUsesFloatReg(compiler->info.GetRetSigType()))
    {
        // Return registers that contain GC references must be reported
        // as live while the GC cookie is checked.

        ReturnTypeDesc& retDesc = compiler->info.retDesc;

        for (unsigned i = 0; i < retDesc.GetRegCount(); ++i)
        {
            gcInfo.gcMarkRegPtrVal(retDesc.GetRegNum(i), retDesc.GetRegType(i));
        }
    }

    regNumber regGSCheck;
    regMaskTP regMaskGSCheck = RBM_NONE;

    if (!pushReg)
    {
        // Non-tail call: we can use any callee trash register that is not
        // a return register or contain 'this' pointer (keep alive this), since
        // we are generating GS cookie check after a GT_RETURN block.
        // Note: On Amd64 System V RDX is an arg register - REG_ARG_2 - as well
        // as return register for two-register-returned structs.
        if (compiler->lvaKeepAliveAndReportThis() && compiler->lvaTable[compiler->info.compThisArg].lvIsInReg() &&
            (compiler->lvaTable[compiler->info.compThisArg].GetRegNum() == REG_ARG_0))
        {
            regGSCheck = REG_ARG_1;
        }
        else
        {
            regGSCheck = REG_ARG_0;
        }
    }
    else
    {
#ifdef TARGET_X86
        // It doesn't matter which register we pick, since we're going to save and restore it
        // around the check.
        // TODO-CQ: Can we optimize the choice of register to avoid doing the push/pop sometimes?
        regGSCheck     = REG_EAX;
        regMaskGSCheck = RBM_EAX;
#else  // !TARGET_X86
        // Jmp calls: specify method handle using which JIT queries VM for its entry point
        // address and hence it can neither be a VSD call nor PInvoke calli with cookie
        // parameter.  Therefore, in case of jmp calls it is safe to use R11.
        regGSCheck = REG_R11;
#endif // !TARGET_X86
    }

    regMaskTP byrefPushedRegs = RBM_NONE;
    regMaskTP norefPushedRegs = RBM_NONE;
    regMaskTP pushedRegs      = RBM_NONE;

    if (compiler->gsGlobalSecurityCookieAddr == nullptr)
    {
#if defined(TARGET_AMD64)
        // If GS cookie value fits within 32-bits we can use 'cmp mem64, imm32'.
        // Otherwise, load the value into a reg and use 'cmp mem64, reg64'.
        if ((int)compiler->gsGlobalSecurityCookieVal != (ssize_t)compiler->gsGlobalSecurityCookieVal)
        {
            genSetRegToIcon(regGSCheck, compiler->gsGlobalSecurityCookieVal, TYP_I_IMPL);
            GetEmitter()->emitIns_S_R(INS_cmp, EA_PTRSIZE, regGSCheck, compiler->lvaGSSecurityCookie, 0);
        }
        else
#endif // defined(TARGET_AMD64)
        {
            assert((int)compiler->gsGlobalSecurityCookieVal == (ssize_t)compiler->gsGlobalSecurityCookieVal);
            GetEmitter()->emitIns_S_I(INS_cmp, EA_PTRSIZE, compiler->lvaGSSecurityCookie, 0,
                                      (int)compiler->gsGlobalSecurityCookieVal);
        }
    }
    else
    {
        // Ngen case - GS cookie value needs to be accessed through an indirection.

        pushedRegs = genPushRegs(regMaskGSCheck, &byrefPushedRegs, &norefPushedRegs);

        instGen_Set_Reg_To_Imm(EA_HANDLE_CNS_RELOC, regGSCheck, (ssize_t)compiler->gsGlobalSecurityCookieAddr);
        GetEmitter()->emitIns_R_AR(ins_Load(TYP_I_IMPL), EA_PTRSIZE, regGSCheck, regGSCheck, 0);
        GetEmitter()->emitIns_S_R(INS_cmp, EA_PTRSIZE, regGSCheck, compiler->lvaGSSecurityCookie, 0);
    }

    BasicBlock* gsCheckBlk = genCreateTempLabel();
    inst_JMP(EJ_je, gsCheckBlk);
    genEmitHelperCall(CORINFO_HELP_FAIL_FAST, 0, EA_UNKNOWN);
    genDefineTempLabel(gsCheckBlk);

    genPopRegs(pushedRegs, byrefPushedRegs, norefPushedRegs);
}

BasicBlock* CodeGen::genCallFinally(BasicBlock* block)
{
#if defined(FEATURE_EH_FUNCLETS)
    // Generate a call to the finally, like this:
    //      mov         rcx,qword ptr [rbp + 20H]       // Load rcx with PSPSym
    //      call        finally-funclet
    //      jmp         finally-return                  // Only for non-retless finally calls
    // The jmp can be a NOP if we're going to the next block.
    // If we're generating code for the main function (not a funclet), and there is no localloc,
    // then RSP at this point is the same value as that stored in the PSPSym. So just copy RSP
    // instead of loading the PSPSym in this case, or if PSPSym is not used (CoreRT ABI).

    if ((compiler->lvaPSPSym == BAD_VAR_NUM) ||
        (!compiler->compLocallocUsed && (compiler->funCurrentFunc()->funKind == FUNC_ROOT)))
    {
#ifndef UNIX_X86_ABI
        inst_Mov(TYP_I_IMPL, REG_ARG_0, REG_SPBASE, /* canSkip */ false);
#endif // !UNIX_X86_ABI
    }
    else
    {
        GetEmitter()->emitIns_R_S(ins_Load(TYP_I_IMPL), EA_PTRSIZE, REG_ARG_0, compiler->lvaPSPSym, 0);
    }
    GetEmitter()->emitIns_J(INS_call, block->bbJumpDest);

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
// TODO-Linux-x86: Do we need to handle the GC information for this NOP or JMP specially, as is done for other
// architectures?
#ifndef JIT32_GCENCODER
        // Because of the way the flowgraph is connected, the liveness info for this one instruction
        // after the call is not (can not be) correct in cases where a variable has a last use in the
        // handler.  So turn off GC reporting for this single instruction.
        GetEmitter()->emitDisableGC();
#endif // JIT32_GCENCODER

        // Now go to where the finally funclet needs to return to.
        if (block->bbNext->bbJumpDest == block->bbNext->bbNext)
        {
            // Fall-through.
            // TODO-XArch-CQ: Can we get rid of this instruction, and just have the call return directly
            // to the next instruction? This would depend on stack walking from within the finally
            // handler working without this instruction being in this special EH region.
            instGen(INS_nop);
        }
        else
        {
            inst_JMP(EJ_jmp, block->bbNext->bbJumpDest);
        }

#ifndef JIT32_GCENCODER
        GetEmitter()->emitEnableGC();
#endif // JIT32_GCENCODER
    }

#else // !FEATURE_EH_FUNCLETS

    // If we are about to invoke a finally locally from a try block, we have to set the ShadowSP slot
    // corresponding to the finally's nesting level. When invoked in response to an exception, the
    // EE does this.
    //
    // We have a BBJ_CALLFINALLY followed by a BBJ_ALWAYS.
    //
    // We will emit :
    //      mov [ebp - (n + 1)], 0
    //      mov [ebp -  n     ], 0xFC
    //      push &step
    //      jmp  finallyBlock
    // ...
    // step:
    //      mov [ebp -  n     ], 0
    //      jmp leaveTarget
    // ...
    // leaveTarget:

    noway_assert(isFramePointerUsed());

    // Get the nesting level which contains the finally
    unsigned finallyNesting = 0;
    compiler->fgGetNestingLevel(block, &finallyNesting);

    // The last slot is reserved for ICodeManager::FixContext(ppEndRegion)
    unsigned filterEndOffsetSlotOffs;
    filterEndOffsetSlotOffs = (unsigned)(compiler->lvaLclSize(compiler->lvaShadowSPslotsVar) - TARGET_POINTER_SIZE);

    unsigned curNestingSlotOffs;
    curNestingSlotOffs = (unsigned)(filterEndOffsetSlotOffs - ((finallyNesting + 1) * TARGET_POINTER_SIZE));

    // Zero out the slot for the next nesting level
    GetEmitter()->emitIns_S_I(INS_mov, EA_PTRSIZE, compiler->lvaShadowSPslotsVar,
                              curNestingSlotOffs - TARGET_POINTER_SIZE, 0);
    GetEmitter()->emitIns_S_I(INS_mov, EA_PTRSIZE, compiler->lvaShadowSPslotsVar, curNestingSlotOffs, LCL_FINALLY_MARK);

    // Now push the address where the finally funclet should return to directly.
    if (!(block->bbFlags & BBF_RETLESS_CALL))
    {
        assert(block->isBBCallAlwaysPair());
        GetEmitter()->emitIns_J(INS_push_hide, block->bbNext->bbJumpDest);
    }
    else
    {
        // EE expects a DWORD, so we provide 0
        inst_IV(INS_push_hide, 0);
    }

    // Jump to the finally BB
    inst_JMP(EJ_jmp, block->bbJumpDest);

#endif // !FEATURE_EH_FUNCLETS

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

#if defined(FEATURE_EH_FUNCLETS)
void CodeGen::genEHCatchRet(BasicBlock* block)
{
    // Set RAX to the address the VM should return to after the catch.
    // Generate a RIP-relative
    //         lea reg, [rip + disp32] ; the RIP is implicit
    // which will be position-independent.
    GetEmitter()->emitIns_R_L(INS_lea, EA_PTR_DSP_RELOC, block->bbJumpDest, REG_INTRET);
}

#else // !FEATURE_EH_FUNCLETS

void CodeGen::genEHFinallyOrFilterRet(BasicBlock* block)
{
    // The last statement of the block must be a GT_RETFILT, which has already been generated.
    assert(block->lastNode() != nullptr);
    assert(block->lastNode()->OperGet() == GT_RETFILT);

    if (block->bbJumpKind == BBJ_EHFINALLYRET)
    {
        assert(block->lastNode()->AsOp()->gtOp1 == nullptr); // op1 == nullptr means endfinally

        // Return using a pop-jmp sequence. As the "try" block calls
        // the finally with a jmp, this leaves the x86 call-ret stack
        // balanced in the normal flow of path.

        noway_assert(isFramePointerRequired());
        inst_RV(INS_pop_hide, REG_EAX, TYP_I_IMPL);
        inst_RV(INS_i_jmp, REG_EAX, TYP_I_IMPL);
    }
    else
    {
        assert(block->bbJumpKind == BBJ_EHFILTERRET);

        // The return value has already been computed.
        instGen_Return(0);
    }
}

#endif // !FEATURE_EH_FUNCLETS

//  Move an immediate value into an integer register

void CodeGen::instGen_Set_Reg_To_Imm(emitAttr  size,
                                     regNumber reg,
                                     ssize_t   imm,
                                     insFlags flags DEBUGARG(size_t targetHandle) DEBUGARG(unsigned gtFlags))
{
    // reg cannot be a FP register
    assert(!genIsValidFloatReg(reg));

    if (!compiler->opts.compReloc)
    {
        size = EA_SIZE(size); // Strip any Reloc flags from size if we aren't doing relocs
    }

    if ((imm == 0) && !EA_IS_RELOC(size))
    {
        instGen_Set_Reg_To_Zero(size, reg, flags);
    }
    else
    {
        if (genDataIndirAddrCanBeEncodedAsPCRelOffset(imm))
        {
            emitAttr newSize = EA_PTR_DSP_RELOC;
            if (EA_IS_BYREF(size))
            {
                newSize = EA_SET_FLG(newSize, EA_BYREF_FLG);
            }

            GetEmitter()->emitIns_R_AI(INS_lea, newSize, reg, imm);
        }
        else
        {
            GetEmitter()->emitIns_R_I(INS_mov, size, reg, imm);
        }
    }
    regSet.verifyRegUsed(reg);
}

/***********************************************************************************
 *
 * Generate code to set a register 'targetReg' of type 'targetType' to the constant
 * specified by the constant (GT_CNS_INT or GT_CNS_DBL) in 'tree'. This does not call
 * genProduceReg() on the target register.
 */
void CodeGen::genSetRegToConst(regNumber targetReg, var_types targetType, GenTree* tree)
{
    switch (tree->gtOper)
    {
        case GT_CNS_INT:
        {
            // relocatable values tend to come down as a CNS_INT of native int type
            // so the line between these two opcodes is kind of blurry
            GenTreeIntConCommon* con    = tree->AsIntConCommon();
            ssize_t              cnsVal = con->IconValue();

            if (con->ImmedValNeedsReloc(compiler))
            {
                emitAttr size = EA_HANDLE_CNS_RELOC;

                if (targetType == TYP_BYREF)
                {
                    size = EA_SET_FLG(size, EA_BYREF_FLG);
                }

                instGen_Set_Reg_To_Imm(size, targetReg, cnsVal);
                regSet.verifyRegUsed(targetReg);
            }
            else
            {
                genSetRegToIcon(targetReg, cnsVal, targetType);
            }
        }
        break;

        case GT_CNS_DBL:
        {
            emitter* emit       = GetEmitter();
            emitAttr size       = emitTypeSize(targetType);
            double   constValue = tree->AsDblCon()->gtDconVal;

            // Make sure we use "xorps reg, reg" only for +ve zero constant (0.0) and not for -ve zero (-0.0)
            if (*(__int64*)&constValue == 0)
            {
                // A faster/smaller way to generate 0
                emit->emitIns_R_R(INS_xorps, size, targetReg, targetReg);
            }
            else
            {
                CORINFO_FIELD_HANDLE hnd = emit->emitFltOrDblConst(constValue, size);
                emit->emitIns_R_C(ins_Load(targetType), size, targetReg, hnd, 0);
            }
        }
        break;

        default:
            unreached();
    }
}

//------------------------------------------------------------------------
// genCodeForNegNot: Produce code for a GT_NEG/GT_NOT node.
//
// Arguments:
//    tree - the node
//
void CodeGen::genCodeForNegNot(GenTree* tree)
{
    assert(tree->OperIs(GT_NEG, GT_NOT));

    regNumber targetReg  = tree->GetRegNum();
    var_types targetType = tree->TypeGet();

    if (varTypeIsFloating(targetType))
    {
        assert(tree->gtOper == GT_NEG);
        genSSE2BitwiseOp(tree);
    }
    else
    {
        GenTree* operand = tree->gtGetOp1();
        assert(operand->isUsedFromReg());
        regNumber operandReg = genConsumeReg(operand);

        inst_Mov(targetType, targetReg, operandReg, /* canSkip */ true);

        instruction ins = genGetInsForOper(tree->OperGet(), targetType);
        inst_RV(ins, targetReg, targetType);
    }

    genProduceReg(tree);
}

//------------------------------------------------------------------------
// genCodeForBswap: Produce code for a GT_BSWAP / GT_BSWAP16 node.
//
// Arguments:
//    tree - the node
//
void CodeGen::genCodeForBswap(GenTree* tree)
{
    // TODO: If we're swapping immediately after a read from memory or immediately before
    // a write to memory, use the MOVBE instruction instead of the BSWAP instruction if
    // the platform supports it.

    assert(tree->OperIs(GT_BSWAP, GT_BSWAP16));

    regNumber targetReg  = tree->GetRegNum();
    var_types targetType = tree->TypeGet();

    GenTree* operand = tree->gtGetOp1();
    assert(operand->isUsedFromReg());
    regNumber operandReg = genConsumeReg(operand);

    inst_Mov(targetType, targetReg, operandReg, /* canSkip */ true);

    if (tree->OperIs(GT_BSWAP))
    {
        // 32-bit and 64-bit byte swaps use "bswap reg"
        inst_RV(INS_bswap, targetReg, targetType);
    }
    else
    {
        // 16-bit byte swaps use "ror reg.16, 8"
        inst_RV_IV(INS_ror_N, targetReg, 8 /* val */, EA_2BYTE);
    }

    genProduceReg(tree);
}

// Produce code for a GT_INC_SATURATE node.
void CodeGen::genCodeForIncSaturate(GenTree* tree)
{
    regNumber targetReg  = tree->GetRegNum();
    var_types targetType = tree->TypeGet();

    GenTree* operand = tree->gtGetOp1();
    assert(operand->isUsedFromReg());
    regNumber operandReg = genConsumeReg(operand);

    inst_Mov(targetType, targetReg, operandReg, /* canSkip */ true);
    inst_RV_IV(INS_add, targetReg, 1, emitActualTypeSize(targetType));
    inst_RV_IV(INS_sbb, targetReg, 0, emitActualTypeSize(targetType));

    genProduceReg(tree);
}

// Generate code to get the high N bits of a N*N=2N bit multiplication result
void CodeGen::genCodeForMulHi(GenTreeOp* treeNode)
{
    assert(!treeNode->gtOverflowEx());

    regNumber targetReg  = treeNode->GetRegNum();
    var_types targetType = treeNode->TypeGet();
    emitter*  emit       = GetEmitter();
    emitAttr  size       = emitTypeSize(treeNode);
    GenTree*  op1        = treeNode->AsOp()->gtOp1;
    GenTree*  op2        = treeNode->AsOp()->gtOp2;

    // to get the high bits of the multiply, we are constrained to using the
    // 1-op form:  RDX:RAX = RAX * rm
    // The 3-op form (Rx=Ry*Rz) does not support it.

    genConsumeOperands(treeNode->AsOp());

    GenTree* regOp = op1;
    GenTree* rmOp  = op2;

    // Set rmOp to the memory operand (if any)
    if (op1->isUsedFromMemory() || (op2->isUsedFromReg() && (op2->GetRegNum() == REG_RAX)))
    {
        regOp = op2;
        rmOp  = op1;
    }
    assert(regOp->isUsedFromReg());

    // Setup targetReg when neither of the source operands was a matching register
    inst_Mov(targetType, REG_RAX, regOp->GetRegNum(), /* canSkip */ true);

    instruction ins;
    if ((treeNode->gtFlags & GTF_UNSIGNED) == 0)
    {
        ins = INS_imulEAX;
    }
    else
    {
        ins = INS_mulEAX;
    }
    emit->emitInsBinary(ins, size, treeNode, rmOp);

    // Move the result to the desired register, if necessary
    if (treeNode->OperGet() == GT_MULHI)
    {
        inst_Mov(targetType, targetReg, REG_RDX, /* canSkip */ true);
    }

    genProduceReg(treeNode);
}

#ifdef TARGET_X86
//------------------------------------------------------------------------
// genCodeForLongUMod: Generate code for a tree of the form
//                     `(umod (gt_long x y) (const int))`
//
// Arguments:
//   node - the node for which to generate code
//
void CodeGen::genCodeForLongUMod(GenTreeOp* node)
{
    assert(node != nullptr);
    assert(node->OperGet() == GT_UMOD);
    assert(node->TypeGet() == TYP_INT);

    GenTreeOp* const dividend = node->gtOp1->AsOp();
    assert(dividend->OperGet() == GT_LONG);
    assert(varTypeIsLong(dividend));

    genConsumeOperands(node);

    GenTree* const dividendLo = dividend->gtOp1;
    GenTree* const dividendHi = dividend->gtOp2;
    assert(dividendLo->isUsedFromReg());
    assert(dividendHi->isUsedFromReg());

    GenTree* const divisor = node->gtOp2;
    assert(divisor->gtSkipReloadOrCopy()->OperGet() == GT_CNS_INT);
    assert(divisor->gtSkipReloadOrCopy()->isUsedFromReg());
    assert(divisor->gtSkipReloadOrCopy()->AsIntCon()->gtIconVal >= 2);
    assert(divisor->gtSkipReloadOrCopy()->AsIntCon()->gtIconVal <= 0x3fffffff);

    // dividendLo must be in RAX; dividendHi must be in RDX
    genCopyRegIfNeeded(dividendLo, REG_EAX);
    genCopyRegIfNeeded(dividendHi, REG_EDX);

    // At this point, EAX:EDX contains the 64bit dividend and op2->GetRegNum()
    // contains the 32bit divisor. We want to generate the following code:
    //
    //   cmp edx, divisor->GetRegNum()
    //   jb noOverflow
    //
    //   mov temp, eax
    //   mov eax, edx
    //   xor edx, edx
    //   div divisor->GetRegNum()
    //   mov eax, temp
    //
    // noOverflow:
    //   div divisor->GetRegNum()
    //
    // This works because (a * 2^32 + b) % c = ((a % c) * 2^32 + b) % c.

    BasicBlock* const noOverflow = genCreateTempLabel();

    //   cmp edx, divisor->GetRegNum()
    //   jb noOverflow
    inst_RV_RV(INS_cmp, REG_EDX, divisor->GetRegNum(), TYP_I_IMPL);
    inst_JMP(EJ_jb, noOverflow);

    //   mov temp, eax
    //   mov eax, edx
    //   xor edx, edx
    //   div divisor->GetRegNum()
    //   mov eax, temp
    const regNumber tempReg = node->GetSingleTempReg();
    inst_Mov(TYP_INT, tempReg, REG_EAX, /* canSkip */ false);
    inst_Mov(TYP_INT, REG_EAX, REG_EDX, /* canSkip */ false);
    instGen_Set_Reg_To_Zero(EA_PTRSIZE, REG_EDX);
    inst_RV(INS_div, divisor->GetRegNum(), TYP_INT);
    inst_Mov(TYP_INT, REG_EAX, tempReg, /* canSkip */ false);

    // noOverflow:
    //   div divisor->GetRegNum()
    genDefineTempLabel(noOverflow);
    inst_RV(INS_div, divisor->GetRegNum(), TYP_INT);

    const regNumber targetReg = node->GetRegNum();
    inst_Mov(TYP_INT, targetReg, REG_RDX, /* canSkip */ true);
    genProduceReg(node);
}
#endif // TARGET_X86

//------------------------------------------------------------------------
// genCodeForDivMod: Generate code for a DIV or MOD operation.
//
// Arguments:
//    treeNode - the node to generate the code for
//
void CodeGen::genCodeForDivMod(GenTreeOp* treeNode)
{
    assert(treeNode->OperIs(GT_DIV, GT_UDIV, GT_MOD, GT_UMOD));

    GenTree* dividend = treeNode->gtOp1;

#ifdef TARGET_X86
    if (varTypeIsLong(dividend->TypeGet()))
    {
        genCodeForLongUMod(treeNode);
        return;
    }
#endif // TARGET_X86

    GenTree*   divisor    = treeNode->gtOp2;
    genTreeOps oper       = treeNode->OperGet();
    emitAttr   size       = emitTypeSize(treeNode);
    regNumber  targetReg  = treeNode->GetRegNum();
    var_types  targetType = treeNode->TypeGet();
    emitter*   emit       = GetEmitter();

    // Node's type must be int/native int, small integer types are not
    // supported and floating point types are handled by genCodeForBinary.
    assert(varTypeIsIntOrI(targetType));
    // dividend is in a register.
    assert(dividend->isUsedFromReg());

    genConsumeOperands(treeNode->AsOp());
    // dividend must be in RAX
    genCopyRegIfNeeded(dividend, REG_RAX);

    // zero or sign extend rax to rdx
    if (oper == GT_UMOD || oper == GT_UDIV ||
        (dividend->IsIntegralConst() && (dividend->AsIntConCommon()->IconValue() > 0)))
    {
        instGen_Set_Reg_To_Zero(EA_PTRSIZE, REG_EDX);
    }
    else
    {
        emit->emitIns(INS_cdq, size);
        // the cdq instruction writes RDX, So clear the gcInfo for RDX
        gcInfo.gcMarkRegSetNpt(RBM_RDX);
    }

    // Perform the 'targetType' (64-bit or 32-bit) divide instruction
    instruction ins;
    if (oper == GT_UMOD || oper == GT_UDIV)
    {
        ins = INS_div;
    }
    else
    {
        ins = INS_idiv;
    }

    emit->emitInsBinary(ins, size, treeNode, divisor);

    // DIV/IDIV instructions always store the quotient in RAX and the remainder in RDX.
    // Move the result to the desired register, if necessary
    if (oper == GT_DIV || oper == GT_UDIV)
    {
        inst_Mov(targetType, targetReg, REG_RAX, /* canSkip */ true);
    }
    else
    {
        assert((oper == GT_MOD) || (oper == GT_UMOD));
        inst_Mov(targetType, targetReg, REG_RDX, /* canSkip */ true);
    }
    genProduceReg(treeNode);
}

//------------------------------------------------------------------------
// genCodeForBinary: Generate code for many binary arithmetic operators
//
// Arguments:
//    treeNode - The binary operation for which we are generating code.
//
// Return Value:
//    None.
//
// Notes:
//    Integer MUL and DIV variants have special constraints on x64 so are not handled here.
//    See the assert below for the operators that are handled.

void CodeGen::genCodeForBinary(GenTreeOp* treeNode)
{
#ifdef DEBUG
    bool isValidOper = treeNode->OperIs(GT_ADD, GT_SUB);
    if (varTypeIsFloating(treeNode->TypeGet()))
    {
        isValidOper |= treeNode->OperIs(GT_MUL, GT_DIV);
    }
    else
    {
        isValidOper |= treeNode->OperIs(GT_AND, GT_OR, GT_XOR);
#ifndef TARGET_64BIT
        isValidOper |= treeNode->OperIs(GT_ADD_LO, GT_ADD_HI, GT_SUB_LO, GT_SUB_HI);
#endif
    }
    assert(isValidOper);
#endif

    genConsumeOperands(treeNode);

    const genTreeOps oper       = treeNode->OperGet();
    regNumber        targetReg  = treeNode->GetRegNum();
    var_types        targetType = treeNode->TypeGet();
    emitter*         emit       = GetEmitter();

    GenTree* op1 = treeNode->gtGetOp1();
    GenTree* op2 = treeNode->gtGetOp2();

    assert(IsValidSourceType(targetType, op1->GetType()));
    assert(IsValidSourceType(targetType, op2->GetType()));

    // Commutative operations can mark op1 as contained or reg-optional to generate "op reg, memop/immed"
    if (!op1->isUsedFromReg())
    {
        assert(treeNode->OperIsCommutative());
        assert(op1->isMemoryOp() || op1->IsLocal() || op1->IsCnsNonZeroFltOrDbl() || op1->IsIntCnsFitsInI32() ||
               op1->IsRegOptional());

        op1 = treeNode->gtGetOp2();
        op2 = treeNode->gtGetOp1();
    }

    instruction ins = genGetInsForOper(treeNode->OperGet(), targetType);

    // The arithmetic node must be sitting in a register (since it's not contained)
    noway_assert(targetReg != REG_NA);

    regNumber op1reg = op1->isUsedFromReg() ? op1->GetRegNum() : REG_NA;
    regNumber op2reg = op2->isUsedFromReg() ? op2->GetRegNum() : REG_NA;

    if (varTypeIsFloating(treeNode->TypeGet()))
    {
        // floating-point addition, subtraction, multiplication, and division
        // all have RMW semantics if VEX support is not available

        bool isRMW = !compiler->canUseVexEncoding();
        inst_RV_RV_TT(ins, emitTypeSize(treeNode), targetReg, op1reg, op2, isRMW);

        genProduceReg(treeNode);
        return;
    }

    GenTree* dst;
    GenTree* src;

    // This is the case of reg1 = reg1 op reg2
    // We're ready to emit the instruction without any moves
    if (op1reg == targetReg)
    {
        dst = op1;
        src = op2;
    }
    // We have reg1 = reg2 op reg1
    // In order for this operation to be correct
    // we need that op is a commutative operation so
    // we can convert it into reg1 = reg1 op reg2 and emit
    // the same code as above
    else if (op2reg == targetReg)
    {
        noway_assert(GenTree::OperIsCommutative(oper));
        dst = op2;
        src = op1;
    }
    // now we know there are 3 different operands so attempt to use LEA
    else if (oper == GT_ADD && !varTypeIsFloating(treeNode) && !treeNode->gtOverflowEx() // LEA does not set flags
             && (op2->isContainedIntOrIImmed() || op2->isUsedFromReg()) && !treeNode->gtSetFlags())
    {
        if (op2->isContainedIntOrIImmed())
        {
            emit->emitIns_R_AR(INS_lea, emitTypeSize(treeNode), targetReg, op1reg,
                               (int)op2->AsIntConCommon()->IconValue());
        }
        else
        {
            assert(op2reg != REG_NA);
            emit->emitIns_R_ARX(INS_lea, emitTypeSize(treeNode), targetReg, op1reg, op2reg, 1, 0);
        }
        genProduceReg(treeNode);
        return;
    }
    // dest, op1 and op2 registers are different:
    // reg3 = reg1 op reg2
    // We can implement this by issuing a mov:
    // reg3 = reg1
    // reg3 = reg3 op reg2
    else
    {
        var_types op1Type = op1->TypeGet();
        inst_Mov(op1Type, targetReg, op1reg, /* canSkip */ false);
        regSet.verifyRegUsed(targetReg);
        gcInfo.gcMarkRegPtrVal(targetReg, op1Type);
        dst = treeNode;
        src = op2;
    }

    // try to use an inc or dec
    if (oper == GT_ADD && !varTypeIsFloating(treeNode) && src->isContainedIntOrIImmed() && !treeNode->gtOverflowEx())
    {
        if (src->IsIntegralConst(1))
        {
            emit->emitIns_R(INS_inc, emitTypeSize(treeNode), targetReg);
            genProduceReg(treeNode);
            return;
        }
        else if (src->IsIntegralConst(-1))
        {
            emit->emitIns_R(INS_dec, emitTypeSize(treeNode), targetReg);
            genProduceReg(treeNode);
            return;
        }
    }
    regNumber r = emit->emitInsBinary(ins, emitTypeSize(treeNode), dst, src);
    noway_assert(r == targetReg);

    if (treeNode->gtOverflowEx())
    {
#if !defined(TARGET_64BIT)
        assert(oper == GT_ADD || oper == GT_SUB || oper == GT_ADD_HI || oper == GT_SUB_HI);
#else
        assert(oper == GT_ADD || oper == GT_SUB);
#endif
        genCheckOverflow(treeNode);
    }
    genProduceReg(treeNode);
}

//------------------------------------------------------------------------
// genCodeForMul: Generate code for a MUL operation.
//
// Arguments:
//    treeNode - the node to generate the code for
//
void CodeGen::genCodeForMul(GenTreeOp* treeNode)
{
    assert(treeNode->OperIs(GT_MUL));

    regNumber targetReg  = treeNode->GetRegNum();
    var_types targetType = treeNode->TypeGet();
    emitter*  emit       = GetEmitter();

    // Node's type must be int or long (only on x64), small integer types are not
    // supported and floating point types are handled by genCodeForBinary.
    assert(varTypeIsIntOrI(targetType));

    instruction ins;
    emitAttr    size                  = emitTypeSize(treeNode);
    bool        isUnsignedMultiply    = ((treeNode->gtFlags & GTF_UNSIGNED) != 0);
    bool        requiresOverflowCheck = treeNode->gtOverflowEx();

    GenTree* op1 = treeNode->gtGetOp1();
    GenTree* op2 = treeNode->gtGetOp2();

    // there are 3 forms of x64 multiply:
    // 1-op form with 128 result:  RDX:RAX = RAX * rm
    // 2-op form: reg *= rm
    // 3-op form: reg = rm * imm

    genConsumeOperands(treeNode);

    // This matches the 'mul' lowering in Lowering::SetMulOpCounts()
    //
    // immOp :: Only one operand can be an immediate
    // rmOp  :: Only one operand can be a memory op.
    // regOp :: A register op (especially the operand that matches 'targetReg')
    //          (can be nullptr when we have both a memory op and an immediate op)

    GenTree* immOp = nullptr;
    GenTree* rmOp  = op1;
    GenTree* regOp;

    if (op2->isContainedIntOrIImmed())
    {
        immOp = op2;
    }
    else if (op1->isContainedIntOrIImmed())
    {
        immOp = op1;
        rmOp  = op2;
    }

    if (immOp != nullptr)
    {
        // CQ: When possible use LEA for mul by imm 3, 5 or 9
        ssize_t imm = immOp->AsIntConCommon()->IconValue();

        if (!requiresOverflowCheck && rmOp->isUsedFromReg() && ((imm == 3) || (imm == 5) || (imm == 9)))
        {
            // We will use the LEA instruction to perform this multiply
            // Note that an LEA with base=x, index=x and scale=(imm-1) computes x*imm when imm=3,5 or 9.
            unsigned int scale = (unsigned int)(imm - 1);
            GetEmitter()->emitIns_R_ARX(INS_lea, size, targetReg, rmOp->GetRegNum(), rmOp->GetRegNum(), scale, 0);
        }
        else if (!requiresOverflowCheck && rmOp->isUsedFromReg() && (imm == genFindLowestBit(imm)) && (imm != 0))
        {
            // Use shift for constant multiply when legal
            uint64_t     zextImm     = static_cast<uint64_t>(static_cast<size_t>(imm));
            unsigned int shiftAmount = genLog2(zextImm);

            // Copy reg src to dest register
            inst_Mov(targetType, targetReg, rmOp->GetRegNum(), /* canSkip */ true);

            inst_RV_SH(INS_shl, size, targetReg, shiftAmount);
        }
        else
        {
            // use the 3-op form with immediate
            ins = GetEmitter()->inst3opImulForReg(targetReg);
            emit->emitInsBinary(ins, size, rmOp, immOp);
        }
    }
    else // we have no contained immediate operand
    {
        regOp = op1;
        rmOp  = op2;

        regNumber mulTargetReg = targetReg;
        if (isUnsignedMultiply && requiresOverflowCheck)
        {
            ins          = INS_mulEAX;
            mulTargetReg = REG_RAX;
        }
        else
        {
            ins = INS_imul;
        }

        // Set rmOp to the memory operand (if any)
        // or set regOp to the op2 when it has the matching target register for our multiply op
        //
        if (op1->isUsedFromMemory() || (op2->isUsedFromReg() && (op2->GetRegNum() == mulTargetReg)))
        {
            regOp = op2;
            rmOp  = op1;
        }
        assert(regOp->isUsedFromReg());

        // Setup targetReg when neither of the source operands was a matching register
        inst_Mov(targetType, mulTargetReg, regOp->GetRegNum(), /* canSkip */ true);

        emit->emitInsBinary(ins, size, treeNode, rmOp);

        // Move the result to the desired register, if necessary
        if (ins == INS_mulEAX)
        {
            inst_Mov(targetType, targetReg, REG_RAX, /* canSkip */ true);
        }
    }

    if (requiresOverflowCheck)
    {
        // Overflow checking is only used for non-floating point types
        noway_assert(!varTypeIsFloating(treeNode));

        genCheckOverflow(treeNode);
    }

    genProduceReg(treeNode);
}

#ifdef FEATURE_SIMD

void CodeGen::genMultiRegSIMDReturn(GenTree* src)
{
    assert(varTypeIsSIMD(src->GetType()));
    assert(src->isUsedFromReg());

    regNumber srcReg  = src->GetRegNum();
    regNumber retReg0 = compiler->info.retDesc.GetRegNum(0);
    regNumber retReg1 = compiler->info.retDesc.GetRegNum(1);

#ifdef TARGET_AMD64
    GetEmitter()->emitIns_Mov(INS_movaps, EA_16BYTE, retReg0, srcReg, /*canSkip*/ true);

    if (compiler->canUseVexEncoding())
    {
        GetEmitter()->emitIns_R_R_R(INS_unpckhpd, EA_16BYTE, retReg1, srcReg, srcReg);
    }
    else
    {
        GetEmitter()->emitIns_Mov(INS_movaps, EA_16BYTE, retReg1, srcReg, /*canSkip*/ true);
        GetEmitter()->emitIns_R_R(INS_unpckhpd, EA_16BYTE, retReg1, retReg1);
    }
#else
    assert(src->TypeIs(TYP_SIMD8));
    assert((retReg0 == REG_EAX) && (retReg1 == REG_EDX));

    GetEmitter()->emitIns_Mov(INS_movd, EA_4BYTE, retReg0, srcReg, /* canSkip */ false);

    if (compiler->compOpportunisticallyDependsOn(InstructionSet_SSE41))
    {
        GetEmitter()->emitIns_R_R_I(INS_pextrd, EA_4BYTE, retReg1, srcReg, 1);
    }
    else
    {
        GetEmitter()->emitIns_R_I(INS_psrldq, EA_16BYTE, srcReg, 4);
        GetEmitter()->emitIns_Mov(INS_movd, EA_4BYTE, retReg1, srcReg, /* canSkip */ false);
    }
#endif
}

#endif // FEATURE_SIMD

#ifdef TARGET_X86

void CodeGen::genFloatReturn(GenTree* src)
{
    assert(varTypeIsFloating(src->GetType()));

    var_types srcType = src->GetType();
    emitAttr  srcSize = emitTypeSize(srcType);
    regNumber srcReg  = genConsumeReg(src);

    // Spill the return value register from an XMM register to the stack, then load it on the x87 stack.
    // If it already has a home location, use that. Otherwise, we need a temp.

    if (genIsRegCandidateLclVar(src) && compiler->lvaGetDesc(src->AsLclVar())->lvOnFrame)
    {
        if (compiler->lvaGetDesc(src->AsLclVar())->GetRegNum() != REG_STK)
        {
            src->gtFlags |= GTF_SPILL;
            GetEmitter()->emitIns_S_R(ins_Store(srcType), srcSize, srcReg, src->AsLclVar()->GetLclNum(), 0);
        }

        GetEmitter()->emitIns_S(INS_fld, srcSize, src->AsLclVar()->GetLclNum(), 0);
    }
    else
    {
        // Spill the value, which should be in a register, then load it to the fp stack.
        // TODO-X86-CQ: Deal with things that are already in memory (don't call genConsumeReg yet).

        src->gtFlags |= GTF_SPILL;
        regSet.rsSpillTree(srcReg, src);
        src->gtFlags |= GTF_SPILLED;
        src->gtFlags &= ~GTF_SPILL;

        TempDsc* temp = regSet.rsUnspillInPlace(src, srcReg);
        GetEmitter()->emitIns_S(INS_fld, srcSize, temp->tdTempNum(), 0);
        src->gtFlags &= ~GTF_SPILLED;
        regSet.tmpRlsTemp(temp);
    }
}

#endif // TARGET_X86

//------------------------------------------------------------------------
// genCodeForCompare: Produce code for a GT_EQ/GT_NE/GT_LT/GT_LE/GT_GE/GT_GT/GT_TEST_EQ/GT_TEST_NE/GT_CMP node.
//
// Arguments:
//    tree - the node
//
void CodeGen::genCodeForCompare(GenTreeOp* tree)
{
    assert(tree->OperIs(GT_EQ, GT_NE, GT_LT, GT_LE, GT_GE, GT_GT, GT_TEST_EQ, GT_TEST_NE, GT_CMP));

    // TODO-XArch-CQ: Check if we can use the currently set flags.
    // TODO-XArch-CQ: Check for the case where we can simply transfer the carry bit to a register
    //         (signed < or >= where targetReg != REG_NA)

    GenTree*  op1     = tree->gtOp1;
    var_types op1Type = op1->TypeGet();

    if (varTypeIsFloating(op1Type))
    {
        genCompareFloat(tree);
    }
    else
    {
        genCompareInt(tree);
    }
}

//------------------------------------------------------------------------
// genCodeForBT: Generates code for a GT_BT node.
//
// Arguments:
//    tree - The node.
//
void CodeGen::genCodeForBT(GenTreeOp* bt)
{
    assert(bt->OperIs(GT_BT));

    GenTree*  op1  = bt->gtGetOp1();
    GenTree*  op2  = bt->gtGetOp2();
    var_types type = genActualType(op1->TypeGet());

    assert(op1->isUsedFromReg() && op2->isUsedFromReg());
    assert((genTypeSize(type) >= genTypeSize(TYP_INT)) && (genTypeSize(type) <= genTypeSize(TYP_I_IMPL)));

    genConsumeOperands(bt);
    // Note that the emitter doesn't fully support INS_bt, it only supports the reg,reg
    // form and encodes the registers in reverse order. To get the correct order we need
    // to reverse the operands when calling emitIns_R_R.
    GetEmitter()->emitIns_R_R(INS_bt, emitTypeSize(type), op2->GetRegNum(), op1->GetRegNum());
}

// clang-format off
const CodeGen::GenConditionDesc CodeGen::GenConditionDesc::map[32]
{
    { },        // NONE
    { },        // 1
    { EJ_jl  }, // SLT
    { EJ_jle }, // SLE
    { EJ_jge }, // SGE
    { EJ_jg  }, // SGT
    { EJ_js  }, // S
    { EJ_jns }, // NS

    { EJ_je  }, // EQ
    { EJ_jne }, // NE
    { EJ_jb  }, // ULT
    { EJ_jbe }, // ULE
    { EJ_jae }, // UGE
    { EJ_ja  }, // UGT
    { EJ_jb  }, // C
    { EJ_jae }, // NC

    // Floating point compare instructions (UCOMISS, UCOMISD etc.) set the condition flags as follows:
    //    ZF PF CF  Meaning
    //   ---------------------
    //    1  1  1   Unordered
    //    0  0  0   Greater
    //    0  0  1   Less Than
    //    1  0  0   Equal
    //
    // Since ZF and CF are also set when the result is unordered, in some cases we first need to check
    // PF before checking ZF/CF. In general, ordered conditions will result in a jump only if PF is not
    // set and unordered conditions will result in a jump only if PF is set.

    { EJ_jnp, GT_AND, EJ_je  }, // FEQ
    { EJ_jne                 }, // FNE
    { EJ_jnp, GT_AND, EJ_jb  }, // FLT
    { EJ_jnp, GT_AND, EJ_jbe }, // FLE
    { EJ_jae                 }, // FGE
    { EJ_ja                  }, // FGT
    { EJ_jo                  }, // O
    { EJ_jno                 }, // NO

    { EJ_je                }, // FEQU
    { EJ_jp, GT_OR, EJ_jne }, // FNEU
    { EJ_jb                }, // FLTU
    { EJ_jbe               }, // FLEU
    { EJ_jp, GT_OR, EJ_jae }, // FGEU
    { EJ_jp, GT_OR, EJ_ja  }, // FGTU
    { EJ_jp                }, // P
    { EJ_jnp               }, // NP
};
// clang-format on

//------------------------------------------------------------------------
// inst_SETCC: Generate code to set a register to 0 or 1 based on a condition.
//
// Arguments:
//   condition - The condition
//   type      - The type of the value to be produced
//   dstReg    - The destination register to be set to 1 or 0
//
void CodeGen::inst_SETCC(GenCondition condition, var_types type, regNumber dstReg)
{
    assert(varTypeIsIntegral(type));
    assert(genIsValidIntReg(dstReg) && isByteReg(dstReg));

    const GenConditionDesc& desc = GenConditionDesc::Get(condition);

    inst_SET(desc.jumpKind1, dstReg);

    if (desc.oper != GT_NONE)
    {
        BasicBlock* labelNext = genCreateTempLabel();
        inst_JMP((desc.oper == GT_OR) ? desc.jumpKind1 : emitter::emitReverseJumpKind(desc.jumpKind1), labelNext);
        inst_SET(desc.jumpKind2, dstReg);
        genDefineTempLabel(labelNext);
    }

    if (!varTypeIsByte(type))
    {
        GetEmitter()->emitIns_Mov(INS_movzx, EA_1BYTE, dstReg, dstReg, /* canSkip */ false);
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

    GenTree* data = tree->gtOp1;
    genConsumeRegs(data);
    GenTreeIntCon cns = intForm(TYP_INT, 0);
    cns.SetContained();
    GetEmitter()->emitInsBinary(INS_cmp, emitTypeSize(TYP_INT), data, &cns);

    BasicBlock* skipLabel = genCreateTempLabel();

    inst_JMP(EJ_je, skipLabel);

    // emit the call to the EE-helper that stops for GC (or other reasons)
    regNumber tmpReg = tree->GetSingleTempReg(RBM_ALLINT);
    assert(genIsValidIntReg(tmpReg));

    genEmitHelperCall(CORINFO_HELP_STOP_FOR_GC, 0, EA_UNKNOWN, tmpReg);
    genDefineTempLabel(skipLabel);
}

/*****************************************************************************
 *
 * Generate code for a single node in the tree.
 * Preconditions: All operands have been evaluated
 *
 */
void CodeGen::genCodeForTreeNode(GenTree* treeNode)
{
    regNumber targetReg;
#if !defined(TARGET_64BIT)
    if (treeNode->TypeGet() == TYP_LONG)
    {
        // All long enregistered nodes will have been decomposed into their
        // constituent lo and hi nodes.
        targetReg = REG_NA;
    }
    else
#endif // !defined(TARGET_64BIT)
    {
        targetReg = treeNode->GetRegNum();
    }
    var_types targetType = treeNode->TypeGet();
    emitter*  emit       = GetEmitter();

#ifdef DEBUG
    // Validate that all the operands for the current node are consumed in order.
    // This is important because LSRA ensures that any necessary copies will be
    // handled correctly.
    lastConsumedNode = nullptr;
    if (compiler->verbose)
    {
        unsigned seqNum = treeNode->gtSeqNum; // Useful for setting a conditional break in Visual Studio
        compiler->gtDispLIRNode(treeNode, "Generating: ");
    }
#endif // DEBUG

    // Is this a node whose value is already in a register?  LSRA denotes this by
    // setting the GTF_REUSE_REG_VAL flag.
    if (treeNode->IsReuseRegVal())
    {
        // For now, this is only used for constant nodes.
        assert(treeNode->OperIsConst() || treeNode->IsHWIntrinsicZero());
        JITDUMP("  TreeNode is marked ReuseReg\n");
        return;
    }

    // contained nodes are part of their parents for codegen purposes
    // ex : immediates, most LEAs
    if (treeNode->isContained())
    {
        return;
    }

    switch (treeNode->gtOper)
    {
#ifndef JIT32_GCENCODER
        case GT_START_NONGC:
            GetEmitter()->emitDisableGC();
            break;
#endif // !defined(JIT32_GCENCODER)

        case GT_START_PREEMPTGC:
            // Kill callee saves GC registers, and create a label
            // so that information gets propagated to the emitter.
            gcInfo.gcMarkRegSetNpt(RBM_INT_CALLEE_SAVED);
            genDefineTempLabel(genCreateTempLabel());
            break;

        case GT_PROF_HOOK:
#ifdef PROFILING_SUPPORTED
            // We should be seeing this only if profiler hook is needed
            noway_assert(compiler->compIsProfilerHookNeeded());

            // Right now this node is used only for tail calls. In future if
            // we intend to use it for Enter or Leave hooks, add a data member
            // to this node indicating the kind of profiler hook. For example,
            // helper number can be used.
            genProfilingLeaveCallback(CORINFO_HELP_PROF_FCN_TAILCALL);
#endif // PROFILING_SUPPORTED
            break;

        case GT_LCLHEAP:
            genLclHeap(treeNode);
            break;

        case GT_CNS_INT:
#ifdef TARGET_X86
            assert(!treeNode->IsIconHandle(GTF_ICON_TLS_HDL));
#endif // TARGET_X86
            FALLTHROUGH;

        case GT_CNS_DBL:
            genSetRegToConst(targetReg, targetType, treeNode);
            genProduceReg(treeNode);
            break;

        case GT_NOT:
        case GT_NEG:
            genCodeForNegNot(treeNode);
            break;

        case GT_BSWAP:
        case GT_BSWAP16:
            genCodeForBswap(treeNode);
            break;

        case GT_DIV:
            if (varTypeIsFloating(treeNode->TypeGet()))
            {
                genCodeForBinary(treeNode->AsOp());
                break;
            }
            FALLTHROUGH;
        case GT_MOD:
        case GT_UMOD:
        case GT_UDIV:
            genCodeForDivMod(treeNode->AsOp());
            break;

        case GT_OR:
        case GT_XOR:
        case GT_AND:
            assert(varTypeIsIntegralOrI(treeNode));

            FALLTHROUGH;

#if !defined(TARGET_64BIT)
        case GT_ADD_LO:
        case GT_ADD_HI:
        case GT_SUB_LO:
        case GT_SUB_HI:
#endif // !defined(TARGET_64BIT)

        case GT_ADD:
        case GT_SUB:
            genCodeForBinary(treeNode->AsOp());
            break;

        case GT_MUL:
            if (varTypeIsFloating(treeNode->TypeGet()))
            {
                genCodeForBinary(treeNode->AsOp());
                break;
            }
            genCodeForMul(treeNode->AsOp());
            break;

        case GT_LSH:
        case GT_RSH:
        case GT_RSZ:
        case GT_ROL:
        case GT_ROR:
            genCodeForShift(treeNode);
            break;

#if !defined(TARGET_64BIT)

        case GT_LSH_HI:
        case GT_RSH_LO:
            genCodeForShiftLong(treeNode);
            break;

#endif // !defined(TARGET_64BIT)

        case GT_CAST:
            genCodeForCast(treeNode->AsCast());
            break;

        case GT_BITCAST:
            genCodeForBitCast(treeNode->AsOp());
            break;

        case GT_LCL_FLD_ADDR:
        case GT_LCL_VAR_ADDR:
            genCodeForLclAddr(treeNode->AsLclVarCommon());
            break;

        case GT_LCL_FLD:
            genCodeForLclFld(treeNode->AsLclFld());
            break;

        case GT_LCL_VAR:
            genCodeForLclVar(treeNode->AsLclVar());
            break;

        case GT_STORE_LCL_FLD:
            GenStoreLclFld(treeNode->AsLclFld());
            break;

        case GT_STORE_LCL_VAR:
            GenStoreLclVar(treeNode->AsLclVar());
            break;

        case GT_RETFILT:
            genRetFilt(treeNode);
            break;

        case GT_RETURN:
            genReturn(treeNode);
            break;

        case GT_LEA:
            // If we are here, it is the case where there is an LEA that cannot be folded into a parent instruction.
            genLeaInstruction(treeNode->AsAddrMode());
            break;

        case GT_INDEX_ADDR:
            genCodeForIndexAddr(treeNode->AsIndexAddr());
            break;

        case GT_IND:
            genCodeForIndir(treeNode->AsIndir());
            break;

        case GT_INC_SATURATE:
            genCodeForIncSaturate(treeNode);
            break;

        case GT_MULHI:
#ifdef TARGET_X86
        case GT_MUL_LONG:
#endif
            genCodeForMulHi(treeNode->AsOp());
            break;

        case GT_INTRINSIC:
            genIntrinsic(treeNode);
            break;

#ifdef FEATURE_SIMD
        case GT_SIMD_UPPER_SPILL:
            genSIMDUpperSpill(treeNode->AsUnOp());
            break;
        case GT_SIMD_UPPER_UNSPILL:
            genSIMDUpperUnspill(treeNode->AsUnOp());
            break;
#endif

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            genHWIntrinsic(treeNode->AsHWIntrinsic());
            break;
#endif // FEATURE_HW_INTRINSICS

        case GT_CKFINITE:
            genCkfinite(treeNode);
            break;

        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GE:
        case GT_GT:
        case GT_TEST_EQ:
        case GT_TEST_NE:
        case GT_CMP:
            genCodeForCompare(treeNode->AsOp());
            break;

        case GT_JTRUE:
            genCodeForJumpTrue(treeNode->AsOp());
            break;

        case GT_JCC:
            genCodeForJcc(treeNode->AsCC());
            break;

        case GT_SETCC:
            genCodeForSetcc(treeNode->AsCC());
            break;

        case GT_BT:
            genCodeForBT(treeNode->AsOp());
            break;

        case GT_RETURNTRAP:
            genCodeForReturnTrap(treeNode->AsOp());
            break;

        case GT_STOREIND:
            genCodeForStoreInd(treeNode->AsStoreInd());
            break;

        case GT_COPY:
            // This is handled at the time we call genConsumeReg() on the GT_COPY
            break;

        case GT_FIELD_LIST:
            // Should always be marked contained.
            assert(!"FIELD_LIST nodes should always be marked contained.");
            break;

        case GT_SWAP:
            genCodeForSwap(treeNode->AsOp());
            break;

        case GT_PUTARG_STK:
            genPutArgStk(treeNode->AsPutArgStk());
            break;

        case GT_PUTARG_REG:
            genPutArgReg(treeNode->AsUnOp());
            break;

        case GT_CALL:
            genCallInstruction(treeNode->AsCall());
            break;

        case GT_JMP:
            genJmpMethod(treeNode);
            break;

        case GT_LOCKADD:
            genCodeForLockAdd(treeNode->AsOp());
            break;

        case GT_XCHG:
        case GT_XADD:
            genLockedInstructions(treeNode->AsOp());
            break;

        case GT_XORR:
        case GT_XAND:
            NYI("Interlocked.Or and Interlocked.And aren't implemented for x86 yet.");
            break;

        case GT_MEMORYBARRIER:
        {
            CodeGen::BarrierKind barrierKind =
                treeNode->gtFlags & GTF_MEMORYBARRIER_LOAD ? BARRIER_LOAD_ONLY : BARRIER_FULL;

            instGen_MemoryBarrier(barrierKind);
            break;
        }

        case GT_CMPXCHG:
            genCodeForCmpXchg(treeNode->AsCmpXchg());
            break;

        case GT_RELOAD:
            // do nothing - reload is just a marker.
            // The parent node will call genConsumeReg on this which will trigger the unspill of this node's child
            // into the register specified in this node.
            break;

        case GT_NOP:
            break;

        case GT_KEEPALIVE:
            genConsumeRegs(treeNode->AsOp()->gtOp1);
            break;

        case GT_NO_OP:
            GetEmitter()->emitIns_Nop(1);
            break;

        case GT_ARR_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
        case GT_HW_INTRINSIC_CHK:
#endif
            genRangeCheck(treeNode->AsBoundsChk());
            break;

        case GT_PHYSREG:
            genCodeForPhysReg(treeNode->AsPhysReg());
            break;

        case GT_NULLCHECK:
            genCodeForNullCheck(treeNode->AsIndir());
            break;

        case GT_CATCH_ARG:

            noway_assert(handlerGetsXcptnObj(compiler->compCurBB->bbCatchTyp));

            /* Catch arguments get passed in a register. genCodeForBBlist()
               would have marked it as holding a GC object, but not used. */

            noway_assert(gcInfo.gcRegGCrefSetCur & RBM_EXCEPTION_OBJECT);
            genConsumeReg(treeNode);
            break;

#if !defined(FEATURE_EH_FUNCLETS)
        case GT_END_LFIN:

            // Have to clear the ShadowSP of the nesting level which encloses the finally. Generates:
            //     mov dword ptr [ebp-0xC], 0  // for some slot of the ShadowSP local var

            size_t finallyNesting;
            finallyNesting = treeNode->AsVal()->gtVal1;
            noway_assert(treeNode->AsVal()->gtVal1 < compiler->compHndBBtabCount);
            noway_assert(finallyNesting < compiler->compHndBBtabCount);

            // The last slot is reserved for ICodeManager::FixContext(ppEndRegion)
            unsigned filterEndOffsetSlotOffs;
            PREFIX_ASSUME(compiler->lvaLclSize(compiler->lvaShadowSPslotsVar) >
                          TARGET_POINTER_SIZE); // below doesn't underflow.
            filterEndOffsetSlotOffs =
                (unsigned)(compiler->lvaLclSize(compiler->lvaShadowSPslotsVar) - TARGET_POINTER_SIZE);

            size_t curNestingSlotOffs;
            curNestingSlotOffs = filterEndOffsetSlotOffs - ((finallyNesting + 1) * TARGET_POINTER_SIZE);
            GetEmitter()->emitIns_S_I(INS_mov, EA_PTRSIZE, compiler->lvaShadowSPslotsVar, (unsigned)curNestingSlotOffs,
                                      0);
            break;
#endif // !FEATURE_EH_FUNCLETS

        case GT_PINVOKE_PROLOG:
            noway_assert(((gcInfo.gcRegGCrefSetCur | gcInfo.gcRegByrefSetCur) & ~fullIntArgRegMask()) == 0);

#ifdef PSEUDORANDOM_NOP_INSERTION
            // the runtime side requires the codegen here to be consistent
            emit->emitDisableRandomNops();
#endif // PSEUDORANDOM_NOP_INSERTION
            break;

        case GT_LABEL:
            genPendingCallLabel = genCreateTempLabel();
            emit->emitIns_R_L(INS_lea, EA_PTR_DSP_RELOC, genPendingCallLabel, treeNode->GetRegNum());
            break;

        case GT_STORE_OBJ:
        case GT_STORE_DYN_BLK:
        case GT_STORE_BLK:
            genStructStore(treeNode->AsBlk());
            break;

        case GT_JMPTABLE:
            genJumpTable(treeNode);
            break;

        case GT_SWITCH_TABLE:
            genTableBasedSwitch(treeNode);
            break;

        case GT_ARR_INDEX:
            genCodeForArrIndex(treeNode->AsArrIndex());
            break;

        case GT_ARR_OFFSET:
            genCodeForArrOffset(treeNode->AsArrOffs());
            break;

        case GT_CLS_VAR_ADDR:
            emit->emitIns_R_C(INS_lea, EA_PTRSIZE, targetReg, treeNode->AsClsVar()->gtClsVarHnd, 0);
            genProduceReg(treeNode);
            break;

#if !defined(TARGET_64BIT)
        case GT_LONG:
            assert(treeNode->isUsedFromReg());
            genConsumeRegs(treeNode);
            break;
#endif

        case GT_IL_OFFSET:
            // Do nothing; these nodes are simply markers for debug info.
            break;

        case GT_INSTR:
            genCodeForInstr(treeNode->AsInstr());
            break;

        default:
        {
#ifdef DEBUG
            char message[256];
            _snprintf_s(message, _countof(message), _TRUNCATE, "NYI: Unimplemented node type %s\n",
                        GenTree::OpName(treeNode->OperGet()));
            NYIRAW(message);
#endif
            assert(!"Unknown node in codegen");
        }
        break;
    }
}

#ifdef FEATURE_SIMD

void CodeGen::GenStoreLclVarMultiRegSIMD(GenTreeLclVar* store)
{
#if !defined(UNIX_AMD64_ABI) && !defined(TARGET_X86)
    assert(!"Multireg store to SIMD reg not supported on X64 Windows");
#else
    GenTree* src = store->GetOp(0);

    // This is used to store a Vector3/4 call return value, on UNIX_AMD64_ABI
    // such a value is returned into 2 XMM registers and we need to pack it
    // into the XMM destination register.

    GenTreeCall* call = src->gtSkipReloadOrCopy()->AsCall();

    assert(call->GetRegCount() == 2);
#ifdef TARGET_X86
    assert(!varTypeUsesFloatReg(call->GetRegType(0)));
    assert(!varTypeUsesFloatReg(call->GetRegType(1)));
#else
    assert(varTypeUsesFloatReg(call->GetRegType(0)));
    assert(varTypeUsesFloatReg(call->GetRegType(1)));
#endif

    genConsumeRegs(src);

    regNumber retReg0 = call->GetRegNumByIdx(0);
    regNumber retReg1 = call->GetRegNumByIdx(1);

    if (src->IsCopyOrReload())
    {
        // COPY/RELOAD will have valid reg for those positions
        // that need to be copied or reloaded.

        regNumber reloadReg = src->AsCopyOrReload()->GetRegNumByIdx(0);
        if (reloadReg != REG_NA)
        {
            retReg0 = reloadReg;
        }

        reloadReg = src->AsCopyOrReload()->GetRegNumByIdx(1);
        if (reloadReg != REG_NA)
        {
            retReg1 = reloadReg;
        }
    }

    regNumber dstReg = store->GetRegNum();

#ifdef TARGET_X86
    regNumber tmpReg = store->GetSingleTempReg();

    GetEmitter()->emitIns_Mov(INS_movd, EA_4BYTE, dstReg, retReg0, false);
    GetEmitter()->emitIns_Mov(INS_movd, EA_4BYTE, tmpReg, retReg1, false);
    GetEmitter()->emitIns_R_R(INS_unpcklps, EA_16BYTE, dstReg, tmpReg);
#else
    if (dstReg == retReg0)
    {
        GetEmitter()->emitIns_R_R(INS_movlhps, EA_16BYTE, dstReg, retReg1);
    }
    else if (compiler->canUseVexEncoding())
    {
        GetEmitter()->emitIns_R_R_R(INS_unpcklpd, EA_16BYTE, dstReg, retReg0, retReg1);
    }
    else if (dstReg == retReg1)
    {
        GetEmitter()->emitIns_R_R(INS_movlhps, EA_16BYTE, dstReg, retReg1);
        GetEmitter()->emitIns_Mov(INS_movsdsse2, EA_16BYTE, dstReg, retReg0, /* canSkip */ false);
    }
    else
    {
        GetEmitter()->emitIns_Mov(INS_movaps, EA_16BYTE, dstReg, retReg0, /* canSkip */ false);
        GetEmitter()->emitIns_R_R(INS_movlhps, EA_16BYTE, dstReg, retReg1);
    }
#endif

    genProduceReg(store);
#endif // UNIX_AMD64_ABI
}

#endif // FEATURE_SIMD

//------------------------------------------------------------------------
// genAllocLclFrame: Probe the stack and allocate the local stack frame - subtract from SP.
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
    assert(compiler->compGeneratingProlog);

    if (frameSize == 0)
    {
        return;
    }

    const target_size_t pageSize = compiler->eeGetPageSize();

    if (frameSize == REGSIZE_BYTES)
    {
        // Frame size is the same as register size.
        GetEmitter()->emitIns_R(INS_push, EA_PTRSIZE, REG_EAX);
        compiler->unwindAllocStack(frameSize);
    }
    else if (frameSize < pageSize)
    {
        GetEmitter()->emitIns_R_I(INS_sub, EA_PTRSIZE, REG_SPBASE, frameSize);
        compiler->unwindAllocStack(frameSize);

        const unsigned lastProbedLocToFinalSp = frameSize;

        if (lastProbedLocToFinalSp + STACK_PROBE_BOUNDARY_THRESHOLD_BYTES > pageSize)
        {
            // We haven't probed almost a complete page. If the next action on the stack might subtract from SP
            // first, before touching the current SP, then we need to probe at the very bottom. This can
            // happen on x86, for example, when we copy an argument to the stack using a "SUB ESP; REP MOV"
            // strategy.
            GetEmitter()->emitIns_R_AR(INS_test, EA_4BYTE, REG_EAX, REG_SPBASE, 0);
        }
    }
    else
    {
#ifdef TARGET_X86
        int spOffset = -(int)frameSize;

        if (compiler->info.compPublishStubParam)
        {
            GetEmitter()->emitIns_R(INS_push, EA_PTRSIZE, REG_SECRET_STUB_PARAM);
            spOffset += REGSIZE_BYTES;
        }

        GetEmitter()->emitIns_R_AR(INS_lea, EA_PTRSIZE, REG_STACK_PROBE_HELPER_ARG, REG_SPBASE, spOffset);
        regSet.verifyRegUsed(REG_STACK_PROBE_HELPER_ARG);

        genEmitHelperCall(CORINFO_HELP_STACK_PROBE, 0, EA_UNKNOWN);

        if (compiler->info.compPublishStubParam)
        {
            GetEmitter()->emitIns_R(INS_pop, EA_PTRSIZE, REG_SECRET_STUB_PARAM);
            GetEmitter()->emitIns_R_I(INS_sub, EA_PTRSIZE, REG_SPBASE, frameSize);
        }
        else
        {
            GetEmitter()->emitIns_Mov(INS_mov, EA_PTRSIZE, REG_SPBASE, REG_STACK_PROBE_HELPER_ARG, /* canSkip */ false);
        }
#else  // !TARGET_X86
        static_assert_no_msg((RBM_STACK_PROBE_HELPER_ARG & (RBM_SECRET_STUB_PARAM | RBM_DEFAULT_HELPER_CALL_TARGET)) ==
                             RBM_NONE);

        GetEmitter()->emitIns_R_AR(INS_lea, EA_PTRSIZE, REG_STACK_PROBE_HELPER_ARG, REG_SPBASE, -(int)frameSize);
        regSet.verifyRegUsed(REG_STACK_PROBE_HELPER_ARG);

        genEmitHelperCall(CORINFO_HELP_STACK_PROBE, 0, EA_UNKNOWN);

        if (initReg == REG_DEFAULT_HELPER_CALL_TARGET)
        {
            *pInitRegZeroed = false;
        }

        static_assert_no_msg((RBM_STACK_PROBE_HELPER_TRASH & RBM_STACK_PROBE_HELPER_ARG) == RBM_NONE);

        GetEmitter()->emitIns_Mov(INS_mov, EA_PTRSIZE, REG_SPBASE, REG_STACK_PROBE_HELPER_ARG, /* canSkip */ false);
#endif // !TARGET_X86

        compiler->unwindAllocStack(frameSize);

        if (initReg == REG_STACK_PROBE_HELPER_ARG)
        {
            *pInitRegZeroed = false;
        }
    }

#ifdef USING_SCOPE_INFO
    if (!doubleAlignOrFramePointerUsed())
    {
        psiAdjustStackLevel(frameSize);
    }
#endif // USING_SCOPE_INFO
}

//------------------------------------------------------------------------
// genStackPointerConstantAdjustment: add a specified constant value to the stack pointer.
// No probe is done.
//
// Arguments:
//    spDelta                 - the value to add to SP. Must be negative or zero.
//    regTmp                  - x86 only: an available temporary register. If not REG_NA, hide the SP
//                              adjustment from the emitter, using this register.
//
// Return Value:
//    None.
//
void CodeGen::genStackPointerConstantAdjustment(ssize_t spDelta, regNumber regTmp)
{
    assert(spDelta < 0);

    // We assert that the SP change is less than one page. If it's greater, you should have called a
    // function that does a probe, which will in turn call this function.
    assert((target_size_t)(-spDelta) <= compiler->eeGetPageSize());

#ifdef TARGET_X86
    if (regTmp != REG_NA)
    {
        // For x86, some cases don't want to use "sub ESP" because we don't want the emitter to track the adjustment
        // to ESP. So do the work in the count register.
        // TODO-CQ: manipulate ESP directly, to share code, reduce #ifdefs, and improve CQ. This would require
        // creating a way to temporarily turn off the emitter's tracking of ESP, maybe marking instrDescs as "don't
        // track".
        inst_Mov(TYP_I_IMPL, regTmp, REG_SPBASE, /* canSkip */ false);
        inst_RV_IV(INS_sub, regTmp, (target_ssize_t)-spDelta, EA_PTRSIZE);
        inst_Mov(TYP_I_IMPL, REG_SPBASE, regTmp, /* canSkip */ false);
    }
    else
#endif // TARGET_X86
    {
        inst_RV_IV(INS_sub, REG_SPBASE, (target_ssize_t)-spDelta, EA_PTRSIZE);
    }
}

//------------------------------------------------------------------------
// genStackPointerConstantAdjustmentWithProbe: add a specified constant value to the stack pointer,
// and probe the stack as appropriate. Should only be called as a helper for
// genStackPointerConstantAdjustmentLoopWithProbe.
//
// Arguments:
//    spDelta                 - the value to add to SP. Must be negative or zero. If zero, the probe happens,
//                              but the stack pointer doesn't move.
//    regTmp                  - x86 only: an available temporary register. If not REG_NA, hide the SP
//                              adjustment from the emitter, using this register.
//
// Return Value:
//    None.
//
void CodeGen::genStackPointerConstantAdjustmentWithProbe(ssize_t spDelta, regNumber regTmp)
{
    GetEmitter()->emitIns_AR_R(INS_test, EA_4BYTE, REG_SPBASE, REG_SPBASE, 0);
    genStackPointerConstantAdjustment(spDelta, regTmp);
}

//------------------------------------------------------------------------
// genStackPointerConstantAdjustmentLoopWithProbe: Add a specified constant value to the stack pointer,
// and probe the stack as appropriate. Generates one probe per page, up to the total amount required.
// This will generate a sequence of probes in-line. It is required for the case where we need to expose
// (not hide) the stack level adjustment. We can't use the dynamic loop in that case, because the total
// stack adjustment would not be visible to the emitter. It would be possible to use this version for
// multiple hidden constant stack level adjustments but we don't do that currently (we use the loop
// version in genStackPointerDynamicAdjustmentWithProbe instead).
//
// Arguments:
//    spDelta                 - the value to add to SP. Must be negative.
//    regTmp                  - x86 only: an available temporary register. If not REG_NA, hide the SP
//                              adjustment from the emitter, using this register.
//
// Return Value:
//    Offset in bytes from SP to last probed address.
//
target_ssize_t CodeGen::genStackPointerConstantAdjustmentLoopWithProbe(ssize_t spDelta, regNumber regTmp)
{
    assert(spDelta < 0);

    const target_size_t pageSize = compiler->eeGetPageSize();

    ssize_t spRemainingDelta = spDelta;
    do
    {
        ssize_t spOneDelta = -(ssize_t)min((target_size_t)-spRemainingDelta, pageSize);
        genStackPointerConstantAdjustmentWithProbe(spOneDelta, regTmp);
        spRemainingDelta -= spOneDelta;
    } while (spRemainingDelta < 0);

    // What offset from the final SP was the last probe? This depends on the fact that
    // genStackPointerConstantAdjustmentWithProbe() probes first, then does "SUB SP".
    target_size_t lastTouchDelta = (target_size_t)(-spDelta) % pageSize;
    if ((lastTouchDelta == 0) || (lastTouchDelta + STACK_PROBE_BOUNDARY_THRESHOLD_BYTES > pageSize))
    {
        // We haven't probed almost a complete page. If lastTouchDelta==0, then spDelta was an exact
        // multiple of pageSize, which means we last probed exactly one page back. Otherwise, we probed
        // the page, but very far from the end. If the next action on the stack might subtract from SP
        // first, before touching the current SP, then we do one more probe at the very bottom. This can
        // happen on x86, for example, when we copy an argument to the stack using a "SUB ESP; REP MOV"
        // strategy.

        GetEmitter()->emitIns_AR_R(INS_test, EA_PTRSIZE, REG_EAX, REG_SPBASE, 0);
        lastTouchDelta = 0;
    }

    return lastTouchDelta;
}

//------------------------------------------------------------------------
// genStackPointerDynamicAdjustmentWithProbe: add a register value to the stack pointer,
// and probe the stack as appropriate.
//
// Note that for x86, we hide the ESP adjustment from the emitter. To do that, currently,
// requires a temporary register and extra code.
//
// Arguments:
//    regSpDelta              - the register value to add to SP. The value in this register must be negative.
//                              This register might be trashed.
//    regTmp                  - an available temporary register. Will be trashed.
//
// Return Value:
//    None.
//
void CodeGen::genStackPointerDynamicAdjustmentWithProbe(regNumber regSpDelta, regNumber regTmp)
{
    assert(regSpDelta != REG_NA);
    assert(regTmp != REG_NA);

    // Tickle the pages to ensure that ESP is always valid and is
    // in sync with the "stack guard page".  Note that in the worst
    // case ESP is on the last byte of the guard page.  Thus you must
    // touch ESP-0 first not ESP-0x1000.
    //
    // Another subtlety is that you don't want ESP to be exactly on the
    // boundary of the guard page because PUSH is predecrement, thus
    // call setup would not touch the guard page but just beyond it.
    //
    // Note that we go through a few hoops so that ESP never points to
    // illegal pages at any time during the tickling process
    //
    //       add   regSpDelta, ESP          // reg now holds ultimate ESP
    //       jb    loop                     // result is smaller than original ESP (no wrap around)
    //       xor   regSpDelta, regSpDelta   // Overflow, pick lowest possible number
    //  loop:
    //       test  ESP, [ESP+0]             // tickle the page
    //       mov   regTmp, ESP
    //       sub   regTmp, eeGetPageSize()
    //       mov   ESP, regTmp
    //       cmp   ESP, regSpDelta
    //       jae   loop
    //       mov   ESP, regSpDelta

    BasicBlock* loop = genCreateTempLabel();

    inst_RV_RV(INS_add, regSpDelta, REG_SPBASE, TYP_I_IMPL);
    inst_JMP(EJ_jb, loop);

    instGen_Set_Reg_To_Zero(EA_PTRSIZE, regSpDelta);

    genDefineTempLabel(loop);

    // Tickle the decremented value. Note that it must be done BEFORE the update of ESP since ESP might already
    // be on the guard page. It is OK to leave the final value of ESP on the guard page.
    GetEmitter()->emitIns_AR_R(INS_test, EA_4BYTE, REG_SPBASE, REG_SPBASE, 0);

    // Subtract a page from ESP. This is a trick to avoid the emitter trying to track the
    // decrement of the ESP - we do the subtraction in another reg instead of adjusting ESP directly.
    inst_Mov(TYP_I_IMPL, regTmp, REG_SPBASE, /* canSkip */ false);
    inst_RV_IV(INS_sub, regTmp, compiler->eeGetPageSize(), EA_PTRSIZE);
    inst_Mov(TYP_I_IMPL, REG_SPBASE, regTmp, /* canSkip */ false);

    inst_RV_RV(INS_cmp, REG_SPBASE, regSpDelta, TYP_I_IMPL);
    inst_JMP(EJ_jae, loop);

    // Move the final value to ESP
    inst_Mov(TYP_I_IMPL, REG_SPBASE, regSpDelta, /* canSkip */ false);
}

//------------------------------------------------------------------------
// genLclHeap: Generate code for localloc.
//
// Arguments:
//      tree - the localloc tree to generate.
//
// Notes:
//      Note that for x86, we don't track ESP movements while generating the localloc code.
//      The ESP tracking is used to report stack pointer-relative GC info, which is not
//      interesting while doing the localloc construction. Also, for functions with localloc,
//      we have EBP frames, and EBP-relative locals, and ESP-relative accesses only for function
//      call arguments.
//
//      For x86, we store the ESP after the localloc is complete in the LocAllocSP
//      variable. This variable is implicitly reported to the VM in the GC info (its position
//      is defined by convention relative to other items), and is used by the GC to find the
//      "base" stack pointer in functions with localloc.
//
void CodeGen::genLclHeap(GenTree* tree)
{
    assert(tree->OperGet() == GT_LCLHEAP);
    assert(compiler->compLocallocUsed);

    GenTree* size = tree->AsOp()->gtOp1;
    noway_assert((genActualType(size->gtType) == TYP_INT) || (genActualType(size->gtType) == TYP_I_IMPL));

    regNumber      targetReg      = tree->GetRegNum();
    regNumber      regCnt         = REG_NA;
    var_types      type           = genActualType(size->gtType);
    emitAttr       easz           = emitTypeSize(type);
    BasicBlock*    endLabel       = nullptr;
    target_ssize_t lastTouchDelta = (target_ssize_t)-1;

#ifdef DEBUG
    if (compiler->opts.compStackCheckOnRet)
    {
        genStackPointerCheck(compiler->lvaReturnSpCheck);
    }
#endif

    noway_assert(isFramePointerUsed()); // localloc requires Frame Pointer to be established since SP changes
    noway_assert(genStackLevel == 0);   // Can't have anything on the stack

    unsigned stackAdjustment = 0;

    // compute the amount of memory to allocate to properly STACK_ALIGN.
    size_t amount = 0;
    if (size->IsCnsIntOrI())
    {
        // If size is a constant, then it must be contained.
        assert(size->isContained());

        // If amount is zero then return null in targetReg
        amount = size->AsIntCon()->gtIconVal;
        if (amount == 0)
        {
            instGen_Set_Reg_To_Zero(EA_PTRSIZE, targetReg);
            goto BAILOUT;
        }

        // 'amount' is the total number of bytes to localloc to properly STACK_ALIGN
        amount = AlignUp(amount, STACK_ALIGN);
    }
    else
    {
        // The localloc requested memory size is non-constant.

        // Put the size value in targetReg. If it is zero, bail out by returning null in targetReg.
        genConsumeRegAndCopy(size, targetReg);
        endLabel = genCreateTempLabel();
        GetEmitter()->emitIns_R_R(INS_test, easz, targetReg, targetReg);
        inst_JMP(EJ_je, endLabel);

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

            // Above, we put the size in targetReg. Now, copy it to our new temp register if necessary.
            inst_Mov(size->TypeGet(), regCnt, targetReg, /* canSkip */ true);
        }

        // Round up the number of bytes to allocate to a STACK_ALIGN boundary. This is done
        // by code like:
        //      add reg, 15
        //      and reg, -16
        // However, in the initialized memory case, we need the count of STACK_ALIGN-sized
        // elements, not a byte count, after the alignment. So instead of the "and", which
        // becomes unnecessary, generate a shift, e.g.:
        //      add reg, 15
        //      shr reg, 4

        inst_RV_IV(INS_add, regCnt, STACK_ALIGN - 1, emitActualTypeSize(type));

        if (compiler->info.compInitMem)
        {
            // Convert the count from a count of bytes to a loop count. We will loop once per
            // stack alignment size, so each loop will zero 4 bytes on Windows/x86, and 16 bytes
            // on x64 and Linux/x86.
            //
            // Note that we zero a single reg-size word per iteration on x86, and 2 reg-size
            // words per iteration on x64. We will shift off all the stack alignment bits
            // added above, so there is no need for an 'and' instruction.

            // --- shr regCnt, 2 (or 4) ---
            inst_RV_SH(INS_shr, EA_PTRSIZE, regCnt, STACK_ALIGN_SHIFT);
        }
        else
        {
            // Otherwise, mask off the low bits to align the byte count.
            inst_RV_IV(INS_and, regCnt, ~(STACK_ALIGN - 1), emitActualTypeSize(type));
        }
    }

#if FEATURE_FIXED_OUT_ARGS
    // If we have an outgoing arg area then we must adjust the SP by popping off the
    // outgoing arg area. We will restore it right before we return from this method.
    //
    // Localloc returns stack space that aligned to STACK_ALIGN bytes. The following
    // are the cases that need to be handled:
    //   i) Method has out-going arg area.
    //      It is guaranteed that size of out-going arg area is STACK_ALIGN'ed (see fgMorphArgs).
    //      Therefore, we will pop off the out-going arg area from RSP before allocating the localloc space.
    //  ii) Method has no out-going arg area.
    //      Nothing to pop off from the stack.
    if (compiler->lvaOutgoingArgSpaceSize > 0)
    {
        assert((compiler->lvaOutgoingArgSpaceSize % STACK_ALIGN) == 0); // This must be true for the stack to remain
                                                                        // aligned
        inst_RV_IV(INS_add, REG_SPBASE, compiler->lvaOutgoingArgSpaceSize, EA_PTRSIZE);
        stackAdjustment += compiler->lvaOutgoingArgSpaceSize;
    }
#endif

    if (size->IsCnsIntOrI())
    {
        // We should reach here only for non-zero, constant size allocations.
        assert(amount > 0);
        assert((amount % STACK_ALIGN) == 0);
        assert((amount % REGSIZE_BYTES) == 0);

        // For small allocations we will generate up to six push 0 inline
        size_t cntRegSizedWords = amount / REGSIZE_BYTES;
        if (cntRegSizedWords <= 6)
        {
            for (; cntRegSizedWords != 0; cntRegSizedWords--)
            {
                inst_IV(INS_push_hide, 0); // push_hide means don't track the stack
            }

            lastTouchDelta = 0;

            goto ALLOC_DONE;
        }

        bool initMemOrLargeAlloc =
            compiler->info.compInitMem || (amount >= compiler->eeGetPageSize()); // must be >= not >

#ifdef TARGET_X86
        bool needRegCntRegister = true;
#else  // !TARGET_X86
        bool needRegCntRegister = initMemOrLargeAlloc;
#endif // !TARGET_X86

        if (needRegCntRegister)
        {
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
        }

        if (!initMemOrLargeAlloc)
        {
            // Since the size is less than a page, and we don't need to zero init memory, simply adjust ESP.
            // ESP might already be in the guard page, so we must touch it BEFORE
            // the alloc, not after.

            assert(amount < compiler->eeGetPageSize()); // must be < not <=
            lastTouchDelta = genStackPointerConstantAdjustmentLoopWithProbe(-(ssize_t)amount, regCnt);
            goto ALLOC_DONE;
        }

        // else, "mov regCnt, amount"

        if (compiler->info.compInitMem)
        {
            // When initializing memory, we want 'amount' to be the loop count.
            assert((amount % STACK_ALIGN) == 0);
            amount /= STACK_ALIGN;
        }

        genSetRegToIcon(regCnt, amount, ((int)amount == amount) ? TYP_INT : TYP_LONG);
    }

    if (compiler->info.compInitMem)
    {
        // At this point 'regCnt' is set to the number of loop iterations for this loop, if each
        // iteration zeros (and subtracts from the stack pointer) STACK_ALIGN bytes.
        // Since we have to zero out the allocated memory AND ensure that RSP is always valid
        // by tickling the pages, we will just push 0's on the stack.

        assert(genIsValidIntReg(regCnt));

        // Loop:
        BasicBlock* loop = genCreateTempLabel();
        genDefineTempLabel(loop);

        static_assert_no_msg((STACK_ALIGN % REGSIZE_BYTES) == 0);
        unsigned const count = (STACK_ALIGN / REGSIZE_BYTES);

        for (unsigned i = 0; i < count; i++)
        {
            inst_IV(INS_push_hide, 0); // --- push REG_SIZE bytes of 0
        }
        // Note that the stack must always be aligned to STACK_ALIGN bytes

        // Decrement the loop counter and loop if not done.
        inst_RV(INS_dec, regCnt, TYP_I_IMPL);
        inst_JMP(EJ_jne, loop);

        lastTouchDelta = 0;
    }
    else
    {
        // At this point 'regCnt' is set to the total number of bytes to localloc.
        // Negate this value before calling the function to adjust the stack (which
        // adds to ESP).

        inst_RV(INS_neg, regCnt, TYP_I_IMPL);
        regNumber regTmp = tree->GetSingleTempReg();
        genStackPointerDynamicAdjustmentWithProbe(regCnt, regTmp);

        // lastTouchDelta is dynamic, and can be up to a page. So if we have outgoing arg space,
        // we're going to assume the worst and probe.
    }

ALLOC_DONE:
    // Re-adjust SP to allocate out-going arg area. Note: this also requires probes, if we have
    // a very large stack adjustment! For simplicity, we use the same function used elsewhere,
    // which probes the current address before subtracting. We may end up probing multiple
    // times relatively "nearby".
    if (stackAdjustment > 0)
    {
        assert((stackAdjustment % STACK_ALIGN) == 0); // This must be true for the stack to remain aligned
        assert(lastTouchDelta >= -1);

        if ((lastTouchDelta == (target_ssize_t)-1) ||
            (stackAdjustment + (unsigned)lastTouchDelta + STACK_PROBE_BOUNDARY_THRESHOLD_BYTES >
             compiler->eeGetPageSize()))
        {
            genStackPointerConstantAdjustmentLoopWithProbe(-(ssize_t)stackAdjustment, REG_NA);
        }
        else
        {
            genStackPointerConstantAdjustment(-(ssize_t)stackAdjustment, REG_NA);
        }
    }

    // Return the stackalloc'ed address in result register.
    // TargetReg = RSP + stackAdjustment.
    GetEmitter()->emitIns_R_AR(INS_lea, EA_PTRSIZE, targetReg, REG_SPBASE, stackAdjustment);

    if (endLabel != nullptr)
    {
        genDefineTempLabel(endLabel);
    }

BAILOUT:

#ifdef JIT32_GCENCODER
    if (compiler->lvaLocAllocSPvar != BAD_VAR_NUM)
    {
        GetEmitter()->emitIns_S_R(ins_Store(TYP_I_IMPL), EA_PTRSIZE, REG_SPBASE, compiler->lvaLocAllocSPvar, 0);
    }
#endif // JIT32_GCENCODER

#ifdef DEBUG
    // Update local variable to reflect the new stack pointer.
    if (compiler->opts.compStackCheckOnRet)
    {
        noway_assert(compiler->lvaReturnSpCheck != 0xCCCCCCCC &&
                     compiler->lvaTable[compiler->lvaReturnSpCheck].lvDoNotEnregister &&
                     compiler->lvaTable[compiler->lvaReturnSpCheck].lvOnFrame);
        GetEmitter()->emitIns_S_R(ins_Store(TYP_I_IMPL), EA_PTRSIZE, REG_SPBASE, compiler->lvaReturnSpCheck, 0);
    }
#endif

    genProduceReg(tree);
}

void CodeGen::genStructStore(GenTreeBlk* store)
{
    assert(store->OperIs(GT_STORE_OBJ, GT_STORE_DYN_BLK, GT_STORE_BLK));

    switch (store->GetKind())
    {
#ifdef TARGET_AMD64
        case StructStoreKind::MemSet:
            genStructStoreMemSet(store);
            break;
        case StructStoreKind::MemCpy:
            genStructStoreMemCpy(store);
            break;
#endif
        case StructStoreKind::RepStos:
            genStructStoreRepStos(store);
            break;
        case StructStoreKind::RepMovs:
            genStructStoreRepMovs(store);
            break;
        case StructStoreKind::UnrollInit:
            genStructStoreUnrollInit(store);
            break;
        case StructStoreKind::UnrollCopy:
            genStructStoreUnrollCopy(store);
            break;
        case StructStoreKind::UnrollCopyWB:
        case StructStoreKind::UnrollCopyWBRepMovs:
            genStructStoreUnrollCopyWB(store->AsObj());
            break;
        default:
            unreached();
    }
}

#ifdef TARGET_AMD64

void CodeGen::genStructStoreMemSet(GenTreeBlk* store)
{
    genConsumeStructStore(store, REG_ARG_0, REG_ARG_1, REG_ARG_2);
    genEmitHelperCall(CORINFO_HELP_MEMSET, 0, EA_UNKNOWN);
}

void CodeGen::genStructStoreMemCpy(GenTreeBlk* store)
{
    assert((store->GetLayout() == nullptr) || !store->GetLayout()->HasGCPtr());

    genConsumeStructStore(store, REG_ARG_0, REG_ARG_1, REG_ARG_2);
    genEmitHelperCall(CORINFO_HELP_MEMCPY, 0, EA_UNKNOWN);
}

#endif // TARGET_AMD64

void CodeGen::genStructStoreRepStos(GenTreeBlk* store)
{
    genConsumeStructStore(store, REG_RDI, REG_RAX, REG_RCX);
    instGen(INS_r_stosb);
}

void CodeGen::genStructStoreRepMovs(GenTreeBlk* store)
{
    assert((store->GetLayout() == nullptr) || !store->GetLayout()->HasGCPtr());

    genConsumeStructStore(store, REG_RDI, REG_RSI, REG_RCX);
    instGen(INS_r_movsb);
}

void CodeGen::genStructStoreUnrollInit(GenTreeBlk* store)
{
    assert(store->OperIs(GT_STORE_BLK, GT_STORE_OBJ));

    unsigned  dstLclNum         = BAD_VAR_NUM;
    regNumber dstAddrBaseReg    = REG_NA;
    regNumber dstAddrIndexReg   = REG_NA;
    unsigned  dstAddrIndexScale = 1;
    int       dstOffset         = 0;
    GenTree*  dstAddr           = store->GetAddr();

    if (!dstAddr->isContained())
    {
        dstAddrBaseReg = genConsumeReg(dstAddr);
    }
    else if (GenTreeAddrMode* addrMode = dstAddr->IsAddrMode())
    {
        if (addrMode->HasBase())
        {
            dstAddrBaseReg = genConsumeReg(addrMode->GetBase());
        }

        if (addrMode->HasIndex())
        {
            dstAddrIndexReg   = genConsumeReg(addrMode->GetIndex());
            dstAddrIndexScale = addrMode->GetScale();
        }

        dstOffset = addrMode->GetOffset();
    }
    else
    {
        assert(dstAddr->OperIsLocalAddr());

        dstLclNum = dstAddr->AsLclVarCommon()->GetLclNum();
        dstOffset = dstAddr->AsLclVarCommon()->GetLclOffs();
    }

    regNumber srcIntReg = REG_NA;
    GenTree*  src       = store->GetValue();

    if (src->OperIs(GT_INIT_VAL))
    {
        assert(src->isContained());
        src = src->AsUnOp()->GetOp(0);
    }

    if (!src->isContained())
    {
        srcIntReg = genConsumeReg(src);
    }
    else
    {
        // If src is contained then it must be 0 and the size must be a multiple
        // of XMM_REGSIZE_BYTES so initialization can use only SSE2 instructions.
        assert(src->IsIntegralConst(0));
        assert((store->GetLayout()->GetSize() % XMM_REGSIZE_BYTES) == 0);
    }

    emitter* emit = GetEmitter();
    unsigned size = store->GetLayout()->GetSize();

    assert(size <= INT32_MAX);
    assert(dstOffset < (INT32_MAX - static_cast<int>(size)));

    // Fill as much as possible using SSE2 stores.
    if (size >= XMM_REGSIZE_BYTES)
    {
        regNumber srcXmmReg = store->GetSingleTempReg(RBM_ALLFLOAT);

        if (src->gtSkipReloadOrCopy()->IsIntegralConst(0))
        {
            // If the source is constant 0 then always use xorps, it's faster
            // than copying the constant from a GPR to a XMM register.
            emit->emitIns_R_R(INS_xorps, EA_16BYTE, srcXmmReg, srcXmmReg);
        }
        else
        {
            emit->emitIns_Mov(INS_movd, EA_PTRSIZE, srcXmmReg, srcIntReg, /* canSkip */ false);
            emit->emitIns_R_R(INS_punpckldq, EA_16BYTE, srcXmmReg, srcXmmReg);
#ifdef TARGET_X86
            // For x86, we need one more to convert it from 8 bytes to 16 bytes.
            emit->emitIns_R_R(INS_punpckldq, EA_16BYTE, srcXmmReg, srcXmmReg);
#endif
        }

        instruction simdMov = simdUnalignedMovIns();
        for (unsigned regSize = XMM_REGSIZE_BYTES; size >= regSize; size -= regSize, dstOffset += regSize)
        {
            if (dstLclNum != BAD_VAR_NUM)
            {
                emit->emitIns_S_R(simdMov, EA_ATTR(regSize), srcXmmReg, dstLclNum, dstOffset);
            }
            else
            {
                emit->emitIns_ARX_R(simdMov, EA_ATTR(regSize), srcXmmReg, dstAddrBaseReg, dstAddrIndexReg,
                                    dstAddrIndexScale, dstOffset);
            }
        }

        // TODO-CQ-XArch: On x86 we could initialize 8 byte at once by using MOVQ instead of two 4 byte MOV stores.
        // On x64 it may also be worth zero initializing a 4/8 byte remainder using MOVD/MOVQ, that avoids the need
        // to allocate a GPR just for the remainder.
    }

    // Fill the remainder using normal stores.
    for (unsigned regSize = REGSIZE_BYTES; size > 0; size -= regSize, dstOffset += regSize)
    {
        while (regSize > size)
        {
            regSize /= 2;
        }

        if (dstLclNum != BAD_VAR_NUM)
        {
            emit->emitIns_S_R(INS_mov, EA_ATTR(regSize), srcIntReg, dstLclNum, dstOffset);
        }
        else
        {
            emit->emitIns_ARX_R(INS_mov, EA_ATTR(regSize), srcIntReg, dstAddrBaseReg, dstAddrIndexReg,
                                dstAddrIndexScale, dstOffset);
        }
    }
}

void CodeGen::genStructStoreUnrollCopy(GenTreeBlk* store)
{
    assert(store->OperIs(GT_STORE_BLK, GT_STORE_OBJ));

    if (store->GetLayout()->HasGCPtr())
    {
#ifndef JIT32_GCENCODER
        GetEmitter()->emitDisableGC();
#else
        unreached();
#endif
    }

    unsigned  dstLclNum         = BAD_VAR_NUM;
    regNumber dstAddrBaseReg    = REG_NA;
    regNumber dstAddrIndexReg   = REG_NA;
    unsigned  dstAddrIndexScale = 1;
    int       dstOffset         = 0;
    GenTree*  dstAddr           = store->GetAddr();

    if (!dstAddr->isContained())
    {
        dstAddrBaseReg = genConsumeReg(dstAddr);
    }
    else if (GenTreeAddrMode* addrMode = dstAddr->IsAddrMode())
    {
        if (addrMode->HasBase())
        {
            dstAddrBaseReg = genConsumeReg(addrMode->GetBase());
        }

        if (addrMode->HasIndex())
        {
            dstAddrIndexReg   = genConsumeReg(addrMode->GetIndex());
            dstAddrIndexScale = addrMode->GetScale();
        }

        dstOffset = addrMode->GetOffset();
    }
    else
    {
        assert(dstAddr->OperIsLocalAddr());

        dstLclNum = dstAddr->AsLclVarCommon()->GetLclNum();
        dstOffset = dstAddr->AsLclVarCommon()->GetLclOffs();
    }

    unsigned  srcLclNum         = BAD_VAR_NUM;
    regNumber srcAddrBaseReg    = REG_NA;
    regNumber srcAddrIndexReg   = REG_NA;
    unsigned  srcAddrIndexScale = 1;
    int       srcOffset         = 0;
    GenTree*  src               = store->GetValue();

    assert(src->isContained());

    if (src->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        srcLclNum = src->AsLclVarCommon()->GetLclNum();
        srcOffset = src->AsLclVarCommon()->GetLclOffs();
    }
    else
    {
        assert(src->OperIs(GT_IND, GT_OBJ, GT_BLK));

        GenTree* srcAddr = src->AsIndir()->GetAddr();

        if (!srcAddr->isContained())
        {
            srcAddrBaseReg = genConsumeReg(srcAddr);
        }
        else if (GenTreeAddrMode* addrMode = srcAddr->IsAddrMode())
        {
            if (addrMode->HasBase())
            {
                srcAddrBaseReg = genConsumeReg(addrMode->GetBase());
            }

            if (addrMode->HasIndex())
            {
                srcAddrIndexReg   = genConsumeReg(addrMode->GetIndex());
                srcAddrIndexScale = addrMode->GetScale();
            }

            srcOffset = addrMode->GetOffset();
        }
        else
        {
            assert(srcAddr->OperIsLocalAddr());

            srcLclNum = srcAddr->AsLclVarCommon()->GetLclNum();
            srcOffset = srcAddr->AsLclVarCommon()->GetLclOffs();
        }
    }

    emitter* emit = GetEmitter();
    unsigned size = store->GetLayout()->GetSize();

    assert(size <= INT32_MAX);
    assert(srcOffset < (INT32_MAX - static_cast<int>(size)));
    assert(dstOffset < (INT32_MAX - static_cast<int>(size)));

    if (size >= XMM_REGSIZE_BYTES)
    {
        regNumber tempReg = store->GetSingleTempReg(RBM_ALLFLOAT);

        instruction simdMov = simdUnalignedMovIns();
        for (unsigned regSize = XMM_REGSIZE_BYTES; size >= regSize;
             size -= regSize, srcOffset += regSize, dstOffset += regSize)
        {
            if (srcLclNum != BAD_VAR_NUM)
            {
                emit->emitIns_R_S(simdMov, EA_ATTR(regSize), tempReg, srcLclNum, srcOffset);
            }
            else
            {
                emit->emitIns_R_ARX(simdMov, EA_ATTR(regSize), tempReg, srcAddrBaseReg, srcAddrIndexReg,
                                    srcAddrIndexScale, srcOffset);
            }

            if (dstLclNum != BAD_VAR_NUM)
            {
                emit->emitIns_S_R(simdMov, EA_ATTR(regSize), tempReg, dstLclNum, dstOffset);
            }
            else
            {
                emit->emitIns_ARX_R(simdMov, EA_ATTR(regSize), tempReg, dstAddrBaseReg, dstAddrIndexReg,
                                    dstAddrIndexScale, dstOffset);
            }
        }

        // TODO-CQ-XArch: On x86 we could copy 8 byte at once by using MOVQ instead of four 4 byte MOV stores.
        // On x64 it may also be worth copying a 4/8 byte remainder using MOVD/MOVQ, that avoids the need to
        // allocate a GPR just for the remainder.
    }

    if (size > 0)
    {
        regNumber tempReg = store->GetSingleTempReg(RBM_ALLINT);

        for (unsigned regSize = REGSIZE_BYTES; size > 0; size -= regSize, srcOffset += regSize, dstOffset += regSize)
        {
            while (regSize > size)
            {
                regSize /= 2;
            }

            if (srcLclNum != BAD_VAR_NUM)
            {
                emit->emitIns_R_S(INS_mov, EA_ATTR(regSize), tempReg, srcLclNum, srcOffset);
            }
            else
            {
                emit->emitIns_R_ARX(INS_mov, EA_ATTR(regSize), tempReg, srcAddrBaseReg, srcAddrIndexReg,
                                    srcAddrIndexScale, srcOffset);
            }

            if (dstLclNum != BAD_VAR_NUM)
            {
                emit->emitIns_S_R(INS_mov, EA_ATTR(regSize), tempReg, dstLclNum, dstOffset);
            }
            else
            {
                emit->emitIns_ARX_R(INS_mov, EA_ATTR(regSize), tempReg, dstAddrBaseReg, dstAddrIndexReg,
                                    dstAddrIndexScale, dstOffset);
            }
        }
    }

    if (store->GetLayout()->HasGCPtr())
    {
#ifndef JIT32_GCENCODER
        GetEmitter()->emitEnableGC();
#else
        unreached();
#endif
    }
}

// Generate code for a struct store that contains GC pointers.
// This will generate a sequence of (REP) MOVS instructions for
// non-GC slots and calls to the BY_REF_ASSIGN helper otherwise.
//
void CodeGen::genStructStoreUnrollCopyWB(GenTreeObj* store)
{
    assert(store->GetLayout()->HasGCPtr());

    genConsumeStructStore(store, REG_RDI, REG_RSI, REG_NA);

    GenTree*  dstAddr     = store->GetAddr();
    bool      dstOnStack  = dstAddr->gtSkipReloadOrCopy()->OperIsLocalAddr();
    var_types dstAddrType = dstAddr->GetType();

    GenTree*  src = store->GetValue();
    var_types srcAddrType;

    if (src->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        srcAddrType = TYP_I_IMPL;
    }
    else
    {
        assert(src->OperIs(GT_IND, GT_OBJ, GT_BLK));

        srcAddrType = src->AsIndir()->GetAddr()->GetType();
    }

    gcInfo.gcMarkRegPtrVal(REG_RSI, srcAddrType);
    gcInfo.gcMarkRegPtrVal(REG_RDI, dstAddrType);

    ClassLayout* layout    = store->GetLayout();
    unsigned     slotCount = layout->GetSlotCount();

    if (dstOnStack)
    {
        // Stack stores do not require write barriers.

        if (slotCount < CPOBJ_NONGC_SLOTS_LIMIT)
        {
            for (unsigned i = 0; i < slotCount; i++)
            {
                instGen(INS_movsp);
            }
        }
        else
        {
            assert(store->HasTempReg(REG_RCX));

            GetEmitter()->emitIns_R_I(INS_mov, EA_4BYTE, REG_RCX, slotCount);
            instGen(INS_r_movsp);
        }
    }
    else
    {
        for (unsigned i = 0; i < slotCount; i++)
        {
            if (layout->IsGCPtr(i))
            {
                genEmitHelperCall(CORINFO_HELP_ASSIGN_BYREF, 0, EA_PTRSIZE);
            }
            else
            {
                unsigned nonWBSequenceLength = 1;

                while ((i + 1 < slotCount) && !layout->IsGCPtr(i + 1))
                {
                    nonWBSequenceLength++;
                    i++;
                }

                if (nonWBSequenceLength < CPOBJ_NONGC_SLOTS_LIMIT)
                {
                    for (unsigned j = 0; j < nonWBSequenceLength; j++)
                    {
                        instGen(INS_movsp);
                    }
                }
                else
                {
                    assert(store->HasTempReg(REG_RCX));

                    GetEmitter()->emitIns_R_I(INS_mov, EA_4BYTE, REG_RCX, nonWBSequenceLength);
                    instGen(INS_r_movsp);
                }
            }
        }
    }

    // Clear the gcInfo for RSI and RDI.
    // While we normally update GC info prior to the last instruction that uses them,
    // these actually live into the helper call.
    gcInfo.gcMarkRegSetNpt(RBM_RSI);
    gcInfo.gcMarkRegSetNpt(RBM_RDI);
}

//------------------------------------------------------------------------
// If any Vector3 args are on stack and they are not pass-by-ref, the upper 32bits
// must be cleared to zeroes. The native compiler doesn't clear the upper bits
// and there is no way to know if the caller is native or not. So, the upper
// 32 bits of Vector argument on stack are always cleared to zero.
#if defined(UNIX_AMD64_ABI) && defined(FEATURE_SIMD)
void CodeGen::genClearStackVec3ArgUpperBits()
{
#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In genClearStackVec3ArgUpperBits()\n");
    }
#endif

    assert(compiler->compGeneratingProlog);

    unsigned varNum = 0;

    for (unsigned varNum = 0; varNum < compiler->info.compArgsCount; varNum++)
    {
        LclVarDsc* varDsc = &(compiler->lvaTable[varNum]);
        assert(varDsc->lvIsParam);

        // Does var has simd12 type?
        if (varDsc->lvType != TYP_SIMD12)
        {
            continue;
        }

        if (!varDsc->lvIsRegArg)
        {
            // Clear the upper 32 bits by mov dword ptr [V_ARG_BASE+0xC], 0
            GetEmitter()->emitIns_S_I(ins_Store(TYP_INT), EA_4BYTE, varNum, genTypeSize(TYP_FLOAT) * 3, 0);
        }
        else
        {
            // Assume that for x64 linux, an argument is fully in registers
            // or fully on stack.
            regNumber argReg = varDsc->GetOtherArgReg();

            // Clear the upper 32 bits by two shift instructions.
            // argReg = argReg << 96
            GetEmitter()->emitIns_R_I(INS_pslldq, emitActualTypeSize(TYP_SIMD12), argReg, 12);
            // argReg = argReg >> 96
            GetEmitter()->emitIns_R_I(INS_psrldq, emitActualTypeSize(TYP_SIMD12), argReg, 12);
        }
    }
}
#endif // defined(UNIX_AMD64_ABI) && defined(FEATURE_SIMD)

// generate code do a switch statement based on a table of ip-relative offsets
void CodeGen::genTableBasedSwitch(GenTree* treeNode)
{
    genConsumeOperands(treeNode->AsOp());
    regNumber idxReg  = treeNode->AsOp()->gtOp1->GetRegNum();
    regNumber baseReg = treeNode->AsOp()->gtOp2->GetRegNum();

    regNumber tmpReg = treeNode->GetSingleTempReg();

    // load the ip-relative offset (which is relative to start of fgFirstBB)
    GetEmitter()->emitIns_R_ARX(INS_mov, EA_4BYTE, baseReg, baseReg, idxReg, 4, 0);

    // add it to the absolute address of fgFirstBB
    GetEmitter()->emitIns_R_L(INS_lea, EA_PTR_DSP_RELOC, compiler->fgFirstBB, tmpReg);
    GetEmitter()->emitIns_R_R(INS_add, EA_PTRSIZE, baseReg, tmpReg);
    // jmp baseReg
    GetEmitter()->emitIns_R(INS_i_jmp, emitTypeSize(TYP_I_IMPL), baseReg);
}

// emits the table and an instruction to get the address of the first element
void CodeGen::genJumpTable(GenTree* treeNode)
{
    noway_assert(compiler->compCurBB->bbJumpKind == BBJ_SWITCH);
    assert(treeNode->OperGet() == GT_JMPTABLE);

    unsigned     jumpCount = compiler->compCurBB->bbJumpSwt->bbsCount;
    BasicBlock** jumpTable = compiler->compCurBB->bbJumpSwt->bbsDstTab;
    unsigned     jmpTabOffs;
    unsigned     jmpTabBase;

    jmpTabBase = GetEmitter()->emitBBTableDataGenBeg(jumpCount, true);

    jmpTabOffs = 0;

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
    GetEmitter()->emitIns_R_C(INS_lea, emitTypeSize(TYP_I_IMPL), treeNode->GetRegNum(),
                              compiler->eeFindJitDataOffs(jmpTabBase), 0);
    genProduceReg(treeNode);
}

//------------------------------------------------------------------------
// genCodeForLockAdd: Generate code for a GT_LOCKADD node
//
// Arguments:
//    node - the GT_LOCKADD node
//
void CodeGen::genCodeForLockAdd(GenTreeOp* node)
{
    assert(node->OperIs(GT_LOCKADD));

    GenTree* addr = node->gtGetOp1();
    GenTree* data = node->gtGetOp2();
    emitAttr size = emitActualTypeSize(data->TypeGet());

    assert(addr->isUsedFromReg());
    assert(data->isUsedFromReg() || data->isContainedIntOrIImmed());
    assert((size == EA_4BYTE) || (size == EA_PTRSIZE));

    genConsumeOperands(node);
    instGen(INS_lock);

    if (data->isContainedIntOrIImmed())
    {
        int imm = static_cast<int>(data->AsIntCon()->IconValue());
        assert(imm == data->AsIntCon()->IconValue());
        GetEmitter()->emitIns_I_AR(INS_add, size, imm, addr->GetRegNum(), 0);
    }
    else
    {
        GetEmitter()->emitIns_AR_R(INS_add, size, data->GetRegNum(), addr->GetRegNum(), 0);
    }
}

//------------------------------------------------------------------------
// genLockedInstructions: Generate code for a GT_XADD or GT_XCHG node.
//
// Arguments:
//    node - the GT_XADD/XCHG node
//
void CodeGen::genLockedInstructions(GenTreeOp* node)
{
    assert(node->OperIs(GT_XADD, GT_XCHG));

    GenTree* addr = node->gtGetOp1();
    GenTree* data = node->gtGetOp2();
    emitAttr size = emitTypeSize(node->TypeGet());

    assert(addr->isUsedFromReg());
    assert(data->isUsedFromReg());
    assert((size == EA_4BYTE) || (size == EA_PTRSIZE));

    genConsumeOperands(node);

    // If the destination register is different from the data register then we need
    // to first move the data to the target register. Make sure we don't overwrite
    // the address, the register allocator should have taken care of this.
    assert((node->GetRegNum() != addr->GetRegNum()) || (node->GetRegNum() == data->GetRegNum()));
    GetEmitter()->emitIns_Mov(INS_mov, size, node->GetRegNum(), data->GetRegNum(), /* canSkip */ true);

    instruction ins = node->OperIs(GT_XADD) ? INS_xadd : INS_xchg;

    // XCHG has an implied lock prefix when the first operand is a memory operand.
    if (ins != INS_xchg)
    {
        instGen(INS_lock);
    }

    GetEmitter()->emitIns_AR_R(ins, size, node->GetRegNum(), addr->GetRegNum(), 0);
    genProduceReg(node);
}

//------------------------------------------------------------------------
// genCodeForCmpXchg: Produce code for a GT_CMPXCHG node.
//
// Arguments:
//    tree - the GT_CMPXCHG node
//
void CodeGen::genCodeForCmpXchg(GenTreeCmpXchg* tree)
{
    assert(tree->OperIs(GT_CMPXCHG));

    var_types targetType = tree->TypeGet();
    regNumber targetReg  = tree->GetRegNum();

    GenTree* location  = tree->gtOpLocation;  // arg1
    GenTree* value     = tree->gtOpValue;     // arg2
    GenTree* comparand = tree->gtOpComparand; // arg3

    assert(location->GetRegNum() != REG_NA && location->GetRegNum() != REG_RAX);
    assert(value->GetRegNum() != REG_NA && value->GetRegNum() != REG_RAX);

    genConsumeReg(location);
    genConsumeReg(value);
    genConsumeReg(comparand);

    // comparand goes to RAX;
    // Note that we must issue this move after the genConsumeRegs(), in case any of the above
    // have a GT_COPY from RAX.
    inst_Mov(comparand->TypeGet(), REG_RAX, comparand->GetRegNum(), /* canSkip */ true);

    // location is Rm
    instGen(INS_lock);

    GetEmitter()->emitIns_AR_R(INS_cmpxchg, emitTypeSize(targetType), value->GetRegNum(), location->GetRegNum(), 0);

    // Result is in RAX
    inst_Mov(targetType, targetReg, REG_RAX, /* canSkip */ true);

    genProduceReg(tree);
}

void CodeGen::genRangeCheck(GenTreeBoundsChk* bndsChk)
{
    GenTree* arrIndex = bndsChk->GetIndex();
    GenTree* arrLen   = bndsChk->GetLength();

    GenTree*     src1;
    GenTree*     src2;
    emitJumpKind jmpKind;
    instruction  cmpKind;

    genConsumeRegs(arrIndex);
    genConsumeRegs(arrLen);

    if (arrIndex->IsIntegralConst(0) && arrLen->isUsedFromReg())
    {
        // arrIndex is 0 and arrLen is in a reg. In this case
        // we can generate
        //      test reg, reg
        // since arrLen is non-negative
        src1    = arrLen;
        src2    = arrLen;
        jmpKind = EJ_je;
        cmpKind = INS_test;
    }
    else if (arrIndex->isContainedIntOrIImmed())
    {
        // arrIndex is a contained constant.  In this case
        // we will generate one of the following
        //      cmp [mem], immed    (if arrLen is a memory op)
        //      cmp reg, immed      (if arrLen is in a reg)
        //
        // That is arrLen cannot be a contained immed.
        assert(!arrLen->isContainedIntOrIImmed());

        src1    = arrLen;
        src2    = arrIndex;
        jmpKind = EJ_jbe;
        cmpKind = INS_cmp;
    }
    else
    {
        // arrIndex could either be a contained memory op or a reg
        // In this case we will generate one of the following
        //      cmp  [mem], immed   (if arrLen is a constant)
        //      cmp  [mem], reg     (if arrLen is in a reg)
        //      cmp  reg, immed     (if arrIndex is in a reg)
        //      cmp  reg1, reg2     (if arrIndex is in reg1)
        //      cmp  reg, [mem]     (if arrLen is a memory op)
        //
        // That is only one of arrIndex or arrLen can be a memory op.
        assert(!arrIndex->isUsedFromMemory() || !arrLen->isUsedFromMemory());

        src1    = arrIndex;
        src2    = arrLen;
        jmpKind = EJ_jae;
        cmpKind = INS_cmp;
    }

    var_types bndsChkType = src2->TypeGet();
#if DEBUG
    // Bounds checks can only be 32 or 64 bit sized comparisons.
    assert(bndsChkType == TYP_INT || bndsChkType == TYP_LONG);

    // The type of the bounds check should always wide enough to compare against the index.
    assert(emitTypeSize(bndsChkType) >= emitTypeSize(src1->TypeGet()));
#endif // DEBUG

    GetEmitter()->emitInsBinary(cmpKind, emitTypeSize(bndsChkType), src1, src2);
    genJumpToThrowHlpBlk(jmpKind, bndsChk->GetThrowKind(), bndsChk->GetThrowBlock());
}

//---------------------------------------------------------------------
// genCodeForPhysReg - generate code for a GT_PHYSREG node
//
// Arguments
//    tree - the GT_PHYSREG node
//
// Return value:
//    None
//
void CodeGen::genCodeForPhysReg(GenTreePhysReg* tree)
{
    assert(tree->OperIs(GT_PHYSREG));

    var_types targetType = tree->TypeGet();
    regNumber targetReg  = tree->GetRegNum();

    inst_Mov(targetType, targetReg, tree->gtSrcReg, /* canSkip */ true);
    genTransferRegGCState(targetReg, tree->gtSrcReg);

    genProduceReg(tree);
}

//---------------------------------------------------------------------
// genCodeForNullCheck - generate code for a GT_NULLCHECK node
//
// Arguments
//    tree - the GT_NULLCHECK node
//
// Return value:
//    None
//
void CodeGen::genCodeForNullCheck(GenTreeIndir* tree)
{
    assert(tree->OperIs(GT_NULLCHECK));

    assert(tree->gtOp1->isUsedFromReg());
    regNumber reg = genConsumeReg(tree->gtOp1);
    GetEmitter()->emitIns_AR_R(INS_cmp, EA_4BYTE, reg, reg, 0);
}

//------------------------------------------------------------------------
// genOffsetOfMDArrayLowerBound: Returns the offset from the Array object to the
//   lower bound for the given dimension.
//
// Arguments:
//    elemType  - the element type of the array
//    rank      - the rank of the array
//    dimension - the dimension for which the lower bound offset will be returned.
//
// Return Value:
//    The offset.

unsigned CodeGen::genOffsetOfMDArrayLowerBound(var_types elemType, unsigned rank, unsigned dimension)
{
    // Note that the lower bound and length fields of the Array object are always TYP_INT, even on 64-bit targets.
    return compiler->eeGetArrayDataOffset(elemType) + genTypeSize(TYP_INT) * (dimension + rank);
}

//------------------------------------------------------------------------
// genOffsetOfMDArrayLength: Returns the offset from the Array object to the
//   size for the given dimension.
//
// Arguments:
//    elemType  - the element type of the array
//    rank      - the rank of the array
//    dimension - the dimension for which the lower bound offset will be returned.
//
// Return Value:
//    The offset.

unsigned CodeGen::genOffsetOfMDArrayDimensionSize(var_types elemType, unsigned rank, unsigned dimension)
{
    // Note that the lower bound and length fields of the Array object are always TYP_INT, even on 64-bit targets.
    return compiler->eeGetArrayDataOffset(elemType) + genTypeSize(TYP_INT) * dimension;
}

//------------------------------------------------------------------------
// genCodeForArrIndex: Generates code to bounds check the index for one dimension of an array reference,
//                     producing the effective index by subtracting the lower bound.
//
// Arguments:
//    arrIndex - the node for which we're generating code
//
// Return Value:
//    None.
//

void CodeGen::genCodeForArrIndex(GenTreeArrIndex* arrIndex)
{
    GenTree* arrObj    = arrIndex->ArrObj();
    GenTree* indexNode = arrIndex->IndexExpr();

    regNumber arrReg   = genConsumeReg(arrObj);
    regNumber indexReg = genConsumeReg(indexNode);
    regNumber tgtReg   = arrIndex->GetRegNum();

    unsigned  dim      = arrIndex->gtCurrDim;
    unsigned  rank     = arrIndex->gtArrRank;
    var_types elemType = arrIndex->gtArrElemType;

    noway_assert(tgtReg != REG_NA);

    // Subtract the lower bound for this dimension.
    // TODO-XArch-CQ: make this contained if it's an immediate that fits.
    inst_Mov(indexNode->TypeGet(), tgtReg, indexReg, /* canSkip */ true);
    GetEmitter()->emitIns_R_AR(INS_sub, emitActualTypeSize(TYP_INT), tgtReg, arrReg,
                               genOffsetOfMDArrayLowerBound(elemType, rank, dim));
    GetEmitter()->emitIns_R_AR(INS_cmp, emitActualTypeSize(TYP_INT), tgtReg, arrReg,
                               genOffsetOfMDArrayDimensionSize(elemType, rank, dim));
    genJumpToThrowHlpBlk(EJ_jae, SCK_RNGCHK_FAIL);

    genProduceReg(arrIndex);
}

//------------------------------------------------------------------------
// genCodeForArrOffset: Generates code to compute the flattened array offset for
//    one dimension of an array reference:
//        result = (prevDimOffset * dimSize) + effectiveIndex
//    where dimSize is obtained from the arrObj operand
//
// Arguments:
//    arrOffset - the node for which we're generating code
//
// Return Value:
//    None.
//
// Notes:
//    dimSize and effectiveIndex are always non-negative, the former by design,
//    and the latter because it has been normalized to be zero-based.

void CodeGen::genCodeForArrOffset(GenTreeArrOffs* arrOffset)
{
    GenTree* offsetNode = arrOffset->gtOffset;
    GenTree* indexNode  = arrOffset->gtIndex;
    GenTree* arrObj     = arrOffset->gtArrObj;

    regNumber tgtReg = arrOffset->GetRegNum();
    assert(tgtReg != REG_NA);

    unsigned  dim      = arrOffset->gtCurrDim;
    unsigned  rank     = arrOffset->gtArrRank;
    var_types elemType = arrOffset->gtArrElemType;

    // First, consume the operands in the correct order.
    regNumber offsetReg = REG_NA;
    regNumber tmpReg    = REG_NA;
    if (!offsetNode->IsIntegralConst(0))
    {
        offsetReg = genConsumeReg(offsetNode);

        // We will use a temp register for the offset*scale+effectiveIndex computation.
        tmpReg = arrOffset->GetSingleTempReg();
    }
    else
    {
        assert(offsetNode->isContained());
    }
    regNumber indexReg = genConsumeReg(indexNode);
    // Although arrReg may not be used in the constant-index case, if we have generated
    // the value into a register, we must consume it, otherwise we will fail to end the
    // live range of the gc ptr.
    // TODO-CQ: Currently arrObj will always have a register allocated to it.
    // We could avoid allocating a register for it, which would be of value if the arrObj
    // is an on-stack lclVar.
    regNumber arrReg = REG_NA;
    if (arrObj->gtHasReg())
    {
        arrReg = genConsumeReg(arrObj);
    }

    if (!offsetNode->IsIntegralConst(0))
    {
        assert(tmpReg != REG_NA);
        assert(arrReg != REG_NA);

        // Evaluate tgtReg = offsetReg*dim_size + indexReg.
        // tmpReg is used to load dim_size and the result of the multiplication.
        // Note that dim_size will never be negative.

        GetEmitter()->emitIns_R_AR(INS_mov, emitActualTypeSize(TYP_INT), tmpReg, arrReg,
                                   genOffsetOfMDArrayDimensionSize(elemType, rank, dim));
        inst_RV_RV(INS_imul, tmpReg, offsetReg, TYP_I_IMPL);

        if (tmpReg == tgtReg)
        {
            inst_RV_RV(INS_add, tmpReg, indexReg, TYP_I_IMPL);
        }
        else
        {
            inst_Mov(TYP_I_IMPL, tgtReg, indexReg, /* canSkip */ true);
            inst_RV_RV(INS_add, tgtReg, tmpReg, TYP_I_IMPL);
        }
    }
    else
    {
        inst_Mov(TYP_INT, tgtReg, indexReg, /* canSkip */ true);
    }
    genProduceReg(arrOffset);
}

instruction CodeGen::ins_FloatCompare(var_types type)
{
    return (type == TYP_FLOAT) ? INS_ucomiss : INS_ucomisd;
}

instruction CodeGen::ins_FloatSqrt(var_types type)
{
    return (type == TYP_FLOAT) ? INS_sqrtss : INS_sqrtsd;
}

instruction CodeGen::ins_MathOp(genTreeOps oper, var_types type)
{
    switch (oper)
    {
        case GT_ADD:
            return type == TYP_DOUBLE ? INS_addsd : INS_addss;
        case GT_SUB:
            return type == TYP_DOUBLE ? INS_subsd : INS_subss;
        case GT_MUL:
            return type == TYP_DOUBLE ? INS_mulsd : INS_mulss;
        case GT_DIV:
            return type == TYP_DOUBLE ? INS_divsd : INS_divss;
        default:
            unreached();
    }
}

instruction CodeGen::genGetInsForOper(genTreeOps oper, var_types type)
{
    // Operations on SIMD vectors shouldn't come this path
    assert(!varTypeIsSIMD(type));

    if (varTypeIsFloating(type))
    {
        return ins_MathOp(oper, type);
    }

    switch (oper)
    {
        case GT_ADD:
            return INS_add;
        case GT_AND:
            return INS_and;
        case GT_LSH:
            return INS_shl;
        case GT_MUL:
            return INS_imul;
        case GT_NEG:
            return INS_neg;
        case GT_NOT:
            return INS_not;
        case GT_OR:
            return INS_or;
        case GT_ROL:
            return INS_rol;
        case GT_ROR:
            return INS_ror;
        case GT_RSH:
            return INS_sar;
        case GT_RSZ:
            return INS_shr;
        case GT_SUB:
            return INS_sub;
        case GT_XOR:
            return INS_xor;
#if !defined(TARGET_64BIT)
        case GT_ADD_LO:
            return INS_add;
        case GT_ADD_HI:
            return INS_adc;
        case GT_SUB_LO:
            return INS_sub;
        case GT_SUB_HI:
            return INS_sbb;
        case GT_LSH_HI:
            return INS_shld;
        case GT_RSH_LO:
            return INS_shrd;
#endif
        default:
            unreached();
    }
}

//------------------------------------------------------------------------
// genCodeForShift: Generates the code sequence for a GenTree node that
// represents a bit shift or rotate operation (<<, >>, >>>, rol, ror).
//
// Arguments:
//    tree - the bit shift node (that specifies the type of bit shift to perform).
//
// Assumptions:
//    a) All GenTrees are register allocated.
//    b) The shift-by-amount in tree->AsOp()->gtOp2 is either a contained constant or
//       it's a register-allocated expression. If it is in a register that is
//       not RCX, it will be moved to RCX (so RCX better not be in use!).
//
void CodeGen::genCodeForShift(GenTree* tree)
{
    // Only the non-RMW case here.
    assert(tree->OperIsShiftOrRotate());
    assert(tree->AsOp()->gtOp1->isUsedFromReg());
    assert(tree->GetRegNum() != REG_NA);

    genConsumeOperands(tree->AsOp());

    var_types   targetType = tree->TypeGet();
    instruction ins        = genGetInsForOper(tree->OperGet(), targetType);

    GenTree*  operand    = tree->gtGetOp1();
    regNumber operandReg = operand->GetRegNum();

    GenTree* shiftBy = tree->gtGetOp2();

    if (shiftBy->isContainedIntOrIImmed())
    {
        emitAttr size = emitTypeSize(tree);

        // Optimize "X<<1" to "lea [reg+reg]" or "add reg, reg"
        if (tree->OperIs(GT_LSH) && !tree->gtOverflowEx() && !tree->gtSetFlags() && shiftBy->IsIntegralConst(1))
        {
            if (tree->GetRegNum() == operandReg)
            {
                GetEmitter()->emitIns_R_R(INS_add, size, tree->GetRegNum(), operandReg);
            }
            else
            {
                GetEmitter()->emitIns_R_ARX(INS_lea, size, tree->GetRegNum(), operandReg, operandReg, 1, 0);
            }
        }
        else
        {
            int shiftByValue = (int)shiftBy->AsIntConCommon()->IconValue();

#if defined(TARGET_64BIT)
            // Try to emit rorx if BMI2 is available instead of mov+rol
            // it makes sense only for 64bit integers
            if ((genActualType(targetType) == TYP_LONG) && (tree->GetRegNum() != operandReg) &&
                compiler->compOpportunisticallyDependsOn(InstructionSet_BMI2) && tree->OperIs(GT_ROL, GT_ROR) &&
                (shiftByValue > 0) && (shiftByValue < 64))
            {
                const int value = tree->OperIs(GT_ROL) ? (64 - shiftByValue) : shiftByValue;
                GetEmitter()->emitIns_R_R_I(INS_rorx, size, tree->GetRegNum(), operandReg, value);
                genProduceReg(tree);
                return;
            }
#endif
            // First, move the operand to the destination register and
            // later on perform the shift in-place.
            // (LSRA will try to avoid this situation through preferencing.)
            inst_Mov(targetType, tree->GetRegNum(), operandReg, /* canSkip */ true);
            inst_RV_SH(ins, size, tree->GetRegNum(), shiftByValue);
        }
    }
    else
    {
        // We must have the number of bits to shift stored in ECX, since we constrained this node to
        // sit in ECX. In case this didn't happen, LSRA expects the code generator to move it since it's a single
        // register destination requirement.
        genCopyRegIfNeeded(shiftBy, REG_RCX);

        // The operand to be shifted must not be in ECX
        noway_assert(operandReg != REG_RCX);

        inst_Mov(targetType, tree->GetRegNum(), operandReg, /* canSkip */ true);
        inst_RV(ins, tree->GetRegNum(), targetType);
    }

    genProduceReg(tree);
}

#ifdef TARGET_X86
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
// TODO-X86-CQ: This only handles the case where the operand being shifted is in a register. We don't
// need sourceHi to be always in reg in case of GT_LSH_HI (because it could be moved from memory to
// targetReg if sourceHi is a memory operand). Similarly for GT_RSH_LO, sourceLo could be marked as
// contained memory-op. Even if not a memory-op, we could mark it as reg-optional.
//
void CodeGen::genCodeForShiftLong(GenTree* tree)
{
    // Only the non-RMW case here.
    genTreeOps oper = tree->OperGet();
    assert(oper == GT_LSH_HI || oper == GT_RSH_LO);

    GenTree* operand = tree->AsOp()->gtOp1;
    assert(operand->OperGet() == GT_LONG);
    assert(operand->AsOp()->gtOp1->isUsedFromReg());
    assert(operand->AsOp()->gtOp2->isUsedFromReg());

    GenTree* operandLo = operand->gtGetOp1();
    GenTree* operandHi = operand->gtGetOp2();

    regNumber regLo = operandLo->GetRegNum();
    regNumber regHi = operandHi->GetRegNum();

    genConsumeOperands(tree->AsOp());

    var_types   targetType = tree->TypeGet();
    instruction ins        = genGetInsForOper(oper, targetType);

    GenTree* shiftBy = tree->gtGetOp2();

    assert(shiftBy->isContainedIntOrIImmed());

    unsigned int count = (unsigned int)shiftBy->AsIntConCommon()->IconValue();

    regNumber regResult = (oper == GT_LSH_HI) ? regHi : regLo;

    inst_Mov(targetType, tree->GetRegNum(), regResult, /* canSkip */ true);

    if (oper == GT_LSH_HI)
    {
        inst_RV_RV_IV(ins, emitTypeSize(targetType), tree->GetRegNum(), regLo, count);
    }
    else
    {
        assert(oper == GT_RSH_LO);
        inst_RV_RV_IV(ins, emitTypeSize(targetType), tree->GetRegNum(), regHi, count);
    }

    genProduceReg(tree);
}
#endif

//------------------------------------------------------------------------
// genCodeForShiftRMW: Generates the code sequence for a GT_STOREIND GenTree node that
// represents a RMW bit shift or rotate operation (<<, >>, >>>, rol, ror), for example:
//      GT_STOREIND( AddressTree, GT_SHL( Ind ( AddressTree ), Operand ) )
//
// Arguments:
//    storeIndNode: the GT_STOREIND node.
//
void CodeGen::genCodeForShiftRMW(GenTreeStoreInd* storeInd)
{
    GenTree* data = storeInd->Data();

    assert(data->OperIsShift() || data->OperIsRotate());

    // This function only handles the RMW case.
    assert(data->AsOp()->gtOp1->isUsedFromMemory());
    assert(data->AsOp()->gtOp1->isIndir());
    assert(Lowering::IndirsAreEquivalent(data->AsOp()->gtOp1, storeInd));
    assert(data->GetRegNum() == REG_NA);

    var_types   targetType = data->TypeGet();
    genTreeOps  oper       = data->OperGet();
    instruction ins        = genGetInsForOper(oper, targetType);
    emitAttr    attr       = EA_ATTR(genTypeSize(targetType));

    GenTree* shiftBy = data->AsOp()->gtOp2;
    if (shiftBy->isContainedIntOrIImmed())
    {
        int shiftByValue = (int)shiftBy->AsIntConCommon()->IconValue();
        ins              = genMapShiftInsToShiftByConstantIns(ins, shiftByValue);
        if (shiftByValue == 1)
        {
            // There is no source in this case, as the shift by count is embedded in the instruction opcode itself.
            GetEmitter()->emitInsRMW(ins, attr, storeInd);
        }
        else
        {
            GetEmitter()->emitInsRMW(ins, attr, storeInd, shiftBy);
        }
    }
    else
    {
        // We must have the number of bits to shift stored in ECX, since we constrained this node to
        // sit in ECX. In case this didn't happen, LSRA expects the code generator to move it since it's a single
        // register destination requirement.
        genCopyRegIfNeeded(shiftBy, REG_RCX);

        // The shiftBy operand is implicit, so call the unary version of emitInsRMW.
        GetEmitter()->emitInsRMW(ins, attr, storeInd);
    }
}

//------------------------------------------------------------------------
// genCodeForLclFld: Produce code for a GT_LCL_FLD node.
//
// Arguments:
//    tree - the GT_LCL_FLD node
//
void CodeGen::genCodeForLclFld(GenTreeLclFld* tree)
{
    assert(tree->OperIs(GT_LCL_FLD));

#ifdef FEATURE_SIMD
    if (tree->TypeIs(TYP_SIMD12))
    {
        genLoadSIMD12(tree);
        return;
    }
#endif

    var_types targetType = tree->TypeGet();
    regNumber targetReg  = tree->GetRegNum();

    noway_assert(targetReg != REG_NA);

    noway_assert(targetType != TYP_STRUCT);

    emitAttr size   = emitTypeSize(targetType);
    unsigned offs   = tree->GetLclOffs();
    unsigned varNum = tree->GetLclNum();
    assert(varNum < compiler->lvaCount);

    GetEmitter()->emitIns_R_S(ins_Load(targetType), size, targetReg, varNum, offs);

    genProduceReg(tree);
}

//------------------------------------------------------------------------
// genCodeForLclVar: Produce code for a GT_LCL_VAR node.
//
// Arguments:
//    tree - the GT_LCL_VAR node
//
void CodeGen::genCodeForLclVar(GenTreeLclVar* tree)
{
    assert(tree->OperIs(GT_LCL_VAR));

    // lcl_vars are not defs
    assert((tree->gtFlags & GTF_VAR_DEF) == 0);

    LclVarDsc* varDsc         = compiler->lvaGetDesc(tree);
    bool       isRegCandidate = varDsc->lvIsRegCandidate();

    // If this is a register candidate that has been spilled, genConsumeReg() will
    // reload it at the point of use.  Otherwise, if it's not in a register, we load it here.

    if (!isRegCandidate && !tree->IsMultiReg() && !(tree->gtFlags & GTF_SPILLED))
    {
#if defined(FEATURE_SIMD) && defined(TARGET_X86)
        if (tree->TypeIs(TYP_SIMD12))
        {
            genLoadSIMD12(tree);
            return;
        }
#endif

        var_types type = varDsc->GetRegisterType(tree);
        GetEmitter()->emitIns_R_S(ins_Load(type, compiler->lvaIsSimdTypedLocalAligned(tree->GetLclNum())),
                                  emitTypeSize(type), tree->GetRegNum(), tree->GetLclNum(), 0);
        genProduceReg(tree);
    }
}

void CodeGen::GenStoreLclFld(GenTreeLclFld* store)
{
    assert(store->OperIs(GT_STORE_LCL_FLD));
    assert(!store->TypeIs(TYP_STRUCT));

#ifdef FEATURE_SIMD
    if (store->TypeIs(TYP_SIMD12))
    {
        genStoreSIMD12(store, store->GetOp(0));
        return;
    }
#endif

    var_types type = store->GetType();
    GenTree*  src  = store->GetOp(0);

    assert(IsValidSourceType(type, src->GetType()));

    genConsumeRegs(src);
    GetEmitter()->emitInsBinary(ins_Store(type), emitTypeSize(type), store, src);
    genUpdateLife(store);
}

void CodeGen::GenStoreLclVar(GenTreeLclVar* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR));

    GenTree* src = store->GetOp(0);

    if (src->gtSkipReloadOrCopy()->IsMultiRegNode())
    {
        GenStoreLclVarMultiReg(store);
        return;
    }

    LclVarDsc* lcl        = compiler->lvaGetDesc(store);
    var_types  lclRegType = lcl->GetRegisterType(store);

#ifdef DEBUG
    {
        var_types srcRegType = src->GetType();

        if (srcRegType == TYP_STRUCT)
        {
            GenTreeLclVar* srcLclVar = src->AsLclVar();

            srcRegType = compiler->lvaGetDesc(srcLclVar)->GetRegisterType(srcLclVar);
        }

        assert(varTypeUsesFloatReg(lclRegType) == varTypeUsesFloatReg(srcRegType));
        assert(!varTypeUsesFloatReg(lclRegType) || (emitTypeSize(lclRegType) == emitTypeSize(srcRegType)));
    }
#endif

#ifndef TARGET_64BIT
    if (lclRegType == TYP_LONG)
    {
        GenStoreLclVarLong(store);
        return;
    }
#endif

#ifdef FEATURE_SIMD
    if (lclRegType == TYP_SIMD12)
    {
        genStoreSIMD12(store, src);
        return;
    }
#endif

    regNumber dstReg = store->GetRegNum();
    unsigned  lclNum = store->GetLclNum();

    if (src->OperIs(GT_BITCAST) && src->isContained())
    {
        GenTree*  bitCastSrc     = src->AsUnOp()->GetOp(0);
        var_types bitCastSrcType = bitCastSrc->GetType();
        regNumber bitCastSrcReg  = genConsumeReg(bitCastSrc);

        if (dstReg == REG_NA)
        {
            GetEmitter()->emitIns_S_R(ins_Store(bitCastSrcType, compiler->lvaIsSimdTypedLocalAligned(lclNum)),
                                      emitTypeSize(lclRegType), bitCastSrcReg, lclNum, 0);

            genUpdateLife(store);
            lcl->SetRegNum(REG_STK);
        }
        else
        {
            inst_BitCast(lclRegType, dstReg, bitCastSrcType, bitCastSrc->GetRegNum());

            genProduceReg(store);
        }

        return;
    }

    if (dstReg == REG_NA)
    {
        instruction ins  = ins_Store(lclRegType, compiler->lvaIsSimdTypedLocalAligned(lclNum));
        emitAttr    attr = emitTypeSize(lclRegType);

        inst_set_SV_var(store);

        if (src->isContained())
        {
            GetEmitter()->emitIns_S_I(ins, attr, lclNum, 0, static_cast<int>(src->AsIntCon()->GetValue()));
        }
        else
        {
            regNumber srcReg = genConsumeReg(src);

            GetEmitter()->emitIns_S_R(ins, attr, srcReg, store->GetLclNum(), 0);
        }

        genUpdateLife(store);
        lcl->SetRegNum(REG_STK);

        return;
    }

    genConsumeRegs(src);

    // Look for the case where we have a constant zero which we've marked for reuse,
    // but which isn't actually in the register we want.  In that case, it's better to create
    // zero in the target register, because an xor is smaller than a copy. Note that we could
    // potentially handle this in the register allocator, but we can't always catch it there
    // because the target may not have a register allocated for it yet.

    if (src->isUsedFromReg() && (src->GetRegNum() != dstReg) &&
        (src->IsIntegralConst(0) || src->IsDblConPositiveZero()))
    {
        src->SetRegNum(REG_NA);
        src->ResetReuseRegVal();
        src->SetContained();
    }

    if (!src->isUsedFromReg())
    {
        // Currently, we assume that the non-reg source of a GT_STORE_LCL_VAR writing to a register
        // must be a constant. However, in the future we might want to support an operand used from
        // memory. This is a bit tricky because we have to decide it can be used from memory before
        // register allocation, and this would be a case where, once that's done, we need to mark
        // that node as always requiring a register - which we always assume now anyway, but once we
        // "optimize" that we'll have to take cases like this into account.

        assert((src->GetRegNum() == REG_NA) && src->OperIsConst());

        genSetRegToConst(dstReg, lclRegType, src);
    }
    else
    {
        assert(src->GetRegNum() != REG_NA);

        inst_Mov_Extend(lclRegType, true, dstReg, src->GetRegNum(), /* canSkip */ true, emitTypeSize(lclRegType));
    }

    genProduceReg(store);
}

//------------------------------------------------------------------------
// genCodeForIndexAddr: Produce code for a GT_INDEX_ADDR node.
//
// Arguments:
//    tree - the GT_INDEX_ADDR node
//
void CodeGen::genCodeForIndexAddr(GenTreeIndexAddr* node)
{
    GenTree* const base  = node->GetArray();
    GenTree* const index = node->GetIndex();

    const regNumber baseReg  = genConsumeReg(base);
    regNumber       indexReg = genConsumeReg(index);
    const regNumber dstReg   = node->GetRegNum();

    // NOTE: `genConsumeReg` marks the consumed register as not a GC pointer, as it assumes that the input registers
    // die at the first instruction generated by the node. This is not the case for `INDEX_ADDR`, however, as the
    // base register is multiply-used. As such, we need to mark the base register as containing a GC pointer until
    // we are finished generating the code for this node.

    gcInfo.gcMarkRegPtrVal(baseReg, base->TypeGet());
    assert(varTypeIsIntegral(index->TypeGet()));

    regNumber tmpReg = REG_NA;
#ifdef TARGET_64BIT
    tmpReg = node->GetSingleTempReg();
#endif

    // Generate the bounds check if necessary.
    if ((node->gtFlags & GTF_INX_RNGCHK) != 0)
    {
#ifdef TARGET_64BIT
        // The CLI Spec allows an array to be indexed by either an int32 or a native int.  In the case that the index
        // is a native int on a 64-bit platform, we will need to widen the array length and then compare.
        if (index->TypeGet() == TYP_I_IMPL)
        {
            GetEmitter()->emitIns_R_AR(INS_mov, EA_4BYTE, tmpReg, baseReg, node->GetLenOffs());
            GetEmitter()->emitIns_R_R(INS_cmp, EA_8BYTE, indexReg, tmpReg);
        }
        else
#endif // TARGET_64BIT
        {
            GetEmitter()->emitIns_R_AR(INS_cmp, EA_4BYTE, indexReg, baseReg, node->GetLenOffs());
        }

        genJumpToThrowHlpBlk(EJ_jae, SCK_RNGCHK_FAIL, node->GetThrowBlock());
    }

#ifdef TARGET_64BIT
    if (index->TypeGet() != TYP_I_IMPL)
    {
        // LEA needs 64-bit operands so we need to widen the index if it's TYP_INT.
        GetEmitter()->emitIns_Mov(INS_mov, EA_4BYTE, tmpReg, indexReg, /* canSkip */ false);
        indexReg = tmpReg;
    }
#endif // TARGET_64BIT

    // Compute the address of the array element.
    unsigned scale = node->GetElemSize();

    switch (scale)
    {
        case 1:
        case 2:
        case 4:
        case 8:
            tmpReg = indexReg;
            break;

        default:
#ifdef TARGET_64BIT
            // IMUL treats its immediate operand as signed so scale can't be larger than INT32_MAX.
            // The VM doesn't allow such large array elements but let's be sure.
            noway_assert(scale <= INT32_MAX);
#else  // !TARGET_64BIT
            tmpReg = node->GetSingleTempReg();
#endif // !TARGET_64BIT

            GetEmitter()->emitIns_R_I(emitter::inst3opImulForReg(tmpReg), EA_PTRSIZE, indexReg,
                                      static_cast<ssize_t>(scale));
            scale = 1;
            break;
    }

    GetEmitter()->emitIns_R_ARX(INS_lea, emitTypeSize(node->TypeGet()), dstReg, baseReg, tmpReg, scale,
                                node->GetDataOffs());

    gcInfo.gcMarkRegSetNpt(base->gtGetRegMask());

    genProduceReg(node);
}

//------------------------------------------------------------------------
// genCodeForIndir: Produce code for a GT_IND node.
//
// Arguments:
//    tree - the GT_IND node
//
void CodeGen::genCodeForIndir(GenTreeIndir* tree)
{
    assert(tree->OperIs(GT_IND));

#ifdef FEATURE_SIMD
    if (tree->TypeIs(TYP_SIMD12))
    {
        genLoadSIMD12(tree);
        return;
    }
#endif

    var_types targetType = tree->TypeGet();
    emitter*  emit       = GetEmitter();

    GenTree* addr = tree->Addr();
    if (addr->IsCnsIntOrI() && addr->IsIconHandle(GTF_ICON_TLS_HDL))
    {
        noway_assert(EA_ATTR(genTypeSize(targetType)) == EA_PTRSIZE);
        emit->emitIns_R_C(ins_Load(TYP_I_IMPL), EA_PTRSIZE, tree->GetRegNum(), FLD_GLOBAL_FS,
                          (int)addr->AsIntCon()->gtIconVal);
    }
    else
    {
        genConsumeAddress(addr);
        emit->emitInsLoadInd(ins_Load(targetType), emitTypeSize(tree), tree->GetRegNum(), tree);
    }

    genProduceReg(tree);
}

//------------------------------------------------------------------------
// genCodeForStoreInd: Produce code for a GT_STOREIND node.
//
// Arguments:
//    tree - the GT_STOREIND node
//
void CodeGen::genCodeForStoreInd(GenTreeStoreInd* tree)
{
    assert(tree->OperIs(GT_STOREIND));

#ifdef FEATURE_SIMD
    if (tree->TypeIs(TYP_SIMD12))
    {
        genStoreSIMD12(tree, tree->GetValue());
        return;
    }
#endif

    GenTree*  data       = tree->Data();
    GenTree*  addr       = tree->Addr();
    var_types targetType = tree->TypeGet();

    assert(IsValidSourceType(targetType, data->GetType()));

    GCInfo::WriteBarrierForm writeBarrierForm = gcInfo.GetWriteBarrierForm(tree);
    if (writeBarrierForm != GCInfo::WBF_NoBarrier)
    {
        genConsumeOperands(tree);

        if (!genEmitOptimizedGCWriteBarrier(writeBarrierForm, addr, data))
        {
            // At this point, we should not have any interference.
            // That is, 'data' must not be in REG_ARG_0, as that is where 'addr' must go.
            noway_assert(data->GetRegNum() != REG_ARG_0);

            genCopyRegIfNeeded(addr, REG_ARG_0);
            genCopyRegIfNeeded(data, REG_ARG_1);
            genGCWriteBarrier(tree, writeBarrierForm);
        }

        return;
    }

    bool     dataIsUnary   = false;
    bool     isRMWMemoryOp = tree->IsRMWMemoryOp();
    GenTree* rmwSrc        = nullptr;

    // We must consume the operands in the proper execution order, so that liveness is
    // updated appropriately.
    genConsumeAddress(addr);

    // If tree represents a RMW memory op then its data is a non-leaf node marked as contained
    // and non-indir operand of data is the source of RMW memory op.
    if (isRMWMemoryOp)
    {
        assert(data->isContained() && !data->OperIsLeaf());

        GenTree* rmwDst = nullptr;

        dataIsUnary = (GenTree::OperIsUnary(data->OperGet()) != 0);
        if (!dataIsUnary)
        {
            if (tree->IsRMWDstOp1())
            {
                rmwDst = data->gtGetOp1();
                rmwSrc = data->gtGetOp2();
            }
            else
            {
                assert(tree->IsRMWDstOp2());
                rmwDst = data->gtGetOp2();
                rmwSrc = data->gtGetOp1();
            }

            genConsumeRegs(rmwSrc);
        }
        else
        {
            // *(p) = oper *(p): Here addr = p, rmwsrc=rmwDst = *(p) i.e. GT_IND(p)
            // For unary RMW ops, src and dst of RMW memory op is the same.  Lower
            // clears operand counts on rmwSrc and we don't need to perform a
            // genConsumeReg() on it.
            assert(tree->IsRMWDstOp1());
            rmwSrc = data->gtGetOp1();
            rmwDst = data->gtGetOp1();
            assert(rmwSrc->isUsedFromMemory());
        }

        assert(rmwSrc != nullptr);
        assert(rmwDst != nullptr);
        assert(Lowering::IndirsAreEquivalent(rmwDst, tree));
    }
    else
    {
        genConsumeRegs(data);
    }

    if (isRMWMemoryOp)
    {
        if (dataIsUnary)
        {
            // generate code for unary RMW memory ops like neg/not
            GetEmitter()->emitInsRMW(genGetInsForOper(data->OperGet(), data->TypeGet()), emitTypeSize(tree), tree);
        }
        else
        {
            if (data->OperIsShiftOrRotate())
            {
                // Generate code for shift RMW memory ops.
                // The data address needs to be op1 (it must be [addr] = [addr] <shift> <amount>, not [addr] =
                // <amount> <shift> [addr]).
                assert(tree->IsRMWDstOp1());
                assert(rmwSrc == data->gtGetOp2());
                genCodeForShiftRMW(tree);
            }
            else if (data->OperGet() == GT_ADD && (rmwSrc->IsIntegralConst(1) || rmwSrc->IsIntegralConst(-1)))
            {
                // Generate "inc/dec [mem]" instead of "add/sub [mem], 1".
                //
                // Notes:
                //  1) Global morph transforms GT_SUB(x, +/-1) into GT_ADD(x, -/+1).
                //  2) TODO-AMD64: Debugger routine NativeWalker::Decode() runs into
                //     an assert while decoding ModR/M byte of "inc dword ptr [rax]".
                //     It is not clear whether Decode() can handle all possible
                //     addr modes with inc/dec.  For this reason, inc/dec [mem]
                //     is not generated while generating debuggable code.  Update
                //     the above if condition once Decode() routine is fixed.
                assert(rmwSrc->isContainedIntOrIImmed());
                instruction ins = rmwSrc->IsIntegralConst(1) ? INS_inc : INS_dec;
                GetEmitter()->emitInsRMW(ins, emitTypeSize(tree), tree);
            }
            else
            {
                // generate code for remaining binary RMW memory ops like add/sub/and/or/xor
                GetEmitter()->emitInsRMW(genGetInsForOper(data->OperGet(), data->TypeGet()), emitTypeSize(tree), tree,
                                         rmwSrc);
            }
        }
    }
    else
    {
        GetEmitter()->emitInsStoreInd(ins_Store(data->TypeGet()), emitTypeSize(tree), tree);
    }
}

//------------------------------------------------------------------------
// genCodeForSwap: Produce code for a GT_SWAP node.
//
// Arguments:
//    tree - the GT_SWAP node
//
void CodeGen::genCodeForSwap(GenTreeOp* tree)
{
    assert(tree->OperIs(GT_SWAP));

    // Swap is only supported for lclVar operands that are enregistered
    // We do not consume or produce any registers.  Both operands remain enregistered.
    // However, the gc-ness may change.
    assert(genIsRegCandidateLclVar(tree->gtOp1) && genIsRegCandidateLclVar(tree->gtOp2));

    GenTreeLclVar* lcl1    = tree->gtOp1->AsLclVar();
    LclVarDsc*     varDsc1 = compiler->lvaGetDesc(lcl1);
    var_types      type1   = varDsc1->TypeGet();
    GenTreeLclVar* lcl2    = tree->gtOp2->AsLclVar();
    LclVarDsc*     varDsc2 = compiler->lvaGetDesc(lcl2);
    var_types      type2   = varDsc2->TypeGet();

    // We must have both int or both fp regs
    assert(!varTypeUsesFloatReg(type1) || varTypeUsesFloatReg(type2));

    // FP swap is not yet implemented (and should have NYI'd in LSRA)
    assert(!varTypeUsesFloatReg(type1));

    regNumber oldOp1Reg     = lcl1->GetRegNum();
    regMaskTP oldOp1RegMask = genRegMask(oldOp1Reg);
    regNumber oldOp2Reg     = lcl2->GetRegNum();
    regMaskTP oldOp2RegMask = genRegMask(oldOp2Reg);

    // We don't call genUpdateVarReg because we don't have a tree node with the new register.
    varDsc1->SetRegNum(oldOp2Reg);
    varDsc2->SetRegNum(oldOp1Reg);

    // Do the xchg
    emitAttr size = EA_PTRSIZE;
    if (varTypeGCtype(type1) != varTypeGCtype(type2))
    {
        // If the type specified to the emitter is a GC type, it will swap the GC-ness of the registers.
        // Otherwise it will leave them alone, which is correct if they have the same GC-ness.
        size = EA_GCREF;
    }
    inst_RV_RV(INS_xchg, oldOp1Reg, oldOp2Reg, TYP_I_IMPL, size);

    // Update the gcInfo.
    // Manually remove these regs for the gc sets (mostly to avoid confusing duplicative dump output)
    gcInfo.gcRegByrefSetCur &= ~(oldOp1RegMask | oldOp2RegMask);
    gcInfo.gcRegGCrefSetCur &= ~(oldOp1RegMask | oldOp2RegMask);

    // gcMarkRegPtrVal will do the appropriate thing for non-gc types.
    // It will also dump the updates.
    gcInfo.gcMarkRegPtrVal(oldOp2Reg, type1);
    gcInfo.gcMarkRegPtrVal(oldOp1Reg, type2);
}

//------------------------------------------------------------------------
// genEmitOptimizedGCWriteBarrier: Generate write barrier store using the optimized
// helper functions.
//
// Arguments:
//    writeBarrierForm - the write barrier form to use
//    addr - the address at which to do the store
//    data - the data to store
//
// Return Value:
//    true if an optimized write barrier form was used, false if not. If this
//    function returns false, the caller must emit a "standard" write barrier.

bool CodeGen::genEmitOptimizedGCWriteBarrier(GCInfo::WriteBarrierForm writeBarrierForm, GenTree* addr, GenTree* data)
{
    assert(writeBarrierForm != GCInfo::WBF_NoBarrier);

#if defined(TARGET_X86) && NOGC_WRITE_BARRIERS
    if (!genUseOptimizedWriteBarriers())
    {
        return false;
    }

    const static int regToHelper[2][8] = {
        // If the target is known to be in managed memory
        {
            CORINFO_HELP_ASSIGN_REF_EAX, // EAX
            CORINFO_HELP_ASSIGN_REF_ECX, // ECX
            -1,                          // EDX (always the target address)
            CORINFO_HELP_ASSIGN_REF_EBX, // EBX
            -1,                          // ESP
            CORINFO_HELP_ASSIGN_REF_EBP, // EBP
            CORINFO_HELP_ASSIGN_REF_ESI, // ESI
            CORINFO_HELP_ASSIGN_REF_EDI, // EDI
        },

        // Don't know if the target is in managed memory
        {
            CORINFO_HELP_CHECKED_ASSIGN_REF_EAX, // EAX
            CORINFO_HELP_CHECKED_ASSIGN_REF_ECX, // ECX
            -1,                                  // EDX (always the target address)
            CORINFO_HELP_CHECKED_ASSIGN_REF_EBX, // EBX
            -1,                                  // ESP
            CORINFO_HELP_CHECKED_ASSIGN_REF_EBP, // EBP
            CORINFO_HELP_CHECKED_ASSIGN_REF_ESI, // ESI
            CORINFO_HELP_CHECKED_ASSIGN_REF_EDI, // EDI
        },
    };

    noway_assert(regToHelper[0][REG_EAX] == CORINFO_HELP_ASSIGN_REF_EAX);
    noway_assert(regToHelper[0][REG_ECX] == CORINFO_HELP_ASSIGN_REF_ECX);
    noway_assert(regToHelper[0][REG_EBX] == CORINFO_HELP_ASSIGN_REF_EBX);
    noway_assert(regToHelper[0][REG_ESP] == -1);
    noway_assert(regToHelper[0][REG_EBP] == CORINFO_HELP_ASSIGN_REF_EBP);
    noway_assert(regToHelper[0][REG_ESI] == CORINFO_HELP_ASSIGN_REF_ESI);
    noway_assert(regToHelper[0][REG_EDI] == CORINFO_HELP_ASSIGN_REF_EDI);

    noway_assert(regToHelper[1][REG_EAX] == CORINFO_HELP_CHECKED_ASSIGN_REF_EAX);
    noway_assert(regToHelper[1][REG_ECX] == CORINFO_HELP_CHECKED_ASSIGN_REF_ECX);
    noway_assert(regToHelper[1][REG_EBX] == CORINFO_HELP_CHECKED_ASSIGN_REF_EBX);
    noway_assert(regToHelper[1][REG_ESP] == -1);
    noway_assert(regToHelper[1][REG_EBP] == CORINFO_HELP_CHECKED_ASSIGN_REF_EBP);
    noway_assert(regToHelper[1][REG_ESI] == CORINFO_HELP_CHECKED_ASSIGN_REF_ESI);
    noway_assert(regToHelper[1][REG_EDI] == CORINFO_HELP_CHECKED_ASSIGN_REF_EDI);

    regNumber reg = data->GetRegNum();
    noway_assert((reg != REG_ESP) && (reg != REG_WRITE_BARRIER));

    // Generate the following code:
    //            lea     edx, addr
    //            call    write_barrier_helper_reg

    // addr goes in REG_ARG_0
    genCopyRegIfNeeded(addr, REG_WRITE_BARRIER);

    unsigned tgtAnywhere = 0;
    if (writeBarrierForm != GCInfo::WBF_BarrierUnchecked)
    {
        tgtAnywhere = 1;
    }

    genEmitHelperCall(regToHelper[tgtAnywhere][reg],
                      0,           // argSize
                      EA_PTRSIZE); // retSize

    return true;
#else  // !defined(TARGET_X86) || !NOGC_WRITE_BARRIERS
    return false;
#endif // !defined(TARGET_X86) || !NOGC_WRITE_BARRIERS
}

// Produce code for a GT_CALL node
void CodeGen::genCallInstruction(GenTreeCall* call)
{
    genAlignStackBeforeCall(call);

    gtCallTypes callType = (gtCallTypes)call->gtCallType;

    IL_OFFSETX ilOffset = BAD_IL_OFFSET;

    // all virtuals should have been expanded into a control expression
    assert(!call->IsVirtual() || call->gtControlExpr || call->gtCallAddr);

    // Insert a GS check if necessary
    if (call->IsTailCallViaJitHelper())
    {
        if (compiler->getNeedsGSSecurityCookie())
        {
#if FEATURE_FIXED_OUT_ARGS
            // If either of the conditions below is true, we will need a temporary register in order to perform the GS
            // cookie check. When FEATURE_FIXED_OUT_ARGS is disabled, we save and restore the temporary register using
            // push/pop. When FEATURE_FIXED_OUT_ARGS is enabled, however, we need an alternative solution. For now,
            // though, the tail prefix is ignored on all platforms that use fixed out args, so we should never hit this
            // case.
            assert(compiler->gsGlobalSecurityCookieAddr == nullptr);
            assert((int)compiler->gsGlobalSecurityCookieVal == (ssize_t)compiler->gsGlobalSecurityCookieVal);
#endif
            genEmitGSCookieCheck(true);
        }
    }

    // Consume all the arg regs
    for (GenTreeCall::Use& use : call->LateArgs())
    {
        GenTree* argNode = use.GetNode();

        if (argNode->OperIs(GT_PUTARG_STK))
        {
            continue;
        }

        CallArgInfo* argInfo = call->GetArgInfoByArgNode(argNode->gtSkipReloadOrCopy());

#ifdef UNIX_AMD64_ABI
        if (argNode->OperIs(GT_FIELD_LIST))
        {
            unsigned regIndex = 0;
            for (GenTreeFieldList::Use& use : argNode->AsFieldList()->Uses())
            {
                GenTree* node = use.GetNode();

                regNumber argReg = argInfo->GetRegNum(regIndex++);
                regNumber srcReg = genConsumeReg(node);

                // TODO-MIKE-Review: Huh, this pulls EA_PTRSIZE out of the hat. Potential GC hole?
                // It may be that this code is useless. We have reg constraints on PUTARG_REG defs
                // so the arg is likely in the correct register already and nothing ever needs to
                // be moved.
                inst_Mov_Extend(node->GetType(), false, argReg, srcReg, /* canSkip */ true, EA_PTRSIZE);
            }

            continue;
        }
#endif // UNIX_AMD64_ABI

        regNumber argReg = argInfo->GetRegNum();
        regNumber srcReg = genConsumeReg(argNode);

        // TODO-MIKE-Review: Huh, this pulls EA_PTRSIZE out of the hat. Potential GC hole?
        inst_Mov_Extend(argNode->GetType(), false, argReg, srcReg, /* canSkip */ true, EA_PTRSIZE);

#if FEATURE_VARARG
        if (call->IsVarargs() && varTypeIsFloating(argNode->GetType()))
        {
            // For varargs calls on win-x64 we need to pass floating point register arguments in 2 registers:
            // the XMM reg that's normally used to pass a floating point arg and the GPR that's normally used
            // to pass an integer argument at the same position.
            regNumber intArgReg = compiler->getCallArgIntRegister(argNode->GetRegNum());
            GetEmitter()->emitIns_Mov(INS_movd, emitTypeSize(argNode->GetType()), intArgReg, srcReg,
                                      /* canSkip */ false);
        }
#endif
    }

#if defined(TARGET_X86) || defined(UNIX_AMD64_ABI)
    // The call will pop its arguments.
    // for each putarg_stk:
    target_ssize_t stackArgBytes = 0;
    for (GenTreeCall::Use& use : call->Args())
    {
        GenTree* arg = use.GetNode();
        if (arg->OperIs(GT_PUTARG_STK) && ((arg->gtFlags & GTF_LATE_ARG) == 0))
        {
            stackArgBytes += arg->AsPutArgStk()->GetArgSize();
        }
    }
#endif // defined(TARGET_X86) || defined(UNIX_AMD64_ABI)

    // Insert a null check on "this" pointer if asked.
    if (call->NeedsNullCheck())
    {
        const regNumber regThis = genGetThisArgReg(call);
        GetEmitter()->emitIns_AR_R(INS_cmp, EA_4BYTE, regThis, regThis, 0);
    }

    // Either gtControlExpr != null or gtCallAddr != null or it is a direct non-virtual call to a user or helper method.
    CORINFO_METHOD_HANDLE methHnd;
    GenTree*              target = call->gtControlExpr;
    if (callType == CT_INDIRECT)
    {
        assert(target == nullptr);
        target  = call->gtCallAddr;
        methHnd = nullptr;
    }
    else
    {
        methHnd = call->gtCallMethHnd;
    }

    CORINFO_SIG_INFO* sigInfo = nullptr;
#ifdef DEBUG
    // Pass the call signature information down into the emitter so the emitter can associate
    // native call sites with the signatures they were generated from.
    if (callType != CT_HELPER)
    {
        sigInfo = call->callSig;
    }
#endif // DEBUG

    // If fast tail call, then we are done.  In this case we setup the args (both reg args
    // and stack args in incoming arg area) and call target in rax.  Epilog sequence would
    // generate "jmp rax".
    if (call->IsFastTailCall())
    {
        // Don't support fast tail calling JIT helpers
        assert(callType != CT_HELPER);

        // If this is indirect then we go through RAX with epilog sequence
        // generating "jmp rax". Otherwise epilog will try to generate a
        // rip-relative jump.
        if (target != nullptr)
        {
            genConsumeReg(target);
            genCopyRegIfNeeded(target, REG_RAX);
        }

        return;
    }

    // For a pinvoke to unmanged code we emit a label to clear
    // the GC pointer state before the callsite.
    // We can't utilize the typical lazy killing of GC pointers
    // at (or inside) the callsite.
    if (compiler->killGCRefs(call))
    {
        genDefineTempLabel(genCreateTempLabel());
    }

    // Determine return value size(s).
    emitAttr retSize       = EA_PTRSIZE;
    emitAttr secondRetSize = EA_UNKNOWN;

    if (call->HasMultiRegRetVal())
    {
        retSize       = emitTypeSize(call->GetRegType(0));
        secondRetSize = emitTypeSize(call->GetRegType(1));
    }
    else
    {
        assert(!varTypeIsStruct(call));

        if (call->gtType == TYP_REF)
        {
            retSize = EA_GCREF;
        }
        else if (call->gtType == TYP_BYREF)
        {
            retSize = EA_BYREF;
        }
    }

#if defined(DEBUG) && defined(TARGET_X86)
    // Store the stack pointer so we can check it after the call.
    if (compiler->opts.compStackCheckOnCall && call->gtCallType == CT_USER_FUNC)
    {
        noway_assert(compiler->lvaCallSpCheck != 0xCCCCCCCC &&
                     compiler->lvaTable[compiler->lvaCallSpCheck].lvDoNotEnregister &&
                     compiler->lvaTable[compiler->lvaCallSpCheck].lvOnFrame);
        GetEmitter()->emitIns_S_R(ins_Store(TYP_I_IMPL), EA_PTRSIZE, REG_SPBASE, compiler->lvaCallSpCheck, 0);
    }
#endif // defined(DEBUG) && defined(TARGET_X86)

    bool            fPossibleSyncHelperCall = false;
    CorInfoHelpFunc helperNum               = CORINFO_HELP_UNDEF;

    // We need to propagate the IL offset information to the call instruction, so we can emit
    // an IL to native mapping record for the call, to support managed return value debugging.
    // We don't want tail call helper calls that were converted from normal calls to get a record,
    // so we skip this hash table lookup logic in that case.
    if (compiler->opts.compDbgInfo && compiler->genCallSite2ILOffsetMap != nullptr && !call->IsTailCall())
    {
        (void)compiler->genCallSite2ILOffsetMap->Lookup(call, &ilOffset);
    }

#if defined(TARGET_X86)
    bool fCallerPop = call->CallerPop();

    // If the callee pops the arguments, we pass a positive value as the argSize, and the emitter will
    // adjust its stack level accordingly.
    // If the caller needs to explicitly pop its arguments, we must pass a negative value, and then do the
    // pop when we're done.
    target_ssize_t argSizeForEmitter = stackArgBytes;
    if (fCallerPop)
    {
        argSizeForEmitter = -stackArgBytes;
    }
#endif // defined(TARGET_X86)

    // When it's a PInvoke call and the call type is USER function, we issue VZEROUPPER here
    // if the function contains 256bit AVX instructions, this is to avoid AVX-256 to Legacy SSE
    // transition penalty, assuming the user function contains legacy SSE instruction.
    // To limit code size increase impact: we only issue VZEROUPPER before PInvoke call, not issue
    // VZEROUPPER after PInvoke call because transition penalty from legacy SSE to AVX only happens
    // when there's preceding 256-bit AVX to legacy SSE transition penalty.
    if (call->IsPInvoke() && (call->gtCallType == CT_USER_FUNC) && GetEmitter()->Contains256bitAVX())
    {
        assert(compiler->canUseVexEncoding());
        instGen(INS_vzeroupper);
    }

    if (callType == CT_HELPER && compiler->info.compFlags & CORINFO_FLG_SYNCH)
    {
        fPossibleSyncHelperCall = true;
        helperNum               = compiler->eeGetHelperNum(methHnd);
        noway_assert(helperNum != CORINFO_HELP_UNDEF);
    }

    if (target != nullptr)
    {
#ifdef TARGET_X86
        if (call->IsVirtualStub() && (call->gtCallType == CT_INDIRECT))
        {
            // On x86, we need to generate a very specific pattern for indirect VSD calls:
            //
            //    3-byte nop
            //    call dword ptr [eax]
            //
            // Where EAX is also used as an argument to the stub dispatch helper. Make
            // sure that the call target address is computed into EAX in this case.

            assert(compiler->virtualStubParamInfo->GetReg() == REG_VIRTUAL_STUB_TARGET);

            assert(target->isContainedIndir());
            assert(target->OperGet() == GT_IND);

            GenTree* addr = target->AsIndir()->Addr();
            assert(addr->isUsedFromReg());

            genConsumeReg(addr);
            genCopyRegIfNeeded(addr, REG_VIRTUAL_STUB_TARGET);

            GetEmitter()->emitIns_Nop(3);

            // clang-format off
            GetEmitter()->emitIns_Call(emitter::EmitCallType(emitter::EC_INDIR_ARD),
                                       methHnd,
                                       INDEBUG_LDISASM_COMMA(sigInfo)
                                       nullptr,
                                       argSizeForEmitter,
                                       retSize
                                       MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(secondRetSize),
                                       gcInfo.gcVarPtrSetCur,
                                       gcInfo.gcRegGCrefSetCur,
                                       gcInfo.gcRegByrefSetCur,
                                       ilOffset, REG_VIRTUAL_STUB_TARGET, REG_NA, 1, 0);
            // clang-format on
        }
        else
#endif
            if (target->isContainedIndir())
        {
            if (target->AsIndir()->HasBase() && target->AsIndir()->Base()->isContainedIntOrIImmed())
            {
                // Note that if gtControlExpr is an indir of an absolute address, we mark it as
                // contained only if it can be encoded as PC-relative offset.
                assert(target->AsIndir()->Base()->AsIntConCommon()->FitsInAddrBase(compiler));

                // clang-format off
                genEmitCall(emitter::EC_FUNC_TOKEN_INDIR,
                            methHnd,
                            INDEBUG_LDISASM_COMMA(sigInfo)
                            (void*) target->AsIndir()->Base()->AsIntConCommon()->IconValue()
                            X86_ARG(argSizeForEmitter),
                            retSize
                            MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(secondRetSize),
                            ilOffset);
                // clang-format on
            }
            else
            {
                // clang-format off
                genEmitCall(emitter::EC_INDIR_ARD,
                            methHnd,
                            INDEBUG_LDISASM_COMMA(sigInfo)
                            target->AsIndir()
                            X86_ARG(argSizeForEmitter),
                            retSize
                            MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(secondRetSize),
                            ilOffset);
                // clang-format on
            }
        }
        else
        {
            // We have already generated code for gtControlExpr evaluating it into a register.
            // We just need to emit "call reg" in this case.
            assert(genIsValidIntReg(target->GetRegNum()));

            // clang-format off
            genEmitCall(emitter::EC_INDIR_R,
                        methHnd,
                        INDEBUG_LDISASM_COMMA(sigInfo)
                        nullptr // addr
                        X86_ARG(argSizeForEmitter),
                        retSize
                        MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(secondRetSize),
                        ilOffset,
                        genConsumeReg(target));
            // clang-format on
        }
    }
#ifdef FEATURE_READYTORUN_COMPILER
    else if (call->gtEntryPoint.addr != nullptr)
    {
        // clang-format off
        genEmitCall((call->gtEntryPoint.accessType == IAT_VALUE) ? emitter::EC_FUNC_TOKEN
                                                                 : emitter::EC_FUNC_TOKEN_INDIR,
                    methHnd,
                    INDEBUG_LDISASM_COMMA(sigInfo)
                    (void*) call->gtEntryPoint.addr
                    X86_ARG(argSizeForEmitter),
                    retSize
                    MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(secondRetSize),
                    ilOffset);
        // clang-format on
    }
#endif
    else
    {
        // Generate a direct call to a non-virtual user defined or helper method
        assert(callType == CT_HELPER || callType == CT_USER_FUNC);

        void* addr = nullptr;
        if (callType == CT_HELPER)
        {
            // Direct call to a helper method.
            helperNum = compiler->eeGetHelperNum(methHnd);
            noway_assert(helperNum != CORINFO_HELP_UNDEF);

            void* pAddr = nullptr;
            addr        = compiler->compGetHelperFtn(helperNum, (void**)&pAddr);
            assert(pAddr == nullptr);
        }
        else
        {
            // Direct call to a non-virtual user function.
            addr = call->gtDirectCallAddress;
        }

        assert(addr != nullptr);

        // Non-virtual direct calls to known addresses

        // clang-format off
        genEmitCall(emitter::EC_FUNC_TOKEN,
                    methHnd,
                    INDEBUG_LDISASM_COMMA(sigInfo)
                    addr
                    X86_ARG(argSizeForEmitter),
                    retSize
                    MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(secondRetSize),
                    ilOffset);
        // clang-format on
    }

    // if it was a pinvoke or intrinsic we may have needed to get the address of a label
    if (genPendingCallLabel)
    {
        genDefineInlineTempLabel(genPendingCallLabel);
        genPendingCallLabel = nullptr;
    }

    // Update GC info:
    // All Callee arg registers are trashed and no longer contain any GC pointers.
    // TODO-XArch-Bug?: As a matter of fact shouldn't we be killing all of callee trashed regs here?
    // For now we will assert that other than arg regs gc ref/byref set doesn't contain any other
    // registers from RBM_CALLEE_TRASH.
    assert((gcInfo.gcRegGCrefSetCur & (RBM_CALLEE_TRASH & ~RBM_ARG_REGS)) == 0);
    assert((gcInfo.gcRegByrefSetCur & (RBM_CALLEE_TRASH & ~RBM_ARG_REGS)) == 0);
    gcInfo.gcRegGCrefSetCur &= ~RBM_ARG_REGS;
    gcInfo.gcRegByrefSetCur &= ~RBM_ARG_REGS;

    var_types returnType = call->TypeGet();
    if (returnType != TYP_VOID)
    {
#ifdef TARGET_X86
        if (varTypeIsFloating(returnType))
        {
            // Spill the value from the fp stack.
            // Then, load it into the target register.
            call->gtFlags |= GTF_SPILL;
            regSet.rsSpillFPStack(call);
            call->gtFlags |= GTF_SPILLED;
            call->gtFlags &= ~GTF_SPILL;
        }
        else
#endif // TARGET_X86
        {
            if (call->HasMultiRegRetVal())
            {
                // If regs allocated to call node are different from ABI return
                // regs in which the call has returned its result, move the result
                // to regs allocated to call node.
                for (unsigned i = 0; i < call->GetRegCount(); ++i)
                {
                    var_types regType      = call->GetRegType(i);
                    regNumber returnReg    = call->GetRetDesc()->GetRegNum(i);
                    regNumber allocatedReg = call->GetRegNumByIdx(i);
                    inst_Mov(regType, allocatedReg, returnReg, /* canSkip */ true);
                }

#ifdef FEATURE_SIMD
                // A Vector3 return value is stored in xmm0 and xmm1.
                // RyuJIT assumes that the upper unused bits of xmm1 are cleared but
                // the native compiler doesn't guarantee it.
                if (returnType == TYP_SIMD12)
                {
                    regNumber returnReg = call->GetRetDesc()->GetRegNum(1);
                    // Clear the upper 32 bits by two shift instructions.
                    // retReg = retReg << 96
                    // retReg = retReg >> 96
                    GetEmitter()->emitIns_R_I(INS_pslldq, emitActualTypeSize(TYP_SIMD12), returnReg, 12);
                    GetEmitter()->emitIns_R_I(INS_psrldq, emitActualTypeSize(TYP_SIMD12), returnReg, 12);
                }
#endif // FEATURE_SIMD
            }
            else
            {
                regNumber returnReg;

#ifdef TARGET_X86
                if (call->IsHelperCall(compiler, CORINFO_HELP_INIT_PINVOKE_FRAME))
                {
                    // The x86 CORINFO_HELP_INIT_PINVOKE_FRAME helper uses a custom calling convention that returns with
                    // TCB in REG_PINVOKE_TCB. AMD64/ARM64 use the standard calling convention. fgMorphCall() sets the
                    // correct argument registers.
                    returnReg = REG_PINVOKE_TCB;
                }
                else
#endif // TARGET_X86
                    if (varTypeIsFloating(returnType))
                {
                    returnReg = REG_FLOATRET;
                }
                else
                {
                    returnReg = REG_INTRET;
                }

                inst_Mov(returnType, call->GetRegNum(), returnReg, /* canSkip */ true);
            }

            genProduceReg(call);
        }
    }

    // If there is nothing next, that means the result is thrown away, so this value is not live.
    // However, for minopts or debuggable code, we keep it live to support managed return value debugging.
    if ((call->gtNext == nullptr) && compiler->opts.OptimizationEnabled())
    {
        gcInfo.gcMarkRegSetNpt(RBM_INTRET);
    }

#if defined(DEBUG) && defined(TARGET_X86)
    if (compiler->opts.compStackCheckOnCall && call->gtCallType == CT_USER_FUNC)
    {
        noway_assert(compiler->lvaCallSpCheck != 0xCCCCCCCC &&
                     compiler->lvaTable[compiler->lvaCallSpCheck].lvDoNotEnregister &&
                     compiler->lvaTable[compiler->lvaCallSpCheck].lvOnFrame);
        if (!fCallerPop && (stackArgBytes != 0))
        {
            // ECX is trashed, so can be used to compute the expected SP. We saved the value of SP
            // after pushing all the stack arguments, but the caller popped the arguments, so we need
            // to do some math to figure a good comparison.
            GetEmitter()->emitIns_Mov(INS_mov, EA_4BYTE, REG_ARG_0, REG_SPBASE, /* canSkip */ false);
            GetEmitter()->emitIns_R_I(INS_sub, EA_4BYTE, REG_ARG_0, stackArgBytes);
            GetEmitter()->emitIns_S_R(INS_cmp, EA_4BYTE, REG_ARG_0, compiler->lvaCallSpCheck, 0);
        }
        else
        {
            GetEmitter()->emitIns_S_R(INS_cmp, EA_4BYTE, REG_SPBASE, compiler->lvaCallSpCheck, 0);
        }

        BasicBlock* sp_check = genCreateTempLabel();
        GetEmitter()->emitIns_J(INS_je, sp_check);
        instGen(INS_BREAKPOINT);
        genDefineTempLabel(sp_check);
    }
#endif // defined(DEBUG) && defined(TARGET_X86)

#if !defined(FEATURE_EH_FUNCLETS)
    //-------------------------------------------------------------------------
    // Create a label for tracking of region protected by the monitor in synchronized methods.
    // This needs to be here, rather than above where fPossibleSyncHelperCall is set,
    // so the GC state vars have been updated before creating the label.

    if (fPossibleSyncHelperCall)
    {
        switch (helperNum)
        {
            case CORINFO_HELP_MON_ENTER:
            case CORINFO_HELP_MON_ENTER_STATIC:
                noway_assert(compiler->syncStartEmitCookie == NULL);
                compiler->syncStartEmitCookie =
                    GetEmitter()->emitAddLabel(gcInfo.gcVarPtrSetCur, gcInfo.gcRegGCrefSetCur, gcInfo.gcRegByrefSetCur);
                noway_assert(compiler->syncStartEmitCookie != NULL);
                break;
            case CORINFO_HELP_MON_EXIT:
            case CORINFO_HELP_MON_EXIT_STATIC:
                noway_assert(compiler->syncEndEmitCookie == NULL);
                compiler->syncEndEmitCookie =
                    GetEmitter()->emitAddLabel(gcInfo.gcVarPtrSetCur, gcInfo.gcRegGCrefSetCur, gcInfo.gcRegByrefSetCur);
                noway_assert(compiler->syncEndEmitCookie != NULL);
                break;
            default:
                break;
        }
    }
#endif // !FEATURE_EH_FUNCLETS

    unsigned stackAdjustBias = 0;

#if defined(TARGET_X86)
    // Is the caller supposed to pop the arguments?
    if (fCallerPop && (stackArgBytes != 0))
    {
        stackAdjustBias = stackArgBytes;
    }

    SubtractStackLevel(stackArgBytes);
#endif // TARGET_X86

    genRemoveAlignmentAfterCall(call, stackAdjustBias);
}

// Produce code for a GT_JMP node.
// The arguments of the caller needs to be transferred to the callee before exiting caller.
// The actual jump to callee is generated as part of caller epilog sequence.
// Therefore the codegen of GT_JMP is to ensure that the callee arguments are correctly setup.
void CodeGen::genJmpMethod(GenTree* jmp)
{
    assert(jmp->OperGet() == GT_JMP);
    assert(compiler->compJmpOpUsed);

    // If no arguments, nothing to do
    if (compiler->info.compArgsCount == 0)
    {
        return;
    }

    // Make sure register arguments are in their initial registers
    // and stack arguments are put back as well.
    unsigned   varNum;
    LclVarDsc* varDsc;

    // First move any en-registered stack arguments back to the stack.
    // At the same time any reg arg not in correct reg is moved back to its stack location.
    //
    // We are not strictly required to spill reg args that are not in the desired reg for a jmp call
    // But that would require us to deal with circularity while moving values around.  Spilling
    // to stack makes the implementation simple, which is not a bad trade off given Jmp calls
    // are not frequent.
    for (varNum = 0; (varNum < compiler->info.compArgsCount); varNum++)
    {
        varDsc = compiler->lvaTable + varNum;

        if (varDsc->lvPromoted)
        {
            noway_assert(varDsc->lvFieldCnt == 1); // We only handle one field here

            unsigned fieldVarNum = varDsc->lvFieldLclStart;
            varDsc               = compiler->lvaTable + fieldVarNum;
        }
        noway_assert(varDsc->lvIsParam);

        if (varDsc->lvIsRegArg && (varDsc->GetRegNum() != REG_STK))
        {
            // Skip reg args which are already in its right register for jmp call.
            // If not, we will spill such args to their stack locations.
            //
            // If we need to generate a tail call profiler hook, then spill all
            // arg regs to free them up for the callback.
            if (!compiler->compIsProfilerHookNeeded() && (varDsc->GetRegNum() == varDsc->GetArgReg()))
            {
                continue;
            }
        }
        else if (varDsc->GetRegNum() == REG_STK)
        {
            // Skip args which are currently living in stack.
            continue;
        }

        // If we came here it means either a reg argument not in the right register or
        // a stack argument currently living in a register.  In either case the following
        // assert should hold.
        assert(varDsc->GetRegNum() != REG_STK);

        assert(!varDsc->lvIsStructField || (compiler->lvaTable[varDsc->lvParentLcl].lvFieldCnt == 1));
        var_types storeType = genActualType(varDsc->lvaArgType()); // We own the memory and can use the full move.
        GetEmitter()->emitIns_S_R(ins_Store(storeType), emitTypeSize(storeType), varDsc->GetRegNum(), varNum, 0);

        // Update lvRegNum life and GC info to indicate lvRegNum is dead and varDsc stack slot is going live.
        // Note that we cannot modify varDsc->GetRegNum() here because another basic block may not be expecting it.
        // Therefore manually update life of varDsc->GetRegNum().
        regMaskTP tempMask = varDsc->lvRegMask();
        regSet.RemoveMaskVars(tempMask);
        gcInfo.gcMarkRegSetNpt(tempMask);
        if (compiler->lvaIsGCTracked(varDsc))
        {
#ifdef DEBUG
            if (!VarSetOps::IsMember(compiler, gcInfo.gcVarPtrSetCur, varDsc->lvVarIndex))
            {
                JITDUMP("\t\t\t\t\t\t\tVar V%02u becoming live\n", varNum);
            }
            else
            {
                JITDUMP("\t\t\t\t\t\t\tVar V%02u continuing live\n", varNum);
            }
#endif // DEBUG

            VarSetOps::AddElemD(compiler, gcInfo.gcVarPtrSetCur, varDsc->lvVarIndex);
        }
    }

#ifdef PROFILING_SUPPORTED
    // At this point all arg regs are free.
    // Emit tail call profiler callback.
    genProfilingLeaveCallback(CORINFO_HELP_PROF_FCN_TAILCALL);
#endif

    // Next move any un-enregistered register arguments back to their register.
    regMaskTP fixedIntArgMask = RBM_NONE;    // tracks the int arg regs occupying fixed args in case of a vararg method.
    unsigned  firstArgVarNum  = BAD_VAR_NUM; // varNum of the first argument in case of a vararg method.
    for (varNum = 0; (varNum < compiler->info.compArgsCount); varNum++)
    {
        varDsc = compiler->lvaTable + varNum;
        if (varDsc->lvPromoted)
        {
            noway_assert(varDsc->lvFieldCnt == 1); // We only handle one field here

            unsigned fieldVarNum = varDsc->lvFieldLclStart;
            varDsc               = compiler->lvaTable + fieldVarNum;
        }
        noway_assert(varDsc->lvIsParam);

        // Skip if arg not passed in a register.
        if (!varDsc->lvIsRegArg)
        {
            continue;
        }

#if defined(UNIX_AMD64_ABI)
        if (varTypeIsStruct(varDsc->GetType()))
        {
            assert(varDsc->GetLayout()->GetSysVAmd64AbiRegCount() != 0);

            // Move the values into the right registers.
            //

            // Update varDsc->GetArgReg() and lvOtherArgReg life and GC Info to indicate varDsc stack slot is dead and
            // argReg is going live. Note that we cannot modify varDsc->GetRegNum() and lvOtherArgReg here
            // because another basic block may not be expecting it.
            // Therefore manually update life of argReg.  Note that GT_JMP marks
            // the end of the basic block and after which reg life and gc info will be recomputed for the new block in
            // genCodeForBBList().

            var_types type = varActualType(varDsc->GetLayout()->GetSysVAmd64AbiRegType(0));
            GetEmitter()->emitIns_R_S(ins_Load(type), emitTypeSize(type), varDsc->GetArgReg(), varNum, 0);
            regSet.SetMaskVars(regSet.GetMaskVars() | genRegMask(varDsc->GetArgReg()));
            gcInfo.gcMarkRegPtrVal(varDsc->GetArgReg(), type);

            if (varDsc->GetLayout()->GetSysVAmd64AbiRegCount() > 1)
            {
                type = varActualType(varDsc->GetLayout()->GetSysVAmd64AbiRegType(1));
                GetEmitter()->emitIns_R_S(ins_Load(type), emitTypeSize(type), varDsc->GetOtherArgReg(), varNum, 8);
                regSet.SetMaskVars(regSet.GetMaskVars() | genRegMask(varDsc->GetOtherArgReg()));
                gcInfo.gcMarkRegPtrVal(varDsc->GetOtherArgReg(), type);
            }

            if (varDsc->lvTracked)
            {
                VarSetOps::RemoveElemD(compiler, gcInfo.gcVarPtrSetCur, varDsc->lvVarIndex);
            }
        }
        else
#endif // !defined(UNIX_AMD64_ABI)
        {
            // Register argument
            CLANG_FORMAT_COMMENT_ANCHOR;
#ifdef TARGET_X86
            noway_assert(
                isRegParamType(genActualType(varDsc->TypeGet())) ||
                (varTypeIsStruct(varDsc->TypeGet()) && compiler->isTrivialPointerSizedStruct(varDsc->GetLayout())));
#else
            noway_assert(isRegParamType(genActualType(varDsc->TypeGet())));
#endif // TARGET_X86

            // Is register argument already in the right register?
            // If not load it from its stack location.
            var_types loadType = varDsc->lvaArgType();

#ifdef TARGET_X86
            if (varTypeIsStruct(varDsc->TypeGet()))
            {
                // Treat trivial pointer-sized structs as a pointer sized primitive
                // for the purposes of registers.
                loadType = TYP_I_IMPL;
            }
#endif

            regNumber argReg = varDsc->GetArgReg(); // incoming arg register

            if (varDsc->GetRegNum() != argReg)
            {
                assert(genIsValidReg(argReg));
                GetEmitter()->emitIns_R_S(ins_Load(loadType), emitTypeSize(loadType), argReg, varNum, 0);

                // Update argReg life and GC Info to indicate varDsc stack slot is dead and argReg is going live.
                // Note that we cannot modify varDsc->GetRegNum() here because another basic block may not be
                // expecting it. Therefore manually update life of argReg.  Note that GT_JMP marks the end of the
                // basic block and after which reg life and gc info will be recomputed for the new block in
                // genCodeForBBList().
                regSet.AddMaskVars(genRegMask(argReg));
                gcInfo.gcMarkRegPtrVal(argReg, loadType);
                if (compiler->lvaIsGCTracked(varDsc))
                {
#ifdef DEBUG
                    if (VarSetOps::IsMember(compiler, gcInfo.gcVarPtrSetCur, varDsc->lvVarIndex))
                    {
                        JITDUMP("\t\t\t\t\t\t\tVar V%02u becoming dead\n", varNum);
                    }
                    else
                    {
                        JITDUMP("\t\t\t\t\t\t\tVar V%02u continuing dead\n", varNum);
                    }
#endif // DEBUG

                    VarSetOps::RemoveElemD(compiler, gcInfo.gcVarPtrSetCur, varDsc->lvVarIndex);
                }
            }
        }

#if FEATURE_VARARG && defined(TARGET_AMD64)
        // In case of a jmp call to a vararg method also pass the float/double arg in the corresponding int arg
        // register. This is due to the AMD64 ABI which requires floating point values passed to varargs functions to
        // be passed in both integer and floating point registers. It doesn't apply to x86, which passes floating point
        // values on the stack.
        if (compiler->info.compIsVarArgs)
        {
            regNumber intArgReg;
            var_types loadType = varDsc->lvaArgType();
            regNumber argReg   = varDsc->GetArgReg(); // incoming arg register

            if (varTypeIsFloating(loadType))
            {
                intArgReg = compiler->getCallArgIntRegister(argReg);
                inst_Mov(TYP_LONG, intArgReg, argReg, /* canSkip */ false, emitActualTypeSize(loadType));
            }
            else
            {
                intArgReg = argReg;
            }

            fixedIntArgMask |= genRegMask(intArgReg);

            if (intArgReg == REG_ARG_0)
            {
                assert(firstArgVarNum == BAD_VAR_NUM);
                firstArgVarNum = varNum;
            }
        }
#endif // FEATURE_VARARG
    }

#if FEATURE_VARARG && defined(TARGET_AMD64)
    // Jmp call to a vararg method - if the method has fewer than 4 fixed arguments,
    // load the remaining arg registers (both int and float) from the corresponding
    // shadow stack slots.  This is for the reason that we don't know the number and type
    // of non-fixed params passed by the caller, therefore we have to assume the worst case
    // of caller passing float/double args both in int and float arg regs.
    //
    // This doesn't apply to x86, which doesn't pass floating point values in floating
    // point registers.
    //
    // The caller could have passed gc-ref/byref type var args.  Since these are var args
    // the callee no way of knowing their gc-ness.  Therefore, mark the region that loads
    // remaining arg registers from shadow stack slots as non-gc interruptible.
    if (fixedIntArgMask != RBM_NONE)
    {
        assert(compiler->info.compIsVarArgs);
        assert(firstArgVarNum != BAD_VAR_NUM);

        regMaskTP remainingIntArgMask = RBM_ARG_REGS & ~fixedIntArgMask;
        if (remainingIntArgMask != RBM_NONE)
        {
            GetEmitter()->emitDisableGC();
            for (int argNum = 0, argOffset = 0; argNum < MAX_REG_ARG; ++argNum)
            {
                regNumber argReg     = intArgRegs[argNum];
                regMaskTP argRegMask = genRegMask(argReg);

                if ((remainingIntArgMask & argRegMask) != 0)
                {
                    remainingIntArgMask &= ~argRegMask;
                    GetEmitter()->emitIns_R_S(INS_mov, EA_8BYTE, argReg, firstArgVarNum, argOffset);

                    // also load it in corresponding float arg reg
                    regNumber floatReg = compiler->getCallArgFloatRegister(argReg);
                    inst_RV_RV(ins_Copy(argReg, TYP_DOUBLE), floatReg, argReg, TYP_I_IMPL);
                }

                argOffset += REGSIZE_BYTES;
            }
            GetEmitter()->emitEnableGC();
        }
    }
#endif // FEATURE_VARARG
}

// produce code for a GT_LEA subnode
void CodeGen::genLeaInstruction(GenTreeAddrMode* lea)
{
    emitAttr size = emitTypeSize(lea);
    genConsumeOperands(lea);

    if (lea->Base() && lea->Index())
    {
        regNumber baseReg  = lea->Base()->GetRegNum();
        regNumber indexReg = lea->Index()->GetRegNum();
        GetEmitter()->emitIns_R_ARX(INS_lea, size, lea->GetRegNum(), baseReg, indexReg, lea->gtScale, lea->Offset());
    }
    else if (lea->Base())
    {
        GetEmitter()->emitIns_R_AR(INS_lea, size, lea->GetRegNum(), lea->Base()->GetRegNum(), lea->Offset());
    }
    else if (lea->Index())
    {
        GetEmitter()->emitIns_R_ARX(INS_lea, size, lea->GetRegNum(), REG_NA, lea->Index()->GetRegNum(), lea->gtScale,
                                    lea->Offset());
    }

    genProduceReg(lea);
}

//------------------------------------------------------------------------
// genCompareFloat: Generate code for comparing two floating point values
//
// Arguments:
//    treeNode - the compare tree
//
void CodeGen::genCompareFloat(GenTree* treeNode)
{
    assert(treeNode->OperIsCompare());

    GenTreeOp* tree    = treeNode->AsOp();
    GenTree*   op1     = tree->gtOp1;
    GenTree*   op2     = tree->gtOp2;
    var_types  op1Type = op1->TypeGet();
    var_types  op2Type = op2->TypeGet();

    genConsumeOperands(tree);

    assert(varTypeIsFloating(op1Type));
    assert(op1Type == op2Type);

    regNumber   targetReg = treeNode->GetRegNum();
    instruction ins;
    emitAttr    cmpAttr;

    GenCondition condition = GenCondition::FromFloatRelop(treeNode);

    if (condition.PreferSwap())
    {
        condition = GenCondition::Swap(condition);
        std::swap(op1, op2);
    }

    ins     = ins_FloatCompare(op1Type);
    cmpAttr = emitTypeSize(op1Type);

    GetEmitter()->emitInsBinary(ins, cmpAttr, op1, op2);

    // Are we evaluating this into a register?
    if (targetReg != REG_NA)
    {
        if ((condition.GetCode() == GenCondition::FNEU) && (op1->GetRegNum() == op2->GetRegNum()))
        {
            // For floating point, `x != x` is a common way of
            // checking for NaN. So, in the case where both
            // operands are the same, we can optimize codegen
            // to only do a single check.

            condition = GenCondition(GenCondition::P);
        }

        inst_SETCC(condition, treeNode->TypeGet(), targetReg);
        genProduceReg(tree);
    }
}

//------------------------------------------------------------------------
// genCompareInt: Generate code for comparing ints or, on amd64, longs.
//
// Arguments:
//    treeNode - the compare tree
//
// Return Value:
//    None.
void CodeGen::genCompareInt(GenTree* treeNode)
{
    assert(treeNode->OperIsCompare() || treeNode->OperIs(GT_CMP));

    GenTreeOp* tree          = treeNode->AsOp();
    GenTree*   op1           = tree->gtOp1;
    GenTree*   op2           = tree->gtOp2;
    var_types  op1Type       = op1->TypeGet();
    var_types  op2Type       = op2->TypeGet();
    regNumber  targetReg     = tree->GetRegNum();
    emitter*   emit          = GetEmitter();
    bool       canReuseFlags = false;

    genConsumeOperands(tree);

    assert(!op1->isContainedIntOrIImmed());
    assert(!varTypeIsFloating(op2Type));

    instruction ins;
    var_types   type = TYP_UNDEF;

    if (tree->OperIs(GT_TEST_EQ, GT_TEST_NE))
    {
        ins = INS_test;

        // Unlike many xarch instructions TEST doesn't have a form with a 16/32/64 bit first operand and
        // an 8 bit immediate second operand. But if the immediate value fits in 8 bits then we can simply
        // emit a 8 bit TEST instruction, unless we're targeting x86 and the first operand is a non-byteable
        // register.
        // Note that lowering does something similar but its main purpose is to allow memory operands to be
        // contained so it doesn't handle other kind of operands. It could do more but on x86 that results
        // in additional register constrains and that may be worse than wasting 3 bytes on an immediate.
        if (
#ifdef TARGET_X86
            (!op1->isUsedFromReg() || isByteReg(op1->GetRegNum())) &&
#endif
            (op2->IsCnsIntOrI() && genSmallTypeCanRepresentValue(TYP_UBYTE, op2->AsIntCon()->IconValue())))
        {
            type = TYP_UBYTE;
        }
    }
    else if (op1->isUsedFromReg() && op2->IsIntegralConst(0))
    {
        if (compiler->opts.OptimizationEnabled())
        {
            emitAttr op1Size = emitActualTypeSize(op1->TypeGet());
            assert((int)op1Size >= 4);

            // Optimize "x<0" and "x>=0" to "x>>31" if "x" is not a jump condition and in a reg.
            // Morph/Lowering are responsible to rotate "0<x" to "x>0" so we won't handle it here.
            if ((targetReg != REG_NA) && tree->OperIs(GT_LT, GT_GE) && !tree->IsUnsigned())
            {
                inst_Mov(op1->TypeGet(), targetReg, op1->GetRegNum(), /* canSkip */ true);
                if (tree->OperIs(GT_GE))
                {
                    // emit "not" for "x>=0" case
                    inst_RV(INS_not, targetReg, op1->TypeGet());
                }
                inst_RV_IV(INS_shr_N, targetReg, (int)op1Size * 8 - 1, op1Size);
                genProduceReg(tree);
                return;
            }
            canReuseFlags = true;
        }

        // We're comparing a register to 0 so we can generate "test reg1, reg1"
        // instead of the longer "cmp reg1, 0"
        ins = INS_test;
        op2 = op1;
    }
    else
    {
        ins = INS_cmp;
    }

    if (type == TYP_UNDEF)
    {
        if (op1Type == op2Type)
        {
            type = op1Type;
        }
        else if (genTypeSize(op1Type) == genTypeSize(op2Type))
        {
            // If the types are different but have the same size then we'll use TYP_INT or TYP_LONG.
            // This primarily deals with small type mixes (e.g. byte/ubyte) that need to be widened
            // and compared as int. We should not get long type mixes here but handle that as well
            // just in case.
            type = genTypeSize(op1Type) == 8 ? TYP_LONG : TYP_INT;
        }
        else
        {
            // In the types are different simply use TYP_INT. This deals with small type/int type
            // mixes (e.g. byte/short ubyte/int) that need to be widened and compared as int.
            // Lowering is expected to handle any mixes that involve long types (e.g. int/long).
            type = TYP_INT;
        }

        // The common type cannot be smaller than any of the operand types, we're probably mixing int/long
        assert(genTypeSize(type) >= max(genTypeSize(op1Type), genTypeSize(op2Type)));
        // Small unsigned int types (TYP_BOOL can use anything) should use unsigned comparisons
        assert(!(varTypeIsSmallInt(type) && varTypeIsUnsigned(type)) || ((tree->gtFlags & GTF_UNSIGNED) != 0));
        // If op1 is smaller then it cannot be in memory, we're probably missing a cast
        assert((genTypeSize(op1Type) >= genTypeSize(type)) || !op1->isUsedFromMemory());
        // If op2 is smaller then it cannot be in memory, we're probably missing a cast
        assert((genTypeSize(op2Type) >= genTypeSize(type)) || !op2->isUsedFromMemory());
        // If we ended up with a small type and op2 is a constant then make sure we don't lose constant bits
        assert(!op2->IsCnsIntOrI() || !varTypeIsSmall(type) ||
               genSmallTypeCanRepresentValue(type, op2->AsIntCon()->IconValue()));
    }

    // The type cannot be larger than the machine word size
    assert(genTypeSize(type) <= genTypeSize(TYP_I_IMPL));
    // TYP_UINT and TYP_ULONG should not appear here, only small types can be unsigned
    assert(!varTypeIsUnsigned(type) || varTypeIsSmall(type));

    bool needsOCFlags = !tree->OperIs(GT_EQ, GT_NE);
    if (canReuseFlags && emit->AreFlagsSetToZeroCmp(op1->GetRegNum(), emitTypeSize(type), needsOCFlags))
    {
        JITDUMP("Not emitting compare due to flags being already set\n");
    }
    else
    {
        emit->emitInsBinary(ins, emitTypeSize(type), op1, op2);
    }

    // Are we evaluating this into a register?
    if (targetReg != REG_NA)
    {
        inst_SETCC(GenCondition::FromIntegralRelop(tree), tree->TypeGet(), targetReg);
        genProduceReg(tree);
    }
}

#if !defined(TARGET_64BIT)
//------------------------------------------------------------------------
// genLongToIntCast: Generate code for long to int casts on x86.
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

    genConsumeRegs(src);

    var_types srcType  = ((cast->gtFlags & GTF_UNSIGNED) != 0) ? TYP_ULONG : TYP_LONG;
    var_types dstType  = cast->CastToType();
    regNumber loSrcReg = src->gtGetOp1()->GetRegNum();
    regNumber hiSrcReg = src->gtGetOp2()->GetRegNum();
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

            inst_RV_RV(INS_test, loSrcReg, loSrcReg, TYP_INT, EA_4BYTE);
            inst_JMP(EJ_js, allOne);

            inst_RV_RV(INS_test, hiSrcReg, hiSrcReg, TYP_INT, EA_4BYTE);
            genJumpToThrowHlpBlk(EJ_jne, SCK_OVERFLOW);
            inst_JMP(EJ_jmp, success);

            genDefineTempLabel(allOne);
            inst_RV_IV(INS_cmp, hiSrcReg, -1, EA_4BYTE);
            genJumpToThrowHlpBlk(EJ_jne, SCK_OVERFLOW);

            genDefineTempLabel(success);
        }
        else
        {
            if ((srcType == TYP_ULONG) && (dstType == TYP_INT))
            {
                inst_RV_RV(INS_test, loSrcReg, loSrcReg, TYP_INT, EA_4BYTE);
                genJumpToThrowHlpBlk(EJ_js, SCK_OVERFLOW);
            }

            inst_RV_RV(INS_test, hiSrcReg, hiSrcReg, TYP_INT, EA_4BYTE);
            genJumpToThrowHlpBlk(EJ_jne, SCK_OVERFLOW);
        }
    }

    inst_Mov(TYP_INT, dstReg, loSrcReg, /* canSkip */ true);

    genProduceReg(cast);
}
#endif

//------------------------------------------------------------------------
// genIntCastOverflowCheck: Generate overflow checking code for an integer cast.
//
// Arguments:
//    cast - The GT_CAST node
//    desc - The cast description
//    reg  - The register containing the value to check
//
void CodeGen::genIntCastOverflowCheck(GenTreeCast* cast, const GenIntCastDesc& desc, regNumber reg)
{
    switch (desc.CheckKind())
    {
        case GenIntCastDesc::CHECK_POSITIVE:
            GetEmitter()->emitIns_R_R(INS_test, EA_SIZE(desc.CheckSrcSize()), reg, reg);
            genJumpToThrowHlpBlk(EJ_jl, SCK_OVERFLOW);
            break;

#ifdef TARGET_64BIT
        case GenIntCastDesc::CHECK_UINT_RANGE:
        {
            // We need to check if the value is not greater than 0xFFFFFFFF but this value
            // cannot be encoded in an immediate operand. Use a right shift to test if the
            // upper 32 bits are zero. This requires a temporary register.
            const regNumber tempReg = cast->GetSingleTempReg();
            assert(tempReg != reg);
            GetEmitter()->emitIns_Mov(INS_mov, EA_8BYTE, tempReg, reg, /* canSkip */ false);
            GetEmitter()->emitIns_R_I(INS_shr_N, EA_8BYTE, tempReg, 32);
            genJumpToThrowHlpBlk(EJ_jne, SCK_OVERFLOW);
        }
        break;

        case GenIntCastDesc::CHECK_POSITIVE_INT_RANGE:
            GetEmitter()->emitIns_R_I(INS_cmp, EA_8BYTE, reg, INT32_MAX);
            genJumpToThrowHlpBlk(EJ_ja, SCK_OVERFLOW);
            break;

        case GenIntCastDesc::CHECK_INT_RANGE:
            GetEmitter()->emitIns_R_I(INS_cmp, EA_8BYTE, reg, INT32_MAX);
            genJumpToThrowHlpBlk(EJ_jg, SCK_OVERFLOW);
            GetEmitter()->emitIns_R_I(INS_cmp, EA_8BYTE, reg, INT32_MIN);
            genJumpToThrowHlpBlk(EJ_jl, SCK_OVERFLOW);
            break;
#endif

        default:
        {
            assert(desc.CheckKind() == GenIntCastDesc::CHECK_SMALL_INT_RANGE);
            const int castMaxValue = desc.CheckSmallIntMax();
            const int castMinValue = desc.CheckSmallIntMin();

            GetEmitter()->emitIns_R_I(INS_cmp, EA_SIZE(desc.CheckSrcSize()), reg, castMaxValue);
            genJumpToThrowHlpBlk((castMinValue == 0) ? EJ_ja : EJ_jg, SCK_OVERFLOW);

            if (castMinValue != 0)
            {
                GetEmitter()->emitIns_R_I(INS_cmp, EA_SIZE(desc.CheckSrcSize()), reg, castMinValue);
                genJumpToThrowHlpBlk(EJ_jl, SCK_OVERFLOW);
            }
        }
        break;
    }
}

//------------------------------------------------------------------------
// genIntToIntCast: Generate code for an integer cast, with or without overflow check.
//
// Arguments:
//    cast - The GT_CAST node
//
// Assumptions:
//    Neither the source nor target type can be a floating point type.
//    On x86 casts to (U)BYTE require that the source be in a byte register if not contained.
//
void CodeGen::genIntToIntCast(GenTreeCast* cast)
{
    GenTree* src = cast->GetOp(0);

    genConsumeRegs(src);

    regNumber       srcReg = src->GetRegNum();
    const regNumber dstReg = cast->GetRegNum();

    assert(genIsValidIntReg(dstReg));

    GenIntCastDesc desc(cast);

    if (src->isUsedFromMemory())
    {
        instruction ins;

        switch (desc.LoadKind())
        {
            case GenIntCastDesc::LOAD_ZERO_EXTEND_SMALL_INT:
                ins = INS_movzx;
                break;
            case GenIntCastDesc::LOAD_SIGN_EXTEND_SMALL_INT:
                ins = INS_movsx;
                break;
#ifdef TARGET_64BIT
            case GenIntCastDesc::LOAD_SIGN_EXTEND_INT:
                ins = INS_movsxd;
                break;
#endif
            default:
                assert(desc.LoadKind() == GenIntCastDesc::LOAD);
                ins = INS_mov;
                break;
        }

        // Note that we load directly into the destination register, this avoids the
        // need for a temporary register but assumes that enregistered variables are
        // not live in exception handlers. This works with EHWriteThru because the
        // register will be written only in genProduceReg, after the actual cast is
        // performed.

        GetEmitter()->emitInsBinary(ins, EA_ATTR(desc.LoadSrcSize()), cast, src);

        srcReg = dstReg;
    }

    assert(genIsValidIntReg(srcReg));

    if (desc.CheckKind() != GenIntCastDesc::CHECK_NONE)
    {
        genIntCastOverflowCheck(cast, desc, srcReg);
    }

    instruction ins;
    unsigned    insSize;
    bool        canSkip = false;

    switch (desc.ExtendKind())
    {
        case GenIntCastDesc::ZERO_EXTEND_SMALL_INT:
            ins     = INS_movzx;
            insSize = desc.ExtendSrcSize();
            break;
        case GenIntCastDesc::SIGN_EXTEND_SMALL_INT:
            ins     = INS_movsx;
            insSize = desc.ExtendSrcSize();
            break;
#ifdef TARGET_64BIT
        case GenIntCastDesc::ZERO_EXTEND_INT:
            ins     = INS_mov;
            insSize = 4;
            // We can skip emitting this zero extending move if the previous instruction zero extended implicitly
            canSkip = compiler->opts.OptimizationEnabled() && GetEmitter()->AreUpper32BitsZero(srcReg);
            break;
        case GenIntCastDesc::SIGN_EXTEND_INT:
            ins     = INS_movsxd;
            insSize = 4;
            break;
#endif
        default:
            assert(desc.ExtendKind() == GenIntCastDesc::COPY);
            ins     = INS_mov;
            insSize = desc.ExtendSrcSize();
            canSkip = true;
            break;
    }

    GetEmitter()->emitIns_Mov(ins, EA_ATTR(insSize), dstReg, srcReg, canSkip);

    genProduceReg(cast);
}

//------------------------------------------------------------------------
// genFloatToFloatCast: Generate code for a cast between float and double
//
// Arguments:
//    cast - The GT_CAST node
//
void CodeGen::genFloatToFloatCast(GenTreeCast* cast)
{
    assert(!cast->gtOverflow());

    GenTree*  src     = cast->GetOp(0);
    var_types srcType = src->GetType();
    var_types dstType = cast->GetCastType();

    assert((srcType == TYP_FLOAT) || (srcType == TYP_DOUBLE));
    assert((dstType == TYP_FLOAT) || (dstType == TYP_DOUBLE));
    assert(cast->GetType() == dstType);

    assert(genIsValidFloatReg(cast->GetRegNum()));
    assert(!src->isUsedFromReg() || genIsValidFloatReg(src->GetRegNum()));

    genConsumeRegs(src);

    instruction ins     = INS_none;
    emitAttr    insSize = emitTypeSize(dstType);

    if (srcType != dstType)
    {
        ins = srcType == TYP_FLOAT ? INS_cvtss2sd : INS_cvtsd2ss;
    }
    else if (!src->isUsedFromReg())
    {
        ins = srcType == TYP_FLOAT ? INS_movss : INS_movsdsse2;
    }
    else
    {
        // TODO-MIKE-Review: How come we end up with a FLOAT-to-FLOAT cast in RayTracer.dll!?!
        GetEmitter()->emitIns_Mov(INS_movaps, EA_16BYTE, cast->GetRegNum(), src->GetRegNum(), /*canSkip*/ true);
    }

    if (ins != INS_none)
    {
        GetEmitter()->emitInsBinary(ins, insSize, cast, src);
    }

    genProduceReg(cast);
}

//------------------------------------------------------------------------
// genIntToFloatCast: Generate code to cast an int/long to float/double
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

    if (cast->IsUnsigned())
    {
        srcType = genUnsignedType(srcType);
    }

#ifdef TARGET_64BIT
    noway_assert((srcType == TYP_INT) || (srcType == TYP_LONG) || (srcType == TYP_ULONG));
#else
    noway_assert(srcType == TYP_INT);
#endif

    assert((dstType == TYP_FLOAT) || (dstType == TYP_DOUBLE));
    assert(cast->GetType() == dstType);

    genConsumeRegs(src);
    regNumber srcReg = src->isUsedFromReg() ? src->GetRegNum() : REG_NA;
    regNumber dstReg = cast->GetRegNum();

    assert((srcReg == REG_NA) || genIsValidIntReg(srcReg));
    assert(genIsValidFloatReg(dstReg));

    // The source value is never a small int but it may be produced by a small int typed
    // IND or other memory node and in that case the source must not be contained.
    assert(!varTypeIsSmall(src->GetType()) || (srcReg != REG_NA));

    // To convert int to a float/double, cvtsi2ss/sd SSE2 instruction is used
    // which does a partial write to lower 4/8 bytes of xmm register keeping the other
    // upper bytes unmodified.  If "cvtsi2ss/sd xmmReg, r32/r64" occurs inside a loop,
    // the partial write could introduce a false dependency and could cause a stall
    // if there are further uses of xmmReg. We have such a case occurring with a
    // customer reported version of SpectralNorm benchmark, resulting in 2x perf
    // regression.  To avoid false dependency, we emit "xorps xmmReg, xmmReg" before
    // cvtsi2ss/sd instruction.
    GetEmitter()->emitIns_R_R(INS_xorps, EA_16BYTE, dstReg, dstReg);

    instruction ins     = (dstType == TYP_FLOAT) ? INS_cvtsi2ss : INS_cvtsi2sd;
    emitAttr    insSize = emitTypeSize(srcType);

    GetEmitter()->emitInsBinary(ins, insSize, cast, src);

#ifdef TARGET_64BIT
    // Handle the case of srcType = TYP_ULONG. SSE2 conversion instruction
    // will interpret ULONG value as LONG.  Hence we need to adjust the
    // result if sign-bit of srcType is set.
    if (srcType == TYP_ULONG)
    {
        assert(srcReg != REG_NA);

        // The instruction sequence below is less accurate than what clang and gcc generate.
        // However, we keep the current sequence for backward compatibility. If we change the
        // instructions below, FloatingPointUtils::convertUInt64ToDobule should be also updated
        // for consistent conversion result.

        BasicBlock* label = genCreateTempLabel();
        GetEmitter()->emitIns_R_R(INS_test, EA_8BYTE, srcReg, srcReg);
        inst_JMP(EJ_jge, label);

        if (dstType == TYP_DOUBLE)
        {
            if (u8ToDblBitmask == nullptr)
            {
                u8ToDblBitmask =
                    GetEmitter()->emitFltOrDblConst(jitstd::bit_cast<double>(0x43f0000000000000ULL), EA_8BYTE);
            }
            GetEmitter()->emitIns_R_C(INS_addsd, EA_8BYTE, dstReg, u8ToDblBitmask, 0);
        }
        else
        {
            if (u8ToFltBitmask == nullptr)
            {
                u8ToFltBitmask = GetEmitter()->emitFltOrDblConst(jitstd::bit_cast<float>(0x5f800000U), EA_4BYTE);
            }
            GetEmitter()->emitIns_R_C(INS_addss, EA_4BYTE, dstReg, u8ToFltBitmask, 0);
        }

        genDefineTempLabel(label);
    }
#endif

    genProduceReg(cast);
}

//------------------------------------------------------------------------
// genFloatToIntCast: Generate code to cast float/double to int/long
//
// Arguments:
//    treeNode - The GT_CAST node
//
void CodeGen::genFloatToIntCast(GenTreeCast* cast)
{
    assert(!cast->gtOverflow());

    GenTree*  src     = cast->GetOp(0);
    var_types srcType = src->GetType();
    var_types dstType = cast->GetCastType();

    assert((srcType == TYP_FLOAT) || (srcType == TYP_DOUBLE));

#ifndef TARGET_64BIT
    noway_assert(dstType == TYP_INT);
    assert(cast->GetType() == TYP_INT);
#else
    noway_assert((dstType == TYP_INT) || (dstType == TYP_UINT) || (dstType == TYP_LONG));
    assert(cast->GetType() == varActualType(dstType));

    // TODO-XArch-CQ: (Low-pri): Jit64 generates in-line code of 8 instructions for
    // FLOAT/DOUBLE to ULONG casts.
    // There are hardly any occurrences of this conversion operation in platform
    // assemblies or in CQ perf benchmarks (1 occurrence in mscorlib, microsoft.jscript,
    // 1 occurence in Roslyn and no occurrences in system, system.core, system.numerics
    // system.windows.forms, scimark, fractals, bio mums). If we ever find evidence that
    // doing this optimization is a win, should consider generating in-lined code.

    // If the dstType is TYP_UINT, we have 32-bits to encode the
    // float number. Any of 33rd or above bits can be the sign bit.
    // To achieve it we pretend as if we are converting it to a long.
    if (dstType == TYP_UINT)
    {
        dstType = TYP_LONG;
    }
#endif

    genConsumeRegs(src);

    assert(genIsValidIntReg(cast->GetRegNum()));
    assert(!src->isUsedFromReg() || genIsValidFloatReg(src->GetRegNum()));

    instruction ins     = (srcType == TYP_FLOAT) ? INS_cvttss2si : INS_cvttsd2si;
    emitAttr    insSize = emitTypeSize(dstType);

    GetEmitter()->emitInsBinary(ins, insSize, cast, src);

    genProduceReg(cast);
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
// TODO-XArch-CQ - mark the operand as contained if known to be in
// memory (e.g. field or an array element).
//
void CodeGen::genCkfinite(GenTree* treeNode)
{
    assert(treeNode->OperGet() == GT_CKFINITE);

    GenTree*  op1        = treeNode->AsOp()->gtOp1;
    var_types targetType = treeNode->TypeGet();
    int       expMask    = (targetType == TYP_FLOAT) ? 0x7F800000 : 0x7FF00000; // Bit mask to extract exponent.
    regNumber targetReg  = treeNode->GetRegNum();

    // Extract exponent into a register.
    regNumber tmpReg = treeNode->GetSingleTempReg();

    genConsumeReg(op1);

#ifdef TARGET_64BIT

    // Copy the floating-point value to an integer register. If we copied a float to a long, then
    // right-shift the value so the high 32 bits of the floating-point value sit in the low 32
    // bits of the integer register.
    regNumber srcReg        = op1->GetRegNum();
    var_types targetIntType = ((targetType == TYP_FLOAT) ? TYP_INT : TYP_LONG);
    inst_Mov(targetIntType, tmpReg, srcReg, /* canSkip */ false, emitActualTypeSize(targetType));
    if (targetType == TYP_DOUBLE)
    {
        // right shift by 32 bits to get to exponent.
        inst_RV_SH(INS_shr, EA_8BYTE, tmpReg, 32);
    }

    // Mask exponent with all 1's and check if the exponent is all 1's
    inst_RV_IV(INS_and, tmpReg, expMask, EA_4BYTE);
    inst_RV_IV(INS_cmp, tmpReg, expMask, EA_4BYTE);

    // If exponent is all 1's, throw ArithmeticException
    genJumpToThrowHlpBlk(EJ_je, SCK_ARITH_EXCPN);

    // if it is a finite value copy it to targetReg
    inst_Mov(targetType, targetReg, op1->GetRegNum(), /* canSkip */ true);

#else // !TARGET_64BIT

    // If the target type is TYP_DOUBLE, we want to extract the high 32 bits into the register.
    // There is no easy way to do this. To not require an extra register, we'll use shuffles
    // to move the high 32 bits into the low 32 bits, then shuffle it back, since we
    // need to produce the value into the target register.
    //
    // For TYP_DOUBLE, we'll generate (for targetReg != op1->GetRegNum()):
    //    movaps targetReg, op1->GetRegNum()
    //    shufps targetReg, targetReg, 0xB1    // WZYX => ZWXY
    //    mov_xmm2i tmpReg, targetReg          // tmpReg <= Y
    //    and tmpReg, <mask>
    //    cmp tmpReg, <mask>
    //    je <throw block>
    //    movaps targetReg, op1->GetRegNum()   // copy the value again, instead of un-shuffling it
    //
    // For TYP_DOUBLE with (targetReg == op1->GetRegNum()):
    //    shufps targetReg, targetReg, 0xB1    // WZYX => ZWXY
    //    mov_xmm2i tmpReg, targetReg          // tmpReg <= Y
    //    and tmpReg, <mask>
    //    cmp tmpReg, <mask>
    //    je <throw block>
    //    shufps targetReg, targetReg, 0xB1    // ZWXY => WZYX
    //
    // For TYP_FLOAT, it's the same as TARGET_64BIT:
    //    mov_xmm2i tmpReg, targetReg          // tmpReg <= low 32 bits
    //    and tmpReg, <mask>
    //    cmp tmpReg, <mask>
    //    je <throw block>
    //    movaps targetReg, op1->GetRegNum()      // only if targetReg != op1->GetRegNum()

    regNumber copyToTmpSrcReg; // The register we'll copy to the integer temp.

    if (targetType == TYP_DOUBLE)
    {
        inst_Mov(targetType, targetReg, op1->GetRegNum(), /* canSkip */ true);
        inst_RV_RV_IV(INS_shufps, EA_16BYTE, targetReg, targetReg, (int8_t)0xb1);
        copyToTmpSrcReg = targetReg;
    }
    else
    {
        copyToTmpSrcReg = op1->GetRegNum();
    }

    // Copy only the low 32 bits. This will be the high order 32 bits of the floating-point
    // value, no matter the floating-point type.
    inst_Mov(TYP_INT, tmpReg, copyToTmpSrcReg, /* canSkip */ false, emitActualTypeSize(TYP_FLOAT));

    // Mask exponent with all 1's and check if the exponent is all 1's
    inst_RV_IV(INS_and, tmpReg, expMask, EA_4BYTE);
    inst_RV_IV(INS_cmp, tmpReg, expMask, EA_4BYTE);

    // If exponent is all 1's, throw ArithmeticException
    genJumpToThrowHlpBlk(EJ_je, SCK_ARITH_EXCPN);

    if ((targetType == TYP_DOUBLE) && (targetReg == op1->GetRegNum()))
    {
        // We need to re-shuffle the targetReg to get the correct result.
        inst_RV_RV_IV(INS_shufps, EA_16BYTE, targetReg, targetReg, (int8_t)0xb1);
    }
    else
    {
        // In both the TYP_FLOAT and TYP_DOUBLE case, the op1 register is untouched,
        // so copy it to the targetReg. This is faster and smaller for TYP_DOUBLE
        // than re-shuffling the targetReg.
        inst_Mov(targetType, targetReg, op1->GetRegNum(), /* canSkip */ true);
    }

#endif // !TARGET_64BIT

    genProduceReg(treeNode);
}

#ifdef TARGET_AMD64
int CodeGenInterface::genSPtoFPdelta() const
{
    int delta;

#ifdef UNIX_AMD64_ABI

    // We require frame chaining on Unix to support native tool unwinding (such as
    // unwinding by the native debugger). We have a CLR-only extension to the
    // unwind codes (UWOP_SET_FPREG_LARGE) to support SP->FP offsets larger than 240.
    // If Unix ever supports EnC, the RSP == RBP assumption will have to be reevaluated.
    delta = genTotalFrameSize();

#else // !UNIX_AMD64_ABI

    // As per Amd64 ABI, RBP offset from initial RSP can be between 0 and 240 if
    // RBP needs to be reported in unwind codes.  This case would arise for methods
    // with localloc.
    if (compiler->compLocallocUsed)
    {
        // We cannot base delta computation on compLclFrameSize since it changes from
        // tentative to final frame layout and hence there is a possibility of
        // under-estimating offset of vars from FP, which in turn results in under-
        // estimating instruction size.
        //
        // To be predictive and so as never to under-estimate offset of vars from FP
        // we will always position FP at min(240, outgoing arg area size).
        delta = Min(240, (int)compiler->lvaOutgoingArgSpaceSize);
    }
    else if (compiler->opts.compDbgEnC)
    {
        // vm assumption on EnC methods is that rsp and rbp are equal
        delta = 0;
    }
    else
    {
        delta = genTotalFrameSize();
    }

#endif // !UNIX_AMD64_ABI

    return delta;
}

//---------------------------------------------------------------------
// genTotalFrameSize - return the total size of the stack frame, including local size,
// callee-saved register size, etc. For AMD64, this does not include the caller-pushed
// return address.
//
// Return value:
//    Total frame size
//

int CodeGenInterface::genTotalFrameSize() const
{
    assert(!IsUninitialized(compiler->compCalleeRegsPushed));

    int totalFrameSize = compiler->compCalleeRegsPushed * REGSIZE_BYTES + compiler->compLclFrameSize;

    assert(totalFrameSize >= 0);
    return totalFrameSize;
}

//---------------------------------------------------------------------
// genCallerSPtoFPdelta - return the offset from Caller-SP to the frame pointer.
// This number is going to be negative, since the Caller-SP is at a higher
// address than the frame pointer.
//
// There must be a frame pointer to call this function!
//
// We can't compute this directly from the Caller-SP, since the frame pointer
// is based on a maximum delta from Initial-SP, so first we find SP, then
// compute the FP offset.

int CodeGenInterface::genCallerSPtoFPdelta() const
{
    assert(isFramePointerUsed());
    int callerSPtoFPdelta;

    callerSPtoFPdelta = genCallerSPtoInitialSPdelta() + genSPtoFPdelta();

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

    callerSPtoSPdelta -= genTotalFrameSize();
    callerSPtoSPdelta -= REGSIZE_BYTES; // caller-pushed return address

    // compCalleeRegsPushed does not account for the frame pointer
    // TODO-Cleanup: shouldn't this be part of genTotalFrameSize?
    if (isFramePointerUsed())
    {
        callerSPtoSPdelta -= REGSIZE_BYTES;
    }

    assert(callerSPtoSPdelta <= 0);
    return callerSPtoSPdelta;
}
#endif // TARGET_AMD64

//-----------------------------------------------------------------------------------------
// genSSE2BitwiseOp - generate SSE2 code for the given oper as "Operand BitWiseOp BitMask"
//
// Arguments:
//    treeNode  - tree node
//
// Return value:
//    None
//
// Assumptions:
//     i) tree oper is one of GT_NEG or GT_INTRINSIC Abs()
//    ii) tree type is floating point type.
//   iii) caller of this routine needs to call genProduceReg()
void CodeGen::genSSE2BitwiseOp(GenTree* treeNode)
{
    regNumber targetReg  = treeNode->GetRegNum();
    regNumber operandReg = genConsumeReg(treeNode->gtGetOp1());
    emitAttr  size       = emitTypeSize(treeNode);

    assert(varTypeIsFloating(treeNode->TypeGet()));
    assert(treeNode->gtGetOp1()->isUsedFromReg());

    CORINFO_FIELD_HANDLE* maskFld = nullptr;
    UINT64                mask    = 0;
    instruction           ins     = INS_invalid;

    if (treeNode->OperIs(GT_NEG))
    {
        // Neg(x) = flip the sign bit.
        // Neg(f) = f ^ 0x80000000 x4 (packed)
        // Neg(d) = d ^ 0x8000000000000000 x2 (packed)
        ins     = INS_xorps;
        mask    = treeNode->TypeIs(TYP_FLOAT) ? 0x8000000080000000UL : 0x8000000000000000UL;
        maskFld = treeNode->TypeIs(TYP_FLOAT) ? &negBitmaskFlt : &negBitmaskDbl;
    }
    else if (treeNode->OperIs(GT_INTRINSIC))
    {
        assert(treeNode->AsIntrinsic()->gtIntrinsicName == NI_System_Math_Abs);
        // Abs(x) = set sign-bit to zero
        // Abs(f) = f & 0x7fffffff x4 (packed)
        // Abs(d) = d & 0x7fffffffffffffff x2 (packed)
        ins     = INS_andps;
        mask    = treeNode->TypeIs(TYP_FLOAT) ? 0x7fffffff7fffffffUL : 0x7fffffffffffffffUL;
        maskFld = treeNode->TypeIs(TYP_FLOAT) ? &absBitmaskFlt : &absBitmaskDbl;
    }
    else
    {
        assert(!"genSSE2BitwiseOp: unsupported oper");
    }

    if (*maskFld == nullptr)
    {
        UINT64 maskPack[] = {mask, mask};
        *maskFld          = GetEmitter()->emitBlkConst(&maskPack, 16, 16, treeNode->TypeGet());
    }

    GetEmitter()->emitIns_SIMD_R_R_C(ins, size, targetReg, operandReg, *maskFld, 0);
}

//-----------------------------------------------------------------------------------------
// genSSE41RoundOp - generate SSE41 code for the given tree as a round operation
//
// Arguments:
//    treeNode  - tree node
//
// Return value:
//    None
//
// Assumptions:
//     i) SSE4.1 is supported by the underlying hardware
//    ii) treeNode oper is a GT_INTRINSIC
//   iii) treeNode type is a floating point type
//    iv) treeNode is not used from memory
//     v) tree oper is NI_System_Math{F}_Round, _Ceiling, or _Floor
//    vi) caller of this routine needs to call genProduceReg()
void CodeGen::genSSE41RoundOp(GenTreeOp* treeNode)
{
    // i) SSE4.1 is supported by the underlying hardware
    assert(compiler->compIsaSupportedDebugOnly(InstructionSet_SSE41));

    // ii) treeNode oper is a GT_INTRINSIC
    assert(treeNode->OperGet() == GT_INTRINSIC);

    GenTree* srcNode = treeNode->gtGetOp1();

    // iii) treeNode type is floating point type
    assert(varTypeIsFloating(srcNode));
    assert(srcNode->TypeGet() == treeNode->TypeGet());

    // iv) treeNode is not used from memory
    assert(!treeNode->isUsedFromMemory());

    genConsumeOperands(treeNode);

    instruction ins  = (treeNode->TypeGet() == TYP_FLOAT) ? INS_roundss : INS_roundsd;
    emitAttr    size = emitTypeSize(treeNode);

    regNumber dstReg = treeNode->GetRegNum();

    unsigned ival = 0;

    // v) tree oper is NI_System_Math{F}_Round, _Ceiling, or _Floor
    switch (treeNode->AsIntrinsic()->gtIntrinsicName)
    {
        case NI_System_Math_Round:
            ival = 4;
            break;

        case NI_System_Math_Ceiling:
            ival = 10;
            break;

        case NI_System_Math_Floor:
            ival = 9;
            break;

        default:
            ins = INS_invalid;
            assert(!"genSSE41RoundOp: unsupported intrinsic");
            unreached();
    }

    if (srcNode->isContained() || srcNode->isUsedFromSpillTemp())
    {
        emitter* emit = GetEmitter();

        TempDsc* tmpDsc = nullptr;
        unsigned varNum = BAD_VAR_NUM;
        unsigned offset = (unsigned)-1;

        if (srcNode->isUsedFromSpillTemp())
        {
            assert(srcNode->IsRegOptional());

            tmpDsc = getSpillTempDsc(srcNode);
            varNum = tmpDsc->tdTempNum();
            offset = 0;

            regSet.tmpRlsTemp(tmpDsc);
        }
        else if (srcNode->isIndir())
        {
            GenTreeIndir* memIndir = srcNode->AsIndir();
            GenTree*      memBase  = memIndir->gtOp1;

            switch (memBase->OperGet())
            {
                case GT_LCL_VAR_ADDR:
                case GT_LCL_FLD_ADDR:
                {
                    assert(memBase->isContained());
                    varNum = memBase->AsLclVarCommon()->GetLclNum();
                    offset = memBase->AsLclVarCommon()->GetLclOffs();

                    // Ensure that all the GenTreeIndir values are set to their defaults.
                    assert(memBase->GetRegNum() == REG_NA);
                    assert(!memIndir->HasIndex());
                    assert(memIndir->Scale() == 1);
                    assert(memIndir->Offset() == 0);

                    break;
                }

                case GT_CLS_VAR_ADDR:
                {
                    emit->emitIns_R_C_I(ins, size, dstReg, memBase->AsClsVar()->gtClsVarHnd, 0, ival);
                    return;
                }

                default:
                {
                    emit->emitIns_R_A_I(ins, size, dstReg, memIndir, ival);
                    return;
                }
            }
        }
        else
        {
            switch (srcNode->OperGet())
            {
                case GT_CNS_DBL:
                {
                    GenTreeDblCon*       dblConst = srcNode->AsDblCon();
                    CORINFO_FIELD_HANDLE hnd = emit->emitFltOrDblConst(dblConst->gtDconVal, emitTypeSize(dblConst));

                    emit->emitIns_R_C_I(ins, size, dstReg, hnd, 0, ival);
                    return;
                }

                case GT_LCL_FLD:
                    varNum = srcNode->AsLclFld()->GetLclNum();
                    offset = srcNode->AsLclFld()->GetLclOffs();
                    break;

                case GT_LCL_VAR:
                {
                    assert(srcNode->IsRegOptional() ||
                           !compiler->lvaTable[srcNode->AsLclVar()->GetLclNum()].lvIsRegCandidate());

                    varNum = srcNode->AsLclVar()->GetLclNum();
                    offset = 0;
                    break;
                }

                default:
                    unreached();
                    break;
            }
        }

        // Ensure we got a good varNum and offset.
        // We also need to check for `tmpDsc != nullptr` since spill temp numbers
        // are negative and start with -1, which also happens to be BAD_VAR_NUM.
        assert((varNum != BAD_VAR_NUM) || (tmpDsc != nullptr));
        assert(offset != (unsigned)-1);

        emit->emitIns_R_S_I(ins, size, dstReg, varNum, offset, ival);
    }
    else
    {
        inst_RV_RV_IV(ins, size, dstReg, srcNode->GetRegNum(), ival);
    }
}

//---------------------------------------------------------------------
// genIntrinsic - generate code for a given intrinsic
//
// Arguments
//    treeNode - the GT_INTRINSIC node
//
// Return value:
//    None
//
void CodeGen::genIntrinsic(GenTree* treeNode)
{
    // Handle intrinsics that can be implemented by target-specific instructions
    switch (treeNode->AsIntrinsic()->gtIntrinsicName)
    {
        case NI_System_Math_Abs:
            genSSE2BitwiseOp(treeNode);
            break;

        case NI_System_Math_Ceiling:
        case NI_System_Math_Floor:
        case NI_System_Math_Round:
            genSSE41RoundOp(treeNode->AsOp());
            break;

        case NI_System_Math_Sqrt:
        {
            // Both operand and its result must be of the same floating point type.
            GenTree* srcNode = treeNode->AsOp()->gtOp1;
            assert(varTypeIsFloating(srcNode));
            assert(srcNode->TypeGet() == treeNode->TypeGet());

            genConsumeOperands(treeNode->AsOp());
            GetEmitter()->emitInsBinary(ins_FloatSqrt(treeNode->TypeGet()), emitTypeSize(treeNode), treeNode, srcNode);
            break;
        }

        default:
            assert(!"genIntrinsic: Unsupported intrinsic");
            unreached();
    }

    genProduceReg(treeNode);
}

void CodeGen::inst_BitCast(var_types dstType, regNumber dstReg, var_types srcType, regNumber srcReg)
{
    assert(!varTypeIsSmall(dstType));
    assert(!varTypeIsSmall(srcType));

    const bool srcIsFloat = varTypeUsesFloatReg(srcType);
    assert(srcIsFloat == genIsValidFloatReg(srcReg));

    const bool dstIsFloat = varTypeUsesFloatReg(dstType);
    assert(dstIsFloat == genIsValidFloatReg(dstReg));

    inst_Mov(varActualType(dstType), dstReg, srcReg, /* canSkip */ true);
}

void CodeGen::genCodeForBitCast(GenTreeUnOp* bitcast)
{
    GenTree*  src     = bitcast->GetOp(0);
    var_types dstType = bitcast->GetType();
    regNumber dstReg  = bitcast->GetRegNum();

    genConsumeRegs(src);

    if (src->isContained())
    {
        unsigned    lclNum = src->AsLclVar()->GetLclNum();
        instruction ins    = ins_Load(dstType, compiler->lvaIsSimdTypedLocalAligned(lclNum));
        GetEmitter()->emitIns_R_S(ins, emitTypeSize(dstType), dstReg, lclNum, 0);
    }
    else
    {
        inst_BitCast(dstType, dstReg, src->GetType(), src->GetRegNum());
    }

    genProduceReg(bitcast);
}

//-------------------------------------------------------------------------- //
// getBaseVarForPutArgStk - returns the baseVarNum for passing a stack arg.
//
// Arguments
//    treeNode - the GT_PUTARG_STK node
//
// Return value:
//    The number of the base variable.
//
// Note:
//    If tail call the outgoing args are placed in the caller's incoming arg stack space.
//    Otherwise, they go in the outgoing arg area on the current frame.
//
//    On Windows the caller always creates slots (homing space) in its frame for the
//    first 4 arguments of a callee (register passed args). So, the baseVarNum is always 0.
//    For System V systems there is no such calling convention requirement, and the code needs to find
//    the first stack passed argument from the caller. This is done by iterating over
//    all the lvParam variables and finding the first with GetArgReg() equals to REG_STK.
//
unsigned CodeGen::getBaseVarForPutArgStk(GenTree* treeNode)
{
    assert(treeNode->OperGet() == GT_PUTARG_STK);

    unsigned baseVarNum;

    // Whether to setup stk arg in incoming or out-going arg area?
    // Fast tail calls implemented as epilog+jmp = stk arg is setup in incoming arg area.
    // All other calls - stk arg is setup in out-going arg area.
    if (treeNode->AsPutArgStk()->PutInIncomingArgArea())
    {
        // See the note in the function header re: finding the first stack passed argument.
        baseVarNum = getFirstArgWithStackSlot();
        assert(baseVarNum != BAD_VAR_NUM);

#ifdef DEBUG
        // This must be a fast tail call.
        assert(treeNode->AsPutArgStk()->GetCall()->IsFastTailCall());

        // Since it is a fast tail call, the existence of first incoming arg is guaranteed
        // because fast tail call requires that in-coming arg area of caller is >= out-going
        // arg area required for tail call.
        LclVarDsc* varDsc = &(compiler->lvaTable[baseVarNum]);
        assert(varDsc != nullptr);

#ifdef UNIX_AMD64_ABI
        assert(!varDsc->lvIsRegArg && varDsc->GetArgReg() == REG_STK);
#else  // !UNIX_AMD64_ABI
        // On Windows this assert is always true. The first argument will always be in REG_ARG_0 or REG_FLTARG_0.
        assert(varDsc->lvIsRegArg && (varDsc->GetArgReg() == REG_ARG_0 || varDsc->GetArgReg() == REG_FLTARG_0));
#endif // !UNIX_AMD64_ABI
#endif // !DEBUG
    }
    else
    {
#if FEATURE_FIXED_OUT_ARGS
        baseVarNum = compiler->lvaOutgoingArgSpaceVar;
#else  // !FEATURE_FIXED_OUT_ARGS
        assert(!"No BaseVarForPutArgStk on x86");
        baseVarNum = BAD_VAR_NUM;
#endif // !FEATURE_FIXED_OUT_ARGS
    }

    return baseVarNum;
}

//---------------------------------------------------------------------
// genAlignStackBeforeCall: Align the stack if necessary before a call.
//
// Arguments:
//    putArgStk - the putArgStk node.
//
void CodeGen::genAlignStackBeforeCall(GenTreePutArgStk* putArgStk)
{
#if defined(UNIX_X86_ABI)

    genAlignStackBeforeCall(putArgStk->GetCall());

#endif // UNIX_X86_ABI
}

//---------------------------------------------------------------------
// genAlignStackBeforeCall: Align the stack if necessary before a call.
//
// Arguments:
//    call - the call node.
//
void CodeGen::genAlignStackBeforeCall(GenTreeCall* call)
{
#if defined(UNIX_X86_ABI)

    // Have we aligned the stack yet?
    if (!call->GetInfo()->IsStkAlignmentDone())
    {
        // We haven't done any stack alignment yet for this call.  We might need to create
        // an alignment adjustment, even if this function itself doesn't have any stack args.
        // This can happen if this function call is part of a nested call sequence, and the outer
        // call has already pushed some arguments.

        unsigned stkLevel = genStackLevel + call->GetInfo()->GetStkSizeBytes();
        call->GetInfo()->ComputeStackAlignment(stkLevel);

        unsigned padStkAlign = call->GetInfo()->GetStkAlign();
        if (padStkAlign != 0)
        {
            // Now generate the alignment
            inst_RV_IV(INS_sub, REG_SPBASE, padStkAlign, EA_PTRSIZE);
            AddStackLevel(padStkAlign);
            AddNestedAlignment(padStkAlign);
        }

        call->GetInfo()->SetStkAlignmentDone();
    }

#endif // UNIX_X86_ABI
}

//---------------------------------------------------------------------
// genRemoveAlignmentAfterCall: After a call, remove the alignment
// added before the call, if any.
//
// Arguments:
//    call - the call node.
//    bias - additional stack adjustment
//
// Note:
//    When bias > 0, caller should adjust stack level appropriately as
//    bias is not considered when adjusting stack level.
//
void CodeGen::genRemoveAlignmentAfterCall(GenTreeCall* call, unsigned bias)
{
#if defined(TARGET_X86)
#if defined(UNIX_X86_ABI)
    // Put back the stack pointer if there was any padding for stack alignment
    unsigned padStkAlign  = call->GetInfo()->GetStkAlign();
    unsigned padStkAdjust = padStkAlign + bias;

    if (padStkAdjust != 0)
    {
        inst_RV_IV(INS_add, REG_SPBASE, padStkAdjust, EA_PTRSIZE);
        SubtractStackLevel(padStkAlign);
        SubtractNestedAlignment(padStkAlign);
    }
#else  // UNIX_X86_ABI
    if (bias != 0)
    {
        genAdjustSP(bias);
    }
#endif // !UNIX_X86_ABI_
#else  // TARGET_X86
    assert(bias == 0);
#endif // !TARGET_X86
}

#ifdef TARGET_X86

//---------------------------------------------------------------------
// genPreAdjustStackForPutArgStk: Adjust the stack pointer before a non-push stack put arg
//
// Arguments:
//    argSize - the size of the argument
//
void CodeGen::genPreAdjustStackForPutArgStk(unsigned argSize)
{
    // If argSize is large, we need to probe the stack like we do in the prolog (genAllocLclFrame)
    // or for localloc (genLclHeap), to ensure we touch the stack pages sequentially, and don't miss
    // the stack guard pages. The prolog probes, but we don't know at this point how much higher
    // the last probed stack pointer value is. We default a threshold. Any size below this threshold
    // we are guaranteed the stack has been probed. Above this threshold, we don't know. The threshold
    // should be high enough to cover all common cases. Increasing the threshold means adding a few
    // more "lowest address of stack" probes in the prolog. Since this is relatively rare, add it to
    // stress modes.

    if ((argSize >= ARG_STACK_PROBE_THRESHOLD_BYTES) || compiler->compStressCompile(Compiler::STRESS_GENERIC_VARN, 5))
    {
        genStackPointerConstantAdjustmentLoopWithProbe(-(ssize_t)argSize, REG_NA);
    }
    else
    {
        inst_RV_IV(INS_sub, REG_SPBASE, argSize, EA_PTRSIZE);
    }

    AddStackLevel(argSize);
}

//---------------------------------------------------------------------
// genPutArgStkFieldList - generate code for passing a GT_FIELD_LIST arg on the stack.
//
// Arguments
//    treeNode      - the GT_PUTARG_STK node whose op1 is a GT_FIELD_LIST
//
// Return value:
//    None
//
void CodeGen::genPutArgStkFieldList(GenTreePutArgStk* putArgStk)
{
    GenTreeFieldList* const fieldList = putArgStk->gtOp1->AsFieldList();
    assert(fieldList != nullptr);

    assert((putArgStk->gtPutArgStkKind == GenTreePutArgStk::Kind::Push) ||
           (putArgStk->gtPutArgStkKind == GenTreePutArgStk::Kind::PushAllSlots));

    bool pushStkArg = true;

    // If we have pre-adjusted the stack and are simply storing the fields in order, set the offset to 0.
    // (Note that this mode is not currently being used.)
    // If we are pushing the arguments (i.e. we have not pre-adjusted the stack), then we are pushing them
    // in reverse order, so we start with the current field offset at the size of the struct arg (which must be
    // a multiple of the target pointer size).
    unsigned  currentOffset   = putArgStk->GetArgSize();
    unsigned  prevFieldOffset = currentOffset;
    regNumber intTmpReg       = REG_NA;
    regNumber simdTmpReg      = REG_NA;
    if (putArgStk->AvailableTempRegCount() != 0)
    {
        regMaskTP rsvdRegs = putArgStk->gtRsvdRegs;
        if ((rsvdRegs & RBM_ALLINT) != 0)
        {
            intTmpReg = putArgStk->GetSingleTempReg(RBM_ALLINT);
            assert(genIsValidIntReg(intTmpReg));
        }
        if ((rsvdRegs & RBM_ALLFLOAT) != 0)
        {
            simdTmpReg = putArgStk->GetSingleTempReg(RBM_ALLFLOAT);
            assert(genIsValidFloatReg(simdTmpReg));
        }
        assert(genCountBits(rsvdRegs) == (unsigned)((intTmpReg == REG_NA) ? 0 : 1) + ((simdTmpReg == REG_NA) ? 0 : 1));
    }

    for (GenTreeFieldList::Use& use : fieldList->Uses())
    {
        GenTree* const fieldNode   = use.GetNode();
        const unsigned fieldOffset = use.GetOffset();
        var_types      fieldType   = use.GetType();

        // Long-typed nodes should have been handled by the decomposition pass, and lowering should have sorted the
        // field list in descending order by offset.
        assert(!varTypeIsLong(fieldType));
        assert(fieldOffset <= prevFieldOffset);

        // Consume the register, if any, for this field. Note that genConsumeRegs() will appropriately
        // update the liveness info for a lclVar that has been marked RegOptional, which hasn't been
        // assigned a register, and which is therefore contained.
        // Unlike genConsumeReg(), it handles the case where no registers are being consumed.
        genConsumeRegs(fieldNode);
        regNumber argReg = fieldNode->isUsedFromSpillTemp() ? REG_NA : fieldNode->GetRegNum();

        // If the field is slot-like, we can use a push instruction to store the entire register no matter the type.
        //
        // The GC encoder requires that the stack remain 4-byte aligned at all times. Round the adjustment up
        // to the next multiple of 4. If we are going to generate a `push` instruction, the adjustment must
        // not require rounding.
        // NOTE: if the field is of GC type, we must use a push instruction, since the emitter is not otherwise
        // able to detect stores into the outgoing argument area of the stack on x86.
        const bool fieldIsSlot = ((fieldOffset % 4) == 0) && ((prevFieldOffset - fieldOffset) >= 4);
        int        adjustment  = roundUp(currentOffset - fieldOffset, 4);
        if (fieldIsSlot && !varTypeIsSIMD(fieldType))
        {
            fieldType         = genActualType(fieldType);
            unsigned pushSize = genTypeSize(fieldType);
            assert((pushSize % 4) == 0);
            adjustment -= pushSize;
            while (adjustment != 0)
            {
                inst_IV(INS_push, 0);
                currentOffset -= pushSize;
                AddStackLevel(pushSize);
                adjustment -= pushSize;
            }
            pushStkArg = true;
        }
        else
        {
            pushStkArg = false;

            // We always "push" floating point fields (i.e. they are full slot values that don't
            // require special handling).
            assert(varTypeIsIntegralOrI(fieldNode) || varTypeIsSIMD(fieldNode));

            // If we can't push this field, it needs to be in a register so that we can store
            // it to the stack location.
            if (adjustment != 0)
            {
                // This moves the stack pointer to fieldOffset.
                // For this case, we must adjust the stack and generate stack-relative stores rather than pushes.
                // Adjust the stack pointer to the next slot boundary.
                inst_RV_IV(INS_sub, REG_SPBASE, adjustment, EA_PTRSIZE);
                currentOffset -= adjustment;
                AddStackLevel(adjustment);
            }

            // Does it need to be in a byte register?
            // If so, we'll use intTmpReg, which must have been allocated as a byte register.
            // If it's already in a register, but not a byteable one, then move it.
            if (varTypeIsByte(fieldType) && ((argReg == REG_NA) || ((genRegMask(argReg) & RBM_BYTE_REGS) == 0)))
            {
                assert(intTmpReg != REG_NA);
                noway_assert((genRegMask(intTmpReg) & RBM_BYTE_REGS) != 0);
                if (argReg != REG_NA)
                {
                    inst_Mov(fieldType, intTmpReg, argReg, /* canSkip */ false);
                    argReg = intTmpReg;
                }
            }
        }

        if (argReg == REG_NA)
        {
            if (pushStkArg)
            {
                if (fieldNode->isUsedFromSpillTemp())
                {
                    assert(!varTypeIsSIMD(fieldType)); // Q: can we get here with SIMD?
                    assert(fieldNode->IsRegOptional());
                    TempDsc* tmp = getSpillTempDsc(fieldNode);
                    GetEmitter()->emitIns_S(INS_push, emitActualTypeSize(fieldNode->TypeGet()), tmp->tdTempNum(), 0);
                    regSet.tmpRlsTemp(tmp);
                }
                else
                {
                    assert(varTypeIsIntegralOrI(fieldNode));
                    switch (fieldNode->GetOper())
                    {
                        case GT_LCL_VAR:
                            inst_TT(INS_push, fieldNode->AsLclVar());
                            break;
                        case GT_CNS_INT:
                            if (fieldNode->IsIconHandle())
                            {
                                GetEmitter()->emitIns_I(INS_push, EA_HANDLE_CNS_RELOC,
                                                        fieldNode->AsIntCon()->GetValue());
                            }
                            else
                            {
                                inst_IV(INS_push, fieldNode->AsIntCon()->gtIconVal);
                            }
                            break;
                        default:
                            unreached();
                    }
                }
                currentOffset -= TARGET_POINTER_SIZE;
                AddStackLevel(TARGET_POINTER_SIZE);
            }
            else
            {
                // The stack has been adjusted and we will load the field to intTmpReg and then store it on the stack.
                assert(varTypeIsIntegralOrI(fieldNode));
                switch (fieldNode->OperGet())
                {
                    case GT_LCL_VAR:
                        inst_RV_TT(INS_mov, emitTypeSize(fieldNode->GetType()), intTmpReg, fieldNode->AsLclVar());
                        break;
                    case GT_CNS_INT:
                        genSetRegToConst(intTmpReg, fieldNode->TypeGet(), fieldNode);
                        break;
                    default:
                        unreached();
                }

                if (pushStkArg)
                {
                    genPushReg(fieldType, intTmpReg);
                }
                else
                {
                    GetEmitter()->emitIns_AR_R(ins_Store(fieldType), emitTypeSize(fieldType), intTmpReg, REG_SPBASE,
                                               fieldOffset - currentOffset);
                }
            }
        }
        else
        {
#if defined(FEATURE_SIMD)
            if (fieldType == TYP_SIMD12)
            {
                assert(genIsValidFloatReg(simdTmpReg));
                genStoreSIMD12ToStack(argReg, simdTmpReg);
            }
            else
#endif // defined(FEATURE_SIMD)
                if (pushStkArg)
            {
                genPushReg(fieldType, argReg);
            }
            else
            {
                GetEmitter()->emitIns_AR_R(ins_Store(fieldType), emitTypeSize(fieldType), argReg, REG_SPBASE,
                                           fieldOffset - currentOffset);
            }
            if (pushStkArg)
            {
                // We always push a slot-rounded size
                currentOffset -= genTypeSize(fieldType);
            }
        }

        prevFieldOffset = fieldOffset;
    }
    if (currentOffset != 0)
    {
        // We don't expect padding at the beginning of a struct, but it could happen with explicit layout.
        inst_RV_IV(INS_sub, REG_SPBASE, currentOffset, EA_PTRSIZE);
        AddStackLevel(currentOffset);
    }
}
#endif // TARGET_X86

//---------------------------------------------------------------------
// genPutArgStk - Generate code for passing an arg on the stack.
//
// Arguments
//    putArgStk - the GT_PUTARG_STK node
//
void CodeGen::genPutArgStk(GenTreePutArgStk* putArgStk)
{
    GenTree*  src     = putArgStk->GetOp(0);
    var_types srcType = varActualType(src->GetType());

#if defined(TARGET_AMD64)
    unsigned outArgLclNum  = getBaseVarForPutArgStk(putArgStk);
    unsigned outArgLclOffs = putArgStk->GetSlotOffset();
#else
    // On a 32-bit target, all of the long arguments are handled with FIELD_LISTs of TYP_INT.
    assert(srcType != TYP_LONG);

    genAlignStackBeforeCall(putArgStk);
#endif

    if (src->OperIs(GT_FIELD_LIST))
    {
#if defined(TARGET_AMD64)
#if FEATURE_FASTTAILCALL
        // TODO-MIKE-Cleanup: This seems to be sligtly different from ARMARCH's outArgLclSize.
        INDEBUG(unsigned outArgLclSize = putArgStk->PutInIncomingArgArea()
                                             ? compiler->info.compArgStackSize + INIT_ARG_STACK_SLOT * REGSIZE_BYTES
                                             : compiler->lvaLclSize(outArgLclNum);)
#else
        INDEBUG(unsigned outArgLclSize = compiler->lvaLclSize(outArgLclNum);)
#endif
        genPutArgStkFieldList(putArgStk, outArgLclNum, outArgLclOffs DEBUGARG(outArgLclSize));
#else
        genPutArgStkFieldList(putArgStk);
#endif
        return;
    }

    if (srcType == TYP_STRUCT)
    {
#if defined(TARGET_AMD64)
        genPutStructArgStk(putArgStk, outArgLclNum, outArgLclOffs);
#else
        genPutStructArgStk(putArgStk);
#endif
        return;
    }

#if defined(TARGET_AMD64) || !defined(FEATURE_SIMD)
    assert(roundUp(varTypeSize(srcType), REGSIZE_BYTES) <= putArgStk->GetSlotCount() * REGSIZE_BYTES);
#else
    assert((roundUp(varTypeSize(srcType), REGSIZE_BYTES) <= putArgStk->GetArgSize()) || putArgStk->IsSIMD12());
#endif

    if (!src->isUsedFromReg())
    {
#if defined(TARGET_AMD64)
        GetEmitter()->emitIns_S_I(ins_Store(srcType), emitTypeSize(srcType), outArgLclNum,
                                  static_cast<int>(outArgLclOffs), static_cast<int>(src->AsIntCon()->GetValue()));
#else
        assert(putArgStk->GetSlotCount() == 1);

        emitAttr attr = emitActualTypeSize(src->GetType());

        assert(EA_SIZE_IN_BYTES(attr) == REGSIZE_BYTES);

        if (src->isUsedFromSpillTemp())
        {
            genConsumeRegs(src);
            TempDsc* tmp = getSpillTempDsc(src);
            GetEmitter()->emitIns_S(INS_push, attr, tmp->tdTempNum(), 0);
            regSet.tmpRlsTemp(tmp);
        }
        else if (src->OperIs(GT_LCL_VAR, GT_LCL_FLD))
        {
            genConsumeRegs(src);
            unsigned lclOffs = src->OperIs(GT_LCL_VAR) ? 0 : src->AsLclFld()->GetLclOffs();
            GetEmitter()->emitIns_S(INS_push, attr, src->AsLclVarCommon()->GetLclNum(), lclOffs);
        }
        else if (src->OperIs(GT_IND))
        {
            genConsumeRegs(src);
            GetEmitter()->emitIns_A(INS_push, attr, src->AsIndir());
        }
        else if (src->IsIconHandle())
        {
            GetEmitter()->emitIns_I(INS_push, EA_HANDLE_CNS_RELOC, src->AsIntCon()->GetValue());
        }
        else
        {
            GetEmitter()->emitIns_I(INS_push, EA_PTRSIZE, src->AsIntCon()->GetValue());
        }
        AddStackLevel(REGSIZE_BYTES);
#endif
        return;
    }

    regNumber srcReg = genConsumeReg(src);

#if defined(TARGET_AMD64)
    GetEmitter()->emitIns_S_R(ins_Store(srcType), emitTypeSize(srcType), srcReg, outArgLclNum,
                              static_cast<int>(outArgLclOffs));
#else

#if defined(FEATURE_SIMD)
    if (varTypeIsSIMD(srcType))
    {
        assert(genIsValidFloatReg(srcReg));

        inst_RV_IV(INS_sub, REG_SPBASE, putArgStk->GetArgSize(), EA_4BYTE);
        AddStackLevel(putArgStk->GetArgSize());

        if (putArgStk->IsSIMD12())
        {
            regNumber tmpReg = putArgStk->GetSingleTempReg();
            genStoreSIMD12ToStack(srcReg, tmpReg);
        }
        else
        {
            GetEmitter()->emitIns_AR_R(ins_Store(srcType), emitTypeSize(srcType), srcReg, REG_SPBASE, 0);
        }

        return;
    }
#endif

    genPushReg(srcType, srcReg);
#endif
}

void CodeGen::genPutArgReg(GenTreeUnOp* putArg)
{
    assert(putArg->OperIs(GT_PUTARG_REG));

    GenTree*  src    = putArg->GetOp(0);
    regNumber srcReg = genConsumeReg(src);
    var_types type   = putArg->GetType();
    regNumber argReg = putArg->GetRegNum();

    assert(!varTypeIsSmall(type));
#ifdef TARGET_X86
    assert(type != TYP_LONG);
#endif

    inst_Mov(type, argReg, srcReg, /* canSkip */ true);

    genProduceReg(putArg);
}

#ifdef TARGET_X86
// genPushReg: Push a register value onto the stack and adjust the stack level
//
// Arguments:
//    type   - the type of value to be stored
//    reg    - the register containing the value
//
void CodeGen::genPushReg(var_types type, regNumber srcReg)
{
    assert(!varTypeIsLong(type));

    unsigned size = varTypeSize(type);

    if (varTypeIsIntegralOrI(type))
    {
        assert(genIsValidIntReg(srcReg));
        inst_RV(INS_push, srcReg, type);
    }
    else
    {
        assert(genIsValidFloatReg(srcReg));
        inst_RV_IV(INS_sub, REG_SPBASE, size, EA_4BYTE);
        GetEmitter()->emitIns_AR_R(ins_Store(type), emitTypeSize(type), srcReg, REG_SPBASE, 0);
    }

    AddStackLevel(size);
}
#endif // TARGET_X86

//---------------------------------------------------------------------
// genPutStructArgStk - Generate code for copying a struct arg on the stack by value.
//
// Arguments
//    putArgStk - the GT_PUTARG_STK node
//
void CodeGen::genPutStructArgStk(GenTreePutArgStk* putArgStk NOT_X86_ARG(unsigned outArgLclNum)
                                     NOT_X86_ARG(unsigned outArgLclOffs))
{
    GenTree* src = putArgStk->GetOp(0);

    assert(src->TypeIs(TYP_STRUCT));
    assert(src->isContained());

    ClassLayout* srcLayout;
    unsigned     srcLclNum         = BAD_VAR_NUM;
    regNumber    srcAddrBaseReg    = REG_NA;
    regNumber    srcAddrIndexReg   = REG_NA;
    unsigned     srcAddrIndexScale = 1;
    emitAttr     srcAddrAttr       = EA_PTRSIZE;
    int          srcOffset         = 0;

    if (src->OperIs(GT_LCL_VAR))
    {
        srcLclNum = src->AsLclVar()->GetLclNum();
        srcLayout = compiler->lvaGetDesc(srcLclNum)->GetLayout();
    }
    else if (src->OperIs(GT_LCL_FLD))
    {
        srcLclNum = src->AsLclFld()->GetLclNum();
        srcOffset = src->AsLclFld()->GetLclOffs();
        srcLayout = src->AsLclFld()->GetLayout(compiler);
    }
    else
    {
        GenTree* srcAddr = src->AsObj()->GetAddr();

        if (!srcAddr->isContained())
        {
            srcAddrBaseReg = genConsumeReg(srcAddr);
        }
        else
        {
            GenTreeAddrMode* addrMode = srcAddr->AsAddrMode();

            if (addrMode->HasBase())
            {
                srcAddrBaseReg = genConsumeReg(addrMode->GetBase());
            }

            if (addrMode->HasIndex())
            {
                srcAddrIndexReg   = genConsumeReg(addrMode->GetIndex());
                srcAddrIndexScale = addrMode->GetScale();
            }

            srcOffset = addrMode->Offset();
        }

        srcLayout   = src->AsObj()->GetLayout();
        srcAddrAttr = emitTypeSize(srcAddr->GetType());
    }

    if (putArgStk->gtPutArgStkKind == GenTreePutArgStk::Kind::Unroll)
    {
        assert(!srcLayout->HasGCPtr());

        unsigned size = srcLayout->GetSize();

        if (srcLclNum != BAD_VAR_NUM)
        {
            size = roundUp(size, REGSIZE_BYTES);
        }

        regNumber xmmTmpReg = REG_NA;
        regNumber intTmpReg = REG_NA;
#ifdef TARGET_X86
        // On x86 we use an XMM register for both 16 and 8-byte chunks.
        if (size >= (XMM_REGSIZE_BYTES / 2))
        {
            xmmTmpReg = putArgStk->GetSingleTempReg(RBM_ALLFLOAT);
        }

        if ((size % (XMM_REGSIZE_BYTES / 2)) != 0)
        {
            intTmpReg = putArgStk->GetSingleTempReg(RBM_ALLINT);
        }

        if ((size == 1) || (size == 2) || (size == 4) || (size == 12))
        {
            // Use a push (and a movq) if we have a 4 byte reminder, it's smaller
            // than the normal unroll code generated below.

            if ((size == 1) || (size == 2))
            {
                if (srcLclNum != BAD_VAR_NUM)
                {
                    GetEmitter()->emitIns_R_S(INS_movzx, EA_ATTR(size), intTmpReg, srcLclNum, srcOffset);
                }
                else
                {
                    GetEmitter()->emitIns_R_ARX(INS_movzx, EA_ATTR(size), intTmpReg, srcAddrBaseReg, srcAddrIndexReg,
                                                srcAddrIndexScale, srcOffset);
                }

                GetEmitter()->emitIns_R(INS_push, EA_4BYTE, intTmpReg);
            }
            else if ((size == 4) || (size == 12))
            {
                if (srcLclNum != BAD_VAR_NUM)
                {
                    GetEmitter()->emitIns_S(INS_push, EA_4BYTE, srcLclNum, srcOffset + (size & 8));
                }
                else
                {
                    GetEmitter()->emitIns_ARX(INS_push, EA_4BYTE, srcAddrBaseReg, srcAddrIndexReg, srcAddrIndexScale,
                                              srcOffset + (size & 8));
                }
            }

            AddStackLevel(4);

            if (size == 12)
            {
                if (srcLclNum != BAD_VAR_NUM)
                {
                    GetEmitter()->emitIns_R_S(INS_movq, EA_8BYTE, xmmTmpReg, srcLclNum, srcOffset);
                }
                else
                {
                    GetEmitter()->emitIns_R_ARX(INS_movq, EA_8BYTE, xmmTmpReg, srcAddrBaseReg, srcAddrIndexReg,
                                                srcAddrIndexScale, srcOffset);
                }

                GetEmitter()->emitIns_R_I(INS_sub, EA_4BYTE, REG_SPBASE, 8);
                GetEmitter()->emitIns_AR_R(INS_movq, EA_8BYTE, xmmTmpReg, REG_SPBASE, 0);
                AddStackLevel(8);
            }

            return;
        }

        genPreAdjustStackForPutArgStk(putArgStk->GetArgSize());

#else  // !TARGET_X86
        // On x64 we use an XMM register only for 16-byte chunks.
        if (size >= XMM_REGSIZE_BYTES)
        {
            xmmTmpReg = putArgStk->GetSingleTempReg(RBM_ALLFLOAT);
        }

        if ((size % XMM_REGSIZE_BYTES) != 0)
        {
            intTmpReg = putArgStk->GetSingleTempReg(RBM_ALLINT);
        }
#endif // !TARGET_X86

        for (unsigned regSize = XMM_REGSIZE_BYTES, offset = 0; size != 0; size -= regSize, offset += regSize)
        {
            while (regSize > size)
            {
                regSize /= 2;
            }

            instruction ins    = INS_mov;
            regNumber   tmpReg = intTmpReg;

            if (regSize == 16)
            {
                ins    = INS_movdqu;
                tmpReg = xmmTmpReg;
            }
#ifdef TARGET_X86
            else if (regSize == 8)
            {
                ins    = INS_movq;
                tmpReg = xmmTmpReg;
            }
#endif

            if (srcLclNum != BAD_VAR_NUM)
            {
                GetEmitter()->emitIns_R_S(ins, EA_ATTR(regSize), tmpReg, srcLclNum, srcOffset + offset);
            }
            else
            {
                GetEmitter()->emitIns_R_ARX(ins, EA_ATTR(regSize), tmpReg, srcAddrBaseReg, srcAddrIndexReg,
                                            srcAddrIndexScale, srcOffset + offset);
            }

#ifdef TARGET_X86
            GetEmitter()->emitIns_AR_R(ins, EA_ATTR(regSize), tmpReg, REG_SPBASE, offset);
#else
            GetEmitter()->emitIns_S_R(ins, EA_ATTR(regSize), tmpReg, outArgLclNum, outArgLclOffs + offset);
#endif
        }
        return;
    }

#ifdef TARGET_X86
    if (putArgStk->gtPutArgStkKind == GenTreePutArgStk::Kind::Push)
    {
        // On x86, any struct that has contains GC references must be stored to the stack using `push` instructions so
        // that the emitter properly detects the need to update the method's GC information.
        //
        // Strictly speaking, it is only necessary to use `push` to store the GC references themselves, so for structs
        // with large numbers of consecutive non-GC-ref-typed fields, we may be able to improve the code size in the
        // future.

        // We assume that the size of a struct which contains GC pointers is a multiple of the slot size.
        assert(srcLayout->GetSize() % REGSIZE_BYTES == 0);

        for (int i = putArgStk->GetSlotCount() - 1; i >= 0; --i)
        {
            emitAttr slotAttr      = emitTypeSize(srcLayout->GetGCPtrType(i));
            int      slotSrcOffset = srcOffset + i * REGSIZE_BYTES;

            if (srcLclNum != BAD_VAR_NUM)
            {
                GetEmitter()->emitIns_S(INS_push, slotAttr, srcLclNum, slotSrcOffset);
            }
            else
            {
                GetEmitter()->emitIns_ARX(INS_push, slotAttr, srcAddrBaseReg, srcAddrIndexReg, srcAddrIndexScale,
                                          slotSrcOffset);
            }

            AddStackLevel(REGSIZE_BYTES);
        }

        return;
    }

    assert(putArgStk->gtPutArgStkKind == GenTreePutArgStk::Kind::RepInstr);
    assert((putArgStk->gtRsvdRegs & (RBM_RSI | RBM_RDI | RBM_RCX)) == (RBM_RSI | RBM_RDI | RBM_RCX));

    genPreAdjustStackForPutArgStk(putArgStk->GetArgSize());
    GetEmitter()->emitIns_Mov(INS_mov, EA_PTRSIZE, REG_RDI, REG_SPBASE, /* canSkip */ false);

    if (srcLclNum != BAD_VAR_NUM)
    {
        GetEmitter()->emitIns_R_S(INS_lea, EA_PTRSIZE, REG_RSI, srcLclNum, srcOffset);
    }
    else if ((srcAddrIndexReg != REG_NA) || (srcOffset != 0))
    {
        GetEmitter()->emitIns_R_ARX(INS_lea, srcAddrAttr, REG_RSI, srcAddrBaseReg, srcAddrIndexReg, srcAddrIndexScale,
                                    srcOffset);
    }
    else
    {
        GetEmitter()->emitIns_Mov(INS_mov, srcAddrAttr, REG_RSI, srcAddrBaseReg, /* canSkip */ false);
    }

    assert(!srcLayout->HasGCPtr());

    GetEmitter()->emitIns_R_I(INS_mov, EA_4BYTE, REG_RCX, srcLayout->GetSize());
    GetEmitter()->emitIns(INS_r_movsb);
#else
    regNumber intTmpReg = REG_NA;
    regNumber xmmTmpReg = REG_NA;

    if ((putArgStk->gtPutArgStkKind == GenTreePutArgStk::Kind::RepInstr) ||
        (putArgStk->gtPutArgStkKind == GenTreePutArgStk::Kind::RepInstrXMM))
    {
        assert((putArgStk->gtRsvdRegs & (RBM_RSI | RBM_RDI | RBM_RCX)) == (RBM_RSI | RBM_RDI | RBM_RCX));

        GetEmitter()->emitIns_R_S(INS_lea, EA_PTRSIZE, REG_RDI, outArgLclNum, outArgLclOffs);

        if (srcLclNum != BAD_VAR_NUM)
        {
            GetEmitter()->emitIns_R_S(INS_lea, EA_PTRSIZE, REG_RSI, srcLclNum, srcOffset);
        }
        else if ((srcAddrIndexReg != REG_NA) || (srcOffset != 0))
        {
            GetEmitter()->emitIns_R_ARX(INS_lea, srcAddrAttr, REG_RSI, srcAddrBaseReg, srcAddrIndexReg,
                                        srcAddrIndexScale, srcOffset);
        }
        else
        {
            GetEmitter()->emitIns_Mov(INS_mov, srcAddrAttr, REG_RSI, srcAddrBaseReg, /* canSkip */ false);
        }

        if (!srcLayout->HasGCPtr())
        {
            assert(putArgStk->gtPutArgStkKind == GenTreePutArgStk::Kind::RepInstr);
            GetEmitter()->emitIns_R_I(INS_mov, EA_4BYTE, REG_RCX, srcLayout->GetSize());
            GetEmitter()->emitIns(INS_r_movsb);
            return;
        }

        srcLclNum         = BAD_VAR_NUM;
        srcAddrBaseReg    = REG_RSI;
        srcAddrIndexReg   = REG_NA;
        srcAddrIndexScale = 1;
        srcOffset         = 0;

        intTmpReg = REG_RCX;
    }
    else
    {
        assert((putArgStk->gtPutArgStkKind == GenTreePutArgStk::Kind::GCUnroll) ||
               (putArgStk->gtPutArgStkKind == GenTreePutArgStk::Kind::GCUnrollXMM));

        intTmpReg = putArgStk->GetSingleTempReg(RBM_ALLINT);
    }

    if ((putArgStk->gtPutArgStkKind == GenTreePutArgStk::Kind::RepInstrXMM) ||
        (putArgStk->gtPutArgStkKind == GenTreePutArgStk::Kind::GCUnrollXMM))
    {
        xmmTmpReg = putArgStk->GetSingleTempReg(RBM_ALLFLOAT);
    }

    // We assume that the size of a struct which contains GC pointers is a multiple of the slot size.
    assert(srcLayout->GetSize() % REGSIZE_BYTES == 0);

    unsigned numSlots = putArgStk->GetSlotCount();

    for (unsigned i = 0; i < numSlots; i++)
    {
        // Let's see if we can use rep movsp (alias for movsd or movsq for 32 and 64 bits respectively)
        // instead of a sequence of movsp instructions to save cycles and code size.
        unsigned nonGCSequenceLength = 0;
        while ((i + nonGCSequenceLength < numSlots) && !srcLayout->IsGCPtr(i + nonGCSequenceLength))
        {
            nonGCSequenceLength++;
        }

        if (nonGCSequenceLength <= 1)
        {
            // TODO-AMD64-Unix: Here a better solution (for code size) would be to use movsp instruction,
            // but the logic for emitting a GC info record is not available (it is internal for the emitter
            // only). See emitGCVarLiveUpd function. If we could call it separately, we could do
            // instGen(INS_movsp); and emission of gc info.

            emitAttr slotAttr = emitTypeSize(srcLayout->GetGCPtrType(i));

            if (srcLclNum != BAD_VAR_NUM)
            {
                GetEmitter()->emitIns_R_S(INS_mov, slotAttr, intTmpReg, srcLclNum, srcOffset);
            }
            else
            {
                GetEmitter()->emitIns_R_ARX(INS_mov, slotAttr, intTmpReg, srcAddrBaseReg, srcAddrIndexReg,
                                            srcAddrIndexScale, srcOffset);
            }
            GetEmitter()->emitIns_S_R(INS_mov, slotAttr, intTmpReg, outArgLclNum, outArgLclOffs + i * REGSIZE_BYTES);
            srcOffset += REGSIZE_BYTES;
            continue;
        }

        if (nonGCSequenceLength == 2)
        {
            assert(xmmTmpReg != REG_NA);

            if (srcLclNum != BAD_VAR_NUM)
            {
                GetEmitter()->emitIns_R_S(INS_movdqu, EA_16BYTE, xmmTmpReg, srcLclNum, srcOffset);
            }
            else
            {
                GetEmitter()->emitIns_R_ARX(INS_movdqu, EA_16BYTE, xmmTmpReg, srcAddrBaseReg, srcAddrIndexReg,
                                            srcAddrIndexScale, srcOffset);
            }
            GetEmitter()->emitIns_S_R(INS_movdqu, EA_16BYTE, xmmTmpReg, outArgLclNum,
                                      outArgLclOffs + i * REGSIZE_BYTES);
            srcOffset += 2 * REGSIZE_BYTES;
            i++;
            continue;
        }

        assert((putArgStk->gtPutArgStkKind == GenTreePutArgStk::Kind::RepInstr) ||
               (putArgStk->gtPutArgStkKind == GenTreePutArgStk::Kind::RepInstrXMM));
        assert(srcAddrBaseReg == REG_RSI);
        assert(srcAddrIndexReg == REG_NA);

        if (srcOffset != 0)
        {
            GetEmitter()->emitIns_R_I(INS_add, srcAddrAttr, REG_RSI, srcOffset);
            GetEmitter()->emitIns_R_I(INS_add, EA_PTRSIZE, REG_RDI, srcOffset);
            srcOffset = 0;
        }

        if (nonGCSequenceLength < CPOBJ_NONGC_SLOTS_LIMIT)
        {
            for (unsigned j = 0; j < nonGCSequenceLength; j++)
            {
                instGen(INS_movsp);
            }
        }
        else
        {
            GetEmitter()->emitIns_R_I(INS_mov, EA_4BYTE, REG_RCX, nonGCSequenceLength);
            instGen(INS_r_movsp);
        }

        i += nonGCSequenceLength - 1;
    }
#endif // TARGET_AMD64
}

/*****************************************************************************
 *
 *  Create and record GC Info for the function.
 */
#ifndef JIT32_GCENCODER
void
#else  // !JIT32_GCENCODER
void*
#endif // !JIT32_GCENCODER
CodeGen::genCreateAndStoreGCInfo(unsigned codeSize, unsigned prologSize, unsigned epilogSize DEBUGARG(void* codePtr))
{
#ifdef JIT32_GCENCODER
    return genCreateAndStoreGCInfoJIT32(codeSize, prologSize, epilogSize DEBUGARG(codePtr));
#else  // !JIT32_GCENCODER
    genCreateAndStoreGCInfoX64(codeSize, prologSize DEBUGARG(codePtr));
#endif // !JIT32_GCENCODER
}

#ifdef JIT32_GCENCODER
void* CodeGen::genCreateAndStoreGCInfoJIT32(unsigned codeSize,
                                            unsigned prologSize,
                                            unsigned epilogSize DEBUGARG(void* codePtr))
{
    BYTE    headerBuf[64];
    InfoHdr header;

    int s_cached;

#ifdef FEATURE_EH_FUNCLETS
    // We should do this before gcInfoBlockHdrSave since varPtrTableSize must be finalized before it
    if (compiler->ehAnyFunclets())
    {
        gcInfo.gcMarkFilterVarsPinned();
    }
#endif

#ifdef DEBUG
    size_t headerSize =
#endif
        compiler->compInfoBlkSize =
            gcInfo.gcInfoBlockHdrSave(headerBuf, 0, codeSize, prologSize, epilogSize, &header, &s_cached);

    size_t argTabOffset = 0;
    size_t ptrMapSize   = gcInfo.gcPtrTableSize(header, codeSize, &argTabOffset);

#if DISPLAY_SIZES

    if (GetInterruptible())
    {
        gcHeaderISize += compiler->compInfoBlkSize;
        gcPtrMapISize += ptrMapSize;
    }
    else
    {
        gcHeaderNSize += compiler->compInfoBlkSize;
        gcPtrMapNSize += ptrMapSize;
    }

#endif // DISPLAY_SIZES

    compiler->compInfoBlkSize += ptrMapSize;

    /* Allocate the info block for the method */

    compiler->compInfoBlkAddr = (BYTE*)compiler->info.compCompHnd->allocGCInfo(compiler->compInfoBlkSize);

#if 0 // VERBOSE_SIZES
    // TODO-X86-Cleanup: 'dataSize', below, is not defined

//  if  (compiler->compInfoBlkSize > codeSize && compiler->compInfoBlkSize > 100)
    {
        printf("[%7u VM, %7u+%7u/%7u x86 %03u/%03u%%] %s.%s\n",
               compiler->info.compILCodeSize,
               compiler->compInfoBlkSize,
               codeSize + dataSize,
               codeSize + dataSize - prologSize - epilogSize,
               100 * (codeSize + dataSize) / compiler->info.compILCodeSize,
               100 * (codeSize + dataSize + compiler->compInfoBlkSize) / compiler->info.compILCodeSize,
               compiler->info.compClassName,
               compiler->info.compMethodName);
}

#endif

    /* Fill in the info block and return it to the caller */

    void* infoPtr = compiler->compInfoBlkAddr;

    /* Create the method info block: header followed by GC tracking tables */

    compiler->compInfoBlkAddr +=
        gcInfo.gcInfoBlockHdrSave(compiler->compInfoBlkAddr, -1, codeSize, prologSize, epilogSize, &header, &s_cached);

    assert(compiler->compInfoBlkAddr == (BYTE*)infoPtr + headerSize);
    compiler->compInfoBlkAddr = gcInfo.gcPtrTableSave(compiler->compInfoBlkAddr, header, codeSize, &argTabOffset);
    assert(compiler->compInfoBlkAddr == (BYTE*)infoPtr + headerSize + ptrMapSize);

#ifdef DEBUG

    if (0)
    {
        BYTE*  temp = (BYTE*)infoPtr;
        size_t size = compiler->compInfoBlkAddr - temp;
        BYTE*  ptab = temp + headerSize;

        noway_assert(size == headerSize + ptrMapSize);

        printf("Method info block - header [%zu bytes]:", headerSize);

        for (unsigned i = 0; i < size; i++)
        {
            if (temp == ptab)
            {
                printf("\nMethod info block - ptrtab [%u bytes]:", ptrMapSize);
                printf("\n    %04X: %*c", i & ~0xF, 3 * (i & 0xF), ' ');
            }
            else
            {
                if (!(i % 16))
                    printf("\n    %04X: ", i);
            }

            printf("%02X ", *temp++);
        }

        printf("\n");
    }

#endif // DEBUG

#if DUMP_GC_TABLES

    if (compiler->opts.dspGCtbls)
    {
        const BYTE* base = (BYTE*)infoPtr;
        size_t      size;
        unsigned    methodSize;
        InfoHdr     dumpHeader;

        printf("GC Info for method %s\n", compiler->info.compFullName);
        printf("GC info size = %3u\n", compiler->compInfoBlkSize);

        size = gcInfo.gcInfoBlockHdrDump(base, &dumpHeader, &methodSize);
        // printf("size of header encoding is %3u\n", size);
        printf("\n");

        if (compiler->opts.dspGCtbls)
        {
            base += size;
            size = gcInfo.gcDumpPtrTable(base, dumpHeader, methodSize);
            // printf("size of pointer table is %3u\n", size);
            printf("\n");
            noway_assert(compiler->compInfoBlkAddr == (base + size));
        }
    }

#endif // DUMP_GC_TABLES

    /* Make sure we ended up generating the expected number of bytes */

    noway_assert(compiler->compInfoBlkAddr == (BYTE*)infoPtr + compiler->compInfoBlkSize);

    return infoPtr;
}

#else  // !JIT32_GCENCODER
void CodeGen::genCreateAndStoreGCInfoX64(unsigned codeSize, unsigned prologSize DEBUGARG(void* codePtr))
{
    IAllocator*    allowZeroAlloc = new (compiler, CMK_GC) CompIAllocator(compiler->getAllocatorGC());
    GcInfoEncoder* gcInfoEncoder  = new (compiler, CMK_GC)
        GcInfoEncoder(compiler->info.compCompHnd, compiler->info.compMethodInfo, allowZeroAlloc, NOMEM);
    assert(gcInfoEncoder);

    // Follow the code pattern of the x86 gc info encoder (genCreateAndStoreGCInfoJIT32).
    gcInfo.gcInfoBlockHdrSave(gcInfoEncoder, codeSize, prologSize);

    // We keep the call count for the second call to gcMakeRegPtrTable() below.
    unsigned callCnt = 0;
    // First we figure out the encoder ID's for the stack slots and registers.
    gcInfo.gcMakeRegPtrTable(gcInfoEncoder, codeSize, prologSize, GCInfo::MAKE_REG_PTR_MODE_ASSIGN_SLOTS, &callCnt);
    // Now we've requested all the slots we'll need; "finalize" these (make more compact data structures for them).
    gcInfoEncoder->FinalizeSlotIds();
    // Now we can actually use those slot ID's to declare live ranges.
    gcInfo.gcMakeRegPtrTable(gcInfoEncoder, codeSize, prologSize, GCInfo::MAKE_REG_PTR_MODE_DO_WORK, &callCnt);

    if (compiler->opts.compDbgEnC)
    {
        // what we have to preserve is called the "frame header" (see comments in VM\eetwain.cpp)
        // which is:
        //  -return address
        //  -saved off RBP
        //  -saved 'this' pointer and bool for synchronized methods

        // 4 slots for RBP + return address + RSI + RDI
        int preservedAreaSize = 4 * REGSIZE_BYTES;

        if (compiler->info.compFlags & CORINFO_FLG_SYNCH)
        {
            if (!(compiler->info.compFlags & CORINFO_FLG_STATIC))
            {
                preservedAreaSize += REGSIZE_BYTES;
            }

            // bool in synchronized methods that tracks whether the lock has been taken (takes 4 bytes on stack)
            preservedAreaSize += 4;
        }

        // Used to signal both that the method is compiled for EnC, and also the size of the block at the top of the
        // frame
        gcInfoEncoder->SetSizeOfEditAndContinuePreservedArea(preservedAreaSize);
    }

    if (compiler->opts.IsReversePInvoke())
    {
        LclVarDsc* reversePInvokeFrameLcl = compiler->lvaGetDesc(compiler->lvaReversePInvokeFrameVar);
        gcInfoEncoder->SetReversePInvokeFrameSlot(reversePInvokeFrameLcl->GetStackOffset());
    }

    gcInfoEncoder->Build();

    // GC Encoder automatically puts the GC info in the right spot using ICorJitInfo::allocGCInfo(size_t)
    // let's save the values anyway for debugging purposes
    compiler->compInfoBlkAddr = gcInfoEncoder->Emit();
    compiler->compInfoBlkSize = 0; // not exposed by the GCEncoder interface
}
#endif // !JIT32_GCENCODER

/*****************************************************************************
 *  Emit a call to a helper function.
 *
 */

void CodeGen::genEmitHelperCall(unsigned helper, int argSize, emitAttr retSize, regNumber callTargetReg)
{
    void* addr  = nullptr;
    void* pAddr = nullptr;

    emitter::EmitCallType callType = emitter::EC_FUNC_TOKEN;
    addr                           = compiler->compGetHelperFtn((CorInfoHelpFunc)helper, &pAddr);
    regNumber callTarget           = REG_NA;
    regMaskTP killMask             = compiler->compHelperCallKillSet((CorInfoHelpFunc)helper);

    if (!addr)
    {
        assert(pAddr != nullptr);

        // Absolute indirect call addr
        // Note: Order of checks is important. First always check for pc-relative and next
        // zero-relative.  Because the former encoding is 1-byte smaller than the latter.
        if (genCodeIndirAddrCanBeEncodedAsPCRelOffset((size_t)pAddr) ||
            genCodeIndirAddrCanBeEncodedAsZeroRelOffset((size_t)pAddr))
        {
            // generate call whose target is specified by 32-bit offset relative to PC or zero.
            callType = emitter::EC_FUNC_TOKEN_INDIR;
            addr     = pAddr;
        }
        else
        {
#ifdef TARGET_AMD64
            // If this indirect address cannot be encoded as 32-bit offset relative to PC or Zero,
            // load it into REG_HELPER_CALL_TARGET and use register indirect addressing mode to
            // make the call.
            //    mov   reg, addr
            //    call  [reg]

            if (callTargetReg == REG_NA)
            {
                // If a callTargetReg has not been explicitly provided, we will use REG_DEFAULT_HELPER_CALL_TARGET, but
                // this is only a valid assumption if the helper call is known to kill REG_DEFAULT_HELPER_CALL_TARGET.
                callTargetReg            = REG_DEFAULT_HELPER_CALL_TARGET;
                regMaskTP callTargetMask = genRegMask(callTargetReg);
                noway_assert((callTargetMask & killMask) == callTargetMask);
            }
            else
            {
                // The call target must not overwrite any live variable, though it may not be in the
                // kill set for the call.
                regMaskTP callTargetMask = genRegMask(callTargetReg);
                noway_assert((callTargetMask & regSet.GetMaskVars()) == RBM_NONE);
            }
#endif

            callTarget = callTargetReg;
            CodeGen::genSetRegToIcon(callTarget, (ssize_t)pAddr, TYP_I_IMPL);
            callType = emitter::EC_INDIR_ARD;
        }
    }

    // clang-format off
    GetEmitter()->emitIns_Call(callType,
                               compiler->eeFindHelper(helper),
                               INDEBUG_LDISASM_COMMA(nullptr) addr,
                               argSize,
                               retSize
                               MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(EA_UNKNOWN),
                               gcInfo.gcVarPtrSetCur,
                               gcInfo.gcRegGCrefSetCur,
                               gcInfo.gcRegByrefSetCur,
                               BAD_IL_OFFSET, // IL offset
                               callTarget,    // ireg
                               REG_NA, 0, 0,  // xreg, xmul, disp
                               false         // isJump
                               );
    // clang-format on

    regSet.verifyRegistersUsed(killMask);
}

/*****************************************************************************
* Unit testing of the XArch emitter: generate a bunch of instructions into the prolog
* (it's as good a place as any), then use COMPlus_JitLateDisasm=* to see if the late
* disassembler thinks the instructions as the same as we do.
*/

// Uncomment "#define ALL_ARM64_EMITTER_UNIT_TESTS" to run all the unit tests here.
// After adding a unit test, and verifying it works, put it under this #ifdef, so we don't see it run every time.
//#define ALL_XARCH_EMITTER_UNIT_TESTS

#if defined(DEBUG) && defined(LATE_DISASM) && defined(TARGET_AMD64)
void CodeGen::genAmd64EmitterUnitTests()
{
    if (!verbose)
    {
        return;
    }

    if (!compiler->opts.altJit)
    {
        // No point doing this in a "real" JIT.
        return;
    }

    // Mark the "fake" instructions in the output.
    printf("*************** In genAmd64EmitterUnitTests()\n");

    // We use this:
    //      genDefineTempLabel(genCreateTempLabel());
    // to create artificial labels to help separate groups of tests.

    //
    // Loads
    //
    CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef ALL_XARCH_EMITTER_UNIT_TESTS
    genDefineTempLabel(genCreateTempLabel());

    // vhaddpd     ymm0,ymm1,ymm2
    GetEmitter()->emitIns_R_R_R(INS_haddpd, EA_32BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vaddss      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_addss, EA_4BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vaddsd      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_addsd, EA_8BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vaddps      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_addps, EA_16BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vaddps      ymm0,ymm1,ymm2
    GetEmitter()->emitIns_R_R_R(INS_addps, EA_32BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vaddpd      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_addpd, EA_16BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vaddpd      ymm0,ymm1,ymm2
    GetEmitter()->emitIns_R_R_R(INS_addpd, EA_32BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vsubss      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_subss, EA_4BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vsubsd      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_subsd, EA_8BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vsubps      ymm0,ymm1,ymm2
    GetEmitter()->emitIns_R_R_R(INS_subps, EA_16BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vsubps      ymm0,ymm1,ymm2
    GetEmitter()->emitIns_R_R_R(INS_subps, EA_32BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vsubpd      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_subpd, EA_16BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vsubpd      ymm0,ymm1,ymm2
    GetEmitter()->emitIns_R_R_R(INS_subpd, EA_32BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vmulss      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_mulss, EA_4BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vmulsd      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_mulsd, EA_8BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vmulps      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_mulps, EA_16BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vmulpd      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_mulpd, EA_16BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vmulps      ymm0,ymm1,ymm2
    GetEmitter()->emitIns_R_R_R(INS_mulps, EA_32BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vmulpd      ymm0,ymm1,ymm2
    GetEmitter()->emitIns_R_R_R(INS_mulpd, EA_32BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vandps      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_andps, EA_16BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vandpd      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_andpd, EA_16BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vandps      ymm0,ymm1,ymm2
    GetEmitter()->emitIns_R_R_R(INS_andps, EA_32BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vandpd      ymm0,ymm1,ymm2
    GetEmitter()->emitIns_R_R_R(INS_andpd, EA_32BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vorps      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_orps, EA_16BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vorpd      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_orpd, EA_16BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vorps      ymm0,ymm1,ymm2
    GetEmitter()->emitIns_R_R_R(INS_orps, EA_32BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vorpd      ymm0,ymm1,ymm2
    GetEmitter()->emitIns_R_R_R(INS_orpd, EA_32BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vdivss      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_divss, EA_4BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vdivsd      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_divsd, EA_8BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vdivss      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_divss, EA_4BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vdivsd      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_divsd, EA_8BYTE, REG_XMM0, REG_XMM1, REG_XMM2);

    // vdivss      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_cvtss2sd, EA_4BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
    // vdivsd      xmm0,xmm1,xmm2
    GetEmitter()->emitIns_R_R_R(INS_cvtsd2ss, EA_8BYTE, REG_XMM0, REG_XMM1, REG_XMM2);
#endif // ALL_XARCH_EMITTER_UNIT_TESTS
    printf("*************** End of genAmd64EmitterUnitTests()\n");
}

#endif // defined(DEBUG) && defined(LATE_DISASM) && defined(TARGET_AMD64)

#ifdef PROFILING_SUPPORTED

#ifdef TARGET_X86

//-----------------------------------------------------------------------------------
// genProfilingEnterCallback: Generate the profiling function enter callback.
//
// Arguments:
//     initReg        - register to use as scratch register
//     pInitRegZeroed - OUT parameter. This variable remains unchanged.
//
// Return Value:
//     None
//
// Notes:
// The x86 profile enter helper has the following requirements (see ProfileEnterNaked in
// VM\i386\asmhelpers.asm for details):
// 1. The calling sequence for calling the helper is:
//          push FunctionIDOrClientID
//          call ProfileEnterHelper
// 2. The calling function has an EBP frame.
// 3. EBP points to the saved ESP which is the first thing saved in the function. Thus,
//    the following prolog is assumed:
//          push ESP
//          mov EBP, ESP
// 4. All registers are preserved.
// 5. The helper pops the FunctionIDOrClientID argument from the stack.
//
void CodeGen::genProfilingEnterCallback(regNumber initReg, bool* pInitRegZeroed)
{
    assert(compiler->compGeneratingProlog);

    // Give profiler a chance to back out of hooking this method
    if (!compiler->compIsProfilerHookNeeded())
    {
        return;
    }

    unsigned saveStackLvl2 = genStackLevel;

// Important note: when you change enter probe layout, you must also update SKIP_ENTER_PROF_CALLBACK()
// for x86 stack unwinding

#if defined(UNIX_X86_ABI)
    // Manually align the stack to be 16-byte aligned. This is similar to CodeGen::genAlignStackBeforeCall()
    GetEmitter()->emitIns_R_I(INS_sub, EA_4BYTE, REG_SPBASE, 0xC);
#endif // UNIX_X86_ABI

    // Push the profilerHandle
    if (compiler->compProfilerMethHndIndirected)
    {
        GetEmitter()->emitIns_AR_R(INS_push, EA_PTR_DSP_RELOC, REG_NA, REG_NA, (ssize_t)compiler->compProfilerMethHnd);
    }
    else
    {
        inst_IV(INS_push, (size_t)compiler->compProfilerMethHnd);
    }

    // This will emit either
    // "call ip-relative 32-bit offset" or
    // "mov rax, helper addr; call rax"
    genEmitHelperCall(CORINFO_HELP_PROF_FCN_ENTER,
                      0,           // argSize. Again, we have to lie about it
                      EA_UNKNOWN); // retSize

    // Check that we have place for the push.
    assert(compiler->fgGetPtrArgCntMax() >= 1);

#if defined(UNIX_X86_ABI)
    // Restoring alignment manually. This is similar to CodeGen::genRemoveAlignmentAfterCall
    GetEmitter()->emitIns_R_I(INS_add, EA_4BYTE, REG_SPBASE, 0x10);
#endif // UNIX_X86_ABI

    /* Restore the stack level */

    SetStackLevel(saveStackLvl2);
}

//-----------------------------------------------------------------------------------
// genProfilingLeaveCallback: Generate the profiling function leave or tailcall callback.
// Technically, this is not part of the epilog; it is called when we are generating code for a GT_RETURN node.
//
// Arguments:
//     helper - which helper to call. Either CORINFO_HELP_PROF_FCN_LEAVE or CORINFO_HELP_PROF_FCN_TAILCALL
//
// Return Value:
//     None
//
// Notes:
// The x86 profile leave/tailcall helper has the following requirements (see ProfileLeaveNaked and
// ProfileTailcallNaked in VM\i386\asmhelpers.asm for details):
// 1. The calling sequence for calling the helper is:
//          push FunctionIDOrClientID
//          call ProfileLeaveHelper or ProfileTailcallHelper
// 2. The calling function has an EBP frame.
// 3. EBP points to the saved ESP which is the first thing saved in the function. Thus,
//    the following prolog is assumed:
//          push ESP
//          mov EBP, ESP
// 4. helper == CORINFO_HELP_PROF_FCN_LEAVE: All registers are preserved.
//    helper == CORINFO_HELP_PROF_FCN_TAILCALL: Only argument registers are preserved.
// 5. The helper pops the FunctionIDOrClientID argument from the stack.
//
void CodeGen::genProfilingLeaveCallback(unsigned helper)
{
    assert((helper == CORINFO_HELP_PROF_FCN_LEAVE) || (helper == CORINFO_HELP_PROF_FCN_TAILCALL));

    // Only hook if profiler says it's okay.
    if (!compiler->compIsProfilerHookNeeded())
    {
        return;
    }

    compiler->info.compProfilerCallback = true;

    // Need to save on to the stack level, since the helper call will pop the argument
    unsigned saveStackLvl2 = genStackLevel;

#if defined(UNIX_X86_ABI)
    // Manually align the stack to be 16-byte aligned. This is similar to CodeGen::genAlignStackBeforeCall()
    GetEmitter()->emitIns_R_I(INS_sub, EA_4BYTE, REG_SPBASE, 0xC);
    AddStackLevel(0xC);
    AddNestedAlignment(0xC);
#endif // UNIX_X86_ABI

    //
    // Push the profilerHandle
    //

    if (compiler->compProfilerMethHndIndirected)
    {
        GetEmitter()->emitIns_AR_R(INS_push, EA_PTR_DSP_RELOC, REG_NA, REG_NA, (ssize_t)compiler->compProfilerMethHnd);
    }
    else
    {
        inst_IV(INS_push, (size_t)compiler->compProfilerMethHnd);
    }
    genSinglePush();

#if defined(UNIX_X86_ABI)
    int argSize = -REGSIZE_BYTES; // negative means caller-pop (cdecl)
#else
    int argSize = REGSIZE_BYTES;
#endif
    genEmitHelperCall(helper, argSize, EA_UNKNOWN /* retSize */);

    // Check that we have place for the push.
    assert(compiler->fgGetPtrArgCntMax() >= 1);

#if defined(UNIX_X86_ABI)
    // Restoring alignment manually. This is similar to CodeGen::genRemoveAlignmentAfterCall
    GetEmitter()->emitIns_R_I(INS_add, EA_4BYTE, REG_SPBASE, 0x10);
    SubtractStackLevel(0x10);
    SubtractNestedAlignment(0xC);
#endif // UNIX_X86_ABI

    /* Restore the stack level */
    SetStackLevel(saveStackLvl2);
}

#endif // TARGET_X86

#ifdef TARGET_AMD64

//-----------------------------------------------------------------------------------
// genProfilingEnterCallback: Generate the profiling function enter callback.
//
// Arguments:
//     initReg        - register to use as scratch register
//     pInitRegZeroed - OUT parameter. *pInitRegZeroed is set to 'false' if and only if
//                      this call sets 'initReg' to a non-zero value.
//
// Return Value:
//     None
//
void CodeGen::genProfilingEnterCallback(regNumber initReg, bool* pInitRegZeroed)
{
    assert(compiler->compGeneratingProlog);

    // Give profiler a chance to back out of hooking this method
    if (!compiler->compIsProfilerHookNeeded())
    {
        return;
    }

#if !defined(UNIX_AMD64_ABI)

    unsigned   varNum;
    LclVarDsc* varDsc;

    // Since the method needs to make a profiler callback, it should have out-going arg space allocated.
    noway_assert(compiler->lvaOutgoingArgSpaceVar != BAD_VAR_NUM);
    noway_assert(compiler->lvaOutgoingArgSpaceSize >= (4 * REGSIZE_BYTES));

    // Home all arguments passed in arg registers (RCX, RDX, R8 and R9).
    // In case of vararg methods, arg regs are already homed.
    //
    // Note: Here we don't need to worry about updating gc'info since enter
    // callback is generated as part of prolog which is non-gc interruptible.
    // Moreover GC cannot kick while executing inside profiler callback which is a
    // profiler requirement so it can examine arguments which could be obj refs.
    if (!compiler->info.compIsVarArgs)
    {
        for (varNum = 0, varDsc = compiler->lvaTable; varNum < compiler->info.compArgsCount; varNum++, varDsc++)
        {
            noway_assert(varDsc->lvIsParam);

            if (!varDsc->lvIsRegArg)
            {
                continue;
            }

            var_types storeType = varDsc->lvaArgType();
            regNumber argReg    = varDsc->GetArgReg();

            instruction store_ins = ins_Store(storeType);

#ifdef FEATURE_SIMD
            if ((storeType == TYP_SIMD8) && genIsValidIntReg(argReg))
            {
                store_ins = INS_mov;
            }
#endif // FEATURE_SIMD

            GetEmitter()->emitIns_S_R(store_ins, emitTypeSize(storeType), argReg, varNum, 0);
        }
    }

    // Emit profiler EnterCallback(ProfilerMethHnd, caller's SP)
    // RCX = ProfilerMethHnd
    if (compiler->compProfilerMethHndIndirected)
    {
        // Profiler hooks enabled during Ngen time.
        // Profiler handle needs to be accessed through an indirection of a pointer.
        GetEmitter()->emitIns_R_AI(INS_mov, EA_PTR_DSP_RELOC, REG_ARG_0, (ssize_t)compiler->compProfilerMethHnd);
    }
    else
    {
        // No need to record relocations, if we are generating ELT hooks under the influence
        // of COMPlus_JitELTHookEnabled=1
        if (compiler->opts.compJitELTHookEnabled)
        {
            genSetRegToIcon(REG_ARG_0, (ssize_t)compiler->compProfilerMethHnd, TYP_I_IMPL);
        }
        else
        {
            instGen_Set_Reg_To_Imm(EA_8BYTE, REG_ARG_0, (ssize_t)compiler->compProfilerMethHnd);
        }
    }

    // RDX = caller's SP
    // Notes
    //   1) Here we can query caller's SP offset since prolog will be generated after final frame layout.
    //   2) caller's SP relative offset to FramePointer will be negative.  We need to add absolute value
    //      of that offset to FramePointer to obtain caller's SP value.
    assert(compiler->lvaOutgoingArgSpaceVar != BAD_VAR_NUM);
    int callerSPOffset = compiler->lvaToCallerSPRelativeOffset(0, isFramePointerUsed());
    GetEmitter()->emitIns_R_AR(INS_lea, EA_PTRSIZE, REG_ARG_1, genFramePointerReg(), -callerSPOffset);

    // This will emit either
    // "call ip-relative 32-bit offset" or
    // "mov rax, helper addr; call rax"
    genEmitHelperCall(CORINFO_HELP_PROF_FCN_ENTER, 0, EA_UNKNOWN);

    // TODO-AMD64-CQ: Rather than reloading, see if this could be optimized by combining with prolog
    // generation logic that moves args around as required by first BB entry point conditions
    // computed by LSRA.  Code pointers for investigating this further: genFnPrologCalleeRegArgs()
    // and genEnregisterIncomingStackArgs().
    //
    // Now reload arg registers from home locations.
    // Vararg methods:
    //   - we need to reload only known (i.e. fixed) reg args.
    //   - if floating point type, also reload it into corresponding integer reg
    for (varNum = 0, varDsc = compiler->lvaTable; varNum < compiler->info.compArgsCount; varNum++, varDsc++)
    {
        noway_assert(varDsc->lvIsParam);

        if (!varDsc->lvIsRegArg)
        {
            continue;
        }

        var_types loadType = varDsc->lvaArgType();
        regNumber argReg   = varDsc->GetArgReg();

        instruction load_ins = ins_Load(loadType);

#ifdef FEATURE_SIMD
        if ((loadType == TYP_SIMD8) && genIsValidIntReg(argReg))
        {
            load_ins = INS_mov;
        }
#endif // FEATURE_SIMD

        GetEmitter()->emitIns_R_S(load_ins, emitTypeSize(loadType), argReg, varNum, 0);

#if FEATURE_VARARG
        if (compiler->info.compIsVarArgs && varTypeIsFloating(loadType))
        {
            regNumber intArgReg = compiler->getCallArgIntRegister(argReg);
            inst_Mov(TYP_LONG, intArgReg, argReg, /* canSkip */ false, emitActualTypeSize(loadType));
        }
#endif //  FEATURE_VARARG
    }

    // If initReg is one of RBM_CALLEE_TRASH, then it needs to be zero'ed before using.
    if ((RBM_CALLEE_TRASH & genRegMask(initReg)) != 0)
    {
        *pInitRegZeroed = false;
    }

#else // !defined(UNIX_AMD64_ABI)

    // Emit profiler EnterCallback(ProfilerMethHnd, caller's SP)
    // R14 = ProfilerMethHnd
    if (compiler->compProfilerMethHndIndirected)
    {
        // Profiler hooks enabled during Ngen time.
        // Profiler handle needs to be accessed through an indirection of a pointer.
        GetEmitter()->emitIns_R_AI(INS_mov, EA_PTR_DSP_RELOC, REG_PROFILER_ENTER_ARG_0,
                                   (ssize_t)compiler->compProfilerMethHnd);
    }
    else
    {
        // No need to record relocations, if we are generating ELT hooks under the influence
        // of COMPlus_JitELTHookEnabled=1
        if (compiler->opts.compJitELTHookEnabled)
        {
            genSetRegToIcon(REG_PROFILER_ENTER_ARG_0, (ssize_t)compiler->compProfilerMethHnd, TYP_I_IMPL);
        }
        else
        {
            instGen_Set_Reg_To_Imm(EA_8BYTE, REG_PROFILER_ENTER_ARG_0, (ssize_t)compiler->compProfilerMethHnd);
        }
    }

    // R15 = caller's SP
    // Notes
    //   1) Here we can query caller's SP offset since prolog will be generated after final frame layout.
    //   2) caller's SP relative offset to FramePointer will be negative.  We need to add absolute value
    //      of that offset to FramePointer to obtain caller's SP value.
    assert(compiler->lvaOutgoingArgSpaceVar != BAD_VAR_NUM);
    int callerSPOffset = compiler->lvaToCallerSPRelativeOffset(0, isFramePointerUsed());
    GetEmitter()->emitIns_R_AR(INS_lea, EA_PTRSIZE, REG_PROFILER_ENTER_ARG_1, genFramePointerReg(), -callerSPOffset);

    // We can use any callee trash register (other than RAX, RDI, RSI) for call target.
    // We use R11 here. This will emit either
    // "call ip-relative 32-bit offset" or
    // "mov r11, helper addr; call r11"
    genEmitHelperCall(CORINFO_HELP_PROF_FCN_ENTER, 0, EA_UNKNOWN, REG_DEFAULT_PROFILER_CALL_TARGET);

    // If initReg is one of RBM_CALLEE_TRASH, then it needs to be zero'ed before using.
    if ((RBM_CALLEE_TRASH & genRegMask(initReg)) != 0)
    {
        *pInitRegZeroed = false;
    }

#endif // !defined(UNIX_AMD64_ABI)
}

//-----------------------------------------------------------------------------------
// genProfilingLeaveCallback: Generate the profiling function leave or tailcall callback.
// Technically, this is not part of the epilog; it is called when we are generating code for a GT_RETURN node.
//
// Arguments:
//     helper - which helper to call. Either CORINFO_HELP_PROF_FCN_LEAVE or CORINFO_HELP_PROF_FCN_TAILCALL
//
// Return Value:
//     None
//
void CodeGen::genProfilingLeaveCallback(unsigned helper)
{
    assert((helper == CORINFO_HELP_PROF_FCN_LEAVE) || (helper == CORINFO_HELP_PROF_FCN_TAILCALL));

    // Only hook if profiler says it's okay.
    if (!compiler->compIsProfilerHookNeeded())
    {
        return;
    }

    compiler->info.compProfilerCallback = true;

#if !defined(UNIX_AMD64_ABI)

    // Since the method needs to make a profiler callback, it should have out-going arg space allocated.
    noway_assert(compiler->lvaOutgoingArgSpaceVar != BAD_VAR_NUM);
    noway_assert(compiler->lvaOutgoingArgSpaceSize >= (4 * REGSIZE_BYTES));

    // If thisPtr needs to be kept alive and reported, it cannot be one of the callee trash
    // registers that profiler callback kills.
    if (compiler->lvaKeepAliveAndReportThis() && compiler->lvaTable[compiler->info.compThisArg].lvIsInReg())
    {
        regMaskTP thisPtrMask = genRegMask(compiler->lvaTable[compiler->info.compThisArg].GetRegNum());
        noway_assert((RBM_PROFILER_LEAVE_TRASH & thisPtrMask) == 0);
    }

    // At this point return value is computed and stored in RAX or XMM0.
    // On Amd64, Leave callback preserves the return register.  We keep
    // RAX alive by not reporting as trashed by helper call.  Also note
    // that GC cannot kick-in while executing inside profiler callback,
    // which is a requirement of profiler as well since it needs to examine
    // return value which could be an obj ref.

    // RCX = ProfilerMethHnd
    if (compiler->compProfilerMethHndIndirected)
    {
        // Profiler hooks enabled during Ngen time.
        // Profiler handle needs to be accessed through an indirection of an address.
        GetEmitter()->emitIns_R_AI(INS_mov, EA_PTR_DSP_RELOC, REG_ARG_0, (ssize_t)compiler->compProfilerMethHnd);
    }
    else
    {
        // Don't record relocations, if we are generating ELT hooks under the influence
        // of COMPlus_JitELTHookEnabled=1
        if (compiler->opts.compJitELTHookEnabled)
        {
            genSetRegToIcon(REG_ARG_0, (ssize_t)compiler->compProfilerMethHnd, TYP_I_IMPL);
        }
        else
        {
            instGen_Set_Reg_To_Imm(EA_8BYTE, REG_ARG_0, (ssize_t)compiler->compProfilerMethHnd);
        }
    }

    // RDX = caller's SP
    // TODO-AMD64-Cleanup: Once we start doing codegen after final frame layout, retain the "if" portion
    // of the stmnts to execute unconditionally and clean-up rest.
    if (compiler->lvaDoneFrameLayout == Compiler::FINAL_FRAME_LAYOUT)
    {
        // Caller's SP relative offset to FramePointer will be negative.  We need to add absolute
        // value of that offset to FramePointer to obtain caller's SP value.
        int callerSPOffset = compiler->lvaToCallerSPRelativeOffset(0, isFramePointerUsed());
        GetEmitter()->emitIns_R_AR(INS_lea, EA_PTRSIZE, REG_ARG_1, genFramePointerReg(), -callerSPOffset);
    }
    else
    {
        // If we are here means that it is a tentative frame layout during which we
        // cannot use caller's SP offset since it is an estimate.  For now we require the
        // method to have at least a single arg so that we can use it to obtain caller's
        // SP.
        LclVarDsc* varDsc = compiler->lvaTable;
        NYI_IF((varDsc == nullptr) || !varDsc->lvIsParam, "Profiler ELT callback for a method without any params");

        // lea rdx, [FramePointer + Arg0's offset]
        GetEmitter()->emitIns_R_S(INS_lea, EA_PTRSIZE, REG_ARG_1, 0, 0);
    }

    // We can use any callee trash register (other than RAX, RCX, RDX) for call target.
    // We use R8 here. This will emit either
    // "call ip-relative 32-bit offset" or
    // "mov r8, helper addr; call r8"
    genEmitHelperCall(helper, 0, EA_UNKNOWN, REG_ARG_2);

#else // !defined(UNIX_AMD64_ABI)

    // RDI = ProfilerMethHnd
    if (compiler->compProfilerMethHndIndirected)
    {
        GetEmitter()->emitIns_R_AI(INS_mov, EA_PTR_DSP_RELOC, REG_ARG_0, (ssize_t)compiler->compProfilerMethHnd);
    }
    else
    {
        if (compiler->opts.compJitELTHookEnabled)
        {
            genSetRegToIcon(REG_ARG_0, (ssize_t)compiler->compProfilerMethHnd, TYP_I_IMPL);
        }
        else
        {
            instGen_Set_Reg_To_Imm(EA_8BYTE, REG_ARG_0, (ssize_t)compiler->compProfilerMethHnd);
        }
    }

    // RSI = caller's SP
    if (compiler->lvaDoneFrameLayout == Compiler::FINAL_FRAME_LAYOUT)
    {
        int callerSPOffset = compiler->lvaToCallerSPRelativeOffset(0, isFramePointerUsed());
        GetEmitter()->emitIns_R_AR(INS_lea, EA_PTRSIZE, REG_ARG_1, genFramePointerReg(), -callerSPOffset);
    }
    else
    {
        LclVarDsc* varDsc = compiler->lvaTable;
        NYI_IF((varDsc == nullptr) || !varDsc->lvIsParam, "Profiler ELT callback for a method without any params");

        // lea rdx, [FramePointer + Arg0's offset]
        GetEmitter()->emitIns_R_S(INS_lea, EA_PTRSIZE, REG_ARG_1, 0, 0);
    }

    // We can use any callee trash register (other than RAX, RDI, RSI) for call target.
    // We use R11 here. This will emit either
    // "call ip-relative 32-bit offset" or
    // "mov r11, helper addr; call r11"
    genEmitHelperCall(helper, 0, EA_UNKNOWN, REG_DEFAULT_PROFILER_CALL_TARGET);

#endif // !defined(UNIX_AMD64_ABI)
}

#endif // TARGET_AMD64

#endif // PROFILING_SUPPORTED

void CodeGen::genCodeForInstr(GenTreeInstr* instr)
{
    unreached();
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
            if (addrMode->GetBase() != nullptr)
            {
                m_base = codeGen->genConsumeReg(addrMode->GetBase());
            }

            if (addrMode->GetIndex() != nullptr)
            {
                m_index = codeGen->genConsumeReg(addrMode->GetIndex());
                m_scale = static_cast<uint8_t>(addrMode->GetScale());
            }

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
        GetEmitter()->emitIns_R_ARX(ins, attr, reg, addrMode.Base(), addrMode.Index(), addrMode.Scale(),
                                    addrMode.Disp(offset));
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
        GetEmitter()->emitIns_ARX_R(ins, attr, reg, addrMode.Base(), addrMode.Index(), addrMode.Scale(),
                                    addrMode.Disp(offset));
    }
}

void CodeGen::genStoreSIMD12(const GenAddrMode& dst, GenTree* value, regNumber tmpReg)
{
    if (value->isContained())
    {
        GenAddrMode src(value, this);

#ifdef TARGET_64BIT
        inst_R_AM(INS_mov, EA_8BYTE, tmpReg, src, 0);
        inst_AM_R(INS_mov, EA_8BYTE, tmpReg, dst, 0);
        inst_R_AM(INS_mov, EA_4BYTE, tmpReg, src, 8);
        inst_AM_R(INS_mov, EA_4BYTE, tmpReg, dst, 8);
#else
        inst_R_AM(INS_movsdsse2, EA_8BYTE, tmpReg, src, 0);
        inst_AM_R(INS_movsdsse2, EA_8BYTE, tmpReg, dst, 0);
        inst_R_AM(INS_movss, EA_4BYTE, tmpReg, src, 8);
        inst_AM_R(INS_movss, EA_4BYTE, tmpReg, dst, 8);
#endif
        return;
    }

    regNumber valueReg = genConsumeReg(value);

    inst_AM_R(INS_movsdsse2, EA_8BYTE, valueReg, dst, 0);

    if (value->IsHWIntrinsicZero())
    {
        tmpReg = valueReg;
    }
    else
    {
        GetEmitter()->emitIns_R_R(INS_movhlps, EA_16BYTE, tmpReg, valueReg);
    }

    inst_AM_R(INS_movss, EA_4BYTE, tmpReg, dst, 8);
}

void CodeGen::genLoadSIMD12(GenTree* load)
{
    GenAddrMode src(load, this);

    regNumber tmpReg = load->GetSingleTempReg();
    regNumber dstReg = load->GetRegNum();

    assert(tmpReg != dstReg);

    inst_R_AM(INS_movsdsse2, EA_8BYTE, dstReg, src, 0);
    inst_R_AM(INS_movss, EA_4BYTE, tmpReg, src, 8);
    GetEmitter()->emitIns_R_R(INS_movlhps, EA_16BYTE, dstReg, tmpReg);

    genProduceReg(load);
}

#ifdef TARGET_X86

void CodeGen::genStoreSIMD12ToStack(regNumber valueReg, regNumber tmpReg)
{
    assert(genIsValidFloatReg(valueReg));
    assert(genIsValidFloatReg(tmpReg));

    GetEmitter()->emitIns_AR_R(INS_movsdsse2, EA_8BYTE, valueReg, REG_SPBASE, 0);
    GetEmitter()->emitIns_R_R(INS_movhlps, EA_16BYTE, tmpReg, valueReg);
    GetEmitter()->emitIns_AR_R(INS_movss, EA_4BYTE, tmpReg, REG_SPBASE, 8);
}

#endif // TARGET_X86

#ifdef FEATURE_SIMD

// Save the upper half of a TYP_SIMD32 vector to the given register, if any, or to memory.
// The upper half of all AVX registers is volatile, even the callee-save registers.
// When a 32-byte SIMD value is live across a call, the register allocator will use this intrinsic
// to cause the upper half to be saved. It will first attempt to find another, unused, callee-save
// register. If such a register cannot be found, it will save the upper half to the upper half
// of the localVar's home location.
// (Note that if there are no caller-save registers available, the entire 32 byte
// value will be spilled to the stack.)
void CodeGen::genSIMDUpperSpill(GenTreeUnOp* node)
{
    GenTree* op1 = node->GetOp(0);
    assert(op1->OperIs(GT_LCL_VAR) && op1->TypeIs(TYP_SIMD32));

    regNumber srcReg = genConsumeReg(op1);
    assert(srcReg != REG_NA);
    regNumber dstReg = node->GetRegNum();

    if (dstReg != REG_NA)
    {
        GetEmitter()->emitIns_R_R_I(INS_vextractf128, EA_32BYTE, dstReg, srcReg, 1);
        genProduceReg(node);
    }
    else
    {
        unsigned lclNum = op1->AsLclVar()->GetLclNum();
        assert(compiler->lvaGetDesc(lclNum)->lvOnFrame);

        GetEmitter()->emitIns_S_R_I(INS_vextractf128, EA_32BYTE, lclNum, 16, srcReg, 1);
    }
}

// Restore the upper half of a TYP_SIMD32 vector to the given register, if any, or to memory.
// For consistency with genSIMDIntrinsicUpperSave, and to ensure that LCL_VAR nodes always
// have their home register, this node has its dtsReg on the LCL_VAR operand, and its source
// on the node.
void CodeGen::genSIMDUpperUnspill(GenTreeUnOp* node)
{
    GenTree* op1 = node->GetOp(0);
    assert(op1->OperIs(GT_LCL_VAR) && op1->TypeIs(TYP_SIMD32));

    regNumber srcReg = node->GetRegNum();
    regNumber dstReg = genConsumeReg(op1);
    assert(dstReg != REG_NA);

    if (srcReg != REG_NA)
    {
        GetEmitter()->emitIns_R_R_R_I(INS_vinsertf128, EA_32BYTE, dstReg, dstReg, srcReg, 1);
    }
    else
    {
        unsigned lclNum = op1->AsLclVar()->GetLclNum();
        assert(compiler->lvaGetDesc(lclNum)->lvOnFrame);

        GetEmitter()->emitIns_R_R_S_I(INS_vinsertf128, EA_32BYTE, dstReg, dstReg, lclNum, 16, 1);
    }
}

#endif // FEATURE_SIMD

//------------------------------------------------------------------------
// genPushCalleeSavedRegisters: Push any callee-saved registers we have used.
//
void CodeGen::genPushCalleeSavedRegisters()
{
    assert(compiler->compGeneratingProlog);

    // x86/x64 doesn't support push of xmm/ymm regs, therefore consider only integer registers for pushing onto stack
    // here. Space for float registers to be preserved is stack allocated and saved as part of prolog sequence and not
    // here.
    regMaskTP rsPushRegs = regSet.rsGetModifiedRegsMask() & RBM_INT_CALLEE_SAVED;

#if ETW_EBP_FRAMED
    if (!isFramePointerUsed() && regSet.rsRegsModified(RBM_FPBASE))
    {
        noway_assert(!"Used register RBM_FPBASE as a scratch register!");
    }
#endif

    // On X86/X64 we have already pushed the FP (frame-pointer) prior to calling this method
    if (isFramePointerUsed())
    {
        rsPushRegs &= ~RBM_FPBASE;
    }

#ifdef DEBUG
    if (compiler->compCalleeRegsPushed != genCountBits(rsPushRegs))
    {
        printf("Error: unexpected number of callee-saved registers to push. Expected: %d. Got: %d ",
               compiler->compCalleeRegsPushed, genCountBits(rsPushRegs));
        dspRegMask(rsPushRegs);
        printf("\n");
        assert(compiler->compCalleeRegsPushed == genCountBits(rsPushRegs));
    }
#endif // DEBUG

    // Push backwards so we match the order we will pop them in the epilog
    // and all the other code that expects it to be in this order.
    for (regNumber reg = REG_INT_LAST; rsPushRegs != RBM_NONE; reg = REG_PREV(reg))
    {
        regMaskTP regBit = genRegMask(reg);

        if ((regBit & rsPushRegs) != 0)
        {
            inst_RV(INS_push, reg, TYP_REF);
            compiler->unwindPush(reg);
#ifdef USING_SCOPE_INFO
            if (!doubleAlignOrFramePointerUsed())
            {
                psiAdjustStackLevel(REGSIZE_BYTES);
            }
#endif // USING_SCOPE_INFO
            rsPushRegs &= ~regBit;
        }
    }
}

#endif // TARGET_XARCH
