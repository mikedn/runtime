// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef _MSC_VER
#pragma warning(disable : 4310) // cast truncates constant shift - happens for (int8_t)0xb1
#endif

#ifdef TARGET_XARCH

#include "emit.h"
#include "codegen.h"
#include "lower.h"
#include "patchpointinfo.h"

void CodeGen::PrologSetGSSecurityCookie(regNumber initReg, bool* initRegZeroed)
{
    assert(compiler->getNeedsGSSecurityCookie());

    unsigned gsCookieLclNum = compiler->lvaGSSecurityCookie;

    if (m_gsCookieAddr == nullptr)
    {
        noway_assert(m_gsCookieVal != 0);

#ifdef TARGET_AMD64
        if (!FitsIn<int32_t>(m_gsCookieVal))
        {
            GetEmitter()->emitIns_R_I(INS_mov, EA_8BYTE, initReg, m_gsCookieVal);
            GetEmitter()->emitIns_S_R(INS_mov, EA_8BYTE, initReg, gsCookieLclNum, 0);
            *initRegZeroed = false;
        }
        else
#endif
        {
            GetEmitter()->emitIns_S_I(INS_mov, EA_PTRSIZE, gsCookieLclNum, 0, static_cast<int>(m_gsCookieVal));
        }
    }
    else
    {
        // Always use EAX on x86 and x64
        // On x64, if we're not moving into RAX, and the address isn't RIP relative, we can't encode it.
        //  mov   eax, dword ptr [compiler->gsGlobalSecurityCookieAddr]
        //  mov   dword ptr [frame.GSSecurityCookie], eax
        GetEmitter()->emitIns_R_AI(INS_mov, EA_PTR_DSP_RELOC, REG_EAX, reinterpret_cast<ssize_t>(m_gsCookieAddr));
        GetEmitter()->emitIns_S_R(INS_mov, EA_PTRSIZE, REG_EAX, gsCookieLclNum, 0);

        if (initReg == REG_EAX)
        {
            *initRegZeroed = false;
        }
    }
}

void CodeGen::EpilogGSCookieCheck(bool tailCallEpilog)
{
    regNumber regGSCheck;

    if (!tailCallEpilog)
    {
        // We can use any callee trash register that is not a return register
        // or contain 'this' pointer (keep alive this), since we are generating
        // GS cookie check after a GT_RETURN node.

        if (compiler->lvaKeepAliveAndReportThis() && compiler->lvaGetDesc(compiler->info.compThisArg)->lvIsInReg() &&
            (compiler->lvaGetDesc(compiler->info.compThisArg)->GetRegNum() == REG_ARG_0))
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
        regGSCheck = REG_EAX;
#else
        // Jmp calls: specify method handle using which JIT queries VM for its entry point
        // address and hence it can neither be a VSD call nor PInvoke calli with cookie
        // parameter.  Therefore, in case of jmp calls it is safe to use R11.
        regGSCheck = REG_R11;
#endif
    }

    unsigned gsCookieLclNum = compiler->lvaGSSecurityCookie;
#ifdef TARGET_X86
    var_types pushedRegType = TYP_UNDEF;
#endif

    if (m_gsCookieAddr == nullptr)
    {
        noway_assert(m_gsCookieVal != 0);

#ifdef TARGET_AMD64
        if (!FitsIn<int32_t>(m_gsCookieVal))
        {
            GetEmitter()->emitIns_R_I(INS_mov, EA_8BYTE, regGSCheck, m_gsCookieVal);
            GetEmitter()->emitIns_S_R(INS_cmp, EA_8BYTE, regGSCheck, gsCookieLclNum, 0);
        }
        else
#endif
        {
            GetEmitter()->emitIns_S_I(INS_cmp, EA_PTRSIZE, gsCookieLclNum, 0, static_cast<int>(m_gsCookieVal));
        }
    }
    else
    {
#ifdef TARGET_X86
        if (tailCallEpilog)
        {
            pushedRegType = PushTempReg(regGSCheck);
        }
#endif

        instGen_Set_Reg_To_Imm(EA_HANDLE_CNS_RELOC, regGSCheck, reinterpret_cast<ssize_t>(m_gsCookieAddr));
        GetEmitter()->emitIns_R_AR(INS_mov, EA_PTRSIZE, regGSCheck, regGSCheck, 0);
        GetEmitter()->emitIns_S_R(INS_cmp, EA_PTRSIZE, regGSCheck, gsCookieLclNum, 0);
    }

    BasicBlock* gsCheckBlk = genCreateTempLabel();
    inst_JMP(EJ_je, gsCheckBlk);
    genEmitHelperCall(CORINFO_HELP_FAIL_FAST);
    genDefineTempLabel(gsCheckBlk);

#ifdef TARGET_X86
    if (pushedRegType != TYP_UNDEF)
    {
        PopTempReg(regGSCheck, pushedRegType);
    }
#endif
}

#ifdef TARGET_X86

var_types CodeGen::PushTempReg(regNumber reg)
{
    regMaskTP regMask = genRegMask(reg);
    var_types type;

    if ((liveness.GetGCRegs(TYP_REF) & regMask) != RBM_NONE)
    {
        type = TYP_REF;
    }
    else if ((liveness.GetGCRegs(TYP_BYREF) & regMask) != RBM_NONE)
    {
        type = TYP_BYREF;
    }
    else
    {
        type = TYP_INT;
    }

    GetEmitter()->emitIns_R(INS_push, emitTypeSize(type), reg);
    AddStackLevel(REGSIZE_BYTES);
    liveness.RemoveGCRegs(regMask);

    return type;
}

void CodeGen::PopTempReg(regNumber reg, var_types type)
{
    assert(varTypeIsI(type));

    GetEmitter()->emitIns_R(INS_pop, emitTypeSize(type), reg);
    SubtractStackLevel(REGSIZE_BYTES);

    if (varTypeIsGC(type))
    {
        liveness.SetGCRegType(reg, type);
    }
}

// Adjust the stack level, if required, for a throw helper block
// Must be called just prior to generating code for 'block'.
void CodeGen::genAdjustStackLevel(BasicBlock* block)
{
    // Check for inserted throw blocks and adjust genStackLevel.

    if (!compiler->fgIsThrowHelperBlock(block))
    {
        return;
    }

#ifdef UNIX_X86_ABI
    if (isFramePointerUsed())
    {
        // x86/Linux requires stack frames to be 16-byte aligned, but SP may be unaligned
        // at this point if a jump to this block is made in the middle of pushing arguments.
        //
        // Here we restore SP to prevent potential stack alignment issues.
        GetEmitter()->emitIns_R_AR(INS_lea, EA_PTRSIZE, REG_SPBASE, REG_FPBASE, -genSPtoFPdelta());
    }
#endif

    if (!isFramePointerUsed())
    {
        noway_assert(block->bbFlags & BBF_HAS_LABEL);

        SetStackLevel(compiler->fgGetThrowHelperBlockStackLevel(block) * REGSIZE_BYTES);

        if (genStackLevel != 0)
        {
            GetEmitter()->emitMarkStackLvl(genStackLevel);
            inst_RV_IV(INS_add, REG_SPBASE, genStackLevel, EA_PTRSIZE);
            SetStackLevel(0);
        }
    }
}

void CodeGen::SubtractStackLevel(unsigned adjustment)
{
    assert(genStackLevel >= adjustment);
    unsigned newStackLevel = genStackLevel - adjustment;
    if (genStackLevel != newStackLevel)
    {
        JITDUMP("Adjusting stack level from %d to %d\n", genStackLevel, newStackLevel);
    }
    genStackLevel = newStackLevel;
}

void CodeGen::AddStackLevel(unsigned adjustment)
{
    unsigned newStackLevel = genStackLevel + adjustment;
    if (genStackLevel != newStackLevel)
    {
        JITDUMP("Adjusting stack level from %d to %d\n", genStackLevel, newStackLevel);
    }
    genStackLevel = newStackLevel;
}

void CodeGen::SetStackLevel(unsigned newStackLevel)
{
    if (genStackLevel != newStackLevel)
    {
        JITDUMP("Setting stack level from %d to %d\n", genStackLevel, newStackLevel);
    }
    genStackLevel = newStackLevel;
}

#endif // TARGET_X86

BasicBlock* CodeGen::genCallFinally(BasicBlock* block)
{
#ifdef FEATURE_EH_FUNCLETS
    // Generate a call to the finally, like this:
    //      mov         rcx,qword ptr [rbp + 20H]       // Load rcx with PSPSym
    //      call        finally-funclet
    //      jmp         finally-return                  // Only for non-retless finally calls
    // The jmp can be a NOP if we're going to the next block.
    // If we're generating code for the main function (not a funclet), and there is no localloc,
    // then RSP at this point is the same shift as that stored in the PSPSym. So just copy RSP
    // instead of loading the PSPSym in this case, or if PSPSym is not used (CoreRT ABI).

    if ((compiler->lvaPSPSym == BAD_VAR_NUM) ||
        (!compiler->compLocallocUsed && (compiler->funCurrentFunc()->funKind == FUNC_ROOT)))
    {
#ifndef UNIX_X86_ABI
        inst_Mov(TYP_I_IMPL, REG_ARG_0, REG_SPBASE, /* canSkip */ false);
#endif
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
#endif

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
#endif
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
    filterEndOffsetSlotOffs = compiler->lvaGetDesc(compiler->lvaShadowSPslotsVar)->GetBlockSize() - REGSIZE_BYTES;

    unsigned curNestingSlotOffs;
    curNestingSlotOffs = (unsigned)(filterEndOffsetSlotOffs - ((finallyNesting + 1) * REGSIZE_BYTES));

    // Zero out the slot for the next nesting level
    GetEmitter()->emitIns_S_I(INS_mov, EA_PTRSIZE, compiler->lvaShadowSPslotsVar, curNestingSlotOffs - REGSIZE_BYTES,
                              0);
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
        GetEmitter()->emitIns_I(INS_push_hide, EA_4BYTE, 0);
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

#ifdef FEATURE_EH_FUNCLETS

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
        noway_assert(isFramePointerUsed());

        // Return using a pop-jmp sequence. As the "try" block calls
        // the finally with a jmp, this leaves the x86 call-ret stack
        // balanced in the normal flow of path.

        inst_RV(INS_pop_hide, REG_EAX, TYP_I_IMPL);
        inst_RV(INS_i_jmp, REG_EAX, TYP_I_IMPL);
    }
    else
    {
        assert(block->bbJumpKind == BBJ_EHFILTERRET);

        instGen(INS_ret);
    }
}

#endif // !FEATURE_EH_FUNCLETS

void CodeGen::instGen_Set_Reg_To_Zero(emitAttr size, regNumber reg)
{
    GetEmitter()->emitIns_R_R(INS_xor, size, reg, reg);
}

void CodeGen::instGen_Set_Reg_To_Imm(emitAttr  size,
                                     regNumber reg,
                                     ssize_t imm DEBUGARG(void* handle) DEBUGARG(HandleKind handleKind))
{
    // reg cannot be a FP register
    assert(!genIsValidFloatReg(reg));

    if (!compiler->opts.compReloc)
    {
        size = EA_SIZE(size); // Strip any Reloc flags from size if we aren't doing relocs
    }

    if ((imm == 0) && !EA_IS_RELOC(size))
    {
        GetEmitter()->emitIns_R_R(INS_xor, size, reg, reg);
    }
#ifdef TARGET_AMD64
    else if (compiler->eeIsRIPRelativeAddress(reinterpret_cast<void*>(imm)))
    {
        emitAttr newSize = EA_PTR_DSP_RELOC;

        if (EA_IS_BYREF(size))
        {
            newSize = EA_SET_FLG(newSize, EA_BYREF_FLG);
        }

        GetEmitter()->emitIns_R_AI(INS_lea, newSize, reg, imm);
    }
#endif
    else
    {
        GetEmitter()->emitIns_R_I(INS_mov, size, reg, imm);
    }
}

void CodeGen::GenIntCon(GenTreeIntCon* node, regNumber reg, var_types type)
{
    if (node->ImmedValNeedsReloc(compiler))
    {
        emitAttr size = EA_HANDLE_CNS_RELOC;

        // TODO-MIKE-Review: Who cares about byref on a constant?!?
        if (type == TYP_BYREF)
        {
            size = EA_SET_FLG(size, EA_BYREF_FLG);
        }

        instGen_Set_Reg_To_Imm(size, reg, node->GetValue());

        return;
    }

    if (node->GetValue() == 0)
    {
        // TODO-MIKE-Cleanup: The size should be EA_4BYTE, since the entire register
        // is zeroed out anyway on x64. However, this results in diffs in the GC info
        // because managed null isn't reported anymore. It doesn't need to be reported
        // but then DefReg reports it and we end up with inconsistencies at call sites
        // and block boundaries. It's not clear if that could cause problems or not.
        GetEmitter()->emitIns_R_R(INS_xor, emitActualTypeSize(type), reg, reg);

        return;
    }

    // The only REF constant that can come this path is a 'null' since it is not
    // relocatable. Other REF type constants (e.g. string objects) go through a
    // different code path.
    noway_assert(type != TYP_REF);

    // TODO-XArch-CQ: needs all the optimized cases
    GetEmitter()->emitIns_R_I(INS_mov, emitActualTypeSize(type), reg, node->GetValue() DEBUGARG(node->GetHandleKind()));
}

void CodeGen::GenDblCon(GenTreeDblCon* node, regNumber reg, var_types type)
{
    if (node->IsPositiveZero())
    {
        GetEmitter()->emitIns_R_R(INS_xorps, EA_16BYTE, reg, reg);

        return;
    }

    emitAttr             size = emitTypeSize(node->GetType());
    CORINFO_FIELD_HANDLE data = GetEmitter()->emitFltOrDblConst(node->GetValue(), size);
    GetEmitter()->emitIns_R_C(ins_Load(type), size, reg, data);
}

void CodeGen::genCodeForNegNot(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_NEG, GT_NOT) && varTypeIsIntegral(node->GetType()));

    instruction ins    = node->OperIs(GT_NEG) ? INS_neg : INS_not;
    var_types   type   = node->GetType();
    regNumber   dstReg = node->GetRegNum();
    regNumber   srcReg = UseReg(node->GetOp(0));

    inst_Mov(type, dstReg, srcReg, /* canSkip */ true);
    GetEmitter()->emitIns_R(ins, emitActualTypeSize(type), dstReg);

    DefReg(node);
}

void CodeGen::genCodeForBswap(GenTree* tree)
{
    // TODO: If we're swapping immediately after a read from memory or immediately before
    // a write to memory, use the MOVBE instruction instead of the BSWAP instruction if
    // the platform supports it.

    assert(tree->OperIs(GT_BSWAP, GT_BSWAP16));

    regNumber targetReg  = tree->GetRegNum();
    var_types targetType = tree->GetType();
    GenTree*  operand    = tree->AsUnOp()->GetOp(0);
    regNumber operandReg = UseReg(operand);

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

    DefReg(tree);
}

void CodeGen::genCodeForIncSaturate(GenTree* tree)
{
    regNumber targetReg  = tree->GetRegNum();
    var_types targetType = tree->GetType();
    GenTree*  operand    = tree->AsUnOp()->GetOp(0);
    regNumber operandReg = UseReg(operand);

    inst_Mov(targetType, targetReg, operandReg, /* canSkip */ true);
    inst_RV_IV(INS_add, targetReg, 1, emitActualTypeSize(targetType));
    inst_RV_IV(INS_sbb, targetReg, 0, emitActualTypeSize(targetType));

    DefReg(tree);
}

void CodeGen::GenMulLong(GenTreeOp* mul)
{
#ifdef TARGET_X86
    assert(mul->OperIs(GT_MULHI, GT_MUL_LONG));
#else
    assert(mul->OperIs(GT_MULHI));
#endif
    assert(!mul->gtOverflowEx());

    emitAttr  size   = emitTypeSize(mul->GetType());
    regNumber dstReg = mul->GetRegNum();
    GenTree*  op1    = mul->GetOp(0);
    GenTree*  op2    = mul->GetOp(1);

    genConsumeRegs(op1);
    genConsumeRegs(op2);

    GenTree* regOp = op1;
    GenTree* rmOp  = op2;

    if (op1->isUsedFromMemory() || (op2->isUsedFromReg() && (op2->GetRegNum() == REG_RAX)))
    {
        std::swap(regOp, rmOp);
    }

    GetEmitter()->emitIns_Mov(INS_mov, size, REG_RAX, regOp->GetRegNum(), /* canSkip */ true);
    emitInsUnary(mul->IsUnsigned() ? INS_mulEAX : INS_imulEAX, size, rmOp);

    if (mul->OperIs(GT_MULHI))
    {
        GetEmitter()->emitIns_Mov(INS_mov, size, dstReg, REG_RDX, /* canSkip */ true);
    }

#ifdef TARGET_X86
    if (mul->OperIs(GT_MUL_LONG))
    {
        DefLongRegs(mul);
        return;
    }
#endif

    DefReg(mul);
}

#ifdef TARGET_X86
void CodeGen::GenLongUMod(GenTreeOp* node)
{
    assert(node != nullptr);
    assert(node->OperGet() == GT_UMOD);
    assert(node->TypeGet() == TYP_INT);

    GenTreeOp* const dividend = node->gtOp1->AsOp();
    assert(dividend->OperGet() == GT_LONG);
    assert(varTypeIsLong(dividend));

    GenTree* const dividendLo = dividend->gtOp1;
    GenTree* const dividendHi = dividend->gtOp2;

    GenTree* const divisor = node->gtOp2;
    assert(divisor->gtSkipReloadOrCopy()->OperGet() == GT_CNS_INT);
    assert(divisor->gtSkipReloadOrCopy()->isUsedFromReg());
    assert(divisor->gtSkipReloadOrCopy()->AsIntCon()->gtIconVal >= 2);
    assert(divisor->gtSkipReloadOrCopy()->AsIntCon()->gtIconVal <= 0x3fffffff);

    UseReg(dividendLo);
    UseReg(dividendHi);
    UseReg(divisor);

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
    GetEmitter()->emitIns_R_R(INS_xor, EA_4BYTE, REG_EDX, REG_EDX);
    inst_RV(INS_div, divisor->GetRegNum(), TYP_INT);
    inst_Mov(TYP_INT, REG_EAX, tempReg, /* canSkip */ false);

    // noOverflow:
    //   div divisor->GetRegNum()
    genDefineTempLabel(noOverflow);
    inst_RV(INS_div, divisor->GetRegNum(), TYP_INT);

    const regNumber targetReg = node->GetRegNum();
    inst_Mov(TYP_INT, targetReg, REG_RDX, /* canSkip */ true);

    DefReg(node);
}
#endif // TARGET_X86

void CodeGen::GenDivMod(GenTreeOp* div)
{
    assert(div->OperIs(GT_DIV, GT_UDIV, GT_MOD, GT_UMOD));
    assert(varTypeIsIntOrI(div->GetType()));

#ifdef TARGET_X86
    if (varTypeIsLong(div->GetOp(0)->GetType()))
    {
        GenLongUMod(div);

        return;
    }
#endif

    GenTree*  op1        = div->GetOp(0);
    GenTree*  op2        = div->GetOp(1);
    bool      isUnsigned = div->OperIs(GT_UDIV, GT_UMOD);
    bool      isDiv      = div->OperIs(GT_DIV, GT_UDIV);
    emitAttr  size       = emitTypeSize(div->GetType());
    regNumber dstReg     = div->GetRegNum();

    UseReg(op1);
    genConsumeRegs(op2);

    GetEmitter()->emitIns_Mov(INS_mov, size, REG_RAX, op1->GetRegNum(), /* canSkip */ true);

    if (isUnsigned || (op1->IsIntCon() && (op1->AsIntCon()->GetValue() >= 0)))
    {
        GetEmitter()->emitIns_R_R(INS_xor, EA_4BYTE, REG_EDX, REG_EDX);
    }
    else
    {
        GetEmitter()->emitIns(INS_cdq, size);
    }

    liveness.RemoveGCRegs(RBM_RDX);

    emitInsUnary(isUnsigned ? INS_div : INS_idiv, size, op2);
    GetEmitter()->emitIns_Mov(INS_mov, size, dstReg, isDiv ? REG_RAX : REG_RDX, true);

    DefReg(div);
}

void CodeGen::genCodeForBinary(GenTreeOp* node)
{
    assert(varTypeIsIntegralOrI(node->GetType()));
#ifdef DEBUG
    bool isValidOper = node->OperIs(GT_ADD, GT_SUB, GT_AND, GT_OR, GT_XOR);
#ifndef TARGET_64BIT
    isValidOper |= node->OperIs(GT_ADD_LO, GT_ADD_HI, GT_SUB_LO, GT_SUB_HI);
#endif
    assert(isValidOper);
#endif

    GenTree* op1 = node->GetOp(0);
    GenTree* op2 = node->GetOp(1);

    assert(IsValidSourceType(node->GetType(), op1->GetType()));
    assert(IsValidSourceType(node->GetType(), op2->GetType()));

    genConsumeRegs(op1);
    genConsumeRegs(op2);

    if (!op1->isUsedFromReg())
    {
        // The first operand must be a register, it can't be memory as that would
        // imply that this is a RMW operation and those are handled elsewhere.
        // For commutative operators we may need to swap the operands to get the
        // reg operand to be first.

        // TODO-MIKE-Cleanup: Why doesn't lowering swap the operands? Leaving this
        // to codegen would make sense if we could mark both operands reg-optional
        // but since we cannot this is just added complication.
        assert(node->OperIsCommutative());
        assert(op2->isUsedFromReg());

        std::swap(op1, op2);
    }

    regNumber op1reg = op1->GetRegNum();
    regNumber op2reg = op2->isUsedFromReg() ? op2->GetRegNum() : REG_NA;
    regNumber dstReg = node->GetRegNum();
    emitter*  emit   = GetEmitter();
    emitAttr  attr   = emitTypeSize(node->GetType());
    GenTree*  dst;
    GenTree*  src;

    if (op1reg == dstReg)
    {
        dst = op1;
        src = op2;
    }
    else if (op2reg == dstReg)
    {
        assert(node->OperIsCommutative());

        dst = op2;
        src = op1;
    }
    else
    {
        if (node->OperIs(GT_ADD) && !node->gtOverflow() && ((node->gtFlags & GTF_SET_FLAGS) == 0) &&
            (op2->IsContainedIntCon() || (op2reg != REG_NA)))
        {
            if (GenTreeIntCon* imm = op2->IsContainedIntCon())
            {
                emit->emitIns_R_AR(INS_lea, attr, dstReg, op1reg, imm->GetInt32Value());
            }
            else
            {
                emit->emitIns_R_ARX(INS_lea, attr, dstReg, op1reg, op2reg, 1, 0);
            }

            DefReg(node);

            return;
        }

        inst_Mov(op1->GetType(), dstReg, op1reg, /* canSkip */ false);

        liveness.SetGCRegType(dstReg, op1->GetType());

        dst = node;
        src = op2;

        // TODO-MIKE-Review: This seems a bit dodgy, emitInsBinary will try to determine
        // if dst is a register by using isUsedFromSpillTemp(). This works even if the
        // node will be spilled, obviously because we haven't spilled it yet. But sill,
        // functions like isUsedFromSpillTemp() are primarily intended for users of the
        // node, not for the node itself. It may be better to pass only the register to
        // emitInsBinary, but unfortunately that requires a new version of emitInsBinary
        // as the current one still needs to support an in-memory first operand for CMPs.
        assert(dst->isUsedFromReg());
    }

    if (node->OperIs(GT_ADD) && src->IsContainedIntCon() && !node->gtOverflow())
    {
        if (src->IsIntCon(1))
        {
            emit->emitIns_R(INS_inc, attr, dstReg);
            DefReg(node);
            return;
        }

        if (src->IsIntCon(-1))
        {
            emit->emitIns_R(INS_dec, attr, dstReg);
            DefReg(node);
            return;
        }
    }

    emitInsBinary(genGetInsForOper(node->GetOper()), attr, dst, src);

    if (node->gtOverflowEx())
    {
#ifdef TARGET_64BIT
        assert(node->OperIs(GT_ADD, GT_SUB));
#else
        assert(node->OperIs(GT_ADD, GT_SUB, GT_ADD_HI, GT_SUB_HI));
#endif
        noway_assert(!varTypeIsSmall(node->GetType()));

        genJumpToThrowHlpBlk(node->IsUnsigned() ? EJ_jb : EJ_jo, ThrowHelperKind::Overflow);
    }

    DefReg(node);
}

void CodeGen::GenFloatAbs(GenTreeIntrinsic* node)
{
    assert((node->GetIntrinsic() == NI_System_Math_Abs) && varTypeIsFloating(node->GetType()));
    assert(node->GetOp(0)->GetType() == node->GetType());
    assert(node->GetRegNum() != REG_NA);

    CORINFO_FIELD_HANDLE& maskField = node->TypeIs(TYP_FLOAT) ? absBitmaskFlt : absBitmaskDbl;

    if (maskField == nullptr)
    {
        uint64_t mask = node->TypeIs(TYP_FLOAT) ? 0x7fffffff7fffffffUL : 0x7fffffffffffffffUL;
        uint64_t maskPack[]{mask, mask};

        maskField = GetEmitter()->emitBlkConst(&maskPack, 16, 16, node->GetType());
    }

    regNumber dstReg = node->GetRegNum();
    regNumber srcReg = UseReg(node->GetOp(0));

    // TODO-MIKE-Review: The size should be EA_16BYTE but using that results in diffs!?!
    GetEmitter()->emitIns_SIMD_R_R_C(INS_andps, emitTypeSize(node->GetType()), dstReg, srcReg, maskField);
}

void CodeGen::GenFloatNegate(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_FNEG) && varTypeIsFloating(node->GetType()));
    assert(node->GetOp(0)->GetType() == node->GetType());
    assert(node->GetRegNum() != REG_NA);

    CORINFO_FIELD_HANDLE& maskField = node->TypeIs(TYP_FLOAT) ? negBitmaskFlt : negBitmaskDbl;

    if (maskField == nullptr)
    {
        uint64_t mask = node->TypeIs(TYP_FLOAT) ? 0x8000000080000000UL : 0x8000000000000000UL;
        uint64_t maskPack[]{mask, mask};

        maskField = GetEmitter()->emitBlkConst(&maskPack, 16, 16, node->GetType());
    }

    regNumber dstReg = node->GetRegNum();
    regNumber srcReg = UseReg(node->GetOp(0));

    // TODO-MIKE-Review: The size should be EA_16BYTE but using that results in diffs!?!
    GetEmitter()->emitIns_SIMD_R_R_C(INS_xorps, emitTypeSize(node->GetType()), dstReg, srcReg, maskField);

    DefReg(node);
}

void CodeGen::GenFloatBinaryOp(GenTreeOp* node)
{
    assert(node->OperIs(GT_FADD, GT_FSUB, GT_FMUL, GT_FDIV) && varTypeIsFloating(node->GetType()));
    assert((node->GetOp(0)->GetType() == node->GetType()) && (node->GetOp(1)->GetType() == node->GetType()));
    assert(node->GetRegNum() != REG_NA);

    static_assert_no_msg(TYP_DOUBLE - TYP_FLOAT == 1);
    static_assert_no_msg(GT_FSUB - GT_FADD == 1);
    static_assert_no_msg(GT_FMUL - GT_FADD == 2);
    static_assert_no_msg(GT_FDIV - GT_FADD == 3);
    static constexpr instruction insMap[4][2]{{INS_addss, INS_addsd},
                                              {INS_subss, INS_subsd},
                                              {INS_mulss, INS_mulsd},
                                              {INS_divss, INS_divsd}};

    instruction ins = insMap[node->GetOper() - GT_FADD][node->GetType() - TYP_FLOAT];

    GenTree* op1 = node->GetOp(0);
    GenTree* op2 = node->GetOp(1);

    regNumber op1Reg;

    if (op1->isUsedFromReg())
    {
        op1Reg = UseReg(op1);
        genConsumeRegs(op2);
    }
    else
    {
        assert(node->OperIs(GT_FADD, GT_FSUB, GT_FMUL));
        assert(op2->isUsedFromReg());

        genConsumeRegs(op1);
        op1Reg = UseReg(op2);
        op2    = op1;
    }

    inst_RV_RV_TT(ins, emitTypeSize(node->GetType()), node->GetRegNum(), op1Reg, op2, !compiler->canUseVexEncoding());

    DefReg(node);
}

void CodeGen::GenMul(GenTreeOp* mul)
{
    assert(mul->OperIs(GT_MUL) && varTypeIsIntOrI(mul->GetType()));

    emitAttr  size          = emitTypeSize(mul->GetType());
    bool      checkOverflow = mul->gtOverflowEx();
    regNumber dstReg        = mul->GetRegNum();
    GenTree*  op1           = mul->GetOp(0);
    GenTree*  op2           = mul->GetOp(1);

    genConsumeRegs(op1);

    if (GenTreeIntCon* immOp = op2->IsContainedIntCon())
    {
        ssize_t imm = immOp->GetValue();

        if (!checkOverflow && op1->isUsedFromReg() && ((imm == 3) || (imm == 5) || (imm == 9)))
        {
            unsigned scale = static_cast<unsigned>(imm - 1);
            GetEmitter()->emitIns_R_ARX(INS_lea, size, dstReg, op1->GetRegNum(), op1->GetRegNum(), scale, 0);
        }
        else if (!checkOverflow && op1->isUsedFromReg() && (imm != 0) && (imm == genFindLowestBit(imm)))
        {
            GetEmitter()->emitIns_Mov(INS_mov, size, dstReg, op1->GetRegNum(), /* canSkip */ true);
            inst_RV_SH(INS_shl, size, dstReg, genLog2(static_cast<uint64_t>(static_cast<size_t>(imm))));
        }
        else
        {
            emitInsBinary(GetEmitter()->inst3opImulForReg(dstReg), size, op1, op2);
        }
    }
    else
    {
        genConsumeRegs(op2);

        instruction ins       = INS_imul;
        regNumber   mulDstReg = dstReg;
        GenTree*    regOp     = op1;
        GenTree*    rmOp      = op2;

        if (checkOverflow && mul->IsUnsigned())
        {
            ins       = INS_mulEAX;
            mulDstReg = REG_RAX;
        }

        if (op1->isUsedFromMemory() || (op2->isUsedFromReg() && (op2->GetRegNum() == mulDstReg)))
        {
            std::swap(regOp, rmOp);
        }

        assert(regOp->isUsedFromReg());

        GetEmitter()->emitIns_Mov(INS_mov, size, mulDstReg, regOp->GetRegNum(), /* canSkip */ true);

        if (ins == INS_mulEAX)
        {
            emitInsUnary(ins, size, rmOp);
            GetEmitter()->emitIns_Mov(INS_mov, size, dstReg, REG_RAX, /* canSkip */ true);
        }
        else
        {
            emitInsBinary(ins, size, mul, rmOp);
        }
    }

    if (checkOverflow)
    {
        genJumpToThrowHlpBlk(mul->IsUnsigned() ? EJ_jb : EJ_jo, ThrowHelperKind::Overflow);
    }

    DefReg(mul);
}

#ifdef TARGET_X86

void CodeGen::genFloatReturn(GenTree* src)
{
    assert(varTypeIsFloating(src->GetType()));

    var_types srcType = src->GetType();
    emitAttr  srcSize = emitTypeSize(srcType);
    regNumber srcReg  = UseReg(src);

    // Spill the return shift register from an XMM register to the stack, then load it on the x87 stack.
    // If it already has a home location, use that. Otherwise, we need a temp.

    if (IsRegCandidateLclVar(src) && compiler->lvaGetDesc(src->AsLclVar())->lvOnFrame)
    {
        if (compiler->lvaGetDesc(src->AsLclVar())->GetRegNum() != REG_STK)
        {
            // TODO-MIKE-Review: This seems pointless, it's not like we're going to unspill it later...
            src->SetRegSpill(0, true);
            GetEmitter()->emitIns_S_R(ins_Store(srcType), srcSize, srcReg, src->AsLclVar()->GetLclNum(), 0);
        }

        GetEmitter()->emitIns_S(INS_fld, srcSize, src->AsLclVar()->GetLclNum(), 0);
    }
    else
    {
        // Spill the shift, which should be in a register, then load it to the fp stack.
        // TODO-X86-CQ: Deal with things that are already spilled or in memory.

        src->SetRegSpill(0, true);
        SpillNodeReg(src, srcType, 0);
        UnspillST0(src);
    }
}

#endif // TARGET_X86

void CodeGen::GenCompare(GenTreeOp* cmp)
{
    assert(cmp->OperIs(GT_EQ, GT_NE, GT_LT, GT_LE, GT_GE, GT_GT, GT_TEST_EQ, GT_TEST_NE, GT_CMP));

    // TODO-XArch-CQ: Check if we can use the currently set flags.
    // TODO-XArch-CQ: Check for the case where we can simply transfer the carry bit to a register
    //         (signed < or >= where targetReg != REG_NA)

    if (varTypeIsFloating(cmp->GetOp(0)->GetType()))
    {
        GenFloatCompare(cmp);
    }
    else
    {
        GenIntCompare(cmp);
    }
}

void CodeGen::genCodeForBT(GenTreeOp* bt)
{
    assert(bt->OperIs(GT_BT));

    GenTree*  op1  = bt->GetOp(0);
    GenTree*  op2  = bt->GetOp(1);
    var_types type = varActualType(op1->GetType());

    assert(op1->isUsedFromReg() && op2->isUsedFromReg());
    assert((varTypeSize(type) >= varTypeSize(TYP_INT)) && (varTypeSize(type) <= varTypeSize(TYP_I_IMPL)));

    regNumber srcReg1 = UseReg(op1);
    regNumber srcReg2 = UseReg(op2);

    // Note that the emitter doesn't fully support INS_bt, it only supports the reg,reg
    // form and encodes the registers in reverse order. To get the correct order we need
    // to reverse the operands when calling emitIns_R_R.
    GetEmitter()->emitIns_R_R(INS_bt, emitTypeSize(type), srcReg2, srcReg1);
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

void CodeGen::genCodeForReturnTrap(GenTreeOp* tree)
{
    assert(tree->OperIs(GT_RETURNTRAP));

    GenTreeIndir* mem = tree->GetOp(0)->AsIndir();
    assert(mem->isContained());
    genConsumeAddress(mem->GetAddr());
    GetEmitter()->emitIns_A_I(INS_cmp, EA_4BYTE, mem->GetAddr(), 0);

    BasicBlock* skipLabel = genCreateTempLabel();
    inst_JMP(EJ_je, skipLabel);

    regNumber tmpReg = tree->GetSingleTempReg(RBM_ALLINT);
    assert(genIsValidIntReg(tmpReg));
    genEmitHelperCall(CORINFO_HELP_STOP_FOR_GC, EA_UNKNOWN, tmpReg);
    genDefineTempLabel(skipLabel);
}

void CodeGen::GenNode(GenTree* treeNode, BasicBlock* block)
{
    emitter* emit = GetEmitter();

#ifdef DEBUG
    // Validate that all the operands for the current node are consumed in order.
    // This is important because LSRA ensures that any necessary copies will be
    // handled correctly.
    lastConsumedNode = nullptr;
    if (compiler->verbose)
    {
        compiler->gtDispLIRNode(treeNode);
    }
#endif // DEBUG

    // Is this a node whose shift is already in a register?  LSRA denotes this by
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
            liveness.RemoveGCRegs(RBM_INT_CALLEE_SAVED);
            genDefineTempLabel(genCreateTempLabel());
            break;

        case GT_PROF_HOOK:
#ifdef PROFILING_SUPPORTED
            // We should be seeing this only if profiler hook is needed
            noway_assert(compiler->compIsProfilerHookNeeded());

            // Right now this node is used only for tail calls. In future if
            // we intend to use it for Enter or Leave hooks, add a shift member
            // to this node indicating the kind of profiler hook. For example,
            // helper number can be used.
            genProfilingLeaveCallback(CORINFO_HELP_PROF_FCN_TAILCALL);
#endif // PROFILING_SUPPORTED
            break;

        case GT_LCLHEAP:
            genLclHeap(treeNode);
            break;

        case GT_CNS_INT:
#ifdef WINDOWS_X86_ABI
            assert(!treeNode->IsIntCon(HandleKind::TLS));
#endif
            GenIntCon(treeNode->AsIntCon(), treeNode->GetRegNum(), treeNode->GetType());
            DefReg(treeNode);
            break;

        case GT_CNS_DBL:
            GenDblCon(treeNode->AsDblCon(), treeNode->GetRegNum(), treeNode->GetType());
            DefReg(treeNode);
            break;

        case GT_FNEG:
            GenFloatNegate(treeNode->AsUnOp());
            break;

        case GT_FADD:
        case GT_FSUB:
        case GT_FMUL:
        case GT_FDIV:
            GenFloatBinaryOp(treeNode->AsOp());
            break;

        case GT_NOT:
        case GT_NEG:
            genCodeForNegNot(treeNode->AsUnOp());
            break;

        case GT_BSWAP:
        case GT_BSWAP16:
            genCodeForBswap(treeNode);
            break;

        case GT_MUL:
            GenMul(treeNode->AsOp());
            break;

        case GT_DIV:
        case GT_MOD:
        case GT_UMOD:
        case GT_UDIV:
            GenDivMod(treeNode->AsOp());
            break;

        case GT_ADD:
        case GT_SUB:
        case GT_OR:
        case GT_XOR:
        case GT_AND:
#ifndef TARGET_64BIT
        case GT_ADD_LO:
        case GT_ADD_HI:
        case GT_SUB_LO:
        case GT_SUB_HI:
#endif
            genCodeForBinary(treeNode->AsOp());
            break;

        case GT_LSH:
        case GT_RSH:
        case GT_RSZ:
        case GT_ROL:
        case GT_ROR:
            genCodeForShift(treeNode->AsOp());
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

        case GT_LCL_ADDR:
            GenLclAddr(treeNode->AsLclAddr());
            break;

        case GT_LCL_FLD:
            genCodeForLclFld(treeNode->AsLclFld());
            break;

        case GT_LCL_VAR:
            GenLoadLclVar(treeNode->AsLclVar());
            break;

        case GT_STORE_LCL_FLD:
            GenStoreLclFld(treeNode->AsLclFld());
            break;

        case GT_STORE_LCL_VAR:
            GenStoreLclVar(treeNode->AsLclVar());
            break;

        case GT_RETFILT:
            GenRetFilt(treeNode, block);
            break;

        case GT_RETURN:
            GenReturn(treeNode, block);
            break;

        case GT_LEA:
            genLeaInstruction(treeNode->AsAddrMode());
            break;

        case GT_INDEX_ADDR:
            genCodeForIndexAddr(treeNode->AsIndexAddr());
            break;

        case GT_IND:
            GenIndLoad(treeNode->AsIndir());
            break;

        case GT_INC_SATURATE:
            genCodeForIncSaturate(treeNode);
            break;

        case GT_MULHI:
#ifdef TARGET_X86
        case GT_MUL_LONG:
#endif
            GenMulLong(treeNode->AsOp());
            break;

        case GT_INTRINSIC:
            genIntrinsic(treeNode->AsIntrinsic());
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
            GenCompare(treeNode->AsOp());
            break;

        case GT_JTRUE:
            GenJTrue(treeNode->AsUnOp(), block);
            break;

        case GT_JCC:
            GenJCC(treeNode->AsCC(), block);
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
            GenIndStore(treeNode->AsStoreInd());
            break;

        case GT_RELOAD:
        case GT_COPY:
            // These are handled by genConsumeReg
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
            GenJmp(treeNode);
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
            GenMemoryBarrier(treeNode);
            break;

        case GT_CMPXCHG:
            genCodeForCmpXchg(treeNode->AsCmpXchg());
            break;

        case GT_NOP:
            break;

        case GT_KEEPALIVE:
            // TODO-MIKE-Review: Huh, why is this completely different from ARM?!
            genConsumeRegs(treeNode->AsUnOp()->GetOp(0));
            break;

        case GT_NO_OP:
            GetEmitter()->emitIns_Nop(1);
            break;

        case GT_BOUNDS_CHECK:
            genRangeCheck(treeNode->AsBoundsChk());
            break;

        case GT_PHYSREG:
            genCodeForPhysReg(treeNode->AsPhysReg());
            break;

        case GT_NULLCHECK:
            genCodeForNullCheck(treeNode->AsIndir());
            break;

        case GT_CATCH_ARG:
            noway_assert(handlerGetsXcptnObj(block->bbCatchTyp));
            // Catch arguments get passed in a register. genCodeForBBlist()
            // would have marked it as holding a GC object, but not used.
            noway_assert((liveness.GetGCRegs(TYP_REF) & RBM_EXCEPTION_OBJECT) != RBM_NONE);

            UseReg(treeNode);
            break;

#ifndef FEATURE_EH_FUNCLETS
        case GT_END_LFIN:
        {
            // Have to clear the ShadowSP of the nesting level which encloses the finally. Generates:
            //     mov dword ptr [ebp-0xC], 0  // for some slot of the ShadowSP local var

            unsigned finallyNesting = treeNode->AsEndLFin()->GetNesting();
            noway_assert(finallyNesting < compiler->compHndBBtabCount);

            unsigned   shadowSPSlotsLclNum = compiler->lvaShadowSPslotsVar;
            LclVarDsc* shadowSPSlotsLcl    = compiler->lvaGetDesc(shadowSPSlotsLclNum);

            // The last slot is reserved for ICodeManager::FixContext(ppEndRegion)
            assert(shadowSPSlotsLcl->GetBlockSize() > REGSIZE_BYTES);
            unsigned filterEndOffsetSlotOffs = shadowSPSlotsLcl->GetBlockSize() - REGSIZE_BYTES;

            unsigned curNestingSlotOffs = filterEndOffsetSlotOffs - ((finallyNesting + 1) * REGSIZE_BYTES);
            GetEmitter()->emitIns_S_I(INS_mov, EA_PTRSIZE, shadowSPSlotsLclNum, curNestingSlotOffs, 0);
        }
        break;
#endif // !FEATURE_EH_FUNCLETS

        case GT_PINVOKE_PROLOG:
            noway_assert((liveness.GetGCRegs() & ~fullIntArgRegMask()) == 0);
#ifdef PSEUDORANDOM_NOP_INSERTION
            // the runtime side requires the codegen here to be consistent
            emit->emitDisableRandomNops();
#endif
            break;

        case GT_LABEL:
            genPendingCallLabel = genCreateTempLabel();
            emit->emitIns_R_L(INS_lea, EA_PTR_DSP_RELOC, genPendingCallLabel, treeNode->GetRegNum());
            // TODO-MIKE-Review: Hmm, no DefReg call?
            break;

        case GT_STORE_OBJ:
        case GT_STORE_BLK:
            GenStructStore(treeNode->AsBlk(), treeNode->AsBlk()->GetKind(), treeNode->AsBlk()->GetLayout());
            break;

        case GT_COPY_BLK:
        case GT_INIT_BLK:
            GenDynBlk(treeNode->AsDynBlk());
            break;

        case GT_JMPTABLE:
            GenJmpTable(treeNode, block);
            break;

        case GT_SWITCH_TABLE:
            genTableBasedSwitch(treeNode->AsOp());
            break;

        case GT_ARR_INDEX:
            genCodeForArrIndex(treeNode->AsArrIndex());
            break;

        case GT_ARR_OFFSET:
            genCodeForArrOffset(treeNode->AsArrOffs());
            break;

        case GT_CLS_VAR_ADDR:
            emit->emitIns_R_C(INS_lea, EA_PTRSIZE, treeNode->GetRegNum(), treeNode->AsClsVar()->GetFieldHandle());
            DefReg(treeNode);
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

// Probe the stack and allocate the local stack frame - subtract from SP.
//
// frameSize         - the size of the stack frame being allocated.
// initReg           - register to use as a scratch register.
// pInitRegZeroed    - OUT parameter. *pInitRegZeroed is set to 'false' if and only if
//                          this call sets 'initReg' to a non-zero shift.
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
        genEmitHelperCall(CORINFO_HELP_STACK_PROBE);

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
        genEmitHelperCall(CORINFO_HELP_STACK_PROBE);

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
    if (!IsFramePointerRequired())
    {
        psiAdjustStackLevel(frameSize);
    }
#endif // USING_SCOPE_INFO
}

void CodeGen::PrologEstablishFramePointer(int delta, bool reportUnwindData)
{
    if (delta == 0)
    {
        GetEmitter()->emitIns_Mov(INS_mov, EA_PTRSIZE, REG_FPBASE, REG_ESP, /* canSkip */ false);

#ifdef USING_SCOPE_INFO
        psiMoveESPtoEBP();
#endif
    }
    else
    {
        GetEmitter()->emitIns_R_AR(INS_lea, EA_PTRSIZE, REG_FPBASE, REG_ESP, delta);

        // We don't update prolog scope info (there is no function to handle lea),
        // but that is currently dead code anyway.
    }

    if (reportUnwindData)
    {
        compiler->unwindSetFrameReg(REG_FPBASE, delta);
    }
}

// Add a specified constant shift to the stack pointer. No probing is done.
//
// spDelta - the shift to add to SP. Must be negative or zero.
// regTmp  - x86 only: an available temporary register. If not REG_NA, hide the SP
//           adjustment from the emitter, using this register.
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

// Add a specified constant shift to the stack pointer, and probe the stack as appropriate.
// Should only be called as a helper for genStackPointerConstantAdjustmentLoopWithProbe.
//
// spDelta - the shift to add to SP. Must be negative or zero. If zero, the probe happens,
//           but the stack pointer doesn't move.
// regTmp  - x86 only: an available temporary register. If not REG_NA, hide the SP
//           adjustment from the emitter, using this register.
//
void CodeGen::genStackPointerConstantAdjustmentWithProbe(ssize_t spDelta, regNumber regTmp)
{
    GetEmitter()->emitIns_AR_R(INS_test, EA_4BYTE, REG_SPBASE, REG_SPBASE, 0);
    genStackPointerConstantAdjustment(spDelta, regTmp);
}

// Add a specified constant shift to the stack pointer, and probe the stack as appropriate.
// Generates one probe per page, up to the total amount required. This will generate a sequence
// of probes in-line. It is required for the case where we need to expose (not hide) the stack
// level adjustment. We can't use the dynamic loop in that case, because the total stack adjustment
// would not be visible to the emitter. It would be possible to use this version for multiple hidden
// constant stack level adjustments but we don't do that currently (we use the loop version in
// genStackPointerDynamicAdjustmentWithProbe instead).
//
// spDelta - the shift to add to SP. Must be negative.
// regTmp  - x86 only: an available temporary register. If not REG_NA, hide the SP
//           adjustment from the emitter, using this register.
//
// Returns the offset in bytes from SP to last probed address.
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

// Add a register shift to the stack pointer, and probe the stack as appropriate.
// Note that for x86, we hide the ESP adjustment from the emitter. To do that, currently,
// requires a temporary register and extra code.
//
// regSpDelta - the register shift to add to SP. The shift in this register must be negative.
//              This register might be trashed.
// regTmp       - an available temporary register. Will be trashed.
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

    GetEmitter()->emitIns_R_R(INS_xor, EA_4BYTE, regSpDelta, regSpDelta);

    genDefineTempLabel(loop);

    // Tickle the decremented shift. Note that it must be done BEFORE the update of ESP since ESP might already
    // be on the guard page. It is OK to leave the final shift of ESP on the guard page.
    GetEmitter()->emitIns_AR_R(INS_test, EA_4BYTE, REG_SPBASE, REG_SPBASE, 0);

    // Subtract a page from ESP. This is a trick to avoid the emitter trying to track the
    // decrement of the ESP - we do the subtraction in another reg instead of adjusting ESP directly.
    inst_Mov(TYP_I_IMPL, regTmp, REG_SPBASE, /* canSkip */ false);
    inst_RV_IV(INS_sub, regTmp, compiler->eeGetPageSize(), EA_PTRSIZE);
    inst_Mov(TYP_I_IMPL, REG_SPBASE, regTmp, /* canSkip */ false);

    inst_RV_RV(INS_cmp, REG_SPBASE, regSpDelta, TYP_I_IMPL);
    inst_JMP(EJ_jae, loop);

    // Move the final shift to ESP
    inst_Mov(TYP_I_IMPL, REG_SPBASE, regSpDelta, /* canSkip */ false);
}

void CodeGen::genLclHeap(GenTree* tree)
{
    assert(tree->OperIs(GT_LCLHEAP));
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
    if (compiler->lvaReturnSpCheck != BAD_VAR_NUM)
    {
        genStackPointerCheck(compiler->lvaReturnSpCheck);
    }
#endif

    // Note that for x86, we don't track ESP movements while generating the localloc code.
    // The ESP tracking is used to report stack pointer-relative GC info, which is not
    // interesting while doing the localloc construction. Also, for functions with localloc,
    // we have EBP frames, and EBP-relative locals, and ESP-relative accesses only for function
    // call arguments.
    //
    // For x86, we store the ESP after the localloc is complete in the LocAllocSP
    // variable. This variable is implicitly reported to the VM in the GC info (its position
    // is defined by convention relative to other items), and is used by the GC to find the
    // "base" stack pointer in functions with localloc.

    noway_assert(isFramePointerUsed()); // localloc requires Frame Pointer to be established since SP changes
#if !FEATURE_FIXED_OUT_ARGS
    noway_assert(genStackLevel == 0); // Can't have anything on the stack
#endif

    target_size_t stackAdjustment     = 0;
    target_size_t locAllocStackOffset = 0;

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
        // The localloc requested memory size is non-constant.

        // Put the size shift in targetReg. If it is zero, bail out by returning null in targetReg.
        genConsumeReg(size);
        genCopyRegIfNeeded(size, targetReg);
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

    bool initMemOrLargeAlloc; // Declaration must be separate from initialization to avoid clang compiler error.
    initMemOrLargeAlloc = compiler->info.compInitMem || (amount >= compiler->eeGetPageSize()); // must be >= not >

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
    if (outgoingArgSpaceSize > 0)
    {
        // This must be true for the stack to remain aligned
        assert(outgoingArgSpaceSize % STACK_ALIGN == 0);

        // If the localloc amount is a small enough constant, and we're not initializing the allocated
        // memory, then don't bother popping off the ougoing arg space first; just allocate the amount
        // of space needed by the allocation, and call the bottom part the new outgoing arg space.

        if ((amount > 0) && !initMemOrLargeAlloc)
        {
            lastTouchDelta      = genStackPointerConstantAdjustmentLoopWithProbe(-(ssize_t)amount, REG_NA);
            stackAdjustment     = 0;
            locAllocStackOffset = static_cast<target_size_t>(outgoingArgSpaceSize);
            goto ALLOC_DONE;
        }

        inst_RV_IV(INS_add, REG_SPBASE, outgoingArgSpaceSize, EA_PTRSIZE);
        stackAdjustment += static_cast<target_size_t>(outgoingArgSpaceSize);
        locAllocStackOffset = stackAdjustment;
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
                GetEmitter()->emitIns_I(INS_push_hide, EA_PTRSIZE, 0);
            }

            lastTouchDelta = 0;

            goto ALLOC_DONE;
        }

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

        GetEmitter()->emitIns_R_I(INS_mov, AMD64_ONLY(amount > UINT32_MAX ? EA_8BYTE :) EA_4BYTE, regCnt,
                                  static_cast<ssize_t>(amount));
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
            GetEmitter()->emitIns_I(INS_push_hide, EA_PTRSIZE, 0);
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
        // Negate this shift before calling the function to adjust the stack (which
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
            (stackAdjustment + (target_size_t)lastTouchDelta + STACK_PROBE_BOUNDARY_THRESHOLD_BYTES >
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
    // TargetReg = RSP + locAllocStackOffset
    GetEmitter()->emitIns_R_AR(INS_lea, EA_PTRSIZE, targetReg, REG_SPBASE, (int)locAllocStackOffset);

    if (endLabel != nullptr)
    {
        genDefineTempLabel(endLabel);
    }

#ifdef JIT32_GCENCODER
    if (compiler->lvaLocAllocSPvar != BAD_VAR_NUM)
    {
        GetEmitter()->emitIns_S_R(ins_Store(TYP_I_IMPL), EA_PTRSIZE, REG_SPBASE, compiler->lvaLocAllocSPvar, 0);
    }
#endif // JIT32_GCENCODER

#ifdef DEBUG
    // Update local variable to reflect the new stack pointer.
    if (compiler->lvaReturnSpCheck != BAD_VAR_NUM)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(compiler->lvaReturnSpCheck);
        assert(lcl->lvOnFrame && lcl->lvDoNotEnregister);
        GetEmitter()->emitIns_S_R(INS_mov, EA_PTRSIZE, REG_SPBASE, compiler->lvaReturnSpCheck, 0);
    }
#endif

    genProduceReg(tree);
}

void CodeGen::GenDynBlk(GenTreeDynBlk* store)
{
    switch (store->GetKind())
    {
#ifdef TARGET_AMD64
        case StructStoreKind::MemSet:
            ConsumeDynBlk(store, REG_ARG_0, REG_ARG_1, REG_ARG_2);
            genEmitHelperCall(CORINFO_HELP_MEMSET);
            break;
        case StructStoreKind::MemCpy:
            ConsumeDynBlk(store, REG_ARG_0, REG_ARG_1, REG_ARG_2);
            genEmitHelperCall(CORINFO_HELP_MEMCPY);
            break;
#endif
        case StructStoreKind::RepStos:
            ConsumeDynBlk(store, REG_RDI, REG_RAX, REG_RCX);
            instGen(INS_r_stosb);
            break;
        case StructStoreKind::RepMovs:
            ConsumeDynBlk(store, REG_RDI, REG_RSI, REG_RCX);
            instGen(INS_r_movsb);
            break;
        default:
            unreached();
    }
}

StructStoreKind GetStructStoreKind(bool isLocalStore, ClassLayout* layout, GenTree* src)
{
    assert(!layout->IsBlockLayout());

    if (varTypeIsStruct(src->GetType()) && src->IsCall())
    {
#if defined(UNIX_AMD64_ABI) || defined(TARGET_ARM64)
        return isLocalStore || !layout->HasGCRef() ? StructStoreKind::UnrollRegs : StructStoreKind::UnrollRegsWB;
#else
        assert(isLocalStore);
        return StructStoreKind::UnrollRegs;
#endif
    }

    unsigned size = layout->GetSize();

    if (src->OperIs(GT_CNS_INT))
    {
        assert(src->IsIntegralConst(0));

        return size > INITBLK_UNROLL_LIMIT ? StructStoreKind::LargeInit : StructStoreKind::UnrollInit;
    }

    // If the struct contains GC pointers we need to generate GC write barriers, unless
    // the destination is a local variable. Even if the destination is a local we're still
    // going to use UnrollWB if the size is too large for normal unrolling.
    // Normal unrolling requires GC non-interruptible regions, the JIT32 GC encoder does
    // not support that.

    if (layout->HasGCPtr()
#ifndef JIT32_GCENCODER
        && (!isLocalStore || (size > CPBLK_UNROLL_LIMIT))
#endif
            )
    {
        // If we have a long enough sequence of slots that do not require write barriers then
        // we can use REP MOVSD/Q instead of a sequence of MOVSD/Q instructions. According to the
        // Intel Manual, the sweet spot for small structs is between 4 to 12 slots of size where
        // the entire operation takes 20 cycles and encodes in 5 bytes (loading RCX and REP MOVSD/Q).
        unsigned nonWBSequenceLength = 0;

        if (isLocalStore)
        {
            // If the destination is on the stack then no write barriers are needed.
            nonWBSequenceLength = layout->GetSlotCount();
        }
        else
        {
            // Otherwise a write barrier is needed for every GC pointer in the layout
            // so we need to check if there's a long enough sequence of non-GC slots.
            for (unsigned i = 0; i < layout->GetSlotCount(); i++)
            {
                if (layout->IsGCPtr(i))
                {
                    nonWBSequenceLength = 0;
                }
                else
                {
                    nonWBSequenceLength++;

                    if (nonWBSequenceLength >= CPOBJ_NONGC_SLOTS_LIMIT)
                    {
                        break;
                    }
                }
            }
        }

        return nonWBSequenceLength >= CPOBJ_NONGC_SLOTS_LIMIT ? StructStoreKind::UnrollCopyWBRepMovs
                                                              : StructStoreKind::UnrollCopyWB;
    }

    return size > CPBLK_UNROLL_LIMIT ? StructStoreKind::LargeCopy : StructStoreKind::UnrollCopy;
}

void CodeGen::GenStructStore(GenTree* store, StructStoreKind kind, ClassLayout* layout)
{
    assert(store->OperIs(GT_STORE_OBJ, GT_STORE_BLK, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    switch (kind)
    {
#ifdef TARGET_AMD64
        case StructStoreKind::MemSet:
            GenStructStoreMemSet(store, layout);
            break;
        case StructStoreKind::MemCpy:
            GenStructStoreMemCpy(store, layout);
            break;
#endif
        case StructStoreKind::RepStos:
            GenStructStoreRepStos(store, layout);
            break;
        case StructStoreKind::RepMovs:
            GenStructStoreRepMovs(store, layout);
            break;
        case StructStoreKind::UnrollInit:
            GenStructStoreUnrollInit(store, layout);
            break;
        case StructStoreKind::UnrollCopy:
            GenStructStoreUnrollCopy(store, layout);
            break;
        case StructStoreKind::UnrollCopyWB:
        case StructStoreKind::UnrollCopyWBRepMovs:
            GenStructStoreUnrollCopyWB(store, layout);
            break;
        case StructStoreKind::UnrollRegs:
            GenStructStoreUnrollRegs(store, layout);
            break;
#ifdef UNIX_AMD64_ABI
        case StructStoreKind::UnrollRegsWB:
            GenStructStoreUnrollRegsWB(store->AsObj());
            break;
#endif
        default:
            unreached();
    }
}

#ifdef TARGET_AMD64

void CodeGen::GenStructStoreMemSet(GenTree* store, ClassLayout* layout)
{
    ConsumeStructStore(store, layout, REG_ARG_0, REG_ARG_1, REG_ARG_2);
    genEmitHelperCall(CORINFO_HELP_MEMSET);
}

void CodeGen::GenStructStoreMemCpy(GenTree* store, ClassLayout* layout)
{
    assert(!layout->HasGCPtr());

    ConsumeStructStore(store, layout, REG_ARG_0, REG_ARG_1, REG_ARG_2);
    genEmitHelperCall(CORINFO_HELP_MEMCPY);
}

#endif // TARGET_AMD64

void CodeGen::GenStructStoreRepStos(GenTree* store, ClassLayout* layout)
{
    ConsumeStructStore(store, layout, REG_RDI, REG_RAX, REG_RCX);
    instGen(INS_r_stosb);
}

void CodeGen::GenStructStoreRepMovs(GenTree* store, ClassLayout* layout)
{
    assert(!layout->HasGCPtr());

    ConsumeStructStore(store, layout, REG_RDI, REG_RSI, REG_RCX);
    instGen(INS_r_movsb);
}

void CodeGen::GenStructStoreUnrollInit(GenTree* store, ClassLayout* layout)
{
    assert(store->OperIs(GT_STORE_BLK, GT_STORE_OBJ, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    unsigned  dstLclNum         = BAD_VAR_NUM;
    regNumber dstAddrBaseReg    = REG_NA;
    regNumber dstAddrIndexReg   = REG_NA;
    unsigned  dstAddrIndexScale = 1;
    int       dstOffset         = 0;
    GenTree*  src;

    if (store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
    {
        dstLclNum = store->AsLclVarCommon()->GetLclNum();
        dstOffset = store->AsLclVarCommon()->GetLclOffs();

        src = store->AsLclVarCommon()->GetOp(0);
    }
    else
    {
        GenTree* dstAddr = store->AsIndir()->GetAddr();

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
            dstLclNum = dstAddr->AsLclAddr()->GetLclNum();
            dstOffset = dstAddr->AsLclAddr()->GetLclOffs();
        }

        src = store->AsIndir()->GetValue();
    }

    regNumber srcIntReg = REG_NA;

    if (src->OperIs(GT_INIT_VAL))
    {
        assert(src->isContained());
        src = src->AsUnOp()->GetOp(0);
    }

    unsigned size = layout->GetSize();

    if (!src->isContained())
    {
        srcIntReg = genConsumeReg(src);
    }
    else
    {
        assert(src->IsIntegralConst(0));
    }

    emitter* emit = GetEmitter();

    assert(size <= INT32_MAX);
    assert(dstOffset < (INT32_MAX - static_cast<int>(size)));

    if (size == 1)
    {
        if (dstLclNum != BAD_VAR_NUM)
        {
            emit->emitIns_S_I(INS_mov, EA_1BYTE, dstLclNum, dstOffset, 0);
        }
        else
        {
            emit->emitIns_ARX_I(INS_mov, EA_1BYTE, dstAddrBaseReg, dstAddrIndexReg, dstAddrIndexScale, dstOffset, 0);
        }

        return;
    }

    // 16 byte SSE stores are not guaranteed to be atomic so if the struct contains
    // GC pointers we need to use only 8 byte stores.
    // On x64 that means not using SSE at all since 8 byte stores can be done using
    // normal 8 byte MOVs, which have smaller instruction encoding.
    // On x86 we can still use SSE's MOVQ to store 8 bytes instead of two 4 byte MOVs.

    if ((size >= XMM_REGSIZE_BYTES)
#ifdef TARGET_AMD64
        && (!store->IsObj() || !layout->HasGCPtr())
#endif
            )
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
        unsigned    regSize = XMM_REGSIZE_BYTES;
        unsigned    minSize = XMM_REGSIZE_BYTES;

#ifdef TARGET_X86
        minSize = 8;

        if (store->IsObj() && layout->HasGCPtr())
        {
            simdMov = INS_movq;
            regSize = 8;
        }
#endif

        for (; size >= minSize; size -= regSize, dstOffset += regSize)
        {
#ifdef TARGET_X86
            if (regSize > size)
            {
                simdMov = INS_movq;
                regSize = 8;
            }
#endif

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

void CodeGen::GenStructStoreUnrollCopy(GenTree* store, ClassLayout* layout)
{
    assert(store->OperIs(GT_STORE_BLK, GT_STORE_OBJ, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    if (layout->HasGCPtr())
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
    GenTree*  src;

    if (store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
    {
        dstLclNum = store->AsLclVarCommon()->GetLclNum();
        dstOffset = store->AsLclVarCommon()->GetLclOffs();

        src = store->AsLclVarCommon()->GetOp(0);
    }
    else
    {
        GenTree* dstAddr = store->AsIndir()->GetAddr();

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
            dstLclNum = dstAddr->AsLclAddr()->GetLclNum();
            dstOffset = dstAddr->AsLclAddr()->GetLclOffs();
        }

        src = store->AsIndir()->GetValue();
    }

    unsigned  srcLclNum         = BAD_VAR_NUM;
    regNumber srcAddrBaseReg    = REG_NA;
    regNumber srcAddrIndexReg   = REG_NA;
    unsigned  srcAddrIndexScale = 1;
    int       srcOffset         = 0;

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
            srcLclNum = srcAddr->AsLclAddr()->GetLclNum();
            srcOffset = srcAddr->AsLclAddr()->GetLclOffs();
        }
    }

    emitter* emit = GetEmitter();
    unsigned size = layout->GetSize();

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

    if (layout->HasGCPtr())
    {
#ifndef JIT32_GCENCODER
        GetEmitter()->emitEnableGC();
#else
        unreached();
#endif
    }
}

#ifdef FEATURE_MULTIREG_RET
void CodeGen::GenStructStoreUnrollRegs(GenTree* store, ClassLayout* layout)
{
    unsigned  dstLclNum         = BAD_VAR_NUM;
    regNumber dstAddrBaseReg    = REG_NA;
    regNumber dstAddrIndexReg   = REG_NA;
    unsigned  dstAddrIndexScale = 1;
    int       dstOffset         = 0;
    GenTree*  src;

    if (store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
    {
        dstLclNum = store->AsLclVarCommon()->GetLclNum();
        dstOffset = store->AsLclVarCommon()->GetLclOffs();

        src = store->AsLclVarCommon()->GetOp(0);
    }
    else
    {
        GenTree* dstAddr = store->AsIndir()->GetAddr();

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
            dstLclNum = dstAddr->AsLclAddr()->GetLclNum();
            dstOffset = dstAddr->AsLclAddr()->GetLclOffs();
        }

        src = store->AsObj()->GetValue();
    }

    unsigned size = layout->GetSize();

    assert(size <= INT32_MAX);
    assert(dstOffset < INT32_MAX - static_cast<int>(size));

    GenTreeCall* call     = src->gtSkipReloadOrCopy()->AsCall();
    unsigned     regCount = call->GetRegCount();
    regNumber    regs[MAX_RET_REG_COUNT];
    var_types    regTypes[MAX_RET_REG_COUNT];

    for (unsigned i = 0; i < regCount; i++)
    {
        regs[i] = regCount == 1 ? genConsumeReg(call) : UseReg(call, i);

        var_types regType = call->GetRegType(i);
        unsigned  regSize = varTypeSize(regType);

        assert((i < regCount - 1) ? (regSize == REGSIZE_BYTES) : (regSize <= REGSIZE_BYTES));

        if (!varTypeUsesFloatReg(regType))
        {
            regType = layout->GetGCPtrType(i);
        }

        regTypes[i] = regType;
    }

    emitter*  emit     = GetEmitter();
    unsigned  regIndex = 0;
    regNumber reg      = REG_NA;
    var_types regType  = TYP_UNDEF;
    unsigned  regSize  = 0;

    for (; (regIndex < regCount) && (size > 0); regIndex++, dstOffset += regSize, size -= regSize)
    {
        reg     = regs[regIndex];
        regType = regTypes[regIndex];
        regSize = varTypeSize(regType);

        assert(regSize <= REGSIZE_BYTES);

        if (regSize > size)
        {
            break;
        }

        instruction ins  = ins_Store(regType);
        emitAttr    attr = emitTypeSize(regType);

        if (dstLclNum != BAD_VAR_NUM)
        {
            emit->emitIns_S_R(ins, attr, reg, dstLclNum, dstOffset);
        }
        else
        {
            emit->emitIns_ARX_R(ins, attr, reg, dstAddrBaseReg, dstAddrIndexReg, dstAddrIndexScale, dstOffset);
        }
    }

    if ((regIndex < regCount) && (size > 0))
    {
        assert(varTypeIsIntegral(regType) && (size < REGSIZE_BYTES));

        for (unsigned regShift = 0; size > 0; regShift = regSize, dstOffset += regSize, size -= regSize)
        {
            while (regSize > size)
            {
                regSize /= 2;
            }

            if (regShift != 0)
            {
                emit->emitIns_R_I(INS_shr_N, regShift >= 4 ? EA_8BYTE : EA_4BYTE, reg, regShift * 8);
            }

            emitAttr attr = EA_ATTR(regSize);

            if (dstLclNum != BAD_VAR_NUM)
            {
                emit->emitIns_S_R(INS_mov, attr, reg, dstLclNum, dstOffset);
            }
            else
            {
                emit->emitIns_ARX_R(INS_mov, attr, reg, dstAddrBaseReg, dstAddrIndexReg, dstAddrIndexScale, dstOffset);
            }
        }
    }
}
#endif // FEATURE_MULTIREG_RET

void CodeGen::GenStructStoreUnrollCopyWB(GenTree* store, ClassLayout* layout)
{
    assert(layout->HasGCPtr());

    ConsumeStructStore(store, layout, REG_RDI, REG_RSI, REG_NA);

    bool      dstOnStack;
    var_types dstAddrType;

    GenTree*  src;
    var_types srcAddrType;

    if (store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
    {
        src = store->AsLclVarCommon()->GetOp(0);

        dstOnStack  = true;
        dstAddrType = TYP_I_IMPL;
    }
    else
    {
        GenTree* dstAddr = store->AsIndir()->GetAddr();

        dstOnStack  = false;
        dstAddrType = dstAddr->GetType();

        src = store->AsIndir()->GetValue();
    }

    if (src->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        srcAddrType = TYP_I_IMPL;
    }
    else
    {
        assert(src->OperIs(GT_IND, GT_OBJ, GT_BLK));

        srcAddrType = src->AsIndir()->GetAddr()->GetType();
    }

    liveness.SetGCRegType(REG_RSI, srcAddrType);
    liveness.SetGCRegType(REG_RDI, dstAddrType);

    unsigned slotCount = layout->GetSlotCount();

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
            // TODO-MIKE-Cleanup: Remove bogus BYREF write barriers.
            if (layout->IsGCPtr(i))
            {
                genEmitHelperCall(CORINFO_HELP_ASSIGN_BYREF, EA_PTRSIZE);
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

    // While we normally update GC info prior to the last instruction that uses them,
    // these actually live into the helper call.
    liveness.RemoveGCRegs(RBM_RSI);
    liveness.RemoveGCRegs(RBM_RDI);
}

#ifdef UNIX_AMD64_ABI
void CodeGen::GenStructStoreUnrollRegsWB(GenTreeObj* store)
{
    ClassLayout* layout = store->GetLayout();

    assert(layout->HasGCRef());
    assert(layout->GetSize() == 16);
    assert(store->GetValue()->GetMultiRegCount(compiler) == 2);

    regMaskTP inGCrefRegSet = liveness.GetGCRegs(TYP_REF);
    regMaskTP inByrefRegSet = liveness.GetGCRegs(TYP_BYREF);

    GenTree*  addr       = store->GetAddr();
    regNumber addrReg    = addr->isUsedFromReg() ? UseReg(addr) : UseReg(addr->AsAddrMode()->GetBase());
    int       addrOffset = addr->isUsedFromReg() ? 0 : addr->AsAddrMode()->GetOffset();
    GenTree*  val        = store->GetValue();
    regNumber valReg0    = UseReg(val, 0);
    regNumber valReg1    = UseReg(val, 1);
    emitter*  emit       = GetEmitter();

    regMaskTP outGCrefRegSet = liveness.GetGCRegs(TYP_REF);
    regMaskTP outByrefRegSet = liveness.GetGCRegs(TYP_BYREF);

    if (layout->IsGCRef(0))
    {
        regNumber tempReg = store->ExtractTempReg();
        inst_Mov(TYP_REF, tempReg, valReg1, true);
        valReg1 = tempReg;

        emit->emitIns_R_AR(INS_lea, emitTypeSize(addr->GetType()), REG_ARG_0, addrReg, addrOffset);
        inst_Mov(TYP_REF, REG_ARG_1, valReg0, true);

        liveness.SetGCRegs(TYP_REF, inGCrefRegSet | genRegMask(tempReg));
        liveness.SetGCRegs(TYP_BYREF, inByrefRegSet);
        genEmitHelperCall(CORINFO_HELP_CHECKED_ASSIGN_REF, EA_PTRSIZE);
        liveness.SetGCRegs(TYP_REF, outGCrefRegSet);
        liveness.SetGCRegs(TYP_BYREF, outByrefRegSet);
    }
    else
    {
        emit->emitIns_AR_R(INS_mov, EA_8BYTE, valReg0, addrReg, addrOffset);
    }

    addrOffset += TARGET_POINTER_SIZE;

    if (layout->IsGCRef(1))
    {
        emit->emitIns_R_AR(INS_lea, emitTypeSize(addr->GetType()), REG_ARG_0, addrReg, addrOffset);
        inst_Mov(TYP_REF, REG_ARG_1, valReg1, true);
        genEmitHelperCall(CORINFO_HELP_CHECKED_ASSIGN_REF, EA_PTRSIZE);
    }
    else
    {
        emit->emitIns_AR_R(INS_mov, EA_8BYTE, valReg1, addrReg, addrOffset);
    }
}
#endif // UNIX_AMD64_ABI

// If any Vector3 args are on stack and they are not pass-by-ref, the upper 32bits
// must be cleared to zeroes. The native compiler doesn't clear the upper bits
// and there is no way to know if the caller is native or not. So, the upper
// 32 bits of Vector argument on stack are always cleared to zero.
#if defined(UNIX_AMD64_ABI) && defined(FEATURE_SIMD)
void CodeGen::PrologClearVector3StackParamUpperBits()
{
    JITDUMP("*************** In PrologClearVector3StackParamUpperBits()\n");

    assert(generatingProlog);

    for (unsigned lclNum = 0; lclNum < compiler->info.compArgsCount; lclNum++)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);
        assert(lcl->IsParam());

        // This is needed only for stack params, zeroing reg params is
        // done when the 2 param registers are packed together.
        if (lcl->TypeIs(TYP_SIMD12) && !lcl->IsRegParam())
        {
            GetEmitter()->emitIns_S_I(INS_mov, EA_4BYTE, lclNum, 12, 0);
        }
    }
}
#endif // defined(UNIX_AMD64_ABI) && defined(FEATURE_SIMD)

void CodeGen::genTableBasedSwitch(GenTreeOp* treeNode)
{
    regNumber idxReg  = UseReg(treeNode->GetOp(0));
    regNumber baseReg = UseReg(treeNode->GetOp(1));

    regNumber tmpReg = treeNode->GetSingleTempReg();

    // load the ip-relative offset (which is relative to start of fgFirstBB)
    GetEmitter()->emitIns_R_ARX(INS_mov, EA_4BYTE, baseReg, baseReg, idxReg, 4, 0);

    // add it to the absolute address of fgFirstBB
    GetEmitter()->emitIns_R_L(INS_lea, EA_PTR_DSP_RELOC, compiler->fgFirstBB, tmpReg);
    GetEmitter()->emitIns_R_R(INS_add, EA_PTRSIZE, baseReg, tmpReg);
    // jmp baseReg
    GetEmitter()->emitIns_R(INS_i_jmp, emitTypeSize(TYP_I_IMPL), baseReg);
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
    }

    GetEmitter()->emitDataGenEnd();

    GetEmitter()->emitIns_R_C(INS_lea, EA_PTRSIZE, node->GetRegNum(), Emitter::MakeRoDataField(jmpTabBase));
    DefReg(node);
}

void CodeGen::genCodeForLockAdd(GenTreeOp* node)
{
    assert(node->OperIs(GT_LOCKADD));

    GenTree* addr  = node->GetOp(0);
    GenTree* value = node->GetOp(1);
    emitAttr size  = emitActualTypeSize(value->GetType());

    assert((size == EA_4BYTE) || (size == EA_PTRSIZE));

    regNumber addrReg  = UseReg(addr);
    regNumber valueReg = value->isUsedFromReg() ? UseReg(value) : REG_NA;

    instGen(INS_lock);

    if (GenTreeIntCon* imm = value->IsContainedIntCon())
    {
        GetEmitter()->emitIns_AR_I(INS_add, size, addrReg, 0, imm->GetInt32Value());
    }
    else
    {
        GetEmitter()->emitIns_AR_R(INS_add, size, valueReg, addrReg, 0);
    }
}

void CodeGen::genLockedInstructions(GenTreeOp* node)
{
    assert(node->OperIs(GT_XADD, GT_XCHG));

    GenTree* addr = node->gtGetOp1();
    GenTree* data = node->gtGetOp2();
    emitAttr size = emitTypeSize(node->TypeGet());

    assert((size == EA_4BYTE) || (size == EA_PTRSIZE));

    regNumber addrReg = UseReg(addr);
    regNumber dataReg = UseReg(data);
    regNumber dstReg  = node->GetRegNum();

    // If the destination register is different from the shift register then we need
    // to first move the shift to the target register. Make sure we don't overwrite
    // the address, the register allocator should have taken care of this.
    assert((dstReg != addrReg) || (dstReg == dataReg));

    GetEmitter()->emitIns_Mov(INS_mov, size, dstReg, dataReg, /* canSkip */ true);

    instruction ins = node->OperIs(GT_XADD) ? INS_xadd : INS_xchg;

    // XCHG has an implied lock prefix when the first operand is a memory operand.
    if (ins != INS_xchg)
    {
        instGen(INS_lock);
    }

    GetEmitter()->emitIns_AR_R(ins, size, dstReg, addrReg, 0);
    genProduceReg(node);
}

void CodeGen::genCodeForCmpXchg(GenTreeCmpXchg* tree)
{
    assert(tree->OperIs(GT_CMPXCHG));

    var_types targetType = tree->TypeGet();
    regNumber targetReg  = tree->GetRegNum();

    GenTree* location  = tree->GetAddr();
    GenTree* value     = tree->GetValue();
    GenTree* comparand = tree->GetCompareValue();

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

void CodeGen::GenMemoryBarrier(GenTree* barrier)
{
    assert(barrier->OperIs(GT_MEMORYBARRIER));

#ifdef DEBUG
    if (JitConfig.JitNoMemoryBarriers() == 1)
    {
        return;
    }
#endif

    // Only full barrier needs to be emitted on x86/64
    if ((barrier->gtFlags & GTF_MEMORYBARRIER_LOAD) == 0)
    {
        instGen(INS_lock);
        GetEmitter()->emitIns_AR_I(INS_or, EA_4BYTE, REG_SPBASE, 0, 0);
    }
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

    emitInsBinary(cmpKind, emitTypeSize(bndsChkType), src1, src2);
    genJumpToThrowHlpBlk(jmpKind, bndsChk->GetThrowKind(), bndsChk->GetThrowBlock());
}

void CodeGen::genCodeForPhysReg(GenTreePhysReg* tree)
{
    assert(tree->OperIs(GT_PHYSREG));

    var_types targetType = tree->TypeGet();
    regNumber targetReg  = tree->GetRegNum();

    inst_Mov(targetType, targetReg, tree->gtSrcReg, /* canSkip */ true);
    liveness.TransferGCRegType(targetReg, tree->gtSrcReg);

    DefReg(tree);
}

void CodeGen::genCodeForNullCheck(GenTreeIndir* tree)
{
    assert(tree->OperIs(GT_NULLCHECK));

    assert(tree->gtOp1->isUsedFromReg());
    regNumber reg = genConsumeReg(tree->gtOp1);
    GetEmitter()->emitIns_AR_R(INS_cmp, EA_4BYTE, reg, reg, 0);
}

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
    genJumpToThrowHlpBlk(EJ_jae, ThrowHelperKind::IndexOutOfRange);

    genProduceReg(arrIndex);
}

void CodeGen::genCodeForArrOffset(GenTreeArrOffs* arrOffset)
{
    GenTree* offsetNode = arrOffset->GetOffset();
    GenTree* indexNode  = arrOffset->GetIndex();
    GenTree* arrObj     = arrOffset->GetArray();

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
        offsetReg = UseReg(offsetNode);

        // We will use a temp register for the offset*scale+effectiveIndex computation.
        tmpReg = arrOffset->GetSingleTempReg();
    }
    else
    {
        assert(offsetNode->isContained());
    }
    regNumber indexReg = UseReg(indexNode);
    // Although arrReg may not be used in the constant-index case, if we have generated
    // the shift into a register, we must consume it, otherwise we will fail to end the
    // live range of the gc ptr.
    // TODO-CQ: Currently arrObj will always have a register allocated to it.
    // We could avoid allocating a register for it, which would be of shift if the arrObj
    // is an on-stack lclVar.
    regNumber arrReg = UseReg(arrObj);

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

instruction CodeGen::genGetInsForOper(genTreeOps oper)
{
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

void CodeGen::genCodeForShift(GenTreeOp* tree)
{
    assert(tree->OperIsShiftOrRotate());

    var_types   targetType = tree->GetType();
    instruction ins        = genGetInsForOper(tree->GetOper());

    GenTree* operand = tree->GetOp(0);
    GenTree* shiftBy = tree->GetOp(1);

    regNumber operandReg = UseReg(operand);
    regNumber shiftByReg = shiftBy->isUsedFromReg() ? UseReg(shiftBy) : REG_NA;
    regNumber dstReg     = tree->GetRegNum();

    if (shiftByReg == REG_NA)
    {
        int      shiftByValue = shiftBy->AsIntCon()->GetInt32Value();
        emitAttr size         = emitTypeSize(tree->GetType());

        if (tree->OperIs(GT_LSH) && !tree->HasImplicitFlagsDef() && (shiftByValue == 1))
        {
            if (dstReg == operandReg)
            {
                // ADD reg, reg tends to be more efficient than SHL reg, 1.
                GetEmitter()->emitIns_R_R(INS_add, size, dstReg, operandReg);
            }
            else
            {
                // TDOO-MIKE-Review: What about SHL reg, 2/3 => LEA [reg*4/8]?
                GetEmitter()->emitIns_R_ARX(INS_lea, size, dstReg, operandReg, operandReg, 1, 0);
            }
        }
#ifdef TARGET_64BIT
        else if (tree->OperIs(GT_ROL, GT_ROR) && varTypeIsLong(targetType) && (dstReg != operandReg) &&
                 (shiftByValue > 0) && (shiftByValue < 64) &&
                 compiler->compOpportunisticallyDependsOn(InstructionSet_BMI2))
        {
            shiftByValue = tree->OperIs(GT_ROL) ? (64 - shiftByValue) : shiftByValue;
            GetEmitter()->emitIns_R_R_I(INS_rorx, size, dstReg, operandReg, shiftByValue);
        }
#endif
        else
        {
            inst_Mov(targetType, dstReg, operandReg, /* canSkip */ true);
            inst_RV_SH(ins, size, dstReg, shiftByValue);
        }
    }
    else
    {
        noway_assert(operandReg != REG_RCX);

        inst_Mov(TYP_INT, REG_RCX, shiftByReg, /* canSkip */ true);
        inst_Mov(targetType, dstReg, operandReg, /* canSkip */ true);
        inst_RV(ins, dstReg, targetType);
    }

    DefReg(tree);
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

    regNumber regLo  = UseReg(operandLo);
    regNumber regHi  = UseReg(operandHi);
    regNumber dstReg = tree->GetRegNum();

    var_types   targetType = tree->TypeGet();
    instruction ins        = genGetInsForOper(oper);

    GenTree* shiftBy = tree->gtGetOp2();

    assert(shiftBy->isContainedIntOrIImmed());

    unsigned int count = (unsigned int)shiftBy->AsIntConCommon()->IconValue();

    regNumber regResult = (oper == GT_LSH_HI) ? regHi : regLo;
    regNumber reg2      = (oper == GT_LSH_HI) ? regLo : regHi;

    inst_Mov(targetType, dstReg, regResult, /* canSkip */ true);
    GetEmitter()->emitIns_R_R_I(ins, emitTypeSize(targetType), dstReg, reg2, count);

    DefReg(tree);
}
#endif

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
        LoadSIMD12(tree);
        genProduceReg(tree);
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

void CodeGen::GenLoadLclVar(GenTreeLclVar* load)
{
    assert(load->OperIs(GT_LCL_VAR));

    LclVarDsc* lcl = compiler->lvaGetDesc(load);

    assert(!lcl->IsIndependentPromoted());

    // TODO-MIKE-Review: The spilled check is dubious, it cannot be spilled unless it's a reg candidate...
    if (lcl->IsRegCandidate() || load->IsRegSpilled(0))
    {
        return;
    }

#if defined(FEATURE_SIMD) && defined(TARGET_X86)
    if (load->TypeIs(TYP_SIMD12))
    {
        LoadSIMD12(load);
        DefLclVarReg(load);

        return;
    }
#endif

    var_types   type = lcl->GetRegisterType(load);
    instruction ins  = ins_Load(type, IsSimdLocalAligned(load->GetLclNum()));
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
#ifdef FEATURE_SIMD
    else if (type == TYP_SIMD12)
    {
        genStoreSIMD12(store, src);
    }
#endif
    else if (src->isContained() && src->OperIsRMWMemOp())
    {
        GenStoreLclRMW(type, store->GetLclNum(), store->GetLclOffs(), src);
    }
    else if (GenTreeIntCon* imm = src->IsContainedIntCon())
    {
        GetEmitter()->emitIns_S_I(ins_Store(type), emitTypeSize(type), store->GetLclNum(), store->GetLclOffs(),
                                  imm->GetInt32Value());
    }
    else
    {
        assert(IsValidSourceType(type, src->GetType()));

        regNumber srcReg = UseReg(src);
        GetEmitter()->emitIns_S_R(ins_Store(type), emitTypeSize(type), srcReg, store->GetLclNum(), store->GetLclOffs());
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

#ifndef TARGET_64BIT
    if (store->TypeIs(TYP_LONG))
    {
        GenStoreLclVarLong(store);
        return;
    }
#endif

    GenTree* src = store->GetOp(0);

    if (store->TypeIs(TYP_STRUCT))
    {
        ClassLayout*    layout = lcl->GetLayout();
        StructStoreKind kind   = GetStructStoreKind(true, layout, src);
        GenStructStore(store, kind, layout);
        genUpdateLife(store);
        return;
    }

#if !defined(UNIX_AMD64_ABI) && !defined(TARGET_X86)
    assert(!src->IsMultiRegNode());
#else
    if (src->IsMultiRegNode())
    {
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
#endif

    var_types lclRegType = lcl->GetRegisterType(store);

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

#ifdef FEATURE_SIMD
    if (lclRegType == TYP_SIMD12)
    {
        genStoreSIMD12(store, src);
        // TODO-MIKE-Review: Doesn't this need a genUpdateLife call?
        // And how exactly does this work anyway? It does not check if a register was allocated
        // to the local, it always stores to memory. Always storing to memory is probably correct
        // but not always necessary. Problem is, what if the destination register is different
        // from the source register? No reg-reg move is being generated?!?
        // Unpilling SIMD12 is probably broken too since it doesn't use LoadSIMD12, it looks
        // like it will emit a movups and load garbage in the 4th vector element instead of 0.
        // See vec3-param-def-spill.cs.
        return;
    }
#endif

    regNumber dstReg = store->GetRegNum();
    unsigned  lclNum = store->GetLclNum();

    if (dstReg == REG_NA)
    {
        bool        isAligned = IsSimdLocalAligned(lclNum);
        instruction ins       = ins_Store(lclRegType, isAligned);
        emitAttr    attr      = emitTypeSize(lclRegType);

        if (src->isContained())
        {
            if (src->OperIs(GT_BITCAST))
            {
                GenTree*  bitCastSrc     = src->AsUnOp()->GetOp(0);
                var_types bitCastSrcType = bitCastSrc->GetType();
                regNumber bitCastSrcReg  = genConsumeReg(bitCastSrc);

                ins = ins_Store(bitCastSrcType, isAligned);

                GetEmitter()->emitIns_S_R(ins, attr, bitCastSrcReg, lclNum, 0);
            }
            else if (src->OperIsRMWMemOp())
            {
                GenStoreLclRMW(lclRegType, lclNum, 0, src);
            }
            else
            {
                GetEmitter()->emitIns_S_I(ins, attr, lclNum, 0, static_cast<int>(src->AsIntCon()->GetValue()));
            }
        }
        else
        {
            regNumber srcReg = UseReg(src);

            GetEmitter()->emitIns_S_R(ins, attr, srcReg, store->GetLclNum(), 0);
        }

        genUpdateLife(store);
        lcl->SetRegNum(REG_STK);

        return;
    }

    // Look for the case where we have a constant zero which we've marked for reuse,
    // but which isn't actually in the register we want. In that case, it's better to create
    // zero in the target register, because an xor is smaller than a copy. Note that we could
    // potentially handle this in the register allocator, but we can't always catch it there
    // because the target may not have a register allocated for it yet.

    if (src->isUsedFromReg() && (src->GetRegNum() != dstReg) &&
        (src->IsIntegralConst(0) || src->IsDblConPositiveZero()))
    {
        UseReg(src);
        src->SetRegNum(REG_NA);
        src->ResetReuseRegVal();
        src->SetContained();
    }

    if (src->isContained())
    {
        assert(src->GetRegNum() == REG_NA);

        if (src->OperIs(GT_BITCAST))
        {
            GenTree*  bitCastSrc     = src->AsUnOp()->GetOp(0);
            var_types bitCastSrcType = bitCastSrc->GetType();
            regNumber bitCastSrcReg  = genConsumeReg(bitCastSrc);

            inst_BitCast(lclRegType, dstReg, bitCastSrcType, bitCastSrc->GetRegNum());
        }
        else if (GenTreeIntCon* intCon = src->IsIntCon())
        {
            GenIntCon(intCon, dstReg, lclRegType);
        }
        else
        {
            GenDblCon(src->AsDblCon(), dstReg, lclRegType);
        }
    }
    else
    {
        // Note that src cannot be "reg optional", we don't know in advance if this local
        // will be allocated a register so we could end up with a mem-to-mem copy and
        // need a temporary register, which too must be requested before knowing if this
        // local gets a register or not. Hopefully this doesn't actually matter, if the
        // src node is spilled LSRA should reload it directly in our dstReg.
        regNumber srcReg = UseReg(src);

        // TODO-MIKE-Cleanup: emitIns_Mov tries to skip generating useless mov reg, reg
        // instructions but cannot do it properly because it doesn't know the source reg
        // type. If the destination type is a GC type then it tries to check GC liveness
        // to figure out if the destination reg will need to be recorded in GC info, but
        // then GC liveness is out of sync in at least one common case:
        //
        //   mov rdi, ... ; src node loads a value into a reg
        //   mov rdi, rdi ; store node is normally useless, unless src wasn't a GC ref
        //                ; but UseReg removes rdi from GC liveness and the emitter now
        //                ; thinks that rdi isn't a GC ref, according to GC liveness and
        //                ; emits a useless IF_GC_REG instruction
        //
        // Deal with this here, where we can check the source type. This should probably
        // be moved to inst_Mov, but first the "extend" crap needs to be dealt with, so
        // code can be more easily shared with ARM and ARM64 and other places that may
        // need this (basically all reg-to-reg copies).
        //
        // Alternative: never generate STORE_LCL_VAR with mismatched source GC type.
        // Use BITCAST when this happens (rarely anyway) and let it deal with it.

        if ((dstReg != srcReg) || (lclRegType != varActualType(src->GetType())) || varTypeIsSmall(lclRegType))
        {
            // TODO-MIKE-Review: ARM and ARM64 don't seem to be doing the "extend" thing.
            // Stores to "NormalizeOnStore" locals should have been widened to INT and
            // "NormalizeOnLoad" locals are usually not register candidates. With the
            // exception of parameters, but those being "NormalizeOnLoad" have widening
            // casts on loads, so widening here is redundant.
            instruction ins  = ins_Move_Extend(lclRegType);
            emitAttr    attr = emitTypeSize(lclRegType);

            GetEmitter()->emitIns_Mov(ins, attr, dstReg, srcReg, /*canSkip*/ true);
        }
    }

    DefLclVarReg(store);
}

#if defined(UNIX_AMD64_ABI) || defined(TARGET_X86)

void CodeGen::GenStoreLclVarMultiRegSIMDReg(GenTreeLclVar* store)
{
    assert(varTypeIsSIMD(store->GetType()));

    GenTree* src = store->GetOp(0);

    UseRegs(src);

    // This is used to store a Vector3/4 call return shift, on UNIX_AMD64_ABI
    // such a shift is returned into 2 XMM registers and we need to pack it
    // into the XMM destination register.
    // This also handles the case of Vector2 being returned in 2 GPRs on x86.

    GenTreeCall* call = src->gtSkipReloadOrCopy()->AsCall();

    assert(call->GetRegCount() == 2);
#ifdef TARGET_X86
    assert(!varTypeUsesFloatReg(call->GetRegType(0)));
    assert(!varTypeUsesFloatReg(call->GetRegType(1)));
#else
    assert(varTypeUsesFloatReg(call->GetRegType(0)));
    assert(varTypeUsesFloatReg(call->GetRegType(1)));
#endif

    regNumber srcReg0 = call->GetRegNum(0);
    regNumber srcReg1 = call->GetRegNum(1);
    regNumber dstReg  = store->GetRegNum();

#ifdef TARGET_X86
    regNumber tmpReg = store->GetSingleTempReg();

    GetEmitter()->emitIns_Mov(INS_movd, EA_4BYTE, dstReg, srcReg0, false);
    GetEmitter()->emitIns_Mov(INS_movd, EA_4BYTE, tmpReg, srcReg1, false);
    GetEmitter()->emitIns_R_R(INS_unpcklps, EA_16BYTE, dstReg, tmpReg);
#else
    if (dstReg == srcReg0)
    {
        GetEmitter()->emitIns_R_R(INS_movlhps, EA_16BYTE, dstReg, srcReg1);
    }
    else if (compiler->canUseVexEncoding())
    {
        GetEmitter()->emitIns_R_R_R(INS_unpcklpd, EA_16BYTE, dstReg, srcReg0, srcReg1);
    }
    else if (dstReg == srcReg1)
    {
        GetEmitter()->emitIns_R_R(INS_movlhps, EA_16BYTE, dstReg, srcReg1);
        GetEmitter()->emitIns_Mov(INS_movsdsse2, EA_16BYTE, dstReg, srcReg0, /* canSkip */ false);
    }
    else
    {
        GetEmitter()->emitIns_Mov(INS_movaps, EA_16BYTE, dstReg, srcReg0, /* canSkip */ false);
        GetEmitter()->emitIns_R_R(INS_movlhps, EA_16BYTE, dstReg, srcReg1);
    }
#endif

    DefLclVarReg(store);
}

void CodeGen::GenStoreLclVarMultiRegSIMDMem(GenTreeLclVar* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR) && varTypeIsSIMD(store->GetType()) && !store->IsMultiReg());

    GenTree*     src    = store->GetOp(0);
    GenTreeCall* call   = src->gtSkipReloadOrCopy()->AsCall();
    unsigned     lclNum = store->GetLclNum();
    LclVarDsc*   lcl    = compiler->lvaGetDesc(lclNum);

    assert(call->GetRegCount() == 2);
    assert(!lcl->IsRegCandidate() || (store->GetRegNum() == REG_NA));

    regNumber reg0 = UseReg(src, 0);
    regNumber reg1 = UseReg(src, 1);

#ifdef TARGET_X86
    assert(store->TypeIs(TYP_SIMD8));
    assert((call->GetRegType(0) == TYP_INT) && (call->GetRegType(1) == TYP_INT));

    GetEmitter()->emitIns_S_R(INS_mov, EA_4BYTE, reg0, lclNum, 0);
    GetEmitter()->emitIns_S_R(INS_mov, EA_4BYTE, reg1, lclNum, 4);
#else
    assert(store->TypeIs(TYP_SIMD12, TYP_SIMD16));
    assert(call->GetRegType(0) == TYP_DOUBLE);
    assert((call->GetRegType(1) == TYP_DOUBLE) || (call->GetRegType(1) == TYP_FLOAT));

    GetEmitter()->emitIns_S_R(INS_movsdsse2, EA_8BYTE, reg0, lclNum, 0);
    // TODO-MIKE-Review: Do we need to store a 0 for the 4th element of Vector3? Old code did not.
    // Also, it may be better to do a 8 byte store instead of 4 byte store whenever there is
    // enough space (pretty much always since local sizes are normally rounded up to 8 bytes,
    // P-DEP fields are probably the only exception).
    // Actually, it may be even better to pack the 2 regs into one and do a single store, if there
    // are subsequent SIMD loads then doing 2 stores here will block store forwarding.
    GetEmitter()->emitIns_S_R(store->TypeIs(TYP_SIMD12) ? INS_movss : INS_movsdsse2,
                              store->TypeIs(TYP_SIMD12) ? EA_4BYTE : EA_8BYTE, reg1, lclNum, 8);
#endif

    genUpdateLife(store);
    lcl->SetRegNum(REG_STK);
}

#endif // defined(UNIX_AMD64_ABI) || defined(TARGET_X86)

void CodeGen::GenStoreLclRMW(var_types type, unsigned lclNum, unsigned lclOffs, GenTree* src)
{
    assert(src->OperIsRMWMemOp());
    assert(varTypeIsIntegral(type));

    instruction ins  = genGetInsForOper(src->GetOper());
    emitAttr    attr = emitTypeSize(type);

    if (src->OperIsUnary())
    {
        assert(src->AsUnOp()->GetOp(0)->AsLclVarCommon()->GetLclNum() == lclNum);

        GetEmitter()->emitIns_S(ins, attr, lclNum, lclOffs);

        return;
    }

    assert(src->AsOp()->GetOp(0)->AsLclVarCommon()->GetLclNum() == lclNum);

    src = src->AsOp()->GetOp(1);

    bool isShift = (ins == INS_shl) || (ins == INS_shr) || (ins == INS_sar) || (ins == INS_rol) || (ins == INS_ror) ||
                   (ins == INS_rcl) || (ins == INS_rcr);

    if (!src->isUsedFromReg())
    {
        int imm = src->AsIntCon()->GetInt32Value();

        if (isShift && (imm == 1))
        {
            GetEmitter()->emitIns_S(MapShiftInsToShiftBy1Ins(ins), attr, lclNum, lclOffs);
        }
        else
        {
            if (isShift)
            {
                ins = MapShiftInsToShiftByImmIns(ins);
            }

            GetEmitter()->emitIns_S_I(ins, attr, lclNum, lclOffs, imm);
        }

        return;
    }

    regNumber srcReg = genConsumeReg(src);

    if (isShift)
    {
        if (srcReg != REG_RCX)
        {
            GetEmitter()->emitIns_Mov(INS_mov, EA_4BYTE, REG_RCX, srcReg, true);
        }

        GetEmitter()->emitIns_S(ins, attr, lclNum, lclOffs);

        return;
    }

    GetEmitter()->emitIns_S_R(ins, attr, srcReg, lclNum, lclOffs);
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

    const regNumber baseReg  = UseReg(base);
    regNumber       indexReg = UseReg(index);
    const regNumber dstReg   = node->GetRegNum();

    // TODO-MIKE-Review: This is dubious, GC liveness doesn't really matter until we reach a call...

    // NOTE: UseReg marks the consumed register as not a GC pointer, as it assumes that the input registers
    // die at the first instruction generated by the node. This is not the case for `INDEX_ADDR`, however, as the
    // base register is multiply-used. As such, we need to mark the base register as containing a GC pointer until
    // we are finished generating the code for this node.

    liveness.SetGCRegType(baseReg, base->GetType());
    assert(varTypeIsIntegral(index->GetType()));

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

        genJumpToThrowHlpBlk(EJ_jae, ThrowHelperKind::IndexOutOfRange, node->GetThrowBlock());
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

    // TODO-MIKE-Review: Hrm, what if baseReg is a local variable reg?!
    liveness.RemoveGCRegs(genRegMask(baseReg));

    DefReg(node);
}

void CodeGen::GenIndLoad(GenTreeIndir* load)
{
    assert(load->OperIs(GT_IND));

#ifdef FEATURE_SIMD
    if (load->TypeIs(TYP_SIMD12))
    {
        LoadSIMD12(load);
    }
    else
#endif
#ifdef WINDOWS_X86_ABI
        if (GenTreeIntCon* tls = load->GetAddr()->IsIntCon(HandleKind::TLS))
    {
        noway_assert(load->TypeIs(TYP_INT));
        GetEmitter()->emitInsMov_R_FS(load->GetRegNum(), tls->GetInt32Value());
    }
    else
#endif
    {
        genConsumeAddress(load->GetAddr());
        emitInsLoad(ins_Load(load->GetType()), emitTypeSize(load->GetType()), load->GetRegNum(), load->GetAddr());
    }

    DefReg(load);
}

void CodeGen::GenIndStore(GenTreeStoreInd* store)
{
    assert(store->OperIs(GT_STOREIND));

#ifdef FEATURE_SIMD
    if (store->TypeIs(TYP_SIMD12))
    {
        genStoreSIMD12(store, store->GetValue());
        return;
    }
#endif

    GenTree*  addr  = store->GetAddr();
    GenTree*  value = store->GetValue();
    var_types type  = store->GetType();

    assert(IsValidSourceType(type, value->GetType()));

    GCInfo::WriteBarrierForm writeBarrierForm = GCInfo::GetWriteBarrierForm(store);
    if (writeBarrierForm != GCInfo::WBF_NoBarrier)
    {
        regNumber addrReg  = UseReg(addr);
        regNumber valueReg = UseReg(value);

        if (!genEmitOptimizedGCWriteBarrier(writeBarrierForm, addr, value))
        {
            // At this point, we should not have any interference.
            // That is, 'shift' must not be in REG_ARG_0, as that is where 'addr' must go.
            noway_assert(valueReg != REG_ARG_0);

            inst_Mov(addr->GetType(), REG_ARG_0, addrReg, /* canSkip */ true);
            inst_Mov(value->GetType(), REG_ARG_1, valueReg, /* canSkip */ true);
            genGCWriteBarrier(store, writeBarrierForm);
        }

        return;
    }

    emitAttr attr = emitTypeSize(type);

    genConsumeAddress(addr);

    if (!value->isContained() || !value->OperIsRMWMemOp())
    {
        genConsumeRegs(value);
        emitInsStore(ins_Store(value->GetType()), attr, store->GetAddr(), store->GetValue());

        return;
    }

    if (value->OperIsUnary())
    {
        INDEBUG(GenTreeIndir* load = value->AsUnOp()->GetOp(0)->AsIndir());
        assert(load->isUsedFromMemory() && load->isContained());

        GetEmitter()->emitInsRMW_A(genGetInsForOper(value->GetOper()), attr, addr);

        return;
    }

    GenTree* load = value->AsOp()->GetOp(0);
    GenTree* src  = value->AsOp()->GetOp(1);

    assert(load->isUsedFromMemory() && load->isContained());

    genConsumeRegs(src);

    emitter* emit = GetEmitter();

    if (value->OperIs(GT_ADD) && (src->IsIntCon(1) || src->IsIntCon(-1)))
    {
        assert(src->isContained());

        emit->emitInsRMW_A(src->IsIntCon(1) ? INS_inc : INS_dec, attr, addr);
    }
    else if (value->OperIsShiftOrRotate())
    {
        assert(src == value->AsOp()->GetOp(1));

        GenIndStoreRMWShift(addr, value->AsOp(), src);
    }
    else if (GenTreeIntCon* imm = src->IsContainedIntCon())
    {
        emit->emitInsRMW_A_I(genGetInsForOper(value->GetOper()), attr, addr, imm->GetInt32Value());
    }
    else
    {
        emit->emitInsRMW_A_R(genGetInsForOper(value->GetOper()), attr, addr, src->GetRegNum());
    }
}

void CodeGen::GenIndStoreRMWShift(GenTree* addr, GenTreeOp* shift, GenTree* shiftBy)
{
    instruction ins  = genGetInsForOper(shift->GetOper());
    emitAttr    attr = emitTypeSize(shift->GetType());
    emitter*    emit = GetEmitter();

    if (shiftBy->isUsedFromReg())
    {
        genCopyRegIfNeeded(shiftBy, REG_RCX);
        // The shiftBy operand is implicit, so call the unary version of emitInsRMW.
        emit->emitInsRMW_A(ins, attr, addr);
    }
    else if (shiftBy->AsIntCon()->GetInt32Value() == 1)
    {
        emit->emitInsRMW_A(MapShiftInsToShiftBy1Ins(ins), attr, addr);
    }
    else if (GenTreeIntCon* imm = shiftBy->IsContainedIntCon())
    {
        emit->emitInsRMW_A_I(MapShiftInsToShiftByImmIns(ins), attr, addr, imm->GetInt32Value());
    }
    else
    {
        emit->emitInsRMW_A_R(MapShiftInsToShiftByImmIns(ins), attr, addr, shiftBy->GetRegNum());
    }
}

instruction CodeGen::MapShiftInsToShiftBy1Ins(instruction ins)
{
    assert(ins == INS_rcl || ins == INS_rcr || ins == INS_rol || ins == INS_ror || ins == INS_shl || ins == INS_shr ||
           ins == INS_sar);

    assert(INS_rcl + 1 == INS_rcl_1);
    assert(INS_rcr + 1 == INS_rcr_1);
    assert(INS_rol + 1 == INS_rol_1);
    assert(INS_ror + 1 == INS_ror_1);
    assert(INS_shl + 1 == INS_shl_1);
    assert(INS_shr + 1 == INS_shr_1);
    assert(INS_sar + 1 == INS_sar_1);

    return static_cast<instruction>(ins + 1);
}

instruction CodeGen::MapShiftInsToShiftByImmIns(instruction ins)
{
    assert(ins == INS_rcl || ins == INS_rcr || ins == INS_rol || ins == INS_ror || ins == INS_shl || ins == INS_shr ||
           ins == INS_sar);

    assert(INS_rcl + 2 == INS_rcl_N);
    assert(INS_rcr + 2 == INS_rcr_N);
    assert(INS_rol + 2 == INS_rol_N);
    assert(INS_ror + 2 == INS_ror_N);
    assert(INS_shl + 2 == INS_shl_N);
    assert(INS_shr + 2 == INS_shr_N);
    assert(INS_sar + 2 == INS_sar_N);

    return static_cast<instruction>(ins + 2);
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
    assert(IsRegCandidateLclVar(tree->gtOp1) && IsRegCandidateLclVar(tree->gtOp2));

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

    varDsc1->SetRegNum(oldOp2Reg);
    varDsc2->SetRegNum(oldOp1Reg);

    // Do the xchg
    emitAttr size = EA_PTRSIZE;
    if (varTypeGCtype(type1) != varTypeGCtype(type2))
    {
        // If the type specified to the emitter is a GC type, it will swap the GC-ness of the registers.
        // Otherwise it will leave them alone, which is correct if they have the same GC-ness.
        // TODO-MIKE-Review: Check what the emitter does in this case. And LSRA too, presumably it only
        // uses XCHG if GCness matches?
        size = EA_GCREF;
    }

    GetEmitter()->emitIns_R_R(INS_xchg, size, oldOp1Reg, oldOp2Reg);

    // Manually remove these regs for the gc sets (mostly to avoid confusing duplicative dump output)
    liveness.SetGCRegs(TYP_BYREF, liveness.GetGCRegs(TYP_BYREF) & ~(oldOp1RegMask | oldOp2RegMask));
    liveness.SetGCRegs(TYP_REF, liveness.GetGCRegs(TYP_REF) & ~(oldOp1RegMask | oldOp2RegMask));

    liveness.SetGCRegType(oldOp2Reg, type1);
    liveness.SetGCRegType(oldOp1Reg, type2);
}

//------------------------------------------------------------------------
// genEmitOptimizedGCWriteBarrier: Generate write barrier store using the optimized
// helper functions.
//
// Arguments:
//    writeBarrierForm - the write barrier form to use
//    addr - the address at which to do the store
//    shift - the shift to store
//
// Return Value:
//    true if an optimized write barrier form was used, false if not. If this
//    function returns false, the caller must emit a "standard" write barrier.

bool CodeGen::genEmitOptimizedGCWriteBarrier(GCInfo::WriteBarrierForm writeBarrierForm, GenTree* addr, GenTree* data)
{
    assert(writeBarrierForm != GCInfo::WBF_NoBarrier);

#if defined(TARGET_X86) && NOGC_WRITE_BARRIERS
    if (!UseOptimizedWriteBarriers())
    {
        return false;
    }

    const static CorInfoHelpFunc regToHelper[2][8] = {
        // If the target is known to be in managed memory
        {
            CORINFO_HELP_ASSIGN_REF_EAX, // EAX
            CORINFO_HELP_ASSIGN_REF_ECX, // ECX
            CORINFO_HELP_UNDEF,          // EDX (always the target address)
            CORINFO_HELP_ASSIGN_REF_EBX, // EBX
            CORINFO_HELP_UNDEF,          // ESP
            CORINFO_HELP_ASSIGN_REF_EBP, // EBP
            CORINFO_HELP_ASSIGN_REF_ESI, // ESI
            CORINFO_HELP_ASSIGN_REF_EDI, // EDI
        },

        // Don't know if the target is in managed memory
        {
            CORINFO_HELP_CHECKED_ASSIGN_REF_EAX, // EAX
            CORINFO_HELP_CHECKED_ASSIGN_REF_ECX, // ECX
            CORINFO_HELP_UNDEF,                  // EDX (always the target address)
            CORINFO_HELP_CHECKED_ASSIGN_REF_EBX, // EBX
            CORINFO_HELP_UNDEF,                  // ESP
            CORINFO_HELP_CHECKED_ASSIGN_REF_EBP, // EBP
            CORINFO_HELP_CHECKED_ASSIGN_REF_ESI, // ESI
            CORINFO_HELP_CHECKED_ASSIGN_REF_EDI, // EDI
        },
    };

    noway_assert(regToHelper[0][REG_EAX] == CORINFO_HELP_ASSIGN_REF_EAX);
    noway_assert(regToHelper[0][REG_ECX] == CORINFO_HELP_ASSIGN_REF_ECX);
    noway_assert(regToHelper[0][REG_EBX] == CORINFO_HELP_ASSIGN_REF_EBX);
    noway_assert(regToHelper[0][REG_ESP] == CORINFO_HELP_UNDEF);
    noway_assert(regToHelper[0][REG_EBP] == CORINFO_HELP_ASSIGN_REF_EBP);
    noway_assert(regToHelper[0][REG_ESI] == CORINFO_HELP_ASSIGN_REF_ESI);
    noway_assert(regToHelper[0][REG_EDI] == CORINFO_HELP_ASSIGN_REF_EDI);

    noway_assert(regToHelper[1][REG_EAX] == CORINFO_HELP_CHECKED_ASSIGN_REF_EAX);
    noway_assert(regToHelper[1][REG_ECX] == CORINFO_HELP_CHECKED_ASSIGN_REF_ECX);
    noway_assert(regToHelper[1][REG_EBX] == CORINFO_HELP_CHECKED_ASSIGN_REF_EBX);
    noway_assert(regToHelper[1][REG_ESP] == CORINFO_HELP_UNDEF);
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

    genEmitHelperCall(regToHelper[tgtAnywhere][reg], EA_PTRSIZE);

    return true;
#else  // !defined(TARGET_X86) || !NOGC_WRITE_BARRIERS
    return false;
#endif // !defined(TARGET_X86) || !NOGC_WRITE_BARRIERS
}

void CodeGen::genCallInstruction(GenTreeCall* call)
{
    // All virtuals should have been expanded into a control expression
    assert(!call->IsVirtual() || (call->gtControlExpr != nullptr) || (call->gtCallAddr != nullptr));

    genAlignStackBeforeCall(call);

#ifdef TARGET_X86
    if (call->IsTailCallViaJitHelper() && compiler->getNeedsGSSecurityCookie())
    {
        EpilogGSCookieCheck(true);
    }
#endif

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
        if (GenTreeFieldList* fieldList = argNode->IsFieldList())
        {
            INDEBUG(unsigned regIndex = 0;)
            for (GenTreeFieldList::Use& use : fieldList->Uses())
            {
                GenTree* node = use.GetNode();
                assert(node->gtSkipReloadOrCopy()->OperIs(GT_PUTARG_REG));
                UseReg(node);
                assert(node->GetRegNum() == argInfo->GetRegNum(regIndex++));
            }

            continue;
        }
#endif // UNIX_AMD64_ABI

        regNumber argReg = UseReg(argNode);

        assert(argReg == argInfo->GetRegNum());

#ifdef WINDOWS_AMD64_ABI
        if (call->IsVarargs() && varTypeIsFloating(argNode->GetType()))
        {
            regNumber intArgReg = MapVarargsParamFloatRegToIntReg(argReg);
            GetEmitter()->emitIns_Mov(INS_movd, emitTypeSize(argNode->GetType()), intArgReg, argReg,
                                      /* canSkip */ false);
        }
#endif
    }

#ifdef TARGET_X86
    // TODO-MIKE-Cleanup: This can probably just use CallInfo::nextSlotNum instead of going through all args.
    target_ssize_t stackArgBytes = 0;
    for (unsigned i = 0; i < call->GetInfo()->GetArgCount(); i++)
    {
        stackArgBytes += call->GetInfo()->GetArgInfo(i)->GetSlotCount() * REGSIZE_BYTES;
    }
#endif

    // Insert a null check on "this" pointer if asked.
    if (call->NeedsNullCheck())
    {
        assert(call->GetArgInfoByArgNum(0)->GetRegNum() == REG_ARG_0);

        GetEmitter()->emitIns_AR_R(INS_cmp, EA_4BYTE, REG_ARG_0, REG_ARG_0, 0);
    }

    CORINFO_METHOD_HANDLE methHnd;
    GenTree*              target;

    if (call->IsIndirectCall())
    {
        assert(call->gtControlExpr == nullptr);

        methHnd = nullptr;
        target  = call->gtCallAddr;
    }
    else
    {
        methHnd = call->GetMethodHandle();
        target  = call->gtControlExpr;
    }

#if FEATURE_FASTTAILCALL
    // If fast tail call, then we are done.  In this case we setup the args (both reg args
    // and stack args in incoming arg area) and call target in rax.  Epilog sequence would
    // generate "jmp rax".
    if (call->IsFastTailCall())
    {
        assert(!call->IsHelperCall());

        // If this is indirect then we go through RAX with epilog sequence
        // generating "jmp rax". Otherwise epilog will try to generate a
        // rip-relative jump.
        if (target != nullptr)
        {
            UseReg(target);
            genCopyRegIfNeeded(target, REG_RAX);
        }

        return;
    }
#endif

    // For a pinvoke to unmanged code we emit a label to clear
    // the GC pointer state before the callsite.
    // We can't utilize the typical lazy killing of GC pointers
    // at (or inside) the callsite.
    if (compiler->killGCRefs(call))
    {
        genDefineTempLabel(genCreateTempLabel());
    }

    // Determine return shift size(s).
    emitAttr retSize       = EA_PTRSIZE;
    emitAttr secondRetSize = EA_UNKNOWN;

    if (call->HasMultiRegRetVal())
    {
        retSize       = emitTypeSize(call->GetRegType(0));
        secondRetSize = emitTypeSize(call->GetRegType(1));
    }
    else if (varTypeIsStruct(call->GetType()))
    {
        if (call->GetRegType(0) == TYP_REF)
        {
            retSize = EA_GCREF;
        }
        else if (call->GetRegType(0) == TYP_BYREF)
        {
            retSize = EA_BYREF;
        }
    }
    else
    {
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
    if ((compiler->lvaCallSpCheck != BAD_VAR_NUM) && call->IsUserCall())
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(compiler->lvaCallSpCheck);
        assert(lcl->lvOnFrame && lcl->lvDoNotEnregister);
        GetEmitter()->emitIns_S_R(INS_mov, EA_4BYTE, REG_SPBASE, compiler->lvaCallSpCheck, 0);
    }
#endif // defined(DEBUG) && defined(TARGET_X86)

    bool            fPossibleSyncHelperCall = false;
    CorInfoHelpFunc helperNum               = CORINFO_HELP_UNDEF;
    IL_OFFSETX      ilOffset                = BAD_IL_OFFSET;

    // We need to propagate the IL offset information to the call instruction, so we can emit
    // an IL to native mapping record for the call, to support managed return shift debugging.
    // We don't want tail call helper calls that were converted from normal calls to get a record,
    // so we skip this hash table lookup logic in that case.
    if (compiler->opts.compDbgInfo && compiler->genCallSite2ILOffsetMap != nullptr && !call->IsTailCall())
    {
        (void)compiler->genCallSite2ILOffsetMap->Lookup(call, &ilOffset);
    }

#ifdef TARGET_X86
    bool fCallerPop = call->CallerPop();

    // If the callee pops the arguments, we pass a positive shift as the argSize, and the emitter will
    // adjust its stack level accordingly.
    // If the caller needs to explicitly pop its arguments, we must pass a negative shift, and then do the
    // pop when we're done.
    target_ssize_t argSizeForEmitter = stackArgBytes;
    if (fCallerPop)
    {
        argSizeForEmitter = -stackArgBytes;
    }
#endif

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

    if (call->IsHelperCall() && ((compiler->info.compFlags & CORINFO_FLG_SYNCH) != 0))
    {
        fPossibleSyncHelperCall = true;
        helperNum               = compiler->eeGetHelperNum(methHnd);
        noway_assert(helperNum != CORINFO_HELP_UNDEF);
    }

    emitter::EmitCallType emitCallType;
    void*                 callAddr     = nullptr;
    regNumber             amBaseReg    = REG_NA;
    regNumber             amIndexReg   = REG_NA;
    unsigned              amIndexScale = 0;
    int32_t               amOffset     = 0;

    if (target != nullptr)
    {
#ifdef TARGET_X86
        if (call->IsVirtualStub() && call->IsIndirectCall())
        {
            // On x86, we need to generate a very specific pattern for indirect VSD calls:
            //
            //    3-byte nop
            //    call dword ptr [eax]
            //
            // Where EAX is also used as an argument to the stub dispatch helper. Make
            // sure that the call target address is computed into EAX in this case.

            assert(compiler->info.virtualStubParamRegNum == REG_VIRTUAL_STUB_TARGET);

            assert(target->OperIs(GT_IND) && target->isContained());

            GenTree* addr = target->AsIndir()->GetAddr();
            assert(addr->isUsedFromReg());

            genConsumeReg(addr);
            genCopyRegIfNeeded(addr, REG_VIRTUAL_STUB_TARGET);

            GetEmitter()->emitIns_Nop(3);

            emitCallType = emitter::EC_INDIR_ARD;
            amBaseReg    = REG_VIRTUAL_STUB_TARGET;
            amIndexScale = 1;
        }
        else
#endif
            if (target->isContained())
        {
            GenTree* addr = target->AsIndir()->GetAddr();

            if (GenTreeIntCon* intConAddr = addr->IsContainedIntCon())
            {
                // Note that if gtControlExpr is an indir of an absolute address, we mark it as
                // contained only if it can be encoded as PC-relative offset.
                assert(intConAddr->FitsInAddrBase(compiler));

                emitCallType = emitter::EC_FUNC_TOKEN_INDIR;
                callAddr     = reinterpret_cast<void*>(intConAddr->GetValue());
            }
            else if (GenTreeAddrMode* addrMode = addr->IsAddrMode())
            {
                emitCallType = emitter::EC_INDIR_ARD;

                if (GenTree* base = addrMode->GetBase())
                {
                    amBaseReg = UseReg(base);
                }

                if (GenTree* index = addrMode->GetIndex())
                {
                    amIndexReg   = UseReg(index);
                    amIndexScale = addrMode->GetScale();
                }

                amOffset = addrMode->GetOffset();
            }
            else
            {
                // TODO-MIKE-Review: It looks like there's no way to have a contained CLS_VAR_ADDR
                // addr here because the importer spills the target to a local. Maybe it shouldn't.

                emitCallType = emitter::EC_INDIR_ARD;

                amBaseReg = UseReg(addr);
            }
        }
        else
        {
            // We have already generated code for gtControlExpr evaluating it into a register.
            // We just need to emit "call reg" in this case.
            assert(genIsValidIntReg(target->GetRegNum()));

            emitCallType = emitter::EC_INDIR_R;
            amBaseReg    = UseReg(target);
        }
    }
#ifdef FEATURE_READYTORUN_COMPILER
    else if (call->gtEntryPoint.addr != nullptr)
    {
        emitCallType =
            call->gtEntryPoint.accessType == IAT_VALUE ? emitter::EC_FUNC_TOKEN : emitter::EC_FUNC_TOKEN_INDIR;
        callAddr = call->gtEntryPoint.addr;
    }
#endif
    else
    {
        assert(call->IsUserCall() || call->IsHelperCall());

        emitCallType = emitter::EC_FUNC_TOKEN;
        callAddr     = call->gtDirectCallAddress;

        assert(callAddr != nullptr);
    }

    if (compiler->opts.compDbgInfo && (ilOffset != BAD_IL_OFFSET))
    {
        genIPmappingAdd(ilOffset, false);
    }

    // clang-format off
    GetEmitter()->emitIns_Call(
        emitCallType,
        methHnd
        DEBUGARG(call->IsHelperCall() ? nullptr : call->callSig),
        callAddr,
#ifdef TARGET_X86
        argSizeForEmitter,
#endif
        retSize MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(secondRetSize),
        amBaseReg,
        amIndexReg,
        amIndexScale,
        amOffset,
        false);
    // clang-format on

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
    assert((liveness.GetGCRegs(TYP_REF) & (RBM_CALLEE_TRASH & ~RBM_ARG_REGS)) == RBM_NONE);
    assert((liveness.GetGCRegs(TYP_BYREF) & (RBM_CALLEE_TRASH & ~RBM_ARG_REGS)) == RBM_NONE);
    liveness.SetGCRegs(TYP_REF, liveness.GetGCRegs(TYP_REF) & ~RBM_ARG_REGS);
    liveness.SetGCRegs(TYP_BYREF, liveness.GetGCRegs(TYP_BYREF) & ~RBM_ARG_REGS);

    var_types returnType = call->TypeGet();
    if (returnType != TYP_VOID)
    {
#ifdef TARGET_X86
        if (varTypeIsFloating(returnType))
        {
            // TODO-MIKE-Review: It looks like LSRA is out of sync with codegen here,
            // it doesn't know about this spill and thinks that whatever register it
            // allocated to the call is in use, when in fact it will only be in use
            // when the user calls genConsumeReg.
            SpillST0(call);
        }
        else
#endif // TARGET_X86
        {
            if (call->HasMultiRegRetVal() || varTypeIsStruct(call->GetType()))
            {
                // If regs allocated to call node are different from ABI return
                // regs in which the call has returned its result, move the result
                // to regs allocated to call node.
                for (unsigned i = 0; i < call->GetRegCount(); ++i)
                {
                    var_types regType      = call->GetRegType(i);
                    regNumber returnReg    = call->GetRetDesc()->GetRegNum(i);
                    regNumber allocatedReg = call->GetRegNum(i);
                    inst_Mov(regType, allocatedReg, returnReg, /* canSkip */ true);
                }

#ifdef FEATURE_SIMD
                // A Vector3 return shift is stored in xmm0 and xmm1.
                // RyuJIT assumes that the upper unused bits of xmm1 are cleared but
                // the native compiler doesn't guarantee it.
                if (call->IsUnmanaged() && (returnType == TYP_SIMD12))
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

            DefCallRegs(call);
        }
    }

    // If there is nothing next, that means the result is thrown away, so this shift is not live.
    // However, for minopts or debuggable code, we keep it live to support managed return shift debugging.
    if ((call->gtNext == nullptr) && compiler->opts.OptimizationEnabled())
    {
        liveness.RemoveGCRegs(RBM_INTRET);
    }

#if defined(DEBUG) && defined(TARGET_X86)
    if ((compiler->lvaCallSpCheck != BAD_VAR_NUM) && call->IsUserCall())
    {
        regNumber spRegCheck = REG_SPBASE;

        if (!fCallerPop && (stackArgBytes != 0))
        {
            // ECX is trashed, so can be used to compute the expected SP. We saved the shift of SP
            // after pushing all the stack arguments, but the caller popped the arguments, so we need
            // to do some math to figure a good comparison.
            GetEmitter()->emitIns_Mov(INS_mov, EA_4BYTE, REG_ARG_0, REG_SPBASE, /* canSkip */ false);
            GetEmitter()->emitIns_R_I(INS_sub, EA_4BYTE, REG_ARG_0, stackArgBytes);
            GetEmitter()->emitIns_S_R(INS_cmp, EA_4BYTE, REG_ARG_0, compiler->lvaCallSpCheck, 0);
            spRegCheck = REG_ARG_0;
        }

        BasicBlock* sp_check = genCreateTempLabel();
        GetEmitter()->emitIns_S_R(INS_cmp, EA_4BYTE, spRegCheck, compiler->lvaCallSpCheck, 0);
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
                noway_assert(syncStartEmitCookie == nullptr);
                syncStartEmitCookie = GetEmitter()->emitAddLabel();
                break;
            case CORINFO_HELP_MON_EXIT:
            case CORINFO_HELP_MON_EXIT_STATIC:
                noway_assert(syncEndEmitCookie == nullptr);
                syncEndEmitCookie = GetEmitter()->emitAddLabel();
                break;
            default:
                break;
        }
    }
#endif // !FEATURE_EH_FUNCLETS

#ifdef TARGET_X86
    unsigned stackAdjustBias = 0;

    if (fCallerPop && (stackArgBytes != 0))
    {
        stackAdjustBias = stackArgBytes;
    }

    SubtractStackLevel(stackArgBytes);

    // TODO-MIKE-Consider: Emit a breakpoint after CORINFO_HELP_TAILCALL since it never returns.
    // if (call->IsTailCallViaJitHelper())
    // {
    //     instGen(INS_BREAKPOINT);
    //     return;
    // }

    genRemoveAlignmentAfterCall(call, stackAdjustBias);
#endif
}

// Produce code for a GT_JMP node.
// The arguments of the caller needs to be transferred to the callee before exiting caller.
// The actual jump to callee is generated as part of caller epilog sequence.
// Therefore the codegen of GT_JMP is to ensure that the callee arguments are correctly setup.
void CodeGen::GenJmp(GenTree* jmp)
{
    assert(jmp->OperIs(GT_JMP));
    assert(compiler->compJmpOpUsed);

#ifdef PROFILING_SUPPORTED
    genProfilingLeaveCallback(CORINFO_HELP_PROF_FCN_TAILCALL);
#endif

    // Move any register parameters back to their register.

    for (unsigned lclNum = 0; lclNum < compiler->info.compArgsCount; lclNum++)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

        noway_assert(lcl->IsParam() && !lcl->IsPromoted());

        if (!lcl->IsRegParam())
        {
            continue;
        }

        // We expect all params to be DNER, otherwise we'd need to deal with moving between
        // assigned registers and param registers and potential circular dependencies.
        noway_assert(lcl->lvDoNotEnregister);

#ifdef UNIX_AMD64_ABI
        if (varTypeIsStruct(lcl->GetType()))
        {
            assert(lcl->GetLayout()->GetSysVAmd64AbiRegCount() != 0);

            var_types type = varActualType(lcl->GetLayout()->GetSysVAmd64AbiRegType(0));
            regNumber reg  = lcl->GetParamReg(0);

            GetEmitter()->emitIns_R_S(ins_Load(type), emitTypeSize(type), reg, lclNum, 0);
            liveness.AddLiveLclRegs(genRegMask(reg));
            liveness.SetGCRegType(reg, type);

            if (lcl->GetLayout()->GetSysVAmd64AbiRegCount() > 1)
            {
                type = varActualType(lcl->GetLayout()->GetSysVAmd64AbiRegType(1));
                reg  = lcl->GetParamReg(1);

                GetEmitter()->emitIns_R_S(ins_Load(type), emitTypeSize(type), reg, lclNum, 8);
                liveness.AddLiveLclRegs(genRegMask(reg));
                liveness.SetGCRegType(reg, type);
            }

            liveness.RemoveGCSlot(lcl);

            continue;
        }
#endif // UNIX_AMD64_ABI

        var_types type = lcl->GetType();

        if (varTypeIsStruct(type))
        {
            assert(lcl->GetLayout()->GetSize() <= REGSIZE_BYTES);

#ifdef TARGET_X86
            type = TYP_INT;
#else
            type = lcl->GetLayout()->GetSize() <= 4 ? TYP_INT : lcl->GetLayout()->GetGCPtrType(0);
#endif
        }

        regNumber reg = lcl->GetParamReg();
        assert(isValidIntArgReg(reg) || isValidFloatArgReg(reg));

        GetEmitter()->emitIns_R_S(ins_Load(type), emitTypeSize(type), reg, lclNum, 0);
        liveness.AddLiveLclRegs(genRegMask(reg));
        liveness.SetGCRegType(reg, type);
        liveness.RemoveGCSlot(lcl);
    }

#ifdef WINDOWS_AMD64_ABI
    if (!compiler->info.compIsVarArgs)
    {
        return;
    }

    // For varargs we need to load all arg registers, not just those associated with parameters.
    // x86 does not need this because the variable arguments of a varargs methods are never
    // passed in registers.

    regMaskTP varargsIntRegMask = RBM_ARG_REGS;

    for (unsigned lclNum = 0; lclNum < compiler->info.compArgsCount; lclNum++)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

        if (lcl->IsRegParam())
        {
            regNumber reg = lcl->GetParamReg();

            if (emitter::isFloatReg(reg))
            {
                regNumber intReg = MapVarargsParamFloatRegToIntReg(reg);
                GetEmitter()->emitIns_Mov(INS_movd, EA_8BYTE, intReg, reg, /*canSkip*/ false);
                reg = intReg;
            }

            assert(isValidIntArgReg(reg));
            varargsIntRegMask &= ~genRegMask(reg);
        }
    }

    if (varargsIntRegMask == RBM_NONE)
    {
        return;
    }

    assert(compiler->lvaGetDesc(0u)->GetParamReg() == REG_RCX);

    // We have no way of knowing if args contain GC references.
    GetEmitter()->emitDisableGC();

    for (unsigned i = 0; i < MAX_REG_ARG; ++i)
    {
        regNumber reg = intArgRegs[i];

        if ((varargsIntRegMask & genRegMask(reg)) != 0)
        {
            GetEmitter()->emitIns_R_S(INS_mov, EA_8BYTE, reg, 0, i * REGSIZE_BYTES);
            GetEmitter()->emitIns_Mov(INS_movd, EA_8BYTE, MapVarargsParamIntRegToFloatReg(reg), reg, /*canSkip*/ false);
        }
    }

    // The epilog, which is not interruptible, should follow right after this code.
    GetEmitter()->emitEnableGC();
#endif // WINDOWS_AMD64_ABI
}

void CodeGen::GenJmpEpilog(BasicBlock* block)
{
    noway_assert(block->bbJumpKind == BBJ_RETURN);
    noway_assert(block->GetFirstLIRNode());

    // figure out what jump we have
    GenTree* jmpNode = block->lastNode();
#if !FEATURE_FASTTAILCALL
    // x86
    GenTreeJmp* jmp = jmpNode->IsJmp();
    noway_assert(jmp != nullptr);
#else
    // amd64
    // If jmpNode is GT_JMP then gtNext must be null.
    // If jmpNode is a fast tail call, gtNext need not be null since it could have embedded stmts.
    noway_assert(!jmpNode->OperIs(GT_JMP) || (jmpNode->gtNext == nullptr));

    // Could either be a "jmp method" or "fast tail call" implemented as epilog+jmp
    noway_assert(jmpNode->OperIs(GT_JMP) || (jmpNode->OperIs(GT_CALL) && jmpNode->AsCall()->IsFastTailCall()));

    // The next block is associated with this "if" stmt
    if (GenTreeJmp* jmp = jmpNode->IsJmp())
#endif
    {
        CORINFO_CONST_LOOKUP addrInfo;
        compiler->info.compCompHnd->getFunctionEntryPoint(jmp->GetMethodHandle(), &addrInfo);

        emitter::EmitCallType callType   = emitter::EC_FUNC_TOKEN_INDIR;
        void*                 addr       = addrInfo.addr;
        regNumber             indCallReg = REG_NA;

        if (addrInfo.accessType == IAT_PVALUE)
        {
#ifdef TARGET_AMD64
            if (!compiler->eeIsRIPRelativeAddress(addrInfo.addr))
            {
                callType   = emitter::EC_INDIR_ARD;
                indCallReg = REG_RAX;
                instGen_Set_Reg_To_Imm(EA_HANDLE_CNS_RELOC, indCallReg, reinterpret_cast<ssize_t>(addr));
                addr = nullptr;
            }
#endif
        }
        else
        {
            noway_assert(addrInfo.accessType == IAT_VALUE);

            callType = emitter::EC_FUNC_TOKEN;
        }

        // clang-format off
        GetEmitter()->emitIns_Call(
            callType,
            jmp->GetMethodHandle()
            DEBUGARG(nullptr),
            addr,
#ifdef TARGET_X86
            0,                                                      
#endif
            EA_UNKNOWN MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(EA_UNKNOWN),        
            indCallReg, REG_NA, 0, 0, 
            true 
        );
        // clang-format on
    }
#if FEATURE_FASTTAILCALL
    else
    {
#ifdef TARGET_AMD64
        // Fast tail call.
        GenTreeCall* call = jmpNode->AsCall();

        assert(!call->IsHelperCall());

        // Calls to a user func can be dispatched as an RIP-relative jump when they are
        // truly direct; in this case, the control expression will be null and the direct
        // target address will be in gtDirectCallAddress. It is still possible that calls
        // to user funcs require indirection, in which case the control expression will
        // be non-null.
        if (call->IsUserCall() && (call->gtControlExpr == nullptr))
        {
            assert(call->GetMethodHandle() != nullptr);

            // clang-format off
            GetEmitter()->emitIns_Call(
                emitter::EC_FUNC_TOKEN,
                call->GetMethodHandle()
                DEBUGARG(nullptr),
                call->gtDirectCallAddress,
#ifdef TARGET_X86
                0,        
#endif
                EA_UNKNOWN MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(EA_UNKNOWN),
                REG_NA, REG_NA, 0, 0,
                true
            );
            // clang-format on
        }
        else
        {
            // Target requires indirection to obtain. genCallInstruction will have materialized
            // it into RAX already, so just jump to it. The stack walker requires that a register
            // indirect tail call be rex.w prefixed.
            GetEmitter()->emitIns_R(INS_rex_jmp, EA_PTRSIZE, REG_RAX);
        }

#else
        assert(!"Fast tail call as epilog+jmp");
        unreached();
#endif // TARGET_AMD64
    }
#endif // FEATURE_FASTTAILCALL
}

void CodeGen::genLeaInstruction(GenTreeAddrMode* lea)
{
    regNumber baseReg  = lea->GetBase() == nullptr ? REG_NA : UseReg(lea->GetBase());
    regNumber indexReg = lea->GetIndex() == nullptr ? REG_NA : UseReg(lea->GetIndex());
    regNumber dstReg   = lea->GetRegNum();
    // TODO-MIKE-Cleanup: The emitter is dumb and wants scale 1 when index is not present.
    unsigned scale  = indexReg == REG_NA ? 1 : lea->GetScale();
    int      offset = lea->GetOffset();

    GetEmitter()->emitIns_R_ARX(INS_lea, emitTypeSize(lea->GetType()), dstReg, baseReg, indexReg, scale, offset);

    DefReg(lea);
}

void CodeGen::GenFloatCompare(GenTreeOp* cmp)
{
    assert(cmp->OperIsCompare());

    GenTree*  op1  = cmp->GetOp(0);
    GenTree*  op2  = cmp->GetOp(1);
    var_types type = op1->GetType();

    genConsumeRegs(op1);
    genConsumeRegs(op2);

    assert(varTypeIsFloating(type));
    assert(type == op2->GetType());

    GenCondition condition = GenCondition::FromFloatRelop(cmp);

    if (condition.PreferSwap())
    {
        condition = GenCondition::Swap(condition);
        std::swap(op1, op2);
    }

    emitInsBinary(ins_FloatCompare(type), emitTypeSize(type), op1, op2);

    if (cmp->GetRegNum() == REG_NA)
    {
        return;
    }

    if ((condition == GenCondition::FNEU) && (op1->GetRegNum() == op2->GetRegNum()))
    {
        // For floating point, `x != x` is a common way of checking for NaN.
        // So, in the case where both operands are the same, we can optimize
        // codegen to only do a single check.
        condition = GenCondition::P;
    }

    inst_SETCC(condition, cmp->GetType(), cmp->GetRegNum());
    genProduceReg(cmp);
}

void CodeGen::GenIntCompare(GenTreeOp* cmp)
{
    assert(cmp->OperIsCompare() || cmp->OperIs(GT_CMP));

    GenTree*  op1    = cmp->GetOp(0);
    GenTree*  op2    = cmp->GetOp(1);
    var_types type1  = op1->GetType();
    var_types type2  = op2->GetType();
    regNumber dstReg = cmp->GetRegNum();

    genConsumeRegs(op1);
    genConsumeRegs(op2);

    assert(!op1->IsContainedIntCon());
    assert(!varTypeIsFloating(type2));

    instruction ins           = INS_cmp;
    var_types   type          = TYP_UNDEF;
    bool        canReuseFlags = false;

    if (cmp->OperIs(GT_TEST_EQ, GT_TEST_NE))
    {
        // Unlike many x86 instructions TEST doesn't have a form with a 16/32/64 bit first operand and
        // an 8 bit immediate second operand. But if the immediate shift fits in 8 bits then we can simply
        // emit a 8 bit TEST instruction, unless we're targeting x86 and the first operand is not a byte
        // register.
        // Note that lowering does something similar but its main purpose is to allow memory operands to be
        // contained so it doesn't handle other kind of operands. It could do more but on x86 that results
        // in additional register constrains and that may be worse than wasting 3 bytes on an immediate.
        if (
#ifdef TARGET_X86
            (!op1->isUsedFromReg() || isByteReg(op1->GetRegNum())) &&
#endif
            (op2->IsIntCon() && (op2->AsIntCon()->GetUnsignedValue() <= 255)))
        {
            type = TYP_UBYTE;
        }

        ins = INS_test;
    }
    else if (op1->isUsedFromReg() && op2->IsIntegralConst(0))
    {
        if (compiler->opts.OptimizationEnabled())
        {
            // Extract the sign bit for "x < 0" and "x >= 0" if we're evaluating the result into a register.
            // Morph/Lowering are responsible to transform "0 < x" to "x > 0" so we won't handle it here.
            if ((dstReg != REG_NA) && cmp->OperIs(GT_LT, GT_GE) && !cmp->IsUnsigned())
            {
                emitAttr attr = emitActualTypeSize(type1);

                inst_Mov(op1->GetType(), dstReg, op1->GetRegNum(), /*canSkip*/ true);

                if (cmp->OperIs(GT_GE))
                {
                    GetEmitter()->emitIns_R(INS_not, attr, dstReg);
                }

                GetEmitter()->emitIns_R_I(INS_shr_N, attr, dstReg, EA_SIZE(attr) * 8 - 1);
                DefReg(cmp);

                return;
            }

            canReuseFlags = true;
        }

        // We're comparing a register to 0 so we can generate "test reg1, reg1"
        // instead of the longer "cmp reg1, 0"
        ins = INS_test;
        op2 = op1;
    }

    if (type == TYP_UNDEF)
    {
        if (type1 == type2)
        {
            // 16 bit instructions are best avoided due to the extra 66h prefix and possible LCP stalls.
            // We only need to generate a 16 bit instruction is we have a contained memory operand.
            // It could also be useful to generate a 16 bit instruction to avoid casts to (U)SHORT but
            // lowering doesn't currently handle this case, it only removes casts to UBYTE.

            if (varTypeIsShort(type1) && !op1->isContained() && (!op2->isContained() || op2->IsIntCon()))
            {
                type = TYP_INT;
            }
            else
            {
                type = type1;
            }
        }
        else if (varTypeSize(type1) == varTypeSize(type2))
        {
            // If the types are different but have the same size then we'll use TYP_INT or TYP_LONG.
            // This primarily deals with small type mixes (e.g. byte/ubyte) that need to be widened
            // and compared as int. We should not get long type mixes here but handle that as well
            // just in case.
            type = varTypeSize(type1) == 8 ? TYP_LONG : TYP_INT;
        }
        else
        {
            // In the types are different simply use TYP_INT. This deals with small type/int type
            // mixes (e.g. byte/short ubyte/int) that need to be widened and compared as int.
            // Lowering is expected to handle any mixes that involve long types (e.g. int/long).
            type = TYP_INT;
        }

        // The common type cannot be smaller than any of the operand types, we're probably mixing int/long
        assert(varTypeSize(type) >= Max(varTypeSize(type1), varTypeSize(type2)));
        // Small unsigned int types should use unsigned comparisons
        assert(!(varTypeIsSmallInt(type) && varTypeIsUnsigned(type)) || cmp->IsUnsigned());
        // If op1 is smaller then it cannot be in memory, we're probably missing a cast
        assert((varTypeSize(type1) >= varTypeSize(type)) || !op1->isContained());
        // If op2 is smaller then it cannot be in memory, we're probably missing a cast
        assert((varTypeSize(type2) >= varTypeSize(type)) || !op2->isContained() || op2->IsIntCon());
        // If we ended up with a small type and op2 is a constant then make sure we don't lose constant bits
        assert(!op2->IsIntCon() || !varTypeIsSmall(type) ||
               varTypeSmallIntCanRepresentValue(type, op2->AsIntCon()->GetValue()));
    }

    // The type cannot be larger than the machine word size
    assert(varTypeSize(type) <= varTypeSize(TYP_I_IMPL));
    // TYP_UINT and TYP_ULONG should not appear here, only small types can be unsigned
    assert(!varTypeIsUnsigned(type) || varTypeIsSmall(type));

    emitAttr attr = emitTypeSize(type);

    if (canReuseFlags && GetEmitter()->AreFlagsSetToZeroCmp(op1->GetRegNum(), attr, cmp->GetOper()))
    {
        JITDUMP("Not emitting compare due to flags being already set\n");
    }
    else
    {
        emitInsBinary(ins, attr, op1, op2);
    }

    if (dstReg == REG_NA)
    {
        return;
    }

    inst_SETCC(GenCondition::FromIntegralRelop(cmp), cmp->GetType(), dstReg);
    DefReg(cmp);
}

#ifndef TARGET_64BIT
void CodeGen::genLongToIntCast(GenTreeCast* cast)
{
    assert(cast->TypeIs(TYP_INT));

    GenTreeOp* src = cast->GetOp(0)->AsOp();
    noway_assert(src->OperIs(GT_LONG));

    regNumber loSrcReg = UseReg(src->GetOp(0));
    regNumber hiSrcReg = UseReg(src->GetOp(1));
    regNumber dstReg   = cast->GetRegNum();

    assert(genIsValidIntReg(loSrcReg));
    assert(genIsValidIntReg(hiSrcReg));
    assert(genIsValidIntReg(dstReg));

    if (cast->gtOverflow())
    {
        var_types srcType = cast->IsUnsigned() ? TYP_ULONG : TYP_LONG;
        var_types dstType = cast->GetCastType();
        assert((dstType == TYP_INT) || (dstType == TYP_UINT));

        // Generate an overflow check for [u]long to [u]int casts:
        //
        // long  -> int  - check if the upper 33 bits are all 0 or all 1
        //
        // ulong -> int  - check if the upper 33 bits are all 0
        //
        // long  -> uint - check if the upper 32 bits are all 0
        // ulong -> uint - check if the upper 32 bits are all 0

        if ((srcType == TYP_LONG) && (dstType == TYP_INT))
        {
            BasicBlock* allOne  = genCreateTempLabel();
            BasicBlock* success = genCreateTempLabel();

            inst_RV_RV(INS_test, loSrcReg, loSrcReg, TYP_INT);
            inst_JMP(EJ_js, allOne);

            inst_RV_RV(INS_test, hiSrcReg, hiSrcReg, TYP_INT);
            genJumpToThrowHlpBlk(EJ_jne, ThrowHelperKind::Overflow);
            inst_JMP(EJ_jmp, success);

            genDefineTempLabel(allOne);
            inst_RV_IV(INS_cmp, hiSrcReg, -1, EA_4BYTE);
            genJumpToThrowHlpBlk(EJ_jne, ThrowHelperKind::Overflow);

            genDefineTempLabel(success);
        }
        else
        {
            if ((srcType == TYP_ULONG) && (dstType == TYP_INT))
            {
                inst_RV_RV(INS_test, loSrcReg, loSrcReg, TYP_INT);
                genJumpToThrowHlpBlk(EJ_js, ThrowHelperKind::Overflow);
            }

            inst_RV_RV(INS_test, hiSrcReg, hiSrcReg, TYP_INT);
            genJumpToThrowHlpBlk(EJ_jne, ThrowHelperKind::Overflow);
        }
    }

    inst_Mov(TYP_INT, dstReg, loSrcReg, /* canSkip */ true);

    DefReg(cast);
}
#endif

//------------------------------------------------------------------------
// genIntCastOverflowCheck: Generate overflow checking code for an integer cast.
//
// Arguments:
//    cast - The GT_CAST node
//    desc - The cast description
//    reg  - The register containing the shift to check
//
void CodeGen::genIntCastOverflowCheck(GenTreeCast* cast, const GenIntCastDesc& desc, regNumber reg)
{
    switch (desc.CheckKind())
    {
        case GenIntCastDesc::CHECK_POSITIVE:
            GetEmitter()->emitIns_R_R(INS_test, EA_SIZE(desc.CheckSrcSize()), reg, reg);
            genJumpToThrowHlpBlk(EJ_jl, ThrowHelperKind::Overflow);
            break;

#ifdef TARGET_64BIT
        case GenIntCastDesc::CHECK_UINT_RANGE:
        {
            // We need to check if the shift is not greater than 0xFFFFFFFF but this shift
            // cannot be encoded in an immediate operand. Use a right shift to test if the
            // upper 32 bits are zero. This requires a temporary register.
            const regNumber tempReg = cast->GetSingleTempReg();
            assert(tempReg != reg);
            GetEmitter()->emitIns_Mov(INS_mov, EA_8BYTE, tempReg, reg, /* canSkip */ false);
            GetEmitter()->emitIns_R_I(INS_shr_N, EA_8BYTE, tempReg, 32);
            genJumpToThrowHlpBlk(EJ_jne, ThrowHelperKind::Overflow);
        }
        break;

        case GenIntCastDesc::CHECK_POSITIVE_INT_RANGE:
            GetEmitter()->emitIns_R_I(INS_cmp, EA_8BYTE, reg, INT32_MAX);
            genJumpToThrowHlpBlk(EJ_ja, ThrowHelperKind::Overflow);
            break;

        case GenIntCastDesc::CHECK_INT_RANGE:
            GetEmitter()->emitIns_R_I(INS_cmp, EA_8BYTE, reg, INT32_MAX);
            genJumpToThrowHlpBlk(EJ_jg, ThrowHelperKind::Overflow);
            GetEmitter()->emitIns_R_I(INS_cmp, EA_8BYTE, reg, INT32_MIN);
            genJumpToThrowHlpBlk(EJ_jl, ThrowHelperKind::Overflow);
            break;
#endif

        default:
        {
            assert(desc.CheckKind() == GenIntCastDesc::CHECK_SMALL_INT_RANGE);
            const int castMaxValue = desc.CheckSmallIntMax();
            const int castMinValue = desc.CheckSmallIntMin();

            GetEmitter()->emitIns_R_I(INS_cmp, EA_SIZE(desc.CheckSrcSize()), reg, castMaxValue);
            genJumpToThrowHlpBlk((castMinValue == 0) ? EJ_ja : EJ_jg, ThrowHelperKind::Overflow);

            if (castMinValue != 0)
            {
                GetEmitter()->emitIns_R_I(INS_cmp, EA_SIZE(desc.CheckSrcSize()), reg, castMinValue);
                genJumpToThrowHlpBlk(EJ_jl, ThrowHelperKind::Overflow);
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

        emitInsBinary(ins, EA_ATTR(desc.LoadSrcSize()), cast, src);

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

void CodeGen::genFloatToFloatCast(GenTreeCast* cast)
{
    assert(cast->GetType() == cast->GetCastType());
    assert(!cast->gtOverflow());

    GenTree*  src     = cast->GetOp(0);
    var_types srcType = src->GetType();
    var_types dstType = cast->GetType();

    assert((srcType == TYP_FLOAT) || (srcType == TYP_DOUBLE));
    assert((dstType == TYP_FLOAT) || (dstType == TYP_DOUBLE));

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
        emitInsBinary(ins, insSize, cast, src);
    }

    genProduceReg(cast);
}

void CodeGen::genIntToFloatCast(GenTreeCast* cast)
{
    assert(cast->GetType() == cast->GetCastType());
    assert(!cast->gtOverflow());

    GenTree*  src     = cast->GetOp(0);
    var_types srcType = varActualType(src->GetType());
    var_types dstType = cast->GetType();

    if (cast->IsUnsigned())
    {
        srcType = varTypeToUnsigned(srcType);
    }

#ifdef TARGET_64BIT
    noway_assert((srcType == TYP_INT) || (srcType == TYP_LONG) || (srcType == TYP_ULONG));
#else
    noway_assert(srcType == TYP_INT);
#endif

    assert((dstType == TYP_FLOAT) || (dstType == TYP_DOUBLE));

    genConsumeRegs(src);
    regNumber srcReg = src->isUsedFromReg() ? src->GetRegNum() : REG_NA;
    regNumber dstReg = cast->GetRegNum();

    assert((srcReg == REG_NA) || genIsValidIntReg(srcReg));
    assert(genIsValidFloatReg(dstReg));

    // The source shift is never a small int but it may be produced by a small int typed
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

    emitInsBinary(ins, insSize, cast, src);

#ifdef TARGET_64BIT
    // Handle the case of srcType = TYP_ULONG. SSE2 conversion instruction
    // will interpret ULONG shift as LONG.  Hence we need to adjust the
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
            GetEmitter()->emitIns_R_C(INS_addsd, EA_8BYTE, dstReg, u8ToDblBitmask);
        }
        else
        {
            if (u8ToFltBitmask == nullptr)
            {
                u8ToFltBitmask = GetEmitter()->emitFltOrDblConst(jitstd::bit_cast<float>(0x5f800000U), EA_4BYTE);
            }
            GetEmitter()->emitIns_R_C(INS_addss, EA_4BYTE, dstReg, u8ToFltBitmask);
        }

        genDefineTempLabel(label);
    }
#endif

    genProduceReg(cast);
}

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

    emitInsBinary(ins, insSize, cast, src);

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
    assert(treeNode->OperIs(GT_CKFINITE));

    GenTree*  op1        = treeNode->AsUnOp()->GetOp(0);
    var_types targetType = treeNode->GetType();
    int       expMask    = targetType == TYP_FLOAT ? 0x7F800000 : 0x7FF00000; // Bit mask to extract exponent.
    regNumber targetReg  = treeNode->GetRegNum();

    // Extract exponent into a register.
    regNumber tmpReg = treeNode->GetSingleTempReg();
    regNumber srcReg = UseReg(op1);

#ifdef TARGET_64BIT
    // Copy the floating-point shift to an integer register. If we copied a float to a long, then
    // right-shift the shift so the high 32 bits of the floating-point shift sit in the low 32
    // bits of the integer register.
    inst_Mov(targetType == TYP_FLOAT ? TYP_INT : TYP_LONG, tmpReg, srcReg, /* canSkip */ false);

    if (targetType == TYP_DOUBLE)
    {
        // right shift by 32 bits to get to exponent.
        inst_RV_SH(INS_shr, EA_8BYTE, tmpReg, 32);
    }

    // Mask exponent with all 1's and check if the exponent is all 1's
    inst_RV_IV(INS_and, tmpReg, expMask, EA_4BYTE);
    inst_RV_IV(INS_cmp, tmpReg, expMask, EA_4BYTE);

    // If exponent is all 1's, throw ArithmeticException
    genJumpToThrowHlpBlk(EJ_je, ThrowHelperKind::Arithmetic);

    // if it is a finite shift copy it to targetReg
    inst_Mov(targetType, targetReg, srcReg, /* canSkip */ true);

#else // !TARGET_64BIT

    // If the target type is TYP_DOUBLE, we want to extract the high 32 bits into the register.
    // There is no easy way to do this. To not require an extra register, we'll use shuffles
    // to move the high 32 bits into the low 32 bits, then shuffle it back, since we
    // need to produce the shift into the target register.
    //
    // For TYP_DOUBLE, we'll generate (for targetReg != op1->GetRegNum()):
    //    movaps targetReg, op1->GetRegNum()
    //    shufps targetReg, targetReg, 0xB1    // WZYX => ZWXY
    //    mov_xmm2i tmpReg, targetReg          // tmpReg <= Y
    //    and tmpReg, <mask>
    //    cmp tmpReg, <mask>
    //    je <throw block>
    //    movaps targetReg, op1->GetRegNum()   // copy the shift again, instead of un-shuffling it
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
        inst_Mov(targetType, targetReg, srcReg, /* canSkip */ true);
        GetEmitter()->emitIns_R_R_I(INS_shufps, EA_16BYTE, targetReg, targetReg, (int8_t)0xb1);
        copyToTmpSrcReg = targetReg;
    }
    else
    {
        copyToTmpSrcReg = srcReg;
    }

    // Copy only the low 32 bits. This will be the high order 32 bits of the floating-point
    // shift, no matter the floating-point type.
    inst_Mov(TYP_INT, tmpReg, copyToTmpSrcReg, /* canSkip */ false);

    // Mask exponent with all 1's and check if the exponent is all 1's
    inst_RV_IV(INS_and, tmpReg, expMask, EA_4BYTE);
    inst_RV_IV(INS_cmp, tmpReg, expMask, EA_4BYTE);

    // If exponent is all 1's, throw ArithmeticException
    genJumpToThrowHlpBlk(EJ_je, ThrowHelperKind::Arithmetic);

    if ((targetType == TYP_DOUBLE) && (targetReg == srcReg))
    {
        // We need to re-shuffle the targetReg to get the correct result.
        GetEmitter()->emitIns_R_R_I(INS_shufps, EA_16BYTE, targetReg, targetReg, (int8_t)0xb1);
    }
    else
    {
        // In both the TYP_FLOAT and TYP_DOUBLE case, the op1 register is untouched,
        // so copy it to the targetReg. This is faster and smaller for TYP_DOUBLE
        // than re-shuffling the targetReg.
        inst_Mov(targetType, targetReg, srcReg, /* canSkip */ true);
    }

#endif // !TARGET_64BIT

    DefReg(treeNode);
}

// Return the "total" size of the stack frame, including local size and
// callee-saved register size. There are a few things "missing" depending on
// the platform. The function genCallerSPtoInitialSPdelta() includes those things.
// It doesn't include the pushed return address.
// For x86, this doesn't include the frame pointer if isFramePointerUsed is true.
int CodeGenInterface::genTotalFrameSize() const
{
    assert(calleeRegsPushed != UINT_MAX);

    int totalFrameSize = calleeRegsPushed * REGSIZE_BYTES + lclFrameSize;

    assert(totalFrameSize >= 0);
    return totalFrameSize;
}

// Return the offset from Caller-SP to Initial SP.
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

// Return the offset from Caller-SP to the frame pointer.
// This number is going to be negative, since the Caller-SP is at a higher
// address than the frame pointer.
//
// We can't compute this directly from the Caller-SP, since the frame pointer
// is based on a maximum delta from Initial-SP, so first we find SP, then
// compute the FP offset.
int CodeGenInterface::genCallerSPtoFPdelta() const
{
    assert(isFramePointerUsed());

#ifdef TARGET_AMD64
    int callerSPtoFPdelta = genCallerSPtoInitialSPdelta() + genSPtoFPdelta();
    assert(callerSPtoFPdelta <= 0);
    return callerSPtoFPdelta;
#else
    // Thanks to ebp chaining, the difference between ebp-based addresses
    // and caller-SP-relative addresses is just the 2 pointers: the return
    // address and the pushed frame pointer.
    return -2 * REGSIZE_BYTES;
#endif
}

// Return the offset from SP to the frame pointer.
// This number is going to be positive, since SP must be at the lowest
// address.
int CodeGenInterface::genSPtoFPdelta() const
{
#ifdef TARGET_X86
    int delta = -genCallerSPtoInitialSPdelta() + genCallerSPtoFPdelta();
    assert(delta >= 0);
    return delta;
#elif defined(TARGET_AMD64)
#ifdef WINDOWS_AMD64_ABI
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
        return Min(240, static_cast<int>(outgoingArgSpaceSize));
    }

    if (compiler->opts.compDbgEnC)
    {
        // vm assumption on EnC methods is that rsp and rbp are equal
        return 0;
    }
#endif // WINDOWS_AMD64_ABI

    // We require frame chaining on Unix to support native tool unwinding (such as
    // unwinding by the native debugger). We have a CLR-only extension to the
    // unwind codes (UWOP_SET_FPREG_LARGE) to support SP->FP offsets larger than 240.
    // If Unix ever supports EnC, the RSP == RBP assumption will have to be reevaluated.
    return genTotalFrameSize();
#endif
}

void CodeGen::genSSE41RoundOp(GenTreeIntrinsic* treeNode)
{
    assert(compiler->compIsaSupportedDebugOnly(InstructionSet_SSE41));

    GenTree* srcNode = treeNode->GetOp(0);

    assert(varTypeIsFloating(srcNode->GetType()) && (srcNode->GetType() == treeNode->GetType()));

    genConsumeRegs(srcNode);

    instruction ins    = treeNode->TypeIs(TYP_FLOAT) ? INS_roundss : INS_roundsd;
    emitAttr    size   = emitTypeSize(treeNode->GetType());
    regNumber   dstReg = treeNode->GetRegNum();
    unsigned    imm    = 0;

    switch (treeNode->AsIntrinsic()->GetIntrinsic())
    {
        case NI_System_Math_Round:
            imm = 4;
            break;
        case NI_System_Math_Ceiling:
            imm = 10;
            break;
        case NI_System_Math_Floor:
            imm = 9;
            break;
        default:
            unreached();
    }

    // TODO-MIKE-Cleanup: This shouldn't be needed but emitIns_SIMD_R_R_I is messed up.
    if (srcNode->isUsedFromReg())
    {
        GetEmitter()->emitIns_R_R_I(ins, size, dstReg, srcNode->GetRegNum(), imm);
    }
    else
    {
        // TODO-MIKE-CQ: Remove false dependency.
        inst_RV_TT_IV(ins, size, dstReg, srcNode, imm);
    }
}

void CodeGen::genIntrinsic(GenTreeIntrinsic* node)
{
    assert(varTypeIsFloating(node->GetType()));

    switch (node->GetIntrinsic())
    {
        case NI_System_Math_Abs:
            GenFloatAbs(node);
            break;

        case NI_System_Math_Ceiling:
        case NI_System_Math_Floor:
        case NI_System_Math_Round:
            genSSE41RoundOp(node);
            break;

        case NI_System_Math_Sqrt:
        {
            GenTree* src = node->GetOp(0);
            assert(src->GetType() == node->GetType());
            genConsumeRegs(src);
            emitInsBinary(ins_FloatSqrt(node->GetType()), emitTypeSize(node->GetType()), node, src);
            break;
        }

        default:
            assert(!"genIntrinsic: Unsupported intrinsic");
            unreached();
    }

    DefReg(node);
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
        instruction ins    = ins_Load(dstType, IsSimdLocalAligned(lclNum));
        GetEmitter()->emitIns_R_S(ins, emitTypeSize(dstType), dstReg, lclNum, 0);
    }
    else
    {
        inst_BitCast(dstType, dstReg, src->GetType(), src->GetRegNum());
    }

    genProduceReg(bitcast);
}

void CodeGen::genAlignStackBeforeCall(GenTreePutArgStk* putArgStk)
{
#if defined(UNIX_X86_ABI)

    genAlignStackBeforeCall(putArgStk->GetCall());

#endif // UNIX_X86_ABI
}

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

#ifdef TARGET_X86
void CodeGen::genRemoveAlignmentAfterCall(GenTreeCall* call, unsigned bias)
{
#ifdef UNIX_X86_ABI
    // Put back the stack pointer if there was any padding for stack alignment
    unsigned padStkAlign  = call->GetInfo()->GetStkAlign();
    unsigned padStkAdjust = padStkAlign + bias;

    if (padStkAdjust != 0)
    {
        inst_RV_IV(INS_add, REG_SPBASE, padStkAdjust, EA_PTRSIZE);
        SubtractStackLevel(padStkAlign);
        SubtractNestedAlignment(padStkAlign);
    }
#else
    if (bias != 0)
    {
        if (bias == 4)
        {
            inst_RV(INS_pop, REG_ECX, TYP_INT);
        }
        else
        {
            inst_RV_IV(INS_add, REG_SPBASE, bias, EA_PTRSIZE);
        }
    }
#endif
}

void CodeGen::genPreAdjustStackForPutArgStk(unsigned argSize)
{
    // If argSize is large, we need to probe the stack like we do in the prolog (PrologAllocLclFrame)
    // or for localloc (genLclHeap), to ensure we touch the stack pages sequentially, and don't miss
    // the stack guard pages. The prolog probes, but we don't know at this point how much higher
    // the last probed stack pointer shift is. We default a threshold. Any size below this threshold
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

void CodeGen::genPutArgStkFieldList(GenTreePutArgStk* putArgStk)
{
    GenTreeFieldList* const fieldList = putArgStk->gtOp1->AsFieldList();
    assert(fieldList != nullptr);

    assert((putArgStk->GetKind() == GenTreePutArgStk::Kind::Push) ||
           (putArgStk->GetKind() == GenTreePutArgStk::Kind::PushAllSlots));

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

    emitter* emit = GetEmitter();

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
                emit->emitIns_I(INS_push, EA_4BYTE, 0);
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
                GetEmitter()->emitIns_R_I(INS_sub, EA_4BYTE, REG_SPBASE, adjustment);
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
                assert(varTypeSize(varActualType(fieldType)) <= 4);

                unsigned lclNum;
                unsigned lclOffs;

                if (IsLocalMemoryOperand(fieldNode, &lclNum, &lclOffs))
                {
                    emit->emitIns_S(INS_push, emitActualTypeSize(fieldNode->GetType()), lclNum, lclOffs);
                }
                else if (fieldNode->IsIconHandle())
                {
                    emit->emitIns_I(INS_push, EA_HANDLE_CNS_RELOC, fieldNode->AsIntCon()->GetValue());
                }
                else
                {
                    emit->emitIns_I(INS_push, EA_4BYTE, fieldNode->AsIntCon()->GetValue());
                }

                currentOffset -= TARGET_POINTER_SIZE;
                AddStackLevel(TARGET_POINTER_SIZE);
            }
            else
            {
                // The stack has been adjusted and we will load the field to intTmpReg and then store it on the stack.
                assert(varTypeIsIntegralOrI(fieldNode->GetType()));

                // TODO-MIKE-Review: Doesn't this need to handle spill temps?

                if (fieldNode->OperIs(GT_LCL_VAR))
                {
                    emit->emitIns_R_S(INS_mov, emitTypeSize(fieldNode->GetType()), intTmpReg,
                                      fieldNode->AsLclVar()->GetLclNum(), 0);
                }
                else
                {
                    GenIntCon(fieldNode->AsIntCon(), intTmpReg, fieldNode->GetType());
                }

                if (pushStkArg)
                {
                    genPushReg(fieldType, intTmpReg);
                }
                else
                {
                    emit->emitIns_AR_R(ins_Store(fieldType), emitTypeSize(fieldType), intTmpReg, REG_SPBASE,
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
                emit->emitIns_AR_R(ins_Store(fieldType), emitTypeSize(fieldType), argReg, REG_SPBASE,
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

#if FEATURE_FASTTAILCALL
unsigned CodeGen::GetFirstStackParamLclNum()
{
#ifdef WINDOWS_AMD64_ABI
    // On win-x64 all params have home locations on caller's frame, even if they're passed in registers.
    INDEBUG(LclVarDsc* lcl = compiler->lvaGetDesc(0u));
    assert(lcl->IsRegParam() && (lcl->GetParamReg() == REG_ECX) || (lcl->GetParamReg() == REG_XMM0));

    return 0;
#else
    for (unsigned lclNum = 0; lclNum < compiler->info.compArgsCount; lclNum++)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(lclNum);

        assert(lcl->IsParam());

        if (!lcl->IsRegParam())
        {
            return lclNum;
        }
    }

    return BAD_VAR_NUM;
#endif
}
#endif // FEATURE_FASTTAILCALL

void CodeGen::genPutArgStk(GenTreePutArgStk* putArgStk)
{
    GenTree*  src     = putArgStk->GetOp(0);
    var_types srcType = varActualType(src->GetType());

#ifdef TARGET_AMD64
    unsigned outArgLclNum;
    INDEBUG(unsigned outArgLclSize);

    if (putArgStk->PutInIncomingArgArea())
    {
        assert(putArgStk->GetCall()->IsFastTailCall());

        outArgLclNum = GetFirstStackParamLclNum();
        INDEBUG(outArgLclSize = paramsStackSize);

        noway_assert(outArgLclNum != BAD_VAR_NUM);
    }
    else
    {
        outArgLclNum = compiler->lvaOutgoingArgSpaceVar;
        INDEBUG(outArgLclSize = outgoingArgSpaceSize);
    }

    unsigned outArgLclOffs = putArgStk->GetSlotOffset();
#else
    // On a 32-bit target, all of the long arguments are handled with FIELD_LISTs of TYP_INT.
    assert(srcType != TYP_LONG);

    genAlignStackBeforeCall(putArgStk);
#endif

    if (src->OperIs(GT_FIELD_LIST))
    {
#ifdef TARGET_AMD64
        genPutArgStkFieldList(putArgStk, outArgLclNum, outArgLclOffs DEBUGARG(outArgLclSize));
#else
        genPutArgStkFieldList(putArgStk);
#endif
        return;
    }

#ifdef TARGET_X86
    if (src->IsMultiRegCall() && varTypeIsStruct(src->GetType()))
    {
        assert(src->AsCall()->GetRegCount() == 2);
        assert(putArgStk->GetSlotCount() == 2);

        // TODO-MIKE-Cleanup: Using the register types isn't quite right, we need
        // the slot types from the argument layout. But in general they should be
        // the same, unless there's some weird reinterpretation going on, likely
        // due to invalid IL. Anyway, this is currently used only by unmanaged
        // calls so GC pointers should not be involved. It obviously would not
        // work for __vectorcall or if the managed calling convention is changed
        // to be like the native one.
        assert(src->AsCall()->GetRegType(0) == TYP_INT);
        assert(src->AsCall()->GetRegType(1) == TYP_INT);

        regNumber srcReg0 = UseReg(src, 0);
        regNumber srcReg1 = UseReg(src, 1);

        genPushReg(TYP_INT, srcReg1);
        genPushReg(TYP_INT, srcReg0);

        return;
    }
#endif

    if (srcType == TYP_STRUCT)
    {
#ifdef TARGET_AMD64
        genPutStructArgStk(putArgStk, outArgLclNum, outArgLclOffs DEBUGARG(outArgLclSize));
#else
        genPutStructArgStk(putArgStk);
#endif
        return;
    }

    emitter& emit = *GetEmitter();

#ifdef WINDOWS_AMD64_ABI
    assert(putArgStk->GetSlotCount() == 1);
#else
    if (src->IsIntegralConst(0) && (putArgStk->GetSlotCount() > 1))
    {
        if (putArgStk->GetKind() == GenTreePutArgStk::Kind::RepInstrZero)
        {
            regNumber srcReg = genConsumeReg(src);
            genCopyRegIfNeeded(src, REG_RAX);

            assert((putArgStk->gtRsvdRegs & (RBM_RCX | RBM_RDI)) == (RBM_RCX | RBM_RDI));

#ifdef TARGET_X86
            genPreAdjustStackForPutArgStk(putArgStk->GetArgSize());
            emit.emitIns_Mov(INS_mov, EA_4BYTE, REG_RDI, REG_SPBASE, /* canSkip */ false);
#else
            emit.emitIns_R_S(INS_lea, EA_PTRSIZE, REG_RDI, outArgLclNum, static_cast<int>(outArgLclOffs));
#endif
            emit.emitIns_R_I(INS_mov, EA_4BYTE, REG_RCX, putArgStk->GetSlotCount());
            emit.emitIns(INS_r_stosp);
        }
#ifdef TARGET_X86
        else if (putArgStk->GetArgSize() < XMM_REGSIZE_BYTES)
        {
            assert(src->isContained());
            assert(putArgStk->gtRsvdRegs == 0);

            for (unsigned i = 0; i < putArgStk->GetSlotCount(); i++)
            {
                emit.emitIns_I(INS_push, EA_4BYTE, 0);
                AddStackLevel(4);
            }
        }
#endif // TARGET_X86
        else
        {
            assert(putArgStk->GetKind() == GenTreePutArgStk::Kind::UnrollZero);
            assert(src->isContained());

            unsigned size = putArgStk->GetSlotCount() * REGSIZE_BYTES;

#ifdef TARGET_X86
            genPreAdjustStackForPutArgStk(size);
#endif
            regNumber zeroXmmReg = putArgStk->GetSingleTempReg(RBM_ALLFLOAT);
            emit.emitIns_R_R(INS_xorps, EA_16BYTE, zeroXmmReg, zeroXmmReg);

            unsigned offset = 0;

            while (offset + XMM_REGSIZE_BYTES <= size)
            {
#ifdef TARGET_X86
                emit.emitIns_AR_R(INS_movups, EA_16BYTE, zeroXmmReg, REG_SPBASE, offset);
#else
                emit.emitIns_S_R(INS_movups, EA_16BYTE, zeroXmmReg, outArgLclNum,
                                 static_cast<int>(outArgLclOffs + offset));
#endif
                offset += XMM_REGSIZE_BYTES;
            }

#ifdef TARGET_X86
            assert(((size - offset) & ~12) == 0);
#else
            assert(((size - offset) & ~8) == 0);
#endif

            if (size - offset >= 8)
            {
#ifdef TARGET_X86
                emit.emitIns_AR_R(INS_movq, EA_8BYTE, zeroXmmReg, REG_SPBASE, offset);
#else
                emit.emitIns_S_R(INS_movq, EA_8BYTE, zeroXmmReg, outArgLclNum,
                                 static_cast<int>(outArgLclOffs + offset));
#endif
                offset += 8;
            }

#ifdef TARGET_X86
            if (size - offset != 0)
            {
                assert(size - offset == 4);
                emit.emitIns_AR_R(INS_movd, EA_4BYTE, zeroXmmReg, REG_SPBASE, offset);
            }
#endif
        }

        return;
    }
#endif // !WINDOWS_AMD64_ABI

#if defined(TARGET_AMD64) || !defined(FEATURE_SIMD)
    assert(roundUp(varTypeSize(srcType), REGSIZE_BYTES) <= putArgStk->GetSlotCount() * REGSIZE_BYTES);
#else
    assert((roundUp(varTypeSize(srcType), REGSIZE_BYTES) <= putArgStk->GetArgSize()) || putArgStk->IsSIMD12());
#endif

    if (src->isUsedFromReg())
    {
        regNumber srcReg = UseReg(src);

#ifdef TARGET_AMD64
        emit.emitIns_S_R(ins_Store(srcType), emitTypeSize(srcType), srcReg, outArgLclNum,
                         static_cast<int>(outArgLclOffs));
#else
#ifdef FEATURE_SIMD
        if (varTypeIsSIMD(srcType))
        {
            assert(genIsValidFloatReg(srcReg));

            emit.emitIns_R_I(INS_sub, EA_4BYTE, REG_SPBASE, putArgStk->GetArgSize());
            AddStackLevel(putArgStk->GetArgSize());

            if (putArgStk->IsSIMD12())
            {
                genStoreSIMD12ToStack(srcReg, putArgStk->GetSingleTempReg());
            }
            else
            {
                emit.emitIns_AR_R(ins_Store(srcType), emitTypeSize(srcType), srcReg, REG_SPBASE, 0);
            }
        }
        else
#endif
        {
            genPushReg(srcType, srcReg);
        }
#endif
    }
    else
    {
#ifdef TARGET_AMD64
        emit.emitIns_S_I(ins_Store(srcType), emitTypeSize(srcType), outArgLclNum, static_cast<int>(outArgLclOffs),
                         src->AsIntCon()->GetInt32Value());
#else
        genConsumeRegs(src);

        assert(putArgStk->GetSlotCount() == 1);

        emitAttr attr = emitActualTypeSize(src->GetType());

        assert(EA_SIZE_IN_BYTES(attr) == REGSIZE_BYTES);

        unsigned lclNum;
        unsigned lclOffs;

        if (IsLocalMemoryOperand(src, &lclNum, &lclOffs))
        {
            emit.emitIns_S(INS_push, attr, lclNum, lclOffs);
        }
        else if (src->OperIs(GT_IND))
        {
            emit.emitIns_A(INS_push, attr, src->AsIndir()->GetAddr());
        }
        else
        {
            emitAttr attr = src->IsIconHandle() ? EA_HANDLE_CNS_RELOC : EA_PTRSIZE;
            emit.emitIns_I(INS_push, attr, src->AsIntCon()->GetValue());
        }

        AddStackLevel(REGSIZE_BYTES);
#endif
    }
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

void CodeGen::genPutStructArgStk(GenTreePutArgStk* putArgStk
#ifndef TARGET_X86
                                 ,
                                 unsigned outArgLclNum,
                                 unsigned outArgLclOffs DEBUGARG(unsigned outArgLclSize)
#endif
                                     )
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

            srcOffset = addrMode->GetOffset();
        }

        srcLayout   = src->AsObj()->GetLayout();
        srcAddrAttr = emitTypeSize(srcAddr->GetType());
    }

    if (putArgStk->GetKind() == GenTreePutArgStk::Kind::Unroll)
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
    if (putArgStk->GetKind() == GenTreePutArgStk::Kind::Push)
    {
        // On x86, any struct that has contains GC references must be stored to the stack using `push` instructions
        // so that the emitter properly detects the need to update the method's GC information. We also use `push`
        // for structs that are 8 bytes (or less, if the arg is a local var).
        //
        // Strictly speaking, it is only necessary to use `push` to store the GC references themselves, so for structs
        // with large numbers of consecutive non-GC-ref-typed fields, we may be able to improve the code size in the
        // future.

        assert((srcLclNum != BAD_VAR_NUM) || (srcLayout->GetSize() % REGSIZE_BYTES == 0));

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

    assert(putArgStk->GetKind() == GenTreePutArgStk::Kind::RepInstr);
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

    if ((putArgStk->GetKind() == GenTreePutArgStk::Kind::RepInstr) ||
        (putArgStk->GetKind() == GenTreePutArgStk::Kind::RepInstrXMM))
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
            assert(putArgStk->GetKind() == GenTreePutArgStk::Kind::RepInstr);
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
        assert((putArgStk->GetKind() == GenTreePutArgStk::Kind::GCUnroll) ||
               (putArgStk->GetKind() == GenTreePutArgStk::Kind::GCUnrollXMM));

        intTmpReg = putArgStk->GetSingleTempReg(RBM_ALLINT);
    }

    if ((putArgStk->GetKind() == GenTreePutArgStk::Kind::RepInstrXMM) ||
        (putArgStk->GetKind() == GenTreePutArgStk::Kind::GCUnrollXMM))
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

        assert((putArgStk->GetKind() == GenTreePutArgStk::Kind::RepInstr) ||
               (putArgStk->GetKind() == GenTreePutArgStk::Kind::RepInstrXMM));
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

#ifdef TARGET_X86
void CodeGen::genEmitHelperCall(CorInfoHelpFunc helper, int argSize, emitAttr retSize, regNumber callTargetReg)
#else
void CodeGen::genEmitHelperCall(CorInfoHelpFunc helper, emitAttr retSize, regNumber callTargetReg)
#endif
{
    void* addr  = nullptr;
    void* pAddr = nullptr;

    emitter::EmitCallType callType = emitter::EC_FUNC_TOKEN;
    addr                           = compiler->compGetHelperFtn(helper, &pAddr);
    regNumber callTarget           = REG_NA;

    if (addr == nullptr)
    {
        assert(pAddr != nullptr);

#ifdef TARGET_X86
        callType = emitter::EC_FUNC_TOKEN_INDIR;
        addr     = pAddr;
#else  // TARGET_AMD64
        if (compiler->eeIsRIPRelativeAddress(pAddr) || FitsIn<int32_t>(reinterpret_cast<intptr_t>(pAddr)))
        {
            // generate call whose target is specified by 32-bit offset relative to PC or zero.
            callType = emitter::EC_FUNC_TOKEN_INDIR;
            addr     = pAddr;
        }
        else
        {
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
                noway_assert((callTargetMask & compiler->compHelperCallKillSet(helper)) == callTargetMask);
            }
            else
            {
                // The call target must not overwrite any live variable, though it may not be in the
                // kill set for the call.
                regMaskTP callTargetMask = genRegMask(callTargetReg);
                noway_assert((callTargetMask & liveness.GetLiveLclRegs()) == RBM_NONE);
            }

            GetEmitter()->emitIns_R_I(INS_mov, EA_PTRSIZE, callTargetReg, reinterpret_cast<ssize_t>(pAddr));

            callType   = emitter::EC_INDIR_ARD;
            callTarget = callTargetReg;
        }
#endif // TARGET_AMD64
    }

    // clang-format off
    GetEmitter()->emitIns_Call(
        callType,
        Compiler::eeFindHelper(helper)
        DEBUGARG(nullptr),
        addr,
#ifdef TARGET_X86
        argSize,
#endif
        retSize MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(EA_UNKNOWN),
        callTarget, REG_NA, 0, 0,
        false);
    // clang-format on
}

// Uncomment "#define ALL_ARM64_EMITTER_UNIT_TESTS" to run all the unit tests here.
// After adding a unit test, and verifying it works, put it under this #ifdef, so
// we don't see it run every time.
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

// Generate the profiling function enter callback.
//
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
void CodeGen::PrologProfilingEnterCallback(regNumber initReg, bool* pInitRegZeroed)
{
    assert(generatingProlog);

    // Give profiler a chance to back out of hooking this method
    if (!compiler->compIsProfilerHookNeeded())
    {
        return;
    }

    unsigned saveStackLvl2 = genStackLevel;

// Important note: when you change enter probe layout, you must also update SKIP_ENTER_PROF_CALLBACK()
// for x86 stack unwinding

#ifdef UNIX_X86_ABI
    // Manually align the stack to be 16-byte aligned. This is similar to CodeGen::genAlignStackBeforeCall()
    GetEmitter()->emitIns_R_I(INS_sub, EA_4BYTE, REG_SPBASE, 0xC);
#endif

    int32_t profilerMethodAddr = static_cast<int32_t>(reinterpret_cast<intptr_t>(compiler->compProfilerMethHnd));

    if (compiler->compProfilerMethHndIndirected)
    {
        GetEmitter()->emitIns_AR_R(INS_push, EA_PTR_DSP_RELOC, REG_NA, REG_NA, profilerMethodAddr);
    }
    else
    {
        GetEmitter()->emitIns_I(INS_push, EA_4BYTE, profilerMethodAddr);
    }

    // This will emit either
    // "call ip-relative 32-bit offset" or
    // "mov rax, helper addr; call rax"
    genEmitHelperCall(CORINFO_HELP_PROF_FCN_ENTER);

#ifdef UNIX_X86_ABI
    // Restoring alignment manually. This is similar to CodeGen::genRemoveAlignmentAfterCall
    GetEmitter()->emitIns_R_I(INS_add, EA_4BYTE, REG_SPBASE, 0x10);
#endif

    SetStackLevel(saveStackLvl2);
}

// Generate the profiling function leave or tailcall callback.
// Technically, this is not part of the epilog; it is called when we are generating code for a GT_RETURN node.
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
void CodeGen::genProfilingLeaveCallback(CorInfoHelpFunc helper)
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

    int32_t profilerMethodAddr = static_cast<int32_t>(reinterpret_cast<intptr_t>(compiler->compProfilerMethHnd));

    if (compiler->compProfilerMethHndIndirected)
    {
        GetEmitter()->emitIns_AR_R(INS_push, EA_PTR_DSP_RELOC, REG_NA, REG_NA, profilerMethodAddr);
    }
    else
    {
        GetEmitter()->emitIns_I(INS_push, EA_4BYTE, profilerMethodAddr);
    }

    AddStackLevel(REGSIZE_BYTES);

#ifdef UNIX_X86_ABI
    int argSize = -REGSIZE_BYTES; // negative means caller-pop (cdecl)
#else
    int argSize = REGSIZE_BYTES;
#endif
    genEmitHelperCall(helper, argSize, EA_UNKNOWN /* retSize */);

#ifdef UNIX_X86_ABI
    // Restoring alignment manually. This is similar to CodeGen::genRemoveAlignmentAfterCall
    GetEmitter()->emitIns_R_I(INS_add, EA_4BYTE, REG_SPBASE, 0x10);
    SubtractStackLevel(0x10);
    SubtractNestedAlignment(0xC);
#endif

    SetStackLevel(saveStackLvl2);
}

#endif // TARGET_X86

#ifdef TARGET_AMD64

void CodeGen::PrologProfilingEnterCallback(regNumber initReg, bool* pInitRegZeroed)
{
    assert(generatingProlog);

    // Give profiler a chance to back out of hooking this method
    if (!compiler->compIsProfilerHookNeeded())
    {
        return;
    }

#ifdef WINDOWS_AMD64_ABI
    unsigned   varNum;
    LclVarDsc* varDsc;

    // Since the method needs to make a profiler callback, it should have out-going arg space allocated.
    noway_assert(compiler->lvaOutgoingArgSpaceVar != BAD_VAR_NUM);
    noway_assert(outgoingArgSpaceSize >= 4 * REGSIZE_BYTES);

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
            noway_assert(varDsc->IsParam());

            if (!varDsc->IsRegParam())
            {
                continue;
            }

            var_types type = varActualType(varDsc->GetType());
            regNumber reg  = varDsc->GetParamReg();

            if (varTypeIsStruct(type))
            {
                assert(isValidIntArgReg(reg));

                type = varDsc->GetLayout()->GetSize() <= 4 ? TYP_INT : varDsc->GetLayout()->GetGCPtrType(0);
            }

            GetEmitter()->emitIns_S_R(ins_Store(type), emitTypeSize(type), reg, varNum, 0);
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
    else if (compiler->opts.compJitELTHookEnabled)
    {
        // COMPlus_JitELTHookEnabled does not require relocations.
        GetEmitter()->emitIns_R_I(INS_mov, EA_8BYTE, REG_ARG_0,
                                  reinterpret_cast<ssize_t>(compiler->compProfilerMethHnd));
    }
    else
    {
        instGen_Set_Reg_To_Imm(EA_8BYTE, REG_ARG_0, (ssize_t)compiler->compProfilerMethHnd);
    }

    // RDX = caller's SP
    // Notes
    //   1) Here we can query caller's SP offset since prolog will be generated after final frame layout.
    //   2) caller's SP relative offset to FramePointer will be negative.  We need to add absolute shift
    //      of that offset to FramePointer to obtain caller's SP shift.
    assert(compiler->lvaOutgoingArgSpaceVar != BAD_VAR_NUM);
    int callerSPOffset = compiler->lvaToCallerSPRelativeOffset(0, isFramePointerUsed());
    GetEmitter()->emitIns_R_AR(INS_lea, EA_PTRSIZE, REG_ARG_1, genFramePointerReg(), -callerSPOffset);

    // This will emit either
    // "call ip-relative 32-bit offset" or
    // "mov rax, helper addr; call rax"
    genEmitHelperCall(CORINFO_HELP_PROF_FCN_ENTER);

    // TODO-AMD64-CQ: Rather than reloading, see if this could be optimized by combining with prolog
    // generation logic that moves args around as required by first BB entry point conditions
    // computed by LSRA.  Code pointers for investigating this further: genPrologMoveParamRegs()
    // and genPrologEnregisterIncomingStackParams().
    //
    // Now reload arg registers from home locations.
    // Vararg methods:
    //   - we need to reload only known (i.e. fixed) reg args.
    //   - if floating point type, also reload it into corresponding integer reg
    for (varNum = 0, varDsc = compiler->lvaTable; varNum < compiler->info.compArgsCount; varNum++, varDsc++)
    {
        noway_assert(varDsc->IsParam());

        if (!varDsc->IsRegParam())
        {
            continue;
        }

        var_types type = varActualType(varDsc->GetType());
        regNumber reg  = varDsc->GetParamReg();

        if (varTypeIsStruct(type))
        {
            assert(isValidIntArgReg(reg));

            type = varDsc->GetLayout()->GetSize() <= 4 ? TYP_INT : varDsc->GetLayout()->GetGCPtrType(0);
        }

        GetEmitter()->emitIns_R_S(ins_Load(type), emitTypeSize(type), reg, varNum, 0);

        if (compiler->info.compIsVarArgs && varTypeIsFloating(type))
        {
            GetEmitter()->emitIns_Mov(INS_movd, emitTypeSize(type), MapVarargsParamFloatRegToIntReg(reg), reg,
                                      /*canSkip*/ false);
        }
    }

    // If initReg is one of RBM_CALLEE_TRASH, then it needs to be zeroed before using.
    if ((RBM_CALLEE_TRASH & genRegMask(initReg)) != 0)
    {
        *pInitRegZeroed = false;
    }

#else // UNIX_AMD64_ABI

    // Emit profiler EnterCallback(ProfilerMethHnd, caller's SP)
    // R14 = ProfilerMethHnd
    if (compiler->compProfilerMethHndIndirected)
    {
        // Profiler hooks enabled during Ngen time.
        // Profiler handle needs to be accessed through an indirection of a pointer.
        GetEmitter()->emitIns_R_AI(INS_mov, EA_PTR_DSP_RELOC, REG_PROFILER_ENTER_ARG_0,
                                   (ssize_t)compiler->compProfilerMethHnd);
    }
    else if (compiler->opts.compJitELTHookEnabled)
    {
        // COMPlus_JitELTHookEnabled does not require relocations.
        GetEmitter()->emitIns_R_I(INS_mov, EA_8BYTE, REG_PROFILER_ENTER_ARG_0,
                                  reinterpret_cast<ssize_t>(compiler->compProfilerMethHnd));
    }
    else
    {
        instGen_Set_Reg_To_Imm(EA_8BYTE, REG_PROFILER_ENTER_ARG_0, (ssize_t)compiler->compProfilerMethHnd);
    }

    // R15 = caller's SP
    // Notes
    //   1) Here we can query caller's SP offset since prolog will be generated after final frame layout.
    //   2) caller's SP relative offset to FramePointer will be negative.  We need to add absolute shift
    //      of that offset to FramePointer to obtain caller's SP shift.
    assert(compiler->lvaOutgoingArgSpaceVar != BAD_VAR_NUM);
    int callerSPOffset = compiler->lvaToCallerSPRelativeOffset(0, isFramePointerUsed());
    GetEmitter()->emitIns_R_AR(INS_lea, EA_PTRSIZE, REG_PROFILER_ENTER_ARG_1, genFramePointerReg(), -callerSPOffset);

    // We can use any callee trash register (other than RAX, RDI, RSI) for call target.
    // We use R11 here. This will emit either
    // "call ip-relative 32-bit offset" or
    // "mov r11, helper addr; call r11"
    genEmitHelperCall(CORINFO_HELP_PROF_FCN_ENTER, EA_UNKNOWN, REG_DEFAULT_PROFILER_CALL_TARGET);

    // If initReg is one of RBM_CALLEE_TRASH, then it needs to be zero'ed before using.
    if ((RBM_CALLEE_TRASH & genRegMask(initReg)) != 0)
    {
        *pInitRegZeroed = false;
    }

#endif // UNIX_AMD64_ABI
}

void CodeGen::genProfilingLeaveCallback(CorInfoHelpFunc helper)
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
    noway_assert(outgoingArgSpaceSize >= 4 * REGSIZE_BYTES);

    // If thisPtr needs to be kept alive and reported, it cannot be one of the callee trash
    // registers that profiler callback kills.
    if (compiler->lvaKeepAliveAndReportThis() && compiler->lvaTable[compiler->info.compThisArg].lvIsInReg())
    {
        regMaskTP thisPtrMask = genRegMask(compiler->lvaTable[compiler->info.compThisArg].GetRegNum());
        noway_assert((RBM_PROFILER_LEAVE_TRASH & thisPtrMask) == 0);
    }

    // At this point return shift is computed and stored in RAX or XMM0.
    // On Amd64, Leave callback preserves the return register.  We keep
    // RAX alive by not reporting as trashed by helper call.  Also note
    // that GC cannot kick-in while executing inside profiler callback,
    // which is a requirement of profiler as well since it needs to examine
    // return shift which could be an obj ref.

    // RCX = ProfilerMethHnd
    if (compiler->compProfilerMethHndIndirected)
    {
        // Profiler hooks enabled during Ngen time.
        // Profiler handle needs to be accessed through an indirection of an address.
        GetEmitter()->emitIns_R_AI(INS_mov, EA_PTR_DSP_RELOC, REG_ARG_0, (ssize_t)compiler->compProfilerMethHnd);
    }
    else if (compiler->opts.compJitELTHookEnabled)
    {
        // COMPlus_JitELTHookEnabled does not require relocations.
        GetEmitter()->emitIns_R_I(INS_mov, EA_8BYTE, REG_ARG_0,
                                  reinterpret_cast<ssize_t>(compiler->compProfilerMethHnd));
    }
    else
    {
        instGen_Set_Reg_To_Imm(EA_8BYTE, REG_ARG_0, (ssize_t)compiler->compProfilerMethHnd);
    }

    // RDX = caller's SP
    // TODO-AMD64-Cleanup: Once we start doing codegen after final frame layout, retain the "if" portion
    // of the stmnts to execute unconditionally and clean-up rest.
    if (compiler->lvaDoneFrameLayout == Compiler::FINAL_FRAME_LAYOUT)
    {
        // Caller's SP relative offset to FramePointer will be negative.  We need to add absolute
        // shift of that offset to FramePointer to obtain caller's SP shift.
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
        NYI_IF((varDsc == nullptr) || !varDsc->IsParam(), "Profiler ELT callback for a method without any params");

        // lea rdx, [FramePointer + Arg0's offset]
        GetEmitter()->emitIns_R_S(INS_lea, EA_PTRSIZE, REG_ARG_1, 0, 0);
    }

    // We can use any callee trash register (other than RAX, RCX, RDX) for call target.
    // We use R8 here. This will emit either
    // "call ip-relative 32-bit offset" or
    // "mov r8, helper addr; call r8"
    genEmitHelperCall(helper, EA_UNKNOWN, REG_ARG_2);

#else // !defined(UNIX_AMD64_ABI)

    // RDI = ProfilerMethHnd
    if (compiler->compProfilerMethHndIndirected)
    {
        GetEmitter()->emitIns_R_AI(INS_mov, EA_PTR_DSP_RELOC, REG_ARG_0, (ssize_t)compiler->compProfilerMethHnd);
    }
    else if (compiler->opts.compJitELTHookEnabled)
    {
        // COMPlus_JitELTHookEnabled does not require relocations.
        GetEmitter()->emitIns_R_I(INS_mov, EA_8BYTE, REG_ARG_0,
                                  reinterpret_cast<ssize_t>(compiler->compProfilerMethHnd));
    }
    else
    {
        instGen_Set_Reg_To_Imm(EA_8BYTE, REG_ARG_0, (ssize_t)compiler->compProfilerMethHnd);
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
        NYI_IF((varDsc == nullptr) || !varDsc->IsParam(), "Profiler ELT callback for a method without any params");

        // lea rdx, [FramePointer + Arg0's offset]
        GetEmitter()->emitIns_R_S(INS_lea, EA_PTRSIZE, REG_ARG_1, 0, 0);
    }

    // We can use any callee trash register (other than RAX, RDI, RSI) for call target.
    // We use R11 here. This will emit either
    // "call ip-relative 32-bit offset" or
    // "mov r11, helper addr; call r11"
    genEmitHelperCall(helper, EA_UNKNOWN, REG_DEFAULT_PROFILER_CALL_TARGET);

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

void CodeGen::LoadSIMD12(GenTree* load)
{
    GenAddrMode src(load, this);

    regNumber tmpReg = load->GetSingleTempReg();
    regNumber dstReg = load->GetRegNum();

    assert(tmpReg != dstReg);

    inst_R_AM(INS_movsdsse2, EA_8BYTE, dstReg, src, 0);
    inst_R_AM(INS_movss, EA_4BYTE, tmpReg, src, 8);
    GetEmitter()->emitIns_R_R(INS_movlhps, EA_16BYTE, dstReg, tmpReg);
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
// When a 32-byte SIMD shift is live across a call, the register allocator will use this intrinsic
// to cause the upper half to be saved. It will first attempt to find another, unused, callee-save
// register. If such a register cannot be found, it will save the upper half to the upper half
// of the localVar's home location.
// (Note that if there are no caller-save registers available, the entire 32 byte
// shift will be spilled to the stack.)
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

void CodeGen::PrologPushCalleeSavedRegisters()
{
    assert(generatingProlog);

    // x86/x64 doesn't support push of xmm/ymm regs, therefore consider only integer registers for pushing onto stack
    // here. Space for float registers to be preserved is stack allocated and saved as part of prolog sequence and not
    // here.
    regMaskTP rsPushRegs = calleeSavedModifiedRegs & RBM_ALLINT;

    // On X86/X64 we have already pushed the FP (frame-pointer) prior to calling this method
    if (isFramePointerUsed())
    {
        rsPushRegs &= ~RBM_FPBASE;
    }

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

    // Push backwards so we match the order we will pop them in the epilog
    // and all the other code that expects it to be in this order.
    //
    // TODO-MIKE-Review: On x86 this doesn't match REG_CALLEE_SAVED_ORDER,
    // which is supposedly what the VM expects. But the difference is due
    // to EBP being after EBX instead of before, so it looks like this is
    // not an issue because ETW_EBP_FRAMED is defined so we never use EBP,
    // even in ESP based frames.

    for (regNumber reg = REG_INT_LAST; rsPushRegs != RBM_NONE; reg = REG_PREV(reg))
    {
        regMaskTP regBit = genRegMask(reg);

        if ((regBit & rsPushRegs) != 0)
        {
            inst_RV(INS_push, reg, TYP_REF);
            compiler->unwindPush(reg);
#ifdef USING_SCOPE_INFO
            if (!IsFramePointerRequired())
            {
                psiAdjustStackLevel(REGSIZE_BYTES);
            }
#endif // USING_SCOPE_INFO
            rsPushRegs &= ~regBit;
        }
    }
}

void CodeGen::genPopCalleeSavedRegisters(bool jmpEpilog)
{
    assert(generatingEpilog);

    regMaskTP popRegs  = calleeSavedModifiedRegs & RBM_ALLINT;
    unsigned  popCount = 0;

    for (regNumber reg = REG_INT_FIRST; popRegs != RBM_NONE; reg = REG_NEXT(reg))
    {
        regMaskTP regMask = genRegMask(reg);

        if ((popRegs & regMask) != 0)
        {
            popRegs &= ~regMask;
            popCount++;
            GetEmitter()->emitIns_R(INS_pop, EA_PTRSIZE, reg);
        }
    }

    noway_assert(calleeRegsPushed == popCount);
}

// Save compCalleeFPRegsPushed with the smallest register number saved at [RSP+offset], working
// down the stack to the largest register number stored at [RSP+offset-(genCountBits(regMask)-1)*XMM_REG_SIZE]
// Here offset = 16-byte aligned offset after pushing integer registers.
//
// lclFrameSize - Fixed frame size excluding callee pushed int regs.
//                non-funclet: this will be compLclFrameSize.
//                funclet frames: this will be FuncletInfo.fiSpDelta.
//
void CodeGen::PrologPreserveCalleeSavedFloatRegs(unsigned lclFrameSize)
{
    genVzeroupperIfNeeded(false);

#ifndef WINDOWS_AMD64_ABI
    static_assert_no_msg(RBM_FLT_CALLEE_SAVED == RBM_NONE);
#else
    regMaskTP regMask = calleeSavedModifiedRegs & RBM_ALLFLOAT;

    if (regMask == RBM_NONE)
    {
        return;
    }

    unsigned firstFPRegPadding = compiler->lvaIsCalleeSavedIntRegCountEven() ? REGSIZE_BYTES : 0;
    unsigned offset            = lclFrameSize - firstFPRegPadding - XMM_REGSIZE_BYTES;

    // Offset is 16-byte aligned since we use movaps for preserving xmm regs.
    assert((offset % 16) == 0);

    for (regNumber reg = REG_FLT_CALLEE_SAVED_FIRST; regMask != RBM_NONE; reg = REG_NEXT(reg))
    {
        regMaskTP regBit = genRegMask(reg);

        if ((regBit & regMask) != 0)
        {
            GetEmitter()->emitIns_AR_R(INS_movaps, EA_16BYTE, reg, REG_RSP, offset);
            compiler->unwindSaveReg(reg, offset);
            regMask &= ~regBit;
            offset -= XMM_REGSIZE_BYTES;
        }
    }
#endif // WINDOWS_AMD64_ABI
}

// Save/Restore compCalleeFPRegsPushed with the smallest register number saved at [RSP+offset], working
// down the stack to the largest register number stored at [RSP+offset-(genCountBits(regMask)-1)*XMM_REG_SIZE]
// Here offset = 16-byte aligned offset after pushing integer registers.
//
// lclFrameSize - Fixed frame size excluding callee pushed int regs.
//                non-funclet: this will be compLclFrameSize.
//                funclet frames: this will be FuncletInfo.fiSpDelta.
//
void CodeGen::genRestoreCalleeSavedFltRegs(unsigned lclFrameSize)
{
#ifndef WINDOWS_AMD64_ABI
    static_assert_no_msg(RBM_FLT_CALLEE_SAVED == RBM_NONE);
#else
    regMaskTP regMask = calleeSavedModifiedRegs & RBM_ALLFLOAT;

    if (regMask == RBM_NONE)
    {
        genVzeroupperIfNeeded();
        return;
    }

    unsigned  firstFPRegPadding = compiler->lvaIsCalleeSavedIntRegCountEven() ? REGSIZE_BYTES : 0;
    unsigned  offset;
    regNumber regBase;

    if (compiler->compLocallocUsed)
    {
        // localloc frame: use frame pointer relative offset
        assert(isFramePointerUsed());
        regBase = REG_FPBASE;
        offset  = lclFrameSize - genSPtoFPdelta() - firstFPRegPadding - XMM_REGSIZE_BYTES;
    }
    else
    {
        regBase = REG_RSP;
        offset  = lclFrameSize - firstFPRegPadding - XMM_REGSIZE_BYTES;
    }

    // Offset is 16-byte aligned since we use movaps for restoring xmm regs
    assert((offset % 16) == 0);

    for (regNumber reg = REG_FLT_CALLEE_SAVED_FIRST; regMask != RBM_NONE; reg = REG_NEXT(reg))
    {
        regMaskTP regBit = genRegMask(reg);

        if ((regBit & regMask) != 0)
        {
            GetEmitter()->emitIns_R_AR(INS_movaps, EA_16BYTE, reg, regBase, offset);
            regMask &= ~regBit;
            offset -= XMM_REGSIZE_BYTES;
        }
    }
#endif // WINDOWS_AMD64_ABI

    genVzeroupperIfNeeded();
}

// Generate Vzeroupper instruction as needed to zero out upper 128b-bit of all YMM registers so that the
// AVX/Legacy SSE transition penalties can be avoided. This function is been used in PrologPreserveCalleeSavedFloatRegs
// (prolog) and genRestoreCalleeSavedFltRegs (epilog). Issue VZEROUPPER in Prolog if the method contains
// 128-bit or 256-bit AVX code, to avoid legacy SSE to AVX transition penalty, which could happen when native
// code contains legacy SSE code calling into JIT AVX code (e.g. reverse pinvoke). Issue VZEROUPPER in Epilog
// if the method contains 256-bit AVX code, to avoid AVX to legacy SSE transition penalty.
//
// check256bitOnly - true to check if the function contains 256-bit AVX instruction and generate vzeroupper
//                   instruction, false to check if the function contains AVX instruciton (either 128-bit
//                   or 256-bit).
//
void CodeGen::genVzeroupperIfNeeded(bool check256bitOnly)
{
    bool emitVzeroUpper = false;
    if (check256bitOnly)
    {
        emitVzeroUpper = GetEmitter()->Contains256bitAVX();
    }
    else
    {
        emitVzeroUpper = GetEmitter()->ContainsAVX();
    }

    if (emitVzeroUpper)
    {
        assert(compiler->canUseVexEncoding());
        instGen(INS_vzeroupper);
    }
}

void CodeGen::PrologBlockInitLocals(int untrLclLo, int untrLclHi, regNumber initReg, bool* pInitRegZeroed)
{
    assert(generatingProlog && genUseBlockInit);
    assert(untrLclHi > untrLclLo);

    emitter*  emit        = GetEmitter();
    regNumber frameReg    = genFramePointerReg();
    regNumber zeroReg     = REG_NA;
    int       blkSize     = untrLclHi - untrLclLo;
    int       minSimdSize = XMM_REGSIZE_BYTES;

    assert(blkSize >= 0);
    noway_assert((blkSize % sizeof(int)) == 0);
    // initReg is not a live incoming param reg
    assert((genRegMask(initReg) & paramRegState.intRegLiveIn) == RBM_NONE);
#if defined(TARGET_AMD64)
    // We will align on x64 so can use the aligned mov
    instruction simdMov = simdAlignedMovIns();
    // Aligning low we want to move up to next boundary
    int alignedLclLo = (untrLclLo + (XMM_REGSIZE_BYTES - 1)) & -XMM_REGSIZE_BYTES;

    if ((untrLclLo != alignedLclLo) && (blkSize < 2 * XMM_REGSIZE_BYTES))
    {
        // If unaligned and smaller then 2 x SIMD size we won't bother trying to align
        assert((alignedLclLo - untrLclLo) < XMM_REGSIZE_BYTES);
        simdMov = simdUnalignedMovIns();
    }
#else // !defined(TARGET_AMD64)
    // We aren't going to try and align on x86
    instruction simdMov      = simdUnalignedMovIns();
    int         alignedLclLo = untrLclLo;
#endif

    auto GetZeroReg = [this, initReg, pInitRegZeroed]() {
        if (!*pInitRegZeroed)
        {
            instGen_Set_Reg_To_Zero(EA_PTRSIZE, initReg);
            *pInitRegZeroed = true;
        }

        return initReg;
    };

    if (blkSize < minSimdSize)
    {
        zeroReg = GetZeroReg();

        int i = 0;
        for (; i + REGSIZE_BYTES <= blkSize; i += REGSIZE_BYTES)
        {
            emit->emitIns_AR_R(ins_Store(TYP_I_IMPL), EA_PTRSIZE, zeroReg, frameReg, untrLclLo + i);
        }
#if defined(TARGET_AMD64)
        assert((i == blkSize) || (i + (int)sizeof(int) == blkSize));
        if (i != blkSize)
        {
            emit->emitIns_AR_R(ins_Store(TYP_INT), EA_4BYTE, zeroReg, frameReg, untrLclLo + i);
            i += sizeof(int);
        }
#endif // defined(TARGET_AMD64)
        assert(i == blkSize);
    }
    else
    {
        // Grab a non-argument, non-callee saved XMM reg
        CLANG_FORMAT_COMMENT_ANCHOR;
#ifdef UNIX_AMD64_ABI
        // System V x64 first temp reg is xmm8
        regNumber zeroSIMDReg = genRegNumFromMask(RBM_XMM8);
#else
        // Windows first temp reg is xmm4
        regNumber zeroSIMDReg = genRegNumFromMask(RBM_XMM4);
#endif // UNIX_AMD64_ABI

#if defined(TARGET_AMD64)
        int alignedLclHi;
        int alignmentHiBlkSize;

        if ((blkSize < 2 * XMM_REGSIZE_BYTES) || (untrLclLo == alignedLclLo))
        {
            // Either aligned or smaller then 2 x SIMD size so we won't try to align
            // However, we still want to zero anything that is not in a 16 byte chunk at end
            int alignmentBlkSize = blkSize & -XMM_REGSIZE_BYTES;
            alignmentHiBlkSize   = blkSize - alignmentBlkSize;
            alignedLclHi         = untrLclLo + alignmentBlkSize;
            alignedLclLo         = untrLclLo;
            blkSize              = alignmentBlkSize;

            assert((blkSize + alignmentHiBlkSize) == (untrLclHi - untrLclLo));
        }
        else
        {
            // We are going to align

            // Aligning high we want to move down to previous boundary
            alignedLclHi = untrLclHi & -XMM_REGSIZE_BYTES;
            // Zero out the unaligned portions
            alignmentHiBlkSize     = untrLclHi - alignedLclHi;
            int alignmentLoBlkSize = alignedLclLo - untrLclLo;
            blkSize                = alignedLclHi - alignedLclLo;

            assert((blkSize + alignmentLoBlkSize + alignmentHiBlkSize) == (untrLclHi - untrLclLo));

            assert(alignmentLoBlkSize > 0);
            assert(alignmentLoBlkSize < XMM_REGSIZE_BYTES);
            assert((alignedLclLo - alignmentLoBlkSize) == untrLclLo);

            zeroReg = GetZeroReg();

            int i = 0;
            for (; i + REGSIZE_BYTES <= alignmentLoBlkSize; i += REGSIZE_BYTES)
            {
                emit->emitIns_AR_R(ins_Store(TYP_I_IMPL), EA_PTRSIZE, zeroReg, frameReg, untrLclLo + i);
            }
            assert((i == alignmentLoBlkSize) || (i + (int)sizeof(int) == alignmentLoBlkSize));
            if (i != alignmentLoBlkSize)
            {
                emit->emitIns_AR_R(ins_Store(TYP_INT), EA_4BYTE, zeroReg, frameReg, untrLclLo + i);
                i += sizeof(int);
            }

            assert(i == alignmentLoBlkSize);
        }
#else // !defined(TARGET_AMD64)
        // While we aren't aligning the start, we still want to
        // zero anything that is not in a 16 byte chunk at end
        int alignmentBlkSize   = blkSize & -XMM_REGSIZE_BYTES;
        int alignmentHiBlkSize = blkSize - alignmentBlkSize;
        int alignedLclHi       = untrLclLo + alignmentBlkSize;
        blkSize                = alignmentBlkSize;

        assert((blkSize + alignmentHiBlkSize) == (untrLclHi - untrLclLo));
#endif
        // The loop is unrolled 3 times so we do not move to the loop block until it
        // will loop at least once so the threshold is 6.
        if (blkSize < (6 * XMM_REGSIZE_BYTES))
        {
            // Generate the following code:
            //
            //   xorps   xmm4, xmm4
            //   movups  xmmword ptr [ebp/esp-OFFS], xmm4
            //   ...
            //   movups  xmmword ptr [ebp/esp-OFFS], xmm4
            //   mov      qword ptr [ebp/esp-OFFS], rax

            emit->emitIns_R_R(INS_xorps, EA_ATTR(XMM_REGSIZE_BYTES), zeroSIMDReg, zeroSIMDReg);

            int i = 0;
            for (; i < blkSize; i += XMM_REGSIZE_BYTES)
            {
                emit->emitIns_AR_R(simdMov, EA_ATTR(XMM_REGSIZE_BYTES), zeroSIMDReg, frameReg, alignedLclLo + i);
            }

            assert(i == blkSize);
        }
        else
        {
            // Generate the following code:
            //
            //    xorps    xmm4, xmm4
            //    ;movaps xmmword ptr[ebp/esp-loOFFS], xmm4          ; alignment to 3x
            //    ;movaps xmmword ptr[ebp/esp-loOFFS + 10H], xmm4    ;
            //    mov rax, - <size>                                  ; start offset from hi
            //    movaps xmmword ptr[rbp + rax + hiOFFS      ], xmm4 ; <--+
            //    movaps xmmword ptr[rbp + rax + hiOFFS + 10H], xmm4 ;    |
            //    movaps xmmword ptr[rbp + rax + hiOFFS + 20H], xmm4 ;    | Loop
            //    add rax, 48                                        ;    |
            //    jne SHORT  -5 instr                                ; ---+

            emit->emitIns_R_R(INS_xorps, EA_ATTR(XMM_REGSIZE_BYTES), zeroSIMDReg, zeroSIMDReg);

            // How many extra don't fit into the 3x unroll
            int extraSimd = (blkSize % (XMM_REGSIZE_BYTES * 3)) / XMM_REGSIZE_BYTES;
            if (extraSimd != 0)
            {
                blkSize -= XMM_REGSIZE_BYTES;
                // Not a multiple of 3 so add stores at low end of block
                emit->emitIns_AR_R(simdMov, EA_ATTR(XMM_REGSIZE_BYTES), zeroSIMDReg, frameReg, alignedLclLo);
                if (extraSimd == 2)
                {
                    blkSize -= XMM_REGSIZE_BYTES;
                    // one more store needed
                    emit->emitIns_AR_R(simdMov, EA_ATTR(XMM_REGSIZE_BYTES), zeroSIMDReg, frameReg,
                                       alignedLclLo + XMM_REGSIZE_BYTES);
                }
            }

            // Exact multiple of 3 simd lengths (or loop end condition will not be met)
            noway_assert((blkSize % (3 * XMM_REGSIZE_BYTES)) == 0);

            // At least 3 simd lengths remain (as loop is 3x unrolled and we want it to loop at least once)
            assert(blkSize >= (3 * XMM_REGSIZE_BYTES));
            // In range at start of loop
            assert((alignedLclHi - blkSize) >= untrLclLo);
            assert(((alignedLclHi - blkSize) + (XMM_REGSIZE_BYTES * 2)) < (untrLclHi - XMM_REGSIZE_BYTES));
            // In range at end of loop
            assert((alignedLclHi - (3 * XMM_REGSIZE_BYTES) + (2 * XMM_REGSIZE_BYTES)) <=
                   (untrLclHi - XMM_REGSIZE_BYTES));
            assert((alignedLclHi - (blkSize + extraSimd * XMM_REGSIZE_BYTES)) == alignedLclLo);

            // Set loop counter
            emit->emitIns_R_I(INS_mov, EA_PTRSIZE, initReg, -(ssize_t)blkSize);
            // Loop start
            emit->emitIns_ARX_R(simdMov, EA_ATTR(XMM_REGSIZE_BYTES), zeroSIMDReg, frameReg, initReg, 1, alignedLclHi);
            emit->emitIns_ARX_R(simdMov, EA_ATTR(XMM_REGSIZE_BYTES), zeroSIMDReg, frameReg, initReg, 1,
                                alignedLclHi + XMM_REGSIZE_BYTES);
            emit->emitIns_ARX_R(simdMov, EA_ATTR(XMM_REGSIZE_BYTES), zeroSIMDReg, frameReg, initReg, 1,
                                alignedLclHi + 2 * XMM_REGSIZE_BYTES);

            emit->emitIns_R_I(INS_add, EA_PTRSIZE, initReg, XMM_REGSIZE_BYTES * 3);
            // Loop until counter is 0
            emit->emitIns_J(INS_jne, nullptr, -5);

            // initReg will be zero at end of the loop
            *pInitRegZeroed = true;
        }

        if (untrLclHi != alignedLclHi)
        {
            assert(alignmentHiBlkSize > 0);
            assert(alignmentHiBlkSize < XMM_REGSIZE_BYTES);
            assert((alignedLclHi + alignmentHiBlkSize) == untrLclHi);

            zeroReg = GetZeroReg();

            int i = 0;
            for (; i + REGSIZE_BYTES <= alignmentHiBlkSize; i += REGSIZE_BYTES)
            {
                emit->emitIns_AR_R(ins_Store(TYP_I_IMPL), EA_PTRSIZE, zeroReg, frameReg, alignedLclHi + i);
            }
#if defined(TARGET_AMD64)
            assert((i == alignmentHiBlkSize) || (i + (int)sizeof(int) == alignmentHiBlkSize));
            if (i != alignmentHiBlkSize)
            {
                emit->emitIns_AR_R(ins_Store(TYP_INT), EA_4BYTE, zeroReg, frameReg, alignedLclHi + i);
                i += sizeof(int);
            }
#endif // defined(TARGET_AMD64)
            assert(i == alignmentHiBlkSize);
        }
    }
}

void CodeGen::PrologZeroRegs(regMaskTP initRegs, regNumber initReg)
{
    for (regNumber reg = REG_INT_FIRST; reg <= REG_INT_LAST; reg = REG_NEXT(reg))
    {
        if (((initRegs & genRegMask(reg)) == RBM_NONE) || (reg == initReg))
        {
            continue;
        }

        instGen_Set_Reg_To_Zero(EA_PTRSIZE, reg);
    }

    // TODO-MIKE-CQ: Copying from another reg instead of just zeroing with xorps is dubious...
    regNumber zeroReg = REG_NA;

    for (regNumber reg = REG_FP_FIRST; reg <= REG_FP_LAST; reg = REG_NEXT(reg))
    {
        if ((initRegs & genRegMask(reg)) == RBM_NONE)
        {
            continue;
        }

        if (zeroReg == REG_NA)
        {
            GetEmitter()->emitIns_R_R(INS_xorps, EA_16BYTE, reg, reg);
            zeroReg = reg;
            continue;
        }

        GetEmitter()->emitIns_Mov(INS_movaps, EA_16BYTE, reg, zeroReg, /* canSkip */ false);
    }
}

#ifdef TARGET_X86
void CodeGen::PrologInitVarargsStackParamsBaseOffset()
{
    JITDUMP("; PrologInitVarargsStackParamsBaseOffset\n");

    LclVarDsc* varDsc = compiler->lvaGetDesc(compiler->lvaVarargsBaseOfStkArgs);

    noway_assert(compiler->info.compArgsCount > 0);

    GetEmitter()->emitIns_R_S(INS_mov, EA_4BYTE, REG_EAX, compiler->info.compArgsCount - 1, 0);
    GetEmitter()->emitIns_R_AR(INS_mov, EA_4BYTE, REG_EAX, REG_EAX, 0);

    LclVarDsc* lastArg = compiler->lvaGetDesc(compiler->info.compArgsCount - 1);
    noway_assert(!lastArg->lvRegister);
    int32_t offset = lastArg->GetStackOffset();
    assert(offset != BAD_STK_OFFS);
    noway_assert(lastArg->lvFramePointerBased);

    GetEmitter()->emitIns_R_ARR(INS_lea, EA_4BYTE, REG_EAX, genFramePointerReg(), REG_EAX, offset);

    if (varDsc->lvIsInReg())
    {
        GetEmitter()->emitIns_Mov(INS_mov, EA_4BYTE, varDsc->GetRegNum(), REG_EAX, /* canSkip */ true);
    }
    else
    {
        GetEmitter()->emitIns_S_R(INS_mov, EA_4BYTE, REG_EAX, compiler->lvaVarargsBaseOfStkArgs, 0);
    }
}
#endif // TARGET_X86

#ifdef FEATURE_EH_FUNCLETS

// Generates code for an EH funclet prolog.
//
// Funclets have the following incoming arguments:
//
//      catch/filter-handler: rcx = InitialSP, rdx = the exception object that was caught (see GT_CATCH_ARG)
//      filter:               rcx = InitialSP, rdx = the exception object to filter (see GT_CATCH_ARG)
//      finally/fault:        rcx = InitialSP
//
//  Funclets set the following registers on exit:
//
//      catch/filter-handler: rax = the address at which execution should resume (see BBJ_EHCATCHRET)
//      filter:               rax = non-zero if the handler should handle the exception, zero otherwise (see GT_RETFILT)
//      finally/fault:        none
//
//  The AMD64 funclet prolog sequence is:
//
//     push ebp
//     push callee-saved regs
//                      ; TODO-AMD64-CQ: We probably only need to save any callee-save registers that we actually use
//                      ;         in the funclet. Currently, we save the same set of callee-saved regs calculated for
//                      ;         the entire function.
//     sub sp, XXX      ; Establish the rest of the frame.
//                      ;   XXX is determined by lvaOutgoingArgSpaceSize plus space for the PSP slot, aligned
//                      ;   up to preserve stack alignment. If we push an odd number of registers, we also
//                      ;   generate this, to keep the stack aligned.
//
//     ; Fill the PSP slot, for use by the VM (it gets reported with the GC info), or by code generation of nested
//     ;    filters.
//     ; This is not part of the "OS prolog"; it has no associated unwind data, and is not reversed in the funclet
//     ;    epilog.
//     ; Also, re-establish the frame pointer from the PSP.
//
//     mov rbp, [rcx + PSP_slot_InitialSP_offset]       ; Load the PSP (InitialSP of the main function stored in the
//                                                      ; PSP of the dynamically containing funclet or function)
//     mov [rsp + PSP_slot_InitialSP_offset], rbp       ; store the PSP in our frame
//     lea ebp, [rbp + Function_InitialSP_to_FP_delta]  ; re-establish the frame pointer of the parent frame. If
//                                                      ; Function_InitialSP_to_FP_delta==0, we don't need this
//                                                      ; instruction.
//
//  The epilog sequence is then:
//
//     add rsp, XXX
//     pop callee-saved regs    ; if necessary
//     pop rbp
//     ret
//
//  The funclet frame is thus:
//
//      |                       |
//      |-----------------------|
//      |       incoming        |
//      |       arguments       |
//      +=======================+ <---- Caller's SP
//      |    Return address     |
//      |-----------------------|
//      |      Saved EBP        |
//      |-----------------------|
//      |Callee saved registers |
//      |-----------------------|
//      ~  possible 8 byte pad  ~
//      ~     for alignment     ~
//      |-----------------------|
//      |        PSP slot       | // Omitted in CoreRT ABI
//      |-----------------------|
//      |   Outgoing arg space  | // this only exists if the function makes a call
//      |-----------------------| <---- Initial SP
//      |       |               |
//      ~       | Stack grows   ~
//      |       | downward      |
//              V
//
// TODO-AMD64-Bug?: the frame pointer should really point to the PSP slot (the debugger seems to assume this
// in DacDbiInterfaceImpl::InitParentFrameInfo()), or someplace above Initial-SP. There is an AMD64
// UNWIND_INFO restriction that it must be within 240 bytes of Initial-SP. See jit64\amd64\inc\md.h
// "FRAMEPTR OFFSETS" for details.
//
void CodeGen::genFuncletProlog(BasicBlock* block)
{
#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In genFuncletProlog()\n");
    }
#endif

    assert(block != nullptr);
    assert(block->bbFlags & BBF_FUNCLET_BEG);
    assert(isFramePointerUsed());

    ScopedSetVariable<bool> _setGeneratingProlog(&generatingProlog, true);

    compiler->unwindBegProlog();

    // We need to push ebp, since it's callee-saved.
    // We need to push the callee-saved registers. We only need to push the ones that we need, but we don't
    // keep track of that on a per-funclet basis, so we push the same set as in the main function.
    // The only fixed-size frame we need to allocate is whatever is big enough for the PSPSym, since nothing else
    // is stored here (all temps are allocated in the parent frame).
    // We do need to allocate the outgoing argument space, in case there are calls here. This must be the same
    // size as the parent frame's outgoing argument space, to keep the PSPSym offset the same.

    inst_RV(INS_push, REG_FPBASE, TYP_REF);
    compiler->unwindPush(REG_FPBASE);

    // Callee saved int registers are pushed to stack.
    PrologPushCalleeSavedRegisters();

    regMaskTP maskArgRegsLiveIn;
    if ((block->bbCatchTyp == BBCT_FINALLY) || (block->bbCatchTyp == BBCT_FAULT))
    {
        maskArgRegsLiveIn = RBM_ARG_0;
    }
    else
    {
        maskArgRegsLiveIn = RBM_ARG_0 | RBM_ARG_2;
    }

    regNumber initReg       = REG_EBP; // We already saved EBP, so it can be trashed
    bool      initRegZeroed = false;

    PrologAllocLclFrame(genFuncletInfo.fiSpDelta, initReg, &initRegZeroed, maskArgRegsLiveIn);

    // Callee saved float registers are copied to stack in their assigned stack slots
    // after allocating space for them as part of funclet frame.
    PrologPreserveCalleeSavedFloatRegs(genFuncletInfo.fiSpDelta);

    // This is the end of the OS-reported prolog for purposes of unwinding
    compiler->unwindEndProlog();

    // If there is no PSPSym (CoreRT ABI), we are done.
    if (compiler->lvaPSPSym == BAD_VAR_NUM)
    {
        return;
    }

    GetEmitter()->emitIns_R_AR(INS_mov, EA_PTRSIZE, REG_FPBASE, REG_ARG_0, genFuncletInfo.fiPSP_slot_InitialSP_offset);
    GetEmitter()->emitIns_AR_R(INS_mov, EA_PTRSIZE, REG_FPBASE, REG_SPBASE, genFuncletInfo.fiPSP_slot_InitialSP_offset);

    if (genFuncletInfo.fiFunction_InitialSP_to_FP_delta != 0)
    {
        GetEmitter()->emitIns_R_AR(INS_lea, EA_PTRSIZE, REG_FPBASE, REG_FPBASE,
                                   genFuncletInfo.fiFunction_InitialSP_to_FP_delta);
    }
}

void CodeGen::genFuncletEpilog()
{
    JITDUMP("*************** In genFuncletEpilog()\n");

    ScopedSetVariable<bool> _setGeneratingEpilog(&generatingEpilog, true);

    // Restore callee saved XMM regs from their stack slots before modifying SP
    // to position at callee saved int regs.
    genRestoreCalleeSavedFltRegs(genFuncletInfo.fiSpDelta);
    inst_RV_IV(INS_add, REG_SPBASE, genFuncletInfo.fiSpDelta, EA_PTRSIZE);
    genPopCalleeSavedRegisters();
    inst_RV(INS_pop, REG_EBP, TYP_I_IMPL);
    instGen(INS_ret);
}

void CodeGen::genCaptureFuncletPrologEpilogInfo()
{
    if (!compiler->ehAnyFunclets())
    {
        return;
    }

    // Note that compLclFrameSize can't be used (for can we call functions that depend on it),
    // because we're not going to allocate the same size frame as the parent.

    assert(isFramePointerUsed());
    assert(compiler->lvaDoneFrameLayout == Compiler::FINAL_FRAME_LAYOUT);

    genFuncletInfo.fiFunction_InitialSP_to_FP_delta = genSPtoFPdelta();

    assert(outgoingArgSpaceSize % REGSIZE_BYTES == 0);
#ifndef UNIX_AMD64_ABI
    // On win-x64, we always have 4 outgoing argument slots if there are any calls in the function.
    assert((outgoingArgSpaceSize == 0) || (outgoingArgSpaceSize >= 4 * REGSIZE_BYTES));
#endif

    unsigned offset = outgoingArgSpaceSize;

    genFuncletInfo.fiPSP_slot_InitialSP_offset = offset;

    // How much stack do we allocate in the funclet?
    // We need to 16-byte align the stack.

    unsigned totalFrameSize = REGSIZE_BYTES                         // return address
                              + REGSIZE_BYTES                       // pushed EBP
                              + (calleeRegsPushed * REGSIZE_BYTES); // pushed callee-saved int regs, not including EBP

    genFuncletInfo.fiSpDelta = 0;

#ifdef WINDOWS_AMD64_ABI
    if ((calleeSavedModifiedRegs & RBM_ALLFLOAT) != RBM_NONE)
    {
        // Entire 128-bits of XMM register is saved to stack due to ABI encoding requirement.
        // Copying entire XMM register to/from memory will be performant if SP is aligned at XMM_REGSIZE_BYTES boundary.
        unsigned calleeFPRegsSavedSize = genCountBits(calleeSavedModifiedRegs & RBM_ALLFLOAT) * XMM_REGSIZE_BYTES;
        // Alignment padding before pushing entire xmm regs
        unsigned xmmRegsPad = AlignmentPad(totalFrameSize, XMM_REGSIZE_BYTES);

        totalFrameSize += xmmRegsPad + calleeFPRegsSavedSize;
        genFuncletInfo.fiSpDelta += xmmRegsPad + calleeFPRegsSavedSize;
    }
#endif

    unsigned PSPSymSize = (compiler->lvaPSPSym != BAD_VAR_NUM) ? REGSIZE_BYTES : 0;

    totalFrameSize += PSPSymSize + outgoingArgSpaceSize;

    genFuncletInfo.fiSpDelta += AlignmentPad(totalFrameSize, 16);
    genFuncletInfo.fiSpDelta += PSPSymSize + outgoingArgSpaceSize;

#ifdef DEBUG
    if (verbose)
    {
        printf("\n");
        printf("Funclet prolog / epilog info\n");
        printf("   Function InitialSP-to-FP delta: %d\n", genFuncletInfo.fiFunction_InitialSP_to_FP_delta);
        printf("                         SP delta: %d\n", genFuncletInfo.fiSpDelta);
        printf("       PSP slot Initial SP offset: %d\n", genFuncletInfo.fiPSP_slot_InitialSP_offset);
    }

    if (compiler->lvaPSPSym != BAD_VAR_NUM)
    {
        // same offset used in main function and funclet!
        assert(genFuncletInfo.fiPSP_slot_InitialSP_offset == compiler->lvaGetPSPSymInitialSPRelativeOffset());
    }
#endif // DEBUG
}

#endif // FEATURE_EH_FUNCLETS

void CodeGen::genFnEpilog(BasicBlock* block)
{
    JITDUMP("*************** In genFnEpilog()\n");
#ifdef DEBUG
    if (compiler->opts.dspCode)
    {
        printf("\n__epilog:\n");
    }
#endif

    noway_assert(!compiler->opts.MinOpts() || isFramePointerUsed()); // FPO not allowed with minOpts

    ScopedSetVariable<bool> _setGeneratingEpilog(&generatingEpilog, true);

    bool jmpEpilog = ((block->bbFlags & BBF_HAS_JMP) != 0);

    // Restore float registers that were saved to stack before SP is modified.
    genRestoreCalleeSavedFltRegs(lclFrameSize);

#ifdef JIT32_GCENCODER
    // When using the JIT32 GC encoder, we do not start the OS-reported portion of the epilog until after
    // the above call to `genRestoreCalleeSavedFltRegs` because that function
    //   a) does not actually restore any registers: there are none when targeting the Windows x86 ABI,
    //      which is the only target that uses the JIT32 GC encoder
    //   b) may issue a `vzeroupper` instruction to eliminate AVX -> SSE transition penalties.
    // Because the `vzeroupper` instruction is not recognized by the VM's unwinder and there are no
    // callee-save FP restores that the unwinder would need to see, we can avoid the need to change the
    // unwinder (and break binary compat with older versions of the runtime) by starting the epilog
    // after any `vzeroupper` instruction has been emitted. If either of the above conditions changes,
    // we will need to rethink this.
    GetEmitter()->emitStartEpilog();
#endif

    /* Compute the size in bytes we've pushed/popped */

    bool removeEbpFrame = IsFramePointerRequired();

#ifdef TARGET_AMD64
    // We only remove the EBP frame using the frame pointer (using `lea rsp, [rbp + const]`)
    // if we reported the frame pointer in the prolog. The Windows x64 unwinding ABI specifically
    // disallows this `lea` form:
    //
    //    See https://docs.microsoft.com/en-us/cpp/build/prolog-and-epilog?view=msvc-160#epilog-code
    //
    //    "When a frame pointer is not used, the epilog must use add RSP,constant to deallocate the fixed part of the
    //    stack. It may not use lea RSP,constant[RSP] instead. This restriction exists so the unwind code has fewer
    //    patterns to recognize when searching for epilogs."
    //
    // Otherwise, we must use `add RSP, constant`, as stated. So, we need to use the same condition
    // as genFnProlog() used in determining whether to report the frame pointer in the unwind data.
    // This is a subset of the `IsFramePointerRequired()` cases.
    //
    if (removeEbpFrame)
    {
        const bool reportUnwindData = compiler->compLocallocUsed || compiler->opts.compDbgEnC;
        removeEbpFrame              = removeEbpFrame && reportUnwindData;
    }
#endif // TARGET_AMD64

    if (!removeEbpFrame)
    {
        noway_assert(!compiler->compLocallocUsed);

        if (lclFrameSize != 0)
        {
#ifdef TARGET_X86
            if ((lclFrameSize == REGSIZE_BYTES) && !compiler->compJmpOpUsed)
            {
                // Pop a scratch register, it's smaller than ADD.
                GetEmitter()->emitIns_R(INS_pop, EA_4BYTE, REG_ECX);
            }
            else
#endif // TARGET_X86
            {
                GetEmitter()->emitIns_R_I(INS_add, EA_PTRSIZE, REG_RSP, static_cast<int>(lclFrameSize));
            }
        }

        genPopCalleeSavedRegisters();

#ifdef TARGET_AMD64
        // In the case where we have an RSP frame, and no frame pointer reported in the OS unwind info,
        // but we do have a pushed frame pointer and established frame chain, we do need to pop RBP.
        if (isFramePointerUsed())
        {
            GetEmitter()->emitIns_R(INS_pop, EA_8BYTE, REG_RBP);
        }
#endif // TARGET_AMD64

        // Extra OSR adjust to get to where RBP was saved by the original frame, and
        // restore RBP.
        //
        // Note the other callee saves made in that frame are dead, the OSR method
        // will save and restore what it needs.
        if (compiler->opts.IsOSR())
        {
            PatchpointInfo* patchpointInfo    = compiler->info.compPatchpointInfo;
            const int       originalFrameSize = patchpointInfo->FpToSpDelta();

            // Use add since we know the SP-to-FP delta of the original method.
            //
            // If we ever allow the original method to have localloc this will
            // need to change.
            inst_RV_IV(INS_add, REG_SPBASE, originalFrameSize, EA_PTRSIZE);
            inst_RV(INS_pop, REG_EBP, TYP_I_IMPL);
        }
    }
    else
    {
        noway_assert(IsFramePointerRequired());

#ifdef TARGET_X86 // "mov esp, ebp" is not allowed in AMD64 epilogs
        bool needMovEspEbp = false;
#endif

#if DOUBLE_ALIGN
        if (doDoubleAlign())
        {
            //
            // add esp, compLclFrameSize
            //
            // We need not do anything (except the "mov esp, ebp") if
            // compiler->compCalleeRegsPushed==0. However, this is unlikely, and it
            // also complicates the code manager. Hence, we ignore that case.

            noway_assert(lclFrameSize != 0);
            inst_RV_IV(INS_add, REG_SPBASE, lclFrameSize, EA_PTRSIZE);

            needMovEspEbp = true;
        }
        else
#endif // DOUBLE_ALIGN
        {
            bool needLea = false;

            if (compiler->compLocallocUsed)
            {
                // OSR not yet ready for localloc
                assert(!compiler->opts.IsOSR());

                // ESP may be variable if a localloc was actually executed. Reset it.
                //    lea esp, [ebp - compiler->compCalleeRegsPushed * REGSIZE_BYTES]
                needLea = true;
            }
            else if (lclFrameSize != 0)
            {
#ifdef TARGET_X86
                if (calleeRegsPushed == 0)
                {
                    // We will just generate "mov esp, ebp" and be done with it.
                    needMovEspEbp = true;
                }
                else if (lclFrameSize == REGSIZE_BYTES)
                {
                    // Pop a scratch register, it's smaller than LEA.
                    GetEmitter()->emitIns_R(INS_pop, EA_4BYTE, REG_ECX);
                }
                else
#endif // TARGET_X86

                {
                    // We need to make ESP point to the callee-saved registers
                    needLea = true;
                }
            }

            if (needLea)
            {
                int offset;

#ifdef TARGET_AMD64
                // lea esp, [ebp + compiler->compLclFrameSize - genSPtoFPdelta]
                //
                // Case 1: localloc not used.
                // genSPToFPDelta = compiler->compCalleeRegsPushed * REGSIZE_BYTES + compiler->compLclFrameSize
                // offset = compiler->compCalleeRegsPushed * REGSIZE_BYTES;
                // The amount to be subtracted from RBP to point at callee saved int regs.
                //
                // Case 2: localloc used
                // genSPToFPDelta = Min(240, (int)compiler->lvaOutgoingArgSpaceSize)
                // Offset = Amount to be added to RBP to point at callee saved int regs.
                offset = genSPtoFPdelta() - lclFrameSize;

                // Offset should fit within a byte if localloc is not used.
                if (!compiler->compLocallocUsed)
                {
                    noway_assert(offset < UCHAR_MAX);
                }
#else
                // lea esp, [ebp - compiler->compCalleeRegsPushed * REGSIZE_BYTES]
                offset = calleeRegsPushed * REGSIZE_BYTES;
                noway_assert(offset < UCHAR_MAX); // the offset fits in a byte
#endif

                GetEmitter()->emitIns_R_AR(INS_lea, EA_PTRSIZE, REG_SPBASE, REG_FPBASE, -offset);
            }
        }

        genPopCalleeSavedRegisters();

#ifdef TARGET_AMD64
        // Extra OSR adjust to get to where RBP was saved by the original frame.
        //
        // Note the other callee saves made in that frame are dead, the current method
        // will save and restore what it needs.
        if (compiler->opts.IsOSR())
        {
            PatchpointInfo* patchpointInfo    = compiler->info.compPatchpointInfo;
            const int       originalFrameSize = patchpointInfo->FpToSpDelta();

            // Use add since we know the SP-to-FP delta of the original method.
            // We also need to skip over the slot where we pushed RBP.
            //
            // If we ever allow the original method to have localloc this will
            // need to change.
            inst_RV_IV(INS_add, REG_SPBASE, originalFrameSize + TARGET_POINTER_SIZE, EA_PTRSIZE);
        }
#endif

#ifdef TARGET_X86
        if (needMovEspEbp)
        {
            inst_Mov(TYP_INT, REG_ESP, REG_EBP, /* canSkip */ false);
        }
#endif

        inst_RV(INS_pop, REG_EBP, TYP_I_IMPL);
    }

#ifdef JIT32_GCENCODER
    GetEmitter()->emitStartExitSeq();
#endif

    if (jmpEpilog)
    {
        GenJmpEpilog(block);

        return;
    }

#ifndef TARGET_X86
    instGen(INS_ret);
#else
    if ((paramsStackSize == 0) || compiler->info.compIsVarArgs || IsCallerPop(compiler->info.compCallConv))
    {
        instGen(INS_ret);
    }
    else
    {
        GetEmitter()->emitIns_I(INS_ret, EA_4BYTE, paramsStackSize);
    }
#endif // TARGET_X86
}

instruction CodeGen::ins_Copy(var_types type)
{
    assert(emitTypeActSz[type] != 0);

    return varTypeUsesFloatReg(type) ? INS_movaps : INS_mov;
}

instruction CodeGen::ins_Copy(regNumber srcReg, var_types dstType)
{
    return varTypeUsesFloatReg(dstType) != genIsValidFloatReg(srcReg) ? INS_movd : ins_Copy(dstType);
}

instruction CodeGen::ins_Move_Extend(var_types type)
{
    if (varTypeUsesFloatReg(type))
    {
        return INS_movaps;
    }

    if (varTypeIsSmall(type))
    {
        return varTypeIsUnsigned(type) ? INS_movzx : INS_movsx;
    }

    return INS_mov;
}

instruction CodeGen::ins_Load(var_types srcType, bool aligned)
{
    assert(srcType != TYP_STRUCT);

#ifdef FEATURE_SIMD
    if (varTypeIsSIMD(srcType))
    {
        if (srcType == TYP_SIMD8)
        {
            return INS_movsdsse2;
        }

        if (compiler->canUseVexEncoding())
        {
            return aligned ? INS_movapd : INS_movupd;
        }
        else
        {
            return aligned ? INS_movaps : INS_movups;
        }
    }
#endif

    if (varTypeIsFloating(srcType))
    {
        return srcType == TYP_DOUBLE ? INS_movsdsse2 : INS_movss;
    }

    if (varTypeIsSmall(srcType))
    {
        return varTypeIsUnsigned(srcType) ? INS_movzx : INS_movsx;
    }

    return INS_mov;
}

instruction CodeGen::ins_Store(var_types dstType, bool aligned)
{
#ifdef FEATURE_SIMD
    if (varTypeIsSIMD(dstType))
    {
        if (dstType == TYP_SIMD8)
        {
            return INS_movsdsse2;
        }

        if (compiler->canUseVexEncoding())
        {
            return aligned ? INS_movapd : INS_movupd;
        }
        else
        {
            return aligned ? INS_movaps : INS_movups;
        }
    }
#endif

    if (varTypeIsFloating(dstType))
    {
        return dstType == TYP_DOUBLE ? INS_movsdsse2 : INS_movss;
    }

    return INS_mov;
}

void CodeGen::inst_RV_IV(instruction ins, regNumber reg, target_ssize_t val, emitAttr size)
{
    assert(ins != INS_mov);
#ifndef TARGET_64BIT
    assert(size != EA_8BYTE);
#endif

#ifdef TARGET_AMD64
    if ((EA_SIZE(size) == EA_8BYTE) && (!FitsIn<int32_t>(val) || EA_IS_CNS_RELOC(size)))
    {
        assert(!"Invalid immediate for inst_RV_IV");
    }
    else
#endif
    {
        GetEmitter()->emitIns_R_I(ins, size, reg, val);
    }
}

#endif // TARGET_XARCH
