// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "codegen.h"
#include "instr.h"
#include "emit.h"

void CodeGen::instGen(instruction ins)
{
    GetEmitter()->emitIns(ins);

#ifdef TARGET_XARCH
#ifdef PSEUDORANDOM_NOP_INSERTION
    // A workaround necessitated by limitations of emitter
    // if we are scheduled to insert a nop here, we have to delay it
    // hopefully we have not missed any other prefix instructions or places
    // they could be inserted
    if (ins == INS_lock && GetEmitter()->emitNextNop == 0)
    {
        GetEmitter()->emitNextNop = 1;
    }
#endif // PSEUDORANDOM_NOP_INSERTION
#endif
}

void CodeGen::inst_JMP(emitJumpKind jmp, BasicBlock* tgtBlock)
{
#if !FEATURE_FIXED_OUT_ARGS
    // On the x86 we are pushing (and changing the stack level), but on x64 and other archs we have
    // a fixed outgoing args area that we store into and we never change the stack level when calling methods.
    //
    // Thus only on x86 do we need to assert that the stack level at the target block matches the current stack level.
    //
    CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef UNIX_X86_ABI
    // bbTgtStkDepth is a (pure) argument count (stack alignment padding should be excluded).
    assert((tgtBlock->bbTgtStkDepth * sizeof(int) == (genStackLevel - curNestedAlignment)) || isFramePointerUsed());
#else
    assert((tgtBlock->bbTgtStkDepth * sizeof(int) == genStackLevel) || isFramePointerUsed());
#endif
#endif // !FEATURE_FIXED_OUT_ARGS

    GetEmitter()->emitIns_J(emitter::emitJumpKindToIns(jmp), tgtBlock);
}

void CodeGen::inst_SET(emitJumpKind condition, regNumber reg)
{
#ifdef TARGET_XARCH
    instruction ins;

    switch (condition)
    {
        case EJ_js:
            ins = INS_sets;
            break;
        case EJ_jns:
            ins = INS_setns;
            break;
        case EJ_je:
            ins = INS_sete;
            break;
        case EJ_jne:
            ins = INS_setne;
            break;

        case EJ_jl:
            ins = INS_setl;
            break;
        case EJ_jle:
            ins = INS_setle;
            break;
        case EJ_jge:
            ins = INS_setge;
            break;
        case EJ_jg:
            ins = INS_setg;
            break;

        case EJ_jb:
            ins = INS_setb;
            break;
        case EJ_jbe:
            ins = INS_setbe;
            break;
        case EJ_jae:
            ins = INS_setae;
            break;
        case EJ_ja:
            ins = INS_seta;
            break;

        case EJ_jp:
            ins = INS_setp;
            break;
        case EJ_jnp:
            ins = INS_setnp;
            break;

        default:
            NO_WAY("unexpected condition type");
            return;
    }

    assert(genRegMask(reg) & RBM_BYTE_REGS);

    GetEmitter()->emitIns_R(ins, EA_1BYTE, reg);

#elif defined(TARGET_ARM64)

    insCond cond;

    switch (condition)
    {
        case EJ_eq:
            cond = INS_COND_EQ;
            break;
        case EJ_ne:
            cond = INS_COND_NE;
            break;
        case EJ_hs:
            cond = INS_COND_HS;
            break;
        case EJ_lo:
            cond = INS_COND_LO;
            break;

        case EJ_mi:
            cond = INS_COND_MI;
            break;
        case EJ_pl:
            cond = INS_COND_PL;
            break;
        case EJ_vs:
            cond = INS_COND_VS;
            break;
        case EJ_vc:
            cond = INS_COND_VC;
            break;

        case EJ_hi:
            cond = INS_COND_HI;
            break;
        case EJ_ls:
            cond = INS_COND_LS;
            break;
        case EJ_ge:
            cond = INS_COND_GE;
            break;
        case EJ_lt:
            cond = INS_COND_LT;
            break;

        case EJ_gt:
            cond = INS_COND_GT;
            break;
        case EJ_le:
            cond = INS_COND_LE;
            break;

        default:
            NO_WAY("unexpected condition type");
            return;
    }
    GetEmitter()->emitIns_R_COND(INS_cset, EA_8BYTE, reg, cond);
#else
    NYI("inst_SET");
#endif
}

void CodeGen::inst_RV(instruction ins, regNumber reg, var_types type)
{
    GetEmitter()->emitIns_R(ins, emitActualTypeSize(type), reg);
}

void CodeGen::inst_Mov(var_types dstType, regNumber dstReg, regNumber srcReg, bool canSkip)
{
    GetEmitter()->emitIns_Mov(ins_Copy(srcReg, dstType), emitActualTypeSize(dstType), dstReg, srcReg, canSkip);
}

void CodeGen::inst_RV_RV(instruction ins, regNumber reg1, regNumber reg2, var_types type)
{
    GetEmitter()->emitIns_R_R(ins, emitActualTypeSize(type), reg1, reg2);
}

void CodeGen::inst_IV(instruction ins, cnsval_ssize_t val)
{
    GetEmitter()->emitIns_I(ins, EA_PTRSIZE, val);
}

bool CodeGen::IsLocalMemoryOperand(GenTree* op, unsigned* lclNum, unsigned* lclOffs)
{
    if (op->isUsedFromSpillTemp())
    {
        assert(op->IsRegOptional());
        assert(op->IsRegSpilled(0));

        SpillTemp* temp = spillTemps.UseSpillTemp(op, 0);

        *lclNum  = temp->GetNum();
        *lclOffs = 0;

        return true;
    }

    assert(op->isContained());

    if (op->OperIs(GT_LCL_FLD))
    {
        *lclNum  = op->AsLclFld()->GetLclNum();
        *lclOffs = op->AsLclFld()->GetLclOffs();

        return true;
    }

    if (op->OperIs(GT_LCL_VAR))
    {
        assert(op->IsRegOptional() || !compiler->lvaGetDesc(op->AsLclVar())->IsRegCandidate());

        *lclNum  = op->AsLclVar()->GetLclNum();
        *lclOffs = 0;

        return true;
    }

    return false;
}

#ifdef TARGET_XARCH

void CodeGen::inst_RV_SH(instruction ins, emitAttr size, regNumber reg, unsigned val)
{
#ifdef TARGET_AMD64
    // X64 JB BE insures only encodable values make it here.
    // x86 can encode 8 bits, though it masks down to 5 or 6
    // depending on 32-bit or 64-bit registers are used.
    // Here we will allow anything that is encodable.
    assert(val < 256);
#endif

    if (val == 1)
    {
        GetEmitter()->emitIns_R(MapShiftInsToShiftBy1Ins(ins), size, reg);
    }
    else
    {
        GetEmitter()->emitIns_R_I(MapShiftInsToShiftByImmIns(ins), size, reg, val);
    }
}

bool CodeGen::IsMemoryOperand(
    GenTree* op, unsigned* lclNum, unsigned* lclOffs, GenTree** addr, CORINFO_FIELD_HANDLE* field)
{
    if (IsLocalMemoryOperand(op, lclNum, lclOffs))
    {
        *addr  = nullptr;
        *field = nullptr;

        return true;
    }

    if (GenTreeDblCon* dblCon = op->IsDblCon())
    {
        *addr  = nullptr;
        *field = GetEmitter()->emitFltOrDblConst(dblCon->GetValue(), emitTypeSize(dblCon->GetType()));

        return true;
    }

    GenTree* loadAddr;

    if (op->OperIs(GT_IND))
    {
        loadAddr = op->AsIndir()->GetAddr();
    }
#ifdef FEATURE_HW_INTRINSICS
    else if (GenTreeHWIntrinsic* intrin = op->IsHWIntrinsic())
    {
        assert(intrin->OperIsMemoryLoad());
        assert(intrin->IsUnary());

        loadAddr = intrin->GetOp(0);
    }
#endif
    else
    {
        return false;
    }

    if (loadAddr->OperIs(GT_LCL_ADDR))
    {
        assert(loadAddr->isContained());

        *lclNum  = loadAddr->AsLclAddr()->GetLclNum();
        *lclOffs = loadAddr->AsLclAddr()->GetLclOffs();
        *addr    = nullptr;
        *field   = nullptr;
    }
    else
    {
        *addr  = loadAddr;
        *field = nullptr;
    }

    return true;
}

void CodeGen::emitInsUnary(instruction ins, emitAttr attr, GenTree* src)
{
    emitter* emit = GetEmitter();
    unsigned lclNum;
    unsigned lclOffs;

    if (src->isUsedFromReg())
    {
        emit->emitIns_R(ins, attr, src->GetRegNum());
    }
    else if (IsLocalMemoryOperand(src, &lclNum, &lclOffs))
    {
        emit->emitIns_S(ins, attr, lclNum, lclOffs);
    }
    else
    {
        emit->emitIns_A(ins, attr, src->AsIndir()->GetAddr());
    }
}

void CodeGen::emitInsBinary(instruction ins, emitAttr attr, GenTree* dst, GenTree* src)
{
    assert(!emitter::instrHasImplicitRegPairDest(ins));

    emitter* emit  = GetEmitter();
    GenTree* memOp = nullptr;
    GenTree* immOp = nullptr;
    GenTree* regOp = nullptr;

    if (dst->isContained() || (dst->OperIs(GT_LCL_FLD) && (dst->GetRegNum() == REG_NA)) || dst->isUsedFromSpillTemp())
    {
        assert(dst->isUsedFromMemory() || (dst->GetRegNum() == REG_NA) || emitter::instrIs3opImul(ins));

        memOp = dst;

        if (src->isContained())
        {
            assert(src->IsIntCon());

            immOp = src;
        }
        else
        {
            assert(src->isUsedFromReg());

            regOp = src;
        }
    }
    else if (src->isContained() || src->isUsedFromSpillTemp())
    {
        assert(dst->isUsedFromReg());

        regOp = dst;

        if ((src->IsIntCon() || src->IsDblCon()) && !src->isUsedFromSpillTemp())
        {
            assert(!src->isUsedFromMemory() || src->IsDblCon());

            immOp = src;
        }
        else
        {
            assert(src->isUsedFromMemory());

            memOp = src;
        }
    }
    else
    {
        assert(dst->isUsedFromReg() && src->isUsedFromReg());

        emit->emitIns_R_R(ins, attr, dst->GetRegNum(), src->GetRegNum());

        return;
    }

    if (memOp == nullptr)
    {
        if (GenTreeDblCon* dblCon = immOp->IsDblCon())
        {
            CORINFO_FIELD_HANDLE field = emit->emitFltOrDblConst(dblCon->GetValue(), emitTypeSize(dblCon->GetType()));
            emit->emitIns_R_C(ins, attr, regOp->GetRegNum(), field);
        }
        else
        {
            assert(!dst->isContained());

            emit->emitIns_R_I(ins, attr, regOp->GetRegNum(), immOp->AsIntCon()->GetValue());
        }

        return;
    }

    unsigned lclNum;
    unsigned lclOffs;

    if (IsLocalMemoryOperand(memOp, &lclNum, &lclOffs))
    {
        if (memOp == src)
        {
            emit->emitIns_R_S(ins, attr, regOp->GetRegNum(), lclNum, lclOffs);
        }
        else if (immOp != nullptr)
        {
            emit->emitIns_S_I(ins, attr, lclNum, lclOffs, immOp->AsIntCon()->GetInt32Value());
        }
        else
        {
            emit->emitIns_S_R(ins, attr, regOp->GetRegNum(), lclNum, lclOffs);
        }
    }
    else
    {
        GenTree* addr = memOp->AsIndir()->GetAddr();

        if (memOp == src)
        {
            emit->emitIns_R_A(ins, attr, regOp->GetRegNum(), addr);
        }
        else if (immOp != nullptr)
        {
            emit->emitIns_A_I(ins, attr, addr, immOp->AsIntCon()->GetInt32Value());
        }
        else
        {
            emit->emitIns_A_R(ins, attr, addr, regOp->GetRegNum());
        }
    }
}

void CodeGen::emitInsLoad(instruction ins, emitAttr attr, regNumber reg, GenTree* addr)
{
    assert(emitter::emitInsModeFormat(ins, emitter::IF_RRD_ARD) == emitter::IF_RWR_ARD);

    GetEmitter()->emitIns_R_A(ins, attr, reg, addr);
}

void CodeGen::emitInsStore(instruction ins, emitAttr attr, GenTree* addr, GenTree* data)
{
    if (GenTreeIntCon* imm = data->IsContainedIntCon())
    {
        GetEmitter()->emitIns_A_I(ins, attr, addr, imm->GetInt32Value());
    }
    else
    {
        GetEmitter()->emitIns_A_R(ins, attr, addr, data->GetRegNum());
    }
}

void CodeGen::inst_RV_TT_IV(instruction ins, emitAttr attr, regNumber reg1, GenTree* rmOp, int ival)
{
    noway_assert(GetEmitter()->emitVerifyEncodable(ins, EA_SIZE(attr), reg1));

    if (rmOp->isContained() || rmOp->isUsedFromSpillTemp())
    {
        unsigned             lclNum;
        unsigned             lclOffs;
        GenTree*             addr;
        CORINFO_FIELD_HANDLE field;

        if (!IsMemoryOperand(rmOp, &lclNum, &lclOffs, &addr, &field))
        {
            unreached();
        }
        else if (addr != nullptr)
        {
            GetEmitter()->emitIns_R_A_I(ins, attr, reg1, addr, ival);
        }
        else if (field != nullptr)
        {
            GetEmitter()->emitIns_R_C_I(ins, attr, reg1, field, ival);
        }
        else
        {
            GetEmitter()->emitIns_R_S_I(ins, attr, reg1, lclNum, lclOffs, ival);
        }
    }
    else
    {
        regNumber rmOpReg = rmOp->GetRegNum();
        GetEmitter()->emitIns_SIMD_R_R_I(ins, attr, reg1, rmOpReg, ival);
    }
}

void CodeGen::inst_RV_RV_TT(
    instruction ins, emitAttr size, regNumber targetReg, regNumber op1Reg, GenTree* op2, bool isRMW)
{
    noway_assert(GetEmitter()->emitVerifyEncodable(ins, EA_SIZE(size), targetReg));

    // TODO-XArch-CQ: Commutative operations can have op1 be contained
    // TODO-XArch-CQ: Non-VEX encoded instructions can have both ops contained

    if (op2->isContained() || op2->isUsedFromSpillTemp())
    {
        unsigned             lclNum;
        unsigned             lclOffs;
        GenTree*             addr;
        CORINFO_FIELD_HANDLE field;

        if (!IsMemoryOperand(op2, &lclNum, &lclOffs, &addr, &field))
        {
            unreached();
        }
        else if (addr != nullptr)
        {
            GetEmitter()->emitIns_SIMD_R_R_A(ins, size, targetReg, op1Reg, addr);
        }
        else if (field != nullptr)
        {
            GetEmitter()->emitIns_SIMD_R_R_C(ins, size, targetReg, op1Reg, field);
        }
        else
        {
            GetEmitter()->emitIns_SIMD_R_R_S(ins, size, targetReg, op1Reg, lclNum, lclOffs);
        }
    }
    else
    {
        regNumber op2Reg = op2->GetRegNum();

        if ((op1Reg != targetReg) && (op2Reg == targetReg) && isRMW)
        {
            // We have "reg2 = reg1 op reg2" where "reg1 != reg2" on a RMW instruction.
            //
            // For non-commutative instructions, we should have ensured that op2 was marked
            // delay free in order to prevent it from getting assigned the same register
            // as target. However, for commutative instructions, we can just swap the operands
            // in order to have "reg2 = reg2 op reg1" which will end up producing the right code.

            op2Reg = op1Reg;
            op1Reg = targetReg;
        }

        GetEmitter()->emitIns_SIMD_R_R_R(ins, size, targetReg, op1Reg, op2Reg);
    }
}
#endif // TARGET_XARCH

//------------------------------------------------------------------------
// ins_StoreFromSrc: Get the machine dependent instruction for performing a store to dstType on the stack from a srcReg.
//
// Arguments:
//   srcReg  - the source register for the store
//   dstType - the destination type
//   aligned - whether the destination is properly aligned if dstType is a SIMD type
//
// Return Value:
//   the instruction to use
//
instruction CodeGen::ins_StoreFromSrc(regNumber srcReg, var_types dstType, bool aligned /*=false*/)
{
    assert(srcReg != REG_NA);

    bool dstIsFloatType = varTypeUsesFloatReg(dstType);
    bool srcIsFloatReg  = genIsValidFloatReg(srcReg);

    if (srcIsFloatReg == dstIsFloatType)
    {
        return ins_Store(dstType, aligned);
    }
    else
    {
        // We know that we are writing to memory, so make the destination type same
        // as the source type.
        var_types dstTypeForStore = TYP_UNDEF;
        unsigned  dstSize         = genTypeSize(dstType);
        switch (dstSize)
        {
            case 4:
                dstTypeForStore = srcIsFloatReg ? TYP_FLOAT : TYP_INT;
                break;
#if defined(TARGET_64BIT)
            case 8:
                dstTypeForStore = srcIsFloatReg ? TYP_DOUBLE : TYP_LONG;
                break;
#endif // TARGET_64BIT
            default:
                assert(!"unexpected write to the stack.");
                break;
        }
        return ins_Store(dstTypeForStore, aligned);
    }
}
