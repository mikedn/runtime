// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#include "codegen.h"
#include "instr.h"
#include "emit.h"

#ifdef DEBUG

// Returns the string representation of the given CPU instruction.
const char* insName(instruction ins)
{
    // clang-format off
    static const char* const insNames[] =
    {
#if defined(TARGET_XARCH)
#define INST0(id, nm, um, mr,                 flags) nm,
#define INST1(id, nm, um, mr,                 flags) nm,
#define INST2(id, nm, um, mr, mi,             flags) nm,
#define INST3(id, nm, um, mr, mi, rm,         flags) nm,
#define INST4(id, nm, um, mr, mi, rm, a4,     flags) nm,
#define INST5(id, nm, um, mr, mi, rm, a4, rr, flags) nm,
#include "instrs.h"

#elif defined(TARGET_ARM)
#define INST1(id, nm, fp, ldst, fmt, e1                                 ) nm,
#define INST2(id, nm, fp, ldst, fmt, e1, e2                             ) nm,
#define INST3(id, nm, fp, ldst, fmt, e1, e2, e3                         ) nm,
#define INST4(id, nm, fp, ldst, fmt, e1, e2, e3, e4                     ) nm,
#define INST5(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5                 ) nm,
#define INST6(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6             ) nm,
#define INST8(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8     ) nm,
#define INST9(id, nm, fp, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9 ) nm,
#include "instrs.h"

#elif defined(TARGET_ARM64)
#define INST1(id, nm, ldst, fmt, e1                                 ) nm,
#define INST2(id, nm, ldst, fmt, e1, e2                             ) nm,
#define INST3(id, nm, ldst, fmt, e1, e2, e3                         ) nm,
#define INST4(id, nm, ldst, fmt, e1, e2, e3, e4                     ) nm,
#define INST5(id, nm, ldst, fmt, e1, e2, e3, e4, e5                 ) nm,
#define INST6(id, nm, ldst, fmt, e1, e2, e3, e4, e5, e6             ) nm,
#define INST9(id, nm, ldst, fmt, e1, e2, e3, e4, e5, e6, e7, e8, e9 ) nm,
#include "instrs.h"

#else
#error "Unknown TARGET"
#endif
    };
    // clang-format on

    assert(ins < _countof(insNames));
    assert(insNames[ins] != nullptr);

    return insNames[ins];
}

//-----------------------------------------------------------------------------
// genInsDisplayName: Get a fully-formed instruction display name. This only handles
// the xarch case of prepending a "v", not the arm case of appending an "s".
// This can be called up to four times in a single 'printf' before the static buffers
// get reused.
//
// Returns:
//    String with instruction name
//
const char* CodeGen::genInsDisplayName(emitter::instrDesc* id)
{
    instruction ins  = id->idIns();
    const char* name = insName(ins);

#ifdef TARGET_XARCH
    const int       TEMP_BUFFER_LEN = 40;
    static unsigned curBuf          = 0;
    static char     buf[4][TEMP_BUFFER_LEN];
    const char*     retbuf;

    if (GetEmitter()->IsAVXInstruction(ins) && !GetEmitter()->IsBMIInstruction(ins))
    {
        sprintf_s(buf[curBuf], TEMP_BUFFER_LEN, "v%s", name);
        retbuf = buf[curBuf];
        curBuf = (curBuf + 1) % 4;
        return retbuf;
    }
#endif // TARGET_XARCH

    return name;
}

#endif

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

void CodeGen::inst_RV(instruction ins, regNumber reg, var_types type, emitAttr size)
{
    if (size == EA_UNKNOWN)
    {
        size = emitActualTypeSize(type);
    }

    GetEmitter()->emitIns_R(ins, size, reg);
}

/*****************************************************************************
 *
 *  Generate a "mov reg1, reg2" instruction.
 */
void CodeGen::inst_Mov(var_types dstType,
                       regNumber dstReg,
                       regNumber srcReg,
                       bool      canSkip,
                       emitAttr  size,
                       insFlags  flags /* = INS_FLAGS_DONT_CARE */)
{
    instruction ins = ins_Copy(srcReg, dstType);

    if (size == EA_UNKNOWN)
    {
        size = emitActualTypeSize(dstType);
    }

#ifdef TARGET_ARM
    GetEmitter()->emitIns_Mov(ins, size, dstReg, srcReg, canSkip, flags);
#else
    GetEmitter()->emitIns_Mov(ins, size, dstReg, srcReg, canSkip);
#endif
}

/*****************************************************************************
 *
 *  Generate a "mov reg1, reg2" instruction.
 */
void CodeGen::inst_Mov_Extend(var_types srcType,
                              bool      srcInReg,
                              regNumber dstReg,
                              regNumber srcReg,
                              bool      canSkip,
                              emitAttr  size,
                              insFlags  flags /* = INS_FLAGS_DONT_CARE */)
{
    instruction ins = ins_Move_Extend(srcType, srcInReg);

    if (size == EA_UNKNOWN)
    {
        size = emitActualTypeSize(srcType);
    }

#ifdef TARGET_ARM
    GetEmitter()->emitIns_Mov(ins, size, dstReg, srcReg, canSkip, flags);
#else
    GetEmitter()->emitIns_Mov(ins, size, dstReg, srcReg, canSkip);
#endif
}

void CodeGen::inst_RV_RV(instruction ins, regNumber reg1, regNumber reg2, var_types type, emitAttr size)
{
    if (size == EA_UNKNOWN)
    {
        size = emitActualTypeSize(type);
    }

    GetEmitter()->emitIns_R_R(ins, size, reg1, reg2);
}

void CodeGen::inst_RV_RV_RV(instruction ins, regNumber reg1, regNumber reg2, regNumber reg3, emitAttr size)
{
    GetEmitter()->emitIns_R_R_R(ins, size, reg1, reg2, reg3);
}

void CodeGen::inst_IV(instruction ins, cnsval_ssize_t val)
{
    GetEmitter()->emitIns_I(ins, EA_PTRSIZE, val);
}

void CodeGen::inst_set_SV_var(GenTreeLclVar* node)
{
    assert(node->OperIs(GT_LCL_VAR, GT_LCL_VAR_ADDR, GT_STORE_LCL_VAR));

    INDEBUG(GetEmitter()->emitVarRefOffs = node->gtLclILoffs;)
}

void CodeGen::inst_RV_IV(instruction ins, regNumber reg, target_ssize_t val, emitAttr size)
{
#if !defined(TARGET_64BIT)
    assert(size != EA_8BYTE);
#endif

#ifdef TARGET_ARM
    if (validImmForInstr(ins, val, INS_FLAGS_DONT_CARE))
    {
        GetEmitter()->emitIns_R_I(ins, size, reg, val, INS_FLAGS_DONT_CARE);
    }
    else if (ins == INS_mov)
    {
        instGen_Set_Reg_To_Imm(size, reg, val);
    }
    else
    {
        // TODO-Cleanup: Add a comment about why this is unreached() for RyuJIT backend.
        unreached();
    }
#elif defined(TARGET_ARM64)
    // TODO-Arm64-Bug: handle large constants!
    // Probably need something like the ARM case above: if (validImmForInstr(ins, val)) ...
    assert(ins != INS_cmp);
    assert(ins != INS_tst);
    assert(ins != INS_mov);
    GetEmitter()->emitIns_R_R_I(ins, size, reg, reg, val);
#else // !TARGET_ARM
#ifdef TARGET_AMD64
    // Instead of an 8-byte immediate load, a 4-byte immediate will do fine
    // as the high 4 bytes will be zero anyway.
    if (size == EA_8BYTE && ins == INS_mov && ((val & 0xFFFFFFFF00000000LL) == 0))
    {
        size = EA_4BYTE;
        GetEmitter()->emitIns_R_I(ins, size, reg, val);
    }
    else if (EA_SIZE(size) == EA_8BYTE && ins != INS_mov && (((int)val != val) || EA_IS_CNS_RELOC(size)))
    {
        assert(!"Invalid immediate for inst_RV_IV");
    }
    else
#endif // TARGET_AMD64
    {
        GetEmitter()->emitIns_R_I(ins, size, reg, val);
    }
#endif // !TARGET_ARM
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

void CodeGen::inst_TT(instruction ins, GenTreeLclVar* node)
{
    assert(node->OperIs(GT_LCL_VAR));
    assert(!node->IsRegSpilled(0));

    inst_set_SV_var(node);

    unsigned lclNum = node->GetLclNum();
    assert(lclNum < compiler->lvaCount);
    GetEmitter()->emitIns_S(ins, emitActualTypeSize(node->GetType()), lclNum, 0);
}

void CodeGen::inst_RV_TT(instruction ins, emitAttr size, regNumber reg, GenTreeLclVar* node)
{
    assert(ins == INS_mov);
    assert(reg != REG_STK);
    assert(!node->IsRegSpilled(0));
    assert(node->OperIs(GT_LCL_VAR));

    inst_set_SV_var(node);

    GetEmitter()->emitIns_R_S(ins, size, reg, node->GetLclNum(), 0);
}

bool CodeGen::IsLocalMemoryOperand(GenTree* op, unsigned* lclNum, unsigned* lclOffs)
{
    if (op->isUsedFromSpillTemp())
    {
        assert(op->IsRegOptional());

        TempDsc* tmpDsc = getSpillTempDsc(op);
        *lclNum         = tmpDsc->tdTempNum();
        *lclOffs        = 0;
        regSet.tmpRlsTemp(tmpDsc);

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

    if (loadAddr->OperIs(GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR))
    {
        assert(loadAddr->isContained());

        *lclNum  = loadAddr->AsLclVarCommon()->GetLclNum();
        *lclOffs = loadAddr->AsLclVarCommon()->GetLclOffs();
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

#ifdef TARGET_ARM
bool CodeGenInterface::validImmForInstr(instruction ins, target_ssize_t imm, insFlags flags)
{
    if (emitter::emitInsIsLoadOrStore(ins) && !emitter::instIsFP(ins))
    {
        return validDispForLdSt(imm, TYP_INT);
    }

    switch (ins)
    {
        case INS_cmp:
        case INS_cmn:
            return validImmForAlu(imm) || validImmForAlu(-imm);
        case INS_and:
        case INS_bic:
        case INS_orr:
        case INS_orn:
        case INS_mvn:
            return validImmForAlu(imm) || validImmForAlu(~imm);
        case INS_mov:
            return validImmForMov(imm);
        case INS_addw:
        case INS_subw:
            return (unsigned_abs(imm) <= 0x00000fff) && (flags != INS_FLAGS_SET); // 12-bit immediate
        case INS_add:
        case INS_sub:
            return validImmForAdd(imm, flags);
        case INS_tst:
        case INS_eor:
        case INS_teq:
        case INS_adc:
        case INS_sbc:
        case INS_rsb:
            return validImmForAlu(imm);
        case INS_asr:
        case INS_lsl:
        case INS_lsr:
        case INS_ror:
            return (imm > 0) && (imm <= 32);
        case INS_vstr:
        case INS_vldr:
            return (imm & 0x3FC) == imm;
        default:
            return false;
    }
}

bool CodeGenInterface::validDispForLdSt(target_ssize_t disp, var_types type)
{
    return varTypeIsFloating(type) ? ((disp & 0x3FC) == disp) : ((disp >= -0x00ff) && (disp <= 0x0fff));
}

bool CodeGenInterface::validImmForAlu(target_ssize_t imm)
{
    return emitter::emitIns_valid_imm_for_alu(imm);
}

bool CodeGenInterface::validImmForMov(target_ssize_t imm)
{
    return emitter::emitIns_valid_imm_for_mov(imm);
}

bool CodeGenInterface::validImmForAdd(target_ssize_t imm, insFlags flags)
{
    return emitter::emitIns_valid_imm_for_add(imm, flags);
}

bool CodeGen::arm_Valid_Imm_For_Add(target_ssize_t imm, insFlags flags)
{
    return emitter::emitIns_valid_imm_for_add(imm, flags);
}

// Check "add Rd,SP,i10"
bool CodeGen::arm_Valid_Imm_For_Add_SP(target_ssize_t imm)
{
    return emitter::emitIns_valid_imm_for_add_sp(imm);
}

bool CodeGenInterface::validImmForBL(ssize_t addr)
{
    return
        // If we are running the altjit for NGEN, then assume we can use the "BL" instruction.
        // This matches the usual behavior for NGEN, since we normally do generate "BL".
        (!compiler->info.compMatchedVM && compiler->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT)) ||
        (compiler->eeGetRelocTypeHint((void*)addr) == IMAGE_REL_BASED_THUMB_BRANCH24);
}

#endif // TARGET_ARM

#ifdef TARGET_ARM64
bool CodeGenInterface::validImmForBL(ssize_t addr)
{
    // On arm64, we always assume a call target is in range and generate a 28-bit relative
    // 'bl' instruction. If this isn't sufficient range, the VM will generate a jump stub when
    // we call recordRelocation(). See the IMAGE_REL_ARM64_BRANCH26 case in jitinterface.cpp
    // (for JIT) or zapinfo.cpp (for NGEN). If we cannot allocate a jump stub, it is fatal.
    return true;
}
#endif // TARGET_ARM64

// Get the machine dependent instruction for performing sign/zero extension.
instruction CodeGen::ins_Move_Extend(var_types srcType, bool srcInReg)
{
#if defined(TARGET_XARCH)
    if (varTypeIsSIMD(srcType))
    {
        // SSE2/AVX requires destination to be a reg always.
        // If src is in reg means, it is a reg-reg move.
        //
        // SSE2 Note: always prefer movaps/movups over movapd/movupd since the
        // former doesn't require 66h prefix and one byte smaller than the
        // latter.
        //
        // TODO-CQ: based on whether src type is aligned use movaps instead

        return srcInReg ? INS_movaps : INS_movups;
    }

    if (varTypeIsFloating(srcType))
    {
        if (!srcInReg)
        {
            return srcType == TYP_DOUBLE ? INS_movsdsse2 : INS_movss;
        }

        return INS_movaps;
    }

    if (varTypeIsSmall(srcType))
    {
        return varTypeIsUnsigned(srcType) ? INS_movzx : INS_movsx;
    }

    return INS_mov;
#elif defined(TARGET_ARMARCH)
    if (varTypeIsSIMD(srcType))
    {
#ifdef TARGET_ARM64
        return srcInReg ? INS_mov : ins_Load(srcType);
#else
        assert(!"unhandled SIMD type");
        return INS_invalid;
#endif
    }

#if defined(TARGET_ARM)
    if (varTypeIsFloating(srcType))
    {
        return INS_vmov;
    }
#else
    if (varTypeIsFloating(srcType))
        return INS_mov;
#endif

    if (!srcInReg)
    {
        return ins_Load(srcType);
    }

    if (varTypeIsByte(srcType))
    {
        return varTypeIsUnsigned(srcType) ? INS_uxtb : INS_sxtb;
    }

    if (varTypeIsShort(srcType))
    {
        return varTypeIsUnsigned(srcType) ? INS_uxth : INS_sxth;
    }

#ifdef TARGET_ARM64
    if (srcType == TYP_INT)
    {
        return INS_sxtw;
    }
#endif

    return INS_mov;
#else
#error "Unknown TARGET"
#endif
}

// Get the machine dependent instruction for performing a load for srcType
instruction CodeGenInterface::ins_Load(var_types srcType, bool aligned)
{
    assert(srcType != TYP_STRUCT);

#if defined(TARGET_XARCH)
    if (varTypeIsSIMD(srcType))
    {
#ifdef FEATURE_SIMD
        if (srcType == TYP_SIMD8)
        {
            return INS_movsdsse2;
        }
#endif
        if (compiler->canUseVexEncoding())
        {
            return aligned ? INS_movapd : INS_movupd;
        }

        // SSE2 Note: always prefer movaps/movups over movapd/movupd since the
        // former doesn't require 66h prefix and one byte smaller than the
        // latter.
        return aligned ? INS_movaps : INS_movups;
    }

    if (varTypeIsFloating(srcType))
    {
        return (srcType == TYP_DOUBLE) ? INS_movsdsse2 : INS_movss;
    }

    if (varTypeIsSmall(srcType))
    {
        return varTypeIsUnsigned(srcType) ? INS_movzx : INS_movsx;
    }

    return INS_mov;
#elif defined(TARGET_ARMARCH)
#ifdef TARGET_ARM
    if (varTypeUsesFloatReg(srcType))
    {
        assert(!varTypeIsSIMD(srcType));
        return INS_vldr;
    }
#endif

    if (varTypeIsSmall(srcType))
    {
        if (varTypeIsByte(srcType))
        {
            return varTypeIsUnsigned(srcType) ? INS_ldrb : INS_ldrsb;
        }

        assert(varTypeIsShort(srcType));
        return varTypeIsUnsigned(srcType) ? INS_ldrh : INS_ldrsh;
    }

    return INS_ldr;
#else
#error "Unknown TARGET"
#endif
}

// Get the machine dependent instruction for performing a reg-reg copy for dstType
instruction CodeGen::ins_Copy(var_types dstType)
{
    assert(emitTypeActSz[dstType] != 0);

#if defined(TARGET_XARCH)
    return varTypeUsesFloatReg(dstType) ? INS_movaps : INS_mov;
#elif defined(TARGET_ARM64)
    return varTypeIsFloating(dstType) ? INS_fmov : INS_mov;
#elif defined(TARGET_ARM)
    return varTypeIsFloating(dstType) ? INS_vmov : INS_mov;
#else
#error "Unknown TARGET"
#endif
}

// Get the machine dependent instruction for performing a reg-reg copy from srcReg
// to a register of dstType.
instruction CodeGen::ins_Copy(regNumber srcReg, var_types dstType)
{
    bool dstIsFloatReg = varTypeUsesFloatReg(dstType);
    bool srcIsFloatReg = genIsValidFloatReg(srcReg);

    if (srcIsFloatReg == dstIsFloatReg)
    {
        return ins_Copy(dstType);
    }

#if defined(TARGET_XARCH)
    return INS_movd;
#elif defined(TARGET_ARM64)
    return dstIsFloatReg ? INS_fmov : INS_mov;
#elif defined(TARGET_ARM)
    if (dstIsFloatReg)
    {
        // Can't have LONG in a register.
        assert(dstType == TYP_FLOAT);
        return INS_vmov_i2f;
    }
    else
    {
        // Can't have LONG in a register.
        assert(dstType == TYP_INT);
        return INS_vmov_f2i;
    }
#else
#error "Unknown TARGET"
#endif
}

// Get the machine dependent instruction for performing a store for dstType
instruction CodeGenInterface::ins_Store(var_types dstType, bool aligned)
{
#if defined(TARGET_XARCH)
    if (varTypeIsSIMD(dstType))
    {
#ifdef FEATURE_SIMD
        if (dstType == TYP_SIMD8)
        {
            return INS_movsdsse2;
        }
#endif
        if (compiler->canUseVexEncoding())
        {
            return aligned ? INS_movapd : INS_movupd;
        }

        // SSE2 Note: always prefer movaps/movups over movapd/movupd since the
        // former doesn't require 66h prefix and one byte smaller than the
        // latter.
        return aligned ? INS_movaps : INS_movups;
    }

    if (varTypeIsFloating(dstType))
    {
        return (dstType == TYP_DOUBLE) ? INS_movsdsse2 : INS_movss;
    }

    return INS_mov;
#elif defined(TARGET_ARMARCH)
#ifdef TARGET_ARM
    if (varTypeUsesFloatReg(dstType))
    {
        assert(!varTypeIsSIMD(dstType));
        return INS_vstr;
    }
#endif

    if (varTypeIsSmall(dstType))
    {
        return varTypeIsByte(dstType) ? INS_strb : INS_strh;
    }

    return INS_str;
#else
#error "Unknown TARGET"
#endif
}

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
instruction CodeGenInterface::ins_StoreFromSrc(regNumber srcReg, var_types dstType, bool aligned /*=false*/)
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

void CodeGen::instGen_Return(unsigned stkArgSize)
{
#if defined(TARGET_XARCH)
    if (stkArgSize == 0)
    {
        instGen(INS_ret);
    }
    else
    {
        inst_IV(INS_ret, stkArgSize);
    }
#elif defined(TARGET_ARM)
// The return on ARM is folded into the pop multiple instruction
// and as we do not know the exact set of registers that we will
// need to restore (pop) when we first call instGen_Return we will
// instead just not emit anything for this method on the ARM
// The return will be part of the pop multiple and that will be
// part of the epilog that is generated by genFnEpilog()
#elif defined(TARGET_ARM64)
    // This function shouldn't be used on ARM64.
    unreached();
#else
#error "Unknown TARGET"
#endif
}

void CodeGen::instGen_MemoryBarrier(BarrierKind barrierKind)
{
#ifdef DEBUG
    if (JitConfig.JitNoMemoryBarriers() == 1)
    {
        return;
    }
#endif

#if defined(TARGET_XARCH)
    // Only full barrier needs to be emitted on Xarch
    if (barrierKind == BARRIER_FULL)
    {
        instGen(INS_lock);
        GetEmitter()->emitIns_AR_I(INS_or, EA_4BYTE, REG_SPBASE, 0, 0);
    }
#elif defined(TARGET_ARM)
    // ARM has only full barriers, so all barriers need to be emitted as full.
    GetEmitter()->emitIns_I(INS_dmb, EA_4BYTE, 0xf);
#elif defined(TARGET_ARM64)
    GetEmitter()->emitIns_BARR(INS_dmb, barrierKind == BARRIER_LOAD_ONLY ? INS_BARRIER_ISHLD : INS_BARRIER_ISH);
#else
#error "Unknown TARGET"
#endif
}

void CodeGen::instGen_Set_Reg_To_Zero(emitAttr size, regNumber reg, insFlags flags)
{
#if defined(TARGET_XARCH)
    GetEmitter()->emitIns_R_R(INS_xor, size, reg, reg);
#elif defined(TARGET_ARMARCH)
    GetEmitter()->emitIns_R_I(INS_mov, size, reg, 0 ARM_ARG(flags));
#else
#error "Unknown TARGET"
#endif
    regSet.verifyRegUsed(reg);
}
