// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_ARMARCH

#include "codegen.h"
#include "lower.h"
#include "emit.h"

void CodeGen::GenNode(GenTree* treeNode, BasicBlock* block)
{
    switch (treeNode->GetOper())
    {
        case GT_START_NONGC:
            GetEmitter()->DisableGC();
            break;

        case GT_START_PREEMPTGC:
            // Kill callee saves GC registers, and create a label
            // so that information gets propagated to the emitter.
            liveness.RemoveGCRegs(RBM_INT_CALLEE_SAVED);
            GetEmitter()->DefineTempLabel();
            break;

        case GT_PROF_HOOK:
            // We should be seeing this only if profiler hook is needed
            noway_assert(compiler->compIsProfilerHookNeeded());

#ifdef PROFILING_SUPPORTED
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
            GenIntCon(treeNode->AsIntCon());
            break;

        case GT_CNS_DBL:
            GenDblCon(treeNode->AsDblCon());
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

#ifdef TARGET_ARM64
        case GT_BSWAP:
        case GT_BSWAP16:
            genCodeForBswap(treeNode);
            break;

        case GT_DIV:
        case GT_UDIV:
            GenDivMod(treeNode->AsOp());
            break;
#endif

        case GT_OR:
        case GT_XOR:
        case GT_AND:
        case GT_ADD:
        case GT_SUB:
        case GT_MUL:
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
        case GT_ROR:
            genCodeForShift(treeNode->AsOp());
            break;

#ifndef TARGET_64BIT
        case GT_LSH_HI:
        case GT_RSH_LO:
            genCodeForShiftLong(treeNode);
            break;
#endif

        case GT_CAST:
            GenCast(treeNode->AsCast());
            break;

        case GT_BITCAST:
            genCodeForBitCast(treeNode->AsOp());
            break;

        case GT_LCL_ADDR:
            GenLclAddr(treeNode->AsLclAddr());
            break;

        case GT_LCL_FLD:
            GenLoadLclFld(treeNode->AsLclFld());
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

#ifdef TARGET_ARM
        case GT_MUL_LONG:
            genCodeForMulLong(treeNode->AsOp());
            break;
#endif

#ifdef TARGET_ARM64
        case GT_INC_SATURATE:
            genCodeForIncSaturate(treeNode);
            break;

        case GT_MULHI:
            GenMulLong(treeNode->AsOp());
            break;
#endif

        case GT_JMP:
            GenJmp(treeNode);
            break;

        case GT_CKFINITE:
            genCkfinite(treeNode);
            break;

        case GT_INTRINSIC:
            genIntrinsic(treeNode->AsIntrinsic());
            break;

#if FEATURE_PARTIAL_SIMD_CALLEE_SAVE
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
#endif

        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GE:
        case GT_GT:
        case GT_CMP:
#ifdef TARGET_ARM64
        case GT_TEST_EQ:
        case GT_TEST_NE:
#endif
            GenCompare(treeNode->AsOp());
            break;

        case GT_JTRUE:
            GenJTrue(treeNode->AsUnOp(), block);
            break;

#ifdef TARGET_ARM64
        case GT_JCMP:
            GenJCmp(treeNode->AsOp(), block);
            break;
#endif

        case GT_JCC:
            GenJCC(treeNode->AsCC(), block);
            break;

        case GT_SETCC:
            GenSetCC(treeNode->AsCC());
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
            assert(!"FIELD_LIST nodes should always be marked contained.");
            break;

        case GT_PUTARG_STK:
            genPutArgStk(treeNode->AsPutArgStk());
            break;

        case GT_PUTARG_REG:
            genPutArgReg(treeNode->AsUnOp());
            break;

#if FEATURE_ARG_SPLIT
        case GT_PUTARG_SPLIT:
            genPutArgSplit(treeNode->AsPutArgSplit());
            break;
#endif

        case GT_CALL:
            genCallInstruction(treeNode->AsCall());
            break;

        case GT_MEMORYBARRIER:
        {
            CodeGen::BarrierKind barrierKind =
                treeNode->gtFlags & GTF_MEMORYBARRIER_LOAD ? BARRIER_LOAD_ONLY : BARRIER_FULL;

            instGen_MemoryBarrier(barrierKind);
            break;
        }

#ifdef TARGET_ARM64
        case GT_XCHG:
        case GT_XORR:
        case GT_XAND:
        case GT_XADD:
            GenInterlocked(treeNode->AsOp());
            break;

        case GT_CMPXCHG:
            GenCmpXchg(treeNode->AsCmpXchg());
            break;
#endif

        case GT_NOP:
            break;

        case GT_KEEPALIVE:
            if (treeNode->AsUnOp()->GetOp(0)->isContained())
            {
                GenTree* src = treeNode->AsUnOp()->GetOp(0);

                // TODO-MIKE-Review: This can't be a LCL_FLD, it's marked as reg optional
                // in lowering and only a reg optional LCL_VAR can become contained.
                if (src->OperIs(GT_LCL_VAR, GT_LCL_FLD))
                {
                    liveness.UpdateLife(this, src->AsLclVarCommon());
                }
            }
            else
            {
                UseReg(treeNode->AsUnOp()->GetOp(0));
            }
            break;

        case GT_NO_OP:
            GetEmitter()->emitIns(INS_nop);
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

        case GT_PINVOKE_PROLOG:
            noway_assert((liveness.GetGCRegs() & ~fullIntArgRegMask()) == RBM_NONE);

#ifdef PSEUDORANDOM_NOP_INSERTION
            // the runtime side requires the codegen here to be consistent
            GetEmitter()->DisableRandomNops();
#endif
            break;

        case GT_LABEL:
            genPendingCallLabel = GetEmitter()->CreateTempLabel();
#ifdef TARGET_ARM
            genMov32RelocatableDisplacement(genPendingCallLabel, treeNode->GetRegNum());
#else
            GetEmitter()->emitIns_R_L(treeNode->GetRegNum(), genPendingCallLabel);
#endif
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
            GenJmpTable(treeNode, block->GetSwitchDesc());
            break;

        case GT_SWITCH_TABLE:
            GenSwitchTable(treeNode->AsOp());
            break;

        case GT_ARR_INDEX:
            genCodeForArrIndex(treeNode->AsArrIndex());
            break;

        case GT_ARR_OFFSET:
            genCodeForArrOffset(treeNode->AsArrOffs());
            break;

#ifdef TARGET_ARM64
        case GT_CONST_ADDR:
            GenConstAddr(treeNode->AsConstAddr());
            break;
#endif

        case GT_INSTR:
            genCodeForInstr(treeNode->AsInstr());
            break;

        default:
        {
#ifdef DEBUG
            char message[256];
            _snprintf_s(message, _countof(message), _TRUNCATE, "NYI: Unimplemented node type %s",
                        GenTree::OpName(treeNode->OperGet()));
            NYIRAW(message);
#else
            NYI("unimplemented node");
#endif
        }
        break;
    }
}

void CodeGen::PrologSetGSSecurityCookie(regNumber initReg, bool* initRegZeroed)
{
    assert(compiler->getNeedsGSSecurityCookie());

    StackAddrMode s = GetStackAddrMode(compiler->lvaGSSecurityCookie, 0);

    if (m_gsCookieAddr == nullptr)
    {
        noway_assert(m_gsCookieVal != 0);

        instGen_Set_Reg_To_Imm(EA_PTRSIZE, initReg, m_gsCookieVal);
        GetEmitter()->Ins_R_S(INS_str, EA_PTRSIZE, initReg, s);
    }
    else
    {
        instGen_Set_Reg_To_Addr(initReg, m_gsCookieAddr DEBUGARG(reinterpret_cast<void*>(THT_SetGSCookie)));
        GetEmitter()->emitIns_R_R_I(INS_ldr, EA_PTRSIZE, initReg, initReg, 0);
        GetEmitter()->Ins_R_S(INS_str, EA_PTRSIZE, initReg, s);
    }

    *initRegZeroed = false;
}

void CodeGen::EpilogGSCookieCheck()
{
    // We need two temporary registers, to load the GS cookie values and compare them. We can't use
    // any argument registers if 'pushReg' is true (meaning we have a JMP call). They should be
    // callee-trash registers, which should not contain anything interesting at this point.
    // We don't have any IR node representing this check, so LSRA can't communicate registers
    // for us to use.

    StackAddrMode s          = GetStackAddrMode(compiler->lvaGSSecurityCookie, 0);
    regNumber     regGSConst = REG_GSCOOKIE_TMP_0;
    regNumber     regGSValue = REG_GSCOOKIE_TMP_1;
    Emitter&      emit       = *GetEmitter();

    if (m_gsCookieAddr == nullptr)
    {
        noway_assert(m_gsCookieVal != 0);

        instGen_Set_Reg_To_Imm(EA_PTRSIZE, regGSConst, m_gsCookieVal);
    }
    else
    {
        instGen_Set_Reg_To_Addr(regGSConst, m_gsCookieAddr DEBUGARG(reinterpret_cast<void*>(THT_GSCookieCheck)));
        emit.emitIns_R_R_I(INS_ldr, EA_PTRSIZE, regGSConst, regGSConst, 0);
    }

    emit.Ins_R_S(INS_ldr, EA_PTRSIZE, regGSValue, s);
    emit.emitIns_R_R(INS_cmp, EA_PTRSIZE, regGSConst, regGSValue);

    insGroup* gsCheckBlk = emit.CreateTempLabel();
    emit.emitIns_J(INS_beq, gsCheckBlk);
    genEmitHelperCall(CORINFO_HELP_FAIL_FAST);
    emit.DefineTempLabel(gsCheckBlk);
}

void CodeGen::genIntrinsic(GenTreeIntrinsic* node)
{
    assert(varTypeIsFloating(node->GetType()));

    GenTree* src = node->GetOp(0);
    assert(src->GetType() == node->GetType());

    regNumber srcReg = UseReg(src);
    regNumber dstReg = node->GetRegNum();

    instruction ins;

    switch (node->GetIntrinsic())
    {
        case NI_System_Math_Abs:
            ins = INS_ABS;
            break;
        case NI_System_Math_Sqrt:
            ins = INS_SQRT;
            break;
#ifdef TARGET_ARM64
        case NI_System_Math_Ceiling:
            ins = INS_frintp;
            break;
        case NI_System_Math_Floor:
            ins = INS_frintm;
            break;
        case NI_System_Math_Round:
            ins = INS_frintn;
            break;
#endif
        default:
            unreached();
    }

    GetEmitter()->emitIns_R_R(ins, emitTypeSize(node->GetType()), dstReg, srcReg);

    DefReg(node);
}

#ifdef TARGET_ARM64
unsigned CodeGen::GetFirstStackParamLclNum() const
{
#ifdef TARGET_WINDOWS
    // This can't deal with split params.
    assert(!compiler->info.compIsVarArgs);
#endif

    for (unsigned i = 0, paramCount = compiler->info.GetParamCount(); i < paramCount; i++)
    {
        LclVarDsc* lcl = compiler->lvaGetDesc(i);

        assert(lcl->IsParam());

        if (!lcl->IsRegParam())
        {
            return i;
        }
    }

    return BAD_VAR_NUM;
}
#endif // TARGET_ARM64

void CodeGen::genPutArgStk(GenTreePutArgStk* putArg)
{
    unsigned outArgLclNum;
    INDEBUG(unsigned outArgLclSize);

#if FEATURE_FASTTAILCALL
    if (putArg->PutInIncomingArgArea())
    {
        assert(putArg->GetCall()->IsFastTailCall());

        outArgLclNum = GetFirstStackParamLclNum();
        INDEBUG(outArgLclSize = paramsStackSize);

        noway_assert(outArgLclNum != BAD_VAR_NUM);
    }
    else
#endif
    {
        outArgLclNum = compiler->lvaOutgoingArgSpaceVar;
        INDEBUG(outArgLclSize = outgoingArgSpaceSize);
    }

    unsigned outArgLclOffs = putArg->GetSlotOffset();

    GenTree*  src     = putArg->GetOp(0);
    var_types srcType = varActualType(src->GetType());

    if (src->OperIs(GT_FIELD_LIST))
    {
        genPutArgStkFieldList(putArg, outArgLclNum, outArgLclOffs DEBUGARG(outArgLclSize));
        return;
    }

    if (srcType == TYP_STRUCT)
    {
        genPutStructArgStk(putArg, outArgLclNum, outArgLclOffs DEBUGARG(outArgLclSize));
        return;
    }

    if (src->IsIntegralConst(0) && (putArg->GetSlotCount() > 1))
    {
#ifdef TARGET_ARM64
        assert(putArg->GetArgSize() == 16);
        assert(src->isContained());

        GetEmitter()->emitIns_S_S_R_R(INS_stp, EA_8BYTE, EA_8BYTE, REG_ZR, REG_ZR,
                                      GetStackAddrMode(outArgLclNum, static_cast<int>(outArgLclOffs)));
#else
        regNumber srcReg = src->GetRegNum();

        for (unsigned offset = 0; offset < putArg->GetArgSize(); offset += REGSIZE_BYTES)
        {
            GetEmitter()->Ins_R_S(INS_str, EA_PTRSIZE, srcReg,
                                  GetStackAddrMode(outArgLclNum, static_cast<int>(outArgLclOffs + offset)));
        }
#endif

        return;
    }

    // We can't write beyound the outgoing area area
    assert(outArgLclOffs + varTypeSize(srcType) <= outArgLclSize);

    instruction storeIns  = ins_Store(srcType);
    emitAttr    storeAttr = emitTypeSize(srcType);
    regNumber   srcReg;

#ifdef TARGET_ARM64
    if (src->isContained())
    {
        assert(src->IsIntegralConst(0) || src->IsDblConPositiveZero());
        assert(storeIns == INS_str);
        srcReg = REG_ZR;
    }
    else
#endif // TARGET_ARM64
    {
        srcReg = genConsumeReg(src);
    }

    GetEmitter()->Ins_R_S(storeIns, storeAttr, srcReg, GetStackAddrMode(outArgLclNum, outArgLclOffs));

#ifdef TARGET_ARM
    if (srcType == TYP_LONG)
    {
        // This case currently only occurs for double types that are passed as TYP_LONG;
        // actual long types would have been decomposed by now.
        regNumber otherReg = src->GetRegNum(1);
        GetEmitter()->Ins_R_S(storeIns, storeAttr, otherReg, GetStackAddrMode(outArgLclNum, outArgLclOffs + 4));
    }
#endif // TARGET_ARM
}

void CodeGen::genPutStructArgStk(GenTreePutArgStk* putArgStk,
                                 unsigned          outArgLclNum,
                                 unsigned outArgLclOffs DEBUGARG(unsigned outArgLclSize))
{
    GenTree* src = putArgStk->GetOp(0);

    assert(src->TypeIs(TYP_STRUCT));
    assert(src->isContained());

    ClassLayout* srcLayout;
    LclVarDsc*   srcLcl         = nullptr;
    regNumber    srcAddrBaseReg = REG_NA;
    int          srcOffset      = 0;

    if (src->OperIs(GT_LCL_VAR))
    {
        srcLcl    = src->AsLclVar()->GetLcl();
        srcLayout = srcLcl->GetLayout();
    }
    else if (src->OperIs(GT_LCL_FLD))
    {
        srcLcl    = src->AsLclFld()->GetLcl();
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
            srcAddrBaseReg = genConsumeReg(srcAddr->AsAddrMode()->GetBase());
            assert(!srcAddr->AsAddrMode()->HasIndex());
            srcOffset = srcAddr->AsAddrMode()->GetOffset();
        }

        srcLayout = src->AsObj()->GetLayout();
    }

    emitter* emit   = GetEmitter();
    unsigned offset = 0;
    unsigned size   = srcLayout->GetSize();

    if (srcLcl != nullptr)
    {
        size = roundUp(size, REGSIZE_BYTES);
    }

    regNumber tempReg = putArgStk->ExtractTempReg();
    assert(tempReg != srcAddrBaseReg);

#ifdef TARGET_ARM64
    regNumber tempReg2 = putArgStk->GetSingleTempReg();
    assert(tempReg2 != srcAddrBaseReg);

    for (unsigned regSize = 2 * REGSIZE_BYTES; size >= regSize; size -= regSize, offset += regSize)
    {
        emitAttr attr  = emitTypeSize(srcLayout->GetGCPtrType(offset / REGSIZE_BYTES + 0));
        emitAttr attr2 = emitTypeSize(srcLayout->GetGCPtrType(offset / REGSIZE_BYTES + 1));

        if (srcLcl != nullptr)
        {
            emit->emitIns_R_R_S_S(INS_ldp, attr, attr2, tempReg, tempReg2,
                                  GetStackAddrMode(srcLcl, srcOffset + offset));
        }
        else
        {
            emit->emitIns_R_R_R_I(INS_ldp, attr, tempReg, tempReg2, srcAddrBaseReg, srcOffset + offset, INS_OPTS_NONE,
                                  attr2);
        }

        // We can't write beyound the outgoing area area
        assert(outArgLclOffs + offset + 16 <= outArgLclSize);

        emit->emitIns_S_S_R_R(INS_stp, attr, attr2, tempReg, tempReg2,
                              GetStackAddrMode(outArgLclNum, outArgLclOffs + offset));
    }
#endif // TARGET_ARM64

    for (unsigned regSize = REGSIZE_BYTES; size != 0; size -= regSize, offset += regSize)
    {
        while (regSize > size)
        {
            regSize /= 2;
        }

        instruction loadIns;
        instruction storeIns;
        emitAttr    attr;

        switch (regSize)
        {
            case 1:
                loadIns  = INS_ldrb;
                storeIns = INS_strb;
                attr     = EA_4BYTE;
                break;
            case 2:
                loadIns  = INS_ldrh;
                storeIns = INS_strh;
                attr     = EA_4BYTE;
                break;
#ifdef TARGET_ARM64
            case 4:
                loadIns  = INS_ldr;
                storeIns = INS_str;
                attr     = EA_4BYTE;
                break;
#endif // TARGET_ARM64
            default:
                assert(regSize == REGSIZE_BYTES);
                loadIns  = INS_ldr;
                storeIns = INS_str;
                attr     = emitTypeSize(srcLayout->GetGCPtrType(offset / REGSIZE_BYTES));
        }

        if (srcLcl != nullptr)
        {
            emit->Ins_R_S(loadIns, attr, tempReg, GetStackAddrMode(srcLcl, srcOffset + offset));
        }
        else
        {
            emit->emitIns_R_R_I(loadIns, attr, tempReg, srcAddrBaseReg, srcOffset + offset);
        }

        // We can't write beyound the outgoing area area
        assert(outArgLclOffs + offset + regSize <= outArgLclSize);

        emit->Ins_R_S(storeIns, attr, tempReg, GetStackAddrMode(outArgLclNum, outArgLclOffs + offset));
    }
}

void CodeGen::genPutArgReg(GenTreeUnOp* arg)
{
    assert(arg->OperIs(GT_PUTARG_REG));

    GenTree* src = arg->GetOp(0);

#ifdef TARGET_ARM
    if (src->TypeIs(TYP_LONG))
    {
        assert(src->OperIs(GT_BITCAST));
        assert(arg->TypeIs(TYP_LONG));

        UseRegs(src);
        GetEmitter()->emitIns_Mov(INS_mov, EA_4BYTE, arg->GetRegNum(0), src->GetRegNum(0), /* canSkip */ true);
        GetEmitter()->emitIns_Mov(INS_mov, EA_4BYTE, arg->GetRegNum(1), src->GetRegNum(1), /* canSkip */ true);

        DefLongRegs(arg);

        return;
    }
#endif

    regNumber srcReg = UseReg(src);
    regNumber argReg = arg->GetRegNum();
    var_types type   = arg->GetType();

    assert(!varTypeIsSmall(type));

    GetEmitter()->emitIns_Mov(ins_Copy(type), emitTypeSize(type), argReg, srcReg, /* canSkip */ true);

    DefReg(arg);
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

    if (src->isContained())
    {
        assert(IsValidContainedLcl(src->AsLclVar()));
        liveness.UpdateLife(this, src->AsLclVar());
        StackAddrMode s      = GetStackAddrMode(src->AsLclVar()->GetLcl(), 0);
        regNumber     dstReg = bitcast->GetRegNum();

        GetEmitter()->Ins_R_S(ins_Load(dstType), emitTypeSize(dstType), dstReg, s);
        DefReg(bitcast);
    }
#ifdef TARGET_ARM
    else if (varTypeIsLong(dstType) && src->TypeIs(TYP_DOUBLE))
    {
        regNumber srcReg  = UseReg(src);
        regNumber dstReg1 = bitcast->GetRegNum(0);
        regNumber dstReg2 = bitcast->GetRegNum(1);
        GetEmitter()->emitIns_R_R_R(INS_vmov_d2i, EA_8BYTE, dstReg1, dstReg2, srcReg);
        DefLongRegs(bitcast);
    }
    else if ((genTypeSize(dstType) > REGSIZE_BYTES) || (genTypeSize(src->GetType()) > REGSIZE_BYTES))
    {
        NYI_ARM("Converting to/from long/SIMD");
    }
#endif
    else
    {
        regNumber srcReg = UseReg(src);
        regNumber dstReg = bitcast->GetRegNum();
        inst_BitCast(dstType, dstReg, src->GetType(), srcReg);
        DefReg(bitcast);
    }
}

#if FEATURE_ARG_SPLIT

void CodeGen::genPutArgSplit(GenTreePutArgSplit* putArg)
{
    const unsigned outArgLclNum  = compiler->lvaOutgoingArgSpaceVar;
    const unsigned outArgLclSize = outgoingArgSpaceSize;
    const unsigned outArgLclOffs = putArg->GetSlotOffset();

    GenTree* src = putArg->GetOp(0);

    if (src->IsIntegralConst(0))
    {
        regNumber srcReg = src->GetRegNum();

        unsigned dstOffset = outArgLclOffs;
        unsigned stackSize = putArg->GetArgSize() - putArg->GetRegCount() * REGSIZE_BYTES;

        for (; stackSize != 0; stackSize -= REGSIZE_BYTES, dstOffset += REGSIZE_BYTES)
        {
            // We can't write beyound the outgoing area area
            assert(dstOffset + REGSIZE_BYTES <= outArgLclSize);

            GetEmitter()->Ins_R_S(INS_str, EA_PTRSIZE, srcReg, GetStackAddrMode(outArgLclNum, dstOffset));
        }

        for (unsigned i = 0; i < putArg->GetRegCount(); i++)
        {
            GetEmitter()->emitIns_R_I(INS_mov, EA_PTRSIZE, putArg->GetRegNum(i), 0);
        }

        DefPutArgSplitRegs(putArg);

        return;
    }

    assert(src->TypeIs(TYP_STRUCT));
    assert(src->isContained());

    if (GenTreeFieldList* fieldList = src->IsFieldList())
    {
        unsigned regSize  = putArg->GetRegCount() * REGSIZE_BYTES;
        unsigned regIndex = 0;
        for (GenTreeFieldList::Use& use : fieldList->Uses())
        {
            GenTree* fieldNode = use.GetNode();

            if (regIndex >= putArg->GetRegCount())
            {
                regNumber fieldReg = UseReg(fieldNode);
                var_types type     = fieldNode->GetType();
                emitAttr  attr     = emitTypeSize(type);

                unsigned dstOffset = outArgLclOffs + use.GetOffset() - regSize;
                assert(dstOffset + EA_SIZE_IN_BYTES(attr) <= outArgLclSize);
                GetEmitter()->Ins_R_S(ins_Store(type), attr, fieldReg, GetStackAddrMode(outArgLclNum, dstOffset));

                continue;
            }

#ifdef TARGET_ARM
            if (fieldNode->TypeIs(TYP_LONG))
            {
                assert(fieldNode->OperIs(GT_BITCAST));

                UseRegs(fieldNode);

                regNumber fieldReg0 = fieldNode->GetRegNum(0);
                regNumber fieldReg1 = fieldNode->GetRegNum(1);
                regNumber argReg0   = putArg->GetRegNum(regIndex++);
                regNumber argReg1   = putArg->GetRegNum(regIndex++);

                GetEmitter()->emitIns_Mov(INS_mov, EA_4BYTE, argReg0, fieldReg0, /* canSkip */ true);
                GetEmitter()->emitIns_Mov(INS_mov, EA_4BYTE, argReg1, fieldReg1, /* canSkip */ true);

                continue;
            }
#endif

            regNumber fieldReg = UseReg(fieldNode);
            regNumber argReg   = putArg->GetRegNum(regIndex);
            emitAttr  attr     = emitTypeSize(putArg->GetRegType(regIndex++));
            assert(EA_SIZE_IN_BYTES(attr) == REGSIZE_BYTES);
            GetEmitter()->emitIns_Mov(INS_mov, attr, argReg, fieldReg, /* canSkip*/ true);
        }

        DefPutArgSplitRegs(putArg);

        return;
    }

    ClassLayout* srcLayout;
    LclVarDsc*   srcLcl         = nullptr;
    regNumber    srcAddrBaseReg = REG_NA;
    int          srcOffset      = 0;

    if (src->OperIs(GT_LCL_VAR))
    {
        srcLcl    = src->AsLclVar()->GetLcl();
        srcLayout = srcLcl->GetLayout();
    }
    else if (src->OperIs(GT_LCL_FLD))
    {
        srcLcl    = src->AsLclFld()->GetLcl();
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
            srcAddrBaseReg = genConsumeReg(srcAddr->AsAddrMode()->GetBase());
            assert(!srcAddr->AsAddrMode()->HasIndex());
            srcOffset = srcAddr->AsAddrMode()->GetOffset();
        }

        srcLayout = src->AsObj()->GetLayout();
    }

    unsigned offset    = 0;
    unsigned dstOffset = outArgLclOffs;
    unsigned size      = srcLayout->GetSize();

    if (srcLcl != nullptr)
    {
        size = roundUp(size, REGSIZE_BYTES);
    }

    // Skip the part that will be loaded in registers.
    offset += putArg->GetRegCount() * REGSIZE_BYTES;
    size -= putArg->GetRegCount() * REGSIZE_BYTES;

    regNumber tempReg = putArg->ExtractTempReg();
    assert(tempReg != srcAddrBaseReg);

    for (unsigned regSize = REGSIZE_BYTES; size != 0; size -= regSize, offset += regSize, dstOffset += regSize)
    {
        while (regSize > size)
        {
            regSize /= 2;
        }

        instruction loadIns;
        instruction storeIns;
        emitAttr    attr;

        switch (regSize)
        {
            case 1:
                loadIns  = INS_ldrb;
                storeIns = INS_strb;
                attr     = EA_4BYTE;
                break;
            case 2:
                loadIns  = INS_ldrh;
                storeIns = INS_strh;
                attr     = EA_4BYTE;
                break;
#ifdef TARGET_ARM64
            case 4:
                loadIns  = INS_ldr;
                storeIns = INS_str;
                attr     = EA_4BYTE;
                break;
#endif // TARGET_ARM64
            default:
                assert(regSize == REGSIZE_BYTES);
                loadIns  = INS_ldr;
                storeIns = INS_str;
                attr     = emitTypeSize(srcLayout->GetGCPtrType(offset / REGSIZE_BYTES));
        }

        if (srcLcl != nullptr)
        {
            GetEmitter()->Ins_R_S(loadIns, attr, tempReg, GetStackAddrMode(srcLcl, srcOffset + offset));
        }
        else
        {
            GetEmitter()->emitIns_R_R_I(loadIns, attr, tempReg, srcAddrBaseReg, srcOffset + offset);
        }

        // We can't write beyound the outgoing area area
        assert(dstOffset + regSize <= outArgLclSize);

        GetEmitter()->Ins_R_S(storeIns, attr, tempReg, GetStackAddrMode(outArgLclNum, dstOffset));
    }

    for (unsigned i = 0; i < putArg->GetRegCount(); i++)
    {
        unsigned  offset   = srcOffset + i * REGSIZE_BYTES;
        regNumber dstReg   = putArg->GetRegNum(i);
        emitAttr  slotAttr = emitTypeSize(putArg->GetRegType(i));

        if (srcLcl != nullptr)
        {
            GetEmitter()->Ins_R_S(INS_ldr, slotAttr, dstReg, GetStackAddrMode(srcLcl, offset));
        }
        else
        {
            // If the source address register is the same as one of the destination registers then
            // copy the address to the temp register (which is always allocated and different from
            // all destination registers) and continue using the temp register as source address.

            if ((dstReg == srcAddrBaseReg) && (i != putArg->GetRegCount() - 1))
            {
                assert(dstReg != tempReg);

                emitAttr srcAddrAttr =
                    src->OperIs(GT_OBJ) ? emitTypeSize(src->AsObj()->GetAddr()->GetType()) : EA_PTRSIZE;
                GetEmitter()->emitIns_Mov(INS_mov, srcAddrAttr, tempReg, srcAddrBaseReg, /* canSkip */ false);
                srcAddrBaseReg = tempReg;
            }

            GetEmitter()->emitIns_R_R_I(INS_ldr, slotAttr, dstReg, srcAddrBaseReg, offset);
        }
    }

    DefPutArgSplitRegs(putArg);
}
#endif // FEATURE_ARG_SPLIT

void CodeGen::genRangeCheck(GenTreeBoundsChk* node)
{
    GenTree*  index  = node->GetIndex();
    GenTree*  length = node->GetLength();
    var_types type   = varActualType(index->GetType());

    assert(type == varActualType(length->GetType()));
#ifdef TARGET_64BIT
    assert((type == TYP_INT) || (type == TYP_LONG));
#else
    assert(type == TYP_INT);
#endif

    emitAttr     attr    = emitTypeSize(type);
    emitJumpKind jmpKind = EJ_hs;

    if (index->isUsedFromReg() && length->isUsedFromReg())
    {
        regNumber indexReg  = UseReg(index);
        regNumber lengthReg = UseReg(length);

        GetEmitter()->emitIns_R_R(INS_cmp, attr, indexReg, lengthReg);
    }
    else
    {
        GenTree*       regOp = index;
        GenTreeIntCon* immOp = length->IsContainedIntCon();

        if (immOp == nullptr)
        {
            regOp   = length;
            immOp   = index->AsIntCon();
            jmpKind = EJ_ls;
        }

#ifdef TARGET_64BIT
        GetEmitter()->emitIns_R_I(INS_cmp, attr, UseReg(regOp), immOp->GetValue());
#else
        GetEmitter()->emitIns_R_I(INS_cmp, attr, UseReg(regOp), immOp->GetInt32Value());
#endif
    }

    genJumpToThrowHlpBlk(jmpKind, node->GetThrowKind(), node->GetThrowBlock());
}

void CodeGen::genCodeForPhysReg(GenTreePhysReg* tree)
{
    assert(tree->OperIs(GT_PHYSREG));

    var_types targetType = tree->TypeGet();
    regNumber targetReg  = tree->GetRegNum();

    inst_Mov(targetType, targetReg, tree->gtSrcReg, /* canSkip */ true);
    liveness.TransferGCRegType(targetReg, tree->gtSrcReg);

    genProduceReg(tree);
}

void CodeGen::genCodeForArrIndex(GenTreeArrIndex* arrIndex)
{
    emitter*  emit      = GetEmitter();
    GenTree*  arrObj    = arrIndex->ArrObj();
    GenTree*  indexNode = arrIndex->IndexExpr();
    regNumber arrReg    = genConsumeReg(arrObj);
    regNumber indexReg  = genConsumeReg(indexNode);
    regNumber tgtReg    = arrIndex->GetRegNum();
    noway_assert(tgtReg != REG_NA);

    // We will use a temp register to load the lower bound and dimension attr values.

    regNumber tmpReg = arrIndex->GetSingleTempReg();
    assert(tgtReg != tmpReg);

    unsigned  dim      = arrIndex->gtCurrDim;
    unsigned  rank     = arrIndex->gtArrRank;
    var_types elemType = arrIndex->gtArrElemType;
    unsigned  offset;

    offset = genOffsetOfMDArrayLowerBound(elemType, rank, dim);
    emit->emitIns_R_R_I(INS_ldr, EA_4BYTE, tmpReg, arrReg, offset);
    emit->emitIns_R_R_R(INS_sub, EA_4BYTE, tgtReg, indexReg, tmpReg);

    offset = genOffsetOfMDArrayDimensionSize(elemType, rank, dim);
    emit->emitIns_R_R_I(INS_ldr, EA_4BYTE, tmpReg, arrReg, offset);
    emit->emitIns_R_R(INS_cmp, EA_4BYTE, tgtReg, tmpReg);

    genJumpToThrowHlpBlk(EJ_hs, ThrowHelperKind::IndexOutOfRange);

    genProduceReg(arrIndex);
}

void CodeGen::genCodeForArrOffset(GenTreeArrOffs* arrOffset)
{
    GenTree*  offsetNode = arrOffset->GetOffset();
    GenTree*  indexNode  = arrOffset->GetIndex();
    regNumber tgtReg     = arrOffset->GetRegNum();

    noway_assert(tgtReg != REG_NA);

    if (!offsetNode->IsIntegralConst(0))
    {
        emitter*  emit      = GetEmitter();
        regNumber offsetReg = genConsumeReg(offsetNode);
        regNumber indexReg  = genConsumeReg(indexNode);
        regNumber arrReg    = genConsumeReg(arrOffset->GetArray());
        noway_assert(offsetReg != REG_NA);
        noway_assert(indexReg != REG_NA);
        noway_assert(arrReg != REG_NA);

        regNumber tmpReg = arrOffset->GetSingleTempReg();

        unsigned  dim      = arrOffset->gtCurrDim;
        unsigned  rank     = arrOffset->gtArrRank;
        var_types elemType = arrOffset->gtArrElemType;
        unsigned  offset   = genOffsetOfMDArrayDimensionSize(elemType, rank, dim);

        // Load tmpReg with the dimension attr and evaluate
        // tgtReg = offsetReg*tmpReg + indexReg.
        emit->emitIns_R_R_I(INS_ldr, EA_4BYTE, tmpReg, arrReg, offset);
        emit->emitIns_R_R_R_R(INS_MULADD, EA_PTRSIZE, tgtReg, tmpReg, offsetReg, indexReg);
    }
    else
    {
        regNumber indexReg = genConsumeReg(indexNode);
        inst_Mov(TYP_INT, tgtReg, indexReg, /* canSkip */ true);
    }
    genProduceReg(arrOffset);
}

void CodeGen::genCodeForShift(GenTreeOp* tree)
{
    var_types   targetType = tree->GetType();
    genTreeOps  oper       = tree->GetOper();
    instruction ins        = genGetInsForOper(oper);
    emitAttr    size       = emitActualTypeSize(targetType);

    GenTree* value   = tree->GetOp(0);
    GenTree* shiftBy = tree->GetOp(1);

    regNumber valueReg   = UseReg(value);
    regNumber shiftByReg = shiftBy->isUsedFromReg() ? UseReg(shiftBy) : REG_NA;
    regNumber dstReg     = tree->GetRegNum();

    if (shiftBy->isUsedFromReg())
    {
        GetEmitter()->emitIns_R_R_R(ins, size, dstReg, valueReg, shiftByReg);
    }
    else
    {
#ifdef TARGET_ARM
        unsigned immWidth = 32;
#else
        unsigned immWidth = EA_BIT_SIZE(size);
#endif
        unsigned shiftByImm = shiftBy->AsIntCon()->GetUInt32Value() & (immWidth - 1);

        GetEmitter()->emitIns_R_R_I(ins, size, dstReg, valueReg, shiftByImm);
    }

    DefReg(tree);
}

void CodeGen::genCodeForIndexAddr(GenTreeIndexAddr* node)
{
    GenTree* const base  = node->GetArray();
    GenTree* const index = node->GetIndex();

    regNumber baseReg  = UseReg(base);
    regNumber indexReg = UseReg(index);

    // TODO-MIKE-Review: This is dubious, GC liveness doesn't really matter until we reach a call...

    // NOTE: UseReg marks the register as not a GC pointer, as it assumes that the input registers
    // die at the first instruction generated by the node. This is not the case for `INDEX_ADDR`, however, as the
    // base register is multiply-used. As such, we need to mark the base register as containing a GC pointer until
    // we are finished generating the code for this node.

    liveness.SetGCRegType(baseReg, base->GetType());
    assert(varTypeIsIntegral(index->GetType()));

    const regNumber tmpReg = node->GetSingleTempReg();

    // Generate the bounds check if necessary.
    if ((node->gtFlags & GTF_INX_RNGCHK) != 0)
    {
        GetEmitter()->emitIns_R_R_I(INS_ldr, EA_4BYTE, tmpReg, baseReg, node->GetLenOffs());
        GetEmitter()->emitIns_R_R(INS_cmp, emitActualTypeSize(index->TypeGet()), indexReg, tmpReg);
        genJumpToThrowHlpBlk(EJ_hs, ThrowHelperKind::IndexOutOfRange, node->GetThrowBlock());
    }

    emitAttr attr = emitActualTypeSize(node->GetType());

    // Can we use a ScaledAdd instruction?
    //
    if (isPow2(node->GetElemSize()) && (node->GetElemSize() <= 32768))
    {
        DWORD scale;
        BitScanForward(&scale, node->GetElemSize());

        // dest = base + index * scale
        genScaledAdd(attr, node->GetRegNum(), baseReg, indexReg, scale);
    }
    else // we have to load the element attr and use a MADD (multiply-add) instruction
    {
        // tmpReg = element attr
        instGen_Set_Reg_To_Imm(EA_4BYTE, tmpReg, static_cast<ssize_t>(node->GetElemSize()));

        // dest = index * tmpReg + base
        GetEmitter()->emitIns_R_R_R_R(INS_MULADD, attr, node->GetRegNum(), indexReg, tmpReg, baseReg);
    }

    // dest = dest + elemOffs
    GetEmitter()->emitIns_R_R_I(INS_add, attr, node->GetRegNum(), node->GetRegNum(), node->GetDataOffs());

    // TODO-MIKE-Review: Hrm, what if baseReg is a local variable reg?!
    liveness.RemoveGCRegs(genRegMask(baseReg));

    DefReg(node);
}

void CodeGen::instGen_MemoryBarrier(BarrierKind barrierKind)
{
#ifdef DEBUG
    if (JitConfig.JitNoMemoryBarriers() == 1)
    {
        return;
    }
#endif

#ifdef TARGET_ARM
    // ARM has only full barriers, so all barriers need to be emitted as full.
    GetEmitter()->emitIns_I(INS_dmb, EA_4BYTE, 0xf);
#else
    GetEmitter()->emitIns_BARR(INS_dmb, barrierKind == BARRIER_LOAD_ONLY ? INS_BARRIER_ISHLD : INS_BARRIER_ISH);
#endif
}

void CodeGen::GenDynBlk(GenTreeDynBlk* store)
{
    ConsumeDynBlk(store, REG_ARG_0, REG_ARG_1, REG_ARG_2);

    if (store->IsVolatile())
    {
        instGen_MemoryBarrier();
    }

    genEmitHelperCall(store->OperIs(GT_COPY_BLK) ? CORINFO_HELP_MEMCPY : CORINFO_HELP_MEMSET);

    if (store->IsVolatile() && store->OperIs(GT_COPY_BLK))
    {
        instGen_MemoryBarrier(BARRIER_LOAD_ONLY);
    }
}

StructStoreKind GetStructStoreKind(bool isLocalStore, ClassLayout* layout, GenTree* src)
{
    assert(!layout->IsBlockLayout());

    if (src->IsCall())
    {
        return StructStoreKind::UnrollRegs;
    }

    unsigned size = layout->GetSize();

    if (src->OperIs(GT_CNS_INT))
    {
        assert(src->IsIntegralConst(0));

        return size > INITBLK_UNROLL_LIMIT ? StructStoreKind::LargeInit : StructStoreKind::UnrollInit;
    }

    // If the struct contains GC pointers we need to generate GC write barriers, unless
    // the destination is a local variable. Even if the destination is a local we're still
    // going to use UnrollWB if the attr is too large for normal unrolling.

    if (layout->HasGCPtr() && (!isLocalStore || (size > CPBLK_UNROLL_LIMIT)))
    {
        return StructStoreKind::UnrollCopyWB;
    }

    return size > CPBLK_UNROLL_LIMIT ? StructStoreKind::LargeCopy : StructStoreKind::UnrollCopy;
}

void CodeGen::GenStructStore(GenTree* store, StructStoreKind kind, ClassLayout* layout)
{
    assert(store->OperIs(GT_STORE_OBJ, GT_STORE_BLK, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    switch (kind)
    {
        case StructStoreKind::MemSet:
            GenStructStoreMemSet(store, layout);
            break;
        case StructStoreKind::MemCpy:
            GenStructStoreMemCpy(store, layout);
            break;
        case StructStoreKind::UnrollInit:
            GenStructStoreUnrollInit(store, layout);
            break;
        case StructStoreKind::UnrollCopy:
            GenStructStoreUnrollCopy(store, layout);
            break;
        case StructStoreKind::UnrollCopyWB:
            GenStructStoreUnrollCopyWB(store, layout);
            break;
        case StructStoreKind::UnrollRegs:
            GenStructStoreUnrollRegs(store, layout);
            break;
#ifdef TARGET_ARM64
        case StructStoreKind::UnrollRegsWB:
            GenStructStoreUnrollRegsWB(store->AsObj());
            break;
#endif
        default:
            unreached();
    }
}

void CodeGen::GenStructStoreMemSet(GenTree* store, ClassLayout* layout)
{
    ConsumeStructStore(store, layout, REG_ARG_0, REG_ARG_1, REG_ARG_2);

    if (store->IsIndir() && store->AsIndir()->IsVolatile())
    {
        instGen_MemoryBarrier();
    }

    genEmitHelperCall(CORINFO_HELP_MEMSET);
}

void CodeGen::GenStructStoreMemCpy(GenTree* store, ClassLayout* layout)
{
    assert(!layout->HasGCPtr());

    ConsumeStructStore(store, layout, REG_ARG_0, REG_ARG_1, REG_ARG_2);

    if (store->IsIndir() && store->AsIndir()->IsVolatile())
    {
        instGen_MemoryBarrier();
    }

    genEmitHelperCall(CORINFO_HELP_MEMCPY);

    if (store->IsIndir() && store->AsIndir()->IsVolatile())
    {
        instGen_MemoryBarrier(BARRIER_LOAD_ONLY);
    }
}

void CodeGen::GenStructStoreUnrollInit(GenTree* store, ClassLayout* layout)
{
    assert(store->OperIs(GT_STORE_BLK, GT_STORE_OBJ, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    LclVarDsc* dstLcl         = nullptr;
    regNumber  dstAddrBaseReg = REG_NA;
    int        dstOffset      = 0;
    GenTree*   src;

    if (store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
    {
        dstLcl    = store->AsLclVarCommon()->GetLcl();
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
            assert(!addrMode->HasIndex());

            dstAddrBaseReg = genConsumeReg(addrMode->GetBase());
            dstOffset      = dstAddr->AsAddrMode()->GetOffset();
        }
        else
        {
            dstLcl    = dstAddr->AsLclAddr()->GetLcl();
            dstOffset = dstAddr->AsLclAddr()->GetLclOffs();
        }

        src = store->AsIndir()->GetValue();
    }

    regNumber srcReg;

    if (src->OperIs(GT_INIT_VAL))
    {
        assert(src->isContained());
        src = src->AsUnOp()->GetOp(0);
    }

    if (!src->isContained())
    {
        srcReg = genConsumeReg(src);
    }
    else
    {
#ifdef TARGET_ARM64
        assert(src->IsIntegralConst(0));
        srcReg = REG_ZR;
#else
        unreached();
#endif
    }

    if (store->IsIndir() && store->AsIndir()->IsVolatile())
    {
        instGen_MemoryBarrier();
    }

    emitter* emit = GetEmitter();
    unsigned size = layout->GetSize();

    assert(size <= INT32_MAX);
    assert(dstOffset < INT32_MAX - static_cast<int>(size));

#ifdef TARGET_ARM64
    for (unsigned regSize = 2 * REGSIZE_BYTES; size >= regSize; size -= regSize, dstOffset += regSize)
    {
        if (dstLcl != nullptr)
        {
            emit->emitIns_S_S_R_R(INS_stp, EA_8BYTE, EA_8BYTE, srcReg, srcReg, GetStackAddrMode(dstLcl, dstOffset));
        }
        else
        {
            emit->emitIns_R_R_R_I(INS_stp, EA_8BYTE, srcReg, srcReg, dstAddrBaseReg, dstOffset);
        }
    }
#endif

    for (unsigned regSize = REGSIZE_BYTES; size > 0; size -= regSize, dstOffset += regSize)
    {
        while (regSize > size)
        {
            regSize /= 2;
        }

        instruction storeIns;
        emitAttr    attr;

        switch (regSize)
        {
            case 1:
                storeIns = INS_strb;
                attr     = EA_4BYTE;
                break;
            case 2:
                storeIns = INS_strh;
                attr     = EA_4BYTE;
                break;
            case 4:
#ifdef TARGET_ARM64
            case 8:
#endif
                storeIns = INS_str;
                attr     = EA_ATTR(regSize);
                break;
            default:
                unreached();
        }

        if (dstLcl != nullptr)
        {
            emit->Ins_R_S(storeIns, attr, srcReg, GetStackAddrMode(dstLcl, dstOffset));
        }
        else
        {
            emit->emitIns_R_R_I(storeIns, attr, srcReg, dstAddrBaseReg, dstOffset);
        }
    }
}

void CodeGen::GenStructStoreUnrollCopy(GenTree* store, ClassLayout* layout)
{
    assert(store->OperIs(GT_STORE_BLK, GT_STORE_OBJ, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    if (layout->HasGCPtr())
    {
        GetEmitter()->DisableGC();
    }

    LclVarDsc* dstLcl         = nullptr;
    regNumber  dstAddrBaseReg = REG_NA;
    int        dstOffset      = 0;
    GenTree*   src;

    if (store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
    {
        dstLcl    = store->AsLclVarCommon()->GetLcl();
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
            assert(!addrMode->HasIndex());

            dstAddrBaseReg = genConsumeReg(addrMode->GetBase());
            dstOffset      = dstAddr->AsAddrMode()->GetOffset();
        }
        else
        {
            // TODO-ARM-CQ: If the local frame offset is too large to be encoded, the emitter automatically
            // loads the offset into a reserved register (see CodeGen::rsGetRsvdReg()). If we generate
            // multiple store instructions we'll also generate multiple offset loading instructions.
            // We could try to detect such cases, compute the base destination address in this reserved
            // and use it in all store instructions we generate. This would effectively undo the effect
            // of local address containment done by lowering.
            //
            // The same issue also occurs in source address case below and in genCodeForInitBlkUnroll.

            dstLcl    = dstAddr->AsLclAddr()->GetLcl();
            dstOffset = dstAddr->AsLclAddr()->GetLclOffs();
        }

        src = store->AsIndir()->GetValue();
    }

    LclVarDsc* srcLcl         = nullptr;
    regNumber  srcAddrBaseReg = REG_NA;
    int        srcOffset      = 0;

    assert(src->isContained());

    if (src->OperIs(GT_LCL_VAR, GT_LCL_FLD))
    {
        srcLcl    = src->AsLclVarCommon()->GetLcl();
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
            srcAddrBaseReg = genConsumeReg(addrMode->GetBase());
            srcOffset      = srcAddr->AsAddrMode()->GetOffset();
        }
        else
        {
            srcLcl    = srcAddr->AsLclAddr()->GetLcl();
            srcOffset = srcAddr->AsLclAddr()->GetLclOffs();
        }
    }

    if (store->IsIndir() && store->AsIndir()->IsVolatile())
    {
        instGen_MemoryBarrier();
    }

    emitter* emit = GetEmitter();
    unsigned size = layout->GetSize();

    assert(size <= INT32_MAX);
    assert(srcOffset < INT32_MAX - static_cast<int>(size));
    assert(dstOffset < INT32_MAX - static_cast<int>(size));

    regNumber tempReg = store->ExtractTempReg(RBM_ALLINT);

#ifdef TARGET_ARM64
    if (size >= 2 * REGSIZE_BYTES)
    {
        regNumber tempReg2 = store->ExtractTempReg(RBM_ALLINT);

        for (unsigned regSize = 2 * REGSIZE_BYTES; size >= regSize;
             size -= regSize, srcOffset += regSize, dstOffset += regSize)
        {
            if (srcLcl != nullptr)
            {
                emit->emitIns_R_R_S_S(INS_ldp, EA_8BYTE, EA_8BYTE, tempReg, tempReg2,
                                      GetStackAddrMode(srcLcl, srcOffset));
            }
            else
            {
                emit->emitIns_R_R_R_I(INS_ldp, EA_8BYTE, tempReg, tempReg2, srcAddrBaseReg, srcOffset);
            }

            if (dstLcl != nullptr)
            {
                emit->emitIns_S_S_R_R(INS_stp, EA_8BYTE, EA_8BYTE, tempReg, tempReg2,
                                      GetStackAddrMode(dstLcl, dstOffset));
            }
            else
            {
                emit->emitIns_R_R_R_I(INS_stp, EA_8BYTE, tempReg, tempReg2, dstAddrBaseReg, dstOffset);
            }
        }
    }
#endif

    for (unsigned regSize = REGSIZE_BYTES; size > 0; size -= regSize, srcOffset += regSize, dstOffset += regSize)
    {
        while (regSize > size)
        {
            regSize /= 2;
        }

        instruction loadIns;
        instruction storeIns;
        emitAttr    attr;

        switch (regSize)
        {
            case 1:
                loadIns  = INS_ldrb;
                storeIns = INS_strb;
                attr     = EA_4BYTE;
                break;
            case 2:
                loadIns  = INS_ldrh;
                storeIns = INS_strh;
                attr     = EA_4BYTE;
                break;
            case 4:
#ifdef TARGET_ARM64
            case 8:
#endif
                loadIns  = INS_ldr;
                storeIns = INS_str;
                attr     = EA_ATTR(regSize);
                break;
            default:
                unreached();
        }

        if (srcLcl != nullptr)
        {
            emit->Ins_R_S(loadIns, attr, tempReg, GetStackAddrMode(srcLcl, srcOffset));
        }
        else
        {
            emit->emitIns_R_R_I(loadIns, attr, tempReg, srcAddrBaseReg, srcOffset);
        }

        if (dstLcl != nullptr)
        {
            emit->Ins_R_S(storeIns, attr, tempReg, GetStackAddrMode(dstLcl, dstOffset));
        }
        else
        {
            emit->emitIns_R_R_I(storeIns, attr, tempReg, dstAddrBaseReg, dstOffset);
        }
    }

    if (store->IsIndir() && store->AsIndir()->IsVolatile())
    {
        instGen_MemoryBarrier(BARRIER_LOAD_ONLY);
    }

    if (layout->HasGCPtr())
    {
        GetEmitter()->EnableGC();
    }
}

void CodeGen::GenStructStoreUnrollRegs(GenTree* store, ClassLayout* layout)
{
    LclVarDsc* dstLcl         = nullptr;
    regNumber  dstAddrBaseReg = REG_NA;
    int        dstOffset      = 0;
    GenTree*   src;

    if (store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
    {
        dstLcl    = store->AsLclVarCommon()->GetLcl();
        dstOffset = store->AsLclVarCommon()->GetLclOffs();

        src = store->AsLclVarCommon()->GetOp(0);
    }
    else
    {
        GenTree* dstAddr = store->AsObj()->GetAddr();

        if (!dstAddr->isContained())
        {
            dstAddrBaseReg = genConsumeReg(dstAddr);
        }
        else if (GenTreeAddrMode* addrMode = dstAddr->IsAddrMode())
        {
            assert(addrMode->GetIndex() == nullptr);

            dstAddrBaseReg = genConsumeReg(addrMode->GetBase());
            dstOffset      = dstAddr->AsAddrMode()->GetOffset();
        }
        else
        {
            dstLcl    = dstAddr->AsLclAddr()->GetLcl();
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
        regs[i] = regCount == 1 ? genConsumeReg(src) : UseReg(src, i);

        var_types regType = call->GetRegType(i);
        unsigned  regSize = varTypeSize(regType);

        if (!varTypeUsesFloatReg(regType))
        {
            assert((i < regCount - 1) ? (regSize == REGSIZE_BYTES) : (regSize <= REGSIZE_BYTES));
            regType = layout->GetGCPtrType(i);
        }
        else if (i != 0)
        {
            assert(regSize == varTypeSize(regTypes[0]));
        }

        regTypes[i] = regType;
    }

    emitter* emit     = GetEmitter();
    unsigned regSize  = varTypeSize(regTypes[0]);
    unsigned regIndex = 0;

    assert((regSize <= REGSIZE_BYTES) || varTypeUsesFloatReg(regTypes[0]));

#ifdef TARGET_ARM64
    // TODO-MIKE-CQ: Using stp with SIMD16 is problematic - the offset needs to be a multiple
    // of 16. We can restrict address containment for STORE_OBJ but what about local stores?
    // We'd need to ensure that the frame offset is also multiple of 16.
    if ((regSize == 4) || (regSize == 8))
    {
        while ((regIndex + 1 < regCount) && (regSize * 2 <= size))
        {
            regNumber reg1  = regs[regIndex];
            emitAttr  attr1 = emitTypeSize(regTypes[regIndex++]);
            regNumber reg2  = regs[regIndex];
            emitAttr  attr2 = emitTypeSize(regTypes[regIndex++]);

            if (dstLcl != nullptr)
            {
                emit->emitIns_S_S_R_R(INS_stp, attr1, attr2, reg1, reg2, GetStackAddrMode(dstLcl, dstOffset));
            }
            else
            {
                emit->emitIns_R_R_R_I(INS_stp, attr1, reg1, reg2, dstAddrBaseReg, dstOffset, INS_OPTS_NONE, attr2);
            }

            dstOffset += regSize * 2;
            size -= regSize * 2;
        }
    }
#endif

    regNumber reg     = REG_NA;
    var_types regType = TYP_UNDEF;

    for (; (regIndex < regCount) && (size > 0); regIndex++, dstOffset += regSize, size -= regSize)
    {
        reg     = regs[regIndex];
        regType = regTypes[regIndex];

        if (regSize > size)
        {
            break;
        }

        instruction ins  = ins_Store(regType);
        emitAttr    attr = emitTypeSize(regType);

        if (dstLcl != nullptr)
        {
            emit->Ins_R_S(ins, attr, reg, GetStackAddrMode(dstLcl, dstOffset));
        }
        else
        {
            emit->emitIns_R_R_I(ins, attr, reg, dstAddrBaseReg, dstOffset);
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
#ifdef TARGET_ARM64
                emit->emitIns_R_R_I(INS_lsr, regShift >= 4 ? EA_8BYTE : EA_4BYTE, reg, reg, regShift * 8);
#else
                emit->emitIns_R_R_I(INS_lsr, EA_4BYTE, reg, reg, regShift * 8, INS_FLAGS_NOT_SET);
#endif
            }

            instruction ins;

            switch (regSize)
            {
                case 1:
                    ins = INS_strb;
                    break;
                case 2:
                    ins = INS_strh;
                    break;
#ifdef TARGET_ARM64
                case 4:
                    ins = INS_str;
                    break;
#endif
                default:
                    unreached();
            }

            if (dstLcl != nullptr)
            {
                emit->Ins_R_S(ins, EA_4BYTE, reg, GetStackAddrMode(dstLcl, dstOffset));
            }
            else
            {
                emit->emitIns_R_R_I(ins, EA_4BYTE, reg, dstAddrBaseReg, dstOffset);
            }
        }
    }
}

// Generate code for a struct store that contains GC pointers.
// This will generate a sequence of LDR/STR/LDP/STP instructions for
// non-GC slots and calls to to the BY_REF_ASSIGN helper otherwise:
//
// ldr tempReg, [X13, #8]
// str tempReg, [X14, #8]
// bl CORINFO_HELP_ASSIGN_BYREF
// ldp tempReg, tmpReg2, [X13, #8]
// stp tempReg, tmpReg2, [X14, #8]
// bl CORINFO_HELP_ASSIGN_BYREF
// ldr tempReg, [X13, #8]
// str tempReg, [X14, #8]
//
void CodeGen::GenStructStoreUnrollCopyWB(GenTree* store, ClassLayout* layout)
{
    assert(layout->HasGCPtr());

    ConsumeStructStore(store, layout, REG_WRITE_BARRIER_DST_BYREF, REG_WRITE_BARRIER_SRC_BYREF, REG_NA);

    GenTree*  dstAddr = nullptr;
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

    liveness.SetGCRegType(REG_WRITE_BARRIER_SRC_BYREF, srcAddrType);
    liveness.SetGCRegType(REG_WRITE_BARRIER_DST_BYREF, dstAddrType);

    if (store->IsIndir() && store->AsIndir()->IsVolatile())
    {
        instGen_MemoryBarrier();
    }

    emitter* emit      = GetEmitter();
    unsigned slotCount = layout->GetSlotCount();

    regNumber tmpReg = store->ExtractTempReg();

    assert(genIsValidIntReg(tmpReg));
    assert(tmpReg != REG_WRITE_BARRIER_SRC_BYREF);
    assert(tmpReg != REG_WRITE_BARRIER_DST_BYREF);

#ifdef TARGET_ARM64
    regNumber tmpReg2 = REG_NA;

    if (slotCount > 1)
    {
        tmpReg2 = store->GetSingleTempReg();

        assert(genIsValidIntReg(tmpReg2));
        assert(tmpReg2 != tmpReg);
        assert(tmpReg2 != REG_WRITE_BARRIER_DST_BYREF);
        assert(tmpReg2 != REG_WRITE_BARRIER_SRC_BYREF);
    }
#endif

    if (dstOnStack)
    {
        unsigned i = 0;

#ifdef TARGET_ARM64
        for (; i + 1 < slotCount; i += 2)
        {
            emitAttr attr0 = emitTypeSize(layout->GetGCPtrType(i + 0));
            emitAttr attr1 = emitTypeSize(layout->GetGCPtrType(i + 1));

            emit->emitIns_R_R_R_I(INS_ldp, attr0, tmpReg, tmpReg2, REG_WRITE_BARRIER_SRC_BYREF, 2 * REGSIZE_BYTES,
                                  INS_OPTS_POST_INDEX, attr1);
            emit->emitIns_R_R_R_I(INS_stp, attr0, tmpReg, tmpReg2, REG_WRITE_BARRIER_DST_BYREF, 2 * REGSIZE_BYTES,
                                  INS_OPTS_POST_INDEX, attr1);
        }
#endif

        for (; i < slotCount; i++)
        {
            emitAttr attr = emitTypeSize(layout->GetGCPtrType(i));

            emit->emitIns_R_R_I(INS_ldr, attr, tmpReg, REG_WRITE_BARRIER_SRC_BYREF, REGSIZE_BYTES,
#ifdef TARGET_ARM64
                                INS_OPTS_POST_INDEX);
#else
                                INS_FLAGS_DONT_CARE, INS_OPTS_LDST_POST_INC);
#endif
            emit->emitIns_R_R_I(INS_str, attr, tmpReg, REG_WRITE_BARRIER_DST_BYREF, REGSIZE_BYTES,
#ifdef TARGET_ARM64
                                INS_OPTS_POST_INDEX);
#else
                                INS_FLAGS_DONT_CARE, INS_OPTS_LDST_POST_INC);
#endif
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
#ifdef TARGET_ARM64
            else if ((i + 1 < slotCount) && !layout->IsGCPtr(i + 1))
            {
                emit->emitIns_R_R_R_I(INS_ldp, EA_PTRSIZE, tmpReg, tmpReg2, REG_WRITE_BARRIER_SRC_BYREF,
                                      2 * REGSIZE_BYTES, INS_OPTS_POST_INDEX);
                emit->emitIns_R_R_R_I(INS_stp, EA_PTRSIZE, tmpReg, tmpReg2, REG_WRITE_BARRIER_DST_BYREF,
                                      2 * REGSIZE_BYTES, INS_OPTS_POST_INDEX);

                i++;
            }
#endif
            else
            {
                emit->emitIns_R_R_I(INS_ldr, EA_PTRSIZE, tmpReg, REG_WRITE_BARRIER_SRC_BYREF, REGSIZE_BYTES,
#ifdef TARGET_ARM64
                                    INS_OPTS_POST_INDEX);
#else
                                    INS_FLAGS_DONT_CARE, INS_OPTS_LDST_POST_INC);
#endif
                emit->emitIns_R_R_I(INS_str, EA_PTRSIZE, tmpReg, REG_WRITE_BARRIER_DST_BYREF, REGSIZE_BYTES,
#ifdef TARGET_ARM64
                                    INS_OPTS_POST_INDEX);
#else
                                    INS_FLAGS_DONT_CARE, INS_OPTS_LDST_POST_INC);
#endif
            }
        }
    }

    if (store->IsIndir() && store->AsIndir()->IsVolatile())
    {
        instGen_MemoryBarrier(BARRIER_LOAD_ONLY);
    }

    // While we normally update GC liveness prior to the last instruction that uses them,
    // these actually live into the helper call.
    liveness.RemoveGCRegs(RBM_WRITE_BARRIER_SRC_BYREF | RBM_WRITE_BARRIER_DST_BYREF);
}

#ifdef TARGET_ARM64
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

    emit->emitIns_R_R_I(INS_add, EA_BYREF, REG_WRITE_BARRIER_DST, addrReg, addrOffset);

    if (layout->IsGCRef(0))
    {
        inst_Mov(TYP_REF, REG_WRITE_BARRIER_SRC, valReg0, true);

        liveness.SetGCRegs(TYP_REF, inGCrefRegSet);
        liveness.SetGCRegs(TYP_BYREF, inByrefRegSet | RBM_WRITE_BARRIER_DST);
        genEmitHelperCall(CORINFO_HELP_CHECKED_ASSIGN_REF, EA_PTRSIZE);
        liveness.SetGCRegs(TYP_REF, outGCrefRegSet);
        liveness.SetGCRegs(TYP_BYREF, outByrefRegSet);
    }
    else
    {
        emit->emitIns_R_R_I(INS_str, EA_8BYTE, valReg0, REG_WRITE_BARRIER_DST, REGSIZE_BYTES, INS_OPTS_POST_INDEX);
    }

    if (layout->IsGCRef(1))
    {
        inst_Mov(TYP_REF, REG_WRITE_BARRIER_SRC, valReg1, true);
        genEmitHelperCall(CORINFO_HELP_CHECKED_ASSIGN_REF, EA_PTRSIZE);
    }
    else
    {
        emit->emitIns_R_R(INS_str, EA_8BYTE, valReg1, REG_WRITE_BARRIER_DST);
    }
}
#endif // TARGET_ARM64

void CodeGen::genCallInstruction(GenTreeCall* call)
{
    // All virtuals should have been expanded into a control expression
    assert(!call->IsVirtual() || (call->gtControlExpr != nullptr) || (call->gtCallAddr != nullptr));

    for (GenTreeCall::Use& use : call->LateArgs())
    {
        GenTree* argNode = use.GetNode();

        if (argNode->OperIs(GT_PUTARG_STK))
        {
            continue;
        }

        CallArgInfo* argInfo = call->GetArgInfoByArgNode(argNode);
        argNode              = argNode->gtSkipReloadOrCopy();

        if (GenTreeFieldList* fieldList = argNode->IsFieldList())
        {
            INDEBUG(unsigned regIndex = 0;)
            for (GenTreeFieldList::Use& use : fieldList->Uses())
            {
                GenTree* node = use.GetNode();

                assert(node->gtSkipReloadOrCopy()->OperIs(GT_PUTARG_REG));

#ifdef TARGET_ARM
                if (node->TypeIs(TYP_LONG))
                {
                    UseRegs(node);

                    assert(node->GetRegNum(0) == argInfo->GetRegNum(regIndex++));
                    assert(node->GetRegNum(1) == argInfo->GetRegNum(regIndex++));

                    continue;
                }
#endif

                UseReg(node);

                assert(node->GetRegNum() == argInfo->GetRegNum(regIndex++));
            }

            continue;
        }

#if FEATURE_ARG_SPLIT
        if (GenTreePutArgSplit* argSplit = argNode->IsPutArgSplit())
        {
            assert((argInfo->GetRegCount() >= 1) && (argInfo->GetSlotCount() >= 1));

            // TODO-MIKE-Review: Why is UnspillRegsIfNeeded called instead of UseRegs?
            // Also, we're skipping a RELOAD/COPY above. Probably we can't actually
            // get a COPY/RELOAD here becuase these nodes have specific, single reg
            // requirements so there's little point in LSRA adding reloads/copies...
            UnspillRegsIfNeeded(argSplit);
            INDEBUG(VerifyUseOrder(argSplit));

            for (unsigned i = 0; i < argInfo->GetRegCount(); i++)
            {
                assert(argNode->GetRegNum(i) == argInfo->GetRegNum(i));
            }

            continue;
        }
#endif

        assert(argNode->OperIs(GT_PUTARG_REG));

#ifdef TARGET_ARM
        if (argNode->TypeIs(TYP_LONG))
        {
            UseRegs(argNode);

            assert(argNode->GetRegNum(0) == argInfo->GetRegNum(0));
            assert(argNode->GetRegNum(1) == argInfo->GetRegNum(1));

            continue;
        }
#endif

        UseReg(argNode);

        assert(argInfo->GetRegCount() == 1);
        assert(argNode->GetRegNum() == argInfo->GetRegNum());
    }

    // Insert a null check on "this" pointer if asked.
    if (call->NeedsNullCheck())
    {
        assert(call->GetArgInfoByArgNum(0)->GetRegNum() == REG_R0);

#if defined(TARGET_ARM)
        const regNumber tmpReg = call->ExtractTempReg();
        GetEmitter()->emitIns_R_R_I(INS_ldr, EA_4BYTE, tmpReg, REG_R0, 0);
#elif defined(TARGET_ARM64)
        GetEmitter()->emitIns_R_R_I(INS_ldr, EA_4BYTE, REG_ZR, REG_R0, 0);
#endif
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
    // and stack args in incoming arg area) and call target.  Epilog sequence would
    // generate "br <reg>".
    if (call->IsFastTailCall())
    {
        assert(!call->IsHelperCall());

        if (target != nullptr)
        {
            genConsumeReg(target);
            inst_Mov(TYP_I_IMPL, REG_FASTTAILCALL_TARGET, target->GetRegNum(), /* canSkip */ true);
        }

        return;
    }
#endif

    // For a pinvoke to unmanaged code we emit a label to clear
    // the GC pointer state before the callsite.
    // We can't utilize the typical lazy killing of GC pointers
    // at (or inside) the callsite.
    if (compiler->killGCRefs(call))
    {
        GetEmitter()->DefineTempLabel();
    }

    // Determine return value attr(s).
    emitAttr retSize       = EA_PTRSIZE;
    emitAttr secondRetSize = EA_UNKNOWN;

    if (varTypeIsStruct(call->GetType()) || call->HasMultiRegRetVal())
    {
        retSize = emitTypeSize(call->GetRegType(0));

        if (call->GetRegCount() > 1)
        {
            secondRetSize = emitTypeSize(call->GetRegType(1));
        }
    }
    else
    {
        assert(call->gtType != TYP_STRUCT);

        if (call->gtType == TYP_REF)
        {
            retSize = EA_GCREF;
        }
        else if (call->gtType == TYP_BYREF)
        {
            retSize = EA_BYREF;
        }
    }

    emitter::EmitCallType emitCallType;
    void*                 callAddr = nullptr;
    regNumber             callReg  = REG_NA;

    if (target != nullptr)
    {
        genConsumeReg(target);

        // We have already generated code for gtControlExpr evaluating it into a register.
        // We just need to emit "call reg" in this case.
        //
        assert(genIsValidIntReg(target->GetRegNum()));

        emitCallType = emitter::EC_INDIR_R;
        callReg      = target->GetRegNum();
    }
#ifdef FEATURE_READYTORUN_COMPILER
    else if (call->IsR2ROrVirtualStubRelativeIndir())
    {
        // Generate a direct call to a non-virtual user defined or helper method
        assert(call->IsHelperCall() || call->IsUserCall());
        assert(((call->IsR2RRelativeIndir()) && (call->gtEntryPoint.accessType == IAT_PVALUE)) ||
               ((call->IsVirtualStubRelativeIndir()) && (call->gtEntryPoint.accessType == IAT_VALUE)));
        assert(call->gtControlExpr == nullptr);
        assert(!call->IsTailCall());

        callReg = call->GetSingleTempReg();
        GetEmitter()->emitIns_R_R(ins_Load(TYP_I_IMPL), emitActualTypeSize(TYP_I_IMPL), callReg,
                                  REG_R2R_INDIRECT_PARAM);

        // We have now generated code for gtControlExpr evaluating it into `tmpReg`.
        // We just need to emit "call tmpReg" in this case.
        //
        assert(genIsValidIntReg(callReg));

        emitCallType = emitter::EC_INDIR_R;
    }
#endif // FEATURE_READYTORUN_COMPILER
    else
    {
        assert(call->IsHelperCall() || call->IsUserCall());

#ifdef FEATURE_READYTORUN_COMPILER
        if (call->gtEntryPoint.addr != NULL)
        {
            assert(call->gtEntryPoint.accessType == IAT_VALUE);

            callAddr = call->gtEntryPoint.addr;
        }
        else
#endif
        {
            callAddr = call->gtDirectCallAddress;
        }

        assert(callAddr != nullptr);

#ifdef TARGET_ARM
        if (!ArmImm::IsBlImm(reinterpret_cast<ssize_t>(callAddr), compiler))
        {
            emitCallType = emitter::EC_INDIR_R;
            callReg      = call->GetSingleTempReg();
            instGen_Set_Reg_To_Addr(callReg, callAddr);
        }
        else
#endif
        {
            emitCallType = emitter::EC_FUNC_TOKEN;
        }

#if 0 && defined(TARGET_ARM64)
        // Use this path if you want to load an absolute call target using
        //  a sequence of movs followed by an indirect call (blr instruction)
        // If this path is enabled, we need to ensure that REG_IP0 is assigned during Lowering.

        // Load the call target address in x16
        emitCallType = emitter::EC_INDIR_R;
        callReg = REG_IP0;
        instGen_Set_Reg_To_Imm(EA_8BYTE, callReg, (ssize_t)addr);
#endif
    }

    // Managed Retval sequence points needs to be generated while generating debug info for debuggable code.
    if ((compiler->genCallSite2ILOffsetMap != nullptr) && !call->IsTailCall())
    {
        if (IL_OFFSETX* ilOffset = compiler->genCallSite2ILOffsetMap->LookupPointer(call))
        {
            assert(*ilOffset != BAD_IL_OFFSET);
            genIPmappingAdd(*ilOffset, false);
        }
    }

    // clang-format off
    GetEmitter()->emitIns_Call(
        emitCallType,
        methHnd
        DEBUGARG(call->IsHelperCall() ? nullptr : call->callSig),
        callAddr,
        retSize MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(secondRetSize),
        callReg,
        false);
    // clang-format on

    if (genPendingCallLabel != nullptr)
    {
        GetEmitter()->DefineInlineTempLabel(genPendingCallLabel);
        genPendingCallLabel = nullptr;
    }

    // Update GC info:
    // All Callee arg registers are trashed and no longer contain any GC pointers.
    // TODO-Bug?: As a matter of fact shouldn't we be killing all of callee trashed regs here?
    // For now we will assert that other than arg regs gc ref/byref set doesn't contain any other
    // registers from RBM_CALLEE_TRASH
    assert((liveness.GetGCRegs(TYP_REF) & (RBM_CALLEE_TRASH & ~RBM_ARG_REGS)) == RBM_NONE);
    assert((liveness.GetGCRegs(TYP_BYREF) & (RBM_CALLEE_TRASH & ~RBM_ARG_REGS)) == RBM_NONE);
    liveness.SetGCRegs(TYP_REF, liveness.GetGCRegs(TYP_REF) & ~RBM_ARG_REGS);
    liveness.SetGCRegs(TYP_BYREF, liveness.GetGCRegs(TYP_BYREF) & ~RBM_ARG_REGS);

    var_types returnType = call->TypeGet();
    if (returnType != TYP_VOID)
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
                regNumber allocatedReg = call->GetRegNum(i);
                inst_Mov(regType, allocatedReg, returnReg, /* canSkip */ true);
            }
        }
        else
        {
            if (returnType == TYP_STRUCT)
            {
                returnType = call->GetRegType(0);
            }

            regNumber returnReg;

#ifdef TARGET_ARM
            if (call->IsHelperCall(compiler, CORINFO_HELP_INIT_PINVOKE_FRAME))
            {
                returnReg = REG_PINVOKE_TCB;
            }
            else
#endif
                if (varTypeUsesFloatArgReg(returnType) ARM_ONLY(&&!compiler->opts.compUseSoftFP))
            {
                returnReg = REG_FLOATRET;
            }
            else
            {
                returnReg = REG_INTRET;
            }

#ifdef TARGET_ARM
            if (compiler->opts.compUseSoftFP && (returnType == TYP_DOUBLE))
            {
                GetEmitter()->emitIns_R_R_R(INS_vmov_i2d, EA_8BYTE, call->GetRegNum(), REG_R0, REG_R1);
            }
            else if (compiler->opts.compUseSoftFP && (returnType == TYP_FLOAT))
            {
                inst_Mov(returnType, call->GetRegNum(), REG_R0, /* canSkip */ false);
            }
            else
#endif
                if (call->GetRegNum() != returnReg)
            {
                {
                    inst_Mov(returnType, call->GetRegNum(), returnReg, /* canSkip */ false);
                }
            }
        }

        DefCallRegs(call);
    }

    // If there is nothing next, that means the result is thrown away, so this value is not live.
    // However, for minopts or debuggable code, we keep it live to support managed return value debugging.
    if ((call->gtNext == nullptr) && !compiler->opts.MinOpts() && !compiler->opts.compDbgCode)
    {
        liveness.RemoveGCRegs(RBM_INTRET);
    }
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

    for (LclVarDsc* lcl : compiler->Params())
    {
        noway_assert(lcl->IsParam() && !lcl->IsPromoted());

        if (!lcl->IsRegParam())
        {
            continue;
        }

        // We expect all params to be DNER, otherwise we'd need to deal with moving between
        // assigned registers and param registers and potential circular dependencies.
        noway_assert(lcl->lvDoNotEnregister);

        if (lcl->IsHfaRegParam())
        {
            assert(varTypeIsStruct(lcl->GetType()));
            assert(!compiler->info.compIsVarArgs ARM_ONLY(&&!compiler->opts.UseSoftFP()));

            var_types type = lcl->GetLayout()->GetHfaElementType();
            emitAttr  size = emitTypeSize(type);
#ifdef TARGET_ARM64
            instruction ins             = INS_ldr;
            unsigned    elementRegCount = 1;
#else
            instruction ins              = INS_vldr;
            unsigned    elementRegCount  = type == TYP_DOUBLE ? 2 : 1;
#endif

            for (unsigned i = 0, regCount = lcl->GetParamRegCount(); i < regCount; i += elementRegCount)
            {
                regNumber reg = lcl->GetParamReg(i);
                assert(genIsValidFloatReg(reg));
                GetEmitter()->Ins_R_S(ins, size, reg, GetStackAddrMode(lcl, i / elementRegCount * EA_SIZE(size)));
            }
        }
        else if (varTypeIsStruct(lcl->GetType()))
        {
            assert(!lcl->HasGCLiveness());

            ClassLayout* layout = lcl->GetLayout();

            assert(lcl->GetParamRegCount() <= layout->GetSlotCount());

            for (unsigned i = 0, regCount = lcl->GetParamRegCount(); i < regCount; i++)
            {
                regNumber reg  = lcl->GetParamReg(i);
                var_types type = layout->GetGCPtrType(i);

                assert(isValidIntArgReg(reg));

                GetEmitter()->Ins_R_S(INS_ldr, emitTypeSize(type), reg, GetStackAddrMode(lcl, i * REGSIZE_BYTES));
                liveness.AddLiveLclRegs(genRegMask(reg));
                liveness.SetGCRegType(reg, type);
            }
        }
#ifdef TARGET_ARM
        else if (lcl->TypeIs(TYP_LONG) ||
                 (lcl->TypeIs(TYP_DOUBLE) && (compiler->info.compIsVarArgs || compiler->opts.UseSoftFP())))
        {
            assert(lcl->GetParamRegCount() == 2);

            regNumber regs[]{lcl->GetParamReg(0), lcl->GetParamReg(1)};

            assert(isValidIntArgReg(regs[0]) && isValidIntArgReg(regs[1]));

            GetEmitter()->Ins_R_S(INS_ldr, EA_4BYTE, regs[0], GetStackAddrMode(lcl, 0));
            GetEmitter()->Ins_R_S(INS_ldr, EA_4BYTE, regs[1], GetStackAddrMode(lcl, 4));
        }
#endif
        else
        {
            assert(lcl->GetParamRegCount() == 1);

            regNumber reg  = lcl->GetParamReg();
            var_types type = compiler->mangleVarArgsType(varActualType(lcl->GetType()));

            GetEmitter()->Ins_R_S(ins_Load(type), emitTypeSize(type), reg, GetStackAddrMode(lcl, 0));

            // Update argReg life and GC Info to indicate varDsc stack slot is dead and argReg is going live.
            // Note that we cannot modify varDsc->GetRegNum() here because another basic block may not be
            // expecting it. Therefore manually update life of argReg.  Note that GT_JMP marks the end of
            // the basic block and after which reg life and gc info will be recomputed for the new block
            // in genCodeForBBList().
            liveness.AddLiveLclRegs(genRegMask(reg));
            liveness.SetGCRegType(reg, type);
            liveness.RemoveGCSlot(lcl);
        }
    }

    if (!compiler->info.compIsVarArgs)
    {
        return;
    }

    // For varargs we need to load all arg registers, not just those associated with parameters.

    regMaskTP varargsIntRegMask = RBM_ARG_REGS;

    for (LclVarDsc* lcl : compiler->Params())
    {
        if (lcl->IsRegParam())
        {
            for (unsigned i = 0; i < lcl->GetParamRegCount(); i++)
            {
                regNumber reg = lcl->GetParamReg(i);
                assert(isValidIntArgReg(reg));
                varargsIntRegMask &= ~genRegMask(reg);
            }
        }
    }

    if (varargsIntRegMask == RBM_NONE)
    {
        return;
    }

#ifdef TARGET_ARM64
    unsigned firstParamLclNum = compiler->info.compRetBuffArg == 0 ? 1 : 0;
#else
    unsigned            firstParamLclNum = 0;
#endif

    // We have no way of knowing if args contain GC references.
    GetEmitter()->DisableGC();

    for (int i = 0; i < MAX_REG_ARG; ++i)
    {
        regNumber reg = static_cast<regNumber>(REG_R0 + i);

        if ((varargsIntRegMask & genRegMask(reg)) != 0)
        {
            GetEmitter()->Ins_R_S(INS_ldr, EA_PTRSIZE, reg, GetStackAddrMode(firstParamLclNum, i * REGSIZE_BYTES));
        }
    }

    // The epilog, which is not interruptible, should follow right after this code.
    GetEmitter()->EnableGC();
}

void CodeGen::GenJmpEpilog(BasicBlock* block, CORINFO_METHOD_HANDLE methHnd, const CORINFO_CONST_LOOKUP& addrInfo)
{
    SetHasTailCalls(true);

    noway_assert(block->bbJumpKind == BBJ_RETURN);
    noway_assert(block->GetFirstLIRNode() != nullptr);

    /* figure out what jump we have */
    GenTree* jmpNode = block->lastNode();
#if !FEATURE_FASTTAILCALL
    noway_assert(jmpNode->gtOper == GT_JMP);
#else  // FEATURE_FASTTAILCALL
    // armarch
    // If jmpNode is GT_JMP then gtNext must be null.
    // If jmpNode is a fast tail call, gtNext need not be null since it could have embedded stmts.
    noway_assert((jmpNode->gtOper != GT_JMP) || (jmpNode->gtNext == nullptr));

    // Could either be a "jmp method" or "fast tail call" implemented as epilog+jmp
    noway_assert((jmpNode->gtOper == GT_JMP) || ((jmpNode->gtOper == GT_CALL) && jmpNode->AsCall()->IsFastTailCall()));

    // The next block is associated with this "if" stmt
    if (jmpNode->gtOper == GT_JMP)
#endif // FEATURE_FASTTAILCALL
    {
        // Simply emit a jump to the methodHnd. This is similar to a call so we can use
        // the same descriptor with some minor adjustments.
        assert(methHnd != nullptr);
        assert(addrInfo.addr != nullptr);

#ifdef TARGET_ARMARCH
        emitter::EmitCallType callType;
        void*                 addr;
        regNumber             indCallReg;
        switch (addrInfo.accessType)
        {
            case IAT_VALUE:
#ifdef TARGET_ARM64
                if (Arm64Imm::IsBlImm(reinterpret_cast<ssize_t>(addrInfo.addr), compiler))
#else
                if (ArmImm::IsBlImm(reinterpret_cast<ssize_t>(addrInfo.addr), compiler))
#endif
                {
                    // Simple direct call
                    callType   = emitter::EC_FUNC_TOKEN;
                    addr       = addrInfo.addr;
                    indCallReg = REG_NA;
                    break;
                }

                // otherwise the target address doesn't fit in an immediate
                // so we have to burn a register...
                FALLTHROUGH;

            case IAT_PVALUE:
                // Load the address into a register, load indirect and call  through a register
                // We have to use R12 since we assume the argument registers are in use
                callType   = emitter::EC_INDIR_R;
                indCallReg = REG_INDIRECT_CALL_TARGET_REG;
                addr       = NULL;
                instGen_Set_Reg_To_Addr(indCallReg, addrInfo.addr);
                if (addrInfo.accessType == IAT_PVALUE)
                {
                    GetEmitter()->emitIns_R_R_I(INS_ldr, EA_PTRSIZE, indCallReg, indCallReg, 0);
                }
                break;

            case IAT_RELPVALUE:
                // Load the address into a register, load relative indirect and call through a register
                // We have to use R12 since we assume the argument registers are in use
                // LR is used as helper register right before it is restored from stack, thus,
                // all relative address calculations are performed before LR is restored.
                callType   = emitter::EC_INDIR_R;
                indCallReg = REG_R12;
                addr       = nullptr;
                break;

            case IAT_PPVALUE:
            default:
                NO_WAY("Unsupported JMP indirection");
        }

        /* Simply emit a jump to the methodHnd. This is similar to a call so we can use
         * the same descriptor with some minor adjustments.
         */

        // clang-format off
        GetEmitter()->emitIns_Call(
            callType,
            methHnd
            DEBUGARG(nullptr),
            addr,
            EA_UNKNOWN ARM64_ARG(EA_UNKNOWN),
            indCallReg, 
            true);
        // clang-format on
        CLANG_FORMAT_COMMENT_ANCHOR;
#endif // TARGET_ARMARCH
    }
#if FEATURE_FASTTAILCALL
    else
    {
        // Fast tail call.
        GenTreeCall* call = jmpNode->AsCall();

        assert(!call->IsHelperCall());

        // Try to dispatch this as a direct branch; this is possible when the call is
        // truly direct. In this case, the control expression will be null and the direct
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
                EA_UNKNOWN ARM64_ARG(EA_UNKNOWN),
                REG_NA,
                true);
            // clang-format on
        }
        else
        {
            // Target requires indirection to obtain. genCallInstruction will have materialized
            // it into REG_FASTTAILCALL_TARGET already, so just branch to it.
            GetEmitter()->emitIns_R(INS_br, EA_PTRSIZE, REG_FASTTAILCALL_TARGET);
        }
    }
#endif // FEATURE_FASTTAILCALL
}

void CodeGen::genIntCastOverflowCheck(GenTreeCast* cast, const GenIntCastDesc& desc, regNumber reg)
{
    switch (desc.CheckKind())
    {
        case GenIntCastDesc::CHECK_POSITIVE:
            GetEmitter()->emitIns_R_I(INS_cmp, EA_ATTR(desc.CheckSrcSize()), reg, 0);
            genJumpToThrowHlpBlk(EJ_lt, ThrowHelperKind::Overflow);
            break;

#ifdef TARGET_ARM64
        case GenIntCastDesc::CHECK_UINT_RANGE:
            // We need to check if the value is not greater than 0xFFFFFFFF but this value
            // cannot be encoded in the immediate operand of CMP. Use TST instead to check
            // if the upper 32 bits are zero.
            GetEmitter()->emitIns_R_I(INS_tst, EA_8BYTE, reg, 0xFFFFFFFF00000000LL);
            genJumpToThrowHlpBlk(EJ_ne, ThrowHelperKind::Overflow);
            break;

        case GenIntCastDesc::CHECK_POSITIVE_INT_RANGE:
            // We need to check if the value is not greater than 0x7FFFFFFF but this value
            // cannot be encoded in the immediate operand of CMP. Use TST instead to check
            // if the upper 33 bits are zero.
            GetEmitter()->emitIns_R_I(INS_tst, EA_8BYTE, reg, 0xFFFFFFFF80000000LL);
            genJumpToThrowHlpBlk(EJ_ne, ThrowHelperKind::Overflow);
            break;

        case GenIntCastDesc::CHECK_INT_RANGE:
            GetEmitter()->emitIns_R_R_I(INS_cmp, EA_8BYTE, reg, reg, 0, INS_OPTS_SXTW);
            genJumpToThrowHlpBlk(EJ_ne, ThrowHelperKind::Overflow);
            break;
#endif

        default:
        {
            assert(desc.CheckKind() == GenIntCastDesc::CHECK_SMALL_INT_RANGE);
            const int castMaxValue = desc.CheckSmallIntMax();
            const int castMinValue = desc.CheckSmallIntMin();

#ifdef TARGET_ARM64
            if (castMinValue != 0)
            {
                assert(((castMinValue == -128) && (castMaxValue == 127)) ||
                       ((castMinValue == -32768) && (castMaxValue == 32767)));

                GetEmitter()->emitIns_R_R_I(INS_cmp, EA_ATTR(desc.CheckSrcSize()), reg, reg, 0,
                                            castMinValue == -128 ? INS_OPTS_SXTB : INS_OPTS_SXTH);
                genJumpToThrowHlpBlk(EJ_ne, ThrowHelperKind::Overflow);
                break;
            }
#endif

            // Values greater than 255 cannot be encoded in the immediate operand of CMP.
            // Replace (x > max) with (x >= max + 1) where max + 1 (a power of 2) can be
            // encoded. We could do this for all max values but on ARM32 "cmp r0, 255"
            // is better than "cmp r0, 256" because it has a shorter encoding.
            if (castMaxValue > 255)
            {
                assert((castMaxValue == 32767) || (castMaxValue == 65535));
                GetEmitter()->emitIns_R_I(INS_cmp, EA_ATTR(desc.CheckSrcSize()), reg, castMaxValue + 1);
                genJumpToThrowHlpBlk((castMinValue == 0) ? EJ_hs : EJ_ge, ThrowHelperKind::Overflow);
            }
            else
            {
                GetEmitter()->emitIns_R_I(INS_cmp, EA_ATTR(desc.CheckSrcSize()), reg, castMaxValue);
                genJumpToThrowHlpBlk((castMinValue == 0) ? EJ_hi : EJ_gt, ThrowHelperKind::Overflow);
            }

            if (castMinValue != 0)
            {
                GetEmitter()->emitIns_R_I(INS_cmp, EA_ATTR(desc.CheckSrcSize()), reg, castMinValue);
                genJumpToThrowHlpBlk(EJ_lt, ThrowHelperKind::Overflow);
            }
        }
        break;
    }
}

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
        unsigned    insSize;

        switch (desc.LoadKind())
        {
            case GenIntCastDesc::LOAD_ZERO_EXTEND_SMALL_INT:
                ins     = (desc.LoadSrcSize() == 1) ? INS_ldrb : INS_ldrh;
                insSize = TARGET_POINTER_SIZE;
                break;
            case GenIntCastDesc::LOAD_SIGN_EXTEND_SMALL_INT:
                ins     = (desc.LoadSrcSize() == 1) ? INS_ldrsb : INS_ldrsh;
                insSize = TARGET_POINTER_SIZE;
                break;
#ifdef TARGET_64BIT
            case GenIntCastDesc::LOAD_SIGN_EXTEND_INT:
                ins     = INS_ldrsw;
                insSize = 8;
                break;
#endif
            default:
                assert(desc.LoadKind() == GenIntCastDesc::LOAD);
                ins     = INS_ldr;
                insSize = desc.LoadSrcSize();
                break;
        }

        // Note that we load directly into the destination register, this avoids the
        // need for a temporary register but assumes that enregistered variables are
        // not live in exception handlers. This works with EHWriteThru because the
        // register will be written only in genProduceReg, after the actual cast is
        // performed.

        StackAddrMode s;

        if (IsLocalMemoryOperand(src, &s))
        {
            GetEmitter()->Ins_R_S(ins, EA_ATTR(insSize), dstReg, s);
        }
        else if (src->OperIs(GT_IND))
        {
            emitInsLoad(ins, EA_ATTR(insSize), dstReg, src->AsIndir());
        }
        else
        {
            unreached();
        }

        srcReg = dstReg;
    }

    assert(genIsValidIntReg(srcReg));

    if (desc.CheckKind() != GenIntCastDesc::CHECK_NONE)
    {
        genIntCastOverflowCheck(cast, desc, srcReg);
    }

    if ((desc.ExtendKind() != GenIntCastDesc::COPY) || (srcReg != dstReg))
    {
        instruction ins;
        unsigned    insSize;

        switch (desc.ExtendKind())
        {
            case GenIntCastDesc::ZERO_EXTEND_SMALL_INT:
                ins     = (desc.ExtendSrcSize() == 1) ? INS_uxtb : INS_uxth;
                insSize = 4;
                break;
            case GenIntCastDesc::SIGN_EXTEND_SMALL_INT:
                ins     = (desc.ExtendSrcSize() == 1) ? INS_sxtb : INS_sxth;
                insSize = 4;
                break;
#ifdef TARGET_64BIT
            case GenIntCastDesc::ZERO_EXTEND_INT:
                ins     = INS_mov;
                insSize = 4;
                break;
            case GenIntCastDesc::SIGN_EXTEND_INT:
                ins     = INS_sxtw;
                insSize = 8;
                break;
#endif
            default:
                assert(desc.ExtendKind() == GenIntCastDesc::COPY);
                ins     = INS_mov;
                insSize = desc.ExtendSrcSize();
                break;
        }

        GetEmitter()->emitIns_Mov(ins, EA_ATTR(insSize), dstReg, srcReg, /* canSkip */ false);
    }

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

    regNumber srcReg = UseReg(src);
    regNumber dstReg = cast->GetRegNum();

    assert(genIsValidFloatReg(srcReg) && genIsValidFloatReg(dstReg));

    emitAttr insSize = emitTypeSize(dstType);

    if (srcType != dstType)
    {
#ifdef TARGET_ARM64
        instruction ins  = INS_fcvt;
        insOpts     opts = (srcType == TYP_FLOAT) ? INS_OPTS_S_TO_D : INS_OPTS_D_TO_S;
#else
        instruction ins = (srcType == TYP_FLOAT) ? INS_vcvt_f2d : INS_vcvt_d2f;
#endif
        GetEmitter()->emitIns_R_R(ins, insSize, dstReg, srcReg ARM64_ARG(opts));
    }
    else
    {
#ifdef TARGET_ARM64
        instruction ins = INS_mov;
#else
        instruction ins = INS_vmov;
#endif
        // TODO-MIKE-Review: How come we end up with a FLOAT-to-FLOAT cast in RayTracer.dll!?!
        GetEmitter()->emitIns_Mov(ins, insSize, dstReg, srcReg, /*canSkip*/ true);
    }

    genProduceReg(cast);
}

void CodeGen::genScaledAdd(emitAttr attr, regNumber targetReg, regNumber baseReg, regNumber indexReg, int scale)
{
    emitter* emit = GetEmitter();
    if (scale == 0)
    {
        // target = base + index
        GetEmitter()->emitIns_R_R_R(INS_add, attr, targetReg, baseReg, indexReg);
    }
    else
    {
        // target = base + index<<scale
        emit->emitIns_R_R_R_I(INS_add, attr, targetReg, baseReg, indexReg, scale ARM_ARG(INS_FLAGS_DONT_CARE),
                              INS_OPTS_LSL);
    }
}

void CodeGen::genLeaInstruction(GenTreeAddrMode* lea)
{
    // TODO-ARM64-CQ: The purpose of the LEA node is to directly reflect a single
    // target architecture addressing mode instruction.  Currently we're cheating
    // by producing one or more instructions to generate the addressing mode so we
    // need to modify lowering to produce LEAs that are a 1:1 relationship to the
    // ARM64 architecture.

    regNumber baseReg  = UseReg(lea->GetBase());
    regNumber indexReg = lea->GetIndex() == nullptr ? REG_NA : UseReg(lea->GetIndex());
    regNumber dstReg   = lea->GetRegNum();

    emitter* emit   = GetEmitter();
    emitAttr attr   = emitTypeSize(lea->GetType());
    int      offset = lea->GetOffset();

    if (indexReg != REG_NA)
    {
        assert(isPow2(lea->GetScale()));
        unsigned scale = genLog2(lea->GetScale());
        assert(scale <= 4);

        if (offset == 0)
        {
            genScaledAdd(attr, dstReg, baseReg, indexReg, scale);
        }
        else
        {
            regNumber tmpReg = lea->GetSingleTempReg();

            // When generating fully interruptible code we have to use the "large offset" sequence
            // when calculating a EA_BYREF as we can't report a byref that points outside of the object
            bool useLargeOffsetSeq = GetInterruptible() && (attr == EA_BYREF);

#ifdef TARGET_ARM64
            if (!useLargeOffsetSeq && Arm64Imm::IsAddImm(offset, EA_8BYTE))
#else
            if (!useLargeOffsetSeq && ArmImm::IsAddImm(offset, INS_FLAGS_DONT_CARE))
#endif
            {
                genScaledAdd(attr, tmpReg, baseReg, indexReg, scale);
                emit->emitIns_R_R_I(INS_add, attr, lea->GetRegNum(), tmpReg, offset);
            }
            else
            {
                noway_assert(tmpReg != indexReg);
                noway_assert(tmpReg != baseReg);

                instGen_Set_Reg_To_Imm(EA_PTRSIZE, tmpReg, offset);
                genScaledAdd(EA_PTRSIZE, tmpReg, tmpReg, indexReg, scale);
                emit->emitIns_R_R_R(INS_add, attr, dstReg, baseReg, tmpReg);
            }
        }
    }
#ifdef TARGET_ARM64
    else if (!Arm64Imm::IsAddImm(offset, EA_8BYTE))
#else
    else if (!ArmImm::IsAddImm(offset, INS_FLAGS_DONT_CARE))
#endif
    {
        regNumber tmpReg = lea->GetSingleTempReg();
        instGen_Set_Reg_To_Imm(EA_PTRSIZE, tmpReg, offset);
        emit->emitIns_R_R_R(INS_add, attr, dstReg, baseReg, tmpReg);
    }
    else if (offset != 0)
    {
        emit->emitIns_R_R_I(INS_add, attr, dstReg, baseReg, offset);
    }
    else
    {
        emit->emitIns_Mov(INS_mov, attr, dstReg, baseReg, /* canSkip */ true);
    }

    DefReg(lea);
}

void CodeGen::GenFloatNegate(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_FNEG) && varTypeIsFloating(node->GetType()));
    assert(node->GetOp(0)->GetType() == node->GetType());
    assert(node->GetRegNum() != REG_NA);

    regNumber reg = UseReg(node->GetOp(0));

#ifdef TARGET_ARM64
    GetEmitter()->emitIns_R_R(INS_fneg, emitTypeSize(node->GetType()), node->GetRegNum(), reg);
#else
    GetEmitter()->emitIns_R_R(INS_vneg, emitTypeSize(node->GetType()), node->GetRegNum(), reg);
#endif

    DefReg(node);
}

void CodeGen::GenFloatBinaryOp(GenTreeOp* node)
{
    assert(node->OperIs(GT_FADD, GT_FSUB, GT_FMUL, GT_FDIV) && varTypeIsFloating(node->GetType()));
    assert((node->GetOp(0)->GetType() == node->GetType()) && (node->GetOp(1)->GetType() == node->GetType()));
    assert(node->GetRegNum() != REG_NA);

    static_assert_no_msg(GT_FSUB - GT_FADD == 1);
    static_assert_no_msg(GT_FMUL - GT_FADD == 2);
    static_assert_no_msg(GT_FDIV - GT_FADD == 3);
#ifdef TARGET_ARM64
    static constexpr instruction insMap[]{INS_fadd, INS_fsub, INS_fmul, INS_fdiv};
#else
    static constexpr instruction insMap[]{INS_vadd, INS_vsub, INS_vmul, INS_vdiv};
#endif

    instruction ins  = insMap[node->GetOper() - GT_FADD];
    regNumber   reg1 = UseReg(node->GetOp(0));
    regNumber   reg2 = UseReg(node->GetOp(1));

    GetEmitter()->emitIns_R_R_R(ins, emitTypeSize(node->GetType()), node->GetRegNum(), reg1, reg2);

    DefReg(node);
}

void CodeGen::genCodeForNegNot(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_NEG, GT_NOT) && varTypeIsIntegral(node->GetType()));
    assert(node->GetRegNum() != REG_NA);

    regNumber reg = UseReg(node->GetOp(0));

#ifdef TARGET_ARM64
    instruction ins = node->OperIs(GT_NEG) ? INS_neg : INS_mvn;
    GetEmitter()->emitIns_R_R(ins, emitActualTypeSize(node->GetType()), node->GetRegNum(), reg);
#else
    instruction                  ins = node->OperIs(GT_NEG) ? INS_rsb : INS_mvn;
    GetEmitter()->emitIns_R_R_I(ins, emitTypeSize(node->GetType()), node->GetRegNum(), reg, 0, INS_FLAGS_SET);
#endif

    DefReg(node);
}

instruction CodeGen::ins_Copy(var_types type)
{
    assert(emitActualTypeSize(type) != EA_UNKNOWN);

    if (varTypeIsFloating(type))
    {
#ifdef TARGET_ARM64
        return INS_fmov;
#else
        return INS_vmov;
#endif
    }

    return INS_mov;
}

instruction CodeGen::ins_Copy(regNumber srcReg, var_types dstType)
{
    bool dstIsFloatReg = varTypeUsesFloatReg(dstType);
    bool srcIsFloatReg = genIsValidFloatReg(srcReg);

    if (srcIsFloatReg == dstIsFloatReg)
    {
        return ins_Copy(dstType);
    }

#ifdef TARGET_ARM64
    return dstIsFloatReg ? INS_fmov : INS_mov;
#else
    if (dstIsFloatReg)
    {
        assert(dstType == TYP_FLOAT);
        return INS_vmov_i2f;
    }
    else
    {
        assert(dstType == TYP_INT);
        return INS_vmov_f2i;
    }
#endif
}

instruction CodeGen::ins_Load(var_types srcType, bool aligned)
{
    assert(srcType != TYP_STRUCT);

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
}

instruction CodeGen::ins_Store(var_types dstType, bool aligned)
{
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
}

#endif // TARGET_ARMARCH
