// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                        ARM/ARM64 Code Generator Common Code               XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/
#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#ifdef TARGET_ARMARCH // This file is ONLY used for ARM and ARM64 architectures

#include "codegen.h"
#include "lower.h"
#include "gcinfo.h"
#include "emit.h"

//------------------------------------------------------------------------
// genStackPointerConstantAdjustment: add a specified constant value to the stack pointer.
// No probe is done.
//
// Arguments:
//    spDelta                 - the value to add to SP. Must be negative or zero.
//    regTmp                  - an available temporary register that is used if 'spDelta' cannot be encoded by
//                              'sub sp, sp, #spDelta' instruction.
//                              Can be REG_NA if the caller knows for certain that 'spDelta' fits into the immediate
//                              value range.
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

#ifdef TARGET_ARM64
    genInstrWithConstant(INS_sub, EA_PTRSIZE, REG_SPBASE, REG_SPBASE, -spDelta, regTmp);
#else
    genInstrWithConstant(INS_sub, EA_PTRSIZE, REG_SPBASE, REG_SPBASE, -spDelta, INS_FLAGS_DONT_CARE, regTmp);
#endif
}

//------------------------------------------------------------------------
// genStackPointerConstantAdjustmentWithProbe: add a specified constant value to the stack pointer,
// and probe the stack as appropriate. Should only be called as a helper for
// genStackPointerConstantAdjustmentLoopWithProbe.
//
// Arguments:
//    spDelta                 - the value to add to SP. Must be negative or zero. If zero, the probe happens,
//                              but the stack pointer doesn't move.
//    regTmp                  - temporary register to use as target for probe load instruction
//
// Return Value:
//    None.
//
void CodeGen::genStackPointerConstantAdjustmentWithProbe(ssize_t spDelta, regNumber regTmp)
{
    GetEmitter()->emitIns_R_R_I(INS_ldr, EA_4BYTE, regTmp, REG_SP, 0);
    genStackPointerConstantAdjustment(spDelta, regTmp);
}

//------------------------------------------------------------------------
// genStackPointerConstantAdjustmentLoopWithProbe: Add a specified constant value to the stack pointer,
// and probe the stack as appropriate. Generates one probe per page, up to the total amount required.
// This will generate a sequence of probes in-line.
//
// Arguments:
//    spDelta                 - the value to add to SP. Must be negative.
//    regTmp                  - temporary register to use as target for probe load instruction
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

        GetEmitter()->emitIns_R_R_I(INS_ldr, EA_4BYTE, regTmp, REG_SP, 0);
        lastTouchDelta = 0;
    }

    return lastTouchDelta;
}

//------------------------------------------------------------------------
// genCodeForTreeNode Generate code for a single node in the tree.
//
// Preconditions:
//    All operands have been evaluated.
//
void CodeGen::genCodeForTreeNode(GenTree* treeNode)
{
    regNumber targetReg  = treeNode->GetRegNum();
    var_types targetType = treeNode->TypeGet();
    emitter*  emit       = GetEmitter();

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

    // Is this a node whose value is already in a register?  LSRA denotes this by
    // setting the GTF_REUSE_REG_VAL flag.
    if (treeNode->IsReuseRegVal())
    {
        // For now, this is only used for constant nodes.
        assert((treeNode->OperGet() == GT_CNS_INT) || (treeNode->OperGet() == GT_CNS_DBL));
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
        case GT_START_NONGC:
            GetEmitter()->emitDisableGC();
            break;

        case GT_START_PREEMPTGC:
            // Kill callee saves GC registers, and create a label
            // so that information gets propagated to the emitter.
            gcInfo.gcMarkRegSetNpt(RBM_INT_CALLEE_SAVED);
            genDefineTempLabel(genCreateTempLabel());
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
        case GT_CNS_DBL:
            genSetRegToConst(targetReg, targetType, treeNode);
            genProduceReg(treeNode);
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
            genCodeForDivMod(treeNode->AsOp());
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
        // case GT_ROL: // No ROL instruction on ARM; it has been lowered to ROR.
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

        case GT_LCL_FLD_ADDR:
        case GT_LCL_VAR_ADDR:
            genCodeForLclAddr(treeNode->AsLclVarCommon());
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
            genRetFilt(treeNode);
            break;

        case GT_RETURN:
            genReturn(treeNode);
            break;

        case GT_LEA:
            genLeaInstruction(treeNode->AsAddrMode());
            break;

        case GT_INDEX_ADDR:
            genCodeForIndexAddr(treeNode->AsIndexAddr());
            break;

        case GT_IND:
            genCodeForIndir(treeNode->AsIndir());
            break;

#ifdef TARGET_ARM
        case GT_MUL_LONG:
            genCodeForMulLong(treeNode->AsOp());
            break;
#endif // TARGET_ARM

#ifdef TARGET_ARM64

        case GT_INC_SATURATE:
            genCodeForIncSaturate(treeNode);
            break;

        case GT_MULHI:
            genCodeForMulHi(treeNode->AsOp());
            break;

        case GT_SWAP:
            genCodeForSwap(treeNode->AsOp());
            break;
#endif // TARGET_ARM64

        case GT_JMP:
            genJmpMethod(treeNode);
            break;

        case GT_CKFINITE:
            genCkfinite(treeNode);
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
#endif // TARGET_ARM64
            genCodeForCompare(treeNode->AsOp());
            break;

        case GT_JTRUE:
            genCodeForJumpTrue(treeNode->AsOp());
            break;

#ifdef TARGET_ARM64
        case GT_JCMP:
            genCodeForJumpCompare(treeNode->AsOp());
            break;
#endif // TARGET_ARM64

        case GT_JCC:
            genCodeForJcc(treeNode->AsCC());
            break;

        case GT_SETCC:
            genCodeForSetcc(treeNode->AsCC());
            break;

        case GT_RETURNTRAP:
            genCodeForReturnTrap(treeNode->AsOp());
            break;

        case GT_STOREIND:
            genCodeForStoreInd(treeNode->AsStoreInd());
            break;

        case GT_RELOAD:
        case GT_COPY:
            // These are handled by genConsumeReg
            break;

        case GT_FIELD_LIST:
            // Should always be marked contained.
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
            genLockedInstructions(treeNode->AsOp());
            break;

        case GT_CMPXCHG:
            genCodeForCmpXchg(treeNode->AsCmpXchg());
            break;
#endif // TARGET_ARM64

        case GT_NOP:
            break;

        case GT_KEEPALIVE:
            if (treeNode->AsOp()->gtOp1->isContained())
            {
                GenTree* src = treeNode->AsUnOp()->GetOp(0);

                if (src->OperIs(GT_LCL_VAR, GT_LCL_FLD))
                {
                    genUpdateLife(src->AsLclVarCommon());
                }
            }
            else
            {
                genConsumeReg(treeNode->AsOp()->gtOp1);
            }
            break;

        case GT_NO_OP:
            instGen(INS_nop);
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

        case GT_PINVOKE_PROLOG:
            noway_assert(((gcInfo.gcRegGCrefSetCur | gcInfo.gcRegByrefSetCur) & ~fullIntArgRegMask()) == 0);

#ifdef PSEUDORANDOM_NOP_INSERTION
            // the runtime side requires the codegen here to be consistent
            emit->emitDisableRandomNops();
#endif // PSEUDORANDOM_NOP_INSERTION
            break;

        case GT_LABEL:
            genPendingCallLabel = genCreateTempLabel();
#if defined(TARGET_ARM)
            genMov32RelocatableDisplacement(genPendingCallLabel, targetReg);
#else
            emit->emitIns_R_L(INS_adr, EA_PTRSIZE, genPendingCallLabel, targetReg);
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
            genJumpTable(treeNode);
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
#ifdef TARGET_ARM
            emit->emitIns_R_C(INS_lea, EA_4BYTE, targetReg, treeNode->AsClsVar()->GetFieldHandle());
#else
            emit->emitIns_R_C(INS_adr, EA_8BYTE, targetReg, REG_NA, treeNode->AsClsVar()->GetFieldHandle());
#endif
            genProduceReg(treeNode);
            break;

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

//------------------------------------------------------------------------
// genSetRegToIcon: Generate code that will set the given register to the integer constant.
//
void CodeGen::genSetRegToIcon(regNumber reg, ssize_t val, var_types type, insFlags flags DEBUGARG(GenTreeFlags gtFlags))
{
    // Reg cannot be a FP reg
    assert(!genIsValidFloatReg(reg));

    // The only TYP_REF constant that can come this path is a managed 'null' since it is not
    // relocatable.  Other ref type constants (e.g. string objects) go through a different
    // code path.
    noway_assert(type != TYP_REF || val == 0);

    instGen_Set_Reg_To_Imm(emitActualTypeSize(type), reg, val, flags);
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
    assert(generatingProlog);

    if (!compiler->getNeedsGSSecurityCookie())
    {
        return;
    }

    if (compiler->gsGlobalSecurityCookieAddr == nullptr)
    {
        noway_assert(compiler->gsGlobalSecurityCookieVal != 0);
        // initReg = #GlobalSecurityCookieVal; [frame.GSSecurityCookie] = initReg
        genSetRegToIcon(initReg, compiler->gsGlobalSecurityCookieVal, TYP_I_IMPL);
        GetEmitter()->emitIns_S_R(INS_str, EA_PTRSIZE, initReg, compiler->lvaGSSecurityCookie, 0);
    }
    else
    {
        instGen_Set_Reg_To_Imm(EA_PTR_DSP_RELOC, initReg, (ssize_t)compiler->gsGlobalSecurityCookieAddr,
                               INS_FLAGS_DONT_CARE DEBUGARG((size_t)THT_SetGSCookie) DEBUGARG(GTF_EMPTY));
        GetEmitter()->emitIns_R_R_I(INS_ldr, EA_PTRSIZE, initReg, initReg, 0);
        regSet.verifyRegUsed(initReg);
        GetEmitter()->emitIns_S_R(INS_str, EA_PTRSIZE, initReg, compiler->lvaGSSecurityCookie, 0);
    }

    *pInitRegZeroed = false;
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
unsigned CodeGen::GetFirstStackParamLclNum()
{
#ifdef TARGET_WINDOWS
    // This can't deal with split params.
    assert(!compiler->info.compIsVarArgs);
#endif

    for (unsigned i = 0; i < compiler->info.compArgsCount; i++)
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
        INDEBUG(outArgLclSize = compiler->lvaOutgoingArgSpaceSize);
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

        GetEmitter()->emitIns_S_S_R_R(INS_stp, EA_8BYTE, EA_8BYTE, REG_ZR, REG_ZR, outArgLclNum,
                                      static_cast<int>(outArgLclOffs));
#else
        regNumber srcReg = src->GetRegNum();

        for (unsigned offset = 0; offset < putArg->GetArgSize(); offset += REGSIZE_BYTES)
        {
            GetEmitter()->emitIns_S_R(INS_str, EA_PTRSIZE, srcReg, outArgLclNum,
                                      static_cast<int>(outArgLclOffs + offset));
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

    GetEmitter()->emitIns_S_R(storeIns, storeAttr, srcReg, outArgLclNum, outArgLclOffs);

#ifdef TARGET_ARM
    if (srcType == TYP_LONG)
    {
        // This case currently only occurs for double types that are passed as TYP_LONG;
        // actual long types would have been decomposed by now.
        regNumber otherReg = src->GetRegNum(1);
        GetEmitter()->emitIns_S_R(storeIns, storeAttr, otherReg, outArgLclNum, outArgLclOffs + 4);
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
    unsigned     srcLclNum      = BAD_VAR_NUM;
    regNumber    srcAddrBaseReg = REG_NA;
    int          srcOffset      = 0;

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
            srcAddrBaseReg = genConsumeReg(srcAddr->AsAddrMode()->GetBase());
            assert(!srcAddr->AsAddrMode()->HasIndex());
            srcOffset = srcAddr->AsAddrMode()->GetOffset();
        }

        srcLayout = src->AsObj()->GetLayout();
    }

    emitter* emit   = GetEmitter();
    unsigned offset = 0;
    unsigned size   = srcLayout->GetSize();

    if (srcLclNum != BAD_VAR_NUM)
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

        if (srcLclNum != BAD_VAR_NUM)
        {
            emit->emitIns_R_R_S_S(INS_ldp, attr, attr2, tempReg, tempReg2, srcLclNum, srcOffset + offset);
        }
        else
        {
            emit->emitIns_R_R_R_I(INS_ldp, attr, tempReg, tempReg2, srcAddrBaseReg, srcOffset + offset, INS_OPTS_NONE,
                                  attr2);
        }

        // We can't write beyound the outgoing area area
        assert(outArgLclOffs + offset + 16 <= outArgLclSize);

        emit->emitIns_S_S_R_R(INS_stp, attr, attr2, tempReg, tempReg2, outArgLclNum, outArgLclOffs + offset);
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

        if (srcLclNum != BAD_VAR_NUM)
        {
            emit->emitIns_R_S(loadIns, attr, tempReg, srcLclNum, srcOffset + offset);
        }
        else
        {
            emit->emitIns_R_R_I(loadIns, attr, tempReg, srcAddrBaseReg, srcOffset + offset);
        }

        // We can't write beyound the outgoing area area
        assert(outArgLclOffs + offset + regSize <= outArgLclSize);

        emit->emitIns_S_R(storeIns, attr, tempReg, outArgLclNum, outArgLclOffs + offset);
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
        IsValidContainedLcl(src->AsLclVar());
        genUpdateLife(src->AsLclVar());
        unsigned  lclNum = src->AsLclVar()->GetLclNum();
        regNumber dstReg = bitcast->GetRegNum();

        GetEmitter()->emitIns_R_S(ins_Load(dstType), emitTypeSize(dstType), dstReg, lclNum, 0);
        DefReg(bitcast);
    }
#ifdef TARGET_ARM
    else if (varTypeIsLong(dstType) && src->TypeIs(TYP_DOUBLE))
    {
        regNumber srcReg  = UseReg(src);
        regNumber dstReg1 = bitcast->GetRegNum(0);
        regNumber dstReg2 = bitcast->GetRegNum(1);
        inst_RV_RV_RV(INS_vmov_d2i, dstReg1, dstReg2, srcReg, EA_8BYTE);
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
    const unsigned outArgLclSize = compiler->lvaOutgoingArgSpaceSize;
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

            GetEmitter()->emitIns_S_R(INS_str, EA_PTRSIZE, srcReg, outArgLclNum, dstOffset);
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
        unsigned dstOffset = outArgLclOffs;
        unsigned regIndex  = 0;
        for (GenTreeFieldList::Use& use : fieldList->Uses())
        {
            GenTree* fieldNode = use.GetNode();

            if (regIndex >= putArg->GetRegCount())
            {
                regNumber fieldReg = UseReg(fieldNode);
                var_types type     = fieldNode->GetType();
                emitAttr  attr     = emitTypeSize(type);

                GetEmitter()->emitIns_S_R(ins_Store(type), attr, fieldReg, outArgLclNum, dstOffset);
                dstOffset += EA_SIZE_IN_BYTES(attr);
                assert(dstOffset <= outArgLclSize); // We can't write beyond the outgoing area area

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
    unsigned     srcLclNum      = BAD_VAR_NUM;
    regNumber    srcAddrBaseReg = REG_NA;
    int          srcOffset      = 0;

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
            srcAddrBaseReg = genConsumeReg(srcAddr->AsAddrMode()->GetBase());
            assert(!srcAddr->AsAddrMode()->HasIndex());
            srcOffset = srcAddr->AsAddrMode()->GetOffset();
        }

        srcLayout = src->AsObj()->GetLayout();
    }

    unsigned offset    = 0;
    unsigned dstOffset = outArgLclOffs;
    unsigned size      = srcLayout->GetSize();

    if (srcLclNum != BAD_VAR_NUM)
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

        if (srcLclNum != BAD_VAR_NUM)
        {
            GetEmitter()->emitIns_R_S(loadIns, attr, tempReg, srcLclNum, srcOffset + offset);
        }
        else
        {
            GetEmitter()->emitIns_R_R_I(loadIns, attr, tempReg, srcAddrBaseReg, srcOffset + offset);
        }

        // We can't write beyound the outgoing area area
        assert(dstOffset + regSize <= outArgLclSize);

        GetEmitter()->emitIns_S_R(storeIns, attr, tempReg, outArgLclNum, dstOffset);
    }

    for (unsigned i = 0; i < putArg->GetRegCount(); i++)
    {
        unsigned  offset   = srcOffset + i * REGSIZE_BYTES;
        regNumber dstReg   = putArg->GetRegNum(i);
        emitAttr  slotAttr = emitTypeSize(putArg->GetRegType(i));

        if (srcLclNum != BAD_VAR_NUM)
        {
            GetEmitter()->emitIns_R_S(INS_ldr, slotAttr, dstReg, srcLclNum, offset);
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

    genJumpToThrowHlpBlk(EJ_hs, SCK_RNGCHK_FAIL);

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

//------------------------------------------------------------------------
// genCodeForShift: Generates the code sequence for a GenTree node that
// represents a bit shift or rotate operation (<<, >>, >>>, rol, ror).
//
// Arguments:
//    tree - the bit shift node (that specifies the type of bit shift to perform).
//
// Assumptions:
//    a) All GenTrees are register allocated.
//
void CodeGen::genCodeForShift(GenTreeOp* tree)
{
    var_types   targetType = tree->TypeGet();
    genTreeOps  oper       = tree->OperGet();
    instruction ins        = genGetInsForOper(oper);
    emitAttr    size       = emitActualTypeSize(tree);

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
        unsigned immWidth   = emitter::getBitWidth(size); // For ARM64, immWidth will be set to 32 or 64
        unsigned shiftByImm = (unsigned)shiftBy->AsIntCon()->gtIconVal & (immWidth - 1);

        GetEmitter()->emitIns_R_R_I(ins, size, dstReg, valueReg, shiftByImm);
    }

    DefReg(tree);
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

    // TODO-MIKE-Review: ARM64 uses 16 byte loads to load Vector3 locals while
    // XARCH uses 12 byte loads. Could XARCH also use 16 byte loads? The problem
    // with ARM64's approach is that the last vector element isn't zeroed. It's
    // not even guaranteed that the load doesn't access another local.
    //
    // XARCH actually does this too but only when loading from a LCL_VAR and only
    // on x64 (probably because on x86 attempting to load 16 byte may also result
    // in the load accessing another local.

    var_types targetType = tree->TypeGet();
    regNumber targetReg  = tree->GetRegNum();
    emitter*  emit       = GetEmitter();

    NYI_IF(targetType == TYP_STRUCT, "GT_LCL_FLD: struct load local field not supported");
    assert(targetReg != REG_NA);

    unsigned offs   = tree->GetLclOffs();
    unsigned varNum = tree->GetLclNum();
    assert(varNum < compiler->lvaCount);

#ifdef TARGET_ARM
    if (tree->IsOffsetMisaligned())
    {
        // Arm supports unaligned access only for integer types,
        // load the floating data as 1 or 2 integer registers and convert them to float.
        regNumber addr = tree->ExtractTempReg();
        emit->emitIns_R_S(INS_lea, EA_PTRSIZE, addr, varNum, offs);

        if (targetType == TYP_FLOAT)
        {
            regNumber floatAsInt = tree->GetSingleTempReg();
            emit->emitIns_R_R(INS_ldr, EA_4BYTE, floatAsInt, addr);
            emit->emitIns_Mov(INS_vmov_i2f, EA_4BYTE, targetReg, floatAsInt, /* canSkip */ false);
        }
        else
        {
            regNumber halfdoubleAsInt1 = tree->ExtractTempReg();
            regNumber halfdoubleAsInt2 = tree->GetSingleTempReg();
            emit->emitIns_R_R_I(INS_ldr, EA_4BYTE, halfdoubleAsInt1, addr, 0);
            emit->emitIns_R_R_I(INS_ldr, EA_4BYTE, halfdoubleAsInt2, addr, 4);
            emit->emitIns_R_R_R(INS_vmov_i2d, EA_8BYTE, targetReg, halfdoubleAsInt1, halfdoubleAsInt2);
        }
    }
    else
#endif // TARGET_ARM
    {
        emitAttr    attr = emitActualTypeSize(targetType);
        instruction ins  = ins_Load(targetType);
        emit->emitIns_R_S(ins, attr, targetReg, varNum, offs);
    }

    genProduceReg(tree);
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

    regNumber baseReg  = UseReg(base);
    regNumber indexReg = UseReg(index);

    // TODO-MIKE-Review: This is dubious, gcInfo stuff doesn't really matter until we reach a call...

    // NOTE: UseReg marks the register as not a GC pointer, as it assumes that the input registers
    // die at the first instruction generated by the node. This is not the case for `INDEX_ADDR`, however, as the
    // base register is multiply-used. As such, we need to mark the base register as containing a GC pointer until
    // we are finished generating the code for this node.

    gcInfo.gcMarkRegPtrVal(baseReg, base->GetType());
    assert(varTypeIsIntegral(index->GetType()));

    const regNumber tmpReg = node->GetSingleTempReg();

    // Generate the bounds check if necessary.
    if ((node->gtFlags & GTF_INX_RNGCHK) != 0)
    {
        GetEmitter()->emitIns_R_R_I(INS_ldr, EA_4BYTE, tmpReg, baseReg, node->GetLenOffs());
        GetEmitter()->emitIns_R_R(INS_cmp, emitActualTypeSize(index->TypeGet()), indexReg, tmpReg);
        genJumpToThrowHlpBlk(EJ_hs, SCK_RNGCHK_FAIL, node->GetThrowBlock());
    }

    // Can we use a ScaledAdd instruction?
    //
    if (isPow2(node->GetElemSize()) && (node->GetElemSize() <= 32768))
    {
        DWORD scale;
        BitScanForward(&scale, node->GetElemSize());

        // dest = base + index * scale
        genScaledAdd(emitActualTypeSize(node), node->GetRegNum(), baseReg, indexReg, scale);
    }
    else // we have to load the element attr and use a MADD (multiply-add) instruction
    {
        // tmpReg = element attr
        CodeGen::genSetRegToIcon(tmpReg, (ssize_t)node->GetElemSize(), TYP_INT);

        // dest = index * tmpReg + base
        GetEmitter()->emitIns_R_R_R_R(INS_MULADD, emitActualTypeSize(node), node->GetRegNum(), indexReg, tmpReg,
                                      baseReg);
    }

    // dest = dest + elemOffs
    GetEmitter()->emitIns_R_R_I(INS_add, emitActualTypeSize(node), node->GetRegNum(), node->GetRegNum(),
                                node->GetDataOffs());

    // TODO-MIKE-Review: Hrm, what if baseReg is a local variable reg?!
    gcInfo.gcMarkRegSetNpt(genRegMask(baseReg));

    DefReg(node);
}

void CodeGen::GenDynBlk(GenTreeDynBlk* store)
{
    ConsumeDynBlk(store, REG_ARG_0, REG_ARG_1, REG_ARG_2);

    if (store->IsVolatile())
    {
        instGen_MemoryBarrier();
    }

    genEmitHelperCall(store->OperIs(GT_COPY_BLK) ? CORINFO_HELP_MEMCPY : CORINFO_HELP_MEMSET, 0, EA_UNKNOWN);

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

    genEmitHelperCall(CORINFO_HELP_MEMSET, 0, EA_UNKNOWN);
}

void CodeGen::GenStructStoreMemCpy(GenTree* store, ClassLayout* layout)
{
    assert(!layout->HasGCPtr());

    ConsumeStructStore(store, layout, REG_ARG_0, REG_ARG_1, REG_ARG_2);

    if (store->IsIndir() && store->AsIndir()->IsVolatile())
    {
        instGen_MemoryBarrier();
    }

    genEmitHelperCall(CORINFO_HELP_MEMCPY, 0, EA_UNKNOWN);

    if (store->IsIndir() && store->AsIndir()->IsVolatile())
    {
        instGen_MemoryBarrier(BARRIER_LOAD_ONLY);
    }
}

void CodeGen::GenStructStoreUnrollInit(GenTree* store, ClassLayout* layout)
{
    assert(store->OperIs(GT_STORE_BLK, GT_STORE_OBJ, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    unsigned  dstLclNum      = BAD_VAR_NUM;
    regNumber dstAddrBaseReg = REG_NA;
    int       dstOffset      = 0;
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
            assert(!addrMode->HasIndex());

            dstAddrBaseReg = genConsumeReg(addrMode->GetBase());
            dstOffset      = dstAddr->AsAddrMode()->GetOffset();
        }
        else
        {
            assert(dstAddr->OperIsLocalAddr());

            dstLclNum = dstAddr->AsLclVarCommon()->GetLclNum();
            dstOffset = dstAddr->AsLclVarCommon()->GetLclOffs();
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
        if (dstLclNum != BAD_VAR_NUM)
        {
            emit->emitIns_S_S_R_R(INS_stp, EA_8BYTE, EA_8BYTE, srcReg, srcReg, dstLclNum, dstOffset);
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

        if (dstLclNum != BAD_VAR_NUM)
        {
            emit->emitIns_S_R(storeIns, attr, srcReg, dstLclNum, dstOffset);
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
        GetEmitter()->emitDisableGC();
    }

    unsigned  dstLclNum      = BAD_VAR_NUM;
    regNumber dstAddrBaseReg = REG_NA;
    int       dstOffset      = 0;
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

            assert(dstAddr->OperIsLocalAddr());

            dstLclNum = dstAddr->AsLclVarCommon()->GetLclNum();
            dstOffset = dstAddr->AsLclVarCommon()->GetLclOffs();
        }

        src = store->AsIndir()->GetValue();
    }

    unsigned  srcLclNum      = BAD_VAR_NUM;
    regNumber srcAddrBaseReg = REG_NA;
    int       srcOffset      = 0;

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
            srcAddrBaseReg = genConsumeReg(addrMode->GetBase());
            srcOffset      = srcAddr->AsAddrMode()->GetOffset();
        }
        else
        {
            assert(srcAddr->OperIsLocalAddr());

            srcLclNum = srcAddr->AsLclVarCommon()->GetLclNum();
            srcOffset = srcAddr->AsLclVarCommon()->GetLclOffs();
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
            if (srcLclNum != BAD_VAR_NUM)
            {
                emit->emitIns_R_R_S_S(INS_ldp, EA_8BYTE, EA_8BYTE, tempReg, tempReg2, srcLclNum, srcOffset);
            }
            else
            {
                emit->emitIns_R_R_R_I(INS_ldp, EA_8BYTE, tempReg, tempReg2, srcAddrBaseReg, srcOffset);
            }

            if (dstLclNum != BAD_VAR_NUM)
            {
                emit->emitIns_S_S_R_R(INS_stp, EA_8BYTE, EA_8BYTE, tempReg, tempReg2, dstLclNum, dstOffset);
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

        if (srcLclNum != BAD_VAR_NUM)
        {
            emit->emitIns_R_S(loadIns, attr, tempReg, srcLclNum, srcOffset);
        }
        else
        {
            emit->emitIns_R_R_I(loadIns, attr, tempReg, srcAddrBaseReg, srcOffset);
        }

        if (dstLclNum != BAD_VAR_NUM)
        {
            emit->emitIns_S_R(storeIns, attr, tempReg, dstLclNum, dstOffset);
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
        GetEmitter()->emitEnableGC();
    }
}

void CodeGen::GenStructStoreUnrollRegs(GenTree* store, ClassLayout* layout)
{
    unsigned  dstLclNum      = BAD_VAR_NUM;
    regNumber dstAddrBaseReg = REG_NA;
    int       dstOffset      = 0;
    GenTree*  src;

    if (store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD))
    {
        dstLclNum = store->AsLclVarCommon()->GetLclNum();
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
            assert(dstAddr->OperIsLocalAddr());

            dstLclNum = dstAddr->AsLclVarCommon()->GetLclNum();
            dstOffset = dstAddr->AsLclVarCommon()->GetLclOffs();
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

            if (dstLclNum != BAD_VAR_NUM)
            {
                emit->emitIns_S_S_R_R(INS_stp, attr1, attr2, reg1, reg2, dstLclNum, dstOffset);
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

        if (dstLclNum != BAD_VAR_NUM)
        {
            emit->emitIns_S_R(ins, attr, reg, dstLclNum, dstOffset);
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

            if (dstLclNum != BAD_VAR_NUM)
            {
                emit->emitIns_S_R(ins, EA_4BYTE, reg, dstLclNum, dstOffset);
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

    gcInfo.gcMarkRegPtrVal(REG_WRITE_BARRIER_SRC_BYREF, srcAddrType);
    gcInfo.gcMarkRegPtrVal(REG_WRITE_BARRIER_DST_BYREF, dstAddrType);

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
                genEmitHelperCall(CORINFO_HELP_ASSIGN_BYREF, 0, EA_PTRSIZE);
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

    // Clear the gcInfo for REG_WRITE_BARRIER_SRC_BYREF and REG_WRITE_BARRIER_DST_BYREF.
    // While we normally update GC info prior to the last instruction that uses them,
    // these actually live into the helper call.
    gcInfo.gcMarkRegSetNpt(RBM_WRITE_BARRIER_SRC_BYREF | RBM_WRITE_BARRIER_DST_BYREF);
}

#ifdef TARGET_ARM64
void CodeGen::GenStructStoreUnrollRegsWB(GenTreeObj* store)
{
    ClassLayout* layout = store->GetLayout();

    assert(layout->HasGCRef());
    assert(layout->GetSize() == 16);
    assert(store->GetValue()->GetMultiRegCount(compiler) == 2);

    regMaskTP inGCrefRegSet = gcInfo.gcRegGCrefSetCur;
    regMaskTP inByrefRegSet = gcInfo.gcRegByrefSetCur;

    GenTree*  addr       = store->GetAddr();
    regNumber addrReg    = addr->isUsedFromReg() ? UseReg(addr) : UseReg(addr->AsAddrMode()->GetBase());
    int       addrOffset = addr->isUsedFromReg() ? 0 : addr->AsAddrMode()->GetOffset();
    GenTree*  val        = store->GetValue();
    regNumber valReg0    = UseReg(val, 0);
    regNumber valReg1    = UseReg(val, 1);
    emitter*  emit       = GetEmitter();

    regMaskTP outGCrefRegSet = gcInfo.gcRegGCrefSetCur;
    regMaskTP outByrefRegSet = gcInfo.gcRegByrefSetCur;

    emit->emitIns_R_R_I(INS_add, EA_BYREF, REG_WRITE_BARRIER_DST, addrReg, addrOffset);

    if (layout->IsGCRef(0))
    {
        inst_Mov(TYP_REF, REG_WRITE_BARRIER_SRC, valReg0, true);

        gcInfo.gcRegGCrefSetCur = inGCrefRegSet;
        gcInfo.gcRegByrefSetCur = inByrefRegSet | RBM_WRITE_BARRIER_DST;
        genEmitHelperCall(CORINFO_HELP_CHECKED_ASSIGN_REF, 0, EA_PTRSIZE);
        gcInfo.gcRegGCrefSetCur = outGCrefRegSet;
        gcInfo.gcRegByrefSetCur = outByrefRegSet;
    }
    else
    {
        emit->emitIns_R_R_I(INS_str, EA_8BYTE, valReg0, REG_WRITE_BARRIER_DST, REGSIZE_BYTES, INS_OPTS_POST_INDEX);
    }

    if (layout->IsGCRef(1))
    {
        inst_Mov(TYP_REF, REG_WRITE_BARRIER_SRC, valReg1, true);
        genEmitHelperCall(CORINFO_HELP_CHECKED_ASSIGN_REF, 0, EA_PTRSIZE);
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
            genCheckConsumeNode(argSplit);

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
        genDefineTempLabel(genCreateTempLabel());
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

    IL_OFFSETX ilOffset = BAD_IL_OFFSET;

    // We need to propagate the IL offset information to the call instruction, so we can emit
    // an IL to native mapping record for the call, to support managed return value debugging.
    // We don't want tail call helper calls that were converted from normal calls to get a record,
    // so we skip this hash table lookup logic in that case.
    if (compiler->opts.compDbgInfo && compiler->genCallSite2ILOffsetMap != nullptr && !call->IsTailCall())
    {
        (void)compiler->genCallSite2ILOffsetMap->Lookup(call, &ilOffset);
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
        if (!validImmForBL(reinterpret_cast<ssize_t>(callAddr)))
        {
            emitCallType = emitter::EC_INDIR_R;
            callReg      = call->GetSingleTempReg();
            instGen_Set_Reg_To_Imm(EA_HANDLE_CNS_RELOC, callReg, reinterpret_cast<ssize_t>(callAddr));
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
        instGen_Set_Reg_To_Imm(EA_8BYTE, callReg, (ssize_t) addr);
#endif
    }

    // clang-format off
    GetEmitter()->emitIns_Call(
        emitCallType,
        methHnd
        DEBUGARG(call->IsHelperCall() ? nullptr : call->callSig),
        callAddr,
        0,
        retSize
        MULTIREG_HAS_SECOND_GC_RET_ONLY_ARG(secondRetSize),
        gcInfo.gcVarPtrSetCur,
        gcInfo.gcRegGCrefSetCur,
        gcInfo.gcRegByrefSetCur,
        ilOffset,
        callReg,
        false);
    // clang-format on

    // if it was a pinvoke we may have needed to get the address of a label
    if (genPendingCallLabel)
    {
        genDefineInlineTempLabel(genPendingCallLabel);
        genPendingCallLabel = nullptr;
    }

    // Update GC info:
    // All Callee arg registers are trashed and no longer contain any GC pointers.
    // TODO-Bug?: As a matter of fact shouldn't we be killing all of callee trashed regs here?
    // For now we will assert that other than arg regs gc ref/byref set doesn't contain any other
    // registers from RBM_CALLEE_TRASH
    assert((gcInfo.gcRegGCrefSetCur & (RBM_CALLEE_TRASH & ~RBM_ARG_REGS)) == 0);
    assert((gcInfo.gcRegByrefSetCur & (RBM_CALLEE_TRASH & ~RBM_ARG_REGS)) == 0);
    gcInfo.gcRegGCrefSetCur &= ~RBM_ARG_REGS;
    gcInfo.gcRegByrefSetCur &= ~RBM_ARG_REGS;

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
                // The CORINFO_HELP_INIT_PINVOKE_FRAME helper uses a custom calling convention that returns with
                // TCB in REG_PINVOKE_TCB. fgMorphCall() sets the correct argument registers.
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
                inst_RV_RV_RV(INS_vmov_i2d, call->GetRegNum(), REG_R0, REG_R1, EA_8BYTE);
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
        gcInfo.gcMarkRegSetNpt(RBM_INTRET);
    }
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

        noway_assert(varDsc->IsParam());

        if (varDsc->IsRegParam() && (varDsc->GetRegNum() != REG_STK))
        {
            // Skip reg args which are already in its right register for jmp call.
            // If not, we will spill such args to their stack locations.
            //
            // If we need to generate a tail call profiler hook, then spill all
            // arg regs to free them up for the callback.
            if (!compiler->compIsProfilerHookNeeded() && (varDsc->GetRegNum() == varDsc->GetParamReg()))
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
        assert(varDsc->TypeGet() != TYP_STRUCT);
        var_types storeType = genActualType(varDsc->TypeGet());
        emitAttr  storeSize = emitActualTypeSize(storeType);

#ifdef TARGET_ARM
        // TODO-MIKE-Cleanup: This is likely dead code.
        if (varDsc->TypeGet() == TYP_LONG)
        {
            // long - at least the low half must be enregistered
            GetEmitter()->emitIns_S_R(INS_str, EA_4BYTE, varDsc->GetRegNum(), varNum, 0);
        }
        else
#endif // TARGET_ARM
        {
            GetEmitter()->emitIns_S_R(ins_Store(storeType), storeSize, varDsc->GetRegNum(), varNum, 0);
        }
        // Update lvRegNum life and GC info to indicate lvRegNum is dead and varDsc stack slot is going live.
        // Note that we cannot modify varDsc->GetRegNum() here because another basic block may not be expecting it.
        // Therefore manually update life of varDsc->GetRegNum().
        regMaskTP tempMask = genRegMask(varDsc->GetRegNum());
        regSet.RemoveMaskVars(tempMask);
        gcInfo.gcMarkRegSetNpt(tempMask);
        if (compiler->lvaIsGCTracked(varDsc))
        {
            VarSetOps::AddElemD(compiler, gcInfo.gcVarPtrSetCur, varNum);
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
    for (unsigned varNum = 0; varNum < compiler->info.compArgsCount; varNum++)
    {
        varDsc = compiler->lvaGetDesc(varNum);

        if (varDsc->IsPromoted())
        {
            noway_assert(varDsc->GetPromotedFieldCount() == 1); // We only handle one field here

            varDsc = compiler->lvaGetDesc(varDsc->GetPromotedFieldLclNum(0));
        }

        noway_assert(varDsc->IsParam());

        // Skip if arg not passed in a register.
        if (!varDsc->IsRegParam())
        {
            continue;
        }

        if (varDsc->IsHfaRegParam())
        {
            // Note that for HFA, the argument is currently marked DNER so locals's register will always be
            // REG_STK. We home the incoming HFA argument registers in the prolog. Then we'll load them back
            // here, whether they are already in the correct registers or not. This is such a corner case that
            // it is not worth optimizing it.

            assert(varDsc->GetRegNum() == REG_STK);
            assert(varTypeIsStruct(varDsc->GetType()));
            assert(!compiler->info.compIsVarArgs ARM_ONLY(&&!compiler->opts.UseSoftFP()));

            var_types type = varDsc->GetLayout()->GetHfaElementType();
            emitAttr  size = emitTypeSize(type);
#ifdef TARGET_ARM64
            instruction ins             = INS_ldr;
            unsigned    elementRegCount = 1;
#else
            instruction ins             = INS_vldr;
            unsigned    elementRegCount = type == TYP_DOUBLE ? 2 : 1;
#endif

            for (unsigned i = 0, regCount = varDsc->GetParamRegCount(); i < regCount; i += elementRegCount)
            {
                regNumber reg = varDsc->GetParamReg(i);
                assert(genIsValidFloatReg(reg));
                GetEmitter()->emitIns_R_S(ins, size, reg, varNum, i / elementRegCount * EA_SIZE(size));
            }

            continue;
        }

        // Is register argument already in the right register?
        // If not load it from its stack location.
        regNumber argReg     = varDsc->GetParamReg();
        regNumber argRegNext = REG_NA;

#ifdef TARGET_ARM64
        if (varDsc->GetRegNum() != argReg)
        {
            var_types loadType;

            if (varTypeIsStruct(varDsc->GetType()))
            {
                // Must be <= 16 bytes or else it wouldn't be passed in registers, except for HFA,
                // which can be bigger (and is handled above).
                noway_assert(EA_SIZE_IN_BYTES(varDsc->lvSize()) <= 16);
                loadType = varDsc->GetLayout()->GetGCPtrType(0);
            }
            else
            {
                loadType = compiler->mangleVarArgsType(genActualType(varDsc->TypeGet()));
            }

            emitAttr loadSize = emitActualTypeSize(loadType);
            GetEmitter()->emitIns_R_S(ins_Load(loadType), loadSize, argReg, varNum, 0);

            // Update argReg life and GC Info to indicate varDsc stack slot is dead and argReg is going live.
            // Note that we cannot modify varDsc->GetRegNum() here because another basic block may not be
            // expecting it. Therefore manually update life of argReg.  Note that GT_JMP marks the end of
            // the basic block and after which reg life and gc info will be recomputed for the new block
            // in genCodeForBBList().
            regSet.AddMaskVars(genRegMask(argReg));
            gcInfo.gcMarkRegPtrVal(argReg, loadType);

            if (compiler->lvaIsMultiRegStructParam(varDsc))
            {
                // Restore the second register.
                // TODO-MIKE-Fix: Hah, they forgot about varargs split params and loaded x8...
                argRegNext = REG_NEXT(argReg);

                loadType = varDsc->GetLayout()->GetGCPtrType(1);
                loadSize = emitActualTypeSize(loadType);
                GetEmitter()->emitIns_R_S(ins_Load(loadType), loadSize, argRegNext, varNum, TARGET_POINTER_SIZE);

                regSet.AddMaskVars(genRegMask(argRegNext));
                gcInfo.gcMarkRegPtrVal(argRegNext, loadType);
            }

            if (compiler->lvaIsGCTracked(varDsc))
            {
                VarSetOps::RemoveElemD(compiler, gcInfo.gcVarPtrSetCur, varDsc->lvVarIndex);
            }
        }

        if (compiler->info.compIsVarArgs)
        {
            // In case of a jmp call to a vararg method ensure only integer registers are passed.
            assert((genRegMask(argReg) & (RBM_ARG_REGS | RBM_ARG_RET_BUFF)) != RBM_NONE);
            assert(!varDsc->IsHfaRegParam());

            fixedIntArgMask |= genRegMask(argReg);

            if (compiler->lvaIsMultiRegStructParam(varDsc))
            {
                assert(argRegNext != REG_NA);
                fixedIntArgMask |= genRegMask(argRegNext);
            }

            if (argReg == REG_ARG_0)
            {
                assert(firstArgVarNum == BAD_VAR_NUM);
                firstArgVarNum = varNum;
            }
        }

#else  // !TARGET_ARM64

        bool      twoParts = false;
        var_types loadType = TYP_UNDEF;
        if (varDsc->TypeGet() == TYP_LONG)
        {
            twoParts = true;
        }
        else if (varDsc->TypeGet() == TYP_DOUBLE)
        {
            if (compiler->info.compIsVarArgs || compiler->opts.compUseSoftFP)
            {
                twoParts = true;
            }
        }

        if (twoParts)
        {
            argRegNext = REG_NEXT(argReg);

            if (varDsc->GetRegNum() != argReg)
            {
                GetEmitter()->emitIns_R_S(INS_ldr, EA_PTRSIZE, argReg, varNum, 0);
                GetEmitter()->emitIns_R_S(INS_ldr, EA_PTRSIZE, argRegNext, varNum, REGSIZE_BYTES);
            }

            if (compiler->info.compIsVarArgs)
            {
                fixedIntArgMask |= genRegMask(argReg);
                fixedIntArgMask |= genRegMask(argRegNext);
            }
        }
        else if (varTypeIsStruct(varDsc))
        {
            regNumber slotReg = argReg;
            unsigned  maxSize = min(varDsc->lvSize(), (REG_ARG_LAST + 1 - argReg) * REGSIZE_BYTES);

            for (unsigned ofs = 0; ofs < maxSize; ofs += REGSIZE_BYTES)
            {
                unsigned idx = ofs / REGSIZE_BYTES;
                loadType     = varDsc->GetLayout()->GetGCPtrType(idx);

                if (varDsc->GetRegNum() != argReg)
                {
                    emitAttr loadSize = emitActualTypeSize(loadType);

                    GetEmitter()->emitIns_R_S(ins_Load(loadType), loadSize, slotReg, varNum, ofs);
                }

                regSet.AddMaskVars(genRegMask(slotReg));
                gcInfo.gcMarkRegPtrVal(slotReg, loadType);
                if (genIsValidIntReg(slotReg) && compiler->info.compIsVarArgs)
                {
                    fixedIntArgMask |= genRegMask(slotReg);
                }

                // TODO-MIKE-Cleanup: Use varDsc->GetParamReg(i) instead of REG_NEXT.
                slotReg = REG_NEXT(slotReg);
            }
        }
        else
        {
            loadType = compiler->mangleVarArgsType(genActualType(varDsc->TypeGet()));

            if (varDsc->GetRegNum() != argReg)
            {
                GetEmitter()->emitIns_R_S(ins_Load(loadType), emitTypeSize(loadType), argReg, varNum, 0);
            }

            regSet.AddMaskVars(genRegMask(argReg));
            gcInfo.gcMarkRegPtrVal(argReg, loadType);

            if (genIsValidIntReg(argReg) && compiler->info.compIsVarArgs)
            {
                fixedIntArgMask |= genRegMask(argReg);
            }
        }

        if (compiler->lvaIsGCTracked(varDsc))
        {
            VarSetOps::RemoveElemD(compiler, gcInfo.gcVarPtrSetCur, varDsc->lvVarIndex);
        }
#endif // !TARGET_ARM64
    }

    // Jmp call to a vararg method - if the method has fewer than fixed arguments that can be max attr of reg,
    // load the remaining integer arg registers from the corresponding
    // shadow stack slots.  This is for the reason that we don't know the number and type
    // of non-fixed params passed by the caller, therefore we have to assume the worst case
    // of caller passing all integer arg regs that can be max attr of reg.
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
                    GetEmitter()->emitIns_R_S(INS_ldr, EA_PTRSIZE, argReg, firstArgVarNum, argOffset);
                }

                argOffset += REGSIZE_BYTES;
            }
            GetEmitter()->emitEnableGC();
        }
    }
}

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
            GetEmitter()->emitIns_R_I(INS_cmp, EA_ATTR(desc.CheckSrcSize()), reg, 0);
            genJumpToThrowHlpBlk(EJ_lt, SCK_OVERFLOW);
            break;

#ifdef TARGET_ARM64
        case GenIntCastDesc::CHECK_UINT_RANGE:
            // We need to check if the value is not greater than 0xFFFFFFFF but this value
            // cannot be encoded in the immediate operand of CMP. Use TST instead to check
            // if the upper 32 bits are zero.
            GetEmitter()->emitIns_R_I(INS_tst, EA_8BYTE, reg, 0xFFFFFFFF00000000LL);
            genJumpToThrowHlpBlk(EJ_ne, SCK_OVERFLOW);
            break;

        case GenIntCastDesc::CHECK_POSITIVE_INT_RANGE:
            // We need to check if the value is not greater than 0x7FFFFFFF but this value
            // cannot be encoded in the immediate operand of CMP. Use TST instead to check
            // if the upper 33 bits are zero.
            GetEmitter()->emitIns_R_I(INS_tst, EA_8BYTE, reg, 0xFFFFFFFF80000000LL);
            genJumpToThrowHlpBlk(EJ_ne, SCK_OVERFLOW);
            break;

        case GenIntCastDesc::CHECK_INT_RANGE:
            GetEmitter()->emitIns_R_R_I(INS_cmp, EA_8BYTE, reg, reg, 0, INS_OPTS_SXTW);
            genJumpToThrowHlpBlk(EJ_ne, SCK_OVERFLOW);
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
                genJumpToThrowHlpBlk(EJ_ne, SCK_OVERFLOW);
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
                genJumpToThrowHlpBlk((castMinValue == 0) ? EJ_hs : EJ_ge, SCK_OVERFLOW);
            }
            else
            {
                GetEmitter()->emitIns_R_I(INS_cmp, EA_ATTR(desc.CheckSrcSize()), reg, castMaxValue);
                genJumpToThrowHlpBlk((castMinValue == 0) ? EJ_hi : EJ_gt, SCK_OVERFLOW);
            }

            if (castMinValue != 0)
            {
                GetEmitter()->emitIns_R_I(INS_cmp, EA_ATTR(desc.CheckSrcSize()), reg, castMinValue);
                genJumpToThrowHlpBlk(EJ_lt, SCK_OVERFLOW);
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
//    The cast node is not a contained node and must have an assigned register.
//    Neither the source nor target type can be a floating point type.
//
// TODO-ARM64-CQ: Allow castOp to be a contained node without an assigned register.
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

        unsigned lclNum;
        unsigned lclOffs;

        if (IsLocalMemoryOperand(src, &lclNum, &lclOffs))
        {
            GetEmitter()->emitIns_R_S(ins, EA_ATTR(insSize), dstReg, lclNum, lclOffs);
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

    regNumber srcReg = genConsumeReg(src);
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

//------------------------------------------------------------------------
// genCreateAndStoreGCInfo: Create and record GC Info for the function.
//
void CodeGen::genCreateAndStoreGCInfo(unsigned codeSize,
                                      unsigned prologSize,
                                      unsigned epilogSize DEBUGARG(void* codePtr))
{
    IAllocator*    allowZeroAlloc = new (compiler, CMK_GC) CompIAllocator(compiler->getAllocatorGC());
    GcInfoEncoder* gcInfoEncoder  = new (compiler, CMK_GC)
        GcInfoEncoder(compiler->info.compCompHnd, compiler->info.compMethodInfo, allowZeroAlloc, NOMEM);
    assert(gcInfoEncoder != nullptr);

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

#ifdef TARGET_ARM64

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
                preservedAreaSize += REGSIZE_BYTES;

            preservedAreaSize += 1; // bool for synchronized methods
        }

        // Used to signal both that the method is compiled for EnC, and also the attr of the block at the top of the
        // frame
        gcInfoEncoder->SetSizeOfEditAndContinuePreservedArea(preservedAreaSize);
    }

#endif // TARGET_ARM64

    if (compiler->opts.IsReversePInvoke())
    {
        LclVarDsc* reversePInvokeFrameLcl = compiler->lvaGetDesc(compiler->lvaReversePInvokeFrameVar);
        gcInfoEncoder->SetReversePInvokeFrameSlot(reversePInvokeFrameLcl->GetStackOffset());
    }

    gcInfoEncoder->Build();

    // GC Encoder automatically puts the GC info in the right spot using ICorJitInfo::allocGCInfo(size_t)
    // let's save the values anyway for debugging purposes
    compInfoBlkAddr = gcInfoEncoder->Emit();
    compInfoBlkSize = 0; // not exposed by the GCEncoder interface
}

// clang-format off
const CodeGen::GenConditionDesc CodeGen::GenConditionDesc::map[32]
{
    { },       // NONE
    { },       // 1
    { EJ_lt }, // SLT
    { EJ_le }, // SLE
    { EJ_ge }, // SGE
    { EJ_gt }, // SGT
    { EJ_mi }, // S
    { EJ_pl }, // NS

    { EJ_eq }, // EQ
    { EJ_ne }, // NE
    { EJ_lo }, // ULT
    { EJ_ls }, // ULE
    { EJ_hs }, // UGE
    { EJ_hi }, // UGT
    { EJ_hs }, // C
    { EJ_lo }, // NC

    { EJ_eq },                // FEQ
    { EJ_gt, GT_AND, EJ_lo }, // FNE
    { EJ_lo },                // FLT
    { EJ_ls },                // FLE
    { EJ_ge },                // FGE
    { EJ_gt },                // FGT
    { EJ_vs },                // O
    { EJ_vc },                // NO

    { EJ_eq, GT_OR, EJ_vs },  // FEQU
    { EJ_ne },                // FNEU
    { EJ_lt },                // FLTU
    { EJ_le },                // FLEU
    { EJ_hs },                // FGEU
    { EJ_hi },                // FGTU
    { },                      // P
    { },                      // NP
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
    assert(genIsValidIntReg(dstReg));

#ifdef TARGET_ARM64
    const GenConditionDesc& desc = GenConditionDesc::Get(condition);

    inst_SET(desc.jumpKind1, dstReg);

    if (desc.oper != GT_NONE)
    {
        BasicBlock* labelNext = genCreateTempLabel();
        inst_JMP((desc.oper == GT_OR) ? desc.jumpKind1 : emitter::emitReverseJumpKind(desc.jumpKind1), labelNext);
        inst_SET(desc.jumpKind2, dstReg);
        genDefineTempLabel(labelNext);
    }
#else
    // Emit code like that:
    //   ...
    //   bgt True
    //   movs rD, #0
    //   b Next
    // True:
    //   movs rD, #1
    // Next:
    //   ...

    BasicBlock* labelTrue = genCreateTempLabel();
    inst_JCC(condition, labelTrue);

    GetEmitter()->emitIns_R_I(INS_mov, emitActualTypeSize(type), dstReg, 0);

    BasicBlock* labelNext = genCreateTempLabel();
    GetEmitter()->emitIns_J(INS_b, labelNext);

    genDefineTempLabel(labelTrue);
    GetEmitter()->emitIns_R_I(INS_mov, emitActualTypeSize(type), dstReg, 1);
    genDefineTempLabel(labelNext);
#endif
}

//------------------------------------------------------------------------
// genScaledAdd: A helper for genLeaInstruction.
//
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
#if defined(TARGET_ARM)
        emit->emitIns_R_R_R_I(INS_add, attr, targetReg, baseReg, indexReg, scale, INS_FLAGS_DONT_CARE, INS_OPTS_LSL);
#elif defined(TARGET_ARM64)
        emit->emitIns_R_R_R_I(INS_add, attr, targetReg, baseReg, indexReg, scale, INS_OPTS_LSL);
#endif
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
    emitAttr attr   = emitTypeSize(lea);
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

            if (!useLargeOffsetSeq && emitter::emitIns_valid_imm_for_add(offset))
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
    else if (!emitter::emitIns_valid_imm_for_add(offset))
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

//------------------------------------------------------------------------
// genPushCalleeSavedRegisters: Push any callee-saved registers we have used.
//
// Arguments (arm64):
//    initReg        - A scratch register (that gets set to zero on some platforms).
//    pInitRegZeroed - OUT parameter. *pInitRegZeroed is set to 'true' if this method sets initReg register to zero,
//                     'false' if initReg was set to a non-zero value, and left unchanged if initReg was not touched.
//
#if defined(TARGET_ARM64)
void CodeGen::genPushCalleeSavedRegisters(regNumber initReg, bool* pInitRegZeroed)
#else
void CodeGen::genPushCalleeSavedRegisters()
#endif
{
    assert(generatingProlog);

#ifdef TARGET_ARM64
    // Probe large frames now, if necessary, since genPushCalleeSavedRegisters() will allocate the frame. Note that
    // for arm64, genAllocLclFrame only probes the frame; it does not actually allocate it (it does not change SP).
    // For arm64, we are probing the frame before the callee-saved registers are saved. The 'initReg' might have
    // been calculated to be one of the callee-saved registers (say, if all the integer argument registers are
    // in use, and perhaps with other conditions being satisfied). This is ok in other cases, after the callee-saved
    // registers have been saved. So instead of letting genAllocLclFrame use initReg as a temporary register,
    // always use REG_SCRATCH. We don't care if it trashes it, so ignore the initRegZeroed output argument.
    bool ignoreInitRegZeroed = false;
    genAllocLclFrame(lclFrameSize, REG_SCRATCH, &ignoreInitRegZeroed, intRegState.rsCalleeRegArgMaskLiveIn);
#endif

    regMaskTP rsPushRegs = regSet.rsGetModifiedRegsMask() & RBM_CALLEE_SAVED;

#if ETW_EBP_FRAMED
    if (!isFramePointerUsed() && regSet.rsRegsModified(RBM_FPBASE))
    {
        noway_assert(!"Used register RBM_FPBASE as a scratch register!");
    }
#endif

#ifdef TARGET_ARMARCH
    // On ARM we push the FP (frame-pointer) here along with all other callee saved registers
    if (isFramePointerUsed())
        rsPushRegs |= RBM_FPBASE;

    //
    // It may be possible to skip pushing/popping lr for leaf methods. However, such optimization would require
    // changes in GC suspension architecture.
    //
    // We would need to guarantee that a tight loop calling a virtual leaf method can be suspended for GC. Today, we
    // generate partially interruptible code for both the method that contains the tight loop with the call and the leaf
    // method. GC suspension depends on return address hijacking in this case. Return address hijacking depends
    // on the return address to be saved on the stack. If we skipped pushing/popping lr, the return address would never
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

    regSet.rsMaskCalleeSaved = rsPushRegs;
#endif // TARGET_ARMARCH

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

#if defined(TARGET_ARM)
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
#elif defined(TARGET_ARM64)
    // See the document "ARM64 JIT Frame Layout" and/or "ARM64 Exception Data" for more details or requirements and
    // options. Case numbers in comments here refer to this document. See also Compiler::lvaAssignFrameOffsets()
    // for pictures of the general frame layouts, and CodeGen::genFuncletProlog() implementations (per architecture)
    // for pictures of the funclet frame layouts.
    //
    // For most frames, generate, e.g.:
    //      stp fp,  lr,  [sp,-0x80]!   // predecrement SP with full frame attr, and store FP/LR pair.
    //      stp r19, r20, [sp, 0x60]    // store at positive offset from SP established above, into callee-saved area
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

    // The amount to subtract from SP before starting to store the callee-saved registers. It might be folded into the
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
        // Do we need another frame pointer register to get good code quality in the case of having the frame pointer
        // point high in the frame, so we can take advantage of arm64's preference for positive offsets? C++ native
        // code dedicates callee-saved x19 to this, so generates:
        //      mov x19, sp
        // in the prolog, then uses x19 for local var accesses. Given that this case is so rare, we currently do
        // not do this. That means that negative offsets from FP might need to use the reserved register to form
        // the local variable offset for an addressing mode.

        if (((compiler->lvaOutgoingArgSpaceSize == 0) && (totalFrameSize <= 504)) &&
            !genSaveFpLrWithAllCalleeSavedRegisters)
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
                        unsigned(compiler->lvaOutgoingArgSpaceSize), totalFrameSize, lclFrameSize);

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
                        unsigned(compiler->lvaOutgoingArgSpaceSize), totalFrameSize, lclFrameSize);

                frameType = 2;

                // Generate:
                //      sub sp,sp,#framesz
                //      stp fp,lr,[sp,#outsz]   // note that by necessity, #outsz <= #framesz - 16, so #outsz <= 496.

                assert(totalFrameSize - compiler->lvaOutgoingArgSpaceSize <= STACK_PROBE_BOUNDARY_THRESHOLD_BYTES);

                GetEmitter()->emitIns_R_R_I(INS_sub, EA_PTRSIZE, REG_SPBASE, REG_SPBASE, totalFrameSize);
                compiler->unwindAllocStack(totalFrameSize);

                assert(compiler->lvaOutgoingArgSpaceSize + 2 * REGSIZE_BYTES <= (unsigned)totalFrameSize);

                GetEmitter()->emitIns_R_R_R_I(INS_stp, EA_PTRSIZE, REG_FP, REG_LR, REG_SPBASE,
                                              compiler->lvaOutgoingArgSpaceSize);
                compiler->unwindSaveRegPair(REG_FP, REG_LR, compiler->lvaOutgoingArgSpaceSize);

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
            // Note that even if we align the SP alterations, that does not imply that we are creating empty alignment
            // slots. In fact, we are not; any empty alignment slots were calculated in
            // Compiler::lvaAssignFrameOffsets() and its callees.

            int calleeSaveSPDeltaUnaligned = totalFrameSize - lclFrameSize;
            if (genSaveFpLrWithAllCalleeSavedRegisters)
            {
                JITDUMP("Frame type 5 (save FP/LR at top). #outsz=%d; #framesz=%d; LclFrameSize=%d\n",
                        unsigned(compiler->lvaOutgoingArgSpaceSize), totalFrameSize, lclFrameSize);

                // This case is much simpler, because we allocate space for the callee-saved register area, including
                // FP/LR. Note the SP adjustment might be SUB or be folded into the first store as a predecrement.
                // Then, we use a single SUB to establish the rest of the frame. We need to be careful about where
                // to establish the frame pointer, as there is a limit of 2040 bytes offset from SP to FP in the
                // unwind codes when FP is established.
                frameType = 5;
            }
            else
            {
                JITDUMP("Frame type 3 (save FP/LR at bottom). #outsz=%d; #framesz=%d; LclFrameSize=%d\n",
                        unsigned(compiler->lvaOutgoingArgSpaceSize), totalFrameSize, lclFrameSize);

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

        offsetSpToSavedFp = compiler->lvaOutgoingArgSpaceSize;
    }
    else if (frameType == 3)
    {
        assert(!genSaveFpLrWithAllCalleeSavedRegisters);

        int remainingFrameSz = totalFrameSize - calleeSaveSPDelta;
        assert(remainingFrameSz > 0);
        assert((remainingFrameSz % 16) == 0); // this is guaranteed to be 16-byte aligned because each component --
                                              // totalFrameSize and calleeSaveSPDelta -- is 16-byte aligned.

        if (compiler->lvaOutgoingArgSpaceSize > 504)
        {
            // We can't do "stp fp,lr,[sp,#outsz]" because #outsz is too big.
            // If compiler->lvaOutgoingArgSpaceSize is not aligned, we need to align the SP adjustment.
            assert(remainingFrameSz > (int)compiler->lvaOutgoingArgSpaceSize);
            int spAdjustment2Unaligned = remainingFrameSz - compiler->lvaOutgoingArgSpaceSize;
            int spAdjustment2          = (int)roundUp((unsigned)spAdjustment2Unaligned, STACK_ALIGN);
            int alignmentAdjustment2   = spAdjustment2 - spAdjustment2Unaligned;
            assert((alignmentAdjustment2 == 0) || (alignmentAdjustment2 == 8));

            JITDUMP("    spAdjustment2=%d\n", spAdjustment2);

            genPrologSaveRegPair(REG_FP, REG_LR, alignmentAdjustment2, -spAdjustment2, false, initReg, pInitRegZeroed);
            offset += spAdjustment2;

            // Now subtract off the #outsz (or the rest of the #outsz if it was unaligned, and the above "sub"
            // included some of it)

            int spAdjustment3 = compiler->lvaOutgoingArgSpaceSize - alignmentAdjustment2;
            assert(spAdjustment3 > 0);
            assert((spAdjustment3 % 16) == 0);

            JITDUMP("    alignmentAdjustment2=%d\n", alignmentAdjustment2);
            genEstablishFramePointer(alignmentAdjustment2, /* reportUnwindData */ true);

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
            genPrologSaveRegPair(REG_FP, REG_LR, compiler->lvaOutgoingArgSpaceSize, -remainingFrameSz, false, initReg,
                                 pInitRegZeroed);
            offset += remainingFrameSz;

            offsetSpToSavedFp = compiler->lvaOutgoingArgSpaceSize;
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
        genEstablishFramePointer(offsetSpToSavedFp, /* reportUnwindData */ true);

        // We just established the frame pointer chain; don't do it again.
        establishFramePointer = false;

        int remainingFrameSz = totalFrameSize - calleeSaveSPDelta;
        assert(remainingFrameSz > 0);
        assert((remainingFrameSz % 16) == 0); // this is guaranteed to be 16-byte aligned because each component --
                                              // totalFrameSize and calleeSaveSPDelta -- is 16-byte aligned.

        JITDUMP("    remainingFrameSz=%d\n", remainingFrameSz);

        // We've already established the frame pointer, so no need to report the stack pointer change to unwind info.
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
        genEstablishFramePointer(offsetSpToSavedFp, /* reportUnwindData */ true);
    }

    assert(offset == totalFrameSize);
#endif // TARGET_ARM64
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

#endif // TARGET_ARMARCH
