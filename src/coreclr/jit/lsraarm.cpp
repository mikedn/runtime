// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

#ifdef TARGET_ARM

#include "lsra.h"

void LinearScan::BuildNode(GenTree* tree)
{
    assert(!tree->isContained());

    switch (tree->GetOper())
    {
        case GT_LCL_VAR:
        case GT_LCL_FLD:
            assert(!compiler->lvaGetDesc(tree->AsLclVarCommon())->IsRegCandidate());

            if (tree->OperIs(GT_LCL_FLD) && tree->AsLclFld()->IsOffsetMisaligned())
            {
                BuildInternalIntDef(tree); // to generate address.
                BuildInternalIntDef(tree); // to move float into an int reg.

                if (tree->TypeIs(TYP_DOUBLE))
                {
                    BuildInternalIntDef(tree); // to move the second half into an int reg.
                }

                BuildInternalUses();
            }

            BuildDef(tree);
            break;

        case GT_STORE_LCL_VAR:
            BuildStoreLclVar(tree->AsLclVar());
            break;

        case GT_STORE_LCL_FLD:
            BuildStoreLclFld(tree->AsLclFld());
            break;

        case GT_KEEPALIVE:
            BuildOperandUses(tree->AsUnOp()->GetOp(0));
            break;

        case GT_CKFINITE:
            BuildInternalIntDef(tree);
            BuildUse(tree->AsUnOp()->GetOp(0));
            BuildInternalUses();
            BuildDef(tree);
            break;

        case GT_INTRINSIC:
            // TODO-ARM: Implement other type of intrinsics (round, sqrt and etc.)
            switch (tree->AsIntrinsic()->GetIntrinsic())
            {
                GenTree* op1;

                case NI_System_Math_Abs:
                case NI_System_Math_Sqrt:
                    op1 = tree->AsIntrinsic()->GetOp(0);
                    assert(varTypeIsFloating(op1->GetType()) && (op1->GetType() == tree->GetType()));
                    BuildUse(op1);
                    BuildDef(tree);
                    break;
                default:
                    unreached();
            }
            break;

        case GT_CAST:
            BuildCast(tree->AsCast());
            break;

        case GT_FNEG:
        case GT_NEG:
        case GT_NOT:
            BuildUse(tree->AsUnOp()->GetOp(0));
            BuildDef(tree);
            break;

        case GT_SWITCH_TABLE:
            BuildUse(tree->AsOp()->GetOp(0));
            BuildUse(tree->AsOp()->GetOp(1));
            break;

        case GT_FADD:
        case GT_FSUB:
        case GT_FMUL:
        case GT_FDIV:
            BuildUse(tree->AsOp()->GetOp(0));
            BuildUse(tree->AsOp()->GetOp(1));
            BuildDef(tree);
            break;

        case GT_ADD_LO:
        case GT_ADD_HI:
        case GT_SUB_LO:
        case GT_SUB_HI:
        case GT_ADD:
        case GT_SUB:
        case GT_AND:
        case GT_OR:
        case GT_XOR:
        case GT_LSH:
        case GT_RSH:
        case GT_RSZ:
        case GT_ROR:
            BuildUse(tree->AsOp()->GetOp(0));

            if (!tree->AsOp()->GetOp(1)->isContained())
            {
                BuildUse(tree->AsOp()->GetOp(1));
            }
            FALLTHROUGH;
        case GT_JMPTABLE:
        case GT_LCL_ADDR:
        case GT_CLS_VAR_ADDR:
        case GT_PHYSREG:
        case GT_LABEL:
        case GT_SETCC:
            BuildDef(tree);
            FALLTHROUGH;
        case GT_NOP:
        case GT_NO_OP:
        case GT_IL_OFFSET:
        case GT_START_NONGC:
        case GT_PINVOKE_PROLOG:
        case GT_PROF_HOOK:
        case GT_MEMORYBARRIER:
        case GT_JTRUE:
        case GT_JCC:
        case GT_JMP:
            break;

        case GT_INDEX_ADDR:
            BuildInternalIntDef(tree);
            BuildUse(tree->AsOp()->GetOp(0));
            BuildUse(tree->AsOp()->GetOp(1));
            BuildInternalUses();
            BuildDef(tree);
            break;

        case GT_LSH_HI:
        case GT_RSH_LO:
            BuildShiftLong(tree->AsOp());
            break;

        case GT_RETURNTRAP:
            BuildUse(tree->AsUnOp()->GetOp(0));
            BuildKills(tree, compiler->compHelperCallKillSet(CORINFO_HELP_STOP_FOR_GC));
            break;

        case GT_MUL:
            if (tree->gtOverflow())
            {
                BuildInternalIntDef(tree);
                setInternalRegsDelayFree = true;
            }

            BuildUse(tree->AsOp()->GetOp(0));
            BuildUse(tree->AsOp()->GetOp(1));

            if (tree->gtOverflow())
            {
                BuildInternalUses();
            }

            BuildDef(tree);
            break;

        case GT_MUL_LONG:
            BuildUse(tree->AsOp()->GetOp(0));
            BuildUse(tree->AsOp()->GetOp(1));
            BuildDef(tree, TYP_INT, RBM_NONE, 0);
            BuildDef(tree, TYP_INT, RBM_NONE, 1);
            break;

        case GT_START_PREEMPTGC:
            BuildKills(tree, RBM_NONE);
            break;

        case GT_LONG:
            // Contained nodes are already processed, only unused LONG can reach here.
            // TODO-MIKE-Review: Why is such a node generated in the first place?
            assert(tree->IsUnusedValue());
            // An unused LONG node needs to consume its sources, but need not produce a register.
            tree->SetType(TYP_VOID);
            tree->ClearUnusedValue();
            BuildUse(tree->AsOp()->GetOp(0));
            BuildUse(tree->AsOp()->GetOp(1));
            break;

        case GT_CNS_DBL:
            if (tree->TypeIs(TYP_FLOAT))
            {
                BuildInternalIntDef(tree);
            }
            else
            {
                assert(tree->TypeIs(TYP_DOUBLE));
                BuildInternalIntDef(tree);
                BuildInternalIntDef(tree);
            }

            BuildInternalUses();
            FALLTHROUGH;
        case GT_CNS_INT:
            BuildDef(tree)->getInterval()->isConstant = true;
            break;

        case GT_RETURN:
            BuildReturn(tree->AsUnOp());
            BuildKills(tree, getKillSetForReturn());
            break;

        case GT_RETFILT:
            if (!tree->TypeIs(TYP_VOID))
            {
                assert(tree->TypeIs(TYP_INT));
                BuildUse(tree->AsUnOp()->GetOp(0), RBM_INTRET);
            }
            break;

        case GT_BOUNDS_CHECK:
            BuildUse(tree->AsBoundsChk()->GetOp(0));
            BuildUse(tree->AsBoundsChk()->GetOp(1));
            break;

        case GT_ARR_INDEX:
            BuildInternalIntDef(tree);
            setInternalRegsDelayFree = true;
            // The lifetime of the arrObj must be extended because it is
            // used multiple times while the result is being computed.
            setDelayFree(BuildUse(tree->AsArrIndex()->ArrObj()));
            BuildUse(tree->AsArrIndex()->IndexExpr());
            BuildInternalUses();
            BuildDef(tree);
            break;

        case GT_ARR_OFFSET:
            if (!tree->AsArrOffs()->GetOp(0)->isContained())
            {
                // Here we simply need an internal register, which must be different
                // from any of the operand's registers, but may be the same as targetReg.
                BuildInternalIntDef(tree);
                BuildUse(tree->AsArrOffs()->GetOp(0));
            }

            BuildUse(tree->AsArrOffs()->GetOp(1));
            BuildUse(tree->AsArrOffs()->GetOp(2));
            BuildInternalUses();
            BuildDef(tree);
            break;

        case GT_LEA:
            BuildAddrMode(tree->AsAddrMode());
            break;

        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GE:
        case GT_GT:
        case GT_CMP:
            BuildCmp(tree->AsOp());
            break;

        case GT_CALL:
            BuildCall(tree->AsCall());
            break;

        case GT_STORE_BLK:
        case GT_STORE_OBJ:
            BuildStructStore(tree->AsBlk(), tree->AsBlk()->GetKind(), tree->AsBlk()->GetLayout());
            break;

        case GT_COPY_BLK:
        case GT_INIT_BLK:
            BuildStoreDynBlk(tree->AsDynBlk());
            break;

        case GT_LCLHEAP:
            BuildLclHeap(tree->AsUnOp());
            break;

        case GT_STOREIND:
            if (GCInfo::GetWriteBarrierForm(tree->AsStoreInd()) != GCInfo::WBF_NoBarrier)
            {
                BuildGCWriteBarrier(tree->AsStoreInd());
            }
            else
            {
                BuildIndir(tree->AsStoreInd());
                BuildUse(tree->AsStoreInd()->GetValue());
            }
            break;

        case GT_IND:
            BuildIndir(tree->AsIndir());
            break;

        case GT_CATCH_ARG:
            BuildDef(tree, RBM_EXCEPTION_OBJECT);
            break;

        case GT_PUTARG_SPLIT:
            BuildPutArgSplit(tree->AsPutArgSplit());
            break;

        case GT_PUTARG_STK:
            BuildPutArgStk(tree->AsPutArgStk());
            break;

        case GT_PUTARG_REG:
            BuildPutArgReg(tree->AsUnOp());
            break;

        case GT_BITCAST:
        {
            if (!tree->AsUnOp()->GetOp(0)->isContained())
            {
                BuildUse(tree->AsUnOp()->GetOp(0));
            }

            regNumber argReg  = tree->GetRegNum(0);
            regMaskTP argMask = argReg == REG_NA ? RBM_NONE : genRegMask(argReg);

            if (tree->TypeIs(TYP_LONG))
            {
                // TODO-MIKE-Cleanup: This should probably use tree->GetRegNum(1) instead of REG_NEXT
                // to be on the safe side. REG_NEXT happens to work because such BITCAST nodes are
                // used only as call args so the registers are consecutive.
                regMaskTP argMaskNext = argReg == REG_NA ? RBM_NONE : genRegMask(REG_NEXT(argReg));

                BuildDef(tree, TYP_INT, argMask, 0);
                BuildDef(tree, TYP_INT, argMaskNext, 1);
            }
            else
            {
                BuildDef(tree, argMask);
            }
        }
        break;

        case GT_INSTR:
            BuildInstr(tree->AsInstr());
            break;

        default:
            unreached();
    }
}

void LinearScan::BuildAddrMode(GenTreeAddrMode* lea)
{
    if (GenTree* base = lea->GetBase())
    {
        BuildUse(base);
    }

    if (GenTree* index = lea->GetIndex())
    {
        BuildUse(index);
    }

    if (lea->GetBase() != nullptr)
    {
        if (((lea->GetIndex() != nullptr) && (lea->GetOffset() != 0)) ||
            !emitter::emitIns_valid_imm_for_add(lea->GetOffset(), INS_FLAGS_DONT_CARE))
        {
            BuildInternalIntDef(lea);
            BuildInternalUses();
        }
    }

    BuildDef(lea);
}

void LinearScan::BuildLclHeap(GenTreeUnOp* tree)
{
    // Need a variable number of temp regs (see genLclHeap() in codegenarm.cpp):
    // Here '-' means don't care.
    //
    //  Size?                   Init Memory?    # temp regs
    //   0                          -               0
    //   const and <=4 str instr    -               0
    //   const and <PageSize        No              0
    //   >4 ptr words               Yes             1
    //   Non-const                  Yes             1
    //   Non-const                  No              1
    //
    // If the outgoing argument space is too large to encode in an "add/sub sp, icon"
    // instruction, we also need a temp (we can use the same temp register needed
    // for the other cases above, if there are multiple conditions that require a
    // temp register).

    GenTree* size = tree->gtGetOp1();
    int      internalIntCount;
    if (size->IsCnsIntOrI())
    {
        assert(size->isContained());

        size_t sizeVal = size->AsIntCon()->gtIconVal;
        if (sizeVal == 0)
        {
            internalIntCount = 0;
        }
        else
        {
            sizeVal          = AlignUp(sizeVal, STACK_ALIGN);
            size_t pushCount = sizeVal / REGSIZE_BYTES;

            // For small allocations we use up to 4 push instructions
            if (pushCount <= 4)
            {
                internalIntCount = 0;
            }
            else if (!compiler->info.compInitMem)
            {
                // No need to initialize allocated stack space.
                if (sizeVal < compiler->eeGetPageSize())
                {
                    internalIntCount = 0;
                }
                else
                {
                    internalIntCount = 1;
                }
            }
            else
            {
                internalIntCount = 1;
            }
        }
    }
    else
    {
        // target (regCnt) + tmp
        internalIntCount = 1;
        BuildUse(size);
    }

    // If we have an outgoing argument space, we are going to probe that SP change, and we require
    // a temporary register for doing the probe. Note also that if the outgoing argument space is
    // large enough that it can't be directly encoded in SUB/ADD instructions, we also need a temp
    // register to load the large sized constant into a register.
    if (compiler->codeGen->outgoingArgSpaceSize > 0)
    {
        internalIntCount = 1;
    }

    // If we are needed in temporary registers we should be sure that
    // it's different from target (regCnt)
    if (internalIntCount > 0)
    {
        setInternalRegsDelayFree = true;
        for (int i = 0; i < internalIntCount; i++)
        {
            BuildInternalIntDef(tree);
        }
    }

    BuildInternalUses();
    BuildDef(tree);
}

void LinearScan::BuildShiftLong(GenTreeOp* node)
{
    assert(node->OperIs(GT_LSH_HI, GT_RSH_LO));

    GenTreeOp* source = node->GetOp(0)->AsOp();
    assert(source->OperIs(GT_LONG) && source->isContained());
    GenTree* sourceLo = source->GetOp(0);
    GenTree* sourceHi = source->GetOp(1);

    GenTree* shiftBy = node->GetOp(1);
    assert(shiftBy->IsContainedIntCon());

    RefPosition* sourceLoUse = BuildUse(sourceLo);
    RefPosition* sourceHiUse = BuildUse(sourceHi);
    setDelayFree(node->OperIs(GT_LSH_HI) ? sourceLoUse : sourceHiUse);
    BuildDef(node);
}

#endif // TARGET_ARM
