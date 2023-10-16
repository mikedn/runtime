// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                     Register Requirements for ARM                         XX
XX                                                                           XX
XX  This encapsulates all the logic for setting register requirements for    XX
XX  the ARM  architecture.                                                   XX
XX                                                                           XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"

#ifdef TARGET_ARM

#include "sideeffects.h"
#include "lower.h"
#include "lsra.h"

int LinearScan::BuildLclHeap(GenTree* tree)
{
    int srcCount = 0;

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
        srcCount = 0;

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
        srcCount         = 1;
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
            buildInternalIntRegisterDefForNode(tree);
        }
    }
    buildInternalRegisterUses();
    BuildDef(tree);
    return srcCount;
}

//------------------------------------------------------------------------
// BuildShiftLongCarry: Set the node info for GT_LSH_HI or GT_RSH_LO.
//
// Arguments:
//    tree      - The node of interest
//
// Return Value:
//    The number of sources consumed by this node.
//
// Note: these operands have uses that interfere with the def and need the special handling.
//
int LinearScan::BuildShiftLongCarry(GenTree* tree)
{
    assert(tree->OperGet() == GT_LSH_HI || tree->OperGet() == GT_RSH_LO);

    int      srcCount = 2;
    GenTree* source   = tree->AsOp()->gtOp1;
    assert((source->OperGet() == GT_LONG) && source->isContained());

    GenTree* sourceLo = source->gtGetOp1();
    GenTree* sourceHi = source->gtGetOp2();
    GenTree* shiftBy  = tree->gtGetOp2();
    assert(!sourceLo->isContained() && !sourceHi->isContained());
    RefPosition* sourceLoUse = BuildUse(sourceLo);
    RefPosition* sourceHiUse = BuildUse(sourceHi);

    if (!tree->isContained())
    {
        if (tree->OperGet() == GT_LSH_HI)
        {
            setDelayFree(sourceLoUse);
        }
        else
        {
            setDelayFree(sourceHiUse);
        }
        if (!shiftBy->isContained())
        {
            BuildUse(shiftBy);
            srcCount++;
        }
        BuildDef(tree);
    }
    else
    {
        if (!shiftBy->isContained())
        {
            BuildUse(shiftBy);
            srcCount++;
        }
    }
    return srcCount;
}

//------------------------------------------------------------------------
// BuildNode: Build the RefPositions for for a node
//
// Arguments:
//    treeNode - the node of interest
//
// Return Value:
//    The number of sources consumed by this node.
//
// Notes:
// Preconditions:
//    LSRA Has been initialized.
//
// Postconditions:
//    RefPositions have been built for all the register defs and uses required
//    for this node.
//
void LinearScan::BuildNode(GenTree* tree)
{
    assert(!tree->isContained());
    int  srcCount;
    int  dstCount      = 0;
    bool isLocalDefUse = false;

    // Reset the build-related members of LinearScan.
    clearBuildState();

    // Set the default dstCount. This may be modified below.
    if (tree->IsValue())
    {
        dstCount = 1;
        if (tree->IsUnusedValue())
        {
            isLocalDefUse = true;
        }
    }
    else
    {
        dstCount = 0;
    }

    switch (tree->OperGet())
    {
        case GT_LCL_VAR:
            // We make a final determination about whether a GT_LCL_VAR is a candidate or contained
            // after liveness. In either case we don't build any uses or defs. Otherwise, this is a
            // load of a stack-based local into a register and we'll fall through to the general
            // local case below.
            if (checkContainedOrCandidateLclVar(tree->AsLclVar()))
            {
                return;
            }
            FALLTHROUGH;

        case GT_LCL_FLD:
        {
            if (tree->OperIs(GT_LCL_FLD) && tree->AsLclFld()->IsOffsetMisaligned())
            {
                buildInternalIntRegisterDefForNode(tree); // to generate address.
                buildInternalIntRegisterDefForNode(tree); // to move float into an int reg.
                if (tree->TypeIs(TYP_DOUBLE))
                {
                    buildInternalIntRegisterDefForNode(tree); // to move the second half into an int reg.
                }
                buildInternalRegisterUses();
            }

            srcCount = 0;
            BuildDef(tree);
        }
        break;

        case GT_STORE_LCL_VAR:
            srcCount = BuildStoreLclVar(tree->AsLclVar(), &dstCount);
            break;

        case GT_STORE_LCL_FLD:
            srcCount = BuildStoreLclFld(tree->AsLclFld());
            break;

        case GT_NOP:
            // A GT_NOP is either a passthrough (if it is void, or if it has
            // a child), but must be considered to produce a dummy value if it
            // has a type but no child
            srcCount = 0;
            assert((tree->gtGetOp1() == nullptr) || tree->isContained());
            if (tree->TypeGet() != TYP_VOID && tree->gtGetOp1() == nullptr)
            {
                assert(dstCount == 1);
                BuildUse(tree->gtGetOp1());
                BuildDef(tree);
            }
            else
            {
                assert(dstCount == 0);
            }
            break;

        case GT_KEEPALIVE:
            assert(dstCount == 0);
            srcCount = BuildOperandUses(tree->gtGetOp1());
            break;

        case GT_INTRINSIC:
        {
            // TODO-ARM: Implement other type of intrinsics (round, sqrt and etc.)
            // Both operand and its result must be of the same floating point type.
            GenTree* op1 = tree->gtGetOp1();
            assert(varTypeIsFloating(op1));
            assert(op1->TypeGet() == tree->TypeGet());
            BuildUse(op1);
            srcCount = 1;

            switch (tree->AsIntrinsic()->GetIntrinsic())
            {
                case NI_System_Math_Abs:
                case NI_System_Math_Sqrt:
                    assert(dstCount == 1);
                    BuildDef(tree);
                    break;
                default:
                    unreached();
                    break;
            }
        }
        break;

        case GT_CAST:
            assert(dstCount == 1);
            srcCount = BuildCast(tree->AsCast());
            break;

        case GT_JTRUE:
            srcCount = 0;
            assert(dstCount == 0);
            break;

        case GT_JMP:
            srcCount = 0;
            assert(dstCount == 0);
            break;

        case GT_SWITCH:
            // This should never occur since switch nodes must not be visible at this
            // point in the JIT.
            srcCount = 0;
            noway_assert(!"Switch must be lowered at this point");
            break;

        case GT_JMPTABLE:
            srcCount = 0;
            assert(dstCount == 1);
            BuildDef(tree);
            break;

        case GT_SWITCH_TABLE:
            assert(dstCount == 0);
            srcCount = BuildBinaryUses(tree->AsOp());
            assert(srcCount == 2);
            break;

        case GT_FADD:
        case GT_FSUB:
        case GT_FMUL:
        case GT_FDIV:
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
            assert(dstCount == 1);
            srcCount = BuildBinaryUses(tree->AsOp());
            assert(srcCount == (tree->AsOp()->GetOp(1)->isContained() ? 1 : 2));
            BuildDef(tree);
            break;

        case GT_LSH_HI:
        case GT_RSH_LO:
            assert(dstCount == 1);
            srcCount = BuildShiftLongCarry(tree);
            assert(srcCount == (tree->AsOp()->gtOp2->isContained() ? 2 : 3));
            break;

        case GT_RETURNTRAP:
            // this just turns into a compare of its child with an int
            // + a conditional call
            srcCount = 1;
            assert(dstCount == 0);
            BuildUse(tree->gtGetOp1());
            BuildKills(tree, compiler->compHelperCallKillSet(CORINFO_HELP_STOP_FOR_GC));
            break;

        case GT_MUL:
            if (tree->gtOverflow())
            {
                // Need a register different from target reg to check for overflow.
                setInternalRegsDelayFree = true;
                buildInternalIntRegisterDefForNode(tree);
            }
            FALLTHROUGH;
        case GT_DIV:
        case GT_MULHI:
        case GT_UDIV:
            assert(dstCount == 1);
            srcCount = BuildBinaryUses(tree->AsOp());
            assert(srcCount == 2);
            buildInternalRegisterUses();
            BuildDef(tree);
            break;

        case GT_MUL_LONG:
            dstCount = 2;
            srcCount = BuildBinaryUses(tree->AsOp());
            assert(srcCount == 2);
            BuildDef(tree, TYP_INT, RBM_NONE, 0);
            BuildDef(tree, TYP_INT, RBM_NONE, 1);
            break;

        case GT_NO_OP:
        case GT_START_NONGC:
        case GT_PROF_HOOK:
            srcCount = 0;
            assert(dstCount == 0);
            break;

        case GT_START_PREEMPTGC:
            // This kills GC refs in callee save regs
            srcCount = 0;
            assert(dstCount == 0);
            BuildKills(tree, RBM_NONE);
            break;

        case GT_LONG:
            assert(tree->IsUnusedValue()); // Contained nodes are already processed, only unused GT_LONG can reach here.
                                           // An unused GT_LONG doesn't produce any registers.
            tree->gtType = TYP_VOID;
            tree->ClearUnusedValue();
            isLocalDefUse = false;

            // An unused GT_LONG node needs to consume its sources, but need not produce a register.
            srcCount = 2;
            dstCount = 0;
            BuildUse(tree->gtGetOp1());
            BuildUse(tree->gtGetOp2());
            break;

        case GT_CNS_DBL:
            if (tree->TypeGet() == TYP_FLOAT)
            {
                // An int register for float constant
                buildInternalIntRegisterDefForNode(tree);
            }
            else
            {
                // TYP_DOUBLE
                assert(tree->TypeGet() == TYP_DOUBLE);

                // Two int registers for double constant
                buildInternalIntRegisterDefForNode(tree);
                buildInternalIntRegisterDefForNode(tree);
            }
            FALLTHROUGH;

        case GT_CNS_INT:
        {
            srcCount = 0;
            assert(dstCount == 1);
            buildInternalRegisterUses();
            RefPosition* def               = BuildDef(tree);
            def->getInterval()->isConstant = true;
        }
        break;

        case GT_RETURN:
            srcCount = BuildReturn(tree->AsUnOp());
            BuildKills(tree, getKillSetForReturn());
            break;

        case GT_RETFILT:
            assert(dstCount == 0);
            if (tree->TypeGet() == TYP_VOID)
            {
                srcCount = 0;
            }
            else
            {
                assert(tree->TypeGet() == TYP_INT);
                srcCount = 1;
                BuildUse(tree->gtGetOp1(), RBM_INTRET);
            }
            break;

        case GT_BOUNDS_CHECK:
            srcCount = 2;
            assert(dstCount == 0);
            BuildUse(tree->AsBoundsChk()->GetOp(0));
            BuildUse(tree->AsBoundsChk()->GetOp(1));
            break;

        case GT_ARR_ELEM:
            // These must have been lowered to GT_ARR_INDEX
            noway_assert(!"We should never see a GT_ARR_ELEM in lowering");
            srcCount = 0;
            assert(dstCount == 0);
            break;

        case GT_ARR_INDEX:
        {
            srcCount = 2;
            assert(dstCount == 1);
            buildInternalIntRegisterDefForNode(tree);
            setInternalRegsDelayFree = true;

            // For GT_ARR_INDEX, the lifetime of the arrObj must be extended because it is actually used multiple
            // times while the result is being computed.
            RefPosition* arrObjUse = BuildUse(tree->AsArrIndex()->ArrObj());
            setDelayFree(arrObjUse);
            BuildUse(tree->AsArrIndex()->IndexExpr());
            buildInternalRegisterUses();
            BuildDef(tree);
        }
        break;

        case GT_ARR_OFFSET:

            // This consumes the offset, if any, the arrObj and the effective index,
            // and produces the flattened offset for this dimension.
            assert(dstCount == 1);

            if (tree->AsArrOffs()->GetOp(0)->isContained())
            {
                srcCount = 2;
            }
            else
            {
                // Here we simply need an internal register, which must be different
                // from any of the operand's registers, but may be the same as targetReg.
                buildInternalIntRegisterDefForNode(tree);
                srcCount = 3;
                BuildUse(tree->AsArrOffs()->GetOp(0));
            }
            BuildUse(tree->AsArrOffs()->GetOp(1));
            BuildUse(tree->AsArrOffs()->GetOp(2));
            buildInternalRegisterUses();
            BuildDef(tree);
            break;

        case GT_LEA:
        {
            GenTreeAddrMode* lea = tree->AsAddrMode();

            // This LEA is instantiating an address, so we set up the srcCount and dstCount here.
            srcCount = 0;
            assert(dstCount == 1);
            if (GenTree* base = lea->GetBase())
            {
                srcCount++;
                BuildUse(base);
            }
            if (GenTree* index = lea->GetIndex())
            {
                srcCount++;
                BuildUse(index);
            }

            int offset = lea->GetOffset();

            // An internal register may be needed too; the logic here should be in sync with the
            // genLeaInstruction()'s requirements for a such register.
            if (lea->HasBase() && lea->HasIndex())
            {
                if (offset != 0)
                {
                    // We need a register when we have all three: base reg, index reg and a non-zero offset.
                    buildInternalIntRegisterDefForNode(tree);
                }
            }
            else if (lea->HasBase())
            {
                if (!emitter::emitIns_valid_imm_for_add(offset, INS_FLAGS_DONT_CARE))
                {
                    // We need a register when we have an offset that is too large to encode in the add instruction.
                    buildInternalIntRegisterDefForNode(tree);
                }
            }
            buildInternalRegisterUses();
            BuildDef(tree);
        }
        break;

        case GT_FNEG:
        case GT_NEG:
        case GT_NOT:
            srcCount = 1;
            assert(dstCount == 1);
            BuildUse(tree->gtGetOp1());
            BuildDef(tree);
            break;

        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GE:
        case GT_GT:
        case GT_CMP:
            srcCount = BuildCmp(tree->AsOp());
            break;

        case GT_CKFINITE:
            srcCount = 1;
            assert(dstCount == 1);
            buildInternalIntRegisterDefForNode(tree);
            BuildUse(tree->gtGetOp1());
            buildInternalRegisterUses();
            BuildDef(tree);
            break;

        case GT_CALL:
            srcCount = BuildCall(tree->AsCall());
            if (tree->AsCall()->HasMultiRegRetVal())
            {
                dstCount = tree->AsCall()->GetRegCount();
            }
            break;

        case GT_STORE_BLK:
        case GT_STORE_OBJ:
            srcCount = BuildStructStore(tree->AsBlk(), tree->AsBlk()->GetKind(), tree->AsBlk()->GetLayout());
            break;

        case GT_COPY_BLK:
        case GT_INIT_BLK:
            srcCount = BuildStoreDynBlk(tree->AsDynBlk());
            break;

        case GT_LCLHEAP:
            srcCount = BuildLclHeap(tree);
            break;

        case GT_STOREIND:
            assert(dstCount == 0);

            if (GCInfo::GetWriteBarrierForm(tree->AsStoreInd()) != GCInfo::WBF_NoBarrier)
            {
                srcCount = BuildGCWriteBarrier(tree->AsStoreInd());
            }
            else
            {
                srcCount = BuildIndir(tree->AsStoreInd());
                BuildUse(tree->AsStoreInd()->GetValue());
                srcCount++;
            }
            break;

        case GT_NULLCHECK:
#ifdef TARGET_ARM
            // On Arm32 we never want to use GT_NULLCHECK, as we require a target register.
            // Previously we used an internal register for this, but that results in a lifetime
            // that overlaps with all the source registers.
            assert(!"Should never see GT_NULLCHECK on Arm/32");
#endif
            // For Arm64 we simply fall through to the GT_IND case, and will use REG_ZR as the target.
            FALLTHROUGH;
        case GT_IND:
            assert(dstCount == (tree->OperIs(GT_NULLCHECK) ? 0 : 1));
            srcCount = BuildIndir(tree->AsIndir());
            break;

        case GT_CATCH_ARG:
            srcCount = 0;
            assert(dstCount == 1);
            BuildDef(tree, RBM_EXCEPTION_OBJECT);
            break;

        case GT_PUTARG_SPLIT:
            srcCount = BuildPutArgSplit(tree->AsPutArgSplit());
            dstCount = tree->AsPutArgSplit()->GetRegCount();
            break;

        case GT_PUTARG_STK:
            srcCount = BuildPutArgStk(tree->AsPutArgStk());
            break;

        case GT_PUTARG_REG:
            srcCount = BuildPutArgReg(tree->AsUnOp());
            dstCount = tree->TypeIs(TYP_LONG) ? 2 : 1;
            break;

        case GT_BITCAST:
        {
            if (!tree->AsUnOp()->GetOp(0)->isContained())
            {
                BuildUse(tree->AsUnOp()->GetOp(0));
                srcCount = 1;
            }
            else
            {
                srcCount = 0;
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

                dstCount = 2;
            }
            else
            {
                BuildDef(tree, argMask);

                assert(dstCount == 1);
            }
        }
        break;

        case GT_LCL_ADDR:
        case GT_PHYSREG:
        case GT_CLS_VAR_ADDR:
        case GT_LABEL:
        case GT_PINVOKE_PROLOG:
        case GT_JCC:
        case GT_SETCC:
        case GT_MEMORYBARRIER:
        case GT_OBJ:
            srcCount = BuildSimple(tree);
            break;

        case GT_INDEX_ADDR:
            dstCount = 1;
            buildInternalIntRegisterDefForNode(tree);
            srcCount = BuildBinaryUses(tree->AsOp());
            assert(srcCount == 2);
            buildInternalRegisterUses();
            BuildDef(tree);
            break;

        case GT_INSTR:
            srcCount = BuildInstr(tree->AsInstr());
            break;

        case GT_IL_OFFSET:
            srcCount = 0;
            break;

        case GT_ARGPLACE:
        case GT_ASG:
        case GT_BLK:
        case GT_FIELD_LIST:
        case GT_INIT_VAL:
        case GT_BOX:
        case GT_COMMA:
        case GT_QMARK:
            unreached();

        default:
#ifdef DEBUG
            char message[256];
            _snprintf_s(message, _countof(message), _TRUNCATE, "NYI: Unimplemented node type %s",
                        GenTree::OpName(tree->OperGet()));
            NYIRAW(message);
#endif
            unreached();
    } // end switch (tree->OperGet())

    // We need to be sure that we've set srcCount and dstCount appropriately
    assert((dstCount < 2) || tree->IsMultiRegNode());
    assert(isLocalDefUse == (tree->IsValue() && tree->IsUnusedValue()));
    assert(!tree->IsUnusedValue() || (dstCount != 0));
    assert(dstCount == static_cast<int>(tree->GetRegisterDstCount(compiler)));
}

#endif // TARGET_ARM
