// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                               Lower                                       XX
XX                                                                           XX
XX  Preconditions:                                                           XX
XX                                                                           XX
XX  Postconditions (for the nodes currently handled):                        XX
XX    - All operands requiring a register are explicit in the graph          XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#include "lower.h"

#if !defined(TARGET_64BIT)
#include "decomposelongs.h"
#endif // !defined(TARGET_64BIT)

//------------------------------------------------------------------------
// MakeSrcContained: Make "childNode" a contained node
//
// Arguments:
//    parentNode - is a non-leaf node that can contain its 'childNode'
//    childNode  - is an op that will now be contained by its parent.
//
// Notes:
//    If 'childNode' it has any existing sources, they will now be sources for the parent.
//
void Lowering::MakeSrcContained(GenTree* parentNode, GenTree* childNode) const
{
    assert(!parentNode->OperIsLeaf());
    assert(childNode->canBeContained());
    childNode->SetContained();
    assert(childNode->isContained());
}

//------------------------------------------------------------------------
// CheckImmedAndMakeContained: Checks if the 'childNode' is a containable immediate
//    and, if so, makes it contained.
//
// Arguments:
//    parentNode - is any non-leaf node
//    childNode  - is an child op of 'parentNode'
//
// Return value:
//     true if we are able to make childNode a contained immediate
//
bool Lowering::CheckImmedAndMakeContained(GenTree* parentNode, GenTree* childNode)
{
    assert(!parentNode->OperIsLeaf());
    // If childNode is a containable immediate
    if (IsContainableImmed(parentNode, childNode))
    {
        // then make it contained within the parentNode
        MakeSrcContained(parentNode, childNode);
        return true;
    }
    return false;
}

//------------------------------------------------------------------------
// IsSafeToContainMem: Checks for conflicts between childNode and parentNode,
// and returns 'true' iff memory operand childNode can be contained in parentNode.
//
// Arguments:
//    parentNode - any non-leaf node
//    childNode  - some node that is an input to `parentNode`
//
// Return value:
//    true if it is safe to make childNode a contained memory operand.
//
bool Lowering::IsSafeToContainMem(GenTree* parentNode, GenTree* childNode)
{
    m_scratchSideEffects.Clear();
    m_scratchSideEffects.AddNode(comp, childNode);

    for (GenTree* node = childNode->gtNext; node != parentNode; node = node->gtNext)
    {
        const bool strict = true;
        if (m_scratchSideEffects.InterferesWith(comp, node, strict))
        {
            return false;
        }
    }

    return true;
}

//------------------------------------------------------------------------
// LowerNode: this is the main entry point for Lowering.
//
// Arguments:
//    node - the node we are lowering.
//
// Returns:
//    next node in the transformed node sequence that needs to be lowered.
//
GenTree* Lowering::LowerNode(GenTree* node)
{
    assert(node != nullptr);
    switch (node->gtOper)
    {
        case GT_NULLCHECK:
        case GT_IND:
            LowerIndir(node->AsIndir());
            break;

        case GT_STOREIND:
            LowerStoreIndirCommon(node->AsStoreInd());
            break;

#ifdef TARGET_ARM64
        case GT_NOT:
            LowerNot(node->AsUnOp());
            break;

        case GT_AND:
        case GT_OR:
        case GT_XOR:
            LowerLogical(node->AsOp());
            break;

        case GT_NEG:
            LowerNegate(node->AsUnOp());
            break;

        case GT_ADD:
        case GT_SUB:
            LowerArithmetic(node->AsOp());
            break;

        case GT_MUL:
        case GT_MULHI:
            LowerMultiply(node->AsOp());
            break;

        case GT_LT:
        case GT_LE:
        case GT_GT:
        case GT_GE:
        case GT_EQ:
        case GT_NE:
        case GT_TEST_EQ:
        case GT_TEST_NE:
        case GT_CMP:
            return LowerRelop(node->AsOp());
#else // TARGET_ARM64
        case GT_ADD:
        {
            GenTree* next = LowerAdd(node->AsOp());
            if (next != nullptr)
            {
                return next;
            }
        }
        break;

#if !defined(TARGET_64BIT)
        case GT_ADD_LO:
        case GT_ADD_HI:
        case GT_SUB_LO:
        case GT_SUB_HI:
#endif
        case GT_SUB:
        case GT_AND:
        case GT_OR:
        case GT_XOR:
            ContainCheckBinary(node->AsOp());
            break;

        case GT_MUL:
        case GT_MULHI:
#if defined(TARGET_X86)
        case GT_MUL_LONG:
#endif
            ContainCheckMul(node->AsOp());
            break;

        case GT_LT:
        case GT_LE:
        case GT_GT:
        case GT_GE:
        case GT_EQ:
        case GT_NE:
        case GT_TEST_EQ:
        case GT_TEST_NE:
        case GT_CMP:
            return LowerCompare(node);
#endif // !TARGET_ARM64

        case GT_UDIV:
        case GT_UMOD:
            if (!LowerUnsignedDivOrMod(node->AsOp()))
            {
                ContainCheckDivOrMod(node->AsOp());
            }
            break;

        case GT_DIV:
        case GT_MOD:
            return LowerSignedDivOrMod(node);

        case GT_SWITCH:
            return LowerSwitch(node->AsUnOp());

        case GT_CALL:
            LowerCall(node);
            break;

        case GT_JTRUE:
            return LowerJTrue(node->AsUnOp());

        case GT_JMP:
            LowerJmpMethod(node);
            break;

        case GT_RETURN:
            LowerRet(node->AsUnOp());
            break;

        case GT_RETURNTRAP:
            ContainCheckReturnTrap(node->AsOp());
            break;

        case GT_BITCAST:
            return LowerBitCast(node->AsUnOp());

        case GT_CAST:
            return LowerCast(node->AsCast());

#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
        case GT_ARR_BOUNDS_CHECK:
#ifdef FEATURE_HW_INTRINSICS
        case GT_HW_INTRINSIC_CHK:
#endif
            ContainCheckBoundsChk(node->AsBoundsChk());
            break;
#endif

        case GT_ARR_ELEM:
            return LowerArrElem(node);

        case GT_ARR_OFFSET:
            ContainCheckArrOffset(node->AsArrOffs());
            break;

        case GT_ROL:
        case GT_ROR:
            LowerRotate(node);
            break;

#ifndef TARGET_64BIT
        case GT_LSH_HI:
        case GT_RSH_LO:
            ContainCheckShiftRotate(node->AsOp());
            break;
#endif // !TARGET_64BIT

        case GT_LSH:
        case GT_RSH:
        case GT_RSZ:
            LowerShift(node->AsOp());
            break;

        case GT_STORE_OBJ:
            if (node->AsObj()->GetValue()->IsCall())
            {
                LowerStoreSingleRegCallStruct(node->AsObj());
                break;
            }

            LowerStoreObj(node->AsObj());
            break;

        case GT_STORE_BLK:
            LowerStoreBlk(node->AsBlk());
            break;

        case GT_STORE_DYN_BLK:
            LowerStoreDynBlk(node->AsDynBlk());
            break;

        case GT_LCLHEAP:
            ContainCheckLclHeap(node->AsOp());
            break;

#ifdef TARGET_XARCH
        case GT_INTRINSIC:
            ContainCheckIntrinsic(node->AsOp());
            break;
#endif // TARGET_XARCH

#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            LowerHWIntrinsic(node->AsHWIntrinsic());
            break;
#endif // FEATURE_HW_INTRINSICS

        case GT_LCL_FLD:
            LowerLclFld(node->AsLclFld());
            break;

        case GT_LCL_VAR:
            WidenSIMD12IfNecessary(node->AsLclVar());
            LowerLclVar(node->AsLclVar());
            break;

        case GT_STORE_LCL_VAR:
            WidenSIMD12IfNecessary(node->AsLclVar());
            LowerStoreLclVar(node->AsLclVar());
            break;

        case GT_STORE_LCL_FLD:
            LowerStoreLclFld(node->AsLclFld());
            break;

#if defined(TARGET_ARM64)
        case GT_CMPXCHG:
            CheckImmedAndMakeContained(node, node->AsCmpXchg()->gtOpComparand);
            break;

        case GT_XORR:
        case GT_XAND:
        case GT_XADD:
            CheckImmedAndMakeContained(node, node->AsOp()->gtOp2);
            break;
#elif defined(TARGET_XARCH)
        case GT_XORR:
        case GT_XAND:
        case GT_XADD:
            if (node->IsUnusedValue())
            {
                node->ClearUnusedValue();
                // Make sure the types are identical, since the node type is changed to VOID
                // CodeGen relies on op2's type to determine the instruction size.
                // Note that the node type cannot be a small int but the data operand can.
                assert(genActualType(node->gtGetOp2()->TypeGet()) == node->TypeGet());
                node->SetOper(GT_LOCKADD);
                node->gtType = TYP_VOID;
                CheckImmedAndMakeContained(node, node->gtGetOp2());
            }
            break;
#endif

        case GT_KEEPALIVE:
            node->gtGetOp1()->SetRegOptional();
            break;

        default:
            break;
    }

    return node->gtNext;
}

/**  -- Switch Lowering --
 * The main idea of switch lowering is to keep transparency of the register requirements of this node
 * downstream in LSRA.  Given that the switch instruction is inherently a control statement which in the JIT
 * is represented as a simple tree node, at the time we actually generate code for it we end up
 * generating instructions that actually modify the flow of execution that imposes complicated
 * register requirement and lifetimes.
 *
 * So, for the purpose of LSRA, we want to have a more detailed specification of what a switch node actually
 * means and more importantly, which and when do we need a register for each instruction we want to issue
 * to correctly allocate them downstream.
 *
 * For this purpose, this procedure performs switch lowering in two different ways:
 *
 * a) Represent the switch statement as a zero-index jump table construct.  This means that for every destination
 *    of the switch, we will store this destination in an array of addresses and the code generator will issue
 *    a data section where this array will live and will emit code that based on the switch index, will indirect and
 *    jump to the destination specified in the jump table.
 *
 *    For this transformation we introduce a new GT node called GT_SWITCH_TABLE that is a specialization of the switch
 *    node for jump table based switches.
 *    The overall structure of a GT_SWITCH_TABLE is:
 *
 *    GT_SWITCH_TABLE
 *           |_________ localVar   (a temporary local that holds the switch index)
 *           |_________ jumpTable  (this is a special node that holds the address of the jump table array)
 *
 *     Now, the way we morph a GT_SWITCH node into this lowered switch table node form is the following:
 *
 *    Input:     GT_SWITCH (inside a basic block whose Branch Type is BBJ_SWITCH)
 *                    |_____ expr (an arbitrarily complex GT_NODE that represents the switch index)
 *
 *    This gets transformed into the following statements inside a BBJ_COND basic block (the target would be
 *    the default case of the switch in case the conditional is evaluated to true).
 *
 *     ----- original block, transformed
 *     GT_STORE_LCL_VAR tempLocal (a new temporary local variable used to store the switch index)
 *        |_____ expr      (the index expression)
 *
 *     GT_JTRUE
 *        |_____ GT_COND
 *                 |_____ GT_GE
 *                           |___ Int_Constant  (This constant is the index of the default case
 *                                               that happens to be the highest index in the jump table).
 *                           |___ tempLocal     (The local variable were we stored the index expression).
 *
 *     ----- new basic block
 *     GT_SWITCH_TABLE
 *        |_____ tempLocal
 *        |_____ jumpTable (a new jump table node that now LSRA can allocate registers for explicitly
 *                          and LinearCodeGen will be responsible to generate downstream).
 *
 *     This way there are no implicit temporaries.
 *
 * b) For small-sized switches, we will actually morph them into a series of conditionals of the form
 *     if (case falls into the default){ goto jumpTable[size]; // last entry in the jump table is the default case }
 *     (For the default case conditional, we'll be constructing the exact same code as the jump table case one).
 *     else if (case == firstCase){ goto jumpTable[1]; }
 *     else if (case == secondCase) { goto jumptable[2]; } and so on.
 *
 *     This transformation is of course made in JIT-IR, not downstream to CodeGen level, so this way we no longer
 *     require internal temporaries to maintain the index we're evaluating plus we're using existing code from
 *     LinearCodeGen to implement this instead of implement all the control flow constructs using InstrDscs and
 *     InstrGroups downstream.
 */

GenTree* Lowering::LowerSwitch(GenTreeUnOp* node)
{
    assert(node->OperIs(GT_SWITCH));

    unsigned     jumpCnt;
    unsigned     targetCnt;
    BasicBlock** jumpTab;

    // The first step is to build the default case conditional construct that is
    // shared between both kinds of expansion of the switch node.

    // To avoid confusion, we'll alias m_block to originalSwitchBB
    // that represents the node we're morphing.
    BasicBlock* originalSwitchBB = m_block;
    LIR::Range& switchBBRange    = LIR::AsRange(originalSwitchBB);

    // jumpCnt is the number of elements in the jump table array.
    // jumpTab is the actual pointer to the jump table array.
    // targetCnt is the number of unique targets in the jump table array.
    jumpCnt   = originalSwitchBB->bbJumpSwt->bbsCount;
    jumpTab   = originalSwitchBB->bbJumpSwt->bbsDstTab;
    targetCnt = originalSwitchBB->NumSucc(comp);

// GT_SWITCH must be a top-level node with no use.
#ifdef DEBUG
    {
        LIR::Use use;
        assert(!switchBBRange.TryGetUse(node, &use));
    }
#endif

    JITDUMP("Lowering switch " FMT_BB ", %d cases\n", originalSwitchBB->bbNum, jumpCnt);

    // Handle a degenerate case: if the switch has only a default case, just convert it
    // to an unconditional branch. This should only happen in minopts or with debuggable
    // code.
    if (targetCnt == 1)
    {
        JITDUMP("Lowering switch " FMT_BB ": single target; converting to BBJ_ALWAYS\n", originalSwitchBB->bbNum);
        noway_assert(comp->opts.OptimizationDisabled());
        if (originalSwitchBB->bbNext == jumpTab[0])
        {
            originalSwitchBB->bbJumpKind = BBJ_NONE;
            originalSwitchBB->bbJumpDest = nullptr;
        }
        else
        {
            originalSwitchBB->bbJumpKind = BBJ_ALWAYS;
            originalSwitchBB->bbJumpDest = jumpTab[0];
        }
        // Remove extra predecessor links if there was more than one case.
        for (unsigned i = 1; i < jumpCnt; ++i)
        {
            (void)comp->fgRemoveRefPred(jumpTab[i], originalSwitchBB);
        }

        // We have to get rid of the GT_SWITCH node but a child might have side effects so just assign
        // the result of the child subtree to a temp.

        // TODO-MIKE-Cleanup: This seems useless, the switch value should simply be marked unused.

        GenTree*  value  = node->GetOp(0);
        var_types type   = varActualType(value->GetType());
        unsigned  lclNum = comp->lvaNewTemp(type, true DEBUGARG("unused switch value temp"));
        GenTree*  store  = comp->gtNewStoreLclVar(lclNum, type, value);

        switchBBRange.InsertAfter(node, store);
        switchBBRange.Remove(node);

        return store;
    }

    noway_assert(jumpCnt >= 2);

    // Spill the argument to the switch node into a local so that it can be used later.
    LIR::Use use(switchBBRange, &(node->AsOp()->gtOp1), node);
    ReplaceWithLclVar(use);

    // GT_SWITCH(indexExpression) is now two statements:
    //   1. a statement containing 'asg' (for temp = indexExpression)
    //   2. and a statement with GT_SWITCH(temp)

    assert(node->gtOper == GT_SWITCH);
    GenTree* temp = node->AsOp()->gtOp1;
    assert(temp->gtOper == GT_LCL_VAR);
    unsigned  tempLclNum  = temp->AsLclVarCommon()->GetLclNum();
    var_types tempLclType = temp->TypeGet();

    BasicBlock* defaultBB   = jumpTab[jumpCnt - 1];
    BasicBlock* followingBB = originalSwitchBB->bbNext;

    /* Is the number of cases right for a test and jump switch? */
    const bool fFirstCaseFollows = (followingBB == jumpTab[0]);
    const bool fDefaultFollows   = (followingBB == defaultBB);

    unsigned minSwitchTabJumpCnt = 2; // table is better than just 2 cmp/jcc

    // This means really just a single cmp/jcc (aka a simple if/else)
    if (fFirstCaseFollows || fDefaultFollows)
    {
        minSwitchTabJumpCnt++;
    }

#if defined(TARGET_ARM)
    // On ARM for small switch tables we will
    // generate a sequence of compare and branch instructions
    // because the code to load the base of the switch
    // table is huge and hideous due to the relocation... :(
    minSwitchTabJumpCnt += 2;
#endif // TARGET_ARM

    // Once we have the temporary variable, we construct the conditional branch for
    // the default case.  As stated above, this conditional is being shared between
    // both GT_SWITCH lowering code paths.
    // This condition is of the form: if (temp > jumpTableLength - 2){ goto jumpTable[jumpTableLength - 1]; }
    GenTree* gtDefaultCaseCond = comp->gtNewOperNode(GT_GT, TYP_INT, comp->gtNewLclvNode(tempLclNum, tempLclType),
                                                     comp->gtNewIconNode(jumpCnt - 2, genActualType(tempLclType)));

    // Make sure we perform an unsigned comparison, just in case the switch index in 'temp'
    // is now less than zero 0 (that would also hit the default case).
    gtDefaultCaseCond->gtFlags |= GTF_UNSIGNED;

    GenTree* gtDefaultCaseJump = comp->gtNewOperNode(GT_JTRUE, TYP_VOID, gtDefaultCaseCond);
    gtDefaultCaseJump->gtFlags = node->gtFlags;

    LIR::Range condRange = LIR::SeqTree(comp, gtDefaultCaseJump);
    switchBBRange.InsertAtEnd(std::move(condRange));

    BasicBlock* afterDefaultCondBlock = comp->fgSplitBlockAfterNode(originalSwitchBB, condRange.LastNode());

    // afterDefaultCondBlock is now the switch, and all the switch targets have it as a predecessor.
    // originalSwitchBB is now a BBJ_NONE, and there is a predecessor edge in afterDefaultCondBlock
    // representing the fall-through flow from originalSwitchBB.
    assert(originalSwitchBB->bbJumpKind == BBJ_NONE);
    assert(originalSwitchBB->bbNext == afterDefaultCondBlock);
    assert(afterDefaultCondBlock->bbJumpKind == BBJ_SWITCH);
    assert(afterDefaultCondBlock->bbJumpSwt->bbsHasDefault);
    assert(afterDefaultCondBlock->isEmpty()); // Nothing here yet.

    // The GT_SWITCH code is still in originalSwitchBB (it will be removed later).

    // Turn originalSwitchBB into a BBJ_COND.
    originalSwitchBB->bbJumpKind = BBJ_COND;
    originalSwitchBB->bbJumpDest = jumpTab[jumpCnt - 1];

    // Fix the pred for the default case: the default block target still has originalSwitchBB
    // as a predecessor, but the fgSplitBlockAfterStatement() moved all predecessors to point
    // to afterDefaultCondBlock.
    flowList* oldEdge = comp->fgRemoveRefPred(jumpTab[jumpCnt - 1], afterDefaultCondBlock);
    comp->fgAddRefPred(jumpTab[jumpCnt - 1], originalSwitchBB, oldEdge);

    bool useJumpSequence = jumpCnt < minSwitchTabJumpCnt;

#if defined(TARGET_UNIX) && defined(TARGET_ARM)
    // Force using an inlined jumping instead switch table generation.
    // Switch jump table is generated with incorrect values in CoreRT case,
    // so any large switch will crash after loading to PC any such value.
    // I think this is due to the fact that we use absolute addressing
    // instead of relative. But in CoreRT is used as a rule relative
    // addressing when we generate an executable.
    // See also https://github.com/dotnet/runtime/issues/8683
    // Also https://github.com/dotnet/coreclr/pull/13197
    useJumpSequence = useJumpSequence || comp->IsTargetAbi(CORINFO_CORERT_ABI);
#endif // defined(TARGET_UNIX) && defined(TARGET_ARM)

    // If we originally had 2 unique successors, check to see whether there is a unique
    // non-default case, in which case we can eliminate the switch altogether.
    // Note that the single unique successor case is handled above.
    BasicBlock* uniqueSucc = nullptr;
    if (targetCnt == 2)
    {
        uniqueSucc = jumpTab[0];
        noway_assert(jumpCnt >= 2);
        for (unsigned i = 1; i < jumpCnt - 1; i++)
        {
            if (jumpTab[i] != uniqueSucc)
            {
                uniqueSucc = nullptr;
                break;
            }
        }
    }
    if (uniqueSucc != nullptr)
    {
        // If the unique successor immediately follows this block, we have nothing to do -
        // it will simply fall-through after we remove the switch, below.
        // Otherwise, make this a BBJ_ALWAYS.
        // Now, fixup the predecessor links to uniqueSucc.  In the original jumpTab:
        //   jumpTab[i-1] was the default target, which we handled above,
        //   jumpTab[0] is the first target, and we'll leave that predecessor link.
        // Remove any additional predecessor links to uniqueSucc.
        for (unsigned i = 1; i < jumpCnt - 1; ++i)
        {
            assert(jumpTab[i] == uniqueSucc);
            (void)comp->fgRemoveRefPred(uniqueSucc, afterDefaultCondBlock);
        }
        if (afterDefaultCondBlock->bbNext == uniqueSucc)
        {
            afterDefaultCondBlock->bbJumpKind = BBJ_NONE;
            afterDefaultCondBlock->bbJumpDest = nullptr;
        }
        else
        {
            afterDefaultCondBlock->bbJumpKind = BBJ_ALWAYS;
            afterDefaultCondBlock->bbJumpDest = uniqueSucc;
        }
    }
    // If the number of possible destinations is small enough, we proceed to expand the switch
    // into a series of conditional branches, otherwise we follow the jump table based switch
    // transformation.
    else if (useJumpSequence || comp->compStressCompile(Compiler::STRESS_SWITCH_CMP_BR_EXPANSION, 50))
    {
        // Lower the switch into a series of compare and branch IR trees.
        //
        // In this case we will morph the node in the following way:
        // 1. Generate a JTRUE statement to evaluate the default case. (This happens above.)
        // 2. Start splitting the switch basic block into subsequent basic blocks, each of which will contain
        //    a statement that is responsible for performing a comparison of the table index and conditional
        //    branch if equal.

        JITDUMP("Lowering switch " FMT_BB ": using compare/branch expansion\n", originalSwitchBB->bbNum);

        // We'll use 'afterDefaultCondBlock' for the first conditional. After that, we'll add new
        // blocks. If we end up not needing it at all (say, if all the non-default cases just fall through),
        // we'll delete it.
        bool        fUsedAfterDefaultCondBlock = false;
        BasicBlock* currentBlock               = afterDefaultCondBlock;
        LIR::Range* currentBBRange             = &LIR::AsRange(currentBlock);

        // Walk to entries 0 to jumpCnt - 1. If a case target follows, ignore it and let it fall through.
        // If no case target follows, the last one doesn't need to be a compare/branch: it can be an
        // unconditional branch.
        bool fAnyTargetFollows = false;
        for (unsigned i = 0; i < jumpCnt - 1; ++i)
        {
            assert(currentBlock != nullptr);

            // Remove the switch from the predecessor list of this case target's block.
            // We'll add the proper new predecessor edge later.
            flowList* oldEdge = comp->fgRemoveRefPred(jumpTab[i], afterDefaultCondBlock);

            if (jumpTab[i] == followingBB)
            {
                // This case label follows the switch; let it fall through.
                fAnyTargetFollows = true;
                continue;
            }

            // We need a block to put in the new compare and/or branch.
            // If we haven't used the afterDefaultCondBlock yet, then use that.
            if (fUsedAfterDefaultCondBlock)
            {
                BasicBlock* newBlock = comp->fgNewBBafter(BBJ_NONE, currentBlock, true);
                comp->fgAddRefPred(newBlock, currentBlock); // The fall-through predecessor.
                currentBlock   = newBlock;
                currentBBRange = &LIR::AsRange(currentBlock);
            }
            else
            {
                assert(currentBlock == afterDefaultCondBlock);
                fUsedAfterDefaultCondBlock = true;
            }

            // We're going to have a branch, either a conditional or unconditional,
            // to the target. Set the target.
            currentBlock->bbJumpDest = jumpTab[i];

            // Wire up the predecessor list for the "branch" case.
            comp->fgAddRefPred(jumpTab[i], currentBlock, oldEdge);

            if (!fAnyTargetFollows && (i == jumpCnt - 2))
            {
                // We're processing the last one, and there is no fall through from any case
                // to the following block, so we can use an unconditional branch to the final
                // case: there is no need to compare against the case index, since it's
                // guaranteed to be taken (since the default case was handled first, above).

                currentBlock->bbJumpKind = BBJ_ALWAYS;
            }
            else
            {
                // Otherwise, it's a conditional branch. Set the branch kind, then add the
                // condition statement.
                currentBlock->bbJumpKind = BBJ_COND;

                // Now, build the conditional statement for the current case that is
                // being evaluated:
                // GT_JTRUE
                //   |__ GT_COND
                //          |____GT_EQ
                //                 |____ (switchIndex) (The temp variable)
                //                 |____ (ICon)        (The actual case constant)
                GenTree* gtCaseCond = comp->gtNewOperNode(GT_EQ, TYP_INT, comp->gtNewLclvNode(tempLclNum, tempLclType),
                                                          comp->gtNewIconNode(i, tempLclType));
                GenTree*   gtCaseBranch = comp->gtNewOperNode(GT_JTRUE, TYP_VOID, gtCaseCond);
                LIR::Range caseRange    = LIR::SeqTree(comp, gtCaseBranch);
                currentBBRange->InsertAtEnd(std::move(caseRange));
            }
        }

        if (fAnyTargetFollows)
        {
            // There is a fall-through to the following block. In the loop
            // above, we deleted all the predecessor edges from the switch.
            // In this case, we need to add one back.
            comp->fgAddRefPred(currentBlock->bbNext, currentBlock);
        }

        if (!fUsedAfterDefaultCondBlock)
        {
            // All the cases were fall-through! We don't need this block.
            // Convert it from BBJ_SWITCH to BBJ_NONE and unset the BBF_DONT_REMOVE flag
            // so fgRemoveBlock() doesn't complain.
            JITDUMP("Lowering switch " FMT_BB ": all switch cases were fall-through\n", originalSwitchBB->bbNum);
            assert(currentBlock == afterDefaultCondBlock);
            assert(currentBlock->bbJumpKind == BBJ_SWITCH);
            currentBlock->bbJumpKind = BBJ_NONE;
            currentBlock->bbFlags &= ~BBF_DONT_REMOVE;
            comp->fgRemoveBlock(currentBlock, /* unreachable */ false); // It's an empty block.
        }
    }
    else
    {
        // At this point the default case has already been handled and we need to generate a jump
        // table based switch or a bit test based switch at the end of afterDefaultCondBlock. Both
        // switch variants need the switch value so create the necessary LclVar node here.
        GenTree*    switchValue      = comp->gtNewLclvNode(tempLclNum, tempLclType);
        LIR::Range& switchBlockRange = LIR::AsRange(afterDefaultCondBlock);
        switchBlockRange.InsertAtEnd(switchValue);

        // Try generating a bit test based switch first,
        // if that's not possible a jump table based switch will be generated.
        if (!TryLowerSwitchToBitTest(jumpTab, jumpCnt, targetCnt, afterDefaultCondBlock, switchValue))
        {
            JITDUMP("Lowering switch " FMT_BB ": using jump table expansion\n", originalSwitchBB->bbNum);

#ifdef TARGET_64BIT
            if (tempLclType != TYP_I_IMPL)
            {
                // SWITCH_TABLE expects the switch value (the index into the jump table) to be TYP_I_IMPL.
                // Note that the switch value is unsigned so the cast should be unsigned as well.
                switchValue = comp->gtNewCastNode(TYP_I_IMPL, switchValue, true, TYP_U_IMPL);
                switchBlockRange.InsertAtEnd(switchValue);
            }
#endif

            GenTree* switchTable = comp->gtNewJmpTableNode();
            GenTree* switchJump  = comp->gtNewOperNode(GT_SWITCH_TABLE, TYP_VOID, switchValue, switchTable);
            switchBlockRange.InsertAfter(switchValue, switchTable, switchJump);

            // this block no longer branches to the default block
            afterDefaultCondBlock->bbJumpSwt->removeDefault();
        }

        comp->fgInvalidateSwitchDescMapEntry(afterDefaultCondBlock);
    }

    GenTree* next = node->gtNext;

    // Get rid of the GT_SWITCH(temp).
    switchBBRange.Remove(node->AsOp()->gtOp1);
    switchBBRange.Remove(node);

    return next;
}

//------------------------------------------------------------------------
// TryLowerSwitchToBitTest: Attempts to transform a jump table switch into a bit test.
//
// Arguments:
//    jumpTable - The jump table
//    jumpCount - The number of blocks in the jump table
//    targetCount - The number of distinct blocks in the jump table
//    bbSwitch - The switch block
//    switchValue - A LclVar node that provides the switch value
//
// Return value:
//    true if the switch has been lowered to a bit test
//
// Notes:
//    If the jump table contains less than 32 (64 on 64 bit targets) entries and there
//    are at most 2 distinct jump targets then the jump table can be converted to a word
//    of bits where a 0 bit corresponds to one jump target and a 1 bit corresponds to the
//    other jump target. Instead of the indirect jump a BT-JCC sequence is used to jump
//    to the appropriate target:
//        mov eax, 245 ; jump table converted to a "bit table"
//        bt  eax, ebx ; ebx is supposed to contain the switch value
//        jc target1
//      target0:
//        ...
//      target1:
//    Such code is both shorter and faster (in part due to the removal of a memory load)
//    than the traditional jump table base code. And of course, it also avoids the need
//    to emit the jump table itself that can reach up to 256 bytes (for 64 entries).
//
bool Lowering::TryLowerSwitchToBitTest(
    BasicBlock* jumpTable[], unsigned jumpCount, unsigned targetCount, BasicBlock* bbSwitch, GenTree* switchValue)
{
#ifndef TARGET_XARCH
    // Other architectures may use this if they substitute GT_BT with equivalent code.
    return false;
#else
    assert(jumpCount >= 2);
    assert(targetCount >= 2);
    assert(bbSwitch->bbJumpKind == BBJ_SWITCH);
    assert(switchValue->OperIs(GT_LCL_VAR));

    //
    // Quick check to see if it's worth going through the jump table. The bit test switch supports
    // up to 2 targets but targetCount also includes the default block so we need to allow 3 targets.
    // We'll ensure that there are only 2 targets when building the bit table.
    //

    if (targetCount > 3)
    {
        return false;
    }

    //
    // The number of bits in the bit table is the same as the number of jump table entries. But the
    // jump table also includes the default target (at the end) so we need to ignore it. The default
    // has already been handled by a JTRUE(GT(switchValue, jumpCount - 2)) that LowerSwitch generates.
    //

    const unsigned bitCount = jumpCount - 1;

    if (bitCount > (genTypeSize(TYP_I_IMPL) * 8))
    {
        return false;
    }

    //
    // Build a bit table where a bit set to 0 corresponds to bbCase0 and a bit set to 1 corresponds to
    // bbCase1. Simply use the first block in the jump table as bbCase1, later we can invert the bit
    // table and/or swap the blocks if it's beneficial.
    //

    BasicBlock* bbCase0  = nullptr;
    BasicBlock* bbCase1  = jumpTable[0];
    size_t      bitTable = 1;

    for (unsigned bitIndex = 1; bitIndex < bitCount; bitIndex++)
    {
        if (jumpTable[bitIndex] == bbCase1)
        {
            bitTable |= (size_t(1) << bitIndex);
        }
        else if (bbCase0 == nullptr)
        {
            bbCase0 = jumpTable[bitIndex];
        }
        else if (jumpTable[bitIndex] != bbCase0)
        {
            // If it's neither bbCase0 nor bbCase1 then it means we have 3 targets. There can't be more
            // than 3 because of the check at the start of the function.
            assert(targetCount == 3);
            return false;
        }
    }

    //
    // One of the case blocks has to follow the switch block. This requirement could be avoided
    // by adding a BBJ_ALWAYS block after the switch block but doing that sometimes negatively
    // impacts register allocation.
    //

    if ((bbSwitch->bbNext != bbCase0) && (bbSwitch->bbNext != bbCase1))
    {
        return false;
    }

#ifdef TARGET_64BIT
    //
    // See if we can avoid a 8 byte immediate on 64 bit targets. If all upper 32 bits are 1
    // then inverting the bit table will make them 0 so that the table now fits in 32 bits.
    // Note that this does not change the number of bits in the bit table, it just takes
    // advantage of the fact that loading a 32 bit immediate into a 64 bit register zero
    // extends the immediate value to 64 bit.
    //

    if (~bitTable <= UINT32_MAX)
    {
        bitTable = ~bitTable;
        std::swap(bbCase0, bbCase1);
    }
#endif

    //
    // Rewire the blocks as needed and figure out the condition to use for JCC.
    //

    GenCondition bbSwitchCondition;
    bbSwitch->bbJumpKind = BBJ_COND;

    comp->fgRemoveAllRefPreds(bbCase1, bbSwitch);
    comp->fgRemoveAllRefPreds(bbCase0, bbSwitch);

    if (bbSwitch->bbNext == bbCase0)
    {
        // GenCondition::C generates JC so we jump to bbCase1 when the bit is set
        bbSwitchCondition    = GenCondition::C;
        bbSwitch->bbJumpDest = bbCase1;

        comp->fgAddRefPred(bbCase0, bbSwitch);
        comp->fgAddRefPred(bbCase1, bbSwitch);
    }
    else
    {
        assert(bbSwitch->bbNext == bbCase1);

        // GenCondition::NC generates JNC so we jump to bbCase0 when the bit is not set
        bbSwitchCondition    = GenCondition::NC;
        bbSwitch->bbJumpDest = bbCase0;

        comp->fgAddRefPred(bbCase0, bbSwitch);
        comp->fgAddRefPred(bbCase1, bbSwitch);
    }

    //
    // Append BT(bitTable, switchValue) and JCC(condition) to the switch block.
    //

    var_types bitTableType = (bitCount <= (genTypeSize(TYP_INT) * 8)) ? TYP_INT : TYP_LONG;
    GenTree*  bitTableIcon = comp->gtNewIconNode(bitTable, bitTableType);
    GenTree*  bitTest      = comp->gtNewOperNode(GT_BT, TYP_VOID, bitTableIcon, switchValue);
    bitTest->gtFlags |= GTF_SET_FLAGS;
    GenTreeCC* jcc = new (comp, GT_JCC) GenTreeCC(GT_JCC, bbSwitchCondition);
    jcc->gtFlags |= GTF_USE_FLAGS;

    LIR::AsRange(bbSwitch).InsertAfter(switchValue, bitTableIcon, bitTest, jcc);

    return true;
#endif // TARGET_XARCH
}

//------------------------------------------------------------------------
// InsertPutArg: rewrites the tree to put an arg in a register or on the stack.
//
// Return Value:
//    The new tree that was created to put the arg in the right place
//    or the incoming arg if the arg tree was not rewritten.
//
GenTree* Lowering::InsertPutArg(GenTreeCall* call, CallArgInfo* info)
{
    GenTree* arg = info->GetNode();

    if (info->GetRegCount() == 0)
    {
        GenTreePutArgStk* putArgStk = new (comp, GT_PUTARG_STK) GenTreePutArgStk(arg, info, call);
        BlockRange().InsertAfter(arg, putArgStk);
        info->SetNode(putArgStk);
        LowerPutArgStk(putArgStk);

        return putArgStk;
    }

    if (info->GetSlotCount() != 0)
    {
#if FEATURE_ARG_SPLIT
        // TODO: Need to check correctness for FastTailCall
        if (call->IsFastTailCall())
        {
            NYI_ARM("lower: struct argument by fast tail call");
        }

        GenTreePutArgSplit* putArgSplit = new (comp, GT_PUTARG_SPLIT) GenTreePutArgSplit(arg, info, call);
        BlockRange().InsertAfter(arg, putArgSplit);
        info->SetNode(putArgSplit);

        for (unsigned regIndex = 0; regIndex < info->GetRegCount(); regIndex++)
        {
            putArgSplit->SetRegNumByIdx(info->GetRegNum(regIndex), regIndex);

            // We don't have GC info in CallArgInfo on ARMARCH (the only user of split args)
            // and only integer registers are used. We'll just set everyting to TYP_I_IMPL
            // here and then update with correct GC types takend from layout or field list.
            //
            // TODO-MIKE-Cleanup: Might be better to just put the correct GC types in
            // CallArgInfo to simplify this and be consistent with UNIX_AMD64_ABI.
            // fgInitArgInfo would only need to take the GC info from the struct layout,
            // it doesn't need to deal with FIELD_LIST.

            assert(info->GetRegType(regIndex) == TYP_I_IMPL);

            putArgSplit->SetRegType(regIndex, TYP_I_IMPL);
        }

        if (arg->OperIs(GT_FIELD_LIST))
        {
            unsigned regIndex = 0;
            for (GenTreeFieldList::Use& use : arg->AsFieldList()->Uses())
            {
                GenTree*  node    = use.GetNode();
                var_types regType = node->GetType();

                if (varTypeIsGC(regType))
                {
                    putArgSplit->SetRegType(regIndex, regType);
                }
#ifdef TARGET_ARM
                else if (regType == TYP_DOUBLE)
                {
                    GenTree* bitcast = comp->gtNewBitCastNode(TYP_LONG, node);
                    bitcast->SetRegNum(info->GetRegNum(regIndex));
                    regIndex++;
                    bitcast->AsMultiRegOp()->gtOtherReg = info->GetRegNum(regIndex);
                    BlockRange().InsertAfter(node, bitcast);
                    use.SetNode(bitcast);
                }
#endif
                regIndex++;

                if (regIndex >= info->GetRegCount())
                {
                    break;
                }
            }
        }
        else if (!arg->IsIntegralConst(0))
        {
            ClassLayout* layout;

            if (arg->OperIs(GT_LCL_VAR))
            {
                layout = comp->lvaGetDesc(arg->AsLclVar())->GetLayout();
            }
            else if (arg->OperIs(GT_LCL_FLD))
            {
                layout = arg->AsLclFld()->GetLayout(comp);
            }
            else
            {
                layout = arg->AsObj()->GetLayout();
            }

            if (layout->HasGCPtr())
            {
                for (unsigned index = 0; index < info->GetRegCount(); index++)
                {
                    if (layout->IsGCPtr(index))
                    {
                        putArgSplit->SetRegType(index, layout->GetGCPtrType(index));
                    }
                }
            }
        }

        LowerPutArgStk(putArgSplit);
        return putArgSplit;
#else  // !FEATURE_ARG_SPLIT
        unreached();
#endif // !FEATURE_ARG_SPLIT
    }

    if (arg->OperIs(GT_FIELD_LIST))
    {
#if FEATURE_MULTIREG_ARGS
        unsigned int regIndex = 0;
        for (GenTreeFieldList::Use& use : arg->AsFieldList()->Uses())
        {
            GenTree* putArgReg = InsertPutArgReg(use.GetNode(), info, regIndex);
            use.SetNode(putArgReg);

#ifdef TARGET_ARM
            regIndex += putArgReg->TypeIs(TYP_LONG) ? 2 : 1;
#else
            regIndex++;
#endif
        }

        return arg;
#else
        unreached();
#endif
    }

    GenTree* putArgReg = InsertPutArgReg(arg, info, 0);
    info->SetNode(putArgReg);

#ifdef TARGET_ARM
    assert(info->GetRegCount() == (putArgReg->TypeIs(TYP_LONG) ? 2u : 1u));
#else
    assert(info->GetRegCount() == 1);
#endif

    return putArgReg;
}

GenTree* Lowering::InsertPutArgReg(GenTree* arg, CallArgInfo* argInfo, unsigned regIndex)
{
    var_types type   = varActualType(arg->GetType());
    regNumber argReg = argInfo->GetRegNum(regIndex);

#ifdef TARGET_ARM
    // LONG args are passed via FIELD_LIST.
    assert(type != TYP_LONG);

    if ((type == TYP_DOUBLE) && genIsValidIntReg(argReg))
    {
        GenTree* intArg = comp->gtNewBitCastNode(TYP_LONG, arg);
        intArg->SetRegNum(argReg);
        intArg->AsMultiRegOp()->gtOtherReg = argInfo->GetRegNum(regIndex + 1);
        BlockRange().InsertAfter(arg, intArg);

        arg  = intArg;
        type = TYP_LONG;
    }

    GenTreeMultiRegOp* putArg = new (comp, GT_PUTARG_REG) GenTreeMultiRegOp(GT_PUTARG_REG, type, arg);

    if (type == TYP_LONG)
    {
        putArg->gtOtherReg = argInfo->GetRegNum(regIndex + 1);
    }
#else
    GenTree* putArg = comp->gtNewOperNode(GT_PUTARG_REG, type, arg);
#endif

    assert(varTypeUsesFloatReg(type) == genIsValidFloatReg(argReg));

    putArg->SetRegNum(argReg);
    BlockRange().InsertAfter(arg, putArg);
    return putArg;
}

void Lowering::LowerCallArgs(GenTreeCall* call)
{
    CallInfo* info = call->GetInfo();

    for (unsigned i = 0; i < info->GetArgCount(); i++)
    {
        JITDUMPTREE(info->GetArgInfo(i)->GetNode(), "lowering call arg %u (before):\n", i);
        LowerCallArg(call, info->GetArgInfo(i));
        JITDUMPTREE(info->GetArgInfo(i)->GetNode(), "lowering call arg %u (after):\n", i);
        JITDUMP("\n");
    }
}

//------------------------------------------------------------------------
// LowerCallArg: Lower one argument of a call. This entails splicing a "putarg" node between
// the argument evaluation and the call. This is the point at which the source is
// consumed and the value transitions from control of the register allocator to the calling
// convention.
//
//    call  - The call node
//    argInfo - Call argument info
//
void Lowering::LowerCallArg(GenTreeCall* call, CallArgInfo* argInfo)
{
    GenTree* arg = argInfo->GetNode();

    assert(!arg->OperIsPutArg());
    assert(!arg->OperIs(GT_STORE_LCL_VAR, GT_ARGPLACE, GT_NOP));

    // Real call arguments should not have GTF_LATE_ARG, only arg setup nodes do.
    assert((arg->gtFlags & GTF_LATE_ARG) == 0);

#if !defined(TARGET_64BIT)
    if (arg->TypeIs(TYP_LONG))
    {
        noway_assert(arg->OperIs(GT_LONG));

        GenTreeFieldList* fieldList = new (comp, GT_FIELD_LIST) GenTreeFieldList();
        fieldList->AddFieldLIR(comp, arg->AsOp()->GetOp(0), 0, TYP_INT);
        fieldList->AddFieldLIR(comp, arg->AsOp()->GetOp(1), 4, TYP_INT);
        argInfo->SetNode(fieldList);
        BlockRange().InsertAfter(arg, fieldList);
        BlockRange().Remove(arg);

        GenTree* newArg = InsertPutArg(call, argInfo);

        if (argInfo->GetRegCount() != 0)
        {
            assert(argInfo->GetRegCount() == 2);
            assert(newArg == fieldList);
        }
        else
        {
            assert(argInfo->GetSlotCount() == 2);
            assert(newArg->OperIs(GT_PUTARG_STK));
        }

        return;
    }
#endif // !defined(TARGET_64BIT)

    assert(!arg->OperIs(GT_OBJ) || arg->TypeIs(TYP_STRUCT));

    if (arg->TypeIs(TYP_STRUCT))
    {
        arg->SetContained();
    }

    InsertPutArg(call, argInfo);
}

// helper that create a node representing a relocatable physical address computation
GenTree* Lowering::AddrGen(ssize_t addr)
{
    // this should end up in codegen as : instGen_Set_Reg_To_Imm(EA_HANDLE_CNS_RELOC, reg, addr)
    GenTree* result = comp->gtNewIconHandleNode(addr, GTF_ICON_FTN_ADDR);
    return result;
}

// variant that takes a void*
GenTree* Lowering::AddrGen(void* addr)
{
    return AddrGen((ssize_t)addr);
}

// do lowering steps for a call
// this includes:
//   - adding the placement nodes (either stack or register variety) for arguments
//   - lowering the expression that calculates the target address
//   - adding nodes for other operations that occur after the call sequence starts and before
//        control transfer occurs (profiling and tail call helpers, pinvoke incantations)
//
void Lowering::LowerCall(GenTree* node)
{
    GenTreeCall* call = node->AsCall();

    JITDUMP("lowering call (before):\n");
    DISPTREERANGE(BlockRange(), call);
    JITDUMP("\n");

    call->ClearOtherRegs();
    LowerCallArgs(call);

    // note that everything generated from this point on runs AFTER the outgoing args are placed
    GenTree* controlExpr          = nullptr;
    bool     callWasExpandedEarly = false;

    // for x86, this is where we record ESP for checking later to make sure stack is balanced

    // Check for Delegate.Invoke(). If so, we inline it. We get the
    // target-object and target-function from the delegate-object, and do
    // an indirect call.
    if (call->IsDelegateInvoke())
    {
        controlExpr = LowerDelegateInvoke(call);
    }
    else
    {
        //  Virtual and interface calls
        switch (call->gtFlags & GTF_CALL_VIRT_KIND_MASK)
        {
            case GTF_CALL_VIRT_STUB:
                controlExpr = LowerVirtualStubCall(call);
                break;

            case GTF_CALL_VIRT_VTABLE:
                assert(call->IsVirtualVtable());
                if (!call->IsExpandedEarly())
                {
                    assert(call->gtControlExpr == nullptr);
                    controlExpr = LowerVirtualVtableCall(call);
                }
                else
                {
                    callWasExpandedEarly = true;
                    controlExpr          = call->gtControlExpr;
                }
                break;

            case GTF_CALL_NONVIRT:
                if (call->IsUnmanaged())
                {
                    controlExpr = LowerNonvirtPinvokeCall(call);
                }
                else if (call->gtCallType == CT_INDIRECT)
                {
                    controlExpr = LowerIndirectNonvirtCall(call);
                }
                else
                {
                    controlExpr = LowerDirectCall(call);
                }
                break;

            default:
                noway_assert(!"strange call type");
                break;
        }
    }

    if (call->IsTailCallViaJitHelper())
    {
        // Either controlExpr or gtCallAddr must contain real call target.
        if (controlExpr == nullptr)
        {
            assert(call->gtCallType == CT_INDIRECT);
            assert(call->gtCallAddr != nullptr);
            controlExpr = call->gtCallAddr;
        }

        controlExpr = LowerTailCallViaJitHelper(call, controlExpr);
    }

    // Check if we need to thread a newly created controlExpr into the LIR
    //
    if ((controlExpr != nullptr) && !callWasExpandedEarly)
    {
        LIR::Range controlExprRange = LIR::SeqTree(comp, controlExpr);

        JITDUMP("results of lowering call:\n");
        DISPRANGE(controlExprRange);

        GenTree* insertionPoint = call;
        if (!call->IsTailCallViaJitHelper())
        {
            // The controlExpr should go before the gtCallCookie and the gtCallAddr, if they exist
            //
            // TODO-LIR: find out what's really required here, as this is currently a tree order
            // dependency.
            if (call->gtCallType == CT_INDIRECT)
            {
                bool isClosed = false;
                if (call->gtCallCookie != nullptr)
                {
#ifdef DEBUG
                    GenTree* firstCallAddrNode = BlockRange().GetTreeRange(call->gtCallAddr, &isClosed).FirstNode();
                    assert(isClosed);
                    assert(call->gtCallCookie->Precedes(firstCallAddrNode));
#endif // DEBUG

                    insertionPoint = BlockRange().GetTreeRange(call->gtCallCookie, &isClosed).FirstNode();
                    assert(isClosed);
                }
                else if (call->gtCallAddr != nullptr)
                {
                    insertionPoint = BlockRange().GetTreeRange(call->gtCallAddr, &isClosed).FirstNode();
                    assert(isClosed);
                }
            }
        }

        ContainCheckRange(controlExprRange);
        BlockRange().InsertBefore(insertionPoint, std::move(controlExprRange));

        call->gtControlExpr = controlExpr;
    }
    if (call->IsFastTailCall())
    {
        // Lower fast tail call can introduce new temps to set up args correctly for Callee.
        // This involves patching LCL_VAR and LCL_VAR_ADDR nodes holding Caller stack args
        // and replacing them with a new temp. Control expr also can contain nodes that need
        // to be patched.
        // Therefore lower fast tail call must be done after controlExpr is inserted into LIR.
        // There is one side effect which is flipping the order of PME and control expression
        // since LowerFastTailCall calls InsertPInvokeMethodEpilog.
        LowerFastTailCall(call);
    }

    if (varTypeIsStruct(call->GetType()))
    {
        LowerCallStruct(call);
    }

    ContainCheckCallOperands(call);
    JITDUMP("lowering call (after):\n");
    DISPTREERANGE(BlockRange(), call);
    JITDUMP("\n");
}

// Inserts profiler hook, GT_PROF_HOOK for a tail call node.
//
// AMD64:
// We need to insert this after all nested calls, but before all the arguments to this call have been set up.
// To do this, we look for the first GT_PUTARG_STK or GT_PUTARG_REG, and insert the hook immediately before
// that. If there are no args, then it should be inserted before the call node.
//
// For example:
//              *  stmtExpr  void  (top level) (IL 0x000...0x010)
// arg0 SETUP   |  /--*  argPlace  ref    REG NA $c5
// this in rcx  |  |     /--*  argPlace  ref    REG NA $c1
//              |  |     |  /--*  call      ref    System.Globalization.CultureInfo.get_InvariantCulture $c2
// arg1 SETUP   |  |     +--*  st.lclVar ref    V02 tmp1          REG NA $c2
//              |  |     |  /--*  lclVar    ref    V02 tmp1         u : 2 (last use) REG NA $c2
// arg1 in rdx  |  |     +--*  putarg_reg ref    REG NA
//              |  |     |  /--*  lclVar    ref    V00 arg0         u : 2 (last use) REG NA $80
// this in rcx  |  |     +--*  putarg_reg ref    REG NA
//              |  |  /--*  call nullcheck ref    System.String.ToLower $c5
//              |  |  {  *  stmtExpr  void  (embedded)(IL 0x000... ? ? ? )
//              |  |  {  \--*  prof_hook void   REG NA
// arg0 in rcx  |  +--*  putarg_reg ref    REG NA
// control expr |  +--*  const(h)  long   0x7ffe8e910e98 ftn REG NA
//              \--*  call      void   System.Runtime.Remoting.Identity.RemoveAppNameOrAppGuidIfNecessary $VN.Void
//
// In this case, the GT_PUTARG_REG src is a nested call. We need to put the instructions after that call
// (as shown). We assume that of all the GT_PUTARG_*, only the first one can have a nested call.
//
// X86:
// Insert the profiler hook immediately before the call. The profiler hook will preserve
// all argument registers (ECX, EDX), but nothing else.
//
// Params:
//    rangeEnd        - tail call node
//    insertionPoint  - if non-null, insert the profiler hook before this point.
//                      If null, insert the profiler hook before args are setup
//                      but after all arg side effects are computed.
//
void Lowering::InsertProfTailCallHook(GenTreeCall* call, GenTree* insertionPoint)
{
    assert(call->IsTailCall());
    assert(comp->compIsProfilerHookNeeded());

#if defined(TARGET_X86)

    if (insertionPoint == nullptr)
    {
        insertionPoint = call;
    }

#else // !defined(TARGET_X86)

    if (insertionPoint == nullptr)
    {
        for (GenTreeCall::Use& use : call->Args())
        {
            assert(!use.GetNode()->OperIs(GT_PUTARG_REG)); // We don't expect to see these in gtCallArgs

            if (use.GetNode()->OperIs(GT_PUTARG_STK))
            {
                // found it
                insertionPoint = use.GetNode();
                break;
            }
        }

        if (insertionPoint == nullptr)
        {
            for (GenTreeCall::Use& use : call->LateArgs())
            {
                if (use.GetNode()->OperIs(GT_PUTARG_REG, GT_PUTARG_STK))
                {
                    // found it
                    insertionPoint = use.GetNode();
                    break;
                }
            }

            // If there are no args, insert before the call node
            if (insertionPoint == nullptr)
            {
                insertionPoint = call;
            }
        }
    }

#endif // !defined(TARGET_X86)

    assert(insertionPoint != nullptr);
    GenTree* profHookNode = new (comp, GT_PROF_HOOK) GenTree(GT_PROF_HOOK, TYP_VOID);
    BlockRange().InsertBefore(insertionPoint, profHookNode);
}

//------------------------------------------------------------------------
// LowerFastTailCall: Lower a call node dispatched as a fast tailcall (epilog +
// jmp).
//
// Arguments:
//    call - the call node that is being dispatched as a fast tailcall.
//
// Assumptions:
//    call must be non-null.
//
// Notes:
//     For fast tail calls it is necessary to set up stack args in the incoming
//     arg stack space area. When args passed also come from this area we may
//     run into problems because we may end up overwriting the stack slot before
//     using it. For example, for foo(a, b) { return bar(b, a); }, if a and b
//     are on incoming arg stack space in foo they need to be swapped in this
//     area for the call to bar. This function detects this situation and
//     introduces a temp when an outgoing argument would overwrite a later-used
//     incoming argument.
//
//     This function also handles inserting necessary profiler hooks and pinvoke
//     method epilogs in case there are inlined pinvokes.
void Lowering::LowerFastTailCall(GenTreeCall* call)
{
#if FEATURE_FASTTAILCALL
    // Tail call restrictions i.e. conditions under which tail prefix is ignored.
    // Most of these checks are already done by importer or fgMorphTailCall().
    // This serves as a double sanity check.
    assert((comp->info.compFlags & CORINFO_FLG_SYNCH) == 0); // tail calls from synchronized methods
    assert(!comp->opts.IsReversePInvoke());                  // tail calls reverse pinvoke
    assert(!call->IsUnmanaged());                            // tail calls to unamanaged methods
    assert(!comp->compLocallocUsed);                         // tail call from methods that also do localloc

#ifdef TARGET_AMD64
    assert(!comp->getNeedsGSSecurityCookie()); // jit64 compat: tail calls from methods that need GS check
#endif                                         // TARGET_AMD64

    // We expect to see a call that meets the following conditions
    assert(call->IsFastTailCall());

    // VM cannot use return address hijacking when A() and B() tail call each
    // other in mutual recursion.  Therefore, this block is reachable through
    // a GC-safe point or the whole method is marked as fully interruptible.
    //
    // TODO-Cleanup:
    // optReachWithoutCall() depends on the fact that loop headers blocks
    // will have a block number > fgLastBB.  These loop headers gets added
    // after dominator computation and get skipped by OptReachWithoutCall().
    // The below condition cannot be asserted in lower because fgSimpleLowering()
    // can add a new basic block for range check failure which becomes
    // fgLastBB with block number > loop header block number.
    // assert((comp->compCurBB->bbFlags & BBF_GC_SAFE_POINT) ||
    //         !comp->optReachWithoutCall(comp->fgFirstBB, comp->compCurBB) || comp->GetInterruptible());

    // If PInvokes are in-lined, we have to remember to execute PInvoke method epilog anywhere that
    // a method returns.  This is a case of caller method has both PInvokes and tail calls.
    if (comp->compMethodRequiresPInvokeFrame())
    {
        InsertPInvokeMethodEpilog(comp->compCurBB DEBUGARG(call));
    }

    // Args for tail call are setup in incoming arg area.  The gc-ness of args of
    // caller and callee (which being tail called) may not match.  Therefore, everything
    // from arg setup until the epilog need to be non-interuptible by GC.  This is
    // achieved by inserting GT_START_NONGC before the very first GT_PUTARG_STK node
    // of call is setup.  Note that once a stack arg is setup, it cannot have nested
    // calls subsequently in execution order to setup other args, because the nested
    // call could over-write the stack arg that is setup earlier.
    ArrayStack<GenTree*> putargs(comp->getAllocator(CMK_ArrayStack));

    for (GenTreeCall::Use& use : call->Args())
    {
        if (use.GetNode()->OperIs(GT_PUTARG_STK))
        {
            putargs.Push(use.GetNode());
        }
    }

    for (GenTreeCall::Use& use : call->LateArgs())
    {
        if (use.GetNode()->OperIs(GT_PUTARG_STK))
        {
            putargs.Push(use.GetNode());
        }
    }

    GenTree* startNonGCNode = nullptr;
    if (!putargs.Empty())
    {
        // Get the earliest operand of the first PUTARG_STK node. We will make
        // the requred copies of args before this node.
        bool     unused;
        GenTree* insertionPoint = BlockRange().GetTreeRange(putargs.Bottom(), &unused).FirstNode();
        // Insert GT_START_NONGC node before we evaluate the PUTARG_STK args.
        // Note that if there are no args to be setup on stack, no need to
        // insert GT_START_NONGC node.
        startNonGCNode = new (comp, GT_START_NONGC) GenTree(GT_START_NONGC, TYP_VOID);
        BlockRange().InsertBefore(insertionPoint, startNonGCNode);

        // Gc-interruptability in the following case:
        //     foo(a, b, c, d, e) { bar(a, b, c, d, e); }
        //     bar(a, b, c, d, e) { foo(a, b, d, d, e); }
        //
        // Since the instruction group starting from the instruction that sets up first
        // stack arg to the end of the tail call is marked as non-gc interruptible,
        // this will form a non-interruptible tight loop causing gc-starvation. To fix
        // this we insert GT_NO_OP as embedded stmt before GT_START_NONGC, if the method
        // has a single basic block and is not a GC-safe point.  The presence of a single
        // nop outside non-gc interruptible region will prevent gc starvation.
        if ((comp->fgBBcount == 1) && !(comp->compCurBB->bbFlags & BBF_GC_SAFE_POINT))
        {
            assert(comp->fgFirstBB == comp->compCurBB);
            GenTree* noOp = new (comp, GT_NO_OP) GenTree(GT_NO_OP, TYP_VOID);
            BlockRange().InsertBefore(startNonGCNode, noOp);
        }

        // Since this is a fast tailcall each PUTARG_STK will place the argument in the
        // _incoming_ arg space area. This will effectively overwrite our already existing
        // incoming args that live in that area. If we have later uses of those args, this
        // is a problem. We introduce a defensive copy into a temp here of those args that
        // potentially may cause problems.
        for (int i = 0; i < putargs.Height(); i++)
        {
            GenTreePutArgStk* put = putargs.Bottom(i)->AsPutArgStk();

            unsigned argStartOffset = put->GetSlotOffset();
            unsigned argEndOffset   = argStartOffset + put->GetArgSize();
#ifndef WINDOWS_AMD64_ABI
            int baseOff = -1; // Stack offset of first arg on stack
#endif

            for (unsigned paramLclNum = 0; paramLclNum < comp->info.GetParamCount(); paramLclNum++)
            {
                LclVarDsc* paramLcl = comp->lvaGetDesc(paramLclNum);

                if (paramLcl->IsRegParam())
                {
                    continue;
                }

#ifdef WINDOWS_AMD64_ABI
                // On Windows x64, the argument position determines the stack slot uniquely, and even the
                // register args take up space in the stack frame (shadow space).
                unsigned paramStartOffset = paramLclNum * REGSIZE_BYTES;
                unsigned paramEndOffset   = paramStartOffset + REGSIZE_BYTES;
#else
                assert(paramLcl->GetStackOffset() != BAD_STK_OFFS);

                if (baseOff == -1)
                {
                    baseOff = paramLcl->GetStackOffset();
                }

                // On all ABIs where we fast tail call the stack args should come in order.
                assert(baseOff <= paramLcl->GetStackOffset());

                // Compute offset of this stack argument relative to the first stack arg.
                // This will be its offset into the incoming arg space area.
                unsigned paramStartOffset = static_cast<unsigned>(paramLcl->GetStackOffset() - baseOff);
                unsigned paramEndOffset   = paramStartOffset + comp->lvaLclSize(paramLclNum);
#endif

                // If ranges do not overlap then this PUTARG_STK will not mess up the arg.
                if ((argEndOffset <= paramStartOffset) || (argStartOffset >= paramEndOffset))
                {
                    continue;
                }

                // Codegen cannot handle a partially overlapping copy. For
                // example, if we have
                // bar(S16 stack, S32 stack2)
                // foo(S32 stack, S32 stack2) { bar(..., stack) }
                // then we may end up having to move 'stack' in foo 16 bytes
                // ahead. It is possible that this PUTARG_STK is the only use,
                // in which case we will need to introduce a temp, so look for
                // uses starting from it. Note that we assume that in-place
                // copies are OK.
                GenTree* lookForUsesFrom = put->gtNext;
                if (argStartOffset != paramStartOffset)
                {
                    lookForUsesFrom = insertionPoint;
                }

                RehomeParamForFastTailCall(paramLclNum, insertionPoint, lookForUsesFrom, call);
                // The above call can introduce temps and invalidate the pointer.
                paramLcl = comp->lvaGetDesc(paramLclNum);

                if (paramLcl->IsPromoted())
                {
                    for (unsigned j = 0; j < paramLcl->GetPromotedFieldCount(); j++)
                    {
                        RehomeParamForFastTailCall(paramLcl->GetPromotedFieldLclNum(j), insertionPoint, lookForUsesFrom,
                                                   call);
                    }
                }
            }
        }
    }

    // Insert GT_PROF_HOOK node to emit profiler tail call hook. This should be
    // inserted before the args are setup but after the side effects of args are
    // computed. That is, GT_PROF_HOOK node needs to be inserted before GT_START_NONGC
    // node if one exists.
    if (comp->compIsProfilerHookNeeded())
    {
        InsertProfTailCallHook(call, startNonGCNode);
    }

#else // !FEATURE_FASTTAILCALL

    // Platform does not implement fast tail call mechanism. This cannot be
    // reached because we always choose to do a tailcall via helper on those
    // platforms (or no tailcall at all).
    unreached();
#endif
}

// Scan the range of nodes [rangeStart, rangeEnd) and update all references
// to the specified local to use a new temp instead. The temp is initialized
// with the original local's value before "insertTempBefore".
// It is assumed that the specified local is not accessed inside the range
// via an address that originated outside of the range.
void Lowering::RehomeParamForFastTailCall(unsigned paramLclNum,
                                          GenTree* insertTempBefore,
                                          GenTree* rangeStart,
                                          GenTree* rangeEnd)
{
    unsigned tmpLclNum = BAD_VAR_NUM;

    for (GenTree* node = rangeStart; node != rangeEnd; node = node->gtNext)
    {
        if (!node->OperIsLocal() && !node->OperIsLocalAddr())
        {
            continue;
        }

        assert(!node->OperIs(GT_PHI_ARG));

        if (node->AsLclVarCommon()->GetLclNum() != paramLclNum)
        {
            continue;
        }

        if (tmpLclNum == BAD_VAR_NUM)
        {
            tmpLclNum = comp->lvaGrabTemp(true DEBUGARG("fast tail call param temp"));

            LclVarDsc* paramLcl = comp->lvaGetDesc(paramLclNum);
            LclVarDsc* tmpLcl   = comp->lvaGetDesc(tmpLclNum);

            tmpLcl->lvDoNotEnregister = paramLcl->lvDoNotEnregister;

            var_types type = varActualType(paramLcl->GetType());

            if (varTypeIsStruct(type))
            {
                comp->lvaSetStruct(tmpLclNum, paramLcl->GetLayout(), /* checkUnsafeBuffer */ false);
            }
            else
            {
                tmpLcl->SetType(type);
            }

            GenTree* value = comp->gtNewLclvNode(paramLclNum, type);

            if (type == TYP_STRUCT)
            {
                // TODO-MIKE-CQ: This code was previously using GT_STORE_BLK with a block layout.
                //
                // It's best to avoid using block layout when the struct layout is available (and
                // BLK/STORE_BLK) but doing so has a somewhat unfortunate side-effect: this copy
                // is done in a no-GC region but GT_STORE_LCL_VAR doesn't know that and it will do
                // it's normal GC copy thing. For unrolled copies it doesn't really matter, as
                // the same code is being generated in both cases, the only difference is that
                // for large copies we get "rep movsq" instead of a helper call.
                //
                // Perhaps there should be a way to tell GT_STORE_LCL_VAR to ignore GC info in such
                // cases. But then there's no real need to put this copy in the no-GC region so
                // maybe it's best to leave GT_STORE_LCL_VAR as is and insert the copy before the
                // no-GC region.
            }
            else
            {
                // TODO-MIKE-Review: This code came from gtNewTempAssign and it's not clear if it's
                // needed and if it's corect. Load "normalization" is done with casts, not by having
                // the LCL_VAR node type set to the small int type of the local. If a cast already
                // exists doing this is pointless. If a cast does not exist then it means that morph
                // decided that it's not needed and changing the type here is also pointless.
                // Removing this results in one byte diff in corelib PMI diff due to a movzx being
                // changed to a mov. The movzx was indeed redundant.

                if (paramLcl->lvNormalizeOnLoad())
                {
                    value->SetType(paramLcl->GetType());
                }
            }

            GenTree* store = comp->gtNewStoreLclVar(tmpLclNum, type, value);
            BlockRange().InsertBefore(insertTempBefore, value, store);

            if (type == TYP_STRUCT)
            {
                LowerNode(store);
            }
        }

        node->AsLclVarCommon()->SetLclNum(tmpLclNum);
    }
}

//------------------------------------------------------------------------
// LowerTailCallViaJitHelper: lower a call via the tailcall JIT helper. Morph
// has already inserted tailcall helper special arguments. This function inserts
// actual data for some placeholders. This function is only used on x86.
//
// Lower
//      tail.call(<function args>, int numberOfOldStackArgs, int dummyNumberOfNewStackArgs, int flags, void* dummyArg)
// as
//      JIT_TailCall(<function args>, int numberOfOldStackArgsWords, int numberOfNewStackArgsWords, int flags, void*
//      callTarget)
// Note that the special arguments are on the stack, whereas the function arguments follow the normal convention.
//
// Also inserts PInvoke method epilog if required.
//
// Arguments:
//    call         -  The call node
//    callTarget   -  The real call target. This is used to replace the dummyArg during lowering.
//
// Return Value:
//    Returns control expression tree for making a call to helper Jit_TailCall.
//
GenTree* Lowering::LowerTailCallViaJitHelper(GenTreeCall* call, GenTree* callTarget)
{
    // Tail call restrictions i.e. conditions under which tail prefix is ignored.
    // Most of these checks are already done by importer or fgMorphTailCall().
    // This serves as a double sanity check.
    assert((comp->info.compFlags & CORINFO_FLG_SYNCH) == 0); // tail calls from synchronized methods
    assert(!call->IsUnmanaged());                            // tail calls to unamanaged methods
    assert(!comp->compLocallocUsed);                         // tail call from methods that also do localloc

    // We expect to see a call that meets the following conditions
    assert(call->IsTailCallViaJitHelper());
    assert(callTarget != nullptr);

    // The TailCall helper call never returns to the caller and is not GC interruptible.
    // Therefore the block containing the tail call should be a GC safe point to avoid
    // GC starvation. It is legal for the block to be unmarked iff the entry block is a
    // GC safe point, as the entry block trivially dominates every reachable block.
    assert((comp->compCurBB->bbFlags & BBF_GC_SAFE_POINT) || (comp->fgFirstBB->bbFlags & BBF_GC_SAFE_POINT));

    // If PInvokes are in-lined, we have to remember to execute PInvoke method epilog anywhere that
    // a method returns.  This is a case of caller method has both PInvokes and tail calls.
    if (comp->compMethodRequiresPInvokeFrame())
    {
        InsertPInvokeMethodEpilog(comp->compCurBB DEBUGARG(call));
    }

    // Remove gtCallAddr from execution order if present.
    if (call->gtCallType == CT_INDIRECT)
    {
        assert(call->gtCallAddr != nullptr);

        bool               isClosed;
        LIR::ReadOnlyRange callAddrRange = BlockRange().GetTreeRange(call->gtCallAddr, &isClosed);
        assert(isClosed);

        BlockRange().Remove(std::move(callAddrRange));
    }

    // The callTarget tree needs to be sequenced.
    LIR::Range callTargetRange = LIR::SeqTree(comp, callTarget);

    // Verify the special args are what we expect, and replace the dummy args with real values.
    // We need to figure out the size of the outgoing stack arguments, not including the special args.
    // The number of 4-byte words is passed to the helper for the incoming and outgoing argument sizes.
    // This number is exactly the next slot number in the call's argument info struct.
    unsigned nNewStkArgsWords = call->fgArgInfo->GetNextSlotNum();
    assert(nNewStkArgsWords >= 4); // There must be at least the four special stack args.
    nNewStkArgsWords -= 4;

    unsigned numArgs = call->fgArgInfo->ArgCount();

    fgArgTabEntry* argEntry;

    // arg 0 == callTarget.
    argEntry = call->GetArgInfoByArgNum(numArgs - 1);
    assert(argEntry != nullptr);
    GenTree* arg0 = argEntry->GetNode()->AsPutArgStk()->gtGetOp1();

    ContainCheckRange(callTargetRange);
    BlockRange().InsertAfter(arg0, std::move(callTargetRange));

    bool               isClosed;
    LIR::ReadOnlyRange secondArgRange = BlockRange().GetTreeRange(arg0, &isClosed);
    assert(isClosed);
    BlockRange().Remove(std::move(secondArgRange));

    argEntry->GetNode()->AsPutArgStk()->gtOp1 = callTarget;

    // arg 1 == flags
    argEntry = call->GetArgInfoByArgNum(numArgs - 2);
    assert(argEntry != nullptr);
    GenTree* arg1 = argEntry->GetNode()->AsPutArgStk()->gtGetOp1();
    assert(arg1->gtOper == GT_CNS_INT);

    ssize_t tailCallHelperFlags = 1 |                                  // always restore EDI,ESI,EBX
                                  (call->IsVirtualStub() ? 0x2 : 0x0); // Stub dispatch flag
    arg1->AsIntCon()->gtIconVal = tailCallHelperFlags;

    // arg 2 == numberOfNewStackArgsWords
    argEntry = call->GetArgInfoByArgNum(numArgs - 3);
    assert(argEntry != nullptr);
    GenTree* arg2 = argEntry->GetNode()->AsPutArgStk()->gtGetOp1();
    assert(arg2->gtOper == GT_CNS_INT);

    arg2->AsIntCon()->gtIconVal = nNewStkArgsWords;

#ifdef DEBUG
    // arg 3 == numberOfOldStackArgsWords
    argEntry = call->GetArgInfoByArgNum(numArgs - 4);
    assert(argEntry != nullptr);
    GenTree* arg3 = argEntry->GetNode()->AsPutArgStk()->gtGetOp1();
    assert(arg3->gtOper == GT_CNS_INT);
#endif // DEBUG

    // Transform this call node into a call to Jit tail call helper.
    call->gtCallType    = CT_HELPER;
    call->gtCallMethHnd = comp->eeFindHelper(CORINFO_HELP_TAILCALL);
    call->gtFlags &= ~GTF_CALL_VIRT_KIND_MASK;

    // Lower this as if it were a pure helper call.
    call->gtCallMoreFlags &= ~(GTF_CALL_M_TAILCALL | GTF_CALL_M_TAILCALL_VIA_JIT_HELPER);
    GenTree* result = LowerDirectCall(call);

    // Now add back tail call flags for identifying this node as tail call dispatched via helper.
    call->gtCallMoreFlags |= GTF_CALL_M_TAILCALL | GTF_CALL_M_TAILCALL_VIA_JIT_HELPER;

#ifdef PROFILING_SUPPORTED
    // Insert profiler tail call hook if needed.
    // Since we don't know the insertion point, pass null for second param.
    if (comp->compIsProfilerHookNeeded())
    {
        InsertProfTailCallHook(call, nullptr);
    }
#endif // PROFILING_SUPPORTED

    return result;
}

#ifndef TARGET_64BIT
//------------------------------------------------------------------------
// Lowering::DecomposeLongCompare: Decomposes a TYP_LONG compare node.
//
// Arguments:
//    cmp - the compare node
//
// Return Value:
//    The next node to lower.
//
// Notes:
//    This is done during lowering because DecomposeLongs handles only nodes
//    that produce TYP_LONG values. Compare nodes may consume TYP_LONG values
//    but produce TYP_INT values.
//
GenTree* Lowering::DecomposeLongCompare(GenTree* cmp)
{
    assert(cmp->gtGetOp1()->TypeGet() == TYP_LONG);

    GenTree* src1 = cmp->gtGetOp1();
    GenTree* src2 = cmp->gtGetOp2();
    assert(src1->OperIs(GT_LONG));
    assert(src2->OperIs(GT_LONG));
    GenTree* loSrc1 = src1->gtGetOp1();
    GenTree* hiSrc1 = src1->gtGetOp2();
    GenTree* loSrc2 = src2->gtGetOp1();
    GenTree* hiSrc2 = src2->gtGetOp2();
    BlockRange().Remove(src1);
    BlockRange().Remove(src2);

    genTreeOps condition = cmp->OperGet();
    GenTree*   loCmp;
    GenTree*   hiCmp;

    if (cmp->OperIs(GT_EQ, GT_NE))
    {
        //
        // Transform (x EQ|NE y) into (((x.lo XOR y.lo) OR (x.hi XOR y.hi)) EQ|NE 0). If y is 0 then this can
        // be reduced to just ((x.lo OR x.hi) EQ|NE 0). The OR is expected to set the condition flags so we
        // don't need to generate a redundant compare against 0, we only generate a SETCC|JCC instruction.
        //
        // XOR is used rather than SUB because it is commutative and thus allows swapping the operands when
        // the first happens to be a constant. Usually only the second compare operand is a constant but it's
        // still possible to have a constant on the left side. For example, when src1 is a uint->ulong cast
        // then hiSrc1 would be 0.
        //

        if (loSrc1->OperIs(GT_CNS_INT))
        {
            std::swap(loSrc1, loSrc2);
        }

        if (loSrc2->IsIntegralConst(0))
        {
            BlockRange().Remove(loSrc2);
            loCmp = loSrc1;
        }
        else
        {
            loCmp = comp->gtNewOperNode(GT_XOR, TYP_INT, loSrc1, loSrc2);
            BlockRange().InsertBefore(cmp, loCmp);
            ContainCheckBinary(loCmp->AsOp());
        }

        if (hiSrc1->OperIs(GT_CNS_INT))
        {
            std::swap(hiSrc1, hiSrc2);
        }

        if (hiSrc2->IsIntegralConst(0))
        {
            BlockRange().Remove(hiSrc2);
            hiCmp = hiSrc1;
        }
        else
        {
            hiCmp = comp->gtNewOperNode(GT_XOR, TYP_INT, hiSrc1, hiSrc2);
            BlockRange().InsertBefore(cmp, hiCmp);
            ContainCheckBinary(hiCmp->AsOp());
        }

        hiCmp = comp->gtNewOperNode(GT_OR, TYP_INT, loCmp, hiCmp);
        BlockRange().InsertBefore(cmp, hiCmp);
        ContainCheckBinary(hiCmp->AsOp());
    }
    else
    {
        assert(cmp->OperIs(GT_LT, GT_LE, GT_GE, GT_GT));

        //
        // If the compare is signed then (x LT|GE y) can be transformed into ((x SUB y) LT|GE 0).
        // If the compare is unsigned we can still use SUB but we need to check the Carry flag,
        // not the actual result. In both cases we can simply check the appropiate condition flags
        // and ignore the actual result:
        //     SUB_LO loSrc1, loSrc2
        //     SUB_HI hiSrc1, hiSrc2
        //     SETCC|JCC (signed|unsigned LT|GE)
        // If loSrc2 happens to be 0 then the first SUB can be eliminated and the second one can
        // be turned into a CMP because the first SUB would have set carry to 0. This effectively
        // transforms a long compare against 0 into an int compare of the high part against 0.
        //
        // (x LE|GT y) can to be transformed into ((x SUB y) LE|GT 0) but checking that a long value
        // is greater than 0 is not so easy. We need to turn this into a positive/negative check
        // like the one we get for LT|GE compares, this can be achieved by swapping the compare:
        //     (x LE|GT y) becomes (y GE|LT x)
        //
        // Having to swap operands is problematic when the second operand is a constant. The constant
        // moves to the first operand where it cannot be contained and thus needs a register. This can
        // be avoided by changing the constant such that LE|GT becomes LT|GE:
        //     (x LE|GT 41) becomes (x LT|GE 42)
        //

        if (cmp->OperIs(GT_LE, GT_GT))
        {
            bool mustSwap = true;

            if (loSrc2->OperIs(GT_CNS_INT) && hiSrc2->OperIs(GT_CNS_INT))
            {
                uint32_t loValue  = static_cast<uint32_t>(loSrc2->AsIntCon()->IconValue());
                uint32_t hiValue  = static_cast<uint32_t>(hiSrc2->AsIntCon()->IconValue());
                uint64_t value    = static_cast<uint64_t>(loValue) | (static_cast<uint64_t>(hiValue) << 32);
                uint64_t maxValue = cmp->IsUnsigned() ? UINT64_MAX : INT64_MAX;

                if (value != maxValue)
                {
                    value++;
                    loValue = value & UINT32_MAX;
                    hiValue = (value >> 32) & UINT32_MAX;
                    loSrc2->AsIntCon()->SetIconValue(loValue);
                    hiSrc2->AsIntCon()->SetIconValue(hiValue);

                    condition = cmp->OperIs(GT_LE) ? GT_LT : GT_GE;
                    mustSwap  = false;
                }
            }

            if (mustSwap)
            {
                std::swap(loSrc1, loSrc2);
                std::swap(hiSrc1, hiSrc2);
                condition = GenTree::SwapRelop(condition);
            }
        }

        assert((condition == GT_LT) || (condition == GT_GE));

        if (loSrc2->IsIntegralConst(0))
        {
            BlockRange().Remove(loSrc2);

            // Very conservative dead code removal... but it helps.

            if (loSrc1->OperIs(GT_CNS_INT, GT_LCL_VAR, GT_LCL_FLD))
            {
                BlockRange().Remove(loSrc1);
            }
            else
            {
                loSrc1->SetUnusedValue();
            }

            hiCmp = comp->gtNewOperNode(GT_CMP, TYP_VOID, hiSrc1, hiSrc2);
            BlockRange().InsertBefore(cmp, hiCmp);
            ContainCheckCompare(hiCmp->AsOp());
        }
        else
        {
            loCmp = comp->gtNewOperNode(GT_CMP, TYP_VOID, loSrc1, loSrc2);
            hiCmp = comp->gtNewOperNode(GT_SUB_HI, TYP_INT, hiSrc1, hiSrc2);
            BlockRange().InsertBefore(cmp, loCmp, hiCmp);
            ContainCheckCompare(loCmp->AsOp());
            ContainCheckBinary(hiCmp->AsOp());

            //
            // Try to move the first SUB_HI operands right in front of it, this allows using
            // a single temporary register instead of 2 (one for CMP and one for SUB_HI). Do
            // this only for locals as they won't change condition flags. Note that we could
            // move constants (except 0 which generates XOR reg, reg) but it's extremely rare
            // to have a constant as the first operand.
            //

            if (hiSrc1->OperIs(GT_LCL_VAR, GT_LCL_FLD))
            {
                BlockRange().Remove(hiSrc1);
                BlockRange().InsertBefore(hiCmp, hiSrc1);
            }
        }
    }

    hiCmp->gtFlags |= GTF_SET_FLAGS;
    if (hiCmp->IsValue())
    {
        hiCmp->SetUnusedValue();
    }

    LIR::Use cmpUse;
    if (BlockRange().TryGetUse(cmp, &cmpUse) && cmpUse.User()->OperIs(GT_JTRUE))
    {
        BlockRange().Remove(cmp);

        GenTree* jcc       = cmpUse.User();
        jcc->AsOp()->gtOp1 = nullptr;
        jcc->ChangeOper(GT_JCC);
        jcc->gtFlags |= GTF_USE_FLAGS;
        jcc->AsCC()->gtCondition = GenCondition::FromIntegralRelop(condition, cmp->IsUnsigned());
    }
    else
    {
        cmp->AsOp()->gtOp1 = nullptr;
        cmp->AsOp()->gtOp2 = nullptr;
        cmp->ChangeOper(GT_SETCC);
        cmp->gtFlags |= GTF_USE_FLAGS;
        cmp->AsCC()->gtCondition = GenCondition::FromIntegralRelop(condition, cmp->IsUnsigned());
    }

    return cmp->gtNext;
}
#endif // !TARGET_64BIT

#ifndef TARGET_ARM64
//------------------------------------------------------------------------
// Lowering::OptimizeConstCompare: Performs various "compare with const" optimizations.
//
// Arguments:
//    cmp - the compare node
//
// Return Value:
//    The original compare node if lowering should proceed as usual or the next node
//    to lower if the compare node was changed in such a way that lowering is no
//    longer needed.
//
// Notes:
//    - Narrow operands to enable memory operand containment (XARCH specific).
//    - Transform cmp(and(x, y), 0) into test(x, y) (XARCH/Arm64 specific but could
//      be used for ARM as well if support for GT_TEST_EQ/GT_TEST_NE is added).
//    - Transform TEST(x, LSH(1, y)) into BT(x, y) (XARCH specific)
//    - Transform RELOP(OP, 0) into SETCC(OP) or JCC(OP) if OP can set the
//      condition flags appropriately (XARCH/ARM64 specific but could be extended
//      to ARM32 as well if ARM32 codegen supports GTF_SET_FLAGS).
//
GenTree* Lowering::OptimizeConstCompare(GenTree* cmp)
{
    assert(cmp->gtGetOp2()->IsIntegralConst());

#if defined(TARGET_XARCH)
    GenTree*       op1      = cmp->gtGetOp1();
    GenTreeIntCon* op2      = cmp->gtGetOp2()->AsIntCon();
    ssize_t        op2Value = op2->IconValue();

    var_types op1Type = op1->TypeGet();
    if (IsContainableMemoryOp(op1) && varTypeIsSmall(op1Type) && genSmallTypeCanRepresentValue(op1Type, op2Value))
    {
        //
        // If op1's type is small then try to narrow op2 so it has the same type as op1.
        // Small types are usually used by memory loads and if both compare operands have
        // the same type then the memory load can be contained. In certain situations
        // (e.g "cmp ubyte, 200") we also get a smaller instruction encoding.
        //

        op2->gtType = op1Type;
    }
    else if (op1->OperIs(GT_CAST) && !op1->gtOverflow())
    {
        GenTreeCast* cast       = op1->AsCast();
        var_types    castToType = cast->CastToType();
        GenTree*     castOp     = cast->gtGetOp1();

        if (((castToType == TYP_BOOL) || (castToType == TYP_UBYTE)) && FitsIn<UINT8>(op2Value))
        {
            //
            // Since we're going to remove the cast we need to be able to narrow the cast operand
            // to the cast type. This can be done safely only for certain opers (e.g AND, OR, XOR).
            // Some opers just can't be narrowed (e.g DIV, MUL) while other could be narrowed but
            // doing so would produce incorrect results (e.g. RSZ, RSH).
            //
            // The below list of handled opers is conservative but enough to handle the most common
            // situations. In particular this include CALL, sometimes the JIT unnecessarilly widens
            // the result of bool returning calls.
            //
            bool removeCast =
                (castOp->OperIs(GT_CALL, GT_LCL_VAR) || castOp->OperIsLogical() || IsContainableMemoryOp(castOp));

            if (removeCast)
            {
                assert(!castOp->gtOverflowEx()); // Must not be an overflow checking operation

                castOp->SetType(castToType);
                op2->SetType(castToType);

                // If we have any contained memory ops on castOp, they must now not be contained.
                if (castOp->OperIsLogical())
                {
                    GenTree* op1 = castOp->gtGetOp1();
                    if ((op1 != nullptr) && !op1->IsCnsIntOrI())
                    {
                        op1->ClearContained();
                    }
                    GenTree* op2 = castOp->gtGetOp2();
                    if ((op2 != nullptr) && !op2->IsCnsIntOrI())
                    {
                        op2->ClearContained();
                    }
                }
                cmp->AsOp()->gtOp1 = castOp;

                BlockRange().Remove(cast);
            }
        }
    }
    else if (op1->OperIs(GT_AND) && cmp->OperIs(GT_EQ, GT_NE))
    {
        //
        // Transform ((x AND y) EQ|NE 0) into (x TEST_EQ|TEST_NE y) when possible.
        //

        GenTree* andOp1 = op1->gtGetOp1();
        GenTree* andOp2 = op1->gtGetOp2();

        if (op2Value != 0)
        {
            //
            // If we don't have a 0 compare we can get one by transforming ((x AND mask) EQ|NE mask)
            // into ((x AND mask) NE|EQ 0) when mask is a single bit.
            //

            if (isPow2<target_size_t>(static_cast<target_size_t>(op2Value)) && andOp2->IsIntegralConst(op2Value))
            {
                op2Value = 0;
                op2->SetIconValue(0);
                cmp->SetOperRaw(GenTree::ReverseRelop(cmp->OperGet()));
            }
        }

        if (op2Value == 0)
        {
            BlockRange().Remove(op1);
            BlockRange().Remove(op2);

            cmp->SetOperRaw(cmp->OperIs(GT_EQ) ? GT_TEST_EQ : GT_TEST_NE);
            cmp->AsOp()->gtOp1 = andOp1;
            cmp->AsOp()->gtOp2 = andOp2;
            // We will re-evaluate containment below
            andOp1->ClearContained();
            andOp2->ClearContained();

            if (IsContainableMemoryOp(andOp1) && andOp2->IsIntegralConst())
            {
                //
                // For "test" we only care about the bits that are set in the second operand (mask).
                // If the mask fits in a small type then we can narrow both operands to generate a "test"
                // instruction with a smaller encoding ("test" does not have a r/m32, imm8 form) and avoid
                // a widening load in some cases.
                //
                // For 16 bit operands we narrow only if the memory operand is already 16 bit. This matches
                // the behavior of a previous implementation and avoids adding more cases where we generate
                // 16 bit instructions that require a length changing prefix (0x66). These suffer from
                // significant decoder stalls on Intel CPUs.
                //
                // We could also do this for 64 bit masks that fit into 32 bit but it doesn't help.
                // In such cases morph narrows down the existing GT_AND by inserting a cast between it and
                // the memory operand so we'd need to add more code to recognize and eliminate that cast.
                //

                size_t mask = static_cast<size_t>(andOp2->AsIntCon()->IconValue());

                if (FitsIn<UINT8>(mask))
                {
                    andOp1->gtType = TYP_UBYTE;
                    andOp2->gtType = TYP_UBYTE;
                }
                else if (FitsIn<UINT16>(mask) && genTypeSize(andOp1) == 2)
                {
                    andOp1->gtType = TYP_USHORT;
                    andOp2->gtType = TYP_USHORT;
                }
            }
        }
    }

    if (cmp->OperIs(GT_TEST_EQ, GT_TEST_NE))
    {
        //
        // Transform TEST_EQ|NE(x, LSH(1, y)) into BT(x, y) when possible. Using BT
        // results in smaller and faster code. It also doesn't have special register
        // requirements, unlike LSH that requires the shift count to be in ECX.
        // Note that BT has the same behavior as LSH when the bit index exceeds the
        // operand bit size - it uses (bit_index MOD bit_size).
        //

        GenTree* lsh = cmp->gtGetOp2();
        LIR::Use cmpUse;

        if (lsh->OperIs(GT_LSH) && varTypeIsIntOrI(lsh->TypeGet()) && lsh->gtGetOp1()->IsIntegralConst(1) &&
            BlockRange().TryGetUse(cmp, &cmpUse))
        {
            GenCondition condition = cmp->OperIs(GT_TEST_NE) ? GenCondition::C : GenCondition::NC;

            cmp->SetOper(GT_BT);
            cmp->gtType = TYP_VOID;
            cmp->gtFlags |= GTF_SET_FLAGS;
            cmp->AsOp()->gtOp2 = lsh->gtGetOp2();
            cmp->gtGetOp2()->ClearContained();

            BlockRange().Remove(lsh->gtGetOp1());
            BlockRange().Remove(lsh);

            GenTreeCC* cc;

            if (cmpUse.User()->OperIs(GT_JTRUE))
            {
                cmpUse.User()->ChangeOper(GT_JCC);
                cc              = cmpUse.User()->AsCC();
                cc->gtCondition = condition;
            }
            else
            {
                cc = new (comp, GT_SETCC) GenTreeCC(GT_SETCC, condition, TYP_INT);
                BlockRange().InsertAfter(cmp, cc);
                cmpUse.ReplaceWith(comp, cc);
            }

            cc->gtFlags |= GTF_USE_FLAGS;

            return cmp->gtNext;
        }
    }
    else if (cmp->OperIs(GT_EQ, GT_NE))
    {
        GenTree* op1 = cmp->gtGetOp1();
        GenTree* op2 = cmp->gtGetOp2();

        // TODO-CQ: right now the below peep is inexpensive and gets the benefit in most
        // cases because in majority of cases op1, op2 and cmp would be in that order in
        // execution. In general we should be able to check that all the nodes that come
        // after op1 do not modify the flags so that it is safe to avoid generating a
        // test instruction.

        if (op2->IsIntegralConst(0) && (op1->gtNext == op2) && (op2->gtNext == cmp) &&
            op1->OperIs(GT_AND, GT_OR, GT_XOR, GT_ADD, GT_SUB, GT_NEG))
        {
            op1->gtFlags |= GTF_SET_FLAGS;
            op1->SetUnusedValue();

            BlockRange().Remove(op2);

            GenTree*   next = cmp->gtNext;
            GenTree*   cc;
            genTreeOps ccOp;
            LIR::Use   cmpUse;

            // Fast check for the common case - relop used by a JTRUE that immediately follows it.
            if ((next != nullptr) && next->OperIs(GT_JTRUE) && (next->gtGetOp1() == cmp))
            {
                cc   = next;
                ccOp = GT_JCC;
                next = nullptr;
                BlockRange().Remove(cmp);
            }
            else if (BlockRange().TryGetUse(cmp, &cmpUse) && cmpUse.User()->OperIs(GT_JTRUE))
            {
                cc   = cmpUse.User();
                ccOp = GT_JCC;
                next = nullptr;
                BlockRange().Remove(cmp);
            }
            else // The relop is not used by a JTRUE or it is not used at all.
            {
                // Transform the relop node it into a SETCC. If it's not used we could remove
                // it completely but that means doing more work to handle a rare case.
                cc   = cmp;
                ccOp = GT_SETCC;
            }

            GenCondition condition = GenCondition::FromIntegralRelop(cmp);
            cc->ChangeOper(ccOp);
            cc->AsCC()->gtCondition = condition;
            cc->gtFlags |= GTF_USE_FLAGS;

            return next;
        }
    }
#endif // defined(TARGET_XARCH)

    return cmp;
}

//------------------------------------------------------------------------
// Lowering::LowerCompare: Lowers a compare node.
//
// Arguments:
//    cmp - the compare node
//
// Return Value:
//    The next node to lower.
//
GenTree* Lowering::LowerCompare(GenTree* cmp)
{
#ifndef TARGET_64BIT
    if (cmp->gtGetOp1()->TypeGet() == TYP_LONG)
    {
        return DecomposeLongCompare(cmp);
    }
#endif

    if (cmp->gtGetOp2()->IsIntegralConst() && !comp->opts.MinOpts())
    {
        GenTree* next = OptimizeConstCompare(cmp);

        // If OptimizeConstCompare return the compare node as "next" then we need to continue lowering.
        if (next != cmp)
        {
            return next;
        }
    }

#ifdef TARGET_XARCH
    if (cmp->gtGetOp1()->TypeGet() == cmp->gtGetOp2()->TypeGet())
    {
        if (varTypeIsSmall(cmp->gtGetOp1()->TypeGet()) && varTypeIsUnsigned(cmp->gtGetOp1()->TypeGet()))
        {
            //
            // If both operands have the same type then codegen will use the common operand type to
            // determine the instruction type. For small types this would result in performing a
            // signed comparison of two small unsigned values without zero extending them to TYP_INT
            // which is incorrect. Note that making the comparison unsigned doesn't imply that codegen
            // has to generate a small comparison, it can still correctly generate a TYP_INT comparison.
            //

            cmp->gtFlags |= GTF_UNSIGNED;
        }
    }
#endif // TARGET_XARCH
    ContainCheckCompare(cmp->AsOp());
    return cmp->gtNext;
}

//------------------------------------------------------------------------
// Lowering::LowerJTrue: Lowers a JTRUE node.
//
// Arguments:
//    jtrue - the JTRUE node
//
// Return Value:
//    The next node to lower (usually nullptr).
//
GenTree* Lowering::LowerJTrue(GenTreeUnOp* jtrue)
{
    ContainCheckJTrue(jtrue);

    assert(jtrue->gtNext == nullptr);
    return nullptr;
}
#endif // !TARGET_ARM64

//----------------------------------------------------------------------------------------------
// LowerNodeCC: Lowers a node that produces a boolean value by setting the condition flags.
//
// Arguments:
//     node - The node to lower
//     condition - The condition code of the generated SETCC/JCC node
//
// Return Value:
//     A SETCC/JCC node or nullptr if `node` is not used.
//
// Notes:
//     This simply replaces `node`'s use with an appropiate SETCC/JCC node,
//     `node` is not actually changed, except by having its GTF_SET_FLAGS set.
//     It's the caller's responsibility to change `node` such that it only
//     sets the condition flags, without producing a boolean value.
//
GenTreeCC* Lowering::LowerNodeCC(GenTree* node, GenCondition condition)
{
    // Skip over a chain of EQ/NE(x, 0) relops. This may be present either
    // because `node` is not a relop and so it cannot be used directly by a
    // JTRUE, or because the frontend failed to remove a EQ/NE(x, 0) that's
    // used as logical negation.
    //
    // Usually there's only one such relop but there's little difference
    // between removing one or all so we may as well remove them all.
    //
    // We can't allow any other nodes between `node` and its user because we
    // have no way of knowing if those nodes change flags or not. So we're looking
    // to skip over a sequence of appropriately connected zero and EQ/NE nodes.

    // The x in EQ/NE(x, 0)
    GenTree* relop = node;
    // The first node of the relop sequence
    GenTree* first = node->gtNext;
    // The node following the relop sequence
    GenTree* next = first;

    while ((next != nullptr) && next->IsIntegralConst(0) && (next->gtNext != nullptr) &&
           next->gtNext->OperIs(GT_EQ, GT_NE) && (next->gtNext->AsOp()->gtGetOp1() == relop) &&
           (next->gtNext->AsOp()->gtGetOp2() == next))
    {
        relop = next->gtNext;
        next  = relop->gtNext;

        if (relop->OperIs(GT_EQ))
        {
            condition = GenCondition::Reverse(condition);
        }
    }

    GenTreeCC* cc = nullptr;

    // Next may be null if `node` is not used. In that case we don't need to generate a SETCC node.
    if (next != nullptr)
    {
        if (next->OperIs(GT_JTRUE))
        {
            // If the instruction immediately following 'relop', i.e. 'next' is a conditional branch,
            // it should always have 'relop' as its 'op1'. If it doesn't, then we have improperly
            // constructed IL (the setting of a condition code should always immediately precede its
            // use, since the JIT doesn't track dataflow for condition codes). Still, if it happens
            // it's not our problem, it simply means that `node` is not used and can be removed.
            if (next->AsUnOp()->gtGetOp1() == relop)
            {
                assert(relop->OperIsCompare());

                next->ChangeOper(GT_JCC);
                cc              = next->AsCC();
                cc->gtCondition = condition;
            }
        }
        else
        {
            // If the node is used by something other than a JTRUE then we need to insert a
            // SETCC node to materialize the boolean value.
            LIR::Use use;

            if (BlockRange().TryGetUse(relop, &use))
            {
                cc = new (comp, GT_SETCC) GenTreeCC(GT_SETCC, condition, TYP_INT);
                BlockRange().InsertAfter(node, cc);
                use.ReplaceWith(comp, cc);
            }
        }
    }

    if (cc != nullptr)
    {
        node->gtFlags |= GTF_SET_FLAGS;
        cc->gtFlags |= GTF_USE_FLAGS;
    }

    // Remove the chain of EQ/NE(x, 0) relop nodes, if any. Note that if a SETCC was
    // inserted after `node`, `first` still points to the node that was initially
    // after `node`.
    if (relop != node)
    {
        BlockRange().Remove(first, relop);
    }

    return cc;
}

// Lower "jmp <method>" tail call to insert PInvoke method epilog if required.
void Lowering::LowerJmpMethod(GenTree* jmp)
{
    assert(jmp->OperGet() == GT_JMP);

    JITDUMP("lowering GT_JMP\n");
    DISPNODE(jmp);
    JITDUMP("============");

    // If PInvokes are in-lined, we have to remember to execute PInvoke method epilog anywhere that
    // a method returns.
    if (comp->compMethodRequiresPInvokeFrame())
    {
        InsertPInvokeMethodEpilog(comp->compCurBB DEBUGARG(jmp));
    }
}

void Lowering::LowerRet(GenTreeUnOp* ret)
{
    assert(ret->OperIs(GT_RETURN));

    JITDUMPTREE(ret, "Lowering RETURN:\n");

    if (ret->TypeIs(TYP_STRUCT))
    {
        LowerRetStruct(ret);
    }

    // Method doing PInvokes has exactly one return block unless it has tail calls.
    if (comp->compMethodRequiresPInvokeFrame() && (comp->compCurBB == comp->genReturnBB))
    {
        InsertPInvokeMethodEpilog(comp->compCurBB DEBUGARG(ret));
    }

    if (!ret->TypeIs(TYP_VOID))
    {
        ContainCheckRet(ret);
    }
}

void Lowering::LowerLclVar(GenTreeLclVar* lclVar)
{
    assert(lclVar->OperIs(GT_LCL_VAR));
    assert(!lclVar->IsMultiReg());
    assert(!comp->lvaGetDesc(lclVar)->IsIndependentPromoted());
}

void Lowering::LowerStoreLclVar(GenTreeLclVar* store)
{
    assert(store->OperIs(GT_STORE_LCL_VAR));
    assert(!store->GetOp(0)->OperIs(GT_PHI));

    JITDUMP("Lowering STORE_LCL_VAR (before):\n");
    DISPTREERANGE(BlockRange(), store);
    JITDUMP("\n");

    GenTree*   src = store->GetOp(0);
    LclVarDsc* lcl = comp->lvaGetDesc(store);

    bool srcIsMultiReg = src->IsMultiRegNode();

#if FEATURE_MULTIREG_RET
    if (srcIsMultiReg)
    {
        const ReturnTypeDesc* retTypeDesc = nullptr;

        if (src->OperIs(GT_CALL))
        {
            retTypeDesc = src->AsCall()->GetRetDesc();
        }

        MakeMultiRegLclVar(store, retTypeDesc);
    }
#endif

    assert(!lcl->IsIndependentPromoted() || store->IsMultiReg());

    if (!src->TypeIs(TYP_STRUCT) && (varTypeUsesFloatReg(store->GetType()) != varTypeUsesFloatReg(src->GetType())))
    {
        if (m_lsra->isRegCandidate(lcl))
        {
            GenTreeUnOp* bitcast = comp->gtNewBitCastNode(store->GetType(), src);
            store->SetOp(0, bitcast);
            BlockRange().InsertBefore(store, bitcast);
            LowerBitCast(bitcast);
            src = bitcast;
        }
        else
        {
            // This is an actual store, we'll just retype it.
            store->SetType(src->GetType());
        }
    }

    if (store->TypeIs(TYP_STRUCT) && !srcIsMultiReg)
    {
        if (GenTreeCall* call = src->IsCall())
        {
            ClassLayout* layout  = lcl->GetLayout();
            var_types    regType = layout->GetRegisterType();

#ifdef DEBUG
#if defined(TARGET_XARCH) && !defined(UNIX_AMD64_ABI)
            // Windows x64 doesn't have multireg returns,
            // x86 uses it only for long return type, not for structs.
            assert(layout->GetSlotCount() == 1);
            assert(regType != TYP_UNDEF);
#else
            if (!lcl->lvIsHfa())
            {
                if (layout->GetSlotCount() > 1)
                {
                    assert(call->HasMultiRegRetVal());
                }
                else
                {
                    unsigned size = layout->GetSize();

                    assert((size <= 8) || (size == 16));

                    bool isPowerOf2    = (((size - 1) & size) == 0);
                    bool isTypeDefined = (regType != TYP_UNDEF);

                    assert(isPowerOf2 == isTypeDefined);
                }
            }
#endif
#endif // DEBUG

#if !defined(WINDOWS_AMD64_ABI)
            if (!call->HasMultiRegRetVal() && (regType == TYP_UNDEF))
            {
                // If we have a single return register,
                // but we can't retype it as a primitive type, we must spill it.

                store->SetOp(0, SpillStructCallResult(call));
                JITDUMP("lowering store lcl var/field has to spill call src.\n");
                LowerStoreLclVar(store);

                return;
            }
#endif // !WINDOWS_AMD64_ABI
        }
        else if (!src->OperIs(GT_LCL_VAR) || !lcl->IsEnregisterable())
        {
            StructStoreKind kind = GetStructStoreKind(true, lcl->GetLayout(), src);
            LowerStructStore(store, kind, lcl->GetLayout());

            return;
        }
    }

    LowerStoreLclVarArch(store);

    JITDUMP("Lowering STORE_LCL_VAR (after):\n");
    DISPTREERANGE(BlockRange(), store);
    JITDUMP("\n");
}

void Lowering::LowerLclFld(GenTreeLclFld* lclFld)
{
    assert(lclFld->OperIs(GT_LCL_FLD));

    verifyLclFldDoNotEnregister(lclFld->GetLclNum());
}

void Lowering::LowerStoreLclFld(GenTreeLclFld* store)
{
    assert(store->OperIs(GT_STORE_LCL_FLD));

    verifyLclFldDoNotEnregister(store->GetLclNum());

    GenTree* value = store->GetOp(0);

    if (store->TypeIs(TYP_STRUCT))
    {
        if (GenTreeCall* call = value->IsCall())
        {
            assert(call->GetRegCount() == 1);
            var_types regType = call->GetRegType(0);

            if (varTypeSize(regType) <= store->GetLayout(comp)->GetSize())
            {
                store->SetType(regType);
                return;
            }

#if defined(WINDOWS_AMD64_ABI)
            // All ABI except Windows x64 supports passing 3 byte structs in registers.
            // Other 64 bites ABI-s support passing 5, 6, 7 byte structs.
            unreached();
#else
            value = SpillStructCallResult(call);
            store->SetOp(0, value);
#endif
        }

        ClassLayout*    layout = store->GetLayout(comp);
        StructStoreKind kind   = GetStructStoreKind(true, layout, value);
        LowerStructStore(store, kind, layout);

        return;
    }

    assert(varTypeUsesFloatReg(store->GetType()) == varTypeUsesFloatReg(value->GetType()));

#ifdef TARGET_XARCH
    if (varTypeIsByte(store->GetType()) && (value->OperIsCompare() || value->OperIs(GT_SETCC)))
    {
        value->SetType(store->GetType());
    }
#endif

    ContainCheckStoreLcl(store);
}

void Lowering::LowerRetStruct(GenTreeUnOp* ret)
{
    assert(ret->OperIs(GT_RETURN) && ret->TypeIs(TYP_STRUCT));

    GenTree* src = ret->GetOp(0);

    if (GenTreeFieldList* fieldList = src->IsFieldList())
    {
#ifdef FEATURE_HW_INTRINSICS
        for (GenTreeFieldList::Use& use : fieldList->Uses())
        {
            // Workaround poor register allocation on linux-x64 - if the returned value is already in XMM0
            // then attempting to extract its elements to XMM0 and XMM1 results in a spill to temp because
            // the first extract kills the value in XMM0, which is then needed again to extract to XMM1.
            // At this point we don't really care about the precise type - FLOAT/DOUBLE/SIMDn - we only care
            // that the value is in an XMM registers so we can get rid of the extract to XMM0.
            // This doesn't appear to be a problem on arm64 but that may simply be due to more registers
            // being available, otherwise there's nothing to suggest that arm64 doesn't have the same issue.

            if (GenTreeHWIntrinsic* extract = use.GetNode()->IsHWIntrinsic())
            {
                if ((extract->GetIntrinsic() == NI_Vector128_GetElement) && extract->GetOp(1)->IsIntegralConst(0) &&
                    varTypeUsesFloatReg(extract->GetType()) && varTypeUsesFloatReg(extract->GetOp(0)->GetType()))
                {
                    GenTree* vec = extract->GetOp(0);
                    vec->ClearContained();
                    use.SetNode(vec);
                    BlockRange().Remove(extract->GetOp(1));
                    BlockRange().Remove(extract);
                }
            }
        }
#endif // FEATURE_HW_INTRINSICS

        return;
    }

    assert(comp->info.retDesc.GetRegCount() == 1);

#ifdef DEBUG
    if (!varTypeIsStruct(src->GetType()))
    {
        var_types retActualType = varActualType(comp->info.retDesc.GetRegType(0));
        var_types srcActualType = varActualType(src->GetType());

        bool constStructInit                  = src->IsConstInitVal();
        bool implicitCastFromSameOrBiggerSize = varTypeSize(retActualType) <= varTypeSize(srcActualType);

        // This could happen if we have retyped op1 as a primitive type during struct promotion.
        bool actualTypesMatch = (retActualType == srcActualType);

        assert(actualTypesMatch || constStructInit || implicitCastFromSameOrBiggerSize);
    }
#endif // DEBUG

    if (src->OperIs(GT_IND, GT_OBJ))
    {
        var_types    retRegType = comp->info.retDesc.GetRegType(0);
        ClassLayout* retLayout  = comp->info.GetRetLayout();

        if (retLayout->GetSize() == varTypeSize(retRegType))
        {
            if (varTypeIsSmall(retRegType))
            {
                retRegType = varTypeSignedToUnsigned(retRegType);
            }

            src->ChangeOper(GT_IND);
            src->SetType(retRegType);

            LowerIndir(src->AsIndir());
        }
        else
        {
#if defined(TARGET_X86) || defined(WINDOWS_AMD64_ABI)
            unreached();
#else
            assert(retLayout->GetSize() < varTypeSize(retRegType));

            unsigned tempLclNum = comp->lvaNewTemp(retLayout, true DEBUGARG("indir ret temp"));
            comp->lvaSetVarDoNotEnregister(tempLclNum DEBUGARG(Compiler::DNER_LocalField));

            GenTree* retRegValue = comp->gtNewLclFldNode(tempLclNum, retRegType, 0);
            ret->SetOp(0, retRegValue);
            BlockRange().InsertBefore(ret, retRegValue);

            GenTreeLclVar* tempStore = comp->gtNewStoreLclVar(tempLclNum, src->GetType(), src);
            BlockRange().InsertAfter(src, tempStore);

            src->ChangeOper(GT_OBJ);
            src->AsObj()->SetLayout(retLayout);

            LowerStoreLclVar(tempStore);
#endif
        }

        ret->SetType(varActualType(retRegType));

        return;
    }

    var_types retRegType = varActualType(comp->info.retDesc.GetRegType(0));
    ret->SetType(retRegType);

    switch (src->GetOper())
    {
        case GT_CALL:
            assert(src->TypeIs(retRegType)); // Type should be changed during call processing.
            break;

        case GT_LCL_VAR:
            LowerRetSingleRegStructLclVar(ret);
            break;

        case GT_LCL_FLD:
            assert(comp->lvaGetDesc(src->AsLclFld())->lvDoNotEnregister);
            src->SetType(retRegType);
            break;

        case GT_CNS_INT:
        case GT_CNS_DBL:
            unreached();

        default:
            assert(!src->TypeIs(TYP_STRUCT));

            if (varTypeUsesFloatReg(ret->GetType()) != varTypeUsesFloatReg(src->GetType()))
            {
                GenTreeUnOp* bitcast = comp->gtNewBitCastNode(ret->GetType(), src);
                ret->SetOp(0, bitcast);
                BlockRange().InsertBefore(ret, bitcast);
                LowerBitCast(bitcast);
            }
            break;
    }
}

//----------------------------------------------------------------------------------------------
// LowerRetSingleRegStructLclVar: Lowers a return node with a struct lclVar as a source.
//
// Notes:
//    - the function is only for LclVars that are returned in one register;
//    - if LclVar is allocated in memory then read it as return type;
//    - if LclVar can be enregistered read it as register type and add a bitcast if necessary;
//
void Lowering::LowerRetSingleRegStructLclVar(GenTreeUnOp* ret)
{
    assert(ret->OperIs(GT_RETURN));
    assert(comp->info.retDesc.GetRegCount() == 1);

    GenTreeLclVar* lclVar = ret->GetOp(0)->AsLclVar();

    unsigned   lclNum = lclVar->GetLclNum();
    LclVarDsc* lcl    = comp->lvaGetDesc(lclNum);

    if (lcl->TypeIs(TYP_STRUCT))
    {
        // TODO-1stClassStructs: We can no longer promote or enregister this struct,
        // since it is referenced as a whole.
        comp->lvaSetVarDoNotEnregister(lclNum DEBUGARG(Compiler::DNER_BlockOp));
    }

    if (lcl->lvDoNotEnregister)
    {
        lclVar->ChangeOper(GT_LCL_FLD);
        lclVar->SetType(ret->GetType());
    }
    else
    {
        var_types lclVarType = lcl->GetRegisterType(lclVar);
        assert(lclVarType != TYP_UNDEF);
        lclVar->SetType(lclVarType);

        if (varTypeUsesFloatReg(ret->GetType()) != varTypeUsesFloatReg(lclVarType))
        {
            GenTreeUnOp* bitcast = comp->gtNewBitCastNode(ret->GetType(), lclVar);
            ret->SetOp(0, bitcast);
            BlockRange().InsertBefore(ret, bitcast);
            LowerBitCast(bitcast);
        }
    }
}

//----------------------------------------------------------------------------------------------
// LowerCallStruct: Lowers a call node that returns a stuct.
//
// Arguments:
//     call - The call node to lower.
//
// Notes:
//    - this handles only single-register returns;
//    - it transforms the call's user for `GT_STOREIND`.
//
void Lowering::LowerCallStruct(GenTreeCall* call)
{
    assert(varTypeIsStruct(call->GetType()));

    if (call->GetRegCount() > 1)
    {
        return;
    }

    var_types regType  = call->GetRegType(0);
    var_types callType = call->GetType();

    call->SetType(varActualType(regType));

    LIR::Use callUse;
    if (BlockRange().TryGetUse(call, &callUse))
    {
        GenTree* user = callUse.User();
        switch (user->OperGet())
        {
            case GT_RETURN:
            case GT_STORE_LCL_VAR:
            case GT_STORE_OBJ:
            case GT_STORE_LCL_FLD:
                // Leave as is, the user will handle it.
                assert(user->TypeIs(callType) || varTypeIsSIMD(user->GetType()));
                break;

            case GT_STOREIND:
#ifdef FEATURE_SIMD
                if (varTypeIsSIMD(user->GetType()))
                {
                    user->SetType(regType);
                    break;
                }
#endif // FEATURE_SIMD
                // importer has a separate mechanism to retype calls to helpers,
                // keep it for now.
                assert(user->TypeIs(TYP_REF) || (user->TypeIs(TYP_I_IMPL) && comp->IsTargetAbi(CORINFO_CORERT_ABI)));
                assert(call->IsHelperCall());
                assert(regType == user->GetType());
                break;

            default:
                unreached();
        }
    }
}

//----------------------------------------------------------------------------------------------
// LowerStoreSingleRegCallStruct: Lowers a store block where the source is a struct typed call.
//
// Arguments:
//     store - The store node to lower.
//
// Notes:
//    - the function is only for calls that return one register;
//    - it spills the call's result if it can be retyped as a primitive type;
//
void Lowering::LowerStoreSingleRegCallStruct(GenTreeObj* store)
{
    assert(varTypeIsStruct(store->GetType()));

    GenTreeCall* call = store->GetValue()->AsCall();
    assert(call->GetRegCount() == 1);
    var_types regType = call->GetRegType(0);

    if (varTypeSize(regType) <= store->GetLayout()->GetSize())
    {
        store->SetType(regType);
        store->SetOper(GT_STOREIND);
        LowerStoreIndirCommon(store->AsStoreInd());

        return;
    }

#if defined(WINDOWS_AMD64_ABI)
    // All ABI except Windows x64 supports passing 3 byte structs in registers.
    // Other 64 bites ABI-s support passing 5, 6, 7 byte structs.
    unreached();
#else
    store->SetValue(SpillStructCallResult(call));
    LowerStoreObj(store);
#endif
}

#if !defined(WINDOWS_AMD64_ABI)
//----------------------------------------------------------------------------------------------
// SpillStructCallResult: Spill call result to memory.
//
// Arguments:
//     call - call with 3, 5, 6 or 7 return size that has to be spilled to memory.
//
// Return Value:
//    load of the spilled variable.
//
GenTreeLclVar* Lowering::SpillStructCallResult(GenTreeCall* call)
{
    // TODO-1stClassStructs: we can support this in codegen for STORE_OBJ without new temps.
    unsigned       lclNum = comp->lvaNewTemp(call->GetRetLayout(), true DEBUGARG("odd sized struct call return temp"));
    GenTreeLclFld* store  = comp->gtNewStoreLclFld(call->GetType(), lclNum, 0, call);
    GenTreeLclVar* load   = comp->gtNewLclvNode(lclNum, TYP_STRUCT);
    BlockRange().InsertAfter(call, store, load);
    return load;
}
#endif // !WINDOWS_AMD64_ABI

GenTree* Lowering::LowerDirectCall(GenTreeCall* call)
{
    noway_assert(call->gtCallType == CT_USER_FUNC || call->gtCallType == CT_HELPER);

    // Don't support tail calling helper methods.
    // But we might encounter tail calls dispatched via JIT helper appear as a tail call to helper.
    noway_assert(!call->IsTailCall() || call->IsTailCallViaJitHelper() || call->gtCallType == CT_USER_FUNC);

    // Non-virtual direct/indirect calls: Work out if the address of the
    // call is known at JIT time.  If not it is either an indirect call
    // or the address must be accessed via an single/double indirection.

    void*           addr;
    InfoAccessType  accessType;
    CorInfoHelpFunc helperNum = comp->eeGetHelperNum(call->gtCallMethHnd);

#ifdef FEATURE_READYTORUN_COMPILER
    if (call->gtEntryPoint.addr != nullptr)
    {
        accessType = call->gtEntryPoint.accessType;
        addr       = call->gtEntryPoint.addr;
    }
    else
#endif
        if (call->gtCallType == CT_HELPER)
    {
        noway_assert(helperNum != CORINFO_HELP_UNDEF);

        // the convention on getHelperFtn seems to be (it's not documented)
        // that it returns an address or if it returns null, pAddr is set to
        // another address, which requires an indirection
        void* pAddr;
        addr = comp->info.compCompHnd->getHelperFtn(helperNum, (void**)&pAddr);

        if (addr != nullptr)
        {
            assert(pAddr == nullptr);
            accessType = IAT_VALUE;
        }
        else
        {
            accessType = IAT_PVALUE;
            addr       = pAddr;
        }
    }
    else
    {
        noway_assert(helperNum == CORINFO_HELP_UNDEF);

        CORINFO_ACCESS_FLAGS aflags = CORINFO_ACCESS_ANY;

        if (call->IsSameThis())
        {
            aflags = (CORINFO_ACCESS_FLAGS)(aflags | CORINFO_ACCESS_THIS);
        }

        if (!call->NeedsNullCheck())
        {
            aflags = (CORINFO_ACCESS_FLAGS)(aflags | CORINFO_ACCESS_NONNULL);
        }

        CORINFO_CONST_LOOKUP addrInfo;
        comp->info.compCompHnd->getFunctionEntryPoint(call->gtCallMethHnd, &addrInfo, aflags);

        accessType = addrInfo.accessType;
        addr       = addrInfo.addr;
    }

    GenTree* result = nullptr;
    switch (accessType)
    {
        case IAT_VALUE:
            // Non-virtual direct call to known address.
            // For JIT helper based tailcall (only used on x86) the target
            // address is passed as an arg to the helper so we want a node for
            // it.
            if (!IsCallTargetInRange(addr) || call->IsTailCallViaJitHelper())
            {
                result = AddrGen(addr);
            }
            else
            {
                // a direct call within range of hardware relative call instruction
                // stash the address for codegen
                call->gtDirectCallAddress = addr;
            }
            break;

        case IAT_PVALUE:
        {
            bool isR2RRelativeIndir = false;
#if defined(FEATURE_READYTORUN_COMPILER) && defined(TARGET_ARMARCH)
            // Skip inserting the indirection node to load the address that is already
            // computed in REG_R2R_INDIRECT_PARAM as a hidden parameter. Instead during the
            // codegen, just load the call target from REG_R2R_INDIRECT_PARAM.
            isR2RRelativeIndir = call->IsR2RRelativeIndir();
#endif // FEATURE_READYTORUN_COMPILER && TARGET_ARMARCH

            if (!isR2RRelativeIndir)
            {
                // Non-virtual direct calls to addresses accessed by
                // a single indirection.
                GenTree* cellAddr = AddrGen(addr);
#ifdef DEBUG
                cellAddr->AsIntCon()->gtTargetHandle = (size_t)call->gtCallMethHnd;
#endif
                GenTree* indir = Ind(cellAddr);
                result         = indir;
            }
            break;
        }

        case IAT_PPVALUE:
            // Non-virtual direct calls to addresses accessed by
            // a double indirection.
            //

            // Expanding an IAT_PPVALUE here, will lose the opportunity
            // to Hoist/CSE the first indirection as it is an invariant load
            //
            assert(!"IAT_PPVALUE case in LowerDirectCall");

            noway_assert(helperNum == CORINFO_HELP_UNDEF);
            result = AddrGen(addr);
            // Double-indirection. Load the address into a register
            // and call indirectly through the register
            //
            result = Ind(Ind(result));
            break;

        case IAT_RELPVALUE:
        {
            // Non-virtual direct calls to addresses accessed by
            // a single relative indirection.
            GenTree* cellAddr = AddrGen(addr);
            GenTree* indir    = Ind(cellAddr);
            result            = comp->gtNewOperNode(GT_ADD, TYP_I_IMPL, indir, AddrGen(addr));
            break;
        }

        default:
            noway_assert(!"Bad accessType");
            break;
    }

    return result;
}

GenTree* Lowering::LowerDelegateInvoke(GenTreeCall* call)
{
    noway_assert(call->gtCallType == CT_USER_FUNC);

    assert((comp->info.compCompHnd->getMethodAttribs(call->gtCallMethHnd) &
            (CORINFO_FLG_DELEGATE_INVOKE | CORINFO_FLG_FINAL)) == (CORINFO_FLG_DELEGATE_INVOKE | CORINFO_FLG_FINAL));

    GenTree* thisArgNode;
    if (call->IsTailCallViaJitHelper())
    {
        thisArgNode = call->GetArgNodeByArgNum(0);
    }
    else
    {
        thisArgNode = call->GetThisArg();
    }

    assert(thisArgNode != nullptr);
    assert(thisArgNode->gtOper == GT_PUTARG_REG);
    GenTree* originalThisExpr = thisArgNode->AsOp()->gtOp1;
    GenTree* thisExpr         = originalThisExpr;

    // We're going to use the 'this' expression multiple times, so make a local to copy it.

    unsigned lclNum;

    if (call->IsTailCallViaJitHelper() && originalThisExpr->IsLocal())
    {
        // For ordering purposes for the special tailcall arguments on x86, we forced the
        // 'this' pointer in this case to a local in Compiler::fgMorphTailCall().
        // We could possibly use this case to remove copies for all architectures and non-tailcall
        // calls by creating a new paramLcl var or paramLcl field reference, as is done in the
        // LowerVirtualVtableCall() code.
        assert(originalThisExpr->OperGet() == GT_LCL_VAR);
        lclNum = originalThisExpr->AsLclVarCommon()->GetLclNum();
    }
    else
    {
        unsigned delegateInvokeTmp = comp->lvaGrabTemp(true DEBUGARG("delegate invoke call"));

        LIR::Use thisExprUse(BlockRange(), &thisArgNode->AsOp()->gtOp1, thisArgNode);
        ReplaceWithLclVar(thisExprUse, delegateInvokeTmp);

        thisExpr = thisExprUse.Def(); // it's changed; reload it.
        lclNum   = delegateInvokeTmp;
    }

    // replace original expression feeding into thisPtr with
    // [originalThis + offsetOfDelegateInstance]

    GenTree* newThisAddr = new (comp, GT_LEA)
        GenTreeAddrMode(TYP_BYREF, thisExpr, nullptr, 0, comp->eeGetEEInfo()->offsetOfDelegateInstance);

    GenTree* newThis = comp->gtNewOperNode(GT_IND, TYP_REF, newThisAddr);

    BlockRange().InsertAfter(thisExpr, newThisAddr, newThis);

    thisArgNode->AsOp()->gtOp1 = newThis;
    ContainCheckIndir(newThis->AsIndir());

    // the control target is
    // [originalThis + firstTgtOffs]

    GenTree* base = new (comp, GT_LCL_VAR) GenTreeLclVar(GT_LCL_VAR, originalThisExpr->TypeGet(), lclNum);

    unsigned targetOffs = comp->eeGetEEInfo()->offsetOfDelegateFirstTarget;
    GenTree* result     = new (comp, GT_LEA) GenTreeAddrMode(TYP_BYREF, base, nullptr, 0, targetOffs);
    GenTree* callTarget = Ind(result);

    // don't need to sequence and insert this tree, caller will do it

    return callTarget;
}

GenTree* Lowering::LowerIndirectNonvirtCall(GenTreeCall* call)
{
#ifdef TARGET_X86
    if (call->gtCallCookie != nullptr)
    {
        NYI_X86("Morphing indirect non-virtual call with non-standard args");
    }
#endif

    // Indirect cookie calls gets transformed by fgMorphArgs as indirect call with non-standard args.
    // Hence we should never see this type of call in lower.

    noway_assert(call->gtCallCookie == nullptr);

    return nullptr;
}

//------------------------------------------------------------------------
// CreateReturnTrapSeq: Create a tree to perform a "return trap", used in PInvoke
// epilogs to invoke a GC under a condition. The return trap checks some global
// location (the runtime tells us where that is and how many indirections to make),
// then, based on the result, conditionally calls a GC helper. We use a special node
// for this because at this time (late in the compilation phases), introducing flow
// is tedious/difficult.
//
// This is used for PInvoke inlining.
//
// Return Value:
//    Code tree to perform the action.
//
GenTree* Lowering::CreateReturnTrapSeq()
{
    // The GT_RETURNTRAP node expands to this:
    //    if (g_TrapReturningThreads)
    //    {
    //       RareDisablePreemptiveGC();
    //    }

    // The only thing to do here is build up the expression that evaluates 'g_TrapReturningThreads'.

    void*    pAddrOfCaptureThreadGlobal = nullptr;
    int32_t* addrOfCaptureThreadGlobal =
        comp->info.compCompHnd->getAddrOfCaptureThreadGlobal(&pAddrOfCaptureThreadGlobal);

    GenTree* testTree;
    if (addrOfCaptureThreadGlobal != nullptr)
    {
        testTree = AddrGen(addrOfCaptureThreadGlobal);
    }
    else
    {
        testTree = Ind(AddrGen(pAddrOfCaptureThreadGlobal));
    }
    return comp->gtNewOperNode(GT_RETURNTRAP, TYP_INT, Ind(testTree, TYP_INT));
}

//------------------------------------------------------------------------
// SetGCState: Create a tree that stores the given constant (0 or 1) into the
// thread's GC state field.
//
// This is used for PInvoke inlining.
//
// Arguments:
//    state - constant (0 or 1) to store into the thread's GC state field.
//
// Return Value:
//    Code tree to perform the action.
//
GenTreeStoreInd* Lowering::SetGCState(int state)
{
    // Thread.offsetOfGcState = 0/1

    assert(state == 0 || state == 1);

    const CORINFO_EE_INFO* pInfo = comp->eeGetEEInfo();

    GenTree* base = new (comp, GT_LCL_VAR) GenTreeLclVar(GT_LCL_VAR, TYP_I_IMPL, comp->info.compLvFrameListRoot);

    GenTree* stateNode = new (comp, GT_CNS_INT) GenTreeIntCon(TYP_BYTE, state);
    GenTree* addr      = new (comp, GT_LEA) GenTreeAddrMode(TYP_I_IMPL, base, nullptr, 1, pInfo->offsetOfGCState);

    return new (comp, GT_STOREIND) GenTreeStoreInd(TYP_BYTE, addr, stateNode);
}

//------------------------------------------------------------------------
// CreateFrameLinkUpdate: Create a tree that either links or unlinks the
// locally-allocated InlinedCallFrame from the Frame list.
//
// This is used for PInvoke inlining.
//
// Arguments:
//    action - whether to link (push) or unlink (pop) the Frame
//
// Return Value:
//    Code tree to perform the action.
//
GenTreeStoreInd* Lowering::CreateFrameLinkUpdate(FrameLinkAction action)
{
    const CORINFO_EE_INFO*                       pInfo         = comp->eeGetEEInfo();
    const CORINFO_EE_INFO::InlinedCallFrameInfo& callFrameInfo = pInfo->inlinedCallFrameInfo;

    GenTree* TCB = new (comp, GT_LCL_VAR) GenTreeLclVar(GT_LCL_VAR, TYP_I_IMPL, comp->info.compLvFrameListRoot);

    // Thread->m_pFrame
    GenTree* addr = new (comp, GT_LEA) GenTreeAddrMode(TYP_I_IMPL, TCB, nullptr, 1, pInfo->offsetOfThreadFrame);

    GenTree* data = nullptr;

    if (action == PushFrame)
    {
        // Thread->m_pFrame = &inlinedCallFrame;
        data = comp->gtNewLclFldAddrNode(comp->lvaInlinedPInvokeFrameVar, callFrameInfo.offsetOfFrameVptr, nullptr);
    }
    else
    {
        assert(action == PopFrame);
        // Thread->m_pFrame = inlinedCallFrame.m_pNext;

        data = new (comp, GT_LCL_FLD) GenTreeLclFld(GT_LCL_FLD, TYP_BYREF, comp->lvaInlinedPInvokeFrameVar,
                                                    pInfo->inlinedCallFrameInfo.offsetOfFrameLink);
    }

    return new (comp, GT_STOREIND) GenTreeStoreInd(TYP_I_IMPL, addr, data);
}

//------------------------------------------------------------------------
// InsertPInvokeMethodProlog: Create the code that runs at the start of
// every method that has PInvoke calls.
//
// Initialize the TCB local and the InlinedCallFrame object. Then link ("push")
// the InlinedCallFrame object on the Frame chain. The layout of InlinedCallFrame
// is defined in vm/frames.h. See also vm/jitinterface.cpp for more information.
// The offsets of these fields is returned by the VM in a call to ICorStaticInfo::getEEInfo().
//
// The (current) layout is as follows:
//
//  64-bit  32-bit                                    CORINFO_EE_INFO
//  offset  offset  field name                        offset                  when set
//  -----------------------------------------------------------------------------------------
//  +00h    +00h    GS cookie                         offsetOfGSCookie
//  +08h    +04h    vptr for class InlinedCallFrame   offsetOfFrameVptr       method prolog
//  +10h    +08h    m_Next                            offsetOfFrameLink       method prolog
//  +18h    +0Ch    m_Datum                           offsetOfCallTarget      call site
//  +20h    n/a     m_StubSecretArg                                           not set by JIT
//  +28h    +10h    m_pCallSiteSP                     offsetOfCallSiteSP      x86: call site, and zeroed in method
//                                                                              prolog;
//                                                                            non-x86: method prolog (SP remains
//                                                                              constant in function, after prolog: no
//                                                                              localloc and PInvoke in same function)
//  +30h    +14h    m_pCallerReturnAddress            offsetOfReturnAddress   call site
//  +38h    +18h    m_pCalleeSavedFP                  offsetOfCalleeSavedFP   not set by JIT
//          +1Ch    m_pThread
//          +20h    m_pSPAfterProlog                  offsetOfSPAfterProlog   arm only
//          +20/24h JIT retval spill area (int)                               before call_gc    ???
//          +24/28h JIT retval spill area (long)                              before call_gc    ???
//          +28/2Ch Saved value of EBP                                        method prolog     ???
//
// Note that in the VM, InlinedCallFrame is a C++ class whose objects have a 'this' pointer that points
// to the InlinedCallFrame vptr (the 2nd field listed above), and the GS cookie is stored *before*
// the object. When we link the InlinedCallFrame onto the Frame chain, we must point at this location,
// and not at the beginning of the InlinedCallFrame local, which is actually the GS cookie.
//
// Return Value:
//    none
//
void Lowering::InsertPInvokeMethodProlog()
{
    noway_assert(comp->info.compUnmanagedCallCountWithGCTransition);
    noway_assert(comp->lvaInlinedPInvokeFrameVar != BAD_VAR_NUM);

    if (comp->opts.ShouldUsePInvokeHelpers())
    {
        return;
    }

    JITDUMP("======= Inserting PInvoke method prolog\n");

    // The first BB must be a scratch BB in order for us to be able to safely insert the P/Invoke prolog.
    assert(comp->fgFirstBBisScratch());

    LIR::Range& firstBlockRange = LIR::AsRange(comp->fgFirstBB);

    const CORINFO_EE_INFO*                       pInfo         = comp->eeGetEEInfo();
    const CORINFO_EE_INFO::InlinedCallFrameInfo& callFrameInfo = pInfo->inlinedCallFrameInfo;

    // First arg:  &compiler->lvaInlinedPInvokeFrameVar + callFrameInfo.offsetOfFrameVptr

    GenTree* frameAddr =
        comp->gtNewLclFldAddrNode(comp->lvaInlinedPInvokeFrameVar, callFrameInfo.offsetOfFrameVptr, nullptr);

    // Call runtime helper to fill in our InlinedCallFrame and push it on the Frame list:
    //     TCB = CORINFO_HELP_INIT_PINVOKE_FRAME(&symFrameStart, secretArg);
    // for x86, don't pass the secretArg.
    CLANG_FORMAT_COMMENT_ANCHOR;

#if defined(TARGET_X86) || defined(TARGET_ARM)
    GenTreeCall::Use* argList = comp->gtNewCallArgs(frameAddr);
#else
    GenTreeCall::Use*     argList = comp->gtNewCallArgs(frameAddr, PhysReg(REG_SECRET_STUB_PARAM));
#endif

    GenTree* call = comp->gtNewHelperCallNode(CORINFO_HELP_INIT_PINVOKE_FRAME, TYP_I_IMPL, argList);

    // some sanity checks on the frame list root vardsc
    LclVarDsc* varDsc = &comp->lvaTable[comp->info.compLvFrameListRoot];
    noway_assert(!varDsc->lvIsParam);
    noway_assert(varDsc->lvType == TYP_I_IMPL);

    GenTree* store = comp->gtNewStoreLclVar(comp->info.compLvFrameListRoot, TYP_I_IMPL, call);

    GenTree* const insertionPoint = firstBlockRange.FirstNonPhiOrCatchArgNode();

    comp->fgMorphTree(store);
    firstBlockRange.InsertBefore(insertionPoint, LIR::SeqTree(comp, store));
    DISPTREERANGE(firstBlockRange, store);

#if !defined(TARGET_X86) && !defined(TARGET_ARM)
    // For x86, this step is done at the call site (due to stack pointer not being static in the function).
    // For arm32, CallSiteSP is set up by the call to CORINFO_HELP_INIT_PINVOKE_FRAME.

    // --------------------------------------------------------
    // InlinedCallFrame.m_pCallSiteSP = @RSP;

    GenTreePhysReg* sp = comp->gtNewPhysRegNode(REG_SPBASE, TYP_I_IMPL);
    GenTreeLclFld*  storeSP =
        comp->gtNewStoreLclFld(TYP_I_IMPL, comp->lvaInlinedPInvokeFrameVar, callFrameInfo.offsetOfCallSiteSP, sp);
    firstBlockRange.InsertBefore(insertionPoint, sp, storeSP);
    DISPTREERANGE(firstBlockRange, storeSP);

#endif // !defined(TARGET_X86) && !defined(TARGET_ARM)

#if !defined(TARGET_ARM)
    // For arm32, CalleeSavedFP is set up by the call to CORINFO_HELP_INIT_PINVOKE_FRAME.

    // --------------------------------------------------------
    // InlinedCallFrame.m_pCalleeSavedEBP = @RBP;

    GenTreePhysReg* fp = comp->gtNewPhysRegNode(REG_FPBASE, TYP_I_IMPL);
    GenTreeLclFld*  storeFP =
        comp->gtNewStoreLclFld(TYP_I_IMPL, comp->lvaInlinedPInvokeFrameVar, callFrameInfo.offsetOfCalleeSavedFP, fp);
    firstBlockRange.InsertBefore(insertionPoint, fp, storeFP);
    DISPTREERANGE(firstBlockRange, storeFP);
#endif // !defined(TARGET_ARM)

    // --------------------------------------------------------
    // On 32-bit targets, CORINFO_HELP_INIT_PINVOKE_FRAME initializes the PInvoke frame and then pushes it onto
    // the current thread's Frame stack. On 64-bit targets, it only initializes the PInvoke frame.
    CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef TARGET_64BIT
    if (comp->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_IL_STUB))
    {
        // Push a frame - if we are NOT in an IL stub, this is done right before the call
        // The init routine sets InlinedCallFrame's m_pNext, so we just set the thead's top-of-stack
        GenTreeStoreInd* frameUpd = CreateFrameLinkUpdate(PushFrame);
        firstBlockRange.InsertBefore(insertionPoint, LIR::SeqTree(comp, frameUpd));
        ContainCheckStoreIndir(frameUpd);
        DISPTREERANGE(firstBlockRange, frameUpd);
    }
#endif // TARGET_64BIT
}

//------------------------------------------------------------------------
// InsertPInvokeMethodEpilog: Code that needs to be run when exiting any method
// that has PInvoke inlines. This needs to be inserted any place you can exit the
// function: returns, tailcalls and jmps.
//
// Arguments:
//    returnBB   -  basic block from which a method can return
//    lastExpr   -  GenTree of the last top level stmnt of returnBB (debug only arg)
//
// Return Value:
//    Code tree to perform the action.
//
void Lowering::InsertPInvokeMethodEpilog(BasicBlock* returnBB DEBUGARG(GenTree* lastExpr))
{
    assert(returnBB != nullptr);
    assert(comp->info.compUnmanagedCallCountWithGCTransition);

    if (comp->opts.ShouldUsePInvokeHelpers())
    {
        return;
    }

    JITDUMP("======= Inserting PInvoke method epilog\n");

    // Method doing PInvoke calls has exactly one return block unless it has "jmp" or tail calls.
    assert(((returnBB == comp->genReturnBB) && (returnBB->bbJumpKind == BBJ_RETURN)) ||
           returnBB->endsWithTailCallOrJmp(comp));

    LIR::Range& returnBlockRange = LIR::AsRange(returnBB);

    GenTree* insertionPoint = returnBlockRange.LastNode();
    assert(insertionPoint == lastExpr);

    // Note: PInvoke Method Epilog (PME) needs to be inserted just before GT_RETURN, GT_JMP or GT_CALL node in execution
    // order so that it is guaranteed that there will be no further PInvokes after that point in the method.
    //
    // Example1: GT_RETURN(op1) - say execution order is: Op1, GT_RETURN.  After inserting PME, execution order would be
    //           Op1, PME, GT_RETURN
    //
    // Example2: GT_CALL(arg side effect computing nodes, Stk Args Setup, Reg Args setup). The execution order would be
    //           arg side effect computing nodes, Stk Args setup, Reg Args setup, GT_CALL
    //           After inserting PME execution order would be:
    //           arg side effect computing nodes, Stk Args setup, Reg Args setup, PME, GT_CALL
    //
    // Example3: GT_JMP.  After inserting PME execution order would be: PME, GT_JMP
    //           That is after PME, args for GT_JMP call will be setup.

    // Pop the frame if necessary. This always happens in the epilog on 32-bit targets. For 64-bit targets, we only do
    // this in the epilog for IL stubs; for non-IL stubs the frame is popped after every PInvoke call.
    CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef TARGET_64BIT
    if (comp->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_IL_STUB))
#endif // TARGET_64BIT
    {
        GenTreeStoreInd* frameUpd = CreateFrameLinkUpdate(PopFrame);
        returnBlockRange.InsertBefore(insertionPoint, LIR::SeqTree(comp, frameUpd));
        ContainCheckStoreIndir(frameUpd);
    }
}

//------------------------------------------------------------------------
// InsertPInvokeCallProlog: Emit the call-site prolog for direct calls to unmanaged code.
// It does all the necessary call-site setup of the InlinedCallFrame.
//
// Arguments:
//    call - the call for which we are inserting the PInvoke prolog.
//
// Return Value:
//    None.
//
void Lowering::InsertPInvokeCallProlog(GenTreeCall* call)
{
    JITDUMP("======= Inserting PInvoke call prolog\n");

    GenTree* insertBefore = call;
    if (call->gtCallType == CT_INDIRECT)
    {
        bool isClosed;
        insertBefore = BlockRange().GetTreeRange(call->gtCallAddr, &isClosed).FirstNode();
        assert(isClosed);
    }

    const CORINFO_EE_INFO::InlinedCallFrameInfo& callFrameInfo = comp->eeGetEEInfo()->inlinedCallFrameInfo;

    gtCallTypes callType = (gtCallTypes)call->gtCallType;

    noway_assert(comp->lvaInlinedPInvokeFrameVar != BAD_VAR_NUM);

    if (comp->opts.ShouldUsePInvokeHelpers())
    {
        // First argument is the address of the frame variable.
        GenTree* frameAddr = comp->gtNewLclVarAddrNode(comp->lvaInlinedPInvokeFrameVar);

#if defined(TARGET_X86) && !defined(UNIX_X86_ABI)
        // On x86 targets, PInvoke calls need the size of the stack args in InlinedCallFrame.m_Datum.
        // This is because the callee pops stack arguments, and we need to keep track of this during stack
        // walking
        const unsigned    numStkArgBytes = call->fgArgInfo->GetNextSlotNum() * REGSIZE_BYTES;
        GenTree*          stackBytes     = comp->gtNewIconNode(numStkArgBytes, TYP_INT);
        GenTreeCall::Use* args           = comp->gtNewCallArgs(frameAddr, stackBytes);
#else
        GenTreeCall::Use* args    = comp->gtNewCallArgs(frameAddr);
#endif
        // Insert call to CORINFO_HELP_JIT_PINVOKE_BEGIN
        GenTree* helperCall = comp->gtNewHelperCallNode(CORINFO_HELP_JIT_PINVOKE_BEGIN, TYP_VOID, args);

        comp->fgMorphTree(helperCall);
        BlockRange().InsertBefore(insertBefore, LIR::SeqTree(comp, helperCall));
        LowerNode(helperCall); // helper call is inserted before current node and should be lowered here.
        return;
    }

    // Emit the following sequence:
    //
    // InlinedCallFrame.callTarget = methodHandle   // stored in m_Datum
    // InlinedCallFrame.m_pCallSiteSP = SP          // x86 only
    // InlinedCallFrame.m_pCallerReturnAddress = return address
    // GT_START_PREEEMPTC
    // Thread.gcState = 0
    // (non-stub) - update top Frame on TCB         // 64-bit targets only

    // ----------------------------------------------------------------------------------
    // Setup InlinedCallFrame.callSiteTarget (which is how the JIT refers to it).
    // The actual field is InlinedCallFrame.m_Datum which has many different uses and meanings.

    GenTree* src = nullptr;

    if (callType == CT_INDIRECT)
    {
#if !defined(TARGET_64BIT)
        // On 32-bit targets, indirect calls need the size of the stack args in InlinedCallFrame.m_Datum.
        const unsigned numStkArgBytes = call->fgArgInfo->GetNextSlotNum() * REGSIZE_BYTES;

        src = comp->gtNewIconNode(numStkArgBytes, TYP_INT);
#else
        // On 64-bit targets, indirect calls may need the stub parameter value in InlinedCallFrame.m_Datum.
        // If the stub parameter value is not needed, m_Datum will be initialized by the VM.
        if (comp->info.compPublishStubParam)
        {
            src = comp->gtNewLclvNode(comp->lvaStubArgumentVar, TYP_I_IMPL);
        }
#endif // !defined(TARGET_64BIT)
    }
    else
    {
        assert(callType == CT_USER_FUNC);

        void*                 pEmbedMethodHandle = nullptr;
        CORINFO_METHOD_HANDLE embedMethodHandle =
            comp->info.compCompHnd->embedMethodHandle(call->gtCallMethHnd, &pEmbedMethodHandle);

        noway_assert((!embedMethodHandle) != (!pEmbedMethodHandle));

        if (embedMethodHandle != nullptr)
        {
            // InlinedCallFrame.callSiteTarget = methodHandle
            src = AddrGen(embedMethodHandle);
        }
        else
        {
            // InlinedCallFrame.callSiteTarget = *pEmbedMethodHandle
            src = Ind(AddrGen(pEmbedMethodHandle));
        }
    }

    if (src != nullptr)
    {
        // Store into InlinedCallFrame.m_Datum, the offset of which is given by offsetOfCallTarget.
        GenTreeLclFld* store =
            comp->gtNewStoreLclFld(TYP_I_IMPL, comp->lvaInlinedPInvokeFrameVar, callFrameInfo.offsetOfCallTarget, src);
        InsertTreeBeforeAndContainCheck(insertBefore, store);
    }

#ifdef TARGET_X86
    // ----------------------------------------------------------------------------------
    // InlinedCallFrame.m_pCallSiteSP = SP

    GenTreePhysReg* callSiteSP      = comp->gtNewPhysRegNode(REG_SPBASE, TYP_I_IMPL);
    GenTreeLclFld*  storeCallSiteSP = comp->gtNewStoreLclFld(TYP_I_IMPL, comp->lvaInlinedPInvokeFrameVar,
                                                            callFrameInfo.offsetOfCallSiteSP, callSiteSP);
    BlockRange().InsertBefore(insertBefore, callSiteSP, storeCallSiteSP);
#endif

    // ----------------------------------------------------------------------------------
    // InlinedCallFrame.m_pCallerReturnAddress = &label (the address of the instruction immediately following the call)

    GenTree*       returnLabel      = new (comp, GT_LABEL) GenTree(GT_LABEL, TYP_I_IMPL);
    GenTreeLclFld* storeReturnLabel = comp->gtNewStoreLclFld(TYP_I_IMPL, comp->lvaInlinedPInvokeFrameVar,
                                                             callFrameInfo.offsetOfReturnAddress, returnLabel);
    BlockRange().InsertBefore(insertBefore, returnLabel, storeReturnLabel);

    // Push the PInvoke frame if necessary. On 32-bit targets this only happens in the method prolog if a method
    // contains PInvokes; on 64-bit targets this is necessary in non-stubs.
    CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef TARGET_64BIT
    if (!comp->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_IL_STUB))
    {
        // Set the TCB's frame to be the one we just created.
        // Note the init routine for the InlinedCallFrame (CORINFO_HELP_INIT_PINVOKE_FRAME)
        // has prepended it to the linked list to maintain the stack of Frames.
        //
        // Stubs do this once per stub, not once per call.
        GenTreeStoreInd* frameUpd = CreateFrameLinkUpdate(PushFrame);
        BlockRange().InsertBefore(insertBefore, LIR::SeqTree(comp, frameUpd));
        ContainCheckStoreIndir(frameUpd);
    }
#endif // TARGET_64BIT

    // IMPORTANT **** This instruction must be the last real instruction ****
    // It changes the thread's state to Preemptive mode
    // ----------------------------------------------------------------------------------
    //  [tcb + offsetOfGcState] = 0
    GenTreeStoreInd* storeGCState = SetGCState(0);
    BlockRange().InsertBefore(insertBefore, LIR::SeqTree(comp, storeGCState));
    ContainCheckStoreIndir(storeGCState);

    // Indicate that codegen has switched this thread to preemptive GC.
    // This tree node doesn't generate any code, but impacts LSRA and gc reporting.
    // This tree node is simple so doesn't require sequencing.
    GenTree* preemptiveGCNode = new (comp, GT_START_PREEMPTGC) GenTree(GT_START_PREEMPTGC, TYP_VOID);
    BlockRange().InsertBefore(insertBefore, preemptiveGCNode);
}

//------------------------------------------------------------------------
// InsertPInvokeCallEpilog: Insert the code that goes after every inlined pinvoke call.
//
// Arguments:
//    call - the call for which we are inserting the PInvoke epilog.
//
// Return Value:
//    None.
//
void Lowering::InsertPInvokeCallEpilog(GenTreeCall* call)
{
    JITDUMP("======= Inserting PInvoke call epilog\n");

    if (comp->opts.ShouldUsePInvokeHelpers())
    {
        noway_assert(comp->lvaInlinedPInvokeFrameVar != BAD_VAR_NUM);

        // First argument is the address of the frame variable.
        GenTree* frameAddr = comp->gtNewLclVarAddrNode(comp->lvaInlinedPInvokeFrameVar);

        // Insert call to CORINFO_HELP_JIT_PINVOKE_END
        GenTreeCall* helperCall =
            comp->gtNewHelperCallNode(CORINFO_HELP_JIT_PINVOKE_END, TYP_VOID, comp->gtNewCallArgs(frameAddr));

        comp->fgMorphTree(helperCall);
        BlockRange().InsertAfter(call, LIR::SeqTree(comp, helperCall));
        ContainCheckCallOperands(helperCall);
        return;
    }

    // gcstate = 1
    GenTree* insertionPoint = call->gtNext;

    {
        GenTreeStoreInd* tree = SetGCState(1);
        BlockRange().InsertBefore(insertionPoint, LIR::SeqTree(comp, tree));
        ContainCheckStoreIndir(tree);
    }

    {
        GenTree* tree = CreateReturnTrapSeq();
        BlockRange().InsertBefore(insertionPoint, LIR::SeqTree(comp, tree));
        ContainCheckReturnTrap(tree->AsOp());
    }

    // Pop the frame if necessary. On 32-bit targets this only happens in the method epilog; on 64-bit targets thi
    // happens after every PInvoke call in non-stubs. 32-bit targets instead mark the frame as inactive.
    CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef TARGET_64BIT
    if (!comp->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_IL_STUB))
    {
        GenTreeStoreInd* tree = CreateFrameLinkUpdate(PopFrame);
        BlockRange().InsertBefore(insertionPoint, LIR::SeqTree(comp, tree));
        ContainCheckStoreIndir(tree);
    }
#else
    const CORINFO_EE_INFO::InlinedCallFrameInfo& callFrameInfo = comp->eeGetEEInfo()->inlinedCallFrameInfo;

    // ----------------------------------------------------------------------------------
    // InlinedCallFrame.m_pCallerReturnAddress = nullptr

    GenTreeIntCon* zero = comp->gtNewIconNode(0, TYP_I_IMPL);
    GenTreeLclFld* storeCallSiteTracker =
        comp->gtNewStoreLclFld(TYP_I_IMPL, comp->lvaInlinedPInvokeFrameVar, callFrameInfo.offsetOfReturnAddress, zero);
    BlockRange().InsertBefore(insertionPoint, zero, storeCallSiteTracker);
    ContainCheckStoreLcl(storeCallSiteTracker);
#endif // TARGET_64BIT
}

//------------------------------------------------------------------------
// LowerNonvirtPinvokeCall: Lower a non-virtual / indirect PInvoke call
//
// Arguments:
//    call - The call to lower.
//
// Return Value:
//    The lowered call tree.
//
GenTree* Lowering::LowerNonvirtPinvokeCall(GenTreeCall* call)
{
    // PInvoke lowering varies depending on the flags passed in by the EE. By default,
    // GC transitions are generated inline; if CORJIT_FLAG_USE_PINVOKE_HELPERS is specified,
    // GC transitions are instead performed using helper calls. Examples of each case are given
    // below. Note that the data structure that is used to store information about a call frame
    // containing any P/Invoke calls is initialized in the method prolog (see
    // InsertPInvokeMethod{Prolog,Epilog} for details).
    //
    // Inline transitions:
    //     InlinedCallFrame inlinedCallFrame;
    //
    //     ...
    //
    //     // Set up frame information
    //     inlinedCallFrame.callTarget = methodHandle;      // stored in m_Datum
    //     inlinedCallFrame.m_pCallSiteSP = SP;             // x86 only
    //     inlinedCallFrame.m_pCallerReturnAddress = &label; (the address of the instruction immediately following the
    //     call)
    //     Thread.m_pFrame = &inlinedCallFrame; (non-IL-stub only)
    //
    //     // Switch the thread's GC mode to preemptive mode
    //     thread->m_fPreemptiveGCDisabled = 0;
    //
    //     // Call the unmanaged method
    //     target();
    //
    //     // Switch the thread's GC mode back to cooperative mode
    //     thread->m_fPreemptiveGCDisabled = 1;
    //
    //     // Rendezvous with a running collection if necessary
    //     if (g_TrapReturningThreads)
    //         RareDisablePreemptiveGC();
    //
    // Transistions using helpers:
    //
    //     OpaqueFrame opaqueFrame;
    //
    //     ...
    //
    //     // Call the JIT_PINVOKE_BEGIN helper
    //     JIT_PINVOKE_BEGIN(&opaqueFrame);
    //
    //     // Call the unmanaged method
    //     target();
    //
    //     // Call the JIT_PINVOKE_END helper
    //     JIT_PINVOKE_END(&opaqueFrame);
    //
    // Note that the JIT_PINVOKE_{BEGIN.END} helpers currently use the default calling convention for the target
    // platform. They may be changed in the future such that they preserve all register values.

    GenTree* result = nullptr;

    // All code generated by this function must not contain the randomly-inserted NOPs
    // that we insert to inhibit JIT spraying in partial trust scenarios.
    // The PINVOKE_PROLOG op signals this to the code generator/emitter.

    GenTree* prolog = new (comp, GT_NOP) GenTree(GT_PINVOKE_PROLOG, TYP_VOID);
    BlockRange().InsertBefore(call, prolog);

    bool addPInvokePrologEpilog = !call->IsSuppressGCTransition();
    if (addPInvokePrologEpilog)
    {
        InsertPInvokeCallProlog(call);
    }

    if (call->gtCallType != CT_INDIRECT)
    {
        noway_assert(call->gtCallType == CT_USER_FUNC);
        CORINFO_METHOD_HANDLE methHnd = call->gtCallMethHnd;

        CORINFO_CONST_LOOKUP lookup;
        comp->info.compCompHnd->getAddressOfPInvokeTarget(methHnd, &lookup);

        void*    addr = lookup.addr;
        GenTree* addrTree;
        switch (lookup.accessType)
        {
            case IAT_VALUE:
                // IsCallTargetInRange always return true on x64. It wants to use rip-based addressing
                // for this call. Unfortunately, in case of pinvokes (+suppressgctransition) to external libs
                // (e.g. kernel32.dll) the relative offset is unlikely to fit into int32 and we will have to
                // turn fAllowRel32 off globally.
                if ((call->IsSuppressGCTransition() && !comp->opts.jitFlags->IsSet(JitFlags::JIT_FLAG_PREJIT)) ||
                    !IsCallTargetInRange(addr))
                {
                    result = AddrGen(addr);
                }
                else
                {
                    // a direct call within range of hardware relative call instruction
                    // stash the address for codegen
                    call->gtDirectCallAddress = addr;
#ifdef FEATURE_READYTORUN_COMPILER
                    call->gtEntryPoint.addr       = nullptr;
                    call->gtEntryPoint.accessType = IAT_VALUE;
#endif
                }
                break;

            case IAT_PVALUE:
                addrTree = AddrGen(addr);
#ifdef DEBUG
                addrTree->AsIntCon()->gtTargetHandle = (size_t)methHnd;
#endif
                result = Ind(addrTree);
                break;

            case IAT_PPVALUE:
                // ToDo:  Expanding an IAT_PPVALUE here, loses the opportunity
                // to Hoist/CSE the first indirection as it is an invariant load
                //
                // This case currently occurs today when we make PInvoke calls in crossgen
                //
                // assert(!"IAT_PPVALUE in Lowering::LowerNonvirtPinvokeCall");

                addrTree = AddrGen(addr);
#ifdef DEBUG
                addrTree->AsIntCon()->gtTargetHandle = (size_t)methHnd;
#endif
                // Double-indirection. Load the address into a register
                // and call indirectly through the register
                //
                result = Ind(Ind(addrTree));
                break;

            case IAT_RELPVALUE:
                unreached();
        }
    }

    if (addPInvokePrologEpilog)
    {
        InsertPInvokeCallEpilog(call);
    }

    return result;
}

// Expand the code necessary to calculate the control target.
// Returns: the expression needed to calculate the control target
// May insert embedded statements
GenTree* Lowering::LowerVirtualVtableCall(GenTreeCall* call)
{
    noway_assert(call->gtCallType == CT_USER_FUNC);

    regNumber thisPtrArgReg = comp->codeGen->genGetThisArgReg(call);

    // get a reference to the thisPtr being passed
    fgArgTabEntry* argEntry = call->GetArgInfoByArgNum(0);
    assert(argEntry->GetRegNum() == thisPtrArgReg);
    assert(argEntry->GetNode()->OperIs(GT_PUTARG_REG));
    GenTree* thisPtr = argEntry->GetNode()->AsUnOp()->gtGetOp1();

    // If what we are passing as the thisptr is not already a local, make a new local to place it in
    // because we will be creating expressions based on it.
    unsigned lclNum;
    if (thisPtr->IsLocal())
    {
        lclNum = thisPtr->AsLclVarCommon()->GetLclNum();
    }
    else
    {
        // Split off the thisPtr and store to a temporary variable.
        if (vtableCallTemp == BAD_VAR_NUM)
        {
            vtableCallTemp = comp->lvaGrabTemp(true DEBUGARG("virtual vtable call"));
        }

        LIR::Use thisPtrUse(BlockRange(), &(argEntry->GetNode()->AsUnOp()->gtOp1), argEntry->GetNode());
        ReplaceWithLclVar(thisPtrUse, vtableCallTemp);

        lclNum = vtableCallTemp;
    }

    // Get hold of the vtable offset (note: this might be expensive)
    unsigned vtabOffsOfIndirection;
    unsigned vtabOffsAfterIndirection;
    bool     isRelative;
    comp->info.compCompHnd->getMethodVTableOffset(call->gtCallMethHnd, &vtabOffsOfIndirection,
                                                  &vtabOffsAfterIndirection, &isRelative);

    // If the thisPtr is a local field, then construct a local field type node
    GenTree* local;
    if (thisPtr->isLclField())
    {
        local = new (comp, GT_LCL_FLD)
            GenTreeLclFld(GT_LCL_FLD, thisPtr->TypeGet(), lclNum, thisPtr->AsLclFld()->GetLclOffs());
    }
    else
    {
        local = new (comp, GT_LCL_VAR) GenTreeLclVar(GT_LCL_VAR, thisPtr->TypeGet(), lclNum);
    }

    // pointer to virtual table = [REG_CALL_THIS + offs]
    GenTree* result = Ind(Offset(local, VPTR_OFFS));

    // Get the appropriate vtable chunk
    if (vtabOffsOfIndirection != CORINFO_VIRTUALCALL_NO_CHUNK)
    {
        if (isRelative)
        {
            // MethodTable offset is a relative pointer.
            //
            // Additional temporary variable is used to store virtual table pointer.
            // Address of method is obtained by the next computations:
            //
            // Save relative offset to tmp (vtab is virtual table pointer, vtabOffsOfIndirection is offset of
            // vtable-1st-level-indirection):
            // tmp = vtab
            //
            // Save address of method to result (vtabOffsAfterIndirection is offset of vtable-2nd-level-indirection):
            // result = [tmp + vtabOffsOfIndirection + vtabOffsAfterIndirection + [tmp + vtabOffsOfIndirection]]
            //
            //
            // If relative pointers are also in second level indirection, additional temporary is used:
            // tmp1 = vtab
            // tmp2 = tmp1 + vtabOffsOfIndirection + vtabOffsAfterIndirection + [tmp1 + vtabOffsOfIndirection]
            // result = tmp2 + [tmp2]
            //
            unsigned lclNumTmp  = comp->lvaNewTemp(TYP_I_IMPL, true DEBUGARG("lclNumTmp"));
            unsigned lclNumTmp2 = comp->lvaNewTemp(TYP_I_IMPL, true DEBUGARG("lclNumTmp2"));

            GenTree* lclvNodeStore = comp->gtNewStoreLclVar(lclNumTmp, TYP_I_IMPL, result);

            GenTree* tmpTree = comp->gtNewLclvNode(lclNumTmp, TYP_I_IMPL);
            tmpTree          = Offset(tmpTree, vtabOffsOfIndirection);

            tmpTree       = comp->gtNewOperNode(GT_IND, TYP_I_IMPL, tmpTree, false);
            GenTree* offs = comp->gtNewIconNode(vtabOffsOfIndirection + vtabOffsAfterIndirection, TYP_I_IMPL);
            result        = comp->gtNewOperNode(GT_ADD, TYP_I_IMPL, comp->gtNewLclvNode(lclNumTmp, TYP_I_IMPL), offs);

            GenTree* base           = OffsetByIndexWithScale(result, tmpTree, 1);
            GenTree* lclvNodeStore2 = comp->gtNewStoreLclVar(lclNumTmp2, TYP_I_IMPL, base);

            LIR::Range range = LIR::SeqTree(comp, lclvNodeStore);
            JITDUMP("result of obtaining pointer to virtual table:\n");
            DISPRANGE(range);
            BlockRange().InsertBefore(call, std::move(range));

            LIR::Range range2 = LIR::SeqTree(comp, lclvNodeStore2);
            ContainCheckIndir(tmpTree->AsIndir());
            JITDUMP("result of obtaining pointer to virtual table 2nd level indirection:\n");
            DISPRANGE(range2);
            BlockRange().InsertAfter(lclvNodeStore, std::move(range2));

            result = Ind(comp->gtNewLclvNode(lclNumTmp2, TYP_I_IMPL));
            result = comp->gtNewOperNode(GT_ADD, TYP_I_IMPL, result, comp->gtNewLclvNode(lclNumTmp2, TYP_I_IMPL));
        }
        else
        {
            // result = [REG_CALL_IND_SCRATCH + vtabOffsOfIndirection]
            result = Ind(Offset(result, vtabOffsOfIndirection));
        }
    }
    else
    {
        assert(!isRelative);
    }

    // Load the function address
    // result = [reg+vtabOffs]
    if (!isRelative)
    {
        result = Ind(Offset(result, vtabOffsAfterIndirection));
    }

    return result;
}

// Lower stub dispatched virtual calls.
GenTree* Lowering::LowerVirtualStubCall(GenTreeCall* call)
{
    assert(call->IsVirtualStub());

    // An x86 JIT which uses full stub dispatch must generate only
    // the following stub dispatch calls:
    //
    // (1) isCallRelativeIndirect:
    //        call dword ptr [rel32]  ;  FF 15 ---rel32----
    // (2) isCallRelative:
    //        call abc                ;     E8 ---rel32----
    // (3) isCallRegisterIndirect:
    //     3-byte nop                 ;
    //     call dword ptr [eax]       ;     FF 10
    //
    // THIS IS VERY TIGHTLY TIED TO THE PREDICATES IN
    // vm\i386\cGenCpu.h, esp. isCallRegisterIndirect.

    GenTree* result = nullptr;

    // This is code to set up an indirect call to a stub address computed
    // via dictionary lookup.
    if (call->gtCallType == CT_INDIRECT)
    {
        // The importer decided we needed a stub call via a computed
        // stub dispatch address, i.e. an address which came from a dictionary lookup.
        //   - The dictionary lookup produces an indirected address, suitable for call
        //     via "call [VirtualStubParam.reg]"
        //
        // This combination will only be generated for shared generic code and when
        // stub dispatch is active.

        // fgMorphArgs will have created trees to pass the address in VirtualStubParam.reg.
        // All we have to do here is add an indirection to generate the actual call target.

        GenTree* ind = Ind(call->gtCallAddr);
        BlockRange().InsertAfter(call->gtCallAddr, ind);
        call->gtCallAddr = ind;

        ind->gtFlags |= GTF_IND_REQ_ADDR_IN_REG;

        ContainCheckIndir(ind->AsIndir());
    }
    else
    {
        // Direct stub call.
        // Get stub addr. This will return NULL if virtual call stubs are not active
        void* stubAddr = call->gtStubCallStubAddr;
        noway_assert(stubAddr != nullptr);

        // If not CT_INDIRECT,  then it should always be relative indir call.
        // This is ensured by VM.
        noway_assert(call->IsVirtualStubRelativeIndir());

        // Direct stub calls, though the stubAddr itself may still need to be
        // accessed via an indirection.
        GenTree* addr = AddrGen(stubAddr);

        // On x86, for tailcall via helper, the JIT_TailCall helper takes the stubAddr as
        // the target address, and we set a flag that it's a VSD call. The helper then
        // handles any necessary indirection.
        if (call->IsTailCallViaJitHelper())
        {
            result = addr;
        }
        else
        {

            bool shouldOptimizeVirtualStubCall = false;
#if defined(FEATURE_READYTORUN_COMPILER) && defined(TARGET_ARMARCH)
            // Skip inserting the indirection node to load the address that is already
            // computed in REG_R2R_INDIRECT_PARAM as a hidden parameter. Instead during the
            // codegen, just load the call target from REG_R2R_INDIRECT_PARAM.
            // However, for tail calls, the call target is always computed in RBM_FASTTAILCALL_TARGET
            // and so do not optimize virtual stub calls for such cases.
            shouldOptimizeVirtualStubCall = !call->IsTailCall();
#endif // FEATURE_READYTORUN_COMPILER && TARGET_ARMARCH

            if (!shouldOptimizeVirtualStubCall)
            {
                result = Ind(addr);
            }
        }
    }

    // TODO-Cleanup: start emitting random NOPS
    return result;
}

//------------------------------------------------------------------------
// Lowering::AreSourcesPossibleModifiedLocals:
//    Given two nodes which will be used in an addressing mode (base,
//    index), check to see if they are lclVar reads, and if so, walk
//    backwards from the use until both reads have been visited to
//    determine if they are potentially modified in that range.
//
// Arguments:
//    addr - the node that uses the base and index nodes
//    base - the base node
//    index - the index node
//
// Returns: true if either the base or index may be modified between the
//          node and addr.
//
bool Lowering::AreSourcesPossiblyModifiedLocals(GenTree* addr, GenTree* base, GenTree* index)
{
    assert(addr != nullptr);

    SideEffectSet baseSideEffects;
    if (base != nullptr)
    {
        if (base->OperIsLocalRead())
        {
            baseSideEffects.AddNode(comp, base);
        }
        else
        {
            base = nullptr;
        }
    }

    SideEffectSet indexSideEffects;
    if (index != nullptr)
    {
        if (index->OperIsLocalRead())
        {
            indexSideEffects.AddNode(comp, index);
        }
        else
        {
            index = nullptr;
        }
    }

    for (GenTree* cursor = addr;; cursor = cursor->gtPrev)
    {
        assert(cursor != nullptr);

        if (cursor == base)
        {
            base = nullptr;
        }

        if (cursor == index)
        {
            index = nullptr;
        }

        if ((base == nullptr) && (index == nullptr))
        {
            return false;
        }

        m_scratchSideEffects.Clear();
        m_scratchSideEffects.AddNode(comp, cursor);
        if ((base != nullptr) && m_scratchSideEffects.InterferesWith(baseSideEffects, false))
        {
            return true;
        }

        if ((index != nullptr) && m_scratchSideEffects.InterferesWith(indexSideEffects, false))
        {
            return true;
        }
    }
}

//------------------------------------------------------------------------
// TryCreateAddrMode: recognize trees which can be implemented using an
//    addressing mode and transform them to a GT_LEA
//
// Arguments:
//    use - the use of the address we want to transform
//    isContainable - true if this addressing mode can be contained
//
// Returns:
//    true if the address node was changed to a LEA, false otherwise.
//
bool Lowering::TryCreateAddrMode(GenTree* addr, bool isContainable)
{
    if (!addr->OperIs(GT_ADD) || addr->gtOverflow())
    {
        return false;
    }

    GenTree* base   = nullptr;
    GenTree* index  = nullptr;
    unsigned scale  = 0;
    ssize_t  offset = 0;

    // Find out if an addressing mode can be constructed
    bool doAddrMode = CreateAddrMode(comp, addr, // address
                                     &base,      // base addr
                                     &index,     // index val
#if SCALED_ADDR_MODES
                                     &scale,   // scaling
#endif                                         // SCALED_ADDR_MODES
                                     &offset); // displacement

    if (scale == 0)
    {
        scale = 1;
    }

    if (!isContainable)
    {
        // this is just a reg-const add
        if (index == nullptr)
        {
            return false;
        }

        // this is just a reg-reg add
        if ((scale == 1) && (offset == 0))
        {
            return false;
        }
    }

    // make sure there are not any side effects between def of leaves and use
    if (!doAddrMode || AreSourcesPossiblyModifiedLocals(addr, base, index))
    {
        JITDUMP("No addressing mode:\n  ");
        DISPNODE(addr);
        return false;
    }

    JITDUMP("Addressing mode:\n");
    JITDUMP("  Base\n    ");
    DISPNODE(base);
    if (index != nullptr)
    {
        JITDUMP("  + Index * %u + %d\n    ", scale, offset);
        DISPNODE(index);
    }
    else
    {
        JITDUMP("  + %d\n", offset);
    }

    // Save the (potentially) unused operands before changing the address to LEA.
    ArrayStack<GenTree*> unusedStack(comp->getAllocator(CMK_ArrayStack));
    unusedStack.Push(addr->AsOp()->gtGetOp1());
    unusedStack.Push(addr->AsOp()->gtGetOp2());

    addr->ChangeOper(GT_LEA);
    // Make sure there are no leftover side effects (though the existing ADD we're
    // changing shouldn't have any at this point, but sometimes it does).
    addr->gtFlags &= ~GTF_ALL_EFFECT;

    GenTreeAddrMode* addrMode = addr->AsAddrMode();
    addrMode->SetBase(base);
    addrMode->SetIndex(index);
    addrMode->SetScale(scale);
    addrMode->SetOffset(static_cast<int>(offset));

    // Neither the base nor the index should now be contained.
    if (base != nullptr)
    {
        base->ClearContained();
    }
    if (index != nullptr)
    {
        index->ClearContained();
    }

    // Remove all the nodes that are no longer used.
    while (!unusedStack.Empty())
    {
        GenTree* unused = unusedStack.Pop();

        // Use a loop to process some of the nodes iteratively
        // instead of pushing them on the stack.
        while ((unused != base) && (unused != index))
        {
            JITDUMP("Removing unused node:\n  ");
            DISPNODE(unused);

            BlockRange().Remove(unused);

            if (unused->OperIs(GT_ADD, GT_MUL, GT_LSH))
            {
                // Push the first operand and loop back to process the second one.
                // This minimizes the stack depth because the second one tends to be
                // a constant so it gets processed and then the first one gets popped.
                unusedStack.Push(unused->AsOp()->gtGetOp1());
                unused = unused->AsOp()->gtGetOp2();
            }
            else
            {
                assert(unused->OperIs(GT_CNS_INT));
                break;
            }
        }
    }

    JITDUMP("New addressing mode node:\n  ");
    DISPNODE(addrMode);
    JITDUMP("\n");

    return true;
}

//------------------------------------------------------------------------
// LowerAdd: turn this add into a GT_LEA if that would be profitable
//
// Arguments:
//    node - the node we care about
//
// Returns:
//    nullptr if no transformation was done, or the next node in the transformed node sequence that
//    needs to be lowered.
//
GenTree* Lowering::LowerAdd(GenTreeOp* node)
{
    if (varTypeIsIntegralOrI(node->TypeGet()))
    {
        GenTree* op1 = node->gtGetOp1();
        GenTree* op2 = node->gtGetOp2();
        LIR::Use use;

        // It is not the best place to do such simple arithmetic optimizations,
        // but it allows us to avoid `LEA(addr, 0)` nodes and doing that in morph
        // requires more changes. Delete that part if we get an expression optimizer.
        if (op2->IsIntegralConst(0))
        {
            JITDUMP("Lower: optimize val + 0: ");
            DISPNODE(node);
            JITDUMP("Replaced with: ");
            DISPNODE(op1);
            if (BlockRange().TryGetUse(node, &use))
            {
                use.ReplaceWith(comp, op1);
            }
            else
            {
                op1->SetUnusedValue();
            }
            GenTree* next = node->gtNext;
            BlockRange().Remove(op2);
            BlockRange().Remove(node);
            JITDUMP("Remove [%06u], [%06u]\n", op2->gtTreeID, node->gtTreeID);
            return next;
        }

#ifndef TARGET_ARMARCH
        if (BlockRange().TryGetUse(node, &use))
        {
            // If this is a child of an indir, let the parent handle it.
            // If there is a chain of adds, only look at the topmost one.
            GenTree* parent = use.User();
            if (!parent->OperIsIndir() && !parent->OperIs(GT_ADD))
            {
                TryCreateAddrMode(node, false);
            }
        }
#endif // !TARGET_ARMARCH
    }

    if (node->OperIs(GT_ADD))
    {
        ContainCheckBinary(node);
    }
    return nullptr;
}

//------------------------------------------------------------------------
// LowerUnsignedDivOrMod: Lowers a GT_UDIV/GT_UMOD node.
//
// Arguments:
//    divMod - pointer to the GT_UDIV/GT_UMOD node to be lowered
//
// Return Value:
//    Returns a boolean indicating whether the node was transformed.
//
// Notes:
//    - Transform UDIV/UMOD by power of 2 into RSZ/AND
//    - Transform UDIV by constant >= 2^(N-1) into GE
//    - Transform UDIV/UMOD by constant >= 3 into "magic division"
//

bool Lowering::LowerUnsignedDivOrMod(GenTreeOp* divMod)
{
    assert(divMod->OperIs(GT_UDIV, GT_UMOD));

#if defined(USE_HELPERS_FOR_INT_DIV)
    if (!varTypeIsIntegral(divMod->TypeGet()))
    {
        assert(!"unreachable: integral GT_UDIV/GT_UMOD should get morphed into helper calls");
    }
    assert(varTypeIsFloating(divMod->TypeGet()));
#endif // USE_HELPERS_FOR_INT_DIV
#if defined(TARGET_ARM64)
    assert(divMod->OperGet() != GT_UMOD);
#endif // TARGET_ARM64

    GenTree* dividend = divMod->gtGetOp1();
    GenTree* divisor  = divMod->gtGetOp2();

#if !defined(TARGET_64BIT)
    if (dividend->OperIs(GT_LONG))
    {
        return false;
    }
#endif

    if (!divisor->IsCnsIntOrI())
    {
        return false;
    }

    if (dividend->IsCnsIntOrI())
    {
        // We shouldn't see a divmod with constant operands here but if we do then it's likely
        // because optimizations are disabled or it's a case that's supposed to throw an exception.
        // Don't optimize this.
        return false;
    }

    const var_types type = divMod->TypeGet();
    assert((type == TYP_INT) || (type == TYP_I_IMPL));

    size_t divisorValue = static_cast<size_t>(divisor->AsIntCon()->IconValue());

    if (type == TYP_INT)
    {
        // Clear up the upper 32 bits of the value, they may be set to 1 because constants
        // are treated as signed and stored in ssize_t which is 64 bit in size on 64 bit targets.
        divisorValue &= UINT32_MAX;
    }

    if (divisorValue == 0)
    {
        return false;
    }

    const bool isDiv = divMod->OperIs(GT_UDIV);

    if (isPow2(divisorValue))
    {
        genTreeOps newOper;

        if (isDiv)
        {
            newOper      = GT_RSZ;
            divisorValue = genLog2(divisorValue);
        }
        else
        {
            newOper = GT_AND;
            divisorValue -= 1;
        }

        divMod->SetOper(newOper);
        divisor->AsIntCon()->SetIconValue(divisorValue);
        ContainCheckNode(divMod);
        return true;
    }
    if (isDiv)
    {
        // If the divisor is greater or equal than 2^(N - 1) then the result is 1
        // iff the dividend is greater or equal than the divisor.
        if (((type == TYP_INT) && (divisorValue > (UINT32_MAX / 2))) ||
            ((type == TYP_LONG) && (divisorValue > (UINT64_MAX / 2))))
        {
            divMod->SetOper(GT_GE);
            divMod->gtFlags |= GTF_UNSIGNED;
            ContainCheckNode(divMod);
            return true;
        }
    }

// TODO-ARM-CQ: Currently there's no GT_MULHI for ARM32
#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
    if (!comp->opts.MinOpts() && (divisorValue >= 3))
    {
        size_t magic;
        bool   increment;
        int    preShift;
        int    postShift;
        bool   simpleMul = false;

        if (type == TYP_INT)
        {
            magic =
                MagicDivide::GetUnsigned32Magic(static_cast<uint32_t>(divisorValue), &increment, &preShift, &postShift);

#ifdef TARGET_64BIT
            // avoid inc_saturate/multiple shifts by widening to 32x64 MULHI
            if (increment || (preShift
#ifdef TARGET_XARCH
                              // IMUL reg,reg,imm32 can't be used if magic<0 because of sign-extension
                              && static_cast<int32_t>(magic) < 0
#endif
                              ))
            {
                magic = MagicDivide::GetUnsigned64Magic(static_cast<uint64_t>(divisorValue), &increment, &preShift,
                                                        &postShift, 32);
            }
            // otherwise just widen to regular multiplication
            else
            {
                postShift += 32;
                simpleMul = true;
            }
#endif
        }
        else
        {
#ifdef TARGET_64BIT
            magic =
                MagicDivide::GetUnsigned64Magic(static_cast<uint64_t>(divisorValue), &increment, &preShift, &postShift);
#else
            unreached();
#endif
        }
        assert(divMod->MarkedDivideByConstOptimized());

        const bool                 requiresDividendMultiuse = !isDiv;
        const BasicBlock::weight_t curBBWeight              = m_block->getBBWeight(comp);

        if (requiresDividendMultiuse)
        {
            LIR::Use dividendUse(BlockRange(), &divMod->gtOp1, divMod);
            dividend = ReplaceWithLclVar(dividendUse);
        }

        GenTree* firstNode        = nullptr;
        GenTree* adjustedDividend = dividend;

        // If "increment" flag is returned by GetUnsignedMagic we need to do Saturating Increment first
        if (increment)
        {
            adjustedDividend = comp->gtNewOperNode(GT_INC_SATURATE, type, adjustedDividend);
            BlockRange().InsertBefore(divMod, adjustedDividend);
            firstNode = adjustedDividend;
            assert(!preShift);
        }
        // if "preShift" is required, then do a right shift before
        else if (preShift)
        {
            GenTree* preShiftBy = comp->gtNewIconNode(preShift, TYP_INT);
            adjustedDividend    = comp->gtNewOperNode(GT_RSZ, type, adjustedDividend, preShiftBy);
            BlockRange().InsertBefore(divMod, preShiftBy, adjustedDividend);
            firstNode = preShiftBy;
        }
        else if (type != TYP_I_IMPL)
        {
            adjustedDividend = comp->gtNewCastNode(TYP_I_IMPL, adjustedDividend, true, TYP_U_IMPL);
            BlockRange().InsertBefore(divMod, adjustedDividend);
            firstNode = adjustedDividend;
        }

#ifdef TARGET_XARCH
        // force input transformation to RAX because the following MULHI will kill RDX:RAX anyway and LSRA often causes
        // reduntant copies otherwise
        if (firstNode && !simpleMul)
            adjustedDividend->SetRegNum(REG_RAX);
#endif

        divisor->gtType = TYP_I_IMPL;
        divisor->AsIntCon()->SetIconValue(magic);

        if (isDiv && !postShift && type == TYP_I_IMPL)
        {
            divMod->SetOper(GT_MULHI);
            divMod->gtOp1 = adjustedDividend;
            divMod->gtFlags |= GTF_UNSIGNED;
        }
        else
        {
            // Insert a new GT_MULHI node before the existing GT_UDIV/GT_UMOD node.
            // The existing node will later be transformed into a GT_RSZ/GT_SUB that
            // computes the final result. This way don't need to find and change the use
            // of the existing node.
            GenTree* mulhi = comp->gtNewOperNode(simpleMul ? GT_MUL : GT_MULHI, TYP_I_IMPL, adjustedDividend, divisor);
            mulhi->gtFlags |= GTF_UNSIGNED;
            BlockRange().InsertBefore(divMod, mulhi);
            if (!firstNode)
                firstNode = mulhi;

            if (postShift)
            {
                GenTree* shiftBy = comp->gtNewIconNode(postShift, TYP_INT);
                BlockRange().InsertBefore(divMod, shiftBy);

                if (isDiv && type == TYP_I_IMPL)
                {
                    divMod->SetOper(GT_RSZ);
                    divMod->gtOp1 = mulhi;
                    divMod->gtOp2 = shiftBy;
                }
                else
                {
                    mulhi = comp->gtNewOperNode(GT_RSZ, TYP_I_IMPL, mulhi, shiftBy);
                    BlockRange().InsertBefore(divMod, mulhi);
                }
            }

            if (!isDiv)
            {
                // divisor UMOD dividend = dividend SUB (div MUL divisor)
                GenTree* divisor = comp->gtNewIconNode(divisorValue, type);
                GenTree* mul     = comp->gtNewOperNode(GT_MUL, type, mulhi, divisor);
                dividend         = comp->gtNewLclvNode(dividend->AsLclVar()->GetLclNum(), dividend->TypeGet());

                divMod->SetOper(GT_SUB);
                divMod->gtOp1 = dividend;
                divMod->gtOp2 = mul;

                BlockRange().InsertBefore(divMod, divisor, mul, dividend);
            }
            else if (type != TYP_I_IMPL)
            {
#ifdef TARGET_ARMARCH
                divMod->SetOper(GT_CAST);
                divMod->gtFlags |= GTF_UNSIGNED;
                divMod->AsCast()->gtCastType = TYP_UINT;
#else
                divMod->SetOper(GT_BITCAST);
#endif
                divMod->gtOp1 = mulhi;
                divMod->gtOp2 = nullptr;
            }
        }

        if (firstNode)
            ContainCheckRange(firstNode, divMod);
        return true;
    }
#endif
    return false;
}

// LowerConstIntDivOrMod: Transform integer GT_DIV/GT_MOD nodes with a power of 2
//     const divisor into equivalent but faster sequences.
//
// Arguments:
//    node - pointer to the DIV or MOD node
//
// Returns:
//    nullptr if no transformation is done, or the next node in the transformed node sequence that
//    needs to be lowered.
//
GenTree* Lowering::LowerConstIntDivOrMod(GenTree* node)
{
    assert((node->OperGet() == GT_DIV) || (node->OperGet() == GT_MOD));
    GenTree* divMod   = node;
    GenTree* dividend = divMod->gtGetOp1();
    GenTree* divisor  = divMod->gtGetOp2();

    const var_types type = divMod->TypeGet();
    assert((type == TYP_INT) || (type == TYP_LONG));

#if defined(USE_HELPERS_FOR_INT_DIV)
    assert(!"unreachable: integral GT_DIV/GT_MOD should get morphed into helper calls");
#endif // USE_HELPERS_FOR_INT_DIV
#if defined(TARGET_ARM64)
    assert(node->OperGet() != GT_MOD);
#endif // TARGET_ARM64

    if (!divisor->IsCnsIntOrI())
    {
        return nullptr; // no transformations to make
    }

    if (dividend->IsCnsIntOrI())
    {
        // We shouldn't see a divmod with constant operands here but if we do then it's likely
        // because optimizations are disabled or it's a case that's supposed to throw an exception.
        // Don't optimize this.
        return nullptr;
    }

    ssize_t divisorValue = divisor->AsIntCon()->IconValue();

    if (divisorValue == -1 || divisorValue == 0)
    {
        // x / 0 and x % 0 can't be optimized because they are required to throw an exception.

        // x / -1 can't be optimized because INT_MIN / -1 is required to throw an exception.

        // x % -1 is always 0 and the IL spec says that the rem instruction "can" throw an exception if x is
        // the minimum representable integer. However, the C# spec says that an exception "is" thrown in this
        // case so optimizing this case would break C# code.

        // A runtime check could be used to handle this case but it's probably too rare to matter.
        return nullptr;
    }

    bool isDiv = divMod->OperGet() == GT_DIV;

    if (isDiv)
    {
        if ((type == TYP_INT && divisorValue == INT_MIN) || (type == TYP_LONG && divisorValue == INT64_MIN))
        {
            // If the divisor is the minimum representable integer value then we can use a compare,
            // the result is 1 iff the dividend equals divisor.
            divMod->SetOper(GT_EQ);
            return node;
        }
    }

    size_t absDivisorValue =
        (divisorValue == SSIZE_T_MIN) ? static_cast<size_t>(divisorValue) : static_cast<size_t>(abs(divisorValue));

    if (!isPow2(absDivisorValue))
    {
        if (comp->opts.MinOpts())
        {
            return nullptr;
        }

#if defined(TARGET_XARCH) || defined(TARGET_ARM64)
        ssize_t magic;
        int     shift;

        if (type == TYP_INT)
        {
            magic = MagicDivide::GetSigned32Magic(static_cast<int32_t>(divisorValue), &shift);
        }
        else
        {
#ifdef TARGET_64BIT
            magic = MagicDivide::GetSigned64Magic(static_cast<int64_t>(divisorValue), &shift);
#else  // !TARGET_64BIT
            unreached();
#endif // !TARGET_64BIT
        }

        divisor->AsIntConCommon()->SetIconValue(magic);

        // Insert a new GT_MULHI node in front of the existing GT_DIV/GT_MOD node.
        // The existing node will later be transformed into a GT_ADD/GT_SUB that
        // computes the final result. This way don't need to find and change the
        // use of the existing node.
        GenTree* mulhi = comp->gtNewOperNode(GT_MULHI, type, divisor, dividend);
        BlockRange().InsertBefore(divMod, mulhi);

        // mulhi was the easy part. Now we need to generate different code depending
        // on the divisor value:
        // For 3 we need:
        //     div = signbit(mulhi) + mulhi
        // For 5 we need:
        //     div = signbit(mulhi) + sar(mulhi, 1) ; requires shift adjust
        // For 7 we need:
        //     mulhi += dividend                    ; requires add adjust
        //     div = signbit(mulhi) + sar(mulhi, 2) ; requires shift adjust
        // For -3 we need:
        //     mulhi -= dividend                    ; requires sub adjust
        //     div = signbit(mulhi) + sar(mulhi, 1) ; requires shift adjust
        bool requiresAddSubAdjust     = signum(divisorValue) != signum(magic);
        bool requiresShiftAdjust      = shift != 0;
        bool requiresDividendMultiuse = requiresAddSubAdjust || !isDiv;

        if (requiresDividendMultiuse)
        {
            LIR::Use dividendUse(BlockRange(), &mulhi->AsOp()->gtOp2, mulhi);
            dividend = ReplaceWithLclVar(dividendUse);
        }

        GenTree* adjusted;

        if (requiresAddSubAdjust)
        {
            dividend = comp->gtNewLclvNode(dividend->AsLclVar()->GetLclNum(), dividend->TypeGet());
            adjusted = comp->gtNewOperNode(divisorValue > 0 ? GT_ADD : GT_SUB, type, mulhi, dividend);
            BlockRange().InsertBefore(divMod, dividend, adjusted);
        }
        else
        {
            adjusted = mulhi;
        }

        GenTree* shiftBy = comp->gtNewIconNode(genTypeSize(type) * 8 - 1, type);
        GenTree* signBit = comp->gtNewOperNode(GT_RSZ, type, adjusted, shiftBy);
        BlockRange().InsertBefore(divMod, shiftBy, signBit);

        LIR::Use adjustedUse(BlockRange(), &signBit->AsOp()->gtOp1, signBit);
        adjusted = ReplaceWithLclVar(adjustedUse);
        adjusted = comp->gtNewLclvNode(adjusted->AsLclVar()->GetLclNum(), adjusted->TypeGet());
        BlockRange().InsertBefore(divMod, adjusted);

        if (requiresShiftAdjust)
        {
            shiftBy  = comp->gtNewIconNode(shift, TYP_INT);
            adjusted = comp->gtNewOperNode(GT_RSH, type, adjusted, shiftBy);
            BlockRange().InsertBefore(divMod, shiftBy, adjusted);
        }

        if (isDiv)
        {
            divMod->SetOperRaw(GT_ADD);
            divMod->AsOp()->gtOp1 = adjusted;
            divMod->AsOp()->gtOp2 = signBit;
        }
        else
        {
            GenTree* div = comp->gtNewOperNode(GT_ADD, type, adjusted, signBit);

            dividend = comp->gtNewLclvNode(dividend->AsLclVar()->GetLclNum(), dividend->TypeGet());

            // divisor % dividend = dividend - divisor x div
            GenTree* divisor = comp->gtNewIconNode(divisorValue, type);
            GenTree* mul     = comp->gtNewOperNode(GT_MUL, type, div, divisor);
            BlockRange().InsertBefore(divMod, dividend, div, divisor, mul);

            divMod->SetOperRaw(GT_SUB);
            divMod->AsOp()->gtOp1 = dividend;
            divMod->AsOp()->gtOp2 = mul;
        }

        return mulhi;
#elif defined(TARGET_ARM)
        // Currently there's no GT_MULHI for ARM32
        return nullptr;
#else
#error Unsupported or unset target architecture
#endif
    }

    // TODO-MIKE-ARM64-CQ: Signed division by 2 generate a LSR that can be combined with
    // the subsequent ADD.

    // We're committed to the conversion now. Go find the use if any.
    LIR::Use use;
    if (!BlockRange().TryGetUse(node, &use))
    {
        return nullptr;
    }

    // We need to use the dividend node multiple times so its value needs to be
    // computed once and stored in a temp variable.
    LIR::Use opDividend(BlockRange(), &divMod->AsOp()->gtOp1, divMod);
    dividend = ReplaceWithLclVar(opDividend);

    GenTree* adjustment = comp->gtNewOperNode(GT_RSH, type, dividend, comp->gtNewIconNode(type == TYP_INT ? 31 : 63));

    if (absDivisorValue == 2)
    {
        // If the divisor is +/-2 then we'd end up with a bitwise and between 0/-1 and 1.
        // We can get the same result by using GT_RSZ instead of GT_RSH.
        adjustment->SetOper(GT_RSZ);
    }
    else
    {
        adjustment = comp->gtNewOperNode(GT_AND, type, adjustment, comp->gtNewIconNode(absDivisorValue - 1, type));
    }

    GenTree* adjustedDividend =
        comp->gtNewOperNode(GT_ADD, type, adjustment,
                            comp->gtNewLclvNode(dividend->AsLclVar()->GetLclNum(), dividend->TypeGet()));

    GenTree* newDivMod;

    if (isDiv)
    {
        // perform the division by right shifting the adjusted dividend
        divisor->AsIntCon()->SetIconValue(genLog2(absDivisorValue));

        newDivMod = comp->gtNewOperNode(GT_RSH, type, adjustedDividend, divisor);
        ContainCheckShiftRotate(newDivMod->AsOp());

        if (divisorValue < 0)
        {
            // negate the result if the divisor is negative
            newDivMod = comp->gtNewOperNode(GT_NEG, type, newDivMod);
            ContainCheckNode(newDivMod);
        }
    }
    else
    {
        // divisor % dividend = dividend - divisor x (dividend / divisor)
        // divisor x (dividend / divisor) translates to (dividend >> log2(divisor)) << log2(divisor)
        // which simply discards the low log2(divisor) bits, that's just dividend & ~(divisor - 1)
        divisor->AsIntCon()->SetIconValue(~(absDivisorValue - 1));

        newDivMod = comp->gtNewOperNode(GT_SUB, type,
                                        comp->gtNewLclvNode(dividend->AsLclVar()->GetLclNum(), dividend->TypeGet()),
                                        comp->gtNewOperNode(GT_AND, type, adjustedDividend, divisor));
    }

    // Remove the divisor and dividend nodes from the linear order,
    // since we have reused them and will resequence the tree
    BlockRange().Remove(divisor);
    BlockRange().Remove(dividend);

    // linearize and insert the new tree before the original divMod node
    InsertTreeBeforeAndContainCheck(divMod, newDivMod);
    BlockRange().Remove(divMod);

    // replace the original divmod node with the new divmod tree
    use.ReplaceWith(comp, newDivMod);

    return newDivMod->gtNext;
}
//------------------------------------------------------------------------
// LowerSignedDivOrMod: transform integer GT_DIV/GT_MOD nodes with a power of 2
// const divisor into equivalent but faster sequences.
//
// Arguments:
//    node - the DIV or MOD node
//
// Returns:
//    The next node to lower.
//
GenTree* Lowering::LowerSignedDivOrMod(GenTree* node)
{
    assert((node->OperGet() == GT_DIV) || (node->OperGet() == GT_MOD));
    GenTree* next = node->gtNext;

    if (varTypeIsIntegral(node->TypeGet()))
    {
        // LowerConstIntDivOrMod will return nullptr if it doesn't transform the node.
        GenTree* newNode = LowerConstIntDivOrMod(node);
        if (newNode != nullptr)
        {
            return newNode;
        }
    }
    ContainCheckDivOrMod(node->AsOp());

    return next;
}

#ifndef TARGET_ARM64
//------------------------------------------------------------------------
// LowerShift: Lower shift nodes
//
// Arguments:
//    shift - the shift node (GT_LSH, GT_RSH or GT_RSZ)
//
void Lowering::LowerShift(GenTreeOp* shift)
{
    assert(shift->OperIs(GT_LSH, GT_RSH, GT_RSZ));

    GenTree* shiftBy = shift->GetOp(1);

    if (shiftBy->OperIs(GT_CNS_INT))
    {
#if defined(TARGET_AMD64) || defined(TARGET_ARM64)
        size_t mask = varTypeIsLong(shift->GetType()) ? 0x3f : 0x1f;
#elif defined(TARGET_X86) || defined(TARGET_ARM)
        size_t mask = 0x1f;
#else
#error Unknown target
#endif

        unsigned shiftByBits = static_cast<unsigned>(shiftBy->AsIntCon()->GetValue()) & mask;
        shiftBy->AsIntCon()->SetValue(shiftByBits);

        if ((shiftByBits >= 24) && shift->OperIs(GT_LSH) && comp->opts.OptimizationEnabled())
        {
            // Remove source casts if the shift discards the produced sign/zero bits.
            //
            // Some of this would probably be better done during morph or some sort
            // of tree narrowing phase. The problem is that this removes INT to LONG
            // casts, transforming
            //     LSH.long(CAST.long(x.int), 32)
            // into
            //     LSH.long(x.int, 32)
            //
            // While there's nothing intrinsically wrong about having a node with
            // different source and destination types, it is possible that some
            // frontend phases might get confused by such a shift node.

            unsigned consumedBits = varTypeBitSize(shift->GetType());

            assert((consumedBits == 32) || (consumedBits == 64));
            assert(shiftByBits < consumedBits);

            consumedBits -= shiftByBits;

            GenTree* src = shift->GetOp(0);

            while (src->OperIs(GT_CAST) && !src->gtOverflow())
            {
                GenTreeCast* cast = src->AsCast();

                if (!varTypeIsIntegral(cast->GetOp(0)->GetType()))
                {
                    break;
                }

                var_types castType = cast->GetCastType();

                // A (U)LONG - (U)LONG cast would normally produce 64 bits but since it
                // has no effect we make it produce 32 bits to keep the check simple.
                // Anyway such a cast should have been removed earlier.
                unsigned producedBits = varTypeIsSmall(castType) ? varTypeBitSize(castType) : 32;

                if (consumedBits > producedBits)
                {
                    break;
                }

                JITDUMP("Removing CAST [%06d] producing %u bits from LSH [%06d] consuming %u bits\n", cast->gtTreeID,
                        producedBits, shift->gtTreeID, consumedBits);

                BlockRange().Remove(src);
                src = cast->GetOp(0);
                src->ClearContained();

#if !defined(TARGET_64BIT)
                if (src->OperIs(GT_LONG))
                {
                    // We're run into a long to int cast on a 32 bit target. The LONG node
                    // needs to be removed since the shift wouldn't know what to do with it.
                    // TODO-MIKE-Cleanup: Why doesn't CAST lowering deal with this?!

                    BlockRange().Remove(src);
                    src->AsOp()->GetOp(1)->SetUnusedValue();
                    src = src->AsOp()->GetOp(0);
                }
#endif
            }

#if defined(TARGET_XARCH)
            // If the source is a small signed int memory operand then we can make it unsigned
            // if the sign bits aren't consumed, movzx has smaller encoding than movsx.

            if (src->OperIs(GT_LCL_FLD, GT_IND) && varTypeIsSmall(src->GetType()) &&
                (consumedBits <= varTypeBitSize(src->GetType())))
            {
                src->SetType(genUnsignedType(src->GetType()));
            }
#endif

            shift->SetOp(0, src);
        }
    }
    else
    {
#if defined(TARGET_AMD64) || defined(TARGET_ARM64)
        size_t mask = varTypeIsLong(shift->GetType()) ? 0x3f : 0x1f;
#elif defined(TARGET_X86)
        size_t mask = 0x1f;
#elif defined(TARGET_ARM)
        size_t mask = 0xff;
#elif
#error Unknown target
#endif

#if !defined(TARGET_ARM)
        // Remove unnecessary shift count masking. x64/x86/ARM64 shift instructions mask the shift count
        // to 5 bits (or 6 bits for 64 bit operations). ARM32 only masks 8 bits so this isn't likely to
        // be very useful since the main goal is to remove the masking done by the C# compiler.

        while (shiftBy->OperIs(GT_AND))
        {
            GenTree* maskOp = shiftBy->AsOp()->GetOp(1);

            if (!maskOp->OperIs(GT_CNS_INT))
            {
                break;
            }

            if ((static_cast<size_t>(maskOp->AsIntCon()->GetValue()) & mask) != mask)
            {
                break;
            }

            BlockRange().Remove(shiftBy);
            BlockRange().Remove(maskOp);

            shiftBy = shiftBy->AsOp()->GetOp(0);
            shiftBy->ClearContained();
        }

        shift->SetOp(1, shiftBy);
#endif
    }

    ContainCheckShiftRotate(shift);
}

#endif // !TARGET_ARM64

void Lowering::WidenSIMD12IfNecessary(GenTreeLclVarCommon* node)
{
#ifdef FEATURE_SIMD
    if (node->TypeGet() == TYP_SIMD12)
    {
        // Assumption 1:
        // RyuJit backend depends on the assumption that on 64-Bit targets Vector3 size is rounded off
        // to TARGET_POINTER_SIZE and hence Vector3 locals on stack can be treated as TYP_SIMD16 for
        // reading and writing purposes.
        //
        // Assumption 2:
        // RyuJit backend is making another implicit assumption that Vector3 type args when passed in
        // registers or on stack, the upper most 4-bytes will be zero.
        //
        // For P/Invoke return and Reverse P/Invoke argument passing, native compiler doesn't guarantee
        // that upper 4-bytes of a Vector3 type struct is zero initialized and hence assumption 2 is
        // invalid.
        //
        // RyuJIT x64 Windows: arguments are treated as passed by ref and hence read/written just 12
        // bytes. In case of Vector3 returns, Caller allocates a zero initialized Vector3 local and
        // passes it retBuf arg and Callee method writes only 12 bytes to retBuf. For this reason,
        // there is no need to clear upper 4-bytes of Vector3 type args.
        //
        // RyuJIT x64 Unix: arguments are treated as passed by value and read/writen as if TYP_SIMD16.
        // Vector3 return values are returned two return registers and Caller assembles them into a
        // single xmm reg. Hence RyuJIT explicitly generates code to clears upper 4-bytes of Vector3
        // type args in prolog and Vector3 type return value of a call
        //
        // RyuJIT x86 Windows: all non-param Vector3 local vars are allocated as 16 bytes. Vector3 arguments
        // are pushed as 12 bytes. For return values, a 16-byte local is allocated and the address passed
        // as a return buffer pointer. The callee doesn't write the high 4 bytes, and we don't need to clear
        // it either.

        unsigned   varNum = node->AsLclVarCommon()->GetLclNum();
        LclVarDsc* varDsc = &comp->lvaTable[varNum];

        if (comp->lvaMapSimd12ToSimd16(varDsc))
        {
            JITDUMP("Mapping TYP_SIMD12 lclvar node to TYP_SIMD16:\n");
            DISPNODE(node);
            JITDUMP("============");

            node->gtType = TYP_SIMD16;
        }
    }
#endif // FEATURE_SIMD
}

//------------------------------------------------------------------------
// LowerArrElem: Lower a GT_ARR_ELEM node
//
// Arguments:
//    node - the GT_ARR_ELEM node to lower.
//
// Return Value:
//    The next node to lower.
//
// Assumptions:
//    pTree points to a pointer to a GT_ARR_ELEM node.
//
// Notes:
//    This performs the following lowering.  We start with a node of the form:
//          /--*  <arrObj>
//          +--*  <index0>
//          +--*  <index1>
//       /--*  arrMD&[,]
//
//    First, we create temps for arrObj if it is not already a lclVar, and for any of the index
//    expressions that have side-effects.
//    We then transform the tree into:
//                      <offset is null - no accumulated offset for the first index>
//                   /--*  <arrObj>
//                   +--*  <index0>
//                /--*  ArrIndex[i, ]
//                +--*  <arrObj>
//             /--|  arrOffs[i, ]
//             |  +--*  <arrObj>
//             |  +--*  <index1>
//             +--*  ArrIndex[*,j]
//             +--*  <arrObj>
//          /--|  arrOffs[*,j]
//          +--*  lclVar NewTemp
//       /--*  lea (scale = element size, offset = offset of first element)
//
//    The new stmtExpr may be omitted if the <arrObj> is a lclVar.
//    The new stmtExpr may be embedded if the <arrObj> is not the first tree in linear order for
//    the statement containing the original arrMD.
//    Note that the arrMDOffs is the INDEX of the lea, but is evaluated before the BASE (which is the second
//    reference to NewTemp), because that provides more accurate lifetimes.
//    There may be 1, 2 or 3 dimensions, with 1, 2 or 3 arrMDIdx nodes, respectively.
//
GenTree* Lowering::LowerArrElem(GenTree* node)
{
    // This will assert if we don't have an ArrElem node
    GenTreeArrElem*     arrElem = node->AsArrElem();
    const unsigned char rank    = arrElem->gtArrRank;

    JITDUMP("Lowering ArrElem\n");
    JITDUMP("============\n");
    DISPTREERANGE(BlockRange(), arrElem);
    JITDUMP("\n");

    assert(arrElem->gtArrObj->TypeGet() == TYP_REF);

    // We need to have the array object in a lclVar.
    if (!arrElem->gtArrObj->IsLocal())
    {
        LIR::Use arrObjUse(BlockRange(), &arrElem->gtArrObj, arrElem);
        ReplaceWithLclVar(arrObjUse);
    }

    GenTree* arrObjNode = arrElem->gtArrObj;
    assert(arrObjNode->IsLocal());

    GenTree* insertionPoint = arrElem;

    // The first ArrOffs node will have 0 for the offset of the previous dimension.
    GenTree* prevArrOffs = new (comp, GT_CNS_INT) GenTreeIntCon(TYP_I_IMPL, 0);
    BlockRange().InsertBefore(insertionPoint, prevArrOffs);
    GenTree* nextToLower = prevArrOffs;

    for (unsigned char dim = 0; dim < rank; dim++)
    {
        GenTree* indexNode = arrElem->gtArrInds[dim];

        // Use the original arrObjNode on the 0th ArrIndex node, and clone it for subsequent ones.
        GenTree* idxArrObjNode;
        if (dim == 0)
        {
            idxArrObjNode = arrObjNode;
        }
        else
        {
            idxArrObjNode = comp->gtClone(arrObjNode);
            BlockRange().InsertBefore(insertionPoint, idxArrObjNode);
        }

        // Next comes the GT_ARR_INDEX node.
        GenTreeArrIndex* arrMDIdx = new (comp, GT_ARR_INDEX)
            GenTreeArrIndex(TYP_INT, idxArrObjNode, indexNode, dim, rank, arrElem->gtArrElemType);
        arrMDIdx->gtFlags |= ((idxArrObjNode->gtFlags | indexNode->gtFlags) & GTF_ALL_EFFECT);
        BlockRange().InsertBefore(insertionPoint, arrMDIdx);

        GenTree* offsArrObjNode = comp->gtClone(arrObjNode);
        BlockRange().InsertBefore(insertionPoint, offsArrObjNode);

        GenTreeArrOffs* arrOffs = new (comp, GT_ARR_OFFSET)
            GenTreeArrOffs(TYP_I_IMPL, prevArrOffs, arrMDIdx, offsArrObjNode, dim, rank, arrElem->gtArrElemType);
        arrOffs->gtFlags |= ((prevArrOffs->gtFlags | arrMDIdx->gtFlags | offsArrObjNode->gtFlags) & GTF_ALL_EFFECT);
        BlockRange().InsertBefore(insertionPoint, arrOffs);

        prevArrOffs = arrOffs;
    }

    // Generate the LEA and make it reverse evaluation, because we want to evaluate the index expression before the
    // base.
    unsigned scale  = arrElem->gtArrElemSize;
    unsigned offset = comp->eeGetMDArrayDataOffset(arrElem->gtArrElemType, arrElem->gtArrRank);

    GenTree* leaIndexNode = prevArrOffs;
    if (!jitIsScaleIndexMul(scale))
    {
        // We do the address arithmetic in TYP_I_IMPL, though note that the lower bounds and lengths in memory are
        // TYP_INT
        GenTree* scaleNode = new (comp, GT_CNS_INT) GenTreeIntCon(TYP_I_IMPL, scale);
        GenTree* mulNode   = new (comp, GT_MUL) GenTreeOp(GT_MUL, TYP_I_IMPL, leaIndexNode, scaleNode);
        BlockRange().InsertBefore(insertionPoint, scaleNode, mulNode);
        leaIndexNode = mulNode;
        scale        = 1;
    }

    GenTree* leaBase = comp->gtClone(arrObjNode);
    BlockRange().InsertBefore(insertionPoint, leaBase);

    GenTree* leaNode = new (comp, GT_LEA) GenTreeAddrMode(arrElem->TypeGet(), leaBase, leaIndexNode, scale, offset);

    BlockRange().InsertBefore(insertionPoint, leaNode);

    LIR::Use arrElemUse;
    if (BlockRange().TryGetUse(arrElem, &arrElemUse))
    {
        arrElemUse.ReplaceWith(comp, leaNode);
    }
    else
    {
        leaNode->SetUnusedValue();
    }

    BlockRange().Remove(arrElem);

    JITDUMP("Results of lowering ArrElem:\n");
    DISPTREERANGE(BlockRange(), leaNode);
    JITDUMP("\n\n");

    return nextToLower;
}

PhaseStatus Lowering::DoPhase()
{
    // If we have any PInvoke calls, insert the one-time prolog code. We'll inserted the epilog code in the
    // appropriate spots later. NOTE: there is a minor optimization opportunity here, as we still create p/invoke
    // data structures and setup/teardown even if we've eliminated all p/invoke calls due to dead code elimination.
    if (comp->compMethodRequiresPInvokeFrame())
    {
        InsertPInvokeMethodProlog();
    }

#if !defined(TARGET_64BIT)
    DecomposeLongs decomp(comp); // Initialize the long decomposition class.
    if (comp->compLongUsed)
    {
        decomp.PrepareForDecomposition();
    }
#endif // !defined(TARGET_64BIT)

    for (BasicBlock* block = comp->fgFirstBB; block; block = block->bbNext)
    {
        /* Make the block publicly available */
        comp->compCurBB = block;

#if !defined(TARGET_64BIT)
        if (comp->compLongUsed)
        {
            decomp.DecomposeBlock(block);
        }
#endif //! TARGET_64BIT

        LowerBlock(block);
    }

#ifdef DEBUG
    JITDUMP("Lower has completed modifying nodes.\n");
    if (VERBOSE)
    {
        comp->fgDispBasicBlocks(true);
    }
#endif

    // Recompute local var ref counts before potentially sorting for liveness.
    // Note this does minimal work in cases where we are not going to sort.
    const bool isRecompute    = true;
    const bool setSlotNumbers = false;
    comp->lvaComputeRefCounts(isRecompute, setSlotNumbers);

    comp->fgLocalVarLiveness();
    // local var liveness can delete code, which may create empty blocks
    if (comp->opts.OptimizationEnabled())
    {
        comp->optLoopsMarked = false;
        bool modified        = comp->fgUpdateFlowGraph();
        if (modified)
        {
            JITDUMP("had to run another liveness pass:\n");
            comp->fgLocalVarLiveness();
        }
    }

    // Recompute local var ref counts again after liveness to reflect
    // impact of any dead code removal. Note this may leave us with
    // tracked vars that have zero refs.
    comp->lvaComputeRefCounts(isRecompute, setSlotNumbers);

    return PhaseStatus::MODIFIED_EVERYTHING;
}

#ifdef DEBUG

//------------------------------------------------------------------------
// Lowering::CheckCallArg: check that a call argument is in an expected
//                         form after lowering.
//
// Arguments:
//   arg - the argument to check.
//
void Lowering::CheckCallArg(GenTree* arg)
{
    if (!arg->IsValue() && !arg->OperIsPutArgStk())
    {
        assert(arg->OperIsStore() || arg->IsArgPlaceHolderNode() || arg->IsNothingNode());
        return;
    }

    if (arg->OperIs(GT_FIELD_LIST))
    {
        GenTreeFieldList* list = arg->AsFieldList();
        assert(list->isContained());

        for (GenTreeFieldList::Use& use : list->Uses())
        {
            assert(use.GetNode()->OperIs(GT_PUTARG_REG));
        }
    }
    else
    {
#if FEATURE_ARG_SPLIT
        assert(arg->OperIs(GT_PUTARG_REG, GT_PUTARG_STK, GT_PUTARG_SPLIT));
#else
        assert(arg->OperIs(GT_PUTARG_REG, GT_PUTARG_STK));
#endif
    }
}

//------------------------------------------------------------------------
// Lowering::CheckCall: check that a call is in an expected form after
//                      lowering. Currently this amounts to checking its
//                      arguments, but could be expanded to verify more
//                      properties in the future.
//
// Arguments:
//   call - the call to check.
//
void Lowering::CheckCall(GenTreeCall* call)
{
    if (call->gtCallThisArg != nullptr)
    {
        CheckCallArg(call->gtCallThisArg->GetNode());
    }

    for (GenTreeCall::Use& use : call->Args())
    {
        CheckCallArg(use.GetNode());
    }

    for (GenTreeCall::Use& use : call->LateArgs())
    {
        CheckCallArg(use.GetNode());
    }
}

//------------------------------------------------------------------------
// Lowering::CheckNode: check that an LIR node is in an expected form
//                      after lowering.
//
// Arguments:
//   compiler - the compiler context.
//   node - the node to check.
//
void Lowering::CheckNode(Compiler* compiler, GenTree* node)
{
    switch (node->OperGet())
    {
        case GT_CALL:
            CheckCall(node->AsCall());
            break;

#ifdef FEATURE_SIMD
        case GT_HWINTRINSIC:
            assert(!node->TypeIs(TYP_SIMD12));
            break;
#ifdef TARGET_64BIT
        case GT_LCL_VAR:
        case GT_STORE_LCL_VAR:
            if (node->TypeIs(TYP_SIMD12))
            {
                LclVarDsc* lcl = compiler->lvaGetDesc(node->AsLclVar());
                assert(lcl->IsDependentPromotedField(compiler) || (lcl->lvSize() == 12));
            }
            break;
#endif // TARGET_64BIT
#endif // SIMD

        case GT_LCL_VAR_ADDR:
        case GT_LCL_FLD_ADDR:
            assert(compiler->lvaGetDesc(node->AsLclVarCommon())->IsAddressExposed());
            break;

        default:
            break;
    }
}

//------------------------------------------------------------------------
// Lowering::CheckBlock: check that the contents of an LIR block are in an
//                       expected form after lowering.
//
// Arguments:
//   compiler - the compiler context.
//   block    - the block to check.
//
bool Lowering::CheckBlock(Compiler* compiler, BasicBlock* block)
{
    assert(block->isEmpty() || block->IsLIR());

    for (GenTree* node : LIR::AsRange(block).NonPhiNodes())
    {
        CheckNode(compiler, node);
    }

    assert(LIR::AsRange(block).CheckLIR(compiler, true));
    return true;
}
#endif

//------------------------------------------------------------------------
// Lowering::LowerBlock: Lower all the nodes in a BasicBlock
//
// Arguments:
//   block    - the block to lower.
//
void Lowering::LowerBlock(BasicBlock* block)
{
    assert(block == comp->compCurBB); // compCurBB must already be set.
    assert(block->isEmpty() || block->IsLIR());

    m_block = block;

    // NOTE: some of the lowering methods insert calls before the node being
    // lowered (See e.g. InsertPInvoke{Method,Call}{Prolog,Epilog}). In
    // general, any code that is inserted before the current node should be
    // "pre-lowered" as they won't be subject to further processing.
    // Lowering::CheckBlock() runs some extra checks on call arguments in
    // order to help catch unlowered nodes.

    GenTree* node = BlockRange().FirstNonPhiNode();
    while (node != nullptr)
    {
        node = LowerNode(node);
    }

    assert(CheckBlock(comp, block));
}

/** Verifies if both of these trees represent the same indirection.
 * Used by Lower to annotate if CodeGen generate an instruction of the
 * form *addrMode BinOp= expr
 *
 * Preconditions: both trees are children of GT_INDs and their underlying children
 * have the same gtOper.
 *
 * This is a first iteration to actually recognize trees that can be code-generated
 * as a single read-modify-write instruction on AMD64/x86.  For now
 * this method only supports the recognition of simple addressing modes (through GT_LEA)
 * or local var indirections.  Local fields, array access and other more complex nodes are
 * not yet supported.
 *
 * TODO-CQ:  Perform tree recognition by using the Value Numbering Package, that way we can recognize
 * arbitrary complex trees and support much more addressing patterns.
 */
bool Lowering::IndirsAreEquivalent(GenTree* candidate, GenTree* storeInd)
{
    assert(candidate->OperGet() == GT_IND);
    assert(storeInd->OperGet() == GT_STOREIND);

    // We should check the size of the indirections.  If they are
    // different, say because of a cast, then we can't call them equivalent.  Doing so could cause us
    // to drop a cast.
    // Signed-ness difference is okay and expected since a store indirection must always
    // be signed based on the CIL spec, but a load could be unsigned.
    if (genTypeSize(candidate->gtType) != genTypeSize(storeInd->gtType))
    {
        return false;
    }

    GenTree* pTreeA = candidate->gtGetOp1();
    GenTree* pTreeB = storeInd->gtGetOp1();

    // This method will be called by codegen (as well as during lowering).
    // After register allocation, the sources may have been spilled and reloaded
    // to a different register, indicated by an inserted GT_RELOAD node.
    pTreeA = pTreeA->gtSkipReloadOrCopy();
    pTreeB = pTreeB->gtSkipReloadOrCopy();

    genTreeOps oper;

    if (pTreeA->OperGet() != pTreeB->OperGet())
    {
        return false;
    }

    oper = pTreeA->OperGet();
    switch (oper)
    {
        case GT_LCL_VAR:
        case GT_LCL_VAR_ADDR:
        case GT_CLS_VAR_ADDR:
        case GT_CNS_INT:
            return NodesAreEquivalentLeaves(pTreeA, pTreeB);

        case GT_LEA:
        {
            GenTreeAddrMode* gtAddr1 = pTreeA->AsAddrMode();
            GenTreeAddrMode* gtAddr2 = pTreeB->AsAddrMode();
            return NodesAreEquivalentLeaves(gtAddr1->Base(), gtAddr2->Base()) &&
                   NodesAreEquivalentLeaves(gtAddr1->Index(), gtAddr2->Index()) &&
                   (gtAddr1->gtScale == gtAddr2->gtScale) && (gtAddr1->Offset() == gtAddr2->Offset());
        }
        default:
            // We don't handle anything that is not either a constant,
            // a local var or LEA.
            return false;
    }
}

/** Test whether the two given nodes are the same leaves.
 *  Right now, only constant integers and local variables are supported
 */
bool Lowering::NodesAreEquivalentLeaves(GenTree* tree1, GenTree* tree2)
{
    if (tree1 == nullptr && tree2 == nullptr)
    {
        return true;
    }

    // both null, they are equivalent, otherwise if either is null not equivalent
    if (tree1 == nullptr || tree2 == nullptr)
    {
        return false;
    }

    tree1 = tree1->gtSkipReloadOrCopy();
    tree2 = tree2->gtSkipReloadOrCopy();

    if (tree1->TypeGet() != tree2->TypeGet())
    {
        return false;
    }

    if (tree1->OperGet() != tree2->OperGet())
    {
        return false;
    }

    if (!tree1->OperIsLeaf() || !tree2->OperIsLeaf())
    {
        return false;
    }

    switch (tree1->OperGet())
    {
        case GT_CNS_INT:
            return tree1->AsIntCon()->gtIconVal == tree2->AsIntCon()->gtIconVal &&
                   tree1->IsIconHandle() == tree2->IsIconHandle();
        case GT_LCL_VAR:
        case GT_LCL_VAR_ADDR:
            return tree1->AsLclVarCommon()->GetLclNum() == tree2->AsLclVarCommon()->GetLclNum();
        case GT_CLS_VAR_ADDR:
            return tree1->AsClsVar()->gtClsVarHnd == tree2->AsClsVar()->gtClsVarHnd;
        default:
            return false;
    }
}

#if FEATURE_MULTIREG_RET

void Lowering::MakeMultiRegLclVar(GenTreeLclVar* lclVar, const ReturnTypeDesc* retDesc)
{
    assert(lclVar->OperIs(GT_LCL_VAR, GT_STORE_LCL_VAR));

    bool canEnregister = false;

    LclVarDsc* lcl = comp->lvaGetDesc(lclVar);

    if (comp->lvaEnregMultiRegVars && lcl->IsIndependentPromoted())
    {
        // We can enregister if we have a promoted struct and all the fields' types match the ABI requirements.
        // Note that we don't promote structs with explicit layout, so we don't need to check field offsets, and
        // if we have multiple types packed into a single register, we won't have matching reg and field counts,
        // so we can tolerate mismatches of integer size.

        // If we have no retTypeDesc, we only care that it is independently promoted.
        if (retDesc == nullptr)
        {
            canEnregister = true;
        }
        else if (retDesc->GetRegCount() == lcl->GetPromotedFieldCount())
        {
            canEnregister = true;
        }
    }

#ifdef TARGET_XARCH
    // For local stores on XARCH we only handle mismatched src/dest register count for
    // calls of SIMD type. If the source was another lclVar similarly promoted, we would
    // have broken it into multiple stores.
    if (lclVar->OperIs(GT_STORE_LCL_VAR) && !lclVar->GetOp(0)->OperIs(GT_CALL))
    {
        canEnregister = false;
    }
#endif // TARGET_XARCH

    if (canEnregister)
    {
        lclVar->SetMultiReg();
    }
    else
    {
        assert(!lclVar->IsMultiReg());

        if (lcl->IsPromoted() && !lcl->lvDoNotEnregister)
        {
            comp->lvaSetVarDoNotEnregister(lclVar->GetLclNum() DEBUGARG(Compiler::DNER_BlockOp));
        }
    }
}

#endif // FEATURE_MULTIREG_RET

//------------------------------------------------------------------------
// Containment Analysis
//------------------------------------------------------------------------
void Lowering::ContainCheckNode(GenTree* node)
{
    switch (node->gtOper)
    {
        case GT_STORE_LCL_VAR:
        case GT_STORE_LCL_FLD:
            ContainCheckStoreLcl(node->AsLclVarCommon());
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
        case GT_JCMP:
            ContainCheckCompare(node->AsOp());
            break;

        case GT_JTRUE:
            ContainCheckJTrue(node->AsUnOp());
            break;

        case GT_ADD:
        case GT_SUB:
#if !defined(TARGET_64BIT)
        case GT_ADD_LO:
        case GT_ADD_HI:
        case GT_SUB_LO:
        case GT_SUB_HI:
#endif
        case GT_AND:
        case GT_OR:
        case GT_XOR:
            ContainCheckBinary(node->AsOp());
            break;

#if defined(TARGET_X86)
        case GT_MUL_LONG:
#endif
        case GT_MUL:
        case GT_MULHI:
            ContainCheckMul(node->AsOp());
            break;
        case GT_DIV:
        case GT_MOD:
        case GT_UDIV:
        case GT_UMOD:
            ContainCheckDivOrMod(node->AsOp());
            break;
        case GT_LSH:
        case GT_RSH:
        case GT_RSZ:
        case GT_ROL:
        case GT_ROR:
#ifndef TARGET_64BIT
        case GT_LSH_HI:
        case GT_RSH_LO:
#endif
            ContainCheckShiftRotate(node->AsOp());
            break;
        case GT_ARR_OFFSET:
            ContainCheckArrOffset(node->AsArrOffs());
            break;
        case GT_LCLHEAP:
            ContainCheckLclHeap(node->AsOp());
            break;
        case GT_RETURN:
            ContainCheckRet(node->AsOp());
            break;
        case GT_RETURNTRAP:
            ContainCheckReturnTrap(node->AsOp());
            break;
        case GT_STOREIND:
            ContainCheckStoreIndir(node->AsStoreInd());
            break;
        case GT_IND:
            ContainCheckIndir(node->AsIndir());
            break;
        case GT_PUTARG_REG:
        case GT_PUTARG_STK:
#if FEATURE_ARG_SPLIT
        case GT_PUTARG_SPLIT:
#endif // FEATURE_ARG_SPLIT
            // The regNum must have been set by the lowering of the call.
            assert(node->GetRegNum() != REG_NA);
            break;
#ifdef TARGET_XARCH
        case GT_INTRINSIC:
            ContainCheckIntrinsic(node->AsOp());
            break;
#endif // TARGET_XARCH
#ifdef FEATURE_HW_INTRINSICS
        case GT_HWINTRINSIC:
            ContainCheckHWIntrinsic(node->AsHWIntrinsic());
            break;
#endif // FEATURE_HW_INTRINSICS
        default:
            break;
    }
}

//------------------------------------------------------------------------
// ContainCheckReturnTrap: determine whether the source of a RETURNTRAP should be contained.
//
// Arguments:
//    node - pointer to the GT_RETURNTRAP node
//
void Lowering::ContainCheckReturnTrap(GenTreeOp* node)
{
#ifdef TARGET_XARCH
    assert(node->OperIs(GT_RETURNTRAP));
    // This just turns into a compare of its child with an int + a conditional call
    if (node->gtOp1->isIndir())
    {
        MakeSrcContained(node, node->gtOp1);
    }
#endif // TARGET_XARCH
}

//------------------------------------------------------------------------
// ContainCheckArrOffset: determine whether the source of an ARR_OFFSET should be contained.
//
// Arguments:
//    node - pointer to the GT_ARR_OFFSET node
//
void Lowering::ContainCheckArrOffset(GenTreeArrOffs* node)
{
    assert(node->OperIs(GT_ARR_OFFSET));
    // we don't want to generate code for this
    if (node->gtOffset->IsIntegralConst(0))
    {
        MakeSrcContained(node, node->AsArrOffs()->gtOffset);
    }
}

//------------------------------------------------------------------------
// ContainCheckLclHeap: determine whether the source of a GT_LCLHEAP node should be contained.
//
// Arguments:
//    node - pointer to the node
//
void Lowering::ContainCheckLclHeap(GenTreeOp* node)
{
    assert(node->OperIs(GT_LCLHEAP));
    GenTree* size = node->AsOp()->gtOp1;
    if (size->IsCnsIntOrI())
    {
        MakeSrcContained(node, size);
    }
}

void Lowering::ContainCheckRet(GenTreeUnOp* ret)
{
    assert(ret->OperIs(GT_RETURN));

    GenTree* src = ret->GetOp(0);

#ifndef TARGET_64BIT
    if (ret->TypeIs(TYP_LONG))
    {
        if (src->TypeIs(TYP_DOUBLE))
        {
            return;
        }

        noway_assert(src->OperIs(GT_LONG));
        src->SetContained();

        return;
    }
#endif

#if FEATURE_MULTIREG_RET
    if (ret->TypeIs(TYP_STRUCT) && src->OperIs(GT_LCL_VAR))
    {
        LclVarDsc* lcl = comp->lvaGetDesc(src->AsLclVar());

        // This must be a multi-reg return or an HFA of a single element.
        assert(!lcl->IsPromoted() || lcl->lvIsMultiRegRet || (lcl->lvIsHfa() && varTypeUsesFloatReg(lcl->GetType())));

        // Mark var as contained if not enregisterable.
        if (src->TypeIs(TYP_STRUCT) && !src->AsLclVar()->IsMultiReg())
        {
            src->SetContained();
        }

        return;
    }
#endif
}

//------------------------------------------------------------------------
// ContainCheckJTrue: determine whether the source of a JTRUE should be contained.
//
// Arguments:
//    node - pointer to the node
//
void Lowering::ContainCheckJTrue(GenTreeUnOp* node)
{
    // The compare does not need to be generated into a register.
    GenTree* cmp = node->gtGetOp1();
    cmp->gtType  = TYP_VOID;
    cmp->gtFlags |= GTF_SET_FLAGS;
}

GenTree* Lowering::LowerBitCast(GenTreeUnOp* bitcast)
{
    assert(bitcast->OperIs(GT_BITCAST));
    assert(!bitcast->TypeIs(TYP_STRUCT));

    auto CanRetypeIndir = [](GenTreeIndir* indir, var_types type) {
#ifdef TARGET_ARMARCH
        // For simplicity and safety recognize only the typical int <-> float case.
        return (type == TYP_INT) || ((type == TYP_FLOAT) && indir->TypeIs(TYP_INT) && !indir->IsUnaligned());
#else
        return true;
#endif
    };

    auto CanRetypeLclFld = [](GenTreeLclFld* lclFld, var_types type) {
#ifdef TARGET_ARMARCH
        // For simplicity and safety recognize only the typical int <-> float case.
        return (type == TYP_INT) || ((type == TYP_FLOAT) && ((lclFld->GetLclOffs() % 4) == 0));
#else
        return true;
#endif
    };

    GenTree* next   = bitcast->gtNext;
    GenTree* src    = bitcast->GetOp(0);
    bool     remove = false;

    if ((src->OperIs(GT_IND) && CanRetypeIndir(src->AsIndir(), bitcast->GetType())) ||
        (src->OperIs(GT_LCL_FLD) && CanRetypeLclFld(src->AsLclFld(), bitcast->GetType())))
    {
        src->SetType(bitcast->GetType());
        remove = true;
    }
    else if (src->OperIs(GT_LCL_VAR))
    {
        if (comp->lvaGetDesc(src->AsLclVar())->lvDoNotEnregister)
        {
            // If it's not a register candidate then we can turn it into a LCL_FLD and retype it.
            src->ChangeOper(GT_LCL_FLD);
            src->SetType(bitcast->GetType());
            comp->lvaSetVarDoNotEnregister(src->AsLclFld()->GetLclNum() DEBUGARG(Compiler::DNER_LocalField));
            remove = true;
        }
        else
        {
            src->SetRegOptional();
        }
    }

    if (remove)
    {
        LIR::Use use;

        if (BlockRange().TryGetUse(bitcast, &use))
        {
            use.ReplaceWith(comp, src);
        }
        else
        {
            src->SetUnusedValue();
        }

        BlockRange().Remove(bitcast);
    }

    return next;
}

//------------------------------------------------------------------------
// LowerCast: Lower GT_CAST nodes.
//
// Arguments:
//    cast - GT_CAST node to be lowered
//
// Return Value:
//    The next node to lower.
//
GenTree* Lowering::LowerCast(GenTreeCast* cast)
{
    GenTree*  src     = cast->GetOp(0);
    var_types dstType = cast->GetCastType();
    var_types srcType = src->GetType();

    if (!cast->gtOverflow())
    {
        bool remove = false;

#ifndef TARGET_64BIT
        if (src->OperIs(GT_LONG))
        {
            assert((dstType == TYP_INT) || (dstType == TYP_UINT));

            BlockRange().Remove(src);
            src->AsOp()->GetOp(1)->SetUnusedValue();
            src    = src->AsOp()->GetOp(0);
            remove = true;
        }
        else
#endif

            if (varTypeIsIntegral(dstType) && varTypeIsIntegral(srcType) &&
                (varTypeSize(dstType) <= varTypeSize(srcType)) && IsContainableMemoryOp(src))
        {
            // This is a narrowing cast with an in memory load source, we can remove it and retype the load.

            // TODO-MIKE-Cleanup: fgMorphCast does something similar but more restrictive. It's not clear
            // if there are any advantages in doing such a transform earlier (in fact there may be one
            // disadvantage - retyping nodes may prevent them from being CSEd) so it should be deleted.

            if (dstType == TYP_UINT)
            {
                dstType = TYP_INT;
            }
            else if (dstType == TYP_ULONG)
            {
                dstType = TYP_LONG;
            }

            if (src->OperIs(GT_LCL_VAR))
            {
                src->ChangeOper(GT_LCL_FLD);
            }

            src->SetType(dstType);
            remove = true;
        }

        if (remove)
        {
            LIR::Use use;

            if (BlockRange().TryGetUse(cast, &use))
            {
                use.ReplaceWith(comp, src);
            }
            else
            {
                src->SetUnusedValue();
            }

            GenTree* next = cast->gtNext;
            BlockRange().Remove(cast);
            return next;
        }
    }

    ContainCheckCast(cast);

    return cast->gtNext;
}

//------------------------------------------------------------------------
// LowerStoreIndirCommon: a common logic to lower StoreIndir.
//
// Arguments:
//    ind - the store indirection node we are lowering.
//
void Lowering::LowerStoreIndirCommon(GenTreeStoreInd* ind)
{
    assert(!ind->TypeIs(TYP_STRUCT));

    TryCreateAddrMode(ind->Addr(), true);

    if (comp->codeGen->gcInfo.GetWriteBarrierForm(ind) == GCInfo::WBF_NoBarrier)
    {
        LowerStoreIndir(ind);
    }
}

//------------------------------------------------------------------------
// LowerIndir: a common logic to lower IND load or NullCheck.
//
// Arguments:
//    ind - the ind node we are lowering.
//
void Lowering::LowerIndir(GenTreeIndir* ind)
{
    assert(ind->OperIs(GT_IND, GT_NULLCHECK));
    // Process struct typed indirs separately unless they are unused;
    // they only appear as the source of a block copy operation or a return node.
    if (!ind->TypeIs(TYP_STRUCT) || ind->IsUnusedValue())
    {
        // TODO-Cleanup: We're passing isContainable = true but ContainCheckIndir rejects
        // address containment in some cases so we end up creating trivial (reg + offfset)
        // or (reg + reg) LEAs that are not necessary.
        TryCreateAddrMode(ind->Addr(), true);
        ContainCheckIndir(ind);

        if (ind->OperIs(GT_NULLCHECK) || ind->IsUnusedValue())
        {
            TransformUnusedIndirection(ind, comp, m_block);
        }
    }
}

//------------------------------------------------------------------------
// TransformUnusedIndirection: change the opcode and the type of the unused indirection.
//
// Arguments:
//    ind   - Indirection to transform.
//    comp  - Compiler instance.
//    block - Basic block of the indirection.
//
void Lowering::TransformUnusedIndirection(GenTreeIndir* ind, Compiler* comp, BasicBlock* block)
{
    // A nullcheck is essentially the same as an indirection with no use.
    // The difference lies in whether a target register must be allocated.
    // On XARCH we can generate a compare with no target register as long as the address
    // is not contained.
    // On ARM64 we can generate a load to REG_ZR in all cases.
    // However, on ARM we must always generate a load to a register.
    // In the case where we require a target register, it is better to use GT_IND, since
    // GT_NULLCHECK is a non-value node and would therefore require an internal register
    // to use as the target. That is non-optimal because it will be modeled as conflicting
    // with the source register(s).
    // So, to summarize:
    // - On ARM64, always use GT_NULLCHECK for a dead indirection.
    // - On ARM, always use GT_IND.
    // - On XARCH, use GT_IND if we have a contained address, and GT_NULLCHECK otherwise.
    // In all cases, change the type to TYP_INT.
    //
    assert(ind->OperIs(GT_NULLCHECK, GT_IND, GT_BLK, GT_OBJ));

    ind->gtType = TYP_INT;
#ifdef TARGET_ARM64
    bool useNullCheck = true;
#elif TARGET_ARM
    bool useNullCheck = false;
#else  // TARGET_XARCH
    bool useNullCheck = !ind->Addr()->isContained();
#endif // !TARGET_XARCH

    if (useNullCheck && !ind->OperIs(GT_NULLCHECK))
    {
        comp->gtChangeOperToNullCheck(ind, block);
        ind->ClearUnusedValue();
    }
    else if (!useNullCheck && !ind->OperIs(GT_IND))
    {
        ind->ChangeOper(GT_IND);
        ind->SetUnusedValue();
    }
}

void Lowering::LowerStoreObj(GenTreeObj* store)
{
    assert(store->OperIs(GT_STORE_OBJ) && store->TypeIs(TYP_STRUCT));

    if (TryTransformStoreObjToStoreInd(store))
    {
        return;
    }

    StructStoreKind kind = GetStructStoreKind(false, store->GetLayout(), store->GetValue());
    store->SetKind(kind);
    LowerStructStore(store, kind, store->GetLayout());
}

void Lowering::LowerStructStore(GenTree* store, StructStoreKind kind, ClassLayout* layout)
{
    assert(store->OperIs(GT_STORE_OBJ, GT_STORE_LCL_VAR, GT_STORE_LCL_FLD) && store->TypeIs(TYP_STRUCT));
    assert(!layout->IsBlockLayout());

    GenTree* dstAddr = nullptr;
    GenTree* src;

    if (!store->OperIs(GT_STORE_OBJ))
    {
        src = store->AsLclVarCommon()->GetOp(0);
    }
    else
    {
        dstAddr = store->AsObj()->GetAddr();
        src     = store->AsObj()->GetValue();

        assert(dstAddr->TypeIs(TYP_BYREF, TYP_I_IMPL));

#ifdef TARGET_XARCH
        TryCreateAddrMode(dstAddr, false);
#endif

        if ((kind == StructStoreKind::UnrollInit) || (kind == StructStoreKind::UnrollCopy))
        {
            ContainBlockStoreAddress(store, layout->GetSize(), dstAddr);
        }
    }

    assert((src->OperIs(GT_OBJ, GT_LCL_VAR, GT_LCL_FLD) && src->TypeIs(TYP_STRUCT)) || src->IsIntegralConst(0));
    assert(!src->OperIs(GT_OBJ) || !src->AsObj()->GetAddr()->isContained());

    if (src->TypeIs(TYP_STRUCT))
    {
        src->SetContained();
    }

    if (kind == StructStoreKind::UnrollInit)
    {
#ifdef TARGET_XARCH
        // If the size is 1 or a multiple of XMM register size there's no need to load
        // 0 in a GPR, codegen will use an immediate 0 or xorps to generate 0 directly
        // in the temporary XMM register.
        if ((layout->GetSize() == 1) || ((layout->GetSize() % XMM_REGSIZE_BYTES) == 0))
        {
            src->SetContained();
        }
#elif defined(TARGET_ARM64)
        // Use REG_ZR as source on ARM64.
        src->SetContained();
#endif
    }
    else if (src->OperIs(GT_OBJ))
    {
        if (kind == StructStoreKind::UnrollCopy)
        {
            ContainBlockStoreAddress(store, layout->GetSize(), src->AsObj()->GetAddr());
        }
#ifdef TARGET_XARCH
        else
        {
            TryCreateAddrMode(src->AsObj()->GetAddr(), false);
        }
#endif
    }
}

void Lowering::LowerStoreBlk(GenTreeBlk* store)
{
    assert(store->OperIs(GT_STORE_BLK) && store->TypeIs(TYP_STRUCT));

    GenTree* dstAddr = store->GetAddr();
    GenTree* src     = store->GetValue();
    unsigned size    = store->Size();

#ifdef TARGET_XARCH
    TryCreateAddrMode(dstAddr, false);
#endif

    if (!src->OperIs(GT_BLK))
    {
        assert(src->OperIs(GT_INIT_VAL) || src->IsIntegralConst(0));

        if (src->OperIs(GT_INIT_VAL))
        {
            src->SetContained();
            src = src->AsUnOp()->GetOp(0);
        }

        if (size > INITBLK_UNROLL_LIMIT)
        {
            store->SetKind(StructStoreKind::LargeInit);
        }
        else if (!src->OperIs(GT_CNS_INT))
        {
#ifdef TARGET_XARCH
            // TODO-CQ: We could unroll even when the initialization value is not a constant
            // by inserting a MUL init, 0x01010101 instruction. We need to determine if the
            // extra latency that MUL introduces isn't worse that rep stosb. Likely not.

            // TODO-MIKE-Review: Why does x64 uses RepStos instead of MemSet like in the
            // constant case? RepStos/MemSet selection should depend only on size.
            store->SetKind(StructStoreKind::RepStos);
#else
            store->SetKind(StructStoreKind::MemSet);
#endif
        }
        else
        {
            store->SetKind(StructStoreKind::UnrollInit);

            // The fill value of an initblk is interpreted to hold a
            // value of (unsigned int8) however a constant of any size
            // may practically reside on the evaluation stack. So extract
            // the lower byte out of the initVal constant and replicate
            // it to a larger constant whose size is sufficient to support
            // the largest width store of the desired inline expansion.

            ssize_t fill = src->AsIntCon()->GetUInt8Value();

            if (fill == 0)
            {
#ifdef TARGET_XARCH
                // If the size is multiple of XMM register size there's no need to load 0 in a GPR,
                // codegen will use xorps to generate 0 directly in the temporary XMM register.
                if ((size % XMM_REGSIZE_BYTES) == 0)
                {
                    src->SetContained();
                }
#elif defined(TARGET_ARM64)
                // Use REG_ZR as source on ARM64.
                src->SetContained();
#endif
            }
#ifdef TARGET_64BIT
            else if (size >= 4)
            {
                fill *= 0x0101010101010101LL;
                src->SetType(TYP_LONG);
            }
#endif
            else
            {
                fill *= 0x01010101;
            }

            src->AsIntCon()->SetValue(fill);

            ContainBlockStoreAddress(store, size, dstAddr);
        }
    }
    else
    {
        assert(src->OperIs(GT_BLK) && src->TypeIs(TYP_STRUCT));
        assert(!src->AsBlk()->GetAddr()->isContained());

        src->SetContained();

        if (size > CPBLK_UNROLL_LIMIT)
        {
            store->SetKind(StructStoreKind::LargeCopy);

#ifdef TARGET_XARCH
            if (src->OperIs(GT_BLK))
            {
                TryCreateAddrMode(src->AsBlk()->GetAddr(), false);
            }
#endif
        }
        else
        {
            store->SetKind(StructStoreKind::UnrollCopy);

            if (src->OperIs(GT_BLK))
            {
                ContainBlockStoreAddress(store, size, src->AsBlk()->GetAddr());
            }

            ContainBlockStoreAddress(store, size, dstAddr);
        }
    }
}

void Lowering::LowerStoreDynBlk(GenTreeDynBlk* store)
{
    assert(store->OperIs(GT_STORE_DYN_BLK));

#ifdef TARGET_XARCH
    TryCreateAddrMode(store->GetAddr(), false);
#endif

    GenTree* src = store->GetValue();

    if (src->OperIs(GT_INIT_VAL, GT_CNS_INT))
    {
        if (src->OperIs(GT_INIT_VAL))
        {
            src->SetContained();
        }

#ifdef TARGET_X86
        // TODO-X86-CQ: Investigate whether a helper call would be beneficial on x86
        store->SetKind(StructStoreKind::RepStos);
#else
        store->SetKind(StructStoreKind::MemSet);
#endif
    }
    else
    {
        assert(src->OperIs(GT_IND) && src->TypeIs(TYP_STRUCT));
        assert(!src->AsIndir()->GetAddr()->isContained());

        src->SetContained();

#ifdef TARGET_XARCH
        TryCreateAddrMode(src->AsIndir()->GetAddr(), false);
#endif

#ifdef TARGET_X86
        // TODO-X86-CQ: Investigate whether a helper call would be beneficial on x86
        store->SetKind(StructStoreKind::RepMovs);
#else
        store->SetKind(StructStoreKind::MemCpy);
#endif
    }
}

bool Lowering::TryTransformStoreObjToStoreInd(GenTreeObj* store)
{
    assert(store->OperIs(GT_STORE_OBJ));

    // TODO-MIKE-Fix: This is backwards...
    if (comp->opts.OptimizationEnabled())
    {
        return false;
    }

    var_types regType = store->GetLayout()->GetRegisterType();

    if (regType == TYP_UNDEF)
    {
        return false;
    }

    if (varTypeIsSIMD(regType))
    {
        // TODO-CQ: support STORE_IND SIMD16(SIMD16, CNT_INT 0).
        return false;
    }

    if (varTypeIsGC(regType))
    {
        // TODO-CQ: STOREIND does not try to contain src if we need a barrier,
        // STORE_OBJ generates better code currently.
        return false;
    }

    GenTree* src = store->GetValue();

    if (varTypeIsSmall(regType) && !src->IsIntegralConst(0))
    {
        // source operand INDIR will use a widening instruction
        // and generate worse code, like `movzx` instead of `mov`
        // on x64.
        return false;
    }

    JITDUMP("Replacing STORE_OBJ with STOREIND for [06%u]", store->gtTreeID);
    store->ChangeOper(GT_STOREIND);
    store->ChangeType(regType);

    if (varTypeIsStruct(src->GetType()))
    {
        if (src->OperIs(GT_OBJ))
        {
            src->ChangeOper(GT_IND);
        }

        src->SetType(regType);
        LowerNode(src);
    }
    else
    {
        assert(src->IsIntegralConst(0));

        src->SetType(varActualType(regType));
    }

    LowerStoreIndirCommon(store->AsStoreInd());

    return true;
}

#ifdef FEATURE_SIMD
bool Lowering::ContainSIMD12MemToMemCopy(GenTree* store, GenTree* value)
{
    assert(IsContainableMemoryOp(store));
    assert(store->TypeIs(TYP_SIMD12));

    if ((varTypeSize(value->GetType()) < 12) || !IsContainableMemoryOp(value) || !IsSafeToContainMem(store, value))
    {
        return false;
    }

    value->SetContained();

    if (value->OperIs(GT_IND))
    {
        GenTree* addr = value->AsIndir()->GetAddr();

        if (addr->isContained() && (addr->IsClsVar() || addr->IsIntCon() || !IsSafeToContainMem(store, addr)))
        {
            addr->ClearContained();
        }
    }

    return true;
}
#endif

#ifdef FEATURE_HW_INTRINSICS
unsigned Lowering::GetSimdMemoryTemp(var_types type)
{
#if defined(TARGET_XARCH)
    assert((type == TYP_SIMD16) || (type == TYP_SIMD32));
    unsigned& tempLclNum = type == TYP_SIMD32 ? m_simd32MemoryTemp : m_simd16MemoryTemp;
#elif defined(TARGET_ARM64)
    assert((type == TYP_SIMD16) || (type == TYP_SIMD8));
    unsigned& tempLclNum = type == TYP_SIMD8 ? m_simd8MemoryTemp : m_simd16MemoryTemp;
#endif

    if (tempLclNum == BAD_VAR_NUM)
    {
        tempLclNum = comp->lvaGrabTempWithImplicitUse(false DEBUGARG("Vector GetElement temp"));

        // TODO-MIKE-Cleanup: This creates a SIMD local without using lvaSetStruct
        // so it doesn't set layout, exact size etc. It happens to work because it
        // is done late, after lowering, otherwise at least the lack of exact size
        // would cause problems.
        // And we don't have where to get a class handle to call lvaSetStruct...
        // Could also be a TYP_BLK local, codegen only needs a memory location where
        // to store a SIMD register in order to extract an element from it. But if
        // it's TYP_BLK then it won't have SIMD alignment. Bleah.

        comp->lvaGetDesc(tempLclNum)->lvType = type;
        comp->lvaSetVarDoNotEnregister(tempLclNum DEBUGARG(Compiler::DNER_LocalField));
    }

    return tempLclNum;
}

GenTree* Lowering::TryRemoveCastIfPresent(var_types expectedType, GenTree* op)
{
    if (!op->IsCast() || op->gtOverflow() || !varTypeIsIntegral(expectedType))
    {
        return op;
    }

    GenTree* castOp = op->AsCast()->GetOp(0);

    if (!varTypeIsIntegral(castOp->GetType()))
    {
        return op;
    }

    if (varTypeSize(op->AsCast()->GetCastType()) > varTypeSize(varActualType(castOp->GetType())))
    {
        return op;
    }

    if (varTypeSize(op->AsCast()->GetCastType()) < varTypeSize(expectedType))
    {
        return op;
    }

    BlockRange().Remove(op);
    castOp->ClearContained();
    return castOp;
}

bool Lowering::VectorConstant::AllBitsZero(unsigned vectorByteSize) const
{
    for (unsigned i = 0; i < vectorByteSize; i++)
    {
        if (u8[i] != 0)
        {
            return false;
        }
    }
    return true;
}

bool Lowering::VectorConstant::AllBitsOne(unsigned vectorByteSize) const
{
    for (unsigned i = 0; i < vectorByteSize; i++)
    {
        if (u8[i] != 0xFF)
        {
            return false;
        }
    }
    return true;
}

bool Lowering::VectorConstant::Insert(var_types type, int index, GenTree* value)
{
    if (GenTreeIntCon* icon = value->IsIntCon())
    {
        switch (type)
        {
            case TYP_BYTE:
            case TYP_UBYTE:
                u8[index] = value->AsIntCon()->GetUInt8Value();
                return true;
            case TYP_SHORT:
            case TYP_USHORT:
                u16[index] = value->AsIntCon()->GetUInt16Value();
                return true;
            case TYP_INT:
            case TYP_UINT:
                u32[index] = value->AsIntCon()->GetUInt32Value();
                return true;
#ifdef TARGET_64BIT
            case TYP_LONG:
            case TYP_ULONG:
                u64[index] = value->AsIntCon()->GetUInt64Value();
                return true;
#endif
            default:
                return false;
        }
    }

    if (GenTreeDblCon* dcon = value->IsDblCon())
    {
        if (type == TYP_FLOAT)
        {
            u32[index] = value->AsDblCon()->GetFloatBits();
        }
        else
        {
            u64[index] = value->AsDblCon()->GetDoubleBits();
        }

        return true;
    }

#ifndef TARGET_64BIT
    if (value->OperIs(GT_LONG) && value->AsOp()->GetOp(0)->IsIntCon() && value->AsOp()->GetOp(1)->IsIntCon())
    {
        uint64_t loBits = value->AsOp()->GetOp(0)->AsIntCon()->GetUInt32Value();
        uint64_t hiBits = value->AsOp()->GetOp(1)->AsIntCon()->GetUInt32Value();
        u64[index]      = (hiBits << 32) | loBits;
        return true;
    }
#endif

    return false;
}

bool Lowering::VectorConstant::Create(GenTreeHWIntrinsic* create)
{
    unsigned  numOps  = create->GetNumOps();
    var_types eltType = create->GetSimdBaseType();

    for (unsigned i = 0; i < numOps; i++)
    {
        if (!Insert(eltType, i, create->GetOp(i)))
        {
            return false;
        }
    }

    return true;
}

bool Lowering::VectorConstant::Broadcast(GenTreeHWIntrinsic* create)
{
    var_types eltType = create->GetSimdBaseType();
    GenTree*  op1     = create->GetOp(0);

    if (!Insert(eltType, 0, op1))
    {
        return false;
    }

    unsigned eltCount = create->GetSimdSize() / varTypeSize(eltType);
    unsigned eltSize  = varTypeSize(eltType);

    for (unsigned i = 1; i < eltCount; i++)
    {
        switch (eltSize)
        {
            case 1:
                u8[i] = u8[0];
                break;
            case 2:
                u16[i] = u16[0];
                break;
            case 4:
                u32[i] = u32[0];
                break;
            default:
                assert(eltSize == 8);
                u64[i] = u64[0];
                break;
        }
    }

    return true;
}
#endif
