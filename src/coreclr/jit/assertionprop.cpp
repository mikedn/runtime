// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XX                                                                           XX
XX                          AssertionProp                                    XX
XX                                                                           XX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*/

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

/*****************************************************************************
 *
 *  Helper passed to Compiler::fgWalkTreePre() to find the Asgn node for optAddCopies()
 */

/* static */
Compiler::fgWalkResult Compiler::optAddCopiesCallback(GenTree** pTree, fgWalkData* data)
{
    GenTree* tree = *pTree;

    if (tree->OperIs(GT_ASG))
    {
        GenTree*  op1  = tree->AsOp()->gtOp1;
        Compiler* comp = data->compiler;

        if ((op1->gtOper == GT_LCL_VAR) && (op1->AsLclVarCommon()->GetLclNum() == comp->optAddCopyLclNum))
        {
            comp->optAddCopyAsgnNode = tree;
            return WALK_ABORT;
        }
    }
    return WALK_CONTINUE;
}

/*****************************************************************************
 *
 *  Add new copies before Assertion Prop.
 */

void Compiler::optAddCopies()
{
    // TODO-MIKE-Review: Does this thing actually work? Copies are added but
    // they don't appear to be used. The idea was probably that copy prop will
    // pick up these copies but that doesn't seem to happen.

    unsigned   lclNum;
    LclVarDsc* varDsc;

#ifdef DEBUG
    if (verbose)
    {
        printf("\n*************** In optAddCopies()\n\n");
    }
    if (verboseTrees)
    {
        printf("Blocks/Trees at start of phase\n");
        fgDispBasicBlocks(true);
    }
#endif

    // Don't add any copies if we have reached the tracking limit.
    if (lvaHaveManyLocals())
    {
        return;
    }

    for (lclNum = 0, varDsc = lvaTable; lclNum < lvaCount; lclNum++, varDsc++)
    {
        var_types typ = varDsc->TypeGet();

        // We only add copies for non temp local variables
        // that have a single def and that can possibly be enregistered

        if (varDsc->lvIsTemp || !varDsc->lvSingleDef || !varTypeIsEnregisterable(typ))
        {
            continue;
        }

        /* For lvNormalizeOnLoad(), we need to add a cast to the copy-assignment
           like "copyLclNum = int(varDsc)" and optAssertionGen() only
           tracks simple assignments. The same goes for lvNormalizedOnStore as
           the cast is generated in fgMorphSmpOpAsg. This boils down to not having
           a copy until optAssertionGen handles this*/
        if (varDsc->lvNormalizeOnLoad() || varDsc->lvNormalizeOnStore())
        {
            continue;
        }

        if (varTypeIsSmall(varDsc->TypeGet()) || typ == TYP_BOOL)
        {
            continue;
        }

        // If locals must be initialized to zero, that initialization counts as a second definition.
        // VB in particular allows usage of variables not explicitly initialized.
        // Note that this effectively disables this optimization for all local variables
        // as C# sets InitLocals all the time starting in Whidbey.

        if (!varDsc->lvIsParam && info.compInitMem)
        {
            continue;
        }

        // On x86 we may want to add a copy for an incoming double parameter
        // because we can ensure that the copy we make is double aligned
        // where as we can never ensure the alignment of an incoming double parameter
        //
        // On all other platforms we will never need to make a copy
        // for an incoming double parameter

        bool isFloatParam = false;

#ifdef TARGET_X86
        isFloatParam = varDsc->lvIsParam && varTypeIsFloating(typ);
#endif

        if (!isFloatParam && !varDsc->lvVolatileHint)
        {
            continue;
        }

        // We don't want to add a copy for a variable that is part of a struct
        if (varDsc->lvIsStructField)
        {
            continue;
        }

        // We require that the weighted ref count be significant.
        if (varDsc->lvRefCntWtd() <= (BB_LOOP_WEIGHT_SCALE * BB_UNITY_WEIGHT / 2))
        {
            continue;
        }

        // For parameters, we only want to add a copy for the heavier-than-average
        // uses instead of adding a copy to cover every single use.
        // 'paramImportantUseDom' is the set of blocks that dominate the
        // heavier-than-average uses of a parameter.
        // Initial value is all blocks.

        BlockSet paramImportantUseDom(BlockSetOps::MakeFull(this));

        // This will be threshold for determining heavier-than-average uses
        BasicBlock::weight_t paramAvgWtdRefDiv2 =
            (varDsc->lvRefCntWtd() + varDsc->lvRefCnt() / 2) / (varDsc->lvRefCnt() * 2);

        bool paramFoundImportantUse = false;

#ifdef DEBUG
        if (verbose)
        {
            printf("Trying to add a copy for V%02u %s, avg_wtd = %s\n", lclNum,
                   varDsc->lvIsParam ? "an arg" : "a local", refCntWtd2str(paramAvgWtdRefDiv2));
        }
#endif

        //
        // We must have a ref in a block that is dominated only by the entry block
        //

        if (BlockSetOps::MayBeUninit(varDsc->lvRefBlks))
        {
            // No references
            continue;
        }

        bool isDominatedByFirstBB = false;

        BlockSetOps::Iter iter(this, varDsc->lvRefBlks);
        unsigned          bbNum = 0;
        while (iter.NextElem(&bbNum))
        {
            /* Find the block 'bbNum' */
            BasicBlock* block = fgFirstBB;
            while (block && (block->bbNum != bbNum))
            {
                block = block->bbNext;
            }
            noway_assert(block && (block->bbNum == bbNum));

            bool     importantUseInBlock = (varDsc->lvIsParam) && (block->getBBWeight(this) > paramAvgWtdRefDiv2);
            bool     isPreHeaderBlock    = ((block->bbFlags & BBF_LOOP_PREHEADER) != 0);
            BlockSet blockDom(BlockSetOps::UninitVal());
            BlockSet blockDomSub0(BlockSetOps::UninitVal());

            if (block->bbIDom == nullptr && isPreHeaderBlock)
            {
                // Loop Preheader blocks that we insert will have a bbDom set that is nullptr
                // but we can instead use the bNext successor block's dominator information
                noway_assert(block->bbNext != nullptr);
                BlockSetOps::AssignNoCopy(this, blockDom, fgGetDominatorSet(block->bbNext));
            }
            else
            {
                BlockSetOps::AssignNoCopy(this, blockDom, fgGetDominatorSet(block));
            }

            if (!BlockSetOps::IsEmpty(this, blockDom))
            {
                BlockSetOps::Assign(this, blockDomSub0, blockDom);
                if (isPreHeaderBlock)
                {
                    // We must clear bbNext block number from the dominator set
                    BlockSetOps::RemoveElemD(this, blockDomSub0, block->bbNext->bbNum);
                }
                /* Is this block dominated by fgFirstBB? */
                if (BlockSetOps::IsMember(this, blockDomSub0, fgFirstBB->bbNum))
                {
                    isDominatedByFirstBB = true;
                }
            }

#ifdef DEBUG
            if (verbose)
            {
                printf("        Referenced in " FMT_BB ", bbWeight is %s", bbNum,
                       refCntWtd2str(block->getBBWeight(this)));

                if (isDominatedByFirstBB)
                {
                    printf(", which is dominated by BB01");
                }

                if (importantUseInBlock)
                {
                    printf(", ImportantUse");
                }

                printf("\n");
            }
#endif

            /* If this is a heavier-than-average block, then track which
               blocks dominate this use of the parameter. */
            if (importantUseInBlock)
            {
                paramFoundImportantUse = true;
                BlockSetOps::IntersectionD(this, paramImportantUseDom,
                                           blockDomSub0); // Clear blocks that do not dominate
            }
        }

        // We should have found at least one heavier-than-averageDiv2 block.
        if (varDsc->lvIsParam)
        {
            if (!paramFoundImportantUse)
            {
                continue;
            }
        }

        // For us to add a new copy:
        // we require that we have a floating point parameter
        // or a lvVolatile variable that is always reached from the first BB
        // and we have at least one block available in paramImportantUseDom
        //
        bool doCopy = (isFloatParam || (isDominatedByFirstBB && varDsc->lvVolatileHint)) &&
                      !BlockSetOps::IsEmpty(this, paramImportantUseDom);

        // Under stress mode we expand the number of candidates
        // to include parameters of any type
        // or any variable that is always reached from the first BB
        //
        if (compStressCompile(STRESS_GENERIC_VARN, 30))
        {
            // Ensure that we preserve the invariants required by the subsequent code.
            if (varDsc->lvIsParam || isDominatedByFirstBB)
            {
                doCopy = true;
            }
        }

        if (!doCopy)
        {
            continue;
        }

        unsigned copyLclNum = lvaGrabTemp(false DEBUGARG("optAddCopies"));
        // Because lvaGrabTemp may have reallocated the lvaTable, ensure varDsc
        // is still in sync with lvaTable[lclNum];
        varDsc = lvaGetDesc(lclNum);

        if (varTypeIsSIMD(varDsc->GetType()))
        {
            lvaSetStruct(copyLclNum, varDsc->GetLayout(), /* checkUnsafeBuffer */ false);
            assert(lvaGetDesc(copyLclNum)->GetType() == typ);
        }
        else
        {
            lvaGetDesc(copyLclNum)->SetType(typ);
        }

        JITDUMP("Finding the best place to insert the assignment V%02i = V%02i\n", copyLclNum, lclNum);

        Statement* stmt;

        if (varDsc->lvIsParam)
        {
            noway_assert((varDsc->lvDefStmt == nullptr) || varDsc->lvIsStructField);

            // Create a new copy assignment tree
            GenTree* copyAsgn = gtNewAssignNode(gtNewLclvNode(copyLclNum, typ), gtNewLclvNode(lclNum, typ));

            /* Find the best block to insert the new assignment     */
            /* We will choose the lowest weighted block, and within */
            /* those block, the highest numbered block which        */
            /* dominates all the uses of the local variable         */

            /* Our default is to use the first block */
            BasicBlock*          bestBlock  = fgFirstBB;
            BasicBlock::weight_t bestWeight = bestBlock->getBBWeight(this);
            BasicBlock*          block      = bestBlock;

#ifdef DEBUG
            if (verbose)
            {
                printf("        Starting at " FMT_BB ", bbWeight is %s", block->bbNum,
                       refCntWtd2str(block->getBBWeight(this)));

                printf(", bestWeight is %s\n", refCntWtd2str(bestWeight));
            }
#endif

            /* We have already calculated paramImportantUseDom above. */
            BlockSetOps::Iter iter(this, paramImportantUseDom);
            unsigned          bbNum = 0;
            while (iter.NextElem(&bbNum))
            {
                /* Advance block to point to 'bbNum' */
                /* This assumes that the iterator returns block number is increasing lexical order. */
                while (block && (block->bbNum != bbNum))
                {
                    block = block->bbNext;
                }
                noway_assert(block && (block->bbNum == bbNum));

#ifdef DEBUG
                if (verbose)
                {
                    printf("        Considering " FMT_BB ", bbWeight is %s", block->bbNum,
                           refCntWtd2str(block->getBBWeight(this)));

                    printf(", bestWeight is %s\n", refCntWtd2str(bestWeight));
                }
#endif

                // Does this block have a smaller bbWeight value?
                if (block->getBBWeight(this) > bestWeight)
                {
#ifdef DEBUG
                    if (verbose)
                    {
                        printf("bbWeight too high\n");
                    }
#endif
                    continue;
                }

                // Don't use blocks that are exception handlers because
                // inserting a new first statement will interface with
                // the CATCHARG

                if (handlerGetsXcptnObj(block->bbCatchTyp))
                {
#ifdef DEBUG
                    if (verbose)
                    {
                        printf("Catch block\n");
                    }
#endif
                    continue;
                }

                // Don't use the BBJ_ALWAYS block marked with BBF_KEEP_BBJ_ALWAYS. These
                // are used by EH code. The JIT can not generate code for such a block.

                if (block->bbFlags & BBF_KEEP_BBJ_ALWAYS)
                {
#if defined(FEATURE_EH_FUNCLETS)
                    // With funclets, this is only used for BBJ_CALLFINALLY/BBJ_ALWAYS pairs. For x86, it is also used
                    // as the "final step" block for leaving finallys.
                    assert(block->isBBCallAlwaysPairTail());
#endif // FEATURE_EH_FUNCLETS
#ifdef DEBUG
                    if (verbose)
                    {
                        printf("Internal EH BBJ_ALWAYS block\n");
                    }
#endif
                    continue;
                }

                // This block will be the new candidate for the insert point
                // for the new assignment
                CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef DEBUG
                if (verbose)
                {
                    printf("new bestBlock\n");
                }
#endif

                bestBlock  = block;
                bestWeight = block->getBBWeight(this);
            }

            // If there is a use of the variable in this block
            // then we insert the assignment at the beginning
            // otherwise we insert the statement at the end
            CLANG_FORMAT_COMMENT_ANCHOR;

#ifdef DEBUG
            if (verbose)
            {
                printf("        Insert copy at the %s of " FMT_BB "\n",
                       (BlockSetOps::IsEmpty(this, paramImportantUseDom) ||
                        BlockSetOps::IsMember(this, varDsc->lvRefBlks, bestBlock->bbNum))
                           ? "start"
                           : "end",
                       bestBlock->bbNum);
            }
#endif

            if (BlockSetOps::IsEmpty(this, paramImportantUseDom) ||
                BlockSetOps::IsMember(this, varDsc->lvRefBlks, bestBlock->bbNum))
            {
                stmt = fgNewStmtAtBeg(bestBlock, copyAsgn);
            }
            else
            {
                stmt = fgNewStmtNearEnd(bestBlock, copyAsgn);
            }
        }
        else
        {
            noway_assert(varDsc->lvDefStmt != nullptr);

            /* Locate the assignment to varDsc in the lvDefStmt */
            stmt = varDsc->lvDefStmt;

            optAddCopyLclNum   = lclNum;  // in
            optAddCopyAsgnNode = nullptr; // out

            fgWalkTreePre(stmt->GetRootNodePointer(), Compiler::optAddCopiesCallback, (void*)this, false);

            noway_assert(optAddCopyAsgnNode);

            GenTree* tree = optAddCopyAsgnNode;
            GenTree* op1  = tree->AsOp()->gtOp1;

            noway_assert(tree && op1 && tree->OperIs(GT_ASG) && (op1->gtOper == GT_LCL_VAR) &&
                         (op1->AsLclVarCommon()->GetLclNum() == lclNum));

            /* Assign the old expression into the new temp */

            GenTree* newAsgn = gtNewAssignNode(gtNewLclvNode(copyLclNum, typ), tree->AsOp()->gtOp2);

            /* Copy the new temp to op1 */

            GenTree* copyAsgn = gtNewAssignNode(op1, gtNewLclvNode(copyLclNum, typ));

            /* Change the tree to a GT_COMMA with the two assignments as child nodes */

            tree->ChangeOper(GT_COMMA);
            tree->AsOp()->SetOp(0, newAsgn);
            tree->AsOp()->SetOp(1, copyAsgn);
            tree->SetType(TYP_VOID);
            tree->SetSideEffects(newAsgn->GetSideEffects() | copyAsgn->GetSideEffects());
            tree->gtFlags &= ~GTF_REVERSE_OPS;
        }

        JITDUMPTREE(stmt->GetRootNode(), "\nIntroduced a copy for V%02u\n", lclNum);
    }
}

//------------------------------------------------------------------------------
// GetAssertionDep: Retrieve the assertions on this local variable
//
// Arguments:
//    lclNum - The local var id.
//
// Return Value:
//    The dependent assertions (assertions using the value of the local var)
//    of the local var.
//

ASSERT_TP& Compiler::GetAssertionDep(unsigned lclNum)
{
    assert(lclNum < lvaCount);

    JitExpandArray<ASSERT_TP>& dep = *optAssertionDep;
    if (dep[lclNum] == nullptr)
    {
        dep[lclNum] = BitVecOps::MakeEmpty(apTraits);
    }
    return dep[lclNum];
}

/*****************************************************************************
 *
 *  Initialize the assertion prop bitset traits and the default bitsets.
 */

void Compiler::optAssertionTraitsInit(AssertionIndex assertionCount)
{
    apTraits = new (this, CMK_AssertionProp) BitVecTraits(assertionCount, this);
    apFull   = BitVecOps::MakeFull(apTraits);
}

/*****************************************************************************
 *
 *  Initialize the assertion prop tracking logic.
 */

void Compiler::optAssertionInit(bool isLocalProp)
{
    // Use a function countFunc to determine a proper maximum assertion count for the
    // method being compiled. The function is linear to the IL size for small and
    // moderate methods. For large methods, considering throughput impact, we track no
    // more than 64 assertions.
    // Note this tracks at most only 256 assertions.
    static const AssertionIndex countFunc[] = {64, 128, 256, 64};
    static const unsigned       lowerBound  = 0;
    static const unsigned       upperBound  = _countof(countFunc) - 1;
    const unsigned              codeSize    = info.compILCodeSize / 512;
    optMaxAssertionCount                    = countFunc[isLocalProp ? lowerBound : min(upperBound, codeSize)];

    optLocalAssertionProp  = isLocalProp;
    optAssertionTabPrivate = new (this, CMK_AssertionProp) AssertionDsc[optMaxAssertionCount];
    optComplementaryAssertionMap =
        new (this, CMK_AssertionProp) AssertionIndex[optMaxAssertionCount + 1](); // zero-inited (NO_ASSERTION_INDEX)
    assert(NO_ASSERTION_INDEX == 0);

    if (!isLocalProp)
    {
        optValueNumToAsserts =
            new (getAllocator(CMK_AssertionProp)) ValueNumToAssertsMap(getAllocator(CMK_AssertionProp));
    }

    if (optAssertionDep == nullptr)
    {
        optAssertionDep =
            new (this, CMK_AssertionProp) JitExpandArray<ASSERT_TP>(getAllocator(CMK_AssertionProp), max(1, lvaCount));
    }

    optAssertionTraitsInit(optMaxAssertionCount);
    optAssertionCount   = 0;
    bbJtrueAssertionOut = nullptr;
}

#ifdef DEBUG
void Compiler::optPrintAssertion(AssertionDsc* curAssertion, AssertionIndex assertionIndex /* =0 */)
{
    if (curAssertion->op1.kind == O1K_EXACT_TYPE)
    {
        printf("Type     ");
    }
    else if (curAssertion->op1.kind == O1K_ARR_BND)
    {
        printf("ArrBnds  ");
    }
    else if (curAssertion->op1.kind == O1K_SUBTYPE)
    {
        printf("Subtype  ");
    }
    else if (curAssertion->op2.kind == O2K_LCLVAR_COPY)
    {
        printf("Copy     ");
    }
    else if ((curAssertion->op2.kind == O2K_CONST_INT) || (curAssertion->op2.kind == O2K_CONST_LONG) ||
             (curAssertion->op2.kind == O2K_CONST_DOUBLE))
    {
        printf("Constant ");
    }
    else if (curAssertion->op2.kind == O2K_SUBRANGE)
    {
        printf("Subrange ");
    }
    else
    {
        printf("?assertion classification? ");
    }
    printf("Assertion: ");
    if (!optLocalAssertionProp)
    {
        printf("(%d, %d) ", curAssertion->op1.vn, curAssertion->op2.vn);
    }

    if (!optLocalAssertionProp)
    {
        printf("(" FMT_VN "," FMT_VN ") ", curAssertion->op1.vn, curAssertion->op2.vn);
    }

    if ((curAssertion->op1.kind == O1K_LCLVAR) || (curAssertion->op1.kind == O1K_EXACT_TYPE) ||
        (curAssertion->op1.kind == O1K_SUBTYPE))
    {
        printf("V%02u", curAssertion->op1.lcl.lclNum);
        if (curAssertion->op1.lcl.ssaNum != SsaConfig::RESERVED_SSA_NUM)
        {
            printf(".%02u", curAssertion->op1.lcl.ssaNum);
        }
    }
    else if (curAssertion->op1.kind == O1K_ARR_BND)
    {
        printf("[idx:");
        vnStore->vnDump(this, curAssertion->op1.bnd.vnIdx);
        printf(";len:");
        vnStore->vnDump(this, curAssertion->op1.bnd.vnLen);
        printf("]");
    }
    else if (curAssertion->op1.kind == O1K_BOUND_OPER_BND)
    {
        printf("Oper_Bnd");
        vnStore->vnDump(this, curAssertion->op1.vn);
    }
    else if (curAssertion->op1.kind == O1K_BOUND_LOOP_BND)
    {
        printf("Loop_Bnd");
        vnStore->vnDump(this, curAssertion->op1.vn);
    }
    else if (curAssertion->op1.kind == O1K_CONSTANT_LOOP_BND)
    {
        printf("Const_Loop_Bnd");
        vnStore->vnDump(this, curAssertion->op1.vn);
    }
    else if (curAssertion->op1.kind == O1K_VALUE_NUMBER)
    {
        printf("Value_Number");
        vnStore->vnDump(this, curAssertion->op1.vn);
    }
    else
    {
        printf("?thisArg.kind?");
    }

    if (curAssertion->assertionKind == OAK_SUBRANGE)
    {
        printf(" in ");
    }
    else if (curAssertion->assertionKind == OAK_EQUAL)
    {
        if (curAssertion->op1.kind == O1K_LCLVAR)
        {
            printf(" == ");
        }
        else
        {
            printf(" is ");
        }
    }
    else if (curAssertion->assertionKind == OAK_NO_THROW)
    {
        printf(" in range ");
    }
    else if (curAssertion->assertionKind == OAK_NOT_EQUAL)
    {
        if (curAssertion->op1.kind == O1K_LCLVAR)
        {
            printf(" != ");
        }
        else
        {
            printf(" is not ");
        }
    }
    else
    {
        printf(" ?assertionKind? ");
    }

    if (curAssertion->op1.kind != O1K_ARR_BND)
    {
        switch (curAssertion->op2.kind)
        {
            case O2K_LCLVAR_COPY:
                printf("V%02u", curAssertion->op2.lcl.lclNum);
                if (curAssertion->op1.lcl.ssaNum != SsaConfig::RESERVED_SSA_NUM)
                {
                    printf(".%02u", curAssertion->op1.lcl.ssaNum);
                }
                break;

            case O2K_CONST_INT:
            case O2K_IND_CNS_INT:
                if (curAssertion->op1.kind == O1K_EXACT_TYPE)
                {
                    printf("Exact Type MT(%08X)", dspPtr(curAssertion->op2.u1.iconVal));
                    assert(curAssertion->op2.u1.iconFlags != GTF_EMPTY);
                }
                else if (curAssertion->op1.kind == O1K_SUBTYPE)
                {
                    printf("MT(%08X)", dspPtr(curAssertion->op2.u1.iconVal));
                    assert(curAssertion->op2.u1.iconFlags != GTF_EMPTY);
                }
                else if (curAssertion->op1.kind == O1K_BOUND_OPER_BND)
                {
                    assert(!optLocalAssertionProp);
                    vnStore->vnDump(this, curAssertion->op2.vn);
                }
                else if (curAssertion->op1.kind == O1K_BOUND_LOOP_BND)
                {
                    assert(!optLocalAssertionProp);
                    vnStore->vnDump(this, curAssertion->op2.vn);
                }
                else if (curAssertion->op1.kind == O1K_CONSTANT_LOOP_BND)
                {
                    assert(!optLocalAssertionProp);
                    vnStore->vnDump(this, curAssertion->op2.vn);
                }
                else
                {
                    var_types op1Type;

                    if (curAssertion->op1.kind == O1K_VALUE_NUMBER)
                    {
                        op1Type = vnStore->TypeOfVN(curAssertion->op1.vn);
                    }
                    else
                    {
                        unsigned lclNum = curAssertion->op1.lcl.lclNum;
                        assert(lclNum < lvaCount);
                        LclVarDsc* varDsc = lvaTable + lclNum;
                        op1Type           = varDsc->lvType;
                    }

                    if (op1Type == TYP_REF)
                    {
                        assert(curAssertion->op2.u1.iconVal == 0);
                        printf("null");
                    }
                    else
                    {
                        if ((curAssertion->op2.u1.iconFlags & GTF_ICON_HDL_MASK) != 0)
                        {
                            printf("[%08p]", dspPtr(curAssertion->op2.u1.iconVal));
                        }
                        else
                        {
                            printf("%d", curAssertion->op2.u1.iconVal);
                        }
                    }
                }
                break;

            case O2K_CONST_LONG:
                printf("0x%016llx", curAssertion->op2.lconVal);
                break;

            case O2K_CONST_DOUBLE:
                if (*((__int64*)&curAssertion->op2.dconVal) == (__int64)I64(0x8000000000000000))
                {
                    printf("-0.00000");
                }
                else
                {
                    printf("%#lg", curAssertion->op2.dconVal);
                }
                break;

            case O2K_SUBRANGE:
                printf("[%d..%d]", curAssertion->op2.u2.loBound, curAssertion->op2.u2.hiBound);
                break;

            default:
                printf("?op2.kind?");
                break;
        }
    }

    if (assertionIndex > 0)
    {
        printf(" index=#%02u, mask=", assertionIndex);
        printf("%s", BitVecOps::ToString(apTraits, BitVecOps::MakeSingleton(apTraits, assertionIndex - 1)));
    }
    printf("\n");
}
#endif // DEBUG

/******************************************************************************
 *
 * Helper to retrieve the "assertIndex" assertion. Note that assertIndex 0
 * is NO_ASSERTION_INDEX and "optAssertionCount" is the last valid index.
 *
 */
Compiler::AssertionDsc* Compiler::optGetAssertion(AssertionIndex assertIndex)
{
    assert(NO_ASSERTION_INDEX == 0);
    assert(assertIndex != NO_ASSERTION_INDEX);
    assert(assertIndex <= optAssertionCount);
    AssertionDsc* assertion = &optAssertionTabPrivate[assertIndex - 1];
#ifdef DEBUG
    optDebugCheckAssertion(assertion);
#endif

    return assertion;
}

//------------------------------------------------------------------------
// optCreateAssertion: Create an (op1 assertionKind op2) assertion.
//
// Arguments:
//    op1 - the first assertion operand
//    op2 - the second assertion operand
//    assertionKind - the assertion kind
//    helperCallArgs - when true this indicates that the assertion operands
//                     are the arguments of a type cast helper call such as
//                     CORINFO_HELP_ISINSTANCEOFCLASS
// Return Value:
//    The new assertion index or NO_ASSERTION_INDEX if a new assertion
//    was not created.
//
// Notes:
//    Assertion creation may fail either because the provided assertion
//    operands aren't supported or because the assertion table is full.
//
AssertionIndex Compiler::optCreateAssertion(GenTree*         op1,
                                            GenTree*         op2,
                                            optAssertionKind assertionKind,
                                            bool             helperCallArgs)
{
    assert(op1 != nullptr);
    assert(!helperCallArgs || (op2 != nullptr));

    AssertionDsc assertion;
    memset(&assertion, 0, sizeof(AssertionDsc));
    assert(assertion.assertionKind == OAK_INVALID);

    var_types toType;

    if (op1->gtOper == GT_ARR_BOUNDS_CHECK)
    {
        if (assertionKind == OAK_NO_THROW)
        {
            GenTreeBoundsChk* arrBndsChk = op1->AsBoundsChk();
            assertion.assertionKind      = assertionKind;
            assertion.op1.kind           = O1K_ARR_BND;
            assertion.op1.bnd.vnIdx      = vnStore->VNConservativeNormalValue(arrBndsChk->gtIndex->gtVNPair);
            assertion.op1.bnd.vnLen      = vnStore->VNConservativeNormalValue(arrBndsChk->gtArrLen->gtVNPair);
            goto DONE_ASSERTION;
        }
    }

    //
    // Are we trying to make a non-null assertion?
    //
    if (op2 == nullptr)
    {
        //
        // Must be an OAK_NOT_EQUAL assertion
        //
        noway_assert(assertionKind == OAK_NOT_EQUAL);

        //
        // Set op1 to the instance pointer of the indirection
        //

        ssize_t offset = 0;
        while ((op1->gtOper == GT_ADD) && (op1->gtType == TYP_BYREF))
        {
            if (op1->gtGetOp2()->IsCnsIntOrI())
            {
                offset += op1->gtGetOp2()->AsIntCon()->gtIconVal;
                op1 = op1->gtGetOp1();
            }
            else if (op1->gtGetOp1()->IsCnsIntOrI())
            {
                offset += op1->gtGetOp1()->AsIntCon()->gtIconVal;
                op1 = op1->gtGetOp2();
            }
            else
            {
                break;
            }
        }

        if (fgIsBigOffset(offset) || op1->gtOper != GT_LCL_VAR)
        {
            goto DONE_ASSERTION; // Don't make an assertion
        }

        unsigned lclNum = op1->AsLclVarCommon()->GetLclNum();
        noway_assert(lclNum < lvaCount);
        LclVarDsc* lclVar = &lvaTable[lclNum];

        ValueNum vn;

        //
        // We only perform null-checks on GC refs
        // so only make non-null assertions about GC refs or byrefs if we can't determine
        // the corresponding ref.
        //
        if (lclVar->TypeGet() != TYP_REF)
        {
            if (optLocalAssertionProp || (lclVar->TypeGet() != TYP_BYREF))
            {
                goto DONE_ASSERTION; // Don't make an assertion
            }

            vn = vnStore->VNConservativeNormalValue(op1->gtVNPair);
            VNFuncApp funcAttr;

            // Try to get value number corresponding to the GC ref of the indirection
            while (vnStore->GetVNFunc(vn, &funcAttr) && (funcAttr.m_func == (VNFunc)GT_ADD) &&
                   (vnStore->TypeOfVN(vn) == TYP_BYREF))
            {
                if (vnStore->IsVNConstant(funcAttr.m_args[1]) &&
                    varTypeIsIntegral(vnStore->TypeOfVN(funcAttr.m_args[1])))
                {
                    offset += vnStore->CoercedConstantValue<ssize_t>(funcAttr.m_args[1]);
                    vn = funcAttr.m_args[0];
                }
                else if (vnStore->IsVNConstant(funcAttr.m_args[0]) &&
                         varTypeIsIntegral(vnStore->TypeOfVN(funcAttr.m_args[0])))
                {
                    offset += vnStore->CoercedConstantValue<ssize_t>(funcAttr.m_args[0]);
                    vn = funcAttr.m_args[1];
                }
                else
                {
                    break;
                }
            }

            if (fgIsBigOffset(offset))
            {
                goto DONE_ASSERTION; // Don't make an assertion
            }

            assertion.op1.kind = O1K_VALUE_NUMBER;
        }
        else
        {
            //  If the local variable has its address exposed then bail
            if (lclVar->lvAddrExposed)
            {
                goto DONE_ASSERTION; // Don't make an assertion
            }

            assertion.op1.kind       = O1K_LCLVAR;
            assertion.op1.lcl.lclNum = lclNum;
            assertion.op1.lcl.ssaNum = op1->AsLclVarCommon()->GetSsaNum();
            vn                       = vnStore->VNConservativeNormalValue(op1->gtVNPair);
        }

        assertion.op1.vn           = vn;
        assertion.assertionKind    = assertionKind;
        assertion.op2.kind         = O2K_CONST_INT;
        assertion.op2.vn           = ValueNumStore::VNForNull();
        assertion.op2.u1.iconVal   = 0;
        assertion.op2.u1.iconFlags = GTF_EMPTY;
#ifdef TARGET_64BIT
        assertion.op2.u1.iconFlags |= GTF_ASSERTION_PROP_LONG; // Signify that this is really TYP_LONG
#endif
    }
    //
    // Are we making an assertion about a local variable?
    //
    else if (op1->gtOper == GT_LCL_VAR)
    {
        unsigned lclNum = op1->AsLclVarCommon()->GetLclNum();
        noway_assert(lclNum < lvaCount);
        LclVarDsc* lclVar = &lvaTable[lclNum];

        //  If the local variable has its address exposed then bail
        if (lclVar->lvAddrExposed)
        {
            goto DONE_ASSERTION; // Don't make an assertion
        }

        if (helperCallArgs)
        {
            //
            // Must either be an OAK_EQUAL or an OAK_NOT_EQUAL assertion
            //
            if ((assertionKind != OAK_EQUAL) && (assertionKind != OAK_NOT_EQUAL))
            {
                goto DONE_ASSERTION; // Don't make an assertion
            }

            if (op2->gtOper == GT_IND)
            {
                op2                = op2->AsOp()->gtOp1;
                assertion.op2.kind = O2K_IND_CNS_INT;
            }
            else
            {
                assertion.op2.kind = O2K_CONST_INT;
            }

            if (op2->gtOper != GT_CNS_INT)
            {
                goto DONE_ASSERTION; // Don't make an assertion
            }

            //
            // TODO-CQ: Check for Sealed class and change kind to O1K_EXACT_TYPE
            //          And consider the special cases, like CORINFO_FLG_SHAREDINST or CORINFO_FLG_VARIANCE
            //          where a class can be sealed, but they don't behave as exact types because casts to
            //          non-base types sometimes still succeed.
            //
            assertion.op1.kind         = O1K_SUBTYPE;
            assertion.op1.lcl.lclNum   = lclNum;
            assertion.op1.vn           = vnStore->VNConservativeNormalValue(op1->gtVNPair);
            assertion.op1.lcl.ssaNum   = op1->AsLclVarCommon()->GetSsaNum();
            assertion.op2.u1.iconVal   = op2->AsIntCon()->gtIconVal;
            assertion.op2.vn           = vnStore->VNConservativeNormalValue(op2->gtVNPair);
            assertion.op2.u1.iconFlags = op2->GetIconHandleFlag();

            //
            // Ok everything has been set and the assertion looks good
            //
            assertion.assertionKind = assertionKind;
        }
        else // !helperCallArgs
        {
            op2 = op2->SkipComma();

            assertion.op1.kind       = O1K_LCLVAR;
            assertion.op1.lcl.lclNum = lclNum;
            assertion.op1.vn         = vnStore->VNConservativeNormalValue(op1->gtVNPair);
            assertion.op1.lcl.ssaNum = op1->AsLclVarCommon()->GetSsaNum();

            switch (op2->gtOper)
            {
                optOp2Kind op2Kind;
                //
                //  No Assertion
                //
                default:
                    goto DONE_ASSERTION; // Don't make an assertion

                //
                //  Constant Assertions
                //
                case GT_CNS_INT:
                    op2Kind = O2K_CONST_INT;
                    goto CNS_COMMON;

                case GT_CNS_LNG:
                    op2Kind = O2K_CONST_LONG;
                    goto CNS_COMMON;

                case GT_CNS_DBL:
                    op2Kind = O2K_CONST_DOUBLE;
                    goto CNS_COMMON;

                CNS_COMMON:
                {
                    //
                    // Must either be an OAK_EQUAL or an OAK_NOT_EQUAL assertion
                    //
                    if ((assertionKind != OAK_EQUAL) && (assertionKind != OAK_NOT_EQUAL))
                    {
                        goto DONE_ASSERTION; // Don't make an assertion
                    }

                    // If the LclVar is a TYP_LONG then we only make
                    // assertions where op2 is also TYP_LONG
                    //
                    if ((lclVar->TypeGet() == TYP_LONG) && (op2->TypeGet() != TYP_LONG))
                    {
                        goto DONE_ASSERTION; // Don't make an assertion
                    }

                    assertion.op2.kind    = op2Kind;
                    assertion.op2.lconVal = 0;
                    assertion.op2.vn      = vnStore->VNConservativeNormalValue(op2->gtVNPair);

                    if (op2->gtOper == GT_CNS_INT)
                    {
#ifdef TARGET_ARM
                        // Do not Constant-Prop large constants for ARM
                        // TODO-CrossBitness: we wouldn't need the cast below if GenTreeIntCon::gtIconVal had
                        // target_ssize_t type.
                        if (!codeGen->validImmForMov((target_ssize_t)op2->AsIntCon()->gtIconVal))
                        {
                            goto DONE_ASSERTION; // Don't make an assertion
                        }
#endif // TARGET_ARM
                        // TODO-MIKE-Cleanup: This logic should be in a GenTreeIntCon function.
                        ssize_t value = op2->AsIntCon()->GetValue();
                        switch (lclVar->GetType())
                        {
                            case TYP_BYTE:
                                value = static_cast<int8_t>(value);
                                break;
                            case TYP_UBYTE:
                            case TYP_BOOL:
                                value = static_cast<uint8_t>(value);
                                break;
                            case TYP_SHORT:
                                value = static_cast<int16_t>(value);
                                break;
                            case TYP_USHORT:
                                value = static_cast<uint16_t>(value);
                                break;
                            default:
                                break;
                        }

                        assertion.op2.u1.iconVal   = value;
                        assertion.op2.u1.iconFlags = op2->GetIconHandleFlag();
#ifdef TARGET_64BIT
                        if (op2->TypeGet() == TYP_LONG || op2->TypeGet() == TYP_BYREF)
                        {
                            assertion.op2.u1.iconFlags |= GTF_ASSERTION_PROP_LONG;
                        }
#endif // TARGET_64BIT
                    }
                    else if (op2->gtOper == GT_CNS_LNG)
                    {
                        assertion.op2.lconVal = op2->AsLngCon()->gtLconVal;
                    }
                    else
                    {
                        noway_assert(op2->gtOper == GT_CNS_DBL);
                        /* If we have an NaN value then don't record it */
                        if (_isnan(op2->AsDblCon()->gtDconVal))
                        {
                            goto DONE_ASSERTION; // Don't make an assertion
                        }
                        assertion.op2.dconVal = op2->AsDblCon()->gtDconVal;
                    }

                    //
                    // Ok everything has been set and the assertion looks good
                    //
                    assertion.assertionKind = assertionKind;
                }
                break;

                //
                //  Copy Assertions
                //
                case GT_LCL_VAR:
                {
                    //
                    // Must either be an OAK_EQUAL or an OAK_NOT_EQUAL assertion
                    //
                    if ((assertionKind != OAK_EQUAL) && (assertionKind != OAK_NOT_EQUAL))
                    {
                        goto DONE_ASSERTION; // Don't make an assertion
                    }

                    unsigned lclNum2 = op2->AsLclVarCommon()->GetLclNum();
                    noway_assert(lclNum2 < lvaCount);
                    LclVarDsc* lclVar2 = &lvaTable[lclNum2];

                    // If the two locals are the same then bail
                    if (lclNum == lclNum2)
                    {
                        goto DONE_ASSERTION; // Don't make an assertion
                    }

                    // If the types are different then bail */
                    if (lclVar->lvType != lclVar2->lvType)
                    {
                        goto DONE_ASSERTION; // Don't make an assertion
                    }

                    // If we're making a copy of a "normalize on load" lclvar then the destination
                    // has to be "normalize on load" as well, otherwise we risk skipping normalization.
                    if (lclVar2->lvNormalizeOnLoad() && !lclVar->lvNormalizeOnLoad())
                    {
                        goto DONE_ASSERTION; // Don't make an assertion
                    }

                    //  If the local variable has its address exposed then bail
                    if (lclVar2->lvAddrExposed)
                    {
                        goto DONE_ASSERTION; // Don't make an assertion
                    }

                    assertion.op2.kind       = O2K_LCLVAR_COPY;
                    assertion.op2.lcl.lclNum = lclNum2;
                    assertion.op2.vn         = vnStore->VNConservativeNormalValue(op2->gtVNPair);
                    assertion.op2.lcl.ssaNum = op2->AsLclVarCommon()->GetSsaNum();

                    //
                    // Ok everything has been set and the assertion looks good
                    //
                    assertion.assertionKind = assertionKind;
                }
                break;

                //  Subrange Assertions
                case GT_EQ:
                case GT_NE:
                case GT_LT:
                case GT_LE:
                case GT_GT:
                case GT_GE:

                    /* Assigning the result of a RELOP, we can add a boolean subrange assertion */

                    toType = TYP_BOOL;
                    goto SUBRANGE_COMMON;

                case GT_CLS_VAR:

                    /* Assigning the result of an indirection into a LCL_VAR, see if we can add a subrange assertion */

                    toType = op2->gtType;
                    goto SUBRANGE_COMMON;

                case GT_ARR_ELEM:

                    /* Assigning the result of an indirection into a LCL_VAR, see if we can add a subrange assertion */

                    toType = op2->gtType;
                    goto SUBRANGE_COMMON;

                case GT_LCL_FLD:

                    /* Assigning the result of an indirection into a LCL_VAR, see if we can add a subrange assertion */

                    toType = op2->gtType;
                    goto SUBRANGE_COMMON;

                case GT_IND:

                    /* Assigning the result of an indirection into a LCL_VAR, see if we can add a subrange assertion */

                    toType = op2->gtType;
                    goto SUBRANGE_COMMON;

                case GT_CAST:
                {
                    if (lvaTable[lclNum].lvIsStructField && lvaTable[lclNum].lvNormalizeOnLoad())
                    {
                        // Keep the cast on small struct fields.
                        goto DONE_ASSERTION; // Don't make an assertion
                    }

                    toType = op2->CastToType();
                SUBRANGE_COMMON:
                    if ((assertionKind != OAK_SUBRANGE) && (assertionKind != OAK_EQUAL))
                    {
                        goto DONE_ASSERTION; // Don't make an assertion
                    }

                    if (varTypeIsFloating(op1->TypeGet()))
                    {
                        // We don't make assertions on a cast from floating point
                        goto DONE_ASSERTION;
                    }

                    switch (toType)
                    {
                        case TYP_BOOL:
                        case TYP_BYTE:
                        case TYP_UBYTE:
                        case TYP_SHORT:
                        case TYP_USHORT:
#ifdef TARGET_64BIT
                        case TYP_UINT:
                        case TYP_INT:
#endif // TARGET_64BIT
                            assertion.op2.u2.loBound = AssertionDsc::GetLowerBoundForIntegralType(toType);
                            assertion.op2.u2.hiBound = AssertionDsc::GetUpperBoundForIntegralType(toType);
                            break;

                        default:
                            goto DONE_ASSERTION; // Don't make an assertion
                    }
                    assertion.op2.kind      = O2K_SUBRANGE;
                    assertion.assertionKind = OAK_SUBRANGE;
                }
                break;
            }
        } // else // !helperCallArgs
    }     // if (op1->gtOper == GT_LCL_VAR)

    //
    // Are we making an IsType assertion?
    //
    else if (op1->gtOper == GT_IND)
    {
        op1 = op1->AsOp()->gtOp1;
        //
        // Is this an indirection of a local variable?
        //
        if (op1->gtOper == GT_LCL_VAR)
        {
            unsigned lclNum = op1->AsLclVarCommon()->GetLclNum();
            noway_assert(lclNum < lvaCount);

            //  If the local variable is not in SSA then bail
            if (!lvaInSsa(lclNum))
            {
                goto DONE_ASSERTION;
            }

            // If we have an typeHnd indirection then op1 must be a TYP_REF
            //  and the indirection must produce a TYP_I
            //
            if (op1->gtType != TYP_REF)
            {
                goto DONE_ASSERTION; // Don't make an assertion
            }

            assertion.op1.kind       = O1K_EXACT_TYPE;
            assertion.op1.lcl.lclNum = lclNum;
            assertion.op1.vn         = vnStore->VNConservativeNormalValue(op1->gtVNPair);
            assertion.op1.lcl.ssaNum = op1->AsLclVarCommon()->GetSsaNum();

            assert((assertion.op1.lcl.ssaNum == SsaConfig::RESERVED_SSA_NUM) ||
                   (assertion.op1.vn ==
                    vnStore->VNConservativeNormalValue(
                        lvaTable[lclNum].GetPerSsaData(assertion.op1.lcl.ssaNum)->m_vnPair)));

            ssize_t      cnsValue  = 0;
            GenTreeFlags iconFlags = GTF_EMPTY;
            // Ngen case
            if (op2->gtOper == GT_IND)
            {
                if (!optIsTreeKnownIntValue(!optLocalAssertionProp, op2->AsOp()->gtOp1, &cnsValue, &iconFlags))
                {
                    goto DONE_ASSERTION; // Don't make an assertion
                }

                assertion.assertionKind  = assertionKind;
                assertion.op2.kind       = O2K_IND_CNS_INT;
                assertion.op2.u1.iconVal = cnsValue;
                assertion.op2.vn         = vnStore->VNConservativeNormalValue(op2->AsOp()->gtOp1->gtVNPair);

                /* iconFlags should only contain bits in GTF_ICON_HDL_MASK */
                assert((iconFlags & ~GTF_ICON_HDL_MASK) == 0);
                assertion.op2.u1.iconFlags = iconFlags;
#ifdef TARGET_64BIT
                if (op2->AsOp()->gtOp1->TypeGet() == TYP_LONG)
                {
                    assertion.op2.u1.iconFlags |= GTF_ASSERTION_PROP_LONG;
                }
#endif // TARGET_64BIT
            }
            // JIT case
            else if (optIsTreeKnownIntValue(!optLocalAssertionProp, op2, &cnsValue, &iconFlags))
            {
                assertion.assertionKind  = assertionKind;
                assertion.op2.kind       = O2K_CONST_INT;
                assertion.op2.u1.iconVal = cnsValue;
                assertion.op2.vn         = vnStore->VNConservativeNormalValue(op2->gtVNPair);

                /* iconFlags should only contain bits in GTF_ICON_HDL_MASK */
                assert((iconFlags & ~GTF_ICON_HDL_MASK) == 0);
                assertion.op2.u1.iconFlags = iconFlags;
#ifdef TARGET_64BIT
                if (op2->TypeGet() == TYP_LONG)
                {
                    assertion.op2.u1.iconFlags |= GTF_ASSERTION_PROP_LONG;
                }
#endif // TARGET_64BIT
            }
            else
            {
                goto DONE_ASSERTION; // Don't make an assertion
            }
        }
    }

DONE_ASSERTION:
    if (assertion.assertionKind == OAK_INVALID)
    {
        return NO_ASSERTION_INDEX;
    }

    if (!optLocalAssertionProp)
    {
        if ((assertion.op1.vn == ValueNumStore::NoVN) || (assertion.op2.vn == ValueNumStore::NoVN) ||
            (assertion.op1.vn == ValueNumStore::VNForVoid()) || (assertion.op2.vn == ValueNumStore::VNForVoid()))
        {
            return NO_ASSERTION_INDEX;
        }

        // TODO: only copy assertions rely on valid SSA number so we could generate more assertions here
        if ((assertion.op1.kind != O1K_VALUE_NUMBER) && (assertion.op1.lcl.ssaNum == SsaConfig::RESERVED_SSA_NUM))
        {
            return NO_ASSERTION_INDEX;
        }
    }

    // Now add the assertion to our assertion table
    noway_assert(assertion.op1.kind != O1K_INVALID);
    noway_assert((assertion.op1.kind == O1K_ARR_BND) || (assertion.op2.kind != O2K_INVALID));
    return optAddAssertion(&assertion);
}

/*****************************************************************************
 *
 * If tree is a constant node holding an integral value, retrieve the value in
 * pConstant. If the method returns true, pConstant holds the appropriate
 * constant. Set "vnBased" to true to indicate local or global assertion prop.
 * "pFlags" indicates if the constant is a handle marked by GTF_ICON_HDL_MASK.
 */
bool Compiler::optIsTreeKnownIntValue(bool vnBased, GenTree* tree, ssize_t* pConstant, GenTreeFlags* pFlags)
{
    // Is Local assertion prop?
    if (!vnBased)
    {
        if (tree->OperGet() == GT_CNS_INT)
        {
            *pConstant = tree->AsIntCon()->IconValue();
            *pFlags    = tree->GetIconHandleFlag();
            return true;
        }
#ifdef TARGET_64BIT
        // Just to be clear, get it from gtLconVal rather than
        // overlapping gtIconVal.
        else if (tree->OperGet() == GT_CNS_LNG)
        {
            *pConstant = tree->AsLngCon()->gtLconVal;
            *pFlags    = tree->GetIconHandleFlag();
            return true;
        }
#endif
        return false;
    }

    // Global assertion prop
    ValueNum vn = vnStore->VNConservativeNormalValue(tree->gtVNPair);
    if (!vnStore->IsVNConstant(vn))
    {
        return false;
    }

    // ValueNumber 'vn' indicates that this node evaluates to a constant

    var_types vnType = vnStore->TypeOfVN(vn);
    if (vnType == TYP_INT)
    {
        *pConstant = vnStore->ConstantValue<int>(vn);
        *pFlags    = vnStore->IsVNHandle(vn) ? vnStore->GetHandleFlags(vn) : GTF_EMPTY;
        return true;
    }
#ifdef TARGET_64BIT
    else if (vnType == TYP_LONG)
    {
        *pConstant = vnStore->ConstantValue<INT64>(vn);
        *pFlags    = vnStore->IsVNHandle(vn) ? vnStore->GetHandleFlags(vn) : GTF_EMPTY;
        return true;
    }
#endif
    return false;
}

#ifdef DEBUG
/*****************************************************************************
 *
 * Print the assertions related to a VN for all VNs.
 *
 */
void Compiler::optPrintVnAssertionMapping()
{
    printf("\nVN Assertion Mapping\n");
    printf("---------------------\n");
    for (ValueNumToAssertsMap::KeyIterator ki = optValueNumToAsserts->Begin(); !ki.Equal(optValueNumToAsserts->End());
         ++ki)
    {
        printf("(%d => ", ki.Get());
        printf("%s)\n", BitVecOps::ToString(apTraits, ki.GetValue()));
    }
}
#endif

/*****************************************************************************
 *
 * Maintain a map "optValueNumToAsserts" i.e., vn -> to set of assertions
 * about that VN. Given "assertions" about a "vn" add it to the previously
 * mapped assertions about that "vn."
 */
void Compiler::optAddVnAssertionMapping(ValueNum vn, AssertionIndex index)
{
    ASSERT_TP* cur = optValueNumToAsserts->LookupPointer(vn);
    if (cur == nullptr)
    {
        optValueNumToAsserts->Set(vn, BitVecOps::MakeSingleton(apTraits, index - 1));
    }
    else
    {
        BitVecOps::AddElemD(apTraits, *cur, index - 1);
    }
}

/*****************************************************************************
 * Statically if we know that this assertion's VN involves a NaN don't bother
 * wasting an assertion table slot.
 */
bool Compiler::optAssertionVnInvolvesNan(AssertionDsc* assertion)
{
    if (optLocalAssertionProp)
    {
        return false;
    }

    static const int SZ      = 2;
    ValueNum         vns[SZ] = {assertion->op1.vn, assertion->op2.vn};
    for (int i = 0; i < SZ; ++i)
    {
        if (vnStore->IsVNConstant(vns[i]))
        {
            var_types type = vnStore->TypeOfVN(vns[i]);
            if ((type == TYP_FLOAT && _isnan(vnStore->ConstantValue<float>(vns[i])) != 0) ||
                (type == TYP_DOUBLE && _isnan(vnStore->ConstantValue<double>(vns[i])) != 0))
            {
                return true;
            }
        }
    }
    return false;
}

/*****************************************************************************
 *
 *  Given an assertion add it to the assertion table
 *
 *  If it is already in the assertion table return the assertionIndex that
 *  we use to refer to this element.
 *  Otherwise add it to the assertion table ad return the assertionIndex that
 *  we use to refer to this element.
 *  If we need to add to the table and the table is full return the value zero
 */
AssertionIndex Compiler::optAddAssertion(AssertionDsc* newAssertion)
{
    noway_assert(newAssertion->assertionKind != OAK_INVALID);

    // Even though the propagation step takes care of NaN, just a check
    // to make sure there is no slot involving a NaN.
    if (optAssertionVnInvolvesNan(newAssertion))
    {
        JITDUMP("Assertion involved Nan not adding\n");
        return NO_ASSERTION_INDEX;
    }

    // Check if exists already, so we can skip adding new one. Search backwards.
    for (AssertionIndex index = optAssertionCount; index >= 1; index--)
    {
        AssertionDsc* curAssertion = optGetAssertion(index);
        if (curAssertion->Equals(newAssertion, !optLocalAssertionProp))
        {
            return index;
        }
    }

    // Check if we are within max count.
    if (optAssertionCount >= optMaxAssertionCount)
    {
        return NO_ASSERTION_INDEX;
    }

    optAssertionTabPrivate[optAssertionCount] = *newAssertion;
    optAssertionCount++;

#ifdef DEBUG
    if (verbose)
    {
        printf("GenTreeNode creates assertion:\n");
        gtDispTree(optAssertionPropCurrentTree, nullptr, nullptr, true);
        printf(optLocalAssertionProp ? "In " FMT_BB " New Local " : "In " FMT_BB " New Global ", compCurBB->bbNum);
        optPrintAssertion(newAssertion, optAssertionCount);
    }
#endif // DEBUG

    // Assertion mask bits are [index + 1].
    if (optLocalAssertionProp)
    {
        assert(newAssertion->op1.kind == O1K_LCLVAR);

        // Mark the variables this index depends on
        unsigned lclNum = newAssertion->op1.lcl.lclNum;
        BitVecOps::AddElemD(apTraits, GetAssertionDep(lclNum), optAssertionCount - 1);
        if (newAssertion->op2.kind == O2K_LCLVAR_COPY)
        {
            lclNum = newAssertion->op2.lcl.lclNum;
            BitVecOps::AddElemD(apTraits, GetAssertionDep(lclNum), optAssertionCount - 1);
        }
    }
    else
    // If global assertion prop, then add it to the dependents map.
    {
        optAddVnAssertionMapping(newAssertion->op1.vn, optAssertionCount);
        if (newAssertion->op2.kind == O2K_LCLVAR_COPY)
        {
            optAddVnAssertionMapping(newAssertion->op2.vn, optAssertionCount);
        }
    }

#ifdef DEBUG
    optDebugCheckAssertions(optAssertionCount);
#endif
    return optAssertionCount;
}

#ifdef DEBUG
void Compiler::optDebugCheckAssertion(AssertionDsc* assertion)
{
    assert(assertion->assertionKind < OAK_COUNT);
    assert(assertion->op1.kind < O1K_COUNT);
    assert(assertion->op2.kind < O2K_COUNT);
    // It would be good to check that op1.vn and op2.vn are valid value numbers.

    switch (assertion->op1.kind)
    {
        case O1K_LCLVAR:
        case O1K_EXACT_TYPE:
        case O1K_SUBTYPE:
            assert(assertion->op1.lcl.lclNum < lvaCount);
            assert(optLocalAssertionProp ||
                   lvaTable[assertion->op1.lcl.lclNum].lvPerSsaData.IsValidSsaNum(assertion->op1.lcl.ssaNum));
            break;
        case O1K_ARR_BND:
            // It would be good to check that bnd.vnIdx and bnd.vnLen are valid value numbers.
            break;
        case O1K_BOUND_OPER_BND:
        case O1K_BOUND_LOOP_BND:
        case O1K_CONSTANT_LOOP_BND:
        case O1K_VALUE_NUMBER:
            assert(!optLocalAssertionProp);
            break;
        default:
            break;
    }
    switch (assertion->op2.kind)
    {
        case O2K_IND_CNS_INT:
        case O2K_CONST_INT:
        {
// The only flags that can be set are those in the GTF_ICON_HDL_MASK, or GTF_ASSERTION_PROP_LONG, which is
// used to indicate a long constant.
#ifdef TARGET_64BIT
            assert((assertion->op2.u1.iconFlags & ~(GTF_ICON_HDL_MASK | GTF_ASSERTION_PROP_LONG)) == 0);
#else
            assert((assertion->op2.u1.iconFlags & ~GTF_ICON_HDL_MASK) == 0);
#endif
            switch (assertion->op1.kind)
            {
                case O1K_EXACT_TYPE:
                case O1K_SUBTYPE:
                    assert(assertion->op2.u1.iconFlags != GTF_EMPTY);
                    break;
                case O1K_LCLVAR:
                    assert((lvaTable[assertion->op1.lcl.lclNum].lvType != TYP_REF) ||
                           (assertion->op2.u1.iconVal == 0) || doesMethodHaveFrozenString());
                    break;
                case O1K_VALUE_NUMBER:
                    assert((vnStore->TypeOfVN(assertion->op1.vn) != TYP_REF) || (assertion->op2.u1.iconVal == 0));
                    break;
                default:
                    break;
            }
        }
        break;

        case O2K_CONST_LONG:
        {
            // All handles should be represented by O2K_CONST_INT,
            // so no handle bits should be set here.
            assert((assertion->op2.u1.iconFlags & GTF_ICON_HDL_MASK) == 0);
        }
        break;

        default:
            // for all other 'assertion->op2.kind' values we don't check anything
            break;
    }
}

/*****************************************************************************
 *
 *  Verify that assertion prop related assumptions are valid. If "index"
 *  is 0 (i.e., NO_ASSERTION_INDEX) then verify all assertions in the table.
 *  If "index" is between 1 and optAssertionCount, then verify the assertion
 *  desc corresponding to "index."
 */
void Compiler::optDebugCheckAssertions(AssertionIndex index)
{
    AssertionIndex start = (index == NO_ASSERTION_INDEX) ? 1 : index;
    AssertionIndex end   = (index == NO_ASSERTION_INDEX) ? optAssertionCount : index;
    for (AssertionIndex ind = start; ind <= end; ++ind)
    {
        AssertionDsc* assertion = optGetAssertion(ind);
        optDebugCheckAssertion(assertion);
    }
}
#endif

//------------------------------------------------------------------------
// optCreateComplementaryAssertion: Create an assertion that is the complementary
//     of the specified assertion.
//
// Arguments:
//    assertionIndex - the index of the assertion
//    op1 - the first assertion operand
//    op2 - the second assertion operand
//    helperCallArgs - when true this indicates that the assertion operands
//                     are the arguments of a type cast helper call such as
//                     CORINFO_HELP_ISINSTANCEOFCLASS
//
// Notes:
//    The created complementary assertion is associated with the original
//    assertion such that it can be found by optFindComplementary.
//
void Compiler::optCreateComplementaryAssertion(AssertionIndex assertionIndex,
                                               GenTree*       op1,
                                               GenTree*       op2,
                                               bool           helperCallArgs)
{
    if (assertionIndex == NO_ASSERTION_INDEX)
    {
        return;
    }

    AssertionDsc& candidateAssertion = *optGetAssertion(assertionIndex);
    if (candidateAssertion.op1.kind == O1K_BOUND_OPER_BND || candidateAssertion.op1.kind == O1K_BOUND_LOOP_BND ||
        candidateAssertion.op1.kind == O1K_CONSTANT_LOOP_BND)
    {
        AssertionDsc dsc  = candidateAssertion;
        dsc.assertionKind = dsc.assertionKind == OAK_EQUAL ? OAK_NOT_EQUAL : OAK_EQUAL;
        optAddAssertion(&dsc);
        return;
    }

    if (candidateAssertion.assertionKind == OAK_EQUAL)
    {
        AssertionIndex index = optCreateAssertion(op1, op2, OAK_NOT_EQUAL, helperCallArgs);
        optMapComplementary(index, assertionIndex);
    }
    else if (candidateAssertion.assertionKind == OAK_NOT_EQUAL)
    {
        AssertionIndex index = optCreateAssertion(op1, op2, OAK_EQUAL, helperCallArgs);
        optMapComplementary(index, assertionIndex);
    }

    // Are we making a subtype or exact type assertion?
    if ((candidateAssertion.op1.kind == O1K_SUBTYPE) || (candidateAssertion.op1.kind == O1K_EXACT_TYPE))
    {
        optCreateAssertion(op1, nullptr, OAK_NOT_EQUAL);
    }
}

//------------------------------------------------------------------------
// optCreateJtrueAssertions: Create assertions about a JTRUE's relop operands.
//
// Arguments:
//    op1 - the first assertion operand
//    op2 - the second assertion operand
//    assertionKind - the assertion kind
//    helperCallArgs - when true this indicates that the assertion operands
//                     are the arguments of a type cast helper call such as
//                     CORINFO_HELP_ISINSTANCEOFCLASS
// Return Value:
//    The new assertion index or NO_ASSERTION_INDEX if a new assertion
//    was not created.
//
// Notes:
//    Assertion creation may fail either because the provided assertion
//    operands aren't supported or because the assertion table is full.
//    If an assertion is created succesfully then an attempt is made to also
//    create a second, complementary assertion. This may too fail, for the
//    same reasons as the first one.
//
AssertionIndex Compiler::optCreateJtrueAssertions(GenTree*                   op1,
                                                  GenTree*                   op2,
                                                  Compiler::optAssertionKind assertionKind,
                                                  bool                       helperCallArgs)
{
    AssertionIndex assertionIndex = optCreateAssertion(op1, op2, assertionKind, helperCallArgs);
    // Don't bother if we don't have an assertion on the JTrue False path. Current implementation
    // allows for a complementary only if there is an assertion on the False path (tree->HasAssertion()).
    if (assertionIndex != NO_ASSERTION_INDEX)
    {
        optCreateComplementaryAssertion(assertionIndex, op1, op2, helperCallArgs);
    }
    return assertionIndex;
}

AssertionInfo Compiler::optCreateJTrueBoundsAssertion(GenTree* tree)
{
    GenTree* relop = tree->gtGetOp1();
    if ((relop->OperKind() & GTK_RELOP) == 0)
    {
        return NO_ASSERTION_INDEX;
    }
    GenTree* op1 = relop->gtGetOp1();
    GenTree* op2 = relop->gtGetOp2();

    ValueNum op1VN   = vnStore->VNConservativeNormalValue(op1->gtVNPair);
    ValueNum op2VN   = vnStore->VNConservativeNormalValue(op2->gtVNPair);
    ValueNum relopVN = vnStore->VNConservativeNormalValue(relop->gtVNPair);

    bool hasTestAgainstZero =
        (relop->gtOper == GT_EQ || relop->gtOper == GT_NE) && (op2VN == vnStore->VNZeroForType(op2->TypeGet()));

    ValueNumStore::UnsignedCompareCheckedBoundInfo unsignedCompareBnd;
    // Cases where op1 holds the upper bound arithmetic and op2 is 0.
    // Loop condition like: "i < bnd +/-k == 0"
    // Assertion: "i < bnd +/- k == 0"
    if (hasTestAgainstZero && vnStore->IsVNCompareCheckedBoundArith(op1VN))
    {
        AssertionDsc dsc;
        dsc.assertionKind    = relop->gtOper == GT_EQ ? OAK_EQUAL : OAK_NOT_EQUAL;
        dsc.op1.kind         = O1K_BOUND_OPER_BND;
        dsc.op1.vn           = op1VN;
        dsc.op2.kind         = O2K_CONST_INT;
        dsc.op2.vn           = vnStore->VNZeroForType(op2->TypeGet());
        dsc.op2.u1.iconVal   = 0;
        dsc.op2.u1.iconFlags = GTF_EMPTY;
        AssertionIndex index = optAddAssertion(&dsc);
        optCreateComplementaryAssertion(index, nullptr, nullptr);
        return index;
    }
    // Cases where op1 holds the lhs of the condition and op2 holds the bound arithmetic.
    // Loop condition like: "i < bnd +/-k"
    // Assertion: "i < bnd +/- k != 0"
    else if (vnStore->IsVNCompareCheckedBoundArith(relopVN))
    {
        AssertionDsc dsc;
        dsc.assertionKind    = OAK_NOT_EQUAL;
        dsc.op1.kind         = O1K_BOUND_OPER_BND;
        dsc.op1.vn           = relopVN;
        dsc.op2.kind         = O2K_CONST_INT;
        dsc.op2.vn           = vnStore->VNZeroForType(op2->TypeGet());
        dsc.op2.u1.iconVal   = 0;
        dsc.op2.u1.iconFlags = GTF_EMPTY;
        AssertionIndex index = optAddAssertion(&dsc);
        optCreateComplementaryAssertion(index, nullptr, nullptr);
        return index;
    }
    // Cases where op1 holds the upper bound and op2 is 0.
    // Loop condition like: "i < bnd == 0"
    // Assertion: "i < bnd == false"
    else if (hasTestAgainstZero && vnStore->IsVNCompareCheckedBound(op1VN))
    {
        AssertionDsc dsc;
        dsc.assertionKind    = relop->gtOper == GT_EQ ? OAK_EQUAL : OAK_NOT_EQUAL;
        dsc.op1.kind         = O1K_BOUND_LOOP_BND;
        dsc.op1.vn           = op1VN;
        dsc.op2.kind         = O2K_CONST_INT;
        dsc.op2.vn           = vnStore->VNZeroForType(op2->TypeGet());
        dsc.op2.u1.iconVal   = 0;
        dsc.op2.u1.iconFlags = GTF_EMPTY;
        AssertionIndex index = optAddAssertion(&dsc);
        optCreateComplementaryAssertion(index, nullptr, nullptr);
        return index;
    }
    // Cases where op1 holds the lhs of the condition op2 holds the bound.
    // Loop condition like "i < bnd"
    // Assertion: "i < bnd != 0"
    else if (vnStore->IsVNCompareCheckedBound(relopVN))
    {
        AssertionDsc dsc;
        dsc.assertionKind    = OAK_NOT_EQUAL;
        dsc.op1.kind         = O1K_BOUND_LOOP_BND;
        dsc.op1.vn           = relopVN;
        dsc.op2.kind         = O2K_CONST_INT;
        dsc.op2.vn           = vnStore->VNZeroForType(TYP_INT);
        dsc.op2.u1.iconVal   = 0;
        dsc.op2.u1.iconFlags = GTF_EMPTY;
        AssertionIndex index = optAddAssertion(&dsc);
        optCreateComplementaryAssertion(index, nullptr, nullptr);
        return index;
    }
    // Loop condition like "(uint)i < (uint)bnd" or equivalent
    // Assertion: "no throw" since this condition guarantees that i is both >= 0 and < bnd (on the appropiate edge)
    else if (vnStore->IsVNUnsignedCompareCheckedBound(relopVN, &unsignedCompareBnd))
    {
        assert(unsignedCompareBnd.vnIdx != ValueNumStore::NoVN);
        assert((unsignedCompareBnd.cmpOper == VNF_LT_UN) || (unsignedCompareBnd.cmpOper == VNF_GE_UN));
        assert(vnStore->IsVNCheckedBound(unsignedCompareBnd.vnBound));

        AssertionDsc dsc;
        dsc.assertionKind = OAK_NO_THROW;
        dsc.op1.kind      = O1K_ARR_BND;
        dsc.op1.vn        = relopVN;
        dsc.op1.bnd.vnIdx = unsignedCompareBnd.vnIdx;
        dsc.op1.bnd.vnLen = vnStore->VNNormalValue(unsignedCompareBnd.vnBound);
        dsc.op2.kind      = O2K_INVALID;
        dsc.op2.vn        = ValueNumStore::NoVN;

        AssertionIndex index = optAddAssertion(&dsc);
        if (unsignedCompareBnd.cmpOper == VNF_GE_UN)
        {
            // By default JTRUE generated assertions hold on the "jump" edge. We have i >= bnd but we're really
            // after i < bnd so we need to change the assertion edge to "next".
            return AssertionInfo::ForNextEdge(index);
        }
        return index;
    }
    // Cases where op1 holds the condition bound check and op2 is 0.
    // Loop condition like: "i < 100 == 0"
    // Assertion: "i < 100 == false"
    else if (hasTestAgainstZero && vnStore->IsVNConstantBound(op1VN))
    {
        AssertionDsc dsc;
        dsc.assertionKind    = relop->gtOper == GT_EQ ? OAK_EQUAL : OAK_NOT_EQUAL;
        dsc.op1.kind         = O1K_CONSTANT_LOOP_BND;
        dsc.op1.vn           = op1VN;
        dsc.op2.kind         = O2K_CONST_INT;
        dsc.op2.vn           = vnStore->VNZeroForType(op2->TypeGet());
        dsc.op2.u1.iconVal   = 0;
        dsc.op2.u1.iconFlags = GTF_EMPTY;
        AssertionIndex index = optAddAssertion(&dsc);
        optCreateComplementaryAssertion(index, nullptr, nullptr);
        return index;
    }
    // Cases where op1 holds the lhs of the condition op2 holds rhs.
    // Loop condition like "i < 100"
    // Assertion: "i < 100 != 0"
    else if (vnStore->IsVNConstantBound(relopVN))
    {
        AssertionDsc dsc;
        dsc.assertionKind    = OAK_NOT_EQUAL;
        dsc.op1.kind         = O1K_CONSTANT_LOOP_BND;
        dsc.op1.vn           = relopVN;
        dsc.op2.kind         = O2K_CONST_INT;
        dsc.op2.vn           = vnStore->VNZeroForType(TYP_INT);
        dsc.op2.u1.iconVal   = 0;
        dsc.op2.u1.iconFlags = GTF_EMPTY;
        AssertionIndex index = optAddAssertion(&dsc);
        optCreateComplementaryAssertion(index, nullptr, nullptr);
        return index;
    }

    return NO_ASSERTION_INDEX;
}

/*****************************************************************************
 *
 *  Compute assertions for the JTrue node.
 */
AssertionInfo Compiler::optAssertionGenJtrue(GenTree* tree)
{
    // Only create assertions for JTRUE when we are in the global phase
    if (optLocalAssertionProp)
    {
        return NO_ASSERTION_INDEX;
    }

    GenTree* relop = tree->AsOp()->gtOp1;
    if ((relop->OperKind() & GTK_RELOP) == 0)
    {
        return NO_ASSERTION_INDEX;
    }

    Compiler::optAssertionKind assertionKind = OAK_INVALID;

    AssertionInfo info = optCreateJTrueBoundsAssertion(tree);
    if (info.HasAssertion())
    {
        return info;
    }

    // Find assertion kind.
    switch (relop->gtOper)
    {
        case GT_EQ:
            assertionKind = OAK_EQUAL;
            break;
        case GT_NE:
            assertionKind = OAK_NOT_EQUAL;
            break;
        default:
            // TODO-CQ: add other relop operands. Disabled for now to measure perf
            // and not occupy assertion table slots. We'll add them when used.
            return NO_ASSERTION_INDEX;
    }

    // Look through any CSEs so we see the actual trees providing values, if possible.
    // This is important for exact type assertions, which need to see the GT_IND.
    //
    GenTree* op1 = relop->AsOp()->gtOp1->gtCommaAssignVal();
    GenTree* op2 = relop->AsOp()->gtOp2->gtCommaAssignVal();

    // Check for op1 or op2 to be lcl var and if so, keep it in op1.
    if ((op1->gtOper != GT_LCL_VAR) && (op2->gtOper == GT_LCL_VAR))
    {
        std::swap(op1, op2);
    }

    ValueNum op1VN = vnStore->VNConservativeNormalValue(op1->gtVNPair);
    ValueNum op2VN = vnStore->VNConservativeNormalValue(op2->gtVNPair);
    // If op1 is lcl and op2 is const or lcl, create assertion.
    if ((op1->gtOper == GT_LCL_VAR) &&
        ((op2->OperKind() & GTK_CONST) || (op2->gtOper == GT_LCL_VAR))) // Fix for Dev10 851483
    {
        return optCreateJtrueAssertions(op1, op2, assertionKind);
    }
    else if (vnStore->IsVNCheckedBound(op1VN) && vnStore->IsVNInt32Constant(op2VN))
    {
        assert(relop->OperIs(GT_EQ, GT_NE));

        int con = vnStore->ConstantValue<int>(op2VN);
        if (con >= 0)
        {
            AssertionDsc dsc;

            // For arr.Length != 0, we know that 0 is a valid index
            // For arr.Length == con, we know that con - 1 is the greatest valid index
            if (con == 0)
            {
                dsc.assertionKind = OAK_NOT_EQUAL;
                dsc.op1.bnd.vnIdx = vnStore->VNForIntCon(0);
            }
            else
            {
                dsc.assertionKind = OAK_EQUAL;
                dsc.op1.bnd.vnIdx = vnStore->VNForIntCon(con - 1);
            }

            dsc.op1.vn           = op1VN;
            dsc.op1.kind         = O1K_ARR_BND;
            dsc.op1.bnd.vnLen    = op1VN;
            dsc.op2.vn           = vnStore->VNConservativeNormalValue(op2->gtVNPair);
            dsc.op2.kind         = O2K_CONST_INT;
            dsc.op2.u1.iconFlags = GTF_EMPTY;
            dsc.op2.u1.iconVal   = 0;

            // when con is not zero, create an assertion on the arr.Length == con edge
            // when con is zero, create an assertion on the arr.Length != 0 edge
            AssertionIndex index = optAddAssertion(&dsc);
            if (relop->OperIs(GT_NE) != (con == 0))
            {
                return AssertionInfo::ForNextEdge(index);
            }
            else
            {
                return index;
            }
        }
    }

    // Check op1 and op2 for an indirection of a GT_LCL_VAR and keep it in op1.
    if (((op1->gtOper != GT_IND) || (op1->AsOp()->gtOp1->gtOper != GT_LCL_VAR)) &&
        ((op2->gtOper == GT_IND) && (op2->AsOp()->gtOp1->gtOper == GT_LCL_VAR)))
    {
        std::swap(op1, op2);
    }
    // If op1 is ind, then extract op1's oper.
    if ((op1->gtOper == GT_IND) && (op1->AsOp()->gtOp1->gtOper == GT_LCL_VAR))
    {
        return optCreateJtrueAssertions(op1, op2, assertionKind);
    }

    // Look for a call to an IsInstanceOf helper compared to a nullptr
    if ((op2->gtOper != GT_CNS_INT) && (op1->gtOper == GT_CNS_INT))
    {
        std::swap(op1, op2);
    }
    // Validate op1 and op2
    if ((op1->gtOper != GT_CALL) || (op1->AsCall()->gtCallType != CT_HELPER) || (op1->TypeGet() != TYP_REF) || // op1
        (op2->gtOper != GT_CNS_INT) || (op2->AsIntCon()->gtIconVal != 0))                                      // op2
    {
        return NO_ASSERTION_INDEX;
    }

    GenTreeCall* call = op1->AsCall();

    // Note CORINFO_HELP_READYTORUN_ISINSTANCEOF does not have the same argument pattern.
    // In particular, it is not possible to deduce what class is being tested from its args.
    //
    // Also note The CASTCLASS helpers won't appear in predicates as they throw on failure.
    // So the helper list here is smaller than the one in optAssertionProp_Call.
    if ((call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_ISINSTANCEOFINTERFACE)) ||
        (call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_ISINSTANCEOFARRAY)) ||
        (call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_ISINSTANCEOFCLASS)) ||
        (call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_ISINSTANCEOFANY)))
    {
        CallInfo* argInfo         = call->GetInfo();
        GenTree*  objectNode      = call->GetArgNodeByArgNum(1);
        GenTree*  methodTableNode = call->GetArgNodeByArgNum(0);

        assert(objectNode->TypeGet() == TYP_REF);
        assert(methodTableNode->TypeGet() == TYP_I_IMPL);

        // Reverse the assertion
        assert((assertionKind == OAK_EQUAL) || (assertionKind == OAK_NOT_EQUAL));
        assertionKind = (assertionKind == OAK_EQUAL) ? OAK_NOT_EQUAL : OAK_EQUAL;

        if (objectNode->OperIs(GT_LCL_VAR))
        {
            return optCreateJtrueAssertions(objectNode, methodTableNode, assertionKind, /* helperCallArgs */ true);
        }
    }

    return NO_ASSERTION_INDEX;
}

/*****************************************************************************
 *
 *  Create an assertion on the phi node if some information can be gleaned
 *  from all of the constituent phi operands.
 *
 */
AssertionIndex Compiler::optAssertionGenPhiDefn(GenTree* tree)
{
    if (!tree->IsPhiDefn())
    {
        return NO_ASSERTION_INDEX;
    }

    // Try to find if all phi arguments are known to be non-null.
    bool isNonNull = true;
    for (GenTreePhi::Use& use : tree->AsOp()->gtGetOp2()->AsPhi()->Uses())
    {
        if (!vnStore->IsKnownNonNull(use.GetNode()->gtVNPair.GetConservative()))
        {
            isNonNull = false;
            break;
        }
    }

    // All phi arguments are non-null implies phi rhs is non-null.
    if (isNonNull)
    {
        return optCreateAssertion(tree->AsOp()->gtOp1, nullptr, OAK_NOT_EQUAL);
    }
    return NO_ASSERTION_INDEX;
}

/*****************************************************************************
 *
 *  If this statement creates a value assignment or assertion
 *  then assign an index to the given value assignment by adding
 *  it to the lookup table, if necessary.
 */
void Compiler::optAssertionGen(GenTree* tree)
{
    tree->ClearAssertion();

    // If there are QMARKs in the IR, we won't generate assertions
    // for conditionally executed code.
    //
    if (optLocalAssertionProp && ((tree->gtFlags & GTF_COLON_COND) != 0))
    {
        return;
    }

#ifdef DEBUG
    optAssertionPropCurrentTree = tree;
#endif

    // For most of the assertions that we create below
    // the assertion is true after the tree is processed
    bool          assertionProven = true;
    AssertionInfo assertionInfo;
    switch (tree->gtOper)
    {
        case GT_ASG:
            // VN takes care of non local assertions for assignments and data flow.
            if (optLocalAssertionProp)
            {
                assertionInfo = optCreateAssertion(tree->AsOp()->gtOp1, tree->AsOp()->gtOp2, OAK_EQUAL);
            }
            else
            {
                assertionInfo = optAssertionGenPhiDefn(tree);
            }
            break;

        case GT_BLK:
            if (tree->AsBlk()->GetLayout()->GetSize() == 0)
            {
                break;
            }
            __fallthrough;
        case GT_OBJ:
        case GT_IND:
        case GT_NULLCHECK:
            // All indirections create non-null assertions
            assertionInfo = optCreateAssertion(tree->AsIndir()->Addr(), nullptr, OAK_NOT_EQUAL);
            break;

        case GT_ARR_LENGTH:
            assertionInfo = optCreateAssertion(tree->AsArrLen()->GetArray(), nullptr, OAK_NOT_EQUAL);
            break;

        case GT_ARR_BOUNDS_CHECK:
            if (!optLocalAssertionProp)
            {
                assertionInfo = optCreateAssertion(tree, nullptr, OAK_NO_THROW);
            }
            break;

        case GT_ARR_ELEM:
            // An array element reference can create a non-null assertion
            assertionInfo = optCreateAssertion(tree->AsArrElem()->gtArrObj, nullptr, OAK_NOT_EQUAL);
            break;

        case GT_CALL:
        {
            // A virtual call can create a non-null assertion. We transform some virtual calls into non-virtual calls
            // with a GTF_CALL_NULLCHECK flag set.
            // Ignore tail calls because they have 'this` pointer in the regular arg list and an implicit null check.
            GenTreeCall* const call = tree->AsCall();
            if (call->NeedsNullCheck() || (call->IsVirtual() && !call->IsTailCall()))
            {
                //  Retrieve the 'this' arg.
                GenTree* thisArg = gtGetThisArg(call);
                assert(thisArg != nullptr);
                assertionInfo = optCreateAssertion(thisArg, nullptr, OAK_NOT_EQUAL);
            }
        }
        break;

        case GT_CAST:
            // We only create this assertion for global assertion prop
            if (!optLocalAssertionProp)
            {
                // This represets an assertion that we would like to prove to be true. It is not actually a true
                // assertion.
                // If we can prove this assertion true then we can eliminate this cast.
                assertionInfo   = optCreateAssertion(tree->AsOp()->gtOp1, tree, OAK_SUBRANGE);
                assertionProven = false;
            }
            break;

        case GT_JTRUE:
            assertionInfo = optAssertionGenJtrue(tree);
            break;

        default:
            // All other gtOper node kinds, leave 'assertionIndex' = NO_ASSERTION_INDEX
            break;
    }

    // For global assertion prop we must store the assertion number in the tree node
    if (assertionInfo.HasAssertion() && assertionProven && !optLocalAssertionProp)
    {
        tree->SetAssertionInfo(assertionInfo);
    }
}

/*****************************************************************************
 *
 * Maps a complementary assertion to its original assertion so it can be
 * retrieved faster.
 */
void Compiler::optMapComplementary(AssertionIndex assertionIndex, AssertionIndex index)
{
    if (assertionIndex == NO_ASSERTION_INDEX || index == NO_ASSERTION_INDEX)
    {
        return;
    }

    assert(assertionIndex <= optMaxAssertionCount);
    assert(index <= optMaxAssertionCount);

    optComplementaryAssertionMap[assertionIndex] = index;
    optComplementaryAssertionMap[index]          = assertionIndex;
}

/*****************************************************************************
 *
 *  Given an assertion index, return the assertion index of the complementary
 *  assertion or 0 if one does not exist.
 */
AssertionIndex Compiler::optFindComplementary(AssertionIndex assertIndex)
{
    if (assertIndex == NO_ASSERTION_INDEX)
    {
        return NO_ASSERTION_INDEX;
    }
    AssertionDsc* inputAssertion = optGetAssertion(assertIndex);

    // Must be an equal or not equal assertion.
    if (inputAssertion->assertionKind != OAK_EQUAL && inputAssertion->assertionKind != OAK_NOT_EQUAL)
    {
        return NO_ASSERTION_INDEX;
    }

    AssertionIndex index = optComplementaryAssertionMap[assertIndex];
    if (index != NO_ASSERTION_INDEX && index <= optAssertionCount)
    {
        return index;
    }

    for (AssertionIndex index = 1; index <= optAssertionCount; ++index)
    {
        // Make sure assertion kinds are complementary and op1, op2 kinds match.
        AssertionDsc* curAssertion = optGetAssertion(index);
        if (curAssertion->Complementary(inputAssertion, !optLocalAssertionProp))
        {
            optMapComplementary(assertIndex, index);
            return index;
        }
    }
    return NO_ASSERTION_INDEX;
}

/*****************************************************************************
 *
 *  Given a lclNum, a fromType and a toType, return assertion index of the assertion that
 *  claims that a variable's value is always a valid subrange of the formType.
 *  Thus we can discard or omit a cast to fromType. Returns NO_ASSERTION_INDEX
 *  if one such assertion could not be found in "assertions."
 */

AssertionIndex Compiler::optAssertionIsSubrange(GenTree*         tree,
                                                var_types        fromType,
                                                var_types        toType,
                                                ASSERT_VALARG_TP assertions)
{
    if (!optLocalAssertionProp && BitVecOps::IsEmpty(apTraits, assertions))
    {
        return NO_ASSERTION_INDEX;
    }

    for (AssertionIndex index = 1; index <= optAssertionCount; index++)
    {
        AssertionDsc* curAssertion = optGetAssertion(index);
        if ((optLocalAssertionProp ||
             BitVecOps::IsMember(apTraits, assertions, index - 1)) && // either local prop or use propagated assertions
            (curAssertion->assertionKind == OAK_SUBRANGE) &&
            (curAssertion->op1.kind == O1K_LCLVAR))
        {
            // For local assertion prop use comparison on locals, and use comparison on vns for global prop.
            bool isEqual = optLocalAssertionProp
                               ? (curAssertion->op1.lcl.lclNum == tree->AsLclVarCommon()->GetLclNum())
                               : (curAssertion->op1.vn == vnStore->VNConservativeNormalValue(tree->gtVNPair));
            if (!isEqual)
            {
                continue;
            }

            // If we have an unsigned fromType, then the loBound can't be negative
            //
            if (varTypeIsUnsigned(fromType))
            {
                if (curAssertion->op2.u2.loBound < 0)
                {
                    continue;
                }
            }

            // Make sure the toType is within current assertion's bounds.
            switch (toType)
            {
                case TYP_BYTE:
                case TYP_UBYTE:
                case TYP_SHORT:
                case TYP_USHORT:
                    if ((curAssertion->op2.u2.loBound < AssertionDsc::GetLowerBoundForIntegralType(toType)) ||
                        (curAssertion->op2.u2.hiBound > AssertionDsc::GetUpperBoundForIntegralType(toType)))
                    {
                        continue;
                    }
                    break;

                case TYP_UINT:
                    if (curAssertion->op2.u2.loBound < AssertionDsc::GetLowerBoundForIntegralType(toType))
                    {
                        continue;
                    }
                    break;

                case TYP_INT:
                    break;

                default:
                    continue;
            }
            return index;
        }
    }
    return NO_ASSERTION_INDEX;
}

/**********************************************************************************
 *
 * Given a "tree" that is usually arg1 of a isinst/cast kind of GT_CALL (a class
 * handle), and "methodTableArg" which is a const int (a class handle), then search
 * if there is an assertion in "assertions", that asserts the equality of the two
 * class handles and then returns the index of the assertion. If one such assertion
 * could not be found, then it returns NO_ASSERTION_INDEX.
 *
 */
AssertionIndex Compiler::optAssertionIsSubtype(GenTree* tree, GenTree* methodTableArg, ASSERT_VALARG_TP assertions)
{
    if (!optLocalAssertionProp && BitVecOps::IsEmpty(apTraits, assertions))
    {
        return NO_ASSERTION_INDEX;
    }
    for (AssertionIndex index = 1; index <= optAssertionCount; index++)
    {
        if (!optLocalAssertionProp && !BitVecOps::IsMember(apTraits, assertions, index - 1))
        {
            continue;
        }

        AssertionDsc* curAssertion = optGetAssertion(index);
        if (curAssertion->assertionKind != OAK_EQUAL ||
            (curAssertion->op1.kind != O1K_SUBTYPE && curAssertion->op1.kind != O1K_EXACT_TYPE))
        {
            continue;
        }

        // If local assertion prop use "lcl" based comparison, if global assertion prop use vn based comparison.
        if ((optLocalAssertionProp) ? (curAssertion->op1.lcl.lclNum != tree->AsLclVarCommon()->GetLclNum())
                                    : (curAssertion->op1.vn != vnStore->VNConservativeNormalValue(tree->gtVNPair)))
        {
            continue;
        }

        if (curAssertion->op2.kind == O2K_IND_CNS_INT)
        {
            if (methodTableArg->gtOper != GT_IND)
            {
                continue;
            }
            methodTableArg = methodTableArg->AsOp()->gtOp1;
        }
        else if (curAssertion->op2.kind != O2K_CONST_INT)
        {
            continue;
        }

        ssize_t      methodTableVal = 0;
        GenTreeFlags iconFlags      = GTF_EMPTY;
        if (!optIsTreeKnownIntValue(!optLocalAssertionProp, methodTableArg, &methodTableVal, &iconFlags))
        {
            continue;
        }

        if (curAssertion->op2.u1.iconVal == methodTableVal)
        {
            return index;
        }
    }
    return NO_ASSERTION_INDEX;
}

//------------------------------------------------------------------------------
// optConstantAssertionProp: Possibly substitute a constant for a local use
//
// Arguments:
//    curAssertion - assertion to propagate
//    tree         - tree to possibly modify
//    stmt         - statement containing the tree
//    index        - index of this assertion in the assertion table
//
// Returns:
//    Updated tree (may be the input tree, modified in place), or nullptr
//
// Notes:
//    stmt may be nullptr during local assertion prop
//
GenTree* Compiler::optConstantAssertionProp(AssertionDsc*        curAssertion,
                                            GenTreeLclVarCommon* tree,
                                            Statement* stmt DEBUGARG(AssertionIndex index))
{
    const unsigned lclNum = tree->GetLclNum();

    if (lclNumIsCSE(lclNum))
    {
        return nullptr;
    }

    GenTree* newTree = tree;

    // Update 'newTree' with the new value from our table
    // Typically newTree == tree and we are updating the node in place
    switch (curAssertion->op2.kind)
    {
        case O2K_CONST_DOUBLE:
            // There could be a positive zero and a negative zero, so don't propagate zeroes.
            if (curAssertion->op2.dconVal == 0.0)
            {
                return nullptr;
            }
            newTree->ChangeOperConst(GT_CNS_DBL);
            newTree->AsDblCon()->gtDconVal = curAssertion->op2.dconVal;
            break;

        case O2K_CONST_LONG:

            if (newTree->gtType == TYP_LONG)
            {
                newTree->ChangeOperConst(GT_CNS_NATIVELONG);
                newTree->AsIntConCommon()->SetLngValue(curAssertion->op2.lconVal);
            }
            else
            {
                newTree->ChangeOperConst(GT_CNS_INT);
                newTree->AsIntCon()->gtIconVal = (int)curAssertion->op2.lconVal;
                newTree->gtType                = TYP_INT;
            }
            break;

        case O2K_CONST_INT:

            // Don't propagate handles if we need to report relocs.
            if (opts.compReloc && ((curAssertion->op2.u1.iconFlags & GTF_ICON_HDL_MASK) != 0))
            {
                return nullptr;
            }

            if (curAssertion->op2.u1.iconFlags & GTF_ICON_HDL_MASK)
            {
                // Here we have to allocate a new 'large' node to replace the old one
                // TODO-MIKE-Cleanup: Huh, what large node?!?
                newTree = gtNewIconHandleNode(curAssertion->op2.u1.iconVal,
                                              curAssertion->op2.u1.iconFlags & GTF_ICON_HDL_MASK);
            }
            else
            {
                // If we have done constant propagation of a struct type, it is only valid for zero-init,
                // and we have to ensure that we have the right zero for the type.
                assert(!varTypeIsStruct(tree->GetType()) || curAssertion->op2.u1.iconVal == 0);

#ifdef FEATURE_SIMD
                if (varTypeIsSIMD(tree->GetType()))
                {
                    LclVarDsc* lcl = lvaGetDesc(lclNum);
                    assert(lcl->GetType() == tree->GetType());
                    newTree = gtNewZeroSimdHWIntrinsicNode(lcl->GetLayout());
                }
                else
#endif // FEATURE_SIMD
                {
                    newTree->ChangeOperConst(GT_CNS_INT);
                    newTree->AsIntCon()->gtIconVal = curAssertion->op2.u1.iconVal;
                    newTree->ClearIconHandleMask();
                    if (newTree->TypeIs(TYP_STRUCT))
                    {
                        // LCL_VAR can be init with a GT_CNS_INT, keep its type INT, not STRUCT.
                        newTree->ChangeType(TYP_INT);
                    }
                }
            }

            // Constant ints are of type TYP_INT, not any of the short forms.
            if (varTypeIsIntegral(newTree->TypeGet()))
            {
#ifdef TARGET_64BIT
                var_types newType =
                    (var_types)((curAssertion->op2.u1.iconFlags & GTF_ASSERTION_PROP_LONG) ? TYP_LONG : TYP_INT);
                if (newTree->TypeGet() != newType)
                {
                    noway_assert(newTree->gtType != TYP_REF);
                    newTree->gtType = newType;
                }
#else
                if (newTree->TypeGet() != TYP_INT)
                {
                    noway_assert(newTree->gtType != TYP_REF && newTree->gtType != TYP_LONG);
                    newTree->gtType = TYP_INT;
                }
#endif
            }
            break;

        default:
            return nullptr;
    }

    if (!optLocalAssertionProp)
    {
        assert(newTree->OperIsConst());                      // We should have a simple Constant node for newTree
        assert(vnStore->IsVNConstant(curAssertion->op2.vn)); // The value number stored for op2 should be a valid
                                                             // VN representing the constant
        newTree->gtVNPair.SetBoth(curAssertion->op2.vn);     // Set the ValueNumPair to the constant VN from op2
                                                             // of the assertion
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("\nAssertion prop in " FMT_BB ":\n", compCurBB->bbNum);
        optPrintAssertion(curAssertion, index);
        gtDispTree(newTree, nullptr, nullptr, true);
    }
#endif

    return optAssertionProp_Update(newTree, tree, stmt);
}

//------------------------------------------------------------------------------
// optAssertionProp_LclVarTypeCheck: verify compatible types for copy prop
//
// Arguments:
//    tree         - tree to possibly modify
//    lclVarDsc    - local accessed by tree
//    copyVarDsc   - local to possibly copy prop into tree
//
// Returns:
//    True if copy prop is safe.
//
// Notes:
//    Before substituting copyVar for lclVar, make sure using copyVar doesn't widen access.
//
bool Compiler::optAssertionProp_LclVarTypeCheck(GenTree* tree, LclVarDsc* lclVarDsc, LclVarDsc* copyVarDsc)
{
    /*
        Small struct field locals are stored using the exact width and loaded widened
        (i.e. lvNormalizeOnStore==false   lvNormalizeOnLoad==true),
        because the field locals might end up embedded in the parent struct local with the exact width.

            In other words, a store to a short field local should always done using an exact width store

                [00254538] 0x0009 ------------               const     int    0x1234
            [002545B8] 0x000B -A--G--NR---               =         short
                [00254570] 0x000A D------N----               lclVar    short  V43 tmp40

            mov   word  ptr [L_043], 0x1234

        Now, if we copy prop, say a short field local V43, to another short local V34
        for the following tree:

                [04E18650] 0x0001 ------------               lclVar    int   V34 tmp31
            [04E19714] 0x0002 -A----------               =         int
                [04E196DC] 0x0001 D------N----               lclVar    int   V36 tmp33

        We will end with this tree:

                [04E18650] 0x0001 ------------               lclVar    int   V43 tmp40
            [04E19714] 0x0002 -A-----NR---               =         int
                [04E196DC] 0x0001 D------N----               lclVar    int   V36 tmp33    EAX

        And eventually causing a fetch of 4-byte out from [L_043] :(
            mov     EAX, dword ptr [L_043]

        The following check is to make sure we only perform the copy prop
        when we don't retrieve the wider value.
    */

    if (copyVarDsc->lvIsStructField)
    {
        var_types varType = (var_types)copyVarDsc->lvType;
        // Make sure we don't retrieve the wider value.
        return !varTypeIsSmall(varType) || (varType == tree->TypeGet());
    }
    // Called in the context of a single copy assertion, so the types should have been
    // taken care by the assertion gen logic for other cases. Just return true.
    return true;
}

//------------------------------------------------------------------------
// optCopyAssertionProp: copy prop use of one local with another
//
// Arguments:
//    curAssertion - assertion triggering the possible copy
//    tree         - tree use to consider replacing
//    stmt         - statment containing the tree
//    index        - index of the assertion
//
// Returns:
//    Updated tree, or nullptr
//
// Notes:
//    stmt may be nullptr during local assertion prop
//
GenTree* Compiler::optCopyAssertionProp(AssertionDsc*        curAssertion,
                                        GenTreeLclVarCommon* tree,
                                        Statement* stmt DEBUGARG(AssertionIndex index))
{
    const AssertionDsc::AssertionDscOp1& op1 = curAssertion->op1;
    const AssertionDsc::AssertionDscOp2& op2 = curAssertion->op2;

    noway_assert(op1.lcl.lclNum != op2.lcl.lclNum);

    const unsigned lclNum = tree->GetLclNum();

    // Make sure one of the lclNum of the assertion matches with that of the tree.
    if (op1.lcl.lclNum != lclNum && op2.lcl.lclNum != lclNum)
    {
        return nullptr;
    }

    // Extract the matching lclNum and ssaNum.
    const unsigned copyLclNum = (op1.lcl.lclNum == lclNum) ? op2.lcl.lclNum : op1.lcl.lclNum;
    unsigned       copySsaNum = BAD_VAR_NUM;
    if (!optLocalAssertionProp)
    {
        // Extract the ssaNum of the matching lclNum.
        unsigned ssaNum = (op1.lcl.lclNum == lclNum) ? op1.lcl.ssaNum : op2.lcl.ssaNum;
        copySsaNum      = (op1.lcl.lclNum == lclNum) ? op2.lcl.ssaNum : op1.lcl.ssaNum;

        if (ssaNum != tree->GetSsaNum())
        {
            return nullptr;
        }
    }

    LclVarDsc* const copyVarDsc = lvaGetDesc(copyLclNum);
    LclVarDsc* const lclVarDsc  = lvaGetDesc(lclNum);

    // Make sure the types are compatible.
    if (!optAssertionProp_LclVarTypeCheck(tree, lclVarDsc, copyVarDsc))
    {
        return nullptr;
    }

    // Make sure we can perform this copy prop.
    if (optCopyProp_LclVarScore(lclVarDsc, copyVarDsc, curAssertion->op1.lcl.lclNum == lclNum) <= 0)
    {
        return nullptr;
    }

    tree->SetSsaNum(copySsaNum);
    tree->SetLclNum(copyLclNum);

#ifdef DEBUG
    if (verbose)
    {
        printf("\nAssertion prop in " FMT_BB ":\n", compCurBB->bbNum);
        optPrintAssertion(curAssertion, index);
        gtDispTree(tree, nullptr, nullptr, true);
    }
#endif

    // Update and morph the tree.
    return optAssertionProp_Update(tree, tree, stmt);
}

//------------------------------------------------------------------------
// optAssertionProp_LclVar: try and optimize a local var use via assertions
//
// Arguments:
//    assertions - set of live assertions
//    tree       - local use to optimize
//    stmt       - statement containing the tree
//
// Returns:
//    Updated tree, or nullptr
//
// Notes:
//   stmt may be nullptr during local assertion prop
//
GenTree* Compiler::optAssertionProp_LclVar(ASSERT_VALARG_TP assertions, GenTreeLclVarCommon* tree, Statement* stmt)
{
    // If we have a var definition then bail or
    // If this is the address of the var then it will have the GTF_DONT_CSE
    // flag set and we don't want to to assertion prop on it.
    if (tree->gtFlags & (GTF_VAR_DEF | GTF_DONT_CSE))
    {
        return nullptr;
    }

    BitVecOps::Iter iter(apTraits, assertions);
    unsigned        index = 0;
    while (iter.NextElem(&index))
    {
        AssertionIndex assertionIndex = GetAssertionIndex(index);
        if (assertionIndex > optAssertionCount)
        {
            break;
        }
        // See if the variable is equal to a constant or another variable.
        AssertionDsc* curAssertion = optGetAssertion(assertionIndex);
        if (curAssertion->assertionKind != OAK_EQUAL || curAssertion->op1.kind != O1K_LCLVAR)
        {
            continue;
        }

        // Copy prop.
        if (curAssertion->op2.kind == O2K_LCLVAR_COPY)
        {
            // Cannot do copy prop during global assertion prop because of no knowledge
            // of kill sets. We will still make a == b copy assertions during the global phase to allow
            // for any implied assertions that can be retrieved. Because implied assertions look for
            // matching SSA numbers (i.e., if a0 == b1 and b1 == c0 then a0 == c0) they don't need kill sets.
            if (optLocalAssertionProp)
            {
                // Perform copy assertion prop.
                GenTree* newTree = optCopyAssertionProp(curAssertion, tree, stmt DEBUGARG(assertionIndex));
                if (newTree != nullptr)
                {
                    return newTree;
                }
            }

            continue;
        }

        // Constant prop.
        //
        // The case where the tree type could be different than the LclVar type is caused by
        // gtFoldExpr, specifically the case of a cast, where the fold operation changes the type of the LclVar
        // node.  In such a case is not safe to perform the substitution since later on the JIT will assert mismatching
        // types between trees.
        const unsigned lclNum = tree->GetLclNum();
        if (curAssertion->op1.lcl.lclNum == lclNum)
        {
            LclVarDsc* const lclDsc = lvaGetDesc(lclNum);
            // Verify types match
            if (tree->TypeGet() == lclDsc->lvType)
            {
                // If local assertion prop, just perform constant prop.
                if (optLocalAssertionProp)
                {
                    return optConstantAssertionProp(curAssertion, tree, stmt DEBUGARG(assertionIndex));
                }

                // If global assertion, perform constant propagation only if the VN's match.
                if (curAssertion->op1.vn == vnStore->VNConservativeNormalValue(tree->gtVNPair))
                {
                    return optConstantAssertionProp(curAssertion, tree, stmt DEBUGARG(assertionIndex));
                }
            }
        }
    }

    return nullptr;
}

/*****************************************************************************
 *
 *  Given a set of "assertions" to search, find an assertion that matches
 *  op1Kind and lclNum, op2Kind and the constant value and is either equal or
 *  not equal assertion.
 */
AssertionIndex Compiler::optLocalAssertionIsEqualOrNotEqual(
    optOp1Kind op1Kind, unsigned lclNum, optOp2Kind op2Kind, ssize_t cnsVal, ASSERT_VALARG_TP assertions)
{
    noway_assert((op1Kind == O1K_LCLVAR) || (op1Kind == O1K_EXACT_TYPE) || (op1Kind == O1K_SUBTYPE));
    noway_assert((op2Kind == O2K_CONST_INT) || (op2Kind == O2K_IND_CNS_INT));
    if (!optLocalAssertionProp && BitVecOps::IsEmpty(apTraits, assertions))
    {
        return NO_ASSERTION_INDEX;
    }

    for (AssertionIndex index = 1; index <= optAssertionCount; ++index)
    {
        AssertionDsc* curAssertion = optGetAssertion(index);
        if (optLocalAssertionProp || BitVecOps::IsMember(apTraits, assertions, index - 1))
        {
            if ((curAssertion->assertionKind != OAK_EQUAL) && (curAssertion->assertionKind != OAK_NOT_EQUAL))
            {
                continue;
            }

            if ((curAssertion->op1.kind == op1Kind) && (curAssertion->op1.lcl.lclNum == lclNum) &&
                (curAssertion->op2.kind == op2Kind))
            {
                bool constantIsEqual  = (curAssertion->op2.u1.iconVal == cnsVal);
                bool assertionIsEqual = (curAssertion->assertionKind == OAK_EQUAL);

                if (constantIsEqual || assertionIsEqual)
                {
                    return index;
                }
            }
        }
    }
    return NO_ASSERTION_INDEX;
}

//------------------------------------------------------------------------
// optGlobalAssertionIsEqualOrNotEqual: Look for an assertion in the specified
//        set that is one of op1 == op1, op1 != op2, or *op1 == op2,
//        where equality is based on value numbers.
//
// Arguments:
//      assertions: bit vector describing set of assertions
//      op1, op2:    the treen nodes in question
//
// Returns:
//      Index of first matching assertion, or NO_ASSERTION_INDEX if no
//      assertions in the set are matches.
//
// Notes:
//      Assertions based on *op1 are the result of exact type tests and are
//      only returned when op1 is a local var with ref type and the assertion
//      is an exact type equality.
//
AssertionIndex Compiler::optGlobalAssertionIsEqualOrNotEqual(ASSERT_VALARG_TP assertions, GenTree* op1, GenTree* op2)
{
    if (BitVecOps::IsEmpty(apTraits, assertions))
    {
        return NO_ASSERTION_INDEX;
    }
    BitVecOps::Iter iter(apTraits, assertions);
    unsigned        index = 0;
    while (iter.NextElem(&index))
    {
        AssertionIndex assertionIndex = GetAssertionIndex(index);
        if (assertionIndex > optAssertionCount)
        {
            break;
        }
        AssertionDsc* curAssertion = optGetAssertion(assertionIndex);
        if ((curAssertion->assertionKind != OAK_EQUAL && curAssertion->assertionKind != OAK_NOT_EQUAL))
        {
            continue;
        }

        if ((curAssertion->op1.vn == vnStore->VNConservativeNormalValue(op1->gtVNPair)) &&
            (curAssertion->op2.vn == vnStore->VNConservativeNormalValue(op2->gtVNPair)))
        {
            return assertionIndex;
        }

        // Look for matching exact type assertions based on vtable accesses
        if ((curAssertion->assertionKind == OAK_EQUAL) && (curAssertion->op1.kind == O1K_EXACT_TYPE) &&
            op1->OperIs(GT_IND))
        {
            GenTree* indirAddr = op1->AsIndir()->Addr();

            if (indirAddr->OperIs(GT_LCL_VAR) && (indirAddr->TypeGet() == TYP_REF))
            {
                // op1 is accessing vtable of a ref type local var
                if ((curAssertion->op1.vn == vnStore->VNConservativeNormalValue(indirAddr->gtVNPair)) &&
                    (curAssertion->op2.vn == vnStore->VNConservativeNormalValue(op2->gtVNPair)))
                {
                    return assertionIndex;
                }
            }
        }
    }
    return NO_ASSERTION_INDEX;
}

/*****************************************************************************
 *
 *  Given a set of "assertions" to search for, find an assertion that is either
 *  op == 0 or op != 0
 *
 */
AssertionIndex Compiler::optGlobalAssertionIsEqualOrNotEqualZero(ASSERT_VALARG_TP assertions, GenTree* op1)
{
    if (BitVecOps::IsEmpty(apTraits, assertions))
    {
        return NO_ASSERTION_INDEX;
    }
    BitVecOps::Iter iter(apTraits, assertions);
    unsigned        index = 0;
    while (iter.NextElem(&index))
    {
        AssertionIndex assertionIndex = GetAssertionIndex(index);
        if (assertionIndex > optAssertionCount)
        {
            break;
        }
        AssertionDsc* curAssertion = optGetAssertion(assertionIndex);
        if ((curAssertion->assertionKind != OAK_EQUAL && curAssertion->assertionKind != OAK_NOT_EQUAL))
        {
            continue;
        }

        if ((curAssertion->op1.vn == vnStore->VNConservativeNormalValue(op1->gtVNPair)) &&
            (curAssertion->op2.vn == vnStore->VNZeroForType(op1->TypeGet())))
        {
            return assertionIndex;
        }
    }
    return NO_ASSERTION_INDEX;
}

/*****************************************************************************
 *
 *  Given a tree consisting of a RelOp and a set of available assertions
 *  we try to propagate an assertion and modify the RelOp tree if we can.
 *  We pass in the root of the tree via 'stmt', for local copy prop 'stmt' will be nullptr
 *  Returns the modified tree, or nullptr if no assertion prop took place
 */

GenTree* Compiler::optAssertionProp_RelOp(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt)
{
    assert(tree->OperKind() & GTK_RELOP);

    if (!optLocalAssertionProp)
    {
        // If global assertion prop then use value numbering.
        return optAssertionPropGlobal_RelOp(assertions, tree, stmt);
    }

    //
    // Currently only GT_EQ or GT_NE are supported Relops for local AssertionProp
    //

    if ((tree->gtOper != GT_EQ) && (tree->gtOper != GT_NE))
    {
        return nullptr;
    }

    // If local assertion prop then use variable based prop.
    return optAssertionPropLocal_RelOp(assertions, tree, stmt);
}

//------------------------------------------------------------------------
// optAssertionProp: try and optimize a relop via assertion propagation
//
// Arguments:
//   assertions  - set of live assertions
//   tree        - tree to possibly optimize
//   stmt        - statement containing the tree
//
// Returns:
//   The modified tree, or nullptr if no assertion prop took place.
//
GenTree* Compiler::optAssertionPropGlobal_RelOp(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt)
{
    GenTree* newTree = tree;
    GenTree* op1     = tree->AsOp()->gtOp1;
    GenTree* op2     = tree->AsOp()->gtOp2;

    // Look for assertions of the form (tree EQ/NE 0)
    AssertionIndex index = optGlobalAssertionIsEqualOrNotEqualZero(assertions, tree);

    if (index != NO_ASSERTION_INDEX)
    {
        // We know that this relop is either 0 or != 0 (1)
        AssertionDsc* curAssertion = optGetAssertion(index);

#ifdef DEBUG
        if (verbose)
        {
            printf("\nVN relop based constant assertion prop in " FMT_BB ":\n", compCurBB->bbNum);
            printf("Assertion index=#%02u: ", index);
            printTreeID(tree);
            printf(" %s 0\n", (curAssertion->assertionKind == OAK_EQUAL) ? "==" : "!=");
        }
#endif

        // Bail out if tree is not side effect free.
        if ((tree->gtFlags & GTF_SIDE_EFFECT) != 0)
        {
            JITDUMP("sorry, blocked by side effects\n");
            return nullptr;
        }

        if (curAssertion->assertionKind == OAK_EQUAL)
        {
            tree->ChangeOperConst(GT_CNS_INT);
            tree->AsIntCon()->gtIconVal = 0;
        }
        else
        {
            tree->ChangeOperConst(GT_CNS_INT);
            tree->AsIntCon()->gtIconVal = 1;
        }

        newTree = fgMorphTree(tree);
        DISPTREE(newTree);
        return optAssertionProp_Update(newTree, tree, stmt);
    }

    // Else check if we have an equality check involving a local or an indir
    if (!tree->OperIs(GT_EQ, GT_NE))
    {
        return nullptr;
    }

    // Bail out if tree is not side effect free.
    if ((tree->gtFlags & GTF_SIDE_EFFECT) != 0)
    {
        return nullptr;
    }

    if (!op1->OperIs(GT_LCL_VAR, GT_IND))
    {
        return nullptr;
    }

    // Find an equal or not equal assertion involving "op1" and "op2".
    index = optGlobalAssertionIsEqualOrNotEqual(assertions, op1, op2);

    if (index == NO_ASSERTION_INDEX)
    {
        return nullptr;
    }

    AssertionDsc* curAssertion         = optGetAssertion(index);
    bool          assertionKindIsEqual = (curAssertion->assertionKind == OAK_EQUAL);

    // Allow or not to reverse condition for OAK_NOT_EQUAL assertions.
    bool allowReverse = true;

    // If the assertion involves "op2" and it is a constant, then check if "op1" also has a constant value.
    ValueNum vnCns = vnStore->VNConservativeNormalValue(op2->gtVNPair);
    if (vnStore->IsVNConstant(vnCns))
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("\nVN relop based constant assertion prop in " FMT_BB ":\n", compCurBB->bbNum);
            printf("Assertion index=#%02u: ", index);
            printTreeID(op1);
            printf(" %s ", assertionKindIsEqual ? "==" : "!=");
            if (genActualType(op1->TypeGet()) == TYP_INT)
            {
                printf("%d\n", vnStore->ConstantValue<int>(vnCns));
            }
            else if (op1->TypeGet() == TYP_LONG)
            {
                printf("%I64d\n", vnStore->ConstantValue<INT64>(vnCns));
            }
            else if (op1->TypeGet() == TYP_DOUBLE)
            {
                printf("%f\n", vnStore->ConstantValue<double>(vnCns));
            }
            else if (op1->TypeGet() == TYP_FLOAT)
            {
                printf("%f\n", vnStore->ConstantValue<float>(vnCns));
            }
            else if (op1->TypeGet() == TYP_REF)
            {
                // The only constant of TYP_REF that ValueNumbering supports is 'null'
                assert(vnStore->ConstantValue<size_t>(vnCns) == 0);
                printf("null\n");
            }
            else
            {
                printf("??unknown\n");
            }
            gtDispTree(tree, nullptr, nullptr, true);
        }
#endif
        // Change the oper to const.
        if (genActualType(op1->TypeGet()) == TYP_INT)
        {
            op1->ChangeOperConst(GT_CNS_INT);
            op1->AsIntCon()->gtIconVal = vnStore->ConstantValue<int>(vnCns);

            if (vnStore->IsVNHandle(vnCns))
            {
                op1->gtFlags |= (vnStore->GetHandleFlags(vnCns) & GTF_ICON_HDL_MASK);
            }
        }
        else if (op1->TypeGet() == TYP_LONG)
        {
            op1->ChangeOperConst(GT_CNS_NATIVELONG);
            op1->AsIntConCommon()->SetLngValue(vnStore->ConstantValue<INT64>(vnCns));

            if (vnStore->IsVNHandle(vnCns))
            {
                op1->gtFlags |= (vnStore->GetHandleFlags(vnCns) & GTF_ICON_HDL_MASK);
            }
        }
        else if (op1->TypeGet() == TYP_DOUBLE)
        {
            double constant = vnStore->ConstantValue<double>(vnCns);
            op1->ChangeOperConst(GT_CNS_DBL);
            op1->AsDblCon()->gtDconVal = constant;

            // Nothing can be equal to NaN. So if IL had "op1 == NaN", then we already made op1 NaN,
            // which will yield a false correctly. Instead if IL had "op1 != NaN", then we already
            // made op1 NaN which will yield a true correctly. Note that this is irrespective of the
            // assertion we have made.
            allowReverse = (_isnan(constant) == 0);
        }
        else if (op1->TypeGet() == TYP_FLOAT)
        {
            float constant = vnStore->ConstantValue<float>(vnCns);
            op1->ChangeOperConst(GT_CNS_DBL);
            op1->AsDblCon()->gtDconVal = constant;
            // See comments for TYP_DOUBLE.
            allowReverse = (_isnan(constant) == 0);
        }
        else if (op1->TypeGet() == TYP_REF)
        {
            op1->ChangeOperConst(GT_CNS_INT);
            // The only constant of TYP_REF that ValueNumbering supports is 'null'
            noway_assert(vnStore->ConstantValue<size_t>(vnCns) == 0);
            op1->AsIntCon()->gtIconVal = 0;
        }
        else
        {
            noway_assert(!"unknown type in Global_RelOp");
        }

        op1->gtVNPair.SetBoth(vnCns); // Preserve the ValueNumPair, as ChangeOperConst/SetOper will clear it.

        // set foldResult to either 0 or 1
        bool foldResult = assertionKindIsEqual;
        if (tree->gtOper == GT_NE)
        {
            foldResult = !foldResult;
        }

        // Set the value number on the relop to 1 (true) or 0 (false)
        if (foldResult)
        {
            tree->gtVNPair.SetBoth(vnStore->VNOneForType(TYP_INT));
        }
        else
        {
            tree->gtVNPair.SetBoth(vnStore->VNZeroForType(TYP_INT));
        }
    }
    // If the assertion involves "op2" and "op1" is also a local var, then just morph the tree.
    else if (op2->gtOper == GT_LCL_VAR)
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("\nVN relop based copy assertion prop in " FMT_BB ":\n", compCurBB->bbNum);
            printf("Assertion index=#%02u: V%02d.%02d %s V%02d.%02d\n", index, op1->AsLclVar()->GetLclNum(),
                   op1->AsLclVar()->GetSsaNum(), (curAssertion->assertionKind == OAK_EQUAL) ? "==" : "!=",
                   op2->AsLclVar()->GetLclNum(), op2->AsLclVar()->GetSsaNum());
            gtDispTree(tree, nullptr, nullptr, true);
        }
#endif
        // If floating point, don't just substitute op1 with op2, this won't work if
        // op2 is NaN. Just turn it into a "true" or "false" yielding expression.
        if (op1->TypeGet() == TYP_DOUBLE || op1->TypeGet() == TYP_FLOAT)
        {
            // Note we can't trust the OAK_EQUAL as the value could end up being a NaN
            // violating the assertion. However, we create OAK_EQUAL assertions for floating
            // point only on JTrue nodes, so if the condition held earlier, it will hold
            // now. We don't create OAK_EQUAL assertion on floating point from GT_ASG
            // because we depend on value num which would constant prop the NaN.
            op1->ChangeOperConst(GT_CNS_DBL);
            op1->AsDblCon()->gtDconVal = 0;
            op2->ChangeOperConst(GT_CNS_DBL);
            op2->AsDblCon()->gtDconVal = 0;
        }
        // Change the op1 LclVar to the op2 LclVar
        else
        {
            noway_assert(varTypeIsIntegralOrI(op1->TypeGet()));
            op1->AsLclVarCommon()->SetLclNum(op2->AsLclVarCommon()->GetLclNum());
            op1->AsLclVarCommon()->SetSsaNum(op2->AsLclVarCommon()->GetSsaNum());
        }
    }
    else
    {
        return nullptr;
    }

    // Finally reverse the condition, if we have a not equal assertion.
    if (allowReverse && curAssertion->assertionKind == OAK_NOT_EQUAL)
    {
        gtReverseCond(tree);
    }

    newTree = fgMorphTree(tree);

#ifdef DEBUG
    if (verbose)
    {
        gtDispTree(newTree, nullptr, nullptr, true);
    }
#endif

    return optAssertionProp_Update(newTree, tree, stmt);
}

/*************************************************************************************
 *
 *  Given the set of "assertions" to look up a relop assertion about the relop "tree",
 *  perform local variable name based relop assertion propagation on the tree.
 *
 */
GenTree* Compiler::optAssertionPropLocal_RelOp(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt)
{
    assert(tree->OperGet() == GT_EQ || tree->OperGet() == GT_NE);

    GenTree* op1 = tree->AsOp()->gtOp1;
    GenTree* op2 = tree->AsOp()->gtOp2;

    // For Local AssertionProp we only can fold when op1 is a GT_LCL_VAR
    if (op1->gtOper != GT_LCL_VAR)
    {
        return nullptr;
    }

    // For Local AssertionProp we only can fold when op2 is a GT_CNS_INT
    if (op2->gtOper != GT_CNS_INT)
    {
        return nullptr;
    }

    optOp1Kind op1Kind = O1K_LCLVAR;
    optOp2Kind op2Kind = O2K_CONST_INT;
    ssize_t    cnsVal  = op2->AsIntCon()->gtIconVal;
    var_types  cmpType = op1->TypeGet();

    // Don't try to fold/optimize Floating Compares; there are multiple zero values.
    if (varTypeIsFloating(cmpType))
    {
        return nullptr;
    }

    // Find an equal or not equal assertion about op1 var.
    unsigned lclNum = op1->AsLclVarCommon()->GetLclNum();
    noway_assert(lclNum < lvaCount);
    AssertionIndex index = optLocalAssertionIsEqualOrNotEqual(op1Kind, lclNum, op2Kind, cnsVal, assertions);

    if (index == NO_ASSERTION_INDEX)
    {
        return nullptr;
    }

    AssertionDsc* curAssertion = optGetAssertion(index);

    bool assertionKindIsEqual = (curAssertion->assertionKind == OAK_EQUAL);
    bool constantIsEqual      = false;

    if (genTypeSize(cmpType) == TARGET_POINTER_SIZE)
    {
        constantIsEqual = (curAssertion->op2.u1.iconVal == cnsVal);
    }
#ifdef TARGET_64BIT
    else if (genTypeSize(cmpType) == sizeof(INT32))
    {
        // Compare the low 32-bits only
        constantIsEqual = (((INT32)curAssertion->op2.u1.iconVal) == ((INT32)cnsVal));
    }
#endif
    else
    {
        // We currently don't fold/optimize when the GT_LCL_VAR has been cast to a small type
        return nullptr;
    }

    noway_assert(constantIsEqual || assertionKindIsEqual);

#ifdef DEBUG
    if (verbose)
    {
        printf("\nAssertion prop for index #%02u in " FMT_BB ":\n", index, compCurBB->bbNum);
        gtDispTree(tree, nullptr, nullptr, true);
    }
#endif

    // Return either CNS_INT 0 or CNS_INT 1.
    bool foldResult = (constantIsEqual == assertionKindIsEqual);
    if (tree->gtOper == GT_NE)
    {
        foldResult = !foldResult;
    }

    op2->AsIntCon()->gtIconVal = foldResult;
    op2->gtType                = TYP_INT;

    return optAssertionProp_Update(op2, tree, stmt);
}

/*****************************************************************************
 *
 *  Given a tree consisting of a Cast and a set of available assertions
 *  we try to propagate an assertion and modify the Cast tree if we can.
 *  We pass in the root of the tree via 'stmt', for local copy prop 'stmt'
 *  will be nullptr.
 *
 *  Returns the modified tree, or nullptr if no assertion prop took place.
 */
GenTree* Compiler::optAssertionProp_Cast(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt)
{
    assert(tree->gtOper == GT_CAST);

    var_types fromType = tree->CastFromType();
    var_types toType   = tree->AsCast()->gtCastType;
    GenTree*  op1      = tree->AsCast()->CastOp();

    // force the fromType to unsigned if GT_UNSIGNED flag is set
    if (tree->IsUnsigned())
    {
        fromType = genUnsignedType(fromType);
    }

    // If we have a cast involving floating point types, then bail.
    if (varTypeIsFloating(toType) || varTypeIsFloating(fromType))
    {
        return nullptr;
    }

    GenTree* lcl = op1->SkipComma();

    if (!lcl->OperIs(GT_LCL_VAR))
    {
        return nullptr;
    }

    AssertionIndex index = optAssertionIsSubrange(lcl, fromType, toType, assertions);
    if (index != NO_ASSERTION_INDEX)
    {
        LclVarDsc* varDsc = &lvaTable[lcl->AsLclVarCommon()->GetLclNum()];
        if (varDsc->lvNormalizeOnLoad() || varTypeIsLong(varDsc->TypeGet()))
        {
            // For normalize on load variables it must be a narrowing cast to remove
            if (genTypeSize(toType) > genTypeSize(varDsc->TypeGet()))
            {
                // Can we just remove the GTF_OVERFLOW flag?
                if ((tree->gtFlags & GTF_OVERFLOW) == 0)
                {
                    return nullptr;
                }
                else
                {

#ifdef DEBUG
                    if (verbose)
                    {
                        printf("\nSubrange prop for index #%02u in " FMT_BB ":\n", index, compCurBB->bbNum);
                        gtDispTree(tree, nullptr, nullptr, true);
                    }
#endif
                    tree->gtFlags &= ~GTF_OVERFLOW; // This cast cannot overflow
                    return optAssertionProp_Update(tree, tree, stmt);
                }
            }

            //             GT_CAST   long -> uint -> int
            //                |
            //           GT_LCL_VAR long
            //
            // Where the lclvar is known to be in the range of [0..MAX_UINT]
            //
            // A load of a 32-bit unsigned int is the same as a load of a 32-bit signed int
            //
            if (toType == TYP_UINT)
            {
                toType = TYP_INT;
            }

            // Change the "lcl" type to match what the cast wanted, by propagating the type
            // change down the comma nodes leading to the "lcl", if we skipped them earlier.
            GenTree* tmp = op1;
            while (tmp->gtOper == GT_COMMA)
            {
                tmp->gtType = toType;
                tmp         = tmp->AsOp()->gtOp2;
            }
            noway_assert(tmp == lcl);
            tmp->gtType = toType;
        }

#ifdef DEBUG
        if (verbose)
        {
            printf("\nSubrange prop for index #%02u in " FMT_BB ":\n", index, compCurBB->bbNum);
            gtDispTree(tree, nullptr, nullptr, true);
        }
#endif
        return optAssertionProp_Update(op1, tree, stmt);
    }
    return nullptr;
}

/*****************************************************************************
 *
 *  Given a tree with an array bounds check node, eliminate it because it was
 *  checked already in the program.
 */
GenTree* Compiler::optAssertionProp_Comma(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt)
{
    // Remove the bounds check as part of the GT_COMMA node since we need parent pointer to remove nodes.
    // When processing visits the bounds check, it sets the throw kind to None if the check is redundant.
    if ((tree->gtGetOp1()->OperGet() == GT_ARR_BOUNDS_CHECK) &&
        ((tree->gtGetOp1()->gtFlags & GTF_ARR_BOUND_INBND) != 0))
    {
        optRemoveCommaBasedRangeCheck(tree, stmt);
        return optAssertionProp_Update(tree, tree, stmt);
    }
    return nullptr;
}

//------------------------------------------------------------------------
// optAssertionProp_Ind: see if we can prove the indirection can't cause
//    and exception.
//
// Arguments:
//   assertions  - set of live assertions
//   tree        - tree to possibly optimize
//   stmt        - statement containing the tree
//
// Returns:
//   The modified tree, or nullptr if no assertion prop took place.
//
// Notes:
//   stmt may be nullptr during local assertion prop
//
GenTree* Compiler::optAssertionProp_Ind(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt)
{
    assert(tree->OperIsIndir());

    if (!(tree->gtFlags & GTF_EXCEPT))
    {
        return nullptr;
    }

    // Check for add of a constant.
    GenTree* op1 = tree->AsIndir()->Addr();
    if ((op1->gtOper == GT_ADD) && (op1->AsOp()->gtOp2->gtOper == GT_CNS_INT))
    {
        op1 = op1->AsOp()->gtOp1;
    }

    if (op1->gtOper != GT_LCL_VAR)
    {
        return nullptr;
    }

#ifdef DEBUG
    bool           vnBased = false;
    AssertionIndex index   = NO_ASSERTION_INDEX;
#endif
    if (optAssertionIsNonNull(op1, assertions DEBUGARG(&vnBased) DEBUGARG(&index)))
    {
#ifdef DEBUG
        if (verbose)
        {
            (vnBased) ? printf("\nVN based non-null prop in " FMT_BB ":\n", compCurBB->bbNum)
                      : printf("\nNon-null prop for index #%02u in " FMT_BB ":\n", index, compCurBB->bbNum);
            gtDispTree(tree, nullptr, nullptr, true);
        }
#endif
        tree->gtFlags &= ~GTF_EXCEPT;
        tree->gtFlags |= GTF_IND_NONFAULTING;

        // Set this flag to prevent reordering
        tree->gtFlags |= GTF_ORDER_SIDEEFF;

        return optAssertionProp_Update(tree, tree, stmt);
    }

    return nullptr;
}

//------------------------------------------------------------------------
// optAssertionIsNonNull: see if we can prove a tree's value will be non-null
//   based on assertions
//
// Arguments:
//   op - tree to check
//   assertions  - set of live assertions
//   pVnBased - [out] set to true if value numbers were used
//   pIndex - [out] the assertion used in the proof
//
// Returns:
//   true if the tree's value will be non-null
//
// Notes:
//   Sets "pVnBased" if the assertion is value number based. If no matching
//    assertions are found from the table, then returns "NO_ASSERTION_INDEX."
//
//   If both VN and assertion table yield a matching assertion, "pVnBased"
//   is only set and the return value is "NO_ASSERTION_INDEX."
//
bool Compiler::optAssertionIsNonNull(GenTree*         op,
                                     ASSERT_VALARG_TP assertions DEBUGARG(bool* pVnBased)
                                         DEBUGARG(AssertionIndex* pIndex))
{
    bool vnBased = (!optLocalAssertionProp && vnStore->IsKnownNonNull(op->gtVNPair.GetConservative()));
#ifdef DEBUG
    *pVnBased = vnBased;
#endif

    if (vnBased)
    {
#ifdef DEBUG
        *pIndex = NO_ASSERTION_INDEX;
#endif
        return true;
    }

    AssertionIndex index = optAssertionIsNonNullInternal(op, assertions DEBUGARG(pVnBased));
#ifdef DEBUG
    *pIndex = index;
#endif
    return index != NO_ASSERTION_INDEX;
}

//------------------------------------------------------------------------
// optAssertionIsNonNullInternal: see if we can prove a tree's value will
//   be non-null based on assertions
//
// Arguments:
//   op - tree to check
//   assertions  - set of live assertions
//   pVnBased - [out] set to true if value numbers were used
//
// Returns:
//   index of assertion, or NO_ASSERTION_INDEX
//
AssertionIndex Compiler::optAssertionIsNonNullInternal(GenTree*         op,
                                                       ASSERT_VALARG_TP assertions DEBUGARG(bool* pVnBased))
{

#ifdef DEBUG
    // Initialize the out param
    //
    *pVnBased = false;
#endif

    // If local assertion prop use lcl comparison, else use VN comparison.
    if (!optLocalAssertionProp)
    {
        if (BitVecOps::MayBeUninit(assertions) || BitVecOps::IsEmpty(apTraits, assertions))
        {
            return NO_ASSERTION_INDEX;
        }

        // Look at both the top-level vn, and
        // the vn we get by stripping off any constant adds.
        //
        ValueNum  vn     = vnStore->VNConservativeNormalValue(op->gtVNPair);
        ValueNum  vnBase = vn;
        VNFuncApp funcAttr;

        while (vnStore->GetVNFunc(vnBase, &funcAttr) && (funcAttr.m_func == (VNFunc)GT_ADD))
        {
            if (vnStore->IsVNConstant(funcAttr.m_args[1]) && varTypeIsIntegral(vnStore->TypeOfVN(funcAttr.m_args[1])))
            {
                vnBase = funcAttr.m_args[0];
            }
            else if (vnStore->IsVNConstant(funcAttr.m_args[0]) &&
                     varTypeIsIntegral(vnStore->TypeOfVN(funcAttr.m_args[0])))
            {
                vnBase = funcAttr.m_args[1];
            }
            else
            {
                break;
            }
        }

        // Check each assertion to find if we have a vn != null assertion.
        //
        BitVecOps::Iter iter(apTraits, assertions);
        unsigned        index = 0;
        while (iter.NextElem(&index))
        {
            AssertionIndex assertionIndex = GetAssertionIndex(index);
            if (assertionIndex > optAssertionCount)
            {
                break;
            }
            AssertionDsc* curAssertion = optGetAssertion(assertionIndex);
            if (curAssertion->assertionKind != OAK_NOT_EQUAL)
            {
                continue;
            }

            if (curAssertion->op2.vn != ValueNumStore::VNForNull())
            {
                continue;
            }

            if ((curAssertion->op1.vn != vn) && (curAssertion->op1.vn != vnBase))
            {
                continue;
            }

#ifdef DEBUG
            *pVnBased = true;
#endif

            return assertionIndex;
        }
    }
    else
    {
        unsigned lclNum = op->AsLclVarCommon()->GetLclNum();
        // Check each assertion to find if we have a variable == or != null assertion.
        for (AssertionIndex index = 1; index <= optAssertionCount; index++)
        {
            AssertionDsc* curAssertion = optGetAssertion(index);
            if ((curAssertion->assertionKind == OAK_NOT_EQUAL) && // kind
                (curAssertion->op1.kind == O1K_LCLVAR) &&         // op1
                (curAssertion->op2.kind == O2K_CONST_INT) &&      // op2
                (curAssertion->op1.lcl.lclNum == lclNum) && (curAssertion->op2.u1.iconVal == 0))
            {
                return index;
            }
        }
    }
    return NO_ASSERTION_INDEX;
}
/*****************************************************************************
 *
 *  Given a tree consisting of a call and a set of available assertions, we
 *  try to propagate a non-null assertion and modify the Call tree if we can.
 *  Returns the modified tree, or nullptr if no assertion prop took place.
 *
 */
GenTree* Compiler::optNonNullAssertionProp_Call(ASSERT_VALARG_TP assertions, GenTreeCall* call)
{
    if ((call->gtFlags & GTF_CALL_NULLCHECK) == 0)
    {
        return nullptr;
    }
    GenTree* op1 = gtGetThisArg(call);
    noway_assert(op1 != nullptr);
    if (op1->gtOper != GT_LCL_VAR)
    {
        return nullptr;
    }

#ifdef DEBUG
    bool           vnBased = false;
    AssertionIndex index   = NO_ASSERTION_INDEX;
#endif
    if (optAssertionIsNonNull(op1, assertions DEBUGARG(&vnBased) DEBUGARG(&index)))
    {
#ifdef DEBUG
        if (verbose)
        {
            (vnBased) ? printf("\nVN based non-null prop in " FMT_BB ":\n", compCurBB->bbNum)
                      : printf("\nNon-null prop for index #%02u in " FMT_BB ":\n", index, compCurBB->bbNum);
            gtDispTree(call, nullptr, nullptr, true);
        }
#endif
        call->gtFlags &= ~GTF_CALL_NULLCHECK;
        call->gtFlags &= ~GTF_EXCEPT;
        noway_assert(call->gtFlags & GTF_SIDE_EFFECT);
        return call;
    }
    return nullptr;
}

/*****************************************************************************
 *
 *  Given a tree consisting of a call and a set of available assertions, we
 *  try to propagate an assertion and modify the Call tree if we can. Our
 *  current modifications are limited to removing the nullptrCHECK flag from
 *  the call.
 *  We pass in the root of the tree via 'stmt', for local copy prop 'stmt'
 *  will be nullptr. Returns the modified tree, or nullptr if no assertion prop
 *  took place.
 *
 */

GenTree* Compiler::optAssertionProp_Call(ASSERT_VALARG_TP assertions, GenTreeCall* call, Statement* stmt)
{
    if (optNonNullAssertionProp_Call(assertions, call))
    {
        return optAssertionProp_Update(call, call, stmt);
    }
    else if (!optLocalAssertionProp && (call->gtCallType == CT_HELPER))
    {
        if (call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_ISINSTANCEOFINTERFACE) ||
            call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_ISINSTANCEOFARRAY) ||
            call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_ISINSTANCEOFCLASS) ||
            call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_ISINSTANCEOFANY) ||
            call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_CHKCASTINTERFACE) ||
            call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_CHKCASTARRAY) ||
            call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_CHKCASTCLASS) ||
            call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_CHKCASTANY) ||
            call->gtCallMethHnd == eeFindHelper(CORINFO_HELP_CHKCASTCLASS_SPECIAL))
        {
            GenTree* arg1 = call->GetArgNodeByArgNum(1);
            if (arg1->gtOper != GT_LCL_VAR)
            {
                return nullptr;
            }

            GenTree* arg2 = call->GetArgNodeByArgNum(0);

            unsigned index = optAssertionIsSubtype(arg1, arg2, assertions);
            if (index != NO_ASSERTION_INDEX)
            {
#ifdef DEBUG
                if (verbose)
                {
                    printf("\nDid VN based subtype prop for index #%02u in " FMT_BB ":\n", index, compCurBB->bbNum);
                    gtDispTree(call, nullptr, nullptr, true);
                }
#endif
                GenTree* list = nullptr;
                gtExtractSideEffList(call, &list, GTF_SIDE_EFFECT, true);
                if (list != nullptr)
                {
                    arg1 = gtNewCommaNode(list, arg1);
                    fgSetTreeSeq(arg1);
                }

                return optAssertionProp_Update(arg1, call, stmt);
            }
        }
    }

    return nullptr;
}

/*****************************************************************************
 *
 *  Given a tree with a bounds check, remove it if it has already been checked in the program flow.
 */
GenTree* Compiler::optAssertionProp_BndsChk(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt)
{
    if (optLocalAssertionProp)
    {
        return nullptr;
    }

    assert(tree->gtOper == GT_ARR_BOUNDS_CHECK);

#ifdef FEATURE_ENABLE_NO_RANGE_CHECKS
    if (JitConfig.JitNoRangeChks())
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("\nFlagging check redundant due to JitNoRangeChks in " FMT_BB ":\n", compCurBB->bbNum);
            gtDispTree(tree, nullptr, nullptr, true);
        }
#endif // DEBUG
        tree->gtFlags |= GTF_ARR_BOUND_INBND;
        return nullptr;
    }
#endif // FEATURE_ENABLE_NO_RANGE_CHECKS

    BitVecOps::Iter iter(apTraits, assertions);
    unsigned        index = 0;
    while (iter.NextElem(&index))
    {
        AssertionIndex assertionIndex = GetAssertionIndex(index);
        if (assertionIndex > optAssertionCount)
        {
            break;
        }
        // If it is not a nothrow assertion, skip.
        AssertionDsc* curAssertion = optGetAssertion(assertionIndex);
        if (!curAssertion->IsBoundsCheckNoThrow())
        {
            continue;
        }

        GenTreeBoundsChk* arrBndsChk = tree->AsBoundsChk();

        // Set 'isRedundant' to true if we can determine that 'arrBndsChk' can be
        // classified as a redundant bounds check using 'curAssertion'
        bool isRedundant = false;
#ifdef DEBUG
        const char* dbgMsg = "Not Set";
#endif

        // Do we have a previous range check involving the same 'vnLen' upper bound?
        if (curAssertion->op1.bnd.vnLen == vnStore->VNConservativeNormalValue(arrBndsChk->gtArrLen->gtVNPair))
        {
            ValueNum vnCurIdx = vnStore->VNConservativeNormalValue(arrBndsChk->gtIndex->gtVNPair);

            // Do we have the exact same lower bound 'vnIdx'?
            //       a[i] followed by a[i]
            if (curAssertion->op1.bnd.vnIdx == vnCurIdx)
            {
                isRedundant = true;
#ifdef DEBUG
                dbgMsg = "a[i] followed by a[i]";
#endif
            }
            // Are we using zero as the index?
            // It can always be considered as redundant with any previous value
            //       a[*] followed by a[0]
            else if (vnCurIdx == vnStore->VNZeroForType(arrBndsChk->gtIndex->TypeGet()))
            {
                isRedundant = true;
#ifdef DEBUG
                dbgMsg = "a[*] followed by a[0]";
#endif
            }
            // Do we have two constant indexes?
            else if (vnStore->IsVNConstant(curAssertion->op1.bnd.vnIdx) && vnStore->IsVNConstant(vnCurIdx))
            {
                // Make sure the types match.
                var_types type1 = vnStore->TypeOfVN(curAssertion->op1.bnd.vnIdx);
                var_types type2 = vnStore->TypeOfVN(vnCurIdx);

                if (type1 == type2 && type1 == TYP_INT)
                {
                    int index1 = vnStore->ConstantValue<int>(curAssertion->op1.bnd.vnIdx);
                    int index2 = vnStore->ConstantValue<int>(vnCurIdx);

                    // the case where index1 == index2 should have been handled above
                    assert(index1 != index2);

                    // It can always be considered as redundant with any previous higher constant value
                    //       a[K1] followed by a[K2], with K2 >= 0 and K1 >= K2
                    if (index2 >= 0 && index1 >= index2)
                    {
                        isRedundant = true;
#ifdef DEBUG
                        dbgMsg = "a[K1] followed by a[K2], with K2 >= 0 and K1 >= K2";
#endif
                    }
                }
            }
            // Extend this to remove additional redundant bounds checks:
            // i.e.  a[i+1] followed by a[i]  by using the VN(i+1) >= VN(i)
            //       a[i]   followed by a[j]  when j is known to be >= i
            //       a[i]   followed by a[5]  when i is known to be >= 5
        }

        if (!isRedundant)
        {
            continue;
        }

#ifdef DEBUG
        if (verbose)
        {
            printf("\nVN based redundant (%s) bounds check assertion prop for index #%02u in " FMT_BB ":\n", dbgMsg,
                   assertionIndex, compCurBB->bbNum);
            gtDispTree(tree, nullptr, nullptr, true);
        }
#endif
        if (arrBndsChk == stmt->GetRootNode())
        {
            // We have a top-level bounds check node.
            // This can happen when trees are broken up due to inlining.
            // optRemoveStandaloneRangeCheck will return the modified tree (side effects or a no-op).
            GenTree* newTree = optRemoveStandaloneRangeCheck(arrBndsChk, stmt);

            return optAssertionProp_Update(newTree, arrBndsChk, stmt);
        }

        // Defer actually removing the tree until processing reaches its parent comma, since
        // optRemoveCommaBasedRangeCheck needs to rewrite the whole comma tree.
        arrBndsChk->gtFlags |= GTF_ARR_BOUND_INBND;

        return nullptr;
    }

    return nullptr;
}

/*****************************************************************************
 *
 *  Called when we have a successfully performed an assertion prop. We have
 *  the newTree in hand. This method will replace the existing tree in the
 *  stmt with the newTree.
 *
 */

GenTree* Compiler::optAssertionProp_Update(GenTree* newTree, GenTree* tree, Statement* stmt)
{
    assert(newTree != nullptr);
    assert(tree != nullptr);

    if (stmt == nullptr)
    {
        noway_assert(optLocalAssertionProp);
    }
    else
    {
        noway_assert(!optLocalAssertionProp);

        // If newTree == tree then we modified the tree in-place otherwise we have to
        // locate our parent node and update it so that it points to newTree.
        if (newTree != tree)
        {
            FindLinkData linkData = gtFindLink(stmt, tree);
            GenTree**    useEdge  = linkData.result;
            GenTree*     parent   = linkData.parent;
            noway_assert(useEdge != nullptr);

            if (parent != nullptr)
            {
                parent->ReplaceOperand(useEdge, newTree);
            }
            else
            {
                // If there's no parent, the tree being replaced is the root of the
                // statement.
                assert((stmt->GetRootNode() == tree) && (stmt->GetRootNodePointer() == useEdge));
                stmt->SetRootNode(newTree);
            }

            // We only need to ensure that the gtNext field is set as it is used to traverse
            // to the next node in the tree. We will re-morph this entire statement in
            // optVNAssertionProp(). It will reset the gtPrev and gtNext links for all nodes.
            newTree->gtNext = tree->gtNext;

            // Old tree should not be referenced anymore.
            DEBUG_DESTROY_NODE(tree);
        }
    }

    optVNAssertionPropStmtMorphPending = true;

    return newTree;
}

//------------------------------------------------------------------------
// optAssertionProp: try and optimize a tree via assertion propagation
//
// Arguments:
//   assertions  - set of live assertions
//   tree        - tree to possibly optimize
//   stmt        - statement containing the tree
//   block       - block containing the statement
//
// Returns:
//   The modified tree, or nullptr if no assertion prop took place.
//
// Notes:
//   stmt may be nullptr during local assertion prop
//
GenTree* Compiler::optAssertionProp(ASSERT_VALARG_TP assertions, GenTree* tree, Statement* stmt, BasicBlock* block)
{
    switch (tree->gtOper)
    {
        case GT_LCL_VAR:
            return optAssertionProp_LclVar(assertions, tree->AsLclVarCommon(), stmt);

        case GT_OBJ:
        case GT_BLK:
        case GT_DYN_BLK:
        case GT_IND:
        case GT_NULLCHECK:
            return optAssertionProp_Ind(assertions, tree, stmt);

        case GT_ARR_BOUNDS_CHECK:
            return optAssertionProp_BndsChk(assertions, tree, stmt);

        case GT_COMMA:
            return optAssertionProp_Comma(assertions, tree, stmt);

        case GT_CAST:
            return optAssertionProp_Cast(assertions, tree, stmt);

        case GT_CALL:
            return optAssertionProp_Call(assertions, tree->AsCall(), stmt);

        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GT:
        case GT_GE:

            return optAssertionProp_RelOp(assertions, tree, stmt);

        case GT_JTRUE:

            if (block != nullptr)
            {
                return optVNConstantPropJTrue(block, tree->AsUnOp());
            }
            return nullptr;

        default:
            return nullptr;
    }
}

//------------------------------------------------------------------------
// optImpliedAssertions: Given a tree node that makes an assertion this
//                       method computes the set of implied assertions
//                       that are also true. The updated assertions are
//                       maintained on the Compiler object.
//
// Arguments:
//      assertionIndex   : The id of the assertion.
//      activeAssertions : The assertions that are already true at this point.

void Compiler::optImpliedAssertions(AssertionIndex assertionIndex, ASSERT_TP& activeAssertions)
{
    noway_assert(!optLocalAssertionProp);
    noway_assert(assertionIndex != 0);
    noway_assert(assertionIndex <= optAssertionCount);

    AssertionDsc* curAssertion = optGetAssertion(assertionIndex);
    if (!BitVecOps::IsEmpty(apTraits, activeAssertions))
    {
        const ASSERT_TP mappedAssertions = optGetVnMappedAssertions(curAssertion->op1.vn);
        if (mappedAssertions == nullptr)
        {
            return;
        }

        ASSERT_TP chkAssertions = BitVecOps::MakeCopy(apTraits, mappedAssertions);

        if (curAssertion->op2.kind == O2K_LCLVAR_COPY)
        {
            const ASSERT_TP op2Assertions = optGetVnMappedAssertions(curAssertion->op2.vn);
            if (op2Assertions != nullptr)
            {
                BitVecOps::UnionD(apTraits, chkAssertions, op2Assertions);
            }
        }
        BitVecOps::IntersectionD(apTraits, chkAssertions, activeAssertions);

        if (BitVecOps::IsEmpty(apTraits, chkAssertions))
        {
            return;
        }

        // Check each assertion in chkAssertions to see if it can be applied to curAssertion
        BitVecOps::Iter chkIter(apTraits, chkAssertions);
        unsigned        chkIndex = 0;
        while (chkIter.NextElem(&chkIndex))
        {
            AssertionIndex chkAssertionIndex = GetAssertionIndex(chkIndex);
            if (chkAssertionIndex > optAssertionCount)
            {
                break;
            }
            if (chkAssertionIndex == assertionIndex)
            {
                continue;
            }

            // Determine which one is a copy assertion and use the other to check for implied assertions.
            AssertionDsc* iterAssertion = optGetAssertion(chkAssertionIndex);
            if (curAssertion->IsCopyAssertion())
            {
                optImpliedByCopyAssertion(curAssertion, iterAssertion, activeAssertions);
            }
            else if (iterAssertion->IsCopyAssertion())
            {
                optImpliedByCopyAssertion(iterAssertion, curAssertion, activeAssertions);
            }
        }
    }
    // Is curAssertion a constant assignment of a 32-bit integer?
    // (i.e  GT_LVL_VAR X  == GT_CNS_INT)
    else if ((curAssertion->assertionKind == OAK_EQUAL) && (curAssertion->op1.kind == O1K_LCLVAR) &&
             (curAssertion->op2.kind == O2K_CONST_INT))
    {
        optImpliedByConstAssertion(curAssertion, activeAssertions);
    }
}

/*****************************************************************************
 *
 *   Given a set of active assertions this method computes the set
 *   of non-Null implied assertions that are also true
 */

void Compiler::optImpliedByTypeOfAssertions(ASSERT_TP& activeAssertions)
{
    if (BitVecOps::IsEmpty(apTraits, activeAssertions))
    {
        return;
    }

    // Check each assertion in activeAssertions to see if it can be applied to constAssertion
    BitVecOps::Iter chkIter(apTraits, activeAssertions);
    unsigned        chkIndex = 0;
    while (chkIter.NextElem(&chkIndex))
    {
        AssertionIndex chkAssertionIndex = GetAssertionIndex(chkIndex);
        if (chkAssertionIndex > optAssertionCount)
        {
            break;
        }
        // chkAssertion must be Type/Subtype is equal assertion
        AssertionDsc* chkAssertion = optGetAssertion(chkAssertionIndex);
        if ((chkAssertion->op1.kind != O1K_SUBTYPE && chkAssertion->op1.kind != O1K_EXACT_TYPE) ||
            (chkAssertion->assertionKind != OAK_EQUAL))
        {
            continue;
        }

        // Search the assertion table for a non-null assertion on op1 that matches chkAssertion
        for (AssertionIndex impIndex = 1; impIndex <= optAssertionCount; impIndex++)
        {
            AssertionDsc* impAssertion = optGetAssertion(impIndex);

            //  The impAssertion must be different from the chkAssertion
            if (impIndex == chkAssertionIndex)
            {
                continue;
            }

            // impAssertion must be a Non Null assertion on lclNum
            if ((impAssertion->assertionKind != OAK_NOT_EQUAL) ||
                ((impAssertion->op1.kind != O1K_LCLVAR) && (impAssertion->op1.kind != O1K_VALUE_NUMBER)) ||
                (impAssertion->op2.kind != O2K_CONST_INT) || (impAssertion->op1.vn != chkAssertion->op1.vn))
            {
                continue;
            }

            // The bit may already be in the result set
            if (!BitVecOps::IsMember(apTraits, activeAssertions, impIndex - 1))
            {
                BitVecOps::AddElemD(apTraits, activeAssertions, impIndex - 1);
#ifdef DEBUG
                if (verbose)
                {
                    printf("\nCompiler::optImpliedByTypeOfAssertions: %s Assertion #%02d, implies assertion #%02d",
                           (chkAssertion->op1.kind == O1K_SUBTYPE) ? "Subtype" : "Exact-type", chkAssertionIndex,
                           impIndex);
                }
#endif
            }

            // There is at most one non-null assertion that is implied by the current chkIndex assertion
            break;
        }
    }
}

//------------------------------------------------------------------------
// optGetVnMappedAssertions: Given a value number, get the assertions
//                           we have about the value number.
//
// Arguments:
//      vn - The given value number.
//
// Return Value:
//      The assertions we have about the value number.
//

ASSERT_VALRET_TP Compiler::optGetVnMappedAssertions(ValueNum vn)
{
    ASSERT_TP set = BitVecOps::UninitVal();
    if (optValueNumToAsserts->Lookup(vn, &set))
    {
        return set;
    }
    return BitVecOps::UninitVal();
}

/*****************************************************************************
 *
 *   Given a const assertion this method computes the set of implied assertions
 *   that are also true
 */

void Compiler::optImpliedByConstAssertion(AssertionDsc* constAssertion, ASSERT_TP& result)
{
    noway_assert(constAssertion->assertionKind == OAK_EQUAL);
    noway_assert(constAssertion->op1.kind == O1K_LCLVAR);
    noway_assert(constAssertion->op2.kind == O2K_CONST_INT);

    ssize_t iconVal = constAssertion->op2.u1.iconVal;

    const ASSERT_TP chkAssertions = optGetVnMappedAssertions(constAssertion->op1.vn);
    if (chkAssertions == nullptr || BitVecOps::IsEmpty(apTraits, chkAssertions))
    {
        return;
    }

    // Check each assertion in chkAssertions to see if it can be applied to constAssertion
    BitVecOps::Iter chkIter(apTraits, chkAssertions);
    unsigned        chkIndex = 0;
    while (chkIter.NextElem(&chkIndex))
    {
        AssertionIndex chkAssertionIndex = GetAssertionIndex(chkIndex);
        if (chkAssertionIndex > optAssertionCount)
        {
            break;
        }
        // The impAssertion must be different from the const assertion.
        AssertionDsc* impAssertion = optGetAssertion(chkAssertionIndex);
        if (impAssertion == constAssertion)
        {
            continue;
        }

        // The impAssertion must be an assertion about the same local var.
        if (impAssertion->op1.vn != constAssertion->op1.vn)
        {
            continue;
        }

        bool usable = false;
        switch (impAssertion->op2.kind)
        {
            case O2K_SUBRANGE:
                // Is the const assertion's constant, within implied assertion's bounds?
                usable = ((iconVal >= impAssertion->op2.u2.loBound) && (iconVal <= impAssertion->op2.u2.hiBound));
                break;

            case O2K_CONST_INT:
                // Is the const assertion's constant equal/not equal to the implied assertion?
                usable = ((impAssertion->assertionKind == OAK_EQUAL) && (impAssertion->op2.u1.iconVal == iconVal)) ||
                         ((impAssertion->assertionKind == OAK_NOT_EQUAL) && (impAssertion->op2.u1.iconVal != iconVal));
                break;

            default:
                // leave 'usable' = false;
                break;
        }

        if (usable)
        {
            BitVecOps::AddElemD(apTraits, result, chkIndex);
#ifdef DEBUG
            if (verbose)
            {
                AssertionDsc* firstAssertion = optGetAssertion(1);
                printf("\nCompiler::optImpliedByConstAssertion: constAssertion #%02d , implies assertion #%02d",
                       (constAssertion - firstAssertion) + 1, (impAssertion - firstAssertion) + 1);
            }
#endif
        }
    }
}

/*****************************************************************************
 *
 *  Given a copy assertion and a dependent assertion this method computes the
 *  set of implied assertions that are also true.
 *  For copy assertions, exact SSA num and LCL nums should match, because
 *  we don't have kill sets and we depend on their value num for dataflow.
 */

void Compiler::optImpliedByCopyAssertion(AssertionDsc* copyAssertion, AssertionDsc* depAssertion, ASSERT_TP& result)
{
    noway_assert(copyAssertion->IsCopyAssertion());

    // Get the copyAssert's lcl/ssa nums.
    unsigned copyAssertLclNum = BAD_VAR_NUM;
    unsigned copyAssertSsaNum = SsaConfig::RESERVED_SSA_NUM;

    // Check if copyAssertion's op1 or op2 matches the depAssertion's op1.
    if (depAssertion->op1.lcl.lclNum == copyAssertion->op1.lcl.lclNum)
    {
        copyAssertLclNum = copyAssertion->op2.lcl.lclNum;
        copyAssertSsaNum = copyAssertion->op2.lcl.ssaNum;
    }
    else if (depAssertion->op1.lcl.lclNum == copyAssertion->op2.lcl.lclNum)
    {
        copyAssertLclNum = copyAssertion->op1.lcl.lclNum;
        copyAssertSsaNum = copyAssertion->op1.lcl.ssaNum;
    }
    // Check if copyAssertion's op1 or op2 matches the depAssertion's op2.
    else if (depAssertion->op2.kind == O2K_LCLVAR_COPY)
    {
        if (depAssertion->op2.lcl.lclNum == copyAssertion->op1.lcl.lclNum)
        {
            copyAssertLclNum = copyAssertion->op2.lcl.lclNum;
            copyAssertSsaNum = copyAssertion->op2.lcl.ssaNum;
        }
        else if (depAssertion->op2.lcl.lclNum == copyAssertion->op2.lcl.lclNum)
        {
            copyAssertLclNum = copyAssertion->op1.lcl.lclNum;
            copyAssertSsaNum = copyAssertion->op1.lcl.ssaNum;
        }
    }

    if (copyAssertLclNum == BAD_VAR_NUM || copyAssertSsaNum == SsaConfig::RESERVED_SSA_NUM)
    {
        return;
    }

    // Get the depAssert's lcl/ssa nums.
    unsigned depAssertLclNum = BAD_VAR_NUM;
    unsigned depAssertSsaNum = SsaConfig::RESERVED_SSA_NUM;
    if ((depAssertion->op1.kind == O1K_LCLVAR) && (depAssertion->op2.kind == O2K_LCLVAR_COPY))
    {
        if ((depAssertion->op1.lcl.lclNum == copyAssertion->op1.lcl.lclNum) ||
            (depAssertion->op1.lcl.lclNum == copyAssertion->op2.lcl.lclNum))
        {
            depAssertLclNum = depAssertion->op2.lcl.lclNum;
            depAssertSsaNum = depAssertion->op2.lcl.ssaNum;
        }
        else if ((depAssertion->op2.lcl.lclNum == copyAssertion->op1.lcl.lclNum) ||
                 (depAssertion->op2.lcl.lclNum == copyAssertion->op2.lcl.lclNum))
        {
            depAssertLclNum = depAssertion->op1.lcl.lclNum;
            depAssertSsaNum = depAssertion->op1.lcl.ssaNum;
        }
    }

    if (depAssertLclNum == BAD_VAR_NUM || depAssertSsaNum == SsaConfig::RESERVED_SSA_NUM)
    {
        return;
    }

    // Is depAssertion a constant assignment of a 32-bit integer?
    // (i.e  GT_LVL_VAR X == GT_CNS_INT)
    bool depIsConstAssertion = ((depAssertion->assertionKind == OAK_EQUAL) && (depAssertion->op1.kind == O1K_LCLVAR) &&
                                (depAssertion->op2.kind == O2K_CONST_INT));

    // Search the assertion table for an assertion on op1 that matches depAssertion
    // The matching assertion is the implied assertion.
    for (AssertionIndex impIndex = 1; impIndex <= optAssertionCount; impIndex++)
    {
        AssertionDsc* impAssertion = optGetAssertion(impIndex);

        //  The impAssertion must be different from the copy and dependent assertions
        if (impAssertion == copyAssertion || impAssertion == depAssertion)
        {
            continue;
        }

        if (!AssertionDsc::SameKind(depAssertion, impAssertion))
        {
            continue;
        }

        bool op1MatchesCopy =
            (copyAssertLclNum == impAssertion->op1.lcl.lclNum) && (copyAssertSsaNum == impAssertion->op1.lcl.ssaNum);

        bool usable = false;
        switch (impAssertion->op2.kind)
        {
            case O2K_SUBRANGE:
                usable = op1MatchesCopy && ((impAssertion->op2.u2.loBound <= depAssertion->op2.u2.loBound) &&
                                            (impAssertion->op2.u2.hiBound >= depAssertion->op2.u2.hiBound));
                break;

            case O2K_CONST_LONG:
                usable = op1MatchesCopy && (impAssertion->op2.lconVal == depAssertion->op2.lconVal);
                break;

            case O2K_CONST_DOUBLE:
                // Exact memory match because of positive and negative zero
                usable = op1MatchesCopy &&
                         (memcmp(&impAssertion->op2.dconVal, &depAssertion->op2.dconVal, sizeof(double)) == 0);
                break;

            case O2K_IND_CNS_INT:
                // This is the ngen case where we have an indirection of an address.
                noway_assert((impAssertion->op1.kind == O1K_EXACT_TYPE) || (impAssertion->op1.kind == O1K_SUBTYPE));

                FALLTHROUGH;

            case O2K_CONST_INT:
                usable = op1MatchesCopy && (impAssertion->op2.u1.iconVal == depAssertion->op2.u1.iconVal);
                break;

            case O2K_LCLVAR_COPY:
                // Check if op1 of impAssertion matches copyAssertion and also op2 of impAssertion matches depAssertion.
                if (op1MatchesCopy && (depAssertLclNum == impAssertion->op2.lcl.lclNum &&
                                       depAssertSsaNum == impAssertion->op2.lcl.ssaNum))
                {
                    usable = true;
                }
                else
                {
                    // Otherwise, op2 of impAssertion should match copyAssertion and also op1 of impAssertion matches
                    // depAssertion.
                    usable = ((copyAssertLclNum == impAssertion->op2.lcl.lclNum &&
                               copyAssertSsaNum == impAssertion->op2.lcl.ssaNum) &&
                              (depAssertLclNum == impAssertion->op1.lcl.lclNum &&
                               depAssertSsaNum == impAssertion->op1.lcl.ssaNum));
                }
                break;

            default:
                // leave 'usable' = false;
                break;
        }

        if (usable)
        {
            BitVecOps::AddElemD(apTraits, result, impIndex - 1);

#ifdef DEBUG
            if (verbose)
            {
                AssertionDsc* firstAssertion = optGetAssertion(1);
                printf("\nCompiler::optImpliedByCopyAssertion: copyAssertion #%02d and depAssertion #%02d, implies "
                       "assertion #%02d",
                       (copyAssertion - firstAssertion) + 1, (depAssertion - firstAssertion) + 1,
                       (impAssertion - firstAssertion) + 1);
            }
#endif
            // If the depAssertion is a const assertion then any other assertions that it implies could also imply a
            // subrange assertion.
            if (depIsConstAssertion)
            {
                optImpliedByConstAssertion(impAssertion, result);
            }
        }
    }
}

#include "dataflow.h"

/*****************************************************************************
 *
 * Dataflow visitor like callback so that all dataflow is in a single place
 *
 */
class AssertionPropFlowCallback
{
private:
    ASSERT_TP preMergeOut;
    ASSERT_TP preMergeJumpDestOut;

    ASSERT_TP* mJumpDestOut;
    ASSERT_TP* mJumpDestGen;

    BitVecTraits* apTraits;

public:
    AssertionPropFlowCallback(Compiler* pCompiler, ASSERT_TP* jumpDestOut, ASSERT_TP* jumpDestGen)
        : preMergeOut(BitVecOps::UninitVal())
        , preMergeJumpDestOut(BitVecOps::UninitVal())
        , mJumpDestOut(jumpDestOut)
        , mJumpDestGen(jumpDestGen)
        , apTraits(pCompiler->apTraits)
    {
    }

    // At the start of the merge function of the dataflow equations, initialize premerge state (to detect change.)
    void StartMerge(BasicBlock* block)
    {
        JITDUMP("AssertionPropCallback::StartMerge: " FMT_BB " in -> %s\n", block->bbNum,
                BitVecOps::ToString(apTraits, block->bbAssertionIn));
        BitVecOps::Assign(apTraits, preMergeOut, block->bbAssertionOut);
        BitVecOps::Assign(apTraits, preMergeJumpDestOut, mJumpDestOut[block->bbNum]);
    }

    // During merge, perform the actual merging of the predecessor's (since this is a forward analysis) dataflow flags.
    void Merge(BasicBlock* block, BasicBlock* predBlock, unsigned dupCount)
    {
        ASSERT_TP pAssertionOut;

        if (predBlock->bbJumpKind == BBJ_COND && (predBlock->bbJumpDest == block))
        {
            pAssertionOut = mJumpDestOut[predBlock->bbNum];

            if (dupCount > 1)
            {
                // Scenario where next block and conditional block, both point to the same block.
                // In such case, intersect the assertions present on both the out edges of predBlock.
                assert(predBlock->bbNext == block);
                BitVecOps::IntersectionD(apTraits, pAssertionOut, predBlock->bbAssertionOut);

                JITDUMP("AssertionPropCallback::Merge     : Duplicate flow, " FMT_BB " in -> %s, predBlock " FMT_BB
                        " out1 -> %s, out2 -> %s\n",
                        block->bbNum, BitVecOps::ToString(apTraits, block->bbAssertionIn), predBlock->bbNum,
                        BitVecOps::ToString(apTraits, mJumpDestOut[predBlock->bbNum]),
                        BitVecOps::ToString(apTraits, predBlock->bbAssertionOut));
            }
        }
        else
        {
            pAssertionOut = predBlock->bbAssertionOut;
        }

        JITDUMP("AssertionPropCallback::Merge     : " FMT_BB " in -> %s, predBlock " FMT_BB " out -> %s\n",
                block->bbNum, BitVecOps::ToString(apTraits, block->bbAssertionIn), predBlock->bbNum,
                BitVecOps::ToString(apTraits, pAssertionOut));
        BitVecOps::IntersectionD(apTraits, block->bbAssertionIn, pAssertionOut);
    }

    //------------------------------------------------------------------------
    // MergeHandler: Merge assertions into the first exception handler/filter block.
    //
    // Arguments:
    //   block         - the block that is the start of a handler or filter;
    //   firstTryBlock - the first block of the try for "block" handler;
    //   lastTryBlock  - the last block of the try for "block" handler;.
    //
    // Notes:
    //   We can jump to the handler from any instruction in the try region.
    //   It means we can propagate only assertions that are valid for the whole try region.
    void MergeHandler(BasicBlock* block, BasicBlock* firstTryBlock, BasicBlock* lastTryBlock)
    {
        BitVecOps::IntersectionD(apTraits, block->bbAssertionIn, firstTryBlock->bbAssertionIn);
        BitVecOps::IntersectionD(apTraits, block->bbAssertionIn, lastTryBlock->bbAssertionOut);
    }

    // At the end of the merge store results of the dataflow equations, in a postmerge state.
    bool EndMerge(BasicBlock* block)
    {
        JITDUMP("AssertionPropCallback::EndMerge  : " FMT_BB " in -> %s\n\n", block->bbNum,
                BitVecOps::ToString(apTraits, block->bbAssertionIn));

        BitVecOps::DataFlowD(apTraits, block->bbAssertionOut, block->bbAssertionGen, block->bbAssertionIn);
        BitVecOps::DataFlowD(apTraits, mJumpDestOut[block->bbNum], mJumpDestGen[block->bbNum], block->bbAssertionIn);

        bool changed = (!BitVecOps::Equal(apTraits, preMergeOut, block->bbAssertionOut) ||
                        !BitVecOps::Equal(apTraits, preMergeJumpDestOut, mJumpDestOut[block->bbNum]));

        if (changed)
        {
            JITDUMP("AssertionPropCallback::Changed   : " FMT_BB " before out -> %s; after out -> %s;\n"
                    "\t\tjumpDest before out -> %s; jumpDest after out -> %s;\n\n",
                    block->bbNum, BitVecOps::ToString(apTraits, preMergeOut),
                    BitVecOps::ToString(apTraits, block->bbAssertionOut),
                    BitVecOps::ToString(apTraits, preMergeJumpDestOut),
                    BitVecOps::ToString(apTraits, mJumpDestOut[block->bbNum]));
        }
        else
        {
            JITDUMP("AssertionPropCallback::Unchanged  : " FMT_BB " out -> %s; \t\tjumpDest out -> %s\n\n",
                    block->bbNum, BitVecOps::ToString(apTraits, block->bbAssertionOut),
                    BitVecOps::ToString(apTraits, mJumpDestOut[block->bbNum]));
        }

        return changed;
    }
};

/*****************************************************************************
 *
 *   Compute the assertions generated by each block.
 */
ASSERT_TP* Compiler::optComputeAssertionGen()
{
    ASSERT_TP* jumpDestGen = fgAllocateTypeForEachBlk<ASSERT_TP>();

    for (BasicBlock* block = fgFirstBB; block != nullptr; block = block->bbNext)
    {
        ASSERT_TP valueGen = BitVecOps::MakeEmpty(apTraits);
        GenTree*  jtrue    = nullptr;

        // Walk the statement trees in this basic block.
        for (Statement* stmt : block->Statements())
        {
            for (GenTree* tree = stmt->GetTreeList(); tree != nullptr; tree = tree->gtNext)
            {
                if (tree->gtOper == GT_JTRUE)
                {
                    // A GT_TRUE is always the last node in a tree, so we can break here
                    assert((tree->gtNext == nullptr) && (stmt->GetNextStmt() == nullptr));
                    jtrue = tree;
                    break;
                }

                if (tree->GeneratesAssertion())
                {
                    AssertionInfo info = tree->GetAssertionInfo();
                    optImpliedAssertions(info.GetAssertionIndex(), valueGen);
                    BitVecOps::AddElemD(apTraits, valueGen, info.GetAssertionIndex() - 1);
                }
            }
        }

        if (jtrue != nullptr)
        {
            // Copy whatever we have accumulated into jumpDest edge's valueGen.
            ASSERT_TP jumpDestValueGen = BitVecOps::MakeCopy(apTraits, valueGen);

            if (jtrue->GeneratesAssertion())
            {
                AssertionInfo  info = jtrue->GetAssertionInfo();
                AssertionIndex valueAssertionIndex;
                AssertionIndex jumpDestAssertionIndex;

                if (info.IsNextEdgeAssertion())
                {
                    valueAssertionIndex    = info.GetAssertionIndex();
                    jumpDestAssertionIndex = optFindComplementary(info.GetAssertionIndex());
                }
                else // is jump edge assertion
                {
                    valueAssertionIndex    = optFindComplementary(info.GetAssertionIndex());
                    jumpDestAssertionIndex = info.GetAssertionIndex();
                }

                if (valueAssertionIndex != NO_ASSERTION_INDEX)
                {
                    // Update valueGen if we have an assertion for the bbNext edge
                    optImpliedAssertions(valueAssertionIndex, valueGen);
                    BitVecOps::AddElemD(apTraits, valueGen, valueAssertionIndex - 1);
                }

                if (jumpDestAssertionIndex != NO_ASSERTION_INDEX)
                {
                    // Update jumpDestValueGen if we have an assertion for the bbJumpDest edge
                    optImpliedAssertions(jumpDestAssertionIndex, jumpDestValueGen);
                    BitVecOps::AddElemD(apTraits, jumpDestValueGen, jumpDestAssertionIndex - 1);
                }
            }

            jumpDestGen[block->bbNum] = jumpDestValueGen;
        }
        else
        {
            jumpDestGen[block->bbNum] = BitVecOps::MakeEmpty(apTraits);
        }

        block->bbAssertionGen = valueGen;

#ifdef DEBUG
        if (verbose)
        {
            printf(FMT_BB " valueGen = %s", block->bbNum, BitVecOps::ToString(apTraits, block->bbAssertionGen));
            if (block->bbJumpKind == BBJ_COND)
            {
                printf(" => " FMT_BB " valueGen = %s,", block->bbJumpDest->bbNum,
                       BitVecOps::ToString(apTraits, jumpDestGen[block->bbNum]));
            }
            printf("\n");
        }
#endif
    }
    return jumpDestGen;
}

/*****************************************************************************
 *
 *   Initialize the assertion data flow flags that will be propagated.
 */

ASSERT_TP* Compiler::optInitAssertionDataflowFlags()
{
    ASSERT_TP* jumpDestOut = fgAllocateTypeForEachBlk<ASSERT_TP>();

    // The local assertion gen phase may have created unreachable blocks.
    // They will never be visited in the dataflow propagation phase, so they need to
    // be initialized correctly. This means that instead of setting their sets to
    // apFull (i.e. all possible bits set), we need to set the bits only for valid
    // assertions (note that at this point we are not creating any new assertions).
    // Also note that assertion indices start from 1.
    ASSERT_TP apValidFull = BitVecOps::MakeEmpty(apTraits);
    for (int i = 1; i <= optAssertionCount; i++)
    {
        BitVecOps::AddElemD(apTraits, apValidFull, i - 1);
    }

    // Initially estimate the OUT sets to everything except killed expressions
    // Also set the IN sets to 1, so that we can perform the intersection.
    for (BasicBlock* block = fgFirstBB; block; block = block->bbNext)
    {
        block->bbAssertionIn      = BitVecOps::MakeCopy(apTraits, apValidFull);
        block->bbAssertionGen     = BitVecOps::MakeEmpty(apTraits);
        block->bbAssertionOut     = BitVecOps::MakeCopy(apTraits, apValidFull);
        jumpDestOut[block->bbNum] = BitVecOps::MakeCopy(apTraits, apValidFull);
    }
    // Compute the data flow values for all tracked expressions
    // IN and OUT never change for the initial basic block B1
    BitVecOps::ClearD(apTraits, fgFirstBB->bbAssertionIn);
    return jumpDestOut;
}

//------------------------------------------------------------------------------
// optVNConstantPropExtractSideEffects
//    Extracts side effects from a tree so it can be replaced with a comma
//    separated list of side effects + a const tree.
//
// Note:
//   The caller expects that the root of the tree has no side effects and it
//   won't be extracted. Otherwise the resulting comma tree would be bigger
//   than the tree before optimization.
//
// Arguments:
//    tree  - The tree node with constant value to extrace side-effects from.
//
// Return Value:
//      1. Returns the extracted side-effects from "tree"
//      2. When no side-effects are present, returns null.
//
//
GenTree* Compiler::optVNConstantPropExtractSideEffects(GenTree* tree)
{
    assert(vnStore->IsVNConstant(vnStore->VNConservativeNormalValue(tree->gtVNPair)));

    GenTree* sideEffList = nullptr;

    // If we have side effects, extract them.
    if ((tree->gtFlags & GTF_SIDE_EFFECT) != 0)
    {
        // Do a sanity check to ensure persistent side effects aren't discarded and
        // tell gtExtractSideEffList to ignore the root of the tree.
        assert(!gtNodeHasSideEffects(tree, GTF_PERSISTENT_SIDE_EFFECTS));

        // Exception side effects may be ignored because the root is known to be a constant
        // (e.g. VN may evaluate a DIV/MOD node to a constant and the node may still
        // have GTF_EXCEPT set, even if it does not actually throw any exceptions).
        bool ignoreRoot = true;

        gtExtractSideEffList(tree, &sideEffList, GTF_SIDE_EFFECT, ignoreRoot);
    }

    return sideEffList;
}

//------------------------------------------------------------------------------
// optVNConstantPropJTrue
//    Constant propagate on the JTrue node by extracting side effects and moving
//    them into their own statements. The relop node is then modified to yield
//    true or false, so the branch can be folded.
//
// Return Value:
//    The jmpTrue tree node that has relop of the form "0 =/!= 0".
//    If "tree" evaluates to "true" relop is "0 == 0". Else relop is "0 != 0".
//
// Description:
//    Special treatment for JTRUE nodes' constant propagation. This is because
//    for JTRUE(1) or JTRUE(0), if there are side effects they need to be put
//    in separate statements. This is to prevent relop's constant
//    propagation from doing a simple minded conversion from
//    (1) STMT(JTRUE(RELOP(COMMA(sideEffect, OP1), OP2)), S.T. op1 =/!= op2 to
//    (2) STMT(JTRUE(COMMA(sideEffect, 1/0)).
//
//    fgFoldConditional doesn't fold (2), a side-effecting JTRUE's op1. So, let us,
//    here, convert (1) as two statements: STMT(sideEffect), STMT(JTRUE(1/0)),
//    so that the JTRUE will get folded by fgFoldConditional.
//
//  Note: fgFoldConditional is called from other places as well, which may be
//  sensitive to adding new statements. Hence the change is not made directly
//  into fgFoldConditional.
//
GenTree* Compiler::optVNConstantPropJTrue(BasicBlock* block, GenTreeUnOp* jtrue)
{
    GenTree* relop = jtrue->GetOp(0);

    // VN based assertion non-null on this relop has been performed.
    if (!relop->OperIsCompare())
    {
        return nullptr;
    }

    // Make sure GTF_RELOP_JMP_USED flag is set so that we don't replace the
    // relop with a constant when we reach it later in the pre-order walk.
    assert((relop->gtFlags & GTF_RELOP_JMP_USED) != 0);

    ValueNum vnCns = vnStore->VNConservativeNormalValue(relop->gtVNPair);

    if (!vnStore->IsVNConstant(vnCns))
    {
        return nullptr;
    }

    GenTree* sideEffects = optVNConstantPropExtractSideEffects(relop);

    // Transform the relop into EQ|NE(0, 0)
    ValueNum vnZero = vnStore->VNZeroForType(TYP_INT);
    GenTree* op1    = gtNewIconNode(0);
    op1->SetVNs(ValueNumPair(vnZero, vnZero));
    relop->AsOp()->SetOp(0, op1);
    GenTree* op2 = gtNewIconNode(0);
    op2->SetVNs(ValueNumPair(vnZero, vnZero));
    relop->AsOp()->SetOp(1, op2);
    relop->SetOper(vnStore->CoercedConstantValue<int64_t>(vnCns) != 0 ? GT_EQ : GT_NE);
    ValueNum vnLib = vnStore->VNLiberalNormalValue(relop->gtVNPair);
    relop->SetVNs(ValueNumPair(vnLib, vnCns));

    while (sideEffects != nullptr)
    {
        Statement* newStmt;

        if (sideEffects->OperIs(GT_COMMA))
        {
            newStmt     = fgNewStmtNearEnd(block, sideEffects->AsOp()->GetOp(0));
            sideEffects = sideEffects->AsOp()->GetOp(1);
        }
        else
        {
            newStmt     = fgNewStmtNearEnd(block, sideEffects);
            sideEffects = nullptr;
        }

        // fgMorphBlockStmt could potentially affect stmts after the current one,
        // for example when it decides to fgRemoveRestOfBlock.

        // TODO-MIKE-Review: Do we really need to remorph? Seems like simply
        // fgSetStmtSeq should suffice here. Also, this morphs trees before
        // doing constant propagation so we may morph again if they contains
        // constants.

        fgMorphBlockStmt(block, newStmt DEBUGARG(__FUNCTION__));
    }

    return jtrue;
}

class VNConstPropVisitor final : public GenTreeVisitor<VNConstPropVisitor>
{
    ValueNumStore* m_vnStore;
    BasicBlock*    m_block;
    Statement*     m_stmt;

public:
    enum
    {
        DoPreOrder = true
    };

    VNConstPropVisitor(Compiler* compiler, BasicBlock* block, Statement* stmt)
        : GenTreeVisitor(compiler), m_vnStore(compiler->vnStore), m_block(block), m_stmt(stmt)
    {
    }

    fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
    {
        PropagateNonNull(*use);
        return PropagateConst(*use);
    }

private:
    void PropagateNonNull(GenTree* tree)
    {
        if (GenTreeCall* call = tree->IsCall())
        {
            PropagateNonNullThisArg(call);
        }
        else if (GenTreeIndir* indir = tree->IsIndir())
        {
            PropagateNonNullIndirAddress(indir);
        }
    }

    void PropagateNonNullThisArg(GenTreeCall* call)
    {
        if (!call->NeedsNullCheck())
        {
            return;
        }

        GenTree* thisArg = m_compiler->gtGetThisArg(call);
        noway_assert(thisArg != nullptr);

        // TODO-MIKE-Review: This check is likely useless, if the VN is known to be non-null
        // then it doesn't matter what kind of node the arg is. It's unlikely that VN will be
        // able to prove that anything else other than a LCL_VAR is non-null conservatively,
        // maybe a LCL_FLD?

        if (!thisArg->OperIs(GT_LCL_VAR))
        {
            return;
        }

        if (!m_vnStore->IsKnownNonNull(thisArg->gtVNPair.GetConservative()))
        {
            return;
        }

        JITDUMP("\nCall " FMT_TREEID " has non-null this arg, removing GTF_CALL_NULLCHECK and GTF_EXCEPT\n",
                call->GetID());

        call->gtFlags &= ~(GTF_CALL_NULLCHECK | GTF_EXCEPT);
        noway_assert((call->gtFlags & GTF_SIDE_EFFECT) != 0);

        m_compiler->optVNAssertionPropStmtMorphPending = true;
    }

    void PropagateNonNullIndirAddress(GenTreeIndir* indir)
    {
        if ((indir->gtFlags & GTF_EXCEPT) == 0)
        {
            return;
        }

        GenTree* addr = indir->GetAddr();

        if (addr->OperIs(GT_ADD) && addr->AsOp()->GetOp(1)->IsIntCon())
        {
            addr = addr->AsOp()->GetOp(0);
        }

        // TODO-MIKE-Review: This check is likely useless, if the VN is known to be non-null
        // then it doesn't matter what kind of node the addr is. It's unlikely that VN will be
        // able to prove that anything else other than a LCL_VAR is non-null conservatively,
        // maybe a LCL_FLD?

        if (!addr->OperIs(GT_LCL_VAR))
        {
            return;
        }

        if (!m_vnStore->IsKnownNonNull(addr->gtVNPair.GetConservative()))
        {
            return;
        }

        JITDUMP("\nIndir " FMT_TREEID " has non-null address, removing GTF_EXCEPT\n", indir->GetID());

        indir->gtFlags &= ~GTF_EXCEPT;
        indir->gtFlags |= GTF_IND_NONFAULTING;

        // Set this flag to prevent reordering, it may be that we were able to prove that the address
        // is non-null due to a previous indirection or null check that prevent us from getting here
        // with a null address.
        // TODO-MIKE-Review: Hmm, that's probably only for local assertion propagation...
        indir->gtFlags |= GTF_ORDER_SIDEEFF;

        m_compiler->optVNAssertionPropStmtMorphPending = true;
    }

    fgWalkResult PropagateConst(GenTree* tree)
    {
        // GTF_DONT_CSE is also used to block constant propagation, not just CSE.
        if (!tree->CanCSE())
        {
            return Compiler::WALK_CONTINUE;
        }

        // Don't propagate floating-point constants into a TYP_STRUCT LclVar
        // This can occur for HFA return values (see hfa_sf3E_r.exe)
        if (tree->TypeIs(TYP_STRUCT))
        {
            return Compiler::WALK_CONTINUE;
        }

        switch (tree->GetOper())
        {
            case GT_LCL_VAR:
                if ((tree->gtFlags & GTF_VAR_DEF) != 0)
                {
                    return Compiler::WALK_CONTINUE;
                }

                // Don't undo constant CSEs.
                if (m_compiler->lclNumIsCSE(tree->AsLclVar()->GetLclNum()))
                {
                    return Compiler::WALK_CONTINUE;
                }

                break;

            case GT_MUL:
                // Don't transform long multiplies.
                if ((tree->gtFlags & GTF_MUL_64RSLT) != 0)
                {
                    // TODO-MIKE-Review: Erm, why skip subtrees?!?
                    return Compiler::WALK_SKIP_SUBTREES;
                }
                FALLTHROUGH;
            case GT_ADD:
            case GT_SUB:
            case GT_DIV:
            case GT_MOD:
            case GT_UDIV:
            case GT_UMOD:
            case GT_EQ:
            case GT_NE:
            case GT_LT:
            case GT_LE:
            case GT_GE:
            case GT_GT:
            case GT_OR:
            case GT_XOR:
            case GT_AND:
            case GT_LSH:
            case GT_RSH:
            case GT_RSZ:
            case GT_NEG:
            case GT_CAST:
            case GT_INTRINSIC:
            case GT_JTRUE:
                // TODO-MIKE-Review: Huh, this is missing various opers - ROL, ROR, BITCAST, NOT, BSWAP...
                break;

            default:
                return Compiler::WALK_CONTINUE;
        }

        GenTree* newTree;

        if (tree->OperIs(GT_JTRUE))
        {
            // Treat JTRUE separately to extract side effects into separate statements
            // rather than creating COMMA trees.
            newTree = m_compiler->optVNConstantPropJTrue(m_block, tree->AsUnOp());
        }
        else
        {
            newTree = GetConstantNode(tree);
        }

        if (newTree == nullptr)
        {
            return Compiler::WALK_CONTINUE;
        }

        // Successful propagation, mark as assertion propagated and skip
        // sub-tree (with side-effects) visits.
        // TODO #18291: at that moment stmt could be already removed from the stmt list.

        UpdateStmt(newTree, tree);

        JITDUMP("After constant propagation on " FMT_TREEID ":\n", tree->GetID());
        DBEXEC(VERBOSE, m_compiler->gtDispStmt(m_stmt));

        return Compiler::WALK_SKIP_SUBTREES;
    }

    GenTree* GetConstantNode(GenTree* tree)
    {
        assert(!tree->OperIs(GT_JTRUE));

        // If relop is part of JTRUE, this should be optimized as part of the parent JTRUE.
        if (tree->OperIsCompare() && ((tree->gtFlags & GTF_RELOP_JMP_USED) != 0))
        {
            return nullptr;
        }

        ValueNumPair vnp = tree->gtVNPair;
        ValueNum     vn  = m_vnStore->VNConservativeNormalValue(vnp);

        if (!m_vnStore->IsVNConstant(vn))
        {
            return nullptr;
        }

        var_types vnType  = m_vnStore->TypeOfVN(vn);
        GenTree*  newTree = nullptr;

        if (m_vnStore->IsVNHandle(vn))
        {
            // Don't perform constant folding that involves a handle that needs to be recorded
            // as a relocation with the VM. The VN type should be TYP_I_IMPL but the tree may
            // sometimes be TYP_BYREF, due to things like Unsafe.As.
            if (!m_compiler->opts.compReloc && tree->TypeIs(TYP_I_IMPL, TYP_BYREF) && (vnType == TYP_I_IMPL))
            {
                newTree = m_compiler->gtNewIconHandleNode(m_vnStore->ConstantValue<target_ssize_t>(vn),
                                                          m_vnStore->GetHandleFlags(vn));
            }
        }
        // The tree type and the VN type should match but VN can't be trusted. At least for SIMD
        // locals, VN manages to pull out a TYP_LONG 0 constant out of the hat, if the local is
        // not explictily initialized and .localsinit is used.
        // TODO-MIKE-Review: Shouldn't this check the actual type of the tree?
        else if (tree->GetType() == vnType)
        {
            switch (vnType)
            {
                case TYP_FLOAT:
                    newTree = m_compiler->gtNewDconNode(m_vnStore->ConstantValue<float>(vn), TYP_FLOAT);
                    break;
                case TYP_DOUBLE:
                    newTree = m_compiler->gtNewDconNode(m_vnStore->ConstantValue<double>(vn), TYP_DOUBLE);
                    break;
                case TYP_INT:
                    newTree = m_compiler->gtNewIconNode(m_vnStore->ConstantValue<int32_t>(vn));
                    break;
                case TYP_LONG:
                    newTree = m_compiler->gtNewLconNode(m_vnStore->ConstantValue<int64_t>(vn));
                    break;
                case TYP_REF:
                    assert(m_vnStore->ConstantValue<size_t>(vn) == 0);
                    newTree = m_compiler->gtNewIconNode(0, TYP_REF);
                    break;
                case TYP_BYREF:
                    // Do not support const byref optimization.
                    break;
                default:
                    unreached();
            }
        }

        if (newTree == nullptr)
        {
            return nullptr;
        }

        newTree->SetVNs(vnp);

        GenTree* sideEffects = m_compiler->optVNConstantPropExtractSideEffects(tree);

        if (sideEffects != nullptr)
        {
            assert((sideEffects->gtFlags & GTF_SIDE_EFFECT) != 0);

            // TODO-MIKE-Review: So we bother setting the VN on the constant node but not on the COMMA tree?
            // Also, the constant node gets the wrong VN, the one that includes exceptions...
            newTree = m_compiler->gtNewCommaNode(sideEffects, newTree);
        }

        return newTree;
    }

    GenTree* UpdateStmt(GenTree* newTree, GenTree* tree)
    {
        // If newTree == tree then we modified the tree in-place otherwise we have to
        // locate our parent node and update it so that it points to newTree.
        if (newTree != tree)
        {
            Compiler::FindLinkData linkData = m_compiler->gtFindLink(m_stmt, tree);
            GenTree**              useEdge  = linkData.result;
            GenTree*               parent   = linkData.parent;
            noway_assert(useEdge != nullptr);

            if (parent != nullptr)
            {
                parent->ReplaceOperand(useEdge, newTree);
            }
            else
            {
                // If there's no parent, the tree being replaced is the root of the
                // statement.
                assert((m_stmt->GetRootNode() == tree) && (m_stmt->GetRootNodePointer() == useEdge));
                m_stmt->SetRootNode(newTree);
            }

            // We only need to ensure that the gtNext field is set as it is used to traverse
            // to the next node in the tree. We will re-morph this entire statement in
            // optVNAssertionProp(). It will reset the gtPrev and gtNext links for all nodes.
            newTree->gtNext = tree->gtNext;

            // Old tree should not be referenced anymore.
            DEBUG_DESTROY_NODE(tree);
        }

        m_compiler->optVNAssertionPropStmtMorphPending = true;

        return newTree;
    }
};

// Perform VN based constant and non null propagation on the specified statement.
// If the statement is removed or if new statements were added before it this
// returns the next statement to process, otherwise it return null.
Statement* Compiler::optVNAssertionPropStmt(BasicBlock* block, Statement* stmt)
{
    // TODO-Review: EH successor/predecessor iteration seems broken.
    // See: SELF_HOST_TESTS_ARM\jit\Directed\ExcepFilters\fault\fault.exe
    if (block->bbCatchTyp == BBCT_FAULT)
    {
        return nullptr;
    }

    // Propagation may add statements before `stmt` or remove `stmt`, remember
    // the previous statment so we know where to continue from. The previous
    // statement will never be affected by propagation in `stmt`.
    Statement* prev = (stmt == block->GetFirstStatement()) ? nullptr : stmt->GetPrevStmt();

    optVNAssertionPropStmtMorphPending = false;

    VNConstPropVisitor visitor(this, block, stmt);
    visitor.WalkTree(stmt->GetRootNodePointer(), nullptr);

    if (optVNAssertionPropStmtMorphPending)
    {
        fgMorphBlockStmt(block, stmt DEBUGARG("optVNAssertionPropStmt"));
    }

    Statement* next = (prev == nullptr) ? block->GetFirstStatement() : prev->GetNextStmt();
    return next == stmt ? nullptr : next;
}

void Compiler::optVNAssertionProp()
{
    if (fgSsaPassesCompleted == 0)
    {
        return;
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("*************** In optVNAssertionProp()\n");
        printf("Blocks/Trees at start of phase\n");
        fgDispBasicBlocks(true);
    }
#endif

    optAssertionInit(false);

    noway_assert(optAssertionCount == 0);

    // Traverse all blocks, perform VN constant propagation and generate assertions.
    for (BasicBlock* block = fgFirstBB; block != nullptr; block = block->bbNext)
    {
        compCurBB           = block;
        fgRemoveRestOfBlock = false;

        for (Statement* stmt = block->GetFirstStatement(); stmt != nullptr;)
        {
            Statement* nextStmt = optVNAssertionPropStmt(block, stmt);

            if (nextStmt != nullptr)
            {
                // Propagation removed the current stmt or added new ones before it, continue
                // from the right statement. If new statements were added this means that we
                // will go back, process those statements and then process this statement again.
                // This should only happen when side effects are extracted from a JTRUE, then
                // the statement contains only a small JTRUE(EQ|NE(0, 0)) so this is not a
                // throughput issue.
                stmt = nextStmt;
                continue;
            }

            for (GenTree* tree = stmt->GetTreeList(); tree != nullptr; tree = tree->gtNext)
            {
                optAssertionGen(tree);
            }

            stmt = stmt->GetNextStmt();
        }
    }

    if (optAssertionCount == 0)
    {
        // Zero out the bbAssertionIn values, as these can be referenced in RangeCheck::MergeAssertion
        // and this is sharedstate with the CSE phase: bbCseIn
        //
        for (BasicBlock* block = fgFirstBB; block; block = block->bbNext)
        {
            block->bbAssertionIn = BitVecOps::MakeEmpty(apTraits);
        }
        return;
    }

#ifdef DEBUG
    fgDebugCheckLinks();
#endif

    // Allocate the bits for the predicate sensitive dataflow analysis
    bbJtrueAssertionOut    = optInitAssertionDataflowFlags();
    ASSERT_TP* jumpDestGen = optComputeAssertionGen();

    // Modified dataflow algorithm for available expressions.
    DataFlow                  flow(this);
    AssertionPropFlowCallback ap(this, bbJtrueAssertionOut, jumpDestGen);
    flow.ForwardAnalysis(ap);

    for (BasicBlock* block = fgFirstBB; block; block = block->bbNext)
    {
        // Compute any implied non-Null assertions for block->bbAssertionIn
        optImpliedByTypeOfAssertions(block->bbAssertionIn);
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("\n");
        for (BasicBlock* block = fgFirstBB; block; block = block->bbNext)
        {
            printf("\n" FMT_BB, block->bbNum);
            printf(" valueIn  = %s", BitVecOps::ToString(apTraits, block->bbAssertionIn));
            printf(" valueOut = %s", BitVecOps::ToString(apTraits, block->bbAssertionOut));
            if (block->bbJumpKind == BBJ_COND)
            {
                printf(" => " FMT_BB, block->bbJumpDest->bbNum);
                printf(" valueOut= %s", BitVecOps::ToString(apTraits, bbJtrueAssertionOut[block->bbNum]));
            }
        }
        printf("\n");
    }
#endif // DEBUG

    ASSERT_TP assertions = BitVecOps::MakeEmpty(apTraits);

    // Perform assertion propagation (and constant folding)
    for (BasicBlock* block = fgFirstBB; block; block = block->bbNext)
    {
        BitVecOps::Assign(apTraits, assertions, block->bbAssertionIn);

        // TODO-Review: EH successor/predecessor iteration seems broken.
        // SELF_HOST_TESTS_ARM\jit\Directed\ExcepFilters\fault\fault.exe
        if (block->bbCatchTyp == BBCT_FAULT)
        {
            continue;
        }

        // Make the current basic block address available globally.
        compCurBB           = block;
        fgRemoveRestOfBlock = false;

        // Walk the statement trees in this basic block
        Statement* stmt = block->FirstNonPhiDef();
        while (stmt != nullptr)
        {
            // Propagation tells us to remove the rest of the block. Remove it.
            if (fgRemoveRestOfBlock)
            {
                fgRemoveStmt(block, stmt);
                stmt = stmt->GetNextStmt();
                continue;
            }

            // Preserve the prev link before the propagation and morph, to check if propagation
            // removes the current stmt.
            Statement* prevStmt = (stmt == block->firstStmt()) ? nullptr : stmt->GetPrevStmt();

            optVNAssertionPropStmtMorphPending = false;

            for (GenTree* tree = stmt->GetTreeList(); tree != nullptr; tree = tree->gtNext)
            {
                JITDUMP("Propagating %s assertions for " FMT_BB ", stmt " FMT_STMT ", tree [%06d], tree -> %d\n",
                        BitVecOps::ToString(apTraits, assertions), block->bbNum, stmt->GetID(), dspTreeID(tree),
                        tree->GetAssertionInfo().GetAssertionIndex());

                GenTree* newTree = optAssertionProp(assertions, tree, stmt, block);
                if (newTree != nullptr)
                {
                    assert(optVNAssertionPropStmtMorphPending);
                    tree = newTree;
                }

                // If this tree makes an assertion - make it available.
                if (tree->GeneratesAssertion())
                {
                    AssertionInfo info = tree->GetAssertionInfo();
                    optImpliedAssertions(info.GetAssertionIndex(), assertions);
                    BitVecOps::AddElemD(apTraits, assertions, info.GetAssertionIndex() - 1);
                }
            }

            if (optVNAssertionPropStmtMorphPending)
            {
#ifdef DEBUG
                if (verbose)
                {
                    printf("Re-morphing this stmt:\n");
                    gtDispStmt(stmt);
                    printf("\n");
                }
#endif
                // Re-morph the statement.
                fgMorphBlockStmt(block, stmt DEBUGARG("optVNAssertionProp"));
            }

            // Check if propagation removed statements starting from current stmt.
            // If so, advance to the next good statement.
            Statement* nextStmt = (prevStmt == nullptr) ? block->firstStmt() : prevStmt->GetNextStmt();
            stmt                = (stmt == nextStmt) ? stmt->GetNextStmt() : nextStmt;
        }
    }

#ifdef DEBUG
    fgDebugCheckBBlist();
    fgDebugCheckLinks();
#endif
}
