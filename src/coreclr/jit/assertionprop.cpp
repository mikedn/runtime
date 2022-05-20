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

        if (varDsc->lvIsTemp || !varDsc->lvSingleDef || (typ == TYP_STRUCT))
        {
            continue;
        }

        /* For lvNormalizeOnLoad(), we need to add a cast to the copy-assignment
           like "copyLclNum = int(varDsc)" and apGenerateNodeAssertions() only
           tracks simple assignments. The same goes for lvNormalizedOnStore as
           the cast is generated in fgMorphSmpOpAsg. This boils down to not having
           a copy until apGenerateNodeAssertions handles this*/
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
        //
        // TODO-MIKE-CQ: So if this is done in order to get around DOUBLE parameter
        // alignment then why the crap it's also checking for FLOAT?!?

        bool isFloatParam = false;

#ifdef TARGET_X86
        isFloatParam = varDsc->lvIsParam && varTypeIsFloating(typ);
#endif

        if (!isFloatParam && !varDsc->lvEHLive)
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

        if (BlockSetOps::MayBeUninit(varDsc->lvUseBlocks))
        {
            // No references
            continue;
        }

        bool isDominatedByFirstBB = false;

        BlockSetOps::Iter iter(this, varDsc->lvUseBlocks);
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
        // or an EH live variable that is always reached from the first BB
        // and we have at least one block available in paramImportantUseDom
        bool doCopy = (isFloatParam || (isDominatedByFirstBB && varDsc->lvEHLive)) &&
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
                        BlockSetOps::IsMember(this, varDsc->lvUseBlocks, bestBlock->bbNum))
                           ? "start"
                           : "end",
                       bestBlock->bbNum);
            }
#endif

            if (BlockSetOps::IsEmpty(this, paramImportantUseDom) ||
                BlockSetOps::IsMember(this, varDsc->lvUseBlocks, bestBlock->bbNum))
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

using AssertionDsc = Compiler::AssertionDsc;

bool AssertionDsc::ComplementaryKind(ApKind kind, ApKind kind2)
{
    switch (kind)
    {
        case OAK_EQUAL:
            return kind2 == OAK_NOT_EQUAL;
        case OAK_NOT_EQUAL:
            return kind2 == OAK_EQUAL;
        default:
            return false;
    }
}

bool AssertionDsc::HasSameOp1(const AssertionDsc* that) const
{
    if (op1.kind != that->op1.kind)
    {
        return false;
    }

    if (op1.kind == O1K_ARR_BND)
    {
        return op1.bnd == that->op1.bnd;
    }

    return op1.vn == that->op1.vn;
}

bool AssertionDsc::HasSameOp2(const AssertionDsc* that) const
{
    if (op2.kind != that->op2.kind)
    {
        return false;
    }

    switch (op2.kind)
    {
        case O2K_LCLVAR_COPY:
            return op2.lcl == that->op2.lcl;
        case O2K_IND_CNS_INT:
        case O2K_CONST_INT:
            return op2.intCon == that->op2.intCon;
#ifndef TARGET_64BIT
        case O2K_CONST_LONG:
            return op2.lngCon == that->op2.lngCon;
#endif
        case O2K_CONST_DOUBLE:
            return op2.dblCon == that->op2.dblCon;
        case O2K_SUBRANGE:
            return op2.range == that->op2.range;
        default:
            assert(op2.kind == O2K_INVALID);
            break;
    }

    return false;
}

bool AssertionDsc::Complementary(const AssertionDsc* that) const
{
    return ComplementaryKind(kind, that->kind) && HasSameOp1(that) && HasSameOp2(that);
}

bool AssertionDsc::Equals(const AssertionDsc* that) const
{
    if (kind != that->kind)
    {
        return false;
    }

    if (kind == OAK_NO_THROW)
    {
        assert(op1.kind == O1K_ARR_BND);
        assert(op2.kind == O2K_INVALID);

        return op1.bnd == that->op1.bnd;
    }

    return HasSameOp1(that) && HasSameOp2(that);
}

void Compiler::apInit()
{
    // Use a function countFunc to determine a proper maximum assertion count for the
    // method being compiled. The function is linear to the IL size for small and
    // moderate methods. For large methods, considering throughput impact, we track no
    // more than 64 assertions.
    // Note this tracks at most only 256 assertions.

    static const AssertionIndex countFunc[]{64, 128, 256, 64};

    const unsigned upperBound = _countof(countFunc) - 1;
    const unsigned codeSize   = info.compILCodeSize / 512;

    CompAllocator allocator = getAllocator(CMK_AssertionProp);

    apMaxAssertionCount       = countFunc[min(upperBound, codeSize)];
    apAssertionTable          = new (allocator) AssertionDsc[apMaxAssertionCount];
    apComplementaryAssertions = new (allocator) AssertionIndex[apMaxAssertionCount + 1]();
    apVNAssertionMap          = new (allocator) ValueNumToAssertsMap(allocator);
    apTraits                  = new (allocator) BitVecTraits(apMaxAssertionCount, this);
    apAssertionCount          = 0;
    apJTrueAssertionOut       = nullptr;
}

Compiler::AssertionDsc* Compiler::apGetAssertion(AssertionIndex index)
{
    assert((1 <= index) && (index <= apAssertionCount));

    AssertionDsc* assertion = &apAssertionTable[index - 1];
    INDEBUG(apDebugCheckAssertion(assertion));
    return assertion;
}

AssertionIndex Compiler::apCreateNoThrowAssertion(GenTreeBoundsChk* boundsChk)
{
    ValueNum indexVN  = vnStore->VNNormalValue(boundsChk->GetIndex()->GetConservativeVN());
    ValueNum lengthVN = vnStore->VNNormalValue(boundsChk->GetLength()->GetConservativeVN());

    if ((indexVN == NoVN) || (lengthVN == NoVN))
    {
        return NO_ASSERTION_INDEX;
    }

    AssertionDsc assertion;
    memset(&assertion, 0, sizeof(AssertionDsc));

    assertion.kind          = OAK_NO_THROW;
    assertion.op1.kind      = O1K_ARR_BND;
    assertion.op1.bnd.vnIdx = indexVN;
    assertion.op1.bnd.vnLen = lengthVN;

    return apAddAssertion(&assertion);
}

AssertionIndex Compiler::apCreateNotNullAssertion(GenTree* addr)
{
    ssize_t offset = 0;

    while (addr->OperIs(GT_ADD) && addr->TypeIs(TYP_BYREF))
    {
        if (GenTreeIntCon* intCon = addr->AsOp()->GetOp(1)->IsIntCon())
        {
            offset += intCon->GetValue();
            addr = addr->AsOp()->GetOp(0);
        }
        else if (GenTreeIntCon* intCon = addr->AsOp()->GetOp(0)->IsIntCon())
        {
            offset += intCon->GetValue();
            addr = addr->AsOp()->GetOp(1);
        }
        else
        {
            break;
        }
    }

    if (!addr->OperIs(GT_LCL_VAR) || fgIsBigOffset(offset))
    {
        return NO_ASSERTION_INDEX;
    }

    unsigned   lclNum = addr->AsLclVar()->GetLclNum();
    LclVarDsc* lcl    = lvaGetDesc(lclNum);

    AssertionDsc assertion;

    if (lcl->TypeIs(TYP_REF))
    {
        if (lcl->IsAddressExposed())
        {
            return NO_ASSERTION_INDEX;
        }

        // TODO: only copy assertions rely on valid SSA number so we could generate more assertions here
        if (addr->AsLclVar()->GetSsaNum() == SsaConfig::RESERVED_SSA_NUM)
        {
            return NO_ASSERTION_INDEX;
        }

        memset(&assertion, 0, sizeof(AssertionDsc));
        assertion.op1.kind       = O1K_LCLVAR;
        assertion.op1.vn         = vnStore->VNNormalValue(addr->GetConservativeVN());
        assertion.op1.lcl.lclNum = lclNum;
        assertion.op1.lcl.ssaNum = addr->AsLclVar()->GetSsaNum();
    }
    else if (lcl->TypeIs(TYP_BYREF))
    {
        ValueNum  vn = vnStore->VNNormalValue(addr->GetConservativeVN());
        VNFuncApp funcApp;

        while (vnStore->GetVNFunc(vn, &funcApp) && (funcApp.m_func == static_cast<VNFunc>(GT_ADD)) &&
               (vnStore->TypeOfVN(vn) == TYP_BYREF))
        {
            if (vnStore->IsVNConstant(funcApp.m_args[1]) && varTypeIsIntegral(vnStore->TypeOfVN(funcApp.m_args[1])))
            {
                offset += vnStore->CoercedConstantValue<ssize_t>(funcApp.m_args[1]);
                vn = funcApp.m_args[0];
            }
            else if (vnStore->IsVNConstant(funcApp.m_args[0]) &&
                     varTypeIsIntegral(vnStore->TypeOfVN(funcApp.m_args[0])))
            {
                offset += vnStore->CoercedConstantValue<ssize_t>(funcApp.m_args[0]);
                vn = funcApp.m_args[1];
            }
            else
            {
                break;
            }
        }

        if (fgIsBigOffset(offset))
        {
            return NO_ASSERTION_INDEX;
        }

        memset(&assertion, 0, sizeof(AssertionDsc));
        assertion.op1.kind = O1K_VALUE_NUMBER;
        assertion.op1.vn   = vn;
    }
    else
    {
        return NO_ASSERTION_INDEX;
    }

    if ((assertion.op1.vn == ValueNumStore::NoVN) || (assertion.op1.vn == ValueNumStore::VNForVoid()))
    {
        return NO_ASSERTION_INDEX;
    }

    assertion.kind             = OAK_NOT_EQUAL;
    assertion.op2.kind         = O2K_CONST_INT;
    assertion.op2.vn           = ValueNumStore::VNForNull();
    assertion.op2.intCon.value = 0;
    assertion.op2.intCon.flags = GTF_EMPTY;
#ifdef TARGET_64BIT
    assertion.op2.intCon.flags |= GTF_ASSERTION_PROP_LONG;
#endif

    return apAddAssertion(&assertion);
}

static ssize_t GetLowerBoundForIntegralType(var_types type)
{
    switch (type)
    {
        case TYP_BYTE:
            return SCHAR_MIN;
        case TYP_SHORT:
            return SHRT_MIN;
        case TYP_INT:
            return INT_MIN;
        case TYP_BOOL:
        case TYP_UBYTE:
        case TYP_USHORT:
        case TYP_UINT:
            return 0;
        default:
            unreached();
    }
}

static ssize_t GetUpperBoundForIntegralType(var_types type)
{
    switch (type)
    {
        case TYP_BOOL:
            return 1;
        case TYP_BYTE:
            return SCHAR_MAX;
        case TYP_SHORT:
            return SHRT_MAX;
        case TYP_INT:
            return INT_MAX;
        case TYP_UBYTE:
            return UCHAR_MAX;
        case TYP_USHORT:
            return USHRT_MAX;
        case TYP_UINT:
            return UINT_MAX;
        default:
            unreached();
    }
}

AssertionIndex Compiler::apCreateSubrangeAssertion(GenTreeCast* cast)
{
    GenTree* value = cast->GetOp(0);

    if (!value->OperIs(GT_LCL_VAR))
    {
        return NO_ASSERTION_INDEX;
    }

    if (varTypeIsFloating(value->GetType()))
    {
        return NO_ASSERTION_INDEX;
    }

    // TODO: only copy assertions rely on valid SSA number so we could generate more assertions here
    if (value->AsLclVar()->GetSsaNum() == SsaConfig::RESERVED_SSA_NUM)
    {
        return NO_ASSERTION_INDEX;
    }

    if (value->GetConservativeVN() == ValueNumStore::NoVN)
    {
        return NO_ASSERTION_INDEX;
    }

    LclVarDsc* lcl = lvaGetDesc(value->AsLclVar());

    if (lcl->IsAddressExposed())
    {
        return NO_ASSERTION_INDEX;
    }

    if (lcl->IsPromotedField() && lcl->lvNormalizeOnLoad())
    {
        return NO_ASSERTION_INDEX;
    }

    var_types toType = cast->GetCastType();

    // Casts to TYP_UINT produce the same ranges as casts to TYP_INT,
    // except in overflow cases which we do not yet handle. To avoid
    // issues with the propagation code dropping, e. g., CAST_OVF(uint <- int)
    // based on an assertion created from CAST(uint <- ulong), normalize the
    // type for the range here. Note that TYP_ULONG theoretically has the same
    // problem, but we do not create assertions for it.
    // TODO-Cleanup: this assertion is not useful - this code exists to preserve
    // previous behavior. Refactor it to stop generating such assertions.
    if (toType == TYP_UINT)
    {
        toType = TYP_INT;
    }

    AssertionDsc::Range range;

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
#endif
            range = {GetLowerBoundForIntegralType(toType), GetUpperBoundForIntegralType(toType)};
            break;

        default:
            return NO_ASSERTION_INDEX;
    }

    AssertionDsc assertion;
    memset(&assertion, 0, sizeof(AssertionDsc));

    assertion.kind           = OAK_SUBRANGE;
    assertion.op1.kind       = O1K_LCLVAR;
    assertion.op1.vn         = vnStore->VNNormalValue(value->GetConservativeVN());
    assertion.op1.lcl.lclNum = value->AsLclVar()->GetLclNum();
    assertion.op1.lcl.ssaNum = value->AsLclVar()->GetSsaNum();
    assertion.op2.kind       = O2K_SUBRANGE;
    assertion.op2.range      = range;

    return apAddAssertion(&assertion);
}

AssertionIndex Compiler::apCreateEqualityAssertion(GenTreeLclVar* op1, GenTree* op2, ApKind kind)
{
    assert((op1 != nullptr) && (op2 != nullptr));
    assert(op1->OperIs(GT_LCL_VAR));
    assert((kind == OAK_EQUAL) || (kind == OAK_NOT_EQUAL));

    LclVarDsc* lcl = lvaGetDesc(op1->AsLclVar());

    if (lcl->IsAddressExposed())
    {
        return NO_ASSERTION_INDEX;
    }

    // TODO: only copy assertions rely on valid SSA number so we could generate more assertions here
    if (op1->GetSsaNum() == SsaConfig::RESERVED_SSA_NUM)
    {
        return NO_ASSERTION_INDEX;
    }

    op2 = op2->SkipComma();

    AssertionDsc assertion;
    memset(&assertion, 0, sizeof(AssertionDsc));

    switch (op2->GetOper())
    {
        var_types toType;

        case GT_CNS_INT:
#ifdef TARGET_ARM
            if (!codeGen->validImmForMov(op2->AsIntCon()->GetInt32Value()))
            {
                return NO_ASSERTION_INDEX;
            }
#endif

            if (lcl->TypeIs(TYP_LONG) && !op2->TypeIs(TYP_LONG))
            {
                return NO_ASSERTION_INDEX;
            }

            assertion.op2.kind         = O2K_CONST_INT;
            assertion.op2.vn           = vnStore->VNNormalValue(op2->GetConservativeVN());
            assertion.op2.intCon.value = op2->AsIntCon()->GetValue(lcl->GetType());
            assertion.op2.intCon.flags = op2->AsIntCon()->GetHandleKind();
#ifdef TARGET_64BIT
            if (op2->TypeIs(TYP_LONG, TYP_BYREF))
            {
                assertion.op2.intCon.flags |= GTF_ASSERTION_PROP_LONG;
            }
#endif
            break;

#ifndef TARGET_64BIT
        case GT_CNS_LNG:
            assertion.op2.kind         = O2K_CONST_LONG;
            assertion.op2.vn           = vnStore->VNNormalValue(op2->GetConservativeVN());
            assertion.op2.lngCon.value = op2->AsLngCon()->GetValue();
            break;
#endif

        case GT_CNS_DBL:
            if (_isnan(op2->AsDblCon()->GetValue()))
            {
                return NO_ASSERTION_INDEX;
            }

            assertion.op2.kind         = O2K_CONST_DOUBLE;
            assertion.op2.vn           = vnStore->VNNormalValue(op2->GetConservativeVN());
            assertion.op2.dblCon.value = op2->AsDblCon()->GetValue();
            break;

        case GT_LCL_VAR:
        {
            if (op1->GetLclNum() == op2->AsLclVar()->GetLclNum())
            {
                return NO_ASSERTION_INDEX;
            }

            LclVarDsc* valLcl = lvaGetDesc(op2->AsLclVar());

            if (lcl->GetType() != valLcl->GetType())
            {
                return NO_ASSERTION_INDEX;
            }

            if (valLcl->lvNormalizeOnLoad() && !lcl->lvNormalizeOnLoad())
            {
                return NO_ASSERTION_INDEX;
            }

            if (valLcl->IsAddressExposed())
            {
                return NO_ASSERTION_INDEX;
            }

            assertion.op2.kind       = O2K_LCLVAR_COPY;
            assertion.op2.vn         = vnStore->VNNormalValue(op2->GetConservativeVN());
            assertion.op2.lcl.lclNum = op2->AsLclVar()->GetLclNum();
            assertion.op2.lcl.ssaNum = op2->AsLclVar()->GetSsaNum();
            break;
        }

        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GT:
        case GT_GE:
            toType = TYP_BOOL;
            goto SUBRANGE_COMMON;
        case GT_LCL_FLD:
            toType = op2->GetType();
            goto SUBRANGE_COMMON;
        case GT_IND:
            toType = op2->GetType();
            goto SUBRANGE_COMMON;

        case GT_CAST:
        {
            if (lcl->IsPromotedField() && lcl->lvNormalizeOnLoad())
            {
                return NO_ASSERTION_INDEX;
            }

            toType = op2->AsCast()->GetCastType();

            // Casts to TYP_UINT produce the same ranges as casts to TYP_INT,
            // except in overflow cases which we do not yet handle. To avoid
            // issues with the propagation code dropping, e. g., CAST_OVF(uint <- int)
            // based on an assertion created from CAST(uint <- ulong), normalize the
            // type for the range here. Note that TYP_ULONG theoretically has the same
            // problem, but we do not create assertions for it.
            // TODO-Cleanup: this assertion is not useful - this code exists to preserve
            // previous behavior. Refactor it to stop generating such assertions.
            if (toType == TYP_UINT)
            {
                toType = TYP_INT;
            }

        SUBRANGE_COMMON:
            if (varTypeIsFloating(op1->GetType()))
            {
                return NO_ASSERTION_INDEX;
            }

            AssertionDsc::Range range;

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
#endif
                    range = {GetLowerBoundForIntegralType(toType), GetUpperBoundForIntegralType(toType)};
                    break;

                default:
                    return NO_ASSERTION_INDEX;
            }

            assertion.op2.kind  = O2K_SUBRANGE;
            assertion.op2.range = range;
        }
        break;

        default:
            return NO_ASSERTION_INDEX;
    }

    if (assertion.op2.vn == NoVN)
    {
        return NO_ASSERTION_INDEX;
    }

    assertion.kind           = kind;
    assertion.op1.kind       = O1K_LCLVAR;
    assertion.op1.vn         = vnStore->VNNormalValue(op1->GetConservativeVN());
    assertion.op1.lcl.lclNum = op1->GetLclNum();
    assertion.op1.lcl.ssaNum = op1->GetSsaNum();

    if (assertion.op1.vn == NoVN)
    {
        return NO_ASSERTION_INDEX;
    }

    if (assertion.op2.kind == O2K_SUBRANGE)
    {
        return kind == OAK_EQUAL ? apAddAssertion(&assertion) : NO_ASSERTION_INDEX;
    }

    if (apAssertionHasNanVN(&assertion))
    {
        return NO_ASSERTION_INDEX;
    }

    AssertionIndex index = apAddAssertion(&assertion);

    if (index != NO_ASSERTION_INDEX)
    {
        assertion.kind = kind == OAK_EQUAL ? OAK_NOT_EQUAL : OAK_EQUAL;
        apAddComplementaryAssertion(index, apAddAssertion(&assertion));
    }

    return index;
}

AssertionIndex Compiler::apCreateExactTypeAssertion(GenTreeIndir* op1, GenTree* op2, ApKind kind)
{
    assert((op1 != nullptr) && (op2 != nullptr));
    assert(op1->OperIs(GT_IND));
    assert((kind == OAK_EQUAL) || (kind == OAK_NOT_EQUAL));

    GenTreeLclVar* addr = op1->GetAddr()->AsLclVar();
    assert(addr->OperIs(GT_LCL_VAR));

    if (!addr->TypeIs(TYP_REF))
    {
        return NO_ASSERTION_INDEX;
    }

    // TODO: only copy assertions rely on valid SSA number so we could generate more assertions here
    if (addr->GetSsaNum() == SsaConfig::RESERVED_SSA_NUM)
    {
        return NO_ASSERTION_INDEX;
    }

    unsigned   lclNum = addr->GetLclNum();
    LclVarDsc* lcl    = lvaGetDesc(lclNum);

    assert(lcl->IsInSsa());

    AssertionDsc assertion;
    memset(&assertion, 0, sizeof(AssertionDsc));

    // Ngen case
    if (op2->OperIs(GT_IND))
    {
        op2 = op2->AsIndir()->GetAddr();

        assertion.op2.kind = O2K_IND_CNS_INT;
    }
    // JIT case
    else
    {
        assertion.op2.kind = O2K_CONST_INT;
    }

    if (!apIsConstInt(op2, &assertion.op2.intCon.value, &assertion.op2.intCon.flags))
    {
        return NO_ASSERTION_INDEX;
    }

    assert((assertion.op2.intCon.flags & ~GTF_ICON_HDL_MASK) == 0);

    ValueNum vn1 = vnStore->VNNormalValue(addr->GetConservativeVN());
    ValueNum vn2 = vnStore->VNNormalValue(op2->GetConservativeVN());

    if ((vn1 == NoVN) || (vn2 == NoVN))
    {
        return NO_ASSERTION_INDEX;
    }

    assert(vn1 == vnStore->VNNormalValue(lcl->GetPerSsaData(addr->GetSsaNum())->GetConservativeVN()));

    assertion.kind           = kind;
    assertion.op1.kind       = O1K_EXACT_TYPE;
    assertion.op1.vn         = vn1;
    assertion.op1.lcl.lclNum = addr->GetLclNum();
    assertion.op1.lcl.ssaNum = addr->GetSsaNum();
    assertion.op2.vn         = vn2;

#ifdef TARGET_64BIT
    if (op2->TypeIs(TYP_LONG))
    {
        assertion.op2.intCon.flags |= GTF_ASSERTION_PROP_LONG;
    }
#endif

    AssertionIndex index = apAddAssertion(&assertion);

    if (index != NO_ASSERTION_INDEX)
    {
        assertion.kind = kind == OAK_EQUAL ? OAK_NOT_EQUAL : OAK_EQUAL;
        apAddComplementaryAssertion(index, apAddAssertion(&assertion));
        apCreateNotNullAssertion(op1);
    }

    return index;
}

AssertionIndex Compiler::apCreateSubtypeAssertion(GenTreeLclVar* op1, GenTree* op2, ApKind kind)
{
    assert((op1 != nullptr) && (op2 != nullptr));
    assert(op1->OperIs(GT_LCL_VAR));
    assert((kind == OAK_EQUAL) || (kind == OAK_NOT_EQUAL));

    // TODO: only copy assertions rely on valid SSA number so we could generate more assertions here
    if (op1->GetSsaNum() == SsaConfig::RESERVED_SSA_NUM)
    {
        return NO_ASSERTION_INDEX;
    }

    if (lvaGetDesc(op1)->IsAddressExposed())
    {
        return NO_ASSERTION_INDEX;
    }

    AssertionDsc assertion;
    memset(&assertion, 0, sizeof(AssertionDsc));

    if (op2->OperIs(GT_IND))
    {
        op2 = op2->AsIndir()->GetAddr();

        assertion.op2.kind = O2K_IND_CNS_INT;
    }
    else
    {
        assertion.op2.kind = O2K_CONST_INT;
    }

    if (!op2->IsIntCon())
    {
        return NO_ASSERTION_INDEX;
    }

    ValueNum vn1 = vnStore->VNNormalValue(op1->GetConservativeVN());
    ValueNum vn2 = vnStore->VNNormalValue(op2->GetConservativeVN());

    if ((vn1 == NoVN) || (vn2 == NoVN))
    {
        return NO_ASSERTION_INDEX;
    }

    assertion.kind             = kind;
    assertion.op1.kind         = O1K_SUBTYPE;
    assertion.op1.vn           = vn1;
    assertion.op1.lcl.lclNum   = op1->AsLclVar()->GetLclNum();
    assertion.op1.lcl.ssaNum   = op1->AsLclVar()->GetSsaNum();
    assertion.op2.vn           = vn2;
    assertion.op2.intCon.value = op2->AsIntCon()->GetValue();
    assertion.op2.intCon.flags = op2->GetIconHandleFlag();

    AssertionIndex index = apAddAssertion(&assertion);

    if (index != NO_ASSERTION_INDEX)
    {
        assertion.kind = kind == OAK_EQUAL ? OAK_NOT_EQUAL : OAK_EQUAL;
        apAddComplementaryAssertion(index, apAddAssertion(&assertion));
        apCreateNotNullAssertion(op1);
    }

    return index;
}

bool Compiler::apIsConstInt(GenTree* node, ssize_t* value, GenTreeFlags* flags)
{
    ValueNum vn = vnStore->VNNormalValue(node->GetConservativeVN());

    if (!vnStore->IsVNConstant(vn))
    {
        return false;
    }

    switch (vnStore->TypeOfVN(vn))
    {
        case TYP_INT:
            *value = vnStore->ConstantValue<int>(vn);
            *flags = vnStore->IsVNHandle(vn) ? vnStore->GetHandleFlags(vn) : GTF_EMPTY;

            return true;

#ifdef TARGET_64BIT
        case TYP_LONG:
            *value = vnStore->ConstantValue<INT64>(vn);
            *flags = vnStore->IsVNHandle(vn) ? vnStore->GetHandleFlags(vn) : GTF_EMPTY;

            return true;
#endif

        default:
            return false;
    }
}

bool Compiler::apAssertionHasNanVN(AssertionDsc* assertion)
{
    ValueNum vns[]{assertion->op1.vn, assertion->op2.vn};

    for (ValueNum vn : vns)
    {
        if (vnStore->IsVNConstant(vn))
        {
            var_types type = vnStore->TypeOfVN(vn);

            if (((type == TYP_FLOAT) && _isnan(vnStore->ConstantValue<float>(vn))) ||
                ((type == TYP_DOUBLE) && _isnan(vnStore->ConstantValue<double>(vn))))
            {
                return true;
            }
        }
    }

    return false;
}

AssertionIndex Compiler::apAddAssertion(AssertionDsc* assertion)
{
    assert(assertion->kind != OAK_INVALID);

    for (AssertionIndex index = apAssertionCount; index >= 1; index--)
    {
        if (apGetAssertion(index)->Equals(assertion))
        {
            return index;
        }
    }

    if (apAssertionCount >= apMaxAssertionCount)
    {
        return NO_ASSERTION_INDEX;
    }

    apAssertionTable[apAssertionCount++] = *assertion;

#ifdef DEBUG
    if (verbose)
    {
        printf(FMT_BB " ", compCurBB->bbNum);
        gtDispTree(optAssertionPropCurrentTree, nullptr, nullptr, true);
        printf("Generates ");
        apDumpAssertion(&apAssertionTable[apAssertionCount - 1]);
    }
#endif

    if (assertion->kind != OAK_NO_THROW)
    {
        apAddVNAssertion(assertion->op1.vn, apAssertionCount);

        if (assertion->op2.kind == O2K_LCLVAR_COPY)
        {
            apAddVNAssertion(assertion->op2.vn, apAssertionCount);
        }
    }

    INDEBUG(apDebugCheckAssertionTable());

    return apAssertionCount;
}

void Compiler::apAddVNAssertion(ValueNum vn, AssertionIndex index)
{
    ASSERT_TP* set = apVNAssertionMap->Emplace(vn);

    if (*set == BitVecOps::UninitVal())
    {
        *set = BitVecOps::MakeSingleton(apTraits, index - 1);
    }
    else
    {
        BitVecOps::AddElemD(apTraits, *set, index - 1);
    }
}

ASSERT_VALRET_TP Compiler::apGetVNAssertions(ValueNum vn)
{
    ASSERT_TP* set = apVNAssertionMap->LookupPointer(vn);

    return set == nullptr ? BitVecOps::UninitVal() : *set;
}

void Compiler::apAddComplementaryAssertion(AssertionIndex index, AssertionIndex complementaryIndex)
{
    if ((index == NO_ASSERTION_INDEX) || (complementaryIndex == NO_ASSERTION_INDEX))
    {
        return;
    }

    assert(index <= apMaxAssertionCount);
    assert(complementaryIndex <= apMaxAssertionCount);

    apComplementaryAssertions[index]              = complementaryIndex;
    apComplementaryAssertions[complementaryIndex] = index;
}

AssertionIndex Compiler::apFindComplementaryAssertion(AssertionIndex index)
{
    if (index == NO_ASSERTION_INDEX)
    {
        return NO_ASSERTION_INDEX;
    }

    AssertionDsc* assertion = apGetAssertion(index);

    if ((assertion->kind != OAK_EQUAL) && (assertion->kind != OAK_NOT_EQUAL))
    {
        return NO_ASSERTION_INDEX;
    }

    AssertionIndex complementaryIndex = apComplementaryAssertions[index];

    if (complementaryIndex != NO_ASSERTION_INDEX)
    {
        return complementaryIndex;
    }

    // TODO-MIKE-Review: Why is this needed? Just in case someone forgets to map the complementary
    // assertion when generating assertions?
    for (complementaryIndex = 1; complementaryIndex <= apAssertionCount; ++complementaryIndex)
    {
        AssertionDsc* complementaryAssertion = apGetAssertion(complementaryIndex);

        if (complementaryAssertion->Complementary(assertion))
        {
            apAddComplementaryAssertion(index, complementaryIndex);

            return complementaryIndex;
        }
    }

    return NO_ASSERTION_INDEX;
}

AssertionIndex Compiler::apAddBoundAssertions(AssertionDsc* assertion)
{
    AssertionIndex index = apAddAssertion(assertion);

    if (index != NO_ASSERTION_INDEX)
    {
        AssertionDsc complementary = *apGetAssertion(index);

        assert((complementary.op1.kind == O1K_BOUND_OPER_BND) || (complementary.op1.kind == O1K_BOUND_LOOP_BND) ||
               (complementary.op1.kind == O1K_CONSTANT_LOOP_BND));

        complementary.kind = complementary.kind == OAK_EQUAL ? OAK_NOT_EQUAL : OAK_EQUAL;
        apAddAssertion(&complementary);
    }

    return index;
}

AssertionInfo Compiler::apGenerateJTrueBoundAssertions(GenTreeUnOp* jtrue)
{
    GenTree* relop = jtrue->GetOp(0);

    if (!relop->OperIsCompare())
    {
        return NO_ASSERTION_INDEX;
    }

    GenTree* op1 = relop->AsOp()->GetOp(0);
    GenTree* op2 = relop->AsOp()->GetOp(1);

    ValueNum op1VN   = vnStore->VNNormalValue(op1->GetConservativeVN());
    ValueNum op2VN   = vnStore->VNNormalValue(op2->GetConservativeVN());
    ValueNum relopVN = vnStore->VNNormalValue(relop->GetConservativeVN());

    bool hasTestAgainstZero = relop->OperIs(GT_EQ, GT_NE) && (op2VN == vnStore->VNZeroForType(op2->GetType()));

    AssertionDsc dsc;

    // Cases where op1 holds the upper bound arithmetic and op2 is 0.
    // Loop condition like: "i < bnd +/-k == 0"
    // Assertion: "i < bnd +/- k == 0"
    if (hasTestAgainstZero && vnStore->IsVNCompareCheckedBoundArith(op1VN))
    {
        dsc.kind             = relop->OperIs(GT_EQ) ? OAK_EQUAL : OAK_NOT_EQUAL;
        dsc.op1.kind         = O1K_BOUND_OPER_BND;
        dsc.op1.vn           = op1VN;
        dsc.op2.kind         = O2K_CONST_INT;
        dsc.op2.vn           = vnStore->VNZeroForType(op2->GetType());
        dsc.op2.intCon.value = 0;
        dsc.op2.intCon.flags = GTF_EMPTY;

        return apAddBoundAssertions(&dsc);
    }

    // Cases where op1 holds the lhs of the condition and op2 holds the bound arithmetic.
    // Loop condition like: "i < bnd +/-k"
    // Assertion: "i < bnd +/- k != 0"
    if (vnStore->IsVNCompareCheckedBoundArith(relopVN))
    {
        dsc.kind             = OAK_NOT_EQUAL;
        dsc.op1.kind         = O1K_BOUND_OPER_BND;
        dsc.op1.vn           = relopVN;
        dsc.op2.kind         = O2K_CONST_INT;
        dsc.op2.vn           = vnStore->VNZeroForType(op2->GetType());
        dsc.op2.intCon.value = 0;
        dsc.op2.intCon.flags = GTF_EMPTY;

        return apAddBoundAssertions(&dsc);
    }

    // Cases where op1 holds the upper bound and op2 is 0.
    // Loop condition like: "i < bnd == 0"
    // Assertion: "i < bnd == false"
    if (hasTestAgainstZero && vnStore->IsVNCompareCheckedBound(op1VN))
    {
        dsc.kind             = relop->OperIs(GT_EQ) ? OAK_EQUAL : OAK_NOT_EQUAL;
        dsc.op1.kind         = O1K_BOUND_LOOP_BND;
        dsc.op1.vn           = op1VN;
        dsc.op2.kind         = O2K_CONST_INT;
        dsc.op2.vn           = vnStore->VNZeroForType(op2->GetType());
        dsc.op2.intCon.value = 0;
        dsc.op2.intCon.flags = GTF_EMPTY;

        return apAddBoundAssertions(&dsc);
    }

    // Cases where op1 holds the lhs of the condition op2 holds the bound.
    // Loop condition like "i < bnd"
    // Assertion: "i < bnd != 0"
    if (vnStore->IsVNCompareCheckedBound(relopVN))
    {
        dsc.kind             = OAK_NOT_EQUAL;
        dsc.op1.kind         = O1K_BOUND_LOOP_BND;
        dsc.op1.vn           = relopVN;
        dsc.op2.kind         = O2K_CONST_INT;
        dsc.op2.vn           = vnStore->VNZeroForType(TYP_INT);
        dsc.op2.intCon.value = 0;
        dsc.op2.intCon.flags = GTF_EMPTY;

        return apAddBoundAssertions(&dsc);
    }

    ValueNumStore::UnsignedCompareCheckedBoundInfo unsignedCompareBnd;

    // Loop condition like "(uint)i < (uint)bnd" or equivalent
    // Assertion: "no throw" since this condition guarantees that i is both >= 0 and < bnd (on the appropiate edge)
    if (vnStore->IsVNUnsignedCompareCheckedBound(relopVN, &unsignedCompareBnd))
    {
        assert(unsignedCompareBnd.vnIdx != ValueNumStore::NoVN);
        assert((unsignedCompareBnd.cmpOper == VNF_LT_UN) || (unsignedCompareBnd.cmpOper == VNF_GE_UN));
        assert(vnStore->IsVNCheckedBound(unsignedCompareBnd.vnBound));

        dsc.kind          = OAK_NO_THROW;
        dsc.op1.kind      = O1K_ARR_BND;
        dsc.op1.vn        = relopVN;
        dsc.op1.bnd.vnIdx = unsignedCompareBnd.vnIdx;
        dsc.op1.bnd.vnLen = vnStore->VNNormalValue(unsignedCompareBnd.vnBound);
        dsc.op2.kind      = O2K_INVALID;
        dsc.op2.vn        = ValueNumStore::NoVN;

        AssertionIndex index = apAddAssertion(&dsc);

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
    if (hasTestAgainstZero && vnStore->IsVNConstantBound(op1VN))
    {
        dsc.kind             = relop->OperIs(GT_EQ) ? OAK_EQUAL : OAK_NOT_EQUAL;
        dsc.op1.kind         = O1K_CONSTANT_LOOP_BND;
        dsc.op1.vn           = op1VN;
        dsc.op2.kind         = O2K_CONST_INT;
        dsc.op2.vn           = vnStore->VNZeroForType(op2->GetType());
        dsc.op2.intCon.value = 0;
        dsc.op2.intCon.flags = GTF_EMPTY;

        return apAddBoundAssertions(&dsc);
    }

    // Cases where op1 holds the lhs of the condition op2 holds rhs.
    // Loop condition like "i < 100"
    // Assertion: "i < 100 != 0"
    if (vnStore->IsVNConstantBound(relopVN))
    {
        dsc.kind             = OAK_NOT_EQUAL;
        dsc.op1.kind         = O1K_CONSTANT_LOOP_BND;
        dsc.op1.vn           = relopVN;
        dsc.op2.kind         = O2K_CONST_INT;
        dsc.op2.vn           = vnStore->VNZeroForType(TYP_INT);
        dsc.op2.intCon.value = 0;
        dsc.op2.intCon.flags = GTF_EMPTY;

        return apAddBoundAssertions(&dsc);
    }

    return NO_ASSERTION_INDEX;
}

AssertionInfo Compiler::apGenerateJTrueAssertions(GenTreeUnOp* jtrue)
{
    GenTree* relop = jtrue->GetOp(0);

    if (!relop->OperIsCompare())
    {
        return NO_ASSERTION_INDEX;
    }

    AssertionInfo info = apGenerateJTrueBoundAssertions(jtrue);

    if (info.HasAssertion())
    {
        return info;
    }

    Compiler::ApKind assertionKind = OAK_INVALID;

    switch (relop->GetOper())
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
    GenTree* op1 = relop->AsOp()->GetOp(0)->gtCommaAssignVal();
    GenTree* op2 = relop->AsOp()->GetOp(1)->gtCommaAssignVal();

    if (!op1->OperIs(GT_LCL_VAR) && op2->OperIs(GT_LCL_VAR))
    {
        std::swap(op1, op2);
    }

    if (op1->OperIs(GT_LCL_VAR) && (op2->OperIsConst() || op2->OperIs(GT_LCL_VAR)))
    {
        return apCreateEqualityAssertion(op1->AsLclVar(), op2, assertionKind);
    }

    ValueNum op1VN = vnStore->VNNormalValue(op1->GetConservativeVN());
    ValueNum op2VN = vnStore->VNNormalValue(op2->GetConservativeVN());

    if (vnStore->IsVNCheckedBound(op1VN) && vnStore->IsVNInt32Constant(op2VN))
    {
        assert(relop->OperIs(GT_EQ, GT_NE));

        int op2Value = vnStore->ConstantValue<int>(op2VN);
        if (op2Value >= 0)
        {
            AssertionDsc dsc;

            dsc.kind             = op2Value == 0 ? OAK_NOT_EQUAL : OAK_EQUAL;
            dsc.op1.kind         = O1K_VALUE_NUMBER;
            dsc.op1.vn           = op1VN;
            dsc.op2.kind         = O2K_CONST_INT;
            dsc.op2.vn           = op2VN;
            dsc.op2.intCon.value = op2Value;
            dsc.op2.intCon.flags = GTF_EMPTY;

            AssertionIndex index = apAddAssertion(&dsc);

            if (relop->OperIs(GT_NE) != (dsc.kind == OAK_NOT_EQUAL))
            {
                return AssertionInfo::ForNextEdge(index);
            }

            return AssertionInfo(index);
        }
    }

    if ((!op1->OperIs(GT_IND) || !op1->AsIndir()->GetAddr()->OperIs(GT_LCL_VAR)) &&
        (op2->OperIs(GT_IND) && op2->AsIndir()->GetAddr()->OperIs(GT_LCL_VAR)))
    {
        std::swap(op1, op2);
    }

    if (op1->OperIs(GT_IND) && op1->AsIndir()->GetAddr()->OperIs(GT_LCL_VAR))
    {
        return apCreateExactTypeAssertion(op1->AsIndir(), op2, assertionKind);
    }

    if (!op2->OperIs(GT_CNS_INT) && op1->OperIs(GT_CNS_INT))
    {
        std::swap(op1, op2);
    }

    if (GenTreeCall* call = op1->IsCall())
    {
        if (!call->IsHelperCall() || !call->TypeIs(TYP_REF) || !op2->IsIntegralConst(0))
        {
            return NO_ASSERTION_INDEX;
        }

        CorInfoHelpFunc helper = eeGetHelperNum(call->GetMethodHandle());

        // Note CORINFO_HELP_READYTORUN_ISINSTANCEOF does not have the same argument pattern.
        // In particular, it is not possible to deduce what class is being tested from its args.
        // Also note The CASTCLASS helpers won't appear in predicates as they throw on failure.
        // So the helper list here is smaller than the one in apPropagateCall.

        if ((helper == CORINFO_HELP_ISINSTANCEOFINTERFACE) || (helper == CORINFO_HELP_ISINSTANCEOFARRAY) ||
            (helper == CORINFO_HELP_ISINSTANCEOFCLASS) || (helper == CORINFO_HELP_ISINSTANCEOFANY))
        {
            GenTree* objectArg      = call->GetArgNodeByArgNum(1);
            GenTree* methodTableArg = call->GetArgNodeByArgNum(0);

            assert(objectArg->TypeIs(TYP_REF));
            assert(methodTableArg->TypeIs(TYP_I_IMPL));

            if (objectArg->OperIs(GT_LCL_VAR))
            {
                // Reverse the assertion
                assertionKind = (assertionKind == OAK_EQUAL) ? OAK_NOT_EQUAL : OAK_EQUAL;

                return apCreateSubtypeAssertion(objectArg->AsLclVar(), methodTableArg, assertionKind);
            }
        }
    }

    return NO_ASSERTION_INDEX;
}

AssertionIndex Compiler::apGeneratePhiAssertions(GenTreeOp* asg)
{
    assert(asg->IsPhiDefn());

    bool allNotNull = true;

    for (GenTreePhi::Use& use : asg->GetOp(1)->AsPhi()->Uses())
    {
        if (!vnStore->IsKnownNonNull(use.GetNode()->GetConservativeVN()))
        {
            allNotNull = false;
            break;
        }
    }

    if (!allNotNull)
    {
        return NO_ASSERTION_INDEX;
    }

    return apCreateNotNullAssertion(asg->GetOp(0));
}

void Compiler::apGenerateNodeAssertions(GenTree* node)
{
    node->ClearAssertion();

    INDEBUG(optAssertionPropCurrentTree = node);

    bool          assertionIsTrue = true;
    AssertionInfo assertionInfo;

    switch (node->GetOper())
    {
        case GT_ASG:
            if (node->IsPhiDefn())
            {
                assertionInfo = apGeneratePhiAssertions(node->AsOp());
            }
            break;

        case GT_BLK:
        case GT_OBJ:
            assert(node->AsBlk()->GetLayout()->GetSize() != 0);
            FALLTHROUGH;
        case GT_IND:
        case GT_NULLCHECK:
            assertionInfo = apCreateNotNullAssertion(node->AsIndir()->GetAddr());
            break;
        case GT_ARR_LENGTH:
            assertionInfo = apCreateNotNullAssertion(node->AsArrLen()->GetArray());
            break;
        case GT_ARR_BOUNDS_CHECK:
            assertionInfo = apCreateNoThrowAssertion(node->AsBoundsChk());
            break;
        case GT_ARR_ELEM:
            assertionInfo = apCreateNotNullAssertion(node->AsArrElem()->GetArray());
            break;

        case GT_CALL:
            // A virtual call can create a non-null assertion. We transform some virtual calls
            // into non-virtual calls with a GTF_CALL_NULLCHECK flag set.
            // Ignore tail calls because they have 'this` pointer in the regular arg list and
            // an implicit null check.
            {
                GenTreeCall* call = node->AsCall();

                if (call->NeedsNullCheck() || (call->IsVirtual() && !call->IsTailCall()))
                {
                    assertionInfo = apCreateNotNullAssertion(call->GetThisArg());
                }
            }
            break;

        case GT_CAST:
            assertionInfo = apCreateSubrangeAssertion(node->AsCast());

            // This represets an assertion that we would like to prove to be true.
            // It is not actually a true assertion. If we can prove this assertion
            // true then we can eliminate this cast.
            assertionIsTrue = false;
            break;

        case GT_JTRUE:
            assertionInfo = apGenerateJTrueAssertions(node->AsUnOp());
            break;

        default:
            break;
    }

    if (assertionInfo.HasAssertion() && assertionIsTrue)
    {
        node->SetAssertionInfo(assertionInfo);
    }
}

AssertionIndex Compiler::apAssertionIsSubrange(ASSERT_VALARG_TP assertions,
                                               ValueNum         vn,
                                               var_types        fromType,
                                               var_types        toType)
{
    for (BitVecOps::Enumerator en(apTraits, assertions); en.MoveNext();)
    {
        AssertionIndex index     = GetAssertionIndex(en.Current());
        AssertionDsc*  assertion = apGetAssertion(index);

        if ((assertion->kind != OAK_SUBRANGE) || (assertion->op1.kind != O1K_LCLVAR))
        {
            continue;
        }

        if (assertion->op1.vn != vn)
        {
            continue;
        }

        if (varTypeIsUnsigned(fromType) && (assertion->op2.range.min < 0))
        {
            continue;
        }

        if (varTypeIsSmallInt(toType))
        {
            if ((assertion->op2.range.min < GetLowerBoundForIntegralType(toType)) ||
                (assertion->op2.range.max > GetUpperBoundForIntegralType(toType)))
            {
                continue;
            }
        }
        else if (toType == TYP_UINT)
        {
            if (assertion->op2.range.min < GetLowerBoundForIntegralType(toType))
            {
                continue;
            }
        }
        else if (toType != TYP_INT)
        {
            continue;
        }

        return index;
    }

    return NO_ASSERTION_INDEX;
}

AssertionIndex Compiler::apAssertionIsSubtype(ASSERT_VALARG_TP assertions, ValueNum vn, GenTree* methodTable)
{
    for (BitVecOps::Enumerator en(apTraits, assertions); en.MoveNext();)
    {
        AssertionIndex index     = GetAssertionIndex(en.Current());
        AssertionDsc*  assertion = apGetAssertion(index);

        if ((assertion->kind != OAK_EQUAL) ||
            ((assertion->op1.kind != O1K_SUBTYPE) && (assertion->op1.kind != O1K_EXACT_TYPE)))
        {
            continue;
        }

        if (assertion->op1.vn != vn)
        {
            continue;
        }

        if (assertion->op2.kind == O2K_IND_CNS_INT)
        {
            if (!methodTable->OperIs(GT_IND))
            {
                continue;
            }

            // TODO-MIKE-Review: This is dubious, it modifies methodTable so on
            // subsequent assertions it no longer does what it's supposed to do.
            methodTable = methodTable->AsIndir()->GetAddr();
        }
        else if (assertion->op2.kind != O2K_CONST_INT)
        {
            continue;
        }

        ssize_t      iconVal   = 0;
        GenTreeFlags iconFlags = GTF_EMPTY;

        if (apIsConstInt(methodTable, &iconVal, &iconFlags) && (assertion->op2.intCon.value == iconVal))
        {
            return index;
        }
    }

    return NO_ASSERTION_INDEX;
}

GenTree* Compiler::apPropagateLclVarConst(AssertionDsc*  assertion,
                                          GenTreeLclVar* lclVar,
                                          Statement* stmt DEBUGARG(AssertionIndex index))
{
    LclVarDsc* lcl = lvaGetDesc(lclVar);

    assert(!lcl->IsAddressExposed());
    assert(lclVar->GetType() == lcl->GetType());

    if (lcl->lvIsCSE)
    {
        return nullptr;
    }

    const auto& val     = assertion->op2;
    GenTree*    conNode = nullptr;

    switch (val.kind)
    {
        case O2K_CONST_DOUBLE:
            // There could be a positive zero and a negative zero, so don't propagate zeroes.
            // TODO-MIKE-Review: So what?
            if (val.dblCon.value == 0.0)
            {
                return nullptr;
            }

            conNode = lclVar->ChangeToDblCon(val.dblCon.value);
            break;

#ifndef TARGET_64BIT
        case O2K_CONST_LONG:
            if (lclVar->TypeIs(TYP_LONG))
            {
                conNode = lclVar->ChangeToLngCon(val.lngCon.value);
            }
            else
            {
                conNode = lclVar->ChangeToIntCon(TYP_INT, static_cast<int32_t>(val.lngCon.value));
            }
            break;
#endif

        case O2K_CONST_INT:
            if (lclVar->TypeIs(TYP_STRUCT))
            {
                assert(val.intCon.value == 0);

                conNode = lclVar->ChangeToIntCon(TYP_INT, 0);
                break;
            }

#ifdef FEATURE_SIMD
            if (varTypeIsSIMD(lclVar->GetType()))
            {
                assert(val.intCon.value == 0);

                conNode = gtNewZeroSimdHWIntrinsicNode(lcl->GetLayout());
                break;
            }
#endif

            if ((val.intCon.flags & GTF_ICON_HDL_MASK) != 0)
            {
                if (opts.compReloc)
                {
                    return nullptr;
                }

                conNode = lclVar->ChangeToIntCon(TYP_I_IMPL, val.intCon.value);
                conNode->AsIntCon()->SetHandleKind(val.intCon.flags & GTF_ICON_HDL_MASK);
            }
            else
            {
                conNode = lclVar->ChangeToIntCon(varActualType(lclVar->GetType()), val.intCon.value);
            }

            if (varTypeIsIntegral(conNode->GetType()))
            {
#ifdef TARGET_64BIT
                var_types newType = ((val.intCon.flags & GTF_ASSERTION_PROP_LONG) != 0) ? TYP_LONG : TYP_INT;
#else
                var_types newType = TYP_INT;
#endif

                if (conNode->GetType() != newType)
                {
#ifdef TARGET_64BIT
                    noway_assert(!conNode->TypeIs(TYP_REF));
#else
                    noway_assert(!conNode->TypeIs(TYP_REF, TYP_LONG));
#endif
                    conNode->SetType(newType);
                }
            }
            break;

        default:
            return nullptr;
    }

    assert(conNode->OperIsConst());
    assert(vnStore->IsVNConstant(val.vn));

    conNode->gtVNPair.SetBoth(val.vn);

#ifdef DEBUG
    if (verbose)
    {
        printf("\nAssertion prop in " FMT_BB ":\n", compCurBB->bbNum);
        apDumpAssertion(assertion);
        gtDispTree(conNode, nullptr, nullptr, true);
    }
#endif

    return apUpdateTree(conNode, lclVar, stmt);
}

GenTree* Compiler::apPropagateLclVar(ASSERT_VALARG_TP assertions, GenTreeLclVar* lclVar, Statement* stmt)
{
    assert(lclVar->OperIs(GT_LCL_VAR));

    if ((lclVar->gtFlags & (GTF_VAR_DEF | GTF_DONT_CSE)) != 0)
    {
        return nullptr;
    }

    for (BitVecOps::Enumerator en(apTraits, assertions); en.MoveNext();)
    {
        AssertionIndex index     = GetAssertionIndex(en.Current());
        AssertionDsc*  assertion = apGetAssertion(index);

        if ((assertion->kind != OAK_EQUAL) || (assertion->op1.kind != O1K_LCLVAR))
        {
            continue;
        }

        if (assertion->op2.kind == O2K_LCLVAR_COPY)
        {
            // Cannot do copy prop during global assertion prop because of no knowledge
            // of kill sets. We will still make a == b copy assertions during the global
            // phase to allow for any implied assertions that can be retrieved. Because
            // implied assertions look for matching SSA numbers (i.e., if a0 == b1 and
            // b1 == c0 then a0 == c0) they don't need kill sets.

            continue;
        }

        if (assertion->op1.lcl.lclNum == lclVar->GetLclNum())
        {
            LclVarDsc* lcl = lvaGetDesc(lclVar);

            if (lclVar->GetType() == lcl->GetType())
            {
                if (assertion->op1.vn == vnStore->VNNormalValue(lclVar->GetConservativeVN()))
                {
                    return apPropagateLclVarConst(assertion, lclVar, stmt DEBUGARG(index));
                }
            }
        }
    }

    return nullptr;
}

AssertionIndex Compiler::apAssertionIsEquality(ASSERT_VALARG_TP assertions, GenTree* op1, GenTree* op2)
{
    ValueNum vn1 = vnStore->VNNormalValue(op1->GetConservativeVN());
    ValueNum vn2 = vnStore->VNNormalValue(op2->GetConservativeVN());

    for (BitVecOps::Enumerator en(apTraits, assertions); en.MoveNext();)
    {
        AssertionIndex index     = GetAssertionIndex(en.Current());
        AssertionDsc*  assertion = apGetAssertion(index);

        if ((assertion->kind != OAK_EQUAL) && (assertion->kind != OAK_NOT_EQUAL))
        {
            continue;
        }

        if (assertion->op2.vn != vn2)
        {
            continue;
        }

        if (assertion->op1.vn == vn1)
        {
            return index;
        }

        // Look for matching exact type assertions based on vtable accesses.
        if ((assertion->kind == OAK_EQUAL) && (assertion->op1.kind == O1K_EXACT_TYPE) && op1->OperIs(GT_IND))
        {
            GenTree* addr = op1->AsIndir()->GetAddr();

            if (addr->OperIs(GT_LCL_VAR) && addr->TypeIs(TYP_REF) &&
                (assertion->op1.vn == vnStore->VNNormalValue(addr->GetConservativeVN())))
            {
                return index;
            }
        }
    }

    return NO_ASSERTION_INDEX;
}

AssertionIndex Compiler::apAssertionIsZeroEquality(ASSERT_VALARG_TP assertions, GenTree* op1)
{
    ValueNum vn1 = vnStore->VNNormalValue(op1->GetConservativeVN());
    ValueNum vn2 = vnStore->VNZeroForType(op1->GetType());

    for (BitVecOps::Enumerator en(apTraits, assertions); en.MoveNext();)
    {
        AssertionIndex index     = GetAssertionIndex(en.Current());
        AssertionDsc*  assertion = apGetAssertion(index);

        if ((assertion->kind != OAK_EQUAL) && (assertion->kind != OAK_NOT_EQUAL))
        {
            continue;
        }

        if ((assertion->op1.vn == vn1) && (assertion->op2.vn == vn2))
        {
            return index;
        }
    }

    return NO_ASSERTION_INDEX;
}

GenTree* Compiler::apPropagateRelop(ASSERT_VALARG_TP assertions, GenTreeOp* relop, Statement* stmt)
{
    assert(relop->OperIsCompare());

    GenTree* op1 = relop->GetOp(0);
    GenTree* op2 = relop->GetOp(1);

    AssertionIndex index = apAssertionIsZeroEquality(assertions, relop);

    if (index != NO_ASSERTION_INDEX)
    {
        AssertionDsc* assertion = apGetAssertion(index);

        JITDUMP("Assertion #%02u: relop [%06u] %s 0\n", index, relop->GetID(),
                (assertion->kind == OAK_EQUAL) ? "==" : "!=");

        if ((relop->gtFlags & GTF_SIDE_EFFECT) != 0)
        {
            JITDUMP("Relop has side effects\n");
            return nullptr;
        }

        relop->ChangeToIntCon(assertion->kind != OAK_EQUAL);

        return apUpdateTree(relop, relop, stmt);
    }

    if (!relop->OperIs(GT_EQ, GT_NE))
    {
        return nullptr;
    }

    if ((relop->gtFlags & GTF_SIDE_EFFECT) != 0)
    {
        return nullptr;
    }

    if (!op1->OperIs(GT_LCL_VAR, GT_IND))
    {
        return nullptr;
    }

    index = apAssertionIsEquality(assertions, op1, op2);

    if (index == NO_ASSERTION_INDEX)
    {
        return nullptr;
    }

    AssertionDsc* assertion    = apGetAssertion(index);
    ValueNum      op2VN        = vnStore->VNNormalValue(op2->GetConservativeVN());
    bool          allowReverse = true;

    if (vnStore->IsVNConstant(op2VN))
    {
        // TODO-MIKE-Review: This stuff's weird. We have op1 ==/!= op2 and op2 is a constant.
        // We change op1 to be the same constant as op2 and then set the relop VN to 0/1.
        // Why don't we just change the relop to a constant like in the zero case above?

        JITDUMP("Assertion #%02u: [%06u] %s ", index, op1->GetID(), assertion->kind == OAK_EQUAL ? "==" : "!=");

        if (varActualTypeIsInt(op1->GetType()))
        {
            int value = vnStore->ConstantValue<int>(op2VN);
            JITDUMP("%d\n", value);
            op1->ChangeToIntCon(TYP_INT, value);

#ifndef TARGET_64BIT
            if (vnStore->IsVNHandle(op2VN))
            {
                op1->gtFlags |= (vnStore->GetHandleFlags(op2VN) & GTF_ICON_HDL_MASK);
            }
#endif
        }
        else if (op1->TypeIs(TYP_LONG))
        {
            int64_t value = vnStore->ConstantValue<int64_t>(op2VN);
            JITDUMP("%lld\n", value);
#ifdef TARGET_64BIT
            op1->ChangeToIntCon(value);

            if (vnStore->IsVNHandle(op2VN))
            {
                op1->gtFlags |= (vnStore->GetHandleFlags(op2VN) & GTF_ICON_HDL_MASK);
            }
#else
            op1->ChangeToLngCon(value);
#endif
        }
        else if (op1->TypeIs(TYP_DOUBLE))
        {
            double value = vnStore->ConstantValue<double>(op2VN);
            JITDUMP("%f\n", value);
            op1->ChangeToDblCon(value);

            // Nothing can be equal to NaN. So if IL had "op1 == NaN", then we already made op1 NaN,
            // which will yield a false correctly. Instead if IL had "op1 != NaN", then we already
            // made op1 NaN which will yield a true correctly. Note that this is irrespective of the
            // assertion we have made.
            allowReverse = !_isnan(value);
        }
        else if (op1->TypeIs(TYP_FLOAT))
        {
            float value = vnStore->ConstantValue<float>(op2VN);
            JITDUMP("%f\n", value);
            op1->ChangeToDblCon(value);
            allowReverse = !_isnan(value);
        }
        else if (op1->TypeIs(TYP_REF))
        {
            noway_assert(vnStore->ConstantValue<size_t>(op2VN) == 0);
            JITDUMP("null\n");
            op1->ChangeToIntCon(0);
        }
        else if (op1->TypeIs(TYP_BYREF))
        {
            target_ssize_t value = static_cast<target_ssize_t>(vnStore->ConstantValue<size_t>(op2VN));
#ifdef TARGET_64BIT
            JITDUMP("%llx\n", value);
#else
            JITDUMP("%x\n", value);
#endif
            op1->ChangeToIntCon(value);
        }
        else
        {
            unreached();
        }

        op1->gtVNPair.SetBoth(op2VN);

        bool foldResult = assertion->kind == OAK_EQUAL;

        if (!relop->OperIs(GT_NE))
        {
            foldResult = !foldResult;
        }

        if (foldResult)
        {
            relop->gtVNPair.SetBoth(vnStore->VNOneForType(TYP_INT));
        }
        else
        {
            relop->gtVNPair.SetBoth(vnStore->VNZeroForType(TYP_INT));
        }
    }
    else if (op2->OperIs(GT_LCL_VAR))
    {
        JITDUMP("Assertion #%02u: V%02u.%02u %s V%02u.%02u\n", index, op1->AsLclVar()->GetLclNum(),
                op1->AsLclVar()->GetSsaNum(), (assertion->kind == OAK_EQUAL) ? "==" : "!=",
                op2->AsLclVar()->GetLclNum(), op2->AsLclVar()->GetSsaNum());

        // If floating point, don't just substitute op1 with op2, this won't work if
        // op2 is NaN. Just turn it into a "true" or "false" yielding expression.
        if (varTypeIsFloating(op1->GetType()))
        {
            // Note we can't trust the OAK_EQUAL as the value could end up being a NaN
            // violating the assertion. However, we create OAK_EQUAL assertions for floating
            // point only on JTrue nodes, so if the condition held earlier, it will hold
            // now. We don't create OAK_EQUAL assertion on floating point from GT_ASG
            // because we depend on value num which would constant prop the NaN.
            op1->ChangeToDblCon(0);
            op2->ChangeToDblCon(0);
        }
        else
        {
            noway_assert(varTypeIsIntegralOrI(op1->GetType()));

            // TODO-MIKE-Review: What about the op1 = IND case? Presumably that can't happen
            // because op2 is expected to be CNS_INT or IND (exact type assertion).
            op1->AsLclVar()->SetLclNum(op2->AsLclVar()->GetLclNum());
            op1->AsLclVar()->SetSsaNum(op2->AsLclVar()->GetSsaNum());
        }
    }
    else
    {
        return nullptr;
    }

    // Finally reverse the condition, if we have a not equal assertion.
    if (allowReverse && (assertion->kind == OAK_NOT_EQUAL))
    {
        gtReverseCond(relop);
    }

    // TODO-MIKE-Review: Why are we morphing here? apUpdateTree will do that anyway.
    GenTree* newTree = fgMorphTree(relop);
    DBEXEC(verbose, gtDispTree(newTree, nullptr, nullptr, true);)
    return apUpdateTree(newTree, relop, stmt);
}

GenTree* Compiler::apPropagateCast(ASSERT_VALARG_TP assertions, GenTreeCast* cast, Statement* stmt)
{
    GenTree*  op1      = cast->GetOp(0);
    var_types fromType = op1->GetType();
    var_types toType   = cast->GetCastType();

    if (varTypeIsFloating(toType) || varTypeIsFloating(fromType))
    {
        return nullptr;
    }

    if (cast->IsUnsigned())
    {
        fromType = varTypeToUnsigned(fromType);
    }

    GenTree* lclVar = op1->SkipComma();

    if (!lclVar->OperIs(GT_LCL_VAR))
    {
        return nullptr;
    }

    ValueNum       vn    = vnStore->VNNormalValue(lclVar->GetConservativeVN());
    AssertionIndex index = apAssertionIsSubrange(assertions, vn, fromType, toType);

    if (index == NO_ASSERTION_INDEX)
    {
        return nullptr;
    }

    LclVarDsc* lcl = lvaGetDesc(lclVar->AsLclVar());
    assert(!lcl->IsAddressExposed());

    if (!lcl->lvNormalizeOnLoad() && !varTypeIsLong(lcl->GetType()))
    {
        return apUpdateTree(op1, cast, stmt);
    }

    if (varTypeSize(toType) > varTypeSize(lcl->GetType()))
    {
        if (!cast->gtOverflow())
        {
            return nullptr;
        }

#ifdef DEBUG
        if (verbose)
        {
            printf("\nSubrange prop for index #%02u in " FMT_BB ":\n", index, compCurBB->bbNum);
            gtDispTree(cast, nullptr, nullptr, true);
        }
#endif
        cast->gtFlags &= ~GTF_OVERFLOW; // This cast cannot overflow

        return apUpdateTree(cast, cast, stmt);
    }

    if (toType == TYP_UINT)
    {
        toType = TYP_INT;
    }

    GenTree* tmp = op1;

    while (tmp->OperIs(GT_COMMA))
    {
        tmp->SetType(toType);
        tmp = tmp->AsOp()->GetOp(1);
    }

    tmp->SetType(toType);

#ifdef DEBUG
    if (verbose)
    {
        printf("\nSubrange prop for index #%02u in " FMT_BB ":\n", index, compCurBB->bbNum);
        gtDispTree(cast, nullptr, nullptr, true);
    }
#endif

    return apUpdateTree(op1, cast, stmt);
}

GenTree* Compiler::apPropagateComma(GenTreeOp* comma, Statement* stmt)
{
    // Remove the bounds check as part of the GT_COMMA node since we need user pointer to remove nodes.
    // When processing visits the bounds check, it sets the throw kind to None if the check is redundant.

    if (!comma->GetOp(0)->OperIs(GT_ARR_BOUNDS_CHECK) || ((comma->GetOp(0)->gtFlags & GTF_ARR_BOUND_INBND) == 0))
    {
        return nullptr;
    }

    optRemoveCommaBasedRangeCheck(comma, stmt);
    return apUpdateTree(comma, comma, stmt);
}

GenTree* Compiler::apPropagateIndir(ASSERT_VALARG_TP assertions, GenTreeIndir* indir, Statement* stmt)
{
    if ((indir->gtFlags & GTF_EXCEPT) == 0)
    {
        return nullptr;
    }

    GenTree* addr = indir->GetAddr();

    if (addr->OperIs(GT_ADD) && addr->AsOp()->GetOp(1)->IsIntCon())
    {
        addr = addr->AsOp()->GetOp(0);
    }

    if (!addr->OperIs(GT_LCL_VAR))
    {
        return nullptr;
    }

    INDEBUG(AssertionIndex index = NO_ASSERTION_INDEX);

    if (!apAssertionIsNotNull(assertions, addr->GetConservativeVN() DEBUGARG(&index)))
    {
        return nullptr;
    }

#ifdef DEBUG
    if (verbose)
    {
        if (index == NO_ASSERTION_INDEX)
        {
            printf("\nVN based non-null prop in " FMT_BB ":\n", compCurBB->bbNum);
        }
        else
        {
            printf("\nNon-null prop for index #%02u in " FMT_BB ":\n", index, compCurBB->bbNum);
        }

        gtDispTree(indir, nullptr, nullptr, true);
    }
#endif

    indir->gtFlags &= ~GTF_EXCEPT;
    indir->gtFlags |= GTF_IND_NONFAULTING;
    // Set this flag to prevent reordering
    indir->gtFlags |= GTF_ORDER_SIDEEFF;

    return apUpdateTree(indir, indir, stmt);
}

bool Compiler::apAssertionIsNotNull(ASSERT_VALARG_TP assertions, ValueNum vn DEBUGARG(AssertionIndex* assertionIndex))
{
    if (vnStore->IsKnownNonNull(vn))
    {
        return true;
    }

    // TODO-MIKE-Review: Shouldn't this be done before the IsKnownNonNull above?
    vn = vnStore->VNNormalValue(vn);

    ValueNum baseVN = vn;

    for (VNFuncApp funcApp; vnStore->GetVNFunc(baseVN, &funcApp) && (funcApp.m_func == static_cast<VNFunc>(GT_ADD));)
    {
        if (vnStore->IsVNConstant(funcApp[1]) && varTypeIsIntegral(vnStore->TypeOfVN(funcApp[1])))
        {
            baseVN = funcApp[0];
        }
        else if (vnStore->IsVNConstant(funcApp[0]) && varTypeIsIntegral(vnStore->TypeOfVN(funcApp[0])))
        {
            baseVN = funcApp[1];
        }
        else
        {
            break;
        }
    }

    for (BitVecOps::Enumerator en(apTraits, assertions); en.MoveNext();)
    {
        AssertionIndex index     = GetAssertionIndex(en.Current());
        AssertionDsc*  assertion = apGetAssertion(index);

        if (assertion->kind != OAK_NOT_EQUAL)
        {
            continue;
        }

        if (assertion->op2.vn != ValueNumStore::VNForNull())
        {
            continue;
        }

        if ((assertion->op1.vn != vn) && (assertion->op1.vn != baseVN))
        {
            continue;
        }

        INDEBUG(*assertionIndex = index);
        return true;
    }

    return NO_ASSERTION_INDEX;
}

GenTree* Compiler::apPropagateCallNotNull(ASSERT_VALARG_TP assertions, GenTreeCall* call)
{
    if ((call->gtFlags & GTF_CALL_NULLCHECK) == 0)
    {
        return nullptr;
    }

    GenTree* thisArg = call->GetThisArg();
    noway_assert(thisArg != nullptr);

    if (!thisArg->OperIs(GT_LCL_VAR))
    {
        return nullptr;
    }

    INDEBUG(AssertionIndex index = NO_ASSERTION_INDEX);

    if (!apAssertionIsNotNull(assertions, thisArg->GetConservativeVN() DEBUGARG(&index)))
    {
        return nullptr;
    }

#ifdef DEBUG
    if (verbose)
    {
        if (index == NO_ASSERTION_INDEX)
        {
            printf("\nVN based non-null prop in " FMT_BB ":\n", compCurBB->bbNum);
        }
        else
        {
            printf("\nNon-null prop for index #%02u in " FMT_BB ":\n", index, compCurBB->bbNum);
        }

        gtDispTree(call, nullptr, nullptr, true);
    }
#endif

    call->gtFlags &= ~GTF_CALL_NULLCHECK;
    call->gtFlags &= ~GTF_EXCEPT;
    noway_assert(call->gtFlags & GTF_SIDE_EFFECT);
    return call;
}

GenTree* Compiler::apPropagateCall(ASSERT_VALARG_TP assertions, GenTreeCall* call, Statement* stmt)
{
    if (apPropagateCallNotNull(assertions, call))
    {
        return apUpdateTree(call, call, stmt);
    }

    if (!call->IsHelperCall())
    {
        return nullptr;
    }

    CorInfoHelpFunc helper = eeGetHelperNum(call->GetMethodHandle());

    if ((helper != CORINFO_HELP_ISINSTANCEOFINTERFACE) && (helper != CORINFO_HELP_ISINSTANCEOFARRAY) &&
        (helper != CORINFO_HELP_ISINSTANCEOFCLASS) && (helper != CORINFO_HELP_ISINSTANCEOFANY) &&
        (helper != CORINFO_HELP_CHKCASTINTERFACE) && (helper != CORINFO_HELP_CHKCASTARRAY) &&
        (helper != CORINFO_HELP_CHKCASTCLASS) && (helper != CORINFO_HELP_CHKCASTANY) &&
        (helper != CORINFO_HELP_CHKCASTCLASS_SPECIAL))
    {
        return nullptr;
    }

    GenTree* objectArg = call->GetArgNodeByArgNum(1);

    if (!objectArg->OperIs(GT_LCL_VAR))
    {
        return nullptr;
    }

    ValueNum objectVN = vnStore->VNNormalValue(objectArg->GetConservativeVN());
    unsigned index    = apAssertionIsSubtype(assertions, objectVN, call->GetArgNodeByArgNum(0));

    if (index == NO_ASSERTION_INDEX)
    {
        return nullptr;
    }

#ifdef DEBUG
    if (verbose)
    {
        printf("\nDid VN based subtype prop for index #%02u in " FMT_BB ":\n", index, compCurBB->bbNum);
        gtDispTree(call, nullptr, nullptr, true);
    }
#endif

    GenTree* sideEffects = nullptr;
    gtExtractSideEffList(call, &sideEffects, GTF_SIDE_EFFECT, true);

    if (sideEffects != nullptr)
    {
        objectArg = gtNewCommaNode(sideEffects, objectArg);
        fgSetTreeSeq(objectArg);
    }

    return apUpdateTree(objectArg, call, stmt);
}

GenTree* Compiler::apPropagateBoundsChk(ASSERT_VALARG_TP assertions, GenTreeBoundsChk* boundsChk, Statement* stmt)
{
#ifdef FEATURE_ENABLE_NO_RANGE_CHECKS
    if (JitConfig.JitNoRangeChks())
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("\nFlagging check redundant due to JitNoRangeChks in " FMT_BB ":\n", compCurBB->bbNum);
            gtDispTree(boundsChk, nullptr, nullptr, true);
        }
#endif

        boundsChk->gtFlags |= GTF_ARR_BOUND_INBND;
        return nullptr;
    }
#endif // FEATURE_ENABLE_NO_RANGE_CHECKS

    ValueNum indexVN  = vnStore->VNNormalValue(boundsChk->GetIndex()->GetConservativeVN());
    ValueNum lengthVN = vnStore->VNNormalValue(boundsChk->GetLength()->GetConservativeVN());

    for (BitVecOps::Enumerator en(apTraits, assertions); en.MoveNext();)
    {
        AssertionIndex index     = GetAssertionIndex(en.Current());
        AssertionDsc*  assertion = apGetAssertion(index);

        if (assertion->kind != OAK_NO_THROW)
        {
            continue;
        }

        // Do we have a previous range check involving the same 'vnLen' upper bound?
        if (assertion->op1.bnd.vnLen != lengthVN)
        {
            continue;
        }

        bool isRedundant = false;
        INDEBUG(const char* message = "");

        if (assertion->op1.bnd.vnIdx == indexVN)
        {
            isRedundant = true;
            INDEBUG(message = "a[i] followed by a[i]");
        }
        else if (indexVN == vnStore->VNZeroForType(boundsChk->GetIndex()->GetType()))
        {
            isRedundant = true;
            INDEBUG(message = "a[*] followed by a[0]");
        }
        else if (vnStore->IsVNConstant(assertion->op1.bnd.vnIdx) && vnStore->IsVNConstant(indexVN))
        {
            var_types type1 = vnStore->TypeOfVN(assertion->op1.bnd.vnIdx);
            var_types type2 = vnStore->TypeOfVN(indexVN);

            if ((type1 == type2) && (type1 == TYP_INT))
            {
                int index1 = vnStore->ConstantValue<int>(assertion->op1.bnd.vnIdx);
                int index2 = vnStore->ConstantValue<int>(indexVN);

                assert(index1 != index2);

                if ((index2 >= 0) && (index1 >= index2))
                {
                    isRedundant = true;
                    INDEBUG(message = "a[K1] followed by a[K2], with K2 >= 0 and K1 >= K2");
                }
            }
        }

        // Extend this to remove additional redundant bounds checks:
        // i.e.  a[i+1] followed by a[i]  by using the VN(i+1) >= VN(i)
        //       a[i]   followed by a[j]  when j is known to be >= i
        //       a[i]   followed by a[5]  when i is known to be >= 5

        if (!isRedundant)
        {
            continue;
        }

#ifdef DEBUG
        if (verbose)
        {
            printf("\nVN based redundant (%s) bounds check assertion prop for index #%02u in " FMT_BB ":\n", message,
                   index, compCurBB->bbNum);
            gtDispTree(boundsChk, nullptr, nullptr, true);
        }
#endif

        if (boundsChk == stmt->GetRootNode())
        {
            return apUpdateTree(optRemoveStandaloneRangeCheck(boundsChk, stmt), boundsChk, stmt);
        }
        else
        {
            boundsChk->gtFlags |= GTF_ARR_BOUND_INBND;
            return nullptr;
        }
    }

    return nullptr;
}

GenTree* Compiler::apUpdateTree(GenTree* newTree, GenTree* tree, Statement* stmt)
{
    assert(newTree != nullptr);
    assert(tree != nullptr);
    assert(stmt != nullptr);

    if (newTree != tree)
    {
        if (stmt->GetRootNode() == tree)
        {
            stmt->SetRootNode(newTree);
        }
        else
        {
            FindLinkData use = gtFindLink(stmt, tree);
            noway_assert((use.useEdge != nullptr) && (use.user != nullptr));
            use.user->ReplaceOperand(use.useEdge, newTree);
        }

        // We need to ensure that the gtNext field is set as it is used to traverse
        // the statement. Later we'll re-morph and sequence the statement, so that
        // gtPrev gets updated as well.
        newTree->gtNext = tree->gtNext;

        DEBUG_DESTROY_NODE(tree);
    }

    apStmtMorphPending = true;

    return newTree;
}

GenTree* Compiler::apPropagateNode(ASSERT_VALARG_TP assertions, GenTree* node, Statement* stmt, BasicBlock* block)
{
    switch (node->GetOper())
    {
        case GT_LCL_VAR:
            return apPropagateLclVar(assertions, node->AsLclVar(), stmt);
        case GT_OBJ:
        case GT_BLK:
        case GT_IND:
        case GT_NULLCHECK:
            return apPropagateIndir(assertions, node->AsIndir(), stmt);
        case GT_ARR_BOUNDS_CHECK:
            return apPropagateBoundsChk(assertions, node->AsBoundsChk(), stmt);
        case GT_COMMA:
            return apPropagateComma(node->AsOp(), stmt);
        case GT_CAST:
            return apPropagateCast(assertions, node->AsCast(), stmt);
        case GT_CALL:
            return apPropagateCall(assertions, node->AsCall(), stmt);
        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GT:
        case GT_GE:
            return apPropagateRelop(assertions, node->AsOp(), stmt);
        case GT_JTRUE:
            return apPropagateJTrue(block, node->AsUnOp());
        default:
            return nullptr;
    }
}

void Compiler::apAddImpliedAssertions(AssertionIndex index, ASSERT_TP& assertions)
{
    AssertionDsc* assertion = apGetAssertion(index);

    if (BitVecOps::IsEmpty(apTraits, assertions))
    {
        if ((assertion->kind == OAK_EQUAL) && (assertion->op1.kind == O1K_LCLVAR) &&
            (assertion->op2.kind == O2K_CONST_INT))
        {
            apAddConstImpliedAssertions(assertion, assertions);
        }

        return;
    }

    const ASSERT_TP vn1Assertions = apGetVNAssertions(assertion->op1.vn);

    if (vn1Assertions == BitVecOps::UninitVal())
    {
        return;
    }

    const ASSERT_TP vn2Assertions =
        assertion->op2.kind != O2K_LCLVAR_COPY ? BitVecOps::UninitVal() : apGetVNAssertions(assertion->op2.vn);

    for (BitVecOps::Enumerator en(apTraits, assertions); en.MoveNext();)
    {
        AssertionIndex impliedIndex = GetAssertionIndex(en.Current());

        if (impliedIndex == index)
        {
            continue;
        }

        if (!BitVecOps::IsMember(apTraits, vn1Assertions, en.Current()) &&
            ((vn2Assertions == BitVecOps::UninitVal()) || !BitVecOps::IsMember(apTraits, vn1Assertions, en.Current())))
        {
            continue;
        }

        AssertionDsc* impliedAssertion = apGetAssertion(impliedIndex);

        if (assertion->IsCopyAssertion())
        {
            apAddCopyImpliedAssertions(assertion, impliedAssertion, assertions);
        }
        else if (impliedAssertion->IsCopyAssertion())
        {
            apAddCopyImpliedAssertions(impliedAssertion, assertion, assertions);
        }
    }
}

void Compiler::apAddTypeImpliedNotNullAssertions(ASSERT_TP& assertions)
{
    for (BitVecOps::Enumerator en(apTraits, assertions); en.MoveNext();)
    {
        AssertionIndex typeIndex     = GetAssertionIndex(en.Current());
        AssertionDsc*  typeAssertion = apGetAssertion(typeIndex);

        if ((typeAssertion->kind != OAK_EQUAL) ||
            ((typeAssertion->op1.kind != O1K_SUBTYPE) && (typeAssertion->op1.kind != O1K_EXACT_TYPE)))
        {
            continue;
        }

        for (AssertionIndex notNullIndex = 1; notNullIndex <= apAssertionCount; notNullIndex++)
        {
            if (notNullIndex == typeIndex)
            {
                continue;
            }

            AssertionDsc* notNullAssertion = apGetAssertion(notNullIndex);

            if ((notNullAssertion->kind != OAK_NOT_EQUAL) ||
                ((notNullAssertion->op1.kind != O1K_LCLVAR) && (notNullAssertion->op1.kind != O1K_VALUE_NUMBER)) ||
                (notNullAssertion->op2.kind != O2K_CONST_INT) || (notNullAssertion->op1.vn != typeAssertion->op1.vn))
            {
                continue;
            }

            if (BitVecOps::TryAddElemD(apTraits, assertions, notNullIndex - 1))
            {
                JITDUMP("\napAddTypeImpliedNotNullAssertions: %s Assertion #%02d, implies assertion #%02d",
                        (typeAssertion->op1.kind == O1K_SUBTYPE) ? "Subtype" : "Exact-type", typeIndex, notNullIndex);
            }

            // There is at most one not null assertion that is implied by a type assertion.
            break;
        }
    }
}

void Compiler::apAddConstImpliedAssertions(AssertionDsc* constAssertion, ASSERT_TP& result)
{
    assert(constAssertion->kind == OAK_EQUAL);
    assert((constAssertion->op1.kind == O1K_LCLVAR) && (constAssertion->op2.kind == O2K_CONST_INT));

    const ASSERT_TP vnAssertions = apGetVNAssertions(constAssertion->op1.vn);

    if (vnAssertions == BitVecOps::UninitVal())
    {
        return;
    }

    ssize_t value = constAssertion->op2.intCon.value;

    for (BitVecOps::Enumerator en(apTraits, vnAssertions); en.MoveNext();)
    {
        AssertionIndex impliedIndex     = GetAssertionIndex(en.Current());
        AssertionDsc*  impliedAssertion = apGetAssertion(impliedIndex);

        if (impliedAssertion == constAssertion)
        {
            continue;
        }

        if (impliedAssertion->op1.vn != constAssertion->op1.vn)
        {
            continue;
        }

        if (impliedAssertion->op2.kind == O2K_SUBRANGE)
        {
            if ((value < impliedAssertion->op2.range.min) || (impliedAssertion->op2.range.max < value))
            {
                continue;
            }
        }
        else if (impliedAssertion->op2.kind == O2K_CONST_INT)
        {
            // TODO-MIKE-Review: This is dubious, it checks for an "implied" assertion that's actually identical.
            if (!((impliedAssertion->kind == OAK_EQUAL) && (impliedAssertion->op2.intCon.value == value)) &&
                !((impliedAssertion->kind == OAK_NOT_EQUAL) && (impliedAssertion->op2.intCon.value != value)))
            {
                continue;
            }
        }
        else
        {
            continue;
        }

        if (BitVecOps::TryAddElemD(apTraits, result, en.Current()))
        {
            INDEBUG(AssertionDsc* firstAssertion = apGetAssertion(1));
            JITDUMP("apAddConstImpliedAssertions: const assertion #%02d implies assertion #%02d\n",
                    GetAssertionIndex(static_cast<unsigned>(constAssertion - firstAssertion)),
                    GetAssertionIndex(static_cast<unsigned>(impliedAssertion - firstAssertion)));
        }
    }
}

void Compiler::apAddCopyImpliedAssertions(AssertionDsc* copyAssertion, AssertionDsc* assertion, ASSERT_TP& result)
{
    assert(copyAssertion->kind == OAK_EQUAL);
    assert((copyAssertion->op1.kind == O1K_LCLVAR) && (copyAssertion->op2.kind == O2K_LCLVAR_COPY));
    assert(assertion->kind != OAK_NO_THROW);

    // TODO-MIKE-Cleanup: It looks like we can end up with `assertion` being a
    // "bound" assertion, for which lcl is not set and may contain garbage.

    AssertionDsc::LclVar copyLcl{BAD_VAR_NUM, SsaConfig::RESERVED_SSA_NUM};

    if (assertion->op1.lcl.lclNum == copyAssertion->op1.lcl.lclNum)
    {
        copyLcl = copyAssertion->op2.lcl;
    }
    else if (assertion->op1.lcl.lclNum == copyAssertion->op2.lcl.lclNum)
    {
        copyLcl = copyAssertion->op1.lcl;
    }
    else if (assertion->op2.kind == O2K_LCLVAR_COPY)
    {
        if (assertion->op2.lcl.lclNum == copyAssertion->op1.lcl.lclNum)
        {
            copyLcl = copyAssertion->op2.lcl;
        }
        else if (assertion->op2.lcl.lclNum == copyAssertion->op2.lcl.lclNum)
        {
            copyLcl = copyAssertion->op1.lcl;
        }
    }

    if ((copyLcl.lclNum == BAD_VAR_NUM) || (copyLcl.ssaNum == SsaConfig::RESERVED_SSA_NUM))
    {
        return;
    }

    AssertionDsc::LclVar lcl{BAD_VAR_NUM, SsaConfig::RESERVED_SSA_NUM};

    if ((assertion->op1.kind == O1K_LCLVAR) && (assertion->op2.kind == O2K_LCLVAR_COPY))
    {
        if ((assertion->op1.lcl.lclNum == copyAssertion->op1.lcl.lclNum) ||
            (assertion->op1.lcl.lclNum == copyAssertion->op2.lcl.lclNum))
        {
            lcl = assertion->op2.lcl;
        }
        else if ((assertion->op2.lcl.lclNum == copyAssertion->op1.lcl.lclNum) ||
                 (assertion->op2.lcl.lclNum == copyAssertion->op2.lcl.lclNum))
        {
            lcl = assertion->op1.lcl;
        }
    }

    if ((lcl.lclNum == BAD_VAR_NUM) || (lcl.ssaNum == SsaConfig::RESERVED_SSA_NUM))
    {
        return;
    }

    bool isConstAssertion =
        (assertion->kind == OAK_EQUAL) && (assertion->op1.kind == O1K_LCLVAR) && (assertion->op2.kind == O2K_CONST_INT);

    for (AssertionIndex impliedIndex = 1; impliedIndex <= apAssertionCount; impliedIndex++)
    {
        AssertionDsc* impliedAssertion = apGetAssertion(impliedIndex);

        if ((impliedAssertion == copyAssertion) || (impliedAssertion == assertion))
        {
            continue;
        }

        if (impliedAssertion->kind != assertion->kind)
        {
            continue;
        }

        if ((impliedAssertion->op1.kind != assertion->op1.kind) || (impliedAssertion->op2.kind != assertion->op2.kind))
        {
            continue;
        }

        if (impliedAssertion->op1.lcl != copyLcl)
        {
            if (!((impliedAssertion->op2.kind == O2K_LCLVAR_COPY) && (impliedAssertion->op2.lcl == copyLcl) &&
                  (impliedAssertion->op1.lcl == lcl)))
            {
                continue;
            }
        }
        else
        {
            switch (impliedAssertion->op2.kind)
            {
                case O2K_SUBRANGE:
                    if ((impliedAssertion->op2.range.min > assertion->op2.range.min) ||
                        (impliedAssertion->op2.range.max < assertion->op2.range.max))
                    {
                        continue;
                    }
                    break;

#ifndef TARGET_64BIT
                case O2K_CONST_LONG:
                    if (impliedAssertion->op2.lngCon != assertion->op2.lngCon)
                    {
                        continue;
                    }
                    break;
#endif

                case O2K_CONST_DOUBLE:
                    if (impliedAssertion->op2.dblCon != assertion->op2.dblCon)
                    {
                        continue;
                    }
                    break;

                case O2K_IND_CNS_INT:
                    assert((impliedAssertion->op1.kind == O1K_EXACT_TYPE) ||
                           (impliedAssertion->op1.kind == O1K_SUBTYPE));
                    FALLTHROUGH;
                case O2K_CONST_INT:
                    if (impliedAssertion->op2.intCon.value != assertion->op2.intCon.value)
                    {
                        continue;
                    }
                    break;

                case O2K_LCLVAR_COPY:
                    if (lcl != impliedAssertion->op2.lcl)
                    {
                        continue;
                    }
                    break;

                default:
                    break;
            }
        }

        if (BitVecOps::TryAddElemD(apTraits, result, impliedIndex - 1))
        {
            INDEBUG(AssertionDsc* firstAssertion = apGetAssertion(1));
            JITDUMP("\napAddCopyImpliedAssertions: copy assertion #%02d and assertion #%02d, implies assertion #%02d",
                    GetAssertionIndex(static_cast<unsigned>(copyAssertion - firstAssertion)),
                    GetAssertionIndex(static_cast<unsigned>(assertion - firstAssertion)),
                    GetAssertionIndex(static_cast<unsigned>(impliedAssertion - firstAssertion)));
        }

        // If the depAssertion is a const assertion then any other assertions
        // that it implies could also imply a subrange assertion.
        if (isConstAssertion)
        {
            apAddConstImpliedAssertions(impliedAssertion, result);
        }
    }
}

#include "dataflow.h"

class AssertionPropFlowCallback
{
    ASSERT_TP preMergeOut;
    ASSERT_TP preMergeJumpDestOut;

    ASSERT_TP* mJumpDestOut;
    ASSERT_TP* mJumpDestGen;

    BitVecTraits* apTraits;
    INDEBUG(Compiler* compiler;)

public:
    AssertionPropFlowCallback(Compiler* compiler, ASSERT_TP* jumpDestOut, ASSERT_TP* jumpDestGen)
        : preMergeOut(BitVecOps::UninitVal())
        , preMergeJumpDestOut(BitVecOps::UninitVal())
        , mJumpDestOut(jumpDestOut)
        , mJumpDestGen(jumpDestGen)
        , apTraits(compiler->apTraits)
#ifdef DEBUG
        , compiler(compiler)
#endif
    {
    }

    // At the start of the merge function of the dataflow equations, initialize premerge state (to detect change.)
    void StartMerge(BasicBlock* block)
    {
#ifdef DEBUG
        if (VerboseDataflow())
        {
            printf("StartMerge: " FMT_BB " ", block->bbNum);
            compiler->apDumpAssertionIndices("in -> ", block->bbAssertionIn, "\n");
        }
#endif

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

#ifdef DEBUG
                if (VerboseDataflow())
                {
                    printf("Merge     : Duplicate flow, " FMT_BB " ", block->bbNum);
                    compiler->apDumpAssertionIndices("in -> ", block->bbAssertionIn, "; ");
                    printf("pred " FMT_BB " ", predBlock->bbNum);
                    compiler->apDumpAssertionIndices("out1 -> ", mJumpDestOut[predBlock->bbNum], "; ");
                    compiler->apDumpAssertionIndices("out2 -> ", predBlock->bbAssertionOut, "\n");
                }
#endif
            }
        }
        else
        {
            pAssertionOut = predBlock->bbAssertionOut;
        }

#ifdef DEBUG
        if (VerboseDataflow())
        {
            printf("Merge     : " FMT_BB " ", block->bbNum);
            compiler->apDumpAssertionIndices("in -> ", block->bbAssertionIn, "; ");
            printf("pred " FMT_BB " ", predBlock->bbNum);
            compiler->apDumpAssertionIndices("out -> ", pAssertionOut, "\n");
        }
#endif

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
#ifdef DEBUG
        if (VerboseDataflow())
        {
            printf("Merge     : " FMT_BB " ", block->bbNum);
            compiler->apDumpAssertionIndices("in -> ", block->bbAssertionIn, "; ");
            printf("firstTryBlock " FMT_BB " ", firstTryBlock->bbNum);
            compiler->apDumpAssertionIndices("in -> ", firstTryBlock->bbAssertionIn, "; ");
            printf("lastTryBlock " FMT_BB " ", lastTryBlock->bbNum);
            compiler->apDumpAssertionIndices("out -> ", lastTryBlock->bbAssertionOut, "\n");
        }
#endif

        BitVecOps::IntersectionD(apTraits, block->bbAssertionIn, firstTryBlock->bbAssertionIn);
        BitVecOps::IntersectionD(apTraits, block->bbAssertionIn, lastTryBlock->bbAssertionOut);
    }

    // At the end of the merge store results of the dataflow equations, in a postmerge state.
    bool EndMerge(BasicBlock* block)
    {
#ifdef DEBUG
        if (VerboseDataflow())
        {
            printf("EndMerge  : " FMT_BB " ", block->bbNum);
            compiler->apDumpAssertionIndices("in -> ", block->bbAssertionIn, "\n\n");
        }
#endif

        BitVecOps::DataFlowD(apTraits, block->bbAssertionOut, block->bbAssertionGen, block->bbAssertionIn);
        BitVecOps::DataFlowD(apTraits, mJumpDestOut[block->bbNum], mJumpDestGen[block->bbNum], block->bbAssertionIn);

        bool changed = (!BitVecOps::Equal(apTraits, preMergeOut, block->bbAssertionOut) ||
                        !BitVecOps::Equal(apTraits, preMergeJumpDestOut, mJumpDestOut[block->bbNum]));

#ifdef DEBUG
        if (VerboseDataflow())
        {
            if (changed)
            {
                printf("Changed   : " FMT_BB " ", block->bbNum);
                compiler->apDumpAssertionIndices("before out -> ", preMergeOut, "; ");
                compiler->apDumpAssertionIndices("after out -> ", block->bbAssertionOut, ";\n        ");
                compiler->apDumpAssertionIndices("jumpDest before out -> ", preMergeJumpDestOut, "; ");
                compiler->apDumpAssertionIndices("jumpDest after out -> ", mJumpDestOut[block->bbNum], ";\n\n");
            }
            else
            {
                printf("Unchanged : " FMT_BB " ", block->bbNum);
                compiler->apDumpAssertionIndices("out -> ", block->bbAssertionOut, "; ");
                compiler->apDumpAssertionIndices("jumpDest out -> ", mJumpDestOut[block->bbNum], "\n\n");
            }
        }
#endif

        return changed;
    }

    // Can be enabled to get detailed debug output about dataflow for assertions.
    bool VerboseDataflow()
    {
#if 0
        return compiler->verbose;
#endif
        return false;
    }
};

ASSERT_TP* Compiler::apComputeBlockAssertionGen()
{
    ASSERT_TP* jumpDestGen = fgAllocateTypeForEachBlk<ASSERT_TP>(CMK_AssertionProp);

    for (BasicBlock* const block : Blocks())
    {
        ASSERT_TP valueGen = BitVecOps::MakeEmpty(apTraits);
        GenTree*  jtrue    = nullptr;

        for (Statement* const stmt : block->Statements())
        {
            for (GenTree* const node : stmt->Nodes())
            {
                if (node->OperIs(GT_JTRUE))
                {
                    // A GT_TRUE is always the last node in a tree, so we can break here
                    assert((node->gtNext == nullptr) && (stmt->GetNextStmt() == nullptr));
                    jtrue = node;
                    break;
                }

                if (node->GeneratesAssertion())
                {
                    AssertionInfo info = node->GetAssertionInfo();
                    apAddImpliedAssertions(info.GetAssertionIndex(), valueGen);
                    BitVecOps::AddElemD(apTraits, valueGen, info.GetAssertionIndex() - 1);
                }
            }
        }

        ASSERT_TP jumpDestValueGen;

        if (jtrue == nullptr)
        {
            jumpDestValueGen = BitVecOps::MakeEmpty(apTraits);
        }
        else
        {
            // Copy whatever we have accumulated into jumpDest edge's valueGen.
            jumpDestValueGen = BitVecOps::MakeCopy(apTraits, valueGen);

            if (jtrue->GeneratesAssertion())
            {
                AssertionInfo  info = jtrue->GetAssertionInfo();
                AssertionIndex valueAssertionIndex;
                AssertionIndex jumpDestAssertionIndex;

                if (info.IsNextEdgeAssertion())
                {
                    valueAssertionIndex    = info.GetAssertionIndex();
                    jumpDestAssertionIndex = apFindComplementaryAssertion(info.GetAssertionIndex());
                }
                else // is jump edge assertion
                {
                    jumpDestAssertionIndex = info.GetAssertionIndex();
                    valueAssertionIndex    = apFindComplementaryAssertion(jumpDestAssertionIndex);
                }

                if (valueAssertionIndex != NO_ASSERTION_INDEX)
                {
                    // Update valueGen if we have an assertion for the bbNext edge
                    apAddImpliedAssertions(valueAssertionIndex, valueGen);
                    BitVecOps::AddElemD(apTraits, valueGen, valueAssertionIndex - 1);
                }

                if (jumpDestAssertionIndex != NO_ASSERTION_INDEX)
                {
                    // Update jumpDestValueGen if we have an assertion for the bbJumpDest edge
                    apAddImpliedAssertions(jumpDestAssertionIndex, jumpDestValueGen);
                    BitVecOps::AddElemD(apTraits, jumpDestValueGen, jumpDestAssertionIndex - 1);
                }
            }
        }

        jumpDestGen[block->bbNum] = jumpDestValueGen;
        block->bbAssertionGen     = valueGen;

#ifdef DEBUG
        if (verbose)
        {
            if (block == fgFirstBB)
            {
                printf("\n");
            }

            printf(FMT_BB " valueGen = ", block->bbNum);
            apDumpAssertionIndices("", block->bbAssertionGen, "");
            if (block->bbJumpKind == BBJ_COND)
            {
                printf(" => " FMT_BB " valueGen = ", block->bbJumpDest->bbNum);
                apDumpAssertionIndices("", jumpDestGen[block->bbNum], "");
            }
            printf("\n");

            if (block == fgLastBB)
            {
                printf("\n");
            }
        }
#endif
    }

    return jumpDestGen;
}

ASSERT_TP* Compiler::apInitAssertionDataflowSets()
{
    ASSERT_TP* jumpDestOut = fgAllocateTypeForEachBlk<ASSERT_TP>(CMK_AssertionProp);

    ASSERT_TP allSet = BitVecOps::MakeEmpty(apTraits);

    for (int i = 1; i <= apAssertionCount; i++)
    {
        BitVecOps::AddElemD(apTraits, allSet, i - 1);
    }

    // Initially estimate the OUT sets to everything except killed expressions
    // Also set the IN sets to 1, so that we can perform the intersection.
    for (BasicBlock* const block : Blocks())
    {
        block->bbAssertionGen = BitVecOps::MakeEmpty(apTraits);

        block->bbAssertionIn  = BitVecOps::MakeCopy(apTraits, allSet);
        block->bbAssertionOut = BitVecOps::MakeCopy(apTraits, allSet);

        jumpDestOut[block->bbNum] = BitVecOps::MakeCopy(apTraits, allSet);
    }

    // Compute the data flow values for all tracked expressions
    // IN and OUT never change for the initial basic block B1
    BitVecOps::ClearD(apTraits, fgFirstBB->bbAssertionIn);

    return jumpDestOut;
}

GenTree* Compiler::apExtractConstantSideEffects(GenTree* tree)
{
    assert(vnStore->IsVNConstant(vnStore->VNNormalValue(tree->GetConservativeVN())));

    if ((tree->gtFlags & GTF_SIDE_EFFECT) == 0)
    {
        return nullptr;
    }

    // Do a sanity check to ensure persistent side effects aren't discarded and
    // tell gtExtractSideEffList to ignore the root of the tree.
    assert(!gtNodeHasSideEffects(tree, GTF_PERSISTENT_SIDE_EFFECTS));

    // Exception side effects may be ignored because the root is known to be a constant
    // (e.g. VN may evaluate a DIV/MOD node to a constant and the node may still
    // have GTF_EXCEPT set, even if it does not actually throw any exceptions).
    bool ignoreRoot = true;

    GenTree* sideEffects = nullptr;
    gtExtractSideEffList(tree, &sideEffects, GTF_SIDE_EFFECT, ignoreRoot);
    JITDUMPTREE(sideEffects, "Extracted side effects from constant tree [%06u]:\n", tree->GetID());
    return sideEffects;
}

GenTree* Compiler::apPropagateJTrue(BasicBlock* block, GenTreeUnOp* jtrue)
{
    GenTree* relop = jtrue->GetOp(0);

    // VN based assertion non-null on this relop has been performed.
    if (!relop->OperIsCompare())
    {
        return nullptr;
    }

    assert((relop->gtFlags & GTF_RELOP_JMP_USED) != 0);

    ValueNum relopVN = vnStore->VNNormalValue(relop->GetConservativeVN());

    if (!vnStore->IsVNConstant(relopVN))
    {
        return nullptr;
    }

    GenTree* sideEffects = apExtractConstantSideEffects(relop);

    // Transform the relop into EQ|NE(0, 0)
    ValueNum vnZero = vnStore->VNZeroForType(TYP_INT);
    GenTree* op1    = gtNewIconNode(0);
    op1->SetVNs(ValueNumPair(vnZero, vnZero));
    relop->AsOp()->SetOp(0, op1);
    GenTree* op2 = gtNewIconNode(0);
    op2->SetVNs(ValueNumPair(vnZero, vnZero));
    relop->AsOp()->SetOp(1, op2);
    relop->SetOper(vnStore->CoercedConstantValue<int64_t>(relopVN) != 0 ? GT_EQ : GT_NE);
    ValueNum vnLib = vnStore->VNLiberalNormalValue(relop->gtVNPair);
    relop->SetVNs(ValueNumPair(vnLib, relopVN));

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
    bool           m_stmtMorphPending;

public:
    enum
    {
        DoPreOrder = true
    };

    VNConstPropVisitor(Compiler* compiler) : GenTreeVisitor(compiler), m_vnStore(compiler->vnStore)
    {
    }

    Statement* VisitStmt(BasicBlock* block, Statement* stmt)
    {
        // TODO-Review: EH successor/predecessor iteration seems broken.
        // See: SELF_HOST_TESTS_ARM\jit\Directed\ExcepFilters\fault\fault.exe
        if (block->bbCatchTyp == BBCT_FAULT)
        {
            return stmt;
        }

        m_block = block;

        do
        {
            m_stmtMorphPending = false;

            if (stmt->GetRootNode()->OperIs(GT_JTRUE))
            {
                stmt = PropagateConstJTrue(stmt);

                // JTRUE's operand was constant and without side effects so the entire
                // statement was removed, we're done.
                if (stmt == nullptr)
                {
                    return nullptr;
                }

                // Otherwise we get back the original statement or a new statement, if
                // JTRUE's operand was constant and had side effects. In the later case
                // the JTRUE statement was already removed so we won't traverse it again.
            }

            m_stmt = stmt;

            WalkTree(stmt->GetRootNodePointer(), nullptr);

            if (!m_stmtMorphPending)
            {
                return stmt;
            }

            // Morph may remove the statement, get the previous one so we know where
            // to continue from. This also works in case morph inserts new statements
            // before or after this one, but that's unlikely.
            Statement* prev = (stmt == block->GetFirstStatement()) ? nullptr : stmt->GetPrevStmt();
            m_compiler->fgMorphBlockStmt(block, stmt DEBUGARG("VNConstPropVisitor::VisitStmt"));
            Statement* next = (prev == nullptr) ? block->GetFirstStatement() : prev->GetNextStmt();

            // Morph didn't add/remove any statements, we're done.
            if (next == stmt)
            {
                break;
            }

            // Morph removed the statement or perhaps added new ones before it, do constant
            // propagation on the next statement so we don't return a statment we did not
            // do constant propagation on.
            stmt = next;
        } while (stmt != nullptr);

        return stmt;
    }

    fgWalkResult PreOrderVisit(GenTree** use, GenTree* user)
    {
        PropagateNonNull(*use);
        return PropagateConst(use, user);
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

        GenTree* thisArg = call->GetThisArg();
        noway_assert(thisArg != nullptr);

        if (!m_vnStore->IsKnownNonNull(thisArg->gtVNPair.GetConservative()))
        {
            return;
        }

        JITDUMP("\nCall " FMT_TREEID " has non-null this arg, removing GTF_CALL_NULLCHECK and GTF_EXCEPT\n",
                call->GetID());

        call->gtFlags &= ~(GTF_CALL_NULLCHECK | GTF_EXCEPT);
        noway_assert((call->gtFlags & GTF_SIDE_EFFECT) != 0);

        m_stmtMorphPending = true;
    }

    void PropagateNonNullIndirAddress(GenTreeIndir* indir)
    {
        if ((indir->gtFlags & GTF_EXCEPT) == 0)
        {
            return;
        }

        GenTree*  addr = indir->GetAddr();
        VNFuncApp localAddr;
        bool      isLocalAddr = false;

        if (m_vnStore->GetVNFunc(addr->gtVNPair.GetConservative(), &localAddr) && (localAddr.m_func == VNF_LclAddr))
        {
            isLocalAddr = true;
        }
        else
        {
            if (addr->OperIs(GT_ADD) && addr->AsOp()->GetOp(1)->IsIntCon())
            {
                addr = addr->AsOp()->GetOp(0);
            }

            if (!m_vnStore->IsKnownNonNull(addr->gtVNPair.GetConservative()))
            {
                return;
            }
        }

        JITDUMP("\nIndir " FMT_TREEID " has non-null address, removing GTF_EXCEPT\n", indir->GetID());

        indir->gtFlags &= ~GTF_EXCEPT;
        indir->gtFlags |= GTF_IND_NONFAULTING;

        if (isLocalAddr)
        {
            if (!addr->OperIs(GT_LCL_VAR_ADDR, GT_LCL_FLD_ADDR))
            {
                ChangeToLocalAddress(addr, localAddr);
            }
        }
        else
        {
            // Set this flag to prevent reordering, it may be that we were able to prove that the address
            // is non-null due to a previous indirection or null check that prevent us from getting here
            // with a null address. Of course, a local address is never null so it doesn't need this.
            // TODO-MIKE-Review: Hmm, that's probably only for local assertion propagation...
            indir->gtFlags |= GTF_ORDER_SIDEEFF;
        }

        m_stmtMorphPending = true;
    }

    Statement* PropagateConstJTrue(Statement* stmt)
    {
        assert(stmt->GetRootNode()->OperIs(GT_JTRUE));
        assert(stmt->GetNextStmt() == nullptr);

        GenTree* relop = stmt->GetRootNode()->AsUnOp()->GetOp(0);

        // VN based assertion non-null on this relop has been performed.
        // TODO-MIKE-Review: This should probably be just an assert.
        if (!relop->OperIsCompare())
        {
            return stmt;
        }

        ValueNum vn = m_vnStore->VNConservativeNormalValue(relop->gtVNPair);

        if (!m_vnStore->IsVNConstant(vn))
        {
            return stmt;
        }

        // Extract any existing side effects into separate statements so we only
        // have JTRUE(const) rather than JTRUE(COMMA(X, const)).
        // TODO-MIKE-Review: Why? Seems like fgFoldConditional can deal with a
        // JTRUE(COMMA...) so why bother doing it here, especially considering
        // that this is a rather rare case?
        // fgFoldConditional simply replaces JTRUE(COMMA(X, const)) with X so
        // that side effects are preserved and we continue with normal constant
        // propagation on X.

        GenTree* sideEffects = ExtractConstTreeSideEffects(relop);

        relop->ChangeOperConst(GT_CNS_INT);
        relop->SetType(TYP_INT);
        int32_t value = m_vnStore->CoercedConstantValue<int64_t>(vn) != 0 ? 1 : 0;
        relop->AsIntCon()->SetValue(value);
        vn = m_vnStore->VNForIntCon(value);
        relop->SetVNs(ValueNumPair(vn, vn));

        JITDUMP("After JTRUE constant propagation on " FMT_TREEID ":\n", relop->GetID());
        DBEXEC(VERBOSE, m_compiler->gtDispStmt(stmt));

        bool removed = m_compiler->fgMorphBlockStmt(m_block, stmt DEBUGARG(__FUNCTION__));
        assert(removed);
        assert(m_block->bbJumpKind != BBJ_COND);

        if (sideEffects == nullptr)
        {
            return nullptr;
        }

        // The side effects statement that we're adding needs to be at least sequenced,
        // if not morphed. And we're yet to do constant propagation on this statement,
        // that will also require morphing if any constants are propagated.
        //
        // To avoid unnecessary double morphing don't morph the statement here, just
        // set m_stmtMorphPending to ensure that the statement gets morphed even if
        // no constants are found.
        m_stmtMorphPending = true;

        return m_compiler->fgNewStmtNearEnd(m_block, sideEffects);
    }

    fgWalkResult PropagateConst(GenTree** use, GenTree* user)
    {
        GenTree* tree = *use;

        // We already handled JTRUE's operand, skip it.
        if ((user != nullptr) && user->OperIs(GT_JTRUE))
        {
            return Compiler::WALK_CONTINUE;
        }

        if (tree->TypeIs(TYP_STRUCT))
        {
            // There are no STRUCT VN constants but ZeroMap can be treated as such,
            // at least when the value is used by an assignment, where it's valid
            // to assign a INT 0 value to a struct variable. Also, the tree must not
            // have side effects, so we don't have to introduce a COMMA between the
            // assignment and the INT 0 node (the tree is likely to be a LCL_VAR
            // this case anyway, it can't be an indir as that won't get ZeroMap as
            // a conservative VN).
            // Note that we intentionally ignore the GTF_DONT_CSE flag here, it is
            // usually set for no reason on both STRUCT assignment operands even if
            // only the destination needs it (well, the reason was probably that you
            // can't use a 0 everywhere a STRUCT value may be used, but we're doing
            // this transform only for assignments anyway).

            // TODO-MIKE-CQ: Check what happens with struct args and returns, we
            // probably can't use a constant INT node in all cases but perhaps it
            // can be done when the struct fits in a register. Otherwise we may
            // need a STRUCT typed constant node instead of abusing GT_CNS_INT.

            if ((user != nullptr) && user->OperIs(GT_ASG) && (user->AsOp()->GetOp(1) == tree) &&
                ((tree->gtFlags & GTF_SIDE_EFFECT) == 0) &&
                (m_vnStore->VNConservativeNormalValue(tree->gtVNPair) == m_vnStore->VNForZeroMap()))
            {
                user->AsOp()->SetOp(1, m_compiler->gtNewIconNode(0));
                m_stmtMorphPending = true;
            }

            return Compiler::WALK_CONTINUE;
        }

        if (varTypeIsSIMD(tree->GetType()))
        {
#ifdef FEATURE_HW_INTRINSICS
            ValueNum  vn = m_vnStore->VNConservativeNormalValue(tree->gtVNPair);
            VNFuncApp func;

            if (m_vnStore->GetVNFunc(vn, &func) && (VNFuncIndex(func.m_func) == VNF_HWI_Vector128_get_Zero))
            {
                // Due to poor const register reuse in LSRA, attempting to propagate SIMD zero
                // isn't always an improvement - we simply end up with more XORPS instructions.
                // Still, there's at least on special case where propagation helps, SIMD12
                // memory stores. If codegen sees that the stored value is 0 then it can
                // omit the shuffling required to exract the upper SIMD12 element. We can
                // still end up with an extra XORPS if we propagate but that's better than
                // unnecessary shuffling.
                // Note that this pattern tends to arise due to the use of `default` to get a
                // zero vector, since `default` is translated to `initobj` which needs a temp.

                // TODO-MIKE-CQ: Another case where 0 propagation might be useful is integer
                // equality, lowering can transform it into PTEST if one operand is 0.
                //
                // There are others VNs that could be treated as constants (such as Create
                // with constant operands or get_AllBitsSet) but it's not clear how useful
                // would that be.

                if ((user != nullptr) && user->OperIs(GT_ASG) && (user->AsOp()->GetOp(1) == tree) &&
                    user->AsOp()->GetOp(0)->OperIs(GT_IND, GT_OBJ, GT_LCL_FLD) &&
                    user->AsOp()->GetOp(0)->TypeIs(TYP_SIMD12) && ((tree->gtFlags & GTF_SIDE_EFFECT) == 0))
                {
                    user->AsOp()->SetOp(1, m_compiler->gtNewZeroSimdHWIntrinsicNode(TYP_SIMD12, TYP_FLOAT));
                    m_stmtMorphPending = true;
                }
            }
#endif // FEATURE_HW_INTRINSICS

            return Compiler::WALK_CONTINUE;
        }

        // GTF_DONT_CSE is also used to block constant propagation, not just CSE.
        if (!tree->CanCSE())
        {
            return Compiler::WALK_CONTINUE;
        }

#ifndef TARGET_64BIT
        // We can't replace the operands of a LONG MUL on 32 bit targets, they're
        // supposed to be INT to LONG CASTs and be marked GTF_DONT_CSE.
        assert((user == nullptr) || !user->OperIs(GT_MUL) || !varTypeIsLong(user->GetType()));
#endif

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

            case GT_ADD:
            case GT_SUB:
            case GT_MUL:
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
            case GT_ROL:
            case GT_ROR:
            case GT_BSWAP:
            case GT_BSWAP16:
            case GT_NEG:
            case GT_NOT:
            case GT_CAST:
            case GT_BITCAST:
            case GT_INTRINSIC:
                // Normally these nodes should not have small int type. If they do, it's either due
                // to bogus JIT code or due to BOOL optimizations that "infect" AND/OR (though that
                // is still more or less due to bogus desig/code). The former case is best ignored,
                // in order to avoid surprises due to bad VN, the later case is unlikely to involve
                // constants, as BOOL expressions tend to use BOOL indirs or BOOL HWINTRINSIC nodes
                // that aren't currently constant evaluated.
                if (varTypeIsSmall(tree->GetType()))
                {
                    return Compiler::WALK_CONTINUE;
                }
                break;

            default:
                return Compiler::WALK_CONTINUE;
        }

        GenTree* newTree = GetConstNode(tree);

        if (newTree != nullptr)
        {
            assert(newTree != tree);

            if (user == nullptr)
            {
                assert(tree == m_stmt->GetRootNode());
                m_stmt->SetRootNode(newTree);
            }
            else
            {
                user->ReplaceOperand(use, newTree);
            }

            JITDUMP("After constant propagation on " FMT_TREEID ":\n", tree->GetID());
            DBEXEC(VERBOSE, m_compiler->gtDispStmt(m_stmt));

            DEBUG_DESTROY_NODE(tree);

            m_stmtMorphPending = true;
        }
        else if ((user != nullptr) && user->IsCall())
        {
            // Change call arguments that are VN local addresses to be local address nodes.
            // We already changed indir addresses and it's unlikely that we'll get local
            // addresses anywhere else. This is especially useful for call args, as it's more
            // likely that the local address will be loaded directly into a call argument reg
            // instead of being loaded into another reg and be moved later. In theory this can
            // undo CSEs but we shouldn't be CSEing local addresses to begin with. They're
            // pretty cheap to produce and doing CSE before calls is likely to make things
            // worse by increasing register pressure, which can be pretty high around calls.

            VNFuncApp lclAddr;

            if (m_vnStore->GetVNFunc(tree->gtVNPair.GetConservative(), &lclAddr) && (lclAddr.m_func == VNF_LclAddr) &&
                ChangeToLocalAddress(tree, lclAddr))
            {
                return Compiler::WALK_SKIP_SUBTREES;
            }
        }

        return Compiler::WALK_CONTINUE;
    }

    GenTree* GetConstNode(GenTree* tree)
    {
        ValueNum vn = m_vnStore->VNConservativeNormalValue(tree->gtVNPair);

        if (!m_vnStore->IsVNConstant(vn))
        {
            return nullptr;
        }

        var_types vnType  = m_vnStore->TypeOfVN(vn);
        GenTree*  newTree = nullptr;

        if (m_vnStore->IsVNHandle(vn))
        {
            assert(vnType == TYP_I_IMPL);

            // Don't perform constant folding that involves a handle that needs to be recorded
            // as a relocation with the VM. The VN type should be TYP_I_IMPL but the tree may
            // sometimes be TYP_BYREF, due to things like Unsafe.As.
            if (!m_compiler->opts.compReloc && tree->TypeIs(TYP_I_IMPL, TYP_BYREF))
            {
                newTree = m_compiler->gtNewIconHandleNode(m_vnStore->ConstantValue<target_ssize_t>(vn),
                                                          m_vnStore->GetHandleFlags(vn));
            }
        }
        // The tree type and the VN type should match but VN can't be trusted. At least for SIMD
        // locals, VN manages to pull out a TYP_LONG 0 constant out of the hat, if the local is
        // not explictily initialized and .locals init is used.
        else if (varActualType(tree->GetType()) == vnType)
        {
            switch (vnType)
            {
                case TYP_FLOAT:
                    newTree = m_compiler->gtNewDconNode(m_vnStore->ConstantValue<float>(vn), TYP_FLOAT);
                    break;
                case TYP_DOUBLE:
                    newTree = m_compiler->gtNewDconNode(m_vnStore->ConstantValue<double>(vn), TYP_DOUBLE);
                    break;
                case TYP_UBYTE:
                case TYP_BOOL:
                    newTree = m_compiler->gtNewIconNode(m_vnStore->CoercedConstantValue<uint8_t>(vn));
                    break;
                case TYP_BYTE:
                    newTree = m_compiler->gtNewIconNode(m_vnStore->CoercedConstantValue<int8_t>(vn));
                    break;
                case TYP_USHORT:
                    newTree = m_compiler->gtNewIconNode(m_vnStore->CoercedConstantValue<uint16_t>(vn));
                    break;
                case TYP_SHORT:
                    newTree = m_compiler->gtNewIconNode(m_vnStore->CoercedConstantValue<int16_t>(vn));
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

        newTree->SetVNs(ValueNumPair(vn, vn));

        GenTree* sideEffects = ExtractConstTreeSideEffects(tree);

        if (sideEffects != nullptr)
        {
            assert((sideEffects->gtFlags & GTF_SIDE_EFFECT) != 0);

            newTree = m_compiler->gtNewCommaNode(sideEffects, newTree);
            newTree->SetVNs(tree->gtVNPair);
        }

        return newTree;
    }

    GenTree* ExtractConstTreeSideEffects(GenTree* tree)
    {
        if ((tree->gtFlags & GTF_SIDE_EFFECT) == 0)
        {
            return nullptr;
        }

        // Do a sanity check to ensure persistent side effects aren't discarded.
        assert(!m_compiler->gtNodeHasSideEffects(tree, GTF_PERSISTENT_SIDE_EFFECTS));

        // Exception side effects on root may be ignored because the root is known to be a constant
        // (e.g. VN may evaluate a DIV/MOD node to a constant and the node may still
        // have GTF_EXCEPT set, even if it does not actually throw any exceptions).
        assert(m_vnStore->IsVNConstant(m_vnStore->VNConservativeNormalValue(tree->gtVNPair)));

        GenTree* sideEffects = nullptr;
        m_compiler->gtExtractSideEffList(tree, &sideEffects, GTF_SIDE_EFFECT, /* ignoreRoot */ true);
        return sideEffects;
    }

    bool ChangeToLocalAddress(GenTree* node, const VNFuncApp& lclAddr)
    {
        assert(lclAddr.m_func == VNF_LclAddr);

        // It doesn't seem to be worth the trouble dealing with side effects on local address trees,
        // they do exist but they're usually COMMAs where the value operand is a LCL_VAR|FLD_ADDR
        // node already.

        if (node->GetSideEffects() != 0)
        {
            return false;
        }

        // TODO-MIKE-Cleanup: If the user is an indir then we leave it to morph to change it to
        // a LCL_FLD. Perhaps we should just do it here?

        // TODO-MIKE-CQ: But then it's not like doing this avoids local address exposure at this
        // stage. We'd need to eliminate local address containing temps that may now be dead due
        // to this transform then figure out if there are no more local address nodes left for a
        // given local and reset lvAddressExposed. Doable, but possibly expensive for throughput
        // and such cases aren't that common.
        // This really should be done via SSA. But for now it's good enough to remove a bunch of
        // LEAs that the JIT has been generating from the dawn of time.

        unsigned      lclNum   = m_vnStore->ConstantValue<unsigned>(lclAddr.m_args[0]);
        target_size_t offset   = m_vnStore->ConstantValue<target_size_t>(lclAddr.m_args[1]);
        FieldSeqNode* fieldSeq = m_vnStore->FieldSeqVNToFieldSeq(lclAddr.m_args[2]);

        if ((offset > UINT16_MAX) || (offset >= m_compiler->lvaGetDesc(lclNum)->GetSize()))
        {
            return false;
        }

        if ((offset == 0) && (fieldSeq == nullptr))
        {
            node->ChangeOper(GT_LCL_VAR_ADDR);
        }
        else
        {
            node->ChangeOper(GT_LCL_FLD_ADDR);
            node->AsLclFld()->SetLclOffs(static_cast<unsigned>(offset));
            node->AsLclFld()->SetFieldSeq(fieldSeq == nullptr ? FieldSeqNode::NotAField() : fieldSeq);
        }

        node->AsLclVarCommon()->SetLclNum(lclNum);
        node->SetType(TYP_I_IMPL);

        m_stmtMorphPending = true;

        return true;
    }
};

void Compiler::apMain()
{
    DBEXEC(verbose, fgDispBasicBlocks(true);)
    assert(ssaForm && (vnStore != nullptr));

    apInit();

    // Traverse all blocks, perform VN constant propagation and generate assertions.
    for (BasicBlock* const block : Blocks())
    {
        compCurBB = block;

        for (Statement* stmt = block->GetFirstStatement(); stmt != nullptr; stmt = stmt->GetNextStmt())
        {
            VNConstPropVisitor visitor(this);
            stmt = visitor.VisitStmt(block, stmt);

            if (stmt == nullptr)
            {
                break;
            }

            for (GenTree* const node : stmt->Nodes())
            {
                apGenerateNodeAssertions(node);
            }
        }
    }

    if (apAssertionCount == 0)
    {
        // Zero out bbAssertionIn as it can be referenced in RangeCheck::MergeAssertion
        // and this member overlaps with bbCseIn used by the CSE phase.
        for (BasicBlock* const block : Blocks())
        {
            block->bbAssertionIn = BitVecOps::MakeEmpty(apTraits);
        }

        return;
    }

    INDEBUG(fgDebugCheckLinks());

    // Allocate the bits for the predicate sensitive dataflow analysis
    apJTrueAssertionOut    = apInitAssertionDataflowSets();
    ASSERT_TP* jumpDestGen = apComputeBlockAssertionGen();

    // Modified dataflow algorithm for available expressions.
    DataFlow                  flow(this);
    AssertionPropFlowCallback ap(this, apJTrueAssertionOut, jumpDestGen);

    if (ap.VerboseDataflow())
    {
        JITDUMP("AssertionPropFlowCallback:\n\n")
    }

    flow.ForwardAnalysis(ap);

    for (BasicBlock* const block : Blocks())
    {
        apAddTypeImpliedNotNullAssertions(block->bbAssertionIn);
    }

#ifdef DEBUG
    if (verbose)
    {
        for (BasicBlock* const block : Blocks())
        {
            printf(FMT_BB ":\n", block->bbNum);
            apDumpAssertionIndices(" in   = ", block->bbAssertionIn, "\n");
            apDumpAssertionIndices(" out  = ", block->bbAssertionOut, "\n");
            if (block->bbJumpKind == BBJ_COND)
            {
                printf(" " FMT_BB " = ", block->bbJumpDest->bbNum);
                apDumpAssertionIndices("", apJTrueAssertionOut[block->bbNum], "\n");
            }
        }
        printf("\n");
    }
#endif // DEBUG

    ASSERT_TP assertions = BitVecOps::MakeEmpty(apTraits);

    for (BasicBlock* const block : Blocks())
    {
        BitVecOps::Assign(apTraits, assertions, block->bbAssertionIn);

        // TODO-Review: EH successor/predecessor iteration seems broken.
        // SELF_HOST_TESTS_ARM\jit\Directed\ExcepFilters\fault\fault.exe
        if (block->bbCatchTyp == BBCT_FAULT)
        {
            continue;
        }

        compCurBB           = block;
        fgRemoveRestOfBlock = false;

        for (Statement* stmt = block->FirstNonPhiDef(); stmt != nullptr;)
        {
            if (fgRemoveRestOfBlock)
            {
                fgRemoveStmt(block, stmt);
                stmt = stmt->GetNextStmt();

                continue;
            }

            // Preserve the prev link before the propagation and morph, to check if propagation
            // removes the current stmt.
            Statement* prevStmt = (stmt == block->firstStmt()) ? nullptr : stmt->GetPrevStmt();

            apStmtMorphPending = false;

            for (GenTree* node = stmt->GetNodeList(); node != nullptr; node = node->gtNext)
            {
                INDEBUG(apDumpAssertionIndices("Propagating ", assertions, " "));
                JITDUMP("for " FMT_BB ", stmt " FMT_STMT ", tree [%06u]", block->bbNum, stmt->GetID(), node->GetID());
                JITDUMP(", tree -> A%02d\n", node->GetAssertionInfo().GetAssertionIndex());

                GenTree* newNode = apPropagateNode(assertions, node, stmt, block);

                if (newNode != nullptr)
                {
                    assert(apStmtMorphPending);
                    node = newNode;
                }

                if (node->GeneratesAssertion())
                {
                    AssertionInfo info = node->GetAssertionInfo();
                    apAddImpliedAssertions(info.GetAssertionIndex(), assertions);
                    BitVecOps::AddElemD(apTraits, assertions, info.GetAssertionIndex() - 1);
                }
            }

            if (apStmtMorphPending)
            {
#ifdef DEBUG
                if (verbose)
                {
                    printf("Re-morphing this stmt:\n");
                    gtDispStmt(stmt);
                    printf("\n");
                }
#endif

                fgMorphBlockStmt(block, stmt DEBUGARG("VNAssertionProp"));
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

#ifdef DEBUG

void Compiler::apDebugCheckAssertion(AssertionDsc* assertion)
{
    assert((assertion->kind > OAK_INVALID) && (assertion->kind < OAK_COUNT));

    const auto& op1 = assertion->op1;
    const auto& op2 = assertion->op2;

    switch (op1.kind)
    {
        case O1K_LCLVAR:
        case O1K_EXACT_TYPE:
        case O1K_SUBTYPE:
            assert(lvaGetDesc(op1.lcl.lclNum)->lvPerSsaData.IsValidSsaNum(op1.lcl.ssaNum));
            break;
        case O1K_ARR_BND:
            assert(varTypeIsIntegral(vnStore->TypeOfVN(op1.bnd.vnIdx)) &&
                   varTypeIsIntegral(vnStore->TypeOfVN(op1.bnd.vnLen)));
            break;
        case O1K_BOUND_OPER_BND:
        case O1K_BOUND_LOOP_BND:
        case O1K_CONSTANT_LOOP_BND:
        case O1K_VALUE_NUMBER:
            break;
        default:
            assert(!"Invalid assertion op1 kind");
            break;
    }

    switch (op2.kind)
    {
        case O2K_IND_CNS_INT:
        case O2K_CONST_INT:
#ifdef TARGET_64BIT
            assert((op2.intCon.flags & ~(GTF_ICON_HDL_MASK | GTF_ASSERTION_PROP_LONG)) == 0);
#else
            assert((op2.intCon.flags & ~GTF_ICON_HDL_MASK) == 0);
#endif
            switch (op1.kind)
            {
                case O1K_EXACT_TYPE:
                case O1K_SUBTYPE:
                    assert(op2.intCon.flags != GTF_EMPTY);
                    break;
                case O1K_LCLVAR:
                    assert(!lvaGetDesc(op1.lcl.lclNum)->TypeIs(TYP_REF) || (op2.intCon.value == 0) ||
                           doesMethodHaveFrozenString());
                    break;
                case O1K_VALUE_NUMBER:
                    assert((vnStore->TypeOfVN(op1.vn) != TYP_REF) || (op2.intCon.value == 0));
                    break;
                default:
                    break;
            }
            break;

#ifndef TARGET_64BIT
        case O2K_CONST_LONG:
#endif
        case O2K_CONST_DOUBLE:
        case O2K_LCLVAR_COPY:
        case O2K_SUBRANGE:
            break;

        default:
            assert(op2.kind == O2K_INVALID);
            break;
    }
}

void Compiler::apDebugCheckAssertionTable()
{
    for (AssertionIndex index = 1; index <= apAssertionCount; ++index)
    {
        apDebugCheckAssertion(apGetAssertion(index));
    }
}

void Compiler::apDumpVNAssertionMap()
{
    printf("\nVN Assertion Mapping:\n");

    for (const auto& pair : *apVNAssertionMap)
    {
        printf("$%u => %s\n", pair.key, BitVecOps::ToString(apTraits, pair.value));
    }
}

void Compiler::apDumpAssertion(const AssertionDsc* assertion)
{
    const auto  kind = assertion->kind;
    const auto& op1  = assertion->op1;
    const auto& op2  = assertion->op2;

    const char* kindName = "???";

    if (op1.kind == O1K_EXACT_TYPE)
    {
        kindName = "ExactType";
    }
    else if (op1.kind == O1K_SUBTYPE)
    {
        kindName = "Subtype";
    }
    else if (op1.kind == O1K_ARR_BND)
    {
        kindName = "ArrayBounds";
    }
    else if (op2.kind == O2K_LCLVAR_COPY)
    {
        kindName = "Copy";
    }
    else if ((op2.kind == O2K_CONST_INT) ||
#ifndef TARGET_64BIT
             (op2.kind == O2K_CONST_LONG) ||
#endif
             (op2.kind == O2K_CONST_DOUBLE))
    {
        kindName = "Const";
    }
    else if (op2.kind == O2K_SUBRANGE)
    {
        kindName = "Subrange";
    }

    printf("%s assertion ", kindName);

    if ((apAssertionTable <= assertion) && (assertion < apAssertionTable + apAssertionCount))
    {
        printf("A%02d ", static_cast<int>(assertion - apAssertionTable) + 1);
    }

    if (kind != OAK_NO_THROW)
    {
        printf("(" FMT_VN ", " FMT_VN ") ", op1.vn, op2.vn);
    }

    if ((op1.kind == O1K_LCLVAR) || (op1.kind == O1K_EXACT_TYPE) || (op1.kind == O1K_SUBTYPE))
    {
        printf("V%02u", op1.lcl.lclNum);

        if (op1.lcl.ssaNum != SsaConfig::RESERVED_SSA_NUM)
        {
            printf(".%02u", op1.lcl.ssaNum);
        }
    }
    else if (op1.kind == O1K_ARR_BND)
    {
        printf("[index:");
        vnStore->vnDump(this, op1.bnd.vnIdx);
        printf(", length:");
        vnStore->vnDump(this, op1.bnd.vnLen);
        printf("]");
    }
    else if (op1.kind == O1K_BOUND_OPER_BND)
    {
        printf("OperBound");
        vnStore->vnDump(this, op1.vn);
    }
    else if (op1.kind == O1K_BOUND_LOOP_BND)
    {
        printf("LoopBound");
        vnStore->vnDump(this, op1.vn);
    }
    else if (op1.kind == O1K_CONSTANT_LOOP_BND)
    {
        printf("ConstLoopBound");
        vnStore->vnDump(this, op1.vn);
    }
    else if (op1.kind == O1K_VALUE_NUMBER)
    {
        printf("ValueNumber");
        vnStore->vnDump(this, op1.vn);
    }
    else
    {
        printf("???");
    }

    if (kind == OAK_SUBRANGE)
    {
        printf(" in ");
    }
    else if (kind == OAK_EQUAL)
    {
        if (op1.kind == O1K_LCLVAR)
        {
            printf(" == ");
        }
        else
        {
            printf(" is ");
        }
    }
    else if (kind == OAK_NOT_EQUAL)
    {
        if (op1.kind == O1K_LCLVAR)
        {
            printf(" != ");
        }
        else
        {
            printf(" is not ");
        }
    }
    else if (kind != OAK_NO_THROW)
    {
        printf(" ??? ");
    }

    if (op1.kind != O1K_ARR_BND)
    {
        switch (op2.kind)
        {
            case O2K_LCLVAR_COPY:
                printf("V%02u", op2.lcl.lclNum);
                if (op1.lcl.ssaNum != SsaConfig::RESERVED_SSA_NUM)
                {
                    printf(".%02u", op1.lcl.ssaNum);
                }
                break;

            case O2K_CONST_INT:
            case O2K_IND_CNS_INT:
                if ((op1.kind == O1K_EXACT_TYPE) || (op1.kind == O1K_SUBTYPE))
                {
                    printf("MT(%08X)", dspPtr(op2.intCon.value));
                }
                else if (op1.kind == O1K_BOUND_OPER_BND)
                {
                    vnStore->vnDump(this, op2.vn);
                }
                else if (op1.kind == O1K_BOUND_LOOP_BND)
                {
                    vnStore->vnDump(this, op2.vn);
                }
                else if (op1.kind == O1K_CONSTANT_LOOP_BND)
                {
                    vnStore->vnDump(this, op2.vn);
                }
                else
                {
                    var_types op1Type;

                    if (op1.kind == O1K_VALUE_NUMBER)
                    {
                        op1Type = vnStore->TypeOfVN(op1.vn);
                    }
                    else
                    {
                        op1Type = lvaGetDesc(op1.lcl.lclNum)->GetType();
                    }

                    if ((op1Type == TYP_REF) && (op2.intCon.value == 0))
                    {
                        printf("null");
                    }
                    else if ((op2.intCon.flags & GTF_ICON_HDL_MASK) != 0)
                    {
                        printf("[%08p]", dspPtr(op2.intCon.value));
                    }
                    else
                    {
                        printf("%d", op2.intCon.value);
                    }
                }
                break;

#ifndef TARGET_64BIT
            case O2K_CONST_LONG:
                printf("0x%016llx", op2.lngCon.value);
                break;
#endif
            case O2K_CONST_DOUBLE:
                printf("%#.17g", op2.dblCon.value);
                break;
            case O2K_SUBRANGE:
                printf("[%d..%d]", op2.range.min, op2.range.max);
                break;
            default:
                printf("???");
                break;
        }
    }

    printf("\n");
}

void Compiler::apDumpAssertionIndices(const char* header, ASSERT_TP assertions, const char* footer)
{
    if (!verbose)
    {
        return;
    }

    if (header != nullptr)
    {
        printf("%s", header);
    }

    const char* separator = "";

    for (BitVecOps::Enumerator en(apTraits, assertions); en.MoveNext();)
    {
        printf("%sA%02d", separator, GetAssertionIndex(en.Current()));
        separator = ", ";
    }

    if (footer != nullptr)
    {
        printf("%s", footer);
    }
}

#endif // DEBUG
