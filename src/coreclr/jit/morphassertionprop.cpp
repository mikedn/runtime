// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

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

    JitExpandArray<ASSERT_TP>& dep = *morphAssertionDep;
    if (dep[lclNum] == nullptr)
    {
        dep[lclNum] = BitVecOps::MakeEmpty(apTraits);
    }
    return dep[lclNum];
}

/*****************************************************************************
 *
 *  Initialize the assertion prop tracking logic.
 */

void Compiler::morphAssertionInit()
{
    optMaxAssertionCount = 64;

    optLocalAssertionProp  = true;
    optAssertionTabPrivate = new (this, CMK_AssertionProp) AssertionDsc[optMaxAssertionCount];

    assert(NO_ASSERTION_INDEX == 0);

    if (morphAssertionDep == nullptr)
    {
        morphAssertionDep =
            new (this, CMK_AssertionProp) JitExpandArray<ASSERT_TP>(getAllocator(CMK_AssertionProp), max(1, lvaCount));
    }

    apTraits            = new (this, CMK_AssertionProp) BitVecTraits(optMaxAssertionCount, this);
    optAssertionCount   = 0;
    bbJtrueAssertionOut = nullptr;
}

#if LOCAL_ASSERTION_PROP

// The following resets the value assignment table
// used only during local assertion prop
void Compiler::morphAssertionReset(AssertionIndex limit)
{
    PREFAST_ASSUME(optAssertionCount <= optMaxAssertionCount);

    while (optAssertionCount > limit)
    {
        AssertionIndex index        = optAssertionCount;
        AssertionDsc*  curAssertion = morphGetAssertion(index);
        optAssertionCount--;
        unsigned lclNum = curAssertion->op1.lcl.lclNum;
        BitVecOps::RemoveElemD(apTraits, GetAssertionDep(lclNum), index - 1);

        //
        // Find the Copy assertions
        //
        if ((curAssertion->assertionKind == OAK_EQUAL) && (curAssertion->op1.kind == O1K_LCLVAR) &&
            (curAssertion->op2.kind == O2K_LCLVAR_COPY))
        {
            //
            //  op2.lcl.lclNum no longer depends upon this assertion
            //
            lclNum = curAssertion->op2.lcl.lclNum;
            BitVecOps::RemoveElemD(apTraits, GetAssertionDep(lclNum), index - 1);
        }
    }
    while (optAssertionCount < limit)
    {
        AssertionIndex index        = ++optAssertionCount;
        AssertionDsc*  curAssertion = morphGetAssertion(index);
        unsigned       lclNum       = curAssertion->op1.lcl.lclNum;
        BitVecOps::AddElemD(apTraits, GetAssertionDep(lclNum), index - 1);

        //
        // Check for Copy assertions
        //
        if ((curAssertion->assertionKind == OAK_EQUAL) && (curAssertion->op1.kind == O1K_LCLVAR) &&
            (curAssertion->op2.kind == O2K_LCLVAR_COPY))
        {
            //
            //  op2.lcl.lclNum now depends upon this assertion
            //
            lclNum = curAssertion->op2.lcl.lclNum;
            BitVecOps::AddElemD(apTraits, GetAssertionDep(lclNum), index - 1);
        }
    }
}

/*****************************************************************************
 *
 *  The following removes the i-th entry in the value assignment table
 *  used only during local assertion prop
 */

void Compiler::morphAssertionRemove(AssertionIndex index)
{
    assert(index > 0);
    assert(index <= optAssertionCount);
    PREFAST_ASSUME(optAssertionCount <= optMaxAssertionCount);

    AssertionDsc* curAssertion = morphGetAssertion(index);

    //  Two cases to consider if (index == optAssertionCount) then the last
    //  entry in the table is to be removed and that happens automatically when
    //  optAssertionCount is decremented and we can just clear the morphAssertionDep bits
    //  The other case is when index < optAssertionCount and here we overwrite the
    //  index-th entry in the table with the data found at the end of the table
    //  Since we are reordering the rable the morphAssertionDep bits need to be recreated
    //  using optAssertionReset(0) and optAssertionReset(newAssertionCount) will
    //  correctly update the morphAssertionDep bits
    //
    if (index == optAssertionCount)
    {
        unsigned lclNum = curAssertion->op1.lcl.lclNum;
        BitVecOps::RemoveElemD(apTraits, GetAssertionDep(lclNum), index - 1);

        //
        // Check for Copy assertions
        //
        if ((curAssertion->assertionKind == OAK_EQUAL) && (curAssertion->op1.kind == O1K_LCLVAR) &&
            (curAssertion->op2.kind == O2K_LCLVAR_COPY))
        {
            //
            //  op2.lcl.lclNum no longer depends upon this assertion
            //
            lclNum = curAssertion->op2.lcl.lclNum;
            BitVecOps::RemoveElemD(apTraits, GetAssertionDep(lclNum), index - 1);
        }

        optAssertionCount--;
    }
    else
    {
        AssertionDsc*  lastAssertion     = morphGetAssertion(optAssertionCount);
        AssertionIndex newAssertionCount = optAssertionCount - 1;

        morphAssertionReset(0); // This make optAssertionCount equal 0

        memcpy(curAssertion,  // the entry to be removed
               lastAssertion, // last entry in the table
               sizeof(AssertionDsc));

        morphAssertionReset(newAssertionCount);
    }
}

void Compiler::morphAssertionMerge(unsigned      elseAssertionCount,
                                   AssertionDsc* elseAssertionTab DEBUGARG(GenTreeQmark* qmark))
{
    if (optAssertionCount == 0)
    {
        return;
    }

    if (elseAssertionCount == 0)
    {
        morphAssertionReset(0);
        return;
    }

    if ((optAssertionCount == elseAssertionCount) &&
        (memcmp(elseAssertionTab, optAssertionTabPrivate, optAssertionCount * sizeof(AssertionDsc)) == 0))
    {
        return;
    }

    for (AssertionIndex index = 1; index <= optAssertionCount;)
    {
        AssertionDsc* thenAssertion = morphGetAssertion(index);
        AssertionDsc* elseAssertion = nullptr;

        for (unsigned j = 0; j < elseAssertionCount; j++)
        {
            AssertionDsc* assertion = &elseAssertionTab[j];

            if ((assertion->assertionKind == thenAssertion->assertionKind) &&
                (assertion->op1.lcl.lclNum == thenAssertion->op1.lcl.lclNum))
            {
                elseAssertion = assertion;
                break;
            }
        }

        if ((elseAssertion != nullptr) && elseAssertion->HasSameOp2(thenAssertion, false))
        {
            index++;
        }
        else
        {
            JITDUMP("The QMARK [%06u] removes assertion candidate #%d\n", qmark->GetID(), index);
            morphAssertionRemove(index);
        }
    }
}

#endif // LOCAL_ASSERTION_PROP

#ifdef DEBUG
void Compiler::morphPrintAssertion(AssertionDsc* curAssertion, AssertionIndex assertionIndex /* = 0 */)
{
    if (curAssertion->op1.kind == O1K_EXACT_TYPE)
    {
        printf("Type     ");
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

    if ((curAssertion->op1.kind == O1K_LCLVAR) || (curAssertion->op1.kind == O1K_EXACT_TYPE) ||
        (curAssertion->op1.kind == O1K_SUBTYPE))
    {
        printf("V%02u", curAssertion->op1.lcl.lclNum);
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
        {
            unsigned lclNum = curAssertion->op1.lcl.lclNum;
            assert(lclNum < lvaCount);
            LclVarDsc* varDsc  = lvaTable + lclNum;
            var_types  op1Type = varDsc->lvType;

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
            printf("[%u..%u]", curAssertion->op2.u2.loBound, curAssertion->op2.u2.hiBound);
            break;

        default:
            printf("?op2.kind?");
            break;
    }

    if (assertionIndex > 0)
    {
        printf(", index = ");
        morphPrintAssertionIndex(assertionIndex);
    }
    printf("\n");
}

void Compiler::morphPrintAssertionIndex(AssertionIndex index)
{
    if (index == NO_ASSERTION_INDEX)
    {
        printf("#NA");
        return;
    }

    printf("#%02u", index);
}

#endif // DEBUG

/******************************************************************************
 *
 * Helper to retrieve the "assertIndex" assertion. Note that assertIndex 0
 * is NO_ASSERTION_INDEX and "optAssertionCount" is the last valid index.
 *
 */
Compiler::AssertionDsc* Compiler::morphGetAssertion(AssertionIndex assertIndex)
{
    assert(NO_ASSERTION_INDEX == 0);
    assert(assertIndex != NO_ASSERTION_INDEX);
    assert(assertIndex <= optAssertionCount);
    AssertionDsc* assertion = &optAssertionTabPrivate[assertIndex - 1];
#ifdef DEBUG
    morphDebugCheckAssertion(assertion);
#endif

    return assertion;
}

//------------------------------------------------------------------------
// morphCreateAssertion: Create an (op1 assertionKind op2) assertion.
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
AssertionIndex Compiler::morphCreateAssertion(GenTree* op1, GenTree* op2, const optAssertionKind assertionKind)
{
    assert(op1 != nullptr);
    assert((op2 != nullptr) || (assertionKind == OAK_NOT_EQUAL));
    assert((assertionKind == OAK_EQUAL) || (assertionKind == OAK_NOT_EQUAL));

    AssertionDsc assertion;
    memset(&assertion, 0, sizeof(AssertionDsc));
    assert(assertion.assertionKind == OAK_INVALID);

    var_types toType;

    //
    // Are we trying to make a non-null assertion?
    //
    if (op2 == nullptr)
    {
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

        //
        // We only perform null-checks on GC refs
        // so only make non-null assertions about GC refs or byrefs if we can't determine
        // the corresponding ref.
        //
        if (lclVar->TypeGet() != TYP_REF)
        {
            goto DONE_ASSERTION; // Don't make an assertion
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
        }

        assertion.assertionKind    = OAK_NOT_EQUAL;
        assertion.op2.kind         = O2K_CONST_INT;
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

        {
            op2 = op2->SkipComma();

            assertion.op1.kind       = O1K_LCLVAR;
            assertion.op1.lcl.lclNum = lclNum;

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
                    // If the LclVar is a TYP_LONG then we only make
                    // assertions where op2 is also TYP_LONG
                    //
                    if ((lclVar->TypeGet() == TYP_LONG) && (op2->TypeGet() != TYP_LONG))
                    {
                        goto DONE_ASSERTION; // Don't make an assertion
                    }

                    assertion.op2.kind    = op2Kind;
                    assertion.op2.lconVal = 0;

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
                    if (assertionKind != OAK_EQUAL)
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
    }

DONE_ASSERTION:
    if (assertion.assertionKind == OAK_INVALID)
    {
        return NO_ASSERTION_INDEX;
    }

    // Now add the assertion to our assertion table
    noway_assert(assertion.op1.kind != O1K_INVALID);
    noway_assert(assertion.op2.kind != O2K_INVALID);
    return morphAddAssertion(&assertion);
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
AssertionIndex Compiler::morphAddAssertion(AssertionDsc* newAssertion)
{
    noway_assert(newAssertion->assertionKind != OAK_INVALID);

    // Check if exists already, so we can skip adding new one. Search backwards.
    for (AssertionIndex index = optAssertionCount; index >= 1; index--)
    {
        AssertionDsc* curAssertion = morphGetAssertion(index);
        if (curAssertion->Equals(newAssertion, false))
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
        printf("In " FMT_BB " New Local ", compCurBB->bbNum);
        morphPrintAssertion(newAssertion, optAssertionCount);
    }
#endif // DEBUG

    // Assertion mask bits are [index + 1].
    assert(newAssertion->op1.kind == O1K_LCLVAR);

    // Mark the variables this index depends on
    unsigned lclNum = newAssertion->op1.lcl.lclNum;
    BitVecOps::AddElemD(apTraits, GetAssertionDep(lclNum), optAssertionCount - 1);
    if (newAssertion->op2.kind == O2K_LCLVAR_COPY)
    {
        lclNum = newAssertion->op2.lcl.lclNum;
        BitVecOps::AddElemD(apTraits, GetAssertionDep(lclNum), optAssertionCount - 1);
    }

#ifdef DEBUG
    morphDebugCheckAssertions(optAssertionCount);
#endif
    return optAssertionCount;
}

#ifdef DEBUG
void Compiler::morphDebugCheckAssertion(AssertionDsc* assertion)
{
    assert(assertion->assertionKind < OAK_COUNT);
    assert(assertion->op1.kind < O1K_COUNT);
    assert(assertion->op2.kind < O2K_COUNT);

    switch (assertion->op1.kind)
    {
        case O1K_LCLVAR:
        case O1K_EXACT_TYPE:
        case O1K_SUBTYPE:
            assert(assertion->op1.lcl.lclNum < lvaCount);
            break;
        case O1K_ARR_BND:
        case O1K_BOUND_OPER_BND:
        case O1K_BOUND_LOOP_BND:
        case O1K_CONSTANT_LOOP_BND:
        case O1K_VALUE_NUMBER:
            assert(false);
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
                    assert(false);
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
void Compiler::morphDebugCheckAssertions(AssertionIndex index)
{
    AssertionIndex start = (index == NO_ASSERTION_INDEX) ? 1 : index;
    AssertionIndex end   = (index == NO_ASSERTION_INDEX) ? optAssertionCount : index;
    for (AssertionIndex ind = start; ind <= end; ++ind)
    {
        AssertionDsc* assertion = morphGetAssertion(ind);
        morphDebugCheckAssertion(assertion);
    }
}
#endif

/*****************************************************************************
 *
 *  If this statement creates a value assignment or assertion
 *  then assign an index to the given value assignment by adding
 *  it to the lookup table, if necessary.
 */
void Compiler::morphAssertionGen(GenTree* tree)
{
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
            assertionInfo = morphCreateAssertion(tree->AsOp()->gtOp1, tree->AsOp()->gtOp2, OAK_EQUAL);
            break;

        case GT_BLK:
        case GT_OBJ:
            assert(tree->AsBlk()->GetLayout()->GetSize() != 0);
            FALLTHROUGH;
        case GT_IND:
        case GT_NULLCHECK:
            // All indirections create non-null assertions
            assertionInfo = morphCreateAssertion(tree->AsIndir()->Addr(), nullptr, OAK_NOT_EQUAL);
            break;

        case GT_ARR_LENGTH:
            assertionInfo = morphCreateAssertion(tree->AsArrLen()->GetArray(), nullptr, OAK_NOT_EQUAL);
            break;

        case GT_ARR_ELEM:
            // An array element reference can create a non-null assertion
            assertionInfo = morphCreateAssertion(tree->AsArrElem()->gtArrObj, nullptr, OAK_NOT_EQUAL);
            break;

        case GT_CALL:
        {
            // A virtual call can create a non-null assertion. We transform some virtual calls into non-virtual calls
            // with a GTF_CALL_NULLCHECK flag set.
            // Ignore tail calls because they have 'this` pointer in the regular arg list and an implicit null check.
            GenTreeCall* const call = tree->AsCall();
            if (call->NeedsNullCheck() || (call->IsVirtual() && !call->IsTailCall()))
            {
                assertionInfo = morphCreateAssertion(call->GetThisArg(), nullptr, OAK_NOT_EQUAL);
            }
        }
        break;

        default:
            // All other gtOper node kinds, leave 'assertionIndex' = NO_ASSERTION_INDEX
            break;
    }
}

/*****************************************************************************
 *
 *  Given a lclNum, a fromType and a toType, return assertion index of the assertion that
 *  claims that a variable's value is always a valid subrange of the fromType.
 *  Thus we can discard or omit a cast to fromType. Returns NO_ASSERTION_INDEX
 *  if one such assertion could not be found in "assertions."
 */

AssertionIndex Compiler::morphAssertionIsSubrange(GenTree* tree, var_types fromType, var_types toType)
{
    for (AssertionIndex index = 1; index <= optAssertionCount; index++)
    {
        AssertionDsc* curAssertion = morphGetAssertion(index);
        if ((curAssertion->assertionKind == OAK_SUBRANGE) && (curAssertion->op1.kind == O1K_LCLVAR))
        {
            if (curAssertion->op1.lcl.lclNum != tree->AsLclVarCommon()->GetLclNum())
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

//------------------------------------------------------------------------------
// morphConstantAssertionProp: Possibly substitute a constant for a local use
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
GenTree* Compiler::morphConstantAssertionProp(AssertionDsc*        curAssertion,
                                              GenTreeLclVarCommon* tree DEBUGARG(AssertionIndex index))
{
    const unsigned lclNum = tree->GetLclNum();

    assert(!lvaGetDesc(lclNum)->IsAddressExposed());

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

#ifdef DEBUG
    if (verbose)
    {
        printf("\nAssertion prop in " FMT_BB ":\n", compCurBB->bbNum);
        morphPrintAssertion(curAssertion, index);
        gtDispTree(newTree, nullptr, nullptr, true);
    }
#endif

    return newTree;
}

//------------------------------------------------------------------------------
// morphAssertionProp_LclVarTypeCheck: verify compatible types for copy prop
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
bool Compiler::morphAssertionProp_LclVarTypeCheck(GenTree* tree, LclVarDsc* lclVarDsc, LclVarDsc* copyVarDsc)
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
// morphCopyAssertionProp: copy prop use of one local with another
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
GenTree* Compiler::morphCopyAssertionProp(AssertionDsc*        curAssertion,
                                          GenTreeLclVarCommon* tree DEBUGARG(AssertionIndex index))
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

    // Extract the matching lclNum.
    const unsigned copyLclNum = (op1.lcl.lclNum == lclNum) ? op2.lcl.lclNum : op1.lcl.lclNum;

    LclVarDsc* const copyVarDsc = lvaGetDesc(copyLclNum);
    LclVarDsc* const lclVarDsc  = lvaGetDesc(lclNum);

    assert(!copyVarDsc->IsAddressExposed());
    assert(!lclVarDsc->IsAddressExposed());

    // Make sure the types are compatible.
    if (!morphAssertionProp_LclVarTypeCheck(tree, lclVarDsc, copyVarDsc))
    {
        return nullptr;
    }

    // Make sure we can perform this copy prop.
    if (optCopyProp_LclVarScore(lclVarDsc, copyVarDsc, curAssertion->op1.lcl.lclNum == lclNum) <= 0)
    {
        return nullptr;
    }

    tree->SetLclNum(copyLclNum);

#ifdef DEBUG
    if (verbose)
    {
        printf("\nAssertion prop in " FMT_BB ":\n", compCurBB->bbNum);
        morphPrintAssertion(curAssertion, index);
        gtDispTree(tree, nullptr, nullptr, true);
    }
#endif

    return tree;
}

//------------------------------------------------------------------------
// morphAssertionProp_LclVar: try and optimize a local var use via assertions
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
GenTree* Compiler::morphAssertionProp_LclVar(GenTreeLclVar* tree)
{
    assert(tree->OperIs(GT_LCL_VAR));

    // If we have a var definition then bail or
    // If this is the address of the var then it will have the GTF_DONT_CSE
    // flag set and we don't want to to assertion prop on it.
    if (tree->gtFlags & (GTF_VAR_DEF | GTF_DONT_CSE))
    {
        return nullptr;
    }

    for (AssertionIndex assertionIndex = 1; assertionIndex <= optAssertionCount; ++assertionIndex)
    {
        // See if the variable is equal to a constant or another variable.
        AssertionDsc* curAssertion = morphGetAssertion(assertionIndex);
        if (curAssertion->assertionKind != OAK_EQUAL || curAssertion->op1.kind != O1K_LCLVAR)
        {
            continue;
        }

        // Copy prop.
        if (curAssertion->op2.kind == O2K_LCLVAR_COPY)
        {
            // Perform copy assertion prop.
            GenTree* newTree = morphCopyAssertionProp(curAssertion, tree DEBUGARG(assertionIndex));
            if (newTree != nullptr)
            {
                return newTree;
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
                return morphConstantAssertionProp(curAssertion, tree DEBUGARG(assertionIndex));
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
AssertionIndex Compiler::morphLocalAssertionIsEqualOrNotEqual(optOp1Kind op1Kind,
                                                              unsigned   lclNum,
                                                              optOp2Kind op2Kind,
                                                              ssize_t    cnsVal)
{
    noway_assert((op1Kind == O1K_LCLVAR) || (op1Kind == O1K_EXACT_TYPE) || (op1Kind == O1K_SUBTYPE));
    noway_assert((op2Kind == O2K_CONST_INT) || (op2Kind == O2K_IND_CNS_INT));

    for (AssertionIndex index = 1; index <= optAssertionCount; ++index)
    {
        AssertionDsc* curAssertion = morphGetAssertion(index);
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
    return NO_ASSERTION_INDEX;
}

/*****************************************************************************
 *
 *  Given a tree consisting of a RelOp and a set of available assertions
 *  we try to propagate an assertion and modify the RelOp tree if we can.
 *  We pass in the root of the tree via 'stmt', for local copy prop 'stmt' will be nullptr
 *  Returns the modified tree, or nullptr if no assertion prop took place
 */

GenTree* Compiler::morphAssertionProp_RelOp(GenTree* tree)
{
    assert(tree->OperIsCompare());

    //
    // Currently only GT_EQ or GT_NE are supported Relops for local AssertionProp
    //

    if ((tree->gtOper != GT_EQ) && (tree->gtOper != GT_NE))
    {
        return nullptr;
    }

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
    AssertionIndex index = morphLocalAssertionIsEqualOrNotEqual(op1Kind, lclNum, op2Kind, cnsVal);

    if (index == NO_ASSERTION_INDEX)
    {
        return nullptr;
    }

    assert(!lvaGetDesc(lclNum)->IsAddressExposed());

    AssertionDsc* curAssertion = morphGetAssertion(index);

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

    return op2;
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
GenTree* Compiler::morphAssertionProp_Cast(GenTree* tree)
{
    assert(tree->gtOper == GT_CAST);

    var_types fromType = tree->CastFromType();
    var_types toType   = tree->AsCast()->gtCastType;
    GenTree*  op1      = tree->AsCast()->CastOp();

    // force the fromType to unsigned if GT_UNSIGNED flag is set
    if (tree->IsUnsigned())
    {
        fromType = varTypeToUnsigned(fromType);
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

    AssertionIndex index = morphAssertionIsSubrange(lcl, fromType, toType);
    if (index != NO_ASSERTION_INDEX)
    {
        LclVarDsc* varDsc = &lvaTable[lcl->AsLclVarCommon()->GetLclNum()];
        assert(!varDsc->IsAddressExposed());

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
                    return tree;
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
        return op1;
    }
    return nullptr;
}

//------------------------------------------------------------------------
// morphAssertionProp_Ind: see if we can prove the indirection can't cause
//    and exception.
//
// Arguments:
//   tree - tree to possibly optimize
//
// Returns:
//   The modified tree, or nullptr if no assertion prop took place.
//
// Notes:
//   stmt may be nullptr during local assertion prop
//
GenTree* Compiler::morphAssertionProp_Ind(GenTree* tree)
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
    AssertionIndex index = NO_ASSERTION_INDEX;
#endif
    if (morphAssertionIsNonNull(op1 DEBUGARG(&index)))
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("\nNon-null prop for index #%02u in " FMT_BB ":\n", index, compCurBB->bbNum);
            gtDispTree(tree, nullptr, nullptr, true);
        }
#endif
        tree->gtFlags &= ~GTF_EXCEPT;
        tree->gtFlags |= GTF_IND_NONFAULTING;

        // Set this flag to prevent reordering
        tree->gtFlags |= GTF_ORDER_SIDEEFF;

        return tree;
    }

    return nullptr;
}

//------------------------------------------------------------------------
// morphAssertionIsNonNull: see if we can prove a tree's value will be non-null
//   based on assertions
//
// Arguments:
//   op - tree to check
//   pIndex - [out] the assertion used in the proof
//
// Returns:
//   true if the tree's value will be non-null
//
bool Compiler::morphAssertionIsNonNull(GenTree* op DEBUGARG(AssertionIndex* pIndex))
{
    unsigned lclNum = op->AsLclVarCommon()->GetLclNum();
    // Check each assertion to find if we have a variable == or != null assertion.
    for (AssertionIndex index = 1; index <= optAssertionCount; index++)
    {
        AssertionDsc* curAssertion = morphGetAssertion(index);
        if ((curAssertion->assertionKind == OAK_NOT_EQUAL) && // kind
            (curAssertion->op1.kind == O1K_LCLVAR) &&         // op1
            (curAssertion->op2.kind == O2K_CONST_INT) &&      // op2
            (curAssertion->op1.lcl.lclNum == lclNum) && (curAssertion->op2.u1.iconVal == 0))
        {
            assert(!lvaGetDesc(lclNum)->IsAddressExposed());
            INDEBUG(*pIndex = index);
            return true;
        }
    }

    INDEBUG(*pIndex = NO_ASSERTION_INDEX);
    return false;
}

/*****************************************************************************
 *
 *  Given a tree consisting of a call and a set of available assertions, we
 *  try to propagate a non-null assertion and modify the Call tree if we can.
 *  Returns the modified tree, or nullptr if no assertion prop took place.
 *
 */
GenTree* Compiler::morphAssertionProp_Call(GenTreeCall* call)
{
    if ((call->gtFlags & GTF_CALL_NULLCHECK) == 0)
    {
        return nullptr;
    }
    GenTree* op1 = call->GetThisArg();
    noway_assert(op1 != nullptr);
    if (op1->gtOper != GT_LCL_VAR)
    {
        return nullptr;
    }

#ifdef DEBUG
    AssertionIndex index = NO_ASSERTION_INDEX;
#endif
    if (morphAssertionIsNonNull(op1 DEBUGARG(&index)))
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("\nNon-null prop for index #%02u in " FMT_BB ":\n", index, compCurBB->bbNum);
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
 *  Called when we have a successfully performed an assertion prop. We have
 *  the newTree in hand. This method will replace the existing tree in the
 *  stmt with the newTree.
 *
 */

//------------------------------------------------------------------------
// morphAssertionProp: try and optimize a tree via assertion propagation
//
// Arguments:
//   tree - tree to possibly optimize
//
// Returns:
//   The modified tree, or nullptr if no assertion prop took place.
//
GenTree* Compiler::morphAssertionProp(GenTree* tree)
{
    switch (tree->gtOper)
    {
        case GT_LCL_VAR:
            return morphAssertionProp_LclVar(tree->AsLclVar());

        case GT_OBJ:
        case GT_BLK:
        case GT_IND:
        case GT_NULLCHECK:
            return morphAssertionProp_Ind(tree);

        case GT_CAST:
            return morphAssertionProp_Cast(tree);

        case GT_CALL:
            return morphAssertionProp_Call(tree->AsCall());

        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GT:
        case GT_GE:
            return morphAssertionProp_RelOp(tree);

        default:
            return nullptr;
    }
}
