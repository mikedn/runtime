// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

struct Compiler::MorphAssertion
{
    enum class Kind : uint8_t
    {
        Invalid,
        Equal,
        NotEqual,
        Range
    };

    enum class ValueKind : uint8_t
    {
        Invalid,
        LclVar,
        IntCon,
        LngCon,
        DblCon,
        Range
    };

    struct LclVar
    {
        unsigned lclNum;
    };

    struct IntCon
    {
        ssize_t      value;
        unsigned     padding; // unused; ensures flags does not overlap lngCon
        GenTreeFlags flags;
    };

    struct LngCon
    {
        int64_t value;
    };

    struct DblCon
    {
        double value;
    };

    struct Range
    {
        ssize_t loBound;
        ssize_t hiBound;
    };

    Kind      kind;
    ValueKind valKind;
    LclVar    lcl;

    union {
        LclVar lcl;
        IntCon intCon;
        LngCon lngCon;
        DblCon dblCon;
        Range  range;
    } val;

    bool HasSameValue(MorphAssertion* that)
    {
        if (valKind != that->valKind)
        {
            return false;
        }

        switch (valKind)
        {
            case ValueKind::IntCon:
                return (val.intCon.value == that->val.intCon.value) && (val.intCon.flags == that->val.intCon.flags);
            case ValueKind::LngCon:
                return (val.lngCon.value == that->val.lngCon.value);
            case ValueKind::DblCon:
                // exact match because of positive and negative zero.
                return memcmp(&val.dblCon.value, &that->val.dblCon.value, sizeof(double)) == 0;
            case ValueKind::LclVar:
                return val.lcl.lclNum == that->val.lcl.lclNum;
            case ValueKind::Range:
                return (val.range.loBound == that->val.range.loBound) && (val.range.hiBound == that->val.range.hiBound);
            default:
                return false;
        }
    }

    bool Equals(MorphAssertion* that)
    {
        return (kind == that->kind) && (lcl.lclNum == that->lcl.lclNum) && HasSameValue(that);
    }
};

using Kind      = Compiler::MorphAssertion::Kind;
using ValueKind = Compiler::MorphAssertion::ValueKind;

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

    optLocalAssertionProp = true;
    morphAssertionTable   = new (this, CMK_AssertionProp) MorphAssertion[optMaxAssertionCount];

    if (morphAssertionDep == nullptr)
    {
        morphAssertionDep =
            new (this, CMK_AssertionProp) JitExpandArray<ASSERT_TP>(getAllocator(CMK_AssertionProp), max(1, lvaCount));
    }

    apTraits          = new (this, CMK_AssertionProp) BitVecTraits(optMaxAssertionCount, this);
    optAssertionCount = 0;
}

#if LOCAL_ASSERTION_PROP

// The following resets the value assignment table
// used only during local assertion prop
void Compiler::morphAssertionReset(unsigned limit)
{
    PREFAST_ASSUME(optAssertionCount <= optMaxAssertionCount);

    while (optAssertionCount > limit)
    {
        unsigned        index        = optAssertionCount - 1;
        MorphAssertion* curAssertion = morphGetAssertion(index);
        optAssertionCount--;
        unsigned lclNum = curAssertion->lcl.lclNum;
        BitVecOps::RemoveElemD(apTraits, GetAssertionDep(lclNum), index);

        //
        // Find the Copy assertions
        //
        if ((curAssertion->kind == Kind::Equal) && (curAssertion->valKind == ValueKind::LclVar))
        {
            //
            //  value.lcl.lclNum no longer depends upon this assertion
            //
            lclNum = curAssertion->val.lcl.lclNum;
            BitVecOps::RemoveElemD(apTraits, GetAssertionDep(lclNum), index);
        }
    }
    while (optAssertionCount < limit)
    {
        unsigned        index        = optAssertionCount++;
        MorphAssertion* curAssertion = morphGetAssertion(index);
        unsigned        lclNum       = curAssertion->lcl.lclNum;
        BitVecOps::AddElemD(apTraits, GetAssertionDep(lclNum), index);

        //
        // Check for Copy assertions
        //
        if ((curAssertion->kind == Kind::Equal) && (curAssertion->valKind == ValueKind::LclVar))
        {
            //
            //  value.lcl.lclNum now depends upon this assertion
            //
            lclNum = curAssertion->val.lcl.lclNum;
            BitVecOps::AddElemD(apTraits, GetAssertionDep(lclNum), index);
        }
    }
}

/*****************************************************************************
 *
 *  The following removes the i-th entry in the value assignment table
 *  used only during local assertion prop
 */

void Compiler::morphAssertionRemove(unsigned index)
{
    assert(index < optAssertionCount);
    PREFAST_ASSUME(optAssertionCount <= optMaxAssertionCount);

    MorphAssertion* curAssertion = morphGetAssertion(index);

    //  Two cases to consider if (index == optAssertionCount) then the last
    //  entry in the table is to be removed and that happens automatically when
    //  optAssertionCount is decremented and we can just clear the morphAssertionDep bits
    //  The other case is when index < optAssertionCount and here we overwrite the
    //  index-th entry in the table with the data found at the end of the table
    //  Since we are reordering the rable the morphAssertionDep bits need to be recreated
    //  using optAssertionReset(0) and optAssertionReset(newAssertionCount) will
    //  correctly update the morphAssertionDep bits
    //
    if (index == static_cast<unsigned>(optAssertionCount) - 1)
    {
        unsigned lclNum = curAssertion->lcl.lclNum;
        BitVecOps::RemoveElemD(apTraits, GetAssertionDep(lclNum), index);

        //
        // Check for Copy assertions
        //
        if ((curAssertion->kind == Kind::Equal) && (curAssertion->valKind == ValueKind::LclVar))
        {
            //
            //  value.lcl.lclNum no longer depends upon this assertion
            //
            lclNum = curAssertion->val.lcl.lclNum;
            BitVecOps::RemoveElemD(apTraits, GetAssertionDep(lclNum), index);
        }

        optAssertionCount--;
    }
    else
    {
        MorphAssertion* lastAssertion     = morphGetAssertion(optAssertionCount - 1);
        unsigned        newAssertionCount = optAssertionCount - 1;

        morphAssertionReset(0); // This make optAssertionCount equal 0

        memcpy(curAssertion,  // the entry to be removed
               lastAssertion, // last entry in the table
               sizeof(MorphAssertion));

        morphAssertionReset(newAssertionCount);
    }
}

unsigned Compiler::morphAssertionTableSize(unsigned count)
{
    assert(count <= optMaxAssertionCount);
    return count * sizeof(MorphAssertion);
}

void Compiler::morphAssertionCopyTable(MorphAssertion* toTable, MorphAssertion* fromTable, unsigned count)
{
    assert(count <= optMaxAssertionCount);
    memcpy(toTable, fromTable, count * sizeof(MorphAssertion));
}

void Compiler::morphAssertionMerge(unsigned        elseAssertionCount,
                                   MorphAssertion* elseAssertionTab DEBUGARG(GenTreeQmark* qmark))
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
        (memcmp(elseAssertionTab, morphAssertionTable, optAssertionCount * sizeof(MorphAssertion)) == 0))
    {
        return;
    }

    for (unsigned index = 0; index < optAssertionCount;)
    {
        MorphAssertion* thenAssertion = morphGetAssertion(index);
        MorphAssertion* elseAssertion = nullptr;

        for (unsigned j = 0; j < elseAssertionCount; j++)
        {
            MorphAssertion* assertion = &elseAssertionTab[j];

            if ((assertion->kind == thenAssertion->kind) && (assertion->lcl.lclNum == thenAssertion->lcl.lclNum))
            {
                elseAssertion = assertion;
                break;
            }
        }

        if ((elseAssertion != nullptr) && elseAssertion->HasSameValue(thenAssertion))
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
void Compiler::morphPrintAssertion(MorphAssertion* curAssertion)
{
    if (curAssertion->valKind == ValueKind::LclVar)
    {
        printf("Copy     ");
    }
    else if ((curAssertion->valKind == ValueKind::IntCon) || (curAssertion->valKind == ValueKind::LngCon) ||
             (curAssertion->valKind == ValueKind::DblCon))
    {
        printf("Constant ");
    }
    else if (curAssertion->valKind == ValueKind::Range)
    {
        printf("Subrange ");
    }
    else
    {
        printf("?assertion classification? ");
    }

    printf("Assertion: V%02u", curAssertion->lcl.lclNum);

    if (curAssertion->kind == Kind::Range)
    {
        printf(" in ");
    }
    else if (curAssertion->kind == Kind::Equal)
    {
        printf(" == ");
    }
    else if (curAssertion->kind == Kind::NotEqual)
    {
        printf(" != ");
    }
    else
    {
        printf(" ?assertionKind? ");
    }

    switch (curAssertion->valKind)
    {
        case ValueKind::LclVar:
            printf("V%02u", curAssertion->val.lcl.lclNum);
            break;

        case ValueKind::IntCon:
        {
            unsigned lclNum = curAssertion->lcl.lclNum;
            assert(lclNum < lvaCount);
            LclVarDsc* varDsc  = lvaTable + lclNum;
            var_types  op1Type = varDsc->lvType;

            if (op1Type == TYP_REF)
            {
                assert(curAssertion->val.intCon.value == 0);
                printf("null");
            }
            else
            {
                if ((curAssertion->val.intCon.flags & GTF_ICON_HDL_MASK) != 0)
                {
                    printf("[%08p]", dspPtr(curAssertion->val.intCon.value));
                }
                else
                {
                    printf("%d", curAssertion->val.intCon.value);
                }
            }
        }
        break;

        case ValueKind::LngCon:
            printf("0x%016llx", curAssertion->val.lngCon.value);
            break;

        case ValueKind::DblCon:
            if (*((__int64*)&curAssertion->val.dblCon.value) == (__int64)I64(0x8000000000000000))
            {
                printf("-0.00000");
            }
            else
            {
                printf("%#lg", curAssertion->val.dblCon.value);
            }
            break;

        case ValueKind::Range:
            printf("[%u..%u]", curAssertion->val.range.loBound, curAssertion->val.range.hiBound);
            break;

        default:
            printf("?valKind?");
            break;
    }

    if ((curAssertion >= morphAssertionTable) && (curAssertion < morphAssertionTable + optAssertionCount))
    {
        printf(", index = %u", curAssertion - morphAssertionTable);
    }

    printf("\n");
}

#endif // DEBUG

Compiler::MorphAssertion* Compiler::morphGetAssertion(unsigned index)
{
    assert(index < optAssertionCount);
    MorphAssertion* assertion = &morphAssertionTable[index];
    INDEBUG(morphDebugCheckAssertion(assertion));
    return assertion;
}

void Compiler::morphCreateNonNullAssertion(GenTree* op1)
{
    assert(op1 != nullptr);

    MorphAssertion assertion;
    memset(&assertion, 0, sizeof(MorphAssertion));
    assert(assertion.kind == Kind::Invalid);

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

            assertion.lcl.lclNum = lclNum;
        }

        assertion.kind             = Kind::NotEqual;
        assertion.valKind          = ValueKind::IntCon;
        assertion.val.intCon.value = 0;
        assertion.val.intCon.flags = GTF_EMPTY;
#ifdef TARGET_64BIT
        assertion.val.intCon.flags |= GTF_ASSERTION_PROP_LONG; // Signify that this is really TYP_LONG
#endif
    }

DONE_ASSERTION:
    if (assertion.kind != Kind::Invalid)
    {
        noway_assert(assertion.valKind != ValueKind::Invalid);
        morphAddAssertion(&assertion);
    }
}

void Compiler::morphCreateEqualAssertion(GenTreeLclVar* op1, GenTree* op2)
{
    assert(op1 != nullptr);
    assert(op2 != nullptr);

    MorphAssertion assertion;
    memset(&assertion, 0, sizeof(MorphAssertion));
    assert(assertion.kind == Kind::Invalid);

    var_types toType;
    unsigned  lclNum = op1->GetLclNum();
    noway_assert(lclNum < lvaCount);
    LclVarDsc* lclVar = &lvaTable[lclNum];

    //  If the local variable has its address exposed then bail
    if (lclVar->lvAddrExposed)
    {
        goto DONE_ASSERTION; // Don't make an assertion
    }

    op2 = op2->SkipComma();

    assertion.lcl.lclNum = lclNum;

    switch (op2->gtOper)
    {
        ValueKind op2Kind;
        //
        //  No Assertion
        //
        default:
            goto DONE_ASSERTION; // Don't make an assertion

        //
        //  Constant Assertions
        //
        case GT_CNS_INT:
            op2Kind = ValueKind::IntCon;
            goto CNS_COMMON;

        case GT_CNS_LNG:
            op2Kind = ValueKind::LngCon;
            goto CNS_COMMON;

        case GT_CNS_DBL:
            op2Kind = ValueKind::DblCon;
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

            assertion.valKind = op2Kind;

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

                assertion.val.intCon.value = value;
                assertion.val.intCon.flags = op2->GetIconHandleFlag();
#ifdef TARGET_64BIT
                if (op2->TypeGet() == TYP_LONG || op2->TypeGet() == TYP_BYREF)
                {
                    assertion.val.intCon.flags |= GTF_ASSERTION_PROP_LONG;
                }
#endif // TARGET_64BIT
            }
            else if (op2->gtOper == GT_CNS_LNG)
            {
                assertion.val.lngCon.value = op2->AsLngCon()->gtLconVal;
            }
            else
            {
                noway_assert(op2->gtOper == GT_CNS_DBL);
                /* If we have an NaN value then don't record it */
                if (_isnan(op2->AsDblCon()->gtDconVal))
                {
                    goto DONE_ASSERTION; // Don't make an assertion
                }
                assertion.val.dblCon.value = op2->AsDblCon()->gtDconVal;
            }

            //
            // Ok everything has been set and the assertion looks good
            //
            assertion.kind = Kind::Equal;
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

            assertion.valKind        = ValueKind::LclVar;
            assertion.val.lcl.lclNum = lclNum2;

            //
            // Ok everything has been set and the assertion looks good
            //
            assertion.kind = Kind::Equal;
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
                    assertion.val.range.loBound = AssertionDsc::GetLowerBoundForIntegralType(toType);
                    assertion.val.range.hiBound = AssertionDsc::GetUpperBoundForIntegralType(toType);
                    break;

                default:
                    goto DONE_ASSERTION; // Don't make an assertion
            }
            assertion.valKind = ValueKind::Range;
            assertion.kind    = Kind::Range;
        }
        break;
    }

DONE_ASSERTION:
    if (assertion.kind != Kind::Invalid)
    {
        noway_assert(assertion.valKind != ValueKind::Invalid);
        morphAddAssertion(&assertion);
    }
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
void Compiler::morphAddAssertion(MorphAssertion* newAssertion)
{
    noway_assert(newAssertion->kind != Kind::Invalid);

    // Check if exists already, so we can skip adding new one. Search backwards.
    for (unsigned index = optAssertionCount - 1; index != UINT32_MAX; index--)
    {
        MorphAssertion* curAssertion = morphGetAssertion(index);
        if (curAssertion->Equals(newAssertion))
        {
            return;
        }
    }

    // Check if we are within max count.
    if (optAssertionCount >= optMaxAssertionCount)
    {
        return;
    }

    morphAssertionTable[optAssertionCount] = *newAssertion;
    newAssertion                           = &morphAssertionTable[optAssertionCount];
    optAssertionCount++;

#ifdef DEBUG
    if (verbose)
    {
        printf("GenTreeNode creates assertion:\n");
        gtDispTree(optAssertionPropCurrentTree, nullptr, nullptr, true);
        morphPrintAssertion(newAssertion);
    }
#endif // DEBUG

    // Mark the variables this index depends on
    unsigned lclNum = newAssertion->lcl.lclNum;
    BitVecOps::AddElemD(apTraits, GetAssertionDep(lclNum), optAssertionCount - 1);
    if (newAssertion->valKind == ValueKind::LclVar)
    {
        lclNum = newAssertion->val.lcl.lclNum;
        BitVecOps::AddElemD(apTraits, GetAssertionDep(lclNum), optAssertionCount - 1);
    }

    INDEBUG(morphDebugCheckAssertion(&morphAssertionTable[optAssertionCount - 1]));
}

#ifdef DEBUG
void Compiler::morphDebugCheckAssertion(MorphAssertion* assertion)
{
    assert(assertion->lcl.lclNum < lvaCount);

    switch (assertion->valKind)
    {
        case ValueKind::IntCon:
        {
// The only flags that can be set are those in the GTF_ICON_HDL_MASK, or GTF_ASSERTION_PROP_LONG, which is
// used to indicate a long constant.
#ifdef TARGET_64BIT
            assert((assertion->val.intCon.flags & ~(GTF_ICON_HDL_MASK | GTF_ASSERTION_PROP_LONG)) == 0);
#else
            assert((assertion->val.intCon.flags & ~GTF_ICON_HDL_MASK) == 0);
#endif
            assert((lvaTable[assertion->lcl.lclNum].lvType != TYP_REF) || (assertion->val.intCon.value == 0) ||
                   doesMethodHaveFrozenString());
        }
        break;

        case ValueKind::LngCon:
        {
            // All handles should be represented by ValueKind::IntCon,
            // so no handle bits should be set here.
            assert((assertion->val.intCon.flags & GTF_ICON_HDL_MASK) == 0);
        }
        break;

        default:
            // for all other 'assertion->valKind' values we don't check anything
            break;
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
    INDEBUG(optAssertionPropCurrentTree = tree);

    switch (tree->GetOper())
    {
        case GT_ASG:
            if (tree->AsOp()->GetOp(0)->OperIs(GT_LCL_VAR))
            {
                morphCreateEqualAssertion(tree->AsOp()->GetOp(0)->AsLclVar(), tree->AsOp()->GetOp(1));
            }
            break;

        case GT_BLK:
        case GT_OBJ:
            assert(tree->AsBlk()->GetLayout()->GetSize() != 0);
            FALLTHROUGH;
        case GT_IND:
        case GT_NULLCHECK:
            morphCreateNonNullAssertion(tree->AsIndir()->GetAddr());
            break;
        case GT_ARR_LENGTH:
            morphCreateNonNullAssertion(tree->AsArrLen()->GetArray());
            break;
        case GT_ARR_ELEM:
            morphCreateNonNullAssertion(tree->AsArrElem()->GetArray());
            break;

        case GT_CALL:
        {
            // A virtual call can create a non-null assertion. We transform some virtual calls into non-virtual calls
            // with a GTF_CALL_NULLCHECK flag set.
            // Ignore tail calls because they have 'this` pointer in the regular arg list and an implicit null check.
            GenTreeCall* const call = tree->AsCall();
            if (call->NeedsNullCheck() || (call->IsVirtual() && !call->IsTailCall()))
            {
                morphCreateNonNullAssertion(call->GetThisArg());
            }
        }
        break;

        default:
            break;
    }
}

/*****************************************************************************
 *
 *  Given a lclNum, a fromType and a toType, return assertion index of the assertion that
 *  claims that a variable's value is always a valid subrange of the fromType.
 *  Thus we can discard or omit a cast to fromType. Returns nullptr
 *  if one such assertion could not be found in "assertions."
 */

Compiler::MorphAssertion* Compiler::morphAssertionIsSubrange(GenTree* tree, var_types fromType, var_types toType)
{
    for (unsigned index = 0; index < optAssertionCount; index++)
    {
        MorphAssertion* curAssertion = morphGetAssertion(index);
        if (curAssertion->kind == Kind::Range)
        {
            if (curAssertion->lcl.lclNum != tree->AsLclVarCommon()->GetLclNum())
            {
                continue;
            }

            // If we have an unsigned fromType, then the loBound can't be negative
            //
            if (varTypeIsUnsigned(fromType))
            {
                if (curAssertion->val.range.loBound < 0)
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
                    if ((curAssertion->val.range.loBound < AssertionDsc::GetLowerBoundForIntegralType(toType)) ||
                        (curAssertion->val.range.hiBound > AssertionDsc::GetUpperBoundForIntegralType(toType)))
                    {
                        continue;
                    }
                    break;

                case TYP_UINT:
                    if (curAssertion->val.range.loBound < AssertionDsc::GetLowerBoundForIntegralType(toType))
                    {
                        continue;
                    }
                    break;

                case TYP_INT:
                    break;

                default:
                    continue;
            }

            return curAssertion;
        }
    }

    return nullptr;
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
GenTree* Compiler::morphConstantAssertionProp(MorphAssertion* curAssertion, GenTreeLclVarCommon* tree)
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
    switch (curAssertion->valKind)
    {
        case ValueKind::DblCon:
            // There could be a positive zero and a negative zero, so don't propagate zeroes.
            if (curAssertion->val.dblCon.value == 0.0)
            {
                return nullptr;
            }
            newTree->ChangeOperConst(GT_CNS_DBL);
            newTree->AsDblCon()->gtDconVal = curAssertion->val.dblCon.value;
            break;

        case ValueKind::LngCon:

            if (newTree->gtType == TYP_LONG)
            {
                newTree->ChangeOperConst(GT_CNS_NATIVELONG);
                newTree->AsIntConCommon()->SetLngValue(curAssertion->val.lngCon.value);
            }
            else
            {
                newTree->ChangeOperConst(GT_CNS_INT);
                newTree->AsIntCon()->gtIconVal = (int)curAssertion->val.lngCon.value;
                newTree->gtType                = TYP_INT;
            }
            break;

        case ValueKind::IntCon:

            // Don't propagate handles if we need to report relocs.
            if (opts.compReloc && ((curAssertion->val.intCon.flags & GTF_ICON_HDL_MASK) != 0))
            {
                return nullptr;
            }

            if (curAssertion->val.intCon.flags & GTF_ICON_HDL_MASK)
            {
                // Here we have to allocate a new 'large' node to replace the old one
                // TODO-MIKE-Cleanup: Huh, what large node?!?
                newTree = gtNewIconHandleNode(curAssertion->val.intCon.value,
                                              curAssertion->val.intCon.flags & GTF_ICON_HDL_MASK);
            }
            else
            {
                // If we have done constant propagation of a struct type, it is only valid for zero-init,
                // and we have to ensure that we have the right zero for the type.
                assert(!varTypeIsStruct(tree->GetType()) || curAssertion->val.intCon.value == 0);

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
                    newTree->AsIntCon()->gtIconVal = curAssertion->val.intCon.value;
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
                    (var_types)((curAssertion->val.intCon.flags & GTF_ASSERTION_PROP_LONG) ? TYP_LONG : TYP_INT);
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
        printf("\nAssertion prop:\n");
        morphPrintAssertion(curAssertion);
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
GenTree* Compiler::morphCopyAssertionProp(MorphAssertion* curAssertion, GenTreeLclVarCommon* tree)
{
    const auto& op1 = curAssertion->lcl;
    const auto& op2 = curAssertion->val;

    noway_assert(op1.lclNum != op2.lcl.lclNum);

    const unsigned lclNum = tree->GetLclNum();

    // Make sure one of the lclNum of the assertion matches with that of the tree.
    if (op1.lclNum != lclNum && op2.lcl.lclNum != lclNum)
    {
        return nullptr;
    }

    // Extract the matching lclNum.
    const unsigned copyLclNum = (op1.lclNum == lclNum) ? op2.lcl.lclNum : op1.lclNum;

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
    if (optCopyProp_LclVarScore(lclVarDsc, copyVarDsc, curAssertion->lcl.lclNum == lclNum) <= 0)
    {
        return nullptr;
    }

    tree->SetLclNum(copyLclNum);

#ifdef DEBUG
    if (verbose)
    {
        printf("\nAssertion prop:\n");
        morphPrintAssertion(curAssertion);
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

    for (unsigned assertionIndex = 0; assertionIndex < optAssertionCount; ++assertionIndex)
    {
        // See if the variable is equal to a constant or another variable.
        MorphAssertion* curAssertion = morphGetAssertion(assertionIndex);
        if (curAssertion->kind != Kind::Equal)
        {
            continue;
        }

        // Copy prop.
        if (curAssertion->valKind == ValueKind::LclVar)
        {
            // Perform copy assertion prop.
            GenTree* newTree = morphCopyAssertionProp(curAssertion, tree);
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
        if (curAssertion->lcl.lclNum == lclNum)
        {
            LclVarDsc* const lclDsc = lvaGetDesc(lclNum);
            // Verify types match
            if (tree->TypeGet() == lclDsc->lvType)
            {
                return morphConstantAssertionProp(curAssertion, tree);
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
Compiler::MorphAssertion* Compiler::morphAssertionIsEqualOrNotEqual(unsigned lclNum, ssize_t cnsVal)
{
    for (unsigned index = 0; index < optAssertionCount; ++index)
    {
        MorphAssertion* curAssertion = morphGetAssertion(index);
        if ((curAssertion->kind != Kind::Equal) && (curAssertion->kind != Kind::NotEqual))
        {
            continue;
        }

        if ((curAssertion->lcl.lclNum == lclNum) && (curAssertion->valKind == ValueKind::IntCon))
        {
            bool constantIsEqual  = (curAssertion->val.intCon.value == cnsVal);
            bool assertionIsEqual = (curAssertion->kind == Kind::Equal);

            if (constantIsEqual || assertionIsEqual)
            {
                return curAssertion;
            }
        }
    }

    return nullptr;
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

    ssize_t   cnsVal  = op2->AsIntCon()->gtIconVal;
    var_types cmpType = op1->TypeGet();

    // Don't try to fold/optimize Floating Compares; there are multiple zero values.
    if (varTypeIsFloating(cmpType))
    {
        return nullptr;
    }

    // Find an equal or not equal assertion about op1 var.
    unsigned lclNum = op1->AsLclVarCommon()->GetLclNum();
    noway_assert(lclNum < lvaCount);
    MorphAssertion* curAssertion = morphAssertionIsEqualOrNotEqual(lclNum, cnsVal);

    if (curAssertion == nullptr)
    {
        return nullptr;
    }

    assert(!lvaGetDesc(lclNum)->IsAddressExposed());

    bool assertionKindIsEqual = (curAssertion->kind == Kind::Equal);
    bool constantIsEqual      = false;

    if (genTypeSize(cmpType) == TARGET_POINTER_SIZE)
    {
        constantIsEqual = (curAssertion->val.intCon.value == cnsVal);
    }
#ifdef TARGET_64BIT
    else if (genTypeSize(cmpType) == sizeof(INT32))
    {
        // Compare the low 32-bits only
        constantIsEqual = (((INT32)curAssertion->val.intCon.value) == ((INT32)cnsVal));
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
        printf("\nAssertion prop for index #%02u:\n", curAssertion - morphAssertionTable);
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

    if (MorphAssertion* assertion = morphAssertionIsSubrange(lcl, fromType, toType))
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
                        printf("\nSubrange prop for index #%02u:\n", assertion - morphAssertionTable);
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
            printf("\nSubrange prop for index #%02u:\n", assertion - morphAssertionTable);
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

    if (MorphAssertion* assertion = morphAssertionIsNonNull(op1))
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("\nNon-null prop for index #%02u:\n", assertion - morphAssertionTable);
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

Compiler::MorphAssertion* Compiler::morphAssertionIsNonNull(GenTree* op)
{
    unsigned lclNum = op->AsLclVarCommon()->GetLclNum();
    // Check each assertion to find if we have a variable == or != null assertion.
    for (unsigned index = 0; index < optAssertionCount; index++)
    {
        MorphAssertion* curAssertion = morphGetAssertion(index);
        if ((curAssertion->kind == Kind::NotEqual) && // kind

            (curAssertion->valKind == ValueKind::IntCon) && // op2
            (curAssertion->lcl.lclNum == lclNum) && (curAssertion->val.intCon.value == 0))
        {
            assert(!lvaGetDesc(lclNum)->IsAddressExposed());
            return curAssertion;
        }
    }

    return nullptr;
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

    if (MorphAssertion* assertion = morphAssertionIsNonNull(op1))
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("\nNon-null prop for index #%02u:\n", assertion - morphAssertionTable);
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

#if LOCAL_ASSERTION_PROP
//------------------------------------------------------------------------
// morphAssertionKillSingle: Kill all assertions specific to lclNum
//
// Arguments:
//    lclNum - The varNum of the lclVar for which we're killing assertions.
//    tree   - (DEBUG only) the tree responsible for killing its assertions.
//
void Compiler::morphAssertionKillSingle(unsigned lclNum DEBUGARG(GenTree* tree))
{
    /* All dependent assertions are killed here */

    ASSERT_TP killed = BitVecOps::MakeCopy(apTraits, GetAssertionDep(lclNum));

    if (killed)
    {
        for (unsigned count = optAssertionCount; killed && (count > 0); count--)
        {
            if (BitVecOps::IsMember(apTraits, killed, count - 1))
            {
#ifdef DEBUG
                MorphAssertion* curAssertion = morphGetAssertion(count - 1);
                noway_assert((curAssertion->lcl.lclNum == lclNum) || ((curAssertion->valKind == ValueKind::LclVar) &&
                                                                      (curAssertion->val.lcl.lclNum == lclNum)));
                if (verbose)
                {
                    printf("\nThe assignment ");
                    printTreeID(tree);
                    printf(" using V%02u removes: ", curAssertion->lcl.lclNum);
                    morphPrintAssertion(curAssertion);
                }
#endif
                // Remove this bit from the killed mask
                BitVecOps::RemoveElemD(apTraits, killed, count - 1);

                morphAssertionRemove(count - 1);
            }
        }

        // killed mask should now be zero
        noway_assert(BitVecOps::IsEmpty(apTraits, killed));
    }
}
//------------------------------------------------------------------------
// morphAssertionKill: Kill all dependent assertions with regard to lclNum.
//
// Arguments:
//    lclNum - The varNum of the lclVar for which we're killing assertions.
//    tree   - (DEBUG only) the tree responsible for killing its assertions.
//
// Notes:
//    For structs and struct fields, it will invalidate the children and parent
//    respectively.
//    Calls morphAssertionKillSingle to kill the assertions for a single lclVar.
//
void Compiler::morphAssertionKill(unsigned lclNum DEBUGARG(GenTree* tree))
{
    LclVarDsc* varDsc = lvaGetDesc(lclNum);

    if (varDsc->lvPromoted)
    {
        noway_assert(varTypeIsStruct(varDsc));

        // Kill the field locals.
        for (unsigned i = varDsc->lvFieldLclStart; i < varDsc->lvFieldLclStart + varDsc->lvFieldCnt; ++i)
        {
            morphAssertionKillSingle(i DEBUGARG(tree));
        }

        // Kill the struct local itself.
        morphAssertionKillSingle(lclNum DEBUGARG(tree));
    }
    else if (varDsc->lvIsStructField)
    {
        // Kill the field local.
        morphAssertionKillSingle(lclNum DEBUGARG(tree));

        // Kill the parent struct.
        morphAssertionKillSingle(varDsc->lvParentLcl DEBUGARG(tree));
    }
    else
    {
        morphAssertionKillSingle(lclNum DEBUGARG(tree));
    }
}
#endif // LOCAL_ASSERTION_PROP
