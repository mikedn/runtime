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
        NotNull
    };

    enum class ValueKind : uint8_t
    {
        Invalid,
        LclVar,
        IntCon,
#ifndef TARGET_64BIT
        LngCon,
#endif
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
        GenTreeFlags flags;
    };

#ifndef TARGET_64BIT
    struct LngCon
    {
        int64_t value;
    };
#endif

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
#ifndef TARGET_64BIT
        LngCon lngCon;
#endif
        DblCon dblCon;
        Range  range;
    } val;

    bool HasSameValue(const MorphAssertion& that) const
    {
        if (valKind != that.valKind)
        {
            return false;
        }

        const auto& x = val;
        const auto& y = that.val;

        switch (valKind)
        {
            case ValueKind::IntCon:
                return (x.intCon.value == y.intCon.value) && (x.intCon.flags == y.intCon.flags);
#ifndef TARGET_64BIT
            case ValueKind::LngCon:
                return (x.lngCon.value == y.lngCon.value);
#endif
            case ValueKind::DblCon:
                return jitstd::bit_cast<uint64_t>(x.dblCon.value) == jitstd::bit_cast<uint64_t>(y.dblCon.value);
            case ValueKind::LclVar:
                return x.lcl.lclNum == y.lcl.lclNum;
            case ValueKind::Range:
                return (x.range.loBound == y.range.loBound) && (y.range.hiBound == y.range.hiBound);
            default:
                return false;
        }
    }
};

using MorphAssertion = Compiler::MorphAssertion;
using Kind           = MorphAssertion::Kind;
using ValueKind      = MorphAssertion::ValueKind;

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

        if ((elseAssertion != nullptr) && elseAssertion->HasSameValue(*thenAssertion))
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
void Compiler::morphPrintAssertion(MorphAssertion* assertion)
{
    printf("Assertion");

    if ((assertion >= morphAssertionTable) && (assertion < morphAssertionTable + optAssertionCount))
    {
        printf(" #%02u", assertion - morphAssertionTable);
    }

    printf(": V%02u", assertion->lcl.lclNum);

    const char* op;

    switch (assertion->kind)
    {
        case Kind::Equal:
            op = assertion->valKind == ValueKind::Range ? "in" : "==";
            break;
        case Kind::NotNull:
            op = "!=";
            break;
        default:
            op = "???";
            break;
    }

    printf(" %s ", op);

    const auto& val = assertion->val;

    switch (assertion->valKind)
    {
        case ValueKind::LclVar:
            printf("V%02u", val.lcl.lclNum);
            break;
        case ValueKind::IntCon:
            if ((val.intCon.flags & GTF_ICON_HDL_MASK) != 0)
            {
                printf("%08p (%s)", dspPtr(val.intCon.value), dmpGetHandleKindName(val.intCon.flags));
            }
            else
            {
                printf("%Id", val.intCon.value);
            }
            break;
#ifndef TARGET_64BIT
        case ValueKind::LngCon:
            printf("0x%016llx", val.lngCon.value);
            break;
#endif
        case ValueKind::DblCon:
            printf("%#.17g", val.dblCon.value);
            break;
        case ValueKind::Range:
            printf("[%Id..%Id]", val.range.loBound, val.range.hiBound);
            break;
        default:
            printf("???");
            break;
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

        unsigned lclNum = op1->AsLclVar()->GetLclNum();
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

        assertion.kind             = Kind::NotNull;
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

    unsigned   lclNum = op1->GetLclNum();
    LclVarDsc* lclVar = lvaGetDesc(lclNum);

    //  If the local variable has its address exposed then bail
    if (lclVar->lvAddrExposed)
    {
        goto DONE_ASSERTION; // Don't make an assertion
    }

    op2 = op2->SkipComma();

    assertion.lcl.lclNum = lclNum;

    switch (op2->gtOper)
    {
        var_types toType;

        default:
            goto DONE_ASSERTION; // Don't make an assertion

        case GT_CNS_INT:
            if (lclVar->TypeIs(TYP_LONG) && !op2->TypeIs(TYP_LONG))
            {
                goto DONE_ASSERTION; // Don't make an assertion
            }

#ifdef TARGET_ARM
            if (!codeGen->validImmForMov(op2->AsIntCon()->GetInt32Value()))
            {
                goto DONE_ASSERTION; // Don't make an assertion
            }
#endif

            {
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
            }

            assertion.kind             = Kind::Equal;
            assertion.valKind          = ValueKind::IntCon;
            assertion.val.intCon.flags = op2->GetIconHandleFlag();
#ifdef TARGET_64BIT
            if (op2->TypeIs(TYP_LONG, TYP_BYREF))
            {
                assertion.val.intCon.flags |= GTF_ASSERTION_PROP_LONG;
            }
#endif
            break;

#ifndef TARGET_64BIT
        case GT_CNS_LNG:
            assert(lclVar->TypeIs(TYP_LONG) && op1->TypeIs(TYP_LONG) && op2->TypeIs(TYP_LONG));

            assertion.kind             = Kind::Equal;
            assertion.valKind          = ValueKind::LngCon;
            assertion.val.lngCon.value = op2->AsLngCon()->GetValue();
            break;
#endif

        case GT_CNS_DBL:
            assert((lclVar->GetType() == op1->GetType()) && (lclVar->GetType() == op2->GetType()));

            if (_isnan(op2->AsDblCon()->GetValue()))
            {
                goto DONE_ASSERTION; // Don't make an assertion
            }

            assertion.kind             = Kind::Equal;
            assertion.valKind          = ValueKind::DblCon;
            assertion.val.dblCon.value = op2->AsDblCon()->GetValue();
            break;

        //
        //  Copy Assertions
        //
        case GT_LCL_VAR:
        {
            unsigned lclNum2 = op2->AsLclVar()->GetLclNum();
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

        case GT_EQ:
        case GT_NE:
        case GT_LT:
        case GT_LE:
        case GT_GT:
        case GT_GE:
            assertion.kind              = Kind::Equal;
            assertion.valKind           = ValueKind::Range;
            assertion.val.range.loBound = 0;
            assertion.val.range.hiBound = 1;
            break;

        case GT_LCL_FLD:
        case GT_IND:
            toType = op2->GetType();

            if (varTypeIsSmall(toType))
            {
                assertion.val.range.loBound = AssertionDsc::GetLowerBoundForIntegralType(toType);
                assertion.val.range.hiBound = AssertionDsc::GetUpperBoundForIntegralType(toType);
            }
#ifdef TARGET_64BIT
            // TODO-MIKE-CQ: This is useless nonsense, of course an INT load has range
            // INT32_MIN..INT32_MAX. Problem is, these assertions still take space in
            // the assertion table and can prevent more assertions from being created
            // so removing this causes diffs. Remove it when it's all done.
            // Also note that loads should not have UINT type but due to bogus JIT code
            // that does happen sometimes.
            else if ((toType == TYP_INT) || (toType == TYP_UINT))
            {
                assertion.val.range.loBound = INT32_MIN;
                assertion.val.range.hiBound = INT32_MAX;
            }
#endif
            else
            {
                return;
            }

            assertion.valKind = ValueKind::Range;
            assertion.kind    = Kind::Equal;
            break;

        case GT_CAST:
            if (lclVar->IsPromotedField() && lclVar->lvNormalizeOnLoad())
            {
                // TODO-MIKE-Review: It's not clear why a range assertion is not generated in
                // this case. In typical idiotic fashion old comment stated what the code is
                // doing instead of why it is doing it.
                return;
            }

            toType = op2->AsCast()->GetCastType();

            if (varTypeIsSmall(toType))
            {
                assertion.val.range.loBound = AssertionDsc::GetLowerBoundForIntegralType(toType);
                assertion.val.range.hiBound = AssertionDsc::GetUpperBoundForIntegralType(toType);
            }
#ifdef TARGET_64BIT
            else if ((toType == TYP_INT) || (toType == TYP_UINT))
            {
                // TODO-MIKE-CQ: Like in the load case, this is pretty much nonsense. There is
                // a difference however, an overflow checking cast to UINT should produce a
                // 0..INT_32MAX/UINT32_MAX range depending on the source value being INT/LONG.
                // No idea why this always produces an INT32_MIN..INT32_MAX range.
                assertion.val.range.loBound = INT32_MIN;
                assertion.val.range.hiBound = INT32_MAX;
            }
#endif
            else
            {
                return;
            }

            assertion.valKind = ValueKind::Range;
            assertion.kind    = Kind::Equal;
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

        if (curAssertion->lcl.lclNum != newAssertion->lcl.lclNum)
        {
            continue;
        }

        if ((curAssertion->kind == Kind::Equal) && (newAssertion->kind == Kind::NotNull))
        {
            // We can add a "not null" assertion even if we already have an "equal" one.
            // It's normally pointless (if the local value is 42 then obviously it's not
            // null) but existing code doesn't infer "not null" from "equal" assertions.
            continue;
        }

        if ((curAssertion->kind == Kind::NotNull) && (newAssertion->kind == Kind::NotNull))
        {
            // We already have a "not null" assertion for this local, don't add another one.
            return;
        }

        // We're trying to add an "equal" assertion when we already have another assertion
        // about this local. This should not happen, existing assertions should have been
        // killed by morph before generating new ones. Just drop to minopts, morphing code
        // is likely broken.
        unreached();
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

MorphAssertion* Compiler::morphAssertionFindRange(unsigned lclNum)
{
    for (unsigned index = 0; index < optAssertionCount; index++)
    {
        MorphAssertion* assertion = morphGetAssertion(index);

        if ((assertion->kind == Kind::Equal) && (assertion->lcl.lclNum == lclNum))
        {
            return assertion->valKind == ValueKind::Range ? assertion : nullptr;
        }
    }

    return nullptr;
}

MorphAssertion* Compiler::morphAssertionIsTypeRange(GenTreeLclVar* lclVar, var_types type)
{
    // For now morph only needs this to eliminate some small int casts.
    // TODO-MIKE-Review: Check why BOOL is ignored, it behaves like UBYTE when used with casts.
    if (!varTypeIsSmallInt(type))
    {
        return nullptr;
    }

    MorphAssertion* assertion = morphAssertionFindRange(lclVar->GetLclNum());

    if (assertion == nullptr)
    {
        return nullptr;
    }

    if ((assertion->val.range.loBound < AssertionDsc::GetLowerBoundForIntegralType(type)) ||
        (assertion->val.range.hiBound > AssertionDsc::GetUpperBoundForIntegralType(type)))
    {
        return nullptr;
    }

    return assertion;
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
GenTree* Compiler::morphConstantAssertionProp(MorphAssertion* curAssertion, GenTreeLclVar* tree)
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

#ifndef TARGET_64BIT
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
#endif

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
GenTree* Compiler::morphCopyAssertionProp(MorphAssertion* curAssertion, GenTreeLclVar* tree)
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

    // We can't use a small int promoted field if the original LCL_VAR doesn't have the same type
    // as the field. If the field ends up being P-DEP then we risk reading bits from adjacent fields.
    // TODO-MIKE-Cleanup: It's not clear what the problem is here. Such a promoted field is supposed
    // to be widened on load so any garbage bits would be discarded. This might be related to struct
    // assignment promotion where widening casts are not inserted.
    if (copyVarDsc->IsPromotedField() && varTypeIsSmall(copyVarDsc->GetType()) &&
        (copyVarDsc->GetType() != tree->GetType()))
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
    unsigned lclNum = op1->AsLclVar()->GetLclNum();
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

GenTree* Compiler::morphAssertionPropCast(GenTreeCast* cast)
{
    GenTree*  src      = cast->GetOp(0);
    var_types fromType = src->GetType();
    var_types toType   = cast->GetCastType();

    if (varTypeIsFloating(toType) || varTypeIsFloating(fromType))
    {
        return nullptr;
    }

    GenTree* actualSrc = src->SkipComma();

    if (!actualSrc->OperIs(GT_LCL_VAR))
    {
        return nullptr;
    }

    MorphAssertion* assertion = morphAssertionFindRange(actualSrc->AsLclVar()->GetLclNum());

    if (assertion == nullptr)
    {
        return nullptr;
    }

    if (cast->IsUnsigned())
    {
        fromType = varTypeToUnsigned(fromType);
    }

    if (varTypeIsUnsigned(fromType) && (assertion->val.range.loBound < 0))
    {
        return nullptr;
    }

    switch (toType)
    {
        case TYP_BYTE:
        case TYP_UBYTE:
        case TYP_SHORT:
        case TYP_USHORT:
            if ((assertion->val.range.loBound < AssertionDsc::GetLowerBoundForIntegralType(toType)) ||
                (assertion->val.range.hiBound > AssertionDsc::GetUpperBoundForIntegralType(toType)))
            {
                return nullptr;
            }
            break;

        case TYP_UINT:
            if (assertion->val.range.loBound < AssertionDsc::GetLowerBoundForIntegralType(toType))
            {
                return nullptr;
            }
            break;

        case TYP_INT:
            break;

        default:
            return nullptr;
    }

    LclVarDsc* lcl = lvaGetDesc(actualSrc->AsLclVar());
    assert(!lcl->IsAddressExposed());

    if (!lcl->lvNormalizeOnLoad() && !varTypeIsLong(lcl->GetType()))
    {
        return src;
    }

    if (varTypeSize(toType) <= varTypeSize(lcl->GetType()))
    {
        if (toType == TYP_UINT)
        {
            toType = TYP_INT;
        }

        GenTree* tmp = src;

        for (; tmp->OperIs(GT_COMMA); tmp = tmp->AsOp()->GetOp(1))
        {
            tmp->SetType(toType);
        }

        tmp->SetType(toType);

#ifdef DEBUG
        if (verbose)
        {
            printf("\nRange prop for index #%02u:\n", assertion - morphAssertionTable);
            gtDispTree(cast, nullptr, nullptr, true);
        }
#endif

        return src;
    }

    if ((cast->gtFlags & GTF_OVERFLOW) != 0)
    {
        cast->gtFlags &= ~GTF_OVERFLOW;

#ifdef DEBUG
        if (verbose)
        {
            printf("\nRange prop for index #%02u:\n", assertion - morphAssertionTable);
            gtDispTree(cast, nullptr, nullptr, true);
        }
#endif

        return cast;
    }

    return nullptr;
}

GenTree* Compiler::morphAssertionPropIndir(GenTreeIndir* indir)
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

    if (MorphAssertion* assertion = morphAssertionIsNotNull(addr->AsLclVar()->GetLclNum()))
    {
#ifdef DEBUG
        if (verbose)
        {
            printf("\nNon-null prop for index #%02u:\n", assertion - morphAssertionTable);
            gtDispTree(indir, nullptr, nullptr, true);
        }
#endif

        indir->gtFlags &= ~GTF_EXCEPT;
        indir->gtFlags |= GTF_IND_NONFAULTING;
        // Set this flag to prevent reordering
        indir->gtFlags |= GTF_ORDER_SIDEEFF;

        return indir;
    }

    return nullptr;
}

Compiler::MorphAssertion* Compiler::morphAssertionIsNotNull(unsigned lclNum)
{
    for (unsigned index = 0; index < optAssertionCount; index++)
    {
        MorphAssertion* curAssertion = morphGetAssertion(index);

        if ((curAssertion->kind == Kind::NotNull) && (curAssertion->lcl.lclNum == lclNum))
        {
            assert(curAssertion->valKind == ValueKind::IntCon);
            assert(curAssertion->val.intCon.value == 0);
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

    if (!op1->OperIs(GT_LCL_VAR))
    {
        return nullptr;
    }

    if (MorphAssertion* assertion = morphAssertionIsNotNull(op1->AsLclVar()->GetLclNum()))
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
            return morphAssertionPropIndir(tree->AsIndir());

        case GT_CAST:
            return morphAssertionPropCast(tree->AsCast());

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
