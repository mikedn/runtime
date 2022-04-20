// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"
#ifdef _MSC_VER
#pragma hdrstop
#endif

#if LOCAL_ASSERTION_PROP

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

    INDEBUG(unsigned id;)

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

    ASSERT_TP& dep = morphAssertionDep->GetRef(lclNum);

    if (BitVecOps::MayBeUninit(dep))
    {
        dep = BitVecOps::MakeEmpty(apTraits);
    }

    return dep;
}

void Compiler::morphAssertionInit()
{
    optLocalAssertionProp = true;

    CompAllocator allocator = getAllocator(CMK_AssertionProp);

    morphAssertionTable = new (allocator) MorphAssertion[morphAssertionMaxCount];
    morphAssertionDep   = new (allocator) JitExpandArray<ASSERT_TP>(allocator, max(1, lvaCount));
    apTraits            = new (allocator) BitVecTraits(morphAssertionMaxCount, this);
    optAssertionCount   = 0;

    INDEBUG(morphAssertionId = 0);
}

// The following resets the value assignment table
// used only during local assertion prop
void Compiler::morphAssertionReset(unsigned limit)
{
    PREFAST_ASSUME(optAssertionCount <= morphAssertionMaxCount);

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
    PREFAST_ASSUME(optAssertionCount <= morphAssertionMaxCount);

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
    assert(count <= morphAssertionMaxCount);
    return count * sizeof(MorphAssertion);
}

void Compiler::morphAssertionCopyTable(MorphAssertion* toTable, MorphAssertion* fromTable, unsigned count)
{
    assert(count <= morphAssertionMaxCount);
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

#ifdef DEBUG

void Compiler::morphAssertionTrace(MorphAssertion* assertion, GenTree* node, const char* message)
{
    printf("[%06u] %s assertion", node->GetID(), message);

    if ((assertion >= morphAssertionTable) && (assertion < morphAssertionTable + optAssertionCount))
    {
        printf(" #%02u", assertion->id);
    }

    printf(" V%02u", assertion->lcl.lclNum);

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
    assert((assertion->kind != Kind::Invalid) && (assertion->lcl.lclNum < lvaCount));
    return assertion;
}

void Compiler::morphAssertionGenNotNull(GenTree* addr)
{
    if (optAssertionCount >= morphAssertionMaxCount)
    {
        return;
    }

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
        return;
    }

    unsigned   lclNum = addr->AsLclVar()->GetLclNum();
    LclVarDsc* lcl    = lvaGetDesc(lclNum);

    // TODO-MIKE-Review: It's not clear why is this restricted to REF. Old comment
    // stated "we only perform null-checks on GC refs" but that's rather bogus.
    if (!lcl->TypeIs(TYP_REF))
    {
        return;
    }

    // TODO-MIKE-CQ: This could probably work, we just need to kill assertions
    // involving such locals when we encounter memory stores or calls.
    if (lcl->IsAddressExposed())
    {
        return;
    }

    // We don't need more than one NotNull assertion for a local. In theory we also
    // don't need a NotNull assertion if we already have a constant assertion, but
    // currently the NotNull propagation code ignores constant assertions.
    for (unsigned i = optAssertionCount - 1; i != UINT32_MAX; i--)
    {
        MorphAssertion* existing = morphGetAssertion(i);

        if ((existing->lcl.lclNum == lclNum) && (existing->kind == Kind::NotNull))
        {
            return;
        }
    }

    MorphAssertion& assertion = morphAssertionTable[optAssertionCount];

    // TODO-MIKE-Cleanup: Try to get rid of memset if possible. The use of memcmp
    // in assertion merging makes it necessary now as there are alignment holes in
    // MorphAssertion and we need to ensure they do not contain garbage. Otherwise
    // it's obvious that the code below initializes all the needed data members.
    memset(&assertion, 0, sizeof(MorphAssertion));

    assertion.kind             = Kind::NotNull;
    assertion.valKind          = ValueKind::IntCon;
    assertion.lcl.lclNum       = lclNum;
    assertion.val.intCon.value = 0;
    assertion.val.intCon.flags = GTF_EMPTY;

    morphAssertionAdd(assertion);
}

void Compiler::morphAssertionGenEqual(GenTreeLclVar* lclVar, GenTree* val)
{
    unsigned   lclNum = lclVar->GetLclNum();
    LclVarDsc* lcl    = lvaGetDesc(lclNum);

    if (lcl->IsAddressExposed())
    {
        return;
    }

    // We're trying to add an Equal assertion when we already have another assertion
    // about this local. This should not happen, existing assertions should have been
    // killed by morph before generating new ones. Just drop to minopts, morphing code
    // is likely broken.
    // TODO-MIKE-Consider: Maybe we can simply overwrite an existing assertion?
    noway_assert(BitVecOps::IsEmpty(apTraits, GetAssertionDep(lclNum)));

    if (optAssertionCount >= morphAssertionMaxCount)
    {
        return;
    }

    MorphAssertion& assertion = morphAssertionTable[optAssertionCount];
    memset(&assertion, 0, sizeof(MorphAssertion));
    assert(assertion.kind == Kind::Invalid);
    assertion.lcl.lclNum = lclNum;

    val = val->SkipComma();

    switch (val->GetOper())
    {
        case GT_CNS_INT:
#ifdef TARGET_ARM
            if (!codeGen->validImmForMov(val->AsIntCon()->GetInt32Value()))
            {
                return;
            }
#endif

            if (lcl->TypeIs(TYP_LONG) && !val->TypeIs(TYP_LONG))
            {
                return;
            }

            assertion.kind             = Kind::Equal;
            assertion.valKind          = ValueKind::IntCon;
            assertion.val.intCon.value = val->AsIntCon()->GetValue(lcl->GetType());
            assertion.val.intCon.flags = val->AsIntCon()->GetHandleKind();
            break;

#ifndef TARGET_64BIT
        case GT_CNS_LNG:
            assert(lcl->TypeIs(TYP_LONG) && lclVar->TypeIs(TYP_LONG) && val->TypeIs(TYP_LONG));

            assertion.kind             = Kind::Equal;
            assertion.valKind          = ValueKind::LngCon;
            assertion.val.lngCon.value = val->AsLngCon()->GetValue();
            break;
#endif

        case GT_CNS_DBL:
            assert((lcl->GetType() == lclVar->GetType()) && (lcl->GetType() == val->GetType()));

            if (_isnan(val->AsDblCon()->GetValue()))
            {
                return;
            }

            assertion.kind             = Kind::Equal;
            assertion.valKind          = ValueKind::DblCon;
            assertion.val.dblCon.value = val->AsDblCon()->GetValue();
            break;

        case GT_LCL_VAR:
        {
            unsigned   valLclNum = val->AsLclVar()->GetLclNum();
            LclVarDsc* valLcl    = lvaGetDesc(valLclNum);

            if ((lclNum == valLclNum) || valLcl->IsAddressExposed())
            {
                return;
            }

            // TODO-MIKE-Review: This might be overly restrictive when small int locals are
            // involved. It should be possible to allow a copy from a small int local to an
            // INT local, probably with a bit of care when doing the actual substitution
            // (now it expects copies to be symmetric).
            if (lcl->GetType() != valLcl->GetType())
            {
                return;
            }

            // TODO-MIKE-Review: This looks a bit odd. Local assertion propagation runs before
            // morphing so we haven't yet inserted the necessary (or not) widening cast. This
            // might be a leftover from global assertion propagation.
            if (valLcl->lvNormalizeOnLoad() && !lcl->lvNormalizeOnLoad())
            {
                return;
            }

            assertion.kind           = Kind::Equal;
            assertion.valKind        = ValueKind::LclVar;
            assertion.val.lcl.lclNum = valLclNum;
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
            if (varTypeIsSmall(val->GetType()))
            {
                assertion.val.range.loBound = AssertionDsc::GetLowerBoundForIntegralType(val->GetType());
                assertion.val.range.hiBound = AssertionDsc::GetUpperBoundForIntegralType(val->GetType());
            }
#ifdef TARGET_64BIT
            // TODO-MIKE-CQ: This is useless nonsense, of course an INT load has range
            // INT32_MIN..INT32_MAX. Problem is, these assertions still take space in
            // the assertion table and can prevent more assertions from being created
            // so removing this causes diffs. Remove it when it's all done.
            // Also note that loads should not have UINT type but due to bogus JIT code
            // that does happen sometimes.
            else if (val->TypeIs(TYP_INT, TYP_UINT))
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
            if (lcl->IsPromotedField() && lcl->lvNormalizeOnLoad())
            {
                // TODO-MIKE-Review: It's not clear why a range assertion is not generated in
                // this case. In typical idiotic fashion old comment stated what the code is
                // doing instead of why it is doing it.
                return;
            }

            {
                var_types toType = val->AsCast()->GetCastType();

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
            }

            assertion.valKind = ValueKind::Range;
            assertion.kind    = Kind::Equal;
            break;

        default:
            return;
    }

    morphAssertionAdd(assertion);
}

void Compiler::morphAssertionAdd(MorphAssertion& assertion)
{
    assert((assertion.kind != Kind::Invalid) && (assertion.valKind != ValueKind::Invalid));
    assert(optAssertionCount < morphAssertionMaxCount);
    assert(&assertion == &morphAssertionTable[optAssertionCount]);

    INDEBUG(assertion.id = ++morphAssertionId);
    DBEXEC(verbose, morphAssertionTrace(&assertion, optAssertionPropCurrentTree, "generated"));

    BitVecOps::AddElemD(apTraits, GetAssertionDep(assertion.lcl.lclNum), optAssertionCount);

    if (assertion.valKind == ValueKind::LclVar)
    {
        BitVecOps::AddElemD(apTraits, GetAssertionDep(assertion.val.lcl.lclNum), optAssertionCount);
    }

    optAssertionCount++;
}

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
                morphAssertionGenEqual(tree->AsOp()->GetOp(0)->AsLclVar(), tree->AsOp()->GetOp(1));
            }
            break;

        case GT_BLK:
        case GT_OBJ:
            assert(tree->AsBlk()->GetLayout()->GetSize() != 0);
            FALLTHROUGH;
        case GT_IND:
        case GT_NULLCHECK:
            morphAssertionGenNotNull(tree->AsIndir()->GetAddr());
            break;
        case GT_ARR_LENGTH:
            morphAssertionGenNotNull(tree->AsArrLen()->GetArray());
            break;
        case GT_ARR_ELEM:
            morphAssertionGenNotNull(tree->AsArrElem()->GetArray());
            break;

        case GT_CALL:
        {
            // A virtual call can create a non-null assertion. We transform some virtual calls into non-virtual calls
            // with a GTF_CALL_NULLCHECK flag set.
            // Ignore tail calls because they have 'this` pointer in the regular arg list and an implicit null check.
            GenTreeCall* const call = tree->AsCall();
            if (call->NeedsNullCheck() || (call->IsVirtual() && !call->IsTailCall()))
            {
                morphAssertionGenNotNull(call->GetThisArg());
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

GenTree* Compiler::morphAssertionPropagateConst(MorphAssertion* assertion, GenTreeLclVar* lclVar)
{
    LclVarDsc* lcl = lvaGetDesc(lclVar->GetLclNum());

    assert(!lcl->IsAddressExposed());

    if (lcl->lvIsCSE)
    {
        return nullptr;
    }

    const auto& val     = assertion->val;
    GenTree*    conNode = nullptr;

    switch (assertion->valKind)
    {
        case ValueKind::DblCon:
            // There could be a positive zero and a negative zero, so don't propagate zeroes.
            // TODO-MIKE-Review: So what?
            if (val.dblCon.value == 0.0)
            {
                break;
            }

            assert(lcl->GetType() == lclVar->GetType());

            conNode = lclVar->ChangeToDblCon(lcl->GetType(), val.dblCon.value);
            break;

#ifndef TARGET_64BIT
        case ValueKind::LngCon:
            assert(lcl->TypeIs(TYP_LONG));

            if (lclVar->TypeIs(TYP_INT))
            {
                // Morphing sometimes performs implicit narrowing by changing LONG LCL_VARs to INT.
                // TODO-MIKE-Review: But propagation is done before morphing, is this needed?
                conNode = lclVar->ChangeToIntCon(static_cast<int32_t>(val.lngCon.value));
                break;
            }

            assert(lclVar->TypeIs(TYP_LONG));

            conNode = lclVar->ChangeToLngCon(val.lngCon.value);
            break;
#endif

        case ValueKind::IntCon:
            if (varTypeIsSmall(lcl->GetType())
#ifdef TARGET_64BIT
                || lcl->TypeIs(TYP_INT) // Handle INT separately on 32 bit as it may be a handle.
#endif
                )
            {
                // For small int locals we often get INT LCL_VARs, otherwise the types should match.
                assert((lcl->GetType() == lclVar->GetType()) || lclVar->TypeIs(TYP_INT));
                assert(lcl->TypeIs(TYP_INT) || varTypeSmallIntCanRepresentValue(lcl->GetType(), val.intCon.value));

                conNode = lclVar->ChangeToIntCon(TYP_INT, val.intCon.value);
                break;
            }

            if (lclVar->TypeIs(TYP_STRUCT))
            {
                assert(val.intCon.value == 0);
                assert(lcl->GetType() == lclVar->GetType());

                conNode = lclVar->ChangeToIntCon(TYP_INT, 0);
                break;
            }

#ifdef FEATURE_SIMD
            if (varTypeIsSIMD(lclVar->GetType()))
            {
                assert(val.intCon.value == 0);
                assert(lcl->GetType() == lclVar->GetType());

                conNode = gtNewZeroSimdHWIntrinsicNode(lcl->GetLayout());
                break;
            }
#endif

            if (lcl->TypeIs(TYP_LONG) && lclVar->TypeIs(TYP_INT))
            {
                // Morphing sometimes performs implicit narrowing by changing LONG LCL_VARs to INT.
                // TODO-MIKE-Review: But propagation is done before morphing, is this needed?
                conNode = lclVar->ChangeToIntCon(TYP_INT, static_cast<int32_t>(val.intCon.value));
                break;
            }

            assert(varTypeIsI(lcl->GetType()));
            assert(varTypeIsI(lclVar->GetType()));

            if ((val.intCon.flags & GTF_ICON_HDL_MASK) != 0)
            {
                if (opts.compReloc)
                {
                    break;
                }

                // TODO-MIKE-Review: It's not clear why this is done only for handles. It's a
                // constant so it obviously does not need to be reported to the GC.
                // On the other hand, we don't know the user and blindly changing types like
                // this isn't great.
                lclVar->SetType(TYP_I_IMPL);
            }

            conNode = lclVar->ChangeToIntCon(val.intCon.value);
            conNode->AsIntCon()->SetHandleKind(val.intCon.flags & GTF_ICON_HDL_MASK);
            break;

        default:
            break;
    }

    if (conNode != nullptr)
    {
        DBEXEC(verbose, morphAssertionTrace(assertion, conNode, "propagated"));
    }

    return conNode;
}

GenTree* Compiler::morphAssertionPropagateCopy(MorphAssertion* assertion, GenTreeLclVar* lclVar)
{
    assert((assertion->kind == Kind::Equal) && (assertion->valKind == ValueKind::LclVar));

    unsigned lclNumDst = assertion->lcl.lclNum;
    unsigned lclNumSrc = assertion->val.lcl.lclNum;

    assert(lclNumDst != lclNumSrc);

    unsigned lclNum = lclVar->GetLclNum();

    if ((lclNum != lclNumDst) && (lclNum != lclNumSrc))
    {
        return nullptr;
    }

    unsigned lclNumCopy = (lclNumDst == lclNum) ? lclNumSrc : lclNumDst;

    LclVarDsc* const lcl = lvaGetDesc(lclNum);
    assert(!lcl->IsAddressExposed());
    LclVarDsc* const lclCopy = lvaGetDesc(lclNumCopy);
    assert(!lclCopy->IsAddressExposed());

    // We can't use a small int promoted field if the original LCL_VAR doesn't have the same type
    // as the field. If the field ends up being P-DEP then we risk reading bits from adjacent fields.
    // TODO-MIKE-Cleanup: It's not clear what the problem is here. Such a promoted field is supposed
    // to be widened on load so any garbage bits would be discarded. This might be related to struct
    // assignment promotion where widening casts are not inserted.
    if (lclCopy->IsPromotedField() && varTypeIsSmall(lclCopy->GetType()) && (lclCopy->GetType() != lclVar->GetType()))
    {
        return nullptr;
    }

    // TODO-MIKE-Review: This whole thing doesn't make much sense. Why special case DOUBLE on x86?
    // Without it we'd be doing copy propagation only in the "normal" case where we have X = Y and
    // replace subsequent uses of X with Y. But due to this weird crap we have Y = X and replace
    // uses of X with Y!
    int score = (lclNumDst == lclNum) ? 1 : -1;

#ifdef TARGET_X86
    if (lcl->TypeIs(TYP_DOUBLE))
    {
        if (lcl->IsParam())
        {
            score += 2;
        }

        if (lclCopy->IsParam())
        {
            score -= 2;
        }
    }
#endif

    if (score <= 0)
    {
        return nullptr;
    }

    lclVar->SetLclNum(lclNumCopy);

    DBEXEC(verbose, morphAssertionTrace(assertion, lclVar, "propagated"));

    return lclVar;
}

//---------------------------------------------------
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
            GenTree* newTree = morphAssertionPropagateCopy(curAssertion, tree);
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
            // TODO-MIKE-CQ: This is dubious, it tends to block constant prop for small int locals.
            // Verify types match
            if (tree->TypeGet() == lclDsc->lvType)
            {
                return morphAssertionPropagateConst(curAssertion, tree);
            }
        }
    }

    return nullptr;
}

GenTree* Compiler::morphAssertionPropRelOp(GenTreeOp* relop)
{
    assert(relop->OperIs(GT_EQ, GT_NE));

    GenTree* op1 = relop->GetOp(0);
    GenTree* op2 = relop->GetOp(1);

    if (!op1->OperIs(GT_LCL_VAR) || !op2->IsIntCon())
    {
        return nullptr;
    }

    assert(varTypeIsIntegralOrI(op1->GetType()));

    unsigned   lclNum = op1->AsLclVar()->GetLclNum();
    LclVarDsc* lcl    = lvaGetDesc(lclNum);

    if (lcl->IsAddressExposed())
    {
        return nullptr;
    }

#ifndef TARGET_64BIT
    // For 32 bit targets IntCon cannot be LONG but may have a LONG local due to implicit
    // narrowing to INT of LCL_VARs. For now just bail out since the rest of the code only
    // looks for IntCon assertions and it won't find anything if the local is LONG.
    if (lcl->TypeIs(TYP_LONG))
    {
        return nullptr;
    }
#endif

    ssize_t value     = op2->AsIntCon()->GetValue();
    ssize_t valueMask = -1;

    // Ensure that INT comparisons are done using only 32 bits.
    // This also deals with the case of implicit LCL_VAR narrowing to INT of LONG locals,
    // when we have a 64 bit value stored in the assertion.
    if (varActualTypeIsInt(op1->GetType()))
    {
        assert(varActualTypeIsInt(op2->GetType()));

        valueMask = UINT32_MAX;
        value &= valueMask;
    }

    int result = -1;

    for (unsigned index = 0; index < optAssertionCount; ++index)
    {
        MorphAssertion* assertion = morphGetAssertion(index);

        if ((assertion->lcl.lclNum != lclNum) || (assertion->valKind != ValueKind::IntCon))
        {
            continue;
        }

        if (assertion->kind == Kind::Equal)
        {
            result = relop->OperIs(GT_EQ) == (value == (assertion->val.intCon.value & valueMask)) ? 1 : 0;
            DBEXEC(verbose, morphAssertionTrace(assertion, op2, "propagated"));
            break;
        }

        assert(assertion->kind == Kind::NotNull);
        assert(assertion->val.intCon.value == 0);

        if (value == 0)
        {
            result = relop->OperIs(GT_NE) ? 1 : 0;
            DBEXEC(verbose, morphAssertionTrace(assertion, op2, "propagated"));
            break;
        }
    }

    if (result == -1)
    {
        return nullptr;
    }

    op2->AsIntCon()->SetValue(TYP_INT, result);

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

        DBEXEC(verbose, morphAssertionTrace(assertion, cast, "propagated"));

        return src;
    }

    if ((cast->gtFlags & GTF_OVERFLOW) != 0)
    {
        cast->gtFlags &= ~GTF_OVERFLOW;

        DBEXEC(verbose, morphAssertionTrace(assertion, cast, "propagated"));

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
        DBEXEC(verbose, morphAssertionTrace(assertion, indir, "propagated"));

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
        DBEXEC(verbose, morphAssertionTrace(assertion, call, "propagated"));

        call->gtFlags &= ~GTF_CALL_NULLCHECK;
        call->gtFlags &= ~GTF_EXCEPT;

        noway_assert(call->gtFlags & GTF_SIDE_EFFECT);

        return call;
    }

    return nullptr;
}

GenTree* Compiler::morphAssertionProp(GenTree* tree)
{
    GenTree* newTree = tree;

    // In some cases (CAST removal, LCL_VAR copy propagation) we end up returning
    // a different node that too may be subject to assertion propagation.
    // TODO-MIKE-Review: This looks dangerous, if some code returns the original
    // tree instead of null when not making any changes we'll get stuck here.
    // There are only a couple of cases that need iteration so it could be better
    // to do this in the specific code (e.g. when a cast is removed).

    do
    {
        tree = newTree;

        switch (tree->GetOper())
        {
            case GT_LCL_VAR:
                newTree = morphAssertionProp_LclVar(tree->AsLclVar());
                break;
            case GT_OBJ:
            case GT_BLK:
            case GT_IND:
            case GT_NULLCHECK:
                newTree = morphAssertionPropIndir(tree->AsIndir());
                break;
            case GT_CAST:
                newTree = morphAssertionPropCast(tree->AsCast());
                break;
            case GT_CALL:
                newTree = morphAssertionProp_Call(tree->AsCall());
                break;
            case GT_EQ:
            case GT_NE:
                newTree = morphAssertionPropRelOp(tree->AsOp());
                break;
            default:
                newTree = nullptr;
                break;
        }
    } while (newTree != nullptr);

    return tree;
}

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
                MorphAssertion* curAssertion = morphGetAssertion(count - 1);

                assert((curAssertion->lcl.lclNum == lclNum) ||
                       ((curAssertion->valKind == ValueKind::LclVar) && (curAssertion->val.lcl.lclNum == lclNum)));

                DBEXEC(verbose, morphAssertionTrace(curAssertion, tree, "killed"));

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
    morphAssertionKillSingle(lclNum DEBUGARG(tree));

    LclVarDsc* varDsc = lvaGetDesc(lclNum);

    if (varDsc->lvPromoted)
    {
        noway_assert(varTypeIsStruct(varDsc));

        // Kill the field locals.
        for (unsigned i = varDsc->lvFieldLclStart; i < varDsc->lvFieldLclStart + varDsc->lvFieldCnt; ++i)
        {
            morphAssertionKillSingle(i DEBUGARG(tree));
        }
    }
    else if (varDsc->lvIsStructField)
    {
        morphAssertionKillSingle(varDsc->lvParentLcl DEBUGARG(tree));
    }
}

#endif // LOCAL_ASSERTION_PROP
