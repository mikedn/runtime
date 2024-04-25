// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

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
        ssize_t    value;
        HandleKind handleKind;
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
        // For now ranges can be INT only, we'd only need LONG for overflow checking casts
        // to/from ULONG, those would generate a 0..INT64_MAX range but they're rare.
        int32_t min;
        int32_t max;
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
                return (x.intCon.value == y.intCon.value) && (x.intCon.handleKind == y.intCon.handleKind);
#ifndef TARGET_64BIT
            case ValueKind::LngCon:
                return (x.lngCon.value == y.lngCon.value);
#endif
            case ValueKind::DblCon:
                return jitstd::bit_cast<uint64_t>(x.dblCon.value) == jitstd::bit_cast<uint64_t>(y.dblCon.value);
            case ValueKind::LclVar:
                return x.lcl.lclNum == y.lcl.lclNum;
            case ValueKind::Range:
                return (x.range.min == y.range.min) && (y.range.max == y.range.max);
            default:
                return false;
        }
    }

    bool operator==(const MorphAssertion& that) const
    {
        return (kind == that.kind) && (lcl.lclNum == that.lcl.lclNum) && HasSameValue(that);
    }
};

using MorphAssertion = Compiler::MorphAssertion;
using Kind           = MorphAssertion::Kind;
using ValueKind      = MorphAssertion::ValueKind;

static const MorphAssertion::Range& GetSmallTypeRange(var_types type)
{
    static const MorphAssertion::Range ranges[]{
        {0, 1},                 // BOOL
        {INT8_MIN, INT8_MAX},   // BYTE
        {0, UINT8_MAX},         // UBYTE
        {INT16_MIN, INT16_MAX}, // SHORT
        {0, UINT16_MAX},        // USHORT
    };

    static_assert_no_msg(TYP_BYTE - TYP_BOOL == 1);
    static_assert_no_msg(TYP_UBYTE - TYP_BOOL == 2);
    static_assert_no_msg(TYP_SHORT - TYP_BOOL == 3);
    static_assert_no_msg(TYP_USHORT - TYP_BOOL == 4);

    assert(varTypeIsSmall(type));
    return ranges[type - TYP_BOOL];
}

struct MorphAssertionBitVecTraits
{
    using Env  = Compiler*;
    using Word = size_t;

    static Word* Alloc(Compiler* c, unsigned wordCount)
    {
        return c->getAllocator(CMK_MorphAssertion).allocate<Word>(wordCount);
    }

    static unsigned GetSize(const Compiler*)
    {
        return Compiler::morphAssertionMaxCount;
    }

    static unsigned GetWordCount(const Compiler*)
    {
        unsigned wordBitSize = sizeof(Word) * CHAR_BIT;
        return roundUp(Compiler::morphAssertionMaxCount, wordBitSize) / wordBitSize;
    }

    static bool IsShort(const Compiler* c)
    {
        return GetWordCount(c) <= 1;
    }
};

// TODO-MIKE-Throughput: It looks like on 32 bit hosts we end up allocating memory for bit vectors
// because the max assertion count is still 64. And we can't reduce it to 32, in fact we'd want to
// increase it because some block need more than 64 assertions. One alternative would be to just
// use uint64_t as a bit vector but then morphAssertionDep would be larger on 32 bit hosts. And if
// we need more than 64 assertions then we're back to square 1 anyway.
//
// At the end of the day local assertion storage is messed up and could use a replacement.
// LclVarDsc appears to have 2 pointer sized members that are not needed during global morph, those
// could be used to link assertions directly to locals and entirely avoid the bit vector stuff, not
// to mention the stupid linear search that's probably one of the reasons the max assertion count is
// limited to 64 now.
using DepBitVecOps = BitSetOps<MorphAssertionBitVecTraits>;

void Compiler::morphAssertionInit()
{
    assert(fgGlobalMorph);

    if (!opts.OptimizationEnabled())
    {
        morphAssertionTable = nullptr;
        morphAssertionCount = 0;

        return;
    }

    CompAllocator allocator = getAllocator(CMK_MorphAssertion);

    morphAssertionTable = new (allocator) MorphAssertion[morphAssertionMaxCount];
    morphAssertionDep   = new (allocator) JitExpandArray<BitVec>(allocator, max(1, lvaCount));
    morphAssertionCount = 0;

    INDEBUG(morphAssertionId = 0);
}

void Compiler::morphAssertionDone()
{
    assert(fgGlobalMorph);

    morphAssertionTable = nullptr;
    morphAssertionCount = 0;
}

void Compiler::morphAssertionSetCount(unsigned count)
{
    assert(fgGlobalMorph);
    assert(count <= morphAssertionMaxCount);

    while (morphAssertionCount > count)
    {
        const unsigned        index     = morphAssertionCount - 1;
        const MorphAssertion& assertion = morphAssertionGet(index);
        morphAssertionCount--;

        DepBitVecOps::RemoveElemD(this, morphAssertionGetDependent(assertion.lcl.lclNum), index);

        if (assertion.valKind == ValueKind::LclVar)
        {
            DepBitVecOps::RemoveElemD(this, morphAssertionGetDependent(assertion.val.lcl.lclNum), index);
        }
    }

    while (morphAssertionCount < count)
    {
        const unsigned        index     = morphAssertionCount++;
        const MorphAssertion& assertion = morphAssertionGet(index);

        DepBitVecOps::AddElemD(this, morphAssertionGetDependent(assertion.lcl.lclNum), index);

        if (assertion.valKind == ValueKind::LclVar)
        {
            DepBitVecOps::AddElemD(this, morphAssertionGetDependent(assertion.val.lcl.lclNum), index);
        }
    }
}

BitVec& Compiler::morphAssertionGetDependent(unsigned lclNum)
{
    assert(lclNum < lvaCount);

    BitVec& dep = (*morphAssertionDep)[lclNum];

    if (DepBitVecOps::MayBeUninit(dep))
    {
        dep = DepBitVecOps::MakeEmpty(this);
    }

    return dep;
}

void Compiler::morphAssertionRemove(unsigned index)
{
    assert(index < morphAssertionCount);
    assert(morphAssertionCount <= morphAssertionMaxCount);

    const MorphAssertion& assertion = morphAssertionGet(index);

    DepBitVecOps::RemoveElemD(this, morphAssertionGetDependent(assertion.lcl.lclNum), index);

    if (assertion.valKind == ValueKind::LclVar)
    {
        DepBitVecOps::RemoveElemD(this, morphAssertionGetDependent(assertion.val.lcl.lclNum), index);
    }

    // The order of the assertions isn't important so if the removed assertion isn't
    // at the end of the table then just move the last assertion instead of moving
    // down all the assertions between the removed one and the end of the table.

    unsigned lastIndex = morphAssertionCount - 1;

    if (index != lastIndex)
    {
        const MorphAssertion& lastAssertion = morphAssertionGet(lastIndex);

        BitVec& lastDep = morphAssertionGetDependent(lastAssertion.lcl.lclNum);
        DepBitVecOps::RemoveElemD(this, lastDep, lastIndex);
        DepBitVecOps::AddElemD(this, lastDep, index);

        if (lastAssertion.valKind == ValueKind::LclVar)
        {
            BitVec& lastCopyDep = morphAssertionGetDependent(lastAssertion.val.lcl.lclNum);
            DepBitVecOps::RemoveElemD(this, lastCopyDep, lastIndex);
            DepBitVecOps::AddElemD(this, lastCopyDep, index);
        }

        morphAssertionTable[index] = lastAssertion;
    }

    morphAssertionCount--;
}

unsigned Compiler::morphAssertionTableSize(unsigned count)
{
    assert(fgGlobalMorph);
    assert(count <= morphAssertionMaxCount);

    return count * sizeof(MorphAssertion);
}

void Compiler::morphAssertionGetTable(MorphAssertion* table, unsigned count)
{
    assert(fgGlobalMorph);
    assert(count <= morphAssertionCount);

    memcpy(table, morphAssertionTable, count * sizeof(MorphAssertion));
}

void Compiler::morphAssertionSetTable(const MorphAssertion* table, unsigned count)
{
    assert(fgGlobalMorph);
    assert(count <= morphAssertionMaxCount);

    memcpy(morphAssertionTable, table, count * sizeof(MorphAssertion));
    morphAssertionSetCount(count);
}

void Compiler::morphAssertionMerge(unsigned              elseAssertionCount,
                                   const MorphAssertion* elseAssertionTable DEBUGARG(GenTreeQmark* qmark))
{
    assert(fgGlobalMorph);

    if (morphAssertionCount == 0)
    {
        return;
    }

    if (elseAssertionCount == 0)
    {
        morphAssertionSetCount(0);
        return;
    }

    for (unsigned index = 0; index < morphAssertionCount;)
    {
        const MorphAssertion& thenAssertion        = morphAssertionGet(index);
        bool                  hasMatchingAssertion = false;

        // QMARK's "else" branch rarely removes assertions, start by searching for a matching assertion
        // from the current "then" index (and wrap around if there actually are fewer "else" assertions).
        for (unsigned j = 0, elseIndex = index; j < elseAssertionCount; j++)
        {
            if (elseIndex > elseAssertionCount)
            {
                elseIndex = 0;
            }

            if (elseAssertionTable[elseIndex++] == thenAssertion)
            {
                hasMatchingAssertion = true;
                break;
            }
        }

        if (hasMatchingAssertion)
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

void Compiler::morphAssertionTrace(const MorphAssertion& assertion, GenTree* node, const char* message)
{
    printf("[%06u] %s assertion #%02u V%02u", node->GetID(), message, assertion.id, assertion.lcl.lclNum);

    const char* op;

    switch (assertion.kind)
    {
        case Kind::Equal:
            op = assertion.valKind == ValueKind::Range ? "in" : "==";
            break;
        case Kind::NotNull:
            op = "!=";
            break;
        default:
            op = "???";
            break;
    }

    printf(" %s ", op);

    const auto& val = assertion.val;

    switch (assertion.valKind)
    {
        case ValueKind::LclVar:
            printf("V%02u", val.lcl.lclNum);
            break;
        case ValueKind::IntCon:
            if (val.intCon.handleKind != HandleKind::None)
            {
                printf("%08p (%s)", dspPtr(val.intCon.value), dmpGetHandleKindName(val.intCon.handleKind));
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
            printf("[%d..%d]", val.range.min, val.range.max);
            break;
        default:
            printf("???");
            break;
    }

    printf("\n");
}

#endif // DEBUG

const MorphAssertion& Compiler::morphAssertionGet(unsigned index)
{
    assert(index < morphAssertionCount);
    const MorphAssertion& assertion = morphAssertionTable[index];
    assert((assertion.kind != Kind::Invalid) && (assertion.lcl.lclNum < lvaCount));
    return assertion;
}

void Compiler::morphAssertionGenerateNotNull(GenTree* addr)
{
    if (morphAssertionCount >= morphAssertionMaxCount)
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

    LclVarDsc* lcl = addr->AsLclVar()->GetLcl();

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

    unsigned lclNum = lcl->GetLclNum();

    // We don't need more than one NotNull assertion for a local. In theory we also
    // don't need a NotNull assertion if we already have a constant assertion, but
    // currently the NotNull propagation code ignores constant assertions.
    for (unsigned i = morphAssertionCount - 1; i != UINT32_MAX; i--)
    {
        const MorphAssertion& existing = morphAssertionGet(i);

        if ((existing.lcl.lclNum == lclNum) && (existing.kind == Kind::NotNull))
        {
            return;
        }
    }

    MorphAssertion& assertion = morphAssertionTable[morphAssertionCount];

    assertion.kind                  = Kind::NotNull;
    assertion.valKind               = ValueKind::IntCon;
    assertion.lcl.lclNum            = lclNum;
    assertion.val.intCon.value      = 0;
    assertion.val.intCon.handleKind = HandleKind::None;

    morphAssertionAdd(assertion);
}

void Compiler::morphAssertionGenerateEqual(GenTreeLclVar* store, GenTree* val)
{
    assert(store->OperIs(GT_STORE_LCL_VAR));

    LclVarDsc* lcl = store->GetLcl();

    if (lcl->IsAddressExposed())
    {
        return;
    }

    unsigned lclNum = lcl->GetLclNum();

    // We're trying to add an Equal assertion when we already have another assertion
    // about this local. This should not happen, existing assertions should have been
    // killed by morph before generating new ones. Just drop to minopts, morphing code
    // is likely broken.
    // TODO-MIKE-Consider: Maybe we can simply overwrite an existing assertion?
    noway_assert(DepBitVecOps::IsEmpty(this, morphAssertionGetDependent(lclNum)));

    if (morphAssertionCount >= morphAssertionMaxCount)
    {
        return;
    }

    MorphAssertion& assertion = morphAssertionTable[morphAssertionCount];
    assertion.kind            = Kind::Invalid;
    assertion.valKind         = ValueKind::Invalid;
    assertion.lcl.lclNum      = lclNum;

    val = val->SkipComma();

    switch (val->GetOper())
    {
        case GT_CNS_INT:
#ifdef TARGET_ARM
            if (!ArmImm::IsMovImm(val->AsIntCon()->GetInt32Value()))
            {
                return;
            }
#endif

            if (lcl->TypeIs(TYP_LONG) && !val->TypeIs(TYP_LONG))
            {
                return;
            }

            assertion.kind                  = Kind::Equal;
            assertion.valKind               = ValueKind::IntCon;
            assertion.val.intCon.value      = val->AsIntCon()->GetValue(lcl->GetType());
            assertion.val.intCon.handleKind = val->AsIntCon()->GetHandleKind();
            break;

#ifndef TARGET_64BIT
        case GT_CNS_LNG:
            assert(lcl->TypeIs(TYP_LONG) && store->TypeIs(TYP_LONG) && val->TypeIs(TYP_LONG));

            assertion.kind             = Kind::Equal;
            assertion.valKind          = ValueKind::LngCon;
            assertion.val.lngCon.value = val->AsLngCon()->GetValue();
            break;
#endif

        case GT_CNS_DBL:
            assert((lcl->GetType() == store->GetType()) && (lcl->GetType() == val->GetType()));

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
            LclVarDsc* valLcl    = val->AsLclVar()->GetLcl();
            unsigned   valLclNum = valLcl->GetLclNum();

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
            assertion.kind      = Kind::Equal;
            assertion.valKind   = ValueKind::Range;
            assertion.val.range = {0, 1};
            break;

        case GT_LCL_FLD:
        case GT_IND:
            if (varTypeIsSmall(val->GetType()))
            {
                assertion.val.range = GetSmallTypeRange(val->GetType());
            }
#ifdef TARGET_64BIT
            // TODO-MIKE-CQ: This is useless nonsense, of course an INT load has range
            // INT32_MIN..INT32_MAX. Problem is, these assertions still take space in
            // the assertion table and can prevent more assertions from being created
            // so removing this causes diffs. Remove it when it's all done.
            else if (val->TypeIs(TYP_INT))
            {
                assertion.val.range = {INT32_MIN, INT32_MAX};
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

            if (varTypeIsLong(lcl->GetType()))
            {
                // TODO-MIKE-Review: We don't generate ranges for LONG locals. Not clear why,
                // it's likely that there aren't many useful cases.
                return;
            }

            if (varTypeIsSmall(val->GetType()))
            {
                assertion.val.range = GetSmallTypeRange(val->GetType());
            }
#ifdef TARGET_64BIT
            else if (val->TypeIs(TYP_INT))
            {
                // TODO-MIKE-CQ: Like in the load case, this is pretty much nonsense. There is
                // a difference however, an overflow checking cast to UINT should produce a
                // 0..INT_32MAX/UINT32_MAX range depending on the source value being INT/LONG.
                // No idea why this always produces an INT32_MIN..INT32_MAX range.
                // We can also have an INT to LONG cast that tells that a LONG local has INT
                // range, this (and any other range information we could deduce from the cast
                // source types) is completely ignored now.
                assertion.val.range = {INT32_MIN, INT32_MAX};
            }
#endif
            else
            {
                return;
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
    assert(morphAssertionCount < morphAssertionMaxCount);
    assert(&assertion == &morphAssertionTable[morphAssertionCount]);

    INDEBUG(assertion.id = ++morphAssertionId);
    DBEXEC(verbose, morphAssertionTrace(assertion, morphAssertionCurrentTree, "generated"));

    DepBitVecOps::AddElemD(this, morphAssertionGetDependent(assertion.lcl.lclNum), morphAssertionCount);

    if (assertion.valKind == ValueKind::LclVar)
    {
        DepBitVecOps::AddElemD(this, morphAssertionGetDependent(assertion.val.lcl.lclNum), morphAssertionCount);
    }

    morphAssertionCount++;
}

void Compiler::morphAssertionGenerate(GenTree* tree)
{
    assert(fgGlobalMorph);
    INDEBUG(morphAssertionCurrentTree = tree);

    switch (tree->GetOper())
    {
        case GT_STORE_LCL_VAR:
            morphAssertionGenerateEqual(tree->AsLclVar(), tree->AsLclVar()->GetOp(0));
            break;

        case GT_OBJ:
        case GT_STORE_OBJ:
        case GT_BLK:
        case GT_STORE_BLK:
            assert(tree->AsBlk()->GetLayout()->GetSize() != 0);
            FALLTHROUGH;
        case GT_IND:
        case GT_STOREIND:
        case GT_NULLCHECK:
            morphAssertionGenerateNotNull(tree->AsIndir()->GetAddr());
            break;
        case GT_ARR_LENGTH:
            morphAssertionGenerateNotNull(tree->AsArrLen()->GetArray());
            break;
        case GT_ARR_ELEM:
            morphAssertionGenerateNotNull(tree->AsArrElem()->GetArray());
            break;

        case GT_CALL:
        {
            // A virtual call can create a non-null assertion. We transform some virtual calls into non-virtual calls
            // with a GTF_CALL_NULLCHECK flag set.
            // Ignore tail calls because they have 'this` pointer in the regular arg list and an implicit null check.
            GenTreeCall* const call = tree->AsCall();
            if (call->NeedsNullCheck() || (call->IsVirtual() && !call->IsTailCall()))
            {
                morphAssertionGenerateNotNull(call->GetThisArg());
            }
        }
        break;

        default:
            break;
    }
}

const MorphAssertion* Compiler::morphAssertionFindRange(unsigned lclNum)
{
    for (unsigned index = 0; index < morphAssertionCount; index++)
    {
        const MorphAssertion& assertion = morphAssertionGet(index);

        if ((assertion.kind == Kind::Equal) && (assertion.lcl.lclNum == lclNum))
        {
            return assertion.valKind == ValueKind::Range ? &assertion : nullptr;
        }
    }

    return nullptr;
}

bool Compiler::morphAssertionIsTypeRange(GenTreeLclVar* lclVar, var_types type)
{
    assert(fgGlobalMorph);

    // For now morph only needs this to eliminate some small int casts.
    // TODO-MIKE-Review: Check why BOOL is ignored, it behaves like UBYTE when used with casts.
    if (!varTypeIsSmallInt(type))
    {
        return false;
    }

    const MorphAssertion* assertion = morphAssertionFindRange(lclVar->GetLcl()->GetLclNum());

    if (assertion == nullptr)
    {
        return false;
    }

    const auto& typeRange  = GetSmallTypeRange(type);
    const auto& valueRange = assertion->val.range;

    if ((valueRange.min < typeRange.min) || (valueRange.max > typeRange.max))
    {
        return false;
    }

    DBEXEC(verbose, morphAssertionTrace(*assertion, lclVar, "propagated"));

    return true;
}

GenTree* Compiler::morphAssertionPropagateLclVarConst(const MorphAssertion& assertion, GenTreeLclVar* lclVar)
{
    LclVarDsc* lcl = lclVar->GetLcl();

    assert(!lcl->IsAddressExposed());

    if (lcl->lvIsCSE)
    {
        return nullptr;
    }

    const auto& val     = assertion.val;
    GenTree*    conNode = nullptr;

    switch (assertion.valKind)
    {
        case ValueKind::DblCon:
            // There could be a positive zero and a negative zero, so don't propagate zeroes.
            // TODO-MIKE-Review: So what?
            // P.S. This is likely debris from old stupid code that was conflating copy and
            // equality assertions.
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

            if (val.intCon.handleKind != HandleKind::None)
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
            conNode->AsIntCon()->SetHandleKind(val.intCon.handleKind);
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

GenTree* Compiler::morphAssertionPropagateLclVarCopy(const MorphAssertion& assertion, GenTreeLclVar* lclVar)
{
    assert((assertion.kind == Kind::Equal) && (assertion.valKind == ValueKind::LclVar));

    unsigned lclNumDst = assertion.lcl.lclNum;
    unsigned lclNumSrc = assertion.val.lcl.lclNum;

    assert(lclNumDst != lclNumSrc);

    unsigned lclNum = lclVar->GetLcl()->GetLclNum();

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

    int score = (lclNumDst == lclNum) ? 1 : -1;

#ifdef TARGET_X86
    // TODO-MIKE-CQ: This avoids replacing a DOUBLE local with a parameter because such parameters
    // aren't 8 byte aligned on x86. It's not clear how useful is this. In terms of code size this
    // turns out to make things worse and the cost of unaligned access is usually small on modern
    // CPUs. And the overall approach is dubious anyway since such parameters can be enregistered.
    // If spilling is needed then it would make more sense to just spill into a local slot instead
    // of the parameter's home. See also VN copy prop and optAddCopies.
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

    lclVar->SetLcl(lclCopy);

    DBEXEC(verbose, morphAssertionTrace(assertion, lclVar, "propagated"));

    return lclVar;
}

GenTree* Compiler::morphAssertionPropagateLclVar(GenTreeLclVar* lclVar)
{
    assert(lclVar->OperIs(GT_LCL_VAR));

    // GTF_DONT_CSE is also used to block constant/copy propagation, not just CSE.
    if ((lclVar->gtFlags & GTF_DONT_CSE) != 0)
    {
        return nullptr;
    }

    LclVarDsc* lcl = lclVar->GetLcl();

    if (lcl->IsAddressExposed())
    {
        return nullptr;
    }

    unsigned lclNum = lcl->GetLclNum();

    for (unsigned index = 0; index < morphAssertionCount; ++index)
    {
        const MorphAssertion& assertion = morphAssertionGet(index);

        if (assertion.kind != Kind::Equal)
        {
            continue;
        }

        if (assertion.valKind == ValueKind::LclVar)
        {
            GenTree* newTree = morphAssertionPropagateLclVarCopy(assertion, lclVar);

            if (newTree != nullptr)
            {
                return newTree;
            }

            continue;
        }

        if (assertion.lcl.lclNum == lclNum)
        {
            // TODO-MIKE-CQ: This is dubious, it tends to block constant prop for small int locals.
            if (lclVar->GetType() == lcl->GetType())
            {
                return morphAssertionPropagateLclVarConst(assertion, lclVar);
            }
        }
    }

    return nullptr;
}

GenTree* Compiler::morphAssertionPropagateLclFld(GenTreeLclFld* lclFld)
{
    assert(lclFld->OperIs(GT_LCL_FLD));

    // GTF_DONT_CSE is also used to block constant/copy propagation, not just CSE.
    if ((lclFld->gtFlags & GTF_DONT_CSE) != 0)
    {
        return nullptr;
    }

    LclVarDsc* lcl = lclFld->GetLcl();

    if (lcl->IsAddressExposed() || !lcl->TypeIs(TYP_STRUCT))
    {
        return nullptr;
    }

    unsigned lclNum = lcl->GetLclNum();

    for (unsigned index = 0; index < morphAssertionCount; ++index)
    {
        const MorphAssertion& assertion = morphAssertionGet(index);

        if ((assertion.lcl.lclNum != lclNum) || (assertion.kind != Kind::Equal))
        {
            continue;
        }

        if (assertion.valKind == ValueKind::IntCon)
        {
            assert(assertion.val.intCon.value == 0);

            if (varTypeIsFloating(lclFld->GetType()))
            {
                return lclFld->ChangeToDblCon(lclFld->GetType(), 0);
            }

#ifdef FEATURE_SIMD
            if (varTypeIsSIMD(lclFld->GetType()))
            {
                return gtNewZeroSimdHWIntrinsicNode(lclFld->GetLayout(this));
            }
#endif

#ifndef TARGET_64BIT
            if (varTypeIsLong(lclFld->GetType()))
            {
                return lclFld->ChangeToLngCon(0);
            }
#endif
            assert(varTypeIsIntegralOrI(lclFld->GetType()) || lclFld->TypeIs(TYP_STRUCT));

            return lclFld->ChangeToIntCon(lclFld->TypeIs(TYP_STRUCT) ? TYP_INT : lclFld->GetType(), 0);
        }

        if (assertion.valKind == ValueKind::LclVar)
        {
            LclVarDsc* lclNumCopySrcLcl = lvaGetDesc(assertion.val.lcl.lclNum);

            assert(lclNumCopySrcLcl->TypeIs(TYP_STRUCT));
            assert(!lclNumCopySrcLcl->IsIndependentPromoted());

            lclFld->SetLcl(lclNumCopySrcLcl);
            lvaSetDoNotEnregister(lclNumCopySrcLcl DEBUGARG(DNER_LocalField));

            return lclFld;
        }

        break;
    }

    return nullptr;
}

GenTree* Compiler::morphAssertionPropagateRelOp(GenTreeOp* relop)
{
    assert(relop->OperIs(GT_EQ, GT_NE));

    GenTree* op1 = relop->GetOp(0);
    GenTree* op2 = relop->GetOp(1);

    if (!op1->OperIs(GT_LCL_VAR) || !op2->IsIntCon())
    {
        return nullptr;
    }

    assert(varTypeIsIntegralOrI(op1->GetType()));

    LclVarDsc* lcl = op1->AsLclVar()->GetLcl();

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

    unsigned lclNum = lcl->GetLclNum();
    int      result = -1;

    for (unsigned index = 0; index < morphAssertionCount; ++index)
    {
        const MorphAssertion& assertion = morphAssertionGet(index);

        if ((assertion.lcl.lclNum != lclNum) || (assertion.valKind != ValueKind::IntCon))
        {
            continue;
        }

        if (assertion.kind == Kind::Equal)
        {
            result = relop->OperIs(GT_EQ) == (value == (assertion.val.intCon.value & valueMask)) ? 1 : 0;
            DBEXEC(verbose, morphAssertionTrace(assertion, op2, "propagated"));
            break;
        }

        assert(assertion.kind == Kind::NotNull);
        assert(assertion.val.intCon.value == 0);

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

GenTree* Compiler::morphAssertionPropagateCast(GenTreeCast* cast)
{
    GenTree*  src      = cast->GetOp(0);
    var_types fromType = src->GetType();
    var_types toType   = cast->GetCastType();

    if (!varActualTypeIsInt(toType) || (toType == TYP_BOOL) || !varTypeIsIntegral(fromType))
    {
        return nullptr;
    }

    GenTree* actualSrc = src->SkipComma();

    if (!actualSrc->OperIs(GT_LCL_VAR))
    {
        return nullptr;
    }

    LclVarDsc* lcl = actualSrc->AsLclVar()->GetLcl();

    if (lcl->IsAddressExposed() || varTypeIsLong(lcl->GetType()))
    {
        return nullptr;
    }

    const MorphAssertion* assertion = morphAssertionFindRange(actualSrc->AsLclVar()->GetLcl()->GetLclNum());

    if (assertion == nullptr)
    {
        return nullptr;
    }

    if (cast->IsUnsigned())
    {
        fromType = varTypeToUnsigned(fromType);
    }

    if (varTypeIsUnsigned(fromType) && (assertion->val.range.min < 0))
    {
        return nullptr;
    }

    if (varTypeIsSmallInt(toType))
    {
        if ((assertion->val.range.min < GetSmallTypeRange(toType).min) ||
            (assertion->val.range.max > GetSmallTypeRange(toType).max))
        {
            return nullptr;
        }
    }
    else if (toType == TYP_UINT)
    {
        if (assertion->val.range.min < 0)
        {
            return nullptr;
        }
    }
    else
    {
        assert(toType == TYP_INT);
    }

    if (!lcl->lvNormalizeOnLoad())
    {
        DBEXEC(verbose, morphAssertionTrace(*assertion, cast, "propagated"));

        return src;
    }

    // TODO-MIKE-CQ: It's not entirely clear what the problem with lvNormalizeOnLoad is here.
    // Old code tried to handle this case but it was broken and removing it produced no diffs.
    // This happens when a small int promoted field or param local is casted to another small
    // int type, which isn't exactly common. See morph-assertion-short-param-byte-cast.cs.
    // There could also be an overflow checking cast to UINT that can be removed if we know
    // that the value store in the local is positive.
    // This needs care in the case of promoted struct fields because we don't know if they're
    // P-DEP or not yet. If they're P-DEP then there will be an implicit truncation on store
    // that range generation currently ignores. osx-arm64 may also have this problem.

    return nullptr;
}

GenTree* Compiler::morphAssertionPropagateIndir(GenTreeIndir* indir)
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

    if (!morphAssertionIsNotNull(addr->AsLclVar()))
    {
        return nullptr;
    }

    indir->gtFlags &= ~GTF_EXCEPT;
    indir->gtFlags |= GTF_IND_NONFAULTING;
    // Set this flag to prevent reordering
    indir->gtFlags |= GTF_ORDER_SIDEEFF;

    return indir;
}

bool Compiler::morphAssertionIsNotNull(GenTreeLclVar* lclVar)
{
    assert(fgGlobalMorph);

    LclVarDsc* lcl    = lclVar->GetLcl();
    unsigned   lclNum = lcl->GetLclNum();

    for (unsigned index = 0; index < morphAssertionCount; index++)
    {
        const MorphAssertion& assertion = morphAssertionGet(index);

        if ((assertion.kind == Kind::NotNull) && (assertion.lcl.lclNum == lclNum))
        {
            assert(assertion.valKind == ValueKind::IntCon);
            assert(assertion.val.intCon.value == 0);
            assert(!lcl->IsAddressExposed());

            DBEXEC(verbose, morphAssertionTrace(assertion, lclVar, "propagated"));

            return true;
        }
    }

    return false;
}

GenTree* Compiler::morphAssertionPropagateCall(GenTreeCall* call)
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

    if (!morphAssertionIsNotNull(op1->AsLclVar()))
    {
        return nullptr;
    }

    call->gtFlags &= ~GTF_CALL_NULLCHECK;
    call->gtFlags &= ~GTF_EXCEPT;

    noway_assert(call->gtFlags & GTF_SIDE_EFFECT);

    return call;
}

GenTree* Compiler::morphAssertionPropagate(GenTree* tree)
{
    assert(fgGlobalMorph);

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
                newTree = morphAssertionPropagateLclVar(tree->AsLclVar());
                break;
            case GT_LCL_FLD:
                newTree = morphAssertionPropagateLclFld(tree->AsLclFld());
                break;
            case GT_OBJ:
            case GT_STORE_OBJ:
            case GT_BLK:
            case GT_STORE_BLK:
            case GT_IND:
            case GT_STOREIND:
            case GT_NULLCHECK:
                newTree = morphAssertionPropagateIndir(tree->AsIndir());
                break;
            case GT_CAST:
                newTree = morphAssertionPropagateCast(tree->AsCast());
                break;
            case GT_CALL:
                newTree = morphAssertionPropagateCall(tree->AsCall());
                break;
            case GT_EQ:
            case GT_NE:
                newTree = morphAssertionPropagateRelOp(tree->AsOp());
                break;
            default:
                newTree = nullptr;
                break;
        }
    } while (newTree != nullptr);

    return tree;
}

void Compiler::morphAssertionKillSingle(unsigned lclNum DEBUGARG(GenTreeLclVarCommon* store))
{
    assert(store->OperIs(GT_STORE_LCL_VAR, GT_STORE_LCL_FLD));

    BitVec& killed = morphAssertionGetDependent(lclNum);

    for (unsigned count = morphAssertionCount; !DepBitVecOps::IsEmpty(this, killed) && (count > 0); count--)
    {
        if (!DepBitVecOps::IsMember(this, killed, count - 1))
        {
            continue;
        }

        const MorphAssertion& assertion = morphAssertionGet(count - 1);

        assert((assertion.lcl.lclNum == lclNum) ||
               ((assertion.valKind == ValueKind::LclVar) && (assertion.val.lcl.lclNum == lclNum)));

        DBEXEC(verbose, morphAssertionTrace(assertion, store, "killed"));

        morphAssertionRemove(count - 1);
    }

    assert(DepBitVecOps::IsEmpty(this, killed));
}

void Compiler::morphAssertionKill(LclVarDsc* lcl DEBUGARG(GenTreeLclVarCommon* store))
{
    assert(fgGlobalMorph);

    morphAssertionKillSingle(lcl->GetLclNum() DEBUGARG(store));

    if (lcl->IsPromoted())
    {
        for (unsigned i = 0; i < lcl->GetPromotedFieldCount(); ++i)
        {
            morphAssertionKillSingle(lcl->GetPromotedFieldLclNum(i) DEBUGARG(store));
        }
    }
    else if (lcl->IsPromotedField())
    {
        morphAssertionKillSingle(lcl->GetPromotedFieldParentLclNum() DEBUGARG(store));
    }
}

#endif // LOCAL_ASSERTION_PROP
