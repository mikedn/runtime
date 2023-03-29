// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

static bool IntAddOverflows(int x, int y)
{
    return (x > 0 && y > 0 && y > INT_MAX - x) || (x < 0 && y < 0 && x < INT_MIN - y);
}

struct Limit
{
    enum class Kind
    {
        Undef,     // The limit is yet to be computed.
        VN,        // The limit is a value number + constant value.
        Const,     // The limit is a constant value.
        Dependent, // The limit is depends on some other value.
        Unknown    // The limit could not be determined.
    };

private:
    Kind     kind;
    ValueNum vn;
    int      cns;

public:
    Limit() : kind(Kind::Undef)
    {
    }

    Limit(Kind kind) : kind(kind)
    {
        assert((kind == Kind::Undef) || (kind == Kind::Dependent) || (kind == Kind::Unknown));
    }

    Limit(int cns) : kind(Kind::Const), vn(NoVN), cns(cns)
    {
    }

    Limit(ValueNum vn, int cns) : kind(Kind::VN), vn(vn), cns(cns)
    {
    }

    bool IsUndef() const
    {
        return kind == Kind::Undef;
    }

    bool IsDependent() const
    {
        return kind == Kind::Dependent;
    }

    bool IsUnknown() const
    {
        return kind == Kind::Unknown;
    }

    bool IsConstant() const
    {
        return kind == Kind::Const;
    }

    bool IsVN() const
    {
        return kind == Kind::VN;
    }

    Kind GetKind() const
    {
        return kind;
    }

    ValueNum GetVN() const
    {
        return vn;
    }

    int GetConstant() const
    {
        assert(IsConstant() || IsVN());
        return cns;
    }

    bool AddConstant(int c)
    {
        if (IsVN() || IsConstant())
        {
            if (IntAddOverflows(cns, c))
            {
                return false;
            }

            cns += c;
            return true;
        }

        return IsDependent();
    }

    Limit operator+(int c) const
    {
        Limit result = *this;
        return result.AddConstant(c) ? result : Limit::Kind::Unknown;
    }

    bool operator==(const Limit& l) const
    {
        switch (kind)
        {
            case Kind::Undef:
            case Kind::Unknown:
            case Kind::Dependent:
                return l.kind == kind;
            case Kind::VN:
                return l.kind == kind && l.vn == vn && l.cns == cns;
            case Kind::Const:
                return l.kind == kind && l.cns == cns;
            default:
                return false;
        }
    }
};

struct Range
{
    Limit min;
    Limit max;

    Range() = default;

    Range(const Limit& limit) : min{limit}, max{limit}
    {
    }

    Range(const Limit& min, const Limit& max) : min{min}, max{max}
    {
    }
};

static Range Add(const Range& r1, const Range& r2)
{
    Range result = Limit::Kind::Unknown;

    const Limit& min1 = r1.min;
    const Limit& min2 = r2.min;

    // TODO-MIKE-Review: This is dubious. Old code had lo1.IsDependent() && !lo1.IsUnknown(),
    // with the Unknown check being obviously pointless. But was that just bogus code or was
    // it actually broken and needs to be lo1.IsDependent() && !lo2.IsUnknown()? It would
    // make sense for Unknown to act as "bottom"...

    if (min1.IsDependent() || min2.IsDependent())
    {
        result.min = Limit::Kind::Dependent;
    }
    else if (min1.IsConstant())
    {
        result.min = min2 + min1.GetConstant();
    }
    else if (min2.IsConstant())
    {
        result.min = min1 + min2.GetConstant();
    }

    const Limit& max1 = r1.max;
    const Limit& max2 = r2.max;

    if (max1.IsDependent() || max2.IsDependent())
    {
        result.max = Limit::Kind::Dependent;
    }
    else if (max1.IsConstant())
    {
        result.max = max2 + max1.GetConstant();
    }
    else if (max2.IsConstant())
    {
        result.max = max1 + max2.GetConstant();
    }

    return result;
}

static Range Merge(const Range& r1, const Range& r2, bool monotonicallyIncreasing)
{
    Range result = Limit::Kind::Unknown;

    const Limit& min1 = r1.min;
    const Limit& min2 = r2.min;

    if (!min1.IsUnknown() && !min2.IsUnknown())
    {
        if (min1.IsUndef())
        {
            result.min = min2;
        }
        else if (min1.IsDependent() || min2.IsDependent())
        {
            if (monotonicallyIncreasing)
            {
                result.min = min1.IsDependent() ? min2 : min1;
            }
            else
            {
                result.min = Limit::Kind::Dependent;
            }
        }
        else if (min1.IsConstant() && min2.IsConstant())
        {
            result.min = Limit(Min(min1.GetConstant(), min2.GetConstant()));
        }
        else if (min1 == min2)
        {
            result.min = min1;
        }
    }

    const Limit& max1 = r1.max;
    const Limit& max2 = r2.max;

    if (!max1.IsUnknown() && !max2.IsUnknown())
    {
        if (max1.IsUndef())
        {
            result.max = max2;
        }
        else if (max1.IsDependent() || max2.IsDependent())
        {
            result.max = Limit::Kind::Dependent;
        }
        else if (max1.IsConstant() && max2.IsConstant())
        {
            result.max = Limit(Max(max1.GetConstant(), max2.GetConstant()));
        }
        else if (max1 == max2)
        {
            result.max = max1;
        }
        // Widen Upper Limit => Max(k, (a.len + n)) yields (a.len + n),
        // This is correct if k >= 0 and n >= k, since a.len always >= 0
        // (a.len + n) could overflow, but the result (a.len + n) also
        // preserves the overflow.
        else if (max1.IsConstant() && max2.IsVN() && (max1.GetConstant() >= 0) &&
                 (max1.GetConstant() <= max2.GetConstant()))
        {
            result.max = max2;
        }
        else if (max1.IsVN() && max2.IsConstant() && (max2.GetConstant() >= 0) &&
                 (max1.GetConstant() >= max2.GetConstant()))
        {
            result.max = max1;
        }
        else if (max1.IsVN() && max2.IsVN() && (max1.GetVN() == max2.GetVN()))
        {
            result.max = max2.GetConstant() > max1.GetConstant() ? max2 : max1;
        }
    }

    return result;
}

class RangeCheck
{
    static constexpr int MaxSearchDepth = 100;
    static constexpr int MaxVisitBudget = 8192;
    // TODO-MIKE-Review: The maximum array length is supposedly smaller, that could
    // help with certain range check elimination cases.
    // https://learn.microsoft.com/en-us/dotnet/api/system.array?view=net-8.0 says:
    // "The array size is limited to a total of 4 billion elements, and to a maximum
    // index of 0X7FEFFFFF in any given dimension (0X7FFFFFC7 for byte arrays and
    // arrays of single-byte structures)."
    static constexpr int MaxArrayLength = 0x7FFFFFFF;

    using OverflowMap = JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, bool>;
    using RangeMap    = JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, Range>;
    using SearchPath  = JitHashSet<GenTree*, JitPtrKeyFuncs<GenTree>>;

    ValueNumStore* vnStore;
    RangeMap       rangeMap;
    SearchPath     searchPath;
    Compiler*      compiler;
    CompAllocator  alloc;
    ValueNum       currentLengthVN         = NoVN;
    int            budget                  = MaxVisitBudget;
    bool           monotonicallyIncreasing = false;

public:
    RangeCheck(Compiler* compiler)
        : vnStore(compiler->vnStore)
        , rangeMap(compiler->getAllocator(CMK_RangeCheck))
        , searchPath(compiler->getAllocator(CMK_RangeCheck))
        , compiler(compiler)
        , alloc(compiler->getAllocator(CMK_RangeCheck))
    {
    }
    void OptimizeRangeChecks();

private:
    bool OptimizeRangeCheck(BasicBlock* block, GenTreeBoundsChk* boundsChk);
    bool IsInBounds(const Range& range, GenTree* lengthExpr, int lengthVal);
    Range GetRange(BasicBlock* block, GenTree* expr);
    Range ComputeRange(BasicBlock* block, GenTree* expr);
    Range ComputeLclUseRange(BasicBlock* block, GenTreeLclUse* use);
    Range ComputeBinOpRange(GenTreeOp* expr);
    Range ComputeAddRange(BasicBlock* block, GenTreeOp* add);
    Range ComputePhiRange(BasicBlock* block, GenTreePhi* phi);
    void MergePhiArgAssertions(BasicBlock* block, GenTreeLclUse* use, Range* range);
    void MergeLclUseAssertions(BasicBlock* block, GenTreeLclUse* use, Range* range);
    void MergeEdgeAssertions(ValueNum vn, const ASSERT_TP assertions, Range* range);
    int GetArrayLength(ValueNum vn);
    bool GetLimitMax(const Limit& limit, int* max);
    bool AddOverflows(const Limit& limit1, const Limit& limit2);
    bool ComputeOverflow();
    void Widen(BasicBlock* block, GenTree* expr, Range* range);
    bool IsAddMonotonicallyIncreasing(GenTreeOp* expr);
    bool IsPhiMonotonicallyIncreasing(GenTreePhi* phi, bool rejectNegativeConst);
    bool IsMonotonicallyIncreasing(GenTree* expr, bool rejectNegativeConst);

#ifdef DEBUG
    const char* ToString(const Limit& limit)
    {
        constexpr size_t size = 64;
        char*            buf  = compiler->getAllocator(CMK_DebugOnly).allocate<char>(size);

        switch (limit.GetKind())
        {
            size_t len;
            int    c;

            case Limit::Kind::Undef:
                return "Undef";
            case Limit::Kind::Unknown:
                return "Unknown";
            case Limit::Kind::Dependent:
                return "Dependent";
            case Limit::Kind::Const:
                sprintf_s(buf, size, "%d", limit.GetConstant());
                return buf;
            case Limit::Kind::VN:
                len = sprintf_s(buf, size, FMT_VN, limit.GetVN());
                c   = limit.GetConstant();
                if (c != 0)
                {
                    sprintf_s(buf + len, size - len, "%c%d", c < 0 ? '-' : '+', abs(c));
                }
                return buf;
            default:
                return "???";
        }
    }

    const char* ToString(const Range& range)
    {
        constexpr size_t size = 64;
        char*            buf  = compiler->getAllocator(CMK_DebugOnly).allocate<char>(size);
        sprintf_s(buf, size, "[%s..%s]", ToString(range.min), ToString(range.max));
        return buf;
    }
#endif
};

int RangeCheck::GetArrayLength(ValueNum vn)
{
    return vnStore->GetNewArrSize(vnStore->GetArrForLenVn(vn));
}

bool RangeCheck::IsInBounds(const Range& range, GenTree* lengthExpr, int lengthVal)
{
    ValueNum upperLimitVN = vnStore->VNNormalValue(lengthExpr->GetConservativeVN());

    JITDUMP("InBounds: %s in [0, " FMT_VN "]\n", ToString(range), upperLimitVN);
    JITDUMP("InBounds: length " FMT_VN " is: ", upperLimitVN);
    DBEXEC(compiler->verbose, vnStore->vnDump(compiler, upperLimitVN));
    JITDUMP("\n");

    if ((lengthVal <= 0) && !vnStore->IsVNCheckedBound(upperLimitVN))
    {
        return false;
    }

    JITDUMP("InBounds: length value is: %d\n", lengthVal);

    if (range.max.IsVN())
    {
        if (range.max.GetVN() != upperLimitVN)
        {
            return false;
        }

        int ucns = range.max.GetConstant();

        if (ucns >= 0)
        {
            return false;
        }

        if (range.min.IsConstant() && (range.min.GetConstant() >= 0))
        {
            return true;
        }

        if (lengthVal <= 0)
        {
            return false;
        }

        if (range.min.IsVN())
        {
            int lcns = range.min.GetConstant();

            return (lcns < 0) && (-lcns <= lengthVal) && (range.min.GetVN() == upperLimitVN) && (lcns <= ucns);
        }
    }
    else if (range.max.IsConstant())
    {
        if (lengthVal <= 0)
        {
            return false;
        }

        int ucns = range.max.GetConstant();

        if (ucns >= lengthVal)
        {
            return false;
        }

        if (range.min.IsConstant())
        {
            int lcns = range.min.GetConstant();

            return (lcns >= 0) && (lcns <= ucns);
        }

        if (range.min.IsVN())
        {
            int lcns = range.min.GetConstant();

            return (lcns < 0) && (-lcns <= lengthVal) && (range.min.GetVN() == upperLimitVN) &&
                   (lengthVal + lcns <= ucns);
        }
    }

    return false;
}

bool RangeCheck::OptimizeRangeCheck(BasicBlock* block, GenTreeBoundsChk* boundsChk)
{
    GenTree* indexExpr  = boundsChk->GetIndex();
    ValueNum indexVN    = vnStore->VNNormalValue(indexExpr->GetConservativeVN());
    GenTree* lengthExpr = boundsChk->GetLength();
    ValueNum lengthVN   = vnStore->VNNormalValue(lengthExpr->GetConservativeVN());
    int      lengthVal  = 0;

    currentLengthVN = lengthVN;

    if (vnStore->IsVNConstant(lengthVN))
    {
        ssize_t      constVal   = -1;
        GenTreeFlags constFlags = GTF_EMPTY;

        if (vnStore->IsVNIntegralConstant(lengthVN, &constVal, &constFlags))
        {
            lengthVal = static_cast<int>(constVal);
        }
    }
    else
    {
        lengthVal = GetArrayLength(lengthVN);

        // If we can't find the length, see if there are any assertions
        // about the length that we can use to get a minimum length.
        if ((lengthVal <= 0) && (compiler->GetAssertionCount() != 0))
        {
            JITDUMP("Optimize: Looking for length " FMT_VN " assertions\n", lengthVN);

            Range lengthRange = Limit::Kind::Dependent;
            MergeEdgeAssertions(lengthVN, block->bbAssertionIn, &lengthRange);

            if (lengthRange.min.IsConstant())
            {
                lengthVal = lengthRange.min.GetConstant();
            }
        }
    }

    JITDUMP("Optimize: " FMT_VN " value %d\n", lengthVN, lengthVal);

    if (vnStore->IsVNConstant(indexVN) && (lengthVal > 0))
    {
        ssize_t      constVal   = -1;
        GenTreeFlags constFlags = GTF_EMPTY;

        if (!vnStore->IsVNIntegralConstant(indexVN, &constVal, &constFlags))
        {
            return false;
        }

        JITDUMP("Optimize: Constant index %d " FMT_VN " in [0, %d " FMT_VN ").\n", constVal, indexVN, lengthVal,
                lengthVN);

        if ((0 <= constVal) && (constVal < lengthVal))
        {
            return true;
        }
    }

    // TODO-CQ: The current implementation is reliant on integer storage types
    // for constants. It could use INT64. Still, representing ULONG constants
    // might require preserving the var_type whether it is a un/signed 64-bit.
    // JIT64 doesn't do anything for "long" either. No asm diffs.
    if (varActualType(indexExpr->GetType()) != TYP_INT)
    {
        JITDUMP("Optimize: Unsupported index type %s\n", varTypeName(indexExpr->GetType()));

        return false;
    }

    monotonicallyIncreasing = false;
    rangeMap.RemoveAll();
    Range range = GetRange(block, indexExpr);

    if (range.max.IsUnknown() || range.min.IsUnknown() || ComputeOverflow())
    {
        return false;
    }

    if (range.min.IsDependent())
    {
        Widen(block, indexExpr, &range);
    }

    return !range.max.IsUnknown() && !range.min.IsUnknown() && IsInBounds(range, lengthExpr, lengthVal);
}

void RangeCheck::Widen(BasicBlock* block, GenTree* expr, Range* range)
{
    JITDUMP("Widen: " FMT_BB " [%06u] %s\n", block->bbNum, expr->GetID(), ToString(*range));

    searchPath.Clear();

    if (IsMonotonicallyIncreasing(expr, false))
    {
        JITDUMP("Widen: " FMT_BB " [%06u] is monotonically increasing.\n", block->bbNum, expr->GetID());

        monotonicallyIncreasing = true;
        rangeMap.RemoveAll();
        *range = GetRange(block, expr);
    }
}

bool RangeCheck::IsAddMonotonicallyIncreasing(GenTreeOp* expr)
{
    assert(expr->OperIs(GT_ADD));

    GenTree* op1 = expr->GetOp(0);
    GenTree* op2 = expr->GetOp(1);

    if (op2->IsLclUse())
    {
        std::swap(op1, op2);
    }

    if (!op1->IsLclUse())
    {
        JITDUMP("Not monotonically increasing because op1 is not LCL_USE.\n");
        return false;
    }

    if (op2->IsLclUse())
    {
        return IsMonotonicallyIncreasing(op1, true) && IsMonotonicallyIncreasing(op2, true);
    }

    if (GenTreeIntCon* con = op2->IsIntCon())
    {
        return (con->GetValue() >= 0) && IsMonotonicallyIncreasing(op1, false);
    }

    JITDUMP("Not monotonically increasing because expression is not recognized.\n");

    return false;
}

bool RangeCheck::IsPhiMonotonicallyIncreasing(GenTreePhi* phi, bool rejectNegativeConst)
{
    for (GenTreePhi::Use& use : phi->Uses())
    {
        if (searchPath.Contains(use.GetNode()))
        {
            continue;
        }

        if (!IsMonotonicallyIncreasing(use.GetNode(), rejectNegativeConst))
        {
            return false;
        }
    }

    return true;
}

bool RangeCheck::IsMonotonicallyIncreasing(GenTree* expr, bool rejectNegativeConst)
{
    JITDUMP("MonotonicallyIncreasing: ");
    DBEXEC(compiler->verbose, compiler->gtDispTree(expr, nullptr, nullptr, true));

    if (!searchPath.Add(expr))
    {
        return true;
    }

    bool monotonicallyIncreasing = false;

    if (searchPath.GetCount() > MaxSearchDepth)
    {
        monotonicallyIncreasing = false;
    }
    else if (vnStore->IsVNInt32Constant(expr->GetConservativeVN()))
    {
        monotonicallyIncreasing = !rejectNegativeConst || (vnStore->ConstantValue<int>(expr->GetConservativeVN()) >= 0);
    }
    else if (expr->OperIs(GT_COMMA))
    {
        monotonicallyIncreasing = IsMonotonicallyIncreasing(expr->gtEffectiveVal(), rejectNegativeConst);
    }
    else if (expr->OperIs(GT_ADD))
    {
        monotonicallyIncreasing = IsAddMonotonicallyIncreasing(expr->AsOp());
    }
    else if (GenTreeLclUse* use = expr->IsLclUse())
    {
        monotonicallyIncreasing = IsMonotonicallyIncreasing(use->GetDef()->GetValue(), rejectNegativeConst);
    }
    else if (GenTreePhi* phi = expr->IsPhi())
    {
        monotonicallyIncreasing = IsPhiMonotonicallyIncreasing(phi, rejectNegativeConst);
    }

    searchPath.Remove(expr);

    return monotonicallyIncreasing;
}

bool RangeCheck::GetLimitMax(const Limit& limit, int* max)
{
    if (limit.IsConstant())
    {
        *max = limit.GetConstant();

        return true;
    }

    if (limit.IsVN())
    {
        int len = GetArrayLength(limit.GetVN());

        if (len <= 0)
        {
            len = MaxArrayLength;
        }

        if (IntAddOverflows(len, limit.GetConstant()))
        {
            return false;
        }

        *max = len + limit.GetConstant();

        return true;
    }

    return false;
}

bool RangeCheck::AddOverflows(const Limit& limit1, const Limit& limit2)
{
    int max1;
    int max2;
    return !GetLimitMax(limit1, &max1) || !GetLimitMax(limit2, &max2) || IntAddOverflows(max1, max2);
}

bool RangeCheck::ComputeOverflow()
{
    for (const auto& pair : rangeMap)
    {
        GenTree* node = pair.key;

        JITDUMP("Overflow: ");
        DBEXEC(compiler->verbose, compiler->gtDispTree(node, nullptr, nullptr, true));

        if (node->OperIs(GT_ADD))
        {
            const Range* r1 = rangeMap.LookupPointer(node->AsOp()->GetOp(0));
            const Range* r2 = rangeMap.LookupPointer(node->AsOp()->GetOp(1));

            if (AddOverflows(r1->max, r2->max))
            {
                JITDUMP("Overflow: [%06u] overflows\n", node->GetID());

                return true;
            }
        }
        else if (!node->OperIs(GT_COMMA, GT_IND, GT_LCL_USE, GT_LCL_DEF, GT_PHI, GT_AND, GT_UMOD, GT_LSH, GT_RSH) &&
                 !vnStore->IsVNConstant(node->GetConservativeVN()))
        {
            JITDUMP("Overflow: [%06u] overflows\n", node->GetID());

            return true;
        }

        JITDUMP("Overflow: [%06u] no overflow\n", node->GetID());
    }

    return false;
}

void RangeCheck::MergeEdgeAssertions(ValueNum vn, const ASSERT_TP assertions, Range* range)
{
    if (vn == NoVN)
    {
        return;
    }

    BitVecTraits apTraits(compiler->GetAssertionCount(), compiler);
    for (BitVecOps::Enumerator en(&apTraits, assertions); en.MoveNext();)
    {
        BoundsAssertion assertion = compiler->apGetBoundsAssertion(en.Current());

        if (!assertion.IsBoundsAssertion())
        {
            continue;
        }

        Limit      limit;
        genTreeOps cmpOper             = GT_NONE;
        bool       isConstantAssertion = false;

        // Current assertion is of the form (i < len - cns) != 0
        if (assertion.IsCompareCheckedBoundArith())
        {
            ValueNumStore::CompareCheckedBoundArithInfo info;

            VNFuncApp funcApp;
            vnStore->GetVNFunc(assertion.GetVN(), &funcApp);
            vnStore->GetCompareCheckedBoundArithInfo(funcApp, &info);
            assert((info.arrOper == GT_ADD) || (info.arrOper == GT_SUB));

            if (vn != info.cmpOp)
            {
                continue;
            }

            if (!vnStore->IsVNInt32Constant(info.arrOp))
            {
                continue;
            }

            int cons = vnStore->ConstantValue<int>(info.arrOp);
            limit    = Limit(info.vnBound, info.arrOper == GT_SUB ? -cons : cons);
            cmpOper  = info.cmpOper;
        }
        // Current assertion is of the form (i < len) != 0
        else if (assertion.IsCompareCheckedBound())
        {
            ValueNumStore::CompareCheckedBoundArithInfo info;

            VNFuncApp funcApp;
            vnStore->GetVNFunc(assertion.GetVN(), &funcApp);
            vnStore->GetCompareCheckedBound(funcApp, &info);

            if (vn == info.cmpOp)
            {
                cmpOper = info.cmpOper;
                limit   = Limit(info.vnBound, 0);
            }
            else if (vn == info.vnBound)
            {
                cmpOper = GenTree::SwapRelop(info.cmpOper);
                limit   = Limit(info.cmpOp, 0);
            }
            else
            {
                continue;
            }
        }
        // Current assertion is of the form i IN [K1..K2]
        else if (assertion.IsRange())
        {
            if (vn != assertion.GetVN())
            {
                continue;
            }

            int max = assertion.GetRangeMax();
            int min = assertion.GetRangeMin();

            // TODO-MIKE-Review: Old code handled only "i < K" like cases,
            // not the more general "i IN [K1..K2]" case. It's likely that
            // we can get useful information from cast related ranges.
            if (max == INT32_MAX)
            {
                limit   = min;
                cmpOper = GT_GE;
            }
            else if (min == INT32_MIN)
            {
                limit   = max;
                cmpOper = GT_LE;
            }
            else
            {
                continue;
            }
        }
        // Current assertion is of the form i == 100
        else if (assertion.IsConstant() && (assertion.GetVN() == vn))
        {
            int cnstLimit = vnStore->CoercedConstantValue<int>(assertion.GetConstantVN());

            if ((cnstLimit == 0) && !assertion.IsEqual() && vnStore->IsVNCheckedBound(assertion.GetVN()))
            {
                // we have arr.Len != 0, so the length must be atleast one
                limit   = 1;
                cmpOper = GT_GE;
            }
            else if (assertion.IsEqual())
            {
                limit   = cnstLimit;
                cmpOper = GT_EQ;
            }
            else
            {
                // We have a != assertion, but it doesn't tell us much about the interval. So just skip it.
                continue;
            }

            isConstantAssertion = true;
        }
        // Current assertion is not supported, ignore it
        else
        {
            continue;
        }

        // Make sure the assertion is of the form != 0 or == 0 if it isn't a constant assertion.
        assert(isConstantAssertion || assertion.IsRange() || (assertion.GetConstantVN() == vnStore->VNForIntCon(0)));

        assert(limit.IsVN() || limit.IsConstant());

        JITDUMP("Assertion: ");
        DBEXEC(compiler->verbose, compiler->apDumpBoundsAssertion(assertion);)

        // Limits are sometimes made with the form vn + constant, where vn is a known constant
        // see if we can simplify this to just a constant
        if (limit.IsVN() && vnStore->IsVNInt32Constant(limit.GetVN()))
        {
            Limit tempLimit = vnStore->ConstantValue<int>(limit.GetVN());

            if (tempLimit.AddConstant(limit.GetConstant()))
            {
                limit = tempLimit;
            }
        }

        ValueNum lengthVN = currentLengthVN;

        if (vnStore->IsVNConstant(lengthVN))
        {
            // Set lengthVN to NoVN; this will make it match the "vn" recorded on
            // constant limits (where we explicitly track the constant and don't
            // redundantly store its VN in the "vn" field).
            lengthVN = NoVN;
        }

        // During assertion prop we add assertions of the form:
        //
        //      (i < length) == 0
        //      (i < length) != 0
        //      (i < 100) == 0
        //      (i < 100) != 0
        //      i == 100
        //
        // At this point, we have detected that either op1.vn is (i < length) or (i < length + cns) or
        // (i < 100) and the op2.vn is 0 or that op1.vn is i and op2.vn is a known constant.
        //
        // Now, let us check if we are == 0 (i.e., op1 assertion is false) or != 0 (op1 assertion
        // is true.).
        //
        // If we have a non-constant assertion of the form == 0 (i.e., equals false), then reverse relop.
        // The relop has to be reversed because we have: (i < length) is false which is the same
        // as (i >= length).
        if (!isConstantAssertion && assertion.IsEqual())
        {
            cmpOper = GenTree::ReverseRelop(cmpOper);
        }

        assert(cmpOper != GT_NONE);

        // Bounds are inclusive, so add -1 for upper bound when "<". But make sure we won't underflow.
        if ((cmpOper == GT_LT) && !limit.AddConstant(-1))
        {
            continue;
        }
        // Bounds are inclusive, so add +1 for lower bound when ">". But make sure we won't overflow.
        if ((cmpOper == GT_GT) && !limit.AddConstant(1))
        {
            continue;
        }

        // Doesn't tighten the current bound. So skip.
        if (range->max.IsConstant() && (limit.GetVN() != lengthVN))
        {
            continue;
        }

        // Check if the incoming limit from assertions tightens the existing upper limit.
        if (range->max.IsVN() && (range->max.GetVN() == lengthVN))
        {
            // We have checked the current range's (pRange's) upper limit is either of the form:
            //      length + cns
            //      and length == the bndsChkCandidate's arrLen
            //
            // We want to check if the incoming limit tightens the bound, and for that
            // we need to make sure that incoming limit is also on the same length (or
            // length + cns) and not some other length.

            if (limit.GetVN() != lengthVN)
            {
                JITDUMP("Assertion: Length VNs did not match: length " FMT_VN ", limit " FMT_VN "\n", lengthVN,
                        limit.GetVN());

                continue;
            }

            int curCns = range->max.GetConstant();
            int limCns = limit.IsVN() ? limit.GetConstant() : 0;

            // Incoming limit doesn't tighten the existing upper limit.
            if (limCns >= curCns)
            {
                JITDUMP("Assertion: Bound limit %d doesn't tighten current bound %d\n", limCns, curCns);
                continue;
            }
        }
        else
        {
            // Current range's upper bound is not "length + cns" and the
            // incoming limit is not on the same length as the bounds check candidate.
            // So we could skip this assertion. But in cases, of Dependent or Unknown
            // type of upper limit, the incoming assertion still tightens the upper
            // bound to a saner value. So do not skip the assertion.
        }

        // cmpOp (loop index i) cmpOper len +/- cns
        switch (cmpOper)
        {
            case GT_LT:
            case GT_LE:
                range->max = limit;
                break;
            case GT_GT:
            case GT_GE:
                range->min = limit;
                break;
            case GT_EQ:
                range->max = limit;
                range->min = limit;
                break;
            default:
                break;
        }

        JITDUMP("Assertion: Range after edge merging: %s\n", ToString(*range));
    }
}

void RangeCheck::MergePhiArgAssertions(BasicBlock* block, GenTreeLclUse* use, Range* range)
{
    if (compiler->GetAssertionCount() == 0)
    {
        return;
    }

    ASSERT_TP   assertions = BitVecOps::UninitVal();
    BasicBlock* pred       = use->GetBlock();

    if (pred->bbFallsThrough() && (pred->bbNext == block))
    {
        assertions = pred->bbAssertionOut;
    }
    else if ((pred->bbJumpKind == BBJ_COND || pred->bbJumpKind == BBJ_ALWAYS) && (pred->bbJumpDest == block))
    {
        assertions = pred->bbAssertionOutJumpDest;
    }

    if (!BitVecOps::MayBeUninit(assertions))
    {
        ValueNum valueVN = vnStore->VNNormalValue(use->GetDef()->GetConservativeVN());

        JITDUMP("Range: Merging assertions for def [%06u] " FMT_VN " from " FMT_BB " predecessor of " FMT_BB "  \n",
                use->GetDef()->GetID(), valueVN, pred->bbNum, block->bbNum);

        MergeEdgeAssertions(valueVN, assertions, range);
    }
}

void RangeCheck::MergeLclUseAssertions(BasicBlock* block, GenTreeLclUse* use, Range* range)
{
    GenTreeLclDef* def        = use->GetDef();
    ASSERT_TP      assertions = block->bbAssertionIn;

    if (!BitVecOps::MayBeUninit(assertions))
    {
        ValueNum valueVN = vnStore->VNNormalValue(def->GetConservativeVN());

        JITDUMP("Range: Merging assertions for def [%06u] " FMT_VN " from predecessors of " FMT_BB "\n", def->GetID(),
                valueVN, block->bbNum);

        MergeEdgeAssertions(valueVN, assertions, range);
    }
}

Range RangeCheck::ComputeLclUseRange(BasicBlock* block, GenTreeLclUse* use)
{
    GenTreeLclDef* def = use->GetDef();

    JITDUMP("Range: " FMT_BB " ", block->bbNum);
    DBEXEC(compiler->verbose, compiler->gtDispTree(def, nullptr, nullptr, true));

    // Uses may perform implicit LONG to INT truncation and possibly
    // other weird conversions such as BYREF to INT, ignore for now.
    if (varActualType(def->GetType()) != TYP_INT)
    {
        return Limit::Kind::Unknown;
    }

    Range range = GetRange(def->GetBlock(), def->GetValue());
    MergeLclUseAssertions(block, use, &range);
    return range;
}

Range RangeCheck::ComputeBinOpRange(GenTreeOp* expr)
{
    assert(expr->OperIs(GT_AND, GT_RSH, GT_LSH, GT_UMOD) && expr->TypeIs(TYP_INT));

    GenTreeIntCon* op2 = expr->GetOp(1)->IsIntCon();

    if (op2 == nullptr)
    {
        return Limit::Kind::Unknown;
    }

    int constLimit = -1;

    if (expr->OperIs(GT_AND))
    {
        constLimit = op2->GetInt32Value();
    }
    else if (expr->OperIs(GT_UMOD))
    {
        constLimit = op2->GetInt32Value() - 1;
    }
    else if (expr->OperIs(GT_RSH, GT_LSH))
    {
        GenTree* op1 = expr->GetOp(0);

        if (op1->OperIs(GT_AND) && op1->AsOp()->GetOp(1)->IsIntCon())
        {
            int mask  = op1->AsOp()->GetOp(1)->AsIntCon()->GetInt32Value();
            int shift = op2->GetInt32Value();

            if ((mask >= 0) && (shift >= 0) && (shift < 32))
            {
                constLimit = expr->OperIs(GT_RSH) ? (mask >> shift) : (mask << shift);
            }
        }
    }

    if (constLimit < 0)
    {
        return Limit::Kind::Unknown;
    }

    return Range(0, constLimit);
}

Range RangeCheck::ComputeAddRange(BasicBlock* block, GenTreeOp* add)
{
    assert(add->OperIs(GT_ADD) && add->TypeIs(TYP_INT));

    GenTree*     op1            = add->GetOp(0);
    const Range* op1RangeCached = rangeMap.LookupPointer(op1);
    Range        op1Range;

    if (op1RangeCached == nullptr)
    {
        op1Range = GetRange(block, op1);
    }
    else if (op1RangeCached->min.IsUndef())
    {
        op1Range = Range(Limit::Kind::Dependent);

        if (GenTreeLclUse* use = op1->IsLclUse())
        {
            MergeLclUseAssertions(block, use, &op1Range);
        }
    }
    else
    {
        op1Range = *op1RangeCached;
    }

    GenTree*     op2            = add->GetOp(1);
    const Range* op2RangeCached = rangeMap.LookupPointer(op2);
    Range        op2Range;

    if (op2RangeCached == nullptr)
    {
        op2Range = GetRange(block, op2);
    }
    else if (op2RangeCached->min.IsUndef())
    {
        op2Range = Range(Limit::Kind::Dependent);

        if (GenTreeLclUse* use = op2->IsLclUse())
        {
            MergeLclUseAssertions(block, use, &op2Range);
        }
    }
    else
    {
        op2Range = *op2RangeCached;
    }

    JITDUMP("Range: ADD(%s, %s)\n", ToString(op1Range), ToString(op2Range));

    return Add(op1Range, op2Range);
}

Range RangeCheck::ComputePhiRange(BasicBlock* block, GenTreePhi* phi)
{
    Range range;

    for (GenTreePhi::Use& use : phi->Uses())
    {
        const Range* cachedRange = rangeMap.LookupPointer(use.GetNode());
        Range        useRange;

        if (cachedRange == nullptr)
        {
            useRange = GetRange(block, use.GetNode());
        }
        else if (cachedRange->min.IsUndef())
        {
            JITDUMP("Range: " FMT_BB " ", block->bbNum);
            DBEXEC(compiler->verbose, compiler->gtDispTree(use.GetNode(), nullptr, nullptr, true));
            JITDUMP("Range: Already being computed\n");

            useRange = Range(Limit::Kind::Dependent);
        }
        else
        {
            useRange = *cachedRange;
        }

        MergePhiArgAssertions(block, use.GetNode(), &useRange);

        JITDUMP("Range: PHI(%s, %s)\n", ToString(range), ToString(useRange));
        range = Merge(range, useRange, monotonicallyIncreasing);
    }

    return range;
}

Range RangeCheck::ComputeRange(BasicBlock* block, GenTree* expr)
{
    assert(varActualType(expr->GetType()) == TYP_INT);

    ValueNum vn = vnStore->VNNormalValue(expr->GetConservativeVN());

    if (vnStore->IsVNConstant(vn))
    {
        return vnStore->TypeOfVN(vn) == TYP_INT ? Limit(vnStore->ConstantValue<int>(vn)) : Limit(Limit::Kind::Unknown);
    }

    if (expr->OperIs(GT_COMMA))
    {
        return GetRange(block, expr->gtEffectiveVal());
    }

    if (expr->OperIs(GT_AND, GT_RSH, GT_LSH, GT_UMOD))
    {
        return ComputeBinOpRange(expr->AsOp());
    }

    if (expr->OperIs(GT_ADD))
    {
        return ComputeAddRange(block, expr->AsOp());
    }

    if (GenTreeLclUse* use = expr->IsLclUse())
    {
        return ComputeLclUseRange(block, use);
    }

    if (GenTreePhi* phi = expr->IsPhi())
    {
        return ComputePhiRange(block, phi);
    }

    switch (expr->GetType())
    {
        case TYP_UBYTE:
            return Range(0, 255);
        case TYP_BYTE:
            return Range(-128, 127);
        case TYP_USHORT:
            return Range(0, 65535);
        case TYP_SHORT:
            return Range(-32768, 32767);
        default:
            return Range(Limit::Kind::Unknown);
    }
}

Range RangeCheck::GetRange(BasicBlock* block, GenTree* expr)
{
    JITDUMP("Range: " FMT_BB " ", block->bbNum);
    DBEXEC(compiler->verbose, compiler->gtDispTree(expr, nullptr, nullptr, true));

    Range* cachedRange = rangeMap.LookupPointer(expr);
    Range  range;

    if (cachedRange == nullptr)
    {
        cachedRange = rangeMap.Emplace(expr, range);
        budget--;
    }
    else if (!cachedRange->min.IsUndef())
    {
        JITDUMP("Range: Cached %s\n", ToString(*cachedRange));

        return *cachedRange;
    }

    if ((budget <= 0) || (rangeMap.GetCount() > MaxSearchDepth))
    {
        JITDUMP("Range: %s exceeded\n", budget <= 0 ? "Budget" : "Depth");

        range = Range(Limit::Kind::Unknown);
    }
    else
    {
        range = ComputeRange(block, expr);

        assert(!range.min.IsUndef() && !range.max.IsUndef());
    }

    *cachedRange = range;

    JITDUMP("Range: " FMT_BB " [%06u] = %s\n", block->bbNum, expr->GetID(), ToString(range));

    return range;
}

void RangeCheck::OptimizeRangeChecks()
{
    for (BasicBlock* block : compiler->Blocks())
    {
        for (Statement* stmt : block->Statements())
        {
            for (GenTree* node : stmt->Nodes())
            {
                if (budget <= 0)
                {
                    return;
                }

                GenTreeOp* comma = nullptr;
                GenTree*   check;

                if (node->OperIs(GT_COMMA))
                {
                    comma = node->AsOp();
                    check = comma->GetOp(0);
                }
                else if (node == stmt->GetRootNode())
                {
                    check = node;
                }
                else
                {
                    continue;
                }

                if (GenTreeBoundsChk* boundsChk = check->IsBoundsChk())
                {
                    if (OptimizeRangeCheck(block, boundsChk))
                    {
                        JITDUMP("Optimize: Removing range check\n");
                        compiler->optRemoveRangeCheck(boundsChk, comma, stmt);
                    }
                }
            }
        }
    }
}

void Compiler::phRemoveRangeCheck()
{
    assert(ssaForm && (vnStore != nullptr));
    DBEXEC(verbose, fgDispBasicBlocks(true))

    RangeCheck rangeCheck(this);
    rangeCheck.OptimizeRangeChecks();
}
