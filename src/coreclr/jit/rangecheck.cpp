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
        Undefined, // The limit is yet to be computed.
        VN,        // The limit is a value number + constant value.
        Constant,  // The limit is a constant value.
        Dependent, // The limit is depends on some other value.
        Unknown    // The limit could not be determined.
    };

private:
    Kind     kind;
    ValueNum vn;
    int      cns;

    Limit(Kind kind, ValueNum vn, int cns) : kind(kind), vn(vn), cns(cns)
    {
    }

public:
    Limit() : kind(Kind::Undefined)
    {
    }

    static Limit Constant(int cns)
    {
        return Limit(Kind::Constant, NoVN, cns);
    }

    static Limit VN(ValueNum vn, int cns = 0)
    {
        return Limit(Kind::VN, vn, cns);
    }

    static Limit Dependent()
    {
        return Limit(Kind::Dependent, NoVN, 0);
    }

    static Limit Unknown()
    {
        return Limit(Kind::Unknown, NoVN, 0);
    }

    bool IsUndefined() const
    {
        return kind == Kind::Undefined;
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
        return kind == Kind::Constant;
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
        assert(IsConstant() || IsVN()); // For constant limits we return NoVN.
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
        return result.AddConstant(c) ? result : Limit::Unknown();
    }

    bool operator==(const Limit& l) const
    {
        switch (kind)
        {
            case Kind::Undefined:
            case Kind::Unknown:
            case Kind::Dependent:
                return l.kind == kind;
            case Kind::VN:
                return l.kind == kind && l.vn == vn && l.cns == cns;
            case Kind::Constant:
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

    Compiler* const      compiler;
    ValueNumStore* const vnStore;
    JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, Range> rangeMap;
    JitHashSet<GenTree*, JitPtrKeyFuncs<GenTree>> searchPath;
    GenTree* currentIndexExpr              = nullptr;
    ValueNum currentLengthVN               = NoVN;
    int      budget                        = MaxVisitBudget;
    int      searchDepth                   = MaxSearchDepth;
    bool     isMonotonicallyIncreasing     = false;
    GenTree* isMonotonicallyIncreasingExpr = nullptr;

public:
    RangeCheck(Compiler* compiler)
        : compiler(compiler)
        , vnStore(compiler->vnStore)
        , rangeMap(compiler->getAllocator(CMK_RangeCheck))
        , searchPath(compiler->getAllocator(CMK_RangeCheck))
    {
    }

    void OptimizeRangeChecks();

private:
    bool OptimizeRangeCheck(BasicBlock* block, GenTreeBoundsChk* boundsChk);
    bool IsInBounds(const Range& range, ValueNum lengthVN, int lengthVal);
    Range* GetRange(BasicBlock* block, GenTree* expr);
    Range ComputeRange(BasicBlock* block, GenTree* expr);
    Range ComputeLclUseRange(BasicBlock* block, GenTreeLclUse* use);
    Range ComputeLeafRange(GenTree* expr) const;
    Range AddRanges(const Range& r1, const Range& r2) const;
    Range ComputeAddRange(BasicBlock* block, GenTreeOp* add);
    Range MergeRanges(const Range& r1, const Range& r2) const;
    Range ComputePhiRange(BasicBlock* block, GenTreePhi* phi);
    void MergePhiArgAssertions(BasicBlock* block, GenTreeLclUse* use, Range* range) const;
    void MergeLclUseAssertions(BasicBlock* block, GenTreeLclUse* use, Range* range) const;
    void MergeEdgeAssertions(ValueNum vn, const ASSERT_TP assertions, Range* range) const;
    int GetArrayLength(ValueNum vn) const;
    bool GetLimitMax(const Limit& limit, int* max) const;
    bool AddOverflows(const Limit& limit1, const Limit& limit2) const;
    bool HasAddOverflow() const;
    bool HasPositiveStep(GenTreePhi* phi, GenTreeLclUse* use) const;
    bool IsAddMonotonicallyIncreasing(GenTreeOp* expr);
    bool IsPhiMonotonicallyIncreasing(GenTreePhi* phi, bool rejectNegativeConst);
    bool IsMonotonicallyIncreasing(GenTree* expr, bool rejectNegativeConst);

#ifdef DEBUG
    const char* ToString(const Limit& limit) const
    {
        constexpr size_t size = 64;
        char*            buf  = compiler->getAllocator(CMK_DebugOnly).allocate<char>(size);

        switch (limit.GetKind())
        {
            size_t len;
            int    c;

            case Limit::Kind::Undefined:
                return "Undefined";
            case Limit::Kind::Unknown:
                return "Unknown";
            case Limit::Kind::Dependent:
                return "Dependent";
            case Limit::Kind::Constant:
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

    const char* ToString(const Range& range) const
    {
        constexpr size_t size = 64;
        char*            buf  = compiler->getAllocator(CMK_DebugOnly).allocate<char>(size);
        sprintf_s(buf, size, "[%s..%s]", ToString(range.min), ToString(range.max));
        return buf;
    }
#endif
};

int RangeCheck::GetArrayLength(ValueNum vn) const
{
    VNFuncApp funcApp;

    if (!vnStore->GetVNFunc(vn, &funcApp) || !funcApp.Is(GT_ARR_LENGTH))
    {
        return -1;
    }

    if (!vnStore->GetVNFunc(funcApp[0], &funcApp) || !funcApp.Is(VNF_JitNewArr, VNF_JitReadyToRunNewArr))
    {
        return -1;
    }

    var_types type = vnStore->GetConstantType(funcApp[1]);

#ifdef TARGET_64BIT
    if (type == TYP_LONG)
    {
        int64_t length = vnStore->ConstantValue<int64_t>(funcApp[1]);
        return length <= INT32_MAX ? static_cast<int>(length) : -1;
    }
#endif

    return type == TYP_INT ? vnStore->ConstantValue<int>(funcApp[1]) : -1;
}

bool RangeCheck::IsInBounds(const Range& range, ValueNum lengthVN, int lengthVal)
{
    JITDUMP("InBounds: %s in [0, " FMT_VN "]\n", ToString(range), lengthVN);
    JITDUMP("InBounds: length " FMT_VN " is: ", lengthVN);
    DBEXEC(compiler->verbose, vnStore->Dump(compiler, lengthVN));
    JITDUMP("\n");
    JITDUMP("InBounds: length value is: %d\n", lengthVal);

    if (range.max.IsConstant())
    {
        if ((lengthVal <= 0) || (range.max.GetConstant() >= lengthVal))
        {
            return false;
        }

        if (range.min.IsVN())
        {
            return (range.min.GetVN() == lengthVN) && (range.min.GetConstant() < 0) &&
                   (-range.min.GetConstant() <= lengthVal) &&
                   (lengthVal + range.min.GetConstant() <= range.max.GetConstant());
        }

        assert(range.min.IsConstant());
        // TODO-MIKE-Review: How could min > max? Should this check be an assert instead?
        return (range.min.GetConstant() >= 0) && (range.min.GetConstant() <= range.max.GetConstant());
    }

    assert(range.max.IsVN());

    if ((range.max.GetVN() != lengthVN) || (range.max.GetConstant() >= 0))
    {
        return false;
    }

    if (range.min.IsVN())
    {
        return (lengthVal > 0) && (range.min.GetVN() == lengthVN) && (range.min.GetConstant() < 0) &&
               (-range.min.GetConstant() <= lengthVal) && (range.min.GetConstant() <= range.max.GetConstant());
    }

    assert(range.min.IsConstant());
    return range.min.GetConstant() >= 0;
}

bool RangeCheck::OptimizeRangeCheck(BasicBlock* block, GenTreeBoundsChk* boundsChk)
{
    GenTree* lengthExpr = boundsChk->GetLength()->SkipComma();
    ValueNum lengthVN   = vnStore->VNNormalValue(lengthExpr->GetConservativeVN());
    ssize_t  lengthVal  = 0;

    if (vnStore->IsIntegralConstant(lengthVN, &lengthVal))
    {
        currentLengthVN = NoVN;

        JITDUMP("Optimize: Constant length " FMT_VN " %d\n", lengthVN, lengthVal);
    }
    else
    {
        currentLengthVN = lengthVN;
        lengthVal       = GetArrayLength(lengthVN);

        if (lengthVal < 0)
        {
            lengthVal = 0;

            if (!BitVecOps::MayBeUninit(block->bbAssertionIn))
            {
                Range lengthRange;
                MergeEdgeAssertions(lengthVN, block->bbAssertionIn, &lengthRange);

                if (lengthRange.min.IsConstant())
                {
                    lengthVal = lengthRange.min.GetConstant();
                }
            }

            JITDUMP("Optimize: Min constant length " FMT_VN " %d\n", lengthVN, lengthVal);
        }
    }

    GenTree* indexExpr = boundsChk->GetIndex()->SkipComma();
    ValueNum indexVN   = vnStore->VNNormalValue(indexExpr->GetConservativeVN());
    ssize_t  indexVal;

    if ((lengthVal > 0) && vnStore->IsIntegralConstant(indexVN, &indexVal) && (0 <= indexVal) && (indexVal < lengthVal))
    {
        JITDUMP("Optimize: Constant index " FMT_VN " %d in [0, %d).\n", indexVN, indexVal, lengthVal);

        return true;
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

    JITDUMP("Optimize: " FMT_BB " ", block->bbNum);
    DBEXEC(compiler->verbose, compiler->gtDispTree(indexExpr, false, false));

    currentIndexExpr          = indexExpr;
    isMonotonicallyIncreasing = false;
    searchDepth               = MaxSearchDepth;
    rangeMap.RemoveAll();

    Range range = ComputeRange(block, indexExpr);

    JITDUMP("Optimize: Index range is %s\n", ToString(range));

    if (range.max.IsUnknown() || range.min.IsUnknown() || HasAddOverflow())
    {
        return false;
    }

    if ((lengthVal <= 0) && !vnStore->IsVNCheckedBound(lengthVN))
    {
        return false;
    }

    return (range.min.IsConstant() || range.min.IsVN()) && (range.max.IsConstant() || range.max.IsVN()) &&
           IsInBounds(range, lengthVN, static_cast<int>(lengthVal));
}

bool RangeCheck::HasPositiveStep(GenTreePhi* phi, GenTreeLclUse* phiArg) const
{
    GenTree* value = phiArg->GetDef()->GetValue();
    int      step  = 0;
    int      count = 0;

    for (unsigned i = 0; i < 16; i++)
    {
        if (!value->TypeIs(TYP_INT))
        {
            return false;
        }

        if (GenTreeLclUse* use = value->IsLclUse())
        {
            value = use->GetDef()->GetValue();

            continue;
        }

        if (value->OperIs(GT_ADD))
        {
            GenTree* op1 = value->AsOp()->GetOp(0);
            GenTree* op2 = value->AsOp()->GetOp(1);

            if (vnStore->IsVNConstant(op1->GetConservativeVN()))
            {
                std::swap(op1, op2);
            }
            else if (!vnStore->IsVNConstant(op2->GetConservativeVN()))
            {
                return false;
            }

            value = op1;
            step += vnStore->GetConstantInt32(op2->GetConservativeVN());

            count++;

            continue;
        }

        break;
    }

    return (value == phi) && (step > 0);
}

bool RangeCheck::IsAddMonotonicallyIncreasing(GenTreeOp* expr)
{
    assert(expr->OperIs(GT_ADD));

    GenTree* op1 = expr->GetOp(0)->SkipComma();
    GenTree* op2 = expr->GetOp(1)->SkipComma();

    if (op2->IsLclUse())
    {
        std::swap(op1, op2);
    }

    if (!op1->IsLclUse())
    {
        JITDUMP("Monotony: Not monotonically increasing because op1 is not LCL_USE.\n");
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

    JITDUMP("Monotony: Not monotonically increasing because expression is not recognized.\n");

    return false;
}

bool RangeCheck::IsPhiMonotonicallyIncreasing(GenTreePhi* phi, bool rejectNegativeConst)
{
    for (GenTreePhi::Use& use : phi->Uses())
    {
        assert(!searchPath.Contains(use.GetNode()));

        if (!IsMonotonicallyIncreasing(use.GetNode(), rejectNegativeConst))
        {
            return false;
        }
    }

    return true;
}

bool RangeCheck::IsMonotonicallyIncreasing(GenTree* expr, bool rejectNegativeConst)
{
    JITDUMP("Monotony: ");
    DBEXEC(compiler->verbose, compiler->gtDispTree(expr, false, false));

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
    else if (expr->OperIs(GT_ADD))
    {
        monotonicallyIncreasing = IsAddMonotonicallyIncreasing(expr->AsOp());
    }
    else if (GenTreeLclUse* use = expr->IsLclUse())
    {
        monotonicallyIncreasing =
            IsMonotonicallyIncreasing(use->GetDef()->GetValue()->SkipComma(), rejectNegativeConst);
    }
    else if (GenTreePhi* phi = expr->IsPhi())
    {
        monotonicallyIncreasing = IsPhiMonotonicallyIncreasing(phi, rejectNegativeConst);
    }

    searchPath.Remove(expr);

    return monotonicallyIncreasing;
}

bool RangeCheck::GetLimitMax(const Limit& limit, int* max) const
{
    if (limit.IsConstant())
    {
        *max = limit.GetConstant();

        return true;
    }

    if (limit.IsVN())
    {
        int len = GetArrayLength(limit.GetVN());

        if (len < 0)
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

bool RangeCheck::AddOverflows(const Limit& limit1, const Limit& limit2) const
{
    int max1;
    int max2;
    return !GetLimitMax(limit1, &max1) || !GetLimitMax(limit2, &max2) || IntAddOverflows(max1, max2);
}

bool RangeCheck::HasAddOverflow() const
{
    for (const auto& pair : rangeMap)
    {
        GenTree* node = pair.key;

        if (node->OperIs(GT_ADD))
        {
            JITDUMP("Overflow: ");
            DBEXEC(compiler->verbose, compiler->gtDispTree(node, false, false));

            const Range* r1 = rangeMap.LookupPointer(node->AsOp()->GetOp(0)->SkipComma());
            const Range* r2 = rangeMap.LookupPointer(node->AsOp()->GetOp(1)->SkipComma());

            if (AddOverflows(r1->max, r2->max))
            {
                JITDUMP("Overflow: overflows\n");

                return true;
            }
        }
    }

    JITDUMP("Overflow: no overflow\n");

    return false;
}

void RangeCheck::MergeEdgeAssertions(ValueNum vn, const ASSERT_TP assertions, Range* range) const
{
    BitVecTraits apTraits(compiler->GetAssertionCount(), compiler);
    for (BitVecOps::Enumerator en(&apTraits, assertions); en.MoveNext();)
    {
        BoundsAssertion assertion = compiler->apGetBoundsAssertion(en.Current());

        if (!assertion.IsBoundsAssertion())
        {
            continue;
        }

        Limit      limit;
        genTreeOps cmpOper = GT_NONE;

        if (assertion.IsCompareCheckedBoundArith()) // (i < length - C) ==/!= 0
        {
            assert(assertion.GetConstantVN() == vnStore->VNForIntCon(0));

            VNFuncApp funcApp;
            vnStore->GetVNFunc(assertion.GetVN(), &funcApp);
            ValueNumStore::CompareCheckedBoundArithInfo info;
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
            limit    = Limit::VN(info.vnBound, info.arrOper == GT_SUB ? -cons : cons);
            cmpOper  = assertion.IsEqual() ? GenTree::ReverseRelop(info.cmpOper) : info.cmpOper;
        }
        else if (assertion.IsCompareCheckedBound()) // (i < length) ==/!= 0
        {
            assert(assertion.GetConstantVN() == vnStore->VNForIntCon(0));

            VNFuncApp funcApp;
            vnStore->GetVNFunc(assertion.GetVN(), &funcApp);
            ValueNumStore::CompareCheckedBoundArithInfo info;
            vnStore->GetCompareCheckedBound(funcApp, &info);

            if (vn == info.vnBound)
            {
                std::swap(info.vnBound, info.cmpOp);
                info.cmpOper = GenTree::SwapRelop(info.cmpOper);
            }
            else if (vn != info.cmpOp)
            {
                continue;
            }

            limit   = Limit::VN(info.vnBound);
            cmpOper = assertion.IsEqual() ? GenTree::ReverseRelop(info.cmpOper) : info.cmpOper;
        }
        else if (assertion.IsRange()) // i IN [C1..C2]
        {
            if (vn != assertion.GetVN())
            {
                continue;
            }

            int max = assertion.GetRangeMax();
            int min = assertion.GetRangeMin();

            // TODO-MIKE-Review: Old code handled only "i < C" like cases,
            // not the more general "i IN [C1..C2]" case. It's likely that
            // we can get useful information from cast related ranges.
            if (max == INT32_MAX)
            {
                limit   = Limit::Constant(min);
                cmpOper = GT_GE;
            }
            else if (min == INT32_MIN)
            {
                limit   = Limit::Constant(max);
                cmpOper = GT_LE;
            }
            else
            {
                continue;
            }
        }
        else if (assertion.IsConstant()) // i ==/!= C
        {
            if (vn != assertion.GetVN())
            {
                continue;
            }

            int constValue = vnStore->CoercedConstantValue<int>(assertion.GetConstantVN());

            if (assertion.IsEqual())
            {
                limit   = Limit::Constant(constValue);
                cmpOper = GT_EQ;
            }
            else if ((constValue == 0) && vnStore->IsVNCheckedBound(assertion.GetVN()))
            {
                limit   = Limit::Constant(1);
                cmpOper = GT_GE;
            }
            else
            {
                continue;
            }
        }
        else
        {
            continue;
        }

        JITDUMP("Assertion: ");
        DBEXEC(compiler->verbose, compiler->apDumpBoundsAssertion(assertion));
        assert(limit.IsVN() || limit.IsConstant());
        assert(cmpOper != GT_NONE);

        // Limits are sometimes VN + constant, where VN is also constant.
        if (limit.IsVN() && vnStore->IsVNInt32Constant(limit.GetVN()))
        {
            Limit temp = Limit::Constant(vnStore->ConstantValue<int>(limit.GetVN()));

            if (temp.AddConstant(limit.GetConstant()))
            {
                limit = temp;
            }
        }

        // Ranges are inclusive, adjust limit as needed.
        if (((cmpOper == GT_LT) || (cmpOper == GT_GT)) && !limit.AddConstant(cmpOper == GT_LT ? -1 : 1))
        {
            continue;
        }

        // Ignore assertions that are redundant or unlikely to be useful (e.g. if we already have
        // "i < a.len + 2" then "i < a.len + 3" is redundant, if the a.len is constant then non
        // constant limits aren't useful).
        //
        // TODO-MIKE-Review: If the limit is constant shouldn't we be checking for redundant cases,
        // like "i < 2" and then "i < 3"? Also, even if the a.len is constant a VN based assertion
        // could still provide some useful info - length VNs are known to be positive.
        // For VN assertions involving different VNs - could "i < a1.len + 1" be better than
        // "i < a2.len + 2" because it's less likely to overflow?
        // Why is range->max being checked even if the limit is intended for range->min?
        if (range->max.IsConstant())
        {
            if (limit.GetVN() != currentLengthVN)
            {
                JITDUMP("Assertion: limit " FMT_VN " != length " FMT_VN "\n", limit.GetVN(), currentLengthVN);
                continue;
            }
        }
        else if (range->max.IsVN() && (range->max.GetVN() == currentLengthVN))
        {
            if (limit.GetVN() != currentLengthVN)
            {
                JITDUMP("Assertion: limit " FMT_VN " != length " FMT_VN "\n", limit.GetVN(), currentLengthVN);
                continue;
            }

            if (limit.GetConstant() >= range->max.GetConstant())
            {
                JITDUMP("Assertion: limit VN + %d >= max VN + %d\n", limit.GetConstant(), range->max.GetConstant());
                continue;
            }
        }

        JITDUMP("Assertion: %s -> ", ToString(*range));

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

        JITDUMP("%s\n", ToString(*range));
    }
}

void RangeCheck::MergePhiArgAssertions(BasicBlock* block, GenTreeLclUse* use, Range* range) const
{
    JITDUMP("Range: " FMT_BB " [%06u] = %s\n", block->bbNum, use->GetID(), ToString(*range));

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
        assert(valueVN != NoVN);
        MergeEdgeAssertions(valueVN, assertions, range);
    }
}

void RangeCheck::MergeLclUseAssertions(BasicBlock* block, GenTreeLclUse* use, Range* range) const
{
    GenTreeLclDef* def        = use->GetDef();
    ASSERT_TP      assertions = block->bbAssertionIn;

    if (!BitVecOps::MayBeUninit(assertions))
    {
        ValueNum valueVN = vnStore->VNNormalValue(def->GetConservativeVN());
        assert(valueVN != NoVN);
        MergeEdgeAssertions(valueVN, assertions, range);
    }
}

Range RangeCheck::ComputeLclUseRange(BasicBlock* block, GenTreeLclUse* use)
{
    GenTreeLclDef* def = use->GetDef();

    JITDUMP("Range: " FMT_BB " ", def->GetBlock()->bbNum);
    DBEXEC(compiler->verbose, compiler->gtDispTree(def, false, false));

    // Uses may perform implicit LONG to INT truncation and possibly
    // other weird conversions such as BYREF to INT, ignore for now.
    if (varActualType(def->GetType()) != TYP_INT)
    {
        return Limit::Unknown();
    }

    Range range = *GetRange(def->GetBlock(), def->GetValue()->SkipComma());
    MergeLclUseAssertions(block, use, &range);
    return range;
}

Range RangeCheck::AddRanges(const Range& r1, const Range& r2) const
{
    Range result = Limit::Unknown();

    const Limit& min1 = r1.min;
    const Limit& min2 = r2.min;

    // TODO-MIKE-Review: This is dubious. Old code had lo1.IsDependent() && !lo1.IsUnknown(),
    // with the Unknown check being obviously pointless. But was that just bogus code or was
    // it actually broken and needs to be lo1.IsDependent() && !lo2.IsUnknown()? It would
    // make sense for Unknown to act as "bottom"...

    if (min1.IsDependent() || min2.IsDependent())
    {
        result.min = Limit::Dependent();
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

    if (max1.IsConstant())
    {
        result.max = max2 + max1.GetConstant();
    }
    else if (max2.IsConstant())
    {
        result.max = max1 + max2.GetConstant();
    }

    return result;
}

Range RangeCheck::ComputeAddRange(BasicBlock* block, GenTreeOp* add)
{
    assert(add->OperIs(GT_ADD) && add->TypeIs(TYP_INT));

    GenTree* op1 = add->GetOp(0)->SkipComma();
    GenTree* op2 = add->GetOp(1)->SkipComma();

    const Range* range1 = GetRange(block, op1);
    const Range* range2 = GetRange(block, op2);

    JITDUMP("Range: " FMT_BB " [%06u] ADD %s, %s\n", block->bbNum, add->GetID(), ToString(*range1), ToString(*range2));

    return AddRanges(*range1, *range2);
}

Range RangeCheck::MergeRanges(const Range& r1, const Range& r2) const
{
    Range result = Limit::Unknown();

    const Limit& min1 = r1.min;
    const Limit& min2 = r2.min;

    if (!min1.IsUnknown() && !min2.IsUnknown())
    {
        if (min1.IsUndefined())
        {
            result.min = min2;
        }
        else if (min1.IsDependent() || min2.IsDependent())
        {
            if (isMonotonicallyIncreasing)
            {
                result.min = min1.IsDependent() ? min2 : min1;
            }
            else
            {
                result.min = Limit::Dependent();
            }
        }
        else if (min1.IsConstant() && min2.IsConstant())
        {
            result.min = Limit::Constant(Min(min1.GetConstant(), min2.GetConstant()));
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
        if (max1.IsUndefined())
        {
            result.max = max2;
        }
        else if (max1.IsConstant() && max2.IsConstant())
        {
            result.max = Limit::Constant(Max(max1.GetConstant(), max2.GetConstant()));
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

Range RangeCheck::ComputePhiRange(BasicBlock* block, GenTreePhi* phi)
{
    Range phiRange;

    for (GenTreePhi::Use& use : phi->Uses())
    {
        GenTreeLclUse* useNode  = use.GetNode();
        Range*         useRange = rangeMap.LookupPointer(useNode);

        JITDUMP("Range: " FMT_BB " ", block->bbNum);
        DBEXEC(compiler->verbose, compiler->gtDispTree(useNode, false, false));

        if (useRange == nullptr)
        {
            ValueNum useVN = vnStore->VNNormalValue(useNode->GetConservativeVN());

            if (vnStore->IsVNInt32Constant(useVN))
            {
                Range range = Limit::Constant(vnStore->GetConstantInt32(useVN));
                MergePhiArgAssertions(block, useNode, &range);
                useRange = rangeMap.Emplace(useNode, range);
            }
            else
            {
                GenTreeLclDef* def = useNode->GetDef();

                JITDUMP("Range: " FMT_BB " ", def->GetBlock()->bbNum);
                DBEXEC(compiler->verbose, compiler->gtDispTree(def, false, false));

                useRange    = rangeMap.Emplace(useNode);
                Range range = *GetRange(def->GetBlock(), def->GetValue()->SkipComma());
                MergePhiArgAssertions(block, useNode, &range);
                *useRange = range;
            }
        }
        else if (useRange->min.IsUndefined())
        {
            Range range = Limit::Unknown();

            if (HasPositiveStep(phi, useNode))
            {
                range.min = phiRange.min;
            }
            else
            {
                if (isMonotonicallyIncreasingExpr != currentIndexExpr)
                {
                    searchPath.Clear();
                    isMonotonicallyIncreasing     = IsMonotonicallyIncreasing(currentIndexExpr, false);
                    isMonotonicallyIncreasingExpr = currentIndexExpr;
                }

                if (isMonotonicallyIncreasing)
                {
                    range.min = Limit::Dependent();
                }
            }

            MergePhiArgAssertions(block, useNode, &range);
            *useRange = range;
        }
        else
        {
            JITDUMP("Range: " FMT_BB " [%06u] = %s\n", block->bbNum, useNode->GetID(), ToString(*useRange));
        }

        JITDUMP("Range: " FMT_BB " [%06u] PHI %s, %s\n", block->bbNum, phi->GetID(), ToString(phiRange),
                ToString(*useRange));

        phiRange = MergeRanges(phiRange, *useRange);
    }

    return phiRange;
}

Range RangeCheck::ComputeLeafRange(GenTree* expr) const
{
    if (expr->OperIs(GT_IND, GT_LCL_FLD, GT_LCL_VAR, GT_CAST))
    {
        switch (expr->IsCast() ? expr->AsCast()->GetCastType() : expr->GetType())
        {
            case TYP_BOOL:
            case TYP_UBYTE:
                return Range(Limit::Constant(0), Limit::Constant(255));
            case TYP_BYTE:
                return Range(Limit::Constant(-128), Limit::Constant(127));
            case TYP_USHORT:
                return Range(Limit::Constant(0), Limit::Constant(65535));
            case TYP_SHORT:
                return Range(Limit::Constant(-32768), Limit::Constant(32767));
            default:
                return Limit::Unknown();
        }
    }
    else if (expr->OperIs(GT_AND, GT_RSH, GT_LSH, GT_UMOD) && expr->TypeIs(TYP_INT))
    {
        GenTreeIntCon* op2 = expr->AsOp()->GetOp(1)->SkipComma()->IsIntCon();

        if (op2 == nullptr)
        {
            return Limit::Unknown();
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
            GenTree* op1   = expr->AsOp()->GetOp(0)->SkipComma();
            int      shift = op2->GetInt32Value();

            if ((shift >= 0) && (shift < 32) && op1->OperIs(GT_AND))
            {
                if (GenTreeIntCon* maskIntCon = op1->AsOp()->GetOp(1)->SkipComma()->IsIntCon())
                {
                    int mask = maskIntCon->GetInt32Value();

                    if (mask >= 0)
                    {
                        constLimit = expr->OperIs(GT_RSH) ? (mask >> shift) : (mask << shift);
                    }
                }
            }
        }

        if (constLimit >= 0)
        {
            return Range(Limit::Constant(0), Limit::Constant(constLimit));
        }
    }

    return Limit::Unknown();
}

Range RangeCheck::ComputeRange(BasicBlock* block, GenTree* expr)
{
    assert(!expr->OperIs(GT_COMMA) && (varActualType(expr->GetType()) == TYP_INT));

    ValueNum vn = vnStore->VNNormalValue(expr->GetConservativeVN());

    if (var_types type = vnStore->GetConstantType(vn))
    {
        return type == TYP_INT ? Limit::Constant(vnStore->ConstantValue<int>(vn)) : Limit::Unknown();
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

    return ComputeLeafRange(expr);
}

Range* RangeCheck::GetRange(BasicBlock* block, GenTree* expr)
{
    JITDUMP("Range: " FMT_BB " ", block->bbNum);
    DBEXEC(compiler->verbose, compiler->gtDispTree(expr, false, false));

    assert(!expr->OperIs(GT_COMMA));

    Range* range = rangeMap.LookupPointer(expr);

    if (range == nullptr)
    {
        range = rangeMap.Emplace(expr);
        budget--;
    }
    else if (!range->min.IsUndefined())
    {
        JITDUMP("Range: Cached %s\n", ToString(*range));

        return range;
    }

    if ((budget <= 0) || (searchDepth <= 0))
    {
        JITDUMP("Range: %s exceeded\n", budget <= 0 ? "Budget" : "Depth");

        *range = Range(Limit::Unknown());
    }
    else
    {
        searchDepth--;
        *range = ComputeRange(block, expr);
        searchDepth++;
    }

    assert(!range->min.IsUndefined() && !range->max.IsUndefined());
    JITDUMP("Range: " FMT_BB " [%06u] = %s\n", block->bbNum, expr->GetID(), ToString(*range));

    return range;
}

void RangeCheck::OptimizeRangeChecks()
{
    for (BasicBlock* block : compiler->Blocks())
    {
        for (Statement* stmt : block->Statements())
        {
            bool stmtModified = false;

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
                        stmtModified = true;
                    }
                }
            }

            if (stmtModified)
            {
                compiler->gtSetStmtInfo(stmt);
                compiler->fgSetStmtSeq(stmt);
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
