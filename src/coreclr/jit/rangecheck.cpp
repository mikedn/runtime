// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#include "jitpch.h"

//  We take the following approach to range check analysis:
//
//  Consider the following loop:
//  for (int i = 0; i < a.len; ++i) {
//      a[i] = 0;
//  }
//
//  This would be represented as:
//              i_0 = 0; BB0
//               /        ______  a[i_1] = 0;     BB2
//              /        /        i_2 = i_1 + 1;
//             /        /          ^
//  i_1 = phi(i_0, i_2); BB1       |
//  i_1 < a.len -------------------+
//
//  BB0 -> BB1
//  BB1 -> (i_1 < a.len) ? BB2 : BB3
//  BB2 -> BB1
//  BB3 -> return
//
//  **Step 1. Walk the statements in the method checking if there is a bounds check.
//  If there is a bounds check, ask the range of the index variable.
//  In the above example i_1's range.
//
//  **Step 2. Follow the defs and the dependency chain:
//  i_1 is a local, so go to its definition which is i_1 = phi(i_0, i_2).
//
//  Since rhs is a phi, we ask the range for i_0 and i_2 in the hopes of merging
//  the resulting ranges for i_1.
//
//  The range of i_0 follows immediately when going to its definition.
//  Ask for the range of i_2, which leads to i_1 + 1.
//  Ask for the range of i_1 and figure we are looping. Call the range of i_1 as
//  "dependent" and quit looping further. The range of "1" is just <1, 1>.
//
//  Now we have exhausted all the variables for which the range can be determined.
//  The others are either "unknown" or "dependent."
//
//  We also merge assertions from its pred block's edges for a phi argument otherwise
//  from the block's assertionIn. This gives us an upper bound for i_1 as a.len.
//
//  **Step 3. Check if an overflow occurs in the dependency chain (loop.)
//  In the above case, we want to make sure there is no overflow in the definitions
//  involving i_1 and i_2. Merge assertions from the block's edges whenever possible.
//
//  **Step 4. Check if the dependency chain is monotonic.
//
//  **Step 5. If monotonic increasing is true, then perform a widening step, where we assume, the
//  SSA variables that are "dependent" get their lower bound values from the definitions in the
//  dependency loop and their initial values must be the definitions that are not in
//  the dependency loop, in this case i_0's value which is 0.
//

static bool IntAddOverflows(int max1, int max2)
{
    if (max1 > 0 && max2 > 0 && INT_MAX - max1 < max2)
    {
        return true;
    }
    if (max1 < 0 && max2 < 0 && max1 < INT_MIN - max2)
    {
        return true;
    }
    return false;
}

struct Limit
{
    enum LimitType
    {
        keUndef, // The limit is yet to be computed.
        keBinOpArray,
        keConstant,
        keDependent, // The limit is dependent on some other value.
        keUnknown,   // The limit could not be determined.
    };

    Limit() : type(keUndef)
    {
    }

    Limit(LimitType type) : type(type)
    {
    }

    Limit(LimitType type, int cns) : cns(cns), vn(ValueNumStore::NoVN), type(type)
    {
        assert(type == keConstant);
    }

    Limit(LimitType type, ValueNum vn, int cns) : cns(cns), vn(vn), type(type)
    {
        assert(type == keBinOpArray);
    }

    bool IsUndef()
    {
        return type == keUndef;
    }
    bool IsDependent()
    {
        return type == keDependent;
    }
    bool IsUnknown()
    {
        return type == keUnknown;
    }
    bool IsConstant()
    {
        return type == keConstant;
    }
    int GetConstant()
    {
        return cns;
    }
    bool IsBinOpArray()
    {
        return type == keBinOpArray;
    }
    bool AddConstant(int i)
    {
        switch (type)
        {
            case keDependent:
                return true;
            case keBinOpArray:
            case keConstant:
                if (IntAddOverflows(cns, i))
                {
                    return false;
                }
                cns += i;
                return true;
            case keUndef:
            case keUnknown:
                // For these values of 'type', conservatively return false
                break;
        }

        return false;
    }

    bool Equals(Limit& l)
    {
        switch (type)
        {
            case keUndef:
            case keUnknown:
            case keDependent:
                return l.type == type;

            case keBinOpArray:
                return l.type == type && l.vn == vn && l.cns == cns;

            case keConstant:
                return l.type == type && l.cns == cns;
        }
        return false;
    }
#ifdef DEBUG
    const char* ToString(CompAllocator alloc)
    {
        unsigned size = 64;
        char*    buf  = alloc.allocate<char>(size);
        switch (type)
        {
            case keUndef:
                return "Undef";

            case keUnknown:
                return "Unknown";

            case keDependent:
                return "Dependent";

            case keBinOpArray:
                sprintf_s(buf, size, FMT_VN " + %d", vn, cns);
                return buf;

            case keConstant:
                sprintf_s(buf, size, "%d", cns);
                return buf;
        }
        unreached();
    }
#endif
    int       cns;
    ValueNum  vn;
    LimitType type;
};

// Range struct contains upper and lower limit.
struct Range
{
    Limit uLimit;
    Limit lLimit;

    Range(const Limit& limit) : uLimit(limit), lLimit(limit)
    {
    }

    Range(const Limit& lLimit, const Limit& uLimit) : uLimit(uLimit), lLimit(lLimit)
    {
    }

    Limit& UpperLimit()
    {
        return uLimit;
    }

    Limit& LowerLimit()
    {
        return lLimit;
    }

#ifdef DEBUG
    char* ToString(CompAllocator alloc)
    {
        size_t size = 64;
        char*  buf  = alloc.allocate<char>(size);
        sprintf_s(buf, size, "<%s, %s>", lLimit.ToString(alloc), uLimit.ToString(alloc));
        return buf;
    }
#endif
};

// Helpers for operations performed on ranges
struct RangeOps
{
    // Given a constant limit in "l1", add it to l2 and mutate "l2".
    static Limit AddConstantLimit(Limit& l1, Limit& l2)
    {
        assert(l1.IsConstant());
        Limit l = l2;
        if (l.AddConstant(l1.GetConstant()))
        {
            return l;
        }
        else
        {
            return Limit(Limit::keUnknown);
        }
    }

    // Given two ranges "r1" and "r2", perform an add operation on the
    // ranges.
    static Range Add(Range& r1, Range& r2)
    {
        Limit& r1lo = r1.LowerLimit();
        Limit& r1hi = r1.UpperLimit();
        Limit& r2lo = r2.LowerLimit();
        Limit& r2hi = r2.UpperLimit();

        Range result = Limit(Limit::keUnknown);

        // Check lo ranges if they are dependent and not unknown.
        if ((r1lo.IsDependent() && !r1lo.IsUnknown()) || (r2lo.IsDependent() && !r2lo.IsUnknown()))
        {
            result.lLimit = Limit(Limit::keDependent);
        }
        // Check hi ranges if they are dependent and not unknown.
        if ((r1hi.IsDependent() && !r1hi.IsUnknown()) || (r2hi.IsDependent() && !r2hi.IsUnknown()))
        {
            result.uLimit = Limit(Limit::keDependent);
        }

        if (r1lo.IsConstant())
        {
            result.lLimit = AddConstantLimit(r1lo, r2lo);
        }
        if (r2lo.IsConstant())
        {
            result.lLimit = AddConstantLimit(r2lo, r1lo);
        }
        if (r1hi.IsConstant())
        {
            result.uLimit = AddConstantLimit(r1hi, r2hi);
        }
        if (r2hi.IsConstant())
        {
            result.uLimit = AddConstantLimit(r2hi, r1hi);
        }
        return result;
    }

    // Given two ranges "r1" and "r2", do a Phi merge. If "monIncreasing" is true,
    // then ignore the dependent variables for the lower bound but not for the upper bound.
    static Range Merge(Range& r1, Range& r2, bool monIncreasing)
    {
        Limit& r1lo = r1.LowerLimit();
        Limit& r1hi = r1.UpperLimit();
        Limit& r2lo = r2.LowerLimit();
        Limit& r2hi = r2.UpperLimit();

        // Take care of lo part.
        Range result = Limit(Limit::keUnknown);
        if (r1lo.IsUnknown() || r2lo.IsUnknown())
        {
            result.lLimit = Limit(Limit::keUnknown);
        }
        // Uninitialized, just copy.
        else if (r1lo.IsUndef())
        {
            result.lLimit = r2lo;
        }
        else if (r1lo.IsDependent() || r2lo.IsDependent())
        {
            if (monIncreasing)
            {
                result.lLimit = r1lo.IsDependent() ? r2lo : r1lo;
            }
            else
            {
                result.lLimit = Limit(Limit::keDependent);
            }
        }

        // Take care of hi part.
        if (r1hi.IsUnknown() || r2hi.IsUnknown())
        {
            result.uLimit = Limit(Limit::keUnknown);
        }
        else if (r1hi.IsUndef())
        {
            result.uLimit = r2hi;
        }
        else if (r1hi.IsDependent() || r2hi.IsDependent())
        {
            result.uLimit = Limit(Limit::keDependent);
        }

        if (r1lo.IsConstant() && r2lo.IsConstant())
        {
            result.lLimit = Limit(Limit::keConstant, min(r1lo.GetConstant(), r2lo.GetConstant()));
        }
        if (r1hi.IsConstant() && r2hi.IsConstant())
        {
            result.uLimit = Limit(Limit::keConstant, max(r1hi.GetConstant(), r2hi.GetConstant()));
        }
        if (r2hi.Equals(r1hi))
        {
            result.uLimit = r2hi;
        }
        if (r2lo.Equals(r1lo))
        {
            result.lLimit = r1lo;
        }
        // Widen Upper Limit => Max(k, (a.len + n)) yields (a.len + n),
        // This is correct if k >= 0 and n >= k, since a.len always >= 0
        // (a.len + n) could overflow, but the result (a.len + n) also
        // preserves the overflow.
        if (r1hi.IsConstant() && r1hi.GetConstant() >= 0 && r2hi.IsBinOpArray() &&
            r2hi.GetConstant() >= r1hi.GetConstant())
        {
            result.uLimit = r2hi;
        }
        if (r2hi.IsConstant() && r2hi.GetConstant() >= 0 && r1hi.IsBinOpArray() &&
            r1hi.GetConstant() >= r2hi.GetConstant())
        {
            result.uLimit = r1hi;
        }
        if (r1hi.IsBinOpArray() && r2hi.IsBinOpArray() && r1hi.vn == r2hi.vn)
        {
            result.uLimit = r1hi;
            // Widen the upper bound if the other constant is greater.
            if (r2hi.GetConstant() > r1hi.GetConstant())
            {
                result.uLimit = r2hi;
            }
        }
        return result;
    }
};

// Max stack depth (path length) in walking the UD chain.
static constexpr int MAX_SEARCH_DEPTH = 100;
// Max nodes to visit in the UD chain for the current method being compiled.
static constexpr int MAX_VISIT_BUDGET = 8192;

class RangeCheck
{
    using OverflowMap = JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, bool>;
    using RangeMap    = JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, Range*>;
    using SearchPath  = JitHashTable<GenTree*, JitPtrKeyFuncs<GenTree>, BasicBlock*>;

    ValueNumStore* vnStore;
    OverflowMap    overflowMap;
    RangeMap       rangeMap;
    SearchPath     searchPath;

public:
    RangeCheck(Compiler* compiler)
        : vnStore(compiler->vnStore)
        , overflowMap(compiler->getAllocator(CMK_RangeCheck))
        , rangeMap(compiler->getAllocator(CMK_RangeCheck))
        , searchPath(compiler->getAllocator(CMK_RangeCheck))
        , m_pCompiler(compiler)
        , m_alloc(compiler->getAllocator(CMK_RangeCheck))
        , m_nVisitBudget(MAX_VISIT_BUDGET)
    {
    }

    int GetArrLength(ValueNum vn);

    // Check whether the computed range is within 0 and upper bounds. This function
    // assumes that the lower range is resolved and upper range is symbolic as in an
    // increasing loop.
    // TODO-CQ: This is not general enough.
    bool BetweenBounds(Range& range, GenTree* upper, int arrSize);

    // Entry point to optimize range checks in the block. Assumes value numbering
    // and assertion prop phases are completed.
    void OptimizeRangeChecks();

    // Given a "tree" node, check if it contains array bounds check node and
    // optimize to remove it, if possible. Requires "stmt" and "block" that
    // contain the tree.
    void OptimizeRangeCheck(BasicBlock* block, Statement* stmt, GenTree* tree);

    // Given the index expression try to find its range.
    // The range of a variable depends on its rhs which in turn depends on its constituent variables.
    // The "path" is the path taken in the search for the rhs' range and its constituents' range.
    // If "monIncreasing" is true, the calculations are made more liberally assuming initial values
    // at phi definitions for the lower bound.
    Range GetRange(BasicBlock* block, GenTree* expr, bool monIncreasing DEBUGARG(int indent));

    // Given the local variable, first find the definition of the local and find the range of the rhs.
    // Helper for GetRange.
    Range ComputeRangeForLocalDef(BasicBlock* block, GenTreeLclUse* use, bool monIncreasing DEBUGARG(int indent));

    // Compute the range, rather than retrieve a cached value. Helper for GetRange.
    Range ComputeRange(BasicBlock* block, GenTree* expr, bool monIncreasing DEBUGARG(int indent));

    // Compute the range for the op1 and op2 for the given binary operator.
    Range ComputeRangeForBinOp(BasicBlock* block, GenTreeOp* binop, bool monIncreasing DEBUGARG(int indent));

    // Merge assertions from AssertionProp's flags, for the corresponding "phiArg."
    // Requires "pRange" to contain range that is computed partially.
    void MergePhiArgAssertion(BasicBlock* block, GenTreeLclUse* use, Range* pRange DEBUGARG(int indent));
    void MergeSsaUseAssertion(BasicBlock* block, GenTreeLclUse* use, Range* pRange DEBUGARG(int indent));

    // Inspect the assertions about the current ValueNum to refine pRange
    void MergeEdgeAssertions(ValueNum num, ASSERT_VALARG_TP assertions, Range* pRange);

    // The maximum possible value of the given "limit." If such a value could not be determined
    // return "false." For example: ARRLEN_MAX for array length.
    bool GetLimitMax(Limit& limit, int* pMax);

    // Does the addition of the two limits overflow?
    bool AddOverflows(Limit& limit1, Limit& limit2);

    // Does the binary operation between the operands overflow? Check recursively.
    bool DoesBinOpOverflow(BasicBlock* block, GenTreeOp* binop);

    // Does the phi operands involve an assignment that could overflow?
    bool DoesPhiOverflow(BasicBlock* block, GenTreePhi* phi);

    bool ComputeDoesOverflow(BasicBlock* block, GenTree* expr);

    // Does the current "expr" which is a use involve a definition, that overflows.
    bool DoesOverflow(BasicBlock* block, GenTree* tree);

    // Widen the range by first checking if the induction variable is monotonically increasing.
    // Requires "pRange" to be partially computed.
    void Widen(BasicBlock* block, GenTree* tree, Range* pRange);

    // Is the binary operation increasing the value.
    bool IsBinOpMonotonicallyIncreasing(GenTreeOp* binop);

    // Given an "expr" trace its rhs and their definitions to check if all the assignments
    // are monotonically increasing.
    //
    bool IsMonotonicallyIncreasing(GenTree* tree, bool rejectNegativeConst);

    // We allocate a budget to avoid walking long UD chains. When traversing each link in the UD
    // chain, we decrement the budget. When the budget hits 0, then no more range check optimization
    // will be applied for the currently compiled method.
    bool IsOverBudget();

private:
    GenTreeBoundsChk* m_pCurBndsChk;

    Compiler*     m_pCompiler;
    CompAllocator m_alloc;

    // The number of nodes for which range is computed throughout the current method.
    // When this limit is zero, we have exhausted all the budget to walk the ud-chain.
    int m_nVisitBudget;
};

bool RangeCheck::IsOverBudget()
{
    return (m_nVisitBudget <= 0);
}

// Get the length of the array vn, if it is new.
int RangeCheck::GetArrLength(ValueNum vn)
{
    return vnStore->GetNewArrSize(vnStore->GetArrForLenVn(vn));
}

//------------------------------------------------------------------------
// BetweenBounds: Check if the computed range is within bounds
//
// Arguments:
//    Range - the range to check if in bounds
//    upper - the array length vn
//    arrSize - the length of the array if known, or <= 0
//
// Return Value:
//    True iff range is between [0 and vn - 1] or [0, arrSize - 1]
//
// notes:
//    This function assumes that the lower range is resolved and upper range is symbolic as in an
//    increasing loop.
//
// TODO-CQ: This is not general enough.
//
bool RangeCheck::BetweenBounds(Range& range, GenTree* upper, int arrSize)
{
    JITDUMP("%s BetweenBounds <%d, [%06u]>\n", range.ToString(m_pCompiler->getAllocatorDebugOnly()), 0, upper->GetID());

    ValueNumStore* vnStore = m_pCompiler->vnStore;

    // Get the VN for the upper limit.
    ValueNum uLimitVN = vnStore->VNConservativeNormalValue(upper->gtVNPair);

#ifdef DEBUG
    JITDUMP(FMT_VN " upper bound is: ", uLimitVN);
    if (m_pCompiler->verbose)
    {
        vnStore->vnDump(m_pCompiler, uLimitVN);
    }
    JITDUMP("\n");
#endif

    if ((arrSize <= 0) && !vnStore->IsVNCheckedBound(uLimitVN))
    {
        // If we don't know the array size and the upper limit is not known, then bail.
        return false;
    }

    JITDUMP("Array size is: %d\n", arrSize);

    // Upper limit: len + ucns (upper limit constant).
    if (range.UpperLimit().IsBinOpArray())
    {
        if (range.UpperLimit().vn != uLimitVN)
        {
            return false;
        }

        int ucns = range.UpperLimit().GetConstant();

        // Upper limit: Len + [0..n]
        if (ucns >= 0)
        {
            return false;
        }

        // Since upper limit is bounded by the array, return true if lower bound is good.
        if (range.LowerLimit().IsConstant() && range.LowerLimit().GetConstant() >= 0)
        {
            return true;
        }

        // Check if we have the array size allocated by new.
        if (arrSize <= 0)
        {
            return false;
        }

        // At this point,
        // upper limit = len + ucns. ucns < 0
        // lower limit = len + lcns.
        if (range.LowerLimit().IsBinOpArray())
        {
            int lcns = range.LowerLimit().GetConstant();
            if (lcns >= 0 || -lcns > arrSize)
            {
                return false;
            }
            return (range.LowerLimit().vn == uLimitVN && lcns <= ucns);
        }
    }
    // If upper limit is constant
    else if (range.UpperLimit().IsConstant())
    {
        if (arrSize <= 0)
        {
            return false;
        }
        int ucns = range.UpperLimit().GetConstant();
        if (ucns >= arrSize)
        {
            return false;
        }
        if (range.LowerLimit().IsConstant())
        {
            int lcns = range.LowerLimit().GetConstant();
            // Make sure lcns < ucns which is already less than arrSize.
            return (lcns >= 0 && lcns <= ucns);
        }
        if (range.LowerLimit().IsBinOpArray())
        {
            int lcns = range.LowerLimit().GetConstant();
            // len + lcns, make sure we don't subtract too much from len.
            if (lcns >= 0 || -lcns > arrSize)
            {
                return false;
            }
            // Make sure a.len + lcns <= ucns.
            return (range.LowerLimit().vn == uLimitVN && (arrSize + lcns) <= ucns);
        }
    }

    return false;
}

void RangeCheck::OptimizeRangeCheck(BasicBlock* block, Statement* stmt, GenTree* treeParent)
{
    // Check if we are dealing with a bounds check node.
    bool isComma        = treeParent->OperIs(GT_COMMA);
    bool isTopLevelNode = treeParent == stmt->GetRootNode();
    if (!(isComma || isTopLevelNode))
    {
        return;
    }

    // If we are not looking at array bounds check, bail.
    GenTree* tree = isComma ? treeParent->AsOp()->gtOp1 : treeParent;
    if (!tree->IsBoundsChk())
    {
        return;
    }

    GenTree*          comma   = treeParent->OperIs(GT_COMMA) ? treeParent : nullptr;
    GenTreeBoundsChk* bndsChk = tree->AsBoundsChk();
    m_pCurBndsChk             = bndsChk;
    GenTree* treeIndex        = bndsChk->gtIndex;

    // Take care of constant index first, like a[2], for example.
    ValueNum idxVn    = m_pCompiler->vnStore->VNNormalValue(treeIndex->GetConservativeVN());
    ValueNum arrLenVn = m_pCompiler->vnStore->VNNormalValue(bndsChk->GetLength()->GetConservativeVN());
    int      arrSize  = 0;

    if (m_pCompiler->vnStore->IsVNConstant(arrLenVn))
    {
        ssize_t      constVal  = -1;
        GenTreeFlags iconFlags = GTF_EMPTY;

        if (m_pCompiler->vnStore->IsVNIntegralConstant(arrLenVn, &constVal, &iconFlags))
        {
            arrSize = (int)constVal;
        }
    }
    else if (bndsChk->OperIs(GT_ARR_BOUNDS_CHECK))
    {
        arrSize = GetArrLength(arrLenVn);

        // if we can't find the array length, see if there
        // are any assertions about the array size we can use to get a minimum length
        if ((arrSize <= 0) && (m_pCompiler->GetAssertionCount() != 0))
        {
            JITDUMP("Looking for array size assertions for: " FMT_VN "\n", arrLenVn);
            Range arrLength = Range(Limit(Limit::keDependent));
            MergeEdgeAssertions(arrLenVn, block->bbAssertionIn, &arrLength);
            if (arrLength.lLimit.IsConstant())
            {
                arrSize = arrLength.lLimit.GetConstant();
            }
        }
    }

    JITDUMP("ArrSize for lengthVN:%03X = %d\n", arrLenVn, arrSize);
    if (m_pCompiler->vnStore->IsVNConstant(idxVn) && (arrSize > 0))
    {
        ssize_t      idxVal    = -1;
        GenTreeFlags iconFlags = GTF_EMPTY;
        if (!m_pCompiler->vnStore->IsVNIntegralConstant(idxVn, &idxVal, &iconFlags))
        {
            return;
        }

        JITDUMP("[RangeCheck::OptimizeRangeCheck] Is index %d in <0, arrLenVn " FMT_VN " sz:%d>.\n", idxVal, arrLenVn,
                arrSize);
        if ((idxVal < arrSize) && (idxVal >= 0))
        {
            JITDUMP("Removing range check\n");
            m_pCompiler->optRemoveRangeCheck(bndsChk, comma, stmt);
            return;
        }
    }

    rangeMap.RemoveAll();
    overflowMap.RemoveAll();
    searchPath.RemoveAll();

    // Get the range for this index.
    Range range = GetRange(block, treeIndex, false DEBUGARG(0));

    // If upper or lower limit is found to be unknown (top), or it was found to
    // be unknown because of over budget or a deep search, then return early.
    if (range.UpperLimit().IsUnknown() || range.LowerLimit().IsUnknown())
    {
        // Note: If we had stack depth too deep in the GetRange call, we'd be
        // too deep even in the DoesOverflow call. So return early.
        return;
    }

    if (DoesOverflow(block, treeIndex))
    {
        JITDUMP("Method determined to overflow.\n");
        return;
    }

    JITDUMP("Range value %s\n", range.ToString(m_pCompiler->getAllocatorDebugOnly()));
    searchPath.RemoveAll();
    Widen(block, treeIndex, &range);

    // If upper or lower limit is unknown, then return.
    if (range.UpperLimit().IsUnknown() || range.LowerLimit().IsUnknown())
    {
        return;
    }

    // Is the range between the lower and upper bound values.
    if (BetweenBounds(range, bndsChk->gtArrLen, arrSize))
    {
        JITDUMP("[RangeCheck::OptimizeRangeCheck] Between bounds\n");
        m_pCompiler->optRemoveRangeCheck(bndsChk, comma, stmt);
    }
    return;
}

void RangeCheck::Widen(BasicBlock* block, GenTree* tree, Range* pRange)
{
    JITDUMP("[RangeCheck::Widen]" FMT_BB ", [%06u]\n", block->bbNum, tree->GetID());

    Range& range = *pRange;

    // Try to deduce the lower bound, if it is not known already.
    if (range.LowerLimit().IsDependent() || range.LowerLimit().IsUnknown())
    {
        // To determine the lower bound, ask if the loop increases monotonically.
        bool increasing = IsMonotonicallyIncreasing(tree, false);
        if (increasing)
        {
            JITDUMP("[%06u] is monotonically increasing.\n", tree->GetID());
            rangeMap.RemoveAll();
            *pRange = GetRange(block, tree, true DEBUGARG(0));
        }
    }
}

bool RangeCheck::IsBinOpMonotonicallyIncreasing(GenTreeOp* binop)
{
    assert(binop->OperIs(GT_ADD));

    GenTree* op1 = binop->gtGetOp1();
    GenTree* op2 = binop->gtGetOp2();

    JITDUMP("[RangeCheck::IsBinOpMonotonicallyIncreasing] [%06u], [%06u]\n", op1->GetID(), op2->GetID());
    // Check if we have a var + const.
    if (op2->IsLclUse())
    {
        std::swap(op1, op2);
    }
    if (!op1->IsLclUse())
    {
        JITDUMP("Not monotonically increasing because op1 is not lclVar.\n");
        return false;
    }
    switch (op2->OperGet())
    {
        case GT_LCL_USE:
            // When adding two local variables, we also must ensure that any constant is non-negative.
            return IsMonotonicallyIncreasing(op1, true) && IsMonotonicallyIncreasing(op2, true);

        case GT_CNS_INT:
            if (op2->AsIntConCommon()->IconValue() < 0)
            {
                JITDUMP("Not monotonically increasing because of encountered negative constant\n");
                return false;
            }

            return IsMonotonicallyIncreasing(op1, false);

        default:
            JITDUMP("Not monotonically increasing because expression is not recognized.\n");
            return false;
    }
}

// The parameter rejectNegativeConst is true when we are adding two local vars (see above)
bool RangeCheck::IsMonotonicallyIncreasing(GenTree* expr, bool rejectNegativeConst)
{
    JITDUMP("[RangeCheck::IsMonotonicallyIncreasing] [%06d]\n", Compiler::dspTreeID(expr));

    // Add hashtable entry for expr.
    bool alreadyPresent = searchPath.Set(expr, nullptr, SearchPath::Overwrite);
    if (alreadyPresent)
    {
        return true;
    }

    // Remove hashtable entry for expr when we exit the present scope.
    auto                                         code = [this, expr] { searchPath.Remove(expr); };
    jitstd::utility::scoped_code<decltype(code)> finally(code);

    if (searchPath.GetCount() > MAX_SEARCH_DEPTH)
    {
        return false;
    }

    // If expr is constant, then it is not part of the dependency
    // loop which has to increase monotonically.
    ValueNum vn = expr->gtVNPair.GetConservative();
    if (m_pCompiler->vnStore->IsVNInt32Constant(vn))
    {
        if (rejectNegativeConst)
        {
            int cons = m_pCompiler->vnStore->ConstantValue<int>(vn);
            return (cons >= 0);
        }
        else
        {
            return true;
        }
    }
    // If the rhs expr is local, then try to find the def of the local.
    else if (GenTreeLclUse* use = expr->IsLclUse())
    {
        return IsMonotonicallyIncreasing(use->GetDef()->GetValue(), rejectNegativeConst);
    }
    else if (expr->OperGet() == GT_ADD)
    {
        return IsBinOpMonotonicallyIncreasing(expr->AsOp());
    }
    else if (GenTreePhi* phi = expr->IsPhi())
    {
        for (GenTreePhi::Use& use : phi->Uses())
        {
            // If the arg is already in the path, skip.
            if (searchPath.Lookup(use.GetNode()))
            {
                continue;
            }
            if (!IsMonotonicallyIncreasing(use.GetNode(), rejectNegativeConst))
            {
                JITDUMP("Phi argument not monotonically increasing\n");
                return false;
            }
        }
        return true;
    }
    else if (expr->OperGet() == GT_COMMA)
    {
        return IsMonotonicallyIncreasing(expr->gtEffectiveVal(), rejectNegativeConst);
    }
    JITDUMP("Unknown tree type\n");
    return false;
}

//------------------------------------------------------------------------
// MergeEdgeAssertions: Merge assertions on the edge flowing into the block about a variable
//
// Arguments:
//    normalLclVN - the value number to look for assertions for
//    assertions - the assertions to use
//    pRange - the range to tighten with assertions
//
void RangeCheck::MergeEdgeAssertions(ValueNum normalLclVN, ASSERT_VALARG_TP assertions, Range* pRange)
{
    if (normalLclVN == ValueNumStore::NoVN)
    {
        return;
    }

    BitVecTraits apTraits(m_pCompiler->GetAssertionCount(), m_pCompiler);
    for (BitVecOps::Enumerator en(&apTraits, assertions); en.MoveNext();)
    {
        BoundsAssertion assertion = m_pCompiler->apGetBoundsAssertion(en.Current());

        if (!assertion.IsBoundsAssertion())
        {
            continue;
        }

        Limit      limit(Limit::keUndef);
        genTreeOps cmpOper             = GT_NONE;
        bool       isConstantAssertion = false;

        // Current assertion is of the form (i < len - cns) != 0
        if (assertion.IsCompareCheckedBoundArith())
        {
            ValueNumStore::CompareCheckedBoundArithInfo info;

            // Get i, len, cns and < as "info."
            VNFuncApp funcApp;
            vnStore->GetVNFunc(assertion.GetVN(), &funcApp);
            vnStore->GetCompareCheckedBoundArithInfo(funcApp, &info);
            assert((info.arrOper == GT_ADD) || (info.arrOper == GT_SUB));

            // If we don't have the same variable we are comparing against, bail.
            if (normalLclVN != info.cmpOp)
            {
                continue;
            }

            // If the operand that operates on the bound is not constant, then done.
            if (!vnStore->IsVNInt32Constant(info.arrOp))
            {
                continue;
            }

            int cons = vnStore->ConstantValue<int>(info.arrOp);
            limit    = Limit(Limit::keBinOpArray, info.vnBound, info.arrOper == GT_SUB ? -cons : cons);
            cmpOper  = info.cmpOper;
        }
        // Current assertion is of the form (i < len) != 0
        else if (assertion.IsCompareCheckedBound())
        {
            ValueNumStore::CompareCheckedBoundArithInfo info;

            // Get the info as "i", "<" and "len"
            VNFuncApp funcApp;
            vnStore->GetVNFunc(assertion.GetVN(), &funcApp);
            vnStore->GetCompareCheckedBound(funcApp, &info);

            // If we don't have the same variable we are comparing against, bail.
            if (normalLclVN == info.cmpOp)
            {
                cmpOper = info.cmpOper;
                limit   = Limit(Limit::keBinOpArray, info.vnBound, 0);
            }
            else if (normalLclVN == info.vnBound)
            {
                cmpOper = GenTree::SwapRelop(info.cmpOper);
                limit   = Limit(Limit::keBinOpArray, info.cmpOp, 0);
            }
            else
            {
                continue;
            }
        }
        // Current assertion is of the form i IN [K1..K2]
        else if (assertion.IsRange())
        {
            // If we don't have the same variable we are comparing against, bail.
            if (normalLclVN != assertion.GetVN())
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
                limit   = Limit(Limit::keConstant, min);
                cmpOper = GT_GE;
            }
            else if (min == INT32_MIN)
            {
                limit   = Limit(Limit::keConstant, max);
                cmpOper = GT_LE;
            }
            else
            {
                continue;
            }
        }
        // Current assertion is of the form i == 100
        else if (assertion.IsConstant() && (assertion.GetVN() == normalLclVN))
        {
            int cnstLimit = vnStore->CoercedConstantValue<int>(assertion.GetConstantVN());

            if ((cnstLimit == 0) && !assertion.IsEqual() && vnStore->IsVNCheckedBound(assertion.GetVN()))
            {
                // we have arr.Len != 0, so the length must be atleast one
                limit   = Limit(Limit::keConstant, 1);
                cmpOper = GT_GE;
            }
            else if (assertion.IsEqual())
            {
                limit   = Limit(Limit::keConstant, cnstLimit);
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

        assert(limit.IsBinOpArray() || limit.IsConstant());

        DBEXEC(m_pCompiler->verbose, m_pCompiler->apDumpBoundsAssertion(assertion);)

        // Limits are sometimes made with the form vn + constant, where vn is a known constant
        // see if we can simplify this to just a constant
        if (limit.IsBinOpArray() && vnStore->IsVNInt32Constant(limit.vn))
        {
            Limit tempLimit = Limit(Limit::keConstant, vnStore->ConstantValue<int>(limit.vn));
            if (tempLimit.AddConstant(limit.cns))
            {
                limit = tempLimit;
            }
        }

        ValueNum arrLenVN = vnStore->VNNormalValue(m_pCurBndsChk->GetLength()->GetConservativeVN());

        if (vnStore->IsVNConstant(arrLenVN))
        {
            // Set arrLenVN to NoVN; this will make it match the "vn" recorded on
            // constant limits (where we explicitly track the constant and don't
            // redundantly store its VN in the "vn" field).
            arrLenVN = ValueNumStore::NoVN;
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
        if (cmpOper == GT_LT && !limit.AddConstant(-1))
        {
            continue;
        }
        // Bounds are inclusive, so add +1 for lower bound when ">". But make sure we won't overflow.
        if (cmpOper == GT_GT && !limit.AddConstant(1))
        {
            continue;
        }

        // Doesn't tighten the current bound. So skip.
        if (pRange->uLimit.IsConstant() && limit.vn != arrLenVN)
        {
            continue;
        }

        // Check if the incoming limit from assertions tightens the existing upper limit.
        if (pRange->uLimit.IsBinOpArray() && (pRange->uLimit.vn == arrLenVN))
        {
            // We have checked the current range's (pRange's) upper limit is either of the form:
            //      length + cns
            //      and length == the bndsChkCandidate's arrLen
            //
            // We want to check if the incoming limit tightens the bound, and for that
            // we need to make sure that incoming limit is also on the same length (or
            // length + cns) and not some other length.

            if (limit.vn != arrLenVN)
            {
                JITDUMP("Array length VN did not match arrLen=" FMT_VN ", limit=" FMT_VN "\n", arrLenVN, limit.vn);
                continue;
            }

            int curCns = pRange->uLimit.cns;
            int limCns = (limit.IsBinOpArray()) ? limit.cns : 0;

            // Incoming limit doesn't tighten the existing upper limit.
            if (limCns >= curCns)
            {
                JITDUMP("Bound limit %d doesn't tighten current bound %d\n", limCns, curCns);
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
                pRange->uLimit = limit;
                break;

            case GT_GT:
            case GT_GE:
                pRange->lLimit = limit;
                break;

            case GT_EQ:
                pRange->uLimit = limit;
                pRange->lLimit = limit;
                break;

            default:
                // All other 'cmpOper' kinds leave lLimit/uLimit unchanged
                break;
        }
        JITDUMP("The range after edge merging:");
        JITDUMP(pRange->ToString(m_pCompiler->getAllocatorDebugOnly()));
        JITDUMP("\n");
    }
}

// Merge assertions from the pred edges of the block, i.e., check for any assertions about "op's" value numbers for phi
// arguments. If not a phi argument, check if we have assertions about local variables.
void RangeCheck::MergePhiArgAssertion(BasicBlock* block, GenTreeLclUse* use, Range* pRange DEBUGARG(int indent))
{
    JITDUMP("Merging assertions from pred edges of " FMT_BB " for op [%06d] " FMT_VN "\n", block->bbNum, use->GetID(),
            m_pCompiler->vnStore->VNConservativeNormalValue(use->gtVNPair));

    if (m_pCompiler->GetAssertionCount() == 0)
    {
        return;
    }

    ASSERT_TP   assertions = BitVecOps::UninitVal();
    BasicBlock* pred       = use->GetBlock();

    if (pred->bbFallsThrough() && pred->bbNext == block)
    {
        assertions = pred->bbAssertionOut;
        JITDUMP("Merge assertions from pred " FMT_BB " edge: ", pred->bbNum);
        INDEBUG(m_pCompiler->apDumpAssertionIndices("", assertions, "\n"));
    }
    else if ((pred->bbJumpKind == BBJ_COND || pred->bbJumpKind == BBJ_ALWAYS) && pred->bbJumpDest == block)
    {
        assertions = pred->bbAssertionOutJumpDest;
        JITDUMP("Merge assertions from pred " FMT_BB " JTrue edge: ", pred->bbNum);
        INDEBUG(m_pCompiler->apDumpAssertionIndices("", assertions, "\n"));
    }

    if (!BitVecOps::MayBeUninit(assertions))
    {
        ValueNum valueVN = m_pCompiler->vnStore->VNNormalValue(use->GetDef()->GetConservativeVN());
        MergeEdgeAssertions(valueVN, assertions, pRange);
    }
}

void RangeCheck::MergeSsaUseAssertion(BasicBlock* block, GenTreeLclUse* use, Range* pRange DEBUGARG(int indent))
{
    JITDUMP("Merging assertions from pred edges of " FMT_BB " for op [%06u] " FMT_VN "\n", block->bbNum, use->GetID(),
            m_pCompiler->vnStore->VNNormalValue(use->GetConservativeVN()));

    ASSERT_TP assertions = block->bbAssertionIn;

    if (!BitVecOps::MayBeUninit(assertions) && (m_pCompiler->GetAssertionCount() != 0))
    {
        ValueNum valueVN = m_pCompiler->vnStore->VNNormalValue(use->GetDef()->GetConservativeVN());
        MergeEdgeAssertions(valueVN, assertions, pRange);
    }
}

// Compute the range for a binary operation.
Range RangeCheck::ComputeRangeForBinOp(BasicBlock* block, GenTreeOp* binop, bool monIncreasing DEBUGARG(int indent))
{
    assert(binop->OperIs(GT_ADD, GT_AND, GT_RSH, GT_LSH, GT_UMOD));

    GenTree* op1 = binop->gtGetOp1();
    GenTree* op2 = binop->gtGetOp2();

    if (binop->OperIs(GT_AND, GT_RSH, GT_LSH, GT_UMOD))
    {
        if (!op2->IsIntCnsFitsInI32())
        {
            // only cns is supported for op2 at the moment for &,%,<<,>> operators
            return Range(Limit::keUnknown);
        }

        int icon = -1;
        if (binop->OperIs(GT_AND))
        {
            // x & cns -> [0..cns]
            icon = static_cast<int>(op2->AsIntCon()->IconValue());
        }
        else if (binop->OperIs(GT_UMOD))
        {
            // x % cns -> [0..cns-1]
            icon = static_cast<int>(op2->AsIntCon()->IconValue()) - 1;
        }
        else if (binop->OperIs(GT_RSH, GT_LSH) && op1->OperIs(GT_AND) && op1->AsOp()->gtGetOp2()->IsIntCnsFitsInI32())
        {
            // (x & cns1) >> cns2 -> [0..cns1>>cns2]
            int icon1 = static_cast<int>(op1->AsOp()->gtGetOp2()->AsIntCon()->IconValue());
            int icon2 = static_cast<int>(op2->AsIntCon()->IconValue());
            if ((icon1 >= 0) && (icon2 >= 0) && (icon2 < 32))
            {
                icon = binop->OperIs(GT_RSH) ? (icon1 >> icon2) : (icon1 << icon2);
            }
        }

        if (icon < 0)
        {
            return Range(Limit::keUnknown);
        }
        Range range(Limit(Limit::keConstant, 0), Limit(Limit::keConstant, icon));
        JITDUMP("Limit range to %s\n", range.ToString(m_pCompiler->getAllocatorDebugOnly()));
        return range;
    }

    // other operators are expected to be handled above.
    assert(binop->OperIs(GT_ADD));

    Range* op1RangeCached = nullptr;
    Range  op1Range       = Limit(Limit::keUndef);
    // Check if the range value is already cached.
    if (!rangeMap.Lookup(op1, &op1RangeCached))
    {
        // If we already have the op in the path, then, just rely on assertions, else
        // find the range.
        if (searchPath.Lookup(op1))
        {
            op1Range = Range(Limit(Limit::keDependent));
        }
        else
        {
            op1Range = GetRange(block, op1, monIncreasing DEBUGARG(indent));
        }
        if (GenTreeLclUse* use = op1->IsLclUse())
        {
            MergeSsaUseAssertion(block, use, &op1Range DEBUGARG(indent + 1));
        }
    }
    else
    {
        op1Range = *op1RangeCached;
    }

    Range* op2RangeCached;
    Range  op2Range = Limit(Limit::keUndef);
    // Check if the range value is already cached.
    if (!rangeMap.Lookup(op2, &op2RangeCached))
    {
        // If we already have the op in the path, then, just rely on assertions, else
        // find the range.
        if (searchPath.Lookup(op2))
        {
            op2Range = Range(Limit(Limit::keDependent));
        }
        else
        {
            op2Range = GetRange(block, op2, monIncreasing DEBUGARG(indent));
        }
        if (GenTreeLclUse* use = op2->IsLclUse())
        {
            MergeSsaUseAssertion(block, use, &op2Range DEBUGARG(indent + 1));
        }
    }
    else
    {
        op2Range = *op2RangeCached;
    }

    Range r = RangeOps::Add(op1Range, op2Range);
    JITDUMP("BinOp add ranges %s %s = %s\n", op1Range.ToString(m_pCompiler->getAllocatorDebugOnly()),
            op2Range.ToString(m_pCompiler->getAllocatorDebugOnly()), r.ToString(m_pCompiler->getAllocatorDebugOnly()));
    return r;
}

// Compute the range for a local var definition.
Range RangeCheck::ComputeRangeForLocalDef(BasicBlock*    block,
                                          GenTreeLclUse* use,
                                          bool monIncreasing DEBUGARG(int indent))
{
    GenTreeLclDef* def = use->GetDef();

#ifdef DEBUG
    if (m_pCompiler->verbose)
    {
        JITDUMP("----------------------------------------------------\n");
        m_pCompiler->gtDispTree(def);
        JITDUMP("----------------------------------------------------\n");
    }
#endif
    Range range = GetRange(def->GetBlock(), def->GetValue(), monIncreasing DEBUGARG(indent));
    if ((m_pCompiler->GetAssertionCount() > 0) && !BitVecOps::MayBeUninit(block->bbAssertionIn))
    {
        ValueNum valueVN = m_pCompiler->vnStore->VNNormalValue(def->GetValue()->GetConservativeVN());
        JITDUMP("Merge assertions from " FMT_BB ": ", block->bbNum);
        INDEBUG(m_pCompiler->apDumpAssertionIndices("", block->bbAssertionIn, " "));
        JITDUMP("for assignment about [%06d]\n", use->GetID());
        MergeEdgeAssertions(valueVN, block->bbAssertionIn, &range);

        JITDUMP("done merging\n");
    }

    return range;
}

// https://msdn.microsoft.com/en-us/windows/apps/hh285054.aspx
// CLR throws IDS_EE_ARRAY_DIMENSIONS_EXCEEDED if array length is > INT_MAX.
// new byte[INT_MAX]; still throws OutOfMemoryException on my system with 32 GB RAM.
// I believe practical limits are still smaller than this number.
#define ARRLEN_MAX (0x7FFFFFFF)

// Get the limit's maximum possible value, treating array length to be ARRLEN_MAX.
bool RangeCheck::GetLimitMax(Limit& limit, int* pMax)
{
    int& max1 = *pMax;
    switch (limit.type)
    {
        case Limit::keConstant:
            max1 = limit.GetConstant();
            break;

        case Limit::keBinOpArray:
        {
            int tmp = GetArrLength(limit.vn);
            if (tmp <= 0)
            {
                tmp = ARRLEN_MAX;
            }
            if (IntAddOverflows(tmp, limit.GetConstant()))
            {
                return false;
            }
            max1 = tmp + limit.GetConstant();
        }
        break;

        default:
            return false;
    }
    return true;
}

// Check if the arithmetic overflows.
bool RangeCheck::AddOverflows(Limit& limit1, Limit& limit2)
{
    int max1;
    if (!GetLimitMax(limit1, &max1))
    {
        return true;
    }

    int max2;
    if (!GetLimitMax(limit2, &max2))
    {
        return true;
    }

    return IntAddOverflows(max1, max2);
}

// Does the bin operation overflow.
bool RangeCheck::DoesBinOpOverflow(BasicBlock* block, GenTreeOp* binop)
{
    GenTree* op1 = binop->gtGetOp1();
    GenTree* op2 = binop->gtGetOp2();

    if (!searchPath.Lookup(op1) && DoesOverflow(block, op1))
    {
        return true;
    }

    if (!searchPath.Lookup(op2) && DoesOverflow(block, op2))
    {
        return true;
    }

    // Get the cached ranges of op1
    Range* op1Range = nullptr;
    if (!rangeMap.Lookup(op1, &op1Range))
    {
        return true;
    }
    // Get the cached ranges of op2
    Range* op2Range = nullptr;
    if (!rangeMap.Lookup(op2, &op2Range))
    {
        return true;
    }

    JITDUMP("Checking bin op overflow %s %s\n", op1Range->ToString(m_pCompiler->getAllocatorDebugOnly()),
            op2Range->ToString(m_pCompiler->getAllocatorDebugOnly()));

    if (!AddOverflows(op1Range->UpperLimit(), op2Range->UpperLimit()))
    {
        return false;
    }
    return true;
}

bool RangeCheck::DoesPhiOverflow(BasicBlock* block, GenTreePhi* phi)
{
    for (GenTreePhi::Use& use : phi->Uses())
    {
        GenTree* arg = use.GetNode();
        if (searchPath.Lookup(arg))
        {
            continue;
        }
        if (DoesOverflow(block, arg))
        {
            return true;
        }
    }
    return false;
}

bool RangeCheck::DoesOverflow(BasicBlock* block, GenTree* expr)
{
    bool overflows = false;
    if (!overflowMap.Lookup(expr, &overflows))
    {
        overflows = ComputeDoesOverflow(block, expr);
    }
    return overflows;
}

bool RangeCheck::ComputeDoesOverflow(BasicBlock* block, GenTree* expr)
{
    JITDUMP("Does overflow [%06d]?\n", Compiler::dspTreeID(expr));
    searchPath.Set(expr, block, SearchPath::Overwrite);

    bool overflows = true;

    if (searchPath.GetCount() > MAX_SEARCH_DEPTH)
    {
        overflows = true;
    }
    // If the definition chain resolves to a constant, it doesn't overflow.
    else if (vnStore->IsVNConstant(expr->gtVNPair.GetConservative()))
    {
        overflows = false;
    }
    else if (expr->OperGet() == GT_IND)
    {
        overflows = false;
    }
    else if (expr->OperGet() == GT_COMMA)
    {
        overflows = ComputeDoesOverflow(block, expr->gtEffectiveVal());
    }
    // Check if the var def has rhs involving arithmetic that overflows.
    else if (GenTreeLclUse* use = expr->IsLclUse())
    {
        overflows = DoesOverflow(use->GetDef()->GetBlock(), use->GetDef()->GetValue());
    }
    // Check if add overflows.
    else if (expr->OperIs(GT_ADD))
    {
        overflows = DoesBinOpOverflow(block, expr->AsOp());
    }
    // GT_AND, GT_UMOD, GT_LSH and GT_RSH don't overflow
    // Actually, GT_LSH can overflow so it depends on the analysis done in ComputeRangeForBinOp
    else if (expr->OperIs(GT_AND, GT_RSH, GT_LSH, GT_UMOD))
    {
        overflows = false;
    }
    // Walk through phi arguments to check if phi arguments involve arithmetic that overflows.
    else if (GenTreePhi* phi = expr->IsPhi())
    {
        overflows = DoesPhiOverflow(block, phi);
    }
    overflowMap.Set(expr, overflows, OverflowMap::Overwrite);
    searchPath.Remove(expr);
    JITDUMP("[%06d] %s\n", Compiler::dspTreeID(expr), ((overflows) ? "overflows" : "does not overflow"));
    return overflows;
}

//------------------------------------------------------------------------
// ComputeRange: Compute the range recursively by asking for the range of each variable in the dependency chain.
//
// Arguments:
//   block - the block that contains `expr`;
//   expr - expression to compute the range for;
//   monIncreasing - true if `expr` is proven to be monotonically increasing;
//   indent - debug printing indent.
//
// Return value:
//   'expr' range as lower and upper limits.
//
// Notes:
//   eg.: c = a + b; ask range of "a" and "b" and add the results.
//   If the result cannot be determined i.e., the dependency chain does not terminate in a value,
//   but continues to loop, which will happen with phi nodes we end the looping by calling the
//   value as "dependent" (dep).
//   If the loop is proven to be "monIncreasing", then make liberal decisions for the lower bound
//   while merging phi node. eg.: merge((0, dep), (dep, dep)) = (0, dep),
//   merge((0, 1), (dep, dep)) = (0, dep), merge((0, 5), (dep, 10)) = (0, 10).
//
Range RangeCheck::ComputeRange(BasicBlock* block, GenTree* expr, bool monIncreasing DEBUGARG(int indent))
{
    bool  newlyAdded = !searchPath.Set(expr, block, SearchPath::Overwrite);
    Range range      = Limit(Limit::keUndef);

    ValueNum vn = m_pCompiler->vnStore->VNConservativeNormalValue(expr->gtVNPair);

    // If we just added 'expr' in the current search path, then reduce the budget.
    if (newlyAdded)
    {
        // Assert that we are not re-entrant for a node which has been
        // visited and resolved before and not currently on the search path.
        noway_assert(!rangeMap.Lookup(expr));
        m_nVisitBudget--;
    }
    // Prevent quadratic behavior.
    if (IsOverBudget())
    {
        // Set to unknown, since an Unknown range resolution, will stop further
        // searches. This is because anything that merges with Unknown will
        // yield Unknown. Unknown is lattice top.
        range = Range(Limit(Limit::keUnknown));
        JITDUMP("GetRange not tractable within max node visit budget.\n");
    }
    // Prevent unbounded recursion.
    else if (searchPath.GetCount() > MAX_SEARCH_DEPTH)
    {
        // Unknown is lattice top, anything that merges with Unknown will yield Unknown.
        range = Range(Limit(Limit::keUnknown));
        JITDUMP("GetRange not tractable within max stack depth.\n");
    }
    // TODO-CQ: The current implementation is reliant on integer storage types
    // for constants. It could use INT64. Still, representing ULONG constants
    // might require preserving the var_type whether it is a un/signed 64-bit.
    // JIT64 doesn't do anything for "long" either. No asm diffs.
    else if (expr->TypeGet() == TYP_LONG || expr->TypeGet() == TYP_ULONG)
    {
        range = Range(Limit(Limit::keUnknown));
        JITDUMP("GetRange long or ulong, setting to unknown value.\n");
    }
    // If VN is constant return range as constant.
    else if (m_pCompiler->vnStore->IsVNConstant(vn))
    {
        range = (m_pCompiler->vnStore->TypeOfVN(vn) == TYP_INT)
                    ? Range(Limit(Limit::keConstant, m_pCompiler->vnStore->ConstantValue<int>(vn)))
                    : Limit(Limit::keUnknown);
    }
    // If local, find the definition from the def map and evaluate the range for rhs.
    else if (GenTreeLclUse* use = expr->IsLclUse())
    {
        range = ComputeRangeForLocalDef(block, use, monIncreasing DEBUGARG(indent + 1));
        MergeSsaUseAssertion(block, use, &range DEBUGARG(indent + 1));
    }
    // compute the range for binary operation
    else if (expr->OperIs(GT_ADD, GT_AND, GT_RSH, GT_LSH, GT_UMOD))
    {
        range = ComputeRangeForBinOp(block, expr->AsOp(), monIncreasing DEBUGARG(indent + 1));
    }
    // If phi, then compute the range for arguments, calling the result "dependent" when looping begins.
    else if (GenTreePhi* phi = expr->IsPhi())
    {
        for (GenTreePhi::Use& use : phi->Uses())
        {
            Range argRange = Range(Limit(Limit::keUndef));
            if (searchPath.Lookup(use.GetNode()))
            {
                JITDUMP("PhiArg [%06d] is already being computed\n", Compiler::dspTreeID(use.GetNode()));
                argRange = Range(Limit(Limit::keDependent));
            }
            else
            {
                argRange = GetRange(block, use.GetNode(), monIncreasing DEBUGARG(indent + 1));
            }
            assert(!argRange.LowerLimit().IsUndef());
            assert(!argRange.UpperLimit().IsUndef());
            MergePhiArgAssertion(block, use.GetNode(), &argRange DEBUGARG(indent + 1));
            JITDUMP("Merging ranges %s %s:", range.ToString(m_pCompiler->getAllocatorDebugOnly()),
                    argRange.ToString(m_pCompiler->getAllocatorDebugOnly()));
            range = RangeOps::Merge(range, argRange, monIncreasing);
            JITDUMP("%s\n", range.ToString(m_pCompiler->getAllocatorDebugOnly()));
        }
    }
    else if (varTypeIsSmallInt(expr->TypeGet()))
    {
        switch (expr->TypeGet())
        {
            case TYP_UBYTE:
                range = Range(Limit(Limit::keConstant, 0), Limit(Limit::keConstant, 255));
                break;
            case TYP_BYTE:
                range = Range(Limit(Limit::keConstant, -128), Limit(Limit::keConstant, 127));
                break;
            case TYP_USHORT:
                range = Range(Limit(Limit::keConstant, 0), Limit(Limit::keConstant, 65535));
                break;
            case TYP_SHORT:
                range = Range(Limit(Limit::keConstant, -32768), Limit(Limit::keConstant, 32767));
                break;
            default:
                range = Range(Limit(Limit::keUnknown));
                break;
        }

        JITDUMP("%s\n", range.ToString(m_pCompiler->getAllocatorDebugOnly()));
    }
    else if (expr->OperGet() == GT_COMMA)
    {
        range = GetRange(block, expr->gtEffectiveVal(), monIncreasing DEBUGARG(indent + 1));
    }
    else
    {
        // The expression is not recognized, so the result is unknown.
        range = Range(Limit(Limit::keUnknown));
    }

    rangeMap.Set(expr, new (m_alloc) Range(range), RangeMap::Overwrite);
    searchPath.Remove(expr);
    return range;
}

#ifdef DEBUG
void Indent(int indent)
{
    for (int i = 0; i < indent; ++i)
    {
        JITDUMP("   ");
    }
}
#endif

// Get the range, if it is already computed, use the cached range value, else compute it.
Range RangeCheck::GetRange(BasicBlock* block, GenTree* expr, bool monIncreasing DEBUGARG(int indent))
{
#ifdef DEBUG
    if (m_pCompiler->verbose)
    {
        Indent(indent);
        JITDUMP("[RangeCheck::GetRange] " FMT_BB, block->bbNum);
        m_pCompiler->gtDispTree(expr);
        Indent(indent);
        JITDUMP("{\n", expr);
    }
#endif

    Range* pRange = nullptr;
    Range range = rangeMap.Lookup(expr, &pRange) ? *pRange : ComputeRange(block, expr, monIncreasing DEBUGARG(indent));

#ifdef DEBUG
    if (m_pCompiler->verbose)
    {
        Indent(indent);
        JITDUMP("   %s Range [%06d] => %s\n", (pRange == nullptr) ? "Computed" : "Cached", Compiler::dspTreeID(expr),
                range.ToString(m_pCompiler->getAllocatorDebugOnly()));
        Indent(indent);
        JITDUMP("}\n", expr);
    }
#endif
    return range;
}

// Entry point to range check optimizations.
void RangeCheck::OptimizeRangeChecks()
{
    assert(m_pCompiler->ssaForm && (m_pCompiler->vnStore != nullptr));

#ifdef DEBUG
    if (m_pCompiler->verbose)
    {
        JITDUMP("*************** In OptimizeRangeChecks()\n");
        JITDUMP("Blocks/trees before phase\n");
        m_pCompiler->fgDispBasicBlocks(true);
    }
#endif

    // Walk through trees looking for arrBndsChk node and check if it can be optimized.
    for (BasicBlock* const block : m_pCompiler->Blocks())
    {
        for (Statement* const stmt : block->Statements())
        {
            for (GenTree* const tree : stmt->TreeList())
            {
                if (IsOverBudget())
                {
                    return;
                }
                OptimizeRangeCheck(block, stmt, tree);
            }
        }
    }
}

void Compiler::phRemoveRangeCheck()
{
    RangeCheck rc(this);
    rc.OptimizeRangeChecks();
}
